##This script is used to conduct data alasysis on TXNs sent and recieved by FIs excluding Bank of Canada.
##The tasks completed in this script include
##                1) Generating adjacency matrices for 
##                      a) TXN values
##                      a) TXN volumes
##                2) computing daily/monthly/quaterly bilateral credit limits
##                3) computing daily/monthly/quaterly multilateral credit limits
##                4) plotting the computed data
##
##
##A similar script is available for payment flows data


##Load Required Libraries
#require(plyr);# contains method and function calls to be used to filter and select subsets of the data
#require(lubridate);# handles that date elements of the data
#require(dplyr);# contains method and function calls to be used to filter and select subsets of the data
#require(data.table);#used to store and manipiulate data in the form of a data table similar to a mySQL database table
#require(xts);#used to manipulate date-time data into quarters
#require(ggplot2);#used to plot data
#require(magrittr);#contains method/function calls to use for chaining e.g. %>%
#require(scales);#component of ggplot2 that requires to be manually loaded to handle the scaling of various axies
#require(foreach);#handles computations for each element of a vector/data frame/array/etc required the use of %do% or %dopar% operators in base R
#require(iterators);#handles iteration through for each element of a vector/data frame/array/etc
#require(stringr);#handles strings. Will be used to remove all quotation marks from the data
#require(foreign);#to export data into formats readable by STATA and other statistical packages
#require(openxlsx);#3-party package to export data into excel xlsx format without the java dependancy that causes errors in the xlsx package
#require(timeDate);#3-party package to work on date and time objects








##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<SCRIPT/CODE EXECUTION>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##Load pre-processed data
load(extractedLVTSTXNDataLoadPath);

##Remove unecessary columns used in the forecast model 
##i.e. RecievedVolume	TotalVolume RecievedValue	TotalValue
LVTSTransDataSeries[,RecievedVolume:=NULL];
LVTSTransDataSeries[,TotalVolume:=NULL];
LVTSTransDataSeries[,RecievedValue:=NULL];
LVTSTransDataSeries[,TotalValue:=NULL];

##Start data cleaning and transformation
#convert imported data.table to data.frame
LVTSTransDataSeries_df <- as.data.frame(LVTSTransDataSeries);

#filter Bank of Canada out of data set
if(noBankOfCanada){
LVTSTransDataSeries_df <- LVTSTransDataSeries_df %>% #select data.frame to be transformed
  select(date.time:SentValue) %>% #select all recorded fields from date.time to SentValue
  filter(sender!="BCANCA", receiver!="BCANCA"); #filter for rows where the sender or receiver FI is not the Bank of Canada 
}

#filter Bank of Canada out of data set
if(noTranche1){
  LVTSTransDataSeries_df <- LVTSTransDataSeries_df %>% #select data.frame to be transformed
    select(date.time:SentValue) %>% #select all recorded fields from date.time to SentValue
    filter(tranche!=1); #filter for rows where the payment flow is through tranche 1 
}

#filter out TXNs recorded after pre-settlement time of 6PM
LVTSTransDataSeries_df <- LVTSTransDataSeries_df %>% #select data.frame to be transformed
  select(date.time:SentValue) %>% #select all recorded fields from date.time to SentValue
  filter((hour(date.time)<=18 & SentValue > 0)); #filter for rows where the time the TXN is extended is before 6PM and value is greater than zero.
                                                     #SentValue > 0 excludes FIs that are not active between 0:00 and 6AM
                                                     #There is a risk of excluding values that go to zero between 6AM and 6PM


#filter out TXNs that will trigger the jumbo queue depending on the value of the noJumboQueue boolean and jumboQueueThresholdTXNSize
if(noJumboQueue){
  LVTSTransDataSeries_df <- LVTSTransDataSeries_df %>% #select data.frame to be transformed
    select(date.time:SentValue) %>% #select all recorded fields from date.time to SentValue
    filter(SentValue < jumboQueueThresholdTXNSize); #filter transactions for which the value is less than the jumbo queue or 
                                                     #Threshold transaction size (i.e. jumboQueueThresholdTXNSize)
}


#Temporarily convert LVTSTransDataSeries_df to a data.table object to quickly compute the share of TXN value and volume extended by sender
LVTSTransDataSeries_df <- as.data.table(LVTSTransDataSeries_df);
#NOTE: Each line of granted TXN value and volume for each sender to receiver pair is weighted sprintf(
#by the total TXN extended by the sender over the entire dataset
LVTSTransDataSeries_df[,ShareOfSentVolume:=round(SentVolume/sum(SentVolume)*100, digits = 4),by=c("sender")];
LVTSTransDataSeries_df[,ShareOfSentValue:=round(SentValue/sum(SentValue)*100, digits = 4),by=c("sender")];
LVTSTransDataSeries_df[,ShareOfRecievedVolume:=round(SentVolume/sum(SentVolume)*100, digits = 4),by=c("receiver")];
LVTSTransDataSeries_df[,ShareOfRecievedValue:=round(SentValue/sum(SentValue)*100, digits = 4),by=c("receiver")];

#Add columns for the bilateral and multilateral sent and revieved values
if(dataFrequency == "dateTime"){
  LVTSTransDataSeries_df[,MultilateralValueSent:=sum(SentValue),by=c("sender","date.time")];
  LVTSTransDataSeries_df[,MultilateralValueRecieved:=sum(SentValue),by=c("receiver","date.time")];
  LVTSTransDataSeries_df[,BilateralValueRecieved:=sum(SentValue),by=c("receiver","sender","date.time")];
  LVTSTransDataSeries_df[,BilateralValueSent:=(-1*sum(SentValue)),by=c("sender","receiver","date.time")];
}

#convert LVTSTransDataSeries_df back to a data.frame object for other computations
LVTSTransDataSeries_df <- as.data.frame(LVTSTransDataSeries_df);


#Ensure the date.time field is a POSIXct object. Converting from data.table converts date.time to an interger rather than the double required for POSIXct objects
if(dataFrequency == "daily"){
  LVTSTransDataSeries_df$date <- as.POSIXct(LVTSTransDataSeries_df$date);
}else if(dataFrequency == "dateTime"){
  LVTSTransDataSeries_df$date.time <- as.POSIXct(LVTSTransDataSeries_df$date.time);
}

#create adjacency matrix from the share of total value and volume of TXNs extended by each sender
#NOTE that the aggrigation function fun.aggregate is sum because each line of granted TXN value and volume for each sender to receiver pair is weighted 
#by the total TXN extended by the sender over the entire dataset
LVTSTXNBilateralVolumeMatrixDataSeries <- as.matrix(dcast(LVTSTransDataSeries_df, sender ~ receiver, fun.aggregate = sum, value.var = "ShareOfSentVolume", fill=0));
LVTSTXNBilateralValueMatrixDataSeries <- as.matrix(dcast(LVTSTransDataSeries_df, sender ~ receiver, fun.aggregate = sum, value.var = "ShareOfSentValue", fill=0));

LVTSTXNBilateralRecieverVolumeMatrixDataSeries <- as.matrix(dcast(LVTSTransDataSeries_df, receiver ~ sender, fun.aggregate = sum, value.var = "ShareOfRecievedVolume", fill=0));
LVTSTXNBilateralRecieverValueMatrixDataSeries <- as.matrix(dcast(LVTSTransDataSeries_df,  receiver ~ sender, fun.aggregate = sum, value.var = "ShareOfRecievedValue", fill=0));


#set the first column as the matrix row names and first row as column headings
#Essentially these lines of code specify the names of the nodes
row.names(LVTSTXNBilateralVolumeMatrixDataSeries)<- LVTSTXNBilateralVolumeMatrixDataSeries[,1];
row.names(LVTSTXNBilateralValueMatrixDataSeries)<- LVTSTXNBilateralValueMatrixDataSeries[,1];

row.names(LVTSTXNBilateralRecieverVolumeMatrixDataSeries)<- LVTSTXNBilateralRecieverVolumeMatrixDataSeries[,1];
row.names(LVTSTXNBilateralRecieverValueMatrixDataSeries)<- LVTSTXNBilateralRecieverValueMatrixDataSeries[,1];

#re-assign the matirx as a matrix
LVTSTXNBilateralVolumeMatrixDataSeries<- as.matrix(LVTSTXNBilateralVolumeMatrixDataSeries[,-1]); 
LVTSTXNBilateralValueMatrixDataSeries<- as.matrix(LVTSTXNBilateralValueMatrixDataSeries[,-1]);

LVTSTXNBilateralRecieverVolumeMatrixDataSeries<- as.matrix(LVTSTXNBilateralRecieverVolumeMatrixDataSeries[,-1]); 
LVTSTXNBilateralRecieverValueMatrixDataSeries<- as.matrix(LVTSTXNBilateralRecieverValueMatrixDataSeries[,-1]);

#Plot TXN extended by each sender
ggplotTXNSentBySendingFI <- ggplot(LVTSTransDataSeries_df,
                                     aes(x=date.time, y=(SentValue/dataUnits), color=receiver)) +
                                     geom_line(aes(group=receiver, order = sample(seq_along(date.time))))+ 
                                     scale_x_datetime(breaks=date_breaks("5 weeks"), labels = date_format("%y/%m/%d")) + 
                                     scale_y_continuous(label=dollar) + facet_wrap(~ sender)+ ylab("Sent TXN Value (CAD mils)")+ xlab("Time of Day");

print(ggplotTXNSentBySendingFI);



#Compute intraday net debit positions

#LVTSBCLDataSeries_df[,BilateralNetDebit:=SentValue/sum(SentValue)*100,by=c("sender", "receiver")];

igraphLVTSTXNBilateralVolumeMatrix <- graph.adjacency(LVTSTXNBilateralVolumeMatrixDataSeries, mode="directed", weighted=TRUE, diag=TRUE);
#rglplot(igraphLVTSTXNBilateralVolumeMatrix, vertex.size=14,vertex.color="red");

tkplot(igraphLVTSTXNBilateralVolumeMatrix,vertex.size=14,vertex.color="green");
#coords <- tkplot.getcoords(igraphLVTSTXNBilateralVolumeMatrix);

