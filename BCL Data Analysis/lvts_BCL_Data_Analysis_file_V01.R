##This script is used to conduct data alasysis on BCLs extended by FIs excluding Bank of Canada.
##The tasks completed in this script include
##                1) Generating adjacency matrices for 
##                      a) BCL values
##                      a) BCL volumes
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
load(extractedLVTSBCLDataLoadPath);

##Start data cleaning and transformation
#convert imported data.table to data.frame
LVTSBCLDataSeries_df <- as.data.frame(LVTSBCLDataSeries);

#filter Bank of Canada out of data set
if(noBankOfCanada){
LVTSBCLDataSeries_df <- LVTSBCLDataSeries_df %>% #select data.frame to be transformed
  select(date.time:ExtendedValue) %>% #select all recorded fields from date.time to ExtendedValue
  filter(grantor!="BCANCA", grantee!="BCANCA"); #filter for rows where the grantor or grantee FI is not the Bank of Canada 
}

#filter out BCLs recorded after pre-settlement time of 6PM
LVTSBCLDataSeries_df <- LVTSBCLDataSeries_df %>% #select data.frame to be transformed
  select(date.time:ExtendedValue) %>% #select all recorded fields from date.time to ExtendedValue
  filter((hour(date.time)<=18 & ExtendedValue > 0)); #filter for rows where the time the BCL is extended is before 6PM and value is greater than zero.
                                                     #ExtendedValue > 0 excludes FIs that are not active between 0:00 and 6AM
                                                     #There is a risk of excluding values that go to zero between 6AM and 6PM



#Temporarily convert LVTSBCLDataSeries_df to a data.table object to quickly compute the share of BCL value and volume extended by grantor
LVTSBCLDataSeries_df <- as.data.table(LVTSBCLDataSeries_df);
#NOTE: Each line of granted BCL value and volume for each grantor to grantee pair is weighted 
#by the total BCL extended by the grantor over the entire dataset
LVTSBCLDataSeries_df[,ShareOfExtendedVolume:=ExtendedVolume/sum(ExtendedVolume)*100,by=c("grantor")];
LVTSBCLDataSeries_df[,ShareOfExtendedValue:=ExtendedValue/sum(ExtendedValue)*100,by=c("grantor")];
#convert LVTSBCLDataSeries_df back to a data.frame object for other computations
LVTSBCLDataSeries_df <- as.data.frame(LVTSBCLDataSeries_df);


#Ensure the date.time field is a POSIXct object. Converting from data.table converts date.time to an interger rather than the double required for POSIXct objects
if(dataFrequency == "dateTime"){
  LVTSBCLDataSeries_df$date.time <- as.POSIXct(LVTSBCLDataSeries_df$date.time)
}

#create adjacency matrix from the share of total value and volume of BCLs extended by each grantor
#NOTE that the aggrigation function fun.aggregate is sum because each line of granted BCL value and volume for each grantor to grantee pair is weighted 
#by the total BCL extended by the grantor over the entire dataset
LVTSBCLBilateralVolumeMatrixDataSeries <- as.matrix(dcast(LVTSBCLDataSeries_df, grantor ~ grantee, fun.aggregate = sum, value.var = "ShareOfExtendedVolume", fill=0));
LVTSBCLBilateralValueMatrixDataSeries <- as.matrix(dcast(LVTSBCLDataSeries_df, grantor ~ grantee, fun.aggregate = sum, value.var = "ShareOfExtendedValue", fill=0));

#set the first column as the matrix row names and first row as column headings
#Essentially these lines of code specify the names of the nodes
row.names(LVTSBCLBilateralVolumeMatrixDataSeries)<- LVTSBCLBilateralVolumeMatrixDataSeries[,1];
row.names(LVTSBCLBilateralValueMatrixDataSeries)<- LVTSBCLBilateralValueMatrixDataSeries[,1];
#re-assign the matirx as a matrix
LVTSBCLBilateralVolumeMatrixDataSeries<- as.matrix(LVTSBCLBilateralVolumeMatrixDataSeries[,-1]); 
LVTSBCLBilateralValueMatrixDataSeries<- as.matrix(LVTSBCLBilateralValueMatrixDataSeries[,-1]);


#Collect the summary data
LVTSBCLDataSeries_dft <- as.data.table(LVTSBCLDataSeries_df);
LVTSBCLDataSeries_dftSum <- LVTSBCLDataSeries_dft[, list(AverageMode=StatisticalMode(ExtendedValue), Maximum=max(ExtendedValue), 
                                                         Minimum=min(ExtendedValue)), by=c("grantor", "grantorFullName")];

#Plot BCL extended by each grantor
ggplotBCLExtendedByGrantor <- ggplot(LVTSBCLDataSeries_df,
                                     aes(x=date.time, y=(ExtendedValue/dataUnits), color=grantee)) +
                                     geom_line(aes(group=grantee, order = sample(seq_along(date.time))))+ 
                                     scale_x_datetime(breaks=date_breaks("5 weeks"), labels = date_format("%y/%m/%d")) + 
                                     scale_y_continuous(label=dollar) + facet_wrap(~ grantor)+ ylab("Extended BCL Value (CAD mils)")+ xlab("Time of Day");

print(ggplotBCLExtendedByGrantor);


igraphLVTSBCLBilateralVolumeMatrix <- graph.adjacency(LVTSBCLBilateralVolumeMatrixDataSeries, mode="directed", weighted=TRUE, diag=TRUE);
#rglplot(igraphLVTSBCLBilateralVolumeMatrix, vertex.size=14,vertex.color="red");
tkplot(igraphLVTSBCLBilateralVolumeMatrix, vertex.size=14,vertex.color="red");



