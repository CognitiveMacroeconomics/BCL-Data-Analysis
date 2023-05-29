##This script looops through the payment transaction files and
##parses the LVTS data to work on the funding model
##
## Author : Segun Bewaji
## Modified : Segun Bewaji
## Modifications Made 18 Apr 2016:
##        1) 
##        2) 
##        3)  
##           
##        4)  
##           
##        5) 
##        6)  
## Modified : Segun Bewaji
## Modifications Made :
##        7) 
##        8) 
##           
##        9)  
##            
##           
##       10) 
##
##
##
##
##
##
##
##
##
## Creation Date : 18 Apr 2016
## Modification Date : 
## $Id$

##the usual
gc();
rm(list=ls());
gc();

require(lubridate);# handles that date elements of the data
require(dplyr);# contains method and function calls to be used to filter and select subsets of the data
require(data.table);#used to store and manipiulate data in the form of a data table similar to a mySQL database table
require(xts);#used to manipulate date-time data into quarters
require(ggplot2);#used to plot data
require(magrittr);#contains method/function calls to use for chaining e.g. %>%
require(scales);#component of ggplot2 that requires to be manually loaded to handle the scaling of various axies
require(foreach);#handles computations for each element of a vector/data frame/array/etc required the use of %do% or %dopar% operators in base R
require(iterators);#handles iteration through for each element of a vector/data frame/array/etc


##is this a test?
test <- FALSE;

##initial variable declarations 
##
LVTS.dir <- "C:/Projects/RawData/LVTS";
working.dir <- "C:/Projects/FundingDataTables";
year.dir <- NULL;
startYr <- 2015;
endYr <- 2015;
jumboQueueThreshold <- 100000000;

dataLVTSTxnsPriodSpecNameString <- c("C:/Projects/FundingDataTables/dataLVTSTxnsPriodSpec2015.Rdata");

full.file.names <- NULL;
save.file.names <- NULL;
dataLVTSTxnsPriodSpec <- NULL; #declare the merged data series object as a NULL to assigned as data.table later on in the code
tempMergeTablesList <- NULL; #declare the merged data table list object as a NULL for use with the rbindlist(a,b) call 
#to append datatables

##Determine current and set required the working directories
cur.dir <- getwd();
setwd(LVTS.dir);
##Set naming pattern of the subdirectories the data files are housed in these are the year subdirectories to look in
year.dir <- list.files(pattern="[[:digit:]]{4}");
yearTMP <- gsub("[^0-9]","",year.dir);


##this substrRight(inputString, numberOfChars) function/method takes an input string/character list or vector and returns the last
##numberOfChars (i.e. n) characters in each of the elements of the string/character list or vector
##it will be used to select the approriate dierectories from which to collate the data being utilised
substrRight <- function(inputString, numberOfChars){
  substr(inputString, nchar(inputString)-numberOfChars+1, nchar(inputString));
}

##loop through the list of years in the list of year-directory names and select those that meet the start and end year criteria for data collation
for(i in yearTMP){
  if((startYr > as.numeric(i)) | (endYr < as.numeric(i))){##since this for-loop is looking to trim down the list of directories to those that
    ##fall within the startYear and endYear of data extraction, this if-statement inverses the success condition. Thus only when the
    ##value of i is outside of the startYr to endYr range will this if-staement be triggered and elements removed from the year.dir list
    idx <- match(i,year.dir);##get the index of the element of list year.dir with value i
    year.dir <- year.dir[-idx];##remove/delete the element of the list year.dir with index idx
  }else{
    #print("false");
  }##end of if-else statement
}##end of for-loop

##this makes a smaller version of the data
if (test){
  year.dir <- year.dir[-c(1:10)];
}

##loop through the list of directories from which to collate data and create a list of folder paths
for (i in year.dir){
  setwd(i);
  tmp <- list.files(pattern="pay_trans_.[[:digit:]]{3,4}.txt");
  digitTMP <- substrRight((gsub("[^0-9]","",tmp)), 2); ##Returns [1] "0801" "0802" "0803" "0804" "0805" "0806" "0807" "0808" "0809" "0810" "0811" "0812"
  digitTMP <- as.numeric(digitTMP);
  save.tmp <- sapply(tmp,function(x){paste(strsplit(x,"\\.")[[1]][1],"Rdata",sep=".")},USE.NAMES=FALSE);
  if (length(tmp) != 0){
    ##build the full path
    full.path <- paste(LVTS.dir,i,tmp,sep="/");
    save.path <- paste(working.dir,save.tmp,sep="/");
    rm(tmp);
    full.file.names <- c(full.file.names,full.path);
    save.file.names <- c(save.file.names,save.path);
  }
  setwd(LVTS.dir)
}

##this function reads in a raw text file and outputs the clean dataset
lvts.file.process <- function(x){
  raw.data <- read.csv(x,stringsAsFactors=F,header=F);
  raw.data <- tbl_df(raw.data);
  tmp.data <- filter(raw.data,V12 != "RD"); ##These are apparently returned payments
  #  clean.data <- select(tmp.data,V1,V9,V3,V4,V7,V10);
  #  exists(tmp.data$V14)
  if("V14" %in% names(tmp.data)){
    clean.data <- select(tmp.data,V1,V9,V3,V4,V7,V10,V14);
  }else{
    clean.data <- select(tmp.data,V1,V9,V3,V4,V7,V10);
    clean.data[, "V14"] <- "";
  }
  colnames(clean.data) <- c("date","tranche","sender","receiver","time","amount","stream");##receiver
  clean.data$date.time <- ymd_hms(paste(clean.data$date,clean.data$time,sep=" "),tz="America/New_York");
  clean.data$txnHour <- hour(clean.data$date.time);
  clean.data <- select(clean.data,-date,-time);#remove time and date fields from final data extract
  clean.data$tranche <- as.factor(clean.data$tranche);
  clean.data$sender <- as.factor(clean.data$sender);
  clean.data$receiver <- as.factor(clean.data$receiver);
  clean.data$stream <- as.factor(clean.data$stream);
  ##Flag Jumbo Queue items
  clean.data$queue <- c("R");
  clean.data$queue <- with(clean.data, ifelse(clean.data$amount>=jumboQueueThreshold, "J", "R"));
  #if(clean.data$amount>=jumboQueueThreshold){
  #  clean.data$queue <- "J";
  #}else{
  #  clean.data$queue <- "R";
  #}
  clean.data <- as.data.table(clean.data);
  return(clean.data);
  
}

##This method carries out a deep copy of the passed data table
dataTableCopy <- function(inputeDataTable){
  outputDataTable <- copy(inputeDataTable);
  return(outputDataTable);
}

gc(); #free up memory
##now loop over the files and save them
for (i in 1:length(full.file.names)){
  
  #check if the dataLVTSTxnsPriodSpec datatable has been created/initialised as a datatable. If not deep copy the created 
  #lvts.trans datatable into dataLVTSTxnsPriodSpec to instantiate it. Else append the new lvts.trans datatable to the end of the 
  #dataLVTSTxnsPriodSpec datatable. 
  if(is.null(dataLVTSTxnsPriodSpec)){
    dataLVTSTxnsPriodSpec  <- lvts.file.process(full.file.names[i]);
  } else{
    tempMergeTablesList <- list(dataLVTSTxnsPriodSpec,lvts.file.process(full.file.names[i]));
    dataLVTSTxnsPriodSpec  <- rbindlist(tempMergeTablesList);
  }
  
  #check if all lvts.trans files have been created and the save.file.names list has been completely looped through
  #if so save the dataLVTSTxnsPriodSpec datatable
  if(i == length(save.file.names)){
    save(dataLVTSTxnsPriodSpec,file=dataLVTSTxnsPriodSpecNameString);
  }
  gc();
}

##return to the original directory
gc();
setwd(cur.dir);