##This script looops through the payment transaction files and
##parses the LVTS data to work on the funding model
##
## Author : Segun Bewaji
## Modified : Segun Bewaji
## Modifications Made 23 June 2015:
##        1) changes made to original LVTS trans processing file to capture the processing of the BCL extracts 
##        
## Modified : Segun Bewaji
## Modifications Made :
##        7)  
##
##
##
##
##
##
##
##
##
## Creation Date : 19 May 2015
## Modification Date : 22 May 2015, 1 June 2015-10 June 2015
## $Id$

##the usual
gc();
rm(list=ls());
gc();

require(lubridate);# handles that date elements of the data
require(dplyr);# contains method and function calls to be used to filter and select subsets of the data
require(data.table);#used to store and manipiulate data in the form of a data table similar to a mySQL database table
require(ggplot2);#used to plot data
require(magrittr);#contains method/function calls to use for chaining e.g. %>%
require(scales);#component of ggplot2 that requires to be manually loaded to handle the scaling of various axies
require(foreach);#handles computations for each element of a vector/data frame/array/etc required the use of %do% or %dopar% operators in base R
require(iterators);#handles iteration through for each element of a vector/data frame/array/etc



##is this a test?
test <- FALSE;

##this section of the code loops through the LVTS directories and 
##pieces together the full file path of the individual files
LVTSBCLs.dir <- "C:/Projects/RawData/LVTS_BCL";
working.dir <- "C:/Projects/LVTSBCLDataTables";
year.dir <- NULL;
startYr <- 2005;##issue with missing column 5 in the 2004-April File. Full years data available from 2003
endYr <- 2015;

mergedLVTSBCLsNameString <- c("C:/Projects/LVTSBCLDataTables/YearsSpecificLVTSBCLYM2005-15DataSeries.Rdata");

#merged data series file
##now walk through and make a big list of files
full.file.names <- NULL;
save.file.names <- NULL;
mergedLVTSBCLsYearsSpecYM <- NULL; #declare the merged data series object as a NULL to assigned as data.table later on in the code
tempMergeTablesList <- NULL; #declare the merged data table list object as a NULL for use with the rbindlist(a,b) call 
                            #to append datatables
cur.dir <- getwd();
setwd(LVTSBCLs.dir);
##these are the year subdirectories to look in
year.dir <- list.files(pattern="[[:digit:]]{4}");
yearTMP <- gsub("[^0-9]","",year.dir);
#print(yrsTMP);


##this substrRight(inputList, numberOfChars) function/method takes an input string/character list or vector and returns the last
##numberOfChars (i.e. n) characters in each of the elements of the string/character list or vector
substrRight <- function(inputList, numberOfChars){
  substr(inputList, nchar(inputList)-numberOfChars+1, nchar(inputList));
}


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


for (i in year.dir){
  setwd(i);
  tmp <- list.files(pattern="boc_ext09_.[[:digit:]]{3,4}.txt");
  digitTMP <- substrRight((gsub("[^0-9]","",tmp)), 2); ##Returns [1] "0801" "0802" "0803" "0804" "0805" "0806" "0807" "0808" "0809" "0810" "0811" "0812"
  digitTMP <- as.numeric(digitTMP);
  save.tmp <- sapply(tmp,function(x){paste(strsplit(x,"\\.")[[1]][1],"Rdata",sep=".")},USE.NAMES=FALSE);
  if (length(tmp) != 0){
    ##build the full path
    full.path <- paste(LVTSBCLs.dir,i,tmp,sep="/");
    save.path <- paste(working.dir,save.tmp,sep="/");
    rm(tmp);
    full.file.names <- c(full.file.names,full.path);
    save.file.names <- c(save.file.names,save.path);
  }
  setwd(LVTSBCLs.dir)
}

##this function reads in a raw text file and outputs the clean dataset
lvts.file.process <- function(x){
  raw.data <- read.csv(x,stringsAsFactors=F,header=F);
  raw.data <- tbl_df(raw.data);
  tmp.data <- raw.data;
  clean.data <- select(tmp.data,V3,V4,V1,V2,V5,V6);
  colnames(clean.data) <- c("date","sequence","grantor","grantee","time","credit_limit");
  clean.data$date.time <- ymd_hms(paste(clean.data$date,clean.data$time,sep=" "),tz="America/New_York");
  clean.data$Year_Month <- ymd(paste(year(clean.data$date.time),month(clean.data$date.time),1),tz="America/New_York");
  clean.data <- select(clean.data,-date,-time);
  clean.data$sequence <- as.factor(clean.data$sequence);
  clean.data$grantor <- as.factor(clean.data$grantor);
  clean.data$grantee <- as.factor(clean.data$grantee);
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
  
  #check if the mergedLVTSBCLsYearsSpecYM datatable has been created/initialised as a datatable. If not deep copy the created 
  #lvts.trans datatable into mergedLVTSBCLsYearsSpecYM to instantiate it. Else append the new lvts.trans datatable to the end of the 
  #mergedLVTSBCLsYearsSpecYM datatable. 
  if(is.null(mergedLVTSBCLsYearsSpecYM)){
    mergedLVTSBCLsYearsSpecYM  <- lvts.file.process(full.file.names[i]);
  } else{
    tempMergeTablesList <- list(mergedLVTSBCLsYearsSpecYM,lvts.file.process(full.file.names[i]));
    mergedLVTSBCLsYearsSpecYM  <- rbindlist(tempMergeTablesList);
  }

  #check if all lvts.trans files have been created and the save.file.names list has been completely looped through
  #if so save the mergedLVTSBCLsYearsSpecYM datatable
  if(i == length(save.file.names)){
    save(mergedLVTSBCLsYearsSpecYM,file=mergedLVTSBCLsNameString);
  }
  gc();
}

##return to the original directory
setwd(cur.dir);