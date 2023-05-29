##This script acts to extract LVTS data from a predefined horizon 
## 
##The Script will import the saved data.table extract from the "LVTS_processing_file_V##_Start-to-End_Year_Selective_Extraction.R" code
##as well as the saved StaticData file extracted using the "process_CPA-MemberNamesBICandIDs.R"code
##
##Final input data tables that the code will work with are 
##        a: dataLVTSTxnsPriodSpec - The LVTS data file
##        b: fiStaticDataFile - The static data file
##
##
##
## Author : Segun Bewaji
## Creation Date : 18 Apr 2016
## Modified : Segun Bewaji
## Modifications Made: 18 Apr 2016
##        1) 
##        2) 
##        3)  
##           
##        4)  
##           
##        5) 
##        6)  
## Modified : Segun Bewaji
## Modifications Made:
##        7) 
##        8) 
##           
##        9)  
##           
##       10) 
##
##
##
##
##
##
## 
## $Id$

##the usual
gc();
rm(list=ls());
gc();

##import required libraries
require(plyr);# contains method and function calls to be used to filter and select subsets of the data
require(lubridate);# handles that date elements of the data
require(dplyr);# contains method and function calls to be used to filter and select subsets of the data
require(data.table);#used to store and manipiulate data in the form of a data table similar to a mySQL database table
require(xts);#used to manipulate date-time data into quarters
require(ggplot2);#used to plot data
require(magrittr);#contains method/function calls to use for chaining e.g. %>%
require(scales);#component of ggplot2 that requires to be manually loaded to handle the scaling of various axies
require(foreach);#handles computations for each element of a vector/data frame/array/etc required the use of %do% or %dopar% operators in base R
require(iterators);#handles iteration through for each element of a vector/data frame/array/etc
require(stringr);#handles strings. Will be used to remove all quotation marks from the data
require(foreign);#to export data into formats readable by STATA and other statistical packages
require(xlsx);#3-party package to export data into excel xlsx format


##<<<<<<<<<<<<<<<<<<<<<<<<<DECLARE GLOBAL VARIABLES>>>>>>>>>>>>>>>>>>>>>>>>
##Declare all global variables to be used in this class
extractedLVTSDataLoadPath <- c("C:/Projects/FundingDataTables/dateTimeLVTSTransDataSeries2014-14.Rdata");#will load the data table: 
#mergedLVTSTransYearsSpecYM
dataLVTSTxnsPriodSpecNameString <- NULL;



##NULL declarations
dataFrequency <- NULL; #used to set the frequency of the data for agreggation 
#the options are monthly, quarterly, annual, daily (does not make sense since it does not distingush the year)


##<<<<<<<<<<<<<<<<<<<<<<<<<LOAD DATA>>>>>>>>>>>>>>>>>>>>>>>>
##Set Data Frequency
dataFrequency <- "dateTime"; #the options are monthly, quarterly, annual, daily, dateTime (does not make sense since it does not distingush the year)
print(paste("data frequency is", dataFrequency, sep=" "));#used to record speed of computations
##Now load the individual FI's complete data tables
print("Loading data tables");#used to record speed of computations
print(now());#used to record speed of computations
load(extractedLVTSDataLoadPath);
gc();
print("data tables loaded");#used to record speed of computations
print(now());#used to record speed of computations
gc();
#mergedLVTSTransYearsSpecYMSummarizedSenderReceiver  <-  as.data.frame(mergedLVTSTransYearsSpecYM);#convert mergedLVTSTransYearsSpecYM to a temporary data.frame object
#rm(mergedLVTSTransYearsSpecYM);#remove mergedLVTSTransYearsSpecYM to free up memory since not in use
gc(); #free up memory
