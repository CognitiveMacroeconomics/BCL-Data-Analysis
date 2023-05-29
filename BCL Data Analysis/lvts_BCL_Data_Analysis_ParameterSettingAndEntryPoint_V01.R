##This script acts as the parameter setting and application entry point for the code that is used to 
##conduct the initial data analysis of BCL data found in the BoC extract # 9
##
##
## To use this script simply enter the  (1) start and end year for the data collection (2) the data frequency (3) path to the BCL pre-processed data 
## (4)
## 
##
## Author : Segun Bewaji
## Creation Date : 19 Sept 2016
## Modified : Segun Bewaji
## Modifications Made: 17 Sept 2016
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

##clear the environment and free up memory
gc();
rm(list=ls());
gc();

##Load Required Libraries
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
require(openxlsx);#3-party package to export data into excel xlsx format without the java dependancy that causes errors in the xlsx package
require(timeDate);#3-party package to work on date and time objects
require(igraph);
require(networkD3);
require(sqldf);




##<<<<<<<<<<<<<<<<<<<<<<<<<DECLARE GLOBAL VARIABLES>>>>>>>>>>>>>>>>>>>>>>>>
##Declare all global variables to be used in this class/script and all called/sourced classes/scripts
startYr <- 2014;
endYr <- 2014;
dataFrequency <- "dateTime"; #the options are monthly, quarterly, annual, daily, dateTime (does not make sense since it does not distingush the year)
dateTimeNettingFrequency <- "5 min"; #This is used only if the dataFrequency is dateTime.
#Available options are "x min", "x hour", "x day" etc where x is the integer value of the interval
dataUnits <- 1000000; # unit of measurment of the data (mainly) for plotting purposes thousands, millions, billions, etc
extractedLVTSBCLDataLoadPath <- NULL;#declared as a place holder
extractedLVTSTXNDataLoadPath <- NULL;#declared as a place holder
noJumboQueue <- FALSE;#Should Jumbo Queue threshold transactions be removed. This can be used for filtering out payments above some threshold size
                      #From speaking to Bill Cyr, it appears that the jumbo queue is not automatically triggered for payments above 100,000,000. 
                      #For such payments, the jumbo queue is only triggered if the risk controls are violated
jumboQueueThresholdTXNSize <- 100000000 #jumbo queue threshold payment size
noBankOfCanada <- TRUE;#Should bank of Canada payment flows be excluded from the analysed dataset
noTranche1 <- TRUE;#Should Tranche 1 payment flows be excluded from the analysed dataset


##<<<<<<<<<<<<<<<<<<<<<<<<<DECLARE GLOBAL VARIABLES>>>>>>>>>>>>>>>>>>>>>>>>
##Declare all global variables to be used in this class/script
LVTSTXNDataSeries_df <- NULL;
LVTSTXNMultilateralPositionDataSeries <- NULL; #collects multilateral net positions
LVTSTXNBilateralPositionDataSeries <- NULL; #collects Bilateral net positions
LVTSTXNBilateralValueMatrixDataSeries <- NULL; #collects Bilateral adjacency matrix from value data
LVTSTXNBilateralVolumeMatrixDataSeries <- NULL; #collects Bilateral adjacency matrix from volume data

LVTSBCLDataSeries_df <- NULL;
LVTSBCLMultilateralPositionDataSeries <- NULL; #collects multilateral net positions
LVTSBCLBilateralPositionDataSeries <- NULL; #collects Bilateral net positions
LVTSBCLBilateralValueMatrixDataSeries <- NULL; #collects Bilateral adjacency matrix from value data
LVTSBCLBilateralVolumeMatrixDataSeries <- NULL; #collects Bilateral adjacency matrix from volume data






##Declare path to pre-processed data
#if monthly
if(dataFrequency == "monthly"){
  ##Intput Load Path
  extractedLVTSTXNDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/monthlyLVTSTransDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSTXNMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/monthlyLVTSTXNMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/monthlyLVTSTXNBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/monthlyLVTSTXNBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/monthlyLVTSTXNBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  
  ##<<<<<<<<<<<<<<<<<BCLs>>>>>>>>>>>>>>>>>>>>>>>>>>
  ##Intput Load Path
  extractedLVTSBCLDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/monthlyLVTSBCLDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSBCLMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/monthlyLVTSBCLMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/monthlyLVTSBCLBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/monthlyLVTSBCLBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/monthlyLVTSBCLBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  
  #if quarterly  
}else if(dataFrequency == "quarterly"){
  ##Intput Load Path
  extractedLVTSTXNDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/quarterlyLVTSTransDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSTXNMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/quarterlyLVTSTXNMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/quarterlyLVTSTXNBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/quarterlyLVTSTXNBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/quarterlyLVTSTXNBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  
  ##<<<<<<<<<<<<<<<<<BCLs>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  
  #if annual 
}else if(dataFrequency == "annual"){
  ##Intput Load Path
  extractedLVTSTXNDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/annualLVTSTransDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSTXNMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/annualLVTSTXNMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/annualLVTSTXNBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/annualLVTSTXNBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/annualLVTSTXNBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  
  ##<<<<<<<<<<<<<<<<<BCLs>>>>>>>>>>>>>>>>>>>>>>>>>>
  ##Intput Load Path
  extractedLVTSBCLDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/annualLVTSBCLDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSBCLMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/annualLVTSBCLMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/annualLVTSBCLBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/annualLVTSBCLBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/annualLVTSBCLBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  
   #if daily   
}else if(dataFrequency == "daily"){
  ##Intput Load Path
  extractedLVTSTXNDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/dailyLVTSTransDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSTXNMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dailyLVTSTXNMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dailyLVTSTXNBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dailyLVTSTXNBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dailyLVTSTXNBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  
  ##<<<<<<<<<<<<<<<<<BCLs>>>>>>>>>>>>>>>>>>>>>>>>>>
  ##Intput Load Path
  extractedLVTSBCLDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/dailyLVTSBCLDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSBCLMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dailyLVTSBCLMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dailyLVTSBCLBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dailyLVTSBCLBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dailyLVTSBCLBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  
  #if date and time specific
}else if(dataFrequency == "dateTime"){
  ##Intput Load Path
  extractedLVTSTXNDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/dateTimeLVTSTransDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSTXNMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dateTimeLVTSTXNMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dateTimeLVTSTXNBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dateTimeLVTSTXNBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSTXNBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dateTimeLVTSTXNBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##<<<<<<<<<<<<<<<<<BCLs>>>>>>>>>>>>>>>>>>>>>>>>>>
  ##Intput Load Path
  extractedLVTSBCLDataLoadPath <- c(paste("C:/Projects/FundingDataTables/Cleaned Transactions Data/dateTimeLVTSBCLDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  
  ##Output Save Paths
  LVTSBCLMultilateralPositionDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dateTimeLVTSBCLMultilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralPositionDataSeriesSavePath <-   c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dateTimeLVTSBCLBilateralPositionDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralValueMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dateTimeLVTSBCLBilateralValueMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
  LVTSBCLBilateralVolumeMatrixDataSeriesSavePath <- c(paste("C:/Projects/FundingDataTables/BCL ACE-Model Data/dateTimeLVTSBCLBilateralVolumeMatrixDataSeries",startYr,"-",endYr,".Rdata", sep = "", collapse = NULL));
}


##Path to source files
bclAnalysisFunctionsSourceFilePath <- c('~/Documents/Development Workspaces/R-Workspace/BCL Data Analysis/LVTS_BCL_Analysis_ApplicationFunctions.R');
bclAnalysisSourceFilePath <- c('~/Documents/Development Workspaces/R-Workspace/BCL Data Analysis/lvts_BCL_Data_Analysis_file_V01.R');
txnAnalysisSourceFilePath <- c('~/Documents/Development Workspaces/R-Workspace/BCL Data Analysis/lvts_TXN_Data_Analysis_file_V02.R');


source(bclAnalysisFunctionsSourceFilePath, echo=TRUE);
source(bclAnalysisSourceFilePath, echo=TRUE);
source(txnAnalysisSourceFilePath, echo=TRUE);




##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<EXPORT DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
write.csv2(LVTSBCLDataSeries_df, 
           file=c(paste("C:/Projects/FundingDataTables/LVTSBCLDataSeries_df",startYr,"-",endYr,".csv", sep = "", collapse = NULL)),
           colNames = TRUE, borders = "columns");

write.dta(LVTSBCLDataSeries_df, 
          file=c(paste("C:/Projects/FundingDataTables/LVTSBCLDataSeries_df",startYr,"-",endYr,".dta", sep = "", collapse = NULL)));


write.csv2(LVTSBCLDataSeries_dftSum, 
          file=c(paste("C:/Projects/FundingDataTables/LVTSBCLDataSeries_Sumary",startYr,"-",endYr,".csv", sep = "", collapse = NULL)));
