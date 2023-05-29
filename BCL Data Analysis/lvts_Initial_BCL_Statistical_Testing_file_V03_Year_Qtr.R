##This is class to test chaining of the filter and select methods/function in R for funding data
##to work with for work on individual FIs in the funding model
##
## Author : Segun Bewaji
## Modified : Segun Bewaji
## Modifications Made:
##        1: Reworking of original LVTS Trans statistical analysis script to handle BCL data. 
##           NOTE: 
##                   
##                 
##                  
##                  
##                 
##        2: 
##           NOTE: 
##                 
##                 
##        3: 
##            
##           

## Creation Date : 23 June 2015
## Modification Date : 
## $Id$


##clear system memory and workspace
gc();
rm(list=ls());
gc();

##import require libraries 
require(lubridate);# handles that date elements of the data
require(dplyr);# contains method and function calls to be used to filter and select subsets of the data
require(data.table);#used to store and manipiulate data in the form of a data table similar to a mySQL database table
require(ggplot2);#used to plot data
require(magrittr);#contains method/function calls to use for chaining e.g. %>%
require(scales);#component of ggplot2 that requires to be manually loaded to handle the scaling of various axies
require(moments);#3rd-party library used to compute statistics such as skewness and kurtosis which are not provided in the base R package set
require(rlist);#3-party package to allow more flexible and java-collections like handling of lists in R. Allows you to save a list as a Rdata file
require(foreign);#to export data into formats readable by STATA and other statistical packages
require(xts);#used to manipulate date-time data into quarters
require(xlsx);#to export data into formats readable by excel

##<<<<<<<<<<<<<<<<<<<<<<<<<DECLARE GLOBAL VARIABLES>>>>>>>>>>>>>>>>>>>>>>>>
##Declare all global variables to be used in this class
isDaily <- FALSE;
startYear  <- 2005;
endYear <- 2015;
summaryGrantorFIStatsSavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLSummaryStatisticsMonthlyByYear2005-2015.Rdata");
grantorFILVTSBCLTimeSeriesDataTableSavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLTimeSeriesDataTableMonthlyByYear2005-2015.Rdata");
grantorFILVTSBCLTimeSeriesXLSXSavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLTimeSeriesXLSXSeries2005-2015.xlsx");
grantorFILVTSBCLTotalXLSXSavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLTotalXLSXSeries2005-2015.xlsx");

##NULL declarations
workingDataTable <- NULL; ##working datatable
workingDataTableMonthlyByYear  <- NULL; ##working datatable
workingSummaryDataTableMonthlyByYear <- NULL;
grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear <- NULL;
grantorGranteeFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear <- NULL;
grantorGranteeFITotalBCLVolumeValueSummaryStatistics <- NULL;
monthlyBCLsValueGrantedRangeScatterPlot <- NULL;
monthlyBCLsValueGrantorRangeScatterPlot <- NULL;
monthlyBCLsVolumeGrantedRangeScatterPlot <- NULL;
monthlyBCLsVolumeGrantorRangeScatterPlot <- NULL;

##NULLs for debuging
grantorFILVTSMeanBCLValueSeriesScatterPlot <- NULL;
grantorFILVTSMeanBCLVolumeSeriesScatterPlot <- NULL;
grantorFILVTSTotalBCLValueSeriesScatterPlot <- NULL;
grantorFILVTSTotalBCLVolumeSeriesScatterPlot <- NULL;
grantorFILVTSBCLVolumeSeriesScatterPlot <- NULL;


vectorOfFullDataSetDates <- NULL;
vectorOfWorkingDataSetDates <- NULL;
vectorFINames <-  NULL;
vectorColumnNames <- NULL;
grantorFILVTSBCLTimeSeriesDataTable  <-  NULL;
grantorFIsBCLVolumeTimeSeries <-  NULL;
grantorFIsBCLValueTimeSeries <-  NULL;

##<<<<<<<<<<<<<<<<<<<<<<<<<LOAD DATA>>>>>>>>>>>>>>>>>>>>>>>>
##Now load the individual FI's complete data tables
print("Loading data table");#used to record speed of computations
print(now());#used to record speed of computations
load("C:/Projects/LVTSBCLDataTables/YearsSpecificLVTSBCLYM2005-15DataSeries.Rdata");
gc();
print("data table loaded");#used to record speed of computations
print(now());#used to record speed of computations
gc();

##<<<<<<<<<<<<<<<<<<<<<<<<<DECLARE CLASS METHODS/FUNCTIONS>>>>>>>>>>>>>>>>>>>>>>>>
##The monthlyDataExtraction method/function takes an input data.table/data frame and compresses it to a smaller
##version containing the data for the specific month and year of interest
getMonthlyDataExtraction <- function(inputDataTable, yr, mth, lookupColumnName){
  gc();
  temporaryTable <- NULL;
  colNames <- names(inputDataTable);#create a vector or array of column heading names from the input table
  if(lookupColumnName %in% colNames & lookupColumnName == "Year_Month"){ #check if the key look-up column name is
                                                                        #in (i.e. %in%) the list of column names
    ##the following filter and this IF-statement in general is hardcoded because R does not appear to have a way of 
    ##passing strings to functions to get column contents. THe ideal implementation would simply be
    ##"inputDataTable$lookupColumnName" and would not require checking that "lookupColumnName == "Year_Month""
    
    temporaryTable <-  filter(inputDataTable, (year(inputDataTable$Year_Month)== yr 
                                    & month(inputDataTable$Year_Month) == mth)
           );##end of filter statement
     };#end of if statment
return(temporaryTable)
}

getColumnNames <- function(inputDataTable){
  colNames <- names(inputDataTable);#create a vector or array of column heading names from the input table
  return(colNames)
}


##version containing the data for the specific month and year of interest
getAnnualDataExtraction <- function(inputDataTable, yr, lookupColumnName, colNames){
  gc();
  temporaryTable <- NULL;
  if(lookupColumnName %in% colNames & lookupColumnName == "Year_Month"){ #check if the key look-up column name is
    #in (i.e. %in%) the list of column names
    ##the following filter and this IF-statement in general is hardcoded because R does not appear to have a way of 
    ##passing strings to functions to get column contents. THe ideal implementation would simply be
    ##"inputDataTable$lookupColumnName" and would not require checking that "lookupColumnName == "Year_Month""
    
    temporaryTable <-  filter(inputDataTable, (year(inputDataTable$Year_Month)== yr)
    );##end of filter statement
  };#end of if statment
  return(temporaryTable)
}

##The monthlyDataExtraction method/function takes an input data.table/data frame and compresses it to a smaller
##version containing the data for the specific month and year of interest
getMonthlyDataRangeExtraction <- function(inputDataTable, yr, startMth, endMth, lookupColumnName, colNames){
  gc();
  temporaryTable <- NULL;
  if(endMth <= startMth){
    return(print("start month is greater than end month: please chakc and correct entry"))
  }else
  if(lookupColumnName %in% colNames & lookupColumnName == "Year_Month"){ #check if the key look-up column name is
    #in (i.e. %in%) the list of column names
    ##the following filter and this IF-statement in general is hardcoded because R does not appear to have a way of 
    ##passing strings to functions to get column contents. THe ideal implementation would simply be
    ##"inputDataTable$lookupColumnName" and would not require checking that "lookupColumnName == "Year_Month""
    
    temporaryTable <-  filter(inputDataTable, (year(inputDataTable$Year_Month)== yr 
                                               & month(inputDataTable$Year_Month) >= startMth & month(inputDataTable$Year_Month) <= endMth)
    );##end of filter statement
  };#end of if statment
  return(temporaryTable)
}

##version containing the data for the specific month and year of interest
getAnnualDataRangeExtraction <- function(inputDataTable, startYr, endYr, lookupColumnName, colNames){
  temporaryTable <- NULL;
  inputDataTable <- as.data.table(inputDataTable);
  if(endYr < startYr){
    return(print("start year is greater than end year: please check and correct entry"))
  }else if(lookupColumnName %in% colNames & lookupColumnName == "Year_Month"){ #check if the key look-up column name is
    #in (i.e. %in%) the list of column names
    ##the following filter and this IF-statement in general is hardcoded because R does not appear to have a way of 
    ##passing strings to functions to get column contents. THe ideal implementation would simply be
    ##"inputDataTable$lookupColumnName" and would not require checking that "lookupColumnName == "Year_Month""
    print("Building TempTable");#used to record speed of computations
    print(now());#used to record speed of computations
    gc();
    temporaryTable <-  filter(inputDataTable, (year(inputDataTable$Year_Month) >= startYr & year(inputDataTable$Year_Month) <= endYr)
    );##end of filter statement
  };#end of if statment
  return(temporaryTable)
}

##This method returns the list of FIs in the input data set
getListOfFINames <- function(inputDataTable, colNames){
  gc();
  tempFINamesVector <- NULL;
  if("grantor" %in% colNames){#check if the field grantor is in the list of column headers and if so get the unique list of grantor names
    tempFINamesVector  <- unique(inputDataTable$grantor);
  } else if("grantees" %in% colNames){#otherwise check if the field reciever is in the list of column headers and if so get the unique list of 
    #grantor names
    tempFINamesVector <- unique(inputDataTable$receiver);
  }
  return(tempFINamesVector);#return the list of unique FI names
}

##This method returns the list of FIs in the input data set
getListOfFullDataSetDates <- function(inputDataTable, colNames){
  gc();
  tempFullDataSetDatesVector <- NULL;
  if("Year_Month" %in% colNames){#check if the field grantor is in the list of column headers and if so get the unique list of grantor names
    tempFullDataSetDatesVector  <- unique(inputDataTable$date.time);
  } else {#otherwise check if the field reciever is in the list of column headers and if so get the unique list of grantor names
    print("No Date field found in input table");
  }
  return(tempFullDataSetDatesVector);#return the list of unique FI names
}

##This method returns the list of FIs in the input data set
getListOfTImeSeriesDates <- function(inputDataTable){
  gc();
  tempFullDataSetDatesVector <- NULL;
  if(!isDaily){#check if the field grantor is in the list of column headers and if so get the unique list of grantor names
    tempFullDataSetDatesVector  <- unique(inputDataTable$Year_Month);
  } else if(isDaily){#otherwise check if the field reciever is in the list of column headers and if so get the unique list of grantor names
    print("This file only handles monthly frequency analysis. For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
  } else{
    print("No Date field found");
  }
  return(tempFullDataSetDatesVector);#return the list of unique FI names
}

##This method return the mean value of payments sent to a given FI by another FI
getMeanAmountByIndividualGrantorGranteePair <- function(inputDataTable, bclGrantor, bclGrantee, fieldOfInterest, colNames){
  gc();
  meanValue <- NULL;
  inputDataTable <- as.data.table(inputDataTable);
  if(fieldOfInterest %in% colNames & fieldOfInterest == "credit_limit"){ #check if the key look-up column name is
    #in (i.e. %in%) the list of column names
    ##the following filter and this IF-statement in general is hardcoded because R does not appear to have a way of 
    ##passing strings to functions to get column contents. THe ideal implementation would simply be
    ##"inputDataTable$lookupColumnName" and would not require checking that "lookupColumnName == "date.time""
    meanValue <- mean(inputDataTable[(grantor == bclGrantor & grantee == bclGrantee), ]$credit_limit);
  }##end of IF-Statement
  temporaryMeanTable <- NULL;
  return(meanValue)
}

##This method returns a data table containing the mean value of payments sent to a given FI by another FI
getSummaryStatisticsAmountByGrantee <- function(inputDataTable, fieldOfInterest, colNames){
  gc();
  temporaryMeanTable <- NULL;
  if(fieldOfInterest %in% colNames & "grantee" %in% colNames 
     & fieldOfInterest == "credit_limit"){ #check if the key look-up column name is
    #in (i.e. %in%) the list of column names
    ##the following statements assigns to the temporaryMeanTable the summary statistics including mean, standard deviation,
    ##frequency, median, min and max. 
    ##these are then grouped by grantor grantee pairings. This approach seems to make more sense from a code resuability
    ##standpoint. That is, it will be simply a matter of adding an additional grouping of year and month to the method to 
    ##extend and apply the method to the entire data sent and then derive summary statistics for each grantor-grantee pairing  
    ##by year and month without having to rewrite the whole method to do that calculation 
    temporaryMeanTable <- inputDataTable[ ,list(Min_Credit_Limit = min(credit_limit),Median_Amount = median(credit_limit, na.rm = TRUE),
                                                Max_Credit_Limit = max(credit_limit),
                                                Mean_Credit_Limit = mean(credit_limit), Standard_Deviation_Credit_Limit = sd(credit_limit),
                                                Total_BCL_Credit_Limit = sum(credit_limit, na.rm = TRUE),
                                                BCL_Credit_Limit_Volume = (sum(credit_limit, na.rm = TRUE)/mean(credit_limit))),
                                         by=c("grantor","grantee")];
    temporaryMeanTable <- temporaryMeanTable[order(grantor),]; ##order the table by the grantor column 
  }##end of IF-Statement
  return(temporaryMeanTable);
}

##This method returns a data table containing the mean value of payments sent to a given FI by another FI
getValueSummaryStatisticsByGranteeAndByYearAndMonth <- function(inputDataTable, fieldOfInterest, colNames){
  gc();
  temporarySummaryTable <- NULL;
  if(fieldOfInterest %in% colNames & "grantee" %in% colNames & "Year_Month" %in% colNames
     & fieldOfInterest == "credit_limit"){ #check if the key look-up column name is
    print("summarising and sorting");
    print(now());
    ##the following statements assigns to the temporarySummaryTable the summary statistics including mean, standard deviation,
    ##frequency, median, min and max. 
    ##these are then grouped by grantor grantee pairings. This approach seems to make more sense from a code resuability
    ##standpoint. That is, it will be simply a matter of adding an additional grouping of year and month to the method to 
    ##extend and apply the method to the entire data sent and then derive summary statistics for each grantor-grantee pairing  
    ##by year and month without having to rewrite the whole method to do that calculation 
    temporarySummaryTable <- inputDataTable[ ,list(Min_Credit_Limit = min(credit_limit),
                                                   Qtl_Credit_Limit_05 = quantile(credit_limit, 0.05, na.rm = TRUE),
                                                   Median_Credit_Limit = median(credit_limit, na.rm = TRUE), Mean_Credit_Limit = mean(credit_limit),
                                                   Qtl_Credit_Limit_95 = quantile(credit_limit, 0.95, na.rm = TRUE), 
                                                   Max_Credit_Limit = max(credit_limit), StdDev_Credit_Limit = sd(credit_limit),
                                                   Total_BCL_Credit_Limit = sum(credit_limit, na.rm = TRUE),
                                                   BCL_Credit_Limit_Volume = (sum(credit_limit, na.rm = TRUE)/mean(credit_limit))
                                                ),
                                         by=c("grantor","grantee", "Year_Month")];
    temporarySummaryTable <- temporarySummaryTable[order(grantor),]; ##order the table by the grantor column 
  }##end of IF-Statement"
  
  return(temporarySummaryTable);
}

##This method returns a data table containing the summary statistics of value of payments sent to a given FI by another FI on a daily basis
getValueSummaryStatisticsByGranteeAndByYearMonthDay <- function(inputDataTable, fieldOfInterest, colNames){
  gc();
  temporarySummaryTable <- NULL;
  if(fieldOfInterest %in% colNames & "grantee" %in% colNames
     & fieldOfInterest == "credit_limit"){ #check if the key look-up column name is
    #in (i.e. %in%) the list of column names
    #ymd_hms(paste(clean.data$date,clean.data$time,sep=" "),tz="America/New_York");
    print("updating date format");
    print(now());
    
    gc();
    print("summarising and sorting");
    print(now());
    ##the following statements assigns to the temporarySummaryTable the summary statistics including mean, standard deviation,
    ##frequency, median, min and max. 
    ##these are then grouped by grantor reciever pairings. This approach seems to make more sense from a code resuability
    ##standpoint. That is, it will be simply a matter of adding an additional grouping of year and month to the method to 
    ##extend and apply the method to the entire data sent and then derive summary statistics for each grantor-receiver pairing  
    ##by year and month without having to rewrite the whole method to do that calculation 
    temporarySummaryTable <- inputDataTable[ ,list(Min_Credit_Limit = min(credit_limit),Qtl_Credit_Limit_05 = quantile(credit_limit, 0.05, na.rm = TRUE),
                                                   Median_Credit_Limit = median(credit_limit, na.rm = TRUE), Mean_Credit_Limit = mean(credit_limit), 
                                                   Qtl_Credit_Limit_95 = quantile(credit_limit, 0.95, na.rm = TRUE), Max_Credit_Limit = max(credit_limit),
                                                   StdDev_Credit_Limit = sd(credit_limit),
                                                   Total_BCL_Credit_Limit = sum(credit_limit, na.rm = TRUE),
                                                   BCL_Credit_Limit_Volume = (sum(credit_limit, na.rm = TRUE)/mean(credit_limit))
    ),
    by=c("grantor","grantee", "Year_Month_Day")];
    temporarySummaryTable <- temporarySummaryTable[order(grantor),]; ##order the table by the grantor column 
  }##end of IF-Statement
  
  return(temporarySummaryTable);
}

##The following method filters a summary statistical data table containing information about the payments amounts and further summarises
##this data by grantor FI and year and month to get summary statistics for number (volume) of payments sent by the FI to all other FIs combined
getGrantorFILVTSVolumeValueSummaryStatistics <- function(inputDataTable, colNames){
  gc();
  temporarySummaryTable <- NULL;
  
  if("BCL_Credit_Limit_Volume" %in% colNames & "grantor" %in% colNames & "Year_Month" %in% colNames
     & "Total_BCL_Credit_Limit" %in% colNames){        #check if the key look-up column names are in (i.e. %in%) the list of column names
    
    ##the following statements assigns to the temporarySummaryTable the summary statistics including mean, standard deviation,
    ##frequency, median, min and max. 
    ##these are then grouped by grantor reciever pairings. This approach seems to make more sense from a code resuability
    ##standpoint. That is, it will be simply a matter of adding an additional grouping of year and month to the method to 
    ##extend and apply the method to the entire data sent and then derive summary statistics for each grantor-receiver pairing  
    ##by year and month without having to rewrite the whole method to do that calculation 
    temporarySummaryTable <- inputDataTable[ ,list(
                                                   Total_BCL_Credit_Limit_Volume = sum(BCL_Credit_Limit_Volume),
                                                   Total_BCL_Credit_Limit_Value= sum(Total_BCL_Credit_Limit),
                                                   Mean_Credit_Limit_Volume = mean(BCL_Credit_Limit_Volume), 
                                                   StdDev_Credit_Limit_Volume = sd(BCL_Credit_Limit_Volume),
                                                   Variance_Payments_Volume = var(BCL_Credit_Limit_Volume),
                                                   Qtl_Vol_05 = quantile(BCL_Credit_Limit_Volume, 0.05, na.rm = TRUE),
                                                   Min_Volume = min(BCL_Credit_Limit_Volume),
                                                   Median_Vol = median(BCL_Credit_Limit_Volume, na.rm = TRUE), Max_Volume = max(BCL_Credit_Limit_Volume),
                                                   Qtl_Vol_95 = quantile(BCL_Credit_Limit_Volume, 0.95, na.rm = TRUE),
                                                   Skewness_Credit_Limit_Volume = skewness(BCL_Credit_Limit_Volume),
                                                   Kurtosis_Credit_Limit_Volume = kurtosis(BCL_Credit_Limit_Volume),
                                                   Mean_Credit_Limit_Value = mean(Total_BCL_Credit_Limit), 
                                                   StdDev_Credit_Limit_Value = sd(Total_BCL_Credit_Limit),
                                                   Variance_Credit_Limit_Value = var(Total_BCL_Credit_Limit ),
                                                   Qtl_Credit_Limit_Low = quantile(Total_BCL_Credit_Limit, 0.05, na.rm = TRUE), 
                                                   Min_Credit_Limit_Value = min(Total_BCL_Credit_Limit),
                                                   Median_Credit_Limit = median(Total_BCL_Credit_Limit, na.rm = TRUE), 
                                                   Max_Credit_Limit_Value = max(Total_BCL_Credit_Limit),
                                                   Qtl_Credit_Limit_Val_High = quantile(Total_BCL_Credit_Limit, 0.95, na.rm = TRUE),
                                                   Skewness_Credit_Limit_Value = skewness(Total_BCL_Credit_Limit),
                                                   Kurtosis_Credit_Limit_Value = kurtosis(Total_BCL_Credit_Limit)
                                                   ),
                                            by=c("grantor", "Year_Month")
                                            ];
  } ##end of If-Statement
  else { 
    print("This file only handles monthly frequency analysis. For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
  }#end of second if or else if statement
  temporarySummaryTable <- temporarySummaryTable[order(grantor),]; ##order the table by the grantor column 
  return(temporarySummaryTable);
}#end of getgrantorFILVTSVolumeValueSummaryStatistics


##The following method filters a summary statistical data table containing information about the payments amounts and further summarises
##this data by grantor FI and year and month to get summary statistics for number (volume) of payments sent by the FI to all other FIs combined
getGrantorGranteeFILVTSVolumeValueSummaryStatistics <- function(inputDataTable, colNames){
  gc();
  temporarySummaryTable <- NULL;
  
  if("Total_BCL_Credit_Limit" %in% colNames & "grantor" %in% colNames & "Year_Month" %in% colNames
     & "BCL_Credit_Limit_Volume" %in% colNames){        #check if the key look-up column names are in (i.e. %in%) the list of column names
    
    ##the following statements assigns to the temporarySummaryTable the summary statistics including mean, standard deviation,
    ##frequency, median, min and max. 
    ##these are then grouped by grantor reciever pairings. This approach seems to make more sense from a code resuability
    ##standpoint. That is, it will be simply a matter of adding an additional grouping of year and month to the method to 
    ##extend and apply the method to the entire data sent and then derive summary statistics for each grantor-receiver pairing  
    ##by year and month without having to rewrite the whole method to do that calculation 
    temporarySummaryTable <- inputDataTable[ ,list(
      Total_BCL_Credit_Limit_Volume = sum(BCL_Credit_Limit_Volume),Total_BCL_Credit_Limit_Value= sum(Total_BCL_Credit_Limit),
      Mean_Credit_Limit_Volume = mean(BCL_Credit_Limit_Volume), StdDev_Credit_Limit_Volume = sd(BCL_Credit_Limit_Volume),
      Variance_Credit_Limit_Volume = var(BCL_Credit_Limit_Volume),
      Qtl_Credit_Limit_Vol_05 = quantile(BCL_Credit_Limit_Volume, 0.05, na.rm = TRUE),
      Min_Credit_Limit_Volume = min(BCL_Credit_Limit_Volume),
      Median_Credit_Limit_Vol = median(BCL_Credit_Limit_Volume, na.rm = TRUE), Max_Credit_Limit_Volume = max(BCL_Credit_Limit_Volume),
      Qtl_Credit_Limit_Vol_95 = quantile(BCL_Credit_Limit_Volume, 0.95, na.rm = TRUE),
      Skewness_Credit_Limit_Volume = skewness(BCL_Credit_Limit_Volume),
      Kurtosis_Credit_Limit_Volume = kurtosis(BCL_Credit_Limit_Volume),
      Mean_Credit_Limit_Value = mean(Total_BCL_Credit_Limit), StdDev_Credit_Limit_Value = sd(Total_BCL_Credit_Limit),
      Variance_Credit_Limit_Value = var(Total_BCL_Credit_Limit ),
      Qtl_Credit_Limit_Val_Low = quantile(Total_BCL_Credit_Limit, 0.05, na.rm = TRUE), Min_Credit_Limit_Value = min(Total_BCL_Credit_Limit),
      Median_Credit_Limit_Val = median(Total_BCL_Credit_Limit, na.rm = TRUE), Max_Credit_Limit_Value = max(Total_BCL_Credit_Limit),
      Qtl_Credit_Limit_Val_High = quantile(Total_BCL_Credit_Limit, 0.95, na.rm = TRUE),
      Skewness_Credit_Limit_Value = skewness(Total_BCL_Credit_Limit),
      Kurtosis_Credit_Limit_Value = kurtosis(Total_BCL_Credit_Limit)
    ),
    by=c("Year_Month","grantor", "grantee")
    ];
  } ##end of If-Statement
  else { 
    print("This file only handles monthly frequency analysis. For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
  }#end of second if or else if statement
  temporarySummaryTable <- temporarySummaryTable[order(grantor),]; ##order the table by the grantor column 
  return(temporarySummaryTable);
}#end of getgrantorRecieverFILVTSVolumeValueSummaryStatistics


##The following method filters a summary statistical data table containing information about the payments amounts and further summarises
##this data by grantor-reciever FI pairing and year and month to get summary statistics for number (volume) of payments sent by the FI to all other 
#FIs combined
getGrantorGranteeFIPairLVTSVolumeSummaryStatistics <- function(inputDataTable, colNames){
    gc();
    temporarySummaryTable <- NULL;
    
    if("Total_BCL_Credit_Limit" %in% colNames & "grantor" %in% colNames
       & "BCL_Credit_Limit_Volume" %in% colNames){        #check if the key look-up column names are in (i.e. %in%) the list of column names
      
      ##the following statements assigns to the temporarySummaryTable the summary statistics including mean, standard deviation,
      ##frequency, median, min and max. 
      ##these are then grouped by grantor reciever pairings. This approach seems to make more sense from a code resuability
      ##standpoint. That is, it will be simply a matter of adding an additional grouping of year and month to the method to 
      ##extend and apply the method to the entire data sent and then derive summary statistics for each grantor-receiver pairing  
      ##by year and month without having to rewrite the whole method to do that calculation 
      temporarySummaryTable <- inputDataTable[ ,list(
        Total_BCL_Credit_Limit_Volume = sum(BCL_Credit_Limit_Volume),Total_BCL_Credit_Limit_Value= sum(Total_BCL_Credit_Limit),
        Mean_Credit_Limit_Volume = mean(BCL_Credit_Limit_Volume), StdDev_Credit_Limit_Volume = sd(BCL_Credit_Limit_Volume),
        Variance_Credit_Limit_Volume = var(BCL_Credit_Limit_Volume),
        Qtl_Credit_Limit_Vol_05 = quantile(BCL_Credit_Limit_Volume, 0.05, na.rm = TRUE),
        Min_Credit_Limit_Volume = min(BCL_Credit_Limit_Volume),
        Median_Credit_Limit_Vol = median(BCL_Credit_Limit_Volume, na.rm = TRUE), Max_Credit_Limit_Volume = max(BCL_Credit_Limit_Volume),
        Qtl_Credit_Limit_Vol_95 = quantile(BCL_Credit_Limit_Volume, 0.95, na.rm = TRUE),
        Skewness_Credit_Limit_Volume = skewness(BCL_Credit_Limit_Volume),
        Kurtosis_Credit_Limit_Volume = kurtosis(BCL_Credit_Limit_Volume),
        Mean_Credit_Limit_Value = mean(Total_BCL_Credit_Limit), StdDev_Credit_Limit_Value = sd(Total_BCL_Credit_Limit),
        Variance_Credit_Limit_Value = var(Total_BCL_Credit_Limit ),
        Qtl_Credit_Limit_Val_Low = quantile(Total_BCL_Credit_Limit, 0.05, na.rm = TRUE), Min_Credit_Limit_Value = min(Total_BCL_Credit_Limit),
        Median_Credit_Limit_Val = median(Total_BCL_Credit_Limit, na.rm = TRUE), Max_Credit_Limit_Value = max(Total_BCL_Credit_Limit),
        Qtl_Credit_Limit_Val_High = quantile(Total_BCL_Credit_Limit, 0.95, na.rm = TRUE),
        Skewness_Credit_Limit_Value = skewness(Total_BCL_Credit_Limit),
        Kurtosis_Credit_Limit_Value = kurtosis(Total_BCL_Credit_Limit)
      ),
      by=c("grantor", "grantee")
      ];
    }
    temporarySummaryTable <- temporarySummaryTable[order(grantor),]; ##order the table by the grantor column 
    return(temporarySummaryTable);
}

##The following method loops through the array of FI names and for each element of the FI names vector extracts the time series of volume and 
#value data for each
##FI from the input data table
getFIVolumeTimeSeriesDataTable <- function(listOfNames,inputDataTable, colNames){
  gc();
  tsData <- NULL;
  tempTS <- NULL;
  listOfDates <- NULL;
  tempDataTable <- NULL;
  tempTSDataTable <- NULL;
  #tempTSDataTable <- data.frame(list(grantor=character(), timeSeries=list()));
  #tempTSDataTable <- as.data.table(tempTSDataTable);
  if("grantor" %in% colNames){
    print("entered 1st if");
    print(now());
    etsList <- list();
    for (i in 1:length(listOfNames) ) {
      etsList[i] = list(ts());
    }
    tempTSDataTable <- tempTSDataTable[,list(grantor=listOfNames)];
  }
  else{
    print("invalid data table sent: check and resend");
  }
  
  #working base structure of the foreach loop implementation
  #foreach(i=listOfNames)%do% grantorFILVTSVolumeValueSummaryStatisticsDaily[grantor==i]$Total_Payments_Volume;
  for (i in listOfNames){ 
    print("entered 1st for");
    print(i);
    print(now());
    for(j in tempTSDataTable$grantor){
      print("entered 2nd for");
      print(j);
      print(now());
      if(i == j){
        print("entered 2nd if");
        print(j);
        print(i);
        print(now());
        tempDataTable <- filter(inputDataTable, (grantor==i)); #filter the inputDataTable by the grantor to select only those 
        #entires pertinant to grantor i
        #note that the filtering is done in the if(i==j){} statement to ensure the work is only done when needed
        ##end of filter inputDataTable[,grantor==i]
        if("Year_Month" %in% colNames){#check for the correct date format to use to order
          print("entered 3rd if");
          print("True");
          print(now());
          tempDataTable <- tempDataTable[order(Year_Month),];
          listOfDates <- unique(tempDataTable$Year_Month);##order the table by the Year_Month column 
        } else {
          print("This file only handles monthly frequency analysis. 
                For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
        } ##order the table by the Year_Month_Day column
        sort(listOfDates);#this is redundant since the tempDataTable is by default ordered by date 
        #but it is included as a failsafe to ensure that the dates are sorted in assending order
        tsData <- tempDataTable$Total_Payments_Volume;
        print("creating tempTimeSeries of FI: ");
        print(i)
        print(now());
        tempTS <- ts(tsData, start=c(year(listOfDates[1]), month(listOfDates[1])), 
                     end=c(year(listOfDates[length(listOfDates)]), month(listOfDates[length(listOfDates)])), frequency = 12,
                     names=c("Total_Payments_Volume"));#create a new time series
        print("adding tempTimeSeries to data.table of FI: ");
        print(i)
        print(now());
      }
    }
  }
  return(tempTSDataTable);
}


getFIVolumeTimeSeriesCollection <- function(listOfNames,inputDataTable, colNames){
  gc();
  tsData <- NULL;
  tempTS <- NULL;
  listOfDates <- NULL;
  tempDataTable <- NULL;
  tempTSList <- NULL;
  counter <- 0;
  if("grantor" %in% colNames){
    tempTSList <- list();
    counter <- counter+1;
  }
  else{
    print("invalid data table sent: check and resend");
  }
  
  #working base structure of the foreach loop implementation
  #foreach(i=listOfNames)%do% grantorFILVTSVolumeValueSummaryStatisticsDaily[grantor==i]$Total_Payments_Volume;
  for (i in listOfNames){ 
        tempDataTable <- filter(inputDataTable, (grantor==i)); #filter the inputDataTable by the grantor to select only those entires pertinant to grantor i
        #note that the filtering is done in the if(i==j){} statement to ensure the work is only done when needed
        ##end of filter inputDataTable[,grantor==i]
        if("Year_Month" %in% colNames){#check for the correct date format to use to order
          tempDataTable <- tempDataTable[order(Year_Month),];
          listOfDates <- unique(tempDataTable$Year_Month);
        } ##order the table by the Year_Month column 
        else {
          print("This file only handles monthly frequency analysis. For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
        } ##order the table by the Year_Month_Day column
        sort(listOfDates);#this is redundant since the tempDataTable is by default ordered by date 
        #but it is included as a failsafe to ensure that the dates are sorted in assending order
        tsData <- tempDataTable$Total_BCL_Credit_Limit_Value;
        tempTS <- ts(tsData, start=c(year(listOfDates[1]), month(listOfDates[1])), 
                     end=c(year(listOfDates[length(listOfDates)]), month(listOfDates[length(listOfDates)])), frequency = 12,
                     names=c("Total_BCL_Credit_Limit_Value"));#create a new time series
        #tempTSDataTable[grantor==i]$timeSeries <- tempTS;
        tempLST <- list(grantor=i, TimeSeries=tempTS);
        #names(tempLST) <- i;
        #tempTSList[[counter]] <- tempLST;
        tempTSList[[counter]] <- list(grantor=i, TimeSeries=tempTS);
    counter <- counter+1;
  }
  return(tempTSList);
}

##The following method loops through the array of FI names and for each element of the FI names vector extracts the time series of volume and value 
#data for each
##FI from the input data table
getFIVolumeValueTimeSeries <- function(inputDataTable, fiName, fieldOfInterest, colNames){
  gc();
  tsData <- NULL;
  tempTS <- NULL;
  listOfDates <- NULL;
  tempDataTable <- NULL;
  
  if("grantor" %in% colNames){#check if the field grantor is in the list of column headers of the input data table
    tempDataTable <- filter(inputDataTable, (grantor==fiName)); #filter the inputDataTable by the grantor to select only those entires 
                                                               #pertinant to the grantor with fiName
    if("Year_Month" %in% colNames){#check for the correct date format (Year_Month => monthly)
      tempDataTable <- tempDataTable[order(Year_Month),];#sort the tempDataTable by date
      listOfDates <- unique(tempDataTable$Year_Month);#extract the list of unique dates from the tempDataTable
    } ##order the table by the Year_Month column 
    else {#check for the correct date format (Year_Month_Day => daily)
      print("This file only handles monthly frequency analysis. For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
    }
    listOfDates <- sort(listOfDates);#this is redundant since the tempDataTable is by default ordered by date 
    if(fieldOfInterest == "volume"){#check the field of interest for analysis (volume => Total_Payments_Volume)
      tsData <- tempDataTable$Total_BCL_Credit_Limit_Volume;
    }else if(fieldOfInterest == "value"){#check the field of interest for analysis (value => Total_Payments_Value)
      tsData <- tempDataTable$Total_BCL_Credit_Limit_Value;
    }else{
      print("invalid data field entered: check and resend");
    }
    
    #the following if-else statments build the returned timeseries based on whether the required data frequency is daily or monthly
    if("Year_Month" %in% colNames){#check for the correct date format to use to build the timeseries object
      tempTS <- ts(tsData, start=c(year(listOfDates[1]), month(listOfDates[1])), 
                   end=c(year(listOfDates[length(listOfDates)]), month(listOfDates[length(listOfDates)])), frequency = 12,
                   names=c("Total_BCL_Credit_Limit_Value"));#create a new time series
    } ##order the table by the Year_Month column 
    else {
      print("This file only handles monthly frequency analysis. For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
     } ##order the table by the Year_Month_Day column
  } else{
    print("invalid data table sent: check and resend");
  }
  return(tempTS);
}


##<<<<<<<<<<<<<<<<<<<<<<<<<PROCESS REQUIRED CALCULATIONS AND PUBLISH OUTPUTS>>>>>>>>>>>>>>>>>>>>>>>>>
print("Build and Processing started");#used to record speed of computations
print(now());#used to record speed of computations
gc();
workingDataTable <- mergedLVTSBCLsYearsSpecYM;
#workingDataTable <- mergedLVTSTransYearMonth
rm(mergedLVTSBCLsYearsSpecYM);
#rm(mergedLVTSTransYearMonth);
vectorColumnNames <- getColumnNames(workingDataTable);
print("vectorColumnNames built");#used to record speed of computations
print(now());#used to record speed of computations
gc();
gc();
workingDataTableMonthlyByYear <- getAnnualDataRangeExtraction(workingDataTable,startYear,endYear,"Year_Month",vectorColumnNames);
rm(workingDataTable);
gc();
print("extracting list of FIs");
print(now());#used to record speed of computations
vectorColumnNames <- getColumnNames(workingDataTableMonthlyByYear);
vectorFINames <- getListOfFINames(workingDataTableMonthlyByYear, vectorColumnNames);
vectorOfFullDataSetDates <- getListOfFullDataSetDates(workingDataTableMonthlyByYear, vectorColumnNames);
gc();#free up some memory


print("printing test bilateral mean calculation");
print(now());#used to record speed of computations
#Print test output to ensure methods works
print(names(workingDataTableMonthlyByYear));
print(getMeanAmountByIndividualGrantorGranteePair(workingDataTableMonthlyByYear,"BOFACA", "ROYCCA", "credit_limit",vectorColumnNames));
gc();#free up some memory



if(!isDaily){#if analysis is not based on daily level granularity
  print("creating phase 1 summary statistics table based on values Monthly Frequency");
  print(now());#used to record speed of computations
  workingSummaryDataTableMonthlyByYear <- getValueSummaryStatisticsByGranteeAndByYearAndMonth(workingDataTableMonthlyByYear,
                                                                                              "credit_limit",vectorColumnNames);
  gc();#free up some memory
  #NOTE:
  ##assuming 18 FIs in LVTS all sending at least one payment to every other FI in LVTS, 
  #then the output table will from the following prints should have a maximum of 18x18 = 324 rows
  #assuming 18 FIs in LVTS all sending at least one payment to every other FI in LVTS, 
  #then the output table will from the following prints should have a maximum of 18x18x12 = 3888 rows per year
  print("creating volume/value phase 2 summary statistics table Monthly Frequency");
  print(now());#used to record speed of computations
  vectorColumnNames <- getColumnNames(workingSummaryDataTableMonthlyByYear);
  grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear <- getGrantorFILVTSVolumeValueSummaryStatistics(workingSummaryDataTableMonthlyByYear,
                                                                                                            vectorColumnNames);
  ##next save the summary data table
    grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear <- grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear[order(grantor),]; 
  ##order the table by the grantor column
  save(grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear, file=summaryGrantorFIStatsSavePath);
  grantorGranteeFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear <- 
    getGrantorGranteeFILVTSVolumeValueSummaryStatistics(workingSummaryDataTableMonthlyByYear, vectorColumnNames);
  grantorGranteeFITotalBCLVolumeValueSummaryStatistics <- 
    getGrantorGranteeFIPairLVTSVolumeSummaryStatistics(workingSummaryDataTableMonthlyByYear, vectorColumnNames);
  vectorColumnNames <- getColumnNames(grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear);
  vectorOfWorkingDataSetDates <- getListOfTImeSeriesDates(grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear);
  gc();#free up some memory
  
  print("creating timeSeries Tables/Collection Monthly Frequency");
  print(now());#used to record speed of computations
  grantorFILVTSBCLTimeSeriesDataTable <- 
    getFIVolumeTimeSeriesCollection(vectorFINames, grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear,vectorColumnNames);
  grantorFILVTSBCLTimeSeriesDataTable <- list.names(grantorFILVTSBCLTimeSeriesDataTable,"FITimeSeriesList");
  list.save(grantorFILVTSBCLTimeSeriesDataTable, grantorFILVTSBCLTimeSeriesDataTableSavePath);
  gc();#free up some memory
  
  
  print("creating timeSeries Monthly Frequency");
  print(now());#used to record speed of computations
  grantorFIsBCLVolumeTimeSeries  <- 
    getFIVolumeValueTimeSeries(grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear, "ROYCCA", "volume",vectorColumnNames);
  grantorFIsBCLValueTimeSeries <- 
    getFIVolumeValueTimeSeries(grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear, "ROYCCA", "value",vectorColumnNames);
  gc();#free up some memory
}else{
  print("This file only handles monthly frequency analysis. For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
  
  gc();#free up some memory
  
}



if(!isDaily){#if analysis is not based on daily level granularity
  ##<<<<<<<<<<<<<<<<<<<<<<<<<PROCESS REQUIRED CALCULATIONS AND PUBLISH OUTPUTS PLOTTING>>>>>>>>>>>>>>>>>>>>>>>>>
  print("creating charts Monthly Frequency");
  print(now());#used to record speed of computations
  ##the following ggplot2 plot depicts the minimum and maximum mean amount sent by each FI to a recieving FI. The plots generated are sperated 
  ##into seperate
  ##charts according to the reciever FI
  
  ##This displays the charts according to the payment recievers ensuring that the data is sequenced by month and year
  monthlyBCLsValueGrantedRangeScatterPlot <- ggplot(workingSummaryDataTableMonthlyByYear, 
                                                    aes(x=(Total_BCL_Credit_Limit/1000000), y=grantor, color=grantor))+
    geom_point(aes(order = sample(seq_along(Year_Month)))) + scale_x_continuous(label=dollar)  + facet_wrap(~ grantee);
  
  monthlyBCLsValueGrantedRangeScatterPlot <- monthlyBCLsValueGrantedRangeScatterPlot + labs(x="Total Credit Limit Granted (in millions)");
  
  ##This displays the charts according to the payment grantors ensuring that the data is sequenced by month and year
  monthlyBCLsValueGrantorRangeScatterPlot <- ggplot(workingSummaryDataTableMonthlyByYear, 
                                                    aes(x=(Total_BCL_Credit_Limit/1000000), y=grantee, color=grantee)) +
    geom_point(aes(order = sample(seq_along(Year_Month)))) + scale_x_continuous(label=dollar) + facet_wrap(~ grantor);
    
  monthlyBCLsValueGrantorRangeScatterPlot <- monthlyBCLsValueGrantorRangeScatterPlot + labs(x="Total Credit Limit Granted (in millions)");
  gc();#free up momory
  
  ##grantor Specific charts
  
  grantorFILVTSPaymentsValueBoxPlot <- ggplot(grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear,
                                              aes(x=grantor, y=(Total_BCL_Credit_Limit_Value/1000000), color=grantor)) +
    geom_boxplot() + scale_y_continuous(label=dollar);
  
  grantorFILVTSPaymentsValueBoxPlot <- grantorFILVTSPaymentsValueBoxPlot + 
    labs(y="Total Credit Limit Granted (in millions)");
  
  grantorFILVTSMeanValueSeriesLinePlot <- ggplot(grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear,
                                                 aes(x=Year_Month, y=(Mean_Credit_Limit_Value/1000000), color=grantor)) +
    geom_line(aes(order = sample(seq_along(Year_Month))))+ scale_y_continuous(label=dollar) + facet_wrap(~ grantor);
  
  grantorFILVTSMeanValueSeriesLinePlot <- grantorFILVTSMeanValueSeriesLinePlot + 
    labs(y="Mean Credit Limit Granted (in millions)");
  grantorFILVTSMeanValueSeriesLinePlot <- grantorFILVTSMeanValueSeriesLinePlot + labs(x="");
    
  grantorFILVTSTotalValueSeriesLinePlot <- ggplot(grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear, 
                                                 aes(x=Year_Month, y=(Total_BCL_Credit_Limit_Value/1000000), color=grantor)) +
    geom_line(aes(order = sample(seq_along(Year_Month)))) + scale_y_continuous(label=dollar) + facet_wrap(~ grantor);
  
  grantorFILVTSTotalValueSeriesLinePlot <- grantorFILVTSTotalValueSeriesLinePlot + 
    labs(y="Total Credit Limit Granted (in millions)");
  grantorFILVTSTotalValueSeriesLinePlot <- grantorFILVTSTotalValueSeriesLinePlot + labs(x="");
  

  
  
  
  gc();#free up momory
  
  print("publishing charts Monthly Frequency");
  print(now());#used to record speed of computations
  ##Display/Print charts
  print(monthlyBCLsValueGrantedRangeScatterPlot);
  print(monthlyBCLsValueGrantorRangeScatterPlot);
  
  print(grantorFILVTSPaymentsValueBoxPlot);

  print(grantorFILVTSMeanValueSeriesLinePlot);

  print(grantorFILVTSTotalValueSeriesLinePlot);

  gc();
  
}else{
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<VISUALISING THE DAILY DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  print("This file only handles monthly frequency analysis. For daily analysis, use lvts_Initial_Statistical_Testing_file_V03_Year_Month_Day.R file");
  print(now());#used to record speed of computations
  gc();#free up some memory
}

#write.xlsx(grantorGranteeFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear, grantorFILVTSBCLTimeSeriesXLSXSavePath);

#write.xlsx(grantorGranteeFITotalBCLVolumeValueSummaryStatistics, grantorFILVTSBCLTotalXLSXSavePath);

print("Build and Processing Complete");
print(now());
gc();