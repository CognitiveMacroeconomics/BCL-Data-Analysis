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
##Data loading paths. Do not change the file names unless the file in which the full dataset is stored in is changed
fiStaticDataLoadPath <- c("C:/Projects/FundingDataTables/FIStaticData.Rdata");#will load the data table: fiStaticDataFile
fiBCLcDataLoadPath <- c("C:/Projects/LVTSBCLDataTables/YearsSpecificLVTSBCLYM2005-15DataSeries.Rdata")

##Data extraction configiration information
isDaily <- FALSE;
startYear  <- 2005;
endYear <- 2014;

##Data savinging paths
summaryGrantorFIStatsSavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLSummaryStatisticsMonthlyByYear2005-2014.Rdata");
grantorFILVTSBCLTimeSeriesDataTableSavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLTimeSeriesDataTableMonthlyByYear2005-2014.Rdata");
grantorFILVTSBCLTimeSeriesXLSXSavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLTimeSeriesXLSXSeries2005-2014.xlsx");
grantorFILVTSBCLTotalXLSXSavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLTotalXLSXSeries2005-2014.xlsx");
grantorFILVTSBCLTimeSeriesSTATASavePath <- c("C:/Projects/FundingDataTables/grantorFILVTSBCLTimeSeriesSTATASeries2005-2014.dta");

##NULL declarations
workingDataTable <- NULL; ##working datatable
workingDataTableMonthlyByYear  <- NULL; ##working datatable
workingSummaryDataTableMonthlyByYear <- NULL;
grantorFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear <- NULL;
grantorGranteeFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear <- NULL;
grantorGranteeFITotalBCLQuarterlySummaryStatistics <- NULL;
grantorGranteeFITotalBCLMonthlySummaryStatistics <- NULL;
averageDailyBCLGrantedPerQuarterByFI <- NULL;
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
load(fiBCLcDataLoadPath);
load(fiStaticDataLoadPath);
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
                                                   Median_Credit_Limit = median(credit_limit, na.rm = TRUE), 
                                                   Max_Credit_Limit = max(credit_limit), StdDev_Credit_Limit = sd(credit_limit),
                                                   Mean_Credit_Limit = mean(credit_limit),
                                                   Total_BCL_Credit_Limit = sum(credit_limit, na.rm = TRUE)
    ),
    by=c("Year_Month","grantee")];
    temporarySummaryTable <- temporarySummaryTable[order(Year_Month, grantee),]; ##order the table by the grantor column 
  }##end of IF-Statement"
  
  return(temporarySummaryTable);
}


##This method returns a data table containing the mean value of payments sent to a given FI by another FI
getValueSummaryStatisticsByGranteeAndByYearAndQuarter <- function(inputDataTable, fieldOfInterest, colNames){
  gc();
  temporarySummaryTable <- NULL;
  if(fieldOfInterest %in% colNames & "grantee" %in% colNames & "Year_Quarter" %in% colNames
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
                                                   Median_Credit_Limit = median(credit_limit, na.rm = TRUE), 
                                                   Max_Credit_Limit = max(credit_limit), StdDev_Credit_Limit = sd(credit_limit),
                                                   Mean_Credit_Limit = mean(credit_limit),
                                                   Total_BCL_Credit_Limit = sum(credit_limit, na.rm = TRUE)
    ),
    by=c("Year_Quarter","grantee")];
    temporarySummaryTable <- temporarySummaryTable[order(Year_Quarter, grantee),]; ##order the table by the grantor column 
  }##end of IF-Statement"
  
  return(temporarySummaryTable);
}

##The following method filters a summary statistical data table containing information about the payments amounts and further summarises
##this data by grantor-reciever FI pairing and year and month to get summary statistics for number (volume) of payments sent by the FI to all other 
#FIs combined
getGrantorGranteeFIPairLVTSQuarterlySummaryStatistics <- function(inputDataTable, colNames){
  gc();
  temporarySummaryTable <- NULL;
  
  if("credit_limit" %in% colNames & "grantor" %in% colNames 
     & "Year_Quarter" %in% colNames){ #check if the key look-up column names are in (i.e. %in%) the list of column names
    
    ##the following statements assigns to the temporarySummaryTable the summary statistics including mean, standard deviation,
    ##frequency, median, min and max. 
    ##these are then grouped by grantor reciever pairings. This approach seems to make more sense from a code resuability
    ##standpoint. That is, it will be simply a matter of adding an additional grouping of year and month to the method to 
    ##extend and apply the method to the entire data sent and then derive summary statistics for each grantor-receiver pairing  
    ##by year and month without having to rewrite the whole method to do that calculation 
    temporarySummaryTable <- inputDataTable[ ,list(
      Total_BCL_Credit_Limit = sum(credit_limit),
      Mean_Credit_Limit = mean(credit_limit), StdDev_Credit_Limit = sd(credit_limit),
      Min_Credit_Limit = min(credit_limit),
      Median_Credit_Limit = median(credit_limit, na.rm = TRUE), Max_Credit_Limit = max(credit_limit)
    ),
    by=c("Year_Quarter","grantor", "grantorID", "grantee", "granteeID")
    ];
  }
  temporarySummaryTable <- temporarySummaryTable[order(grantor),]; ##order the table by the grantor column 
  return(temporarySummaryTable);
}

##The following method filters a summary statistical data table containing information about the payments amounts and further summarises
##this data by grantor-reciever FI pairing and year and month to get summary statistics for number (volume) of payments sent by the FI to all other 
#FIs combined
getGrantorGranteeFIPairLVTSMonthlySummaryStatistics <- function(inputDataTable, colNames){
  gc();
  temporarySummaryTable <- NULL;
  
  if("credit_limit" %in% colNames & "grantor" %in% colNames 
     & "Year_Month" %in% colNames){ #check if the key look-up column names are in (i.e. %in%) the list of column names
    
    ##the following statements assigns to the temporarySummaryTable the summary statistics including mean, standard deviation,
    ##frequency, median, min and max. 
    ##these are then grouped by grantor reciever pairings. This approach seems to make more sense from a code resuability
    ##standpoint. That is, it will be simply a matter of adding an additional grouping of year and month to the method to 
    ##extend and apply the method to the entire data sent and then derive summary statistics for each grantor-receiver pairing  
    ##by year and month without having to rewrite the whole method to do that calculation 
    temporarySummaryTable <- inputDataTable[ ,list(
      Total_BCL_Credit_Limit = sum(credit_limit),
      Mean_Credit_Limit = mean(credit_limit), StdDev_Credit_Limit = sd(credit_limit),
      Min_Credit_Limit = min(credit_limit),
      Median_Credit_Limit = median(credit_limit, na.rm = TRUE), Max_Credit_Limit = max(credit_limit)
    ),
    by=c("Year_Month","grantor", "grantorID", "grantee", "granteeID")
    ];
  }
  temporarySummaryTable <- temporarySummaryTable[order(grantor),]; ##order the table by the grantor column 
  return(temporarySummaryTable);
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
workingDataTableMonthlyByYear <- workingDataTableMonthlyByYear[ credit_limit != 0];
workingDataTableMonthlyByYear <- workingDataTableMonthlyByYear[ grantor != "BCANCA"];#removing data related to bank of canada
#removing data related to ICICCA. This is a temporary exclusion because it only became a full LVTS participant in Q3 of 2015 hence has no
#falls outside of the 2005-2014 data range chosen for LVTS transactions data
#This is a minor ommission that can easily be revesered at a later date.
#workingDataTableMonthlyByYear <- workingDataTableMonthlyByYear[ grantor != "ICICCA"];
#workingDataTableMonthlyByYear <- workingDataTableMonthlyByYear[ grantee != "ICICCA"];
gc();
workingDataTableMonthlyByYear_df <- as.data.frame(workingDataTableMonthlyByYear);
fiStaticDataFile_df <- as.data.frame(fiStaticDataFile);
workingDataTableMonthlyByYear = merge(workingDataTableMonthlyByYear_df, fiStaticDataFile_df, by.x="grantee", by.y="memberBIC");
colnames(workingDataTableMonthlyByYear)[colnames(workingDataTableMonthlyByYear)=="memberID"] <- "granteeID";
colnames(workingDataTableMonthlyByYear)[colnames(workingDataTableMonthlyByYear)=="memberName"] <- "granteeFullName";

rm(workingDataTableMonthlyByYear_df);#remove workingDataTableMonthlyByYear_df to free up memory since not in use
gc(); #free up memory

workingDataTableMonthlyByYear = merge(workingDataTableMonthlyByYear, fiStaticDataFile_df, by.x="grantor", by.y="memberBIC");
colnames(workingDataTableMonthlyByYear)[colnames(workingDataTableMonthlyByYear)=="memberID"] <- "grantorID";
colnames(workingDataTableMonthlyByYear)[colnames(workingDataTableMonthlyByYear)=="memberName"] <- "grantorFullName";
gc(); #free up memory
workingDataTableMonthlyByYear <- as.data.table(workingDataTableMonthlyByYear);
workingDataTableMonthlyByYear <- workingDataTableMonthlyByYear[ by=c("Year_Quater","grantor", "grantorID", "grantee", "granteeID")];
rm(workingDataTable);
gc(); #free up memory
rm(fiStaticDataFile_df);#remove fiStaticDataFile_df to free up memory since not in use
workingDataTableMonthlyByYear <- as.data.table(workingDataTableMonthlyByYear);# convert workingDataTableMonthlyByYear back to a data.table object
gc(); #free up memory



if(!isDaily){#if analysis is not based on daily level granularity
  #NOTE:
  ##assuming 18 FIs in LVTS all sending at least one payment to every other FI in LVTS, 
  #then the output table will from the following prints should have a maximum of 18x18 = 324 rows
  #assuming 18 FIs in LVTS all sending at least one payment to every other FI in LVTS, 
  #then the output table will from the following prints should have a maximum of 18x18x12 = 3888 rows per year
  print("creating phase 1 summary statistics table Monthly Frequency");
  print(now());#used to record speed of computations
  vectorColumnNames <- getColumnNames(workingDataTableMonthlyByYear);
  print(vectorColumnNames);
  print(now());#used to record speed of computations
  grantorGranteeFITotalBCLQuarterlySummaryStatistics <- 
    getGrantorGranteeFIPairLVTSQuarterlySummaryStatistics(workingDataTableMonthlyByYear, vectorColumnNames);
  grantorGranteeFITotalBCLMonthlySummaryStatistics <- 
    getGrantorGranteeFIPairLVTSMonthlySummaryStatistics(workingDataTableMonthlyByYear, vectorColumnNames);
 
  grantorGranteeFITotalBCLQuarterlySummaryStatistics <- 
    grantorGranteeFITotalBCLQuarterlySummaryStatistics[ order(Year_Quarter,grantor, grantorID, grantee, granteeID),];
  
  grantorGranteeFITotalBCLMonthlySummaryStatistics <- 
    grantorGranteeFITotalBCLMonthlySummaryStatistics[ order(Year_Month,grantor, grantorID, grantee, granteeID),];
  
  
  gc();#free up some memory
}else{
  print("This file only handles monthly frequency analysis.");
  
  gc();#free up some memory
  
}



if(!isDaily){#if analysis is not based on daily level granularity
  ##<<<<<<<<<<<<<<<<<<<<<<<<<PROCESS REQUIRED CALCULATIONS AND PUBLISH OUTPUTS PLOTTING>>>>>>>>>>>>>>>>>>>>>>>>>
  print("creating charts Quarterly Frequency");
  print(now());#used to record speed of computations
  ##the following ggplot2 plot depicts the minimum and maximum mean amount sent by each FI to a recieving FI. The plots generated are sperated 
  ##into seperate
  ##charts according to the reciever FI
  

  ##This displays the charts according to the payment grantors ensuring that the data is sequenced by month and year
  monthlyBCLsValueGrantorRangeScatterPlot <- ggplot(grantorGranteeFITotalBCLMonthlySummaryStatistics, 
                                                    aes(x=(Mean_Credit_Limit/1000000), y=grantee, color=grantee)) +
    geom_point(aes(order = sample(seq_along(Year_Month)))) + scale_x_continuous(label=dollar) + facet_wrap(~ grantor);
  
  monthlyBCLsValueGrantorRangeScatterPlot <- monthlyBCLsValueGrantorRangeScatterPlot + 
    labs(x="Mean Daily Bilateral Credit Limit Granted Per Quater (in millions)");
  gc();#free up momory
  
  ##grantor Specific charts
  
  grantorFILVTSPaymentsValueBoxPlot <- ggplot(grantorGranteeFITotalBCLQuarterlySummaryStatistics,
                                              aes(x=grantor, y=(Mean_Credit_Limit/1000000), color=grantor)) +
    geom_boxplot() + scale_y_continuous(label=dollar);
  
  grantorFILVTSPaymentsValueBoxPlot <- grantorFILVTSPaymentsValueBoxPlot + 
    labs(y="Mean Daily Bilateral Credit Limit Granted Per Quater (in millions)");
  
  grantorFILVTSMeanValueSeriesLinePlot <- ggplot(grantorGranteeFITotalBCLQuarterlySummaryStatistics,
                                                 aes(x=Year_Quarter, y=(Mean_Credit_Limit/1000000), color=grantee)) +
    geom_line(aes(group=grantee, order = sample(seq_along(Year_Quarter))))+ scale_y_continuous(label=dollar) + facet_wrap(~ grantor);
  
  grantorFILVTSMeanValueSeriesLinePlot <- grantorFILVTSMeanValueSeriesLinePlot + 
    labs(y="Mean Daily Bilateral Credit Limit Granted Per Quater (in millions)");
  grantorFILVTSMeanValueSeriesLinePlot <- grantorFILVTSMeanValueSeriesLinePlot + labs(x="Years and Quarters");
  

  gc();#free up momory
  
  print("publishing charts Quarterly Frequency");
  print(now());#used to record speed of computations
  ##Display/Print charts
  print(monthlyBCLsValueGrantorRangeScatterPlot);
  
  print(grantorFILVTSPaymentsValueBoxPlot);
  
  print(grantorFILVTSMeanValueSeriesLinePlot);

  gc();
  
}else{
  ##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<VISUALISING THE DAILY DATA>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  print("This file only handles monthly frequency analysis not daily analysis");
  print(now());#used to record speed of computations
  gc();#free up some memory
}

#write.xlsx(grantorGranteeFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear, grantorFILVTSBCLTimeSeriesXLSXSavePath);
write.xlsx(as.data.frame(grantorGranteeFITotalBCLQuarterlySummaryStatistics, optional = TRUE), grantorFILVTSBCLTotalXLSXSavePath);
#write.xlsx(grantorGranteeFITotalBCLMonthlySummaryStatistics, grantorFILVTSBCLTotalXLSXSavePath);
#write.xlsx(grantorGranteeFILVTSBCLVolumeValueSummaryStatisticsMonthlyByYear, grantorFILVTSBCLTimeSeriesSTATASavePath);
grantorGranteeFITotalBCLQuarterlyDF <- as.data.frame(grantorGranteeFITotalBCLQuarterlySummaryStatistics);
#write.dta(as.data.frame(grantorGranteeFITotalBCLQuarterlyDF, optional = TRUE), grantorFILVTSBCLTimeSeriesSTATASavePath);
#write.dta(as.data.frame(grantorGranteeFITotalBCLQuarterlyDF, optional = TRUE), "grantorFILVTSBCLTimeSeriesSTATASeries2005-2015.dta");

print("Build and Processing Complete");
print(now());
gc();