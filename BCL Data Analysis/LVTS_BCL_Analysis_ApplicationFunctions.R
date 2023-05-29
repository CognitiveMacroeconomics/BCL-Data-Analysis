##This script 
##
##
##
## Author : Segun Bewaji
## Creation Date : 08 Nov 2016 - 08 Nov 2016
## Modified : Segun Bewaji
## Modifications Made: 03 Nov 2016
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



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<DECLARE METHODS/FUNCTIONS>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##<<<<<<<<<<<<<<<<<<<<<<<<GLOBAL FUNCTIONS>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##This function is used to calculate the shares
fnShare <- function(x) x/sum(x);

##This function converts factors to numeric values.
##This function is required because of a fundamental omission or BUG in R that
##means R returns the levels of a factor when using the as.numeric() default function
##the developers of R also do not provide a convineit way to get the result one wants when
##trying to convert factors to numeric. Another piece of stupidity in languages like R
convertFactorToNumeric <- function(x) {as.numeric(levels(x))[x]}


##This function is used to round numbers to a specified number of decimal places
roundingToSpecifiedDP <- function(x, k) format(round(x, k), nsmall=k);

##This function replaces all the NA values in a data.table object with the input value
naReplaceFunction <- function(dt, val){
  for(i in seq_along(dt)){
    set(dt, i=which(is.na(dt[[i]])), j=i, value=val)
  }
  return(dt);
}


#Function to use to find the Nth largest value 
findNthLargest <- function(inputVector, nthLargest){
  len <- length(inputVector);
  if(nthLargest>len){
    warning('nth Largest greater than length(inputVector).  Setting nthLargest=length(inputVector)');
    nthLargest <- length(inputVector);
  }
  sort(inputVector,partial=len-nthLargest+1)[len-nthLargest+1];
}

#compute that statistical mode of an input vector
StatisticalMode <- function(inputVector) {
  uniqueItemsFromInputVector <- unique(inputVector)
  uniqueItemsFromInputVector[which.max(tabulate(match(inputVector, uniqueItemsFromInputVector)))]
}

##Function/method to remove the speficed tranche number trN from the data table
removeSpecifiedTranche <- function(inputDataTable, trN){
  temporaryTable <- filter(inputDataTable, (inputDataTable$tranche)== trN);
  return(temporaryTable);
}


##The monthlyDataExtraction method/function takes an input data.table/data frame and compresses it to a smaller
##version containing the data for the specific month and year of interest
getMonthlyDataRangeExtraction <- function(inputDataTable, yr, startMth, endMth, lookupColumnName, colNames){
  gc();
  temporaryTable <- NULL;
  if(endMth <= startMth){
    return(print("start month is greater than end month: please chakc and correct entry"))
  }else
    if(lookupColumnName %in% colNames & lookupColumnName == "Date"){ #check if the key look-up column name is
      #in (i.e. %in%) the list of column names
      ##the following filter and this IF-statement in general is hardcoded because R does not appear to have a way of 
      ##passing strings to functions to get column contents. THe ideal implementation would simply be
      ##"inputDataTable$lookupColumnName" and would not require checking that "lookupColumnName == "Year_Month""
      
      temporaryTable <-  filter(inputDataTable, (year(inputDataTable$Date)== yr 
                                                 & month(inputDataTable$Date) >= startMth & month(inputDataTable$Date) <= endMth)
      );##end of filter statement
    };#end of if statment
  #temporaryTable <- as.data.table(temporaryTable);
  return(temporaryTable)
}


##This function moves columns around in data.table
##SOURCE: http://news.mrdwab.com/post/moveme/
##USAGE:
##myvec <- letters[1:10]
##moveColumns(myvec, "a last; b, e, g before d; c first; h after j")
#  [1] "c" "b" "e" "g" "d" "f" "i" "j" "h" "a"
##Thus, assuming that you wanted to reorder the columns of a data.frame named “mydf”, you would use:
##  mydf[moveColumns(names(mydf), "your move command")]
##Similarly, if you wanted to reorder the columns of a data.table named “DT”, you would use:
##setcolorder(DT, moveColumns(names(mydf), "your move command"))
moveColumns <- function(invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], ",|\\s+"),
                        function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp)-1
      } else if (A == "after") {
        after <- match(ba, temp)
      }
    } else if (A == "first") {
      after <- 0
    } else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}


##The following method extracts the unique column names from the input data table
getColumnNames <- function(inputDataTable){
  colNames <- names(inputDataTable);#create a vector or array of column heading names from the input table
  return(colNames)
}


##This method returns the list of unique FI names in the input data set based on the key field supplied keyFld
getListOfFINames <- function(inputDataTable, keyFld){
  colNames <- getColumnNames(inputDataTable);
  gc();
  tempFINamesVector <- NULL;
  if(keyFld == "defaulter" & keyFld %in% colNames){#check if the field defaulter is in the list of column headers and if so get the unique list of defaulter names
    tempFINamesVector  <- unique(as.character(inputDataTable$defaulter));
  } else if(keyFld == "grantor" & keyFld %in% colNames){#otherwise check if the field grantor is in the list of column headers and if so get the unique list of grantor names
    tempFINamesVector <- unique(as.character(inputDataTable$grantor));
  } else{
    tempFINamesVector <- unique(as.character(inputDataTable$grantor));
  }
  return(tempFINamesVector);#return the list of unique FI names
}


##The following method loops through the array of FI names and for each element of the FI names vector extracts the time series of volume and value data for each
##FI from the input data table
getBilateralTransactionsRelationships <- function(inputDataTable, fiNamesVector, keyFlFd, keyDataFd){
  colNames <- getColumnNames(inputDataTable);
  gc();
  inputDataTable <- as.data.table(inputDataTable);
  tempDataTable <- NULL;
  tempDTList <- NULL;
  counter <- 0;
  
  
  ##The following if statement is for elegant error handling....
  ##This method/function and R script more generally is to create time series objects from an input data table of 
  ##transactions data. Hence the specificity of hard coding values such as "sender".
  ##The code can be easily transformed into a more generic implementation simply by passing a string object into the function for
  ##the reference field/column (in this case sender) and replacing the "sender" with the passed string object/value
  if(keyFlFd %in% colNames & keyDataFd %in% colNames){
    print("entered 1st if");
    print(now());
    tempDTList <- list();
    counter <- counter+1;
  }
  else{
    print("invalid data table sent: check and resend");
  }
  
  #This for-loop creates a two element list containing the FI name and couterparty credit risk table for each FI in the 
  #list of named FIs given the FI's type (i.e. defaulter or grantor or otherwise)
  for (i in fiNamesVector){ 
    print("entered 1st for");
    print(i);
    print(now());
    if(keyFlFd == "sender") {
      jList <- list();
      jCounter <- 1;
      for(j in fiNamesVector){
        if(i != j){
          tempDataTable <- filter(inputDataTable, ((inputDataTable$sender==i & inputDataTable$receiver==j) | 
                                                     (inputDataTable$receiver==i & Leo
                                                      $sender==j))
                                  ); #filter the inputDataTable by the defaulter to select only those entires pertinant to defaulter i
          jList[[jCounter]] <- list(FinancialInstitution=j, FIBilateralData=tempDataTable);
          jCounter <- jCounter+1;
          }
        tempDTList <- tempDTList[[counter]] <- list(FinancialInstitution=i, FIBilateralDataList=jList);
      }
      
    } else if(keyFlFd == "receiver"){
      jList <- list();
      jCounter <- 1;
      for(j in fiNamesVector){
        if(i != j){
          tempDataTable <- filter(inputDataTable, ((inputDataTable$receiver==i & inputDataTable$sender==j) |
                                                     (inputDataTable$sender==i & inputDataTable$receiver==j))
                                  ); #filter the inputDataTable by the defaulter to select only those entires pertinant to defaulter i
          jList[[jCounter]] <- list(FinancialInstitution=j, FIBilateralData=tempDataTable);
          jCounter <- jCounter+1;
          }
        tempDTList <- tempDTList[[counter]] <- list(FinancialInstitution=i, FIBilateralDataList=jList);
      }
    } else{
      print("supplied FI field/column name is not yet supported. 
            Update the getFISpecificCounterpartyCreditRiskDataTable funtion as required");
    }
    tempDTList[[counter]] <- list(FinancialInstitution=i, FICCRData=tempDataTable);
    counter <- counter+1;
    }##End of loop through list of FI names
  return(tempDTList);
}


filterZerosOutOfCreditDataSet <- function(inputDataTable, keyDtFld){
  if(keyDtFld == "exposureAtDefault"){
    tempDataTable <- filter(inputDataTable, (inputDataTable$exposureAtDefault<0)); #filter the inputDataTable to remove zeros from the exposureAtDefault column
  }else if(keyDtFld == "aggregateExposureAtDefault"){
    tempDataTable <- filter(inputDataTable, (inputDataTable$aggregateExposureAtDefault<0)); #filter the inputDataTable to remove zeros from the exposureAtDefault column
  }
  
  return(tempDataTable);
}


