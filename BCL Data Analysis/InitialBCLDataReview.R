##Assessing BCL extending behaviour of LVTS participants


require(data.table);
require(ggplot2);


load("C:/Projects/FundingDataTables/Cleaned Collateral Data/dateTimeLVTSBCLDataSeries2005-2016.Rdata")
gc();

LVTSBCLDataSeries[,Date:=as.Date(date.time)]

LVTSBCLDataSeries <- LVTSBCLDataSeries[(hour(date.time)>=0 & hour(date.time)<19),];

LVTSBCLDataSeries <- LVTSBCLDataSeries[(ExtendedValue > 0.0),];

fiLevelLVTSBCLDataSeries <- LVTSBCLDataSeries[,list(maxBCL=max(ExtendedValue), minBCL=min(ExtendedValue), medianBCL=median(ExtendedValue)), by=c("grantor", "Date")]

fiLevelLVTSBCLDataSeries2006To2009 <- fiLevelLVTSBCLDataSeries[(year(Date)>2005 & year(Date)<2010),];

fiLevelLVTSBCLDataSeries2008 <- fiLevelLVTSBCLDataSeries[(year(Date)==2008),];

fiLevelLVTSBCLDataSeries2010To2013 <- fiLevelLVTSBCLDataSeries[(year(Date)>2009 & year(Date)<2014),];


plotMaxDailyBCLExtended <- ggplot(fiLevelLVTSBCLDataSeries, aes(x=Date, y=(maxBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "black", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Largest Bilateral Credit Limits Extended (millions)");

plotMinDailyBCLExtended <- ggplot(fiLevelLVTSBCLDataSeries, aes(x=Date, y=(minBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "red", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Lowest Bilateral Credit Limits Extended (millions)");

plotMedianDailyBCLExtended <- ggplot(fiLevelLVTSBCLDataSeries, aes(x=Date, y=(medianBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "blue", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Median Bilateral Credit Limits Extended (millions)");


gc();
plotMaxDailyBCLExtended2006To2009 <- ggplot(fiLevelLVTSBCLDataSeries2006To2009, aes(x=Date, y=(maxBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "black", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Largest Bilateral Credit Limits Extended (millions)");

plotMinDailyBCLExtended2006To2009 <- ggplot(fiLevelLVTSBCLDataSeries2006To2009, aes(x=Date, y=(minBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "red", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Lowest Bilateral Credit Limits Extended (millions)");

plotMedianDailyBCLExtended2006To2009 <- ggplot(fiLevelLVTSBCLDataSeries2006To2009, aes(x=Date, y=(medianBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "blue", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Median Bilateral Credit Limits Extended (millions)");


gc();
plotMaxDailyBCLExtended2008 <- ggplot(fiLevelLVTSBCLDataSeries2008, aes(x=Date, y=(maxBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "black", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Largest Bilateral Credit Limits Extended (millions)");

plotMinDailyBCLExtended2008 <- ggplot(fiLevelLVTSBCLDataSeries2008, aes(x=Date, y=(minBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "red", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Lowest Bilateral Credit Limits Extended (millions)");

plotMedianDailyBCLExtended2008 <- ggplot(fiLevelLVTSBCLDataSeries2008, aes(x=Date, y=(medianBCL)/1000000), colour=grantor, group=grantor) + 
  geom_line(colour = "blue", size = 0.5)  + 
  facet_wrap(~grantor) + 
  labs(x = "Date", y = "Median Bilateral Credit Limits Extended (millions)");


gc();
print(plotMaxDailyBCLExtended);
print(plotMinDailyBCLExtended);
print(plotMedianDailyBCLExtended);
gc()
print(plotMaxDailyBCLExtended2006To2009);
print(plotMinDailyBCLExtended2006To2009);
print(plotMedianDailyBCLExtended2006To2009);


gc()
print(plotMaxDailyBCLExtended2008);
print(plotMinDailyBCLExtended2008);
print(plotMedianDailyBCLExtended2008);



plotMedianDailyBCLExtended2008Comp <- ggplot(fiLevelLVTSBCLDataSeries2008, aes(x=Date), color=variable, show.legend=FALSE) + 
  geom_line(aes(y=(maxBCL)/1000000), colour = "black", size = 0.5)  +
  geom_line(aes(y=(medianBCL)/1000000), colour = "blue", size = 0.5) +
  geom_line(aes(y=(minBCL)/1000000), colour = "red", size = 0.5)  + 
  labs(x = "Date", y = "Intraday Bilateral Credit Limits Extended (millions)", color = "Value\nExtended\n") +
  scale_colour_manual(labels=c("Largest BCL","Median BCL","Lowest BCL"),
                      values=c("black","blue","red"), 
                      name="Value\nExtended",
                      breaks = c("maxBCL", "medianBCL", "minBCL"))  + 
  facet_wrap(~grantor) + 
  theme(legend.position="top") +
  theme_bw();
##Print Chart
print(plotMedianDailyBCLExtended2008Comp);


plotMedianDailyBCLExtended2006To2009Comp <- ggplot(fiLevelLVTSBCLDataSeries2006To2009, aes(x=Date), color=variable, show.legend=FALSE) + 
  geom_line(aes(y=(maxBCL)/1000000), colour = "black", size = 0.5)  +
  geom_line(aes(y=(medianBCL)/1000000), colour = "blue", size = 0.5) +
  geom_line(aes(y=(minBCL)/1000000), colour = "red", size = 0.5)  + 
  labs(x = "Date", y = "Intraday Bilateral Credit Limits Extended (millions)", color = "Value\nExtended\n") +
  scale_colour_manual(labels=c("Largest BCL","Median BCL","Lowest BCL"),
                      values=c("black","blue","red"), 
                      name="Value\nExtended",
                      breaks = c("maxBCL", "medianBCL", "minBCL"))  + 
  facet_wrap(~grantor) + 
  theme(legend.position="top") +
  theme_bw();
##Print Chart
print(plotMedianDailyBCLExtended2006To2009Comp);



plotMedianDailyBCLExtendedComp <- ggplot(fiLevelLVTSBCLDataSeries, aes(x=Date), color=variable, show.legend=FALSE) + 
  geom_line(aes(y=(maxBCL)/1000000), colour = "black", size = 0.5)  +
  geom_line(aes(y=(medianBCL)/1000000), colour = "blue", size = 0.5) +
  geom_line(aes(y=(minBCL)/1000000), colour = "red", size = 0.5)  + 
  labs(x = "Date", y = "Intraday Bilateral Credit Limits Extended (millions)", color = "Value\nExtended\n") +
  scale_colour_manual(labels=c("Largest BCL","Median BCL","Lowest BCL"),
                      values=c("black","blue","red"), 
                      name="Value\nExtended",
                      breaks = c("maxBCL", "medianBCL", "minBCL"))  + 
  facet_wrap(~grantor) + 
  theme(legend.position="top") +
  theme_bw();
##Print Chart
print(plotMedianDailyBCLExtendedComp);

plotMedianDailyBCLExtended2010To2013Comp <- ggplot(fiLevelLVTSBCLDataSeries2010To2013, aes(x=Date), color=variable, show.legend=FALSE) + 
  geom_line(aes(y=(maxBCL)/1000000), colour = "black", size = 0.5)  +
  geom_line(aes(y=(medianBCL)/1000000), colour = "blue", size = 0.5) +
  geom_line(aes(y=(minBCL)/1000000), colour = "red", size = 0.5)  + 
  labs(x = "Date", y = "Intraday Bilateral Credit Limits Extended (millions)", color = "Value\nExtended\n") +
  scale_colour_manual(labels=c("Largest BCL","Median BCL","Lowest BCL"),
                      values=c("black","blue","red"), 
                      name="Value\nExtended",
                      breaks = c("maxBCL", "medianBCL", "minBCL"))  + 
  facet_wrap(~grantor) + 
  theme(legend.position="top") +
  theme_bw();
##Print Chart
print(plotMedianDailyBCLExtended2010To2013Comp);


gc()