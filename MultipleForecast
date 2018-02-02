

## Time Series with Week Seasonal periods
```{r }
df <- data.frame(WeekDate=as.Date(character()),
                 Depot=character(), 
                 SalesForecast=numeric()) 
for (i in 1:length(depots)) {
  
        tempData<-sales.aggr[sales.aggr$DepotName==depots[i],]
        if(nrow(tempData)>150){
          startDate<-min(tempData$WeekDate)
          weekTS <- msts(tempData$sales,seasonal.periods = c(365.25/7),start = decimal_date(as.Date(startDate)))
          myWeeks<-tempData$WeekDate
          nbWeeks<-nrow(tempData)
          week.seq<-seq(as.Date(myWeeks[nbWeeks-12]), length=36, by = "week")
        # train/test
          temp.train<- window(weekTS, start=c(decimal_date(as.Date(myWeeks[1]))), end=c(decimal_date(as.Date(myWeeks[nbWeeks-12]))))
          temp.test<- window(weekTS, start=decimal_date(as.Date(myWeeks[nbWeeks-12])), end=decimal_date(as.Date(myWeeks[nbWeeks])))
        # model train
          fit.stlf<- stlf(temp.train)
          fr.stlf<- forecast(fit.stlf,h=36)
        # store results
          newRows<-data.frame(WeekDate =week.seq,Depot=rep(depots[i],36),SalesForecast=as.numeric(fr.stlf$mean))
          df<-rbind(df,newRows)
        }
}
tempDF<-merge(df.Budget, df.Forecast, by = c("DepotName","WeekYear"), all = TRUE)
myDF<-merge(tempDF, df.Sales, by = c("DepotName","WeekYear"), all = TRUE)
write.csv(myDF, file = "Budget_VS_Sales_Forecast.csv")
```
