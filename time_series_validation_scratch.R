tv_data=tbl_df(read.xlsx("tvanalysis.xlsx",sheet=2,rows=c(1:95),cols=c(1,4:20)))
tv_data$date=as.Date(as.character(tv_data$date),"%m/%d/%Y")+years(2000)
na_inds=which(is.na(tv_data$date))
for (ind in na_inds) {tv_data$date[ind]=tv_data$date[(ind-1)]+weeks(1)}

#Subsetting data
study_vars=c("date","tv.spend","organic","organic.home","direct.home","paid.brand.sessions")
study_cols=which(names(tv_data) %in% study_vars)
sub_study=na.omit(tv_data[,study_cols]  %>% arrange(desc(date)))
sub_study=sub_study[-1,]
sub_study$paid.brand.sessions=as.numeric(as.character(sub_study$paid.brand.sessions))


#frequency shoud be 52 (weeks per year), but we don't have two full periods
a10=ts(sub_study$organic, frequency = 13)
a10=ts(sub_study$organic.home, frequency = 13)
a10=ts(sub_study$direct.home, frequency = 13)
a10=ts(sub_study$paid.brand.sessions, frequency = 13)

pred_hor=5
k <- 60 # minimum data length for fitting a model
n <- length(a10)
eff=n-k-pred_hor
mae2<- matrix(NA,eff)



for(i in 1:eff){
  xshort <-a10[1:(k+(i-1))]
  xnext <- a10[(k+i):(k+(i-1)+pred_hor)]
  fit2 <- Arima(xshort, order=c(1,1,0),include.drift=TRUE, lambda=0, method="ML",xreg=c(sub_study$tv.spend[1:(k+(i-1))]))
  # fit2 <- auto.arima(xshort)
  fcast2 <- forecast(fit2,xreg=c(sub_study$tv.spend[1:(k+(i-1))]), h=pred_hor)
  mae2[i] <- abs(fcast2[['mean']]-xnext)
}

plot(1:eff, mae2, type="l", col=2, xlab="horizon", ylab="MAE" )




