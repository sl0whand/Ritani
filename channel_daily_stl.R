library(tidyr)
library(dplyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(forecast)
library(stats)
library(pander)
library(TTR)
library(googlesheets)

##pulling older daily session data
a=gs_title("channeldata with home")
channel_sessions_long=a %>% gs_read(ws = "gadata")
channel_sessions_long=channel_sessions_long %>% rename(channel=channelGrouping)
#converting date
channel_sessions_long$date=as.Date(paste(substr(as.character(channel_sessions_long$date),5,6),
                                         substr(as.character(channel_sessions_long$date),7,8),
                                         substr(as.character(channel_sessions_long$date),1,4),
                                         sep="-"), "%m-%d-%Y")

#removing garbage vars
rm_vars=c("pageviews","X")
rm_cols=which(names(channel_sessions_long) %in% rm_vars)
channel_sessions_long=channel_sessions_long[,-rm_cols]


#plot everything
ggplot(channel_sessions_long) + theme_bw() +
  geom_line(aes(x=date,y=sessions,color=channel))


#casting
channel_sessions_long=channel_sessions_long %>% spread(key=channel,value=sessions)


## Organic Channel
i=3
vars=c("Branded Paid Search", "Direct", "Organic Search")
study_var=channel_sessions_long[,grep(vars[i],names(channel_sessions_long))][[1]]
study_var=na.omit(study_var)
title_paste=names(channel_sessions_long)[grep(vars[i],names(channel_sessions_long))]
###2nd order polynomial fit

#frequency shoud be 52 (weeks per year), but we don't have two full periods
ts_var=ts(study_var, frequency = 365)
stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
if (class(t)=="try-error") {
  ts_var=ts(study_var, frequency = 183)
  stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
}
plot(stl_obj)

#arima model
arima=auto.arima(ts_var,allowdrift=FALSE)
summary(arima)
#forecasting 28 days
arima_fcast=forecast(arima,h=28)
plot(arima_fcast)


arima_R2=round(cor(fitted(arima),ts_var)^2,2)
tmp_df=data.frame(coef(arima))
tmp_df=rbind(tmp_df,arima_R2)
rownames(tmp_df)[length(rownames(tmp_df))]="R2"
pander(tmp_df)
