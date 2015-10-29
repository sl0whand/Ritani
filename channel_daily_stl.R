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
################
###Data Munging
################

##pulling older daily session data
a=gs_title("channeldata with home")
gs_channeldata=a %>% gs_read(ws = "channeldata")
gs_channeldataroot=a %>% gs_read(ws = "channeldataroot")
channel_home_levels=levels(as.factor(gs_channeldataroot$ga.channelGrouping))
# replacing channel names
for (lev in channel_home_levels) {
  swap_inds=which(gs_channeldataroot$ga.channelGrouping==lev)
  gs_channeldataroot$ga.channelGrouping[swap_inds]=paste(gs_channeldataroot$ga.channelGrouping[swap_inds],"home",sep=".")
  
}
gs_channeldataroot[,3]=NULL
channel_sessions_long=rbind(gs_channeldata,gs_channeldataroot)
channel_sessions_long[,4]=NULL
channel_sessions_long=channel_sessions_long %>% rename(date=ga.date)
channel_sessions_long=channel_sessions_long %>% rename(channel=ga.channelGrouping)
channel_sessions_long=channel_sessions_long %>% rename(sessions=ga.sessions)



#casting
channel_sessions_long=channel_sessions_long %>% spread(key=channel,value=sessions)
names(channel_sessions_long) <- sub(" ", ".", names(channel_sessions_long))


#fabricating variables
channel_sessions_long$direct.net.home=channel_sessions_long$Direct-channel_sessions_long$Direct.home
channel_sessions_long$direct.home=channel_sessions_long$Direct.home
channel_sessions_long$organic.net.home=channel_sessions_long$Organic.Search-channel_sessions_long$Organic.Search.home
channel_sessions_long$organic.home=channel_sessions_long$Organic.Search.home
channel_sessions_long$paid.brand.sessions=channel_sessions_long$`Branded.Paid Search`



study_vars=c("date","direct.net.home","direct.home",
             "organic.net.home","organic.home",
             "paid.brand.sessions")
study_cols=which(names(channel_sessions_long) %in% study_vars)
channel_sessions_long=channel_sessions_long[,study_cols]  %>% arrange(desc(date))

channel_sessions_long$date=as.Date(paste(substr(as.character(channel_sessions_long$date),5,6),
                                         substr(as.character(channel_sessions_long$date),7,8),
                                         substr(as.character(channel_sessions_long$date),1,4),
                                         sep="-"), "%m-%d-%Y")

#Common seasonal trend
channel_session_gathered=channel_sessions_long %>% gather(key=channel,value=sessions,2:6)
ts_var=ts(channel_session_gathered$sessions, frequency = 365)
plot(stl_obj)

## Organic Channel
i=4
study_var=channel_sessions_long[,grep(study_vars[i],names(channel_sessions_long))][[1]]
na_inds=which(is.na(study_var))
if (length(na_inds)>0) {
  tv=channel_sessions_long$tv.spend[-na_inds]
  study_var=study_var[-na_inds]
}

title_paste=names(channel_sessions_long)[grep(study_vars[i],names(channel_sessions_long))]

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
