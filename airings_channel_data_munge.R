library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(forecast)
library(stats)
library(pander)
library(TTR)
library(googlesheets)
library(readxl)

airings_data=tbl_df(read_excel("Ritani Weekly TV Report 10.19.15 with historical.xlsx",
                              sheet=2))
airings_data$date=as.Date(airings_data$Date.Time)
airings_data=airings_data %>% group_by(date) %>% summarise(Airings=n())



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
# channel_sessions_long=tbl_df(read.xlsx("channeldata_with_home.xlsx",sheet=1))

#converting date
channel_sessions_long$date=as.Date(paste(substr(as.character(channel_sessions_long$date),5,6),
                                         substr(as.character(channel_sessions_long$date),7,8),
                                         substr(as.character(channel_sessions_long$date),1,4),
                                         sep="-"), "%m-%d-%Y")

#appending airings
channel_sessions_long=rbind(channel_sessions_long,
                            airings_data %>% select(Airings,date) %>% 
                              rename(sessions=Airings) %>% 
                              mutate(channel="Airings"))

#casting
channel_sessions_long=channel_sessions_long %>% spread(key=channel,value=sessions)
names(channel_sessions_long) <- sub(" ", ".", names(channel_sessions_long))

#Replacing missing tv spend with 0
channel_sessions_long$Airings[which(is.na(channel_sessions_long$Airings))]=0

#fabricating variables
channel_sessions_long$direct.net.home=channel_sessions_long$Direct-channel_sessions_long$Direct.home
channel_sessions_long$direct.home=channel_sessions_long$Direct.home
channel_sessions_long$organic.net.home=channel_sessions_long$Organic.Search-channel_sessions_long$Organic.Search.home
channel_sessions_long$organic.home=channel_sessions_long$Organic.Search.home
channel_sessions_long$paid.brand.sessions=channel_sessions_long$`Branded.Paid Search`



study_vars=c("date","Airings","direct.net.home","direct.home",
             "organic.net.home","organic.home",
             "paid.brand.sessions")
study_cols=which(names(channel_sessions_long) %in% study_vars)
channel_sessions_long=channel_sessions_long[,study_cols]  %>% arrange(date)

save(channel_sessions_long,file="channel_sessions_long_airings.rda")

write.csv(channel_sessions_long,file="channel_sessions_long_airings.csv")
gs_upload("channel_sessions_long_airings.csv",sheet_title = "daily_airings_channel_upload_dont_edit")
