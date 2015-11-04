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

tv_data=tbl_df(read.xlsx("tvanalysis.xlsx",sheet=2,rows=c(1:95),cols=c(1,4:20)))
tv_data$date=as.Date(as.character(tv_data$date),"%m/%d/%Y")+years(2000)
na_inds=which(is.na(tv_data$date))
#not great coding practice here
for (ind in na_inds) {tv_data$date[ind]=tv_data$date[(ind-1)]+weeks(1)}

#Subsetting data
study_vars=c("date","tv.spend")
study_cols=which(names(tv_data) %in% study_vars)
sub_study=tv_data[,study_cols]  %>% arrange(desc(date))


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


#appending tv spend
channel_sessions_long=rbind(channel_sessions_long,
                            sub_study %>% select(tv.spend,date) %>% 
                              rename(sessions=tv.spend) %>% 
                              mutate(channel="tv.spend"))


# channel_sessions_long$year=strftime(channel_sessions_long$date,format="%y") 
# channel_sessions_long$week=strftime(channel_sessions_long$date,format="%W") 
# channel_sessions_long$date=as.Date(strptime(paste(channel_sessions_long$year,
#                                                   (as.numeric(channel_sessions_long$week)*7),
#                                                   sep=" "),format="%Y %j") +years(2000))
# 

#Forcing Mondays
channel_sessions_long$weekdays=weekdays(channel_sessions_long$date)
levels(as.factor(channel_sessions_long$weekdays))

tue_inds=which(channel_sessions_long$weekdays=="Tuesday")
channel_sessions_long$date[tue_inds]=channel_sessions_long$date[tue_inds]-days(1)

wed_inds=which(channel_sessions_long$weekdays=="Wednesday")
channel_sessions_long$date[wed_inds]=channel_sessions_long$date[wed_inds]-days(2)

thu_inds=which(channel_sessions_long$weekdays=="Thursday")
channel_sessions_long$date[thu_inds]=channel_sessions_long$date[thu_inds]-days(3)

fri_inds=which(channel_sessions_long$weekdays=="Friday")
channel_sessions_long$date[fri_inds]=channel_sessions_long$date[fri_inds]-days(4)

sat_inds=which(channel_sessions_long$weekdays=="Saturday")
channel_sessions_long$date[sat_inds]=channel_sessions_long$date[sat_inds]-days(5)

sun_inds=which(channel_sessions_long$weekdays=="Sunday")
channel_sessions_long$date[sun_inds]=channel_sessions_long$date[sun_inds]-days(6)



channel_sessions_long=channel_sessions_long %>% group_by(date,channel) %>% 
  summarise(sessions=sum(sessions))


#casting
channel_sessions_long=channel_sessions_long %>% spread(key=channel,value=sessions)
names(channel_sessions_long) <- sub(" ", ".", names(channel_sessions_long))

#Replacing missing tv spend with 0
channel_sessions_long$tv.spend[which(is.na(channel_sessions_long$tv.spend))]=0

#fabricating variables
channel_sessions_long$direct.net.home=channel_sessions_long$Direct-channel_sessions_long$Direct.home
channel_sessions_long$direct.home=channel_sessions_long$Direct.home
channel_sessions_long$organic.net.home=channel_sessions_long$Organic.Search-channel_sessions_long$Organic.Search.home
channel_sessions_long$organic.home=channel_sessions_long$Organic.Search.home
channel_sessions_long$paid.brand.sessions=channel_sessions_long$`Branded.Paid Search`



study_vars=c("date","tv.spend","direct.net.home","direct.home",
             "organic.net.home","organic.home",
             "paid.brand.sessions")
study_cols=which(names(channel_sessions_long) %in% study_vars)
channel_sessions_long=channel_sessions_long[,study_cols]  %>% arrange(date)

save(channel_sessions_long,file="channel_sessions_long.rda")
write.csv(channel_sessions_long,file="channel_sessions_long.csv")
gs_upload("channel_sessions_long.csv",sheet_title = "weekly_tv_channel_upload_dont_edit")


