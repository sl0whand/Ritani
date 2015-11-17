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
############
#Pulling TV data
############
tv_data=tbl_df(read.xlsx("tvanalysis_wgmv.xlsx",sheet="multi-channel comparison",rows=c(1:94),cols=c(1,4:20)))
tv_data$date=as.Date(as.character(tv_data$date),"%m/%d/%y")
na_inds=which(is.na(tv_data$date))
#not great coding practice here
for (ind in na_inds) {tv_data$date[ind]=tv_data$date[(ind-1)]+weeks(1)}

#Subsetting data
study_vars=c("date","tv.spend")
study_cols=which(names(tv_data) %in% study_vars)
sub_study=tv_data[,study_cols]  %>% arrange(desc(date))

##################
#Pulling GMV data
##################
GMV_data=tbl_df(read.xlsx("tvanalysis_wgmv.xlsx",sheet="GMV by week",cols=c(1,3)))
GMV_data$date=as.Date(as.character(GMV_data$Wk.Started),"%m/%d/%y")


################################
#pulling older daily session data
##################################
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
channel_sessions_long=rbind(gs_channeldata[,1:4],gs_channeldataroot[,1:4])
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

###################
#appending data sources
##################
channel_sessions_long=rbind(channel_sessions_long,
                            GMV_data %>% select(GMV,date) %>% 
                              rename(sessions= GMV) %>% 
                              mutate(channel="GMV"))

channel_sessions_long=rbind(channel_sessions_long,
                            sub_study %>% select(tv.spend,date) %>% 
                              rename(sessions=tv.spend) %>% 
                              mutate(channel="tv.spend"))


#Forcing Mondays to combine with weekly tv data later
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


#functionally aggregating sessions by week
channel_sessions_long=channel_sessions_long %>% group_by(date,channel) %>% 
  summarise(sessions=sum(sessions))


#casting to wide form
channel_sessions_long=channel_sessions_long %>% spread(key=channel,value=sessions)

#renaming troublesome variables
names(channel_sessions_long) <- sub(" ", ".", names(channel_sessions_long))
names(channel_sessions_long) <- sub(" ", ".", names(channel_sessions_long))


#Replacing missing tv spend with 0
channel_sessions_long$tv.spend[which(is.na(channel_sessions_long$tv.spend))]=0


#fabricating variables
channel_sessions_long$direct.all=channel_sessions_long$Direct
channel_sessions_long$direct.net.home=channel_sessions_long$direct.all-channel_sessions_long$Direct.home
channel_sessions_long$direct.home=channel_sessions_long$Direct.home
channel_sessions_long$organic.net.home=channel_sessions_long$Organic.Search-channel_sessions_long$Organic.Search.home
channel_sessions_long$organic.all=channel_sessions_long$Organic.Search.home
channel_sessions_long$organic.home=channel_sessions_long$organic.all

channel_sessions_long$paid.brand=channel_sessions_long$Branded.Paid.Search


# Keeping only pertinent variables
study_vars=c("date","tv.spend","direct.net.home","direct.home","direct.all",
             "organic.net.home","organic.home","organic.all",
             "paid.brand","GMV")
study_cols=which(names(channel_sessions_long) %in% study_vars)
channel_sessions_long=channel_sessions_long[,study_cols]  %>% arrange(date)
#I don't believe the first observation is real- it must be an aggregation error
channel_sessions_long=channel_sessions_long[-1,]
#remove most recent sessions observation due to aggregation error (and recent week being incomplete)
study_vars=c("direct.net.home","direct.home","direct.all",
             "organic.net.home","organic.home","organic.all",
             "paid.brand","GMV")
for (var in study_vars) {
  channel_sessions_long[max(which(!is.na(channel_sessions_long[,which(names(channel_sessions_long)==var)]))),which(names(channel_sessions_long)==var)]=NA
}

#save the rda to used later in the pipeline
 save(channel_sessions_long,file="channel_sessions_long.rda")
 


