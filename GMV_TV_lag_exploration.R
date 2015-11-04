library(readr)
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

#GMV Data

GMV_data=tbl_df(read_csv("daily_orders_rev_gmv.csv"))
GMV_data$date=as.Date(GMV_data$Date,"%m/%d/%Y")
GMV_data=GMV_data %>% select(date,Revenue)
GMV_data$channel="GMV"

#combining date
GMV_study=rbind(GMV_data,
                            sub_study %>% select(tv.spend,date) %>% 
                              rename(Revenue=tv.spend) %>% 
                              mutate(channel="tv.spend")) %>% arrange(date)


#Forcing Mondays
GMV_study$weekdays=weekdays(GMV_study$date)
levels(as.factor(GMV_study$weekdays))

tue_inds=which(GMV_study$weekdays=="Tuesday")
GMV_study$date[tue_inds]=GMV_study$date[tue_inds]-days(1)

wed_inds=which(GMV_study$weekdays=="Wednesday")
GMV_study$date[wed_inds]=GMV_study$date[wed_inds]-days(2)

thu_inds=which(GMV_study$weekdays=="Thursday")
GMV_study$date[thu_inds]=GMV_study$date[thu_inds]-days(3)

fri_inds=which(GMV_study$weekdays=="Friday")
GMV_study$date[fri_inds]=GMV_study$date[fri_inds]-days(4)

sat_inds=which(GMV_study$weekdays=="Saturday")
GMV_study$date[sat_inds]=GMV_study$date[sat_inds]-days(5)

sun_inds=which(GMV_study$weekdays=="Sunday")
GMV_study$date[sun_inds]=GMV_study$date[sun_inds]-days(6)




GMV_study=GMV_study %>% group_by(channel,date) %>%   summarise(Revenue=sum(Revenue))

GMV_study=GMV_study %>% spread(key=channel,value=Revenue)

#Replacing missing tv spend with 0
GMV_study$tv.spend[which(is.na(GMV_study$tv.spend))]=0
GMV_study=GMV_study %>% na.omit()

TV_lit_date=min(GMV_study$date[which(GMV_study$tv.spend>0)])
TV_end_date=max(GMV_study$date[which(GMV_study$tv.spend>0)])
GMV_study=GMV_study %>% filter(date>=TV_lit_date) %>% arrange(desc(date))

#always sort by descending date first!!
ccf_temp=ccf(GMV_study$tv.spend,GMV_study$GMV,plot=FALSE)
ccf_df=tbl_df(data.frame(Correlation=ccf_temp$acf,Lag=ccf_temp$lag))

# GMV_study=GMV_study %>% mutate(lag_1=lag(GMV,1))
#  GMV_study=GMV_study %>% mutate(lag_2=lag(GMV,2))
#  GMV_study=GMV_study %>% mutate(lag_3=lag(GMV,3))
#  GMV_study=GMV_study %>% mutate(lag_4=lag(GMV,4))
#  GMV_study=GMV_study %>% mutate(lag_5=lag(GMV,5))
#  
#  cor(GMV_study$GMV,GMV_study$tv.spend)
#  cor(GMV_study$GMV[2:nrow(GMV_study)],GMV_study$lag_1[2:nrow(GMV_study)])
#  cor(GMV_study$GMV[3:nrow(GMV_study)],GMV_study$lag_2[3:nrow(GMV_study)])
#  cor(GMV_study$GMV[4:nrow(GMV_study)],GMV_study$lag_3[4:nrow(GMV_study)])
#  cor(GMV_study$GMV[5:nrow(GMV_study)],GMV_study$lag_4[5:nrow(GMV_study)])
 
ggplot(ccf_df %>% filter(Lag>=0),aes(x=Lag,y=Correlation))+ theme_bw()+
  geom_point()+ggtitle(paste("TV Spend Delayed Effect On GMV",TV_lit_date,"to",TV_end_date))+xlab("Lag (Weeks)")
  

