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


GMV_study$year=strftime(GMV_study$date,format="%y") 
GMV_study$week=strftime(GMV_study$date,format="%W") 
GMV_study$date=as.Date(strptime(paste(GMV_study$year,
                                                  (as.numeric(GMV_study$week)*7),
                                                  sep=" "),format="%Y %j") +years(2000))


GMV_study=GMV_study %>% group_by(channel,year,week) %>% 
  summarise(Revenue=sum(Revenue),date=first(date))

rm_vars=c("year","week")
rm_cols=which(names(GMV_study) %in% rm_vars)
GMV_study=GMV_study[,-rm_cols] %>% na.omit()

GMV_study=GMV_study %>% spread(key=channel,value=Revenue)

#Replacing missing tv spend with 0
GMV_study$tv.spend[which(is.na(GMV_study$tv.spend))]=0
GMV_study=GMV_study %>% na.omit()


GMV_study=GMV_study %>% filter(date>=min(GMV_study$date[which(GMV_study$tv.spend>0)]))
ccf_temp=ccf(GMV_study$tv.spend,GMV_study$GMV,plot=FALSE)
ccf_df=tbl_df(data.frame(Correlation=ccf_temp$acf,Lag=ccf_temp$lag))

ggplot(ccf_df %>% filter(Lag>=0),aes(x=Lag,y=Correlation))+ theme_bw()+
  geom_point()+ggtitle("TV Spend Delayed Effect On GMV")+xlab("Lag (Weeks)")
  
