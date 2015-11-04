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
library(readxl)

#Pulling airings data
airings_data=tbl_df(read_excel("Ritani Weekly TV Report 10.19.15 with historical.xlsx",
                               sheet=2))
airings_data$date=as.Date(airings_data$Date.Time)
airings_data=airings_data %>% group_by(date) %>% summarise(Airings=n())


#GMV Data

GMV_data=tbl_df(read_csv("daily_orders_rev_gmv.csv"))
GMV_data$date=as.Date(GMV_data$Date,"%m/%d/%Y")
GMV_data=GMV_data %>% select(date,Revenue)
GMV_data$channel="GMV"

#combining date
GMV_study=rbind(GMV_data,
                airings_data %>% select(Airings,date) %>% 
                  rename(Revenue=Airings) %>% 
                  mutate(channel="Airings")) %>% arrange(date)

GMV_study=GMV_study %>% spread(key=channel,value=Revenue)

#Replacing missing tv spend with 0
GMV_study$Airings[which(is.na(GMV_study$Airings))]=0
GMV_study=GMV_study %>% na.omit()




lit_date=min(GMV_study$date[which(GMV_study$Airings>0)])
end_date=max(GMV_study$date[which(GMV_study$Airings>0)])
GMV_study=GMV_study %>% filter(date>=lit_date) %>% arrange(desc(date))

#always sort by descending date first!!
ccf_temp=ccf(GMV_study$Airings,GMV_study$GMV,plot=FALSE)
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
  geom_point()+ggtitle(paste("TV Airings Delayed Effect On GMV",lit_date,"to",end_date))+xlab("Lag (Days)")


