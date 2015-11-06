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

source("report_functions.R")

 
load(file="channel_sessions_long.rda")
freq=52


#Pretending Comcast never happened
# comcast_switch_date="2015-05-04"
# adap_switch_date="2015-09-14"
# comcast_ind=which(channel_sessions_long$date>comcast_switch_date & channel_sessions_long$date<adap_switch_date)
# channel_sessions_long$tv.spend[comcast_ind]=0

#Train only through September
# train_end_date="2015-10-01"
# train_end_ind=which(channel_sessions_long$date>=train_end_date)
# 
# channel_sessions_long$direct.net.home[train_end_ind]=NA
# channel_sessions_long$direct.home[train_end_ind]=NA
# channel_sessions_long$organic.net.home[train_end_ind]=NA
# channel_sessions_long$organic.home[train_end_ind]=NA
# channel_sessions_long$paid.brand[train_end_ind]=NA

   

## Organic Net Home Channel
var_name="organic.net.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]

na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]
title_paste="Organic Net of Home"

###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)

###Forecasts

channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

channel_sessions_long=channel_sessions_long %>% rename(organic.net.home.forecast=forecast) 


### approximation of lift- only directionally accurate - violates several model assumptions
lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]

tv_lift=lin_coef*channel_sessions_long$tv.spend+sqr_coef*channel_sessions_long$tv.spend^2
channel_sessions_long$organic.net.home.lift=tv_lift


 ggplot(channel_sessions_long)+
   geom_line(aes(x=date,y=organic.net.home))+
   geom_line(aes(x=date,y=organic.net.home.lift))

## Organic Home Channel
var_name="organic.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]


na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]

title_paste="Organic Home"

###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)

###Forecasts

channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

channel_sessions_long=channel_sessions_long %>% rename(organic.home.forecast=forecast) 

lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]

tv_lift=lin_coef*channel_sessions_long$tv.spend+sqr_coef*channel_sessions_long$tv.spend^2
channel_sessions_long$organic.home.lift=tv_lift

# ggplot(channel_sessions_long)+
#   geom_line(aes(x=date,y=organic.home))+
#   geom_line(aes(x=date,y=tv_lift))

## Direct Net Home Channel

var_name="direct.net.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]


na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]

title_paste="Direct Net Home"


###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 

###Forecasts

  
channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)


channel_sessions_long=channel_sessions_long %>% rename(direct.net.home.forecast=forecast) 



lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]

tv_lift=lin_coef*channel_sessions_long$tv.spend+sqr_coef*channel_sessions_long$tv.spend^2
channel_sessions_long$direct.net.home.lift=tv_lift

# ggplot(channel_sessions_long)+
#   geom_line(aes(x=date,y=direct.net.home))+
#   geom_line(aes(x=date,y=direct.net.home.lift))

## Direct Home Channel
var_name="direct.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]

na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]

title_paste="Direct Home"

###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 

###Forecasts

  
 channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)


channel_sessions_long=channel_sessions_long %>% rename(direct.home.forecast=forecast) 



lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]

tv_lift=lin_coef*channel_sessions_long$tv.spend+sqr_coef*channel_sessions_long$tv.spend^2
channel_sessions_long$direct.home.lift=tv_lift

# ggplot(channel_sessions_long)+
#   geom_line(aes(x=date,y=direct.home))+
#   geom_line(aes(x=date,y=direct.home.lift))


## Paid Brand Channel
var_name="paid.brand"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]
na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]

title_paste="Paid Brand"

###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 
###Forecasts

channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

channel_sessions_long=channel_sessions_long %>% rename(paid.brand.sessions.forecast=forecast) 


lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]

tv_lift=lin_coef*channel_sessions_long$tv.spend+sqr_coef*channel_sessions_long$tv.spend^2
channel_sessions_long$paid.brand.lift=tv_lift

# ggplot(channel_sessions_long)+
#   geom_line(aes(x=date,y=paid.brand))+
#   geom_line(aes(x=date,y=paid.brand.lift))

  
write.csv(channel_sessions_long,file="temp.csv")
gs_upload("temp.csv",
          sheet_title =paste("weekly_tv_channel_with_forecasts",as.character(Sys.Date()),sep="_"))

   

