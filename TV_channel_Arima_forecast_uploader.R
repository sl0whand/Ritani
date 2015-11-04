
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

comcast_switch_date="2015-05-04"
adap_switch_date="2015-09-14"

comcast_ind=which(channel_sessions_long$date>comcast_switch_date & channel_sessions_long$date<adap_switch_date)
channel_sessions_long$tv.spend[comcast_ind]=0


   

## Organic Net Home Channel
var_name="organic.net.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]
na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)

###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)

###Forecasts

channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

channel_sessions_long=channel_sessions_long %>% rename(organic.net.home.forecast=forecast) 

## Organic Home Channel

var_name="organic.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]
na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)


###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 

 
###Forecasts

  
 channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

channel_sessions_long=channel_sessions_long %>% rename(organic.home.forecast=forecast) 


## Direct Net Home Channel

var_name="direct.net.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]

na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)


###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 

###Forecasts

  
 channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)


channel_sessions_long=channel_sessions_long %>% rename(direct.net.home.forecast=forecast) 


## Direct Home Channel
var_name="direct.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]
 

na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)

###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 

###Forecasts

  
 channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)


channel_sessions_long=channel_sessions_long %>% rename(direct.home.forecast=forecast) 


## Paid Brand Channel
var_name="paid.brand.sessions"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]

na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)


###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 
###Forecasts

 channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

channel_sessions_long=channel_sessions_long %>% rename(paid.brand.sessions.forecast=forecast) 

   

  
write.csv(channel_sessions_long,file="channel_sessions_long_with_forecasts_new_vendor.csv")
gs_upload("channel_sessions_long_with_forecasts_new_vendor.csv",sheet_title = "weekly_tv_channel_with_forecasts_new_vendor_upload_dont_edit")

   

