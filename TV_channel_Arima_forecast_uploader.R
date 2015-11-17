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
library(zoo)

source("report_functions.R")

 
load(file="channel_sessions_long.rda")
freq=52
################
###This commented code is poor practice only done by mandate- ideally it should never be implemented
# Pretending Comcast never happened
# comcast_switch_date="2015-05-04"
# adap_switch_date="2015-09-14"
# comcast_ind=which(channel_sessions_long$date>comcast_switch_date & channel_sessions_long$date<adap_switch_date)
# channel_sessions_long$tv.spend[comcast_ind]=0

# Train only through September
# train_end_date="2015-10-01"
# train_end_ind=which(channel_sessions_long$date>=train_end_date)
# 
# channel_sessions_long$direct.net.home[train_end_ind]=NA
# channel_sessions_long$direct.home[train_end_ind]=NA
# channel_sessions_long$organic.net.home[train_end_ind]=NA
# channel_sessions_long$organic.home[train_end_ind]=NA
# channel_sessions_long$paid.brand[train_end_ind]=NA
###########
   

## Organic Net Home Channel
#limiting variables to corresponding non-missing values
var_name="organic.net.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]

na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]


###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)

###Forecasts

channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

#renaming variables to be like this channel
channel_sessions_long=channel_sessions_long %>% rename(organic.net.home.forecast=forecast) 
channel_sessions_long=channel_sessions_long %>% rename(organic.net.home.lift=tv_lift) 
channel_sessions_long=channel_sessions_long %>% rename(organic.net.home.lift.low.bound=tv_lift_low_bound) 
channel_sessions_long=channel_sessions_long %>% rename(organic.net.home.lift.upper.bound=tv_lift_upper_bound) 

# Plots for sanity check
#  ggplot(channel_sessions_long)+
#    geom_line(aes(x=date,y=organic.net.home))+
#    geom_line(aes(x=date,y=organic.net.home.lift))

## Organic Home Channel
#limiting variables to corresponding non-missing values
var_name="organic.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]


na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]



###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)

###Forecasts

channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

#renaming variables to be like this channel
channel_sessions_long=channel_sessions_long %>% rename(organic.home.forecast=forecast) 
channel_sessions_long=channel_sessions_long %>% rename(organic.home.lift=tv_lift) 
channel_sessions_long=channel_sessions_long %>% rename(organic.home.lift.low.bound=tv_lift_low_bound) 
channel_sessions_long=channel_sessions_long %>% rename(organic.home.lift.upper.bound=tv_lift_upper_bound) 

## Organic All Channel
#limiting variables to corresponding non-missing values
var_name="organic.all"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]


na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]



###ARIMAX model with TV spend

tv_arima=arima_func(stepwise_model,study_var,tv,freq)

###Forecasts

channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

#renaming variables to be like this channel
channel_sessions_long=channel_sessions_long %>% rename(organic.all.forecast=forecast) 
channel_sessions_long=channel_sessions_long %>% rename(organic.all.lift=tv_lift) 
channel_sessions_long=channel_sessions_long %>% rename(organic.all.lift.low.bound=tv_lift_low_bound) 
channel_sessions_long=channel_sessions_long %>% rename(organic.all.lift.upper.bound=tv_lift_upper_bound) 


# Plots for sanity check
# ggplot(channel_sessions_long)+
#   geom_line(aes(x=date,y=organic.home))+
#   geom_line(aes(x=date,y=tv_lift))

## Direct Net Home Channel
#limiting variables to corresponding non-missing values
var_name="direct.net.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]
na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]


###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 

###Forecasts

  
channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)

#renaming variables to be like this channel

channel_sessions_long=channel_sessions_long %>% rename(direct.net.home.forecast=forecast) 
channel_sessions_long=channel_sessions_long %>% rename(direct.net.home.lift=tv_lift) 
channel_sessions_long=channel_sessions_long %>% rename(direct.net.home.lift.low.bound=tv_lift_low_bound) 
channel_sessions_long=channel_sessions_long %>% rename(direct.net.home.lift.upper.bound=tv_lift_upper_bound) 

# Plots for sanity check
# ggplot(channel_sessions_long)+
#   geom_line(aes(x=date,y=direct.net.home))+
#   geom_line(aes(x=date,y=direct.net.home.lift))

## Direct Home Channel
#limiting variables to corresponding non-missing values
var_name="direct.home"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]

na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]


###ARIMAX model with TV spend
  
tv_arima=arima_func(stepwise_model,study_var,tv,freq)
 

###Forecasts

  
 channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)
 #renaming variables to be like this channel
 

channel_sessions_long=channel_sessions_long %>% rename(direct.home.forecast=forecast) 
channel_sessions_long=channel_sessions_long %>% rename(direct.home.lift=tv_lift) 
channel_sessions_long=channel_sessions_long %>% rename(direct.home.lift.low.bound=tv_lift_low_bound) 
channel_sessions_long=channel_sessions_long %>% rename(direct.home.lift.upper.bound=tv_lift_upper_bound) 


# Plots for sanity check
# ggplot(channel_sessions_long)+
#   geom_line(aes(x=date,y=direct.home))+
#   geom_line(aes(x=date,y=direct.home.lift))


## Direct All Channel
#limiting variables to corresponding non-missing values
var_name="direct.all"
study_var=channel_sessions_long[,which(names(channel_sessions_long)==var_name)][[1]]

na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=na.omit(study_var)
date=channel_sessions_long$date[-na_inds]


###ARIMAX model with TV spend

tv_arima=arima_func(stepwise_model,study_var,tv,freq)


###Forecasts


channel_sessions_long=add_forecast_to_df(channel_sessions_long,tv_arima,var_name)
#renaming variables to be like this channel


channel_sessions_long=channel_sessions_long %>% rename(direct.all.forecast=forecast) 
channel_sessions_long=channel_sessions_long %>% rename(direct.all.lift=tv_lift) 
channel_sessions_long=channel_sessions_long %>% rename(direct.all.lift.low.bound=tv_lift_low_bound) 
channel_sessions_long=channel_sessions_long %>% rename(direct.all.lift.upper.bound=tv_lift_upper_bound) 



## Paid Brand Channel
#limiting variables to corresponding non-missing values
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
#renaming variables to be like this channel

channel_sessions_long=channel_sessions_long %>% rename(paid.brand.sessions.forecast=forecast) 
channel_sessions_long=channel_sessions_long %>% rename(paid.brand.lift=tv_lift) 
channel_sessions_long=channel_sessions_long %>% rename(paid.brand.lift.low.bound=tv_lift_low_bound) 
channel_sessions_long=channel_sessions_long %>% rename(paid.brand.lift.upper.bound=tv_lift_upper_bound)

# Plots for sanity check
# ggplot(channel_sessions_long)+
#   geom_line(aes(x=date,y=paid.brand))+
#   geom_line(aes(x=date,y=paid.brand.lift))

########
#implementing tv predicting arimax
#####
channel_sessions_tv_pred=na.omit(channel_sessions_long[,c(3,5,6,7,9,10)])
xreg_matrix<-matrix(c(channel_sessions_tv_pred$direct.net.home,
                      channel_sessions_tv_pred$direct.home,
                      channel_sessions_tv_pred$organic.net.home,
                      channel_sessions_tv_pred$organic.home,
                      channel_sessions_tv_pred$paid.brand,
                      I(channel_sessions_tv_pred$direct.net.home^.5),
                      I(channel_sessions_tv_pred$direct.home^.5),
                      I(channel_sessions_tv_pred$organic.net.home^.5),
                      I(channel_sessions_tv_pred$organic.home^.5),
                      I(channel_sessions_tv_pred$paid.brand^.5)),
                    ncol=10)

tv_predict=auto.arima(channel_sessions_tv_pred$tv.spend,xreg=xreg_matrix,allowdrift=FALSE,allowmean=FALSE,stationary=FALSE,
                      stepwise=FALSE,approx=FALSE)

channel_sessions_tv_pred2=na.omit(channel_sessions_long[,c(3,5,6,7,9,10)])
xreg_matrix_next=xreg_matrix<-matrix(c(channel_sessions_tv_pred2$direct.net.home,
                                       channel_sessions_tv_pred2$direct.home,
                                       channel_sessions_tv_pred2$organic.net.home,
                                       channel_sessions_tv_pred2$organic.home,
                                       channel_sessions_tv_pred2$paid.brand,
                                       I(channel_sessions_tv_pred2$direct.net.home^.5),
                                       I(channel_sessions_tv_pred2$direct.home^.5),
                                       I(channel_sessions_tv_pred2$organic.net.home^.5),
                                       I(channel_sessions_tv_pred2$organic.home^.5),
                                       I(channel_sessions_tv_pred2$paid.brand^.5)),
                                     ncol=10)

fcast2 <- forecast(tv_predict,xreg=xreg_matrix_next)
fcast_pred=fcast2[['mean']]

##
#Adding forecasted to real data
last_non_na_channel=max(which(!is.na(channel_sessions_long$direct.net.home)))
channel_sessions_long$tv_forecasted=rep(NA,nrow(channel_sessions_long))
channel_sessions_long$tv_forecasted[(last_non_na_channel-length(fcast_pred)+1):last_non_na_channel]=fcast_pred
#saving a dummy csv to be uploaded to google sheet  
write.csv(channel_sessions_long,file="temp.csv")
#uploading google sheet to be named with date of run
gs_upload("temp.csv",
          sheet_title =paste("weekly_tv_channel_with_forecasts",as.character(Sys.time()),sep="_"))

   

