
#need source for DB connecting once input data is uploaded to DB

#Pulls data and munges it for next step
source("tv_channel_models_data_munge.R")
#Pulls rda does automated ARIMAX fit, saves TV spend, observed data, fitted data, forecasted data, and directionally accurate lift estimates
source("TV_channel_Arima_forecast_uploader.R")
