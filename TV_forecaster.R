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




load(file="channel_sessions_long.rda")
na_date=which(is.na(channel_sessions_long$date))
if (length(na_date)>0) channel_sessions_long=channel_sessions_long[-na_date,]
#Starting when TV turns on
 train_start_date="2014-04-28"
 train_start_ind=which(channel_sessions_long$date<=train_start_date)
 channel_sessions_tv_pred=channel_sessions_long[-train_start_ind,]
 
 train_end_date="2015-11-02"
 train_end_ind=which(channel_sessions_tv_pred$date>=train_end_date)
 channel_sessions_tv_pred=channel_sessions_tv_pred[-train_end_ind,]
 
 
 # channel_sessions_tv_pred=na.omit(channel_sessions_tv_pred)

#simple lm fit
lm_fit=lm(channel_sessions_tv_pred$tv.spend~0+
            channel_sessions_tv_pred$direct.net.home+
            channel_sessions_tv_pred$direct.home+
            channel_sessions_tv_pred$organic.net.home+
            channel_sessions_tv_pred$organic.home+
            # channel_sessions_tv_pred$paid.brand+
            I(channel_sessions_tv_pred$direct.net.home^.5)+
            I(channel_sessions_tv_pred$direct.home^.5)+
            I(channel_sessions_tv_pred$organic.net.home^.5)+
            I(channel_sessions_tv_pred$organic.home^.5))
            # I(channel_sessions_tv_pred$paid.brand^.5)
          
summary(lm_fit) 

qplot(channel_sessions_tv_pred$date,lm_fit$residuals)
qplot(channel_sessions_tv_pred$tv.spend,fitted(lm_fit))
t=Box.test(lm_fit$residuals)
round(t$p.value,4)

##actual model fit
xreg_matrix<-model.matrix(channel_sessions_tv_pred$tv.spend~
                            channel_sessions_tv_pred$direct.net.home+
                            channel_sessions_tv_pred$direct.home+
                            channel_sessions_tv_pred$organic.net.home+
                            channel_sessions_tv_pred$organic.home+
                            # channel_sessions_tv_pred$paid.brand+
                            I(channel_sessions_tv_pred$direct.net.home^.5)+
                            I(channel_sessions_tv_pred$direct.home^.5)+
                            I(channel_sessions_tv_pred$organic.net.home^.5)+
                            I(channel_sessions_tv_pred$organic.home^.5))
                            # I(channel_sessions_tv_pred$paid.brand^.5)
xreg_matrix=xreg_matrix[,-1]



tv_arima=auto.arima(channel_sessions_tv_pred$tv.spend,xreg=xreg_matrix,
                    allowdrift=FALSE,allowmean=FALSE,stepwise=FALSE,approx=FALSE)
arimaorder(tv_arima)
coef(tv_arima)

round(cor(fitted(tv_arima),channel_sessions_tv_pred$tv.spend)^2,2)
t=Box.test(tv_arima$residuals)
round(t$p.value,4)

qplot(channel_sessions_tv_pred$tv.spend,fitted(tv_arima))

#Validation

pred_hor=3
n <- length(channel_sessions_tv_pred$tv.spend)
k <- round(n*.7); if (k<60) k=60 # minimum data length for fitting a model

eff=n-k-pred_hor
# if (eff<10) {
#   print("There are not enough data to validate this model. You must have at least 70 observations")
#   return(NULL)
# }  

MAE<- matrix(NA,eff)
order=arimaorder(tv_arima)


for(i in 1:eff){
  # print(i)
  xshort <-channel_sessions_tv_pred$tv.spend[1:(k+(i-1))]
  xnext <- channel_sessions_tv_pred$tv.spend[(k+i):(k+(i-1)+pred_hor)]
  
  xreg_matrix_short=xreg_matrix[1:(k+(i-1)),]
  xreg_matrix_next=xreg_matrix[(k+i):(k+(i-1)+pred_hor),]
  
  
  # fit2 <- Arima(xshort, model=tv_arima,xreg=xreg_matrix_short,method="ML")
  fit2 <- Arima(xshort, order=order[1:3], seasonal=order[4:6],xreg=xreg_matrix_short)
  
  fcast2 <- forecast(fit2,xreg=xreg_matrix_next)
  fcast_pred=fcast2[['mean']]
  # plot(fcast_pred)
  # plot(c(xshort,xnext))
  
  # c(sub_study$tv.spend[1:(k+(i-1))])
  MAE[i] <- mean(abs(fcast_pred-xnext))
}

qplot(seq(1:length(MAE)),MAE)+theme_bw()+geom_smooth(method="lm")



