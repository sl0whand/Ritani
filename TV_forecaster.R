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
library(glmnet)
library(caret)



load(file="channel_sessions_long.rda")
na_date=which(is.na(channel_sessions_long$date))
if (length(na_date)>0) channel_sessions_long=channel_sessions_long[-na_date,]
channel_sessions_long$GMV=NULL
#Starting when TV turns on
#  train_start_date="2014-04-28"
#  train_start_ind=which(channel_sessions_long$date<=train_start_date)
#  channel_sessions_tv_pred=channel_sessions_long[-train_start_ind,]
#  
 train_end_date="2015-11-01"
 train_end_ind=which(channel_sessions_long$date>=train_end_date)
 channel_sessions_tv_pred=channel_sessions_long[-train_end_ind,]
 
 
 channel_sessions_tv_pred=na.omit(channel_sessions_tv_pred)
 
 inTrain = createDataPartition(y=channel_sessions_tv_pred$tv.spend, p = .6)[[1]]
 training = channel_sessions_tv_pred[ inTrain,]
 testing = channel_sessions_tv_pred[-inTrain,]

#simple lm fit
lm_fit=lm(data=training,tv.spend~direct.net.home+direct.home+organic.net.home+organic.home+paid.brand+
            I(direct.net.home^.5)+I(direct.home^.5)+I(organic.net.home^.5)+I(organic.home^.5)+I(paid.brand^.5))
summary(lm_fit) 


test_pred=predict(lm_fit,newdata=testing)
cor(test_pred,testing$tv.spend)^2



 x <- model.matrix(formula(lm_fit),data=training)
 x=x[,-1]
 y<- as.matrix(training$tv.spend)
 fit<-glmnet(x,y, family="gaussian", alpha=0.9, lambda=0.001,intercept=FALSE)
# summarize the fit
 coefficients(fit)
 # make predictions
 predictions <- predict(fit, x)
 # summarize accuracy
 cor(y,predictions)^2
 
 test_x=model.matrix(formula(lm_fit),data=testing)
 test_x=test_x[,-1]
 test_predictions <- predict(fit, test_x)
 test_y=as.matrix(testing$tv.spend)
 cor(test_y,test_predictions)^2
 
 str(formula(lm_fit))
 lhs=gsub("I\\(","", formula(lm_fit)[2])
 prop=gsub("I\\(","", formula(lm_fit)[1])
 rhs=gsub("I\\(","", formula(lm_fit)[3])
 rhs=gsub("\\(","", rhs)
 rhs=gsub("\\)","", rhs)
 
 temp_string=paste(lhs,prop,rhs)
 
 temp=coef(fit)
 rownames(temp) <- gsub("I\\(","", rownames(temp))
 rownames(temp) <- gsub("\\(","", rownames(temp))
 rownames(temp) <- gsub(")","", rownames(temp))
 temp=data.frame(Variables=rownames(temp)[-1],Coefficients=as.numeric(temp[,1])[-1])
 
 
 print(temp_string)
 pander(temp,digits=10)
 
#final fit of model applied to training and test set
 
 all_x=model.matrix(formula(lm_fit),data=channel_sessions_tv_pred)
 all_x=all_x[,-1]
 all_predictions <- predict(fit, all_x)
 channel_sessions_tv_pred$tv.spend.fit=all_predictions
 cor(channel_sessions_tv_pred$tv.spend,all_predictions)^2
 
 plot(all_predictions,channel_sessions_tv_pred$tv.spend)
 
 
 
 
 
 
 
######################################################
# Validation is succesful no need to look further
######################################################

          

resids=predictions-channel_sessions_tv_pred$tv.spend
plot(x=channel_sessions_tv_pred$date,y=resids)
plot(channel_sessions_tv_pred$tv.spend,predictions)
t=Box.test(resids)
round(t$p.value,4)

##actual model fit

# lm_fit=lm(data=channel_sessions_tv_pred,tv.spend~direct.net.home+direct.home+organic.net.home+organic.home+paid.brand+
#             I(direct.net.home^.5)+I(direct.home^.5)+I(organic.net.home^.5)+I(organic.home^.5)+I(paid.brand^.5))
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

tv_arima=auto.arima(channel_sessions_tv_pred$tv.spend,xreg=xreg_matrix,allowdrift=FALSE,allowmean=FALSE,stationary=FALSE,
                    stepwise=FALSE,approx=FALSE)
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




