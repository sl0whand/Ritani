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

#pulling TV spend data
tv_data=tbl_df(read.xlsx("tvanalysis.xlsx",sheet=2,rows=c(1:95),cols=c(1,4:20)))
tv_data$date=as.Date(as.character(tv_data$date),"%m/%d/%Y")+years(2000)
na_inds=which(is.na(tv_data$date))
#not great coding practice here
for (ind in na_inds) {tv_data$date[ind]=tv_data$date[(ind-1)]+weeks(1)}

#Subsetting data
study_vars=c("date","tv.spend","organic","organic.home","direct.home","paid.brand.sessions","direct.all")
study_cols=which(names(tv_data) %in% study_vars)
sub_study=tv_data[,study_cols]  %>% arrange(desc(date))
sub_study$tv.spend[which(is.na(sub_study$tv.spend))]=0

sub_study$tv.spend=as.numeric(as.character(sub_study$tv.spend))
sub_study$organic=as.numeric(as.character(sub_study$organic))
sub_study$organic.home=as.numeric(as.character(sub_study$organic.home))
sub_study$direct.all=as.numeric(as.character(sub_study$direct.all))
sub_study$direct.home=as.numeric(as.character(sub_study$direct.home))
sub_study$paid.brand.sessions=as.numeric(as.character(sub_study$paid.brand.sessions))


#Removing redundant data
sub_study$organic.net.home=sub_study$organic-sub_study$organic.home
sub_study$direct.net.home=sub_study$direct.all-sub_study$direct.home
sub_study$organic=NULL
sub_study$direct.all=NULL

#Correlations

vars<-c("organic.net.home","organic.home","direct.net.home","direct.home","paid.brand.sessions")

ccf.df=data.frame(Correlation=NULL,Lag=NULL,Source=NULL)
for (var in vars) {
  ind=which(names(sub_study) == var)
  study_var=sub_study[,ind]
  na_inds=which(is.na(study_var))
  tv=sub_study$tv.spend[-na_inds]
  study_var=na.omit(study_var)
  
  
  ccf_temp<-ccf(tv,study_var,plot=FALSE)
  ccf_temp_df=data.frame(Correlation=ccf_temp$acf,Lag=ccf_temp$lag,Source=rep(var,length(ccf_temp$lag)))
  
  ccf.df<-rbind(ccf.df,ccf_temp_df)
}

CCF_plot<-ggplot(ccf.df,aes(x=Lag,y=Correlation,color=Source))+ theme_bw()+
  geom_point()+ggtitle("TV Spending Delayed Effect")+xlab("Lag (Weeks)")+
  scale_color_manual( values=rainbow(length(vars)),
                      name="Source",
                      breaks=vars,
                      labels=vars)
CCF_plot
ccf.df[which(ccf.df$Lag==0),c(1,3)]

# ggplotly(CCF_plot)


## Organic Channel

study_var=sub_study$organic.net.home
na_inds=which(is.na(study_var))
tv=sub_study$tv.spend[-na_inds]
study_var=na.omit(study_var)
title_paste="Organic Net of Home"


###2nd order polynomial fit

#frequency shoud be 52 (weeks per year), but we don't have two full periods
ts_var=ts(study_var, frequency = 13)

poly_fit=lm(study_var~tv+I(tv^2))
tmp_df=data.frame(round(coef(poly_fit),3))
tmp_df=rbind(tmp_df,cor(fitted(poly_fit),na.omit(study_var))^2)
rownames(tmp_df)[length(rownames(tmp_df))]="R2"
pander(tmp_df)




###Stepwise polynomial fit

big_poly_fit=lm(study_var~tv+I(tv^2)+I(tv^3)+I(tv^4))
stepwise_model=step(big_poly_fit,k=log(nrow(sub_study)),scope = list(lower = ~tv))

tmp_df=data.frame(round(coef(stepwise_model),3))
tmp_df=rbind(tmp_df,cor(fitted(stepwise_model),na.omit(study_var))^2)
rownames(tmp_df)[length(rownames(tmp_df))]="R2"
pander(tmp_df)

tmp_df=data.frame(tv.spend=tv,channel=study_var,poly=fitted(poly_fit),
                  step=fitted(stepwise_model))
ggplot(tmp_df) + theme_bw()+
  geom_point(aes(x=tv.spend,y=channel,color=title_paste))+ 
  geom_line(aes(x=tv.spend,y=poly,color="2nd Order Plynomial Fit"))+ 
  geom_line(aes(x=tv.spend,y=step,color="Stepwise Polynomial Fit"))+ 
  ggtitle("Model Fits")+xlab("TV Speding (Dollars)")+ylab("Sessions")+
  guides(color=guide_legend(title="Models"))

#This plot shows a major time trend component- implying previous models were useless
qplot(sub_study$date[-na_inds],stepwise_model$residuals)+theme_bw()+
  ggtitle("Polynomial Fit Residuals")+xlab("Date")+ylab("Residuals")

#These plots shows a major auto-correlative component- we can include that in the model
acf(stepwise_model$residuals,ylab="Correlation",main="Autocorrelation of Residuals")
qplot(study_var,lag(study_var,n=1))+theme_bw()+
  ggtitle(paste(title_paste,"Sessions Lag Plot"))+xlab(paste(title_paste,"Sessions"))+
  ylab(paste(title_paste,"Sessions (Single Lag)"))

#These plots shows a difference component- we can include in the model
pacf(stepwise_model$residuals,ylab="Correlation",main="Partial Autocorrelation of Residuals")
qplot(sub_study$date[-na_inds][-1],diff(stepwise_model$residuals))+theme_bw()+
  ggtitle("Polynomial Fit Differenced Residuals")+xlab("Date")+ylab("First Difference Residuals")



###Arima model with TV spend


stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
plot(stl_obj)+title(paste(title_paste,"Seasonal Trend Decomposition"))

#Building exogeneous variables
#  xreg_matrix<-model.matrix(formula(stepwise_model),data=sub_study)
# xreg_matrix=xreg_matrix[,-1]


#Not allowing for seasonal component until we have two years of data
tv_arima=auto.arima(ts_var,xreg=c(tv),max.P=0,max.D = 0,max.Q = 0,allowdrift=FALSE)
 summary(tv_arima)
 tv_arima_fcast=forecast(tv_arima,xreg=c(tv))
 plot(tv_arima_fcast)

##Attempting to build the arim simple enough for excel


# forecasts using predict
# forecasts by hand

d=tv_arima$arma[2]
if (d>0) y_t=diff(ts_var,d) else y_t=ts_var
  

ma1_coef=coef(tv_arima)[grep("ma1",names(coef(tv_arima)))]
tv_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))]








#Building coefficient and R2 table
tv_arima_R2=round(cor(fitted(tv_arima),ts_var)^2,2)
tmp_df=data.frame(coef(tv_arima))
tmp_df=rbind(tmp_df,tv_arima_R2)
rownames(tmp_df)[length(rownames(tmp_df))]="R2"
pander(tmp_df)





qplot(study_var,fitted(poly_fit))+theme_bw()+ggtitle("2nd order Polynomial Fit")+
  xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values")
qplot(study_var,fitted(stepwise_model))+theme_bw()+ggtitle("Stepwise Polynomial Fit")+
  xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values")
qplot(study_var,as.vector(fitted(tv_arima)))+theme_bw()+ggtitle("ARIMA Fit")+
  xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values")



