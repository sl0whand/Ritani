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

tv_data=tbl_df(read.xlsx("tvanalysis.xlsx",sheet=2,rows=c(1:95),cols=c(1,4:20)))
tv_data$date=as.Date(as.character(tv_data$date),"%m/%d/%Y")+years(2000)
na_inds=which(is.na(tv_data$date))
for (ind in na_inds) {tv_data$date[ind]=tv_data$date[(ind-1)]+weeks(1)}

#Subsetting data
study_vars=c("date","tv.spend","organic","organic.home","direct.home","paid.brand.sessions")
study_cols=which(names(tv_data) %in% study_vars)
sub_study=na.omit(tv_data[,study_cols]  %>% arrange(desc(date)))
sub_study=sub_study[-1,]
sub_study$paid.brand.sessions=as.numeric(as.character(sub_study$paid.brand.sessions))





#Plot relevant data
 
#Correlations
cor(sub_study[,sapply(sub_study,is.numeric)])[1,]
vars<-c("organic","organic.home","direct.home","paid.brand.sessions")

ccf.df=data.frame(Correlation=NULL,Lag=NULL,Source=NULL)
for (var in vars) {
  ind=which(names(sub_study) == var)
  ccf_temp<-ccf(sub_study$tv.spend,sub_study[,ind],plot=FALSE)
  ccf_temp_df=data.frame(Correlation=ccf_temp$acf,Lag=ccf_temp$lag,Source=rep(var,length(ccf_temp$lag)))
  
  ccf.df<-rbind(ccf.df,ccf_temp_df)
}

CCF_plot<-ggplot(ccf.df,aes(x=Lag,y=Correlation,color=Source))+ theme_bw()+
  geom_point()+ggtitle("TV Spending Delayed Effect")+xlab("Lag (Weeks)")+
  scale_color_manual( values=c("red","green","blue","purple"),
                      name="Source",
                      breaks=vars,
                      labels=c("Organic", "Organic Home", "Direct Home","Paid Brand"))
CCF_plot
# ggplotly(CCF_plot)  

## Organic Channel
###2nd order polynomial fit

poly_fit=lm(data=sub_study,organic~tv.spend+I(tv.spend^2))
tmp_df=data.frame(round(coef(poly_fit),3))
tmp_df=rbind(tmp_df,cor(fitted(poly_fit),sub_study$organic)^2)
rownames(tmp_df)[length(rownames(tmp_df))]="R2"
pander(tmp_df)



###Stepwise polynomial fit
big_poly_fit=lm(data=sub_study,organic~tv.spend+I(tv.spend^2)+I(tv.spend^3)+I(tv.spend^4))
stepwise_model=step(big_poly_fit,k=log(nrow(mtcars)))



tmp_df=data.frame(round(coef(stepwise_model),3))
tmp_df=rbind(tmp_df,cor(fitted(stepwise_model),sub_study$organic)^2)
rownames(tmp_df)[length(rownames(tmp_df))]="R2"
pander(tmp_df)


tmp_df=data.frame(tv.spend=sub_study$tv.spend,channel=sub_study$organic,poly=fitted(poly_fit),
                  step=fitted(stepwise_model))

ggplot(tmp_df) + theme_bw()+
  geom_point(aes(x=tv.spend,y=channel,color="Organic"))+ 
  geom_line(aes(x=tv.spend,y=poly,color="2nd Order Plynomial Fit"))+ 
  geom_line(aes(x=tv.spend,y=step,color="Stepwise Polynomial Fit"))+ 
  ggtitle("Model Fits")+xlab("TV Speding (Dollars)")+ylab("Visits")+
  guides(color=guide_legend(title="Models"))

#This plot shows a major time trend component- implying previous models were useless
plot(stepwise_model$residuals)

#These plots shows a major auto-correlative component- we can include that in the model
acf(stepwise_model$residuals,ylab="Correlation",main="Autocorrelation of Residuals")
plot(sub_study$organic)
plot(sub_study$organic,lag(sub_study$organic))

#These plots shows a major difference component- we can include in the model
pacf(stepwise_model$residuals,ylab="Correlation",main="Partial Autocorrelation of Residuals")
plot(diff(stepwise_model$residuals))


plot(sub_study$tv.spend,sub_study$organic)


#
plot(SMA(diff(stepwise_model$residuals),n=2))






stl_obj=stl(ts(sub_study$organic, frequency = 26), s.window="periodic",robust=TRUE)
plot(stl_obj)+title("Organic Seasonal Decomposition")


###Arima model with TV spend

xreg_matrix<-model.matrix(formula(stepwise_model),data=sub_study)
xreg_matrix=xreg_matrix[,-1]

# tv_arima=Arima(sub_study$organic,order=c(1,1,0),xreg=xreg_matrix)

tv_arima=auto.arima(sub_study$organic,xreg=xreg_matrix,max.p=10)
# summary(tv_arima)
tv_arima_fcast=forecast(tv_arima,xreg=xreg_matrix)
plot(tv_arima_fcast)


tv_arima_R2=round(cor(fitted(tv_arima_fcast),sub_study$organic)^2,2)


tmp_df=data.frame(coef(tv_arima))
tmp_df=rbind(tmp_df,tv_arima_R2)
rownames(tmp_df)[length(rownames(tmp_df))]="R2"
pander(tmp_df)


plot(fitted(poly_fit),sub_study$organic)
plot(fitted(stepwise_model),sub_study$organic)
plot(as.vector(fitted(tv_arima_fcast)),sub_study$organic)


