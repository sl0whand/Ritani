library(tidyr)
library(dplyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(forecast)
library(stats)

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
tv_spend_plot=ggplot(sub_study) + theme_bw()+ 
              geom_line(aes(x=date,y=tv.spend,color="TV Spending")) +
              xlab("Date")+ylab("Dollars") +
              guides(color=guide_legend(title=NULL))
traffic_plot=ggplot(sub_study) + theme_bw()+
              geom_line(aes(x=date,y=organic,color="Organic"))+ 
              geom_line(aes(x=date,y=organic.home,color="Organic Home"))+ 
              geom_line(aes(x=date,y=direct.home,color="Direct Home"))+ 
              geom_line(aes(x=date,y=paid.brand.sessions,color="Paid Brand"))+
              ggtitle("Traffic")+xlab("Date")+ylab("Visits")+
              guides(color=guide_legend(title="Source"))

grid.arrange(traffic_plot, tv_spend_plot, ncol = 1)

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

#Add a lagged TV element at 7 weeks based on findings in CCF
sub_study$tv_lag_7=c(rep(NA,7),sub_study$tv.spend[1:(length(sub_study$tv.spend)-7)])
# organic+organic.home+direct.home+paid.brand.sessions

##ACF explorations
#organic
stl_obj=stl(ts(sub_study$organic, frequency = 2),t.window=15,s.window="periodic",robust=TRUE)
plot(stl_obj)+title("Organic Seasonal Decomposition")

lm_fit_organic=lm(organic~date,data=sub_study)
acf(sub_study$organic)
acf(lm_fit_organic$residuals)
acf(diff(sub_study$organic))


organic_arima=auto.arima(sub_study$organic)
summary(organic_arima)
organic_arima_fcast=forecast(organic_arima)
plot(organic_arima_fcast)
organic_arima_R2=round(cor(fitted(organic_arima_fcast),sub_study$organic)^2,2)
organic_arima_R2

organic_tv_arima=auto.arima(sub_study$organic,xreg=c(sub_study$tv.spend))
summary(organic_tv_arima)
organic_tv_arima_fcast=forecast(organic_tv_arima,xreg=c(sub_study$tv.spend))
plot(organic_tv_arima_fcast)
organic_tv_arima_R2=round(cor(fitted(organic_tv_arima_fcast),sub_study$organic)^2,2)
organic_tv_arima_R2




#organic home


lm_fit_organic.home=lm(organic.home~date,data=sub_study)
acf(sub_study$organic.home)
acf(lm_fit_organic.home$residuals)
acf(diff(sub_study$organic.home))





#direct home
lm_fit_direct.home=lm(direct.home~date,data=sub_study)
acf(sub_study$direct.home)
acf(lm_fit_direct.home$residuals)
acf(diff(sub_study$direct.home))


# paid brand session
lm_fit_paid.brand.sessions=lm(paid.brand.sessions~date,data=sub_study)
acf(sub_study$paid.brand.sessions)
acf(lm_fit_paid.brand.sessions$residuals)
acf(diff(sub_study$paid.brand.sessions))


##Simple Regressions

study_vars=c("date","tv.spend","organic","organic.home","direct.home","paid.brand.sessions")
all_fit=lm(data=sub_study,tv.spend~0+organic+organic.home+direct.home+paid.brand.sessions)
coef(all_fit)
cor(sub_study[,sapply(sub_study,is.numeric)])[5,]















