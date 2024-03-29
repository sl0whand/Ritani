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
################
###Data Munging
################
#pulling TV spend data
tv_data=tbl_df(read.xlsx("tvanalysis.xlsx",sheet=2,rows=c(1:95),cols=c(1,4:20)))
tv_data$date=as.Date(as.character(tv_data$date),"%m/%d/%Y")+years(2000)
na_inds=which(is.na(tv_data$date))
#not great coding practice here
for (ind in na_inds) {tv_data$date[ind]=tv_data$date[(ind-1)]+weeks(1)}

#Subsetting data
study_vars=c("date","tv.spend")
study_cols=which(names(tv_data) %in% study_vars)
sub_study=tv_data[,study_cols]  %>% arrange(desc(date))


##pulling older daily session data
a=gs_title("channeldata with home")
gs_channeldata=a %>% gs_read(ws = "channeldata")
gs_channeldataroot=a %>% gs_read(ws = "channeldataroot")
channel_home_levels=levels(as.factor(gs_channeldataroot$ga.channelGrouping))
# replacing channel names
for (lev in channel_home_levels) {
  swap_inds=which(gs_channeldataroot$ga.channelGrouping==lev)
  gs_channeldataroot$ga.channelGrouping[swap_inds]=paste(gs_channeldataroot$ga.channelGrouping[swap_inds],"home",sep=".")
  
}
gs_channeldataroot[,3]=NULL
channel_sessions_long=rbind(gs_channeldata,gs_channeldataroot)
channel_sessions_long[,4]=NULL
channel_sessions_long=channel_sessions_long %>% rename(date=ga.date)
channel_sessions_long=channel_sessions_long %>% rename(channel=ga.channelGrouping)
channel_sessions_long=channel_sessions_long %>% rename(sessions=ga.sessions)
# channel_sessions_long=tbl_df(read.xlsx("channeldata_with_home.xlsx",sheet=1))

#converting date
channel_sessions_long$date=as.Date(paste(substr(as.character(channel_sessions_long$date),5,6),
                                         substr(as.character(channel_sessions_long$date),7,8),
                                         substr(as.character(channel_sessions_long$date),1,4),
                                         sep="-"), "%m-%d-%Y")

#appending tv spend
channel_sessions_long=rbind(channel_sessions_long,
                            sub_study %>% select(tv.spend,date) %>% 
                              rename(sessions=tv.spend) %>% 
                              mutate(channel="tv.spend"))


channel_sessions_long$year=strftime(channel_sessions_long$date,format="%y") 
channel_sessions_long$week=strftime(channel_sessions_long$date,format="%W") 
channel_sessions_long$date=as.Date(strptime(paste(channel_sessions_long$year,
                                                  (as.numeric(channel_sessions_long$week)*7),
                                                  sep=" "),format="%Y %j") +years(2000))



channel_sessions_long=channel_sessions_long %>% group_by(channel,year,week) %>% 
  summarise(sessions=sum(sessions),date=first(date))
# swapping in mondays
for (i in which(is.na(channel_sessions_long$date))) {
  channel_sessions_long$date[i]=channel_sessions_long$date[i+1]-weeks(1)
}
channel_sessions_long=channel_sessions_long %>% group_by(channel,year,week) %>% 
  summarise(sessions=sum(sessions),date=first(date))


rm_vars=c("year","week")
rm_cols=which(names(channel_sessions_long) %in% rm_vars)
channel_sessions_long=channel_sessions_long[,-rm_cols]


#casting
channel_sessions_long=channel_sessions_long %>% spread(key=channel,value=sessions)
names(channel_sessions_long) <- sub(" ", ".", names(channel_sessions_long))

#Replacing missing tv spend with 0
channel_sessions_long$tv.spend[which(is.na(channel_sessions_long$tv.spend))]=0

#fabricating variables
channel_sessions_long$direct.net.home=channel_sessions_long$Direct-channel_sessions_long$Direct.home
channel_sessions_long$direct.home=channel_sessions_long$Direct.home
channel_sessions_long$organic.net.home=channel_sessions_long$Organic.Search-channel_sessions_long$Organic.Search.home
channel_sessions_long$organic.home=channel_sessions_long$Organic.Search.home
channel_sessions_long$paid.brand.sessions=channel_sessions_long$`Branded.Paid Search`
  
  

study_vars=c("date","tv.spend","direct.net.home","direct.home",
             "organic.net.home","organic.home",
             "paid.brand.sessions")
study_cols=which(names(channel_sessions_long) %in% study_vars)
channel_sessions_long=channel_sessions_long[,study_cols]  %>% arrange(desc(date))

write.xlsx(channel_sessions_long, "tv_channel_analysis.xlsx")
 gs_upload("tv_channel_analysis.xlsx",sheet_title="upload_dont_edit")


## Organic Channel
i=5
study_var=channel_sessions_long[,grep(study_vars[i],names(channel_sessions_long))][[1]]
na_inds=which(is.na(study_var))
tv=channel_sessions_long$tv.spend[-na_inds]
study_var=study_var[-na_inds]

title_paste=names(channel_sessions_long)[grep(study_vars[i],names(channel_sessions_long))]


###2nd order polynomial fit

#frequency shoud be 52 (weeks per year), but we don't have two full periods
ts_var=ts(study_var, frequency = 52)
stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
if (class(stl_obj)=="try-error") {
  ts_var=ts(study_var, frequency = 26)
  stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
}
plot(stl_obj)+title(paste(title_paste,"Seasonal Trend Decomposition"))


poly_fit=lm(study_var~tv+I(tv^2))
tmp_df=data.frame(round(coef(poly_fit),3))
tmp_df=rbind(tmp_df,cor(fitted(poly_fit),na.omit(study_var))^2)
rownames(tmp_df)[length(rownames(tmp_df))]="R2"
pander(tmp_df)




###Stepwise polynomial fit

big_poly_fit=lm(study_var~tv+I(tv^2)+I(tv^3)+I(tv^4)+I(tv))
stepwise_model=step(big_poly_fit,k=log(nrow(channel_sessions_long)),scope = list(lower = ~tv))

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
qplot(channel_sessions_long$date[-na_inds],stepwise_model$residuals)+theme_bw()+
  ggtitle("Polynomial Fit Residuals")+xlab("Date")+ylab("Residuals")

###Arima model with TV spend

#Building exogeneous variables
# xreg_matrix<-model.matrix(formula(stepwise_model),data=sub_study)
# xreg_matrix=xreg_matrix[,-1]


#Not allowing for seasonal component until we have two years of data
tv_arima=auto.arima(ts_var,xreg=c(tv),max.q=0,max.P=0,max.D = 0,max.Q = 0,allowdrift=FALSE)
summary(tv_arima)
tv_arima_fcast=forecast(tv_arima,xreg=c(tv))


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
qplot(study_var,as.vector(fitted(tv_arima)))+theme_bw()+ggtitle("ARIMAX Fit")+
  xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values")





