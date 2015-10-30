library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(forecast)
library(stats)
library(pander)
library(TTR)
library(googlesheets)
library(readxl)



load(file="channel_sessions_long_airings.rda")



#Correlations

vars<-c("organic.net.home","organic.home","direct.net.home","direct.home","paid.brand.sessions")

ccf.df=data.frame(Correlation=NULL,Lag=NULL,Source=NULL)

for (var in vars) {
  print(var)
  ind=which(names(channel_sessions_long) == var)
  study_var=channel_sessions_long[,ind]
  na_inds=which(is.na(study_var))
  Airings=channel_sessions_long$Airings[-na_inds]
  study_var=na.omit(study_var)
  
  
  ccf_temp<-ccf(Airings,study_var,plot=FALSE)
  ccf_temp_df=data.frame(Correlation=ccf_temp$acf,Lag=ccf_temp$lag,Source=rep(var,length(ccf_temp$lag)))
  
  ccf.df<-rbind(ccf.df,ccf_temp_df)
}

CCF_plot<-ggplot(ccf.df %>% filter(Lag>=0),aes(x=Lag,y=Correlation,color=Source))+ theme_bw()+
  geom_point()+ggtitle("TV Airings Delayed Effect")+xlab("Lag (Days)")+
  scale_color_manual( values=rainbow(length(vars)),
                      name="Source",
                      breaks=vars,
                      labels=vars)
CCF_plot
ccf.df %>% filter(Lag==0)
