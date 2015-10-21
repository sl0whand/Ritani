library(dplyr)
library(tidyr)
library(xlsx)
library(lubridate)
library(ggplot2)
library(gridExtra)


tv_data=tbl_df(read.xlsx("tvanalysis.xlsx",2))
tv_data$date=as.Date(as.character(tv_data$date),"%m/%d/%Y")+years(2000)



#Subsetting data
sub_study=na.omit(tv_data %>% 
                    select(date,tv.spend,organic,organic.home,direct.home,paid.brand.sessions) %>%
                    arrange(desc(date)))
sub_study$paid.brand.sessions=as.numeric(as.character(sub_study$paid.brand.sessions))




#Plot relevant data
tv_spend_plot=ggplot(sub_study) + theme_minimal()+ 
              geom_line(aes(x=date,y=tv.spend,color="TV Spending")) +
              xlab("Date")+ylab("Dollars") +
              guides(color=guide_legend(title=NULL))
traffic_plot=ggplot(sub_study) + theme_minimal()+
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
ggplotly(CCF_plot)  

#organic
lm_fit_organic=lm(organic~date,data=sub_study)
acf(sub_study$organic)
acf(lm_fit_organic$residuals)
acf(diff(sub_study$organic))

#organic home
acf(sub_study$organic.home)
pacf(sub_study$organic.home)

#direct home
acf(sub_study$direct.home)
pacf(sub_study$direct.home)

# paid brand session
acf(sub_study$paid.brand.sessions)
pacf(sub_study$paid.brand.sessions)







