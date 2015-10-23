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



####organic
stl_obj=stl(ts(sub_study$organic, frequency = 20),t.window=15,s.window="periodic",robust=TRUE)
plot(stl_obj)+title("Organic Seasonal Decomposition")

#Arima model with TV spend
tv_arima=auto.arima(sub_study$organic,xreg=c(sub_study$tv.spend))
coef(tv_arima)
tv_arima_fcast=forecast(tv_arima,xreg=c(sub_study$tv.spend))
plot(tv_arima_fcast)
tv_arima_R2=round(cor(fitted(tv_arima_fcast),sub_study$organic)^2,2)
tv_arima_R2

#2nd order polyfit
poly_fit=lm(data=sub_study,organic~tv.spend+I(tv.spend^2))
summary(poly_fit)
plot(poly_fit)


#Stepwise poly fit
big_poly_fit=lm(data=sub_study,organic~tv.spend+I(tv.spend^2)+I(tv.spend^3)+I(tv.spend^4))
summary(big_poly_fit)
stepwise_model=step(big_poly_fit,k=log(nrow(mtcars)))
summary(stepwise_model)

anova(big_poly_fit,stepwise_model, test="Chi")
#high p-value implies the smaller models is best



####organic home
stl_obj=stl(ts(sub_study$organic.home, frequency = 20),t.window=15,s.window="periodic",robust=TRUE)
plot(stl_obj)+title("Organic Home Seasonal Decomposition")

#Arima model with TV spend
tv_arima=auto.arima(sub_study$organic.home,xreg=c(sub_study$tv.spend))
coef(tv_arima)
tv_arima_fcast=forecast(tv_arima,xreg=c(sub_study$tv.spend))
plot(tv_arima_fcast)
tv_arima_R2=round(cor(fitted(tv_arima_fcast),sub_study$organic.home)^2,2)
tv_arima_R2
#2nd order polyfit
poly_fit=lm(data=sub_study,organic.home~tv.spend+I(tv.spend^2))
summary(poly_fit)

#Stepwise poly fit
big_poly_fit=lm(data=sub_study,organic.home~tv.spend+I(tv.spend^2)+I(tv.spend^3)+I(tv.spend^4))
summary(big_poly_fit)
stepwise_model=step(big_poly_fit,k=log(nrow(mtcars)))
summary(stepwise_model)

anova(big_poly_fit,stepwise_model, test="Chi")
#low p-value implies the larger models makes no difference

####direct.home
stl_obj=stl(ts(sub_study$direct.home, frequency = 20),t.window=15,s.window="periodic",robust=TRUE)
plot(stl_obj)+title("Direct Home Seasonal Decomposition")

#Arima model with TV spend
tv_arima=auto.arima(sub_study$direct.home,xreg=c(sub_study$tv.spend))
coef(tv_arima)
tv_arima_fcast=forecast(tv_arima,xreg=c(sub_study$tv.spend))
plot(tv_arima_fcast)
tv_arima_R2=round(cor(fitted(tv_arima_fcast),sub_study$organic.home)^2,2)
tv_arima_R2
#2nd order polyfit
poly_fit=lm(data=sub_study,direct.home~tv.spend+I(tv.spend^2))
summary(poly_fit)
#Stepwise poly fit
big_poly_fit=lm(data=sub_study,direct.home~tv.spend+I(tv.spend^2)+I(tv.spend^3)+I(tv.spend^4))
summary(big_poly_fit)
stepwise_model=step(big_poly_fit,k=log(nrow(mtcars)))
summary(stepwise_model)

anova(big_poly_fit,stepwise_model, test="Chi")
#high p-value implies the smaller models is best

####paid.brand.sessions
stl_obj=stl(ts(sub_study$paid.brand.sessions, frequency = 20),t.window=15,s.window="periodic",robust=TRUE)
plot(stl_obj)+title("Paid Brand Seasonal Decomposition")

#Arima model with TV spend
tv_arima=auto.arima(sub_study$paid.brand.sessions,xreg=c(sub_study$tv.spend))
coef(tv_arima)
tv_arima_fcast=forecast(tv_arima,xreg=c(sub_study$tv.spend))
plot(tv_arima_fcast)
tv_arima_R2=round(cor(fitted(tv_arima_fcast),sub_study$paid.brand.sessions)^2,2)
tv_arima_R2

#2nd order polyfit
poly_fit=lm(data=sub_study,paid.brand.sessions~tv.spend+I(tv.spend^2))
summary(poly_fit)

#Stepwise poly fit
big_poly_fit=lm(data=sub_study,paid.brand.sessions~tv.spend+I(tv.spend^2)+I(tv.spend^3)+I(tv.spend^4))
summary(big_poly_fit)
stepwise_model=step(big_poly_fit,k=log(nrow(mtcars)))
summary(stepwise_model)

anova(big_poly_fit,stepwise_model, test="Chi")
#high p-value implies the smaller models is best

