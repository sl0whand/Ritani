
report_poly_fit_2=function(study_var,tv){
  poly_fit=lm(study_var~tv+I(tv^2))
  return(poly_fit)
} #end of report_poly_fit_2

report_poly_fit_2_sub=function(poly_fit,study_var){
  tmp_df=data.frame(round(coef(poly_fit),3))
  tmp_df=rbind(tmp_df,cor(fitted(poly_fit),na.omit(study_var))^2)
  rownames(tmp_df)[length(rownames(tmp_df))]="R2"
  pander(tmp_df)
} #end of report_poly_fit_2_sub


step_model=function(study_var,tv){
  big_poly_fit=lm(study_var~tv+I(tv^2)+I(tv^3)+I(tv^(1/2)))
  stepwise_model=step(big_poly_fit,k=log(nrow(channel_sessions_long)))
  return(stepwise_model)
} # end of step_model


step_pander=function(stepwise_model){
  
  tmp_df=data.frame(round(coef(stepwise_model),3))
  tmp_df=rbind(tmp_df,cor(fitted(stepwise_model),na.omit(study_var))^2)
  rownames(tmp_df)[length(rownames(tmp_df))]="R2"
  pander(tmp_df)
} # step_pander


poly_plots=function(tv,study_var,poly_fit,stepwise_model){
  tmp_df=data.frame(tv.spend=tv,channel=study_var,poly=fitted(poly_fit),
                    step=fitted(stepwise_model))
  print(ggplot(tmp_df) + theme_bw()+
          # geom_smooth(aes(x=tv.spend,y=channel),method="smooth")+
          geom_point(aes(x=tv.spend,y=channel,color=title_paste))+ 
          geom_line(aes(x=tv.spend,y=poly,color="2nd Order Plynomial Fit"))+ 
          geom_line(aes(x=tv.spend,y=step,color="Stepwise Polynomial Fit"))+ 
          ggtitle("Model Fits")+xlab("TV Spending (Dollars)")+ylab("Sessions")+
          guides(color=guide_legend(title="Models")))
  
  #This plot shows a major time trend component- implying previous models were useless
  t=Box.test(stepwise_model$residuals)
  box_p=round(t$p.value,4)
  print(qplot(channel_sessions_long$date[-na_inds],stepwise_model$residuals)+theme_bw()+
          ggtitle(paste("Polynomial Fit Residuals, Box Test p-value",box_p) )+xlab("Date")+ylab("Residuals"))
} # poly_plots

stl_func=function(study_var,freq){
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=ts(study_var, frequency = round(freq/2))
    stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
  }
  plot(stl_obj)+title(paste(title_paste,"Seasonal Trend Decomposition"))
} # stl_func




arima_func=function(stepwise_model,study_var,tv,freq){
  # xreg_matrix<-model.matrix(formula(stepwise_model))
  xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
  
  xreg_matrix=xreg_matrix[,-1]
  
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=study_var
    
  } 
  
  #Not allowing for seasonal component until we have two years of data
  tv_arima=try(auto.arima(ts_var,xreg=xreg_matrix,allowdrift=FALSE,allowmean=FALSE))
   # allowmean = FALSE
  # print(tv_arima)
  # print(arimaorder(tv_arima))
  # standard_resids=(tv_arima$residuals-mean(tv_arima$residuals))/sd(tv_arima$residuals)
  # hist(standard_resids)
  
  
  return(tv_arima)
} # end of arima_func

arima_func_sub=function(tv_arima,stepwise_model,study_var,tv,freq){
  
  print(tv_arima)
  # xreg_matrix<-model.matrix(formula(stepwise_model))
  xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
  xreg_matrix=xreg_matrix[,-1]
  
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=study_var
    
  }
  
  #gathering degrees of freedom and max lag
  arima.order=as.character(arimaorder(tv_arima))
  if (length(arima.order)>3){
    p_s=as.numeric(arima.order[5])
    d_s=as.numeric(arima.order[6])
  } else {
    p_s=0
    d_s=0
  }
  p=as.numeric(arima.order[1])
  d=as.numeric(arima.order[2])
  
  
  combined_obs=p+d+p_s+d_s+1
  d_freedom=nrow(xreg_matrix)-combined_obs-ncol(xreg_matrix)+1
  
  #performing t-tests on TV coefficients
  # tv_arima$coef
  # tv_arima$var.coef
  
  
  t=Box.test(tv_arima$residuals)
  box_p=round(t$p.value,4)
  
  #Building coefficient and R2 table
  tv_arima_R2=round(cor(fitted(tv_arima),ts_var)^2,2)
  # tmp_df=data.frame(coef(tv_arima))
#   tmp_df=rbind(tmp_df,p)
#   rownames(tmp_df)[length(rownames(tmp_df))]="p"
#   tmp_df=rbind(tmp_df,d)
#   rownames(tmp_df)[length(rownames(tmp_df))]="d"
#   tmp_df=rbind(tmp_df,q)
#   rownames(tmp_df)[length(rownames(tmp_df))]="q"
  
  
  tmp_df=data.frame(box_p)
  rownames(tmp_df)[length(rownames(tmp_df))]="Box Test p-value"
  
  tmp_df=rbind(tmp_df,d_freedom)
  rownames(tmp_df)[length(rownames(tmp_df))]="Degrees of Freedom"
  
  tmp_df=rbind(tmp_df,combined_obs)
  rownames(tmp_df)[length(rownames(tmp_df))]="Combined Observations"
  
  tmp_df=rbind(tmp_df,tv_arima_R2)
  rownames(tmp_df)[length(rownames(tmp_df))]="R2"
  names(tmp_df)="Values"
  pander(tmp_df,style = "grid",caption ="ARIMX Model Performance Measures")
  
  
} # arima_func_sub

model_comp_plots=function(stepwise_model,tv_arima){
  print(qplot(study_var,fitted(stepwise_model))+theme_bw()+ggtitle("Stepwise Polynomial Fit")+
          xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values"))
  print(qplot(study_var,as.vector(fitted(tv_arima)))+theme_bw()+ggtitle("ARIMAX Fit")+
          xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values"))
} # model_comp_plots



transformed_plot=function(tv_arima,tv,title_paste){
  #scratch code for finding point of diminishing returns
  lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
  sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]
  # sqrt_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][3]
  # tv_lim=max(tv)
  # tv_dummy=seq(0,tv_lim,by=round(tv_lim/200))
  session_dummy=lin_coef*tv+sqr_coef*tv^2
  # +sqrt_coef*sqrt(tv_dummy)
  
  plot(qplot(tv,session_dummy)+theme_bw()+
    ggtitle("Transformed Effect on Visits")+ylab("Sessions")+xlab(title_paste))
} # transformed_plot


transformed_plot_by_date=function(channel_sessions_long,tv_arima,var_name){
  
  var_ind=which(names(channel_sessions_long)==var_name)
  
  lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
  sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]
  
  tv_lift=lin_coef*channel_sessions_long$tv.spend+sqr_coef*channel_sessions_long$tv.spend^2
  
  
  
  ggplot(channel_sessions_long)+
    geom_line(aes(x=date,y=organic.net.home))+
    geom_line(aes(x=date,y=organic.net.home.lift))
  
} # transformed_plot


# tv_arima=Airings_arima
# tv=Airings
# freq=365
model_validation=function(tv_arima,study_var,tv,freq,date){
  
  #forcing ts form
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=study_var
    
  }
  
  #building exogeneous regressors
  xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
  xreg_matrix=xreg_matrix[,-1]
  
  pred_hor=5
  n <- length(ts_var)
  k <- round(n*.5); if (k<60) k=60 # minimum data length for fitting a model
  
  eff=n-k-pred_hor
  if (eff<10) {
    print("There are not enough data to validate this model. You must have at least 70 observations")
    return(NULL)
  }  
    
  MAPE<- matrix(NA,eff)
  order=arimaorder(tv_arima)
  
  
  for(i in 1:eff){
    # print(i)
    xshort <-ts_var[1:(k+(i-1))]
    xnext <- ts_var[(k+i):(k+(i-1)+pred_hor)]
    
    xreg_matrix_short=xreg_matrix[1:(k+(i-1)),]
    xreg_matrix_next=xreg_matrix[(k+i):(k+(i-1)+pred_hor),]
    
    
    fit2 <- Arima(xshort, model=tv_arima,xreg=xreg_matrix_short,method="ML")
    # fit2 <- Arima(xshort, order=order[1:3], seasonal=order[4:6],xreg=xreg_matrix_short)
  
    fcast2 <- forecast(fit2,xreg=xreg_matrix_next)
    fcast_pred=fcast2[['mean']]
    # plot(fcast_pred)
    # plot(c(xshort,xnext))
    
    # c(sub_study$tv.spend[1:(k+(i-1))])
    MAPE[i] <- mean(abs(fcast_pred-xnext)/xnext)*100
  }
  
  qplot(date[(k+pred_hor+1):n],MAPE)+theme_bw()+geom_smooth(method="lm")+
    ggtitle(paste("ARIMA Model Validation, Forecast Window of",pred_hor))+ylab("Mean Absolute Percent Error")+xlab("Additional Training Points")
  
} # end of model_validation


add_forecast_to_df=function(channel_sessions_long,tv_arima,var_name,tv){
  rows=nrow(channel_sessions_long)
  var_ind=which(names(channel_sessions_long)==var_name)
  study_var=channel_sessions_long[,var_ind][[1]]
  na_inds=which(is.na(study_var))
  tv=channel_sessions_long$tv.spend[-na_inds]
  study_var=na.omit(study_var)
  
  
  #Fitting old values
  xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
  xreg_matrix=xreg_matrix[,-1]
  
  fcast2 <- forecast(tv_arima,xreg=xreg_matrix)
  fcast_pred=fcast2[['mean']]
  
  #Forecasting new values
  if (sum(which(diff(na_inds)>1))>0) {
    new_tv_start_ind=na_inds[which(diff(na_inds)>1)[length(which(diff(na_inds)>1))]+1]
  } else {
    new_tv_start_ind=na_inds[1]
  }
  
  
  tv_next=channel_sessions_long$tv.spend[new_tv_start_ind:rows]
  xreg_matrix_next=matrix(c(tv_next,tv_next^2),ncol=2)
  
  fcast3 <- forecast(tv_arima,xreg=xreg_matrix_next,level=80)
  fcast_pred_next=fcast3[['mean']]
  
  full_forecast_fit=as.vector(c(as.vector(fitted(tv_arima)),fcast_pred_next))
  channel_sessions_long$forecast=NA
  channel_sessions_long$forecast[(rows-length(full_forecast_fit)+1):rows]=full_forecast_fit
    
  
  #check package forecasts against
#  plot(fcast2)
  # plot(fcast3)
   # plot(channel_sessions_long$forecast)
   # lines(study_var)
  #Make a good ggplot!
  
   plot_date=channel_sessions_long$date[(rows-length(full_forecast_fit)+1):rows]
   plot_var=channel_sessions_long[((rows-length(full_forecast_fit)+1):rows),var_ind][[1]]
   low=fcast3$lower[1:length(fcast_pred_next)]
   plot_lower=c(rep(NA,(length(full_forecast_fit))-length(low)),low)
   up=fcast3$upper[1:length(fcast_pred_next)]
   plot_upper=c(rep(NA,(length(full_forecast_fit))-length(up)),up)
   temp_df=data.frame(plot_date,full_forecast_fit,plot_var,plot_lower,plot_upper)
   
   #This should not be hard coded
    # plot_start="2015-07-01"
    # plot_length=length(which(channel_sessions_long$date>plot_start))
   plot_length=24
   
   plot(ggplot(temp_df[(nrow(temp_df)-plot_length):nrow(temp_df),])+theme_bw()+
     geom_point(aes(x=plot_date,y=plot_var,color="Observed"))+
     geom_line(aes(x=plot_date,y=full_forecast_fit,color="Modeled"))+
     geom_line(aes(x=plot_date,y=plot_lower,color="Lower Bound, \n %80 Confidence"))+
     geom_line(aes(x=plot_date,y=plot_upper,color="Upper Bound, \n %80 Confidence"))+
     ggtitle("Model Fit and Forecast")+xlab("Date")+ylab("Sessions")+
     theme(legend.title=element_blank()))
     
   
  
  return(channel_sessions_long)
} # end of add_forecast_to_df
  





