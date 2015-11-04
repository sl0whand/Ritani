

arima_formula=function(arima_object,xreg1){
  
  #Isolating AR parts
  ar_inds=grep("ar",names(coef(arima_object)))
  if (sum(ar_inds)>0){
    count=0
    ar_formula="(1"
    for (ar_ind in ar_inds) {
      count=count+1
      this_coef=round(coef(arima_object)[ar_ind],4)
      ar_formula=paste0(ar_formula,"-(",this_coef,")*B^(",count,")")
    }
    ar_formula=paste0(ar_formula,")")
  } else {ar_formula=""}
  
  #Isolating MA parts
  ma_inds=grep("ma",names(coef(arima_object)))
  if (sum(ma_inds)>0){
    count=0
    ma_formula="(1"
    for (ma_ind in ma_inds) {
      count=count+1
      this_coef=round(coef(arima_object)[ma_ind],4)
      ma_formula=paste0(ma_formula,"-(",this_coef,")*B^(",count,")")
    }
    ma_formula=paste0(ma_formula,")*")
  } else {ma_formula=""}
  
  
  #Isolating differencing
  I_inds=as.numeric(substr(forecast(arima_object,xreg=xreg1)$method,9,9))
  
  if (sum(I_inds)>0){
    count=0
    I_formula="(1"
    for (ma_ind in I_inds) {
      count=count+1
      I_formula=paste0(I_formula,"-B^(",count,")")
    }
    I_formula=paste0(I_formula,")")
  } else {I_formula=""}
  
  tv_coef=round(coef(arima_object)[grep("tv",names(coef(arima_object)))],4)
  
  final_formula=paste0(ar_formula,I_formula,"*y_t=",
                       tv_coef,"*",ar_formula,I_formula,"*x_t+",ma_formula,"e_t")
  
  print(final_formula)
  
}


report_poly_fit_2=function(study_var,tv){
  poly_fit=lm(study_var~tv+I(tv^2))
  return(poly_fit)
}

report_poly_fit_2_sub=function(poly_fit,study_var){
  tmp_df=data.frame(round(coef(poly_fit),3))
  tmp_df=rbind(tmp_df,cor(fitted(poly_fit),na.omit(study_var))^2)
  rownames(tmp_df)[length(rownames(tmp_df))]="R2"
  pander(tmp_df)
}


step_model=function(study_var,tv){
  big_poly_fit=lm(study_var~tv+I(tv^2)+I(tv^3)+I(tv^(1/2)))
  stepwise_model=step(big_poly_fit,k=log(nrow(channel_sessions_long)))
  return(stepwise_model)
}


step_pander=function(stepwise_model){
  
  tmp_df=data.frame(round(coef(stepwise_model),3))
  tmp_df=rbind(tmp_df,cor(fitted(stepwise_model),na.omit(study_var))^2)
  rownames(tmp_df)[length(rownames(tmp_df))]="R2"
  pander(tmp_df)
}


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
}

stl_func=function(study_var,freq){
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=ts(study_var, frequency = round(freq/2))
    stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
  }
  plot(stl_obj)+title(paste(title_paste,"Seasonal Trend Decomposition"))
}




arima_func=function(stepwise_model,study_var,tv,freq){
  # xreg_matrix<-model.matrix(formula(stepwise_model))
  xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
  
  xreg_matrix=xreg_matrix[,-1]
  
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=ts(study_var, frequency = round(freq/2))
    stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
  }
  
  #Not allowing for seasonal component until we have two years of data
  tv_arima=try(auto.arima(ts_var,xreg=xreg_matrix,allowdrift=FALSE,allowmean = FALSE))
  print(arimaorder(tv_arima))
  
  
  
  return(tv_arima)
}

arima_func_sub=function(tv_arima,stepwise_model,study_var,tv,freq){
  # xreg_matrix<-model.matrix(formula(stepwise_model))
  xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
  xreg_matrix=xreg_matrix[,-1]
  
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=ts(study_var, frequency = round(freq/2))
    stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
  }
  
  arima_pdq=try(forecast(tv_arima,xreg=xreg_matrix)$method)
  p=substr(arima_pdq,7,7)
  d=substr(arima_pdq,9,9)
  q=substr(arima_pdq,11,11)
  
  combined_obs=as.numeric(p)+as.numeric(d)+1
  
  
  t=Box.test(tv_arima$residuals)
  box_p=round(t$p.value,4)
  
  
  
  #Building coefficient and R2 table
  tv_arima_R2=round(cor(fitted(tv_arima),ts_var)^2,2)
  tmp_df=data.frame(coef(tv_arima))
  tmp_df=rbind(tmp_df,p)
  rownames(tmp_df)[length(rownames(tmp_df))]="p"
  tmp_df=rbind(tmp_df,d)
  rownames(tmp_df)[length(rownames(tmp_df))]="d"
  tmp_df=rbind(tmp_df,q)
  rownames(tmp_df)[length(rownames(tmp_df))]="q"
  tmp_df=rbind(tmp_df,combined_obs)
  rownames(tmp_df)[length(rownames(tmp_df))]="combined_obs"
  
  tmp_df=rbind(tmp_df,box_p)
  rownames(tmp_df)[length(rownames(tmp_df))]="Box Test p-value"
  
  tmp_df=rbind(tmp_df,tv_arima_R2)
  rownames(tmp_df)[length(rownames(tmp_df))]="R2"
  pander(tmp_df)
}

model_comp_plots=function(stepwise_model,tv_arima){
  print(qplot(study_var,fitted(stepwise_model))+theme_bw()+ggtitle("Stepwise Polynomial Fit")+
          xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values"))
  print(qplot(study_var,as.vector(fitted(tv_arima)))+theme_bw()+ggtitle("ARIMAX Fit")+
          xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values"))
}



transformed_plot=function(tv_arima,tv){
  #scratch code for finding point of diminishing returns
  lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
  sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]
  # sqrt_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][3]
  tv_lim=max(tv)
  tv_dummy=seq(0,tv_lim)
  session_dummy=lin_coef*tv_dummy+sqr_coef*tv_dummy^2
  # +sqrt_coef*sqrt(tv_dummy)
  
  qplot(tv_dummy,session_dummy)+theme_bw()+
    ggtitle("Transformed Effect on Visits")+ylab("Sessions")
}



# tv_arima=Airings_arima
# tv=Airings
# freq=365
model_validation=function(tv_arima,study_var,tv,freq,date){
  
  #forcing ts form
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=ts(study_var, frequency = round(freq/2))
    stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
  }
  
  #building exogeneous regressors
  xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
  xreg_matrix=xreg_matrix[,-1]
  
  pred_hor=5
  n <- length(ts_var)
  k <- round(n*(1/2)); if (k<60) k=60 # minimum data length for fitting a model
  
  eff=n-k-pred_hor
  MAPE<- matrix(NA,eff)
  order=arimaorder(tv_arima)
  
  
  for(i in 1:eff){
    xshort <-ts_var[1:(k+(i-1))]
    xnext <- ts_var[(k+i):(k+(i-1)+pred_hor)]
    
    xreg_matrix_short=xreg_matrix[1:(k+(i-1)),]
    xreg_matrix_next=xreg_matrix[(k+i):(k+(i-1)+pred_hor),]
    
    
    fit2 <- Arima(xshort, model=tv_arima,xreg=xreg_matrix_short,include.drift=FALSE, method="ML")
    # fit2 <- Arima(xshort, order=order[1:3], seasonal=order[4:6],xreg=xreg_matrix_short)
  
    fcast2 <- forecast(fit2,xreg=xreg_matrix_next)
    fcast_pred=fcast2[['mean']]
    # plot(fcast_pred)
    # plot(c(xshort,xnext))
    
    # c(sub_study$tv.spend[1:(k+(i-1))])
    MAPE[i] <- mean(abs(fcast_pred-xnext)/xnext)*100
  }
  
  qplot(date[(k+pred_hor+1):n],MAPE)+theme_bw()+geom_smooth(method="lm")+
    ggtitle("ARIMA Model Validation")+ylab("Mean Absolute Percent Error")+xlab("Additional Training Points")
  
}


add_forecast_to_df=function(channel_sessions_long,tv_arima,study_var){
  
  
  
  #Fitting old values
  xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
  xreg_matrix=xreg_matrix[,-1]
  
  fcast2 <- forecast(tv_arima,xreg=xreg_matrix)
  fcast_pred=fcast2[['mean']]
  
  #Forecasting new values
  
  #These 2 lines of code is far from robust- better to pass in the variable name later
  tv_match_start=which(channel_sessions_long$tv.spend== tv[1])[1]
  tv_start_ind=which(channel_sessions_long$tv.spend== tv[1])[1]+length(tv)
  
  tv_next=channel_sessions_long$tv.spend[tv_start_ind:nrow(channel_sessions_long)]
  xreg_matrix_next=matrix(c(tv_next,tv_next^2),ncol=2)
  
  fcast3 <- forecast(tv_arima,xreg=xreg_matrix_next)
  fcast_pred_next=fcast3[['mean']]
  
  full_forecast_fit=as.vector(c(as.vector(fitted(tv_arima)),fcast_pred_next))
  channel_sessions_long$forecast=NA
  channel_sessions_long$forecast[tv_match_start:nrow(channel_sessions_long)]=full_forecast_fit
    
  
  #check package forecasts against
#  plot(fcast2)
  plot(fcast3)
#   plot(channel_sessions_long$forecast)
#   lines(study_var)
  return(channel_sessions_long)
}
  
  

