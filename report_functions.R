######################################
# Report functions
# holds functions called to produce the report and 'Tv_channel_Arima_forecst_uploader.R'
#
#
#
######################################




#Fits a 2nd order polynomial without time-series considerations
report_poly_fit_2=function(study_var,tv){
  poly_fit=lm(study_var~tv+I(tv^2))
  return(poly_fit)
} #end of report_poly_fit_2

#Displays the model coefficients and diagnostics for the 2nd order polynomial fit without time-series considerations
report_poly_fit_2_sub=function(poly_fit,study_var){
  tmp_df=data.frame(round(coef(poly_fit),3))
  tmp_df=rbind(tmp_df,cor(fitted(poly_fit),na.omit(study_var))^2)
  rownames(tmp_df)[length(rownames(tmp_df))]="R2"
  pander(tmp_df)
} #end of report_poly_fit_2_sub


# Fits a polyomial models without time-series considerations using backwards stepwise selection on akaike information criterion
step_model=function(study_var,tv){
  big_poly_fit=lm(study_var~tv+I(tv^2)+I(tv^3)+I(tv^(1/2)))
  stepwise_model=step(big_poly_fit,k=log(nrow(channel_sessions_long)))
  return(stepwise_model)
} # end of step_model

#Displays the model coefficients and diagnostics for the stepwise polynomial fit without time-series considerations
step_pander=function(stepwise_model){
  
  tmp_df=data.frame(round(coef(stepwise_model),3))
  tmp_df=rbind(tmp_df,cor(fitted(stepwise_model),na.omit(study_var))^2)
  rownames(tmp_df)[length(rownames(tmp_df))]="R2"
  pander(tmp_df)
} # step_pander



# Produces a two graphs: one of the non-time-series models of the covariate against dependent
# The other plots the residuals of the stepwise model
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


#Plots the seasonal trend lowess decomposition
stl_func=function(study_var,freq){
  ts_var=ts(study_var, frequency = freq)
  stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
  if (class(stl_obj)=="try-error") {
    ts_var=ts(study_var, frequency = round(freq/2))
    stl_obj=stl(ts_var, s.window="periodic",robust=TRUE)
  }
  plot(stl_obj)+title(paste(title_paste,"Seasonal Trend Decomposition"))
} # stl_func



#Fits the arima model- the auto.arima will select paramters via stepwise selection on AIC
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
  #Forcing model calculations not to take any shortcuts
  #Not allowing mean nor drift to allow for easier integration in Excel *shudder*
  tv_arima=auto.arima(ts_var,xreg=xreg_matrix,
                          allowdrift=FALSE,allowmean=FALSE,stepwise=FALSE,approx=FALSE)
  
  #Finding and removing high leverage points 
  # high_resid_inds=which(tv_arima$residuals > (mean(tv_arima$residuals+3*sd(tv_arima$residuals))) |
          # tv_arima$residuals < (mean(tv_arima$residuals-3*sd(tv_arima$residuals))))


  
   # standard_resids=(tv_arima$residuals-mean(tv_arima$residuals))/sd(tv_arima$residuals)
   # hist(standard_resids)
  
  
  return(tv_arima)
} # end of arima_func


#Displays coefficients and model diagnostics for the arima model
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


#Prints two plots: one of the observed vs fit of the non-time-series model, the other with time-series considerations
model_comp_plots=function(stepwise_model,tv_arima){
  print(qplot(study_var,fitted(stepwise_model))+theme_bw()+ggtitle("Stepwise Polynomial Fit")+
          xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values"))
  print(qplot(study_var,as.vector(fitted(tv_arima)))+theme_bw()+ggtitle("ARIMAX Fit")+
          xlab(paste(title_paste,"Sessions"))+ylab("Fitted Values"))
} # model_comp_plots


#plots the directionally accurate lift estimates
transformed_plot=function(tv_arima,tv,title_paste){
  #scratch code for finding point of diminishing returns
  lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
  sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]
  # sqrt_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][3]
  # tv_lim=max(tv)
  # tv_dummy=seq(0,tv_lim,by=round(tv_lim/200))
  session_dummy=lin_coef*tv+sqr_coef*tv^2
  # +sqrt_coef*sqrt(tv_dummy)
  
  
  conf=confint(tv_arima,level=.8)
  tv_rows=grep("tv",rownames(conf))
  lin_low=conf[tv_rows[1],1]
  lin_high=conf[tv_rows[1],2]
  sqr_low=conf[tv_rows[2],1]
  sqr_high=conf[tv_rows[2],2]
  
  session_low=lin_low*tv+sqr_low*tv^2
  session_high=lin_high*tv+sqr_high*tv^2
  
  
  temp_df=data.frame(tv,session_dummy,session_low,session_high)
  
  ggplot(temp_df)+theme_bw()+
    geom_point(aes(x=tv,session_dummy,color="Approximate Lift"))+
    geom_point(aes(x=tv,session_low,color="Lower Bound, \n %80 Confidence"))+
    geom_point(aes(x=tv,session_high,color="Upper Bound, \n %80 Confidence"))+
    ggtitle("Approximate Transformed Effect on Visits")+ylab("Sessions")+xlab(title_paste)+
    theme(legend.title=element_blank())
  
  
  
} # transformed_plot


#Plots the directionally accurate lift estimates with observed vales over time
transformed_plot_by_date=function(channel_sessions_long,tv_arima,var_name){
  
  var_ind=which(names(channel_sessions_long)==var_name)
  study_var=channel_sessions_long[,var_ind][[1]]
  
  lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
  sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]
  tv_lift=lin_coef*channel_sessions_long$tv.spend+sqr_coef*channel_sessions_long$tv.spend^2
  conf=confint(tv_arima,level=.8)
  tv_rows=grep("tv",rownames(conf))
  lin_low=conf[tv_rows[1],1]
  lin_high=conf[tv_rows[1],2]
  sqr_low=conf[tv_rows[2],1]
  sqr_high=conf[tv_rows[2],2]
  
  tv_lift_low=lin_low*channel_sessions_long$tv.spend+sqr_low*channel_sessions_long$tv.spend^2
  tv_lift_high=lin_high*channel_sessions_long$tv.spend+sqr_high*channel_sessions_long$tv.spend^2
  
  
  temp_df=data.frame(date=channel_sessions_long$date,study_var=channel_sessions_long[,var_ind][[1]],tv_lift,tv_lift_low,tv_lift_high)
  if (length(which(is.na(temp_df$date)))>0) temp_df=temp_df[-which(is.na(temp_df$date)),]
  
  
  
  ggplot(temp_df)+theme_bw()+
    geom_line(aes(x=date,y=study_var,color="Observed Sessions"))+
    geom_line(aes(x=date,y=tv_lift,color="Approximate Lift"))+
    geom_line(aes(x=date,y=tv_lift_low,color="Lower Bound, \n %80 Confidence"))+
    geom_line(aes(x=date,y=tv_lift_high,color="Upper Bound, \n %80 Confidence"))+
    ggtitle("Lift Estimation")+ylab("Sessions")+xlab("Date")+
    theme(legend.title=element_blank())
} # transformed_plot


# tv_arima=Airings_arima
# tv=Airings
# freq=365
#Performs and prints graph of rolling forecast model validation
#This uses only the model intill fit to forecast- it does not adjust forecast coefficients
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
  
  pred_hor=3
  n <- length(ts_var)
  k <- round(n*.7); if (k<60) k=60 # minimum data length for fitting a model
  
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
    
    
    # fit2 <- Arima(xshort, model=tv_arima,xreg=xreg_matrix_short,method="ML")
    fit2 <- Arima(xshort, order=order[1:3], seasonal=order[4:6],xreg=xreg_matrix_short)
  
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


#Adds the forecasts and directionally accurate lift estimates to the df to allow for spreadsheet production
#Also produces a graph of forecast past training period
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
  
# Attempted to forecast with only tv as 0- not valid  
#   xreg_0=matrix(rep(0,length(tv)*2),ncol=2)
#   fcast_0 <- forecast(tv_arima,xreg=xreg_0,level=80)
#   fcast_pred_0=fcast_0[['mean']]
  
  
  
  plot_date=channel_sessions_long$date[(rows-length(full_forecast_fit)+1):rows]
  plot_var=channel_sessions_long[((rows-length(full_forecast_fit)+1):rows),var_ind][[1]]
  low=fcast3$lower[1:length(fcast_pred_next)]
  plot_lower=c(rep(NA,(length(full_forecast_fit))-length(low)),low)
  up=fcast3$upper[1:length(fcast_pred_next)]
  plot_upper=c(rep(NA,(length(full_forecast_fit))-length(up)),up)
  temp_df=data.frame(plot_date,full_forecast_fit,plot_var,plot_lower,plot_upper)
  
  ##Make a good ggplot!

   
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
     
   #Adding Directionally accurate lift estimates
   lin_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][1]
   sqr_coef=coef(tv_arima)[grep("tv",names(coef(tv_arima)))][2]
   
   channel_sessions_long$tv_lift=lin_coef*channel_sessions_long$tv.spend+sqr_coef*channel_sessions_long$tv.spend^2
   
   conf=confint(tv_arima,level=.8)
   tv_rows=grep("tv",rownames(conf))
   lin_low=conf[tv_rows[1],1]
   lin_high=conf[tv_rows[1],2]
   sqr_low=conf[tv_rows[2],1]
   sqr_high=conf[tv_rows[2],2]
   
   channel_sessions_long$tv_lift_low_bound=lin_low*channel_sessions_long$tv.spend+sqr_low*channel_sessions_long$tv.spend^2
   channel_sessions_long$tv_lift_upper_bound=lin_high*channel_sessions_long$tv.spend+sqr_high*channel_sessions_long$tv.spend^2
   
  return(channel_sessions_long)
} # end of add_forecast_to_df
  

# 
# PR2=function(tv_arima,study_var,tv,freq,date){
#   
#   #forcing ts form
#   ts_var=ts(study_var, frequency = freq)
#   stl_obj=try(stl(ts_var, s.window="periodic",robust=TRUE))
#   if (class(stl_obj)=="try-error") {
#     ts_var=study_var
#     
#   }
#   
#   #building exogeneous regressors
#   xreg_matrix<-model.matrix(study_var~tv+I(tv^2))
#   xreg_matrix=xreg_matrix[,-1]
#   
#   pred_hor=1
#   n <- length(ts_var)
#   k <- round(n*.7); if (k<60) k=60 # minimum data length for fitting a model
#   
#   eff=n-k-pred_hor
#   if (eff<10) {
#     print("There are not enough data to validate this model. You must have at least 70 observations")
#     return(NULL)
#   }  
#   
#   leave_one_forecast<- matrix(NA,eff)
#   order=arimaorder(tv_arima)
#   
#   
#   for(i in 1:eff){
#     # print(i)
#     xshort <-ts_var[1:(k+(i-1))]
#     
#     
#     xreg_matrix_short=xreg_matrix[1:(k+(i-1)),]
#     xreg_matrix_next=xreg_matrix[(k+i):(k+(i-1)+pred_hor),]
#     
#     
#     # fit2 <- Arima(xshort, model=tv_arima,xreg=xreg_matrix_short,method="ML")
#     fit2 <- Arima(xshort, order=order[1:3], seasonal=order[4:6],xreg=xreg_matrix_short)
#     
#     
#     fcast2 <- forecast(fit2,xreg=matrix(xreg_matrix_next,ncol=2))
#     fcast_pred=fcast2[['mean']]
# 
#     
#     # c(sub_study$tv.spend[1:(k+(i-1))])
#     leave_one_forecast[i] <- fcast_pred
#   }
#   
#   Pr2=round(cor(leave_one_forecast,study_var[(length(study_var)-length(leave_one_forecast)+1):length(study_var)])^2,2)
#   
# 
#   return(Pr2)
#   
# } # end of PR2
# 
# 

