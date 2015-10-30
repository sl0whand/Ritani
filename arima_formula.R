

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
    I_inds=as.numeric(substr(forecast(arima_object,xreg=c(xreg1))$method,9,9))
    
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