library(tidyr)
library(dplyr)
library(corrplot)
library(glmnet)
source("Ritani_log_analysis_DB_connection.R")

query=paste("
            SELECT order_number, name AS channel, state,
            max(total) AS revenue, count(*) as session_count
            FROM (
            SELECT channel_id, clickstream_id, session_id
            FROM channel_touchpoints
            ) ct INNER JOIN (
            SELECT id, name, has_attributed_revenue
            FROM channels
            ) ch ON ct.channel_id=ch.id
            INNER JOIN (
            SELECT id, pre_purchase
            FROM sessions
            WHERE pre_purchase=1
            ) se ON ct.session_id=se.id
            INNER JOIN (
            SELECT id, order_number, total, state
            FROM clickstreams
            ) cl ON ct.clickstream_id=cl.id
            GROUP BY order_number, channel
            HAVING state LIKE '%shipped%' OR state LIKE '%accepted%' OR state LIKE '%delivered%'
            OR state LIKE '%store%' OR state LIKE '%charged%' OR state LIKE '%authorized%'
            OR state LIKE '%shipped%'
                        ")

data_long=tbl_df(dbGetQuery(con,query))

#ditching some data
data_long$state=NULL
data_long=data_long[-which(data_long$revenue==0),]
data_long=data_long[-which(data_long$channel=="unknown"),]
data_long=data_long[-which(data_long$channel=="tv"),]
data_long=data_long[-which(data_long$channel=="session continuation"),]

#check channel distribution
data_long %>% group_by(channel) %>%  summarise(count=n()) %>% arrange(desc(count))

#reassign some channel names
disp_inds=grep("display",data_long$channel)
data_long$channel[disp_inds]="display"

email_inds=grep("email",data_long$channel)
data_long$channel[email_inds]="email"

email_inds=grep("paid",data_long$channel)
data_long$channel[email_inds]="paid"

email_inds=grep("organic",data_long$channel)
data_long$channel[email_inds]="organic"



#combine displays and combine emails by order
data_long=data_long %>% group_by(order_number,channel) %>% 
  summarise(revenue=max(revenue),session_count=sum(session_count))

#recheck channel distribution with corrections
data_long %>% group_by(channel) %>%  summarise(count=n()) %>% arrange(desc(count))
data_long %>% group_by(order_number) %>%  summarise(count=n()) %>% arrange(desc(count))

#need to resample to adjust for class imbalance
#duplicating instead of resampling due to order_number spreading difficulties
channel_levels=levels(as.factor(data_long$channel))
resampled_data_frame=data.frame()
for (channel in channel_levels){
  ch_inds=which(data_long$channel==channel)
  for (i in 1:round(60000/length(ch_inds))){
    temp=data_long[ch_inds,]
    temp$order_number=paste0(temp$order_number,i)
    resampled_data_frame=rbind(resampled_data_frame,temp)
  }
}
#recheck channel distribution with resampling
resampled_data_frame %>% group_by(channel) %>%  summarise(count=n(),revenue=mean(revenue)) %>% arrange(desc(count))

# putting data in wide form
data_wide=resampled_data_frame %>%   spread(key=channel,value=session_count)

#fix column names
names(data_wide) <- sub(" ", ".", names(data_wide))

#setting all missings to 0
numeric_inds=sapply(data_wide,is.numeric)
data_wide[,numeric_inds]=apply(data_wide[,numeric_inds],2,function(x){
  na_inds=which(is.na(x))
  if (length(na_inds)>0){
    x[na_inds]=0  
  } else {
    x
  }
  x
})

#combining orders for total sessions by each channel
data_wide=data_wide %>% group_by(order_number) %>% summarise(revenue=mean(revenue),
          affiliates=sum(affiliates),direct=sum(direct),display=sum(display),email=sum(email),
          organic=sum(organic),paid=sum(paid),referral=sum(referral))
#order_number is irrelevant now
data_wide$order_number=NULL


##Sanity checks
row_sums=apply(data_wide[,2:8],1,function(x){sum(x)})
summary(row_sums)
sd(row_sums)
qplot(row_sums)

#omitting outrageously high session counts
high_inds=which(row_sums> (mean(row_sums)+10*sd(row_sums)))
if (length(high_inds)) data_wide=data_wide[-high_inds,]



summary(data_wide$revenue)
sd(data_wide$revenue)
qplot(data_wide$revenue)
#omitting outrageously revenue
high_inds=which(data_wide$revenue> (mean(data_wide$revenue)+10*sd(data_wide$revenue)))
 if (length(high_inds)) data_wide=data_wide[-high_inds,]

row_sums=apply(data_wide[,2:8],1,function(x){sum(x)})

#check for implict relationship between total sessions and total revenue
cor(row_sums,data_wide$revenue)
qplot(row_sums,data_wide$revenue)+geom_smooth()+theme_bw()+
  xlab("Sessions")+ylab("Purchase Total (Dollars)")


  
   
#Dummify for tabulation
channels_dummy=data_wide
channels_dummy[,2:8]=data.frame(apply(channels_dummy[,2:8],2,function(x){
  pos_inds=which(x>0)
  x[pos_inds]=1
  x
})
)


#Invert session counts assuming more sessions are better
channels_invert=data_wide
channels_invert[,2:8]=data.frame(apply(channels_invert[,2:8],2,function(x){
  pos_inds=which(x>0)
  x[pos_inds]=1/x[pos_inds]
  x
})
)


#Correlation plot of channel sessions
corrplot(cor(data_wide[,1:8]))
#correlation plot of channel indicators
corrplot(cor(channels_dummy))
#correlation plot of channel indicators
corrplot(cor(channels_invert))





all_model_no_interaction=lm(revenue~.+0,data=data_wide)
summary(all_model_no_interaction)
round(coef(all_model_no_interaction)/sum(coef(all_model_no_interaction)),2)

all_model_no_interaction_dummy=lm(revenue~.+0,data=channels_dummy)
summary(all_model_no_interaction_dummy)
round(coef(all_model_no_interaction_dummy)/sum(coef(all_model_no_interaction_dummy)),2)

all_model_no_interaction_invert=lm(revenue~.+0,data=channels_invert)
summary(all_model_no_interaction_invert)
round(coef(all_model_no_interaction_invert)/sum(coef(all_model_no_interaction_invert)),2)


#Continuing using the dummy indicators- now check for interaction
#The front runner
all_model_dummy=lm(revenue~.*.+0,data=channels_dummy)
summary(all_model_dummy)

 stepwise_model=step(all_model_dummy,k=log(nrow(channels_dummy)))
 summary(stepwise_model)
 
 anova(stepwise_model,all_model_no_interaction_dummy, test="Chi")
 
 round(coef(stepwise_model)[1:7]/sum(coef(stepwise_model)[1:7]),2)
 
















