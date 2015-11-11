library(tidyr)
library(dplyr)
library(corrplot)
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
            WHERE has_attributed_revenue=1
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

levels(as.factor(data_long$channel))

data_long=data_long %>% select(order_number,channel,revenue,session_count)


disp_inds=grep("display",data_long$channel)
data_long$channel[disp_inds]="display"


email_inds=grep("email",data_long$channel)
data_long$channel[email_inds]="email"

#combine displays and combine emails by order
data_long=data_long %>% group_by(order_number,channel) %>% 
  summarise(revenue=max(revenue),session_count=sum(session_count))

# putting data in wide form
data_wide=data_long %>%   spread(key=channel,value=session_count)

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



data_wide=data_wide %>% group_by(order_number) %>% summarise(revenue=mean(revenue),
          affiliates=sum(affiliates),display=sum(display),email=sum(email),
          organic.search=sum(organic.search),paid.search=sum(paid.search),
          referral=sum(referral),tv=sum(tv))
data_wide$order_number=NULL
#Correlation plot- clearly there is need for interaction terms
corrplot(cor(data_wide))

##Sanity checks
#every row should have at least one channel influence
row_sums=apply(data_wide[,2:8],1,function(x){
  sum(x)
})
summary(row_sums)


all_summaries=data_wide %>% summarise_each(funs(min,mean,median,max))

#Dummify for tabulation
channels_dummy=data_wide
channels_dummy=apply(data_dummy[,2:8],2,function(x){
  pos_inds=which(x>0)
  x[pos_inds]=1
  as.factor(x)
})
table(channels_dummy)

#Throwing some models at the wall
all_model_no_interaction=lm(revenue~0+.,data=data_wide)
summary(all_model_no_interaction)
















