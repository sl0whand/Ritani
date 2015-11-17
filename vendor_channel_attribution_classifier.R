library(tidyr)
library(dplyr)
library(corrplot)
library(glmnet)
library(readr)
library(caret)
library(randomForest)
library(pander)
source("Ritani_log_analysis_DB_connection.R")
###



#####imorting purchasers
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
data_long$revenue=NULL
data_long$revenue=NULL
data_long$purchaser=1

# Importing non-purschasers
non_purchaser_data=tbl_df(read_csv(file=gzfile('non-purchaser-channels.csv.gz'),col_names=FALSE))
non_purchaser_data=non_purchaser_data %>% rename(Time=X1) 
# non_purchaser_data$Time=NULL
non_purchaser_data$X4=NULL
non_purchaser_data$purchaser=0
non_purchaser_data$session_count=1
non_purchaser_data=non_purchaser_data %>% rename(order_number=X2) 
non_purchaser_data=non_purchaser_data %>% rename(channel=X3)



#omit visit within 30 minutes of each other
non_purchaser_data=non_purchaser_data %>% arrange(order_number,channel,Time)

non_purchaser_data=non_purchaser_data %>% group_by(order_number,channel) %>%
  mutate(duration = c(1,diff(Time))*(60*24))
short_visit_inds=which(non_purchaser_data$duration<30 & non_purchaser_data$duration>=0)

non_purchaser_data %>% group_by(channel,purchaser) %>%  summarise(sessions=n()) %>% arrange(channel,purchaser)
non_purchaser_data[-short_visit_inds,] %>% group_by(channel,purchaser) %>%  summarise(sessions=n()) %>% arrange(channel,purchaser)
non_purchaser_data=non_purchaser_data[-short_visit_inds,]



non_purchaser_data$duration=NULL
non_purchaser_data$Time=NULL

#combine data
data_long=rbind(data_long,non_purchaser_data)
# data_long$purchaser=as.factor(data_long$purchaser)

#cleaning some unneccesary 
data_long=data_long[-which(data_long$channel=="unknown"),]
data_long=data_long[-which(data_long$channel=="social"),]
data_long=data_long[-grep("retailer",data_long$channel),]
data_long=data_long[-which(data_long$channel=="tv"),]
data_long=data_long[-which(data_long$channel=="session continuation"),]

#check channel distribution
data_long %>% group_by(channel,purchaser) %>%  summarise(count=n()) %>% arrange(desc(count))

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
  summarise(session_count=sum(session_count),purchaser=first(purchaser))

#recheck channel distribution with corrections
data_long %>% group_by(channel,purchaser) %>%  summarise(sessions=n()) %>% arrange(channel,purchaser)
data_long %>% group_by(purchaser) %>%  summarise(Users=n()) %>% arrange(purchaser)


#need to resample to adjust for class imbalance
#duplicating instead of resampling due to order_number spreading difficulties
channel_levels=levels(as.factor(data_long$channel))
purchase_levels=levels(as.factor(data_long$purchaser))
resampled_data_frame=data.frame()
for (purchase in purchase_levels) {
  for (channel in channel_levels){
    ch_inds=which(data_long$channel==channel & data_long$purchaser==purchase)
    for (i in 1:round(10000/length(ch_inds))){
      temp=data_long[ch_inds,]
      temp$order_number=paste0(temp$order_number,i)
      resampled_data_frame=rbind(resampled_data_frame,temp)
    }
  }
}
#recheck channel distribution with resampling
resampled_data_frame %>% group_by(channel,purchaser) %>%  summarise(Sessions=n()) %>% arrange(desc(Sessions))
resampled_data_frame %>% group_by(purchaser) %>%  summarise(Users=n()) %>% arrange(desc(Users))

# putting data in wide form
 # data_wide=resampled_data_frame %>%  spread(key=channel,value=session_count)
#try without bootstrapping
data_wide=data_long %>%  spread(key=channel,value=session_count)



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
data_wide=data_wide %>% group_by(order_number) %>% summarise(purchaser=first(purchaser),affiliates=sum(affiliates),
                                                             direct=sum(direct),display=sum(display),email=sum(email),
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


#### check to see if number of sessions are related to purchase
row_sums=apply(data_wide[,2:8],1,function(x){sum(x)})
data_wide$row_sums=row_sums
pander(data_wide %>% group_by(purchaser) %>% summarise(row_sums=mean(row_sums),
                                                affiliates=mean(affiliates),
                                                direct=mean(direct),
                                                display=mean(display),
                                                email=mean(email),
                                                organic=mean(organic),
                                                paid=mean(paid),
                                                referral=mean(referral))
,digits=2)

#t test total sessions- ss diff
t.test(as.vector(data_wide %>% filter(purchaser==0) %>% select(row_sums))[[1]],
       as.vector(data_wide %>% filter(purchaser==1) %>% select(row_sums))[[1]])
#t test affiliates- ss diff
t.test(as.vector(data_wide %>% filter(purchaser==0) %>% select(affiliates))[[1]],
       as.vector(data_wide %>% filter(purchaser==1) %>% select(affiliates))[[1]])
#t test direct- ss diff
t.test(as.vector(data_wide %>% filter(purchaser==0) %>% select(direct))[[1]],
       as.vector(data_wide %>% filter(purchaser==1) %>% select(direct))[[1]])
#t test display- no diff
t.test(as.vector(data_wide %>% filter(purchaser==0) %>% select(display))[[1]],
       as.vector(data_wide %>% filter(purchaser==1) %>% select(display))[[1]])
#t test email- ss diff
t.test(as.vector(data_wide %>% filter(purchaser==0) %>% select(email))[[1]],
       as.vector(data_wide %>% filter(purchaser==1) %>% select(email))[[1]])
#t test organic- ss diff
t.test(as.vector(data_wide %>% filter(purchaser==0) %>% select(organic))[[1]],
       as.vector(data_wide %>% filter(purchaser==1) %>% select(organic))[[1]])
#t test paid- ss diff
t.test(as.vector(data_wide %>% filter(purchaser==0) %>% select(paid))[[1]],
       as.vector(data_wide %>% filter(purchaser==1) %>% select(paid))[[1]])
#t test referral- ss diff
t.test(as.vector(data_wide %>% filter(purchaser==0) %>% select(referral))[[1]],
       as.vector(data_wide %>% filter(purchaser==1) %>% select(referral))[[1]])


 # ggplot(data_wide %>% filter(row_sums<30))+geom_boxplot(aes(y=row_sums,x=purchaser))
 # ggplot(data_wide %>% filter(row_sums<30))+geom_density(aes(x=row_sums,color=purchaser))

data_wide$row_sums=NULL

#Invert session counts assuming more sessions are better
# channels_invert=data_wide
# channels_invert[,2:8]=data.frame(apply(channels_invert[,2:8],2,function(x){
#   pos_inds=which(x>0)
#   x[pos_inds]=1/x[pos_inds]
#   x
# })
# )


# #Correlation plot of channel sessions
# corrplot(cor(data_wide[,1:8]))

# #correlation plot of channel indicators
# corrplot(cor(channels_invert))




#Checking out various models
inTrain = createDataPartition(y=data_wide$purchaser, p = .6)[[1]]
training = data_wide[ inTrain,]
testing = data_wide[-inTrain,]



#high leverage point
fit1= lm(purchaser~.+0,data=training)
# summary(fit1)
# summary(fitted(fit1))
# qplot(fitted(fit1))
# pred=round(predict(fit1,newdata= data_wide, type = "response"))
# confusionMatrix(training$purchaser, round(fitted(fit1)))


# pred=round(predict(fit1,newdata= data_wide, type = "response"))

# round(coef(fit1)[1:7]/sum(coef(fit1)[1:7]),2)


x <- model.matrix(formula(fit1),data=training)
y<- as.matrix(training$purchaser)
fit<-glmnet(x,y, family="gaussian", alpha=0.9, lambda=0.0001,intercept=FALSE)
# summarize the fit
coefficients(fit)
# make predictions
predictions <- predict(fit, x)
predictions[which(predictions>1)]=1
# summarize accuracy
confusionMatrix(y, round(predictions))


#checking accuracy on test set
x_test <- model.matrix(formula(fit1),data=testing)
y_test<- as.matrix(testing$purchaser)
predictions <- predict(fit, x_test)
predictions[which(predictions>1)]=1
confusionMatrix(y_test, round(predictions))

#checking accuracy over all
x_all <- model.matrix(formula(fit1),data=data_wide)
y_all<- as.matrix(data_wide$purchaser)
predictions <- predict(fit, x_all)
predictions[which(predictions>1)]=1
confusionMatrix(y_all, round(predictions))


coef_df=data.frame(Coefficients=coefficients(fit)[2:8])
rownames(coef_df)=(coefficients(fit))@Dimnames[[1]][2:8]
pander(coef_df,digits=2)


#fitting random forest just for variable importance analysis
modFit <- randomForest(purchaser ~. , data=training)
varImpPlot(modFit)


# fit_invert=lm(purchaser~.+0,data=channels_invert)
#   
# summary(fit_invert)
# qplot(fitted(fit_invert)/max(fitted(fit_invert)))
# confusionMatrix(channels_invert$purchaser, round(fitted(fit_invert)))
# round(coef(fit_invert)[1:7]/sum(coef(fit_invert)[1:7]),2)




#Continuing using the dummy indicators- now check for interaction
#The front runner
all_model_dummy=lm(revenue~.*.+0,data=channels_dummy)
summary(all_model_dummy)
round(coef(all_model_dummy)[1:7]/sum(coef(all_model_dummy)[1:7]),2)


stepwise_model=step(all_model_dummy,k=log(nrow(channels_dummy)))
summary(stepwise_model)
#low p-value implies the models are not statistically different - may as well take simpler model
anova(stepwise_model,all_model_dummy, test="Chi")
#low p-value implies the models are not statistically different - may as well take simpler model
anova(all_model_dummy,all_model_no_interaction_dummy, test="Chi")

round(coef(stepwise_model)[1:7]/sum(coef(stepwise_model)[1:7]),2)




















