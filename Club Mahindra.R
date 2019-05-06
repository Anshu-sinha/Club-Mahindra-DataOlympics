#clearing the global enviornment
rm(list = ls())

#Loading the Package
library(dplyr)
library(psych)
library(caret)
library(car)
library(lubridate)
require(corrplot)

#setting the directory
setwd("F:/hackathon/Club Mahindra")

#Reading the file
train <- read.csv("F:/hackathon/Club Mahindra/train.csv")
test <- read.csv("F:/hackathon/Club Mahindra/test.csv")

#Understanding the data
head(train)
tail(train)
summary(train)
names(train)
View(train)
dim(train)
describe(train) #from psych package
str(train)
sapply(train,function(x) sum(is.na(x))) #missing in state_code_residence, season_holidayed_code

#Important variables from describe func
  # reservation_id, booking_date, checkin_date, checkout_date, member_age_buckets, memberid, cluster_code, 
  # reservationstatusid_code, resort_id

# train <- train

#converting the Booking_date variable into duration with system date
train$booking_date <- dmy(train$booking_date)
train$booking_date <- as.numeric(Sys.Date())-as.numeric(train$booking_date)
class(train$booking_date)
Neg <- as.numeric(Sys.Date())
train$booking_date <- ifelse(train$booking_date<0,Neg,train$booking_date)


#converting the Checkin_date variable into duration with system date
train$checkin_date <- dmy(train$checkin_date)
train$checkin_date <- as.numeric(Sys.Date())-as.numeric(train$checkin_date)
class(train$checkin_date)
train$checkin_date <- ifelse(train$checkin_date<0,Neg,train$checkin_date)


#converting the Checkout_date variable into duration with system date
train$checkout_date <- dmy(train$checkout_date)
train$checkout_date <- as.numeric(Sys.Date())-as.numeric(train$checkout_date)
class(train$checkout_date)
train$checkout_date <- ifelse(train$checkout_date<0,Neg,train$checkout_date)

#Making a new variable from checkin and checkout
train$Stay_duration <- train$checkin_date - train$checkout_date

#rearranging the columns
train <- train[c(1:4,25,5:24)]

#Removing the not required variable
train$checkin_date <- NULL
train$checkout_date <- NULL  
train$reservation_id <- NULL #More than 30 levels
train$memberid <- NULL
train$resort_id <- NULL

#to identify which categorical variable is significant
summary(aov(train$amount_spent_per_room_night_scaled~train$member_age_buckets)) #significant
summary(aov(train$amount_spent_per_room_night_scaled~train$cluster_code)) #significant
summary(aov(train$amount_spent_per_room_night_scaled~train$reservationstatusid_code)) #significant

# # Proportion less than 1 named as Others
# train$reservationstatusid_code <- as.character(train$reservationstatusid_code)
# class(train$reservationstatusid_code)
# prop=(prop.table(table(train$reservationstatusid_code)))*100
# View(prop)
# class(train$cluster_code)
# train$reservationstatusid_code <- as.factor(train$reservationstatusid_code)


# Encoding the member_age_buckets as factor 
train[train == ""] <- NA # putting NA's into blank spaces
colSums(is.na(train)) #Checking that blank has been filled with na's or not

train$member_age_buckets <- as.numeric(train$member_age_buckets,
                                    levels = c('A','B','C','D','E','F','G','H','I','J'),
                                    labels = c(1,2,3,4,5,6,7,8,9,10))

# Encoding the cluster_code as factor 
train$cluster_code <- as.numeric(train$cluster_code,
                                  levels = c('A','B','C','D','E','F'),
                                  labels = c(1,2,3,4,5,6))

# Encoding the reservationstatusid_code as factor 
train$reservationstatusid_code <- as.numeric(train$reservationstatusid_code,
                                              levels = c('A','B','C','D'),
                                              labels = c(1,2,3,4))
str(train)


#User defined func to create audit report
mydata_num = function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_perct = mean(is.na(x))
  sum = sum(x,na.rm = T)
  mean = mean(x,na.rm = T)
  median = median(x,na.rm = T)
  std_dev = sd(x,na.rm = T)
  cv = sd(x,na.rm = T)/mean(x,na.rm = T)
  var = var(x,na.rm = T)
  range = max(x,na.rm = T)-min(x,na.rm = T)
  perctl = quantile(x, p=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm = T)
  return(c(N=n,NMISS=nmiss,NMiss_Perct=nmiss_perct,Sum=sum,Avg=mean,Median=median,std_dev=std_dev,Cv=cv,Variance= var, Range=range,perctl=perctl))
}

#Vector of numerical variables
data <- sapply(train,FUN = is.numeric)
numdata <- train[data]

#Applying above defined function on variables
numstat <- data.frame(sapply(numdata,FUN=mydata_num))

#missing value treatment for numerical variable
sapply(train,function(x) sum(is.na(x))) 

miss_treat_num <- function(x){
  x[is.na(x)]=median(x,na.rm = T)  #replace missing value with mean
  return(x)
}

train1 <- data.frame(apply(numdata,2,FUN = miss_treat_num))
sapply(train1,function(x) sum(is.na(x))) 


#Outlier Treatment
outlier_treat <- function(x){
  Uc1 = quantile(x, p= 0.99,na.rm = T)
  LC1 = quantile(x, p= 0.01,na.rm = T)
  x= ifelse(x>Uc1,Uc1,x)
  x= ifelse(x<LC1,LC1,x)
  return(x)
}

train1 <- data.frame(sapply(train1,FUN = outlier_treat))
View(train1)


hist(train1$amount_spent_per_room_night_scaled)

corrplot(cor(train1), method = "number",number.font = 1,type = "full") #To increase the plot size we had to include type="full"

# trying regression
fit <- lm(amount_spent_per_room_night_scaled~., data= train1)
# fit <- train(amount_spent_per_room_night_scaled~., data= train1, method = 'lm')
summary(fit)
vif(fit)

#reducing the variables in step function
step(fit,direction = "both") #It helps in reducing the variables based on AIC value(low value good model)

fit2 <- lm(formula = amount_spent_per_room_night_scaled ~ booking_date + 
      Stay_duration + channel_code + main_product_code + numberofadults + 
        numberofchildren + persontravellingid + resort_region_code + 
        resort_type_code + room_type_booked_code + roomnights + 
        season_holidayed_code + state_code_residence + state_code_resort +
        total_pax + member_age_buckets + booking_type_code + cluster_code +
        reservationstatusid_code, data = train1)

summary(fit2)
car::vif(fit2) #Two of the variable is having high value so we will drop the 1 and we will continue with the rest

fit3 <- lm(formula = amount_spent_per_room_night_scaled ~ booking_date + 
             Stay_duration + channel_code + main_product_code + numberofadults + 
             numberofchildren + persontravellingid +resort_region_code + 
             resort_type_code + room_type_booked_code +roomnights + season_holidayed_code +
             state_code_residence +state_code_resort + total_pax + booking_type_code + 
             cluster_code, data = train1)

summary(fit3)
car::vif(fit3)


##Prediction on Test Data

#converting the Booking_date variable into duration with system date
test$booking_date <- dmy(test$booking_date)
test$booking_date <- as.numeric(Sys.Date())-as.numeric(test$booking_date)
class(test$booking_date)
Neg <- as.numeric(Sys.Date())
test$booking_date <- ifelse(test$booking_date<0,Neg,test$booking_date)


#converting the Checkin_date variable into duration with system date
test$checkin_date <- dmy(test$checkin_date)
test$checkin_date <- as.numeric(Sys.Date())-as.numeric(test$checkin_date)
class(test$checkin_date)
test$checkin_date <- ifelse(test$checkin_date<0,Neg,test$checkin_date)


#converting the Checkout_date variable into duration with system date
test$checkout_date <- dmy(test$checkout_date)
test$checkout_date <- as.numeric(Sys.Date())-as.numeric(test$checkout_date)
class(test$checkout_date)
test$checkout_date <- ifelse(test$checkout_date<0,Neg,test$checkout_date)

#Making a new variable from checkin and checkout
test$Stay_duration <- test$checkin_date - test$checkout_date

#rearranging the columns
test <- test[c(1:4,24,5:23)]

#Removing the not required variable
# test$checkin_date <- NULL
# test$checkout_date <- NULL  #keeping on hold to check whether we can make a new variables from these two 
test$reservation_id <- NULL #More than 30 levels
test$memberid <- NULL
test$resort_id <- NULL

# Encoding the member_age_buckets as factor 
test[test == ""] <- NA # putting NA's into blank spaces
colSums(is.na(test)) #Checking that blank has been filled with na's or not

test$member_age_buckets <- as.numeric(test$member_age_buckets,
                                       levels = c('A','B','C','D','E','F','G','H','I','J'),
                                       labels = c(1,2,3,4,5,6,7,8,9,10))

# Encoding the cluster_code as factor 
test$cluster_code <- as.numeric(test$cluster_code,
                                 levels = c('A','B','C','D','E','F'),
                                 labels = c(1,2,3,4,5,6))

# Encoding the reservationstatusid_code as factor 
test$reservationstatusid_code <- as.numeric(test$reservationstatusid_code,
                                             levels = c('A','B','C','D'),
                                             labels = c(1,2,3,4))
str(test)


#User defined func to create audit report
mydata_num = function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_perct = mean(is.na(x))
  sum = sum(x,na.rm = T)
  mean = mean(x,na.rm = T)
  median = median(x,na.rm = T)
  std_dev = sd(x,na.rm = T)
  cv = sd(x,na.rm = T)/mean(x,na.rm = T)
  var = var(x,na.rm = T)
  range = max(x,na.rm = T)-min(x,na.rm = T)
  perctl = quantile(x, p=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm = T)
  return(c(N=n,NMISS=nmiss,NMiss_Perct=nmiss_perct,Sum=sum,Avg=mean,Median=median,std_dev=std_dev,Cv=cv,Variance= var, Range=range,perctl=perctl))
}

#Vector of numerical variables
data <- sapply(test,FUN = is.numeric)
numdata <- test[data]

#Applying above defined function on variables
numstat <- data.frame(sapply(numdata,FUN=mydata_num))

#missing value treatment for numerical variable
sapply(test,function(x) sum(is.na(x))) 

miss_treat_num <- function(x){
  x[is.na(x)]=median(x,na.rm = T)  #replace missing value with mean
  return(x)
}

test1 <- data.frame(apply(numdata,2,FUN = miss_treat_num))
sapply(test1,function(x) sum(is.na(x))) 


#Outlier Treatment
outlier_treat <- function(x){
  Uc1 = quantile(x, p= 0.99,na.rm = T)
  LC1 = quantile(x, p= 0.01,na.rm = T)
  x= ifelse(x>Uc1,Uc1,x)
  x= ifelse(x<LC1,LC1,x)
  return(x)
}

test1 <- data.frame(sapply(test1,FUN = outlier_treat))
View(test1)


test2 <- data.frame(cbind(test1, amount_spent_per_room_night_scaled= predict(fit,newdata = test1)))


# #RMSE
# 
# dev_rmse <- sqrt(mean((dev1$Interest_Rate-dev1$Pred_int_rate)**2))
# val_rmse <- sqrt(mean((val1$Interest_Rate-val1$pred_int_rate)**2))
# 
# print(dev_rmse)
# print(val_rmse)


str(test)
test <- read.csv("F:/hackathon/Club Mahindra/test.csv")
reservation_id <- as.data.frame(test$reservation_id)
Spend_pred <- as.data.frame(cbind(reservation_id,test2$amount_spent_per_room_night_scaled))
names(Spend_pred) <- c("reservation_id","amount_spent_per_room_night_scaled")
write.csv(Spend_pred, "Spend_pred.csv",row.names = F)
