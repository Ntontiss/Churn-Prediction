# Clear workspace ----
rm(list = ls())


library(readr)
data<- read_delim("Library/Mobile Documents/com~apple~CloudDocs/Master MADS/Data Science/ASSIGNMENT 1/R CODE ass1/DataTitanic.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Install and unpack packages ----
install.packages("psych")
install.packages("ROCR")
install.packages("randomForest")
install.packages("gbm")
install.packages("partykit")
install.packages("PerformanceAnalytics")
install.packages("party")
library(strucchange)
library(party)
library(PerformanceAnalytics)
library(rpart)
library(partykit)
library(gbm)
library(randomForest)
library(ROCR)
library(psych)
library(stats)
library(ggplot2)
library(psych)
library(corrplot)
library(dplyr) 
library(e1071)
library(MASS) 


# First look at the data ----
summary(data)
describe(data)
aggregate(data$Age, by=list(data$Start_channel),FUN=mean)

# Look at type of variables and change if needed ----
class(data$Customer_ID)
class(data$Gender)
class(data$Age)
class(data$Income)
class(data$Relation_length)
class(data$Contract_length)
class(data$Start_channel)
class(data$Home_age)
class(data$Home_label)
class(data$Electr)
class(data$Customer_ID)
class(data$Customer_ID)
class(data$Customer_ID)


# Change Home label into levels ----
# Good energy efficiency being 1 to worst efficiency 7
data$Home_label[data$Home_label== "A"]<- 1
data$Home_label[data$Home_label== "B"]<- 2
data$Home_label[data$Home_label== "C"]<- 3
data$Home_label[data$Home_label== "D"]<- 4
data$Home_label[data$Home_label== "E"]<- 5
data$Home_label[data$Home_label== "F"]<- 6
data$Home_label[data$Home_label== "G"]<- 7

# Change variable type ----
class(data$Home_label)
data$Home_label <- as.integer(data$Home_label)

# Add an age group variable ----
data$Age_group <- ifelse(data$Age <= 30,1,
                         ifelse(data$Age >= 31 & data$Age <= 50,2,
                                ifelse(data$Age >= 51 & data$Age < 67,3,
                                       ifelse(data$Age >= 67,4,NA))))

# Change Start channel into dummy ----
data$Start_channel <- ifelse(data$Start_channel == "Online",1,0)

# New variable for electricity and gas usage ----
## Deal with outliers first ----
### Electricity usage ----
sum <- summary(data$Electricity_usage)
sum
iqr <- sum[5] - sum[2]
iqr
llimit <- sum[2] - 1.5*iqr
llimit
ulimit <- sum[5] + 1.5*iqr
ulimit

### Truncate the outliers for electricity down to the highest electricity usage which is not an outlier ----

data$Electricity_usage <- ifelse(data$Electricity_usage > ulimit, ulimit,data$Electricity_usage)
data$Electricity_usage <- ifelse(data$Electricity_usage < llimit, llimit,data$Electricity_usage)
summary(data$Electricity_usage)


### Gas usage ----
sum2 <- summary(data$Gas_usage)
sum2
iqr2 <- sum2[5] - sum2[2]
iqr2
llimit2 <- sum2[2] - 1.5*iqr2
llimit2
ulimit2 <- sum2[5] + 1.5*iqr2
ulimit2

### Truncate the outliers for gas down to the highest gas usage which is not an outlier ----
summary(data$Gas_usage)
data$Gas_usage <- ifelse(data$Gas_usage > ulimit2, ulimit2,data$Gas_usage)
data$Gas_usage <- ifelse(data$Gas_usage < llimit2, llimit2,data$Gas_usage)
summary(data$Gas_usage)

## Standardize electricity usage and gas usage ----
data$Electricity_usage <- scale(data$Electricity_usage)
summary(data$Electricity_usage)
data$Gas_usage <- scale(data$Gas_usage)
summary(data$Gas_usage)

## New variable using mean of electricity and gas ----
data$electr_gas_usage <- (data$Electricity_usage + data$Gas_usage)/2
summary(data$electr_gas_usage)

## Look at simple regression of the new variable ----
summary(lm(Churn ~ electr_gas_usage,data))

aggregate(data$Churn, by=list(data$Age_group), FUN=mean)

summ <- summary(data$Income)
summ

data$Income_group <- ifelse(data$Income <= summ[2],1,
                            ifelse(data$Income > summ[2] & data$Income <= summ[3],2,
                                   ifelse(data$Income > summ[3] & data$Income <= summ[5],3,
                                          ifelse(data$Income > summ[5],4,NA))))

aggregate(data$Churn, by=list(data$Income_group), FUN=mean)

ggplot(data, aes(Age, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Age_group, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Income, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Income_group, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Relation_length, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Contract_length, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Start_channel, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Email_list, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Home_age, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Home_label, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Electricity_usage, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(Gas_usage, Churn)) +  geom_point() +  geom_smooth(method = lm)
ggplot(data, aes(electr_gas_usage, Churn)) +  geom_point() +  geom_smooth(method = lm)
summary(data$electr_gas_usage)

ggplot(data, aes(Income, Churn)) +  geom_point() +  geom_smooth(method = lm)


t1 <- aggregate(data$Churn, by=list(data$Age), FUN=mean)
t1
ggplot(t1, aes(x=Group.1, y=x)) + geom_line() + xlab("Age") + ylab("Churn")


aggregate(data$Churn, by=list(data$Start_channel), FUN=mean)

aggregate(data$Churn, by=list(data$Email_list), FUN=mean)

aggregate(data$Churn, by=list(data$Home_label), FUN=mean)

aggregate(data$Churn, by=list(data$Province), FUN=mean)

aggregate(data$Churn, by=list(data$Age_group), FUN=mean)

aggregate(data$Churn, by=list(data$Income_group), FUN=mean)


data$count <- 1

aggregate(data$count, by=list(data$Age_group), FUN=sum)

unique(data$Home_label)

#Some descriptive statistics:
describe(data)
summary(data)


summary(glm(Churn ~ Home_age, data, family="binomial"))

summary(glm(Churn ~ Electricity_usage, data, family="binomial"))

summary(glm(Churn ~ Gas_usage, data, family="binomial"))

summary(glm(Churn ~ electr_gas_usage, data, family="binomial"))

summary(glm(Churn ~ Start_channel, data, family="binomial"))

summary(glm(Churn ~ Income_group, data, family="binomial"))

summary(glm(Churn ~ Age_group, data, family="binomial"))


#Outliers for income ----
#We are not using this since we decided on using levels instead of income as a numerical
#IQR <- summary(data$Income)[5] - summary(data$Income)[2]
#IQR
#Upper bounds
#ub <- 1.5*IQR + summary(data$Income)[5]
#Lower bounds
#lb <- summary(data$Income)[2] - 1.5*IQR

#data$outlier <- ifelse(data$Income > ub,1,0)

#mean(data$outlier)
#So we will only have outliers in the upper limit

#Truncate the outliers for income down to the highest income which is not an outlier
#agg <- aggregate(data$Income, by=list(data$outlier),FUN=max)
#upperlimit <- agg[1,2]
#data$Income <- ifelse(data$Income > upperlimit, upperlimit,data$Income)
#plot(data$Customer_ID,data$Income, type="l")



aggregate(data$count, by=list(data$Income_group), FUN=sum)

#Exploratative analysis
summary(glm(Churn ~ Income, family = "binomial",data))
summary(lm(Churn ~ Income,data))

summary(lm(Gas_usage ~ Income, data))
summary(lm(Electricity_usage ~ Income, data))
summary(lm(Income ~ Age, data))
summary(lm(Relation_length ~ Income, data))
summary(glm(Churn ~ Home_label, family="binomial",data))

correl <- data[,c(3:6,9,11,12,14,16)]
correlation <- cor(correl)
corrplot(correlation, order="hclust",addCoef.col = "black")
corrplot.mixed((correlation), upper="ellipse")

class(data$Income)
class(data$Home_age)
class(data$Gas_usage)
class(data$Electricity_usage)

class(data$Income_group)
data$Income_group <- as.integer(data$Income_group)
corr=cor(data[as.numeric(which(sapply(data,class)=="integer"))])  
corr <- corr[1:11,1:11]
corr=cor(data[as.numeric(which(sapply(data,class)=="in"))])  

table(data$Start_channel)


##Start channel 
table(data$Start_channel)
mean(data$Start_channel)
wilcox.test(log(data$Churn)~as.factor(data$Start_channel))
#significant relationship 


#Make subset of the data
data2 <- data[c(7,10,14:17)]


# Logistic regression -----------------------------------------------------

#First logistic regression model
Logistic_regression1 <- glm(Churn ~ Age_group + Start_channel + Home_label + 
                              electr_gas_usage + Income_group, family=binomial, data2)
summary(Logistic_regression1)


#Get predictions from the logistic regression model
predictions_model1 <- predict(Logistic_regression1, type = "response", newdata=data2)
summary(predictions_model1)



# Fit criteria ------------------------------------------------------------


#Make the basis for the hit rate table
predicted_model1 <- ifelse(predictions_model1>.5,1,0)

hit_rate_model1 <- table(data2$Churn, predicted_model1, dnn= c("Observed", "Predicted"))

hit_rate_model1

#Get the hit rate
(hit_rate_model1[1,1]+hit_rate_model1[2,2])/sum(hit_rate_model1)


#Top decile lift
decile_predicted_model1 <- ntile(predictions_model1, 10)

decile_model1 <- table(data2$Churn, decile_predicted_model1, dnn= c("Observed", "Decile"))

decile_model1

#Calculate the TDL
(decile_model1[2,10] / (decile_model1[1,10]+ decile_model1[2,10])) / mean(data2$Churn)


#Make lift curve
pred_model1 <- prediction(predictions_model1, data2$Churn)
perf_model1 <- performance(pred_model1,"tpr","fpr")
plot(perf_model1,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model1 <- performance(pred_model1,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model1@y.values)*2-1



# Out of sample validation ------------------------------------------------

#Get a 75% estimation sample and 25% validation sample
set.seed(1234)
data2$estimation_sample <-rbinom(nrow(data), 1, 0.75)
summary(data2)

#Estimate the model using only the estimation sample
Logistic_regression2 <- glm(Churn ~ Age_group + Start_channel + Home_label + 
                              electr_gas_usage + Income_group, family=binomial, data2, subset=estimation_sample==1)


#Create a new dataframe with only the validation sample
our_validation_dataset <- data2[data2$estimation_sample==0,]

#Get predictions for all observations
predictions_model2 <- predict(Logistic_regression2, type = "response", newdata= our_validation_dataset)

### After this you can calculate the fit criteria on this validation sample

predicted_model2 <- ifelse(predictions_model2>.5,1,0)

hit_rate_model2 <- table(our_validation_dataset$Churn, predicted_model2)
hit_rate_model2

#Get the hit rate
(hit_rate_model2[1,1]+hit_rate_model2[2,2])/sum(hit_rate_model2)


# Lecture 2 ---------------------------------------------------------------

# Step wise regression ----------------------------------------------------


#Estimate full and null model
Logistic_regression_full <- glm(Churn ~ ., data2, family = binomial)
Logistic_regression_null <- glm(Churn ~ 0, data2, family = binomial)

# Fit the model backward
Logistic_regression_backward <- stepAIC(Logistic_regression_full, direction="backward", trace = T)
Logistic_regression_backward$anova
Logistic_regression_backward$coefficients

# Fit the model forward
Logistic_regression_forward <- stepAIC(Logistic_regression_null, direction="forward", scope=list(lower=Logistic_regression_null, upper=Logistic_regression_full), trace = TRUE)
Logistic_regression_forward$anova
Logistic_regression_forward$coefficients

# Fit the model both directions
Logistic_regression_both <- stepAIC(Logistic_regression_full, direction="both", trace = TRUE)
Logistic_regression_both$anova
Logistic_regression_both$coefficients

nrow(data2)

## To do step-wise regression with the BIC you can add "k = log(n)" (where n is the amount of observations on which the model is estimated) to the stepAIC function, example:

# Fit the model backward using BIC
Logistic_regression_backward_BIC <- stepAIC(Logistic_regression_full, direction="backward", trace = TRUE, k = log(nrow(data2)))

#fit criteria
###backward 
predictions_model_backward <- predict(Logistic_regression_backward, type = "response", newdata=data2)

#Make the basis for the hit rate table
predicted_model_backward <- ifelse(predictions_model_backward>.5,1,0)

hit_rate_model_backward <- table(data2$Churn, predicted_model_backward, dnn= c("Observed", "Predicted"))

hit_rate_model_backward

#Get the hit rate
(hit_rate_model_backward[1,1]+hit_rate_model_backward[2,2])/sum(hit_rate_model_backward)


#Top decile lift
decile_predicted_model_backward <- ntile(predictions_model_backward, 10)

decile_model_backward <- table(data2$Churn, decile_predicted_model_backward, dnn= c("Observed", "Decile"))

decile_model_backward

#Calculate the TDL
(decile_model_backward[2,10] / (decile_model_backward[1,10]+ decile_model_backward[2,10])) / mean(data2$Churn)

pred_model_backward <- prediction(predictions_model_backward, data2$Churn)
perf_model_backward <- performance(pred_model_backward,"tpr","fpr")
plot(perf_model_backward,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model_backward <- performance(pred_model_backward,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model_backward@y.values)*2-1


###forward 
predictions_model_forward <- predict(Logistic_regression_forward, type = "response", newdata=data2)

#Make the basis for the hit rate table
predicted_model_forward <- ifelse(predictions_model_forward>.5,1,0)

hit_rate_model_forward <- table(data2$Churn, predicted_model_forward, dnn= c("Observed", "Predicted"))

hit_rate_model_forward

#Get the hit rate
(hit_rate_model_forward[1,1]+hit_rate_model_forward[2,2])/sum(hit_rate_model_forward)

#Top decile lift
decile_predicted_model_forward<- ntile(predictions_model_forward, 10)

decile_model_forward <- table(data2$Churn, decile_predicted_model_forward, dnn= c("Observed", "Decile"))

decile_model_forward

#Calculate the TDL
(decile_model_forward[2,10] / (decile_model_forward[1,10]+ decile_model_forward[2,10])) / mean(data2$Churn)

pred_model_forward <- prediction(predictions_model_forward, data2$Churn)
perf_model_forward <- performance(pred_model_forward,"tpr","fpr")
plot(perf_model_forward,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model_forward <- performance(pred_model_backward,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model_forward@y.values)*2-1


###both
predictions_model_both <- predict(Logistic_regression_both, type = "response", newdata=data2)

#Make the basis for the hit rate table
predicted_model_both <- ifelse(predictions_model_both>.5,1,0)

hit_rate_model_both <- table(data2$Churn, predicted_model_both, dnn= c("Observed", "Predicted"))

hit_rate_model_both

#Get the hit rate
(hit_rate_model_both[1,1]+hit_rate_model_both[2,2])/sum(hit_rate_model_both)


#Top decile lift
decile_predicted_model_both <- ntile(predictions_model_both, 10)

decile_model_both<- table(data2$Churn, decile_predicted_model_both, dnn= c("Observed", "Decile"))

decile_model_both

#Calculate the TDL
(decile_model_both[2,10] / (decile_model_both[1,10]+ decile_model_both[2,10])) / mean(data2$Churn)

pred_model_both <- prediction(predictions_model_both, data2$Churn)
perf_model_both <- performance(pred_model_both,"tpr","fpr")
plot(perf_model_both,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model_both <- performance(pred_model_both,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model_both@y.values)*2-1




#Out of sample validation -----

#Estimate full and null model
Logistic_regression_full2 <- glm(Churn ~ ., data2, family = binomial, subset=estimation_sample==1)
Logistic_regression_null2 <- glm(Churn ~ 0, data2, family = binomial, subset=estimation_sample==1)

# Fit the model backward
Logistic_regression_backward2 <- stepAIC(Logistic_regression_full2, direction="backward", trace = T)
Logistic_regression_backward2$anova
Logistic_regression_backward2$coefficients

# Fit the model forward
Logistic_regression_forward2 <- stepAIC(Logistic_regression_null2, direction="forward", scope=list(lower=Logistic_regression_null2, upper=Logistic_regression_full2), trace = TRUE)
Logistic_regression_forward2$anova
Logistic_regression_forward2$coefficients

# Fit the model both directions
Logistic_regression_both2 <- stepAIC(Logistic_regression_full2, direction="both", trace = TRUE)
Logistic_regression_both2$anova
Logistic_regression_both2$coefficients



## To do step-wise regression with the BIC you can add "k = log(n)" (where n is the amount of observations on which the model is estimated) to the stepAIC function, example:

# Fit the model backward using BIC
Logistic_regression_backward_BIC2 <- stepAIC(Logistic_regression_full2, direction="backward", trace = TRUE, k = log(sum(data2$estimation_sample)))

#fit criteria
###backward 
predictions_model_backward2 <- predict(Logistic_regression_backward2, type = "response", newdata=our_validation_dataset)
#-----
#Make the basis for the hit rate table
predicted_model_backward2 <- ifelse(predictions_model_backward2>.5,1,0)

hit_rate_model_backward2 <- table(our_validation_dataset$Churn, predicted_model_backward2, dnn= c("Observed", "Predicted"))

hit_rate_model_backward2

#Get the hit rate
(hit_rate_model_backward2[1,1]+hit_rate_model_backward2[2,2])/sum(hit_rate_model_backward2)


#Top decile lift
decile_predicted_model_backward2 <- ntile(predictions_model_backward2, 10)

decile_model_backward2 <- table(our_validation_dataset$Churn, decile_predicted_model_backward2, dnn= c("Observed", "Decile"))

decile_model_backward2

#Calculate the TDL
(decile_model_backward2[2,10] / (decile_model_backward2[1,10]+ decile_model_backward2[2,10])) / mean(our_validation_dataset$Churn)

pred_model_backward2 <- prediction(predictions_model_backward2, our_validation_dataset$Churn)
perf_model_backward2 <- performance(pred_model_backward2,"tpr","fpr")
plot(perf_model_backward2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model_backward2 <- performance(pred_model_backward2,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model_backward2@y.values)*2-1


###forward 
predictions_model_forward2 <- predict(Logistic_regression_forward2, type = "response", newdata=our_validation_dataset)

#Make the basis for the hit rate table
predicted_model_forward2 <- ifelse(predictions_model_forward2>.5,1,0)

hit_rate_model_forward2 <- table(our_validation_dataset$Churn, predicted_model_forward2, dnn= c("Observed", "Predicted"))

hit_rate_model_forward2

#Get the hit rate
(hit_rate_model_forward2[1,1]+hit_rate_model_forward2[2,2])/sum(hit_rate_model_forward2)

#Top decile lift
decile_predicted_model_forward2 <- ntile(predictions_model_forward2, 10)

decile_model_forward2 <- table(our_validation_dataset$Churn, decile_predicted_model_forward2, dnn= c("Observed", "Decile"))

decile_model_forward2

#Calculate the TDL
(decile_model_forward2[2,10] / (decile_model_forward2[1,10]+ decile_model_forward2[2,10])) / mean(our_validation_dataset$Churn)

pred_model_forward2 <- prediction(predictions_model_forward2, our_validation_dataset$Churn)
perf_model_forward2 <- performance(pred_model_forward2,"tpr","fpr")
plot(perf_model_forward2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model_forward2 <- performance(pred_model_backward2,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model_forward2@y.values)*2-1


###both
predictions_model_both2 <- predict(Logistic_regression_both2, type = "response", newdata=our_validation_dataset)

#Make the basis for the hit rate table
predicted_model_both2 <- ifelse(predictions_model_both2>.5,1,0)

hit_rate_model_both2 <- table(our_validation_dataset$Churn, predicted_model_both2, dnn= c("Observed", "Predicted"))

hit_rate_model_both2

#Get the hit rate
(hit_rate_model_both2[1,1]+hit_rate_model_both2[2,2])/sum(hit_rate_model_both2)


#Top decile lift
decile_predicted_model_both2 <- ntile(predictions_model_both2, 10)

decile_model_both2 <- table(our_validation_dataset$Churn, decile_predicted_model_both2, dnn= c("Observed", "Decile"))

decile_model_both2

#Calculate the TDL
(decile_model_both2[2,10] / (decile_model_both2[1,10]+ decile_model_both2[2,10])) / mean(our_validation_dataset$Churn)

pred_model_both2 <- prediction(predictions_model_both2, our_validation_dataset$Churn)
perf_model_both2 <- performance(pred_model_both2,"tpr","fpr")
plot(perf_model_both2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model_both2 <- performance(pred_model_both2,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model_both2@y.values)*2-1


# Estimate a CART tree ----------------------------------------------------


Cart_tree3 <- rpart(Churn ~ Age_group + Start_channel + Home_label + 
                      electr_gas_usage + Income_group, data2, method="class")

Cart_tree3_visual <- as.party(Cart_tree3)
plot(Cart_tree3_visual , type="simple", gp = gpar(fontsize = 10))


# Changing settings
newsettings1 <- rpart.control(minsplit = 100, minbucket = 50, cp = 0.01, maxdepth = 3)

Cart_tree4 <- rpart(Churn ~ Age_group + Start_channel + Home_label + 
                      electr_gas_usage + Income_group, data2, method="class", control=newsettings1)
Cart_tree4_visual <- as.party(Cart_tree4)
plot(Cart_tree4_visual , type="simple")


#Save predictions
predictions_cart3 <- predict(Cart_tree3, newdata=data2, type ="prob")

# For the sctest function to extract p-values (see help for ctree and sctest)

Cart_Tree5 <- ctree(Churn~Age_group + Income_group + Start_channel+Home_label+electr_gas_usage, data=data2)
plot(Cart_Tree5 , type="simple", gp = gpar(fontsize = 10))

#Get predictions from the logistic regression model
predictions_Tree5 <- predict(Cart_Tree5, type = "response", data=data2)


#Make the basis for the hit rate table
predicted_Tree5  <- ifelse(predictions_Tree5>.5,1,0)

Tree_hit_rate_model5 <- table(data2$Churn, predicted_Tree5)

Tree_hit_rate_model5 

#Get the hit rate
(Tree_hit_rate_model5 [1,1]+Tree_hit_rate_model5 [2,2])/sum(Tree_hit_rate_model5 )

#Top decile lift

decile_predicted_Tree5 <- ntile(predictions_Tree5, 10)

decile_Tree_model5 <- table(data2$Churn, decile_predicted_Tree5, dnn= c("Observed", "Decile"))

decile_Tree_model5

#Calculate the TDL
(decile_Tree_model5 [2,10] / (decile_Tree_model5 [1,10]+ decile_Tree_model5 [2,10])) / mean(data2$Churn)

#Make lift curve

pred_Tree_model5 <- prediction(predictions_Tree5 , data2$Churn)
perf_Tree_model5 <- performance(pred_Tree_model5 ,"tpr","fpr")
plot(perf_Tree_model5 ,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_Tree_model5 <- performance(pred_Tree_model5,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_Tree_model5@y.values)*2-1



# Validation ----
# Tree model

Cart_tree1 <- rpart(Churn ~ Age_group + Start_channel + Home_label + 
                      electr_gas_usage + Income_group, data2, method="class", subset=estimation_sample==1)

Cart_tree1_visual <- as.party(Cart_tree1)
plot(Cart_tree1_visual , type="simple", gp = gpar(fontsize = 10))


# Changing settings
newsettings1 <- rpart.control(minsplit = 100, minbucket = 50, cp = 0.01, maxdepth = 3)

Cart_tree2 <- rpart(Churn ~ Age_group + Start_channel + Home_label + 
                      electr_gas_usage + Income_group, data2, method="class", subset=estimation_sample==1, control=newsettings1)
Cart_tree2_visual <- as.party(Cart_tree2)
plot(Cart_tree2_visual , type="simple")


#Save predictions
predictions_cart1 <- predict(Cart_tree1, newdata=our_validation_dataset, type ="prob")
# -----------
# For the sctest function to extract p-values (see help for ctree and sctest)


Cart_Tree <- ctree(Churn~Age_group + Income_group + Start_channel+Home_label+electr_gas_usage, data=our_validation_dataset)
plot(Cart_Tree , type="simple", gp = gpar(fontsize = 10))

#Get predictions from the logistic regression model
predictions_Tree <- predict(Cart_Tree, type = "response", data=our_validation_dataset)


#Make the basis for the hit rate table
predicted_Tree  <- ifelse(predictions_Tree>.5,1,0)

Tree_hit_rate_model <- table(our_validation_dataset$Churn, predicted_Tree)

Tree_hit_rate_model 

#Get the hit rate
(Tree_hit_rate_model [1,1]+Tree_hit_rate_model [2,2])/sum(Tree_hit_rate_model )

#Top decile lift

decile_predicted_Tree <- ntile(predictions_Tree, 10)

decile_Tree_model2 <- table(our_validation_dataset$Churn, decile_predicted_Tree, dnn= c("Observed", "Decile"))

decile_Tree_model2

#Calculate the TDL
(decile_Tree_model2 [2,10] / (decile_Tree_model2 [1,10]+ decile_Tree_model2 [2,10])) / mean(our_validation_dataset$Churn)

#Make lift curve

pred_Tree_model2 <- prediction(predictions_Tree , our_validation_dataset$Churn)
perf_Tree_model2 <- performance(pred_Tree_model2 ,"tpr","fpr")
plot(perf_Tree_model2 ,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_Tree_model2 <- performance(pred_Tree_model2,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_Tree_model2@y.values)*2-1


# Random forest -----------------------------------------------------------
library(randomForest)

Random_forest1 <- randomForest(Churn ~ ., data=data2, importance=TRUE)

predictions_forest1 <- predict(Random_forest1, newdata=data2, type ="prob")[,2]

varImpPlot(Random_forest1)

knitr::kable(importance(Random_forest1))

#Some extra setting you can play around with
Random_forest1 <- randomForest(Churn ~ ., data=data2,
                               ntree=500, mtry=3, nodesize=1, maxnodes=100, importance=TRUE)


##Hit rate

#Get predictions for all observations
forest_predictions1 <- predict(Random_forest1, type = "response", newdata=data2)

### After this you can calculate the fit criteria on this validation sample

#Make the basis for the hit rate table
predicted_forest1  <- ifelse(forest_predictions1>.5,1,0)

Forest_hit_rate_model <- table(data2$Churn, predicted_forest1)

Forest_hit_rate_model

#Get the hit rate
(Forest_hit_rate_model[1,1]+Forest_hit_rate_model[2,2])/sum(Forest_hit_rate_model)



##Top decile lift

decile_predicted_forest2 <- ntile(forest_predictions1, 10)

decile_forest_model2 <- table(data2$Churn, decile_predicted_forest2, dnn= c("Observed", "Decile"))

decile_forest_model2 

#Calculate the TDL
(decile_forest_model2 [2,10] / (decile_forest_model2 [1,10]+ decile_forest_model2 [2,10])) / mean(data2$Churn)

#Make lift curve

pred_forest_model2 <- prediction(forest_predictions1 , data2$Churn)
perf_forest_model2 <- performance(pred_forest_model2,"tpr","fpr")
plot(perf_forest_model2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_forest_model2 <- performance(pred_forest_model2,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_forest_model2@y.values)*2-1


# Out of sample validation ----
Random_forest2 <- randomForest(Churn ~ ., data=data2, subset=estimation_sample==1, importance=TRUE)

predictions_forest2 <- predict(Random_forest2, newdata=our_validation_dataset, type ="prob")[,2]

varImpPlot(Random_forest2)

knitr::kable(importance(Random_forest2))

#Some extra setting you can play around with
Random_forest2 <- randomForest(Churn ~ ., data=data2, subset=estimation_sample==1,
                               ntree=500, mtry=3, nodesize=1, maxnodes=100, importance=TRUE)


##Hit rate

#Get predictions for all observations
forest_predictions2 <- predict(Random_forest2, type = "response", newdata=our_validation_dataset)

### After this you can calculate the fit criteria on this validation sample

#Make the basis for the hit rate table
predicted_forest2  <- ifelse(forest_predictions2>.5,1,0)

Forest_hit_rate_model2 <- table(our_validation_dataset$Churn, predicted_forest2)

Forest_hit_rate_model2

#Get the hit rate
(Forest_hit_rate_model2[1,1]+Forest_hit_rate_model2[2,2])/sum(Forest_hit_rate_model2)



##Top decile lift

decile_predicted_forest3 <- ntile(forest_predictions2, 10)

decile_forest_model3 <- table(our_validation_dataset$Churn, decile_predicted_forest3, dnn= c("Observed", "Decile"))

decile_forest_model3 

#Calculate the TDL
(decile_forest_model3[2,10] / (decile_forest_model3[1,10]+ decile_forest_model3[2,10])) / mean(our_validation_dataset$Churn)

#Make lift curve

pred_forest_model3 <- prediction(forest_predictions2 , our_validation_dataset$Churn)
perf_forest_model3 <- performance(pred_forest_model3,"tpr","fpr")
plot(perf_forest_model3,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_forest_model3 <- performance(pred_forest_model3,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_forest_model3@y.values)*2-1



# Support Vector Machine --------------------------------------------------
library(e1071)
data3 <- data2[data2$estimation_sample == 1,]

svm_1 <- svm(Churn ~ Age_group + Start_channel + Home_label + 
               electr_gas_usage + Income_group, data2,
             type = 'C-classification', probability = TRUE,
             kernel = 'linear')

plot(svm_1, data2, Age_group~Start_channel)
plot(svm_1, data2, Age_group~Home_label)
plot(svm_1, data2, Age_group~electr_gas_usage)
plot(svm_1, data2, Age_group~Income_group)
plot(svm_1, data2, Start_channel~Home_label)
plot(svm_1, data2, Start_channel~electr_gas_usage)
plot(svm_1, data2, Start_channel~Income_group)
plot(svm_1, data2, Home_label~electr_gas_usage)
plot(svm_1, data2, Home_label~Income_group)
plot(svm_1, data2, electr_gas_usage~Income_group)

#Get predictions
predictions_svm1 <- predict(svm_1, newdata=data2, probability=TRUE)
predictions_svm1 <- attr(predictions_svm1,"probabilities")[,1]


summary(predictions_svm1)
predicted_model3 <- ifelse(predictions_svm1>.5,1,0)

hit_rate_model3 <- table(data2$Churn, predicted_model3)
hit_rate_model3

#Get the hit rate
(hit_rate_model3[1,1]+hit_rate_model3[2,2])/sum(hit_rate_model3)

#Top decile lift

decile_predicted_modelsvm <- ntile(predictions_svm1, 10)

decile_modelsvm <- table(data2$Churn, decile_predicted_modelsvm, dnn= c("Observed", "Decile"))

decile_modelsvm

#Calculate the TDL
(decile_modelsvm[2,10] / (decile_modelsvm[1,10]+ decile_modelsvm[2,10])) / mean(data2$Churn)


#Make lift curve


pred_modelsvm <- prediction(predictions_svm1, data2$Churn)
perf_modelsvm <- performance(pred_modelsvm,"tpr","fpr")
plot(perf_modelsvm,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_modelsvm <- performance(pred_modelsvm,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_modelsvm@y.values)*2-1



# Out of sample validation ----
svm_2 <- svm(Churn ~ Age_group + Start_channel + Home_label + 
               electr_gas_usage + Income_group, data2, subset=data2$estimation_sample==1,
             type = 'C-classification', probability = TRUE,
             kernel = 'linear')

#Get predictions
predictions_svm2 <- predict(svm_2, newdata=our_validation_dataset, probability=TRUE)
predictions_svm2 <- attr(predictions_svm2,"probabilities")[,1]


summary(predictions_svm2)
predicted_model4 <- ifelse(predictions_svm2>.5,1,0)

hit_rate_model4 <- table(our_validation_dataset$Churn, predicted_model4)
hit_rate_model4

#Get the hit rate
(hit_rate_model4[1,1]+hit_rate_model4[2,2])/sum(hit_rate_model4)

#Top decile lift

decile_predicted_modelsvm2 <- ntile(predictions_svm2, 10)

decile_modelsvm2 <- table(our_validation_dataset$Churn, decile_predicted_modelsvm2, dnn= c("Observed", "Decile"))

decile_modelsvm2

#Calculate the TDL
(decile_modelsvm2[2,10] / (decile_modelsvm2[1,10]+ decile_modelsvm2[2,10])) / mean(our_validation_dataset$Churn)


#Make lift curve


pred_modelsvm2 <- prediction(predictions_svm2, our_validation_dataset$Churn)
perf_modelsvm2 <- performance(pred_modelsvm2,"tpr","fpr")
plot(perf_modelsvm2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_modelsvm2 <- performance(pred_modelsvm2,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_modelsvm2@y.values)*2-1
