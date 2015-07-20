setwd("C:/Fish/classes/summer_2015/Analytic_edge/data")

loans<-read.csv("loans.csv")
table(loans$not.fully.paid)

table(loans$not.fully.paid)[2]/sum(table(loans$not.fully.paid))

library(mice)
library(caTools)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

###
loans<-read.csv("loans_imputed.csv")

set.seed(144)
split=sample.split(loans$not.fully.paid,SplitRatio = 0.7)

train<-loans[split==TRUE, ]
test<-loans[split==FALSE, ]

mod1<-glm(not.fully.paid~., data=train, family=binomial)
mod1$coefficients['fico']
predicted.risk=predict(mod1,newdata=test, type="response")
test$predicted.risk=predicted.risk



# load ROCR package

library(ROCR)

# Prediction function
ROCRpred = prediction(test$predicted.risk,test$not.fully.paid)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# AUC
as.numeric(performance(ROCRpred, "auc")@y.values)

mod2<-glm(not.fully.paid~int.rate, data=train, family=binomial)
pred2<-predict(mod2,newdata=test,type="response")
ROCRpred = prediction(pred2,test$not.fully.paid)

test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1

highInterest<-subset(test,test$int.rate>=0.15)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

invest<-subset(highInterest,highInterest$predicted.risk<=cutoff)
