setwd("C:/Fish/classes/summer_2015/Analytic_edge/assignments/assignment4")


rber<-read.csv("gerber.csv")
table(gerber$voting)/length(gerber$voting)


names=names(gerber)[4:7]


table(gerber$self,gerber$voting)[2,2]/sum(gerber$self)
table(gerber$hawthorne,gerber$voting)[2,2]/sum(gerber$hawthorne)
table(gerber$civicduty,gerber$voting)[2,2]/sum(gerber$civicduty)
table(gerber$neighbors,gerber$voting)[2,2]/sum(gerber$neighbors)


logit1<-glm(voting~civicduty+hawthorne+self+neighbors,
            data=gerber,family=binomial)
summary(logit1)

prob<-predict(logit1,type="response")
confusion=table(prob>0.3,gerber$voting)
accuracy=(confusion[1,1]+confusion[2,2])/(sum(confusion))


# Install and load ROCR package
if (!require(ROCR)){
    install.packages("ROCR")
}

library(ROCR)

# Prediction function
ROCRpred = prediction(prob, gerber$voting)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)



# Install and load rpart package
if (!require(rpart)){
    install.packages("rpart")
}

library(rpart)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)


library(rpart.plot)
prp(CARTmodel)


CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ sex+civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

prp(CARTmodel3)



CARTmodelC = rpart(voting ~control , data=gerber, cp=0.0)

prp(CARTmodelC, digits = 6)


CARTmodelCS = rpart(voting ~control+sex , data=gerber, cp=0.0)

prp(CARTmodelCS,digits = 6)

logit1<-glm(voting~control+sex, data=gerber,family='binomial')
summary(logit1)


Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logit1, newdata=Possibilities, type="response")


LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")

summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")



## page 2

letters<-read.csv("letters_ABPR.csv")

letters$isB=as.factor(letters$letter=='B')
set.seed(100)
library(caTools)
split<-sample.split(letters$isB,SplitRatio = 0.5)

train<-subset(letters, split==TRUE)

test<-subset(letters, split==FALSE)

CARTb = rpart(isB ~ . - letter, data=train, method="class")
prediction<-predict(CARTb, type="class",newdata=test)

table<-table(prediction,test$isB)

accuracy<-(table[1,1]+table[2,2])/(sum(table))


library(randomForest)
rfb = randomForest(isB ~ . - letter, data=train)

prediction<-predict(rfb, type="class",newdata=test)

table<-table(prediction,test$isB)

accuracy<-(table[1,1]+table[2,2])/(sum(table))

letters$letter = as.factor( letters$letter ) 



set.seed(2000)

split<-sample.split(letters$letter,SplitRatio = 0.5)

train<-subset(letters, split==TRUE)

test<-subset(letters, split==FALSE)

max(table(test$letter))/sum(table(test$letter))

CARTl = rpart(letter ~ . - isB, data=train, method="class")
prediction<-predict(CARTl, type="class",newdata=test)

table<-table(prediction,test$letter)
accuracy<-sum(diag(table))/(sum(table))
accuracy

set.seed(1000)

rf = randomForest(letter ~ . - isB, data=train, method="class")
prediction<-predict(rf, type="class",newdata=test)

table<-table(prediction,test$letter)
accuracy<-sum(diag(table))/(sum(table))
accuracy

##
##  Page 3
##

census<-read.csv("census.csv")

set.seed(2000)

split<-sample.split(census$over50k,SplitRatio = 0.6)

train<-subset(census, split==TRUE)

test<-subset(census, split==FALSE)
logit1<-glm(over50k~., data=train, family='binomial')
summary(logit1)



prediction<-predict(logit1, type="response",newdata=test)

table<-table(prediction>0.5,test$over50k)
accuracy<-sum(diag(table))/(sum(table))
accuracy


table(test$over50k)[1]/(sum(table(test$over50k)))




# Prediction function
ROCRpred = prediction(prediction, test$over50k)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)



CARTmodel = rpart(over50k ~. , data=train, method="class")

prp(CARTmodel, digits = 6)


prediction<-predict(CARTmodel, type="class",newdata=test)

table<-table(prediction,test$over50k)
accuracy<-sum(diag(table))/(sum(table))
accuracy


table(test$over50k)[1]/(sum(table(test$over50k)))


prediction<-predict(CARTmodel,newdata=test)[ , 2]

# Prediction function
ROCRpred = prediction(prediction, test$over50k)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)


#RF
set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]

rfb = randomForest(over50k~. -nativecountry, data=trainSmall)

prediction<-predict(rfb,newdata=test)

table<-table(prediction,test$over50k)

accuracy<-(sum(diag(table)))/(sum(table))


vu = varUsed(rfb, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(rfb$forest$xlevels[vusorted$ix]))


varImpPlot(rfb)


set.seed(2)

# Load libraries for cross-validation
library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# Cross-validation
tr = train(over50k ~., data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)

# Extract tree
library(rpart.plot)

best.tree = tr$finalModel
prp(best.tree)

CARTmodel = rpart(over50k ~. , data=train, cp=0.002)

prp(CARTmodel, digits = 6)

prediction<-predict(CARTmodel, type="class",newdata=test)

table<-table(prediction,test$over50k)
accuracy<-sum(diag(table))/(sum(table))
accuracy


# Make predictions
best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
