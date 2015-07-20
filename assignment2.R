setwd("C:/Fish/classes/summer_2015/Analytic_edge/assignments/assignment2")

clim<-read.csv("climate_change.csv")
train<-subset(clim,clim$Year<=2006)
test<-subset(clim,clim$Year> 2006)

##

training=train
testing=test

modelFit <- train(Temp ~., data=training, method="glm",preProcess=c("center", "scale"))
predictions <- predict(modelFit,newdata=testing)

pcp_train=data.frame('fitted'=modelFit$finalModel$fitted.values,'observed'=training$Temp)

pcp_test=data.frame('predicted'=predictions,'observed'=testing$Temp)

##

model1<-lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=train)

summary(model1)

cor(train)

model2<-lm(Temp~MEI+TSI+Aerosols+N2O, data=train)

summary(model2)

model3<-step(model1)

summary(model3)

predictions<-predict(model3, newdata=test)

SSE=sum((test$Temp-predictions)^2)

SST = sum((test$Temp-mean(train$Temp))^2)# remember to use training data here

R_square=1-SSE/SST



##### Page 2

pisaTrain<-read.csv("pisa2009train.csv")
pisaTest<-read.csv("pisa2009test.csv")

tapply(pisaTrain$readingScore,pisaTrain$male,mean)


pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore<-lm(readingScore ~ ., data=pisaTrain)

predTest<-predict(lmScore,newdata=pisaTest)

SSE<-sum((pisaTest$readingScore-predTest)^2)

RMSE=sqrt(SSE/length(predTest))

base<-mean(pisaTrain$readingScore)

SST<-sum((pisaTest$readingScore-base)^2)
R_square<-1-SSE/SST

################ Page 3
FluTrain<-read.csv("FluTrain.csv")
names(FluTrain)

FluTrain[which(FluTrain$ILI==max(FluTrain$ILI,na.rm=T)),]

FluTrain[which(FluTrain$Queries==max(FluTrain$Queries,na.rm=T)),]

hist(FluTrain$ILI)

plot(log(FluTrain$ILI), FluTrain$Queries)

fit1<-lm(log(ILI)~Queries,data=FluTrain)
summary(fit1)


##

Flutest<-read.csv("FluTest.csv")


PredTest1 = exp(predict(fit1, newdata=Flutest))
PredTest1[11]
sqrt(mean((PredTest1-Flutest$ILI)^2))
     

library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

plot(log(ILILag2), log(FluTrain$ILI))


FluTrend2<-lm(log(ILI)~Queries+log(ILILag2),data=FluTrain)






ILILag2 = lag(zoo(Flutest$ILI), -2, na.pad=TRUE)

Flutest$ILILag2 = coredata(ILILag2)

Flutest$ILILag2[1] = FluTrain$ILI[416]

Flutest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=Flutest))
sqrt(mean((PredTest2-Flutest$ILI)^2))

