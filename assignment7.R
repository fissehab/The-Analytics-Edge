setwd("C:/Fish/classes/summer_2015/Analytic_edge/assignments/assignment7")

library(ggplot2)
library(maps)
library(ggmap)
statesMap = map_data("state")
str(statesMap)
unique(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = statesMap$group, color = "black") 
polling = read.csv("PollingImputed.csv")
# Subset data into training set and test set
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

predictionMap = predictionMap[order(predictionMap$order),]
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, 
    group = group, fill = TestPredictionBinary))+ 
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "blue", high = "red", 
    guide = "legend", breaks= c(0,1), 
    labels = c("Democrat", "Republican"), 
    name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat,
    group = group, fill = TestPrediction,name='Fish'))+ 
    geom_polygon(color = "black",lty=3)+
    scale_fill_continuous(name="Probability of\nRepublican")


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",lwd=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


ggplot(predictionMap, aes(x = long, y = lat, group = group,
                          
fill = TestPrediction))+ geom_polygon(color = "black",lty=3) + scale_fill_gradient(low = "orange", high = "yellow")



###page 2


eadges<-read.csv("edges.csv")
users<-read.csv('users.csv') 
