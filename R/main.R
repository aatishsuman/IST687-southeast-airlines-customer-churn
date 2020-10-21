install.packages("jsonlite")
library(jsonlite)
install.packages("rapportools")
library(rapportools)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("tm")
library(tm)
install.packages("fastDummies")
library(fastDummies)
install.packages("caret")
library(caret)
install.packages("kernlab")
library(kernlab)
install.packages("e1071")
library(e1071)
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("MLmetrics")
library(MLmetrics)

setwd("C:/Data/study/IST-687/Project/ist687") # set this to the location where the json file is stored
source("R/munging.R") # set this to the location of the munging.R file

####### association rules mining on all data #######

df = jsonlite::fromJSON("fall2019-survey-M02.json")
dfBinnedData = getBinnedData(df)
dfTnx = as(dfBinnedData, "transactions")

##### association rules #####
rulesetPromoters <- apriori(dfTnx,
                            parameter=list(support=0.005,confidence=0.5),
                            appearance = list(default="lhs", rhs=("Likelihood.to.recommend=Promoter")))

rulesetDetractors <- apriori(dfTnx,
                             parameter=list(support=0.05,confidence=0.8),
                             appearance = list(default="lhs", rhs=("Likelihood.to.recommend=Detractor")))

summary(quality(rulesetPromoters)$lift)
summary(quality(rulesetDetractors)$lift)
length(rulesetPromoters[quality(rulesetPromoters)$lift > 2.35])
length(rulesetDetractors[quality(rulesetDetractors)$lift > 1.99])
arules::inspect(rulesetPromoters[quality(rulesetPromoters)$lift > 2.35])
arules::inspect(rulesetDetractors[quality(rulesetDetractors)$lift > 1.99])

inspectDT(rulesetPromoters[quality(rulesetPromoters)$lift > 2.35])
inspectDT(rulesetDetractors[quality(rulesetDetractors)$lift > 1.99])

# The columns with strong association were considered for classification modeling

##### classification models for all data #####
analysisColumns = c("Airline.Status", "Type.of.Travel", "Eating.and.Drinking.at.Airport", "Departure.Delay.in.Minutes", "Flights.Per.Year", "Price.Sensitivity", "olong", "dlat", "Total.Freq.Flyer.Accts")
analysisData = dfBinnedData[, names(dfBinnedData) %in% c(analysisColumns, "Likelihood.to.recommend") ]
analysisData$Likelihood.to.recommend[is.na(analysisData$Likelihood.to.recommend)] = "Passive"

analysisData = prepareForAnalysis(analysisData, analysisColumns)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(1)
inTraining = createDataPartition(analysisData$Likelihood.to.recommend, p = 0.75, list = FALSE)
trainData = analysisData[inTraining,]
testData = analysisData[-inTraining,]

logitBoost <- train(factor(Likelihood.to.recommend) ~., data = trainData, method = "LogitBoost", trControl=fitControl, preProcess = c("center", "scale"), tuneLength = 10)
logitBoost
# train accuracy - 79.55%

plot(logitBoost)

result = predict(logitBoost, testData)
sum(result == testData$Likelihood.to.recommend)/length(testData$Likelihood.to.recommend)
# test accuracy - 80.48%

F1_Score(testData$Likelihood.to.recommend, result)
# F1 score - 87.67%

varImp(logitBoost)

personalTravelResult = predict(logitBoost, testData[testData$`Type.of.Travel_Personal Travel` == 1,])
sum(personalTravelResult == "Detractor")/nrow(testData[testData$`Type.of.Travel_Personal Travel` == 1,])
# Inference - Personal travel customers have a strong relationship with their ratings.


###### association rules mining on Personal Travel data #######

personalTravel = df[str_trim(df$Type.of.Travel) == "Personal Travel", names(df) != "Type.of.Travel"]
personalTravelBinned = getBinnedData(personalTravel)
personalTravelTnx = as(personalTravelBinned, "transactions")

rulesetPromoters <- apriori(personalTravelTnx,
                   parameter=list(support=0.005,confidence=0.5),
                   appearance = list(default="lhs", rhs=("Likelihood.to.recommend=Promoter")))

rulesetDetractors <- apriori(personalTravelTnx,
                            parameter=list(support=0.05,confidence=0.8),
                            appearance = list(default="lhs", rhs=("Likelihood.to.recommend=Detractor")))

summary(quality(rulesetPromoters)$lift)
summary(quality(rulesetDetractors)$lift)
length(rulesetDetractors[quality(rulesetDetractors)$lift > 1.178])
arules::inspect(rulesetPromoters)
arules::inspect(rulesetDetractors[quality(rulesetDetractors)$lift > 1.178])

inspectDT(rulesetPromoters)
inspectDT(rulesetDetractors[quality(rulesetDetractors)$lift > 1.178])
# The columns with strong association were considered for classification modeling.

##### classification models for Personal Travel data #####
analysisColumns3 = colnames(personalTravelBinned)[colnames(personalTravelBinned) != "Likelihood.to.recommend"]

analysisData3 = personalTravelBinned[, names(personalTravelBinned) %in% c(analysisColumns3, "Likelihood.to.recommend") ]

analysisData3 = prepareForAnalysis(analysisData3, analysisColumns3)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(1)
inTraining3 = createDataPartition(analysisData3$Likelihood.to.recommend, p = .75, list = FALSE)
trainData3 = analysisData3[inTraining3,]
testData3 = analysisData3[-inTraining3,]

logitBoost3 <- train(factor(Likelihood.to.recommend) ~., data = trainData3, method = "LogitBoost", trControl=fitControl, preProcess = c("center", "scale"), tuneLength = 10)
logitBoost3
# train accuracy - 86.02%

resultX = predict(logitBoost3, newdata = testData3)
sum(resultX == testData3$Likelihood.to.recommend)/length(testData3$Likelihood.to.recommend)
# test accuracy - 80.92%

F1_Score(testData3$Likelihood.to.recommend, resultX)
# F1 score - 92.11%

varImp(logitBoost1)

sum(personalTravelBinned$Likelihood.to.recommend == "Detractor")/length(personalTravelBinned$Likelihood.to.recommend)

analysisColumns1 = c("Airline.Status", "olong", "olat",  "Age", "Gender", "Price.Sensitivity", "Flight.Distance",
                    "Eating.and.Drinking.at.Airport", "Flights.Per.Year","Origin.State", "Total.Freq.Flyer.Accts")
analysisData1 = personalTravelBinned[, names(personalTravelBinned) %in% c(analysisColumns1, "Likelihood.to.recommend") ]

analysisData1 = prepareForAnalysis(analysisData1, analysisColumns1)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(1)
inTraining1 = createDataPartition(analysisData1$Likelihood.to.recommend, p = .75, list = FALSE)
trainData1 = analysisData1[inTraining1,]
testData1 = analysisData1[-inTraining1,]

logitBoost1 <- train(factor(Likelihood.to.recommend) ~., data = trainData1, method = "LogitBoost", trControl=fitControl, preProcess = c("center", "scale"), tuneLength = 10)
logitBoost1
# train accuracy - 86.06%

result1 = predict(logitBoost1, newdata = testData1)
sum(result1 == testData1$Likelihood.to.recommend)/length(testData1$Likelihood.to.recommend)
# test accuracy - 86.06%

F1_Score(testData1$Likelihood.to.recommend, result1)
# F1 score - 92.11%

varImp(logitBoost1)

# Removing unimportant data
analysisData2 = personalTravelBinned[, names(personalTravelBinned) %in% c(analysisColumns1, "Likelihood.to.recommend") ]
analysisData2 = analysisData2[analysisData2$Eating.and.Drinking.at.Airport != ">400",]
analysisData2$Eating.and.Drinking.at.Airport = as.factor(as.character(analysisData2$Eating.and.Drinking.at.Airport))

analysisData2 = analysisData2[!analysisData2$Origin.State %in% c("Maine", "Idaho", "Mississippi", "New Jersey"),]
analysisData2$Origin.State = as.factor(as.character(analysisData2$Origin.State))

analysisData2 = analysisData2[as.numeric(analysisData2$Total.Freq.Flyer.Accts) < 7,]
analysisData2$Total.Freq.Flyer.Accts = as.factor(as.character(analysisData2$Total.Freq.Flyer.Accts))

analysisData2 = prepareForAnalysis(analysisData2, analysisColumns1)

set.seed(1)
inTraining2 = createDataPartition(analysisData2$Likelihood.to.recommend, p = .75, list = FALSE)
trainData2 = analysisData2[inTraining2,]
testData2 = analysisData2[-inTraining2,]

logitBoost2 <- train(factor(Likelihood.to.recommend) ~., data = trainData2, method = "LogitBoost", trControl=fitControl, preProcess = c("center", "scale"), tuneLength = 10)
logitBoost2
# train accuracy - 86.19%

result2 = predict(logitBoost2, newdata = testData2)
sum(result2 == testData2$Likelihood.to.recommend)/length(testData2$Likelihood.to.recommend)
# test accuracy - 82.81%

F1_Score(testData2$Likelihood.to.recommend, result2)
# F1 score - 92.68%

varImp(logitBoost2)

svmRadial1 <- train(factor(Likelihood.to.recommend) ~., data = trainData1, method = "svmRadial", trControl=fitControl, preProcess = c("center", "scale"), tuneLength = 10)
svmRadial1
# train accuracy - 83.97%

result3 = predict(svmRadial1, newdata = testData1)
sum(result3 == testData1$Likelihood.to.recommend)/length(testData1$Likelihood.to.recommend)
# test accuracy - 84.04%

F1_Score(testData1$Likelihood.to.recommend, result3)
# F1 score -

varImp(svmRadial1)

svmRadial2 <- train(factor(Likelihood.to.recommend) ~., data = trainData2, method = "svmRadial", trControl=fitControl, preProcess = c("center", "scale"), tuneLength = 10)
svmRadial2
# train accuracy - 83.95%

result4 = predict(svmRadial2, newdata = testData2)
sum(result4 == testData2$Likelihood.to.recommend)/length(testData2$Likelihood.to.recommend)
# test accuracy - 84.07%

F1_Score(testData2$Likelihood.to.recommend, result4)
# F1 score -
varImp(svmRadial2)

nrow(personalTravel[personalTravel$Airline.Status == "Blue",])
nrow(df[df$Airline.Status == "Blue",])

# No significance improvement by using the 2nd model.
# High accuracy proves the strong association of the Personal Travel customers with their ratings. They tend to be
# the detractors and the airline should focus more on these type of customers.
# No definite conclusions about which factors affect the ratings of these type of customers.
# Trying to get a better model with Airline.Status == "Blue" since this has the bulk of the data (2504/3212).

personalBlue = df[str_trim(df$Type.of.Travel) == "Personal Travel" & str_trim(df$Airline.Status) == "Blue", !names(df) %in% c("Type.of.Travel", "Airline.Status")]
personalBlueBinned = getBinnedData(personalBlue)
personalBlueTnx = as(personalBlueBinned, "transactions")

rulesetPromoters <- apriori(personalBlueTnx,
                            parameter=list(support=0.002,confidence=0.5),
                            appearance = list(default="lhs", rhs=("Likelihood.to.recommend=Promoter")))

rulesetDetractors <- apriori(personalBlueTnx,
                             parameter=list(support=0.05,confidence=0.8),
                             appearance = list(default="lhs", rhs=("Likelihood.to.recommend=Detractor")))

summary(quality(rulesetPromoters)$lift)
summary(quality(rulesetDetractors)$lift)
length(rulesetPromoters[quality(rulesetPromoters)$lift > 28])
length(rulesetDetractors[quality(rulesetDetractors)$lift > 1.11])
arules::inspect(rulesetPromoters[quality(rulesetPromoters)$lift > 28])
arules::inspect(rulesetDetractors[quality(rulesetDetractors)$lift > 1.11])

inspectDT(rulesetPromoters[quality(rulesetPromoters)$lift > 28])
inspectDT(rulesetDetractors[quality(rulesetDetractors)$lift > 1.11])

# The columns with strong association were considered for classification modeling.

##### classification models for Personal Travel, Blue data #####
analysisColumns1 = c("Age", "Gender", "Flight.Distance", "Eating.and.Drinking.at.Airport",
                    "olong", "Arrival.Delay.in.Minutes", "Loyalty", "Total.Freq.Flyer.Accts")
analysisData1 = personalBlueBinned[, names(personalBlueBinned) %in% c(analysisColumns1, "Likelihood.to.recommend")]
analysisData1 = prepareForAnalysis(analysisData1, analysisColumns1)

set.seed(1)
inTraining1 = createDataPartition(analysisData1$Likelihood.to.recommend, p = .75, list = FALSE)
trainData1 = analysisData1[inTraining1,]
testData1 = analysisData1[-inTraining1,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

logitBoost1 <- train(factor(Likelihood.to.recommend) ~., data = trainData1, method = "LogitBoost", trControl=fitControl, preProcess = c("center", "scale"), tuneLength = 10)
logitBoost1
# train accuracy - 89.84%

plot(logitBoost1)

result5 = predict(logitBoost1, newdata = testData1)
sum(result5 == testData1$Likelihood.to.recommend)/length(testData1$Likelihood.to.recommend)
# test accuracy - 89.76%

F1_Score(testData1$Likelihood.to.recommend, result5)
# F1 score - 94.68%

varImp(logitBoost1)

svmRadial1 <- train(factor(Likelihood.to.recommend) ~., data = trainData1, method = "svmRadial", trControl=fitControl, preProcess = c("center", "scale"), tuneLength = 10)
svmRadial1
# train accuracy - 89.92%

plot(svmRadial1)

result6 = predict(svmRadial1, newdata = testData1)
sum(result6 == testData1$Likelihood.to.recommend)/length(testData1$Likelihood.to.recommend)
# test accuracy - 89.44%

F1_Score(testData1$Likelihood.to.recommend, result6)
# F1 score - 94.4%

varImp(svmRadial1)

femalePredict = predict(svmRadial1, testData1[testData1$Gender_Male == 0,])
sum(femalePredict == "Detractor")/nrow(testData1[testData1$Gender_Male == 0,])

varImp(svmRadial1)
sum(personalBlueBinned$Likelihood.to.recommend == "Detractor")/length(personalBlueBinned$Likelihood.to.recommend)
# The high accuracy of the model proves the strong association between the customers and their ratings.
# The female customers with type of travel as Personal, airline status as Blue and with no frequent flyer accounts tend to be detactors.
# Customers with origin longitude between -120 - -95 tend to be detractors.

nrow(df[df$Gender == "Female",])/nrow(df)
nrow(personalBlue[personalBlue$Gender == "Female",])/nrow(personalBlue)

mean(df$Likelihood.to.recommend[df$Gender == "Female"])
mean(personalBlue$Likelihood.to.recommend[personalBlue$Gender == "Female"])

# The airline should focus more on the female customers with type of travel as Personal and airline status
# as Blue as their average ratings are significantly lower than that of all female customers and they have
# a tendency to be detractors as proven by the model. They also contribute more to the overall ratings as their
# ratio is higher in this category than the overall data.

nrow(df[df$Gender == "Female" & df$Total.Freq.Flyer.Accts == 0,])/nrow(df)
nrow(personalBlue[personalBlue$Gender == "Female" & personalBlue$Total.Freq.Flyer.Accts == 0,])/nrow(personalBlue)

mean(df$Likelihood.to.recommend[df$Gender == "Female" & df$Total.Freq.Flyer.Accts == 0])
mean(personalBlue$Likelihood.to.recommend[personalBlue$Gender == "Female" & personalBlue$Total.Freq.Flyer.Accts == 0])

# The above conclusion is also true for female customers with no frequent flyer accounts.
nrow(dfBinnedData[dfBinnedData$Type.of.Travel == "Personal Travel" & df$Airline.Status == "Blue" & df$Gender == "Female",])/nrow(dfBinnedData)
mean(df$Likelihood.to.recommend[df$Type.of.Travel == "Personal Travel"])
mean(df$Likelihood.to.recommend)
mean(df$Likelihood.to.recommend[df$Type.of.Travel == "Personal Travel" & df$Airline.Status == "Blue" & df$Gender == "Female"])
mean(df$Likelihood.to.recommend[df$Gender == "Female"])

