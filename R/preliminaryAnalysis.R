library(jsonlite)
install.packages("rapport")
library(rapportools)
library(tidyverse)
library(tm)
library(arules)

setwd("C:/Data/study/IST-687/Project/ist687")

df = jsonlite::fromJSON("fall2019-survey-M02.json")
sum(sapply(df[,-32], function(v) {
  return ((is.character(v) & is.empty(v)) | is.na(v))
}))

sum(sapply(df$freeText, function(v) {
  return (is.empty(v))
}))

getMissingIndices = function(data) {
  dfMissingIndices = which(sapply(data[,-32], function(v) {
    return ((is.character(v) & is.empty(v)) | is.na(v))
  }, simplify = TRUE))
  dfMissingValues = data[dfMissingIndices%%nrow(data),]
  return(dfMissingValues)
}

View(dfMissingValues)

summary(df)

summaryDf = apply(df, MARGIN = 2, FUN = table)
summaryDf$Type.of.Travel
summaryDf$Partner.Code
summaryDf$Origin.State
summaryDf$Destination.State

plotLTR = function(data, colName) {
  aggregateData = tapply(data$Likelihood.to.recommend, data[, colnames(data) == colName], mean)
  aggregateDf = data.frame(temp = names(aggregateData), avgLTR = aggregateData)
  ggplot(aggregateDf, aes(x = temp, y = avgLTR)) + labs(y = "Average LTR", x = colName) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


########### Sentiment Analysis ############

posWords = scan("positive-words.txt", character(0), sep = "\n")[-1:-34]
negWords = scan("negative-words.txt", character(0), sep = "\n")[-1:-34]

df = data.frame(Id = 1:nrow(df), df)
freeTextData = data.frame(Id = df$Id, text = df$freeText)[!is.na(df$freeText),]
View(freeTextData)

lists = strsplit(as.character(freeTextData$text), ":|\\.|!|\\?|\\n")
lists = sapply(lists, str_trim)
freeTextData$charVector = sapply(lists, function(l) {
  l[sapply(l, function(e) !is.empty(e))]
})

getRatios = function(charVector, posWords, negWords) {
  words.vec = VectorSource(charVector)
  words.corpus = Corpus(words.vec)
  words.corpus = tm_map(tm_map(tm_map(tm_map(words.corpus, content_transformer(tolower)), removePunctuation), removeNumbers), removeWords, stopwords("english"))
  tdm = TermDocumentMatrix(words.corpus)
  wordCounts = rowSums(as.matrix(tdm))
  wordCounts <- sort(wordCounts, decreasing=TRUE)
  matchedP <- match(names(wordCounts), posWords, nomatch = 0)
  matchedN <- match(names(wordCounts), negWords, nomatch = 0)
  ratioPos = sum(wordCounts[matchedP != 0])/sum(wordCounts)
  ratioNeg = sum(wordCounts[matchedN != 0])/sum(wordCounts)
  return(c(ratioPos, ratioNeg))
}

textAnalysis = sapply(freeTextData$charVector, function(v) getRatios(v, posWords, negWords))
View(textAnalysis)

freeTextData$ratioPos = c(textAnalysis[1,])
freeTextData$ratioNeg = c(textAnalysis[2,])

df$ratioPos[!is.na(df$freeText)] = freeTextData$ratioPos
df$ratioNeg[!is.na(df$freeText)] = freeTextData$ratioNeg


########## TypeOfTravel ##########
dfTypeOfTravelTable = tapply(df$Likelihood.to.recommend, df$Type.of.Travel, mean)
dfTypeOfTravel = data.frame(typeOfTravel = names(dfTypeOfTravelTable), avgLTR = dfTypeOfTravelTable)

dfTypeOfTravelTable2 = tapply(df$Likelihood.to.recommend, df$Type.of.Travel, length)
dfTypeOfTravel2 = data.frame(typeOfTravel = names(dfTypeOfTravelTable2), countLTR = dfTypeOfTravelTable2)
temp = (df %>% mutate(Likelihood.to.recommend = cut(Likelihood.to.recommend, breaks = c(min(Likelihood.to.recommend), 7, 8, max(Likelihood.to.recommend)), labels = c("Detractor", "Passive", "Promoter"), include.lowest = TRUE)))
temp$Likelihood.to.recommend[is.na(temp$Likelihood.to.recommend)] = "Passive" # subsituting with the average value (falls under the Passive category)

dfPlotData = data.frame(typeOfTravel = df$Type.of.Travel, Likelihood.to.recommend = temp$Likelihood.to.recommend, avgLTR = dfTypeOfTravel[match(df$Type.of.Travel, dfTypeOfTravel$typeOfTravel),]$avgLTR, countLTR = dfTypeOfTravel2[match(df$Type.of.Travel, dfTypeOfTravel2$typeOfTravel),]$countLTR)
dfPlotData = dfPlotData[order(dfPlotData$Likelihood.to.recommend, decreasing = TRUE),]
ggplot(dfPlotData, aes(x = typeOfTravel, y = avgLTR/countLTR, fill = Likelihood.to.recommend)) + labs(y = "Average Rating", x = "Type of Travel") + ggtitle("Average ratings for types of travel") + geom_col(position = position_stack(reverse = TRUE)) + geom_hline(aes(yintercept = mean(df$Likelihood.to.recommend))) + theme_minimal()

########## PartnerCode ##########
dfPartnerCodeTable = tapply(df$Likelihood.to.recommend, df$Partner.Code, mean)
dfPartnerCode = data.frame(partnerCode = names(dfPartnerCodeTable), avgLTR = dfPartnerCodeTable)
ggplot(dfPartnerCode, aes(x = partnerCode, y = avgLTR)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(dfPartnerCode[dfPartnerCode$avgLTR < 7,], aes(x = partnerCode, y = avgLTR)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

########## OriginState ##########
dfOriginStateTable = tapply(df$Likelihood.to.recommend, df$Origin.State, mean)
dfOriginState = data.frame(originState = names(dfOriginStateTable), avgLTR = dfOriginStateTable)
ggplot(dfOriginState, aes(x = originState, y = avgLTR)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(dfOriginState[dfOriginState$avgLTR < 7,], aes(x = originState, y = avgLTR)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

########## DestinationState ##########
dfDestinationStateTable = tapply(df$Likelihood.to.recommend, df$Destination.State, mean)
dfDestinationState = data.frame(destinationState = names(dfDestinationStateTable), avgLTR = dfDestinationStateTable)
ggplot(dfDestinationState, aes(x = destinationState, y = avgLTR)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(dfDestinationState[dfDestinationState$avgLTR < 7,], aes(x = destinationState, y = avgLTR)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

dfDestinationStateTable = tapply(df$Likelihood.to.recommend, df$Flights.Per.Year, mean)
dfDestinationState = data.frame(destinationState = names(dfDestinationStateTable), avgLTR = dfDestinationStateTable)
ggplot(dfDestinationState, aes(x = destinationState, y = avgLTR)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(dfDestinationState[dfDestinationState$avgLTR < 7,], aes(x = destinationState, y = avgLTR)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


########## Relative Distributions ##########
typeOfTravelData = df[df$Type.of.Travel %in% dfTypeOfTravel$typeOfTravel[dfTypeOfTravel$avgLTR < 7],]
prop.table(table(typeOfTravelData$Type.of.Travel, typeOfTravelData$Likelihood.to.recommend))*100

partnerCodeData = df[df$Partner.Code %in% dfPartnerCode$partnerCode[dfPartnerCode$avgLTR < 7],]
prop.table(table(partnerCodeData$Partner.Code, partnerCodeData$Likelihood.to.recommend))*100

originStateData = df[df$Origin.State %in% dfOriginState$originState[dfOriginState$avgLTR < 7],]
prop.table(table(originStateData$Origin.State, originStateData$Likelihood.to.recommend))*100

destinationData = df[df$Destination.State %in% dfDestinationState$destinationState[dfDestinationState$avgLTR < 7],]
prop.table(table(destinationData$Destination.State, destinationData$Likelihood.to.recommend))*100


hist(df$Likelihood.to.recommend[df$Type.of.Travel == "Business travel"])

dfPersonalTravel = df[df$Type.of.Travel == "Personal Travel",]
dfPersonalTravelBlueTable = tapply(dfPersonalTravel$Likelihood.to.recommend, dfPersonalTravel$Airline.Status, mean)
dfPersonalTravelBlue = data.frame(typeOfTravel = names(dfPersonalTravelBlueTable), avgLTR = dfPersonalTravelBlueTable)
ggplot(dfPersonalTravelBlue, aes(x = typeOfTravel, y = avgLTR)) + labs(y = "Average Rating", x = "Type of Travel") + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_hline(aes(yintercept = 6))
ggplot(dfTypeOfTravel[dfTypeOfTravel$avgLTR < 7,], aes(x = typeOfTravel, y = avgLTR)) + labs(y = "Average Rating", x = "Type of Travel") + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

