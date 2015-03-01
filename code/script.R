#### initial setup
rm(list=ls())
getwd()
setwd('/Users/hawooksong/Desktop/stockPrediction')



#### load libraries
library(xts)
library(quantmod)  # for getSymbols()
library(data.table)  # for rbindlist()
library(plyr)  # for ddply()
library(caTools)  # for sample.split()
library(rpart)  # for rpart()
library(rpart.plot)  # for prp()
library(randomForest)  # for randomForest()
library(MASS)  # for lda() and qda()
# library(e1071)  # for svm()

#### download data for NASDAQ 100 stocks
symbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", 
             "AMAT", "AMGN", "AMZN", "ATVI", "AVGO", "BBBY", "BIDU", "BIIB", 
             "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CHTR", "CMCSA", 
             "COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DISCK", "DISH", 
             "DLTR", "DTV", "EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST", 
             "FB", "FFIV", "FISV", "FOXA", "GILD", "GMCR", "GOOG", "GOOGL", 
             "GRMN", "HSIC", "ILMN", "INTC", "INTU", "ISRG", "KLAC", "KRFT", 
             "LBTYA", "LLTC", "LMCA", "LMCK", "LVNTA", "MAR", "MAT", "MDLZ", 
             "MNST", "MSFT", "MU", "MXIM", "MYL", "NFLX", "NTAP", "NVDA", 
             "NXPI", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "QVCA", "REGN", 
             "ROST", "SBAC", "SBUX", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", 
             "STX", "SYMC", "TRIP", "TSCO", "TSLA", "TXN", "VIAB", "VIP", 
             "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "YHOO")
getSymbols(symbols)



#### combine all datasets into one vertically
## function that extracts ticker name from individual stock df
getTicker <- function(df) {
  colnames <- colnames(df)  
  ticker <- unlist(strsplit(colnames, '\\.'))[[1]][1]
  return(ticker)
}

## function that adds a column for ticker
markTicker <- function(df) {
  df$ticker <- getTicker(df)
  return(df)
}

## standardize individual column 
stdColnames <- function(df) {
  ticker <- getTicker(df)
  rmPattern <- paste0(ticker, '\\.')
  colnames(df) <- gsub(rmPattern, '', colnames(df))
  colnames(df) <- tolower(colnames(df))
  return(df)
}

## add date column from row names
addDatesFrRownames <- function(df) {
  df$date <- as.Date(row.names(df))
  row.names(df) <- NULL
  return(df)
}

## prepare/preprocess individual ticker dataset
prepIndTickerSet <- function(df) {
  df <- as.data.frame(df)
  df <- markTicker(df)
  df <- stdColnames(df)
  df <- addDatesFrRownames(df)
  return(df)
}

## vertical merge
mergeDatasetsVer <- function(symbols) {
  stocksList <- lapply(symbols, get)  # load all individuals datasets into a list
  stocksList <- lapply(stocksList, prepIndTickerSet)  # prepare each individual dataset in the list
  outputDF <- rbindlist(stocksList)  # bind verticall
  outputDF <- as.data.frame(outputDF)
  row.names(outputDF) <- 1:nrow(outputDF)
  return(outputDF)
}

stocks <- mergeDatasetsVer(symbols)
dim(stocks)
str(stocks)
write.csv(stocks, './data/stocksVer.csv', row.names=FALSE)



#### remove individual datasets
rm(list=symbols)



#### read from CSV
stocks <- read.csv('./data/stocksVer.csv', na.strings=c('', ' ', NA))



#### functions
## add extra calendar info from date
addExtCalendarInfo <- function(df) {
  df <- as.data.frame(df)
  df$date <- as.Date(df$date)
  df$year <- as.integer(format(df$date, '%Y'))
  df$month <- as.factor(format(df$date, '%m'))
  df$week <- as.factor(format(df$date, '%W'))
  df$day <- as.factor(weekdays(df$date, abbr=TRUE))
  return(df)
}

## remove extra calendar info
rmExtCalendarInfo <- function(df) {
  df$year <- df$month <- df$week <- df$day <- NULL
  return(df)
}

## spread
addSpread <- function(df) {
  df$spread <- df$high - df$low
  return(df)
}

## simple moving average for a single stock
addSMASingleStock <- function(df, n) {
  colName <- paste0('sma', n)
  df[[colName]] <- SMA(Cl(df), n)
  return(df)
}

## simple moving average for multiple stocks
addSMAMultStocks <- function(df, n) {
  outputDF <- ddply(df, 'ticker', function(x) {
    addSMASingleStock(x, n)
  })
  return(outputDF)
}

## create a column for next day's performance
addNextDayPerfSingleStock <- function(df, perfMetric) {  
  colName <- paste0(perfMetric, 'NextDay')
  df[[colName]] <- c(df[[perfMetric]][2:nrow(df)], NA)
  return(df)
}

## create a column for perf differentials for multiple stocks
addNextDayPerfMultStocks <- function(df, perfMetric) {
  outputDF <- ddply(df, 'ticker', function(x) {
    # print(as.character(x$ticker[1]))
    addNextDayPerfSingleStock(x, perfMetric)
  })
  return(outputDF)
}

## normalize columns for a single stock
normalizeSingleStock <- function(df) {
  ## numeric columns to normalize
  numCols <- c('open', 'high', 'low', 'close', 'volume', 'adjusted', 'spread')
  numCols <- c(numCols, colnames(df)[grepl('sma|diff', colnames(df))])
  
  ## normalize
  df[numCols] <- sapply(df[numCols], scale)
  
  ## round
  df[numCols] <- round(df[numCols], 2)
  
  ## return
  return(df)
}

## normalize columns for multiple stocks
normalizeMultStocks <- function(df) {
  outputDF <- ddply(df, 'ticker', function(x) {
    # print(as.character(x$ticker[1]))
    normalizeSingleStock(x)
  })
  return(outputDF)
}

## add outcome column for df containing multiple stocks
addOutcomeCol <- function(df, perfMetric) {

  ## add column for next day's performance
  df <- addNextDayPerfMultStocks(df, perfMetric)

  ## create outcome column
  nextDayPerfColName <- paste0(perfMetric, 'NextDay')  
  df$outcome <- ifelse(df[[nextDayPerfColName]] > df[[perfMetric]], 1, 0)  
  
  ## remove column for next day's performance
  df[[nextDayPerfColName]] <- NULL
  
  ## return
  return(df)
}

## remove rows with NAs for outcome
rmMissValRows <- function(df) {
  df <- subset(df, !is.na(df$outcome))
  return(df)
}

## calculate accuracy from confusion matrix
calcAccFrConfMatrix <- function(confMatrix) {
  accuracy <- sum(diag(confMatrix)) / sum(confMatrix)
  return(accuracy)
}



#### 
# stocks <- addExtCalendarInfo(stocks)
# stocks <- addSMAMultStocks(stocks, 20)
stocks <- addSpread(stocks)
stocks <- addOutcomeCol(stocks, perfMetric='close')
stocks <- rmMissValRows(stocks)
stocks <- normalizeMultStocks(stocks)



#### split training vs. test
set.seed(123)
split <- sample.split(stocks$outcome, SplitRatio=0.7)
train <- stocks[split==TRUE, ]
test <- stocks[split==FALSE, ]



#### naive model
table(train$outcome)
predNaive <- rep(1, nrow(test))

confMatrixNaive <- table(predNaive, test$outcome)
accNaive <- calcAccFrConfMatrix(confMatrixNaive)
accNaive



#### logistic regression
modelLogis <- glm(outcome ~ . - date - ticker, data=train, family='binomial')
summary(modelLogis)

predProbLogis <- predict(modelLogis, newdata=test, type='response')
predLogis <- ifelse(predProbLogis > 0.5, 1, 0)

confMatrixLogis <- table(predLogis, test$outcome)
accLogis <- calcAccFrConfMatrix(confMatrixLogis)
accLogis



#### decision tree model
# modelDT <- rpart(outcome ~ . - date - ticker, data=train, method='class')
modelDT <- rpart(outcome ~ open + high + low + close + volume + adjusted + spread, data=train, method='class')
prp(modelDT)

predProbDT <- predict(modelDT, newdata=test, type='prob')[ , 2]
predDT <- ifelse(predProbDT > 0.5, 1, 0)

confMatrixDT <- table(predDT, test$outcome)
accDT <- calcAccFrConfMatrix(confMatrixDT)
accDT



#### random forest model
train$outcome <- as.factor(train$outcome)
test$outcome <- as.factor(test$outcome)

modelRF <- randomForest(outcome ~ . - date - ticker, data=train)

predProbRF <- predict(modelRF, newdata=test, type='prob')[ , 2]
predRF <- ifelse(predProbRF > 0.5, 1, 0)

confMatrixRF <- table(predRF, test$outcome)
accRF <- calcAccFrConfMatrix(confMatrixRF)
accRF


#### linear discriminant analysis
modelLDA <- lda(outcome ~ . - date - ticker, data=train)

predProbLDA <- predict(modelLDA, newdata=test)$posterior[ , 2]
predLDA <- ifelse(predProbLDA > 0.5, 1, 0)

confMatrixLDA <- table(predLDA, test$outcome)
accLDA <- calcAccFrConfMatrix(confMatrixLDA)
accLDA



#### quadratic discriminant analysis
modelQDA <- qda(outcome ~ . - date - ticker, data=train)

predProbQDA <- predict(modelQDA, newdata=test)$posterior[ , 2]
predQDA <- ifelse(predProbQDA > 0.5, 1, 0)

confMatrixQDA <- table(predQDA, test$outcome)
accQDA <- calcAccFrConfMatrix(confMatrixQDA)
accQDA



#### support vector machine model (crashes; too many data points)
# modelSVM <- svm(outcome ~ . - date - ticker, data=train)
# summary(modelSVM)
# 
# predSVM <- predict(modelSVM, newdata=test)
# 
# confMatrixSVM <- table(predSVM, test$outcome)
# calcAccFrConfMatrix(confMatrixSVM)



#### ensemble (LR + RF + LDA)
predProbEns1 <- (predProbLogis + predProbRF + predProbLDA) / 3
predEns1 <- ifelse(predProbEns1 > 0.5, 1, 0)

confMatrixEns1 <- table(predEns1, test$outcome)
accEns1 <- calcAccFrConfMatrix(confMatrixEns1)
accEns1



#### ensemble 2 (LR + LDA)
predProbEns2 <- (predProbLogis + predProbLDA) / 2
predEns2 <- ifelse(predProbEns2 > 0.5, 1, 0)

confMatrixEns2 <- table(predEns2, test$outcome)
accEns2 <- calcAccFrConfMatrix(confMatrixEns2)
accEns2




#### accuracy comparison
accNaive
accLogis
accDT
accRF
accLDA
accQDA
accEns1
accEns2
