# Based on Ben Hamner script from Springleaf
# https://www.kaggle.com/benhamner/springleaf-marketing-response/random-forest-example

library(readr)
library(xgboost)

RMPSE <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    elab <- exp(as.numeric(labels))-1
    epreds <- exp(as.numeric(preds))-1
    err <- sqrt(mean((epreds/elab-1)^2))
    return(list(metric = "RMPSE", value = err))
}

#my favorite seed^^
set.seed(1001)

train <- read_csv("./data/train.csv", col_types = cols(StateHoliday = 'c'))
test  <- read_csv("./data/test.csv", col_types = cols(StateHoliday = 'c'))
store <- read_csv("./data/store.csv")

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
train <- merge(train, store)
test <- merge(test, store)

# There are some NAs in the integer columns so conversion to zero
train[is.na(train)] <- 0
test[is.na(test)] <- 0

# seperating out the elements of the date column for the train set
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))

train$Date_yearday <- as.integer(format(train$Date, "%j"))
train$Date_year <- as.integer(format(train$Date, "%Y"))


# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
train <- train[,-c(3,8)]

# seperating out the elements of the date column for the test set
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))

test$Date_yearday <- as.integer(format(test$Date, "%j"))
test$Date_year <- as.integer(format(test$Date, "%Y"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
test <- test[,-c(4,7)]

feature.names <- names(train)[c(1,2,6:19)]
feature.names

for (f in feature.names) {
    if (class(train[[f]])=="character") {
        levels <- unique(c(train[[f]], test[[f]]))
        train[[f]] <- as.integer(factor(train[[f]], levels=levels))
        test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    }
}

trainOpened <- train[train$Open == 1 & train$Sales > 0, ]
testOpened <- test[test$Open == 1, ]



tra <- trainOpened[, feature.names]
dtest <- data.matrix(testOpened[, feature.names])

folds <- createFolds(trainOpened$Store, 20)
for(i in 1:20)
{
    print(i)

    validFoldIdx <- unlist(folds[i])
    trainFoldIds <- unlist(folds[-i])

    dval <- xgb.DMatrix(data = data.matrix(tra[validFoldIdx,]), label = log(trainOpened$Sales+1)[validFoldIdx])
    dtrain <- xgb.DMatrix(data = data.matrix(tra[trainFoldIds,]), label = log(trainOpened$Sales+1)[trainFoldIds])
    dfull <- xgb.DMatrix(data = data.matrix(tra), label = log(trainOpened$Sales+1))
    watchlist <- list(val = dval, train = dtrain)
    param <- list(  objective           = "reg:linear",
                    booster             = "gbtree",
                    eta                 = 0.03,
                    max_depth           = 10,
                    subsample           = 0.8,
                    colsample_bytree    = 0.8
    )

    clf <- xgb.train(   params              = param,
                        data                = dtrain,
                        nrounds             = 3001,
                        #verbose             = 1,
                        early.stop.round    = 100,
                        watchlist           = watchlist,
                        print.every.n       = 200,
                        maximize            = FALSE,
                        feval               = RMPSE
    )

    resultColName = paste0('predFoldedSecond', i)
    trainOpened[, resultColName] <- exp(predict(clf, dfull)) - 1
    testOpened[, resultColName] <- exp(predict(clf, dtest)) - 1
}

# ----------------------------------------------------------------------------
tranNames <- names(trainOpened)
predFoldedNames <- tranNames[grep('predFolded([0-9]{1,2})', tranNames)]
#feature.names.with.predicted <- predFoldedNames
feature.names.with.predicted <- c(feature.names, predFoldedNames)
feature.formula <- formula(paste('Sales ~ ', paste(feature.names.with.predicted, collapse = ' + '), sep = ''))


dtest_cv <- testOpened[, feature.names.with.predicted]
dtest <- data.matrix(dtest_cv)
sparseMatrixColNamesTest <- colnames(dtest)


dtrain_cv <- trainOpened[, feature.names.with.predicted]
sparseMatrixColNamesTrain <- colnames(dtrain_cv)
indexes <- sample(seq_len(nrow(dtrain_cv)), floor(nrow(dtrain_cv)*0.98))
dtrain <- xgb.DMatrix(data = data.matrix(dtrain_cv[indexes, ]), label = trainOpened[indexes, 'Sales'])
dvalid <- xgb.DMatrix(data = data.matrix(dtrain_cv[-indexes, ]), label = trainOpened[-indexes, 'Sales'])
dfull <- xgb.DMatrix(data = data.matrix(dtrain_cv), label = trainOpened$Sales)

n_rounds = 200
watchlist <- list(eval = dvalid, train = dtrain)
params <- list(booster = "gbtree", objective = "reg:linear",
               max_depth = 15, eta = 0.05,
               colsample_bytree = 0.9, subsample = 0.9)

model <- xgb.train(params = params, data = dtrain,
                   nrounds = n_rounds, early.stop.round = 10, maximize = F,
                   feval = rmpse.feval.sales,
                   watchlist = watchlist, print.every.n = 10)
#feature.importance <- xgb.importance(sparseMatrixColNamesTrain, model = model)

#rmpse(predict(model, dvalid), trainOpened[-indexes, 'Sales'])
#rmpse(predict(model, dfull), dtrain_cv$Sales)

trainOpened$StackedSales <- predict(model, dfull)
testOpened$StackedSales <- predict(model, dtest)
testOpened$MeanSales <- rowMeans(testOpened[, predFoldedNames])

temp <- left_join(rf.0.10, testOpened[, c('Id', 'StackedSales', 'MeanSales')], by = 'Id')
temp[is.na(temp)] <- 0

#feature.importance.1 <- xgb.importance(colnames(dval), model = clf)

pred6 <- exp(predict(clf, data.matrix(test[,feature.names]))) -1
submission <- data.frame(Id=temp$Id, Sales=temp$MeanSales)
write_csv(submission, "./results/MeanSales.csv")
