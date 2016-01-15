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

cat("reading the train and test data\n")
train <- read_csv("./data/train.csv", col_types = cols(StateHoliday = 'c'))
test  <- read_csv("./data/test.csv", col_types = cols(StateHoliday = 'c'))
store <- read_csv("./data/store.csv")

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
train <- merge(train,store)
test <- merge(test,store)

# There are some NAs in the integer columns so conversion to zero
train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

cat("train data column names and details\n")
names(train)
str(train)
summary(train)
cat("test data column names and details\n")
names(test)
str(test)
summary(test)

# looking at only stores that were open in the train set
# may change this later
train <- train[ which(train$Open=='1' & train$Sales!='0'),]
# seperating out the elements of the date column for the train set
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))

train$Date_yearday <- as.integer(format(train$Date, "%j"))
train$Date_year <- as.integer(format(train$Date, "%Y"))


# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
train <- train[, -3]

# seperating out the elements of the date column for the test set
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))

test$Date_yearday <- as.integer(format(test$Date, "%j"))
test$Date_year <- as.integer(format(test$Date, "%Y"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
test <- test[, -4]

feature.names <- names(train)[c(1,2,5:20)]
cat("Feature Names\n")
feature.names

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
    if (class(train[[f]])=="character") {
        levels <- unique(c(train[[f]], test[[f]]))
        train[[f]] <- as.integer(factor(train[[f]], levels=levels))
        test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    }
}

cat("train data column names after slight feature engineering\n")
names(train)
cat("test data column names after slight feature engineering\n")
names(test)

train[is.na(train)] <- 0
test[is.na(test)] <- 0


trainOpened <- train[train$Open == 1 & train$Sales > 0, ]
testOpened <- test[test$Open == 1, ]



feature.names <- names(train)[c(1,2,6:19)]
feature.names <- c(feature.names, 'PrevDayPromo', 'PrevDayStateHoliday', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'Promo2Open', 'IsPromo2Month',
                   'StateHoliday')
tra <- train[, feature.names]

for(i in 1:20) {
    print(i)
    resultColName = paste0('predWinnerHoliday_', i)

    h <- sample(nrow(train), 20000)
    dval <- xgb.DMatrix(data=data.matrix(tra[h,]), label=log(train$Sales+1)[h])
    dtrain <- xgb.DMatrix(data=data.matrix(tra[-h,]), label=log(train$Sales+1)[-h])
    watchlist <- list(val=dval, train=dtrain)
    param <- list(  objective           = "reg:linear",
                    booster             = "gbtree",
                    eta                 = 0.03, # 0.06, #0.01,
                    max_depth           = 12, #changed from default of 8
                    subsample           = 0.9, # 0.7
                    colsample_bytree    = 0.8 # 0.7
    )

    clf <- xgb.train(   params              = param,
                        data                = dtrain,
                        nrounds             = 3001,
                        #verbose             = 1,
                        early.stop.round    = 20,
                        watchlist           = watchlist,
                        print.every.n       = 20,
                        maximize            = FALSE,
                        feval               = RMPSE
    )

    #feature.importance.1 <- xgb.importance(colnames(tra), model = clf)

    train[, resultColName] <- exp(predict(clf, data.matrix(train[, feature.names]))) - 1
    test[, resultColName] <- exp(predict(clf, data.matrix(test[, feature.names]))) - 1
}


submission <- data.frame(Id=test$Id, Sales=pred6)
cat("saving the submission file\n")
write_csv(submission, "rf6.csv")
