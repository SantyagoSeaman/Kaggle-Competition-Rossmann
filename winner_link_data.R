
trainOpened$Date <- as.Date(apply(trainOpened[, c('month', 'day', 'year')], 1, function(row) {
    return (paste(row, collapse = '-'))
}), "%m-%d-%y")
trainOpened$Date_int <- as.integer(trainOpened$Date)


testOpened <- left_join(testOpened,
                        test[, c('Id', 'PrevDayPromo', 'PrevDayStateHoliday', 'WorkInHoliday', 'WorkInSunday',
                                 'CompetitionOpen', 'Promo2Open', 'IsPromo2Month',
                                 'PredictedMeanMonthSales', 'PredictedMeanMonthSalesLog')],
                        by = 'Id')

trainOpened <- left_join(trainOpened,
                         train[, c('Store', 'Date', 'PrevDayPromo', 'PrevDayStateHoliday', 'WorkInHoliday', 'WorkInSunday',
                                 'CompetitionOpen', 'Promo2Open', 'IsPromo2Month',
                                 'PredictedMeanMonthSales', 'PredictedMeanMonthSalesLog')],
                        by = 'Store', 'Date')


# -------------------------------------------------------------------
trainConverted <- train
testConverted <- test
rm(fullDataset, train, test)

# -------------------------------------------------------------------
train$Date <- as.Date(apply(train[, c('month', 'day', 'year')], 1, function(row) {
    return (paste(row, collapse = '-'))
}), "%m-%d-%y")
train$Date_int <- as.integer(train$Date)


test <- left_join(test,
                  testConverted[, c('Id', 'PrevDayPromo', 'PrevDayStateHoliday', 'WorkInHoliday', 'WorkInSunday',
                                 'CompetitionOpen', 'Promo2Open', 'IsPromo2Month',
                                 'PredictedMeanMonthSales', 'PredictedMeanMonthSalesLog')],
                        by = 'Id')
trainConverted <- trainConverted[trainConverted$Sales > 0 & trainConverted$Open == 1, ]
trainConverted <- trainConverted[order(trainConverted$Store, trainConverted$Date_int), ]
train <- train[order(train$Store, train$Date_int), ]
train <- cbind(train, trainConverted[, c('PrevDayPromo', 'PrevDayStateHoliday', 'WorkInHoliday', 'WorkInSunday',
                                   'CompetitionOpen', 'Promo2Open', 'IsPromo2Month',
                                   'PredictedMeanMonthSales', 'PredictedMeanMonthSalesLog')])



train$PrevDayStateHoliday <- as.integer(train$PrevDayStateHoliday)
test$PrevDayStateHoliday <- as.integer(test$PrevDayStateHoliday)

train$PrevDayPromo <- as.integer(train$PrevDayPromo)
test$PrevDayPromo <- as.integer(test$PrevDayPromo)

train$CompetitionOpen <- as.integer(train$CompetitionOpen)
test$CompetitionOpen <- as.integer(test$CompetitionOpen)

train$Promo2Open <- as.integer(train$Promo2Open)
test$Promo2Open <- as.integer(test$Promo2Open)

train$IsPromo2Month <- as.integer(train$IsPromo2Month)
test$IsPromo2Month <- as.integer(test$IsPromo2Month)

train[is.na(train)] <- 0
test[is.na(test)] <- 0


rm(trainConverted, testConverted, trainOpened, testOpened)

