feature.names <- c('DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday', 'StoreType', 'Assortment',
                   'Date_month', 'Date_year', 'MeanMonthSalesLog', 'MeanYearSalesLog', 'MeanSalesInDayOfWeekLog',
                   'PrevDayStateHoliday', 'PrevDayPromo',
                   'WorkInHoliday', 'WorkInSunday')

feature.names <- c('Store', 'DayOfWeek_factor', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'StoreType', 'Assortment',
                   'Date_month', 'Date_year',
                   'PrevDayStateHoliday', 'PrevDayPromo',
                   'WorkInHoliday', 'WorkInSunday',
                   'CompetitionDistance',
                   'CompetitionOpenSinceMonth',
                   'CompetitionOpenSinceYear'
                   )

feature.names <- c('Store', 'StoreType', 'Assortment',
                   'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month',
                   'MeanSalesInDayOfWeekLog', 'CurrentYearMeanCurMonthSalesLog', 'PredictedMeanMonthSalesLog',
                   'MeanSalesInDayOfWeekEjection', 'MeanSalesInDayOfWeekSuperEjection')


feature.names <- c('Store', 'StoreType', 'Assortment',
                   'Date_year', 'Date_month', 'Date_yearday', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek', 'Date_monthday',
                   #'Date_year', 'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek',
                   'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month',
                   'PredictedMeanMonthSalesLog')

feature.names <- c('Store', 'StoreType', 'Assortment',
                   'Date_year', 'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek', 'Date_monthday',
                   #'Date_year', 'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek',
                   'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month')

feature.formula <- formula(paste('SalesLog ~ ', paste(feature.names, collapse = ' + '), sep = ''))


trainDatesFilter <- train$Date >= as.Date('2013-01-01') & train$Date <= '2015-07-31'

dtest_cv <- test[test$Open == 1, feature.names]
dtest_cv$SalesLog = 0
dtest_cv[is.na(dtest_cv)]<- 0
dtest <- sparse.model.matrix(feature.formula, data = dtest_cv)
sparseMatrixColNamesTest <- colnames(dtest)
errors.valid <- c()

for(i in 1:20) {
    print(i)
    resultColName = paste0('predFullMonth_', i)

    dtrain_cv <- train[train$Open == 1 & trainDatesFilter, c(feature.names, 'Sales', 'SalesLog')]
    indexes <- sample(seq_len(nrow(dtrain_cv)), floor(nrow(dtrain_cv)*0.98))
    data <- sparse.model.matrix(feature.formula, data = dtrain_cv[indexes, ])
    sparseMatrixColNamesTrain <- colnames(data)
    dtrain <- xgb.DMatrix(data, label = dtrain_cv[indexes, 'SalesLog'])
    dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv[-indexes, ]),
                          label = dtrain_cv[-indexes, 'SalesLog'])
    dfull <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv),
                         label = dtrain_cv$SalesLog)

    #n_rounds = 1001 + 200*i
    n_rounds = 1001
    #watchlist <- list(eval = dvalid, train = dtrain, main = dfull)
    watchlist <- list(eval = dvalid, train = dtrain)
    #watchlist <- list(full = dtrain)
    params <- list(booster = "gbtree", objective = "reg:linear",
                   max_depth = 10, eta = 0.02,
                   colsample_bytree = 0.9, subsample = 0.9)

    model <- xgb.train(params = params, data = dtrain,
                       nrounds = n_rounds, early.stop.round = 100, maximize = F,
                       feval = rmpse.feval,
                       watchlist = watchlist, print.every.n = 200)
    #feature.importance.3 <- xgb.importance(sparseMatrixColNamesTrain, model = model)

    errors.valid <- c(errors.valid, rmpse(exp(predict(model, dvalid)), dtrain_cv[-indexes, 'Sales']))

    pred <- predict(model, dfull)
    train[, resultColName] <- 0
    train[train$Open == 1 & trainDatesFilter, resultColName] <- as.integer(exp(pred) - 1)
    print(paste0('Full dataset rmpse: ', rmpse(train[trainDatesFilter, resultColName], train[trainDatesFilter, 'Sales'])))

    pred <- predict(model, dtest)
    test[, resultColName] <- 0
    test[test$Open == 1, resultColName] <- as.integer(exp(pred) - 1)
}


View(train[, c(names(train), 'Sales')])


pred.mean <- apply(pred, 1, function(r) mean(unlist(r)))
pred.mean <- as.integer(exp(pred.mean) - 1)

train$pred.sales.log <- as.integer(exp(pred) - 1)
View(train[train$pred.sales.log > 1 & train$pred.sales.log < 200, c('Sales', 'pred.sales.log')])
View(train[train$pred.sales.log > 1 & train$pred.sales.log < 500,])
rmpse(train$pred.sales.log, train$Sales)

View(train[train$Store == 1, c(names(train), 'Sales')])


test$pred.sales <- 0
test[test$Open == 1, 'pred.sales'] <- pred.mean
View(test[test$pred.sales> 1 & test$pred.sales.log < 500,])


dtrain_cv[, 'pred.sales'] <- pred
train[train$Open == 1, 'pred.sales'] <- pred
dtrain_cv[, 'pred.sales.log'] <- as.integer(exp(pred) - 1)
train[train$Open == 1, 'pred.sales.log'] <- as.integer(exp(pred) - 1)
dtrain_cv[, 'pred.sales.log.by.store'] <- as.integer(exp(pred) - 1)
train[train$Open == 1 & train$Store == 13, 'pred.sales.log.by.store'] <- as.integer(exp(pred) - 1)

rmpse(dtrain_cv$pred.sales.log.by.store, train[train$Open == 1 & train$Store == 13, 'Sales'])


rmpse(test[test$Open == 1, 'pred.full.month.3'], test[test$Open == 1, 'BestPredictedSales'])

