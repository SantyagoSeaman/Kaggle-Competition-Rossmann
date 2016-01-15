feature.names <- c('Store', 'StoreType', 'Assortment',
                   'Date_month', 'Date_week', 'Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month',
                   'MeanSalesInDayOfWeekLog', 'CurrentYearMeanCurMonthSalesLog', 'PredMeanMonthSalesLog',
                   'MeanSalesInDayOfWeekEjection', 'MeanSalesInDayOfWeekSuperEjection')

feature.names <- c('Store', 'StoreType', 'Assortment',
                   'Date_month', 'Date_week', 'Date_monthweek', 'DayOfWeek',
                   'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month')

feature.formula <- formula(paste('SalesLog ~ ', paste(feature.names, collapse = ' + '), sep = ''))


trainDatesFilter <- train$Date >= as.Date('2013-01-01') & train$Date <= '2015-07-31'

dtest_cv <- test[, feature.names]
dtest_cv$SalesLog = 0
dtest_cv[is.na(dtest_cv)]<- 0
dtest <- sparse.model.matrix(feature.formula, data = dtest_cv)
sparseMatrixColNamesTest <- colnames(dtest)

for(i in 1:10) {
    print(i)
    resultColName = paste0('pred.full.short.', i)

    dtrain_cv <- train[trainDatesFilter, c(feature.names, 'Sales', 'SalesLog')]
    indexes <- sample(seq_len(nrow(dtrain_cv)), floor(nrow(dtrain_cv)*0.8))
    data <- sparse.model.matrix(feature.formula, data = dtrain_cv[indexes, ])
    sparseMatrixColNamesTrain <- colnames(data)
    dtrain <- xgb.DMatrix(data, label = dtrain_cv[indexes, 'SalesLog'])
    dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv[-indexes, ]),
                          label = dtrain_cv[-indexes, 'SalesLog'])
    dfull <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv),
                         label = dtrain_cv$SalesLog)

    #n_rounds = 1001 + 200*i
    n_rounds = 501
    watchlist <- list(eval = dvalid, train = dtrain, main = dfull)
    params <- list(booster = "gbtree", objective = "reg:linear", eval_metric = 'rmse',
                   max_depth = 10, eta = 0.3, colsample_bytree = 0.85, subsample = 0.75)

    model <- xgb.train(params = params, data = dtrain, nrounds = n_rounds, watchlist = watchlist, print.every.n = 100)
    #feature.importance <- xgb.importance(sparseMatrixColNamesTrain, model = model)

    pred <- predict(model, dfull)

    train[, resultColName] <- 0
    train[trainDatesFilter, resultColName] <- as.integer(exp(pred) - 1)
    train[trainDatesFilter & train$resultColName < 100, resultColName] <- 0
    rmpse(train[trainDatesFilter, resultColName], train[trainDatesFilter, 'Sales'])


    pred <- predict(model, dtest)
    test[, resultColName] <- 0
    test[, resultColName] <- as.integer(exp(pred) - 1)
    test[test$resultColName < 100, resultColName] <- 0
}




pred.mean <- apply(pred, 1, function(r) mean(unlist(r)))
pred.mean <- as.integer(exp(pred.mean) - 1)

train$pred.sales.log <- as.integer(exp(pred) - 1)
View(train[train$pred.sales.log > 1 & train$pred.sales.log < 200, c('Sales', 'pred.sales.log')])
View(train[train$pred.sales.log > 1 & train$pred.sales.log < 500,])
rmpse(train$pred.sales.log, train$Sales)

test$pred.sales <- 0
test[test$Open == 1, 'pred.sales'] <- pred.mean
View(test[test$pred.sales> 1 & test$pred.sales.log < 500,])
