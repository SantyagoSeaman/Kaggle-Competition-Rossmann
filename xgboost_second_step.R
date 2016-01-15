tranNames <- names(train)
# feature.names <- c('Store', 'StoreType', 'Assortment',
#                    'Date_month', 'Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
#                    'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
#                    'MeanSalesInDayOfWeekLog',
#                    'MeanSalesInDayOfWeekEjection', 'MeanSalesInDayOfWeekSuperEjection')
# feature.names <- c('Store', 'StoreType', 'Assortment',
#                    'Date_month', 'Date_week', 'Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
#                    'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
#                    'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month',
#                    'MeanSalesInDayOfWeekLog', 'CurrentYearMeanCurMonthSalesLog', 'PredMeanMonthSalesLog',
#                    'MeanSalesInDayOfWeekEjection', 'MeanSalesInDayOfWeekSuperEjection')
# feature.names <- c(feature.names, tranNames[grep('pred[.]full[.]([0-9]{1,2})', tranNames)])
feature.names <- tranNames[grep('pred[.]full[.]([0-9]{1,2})', tranNames)]
feature.formula <- formula(paste('Sales ~ ', paste(feature.names, collapse = ' + '), sep = ''))


trainDatesFilter <- train$Date >= as.Date('2014-03-01') & train$Date <= '2015-07-31'

dtest_cv <- test[test$Open == 1, feature.names]
dtest_cv$Sales = 0
dtest <- sparse.model.matrix(feature.formula, data = dtest_cv)
sparseMatrixColNamesTest <- colnames(dtest)

for(i in 1:5) {
    print(i)
    resultColName = paste0('pred.full.second.', i)

    dtrain_cv <- train[train$Open == 1 & trainDatesFilter, c(feature.names, 'Sales')]
    indexes <- sample(seq_len(nrow(dtrain_cv)), floor(nrow(dtrain_cv)*0.8))
    data <- sparse.model.matrix(feature.formula, data = dtrain_cv[indexes, ])
    sparseMatrixColNamesTrain <- colnames(data)
    dtrain <- xgb.DMatrix(data, label = dtrain_cv[indexes, 'Sales'])
    dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv[-indexes, ]),
                          label = dtrain_cv[-indexes, 'Sales'])
    dfull <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv),
                         label = dtrain_cv$Sales)

    n_rounds = 201
    watchlist <- list(eval = dvalid, train = dtrain, main = dfull)
    params <- list(booster = "gbtree", objective = "reg:linear", eval_metric = 'rmse',
                   max_depth = 10, eta = 0.05, colsample_bytree = 0.75, subsample = 0.75)

    model <- xgb.train(params = params, data = dtrain, nrounds = n_rounds, watchlist = watchlist, print.every.n = 20)
    #feature.importance <- xgb.importance(sparseMatrixColNamesTrain, model = model)

    pred <- predict(model, dfull)

    train[, resultColName] <- 0
    train[train$Open == 1 & trainDatesFilter, resultColName] <- as.integer(pred)
    rmpse(train[train$Open == 1 & trainDatesFilter, resultColName], train[train$Open == 1 & trainDatesFilter, 'Sales'])


    pred <- predict(model, dtest)
    test[, resultColName] <- 0
    test[test$Open == 1, resultColName] <- as.integer(pred)
}

View(train[trainDatesFilter, c('Sales', 'pred.full.second.2')])
rmpse(train[trainDatesFilter, 'pred.full.second.3'], train[trainDatesFilter, 'Sales'])

View(test[, c('pred.full.second.1', 'pred.full.second.2', 'pred.full.second.3', 'pred.full.second.4', 'pred.full.second.5')])

