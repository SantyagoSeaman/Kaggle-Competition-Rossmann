feature.names <- c('Date_month', 'Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'MeanSalesInDayOfWeek')
feature.formula <- formula(paste('Sales ~ ', paste(feature.names, collapse = ' + '), sep = ''))

feature.names <- c('Date_month', 'Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'MeanSalesInDayOfWeekLog', 'CurrentYearMeanCurMonthSalesLog', 'PredMeanMonthSalesLog')

feature.names <- c('Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'MeanSalesInDayOfWeekLog')

feature.names <- c('Date_year', 'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek', 'Date_monthday',
                   'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month',
                   'PredictedMeanMonthSalesLog')

feature.formula <- formula(paste('SalesLog ~ ', paste(feature.names, collapse = ' + '), sep = ''))


n_rounds = 2001
params <- list(booster = "gbtree", objective = "reg:linear", eval_metric = 'rmse',
               max_depth = 5, eta = 0.01, colsample_bytree = 0.9, subsample = 0.8)
#trainDatesFilter <- train$Date >= as.Date('2014-07-01') & train$Date <= '2015-07-31'
trainDatesFilter <- train$Date >= as.Date('2013-01-01') & train$Date <= '2015-07-31'
StoreIds <- unique(train$Store)
for(StoreId in StoreIds[1:length(StoreIds)]) {
    dtrain_cv <- train[train$Open == 1 & train$Store == StoreId & trainDatesFilter, c(feature.names, 'Sales', 'SalesLog')]
    dfull <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv), label = dtrain_cv$SalesLog)

    bst.cv <- xgb.cv(params, dfull, n_rounds, nfold = 5, feval = rmpse.feval,
                     print.every.n = 200, prediction = TRUE)
    min.rmse = min(bst.cv$dt[, test.RMPSE.mean])
    min.rmse.idx = which.min(bst.cv$dt[, test.RMPSE.mean])
    result.rmpse = rmpse(exp(bst.cv$pred)-1, dtrain_cv$Sales)
    print(paste(StoreId, ':', min.rmse, min.rmse.idx, result.rmpse))


}

