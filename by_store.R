feature.names <- c('DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday', 'StoreType', 'Assortment',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday', 'MeanSalesInDay')

feature.names <- c('DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday', 'StoreType', 'Assortment',
                   'Date_month', 'Date_year', 'MeanMonthSalesLog', 'MeanYearSalesLog', 'PrevDayStateHoliday',
                   'WorkInHoliday', 'MeanSalesInDayOfWeekLog')

feature.names <- c('Promo', 'StateHoliday', 'SchoolHoliday', 'StoreType', 'Assortment',
                   'PrevDayStateHoliday',
                   'WorkInHoliday', 'MeanSalesInDayOfWeekLog')

feature.names <- c('DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday', 'StoreType', 'Assortment',
                   'Date_month', 'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'MeanSalesInDayOfWeek', 'MeanMonthSales', 'MeanYearSales')

feature.names <- c('Date_year', 'Date_month', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'FinalMeanSalesInDayOfWeekLog', 'MeanMonthSalesLog', 'MeanYearSalesLog', 'CurrentYearMeanMonthSalesLog')
feature.names <- c('Date_year', 'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'Date_yearday', 'Date_monthday',
                   'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'FinalMeanSalesInDayOfWeekLog')

feature.names <- c('Date_month', 'Date_monthweek', 'Date_monthday',
                   'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'MeanSalesInDayOfWeekLog')

feature.names <- c('Date_month', 'Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CurrentYearMeanCurMonthSalesLog', 'PredMeanMonthSalesLog', 'MeanSalesInDayOfWeekLog')

feature.names <- c('Date_month', 'Date_monthweek', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'MeanSalesInDayOfWeekLog',
                   'MeanSalesInDayOfWeekEjection', 'MeanSalesInDayOfWeekSuperEjection')

feature.names <- c('Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek',
                   'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month')
                   #'PredictedMeanMonthSalesLog',
                   #'MeanSalesInDayOfWeekEjection', 'MeanSalesInDayOfWeekSuperEjection')

feature.names <- c('Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek',
                   'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month',
                   'PredictedMeanMonthSalesLog')

feature.names <- c('Store', 'StoreType', 'Assortment',
                   'Date_year', 'Date_month', 'Date_yearday', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek', 'Date_monthday',
                   #'Date_year', 'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek',
                   'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month')

feature.formula <- formula(paste('SalesLog ~ ', paste(feature.names, collapse = ' + '), sep = ''))


trainDatesFilter <- train$Date >= as.Date('2013-01-01') & train$Date <= '2015-07-31'
#trainDatesFilter <- train$Date >= as.Date('2014-03-01') & train$Date <= '2015-07-31'
params <- list(booster = "gbtree", objective = "reg:linear",
               max_depth = 6, eta = 0.01, colsample_bytree = 1, subsample = 1)
StoreIds <- unique(train$Store)
errors <- list()
resultColNamePattern <- 'predSales.'
for(StoreId in StoreIds[1:length(StoreIds)]) {
#for(StoreId in StoreIds[1:500]) {

    dtrain_cv <- train[train$Open == 1 & train$Store == StoreId & trainDatesFilter, c(feature.names, 'Sales', 'SalesLog')]
    dfull <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv),
                         label = dtrain_cv$SalesLog)
    dtest_cv <- test[test$Open == 1 & test$Store == StoreId, feature.names]
    if (nrow(dtest_cv) > 0) {
        dtest_cv$SalesLog = 0
        dtest <- sparse.model.matrix(feature.formula, data = dtest_cv)
        sparseMatrixColNamesTest <- colnames(dtest)
    }

    print('============================================================================')
    print(paste0(StoreId, ' - ', nrow(dtrain_cv), ' rows'))

    # Calc nrounds with CV
    bst.cv <- xgb.cv(params, dfull, nrounds = 2001, nfold = 5, feval = rmpse.feval,
                     print.every.n = 200, prediction = TRUE, verbose = T)
    min.rmse = min(bst.cv$dt[, test.RMPSE.mean])
    min.rmse.idx = which.min(bst.cv$dt[, test.RMPSE.mean])
    cv.rmpse = rmpse(exp(bst.cv$pred)-1, dtrain_cv$Sales)
    print(paste('CV min:', round(min.rmse, 4), min.rmse.idx,
                ', CV full rmpse:', round(cv.rmpse, 4)))

    store.errors <- c()
    for(j in 1:20) {
        resultColName = paste0(resultColNamePattern, j)

        indexes <- sample(seq_len(nrow(dtrain_cv)), floor(nrow(dtrain_cv)*0.9))
        data <- sparse.model.matrix(feature.formula, data = dtrain_cv[indexes, ])
        sparseMatrixColNamesTrain <- colnames(data)
        dtrain <- xgb.DMatrix(data, label = dtrain_cv[indexes, 'SalesLog'])
        dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = dtrain_cv[-indexes, ]),
                              label = dtrain_cv[-indexes, 'SalesLog'])

        print(paste0(' -- ', j))

        # Train
        n_rounds = min.rmse.idx + 100
        #watchlist <- list(eval = dvalid, train = dfull)
        model <- xgb.train(params = params, data = dtrain,
                           nrounds = n_rounds, feval = rmpse.feval)#,
                           #watchlist = watchlist, print.every.n = 200)
        #feature.importance <- xgb.importance(sparseMatrixColNamesTrain, model = model)

        pred.full <- as.integer(exp(predict(model, dfull)) - 1)
        #pred.valid <- as.integer(exp(predict(model, dvalid)) - 1)

        train[train$Open == 0 & train$Store == StoreId & trainDatesFilter, resultColName] <- 0
        train[train$Open == 1 & train$Store == StoreId & trainDatesFilter, resultColName] <- pred.full

        #errValid <- rmpse(pred.valid, dtrain_cv[-indexes, 'Sales'])
        errFull <- rmpse(pred.full, dtrain_cv[, 'Sales'])
        print(paste(#'Valid rmpse: ', round(errValid, 4),
                    ', Full rmpse: ', round(errFull, 4)
        ))
        #store.errors <- c(store.errors, errValid)

        if (nrow(dtest_cv) > 0) {
            test[test$Open == 0 & test$Store == StoreId, resultColName] <- 0
            test[test$Open == 1 & test$Store == StoreId, resultColName] <- as.integer(exp(predict(model, dtest)) - 1)
        }
    }

    #errors <- rbind(errors, store.errors)

    #gc()
}


rmpse(train[trainDatesFilter, 'pred.sales.log.by.store.1'], train[trainDatesFilter, 'Sales'])

errors[errors > 0.1]
zzz <- which(errors > 0.2)
zzzTrain <- train[trainDatesFilter & train$Store %in% zzz, c(names(train), 'Sales')]
zzzTrain <- zzzTrain[order(zzzTrain$Date, decreasing = T), ]
View(zzzTrain)
table(zzzTrain$Promo)

