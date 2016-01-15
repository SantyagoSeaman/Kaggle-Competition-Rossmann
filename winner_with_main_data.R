train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

train <- train[train$Sales > 0 & train$Open == 1, ]

feature.names <- names(train)
for (f in feature.names) {
    if (class(train[[f]])=="character") {
        levels <- unique(c(train[[f]], test[[f]]))
        train[[f]] <- as.integer(factor(train[[f]], levels=levels))
        test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    } else {
        if (class(train[[f]])=="factor") {
            train[[f]] <- as.integer(train[[f]])
            test[[f]]  <- as.integer(test[[f]])
        }
    }
}


feature.names <- c('Store', 'StoreType', 'Assortment',
                   'Date_year', 'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek', 'Date_monthday',
                   #'Date_year', 'Date_month', 'Date_weekOfYear', 'Date_monthweek', 'DayOfWeek',
                   'Promo', 'StateHoliday', 'SchoolHoliday',
                   'PrevDayStateHoliday', 'PrevDayPromo', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'CompetitionDistance', 'Promo2', 'Promo2Open', 'IsPromo2Month',
                   "CompetitionOpenSinceYear", "Promo2SinceYear")

tra<-train[, feature.names]

for (i in 1:10) {
    h<-sample(nrow(train),20000)

    dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(train$Sales+1)[h])
    dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(train$Sales+1)[-h])
    watchlist<-list(val=dval,train=dtrain)
    param <- list(  objective           = "reg:linear",
                    booster             = "gbtree",
                    eta                 = 0.02, # 0.06, #0.01,
                    max_depth           = 10, #changed from default of 8
                    subsample           = 0.9, # 0.7
                    colsample_bytree    = 0.7 # 0.7
                    #num_parallel_tree   = 2
                    # alpha = 0.0001,
                    # lambda = 1
    )

    n <- 3001
    clf <- xgb.train(   params              = param,
                        data                = dtrain,
                        nrounds             = n,
                        verbose             = 1,
                        early.stop.round    = 100,
                        watchlist           = watchlist,
                        print.every.n       = 200,
                        maximize            = FALSE,
                        feval=RMPSE
    )
}

#feature.importance.5 <- xgb.importance(colnames(dval), model = clf)

predFull1 <- exp(predict(clf, data.matrix(test[,feature.names]))) -1
submission <- data.frame(Id=test$Id, Sales=predFull1)
cat("saving the submission file\n")
write_csv(submission, "predFull1.csv")


rmpse(predFull1, test$BestPredictedSales)
