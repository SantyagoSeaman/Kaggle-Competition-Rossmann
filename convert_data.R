# -----------
# Split dataset
train <- fullDataset[fullDataset$Id == -1,]
test <- fullDataset[fullDataset$Id != -1,]

train$SalesLog <- log(train$Sales + 1)

# -----------------
# For plots
rf.0.10 <- read.csv("C:/Users/Alexander/Dropbox/Work/Kaggle/Rossmann/data/rf.0.10.csv", stringsAsFactors=FALSE)
names(rf.0.10) <- c('Id', 'BestPredictedSales')
test <- left_join(test, rf.0.10[, 1:2], "Id")

# -----------
# calc averages
byYearMonth <- aggregate(Sales ~ Store + Date_year + Date_month + Promo,
                         #train[train$Open == 1 & train$StateHoliday == '0',],
                         train[train$Open == 1,],
                         mean)
names(byYearMonth) <- c('Store', 'Date_year', 'Date_month', 'Promo', 'MeanMonthSales')
byYearMonth$Date = as.Date(paste(byYearMonth$Date_year, byYearMonth$Date_month, '01', sep = '-'))

byYear <- aggregate(MeanMonthSales ~ Store + Date_year + Promo, byYearMonth[byYearMonth$Date_month %in% 1:7,], mean)
names(byYear) <- c('Store', 'Date_year', 'Promo', 'MeanYearSales')
# # HACK!!!
# zzz <- aggregate(Sales ~ Store + Promo, train[train$Date_year == 2013,], mean)
# #names(zzz) <- c("Store", "Promo", "FullMeanMonthSales")
# zzzz <- aggregate(Sales ~ Store + Promo, train[train$Date_year == 2013 & train$Date_month %in% 1:7,], mean)
# #names(zzzz) <- c("Store", "Promo", "SevenMonthMeanMonthSales")
# zzz$FullYearCoef <- zzz$Sales / zzzz$Sales
# byYear <- left_join(byYear, zzz[, c("Store", "Promo", "FullYearCoef")], c("Store", "Promo"))
# byYear[byYear$Date_year == 2015, 'MeanYearSales'] <- byYear[byYear$Date_year == 2015, 'MeanYearSales'] * byYear[byYear$Date_year == 2015, 'FullYearCoef']

# Сделать индивидуальный подсчёт по каждому магазину
# Сделать поправку для магазинов с пропущенными месяцами в 2014 году
byYearMonth <- left_join(x = byYearMonth, y = byYear[, c('Store', 'Date_year', 'MeanYearSales', 'Promo')], by = c('Store', 'Date_year', 'Promo'))
byYearMonth$MonthYearCoef <- byYearMonth$MeanMonthSales / byYearMonth$MeanYearSales


# средний месячный коэффициент сезонных колебаний за три года
meanMonthYearCoefByStore <- aggregate(MonthYearCoef ~ Store + Date_month + Promo, byYearMonth, mean)
names(meanMonthYearCoefByStore) <- c(names(meanMonthYearCoefByStore)[1:3], 'MeanMonthYearCoef')
byYearMonth <- left_join(byYearMonth, meanMonthYearCoefByStore, c('Store', 'Date_month', 'Promo'))


# ----------------------------------------------------------
train <- left_join(x = train,
                   y = byYearMonth[, c('Store', 'Date_year', 'Date_month', 'Promo', 'MeanMonthSales', 'MeanYearSales', 'MonthYearCoef', 'MeanMonthYearCoef')],
                   by = c('Store', 'Date_year', 'Date_month', 'Promo'))
#zzz <- train[is.na(train$MeanMonthSales) & train$Sales > 0,]
# Быстрый багфикс для 7 дней ряда магазинов. Они не будут учитываться при тренировке модели.
train[is.na(train$MeanMonthSales) & train$Sales > 0, 'Sales'] <- 0

# Предсказанное среднемесячное на основании среднегодичного и сезонного коэффициента
train$PredictedMeanMonthSales <- train$MeanYearSales * train$MeanMonthYearCoef
#zzz <- train[is.na(train$PredictedMeanMonthSales) & train$Sales > 0,]


test <- left_join(x = test,
                  y = meanMonthYearCoefByStore,
                  by = c('Store', 'Date_month', 'Promo'))
test <- left_join(x = test,
                  y = byYear[, c('Store', 'Date_year', 'Promo', 'MeanYearSales')],
                  by = c('Store', 'Date_year', 'Promo'))
test$PredictedMeanMonthSales <- test$MeanYearSales * test$MeanMonthYearCoef
#zzz <- test[is.na(test$PredictedMeanMonthSales),]


train$PredictedMeanMonthSalesLog <- log(train$PredictedMeanMonthSales + 1)
test$PredictedMeanMonthSalesLog <- log(test$PredictedMeanMonthSales + 1)
# ----------------------------------------------------------

View(test[test$Store == 8,])
View(train[train$Store == 8,])



# среднее количество продаж по дням недели
byDayOfWeek <- aggregate(Sales ~ Date_year + Date_month + DayOfWeek + Store + Promo,
                         train[train$Open == 1 & train$StateHoliday == '0',
                               c('Sales', 'Date_year', 'Date_month', 'DayOfWeek', 'Store', 'Promo')],
                         mean)
names(byDayOfWeek) <- c('Date_year', 'Date_month', 'DayOfWeek', 'Store', 'Promo', 'MeanSalesInDayOfWeek')
byDayOfWeek$Date <- as.Date(paste(byDayOfWeek$Date_year, byDayOfWeek$Date_month, '01', sep = '-'))
byDayOfWeek <- left_join(x = byDayOfWeek,
                         y = byYearMonth[, c('Date', 'Store', 'Promo', 'MeanMonthSales')],
                         by = c('Date', 'Store', 'Promo'))
byDayOfWeek$MeanSalesInDayOfWeekMonthCoef <- byDayOfWeek$MeanSalesInDayOfWeek / byDayOfWeek$MeanMonthSales
#byDayOfWeek[is.na(byDayOfWeek$MeanSalesInDayOfWeekMonthCoef), 'MeanSalesInDayOfWeekMonthCoef'] <- 1


byDayOfWeek.mean <- aggregate(MeanSalesInDayOfWeekMonthCoef ~ Store + Date_month + DayOfWeek + Promo,
                              byDayOfWeek,
                              mean)



StoreIds <- unique(train$Store)
monthYearSeq <- as.list(seq.Date(as.Date('2014-03-01'), as.Date('2015-07-01'), by = '1 month'))
trainDatesFilter <- train$Date >= as.Date('2014-03-01') & train$Date <= '2015-07-31'
for(d in monthYearSeq) {
    print(format(d, "%Y-%m-%d"))

    year <- as.integer(format(d, "%Y"))
    month <- as.integer(format(d, "%m"))
    d.prev <- as.Date(paste(year-1, month, '01', sep = '-'))
    d.prev.2 <- as.Date(paste(year-2, month, '01', sep = '-'))

    # ====================================================================================
    # Среднее количество всех продаж за предыдущие 4 месяцев
    curYearDateSeq <- rev(seq.Date(d, by = '-1 month', length.out = 4)[-1])
    curYearMonthsSales <- left_join(data.frame(Store = StoreIds),
                                    aggregate(MeanMonthSales ~ Store, byYearMonth[byYearMonth$Date %in% curYearDateSeq, ], mean),
                                    by = c('Store'))
    naStores <- curYearMonthsSales[is.na(curYearMonthsSales$MeanMonthSales), 'Store']
    if (length(naStores) > 0) {
        curYearDateSeq <- rev(seq.Date(d, by = '-1 month', length.out = 12)[-1])
        curYearMonthsSales[curYearMonthsSales$Store %in% naStores, 'MeanMonthSales'] <- aggregate(MeanMonthSales ~ Store,
                                                                                                  byYearMonth[byYearMonth$Date %in% curYearDateSeq & byYearMonth$Store %in% naStores, ],
                                                                                                  mean)['MeanMonthSales']
    }

    # Среднее количество всех продаж предыдущие 4 месяцев предыдущего года
    prevYearDateSeq <- rev(seq.Date(d.prev, by = '-1 month', length.out = 4)[-1])
    prevYearMonthsSales <- aggregate(MeanMonthSales ~ Store,
                                     byYearMonth[byYearMonth$Date %in% prevYearDateSeq, ],
                                     mean)

    # Коэффициент роста/падения продаж на основании последних 4 месяцев по сравнению с предыдущим годом
    yearCoefByStore <- curYearMonthsSales[, c('Store', 'MeanMonthSales')]
    yearCoefByStore <- left_join(yearCoefByStore,
                                 prevYearMonthsSales[, c('Store', 'MeanMonthSales')],
                                 c('Store'))
    names(yearCoefByStore) <- c('Store', 'MeanMonthSales.cur', 'MeanMonthSales.prev')
    yearCoefByStore$YearCoefByStore = yearCoefByStore$MeanMonthSales.cur / yearCoefByStore$MeanMonthSales.prev
    yearCoefByStore[is.na(yearCoefByStore$YearCoefByStore), 'YearCoefByStore'] <- 1

    # ====================================================================================
    # Среднемесячное количество продаж за текущий месяц в прошлом году
    prevYearMeanCurMonthSales <- unique(byYearMonth[byYearMonth$Date == d, c('Store', 'Promo')])
    prevYearMeanCurMonthSales <- left_join(prevYearMeanCurMonthSales,
                                           byYearMonth[byYearMonth$Date == d.prev, ],
                                           c('Store', 'Promo'))
    prevYearMeanCurMonthSales <- left_join(prevYearMeanCurMonthSales,
                                        yearCoefByStore[, c('Store', 'YearCoefByStore')],
                                        'Store')

    # Для тех магазинов, для которых нет данных в предыдущем году, берём из позапрошлого года
    prevYearMeanCurMonthSales.names <- names(prevYearMeanCurMonthSales)
    prevYearMeanCurMonthSales[is.na(prevYearMeanCurMonthSales$MeanMonthSales),
                              prevYearMeanCurMonthSales.names[1:(length(prevYearMeanCurMonthSales.names)-1)]
                              ] <- left_join(prevYearMeanCurMonthSales[is.na(prevYearMeanCurMonthSales$MeanMonthSales), c('Store', 'Promo')],
                                             byYearMonth[byYearMonth$Date == d.prev.2, ],
                                             c('Store', 'Promo'))

    #prevYearMeanMonthSales[is.na(prevYearMeanMonthSales$YearCoefByStore), ]
    # Умножаем на годичный коэффициент и получаем среднемесячные продажи в текущем году
    prevYearMeanCurMonthSales$CurrentYearMeanCurMonthSales <- prevYearMeanCurMonthSales$MeanMonthSales * prevYearMeanCurMonthSales$YearCoefByStore
    #prevYearMeanMonthSales[is.na(prevYearMeanMonthSales$CurrentYearMeanMonthSales), ]


#     # Коэффициенты по дням недели без учёта промо за предыдущие 6 месяцев текущего года
#     curYearSalesCoefByDayOfWeek <- aggregate(MeanSalesInDayOfWeekMonthCoef ~ Store + DayOfWeek + Promo,
#                                              byDayOfWeek[byDayOfWeek$Date %in% curYearDateSeq, c('Store', 'Promo', 'DayOfWeek', 'MeanSalesInDayOfWeekMonthCoef')],
#                                              mean)

    # Усреднённые коэффициенты по дням недели
    meanYearSalesCoefByDayOfWeek <- aggregate(MeanSalesInDayOfWeekMonthCoef ~ Store + DayOfWeek + Promo,
                                             byDayOfWeek[byDayOfWeek$Date_month == month, c('Store', 'Promo', 'DayOfWeek', 'MeanSalesInDayOfWeekMonthCoef')],
                                             mean)

    # join с среднемесячными продажами
    curYearSalesCoefByDayOfWeek <- left_join(meanYearSalesCoefByDayOfWeek,
                                             prevYearMeanCurMonthSales[, c('Store', 'Promo', 'CurrentYearMeanCurMonthSales')],
                                             c('Store', 'Promo'))
    curYearSalesCoefByDayOfWeek$MeanSalesInDayOfWeek <- curYearSalesCoefByDayOfWeek$CurrentYearMeanCurMonthSales * curYearSalesCoefByDayOfWeek$MeanSalesInDayOfWeekMonthCoef

    # получаем данные по текущему месяцу
    trainIndex <- train$Date_year == year & train$Date_month == month & train$Open == 1
    trainMonth <- left_join(x = train[trainIndex, c('Date', 'Store', 'DayOfWeek', 'Promo')],
                            y = curYearSalesCoefByDayOfWeek[, c('Store', 'DayOfWeek', 'Promo',
                                                                'CurrentYearMeanCurMonthSales', 'MeanSalesInDayOfWeek', 'MeanSalesInDayOfWeekMonthCoef')],
                            by = c('Store', 'DayOfWeek', 'Promo'))
    # Вроде все NA за воскресенье...
    #zzz <- trainMonth[is.na(trainMonth$MeanSalesInDayOfWeek) & trainMonth$DayOfWeek != 7, 'MeanSalesInDayOfWeek']
    trainMonth[is.na(trainMonth$MeanSalesInDayOfWeek), 'MeanSalesInDayOfWeek'] <- 0
    trainMonth[is.na(trainMonth$CurrentYearMeanCurMonthSales), 'CurrentYearMeanCurMonthSales'] <- 0

    train[trainIndex, 'MeanSalesInDayOfWeek'] <- trainMonth$MeanSalesInDayOfWeek
    train[trainIndex, 'CurrentYearMeanCurMonthSales'] <- trainMonth$CurrentYearMeanCurMonthSales
}

train[is.na(train$MeanSalesInDayOfWeek), 'MeanSalesInDayOfWeek'] <- 0
train$MeanSalesInDayOfWeekLog <- log(train$MeanSalesInDayOfWeek + 1)
train[is.na(train$CurrentYearMeanCurMonthSales), 'CurrentYearMeanCurMonthSales'] <- 0
train$CurrentYearMeanCurMonthSalesLog <- log(train$CurrentYearMeanCurMonthSales + 1)

# It's a magic!
#train$MeanSalesInDayOfWeekEjection <- abs(train$MeanSalesInDayOfWeek - train$Sales)/train$MeanSalesInDayOfWeek > 0.3
#train[is.na(train$MeanSalesInDayOfWeekEjection), 'MeanSalesInDayOfWeekEjection'] <- F
#View(train[trainDatesFilter & train$MeanSalesInDayOfWeekEjection == T,])
#train$MeanSalesInDayOfWeekSuperEjection <- abs(train$MeanSalesInDayOfWeek - train$Sales)/train$MeanSalesInDayOfWeek > 0.5
#train[is.na(train$MeanSalesInDayOfWeekSuperEjection), 'MeanSalesInDayOfWeekSuperEjection'] <- F
#View(train[trainDatesFilter & train$MeanSalesInDayOfWeekSuperEjection == T,])


View(train[train$Store == 1, c(names(train), 'Sales', 'Promo')])


for(d in list(as.Date('2015-08-01'), as.Date('2015-09-01'))) {
    print(format(d, "%Y-%m-%d"))

    year <- as.integer(format(d, "%Y"))
    month <- as.integer(format(d, "%m"))
    d.prev <- as.Date(paste(year-1, month, '01', sep = '-'))
    d.prev.2 <- as.Date(paste(year-2, month, '01', sep = '-'))


    # ====================================================================================
    # Среднемесячное количество продаж за текущий месяц в прошлом году
    prevYearMeanCurMonthSales <- unique(test[test$Date_month == month, c('Store', 'Promo')])
    prevYearMeanCurMonthSales <- left_join(prevYearMeanCurMonthSales,
                                           byYearMonth[byYearMonth$Date == d.prev, ],
                                           c('Store', 'Promo'))
    prevYearMeanCurMonthSales <- left_join(prevYearMeanCurMonthSales,
                                           yearCoefByStore[, c('Store', 'YearCoefByStore')],
                                           'Store')

    prevYearMeanCurMonthSales.names <- names(prevYearMeanCurMonthSales)
    prevYearMeanCurMonthSales[is.na(prevYearMeanCurMonthSales$MeanMonthSales),
                              prevYearMeanCurMonthSales.names[1:(length(prevYearMeanCurMonthSales.names)-1)]
                              ] <- left_join(prevYearMeanCurMonthSales[is.na(prevYearMeanCurMonthSales$MeanMonthSales), c('Store', 'Promo')],
                                             byYearMonth[byYearMonth$Date == d.prev.2, ],
                                             c('Store', 'Promo'))
    # Умножаем на годичный коэффициент и получаем среднемесячные продажи в текущем году
    prevYearMeanCurMonthSales$CurrentYearMeanCurMonthSales <- prevYearMeanCurMonthSales$MeanMonthSales * prevYearMeanCurMonthSales$YearCoefByStore
    # Усреднённые коэффициенты по дням недели
    meanYearSalesCoefByDayOfWeek <- aggregate(MeanSalesInDayOfWeekMonthCoef ~ Store + DayOfWeek + Promo,
                                              byDayOfWeek[byDayOfWeek$Date_month == month, c('Store', 'Promo', 'DayOfWeek', 'MeanSalesInDayOfWeekMonthCoef')],
                                              mean)

    # join с среднемесячными продажами
    curYearSalesCoefByDayOfWeek <- left_join(prevYearMeanCurMonthSales[, c('Store', 'Promo', 'CurrentYearMeanCurMonthSales')],
                                             meanYearSalesCoefByDayOfWeek,
                                             c('Store', 'Promo'))
    curYearSalesCoefByDayOfWeek$MeanSalesInDayOfWeek <- curYearSalesCoefByDayOfWeek$CurrentYearMeanCurMonthSales * curYearSalesCoefByDayOfWeek$MeanSalesInDayOfWeekMonthCoef

    # получаем данные по текущему месяцу
    testIndex <- test$Date_year == year & test$Date_month == month & test$Open == 1
    testMonth <- left_join(x = test[testIndex, c('Date', 'Store', 'DayOfWeek', 'Promo')],
                           y = curYearSalesCoefByDayOfWeek[, c('Store', 'DayOfWeek', 'Promo',
                                                               'CurrentYearMeanCurMonthSales', 'MeanSalesInDayOfWeek', 'MeanSalesInDayOfWeekMonthCoef')],
                           by = c('Store', 'DayOfWeek', 'Promo'))
    testMonth[is.na(testMonth$MeanSalesInDayOfWeek), 'MeanSalesInDayOfWeek'] <- 0
    testMonth[is.na(testMonth$CurrentYearMeanCurMonthSales), 'CurrentYearMeanCurMonthSales'] <- 0

    test[testIndex, 'MeanSalesInDayOfWeek'] <- testMonth$MeanSalesInDayOfWeek
    test[testIndex, 'CurrentYearMeanCurMonthSales'] <- testMonth$CurrentYearMeanCurMonthSales
}

test[is.na(test$MeanSalesInDayOfWeek), 'MeanSalesInDayOfWeek'] <- 0
test$MeanSalesInDayOfWeekLog <- log(test$MeanSalesInDayOfWeek + 1)
test[is.na(test$CurrentYearMeanCurMonthSales), 'CurrentYearMeanCurMonthSales'] <- 0
test$CurrentYearMeanCurMonthSalesLog <- log(test$CurrentYearMeanCurMonthSales + 1)

#test$MeanSalesInDayOfWeekEjection <- F
#test$MeanSalesInDayOfWeekSuperEjection <- F

View(test[test$Store == 1, ])










zzzTrain <- trainMonth[trainMonth$Store == 2, ]
zzzTrain <- zzzTrain[order(as.Date(zzzTrain$Date)),]
View(zzzTrain)

zzzTrain <- train[train$Store == 28, ]
zzzTrain <- fullDataset[fullDataset$Store == 28, ]

zzzTrain <- train2[train2$Store == 28, ]

zzzTrain <- train[train$Store == 1 & trainDatesFilter, c(names(train), 'Sales')]
zzzTrain <- train[train$Store == 68, c(names(train), 'Sales')]
zzzTrain <- zzzTrain[order(as.Date(zzzTrain$Date)),]
View(zzzTrain)

zzzTest <- test[test$Store == 102,]
zzzTest <- test[test$Store == 68, c('Date', 'DayOfWeek', 'Open', 'Promo', 'StateHoliday', 'SchoolHoliday', 'FinalMeanSalesInDayOfWeek',
                                    'pred.sales.log.by.store.1', 'pred.sales.log.by.store.2', 'pred.sales.log.by.store.3',
                                    'pred.sales.log.by.store.4', 'pred.sales.log.by.store.5', 'pred.sales.log.by.store.6', 'pred.sales.log.by.store.7')]
zzzTest <- zzzTest[order(as.Date(zzzTest$Date)),]
View(zzzTest)




zzzTrain <- train[train$Date == '2013-07-31', c(names(train), 'Sales')]
zzzTrain <- zzzTrain[order(as.Date(zzzTrain$Date)),]
View(zzzTrain)


fields <- c('Date', 'DayOfWeek', 'Sales',
            'Customers', 'Open', 'Promo', 'StateHoliday', 'SchoolHoliday')
zzz <- train[train$Date_year == 2013 & train$Store == 102, fields]
zzz <- zzz[order(zzz$Date),]
zzz2 <- train[train$Date_year == 2014 & train$Store == 102, fields]
zzz2 <- zzz2[order(zzz2$Date),]
names(zzz2) <- paste(fields, '_2', sep = '')
zzz3 <- cbind(zzz, zzz2)
