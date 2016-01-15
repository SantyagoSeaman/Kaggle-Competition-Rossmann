library(data.table)
library(caret)
library(dplyr)


# победитель на данный момент
# xg_16_07_01_best_zero.csv - 0.10360


test$PromoIntervalList <- NULL
train$PromoIntervalList <- NULL
test$PromoIntervalListInt <- NULL
train$PromoIntervalListInt <- NULL

# xgboost и 6 предсказаний по алгоритму победителя с выходом rf1-6.csv
# среднее по 6 предсказаниям и победителю дало 0.10389
saveRDS(test, file="saved data/test full month and winner.Rda")
saveRDS(train, file="saved data/train full month.Rda")

# random forest by store с средним 0.10612
saveRDS(test, file="saved data/test rf by store and full month.Rda")
saveRDS(train, file="saved data/train rf by store and full month.Rda")

# среднее дало 0.10612
saveRDS(testOpened, file="saved data/test open pred folded.Rda")
saveRDS(trainOpened, file="saved data/train open pred folded.Rda")

saveRDS(test, file="saved data/test winner 8.Rda")
saveRDS(train, file="saved data/train winner 8.Rda")

# predWinnerThird_4 valid [1920]	val-RMPSE:0.0916012623879874	train-RMPSE:0.0673077845993437
# predWinnerThird_9 valid [2060]	val-RMPSE:0.091979863050366	train-RMPSE:0.0643233670564083
saveRDS(test, file="saved data/test winner third 12.Rda")
saveRDS(train, file="saved data/train winner third 12.Rda")

saveRDS(test, file="saved data/test winner holiday.Rda")
saveRDS(train, file="saved data/train winner holiday.Rda")

saveRDS(test, file="saved data/test h2o.Rda")

# ----------------------------------------------------------------------
test1 <- readRDS(file="saved data/test full month and winner.Rda")
train1 <- readRDS(file="saved data/train full month.Rda")

test2 <- readRDS(file="saved data/test rf by store and full month.Rda")
train2 <- readRDS(file="saved data/train rf by store and full month.Rda")

testOpened <- readRDS(file="saved data/test open pred folded.Rda")
trainOpened <- readRDS(file="saved data/train open pred folded.Rda")

test3 <- readRDS(file="saved data/test winner 8.Rda")
train3 <- readRDS(file="saved data/train winner 8.Rda")

test4 <- readRDS(file="saved data/test winner third 12.Rda")
train4 <- readRDS(file="saved data/train winner third 12.Rda")

test5 <- readRDS(file="saved data/test winner holiday.Rda")
train5 <- readRDS(file="saved data/train winner holiday.Rda")

test6 <- readRDS(file="saved data/test h2o.Rda")

# ----------------------------------------------------------------------
train <- train1[, c('Store', 'DayOfWeek', 'Date', 'Open', 'Promo',
                    'Date_month', 'Date_year', 'Date_weekOfYear', 'Date_yearday', 'Date_monthday',
                    'PredictedMeanMonthSales', 'Sales')]
test <- test1[, c('Id', 'Store', 'DayOfWeek', 'Date', 'Open', 'Promo',
                  'Date_month', 'Date_year', 'Date_weekOfYear', 'Date_yearday', 'Date_monthday',
                  'PredictedMeanMonthSales', 'BestPredictedSales')]

fieldNames <- names(train1)
fieldNames <- fieldNames[grep('pred[.]full[.]month[.]([0-9]{1,2})', fieldNames)]
train <- left_join(train, train1[, c('Store', 'Date', fieldNames)], by = c('Store', 'Date'))
test <- left_join(test, test1[, c('Id', fieldNames)], by = c('Id'))

fieldNames <- names(train2)
fieldNames <- c(fieldNames[grep('rf_([0-9]{1,2})', fieldNames)],
                fieldNames[grep('predFullDate_([0-9]{1,2})', fieldNames)])
train <- left_join(train, train2[, c('Store', 'Date', fieldNames)], by = c('Store', 'Date'))
test <- left_join(test, test2[, c('Id', fieldNames)], by = c('Id'))


fieldNames <- names(trainOpened)
fieldNames <- fieldNames[grep('predFolded([0-9]{1,2})', fieldNames)]
train <- left_join(train, trainOpened[, c('Store', 'Date_year', 'Date_yearday', fieldNames)], by = c('Store', 'Date_year', 'Date_yearday'))
test <- left_join(test, testOpened[, c('Id', fieldNames)], by = c('Id'))


fieldNames <- names(train3)
fieldNames <- fieldNames[grep('predWinnerSecond_([0-9]{1,2})', fieldNames)]
train <- left_join(train, train3[, c('Store', 'Date', fieldNames)], by = c('Store', 'Date'))
test <- left_join(test, test3[, c('Id', fieldNames)], by = c('Id'))

fieldNames <- names(train4)
fieldNames <- fieldNames[grep('predWinnerThird_([0-9]{1,2})', fieldNames)]
train <- left_join(train, train4[, c('Store', 'Date', fieldNames)], by = c('Store', 'Date'))
test <- left_join(test, test4[, c('Id', fieldNames)], by = c('Id'))

fieldNames <- names(train5)
fieldNames <- fieldNames[grep('predWinnerHoliday_([0-9]{1,2})', fieldNames)]
train <- left_join(train, train5[, c('Store', 'Date', fieldNames)], by = c('Store', 'Date'))
test <- left_join(test, test5[, c('Id', fieldNames)], by = c('Id'))

fieldNames <- names(test6)
fieldNames <- fieldNames[grep('h2o_', fieldNames)]
test <- left_join(test, test6[, c('Id', fieldNames)], by = c('Id'))

# ----------------------------------------------------------------------
rm(train1, test1, train2, test2, train3, test3, train4, test4, trainOpened, testOpened)
rm(train5, test5, test6)
rm(trainConverted, testConverted, tra)

test$predFullDate_10 <- NULL
train$predFullDate_10 <- NULL

# ----------------------------------------------------------------------
test$Id <- as.integer(test$Id)

train[is.na(train)] <- 0
test[is.na(test)] <- 0

train[train$Open == 0 | train$Sales == 0, names(train)[13:length(names(train))]] <- 0
test[test$Open == 0 | test$Sales == 0, names(test)[12:length(names(test))]] <- 0

# ----------------------------------------------------------------------
for(i in c(1, 2, 3, 4, 6)) {
    rf1 <- read.csv(paste0("rf", i, ".csv"), stringsAsFactors=FALSE)
    fn <- paste0("winnerRf_", i)
    names(rf1) <- c("Id", fn)
    test <- left_join(test, rf1, by = c('Id'))
}
rm(rf1, fn, i)

# ----------------------------------------------------------------------
test$BestPredictedSales <- ifelse(test$Open == 1, test$BestPredictedSales, 0)
fieldNames <- names(test)
fieldNames <- fieldNames[14:length(fieldNames)]
test[test$Open == 0, fieldNames] <- 0

# ----------------------------------------------------------------------
fieldNames <- names(test)
fieldNames <- fieldNames[13:length(fieldNames)]
fieldNamesBestPred <- fieldNames[grep('BestPredictedSales', fieldNames)]
fieldNames <- fieldNames[-which(fieldNames %in% fieldNamesBestPred)]


errors <- c()
for(field in fieldNames) {
    error <- rmpse(test[, field], test$BestPredictedSales6)
    print(paste0(field, ' - ', error))
    errors <- c(errors, error)
}
names(errors) <- fieldNames
errors <- errors[order(errors)]
topAccPredictors <- names(head(errors, 30))

mixed <- rowMeans(test[, topAccPredictors])
mixed <- mixed * 0.1 + test$BestPredictedSales6 * 0.9
rmpse(mixed, test$BestPredictedSales)
rmpse(mixed, test$BestPredictedSales2)
rmpse(mixed, test$BestPredictedSales3) # 0.10338
rmpse(mixed, test$BestPredictedSales4) # 0.10331
rmpse(mixed, test$BestPredictedSales5) # 0.10329
rmpse(mixed, test$BestPredictedSales6)


test$BestPredictedSales5 <- mixed
test$BestPredictedSales6 <- test2$top_mixed


mixedWinner <- mixed # - 0.10356
mixedWinner2 <- mixed # - 0.10356

# -------------------------------------------------------------
# top
fieldNames <- names(train)
fieldNames <- c(
    fieldNames[grep('predWinner', fieldNames)],
    fieldNames[grep('predFolded', fieldNames)]
)
#fieldNames <- fieldNames[grep('predWinnerThird_([0-9]{1,2})', fieldNames)]

test2 <- test
test2$top_mixed <- test2$BestPredictedSales6
test2[test2$top_mixed > test2$PredictedMeanMonthSales, 'top_mixed'] <- 0.1 * rowMeans(
    test2[test2$top_mixed > test2$PredictedMeanMonthSales, fieldNames]
) + 0.9 * test2[test2$top_mixed > test2$PredictedMeanMonthSales, 'top_mixed']

# на текущей итерации сработал сдвиг только нижней части
# на следующей итерации попробовать откатить верхюю планку до BestPredictedSales5
# попробовать разбить датасет на 4 части и двигать каждую часть раздельно. если улучшися результат - повторить. если нет - больше не трогать.

test2[test2$top_mixed < test2$PredictedMeanMonthSales & test2$Promo == 0, 'top_mixed'] <- 0.2 * rowMeans(
    test2[test2$top_mixed < test2$PredictedMeanMonthSales & test2$Promo == 0, topAccPredictors]
) + 0.8 * test2[test2$top_mixed < test2$PredictedMeanMonthSales & test2$Promo == 0, 'top_mixed']


rmpse(test2$top_mixed, test$BestPredictedSales6)
rmpse(test2[test2$Promo == 1, 'top_mixed'], test2[test2$Promo == 1, 'BestPredictedSales6'])
rmpse(test2[test2$Promo == 0, 'top_mixed'], test2[test2$Promo == 0, 'BestPredictedSales6'])

submission <- data.frame(Id = as.integer(test2$Id), Sales = as.integer(test2$top_mixed))
submissionName <- paste0("results/xg_", format(Sys.time(), "%H_%M_%S"), '_mixed_top')

# ----------------------------------------------------------------------
fieldNames <- names(train)
fieldNames <- c(
    fieldNames[grep('predWinner', fieldNames)],
    fieldNames[grep('predFolded', fieldNames)]
)
#View(test[, c(fieldNames, 'BestPredictedSales6')])

fieldNames <- topAccPredictors



test2 <- test
test2[test2$Open == 0, 'BestPredictedSales7'] <- 0
test2$top_mixed <- test2$BestPredictedSales7
idx.list = list(
    test2$Promo == 1 & test2$top_mixed > test2$PredictedMeanMonthSales,
    test2$Promo == 1 & test2$top_mixed <= test2$PredictedMeanMonthSales,
    test2$Promo == 0 & test2$top_mixed > test2$PredictedMeanMonthSales,
    test2$Promo == 0 & test2$top_mixed <= test2$PredictedMeanMonthSales
)

for (i in 1:4) {
    test2$top_mixed <- test2$BestPredictedSales7

    idx <- unlist(idx.list[i])
    test2[idx, 'top_mixed'] <- 0.1 * rowMeans(test2[idx, fieldNames]) + 0.9 * test2[idx, 'top_mixed']

    test2[test2$Open == 0, 'top_mixed'] <- 0

    print(rmpse(test2$top_mixed, test2$BestPredictedSales7))

    submission <- data.frame(Id = as.integer(test2$Id), Sales = as.integer(test2$top_mixed))
    submissionName <- paste0("results/xg_", format(Sys.time(), "%H_%M_%S"), paste0('_mixed_top_', i))
    submissionFile <- paste0(submissionName, ".csv")
    write.csv(submission, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)
}

# 1 0.10314 -
# 2 0.10309
# 3 0.10311 -
# 4 0.10309


#test$BestPredictedSales7 <- test2$top_mixed



# ----------------------------------------------------------------------
test2$top_mixed <- 0.1 * rowMeans(test2[, c('h2o_1', 'h2o_2')]) + 0.9 * test2$top_mixed
# 0.10319

submission <- data.frame(Id = as.integer(test2$Id), Sales = as.integer(test2$top_mixed))
submissionName <- paste0("results/xg_", format(Sys.time(), "%H_%M_%S"), '_h2o')


# ----------------------------------------------------------------------
submission <- data.frame(Id = as.integer(test$Id), Sales = as.integer(mixed))
submissionName <- paste0("results/xg_", format(Sys.time(), "%H_%M_%S"), '_mixed')


# ----------------------------------------------------------------------
fieldNames <- names(test)
fieldNames <- fieldNames[grep('predWinnerHoliday_([0-9]{1,2})', fieldNames)]

fieldNames <- names(test)
fieldNames <- fieldNames[grep('predWinnerThird_([0-9]{1,2})', fieldNames)]

fieldNames <- names(test)
fieldNames <- fieldNames[grep('predWinnerSecond_([0-9]{1,2})', fieldNames)]

fieldNames <- names(test)
fieldNames <- fieldNames[grep('predFolded([0-9]{1,2})', fieldNames)]

fieldNames <- names(test)
fieldNames <- fieldNames[grep('winnerRf_([0-9]{1,2})', fieldNames)]

fieldNames <- names(test)
fieldNames <- fieldNames[grep('predFullDate_([0-9]{1,2})', fieldNames)]


fieldNames <- names(test)
fieldNames <- c(
    fieldNames[grep('predWinnerHoliday_([0-9]{1,2})', fieldNames)],
    fieldNames[grep('predWinnerThird_([0-9]{1,2})', fieldNames)],
    fieldNames[grep('predWinnerSecond_([0-9]{1,2})', fieldNames)],
    fieldNames[grep('predFolded([0-9]{1,2})', fieldNames)],
    fieldNames[grep('winnerRf_([0-9]{1,2})', fieldNames)],
    #fieldNames[grep('predFullDate_([0-9]{1,2})', fieldNames)],
    #fieldNames[grep('rf_[0-9]$', fieldNames)],
    fieldNames[grep('BestPred', fieldNames)]
)


test2 <- test
test2[test2$Open == 0, 'BestPredictedSales7'] <- 0
test2$top_mixed <- test2$BestPredictedSales7
#idx = test2$top_mixed > 10000
idx = test2$top_mixed > test2$PredictedMeanMonthSales

test2[idx, 'top_mixed'] <- 0.05 * rowMeans(test2[idx, fieldNames]) + 0.95 * test2[idx, 'top_mixed']
test2[test2$Open == 0, 'top_mixed'] <- 0


print(rmpse(test2$top_mixed, test2$BestPredictedSales7))


submission <- data.frame(Id = as.integer(test2$Id), Sales = as.integer(test2$top_mixed))
submissionName <- paste0("results/xg_", format(Sys.time(), "%H_%M_%S"), 'top_mixed_FINAL_0000')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)


# ----------------------------------------------------------------------

submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)


