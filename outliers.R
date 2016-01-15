
patterns <- c(
    'pred[.]full[.]month[.]([0-9]{1,2})',
    'rf_([0-9]{1,2})',
    'predFullDate_([0-9]{1,2})',
    'predFolded([0-9]{1,2})',
    'predWinnerSecond_([0-9]{1,2})',
    'winnerRf_([0-9]{1,2})'
)

fileNames <- c(
    'pred_full_month',
    'rf',
    'predFullDate',
    'predFolded',
    'predWinnerSecond',
    'winnerRf'
)

for(i in 1:6) {
    print(fileNames[i])
    fieldNames <- names(test)
    fieldNames <- fieldNames[grep(patterns[i], fieldNames)]

    test.promo <- test[test$Open == 1 & test$Promo == 1, c('Id', 'PredictedMeanMonthSales', fieldNames)]
    test.promo$Sales <- as.integer(rowMeans(test.promo[, fieldNames]))
    test.promo.top <- test.promo[test.promo$Sales / test.promo$PredictedMeanMonthSales > 1.2 & test.promo$Sales / test.promo$PredictedMeanMonthSales < 1.5, ]
    print(nrow(test.promo.top))
    test.submit <- data.frame(Id = test[, 'Id'])
    test.submit <- left_join(test.submit, test.promo.top[, c('Id', 'Sales')], by = 'Id')
    test.submit[is.na(test.submit$Sales), 'Sales'] <- 0

    submissionName <- paste0("results/outliers_", format(Sys.time(), "%H_%M_%S"), '_', fileNames[i])
    submissionFile <- paste0(submissionName, ".csv")
    #write.csv(test.submit, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)
}


for(i in 1:6) {
    print(fileNames[i])
    fieldNames <- names(test)
    fieldNames <- fieldNames[grep(patterns[i], fieldNames)]

    test.promo <- test[test$Open == 1 & test$Promo == 1, c('Id', 'BestPredictedSales4', fieldNames)]
    test.promo$Sales <- as.integer(rowMeans(test.promo[, fieldNames]))
    test.promo.top <- test.promo[test.promo$Sales / test.promo$BestPredictedSales4 > 1.1, ]
    print(nrow(test.promo.top))
    test.submit <- data.frame(Id = test[, 'Id'])
    test.submit <- left_join(test.submit, test.promo.top[, c('Id', 'Sales')], by = 'Id')
    test.submit[is.na(test.submit$Sales), 'Sales'] <- 0

    submissionName <- paste0("results/above_best_", format(Sys.time(), "%H_%M_%S"), '_', fileNames[i])
    submissionFile <- paste0(submissionName, ".csv")
    #write.csv(test.submit, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)
}


