submission <- data.frame(Id=test$Id, stringsAsFactors = F)
submission$Id <- as.integer(submission$Id)
submission$Sales <- as.integer(test$pred.sales.log.by.store.2)
submission$Sales <- as.integer(test$pred.full.month.1)
submission$Sales <- as.integer(test$pred.sales)
submission$Sales <- as.integer(test$BestPredictedSalesZero)
#submission$Sales <- as.integer(test$MeanSalesInDayOfWeek)
#submission$Sales <- pred.mean

tranNames <- names(test)
submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('pred[.]full[.]short[.]([0-9]{1,2})', tranNames)]]))
submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('pred[.]full[.]monthmean[.]([0-9]{1,2})', tranNames)]]))
submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('predSales[.]([0-9]{1,2})', tranNames)]]))
submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('pred[.]full[.]second[.]([0-9]{1,2})', tranNames)]]))
submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('pred[.]full[.]third[.]([0-9]{1,2})', tranNames)]]))
submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('pred[.]full[.]fifth[.]([0-9]{1,2})', tranNames)]]))
submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('pred[.]full[.]month[.]([0-9]{1,2})', tranNames)]]))

submission$Sales <- as.integer(rowMeans(test[, c('pred.full.month.5', 'pred.full.month.6', 'BestPredictedSales')]))

submission$Sales <- as.integer(rowMeans(test[, c(tranNames[grep('predSales[.]([0-9]{1,2})', tranNames)], tranNames[grep('pred[.]full[.]short[.]([0-9]{1,2})', tranNames)])]))
submission$Sales <- as.integer(rowMeans(test[, tranNames[grep('BestPredictedSales[0-9]{1}', tranNames)]]))


submissionName <- paste0("results/xg_", format(Sys.time(), "%H_%M_%S"), '_best_mean')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)


write.csv(rf.0.10, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)

train$pred_sales = train$MeanSalesInDayOfWeek * (1 + train$PrevDayStateHolidayCoef + train$WorkInHolidayCoef + train$PromoCoef + train$SchoolHoliday * 0)
rmse(train[!is.na(train$pred_sales), 'Sales'], train[!is.na(train$pred_sales), 'pred_sales'])

zzzTrain <- train[train$Store == 13 & train$Date_month == 9,]
zzzTrain <- zzzTrain[order(as.Date(zzzTrain$Date)),]
View(zzzTrain)




zzz <- train[is.na(train$pred_sales),]



test$pred_sales_log <- pred.mean
