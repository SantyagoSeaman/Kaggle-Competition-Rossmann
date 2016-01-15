library(h2o)
library(data.table)
library(Metrics)
h2o.init(nthreads=-1, min_mem_size = "4G", max_mem_size = "8G")
#h2o.shutdown(F)


feature.names <- names(train)[c(1, 2, 6:19)]
feature.names <- c(feature.names, 'PrevDayPromo', 'PrevDayStateHoliday', 'WorkInHoliday', 'WorkInSunday',
                   'CompetitionOpen', 'Promo2Open', 'IsPromo2Month')

train$SalesLog <- log(train$Sales)

trainHex <- as.h2o(train[, c(feature.names, 'SalesLog')], destination_frame="train.hex")
#summary(trainHex)
testHex <- as.h2o(test[test$Open == 1, feature.names], destination_frame="test.hex")

predictionsList <- list()
for(i in 2:20)
{
#     rfHex <- h2o.randomForest(x = feature.names, y = "Sales", training_frame = trainHex, model_id = "rfStarter.hex",
#                               ntrees = 100, sample_rate = 0.8, nfolds = 5, stopping_rounds = 10)#, mtries = 6
#     rfHex <- h2o.randomForest(x = feature.names, y = "Sales", training_frame = trainHex, model_id = "rfStarter.hex",
#                               ntrees = 100)#, mtries = 6
    gbmHex <- h2o.gbm(x = feature.names,
                      y = "SalesLog", training_frame=trainHex, model_id="gbmStarter.hex",
                      distribution="AUTO",
                      nfolds = 5, stopping_rounds = 10,
                      seed = 123,
                      ntrees = 3000,
                      max_depth = 10,
                      min_rows = 10,
                      learn_rate = 0.01,
                      sample_rate = 0.9,
                      col_sample_rate = 0.9)

    #predictionsList <- c(predictionsList, h2o.predict(rfHex, testHex))
    #predictionsList <- c(predictionsList, h2o.predict(gbmHex, testHex))

    zzz <- as.data.frame(h2o.predict(gbmHex, testHex))
    test[, paste0('h2o_', i)] <- 0
    test[test$Open == 1, paste0('h2o_', i)] <- exp(zzz$predict)
}


#rfHex
#h2o.varimp(rfHex)
#rm(train)

#summary(testHex)


predictions <- as.data.frame(expm1(predictionsList[1][[1]]))
predictions <- cbind(predictions, as.data.frame(expm1(predictionsList[2][[1]])))
predictions <- cbind(predictions, as.data.frame(expm1(predictionsList[3][[1]])))
names(predictions) <- c('pred1', 'pred2', 'pred3')
predictions$pred1_inch <- convertTo001Inch(predictions$pred1)
predictions$pred2_inch <- convertTo001Inch(predictions$pred2)
predictions$pred3_inch <- convertTo001Inch(predictions$pred3)


predictions <- cbind(predictions, data.frame(submission = submission$Expected))

predictions <- cbind(predictions, data.frame(submission2 = submission2$Expected))
predictions <- cbind(predictions, data.frame(submission2_gbm = submission2_gbm$Expected))
predictions <- cbind(predictions, data.frame(submission2_gbm_2 = submission2_gbm_2$Expected))
predictions <- cbind(predictions, data.frame(submission2_gbm_3 = submission2_gbm_3$Expected))

predictions <- cbind(predictions, data.frame(rfv3cn3 = rfv3cn3$Expected))
predictions <- cbind(predictions, data.frame(rf_4_2_2_2 = rf_4_2_2_2$Expected))


submission <- fread("./data/sample_solution.csv")
#submission$Expected <- 0.79 * expm1(predictions$predict) + 0.21 * submission$Expected
#means <- rowMeans(predictions[, c('pred1', 'pred2', 'pred3')])
#means <- round(rowMeans(predictions[, c('pred1', 'pred2', 'pred3')]) / 0.254) * 0.254

submission$Expected <- 0.7*predictions$rfv3cn3 + 0.1*predictions$pred1_inch + 0.1*predictions$pred2_inch + 0.1*predictions$pred3_inch

submission$Expected <- 0.79*means + 0.21 * submission$Expected

#convert expected values to 0.01in values
#submission$Expected <- round(submission$Expected / 0.254) * 0.254

write.csv(submission, "rf_means.csv", row.names=F)






#submission <- read.csv("submission.csv", stringsAsFactors=FALSE)
submission_gbm <- read.csv("submission_gbm.csv", stringsAsFactors=FALSE)
submission_gbm_2 <- read.csv("submission_gbm_2.csv", stringsAsFactors=FALSE)
submission_gbm_3 <- read.csv("submission_gbm_3.csv", stringsAsFactors=FALSE)

rfv3cn3 <- read.csv("rfv3cn3.csv", stringsAsFactors=FALSE)
rf_4_2_2_2 <- read.csv("rf_4_2_2_2.csv", stringsAsFactors=FALSE)


