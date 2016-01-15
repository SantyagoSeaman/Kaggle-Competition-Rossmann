
ticks = data.frame(days = seq(4, 360, 7))
ticks$values <- 3000

train$Date_yearday <- as.integer(format(train$Date, "%j"))
train$Date_monthday <- as.integer(format(train$Date, "%d"))
test$Date_yearday <- as.integer(format(test$Date, "%j"))
test$Date_monthday <- as.integer(format(test$Date, "%d"))

for(StoreId in StoreIds) {
    test.data <- test[test$Store == StoreId & test$Open == 1 & test$Date_year == 2015, ]
    if (nrow(test.data) > 0) {

        zzz <- aggregate(Sales ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1, ], mean)
        zzz.promo <- aggregate(Promo ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1, ], sum)
        zzz <- left_join(zzz, zzz.promo, c("Date_yearday", "Date_year"))

        zzz.pred <- aggregate(pred.full.month.1 ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1, ], mean)
        zzz.pred.promo <- aggregate(Promo ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1, ], sum)
        zzz.pred <- left_join(zzz.pred, zzz.pred.promo, c("Date_yearday", "Date_year"))

        zzz.test <- aggregate(pred.full.month.1 ~ Date_yearday + Date_year, test.data, mean)
        zzz.test.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzz.test <- left_join(zzz.test, zzz.test.promo, c("Date_yearday", "Date_year"))

#         zzz.test2 <- aggregate(predSales.18 ~ Date_yearday + Date_year, test.data, mean)
#         zzz.test2.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
#         zzz.test2 <- left_join(zzz.test2, zzz.test2.promo, c("Date_yearday", "Date_year"))

        zzzz <- aggregate(PredictedMeanMonthSales ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1, ], mean)

        zzzz.test <- aggregate(PredictedMeanMonthSales ~ Date_yearday + Date_year, test.data, mean)
        zzzz.test.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzzz.test <- left_join(zzzz.test, zzzz.test.promo, c("Date_yearday", "Date_year"))

        zzz.p1 <- aggregate(Sales ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1 & train$Promo == 1, ], mean)
        zzz.p0 <- aggregate(Sales ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1 & train$Promo == 0, ], mean)


        zzz.test3 <- aggregate(BestPredictedSales ~ Date_yearday + Date_year, test.data, mean)
        zzz.test3.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzz.test3 <- left_join(zzz.test3, zzz.test3.promo, c("Date_yearday", "Date_year"))

#         zzz.test4 <- aggregate(pred.full.month.2 ~ Date_yearday + Date_year, test.data, mean)
#         zzz.test4.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
#         zzz.test4 <- left_join(zzz.test4, zzz.test4.promo, c("Date_yearday", "Date_year"))



#         data <- train[train$Store == StoreId & train$Open == 1 & train$Date_year == 2015, ]
#         dd <- test[test$Store == StoreId & test$Open == 1, ]
#         dd <- dd[c(10, 40), ]
#         dd$Sales <- c(5000, 3000)
#         data <- bind_rows(data, dd[, c('Sales', "Date_yearday", "Date_year")])
#         model <- lm(Sales ~ poly(Date_yearday, degree = 5, raw = T, coefs = model$coefficients), data)
#         pred <- data.frame(pred = model$fitted.values)
#         pred <- bind_cols(pred, data[, c('Sales', "Date_yearday", "Date_year")])
#
#         data <- test[test$Store == StoreId & test$Open == 1, ]
#         predTest <- data.frame(pred = predict(model, data))
#         predTest <- bind_cols(predTest, data[, 1:50])
#
#         ggplot(pred, aes(Date_yearday, pred)) + scale_x_discrete() +
#             geom_point(size = 3, stat = "identity", color = "yellow") +
#             geom_point(aes(Date_yearday, Sales, color=factor(Date_year)), size = 3, stat = "identity") +
#             geom_point(aes(Date_yearday, pred), data = predTest, size = 3, stat = "identity", color = "orange")

        g <- ggplot(zzz, aes(Date_yearday, Sales, color = factor(Date_year))) +
            scale_x_discrete() + #scale_y_discrete(breaks = seq(0, 50000, 1000)) +
            #geom_smooth(data = zzz, size = 1, method="loess", alpha = 0.2) +
            geom_smooth(size = 1.2, method="loess", alpha = 0.2, span = 0.5) +
            geom_smooth(data = zzz.p1, size = 0.7, method="loess", alpha = 0.1) +
            geom_smooth(data = zzz.p0, size = 0.7, method="loess", alpha = 0.1) +
            #geom_smooth(size = 0.7, method="lm", se = FALSE, fullrange=TRUE) +
            geom_point(aes(shape = factor(Promo)), size = 5, stat = "identity", alpha = 0.8) +
            geom_point(aes(Date_yearday, PredictedMeanMonthSales), zzzz, stat = "identity", size = 2, alpha = 0.5, shape = 8) +
            geom_point(aes(Date_yearday, PredictedMeanMonthSales), zzzz.test, stat = "identity", size = 2, alpha = 0.5, shape = 8) +
            geom_point(aes(Date_yearday, pred.full.month.1, shape = factor(Promo)), zzz.pred, size = 3, stat = "identity", alpha = 0.7) +
            geom_point(aes(Date_yearday, pred.full.month.1, shape = factor(Promo)), zzz.test, size = 5, stat = "identity", alpha = 0.8, color="skyblue3") +
            # geom_point(aes(Date_yearday, pred.full.month.2, shape = factor(Promo)), zzz.test4, size = 5, stat = "identity", alpha = 0.8, color="skyblue") +
            # geom_point(aes(Date_yearday, predSales.18, shape = factor(Promo)), zzz.test2, size = 4, stat = "identity", alpha = 0.8, color="orange") +
            geom_point(aes(Date_yearday, BestPredictedSales, shape = factor(Promo)), zzz.test3, size = 3, stat = "identity", alpha = 0.8, color="black") +
            geom_point(aes(days, values), ticks, size = 2, stat = "identity", alpha = 0.5, color="black")


        ggsave(paste0('plots/curves.by.day/', StoreId, '.png'), g, width = 25, height = 7, scale = 4, dpi = 72, limitsize = F)

    }
}


