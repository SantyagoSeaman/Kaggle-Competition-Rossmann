

for(StoreId in StoreIds) {
    test.data <- test[test$Store == StoreId & test$Open == 1 & test$Date_year == 2015, ]
    if (nrow(test.data) > 0) {
        zzz <- aggregate(Sales ~ Date_weekOfYear + Date_year, train[train$Store == StoreId & train$Open == 1, ], mean)
        zzz.promo <- aggregate(Promo ~ Date_weekOfYear + Date_year, train[train$Store == StoreId & train$Open == 1, ], sum)
        zzz <- left_join(zzz, zzz.promo, c("Date_weekOfYear", "Date_year"))

        zzz.test <- aggregate(pred.full.second.1 ~ Date_weekOfYear + Date_year, test.data, mean)
        zzz.test.promo <- aggregate(Promo ~ Date_weekOfYear + Date_year, test.data, sum)
        zzz.test <- left_join(zzz.test, zzz.test.promo, c("Date_weekOfYear", "Date_year"))

        zzzz <- aggregate(PredictedMeanMonthSales ~ Date_weekOfYear + Date_year, train[train$Store == StoreId & train$Open == 1 & train$Date_year == 2015, ], mean)

        zzzz.test <- aggregate(PredictedMeanMonthSales ~ Date_weekOfYear + Date_year, test.data, mean)
        zzzz.test.promo <- aggregate(Promo ~ Date_weekOfYear + Date_year, test.data, sum)
        zzzz.test <- left_join(zzzz.test, zzzz.test.promo, c("Date_weekOfYear", "Date_year"))

        zzzzz <- aggregate(Sales ~ Date_weekOfYear + Date_year, train[train$Store == StoreId & train$Open == 1 & train$Date_year == 2015, ], mean)

        g <- ggplot(zzz, aes(Date_weekOfYear, Sales, color = factor(Date_year))) +
            scale_x_discrete() + #scale_y_discrete(breaks = seq(0, 50000, 1000)) +
            #geom_smooth(data = zzz, size = 1, method="loess", alpha = 0.2) +
            geom_smooth(size = 1.2, method="loess", alpha = 0.2) +
            geom_smooth(size = 0.7, method="lm", se = FALSE, fullrange=TRUE) +
            geom_point(aes(shape = factor(Promo)), size = 4, stat = "identity", alpha = 0.7) +
            geom_point(aes(Date_yearday, PredictedMeanMonthSales), zzzz, stat = "identity", size = 3, alpha = 0.7, color="grey") +
            geom_point(aes(Date_yearday, PredictedMeanMonthSales), zzzz.test, size = 3, stat = "identity", alpha = 0.7, color="grey") +
            geom_point(aes(Date_weekOfYear, pred.full.second.1, shape = factor(Promo)), zzz.test, size = 4, stat = "identity", alpha = 0.7, color="yellow")
        g

        ggsave(paste0('plots/curves/', StoreId, '.png'), g, width = 7, height = 5, scale = 4, dpi = 72)
    }
}



View(train[train$Store == 644 & train$Open == 1, c('Sales', 'Date_year', 'Date_weekOfYear')])
View(train[train$Store == StoreId & train$Open == 1, c('Sales', 'Date', 'Date_year', 'Date_weekOfYear', 'DayOfWeek', 'Promo')])
?geom_bar
?scale_y_discrete

StoreId = 931
View(train[train$Store == StoreId, c('Sales', 'Date', 'DayOfWeek', 'Date_weekOfYear', 'Promo')])
View(test[test$Store == StoreId, c('pred.full.second.1', 'Date', 'DayOfWeek', 'Date_weekOfYear', 'Promo')])



View(train[train$Store == StoreId & train$Date_year==2014, ])

