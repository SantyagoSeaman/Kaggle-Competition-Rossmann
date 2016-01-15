
ticks = data.frame(days = seq(4, 360, 7))
ticks$values <- 3000

for(StoreId in StoreIds[1:10]) {
    test.data <- test2[test2$Store == StoreId & test2$Open == 1 & test2$Date_year == 2015, ]
    if (nrow(test.data) > 0) {

        zzz <- aggregate(Sales ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1 & train$Date_year == 2015 & train$Date_month %in% c(6,7), ], mean)
        zzz.promo <- aggregate(Promo ~ Date_yearday + Date_year, train[train$Store == StoreId & train$Open == 1 & train$Date_year == 2015 & train$Date_month %in% c(6,7), ], sum)
        zzz <- left_join(zzz, zzz.promo, c("Date_yearday", "Date_year"))

        zzz1 <- aggregate(BestPredictedSales7 ~ Date_yearday + Date_year, test.data, mean)
        zzz1.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzz1 <- left_join(zzz1, zzz1.promo, c("Date_yearday", "Date_year"))

        zzz2 <- aggregate(BestPredictedSales6 ~ Date_yearday + Date_year, test.data, mean)
        zzz2.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzz2 <- left_join(zzz2, zzz2.promo, c("Date_yearday", "Date_year"))

        zzz3 <- aggregate(BestPredictedSales5 ~ Date_yearday + Date_year, test.data, mean)
        zzz3.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzz3 <- left_join(zzz3, zzz3.promo, c("Date_yearday", "Date_year"))

        zzz4 <- aggregate(BestPredictedSales4 ~ Date_yearday + Date_year, test.data, mean)
        zzz4.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzz4 <- left_join(zzz4, zzz4.promo, c("Date_yearday", "Date_year"))

        zzz5 <- aggregate(BestPredictedSales3 ~ Date_yearday + Date_year, test.data, mean)
        zzz5.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzz5 <- left_join(zzz5, zzz5.promo, c("Date_yearday", "Date_year"))

        zzzM <- aggregate(top_mixed ~ Date_yearday + Date_year, test.data, mean)
        zzzM.promo <- aggregate(Promo ~ Date_yearday + Date_year, test.data, sum)
        zzzM <- left_join(zzzM, zzzM.promo, c("Date_yearday", "Date_year"))

        g <- ggplot(zzz, aes(Date_yearday, Sales, color = factor(Date_year))) +
            scale_x_discrete() +
            #geom_point(aes(shape = factor(Promo)), size = 5, stat = "identity", alpha = 0.8) +
            geom_point(aes(Date_yearday, BestPredictedSales3, shape = factor(Promo)), zzz5, size = 3, stat = "identity", alpha = 0.4, color="grey") +
            geom_point(aes(Date_yearday, BestPredictedSales4, shape = factor(Promo)), zzz4, size = 4, stat = "identity", alpha = 0.5, color="orange") +
            geom_point(aes(Date_yearday, BestPredictedSales5, shape = factor(Promo)), zzz3, size = 4, stat = "identity", alpha = 0.6, color="green") +
            geom_point(aes(Date_yearday, BestPredictedSales6, shape = factor(Promo)), zzz2, size = 5, stat = "identity", alpha = 0.7, color="blue") +
            geom_point(aes(Date_yearday, BestPredictedSales7, shape = factor(Promo)), zzz1, size = 5, stat = "identity", alpha = 0.8, color="yellow") +
            geom_point(aes(Date_yearday, top_mixed, shape = factor(Promo)), zzzM, size = 2, stat = "identity", alpha = 1, color="red") +
            geom_point(aes(days, values), ticks, size = 2, stat = "identity", alpha = 0.5, color="black")


        ggsave(paste0('plots/best/', StoreId, '.png'), g, width = 25, height = 7, scale = 4, dpi = 72, limitsize = F)

    }
}


