qplot(Date_week, MeanSalesInDayOfWeek, color = factor(Store), data = train[trainDatesFilter & train$Store %in% 1:2, ])

qplot(factor(Store), MeanSalesInDayOfWeek, data = train[trainDatesFilter & train$Open == 1 & train$Store %in% 1:1100, ], geom = c('boxplot'))
qplot(factor(Store), Sales, data = train[trainDatesFilter & train$Open == 1 & train$Store %in% 1:1100, ], geom = c('boxplot'))

qplot(Date, MeanSalesInDayOfWeek, color = factor(Store), data = train[trainDatesFilter & train$Store %in% 1:10, ])



qplot(DayOfWeek, err2, data = train[trainDatesFilter & train$Store == 909, ])
qplot(factor(Store), err2, data = train[trainDatesFilter & train$Open == 1 & train$Store %in% 1:1100, ], geom = c('boxplot'))



# ======================================
boxplot.results <- boxplot(Sales ~ Store, data = train[train$Open == 1, ])
conf <- data.frame(t(boxplot.results$stats))
names(conf) <- c('minBoxplotWhisker', 'minBoxplotHinge', 'medianBoxplot', 'maxBoxplotHinge', 'maxBoxplotWhisker')
conf$Store <- as.integer(rownames(conf))
rownames(conf) <- NULL
train <- left_join(train, conf, by='Store')
train$IsSalesOutsideConf <- F
train$IsSalesOutsideConf <- train$Sales != 0 & (train$Sales < train$minBoxplotWhisker | train$Sales > train$maxBoxplotWhisker)
nrow(train[train$IsSalesOutsideConf == T, ])
test$IsSalesOutsideConf = F
