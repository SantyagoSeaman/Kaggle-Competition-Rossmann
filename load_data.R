#library(data.table)
library(xgboost)
library(Matrix)
library(methods)
#library(randomForest)
library(caret)
library(dplyr)
library(Metrics)

rmpse <- function(preds, labels) {
    idx <- labels > 0
    err <- sqrt(mean((preds[idx] / labels[idx] - 1)^2))
    return(err)
}

rmpse.feval <- function(preds, dtrain)
{
    labels <- getinfo(dtrain, "label")
    eratio <- (exp(as.numeric(preds))-1) / (exp(as.numeric(labels))-1)
    #eratio[mapply(is.infinite, eratio)] <- 2
    err <- sqrt(mean((eratio-1)^2))
    return(list(metric = "RMPSE", value = err))
}

rmpse.feval.sales <- function(preds, dtrain)
{
    labels <- getinfo(dtrain, "label")
    eratio <- as.numeric(preds) / as.numeric(labels)
    err <- sqrt(mean((eratio-1)^2))
    return(list(metric = "RMPSE", value = err))
}

wk <- function(x) as.numeric(format(x, "%U"))



Sys.setlocale("LC_TIME", "English")
Sys.setlocale("LC_ALL", "English")


train <- read.csv("data/train.csv", header = T, na.strings = c("", "na", "NA", "null", "NULL"))
test <- read.csv("data/test.csv", header = T, na.strings = c("", "na", "NA", "null", "NULL"))
store <- read.csv("data/store.csv", header = T, na.strings = c("", "na", "NA", "null", "NULL"))

train <- left_join(x = train, y = store, by = 'Store')
test <- left_join(x = test, y = store, by = 'Store')

train$Id <- -1
test$Sales <- -1
test$Customers <- -1
fullDataset <- rbind(train, test)

# Пропущенные данные
# https://www.kaggle.com/c/rossmann-store-sales/forums/t/17048/putting-stores-on-the-map/96921#post96921

# Bugfix in 622 store
# https://www.kaggle.com/c/rossmann-store-sales/forums/t/17048/putting-stores-on-the-map?page=2
fullDataset[is.na(fullDataset$Open), 'Open'] <- 0

# fullDataset <- fullDataset[, !(names(fullDataset) %in% c('CompetitionDistance',
#                                                          'CompetitionOpenSinceMonth',
#                                                          'CompetitionOpenSinceYear'))]
# fullDataset <- fullDataset[, !(names(fullDataset) %in% c('Promo2',
#                                                          'Promo2SinceWeek',
#                                                          'Promo2SinceYear',
#                                                          'PromoInterval'))]
# fullDataset <- fullDataset[, !(names(fullDataset) %in% c('StoreType',
#                                                          'Assortment'))]

fullDataset$Store_factor <- as.factor(fullDataset$Store)
fullDataset$Date <- as.Date(fullDataset$Date)
fullDataset$Date_int <- as.integer(fullDataset$Date)
fullDataset$Date_month <- as.integer(format(fullDataset$Date, "%m"))
#fullDataset$Date_month_factor <- as.factor(fullDataset$Date_month)
fullDataset$Date_year <- as.integer(format(fullDataset$Date, "%Y"))
#fullDataset$Date_year_factor <- as.factor(fullDataset$Date_year)
#fullDataset$DayOfWeek_factor <- as.factor(fullDataset$DayOfWeek)

fullDataset$Date_weekOfYear <- as.integer(format(fullDataset$Date, "%W"))
fullDataset$Date_monthweek <- fullDataset$Date_weekOfYear - wk(as.Date(cut(fullDataset$Date, "month"))) + 1
fullDataset$Date_yearday <- as.integer(format(fullDataset$Date, "%j"))
fullDataset$Date_monthday <- as.integer(format(fullDataset$Date, "%d"))


fullDataset[fullDataset$Sales == 0, 'Open'] <- 0

# -----------
# Находим и заполняем дни, следующие за последним промо днём
StoreIds <- unique(fullDataset$Store)
StoreId <- 1
for(StoreId in StoreIds) {
    promodays <- fullDataset[fullDataset$Store == StoreId, c('Store', 'Date', 'Promo')]
    promodays <- promodays[order(promodays$Date),]
    rownames(promodays) <- NULL
    promodays$PrevDayPromo <- c(0, promodays[1:(nrow(promodays) - 1), 'Promo'])
    promodays$PrevDayPromo <- ifelse(promodays$PrevDayPromo == promodays$Promo, 0, promodays$PrevDayPromo)
    fullDataset[fullDataset$Store == StoreId, 'PrevDayPromo'] <- left_join(x = fullDataset[fullDataset$Store == StoreId, c('Store', 'Date')],
                                                               y = promodays[, c('Store', 'Date', 'PrevDayPromo')],
                                                               by = c('Store', 'Date'))[['PrevDayPromo']]
    if (StoreId %% 100 == 0) {
        print(StoreId)
    }
}

# -----------
# Находим и заполняем дни, следующие за праздничным днём
holidays <- fullDataset[fullDataset$Store == 1, c('Date', 'Date_year', 'Date_month', 'DayOfWeek', 'StateHoliday', 'SchoolHoliday')]
holidays <- holidays[order(holidays$Date),]
rownames(holidays) <- NULL
holidays$PrevDayStateHoliday <- factor(c('0', as.character(holidays[1:(nrow(holidays) - 1), 'StateHoliday'])))
fullDataset <- left_join(x = fullDataset, y = holidays[, c('Date', 'PrevDayStateHoliday')], by = c('Date'))
#fullDataset$PrevDayStateHolidayCoef <- ifelse(fullDataset$PrevDayStateHoliday != '0', 0.1, 0)


# -----------
fullDataset$WorkInHoliday <- as.integer(fullDataset$Open == 1 & as.integer(fullDataset$StateHoliday) > 1)
fullDataset$WorkInSunday <- as.integer(fullDataset$Open == 1 & fullDataset$DayOfWeek == 7)

# TODO: добавить ручное снижение коэффициентов предположим для 1 января
# fullDataset$WorkInHolidayCoef <- ifelse(fullDataset$WorkInHoliday, 0.1, 0)
# fullDataset$PromoCoef <- ifelse(fullDataset$Promo == 1, 0.3, 0)  # победитель - 0.3
# fullDataset$PromoCoef <- ifelse(fullDataset$Promo == 1, 0.3, 0)


fullDataset$CompetitionOpen <- 12 * (fullDataset$Date_year - fullDataset$CompetitionOpenSinceYear) +
    (fullDataset$Date_month - fullDataset$CompetitionOpenSinceMonth)
fullDataset[is.na(fullDataset$CompetitionOpen), 'CompetitionOpen'] <- 0
fullDataset[is.na(fullDataset$CompetitionDistance), 'CompetitionDistance'] <- 0

fullDataset$Promo2Open <- 12 * (fullDataset$Date_year - fullDataset$Promo2SinceYear) +
    (fullDataset$Date_weekOfYear - fullDataset$Promo2SinceWeek) / 4.0
fullDataset[is.na(fullDataset$Promo2Open), 'Promo2Open'] <- 0

month2str = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
fullDataset$PromoInterval <- as.character(fullDataset$PromoInterval)
fullDataset[is.na(fullDataset$PromoInterval), 'PromoInterval'] <- ''
fullDataset$PromoIntervalList <- strsplit(fullDataset$PromoInterval, ",")
fullDataset$PromoIntervalListInt <- sapply(fullDataset$PromoIntervalList, function(row) {
    if (length(row) > 0) {
        f <- factor(row, levels = month2str)
        return (as.integer(f))
    }
    return (c())
})

fullDataset$IsPromo2Month <- apply(fullDataset[, c('Date_month', 'PromoIntervalListInt')], 1, function(row) {
    return(ifelse(row$Date_month %in% row$PromoIntervalListInt, 1, 0))
})
fullDataset[fullDataset$IsPromo2Month == 0, 'Promo2Open'] <- 0

