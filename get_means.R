
rownames(errors) <- NULL
errors.df <- data.frame(errors)
colSums(errors.df)
apply(data.frame(errors), 2, function(col) {
    return(sum(unlist(col)))
})

submission$Sales <- as.integer(rowMeans(test[, c('predSales.5', 'predSales.8')]))



errors.short <- apply(train[, tranNames[grep('pred[.]full[.]short[.]([0-9]{1,2})', tranNames)]], 2, function(col) {
  return(rmpse(col, train$Sales))
})

submission$Sales <- as.integer(rowMeans(test[, names(errors.short[errors.short > 0.15])]))
