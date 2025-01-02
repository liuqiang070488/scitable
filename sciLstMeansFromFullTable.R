sciLstMeansFromFullTable<-function (strataByLevels) {
  propTables <- prop.table(strataByLevels, margin = 1)
  if (ncol(propTables) > 1) {
    propTables <- propTables[, -1, drop = FALSE]
  }
  lstMeans <- lapply(seq_len(nrow(propTables)), function(i) {
    propTables[i, ]
  })
  lstMeans
}
