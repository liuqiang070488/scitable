sciStdDiffMulti<-function (variable, group) {
  strataByLevels <- table(group, variable)
  lstMeans <- sciLstMeansFromFullTable(strataByLevels)
  sciStdDiffFromLstMeans(lstMeans)
}
