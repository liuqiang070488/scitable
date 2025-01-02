#'
#'@importFrom  "stats" "var"
#'

utils::globalVariables(c('var'
))


sciStdDiff<-function (variable, group, binary = FALSE, na.rm = TRUE) {
  means <- tapply(variable, group, mean, na.rm = na.rm)
  vars<-NULL
  if (binary) {
    vars <- means * (1 - means)
  }
  else {
    vars <- tapply(variable, group, var, na.rm = na.rm)
  }
  meanDiffs <- outer(X = means, Y = means, FUN = "-")
  varMeans <- outer(X = vars, Y = vars, FUN = "+")/2
  out <- meanDiffs/sqrt(varMeans)
  out[is.na(out) & !is.na(meanDiffs) & (meanDiffs == 0) & !is.na(varMeans) &
        (varMeans == 0)] <- 0
  abs(out[lower.tri(out)])
}
