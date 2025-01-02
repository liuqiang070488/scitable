sciStdDiffFromLstMeans<-function (lstMeans) {
  lstCovs <- lapply(lstMeans, sciMultinomialVar)
  lstMeanDiffs <- lapply(lstMeans, function(x) {
    lapply(lstMeans, function(y) {
      x - y
    })
  })
  lstCovMeans <- lapply(lstCovs, function(x) {
    lapply(lstCovs, function(y) {
      (x + y)/2
    })
  })
  sqSmds <- vector(mode = "numeric")
  for (i in seq_along(lstMeans)) {
    for (j in seq_along(lstMeans)) {
      if (i < j) {
        T_C <- t(t(lstMeanDiffs[[i]][[j]]))
        S <- lstCovMeans[[i]][[j]]
        if (all(S[!is.na(S)] %in% 0)) {
          if (all(!is.na(T_C) & (T_C == 0))) {
            sqMD <- 0
          }
          else {
            sqMD <- NaN
          }
        }
        else {
          sqMD <- t(T_C) %*% MASS::ginv(S) %*% T_C
        }
        sqSmds <- c(sqSmds, sqMD)
      }
    }
  }
  sqrt(sqSmds)
}
