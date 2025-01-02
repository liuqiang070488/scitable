sciMultinomialVar<-function (multinomialMeans)
{
  p <- multinomialMeans
  vars <- p * (1 - p)
  covs <- -outer(p, p)
  diag(covs) <- vars
  drop(covs)
}
