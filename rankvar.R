rankvar <- function(var, num) {
  qprobs <- 1/num
  if (num>2) {for (i in (2:(num-1))) {qprobs <- c(qprobs, 1/num * i) }} 
  outvar <- rep(0, times=length(var))
  outvar[is.na(var)] <- NA
  cutpoints <- quantile(var,probs=qprobs, na.rm=TRUE)
  for (k in (1:length(cutpoints))) {outvar[var>=cutpoints[k]] <- k}
  tmp<-c(min(var,na.rm=TRUE),cutpoints,max(var,na.rm=TRUE))
  names(tmp)<-c("Min",names(cutpoints),"Max")
  print(tmp)
  return(outvar)
}