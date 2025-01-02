utils::globalVariables(c('quantile'
))

mxp5<-function(mx) {return(quantile(mx,probs=0.05,na.rm=TRUE))}
