utils::globalVariables(c('sd'
))

stdev<-function(mx) {return(sd(mx,na.rm=TRUE))}
