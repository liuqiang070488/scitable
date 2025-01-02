utils::globalVariables(c('std.error'
))

stderr<-function(mx) {return(std.error(mx,na.rm=TRUE))}
