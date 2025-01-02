#'@title  Maddala.Cox.Snell
#'@name  Maddala.Cox.Snell
#'@description  Maddala Cox Snell in the computational model.
#'
#'
#'@param fit Your model. Support logistic regression and Cox regression.
#'
#'
#'@return Maddala Cox Snell.
#'
#'
#'
#'#'@details The outcome variables in the model must be represented using 0 and 1. Among them,
#'           1 represents the occurrence of the event.
#'
#'@export
#'
#'@references Riley RD, Ensor J, Snell KIE, Harrell FE, Martin GP, Reitsma JB, et al. Calculating the sample size required for
#'            developing a clinical prediction model. BMJ (Clinical research ed). 2020
#'
#'
#'


Maddala.Cox.Snell<-function(fit) {
  if (any('glm' %in% class(fit))==TRUE) {
    modely<-all.vars(fit$terms)[c(1)]
    data<-modeldata(fit)
    e<-sum(data[,modely]==1)
    n<-dim(data)[1]
    lnl.null<-e*log(e/n)+(n-e)*log(1-e/n)
    MaddalaCoxSnell<-1-exp(lnl.null/n)
  }
  if (any(class(fit)=="coxph")==TRUE) {
    modely<-model.y(fit)
    data<-modeldata(fit)
    e<-sum(data[,modely[2]]==1)
    n<-dim(data)[1]
    y<-e/n
    meantime<-sum(data[,modely[1]])/n
    y<-y*meantime
    lnl.null<- y*n*log(y)-y*n
    MaddalaCoxSnell<-1-exp(lnl.null/n)
  }
  MaddalaCoxSnell
}
