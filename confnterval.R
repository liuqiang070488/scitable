#'@title  confnterval
#'@name  confnterval
#'@description  P-values were inferred from confidence intervals.
#'
#'
#'@param est Enter the effect value.
#'@param ratio Effect ratio values. Includes OR,HR,RR.
#'@param ul The upper limit of the credible interval.
#'@param ll Lower limit of the credible interval.
#'
#'
#'@return A list of results.
#'
#'@details Limitations of the method:The formula for P is unreliable for very small P values and if your P value is smaller than 0.0001,
#'         just report it as P<0.0001.The methods described can be applied in a wide range of settings,
#'         including the results from meta-analysis and regression analyses.
#'         The main context where they are not correct is small samples where the outcome is continuous and the analysis has been done by a t test or analysis of variance,
#'         or the outcome is dichotomous and an exact method has been used for the confidence interval.
#'         However, even here the methods will be approximately correct in larger studies with, say,
#'         60 patients or more.
#'
#'@export
#'
#'@references Altman DG, Bland JM. How to obtain the P value from a confidence interval.
#'            BMJ. 2011;343:d2304. doi: 10.1136/bmj.d2304. PMID: 22803193.
#'
#'
#'@examples
#'confnterval(est=0.05917381,ul=0.06756194,ll=0.05091284)
#'
#'






confnterval<-function(est=NULL,ratio=NULL,ul=NULL,ll=NULL) {
  if (is.null(est) & is.null(ratio)) {
    stop("Coefficients and effect ratios must be filled in one.")
  }
  if (!is.null(est) & !is.null(ratio)) {
    stop("Only one of the coefficients and effect ratios can be filled in..")
  }
  if (is.null(ul)) stop("Ul cannot be missing.")
  if (is.null(ll)) stop("Ul cannot be missing.")
  if (!is.null(est)) {
    est<-est;ul<-ul;ll<-ll
    se<-(ul-ll)/2*1.96
    z<-abs(est/se)
    p1<-exp(-0.717*z-0.406*(z^2))
    if (p1==0) {p1<-paste0("<",0.0001)}
    p2<-(1-stats::pnorm(abs(z)))*2
    if (p2==0) {p2<-paste0("<",0.0001)}
    o1<-paste0("method1 P:",p1)
    o2<-paste0("method2 P:",p2)
    #out<-paste(o1,o2, sep = "\n")
    out<-list(o1,o2)
  }
  if (!is.null(ratio)) {
    est<-log(ratio);ul<-log(ul);ll<-log(ll)
    se<-(ul-ll)/2*1.96
    z<-abs(est/se)
    p1<-exp(-0.717*z-0.406*(z^2))
    if (p1==0) {p1<-paste0("<",0.0001)}
    p2<-(1-stats::pnorm(abs(z)))*2
    if (p2==0) {p2<-paste0("<",0.0001)}
    o1<-paste0("method1 P:",p1)
    o2<-paste0("method2 P:",p2)
    #out<-paste(o1,o2, sep = "\n")
    out<-list(o1,o2)
  }
  out
}
