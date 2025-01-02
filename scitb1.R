#'@title  scitb1
#'@name  scitb1
#'@description  You can use it to draw a baseline table of data.
#'
#'@details  Table 1 represents the relationship between the baseline values of the data.
#'          This function can be easily done.Creates 'Table 1', i.e., description of baseline patient
#'          characteristics, which is essential in every medical research.
#'          Supports both continuous and categorical variables, as well as
#'          p-values and standardized mean differences.
#'
#'
#'@param vars The full range of variables you don't want to compare.
#'@param fvars The categorical variables in the data are filled in here.
#'@param strata Enter the variables to be layered. If you fill in consecutive variables, by default they will be split into 3 layers.
#'@param data Enter your data.
#'@param fvars Define the categorical variables in your data.
#'@param dec The precision of the data, which defaults to 2 decimal places.
#'@param num When continuous variables are layered, use it to control the number of layers, which defaults to 3.
#'@param nonnormal When the data belongs to a non-normal distribution, this parameter is needed to indicate which is variable is non-normally distributed.
#'@param type The type of encoding generally does not require input.Contains three types: "A", "B", and "C".
#'@param statistic Statistical effect values. Usually, it is the default F, and selecting T will return a statistical effect value.
#'@param atotest Check if the data is normally distributed. The default is T.
#'@param NormalTest A method for detecting whether data is normally distributed.The default values are Kolmogorov Smirnov test and Kolmogorov Smirnov test.Other options are: "ad", "cvm", "pearson".
#'@param fisher Fisher's exact test. The default is FALSE.
#'@param correct Chi square test for continuity correction.The default is FALSE.
#'@param Overall Generate summary data.The default is FALSE.
#'@param smd The default is FALSE. If it is true, return the SMD value.
#'
#'@return A data frame.
#'
#'
#'@format NULL
#'@usage scitb1(vars,fvars=NULL,strata,data,dec,num,nonnormal=NULL,type=NULL,
#'statistic=F,atotest=T,NormalTest=NULL,fisher=FALSE,correct=FALSE,Overall=FALSE,smd=FALSE)
#'@export
#'@examples
#'## Import data
#'bc<-prematurity
#'## Hierarchical variables converted to factors.
#'bc$race<-as.factor(bc$race)
#'###Define all variables, categorical and stratified.
#'allVars <-c("age", "lwt",  "smoke", "ptl", "ht", "ui", "ftv", "bwt")
#'fvars<-c("smoke","ht","ui")
#'strata<-"race"
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc)
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc,statistic=TRUE)
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc,statistic=TRUE,Overall=TRUE)
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc,statistic=TRUE,Overall=TRUE,smd=TRUE)
#'print(out)
#'
#'###Stratified variables are continuous variables.
#'allVars <-c("race", "lwt",  "smoke", "ptl", "ht", "ui", "ftv", "bwt")
#'fvars<-c("smoke","ht","ui","race")
#'strata<-"age"
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc)
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc,statistic=TRUE)
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc,statistic=TRUE,Overall=TRUE,smd=TRUE)
#'print(out)
#'




scitb1<-function(vars,fvars=NULL,strata,data,dec,num,nonnormal=NULL,type=NULL,
                 statistic=F,atotest=T,NormalTest=NULL,fisher=FALSE,
                 correct=FALSE,Overall=FALSE,smd=FALSE) {
  if (missing(vars)) {stop("Missing vars.")}
  if (missing(strata)) {stop("Missing strata.")}
  if (missing(data)) {stop("Missing data.")}
  vars<-vars;fvars<-fvars;data<-as.data.frame(data);nonnormal<-nonnormal;
  if (missing(dec)) {dec<-2} else {dec<-dec}
  if (missing(num)) {num<-3} else {num<-num}
  if (missing(type)) {type<-"A"} else {type<-type}
  strata<-strata;NormalTest<-NormalTest;statistic<-statistic;
  atotest<-atotest;correct<-correct;Overall<-Overall;fisher<-fisher;smd<-smd
  if (!is.factor(data[,strata]) & length(levels(factor(data[,strata]))) >5 ) {
    G<-rankvar(data[,strata],num=num)
    data$G<-G
    strata<-"G"
    message("Strata is treated as a continuous variable.")
  }
  mvars<-setdiff(vars, fvars)
  if (!is.null(fvars)) {
    if (identical(vars,fvars)) {
      fout<-sci1freq(mvars=fvars,x=strata,data=data,nonnormal=nonnormal,dec=dec,
                     statistic=statistic,correct=correct,Overall=Overall,fisher=fisher,smd=smd)
      dat<-fout[["data"]]
    } else {
      fout<-sci1freq(mvars=fvars,x=strata,data=data,nonnormal=nonnormal,dec=dec,
                     statistic=statistic,correct=correct,Overall=Overall,fisher=fisher,smd=smd)
      mout<-sci1mean(mvars=mvars,x=strata,data=data,nonnormal=nonnormal,dec=dec,
                     type=type,statistic=statistic,atotest=atotest,
                     NormalTest=NormalTest,Overall=Overall,smd=smd)
      dat<-rbind(mout[["data"]],fout[["data"]])
    }
  } else {
    mout<-sci1mean(mvars=mvars,x=strata,data=data,nonnormal=nonnormal,dec=dec,type=type,
                   statistic=statistic,atotest=atotest,NormalTest=NormalTest,Overall=Overall,smd=smd)
    dat<-mout[["data"]]
  }
  dat
}
