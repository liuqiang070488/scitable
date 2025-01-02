#'@title  sci1freq
#'@name  sci1freq
#'@description  You can use it to draw a baseline table of data.Creates 'Table 1', i.e., description of baseline patient
#'              characteristics, which is essential in every medical research.
#'              Supports both continuous and categorical variables, as well as
#'              p-values and standardized mean differences.
#'
#'@details  Table 1 represents the relationship between the baseline values of the data.
#'          This function can be easily done.
#'
#'
#'@param mvars The full range of variables you don't want to compare.
#'@param x Enter the variables to be layered. If you fill in consecutive variables, by default they will be split into 3 layers.
#'@param data Enter your data.
#'@param dec The precision of the data, which defaults to 2 decimal places.
#'@param nonnormal When the data belongs to a non-normal distribution, this parameter is needed to indicate which is variable is non-normally distributed.
#'@param statistic Statistical effect values. Usually, it is the default F, and selecting T will return a statistical effect value.
#'@param fisher Fisher's exact test. The default is FALSE.
#'@param correct Chi square test for continuity correction.The default is FALSE.
#'@param Overall Generate summary data.The default is FALSE.
#'@param smd The default is FALSE. If it is true, return the SMD value.
#'@importFrom  "stats" "fisher.test"
#'
#'@return A data frame.

utils::globalVariables(c('chisq.test'
))


sci1freq<- function(mvars,x,data,dec,nonnormal=NULL,statistic=NULL,fisher=NULL,
                    correct=NULL,Overall=NULL,smd=NULL) {
  options(warn=-1)
  mvars<-mvars;x<-x;data<-data;nonnormal<-nonnormal
  fisher<-fisher;correct<-correct;Overall<-Overall
  if (missing(dec)) {dec<-2} else {dec<-dec}
  xvt<-data[,x];nc<-length(mvars);varsdt<-data[,mvars];
  if (nc==1) varsdt<-as.matrix(varsdt,ncol=nc)
  n.x<-length(levels(factor(xvt)));
  queshiliebiao<-is.na(cbind(xvt,varsdt))
  pp<-NULL; st.diff<-NULL;d<-NULL;sv<-NULL;o.smd<-NULL
  for (i in (1:nc)) {
    t1<-table(varsdt[,i],factor(xvt),useNA="no")
    Overall.t1<-table(varsdt[,i],useNA="no")
    if (fisher==FALSE) {
      pvalue<-chisq.test(t1,correct=correct)$p.value;svv1<-chisq.test(t1,correct=correct)[["statistic"]]
    } else if (fisher==TRUE) {
      pvalue<-fisher.test(t1)$p.value;svv1<-""} else {
        stop("Fisher can only take the value FALSE or TRUE")
      }
    pp1<-ifelse(pvalue<0.001, "<0.001", pvformat(pvalue,3));
    if (fisher==FALSE) {sv1<-ifelse(svv1<0.001, "<0.001",pvformat(svv1,3))}
    if (fisher==T) {sv1<-""}
    if (Overall==FALSE) {
      ooi<-cbind(matrix(rep(" ",times=ncol(t1)),nrow=1), pp1)
    } else {
      ooi<-cbind("Overall",matrix(rep(" ",times=ncol(t1)),nrow=1), pp1)
    }
    p1<-prop.table(t1,2)
    Overall.p1<-Overall.t1/length(varsdt[,i])
    smds<-sciStdDiffMulti(variable = varsdt[,i], group = factor(xvt))
    smds<-pvformat(mean(smds),3)
    o.smd<-rbind(o.smd,smds)
    tb1<-matrix(paste(format(t1)," (", numfmt(p1*100,dec), "%)", sep=""),nrow=nrow(t1))
    Overall.tb1<-matrix(paste(format(Overall.t1)," (", numfmt(Overall.p1*100,dec), "%)", sep=""),
                        nrow=nrow(Overall.p1))
    varnames<-paste(x,".",levels(factor(xvt)),sep="")
    if (Overall==T) {
      tb1<-cbind(Overall.tb1,tb1)
      varnames<-c("Overall",varnames)
    }
    tmp.names<-c(mvars[i],rownames(t1))
    if (statistic==T) {
      d1<-cbind(rownames(t1),tb1,rep(" ",times=length(rownames(t1))),rep(" ",times=length(rownames(t1))))
      varnames<-c(varnames,"statistic")
    } else {d1<-cbind(rownames(t1),tb1,rep(" ",times=length(rownames(t1))))}
    if (statistic==T & Overall==T) {
      d2<-c(mvars[i],"",rep(" ",times=n.x),sv1,pp1)
    } else if (statistic==T){
      d2<-c(mvars[i],rep(" ",times=n.x),sv1,pp1)
    } else if (Overall==T) {
      d2<-c(mvars[i],"",rep(" ",times=n.x),pp1)
    } else {d2<-c(mvars[i],rep(" ",times=n.x),pp1)
    }
    if (smd==T) {
      n.f<-length(levels(factor(varsdt[,i])))
      d2<-c(d2,smds)
      d1<-cbind(d1,rep(" ",times=n.f))
    }
    d0<-rbind(d2,d1)
    d<-rbind(d,d0)
  }
  if (smd==T) {
    colnames(d)<-c("Characteristic",varnames,"p value","smd")
  } else {
    colnames(d)<-c("Characteristic",varnames,"p value")
  }
  out1<-list(data=d,smd=o.smd)
  out1
}
