#'@title  sci1mean
#'@name  sci1mean
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
#'@param type The type of encoding generally does not require input.
#'@param statistic Statistical effect values. Usually, it is the default F, and selecting T will return a statistical effect value.
#'@param atotest Check if the data is normally distributed. The default is T.
#'@param NormalTest A method for detecting whether data is normally distributed.The default values are Kolmogorov Smirnov test and Kolmogorov Smirnov test.Other options are: "ad", "cvm", "pearson".
#'@importFrom "stringi" "stri_escape_unicode" "stri_escape_unicode"
#'@param Overall Generate summary data.The default is FALSE.
#'@param smd The default is FALSE. If it is true, return the SMD value.
#'
#'@return A data frame.

utils::globalVariables(c('aov',
                         'kruskal.test'
))




sci1mean<- function(mvars,x,data,dec,nonnormal=NULL,type=NULL,statistic=NULL,
                    atotest=NULL,NormalTest=NULL,Overall=NULL,smd=NULL) {
  mvars<-mvars;x<-x;data<-data;nonnormal<-nonnormal;type<-type
  NormalTest<-NormalTest;Overall<-Overall
  if (missing(dec)) {dec<-2} else {dec<-dec}
  xvt<-data[,x];nc<-length(mvars);varsdt<-data[,mvars];
  if (nc==1) varsdt<-as.matrix(varsdt,ncol=nc)
  n.x<-length(levels(factor(xvt)));
  queshiliebiao<-is.na(cbind(xvt,varsdt))
  pp<-NULL; st.diff<-NULL;d0<-NULL;sv<-NULL;o.dd<-NULL;o.smd<-NULL
  jia<-code(type=type)
  ntp<-999
  for (i in (1:nc)) {
    scimean<-tapply(varsdt[,i],factor(xvt),average)
    o.mean<-average(varsdt[,i])
    scistd<-tapply(varsdt[,i],factor(xvt),stdev)
    o.st<-stdev(varsdt[,i])
    scinn<-table(xvt[!is.na(varsdt[,i])])
    o.n<-length(!is.na(varsdt[,i]))
    scimedian<-numfmt(tapply(varsdt[,i],factor(xvt),mxmedian),dec)
    o.median<-numfmt(mxmedian(varsdt[,i]),dec)
    scimin<-numfmt(tapply(varsdt[,i],factor(xvt),mxmin),dec)
    o.min<-numfmt(mxmin(varsdt[,i]),dec)
    scimax<-numfmt(tapply(varsdt[,i],factor(xvt),mxmax),dec)
    o.max<-numfmt(mxmax(varsdt[,i]),dec)
    sciq1<-numfmt(tapply(varsdt[,i],factor(xvt),mxq1),dec)
    o.q1<-numfmt(mxq1(varsdt[,i]),dec)
    sciq3<-numfmt(tapply(varsdt[,i],factor(xvt),mxq3),dec)
    o.q3<-numfmt(mxq3(varsdt[,i]),dec)
    smds<-sciStdDiff(variable = varsdt[,i], group = factor(xvt))
    smds<-numfmt(mean(smds),3)
    if (atotest==T) {ntp<-nt(varsdt[,i],kind = NormalTest)}
    scitmp<-xvt[apply(queshiliebiao[,c(1,i+1)],1,sum)==0]  #取没有缺失的数据
    if (length(levels(factor(scitmp)))>1) {
      pvalue<-summary(aov(varsdt[,i]~factor(xvt)))[[1]]$"Pr(>F)"[1];svv1<-summary(aov(varsdt[,i]~factor(xvt)))[[1]]$"F value"[1]
      pp1<-ifelse(pvalue<0.001, "<0.001",pvformat(pvalue,3));sv1<-ifelse(svv1<0.001, "<0.001",pvformat(svv1,3))
    }
    if (length(levels(factor(scitmp)))>1) {
      pvalue.npr<-kruskal.test(varsdt[,i]~factor(xvt))$p.value;svv2<-kruskal.test(varsdt[,i]~factor(xvt))[["statistic"]]
      pp1.npr<-ifelse(pvalue.npr<0.001, "<0.001",pvformat(pvalue.npr,3))
      sv1.npr<-ifelse(svv2<0.001, "<0.001",pvformat(svv2,3))
    }
    if (!mvars[i] %in% nonnormal) {
      d1<-paste(numfmt(scimean,dec),jia,numfmt(scistd,dec),sep="")
      o.d<-paste(numfmt(o.mean,dec),jia,numfmt(o.st,dec),sep="")
      p<-pp1;sv0<-sv1
    }
    if (ntp<0.05) {
      d1<-paste(scimedian," (",sciq1,"-",sciq3,")",sep="")
      o.d<-paste(o.median," (",o.q1,"-",o.q3,")",sep="")
      p<-pp1.npr;sv0<-sv1.npr
    }
    if (mvars[i] %in% nonnormal) {
      d1<-paste(scimedian," (",sciq1,"-",sciq3,")",sep="")
      o.d<-paste(o.median," (",o.q1,"-",o.q3,")",sep="")
      p<-pp1.npr;sv0<-sv1.npr
    }
    d0<-rbind(d0,d1);pp<-rbind(pp,p);sv<-rbind(sv,sv0);o.dd<-rbind(o.dd,o.d);o.smd<-rbind(o.smd,smds)
  }
  varnames<-paste(x,".",levels(factor(xvt)),sep="")
  if (Overall==FALSE) {
    dd<-rbind(scinn,d0)
  } else if (Overall==TRUE) {
    d0<-cbind(o.dd,d0)
    scinn<-c(o.n,scinn)
    dd<-rbind(scinn,d0)
    varnames<-c("Overall",varnames)
  } else {
    stop("Overall can only be FALSE or TRUE")
  }
  if (statistic==T) {
    dd1<-cbind(dd,c("",sv),c("",pp))
    varnames<-c(varnames,"statistical value")
  } else {dd1<-cbind(dd,c("",pp))}
  if (smd==T) {
    dd1<-cbind(dd1,c("",o.smd))
  }
  dd2<-cbind(c("N",mvars),dd1)
  if (smd==T) {
    colnames(dd2)<-c("Characteristic",varnames,"p value","smd")
  } else {colnames(dd2)<-c("Characteristic",varnames,"p value")
  }
  out1<-list(data=dd2,smd=o.smd)
  out1
}


