numfmt<-function(p,dec) {
  if (is.list(p)) p<-as.matrix(p)
  pp <- sprintf(paste("%.",dec,"f",sep=""),as.numeric(p))
  if (is.matrix(p)) {pp<-matrix(pp, nrow=nrow(p));colnames(pp)<-colnames(p);rownames(pp)<-rownames(p);}
  pp[as.numeric(p)>10000000]<- "inf."
  pp[is.na(p) | gsub(" ","",p)==""]<- ""
  pp[p=="-Inf"]<-"-Inf"
  pp[p=="Inf"]<-"Inf"
  return(pp)
}