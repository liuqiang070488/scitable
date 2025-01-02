pvformat<-function(p,dec) {
  pp <- sprintf(paste("%.",dec,"f",sep=""),as.numeric(p))
  if (is.matrix(p)) {pp<-matrix(pp, nrow=nrow(p)); colnames(pp)<-colnames(p);rownames(pp)<-rownames(p);}
  lw <- paste("<",substr("0.00000000000",1,dec+1),"1",sep="");
  pp[as.numeric(p)<(1/10^dec)]<-lw
  return(pp)
}