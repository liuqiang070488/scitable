code<-function(type=NULL) {
  if (is.null(type)) {type<-"A"} else {type<-type}
  if (type=="A") {
    o<-stringi::stri_unescape_unicode("\\u00b1")
  }
  if (type=="B") {
    a <- as.raw(as.hexmode("c2"))
    b <- as.raw(as.hexmode("b1"))
    xxx <- c(a, b)
    o<-rawToChar(xxx)
  }
  if (type=="C") {
    a <- as.raw(as.hexmode("a1"))
    b <- as.raw(as.hexmode("c0"))
    xxx <- c(a, b)
    o<-rawToChar(xxx)
  }
  o
}