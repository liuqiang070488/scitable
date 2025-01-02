nt<-function(vt,kind=NULL) {
  if (is.null(kind)) {
    if (length(vt) < 3000) {
      ntp<-nortest::sf.test(vt)
    } else {ntp<-nortest::lillie.test(vt)}
  } else if (kind=="ad") {
    ntp<-nortest::ad.test(vt)
  } else if (kind=="cvm") {
    ntp<-nortest::cvm.test (vt)
  } else if (kind=="pearson") {
    ntp<-nortest::pearson.test(vt)
  }
  nt1<-ntp[["p.value"]]
  nt1
}
