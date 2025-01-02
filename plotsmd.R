#'@title  plotsmd
#'@name  plotsmd
#'
#'@description  You can use it to draw a baseline table of data.
#'@details The differences between variables can be represented using SMD. This program can draw SMD graphs of variable differences.
#'
#'@param vars List the variables you need to compare.
#'@param unmatchdata Data before conducting propensity matching.
#'@param matchdata The data after propensity score matching.
#'@param refline Set a reference line with a default value of 0.1.
#'@param title The title of the image.
#'@param xlab The name of the X-axis.
#'@param ylab The name of the Y-axis.
#'
#'@return A picture.
#'@export
#'
#'@usage plotsmd(vars,unmatchdata,matchdata,refline=NULL,title=NULL,xlab='SMD',ylab='variable')
#'
#'
#'




utils::globalVariables(c('melt',
                         'ggplot',
                         'aes',
                         'geom_line',
                         'geom_point',
                         'coord_flip',
                         'theme_bw',
                         'theme',
                         'element_blank',
                         'labs',
                         'geom_hline'
                         ))





plotsmd<-function(vars,unmatchdata,matchdata,refline=NULL,title=NULL,xlab='SMD',ylab='variable') {
  if (missing(vars)) {stop("Missing vars.")}
  if (missing(matchdata)) {stop("Missing matchdata.")}
  if (missing(matchdata)) {stop("Missing matchdata.")}
  variable<-NULL;SMD=NULL;Method=NULL
  var<-var
  unmatchdata<-as.data.frame(unmatchdata);matchdata<-as.data.frame(matchdata)
  if (missing(refline)) {refline<-0.1} else {refline<-refline}
  unmatchdata<-as.data.frame(unmatchdata)
  unpout<-as.data.frame(unmatchdata[match(vars,unmatchdata$Characteristic),])
  unpout<-data.frame(names=unpout$Characteristic,smd=unpout$smd)
  matchdata<-as.data.frame(matchdata)
  pout<-as.data.frame(matchdata[match(vars,matchdata$Characteristic),])
  pout<-data.frame(names=pout$Characteristic,smd=pout$smd)
  dataPlot <- data.frame(variable= vars,
                         Unmatched= unpout$smd,
                         Matched= pout$smd
  )
  dataPlotMelt <- reshape2::melt(data= dataPlot,
                       id.vars= c("variable"),
                       variable.name= "Method",
                       value.name= "SMD")
  dataPlotMelt$SMD <- gsub("<0.001", "0.001", dataPlotMelt$SMD)
  varNames <- as.character(dataPlot$variable)[order(dataPlot$Unmatched)]
  dataPlotMelt$variable <- factor(dataPlotMelt$variable,
                                  levels = varNames)
  dataPlotMelt$SMD<-as.numeric(dataPlotMelt$SMD)
  dataPlotMelt$Method<-factor(dataPlotMelt$Method)
  p<-ggplot(data = dataPlotMelt,
            mapping = aes(x = variable, y = SMD, group =Method, color = Method)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = refline, color = "black", size = 0.1) +
    coord_flip() +
    theme_bw() +
    theme(legend.key = element_blank())+
    labs(title = title, x = ylab, y = xlab)
  p
}
