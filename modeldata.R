utils::globalVariables(c('as.formula',
                         'update'
))


modeldata<-function(fit,newdata=NULL,crrmol=FALSE){
  if ('coxph' %in% class(fit) & crrmol==FALSE){
    formu1=paste0('~',paste0(model.x(fit),collapse = '+'))
    formu=as.formula(paste0('Surv(',paste0(model.y(fit),collapse = ','),')',formu1))
    fit2=update(object = fit,formu,model=TRUE,x=TRUE,y=TRUE)
    fit2$model$timeggg=as.numeric(fit2$model[,1])[1:nrow(fit2$model)]
    fit2$model$eventggg=as.numeric(fit2$model[,1])[-c(1:nrow(fit2$model))]
    colnames(fit2$model)[(ncol(fit2$model)-1):ncol(fit2$model)]=model.y(fit2)
    fit2$model=fit2$model[,-1]
    data<-fit2$model
  }
  if ('glm' %in% class(fit)){
    data<-fit[["data"]]
  }
  if ('coxph' %in% class(fit) & crrmol==TRUE) {
    formu1=paste0('~',paste0(model.x(fit),collapse = '+'))
    y1<-paste0(model.y(fit)[2],"==1")
    formu2<-paste0(model.y(fit)[1],',',y1)
    formu=as.formula(paste0('Surv(',formu2,')',formu1))
    fit2=update(object = fit,formu,model=TRUE,x=TRUE,y=TRUE)
    fit2$model$timeggg=as.numeric(fit2$model[,1])[1:nrow(fit2$model)]
    fit2$model$eventggg=as.numeric(fit2$model[,1])[-c(1:nrow(fit2$model))]
    colnames(fit2$model)[(ncol(fit2$model)-1):ncol(fit2$model)]=model.y(fit2)
    fit2$model=fit2$model[,-1]
    data<-fit2$model
  }
  data
}

model.y<-function(fit){
  if ('coxph' %in% class(fit)){
    modely<-all.vars(fit$terms)[c(1,2)]
  }
  if ('glm' %in% class(fit)){
    modely<-all.vars(fit$terms)[c(1)]
  }
  if ('randomForest' %in% class(fit)){
    modely<-all.vars(fit$terms)[c(1)]
  }
  if ('ksvm' %in% class(fit)){
    modely<-all.vars(fit@terms)[c(1)]
  }
  if ('svm' %in% class(fit)){
    modely<-all.vars(fit$terms)[c(1)]
  }
  modely
}
model.x<-function(fit){
  if ('coxph' %in% class(fit)){
    modelx<-all.vars(fit$terms)[-c(1,2)]
  }
  if ('glm' %in% class(fit)){
    modelx<-all.vars(fit$terms)[-c(1)]
  }
  if ('randomForest' %in% class(fit)){
    modelx<-all.vars(fit$terms)[-c(1)]
  }
  if ('ksvm' %in% class(fit)){
    modelx<-all.vars(fit@terms)[-c(1)]
  }
  if ('svm' %in% class(fit)){
    modelx<-all.vars(fit$terms)[-c(1)]
  }
  modelx
}
