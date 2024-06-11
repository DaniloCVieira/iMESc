rm(list=ls())
newmodels<-c("rf","nb",'knn','gbm','xyf',"glm","dnn","earth",
             "cforest",
             #"gamSpline",
             "gaussprRadial","svmLinear","svmRadial","svmRadialCost","avNNet",'nnet','pcaNNet',"rpart",
             #"blackboost",
             #"randomGLM",
             #'gbm_h2o',
             "monmlp","mlpML",
             #'rbf',
             'evtree')


models<-sapply(newmodels,function(x){
  info<-caret::getModelInfo(x,regex = F)

  res<-lapply(info,function(x) x$type)
  res[sapply(res,length)==2]
} )


models<-names(models)
model_labels<-sapply(models,function(x){
  info<-caret::getModelInfo(x,regex =F)
  as.character(sapply(info,function(x) x$label))
})
#model_labels<-sort(model_labels)
models<-names(model_labels)
model_library<-sapply(models,function(x){
  info<-caret::getModelInfo(x,regex =F)
  as.character(sapply(info,function(x) x$library))
})


model_parameters<-lapply(models,function(x){
  info<-caret::getModelInfo(x,regex =F)
  # info[[1]]$parameters
  lapply(info,function(x) x$parameters)[[1]]
})
names(model_parameters)<-models


model_varImp<-lapply(models,function(x){
  info<-caret::getModelInfo(x,regex =F)
  # info[[1]]$parameters
  lapply(info,function(x) x$varImp)[[1]]
})
names(model_varImp)<-models


model_fits<-lapply(models,function(x){
  info<-caret::getModelInfo(x,regex =F)
  # info[[1]]$parameters
  lapply(info,function(x) capture.output(x$fit))[[1]]
})
names(model_fits)<-models


model_tags<-lapply(models,function(x){
  info<-caret::getModelInfo(x,regex =F)
  # info[[1]]$parameters
  lapply(info,function(x)x$tags)[[1]]
})
names(model_tags)<-models
model_labels2<-names(models)<-paste(models,"-",model_labels)




train_functions<-list(
  rf='randomForest::randomForest',
  nb='klaR::NaiveBayes',
  knn=c('caret::knn3Train'),
  gbm='gbm::gbm',
  xyf='kohonen::supersom',
  glm='stats::glm',
  dnn='deepnet::sae.dnn.train',
  cforest="party::cforest",
  earth='earth::earth',
  gamSpline='gam::gam',
#  gaussprPoly='kernlab::gausspr',
  gaussprRadial='kernlab::gausspr',
  svmLinear='kernlab::ksvm',
  svmRadial='kernlab::ksvm',
  svmRadialCost='kernlab::ksvm',
  avNNet="caret::avNNet",
  nnet='nnet::nnet',
  pcaNNet='caret::pcaNNet',
  rpart="rpart::rpart",
 # blackboost='mboost::blackboost',
 # randomGLM="randomGLM::randomGLM",
  gbm_h2o="h2o::h2o.gbm",
  monmlp="monmlp::monmlp.fit",
  mlpML="RSNNS::mlp",
  rbf="RSNNS::rbf",
  evtree="evtree::evtree.control"

)



train_functions2<-lapply(train_functions,function(x){
  res=as.list(strsplit(x,"::")[[1]])
  names(res)<-c("package","topic")
  res
})
names(train_functions2)

torep<-names(models)[grepl("knn",names(models))]
position<-which(grepl("knn",names(models)))
model_labels3<-append(model_labels2, torep, after=position)


model_grid<-lapply(models,function(x){
  info<-caret::getModelInfo(x,regex =F)
  # info[[1]]$parameters
  info[[1]]$grid
})
names(model_grid)<-models
model_fits$svmLinear
svmRadial<-getModelInfo("svmRadial",regex=F)[[1]]
svmLinear<-getModelInfo("svmLinear",regex=F)[[1]]
gaussprRadial<-getModelInfo("gaussprRadial",regex=F)[[1]]
svmRadial$grid<-function (x, y, len = NULL, search = "grid") {
  sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit,
                            scaled = TRUE)
  if (search == "grid") {
    rng <- exp(extendrange(log(sigmas), f = 0.75))
    out <- expand.grid(sigma =round(sort(seq(rng[1],rng[2],len=len)),3),
                       C = 2^((1:len) - 3))
  }  else {
    rng <- extendrange(log(sigmas), f = 0.75)
    out <- data.frame(sigma = exp(runif(len, min = rng[1],
                                        max = rng[2])),
                      C = 2^runif(len, min = -5, max = 10))
  }
  out
}
gaussprRadial$grid<-function (x, y, len = NULL, search = "grid")
{

  sigmas <- kernlab::sigest(as.matrix(x),na.action = na.omit,
                            scaled = TRUE)
  if (search == "grid") {
    rng <- exp(extendrange(log(sigmas), f = 0.75))
    out <- data.frame(sigma =round(sort(seq(rng[1],rng[2],len=len)),3))
  }  else {
    rng <- extendrange(log(sigmas), f = 0.75)
    out <- data.frame(sigma = sort(exp(runif(len, min = rng[1],
                                             max = rng[2]))))
  }
  out
}
svmLinear$grid<-function (x, y, len = NULL, search = "grid") {
  if (search == "grid") {
    out <- data.frame(C = 2^((1:len) - 3))
  }  else {
    out <- data.frame(C = 2^runif(len, min = -5, max = 10))
  }
  out
}


model_grid$svmRadial<-svmRadial$grid
model_grid$svmLinear<-svmLinear$grid
model_grid$gaussprRadial<-gaussprRadial$grid

avNNet<-getModelInfo("avNNet",regex=F)[[1]]

avNNet$varImp<-function(object,...){

  getModelInfo("avNNet",regex = F)[[1]]$varImp
  res<-lapply(object$model,function(mod1){
    mod1$call$x<-object$xNames
    mod1$call$y<-object$obsLevels
    if(object$problemType=="Regression"){

      NeuralNetTools::olden(mod1,x_names=object$xNames,y_names="1",bar_plot =F)^2
    } else {

      imps<-do.call(cbind,lapply(object$obsLevels, function(lev){
        NeuralNetTools::olden(mod1,x_names=object$xNames,y_names=object$obsLevels,out_var =lev,bar_plot =F)
      }))
      colnames(imps)<-object$obsLevels
      #apply(imps^2,1,mean)
      imps^2
    }

  })
  res2<-sapply(1:ncol(res[[1]]),function(i){
    apply(sapply(res, function(x) x[,i]),1,mean)
  })

  rownames(res2)<-rownames(res[[1]])
  colnames(res2)<-colnames(res[[1]])
  res2
}

model_varImp$avNNet<-avNNet$varImp
model_library$avNNet<-c('nnet',"NeuralNetTools")






SL_models<-list(model_fits=model_fits,
                    model_grid=model_grid,
                    model_labels=model_labels,
                    model_labels2=model_labels2,
                    model_labels3=model_labels3,
                    model_library=model_library,
                    model_parameters=model_parameters,
                    model_tags=model_tags,
                    model_varImp=model_varImp,
                    models=models,
                    newmodels=newmodels,
                    train_functions=train_functions,
                    train_functions2=train_functions2,
                    gaussprRadial=gaussprRadial,
                    svmLinear=svmLinear,
                    svmRadial=svmRadial,
                    avNNet=avNNet)

  saveRDS(SL_models,"inst/www/SL_models.rds")

