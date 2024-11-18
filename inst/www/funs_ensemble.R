predcomb_table<-function(modelist,newdata, progress=T){
  res_compre<-lapply(modelist,newdata=newdata,function(x,newdata){
    trainingData<-x$trainingData
    trainingData<-trainingData[-which(colnames(trainingData)==".outcome")]
    res<-suppressWarnings(predict(x,newdata= newdata[which(colnames(newdata)%in%colnames(trainingData))]))
    res

  })
  res_compre<-data.frame(res_compre)
  rownames(res_compre)<-rownames(newdata)
  colnames(res_compre)<-names(modelist)
  attr(res_compre,"rownames")<-rownames(newdata)
  res_compre
}

get_ensemble_pred<-function(predtab,modelist, en_method=NULL, obc=NULL, weitype=NULL,newdata=NULL,weis, prob=F){


  type<-modelist[[1]]$modelType
  if(type=="Regression"){
    get_weimean(predtab,weis)
  } else{
    get_weivoting(predtab,weis,modelist, prob=prob)
  }

}



#' @export
get_weimean<-function(predtab,weis){
  req(length(weis)==ncol(predtab))
  res<-data.frame(
    apply(predtab,1,function(x){
      weighted.mean(x, weis,na.rm=T)

    })
  )
  colnames(res)<-"Weighted_mean"
  res
}

#' @export
get_weivoting<-function(predtab,weis,modelist, prob=F){
  if(isTRUE(prob)){
    levels_obc<-modelist[[1]]$levels
    req(length(names(weis))>0)
    names(weis)<-names(modelist)
    colnames(predtab)<-names(modelist)
    inst=1
    res_temp<-t(sapply(1:nrow(predtab),function(inst){
      i<-predtab[inst,, drop=F]
      sapply(levels_obc,function(lev){
        pic<-i[which(i==lev)]
        sum(weis[names(pic)])
      })
    }))
    res<-data.frame(t(apply(res_temp,1,function(x) x/sum(x))))
    colnames(res)<-levels_obc
    rownames(res)<-rownames(predtab)
    return(res)

  }
  levels_obc<-modelist[[1]]$levels
  req(length(names(weis))>0)
  names(weis)<-names(modelist)
  colnames(predtab)<-names(modelist)
  res<-unlist(
    lapply(1:nrow(predtab),function(inst){
      i<-predtab[inst,, drop=F]
      win<-which.max(
        unlist(
          lapply(levels_obc,function(lev){
            pic<-i[which(i==lev)]
            sum(weis[names(pic)])
          })
        )
      )
      levels_obc[win]

    })
  )
  names(res)<-rownames(predtab)
  res<-data.frame(res)
  colnames(res)<-"Majority weighted votes"
  res

}

get_metrics_ensemble<-function(modelist,pred,obs){




  if(modelist[[1]]$modelType=="Classification"){


    pred_tab<-data.frame(pred=factor(pred,levels(obs)),obs=obs)
    metric1<-caret::multiClassSummary(pred_tab, lev=levels(obs))
    metric<-metric1
  } else{


    metric_reg<-c(
      F1_Score=MLmetrics::F1_Score(pred,obs),
      FBeta_Score=MLmetrics::FBeta_Score(pred,obs),
      Gini=MLmetrics::Gini(pred,obs),
      MAE=MLmetrics::MAE(pred,obs),
      MAPE=MLmetrics::MAPE(pred,obs),
      MedianAE=MLmetrics::MedianAE(pred,obs),
      MedianAPE=MLmetrics::MedianAPE(pred,obs),
      MSE=MLmetrics::MSE(pred,obs),
      NormalizedGini=MLmetrics::NormalizedGini(pred,obs),
      Poisson_LogLoss=MLmetrics::Poisson_LogLoss(pred,obs),
      R2_Score=MLmetrics::R2_Score(pred,obs),
      RAE=MLmetrics::RAE(pred,obs),
      RMSE=MLmetrics::RMSE(pred,obs),
      RMSLE=MLmetrics::RMSLE(pred,obs),
      RMSPE=MLmetrics::RMSPE(pred,obs),
      RRSE=MLmetrics::RRSE(pred,obs)

    )

    metric<-metric_reg

  }
  metric
}






#' @export
ensemble_find<-function(models_grid,modelist,weis,predtab,newdata,obc,  en_method,show_progress=T, pararell=F,session=MockShinySession$new()){
  models<-colnames(predtab)
  models_grid<-do.call("c", lapply(seq_along(models), function(i) combn(models, i, FUN = list)))
  rep=length(models_grid)
  picprogress<-function(n) paste0(round((n/rep),2)*100,"%")
  if(isTRUE(show_progress)){
    if(isTRUE(pararell)){
      progress <- shiny::Progress$new()
      progress$set(message = "Parallelizing the run...", value = 0)
      prog<-function(n) progress$set(message =  picprogress(n), value = (n/rep))
    } else{
      prog<-function(n) message(picprogress(n))
    }

  } else{
    prog<-function(n) message(picprogress(n))
  }
  opts <- list(progress=prog)
  if(isTRUE(pararell)){
    res<-foreach(i = 1:length(models_grid), .combine = data.frame, .packages ="shiny",.options.snow = opts) %do% {get_ensemble_pred(
      predtab=predtab[,models_grid[[i]],drop=F],
      modelist=modelist[models_grid[[i]]],
      weis=weis[models_grid[[i]]],
      newdata=newdata,weitype=NULL,obc=NULL,en_method=en_method)[,1]}
    if(isTRUE(show_progress)){
      progress$close()
    }
    res<-t(res)
  } else{
    withProgress(session=session,message="Running", max=rep,{
      res<-lapply(1:length(models_grid),function(i){
        prog(i)
        pred<-get_ensemble_pred(
          predtab=predtab[,models_grid[[i]],drop=F],
          modelist=modelist[models_grid[[i]]],
          weis=weis[models_grid[[i]]],
          newdata=newdata,weitype=NULL,obc=NULL,en_method=en_method)[,1]
        incProgress(1,message=paste0("Making predictions...",round(i/length(models_grid),2)*100,"%"), session=session)
        pred

      })

      res<- do.call(cbind,res)

    })

  }



  res


}
#' @export
ensemble_find_auc<-function(df_pred,tabprob,obc, show_progress=T, pararell=F, session=MockShinySession$new()){
  rep=length(tabprob)
  picprogress<-function(n) paste0(round((n/rep),2)*100,"%")
  if(isTRUE(show_progress)){
    if(isTRUE(pararell)){
      progress <- shiny::Progress$new()
      progress$set(message = "Parallelizing the run...", value = 0)
      prog<-function(n) progress$set(message =  picprogress(n), value = (n/rep))
    } else{
      prog<-function(n) message(picprogress(n))
    }
  } else{
    prog<-function(n) message(picprogress(n))
  }

  #x=7
  #x<-tabprob[[7]]
  opts <- list(progress=prog)
  if(isTRUE(pararell)){
    aucs<-foreach(x = tabprob, .packages ="shiny",.options.snow = opts) %do% {
      pic<-which(!is.na(rowSums(x)))
      pROC::multiclass.roc(obc[pic], x[pic,])$auc
    }

    if(isTRUE(show_progress)){
      progress$close()
    }

  } else{
    withProgress(max=ncol(df_pred),message="Calculating AUCs...",session=session,{
      aucs<-sapply(tabprob,function(x){
        incProgress(1,session=session)
        pic<-which(!is.na(rowSums(x)))
        pROC::multiclass.roc(obc[pic], x[pic,])$auc

      })

    })
  }
  aucs
}
#' @export
ensemble_find_metrics<-function(modelist,df_pred,obc,  show_progress=T, pararell=F,session=MockShinySession$new(), fun='multiClassSummary'){
  picprogress<-function(n) paste0(round((n/rep),2)*100,"%")
  if(isTRUE(show_progress)){
    if(isTRUE(pararell)){
      progress <- shiny::Progress$new()
      progress$set(message = "Parallelizing the run...", value = 0)
      prog<-function(n) progress$set(message =  picprogress(n), value = (n/rep))
    } else{
      prog<-function(n) message(picprogress(n))
    }
  } else{
    prog<-function(n) message(picprogress(n))
  }

  opts <- list(progress=prog)
  if(isTRUE(pararell)){
    metrics_caret<-foreach(x = df_pred, .combine = cbind, .packages ="shiny",.options.snow = opts) %do% {
      get_metrics_ensemble2(modelist,x,obc, fun=fun)
    }
    if(isTRUE(show_progress)){
      progress$close()
    }

  } else{
    withProgress(session=session,max=ncol(df_pred),message="Calculating metrics...",{
      metrics_caret<-sapply(df_pred,function(x){
        incProgress(1,session=session)
        get_metrics_ensemble2(modelist,x,obc, fun=fun)
      })
    })
  }

  metrics_caret
}
#' @export
ensemble_find_probs<-function(models_grid,modelist,weis,predtab,  show_progress=T, pararell=F, session=MockShinySession$new()){

  rep=length(models_grid)
  picprogress<-function(n) paste0(round((n/rep),2)*100,"%")
  if(isTRUE(show_progress)){
    if(isTRUE(pararell)){
      progress <- shiny::Progress$new()
      progress$set(message = "Parallelizing the run...", value = 0)
      prog<-function(n) progress$set(message =  picprogress(n), value = (n/rep))
    } else{
      prog<-function(n) message(picprogress(n))
    }
  } else{
    prog<-function(n) message(picprogress(n))
  }

  opts <- list(progress=prog)
  if(isTRUE(pararell)){
    res<-foreach(i = 1:length(models_grid), .packages ="shiny",.options.snow = opts) %do% {
      get_ensemble_pred(predtab[,models_grid[[i]],drop=F],modelist[models_grid[[i]]],weis=weis[models_grid[[i]]], prob=T)
    }


    if(isTRUE(show_progress)){
      progress$close()
    }

  } else{
    withProgress(session=session,message="Gathering prob tables for AUCs...", max=rep,{
      res<-lapply(1:length(models_grid),function(i){
        prog(i)
        pred<-get_ensemble_pred(predtab[,models_grid[[i]],drop=F],modelist[models_grid[[i]]],weis=weis[models_grid[[i]]], prob=T)
        incProgress(1,message=paste0("Gathering prob tables...",round(i/length(models_grid),2)*100,"%"), session=session)
        pred

      })

    })
  }

  res
}
#' @export
get_metrics_ensemble2<-function(modelist,pred,obs, fun='multiClassSummary'){
  if(modelist[[1]]$modelType=="Classification"){
    pred_tab<-data.frame(pred=factor(pred,levels(obs)),obs=obs)
    metric<- if(fun=='multiClassSummary'){
      multiClassSummary(pred_tab, lev=levels(obs))
    } else if(fun=='postResample'){
      postResample(pred, obs)
    }
  } else{
    postResample(pred, obs)
  }

}


#' @export
get_wei<-function(predtab,newdata,modelist,obc,en_method,wei_datatype){

  class_whei<-if(modelist[[1]]$modelType=="Classification"){
    getClass_wei(
      predtab,
      obc,
      modelist,
      en_method=en_method,
      weitype=wei_datatype,
      newdata=newdata
    )
  } else{
    args<-list(modelist=modelist,newdata=newdata,obc=obc,en_method=en_method,weitype=wei_datatype)
    do.call(getReg_wei,args)
  }
  if(!is.null(class_whei)){
    class_whei<-as.vector(class_whei)
    names(class_whei)<-names(modelist)
    return(class_whei)
  }
}


#' @export
pdp_ensemble<-function (object, pred.var, pred.grid=NULL, pred.fun = NULL, grid.resolution = NULL,ice = FALSE, center = FALSE, approx = FALSE, quantiles = FALSE,probs = 1:9/10, trim.outliers = FALSE, type = c("auto", "regression","classification"), inv.link = NULL, which.class = 1L,prob = FALSE, recursive = TRUE, plot = FALSE, plot.engine = c("lattice","ggplot2"), smooth = FALSE, rug = FALSE, chull = FALSE,levelplot = TRUE, contour = FALSE, contour.color = "white",alpha = 1, train=NULL, cats = NULL, check.class = TRUE, progress = FALSE,parallel = FALSE, paropts = NULL, ...) {

  if (!is.null(inv.link)) {
    inv.link <- match.fun(inv.link)
  }
  if (is.null(train)) {
    train <- pdp:::get_training_data.train(object)
  }
  pdp:::get_training_data.train
  if (inherits(object, "xgb.Booster") && inherits(train, "data.frame")) {
    train <- data.matrix(train)
  }
  if (is.numeric(pred.var)) {
    pred.var <- colnames(train)[pred.var]
  }
  if (!all(pred.var %in% colnames(train))) {
    stop(paste(paste(pred.var[!(pred.var %in% colnames(train))],
                     collapse = ", "), "not found in the training data."))
  }
  if ("yhat" %in% pred.var) {
    stop("\"yhat\" cannot be a predictor name.")
  }
  if (length(pred.var) != 1 && ice) {
    stop("ICE curves cannot be constructed for multiple predictors.")
  }
  # pred.grid0<-pred.grid
  pred.grid <- if (is.null(pred.grid)) {
    pred.grid <-pdp:::pred_grid(train = train, pred.var = pred.var, grid.resolution = grid.resolution,
                                quantiles = quantiles, probs = probs, trim.outliers = trim.outliers,
                                cats = cats)
  }  else {
    if (!is.data.frame(pred.grid)) {
      stop("`pred.grid` shoud be a data frame.")
    }    else {
      if (!all(pred.var %in% colnames(pred.grid))) {
        stop(paste(paste(pred.var[!(pred.var %in% colnames(pred.grid))],
                         collapse = ", "), "not found in pred.grid."))
      }      else {
        if (quantiles || trim.outliers) {
          warning(paste("Options `quantiles` and `trim.outliers`",
                        "ignored whenever `pred.grid` is specified."))
        }
        pdp:::order_grid.data.frame(pred.grid)
      }
    }
  }

  if (inherits(train, "data.frame") && check.class) {
    pred.grid <- pdp:::copy_classes(pred.grid, train)
  }
  if (inherits(train, "matrix")) {
    pred.grid <- data.matrix(pred.grid)
  }
  if (inherits(train, "dgCMatrix")) {
    pred.grid <- methods::as(data.matrix(pred.grid), "dgCMatrix")
  }
  if (chull) {
    pred.grid <- pdp:::train_chull(pred.var, pred.grid = pred.grid,
                                   train = train)
  }

  type <- match.arg(type)
  if (type == "auto" && is.null(pred.fun)) {
    type <- get_task(object)
  }
  if (inherits(object, "gbm") && recursive && ice) {
    warning("Recursive method not available for \"gbm\" objects when `ice = ",
            "TRUE`. Using brute force method instead.")
  }
  if (isTRUE(approx)) {
    train <- exemplar(train)
  }
  if (inherits(object, "gbm") && recursive && !ice) {
    if (!is.null(inv.link)) {
      warning("`inv.link` option ignored whenever `recursive = TRUE`")
    }
    if (!is.null(pred.fun)) {
      stop("Option `pred.fun` cannot currently be used when ",
           "`recursive = TRUE`.")
    }
    if (isTRUE(progress)) {
      message("progress bars are not availble when `recursive = TRUE`.",
              call. = FALSE)
    }
    if (parallel) {
      stop("parallel processing cannot be used when `recursive = TRUE`.",
           call. = FALSE)
    }
    pd.df <- pardep_gbm(object, pred.var = pred.var, pred.grid = pred.grid,
                        which.class = which.class, prob = prob, ...)
    class(pd.df) <- c("partial", "data.frame")
    names(pd.df) <- c(pred.var, "yhat")
    rownames(pd.df) <- NULL
  }  else {
    pd.df <- if (type %in% c("regression", "classification") ||
                 !is.null(pred.fun)) {
      ensemble_pardep(object, pred.var = pred.var, pred.grid = pred.grid,
                      pred.fun = pred.fun, inv.link = inv.link, ice = ice,
                      task = type, which.class = which.class, logit = !prob,
                      train = train, progress = progress, parallel = parallel,
                      paropts = paropts)
    }    else {
      stop("partial dependence and ICE are currently only available for ",
           "classification and regression problems.", call. = FALSE)
    }
    if (is.matrix(pd.df)) {
      pd.df <- as.data.frame(pd.df)
    }
    if (inherits(pd.df, what = "dgCMatrix")) {
      pd.df <- as.data.frame(as.matrix(pd.df))
    }
    if (isTRUE(ice) || ("yhat.id" %in% names(pd.df))) {
      class(pd.df) <- c("ice", "data.frame")
      pd.df <- pd.df[order(pd.df$yhat.id), ]
      if (isTRUE(center)) {
        pd.df <- center_ice_curves(pd.df)
        if (type == "classification" && prob) {
          warning("Centering may result in probabilities outside of [0, 1].")
          pd.df$yhat <- pd.df$yhat + 0.5
        }
      }
    }    else {
      class(pd.df) <- c("partial", "data.frame")
    }
    rownames(pd.df) <- NULL
  }
  if (plot) {
    plot.engine <- match.arg(plot.engine)
    res <- if (inherits(pd.df, what = c("ice", "cice"))) {
      if (plot.engine == "ggplot2") {
        autoplot(object = pd.df, plot.pdp = TRUE, rug = rug,
                 train = train, alpha = alpha)
      }      else {
        plotPartial(object = pd.df, plot.pdp = TRUE,
                    rug = rug, train = train, alpha = alpha, )
      }
    }    else {
      if (plot.engine == "ggplot2") {
        autoplot(pd.df, smooth = smooth, rug = rug, train = train,
                 contour = contour, contour.color = contour.color,
                 alpha = alpha)
      }    else {
        plotPartial(pd.df, smooth = smooth, rug = rug,
                    train = train, levelplot = levelplot, contour = contour,
                    contour.color = contour.color, screen = list(z = -30,
                                                                 x = -60))
      }
    }
    attr(res, "partial.data") <- pd.df
  }  else {
    res <- pd.df
  }
  res
}



#' @export
getReg_wei<-function(modelist,newdata,obc, en_method,weitype){
  #req(is.numeric(obc))
  weis<-rep(1,length(modelist))
  if(en_method%in%c("non-weighted")){
    return(weis)
  }
  m<-modelist[[1]]
  class=which(colnames(m$trainingData)==".outcome")
  traindata<-data.frame(m$trainingData[-class])
  #req(sum(colnames(newdata)%in%colnames(colnames(m$trainingData)[-class])==ncol(newdata)))
  #req(sum(names(obc)%in%rownames(newdata))==length(obc))

  if(en_method=="accu_weighted") {
    if(weitype=="training"){
      weis<-do.call(rbind,lapply(modelist,function(m){
        postResample(m$pred$pred,m$pred$obs)
      }))
      weis<-if(m$modelType=="Classification"){weis[,"Accuracy"]
      } else{weis[,'Rsquared']}
    }
    if(weitype=="test") {
      newdata<-newdata[names(obc),colnames(m$trainingData)[-class]]
      req(nrow(newdata)==length(obc))

      weis<-unlist(do.call(data.frame,lapply(modelist,function(m){
        pred<-suppressWarnings(predict(m,newdata))
        req(length(pred)==length(obc))
        res<-postResample(pred,obc)
        res<-if(m$modelType=="Classification"){res["Accuracy"]
        } else{res['Rsquared']}
        res
      }
      )))
    }
    weis<-weis/sum(weis)
    weis
    #weis<-getAccu_wei(predtab,obc)

  }
}
#' @export
getClass_wei<-function(predtab,obc,modelist, en_method, weitype="test",newdata){
  #req(is.factor(obc))
  req(length(predtab)>0)
  req(ncol(predtab)>1)
  req(length(weitype)==1)

  weis<-rep(1,ncol(predtab))
  if(en_method%in%c("non-weighted")){
    names(weis)<-names(modelist)
    return(weis)
  }
  m<-modelist[[1]]
  class=which(colnames(m$trainingData)==".outcome")
  traindata<-data.frame(m$trainingData[-class])
  if(weitype=="training"){
    newdata<-traindata
    predtab<-predcomb_table(modelist,newdata, progress=F)
    obc_train<-m$trainingData[[class]]
    names(obc_train)<-rownames(traindata)
    obc<-obc_train

  }
  newdata<-newdata[,colnames(m$trainingData)[-class]]
  tab1<-apply(predtab, 2,function(x){
    as.numeric(x==obc)
  })
  if(en_method=="weighted") {
    for(i in 1:nrow(tab1)){
      for(j in 1:length(weis)){
        ai=sum(tab1[i,]==0, na.rm=T)/ncol(tab1)
        if(!is.na(tab1[i,j])){weis[j]<-if(tab1[i,j]==1){weis[j]+ai} else {weis[j]}}

      }
    }
    names(weis)<-names(modelist)
    return(weis)
  }

  if(en_method=="accu_weighted") {
    if(weitype=="training"){
      weis<-do.call(rbind,lapply(modelist,function(m){
        postResample(m$pred$pred,m$pred$obs)
      }))
      weis<-if(m$modelType=="Classification"){weis[,"Accuracy"]
      } else{weis[,'Rsquared']}
      weis<-weis/sum(weis)
      names(weis)<-names(modelist)
      return(weis)
    }
    if(weitype=="test") {
      weis<-unlist(do.call(data.frame,lapply(modelist,function(m){
        pred<-suppressWarnings(predict(m,newdata=newdata))
        res<-postResample(pred,obc)
        res<-if(m$modelType=="Classification"){res["Accuracy"]
        } else{res['Rsquared']}
        res
      }
      )))
      weis<-weis/sum(weis)
      names(weis)<-names(modelist)
      return(weis)
    }
    #weis<-getAccu_wei(predtab,obc)

  }


  weis
}
#' @export
getAccu_wei<-function(predtab,obc){
  model_perfomaces<-data.frame(t(apply(predtab,2,function(i){
    postResample(i,obc)
  })))
  weis<-model_perfomaces$Accuracy
  names(weis)<-colnames(predtab)
  weis
}





#' @export
predcomb_table<-function(modelist,newdata, progress=T){
  if(isTRUE(progress)){
    session=getDefaultReactiveDomain()
  } else{
    session<-MockShinySession$new()
  }

  withProgress(min=0, max=length(modelist),message="Running...",session=session,{
    res_compre<-lapply(modelist,newdata=newdata,function(x,newdata){
      try({
        if(isTRUE(progress)){   incProgress(1)}

        trainingData<-x$trainingData
        trainingData<-trainingData[-which(colnames(trainingData)==".outcome")]
        res<-suppressWarnings(predict(x,newdata= newdata[which(colnames(newdata)%in%colnames(trainingData))]))
        res
      })
    })
  })

  pic<-which(unlist(lapply(res_compre,function(x)class(x)=="try-error")))
  if(length(pic)>0){
    res_compre<-res_compre[-pic]
  }
  res_compre<-data.frame(res_compre)
  rownames(res_compre)<-rownames(newdata)
  colnames(res_compre)<-names(modelist)
  attr(res_compre,"rownames")<-rownames(newdata)
  res_compre


}

