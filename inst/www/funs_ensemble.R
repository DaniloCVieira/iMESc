
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
      caret::multiClassSummary(pred_tab, lev=levels(obs))
    } else if(fun=='postResample'){
      caret::postResample(pred, obs)
    }
  } else{
    caret::postResample(pred, obs)
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
        caret::postResample(m$pred$pred,m$pred$obs)
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
        res<-caret::postResample(pred,obc)
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
        caret::postResample(m$pred$pred,m$pred$obs)
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
        res<-caret::postResample(pred,obc)
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

