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
get_ensemble_pred<-function(predtab,modelist, en_method=c("weighted","non-weighted"), obc=NULL, weitype,newdata,weis){
  type<-modelist[[1]]$modelType
  if(type=="Regression"){
    if(en_method=="non-weighted"){
      get_mavoting(predtab,modelist)
    } else{
      get_weimean(predtab,weis)
    }

  } else{
    if(en_method=="non-weighted"){
      get_mavoting(predtab,modelist)
    } else{
      get_weivoting(predtab,weis,modelist)
    }
  }

}

get_weimean<-function(predtab,weis){
  req(length(weis)==ncol(predtab))
  res<-data.frame(
    apply(predtab,1,function(x){
      weighted.mean(x, weis)

    })
  )
  colnames(res)<-"Weighted_mean"
  res
}
get_mavoting<-function(predtab,modelist){
  type=modelist[[1]]$modelType
  if(is.factor(predtab[[1]])){
    col_name<-c("Majority votes")
    res<-data.frame(as.factor(names(unlist(lapply(apply(do.call(data.frame,predtab),1,function(x) table(x)), function(x) which.max(x))))))

  }else{
    col_name<-c("Mean")
    res<-data.frame(apply(do.call(data.frame,predtab),1,function(x) mean(x)))
  }
  rownames(res)<-  rownames(predtab)
  colnames(res)<-col_name
  res
}
get_weivoting<-function(predtab,weis,modelist){
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
