
#' @export
stats_ml <- function(obs, pred,m) {
  # Para Classificação Binária
  if(is.factor(obs)) {
    #cat("Multi-class Classification Metrics:\n")
    metrics <-multiclass_stats(obs,pred,m)
  } else if(is.numeric(obs) & is.numeric(pred)) {
    #cat("Regression Metrics:\n")
    metrics <-regression_stats(obs,pred)

  }

  return(metrics)
}
#' @export
multiclass_stats<-function(obs,pred,m){
  predtab<-data.frame(pred)
  models<-colnames(predtab)
  models_grid<-do.call("c", lapply(seq_along(models), function(i) combn(models, i, FUN = list)))
  modelist=list(pred=m)
  weis=c(pred=1)
  tabprob<-ensemble_find_probs(models_grid,modelist,weis,predtab,  show_progress=T, pararell=F, session=MockShinySession$new())
  aucs<-ensemble_find_auc(predtab,tabprob,obs, show_progress=T, pararell=F, session=MockShinySession$new())
  metrics<-ensemble_find_metrics(modelist,predtab,obs,
                                 show_progress=T,
                                 pararell=F,
                                 #session=getDefaultReactiveDomain(),
                                 session=MockShinySession$new(),
                                 fun='multiClassSummary')
  metrics<-data.frame(metrics)
  metrics["multiClassAUC",1]<-aucs
  colnames(metrics)<-"metric"
  metrics
}
#' @export
regression_stats<-function(obs,pred){
  regression_metrics <- postResample(pred, obs)
  residuals <- obs - pred

  metrics <- data.frame(
    RMSE = rmse(obs, pred),
    Rsquared = rsq(obs, pred),
    MAE = mae(obs, pred),
    RMSESD = sd(residuals),
    RsquaredSD = sd(residuals^2),
    MAESD = sd(abs(residuals)),
    MAPE = mape(obs, pred)
  )
  df<-data.frame(metric=t(metrics))
  colnames(df)<-"metric"
  df
}
#' @export
classification_stats<-function(obs,pred){
  lev<-levels(c(obs,pred))
  metrics<-multiClassSummary(data.frame(obs=obs,pred=pred), lev=lev)
  as.data.frame(metrics)
}

#' @export
plot_interframe<-function(frame,x = 'mean_min_depth',y = 'no_of_nodes',z="accuracy_decrease",sig=0.05,xlab=x,ylab=y,zlab=z,palette="turbo",newcolhabs,main="",logx=F,logy=T,cex.axes=12,cex.lab=12,cex.main=12,cex.leg=12,size=3,label.size=0.5,   max.overlaps=15, xlim=NULL, ylim=NULL,only_sigs=T) {
  frame<-data.frame(frame)
  #if(!is.null(xlim)){xlim<-range(frame[,x], na.rm=T)}
  #if(is.null(ylim)){ylim<-range(frame[,y], na.rm=T)}
  if(isTRUE(only_sigs)){
    pic<-which(frame$p_value<sig)
    frame<-frame[pic,c("variable",x,y,if(z!="None"){z})]
  } else{
    frame<-frame[,c("variable",x,y,if(z!="None"){z})]
  }


  if(is.null(frame$z)){
    frame$z<-1
  }

  colnames(frame)<-c("variable","x","y","z")

  p<-ggplot(frame, aes(x = x, y = y, color=z,label=variable)) +
    geom_point(size=size)
  if(isTRUE(logx)){
    p<-p+ scale_x_log10()} else{
    p<-p+scale_x_continuous()
    }

  if(isTRUE(logy)){


    p<-p+ scale_y_log10()} else{
    p<-p+scale_y_continuous()
  }

  p<-p+xlab(xlab)+
    ylab(ylab) +
    guides(color=guide_legend(title=zlab))+
    scale_colour_gradientn (colours=getcolhabs(newcolhabs,palette,100), guide="none")+
    geom_text_repel(show.legend=F,max.overlaps =max.overlaps, size = label.size  )+
    ggtitle(main)+
    theme(
      panel.grid.major = element_blank(),
      panel.background=element_rect(fill=NA, color="gray"),
      panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab,face="bold"),
      plot.title=element_text(size=cex.main),
      legend.text=element_text(size=cex.leg),
      legend.title=element_text(size=cex.leg)
    )
  if(z=="None"){
    p<-p+ theme(legend.position="none")
  }
  return(p)


}
#' @export
getrf<-function(saved_data,rf_name){
  rf_attr<-attr(saved_data,"rf")
  if(is.list(rf_attr[[rf_name]][[1]])){
    rf_attr[[rf_name]][[1]]
  } else{
    rf_attr[[rf_name]]
  }}
#' @export
grups_pds<-function(res_pd,m=NULL){
  res0<-res1<-list()
  x<-res_pd
  res<-list()
  for(i in 1:length(x)){
    res[[i]]<-data.frame(x[[i]],groups=i)
  }
  res
}
#' @export
plot_pd_groups<-function(res_pd,m,best_pds,pd_size_plot=12,pd_palette="viridis",newcolhabs){
  gpds<-grups_pds(res_pd,m)

  gpds2<-gpds

  pd<-do.call(rbind,gpds)
  cols<-colnames(pd)
  colnames(pd)<-c("x","y","z","group")
  pd$group<-as.factor(pd$group)

  p<-ggplot(pd, aes(x = x, y = y, z = z, fill = z)) +
    geom_tile() +
    geom_contour(color = "white", alpha = 0.5) +
    scale_fill_gradientn(colours =scale_color_2(pd_palette,newcolhabs),name="Partial\ndependence (prob)") +
    theme_bw(base_size = pd_size_plot) +
    facet_grid(~ group)+
    xlab(cols[1])+
    ylab(cols[2])

  p
}
#' @export
loop_pd3<-function(m,interactions_frame,var0=1, var1=2, grid=10){
  withProgress(min=0, max=length(m$levels),message="Running...",{
    res000<-lapply(1:length(m$levels), function(j){
      incProgress(1)
      cat("\n",j)
      cat("\n","-------------- \n")

      pdp::partial(m, pred.var = c(var0,var1), type="classification",which.class=m$levels[j],
                   grid.resolution=grid,

                   prob=T,
                   chull=T)})


  })
  return(res000)
}


#' @export
container_global_caret<-function(table){
  #table=data.frame(table)
  #rownames(table)<-NULL
  #colnames(table)<-NULL
  ncol_base<-max(attr(table,"ncol_train"))
  cols<-colnames(table)
  if(ncol_base==length(cols)){
    res<-withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 0, ''),
          th(colspan = ncol_base, 'Training',style = "border-top: solid 1px;border-left: solid 1px;border-right: solid 1px;")),
        tr(
          th("Model"),
          th(cols[1],style="border-left: solid 1px;"),
          lapply(c(cols)[c(2:(ncol_base-1))],th),
          th(cols[ncol_base],style="border-right: solid 1px;"),
          #lapply(colnames(table),th)
        )

      )))} else{
        res<-withTags(table(
          class = 'display',
          thead(
            tr(
              th(colspan = 0, ''),
              th(colspan = ncol_base, 'Training',style = "border-top: solid 1px;border-left: solid 1px;border-right: solid 1px;"),
              th(colspan = length(cols)-ncol_base, 'Test',style = "border-top: solid 1px;border-right: solid 1px;")
            ),
            tr(
              th("Model"),
              th(cols[1],style="border-left: solid 1px;"),
              lapply(c(cols)[c(2:(ncol_base-1))],th),
              th(cols[ncol_base],style="border-right: solid 1px;"),
              lapply(c(cols)[c((ncol_base+1):(length(cols)-1))],th),
              th(cols[length(cols)],style="border-right: solid 1px;border-top: solid 1px"),
            )

          )))
      }
  res
}
#' @export
caret_global_stats<-function(models){
  pic<-which(unlist(lapply(models, function(x) class(x)[1]))=="train")

  models<-models[pic]
  class_models<-names(models)[which(unlist(lapply(models,function(m) m$modelType=="Classification")))]
  regs_models<-names(models)[which(unlist(lapply(models,function(m) m$modelType=="Regression")))]
  global_train<-lapply(models,rf_global_train)
  ncol_train<-lapply(global_train,ncol)
  global_test<-rf_global_predall(models)
  if(length(global_test)>0){
    res00<-list()
    i=1
    for(i in 1:length(global_test)){
      res00[[names(global_test)[i]]]<- data.frame(c(global_train[[which(names(global_train)==names(global_test)[i])]],global_test[[i]]))
    }
    global_train[names(res00)]<-res00}
  class<-data.frame(rbindlist(global_train[class_models], fill = TRUE))
  rownames(class)<-class_models
  regs<-data.frame(rbindlist(global_train[regs_models], fill = TRUE))
  rownames(regs)<-regs_models
  attr(class,"ncol_train")<-do.call(c,ncol_train[ rownames(class)])
  attr(regs,"ncol_train")<-do.call(c,ncol_train[ rownames(regs)])

  list(
    class=class,
    regs=regs
  )



}
#' @export
rf_global_train<-function(m){
  nobs=nrow(m$trainingData)
  Y=attr(m,"Y")
  data.frame(nobs=nobs,Y=Y, m$results[rownames(m$bestTune),])
}
#' @export
rf_global_predall<-function(models){
  pic<-which(unlist(lapply(models,function(x) is.data.frame(attr(x,"test")))))


  lapply(models[pic],rf_global_test)

}


#' @export
## check

rf_global_test<-function(m) {
  observed<-attr(m,"sup_test")
  if(class(m$finalModel)=="randomForest"){
    rf_pred <- randomForest:::predict.randomForest(m$finalModel,newdata = attr(m,"test"), predict.all=T)} else{
      rf_pred <- predict(m,newdata = attr(m,"test"))
    }
  if(m$modelType=="Regression"){
    if(is.list(rf_pred)){
      if(length(rf_pred$individual)>0){
        pred<-apply(rf_pred$individual,1,mean)
        stat<-postResample(pred,observed)
        stat
      }
    } else{
      stat<-postResample(rf_pred,observed)
      stat
    } } else{
      if(is.list(rf_pred)){
        if(length(rf_pred$individual)>0){
          pred<-apply(rf_pred$individual,1, function(x){
            res<-table(x)
            names(res)[ which.max(res)]})
        } else{
          pred<-rf_pred[[1]]
        }
      } else{
        pred<-rf_pred
      }



      stat<-postResample(pred,observed)


      actual<-as.numeric(as.character(pred)==as.character(observed))
      #auc<-  Metrics::auc(actual,pred)
      stat<-c(stat,
              #AUC=auc,
              use.names=T)
    }
  return(c(nobs=length(observed),stat))
}

#' @export
RFerror_class<-function(predicted,observed)
{suppressWarnings({

  res<-data.frame(predicted$individual)

  res0<-matrix(NA,length(observed),2, dimnames = list(c(rownames(res)),c('Accuracy','Error')))


  for ( i in 1:length(observed))
  {
    acc<-as.numeric(res[i,]==as.character(observed[i]))
    res0[i,'Accuracy']<-sum(acc)/length(res[i,])*100
    res0[i,'Error']<-100-res0[i,'Accuracy']

  }
  res0

})}

#' @export
RFerror_reg<-function(predicted,observed, pred_interval=0.05){
  suppressWarnings({
    pred.rf.int <- data.frame(
      t(apply(predicted$individual, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )
    colnames(pred.rf.int)<-paste0(c(pred_interval/2,   1-(pred_interval/2))*100,"%")



    res<-data.frame(predicted$individual)
    res0<-data.frame(matrix(NA,length(observed),8, dimnames = list(c(rownames(observed)),c("obs","pred",colnames(pred.rf.int),'mae','mse','rmse','mape'))))
    res0[,"obs"]<-observed
    res0[,"pred"]<-predicted$aggregate
    res0[,3:4]<-pred.rf.int

    for ( i in 1:length(observed)){
      res0[i,"mae"]<-Metrics::mae(as.numeric(observed[i]),as.numeric(res[i,]))
      res0[i,"mse"]<-Metrics::mse(as.numeric(observed[i]),as.numeric(res[i,]))
      res0[i,"rmse"]<-Metrics::rmse(as.numeric(observed[i]),as.numeric(res[i,]))
      res0[i,"mape"]<-Metrics::mape(as.numeric(observed[i]),as.numeric(res[i,]))


    }
    colnames(res0)[3:4]<-colnames(pred.rf.int)
    res0




  })
}

#' @export
rsq <- function (obs, pred) cor(obs, pred) ^ 2

#' @export

accu_som_class<-function(m)
{
  pred<-m$pred
  pred<-na.omit(pred)
  predf<-pred[1:2]
  rowind<-rownames(m$trainingData)[pred$rowIndex]
  res<-data.frame(do.call(rbind,lapply(split(predf,rowind),function(x){
    postResample(x$pred,x$obs)
  })))

  n_test<-data.frame(n_test=do.call(rbind,lapply(split(pred[1:2],pred$rowIndex),function(x){
    nrow(x)
  })))
  Acc_error<-NULL
  Kappa_error<-NULL
  if("Accuracy"%in%colnames(res)){
    res$Acc_error<-1-res$Accuracy
    res$Kappa_error<-NULL
    res$Kappa<-NULL

  }

  res$n_test<-n_test
  res

  return(res)
}
#' @export
accu_rf_class<-function(m)
{
  if(m$modelType=="Regression"){
    return(accu_rf_reg_model(m))
  }
  do.call(rbind,lapply(split(m$pred,m$pred$row),function(x){
    ps<-postResample(x$pred,x$obs)[m$metric]
    data.frame(rbind(ps),
               error=1-ps,
               n_test=length(x$pred)

    )
  }))}


#' @export
accu_rf_reg_model<-function(m)
{
 # x<-split(m$pred,m$pred$row)[[1]]
  do.call(rbind,lapply(split(m$pred,m$pred$row),function(x){
    x<-na.omit(x)
    x$pred<-as.numeric(x$pred)

    mape=Metrics::mape(x$pred,x$obs)
    mse=mse(x$obs,x$pred)
    mae=mae(x$obs,x$pred)
    rmse=rmse(x$obs,x$pred)
    data.frame(mape,mse,mae,rmse,
               n_test=length(x$pred)

    )
  }))


}

#' @export
accu_rf_reg<-function(observed,predicted)
{
  obspred<-data.frame(obs=observed, pred=predicted)
  reslist<-split(obspred,rownames(obspred))
  mae_res<-list()
  mape_res<-list()
  mse_res<-list()
  for(i in 1:length(observed))
  {
    mape_res[[i]]<-mape(observed[i],predicted[i])
    mae_res[[i]]<-mae(observed[i], predicted[i])
    mse_res[[i]]<-mse(observed[i],predicted[i])
  }
  mse_res<-data.frame(MSE=do.call(c,mse_res))
  mape_res<-data.frame(MAPE=do.call(c,mape_res))
  mae_df<-data.frame(MAE=do.call(c,mae_res))
  rownames(mse_res)<-names(observed)
  rownames(mape_res)<-names(observed)
  rownames(mae_df)<-names(observed)
  res<-data.frame(mse=mse_res, mae=mae_df,mape=mape_res)
  rownames(res)<-names(predicted)
  res$obs=observed
  res$pred=predicted
  return(res)
}
#' @export
getRF<-function(rfs,groups,somC, method="rf"){
  selection<-which(rfs$acutable$method==method&rfs$acutable$k==groups)
  RF=  rfs$trains[[selection]]
  col_vector=somC$colhabs
  sup="supervisor: Clusters from SOM"
  attr(RF,"supervisor")<-sup
  attr(RF,"col_vector")<-col_vector
  return(RF)
}


#' @export
wrapRF<-function(data,supv=NULL, ntree=500, seed=NULL,trainControl.args = list(method = 'repeatedcv', number = 2,repeats=1,savePredictions = "final"),preProcess="",...)
{

  if(preProcess==""){ preProcess=NULL}

  suppressWarnings({
    controle_treinamento=do.call(trainControl,trainControl.args)

    base<-na.omit(cbind(HAB=supv,data[rownames(supv),,drop=F]))
    colnames(base)[1]<-"HAB"


    if( class(unlist(supv))=="factor")
    {

    lev<-names(table(base$HAB)>0)[which(table(base$HAB)>0)]
    base$HAB<-factor(base$HAB,levels=lev, labels = lev)

    }
    set.seed(seed)
    modelo = caret::train(HAB ~ ., data = base, trControl = controle_treinamento, method ="rf", ntree=ntree, localImp = TRUE, preProcess=preProcess,keep.forest=T,keep.inbag=T,...)
    attr(modelo,"data")<-base
    return(modelo)
  })



}


#' @export
getConfusion<-function(rf){
  ConfMat<-list()
  if(class(rf)[1]=="train"){
    validate(need(rf$modelType=="Classification","Confusion Matrices are only valid for Regression models"))
    ConfMat<-confusionMatrix(rf)
  } else  if(class(rf)[1]=='table'){  ConfMat<-confusionMatrix(rf)} else{
  if(sum(names(rf)=="finalModel")==0)
  {
    ConfMat$table<-rf$confusion[,-c(ncol(rf$confusion))]
    ConfMat$text<-NULL
    ConfMat$table<-ConfMat$table/sum(ConfMat$table)
  } else {  ConfMat<-confusionMatrix(rf)}}

  CM<-CMerror<-ConfMat$table
  CM3<-as.data.frame(matrix(NA,nrow(CM),ncol(CM)))
  rownames(CM3)<-rownames(as.matrix(ConfMat$table))
  colnames(CM3)<-colnames(as.matrix(ConfMat$table))
  CM3[rownames(CM),colnames(CM)]<-CM
  diag(CMerror)<-0
  class.error<-rowSums(CMerror)/rowSums(CM)
  CM3[names(class.error),(ncol(CM3)+1)]<-data.frame(class.error=rowSums(CMerror)/rowSums(CM))[1]
  attr(CM3,"title")<- ConfMat$text
  return(CM3)
}
#' @export
multipimp<-function(rf, mean_sample = "top_trees", measures = NULL){


  if(class(rf)[1]=="randomForest"){
    forest=rf } else {forest=rf$finalModel}

  multi_imps = randomForestExplainer::measure_importance(forest,mean_sample=mean_sample,measures=measures)
  indicadores<-as.character(multi_imps[order(multi_imps$mean_min_depth),][,"variable"])
  min_depth_frame <- randomForestExplainer::min_depth_distribution(forest)
  res_multi<-list(min_depth_frame,multi_imps)
  return(res_multi)
}
#' @export
plot_mdd<-function (min_depth_frame, k = 10, min_no_of_trees = 0, mean_sample = "top_trees",mean_scale = FALSE, mean_round = 2, main = "Distribution of minimal depth and its mean",newcolhabs,palette="viridis",
                    size_mmd=5,size_plot=10){
  minimal_depth <- NULL
  mean_minimal_depth_label <- NULL
  mean_minimal_depth <- NULL
  if (any(c("randomForest", "ranger") %in% class(min_depth_frame))) {
    min_depth_frame <- randomForestExplainer::min_depth_distribution(min_depth_frame)
  }

  min_depth_count_list <- randomForestExplainer:::min_depth_count(min_depth_frame)
  min_depth_means <- randomForestExplainer:::get_min_depth_means(min_depth_frame, min_depth_count_list,
                                                                 mean_sample)
  frame_with_means <- merge(min_depth_count_list[[1]], min_depth_means)
  frame_with_means[is.na(frame_with_means$minimal_depth), "count"] <- frame_with_means[is.na(frame_with_means$minimal_depth),
                                                                                       "count"] - min(frame_with_means[is.na(frame_with_means$minimal_depth),
                                                                                                                       "count"])
  if (mean_scale == TRUE) {
    frame_with_means$mean_minimal_depth <- (frame_with_means$mean_minimal_depth -
                                              min(frame_with_means$mean_minimal_depth))/(max(frame_with_means$mean_minimal_depth) -
                                                                                           min(frame_with_means$mean_minimal_depth))
  }
  frame_with_means$mean_minimal_depth_label <- (frame_with_means$mean_minimal_depth -
                                                  min(frame_with_means$mean_minimal_depth))/(max(frame_with_means$mean_minimal_depth) -
                                                                                               min(frame_with_means$mean_minimal_depth)) * max(min_depth_count_list[[2]]$no_of_occurrences)
  variables <- min_depth_count_list[[2]][min_depth_count_list[[2]]$no_of_occurrences >=
                                           min_no_of_trees, "variable"]
  frame_with_means <- frame_with_means[frame_with_means$variable %in%
                                         variables, ]
  frame_with_means <- within(frame_with_means, variable <- factor(variable,
                                                                  levels = unique(frame_with_means[order(frame_with_means$mean_minimal_depth),
                                                                                                   "variable"])))
  data <- frame_with_means[frame_with_means$variable %in% levels(frame_with_means$variable)[1:min(k,
                                                                                                  length(unique(frame_with_means$variable)))], ]
  data$variable <- droplevels(data$variable)
  data_for_labels <- unique(data[, c("variable", "mean_minimal_depth",
                                     "mean_minimal_depth_label")])
  data_for_labels$mean_minimal_depth <- round(data_for_labels$mean_minimal_depth, digits = mean_round)



  #colors<-colors[mdepth]
  plot<- ggplot(data, aes(x = variable, y = count)) +
    geom_col(position = position_stack(reverse = TRUE),aes(fill = as.factor(minimal_depth)))+
    scale_fill_manual(values=getcolhabs(newcolhabs,palette, nlevels(as.factor(data$minimal_depth))))+
    coord_flip() +
    scale_x_discrete(limits = rev(levels(data$variable))) +
    geom_errorbar(aes(ymin = mean_minimal_depth_label, ymax = mean_minimal_depth_label),size = 1.5) +
    xlab("Variable") +
    ylab("Number of trees") +
    guides(fill = guide_legend(title = "Minimal depth")) +
    theme_bw(base_size = size_plot) +
    geom_label(data = data_for_labels, aes(y = mean_minimal_depth_label,
                                           label = mean_minimal_depth), size=size_mmd)
  if (!is.null(main)) {
    plot <- plot + ggtitle(main)
  }
  return(plot)
}

#' @export
prf<-function(res_multi, sigs=T, sig.value=0.05,size_plot=10,
              min_no_of_trees = 0,
              mean_sample = "top_trees",
              mean_scale = FALSE,
              newcolhabs=newcolhabs,
              palette=palette,
              size_mmd=5){
  multi_imps=res_multi[[2]]
  multi_imps$col<-"gray"
  min_depth_frame=res_multi[[1]]


  multi_imps<-multi_imps[order(multi_imps$mean_min_depth),]
  multi_imps[which(multi_imps$p_value<sig.value),"col"]<-"red"


  BoldImportance<-multi_imps[which(multi_imps$p_value<sig.value),]


  sig_vars<-which(min_depth_frame$variable%in%BoldImportance$variable)

  if(isTRUE(sigs)){min_depth_frame_sigs<-min_depth_frame[sig_vars,]
} else{    min_depth_frame_sigs<-min_depth_frame
  }

  if(isTRUE(sigs)){pick<-nrow(BoldImportance)} else{pick=sigs}




  MDD.plot<-plot_mdd(min_depth_frame_sigs, k=pick,
                                        min_no_of_trees = min_no_of_trees,
                                        mean_sample = mean_sample,
                                        mean_scale = mean_scale,
                     newcolhabs=newcolhabs,palette=palette,size_mmd=size_mmd,size_plot=size_plot)
#  MDD.plot<-MDD.plot+theme_set(theme_grey(base_size = size_plot))

  a<-MDD.plot$layers[[3]]$data
  a<-a[order(a$mean_minimal_depth),]


  MWI1.plot<-randomForestExplainer::plot_multi_way_importance(multi_imps, no_of_labels = nrow(BoldImportance))
  attr(MDD.plot,"sigs")<-multi_imps[  a$variable,]


  MDD.plot$layers[[2]]$layer_data
   par(mfrow=c(1,2))
  return(MDD.plot)
  }



#' @export
ptree<-function(rfs,
                teststat = c("quad", "max"),
                testtype = c("Bonferroni", "MonteCarlo",
                             "Univariate", "Teststatistic"),
                mincriterion = 0.95,
                minsplit = 20,
                minbucket = 7,
                nresample = 9999,
                maxsurrogate = 0,
                mtry = 0,
                savesplitstats = TRUE,
                maxdepth = 0,
                remove_weights = FALSE, palette="turbo", newcolhabs) {


  data=rfs[-1]
  prev=rfs[,1]

  if(is.factor(prev)==T)
  {
    nlevels<-nlevels(prev)


    colors=getcolhabs(newcolhabs,palette,nlevels)

  } else {colors<-"gray"}


  datatree<-data.frame(prev=prev,data)

  treecontrol=ctree_control(teststat = teststat,
                            testtype = testtype,
                            mincriterion = mincriterion,
                            minsplit = minsplit,
                            minbucket = minbucket,
                            nresample = nresample,
                            maxsurrogate = maxsurrogate,
                            mtry = mtry,
                            savesplitstats = savesplitstats,
                            maxdepth = maxdepth,
                            remove_weights = remove_weights)


  model <- ctree(
    prev ~., data = datatree, controls=treecontrol)
  plot(model, tp_args = list(fill=colors))
  dectree<-recordPlot()

}




#' @export
plotCM<-function(rf, palette="turbo",newcolhabs, font_color="black", title="Confusion Matrix", round_cm=3){
  my.data=round(getConfusion(rf),round_cm)

  my.data[,ncol(my.data)][is.na(my.data[,ncol(my.data)])]<-1
  acc<-sum(diag(as.matrix(my.data[,-ncol(my.data)])))/sum(my.data[,-ncol(my.data)])
  acc<-round(acc*100,round_cm)
  subtitle<-paste("Accuracy:",acc,"%")
  title<-title

  col_vector<-getcolhabs(newcolhabs,palette,nrow(my.data))
  col_vector2<-c("white",col_vector)

  tab<-cbind(paste0(1:nrow(my.data),"  "),as.matrix(my.data))
  tab<-rbind(c("",1:nrow(my.data), "class.error"), tab)



  ggtab<-ggtexttable(tab, rows = NULL,
                     theme = ttheme(
                       "default",
                       tbody.style = tbody_style(size = 15),
                       colnames.style=
                         colnames_style(
                           color = 'white',
                           face = "bold",
                           size = 1,
                           fill = NA,
                           linewidth = 1,
                           linecolor = "white",
                           parse = FALSE
                         )
                     ))
  i=2
  for(i in 2:(nrow(tab))) {
    ggtab <-thead_add_border(
      ggtab,
      from.row = i+1,
      to.row = i+1,
      from.column = i,
      to.column = i,
      linetype = 1,
      linewidth = 2,
      linecolor = col_vector2[i]
    )
  }

  i=2
  for(i in 2:(nrow(tab))){
    ggtab<-tab_add_vline(ggtab,1,"right",i+1,i+1,linewidth = 12,
                         linecolor = col_vector2[i])
  }
  i=2
  for(i in 2:(nrow(tab))){
    ggtab<-tab_add_hline(ggtab,2,"bottom",i,i,linewidth = 12,
                         linecolor = col_vector2[i])
  }

  ggtab<-table_cell_font(ggtab, 2, 2:ncol(tab), face = "bold.italic", size = 14)
  ggtab<-table_cell_font(ggtab, 2:nrow(tab)+1, 1, face = "bold.italic", size = 14)


  ggtab<-table_cell_bg(ggtab, 2:(nrow(tab)+1), ncol(tab), fill="white",color ="white")


  ggtab<-tab_add_title(
    ggtab, subtitle, just="left",padding = unit(1.5, "line"),
    size = 15,face="italic",
  )
  ggtab<-tab_add_title(
    ggtab, title, just="left",padding = unit(.5, "line"),
    size = 18,face="bold",
  )

  if(class(rf)[1]=="randomForest"){
    ggtab<-tab_add_footnote(ggtab,  gsub("Confusion Matrix","",attr(rf,'title')), face="italic")
  } else{
    ggtab<-tab_add_footnote(ggtab,  gsub("Confusion Matrix","",attr(getConfusion(rf),'title')), face="italic")
  }

  return(ggtab)
}

#' @export
resampName<-function (x, numbers = TRUE)
{
  if (!("control" %in% names(x)))
    return("")
  if (numbers) {
    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)
    out <- switch(tolower(x$control$method), none = "None",
                  apparent = "Apparent", custom = paste("Custom Resampling (",
                                                        numResamp, " reps)", sep = ""), timeslice = paste("Rolling Forecasting Origin Resampling (",
                                                                                                          x$control$horizon, " held-out with", ifelse(x$control$fixedWindow,
                                                                                                                                                      " a ", " no "), "fixed window)",
                                                                                                          sep = ""), oob = "Out of Bag Resampling",
                  boot = , optimism_boot = , boot_all = , boot632 = paste("Bootstrapped (",
                                                                          numResamp, " reps)", sep = ""), cv = paste("Cross-Validated (",
                                                                                                                     x$control$number, " fold)", sep = ""),
                  repeatedcv = paste("Cross-Validated (", x$control$number,
                                     " fold, repeated ", x$control$repeats,
                                     " times)", sep = ""), lgocv = paste("Repeated Train/Test Splits Estimated (",
                                                                         numResamp, " reps, ", round(x$control$p *
                                                                                                       100, 1), "%)", sep = ""), loocv = "Leave-One-Out Cross-Validation",
                  adaptive_boot = paste("Adaptively Bootstrapped (",
                                        numResamp, " reps)", sep = ""), adaptive_cv = paste("Adaptively Cross-Validated (",
                                                                                            x$control$number, " fold, repeated ", x$control$repeats,
                                                                                            " times)", sep = ""), adaptive_lgocv = paste("Adaptive Repeated Train/Test Splits Estimated (",
                                                                                                                                         numResamp, " reps, ", round(x$control$p,
                                                                                                                                                                     2), "%)", sep = ""))
  }
  else {
    out <- switch(tolower(x$control$method), none = "None",
                  apparent = "(Apparent)", custom = "Custom Resampling",
                  timeslice = "Rolling Forecasting Origin Resampling",
                  oob = "Out of Bag Resampling", boot = "(Bootstrap)",
                  optimism_boot = "(Optimism Bootstrap)", boot_all = "(Bootstrap All)",
                  boot632 = "(Bootstrap 632 Rule)", cv = "(Cross-Validation)",
                  repeatedcv = "(Repeated Cross-Validation)",
                  loocv = "Leave-One-Out Cross-Validation", lgocv = "(Repeated Train/Test Splits)")
  }
  out
}
#' @export
stringFunc<-function (x)
{
  if (!is.character(x))
    x <- format(x)
  numElements <- length(x)
  out <- if (length(x) > 0) {
    switch(min(numElements, 3), x, paste(x, collapse = " and "),
           {
             x <- paste0(x, c(rep(",", numElements -
                                    2), " and", ""))
             paste(x, collapse = " ")
           })
  }
  else ""
  out
}
#' @export
#'
truncateText<-function (x) {
  if (length(x) > 1)
    x <- paste(x, collapse = "")
  w <- options("width")$width
  if (nchar(x) <= w)
    return(x)
  cont <- TRUE
  out <- x
  while (cont) {
    tmp <- out[length(out)]
    tmp2 <- substring(tmp, 1, w)
    spaceIndex <- gregexpr("[[:space:]]", tmp2)[[1]]
    stopIndex <- spaceIndex[length(spaceIndex) - 1] - 1
    tmp <- c(substring(tmp2, 1, stopIndex), substring(tmp,
                                                      stopIndex + 1))
    out <- if (length(out) == 1)
      tmp
    else c(out[1:(length(x) - 1)], tmp)
    if (all(nchar(out) <= w))
      cont <- FALSE
  }
  paste(out, collapse = "\n")
}
#' @export
#'
gettrain<-function (x, printCall = FALSE, details = FALSE, selectCol = FALSE,showSD = FALSE, ...){
  if (!is.null(x$modelInfo$label))
    cat(x$modelInfo$label, "\n\n")
  if (printCall)
    printCall(x$call)
  if (!is.null(x$trainingData)) {
    chDim <- dim(x$trainingData)
    chDim[2] <- chDim[2] - 1
    if (x$modelType == "Classification") {
      lev <- levels(x)
      if (is.character(lev))
        chDim <- c(chDim, length(lev))
    }
    else lev <- NULL
    chDim <- format(chDim)
    cat(chDim[1], " samples", sep = "")
    if (!is.null(x$control$indexFinal))
      cat(",", length(x$control$indexFinal), "used for final model\n")
    else cat("\n")
    cat(chDim[2], " predictor", ifelse(chDim[2] > 1,
                                       "s\n", "\n"), sep = "")
    if (is.character(lev)) {
      cat(chDim[3], "classes:", paste("'",
                                      lev, "'", sep = "", collapse = ", "),
          "\n")
    }
    cat("\n")
  }
  if (!is.null(x$preProc)) {
    pp_list(x$preProc$method)
  }  else {
    if (inherits(x, "train.recipe")) {
      step_names <- function(x) gsub("^step_", "",
                                     class(x)[1])
      steps_used <- unlist(lapply(x$recipe$steps, step_names))
      ppText <- paste("Recipe steps:", paste(steps_used,
                                             collapse = ", "))
      cat(truncateText(ppText), "\n")
    }
    else cat("No pre-processing\n")
  }
  if (!is.null(x$control$index)) {
    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)
    resampText <- resampName(x)
    cat("Resampling:", resampText, "\n")
    if (x$control$method != "none") {
      outLabel <- x$metric
      resampleN <- as.character(resampleN)
      if (numResamp > 5)
        resampleN <- c(resampleN[1:6], "...")
      cat("Summary of sample sizes:", paste(resampleN,
                                            collapse = ", "), "\n")
    }
  }
  if (!is.null(x$control$sampling)) {
    cat("Addtional sampling using ")
    cat(switch(x$control$sampling$name, down = "down-sampling",
               up = "up-sampling", smote = "SMOTE",
               rose = "ROSE", custom = "a custom function"))
    if (!is.null(x$preProc)) {
      if (x$control$sampling$first)
        cat(" prior to pre-processing")
      else cat(" after to pre-processing")
    }
    cat("\n\n")
  }
  if (x$control$method != "none") {
    tuneAcc <- x$results
    tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]
    cat("Resampling results")
    if (dim(tuneAcc)[1] > 1)
      cat(" across tuning parameters")
    if (showSD)
      cat(" (values below are 'mean (sd)')")
    cat(":\n\n")
    if (dim(tuneAcc)[1] > 1) {
      numParam <- length(x$bestTune)
      finalTune <- x$bestTune
      optValues <- paste(names(finalTune), "=", format(finalTune
                                                       ))
      optString <- paste0("The final ", ifelse(numParam >
                                                 1, "values", "value"), " used for the model ",
                          ifelse(numParam > 1, "were ", "was "),
                          stringFunc(optValues), ".")
      finalTune$Selected <- "*"
      if (any(names(tuneAcc) %in% "method"))
        names(tuneAcc)[names(tuneAcc) %in% "method"] <- ".method"
      if (any(names(finalTune) %in% "method"))
        names(finalTune)[names(finalTune) %in% "method"] <- ".method"
      tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)
      if (any(names(tuneAcc) %in% ".method"))
        names(tuneAcc)[names(tuneAcc) %in% ".method"] <- "method"
      tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""
    }  else optString <- ""
    sdCols <- grep("SD$", colnames(tuneAcc))
    if (showSD) {
      sdCheck <- unlist(lapply(tuneAcc[, sdCols, drop = FALSE],
                               function(u) all(is.na(u))))
      if (any(sdCheck)) {
        rmCols <- names(sdCheck)[sdCheck]
        tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% rmCols)]
      }
    }    else {
      if (length(sdCols) > 0)
        tuneAcc <- tuneAcc[, -sdCols, drop = FALSE]
    }
    params <- names(x$bestTune)
    if (!all(params == "parameter")) {
      numVals <- apply(tuneAcc[, params, drop = FALSE],
                       2, function(x) length(unique(x)))
      if (any(numVals < 2)) {
        constString <- NULL
        for (i in seq(along = numVals)) {
          if (numVals[i] == 1)
            constString <- c(constString, paste0("Tuning parameter '",
                                                 names(numVals)[i], "' was held constant at a value of ",
                                                 stringFunc(tuneAcc[1, names(numVals)[i]])))
        }
        discard <- names(numVals)[which(numVals == 1)]
        tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% discard),
                           drop = FALSE]
      }   else constString <- NULL
    }  else constString <- NULL
    tuneAcc <- tuneAcc[, !grepl("Apparent$|Optimism$",
                                names(tuneAcc)), drop = FALSE]
    colnames(tuneAcc)[colnames(tuneAcc) == ".B"] <- "Resamples"
    nms <- names(tuneAcc)[names(tuneAcc) %in% params]
    sort_args <- vector(mode = "list", length = length(nms))
    for (i in seq(along = nms)) {
      sort_args[[i]] <- tuneAcc[, nms[i]]
    }
    tune_ord <- do.call("order", sort_args)
    if (!is.null(tune_ord))
      tuneAcc <- NULL
    theDots <- list()
    theDots$x <- tuneAcc

    #rownames(printMat) <- rep("", dim(printMat)[1])
    if (showSD) {
      sdCols <- grep("SD$", colnames(printMat), value = TRUE)
      sd_dat <- NULL
      printMat <- NULL

    }

    cat("\n")
    if (!is.null(constString)) {
      cat(truncateText(paste(constString, collapse = "\n")))
      cat("\n")
    }

    if (is.null(x$update)) {
      met <- paste(x$metric, "was used to select the optimal model using")
      if (is.function(x$control$selectionFunction)) {
        met <- paste(met, " a custom selection rule.\n")
      }else {
        met <- paste(met, switch(x$control$selectionFunction,
                                 best = paste("the", ifelse(x$maximize,
                                                            "largest", "smallest"), "value.\n"),
                                 oneSE = " the one SE rule.\n", tolerance = " a tolerance rule.\n"))
      }
    }      else {
      met <- paste("The tuning", ifelse(ncol(x$bestTune) >
                                          1, "parameters", "parameter"),
                   "was set manually.\n")
    }
    cat(truncateText(met))

    cat(truncateText(optString))
    if (nzchar(optString))
      cat("\n")
  }

}

#' @export
stringFunc <- function (x)  {
  if (!is.character(x)) x <- format(x)
  numElements <- length(x)
  out <- if (length(x) > 0) {
    switch(min(numElements, 3), x, paste(x, collapse = " and "), {
      x <- paste0(x, c(rep(",", numElements - 2), " and", ""))
      paste(x, collapse = " ")
    })
  } else ""
  out
}


#' @export
pp_list <- function(x) {
  if(is.list(x)) {
    pp <- unlist(lapply(x, length))
    pp <- pp[pp > 0]
    if(length(pp) > 0) {
      names(pp) <- gsub("BoxCox", "Box-Cox transformation", names(pp))
      names(pp) <- gsub("YeoJohnson", "Yeo-Johnson transformation", names(pp))
      names(pp) <- gsub("expoTrans", "exponential transformation", names(pp))
      names(pp) <- gsub("scale", "scaled", names(pp))
      names(pp) <- gsub("center", "centered", names(pp))
      names(pp) <- gsub("pca", "principal component signal extraction", names(pp))
      names(pp) <- gsub("ica", "independent component signal extraction", names(pp))
      names(pp) <- gsub("spatialSign", "spatial sign transformation", names(pp))
      names(pp) <- gsub("knnImpute", "nearest neighbor imputation", names(pp))
      names(pp) <- gsub("bagImpute", "bagged tree imputation", names(pp))
      names(pp) <- gsub("medianImpute", "median imputation", names(pp))
      names(pp) <- gsub("range", "re-scaling to [0, 1]", names(pp))
    } else pp <- "None"
    ppText <- paste("Pre-processing:", paste0(names(pp),  " (", pp, ")", collapse = ", "))
    cat(truncateText(ppText), "\n")
  } else {
    pp <- x
    pp <- gsub("BoxCox", "Box-Cox transformation", pp)
    pp <- gsub("YeoJohnson", "Yeo-Johnson transformation", pp)
    pp <- gsub("expoTrans", "exponential transformation", pp)
    pp <- gsub("scale", "scaled", pp)
    pp <- gsub("center", "centered", pp)
    pp <- gsub("pca", "principal component signal extraction", pp)
    pp <- gsub("ica", "independent component signal extraction", pp)
    pp <- gsub("spatialSign", "spatial sign transformation", pp)
    pp <- gsub("knnImpute", "nearest neighbor imputation", pp)
    pp <- gsub("bagImpute", "bagged tree imputation", pp)
    pp <- gsub("medianImpute", "median imputation", pp)
    pp <- gsub("range", "re-scaling to [0, 1]", pp)

    if(length(pp) == 0) pp <- "None"

    ppText <- paste("Pre-processing:", paste(pp, collapse = ", "))
    cat(truncateText(ppText), "\n")
  }
  invisible(NULL)
}


#' @export
resampName<-function (x, numbers = TRUE)
{
  if (!("control" %in% names(x)))
    return("")
  if (numbers) {
    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)
    out <- switch(tolower(x$control$method), none = "None",
                  apparent = "Apparent", custom = paste("Custom Resampling (",
                                                        numResamp, " reps)", sep = ""), timeslice = paste("Rolling Forecasting Origin Resampling (",
                                                                                                          x$control$horizon, " held-out with", ifelse(x$control$fixedWindow,
                                                                                                                                                      " a ", " no "), "fixed window)",
                                                                                                          sep = ""), oob = "Out of Bag Resampling",
                  boot = , optimism_boot = , boot_all = , boot632 = paste("Bootstrapped (",
                                                                          numResamp, " reps)", sep = ""), cv = paste("Cross-Validated (",
                                                                                                                     x$control$number, " fold)", sep = ""),
                  repeatedcv = paste("Cross-Validated (", x$control$number,
                                     " fold, repeated ", x$control$repeats,
                                     " times)", sep = ""), lgocv = paste("Repeated Train/Test Splits Estimated (",
                                                                         numResamp, " reps, ", round(x$control$p *
                                                                                                       100, 1), "%)", sep = ""), loocv = "Leave-One-Out Cross-Validation",
                  adaptive_boot = paste("Adaptively Bootstrapped (",
                                        numResamp, " reps)", sep = ""), adaptive_cv = paste("Adaptively Cross-Validated (",
                                                                                            x$control$number, " fold, repeated ", x$control$repeats,
                                                                                            " times)", sep = ""), adaptive_lgocv = paste("Adaptive Repeated Train/Test Splits Estimated (",
                                                                                                                                         numResamp, " reps, ", round(x$control$p,
                                                                                                                                                                     2), "%)", sep = ""))
  }
  else {
    out <- switch(tolower(x$control$method), none = "None",
                  apparent = "(Apparent)", custom = "Custom Resampling",
                  timeslice = "Rolling Forecasting Origin Resampling",
                  oob = "Out of Bag Resampling", boot = "(Bootstrap)",
                  optimism_boot = "(Optimism Bootstrap)", boot_all = "(Bootstrap All)",
                  boot632 = "(Bootstrap 632 Rule)", cv = "(Cross-Validation)",
                  repeatedcv = "(Repeated Cross-Validation)",
                  loocv = "Leave-One-Out Cross-Validation", lgocv = "(Repeated Train/Test Splits)")
  }
  out
}
