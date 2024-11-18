
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
  regression_metrics <- caret::postResample(pred, obs)
  residuals <- obs - pred

  metrics <- data.frame(
    RMSE = Metrics::rmse(obs, pred),
    Rsquared = rsq(obs, pred),
    MAE = Metrics::mae(obs, pred),
    RMSESD = sd(residuals),
    RsquaredSD = sd(residuals^2),
    MAESD = sd(abs(residuals)),
    MAPE = Metrics::mape(obs, pred)
  )
  df<-data.frame(metric=t(metrics))
  colnames(df)<-"metric"
  df
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
    ggrepel::geom_text_repel(show.legend=F,max.overlaps =max.overlaps, size = label.size  )+
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
        stat<-caret::postResample(pred,observed)
        stat
      }
    } else{
      stat<-caret::postResample(rf_pred,observed)
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



      stat<-caret::postResample(pred,observed)


      actual<-as.numeric(as.character(pred)==as.character(observed))
      #auc<-  Metrics::auc(actual,pred)
      stat<-c(stat,
              #AUC=auc,
              use.names=T)
    }
  return(c(nobs=length(observed),stat))
}




#' @export
rsq <- function (obs, pred) cor(obs, pred) ^ 2

#' @export

#' @export
accu_rf_class<-function(m)
{
  if(m$modelType=="Regression"){
    return(accu_rf_reg_model(m))
  }
  do.call(rbind,lapply(split(m$pred,m$pred$row),function(x){
    ps<-caret::postResample(x$pred,x$obs)[m$metric]
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
    mse=Metrics::mse(x$obs,x$pred)
    mae=Metrics::mae(x$obs,x$pred)
    rmse=Metrics::rmse(x$obs,x$pred)
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
    mape_res[[i]]<-Metrics::mape(observed[i],predicted[i])
    mae_res[[i]]<-Metrics::mae(observed[i], predicted[i])
    mse_res[[i]]<-Metrics::mse(observed[i],predicted[i])
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



#' @export
getConfusion<-function(rf){
  ConfMat<-list()
  if(class(rf)[1]=="train"){
    validate(need(rf$modelType=="Classification","Confusion Matrices are only valid for Regression models"))
    ConfMat<-caret::confusionMatrix(rf)
  } else  if(class(rf)[1]=='table'){  ConfMat<-caret::confusionMatrix(rf)} else{
  if(sum(names(rf)=="finalModel")==0)
  {
    ConfMat$table<-rf$confusion[,-c(ncol(rf$confusion))]
    ConfMat$text<-NULL
    ConfMat$table<-ConfMat$table/sum(ConfMat$table)
  } else {  ConfMat<-caret::confusionMatrix(rf)}}

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
                    size_mmd=5,size_plot=10) {
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


  #print(aggregate(data["count"],data["variable"],sum))
  # print(unique(data[order(data$count),"variable"]))


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
  attr(plot,"sigs")<-  levels(data$variable)
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


  return(MDD.plot)
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



  ggtab<-ggpubr::ggtexttable(tab, rows = NULL,
                     theme = ggpubr::ttheme(
                       "default",
                       tbody.style = ggpubr::tbody_style(size = 15),
                       colnames.style=
                         ggpubr::colnames_style(
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
    ggtab <-ggpubr::thead_add_border(
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
    ggtab<-ggpubr::tab_add_vline(ggtab,1,"right",i+1,i+1,linewidth = 12,
                         linecolor = col_vector2[i])
  }
  i=2
  for(i in 2:(nrow(tab))){
    ggtab<-ggpubr::tab_add_hline(ggtab,2,"bottom",i,i,linewidth = 12,
                         linecolor = col_vector2[i])
  }

  ggtab<-ggpubr::table_cell_font(ggtab, 2, 2:ncol(tab), face = "bold.italic", size = 14)
  ggtab<-ggpubr::table_cell_font(ggtab, 2:nrow(tab)+1, 1, face = "bold.italic", size = 14)


  ggtab<-ggpubr::table_cell_bg(ggtab, 2:(nrow(tab)+1), ncol(tab), fill="white",color ="white")


  ggtab<-ggpubr::tab_add_title(
    ggtab, subtitle, just="left",padding = unit(1.5, "line"),
    size = 15,face="italic",
  )
  ggtab<-ggpubr::tab_add_title(
    ggtab, title, just="left",padding = unit(.5, "line"),
    size = 18,face="bold",
  )

  if(class(rf)[1]=="randomForest"){
    ggtab<-ggpubr::tab_add_footnote(ggtab,  gsub("Confusion Matrix","",attr(rf,'title')), face="italic")
  } else{
    ggtab<-ggpubr::tab_add_footnote(ggtab,  gsub("Confusion Matrix","",attr(getConfusion(rf),'title')), face="italic")
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
  }  else {
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
