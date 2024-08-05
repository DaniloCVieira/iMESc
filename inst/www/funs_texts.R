
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.






#' @export
getdata_model<-function(m,which="train"){

  pic<-which(colnames(  m$trainingData)==".outcome")
  if(which=="train"){
    m$trainingData[-pic]
  } else{
    m$trainingData[,pic]
  }

}

#' @export
get_feature_data_plot<-function(df,m,newdata,obc, npic){
  colnames(df)[2]<-"Metric"
  req(is.data.frame(df))

  metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
  value<-caret::postResample(predict(m,newdata),obc)[metric]
  df[,2]<-value-df[,2]
  df<-df[which(df$var%in%names(sort(sapply(split(df,df$var),function(x) mean(x$Metric)),decreasing=T)[1:npic])),]
}

#' @export
permute_importance_foreach_progress<-function(m,newdata,obc, rep, seed=seed, show_progress=T,predtype="caret"){

  set.seed(seed)
  seeds_vec<- sample(1:(100*ncol(newdata)), rep, replace = F)


  sample_obs_list2<-lapply(seeds_vec,function(x){
    set.seed(x)
    sample(1:(100*ncol(newdata)), ncol(newdata), replace = F)
  })

  x<-sample_obs_list2[[1]]
  sample_obs_list<-lapply(sample_obs_list2,function(x){
    sapply(x,function(xx){
      set.seed(xx)
      sample(1:nrow(newdata),replace=T)
    })


  })



  start<-Sys.time()
  # Set the number of cores to utilize

  #cat<-message
  # env = parent.frame()
  #  session = getDefaultReactiveDomain()


  metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
  nvar<-ncol(newdata)
  picprogress<-function(n) paste0(round((n/rep)/nvar,2)*100,"%")

  if(isTRUE(show_progress)){
    progress <- shiny::Progress$new()
    progress$set(message = "Running", value = 0)
    prog<-function(n) progress$set(message =  picprogress(n), value = (n/rep)/nvar)
  } else{
    prog<-function(n) message(picprogress(n))
  }

  opts <- list(progress=prog)

  r=n=1
  rands<-foreach(r = 1:rep, .combine = rbind, .packages ="shiny",.options.snow = opts) %:%
    foreach(n = 1:ncol(newdata), .combine = rbind, .packages=c("shiny","randomForest","gbm")) %dopar% {

      temp<-newdata
      temp[n]<-temp[sample_obs_list[[r]][,n],n]
      if(class(m$finalModel)=="randomForest"){
        predall<-data.frame(predict(m$finalModel,temp,predict.all=T,norm.votes=F)$individual)
        df_pred<-data.frame(do.call(rbind,lapply(as.list(predall),function(x){
          data.frame(var=colnames(temp)[n],pred=x,obs=obc,rep=r)
        })))
        df_pred
      } else if(class(m$finalModel)=="gbm") {
        source("inst/www/predgbm.R")
        modelFit<-m$finalModel
        nmax_trees=modelFit$tuneValue$n.trees-1
        predlist<-predict(modelFit, temp, type = "response", n.trees =1:nmax_trees)
        df_pred<-data.frame(do.call(rbind,lapply(1:dim(predlist)[3],function(x){
          predd=apply(predlist[,,x],1,function(xx) which.max(xx))
          data.frame(var=colnames(temp)[n],pred=predd,obs=obc,rep=r)

        })))
        df_pred

      } else{
        if(predtype=="caret"){
        pred<-predict(m,temp)} else{
          pred<-predict(m$finalModel,temp)
        }
        df_pred<-data.frame(var=colnames(temp)[n],pred,obs=obc,rep=r)
        df_pred
      }

      # perf_class<-sapply(split(df_pred,df_pred$obc),function(x) caret::postResample(x$pred,x$obc))

      # opts$progress((n*r)/rep)
      #c(var=colnames(temp)[n],caret::postResample(pred,obc),rep=r,perf_class[metric,])

    }
  if(isTRUE(show_progress)){
    progress$close()

  }



  rands0<-rands


  rands
}



#' @export
permimp_metric<-function(predtable, metric="Accuracy", class=NULL,m, newdata,obc){
  actual<-NULL
  if(!is.null(class)){
    rands_lis<-split(predtable,predtable$obs)
    predtable<- rands_lis[[class]]
    df_pred<-data.frame(pred=predict(m,as.matrix(newdata)),obs=obc)
    actual<-sapply(split(df_pred,df_pred$obs), function(x) postResample(x$pred,x$obs))[metric,class]


  }

  rands_lis<-split(predtable,predtable$rep)
  x<-names(rands_lis)[1]
  remetric<-lapply(names(rands_lis),function(x){
    lis0<-split(rands_lis[[x]],rands_lis[[x]]$var)
    df0<-data.frame(metric=sapply(lis0,function(xx){
      c(postResample(xx$pred,xx$obs)[metric])
    }))
    df0$var<-names(lis0)
    rownames(df0)<-NULL
    df0[metric]<-df0$metric
    df0$metric<-NULL
    df0$rep<-x
    df0

  })

  metrics<-data.frame(do.call(rbind,remetric))
  if(!is.null(actual)){
  metrics$Accuracy<-(actual*metrics$Accuracy)/ max(metrics$Accuracy)}
  metrics
}
#' @export
sig_feature<-function(rands, m,newdata,obc,sig_level){
  metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
  lis<-split(rands,rands$var)
  actual_value<-caret::postResample(predict(m,as.matrix(newdata)), obc)[metric]
  x<-lis[['CASC_TOTAL_0A10']]
  res<-sapply(lis,function(x){
    permuted_values<-x[,metric]

    #distribuicao<-qnorm(permuted_values,mean=mean(permuted_values), sd=sd(permuted_values))
    p_value<-pnorm(actual_value,mean=mean(permuted_values), sd=sd(permuted_values), lower.tail = F)
    p_result<-p_value
    p_result
    xmean<-mean(permuted_values)
    c(actual_value=actual_value,mean=xmean,sd=sd(permuted_values),dff=actual_value-xmean,p_value=p_result)

  })
  perm_summary<-data.frame(t(res))
  colnames(perm_summary)<-c(
    'actual_value',
    paste0("mean_",metric,"(rand)"),
                            paste0("sd",metric,"(rand)"),
                            paste0("TrueAcc - ",paste0("mean_",metric,"(rand)")),
                            "p_value")

  perm_summary$sig<-""
  sigs<-which(perm_summary$p_value<sig_level)
  if(length(sigs)>0){
    perm_summary$sig[sigs]<-"*"
  }


  perm_summary
  }


#' @export
permute_importance_foreach<-function(m,newdata,obc, rep,.export=NULL){
  start<-Sys.time()
  # Set the number of cores to utilize


  rands <- foreach(r = 1:rep, .combine = rbind,.packages=c('doParallel'),.verbose=T,.export=.export) %dopar% {
    print("hello")

        randdata <- foreach(n = 1:ncol(newdata), .combine = rbind,.packages=c('caret', "shiny")) %dopar% {

      temp<-newdata
      temp[n]<-sample(temp[,n],replace=T)
      pred<-predict(m,temp)
      c(var=colnames(temp)[n],rep=r,postResample(pred,obc))



    }

    randdata
  }

  metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
  rands1<-data.frame(rands)[c("var",metric,"rep")]
  rands1[,2]<-as.numeric(rands1[,2])
  lis<-split(rands1,rands1$var)
  values<-sapply(split(m$pred,m$pred$Resample), function(x){
    postResample(x$pred,x$obs)[metric]
  })

  res<-sapply(lis,function(x){
    distribution<-x[,metric]
    mean(distribution)
    if(var(distribution)==0){
      NA
    } else{
      mean(values)
      mean(distribution)
      t_test <- t.test(distribution, values, alternative ="less", var.equal = F)
      t_test$p.value
    }
  })

  end<-Sys.time()
  cat("\n",end-start)
  list(res,rands1)
}

#' @export
get_saved_models<-function(data){
  choices<-list(
    if(check_model_caret(data,"rf")){"rf"},
    if(check_model_caret(data,"nb")){"nb"},
    if(check_model_caret(data,"svm")){"svm"},
    if(check_model_caret(data,"knn")){"knn"},
    if(check_model_caret(data,"sgboost")){"sgboost"},
    if(check_model_caret(data,"xyf")){"xyf"}
  )
  attributes<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]
  if(!length(attributes)>0){ return(NULL)}
  limodels2<-lapply(attributes,function(x) attr(data,x))
  names0<-unlist(lapply(limodels2,names))
  limodels2<-flattenlist(limodels2)
  pic<-which(sapply(limodels2,function(x) class(x)[1])=="train")
  limodels2<-limodels2[pic]
  #names0<-gsub(".*\\.","",names(limodels2))

  linames<-names0

  resmethos<-unlist(lapply(limodels2,function(x) paste0("Type:",x$modelType,"; ")))
 # linames<-unlist(lapply(attributes,function(x) names(attr(data,x))))
  x<-limodels2[[1]]

  ressup<-unlist(lapply(limodels2,function(x) attr(x,"supervisor")))

  dftype<-data.frame(type=as.vector(resmethos),sup=as.vector(ressup))
  resmethos<-as.vector(apply(dftype,1,function(x) {
    paste0("Y:",x,collapse="")
  }))
  liequals<-split(limodels2,as.vector(unlist(lapply(limodels2,function(x)nrow(x$resample)))))
  liequals<-lapply(liequals,function(x) names(x))
  listamodels2<-split(limodels2,resmethos)
  listanames<-split(names0,resmethos)
  for(i in 1:length(listamodels2)){
    names(listamodels2[[i]])<-listanames[[i]]
  }
  listamodels2

}
#' @export
is.factor.matrix<-function (datamat, tolerance = 1e-08, completeThreshold = 5){
  idx <- apply(datamat, 1, function(x) !any(is.na(x)))
  if (sum(idx) > completeThreshold) {
    datamat <- datamat[idx, ]
    is.matrix(datamat) && min(datamat, na.rm = TRUE) >= 0 &&
      max(datamat, na.rm = TRUE) <= 1 && all(abs(rowSums(datamat) -
                                                   1) < tolerance)
  }  else {
    FALSE
  }
}

#' @export
feat<-function(m,moldels_importance,palette="turbo", newcolhabs=list(turbo=turbo),nvars=20,xlab="Importace",ylab="Variables", feat_type=c("Mean","side-by-side", "stacked")){

  if(length(m$levels)>1){
    m_levels<-m$levels
  } else{
    if(!is.na(m$levels)){
      m_levels<-m$levels
    } else{
      m_levels<-1
    }
  }


  imp_name<-"Mean importance"
  if(is.null(feat_type)){
    feat_type<-"Mean"
    imp_name<-"Importance"
  }

  ptype<-feat_type
  mi<-data.frame(moldels_importance)
  colnames(mi)<-paste0(m_levels)
  if(!feat_type%in%c("Mean","side-by-side", "stacked")){
    mi<-mi[feat_type]
  }

  df<-reshape2::melt(data.frame(id=rownames(mi),mi),"id")
  if(!feat_type%in%c("Mean","side-by-side", "stacked")){
    df$variable<-factor(df$variable, labels=feat_type, levels=levels(df$variable))

  } else{
    df$variable<-factor(df$variable, labels=m_levels, levels=levels(df$variable))

  }

  if(ptype=='Mean'){
    df2<-aggregate(df[-c(1:2)],df[1],sum)
    df2$variable<-imp_name
    df<-df2

  }

  mean_vals<-tapply(df$value,df$id, sum)
  col_names<-names(mean_vals)[order(mean_vals,decreasing = T)[1:nvars]]
  picvar<-which(df$id%in%col_names)
  colvalues<-newcolhabs[[palette]](length(m_levels))
  names(colvalues)<-m_levels
  if(!feat_type%in%c("Mean","side-by-side", "stacked")){
    colvalues<-colvalues[feat_type]
  }
  if(feat_type=="Mean"){
    if(length(m_levels)>1){
    col0<-colorspace::mixcolor(0.5,hex2RGB(colvalues[1]), hex2RGB(colvalues[2]))
    for(i in 3:length(colvalues)){
      col0<-colorspace::mixcolor(0.5,col0, hex2RGB(colvalues[i]))
    }
    colvalues=rgb(col0@coords[1,1],col0@coords[1,2],col0@coords[1,3])

  }}


  p<-ggplot(df[picvar,], aes(reorder(id,value, sum), value, fill=variable))+
    geom_bar(stat = "identity")+ scale_fill_manual(name='',values=as.character(colvalues))+coord_flip()



  if(imp_name=="Importance"|length(m_levels)==1){
  p<-p+ theme(legend.position = "none")}
  if(ptype=="side-by-side"){
    p<-p + facet_grid(~factor(variable, levels=colnames(mi)))

  }
  p+xlab(ylab)+ylab(xlab)
}



#' @export
plotforest_qclass<-function(
    result_forest,pred_res, palette="turbo", newcolhabs,ylab="Observations",xlab="Percentage of classifications", leg="Model predictions" ,leg2="Final prediction",cex.axes=13,cex.lab=14,cex.main=15,cex.leg=13,textcolor="white", main="", obc,leg3="Correct observation",size_points=3){
  df0<-df<-result_forest
  pred=pred_res
  obs<-obc



  df<-data.frame(t(apply(df,1,function(x) x/sum(x))))
  colnames(df)<-colnames(df0)
  df$id=rownames(df)
  df$final_pred<-pred[rownames(df),]
  df$order<-1:nrow(df)
  df$obs<-obs[rownames(df)]
#  df$final_pred<-as.factor(sample(c(2,3,4),6,replace = T))

  df3<-reshape2::melt(df,id=c('id','order','obs',"final_pred"))
  colnames(df3)<-c("id",'order','obs',"final_pred","pred","value")
  a<-df3[,c("id","obs","final_pred")]
  df4<-reshape2::melt(a,id="id")

  df5<-df4<-df3
  df4$y<-1.1
  df4$val<-df4$final_pred
  df5$y<-1.2
  df5$val<-df5$obs
  df5$fac<-"observed"
  df4$fac<-"predicted"

  df6<-rbind(df4,df5)

  col<-getcolhabs(newcolhabs,palette, nlevels(pred[,1]))
  p<-ggplot(df3, aes(x=reorder(id,order), y=value, fill=pred)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=ifelse(value >= 0.07, paste(round(value*100,2),"%"),""),y=value),position=position_stack(vjust=0.5), colour=getcolhabs(newcolhabs,textcolor,1)) +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(y="", x="")+scale_fill_manual(name=leg,values=col)


  col2<-getcolhabs(newcolhabs,palette, nlevels(pred[,1]))[which(levels(pred[,1])%in%c(df6$val) )]



 p<- p+geom_point(aes(x=id, y=y, col=val, shape=fac),data=df6,size=size_points)+scale_color_manual(name=leg2,values=col2)+guides(

      fill = guide_legend(override.aes = list(shape = NA))
    )+scale_shape_manual(name="",values = c(16,15))



  p<-p+ylab(xlab)+
    xlab(ylab)+theme(
      axis.text=element_text(size=cex.axes),
      axis.title=element_text(size=cex.lab),
      plot.title=element_text(size=cex.main),
      strip.text.x=element_text(size=cex.main),
      legend.text=element_text(size=cex.leg),
      legend.title=element_text(size=cex.leg))



  p<-p+ggtitle(main)
  p
}


#' @export
getglobal_model<-function(data,model='nb'){
  models<-lapply(attr(data,model), function (x) {
    if(class(x)[1]=='train'){x} else{ x[[1]]}
  })

  res<-caret_global_stats(models)
  res

}
#' @export
getmodelist_names<-function(saved_data){
  x<-saved_data[[1]]

  nb=lapply(saved_data, function(x) names(attr(x,"nb")))
  rf=lapply(saved_data, function(x) names(attr(x,"rf")))
  som=lapply(saved_data, function(x) names(attr(x,"som")))
  sgboost=lapply(saved_data, function(x) names(attr(x,"sgboost")))
  svm=lapply(saved_data, function(x) names(attr(x,"svm")))
  knn=lapply(saved_data, function(x) names(attr(x,"knn")))

  reat<-lapply(list(som=som,nb=nb,svm=svm,knn=knn,rf=rf,sgboost=sgboost), function(x) unlist(x))


  model_names<-do.call(c,c(som,nb,svm,knn,rf,sgboost))
  res<-data.frame(Orig_Datalist=names(model_names),Attribute= rep(names(reat),unlist(lapply(reat,length))),model=model_names)


  res

}
#' @export

getmodelist<-function(saved_data){
  nb=lapply(saved_data, function(x) attr(x,"nb"))
  rf=lapply(saved_data, function(x) attr(x,"rf"))
  som=lapply(saved_data, function(x) attr(x,"som"))
  sgboost=lapply(saved_data, function(x)attr(x,"sgboost"))
  svm=lapply(saved_data, function(x) attr(x,"svm"))
  knn=lapply(saved_data, function(x) attr(x,"knn"))
  res<-do.call(c,c(som,nb,svm,knn,rf,sgboost))

  pic<-unlist(lapply(res,function(x) class(x)[[1]]=="list"))
  if(length(which(pic))>0){
    re0<-unlist(res[pic],recursive = F)
    names(re0)<-names(res[pic])
    res[pic]<-re0}
  res

}


#' @export
#' #' @export
shannon<-function (vect, base = 2)
{
  vect <- as.numeric(vect)
  if (all(vect <= 0))
    return(c(H = NA, J = NA))
  vect <- vect/sum(vect)
  vect <- vect * log(vect, base)
  h <- sum(vect[is.finite(vect)])
  hmax <- log(1/length(vect), base)
  res <- c(H = -h, J = h/hmax)
  attributes(res)$baseLog <- base
  res
}





#' @export
js_tip<-function(mytips,id){
  paste0("
var mytips = [",mytips,"];
$('#",id,"').on('shown.bs.select', function() {
  var $lis = $($(this).data('selectpicker').selectpicker.current.elements);
  $lis.each(function(i) {
    $(this).attr('title', mytips[i]);
  });
});")
}
tiphelp2<-function(text,placement ="bottom"){
  tipify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),text,placement =placement,options=list(container="body"))
}

#' @export
tiphelp<-function(text,placement ="bottom"){
  tipify(a(icon("fas fa-question-circle")),text,placement =placement)
}
tipright<-function(text,placement ="right",...){
  tipify(a(icon("fas fa-question-circle")),text,placement =placement)
}
#' @export
tiphelp3<-function(title,text,placement ="bottom"){
  span(
    style="color: #3c8dbc;",
    title,tipify(icon("fas fa-question-circle"),text,placement =placement ))
}
#' @export
tiphelp4<-function(text,placement ="bottom"){
  tipify(a(icon("fas fa-question-circle")),text,placement =placement)
}
#' @export
pophelp<-function(title,text, placement="right"){
  popify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),title,text, placement = placement, trigger="hover")
}

#' @export
get_change_attr<-function(data){
  transf=attr(data,"transf")
  column(12,
         span(inline(h5(actionLink("change_attr","Change-Attribute"), style="color: #05668D")),em(icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"Click to expand change history")),
         conditionalPanel("input.change_attr % 2",{

           if(!is.null(transf)) {
             column(12,
                    splitLayout(cellWidths = c('30%','70%'),
                                column(12,style="margin-top: 25px",
                                       div(strong(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'removed observations. If > 0 displays in the form of:: Original number of observations :: number of retained observations.',options=list(container="body"))
                                                  ,"Subobs"), style="margin: 7px"),
                                       div(strong(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'removed variables. If > 0 displays in the form of:: Original number of variables :: number of retained variables',options=list(container="body")),"Subvars"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"), 'Transformation option from the Transformation toolbox',options=list(container="body")),strong("Transf"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Variables removed by frequency/abundance',options=list(container="body")),strong("Removed"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Scale option from the Transformation toolbox',options=list(container="body")),strong("Scale"), style="margin: 7px"),
                                       div(
                                         tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Center option from the Transformation toolbox',options=list(container="body")),strong("Center"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'NA.omit  option from the Transformation toolbox',options=list(container="body")),strong("NA.omit"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Data-Attribute Imputation',options=list(container="body")),strong("Data_imp"), style="margin: 7px"),
                                       div(tipify(placement = "left",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),  'Factor-Attribute Imputation',options=list(container="body")),strong("Factor_imp"), style="margin: 7px")
                                ),
                                column(12,  style="margin-left: -20px;overflow-x: scroll",
                                       renderTable(
                                         transf, striped=T,rownames =F,spacing ="xs"
                                       )
                                )

                    )
             )
           }

         })


  )
}
#' @export
datalist_render<-function(datalist=NULL,bagdata=F,width="90px"){
  data=datalist
  factors=attr(data,"factors")
  datalist_name=attr(data,"datalist")

  coords=attr(data,"coords")
  base_shape=attr(data,"base_shape")
  layer_shape=attr(data,"layer_shape")
  data.factors=attr(data,"data.factors")
  transf=attr(data,"transf")
  div(tags$style(HTML(
    "
    .rdl{
        width: fit-content
    }
       .rdl_name h5{
    background: #e6e6e6ff;
    color: black;
    border: 1px dashed black;
    padding: 10px;

       }

    .rdl_items h5{
    color: #05668D
    }
    .rdl_items h5 div:before{
    color: green;
    width: 40px;
    margin-right: 5px;
    padding-bottom: 5px;
    content:' ';
    display: inline-block;
    border-top: 1px dashed black
    }



    .rdl_blocks{
    color: gray;
    padding-left: 20px
    }
    .rdl_in{
    padding-left: 45px
    }

    "
  )),
  div(class="rdl",

      div(class="rdl_name",
          h5(strong("Datalist:"),if(length(datalist_name)>0) {em(datalist_name)})
      ),
      div(class="rdl_items",

          if(length(data)>0){
            div(class="rdl_blocks",
                h5(div("Data-Attribute")),
                div(class="rdl_in",
                    em(paste0("n.obs:",nrow(data), ";"),
                       paste0("nvar-numeric:", ncol(data)), )
                )
            )},
          if(length(factors)>0){
            div(class="rdl_blocks",
                h5(div("Factor-Attribute")),
                div(class="rdl_in",
                    em(paste0("n.obs:",nrow(factors), ";"),
                       paste0("nvar:", ncol(factors)))
                )
            )},
          if(length(coords)>0){
            div(
              class="rdl_blocks",
              div(h5(div("Coords-Attribute"))),
              div(class="rdl_in",
                  em(paste0("n.obs:",nrow(coords), ";"),
                     paste0("nvar:", ncol(coords)))
              )


            )},
          if(length(base_shape)>0){
            div(
              class="rdl_blocks",
              div(h5(div("Base-shape-Attribute"))),
              div(class="rdl_in",
                  em("sf object"))

            )},
          if(length(layer_shape)>0){
            div(class="rdl_blocks",
                div(h5(div("Layer-shape-Attribute"))),
                div(class="rdl_in",
                    em("sf object"))
            )}


      ))
  )

}

#' @export
data_migrate<-function(data,newdata, newname=NULL){
  {
    attr(newdata, "datalist_root")=attr(data, "datalist_root")
    #attr(newdata, "data.factor")=attr(data,"data.factor")[rownames(newdata),]
    attr(newdata, "datalist")=attr(data, "datalist")
    attr(newdata, "filename")=attr(data, "filename")
    attr(newdata, "factors")=attr(data, "factors")[rownames(newdata), , drop=FALSE]
    attr(newdata, "coords")= attr(data,"coords")[rownames(newdata), , drop=FALSE]
    attr(newdata, "base_shape")= attr(data,"base_shape")
    attr(newdata, "layer_shape")=attr(data,"layer_shape")
    attr(newdata, "extra_shape")=attr(data,"extra_shape")
    scale_attr=attr(data,"scale")
    transf<-attr(data, "transf")
    if(!is.null(transf)){
      colnames(transf)<-paste("Change",1:ncol(transf))
    }
    attr(newdata, "transf")=transf
    attr(newdata, "nobs_ori")=attr(data, "nobs_ori")
    attr(newdata, "nvar_ori")=attr(data, "nvar_ori")
   # attr(newdata, "rf")=attr(data, "rf")
    #attr(newdata, "nb")=attr(data, "nb")
    #attr(newdata, "svm")=attr(data, "svm")
    #attr(newdata, "som")=attr(data, "som")
    attr(newdata, "scale")=scale_attr
    return(newdata)
  }
}



#' @export
ppsummary <- function(m){
  write.table(format(m, justify="left", trim=T),
              row.names=F, col.names=F, quote=F)
}


#' @export
textintro<-function(...){
  column(12,

        column(12,
               br(),
               h4(strong("Welcome to",span('iMESc',style="font-family: 'Alata', sans-serif;"),style="color: #05668D")),
               p("iMESc is a shiny-based application that allows the performance of end-to-end machine learning workflows. The available resources meet several needs of environmental workflows, but it is not restricted to. iMESc includes tools for data pre-processing, to perform descriptive statistics, supervised and unsupervised machine learning algorithms and interactive data visualization by means of graphs, maps, and tables. Throughout the app, data input and output are organized in modules enabling the creation of multiple ML pipelines. Additionally, it allows saving the workspace in a single file, contributing to the best practices in data-sharing and analysis reproducibility. The app is entirely written in the R programming language and thus it is free."),
               p("iMESc allows users to create a savepoint, a single R object that can be reloaded later to restore analysis output."),
               p("Get started by creating a Datalist. Use the",icon(verify_fa = FALSE,name=NULL,class="fas fa-plus"),"button."),
               p("To ensure that iMESc content fits nicely on the screen, we recommend a landscape", strong("minimum resolution  of 1377 x 768 pixels.")))


      )

}


#' @export
textauthors<-function(...){
  column(
    12,
    style="background: white; margin-left: 20px",

    h5(
      strong("Author/Developer")
    ),

    column(
      12,
      p("Danilo C Vieira")
    ),

    h5(
      strong("Contributors")
    ),

    column(
      12,
      p("Fabiana S. Paula"),
      p("Luciana E. Yaginuma"),
      p("Dr. Gustavo Fonseca")
    ),

    h5(
      strong('Acknowledgments')
    ),

    column(
      12,
      p("Special thanks to Daiane Faller, Juliane Castro Carneiro, and Julio Cezar F. Moreira for sparking the initial conversations about machine learning. Their insights were instrumental in shaping the direction and development of this app.")
    )
  )


}
#' @export
Read_Shapefile <- function(shp_path) {

  infiles <- shp_path$datapath # get the location of files
  required<-c("\\.shp","\\.dbf","\\.shx")
  val<-sapply(required,function(x) grepl(x,infiles))
  validate(need(nrow(val)>0,"Require at leat three files: '.shp','.dbf' and '.shx' "))
  val<-sum(rowSums(val))==3
  validate(need(val,"Require at leat three files: '.shp','.dbf' and '.shx' "))
  dir <- unique(dirname(infiles)) # get the directory
  outfiles <- file.path(dir, shp_path$name) # create new path name
  name <- strsplit(shp_path$name[1], "\\.")[[1]][1] # strip name
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
  x <- read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
  return(x)
}

#' @export
textupload<-function(...){
  as.character(

    "csv or xlsx file where rows are the observations, columns are the variables  The first column must contain the observation labels. Columns containing characters are initially omitted and can later be included as binary columns by factor level."

  )}
#' @export
textlab<-function(...){
  as.character(
    "csv or xlsx file containg the factors for your data, which will be used for labeling, grouping and viewing the results. It can contain as many factors as you want."
  )
}
#' @export
textinput<-function(...){
  paste(
    "Create Datalists and upload your data and their attributes. All analyses available at ",strong('iMESc',style="font-family: 'Alata', sans-serif;")," will require a Datalist created by the user, either uploaded or using example data."


  )
}



#' @export
textmdshelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      code("metaMDS") ,
                      "function from the 'vegan' package"),
                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),code("distance")," argument is described  in 'Detail' section of the ",actionLink("mdshelph","help page")," of the function;")
    ),
    column(12,
           htmlOutput("mdshelphelp")
    )))
}
#' @export
textpcahelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the"
                      ,actionLink("pcahelph","prcomp") ,
                      "function from R base")
    ),
    column(12,
           htmlOutput("pcahelphelp")
    )))
}

#' @export
textsupersom<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the ",code('som')," function from the",
                    actionLink("kohonen","kohonen"),"package."),
                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"), "arguments are described 'in the ",actionLink("supersomh","help page")," of the function;")
    ),
    column(12,
           htmlOutput("supersomhelp")
    )))
}



#' @export
textrf<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    "In essence the Random Forest is based on generating a large number of decision trees, each constructed using a different subset of your training set. These subsets are selected by sampling at random and with replacement from the original data set. The decision trees are then used to identify a classification consensus by selecting the most common output (mode).",
                    p("This functionality uses the",
                      code("train"),"and",code("trainControl"),
                      "functions from the 'caret' package"),

                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),"Please visit their respective help pages (",actionLink("rfh","train"),"and",actionLink("rfh2","trainControl"),") for more details about the parameters available in",span('iMESc',style="font-family: 'Alata', sans-serif;"))
    ),
    column(12,
           htmlOutput("rfhelp")
    )))
}
#' @export
textctree<-function(...){

  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    "Decision Trees are a non-parametric supervised learning method used for classification and regression. The goal is to create a model that predicts the value of a target variable by learning simple decision rules inferred from the data features.",
                    p("This functionality uses the",
                      code("ctree"),"and",code("ctree_control"),
                      "functions from the 'caret' package"),

                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),"Please visit their respective help pages (",actionLink("ctreeh","ctree"),"and",actionLink("ctreeh2","ctree_control"),") for more details about the parameters")
    ),
    column(12,
           htmlOutput("ctreehelp")
    )))

}
#' @export
textseed<-function(...){
  "A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the analysis."
}

#' @export
textres<-function(...){
  "The resolution of the interpolation"
}
#' @export
textdisplayas<-function(...){

  paste0("The option ",code('discrete')," generate the map using only the provided coordinates, whereas the option ",code('interpolation')," provide some tools for generating an interpolated surface on the map.")





}
#' @export
textcoords<-function(...){
  paste(
    em('Required only for the spatial tools menu:'),
    "csv or xlsx file with the longitudes and latitudes of the observations. The first column must contain the name of the observations. The second and third columns must contain the longitude and latitude respectively"


  )
}
#' @export
textbase<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    "))
    ,column(12,
            "A single R file containing the shape to be used to clip interpolated surfaces when generating maps(e. g an oceanic basin shape). To create this file, use the ",actionLink("shp_tool","Shp Toolbox"),", or in R, run the following code:",
            verbatimTextOutput("codechunk_base"),
            column(12,"Upload the generated 'my_base_shape_shape' file"),
            column(12,
                   htmlOutput("basehelp"))

    ))
}
#' @export
codebase<-function(...){
  cat(c("library('df')","\n",
  "my_base_shape_shape<-st_read('directory_containing_the_shapefiles/shape_name.shp')","\n",
  "save(my_base_shape_shape,
       file='directory_to_save_the_base_shapeshape/my_base_shape_shape')"))
}
#' @export
codelayer<-function(...){
  cat(c("library('df')","\n",
        "my_layer_shape_shape<-st_read('directory_containing_the_shapefiles/shape_name.shp')","\n",
        "save(my_layer_shape_shape,
       file='directory_to_save_the_layer_shapeshape/my_layer_shape_shape')"))
}
#' @export
textlayer<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    "))
    ,column(12,
            "A single R file containing the shape to be used to be used as an additional layer (e.g. a continent shape). To create this file, use the ",actionLink("shp_tool2","Shp Toolbox"),", or in R, run the following code:",
            verbatimTextOutput("codechunk_layer"),
            column(12,"Upload the generated 'my_layer_shape_shape' file"),
            column(12,
                   htmlOutput("layerhelp"))

    ))
}
#' @export
textremove<-function(...){
  column(12,
         p(code('singleton'),"Remove variables occurring only once.",icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"), "Requires a counting data"),
         p(code("pctRare"),"Remove variables with an abundance less than pctRare% of total abundance."),
         p(code("pctPrev"),"Remove species occurring in less than  pctPrev% of the samples")
  )

}
#' @export
texttraining<-function(...){div(
  br(),
  br(),
  h3("Parametrization"),
  br(),
  h4(strong("Topology")),
  p("choose between a hexagonal or rectangular topology"),
  h4(strong("xdim, ydim")),
  p("dimensions of the grid."),
  h4(strong("Distance method")),
  p("Distance functions to be used for the data"),
  h4(strong('rlen')),
  p("The number of times the complete data set will be presented to the network."),
  h4(strong("seed")),
  p("A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the som analysis.")
)}
#' @export
text_som_fine_tuning<-function(...){

  div(

    h3('finetuning'),
    br(),
    br(),
    h4(strong('a1, a2')),
    p("learning rate, indicating the amount of change. Default is to decline linearly from 0.05 to 0.01 over rlen updates. Not used for the batch algorithm."),
    h4(strong('neigh.fct')),
    p("choose between bubble and gaussian neighbourhoods when training a SOM."),


    h4(strong('r1, r2')),
    p("the radius of the neighbourhood, either given as a single number or a vector (start, stop). If it is given as a single number the radius will change linearly from radius to zero; as soon as the neighbourhood gets smaller than one only the winning unit will be updated."),
    h4(strong('mode')),
    p("type of learning algorithm")
  )


}
#' @export
datastr<-function(data){
  data_structure<-list( n.obs=nrow(data),
                        n.vars=ncol(data),
                        type.vars=table(unlist(lapply(data,function(x) class(x)))))
  return(data_structure)
}
#' @export
pfactors<-function(data.factors, palette="FantasticFox1",newcolhabs){

  for(i in 1:ncol(data.factors)){
    dt<-table(data.factors)
    par(mar=c(1,2,1,0.5))
    barplot(as.matrix(dt), beside=F,legend.text = names(dt), col=getcolhabs(newcolhabs,palette,length(dt)), main=colnames(data.factors)[i],las=1)
  }
  plofac<-recordPlot()
  return(plofac)

}
#' @export
new_limits <- function(bars, width=0.7) {
  original_width <- 1
  new_width <- original_width * width
  offset <- (original_width - new_width) / 2
  new_bars_x1 <- bars - 0.5 + offset
  new_bars_x2 <- bars + 0.5 - offset
  return(list(new_bars_x1 = new_bars_x1, new_bars_x2 = new_bars_x2))
}
blend_with_white <- function(color, percent=1) {
  col_rgb <- col2rgb(color)
  white_rgb <- matrix(rep(255, 3), nrow=3)
  blended_rgb <- percent * white_rgb + (1 - percent) * col_rgb
  return(rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue=255))
}
make_pastel <- function(colors, percent=0.7) {
  sapply(colors, function(color) blend_with_white(color, percent))
}

pfac<-function(factors, width=0.4,
               xlab="Factors",
               ylab='Number of Observations',
               title="Observation Totals by Factor and Level",
               base_size=12,
               border_palette=turbo,
               fill_palette=turbo,
               pastel=0.4,
               show_levels=T,
               show_obs=T,
               col_lev="lightsteelblue",
               col_obs="lightcyan"){

  df<-do.call(rbind,lapply(1:ncol(factors),function(i){
    x<-factors[,i]
    tt<-table(x)
    n=as.vector(tt)
    levels=names(tt)
    labels=paste0(levels)
    dd<-data.frame(factor=colnames(factors)[i],
                   level=levels,
                   nobs=as.vector(n),
                   labels=labels)

    dd
  }))
  df <- df[order(df$factor, -as.numeric(as.factor(df$level))),,drop=F]
  df$factor<-factor(df$factor, levels=rev(colnames(factors)))
  df$position <- ave(df$nobs, df$factor, FUN = function(x){
    res<-cumsum(x)
    c(0,res[-length(res)])
  })
  df$label_position_top <- df$position
  li<-split(df,df$factor)
  newl<-new_limits(1:ncol(factors),width)
  df<-data.frame(do.call(rbind,lapply(1:length(li),function(i){
    x<-li[[i]]
    x$pos_x<-newl[[1]][i]
    x$pos_x2<-newl[[2]][i]
    x
  })))
  blend_with_white <- function(color, percent=1) {
    col_rgb <- col2rgb(color)
    white_rgb <- matrix(rep(255, 3), nrow=3)
    blended_rgb <- percent * white_rgb + (1 - percent) * col_rgb
    return(rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue=255))
  }
  make_pastel <- function(colors, percent=0.7) {
    sapply(colors, function(color) blend_with_white(color, percent))
  }
  border_colors <- border_palette(256)
  fill_colors <- fill_palette(256)
  pastel_fill <- make_pastel(fill_colors, pastel)
  df$label_fill <- "lightcyan"
  df$nobs_fill <- "lightsteelblue"

  p<-ggplot(df, aes(x=factor, y=nobs)) +
    geom_bar(aes(fill=position, color=position), position="stack", stat="identity", show.legend=F, width=width) +
    scale_fill_gradientn(colors = pastel_fill, guide = "none") +
    scale_color_gradientn(colors = border_colors, guide = "none") +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    new_scale_fill()+
    theme_bw(base_size)
  lab_levels<-col_levels<-c()
  if(isTRUE(show_levels)) {
    req(length(col_lev)==1)
    p<-p + geom_label(
      aes(label = labels, y = label_position_top,x=pos_x2, fill = label_fill),
      label.r = unit(0, "lines"),
      label.size = 0,
      label.padding = unit(0.15, "lines"),
      hjust = 0,
      vjust = 1,
      show.legend = T
    )

    col_levels[ length(col_levels)+1]<-col_lev
    lab_levels[ length(lab_levels)+1]<-"Level"

  }
  if(isTRUE(show_obs)){
    req(length(col_obs)==1)
    p<-p +geom_label(
      aes(label = nobs, y = label_position_top,x=pos_x, fill = nobs_fill),
      label.r = unit(0, "lines"),
      label.size = 0,
      label.padding = unit(0.15, "lines"),
      hjust = 0,
      vjust = 0,
      show.legend = T
    )
    col_levels[length(col_levels)+1]<-col_obs
    lab_levels[length(lab_levels)+1]<-"Number of Observations"
  }
  if(isTRUE(show_obs)|isTRUE(show_levels)){
    p<-p+ scale_fill_manual(values = col_levels,
                            labels =lab_levels,
                            name = "")}

  p<-p+guides(fill = guide_legend(override.aes = list(label = "")))+ coord_flip()

  return(p)
}

.checkSelect<-function (select, scores) {
  if (is.logical(select) && !isTRUE(all.equal(length(select),
                                              NROW(scores)))) {
    warning("length of 'select' does not match the number of scores: ignoring 'select'")
  }
  else {
    scores <- if (is.matrix(scores)) {
      scores[select, , drop = FALSE]
    }
    else {
      scores[select]
    }
  }
  scores
}
#' @export
labels.cca<-function (object, display, ...) {
  if (is.null(object$CCA))
    CCA <- "CA"
  else CCA <- "CCA"
  switch(display, sp = , species = rownames(object[[CCA]]$v),
         wa = , sites = , lc = rownames(object[[CCA]]$u), reg = colnames(object[[CCA]]$QR$qr),
         bp = rownames(object[[CCA]]$biplot), cn = {
           cn <- rownames(object[[CCA]]$centroids)
           bp <- rownames(object[[CCA]]$biplot)
           c(cn, bp[!(bp %in% cn)])
         })
}
#' @export
text.cca<-function (x, display = "sites", labels, choices = c(1,2), scaling = "species", arrow.mul, head.arrow = 0.05,select, const, axis.bp = FALSE, correlation = FALSE, hill = FALSE,...) {
  if (length(display) > 1)
    stop("only one 'display' item can be added in one command")
  pts <- scores(x, choices = choices, display = display, scaling = scaling,
                const, correlation = correlation, hill = hill, tidy = FALSE)
  cnam <- rownames(pts)
  if (missing(labels))
    labels <- labels.cca(x, display)
  if (!missing(select)) {
    pts <- .checkSelect(select, pts)
    labels <- labels[select]
  }
  if (display == "cn") {
    if (!is.null(nrow(pts))) {
      cnlabs <- seq_len(nrow(pts))
      text(pts, labels = labels[cnlabs], ...)
    }
    else {
      cnlabs <- NULL
    }
    pts <- scores(x, choices = choices, display = "bp",
                  scaling = scaling, const, correlation = correlation,
                  hill = hill, tidy = FALSE)
    bnam <- rownames(pts)
    pts <- pts[!(bnam %in% cnam), , drop = FALSE]
    if (nrow(pts) == 0)
      return(invisible())
    else {
      display <- "bp"
      if (!is.null(cnlabs))
        labels <- labels[-cnlabs]
    }
  }
  if (display %in% c("bp", "reg", "re", "r")) {
    if (missing(arrow.mul)) {
      arrow.mul <- ordiArrowMul(pts)
    }
    pts <- pts * arrow.mul
    arrows(0, 0, pts[, 1], pts[, 2], length = head.arrow,
           ...)
    pts <- ordiArrowTextXY(pts, labels, rescale = FALSE,
                           ...)
    if (axis.bp) {
      axis(side = 3, at = c(-arrow.mul, 0, arrow.mul),
           labels = rep("", 3))
      axis(side = 4, at = c(-arrow.mul, 0, arrow.mul),
           labels = c(-1, 0, 1))
    }
  }
  text(pts, labels = labels, ...)
  invisible()
}
#' @export
points.cca<-function (x, display = "sites", choices = c(1, 2), scaling = "species",
          arrow.mul, head.arrow = 0.05, select, const, axis.bp = FALSE,
          correlation = FALSE, hill = FALSE, ...) {
  if (length(display) > 1)
    stop("only one 'display' item can be added in one command")
  pts <- scores(x, choices = choices, display = display, scaling = scaling,
                const, correlation = correlation, hill = hill, tidy = FALSE)
  if (!missing(select))
    pts <- .checkSelect(select, pts)
  if (display == "cn") {
    cnam <- rownames(pts)
    points(pts, ...)
    pts <- scores(x, choices = choices, display = "bp",
                  scaling = scaling, const, correlation = correlation,
                  hill = hill)
    bnam <- rownames(pts)
    pts <- pts[!(bnam %in% cnam), , drop = FALSE]
    if (nrow(pts) == 0)
      return(invisible())
    else display <- "bp"
  }
  if (display %in% c("bp", "reg", "re", "r")) {
    if (missing(arrow.mul)) {
      arrow.mul <- ordiArrowMul(pts)
    }
    pts <- pts * arrow.mul
    arrows(0, 0, pts[, 1], pts[, 2], length = head.arrow,
           ...)
    pts <- pts * 1.1
    if (axis.bp) {
      axis(3, at = c(-arrow.mul, 0, arrow.mul), labels = rep("",
                                                             3))
      axis(4, at = c(-arrow.mul, 0, arrow.mul), labels = c(-1,
                                                           0, 1))
    }
    return(invisible())
  }
  points(pts, ...)
  invisible()
}
linestack<-function (x, labels, cex = 0.8, side = "right", hoff = 2,
          air = 1.1, at = 0, add = FALSE, axis = FALSE, ...)
{
  if (length(at) > 1 || length(hoff) > 1 || length(air) > 1 ||
      length(cex) > 1)
    stop("only one value accepted for arguments 'cex', 'hoff', 'air' and 'at'")
  x <- drop(x)
  n <- length(x)
  misslab <- missing(labels)
  if (misslab) {
    labels <- names(x)
  }
  if (!is.expression(labels) && !is.character(labels)) {
    labels <- as.character(labels)
  }
  nlab <- length(labels)
  if (!misslab && n != nlab) {
    stop(gettextf("wrong number of supplied 'labels: expected %d, got %d",
                  n, nlab))
  }
  side <- match.arg(side, c("right", "left"))
  op <- par(xpd = TRUE)
  on.exit(par(op))
  ord <- order(x)
  x <- x[ord]
  labels <- labels[ord]
  pos <- numeric(n)
  if (!add) {
    plot(pos, x, type = "n", axes = FALSE, xlab = "",
         ylab = "", ...)
  }
  hoff <- hoff * strwidth("m")
  ht <- air * strheight(labels, cex = cex)
  mid <- (n + 1)%/%2
  pos[mid] <- x[mid]
  if (n > 1) {
    for (i in (mid + 1):n) {
      pos[i] <- max(x[i], pos[i - 1] + ht[i])
    }
  }
  if (n > 2) {
    for (i in (mid - 1):1) {
      pos[i] <- min(x[i], pos[i + 1] - ht[i])
    }
  }
  segments(at, x[1], at, x[n])
  if (side == "right") {
    text(at + hoff, pos, labels, pos = 4, cex = cex, offset = 0.2,
         ...)
    segments(at, x, at + hoff, pos)
  }
  else if (side == "left") {
    text(at - hoff, pos, labels, pos = 2, cex = cex, offset = 0.2,
         ...)
    segments(at, x, at - hoff, pos)
  }
  if (axis)
    axis(if (side == "right")
      2
      else 4, pos = at, las = 2)
  invisible(pos[order(ord)])
}
#' @export
#'
plot.cca<-function (x, choices = c(1, 2), display = c("sp", "wa", "cn"), scaling = "species", type, xlim, ylim,const, correlation = FALSE, hill = FALSE, ...) {
  TYPES <- c("text", "points", "none")
  if (any(display %in% c("c", "cn")))
    display <- c(display, "bp")
  g <- scores(x, choices, display, scaling, const, correlation = correlation,
              hill = hill, tidy = FALSE)
  if (length(g) == 0 || all(is.na(g)))
    stop("nothing to plot: requested scores do not exist")
  if (!is.list(g))
    g <- list(default = g)
  for (i in seq_along(g)) {
    if (length(dim(g[[i]])) > 1)
      rownames(g[[i]]) <- rownames(g[[i]], do.NULL = FALSE,
                                   prefix = substr(names(g)[i], 1, 3))
  }
  if (!is.null(g$centroids)) {
    if (is.null(g$biplot))
      g$biplot <- scores(x, choices, "bp", scaling)
    bipnam <- rownames(g$biplot)
    cntnam <- rownames(g$centroids)
    g$biplot <- g$biplot[!(bipnam %in% cntnam), , drop = FALSE]
    if (nrow(g$biplot) == 0)
      g$biplot <- NULL
  }
  if (missing(type)) {
    nitlimit <- 80
    nit <- max(nrow(g$spe), nrow(g$sit), nrow(g$con), nrow(g$def))
    if (nit > nitlimit)
      type <- "points"
    else type <- "text"
  }
  else type <- match.arg(type, TYPES)
  if (length(choices) == 1) {
    if (length(g) == 1)
      pl <- linestack(g[[1]], ...)
    else {
      hasSpec <- names(g)[1] == "species"
      ylim <- range(c(g[[1]], g[[2]]), na.rm = TRUE)
      pl <- linestack(g[[1]], ylim = ylim, side = ifelse(hasSpec,
                                                         "left", "right"), ...)
      linestack(g[[2]], ylim = ylim, side = ifelse(hasSpec,
                                                   "right", "left"), add = TRUE, ...)
    }
    return(invisible(pl))
  }
  if (missing(xlim)) {
    xlim <- range(g$species[, 1], g$sites[, 1], g$constraints[,
                                                              1], g$biplot[, 1], if (length(g$centroids) > 0 &&
                                                                                     all(is.na(g$centroids))) NA else g$centroids[, 1],
                  g$default[, 1], na.rm = TRUE)
  }
  if (!any(is.finite(xlim)))
    stop("no finite scores to plot")
  if (missing(ylim)) {
    ylim <- range(g$species[, 2], g$sites[, 2], g$constraints[,
                                                              2], g$biplot[, 2], if (length(g$centroids) > 0 &&
                                                                                     all(is.na(g$centroids))) NA else g$centroids[, 2],
                  g$default[, 2], na.rm = TRUE)
  }
  plot(g[[1]], xlim = xlim, ylim = ylim, type = "n",
       asp = 1, ...)
  abline(h = 0, lty = 3)
  abline(v = 0, lty = 3)
  if (!is.null(g$species)) {
    if (type == "text")
      text(g$species, rownames(g$species), col = "red",
           cex = 0.7)
    else if (type == "points")
      points(g$species, pch = "+", col = "red",
             cex = 0.7)
  }
  if (!is.null(g$sites)) {
    if (type == "text")
      text(g$sites, rownames(g$sites), cex = 0.7)
    else if (type == "points")
      points(g$sites, pch = 1, cex = 0.7)
  }
  if (!is.null(g$constraints)) {
    if (type == "text")
      text(g$constraints, rownames(g$constraints), cex = 0.7,
           col = "darkgreen")
    else if (type == "points")
      points(g$constraints, pch = 2, cex = 0.7, col = "darkgreen")
  }
  if (!is.null(g$biplot) && nrow(g$biplot) > 0 && type != "none") {
    if (length(display) > 1) {
      mul <- ordiArrowMul(g$biplot)
    }
    else mul <- 1
    attr(g$biplot, "arrow.mul") <- mul
    arrows(0, 0, mul * g$biplot[, 1], mul * g$biplot[, 2],
           length = 0.05, col = "blue")
    biplabs <- ordiArrowTextXY(mul * g$biplot, rownames(g$biplot))
    text(biplabs, rownames(g$biplot), col = "blue")
  }
  if (!is.null(g$regression) && nrow(g$regression > 0) && type !=
      "none") {
    rcol <- "purple4"
    if (length(display) > 1) {
      mul <- ordiArrowMul(g$regression)
    }
    else mul <- 1
    attr(g$regression, "arrow.mul") <- mul
    arrows(0, 0, mul * g$regression[, 1], mul * g$regression[,
                                                             2], length = 0.05, col = rcol)
    biplabs <- ordiArrowTextXY(mul * g$regression, rownames(g$regression))
    text(biplabs, rownames(g$regression), col = rcol)
  }
  if (!is.null(g$centroids) && !anyNA(g$centroids) && type !=
      "none") {
    if (type == "text")
      text(g$centroids, rownames(g$centroids), col = "blue")
    else if (type == "points")
      points(g$centroids, pch = "x", col = "blue")
  }
  if (!is.null(g$default) && type != "none") {
    if (type == "text")
      text(g$default, rownames(g$default), cex = 0.7)
    else if (type == "points")
      points(g$default, pch = 1, cex = 0.7)
  }
  class(g) <- "ordiplot"
  invisible(g)
}
#' @export
#'
pwRDA.source<-function (x.ord, y.ord, BPs) {
  y.ord <- as.matrix(y.ord)
  x.ord <- as.matrix(x.ord)
  n <- nrow(x.ord)
  k <- ncol(x.ord)
  bks <- c(0, BPs, nrow(x.ord))
  nBPs <- length(bks) - 1
  Xb <- matrix(0, ncol = nBPs * k, nrow = n)
  for (i in 1:(nBPs)) {
    Xb[(bks[i] + 1):bks[i + 1], ((k * (i - 1)) + 1):((k *
                                                        (i - 1)) + k)] <- x.ord[(bks[i] + 1):bks[i + 1],
                                                        ]
  }
  Xb <- jitter(Xb)
  Xbc = scale(Xb, center = T, scale = F)
  rda.0 <- rda(data.frame(y.ord) ~ ., data.frame(x.ord))
  rda.pw <- rda(data.frame(y.ord) ~ ., data.frame(Xb))
  Yc = scale(y.ord, center = T, scale = F)
  Y.avg = matrix(rep(apply(Yc, 2, mean), times = nrow(Yc)),
                 ncol = ncol(Yc), byrow = T)
  B.pw = solve(t(Xbc) %*% Xbc) %*% (t(Xbc) %*% Yc)
  coord <- rda.pw$CCA$biplot
  bew.bp <- t(cor(coord, t(cor(x.ord, Xb))))
  rda.pw$CCA$biplot <- bew.bp
  Ypred.pw = Xbc %*% B.pw
  Yres <- Yc - Ypred.pw
  TSS.pw = sum((Yc)^2)
  RSS.pw = sum((Ypred.pw - Y.avg)^2)
  r2.pw <- RSS.pw/TSS.pw
  n.pw <- nrow(Xbc)
  k.pw <- ncol(Xbc)
  Radj.pw <- 1 - ((1 - r2.pw) * ((n.pw - 1)/(n.pw - k.pw -
                                               1)))
  Xc = scale(jitter(x.ord), center = T, scale = F)
  B.full = solve(t(Xc) %*% Xc) %*% (t(Xc) %*% Yc)
  Ypred.full = Xc %*% B.full
  Yres <- Yc - Ypred.full
  TSS.full = sum((Yc)^2)
  RSS.full = sum((Ypred.full - Y.avg)^2)
  r2.full <- RSS.full/TSS.full
  n.full <- nrow(Xc)
  k.full <- ncol(Xc)
  Radj.full <- 1 - ((1 - r2.full) * ((n.full - 1)/(n.full -
                                                     k.full - 1)))
  F.stat <- ((RSS.full - RSS.pw)/(k.full - k.pw))/(RSS.full/(n.pw -
                                                               k.full))
  dg1 <- k.pw - k.full
  dg2 <- n.pw - k.pw
  F.stat <- ((RSS.pw - RSS.full)/(dg1))/(RSS.pw/(dg2))
  p.value <- 1 - pf(F.stat, dg1, dg2, lower.tail = T)
  summ <- c(Radj.full = Radj.full, Radj.pw = Radj.pw, F.stat = F.stat,
            p.value = p.value)
  pw <- list(summ = summ, rda.0 = rda.0, rda.pw = rda.pw)
  class(pw) <- "pw"
  return(invisible(pw))
}


#' @export
str_numerics<-function(numerics, cextext=1, col="gray", border="gray", width_varname=0.2, width_metrics=0.35, width_histo=0.35, round=2, show=c('Min.' ,'1st Qu.',"Mean",'Median','3rd Qu.','Max.')){

  par(mar=c(0.5,0,0,0), cex=2)
  m<-matrix(1:(ncol(numerics)*3), ncol=3)
  m=m+3
  m<-rbind(1:3, m)
  layout(m, widths = c(width_varname,width_metrics,width_histo))
  opar<-par(no.readonly=TRUE)
  plot.new()
  text(.5,.5,"Variable", cex=cextext)
  plot.new()
  text(  seq(0.1,0.9, length.out=length(show)),.5,show, cex=cextext)
  plot.new()
  text(.5,.5,"Histogram", cex=cextext)
i=1
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,colnames(numerics)[i], cex=cextext)
  }
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(  seq(0.1,0.9, length.out=length(show)),.5,round(summary(numerics[,i]),round)[show], cex=cextext)
  }

  for(i in 1: ncol(numerics))
  {

    hist(numerics[,i], ann=F, axes=F, col=col, border=border)
  }



}



#' @export
str_numerics3<-function(numerics,obs=NULL, pred_interval=NULL,
                        col.conf="#FCC7C7", col.obs="darkblue", col.pred="#CDCCD9",q_class,col.mean="SeaGreen", cex=2,pred_ensemble=NULL)
{
  q_class[,c(1:3)]<-round(q_class[,c(1:3)],5)

  par(mar=c(0,0,0,0), cex=cex)
  m<-matrix(1:((ncol(numerics)*6)+6), ncol=6)
  confs<-c(pred_interval/2,   1-(pred_interval/2))
  layout(m, widths = c(200,100,100,100,100,300))
  opar<-par(no.readonly=TRUE)
  plot.new()
  par(mar=c(0,0,0,0))
  text(.5,.5,"ID", cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,colnames(numerics)[i], cex=cex)
  }
  plot.new()
  text(.5,.5,"Observed", cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,q_class[i,1], cex=cex)
  }
  plot.new()
  text(.5,.5,paste0("q",confs[[1]]), cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,q_class[i,2], cex=cex)
  }
  plot.new()
  text(.5,.5,paste0("q",confs[[2]]), cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,q_class[i,3], cex=cex)
  }
  plot.new()
  text(.5,.5,"q_class", cex=cex)
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,q_class[i,4], cex=cex)
  }

  plot.new()
  text(.5,.5,"Density plot", cex=cex)

i=2

     for(i in 1: ncol(numerics))
  {
    confcurve(
      vec=numerics[,i],
      ob=obs[i],
      pred_interval=pred_interval,
      col.conf=col.conf, col.obs=col.obs, col.pred=col.pred,col.mean=col.mean,
      pred_ensemble=  if(!is.null(pred_ensemble)){pred_ensemble[i]}
    )
  }



}
#' @export
confcurve<-function(vec,ob,pred_interval, col.conf="#FCC7C7", col.obs="darkblue", col.pred="#CDCCD9",col.mean="SeaGreen",pred_ensemble=NULL){

  confs<-c(pred_interval/2,   1-(pred_interval/2))

  dens <- density(vec, na.rm=T)
  plot(dens, xlim=range(c(ob,dens$x)),yaxt="n",xaxt="n", ann=F, type="n", bty="n")
  #plot(dens, xlim=range(c(ob,vec)))

  x1_sup <- min(which(dens$x >= quantile(vec, confs[2], na.rm=T)), na.rm=T) # quantile() ftw!
  x2_sup <- max(which(dens$x <  max(dens$x, na.rm=T)), na.rm=T)
  with(dens, polygon(x=c(x[c(x1_sup,x1_sup:x2_sup,x2_sup)]), y= c(0, y[x1_sup:x2_sup], 0),
                     col=col.conf, border=NA
  ))

  x1_inf <- 1 # quantile() ftw!
  x2_inf <- max(which(!dens$x >= quantile(vec, confs[1], na.rm=T)), na.rm=T)
  with(dens, polygon(x=c(x[c(x1_inf,x1_inf:x2_inf,x2_inf)]), y= c(0, y[x1_inf:x2_inf], 0),
                     col=col.conf, border=NA
  ))

  x1 <- x1_sup
  x2 <- x2_inf
  with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0),
                     col=col.pred, border=NA
  ))
  abline(v=ob, lty=2, col=col.obs, lwd=2)
  if(!is.null(pred_ensemble)){
    abline(v=pred_ensemble, lty=2, col=col.mean, lwd=2)
  } else{
    abline(v=mean(vec,na.rm=T), lty=2, col=col.mean, lwd=2)
  }


}


#' @export
getHelp <- function(fun){
  temp = tools::Rd2HTML(gbRd::Rd_fun(fun),out = tempfile("docs"),dynamic=T)
  content = readr::read_file(temp)
  file.remove(temp)
  content
}
#' @export

textrfloop<-function(...)
{
  div(
    p("Compute the accuracies using random forest algorithm for increasing number of clusters (k):"),
    p(style="margin-left: 5px;",code("*"),"For each k, calculate the accuracy;"),
    p(style="margin-left: 10px;",code("*"),"Plot the curve of accuracies according to the number of clusters k;"),
    p(style="margin-left: 5px;",code("*"),"The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters.")
  )
}
#' @export
textelbow<-function(...)
{

  div(
    p("Compute the Within-Sum-of-Squares (WSS) for increasing number of clusters (k):"),
    p(style="margin-left: 5px;",code("*"),"For each k, calculate the WSS;"),
    p(style="margin-left: 5px;",code("*"),"Plot the curve of WSS according to the number of clusters k;"),
    p(style="margin-left: 5px;",code("*"),"The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters.")
  )
}
#' @export
textsmw<-function(...){
  "Performs the split moving window analysis for several windows sizes, calculates the mean dissimilarity profiles, and considers as significant the dissimilarities that exceed mean plus one standard deviation. Click for more information"

}
#' @export
textrfbphelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p(strong("Dissimilarity profile of the moving split-window analysis:")),
                    p(strong("1."),"Placing a window of even-numbered size at the beginning of the data series"),
                    div(style="margin-left: 5px;",
                        p(strong("2."),"splitting the window into two equal halves"),
                        p(strong("3."),"computing a dissimilarity  between the two halves"),
                        p(strong("4."),"shifting window one position along the series, and"),
                        p(strong("5.")," repeating the procedure till the end of the data series  (Cornelius & Reynolds 1991)")
                    )




    )
    ))
}
#' @export
textbphelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p(strong("Dissimilarity profile of the moving split-window analysis:")),
                    p(strong("1."),"Placing a window of even-numbered size at the beginning of the data series"),
                    div(style="margin-left: 5px;",
                        p(strong("2."),"splitting the window into two equal halves"),
                        p(strong("3."),"computing a dissimilarity  between the two halves"),
                        p(strong("4."),"shifting window one position along the series, and"),
                        p(strong("5.")," repeating the procedure till the end of the data series  (Cornelius & Reynolds 1991)")
                    )




    )
    ))
}
#' @export
#' @export
divI<-function(abund,choices=c("N","S","margalef","D","H","J'","Dom_rel","Skewness")){
  res=list()
  if("N"%in%choices){res$N<-rowSums(abund)}
  if("S"%in%choices){res$S<-specnumber(abund)}
  if("margalef"%in%choices){res$margalef=(specnumber(abund)-1)/log(rowSums(abund))}
  if("D"%in%choices){res$D<-diversity(abund, index="simpson")}
  if("H"%in%choices){res$H<-diversity(abund)}
  if("J'"%in%choices){
    H<-diversity(abund)
    S<-specnumber(abund)
    res$J <- H/log(S)}
  if("Dom_rel"%in%choices){res$Dom_rel<-apply(decostand(abund, "total"),1,sort,T)[1,]}
  if("Skewness"%in%choices){res$Skewness=apply(decostand(abund,"log"),1,skewness)}
  return(data.frame(do.call(cbind,res)))
}


#' @export
sortImp2<-function (object, top)
{
  if (object$calledFrom == "varImp") {
    best <- switch(object$model, pam = "maxabs", "max")
  }
  else {
    best <- "max"
  }
  featureRank <- switch(best, max = rank(-apply(object$importance,
                                                1, max, na.rm = TRUE)), min = rank(apply(object$importance,
                                                                                         1, min, na.rm = TRUE)), maxabs = rank(-apply(abs(object$importance),
                                                                                                                                      1, max, na.rm = TRUE)))
  tiedRanks <- as.numeric(names(table(featureRank)[table(featureRank) >
                                                     1]))
  if (length(tiedRanks) > 0) {
    for (i in seq(along = tiedRanks)) {
      tmp <- featureRank[featureRank == tiedRanks[i]]
      featureRank[featureRank == tiedRanks[i]] <- tmp +
        runif(length(tmp), min = 0.001, max = 0.999)
    }
  }
  featureOrder <- order(featureRank)
  out <- object$importance[featureOrder, , drop = FALSE]
  out <- out[1:top, , drop = FALSE]
  out
}
#' @export
plot_varimport<-function(m,palette='viridis',newcolhabs,position="dodge",type="gg",nvars=10, cex.axes=13,cex.lab=13,cex.main=15,cex.sub=13,cex.leg=13){

  X <- varImp(m)

  #colnames(X$importance)<- m$levels
   if(type=="default"){

     #nvars=10
     DF<-data.frame(variable=rownames(X$importance), Importance=X$importance)
     colnames(DF)<-c("variable","Importance")
     col<-colsbyprof(DF$Importance, cols=getcolhabs(newcolhabs,palette,100))

     pic<-order(DF$Importance, decreasing=T)[1:nvars]
     DF<-DF[pic,]


     gg<-ggplot(DF, aes(x=reorder(variable,Importance), y=Importance,fill=Importance))+
       geom_bar(stat="identity")+ coord_flip()+
       ylab("Variable Importance")+
       xlab("")+
       ggtitle("Feature Importance")+


       scale_fill_gradientn(colours=getcolhabs(newcolhabs,palette,nrow(X$importance)),breaks=pretty(X$importance[pic,]), limits=range(pretty(X$importance[pic,])))+
       theme_minimal() +
       theme(
         panel.ontop = TRUE,
             panel.background=element_rect(fill=NA, color="white"),
         panel.grid.major=element_line(color=NA),
         panel.grid.minor=element_line(color="gray",linetype=2),
             panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),

             axis.line=element_line(),
             axis.text=element_text(size=cex.axes),
             axis.title=element_text(size=cex.lab,face="bold"),
             plot.title=element_text(size=cex.main),
             plot.subtitle=element_text(size=cex.sub),
             legend.text=element_text(size=cex.leg),
             legend.title=element_text(size=cex.leg))
gg



    # gg<-plot(X,nvars)
   } else{
     plotObj <- sortImp2(X, top=nvars)
     if (ncol(plotObj) == 2) {
       plotObj <- plotObj[, 1, drop = FALSE]
       names(plotObj) <- "Importance"
     }

     featureNames <- rownames(plotObj)
     outcomeNames <- colnames(plotObj)
     df<- data.frame(as.table(as.matrix(X$importance[featureNames,outcomeNames])))
     colnames(df)[2]<-"Classes"
     df$Var1<-ordered(df$Var1,levels=names(sort(unlist(lapply(split(df,df$Var1),function(x) sum(x[,3]))),decreasing  =F)))
     col<-getcolhabs(newcolhabs,palette,nlevels(df$Classes))
     gg<-df %>%
       ggplot(aes(x=Var1,y=Freq, fill=Classes)) +
       geom_bar(position = position,stat = "identity") +
       scale_fill_manual(values=col)+
       coord_flip() +
       labs(x="Variables", y="Importance")
  }

  gg

}
#' @export
plot_florest_class<-function(res_forest,newcolhabs, palette="viridis", ylab="N of Trees"){
  obs<-attr(res_forest,'obs')
  df<- data.frame(as.table(as.matrix(res_forest)))
  df$Obs<-obs[df$Var1,]
  df$obs_x<-attr(res_forest,'ntree')+3
  Obs<-obs[df$Var1,]
  colnames(df)[2]<-"Classes"

  col<-getcolhabs(newcolhabs,palette,nlevels(df$Classes))
  gg<-df %>%
    ggplot(aes(x=Var1,y=Freq, fill=Classes)) +
    geom_bar(position = 'stack',stat = "identity") +
    scale_fill_manual(values=col)+
    scale_y_continuous(limits = c(0,  attr(res_forest,'ntree')+5))+
    geom_point(data=df, aes(Var1, obs_x, col=factor(Obs)), col=col[Obs],show.legend=F) +
    coord_flip() +
    labs(x="Observations", y=ylab)

  gg

}
#' @export
retype<-function(data){
  suppressWarnings({
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    num<-data
    ints<-names(which(unlist(lapply(num, function(x) sum(is.wholenumber(x))==nrow(data)))))

    num[ints]<-lapply(data[ints],function(x) as.integer(x))

    return(num)


  })
}
#' @export
plot_ridges<-functiondataplot_ridges<-function(data,fac,palette,newcolhabs,ncol=3, title="",base_size=11){
  col<-getcolhabs(newcolhabs,palette,nlevels(data$class))
  df<-data.frame(id=rownames(data),y=fac,data)
  df<-reshape2::melt(data,'class')
  ggplot(df, aes(x = value, y = class)) +
    ggridges::geom_density_ridges(aes(fill = class),show.legend = T) +
    scale_fill_manual(values = c(col))+
    ggtitle(NULL)+facet_wrap(~variable,ncol=ncol)+
    guides(fill=guide_legend(title=fac))+ggtitle(title)+theme(
    strip.text.x = element_text(size = base_size),
    strip.text.y = element_text(size = base_size),
    axis.text=element_text(size=base_size),
    axis.title=element_text(size=base_size),
    plot.title=element_text(size=base_size),
    plot.subtitle=element_text(size=base_size,face="italic"),
    legend.text=element_text(size=base_size),
    legend.title=element_text(size=base_size),
  )


}

