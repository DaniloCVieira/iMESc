
imesc_models<-c('som','kmeans','rf','nb','svm','knn','sgboost','xyf')
imesc_attrs<-c("numeric","factors","coords","base_shape","layer_shape","extra_shape",'notes')

list_models<-function(data,size=F,imesc_models){
  models=imesc_models
  lis<-sapply(models,function(m){
    md<-attr(data,m)
    lapply(md, function (mi){
      if(isTRUE(size)){
        format(object.size(mi), "auto")
      } else{
        length(mi)>0
      }
    })
  })
  lis[sapply(lis,length)>0]
}
list_ensemble<-function(vals,size=F){
  md<-vals$saved_ensemble
  lis<-lapply(md, function (mi){
    if(isTRUE(size)){
      format(object.size(mi), "auto")
    } else{
      length(mi)>0
    }
  })
  lis[sapply(lis,length)>0]
}
list_attrs<-function(data,size=F,imesc_attrs,imesc_models){
  attrs<-imesc_attrs
  attr(data,"numeric")<-data.frame(data)
  li<-lapply(attrs,function(a){
    md<-list()
    md[[a]]<-attr(data,a)
    lapply(md, function (mi){
      if(isTRUE(size)){
        format(object.size(mi), "auto")
      } else{
        length(mi)>0
      }
    })
  })
  names(li)<-attrs

  pic<-sapply(li,function(x) length(x))>0
  attrs<-li[pic]
  attrs<-lapply(attrs, function(x)x[[1]])
  models<-c(list_models(data,size,imesc_models))
  valid<-sapply(models,function(x) length(x))>0
  models<-models[valid]
  #models<-lapply(models, function(x)x[[1]])

  c(attrs,models)
}

getTree_saved_data<-function(vals,size,imesc_attrs,imesc_models){
x<-vals$saved_data[[1]]
  size=F
  # data
  attrs<-lapply(vals$saved_data,function(x) list_attrs(x,size,imesc_attrs,imesc_models))
  attrs<-attrs[sapply(attrs,length)>0]

  #ems<-list_ensemble(vals,size)
  #ems<-ems[sapply(ems,length)>0]
  #ems<-list(Ensemble=ems)
 # attrlist<-c(attrs,ems)
  attrs
}


get_attr_recursive <- function(tree) {
  get_attr_or_dive_deeper <- function(x) {
    if (is.list(x)) {
      st<-unlist(sapply(x,function(xx) attr(xx,'stselected')))
      if(!is.null(st))
      if(all(st)){
       # return(TRUE)
      }
      res<-as.list(get_attr_recursive(x))
      res<-res[sapply(res,length)>0]
      if(!length(res)>0){
        return(NULL)
      } else{
        return(res)
      }
    } else {
      # Se não for uma lista, retorna o atributo stselected
      if(!is.null(attr(x, "stselected")))
        return(attr(x, "stselected"))
    }
  }


  # Usa sapply para aplicar a função auxiliar a cada elemento da lista
  attrs <- lapply(tree, get_attr_or_dive_deeper)
  attrs
}

selected_Tree_attrs<-function(tree){
  sel<-get_attr_recursive(tree)

  df<-reshape::melt(sel[sapply(sel,length)>0])
  if(is.null(df)){
    return(NULL)
  }
  colnames(df)[which(colnames(df)=="L1")]<-"datalist"
  colnames(df)[which(colnames(df)=="L2")]<-"attr"
  colnames(df)[which(colnames(df)=="L3")]<-"model_name"
  res<-data.frame(cbind(datalist=df$datalist,attr=df$attr,model_name=df$model_name))
  if(is.null(res$model_name)){
    res$model_name<-NA
  }

  toem<-which(res$datalist%in%"Ensemble")
  if(length(toem)>0){
    res$model_name[toem]<-res$attr[toem]
    res$attr[toem]<-res$datalist[toem]
  }

  res

}

get_attr_imesc<-function(datalist,attr, model_name=NA,vals,return_data=F,size=T,class=T,dim=T){
  if(attr=="Ensemble"){
    result<-vals$saved_ensemble[[model_name]]
  } else{
    data<-vals$saved_data[[datalist]]
    req(data)
    if(attr=="numeric"){
      result<-attr(data,"numeric")<-data.frame(data)
    }
    if(attr%in% imesc_models){
      result<-attr(data,attr)[[model_name]]
      if(!attr%in%c('som','kmeans')){
        result<-result[[1]]
      }

    } else{
      result<-attr(data,attr)
    }
  }

  if(isTRUE(return_data)){
    return(result)
  }
  c<-d1<-d2<-s<-""
  if(isTRUE(class)){
    c=class(result)[[1]]
  }


  s=format(object.size(result),"auto")

  if(attr=="Ensemble"){
    attr=model_name
  }

  c(datalist=datalist,attr=attr,model_name=model_name,class=c,size=s)


}

