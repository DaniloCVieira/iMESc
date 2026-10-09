# Models stored in a Datalist: one registry of the model types and one set of functions to
# list, read, save, rename and delete them, used by the modules (SOM, K-means, HC,
# density-based clustering, Supervised Algorithms), the Datalist manager, the Rename models
# tool and the Datalist overview.
#
# Each type is an attribute of the Datalist holding a named list of models:
#   attr(datalist, type)[[name]]
# Supervised models are stored wrapped as list(m = <caret model>, ...) ('wrapped'); the
# others are stored as they are. Each module may keep a temporary unsaved model under a
# placeholder name ('unsaved'), which is not listed as a saved model.

# registry of the model types: type (attribute name), label, short label, tip, wrapped,
# unsaved placeholder and group
#' @export
imesc_model_types<-function(){
  base<-data.frame(
    type=c("som","kmeans","hc","dbscan","pwRDA","rfGA"),
    label=c("SOM (unsupervised)","K-means","Hierarchical clustering","Density-based (DBSCAN/HDBSCAN)","pwRDA","Random Forest (genetic algorithm)"),
    short=c("SOM","k-Means","HC","DBSCAN","pwRDA","RF-GA"),
    tip=c("Self-Organizing Maps","K-Means","Hierarchical clustering","Density-based clustering (DBSCAN / HDBSCAN)","Piecewise redundancy analysis","Random Forest with feature selection by genetic algorithm"),
    wrapped=c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE),
    unsaved=c("new som (unsaved)","new kmeans (unsaved)",NA,NA,NA,"new model"),
    group=c("unsupervised","unsupervised","unsupervised","unsupervised","descriptive","supervised"),
    stringsAsFactors=FALSE)
  # supervised models: the caret methods available in the app (SL_models.rds)
  sl<-tryCatch(SL_models$models,error=function(e) NULL)
  if(length(sl)){
    lab<-sub("^[^-]+ - ","",names(sl))
    base<-rbind(base,data.frame(type=unname(sl),label=lab,short=unname(sl),tip=lab,wrapped=TRUE,unsaved="new model",
                                group="supervised",stringsAsFactors=FALSE))
  }
  # types of older iMESc versions, kept so that the models of older Datalists are still listed
  legacy<-data.frame(type=c("svm","sgboost"),label=c("Support Vector Machine","Stochastic Gradient Boosting"),short=c("SVM","GBM"),
                     tip=c("Support Vector Machine","Stochastic Gradient Boosting"),wrapped=TRUE,unsaved="new model",group="supervised",stringsAsFactors=FALSE)
  base<-rbind(base,legacy)
  base[!duplicated(base$type),,drop=FALSE]
}

# attribute names of all model types (the former imesc_models vector)
imesc_models<-tryCatch(imesc_model_types()$type,error=function(e) c("som","kmeans","hc","dbscan","pwRDA","rf","nb","knn","gbm","xyf"))

#' @export
imesc_model_info<-function(type){
  r<-imesc_model_types()
  i<-match(type,r$type)
  if(is.na(i)) return(list(type=type,label=type,short=type,tip=type,wrapped=FALSE,unsaved=NA,group="other"))
  as.list(r[i,,drop=FALSE])
}

# label of a type with its tip, for lists of models
#' @export
imesc_model_tip_ui<-function(type){
  if(is.null(type)) return(NULL)
  i<-imesc_model_info(type)
  span(tiphelp(i$tip,"left"),span(i$short))
}

# models of a type (named list); saved = TRUE leaves out the unsaved placeholder
#' @export
imesc_models_of<-function(data,type,saved=TRUE){
  ms<-attr(data,type)
  if(is.null(ms)||!is.list(ms)) return(list())
  if(isTRUE(saved)){
    ph<-imesc_model_info(type)$unsaved
    if(!is.na(ph)) ms<-ms[names(ms)!=ph]
    # HC: only records with a tree or clusters (older Datalists keep an empty "Numeric-hc"
    # label, migrated by the HC module), as hc_model_names_saved() of the HC module
    if(identical(type,"hc")) ms<-ms[vapply(ms,function(x) !is.null(attr(x,"hc.object"))||!is.null(attr(x,"hc.clusters"))||!is.null(attr(x,"obs.clusters")),logical(1))]
  }
  ms
}

#' @export
imesc_model_names<-function(data,type,saved=TRUE){
  names(imesc_models_of(data,type,saved))
}

# one model; unwrap = TRUE returns the model itself for wrapped (supervised) entries:
# list(m = model, ...) or, in savepoints of older versions, an unnamed list(model)
#' @export
imesc_model_get<-function(data,type,name,unwrap=FALSE){
  e<-attr(data,type)[[name]]
  if(isTRUE(unwrap)&&isTRUE(imesc_model_info(type)$wrapped)&&identical(class(e),"list")&&length(e)){
    e<-if(!is.null(e$m)) e$m else e[[1]]
  }
  e
}

# replaces the whole list of models of a type (NULL when it is empty)
#' @export
imesc_models_put<-function(data,type,models){
  attr(data,type)<-if(length(models)) models else NULL
  data
}

# saves a model (a model with the same name is replaced in its position); first = TRUE
# puts it at the top of the list
#' @export
imesc_model_set<-function(data,type,name,entry,first=FALSE){
  ms<-attr(data,type)
  if(is.null(ms)) ms<-list()
  new<-stats::setNames(list(entry),name)
  if(isTRUE(first)){
    ms<-c(new,ms[names(ms)!=name])
  } else if(name%in%names(ms)){
    ms[name]<-new
  } else{
    ms<-c(ms,new)
  }
  imesc_models_put(data,type,ms)
}

# saves the unsaved placeholder of a type under a name (the placeholder is removed)
#' @export
imesc_model_save_unsaved<-function(data,type,name,entry=NULL){
  ph<-imesc_model_info(type)$unsaved
  if(is.null(entry)&&!is.na(ph)) entry<-attr(data,type)[[ph]]
  if(!is.na(ph)) data<-imesc_model_delete(data,type,ph)
  imesc_model_set(data,type,name,entry)
}

#' @export
imesc_model_delete<-function(data,type,names){
  ms<-attr(data,type)
  if(is.null(ms)) return(data)
  ms<-ms[!names(ms)%in%names]
  imesc_models_put(data,type,ms)
}

# problem with a new model name (NULL when it is valid): empty, or already used by another
# model of the same type
#' @export
imesc_model_name_issue<-function(data,type,name,old=NULL){
  name<-trimws(if(is.null(name)) "" else name)
  if(!nzchar(name)) return("The model name is empty.")
  others<-setdiff(names(attr(data,type)),old)
  if(name%in%others) return(paste0("A ",imesc_model_info(type)$label," model named '",name,"' already exists."))
  NULL
}

# name not used yet by the models of a type: base, base_1, base_2...
#' @export
imesc_model_unique_name<-function(data,type,base){
  nm<-make.unique(c(names(attr(data,type)),base),sep="_")
  nm[length(nm)]
}

# renames the models of a type (new: new names in the order of the current ones). Names must
# be non-empty and unique. References to a renamed SOM in other models (density-based models
# of a SOM codebook) are updated.
#' @export
imesc_model_rename<-function(data,type,new){
  ms<-attr(data,type)
  if(is.null(ms)) return(data)
  new<-trimws(as.character(new))
  if(length(new)!=length(ms)) stop("One name is needed for each model.")
  if(any(!nzchar(new))) stop("Model names cannot be empty.")
  if(anyDuplicated(new)) stop(paste0("Duplicated model names: ",paste(unique(new[duplicated(new)]),collapse=", "),"."))
  old<-names(ms)
  names(ms)<-new
  data<-imesc_models_put(data,type,ms)
  if(identical(type,"som")){
    map<-stats::setNames(new,old)
    db<-attr(data,"dbscan")
    if(length(db)){
      for(i in seq_along(db)){
        s<-db[[i]]$som_model
        if(!is.null(s)&&s%in%old) db[[i]]$som_model<-unname(map[s])
      }
      attr(data,"dbscan")<-db
    }
  }
  data
}

# all the models of a Datalist: type, label, name, class and size
#' @export
imesc_model_table<-function(data,saved=TRUE){
  r<-imesc_model_types()
  out<-do.call(rbind,lapply(r$type,function(tp){
    ms<-imesc_models_of(data,tp,saved)
    if(!length(ms)) return(NULL)
    do.call(rbind,lapply(names(ms),function(nm){
      m<-imesc_model_get(data,tp,nm,unwrap=TRUE)
      data.frame(type=tp,label=imesc_model_info(tp)$label,name=nm,class=class(m)[1],size=format(utils::object.size(m),"auto"),stringsAsFactors=FALSE)
    }))
  }))
  if(is.null(out)) data.frame(type=character(0),label=character(0),name=character(0),class=character(0),size=character(0)) else out
}
