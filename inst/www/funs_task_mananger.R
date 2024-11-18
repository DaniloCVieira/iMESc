#' @export
my_object.size<-function(x,dlm_getsize=T){
  if(isTRUE(dlm_getsize)){
    object.size(x)
  } else{
    NULL
  }
}


#' @export
divstru_model<-function(attrx,datalist_name, i, j,x,class_attr){
  style="display: none"
  if (i != "som")
    div(
      id = ns_model_out(paste0(datalist_name, i, x)),
      style = style,
      div(style = "margin-left: 20px",
          inline(div(class = 'link_mananger')),
          "class=", class_attr,
          if (class_attr == "train") div(style = "margin-left: 20px", div(class = 'link_mananger'), "finalModel class=", class(attrx[[i]][[j]][[1]]$finalModel)[1])
      )
    )

}
#' @export
divstru_feat<-function(attrx,datalist_name,i,j,x,dlm_getsize){

  if ( 'feature_rands' %in% names(attrx[[i]][[j]]))
    div(
      style = "margin-left: 20px",
      div(class = 'link_mananger'),
      'Permutation importance=',
      myformat(my_object.size(attrx[[i]][[j]]$feature_rands,dlm_getsize=dlm_getsize), "auto", total_size = datalist_bytes()),
      actionLink(ns_feature_down(paste0(datalist_name, i, x)), icon('fas fa-download')),
      actionLink(ns_feature_trash(paste0(datalist_name, i, x)), icon('fas fa-trash'))
    )
}
#' @export
divstru_attr<-function(datalist_name,i,x,obj_size,total_size){
  div(
    actionLink(ns_model_name(paste0(datalist_name, i, x)), x),
    myformat(obj_size, "auto", total_size =total_size),
    actionLink(ns_model_trash(paste0(datalist_name, i, x)), icon('fas fa-trash'))

  )
}
#' @export
divstru_attributes<-function(attrx,i,j,x,datalist_name,class_attr,dlm_getsize,obj_size,total_size,names_list){
  div(

    div(
      style = "margin-left: 20px",
      div(
        inline(div(class = 'link_mananger')),
        inline(divstru_attr(datalist_name,i,x,obj_size,total_size))
      ),

      if (i %in% c("extra_shape", "som")) div(style = "margin-left: 20px", "class=", class_attr),

      divstru_model(attrx,datalist_name, i, j,x,class_attr),
      divstru_feat(attrx,datalist_name,i,j,x,dlm_getsize)

    )
  )
}
#' @export
div2<-function(attrx,datalist_name,i,dlm_getsize,class_name,names_list,total_size,vals){
  style="display: none"
  div(
    id = ns_attr_out(paste0(datalist_name, i)),
    style = style,

    div(style = "margin-left: 20px",
        div('class=', em(class_name),
            if(class_name=="data.frame"){
              if(i=="numeric"){
                di<-dim(vals$saved_data[[vals$data_dlmX]])
              } else{
                di<-dim(attrx[[i]])
              }
              paste0("(",paste0(di, collapse="x"),")")}

        ),
        div(if (i %in% c("som", "rf", "svm", "nb", "knn", "kmeans", "sgboost", "xyf")) div("Number of models:", em(length(names_list)))),

        if(vals$data_dlmX!="Saved Ensembles"){
          if (class(attrx[[i]])[[1]] == "list") {
            lapply(1:length(names_list), function(j) {
              x<-names_list[[j]]
              obj_size<-my_object.size(attrx[[i]][[j]],dlm_getsize=dlm_getsize)
              class_attr<-class(attrx[[i]][[j]][[1]])[1]
              divstru_attributes(attrx,i,j,x,datalist_name,class_attr,dlm_getsize,obj_size,total_size,names_list)
            })
          }
        }
    )
  )
}
#' @export
myformat<-function(x, ..., total_size) {
  if(!is.null(x)){
    color = colorRampPalette(c("Blue", "Red"))(100)[round(as.numeric(x) / as.numeric(total_size) *
                                                            100)]
    style = paste0("color: ", color)
    em(format(x, "auto"), style = style)
  } else{  }

}
