transf_data<-function(data,transf, cols=1:ncol(data)){

  req(cols%in%colnames(data))
  if(transf=="None"){
    return(data)
  }
  data_end<-data0<-data
  data<-data[cols]
  data<- switch(transf,
                "None"=data,
                "log2"={vegan::decostand(data, "log", na.rm = T, logbase = 2)},
                "log10"={vegan::decostand(data, "log", na.rm = T, logbase = 10)},
                "total"={vegan::decostand(data, "total", na.rm = T)},
                "max"={vegan::decostand(data, "max", na.rm = T)},
                "frequency"={vegan::decostand(data, "frequency", na.rm = T)},
                "range"={vegan::decostand(data, "range", na.rm = T)},
                "pa"={vegan::decostand(data, "pa", na.rm = T)},
                "chi.square"={vegan::decostand(data, "chi.square", na.rm = T)},
                "hellinger"={vegan::decostand(data, "hellinger", na.rm = T)},
                "sqrt2"={sqrt(data)},
                "sqrt4"={sqrt(sqrt(data))},
                "log2(x+1)"={log2(data+1)},
                "log10(x+1)"={log10(data+1)},
                "BoxCox"={
                  imp <- caret::preProcess(data, method = "BoxCox")
                  data <- predict(imp, data)
                  data
                },
                "YeoJohnson"={
                  imp <- caret::preProcess(data, method = "YeoJohnson")
                  data <- predict(imp, data)
                  data
                },
                "expoTrans"={
                  imp <- caret::preProcess(data, method = "expoTrans")
                  data <- predict(imp, data)
                  data
                },
                "exp"=exp(data),
                "exp10"=10^data

  )
  data_end[cols]<-data.frame(data)

  attr(data_end,"transf")<-c(attr(data0,"transf"),transform=transf)

  data_end
}

print_transf<-function(data,transformed){
  original<-summary(unlist(data))[1:6]
  transformed<-summary(unlist(transformed))[1:6]
  res<-data.frame(rbind(original,transformed))
  colnames(res)<-names(original)
  t(res)
}

removed_ids<-function(rm=NULL){
  return(c("Removed IDs"=length(rm)))
}

removed_cols<-function(rm=NULL){
  return(c("Removed cols"=length(rm)))
}

rowCallback_colored <- function(df, coords) {
  # Generate JavaScript code for each cell coordinate
  jsCode <- sapply(1:nrow(coords), function(i) {
    row <- coords[i, 1] - 1  # Adjust for JavaScript zero-indexing
    col <- coords[i, 2] - 1
    sprintf("if(index === %d) { $('td:eq(%d)', row).css('background-color', 'yellow'); }", row, col)
  })
  # Collapse the JavaScript snippets into a single string
  jsCodeStr <- paste(jsCode, collapse = "\n    ")
  rowCallback=JS(sprintf("function(row, data, index) {\n    %s\n  }", jsCodeStr))
  rowCallback
}


option_tips <- {
  list(
    "Rename a Datalist",
    "Combine two or more Datalists, either by rows or columns",
    "Convert variables between numeric and factor types",
    "Replace Attributes with those from a new file",
    "Remove columns, rename them, or concatenate factor levels into new columns",
    "Modify the names of models in a Datalist",
    "Rearrange a Datalist by flipping rows and columns.",
    "Create and manage Shape Attributes.",
    "Run custom R scripts using user-created Datalists within iMESc.",
    "Manage saved Datalists and view their sizes",
    "Permanently delete a Datalist "
  )
}
option_names<-{
  list(
    "Rename Datalist",
    "Merge Datalists",
    "Exchange Factor/Variables",
    "Replace Attributes",
    "Edit Datalist columns",
    "Edit model names",
    "Transpose Datalist",
    "SHP toolbox",
    "Run Script",
    "Datalist mananger",
    "Delete Datalist"
  )
}
get_tips<-function(tips){
  lapply(tips,function(title){
    span(icon("fas fa-question-circle", class="text-info", `data-toggle`="tooltip", `data-placement`="right", title=title)
    )

  })
}
option_tips<-get_tips(option_tips)

split_vector <- function(vetor, positions = c(5, 20)) {
  return(split(vetor, findInterval(seq_along(vetor), c(0, positions), rightmost.closed = TRUE)))
}
option_tabs<-tool2_tabs<-lapply(seq_along(option_names),function(i)
  span(option_names[i],option_tips[i]))





imesc_column_bind<-function(to_merge,fill=T){

  if(isTRUE(fill)){
    binded<-plyr::rbind.fill(lapply(to_merge,function(y){
      td<-data.frame(t(y))
      colnames(td)<-rownames(y)
      td
    }))
    final_bind<-data.frame(t(binded))
  } else{
    bigger<-rep(NA,nrow(to_merge[[names(which.max(sapply(to_merge,nrow)))]]))
    x<-to_merge[[1]]
    match_row_df<-data.frame(sapply(to_merge,function(x){
      names<-   rownames(x)
      bigger[1:length(names)]<-names
      bigger
    }))
    common_elements <- Reduce(intersect, match_row_df)

    if(!length(common_elements)>0){
      return("Error: No common rows found across the selected datasets.Use the option 'fill' or ensure there is at least one common row in all datasets to proceed.")
    }
    final_bind<-do.call(cbind,lapply(to_merge,function(x) x[common_elements,,drop=F]))

  }

}
imesc_row_bind <- function(to_merge, fill = TRUE) {
  to_merge1<-to_merge
  ids<-lapply(to_merge, rownames)
  for(i in 1:length(ids)){
    to_merge1[[i]][,"imesc_id"]<-ids[[i]]
  }

  if (isTRUE(fill)) {
    final_bind <- plyr::rbind.fill(to_merge1)
  } else {
    # Find the common columns across all data frames
    common_columns <- Reduce(intersect, lapply(to_merge, colnames))

    if(!length(common_columns)>0){
      return("Error: No common columns found across the selected datasets. Use the option 'fill' or ensure there is at least one common column in all datasets to proceed.")
    }
    common_columns <- Reduce(intersect, lapply(to_merge1, colnames))
    final_bind <- do.call(rbind, lapply(to_merge1, function(x) x[, common_columns, drop = FALSE]))
  }
  rownames(final_bind)<-make.unique(final_bind$imesc_id)
  final_bind$imesc_id<-NULL

  return(final_bind)
}

imesc_merge<-function(to_merge,mergeby="row",fill=T){
  if(mergeby== "row") {
    imesc_row_bind(to_merge,fill)
  } else{
    res<-imesc_column_bind(to_merge,fill)
    colnames(res)<-make.unique(unlist(sapply(to_merge,function(x) colnames(x))))
    res
  }
}



imesc_data<-function(path,sheet=NULL,attr){
  if(grepl(".csv",path)){
    data<-imesc_fread(path)
  } else{
    if(is.null(sheet)){
      sheets<-readxl::excel_sheets(path =path)
      sheet=sheets[1]
    }
    data<-imesc_excel_sheets(path,sheet=sheet,attr=attr)
  }
  facs<-which(unlist(lapply(data,class))=="character")
  if(length(facs)>0){
    facs2<-data[,facs]
    newfacs<-lapply(facs2,function(x) factor(x))
    data[names(newfacs)]<-newfacs
  }
  data<-remove_na_cols(data)
  if(attr=="Factors"){
    newfacs<-lapply(data,function(x) factor(x))
    data[names(newfacs)]<-newfacs
  }
  pic_log<-which(unlist(lapply(data,is.logical)))
  if(length(pic_log)>0){
    data[,  pic_log ]<- do.call(data.frame,lapply(data[, pic_log, drop=F], function(x) as.factor(x)))
  }

  data

}

na_cols<-function(num){
  pic<-sapply(num,function(x){all(is.na(x))})
  pic
}
remove_na_cols<-function(data){
  pic<-which(sapply(data,function(x){all(is.na(x))}))
  if(length(pic)>0){
    data<-data[,-pic,drop=F]
  }
  data
}

to_num<-function(data){
  if(all(sapply(data,class)=="numeric")){
    return(data)
  } else{
    num0<-suppressWarnings(data.frame(sapply(data,function(x) as.numeric(x))))
    nacol<-na_cols(num0)
    data[,which(!nacol)]<-num0[,which(!nacol)]
    data
  }

}


imesc_excel_sheets<-function(path,sheet,attr="Numeric"){
  df<-data.frame(readxl::read_excel(path, sheet,na=c("","NA"),col_names =F))
  validate(need(!any(duplicated(df[, 1])),"Duplicated row names not allowd"))
  colnames(df)<-NULL
  cols<-make.unique(unlist(df[1,]))[-1]

  df<-df[-1,]

  ids<-make.unique(  as.character(df[,1]))
  df<-df[,-1,drop=F]
  colnames(df)<-cols
  if(any(is.na(ids))){
    ids[is.na(ids)]<-"NA_id"
  }

  if(attr%in%c('Numeric',"Coords")){
    df<-to_num(df)
  }

  rownames(df)<-ids
  df<-data.frame(df)
  facs<-which(unlist(lapply(df,class))=="character")
  if(length(facs)>0){
    facs2<-df[,facs]
    newfacs<-lapply(facs2,function(x) factor(x))
    df[names(newfacs)]<-newfacs
  }

  df

}

imesc_fread<-function(path){

  data<-data.frame(data.table::fread(path, stringsAsFactors = T,na.strings=c("","NA"),header=T))
  validate(need(!any(duplicated(data[, 1])),"Duplicated row names not allowd"))
  rownames(data)<-data[, 1]
  data[, 1]<-NULL
  data
}



check_data<-function(newdata,datalist="nema_araca",attr="Numeric", saved_data){

  data<-saved_data[[datalist]]
  if(attr%in%c("Factor","Coords")){
    base_check<-data
  } else{
    base_check<-attr(data,"factors")
  }
  ids_in<-rownames(newdata)%in%rownames(base_check)
  if(!all(ids_in)){
    errobox<-div(style="height: 200px; overflow-y:scroll; color:gray",
      p("Error: Missing IDs: Please verify and ensure that the IDs match the attributes already present in the Data"),
      p("Missing IDs:"),
      lapply(rownames(newdata)[!ids_in],function(x)div(em(x)))
    )
    return(errobox)
  } else{
    if(attr=="Coords"){
      newdata<-newdata[,1:2]
    }
    newdata
  }
}
emgreen<-function(text){
  em(text,style="color: SeaGreen")
}
embrown<-function(text){
  em(text,style="color: brown")
}
emgray<-function(...){
  em(...,style="color: gray")
}
emwarning<-function(text){
  div(
    class = "alert_warning",
    span(icon("triangle-exclamation",style="color: gold"),emgray(text))

  )
}
basic_summary<-function(data,show_missing=T){
  if(is.null(data)){return(NULL)}
  column(12,style="font-size: 11px; margin-top: 10px",
         column(6,
                div(strong('Number of rows:')),
                div(strong('Number of columns:')),
                if(show_missing)
                div('Missing Values')),
         column(6,
                div(emgreen(nrow(data))),
                div(emgreen(ncol(data))),
                if(show_missing){div(emgreen(sum(is.na(data))))}
         )

  )
}

confirm_modal<-function(ns=ns("id"),action="",data1=NULL,data2=NULL,left="Old:",right="New:",from="",to="",div_left=T,div_right=T,div1_post="",arrow=T,print=NULL){
  showModal(
    modalDialog(
      title=NULL,
      footer=div(

        modalButton("Cancel"),
        actionButton(ns("confirm"),"Confirm")
      ),
      easyClose = T,
      div(style="overflow-x:hidden; overflow-y:auto; max-height: 600px",
          div(action,style="margin-bottom: 15px"),
          get_basic_compare(data1=data1,data2=data2,left=left,right=right,from=from,to=to,div_left=div_left,div_right,div1_post=div1_post,arrow=arrow),
          if(!is.null(print))
            renderPrint(print)

      )

    )
  )
}
get_basic_compare<-function(data1=NULL,data2=NULL,left="New:",right="Old:",from="",to="",div_left=T,div_right=T,div1_post=NULL,arrow=T){
  d1<-div()
  d2<-div()
  d3<-div()
  if(isTRUE(div_left)){
    d1<-div(style="background: #FFF0F5",
            div(span(embrown(strong(left)),from)),
            if(!is.null(data1))
              basic_summary2(data1))
  } else{
    d1<-div_left
  }
  if(isTRUE(div_right)){
    d2<-div( icon("arrow-right", style = "color: #007bff; font-size: 24px;padding-left: 10px; padding-right: 10px"))
    if(isFALSE(arrow)){
      d2<-NULL
    }
    d3<-div(
      style="background: #F0FFF0",
      div(span(strong(right,style="color: #007bff"),to)),
      if(!is.null(data2))
        basic_summary2(data2))
  }

  div(style="display: flex",
      div(d1,div1_post),
      d2,
      d3

  )
}
basic_summary2<-function(data, style_numeric="",style_factor="",style_coords="", name=NULL,missing_name='Missing Values:'){

  if(is.null(data)){return(NULL)}
  if(is.null(name)){
    if(is.factor(data[,1])){
      name<-"Factor-Attribute:"
    } else{
      name<-"Numeric-Attribute:"
    }
  }
  name_datalist<-NULL
  value_name_datalist<-attr(data,"datalist")
  if(!is.null(value_name_datalist)){
    name_datalist<-"Datalist name"
  }



  shapes<-c()
  name_shapes<-name_fac<-name_coords<-NULL
  if(length(attr(data,"base_shape"))>0){
    shapes[length(shapes)+1]<-"base_shape"
  }
  if(length(attr(data,"layer_shape"))>0){
    shapes[length(shapes)+1]<-"layer_shape"
  }
  if(length(attr(data,"extra_shape"))>0){
    shapes[length(shapes)+1]<-"extra_shape"
  }

  if(!is.null(shapes)){
    shapes<-paste(shapes,collapse=", ")
    name_shapes<-"Shapes:"
  }
  dim_fac<-factor_attr<-attr(data,"factors")

  if(!is.null(factor_attr)){
    dim_fac<-div(paste0(dim(factor_attr),collapse=" x "))
    name_fac<-div("Factor-Attribute:",style=style_factor)
  }
  dim_coords<-coords_attr<-attr(data,"coords")
  if(!is.null(coords_attr)){
    dim_coords<-div(paste0(dim(coords_attr),collapse=" x "))
    name_coords<-div("Coords-Attribute:",style=style_coords)
  }

  div(style="font-size: 11px; margin-top: 10px;display: flex",
      div(
        div(name_datalist),
        div(name,style=style_numeric),
        div(missing_name),
        div(name_fac),
        div(name_coords),
        div(name_shapes)
      ),
      div(style="padding-left: 5px",
          div(value_name_datalist),
          div(emgreen(paste0(dim(data),collapse=" x ")),style=style_numeric),
          div(emgreen(sum(is.na(data)))),
          div(emgreen(dim_fac)),
          div(emgreen(dim_coords)),
          div(emgreen(shapes))

      )

  )
}


# Função para tornar a primeira letra maiúscula
first_upper <- function(s) {

  if(nchar(s) == 0) {
    return(s)
  }
  return(paste(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))), sep = ""))
}


shp_help<-function(){
  HTML(
    paste0(
      "Shapefiles are a simple, nontopological format for storing the geometric location and attribute information of geographic features. The shapefile format defines the geometry and attributes of geographically referenced features in three or more files with specific file extensions that should be stored in the same project workspace. Requires at least three files:",
      div(HTML(paste0(hr()))),
      div(HTML(paste0(strong(".shp --")," The main file that stores the feature geometry; required."))),
      div(HTML(paste0(strong(".shx --")," The index file that stores the index of the feature geometry; required."))),

      div(HTML(paste0(strong(".dbf --")," The dBASE table that stores the attribute information of features; required."))),
      div(HTML(paste0(hr()))),
      div(HTML(paste0(em(
        "Each file must have the same prefix, for example, basin.shp, basin.shx, and basin.dbf"
      ))))))
}
create_DATALIST<-function(up_or_ex="example",data,factors,coords,base_shape,layer_shape,name_example='nema_araca', data_name="New Datalist"){
  data=data
  if(is.null(factors)){
    factors<-data.frame(id=as.factor(rownames(data)))
    rownames(factors)<-rownames(data)
  }
  factors_in<-factors[rownames(data),,drop=F]
  validate(need(nrow(factors_in)==nrow(data),''))
  if (any(names(datastr(data)$type.vars)=="factor")){
    for(i in 1:ncol(data)){
      new<-as.numeric(as.character(data[,i]))
      if(!sum(is.na(new))==length(new)){data[,i]<-new}
    }
    num_cols<-which(unlist(lapply(data,function(x)is.numeric(x))))#ok
    data.numerics<-data[,num_cols,drop=F]
    data.factors<-data[,which(unlist(lapply(data,function(x)is.factor(x)))),drop=F]
    if(ncol(data.factors)>0){
      data.factors<-cbind(data.factors)
      rownames(data.numerics)<-rownames(data)
      data<-data.numerics
      attr(data,"data.factors")<-"factors removed from numeric-attribute"
    }
    factors_in[,colnames(data.factors)]<-data.factors
  }
  attr(data,"nobs_ori")<-nrow(data)
  attr(data,"nvar_ori")<-ncol(data)
  if(up_or_ex=="example"){
    attr(data,"filename")<-'nema_araca.csv'
    attr(data,"datalist")<-paste("nema_araca")
  } else {attr(data,"filename")<-data_name
  attr(data,"datalist")<-data_name

  }
  validate(need(sum(rownames(factors_in)%in%rownames(data))>0,"Numeric-Attribute and Factors-Attribute must have conforming IDs (first column). Check these files and reload again;"))
  attr(data,"factors")<-factors_in
  attr(data,"coords")<-coords[rownames(data),]
  attr(data,"base_shape")<-base_shape
  attr(data,"layer_shape")<-layer_shape
  datalist = list(data)
  if(up_or_ex=="example"){
    names(datalist)<-name_example
  } else{
    names(datalist)<-data_name
  }

  datalist
}
find_na<-function(x){
  x==""|x=="NA"|is.na(x)
}



scale_back <- function(data_scaled) {
  scaled_center <- attr(data_scaled, "scaled:center")
  scaled_scale <- attr(data_scaled, "scaled:scale")
  original_data <- sweep(data_scaled, 2, scaled_scale, "*")
  original_data <- sweep(original_data, 2, scaled_center, "+")
  return(round(original_data,14))
}
scale_back_imp <- function(imp,pred) {
  scaled_center <- imp$mean
  scaled_scale <- imp$std
  original_data <- sweep(pred, 2, scaled_scale, "*")
  original_data <- sweep(original_data, 2, scaled_center, "+")
  data<-round(original_data,14)
  data<-num_char_num(data)

  return(data)
}
num_char_num<-function(data){
  a<-data.frame(sapply(data,as.character))
  data.frame(sapply(a,as.numeric))
}





create_newname<-function(saved_data,data,bag='New Datalist',action="datalist"){
  if(is.null(action)){
    action<-"datalist"
  }
  if(action=="datalist"){
    root<-attr(data,"datalist_root")
    oldnames<-names(saved_data)
  } else if(action=="factor-column"){
    root<-NULL
    oldnames<-colnames(attr(data,"factors"))
  } else if(action=="numeric-column"){
    root<-NULL
    oldnames<-colnames(data)
  }

  newname<-paste0(root,bag)
  data.table::last(make.unique(c(oldnames,newname)))
}


#' @export
get_faclevels<-function(factors){
  l<-lapply(factors,levels)
  l2<-mapply(list, l, as.list(colnames(factors)), SIMPLIFY=FALSE)
  l2
}

#' @export
get_selfactors<-function(res,factors){
  x<-res[[1]]
  res1<-lapply(res, function(x){
    level=x[1]
    factor=attr(x,"ancestry")
    val=attr(x,"stselected")
    if(length(level)==1 & length(factor)==1){
      data.frame(factor,level,val=val)}
  })


  if(length(res1)>0){
    pic<-which(unlist(lapply(res1,function(x) !is.null(x))))
    dfsel<-do.call(rbind, res1[pic])
    ls2<-split(dfsel,dfsel$factor)

    ls2<-ls2[ which(names(ls2)%in%colnames(factors))]

    ls3<-mapply(list, ls2,as.list(factors[names(ls2)]) ,SIMPLIFY=FALSE)
    lapply(ls3,function(x) length(x[[2]]))
    ls3<-do.call(cbind,lapply(ls3,function(x)
      x[[2]]%in%x[[1]]$level))
    ls3[ls3==F]<-NA
    factors2<-factors
    # colnames(factors2)%in%colnames(ls3)
    dim(na.omit(ls3))
    factors2<-factors2[,colnames(ls3), drop=F]
    factors2[,colnames(ls3)]<-ls3
    rownames(na.omit(factors2))} else{
      NULL
    }
}
#' @export
gettree<-function(a)
{

  # defining tree as list
  tree<-list()

  # Condition to identifly if selected column is not last column
  if(class(a)!="factor"&&length(a)>1)
  {

    # getting uniques list of names from the columns to create folder
    b<-unique(a[,1])

    # runnig file name in loop
    for(i in b)
    {
      # check if the flie name i not blank
      if(i!="")
      {
        # subset data for content of folder
        subdata<-a[which(a[,1]==i),]

        # if there is only one element for that item change icon as file
        if(length(subdata[,-1])==1)
        {tree[[i]]=structure("",sticon="file",stselected=TRUE)}  else{
          # call the function recursively
          tree[[i]]<-gettree(subdata[,-1])
        }}

    }}

  # Change icon of last columns as file
  if(class(a)=="factor"||length(a)==1)
  {
    for(i in a)
    {
      tree[[i]]=structure("",sticon="file",stselected=TRUE)
    }

  }
  return(tree)
}
#' @export
getclassmat<-function(data.factors)
{
  res<-lapply(data.factors,classvec2classmat)
  namesfac<-names(res)
  colnamesfac<-lapply(res,colnames)
  names<-list()
  for( i in 1:length(namesfac))
  {
    names[[i]]<-paste(namesfac[i], colnamesfac[[i]], sep="_")
  }
  classmat<-do.call("cbind",res)
  colnames(classmat)<-unlist(names)
  return(classmat)
}




get_all_selected_from_choices<-function(selected, choices){
  if(!is.null(selected)){
    if(!all(selected%in%choices)){
      selected<-NULL
    }
  }
  selected[selected%in%choices]
}
fixed_dt<-function(data,scrollY = "400px",scrollX = TRUE,dom = 't',
                   round=3,fixedHeader=TRUE,autoWidth=F,
                   lengthMenu = list(c(20, -1), c( "20","All")),
                   rowCallback=NULL,
                   pageLength = nrow(data),
                   extensions = c('FixedColumns',"FixedHeader"),
                   colored=F,
                   rownames=T,
                   coords=NULL,buttons=NULL
) {
  if(is.null(data)){return(NULL)}
  if(!is.null(round)){
    if(is.numeric(data[1,1])){
      data<-round(data,round)
    }

  }


  if(isTRUE(colored)){
    rowCallback<-rowCallback_colored(data,coords)

  }
  div(


    div(class="dt_small",
        DT::renderDataTable({
          dt<-DT::datatable(
            data,
            extensions = extensions,
            rownames = rownames,
            options = list(
              escape=F,

              autoWidth=autoWidth,
              lengthMenu = lengthMenu,
              pageLength = pageLength,
              dom=dom,
              deferRender = TRUE,
              scroller = TRUE,
              info = FALSE,
              scrollX = scrollX,
              scrollY = scrollY,
              rowCallback = rowCallback,
              buttons=buttons,

              fixedColumns = list(leftColumns = 1, rightColumns = 0))
          )

        })
    )
  )
}



