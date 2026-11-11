
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.



#' @export
tipify_ui<-function(el, title, placement = "bottom", trigger = "hover", options = NULL){
  shinyBS::tipify(el, title, placement = placement, trigger = trigger, options = options)
}


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
get_feature_data_plot<-function(df,m, npic){
  colnames(df)[2]<-"Metric"
  req(is.data.frame(df))


  df[,2]<-df$actual_metric-df[,2]
  df<-df[which(df$var%in%names(sort(sapply(split(df,df$var),function(x) mean(x$Metric)),decreasing=T)[1:npic])),]
}

#' @export

























tiphelp <- function(text, placement = "bottom",class="tipnormal") {
  tags$span(class=class,
    icon("fas fa-question-circle"),
    class = "help-tip",
    style = "margin-left: 2px; cursor: help; color: #2c7fb8;",
    `data-placement` = placement,
    `data-html` = "true",
    `data-original-title` = text
  )
}

tiphelp_icon <- function(icon,text, placement = "bottom") {
  tags$span(
    icon,
    class = "help-tip",
    style = "margin-left: 4px; cursor: help; color: #2c7fb8;",
    `data-placement` = placement,
    `data-html` = "true",
    `data-original-title` = text
  )
}



tipright <- function(text, placement = "right") {
  tiphelp(text, placement = placement)
}










#' @export
pophelp<-function(title,text, placement="right"){
  shinyBS::popify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),title,text, placement = placement, trigger="hover")
}


#' @export
datalist_render <- function(datalist = NULL, bagdata = FALSE, width = "90px") {

  data <- datalist

  factors <- attr(data, "factors")
  time <- attr(data, "time")

  datalist_name <- attr(data, "datalist")

  coords <- attr(data, "coords")
  base_shape <- attr(data, "base_shape")
  layer_shape <- attr(data, "layer_shape")

  data.factors <- attr(data, "data.factors")
  data.time <- attr(data, "data.time")

  transf <- attr(data, "transf")

  div(
    tags$style(HTML(
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

    div(
      class = "rdl",

      div(
        class = "rdl_name",
        h5(
          strong("Datalist:"),
          if (length(datalist_name) > 0) {
            em(datalist_name)
          }
        )
      ),

      div(
        class = "rdl_items",

        if (length(data) > 0) {
          div(
            class = "rdl_blocks",
            h5(div("Data-Attribute")),
            div(
              class = "rdl_in",
              em(
                paste0("n.obs:", nrow(data), ";"),
                paste0("nvar-numeric:", ncol(data))
              )
            )
          )
        },

        if (length(factors) > 0) {
          div(
            class = "rdl_blocks",
            h5(div("Factor-Attribute")),
            div(
              class = "rdl_in",
              em(
                paste0("n.obs:", nrow(factors), ";"),
                paste0("nvar:", ncol(factors))
              )
            )
          )
        },

        if (length(time) > 0) {
          div(
            class = "rdl_blocks",
            h5(div("Time-Attribute")),
            div(
              class = "rdl_in",
              em(
                paste0("n.obs:", nrow(time), ";"),
                paste0("nvar:", ncol(time))
              )
            )
          )
        },

        if (length(coords) > 0) {
          div(
            class = "rdl_blocks",
            div(h5(div("Coords-Attribute"))),
            div(
              class = "rdl_in",
              em(
                paste0("n.obs:", nrow(coords), ";"),
                paste0("nvar:", ncol(coords))
              )
            )
          )
        },

        if (length(base_shape) > 0) {
          div(
            class = "rdl_blocks",
            div(h5(div("Base-shape-Attribute"))),
            div(
              class = "rdl_in",
              em("sf object")
            )
          )
        },

        if (length(layer_shape) > 0) {
          div(
            class = "rdl_blocks",
            div(h5(div("Layer-shape-Attribute"))),
            div(
              class = "rdl_in",
              em("sf object")
            )
          )
        }
      )
    )
  )
}
reshape_names<-function(names){
  names<-  make.names(names, unique = TRUE)
  names <- gsub("\\.+", ".", names)
  names
}

reshape_colnames<-function(data){
  colnames(data)<-  make.names(colnames(data), unique = TRUE)
  colnames(data) <- gsub("\\.+", ".", colnames(data))
  data
}




#' @export
data_migrate<-function(data,newdata, newname=NULL){
  {
    #newdata<-reshape_datalist_colnames(newdata)
   # data<-reshape_datalist_colnames(data)
    attr(newdata, "datalist_root")=attr(data, "datalist_root")
    #attr(newdata, "data.factor")=attr(data,"data.factor")[rownames(newdata),]
    attr(newdata, "datalist")=attr(data, "datalist")



    attr(newdata, "filename")=attr(data, "filename")
    factors<-attr(data, "factors")[rownames(newdata), , drop=FALSE]
    # the levels are kept even when a class has no observations left (e.g. after filtering),
    # so each class keeps its colour, since palettes are built over all the levels
    fac2<-factors
    rownames(fac2)<-rownames(newdata)
    colnames(fac2)<-colnames(factors)
    attr(newdata, "factors")=fac2
    attr(newdata, "coords")= attr(data,"coords")[rownames(newdata), , drop=FALSE]

    reshape_colnames(data)
    time<-attr(data,"time")
    if(!is.null(time)&&all(rownames(newdata)%in%rownames(time))){
      time<-time[rownames(newdata), , drop=FALSE]
    }
    attr(newdata, "time")= time
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
# colours of the levels of a factor, named by level: the palette is built over all the levels, so a
# class keeps its colour when other classes have no observations (ggplot shows only the present ones)
pal_levels<-function(pal,f){
  f<-as.factor(f)
  cols<-pal(nlevels(f))
  names(cols)<-levels(f)
  cols
}

#' @export
ppsummary <- function(m){
  write.table(format(m, justify="left", trim=T),
              row.names=F, col.names=F, quote=F)
}


#' @export
textintro <- function(...) {
  column(12,
         column(12,
                br(),
                h4(strong("Welcome to", span('iMESc', style = "font-family: 'Alata', sans-serif;"), style = "color: #05668D")),
                p(
                  "iMESc is a shiny-based application that allows the performance of end-to-end machine learning workflows. The available resources meet several needs of environmental workflows, but it is not restricted to. iMESc includes tools for data pre-processing, to perform descriptive statistics, supervised and unsupervised machine learning algorithms and interactive data visualization by means of graphs, maps, and tables. Throughout the app, data input and output are organized in modules enabling the creation of multiple ML pipelines. Additionally, it allows saving the workspace in a single file, contributing to the best practices in data-sharing and analysis reproducibility. The app is entirely written in the R programming language and thus it is free. ",
                  tags$a(href = "https://www.frontiersin.org/journals/environmental-science/articles/10.3389/fenvs.2025.1533292/full",
                         target = "_blank", "Read the publication here.")
                ),
                p("iMESc allows users to create a savepoint, a single R object that can be reloaded later to restore analysis output."),
                p("Get started by creating a Datalist. Use the", icon(verify_fa = FALSE, name = NULL, class = "fas fa-plus"), "button."),
                p("To ensure that iMESc content fits nicely on the screen, we recommend a landscape", strong("minimum resolution of 1377 x 768 pixels."))
         )
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
#' @export
# Reads a base/layer shape saved as .rda (load) or .rds (readRDS); a file that is neither (e.g. a
# .shp or .zip) stops with a validation message (shown by the Create Datalist window)
#' @export
imesc_read_shape_rds<-function(path,what="shape"){
  t<-try(silent=TRUE,{
    e<-new.env()
    nm<-suppressWarnings(load(path,envir=e))
    get(nm[1],envir=e)
  })
  if(inherits(t,"try-error")) t<-try(readRDS(path),silent=TRUE)
  validate(need(!inherits(t,"try-error")&&!is.null(t),
                paste0("The ",what," file could not be read: use a .rds (or .rda) file. A shapefile (.shp) can be converted in Pre-processing > Shapefile toolbox.")))
  t
}

# Reads a shapefile uploaded as its separate files (.shp, .dbf, .shx and optionally .prj...).
# Returns NULL (with a notification) when it cannot be read. The name is taken from the .shp file
# (names with more than one dot, e.g. 'basin.v2.shp', work). Without a .prj file, coordinates in
# the longitude/latitude range are taken as WGS84; otherwise the shapefile is refused.
Read_Shapefile <- function(shp_path) {
  fail <- function(msg) {
    showNotification(msg, type = "error", duration = 10)
    NULL
  }
  infiles <- shp_path$datapath # get the location of files
  names_in <- shp_path$name
  required <- c("\\.shp$", "\\.dbf$", "\\.shx$")
  has <- vapply(required, function(x) any(grepl(x, names_in, ignore.case = TRUE)), logical(1))
  if (!all(has)) {
    return(fail("Select at least the three files of the shapefile: '.shp', '.dbf' and '.shx' (and the '.prj', with the coordinate system)."))
  }
  shp_name <- names_in[grepl("\\.shp$", names_in, ignore.case = TRUE)]
  if (length(shp_name) > 1) {
    return(fail("Select the files of one shapefile only (more than one '.shp' file was selected)."))
  }

  dir <- unique(dirname(infiles)) # get the directory
  outfiles <- file.path(dir, names_in) # create new path name

  # Use a for loop to rename files
  for (i in seq_along(infiles)) {
    file.rename(infiles[i], outfiles[i])
  }

  x <- tryCatch(read_sf(file.path(dir, shp_name)), error = function(e) e) # read-in shapefile
  if (inherits(x, "error")) {
    return(fail(paste("The shapefile could not be read:", conditionMessage(x))))
  }
  if (!nrow(x)) {
    return(fail("The shapefile has no features."))
  }
  if (is.na(sf::st_crs(x))) {
    bb <- sf::st_bbox(x)
    if (all(is.finite(bb)) && bb[["xmin"]] >= -180 && bb[["xmax"]] <= 180 && bb[["ymin"]] >= -90 && bb[["ymax"]] <= 90) {
      sf::st_crs(x) <- 4326
      showNotification("The shapefile has no '.prj' file: its coordinates were taken as longitude/latitude (WGS84).", type = "warning", duration = 10)
    } else {
      return(fail("The shapefile has no '.prj' file (coordinate system) and its coordinates are not longitude/latitude: select the '.prj' file too."))
    }
  }
  return(x)
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
textseed<-function(...){
  "A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the analysis."
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
getHelp <- function(fun){
  temp = tools::Rd2HTML(gbRd::Rd_fun(fun),out = tempfile("docs"),dynamic=T)
  content = readr::read_file(temp)
  file.remove(temp)
  content
}
#' @export





#' @export
#' @export
divI<-function(abund,choices=c("N","S","margalef","D","H","J'","Dom_rel","Skewness")){
  res=list()
  if("N"%in%choices){res$N<-rowSums(abund)}
  if("S"%in%choices){res$S<-vegan::specnumber(abund)}
  if("margalef"%in%choices){res$margalef=(vegan::specnumber(abund)-1)/log(rowSums(abund))}
  if("D"%in%choices){res$D<-vegan::diversity(abund, index="simpson")}
  if("H"%in%choices){res$H<-vegan::diversity(abund)}
  if("J'"%in%choices){
    H<-vegan::diversity(abund)
    S<-vegan::specnumber(abund)
    res$J <- H/log(S)}
  if("Dom_rel"%in%choices){res$Dom_rel<-apply(vegan::decostand(abund, "total"),1,sort,T)[1,]}
  if("Skewness"%in%choices){res$Skewness=apply(vegan::decostand(abund,"log"),1,skewness)}
  return(data.frame(do.call(cbind,res)))
}







virtualPicker_unique<-function(id,label,choices,selected=NULL,search=T,multiple=F,allOptionsSelectedText="All",alwaysShowSelectedOptionsCount=F,width=NULL,position="bottom",optionHeight='24px',...){
  div(class="picker13",
      shinyWidgets::virtualSelectInput(
        id,

        label,choices=choices,selected=selected,multiple = multiple,  search =search,
        alwaysShowSelectedOptionsCount=alwaysShowSelectedOptionsCount,
        allOptionsSelectedText=allOptionsSelectedText,

        optionHeight=optionHeight,position=position,
        width=width,
        ...
      )
  )
}
