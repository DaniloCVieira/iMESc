measure_time<-function(f,args) {
  t1<-Sys.time()
  res<-do.call(f,args)
  t2<-Sys.time()
  print(t2-t1)
  res
}

segment_dd<-function (x) {
  x$segments
}
plotNode<-function (x1, x2, subtree, type, center, leaflab, dLeaf, nodePar,edgePar, horiz = FALSE){
  ddsegments <- NULL
  ddlabels <- list()
  wholetree <- subtree
  depth <- 0L
  llimit <- list()
  KK <- integer()
  kk <- integer()
  repeat {
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x
    depth <- depth + 1L
    llimit[[depth]] <- bx$limit
    hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
    if (!hasP) {
      nPar <- nodePar}

    Xtract <- function(nam, L, default, indx) rep(if (nam %in%
                                                      names(L)) {L[[nam]] }else {default}, length.out = indx)[indx]
    asTxt <- function(x) if (is.character(x) || is.expression(x) ||is.null(x)) {x}  else {as.character(x)}
    i <- if (inner || hasP) {
      1} else {2}
    if (!is.null(nPar)) {
      pch <- Xtract("pch", nPar, default = 1L:2, i)
      cex <- Xtract("cex", nPar, default = c(1, 1), i)
      col <- Xtract("col", nPar, default = par("col"),
                    i)
      bg <- Xtract("bg", nPar, default = par("bg"), i)
      points(if (horiz)
        cbind(yTop, xTop)
        else cbind(xTop, yTop), pch = pch, bg = bg, col = col,
        cex = cex)
    }
    if (leaflab == "textlike")
      p.col <- Xtract("p.col", nPar, default = "white",
                      i)
    lab.col <- Xtract("lab.col", nPar, default = par("col"),
                      i)
    lab.cex <- Xtract("lab.cex", nPar, default = c(1, 1),
                      i)
    lab.font <- Xtract("lab.font", nPar, default = par("font"),
                       i)
    lab.xpd <- Xtract("xpd", nPar, default = c(TRUE, TRUE),
                      i)
    if (is.leaf(subtree)) {
      if (leaflab == "perpendicular") {
        if (horiz) {
          X <- yTop + dLeaf * lab.cex
          Y <- xTop
          srt <- 0
          adj <- c(0, 0.5)
        }        else {
          Y <- yTop - dLeaf * lab.cex
          X <- xTop
          srt <- 90
          adj <- 1
        }
        nodeText <- asTxt(attr(subtree, "label"))
        ddlabels$xy <- c(ddlabels$xy, X, 0)
        ddlabels$text <- c(ddlabels$text, nodeText)
      }
    }    else if (inner) {
      for (k in seq_along(subtree)) {
        child <- subtree[[k]]
        yBot <- attr(child, "height")
        if (getOption("verbose"))
          cat("ch.", k, "@ h=", yBot, "; ")
        if (is.null(yBot))
          yBot <- 0
        xBot <- if (center) {
          mean(bx$limit[k:(k + 1)])}else {bx$limit[k] + .midDend(child)}
        hasE <- !is.null(ePar <- attr(child, "edgePar"))
        if (!hasE)
          ePar <- edgePar
        i <- if (!is.leaf(child) || hasE) {
          1}  else {2}
        col <- Xtract("col", ePar, default = par("col"),
                      i)
        lty <- Xtract("lty", ePar, default = par("lty"),
                      i)
        lwd <- Xtract("lwd", ePar, default = par("lwd"),
                      i)
        if (type == "triangle") {
          ddsegments <- c(ddsegments, xTop, yTop, xBot,
                          yBot)
        }        else {
          ddsegments <- c(ddsegments, xTop, yTop, xBot,
                          yTop)
          ddsegments <- c(ddsegments, xBot, yTop, xBot,
                          yBot)
        }
        vln <- NULL
      }
    }
    if (inner && length(subtree)) {
      KK[depth] <- length(subtree)
      if (storage.mode(kk) != storage.mode(KK))
        storage.mode(kk) <- storage.mode(KK)
      kk[depth] <- 1L
      x1 <- bx$limit[1L]
      x2 <- bx$limit[2L]
      subtree <- subtree[[1L]]
    }    else {
      repeat {
        depth <- depth - 1L
        if (!depth || kk[depth] < KK[depth])
          break
      }
      if (!depth)
        break
      length(kk) <- depth
      kk[depth] <- k <- kk[depth] + 1L
      x1 <- llimit[[depth]][k]
      x2 <- llimit[[depth]][k + 1L]
      subtree <- wholetree[[kk]]
    }
  }
  list(segments = ddsegments, labels = ddlabels)
}

imesc_dendrogram_data<-function (x, type = c("rectangle", "triangle"), ...) {
  leaflab <- "perpendicular"
  center <- FALSE
  xlab <- ""
  ylab <- ""
  horiz <- FALSE
  xaxt <- "n"
  yaxt <- "s"
  nodePar <- NULL
  edgePar <- list()
  dLeaf <- NULL
  edge.root <- is.leaf(x) || !is.null(attr(x, "edgetext"))
  type <- match.arg(type)
  hgt <- attr(x, "height")
  if (edge.root && is.logical(edge.root)) {
    edge.root <- 0.0625 * if (is.leaf(x)) {
      1}    else {hgt}
  }
  mem.x <- .memberDend(x)
  yTop <- hgt + edge.root
  if (center) {
    x1 <- 0.5
    x2 <- mem.x + 0.5
  }  else {
    x1 <- 1
    x2 <- mem.x
  }
  xl. <- c(x1 - 1/2, x2 + 1/2)
  yl. <- c(0, yTop)
  if (edge.root) {
    if (!is.null(et <- attr(x, "edgetext"))) {
      my <- mean(hgt, yTop)
    }
  }
  ret <- plotNode(x1, x2, x, type = type, center = center,
                  leaflab = leaflab, dLeaf = dLeaf, nodePar = nodePar,                   edgePar = edgePar, horiz = FALSE)
  ret$segments <- as.data.frame(matrix(ret$segments, ncol = 4,
                                       byrow = TRUE, dimnames = list(NULL, c("x", "y", "xend",
                                                                             "yend"))))
  ret$labels <- cbind(as.data.frame(matrix(ret$labels$xy, ncol = 2,byrow = TRUE, dimnames = list(NULL, c("x", "y")))), data.frame(label = ret$labels$text))
  ret
}
.memberDend<-function (x){
  r <- attr(x, "x.member")
  if (is.null(r)) {
    r <- attr(x, "members")
    if (is.null(r)) {
      r <- 1L
    }
  }
  r
}
plotNodeLimit<-function (x1, x2, subtree, center){
  inner <- !is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    mTop <- .memberDend(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      m <- .memberDend(subtree[[k]])
      xx1 <- xx1 + (if (center) {
        (x2 - x1) * m/mTop
      }      else {
        m
      })
      limit[k] <- xx1
    }
    limit <- c(x1, limit)
  }  else {
    limit <- c(x1, x2)
  }
  mid <- attr(subtree, "midpoint")
  center <- center || (inner && !is.numeric(mid))
  x <- if (center) {
    mean(c(x1, x2))
  }  else {
    x1 + (if (inner) {
      mid
    }    else {
      0
    })
  }
  list(x = x, limit = limit)
}
.midDend<-function (x){
  if (is.null(mp <- attr(x, "midpoint")))
    0  else mp
}
add_ggtheme<-function(p,theme,base_size){
  p<-switch(theme,
            'theme_grey'={p+theme_grey(base_size)},
            'theme_bw'={p+theme_bw(base_size)},
            'theme_linedraw'={p+theme_linedraw(base_size)},
            'theme_light'={p+theme_light(base_size)},
            'theme_dark'={p+theme_dark(base_size)},
            'theme_minimal'={p+theme_minimal(base_size)},
            'theme_classic'={p+theme_classic(base_size)},
            'theme_void'={p+theme_void(base_size)})
  p
}
gg_dendrogram<-function(obs.clusters,hc.clusters,hc.object, palette=NULL, labels=NULL, lwd=2, main="", xlab="Observations", ylab="Height", base_size=12, theme='theme_grey',offset_labels=-.1,xlab_adj=20, legend=c("outside","inside"),base_color="black",angle_label=0,log=F){
  {
    tree_data <- imesc_dendrogram_data(as.dendrogram(hc.object))
    cols<-palette(nlevels(hc.clusters))
    segtree<-segment_dd(tree_data)

    tree_data$labels$group<-hc.clusters[tree_data$labels$label]

    ranges<-data.frame(do.call(rbind,lapply(split(tree_data$labels,tree_data$labels$group),function(x) range(x$x)))
    )

    levs<-levels(hc.clusters)

    group<-lapply(1:nrow(segtree),function(i){
      pic<-as.numeric(which(apply(ranges,1,function(x) between(segtree$xend[i],x[1],x[2]))))
      if(length(pic)==0){pic<-NA}
      pic
    })

    segtree$group<-do.call(c,group)
    segtree$group<-factor(segtree$group,levels=levs)
    #segtree[tree_data$labels$x,"group"]<-hc.clusters[tree_data$labels$label]


    num_clusters<-nlevels(hc.clusters)
    heights <- sort(hc.object$height, decreasing = TRUE)
    cut_height <- heights[num_clusters - 1]


    #  e<-sapply(1:nrow(segtree) ,function(i) segtree$y[i]!=segtree$yend[i])
    #segtree$group[segtree$y>=cut_height&e]<-NA
    segtree2<-segtree
    segtree2$group[segtree$yend>=cut_height]<-NA
  }

  {
    labels_dend<-do.call(rbind,lapply(split(segtree2,segtree2$group),function(x){
      data.frame(x=x$x[which.max(x$y)],y=max(x$y),group=x$group[1])
    }))

    labels_dend$y<-min(labels_dend$y)
    if(!is.null(labels)){tree_data$labels$label<-labels}
    segtree_head<-segtree_a<-segtree[segtree$y>=cut_height,]

    minv<-segtree_a[segtree_a$yend<cut_height,]
    rownames(labels_dend)<-labels_dend$group
    rownames(minv)<-minv$group
    minv[rownames(labels_dend),"y"]<-labels_dend$y
    maxv<-segtree_a[sapply(1:nrow(segtree_a),function(i){
      segtree_a$x[i]!=segtree_a$xend[i]
    }),]
    maxv2<-segtree_a[segtree_a$y>=cut_height,]
    maxv<-maxv[maxv$y<=cut_height,]
    segtree_a<-minv
    segtree_b<-segtree[!is.na(segtree$group),]
    segtree_b<-segtree_b
    segtree_a<-rbind(segtree_a,maxv)
    maxv2$yend[which(maxv2$yend<=cut_height)]<-labels_dend$y[1]
    maxv2<-maxv2[maxv2$y!=cut_height,]

    segtree_head<-maxv2
    p<-ggplot()
    # if(FALSE)
    p<-p+geom_segment(
      data=segtree_b,
      lineend="square",
      aes(x = x,
          y = y,
          xend = xend,
          yend = yend,
          color=group),
      linewidth=lwd
    )
    #if(FALSE)
    p<-p+
      geom_segment(
        data=segtree_head,
        lineend="square",
        aes(x = x,
            y = y,
            xend = xend,
            yend = yend),
        color=base_color,
        linewidth=lwd


      )
    #if(FALSE)
    p<-p+geom_segment(
      data=segtree_a,
      lineend="square",
      aes(x = x,
          y = y,
          xend = xend,
          yend = yend,
          color=group),

      linewidth=lwd)

    p<-p+scale_color_manual(values=cols,name="")


  }

  p<-p+geom_text(aes(x=x,y=y,label=label),data=tree_data$labels,size=base_size*.2,nudge_y=offset_labels,angle=angle_label)



  p<-p+geom_label(data=labels_dend,aes(x,y,label=group),color=cols,nudge_y=-0.1)

  p<-add_ggtheme(p,theme,base_size)+xlab(xlab)+ylab(ylab)+ggtitle(main)
  if(isTRUE(log)){
    p<-p+scale_y_continuous(transform="log")
  }
  p
}






hc_module<-list()
#' @export
hc_module$ui<-function(id){
  module_progress("Loading module: Hierarchichal Clustering")
  ns<-NS(id)
  column(12,class="mp0",style="width: 100%",

         h4("Hierarchical Clustering", class="imesc_title"),

         #actionLink(ns('save_bug'),"save_bug"),
         box_caret(
           ns("box_setup"),
           title="Model setup",
           color="#374061ff",
           inline=F,
           fluidRow(
             style="display: flex; flex-flow: row wrap;font-size: 12px",class="picker13",
             column(
               12,style="margin-bottom: 0px;",div(
                 style="gap: 10px;margin-bottom: 5px",class="som_grid",
                 pickerInput_fromtop_live(
                   ns("data_hc"),
                   strong("Datalist:"),choices = NULL
                 ),
                 radioButtons(ns("model_or_data"), strong("Clustering target:"), choiceValues = c("data", "som codebook"), choiceNames = c("Numeric-Attribute", "SOM-codebook"),width="130px"),
                 div(
                   pickerInput_fromtop_live(ns("som_model_name"), strong("Som model:"), choices=NULL, selected=NULL),
                   div(class="small_check",style="",

                       checkboxInput(ns('show_hcsom_fine'),em("Select layers"))
                   ),
                   div(class="picker_fit inline_pickers",
                       style="background:white;padding: 5px;display:none",
                       id=ns("hcsom_fine"),
                       #checkboxInput(ns("use_weights"),span("Use weights",tipright("Use user weights from som model to calculate a weighted mean distance matrix")),width="250px"),
                       virtualPicker_unique(
                         ns("som_whatmap"),
                         strong("Whatmap",tipright("SOM Layers for clustering")), choices = NULL,search=F,multiple=T,allOptionsSelectedText="All layers",
                         alwaysShowSelectedOptionsCount=F
                       )
                   ),
                 ),


                 pickerInput_fromtop(ns("hc_fun"), strong("HC function:", actionLink(ns("help_hc_fun"), icon("fas fa-question-circle"))), choices = list("Hierarchical Clustering" = "hclust", "Agglomerative Nesting" = "agnes", "Divisive hierarchical clustering" = "diana")),
                 div(id=ns("disthc_id"),
                     pickerInput_fromtop(ns("disthc"), strong("Distance:"), choices = c('bray', "euclidean", 'jaccard'))
                 ),
                 pickerInput_fromtop(ns("method.hc0"),strong( "Method:"), choices = c("ward.D2", "ward.D", "single", "complete", "average", "mcquitty", "median", "centroid"))

               )
             ),
             column(12,style="margin-top: -5px;margin-bottom: 0px;",
                    uiOutput(ns("som_layers"))
             )

           )

         ),
         #actionLink(ns('save_bug'),"save_bug"),

         uiOutput(ns('hc_error')),
         tabsetPanel(id=ns("tabs_view"),title=NULL,
                     #selected="tab4",
                     tabPanel("1. Dendrogram",value="tab1"),
                     tabPanel("2. Scree Plot",value="tab2"),
                     tabPanel("3. Cut Dendrogram",value="tab3")),

         tabsetPanel(
           id=ns("tabs"),
           type="hidden",
           #selected="tab4",

           header=column(12,class="mp0",id=ns("Kcustom"),
                         column(
                           4,class="mp0",
                           box_caret(
                             ns("box_nclust"),
                             title="Number of Clusters",
                             color="#c3cc74ff",
                             div(numericInput(ns("customKdata"),"Number of clusters: ",value = 3,step = 1),
                                 uiOutput(ns("saveHC")),

                                 div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; padding-bottom: 5px',
                                     div(
                                       checkboxInput(ns("hc_sort"),span("Sort clusters",tiphelp("Sort clusters by a  variable")),value=F),
                                       div(style="margin-left: 15px;",
                                           pickerInput_fromtop(ns("hc_ord_datalist"),"Datalist:",choices=NULL,width="200px", options=shinyWidgets::pickerOptions(liveSearch =T)),

                                           pickerInput_fromtop(ns("hc_ord_factor"),"Variable:",choices = NULL,selected=NULL,options=shinyWidgets::pickerOptions(liveSearch=T))
                                       )

                                     )
                                 )
                             ),


                           )
                         )
           ),
           tabPanel(
             "1. Dendrogram",
             value="tab1",
             column(
               4,class="mp0",
               box_caret(ns("box1_a"),
                         title="Options",
                         color="#c3cc74ff",
                         div(textInput(ns("hc_title"), "Title", value =NULL),
                             uiOutput(ns("labhc_out")))
               )
             ),

             column(
               8,class="mp0",style="position: absolute; right: 0px; padding-left: 6px",
               box_caret(ns("box1_b"),
                         title="Plot",
                         button_title=actionLink(ns("download_plot1"),"Download",icon("download")),

                         div(
                           div(id=ns("hcut_btn1"),class="save_changes",
                               actionButton(ns("run_hc1"),"RUN >>")
                           ),
                           uiOutput(ns("hcdata_plot"))
                         )
               )
             )
           ),
           tabPanel(
             "2. Scree Plot",
             value="tab2",
             column(
               4,class="mp0",
               box_caret(
                 ns("box2_a"),
                 title="Options",
                 color="#c3cc74ff",
                 div(
                   div(style="display: flex",
                       numericInput(ns("screeplot_hc_k"), span("k", tipright("maximum number of clusters to be tested")),NULL),
                       div(id=ns('run_screeplot_hc_btn'),style="display: inline-block; vertical-align: top;", class="save_changes",actionButton(ns("run_screeplot_hc"), "Run screeplot"))
                   ),
                   div(style="margin-top: 20px; border-top: 1px solid gray",
                       div(style="display: flex",
                           checkboxInput(ns("show_smw_hc"), value = F,
                                         strong(
                                           "split moving window",
                                           tipright("Performs split moving window to detect significant discontinuities in the relationship between the number of clusters and WSS values. Click for more information.")
                                         )),
                           div(id=ns('run_smw_hc_btn'),class="save_changes",actionButton(ns("run_smw_hc"),"RUN smw")),
                           inline(uiOutput(ns("smw_validate")))
                       ),
                       div(id=ns("hc_smw_control"),
                           # uiOutput("smw_hc_seed_out"),
                           uiOutput(ns("smw_hc_w_out")),
                           numericInput(ns("smw_hc_rand"),"N randomizations",50),
                           numericInput(ns("smw_hc_tol"), span("tol", tiphelp("Adjusts sensitivity when identifying potential breakpoints. If the dissimilarity score (DS) exceeds -tol- times the standard deviation, a breakpoint is suggested.")), 1.5, step=0.1)
                       )),
                   div(
                     actionLink(ns('down_results_screeplot'),"Download Results")
                   )
                 )

               )
             ),

             column(
               8,class="mp0",style="position: absolute; right: 0px; padding-left: 6px",
               box_caret(ns("box2_b"),
                         title="Plot",
                         button_title=actionLink(ns("download_plot2"),"Download",icon("download")),
                         div(
                           uiOutput(ns('smw_error')),
                           uiOutput(ns('smw_error2')),
                           uiOutput(ns("hc_tab2_out")))
               )
             )

           ),
           tabPanel(
             '3. Cut Dendrogram',
             value="tab3",
             column(
               4,class="mp0",style="margin-left: -1px; padding-right: 3px",
               div(style="overflow-y: auto;height: calc(100vh - 200px); padding-left: 1px",
                   box_caret(
                     ns("box3_a"),
                     title="Options",
                     color="#c3cc74ff",
                     div(
                       div(
                         pickerInput_fromtop_live(inputId = ns("hcdata_palette"),label = "HC Palette:",NULL),
                         pickerInput_fromtop(ns("hcut_labels"),"Factor",NULL),
                         div(
                           pickerInput_fromtop(ns("hcut_theme"),"Theme:",c('theme_minimal','theme_grey','theme_linedraw','theme_light','theme_bw','theme_classic')),
                           numericInput(ns("hcut_cex"),"Size",value = 12,step = 1),
                           numericInput(ns("hcut_lwd"),"Line width",value = .5,step = .5),
                           textInput(ns("hcut_main"),"Title","Cluster Dendrogram"),
                           textInput(ns("hcut_ylab"),"y label","Height"),
                           textInput(ns("hcut_xlab"),"x label","Observations"),
                           numericInput(ns("hcut_xlab_angle"),"rotate labels",value = 0,step =15),
                           numericInput(ns("hcut_offset"),"offset",value = -.1,step = 0.05),
                           checkboxInput(ns("hcut_log"),"Log Scale:",F)
                         )
                       )
                     )

                   )
               )),

             column(
               8,class="mp0",style="position: absolute; right: 12px; padding-left: 15px",
               box_caret(ns("box3_b"),
                         title="Plot",
                         button_title=actionLink(ns("download_plot3"),"Download",icon("download")),
                         div(
                           div(id=ns("hcut_btn"),class="save_changes",
                               actionButton(ns("run_hc"),"RUN >>")
                           ),

                           uiOutput(ns("hcut_plot"))
                         )
               )
             )),
           tabPanel('4. Codebook clusters',
                    value="tab4",
                    column(
                      4,class="mp0",style="margin-left: -1px; padding-right: 3px",
                      div(style="overflow-y: auto;height: calc(100vh - 200px); padding-left: 1px",
                          box_caret(
                            ns("box_4mapping"),
                            color="#c3cc74ff",
                            tip=tiphelp("Add predictions from new data to the trained SOM", "bottom"),
                            title=span(style="display: inline-block",
                                       class="checktitle",
                                       checkboxInput(ns("hcsom_newdata") ,label =strong(span("Predict")),F,width="80px")
                            ),
                            div(
                              uiOutput(ns("hc_save_tab4")),
                              uiOutput(ns("hcsom_newdata_mess")),
                              uiOutput(ns("out_hcsom_whatmap")))
                          ),
                          box_caret(
                            ns("box4_a"),
                            title="Neurons",
                            color="#c3cc74ff",
                            div(

                              checkboxInput(ns("fill_neurons"),"Fill",T),
                              pickerInput_fromtop_live(ns("bg_palette"),label ="Palette",NULL),
                              div(id=ns("neu_options"),


                                  numericInput(ns("pcodes_bgalpha"),"Lightness",value = 0,min = 0,max = 1,step = .1),
                                  pickerInput_fromtop(ns("pclus_border"),label ='Border:',choices = NULL),
                              ),
                              numericInput(ns("border_width"),"Border width",value = 0.5,step=0.1),
                              textInput(ns("neuron_legend_text"),"Legend text","Group")

                            )),
                          box_caret(
                            ns("box4_points"),
                            color="#c3cc74ff",
                            title=span(style="display: inline-block",
                                       class="checktitle",
                                       checkboxInput(ns("pclus_addpoints"),"Points",value=T,width="80px")
                            ),
                            div(id=ns("pclus_points_inputs"),
                                pickerInput_fromtop_live(inputId = ns("pclus_points_palette"),label ="Palette",choices =NULL),
                                div(
                                  id=ns("options_points_factor"),
                                  pickerInput_fromtop(ns("pclus_points_factor"),"Factor",
                                                      choices = NULL),
                                  tags$div(id=ns("color_factor"),
                                           class="form-group shiny-input-container",
                                           tags$label(class = "control-label", " + Factor"),
                                           tags$div(class="dummy-input",
                                                    "Choose a gradient palette for adding a factor",style="color: gray"
                                           )
                                  ),

                                  pickerInput_fromtop(inputId = ns("pclus_symbol"),label = "Shape",choices=NULL),
                                  numericInput(ns("pclus_points_size"),"Size",value = 1,min = 0.1,max = 3,step = .1),
                                  checkboxInput(ns("pclus_show_legend"),"Show legend",T),
                                  textInput(ns("pclus_points_legend_text"),"Legend text","Observations"),

                                ))
                          ),
                          box_caret(
                            ns("box4_text"),
                            color="#c3cc74ff",
                            title=span(style="display: inline-block",
                                       class="checktitle",
                                       checkboxInput(ns("pclus_addtext"),"Labels",value=F,width="80px")
                            ),
                            div(id=ns('pclus_addtext_out'),
                                pickerInput_fromtop_live(ns("pclus_text_palette"),label ="Palette",NULL),
                                pickerInput_fromtop(ns("pclus_text_factor"),"Factor",choices = NULL),
                                numericInput(ns("pclus_text_size"),"Size",value = 1,min = 0.1,max = 3,step = .1),
                                checkboxInput(ns("text_repel"),"Repel Labels",F),
                                numericInput(ns("max.overlaps"),"max.overlaps",value = 10,min = 1,step = 1)
                            )),
                          box_caret(
                            ns("box4_vfm"),
                            color="#c3cc74ff",
                            button_title=tipify_ui(actionLink(ns("varfacmap"), icon("fas fa-question-circle")),"Click for details","right"),
                            title=span(style="display: inline-block",
                                       class="checktitle",

                                       checkboxInput(ns("varfacmap_action"),span("Variable factor map"),value =T,width="210px"),

                            ),
                            div(id=ns('varfac_out'),
                               # pickerInput_fromtop(ns("vfm_layer"),"Layer:",choices =NULL),
                                pickerInput_fromtop(ns("vfm_type"),"Show correlation:",choices =list("Highest"='var', "Chull"="cor","Cluster"="cor_hc")),

                                numericInput(ns("npic"), span(tiphelp("Number of variables to display"),"Number"), value = 10, min = 2),
                                numericInput(ns("pclus.cex.var"), "Var size", value = 1, min = 2),
                                div(class="palette",
                                    pickerInput_fromtop(ns("p.clus.col.text"),label = "Var text color",choices =NULL )),
                                pickerInput_fromtop(ns("var_bg"),label = "Var background",choices = NULL),
                                numericInput(ns("var_bg_transp"), "Var transparency", value = 0, min = 2),

                                div(actionLink(ns("create_dl_vfm"),span("Create Datalist",tiphelp("Create Datalist with variables from VFM")),icon("creative-commons-share")))
                            )
                          ),
                          box_caret(
                            ns("box_var_pie"),
                            color="#c3cc74ff",
                            button_title=tipify_ui(actionLink(ns("var_pie_help"), icon("fas fa-question-circle")),"Click for details","right"),
                            title=span(style="display: inline-block",
                                       class="checktitle",

                                       checkboxInput(ns("var_pie"),strong("Variable pies"),value =F,width="210px"),

                            ),

                            div(id=ns('var_pie_out'),
                                pickerInput_fromtop(ns("var_pie_type"),"Show:",choices =list("Top importance by cluster"='top_hc',"Top importance"="rsquared" ,"Relative importance"="top","Top weight"="top_w","Manual"="manual")),
                                pickerInput_fromtop(ns("var_pie_layer"),"Layer",NULL),
                                div(class="virtual-130",
                                    virtualPicker(ns("var_pie_manual"),"variables selected")
                                ),

                                numericInput(ns("var_pie_n"), span(tipright("Number of variables to display"),"Number"), value = 10, min = 2),
                                pickerInput_fromtop_live(ns("var_pie_bg"),label = "Palette",choices = NULL),
                                numericInput(ns("var_pie_transp"), "Transparency", value = 0, min = 2))
                          ),


                          box_caret(
                            ns("box4_more"),
                            title = "General options",
                            color="#c3cc74ff",
                            div(
                              numericInput(ns("base_size"),"Base size",value = 12),
                              textInput(ns("hcs_title"), "Title: ", ""),
                              checkboxInput(ns("hcs_theme"),label = "show neuron coordinates",value = F),
                              div(actionLink(ns('create_codebook'),"Create Datalist with the Codebook and HC class")),
                              div(tipify_ui(downloadLink(ns('down_hc_model'),"Download HC model", style="button_active"),"Download file as .rds"))
                            )
                          )

                      )),
                    column(
                      8,class="mp0",style="position: absolute;right: 0px",
                      box_caret(
                        ns("box4_b"),
                        title="Plot",
                        button_title=actionLink(ns("download_plot4"),"Download",icon("download")),
                        div(
                          id=ns("hc_tab4_out"),
                          div(
                            div(id=ns("run_bmu_btn"),
                                actionButton(ns("run_bmu"),"RUN >>")
                            ),
                            div(
                              style="position: absolute;top: 25px; right: 0px",
                              uiOutput(ns("importance_results")),
                              uiOutput(ns("create_importance_results"))


                            )
                          ),
                          plotOutput(ns("BMU_PLOT")),


                        )

                      )
                    )


           ),
           tabPanel(
             '5. Codebook screeplot',
             value='tab5',
             column(
               4,class="mp0",
               box_caret(
                 ns("box5_a"),
                 title="Options",

                 color="#c3cc74ff",
                 div(
                   numericInput(ns("mapcode_loop_K"), "K", 20),
                   checkboxGroupInput(ns("show_mapcode_errors"), 'Show error: ',
                                      choices = c("Within Sum of Squares", "Dendrogram Height"), selected=c("Within Sum of Squares", "Dendrogram Height")),
                   textInput(ns('code_screeplot_title'), "Title", ""),
                   pickerInput_fromtop(ns('code_screeplot_agg'), "Aggregate Errors", c("Mean", "Median", "Sum"))

                 )

               )
             ),

             column(
               8,class="mp0",style="position: absolute; right: 0px; padding-left: 6px",
               box_caret(ns("box5_b"),
                         title="Plot",
                         button_title=actionLink(ns("download_plot5"),"Download",icon("download")),
                         div(
                           actionButton(ns("mapcode_loop_go"), "Run loop"),
                           uiOutput(ns("plot5")))
               )
             )

           )
         )
  )
}

#' @export
hc_module$server<-function(id, vals){
  moduleServer(id,function(input, output, session) {
    ns<-session$ns

    ##


    som.hc.object<-reactive({
      res<-phc()$hc.object
      req(res)
      res
    })
    som.hc.clusters<-reactive({
      res<-phc()$som.hc
      req(res)
      res
    })
    som.obs.clusters<-reactive({
      res<-phc()$somC
      req(res)
      attr(res,"order")<-list(
        hc_sort=input$hc_sort,
        hc_ord_datalist=input$hc_ord_datalist,
        hc_ord_factor=input$hc_ord_factor
      )

      res
    })
    cluster_already<-reactive({
      req(input$data_hc)
      datao<-vals$saved_data[[input$data_hc]]
      factors<-attr(datao,"factors")
      req(rownames(factors)%in%names(phc()$somC))
      hc<-as.character(phc()$somC[rownames(factors)])
      fac<-as.list(factors)
      fac<-lapply(fac,function(x) as.character(x))
      cluster_already<-which(sapply(fac, function(x) identical(x, hc)))
      names(cluster_already)
    })
    output$saveHC<-renderUI({

      clu_al<-cluster_already()
      if (length(cluster_already()) > 0) {
        class1<-"button_normal"
        class2<-"div"
        class3<-'divnull'
      } else {
        class1<-"save_changes"
        class2<-"divnull"
        class3<-'div'
      }

      div(style="display: flex",
          div(style="display: flex",
              div(class = class1,style="padding-right: 5px",
                  actionButton(ns("tools_savehc"), icon("fas fa-save"),  type = "action", value = FALSE)), span(style = "font-size: 12px", icon("fas fa-hand-point-left"), "Save Clusters in Datalist ", strong("X"), class = class3)
          )
          ,
          div(style = "margin-bottom: 5px", class = class2,style="text-direction: normal",em(paste0("The current clustering is saved in the Factor-Attribute as '", paste0(clu_al, collapse = "; "), "'"))))
    })

    observeEvent(som.hc.object(),{
      model_name=hcargs()$model_name
      attr_hc=hcargs()$attr_hc
      attr(attr(vals$saved_data[[input$data_hc]],attr_hc)[[model_name]],"hc.object")<-som.hc.object()
    })

    observeEvent(phc(),{
      model_name=hcargs()$model_name
      attr_hc=hcargs()$attr_hc
      nl<-as.character(nlevels(som.hc.clusters()))
      attr(attr(vals$saved_data[[input$data_hc]],attr_hc)[[model_name]],"hc.clusters")[[nl]]<-som.hc.clusters()
      attr(attr(vals$saved_data[[input$data_hc]],attr_hc)[[model_name]],"obs.clusters")[[nl]]<-som.obs.clusters()
    })
    observeEvent(getdata_hc(),{

      if(is.null(attr(vals$saved_data[[input$data_hc]],"hc"))){
        attr(vals$saved_data[[input$data_hc]],"hc")<-list()
        attr(vals$saved_data[[input$data_hc]],"hc")[["Numeric-hc"]]<-"hc-models"
      }
    })
    hcargs<-reactive({
      if(input$model_or_data == "som codebook"){
        attr_hc="som"
        model_name=input$som_model_name
      } else{
        attr_hc="hc"
        model_name='Numeric-hc'
      }
      list(attr_hc=attr_hc,model_name=model_name)
    })
    cur_som.hc.object<-reactive({
      vals$cur_hc<-NULL
      model_name=hcargs()$model_name
      attr_hc=hcargs()$attr_hc
      cur<-attr(attr(vals$saved_data[[input$data_hc]],attr_hc)[[model_name]],"hc.object")
      #vals$cur_hc<-cur


      req(cur)
      cur
    })


    observe({
      model_name=hcargs()$model_name
      attr_hc=hcargs()$attr_hc
      req(input$data_hc)

      req(input$data_hc%in%names(vals$saved_data))
      req(!is.null(attr(vals$saved_data[[input$data_hc]],attr_hc)))
      req(model_name%in%names(attr(vals$saved_data[[input$data_hc]],attr_hc)))
      cur0<-attr(attr(vals$saved_data[[input$data_hc]],attr_hc)[[model_name]],"obs.clusters")
      req( as.character(input$customKdata)%in%names(cur0))
      cur<-cur0[[ as.character(input$customKdata)]]
      shinyjs::toggleClass("hcut_btn","save_changes",condition = is.null(cur))

    })
    cur_som.obs.clusters<-reactive({
      req(input$data_hc)
      model_name=hcargs()$model_name
      attr_hc=hcargs()$attr_hc
      req(input$data_hc%in%names(vals$saved_data))
      attrs_hc<-attr(vals$saved_data[[input$data_hc]],attr_hc)
      req(attrs_hc)
      req(model_name%in%names(attrs_hc))
      attrs_hc_obs<-attr(attrs_hc[[model_name]],"obs.clusters")
      req(as.character(input$customKdata)%in%names(attrs_hc_obs))
      cur<-attrs_hc_obs[[ as.character(input$customKdata)]]
      req(cur)
      cur
    })
    cur_som.hc.clusters<-reactive({
      model_name=hcargs()$model_name
      attr_hc=hcargs()$attr_hc
      cur<-attr(attr(vals$saved_data[[input$data_hc]],attr_hc)[[model_name]],"hc.clusters")[[ as.character(input$customKdata)]]
      req(cur)
      cur
    })

    observeEvent(cur_som.obs.clusters(),{
      res<-attr(cur_som.obs.clusters(),"order")
      updateCheckboxInput(session,'hc_sort',value=res$hc_sort)
      updatePickerInput(session,'hc_ord_factor',selected=res$hc_ord_factor)
      updatePickerInput(session,'hc_ord_factor',selected=res$hc_ord_factor)
    })







    hcplot3<-reactiveVal()
    hcut_argsplot<-reactive({
      hc.object<-cur_som.hc.object()
      hc.clusters<-cur_som.hc.clusters()
      obs.clusters<-cur_som.obs.clusters()
      args<-list(
        obs.clusters=obs.clusters,
        hc.object=hc.object,
        hc.clusters=hc.clusters,
        palette=vals$newcolhabs[[input$hcdata_palette]],
        labels=get_hcut_labels(),
        lwd=input$hcut_lwd,
        base_size=input$hcut_cex,
        main=input$hcut_main,
        xlab=input$hcut_xlab,
        ylab=input$hcut_ylab,
        theme=input$hcut_theme,
        offset_labels=input$hcut_offset,
        angle=input$hcut_xlab_angle,
        log=input$hcut_log
      )
      args
    })
    output$hcut_plot<-renderUI({
      renderPlot({
        args<-hcut_argsplot()

        p<-do.call(gg_dendrogram,args)

        hcplot3(p)
        p


      })
    })
    observeEvent(args_hc2(),{
      vals$hc_messages<-NULL
      hcplot3(NULL)
      #  shinyjs::addClass("hcut_btn","save_changes")

    })
    observeEvent(ignoreInit = T,input$download_plot3,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=do.call(gg_dendrogram,hcut_argsplot())
      name_c=paste0("Dendrogram_",input$customKdata,"groups")
      datalist_name=attr(getdata_hc(),'datalist')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message=name_c, name_c=name_c,datalist_name=datalist_name)
    })

    argsplot_somplot<-reactive({


      req(input$pclus_points_palette)
      req(input$pcodes_bgalpha)


      indicate=indicate_hc()
      m<-getmodel_hc()
      som.hc<-cur_som.hc.clusters()


      tryco<-try(copoints_scaled(), silent = T)
      req(class(tryco)!='try-error')

      trybp<-try( bp_som(), silent = T)

      req(class(trybp)!='try-error')

      errors<-NULL
      copoints2<-vals$copoints2
      copoints3<-copoints2
      args<-list(m=m,
                 hexs=get_network(),
                 points_tomap=copoints_scaled(),
                 bp=bp_som(),
                 points=input$pclus_addpoints,
                 points_size=input$pclus_points_size,
                 points_palette=input$pclus_points_palette,
                 pch=as.numeric(input$pclus_symbol),
                 text=input$pclus_addtext,
                 text_size=input$pclus_text_size,
                 text_palette=input$pclus_text_palette,
                 bg_palette=input$bg_palette,
                 newcolhabs=vals$newcolhabs,
                 bgalpha=input$pcodes_bgalpha,
                 border=input$pclus_border,
                 indicate=indicate$indicate,
                 cex.var=as.numeric(input$pclus.cex.var),
                 col.text=input$p.clus.col.text,
                 col.bg.var=input$var_bg,
                 col.bg.var.alpha=1-input$var_bg_transp,
                 show_error=errors,
                 base_size=input$base_size,
                 show_neucoords=input$hcs_theme,
                 newdata=input$newdata_hc,
                 title=input$hcs_title,
                 hc=som.hc,
                 var_pie=input$var_pie,
                 var_pie_type=input$var_pie_type,
                 n_var_pie=input$var_pie_n,
                 Y_palette=input$var_pie_bg,
                 var_pie_transp=input$var_pie_transp,
                 var_pie_layer=input$var_pie_layer,
                 pie_variables=input$var_pie_manual,
                 border_width=input$border_width,
                 fill_neurons=input$fill_neurons,
                 text_repel=input$text_repel,
                 max.overlaps=input$max.overlaps,
                 show_legend=input$pclus_show_legend,
                 neuron_legend=input$neuron_legend_text,
                 points_legend=input$pclus_points_legend_text
      )

      args

    })

    output$BMU_PLOT<-renderPlot({


      args<-argsplot_somplot()



      args$hc<-cur_som.hc.clusters()

      p<-do.call(bmu_plot_hc,args)
      hcplot4(p)
      p


    })
    observeEvent(argsplot_somplot(),{
      vals$hc_messages<-NULL
      shinyjs::addClass("run_bmu_btn","save_changes")
    })
    hcplot4<-reactiveVal()
    observeEvent(hcplot4(),{
      shinyjs::removeClass("run_bmu_btn","save_changes")
    })
    ##

    observeEvent(input$show_hcsom_fine,ignoreInit = T,{
      shinyjs::toggle("hcsom_fine")
    })




    output$importance_results<-renderUI({
      imp_results<-attr(hcplot4(),"imp_results")
      #req(imp_results)
      actionLink(ns("show_hc_imp"),"Importance results",icon("expand"))
    })
    output$create_importance_results<-renderUI({
      p<-hcplot4()
      #imp_results<-attr(p,"imp_results")
      imp_vars<-attr(p,"imp_vars")
      req(imp_vars)
      #req(imp_results)
      div(actionLink(ns("create_hc_imp"),span("Create Datalist",tiphelp("Create Datalist with selected variables for pies")),icon("creative-commons-share")))
    })

    observeEvent(input$create_hc_imp,{

      p<-hcplot4()

      imp_layer<-attr(p,"imp_layer")
      imp_vars<-attr(p,"imp_vars")

      req(imp_layer)
      req(imp_vars)
      req(imp_layer%in%names(vals$saved_data))
      imp_vars<-attr(p,"imp_vars")
      data_o<-vals$saved_data[[imp_layer]]
      req(data_o)
      req(imp_vars%in%colnames(data_o))
      data<-data_o[,imp_vars,drop=F]
      req(data)
      data<-data_migrate(data_o,data)

      bag<-paste0(imp_layer,"_som_top_vars")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,"bag")<-bag
      vals$newdatalist<-data
      module_save_changes$ui(ns("som-imp-create"), vals)

    })


    module_save_changes$server("som-imp-create", vals)
    observeEvent(input$show_hc_imp,{
      data<-attr(hcplot4(),"imp_results")
      req(data)
      showModal(
        modalDialog(
          title="SOM Variable Importance Results",
          easyClose = T,
          div(class="half-drop-inline",
              div(actionLink(ns("download_hc_imp"),"Download",icon("download"))),

              fixed_dt(data,dom = 'lt',
                       pageLength=20,
                       lengthMenu = list(c(20, -1), c( "20","All")))
          )


        )
      )
    })
    observeEvent(input$download_hc_imp,{
      data<-data.frame(attr(hcplot4(),"imp_results"))
      req(data)
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"som_imp_results"
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Permutation Importance Results",data=data, name=name)

    })
    observeEvent(input$var_pie_layer,{
      m<-getmodel_hc()


      req(input$var_pie_layer%in%names(m$codes))
      order<-order(colMeans(abs( m$codes[[input$var_pie_layer]])),decreasing=T)
      choices<-colnames(m$codes[[input$var_pie_layer]])[order]


      shinyWidgets::updateVirtualSelect(
        "var_pie_manual",
        choices=choices,
        selected=choices[1:4]
      )
    })
    observe({
      shinyjs::toggle('var_pie_manual',condition=input$var_pie_type%in%"manual")
      shinyjs::toggle('var_pie_n',condition=!input$var_pie_type%in%"manual")
    })
    observeEvent(input$var_pie_help,{
      showModal(
        modalDialog(
          title = "Variable Pies in SOM codebook",
          easyClose = TRUE,
          div(
            p("The variables to display in the pie plots can be ranked using four methods:"),
            p("The pie plots represent variables from the trained codebook, where each value is calculated as the square root of the squared codebook weights. This allows for a straightforward comparison of variable contributions within each SOM unit."),
            tags$ul(
              tags$li(strong("Top importance by cluster"),
                      p("The groups based on the hierarchical clustering (HC) results assigned to neurons are used to split the codebook. For each group, the sum of the codebook weights for each variable is calculated, providing a measure of the absolute importance of each variable within each group. These importance scores are normalized by the total importance of each variable across all groups, resulting in a relative importance score for each variable within each group. The variables are ranked using relative importance scores for each group.")
              ),
              tags$li(strong("Top importance"),
                      p("Variables are ranked based on their ability to explain the clustering of the SOM. Importance is calculated by evaluating the coefficient of determination (R²) for each variable, representing the proportion of variance explained relative to the data associated with each classification unit in the SOM. Variables with the highest R² values are selected, highlighting those that contribute most to the SOM’s data structure.")
              ),
              tags$li(strong("Relative importance"),
                      p("The importance of each variable is determined based on the codebook weights for each neuron. The relative importance scores for each variable are calculated by normalizing the codebook weights by the sum of weights for each neuron. The top variables with the highest sum of relative importance scores across all neurons are identified and plotted using pies representing their weights in the codebook.")
              ),
              tags$li(strong("Top weight"),
                      p("The absolute importance of each variable is determined based on the sum of codebook weights for each variable across all neurons. The top variables with the highest absolute weights are identified and plotted using pies representing their weights in the codebook.")
              ),
              tags$li(strong("Manual"),
                      p("Select manually the variables to display.")
              )
            )
          )
        )
      )
    })
    observeEvent(getmodel_hc(),{
      updatePickerInput(session,'var_pie_layer',choices=names(getmodel_hc()$data))

    })
    observe({
      shinyjs::toggle('var_pie_layer',condition=length(getmodel_hc()$data)>1)
    })
    observeEvent(input$var_pie,{
      if(isTRUE(input$var_pie)){
        lapply(c('fill_neurons','pclus_addpoints','varfacmap_action'),function(x){
          updateCheckboxInput(session,x,value=F)
        })


      }
    })
    observe({
      shinyjs::toggle('neu_options',condition=isTRUE(input$fill_neurons))
    })
    observeEvent(input$fill_neurons,ignoreInit = T,{
      if(isFALSE(input$fill_neurons)){
        updatePickerInput(session,"pclus_points_palette",
                          selected="black",
                          choices =  vals$colors_img$val[getsolid_col()],
                          choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ))
      } else{
        updatePickerInput(session,"pclus_points_palette",
                          choices =  vals$colors_img$val,
                          choicesOpt = list(content =  vals$colors_img$img ),selected=vals$pclus_points_palette)
      }
    })
    observe({
      req(is.null(vals$pclus_points_palette))

      vals$pclus_points_palette<-"black"

    })
    observe({
      shinyjs::toggle("options_points_factor",condition = isTRUE(input$fill_neurons))

    })
    observe({
      shinyjs::toggle("var_pie_out",condition=isTRUE(input$var_pie))
    })
    observe({
      updatePickerInput(session,'var_pie_bg',choices = vals$colors_img$val[getgrad_col()],selected="viridis",choicesOpt = list(content =  vals$colors_img$img[getgrad_col()]))

    })
    box_caret_server("box_var_pie")

    output$hc_error<-renderUI({
      req(vals$hc_messages)
      messages<-vals$hc_messages
      render_message(messages)
    })

    observeEvent(input$tabs_view,{
      vals$cur_hc_tab<-input$tabs_view
    })

    observeEvent(input$tabs_view, {
      updateTabsetPanel(session,'tabs',selected=input$tabs_view)

    })





    observeEvent(input$model_or_data,{
      if(input$model_or_data== "data"){
        removeTab("tabs_view","tab4")
        removeTab("tabs_view","tab5")
      } else{
        insertTab("tabs_view",tabPanel('4. Codebook clusters',value="tab4"),select=T)
        insertTab("tabs_view",tabPanel('5. Codebook screeplot',value='tab5')
        )
      }
    })

    box_caret_server('box_setup')
    box_caret_server('box_nclust')
    box_caret_server('box1_a')
    box_caret_server('box1_b')
    box_caret_server('box2_a')
    box_caret_server('box2_b')
    box_caret_server('box3_a')
    box_caret_server('box3_b')
    box_caret_server('box4_a')
    box_caret_server('box4_points')
    box_caret_server('box4_text')
    box_caret_server('box4_vfm')
    box_caret_server('box4_more')
    box_caret_server('box_4mapping')
    box_caret_server('box4_b')
    box_caret_server('box5_a')
    box_caret_server('box5_b')

    output$out_hcsom_whatmap<-renderUI({
      req(input$hcsom_newdata)
      req(isTRUE(input$hcsom_newdata))
      req(input$model_or_data)
      req(input$model_or_data == "som codebook")
      layers<-getsom_layers()
      div(style = "margin-left: 20px;",
          tags$style(HTML(
            ""
          )),
          strong("New Data:"),
          lapply(layers, function(x) {
            div(class = "map_control_style2", style = "color: #05668D",
                checkboxInput(ns(paste0("hcsom_layer", x)), div(
                  style="display: flex; align-items:center;margin-top: -8px;height: 30px",
                  x,uiOutput(ns(paste0("hcsom_piclayer", x)))
                ), TRUE),

            )
          })
      )
    })
    output$smw_validate<-renderUI({
      req(isTRUE(input$show_smw_hc))
      req(length(input$smw_hc_w)>0)
      ws<-as.numeric(unlist(strsplit(input$smw_hc_w,",")))
      max<-input$screeplot_hc_k
      validate(need(!any(ws>max),
                    "Running SMW is unavailable ecause the maximum size should not exceed half of K."))
      validate(need(!any(ws%%2 == 1),"Running SMW is unavailable as all window sizes must be even."))
      NULL

    })
    output$smw_hc_w_out<-renderUI({
      req(vals$screeplot_results0$WSS)
      ws=seq(2,length(vals$screeplot_results0$WSS)/2,by=2)
      tags$div(id="smw_hc_pool",
               textInput(
                 ns("smw_hc_w"),
                 span("Window sizes",tiphelp("comma-delimeted")),
                 value = paste0(ws,collapse=", ")
               )
      )
    })
    output$smw_hc_seed_out<-renderUI({
      numericInput(ns("smw_hc_seed"),"Seed",NA)
    })











    output$hc_tab2_out<-renderUI({

      req(!is.null(vals$screeplot_results))
      div(
        div(strong("Scree plot",
                   actionLink(ns('screeplothelp'), icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"))
        ), style = "margin-bottom: 5px;"),
        div(uiOutput(ns('plot_hc_screeplot')))


      )
    })
    observeEvent(input$hc_ord_datalist,{
      vals$cur_hc_ord_datalist<-input$hc_ord_datalist
    })



    observe({
      shinyjs::toggle('hc_ord_factor',condition=isTRUE(input$hc_sort))

      shinyjs::toggle('hc_ord_datalist',condition=isTRUE(input$hc_sort))
    })


    observeEvent(getdata_for_hc(),{
      selected=vals$cur_hc_ord_datalist
      choices = c(names(vals$saved_data[getdata_for_hc()]))
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'hc_ord_datalist',choices=choices,selected=selected)
    })


    observeEvent(input$hc_ord_datalist,{
      req(input$hc_ord_datalist)
      data<-vals$saved_data[[input$hc_ord_datalist]]
      choices=c(colnames(data))
      selected=vals$cur_hc_ord_factor
      selected<-get_selected_from_choices(selected,choices)
      updatePickerInput(session,'hc_ord_factor',choices=choices,selected=selected)

    })

    observeEvent(input$hc_ord_factor,{
      vals$cur_hc_ord_factor<-input$hc_ord_factor
    })
    args_bmu<-reactiveVal()

    output$somcut_display<-renderUI({
      div(span("Display:", inline(
        radioButtons(
          ns("dot_label_clus"), NULL, choices = c("labels", "symbols"), inline = TRUE, width = "100px", selected = vals$dot_label_clus)
      )))
    })


    observeEvent(input$hcsom_newdata,{
      if(isTRUE(input$hcsom_newdata)){
        cols<-vals$newcolhabs[[input$pclus_points_palette]](100)[1:2]
        if(cols[1]==cols[2])
          updatePickerInput(session,"pclus_points_palette",selected="turbo")
      }

    })

    output$hc_save_tab4<-renderUI({
      req(hcplot4())
      req(isTRUE(input$hcsom_newdata))
      div(class = "save_changes",
          tipify_ui(shinyBS::bsButton(ns("savemapcode"), icon(verify_fa = FALSE, name = NULL, class = "fas fa-save"), style = "button_active", type = "action", value = FALSE),"Create Datalist with prediction results","right"), span(style = "font-size: 12px", icon(verify_fa = FALSE, name = NULL, class = "fas fa-hand-point-left"), "Create Datalist")
      )
    })

    output$down_hc_model<-downloadHandler(
      filename = function() {
        paste0("HC","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(phc(),file)
      })
    output$new_fac_hc<-renderPrint({
      attr(getdata_hc(),"factors")
    })
    output$hcsom_newdata_mess<-  renderUI({
      req(isTRUE(input$hcsom_newdata))
      span("select a gradient palette in 'Points' to differentiate between training and the new data", style="color: gray")

    })

    getgrad_col<-reactive({
      res<-lapply(vals$newcolhabs, function(x) x(2))
      res1<-unlist(lapply(res, function(x) x[1]==x[2]))
      grad<-names(res1[res1==F])
      pic<-which(vals$colors_img$val%in%grad)
      pic
    })
    getsolid_col<-reactive({
      res<-lapply(vals$newcolhabs, function(x) x(10))
      res1<-unlist(lapply(res, function(x) x[1]==x[2]))
      solid<-names(res1[res1==T])
      pic<-which(vals$colors_img$val%in%solid)
      pic
    })
    bag_mp<-reactive({
      name0<-paste0('new_HC_',input$customKdata)
      if(length(input$fixname)>0){
        if(isTRUE(input$fixname)){
          name0<-paste0(input$data_hc,'_HC')
        }
      }
      name1<-make.unique(c(names(vals$saved_data),name0))
      name1[length(name1)]


    })
    choices_hc<-reactive({
      req(input$data_hc)
      a<-if (length(   names(vals$saved_data) > 0)) {
        "data"
      } else {
        NULL
      }

      b<-   if(length(attr(vals$saved_data[[input$data_hc]],"som"))>0){"som codebook"}else{NULL}
      res<-c(a, b)
      res
    })

    getdata_hc<-reactive({
      req(input$data_hc)
      req(input$data_hc%in%names(vals$saved_data))
      data=vals$saved_data[[input$data_hc]]
      validate(need(length(data)>0,"no data found"))

      data
    })
    get_hcut_labels<- reactive({
      if(input$model_or_data=="data"){
        req(input$hcut_labels)
        if(input$hcut_labels=="rownames"){
          rownames(getdata_hc())
        } else{
          as.factor(attr(getdata_hc(),"factors")[rownames(getdata_hc()), input$hcut_labels])
        }
      } else{NULL}


    })

    output$smw_error2<-renderUI({
      render_message(vals$smw_message2)

    })
    output$smw_error<-renderUI({
      render_message(vals$smw_message)

    })

    hc_screeplot<-function(data,model_or_data="som codebook",model_name=1,disthc,screeplot_hc_k,whatmap=NULL,use_weights=F){
      cmd_log_type<-d_log_type<-p_log_type<-NULL
      cmd_log_message<-d_log_message<-p_log_message<-NULL


      if(model_or_data=="som codebook"){
        m<- attr(data,"som")[[model_name]]
        weights<-rep(1,length(whatmap))
        if(isTRUE(use_weights)){
          weights<-NULL
        }
        dist=get_somdist_weighted(m,weights=weights,whatmap)
        data_log<-capture_log1(cmdscale)(dist, k=30)
        data<-data_log[[1]]
        cmd_log_message<-sapply(data_log$logs,function(x) x$message)
        if(length(cmd_log_message)==0){
          cmd_log_message<-NULL
          cmd_log_message<-NULL
        }
        cmd_log_type<-sapply(data_log$logs,function(x) x$type)
      } else{
        dist_log<-capture_log1(vegan::vegdist)(data,disthc)
        dist<-dist_log[[1]]
        d_log_message<-sapply(dist_log$logs,function(x) x$message)
        if(length(d_log_message)==0){
          d_log_message<-NULL
          d_log_message<-NULL
        }
        d_log_type<-sapply(dist_log$logs,function(x) x$type)
      }
      if(!is.null(dist)){
        p_log<-capture_log1(factoextra::fviz_nbclust)(data, factoextra::hcut, method = "wss", k.max = screeplot_hc_k, diss=dist)




      } else{
        p_log<-list(result=NULL,logs=list(list(message="Error", type="error")))
      }
      x<-p_log$logs
      p<-p_log[[1]]

      if(!is.null(p)){
        p<-p+ theme_minimal() + ggtitle("the Elbow Method")
      }
      p_log_message<-sapply(p_log$logs,function(x) x$message)
      p_log_type<-sapply(p_log$logs,function(x) x$type)
      if(length(p_log_message)==0){
        p_log_message<-NULL
        p_log_type<-NULL
      }

      if(length(p)>0){
        p$data$clusters<-as.numeric(p$data$clusters)
        re<-p$data
        colnames(re)<-c("Clusters","WSS")
        attr(p,"result")<-re

      }
      if(is.null(p)){
        p<-FALSE
      }
      logs<-c(cmd_log_message,
              d_log_message,
              p_log_message)
      if(!is.null(logs)){
        attr(logs,"type")<-c(cmd_log_type,
                             d_log_type,
                             p_log_type)
      }

      attr(p,"logs")<-logs

      p
    }



    getsmw_plot<-reactive({
      tol=input$smw_hc_tol
      p<-   vals$scree_plot_hc0
      req(vals$screeplot_results)
      if(ncol(vals$screeplot_results)>2){
        smw<-vals$screeplot_results
        smw2<-get_screeplot_smw_sig(smw,tol)
        df<-get_ggdata_screesms(smw2,tol)
        p<-scree_smw_ggplot(df)
      }
      p

    })

    hc_newlevels<-reactive({
      req(input$data_hc)
      req(input$data_hc%in%names(vals$saved_data))
      req(input$hc_ord_datalist)
      req(input$hc_ord_factor)
      req(input$hc_ord_datalist%in%names(vals$saved_data))

      data_o<-getdata_hc()
      data<-vals$saved_data[[input$hc_ord_datalist]][rownames(data_o),,drop=F]
      hc<-get_hc()

      validate(need(sum(rownames(data_o)%in%rownames(data))==nrow(data_o),"The IDs of the sorted data chosen do not match those of the training data."))
      data<-data[rownames(data_o),,drop=F]
      req(input$hc_ord_factor%in%names(data))
      fac<-data[names(hc$somC),input$hc_ord_factor,drop=F]
      clusters<-hc$somC
      newlevels<-names(sort(tapply(fac[,1],as.numeric(as.character(clusters)),mean)))

      newlevels
    })
    # phc<-reactiveVal()
    observeEvent(input$run_bmu,ignoreInit = T,{
      #  phc(phc_root())
    })

    observeEvent(input$run_hc,ignoreInit = T,{
      # phc(phc_root())
    })






    hierarc_cluster<-reactive({

      req(input$model_or_data)
      req(input$method.hc0)
      req(length(input$hc_sort)>0)
      hc<-get_hc()
      vals$cutsom<-hc$som.hc
      vals$cutsom_samples<-hc$somC
      if(isFALSE(input$hc_sort)){
        vals$hc_newlevels<-NULL
        hc$som.hc<-factor(hc$som.hc)
        hc$somC<-factor(hc$somC)
        hc
      } else{
        req(input$hc_ord_datalist)
        req(input$hc_ord_factor)

        som.hc_names<-names(hc$som.hc)

        somC_names<-names(hc$somC)

        newlevels<-hc_newlevels()

        vals$hc_newlevels<-newlevels

        hc<-get_hc()

        hc$som.hc<-factor(hc$som.hc,levels=newlevels,labels=1:length(newlevels))

        hc$somC<-factor(hc$somC,levels=newlevels,labels=1:length(newlevels))

        hc$som.hc<-hc$som.hc[som.hc_names]

        hc$somC<-hc$somC[somC_names]

        vals$cutsom<-hc$som.hc

        vals$cutsom_samples<-hc$somC

        hc
      }


      hc

    })

    phc<-reactiveVal()
    observeEvent(input$run_bmu,ignoreInit = T,{
      phc(hierarc_cluster())
    })
    observeEvent(input$run_hc,ignoreInit = T,{
      phc(hierarc_cluster())
    })



    indicate_hc<-reactive({
      npic<-NULL
      indicate<-NULL
      if(isTRUE(input$varfacmap_action)){

        npic<- input$npic
        indicate<- input$vfm_type
      }
      iind=list(indicate=indicate,npic=npic)
      iind
    })




    bp_som<-reactive({

      iind=indicate_hc()
      m<-getmodel_hc()


      bp<-getbp_som2(m=m,indicate=iind$indicate,npic=iind$npic,hc=vals$cutsom)
      bp
    })

    observeEvent(input$create_dl_vfm,{
      m<-getmodel_hc()
      vars<-rownames(bp_som())
      data_o<-vals$saved_data[[names(m$data)[1]]]
      data_n<-data.frame(do.call(rbind,m$data))

      data_n<-data_n[,vars,drop=F]
      data_n<-data_migrate(data_o,data_n)
      npic=input$npic

      type<-switch(input$vfm_type,
                   "var"='Highest',
                   "cor"="Chull",
                   "cor_hc"="Cluster")
      bag<-paste0(input$som_model_name,"_vfm",type,npic,"vars")

      attr(data_n,"bag")<-bag
      vals$newdatalist<-data_n
      module_save_changes$ui(ns("som-vfm-create"), vals)
    })


    module_save_changes$server("som-vfm-create", vals)

    get_network<-reactive({
      backtype=NULL
      property=NULL
      m<-getmodel_hc()
      hc<-cur_som.hc.clusters()
      hexs<-get_neurons(m,background_type="hc",property=NULL,hc=hc)
      hexs
    })
    get_copoints<-reactive({
      m<-getmodel_hc()
      copoints<-getcopoints(m)
      copoints
    })
    points_to_map<-reactive({
      rescale_copoints(hexs=get_network(),copoints=get_copoints())
    })
    points_tomap2<-reactive({
      m2<-   predsupersom_hc()
      points_tomap2=rescale_copoints(hexs=get_network(),copoints=getcopoints(m2))
      points_tomap2
    })
    copoints_scaled<-reactive({
      points_tomap=points_to_map()
      data<-vals$saved_data[[input$data_hc]]
      factors<-attr(data,"factors")

      if(isTRUE(input$hcsom_newdata)){
        points_tomap2<-points_tomap2()
        points_tomap2$point<-"New data"
        points_tomap2$label<-rownames(points_tomap2)
        points_tomap$point<-"Training"
        dftemp<-rbind(points_tomap,points_tomap2)
        points_tomap<-dftemp
        attr(points_tomap,"namepoints")<-""
        return(points_tomap)
      }


      if(length(input$pclus_text_factor)>0){
        if(input$pclus_text_factor%in%colnames(factors)){
          text_factor= factors[rownames(data),input$pclus_text_factor, drop=F]
          points_tomap$label<-text_factor[rownames(points_tomap),]
        }
      }

      if(length(input$pclus_points_factor)>0){
        if(input$pclus_points_factor%in%colnames(factors)){
          points_factor= factors[rownames(data),input$pclus_points_factor, drop=F]
          points_tomap$point<-points_factor[rownames(points_tomap),]
          attr(points_tomap,"namepoints")<-input$pclus_points_factor
        }
      }

      points_tomap
    })

    getdata_for_hc<-reactive({
      req(input$data_hc)
      datalist<-vals$saved_data
      data<-vals$saved_data[[input$data_hc]]
      req(length(data)>0)
      res0<-unlist(
        lapply(datalist, function (x){
          all(rownames(data)%in%rownames(x))
        })
      )
      names(res0[res0==T])
    })
    choices_hc_names<-reactive({
      req(input$data_hc%in%names(vals$saved_data))
      a<-if (length(   names(vals$saved_data) > 0)) {
        "Numeric-Attribute"
      } else {
        NULL
      }

      b<-   if(length(attr(vals$saved_data[[input$data_hc]],"som"))>0){"SOM-codebook"}else{NULL}
      res<-c(a, b)
      res
    })

    getmodel_hc<-reactive({
      req(input$data_hc)
      req(input$som_model_name)
      req(input$model_or_data=="som codebook")
      data<-getdata_hc()
      m<-attr(data,"som")[[as.character(input$som_model_name)]]
      req(m)
      m
    })
    getmodel_hc0<-reactive({
      req(input$data_hc)
      req(input$som_model_name)
      data<-getdata_hc()
      m<-attr(data,"som")[[as.character(input$som_model_name)]]
      m
    })
    hcsom_active_layers<-reactive({
      layers<-getsom_layers()
      active_layers<-sapply(layers,function(x) {
        if(isTRUE(input[[paste0("hcsom_layer",x)]])){
          input[[paste0("hcsom_newdata_layer",x)]]
        } else{NULL}

      })
      unlist(active_layers)
    })
    hcsom_whatmap<-reactive({
      layers<-getsom_layers()
      sapply(layers,function(x) {isTRUE(input[[paste0("hcsom_layer",x)]])})
    })
    getsom_layers<-reactive({
      m<-getmodel_hc0()
      layers<-names(m$data)
      layers

    })
    predsupersom_hc<-reactive({
      layers<-getsom_layers()
      whatmap=layers[hcsom_whatmap()]

      m<-getmodel_hc0()
      if(length(m$data)==1){
        whatmap=NULL
      }

      newdatas<-vals$saved_data[hcsom_active_layers()]
      newdata_matrices<-lapply(newdatas,function(x) as.matrix(x))
      pic0<-names(which.min(sapply(newdata_matrices, nrow)))
      id_o<-rownames(newdata_matrices[[pic0]])
      newdata_matrices<-lapply(newdata_matrices,function(x){
        x[id_o,,drop=F]
      })
      if(length(m$data)>1){
        names(newdata_matrices)<-whatmap
      } else{
        newdata_matrices<-as.matrix(newdata_matrices[[1]])
      }
      pred<-predict(m,newdata_matrices,unit.predictions=m$codes,whatmap=whatmap)
      m2<-m
      m2$data<-pred$predictions
      m2$codes<-pred$unit.predictions
      bmus<-pred$unit.classif
      names(bmus)<-rownames(pred$predictions[[1]])
      m2$unit.classif<-bmus
      m2$whatmap<-pred$whatmap
      m<-m2
      m
    })
    bag_hc<-reactive({
      datalist<-sommodel<-K<-""

      name0<-paste0('HC',input$customKdata)
      if(length(input$fixname)>0){
        if(isTRUE(input$fixname)){
          datalist<-paste0(input$data_hc,"_")
        }
      }
      if(length(input$fixmodel)>0){
        if(isTRUE(input$fixmodel)){
          sommodel<-paste0(input$som_model_name,"_")
        }
      }
      name0<-paste0(datalist,sommodel,name0)
      data<-attr(vals$saved_data[[input$data_hc]],"factors")
      name1<-make.unique(c(colnames(data),name0), sep="_")
      name1[ncol(data)+1]


    })
    labhc<-reactive({
      req(input$labhc)
      as.character(attr(getdata_hc(),"factors")[rownames(getdata_hc()),as.character(input$labhc)])
    })

    gosave<-reactiveValues(df=0,  modal=F)
    hand_save_modal<-reactive({

      tags$div(id="savemodal",
               modalDialog(
                 shinycssloaders::withSpinner(type=8,color="SeaGreen",uiOutput(ns("databank_storage"))),
                 title=span(icon(verify_fa = FALSE,name=NULL,class="fas fa-save"),'Save'),
                 footer=column(12,class="needed",
                               fluidRow(shinyBS::bsButton(ns("cancel_save"),"Cancel"),
                                        inline(actionButton(ns("data_confirm"),strong("Confirm")))
                               )),
                 size="l",
                 easyClose = T
               )
      )
    })
    output$databank_storage<-renderUI({
      column(12,
             fluidRow(
               column(12,p(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")), p(vals$hand_save2,style="color: gray")),
               column(12,vals$hand_save3),
               column(12,style='margin-top: 10px; margin-left: 10px',
                      splitLayout(cellWidths = c("30%","70%"),
                                  radioButtons(ns("hand_save"),NULL,
                                               choiceNames= list(div(style="height: 50px","create"),
                                                                 div(style="height: 50px","overwrite")),
                                               choiceValues=list('create',"over")),
                                  column(12,div(style="height: 50px",
                                                shinycssloaders::withSpinner(type=8,color="SeaGreen",uiOutput(ns("data_create")))),
                                         div(style="height: 50px",
                                             shinycssloaders::withSpinner(type=8,color="SeaGreen",uiOutput(ns("data_over")))))

                      )
               )

             )
      )
    })
    data_overwritte<-reactiveValues(df=F)
    data_store<-reactiveValues(df=F)
    output$data_create<-renderUI({
      req(length(vals$hand_save)>0)
      req(input$hand_save=="create")
      data_store$df<-F
      res<-switch (vals$hand_save,
                   "create_codebook"=textInput(ns("codebook_newname"), NULL,paste0(input$data_hc,"Codebook")),
                   "Save Clusters"= textInput(ns("hc_newname"), NULL,bag_hc()),
                   "Create Datalist with new mapping"= textInput(ns("mc_newname"), NULL,bag_mp()),

      )
      data_store$df<-T
      res
    })
    output$data_over<-renderUI({
      data_overwritte$df<-F
      req(input$hand_save=="over")
      res<-switch (vals$hand_save,
                   'Create Datalist with new mapping' = selectInput(ns("mc_over"), NULL,choices=c(names(vals$saved_data)),selectize=T),
                   'create_codebook' = selectInput(ns("codebook_over"), NULL,choices=c(names(vals$saved_data)),selected=input$data_upload),
                   'Save Clusters' = selectInput(ns("hc_over"), NULL,choices=c(colnames(attr(getdata_hc(),"factors"))),selectize=T))
      data_overwritte$df<-T
      res
    })
    saveclusters<-reactive({
      vals$baghc0<-vals$baghc0+1
      hc<-phc()
      temp<-hc$somC
      if(input$hand_save=="create"){
        attr(vals$saved_data[[input$data_hc]],"factors")[names(temp),input$hc_newname]<-temp
      } else{
        data_o<-vals$saved_data[[input$data_hc]]
        facold<-attr(data_o,"factors")[rownames(data_o),]
        facold[,input$hc_over]<-temp[rownames(data_o)]
        attr(vals$saved_data[[input$data_hc]],"factors")<-facold
      }

    })
    mapcode<-reactive({
      kohonen::map(getmodel_hc(),as.matrix(vals$saved_data[[input$data_mapcode]]))
    })
    get_datalist_newmaps<-reactive({

      layers<-getsom_layers()
      news<-lapply(layers,function(x){
        if( isTRUE(input[[paste0("hcsom_layer",x)]])){ input[[paste0("hcsom_newdata_layer",x)]]} else{
          NULL
        }

      })
      unlist(news)


    })
    savemapcode<-reactive({
      news<-get_datalist_newmaps()
      numeric<-do.call(cbind,vals$saved_data[news])
      colnames(numeric)<-make.unique(unlist(sapply(vals$saved_data[news],colnames)))

      coords<-lapply(vals$saved_data[news],function(x) attr(x,"coords"))
      ids_coords<-unlist(lapply(coords,rownames))
      newcoords<-do.call(rbind,coords)
      newcoords$id<-ids_coords
      newcoords<-newcoords[!duplicated(newcoords$id),1:2]
      rownames(newcoords)<-unique(ids_coords)

      args<-argsplot_somplot()
      req(args)
      newdata<-args$points_tomap[  args$points_tomap$point=="New data",]
      new1<-newdata["hc"]
      rownames(new1)<-newdata$label
      new1<-new1[rownames(numeric),,drop=F]
      colnames(new1)<-paste0("HC",input$customKdata)

      numeric<-data_migrate(vals$saved_data[[input$data_hc]],numeric)

      attr(numeric,"coords")<-newcoords[rownames(numeric),]
      attr(numeric,"factors")<-new1[rownames(numeric),,drop=F]
      if(input$hand_save=="create"){
        vals$saved_data[[input$mc_newname]]<-numeric
      } else{
        vals$saved_data[[input$mc_over]]<-numeric
      }

    })
    status_changes<-reactiveValues(df=F)

    savecodebook<-reactive({
      req(input$hand_save)
      data<-getdata_hc()
      m<-getmodel_hc()
      codes<-data.frame(do.call(cbind,m$codes))

      factors<-data.frame(som.hc.clusters())

      rownames(factors)<-rownames(codes)<-paste0("unit_",1:nrow(codes))
      colnames(factors)<-paste0("Class",input$customKdata)

      attr(codes,"factors")<-factors
      temp<-codes
      temp<-data_migrate(data,temp,"new")
      attr(temp,"data.factor")<-NULL
      attr(temp,"factors")<-factors
      attr(temp,"datalist")<-NULL
      attr(temp,"filename")<-NULL
      attr(temp,"coords")<-NULL
      attr(temp,"base_shape")<-NULL
      attr(temp,"layer_shape")<-NULL
      attr(temp,"transf")<-NULL
      attr(temp,"nobs_ori")<-NULL
      if(input$hand_save=="create"){
        req(input$codebook_newname)
        vals$saved_data[[input$codebook_newname]]<-temp
      } else{
        req(input$codebook_over)
        vals$saved_data[[input$codebook_over]]<-temp
      }
      vals$new_facts<-NULL
      status_changes$df<-c(T,T)

    })
    save_switch<-reactive({
      switch(vals$hand_save,
             "Create Datalist with new mapping"= {savemapcode()},
             "create_codebook"=savecodebook(),
             "Save Clusters"= {saveclusters()}
      )

    })

    output$plot_hc_screeplot <-renderUI({
      vals$scree_plot_hc<-getsmw_plot()
      renderPlot({print(vals$scree_plot_hc)})

    })
    hcplot5<-reactive({
      k.max<-input$mapcode_loop_K
      req(input$som_model_name)
      result<-attr(attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]],"codebook_screeplot")
      req(input$show_mapcode_errors%in%result$variable)
      df<-result[    result$variable%in%input$show_mapcode_errors,]
      p<-ggplot(df)+geom_line(aes(k,value))+facet_wrap(~variable,scales="free")+xlab("Number of Clusters")
      p
    })
    output$plot5<-renderUI({
      renderPlot(hcplot5())
    })


    observeEvent(args_hc1(),{
      vals$hc_messages<-NULL
      vals$hc_tab1_plot<-NULL
      shinyjs::addClass("hcut_btn1","save_changes")
    })









    args_hc2<-reactive({
      list(
        getdata_hc(),
        input$hcdata_palette,
        input$customKdata,
        input$hcut_labels,
        lwd=input$hcut_lwd,
        base_size=input$hcut_cex,
        main=input$hcut_main,
        xlab=input$hcut_xlab,
        ylab=input$hcut_ylab,
        theme=input$hcut_theme,
        offset_labels=input$hcut_offset,
        angle=input$hcut_xlab_angle,
        log=input$hcut_log,
        hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc,model_name=input$som_model_name,target=input$model_or_data

      )
    })

    hc_model<-reactive({
      args<-args_hc1()
      somC<-do.call(imesc_hclutering,args)
      somC
    })


    observeEvent(input$run_hc1,ignoreInit = T,{
      shinyjs::removeClass("hcut_btn1","save_changes")
      req(input$model_or_data)
      req(input$method.hc0)
      somC<-hc_model()
      vals$hc_messages<-attr(somC,"logs")

      hc<-somC$hc.object
      args<-list(hc,main = input$hc_title)
      if(input$model_or_data!="som codebook"){
        args$labels = as.character(labhc())
      }

      if(!is.null(args[[1]])){

        if(inherits(args[[1]],c("agnes","diana"))){
          args$ask<-F
          args$which.plots<-2
        }
        output$hcdata_plot<-renderUI({
          renderPlot({
            do.call(plot,args)
            vals$hc_tab1_plot<-recordPlot()
          })
        })


      }


    })

    observeEvent(input$data_hc,{
      vals$cur_data_hc<-input$data_hc
    })


    observeEvent(vals$saved_data,{
      selected=vals$cur_data_hc
      choices=names(vals$saved_data)
      selected=get_selected_from_choices(selected,choices)

      updatePickerInput(session,"data_hc",choices=choices, selected=selected)
    })

    observe(shinyjs::toggle("hcut_labels",condition=input$model_or_data=="data"))
    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'hcdata_palette',
                        choices = vals$colors_img$val,
                        choicesOpt=list(content=vals$colors_img$img),
                        selected=vals$hcdata_palette
      )
    })
    observeEvent(vals$newcolhabs,{
      choices =  vals$colors_img$val[getgrad_col()]
      choicesOpt = list(content =  vals$colors_img$img[getgrad_col()] )
      updatePickerInput(session,'bg_palette',
                        choices=choices,
                        choicesOpt=choicesOpt
      )
      choices =  vals$colors_img$val[getsolid_col()]
      choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] )
      updatePickerInput(session,'pclus_border',
                        choices=choices,
                        choicesOpt=choicesOpt,
                        selected="white"
      )
    })


    observeEvent(names(attr(getdata_hc(), "som")),{
      choices= names(attr(getdata_hc(), "som"))
      selected=vals$cur_som_model_name

      selected<-get_selected_from_choices(selected,choices)
      if(is.null(selected)){
        selected<-choices[1]
      }

      updatePickerInput(session,'som_model_name',selected=selected,choices=choices)
    })



    observeEvent(ignoreInit = T,input$download_plot4,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=hcplot4()
      datalist_name=attr(getdata_hc(),'datalist')
      name_c=paste0("Codebook",input$customKdata,"groups")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="som codebook - HC", name_c=name_c,datalist_name=datalist_name)
    })
    observeEvent(input$model_or_data,{
      value=if(input$model_or_data=="som codebook"){
        paste0(input$data_hc,"(SOM codebook)")
      } else{ input$data_hc}

      updateTextInput(session,'hc_title',value=value)
    })
    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'pclus_points_palette',
                        choices = vals$colors_img$val,
                        choicesOpt=list(content=vals$colors_img$img),
                        selected='black'
      )
      updatePickerInput(session,'pclus_text_palette',
                        choices =  vals$colors_img$val[getsolid_col()],
                        selected="black",
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] )
      )


    })
    observeEvent(input$pclus_points_palette,{
      cols<-vals$newcolhabs[[input$pclus_points_palette]](8)
      shinyjs::toggle('pclus_points_factor',condition=cols[1]!=cols[2])
      shinyjs::toggle('color_factor',condition=cols[1]==cols[2])
      if(cols[1]==cols[2]){
        updateTextInput(session,'pclus_points_legend_text',value="Observations")
      } else{
        updateTextInput(session,'pclus_points_legend_text',value=input$pclus_points_factor)
      }


    })
    observeEvent(df_symbol$val,{
      updatePickerInput(session,'pclus_symbol',choices = df_symbol$val,
                        choicesOpt = list(content = df_symbol$img))
    })
    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'p.clus.col.text',
                        choices=vals$colors_img$val[getsolid_col()],
                        choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),selected="black")
      updatePickerInput(session,'var_bg',choices=vals$colors_img$val[getsolid_col()],choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),selected="white")
    })
    observeEvent(input$run_screeplot_hc,ignoreInit = T,{
      shinyjs::removeClass('run_screeplot_hc_btn',"save_changes")
    })
    observeEvent(input$run_smw_hc,{
      shinyjs::removeClass("run_smw_hc_btn","save_changes")
      vals$run_smw_hc<-T
    })
    observeEvent(input$show_smw_hc,{
      vals$show_smw_hc<-input$show_smw_hc
    })
    observeEvent(input$smw_hc_rand,{
      vals$smw_hc_rand<-input$smw_hc_rand
    })
    observeEvent(input$smw_hc_tol,{
      vals$smw_hc_tol<-input$smw_hc_tol
    })
    observeEvent(ignoreInit = T,input$model_or_data,{
      vals$screeplot_results<-NULL
    })
    observeEvent(ignoreInit = T,input$run_screeplot_hc, {

      get_hc_screeplot()

    })
    observeEvent(ignoreInit = T,input$customKdata,
                 vals$saved_kcustom<-input$customKdata)
    observeEvent(ignoreInit = T,input$hcdata_palette,{vals$hcdata_palette<-input$hcdata_palette})

    observe({
      shinyjs::toggle('run_smw_hc', condition =isTRUE(input$show_smw_hc) )
    })
    observeEvent(input$run_smw_hc,{
      vals$screeplot_results<-vals$screeplot_results0
      vals$scree_plot_hc<- vals$scree_plot_hc0



      req(input$smw_hc_rand>2)
      result<-vals$screeplot_results
      n.rand=input$smw_hc_rand
      ws<-as.numeric(unlist(strsplit(input$smw_hc_w,",")))
      #session=MockShinySession$new()
      smwlog<-capture_log1(smwhc)(result, n.rand,ws)

      smw<-smwlog[[1]]
      logs<-sapply(smwlog$logs,function(x) x$message)

      attr(logs,"type")<-sapply(smwlog$logs,function(x) x$type)
      if(length(logs)==0){
        logs<-NULL
      }
      vals$smw_message2<-logs
      req(smw)
      vals$screeplot_results<-smw
      #savereac()

    })



    observeEvent(ignoreInit = T,input$round_error,{
      vals$round_error<-input$round_error
    })
    observeEvent(ignoreInit = T,input$bg_palette,{
      vals$pclussomplot_bg<-input$bg_palette
    })
    observeEvent(ignoreInit = T,input$pclus_text_palette,{
      vals$pclus_text_palette<-input$pclus_text_palette
    })
    observeEvent(ignoreInit = T,input$pclus_text_factor,{
      vals$pclus_text_factor<-input$pclus_text_factor
    })
    observeEvent(ignoreInit = T,input$pclus_border,{
      vals$pclus_border<-input$pclus_border
    })
    observeEvent(ignoreInit = T,input$vfm_type,{
      vals$vfm_type<-input$vfm_type
    })
    observeEvent(ignoreInit = T,input$npic,{
      vals$npic<-input$npic
    })
    observeEvent(ignoreInit = T,input$pclus.cex.var,{
      vals$pclus.cex.var<-input$pclus.cex.var
    })
    observeEvent(ignoreInit = T,input$p.clus.col.text,{
      vals$p.clus.col.text<-input$p.clus.col.text
    })
    observeEvent(ignoreInit = T,input$var_bg,{
      vals$var_bg<-input$var_bg
    })
    observeEvent(ignoreInit = T,input$var_bg_transp,{
      vals$var_bg_transp.alpha<-input$var_bg_transp
    })
    observeEvent(ignoreInit = T,input$pclus_points_palette,{
      vals$pclus_points_palette<-input$pclus_points_palette
    })
    observeEvent(ignoreInit = T,input$insertx_pclus,{
      vals$insertx_pclus<-input$insertx_pclus
    })
    observeEvent(ignoreInit = T,input$inserty_pclus,{
      vals$inserty_pclus<-input$inserty_pclus
    })
    observeEvent(ignoreInit = T,input$ncol_pclus,{
      vals$ncol_pclus<-input$ncol_pclus
    })
    observeEvent(ignoreInit = T,input$bgleg_pclus,{
      vals$bgleg_pclus<-input$bgleg_pclus
    })
    observeEvent(ignoreInit = T,input$pclus_symbol,{
      vals$pclus_symbol<-input$pclus_symbol
    })
    observeEvent(ignoreInit = T,input$dot_label_clus,{
      vals$dot_label_clus<-input$dot_label_clus
    })
    observeEvent(ignoreInit = T,input$varfacmap_action,{
      vals$pclus_varfacmap_action<-input$varfacmap_action
    })
    observeEvent(ignoreInit = T,input$pclus_points_size,{
      vals$pclus_points_size<-input$pclus_points_size
    })
    observeEvent(ignoreInit = T,input$pcodes_bgalpha,{
      vals$pcodes_bgalpha<-input$pcodes_bgalpha
    })
    observeEvent(ignoreInit = T,input$down_results_screeplot,{
      vals$hand_down<-switch (input$model_or_data,
                              "som codebook" = "screeplot_WSS som",
                              "data"="screeplot_WSS data"
      )
      vals$hand_down<-"screeplot"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$hc_results,{
      vals$hc_results<-input$hc_results
    })


    observeEvent(ignoreInit = T,input$data_hc,{
      vals$show_mapcode_errors<-c("Within Sum of Squares","Dendrogram Height")
    })
    observeEvent(ignoreInit = T,input$help_hc_fun, {
      modal_help("hclust", intro=div(
        "iMESC implements ", tags$code("Hierarchical Clustering"), "analysis using the ", tags$code("hcut"), " function, which is part of the  ", tags$code("factoextra"), "package. The parameters that can be customized in iMESc are ",tags$code("hc_func"),"(clustering function),",  tags$code("k"), " (number of groups), ", tags$code("hc_method"), ", and ", tags$code("hc_metric"), " (distance measure for clustering numeric attributes). When clustering som codebook, iMESc uses the same distance metric used to train the SOM. The remaining parameters of the ", tags$code("hclust()"), " function are set to their default values. For more information regarding ",tags$code("hc_func"),"argument, refer to their documentation: ",
        actionLink("hclust_help", "hclust"),
        ",",  actionLink("hclust_diana", "Divisive Analysis Clustering(diana)"),
        ", and", actionLink("hclust_agnes", "Agglomerative Nesting (agnes)"),
        "."))
    })
    observeEvent(ignoreInit = T,input$diana_help,{
      modal_help("diana")
    })
    observeEvent(ignoreInit = T,input$agnes_help,{
      modal_help("agnes")
    })
    observeEvent(ignoreInit = T,input$hclust_help,{
      modal_help("hclust")
    })
    observeEvent(ignoreInit = T,input$method.hc0,{
      vals$method.hc0<-input$method.hc0
    })
    observeEvent(ignoreInit = T,input$hc_fun,{
      vals$hc_fun<-input$hc_fun
    })
    observeEvent(ignoreInit = T,input$model_or_data,{
      vals$cur_model_or_data<-input$model_or_data
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_addpoints,{
      vals$pclus_newdata_addpoints<-input$pclus_newdata_addpoints
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_points_palette,{
      vals$pclus_newdata_points_palette<-input$pclus_newdata_points_palette
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_points_factor,{
      vals$pclus_newdata_points_factor<-input$pclus_newdata_points_factor
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_symbol,{
      vals$pclus_newdata_symbol<-input$pclus_newdata_symbol
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_points_size,{
      vals$pclus_newdata_points_size<-input$pclus_newdata_points_size
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_addtext,{
      vals$pclus_newdata_addtext<-input$pclus_newdata_addtext
    })
    observeEvent(ignoreInit = T,input$data_hc,{
      vals$cur_data=input$data_hc
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_text_palette,{
      vals$pclus_newdata_text_palette<-input$pclus_newdata_text_palette
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_text_factor,{
      vals$pclus_newdata_text_factor<-input$pclus_newdata_text_factor
    })
    observeEvent(ignoreInit = T,input$pclus_newdata_text_size,{
      vals$pclus_newdata_text_size<-input$pclus_newdata_text_size
    })
    observeEvent(ignoreInit = T,input$hcsom_whatmap,{
      vals$hcsom_whatmap<-input$hcsom_whatmap
    })
    observeEvent(ignoreInit = T,input$hcsom_newdata,{
      vals$hcsom_newdata<-input$hcsom_newdata
    })
    observeEvent(ignoreInit = T,input$som_model_name,{
      vals$cur_som_model_name<-input$som_model_name
    })
    observeEvent(ignoreInit = T,input$fixname,{
      vals$fixname<-input$fixname
    })
    observeEvent(ignoreInit = T,input$fixmodel,{
      vals$fixmodel<-input$fixmodel
    })
    observeEvent(ignoreInit = T,input$labhc,{
      vals$labhc<-input$labhc
    })
    observeEvent(input$hcut_labels,{
      vals$hcut_labels<-input$hcut_labels
    })
    observeEvent(ignoreInit = T,input$savemapcode,{
      if(input$savemapcode %% 2) {
        vals$hand_save<-"Create Datalist with new mapping"
        vals$hand_save3<-NULL
        showModal(
          hand_save_modal()
        )
      }
    })
    observeEvent(ignoreInit = T,input$tools_savehc,{
      if(is.null(vals$fixname)){
        vals$fixname<-F
      }
      if(input$tools_savehc %% 2) {
        vals$hand_save<-"Save Clusters"
        vals$hand_save2<-p(
          div( style="color: gray",
               "Target:",em(input$data_hc),"->",em("Factor-Attribute")
          )
        )

        vals$hand_save3<-div(
          strong("Include name:"),
          inline(checkboxInput(ns("fixname"),"Datalist",vals$fixname, width="80px")),
          inline(checkboxInput(ns("fixmodel"),"Model",vals$fixmodel, width="80px")),
        )
        showModal(
          hand_save_modal()
        )
      }
    })
    observeEvent(ignoreInit = T,input$create_codebook,{
      if(input$create_codebook %% 2) {
        vals$hand_save<-"create_codebook"
        vals$hand_save2<-"Create Datalist with the Codebook and HC class"
        vals$hand_save3<-NULL
        showModal(
          hand_save_modal()
        )
      }
    })
    observeEvent(ignoreInit = T,input$data_confirm,{
      vals$cur_hc_plot<-vals$hc_tab3_plot
      save_switch()
      removeModal()
    })
    observeEvent(ignoreInit = T,input$cancel_save,{
      removeModal()
    })
    observeEvent(input$download_plot2,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=getsmw_plot()

      datalist_name=attr(getdata_hc(),'datalist')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Scree plot", name_c="screeplot",datalist_name=datalist_name)
    })
    observeEvent(input$download_plot5,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      datalist_name=attr(getdata_hc(),'datalist')
      generic=hcplot5()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="SOM- Scree plot", name_c="screeplot",datalist_name=datalist_name)
    })
    observeEvent(ignoreInit = T,input$download_plot1,{
      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=vals$hc_tab1_plot
      datalist_name=attr(getdata_hc(),'datalist')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Dendrogram", name_c="dendrogram",datalist_name=datalist_name)
    })
    observe({
      shinyjs::toggle("Kcustom",condition=input$tabs%in%c("tab3","tab4"))
    })
    observe({
      shinyjs::toggle('labhc',condition=input$model_or_data=='data')
    })
    observe({
      req(isTRUE(input$show_smw_hc))
      req(length(input$smw_hc_w)>0)
      ws<-as.numeric(unlist(strsplit(input$smw_hc_w,",")))
      max<-input$screeplot_hc_k
      shinyjs::toggle("run_smw_hc",condition=!any(ws>max)&!any(ws%%2 == 1))
    })
    observe({
      shinyjs::toggleClass('run_smw_hc_btn',class='save_changes',condition=!isTRUE(vals$run_smw_hc))
    })
    observe({
      shinyjs::toggle("hc_smw_control",condition=isTRUE(input$show_smw_hc))
    })
    observe({
      shinyjs::toggle("pclus_points_inputs",condition=isTRUE(input$pclus_addpoints))
    })
    observe({
      shinyjs::toggle('disthc_id',condition=input$model_or_data=="data")

    })
    observe({
      shinyjs::toggle('som_model_name',condition=input$model_or_data=="som codebook")




    })

    observe({
      m<-getmodel_hc()
      shinyjs::toggle('som_whatmap',condition=input$model_or_data=="som codebook"&length(m$codes)>1)


    })


    observe({
      shinyjs::toggle('show_hcsom_fine',condition=!is.null(vals$som_whatmap)&input$model_or_data=="som codebook")
    })




    observe({
      if(is.null(vals$cur_whatmap)){
        m<-getmodel_hc()
        choices=names(m$codes)
        vals$cur_whatmap<-choices
      }
    })


    {

      observeEvent(getmodel_hc(),{
        m<-getmodel_hc()
        choices=names(m$codes)

        if(length(choices)==1){
          choices=1
        }
        selected<-vals$som_whatmap
        if(is.null(selected)){
          selected=choices
        }
        #updateCheckboxInput(session,"show_hcsom_fine",value=F)
        shinyWidgets::updateVirtualSelect('som_whatmap',choices=choices,selected=choices)
      })




      observe({
        m<-getmodel_hc()
        choices=names(m$codes)

        if(length(choices)==1){
          vals$som_whatmap<-NULL
        } else{
          vals$som_whatmap<-input$som_whatmap
        }

      })

      cutsom.reactive<-get_hc<-reactive({

        req(input$model_or_data)
        req(input$method.hc0)

        args<-list(data=getdata_hc(), k= input$customKdata,hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc,model_name=as.character(input$som_model_name),target=input$model_or_data,whatmap=vals$som_whatmap,use_weights=F)



        somC<-do.call(imesc_hclutering,args)
        vals$hc_messages<-attr(somC,"logs")

        somC

      })
      get_hc_screeplot<-reactive({

        args<-list(
          data=getdata_hc(),
          model_or_data=input$model_or_data,
          model_name=input$som_model_name,
          disthc=input$disthc,
          screeplot_hc_k=input$screeplot_hc_k,
          use_weights=F,
          whatmap=vals$som_whatmap
        )

        re<-do.call(hc_screeplot,args)

        vals$smw_message<-attr(re,"logs")
        req(!isFALSE(re[1]))


        result<-attr(re,"result")
        vals$screeplot_results0<-vals$screeplot_results<-result
        vals$scree_plot_hc<- vals$scree_plot_hc0<-re
      })

      args_hc1<-reactive({
        list(data=getdata_hc(), k= 1,hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc,model_name=as.character(input$som_model_name),target=input$model_or_data,whatmap=vals$som_whatmap,use_weights=F)
      })

      observeEvent(ignoreInit = T,input$mapcode_loop_go,{


        somC<-hc_model()

        vals$mapcode_loop_res<-NULL

        m<-getmodel_hc()

        k.max<-input$mapcode_loop_K
        hc_fun=input$hc_fun;hc_method=input$method.hc0





        result<-screeplot_som(m,k.max,hc_fun,hc_method,whatmap=vals$som_whatmap,use_weights=F)



        dend_hei<-somC$hc.object$height
        result$dh<-rev(dend_hei)[1:k.max]
        colnames(result)<-c("k","Within Sum of Squares","Dendrogram Height")

        result<-reshape2::melt(result,"k")

        attr(result,"class_result")<-"som screeplot"
        attr(attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]],"codebook_screeplot")<-result



      })


      output$som_layers<-renderUI({
        m<-getmodel_hc()
        req(length(m$codes)>1)
        req(input$model_or_data=="som codebook")
        div(style="display: flex; justify-content: flex-start;;align-items: flex-end",
            div(strong("SOM layers:",style="white-space: nowrap;")),
            div(style="white-space: normal;max-width: 350px; padding-left: 2px",
                emgreen(
                  paste(vals$som_whatmap, collapse="; ")
                ))
        )
      })



    }







    observe({
      shinyjs::toggle("hc_side4",condition = input$model_or_data == "som codebook")
    })
    observe({
      shinyjs::toggle("pclus_addtext_out", condition = isTRUE(input$pclus_addtext))
    })
    observe({
      shinyjs::toggle("varfac_out",condition = isTRUE(input$varfacmap_action))
    })

    observeEvent(input$varfacmap, {
      showModal(modalDialog(
        uiOutput(ns("textvarfacmap")),
        title = h4(strong("Variable factor map")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      ))
    })
    output$textvarfacmap<-renderUI({

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

        div(
          column(12,
                 h4("Variable factor map"),
                 p("The chart is very similar to the variable factor map obtained from the principal component analysis (PCA). It calculates the weighted correlation for each variable using the coordinates (x, y) of the neurons and their weights (number of instances). The codebooks vectors of the cells correspond to an estimation of the conditional averages, calculating their variance for each variable is equivalent to estimating the between-node variance of the variable, and hence their relevance."),
                 p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("Chull correlations")," returns",code("npic")," variables with the highest correlation considering the convex hull, while also ensuring that the points are ordered by their proximity to codebook center")
          )

        )
      )

    })


    observe({
      shinyjs::toggle("hc_tab4_out",condition=input$model_or_data == "som codebook")
    })
    observe({
      req(is.null(vals$hcsom_newdata))
      vals$hcsom_newdata<-F
    })
    observe({
      layers<-getsom_layers()
      m<-getmodel_hc0()
      lapply(layers,function(x){
        output[[paste0("hcsom_piclayer",x)]]<-renderUI({
          if( isTRUE(input[[paste0("hcsom_layer",x)]])){
            choices_temp<-names(which(sapply(vals$saved_data,function(xx){
              identical(sort(colnames(xx)),
                        sort(colnames(m$data[[x]])))
            })))
            div(class="label_none",style="max-width: 200px",
                pickerInput_fromtop(ns(paste0("hcsom_newdata_layer",x)), "", choices_temp))
          }
        })
      })
    })

    observe({
      req(input$model_or_data)
      if(input$model_or_data=="som codebook"){
        m<- getmodel_hc()
        data<-m$codes[[1]]
      } else{
        data<-  getdata_hc()
      }

      updateNumericInput(session,"screeplot_hc_k", value = round(nrow(data)/2))

    })



    output$labhc_out<-renderUI({
      choices = c(colnames(attr(getdata_hc(),"factors")))
      pickerInput_fromtop(
        ns("labhc"),
        "Labels",
        choices=choices,selected=vals$labhc)
    })


    observeEvent(getdata_hc(),{
      choiceValues<-choices_hc()
      choiceNames<-choices_hc_names()

      selected<-get_selected_from_choices(vals$cur_model_or_data,choiceValues)
      updateRadioButtons(session,"model_or_data",choiceNames =choiceNames,choiceValues =choiceValues,selected=selected )
    })
    observeEvent(getdata_hc(),{
      data<-getdata_hc()
      choices<-c(colnames(attr(data,"factors")))
      selected=vals$hcut_labels
      choices = c("rownames",choices)
      updatePickerInput(session,'hcut_labels',choices=choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })

    observeEvent(list(getdata_hc(),
                      input$hcsom_newdata),{

                        data<-getdata_hc()
                        choices<-colnames(attr(data,"factors"))
                        updatePickerInput(session,'pclus_text_factor',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))
                        options<-NULL
                        if(isTRUE(input$hcsom_newdata)){
                          choices<-c("Training/New data","Training")
                          options=shinyWidgets::pickerOptions(liveSearch=T)
                        }

                        updatePickerInput(session,'pclus_points_factor',choices=choices,options=options)
                      })
    observeEvent(list(input$screeplot_hc_k,getmodel_hc()),ignoreInit = T,{
      req(input$run_screeplot_hc)
      shinyjs::addCssClass('run_screeplot_hc_btn',"save_changes")
      vals$screeplot_results<-NULL
    })

    observeEvent(list(
      input$smw_hc_w,
      input$smw_hc_rand
    ),ignoreInit = T,{
      vals$run_smw_hc<-F

    })
    observe(shinyjs::toggle("run_smw_hc_btn",condition = vals$screeplot_results0$WSS))



    observeEvent(list(choices_hc(),vals$cur_model_or_data),{
      req(choices_hc())
      if(!any(choices_hc()==vals$cur_model_or_data))
        vals$cur_model_or_data<-NULL
    })
    observe({
      req(vals$update_state)
      update_state<-vals$update_state
      ids<-names(update_state)
      update_on<-grepl(id,ids)
      names(update_on)<-ids
      to_loop<-names(which(update_on))
      withProgress(min=1,max=length(to_loop),message="Restoring",{
        for(i in to_loop) {
          idi<-gsub(paste0(id,"-"),"",i)
          incProgress(1)
          restored<-restoreInputs2(session, idi, update_state[[i]])
          if(isTRUE(restored)){
            vals$update_state[[i]]<-NULL
          }

        }
      })

    })

  })
}
