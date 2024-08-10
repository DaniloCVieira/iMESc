#' @import kohonen leaflet sf  ggplot2
#' @import shiny viridis colorRamps wesanderson shinyTree sortable
#' @import randomForestExplainer
#' @importFrom readxl excel_sheets read_excel
#' @importFrom ggforce geom_arc_bar
#' @importFrom gplots heatmap.2
#' @importFrom Metrics mae mape mse rmse
#' @importFrom vegan ordiplot rda scores decostand vegdist metaMDS specnumber diversity rrarefy ordiArrowTextXY estimateR fisher.alpha
#' @importFrom mice mice complete
#' @importFrom colorspace darken
#' @importFrom tibble rownames_to_column
#' @importFrom foreach `%dopar%` `%:%` foreach
#' @importFrom shinycssloaders withSpinner
#' @importFrom doSNOW	registerDoSNOW
#' @importFrom parallel	detectCores makeCluster stopCluster
#' @importFrom data.table fread
#' @importFrom beepr beep
#' @importFrom igraph delete_vertices graph_from_data_frame
#' @importFrom ggraph ggraph geom_edge_link geom_edge_diagonal geom_node_point geom_node_text geom_node_label
#' @importFrom ggpubr ggtexttable ttheme colnames_style tbody_style thead_add_border tab_add_vline tab_add_hline table_cell_font table_cell_bg tab_add_title ggscatter tab_add_footnote
#' @importFrom ade4 dudi.pca niche niche.param
#' @importFrom ggrepel geom_text_repel
#' @importFrom factoextra hcut fviz_nbclust
#' @importFrom caret BoxCoxTrans createDataPartition confusionMatrix confusionMatrix.train createFolds createResample postResample varImp filterVarImp multiClassSummary trainControl plot.train nzv findCorrelation preProcess getModelInfo

#' @importFrom mice mice complete
#' @importFrom tibble rownames_to_column
#' @importFrom foreach `%dopar%` `%:%` foreach
#' @importFrom shinycssloaders withSpinner
#' @importFrom doSNOW	registerDoSNOW
#' @importFrom parallel	detectCores makeCluster stopCluster
#' @importFrom data.table fread
#' @importFrom beepr beep
#' @description A shiny Module.
#' @importFrom shinydashboardPlus renderUser
#' @importFrom randomForest randomForest getTree importance
#' @importFrom e1071 skewness
#' @importFrom dendextend color_branches `labels<-` `labels_colors<-` highlight_branches_lwd as.ggdend prepare.ggdend heights_per_k.dendrogram cutree get_leaves_branches_col
#' @importFrom writexl write_xlsx
#' @importFrom GGally ggpairs ggally_points ggally_densityDiag ggally_cor
#' @importFrom ggridges geom_density_ridges
#' @importFrom grDevices colorRampPalette dev.off extendrange graphics.off jpeg pdf png recordPlot replayPlot svg tiff topo.colors
#' @importFrom graphics abline grid hist image layout legend lines par plot.new points spineplot
#' @importFrom stats aggregate ave cmdscale kmeans median na.omit prcomp quantile reorder runif sd var weighted.mean
#' @importFrom utils capture.output citation combn compareVersion head help object.size packageDescription str write.table zip

#' @importFrom dplyr mutate
#' @importFrom plotly plotlyOutput renderPlotly ggplotly config event_register style event_data plotlyProxy plotlyProxyInvoke


#roxygen2::roxygenise()
#' @export
#' @noRd
app_server<-function(input, output, session) {
  t0<-Sys.time()
  # init_server<-Sys.time()
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  # session$onSessionEnded(function() {    stopApp()  })

  #server_init<-Sys.time()

  #ns_tab5<-NS('hc_tab5')
  once_load<-reactiveValues(df=0)


  shinyjs::onevent("mouseenter", "savepoint_out", hide(selector=".tooltip"))

  observeEvent(input$savepoint_out,ignoreInit = T,{

    vals$update_pp<-'tool10'
    updateTabsetPanel(session,"disable_tool",selected=vals$update_pp)
  })
  options(shiny.maxRequestSize=100*1024^3)
  if (    # Make sure that {rstudioapi} is available
    requireNamespace("rstudioapi", quietly = TRUE) &&
    # Returns TRUE if RStudio is running
    rstudioapi::hasFun("viewer")
  ) {
    #options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
  }


  js_code_add_unsup<-'
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(6) > a").addClass("teste_go");
    });
    '
  js_code_add_sup<-'
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(7) > a").addClass("teste_go");
    });
    '

  js_code_add_ens<-'
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(8) > a").addClass("teste_go");
    });
    '








  myBookmarks<-vals<-reactiveValues(
    cur_tool_menu="menu_intro",
    cur_unsup_menu="menu_som",
    cur_sup_menu="menu_nb",
    cur_cc_menu="menu_compare",
    curpage002=1,
    curpage001=1,
    lock_change=F,
    cur_fheight=15,
    cur_fwidth=20,
    cur_fres="100",
    cur_pointsize=12,
    cur_fformat="png",
    cutfacs=F,
    map_data_raster=NULL,
    saved_data=list(),
    saved_model=NULL,
    saved_kcustom=NULL,

    saved_divset_value=0,
    som_unsaved=0,


    shptool="upload",
    bmu_points=T,
    bmuleg=F,
    # bags
    bagdata=c(T,T),
    bagmodel=FALSE,
    baghc=F,
    bagpart0=0,
    baghc0=0,
    bagbp0=0,
    bagshp=F,
    bag_submenu=0,
    bag_user_bp=NULL,


    # hands
    hand_plot=0,
    hand_save=0,
    hand_save2=NULL,
    hand_save3=NULL,
    hand_down=0,

    #results
    bmu_p_results=0,
    cm_rf=0,
    conf_rf=0,
    som_results=NULL,
    pcorr_results=0,

    map_data_disc=NULL,
    map_data_interp=NULL,
    map_fac_disc=NULL,
    map_fac_interp=NULL,
    smw_dp=NULL,
    rfd_res=0,
    rfm_res=0,
    RF_results=NULL,
    rf_unsaved=0,
    map_res=0,

    ##plot
    conf_som=0,
    bmus_pred_plot=0,
    rda_plot=0,
    seg_rda_plot=0,
    pbox_plot=0,
    pclus_plot=0,
    bmus_plot=0,
    pprop_plot=0,
    varplot=0,
    pmds_plot=0,
    ppca_plot=0,
    pdend_plot=0,



    ##Map



    showcoords=F,
    showfactors=F,
    nmax=4,
    nmax_idp=NA,
    nmin_idp=3,
    omax=0,
    maxdist=NA,
    idp=4,
    scalesize_color=T,
    scalesize_size=T,
    pt_factor=1,
    pt_coords=1,
    pt_points_min=0,
    pt_points=1,
    pt_legend=12,
    pt_titlemap=12,
    showguides=F,
    lwd_guides=1,
    scale_map=T,
    colored_map=F,

    #internal
    new_facts=NULL,

    segrda_windows=NULL,
    plot_we=0,
    plot_dp=NULL,
    splitBP=F,
    insertbmu=F,
    sidebar=T,

    newcolhabs=list(
      "turbo" = get('turbo'),
      "matlab.like2"= get('matlab.like2'),
      "viridis" = get('viridis'),
      "plasma" = get('plasma'),
      "Rushmore1"=colorRampPalette(wes_palette("Rushmore1",100,type ="continuous")),
      "FantasticFox1"=colorRampPalette(wes_palette("FantasticFox1",100,type ="continuous")),
      "Blues"=colorRampPalette(RColorBrewer::brewer.pal(9,"Blues")),
      "heat"=get('heat.colors'),
      "Purples"=colorRampPalette(RColorBrewer::brewer.pal(9,"Purples")),
      "Greens"=colorRampPalette(RColorBrewer::brewer.pal(9,"Greens")),
      "Grays"=colorRampPalette(c("gray90","gray10")),
      "gray" = colorRampPalette("gray"),
      "white" = colorRampPalette("white"),
      "black" = colorRampPalette("black"),
      "midnight" = colorRampPalette("#112446"),
      "royalblue" = colorRampPalette("royalblue"),
      "firebrick" = colorRampPalette("firebrick"),
      "forestGreen" = colorRampPalette("forestGreen"),
      "goldenrod3" =  colorRampPalette("goldenrod3")

    ) ## in global
  )




  get_colorimages<-reactive({

    vals$colors_img<-NULL
    for(i in 1:length(vals$newcolhabs)) {
      palname<-names(vals$newcolhabs)[i]
      palette_name<-paste("palette",i)
      outfile<-tempfile(fileext = ".png")

      png(outfile, height =70, width=200)
      par(mar=c(0,0,0,0),mai=c(0,0,0,0),mgp=c(0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0),xpd=T,ann=F, bg=NA, fg=NA)
      colors<-getcolhabs(vals$newcolhabs,names(vals$newcolhabs)[i],1000)
      image(t(matrix(seq(0,1,length.out=1000), nrow=1)), axes=F,col=  colors)

      dev.off()
      palette1<-base64enc::dataURI(file =outfile,mime = "image/png")
      vals$colors_img<-rbind(vals$colors_img,data.frame(val=palname,img= sprintf(paste0(img(src = palette1, height = '20',width = '100',style="margin-top:-40px;margin-bottom:-40px;margin-left:-20px;margin-right:-20px;") ))))
      vals$newcolhabs[[palname]]<-colorRampPalette(colors,alpha=T)

    }
    vals$once_cols<-T

  })
  observeEvent(vals$newcolhabs,{
    req(is.null(vals$once_cols))
    get_colorimages()

  })


  last_btn<-reactiveValues(df='tools_drop1')
  output$note_target<-renderUI({
    # selected=NULL
    # selected=ifelse(input$tabs=='menu_upload',input$data_bank,vals$cur_data)
    tags$div(style="max-width: 250px",pickerInput("note_targ","1. Select the target Datalist:",names(vals$saved_data), selected=NULL))
  })
  modal_comment<-reactive({
    modalDialog(
      title="Create a comment",
      easyClose = T,
      footer=div(modalButton("Cancel"), inline(uiOutput("confirm_note"))),
      div(
        uiOutput("note_target"),
        uiOutput("note_create")
      )
    )
  })
  observeEvent(ignoreInit = T,input$qnote_open,{
    showModal( modal_comment())
  })
  observeEvent(ignoreInit = T,input$confirm_note,{
    vals$note<-gsub("\n",'<br/>',input$note_new)
    attr(vals$saved_data[[input$note_targ]],"notes")<-vals$note
    vals$note<-NULL
    showModal(removeModal())
  })
  output$confirm_note<-renderUI({
    actionButton("confirm_note","4. Confirm")
  })
  output$note_create<-renderUI({
    req(input$note_targ)
    textAreaInput("note_new", "Comments:", value = gsub("<br/>","\n",attr(vals$saved_data[[input$note_targ]],"notes")), width = '500px',
                  height = '150px', cols = NULL, rows = NULL, placeholder = NULL,
                  resize = "both")
  })

  observeEvent(ignoreInit = T,input$tabs,{
    vals$cur_tab<-input$tabs
  }, priority=0)

  observeEvent(vals$newdata,{
    req(isTRUE(vals$newdata))
    vals$newdata<-NULL
    updateTabsetPanel(session,"tabs",selected="menu_upload")
  })
  observeEvent(ignoreInit = T,input$tabs,{
   # remove_unsaved_models()
  })
  remove_unsaved_models<-reactive({
    req(length(vals$saved_data)>0)
    req(length(vals$cur_data)>0)

    try({



      models<-as.character(available_models)
      for(x in models){
        {
          res<-attr(vals$saved_data[[vals$cur_data]],x)
          pic<-grep("(unsaved)",names(res))
          if(length(pic)>0){
            attr(vals$saved_data[[vals$cur_data]],x)<-attr(vals$saved_data[[vals$cur_data]],x)[-pic]
          }


        }
      }


    })
  })


  output$menutitle<-renderUI({
    req(input$tabs!='menu_intro')
    column(12,
           uiOutput("bug"),
           uiOutput("imesc_title")
    )
  })
  output$imesc_title<-renderUI({
    gettitle()
  })
  gettitle<-reactive({
    div(style="color: #05668D", id="menu-title",


        switch(input$tabs,
               "menu_intro"={""},
               "menu_upload"={
                 h4(strong("Data Bank",
                           popify(actionLink('help_databank',icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")), NULL,"Create, manage and download datalists and their associated results. Click for start the tutorial on this panel", options = list(container="body"))
                 ))
               },
               'menu_explore'={
                 h4(strong("Descriptive tools", pophelp(NULL,"Explore and modify data along boxplots, histograms and classic ordination techniques")))
               },
               'menu_sl'={
                 h4(strong("Supervised Algorithms"),
                    # actionLink('slhelp',icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle"))
                 )
               },

               'menu_som'={
                 h4(strong("Unsupervised Self-Organizing Maps"),
                    actionLink('somhelp',icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },

               'menu_som2'={
                 h4(strong("Supervised Self-Organizing Maps (xyf)"),
                    actionLink('somhelp',icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },


               "menu_compare"={h4(strong("Compare models"))},

               'menu_kmeans'={
                 h4(strong("K-Means"),
                    actionLink('kmeanshelp',icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },
               'menu_hc'={
                 h4(strong("Hierarchical Clustering"),
                    actionLink('hclusthelp', icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },

               'menu_maps2'={
                 h4(strong("Spatial tools"))
               },
               'menu_box'={
                 h4(strong("Box plots"))
               },

               'menu_div'={
                 h4( strong("Biodiversity tools"))
               },
               'menu_down'={
                 h4(strong("Download Center"))
               }

        )

    )
  })
  output$textsupersom<-renderUI({
    div(
      fluidRow(column(12,
                      p("This functionality uses the ",code('som')," function from the",
                        actionLink("kohonen","kohonen"),"package."),
                      p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"), "arguments are described 'in the ",actionLink("supersomh","help page")," of the function;")
      ),
      column(12,
             htmlOutput("supersomhelp")
      )))

  })
  output$textsom<-renderUI({

    div(h4("Self-organizing maps..."),
        p("Self-organizing maps (SOMs) is an unsupervised competitive learning method (Kohonen, 2001). The application of SOMs can be thought as a data reduction procedure which aggregates similar data in map units (Best-mathing units - BMUs). BMUs are not the means, or medians, of group of samples, but the results of a learning phase, so that data patterns and features are extracted by modeling, rather than by 'simply' averaging, or linearly projecting on reduced subspaces, data (Liu et al., 2006)"))


  })


  observeEvent(ignoreInit = T,input$somhelp, {
    showModal( modalDialog(
      div(uiOutput("textsom"),
          uiOutput("textsupersom"))
      ,
      title = "Self-Organizing Maps",
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    ))
  })
  observeEvent(ignoreInit = T,input$supersomh,{
    output$supersomhelp<-renderText({
      paste0(
        br(),
        h4("supersom {kohonen}"),
        p(
          icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
          "Argument",
          code("X"),
          "fixed to your uploaded and pre-treated data;"
        ),
        p(
          icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
          code("a1"),
          "and" ,
          code("a2") ,
          "refers to the",
          code("alpha"),
          "argument"
        ),
        p(
          icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
          code("r1"),
          "and" ,
          code("r2") ,
          "refers to the",
          code("radius"),
          "argument"
        ),
        p(
          icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
          "Arguments fixed to their default values:",
          code("whatmap"),
          ", " ,
          code("user.weights"),
          ", " ,
          code("dist.fcts"),
          ", " ,
          code("cores"),
          ", " ,
          code("init"),
          ", " ,
          code("normalizeDataLayers")
        ),
        getHelp('supersom')
      )
    })
  })
  observeEvent(ignoreInit = T,input$kohonen,{
    output$supersomhelp<-renderText({
      getHelp('kohonen')
    })
  })
  output$textrf<-renderUI({

    div(
      fluidRow(column(12,
                      "In essence the Random Forest is based on generating a large number of decision trees, each constructed using a different subset of your training set. These subsets are selected by sampling at random and with replacement from the original data set. The decision trees are then used to identify a classification consensus by selecting the most common output (mode).",
                      p("Random Forest models are generated with",actionLink("gethelp_rf","randomForest"),"package","and tuned with",code('caret'),"(",actionLink('gethelp_train','train'),"and",actionLink('gethelp_trainControl','trainControl'), "functions)")
      ),
      column(12,
             htmlOutput("help_menurf")
      )))

  })
  observeEvent(ignoreInit = T,input$rfhelp, {
    showModal(modalDialog(
      uiOutput("textrf"),
      title = h4(strong("Random Forest")),
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    ))
  })
  observeEvent(ignoreInit = T,input$gethelp_rf,{
    output$help_menurf<-renderText({
      getHelp(utils::help(randomForest, "randomForest"))
    })
  })
  observeEvent(ignoreInit = T,input$gethelp_train,{
    output$help_menurf<-renderText({
      getHelp(utils::help(train, "caret"))
    })
  })
  observeEvent(ignoreInit = T,input$gethelp_trainControl,{
    output$help_menurf<-renderText({
      getHelp(utils::help(trainControl, "caret"))
    })
  })




  modal_help<-function(fun, intro=NULL){

    # Capture the help documentation
    help_doc<-getHelp(fun)

    # Convert the captured documentation to HTML
    showModal(modalDialog(
      title = paste("Help page:",fun),
      tags$div(
        style = "max-height: 500px; overflow-y: auto;",
        intro,
        HTML(help_doc)
      ),
      easyClose = T,
      size="l",
    ))

  }

  output$map_header<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))

    ll_data$server('map_data',vals)
    NULL
  })

  validate_data_map<-reactive({
    validate_data(vals$data_map)
  })




  observeEvent(validate_data_map(),{
    if(isTRUE(validate_data_map())){
      shinyjs::hide("invalid_data_map")
      shinyjs::show("map_data_out")
    } else{
      shinyjs::show("invalid_data_map")
      shinyjs::hide("map_data_out")
    }
  })

  observe({
    llet$server('llet',vals=vals)
  })


  savereac<-reactive({
    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img
    tosave$ssom_tab0<-vals$ssom_tab0

    tosave$saved_ensemble<-vals$saved_ensemble
    tosave$screeplot_results<-vals$screeplot_results

    #saveRDS(reactiveValuesToList(input),"input.rds")
    #saveRDS(tosave,"savepoint.rds")
    beep(10)

  })
  observeEvent(ignoreInit = T,input$flashsave,{
    savereac()

  })
  delay(500,{
    if(!length(grep("connect/apps",getwd()))>0) {
      if(file.exists(paste0(getwd(),'/savepoint.rds'))){
        #showModal(load_qsave_modal()        )
      }
    }
  })



  observeEvent(ignoreInit = T,input$quick_load,{
    showModal(  load_qsave_modal())
  })
  load_qsave_modal<-reactive({
    modalDialog(
      size="m",
      easyClose =T,
      title="Load savepoint",
      footer=div(
        actionButton("load_qsave","Yes"),
        modalButton("No")
      ),
      div(

        div("There is a",em('savepoint', style="color: Grey"),"file in your working directory. Would you like to load it?"),
        div(strong('Directory:'),getwd()),
        div(strong('File:'),'savepoint.rds'),


      ))
  })
  load_qsave<-reactive({
    req(once_load$df==0)
    action<-0
    if(length(input$load_qsave)>0){
      if(input$load_qsave %% 2|input$lastkeypresscode == 13){action<-1} else  if(input$lastkeypresscode == 27){ action<-0 }
    }


    action
  })
  output$qsave_btn<-renderUI({
    req(getwd()=="D:/R3/imesc/imesc3")
    req(length(vals$saved_data)>0)
    div(id="quick_save",actionButton("flashsave",icon(verify_fa = FALSE,name=NULL,class="fas fa-bolt")))
  })
  exist_save<-reactiveValues(df=0)
  observe({
    if(file.exists(paste0(getwd(),'/savepoint.rds'))){
      exist_save$df<-1
    }
  })
  output$qload_btn<-renderUI({
    req(!length(grep("connect/apps",getwd()))>0)
    req( exist_save$df==1)
    actionButton("quick_load",icon(verify_fa = FALSE,name=NULL,class="fas fa-spinner"))
  })
  output$qcomment_btn<-renderUI({
    req(length(vals$saved_data)>0)
    div(actionButton("qnote_open",icon(verify_fa = FALSE,name=NULL,class="fas fa-comment")))
  })
  output$footer_qsave<-renderUI({
    div(class="footer_buttons",
        #p(em("by: Danilo C Vieira")),
        style="background: transparent; color: white",
        inline(uiOutput("imesc_footer")),
        inline(tipify(uiOutput("qnote"),"Creates a note for the Datalist", placement = 'top'))
    )
  })
  output$imesc_footer<-renderUI({

    div(id="qsave_btns",
        inline(uiOutput("qsave_btn")),
        #inline(uiOutput("qload_btn")),
        inline(uiOutput("qcomment_btn")),
        bsTooltip('flashsave', "Creates savepoint file in your working directory", 'top'),
        bsTooltip('quick_load',  "Load savepoint file from your working directory", 'top'),
        bsTooltip('qnote_open',  "Creates a comment for the Datalist", 'top')
    )
  })


  observeEvent(input$dimension,{
    delay(500,{vals$dimension_display<-input$dimension})
  })


  pre_process$server("preproc", vals)

  output$preprocess<-renderUI({

    NULL
  })

  output$menu_intro_out<-renderUI({
    div(style="background: white",
        tabsetPanel(
          tabPanel("Welcome",
                   value="intro1",
                   column(12,
                          style="background: white",
                          textintro(),
                          uiOutput("license")
                   )),
          tabPanel("Authors",
                   value="intro2",
                   textauthors()),
          tabPanel("Version",value="intro5",
                   column(12, style="background: white",
                          div(
                            p(h4(style="margin-top 25px",span("iMESc", style="font-family: 'Alata', sans-serif;")),version),
                            p(strong("Last update:"),last_update)

                          ),
                          div(h5(strong("Technical Development Specifications:")),
                              uiOutput("sys_info")
                          ),
                          div(em("developed by Danilo C Vieira"))


                   )
          ),
          tabPanel(
            "Documentation",value="intro5",
            column(6, style="background: white",
                   div("Access the complete documentation at",actionLink('imesc_help_link',HTML("iMESc<sup>help!</sup>")),"for detailed guides and support. It provides everything needed to effectively utilize iMESc."),
                   #HTML("Access the complete documentation at <a href='https://danilocvieira.github.io/iMESc_help' target='_blank'>iMESc<sup>help!</sup></a> for detailed guides and support. It provides everything needed to effectively utilize iMESc."),
            )),
          tabPanel(
            "Video Tutorials",value="intro6",
            column(
              12, style="background: white",
              column(
                3,class="mp0",
                radioGroupButtons(
                  "video_tab",NULL,
                  direction ="vertical",
                  justified = FALSE,
                  choices=c(
                    "Creating Datalists"="video1",
                    "Pre-processing tools"="video2",
                    "Descriptive tools"="video3",
                    "Biodiversity tools"="video4",
                    "Spatial tools"="video5",
                    "Self-Organizing Maps"="video6",
                    "Hierarchical Clustering"="video7",
                    "K-Means"="video8",
                    "Supervised Algorithms"="video9",
                    "Compare Models"="video10",
                    "Exchange Factor/Variables"="video11",
                    "SHP toolbox"="video12"
                  )
                )),
              column(9,class="mp0",uiOutput("videos"))
            ))

        )
    )
  })


  get_video<-reactive({
    switch(input$video_tab,
           "video1"={'https://danilocvieira.github.io/iMESc_help/images/t1-creating-datalist.mp4'},
           "video2"={'https://danilocvieira.github.io/iMESc_help/images/pre-processing.mp4'},
           "video3"={'https://danilocvieira.github.io/iMESc_help/images/module02-descriptive-tools.mp4'},
           "video4"={'https://danilocvieira.github.io/iMESc_help/images/module03-biodiversity-tools.mp4'},
           "video5"={'https://danilocvieira.github.io/iMESc_help/images/module04-spatial-tools.mp4'},
           "video6"={'https://danilocvieira.github.io/iMESc_help/images/module05-supersom.mp4'},
           "video7"={'https://danilocvieira.github.io/iMESc_help/images/module06-hierarchical-clustering.mp4'},
           "video8"={'https://danilocvieira.github.io/iMESc_help/images/module07-kmeans.mp4'},
           "video9"={'https://danilocvieira.github.io/iMESc_help/images/module08-supervised-algorithms.mp4'},
           "video10"={'https://danilocvieira.github.io/iMESc_help/images/module09-compare-models.mp4'},
           "video11"={'https://danilocvieira.github.io/iMESc_help/images/options-exchange-factors-variables.mp4'},
           "video12"={'https://danilocvieira.github.io/iMESc_help/images/options-shp-toolbox.mp4'})
  })

  output$videos<-renderUI({
    tags$video(
      id = "myVideo",
      height="400px",
      src = get_video(),
      type = "video/mp4",
      autoplay=FALSE,
      muted=TRUE,
      playsinline=TRUE,
      loop=F,
      controls=T)

  })
  output$license<-renderUI({
    column(12,
           column(12, h4(strong('Copyright Notice'), style="color: #05668D;"),
                  p(icon('copyright'),'2023 Danilo Candido Vieira and Gustavo Fonseca. All rights reserved.'),
                  p('The content, coding, and related assets associated with the iMESc project are the sole property of the authors, Danilo Candido Vieira and Gustavo Fonseca. Redistribution, modification, commercial use, or any other form of utilization of this project is strictly prohibited without the express written consent of the authors.'),

                  h4(strong('License'), style="color: #05668D;"),
                  p("This project is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International (CC BY-NC-ND 4.0) license. This license allows others to download the project and share it with others as long as they credit the authors, but they can't change it in any way or use it commercially."),

                  h4(strong("Disclaimer"), style="color: #05668D;"),
                  p("The iMESc application ('the software') is provided 'as-is', without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose, and non-infringement. In no event shall the authors or copyright holders be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of, or in connection with the software or the use or other dealings in the software."),

                  p("While efforts have been made to ensure the accuracy and reliability of the software, the authors cannot guarantee that the software will always operate error-free or that it is completely secure. Users are advised to exercise their own skill and care with respect to their use of the software."),

                  p("The software may contain links to other websites or resources. The authors are not responsible for the availability of such external sites or resources, and do not endorse and are not responsible or liable for any content, advertising, products, or other materials on or available from such sites or resources.")
           ))
  })
  output$sys_info<-renderUI({
    print(getwd())
    sys_info<-readRDS('inst/www/sys_info.rds')
    do.call('div',args=list(sys_info))
  })
  observeEvent(input$imesc_help_link_side,{
    browseURL("https://danilocvieira.github.io/iMESc_help")
  })
  observeEvent(input$imesc_help_link,{
    browseURL("https://danilocvieira.github.io/iMESc_help")
  })
  output$menu_bank_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))

    databank_module$server('databank', vals=vals)
    NULL

  })




  output$menu_div_out<-renderUI({


    mod_div<-diversity_tool$server("module_div",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    NULL
  })
  output$menu_hc_out<-renderUI({

    hc_module$server('hc', vals=vals)
    NULL
  })
  output$menu_kmeans_out<-renderUI({
    k_means_module$server('module_kmeans',vals)
    NULL
  })


  output$menu_upload_out<-renderUI({
    desctools$server("module_desctools",  vals=vals)
    NULL
  })





  output$menu_sl_out<-renderUI({
    caret_models$server("sl_models",vals)
    NULL
  })




  output$menu_som_out<-renderUI({
    div(imesc_supersom$ui("som",vals),
        uiOutput('som_out'))


  })
  output$som_out<-renderUI({
    imesc_supersom$server("som",vals=vals)
    NULL
  })


  output$menu_keras_out<-renderUI({
    # module_keras$server("keras",vals=vals)
    NULL
  })




  output$menu_spd_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<-module_ui_spd("module_spd")
    mod_spd<-callModule(module_server_spd, "module_spd",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs,df_symbol=df_symbol)
    removeModal()
    res
  })

  output$menu_ensemble_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    #res<-module_ui_comp2("ensemble")
    #callModule(module_server_comp2, "ensemble",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    #res
  })
  output$menu_compare_out<-renderUI({

    compare_models$server("module_comp",  vals)
    NULL
  })



  observeEvent(vals$update_curtab,{
    updateTabsetPanel(session,"tabs",selected=vals$update_curtab)
    vals$update_curtab<-NULL
  })

  once_savepoint<-reactiveVal(F)
  observe({
    req(file.exists("savepoint.rds"))
    req(isFALSE(once_savepoint()))
    once_savepoint(TRUE)


    newvals0<-newvals<-readRDS('savepoint.rds')

    newvals <-newvals0[!names(newvals)%in%c('newcolhabs','colors_img')]



    if(is.null(newvals$cur_data)){newvals$cur_data<-names(vals$saved_data)[[1]]}
    vals<-myBookmarks
    for (var in names(newvals) ) {
      vals[[var]]<-newvals[[var]]
    }


    delay(500,{
      # updateTextInput(session, "tabs", value = newvals$cur_tab)
      runjs("Shiny.setInputValue('last_btn', 'fade');")
    })

    removeModal()
  })

  observeEvent(vals$saved_data,{
    if(length(vals$saved_data)>0){
      shinyjs::hide("disable_tool")
    }
  })

  t1<-Sys.time()
  #print(t1-t0)

  observeEvent(input$imesc_desktop_git,{
    browseURL("https://github.com/DaniloCVieira/iMESc-Desktop")
  })

  observe({
    req(grepl("iMESc-Desktop",getwd()))
    req(check_version())
    result<-check_version()
    current_version<-attr(result,"current")
    latest_version<-attr(result,"latest")
    showModal(
      modalDialog(
        title="iMESc ",
        div(


          div("A newer version of iMESc-Desktop is available:", embrown(latest_version)),
          div("Downlod the new version in",
              actionLink('imesc_desktop_git', "https://github.com/DaniloCVieira/iMESc-Desktop"),
          ),
        )
      )
    )

  })

}
