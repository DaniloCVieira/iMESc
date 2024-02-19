options(shiny.autoload.r=FALSE)
#' @importFrom readxl excel_sheets read_excel
#' @importFrom gplots heatmap.2
#' @importFrom Metrics mae mape mse rmse
#' @import leaflet sf gstat randomForestExplainer ggplot2
#' @importFrom vegan ordiplot rda scores decostand vegdist metaMDS specnumber diversity rrarefy ordiArrowTextXY
#' @importFrom mice mice complete
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
#' @import kohonen
#' @importFrom caret BoxCoxTrans createDataPartition confusionMatrix confusionMatrix.train createFolds createResample postResample varImp filterVarImp multiClassSummary trainControl plot.train nzv findCorrelation preProcess getModelInfo
#' @import shiny viridis colorRamps wesanderson shinyTree sortable
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
#' @importFrom utils capture.output citation combn           compareVersion head help object.size packageDescription str write.table zip


#roxygen2::roxygenise()
#' @export
#' @noRd

app_server<-function(input, output, session) {
 # init_server<-Sys.time()
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
 # session$onSessionEnded(function() {    stopApp()  })

  #server_init<-Sys.time()

  #ns_tab5<-NS('hc_tab5')
  once_load<-reactiveValues(df=0)
  insertbmu_tab<-reactiveValues(df=F)

  options(shiny.maxRequestSize=100*1024^3)
  if (    # Make sure that {rstudioapi} is available
    requireNamespace("rstudioapi", quietly = TRUE) &&
    # Returns TRUE if RStudio is running
    rstudioapi::hasFun("viewer")
  ) {
    #options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
  }

  aggreg_reac<-reactiveValues(df=0)
  symbols<-c("pch1","pch2","pch3","pch4")
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

  #sidebar-main > div.sidebar-menu > li:nth-child(6) > a
  js_code_remove<-'$(".teste_go").removeClass("teste_go");'
  fun_toogle_submenus<-'$(".sidebar-menu ul.treeview-menu").css("display", "none");'
  funs_dynamic_class<-list(
    'add_unsup'=js_code_add_unsup,
    'add_sup'=js_code_add_sup,
    'add_cc'=js_code_add_ens,
    "remove"=js_code_remove
  )
  is_ccmenu<-function(x){
    x%in%c("menu_compare","menu_ensemble")
  }
  is_unsupmenu<-function(x){
    x%in%c("menu_som","menu_hc",'menu_kmeans')
  }
  is_supmenu<-function(x){
    x%in%c("menu_rf","menu_nb",'menu_svm','menu_knn','menu_sgboost','menu_som2')
  }
  is_toolmenu<-function(x){
    x%in%c('menu_intro','menu_upload','menu_explore','menu_div')
  }
  is_spatialmenu<-function(x){
    x%in%c('menu_maps','menu_maps2')
  }
  which_menu<-function(x){
    if(is_spatialmenu(x)){return("spatial_expand")} else if(is_toolmenu(x)){return("tool_menu")} else if(is_unsupmenu(x)){'unsup_expand'}else
      if(is_supmenu(x)){'sup_expand'}else  if(is_ccmenu(x)){'cc_expand'} else{"none"}
  }
  menu_is_in_expand<-function(menu, expand) {
    return(which_menu(menu) == expand)
  }
  observeEvent(input$sidebarItemExpanded,ignoreInit = T,{
    shinyjs::runjs(funs_dynamic_class$remove)
  })
  getjs_expand2<-function(menu,funs_dynamic_class,expanded){
    if(is.null(menu)){return()}
    if(is_toolmenu(menu)){
      shinyjs::runjs(fun_toogle_submenus)
      #shinyjs::runjs('$(".sidebar-menu ul.treeview-menu").css("display", "none");')
      shinyjs::runjs(funs_dynamic_class$remove)
    }
    if (is_unsupmenu(menu)) {
      if(!is.null(expanded)){
        shinyjs::runjs(funs_dynamic_class$remove)
        shinyjs::runjs(funs_dynamic_class$add_unsup)}
    }
    if(is_supmenu(menu)){
      if(!is.null(expanded))
        if(expanded=="sup_expand"){
          shinyjs::runjs(funs_dynamic_class$remove)
          shinyjs::runjs(funs_dynamic_class$add_sup)}
    }
    if(is_ccmenu(menu)){
      if(!is.null(expanded))
        if(expanded=="cc_expand"){
          shinyjs::runjs(funs_dynamic_class$remove)
          shinyjs::runjs(funs_dynamic_class$add_cc)}

    }
  }
  observe({    #
    getjs_expand2(input$tabs,funs_dynamic_class,input$sidebarItemExpanded)
  })
  observe({
    #update_submenu(input$sidebarItemExpanded,funs_dynamic_class, input$tabs)
  })
  observeEvent(ignoreInit = T,input$tabs,{
    if(is_supmenu(input$tabs)){
      vals$cur_sup_menu<-input$tabs
    } else  if(is_unsupmenu(input$tabs)){
      vals$cur_unsup_menu<-input$tabs
    } else if(is_toolmenu(input$tabs)){
      vals$cur_tool_menu<-input$tabs
    } else if(is_ccmenu(input$tabs)){
      vals$cur_cc_menu<-input$tabs
    }
  })
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
  upload_bag<-reactiveValues(df=0, df0=0)
  cur_surface<-reactiveValues(df=F)
  cur_map_filter1<-reactiveValues(df="None")
  cur_map_filter2<-reactiveValues(df=1)
  vars_fac<-reactiveValues(df=0)
  vars_data<-reactiveValues(df=0)
  cur_tag<-reactiveValues(df=1)
  bagbuck<-reactiveValues(df=list())
  bag_newfac<-reactiveValues(df=0)
  bag_newdat<-reactiveValues(df=0)
  import_rv<-reactiveValues(page_imp = 1)
  cutfacs<-reactiveValues(df=F)
  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  gosave<-reactiveValues(df=0,  modal=F) # Corresponds to the current displayed input$myinput
  last_btn<-reactiveValues(df='tools_drop1')
  insert_rv<-reactiveValues(page = 1)
  react<-reactiveValues(df=NULL)
  update_selecvar<-reactiveValues(df=F)
  data_cogs<-reactiveValues(df=0)
  data_cog_bag<-reactiveValues(df=0)
  toggled<-reactiveValues(df=F)
  status_changes<-reactiveValues(df=F)
  treefalse<-reactiveValues(df=F)
  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(10))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic
  })
  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })
  symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
  df_symbol<-data.frame(
    val = c(16,15,17,18,8,1,5,3)
  )
  for(i in 1:length(symbols))
  {
    symbol1<-base64enc::dataURI(file = paste0('inst/www/pch',i,".png"), mime = "image/png")

    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}
  observeEvent(ignoreInit = T,input$choices_map,{
    vals$cur_choices_map=input$choices_map
  })



  #######

  observeEvent(ignoreInit = T,input$delete_datalist,{
    req(length(input$del_datalist)>0)
    pic<-which(names(vals$saved_data)%in%input$del_datalist)
    vals$saved_data<-vals$saved_data[-pic]
    removeModal()
    runjs("Shiny.setInputValue('last_btn', 'fade');")
  })



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






  observeEvent(ignoreInit = T,input$tools_save_changes,{

    # updateRadioGroupButtons(session,'radio_cogs',selected = last_btn$equal[1])

    vals$hand_save<-"Save changes"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(
      hand_save_modal()
    )


  })



  output$bank_input0<-renderUI({
    req(length(vals$saved_data)<1)
    div(style="margin-top: 5px;",
        div(class="get_started",
            div(icon("fas fa-hand-point-up", style="font-size: 30px; color: SeaGreen")),
            div(em("Get started by creating a Datalist", style="color: black"))

        )

    )
  })






  #######


  observeEvent(ignoreInit = T,input$data_div,{
    vals$cur_data<-input$data_div})

  output$menu_nb_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    vals$module<-"Naive bayes"
    res<-module_ui_nb("module_nb")
    mod1<-callModule(module_server_nb, "module_nb",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })
  output$menu_kmeans_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    vals$module<-"kmeans"

    res<-module_ui_kmeans("module_kmeans")
    mod_kmeans<-callModule(module_server_kmeans, "module_kmeans",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })
  output$menu_ensemble_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<-module_ui_comp2("ensemble")
    callModule(module_server_comp2, "ensemble",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    res
  })
  output$menu_compare_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<-module_ui_comp("module_comp")
    callModule(module_server_comp, "module_comp",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs,df_symbol=df_symbol)
    res
  })
  output$menu_sgboost_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))

    res<-module_ui_sgboost("module_sgboost")
    mod_sgboost<-callModule(module_server_sgboost, "module_sgboost",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })
  output$menu_knn_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<-module_ui_knn("module_knn")
    mod_knn<-callModule(module_server_knn, "module_knn",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })
  output$menu_svm_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))

    res<-module_ui_svm("module_svm")
    mod_svm<-callModule(module_server_svm, "module_svm",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })
  output$menu_rf_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))


    res<-module_ui_rf("module_rf")
    mod_rf<-callModule(module_server_rf, "module_rf",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })
  observeEvent(ignoreInit = T,input$load_savepoint_yes,{

    removeModal()
  })
  lines_vals<-c("solid","dashed","dotted")
  dflines<-data.frame(vals=1:3)
  for(i in  1:length(lines_vals))  {
    dflines$img[i]<- sprintf(paste(
      div(style=paste('border-top: 1px',lines_vals[i],'SeaGreen;',"padding: 2px; color: black"))))}
  output$down_raster<-downloadHandler(
    filename = "raster.tif",
    content = function(file) {
      raster::writeRaster(attr(vals$map_res,"my_rst"), file,format="GTiff")
    }
  )
  observeEvent(ignoreInit = T,input$tabs,{
    vals$cur_tab<-input$tabs
  }, priority=0)
  observeEvent(ignoreInit = T,input$tabs,{
    remove_unsaved_models()
  })
  remove_unsaved_models<-reactive({
    req(length(vals$saved_data)>0)
    req(length(vals$cur_data)>0)

    try({



      models<-c("kmeans","som","nb","svm","knn","rf","sgboost","xyf")
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

  menu_steps<-reactiveValues(df="menu_intro", num=1, cur=NULL)
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
  observeEvent(ignoreInit = T,input$tabs,{
    vals$cur_tab<-input$tabs
  })
  # These cause a side-effect by changing the place value


  observe(vals$saved_divset_value<-isolate(length(vals$saved_data)+1))

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
               'menu_nb'={
                 h4(strong("Naive Bayes", pophelp(NULL,"classification technique based on Bayes Theorem with an assumption of independence among predictors")))

               },
               'menu_svm'={
                 h4(strong("Support Vector Machine", pophelp(NULL," Support vector machine methods can handle both linear and non-linear class boundaries")))

               },
               'menu_som'={
                 h4(strong("Unsupervised Self-Organizing Maps"),
                    actionLink('somhelp',icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },

               'menu_som2'={
                 h4(strong("Supervised Self-Organizing Maps (xyf)"),
                    actionLink('somhelp',icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },
               'menu_knn'={
                 h4(strong("K-Nearest Neighbor"),
                    actionLink('knnhelp',pophelp("K-Nearest Neighbor","For each row of the test set, the k nearest (in Euclidean distance) training set vectors are found, and the classification is decided by majority vote, with ties broken at random. If there are ties for the kth nearest vector, all candidates are included in the vote")))
               },
               'menu_sgboost'={
                 h4(strong("Stochastic Gradient Boosting"),
                    actionLink('gbmhelp',pophelp("sgboost","GBM is defined as a stepwise approximation procedure to match an additive regression model. GBM minimizes the loss function. Weak models are introduced sequentially, trained using weighted data from training. The process goes on until a pre-set number of weak learners is generated or no further progress can be made on the learning data set.")))
               },
               "menu_compare"={h4(strong("Compare models"))},
               "menu_ensemble"={h4(strong("Ensemble models"))},
               'menu_kmeans'={
                 h4(strong("K-Means"),
                    actionLink('kmeanshelp',icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },
               'menu_hc'={
                 h4(strong("Hierarchical Clustering"),
                    actionLink('hclusthelp', icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },
               'menu_rf'={
                 h4(strong("Random Forest"),
                    actionLink('rfhelp', icon(verify_fa = FALSE,name=NULL,class="fas fa-info-circle")))
               },

               'menu_maps2'={
                 h4(strong("Spatial tools"))
               },
               'menu_box'={
                 h4(strong("Box plots"))
               },
               'menu_spd'={
                 h4(strong("Explore Saved Models"))
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
      getHelp(help(randomForest, "randomForest"))
    })
  })
  observeEvent(ignoreInit = T,input$gethelp_train,{
    output$help_menurf<-renderText({
      getHelp(help(train, "caret"))
    })
  })
  observeEvent(ignoreInit = T,input$gethelp_trainControl,{
    output$help_menurf<-renderText({
      getHelp(help(trainControl, "caret"))
    })
  })
  observeEvent(ignoreInit = T,input$tag_edit,{
    cur_tag$df<-input$tag_edit
  })
  observeEvent(ignoreInit = T,input$tag_fun,{
    vals$cur_tag_fun<-input$tag_fun
  })
  output$tag_edit_level<-renderUI({
    factors<-attr(getdata_upload(),"factors")
    inline(div(class="tool6_1", pickerInput('tag_edit_level',"Level:",levels(factors[,input$tag_edit]), width="125px")))
  })
  output$tag_edit_newlevel<-renderUI({
    textInput('tag_edit_newlevel',"New label:",placeholder="Text a new label", width="125px")
  })
  output$na_warning<-renderUI({
    req(input$na_method=='bagImpute')
    div(
      strong("warning",style="color: red"),"This method has higher computational cost and can be very time-consuming."
    )
  })
  output$na_methods<-renderUI({
    req(input$na_targ)
    choices<-c("Knn","bagImpute","medianImpute","pmm","rf","cart")
    if(input$na_targ=="Factor-Attribute"){
      choices<-c("pmm","rf","cart")
    }

    pickerInput("na_method", span("Method",tipify(actionLink("na_help",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), type="toggle"),"Click for details","right")),choices, width="120px")
  })
  observeEvent(ignoreInit = T,input$na_targ,{
    choices=if(input$na_targ=="Numeric-Attribute"){
      c("median/mode","Knn","bagImpute","medianImpute")
    } else{
      c("median/mode")
    }
    updateSelectInput(session,'na_method', choices=choices)
  })
  output$imputation_help<-renderUI({
    hidden(
      column(12, id="na_help_text", style="background: white; white-space: normal;",
             h4("Imputation methods",style="border-bottom: 1px solid SeaGreen;",actionLink("naclose",icon(verify_fa = FALSE,name=NULL,class='fas fa-window-close'))),

             div(
               strong("median/mode:"),
               p('Numeric-Attribute columns are imputed with the median;'),
               p('Factor-Attribute columns are imputed with the mode')

             ),
             div(
               strong("Knn"),
               p('k-nearest neighbor imputation is only available for the Numeric-Attribute. It is carried out by finding the k closest samples (Euclidian distance) in the dataset. This method automatically center and scale your data.')),
             div(
               strong("bagImpute"),
               p('Only available for the Numeric-Attribute. Imputation via bagging fits a bagged tree model for each predictor (as a function of all the others). This method is simple, accurate and accepts missing values, but it has much higher computational cost. ')),
             div(
               strong("medianImpute"),
               p('Only available for the Numeric-Attribute. Imputation via medians takes the median of each predictor in the training set, and uses them to fill missing values. This method is simple, fast, and accepts missing values, but treats each predictor independently, and may be inaccurate.'))

      )
    )
  })
  observeEvent(ignoreInit = T,input$na_help,{
    shinyjs::toggle("na_help_text")
  })
  observeEvent(ignoreInit = T,input$naclose,{
    shinyjs::toggle("na_help_text")
  })
  observeEvent(ignoreInit = T,input$dropID_tool4,{
    if(!input$dropID_tool4 %%2){
      shinyjs::hide("na_help_text")
    }
  })
  observeEvent(ignoreInit = T,input$match,{
    if(!input$match %% 2)
      updateSelectInput(session,"filter_data", selected="none")
  })
  observeEvent(ignoreInit = T,input$addpart,{
    vals$hand_save<-"add partition"
    vals$hand_save2<-"This action adds the partition as a factor in the Factor-Attribute."
    vals$hand_save3<-NULL
    toggleDropdownButton('dropID_tool5')
    showModal(
      hand_save_modal()
    )})
  output$partition_preview<-renderPrint({get_partition()})
  output$datalist_cogs_out<-renderUI({
    #req(length(vals$saved_data)>0)
    div(id="dcogs",
        div(id="dcogs0",class="dcogs0",span(icon(verify_fa = FALSE,name=NULL,class="fas fa-cog"),style="padding-left: 7px")),
        div(id="dlinks_all",class='dlinks_all',


            div(class="dlinks",
                actionLink("dcogs1","Rename Datalist")
            ),
            div(class="dlinks",
                actionLink("dcogs2","Merge Datalists")
            ),
            div(class="dlinks",
                actionLink("dcogs3","Exchange Factor/Variables")
            ),
            div(class="dlinks",
                actionLink("dcogs4","Replace Factor-Attribute")
            ),
            div(class="dlinks",
                actionLink("dcogs6","Edit Datalist columns")
            ),
            div(class="dlinks",
                actionLink("dcogs9","Build IDs/Columns & Formulas")
            ),

            div(class="dlinks",
                actionLink("dcogs8","Edit/Merge Models")
            ),
            div(class="dlinks",
                actionLink("dcogs5","Transpose Datalist")
            ),
            div(class="dlinks",
                actionLink("shp_view","SHP toolbox")
            ),
            div(class="dlinks",
                actionLink("show_mananger","Datalist mananger",style="color: red")
            ),
            div(class="dlinks",
                actionLink("dcogs7","Delete Datalist",style="color: red")
            )


        )
    )
  })
  observeEvent(ignoreInit = T,input$dcogs9,{
    showModal( modal_char())
  })
  modal_char<-reactive({

    tags$div(     id='char_modal',
                  modalDialog(
                    fluidPage(
                      column(12,
                             column(
                               12,
                               column(4,uiOutput('tochar_fun4')),
                               column(4, uiOutput('tochar_data1')),
                               column(4, uiOutput('tochar_att2')),
                               column(4,uiOutput('tochar_colbox3')),

                               column(4,uiOutput("tochar_find5")),
                               column(4,uiOutput('tochar_replace7'))
                             ),


                             column(12,
                                    style="padding-top: 10px",class="well3",
                                    inline(uiOutput('tochar_find6')),
                                    uiOutput("tochar_form6"),
                                    column(4,uiOutput("tochar_new")),
                                    column(4,uiOutput('tochar_buildcols8'))),
                             column(12,style="padding-top: 10px",
                                    uiOutput("tochar_build9")),
                      ),







                    ),

                    title="Build Variables/Factor",
                    easyClose = T,
                    size ="xl"
                  )
    )

  })
  observeEvent(ignoreInit = T,input$char_fun,{
    vals$char_fun<-input$char_fun
  })
  output$tochar_fun4<-renderUI({
    div(
      div(strong("1. Function")),
      div(pickerInput("char_fun", NULL,choices=list(
        "Find & Replace"="fr",
        "Concatenate"="con",
        "Apply formula"="for"

      ), width="200px",selected=vals$char_fun))
    )
  })
  output$tochar_new<-renderUI({
    #req(length(input$char_pat)>0)
    df<-char_picvar()
    if(input$char_fun=="fr"){
      req(input$char_pat)
      replaces_names<-lapply(df,function(x) rownames(df)[grep(input$char_pat,x)])
      replaces<-lapply(df,function(x) grep(input$char_pat,x))
    }





    div(style=" overflow-x: scroll;height:120px;overflow-y: scroll;",
        strong("Preview", tiphelp("preview of the first 6 lines")),
        #renderPrint(replaces_names),
        renderPrint({
          #req(length(input$char_pat)>0)
          df<-char_picvar()
          if(input$char_fun=="fr"){
            req(length(char_replacements())>0)

            df<-data.frame(lapply(df,function(x) gsub(input$char_pat,input$char_rep,x)))
            res<-lapply(1:length(replaces),function(i){
              head(data.frame(id=replaces_names[[i]],newvalue=df[replaces[[i]],i]))
            })
            names(res)<-names(replaces)
            res
          } else if(input$char_fun=="con"){
            #req(input$char_paste_collapse)
            df<-apply(data.frame(df),1,function(x){
              paste(x,collapse=input$char_paste_collapse)
            })
            res<-data.frame(df)
            colnames(res)<-vals$char_newcols[,1]
            head(res)

          } else if(input$char_fun=="for"){
            form<-applyformula()
            req(length(form)>0)
            df<-data.frame(form)
            res<-data.frame(df)
            colnames(res)<-vals$char_newcols[,1]
            res
          }

          #df<-attr(vals$saved_data$`data_2023-03-14`,"factors")
          #
          # input<-list()
          #input$char_pat<-"C2"





        })
    )
  })
  applyformula<-reactive({

    charvar<- char_picvar()
    form<-input[["sort_x"]]


    fun<-function(charvar,form){
      res<-try({
        with(charvar,eval(parse(text=paste0(form, collapse=""))))
      },silent =T)
      validate(need(class(res)!="try-error","Check your formula. if you are using square root, make sure that the object you want to take the root of is enclosed in parentheses."))


      res
    }
    fun(charvar,form)


  })
  observeEvent(ignoreInit = T,input$char_build,{
    data_o<-vals$saved_data[[input$char_data]]
    df<-char_picvar()

    if(input$char_fun=="fr"){
      req(length(input$char_pat)>0)
      temp<-data.frame(lapply(df,function(x) gsub(input$char_pat,input$char_rep,x)))
    } else if(input$char_fun=="con"){
      temp<-apply(data.frame(df),1,function(x){
        paste(x,collapse=input$char_paste_collapse)
      })
      temp<-data.frame(temp)
    } else if(input$char_fun=="for"){
      temp<-data.frame(applyformula())
      temp<-data.frame(temp)
      colnames(temp)<-vals$char_newcols[,1]

    }



    if( attr(df,"char_attrib")=="Numeric"){

      data_o[vals$char_newcols[,1]]<-lapply(temp,function(x) as.numeric(x))
    } else{
      attr(data_o,"factors")[,vals$char_newcols[,1]]<-factor(temp[,1])
    }



    data_cogs$df<-data_o
    # updateRadioGroupButtons(session,'radio_cogs',selected = last_btn$equal[1])

    vals$hand_save<-"Save changes"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(
      hand_save_modal()
    )


  })
  char_replacements<-reactive({
    req(length(input$char_col)>0)
    req(input$char_att)
    req(length(input$char_pat)>0)
    df<-char_picvar()
    req(input$char_pat)
    replaces<-lapply(df,function(x) grep(input$char_pat,x))
    replen<-lapply(replaces,function(x) length(x))
    unlist(replen)


  })
  observeEvent(ignoreInit = T,input$char_data,{
    vals$char_data<-input$char_data
  })
  output$tochar_data1<-renderUI({
    div(
      div(strong("2. Select the Datalist:")),
      div(pickerInput("char_data", NULL,names(vals$saved_data),selected=vals$char_data, width="200px"))
    )
  })
  output$tochar_att2<-renderUI({
    req(input$char_data)
    req(input$char_fun)
    choices<-if(input$char_fun=="fr"){
      c("Factor","Numeric")
    } else if(input$char_fun=="con"){
      c("Factor")
    } else if(input$char_fun=="for"){
      c("Numeric")
    }
    div(

      div(strong("3. the Attribute:")),
      div(pickerInput("char_att", NULL,choices=choices, width="200px"))
    )
  })
  output$tochar_colbox3<-renderUI({
    req(input$char_fun)
    req(input$char_data)
    req(input$char_att)
    div(style="width: 200px",
        strong("4. the target Columns:"),
        div(
          uiOutput("tochar_col_checkall"),
          uiOutput("tochar_col")
        )
    )

  })
  output$tochar_col_checkall<-renderUI({
    req(input$char_fun)
    req(input$char_fun!="fr")
    div(
      checkboxInput("charvar_check",strong('Select/Unselect all'),F)
    )
  })
  output$tochar_col<-renderUI({
    req(input$char_fun)
    choices<-char_df()
    multiple<-ifelse(input$char_fun=="fr",FALSE,TRUE)
    div(
      div(
        class = "choosechannel",
        pickerInput("char_col", NULL,colnames(choices),colnames(choices)[1:2],multiple =multiple))
    )
  })
  observeEvent(ignoreInit = T,input$charvar_check,{
    choices<-colnames(char_df())
    req(isFALSE(input$charvar_check))
    updatePickerInput(session,"char_col",NULL,choices = choices,selected=NULL   )
  })
  observeEvent(ignoreInit = T,input$charvar_check,{
    choices<-colnames(char_df())
    req(isTRUE(input$charvar_check))

    updatePickerInput(session,
                      "char_col",NULL,
                      choices = choices,
                      selected = choices
    )


  })
  observeEvent(ignoreInit = T,input$char_paste_collapse,{
    vals$char_paste_collapse<-input$char_paste_collapse
  })
  output$tochar_conc<-renderUI({
    req(input$char_fun)
    req(input$char_fun=="con")
    if(is.null(vals$char_paste_collapse)){vals$char_paste_collapse<-""}
    div(style="vertical-align: text-top;display: table;",
        div(
          div(
            div(strong("5. Collapse:")),
            div(textInput("char_paste_collapse", NULL,vals$char_paste_collapse, width="200px"))
          )


        )

    )
  })
  output$tochar_find5<-renderUI({
    req(input$char_fun)
    if(input$char_fun=="fr"){
      div(
        div(strong("5. Find pattern:"), style="color: Seagreen"),
        div(textInput("char_pat", NULL,"", width="200px"))
      )} else if(input$char_fun=="con"){
        uiOutput("tochar_conc")
      }
  })
  output$tochar_replace7<-renderUI({
    req(input$char_fun)
    req(input$char_fun=="fr")
    div(
      div(strong("6. Replace pattern by:",tipify(icon('fas fa-question-circle'),"Type the replacement"), style="color: Seagreen")),
      div(textInput("char_rep", NULL,"", width="200px"))
    )
  })
  output$tochar_buildcols8<-renderUI({
    req(input$char_fun)

    num<-if(input$char_fun=="fr"){
      '7. '} else if(input$char_fun=="con") {"6. "} else if (input$char_fun=="for"){'6.'

      }
    div(
      div(strong(num,"Create columns:", tipify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Double-click to edit"))),
      div(class="save_changes",

          uiOutput("tochar_buildcols_df")
      )
    )
  })
  output$tochar_build9<-renderUI({
    req(input$char_fun)
    num<-if(input$char_fun=="fr"){
      '8. '} else if(input$char_fun=="con") {"7. "} else if (input$char_fun=="for"){'7.'

      }

    div(
      div(strong(num,"Build columns with the new values")),
      div(class="save_changes",
          actionButton("char_build", "Build Column", width="200px"),
          inline(uiOutput("build_rownames_button"))

      )
    )
  })
  output$build_rownames_button<-renderUI({
    req(input$char_fun=="con")
    span(strong("or"),actionButton("build_rownames", "Build IDs", width="200px"))
  })
  observeEvent(ignoreInit = T,input$build_rownames,{


    data_cogs$df<-get_datanewID()
    # updateRadioGroupButtons(session,'radio_cogs',selected = last_btn$equal[1])

    vals$hand_save<-"Save changes"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(
      hand_save_modal()
    )

  })
  get_datanewID<-reactive({

    data_o<-data<-vals$saved_data[[input$char_data]]
    factors<-attr(data,"factors")[rownames(data),,drop=F]
    coords<-attr(data,"coords")[rownames(data),,drop=F]
    base_shape<-attr(data,"base_shape")
    layer_shape<-attr(data,"layer_shape")
    extra_shape<-attr(data,"extra_shape")

    df<-char_picvar()
    vals$char_picvar<-df
    new_ids<-as.vector(apply(data.frame(df),1,function(x){
      paste(x,collapse=input$char_paste_collapse)
    }))
    req(length(new_ids)==nrow(data))

    if(length(data)>0){
      rownames(data)<-make.unique(new_ids)
    }

    if(length(factors)>0){
      rownames(factors)<-make.unique(new_ids)
    }

    if(length(coords)>0){
      rownames(coords)<-make.unique(new_ids)
    }




    attr(data,"factors")<-factors
    attr(data,"coords")<-coords
    attr(data,"base_shape")<-base_shape
    attr(data,"layer_shape")<-layer_shape
    attr(data,"extra_shape")<-extra_shape
    data

  })
  icons<-function(x) {lapply(x,function(x){tags$div(tags$strong(x))})}
  icon_list<-function(x){
    lapply(
      x,
      function(x) {
        tags$span(style="border: 1px solid gray",
                  tags$strong(x)
        )
      }
    )
  }
  icon_list<-function(x){
    lapply(
      x,
      function(x) {
        tags$span(style="border: 1px solid gray; padding: 2px;
                  border-radius: 3px 3px 3px 3px;
                  background: #A8E3C099;
                  ",
                  tags$strong(x)
        )
      }
    )
  }
  icon_list2<-function(x){
    lapply(
      x,
      function(x) {
        tags$span(style="border: 1px solid gray; padding: 2px;
                  border-radius: 3px 3px 3px 3px;
                  background: #E8232349;
                  ",
                  tags$strong(x)
        )
      }
    )
  }
  icon_list3<-function(x){
    lapply(
      x,
      function(x) {
        tags$span(style="border: 1px solid gray; padding: 2px;
                  border-radius: 3px 3px 3px 3px;
                  background: lightblue;
                  ",
                  tags$strong(x)
        )
      }
    )
  }
  output$tochar_form6<-renderUI({

    req(input$char_fun=="for")
    req(input$char_col)
    column(12,
           div(strong("5. build the formula")),

           fluidRow(
             class = "panel panel-heading",

             fluidRow(
               class = "panel-body",
               column(
                 width = 4,
                 tags$div(
                   class = "panel panel-default",
                   tags$div(
                     icon("arrow-right"),
                     "Drag the variables (items will clone)"
                   ),
                   tags$div(
                     class = "panel-body",
                     id = "char_dragvar",
                     icon_list(c(input$char_col))
                   )
                 )
               ),
               column(
                 width = 4,
                 tags$div(
                   class = "panel panel-default",
                   tags$div(
                     icon("arrow-right"),
                     "Drag the operators(items will clone)"
                   ),
                   tags$div(
                     class = "panel-body",
                     id = "char_dragope",
                     icon_list2(c("+","-","*","/","(",")","sqrt",'^'))
                   )
                 )
               ),
               column(
                 width = 4,
                 tags$div(
                   class = "panel panel-default",
                   tags$div(
                     icon("arrow-right"),
                     "Drag the numbers (items will clone)"
                   ),
                   tags$div(
                     class = "panel-body",
                     id = "char_dragnum",
                     icon_list3(c(0:9,"pi"))
                   )
                 )
               ),

               column(
                 width = 12,
                 # analyse as x
                 tags$div(
                   class = "panel panel-default",
                   tags$div(

                     icon("exchange"),
                     "To here"
                   ),
                   tags$div(
                     class = "panel-body",
                     id = "char_drop"
                   )
                 ),


               ),
               column(
                 width = 12,
                 # bin
                 tags$div(
                   class = "panel panel-default",
                   tags$div(

                     icon("trash"),
                     "Drop here to Remove a item"
                   ),
                   tags$div(
                     class = "panel-body",
                     id = "sortable_bin"
                   )
                 )

               )
             )
           ),
           div(uiOutput("sortresult")),
           sortable_js(
             "char_dragvar",
             options = sortable_options(
               group = list(
                 pull = "clone",
                 name = "sortGroup1",
                 put = FALSE
               ),
               # swapClass = "sortable-swap-highlight",
               onSort = sortable_js_capture_input("sort_vars")
             )
           ),
           sortable_js(
             "char_dragope",
             options = sortable_options(
               group = list(
                 pull = "clone",
                 name = "sortGroup1",
                 put = FALSE
               ),
               # swapClass = "sortable-swap-highlight",
               onSort = sortable_js_capture_input("sort_vars")
             )
           ),
           sortable_js(
             "char_dragnum",
             options = sortable_options(
               group = list(
                 pull = "clone",
                 name = "sortGroup1",
                 put = FALSE
               ),
               # swapClass = "sortable-swap-highlight",
               onSort = sortable_js_capture_input("sort_vars")
             )
           ),
           sortable_js(
             "char_drop",
             options = sortable_options(
               group = list(
                 group = "sortGroup1",
                 put = T,
                 pull = TRUE
               ),
               swapClass = "sortable-swap-highlight",
               onSort = sortable_js_capture_input("sort_x")
             )
           ),

           sortable_js(
             "sortable_bin",
             options = sortable_options(
               group = list(
                 group = "sortGroup1",
                 put = TRUE,
                 pull = TRUE
               ),
               onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
             )
           )
    )
  })
  char_picvar<-reactive({
    req(input$char_col)
    req(length(input$char_col)>0)
    req(input$char_att)
    if(input$char_fun=="con"){
      #req(input$char_paste_collapse)
    }

    df<-char_df()
    req(length(colnames(df))>0)
    req(input$char_col%in%colnames(df))
    res<-df[input$char_col]
    attr(res,"char_attrib")<-input$char_att

    res
  })
  output$tochar_find6<-renderUI({
    req(input$char_fun)
    req(input$char_fun=="fr")
    req(length(char_replacements())>0)

    column(4,
           div(strong("Pattern results")),
           id="char_found",
           style="width: 200px; overflow-x: scroll;height:120px;overflow-y: scroll;",
           inline(DT::dataTableOutput('char_founds'))
    )




  })
  output$tochar_buildcols_df<-renderUI({
    res<-vals$char_newcols
    req(length(res)>0)
    div(
      style="width: 200px; overflow-x: scroll;height:120px;overflow-y: scroll;",
      inline(DT::dataTableOutput('char_newcols'))
    )
  })
  output$char_newcols<-DT::renderDT({
    res<-vals$char_newcols
    DT::datatable(res, editable = TRUE, options=list(dom = 't',ordering=F), rownames=T)
  })
  observeEvent(ignoreInit = T,input$char_newcols_cell_edit, {
    row <-input$char_newcols_cell_edit$row
    clmn<-input$char_newcols_cell_edit$col
    value<-input$char_newcols_cell_edit$value
    vals$char_newcols[row, clmn]<-value
  })
  observe({
    req(input$char_att)
    req(input$char_data)
    req(input$char_fun)
    if(input$char_fun=="for"){
      vals$char_newcols<-data.frame("New_names:"="Formula")

    } else if(input$char_fun=="fr"){
      try({
        req(length(input$char_pat)>0)
        df<-char_replacements()
        req(length(df)>0)
        res<-data.frame(patterns_found=df)
        vals$char_newcols<-data.frame("New_names:"=rownames(res))

      })
    } else if(input$char_fun=="con"){
      try({

        df<-char_picvar()
        req(is.data.frame(df))
        #req(input$char_paste_collapse)
        req(input$char_att=="Factor")
        vals$char_newcols<-data.frame("New_names:"=paste(colnames(df), collapse=input$char_paste_collapse))

      })
    }
  })
  output$char_founds<-DT::renderDataTable({
    resfound<-char_replacements()
    req(length(resfound)>0)
    res<-data.frame(resfound)



    #colnames(res)<-NULL

    DT::datatable(res, colnames = rep("", ncol(res)),options=list(dom = 't',ordering=F))
  })
  #datatable(res, colnames = rep("", ncol(emrespty)),options=list(ordering=F))
  char_df<-reactive({

    req(input$char_data)
    req(input$char_att)
    req(input$char_fun)
    datalist<-vals$saved_data[[input$char_data]]
    if(input$char_att=="Numeric"){
      res<-datalist
    } else if(input$char_att=="Factor" ){
      res<-attr(datalist,"factors")
    }
    res
  })


  observeEvent(ignoreInit = T,input$dcogs8,{
    showModal(
      modalDialog(
        title="Edit Model names",
        easyClose = T,
        div(radioButtons("modeledit_choice",NULL, c("Edit names","Merge models")),
            uiOutput("modelnameedit"),
            uiOutput("modelmerge")
        ),
        footer=div(modalButton("Cancel"), inline(uiOutput("confirm_editmodel")))
      )
    )
  })
  output$modelmerge<-renderUI({
    req(input$modeledit_choice=="Merge models")
    div(
      div(div(strong("1. Select the models to merge:")),
          inline(DT::DTOutput("merge_model_tab"))),
      uiOutput("selected_models"),

      tags$div(style="max-width: 250px",pickerInput("editmodel_targ_merge","2. Select the target Datalist:",names(vals$saved_data), selected=vals$cur_data))
    )
  })
  observeEvent(ignoreInit = T,input$confirm_newmodelnames,{

    req(input$modeledit_choice=="Merge models")

    saved_data<-vals$saved_data

    models<-getmodelist(saved_data)
    modeltab<-getmodelist_names(saved_data)
    selected<-input$merge_model_tab_rows_selected
    names_org<-which(modeltab$Orig_Datalist==input$editmodel_targ_merge)



    attrs<-unique(modeltab[input$merge_model_tab_rows_selected,2])
    i=1
    for(i in 1:length(attrs)){
      pic<-modeltab[input$merge_model_tab_rows_selected,2]==attrs[i]
      if(length(pic)>0){
        newmodels<-models[selected]
        newmodels<-newmodels[which(pic)]
        names(newmodels)<-make.unique(c(modeltab$model[names_org],modeltab$model[selected]))[-names_org][which(pic)]
        if(!is.null( attrs[i])){
          attr(vals$saved_data[[input$editmodel_targ_merge]],  attrs[i])<-    newmodels
        }
      }

    }

    removeModal()
  })
  output$selected_models<-renderUI({
    div(
      strong("selected models:"),
      renderPrint({
        input$merge_model_tab_rows_selected
      })

    )
  })
  output$merge_model_tab<-DT::renderDT({
    vals$merge_models<- getmodelist_names(vals$saved_data)
    DT::datatable(  vals$merge_models   , editable = TRUE, options=list(info = FALSE,lengthMenu = list(c(-1), c("All")), autowidth=F,dom = 't'))
  })
  output$modelnameedit<-renderUI({
    req(input$modeledit_choice=="Edit names")
    div(
      div(inline(uiOutput("editmodel_target")),inline(uiOutput("editmodel_attrs"))),
      div(div(strong("3. Edit Names:", tiphelp("Double-click the model name to edit it"))),
          inline(DT::DTOutput("editmodel_model")))



    )})
  output$confirm_editmodel<-renderUI({
    actionButton("confirm_newmodelnames","4. Confirm")
  })
  observeEvent(ignoreInit = T,input$confirm_newmodelnames,{
    req(input$modeledit_choice=="Edit names")
    names(attr(vals$saved_data[[input$editmodel_targ]],input$editmodel_at))<- vals$oldmodelsnames[,1]
    vals$cur_data<-input$editmodel_targ
    vals$oldmodelsnames<-NULL
    #removeModal()
  })
  observeEvent(list(vals$saved_data,input$editmodel_targ,input$editmodel_at),{
    req(input$editmodel_targ)
    req(input$editmodel_at)
    vals$oldmodelsnames<-data.frame('Model Name'=names(attr(vals$saved_data[[input$editmodel_targ]],input$editmodel_at)))
  })
  output$editmodel_model<-DT::renderDT({
    DT::datatable(vals$oldmodelsnames, editable = TRUE, options=list(info = FALSE,lengthMenu = list(c(-1), c("All")), autowidth=F,dom = 't'))
  })
  observeEvent(ignoreInit = T,input$editmodel_model_cell_edit, {
    row <-input$editmodel_model_cell_edit$row
    clmn<-input$editmodel_model_cell_edit$col
    vals$oldmodelsnames[row, clmn]<-input$editmodel_model_cell_edit$value
  })

  observeEvent(ignoreInit = T,input$editmodel_targ,{
    vals$cur_data<-input$editmodel_targ
  })
  output$editmodel_target<-renderUI({
    tags$div(style="max-width: 250px",pickerInput("editmodel_targ","1. Select the target Datalist:",names(vals$saved_data), selected=vals$cur_data))
  })
  output$editmodel_attrs<-renderUI({
    req(length(input$editmodel_targ)>0)
    data<-vals$saved_data[[input$editmodel_targ]]
    choices<-list(
      if(!is.null(attr(data,"som"))){"som"},
      if(check_model0(data,"rf")){"rf"},
      if(check_model0(data,"nb")){"nb"},
      if(check_model0(data,"svm")){"svm"},
      if(check_model0(data,"knn")){"knn"},
      if(check_model0(data,"sgboost")){"sgboost"}

    )
    choices<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]
    tags$div(style="max-width: 200px",pickerInput("editmodel_at","2. Model-Attribute",choices=choices, selected=vals$editmodel_at))

  })
  observeEvent(ignoreInit = T,input$editmodel_at,{
    vals$editmodel_at<-input$editmodel_at
  })
  observeEvent(ignoreInit = T,input$editmodel_pick,{
    vals$editmodel_pick<-input$editmodel_pick
  })
  observeEvent(ignoreInit = T,input$dcogs4,{
    showModal(
      modalDialog(
        title="Relace Factor-Attribute",
        easyClose = T,
        size="m",
        uiOutput("newfactors_page"),
        footer=div(modalButton("Cancel"), inline(uiOutput("confirm_newfac")))
      )
    )
  })
  output$newfactors_page<-renderUI({
    div(
      uiOutput("newfactors_target"),
      fileInput("newfactorfile", label="2. Upload a file:",accept = c(".xls",".xlsx",".csv")),
      uiOutput("newfactors_missing")
    )
  })

  observeEvent(ignoreInit = T,input$picker_bucked_cols,
               output$bucked_cols<-renderUI({}))
  observeEvent(vals$saved_data,{
    choices_a<-lapply(vals$saved_data, function (x) colnames(x))
    choices_b<-lapply(vals$saved_data, function (x) NULL)
    vals$choices_bag<-mapply(list, choices_a,choices_b, SIMPLIFY=FALSE)


  })
  observeEvent(vals$choices_bag,{
    choices_togther<-do.call(c,lapply(vals$choices_bag, function (x) x[[2]]))


    vals$choices_togther<-choices_togther

  })
  output$merge_button<-renderUI({
    div( inline(uiOutput("merge_btn_cancel")),
         inline(uiOutput("merge_btn_prev")),

         inline(uiOutput("merge_btn_next")),
         inline(uiOutput("merge_btn_end")))
  })
  observeEvent(ignoreInit = T,input$merge_datalist_cancel,
               removeModal())
  observeEvent(ignoreInit = T,input$editfac,{
    bag_newfac$df<-0
  })
  observe({
    req(bag_newfac$df==1)
    factors<-attr(vals$saved_data[[input$editfac]],"factors")
    req(length(factors)>0)
    cols<-colnames(factors)
    req(length(input$select_factor)>0)
    lapply(cols,input=input,function(x,input){
      output[[paste("newcolfac",x)]]<-renderUI({
        div(class="cutfacs_breaks_form0",
            if(x%in%input$select_factor){
              textInput(paste("newcolfac",x, sep="_"), NULL, value=x,width="200px")
            }else{ div(x,style="color:red; text-decoration: line-through;")

            })
      })
    })
    bag_newfac$df<-2
  })
  observe({
    req(bag_newfac$df==2)
    req(length(input$select_factor)>0)
    factors<-attr(vals$saved_data[[input$editfac]],"factors")
    if(any(input$select_factor%in%colnames(factors))){
      try({

        newfac<-factors[input$select_factor]
        colnames(newfac)<-do.call(c,lapply(paste("newcolfac",colnames(newfac),sep="_"), function (x) input[[x]]))
        vals$edifac<-newfac

      }, silent=T)
    }
    bag_newfac$df==0
  })
  observeEvent(ignoreInit = T,input$editfac_go,{
    attr(vals$saved_data[[input$editfac]],"factors")<-vals$edifac
    shinyjs::reset("edifac_inps")
    bag_newfac$df<-0
    vals$cur_data<-input$editfac
    removeModal()
  })
  observeEvent(ignoreInit = T,input$editfac_end,{
    bag_newfac$df<-0
    removeModal()
  })
  observeEvent(ignoreInit = T,input$dcogs6,{
    vals$editdata<-vals$cur_data
    showModal(
      edit_data()
    )
  })
  observeEvent(ignoreInit = T,input$editattr,{
    vals$editattr<-input$editattr
  })
  observeEvent(ignoreInit = T,input$editdata,{
    vals$editdata<-input$editdata
  })
  output$editdat_datalist<-renderUI({

    div(
      inline(pickerInput("editdata","1. Select the Datalist:",choices=names(vals$saved_data), width="250px",selected = vals$editdata)),
      inline(pickerInput("editattr","2. Select the attribute to be edited",choices=c("Factors","Numeric"), width="250px", selected=vals$editattr))
    )
  })
  edit_data<- reactive({
    modalDialog(easyClose = T,
                title="Edit data:",
                footer = uiOutput("editdat_btns"),
                uiOutput("editdata_page")


    )
  })
  output$editdata_page<-renderUI({
    div(id="edidat_inps",
        uiOutput("editdat_datalist"),
        uiOutput("editdat_cols")

    )
  })

  output$editdat_btns<-renderUI({
    div(
      actionButton("editdat_go",strong("4. Confirm")),
      modalButton("Cancel")
    )
  })
  # output read checkboxes
  output$editdat_cols<-renderUI({
    div(div(strong("3. Double-click the variable names to edit them:")),
        inline(DT::DTOutput("tabcolnames"))
    )
  })

  getRemoveButton<-function(n, idS = "", lab = "Pit") {
    if (stringr::str_length(idS) > 0) idS<-paste0(idS, "-")
    ret<-shinyInput(actionLink, n,
                    'button_', label = icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),
                    onclick = sprintf('Shiny.onInputChange(\"%sremove_button_%s\",  this.id)' ,idS, lab), style="font-size: 15px")
    return (ret)
  }
  jssss<-c(
    "table.on('key', function(e, datatable, key, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  if(key == 13 && targetName == 'body'){",
    "    $(cell.node()).trigger('dblclick.dt');",
    "  }",
    "});",
    "table.on('keydown', function(e){",
    "  var keys = [9,13,37,38,39,40];",
    "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
    "    $(e.target).trigger('blur');",
    "  }",
    "});",
    "table.on('key-focus', function(e, datatable, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  var type = originalEvent.type;",
    "  if(type == 'keydown' && targetName == 'input'){",
    "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
    "      $(cell.node()).trigger('dblclick.dt');",
    "    }",
    "  }",
    "});"
  )

  shinyInput<-function(FUN, n, id, ses, ...) {
    as.character(FUN(paste0(id, n), ...))
  }
  delcol_values<-reactiveValues()
  output$tabcolnames<-DT::renderDT(
    data.frame(delcol_values$tab[,-c(2:3)]),
    escape = FALSE,
    editable=T,
    options = list(
      pageLength = 10,
      displayStart = vals$previousPage,
      info = FALSE,
      lengthMenu = list(c(-1), c("All")),
      dom = 'tp'
    ),

    selection = list(mode = "single", target = "row", selected = vals$previousSelection),
    class ='cell-border compact stripe'
  )
  observeEvent(ignoreInit = T,input$remove_button_Tab1, {
    tabcolnames<-delcol_values$tab
    s<-as.numeric(strsplit(input$remove_button_Tab1, "_")[[1]][2])
    pic<-tabcolnames$id!=s
    tabcolnames<- tabcolnames[pic,, drop=F]

    #colnames(attr(delcol_values$tab,'data'))<-tabcolnames[pic,1]
    #DT::replaceData(proxyTable, tabcolnames, resetPaging = FALSE)
    delcol_values$tab<-tabcolnames
    vals$updeldata<-pic
  })
  observeEvent(vals$updeldata,{
    req(length(vals$updeldata)>0)
    pic<-vals$updeldata
    vals$updeldata<-NULL
    data<-attr(delcol_values$tab,'data')[,pic, drop=F]
    colnames(data)<-delcol_values$tab[,1]
    attr(delcol_values$tab,'data')<- data
  })
  observeEvent(ignoreInit = T,input$editdata,{
    vals$previousSelection<-NULL
    vals$previousPage<-NULL
  })
  observeEvent(ignoreInit = T,input$editattr,{
    vals$previousSelection<-NULL
    vals$previousPage<-NULL
  })
  observeEvent(list(vals$saved_data,input$editdata,input$editattr),{


    req(input$editdata)
    data<-vals$saved_data[[input$editdata]]
    req(input$editattr)
    data<-switch(input$editattr,
                 'Numeric'=data,
                 "Factors"=attr(data,"factors"))

    datavars<-vals$olddatnames<-data.frame(Var_name=colnames(data))
    delcol_values$tab<-data.frame(datavars,data.frame(Row=1:nrow(datavars), id=1:nrow(datavars)),Remove=do.call(rbind,lapply(1:nrow(datavars),function(id) {
      getRemoveButton(id, idS = "", lab = "Tab1")
    })))
    delcol_values$row<-0
    attr(delcol_values$tab,'data')<-data



  })
  Clicked<-eventReactive(input$tabcolnames_rows_selected,{
    input$tabcolnames_rows_selected
  })

  observeEvent(ignoreInit = T,input$tabcolnames_cell_edit, {
    vals$previousSelection<-input$tabcolnames_rows_selected
    vals$previousPage<-input$tabcolnames_rows_current[1]-1

    row <-input$tabcolnames_cell_edit$row
    clmn<-input$tabcolnames_cell_edit$col
    #delcol_values$tab[row, clmn]<-input$tabcolnames_cell_edit$value
    delcol_values<-delcol_values
    delcol_values$tab[row, clmn]<-input$tabcolnames_cell_edit$value
    newdf<- attr(delcol_values$tab,'data')
    colnames(newdf)<-delcol_values$tab[,1]
    attr(delcol_values$tab,'data')<-newdf
    #delcol_values$row<-input$tabcolnames_rows_selected-4

  })
  observeEvent(ignoreInit = T,input$editdat_go,{

    newdf<-attr(delcol_values$tab,'data')
    if(input$editattr=="Factors"){
      attr(vals$saved_data[[input$editdata]],"factors")<-newdf
    } else{
      data<-vals$saved_data[[input$editdata]]
      newdf<-data_migrate2(data,newdf, input$editdata)
      vals$saved_data[[input$editdata]]<-newdf
    }

  })
  output$del_datalist_choices<-renderUser({
    div(
      div(style="overflow-y: scroll;height: 400px;overflow-x: scroll; padding-top: 10px",
          checkboxGroupInput('del_datalist',NULL,names(vals$saved_data),width='550px')
      )
    )
  })
  output$removedatalist_out<-renderUI({
    div(
      div(checkboxInput("checkall_datlist",strong('Select/Unselect all'),F),
          uiOutput("del_datalist_choices")
      )
    )

  })
  observeEvent(ignoreInit = T,input$checkall_datlist,{
    if(isTRUE(input$checkall_datlist)){
      updateCheckboxGroupInput(session,
                               "del_datalist",NULL,
                               choices = names(vals$saved_data),
                               selected = names(vals$saved_data)
      )
    } else{
      updateCheckboxGroupInput(session,
                               "del_datalist",NULL,
                               choices = names(vals$saved_data)

      )
    }

  })
  observeEvent(ignoreInit = T,input$dcogs7,{
    showModal(
      modalDialog(size="m",easyClose = T,
                  title=strong("Remove Datalists"),
                  div(uiOutput("removedatalist_out"),
                      bsButton("delete_datalist",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                  )
      )
    )

  })

  observeEvent(ignoreInit = T,input$rename_datalist,{vals$rename_datalist<-input$rename_datalist})
  observeEvent(ignoreInit = T,input$dcogs1,{
    showModal(
      modalDialog(size="m",easyClose = T,footer =NULL,
                  title="Rename Datalist",
                  div(
                    div(
                      span(
                        inline(
                          pickerInput("rename_datalist","Datalist:",choices=names(vals$saved_data), width="200px", selected=vals$rename_datalist)
                        ),
                        inline(uiOutput("rename_datalist_newname")),
                        bsButton("dcogs1_rename_go","Rename"), modalButton("Dismiss")
                      )
                    )
                  )
      )
    )
  })
  output$rename_datalist_newname<-renderUI({
    textInput("dcogs1_rename", "New name:",value=input$rename_datalist,
              placeholder = "New name", width="200px")
  })



  observeEvent(ignoreInit = T,input$dcogs1_rename_go,{
    req(input$dcogs1_rename!="")
    names(vals$saved_data)[which(names(vals$saved_data)==input$rename_datalist)]<-input$dcogs1_rename
    vals$rename_datalist<-input$dcogs1_rename
    updatePickerInput(session, "rename_datalist","Datalist:",choices=names(vals$saved_data),selected=vals$rename_datalist)



  })
  observeEvent(ignoreInit = T,input$import_from_data,{
    vals$cur_import_from_data<-input$import_from_data
  })
  observeEvent(ignoreInit = T,input$import_from_attr,{
    vals$cur_import_from_attr<-input$import_from_attr
  })
  observeEvent(ignoreInit = T,input$import_to_attr,{
    vals$cur_import_to_attr<-input$import_to_attr
  })
  observeEvent(ignoreInit = T,input$dcogs2,{
    merge_rv$page_merge<-1
    showModal(
      modalDialog(size="m",easyClose = T,footer =uiOutput("merge_button"),
                  title=span("Merge Datalists",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Merge columns/rows from different Datalists. This action affects Numerical-, Factor- and Coords- Attribute (if exists).")),
                  uiOutput("merge_page_merges"))
    )
  })
  merge_rv<-reactiveValues(page_merge = 1)
  output$dcogs2_step1<-renderUI({
    div(

      div(
        strong("1. Select the Datalists"),
        uiOutput("merge_cbind_out")
      ),
      splitLayout(style="padding-top: 15px",
                  div(
                    div(
                      strong("2. Merge type:"),
                      inline( pickerInput("merge_cr",NULL,c("columns","rows"), inline=T)), inline(uiOutput("merge_fill"))
                    ),
                    div(
                      strong("3. "),
                      inline(uiOutput("rm_dupcol"))
                    )
                  ),

      )
    )
  })
  output$dcogs2_step2<-renderUI({
    div(
      uiOutput("to_mergeprint")
    )
  })
  merge_npage<-2
  output$merge_page_merges<-renderUI({
    div(id='merge_inputs',
        div(class = "page_merge",
            id = 'step1',
            uiOutput("dcogs2_step1")),
        hidden(
          div(class = "page_merge",id = 'step2',
              uiOutput("dcogs2_step2")))
    )
  })
  output$merge_btn_prev<-renderUI({
    req(merge_rv$page_merge!=1)
    bsButton("prev_merge", "< Previous", class="button_change")
  })
  output$merge_btn_cancel<-renderUI({
    req(merge_rv$page_merge==1)
    bsButton("cancel_merge", "Cancel", class="button_change")
  })
  output$merge_btn_end<-renderUI({
    req(merge_rv$page_merge==merge_npage)
    bsButton("merge_datalist_go", "Next >", class="button_change")
  })
  output$merge_btn_next<-renderUI({
    req(merge_rv$page_merge<merge_npage)
    #req(length(input$mergevar)>=1)
    dis<-if(length(input$merge_cbind)>=1){F} else{ T}
    div(
      div(next_title=if(isTRUE(dis)){"Select at least two datalists"} else{ NULL},
          bsButton("next_merge", "Next >", class="button_change", disabled = dis),

      )
    )
  })
  observe({
    toggleState(id = "prev_merge", condition = merge_rv$page_merge > 1)
    toggleState(id = "page_merge1", condition = merge_rv$page_merge > 1)
    toggleState(id = "next_merge", condition = merge_rv$page_merge < merge_npage)
    toggleState(id = "page_merge2", condition = merge_rv$page_merge < merge_npage)
    hide(selector = ".page_merge")
    shinyjs::show(paste0("step", merge_rv$page_merge))
  })
  navpage_merge<-function(direction) {
    merge_rv$page_merge<-merge_rv$page_merge + direction
  }
  observeEvent(ignoreInit = T,input$cancel_merge,{
    removeModal()
  })
  observeEvent(ignoreInit = T,input$prev_merge, navpage_merge(-1))
  observeEvent(ignoreInit = T,input$next_merge, navpage_merge(1))
  output$merge_fill<-renderUI({
    req(input$merge_cr=='rows')
    inline(checkboxInput("fill_row", span("Fill", tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Fills missing columns with NAs. If not checked, restric data to common columns from the selected Datalists"))))
  })
  output$to_mergeprint<-renderUI({
    req(length(input$merge_cbind)>1)
    data=switch(input$merge_cr,
                "rows"=to_merge_datarow(),
                "columns"=to_merge_datacol())


    div(
      strong("New Data:"),
      renderPrint({

        withProgress(message = "Calculating Numeric-Attribute summary ... Please, wait!",
                     min = 1,
                     max = 13,
                     {

                       nas=sum(is.na(unlist(data)))
                       incProgress(1)

                       n=data.frame(rbind(Param=paste('Missing values:', nas)))
                       incProgress(1)
                       a<-data.frame(rbind(Param=paste('nrow:', nrow(data)),paste('ncol:', ncol(data))))
                       incProgress(1)


                       ppsummary("-------------------")
                       incProgress(1)
                       ppsummary(n)
                       ppsummary("-------------------")
                       incProgress(1)
                       ppsummary(a)
                       ppsummary("-------------------")
                       incProgress(1)


                     })


      })
    )
  })
  output$merge_cbind_out<-renderUI({
    div(id="tog_cbind",
        div(div(class="cogs_in",
                div(class="merge_datalist",
                    div(style="overflow-y: scroll;height: 150px;padding: 10px",
                        checkboxGroupInput("merge_cbind", NULL,choices=names(vals$saved_data))
                    )
                )
        )
        )

    )
  })
  output$rm_dupcol<-renderUI({

    div(checkboxInput("rm_dup", paste("Remove duplicated", input$merge_cr), T))
  })
  output$import_from<-renderUI({

    div(
      div(
        strong(inline(div("Datalist:", style="width: 70px"))),
        inline(
          div(
            div(strong( "From:")),
            div(pickerInput("import_from_data", NULL, choices=names(vals$saved_data),width="200px", selected=vals$cur_import_from_data))
          )
        )

      ),
      div(

        div(inline(div(strong("Attribute:"), style="width: 70px")),
            inline(pickerInput("import_from_attr", NULL, choices=c("Factor-Attribute","Numeric-Attribute"), width="200px", selected=vals$cur_import_from_attr))
        ),
        uiOutput("import_var")
      )

    )

  })
  observeEvent(ignoreInit = T,input$import_to_data,{
    vals$cur_import_to_data<-input$import_to_data
  })
  output$import_to<-renderUI({
    div(

      div(
        div(strong("To:")),
        div(pickerInput("import_to_data", NULL, choices=names(vals$saved_data),selected=vals$cur_import_to_data, width="200px"))

      ),
      div(pickerInput("import_to_attr", NULL, choices=c("Numeric-Attribute","Factor-Attribute"), width="200px", selected=vals$cur_import_to_attr)),
      div(
        uiOutput('factonum_opt')
      )
    )
  })

  observeEvent(ignoreInit = T,input$hand_facs,{
    vals$cur_hand_facs<-input$hand_facs
  })
  output$factonum_opt<-renderUI({
    req(input$import_from_attr=="Factor-Attribute"&input$import_to_attr=="Numeric-Attribute")

    div(
      div(span(strong("Convertion type:"))),
      radioButtons("hand_facs",NULL,
                   choiceValues = list("Binary","Ordinal"),selected=vals$cur_hand_facs,
                   choiceNames = list(
                     div("Binary",span(id="conv_bin",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"))),
                     div("Integer",span(id="conv_ord",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")))
                   ),
                   width="300px"),
      bsTooltip("conv_bin","Creates a single binary column per factor level,with 1 indicating the class of that particular observation", placement = "right",options =list(style="width: 600px")),
      bsTooltip("conv_ord","Creates a single column using numeric (integer) representation (values) of the factor levels", placement = "right")
    )
  })
  observeEvent(ignoreInit = T,input$import_from_attr,{
    if(input$import_from_attr=="Factor-Attribute"& input$import_to_attr=="Factor-Attribute")
      vals$cutfacs<-F
  })
  get_factor_cuts<-function(l1){
    do.call(data.frame,lapply(l1, function (x) {
      validate(need(x[[2]]>1,"Wrong length"))
      cut(as.numeric(as.character(x[[1]])),as.numeric(x[[2]]))

    }))
  }
  output$toorder_in<-renderUI({

    data<-getconvert_datavars()[[1]]
    vars<-getconvert_datavars()[[2]]
    l1<-lapply(seq_along(vars), function(x) {
      inline(div(div(
        if(isTRUE(vals$cutfacs)) {div("Cut:",class="cutfacs_breaks_form",
                                      inline(textInput(paste0("cutfacs_breaks_",x),NULL,value =nlevels(data[[vars[[x]]]]),width="60px")),width="60px"
        )} else {
          div("levels:",class="cutfacs_breaks")
        },style="width: 100px;height: 25px; background: #c7d3d4ff; margin: 0px; padding: 0px"),
        uiOutput(paste0('facord_outl',x))

      ))
    })
    l2<-lapply(vars,function(x) uiOutput(paste0("facinteger",x)))
    l2<-lapply(l2, function (x) inline(div(x, style="margin-top")))
    l3<-mapply(list, l2,l1,vars, SIMPLIFY=FALSE)
    l4<-lapply(l3,function(x)
      inline(div(div(class="cutfacs_breaks_form0",inline(textInput(paste0("newcol",x[[3]],sep="_"),NULL,value=x[[3]], width="150px"))),
                 div(inline(div(
                   div("Value",style="height: 28px;width: 80px; background: #c7d3d4ff; margin: 0px; padding: 0px"),x[[1]])),x[[2]]), class='align_top'))
    )
    new_style<-function(...,class='align_top'){
      return(div(...,class=class))
    }

    div(class='align_top',id="rank_levels",
        l4
    )
  })
  cutdata<-reactive({
    req(input$import_from_attr)
    req(input$import_to_attr)
    cuts<-NULL
    if(input$import_from_attr=="Numeric-Attribute" & input$import_to_attr=="Factor-Attribute"){
      vars<-getconvert_datavars()[[2]]
      cuts<-lapply(seq_along(vars), function(x)    input[[paste0("cutfacs_breaks_",x)]])}
    cuts
  })
  observeEvent(ignoreInit = T,input$cutfacs,{
    vals$cutfacs<-input$cutfacs
  })
  ordlabels<-reactive({
    data<-getconvert_datavars()[[1]]
    vars<-getconvert_datavars()[[2]]



    if(input$import_from_attr=="Numeric-Attribute" & input$import_to_attr=="Factor-Attribute"){

      if(isTRUE(input$cutfacs)){
        if(sum(unlist(lapply(seq_along(vars), function(x)    length(input[[paste0("cutfacs_breaks_",x)]])>0)))==length(vars)){
          cuts<-lapply(seq_along(vars), function(x)    input[[paste0("cutfacs_breaks_",x)]])
          l1<-mapply(list,as.list(data[input$importvar]),cuts,SIMPLIFY = F)
          data<-get_factor_cuts(l1)

        }else{

          data<-do.call(data.frame,lapply(data[input$importvar],function(x) as.factor(x)))

        }
      } else{
        data<-do.call(data.frame,lapply(data[input$importvar],function(x) as.factor(x)))
      }
    }
    l2<-lapply(seq_along(vars), function (x)
      lapply( as.list(levels(data[input$importvar][[x]])),function(xx){
        c("ordrank_label",x,xx)
      })

    )
    res_labels<-lapply(l2,input=input,function(x,input) unlist(lapply(x, function(xx){
      input[[paste0(xx, collapse = "_")]]
    })))
    res_labels
  })
  ordvalues<-reactive({
    data<-getconvert_datavars()[[1]]
    vars<-getconvert_datavars()[[2]]
    if(input$import_from_attr=="Numeric-Attribute" & input$import_to_attr=="Factor-Attribute"){
      req(input$cutfacs)
      if(isTRUE(input$cutfacs)) {
        if(sum(unlist(lapply(seq_along(vars), function(x)    length(input[[paste0("cutfacs_breaks_",x)]])>0)))==length(vars)){
          cuts<-lapply(seq_along(vars), function(x)    input[[paste0("cutfacs_breaks_",x)]])
          l1<-mapply(list,as.list(data[input$importvar]),cuts,SIMPLIFY = F)
          data<-get_factor_cuts(l1)
        }else{

          data<-do.call(data.frame,lapply(data[input$importvar],function(x) as.factor(x)))

        }

      } else{
        data<-do.call(data.frame,lapply(data[input$importvar],function(x) as.factor(x)))

      }
    }

    l2<-lapply(vars, function (x)
      lapply(1:nlevels(data[,x]),function(xx){
        c("facvalue",x,xx)
      })

    )
    res_values<-lapply(l2,input=input,function(x,input) unlist(lapply(x, function(xx){input[[paste0(xx, collapse = "")]]})))

  })
  final_import<-reactive({
    data<-getconvert_datavars()[[1]]
    vars<-getconvert_datavars()[[2]]
    res_labels<-ordlabels()
    if(input$import_from_attr=="Numeric-Attribute" & input$import_to_attr=="Factor-Attribute"){
      if(isTRUE(vals$cutfacs)) {
        req(sum(unlist(lapply(seq_along(vars), function(x)    length(input[[paste0("cutfacs_breaks_",x)]])>0)))==length(vars))
        cuts<-lapply(seq_along(vars), function(x)    input[[paste0("cutfacs_breaks_",x)]])
        l1<-mapply(list,as.list(data[input$importvar]),cuts,SIMPLIFY = F)
        df<-get_factor_cuts(l1)
        rownames(df)<-rownames(data)
        data<-df
      }
    }


    df_to<-if(input$import_to_attr=="Numeric-Attribute"){vals$saved_data[[input$import_to_data]]} else{attr(vals$saved_data[[input$import_to_data]],'factors')}

    data<-data[rownames(df_to),, drop=F]
    if(input$import_from_attr=="Numeric-Attribute"){
      newvars<-data[input$importvar]
      if(input$import_to_attr=="Numeric-Attribute"){
        new<-df_to

        newv<-as.list(newvars[rownames(df_to),, drop=F])
        names(newv)<-do.call(c,lapply(paste("newbin",names(newv),sep="_"), function (x) input[[x]]))
        new[names(newv)]<-newv

      }

      if(input$import_to_attr=="Factor-Attribute"){

        new<-df_to
        l1<-as.list(newvars[rownames(df_to),input$importvar,drop=F])
        l2<-lapply(seq_along(vars), function(x) {input[[paste0("rank_lev_",x,collapse="_")]]})

        l3<-mapply(list, l1,l2,res_labels, SIMPLIFY=FALSE)

        #newvars<-do.call(data.frame,lapply(l3,function(x) factor(x[[1]], levels=x[[2]])))
        l4<-lapply(l3,input=input,function(x, input) {
          res<-factor(x[[1]], levels=x[[2]], labels=x[[3]])
          if(anyNA(x[[1]])){
            res<-addNA(res)
          }
          res
        })
        names(l4)<-do.call(c,lapply(paste0('newcol',input$importvar,sep="_"), function (x) input[[x]]))
        new[names(l4)]<-l4
      }

    }


    if(input$import_from_attr=="Factor-Attribute"){
      newvars<-data[input$importvar]
      if(any(unlist(lapply(newvars,function(x) !is.factor(x))))){
        pic<-which(unlist(lapply(newvars,function(x) !is.factor(x))))
        newvars[pic]<-do.call(data.frame,lapply(newvars[pic],function(x) as.factor(x)))
      }
      if(input$import_from_attr=="Factor-Attribute"){
        if(input$import_to_attr=="Numeric-Attribute"){
          if(input$hand_facs=="Binary"){
            newv<-getclassmat(data[,input$importvar,drop=F])
            rownames(newv)<-rownames(data)
            names<-paste("newbin",colnames(newv),sep="_")
            colnames(newv)<-do.call(c,lapply(paste("newbin",colnames(newv),sep="_"), function (x) input[[x]]))
            newvars<-newv
            new<-df_to
            new[,colnames(newv)]<-newv[rownames(df_to),,drop=F]



          }
          if(input$hand_facs=="Ordinal"){


            new<-df_to
            res_values<-ordvalues()

            l1<-as.list(newvars[rownames(df_to),input$importvar,drop=F])
            l2<-lapply(seq_along(vars), function(x) {input[[paste0("rank_lev_",x,collapse="_")]]})
            #lvalue<-lapply(seq_along(vars), function(x) {input[[paste0("facvalue",x)]]})


            l3<-mapply(list, l1,l2,res_values, SIMPLIFY=FALSE)

            #x<-l3[[1]]
            #newvars<-do.call(data.frame,lapply(l3,function(x) factor(x[[1]], levels=x[[2]])))
            l4<-lapply(l3,input=input,function(x, input) {
              res<-as.numeric(as.character(factor(x[[1]], levels=x[[2]], labels=x[[3]])))

              res
            })
            names(l4)<-do.call(c,lapply(paste0('newcol',input$importvar,sep="_"), function (x) input[[x]]))

            new[names(l4)]<-l4

          }

        }
      }

      if(input$import_to_attr=="Factor-Attribute"){
        new<-df_to
        l1<-as.list(newvars[rownames(df_to),input$importvar,drop=F])
        l2<-lapply(seq_along(vars), function(x) {input[[paste0("rank_lev_",x,collapse="_")]]})
        l3<-mapply(list, l1,l2,res_labels, SIMPLIFY=FALSE)
        #newvars<-do.call(data.frame,lapply(l3,function(x) factor(x[[1]], levels=x[[2]])))
        l4<-lapply(l3,input=input,function(x, input) {
          res<-factor(x[[1]], levels=x[[2]], labels=x[[3]])
          if(anyNA(x[[1]])){
            res<-addNA(res)
          }
          res
        })
        names(l4)<-do.call(c,lapply(paste0('newcol',input$importvar,sep="_"), function (x) input[[x]]))
        new[names(l4)]<-l4


      }

    }

    return(new)


  })
  observeEvent(list(input$next_import,cutdata(), input$cutfacs), {
    req(input$import_from_attr)
    req(length(input$importvar)>0)
    req(input$import_from_attr)
    #req(input$import_from_attr=="Numeric-Attribute" & input$import_to_attr=="Numeric-Attribute")
    #req(input$import_to_attr=="Factor-Attribute")
    if(input$import_from_attr=="Factor-Attribute" & input$import_to_attr=="Numeric-Attribute" ){
      #req(input$hand_facs=="Ordinal")
    }
    data<-getconvert_datavars()[[1]]
    vars<-getconvert_datavars()[[2]]
    req(input$importvar %in% colnames(data))


    if(input$import_from_attr=="Numeric-Attribute" & input$import_to_attr=="Factor-Attribute" )
    {
      #data<-get_factors_exchange(data,vars, input)
      if(isTRUE(vals$cutfacs)){

        req(sum(unlist(lapply(seq_along(vars), function(x)    length(input[[paste0("cutfacs_breaks_",x)]])>0)))==length(vars))
        cuts<-lapply(seq_along(vars), function(x)    input[[paste0("cutfacs_breaks_",x)]])
        l1<-mapply(list,as.list(data[input$importvar]),cuts,SIMPLIFY = F)
        data<-get_factor_cuts(l1)
      }
      lapply(seq_along(vars),function(x){
        output[[paste0('facord_outl',x)]]<-renderUI({
          valid<-input[[paste0("cutfacs_breaks_",x)]]
          if(isTRUE(input$cutfacs)){
            validate(need(valid>1,"Wrong length"))
          }

          div(style="margin-top: 3px",
              rank_list(
                text = NULL,
                labels = lapply(as.list(levels(data[input$importvar][[x]])), function(xx) div(xx,inline(div(class='cutfacs_breaks_form2',textInput(paste("ordrank_label",x,xx,sep="_"),NULL,xx, width= "95px"))))),
                input_id = paste("rank_lev",x,sep="_"),
                css_id=paste("rankid_lev",x,sep="_"),
                class="rankfactors"
              )
          )
        })
      })

      lapply(vars,function(x) {
        output[[paste0("facinteger",x)]]<-renderUI({
          lapply(1:nlevels(data[,x]),function(x) div(x, class="rank_col"))
        })
      })
    }
    if(input$import_from_attr=="Factor-Attribute" & input$import_to_attr=="Factor-Attribute" )
    {
      lapply(seq_along(vars),function(x){
        output[[paste0('facord_outl',x)]]<-renderUI({
          div(style="margin-top: 3px",
              rank_list(
                text = NULL,
                labels = lapply(as.list(levels(data[input$importvar][[x]])), function(xx) div(xx,inline(div(class='cutfacs_breaks_form2',textInput(paste("ordrank_label",x,xx,sep="_"),NULL,xx, width= "95px"))))),
                input_id = paste("rank_lev",x,sep="_"),
                css_id=paste("rankid_lev",x,sep="_"),
                class="rankfactors"
              )
          )
        })
      })

      lapply(vars,function(x) {
        output[[paste0("facinteger",x)]]<-renderUI({
          lapply(1:nlevels(data[,x]),function(x) div(x, class="rank_col"))
        })
      })
    }

    if(input$import_from_attr=="Factor-Attribute" & input$import_to_attr=="Numeric-Attribute" )
    {

      lapply(seq_along(vars),function(x){
        output[[paste0('facord_outl',x)]]<-renderUI({
          div(style="margin-top: 3px",
              rank_list(
                text = NULL,
                labels = levels(data[input$importvar][[x]]),
                input_id = paste("rank_lev",x,sep="_"),
                css_id=paste("rankid_lev",x,sep="_"),
                class="rankfactors2"
              )
          )
        })
      })

      lapply(vars,function(x) {
        output[[paste0("facinteger",x)]]<-renderUI({
          lapply(1:nlevels(data[,x]),function(xx) div(textInput(paste0("facvalue",x,xx), NULL,value=xx,width="80px"), class="cutfacs_breaks_form"))
        })
      })
    }



    #lapply(input$importvar, function(x) textInput("label"))


    if(input$import_from_attr=="Factor-Attribute" & input$import_to_attr=="Factor-Attribute") {

    }

  })
  getconvert_datavars<-reactive({
    req(input$import_from_attr)
    req(input$importvar)
    req(input$import_from_attr)
    if(input$import_from_attr=="Factor-Attribute"){
      data<- attr(vals$saved_data[[input$import_from_data]],"factors")
      if(any(unlist(lapply(data,function(x) !is.factor(x))))){
        pic<-which(unlist(lapply(data,function(x) !is.factor(x))))
        data[pic]<-do.call(data.frame,lapply(data[pic],function(x) as.factor(x)))
      }

    } else{
      data<-vals$saved_data[[input$import_from_data]]

    }
    if(input$import_from_attr=="Numeric-Attribute" & input$import_to_attr=="Factor-Attribute"){
      res<-do.call(data.frame,lapply(data,function(x) as.factor(x)))
      rownames(res)<-rownames(data)
      data<-res
    }

    vars<-as.list(input$importvar)

    return(list(data,vars))
  })
  import_npage<-2
  import_diag0<-reactive({
    tags$div(id="import_dialog",
             modalDialog(size="m",easyClose = T,
                         title="Exchange Factor/Variables",
                         footer = div( inline(uiOutput("import_btn_cancel")),
                                       inline(uiOutput("import_btn_prev")),

                                       inline(uiOutput("import_btn_next")),
                                       inline(uiOutput("import_btn_end"))),

                         uiOutput("bug_exchange"),
                         uiOutput("import_page_imps")
             )
    )
  })
  output$copy_transfer<-renderUI({
    cond<-input$import_from_data==input$import_to_data & input$import_from_attr==input$import_to_attr
    req(length(cond)>0)
    choices<-if(cond){
      radioButtons("copy_transfer","Action","Copy",inline=T, selected="Copy")} else{
        radioButtons("copy_transfer","Action", c("Copy","Move"),inline=T, selected=vals$cur_copy_transfer)
      }

  })
  observeEvent(ignoreInit = T,input$copy_transfer,{
    vals$cur_copy_transfer<-input$copy_transfer
  })
  output$import_page_imps<-renderUI({
    div(id='import_inputs',
        div(class = "page_imp",
            id = 'step1',
            uiOutput("copy_transfer"),
            splitLayout(
              uiOutput("import_from"),
              uiOutput("import_to"))
        )    ,
        hidden(
          div(class = "page_imp",id = 'step2',
              uiOutput("import_factor_yes"),
              div(style="height:400px;overflow-y: scroll;overflow-x: scroll",

                  uiOutput("ordinal_page"),
                  uiOutput("binary_page")
              )))
    )
  })
  output$ordinal_page<-renderUI({
    if(input$import_from_attr=="Factor-Attribute" & input$import_to_attr=="Numeric-Attribute"){
      req(input$hand_facs=="Ordinal")
    }
    if(input$import_from_attr=="Numeric-Attribute"){
      req(input$import_to_attr=="Factor-Attribute")
    }
    div(
      div(
        inline(
          div(class="exchange_factors",
              uiOutput("cutfac"),
              div(
                div("Drag and drop the levels (green blocks) to define their values",style='white-space: normal;'),

                div(uiOutput("toorder_in"),style="font-size: 11px;"),


              )
          )
        )
      ))

  })
  output$cutfac<-renderUI({
    req(input$import_from_attr=="Numeric-Attribute")
    div(checkboxInput("cutfacs",span("Cut into intervals",tiphelp("divides the range of x into intervals and codes the values in x according to which interval they fall (cut function from R base).", "right")), F))
  })
  output$binary_page<-renderUI({
    data<-vals$saved_data[[input$import_from_data]]
    if(input$import_from_attr=="Factor-Attribute"){
      req(input$import_to_attr=="Numeric-Attribute")
      req(input$hand_facs=="Binary")
      factors<-attr(data,"factors")[,input$importvar,drop=F]
      cols<-colnames(getclassmat(factors))
    }
    if(input$import_from_attr=="Numeric-Attribute"){
      req(input$import_to_attr=="Numeric-Attribute")
      cols<-colnames(data[,input$importvar,drop=F])
    }

    lbin<-lapply(cols,function(x){
      inline(
        div(style="width: 150px",
            div(class="cutfacs_breaks_form0",textInput(paste("newbin",x, sep="_"), NULL, value=x)))
      )
    })



    div(style="height:200px;overflow-y: scroll;overflow-x: scroll",
        h5(strong("New columns")),
        lbin)
  })
  observeEvent(ignoreInit = T,input$final_import,{
    new<-final_import()


    #

    # data<-getconvert_datavars()[[1]]

    df_to<-if(input$import_to_attr=="Numeric-Attribute"){vals$saved_data[[input$import_to_data]]} else{attr(vals$saved_data[[input$import_to_data]],'factors')}
    #df_to<-df_to[rownames(data),]
    if(input$import_to_attr=="Numeric-Attribute"){
      new<-data_migrate(df_to,new,"new")
      vals$saved_data[[input$import_to_data]]<-new
    }  else{
      attr(vals$saved_data[[input$import_to_data]],"factors")<-new
    }

    if(input$copy_transfer=="Move"){
      if(input$import_from_attr=="Factor-Attribute"){
        attr(vals$saved_data[[input$import_from_data]],"factors")[,input$importvar]<-NULL
      } else{
        vals$saved_data[[input$import_from_data]][,input$importvar]<-NULL
      }
    }
    vals$cur_data<-input$import_to_data
    #removeModal()
    shinyjs::reset('import_inputs')
    runjs("Shiny.setInputValue('last_btn', 'fade');")
    modal_sucess1()

  })
  modal_sucess1<-function(){
    showModal(
      modalDialog(
        h3("Success!"),
        footer=div(actionButton("sucess_back","Back"),actionButton("sucess_close","Close")),

      )
    )

  }
  observeEvent(input$sucess_back,{
    import_rv$page_imp<-1
    showModal(
      import_diag0()
    )
  })
  observeEvent(input$sucess_close,{
    removeModal()
  })
  output$import_btn_prev<-renderUI({
    req(import_rv$page_imp!=1)
    bsButton("prev_import", "< Previous", class="button_change")
  })
  output$import_btn_cancel<-renderUI({
    req(import_rv$page_imp==1)
    bsButton("cancel_import", "Cancel", class="button_change")
  })
  output$import_btn_end<-renderUI({
    req(import_rv$page_imp==import_npage)
    bsButton("final_import", "Confirm >", class="button_change")
  })
  output$import_btn_next<-renderUI({
    req(import_rv$page_imp<import_npage)
    #req(length(input$importvar)>=1)
    dis<-if(length(input$importvar)>=1){F} else{ T}

    div(
      div(next_title=if(isTRUE(dis)){"Select at least one variable/factor"} else{ NULL},
          bsButton("next_import", "Next >", class="button_change", disabled = dis),

      )
    )
  })
  observe({
    toggleState(id = "prev_import", condition = import_rv$page_imp > 1)
    toggleState(id = "page_imp1", condition = import_rv$page_imp > 1)
    toggleState(id = "next_import", condition = import_rv$page_imp < import_npage)
    toggleState(id = "page_imp2", condition = import_rv$page_imp < import_npage)
    hide(selector = ".page_imp")
    shinyjs::show(paste0("step", import_rv$page_imp))
  })
  navpage_imp<-function(direction) {
    import_rv$page_imp<-import_rv$page_imp + direction
  }
  observeEvent(ignoreInit = T,input$cancel_import,{
    removeModal()
  })
  observeEvent(ignoreInit = T,input$prev_import, navpage_imp(-1))
  observeEvent(ignoreInit = T,input$next_import, navpage_imp(1))
  observeEvent(ignoreInit = T,input$dcogs3,{
    import_rv$page_imp<-1
    showModal(
      import_diag0()
    )
  })

  observeEvent(ignoreInit = T,input$merge_datalist_go,{
    if(input$merge_cr=='columns'){
      vals$hand_save<-"Merge Datalists (cols)"
    } else{
      vals$hand_save<-"Merge Datalists (rows)"
    }


    validate(need(length(input$merge_cbind)>1,"Select at least two Datalists to merge"))
    showModal(
      hand_save_modal()
    )})
  observeEvent(ignoreInit = T,input$dcogs5,{
    showModal(
      modalDialog(
        title="Transpose Datalist",
        easyClose = T,
        div(uiOutput("transp_target"),
            uiOutput("transp_war")),
        footer=div(modalButton("Cancel"), inline(uiOutput("confirm_transp")))
      )
    )
  })
  observeEvent(ignoreInit = T,input$transp_confirm,{
    data<-vals$saved_data[[input$transp_targ]]
    if(!is.null(attr(data,'coords'))){
      vals$hand_save2<-"The Coord-Attribute will be excluded"
    }
    vals$hand_save<-"Transpose Datalist"

    vals$hand_save3<-NULL

    showModal(
      hand_save_modal()
    )
  })
  save_transp<-reactive({
    try({
      data<-vals$saved_data[[input$transp_targ]]
      factors<-attr(data,"factors")
      temp<-data_migrate(data,t(data),"new")
      attr(temp,"factors")<-t(factors)
      attr(temp,"coords")<-NULL

      if(input$hand_save=="create"){
        vals$saved_data[[input$transp_newname]]<-temp
      } else{
        vals$saved_data[[input$transp_over]]<-temp
      }
      vals$new_facts<-NULL
      status_changes$df<-c(T,T)
      data_cogs$df<-temp
      updateRadioGroupButtons(session,'radio_cogs',selected = last_btn$equal[1])

    })
  })
  output$confirm_transp<-renderUI({
    actionButton('transp_confirm',strong("2. Confirm"))
  })
  output$transp_target<-renderUI({
    pickerInput("transp_targ","1. Select the target Datalist:",names(vals$saved_data), selected=vals$cur_data)
  })
  output$newfactors_target<-renderUI({

    pickerInput("newfac_targ","1. Select the target Datalist:",names(vals$saved_data), selected=vals$cur_data)
  })
  output$confirm_newfac<-renderUI({
    #req(length(vals$newfactors_att)>0)
    actionButton('newfac_confirm',strong("3. Confirm"))
  })
  observeEvent(ignoreInit = T,input$newfac_confirm,{
    #req(!is.null(vals$newfactors_att))
    newfac<-vals$newfactors_att
    attr(vals$saved_data[[input$newfac_targ]],"factors")<-newfac
    runjs("Shiny.setInputValue('newfactorfile', null);")
    vals$cur_data<-input$newfac_targ
    #vals$newfactors_att<-NULL

  })
  observeEvent(ignoreInit = T,input$newfactorfile,{
    vals$newfactors_att<-NULL
    req(length(input$newfactorfile$datapath)!=0)

    data<-vals$saved_data[[input$newfac_targ]]
    labels <-data.frame(fread(input$newfactorfile$datapath, stringsAsFactors = T,na.strings=c("","NA"), header=T))

    rownames(labels) <-labels[, 1]
    labels[, 1]<-NULL

    labels[which(unlist(lapply(labels, is.numeric)))] <-
      do.call("data.frame", lapply(labels[which(unlist(lapply(labels, is.numeric)))], function(x) as.factor(x)))
    data<-vals$saved_data[[input$newfac_targ]]
    factors_in<-labels[rownames(data),,drop=F]
    notfoundnew<-which(!rownames(data)%in%na.omit(rownames(factors_in)))
    if(length(notfoundnew)>0){
      output$newfactors_missing<-renderUI({
        div(style="overflow-x: scroll;height:180px;overflow-y: scroll",
            div(strong("Error",style="color: red"),"The IDs below exist in the Data-Attribute, but not in the",strong('Factor-Attribute.'),"Please upload a new file that contains IDs compatible with the selected Datalist."),
            renderPrint(data.frame(IDs=rownames(data)[notfoundnew]))
        )
      })
    } else{
      output$newfactors_missing<-renderUI(NULL)

    }
    req(!length(notfoundnew)>0)
    vals$newfactors_att<-labels
  })
  output$import_factor_yes<-renderUI({
    action<-span(strong("Action:"),input$copy_transfer)
    vars<- inline(div(style="max-width: 200px; color: SeaGreen; background: Gainsboro",em(paste0(input$importvar,collapse="; "))))
    sub<-NULL
    sub<- if(input$import_from_attr=="Numeric-Attribute"){
      if(input$import_to_attr=="Numeric-Attribute"){} else {
        span("(as factor)")
      }
    }
    sub<- if(input$import_from_attr=="Factor-Attribute"){
      if(input$import_to_attr=="Numeric-Attribute"){
        if(input$hand_facs=="Binary"){
          "(as binary columns)"
        } else{
          "(as value-factor-levels)"
        }
      }
    }
    from_to<-span("from",strong(input$import_from_data), "to",strong(input$import_to_data))
    div(
      div(
        action,
        vars,
        span(sub, style="color:  #05668D;"),
        from_to
      )


    )
  })
  output$import_var<-renderUI({
    req(input$import_from_data)
    data<-vals$saved_data[[input$import_from_data]]
    if(input$import_from_attr=='Factor-Attribute'){
      choices<-colnames(attr(data,"factors"))
      title="Factors:"
    } else{
      title="Variables:"
      choices<- colnames(data)
    }
    div(style="overflow-x: scroll;height:180px;overflow-y: scroll",
        div(strong(title)),
        checkboxGroupInput("importvar", NULL, choices=choices,selected=vals$cur_importvar,width="150px")
    )

  })
  observeEvent(ignoreInit = T,input$importvar,{
    vals$cur_importvar<-input$importvar
  })
  layers_choices2<-reactive({
    base_shape<-attr(vals$saved_data[[input$shp_datalist]],"base_shape")
    layer_shape<-attr(vals$saved_data[[input$shp_datalist]],"layer_shape")
    eshape<-attr(vals$saved_data[[input$shp_datalist]],"extra_shape")

    pic<-which(unlist(lapply(list(base_shape,layer_shape),function(x)length(x)>0)))
    choices=c(c("Base Shape","Layer Shape"),names(eshape))
    choices
  })


  observeEvent(ignoreInit = T,input$radio_cogs,{
    req(input$radio_cogs!='')
    req(input$radio_cogs!='tools_save_changes')
    req(!is.na(input$radio_cogs))
    req(length(input$radio_cogs)>0)
    last_btn$equal<-c(input$radio_cogs,last_btn$df[1])
    last_btn$df<-input$radio_cogs

  })
  hand_save_modal<-reactive({

    tags$div(id="savemodal",

             modalDialog(
               withSpinner(type=8,color="SeaGreen",uiOutput("databank_storage")),
               title=span(icon(verify_fa = FALSE,name=NULL,class="fas fa-save"),'Save'),
               footer=column(12,class="needed",
                             fluidRow(bsButton("cancel_save","Cancel"),
                                      inline(uiOutput("save_confirm"))
                             )),
               size="l",
               easyClose = T
             )
    )
  })
  observe({
    req(length(input$lastkeypresscode)>0)
    if(input$lastkeypresscode == 27){ removeModal()}
  })
  observeEvent(ignoreInit = T,input$data_confirm,{

    gosave$df<-1

  })
  observe({
    req(isTRUE(gosave$modal))
    req(length(input$data_confirm)>0)
    req(length(input$lastkeypresscode)>0)
    action<-gosave$df
    # Decide which action should be taken
    gosave$df<- if(input$data_confirm %% 2|input$lastkeypresscode == 13) {
      action<-1
    } else    if(input$lastkeypresscode == 27){
      action<-0

    }

    gosave$df
  })

  save_switch<-reactive({
    switch(vals$hand_save,
           "create_codebook"=savecodebook(),
           "Save new som in" = {savesom()},
           "Save changes"= {  savechanges_comb()},
           "Transpose Datalist"={save_transp()},
           "Include binary columns in the Numeric-Attribute"=  {savebinary()},
           "Include ordinal variables in the Numeric-Attribute"=  {saveordinal()},
           "Save Clusters"= {saveclusters()},
           "Save new data clusters"= {savemapcode()},
           "Create data list from aggregation results"= {saveagg()},
           "Add an Extra-Layer-Attribute to the Datalist"=addlayer(),
           "Save diversity results"= { savediv()},
           "Save rarefied data"= { saverare()},

           "add partition"= {savepart()},
           "Save som predictions"= {savesompred()},
           "Save errors from som predictions (X)"= {datalist_som_errorsX()},
           "Save errors from som predictions (Y)"= {datalist_som_errorsY()},


           "Save discrete model"=savediscrete(),

           "Merge Datalists (cols)"=mergedatacol(),
           "Merge Datalists (rows)"=mergedatarow()
    )
  })
  observeEvent(gosave$df,{
    req(length(vals$hand_save)>0)


    runjs("Shiny.setInputValue('lastkeypresscode', 18);")
    req(length(gosave$df)>0)
    if(gosave$df==1){ save_switch()
      gosave$df<-0


      vals$hand_save<-NULL
      vals$hand_save3<-NULL

    }

    removeModal()
  })
  observeEvent(ignoreInit = T,input$cancel_save,{
    if(input$cancel_save %%2)
      runjs("Shiny.setInputValue('lastkeypresscode', 27);")
  })
  output$databank_storage<-renderUI({
    column(12,
           fluidRow(
             column(12,p(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")), p(vals$hand_save2,style="color: gray")),
             column(12,vals$hand_save3),
             column(12,style='margin-top: 10px; margin-left: 10px',
                    splitLayout(cellWidths = c("30%","70%"),
                                radioButtons("hand_save",NULL,
                                             choiceNames= list(div(style="height: 50px","create"),
                                                               div(style="height: 50px","overwrite")),
                                             choiceValues=list('create',"over")),
                                column(12,div(style="height: 50px",
                                              withSpinner(type=8,color="SeaGreen",uiOutput("data_create"))),
                                       div(style="height: 50px",
                                           withSpinner(type=8,color="SeaGreen",uiOutput("data_over"))))
                    ))

           )
    )
  })
  output$save_confirm<-renderUI({
    req(isTRUE(data_store$df)|isTRUE(data_overwritte$df))
    actionButton("data_confirm",strong("confirm"))
  })
  bag_merge<-reactive({
    bag<-1

    name0<-paste("Datalist_merged_",input$merge_cr)
    name1<-paste0(name0," (",bag,")")
    if(name1%in%names(vals$saved_maps))
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%names(vals$saved_maps)) break
      }
    }
    paste0(name0," (",bag,")")

  })
  bag_agg<-reactive({
    bag<-1
    name0<-paste0(input$data_upload,"_",input$spread_measures)
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]


  })
  bag_extralayer<-reactive({
    bag<-1
    name0<-'Extra-Layer'
    names_shapes<-names(attr(vals$saved_data[[input$shp_datalist]],"extra_shape"))
    name1<-paste0(name0," (",bag,")")
    if(length(names_shapes)>0){
      if(name1%in%names_shapes)
      {
        repeat{
          bag<-bag+1
          name1<-paste0(name0," (",bag,")")
          if(!name1%in%names_shapes) break
        }
      }
    }

    paste0(name0," (",bag,")")

  })
  observe({
    req(vals$hand_save=="Create factor using breakpoints from the dissimilarity profile")
    req(input$data_confirm %% 2)
    updateRadioGroupButtons(session,'desc_options',selected='segRDA')
    updateTabsetPanel(session,'segrda_panels',selected='DP')
  })
  to_merge_datacol<-reactive({
    try({

      #input$merge_cbind<-names(vals$saved_data)[c(3,5)]
      to_merge<-vals$saved_data[input$merge_cbind]

      to_merge_fac<-lapply(vals$saved_data[input$merge_cbind],function(x) attr(x,"factors"))
      mx<-which.max(do.call(c,lapply(to_merge,nrow)))
      newmerge<-data.frame(id=rownames(to_merge[[mx]]))
      rownames(newmerge)<-newmerge$id
      l1<-unlist(lapply(to_merge,function(x){
        x[rownames(newmerge),, drop=F]
      }),
      recursive = F)
      newdata<-data.frame(l1)

      rownames(newdata)<-rownames(to_merge[[mx]])
      colnames(newdata)<-c(do.call(c,lapply(to_merge,colnames)))

      if(isTRUE(input$rm_dup)) {
        if(any(duplicated(colnames(newdata)))){
          dup<-which(duplicated(colnames(newdata)))
          keep<-which.max(do.call(c,lapply(lapply(newdata[dup],na.omit),length)))
          fall<-dup[-keep]
          newdata<-newdata[colnames(newdata)[-fall]]
        }
      }



      mxfac<-which.max(do.call(c,lapply(to_merge_fac,nrow)))
      newmerge_fac<-data.frame(id=rownames(to_merge_fac[[mxfac]]))
      rownames(newmerge_fac)<-newmerge_fac$id
      l2<-unlist(lapply(to_merge_fac,function(x){
        x[rownames(newmerge_fac),, drop=F]
      }),
      recursive = F)
      newfac<-data.frame(l2)

      rownames(newfac)<-rownames(to_merge_fac[[mx]])
      colnames(newfac)<-c(do.call(c,lapply(to_merge_fac,colnames)))
      if(isTRUE(input$rm_dup)){
        if(any(duplicated(colnames(newfac)))){
          dup<-which(duplicated(colnames(newfac)))
          keep<-which.max(do.call(c,lapply(lapply(newfac[dup],na.omit),length)))
          fall<-dup[-keep]
          newfac<-newfac[colnames(newfac)[-fall]]
        }
      }

      newdata<-data_migrate(to_merge[[mx]],newdata,input$mergec_newname)
      attr(newdata, "transf")=NULL
      attr(newdata,"factors")<-newfac[rownames(newdata),, drop=F]
      newdata

    })
  })
  mergedatacol<-reactive({
    try({

      newdata<-to_merge_datacol()
      if(input$hand_save=="create"){
        vals$saved_data[[input$mergec_newname]]<-newdata}
      else{
        vals$saved_data[[input$mergec_over]]<-newdata
      }



    })
  })
  to_merge_datarow<-reactive({

    datarow_list<-vals$saved_data[input$merge_cbind]
    #input$fill_row

    #Reduce(intersect, list(a,b,c))
    if(isFALSE(input$fill_row)){
      li<-lapply(datarow_list, function(x) colnames(x))
      com_cols<-Reduce(intersect, li)
      validate(need(length(com_cols)>0,"No compatible columns between selected Datalists. Consider using the 'Fill' option."))
      datarow_list<-lapply(datarow_list, function(x){
        x[,com_cols]
      })
    }

    transfer_rownames<-function(datalist, dataframe){
      li<-lapply(datalist,function(x){
        rownames(x)
      })
      dup_rows<-Reduce(intersect, li)
      if(!length(dup_rows)>0){
        rownames(dataframe)<-make.unique(do.call(c,li))
      }
      return(dataframe)

    }



    datarow_df<-data.frame(data.table::rbindlist(datarow_list, use.names=T, fill=T, idcol=NULL))
    datarow_df<-transfer_rownames(datarow_list,datarow_df)


    factors_list<-lapply(vals$saved_data[input$merge_cbind],function(x) attr(x,"factors"))
    factors_df<-data.frame(data.table::rbindlist(factors_list, use.names=T, fill=T, idcol=NULL))
    factors_df<-transfer_rownames(factors_list,factors_df)

    #factors_list<-lapply(factors_list, function(x) data.frame(id=rownames(x),x))


    coords_list<-lapply(vals$saved_data[input$merge_cbind],function(x) attr(x,"coords"))
    coords_df<-data.frame(data.table::rbindlist(coords_list, use.names=T, fill=T, idcol=NULL))
    coords_df<-transfer_rownames(coords_list,coords_df)
    base_shape<-lapply(vals$saved_data[input$merge_cbind],function(x) attr(x,"base_shape"))
    layer_shape<-lapply(vals$saved_data[input$merge_cbind],function(x) attr(x,"layer_shape"))
    extra_shape<-lapply(vals$saved_data[input$merge_cbind],function(x) attr(x,"extra_shape"))
    datarow_df<-data_migrate(vals$saved_data[[1]],datarow_df,"new")

    pic<-if(isTRUE(input$rm_dup)){
      !duplicated(datarow_df)
    }else{
      1:nrow(datarow_df)
    }
    datarow_df<-datarow_df[pic,,drop=F]
    attr(datarow_df,"factors")<-factors_df[pic,,drop=F]
    attr(datarow_df,"coords")<-coords_df[pic,]
    attr(datarow_df,"base_shape")<-base_shape[[1]]
    attr(datarow_df,"layer_shape")<-layer_shape[[1]]
    attr(datarow_df,"extra_shape")<-extra_shape
    attr(datarow_df, "transf")=NULL
    datarow_df
  })
  mergedatarow<-reactive({
    newdata<-to_merge_datarow()
    if(input$hand_save=="create"){
      vals$saved_data[[input$merger_newname]]<-newdata
      vals$cur_data<-input$merger_newname}
    else{
      vals$saved_data[[input$merger_over]]<-newdata
      vals$cur_data<-input$merger_over
    }


  })
  saveagg<-reactive({
    curdata<-input$data_upload
    temp<-vals$agg
    factors<-attr(temp,"factors")
    coords<-attr(temp,"coords")
    data<-vals$saved_data[[input$data_upload]]
    temp<-data_migrate(data,temp,"new")
    attr(temp,"factors")<-factors
    attr(temp,"coords")<-coords

    if(input$hand_save=="create"){
      vals$saved_data[[input$agg_newname]]<-temp
      vals$cur_data<-input$agg_newname
    } else{
      vals$saved_data[[input$agg_over]]<-temp
      vals$cur_data<-input$agg_over
    }
    temp<-data.rares_fun(

      saved_data=vals$saved_data,
      cur_data=vals$cur_data,
      filter_data=input$filter_data,
      cutlevel_obs=input$cutlevel_obs,
      filter_datalist=input$filter_datalist,
      selecvar=input$selecvar,
      selecobs=input$selecobs,
      seltree=vals$seltree,
      na.omit=input$na.omit,
      transf=input$transf,
      scale=input$scale,
      center=input$center,
      rareabund=input$rareabund,
      pct_rare=input$pct_rare,
      rarefreq=input$rarefreq,
      pct_prev=input$pct_prev,
      raresing=input$raresing,
      obs_match=input$obs_match,
      obs_match_datalist=input$obs_match_datalist,
      nzv= getnearzero(),
      cor_filter=get_corrdata()


    )


    if(input$hand_save=="create"){
      vals$saved_data[[input$agg_newname]]<-temp
      vals$cur_data<-input$agg_newname
    } else{
      vals$saved_data[[input$agg_over]]<-temp
      vals$cur_data<-input$agg_over
    }
    vals$agg<-NULL
    status_changes$df<-c(T,T)
    vals$cur_data<-curdata
    updateRadioGroupButtons(session,"radio_cogs",selected=last_btn$equal[1])
    runjs(paste0("Shiny.setInputValue('radio_cogs', ",last_btn$equal[1],");"))


  })
  modal_dialog<-function(){
    modalDialog(
      title="Sucess",
    )
  }
  bag_divname<-reactive({
    bag<-1
    name0<-paste("Div_results")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("Div_results",bag)

  })

  observeEvent(ignoreInit = T,input$bank_button, {
    showModal(insert_modal())
    insert_rv$page<-1
  })
  observeEvent(ignoreInit = T,input$layer_back, {
    showModal(insert_modal())
    insert_rv$page<-1
  })
  observeEvent(ignoreInit = T,input$base_back, {
    showModal(insert_modal())
    insert_rv$page<-1
  })
  insert_npages<-2
  insert_modal<-reactive({
    tags$div(id="insert_dialog",
             modalDialog(easyClose = T,
                         footer = div(
                           id="intert_btns",
                           inline(uiOutput("insert_cancel_btn")),
                           inline(uiOutput("insert_revbtn")),

                           inline(uiOutput("insert_next_btn")),
                           inline(uiOutput("insert_last_btn"))
                         ),

                         column(12,uiOutput("insert_pages"))
             )
    )
  })
  navPage<-function(direction) {
    insert_rv$page<-insert_rv$page + direction
  }
  output$insert_pages<-renderUI({
    div(
      div(class = "page",
          id = 'insert_step1',
          uiOutput("insert_page1")
      )
      ,

      hidden(
        div(class = "page",id = 'insert_step2',
            uiOutput('factors_missing'),
            uiOutput("insert_page2")))

    )
  })
  output$insert_revbtn<-renderUI({
    req(insert_rv$page!=1)
    actionButton("insert_prev", "< Previous")
  })
  output$insert_cancel_btn<-renderUI({
    req(insert_rv$page==1)
    actionButton("insert_cancel", "Cancel")
  })
  output$insert_last_btn<-renderUI({
    req(insert_rv$page==insert_npages)
    actionButton("insert_end", "Insert Datalist >")
  })
  output$insert_next_btn<-renderUI({
    req(length(input$filedata$datapath)>0|input$up_or_ex=='use example data')
    req(insert_rv$page<insert_npages)
    actionButton("insert_next", "Next >")
  })
  output$insert_page1<-renderUI({
    div(
      h4("Create Datalist",
         popify(actionLink("uphelp0",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Data bank structure",as.character(paste0("Each Datalist in the Databank requires at least one file for the Numeric-Attribute. If a Factor-Attribute is not provided, iMESc automatically generates this attribute as a single-column sheet containing the IDs of the Numeric-Attribute.")),trigger="hover")),
      uiOutput("upload_input")

    )
  })
  output$upload_input<-renderUI({
    div(
      div(
        div(class="insert_radio_border_top"),
        uiOutput("upload_example"),

        div(class="insert_radio_border_bottom"),
        div(style="margin-left: 50px",div(class="req_border0"))),
      uiOutput("upload_input2")

    )
  })
  output$datalistname<-renderUI({
    div(
      inline(textInput("data_name", NULL, value=NULL, width= '320px')),

    )
  })
  observeEvent(input$filedata,{
    req(input$data_name=="")
    updateTextInput(session,"data_name",value=gsub(".csv|.xlsx|xls","",input$filedata$name))
  })
  observeEvent(input$up_or_ex,{
    if(input$up_or_ex == 'use example data'){
      updateTextInput(session,"data_name",value="nema_araca/envi_araca")

    }
  })
  observeEvent(input$data_name,{
    if(input$data_name=='nema_araca/envi_araca'){
      shinyjs::disable('data_name')
    }
  })
  output$upload_example<-renderUI({
    tags$div(class="insert_radio",

             div(
               radioButtons("up_or_ex",NULL,choiceValues  = list("upload", "use example data"),
                            choiceNames   = list(tipify(strong("Upload"),"upload your own data",placement = "bottom", options=list(container="body")), tipify(strong("Use example data"),"Use Nematode datasets from Araca Bay as example",placement = "bottom", options=list(container="body"))),selected = "upload", inline = T)
             )
    )
  })
  output$upload_input2<-renderUI({
    req(input$up_or_ex)
    div(style="width: 100%; display: flex",
        class="insert_c0",
        div(style="width: 20%",
            div(class="insert_col1",
                div(
                  class="topa",
                  div(strong("Required:"), style="color: SeaGreen;"),
                  div(style="margin-left: 50px",div(class="req_border1"))
                )
            ),
            div(style="width: 150px; margin-top: 20px",
                div(strong("Optional:",style="color: #05668D")),
                div(style="margin-left: 50px; margin-bottom: 60px",div(class="req_border2")),
                em(span("*",style="font-size: 15px"),"Required for the spatial tools menu", style="margin-left: 10px")
            )
        ),
        div(style="width: 80%",
            div(
              div(strong("Name the datalist"), style="color: SeaGreen;"),
              div(inline(uiOutput("datalistname"))),
              div(
                inline(
                  div(
                    class='insert_form',
                    # uiOutput('use_example_mess'),
                    div(style="display: table-row",
                        inline(div(class="req_border11")),
                        inline(uiOutput('upload_numeric')),
                        inline(uiOutput('insertDL_bts1'))
                    ),
                    div(id="insert_opt",
                        div(style="display: table-row",
                            inline(div(class="req_border12")),
                            inline(uiOutput('upload_factors')),
                            inline(uiOutput('insertDL_bts2'))
                        ),
                        div(style="display: table-row",
                            inline(div(class="req_border12")),
                            inline(uiOutput('upload_coords')),
                            inline(uiOutput('insertDL_bts3'))
                        ),
                        div(style="display: table-row",
                            inline(div(class="req_border12")),
                            inline(uiOutput('upload_base_shape')),
                            inline(uiOutput('insertDL_bts4'))
                        ),
                        div(style="display: table-row",
                            inline(div(class="req_border12")),
                            inline(uiOutput('upload_layer_shape')),
                            inline(uiOutput('insertDL_bts5'))
                        )
                    )
                  )
                ),
                bsTooltip("reset_insert_1","reset"),
                bsTooltip("reset_insert_2","reset"),
                bsTooltip("reset_insert_3","reset"),
                bsTooltip("reset_insert_4","reset"),
                bsTooltip("reset_insert_5","reset")
              )
            )
        )
    )
  })
  output$use_example_mess<-renderUI({
    req(input$up_or_ex)
    req(input$up_or_ex == 'use example data')
    "Using example Datalists: nema_araca and envi_araca"
  })
  output$upload_numeric<-renderUI({
    div(id="insert_req",
        div(
          style="color:  SeaGreen;",
          div(strong("Numeric-Attribute:",style="color:  SeaGreen"),
              popify(actionLink('uphelp', icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Upload the observations",textupload(), trigger = "hover", placement ="right")),
          if(length(input$up_or_ex)>0){
            if(input$up_or_ex=='upload') {
              fileInput(inputId = "filedata",label = NULL,accept = c(".csv",".xlsx",".xls"), placeholder=if(length(input$filedata$datapath)>0){input$filedata$name})
            }else{
              div(class="use_example",
                  div(
                    div(class="insert_e","nematode/abiotic data from Araca Bay, Brazil")
                  )
              )
            }
          }

        )

    )
  })
  output$upload_factors<-renderUI({
    div(
      style="color:  #05668D;",
      div(strong("Factor-Attribute:",style="color:  #05668D"),
          popify(actionLink('labhelp', icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Upload the factors",textlab(), trigger = "hover", placement ="right")),
      if(length(input$up_or_ex)>0){
        if(input$up_or_ex=='upload'){
          fileInput("labels", NULL, accept = c(".csv",".xlsx"),placeholder=if(length(input$labels$datapath)>0){input$labels$name})
        }else{

          div(class="use_example",
              div(class="shiny-input-container",
                  div(class="insert_e","sampling factors for both Datalists")
              )
          )
        }
      }
    )
  })
  output$upload_coords<-renderUI({
    div(
      style="color:  #05668D;",
      div(strong(span("*",style="font-size: 15px"),"Coords-Attribute:"),
          popify(actionLink('cohelp', icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Upload the coordinates", textcoords(), trigger = "hover", placement ="right")),
      if(length(input$up_or_ex)>0){
        if(input$up_or_ex=='upload'){
          fileInput(inputId = "coords",label =  NULL,accept = c(".csv",".xlsx"),placeholder=if(length(input$coords$datapath)>0){input$coords$name})
        } else {
          div(class="use_example",
              div(class="shiny-input-container",
                  div(class="insert_e","sampling coordinates for both Datalists")
              )
          )
        }
      }

    )
  })
  output$upload_base_shape<-renderUI({
    div(

      style="color:  #05668D;",
      div(strong(span("*",style="font-size: 15px"),"Base shape:",actionLink("basehelp", tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Click for more details")))),
      if(length(input$up_or_ex)>0){
        if(input$up_or_ex=='upload'){
          fileInput(inputId = "base_shape",label = NULL,placeholder=if(length(input$base_shape$datapath)>0){input$base_shape$name})
        }else {
          div(class="use_example",
              div(class="shiny-input-container",
                  div(class="insert_e","base shape of the Araca Bay, for both Datalists")
              )
          )

        }
      }



    )
  })
  output$upload_layer_shape<-renderUI({
    div(

      style="color:  #05668D;",
      div(strong(span("*",style="font-size: 15px"),"Layer shape:",actionLink("layerhelp", tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Click for more details")))),
      if(length(input$up_or_ex)>0){
        if(input$up_or_ex=='upload'){
          fileInput(inputId = "layer_shape",label = NULL,placeholder=if(length(input$layer_shape$datapath)>0){input$layer_shape$name})
        }else{
          div(class="use_example",
              div(class="shiny-input-container",
                  div(class="insert_e","layer shape of the Araca Bay, for both Datalists")
              )
          )

        }
      }

    )
  })
  observeEvent(ignoreInit = T,input$sheet_data,{
    vals$sheet_data<-input$sheet_data
  })
  observeEvent(ignoreInit = T,input$sheet_fac,{
    vals$sheet_fac<-input$sheet_fac
  })
  observeEvent(ignoreInit = T,input$sheet_coord,{
    vals$sheet_coord<-input$sheet_coord
  })
  output$insertDL_bts1<-renderUI({
    req(input$up_or_ex=='upload')
    req(length(input$labels$datapath)>0)
    div(class="topa7",
        if(length(input$filedata$datapath)>0) {
          if(length(grep(".xlsx",input$filedata$datapath))>0){

            choices_data<-excel_sheets(path = input$filedata$datapath)
            res<-inline(pickerInput("sheet_data","sheet:", choices=choices_data, width = "110px", selected=vals$sheet_data))} else{res<-NULL}
          inline(div(inline(res),inline(bsButton("reset_insert_1",icon(verify_fa = FALSE,name=NULL,class="fas fa-undo"), style="button_change"))))
        })
  })
  output$insertDL_bts2<-renderUI({
    req(input$up_or_ex=='upload')
    req(length(input$labels$datapath)>0)
    div(class="topa7",
        if(length(input$labels$datapath)>0){
          if(length(grep(".xlsx",input$labels$datapath))>0){

            choices_labels<-excel_sheets(path = input$labels$datapath)
            res<-inline(pickerInput("sheet_fac","sheet:", choices=choices_labels, width = "110px", selected=vals$sheet_fac))} else{res<-NULL}
          inline(div(inline(res),inline(bsButton("reset_insert_2",icon(verify_fa = FALSE,name=NULL,class="fas fa-undo"), style="button_change"))))

        })
  })
  output$insertDL_bts3<-renderUI({
    req(input$up_or_ex=='upload')
    req(length(input$coords$datapath)>0)
    div(class="topa7",
        if(length(input$coords$datapath)>0){
          if(length(grep(".xlsx",input$coords$datapath))>0){

            choices_coords<-excel_sheets(path = input$coords$datapath)
            res<-inline(pickerInput("sheet_coord","sheet:", choices=choices_coords, width = "110px", selected=vals$sheet_coord))} else{res<-NULL}
          inline(div(inline(res),inline(bsButton("reset_insert_3",icon(verify_fa = FALSE,name=NULL,class="fas fa-undo"), style="button_change"))))})
  })
  output$insertDL_bts4<-renderUI({
    req(input$up_or_ex=='upload')
    req(length(input$base_shape$datapath)>0)
    div(class="topa7",
        if(length(input$base_shape$datapath)>0){bsButton("reset_insert_4",icon(verify_fa = FALSE,name=NULL,class="fas fa-undo"), style="button_change")})
  })
  output$insertDL_bts5<-renderUI({
    req(input$up_or_ex=='upload')
    req(length(input$layer_shape$datapath)>0)
    div(class="topa7",
        if(length(input$layer_shape$datapath)>0){bsButton("reset_insert_5",icon(verify_fa = FALSE,name=NULL,class="fas fa-undo"), style="button_change")})
  })
  observeEvent(ignoreInit = T,input$reset_insert_1,{
    runjs("Shiny.setInputValue('filedata', null);")
  })
  observeEvent(ignoreInit = T,input$reset_insert_2,{
    runjs("Shiny.setInputValue('labels', null);")
  })
  observeEvent(ignoreInit = T,input$reset_insert_3,{
    runjs("Shiny.setInputValue('coords', null);")
  })
  observeEvent(ignoreInit = T,input$reset_insert_4,{
    runjs("Shiny.setInputValue('base_shape', null);")
  })
  observeEvent(ignoreInit = T,input$reset_insert_5,{
    runjs("Shiny.setInputValue('layer_shape', null);")
  })
  getdatalist_envi<-reactive({
    data=envi_araca()
    factors<-attr(data,"factors")
    factors_in<-read_labels()[rownames(data),,drop=F]
    notfound<-which(!rownames(data)%in%na.omit(rownames(factors_in)))
    if(length(notfound)>0){
      output$factors_missing<-renderUI({
        div(style="width: 400px; height: 80px",
            div(strong("Error",style="color: red"),"The IDs below exist in the Data-Attribute, but not in the",strong('Factor-Attribute')),
            renderPrint(data.frame(IDs=rownames(data)[notfound]))
        )
      })
    } else{
      output$factors_missing<-renderUI(NULL)
    }
    validate(need(nrow(factors_in)==nrow(data),
                  ''))


    # renderPrint()
    #data<-read.csv("som.csv",sep=";", stringsAsFactors = T)
    if (any(names(datastr(data)$type.vars) == "factor")){

      for(i in 1:ncol(data)){
        new<-as.numeric(as.character(data[,i]))
        if(!sum(is.na(new))==length(new)){data[,i]<-new}
      }
      num_cols<-which(unlist(lapply(data,function(x)is.numeric(x))))#ok
      data.numerics<-data[,num_cols,drop=F]
      data.factors<-data[,which(unlist(lapply(data,function(x)is.factor(x)))),drop=F]

      data.factors<-cbind(data.factors)
      rownames(data.numerics)<-rownames(data)
      data<-data.numerics
      attr(data,"data.factors")<-"factors removed from numeric-attribute"
      factors_in[,colnames(data.factors)]<-data.factors
    }


    attr(data,"nobs_ori")<-nrow(data)
    attr(data,"nvar_ori")<-ncol(data)


    if(input$up_or_ex == 'use example data'){
      attr(data,"filename")<-'nema_araca.csv'
      attr(data,"datalist")<-paste("envi_araca")
    } else {attr(data,"filename")<-input$data_name
    attr(data,"datalist")<-input$data_name

    }
    validate(need(sum(rownames(factors_in)%in%rownames(data))>0,"Numeric-Attribute and Factors-Attribute must have conforming IDs (first column). Check these files and reload again;"))
    attr(data,"factors")<-factors_in
    attr(data,"coords")<-coords()[rownames(data),]
    attr(data,"base_shape")<-base_shape()
    attr(data,"layer_shape")<-layer_shape()

    datalist = list(data)
    names(datalist)<-input$data_name
    datalist
  })
  output$insert_page2<-renderUI({
    coords<-attr(getdatalist()[[1]],"coords")
    if(!is.null(coords)){
      validate(need(ncol(coords)==2, "Cannot proceed: The coordinates sheet has more columns than allowed. It should only contain IDs followed by 2 columns: longitude and latitude in decimal format. Please correct or remove the coordinates sheet"))
      validate(need(is.numeric(coords[,1]), "Cannot proceed: The longitude column in the coordinates table is not numeric. Please correct or remove the coordinates sheet."))
      validate(need(is.numeric(coords[,2]), "Cannot proceed: The latitude column in the coordinates table is not numeric. Please correct or remove the coordinates sheet."))

    }

    if(input$up_or_ex == 'use example data'){
      splitLayout(
        datalist_render(getdatalist()[[1]],F,width="60px"),
        datalist_render(getdatalist_envi()[[1]],F,width="60px")
      )
    } else{
      div(style="width: 50%",
          datalist_render(getdatalist()[[1]],F,width="60px")
      )
    }


  })
  envi_araca<- reactive({
    req(input$up_or_ex)

    data <-data.frame(fread("inst/www/envi_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
    rownames(data)<-data[, 1]
    data[, 1]<-NULL
    data
  })
  observe({
    toggleState(id = "insert_prev", condition = insert_rv$page > 1)
    toggleState(id = "page1", condition = insert_rv$page > 1)
    toggleState(id = "insert_next", condition = insert_rv$page < insert_npages)
    toggleState(id = "page2", condition = insert_rv$page < insert_npages)
    hide(selector = ".page")
    shinyjs::show(paste0("insert_step", insert_rv$page))
  })
  observeEvent(ignoreInit = T,input$insert_cancel,{
    removeModal()
  })
  observeEvent(ignoreInit = T,input$insert_prev, navPage(-1))
  observeEvent(ignoreInit = T,input$insert_next, navPage(1))
  observeEvent(ignoreInit = T,input$bookmark1, {
    reactiveValuesToList(input)
    session$doBookmark()
  })
  output$bookmarkBtn<-{
    downloadHandler(
      filename = function() {
        paste("Savepoint_",Sys.Date(), ".rds", sep = "")

      },

      content = function(file) {
        withProgress(
          message = "Preparing the download ...",
          min = 1,
          max = 1,
          {
            #tosave<-isolate(reactiveValuesToList(vals))
            #tosave<-c(tosave["saved_data"],tosave["saved_maps"],tosave["newcolhabs"],tosave['colors_img'],tosave[grep("cur",names(tosave))])
            tosave<-isolate(reactiveValuesToList(vals))
            tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
            tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]

            tosave$saved_ensemble<-vals$saved_ensemble
            #tosave$saved_maps<-vals$saved_maps
            tosave$saved_data<-vals$saved_data
            tosave$newcolhabs<-vals$newcolhabs
            tosave$colors_img<-vals$colors_img
            saveRDS(tosave, file)
            beep(10)

            runjs("Shiny.setInputValue('last_btn', 'fade');")
          }
        )

      }
    )
  }
  observeEvent(ignoreInit = T,input$load_savepoint,{
    output$validade_savepoint<-renderUI({


      validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Error: the uploaded file is not an rds file"))
    })

  })
  observeEvent(ignoreInit = T,input$savepoints_button, {
    showModal(dialogmodal())
  })
  output$downbook<-renderUI({

    fluidRow(
      column(12,tipify(downloadButton("bookmarkBtn", style = "font-size: 12px", label=paste("Download", input$savepoint_name)),"Download the Savepoint as rds file. You can latter upload the created savepoint (panel bellow)."))
    )})
  observeEvent(ignoreInit = T,input$load_savepoint,{
    validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Requires rds file"))
    delay(600,{
      showModal(
        modalDialog(
          uiOutput("savepoint_request"),
          title="",
          footer = column(12,inline(uiOutput("savepoints_proceed")),modalButton("close"))

        )
      )
    })
  })
  output$savepoints_proceed<-renderUI({
    req(length(grep(".rds",input$load_savepoint$datapath)))
    inline(actionButton("load_savepoint_yes","Proceed"))
  })
  output$savepoint_request<-renderUI({
    validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Requires rds file"))
    column(12,
           h5('Are you sure? '),
           column(12,p(strong("Warning:", style="color: #0093fcff"),"The application will restart with the loaded savepoint and unsaved changes will be lost")))

  })
  observeEvent(ignoreInit = T,input$load_savepoint_yes,{
    data_cogs$df<-0
    vals$saved_data<-NULL
    vals$cur_tab<-"menu_intro"
    updateTextInput(session, "tabs", value = 'menu_intro')
    validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Requires rds file"))

    if(length(input$load_savepoint$datapath)>1){

      res<-lapply(as.list(input$load_savepoint$datapath), function(x) readRDS(x))
      saved_data<-do.call(c,lapply(res,function(x)    x[["saved_data"]]))
      saved_ensemble<-do.call(c,lapply(res,function(x)    x[["saved_ensemble"]]))
      mybooks<-res[[1]]
      if(length(saved_ensemble)>0){
        names(saved_ensemble) <-make.unique(names(saved_ensemble))
      }

      names(saved_data) <-make.unique(names(saved_data))
      mybooks$saved_ensemble<-saved_ensemble
      mybooks$saved_data<-saved_data
    } else{
      mybooks<-readRDS(input$load_savepoint$datapath)
    }
    newvals<-mybooks



    if(is.null(newvals$cur_data)){newvals$cur_data<-names(vals$saved_data)[[1]]}
    vals<-myBookmarks

    #vals$newcolhabs<-newvals$newcolhabs
    #newcolhabs<-newvals$newcolhabs
    #newvals$newcolhabs<-NULL
    for (var in names(newvals) ) {
      vals[[var]]<-newvals[[var]]
    }

    #get_colorimages()
    #vals$newcolhabs[picin]<-newcolhabs[picin]
    delay(500,{
      updateTextInput(session, "tabs", value = newvals$cur_tab)
      runjs("Shiny.setInputValue('last_btn', 'fade');")
    })

    beep(10)
    # modal_gerenciador()
  })
  observeEvent(load_qsave(),{
    if(load_qsave()==1){
      removeModal()
    }
  })
  observeEvent(load_qsave(),{
    req(once_load$df==0)
    req(load_qsave()==1)
    once_load$df<-1
    mybooks_teste<-readRDS('savepoint.rds')

    newvals<-readRDS('savepoint.rds')

    if(is.null(newvals$cur_data)){newvals$cur_data<-names(vals$saved_data)[[1]]}
    vals<-myBookmarks
    for (var in names(newvals) ) {
      vals[[var]]<-newvals[[var]]
    }

    delay(500,{
      updateTextInput(session, "tabs", value = newvals$cur_tab)
      runjs("Shiny.setInputValue('last_btn', 'fade');")
    })



  })
  output$down_data<-{
    downloadHandler(
      filename = function() {
        if(input$down_choices=="data"){
          paste0("data_",input$getdata_down,"_", Sys.Date(), ".csv")} else{
            paste0("factors_",input$getdata_down,"_", Sys.Date(), ".csv")
          }
      }, content = function(file) {
        write.table(x=data.frame(get_down()),file,append=T,quote=F,row.names=T,col.names=NA, sep=input$down_data_sep,
                    dec=input$down_data_dec)})}
  output$downloadData<-downloadHandler(
    filename = function() {
      paste("iMESc.zip", sep = "")
    },

    content = function(name1) {
      fs<-c()
      fs <-
        c('meta',"code_to_run.R","app.R","iMESc.R"

        )

      zip(zipfile = name1, files = fs)

      if (file.exists(name1)) {
        file.rename(name1, name1)
      }
    },
    contentType = "application/zip"
  )

  observeEvent(ignoreInit = T,input$desc_options,{
    vals$bag_submenu<-switch(input$desc_options,
                             'Boxplot'="boxplot",
                             'MDS'="mds",
                             'RDA'="rda",
                             'PCA'="pca")
  })
  observeEvent(ignoreInit = T,input$summ_options,
               {vals$curview_summ_options<-input$summ_options})
  uploadShpfile<-eventReactive(input$shp, {
    user_shp<-Read_Shapefile(input$shp)
    vals$bagshp<-T
    user_shp
  })
  output$save_feature<-{
    downloadHandler(
      filename = function() {
        paste0("feature_shape","_", Sys.Date())
      }, content = function(file) {
        saveRDS(filtershp(),file)
      })

  }
  observeEvent(ignoreInit = T,input$shp_view,{
    showModal(
      shp_tool()
    )
  })
  output$shp_help<-renderUI({

    div(id="shp_help",
        strong("1. Upload "),
        popify(a("shape files*"),"Shape files",options=list(style="width: 600px"), placement = "right",
               HTML(
                 paste0(
                   "Shapefiles are a simple, nontopological format for storing the geometric location and attribute information of geographic features. The shapefile format defines the geometry and attributes of geographically referenced features in three or more files with specific file extensions that should be stored in the same project workspace. Requires at least three files:",
                   div(HTML(paste0(hr()))),
                   div(HTML(paste0(strong(".shp --")," The main file that stores the feature geometry; required."))),
                   div(HTML(paste0(strong(".shx --")," The index file that stores the index of the feature geometry; required."))),

                   div(HTML(paste0(strong(".dbf --")," The dBASE table that stores the attribute information of features; required."))),
                   div(HTML(paste0(hr()))),
                   div(HTML(paste0("There is a one-to-one relationship between geometry and attributes, which is based on record number. Attribute records in the dBASE file must be in the same order as records in the main file. "))),
                   div(HTML(paste0(em(
                     "Each file must have the same prefix, for example, basin.shp, basin.shx, and basin.dbf"
                   ))))





                 )
               )


        )," at once",




    )

  })
  shp_tool<-reactive({
    div(id="shp_toobox",style="width: 600px",
        modalDialog(
          div(
            tabsetPanel(id="shp_tab",
                        tabPanel("View and download",value="tab_view",
                                 uiOutput("shp_view_down")),
                        tabPanel("Create Shape",value="tab_create",
                                 sidebarLayout(
                                   sidebarPanel(width=5,style="height:320px",
                                                div(
                                                  uiOutput('shp_help'),
                                                  fileInput(inputId = "shp", NULL, multiple = TRUE, accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj'), width="200px")),
                                                uiOutput("shp_feature1")
                                   ),
                                   mainPanel(width=7,
                                             div(
                                               uiOutput("SHP_plot")

                                             )
                                   ))
                        ),
                        tabPanel("Crop Shapes",value="tab_crop",
                                 uiOutput("shp_crop")),
                        column(12,style="align: center",uiOutput("shp_control"))

            )
          ),
          title=strong("Shapefile toolbox"),
          easyClose = T
        )
    )
  })
  observeEvent(ignoreInit = T,input$shp_view_down,{
    vals$shp_datalist<-input$shp_data_view_down
  })
  observeEvent(ignoreInit = T,input$shp_attr_view_down,{
    vals$shp_attr_view_down<-input$shp_attr_view_down
  })
  output$shp_view_down<-renderUI({

    div(em(icon("fas fa-lightbulb"),"Optimize your datalist creation speed by downloading the geometric shape as an .rds file. This file format loads quickly and avoids the need to create the shape from scratch each time you need it."),
        sidebarLayout(
          sidebarPanel(pickerInput("shp_data_view_down","1. Select the Datalist:",choices=names(vals$saved_data), width="200px", selected=vals$shp_datalist),
                       pickerInput("shp_attr_view_down","2. Select the Attribute:",choices=list(
                         "Base-Shape"="base_shape",
                         "Layer-Shape"="layer_shape",
                         "Extra-Shape"="extra_shape"
                       ), width="200px", selected=vals$shp_attr_view_down),
                       uiOutput("download_shape_button")),
          mainPanel(
            uiOutput("shapeprint"),
            uiOutput("shape_view"))





        )
    )
  })
  output$shape_view<-renderUI({
    req(input$shp_data_view_down)
    req(input$shp_attr_view_down)
    shape<-attr(vals$saved_data[[input$shp_data_view_down]],input$shp_attr_view_down)
    req(length(shape)>0)
    renderPlot({ggplot(st_as_sf(shape)) + geom_sf()+
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))})
  })
  output$download_shape_button<-renderUI({
    req(input$shp_data_view_down)
    req(input$shp_attr_view_down)
    shape<-attr(vals$saved_data[[input$shp_data_view_down]],input$shp_attr_view_down)
    req(length(shape)>0)
    downloadButton("download_shape","Download")
  })
  output$download_shape<-{
    downloadHandler(
      filename = function() {
        paste0("feature_shape","_", Sys.Date())
      }, content = function(file) {
        saveRDS(attr(vals$saved_data[[input$shp_data_view_down]],input$shp_attr_view_down),file)
      })

  }
  output$shp_crop<-renderUI({
    choices<-layers_choices2()
    div(
      validate(need(length(choices)>1,"Requires a Datalist with at least two shapes")),

      sidebarPanel(
        div(style="margin: 15px",
            div(strong("1. Shape A:")),
            pickerInput("croplayerA",NULL,  choices, width="150px")),
        div(uiOutput("croplayerA_plot"))
      ),
      sidebarPanel(
        uiOutput("croplayer_B"),
        div(uiOutput("croplayerB_plot"))
      ),
      sidebarPanel(
        div(style="height: 217px",
            uiOutput("croplayer_C"))
      )
      ,

    )
  })
  getcrop<-reactive({
    req(length(shapes_list())>0)
    new<-shapes_list()
    req(input$croplayerA)
    req(input$croplayerB)
    a<-st_as_sf(new[[input$croplayerA]])
    b<-st_as_sf(new[[input$croplayerB]])
    st_combine( st_crop(a,b))
  })
  output$croplayer_C<-renderUI({
    res<-getcrop()
    div(style="margin-top: 20px",
        div(strong("3. New shape:"),style="margin-bottom: 20px"),
        renderPlot({
          par(mar=c(0,0,0,0))
          plot(res, col="gray")
        },height = 150)
    )
  })
  output$croplayerA_plot<-renderUI({
    req(input$croplayerA)
    req(length(shapes_list())>0)
    renderPlot({
      plotshape(shapes_list()[[input$croplayerA]])
    },height = 150)
  })
  output$croplayerB_plot<-renderUI({
    req(input$croplayerB)
    req(length(shapes_list())>0)
    renderPlot({
      plotshape(shapes_list()[[input$croplayerB]])
    },height = 150)
  })
  shapes_list<-reactive({
    base_shape<-attr(vals$saved_data[[input$shp_datalist]],"base_shape")
    layer_shape<-attr(vals$saved_data[[input$shp_datalist]],"layer_shape")
    eshape<-attr(vals$saved_data[[input$shp_datalist]],"extra_shape")
    pic<-which(unlist(lapply(list(base_shape,layer_shape),function(x)length(x)>0)))
    eshape[['Base Shape']]<-base_shape
    eshape[['Layer Shape']]<-layer_shape
    new=c(eshape)
    new
  })
  output$croplayer_B<-renderUI({
    choices<-layers_choices2()
    pic<-which(choices!=input$croplayerA)
    div(style="margin: 15px",
        div(strong("2. Shape B:")),
        div(pickerInput("croplayerB",NULL,  choices[pic], width="150px"))

    )

  })
  output$shp_control<-renderUI({
    req(input$shp_tab)
    req(input$shp_tab%in%c('tab_create','tab_crop'))
    #req(length(input$shp$datapath)!=0|input$shp_tab=="tab_crop")
    if(input$shp_tab=="tab_crop"){
      choices<-layers_choices2()
      validate(need(length(choices)>1,"Requires a Datalist with at least two shapes"))
    }
    div(class="well",
        div(
          inline(pickerInput("shp_include","4. Include shape as:", c("Base-Shape","Layer-Shape","Extra-Shape"),width="150px")),
          inline(uiOutput("shp_datalist")),
          inline(uiOutput("extra_layer_newname")),
          tipify(bsButton("add_shape",span(icon(verify_fa = FALSE,name=NULL,class="fas fa-map"),icon(verify_fa = FALSE,name=NULL,class="fas fa-arrow-circle-right")), style='button_active'),"Click to add the Shape into the selected Datalist"),

          inline(
            tipify(downloadButton("save_feature",label =NULL),"Save shape as a single object. This file can be uploaded when creating a Datalist as base_shape or layer_shape.")
          )
        )
    )
  })
  output$extra_layer_newname<-renderUI({
    req(input$shp_include=='Extra-Shape')

    textInput("extra_layer_newname", "5. Name of of layer",bag_extralayer(), width="200px")
  })
  observeEvent(ignoreInit = T,input$add_shape,{
    shape<- if(input$shp_tab=="tab_create"){
      filtershp()
    }else{
      getcrop()
    }
    res<-switch(input$shp_include,
                "Base-Shape"={attr(vals$saved_data[[input$shp_datalist]],"base_shape")<-shape},
                "Layer-Shape"={attr(vals$saved_data[[input$shp_datalist]],"layer_shape")<-shape},
                'Extra-Shape'={
                  attr(vals$saved_data[[input$shp_datalist]],"extra_shape")[[input$extra_layer_newname]]<-shape
                }
    )
    # removeModal()
    res

  })
  output$SHP_plot<-renderUI({
    req(length(filtershp())>0)
    div(
      strong("3. New shape:"),
      renderPlot({
        ggplot(st_as_sf(filtershp())) + geom_sf()+
          theme(panel.background = element_rect(fill = "white"),
                panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))


      }, height = 300)
    )
  })
  output$shp_datalist<-renderUI({
    req(length(vals$saved_data)>0)
    pickerInput("shp_datalist","5. Select the Datalist:",choices=names(vals$saved_data), width="200px", selected=vals$shp_datalist)
  })
  observeEvent(ignoreInit = T,input$shp_datalist,{
    vals$shp_datalist<-input$shp_datalist
  })
  observeEvent(ignoreInit = T,input$close_shp,{

    removeModal()
  })
  output$shp_feature1<-renderUI({

    req(isTRUE(vals$bagshp))
    atributos_shp<-attributes(uploadShpfile())$names
    div(
      div(strong("2. Filter the features:")),
      splitLayout(
        div(
          div(a("feature 1:")),
          pickerInput('shp_feature1',NULL,choices=c("None",atributos_shp))
        ),
        uiOutput("shp_feature2")
      )
    )
  })
  filtershp<-reactive({

    bacias<-    uploadShpfile()
    req(isTRUE(vals$bagshp))
    req(input$shp_feature1)
    req(input$shp_feature2)
    if(input$shp_feature1!="None"&input$shp_feature2!="None")
    {
      bacias<-uploadShpfile()
      bacias<-bacias[bacias[[input$shp_feature1]]==input$shp_feature2,]

    }
    bacias
  })
  output$shp_feature2<-renderUI({
    req(isTRUE(vals$bagshp))
    lev_attrs<-unique(uploadShpfile()[[input$shp_feature1]])
    div(
      div(a("feature 2:")),
      pickerInput('shp_feature2',NULL,choices=c("None",lev_attrs))
    )
  })
  output$down_csv_semmi<-renderUI({
    if(input$down_sep==';'){
      radioButtons("down_dec",strong("dec",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                   choiceValues =list(".",","),
                   choiceNames=list(
                     "dot","comma"), selected=vals$down_dec)} else{
                       radioButtons("down_dec",strong("dec",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                                    choiceValues =list("."),
                                    choiceNames=list("dot"), selected=vals$down_dec
                       )
                     }

  })
  observeEvent(ignoreInit = T,input$down_dec,{
    vals$down_dec<-input$down_dec
  })
  observeEvent(ignoreInit = T,input$down_sep,{
    vals$down_sep<-input$down_sep
  })
  output$down_csv<-renderUI({
    req(input$down_type=='.csv')
    splitLayout(
      column(12,
             radioButtons("down_sep",strong("sep",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"the field separator string. Values within each row of x are separated by this string.", options=list(container="body"))),
                          selected=vals$down_sep,
                          choiceValues =list(",",";"),
                          choiceNames =list(
                            "comma",
                            "semicolon"
                          )
             )),
      column(12,
             uiOutput("down_csv_semmi"),
             uiOutput("down_csv_comma")

      )

    )
  })
  observeEvent(ignoreInit = T,input$down_type,{
    vals$down_type<-input$down_type
  })
  downcenter<-reactive({


    modalDialog(

      column(12,
             h5(strong(vals$hand_down)),
             splitLayout(cellWidths = c("30%","70%"),
                         column(12,
                                radioButtons("down_type",strong("format",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"file extension", options=list(container="body"))),c(".xlsx",".csv"), selected=vals$down_type)),
                         uiOutput("down_csv"),

             ),
             column(12,
                    downloadButton("download_action",NULL,icon=icon(verify_fa = FALSE,name=NULL,class="fas fa-download"),style="width: 50%"))

      )

      ,

      title=h4(icon(verify_fa = FALSE,name=NULL,class="fas fa-download"),strong("Download")),
      size="m",
      easyClose = T

    )
  })


  output$download_action<-{
    downloadHandler(
      filename = function() {
        paste0(vals$hand_down,"_", Sys.Date(), input$down_type)
      }, content = function(file) {
        if(input$down_type==".csv"){
          write.table(x=data.frame(getdown()),file,append=T,quote=F,row.names=T,col.names=NA, input$down_sep,
                      dec=input$down_dec)
        }
        if(input$down_type==".xlsx"){

          write_xlsx(cbind(id=rownames(getdown()),getdown()), file)
        }
        removeModal()

      })

  }
  observeEvent(ignoreInit = T,input$data_upload,{
    output$remove_sp<-renderUI({
      div(style="margin-top:10px; ",
          div(style="margin-top: 10px;",span("Value-based remotion",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Check to select a filter"))
          ),
          div(
            class="cogs_in",style=" height: 180px",
            div(uiOutput("pct_rareabund")),
            div(uiOutput("pct_rarefreq")),
            div(inline(uiOutput("pct_raresing")))

          )

      )
    })
  })
  output$pct_rareabund<-renderUI({
    div(style="margin-left: 5px",
        tipify( icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Remove variables with an value less than x-percent of the total","right"),
        inline( checkboxInput("rareabund","Abund<",F, width='70px')),
        inline(uiOutput("p_rareabund")),

        bsTooltip("pct_rare","Percentage", options=list(container="body"))
    )
  })
  output$p_rareabund<-renderUI({
    req(isTRUE(input$rareabund))
    div( inline(numericInput('pct_rare', NULL, value =0.1, width='70px')),"%")
  })
  output$p_rarefreq<-renderUI({
    req(isTRUE(input$rarefreq))
    div(inline(numericInput('pct_prev', NULL, value =0.1, width='70px')),"%")
  })
  output$pct_rarefreq<-renderUI({
    div(style="margin-left: 5px",
        tipify( icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Remove variables occurring in less than  x-percent of the number of samples","right"),
        inline(checkboxInput("rarefreq","Freq<",F, width='70px')),
        inline(uiOutput("p_rarefreq")),

        bsTooltip("pct_prev","Percentage")
    )
  })
  output$pct_raresing<-renderUI({
    #req(sum(apply(data_cogs$df,2, is.integer), na.rm=T)==0)
    div(style="margin-left: 5px",
        tipify( icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Requires a counting data. Remove variables occurring only once","right"),
        inline(checkboxInput("raresing","Singletons",F, width='100px')),
        inline(
          conditionalPanel("input.raresing==T",{
            uiOutput("war_raresing")
          })
        )

    )
  })
  dataraw<-reactive({
    req(input$up_or_ex)
    if (input$up_or_ex == 'use example data') {
      data <-data.frame(fread("inst/www/nema_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
      #data <-data.frame(fread("DADOS SANSED_OFICIAL CENPES.csv", stringsAsFactors = T))
      rownames(data)<-data[, 1]
      data[, 1]<-NULL


    } else {
      # input<-list()
      # input$filedata$datapath<-"data.xlsx"
      # input$sheet_data<-"data"
      validate(need(length(input$filedata$datapath)!=0,"Data file is required"))
      if(length(grep(".xlsx",input$filedata$datapath))>0) {

        df<-read_excel(input$filedata$datapath, sheet = input$sheet_data,na=c("","NA"))
        data<-data.frame(df)
        cnames<-iconv(colnames(df), from = 'UTF-8', to = 'ASCII//TRANSLIT')
        colnames(data)<-cnames
        rownames(data)<-make.unique(iconv(data[,1], from = 'UTF-8', to = 'ASCII//TRANSLIT'))
        data[, 1]<-NULL
        facs<-which(unlist(lapply(data,class))=="character")
        facs2<-data[,facs]
        newfacs<-lapply(facs2,function(x) factor(x))
        data[names(newfacs)]<-newfacs
        #class(data)
        #res<-data.frame(data[,1:3],stringsAsFactors = TRUE)
        #str(as.data.frame(as.matrix(res),stringsAsFactors = TRUE))
      } else{
        data <-
          data.frame(fread(input$filedata$datapath, stringsAsFactors = T,na.strings=c("","NA"),header=T))

        rownames(data)<-data[, 1]
        data[, 1]<-NULL
      }



    }
    #input<-list()
    #input$filedata$datapath<-"som_tab.csv"
    #data<- data.frame(fread('som.csv', stringsAsFactors = T,na.strings=c("","NA")))


    pic_log<-which(unlist(lapply(data,is.logical)))
    if(length(pic_log)>0){
      data[,  pic_log ]<- do.call(data.frame,lapply(data[, pic_log, drop=F], function(x) as.factor(x)))
    }


    remove_IDs<-which(rowSums(is.na(data))==ncol(data))
    if(length(remove_IDs)>0){
      data<-data[-remove_IDs,,drop=F]}


    data
  })
  read_labels<-reactive({

    if (input$up_or_ex == 'upload') {
      if(length(input$labels$datapath)!=0){
        if(length(grep(".xlsx",input$labels$datapath))>0) {

          df<-read_excel(input$labels$datapath, sheet = input$sheet_fac,na=c("","NA"))
          data<-data.frame(df)
          cnames<-iconv(colnames(df), from = 'UTF-8', to = 'ASCII//TRANSLIT')
          colnames(data)<-cnames
          rownames(data)<-make.unique(iconv(data[,1], from = 'UTF-8', to = 'ASCII//TRANSLIT'))
          data[, 1]<-NULL

          newfacs<-lapply(data,function(x) factor(x))
          data[names(newfacs)]<-newfacs
          labels<-data
          #class(data)
          #res<-data.frame(data[,1:3],stringsAsFactors = TRUE)
          #str(as.data.frame(as.matrix(res),stringsAsFactors = TRUE))
        } else{
          labels <-
            data.frame(fread(input$labels$datapath, stringsAsFactors = T,na.strings=c("","NA"), header=T))
          rownames(labels)<-labels[, 1]

          #colnames(labels)[1]<-"id"
          labels[, 1]<-NULL
          labels[which(unlist(lapply(labels, is.numeric)))] <-
            do.call("data.frame", lapply(labels[which(unlist(lapply(labels, is.numeric)))], function(x) as.factor(x)))
        }


      } else{
        req(length(dataraw())>0)
        labels<-data.frame(id= as.factor(rownames(dataraw())))
        rownames(labels)<- rownames(dataraw())

      } } else {
        labels <-data.frame(fread("inst/www/factors_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
        #labels <-data.frame(fread("DADOS SANSED_OFICIAL CENPES.csv", stringsAsFactors = T))

        rownames(labels)<-labels[, 1]
        #colnames(labels)[1]<-"id"
        labels[, 1]<-NULL
        labels[which(unlist(lapply(labels, is.numeric)))] <-
          do.call("data.frame", lapply(labels[which(unlist(lapply(labels, is.numeric)))], function(x) as.factor(x)))




      }

    labels

  })
  coords<-reactive({
    datao<-dataraw()
    if (input$up_or_ex == 'use example data') {
      coords <-data.frame(fread("inst/www/coords_araca.csv"))
      rownames(coords)<-coords[, 1]
      coords[, 1]<-NULL
    } else {
      #input<-list()
      #input$coords$datapath<-'data - Copia.xlsx'
      #input$sheet_fac<-1
      if(length(input$coords$datapath)>0){
        if(length(grep(".xlsx",input$coords$datapath))>0) {

          df<-read_excel(input$coords$datapath, sheet = input$sheet_coord,na=c("","NA"))[1:3]
          data<-data.frame(df)
          cnames<-iconv(colnames(df), from = 'UTF-8', to = 'ASCII//TRANSLIT')
          colnames(data)<-cnames
          coords<-data
          rownames(coords)<-coords[, 1]
          coords[, 1]<-NULL
        } else{
          coords<-data.frame(fread(input$coords$datapath))[1:3]
          rownames(coords)<-coords[, 1]
          coords[, 1]<-NULL
        }


      } else{coords=NULL}}

    #coords<-read.csv("04_coords.csv",sep=";", row.names=1)
    coords[rownames(datao),]
  })
  base_shape<-reactive({
    if (input$up_or_ex == 'use example data') {
      get(gsub(" ","",capture.output(load("inst/www/base_shape_araca",verbose=T))[2]))
      #get(gsub(" ","",capture.output(load("0005_base_shape",verbose=T))[2]))
    } else {
      if(length(input$base_shape$datapath)>0){
        t<-try({get(gsub(" ", "", capture.output(
          load(input$base_shape$datapath, verbose = T)
        )[2]))})
        if("try-error" %in% class(t)) {t<-readRDS(input$base_shape$datapath) }
        t

      }else{NULL}

    }
  })
  layer_shape<-reactive({

    if (input$up_or_ex == 'use example data'){
      get(gsub(" ","",capture.output(load("inst/www/layer_shape_araca",verbose=T))[2]))
      #get(gsub(" ","",capture.output(load("0006_layer_shape",verbose=T))[2]))
    } else {
      if(length(input$layer_shape$datapath)>0){
        t<-try({get(gsub(" ", "", capture.output(
          load(input$layer_shape$datapath, verbose = T)
        )[2]))})
        if("try-error" %in% class(t)) {t<-readRDS(input$layer_shape$datapath) }
        t

      }else{NULL}

    }})
  observeEvent(ignoreInit = T,input$insert_end,{

    req(input$tabs)
    if(input$up_or_ex=="upload"){
      # validate(need(length(input$labels$datapath)>0,"error"))
      validate(need(length(input$filedata$datapath)>0,"error"))
    }
    datalist<-getdatalist()
    vals$saved_data<-c(vals$saved_data, datalist)
    if(input$up_or_ex == 'use example data'){
      envi <-data.frame(fread("inst/www/envi_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
      rownames(envi)<-envi[, 1]
      envi[, 1]<-NULL
      envi<-data_migrate(datalist[[1]],envi,"envi_araca")
      vals$saved_data[[length(vals$saved_data)+1]]<-envi
      names(vals$saved_data)[[length(vals$saved_data)]]<-"envi_araca"
    }
    names(vals$saved_data)<-make.unique(names(vals$saved_data))
    removeModal()
    vals$cur_data<-input$data_name
    if(input$tabs=="menu_intro"){
      delay(500,    updateTextInput(session,"tabs",value="menu_upload"))

    }
    runjs("Shiny.setInputValue('last_btn', 'fade');")

  })


  #screeplothelpmodal
  {
    screeplothelpmodal<-function() {
      modalDialog(
        div(textscreeplot())
        ,
        title = "Scree plot",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(ignoreInit = T,input$screeplothelp, {
      showModal(screeplothelpmodal())
    })


  }
  #voteshelpmodal
  {
    voteshelpmodal<-function() {
      modalDialog(
        div(textvotes())
        ,
        title = "Votes on the best number of clusters",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(ignoreInit = T,input$voteshelp, {
      showModal(voteshelpmodal())
    })


    output$voteshelp<-renderText({
      if (input$votesh %% 2) {
        paste0(
          br(),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            code("K"),
            "is passed to the",
            code("max.nc"),
            "argument"
          ),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            code("min.nc"),
            "argument is fixed to 2"
          ),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            code('index'),
            "argument fixed to:",
            code(
              "kl",
              "ch",
              "hartigan",
              "ccc",
              "scott",
              "marriot",
              "trcovw",
              "tracew",
              "friedman",
              "rubin",
              "cindex",
              "db",
              "silhouette",
              "duda",
              "pseudot2",
              "beale",
              "ratkowsky",
              "ball",
              "ptbiserial",
              "gap",
              "frey",
              "mcclain",
              "gamma",
              "gplus",
              "tau",
              "dunn",
              "sdindex",
              "dindex",
              "sdbw"
            ),
            ". Only indexes running safely are computed."
          ),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            "The remaining arguments are fixed to their default values;"
          ),
          h4("NbClust {NbClust}"),
          getHelp('NbClust')
        )
      }
    })
  }
  #hchelpmodal
  {
    hchelpmodal<-function() {
      modalDialog(
        div(textclustering())
        ,
        title = "Hierarchical Clustering",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(ignoreInit = T,input$hchelp, {
      showModal(hchelpmodal())
    })




  }
  #removemodal
  {
    removemodal<-function() {
      modalDialog(
        textremove(),
        title = "Remove",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(ignoreInit = T,input$transfhelp, {
      showModal(transfmodal())
    })



    observeEvent(ignoreInit = T,input$Remove, {
      showModal(removemodal())
    })
  }
  #scalehelpmodal
  {
    scalehelpmodal<-function() {
      modalDialog(
        textscale(),
        title =  h4(strong("Scale")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$scalehelp, {
      showModal(scalehelpmodal())
    })

    output$scalehelp_out<-renderText({
      if (input$scalehelph %% 2) {
        paste0(
          br(),
          h4("scale {base}"),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            code("scale"),
            "argument",
            " fixed to",
            code("TRUE")
          ),
          getHelp(help(scale, "base"))

        )
      }
    })


  }
  #hclustmodal
  {
    hclustmodal<-function() {
      modalDialog(
        texthclust(),
        title = h4(strong("Hierarchical Clustering")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$hclusthelp, {
      showModal(hclustmodal())
    })

    output$hclusthelp<-renderText({
      if (input$hclusth %% 2) {
        paste0(
          br(),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            "Argument ",
            code("members"),
            " is fixed to =",
            code("NULL")
          ),

          h4("hclust {stats}"),
          getHelp('hclust')
        )
      }
    })


  }
  {
    pcahelpmodal<-function() {
      modalDialog(
        textpcahelp(),
        title = "pcahelp",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$pcahelp, {
      showModal(pcahelpmodal())
    })
    output$pcahelphelp<-renderText({
      if (input$pcahelph %% 2) {
        paste0(
          br(),
          h4("prcomp  {base}"),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            "Default for the S3 method. The 'center' and 'scale' arguments are set in the panel",code("3.1 Tranformation"),"from the dashboard."
          ),

          getHelp('prcomp')
        )
      }
    })

  }
  #mdshelpmodal
  {
    mdshelpmodal<-function() {
      modalDialog(
        textmdshelp(),
        title = "mdshelp",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$mdshelp, {
      showModal(mdshelpmodal())
    })

    output$mdshelphelp<-renderText({
      if (input$mdshelph %% 2) {
        paste0(
          br(),
          h4("Multidimensional Scaling  {vegan}"),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            "Only",
            code("distance"),
            "argument is passed to",
            code("metaMDS"),
            "function from 'vegan'",
            " . The remaining arguments are fixed to their default values;'"
          ),

          getHelp('metaMDS')
        )
      }
    })


  }

  #rfbphelpmodal
  {
    rfbphelpmodal<-function() {
      modalDialog(
        textrfbphelp(),
        title = "Split moving window",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$rfbphelp, {
      showModal(rfbphelpmodal())
    })




  }
  #bphelpmodal
  {
    bphelpmodal<-function() {
      modalDialog(
        textbphelp(),
        title = "Split moving window",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$bphelp, {
      showModal(bphelpmodal())
    })




  }
  output$decostand<-renderText({
    if (input$decostand %% 2) {
      paste0(
        br(),
        h4("decostand {vegan}"),

        p(
          icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
          "Only",
          code("method"),
          "argument is passed to",
          code("decostand"),
          "The remaining arguments are fixed to their default values;"
        ),
        getHelp('decostand')
      )


    } else if (input$vegan %% 2) {
      paste0(br(),
             h4(h4("vegan {package}")),
             getHelp('vegan'))
    }
  })
  {
    basemodal<-function() {
      modalDialog(
        textbase(),
        title = "base shape",
        footer = column(12,actionButton("base_back","back"),modalButton("close")),
        size = "l",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$basehelp, {
      showModal(basemodal())
    })



  }
  {
    layermodal<-function() {
      modalDialog(
        textlayer(),
        title = "layer shape",
        footer =column(12,actionButton("layer_back","back"),modalButton("close")),
        size = "l",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$layerhelp, {
      showModal(layermodal())
    })




  }


  #######

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



  observeEvent(ignoreInit = T,input$fheight,{vals$cur_fheight<-input$fheight})
  observeEvent(ignoreInit = T,input$fwidth,{vals$cur_fwidth<-input$fwidth})
  observeEvent(ignoreInit = T,input$fres,{vals$cur_fres<-input$fres})
  observeEvent(ignoreInit = T,input$pointsize,{vals$cur_pointsize<-input$pointsize})
  observeEvent(ignoreInit = T,input$fformat,{vals$cur_fformat<-input$fformat})
  get_plot<-reactive({

  })

  getdata_upload<-reactive({
    req(is.data.frame(data_cogs$df))
    data_cogs$df
  })

  getdatalist<-reactive({
    data=dataraw()
    factors<-attr(data,"factors")
    factors_in<-read_labels()[rownames(data),,drop=F]
    notfound<-which(!rownames(data)%in%na.omit(rownames(factors_in)))
    if(length(notfound)>0){
      output$factors_missing<-renderUI({
        div(style="width: 400px; height: 80px",
            div(strong("Error",style="color: red"),"The IDs below exist in the Data-Attribute, but not in the",strong('Factor-Attribute')),
            renderPrint(data.frame(IDs=rownames(data)[notfound]))
        )
      })
    } else{
      output$factors_missing<-renderUI(NULL)
    }
    validate(need(nrow(factors_in)==nrow(data),
                  ''))


    # renderPrint()
    #data<-read.csv("som.csv",sep=";", stringsAsFactors = T)
    if (any(names(datastr(data)$type.vars) == "factor")){

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


    if(input$up_or_ex == 'use example data'){
      attr(data,"filename")<-'nema_araca.csv'
      attr(data,"datalist")<-paste("nema_araca")
    } else {attr(data,"filename")<-input$data_name
    attr(data,"datalist")<-input$data_name

    }
    validate(need(sum(rownames(factors_in)%in%rownames(data))>0,"Numeric-Attribute and Factors-Attribute must have conforming IDs (first column). Check these files and reload again;"))
    attr(data,"factors")<-factors_in
    attr(data,"coords")<-coords()[rownames(data),]
    attr(data,"base_shape")<-base_shape()
    attr(data,"layer_shape")<-layer_shape()

    datalist = list(data)
    if(input$up_or_ex == 'use example data'){
      names(datalist)<-'nema_araca'
    } else{
      names(datalist)<-input$data_name
    }

    datalist
  })
  observe({
    vals$last_cog<-input$last_btn%in%c("tools_drop1",
                                       "tools_drop2",
                                       "tools_drop3",
                                       "tools_drop4",
                                       "tools_drop5",
                                       "tools_drop6",
                                       "tools_drop7",
                                       "tools_drop8",
                                       "tools_drop9",
                                       "sidepanel")})
  change_list<-reactive({
    list(input$dropID_tool1,input$dropID_tool2,input$dropID_tool3,input$dropID_tool4,input$dropID_tool5,input$dropID_tool6,input$dropID_tool7,input$dropID_tool9)
  })
  observeEvent(ignoreInit = T,input$hist_tabs,{
    vals$hist_tabs<-input$hist_tabs
  })
  output$histo_plot<-renderUI({
    data0<-vals$saved_data[[vals$cur_data]]
    not_num<-any(!sapply(data0,is.numeric))
    # if(length(not_num)>0){data[not_num]<-data.frame(lapply(data[not_num],as.numeric))}
    validate(need(isFALSE(not_num),
                  paste0("Error: Mixed character and numeric variables in ",
                         vals$cur_data,
                         " Datalist. Recreate it by downloading their attributes from Data-Bank menu and uploading them in 'Create Datalist' (Pre-processing tools)")
    ))


    #if(is.null(vals$hist_tabs)){vals$hist_tabs<-"Summary"}
   # validate(need(nrow(vals$saved_data[[vals$cur_data]])<=1000,"This functionality is only available for data with less than 1000 observations"))
    column(12,style="padding: 5px; font-size: 12px",
           column(12,em("Track changes"),style="background: SeaGreen; color: white"),
           tabsetPanel(
             id="hist_tabs",selected="Summary",
             tabPanel("Changes",uiOutput("track_change")),
             tabPanel("Summary",uiOutput("hist_d0")),
             tabPanel("Str",uiOutput("hist_str")),
             tabPanel("Data",uiOutput("hist_d1")),
             tabPanel("colSums",uiOutput("hist_d2")),
             tabPanel("rowSums",uiOutput("hist_d3")),
             tabPanel("Missing values",
                      #uiOutput("hist_d4"),
                      div(style="overflow-x: scroll;height:300px;overflow-y: scroll",
                          uiOutput("hist_d5")
                      )
             )

           )
    )
  })
  output$hist_str<-renderUI({
    div(
      checkboxInput("check_str_num","str-> Numeric-Attribute"),
      uiOutput('out_str_num'),

      checkboxInput("check_str_factor","str-> Factor-Attribute"),
      uiOutput('out_str_factor')

    )
  })
  output$out_str_num<-renderUI({
    req(isTRUE(input$check_str_num))
    div(style="height: 200px; overflow-y: scroll",
      renderPrint({
        str(data_cogs$df)
      })
    )
  })
  output$out_str_factor<-renderUI({
    req(isTRUE(input$check_str_factor))
    div(style="height: 200px; overflow-y: scroll",
        renderPrint({
          str(attr(data_cogs$df,"factors"))
        })
    )
  })
  output$hist_d5<-renderUI({
    req(is.data.frame(data_cogs$df))
    data=data_cogs$df

    res0<-res<-which(is.na(data), arr.ind=TRUE)
    if(length(res0)>0){
      for(i in 1:nrow(res)){
        res0[i,1]<-rownames(data)[res[i,1]]
        res0[i,2]<-colnames(data)[res[i,2]]
      }
      colnames(res0)<-c("ID","Variable")
      rownames(res0)<-NULL
      res<-data.frame( table(res0[,2]))
      colnames(res)<-c("Variable","Missing")
      rownames(res)<-res[,1]
      pic<-colnames(data_cogs$df)[which(colnames(data_cogs$df)%in%res[,1])]
      res[,1]<-NULL
      if(length(pic)>0)
        renderPrint(res[pic,, drop=F])
    }else{div(em("No missing values"))}
  })
  output$hist_d0<-renderUI({
    div(
      renderPrint(attr(vals$saved_data[[input$data_upload]],"scale")),
      renderPrint({
        #req(is.data.frame(data_cogs$df))
        data=data_cogs$df

        nas=sum(is.na(unlist(data)))
        n=data.frame(rbind(Param=paste('Missing values:', nas)))

        a<-data.frame(rbind(Param=paste('nrow:', nrow(data)),paste('ncol:', ncol(data))))
        c<-data.frame(
          rbind(paste('min:', min(data,na.rm = T)),
                paste('mean:', mean(unlist(data),na.rm = T)),
                paste('median:', median(unlist(data),na.rm = T)),
                paste('max:', max(data,na.rm = T)))
        )


        ppsummary("-------------------")

        ppsummary(n)
        ppsummary("-------------------")

        ppsummary(a)
        ppsummary("-------------------")

        ppsummary(c)
        ppsummary("-------------------")


      })
    )


  })
  output$hist_d1<-renderUI({
    req(is.data.frame(data_cogs$df))

    div(class="map_control_style",
        column(12,class="track_changes",style="padding-top: 10px",
               column(1,),
               column(11,
                      textInput("hist_unlist_data_title",NULL,"Histogram", width="100%"),
                      tags$style(type='text/css',"#hist_unlist_data_title {padding-left: 40px; border: 0px;     box-shadow: inset 0 0px 0px;}")
               ),
               column(1,style=" height: 350px; ",
                      div(style="position: absolute; bottom: 0px;
                           ",
                          textInput("hist_unlist_data_ylab",NULL,"Frequency"),
                          tags$style(type='text/css',"#hist_unlist_data_ylab {
                                   text-align:center;
                                    background-color: transparent;
                                   transform: rotate(-90deg);
                                   border: 0px;
                                  box-shadow: inset 0 0px 0px;
                                   transform-origin: top left;
                        }")
                      )
               ),
               column(11,style=" height: 350px",
                      uiOutput("histogram_unlist_data"),
                      textInput("hist_unlist_data_xlab",NULL,"Bins", width="100%"),
                      tags$style(type='text/css',"#hist_unlist_data_xlab {text-align:center;border: 0px;    box-shadow: inset 0 0px 0px;}")
               )),
        column(12,
               column(3,align="right",
                      column(12,style='text-align:left',uiOutput("bins_histunlist_data")),
                      column(12,style='text-align:left',actionLink("down_histunlist_data","+ Download plot")),

               )
        )
    )
  })
  output$bins_histunlist_data<-renderUI({
    h<-hist(getunlist_data(), plot=F)
    numericInput("bins_histunlist_data","+ Bins",length(h$breaks),step=1,width= '80px')
  })
  observeEvent(input$down_histunlist_data,ignoreInit = T,{
    vals$hand_plot<-'data Histogram'
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  getunlist_data<-reactive({
    data<-data_cogs$df
    vec<-as.vector(unlist(data_cogs$df))
    vec
  })
  output$histogram_unlist_data<-renderUI({
    data<-data_cogs$df
    req(input$bins_histunlist_data>0)
    renderPlot({
      vec<-getunlist_data()
      gg<-gghistogram(vec,fill="gray", bins= input$bins_histunlist_data)+xlab("")+ylab("")
      vals$gghist_unlist_data<-gg+xlab(input$hist_unlist_data_xlab)+ylab(input$hist_unlist_data_ylab)+ggtitle(input$hist_unlist_data_title)
      gg
    }, height = 310)
  })
  output$hist_d2<-renderUI({
    req(is.data.frame(data_cogs$df))

    div(class="map_control_style",
        column(12,class="track_changes",style="padding-top: 10px",
               column(1,),
               column(11,
                      textInput("hist_colsums_title",NULL,"Histogram of Variable Sums", width="100%"),
                      tags$style(type='text/css',"#hist_colsums_title {padding-left: 40px; border: 0px;     box-shadow: inset 0 0px 0px;}")
               ),
               column(1,style=" height: 350px; ",
                      div(style="position: absolute; bottom: 0px;
                           ",
                          textInput("hist_colsums_ylab",NULL,"Frequency"),
                          tags$style(type='text/css',"#hist_colsums_ylab {
                                   text-align:center;
                                    background-color: transparent;
                                   transform: rotate(-90deg);
                                   border: 0px;
                                  box-shadow: inset 0 0px 0px;
                                   transform-origin: top left;
                        }")
                      )
               ),
               column(11,style=" height: 350px",
                      uiOutput("histogram_colsums"),
                      textInput("hist_colsums_xlab",NULL,"Variable Sums", width="100%"),
                      tags$style(type='text/css',"#hist_colsums_xlab {text-align:center;border: 0px;    box-shadow: inset 0 0px 0px;}")
               )),
        column(12,
               column(3,align="right",
                      column(12,style='text-align:left',uiOutput("bins_histcolsums")),
                      column(12,style='text-align:left',actionLink("down_histcolsums","+ Download plot")),

               )
        )
    )
  })
  output$bins_histcolsums<-renderUI({
    h<-hist(getcolSums(), plot=F)
    numericInput("bins_histcolsums","+ Bins",length(h$breaks),step=1,width= '80px')
  })
  observeEvent(input$down_histcolsums,ignoreInit = T,{
    vals$hand_plot<-'colSums Histogram'
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  getcolSums<-reactive({
    data<-data_cogs$df
    vec<-colSums(data)
    vec
  })
  output$histogram_colsums<-renderUI({
    data<-data_cogs$df
    req(input$bins_histcolsums>0)
    renderPlot({
      vec<-getcolSums()
      gg<-gghistogram(vec,fill="gray", bins= input$bins_histcolsums)+xlab("")+ylab("")
      vals$gghist_colsums<-gg+xlab(input$hist_colsums_xlab)+ylab(input$hist_colsums_ylab)+ggtitle(input$hist_colsums_title)
      gg
    }, height = 310)
  })
  output$hist_d3<-renderUI({
    req(is.data.frame(data_cogs$df))

    div(class="map_control_style",
        column(12,class="track_changes",style="padding-top: 10px",
               column(1,),
               column(11,
                      textInput("hist_rowSums_title",NULL,"Histogram of Observation Sums", width="100%"),
                      tags$style(type='text/css',"#hist_rowSums_title {padding-left: 40px; border: 0px;     box-shadow: inset 0 0px 0px;}")
               ),
               column(1,style=" height: 350px; ",
                      div(style="position: absolute; bottom: 0px;
                           ",
                          textInput("hist_rowSums_ylab",NULL,"Frequency"),
                          tags$style(type='text/css',"#hist_rowSums_ylab {
                                   text-align:center;
                                    background-color: transparent;
                                   transform: rotate(-90deg);
                                   border: 0px;
                                  box-shadow: inset 0 0px 0px;
                                   transform-origin: top left;
                        }")
                      )
               ),
               column(11,style=" height: 350px",
                      uiOutput("histogram_rowSums"),
                      textInput("hist_rowSums_xlab",NULL,"Observation Sums", width="100%"),
                      tags$style(type='text/css',"#hist_rowSums_xlab {text-align:center;border: 0px;    box-shadow: inset 0 0px 0px;}")
               )),
        column(12,
               column(3,align="right",
                      column(12,style='text-align:left',uiOutput("bins_histrowSums")),
                      column(12,style='text-align:left',actionLink("down_histrowSums","+ Download plot")),

               )
        )
    )
  })
  output$bins_histrowSums<-renderUI({
    h<-hist(getrowSums(), plot=F)
    numericInput("bins_histrowSums","+ Bins",length(h$breaks),step=1,width= '80px')
  })
  observeEvent(input$down_histrowSums,ignoreInit = T,{
    vals$hand_plot<-'rowSums Histogram'
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  getrowSums<-reactive({
    data<-data_cogs$df
    vec<-rowSums(data)
    vec
  })
  output$histogram_rowSums<-renderUI({
    data<-data_cogs$df
    req(input$bins_histrowSums>0)
    renderPlot({
      vec<-getrowSums()
      gg<-gghistogram(vec,fill="gray", bins= input$bins_histrowSums)+xlab("")+ylab("")
      vals$gghist_rowSums<-gg+xlab(input$hist_rowSums_xlab)+ylab(input$hist_rowSums_ylab)+ggtitle(input$hist_rowSums_title)
      gg
    }, height = 310)
  })
  output$hist_d4<-renderUI({
    div(
      renderPlot({
        req(is.data.frame(data_cogs$df))
        plothists(data_cogs$df,vals$newcolhabs, len=6)
      })
    )
  })
  output$change_background<-renderUI({
    req(isTRUE(vals$last_cog))


    div(id="change_background",

    )


  })
  output$track_change<-renderUI({
    req(input$last_btn!='tools_drop7'& input$last_btn!='tools_drop9' & input$last_btn!='tools_drop8')
    # req(is.data.frame(data_cogs$df))
    req(isTRUE(vals$last_cog))
    div(

      tags$div(div(class="tools_content",
                   tags$style(
                     paste(paste0("#transf_attr"),"td {padding: 3px;
                     text-align: left;
                     font-size:11px;
                background: #f0eeeeff}")
                   ),
                tags$style(
                  paste0("#transf_attr"),"th {padding: 3px;
                     text-align: left;
                     font-size:11px;
                background: transparent}"
                ),
                div("Changes:", style="color: #696969; margin-left: 3px"),
                div(class="cogs_in",
                    div(style="margin: 3px",
                        inline(DT::dataTableOutput('transf_attr'))
                    ))
      )))
  })
  output$showhisto<-renderUI({
    req(length(input$radio_cogs)>0)
    if(is.null(vals$cur_showhisto)){vals$cur_showhisto<-F}
    div(
      id="switch_histo",
      uiOutput("lock_back"),
      if(!input$radio_cogs%in%c('tools_drop9','tools_drop7')){
        uiOutput('histo_plot')
      }
    )
  })
  output$transf_attr<-DT::renderDataTable({
    #req(is.data.frame(data_cogs$df))
    table<-  attr(data_cogs$df, 'transf')
    req(!is.null(table))
    req(nrow(table)==8)
    rownames(table)<-c("Selected.obs",
                       "Selected.vars",
                       "Transf",
                       "Scale",
                       "Center",
                       "NA.omit",
                       "Data.impt",
                       "Factor.impt")

    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autowidth=F,dom = 't', rownames = TRUE,class ='compact cell-border'))
  })
  output$change_head_bg<-renderUI({
    #req(isTRUE(vals$show_progress_title))
    div(class="tool_title0",uiOutput("preprocess_title"))
  })
  output$preprocess<-renderUI({

    req(isTRUE(vals$lock_change))
    renderUI({
      div(
        # div(id="preprocess0", style="padding: 0px;margin:0px"),
        div(id="preprocess",em("Pre-processing tools"), style="padding: 0px;margin:0px")
        # div(id="preprocess1", style="padding: 0px;margin:0px"),

      )
    })
  })
  output$change_head_out<-renderUI({
    #req(!is.na(input$radio_cogs))
    div(
      hidden(
        div(id="change_head0",
            div(id="change_head",
                uiOutput("change_head_bg"),
                uiOutput("get_data_upload"))
        )
      ),
      div(id="tog_tools",style="padding: 15px",
          hidden(div(id="tog_tool1",
                     withSpinner(type=8,color="SeaGreen",
                                 uiOutput("tool1")
                     ))),
          hidden(div(id="tog_tool2",uiOutput("tool2"))),
          hidden(div(id="tog_tool3",uiOutput("tool3"))),
          hidden(div(id="tog_tool4",uiOutput("tool4"))),
          hidden(div(id="tog_tool5",uiOutput("tool5"))),
          hidden(div(id="tog_tool6",uiOutput("tool6"))),
          hidden(div(id="tog_tool7",uiOutput("tool7"))),
          hidden(div(id="tog_tool8",uiOutput("tool8"))),
          hidden(div(id="tog_tool9",uiOutput("tool9"))))
    )
  })
  output$preprocess_title<-renderUI({

    req(input$radio_cogs)
    req(length(input$radio_cogs)==1)
    req(length(input$last_btn)>0)
    res<-switch (input$radio_cogs,
                 "tools_drop1"={"Filter observations"},
                 "tools_drop2"={"Filter variables"},
                 "tools_drop3"={"Transformations"},
                 "tools_drop4"={"Data imputation"},
                 "tools_drop5"={"Data partition"},
                 "tools_drop6"={"Edit factors"},
                 "tools_drop7"={"Create color palette"},
                 "tools_drop8"={"Aggregate"},
                 "tools_drop9"={"Savepoint"}
    )
    vals$last_tool<-res

    div(

      inline(uiOutput('showhisto')),
      inline(div(res,class="tool_title",id='tool_title'))

    )
  })
  output$lock_back<-renderUI({
    #req(length(vals$saved_data)>0)
    div(
      div(id="fade",class="needed",
          column(12,id="fade_panel",class="fade_panel"),
      ),


      div( id="sidepanel",class="needed",column(12,id="fade_panel2",class="side_panel"))

    )

  })
  observe({
    class  = if(sum(vals$bagdata==c(T,T))==2){removeClass('fade_panel2',"border_alert")} else{
      addClass('fade_panel2',"border_alert")
    }

  })
  output$get_data_upload<-renderUI({
    req(length(vals$saved_data)>0)
    req(!input$radio_cogs%in%  c('tools_drop7','tools_drop9'))
    div(class="data_change0",

        div(id="data_change",
            inline(
              div(id="change_picker",
                  inline(pickerInput(
                    "data_upload",NULL,choices=names(vals$saved_data),  selected=vals$cur_data, width="180px", options=list(container="body")
                  ))
              )
            ),
            inline(bsButton("reset_change",icon(verify_fa = FALSE,name=NULL,class="fas fa-undo"), style="button_change", width='30px')),
            inline(uiOutput("save_changes_button"))

        ),
        bsTooltip("change_picker","Target Datalist"),
        bsTooltip("reset_change","Reset changes"),
        bsTooltip("tools_save_changes","Save Changes")
    )
  })
  observeEvent(ignoreInit = T,input$reset_change,{
    shinyjs::reset("change_head")


  })
  output$save_changes_button<-renderUI({
    class  = if(sum(vals$bagdata==c(T,T))==2){"novo"} else{"save_changes"}
    div(class=class,
        bsButton("tools_save_changes",icon(verify_fa = FALSE,name=NULL,class="fas fa-save"),style='save_button', width='30px')
    )

  })
  observe({
    req(!is.na(input$radio_cogs))
    if(!input$radio_cogs%in%  c('tools_drop7','tools_drop9')){
      shinyjs::show("data_change")
    } else{
      shinyjs::hide("data_change")
    }
  })
  observeEvent(ignoreInit = T,input$tools_drop9,{
    updateButton(session,'cogs', value=F)
    removeModal()
  })
  observeEvent(change_list(),{
    req(length(vals$last_cog)>0)
    if(isTRUE(vals$last_cog)){
      toggled$df<-T
    } else{toggled$df<-F}
  })
  observeEvent(ignoreInit = T,input$radio_cogs,{
    req(input$radio_cogs)
    req(!is.na(input$radio_cogs))
    runjs(paste0("Shiny.setInputValue('last_btn', '",input$radio_cogs,"');"))
  })
  observeEvent(ignoreInit = T,input$savechanges_return,{
    removeModal()
    updateRadioGroupButtons(session,"radio_cogs",selected=last_btn$equal[1])
    runjs(paste0("Shiny.setInputValue('radio_cogs', ",last_btn$equal[1],");"))



  })
  observeEvent(ignoreInit = T,input$savechanges_save,{
    vals$hand_save<-"Save changes"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(
      hand_save_modal()
    )
  })
  observe({
    status_changes$df<-sum(vals$bagdata==c(T,T))==2
  })
  choiceValues_preprocess<-reactive({
    list(
      'tools_drop1',
      'tools_drop2',
      'tools_drop3',
      'tools_drop4',
      'tools_drop5',
      'tools_drop8',
      'tools_drop7',
      'tools_drop9'
    )
  })
  choiceNames_preprocess<-reactive({
    list(

      div(id="tools_drop1",class="needed",
          div(
            cogs_title="Filter observations",
            class="blur",
            icon(verify_fa = FALSE,name=NULL,class="fas fa-list")
          )),
      div(id="tools_drop2",class="needed",
          div(
            cogs_title="Filter variables",
            class="blur",
            icon(verify_fa = FALSE,name=NULL,class="fas fa-list fa-rotate-90")
          )),
      div(
        id="tools_drop3",
        class="needed",
        div(cogs_title="Transformation",
            icon(verify_fa = FALSE,name=NULL,class="fas fa-hammer"))
      ),
      div(
        id="tools_drop4",
        class="needed",div(cogs_title="Data imputation",                                          img(src=na_icon,height='14',width='14',class="blur"))
      ),
      div(
        id="tools_drop5",class="needed",
        div(cogs_title="Data partition",
            img(src=split_icon,height='14',width='14',class="blur"))
      ),
      div(
        id="tools_drop8",class="needed",
        div(cogs_title="Aggregate",
            img(src=agg_icon2,height='14',width='14',class="blur"))
      ),
      div(
        id="tools_drop7",class="needed",
        div(cogs_title="Create palette",
            icon(verify_fa = FALSE,name=NULL,class="fas fa-palette"))
      ),

      div(
        id="tools_drop9",class="needed",
        div(cogs_title="Savepoint",icon(verify_fa = FALSE,name=NULL,class="fas fa-thumbtack"))
      )
    )
  })
  output$preprocess_on<-renderUI({
    req(length(vals$saved_data)>0)
    choiceValues=choiceValues_preprocess()
    choiceNames=choiceNames_preprocess()
    div(id="preprocess_on",
        inline(
          div(id="create_datalist",
              inline(div(id="exit_cogs",    uiOutput("preprocess_exit"))),
              div(class="create_datalist",

                  popify(bsButton("bank_button", strong(icon(verify_fa = FALSE,name=NULL,class="fa-sharp fa-solid fa-plus")), style="radio_cogs",size="small"),"Data Input",textinput(),options=list(container="body"))
              )
          )
        ),
        #inline(uiOutput("bank_input0")),
        inline(div(id="datalist_cogs",    uiOutput("datalist_cogs_out"))),
        inline(
          div(
            id="upload_tools",class='upload_tools',
            useShinyjs(),
            inline(div(tags$div(id="coog0",inline(radioGroupButtons("radio_cogs",justified =T,selected=NA,status="radio_cogs",choiceValues =choiceValues,
                                                                    choiceNames=choiceNames
            )))
            )))
        ))
  })
  output$preprocess_off<-renderUI({
    req(!length(vals$saved_data)>0)
    choiceValues=choiceValues_preprocess()
    choiceNames=choiceNames_preprocess()
    div(
      div(
        inline(
          div(id="create_datalist",
              div(class="create_datalist",
                  popify(div(class='save_changes',bsButton("bank_button", icon(verify_fa = FALSE,name=NULL,class="fas fa-plus"), style="radio_cogs",size="small")),"Data Input",textinput()))
          )
        ),
        inline(uiOutput("bank_input0")),
        inline(div(id="datalist_cogs",    uiOutput("datalist_cogs_out"))),

        inline(shinyjs::hidden(div(id="exit_cogs",    uiOutput("preprocess_exit")))),
        inline(
          div(
            useShinyjs(),
            inline(
              div(
                id="upload_tools",class='upload_tools',
                div(tags$div(id="coog0",style="margin-right: 36px",inline(radioGroupButtons("radio_cogs_null",justified =T,selected=NA,status="radio_cogs",choiceValues =choiceValues[-length(choiceValues)],choiceNames=choiceNames[-length(choiceValues)], disabled = T
                )))
                ))
            ),
            inline(
              tags$div(class="cogs_null",
                       style="width: 35px;
                       height: 35px;
                       right: 5px;
                       top: 16px",
                       id="coog0_NULL",
                       div(style="padding: 0px;
                 margin: 0px;
                 line-height: 30px",id="upload_tools_off",
                 radioGroupButtons("radio_cogs",justified =T,selected=NA,status="radio_cogs",choiceValues =list('tools_drop9'),
                                   choiceNames=list(
                                     div(id="tools_drop9",class="needed",div(
                                       icon(verify_fa = FALSE,name=NULL,class="fas fa-thumbtack"))))
                 ),
                 bsTooltip('tools_drop9',"Load a savepoint"),
                 bsTooltip('radio_cogs_null',"Create a datalist first to enable preprocessing tools"),

                       ))
            )
          )
        )
      )
    )

  })
  observeEvent(ignoreInit = T,input$show_mananger,{
    vals$task_open<-T
    modal_gerenciador()
  })
  observeEvent(vals$task_open_restore,{
    if(isTRUE(vals$task_open_restore)){
      vals$task_open_restore<-NULL
      modal_gerenciador()
    }
  })
  observe({
    req(is.null(vals$data_dlmX))
    vals$data_dlmX<-names(vals$saved_data)[1]
  })
  modal_gerenciador<-function(){
    showModal(
      modalDialog(
        footer =NULL,

        div(
          uiOutput("dlm_structure"),
          absolutePanel(  draggable = T, top="80px",
                          div(
                            uiOutput("gerenciador")
                          )
          )
        )
      )
    )
  }
  ns_dl_box<-NS("dlm_box")
  ns_dl_name<-NS("dlm_name")
  ns_dl_trash<-NS("dlm_trash")
  ns_dl_out<-NS("dlm_out")
  ns_attr_name<-NS("dlm_attr_name")
  ns_attr_trash<-NS("dlm_attr_trash")
  ns_attr_out<-NS("dlm_attr_out")
  ns_model_name<-NS("dlm_model_name")
  ns_model_out<-NS("dlm_model_out")
  ns_model_trash<-NS("dlm_model_trash")
  ns_feature_trash<-NS("dlm_feature_trash")
  ns_attr_down<-NS("dlm_attr_down")
  ns_feature_down<-NS("dlm_feat_down")
  dlm_gerenciador<-reactiveValues(df=F)
  my_object.size<-function(x,dlm_getsize=T){
    if(isTRUE(dlm_getsize)){
      object.size(x)
    } else{
      NULL
    }
  }
  output$gerenciador<-renderUI({
    req(length(vals$task_mananger) > 0)
    req(isTRUE(dlm_gerenciador$df))
    div(id = "gerenciador_yes",style="background-color: white;",
        div(style = "width: 500px; height: 500px; padding: 80px; ",
            h3("Are you sure?"),
            div(strong("Romeve attribute:"),
                print_mananger()),
            div(
              actionButton("remove_gerenciador", "Cancel"),
              actionButton("remove_attribute", "Confirm"))
        ))
  })
  modal_mananger<-reactive({
    dlm_gerenciador$df<-T
  })
  output$datalist_attrtree<-renderUI({
    datalist_names =   vals$dl_datalistnames

    datalist_values =  gsub("[^[:alnum:]]", "", datalist_names)
    datalist_bytes =  lapply(vals$datalist_sizes,function(xs)myformat(xs, "auto", total_size = savepoint_size()))
    datalist_links<-lapply(seq_along(datalist_names), function(i) {
      div(id = "datalist_tree",
          div(id = ns_dl_box(datalist_values[i]),class = "default_tree",
              div(
                actionLink(ns_dl_name(datalist_values[i]), datalist_names[i], class = "red-link"),
                datalist_bytes[[i]],
                actionLink(ns_dl_trash(paste0(datalist_values[i], '_', i)), icon('fas fa-trash')),

              ),
              div(id = ns_dl_out(paste0(datalist_values[i])))
          ))
    })

    datalist_links
  })
  output$datalist_total_size<-renderUI({
    req(isTRUE(input$dlm_getsize))
    column(12,
           strong("Total size="),
           myformat(savepoint_size(), "auto", total_size = savepoint_size())
    )
  })
  observeEvent(input$close_task,{
    vals$task_open<-NULL
    removeModal()
  })
  output$dlm_structure<-renderUI({
    fluidPage(id = "gerenciador_panel",
              column(12, column(10,h3("Datalist mananger")), column(2,actionLink("close_task",h3(icon("fas fa-window-close",style='background: white'), style="color: red;"))),
                     style = "background: Teal; color: white;"),
              column(12,style = "margin-top: 50px",class = "well3",
                     column(12,
                            div(checkboxInput("dlm_getsize","Calculate attribute sizes",F)),
                            uiOutput('datalist_total_size'),
                            column(12, style = "margin-left: 20px",

                                   uiOutput('datalist_attrtree')))
              )
    )
  })
  savepoint_size<-reactive({
    vals<-reactiveValuesToList(vals)
    my_object.size(vals,dlm_getsize=input$dlm_getsize)
  })
  datalist_bytes<-eventReactive(vals$data_dlmX,{
    req(vals$data_dlmX)
    if(vals$data_dlmX%in%names(vals$datalist_sizes)){
      if(length(vals$datalist_sizes[[vals$data_dlmX]])>0){
        max(unlist(vals$datalist_sizes[[vals$data_dlmX]]))
      } else{NULL}

    } else{
      NULL
    }

  })
  get_attrx_sizes<-reactive({
    attrx<-get_attributes_dl()
    res<-lapply(names(attrx), function(i) {
      my_object.size(attrx[[i]],dlm_getsize=input$dlm_getsize)
    })
    names(res)<-names(attrx)
    res
  })
  getclass_names<-function(i,attrx){
    if (i %in% c("som", "rf", "svm", "nb", "knn", "kmeans", "sgboost", "xyf")) "model-attribute"
    else if (i == "extra_shape") "shape list"
    else paste0(class(attrx[[i]])[1], collapse = "; ")
  }
  divstru_dl<-function(attrx_sizes,datalist_name,i,total_size=total_size,class_name,dlm_getsize, link=T,datalist_sizes,data_dlmX){
    div(
      inline(div(class = 'link_mananger')),
      if(isTRUE(link)){

        actionLink(ns_attr_name(paste0(datalist_name, i)), strong(i))
      }else{
        i
      },
      if(isTRUE(dlm_getsize)){
        if(i=="numeric"){
          span(myformat(datalist_sizes[[data_dlmX]]-sum(unlist(attrx_sizes)), "auto", total_size = total_size))
        } else{
          span(myformat(attrx_sizes[[i]], "auto", total_size = total_size))
        }
      },


      if(length(class_name)>0)
        if(class_name=="data.frame")
          actionLink(ns_attr_down(paste0(datalist_name, i)), icon('fas fa-download')),

      if(!i%in%c("factors","numeric")){
        actionLink(ns_attr_trash(paste0(datalist_name, i)), icon('fas fa-trash'))
      }

    )
  }
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
  divstru_attr<-function(datalist_name,i,x,obj_size,total_size){
    div(
      actionLink(ns_model_name(paste0(datalist_name, i, x)), x),
      myformat(obj_size, "auto", total_size =total_size),
      actionLink(ns_model_trash(paste0(datalist_name, i, x)), icon('fas fa-trash'))

    )
  }
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
  dim_numeric<-eventReactive(vals$data_dlmX,{
    dim(vals$saved_data[[vals$data_dlmX]])
  })
  div2<-function(attrx,datalist_name,i,dlm_getsize,class_name,names_list,total_size,data_dlmX){
    style="display: none"
    div(
      id = ns_attr_out(paste0(datalist_name, i)),
      style = style,

      div(style = "margin-left: 20px",
          div('class=', em(class_name),
              if(class_name=="data.frame"){
                if(i=="numeric"){
                  di<-dim_numeric()
                } else{
                  di<-dim(attrx[[i]])
                }
                paste0("(",paste0(di, collapse="x"),")")}

          ),
          div(if (i %in% c("som", "rf", "svm", "nb", "knn", "kmeans", "sgboost", "xyf")) div("Number of models:", em(length(names_list)))),

          if(data_dlmX!="Saved Ensembles"){
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
  datatalist_attributes<-reactive({

    req(vals$data_dlmX)
    if(isTRUE(input$dlm_getsize)){
      req(datalist_bytes())
    }

    req(get_attrx_sizes())
    req(!is.null(vals$apendto))
    attrx<-get_attributes_dl()
    attrx_sizes<-get_attrx_sizes()
    datalist_name<- gsub("[^[:alnum:]]", "", vals$data_dlmX)
    total_size=datalist_bytes()
    link<-vals$data_dlmX!="Saved Ensembles"
    lis<-list()
    withProgress( min = 0,
                  max = length(attrx),message="Listing 2...",{
                    for(i in names(attrx)) {
                      {
                        class_name<-getclass_names(i,attrx)
                        names_list<-gsub("[^[:alnum:]]", "", names(attrx[[i]]))
                        dlm_getsize<-input$dlm_getsize
                        lis[[i]]<-div(
                          id = "attr_tree",
                          divstru_dl(attrx_sizes,datalist_name,i,total_size=total_size,class_name,dlm_getsize, link,datalist_sizes=vals$datalist_sizes,data_dlmX=vals$data_dlmX),
                          if(link){
                            div2(attrx,datalist_name,i,dlm_getsize,class_name,names_list,total_size,data_dlmX=vals$data_dlmX)
                          }


                        )
                        incProgress(1)
                      }
                    }
                  })


    div(style="display: table-cell;vertical-align: top;",
        #  inline(actionLink(ns_dl_expand(vals$dl_datalist_values[vals$data_dlmX]),icon('fas fa-angle-down'))),
        inline(div(style = "margin-left: 20px;", lis)))
  })
  get_attributes_dl<-reactive({
    req(length(vals$saved_data)>0)
    attrx<-if(vals$data_dlmX=="Saved Ensembles"){
      vals$saved_ensemble} else{
        attributes(vals$saved_data[[vals$data_dlmX]])}
    if(length(attrx)>0){
      attrx<-attrx[sapply(attrx, length) > 0]

      #names(attrx)<-unique(names(attrx))


      attrx[c('names','row.names','class','nobs_ori',
              'nvar_ori','filename','datalist','transf',"data.factors")] <-NULL


      if(vals$data_dlmX!="Saved Ensembles"){
        attrx<-c(numeric=list(data.frame(NULL)),attrx)
      }
      attrx
    }})
  print_mananger<-reactive({
    req(vals$data_dlmX)
    if(vals$data_dlmX=="Saved Ensembles"){
      span(
        em(vals$data_dlmX),">",em(vals$task_mananger[1])
      )
    } else{
      if (length(vals$task_mananger) == 1) {
        span(span(em(vals$data_dlmX), ">"),
             em(vals$task_mananger[1], style = "color: red"))
      } else   if (length(vals$task_mananger) == 2) {
        span(
          em(vals$data_dlmX),">",
          em(vals$task_mananger[1]),">",
          em(vals$task_mananger[2], style = "color: red")
        )
      } else   if (length(vals$task_mananger) == 3) {
        span(
          em(vals$data_dlmX),">",em(vals$task_mananger[1]),">",
          em(vals$task_mananger[2]),">",
          em(vals$task_mananger[3], style = "color: red")

        )
      }
    }


  })
  dlm_remove1<-reactive({
    req(vals$data_dlmX%in%names(vals$saved_data))
    vals$apendto<-NULL
    runjs(paste0("Shiny.setInputValue('remove_gerenciador', 0);"))
    removeUI("#data_dlmX_bytes")
    vals$data_dlmX<-NULL
    vals$data_dlmX<-1
    vals$saved_data[[vals$data_dlmX]]<-NULL
    vals$dl_datalistnames<-names(vals$saved_data)
    vals$dl_datalist_values<-gsub("[^[:alnum:]]", "", vals$dl_datalistnames)
    names(vals$dl_datalist_values)<-vals$dl_datalistnames
    dlm_gerenciador$df<-F


  })
  observe({
    vals$dlm_a<-list(i=NULL)
  }, autoDestroy = T)
  dlm_remove_attr<-reactive({
    attr(vals$saved_data[[vals$data_dlmX]], vals$task_mananger)<-NULL
    dlm_gerenciador$df<-F
  })
  dlm_remove_attr2<-reactive({

    attr(vals$saved_data[[vals$data_dlmX]], vals$task_mananger[[1]])[[vals$task_mananger[[2]]]] <-
      NULL
    dlm_gerenciador$df<-F
  })
  dlm_remove_attr3<-reactive({
    if ("feature_rands" %in% unlist(vals$task_mananger)) {
      attr(vals$saved_data[[vals$data_dlmX]], vals$task_mananger[[1]])[[vals$task_mananger[[2]]]]$feature_rands <-
        NULL
      dlm_gerenciador$df<-F}
  })
  div_remove_all_ensemble<-reactive({
    vals$saved_ensemble<-NULL
    vals$saved_data[["Saved Ensembles"]]<-NULL
    vals$dl_datalistnames<-names(vals$saved_data)
    vals$dl_datalist_values<-gsub("[^[:alnum:]]", "", vals$dl_datalistnames)
    names(vals$dl_datalist_values)<-vals$dl_datalistnames
  })
  dlm_remove_en<-reactive({
    if(vals$task_mananger=="Remove All Ensembles"){
      div_remove_all_ensemble()
    } else{
      vals$saved_ensemble[[vals$task_mananger]]<-NULL
      if(!length(vals$saved_ensemble)>0){
        div_remove_all_ensemble()
      }


    }
    dlm_gerenciador$df<-F

  })
  observeEvent(list(vals$dl_datalistnames,input$dlm_getsize), {
    req(length(input$dlm_getsize)>0)
    datalist_names<-vals$dl_datalistnames
    res<- lapply(seq_along(datalist_names), function(name)
      my_object.size(vals$saved_data[[name]],dlm_getsize=input$dlm_getsize))
    names(res)<-datalist_names
    vals$datalist_sizes <-res

  },ignoreInit = T)
  observeEvent(vals$dlm_a$i, {

    i<-vals$dlm_a$i

    removeUI("#data_dlmX_bytes")

    datalist_names<-vals$dl_datalistnames
    datalist_values<-vals$dl_datalist_values
    lapply(seq_along(datalist_names), function(i) {
      removeClass(ns_dl_box(datalist_values[i]), "active_tree")
    })
    addClass(vals$ns_dl_box, "active_tree")
    output$data_dlmX_bytes<-renderUI({datatalist_attributes()})
    datalist_values<-vals$dl_datalist_values
    apendto<-paste0('#', ns_dl_out(paste0(datalist_values[i])))
    vals$apendto<-apendto
    insertUI(vals$apendto, ui = uiOutput('data_dlmX_bytes'))
  },ignoreInit = T)
  observeEvent(vals$dl_datalistnames, {
    vals$apendto<-NULL
  },ignoreInit = T)
  observe({
    req(isTRUE(vals$task_open))
    req(length(vals$saved_data)>0)
    if(!is.null(vals$saved_ensemble)){
      vals$saved_data[["Saved Ensembles"]]<-vals$saved_ensemble}
    vals$dl_datalistnames<-names(vals$saved_data)
    vals$dl_datalist_values<-gsub("[^[:alnum:]]", "", vals$dl_datalistnames)
    vals$data_dlmX<-1
  })
  observeEvent(input$remove_gerenciador, {
    vals$task_mananger<-NULL

  },ignoreInit = T)
  observeEvent(input$remove_attribute, {
    req(length(vals$data_dlmX)>0)
    req(length(unlist(vals$task_mananger)) > 0)
    if(vals$data_dlmX=="Saved Ensembles"){
      dlm_remove_en()
    } else{

      task_length<-length(vals$task_mananger)
      req(length(vals$task_mananger)>0)
      if (task_length == 1) {
        if (vals$task_mananger == "Remove Datalist") {dlm_remove1()} else {dlm_remove_attr()}} else if (task_length == 2) {dlm_remove_attr2()} else if (task_length == 3) {dlm_remove_attr3()}

      # vals$apendto<-NULL

    }},ignoreInit = T)
  observe({
    req(isTRUE(vals$task_open))
    datalist_names<-vals$dl_datalistnames
    datalist_values<-vals$dl_datalist_values
    lapply(seq_along(datalist_names), function(i) {
      onclick(ns_dl_trash(paste0(datalist_values[i], '_', i)), {
        dlm_gerenciador$df<-T
        vals$data_dlmX<-datalist_names[i]
        vals$task_mananger<-"Remove Datalist"
        if(vals$data_dlmX=="Saved Ensembles"){
          vals$task_mananger<-"Remove All Ensembles"
        }
        removeUI("#data_dlmX_bytes")
      })

      onclick(ns_dl_name(datalist_values[i]), {
        vals$dlm_a$i<-i
        vals$data_dlmX<-datalist_names[i]
        vals$ns_dl_box<-ns_dl_box(datalist_values[i])
        vals$dl_name<-ns_dl_name(datalist_values[i])
        vals$dl_namepicker<-datalist_names[i]
      })
    })
  })
  observe({
    req(isTRUE(vals$task_open))
    req(vals$data_dlmX)
    attrx<-get_attributes_dl()
    datalist_name<-gsub("[^[:alnum:]]", "", vals$data_dlmX)
    lapply(names(attrx), function(i) {
      shinyjs::onclick(ns_attr_name(paste0(datalist_name, i)), {
        shinyjs::toggle(ns_attr_out(paste0(datalist_name, i)))
      })
      names_list<-as.list(names(attrx[[i]]))
      names_list<-gsub("[^[:alnum:]]", "", names_list)
      lapply(names_list, function(x) {
        shinyjs::onclick(ns_model_name(paste0(datalist_name, i, x)), {
          shinyjs::toggle(ns_model_out(paste0(datalist_name, i, x)))
        })
      })
    })
  })
  observe({
    req(isTRUE(vals$task_open))
    req(vals$data_dlmX)
    attrx<-get_attributes_dl()
    datalist_name<-gsub("[^[:alnum:]]", "", vals$data_dlmX)
    lapply(names(attrx), function(i) {
      trash<-ns_attr_trash(paste0(datalist_name, i))
      shinyjs::onclick(trash, {
        vals$task_mananger<-i
        dlm_gerenciador$df<-T
      })
      down_atr<-ns_attr_down(paste0(datalist_name, i))
      shinyjs::onclick(down_atr, {
        if(i=="numeric"){
          vals$down_data.frame<-vals$saved_data[[vals$data_dlmX]]
        } else{
          vals$down_data.frame<-attr(vals$saved_data[[vals$data_dlmX]], i)
        }
        vals$hand_down<-"Download dataframe"

        module_ui_downcenter("downcenter")
        mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals,name=paste0(vals$data_dlmX,"-",i))
      })

      names_list0<-names_list<-as.list(names(attrx[[i]]))
      names_list<-gsub("[^[:alnum:]]", "", names_list)
      lapply(1:length(names_list), function(j) {
        x<-names_list[j]
        trash2<-ns_model_trash(paste0(datalist_name, i, x))
        shinyjs::onclick(trash2, {
          dlm_gerenciador$df<-T
          vals$task_mananger_curmodel<-x
          vals$task_mananger<-c(i, names_list0[j])
        })

        trash3<-ns_feature_trash(paste0(datalist_name, i, x))
        shinyjs::onclick(trash3, {
          dlm_gerenciador$df<-T
          vals$task_mananger<-c(i, names_list0[j], "feature_rands")
        })

        down_feat<-ns_feature_down(paste0(datalist_name, i, x))
        shinyjs::onclick(down_feat, {
          task_mananger<-c(i, names_list0[j], "feature_rands")
          vals$down_data.frame<-
            attr(vals$saved_data[[vals$data_dlmX]], task_mananger[[1]])[[task_mananger[[2]]]]$feature_rands


          vals$hand_down<-"Download dataframe"
          module_ui_downcenter("downcenter")
          mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals,name=paste0(vals$data_dlmX,"-",i))

          dlm_gerenciador$df<-T
          vals$task_mananger<-c(i, names_list0[j], "feature_rands")
        })



      })
    })
  })
  observe({
    req(isTRUE(vals$task_open))
    req(vals$data_dlmX)
    attrx<-get_attributes_dl()
    attrx$transf<-NULL
    datalist_name<-gsub("[^[:alnum:]]", "", vals$data_dlmX)
    lapply(names(attrx), function(i) {
      trash<-input[[ns_attr_trash(paste0(datalist_name, i))]]
      observeEvent(trash, {if (trash) {modal_mananger()}})
      names_list0<-names_list<-as.list(names(attrx[[i]]))
      names_list<-gsub("[^[:alnum:]]", "", names_list)
      lapply(1:length(names_list), function(j) {
        x<-names_list[j]
        trash2<-input[[ns_model_trash(paste0(datalist_name, i, x))]]
        observeEvent(trash2, {if (trash2) {modal_mananger()}})
        trash3 <-input[[ns_feature_trash(paste0(datalist_name, i, x))]]
        observeEvent(trash3, {if (trash3) {modal_mananger()
        }
        })
      })
    })
  })
  output$tools_upload<-renderUI({
    choiceValues=choiceValues_preprocess()
    choiceNames=choiceNames_preprocess()


    div(

      if(length(vals$saved_data)>0) {
        uiOutput("preprocess_on")
      } else{
        uiOutput("preprocess_off")
      })


  })
  observe({
    vals$show_progress_title<-input$last_btn%in%c(paste0('tools_drop',1:9),"sidepanel")
  })
  observeEvent(vals$show_progress_title,{

    if(!isTRUE(vals$show_progress_title)){
      shinyjs::hide('exit_cogs')
    } else{
      shinyjs::show('exit_cogs')
    }
  })
  output$preprocess_exit<-renderUI({
    req(isTRUE(vals$show_progress_title))
    # req(!is.na(input$radio_cogs))
    #req(isTRUE(vals$last_cog))
    div(style="margin-left: 20px",
        span(actionButton("exit_preprocess",div(
          style="align: center",
          div(icon(verify_fa = FALSE,name=NULL,class="fas fa-window-close", style="background: white; padding: 0px; margin: 0px;line-height: 15px;")),
          div(span("close",style="font-size: 10px; "), style="margin-top: 2px")
        )),span("Pre-processing tools", style="color: white; font-size: 18px;font-weight: bold;"))
    )
  })
  observeEvent(ignoreInit = T,input$exit_preprocess,{
    runjs("Shiny.setInputValue('last_btn', 'fade');")
  })
  output$download_savepoint<-renderUI({
    req(length(vals$saved_data)>0)
    div(class="cogs_in",
        div(style="padding: 5px;height: 100px",
            div(strong(icon(verify_fa = FALSE,name=NULL,class="fas fa-thumbtack"),'Create a savepoint:',style="color: #05668D")),
            div(
              splitLayout(
                cellArgs = list(style='white-space: normal;'),
                column(12,uiOutput("downbook"))

              ))


        ))
  })
  output$tool7<-renderUI({uiOutput("palette_creation")})
  output$custom_palette<-renderUI({
    pic<-which(!vals$colors_img$val%in%c("turbo",
                                         "matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"
    ))
    req(length(pic)>0)
    div(
      inline(pickerInput(inputId = 'available_palette',
                         label = NULL,
                         choices = vals$colors_img$val[pic],
                         choicesOpt = list(content = vals$colors_img$img[pic]),  width="100px")),
      tipify( actionButton("delete_palette",icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt")),"Click to delete the selected palette")

    )})
  output$color_picker<-renderUI({
    if(is.null(vals$cur_col_cc)){vals$cur_col_cc<-"#3498DB"}
    tipify(
      colourpicker::colourInput(
        "col", NULL, vals$cur_col_cc,allowTransparent=T,
        closeOnClick=F,
      ),"Click to change color"
    )
  })
  output$palette_creation<-renderUI({
    div(style="padding-left: 50px",

        inline(
          div(class="tools_content",
              div(
                div(class="cogs_in",
                    inline(div(
                      style="padding: 5px; height: 75px;width: 130px; align: center",
                      icon(verify_fa = FALSE,name=NULL,class="fas fa-long-arrow-alt-right"),
                      "1. Pick a color:", class='tool_color',
                      uiOutput('color_picker'))),
                    inline(div(
                      style="padding: 5px; height: 75px;width: 110px; align: center",
                      div("2. Add:", class='tool_color'),
                      tipify(actionButton("add_color",icon(verify_fa = FALSE,name=NULL,class="fas fa-level-down")),"Click to include the color")))
                ),
                div(style="margin-left: 70px",
                    span(icon(verify_fa = FALSE,name=NULL,class="fas fa-level-down fa-rotate-180", style="width: 50px"),style="vertical-align: text-bottom;"),
                    span(icon(verify_fa = FALSE,name=NULL,class="fas fa-level-down fa-rotate-90" ),style="vertical-align: text-top;margin-left: 20px")
                )
              ),
              div(class="cogs_in",
                  style="padding: 5px; height: 75px;width: 300px; align: center",
                  uiOutput('newpalette_img')
              )
              ,
              div(style=" background:#FBEEE6;margin-top: 1px, width:220px; padding: 5px ",
                  p(strong("Created palettes:")),
                  uiOutput("custom_palette")
              )


              #verbatimTextOutput("newpalette_out"),


          )
        )
    )
  })
  output$add_palette_button<-renderUI({
    req(length(vals$newpalette_colors)>0)
    tipify(bsButton("add_palette",span(icon(verify_fa = FALSE,name=NULL,class="fas fa-palette"),icon(verify_fa = FALSE,name=NULL,class="fas fa-arrow-down"))),"Click to create a palette with the colors above. The new palette will be available in all color inputs")
  })
  observeEvent(ignoreInit = T,input$col,{
    vals$cur_col_cc<-input$col
  })
  output$newpalette_img<-renderUI({
    req(length(vals$newpalette_colors)>0)
    div(
      inline(
        div(style=" background: #D1F2EB ; padding: 5px;",
            div(class="tool_color","New palette"),
            div(inline(div(style="width: 140px",
                           renderPlot({
                             par(mar=c(0,0,0,0),mai=c(0,0,0,0),mgp=c(0,0,0),xpd=T,ann=F, bg=NA, fg=NA)
                             image(t(matrix(seq(0,1,length.out=100), nrow=1)), axes=F,col=  colorRampPalette(  vals$newpalette_colors,alpha=T)(100))
                           }, height =30)))
            )

        )
      ),
      inline(
        div(style="width: 135px",
            inline(
              div(
                div(class="tool_color","3. Create palette"),
                inline(uiOutput('add_palette_button')),
                tipify(actionButton("restart_palette",span(icon(verify_fa = FALSE,name=NULL,class="fas fa-undo"))),"Click to restart")

              )
            )
        )
      )
    )

  })
  output$newpalette_out<-renderPrint({
    vals$newpalette_colors
  })
  maxcol<-reactive({
    max<-length(vals$newpalette_colors)
    if(max<10){max=10}
    max
  })
  observeEvent(ignoreInit = T,input$add_color,{
    req(maxcol()<=48)
    vals$newpalette_colors<-c( vals$newpalette_colors,input$col)
  })
  observeEvent(ignoreInit = T,input$add_palette,{
    validate(need(!is.null(vals$newpalette_colors),"Please use the arrow button to include the selected color in the new palette"))
    palette_name<-paste("palette",length(vals$newcolhabs))
    outfile<-tempfile(fileext = ".png")
    png(outfile, height =70)
    par(mar=c(0,0,0,0),mai=c(0,0,0,0),mgp=c(0,0,0),xpd=T,ann=F, bg=NA, fg=NA)
    image(t(matrix(seq(0,1,length.out=100), nrow=1)), axes=F,col=  colorRampPalette(vals$newpalette_colors,alpha=T)(100))
    dev.off()
    palette1<-base64enc::dataURI(file =outfile,mime = "image/png")
    vals$colors_img<-rbind(vals$colors_img,data.frame(val=palette_name,img= sprintf(paste0(img(src = palette1, height = '20',width = '70',style="margin-top:-40px;margin-bottom:-40px;margin-left:-20px;margin-right:-20px;") ))))
    vals$newcolhabs[[palette_name]]<-colorRampPalette(vals$newpalette_colors,alpha=T)
    vals$created_pal<-T
    updatePickerInput(session,'available_palette',selected = palette_name)
    vals$newpalette_colors<-NULL
  })
  observeEvent(ignoreInit = T,input$restart_palette,{
    vals$newpalette_colors<-NULL
  })
  observeEvent(ignoreInit = T,input$delete_palette,{
    pic<-which( names(vals$newcolhabs)==input$available_palette)
    vals$newcolhabs[[pic]]<-NULL
    pic<-which(vals$colors_img$val==input$available_palette)
    vals$colors_img<-vals$colors_img[-pic,]

  })
  observeEvent(vals$seltree,{
    req(any(vals$seltree%in%input$selecobs))
    updateCheckboxGroupInput(session,'selecobs',selected=vals$seltree)
  })
  output$individual_selec<-renderUI({
    req(input$show_indsel %% 2)
    div(
      div(class="cogs_in",
          div(class="cogs_in2",
              checkboxInput("check_obs",strong('Select/Unselect all'),T)
          ),
          uiOutput("filterby_indobs")
      )
    )

  })
  output$treefactor<-renderUI({
    req(input$showtree%%2)
    div(style="overflow-x: scroll;height:250px;overflow-y: scroll",
        div(class="cogs_in",
            shinyTree("tree", checkbox = TRUE,themeIcons=F,themeDots=T)
        )
    )
  })
  observeEvent(list(input$data_upload,input$reset_change, input$savechanges_cancel,input$showtree),{
    treefalse$df<-F
    vals$tree<-NULL
    vals$seltree<-NULL
    output$tree<-renderTree({



      data<-vals$saved_data[[input$data_upload]]

      #data<-vals$saved_data[['abio_mean']]
      req(input$data_upload)
      factors<-attr(data,"factors")
      nas<- which(unlist(lapply(factors,function(x) anyNA(x)) ))
      if(length(nas)>0){
        factors<-factors[-nas]
      }
      vals$factor_tree<-factors




      req(length(factors)>0)
      lis<-get_faclevels(factors)

      pic<-which(unlist(lapply(lis, function(x) length(x[[1]])))<100)
      validate(need(length(pic)>0,"Only available for factors with less than 100 levels."))

      try({
        lis<-lis[names(pic)]
        df<-do.call(rbind,lapply(lis,function(x) {
          data.frame(parent=as.factor(rep(x[[2]],length(x[[1]]))),
                     child1=as.factor(x[[1]]))
        }))
        vals$tree<-gettree(df)
        treefalse$df<-T
        vals$tree

      })
    })


  })
  observeEvent(ignoreInit = T,input$tree,{
    req(!is.null(vals$factor_tree))

    req(isTRUE(treefalse$df))
    req(length(get_selected(input$tree))>0)

    args<-list(res=get_selected(input$tree),factors=vals$factor_tree)

    vals$seltree<-try({get_selfactors(res=get_selected(input$tree),factors=vals$factor_tree)})
    #beep(10)
  })
  output$match_datalist<-renderUI({
    req(isTRUE(input$obs_match))

    pickerInput("obs_match_datalist",NULL,names(vals$saved_data), width="180px")

  })
  observeEvent(ignoreInit = T,input$data_upload,{
    output$tool3<-renderUI({

      div(style=" height: 180px; margin-top: 10px",
          tags$script(HTML(js_tip(mytips,"transf"))),

          id="tool3",class="tools_content",
          div(
            div(style="vertical-align: middle",strong("Transformation"),tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "hover the options from to get method descriptions"),
                inline(
                  pickerInput(inputId = "transf",label = NULL,
                              choices =   do.call(c,lapply(transf_df, function (x) x$value)),inline=F, width="120px", selected="None")
                ))
          ),
          div(style="margin-top: 15px",
              div(strong("Scaling and Centering:")),
              div(class="cogs_in0",
                  div(style="padding: 10px",
                      div(
                        span(
                          inline(
                            div(style="width: 110px",
                                inline(
                                  checkboxInput("scale", 'Scale', F, width="65px")
                                ),
                                tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")," If scale is TRUE then scaling is done by dividing the (centered) columns of x by their standard deviations if center is TRUE, and the root mean square otherwise. If scale is FALSE, no scaling is done.")
                            )
                          )
                        ),
                        inline(uiOutput("scale_center"))
                      )
                  ))
          ),
          div(style="margin-top: 15px",
              div(
                strong("Random rarefaction:"),tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Generates one randomly rarefied community given sample size. If the sample size is equal to or larger than the observed number of individuals, the non-rarefied community will be returned"),
              ),
              div(class="cogs_in0",
                  div(style="padding: 10px",
                      uiOutput("rare_out")
                  ))
          )

      )
    })
  })
  output$tool1<-renderUI({
    try({

      validate(need(length(vals$saved_data)>0,"No data found"))

      div(id="tool1",class="tools_content",

          div(checkboxInput("na.omit", span("NA.omit",pophelp(NULL,"check to remove all observations that contain any empty cases (NAs)")), value = F,)),
          div(
            div(
              inline(checkboxInput("obs_match", span("Match with", pophelp(NULL,"Restrict current Datalist to observations (IDs) from another Datalist")), value = F, width="120px")),
              inline(
                uiOutput("match_datalist")
              )
            )
          ),
          splitLayout(
            uiOutput("filterobs_byfactor"),
            uiOutput("filterobs_byindividual"),


          )
      )

    })
  })
  output$filterobs_byfactor<-renderUI({
    div(
      div(
        div(style="border-bottom: 1px solid gray; margin-top: 10px",
            actionLink("showtree","Filter by factors"),tipify( icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Click in the nodes to expand and select the factor levels. Only available for factors with less than 100 levels")),
        uiOutput("treefactor"))
    )
  })
  output$filterobs_byindividual<-renderUI({
    validate(need(nrow(vals$saved_data[[vals$cur_data]])<=1000,"This functionality is only available for data with less than 1000 observations"))
    req(vals$cur_data)
    div(
      div(
        div(actionLink("show_indsel","Individual selection:"),tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"Click to expand observations"),style="border-bottom: 1px solid gray; margin-top: 10px"),
        uiOutput("individual_selec")


      )
    )
  })
  output$tool2<-renderUI({

    div(id="tool2",class="tools_content",

        div(splitLayout(cellWidths = c("40%","60%"),
                        uiOutput("remove_sp"),
                        uiOutput("filterby_indvars")
        )),
        div(uiOutput("filter_cor"),
            uiOutput("filter_nzv")
        ))
  })
  output$filter_nzv<-renderUI({
    div(style="margin-top:10px; ",
        div(style="margin-top: 10px;",
            span(inline(checkboxInput("nzv_check",span("nearZeroVar remotion", actionLink("nzv_help",icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"))),F)),

            )
        ),
        uiOutput("nzv_options")


    )
  })
  output$nzv_options<-renderUI({
    req(isTRUE(input$nzv_check))
    div(class="cogs_in",
        div(class="map_control_style; ",
            splitLayout(cellWidths = c("40%","60%"),
                        div(
                          div(
                            span("+ freqCut",tiphelp("the cutoff for the ratio of the most common value to the second most common value","right"),
                                 inline(numericInput("freqCut",NULL,value =95/5,min = 0.1,max = 1,step = .1, width='100px')))),
                          div(
                            span("+ uniqueCut",tiphelp("the cutoff for the ratio of the most common value to the second most common value","right"),
                                 inline(numericInput("uniqueCut",NULL,value =10,min = 0.1,max = 1,step = .1, width='100px')
                                 )))
                        ),
                        uiOutput('nzv_message')

            ),
            uiOutput("nearZeroVar_print")
        )
    )
  })
  get_nzv<-reactive({
    req(input$data_upload)
    res<-NULL
    if(length(input$nzv_check)>0){
      if(isTRUE(input$nzv_check)){
        req(input$freqCut)
        req(input$uniqueCut)
        data<-vals$saved_data[[input$data_upload]]
        res<-nearZeroVar(data,freqCut=input$freqCut,uniqueCut=input$uniqueCut,saveMetrics = T)
      } else{
        res<-NULL
      }
    }
    res

  })
  getnearzero<-reactive({
    req(input$data_upload)
    res<-NULL
    if(length(input$nzv_check)>0){
      if(is.null(get_nzv())){
        res<-NULL
      } else{
        data<-vals$saved_data[[input$data_upload]]
        nearzero<-get_nzv()
        res<-colnames(data)[which(nearzero$nzv)]
        if(!length(res)>0){
          res<-NULL
        }
        res
      }
      res
    }

  })
  output$nzv_message<-renderUI({
    req(isTRUE(input$nzv_check))
    n<-length(getnearzero())
    req(n>0)
    column(12,span(strong("Warning:")),em(n,"variables with a true NZV (Non-Zero Variance) have been temporarily removed. Please save the changes to make this removal permanent.", style=" white-space: normal;"))
  })
  output$nearZeroVar_print<-renderUI({
    req(isTRUE(input$nzv_check))
    div(style="max-height: 150px;overflow-y: scroll",
        strong("NZV results:"),
        renderTable({
          df<-get_nzv()
          df[order(df$nzv, decreasing = T),]
        }, rownames=T)

    )
  })
  observeEvent(ignoreInit = T,input$nzv_help, {

    pkgs<-"caret"
    citations<-do.call('c',lapply(pkgs, citation))
    citations<- lapply(citations,function(x){format(x, style = "html")})



    showModal(
      modalDialog(
        title = "Identification of Near Zero Variance Predictors",
        easyClose = TRUE,
        size = "l",
        fluidPage(
          p("The 'nearZeroVar' function from the 'caret' package provides functionality for identifying and removing near zero variance predictors. These predictors have either zero variance (i.e., only one unique value) or very few unique values relative to the number of samples, with a large frequency ratio between the most common and second most common values."),
          p(strong(em("Details provided by the function's help documentation:"))),
          p("For example, a near zero variance predictor could be a variable that has only two distinct values out of 1000 samples, with 999 of them being the same value."),
          p("To be flagged as a near zero variance predictor, two conditions must be met. First, the frequency ratio of the most prevalent value over the second most frequent value (referred to as the 'frequency ratio') must exceed a specified threshold (freqCut). Second, the 'percent of unique values' (the number of unique values divided by the total number of samples, multiplied by 100) must be below another specified threshold (uniqueCut)."),
          p("In the example mentioned above, the frequency ratio would be 999 and the percent of unique values would be 0.0001."),
          p("For certain models, such as naive Bayes, it may be necessary to check the conditional distribution of predictors to ensure that each class has at least one data point."),
          p("The 'nzv' function represents the original version of this functionality."),
          h4("Reference:"),
          div(style="padding-top: 20px",
              HTML(paste0(citations))
          )
        )
      )
    )
  })
  observeEvent(ignoreInit = T,input$cor_use,{
    vals$cor_use<-input$cor_use
  })
  output$filter_cor<-renderUI({


    div(style="margin-top:10px; ",
        div(style="margin-top: 10px;",checkboxInput("corr_check",span("Correlation-based remotion",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Removes variables using the function findCorrelation from caret package. The absolute values of pair-wise correlations are considered. If two variables have a high correlation, the function looks at the mean absolute correlation of each variable and removes the variable with the largest mean absolute correlation. Argument exact is fixed TRUE, which means that the function re-evaluates the average correlations at each step.")),F)
        ),
        uiOutput("corr_options")


    )
  })
  output$corr_options<-renderUI({
    req(isTRUE(input$corr_check))
    cor_method = c("pearson", "kendall", "spearman")
    cor_use=c( "complete.obs","everything", "all.obs", "na.or.complete", "pairwise.complete.obs")
    div(

      class="cogs_in",style=" height: 120px",
      div(class="map_control_style",

          div(pickerInput("cor_method", span("+ Corr method",tiphelp("correlation coefficient to be computed")),choices=cor_method, selected=vals$cor_method)),
          div(
            span("+ Cutoff",tiphelp("The pair-wise absolute correlation cutoff","right"),
                 inline(numericInput("cor_cutoff",NULL,value =1,min = 0.1,max = 1,step = .1, width='100px')
                 ))),
          div(pickerInput("cor_use", span("+ Use",tiphelp("method for computing covariances in the presence of missing values","right")),
                          choices=cor_use, selected=vals$cor_use)),
          div(style="margin-top: 20px",
              icon(verify_fa = FALSE,name=NULL,class="fas fa-lightbulb"),"Explore the correlation plot in the Descriptive tools menu"
          )

      )
    )
  })
  observeEvent(ignoreInit = T,input$cor_method,{
    vals$cor_method<-input$cor_method
  })

  get_corrdata<-reactive({
    cordata<-NULL
    if(length(input$cor_cutoff)>0){
      req(is.numeric(input$cor_cutoff))
      if(input$cor_cutoff<1){
        data<-vals$saved_data[[input$data_upload]]
        req(nrow(data)>1)
        req(ncol(data)>1)
        cordata<-try({
          cordata_filter(data=data,cor_method=input$cor_method,cor_cutoff=input$cor_cutoff,cor_use=input$cor_use)
        })
        if("try-error" %in% class(cordata)){
          cordata<-NULL
        } else{
          if(length(colnames(cordata))<0){
            cordata<-NULL
          }
          cordata<-colnames(cordata)
        }

      }
    }

    cordata
  })
  output$tool4<-renderUI({

    div(

      id="tool4",
      class="tools_content",
      div(id="tool4_2",
          span("Target:", inline(pickerInput("na_targ",NULL, c("Numeric-Attribute","Factor-Attribute"), width="150px"))),
          uiOutput("tool4_2")
      )
    )
  })
  output$tool4_2<-renderUI({
    data<-vals$saved_data[[vals$cur_data]]
    factors<-attr(data,"factors")
    if(input$na_targ=="Numeric-Attribute"){
      validate(need(anyNA(data),"No missing values in the Numeric-Attribute"))
    }
    if(input$na_targ=="Factor-Attribute"){
      validate(need(anyNA(factors),"No missing values in the Factor-Attribute"))
    }
    div(
      span(
        span(id="na_method2",
             inline(uiOutput("na_methods"))
        ),
        tags$style("#na_method2 .selectize-input {margin-bottom: -12px;}"),
        inline(uiOutput("na_knn")),

        tipify(actionButton("go_na",img(src=na_icon,height='15',width='15'),style="margin-top:-5px "),"Impute missing values","top"),
        tipify(actionButton("undo_na",icon(verify_fa = FALSE,name=NULL,class="fas fa-undo"),style="margin-top:-5px "),"Undo imputation","top"),
        uiOutput("na_warning"),
        uiOutput("imputation_help")
      )
    )
  })
  output$na_knn<-renderUI({
    req(input$na_method=='knn')
    numericInput("na_knn", "k",value=5, width="75px")

  })
  output$tool5<-renderUI({

    div(

      div(
        id="tool5",
        class="tools_content",
        inline(
          div(
            id="tool5_1",
            div(style="margin-top: 10px",
                radioButtons(
                  "split_options",NULL,
                  choices =c("Random sampling","Balanced sampling"),width = "200px", selected=vals$split_options
                )),
            inline(uiOutput("split_factor")),
            inline(numericInput("splitperc_data","Size (%)",value=20,min=0,max=99,step=1,width="100px")),
            inline(numericInput("seed_test","Seed",value=NA,width="80px"))
          )
        ),
        inline(
          div(class="save_changes",tipify(actionButton("addpart",label=icon(verify_fa = FALSE,name=NULL,class="fas fa-plus")),"Add partition  as a factor in Factor-Attribute. You can later choose the factor which you want to use to constrain training and testing data. "))

        ),
        column(12,
               splitLayout(
                 column(12,style="overflow-x: scroll;height:250px;overflow-y: scroll;",
                        strong("Partition"),
                        verbatimTextOutput("partition_preview")
                 ))
        )

      )
    )
  })
  output$tool6<-renderUI({
    req(vals$cur_data)

    factors<-attr(vals$saved_data[[vals$cur_data]],"factors")

    div(
      id="tool6",
      class="tools_content",
      div(id="tool6_1",


          inline(pickerInput("tag_fun","Edit factor levels:",choices =c('label',"order"), selected=vals$cur_tag_fun, width="125px" )),

          inline(
            div(class="tool6_1",
                conditionalPanel("input.tag_fun!='convert'",{
                  inline(pickerInput("tag_edit","Factor",rev(colnames(factors)), selected=cur_tag$df, width="125px"))
                })



            )
          ),

          uiOutput("edit_fac_labels"),
          uiOutput("edit_fac_ord")
      )

    )
  })
  observeEvent(ignoreInit = T,input$data_upload,{
    output$tool8<-renderUI({
      div(id="tool8",
          div(strong("Aggregate:"),
              popify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),
                     "Aggregate","The process involves two stages. First, collate individual cases of the Numeric-Attribute together with a grouping variable (unselected factors). Second, perform which calculation you want on each group of cases (selected factors)", options=list(container="body"))),
          splitLayout(
            div(
              div(class="cogs_in",
                  div(style=" margin-top: 10px;overflow-x: scroll;height:180px;overflow-y: scroll",
                      div(checkboxInput("check_allfac_agg",strong('Select/Unselect all'),T), style="padding-bottom:10px"),
                      div(style="padding-top: 10px;",
                          checkboxGroupInput('fac_descs', NULL, choices=colnames(
                            attr(vals$saved_data[[input$data_upload]],"factors")), selected = colnames(
                              attr(vals$saved_data[[input$data_upload]],"factors"))[1:(length(colnames(
                                attr(vals$saved_data[[input$data_upload]],"factors")))-1)])
                      )
                  )
              )
            ),
            div(
              div(strong("Function:")),
              div(pickerInput("spread_measures",NULL,choices=c("sum","mean","median","var","sd","min","max"),  selected="mean")),
              div(
                uiOutput('aggregation_button'),
                uiOutput("save_agg_btn"),
                bsTooltip('go_agg',"Click to aggregate")
              )
            )
          ),


          div(style="overflow-x: scroll;height:250px;overflow-y: scroll;",
              tags$style('#desc_dataout td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
              tags$style('#desc_dataout th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),

              inline(
                DT::dataTableOutput("desc_dataout")
              ))
      )
    })
  })
  output$aggregation_button<-renderUI({
    class=if(is.data.frame(vals$agg)){"div"} else{"save_changes"}
    div(class=class,
        actionButton("go_agg", img(src=agg_icon2,height='14',width='14')),em(icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"Click to aggregate")
    )
  })
  observeEvent(ignoreInit = T,input$check_allfac_agg,{


    if(isTRUE(input$check_allfac_agg)){
      updateCheckboxGroupInput(session,
                               "fac_descs",NULL,
                               choices = colnames(attr(vals$saved_data[[input$data_upload]],"factors")),
                               selected = colnames(attr(vals$saved_data[[input$data_upload]],"factors"))
      )
    } else{
      updateCheckboxGroupInput(session,
                               "fac_descs",NULL,
                               choices = colnames(attr(vals$saved_data[[input$data_upload]],"factors"))

      )
    }

  })
  observeEvent(ignoreInit = T,input$tabs,{
    runjs("Shiny.setInputValue('last_btn', 'main_panel');")
  })
  observeEvent(ignoreInit = T,input$tools_saveagg,{
    vals$hand_save<-"Create data list from aggregation results"
    vals$hand_save2<-if(!is.null(attr(vals$agg,"coords"))){p("The select Datalist contains a Coords-Attribute. Mean coordinates will be retrieved from the choosen factor group.")} else {NULL}
    vals$hand_save3<-NULL
    showModal(
      hand_save_modal()
    )
  })
  output$save_agg_btn<-renderUI({
    if(is.data.frame(vals$agg)){
      div(class='save_changes',actionButton("tools_saveagg", icon(verify_fa = FALSE,name=NULL,class="fas fa-file-signature")),em(icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"Click to Create a Datalist"),

          bsTooltip('tools_saveagg','Click to create a Datalist with the aggregation results'))

    }
  })
  output$desc_dataout = {DT::renderDataTable({

    data<-vals$agg
    aggreg_reac$df<-data
    factors<-attr(data,"factors")
    cbind(factors,data)
  },options = list(pageLength = 15, info = FALSE,lengthMenu = list(c(15, -1), c( "15","All")), autowidth=F,scrollX = TRUE, scrollY = "200px"), rownames = TRUE,class ='cell-border compact stripe')}
  observeEvent(ignoreInit = T,input$go_agg,{try({
    req(is.data.frame(data_cogs$df))
    data<-data_cogs$df
    factors<-attr(data,"factors")[rownames(data),,drop=F]
    coords<-attr(data,"coords")[rownames(data),,drop=F]
    df<- data.frame(aggregate(data,data.frame(factors[,input$fac_descs, drop=F]),get(input$spread_measures), na.rm=T))
    if(!is.null(coords)){
      coords<- data.frame(aggregate(coords,factors[rownames(coords),input$fac_descs, drop=F],mean , na.rm=T))
      coords<-coords[,which(unlist(lapply(coords,is.numeric))), drop=F]

    }
    dfnum<-df[,which(unlist(lapply(df,is.numeric))), drop=F]
    dffac<-df[,which(unlist(lapply(df,is.factor))), drop=F]
    df<-dfnum
    labels<-make.unique(apply(dffac[,input$fac_descs, drop=F], 1 , paste,collapse = "_"))


    rownames(df)<-labels
    df<-data_migrate(data,df,"Aggregated_results")
    rownames(dffac)<-labels
    if(!is.null(coords)){
      rownames(coords)<-labels
    }

    attr(df,"factors")<-dffac

    attr(df,"coords")<-coords

    #if(anyNA(df)){df<-"Requires groups with at least two counts "}
    vals$agg<-df
    vals$agg

  })})
  output$tool9<-renderUI({
    fluidRow(id="tool9",class="tools_content",
             div(
               uiOutput("download_savepoint")),
             div(
               div(class="cogs_in",
                   div(style="padding: 5px;height: 100px",
                       div(
                         tipify(strong(icon(verify_fa = FALSE,name=NULL,class="fas fa-external-link-alt"),"Load a savepoint:",style="color: #05668D"),"Restore the dashboard with a previously saved savepoint (.rds file)")),
                       div(id="tool_savepoint", fileInput("load_savepoint", label=NULL,accept =".rds",multiple =T))))),
             uiOutput("validade_savepoint"))
  })
  output$filterby_indobs<-renderUI({
    req(vals$cur_data)

    data<-vals$saved_data[[vals$cur_data]]
    req(is.data.frame(data_cogs$df))
    #data<-getdata_upload()
    div(style="overflow-x: scroll;height:250px;overflow-y: scroll",
        validate(need(nrow(data)<1000,"Only available for data with less than 5000 observations")),
        checkboxGroupInput("selecobs",NULL,choices = rownames(data),selected = rownames(data_cogs$df))
    )

  })
  observeEvent(ignoreInit = T,input$split_options,{
    vals$split_options<-input$split_options
  })
  observeEvent(ignoreInit = T,input$splitfactor,{
    vals$splitfactor<-input$splitfactor
  })
  output$split_factor<-renderUI({
    req(input$split_options=='Balanced sampling')
    inline(pickerInput("splitfactor","Factor",choices=colnames(attr(vals$saved_data[[vals$cur_data]],"factors")), width ='150px', selected=vals$splitfactor))

  })
  output$toorder_tag<-renderUI({
    div(
      rank_list(
        text = "Drag the levels in any desired order",
        labels = levels(attr(vals$saved_data[[vals$cur_data]],"factors")[,input$tag_edit]),
        input_id = "rank_list_2",
        class="custom-sortable"
      )

    )
  })
  change_inputs<-reactive({
    list(
      input$radio_cogs,
      input$data_upload,
      input$filter_data,
      input$cutlevel_obs,
      input$selecvar,
      input$selecobs,
      input$na.omit,
      input$transf,
      input$scale,
      input$center,
      input$rareabund,
      input$rarefreq,
      input$raresing,
      input$pct_rare,
      input$pct_prev,
      vals$seltree,
      input$tag_edit,
      input$rank_list_2,
      input$obs_match,
      input$obs_match_datalist,
      getnearzero(),
      get_corrdata()
    )
  })
  data.rares<-reactive({
    req(length(vals$saved_data)>0)
    req(vals$cur_data%in%names(vals$saved_data))
    args<-list(

      saved_data=vals$saved_data,
      cur_data=input$data_upload,
      filter_data=input$filter_data,
      cutlevel_obs=input$cutlevel_obs,
      filter_datalist=input$filter_datalist,
      selecvar=input$selecvar,
      selecobs=input$selecobs,
      seltree=vals$seltree,
      na.omit=input$na.omit,
      transf=input$transf,
      scale=input$scale,
      center=input$center,
      rareabund=input$rareabund,
      pct_rare=input$pct_rare,
      rarefreq=input$rarefreq,
      pct_prev=input$pct_prev,
      raresing=input$raresing,
      obs_match=input$obs_match,
      obs_match_datalist=input$obs_match_datalist,
      nzv=getnearzero(),
      cor_filter=get_corrdata()

    )


    data<-do.call(data.rares_fun,args)
    data
  })
  getdata_split<-reactive({
    data<-vals$saved_data[[vals$cur_data]]
    factors<-attr(data,"factors")
    fac<-factors[,input$splitfactor, drop=F]
    if(input$split_options=='Balanced sampling'){
      if (!is.na(input$seed_test)) {set.seed(input$seed_test)}
      res<- createDataPartition(unlist(fac),p=input$splitperc_data/100,list=F)
      res<-unlist(res)
    } else if(input$split_options=='Random sampling')
    {
      n_sample<-round(nrow(data)*input$splitperc_data/100)
      if (!is.na(input$seed_test)) {set.seed(input$seed_test)}
      res<-sample(1:nrow(data),n_sample)

    }

    res
  })
  get_partition<-reactive({
    data<-vals$saved_data[[vals$cur_data]]
    factors<-attr(data,"factors")
    factors$Partition<-NA
    res<-  factors[,"Partition", drop=F]
    res[rownames(data)[as.vector(getdata_split())] ,"Partition"]<-"test"
    res[rownames(data)[-as.vector(getdata_split())] ,"Partition"]<-"training"
    res$Partition<-as.factor( res$Partition)
    res
  })
  observeEvent(change_inputs(),{
    req(data_cog_bag$df==0)
    req(length(vals$saved_data)>0)
    req(vals$cur_data)
    data<- data.rares()
    factors<-attr(data,"factors")
    #if(!is.null(get_corrdata())){data<- data[get_corrdata()]    }
    data_cogs$df<-data
  })
  observe({
    req(length(vals$saved_data)>0)
    req(is.data.frame(data_cogs$df))
    req(nrow(data_cogs$df>0))
    req(data_cogs$df)
    req(vals$cur_data)
    req(!is.null(data_cogs$df))
    req(!is.null(attr(data_cogs$df,"factors")))
    data<-data.frame(as.matrix(data_cogs$df))
    factors<-data.frame(as.matrix(attr(data_cogs$df,"factors")))
    req(is.data.frame(vals$saved_data[[vals$cur_data]]))
    data_o<-data.frame(as.matrix(vals$saved_data[[vals$cur_data]]))
    req(is.data.frame(attr(vals$saved_data[[vals$cur_data]],"factors")))
    factors_o<-data.frame(as.matrix(attr(vals$saved_data[[vals$cur_data]],"factors")))
    bagdata<-identical(data_o,data)
    bagfac<-identical(factors,factors_o)
    vals$bagdata<-c(bagdata,bagfac)
  })
  output$filterby_indvars<-renderUI({
    #req(is.data.frame(data_cogs$df))
    data<-vals$saved_data[[vals$cur_data]]
    req(length(data)>0)
    validate(need(ncol(data)<1000,"Individual selection not available for data with more than 1000 columns"))
    div(style="margin-top: 10px;",
        div("Individual selection:"),
        div(class="cogs_in",style='overflow-x: scroll;overflow-y: scroll;height:300px;',
            div(
              div(class="cogs_in2",checkboxInput("check_fac",strong('Select/Unselect all'),T)),
              div(style=" margin-top: 10px;",
                  checkboxGroupInput("selecvar",NULL,choices = colnames(data),selected = colnames(data)))
            )
        )
    )
  })
  observeEvent(list(input$radio_cogs,input$last_btn),{
    req(input$last_btn)
    req(!is.na(input$radio_cogs))
    vals$impute<-F
    if(input$radio_cogs=='tools_drop1'){shinyjs::show('tog_tool1')} else{ hide('tog_tool1')}
    if(input$radio_cogs=='tools_drop2'){shinyjs::show('tog_tool2')} else{ hide('tog_tool2')}
    if(input$radio_cogs=='tools_drop3'){shinyjs::show('tog_tool3')} else{ hide('tog_tool3')}
    if(input$radio_cogs=='tools_drop4'){shinyjs::show('tog_tool4')} else{ hide('tog_tool4')}
    if(input$radio_cogs=='tools_drop5'){shinyjs::show('tog_tool5')} else{ hide('tog_tool5')}
    if(input$radio_cogs=='tools_drop6'){shinyjs::show('tog_tool6')} else{ hide('tog_tool6')}
    if(input$radio_cogs=='tools_drop7'){shinyjs::show('tog_tool7')} else{ hide('tog_tool7')}
    if(input$radio_cogs=='tools_drop8'){shinyjs::show('tog_tool8')} else{ hide('tog_tool8')}
    if(input$radio_cogs=='tools_drop9'){shinyjs::show('tog_tool9')} else{ hide('tog_tool9')}
  })
  observe({
    req(length( input$last_btn)>0)
    vals$cur_showhisto=F
    req(input$last_btn)
    #req(length(input$radio_cogs)>0)
    if(input$last_btn=='fade'){

      vals$lock_change=T

      shinyjs::hide('tog_tools')
      shinyjs::hide('change_head0')
      #runjs("Shiny.setInputValue('lock_change', FALSE);")
      #if(isFALSE(status_changes$df)){modal_unsaved_chages()} else{}

      if(length(vals$saved_data)>0){
        req(vals$cur_data%in%names(vals$saved_data))
        data_cogs$df<-vals$saved_data[[vals$cur_data]]}

      updateRadioGroupButtons(session,"radio_cogs",selected=NA)
      #runjs("Shiny.setInputValue('radio_cogs', NA);")

    }
    if(isTRUE(vals$last_cog)){


      vals$lock_change=F
      shinyjs::show('tog_tools')
      shinyjs::show('change_head0')
      shinyjs::show('data_change')


    }
  } )
  modal_unsaved_chages<-reactive({

    showModal(
      modalDialog(
        easyClose = F,
        div("The",vals$cur_data,"has unsaved changes. Do you want to save these changes?"),
        footer = div(

          bsButton("savechanges_return","Return"),
          bsButton("savechanges_cancel","Don't save"),
          bsButton("savechanges_save","Save")

        )
      )
    )

  })
  observeEvent(ignoreInit = T,input$savechanges_cancel,{
    data_cogs$df<-vals$saved_data[[vals$cur_data]]

    shinyjs::reset("change_head")
    removeModal()
  })
  observeEvent(ignoreInit = T,input$go_na,{
    try({

      req(input$na_targ=="Numeric-Attribute")
      data<-data_cogs$df
      transf<-attr(data,"transf")
      withProgress(message="Performing Imputation...",
                   min = 1,
                   max = 1,{
                     data<-nadata(data=data,
                                  na_method=input$na_method,
                                  data_old=vals$saved_data[[vals$cur_data]],
                                  data_name=vals$cur_data,
                                  k=input$na_knn)
                     beep(10)

                   })

      transf<-attr(data,"transf")

      transf["Data_imp",'current']<-input$na_method
      attr(data,"transf")<-transf
      vals$impute<-T


      data_cogs$df<-data
      vals$hand_save<-"Save changes"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL

      showModal(
        hand_save_modal()
      )

    })
  })
  observeEvent(ignoreInit = T,input$check_fac,{
    if(isTRUE(input$check_fac)){
      updateCheckboxGroupInput(session,
                               "selecvar",NULL,
                               choices = colnames(vals$saved_data[[vals$cur_data]]),
                               selected = colnames(vals$saved_data[[vals$cur_data]])
      )
    } else{
      updateCheckboxGroupInput(session,
                               "selecvar",NULL,
                               choices = colnames(vals$saved_data[[vals$cur_data]])

      )
    }

  })
  observeEvent(ignoreInit = T,input$check_obs,{
    if(isTRUE(input$check_obs)){
      updateCheckboxGroupInput(session,
                               "selecobs",NULL,
                               choices = rownames(vals$saved_data[[vals$cur_data]]),
                               selected = rownames(vals$saved_data[[vals$cur_data]])
      )
    } else{
      updateCheckboxGroupInput(session,
                               "selecobs",NULL,
                               choices = rownames(vals$saved_data[[vals$cur_data]])

      )
    }

  })
  observeEvent(ignoreInit = T,input$tag_edit_apply,{

    #input$tag_edit<-'season'
    # input$tag_edit_level<-"Spring"
    #input$tag_edit_newlevel<-'Primavera'
    factors<-attr(vals$saved_data[[vals$cur_data]],"factors")
    newfactor<-oldfactor<-factors[,input$tag_edit]
    newlevels<-oldlevels<-levels(oldfactor)
    piclevels<-which(oldlevels==input$tag_edit_level)
    newlevels[oldlevels]<-input$tag_edit_newlevel
    newfactor<-as.character(newfactor)
    newfactor[oldfactor==input$tag_edit_level]<-input$tag_edit_newlevel
    attr(vals$saved_data[[vals$cur_data]],"factors")[,input$tag_edit]<- as.factor(newfactor)

  })
  observeEvent(ignoreInit = T,input$undo_na,{
    data_cogs$df<-vals$saved_data[[vals$cur_data]]
  })
  observeEvent(ignoreInit = T,input$data_upload,{
    #runjs("Shiny.setInputValue('reset_change', true);")
    cur_data<-input$data_upload
    vals$cur_data<-cur_data

  })
  output$rare_out<-renderUI({
    data<-data_cogs$df
    div(
      inline(
        numericInput("rrarefy_sample","sample",50, width="100px")
      ),
      inline(tipify(actionButton("rrarefy_go", span(icon(verify_fa = FALSE,name=NULL,class="fas fa-arrow-circle-right"))),"Run"))
    )
  })
  observeEvent(ignoreInit = T,input$rrarefy_go,{
    data<-data_cogs$df
    if(!identical(all.equal(data, round(data)), TRUE)){
      showModal(
        modalDialog(
          div(icon(verify_fa = FALSE,name=NULL,class="fas fa-bug"),"Functionality is available only for integers (counts)")
        )
      )
    } else{
      newdata<-data.frame(rrarefy(data,input$rrarefy_sample))
      newdata<-data_migrate(data,newdata,"newrare")
      data_cogs$df<-newdata

    }

  })
  observeEvent(input$tabs, {
    shinyjs::runjs('$("body, html").scrollTop(0);')
  })
  savepart<-reactive({
    curdata<-vals$cur_data
    data<-getdata_upload()
    part<-get_partition()
    factors<-attr(data,"factors")
    vals$cur_dataY<-input$data_upload
    if(input$hand_save=="create"){
      vals$bagpart0<-vals$bagpart0+1
      factors[,input$split_newname]<-part
      attr(vals$saved_data[[input$data_upload]],"factors")<-factors
      #input$data_upload<-input$split_newname
      #vals$cur_partsom<-input$split_newname
    } else {
      factors[,input$split_over]<-part
      attr(vals$saved_data[[input$data_upload]],"factors")<-factors
      # input$data_upload<-input$split_over
      #vals$cur_partsom<-input$split_over
    }
    status_changes$df<-c(T,T)
    #shinyjs::reset("change_head")
    updateRadioGroupButtons(session,'radio_cogs',selected = last_btn$equal[1])
    status_changes$df<-c(T,T)


  })
  savechanges_comb<-reactive({
    temp<-data_cogs$df
    data<-data_cogs$df
    pic<-which(do.call(c,lapply(temp,function(x) sum(is.na(x))))==nrow(temp))
    if(length(pic)>0){
      temp<-temp[,-pic]
    }
    temp<-data_migrate(data,temp,"new")
    factors<-attr(data,"factors")
    if(!is.null(factors)){
      factors<-attr(data,"factors")
      attr(temp,"factors")<-factors
    }


    if(input$hand_save=="create"){
      vals$saved_data[[input$data_newname]]<-temp
      #  vals$cur_data<-input$data_newname
    } else{
      vals$saved_data[[input$data_over]]<-temp
      # vals$cur_data<-input$data_over

    }
    vals$new_facts<-NULL
    #runjs("Shiny.setInputValue('last_btn', false);")
    status_changes$df<-c(T,T)
    data_cogs$df<-temp
    shinyjs::reset("change_head")

    #runjs("Shiny.setInputValue('last_btn', 'fade');")
    updateRadioGroupButtons(session,'radio_cogs',selected = last_btn$equal[1])




    if(isTRUE(vals$impute)){vals$impute<-F}
  })
  output$imesc_panel<-renderUI({

    column(12,id="imesc_panes",class="needed",

           uiOutput("menutitle"),

           div(id="imesc_after_title",
               tabsetPanel(type ="hidden",
                 tabPanel(title=NULL,
                          tabItems(
                            tabItem(tabName = "menu_intro",
                                    uiOutput("menu_intro_out")),
                            tabItem(tabName = "menu_nb",
                                    uiOutput('menu_nb_out')),
                            tabItem(tabName = "menu_svm",
                                    uiOutput('menu_svm_out')),
                            tabItem(tabName = "menu_upload",
                                    uiOutput('menu_bank_out')),
                            tabItem(tabName = "menu_explore",
                                    uiOutput('menu_upload_out')),
                            tabItem(tabName = "menu_som",
                                    column(12,uiOutput('menu_som_out'))),

                            tabItem(tabName = "menu_som2",
                                    column(12,uiOutput('menu_som2_out'))),
                            tabItem(tabName = "menu_hc",
                                    column(12,uiOutput('menu_hc_out'))),
                            tabItem(tabName = "menu_rf",
                                    uiOutput('menu_rf_out')),
                            tabItem(tabName = "menu_knn",
                                    uiOutput('menu_knn_out')),
                            tabItem(tabName = "menu_sgboost",
                                    uiOutput('menu_sgboost_out')),
                            tabItem(tabName = "menu_compare",
                                    uiOutput('menu_compare_out')),
                            tabItem(tabName = "menu_ensemble",
                                    uiOutput('menu_ensemble_out')),
                            tabItem(tabName = "menu_maps2",
                                    uiOutput('map_header'),
                                    uiOutput('map_body')),
                            tabItem(tabName = "menu_sim",
                                    uiOutput('menu_sim_out')),
                            tabItem(tabName = "menu_kmeans",
                                    uiOutput('menu_kmeans_out')),

                            tabItem(  tabName = "menu_spd",
                                      column(12,uiOutput('menu_spd_out'))

                            ),
                            tabItem(tabName = "menu_div",
                                    column(12,
                                           uiOutput('menu_div_out'))
                            ),
                            tabItem(tabName = "menu_teste",
                                    column(12,
                                           uiOutput("menu_teste"))
                            )
                          )
                 )
               )
           )

    )
  })

  output$menu_bank_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<- databank_module$ui("databank")
    databank_module$server('databank', vals=vals)
    res

  })

  output$menu_hc_out<-renderUI({
    res<- hc_module$ui("hc")
    hc_module$server('hc', vals=vals)
    res
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
          column(12, style="background: white",
                 HTML("Access the complete documentation at <a href='https://danilocvieira.github.io/iMESc_help' target='_blank'>iMESc<sup>help!</sup></a> for detailed guides and support. It provides everything needed to effectively utilize iMESc.")
          ))

      )
    )
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
  output$menu_spd_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<-module_ui_spd("module_spd")
    mod_spd<-callModule(module_server_spd, "module_spd",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs,df_symbol=df_symbol)
    removeModal()
    res
  })
  output$menu_div_out<-renderUI({
    # #validate(need(length(vals$saved_data)>0,"No Datalist found"))


    res<-module_ui_div("module_div")
    mod_div<-callModule(module_server_div, "module_div",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })
  output$scale_center<-renderUI({
    req(isTRUE(input$scale))
    span(
      inline(
        checkboxInput("center", 'Center', F, width="65px")
      ),
      tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"),"If center is TRUE then centering is done by subtracting the column means (omitting NAs) of x from their corresponding columns, and if center is FALSE, no centering is done."),

    )
  })
  output$edit_fac_ord<-renderUI({
    req(input$tag_fun=='order')
    div(class="cogs_in",
        splitLayout(  cellWidths = c("80%","20%"),style="margin: 5px;overflow-x: scroll;height:150px;overflow-y: scroll;",
                      div(uiOutput("toorder_tag"),style="font-size: 11px;margin-top: 5px")

        )
    )
  })
  output$edit_fac_labels<-renderUI({
    req(input$tag_fun=='label')
    div(
      inline(  uiOutput("tag_edit_level")),
      inline(  uiOutput("tag_edit_newlevel")),
      inline(actionButton("tag_edit_apply","Rename"))
    )
  })


  output$data_create<-renderUI({
    req(length(vals$hand_save)>0)
    req(input$hand_save=="create")

    data_store$df<-F

    res<-switch (vals$hand_save,
                # "create_codebook"=textInput("codebook_newname", NULL,paste0(input$data_hc,"Codebook")),
                 "Transpose Datalist"=textInput("transp_newname", NULL,bag_name()),
                 "Save new som in" = textInput("model_newname", NULL,bag_somname()),
                 "Save changes"= textInput("data_newname", NULL,bag_name()),
                 "Include binary columns in the Numeric-Attribute"= textInput("data_newname", NULL,bag_name_new()),
                 "Include ordinal variables in the Numeric-Attribute"= textInput("data_newname", NULL,bag_name_new()),
                # "Save Clusters"= textInput("hc_newname", NULL,bag_hc()),
                 "Save new data clusters"= textInput("mc_newname", NULL,bag_mp()),
                 "Create data list from aggregation results"= textInput("agg_newname", NULL,bag_agg()),
                 "Add an Extra-Layer-Attribute to the Datalist"=textInput("extra_layer_newname", NULL,bag_extralayer()),

                 "Save diversity results"= textInput("div_newname", NULL,bag_divname()),
                 "Save rarefied data"= textInput("rare_newname", NULL,bag_name()),

                 "add partition"= textInput("split_newname", NULL,bag_partition()),
                 "Save som predictions"= textInput("predsom_newname", NULL,predsom_name()),
                 "Save errors from som predictions (X)"= textInput("predsom_eX_newname", NULL,bag_sompred_eX()),
                 "Save errors from som predictions (Y)"= textInput("predsom_eY_newname", NULL,bag_sompred_eY()),




                 "Merge Datalists (cols)"= textInput("mergec_newname", NULL,bag_merge()),
                 "Merge Datalists (rows)"= textInput("merger_newname", NULL,bag_merge())



    )
    data_store$df<-T
    res
  })




  output$data_over<-renderUI({

    data_overwritte$df<-F
    req(input$hand_save=="over")
    validate(need(vals$hand_save!='Save som predictions',"This functionality is unavailable for saving SOM predictions."))
    res<-switch (vals$hand_save,
                 'create_codebook' = selectInput("codebook_over", NULL,choices=c(names(vals$saved_data)),selected=input$data_upload),
                 'Save new som in' = {
                   if(length(attr(vals$saved_data[[input$data_som]],"factors","som"))>0){
                     selectInput("model_over", NULL,choices=c(names(attr(vals$saved_data[[input$data_som]],"factors","som"))),selectize=T)
                   }else{
                     column(12,verbatimTextOutput("nosommodel"))
                   }
                 },
                 'Transpose Datalist' = selectInput("transp_over", NULL,choices=c(names(vals$saved_data)),selected=input$data_upload),
                 'Save changes' = selectInput("data_over", NULL,choices=c(names(vals$saved_data)),selected=input$data_upload),
                 'Include binary columns in the Numeric-Attribute' = selectInput("data_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Include ordinal variables in the Numeric-Attribute' = selectInput("data_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save new data clusters' = selectInput("mc_over", NULL,choices=c(colnames(attr(vals$saved_data[[vals[[ns_tab5("somplot_args")]]$newdata]],"factors"))),selectize=T),




                 'Create data list from aggregation results' = selectInput("agg_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save diversity results' = selectInput("div_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save rarefied data' = selectInput("rare_over", NULL,choices=c(names(vals$saved_data)),selectize=T),



                 'Save errors from som predictions (Y)' = selectInput("predsom_eY_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save errors from som predictions (X)' = selectInput("predsom_eX_over", NULL,choices=c(names(vals$saved_data)),selectize=T),


                 'Add an Extra-Layer-Attribute to the Datalist' = selectInput("extra_layer_over", NULL,choices=c(names(attr(vals$saved_data[[input$cur_data]],"extra_shape"))),selectize=T),

                 'add partition' = selectInput("split_over", NULL,choices=colnames(attr(vals$saved_data[[input$data_upload]],"factors")),selectize=T),
                 'Save som predictions' = {
                   if(length(attr(vals$saved_data[[input$data_som]],"factors","predictions"))>0){
                     selectInput("predsom_over", NULL,choices=c(names(attr(vals$saved_data[[input$data_som]],"factors","predictions"))),selectize=T)
                   }else{
                     column(12,verbatimTextOutput("nopredictions"))
                   }

                 },


                 "Merge Datalists (cols)"= selectInput("mergec_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 "Merge Datalists (rows)"= selectInput("merger_over", NULL,choices=c(names(vals$saved_data)),selectize=T)
    )
    data_overwritte$df<-T
    res
  })
  observeEvent(ignoreInit = T,input$go_na,{
    vals$impute<-T
    #savereac()
    req(input$na_targ=="Factor-Attribute")
    data<-vals$saved_data[[input$data_upload]]
    transf<-attr(data,"transf")

    factors=attr(data,"factors")
    transf<-data.frame(attr(data,"transf"))

    factors<-nadata(data=factors,
                    na_method=input$na_method, k=input$na_knn
    )

    if(nrow(transf)>0){
      transf["Factor_imp",ncol(transf)]<-input$na_method
      attr(data_cogs$df,"transf")<-transf
    }

    attr(data,"factors")<-factors
    data_cogs$df<-data

    vals$hand_save<-"Save changes"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(
      hand_save_modal()
    )
  })
  bag_partition<-reactive({



    name0<- if(input$split_options=='Random sampling'){
      "Partition"
    } else{
      paste0("Partition_",input$splitfactor)
    }

    data<-attr(vals$saved_data[[vals$cur_data]],"factors")
    name1<-make.unique(c(colnames(data),name0), sep="_")
    name1[ncol(data)+1]
  })
  bag_name<-reactive({


    name0<-paste0(vals$cur_data,
                  if(length(input$transf)>0){
                    if(input$transf!="None") {paste0("_",input$transf)}
                  },
                  if(length(input$scale)>0) {
                    if(isTRUE(input$scale)) {paste0("_scaled")}
                  },
                  if(length(input$rareabund)>0) {
                    if(isTRUE(input$rareabund)) {paste0("[,-",paste0("Abund<",input$pct_rare,"%"),"]")}
                  },
                  if(length(input$rarefreq)>0) {
                    if(isTRUE(input$rarefreq)) {paste0("[,-",paste0("Freq<",input$pct_prev,"%"),"]")}
                  },
                  if(length(input$raresing)>0) {
                    if(isTRUE(input$raresing)) {paste0("[,-",input$raresing,"]")}
                  }
    )

    if(isTRUE(vals$impute)){
      topaste<-switch(input$na_method,
                      "median/mode"="(imp_medmod)",
                      "Knn"="(imp_knn)",
                      "bagImpute"="(imp_bagI)",
                      "medianImpute"="(imp_medI)",
                      "missForest"="(imp_missF)",
                      "mice"="(imp_mice)")

      name0<-paste0(name0,topaste)

    }
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]



  })
  observeEvent(ignoreInit = T,input$radio_cogs,{
    vals$impute<-F
  })


  data_map<-reactive({
    name<-ll_data$server_inputs('map_data')$name
    req(name)
    data<-vals$saved_data[[name]]

    attr<-ll_data$server_inputs('map_data')$attr
    if(attr=="Factor-Attribute"){
      coords<-attr(data,"coords")
      factors<-attr(data,"factors")
      attr(factors,"factors")<-factors
      attr(factors,"coords")<-coords
      data<-factors
    }
    data
  })
  data_inputs<-reactive({
    args<-ll_data$server_inputs('map_data')
    req(args$name)
    req(args$attr)
    req(args$var)
    req(args$var%in%colnames(data_map()))
    req(args$filter)
    args
  })
  observeEvent(ll_data$server_inputs('map_data')$name,{
    vals$cur_data<-ll_data$server_inputs('map_data')$name
  })
  observeEvent(ll_data$server_inputs('map_data')$var,{
    vals$cur_var_map<-ll_data$server_inputs('map_data')$var
  })
  observeEvent(ll_data$server_inputs('map_data')$filter,{
    vals$cur_factor_filter<-ll_data$server_inputs('map_data')$filter
  })
  observeEvent(ll_data$server_inputs('map_data')$filter_level,{
    vals$cur_level_filter<-ll_data$server_inputs('map_data')$filter_level
  })
  output$map_header<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<-ll_data$ui("map_data")
    ll_data$server('map_data',vals)
    res
  })
  output$map_body<-renderUI({
    req(length(vals$saved_data)>0)
    res<-div(
      hidden(uiOutput("invalid_data_map")),
      withSpinner(type=8,color="SeaGreen",uiOutput("map_data_out")))
    res
  })

  validate_data_map<-reactive({
    validate_data(vals$data_map)
  })

  output$invalid_data_map<-renderUI({
   column(12, em(validate_data_map(),style="color: gray"))
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

  output$map_data_out<-renderUI({

    res<-llet$ui("llet",colors_img=vals$colors_img)

    res

  })
  reac_map<-reactive({
    req(data_plot())
    list(
      data_plot(),
      vals$newcolhabs
    )
  })
  data_plot<-reactive({
    args<-data_inputs()
    vals$data_map<-NULL
    args$saved_data<-vals$saved_data
    res<-do.call(get_data_map,args)

    res
  })
  observeEvent(data_plot(),{

    vals$data_map<-data_plot()
  })
  observe({

    llet$server('llet',vals=vals)
  })





  runjs("Shiny.setInputValue('last_btn', 'main_panel');")
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

    saveRDS(reactiveValuesToList(input),"input.rds")
    saveRDS(tosave,"savepoint.rds")
    beep(10)

  })
  observeEvent(ignoreInit = T,input$flashsave,{
    savereac()

  })
  updateTextInput(session, "tabs", value = 'menu_som')
  updateTextInput(session, "tabs", value = 'menu_intro')
  delay(500,{
    if(!length(grep("connect/apps",getwd()))>0) {
      if(file.exists(paste0(getwd(),'/savepoint.rds'))){
        showModal(
          load_qsave_modal()
        )
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

  output$menu_som_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(
      div(
        style="position: absolute; top: -30px; left: 350px",
        # switchInput("callmodule_som","module version",value=T,size="mini", inline=T,labelWidth="110px",handleWidth="30px",onLabel = "3.0",offLabel = "1.0")
      ),
      uiOutput("module_som_call"))


  })
  output$module_som_call<-renderUI({
    res<-  ui_supersom("som")
    mod_som<-callModule(server_supersom,"som",vals=vals, df_colors=vals$colors_img,newcolhabs=vals$newcolhabs)
    res
  })
  output$menu_upload_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<-module_ui_desctools("module_desctools")
    mod_desctools<-callModule(module_server_desctools, "module_desctools",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs,df_symbol=df_symbol)
    removeModal()
    res
  })
  output$menu_som2_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    res<-module_ui_som2("som2")
    mod_som2<-callModule(module_server_som2,"som2",vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    res
  })
  observeEvent(input$dimension,{
    delay(500,{vals$dimension_display<-input$dimension})
  })
  output$sys_info<-renderUI({
    sys_info<-readRDS('inst/www/sys_info.rds')
    do.call('div',args=list(sys_info))
  })
  exportTestValues(saved_data=vals$saved_data,data_cogs=data_cogs)
  #end_server<-Sys.time()
  #saveRDS(end_server-init_server,'server_time.rds')
}
