
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server<-function(input, output, session) {
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })


  middle <- Sys.time()

  ns_tab4<-NS('hc_tab4')
  ns_tab5<-NS('hc_tab5')

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





  js_code_add_unsup <- '
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(6) > a").addClass("teste_go");
    });
    '
  js_code_add_sup <- '
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(7) > a").addClass("teste_go");
    });
    '

  js_code_add_ens <- '
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(8) > a").addClass("teste_go");
    });
    '

  #sidebar-main > div.sidebar-menu > li:nth-child(6) > a
  js_code_remove <- '$(".teste_go").removeClass("teste_go");'

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
    x%in%c('menu_intro','menu_upload','menu_explore','menu_maps','menu_div')
  }


  which_menu<-function(x){
    if(is_toolmenu(x)){return("tool_menu")} else if(is_unsupmenu(x)){'unsup_expand'}else
      if(is_supmenu(x)){'sup_expand'}else  if(is_ccmenu(x)){'cc_expand'} else{"none"}
  }

  menu_is_in_expand <- function(menu, expand) {
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
    saved_maps=NULL,
    saved_divset_value=0,
    som_unsaved=0,
    bmu_p_symbol_size=1,
    bmu_symbol_size=1,
    bmu_p_bg_transp=0.3,
    bmu_bg_transp=0.3,
    bmu_p_factors=1,
    bmu_insertx=0,
    bmu_inserty=0,
    bmu_ncol=1,
    bmu_leg_transp=0,
    bmu_cexvar=1,
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
      palette1<-dataURI(file =outfile,mime = "image/png")
      vals$colors_img<-rbind(vals$colors_img,data.frame(val=palname,img= sprintf(paste0(img(src = palette1, height = '20',width = '100',style="margin-top:-40px;margin-bottom:-40px;margin-left:-20px;margin-right:-20px;") ))))
      vals$newcolhabs[[palname]]<-colorRampPalette(colors,alpha=T)

    }
    vals$once_cols<-T

  })

  observeEvent(vals$newcolhabs,{
    req(is.null(vals$once_cols))
    get_colorimages()

  })




  stopmap<-reactiveValues(df=F)
  gomap<-reactiveValues(df=F)
  reac_stack_order<-reactiveValues(df=0)
  upload_bag<-reactiveValues(df=0, df0=0)
  stopbigmap<-reactiveValues(df=F)
  warbigmap<-reactiveValues(df=F)
  cur_surface<-reactiveValues(df=F)
  cur_map_filter1<-reactiveValues(df="None")
  cur_map_filter2<-reactiveValues(df=1)
  vars_fac<-reactiveValues(df=0)
  vars_data<-reactiveValues(df=0)
  map_new<-reactiveValues(df=F)
  added_ss3d<-reactiveValues(df=NULL)
  ss1<-reactiveValues(df=NULL)
  ss_map<-reactiveValues(df=NULL)
  res_map_cur<-reactiveValues(df=20000)
  mantel_plot<-reactiveValues(df=0)
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
    symbol1<-dataURI(file = paste0('inst/app/www/pch',i,".png"), mime = "image/png")

    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}


  observeEvent(ignoreInit = T,input$choices_map,{
    vals$cur_choices_map=input$choices_map
  })


  observeEvent(ignoreInit = T,input$data_bank,{
    vals$cur_data<-input$data_bank
  })



  layers_choices<-reactive({
    base_shape<-attr(vals$saved_data[[input$data_bank]],"base_shape")
    layer_shape<-attr(vals$saved_data[[input$data_bank]],"layer_shape")
    eshape<-attr(vals$saved_data[[input$data_bank]],"extra_shape")

    pic<-which(unlist(lapply(list(base_shape,layer_shape),function(x)length(x)>0)))
    choices=c(c("Base Shape","Layer Shape"),names(eshape))
    choices
  })
  output$menu_bank_out<-renderUI({

    column(12, class="databank_page",style="margin-left: 5px",
           uiOutput("bank_tools"),

           uiOutput("view_out")
    )
  })
  output$view_out<-renderUI({
    req(input$view_datalist)
    column(12,id="view_out",
           div(style="margin-top: 20px;",
               switch(input$view_datalist,
                      'data'= uiOutput("viewdata"),
                      'factors'=uiOutput("viewfactors"),
                      'coords'= uiOutput("viewcoords"),
                      'extra_shape'= uiOutput("viewshapes"),
                      'som'= uiOutput("viewsom"),
                      'rf'=   uiOutput("viewrf"),
                      'nb'=uiOutput("viewnb"),
                      'svm'=uiOutput("viewsvm"),
                      'knn'=uiOutput("viewknn"),
                      'sgboost'=uiOutput("viewsgboost"),
                      'xyf'=uiOutput("viewxyf"),
                      'notes'=uiOutput("comments")
               )



           ))
  })
  output$viewfactors<-renderUI({

    div(
      div(class="datalist_tools",h5(
        span(strong("Factor-Attribute")),
        actionButton("dfcogs2",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-download"),"Download table")),
        #tipify(actionButton("replace_factors",icon(verify_fa = FALSE,name=NULL,class="fas fa-file-upload")),"Replace Factor-Attribute")
      )),

      column(12,style=" background: white;",
             uiOutput("factor_attr")
      )

    )


  })
  output$viewcoords<-renderUI({

    if(is.null(attr(getdata_bank(),"coords"))) {
      fluidRow(

        column(12,
               p(strong("No coords found in Datalist:",style="color: red"),em(input$data_bank))),
        uiOutput("add_coords_intru"),
        column(12,style="margin-top: 20px",
               splitLayout(
                 cellWidths = c("30%","20%"),
                 column(12,
                        fileInput(inputId = "add_coords_file",label = NULL)),
                 column(12,
                        uiOutput("add_coords_button"))

               ))

      )
    } else{
      div(
        h5(id="databank_coords",strong("Coords-Attribute"),tipify(actionButton("downcenter_coords",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")),"Download table"),
           actionButton("delete_coords",icon(verify_fa = FALSE,name=NULL,class="fas fa-trash-alt"))),
        tags$style('#DTcoords td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
        tags$style('#DTcoords th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
        inline(
          DT::dataTableOutput("DTcoords")
        )
      )
    }

  })
  output$DTcoords<-DT::renderDataTable(data.frame(attr(getdata_bank(),"coords")),options = list(pageLength = 15, info = FALSE,lengthMenu = list(c(15, -1), c( "15","All")), autoWidth=T,dom = 'lt',scrollX = TRUE, scrollY = "400px"), rownames = TRUE,class ='cell-border compact stripe')
  output$viewbase<-renderUI({
    req(input$pick_elayers)
    req(input$pick_elayers=="Base Shape")

    column(12,
           #renderPlot(bankShpfile()),
           if(is.null(attr(getdata_bank(),"base_shape"))) {fluidRow(

             column(12,
                    p(strong("No base_shape found in Datalist:",style="color: red"),em(input$data_bank))),
             uiOutput("add_base_intru"),
             column(12,style="margin-top: 20px",
                    splitLayout(
                      cellWidths = c("30%","20%"),
                      column(12,
                             fileInput(inputId = "add_base_file",label = NULL)),
                      column(12,
                             uiOutput("add_base_button"))

                    ))

           )} else {
             renderPlot({
               base_shape<-attr(getdata_bank(),"base_shape")
               ggplot(st_as_sf(base_shape)) + geom_sf()+
                 theme(panel.background = element_rect(fill = "white"),
                       panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))
             })
           }

    )
  })
  output$viewlayer<-renderUI({

    req(input$pick_elayers)
    req(input$pick_elayers=="Layer Shape")
    div(
      if(is.null(attr(getdata_bank(),"layer_shape"))) {fluidRow(

        column(12,
               p(strong("No layer_shape found in Datalist:",style="color: red"),em(input$data_bank))),
        uiOutput("add_layer_intru"),
        column(12,style="margin-top: 20px",
               splitLayout(
                 cellWidths = c("30%","20%"),
                 column(12,
                        fileInput(inputId = "add_layer_file",label = NULL)),
                 column(12,
                        uiOutput("add_layer_button"))

               ))

      )} else    {
        fluidRow(

          renderPlot({
            layer_shape<-attr(getdata_bank(),"layer_shape")
            ggplot(st_as_sf(layer_shape)) + geom_sf()+
              theme(panel.background = element_rect(fill = "white"),
                    panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))

          })
        )
      }


    )
  })
  #######

  observeEvent(ignoreInit = T,input$delete_datalist,{
    req(length(input$del_datalist)>0)
    pic<-which(names(vals$saved_data)%in%input$del_datalist)
    vals$saved_data<-vals$saved_data[-pic]
    removeModal()
    runjs("Shiny.setInputValue('last_btn', 'fade');")
  })
  output$comments<-renderUI({
    div(
      h4(strong("Comments"),inline(uiOutput('comments_edit')),inline(uiOutput('comments_remove')),inline(uiOutput('comments_down'))),
      div(   uiOutput('comment_data'))
    )
  })
  output$comments_down<-renderUI({
    downloadButton('comments_downs', label=NULL)
  })
  output$comments_downs<-{
    downloadHandler(
      filename = function() {
        paste(input$data_bank,"_comment",Sys.Date(), ".txt", sep = "")

      },
      content = function(file) {
        res<-attr(vals$saved_data[[input$data_bank]],"notes")
        res<-gsub("<br/>","\n",res)
        writeLines(res, file,sep='\n')
        beep(10)
      }
    )
  }
  observeEvent(ignoreInit = T,input$comments_edit,{
    showModal( modal_comment())
  })
  output$comment_data<-renderUI({
    HTML(attr(vals$saved_data[[input$data_bank]],"notes"))
  })
  output$comments_edit<-renderUI({
    actionButton("comments_edit",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"))
  })
  output$comments_remove<-renderUI({
    div(
      popify(bsButton("comments_remove",icon(verify_fa = FALSE,name=NULL,class="fas fa-trash"), style="button_active"),NULL,"Delete comment")
    )
  })
  observeEvent(ignoreInit = T,input$comments_remove,{
    attr(vals$saved_data[[input$data_bank]],"notes")<-NULL
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
    textAreaInput("note_new", "Comments:", value = gsub("<br/>","\n",attr(vals$saved_data[[input$note_targ]],"notes")), width = '500px',
                  height = '150px', cols = NULL, rows = NULL, placeholder = NULL,
                  resize = "both")
  })
  output$note_target<-renderUI({
    selected=ifelse(input$tabs=='menu_upload',input$data_bank,vals$cur_data)
    tags$div(style="max-width: 250px",pickerInput("note_targ","1. Select the target Datalist:",names(vals$saved_data), selected=selected))
  })
  output$factor_attr<-renderUI({
    validate(need(ncol(attr(getdata_bank(),"factors"))<1000,"Preview not available for data with more than 1000 columns"))
    div(
      DT::dataTableOutput("DT_factors")
    )
    #screenshot(scale=3, timer=2)

  })
  output$DT_factors<-DT::renderDataTable(attr(getdata_bank(),"factors"),options = list(pageLength = 15,lengthMenu = list(c(15, -1), c( "15","All")), autoWidth=F,scrollX = TRUE, scrollY = "400px"), rownames = TRUE,class ='cell-border compact stripe', editable=T)
  observeEvent(ignoreInit = T,input$tools_save_changes,{

    # updateRadioGroupButtons(session,'radio_cogs',selected = last_btn$equal[1])

    vals$hand_save<-"Save changes"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(
      hand_save_modal()
    )


  })
  output$viewdata_war<-renderUI({
    req(length(vals$saved_data)>0)
    req(input$data_bank)
    data=vals$saved_data[[input$data_bank]]
    req(length(attr(data,"data.factors"))>0)
    column(12,h5(
      p(strong("Wargning:",style="color: red"),
        strong("Factor/character columns moved from the Numeric-Attribute to Factor-Attribute")),
      p("A Numeric-Attribute can handle only numeric variables. Use ",icon(verify_fa = FALSE,name=NULL,class="fas fa-cog"),">","Exchange Factors/Variables' to convert factors to numeric variables")
    ))
  })
  observeEvent(ignoreInit = T,input$data_bank,{

    output$viewdata<-renderUI({

      div(
        div(class="datalist_tools",
            h5(strong("Numeric-Attribute"),
               inline(
                 div(
                   id="ddcogs",
                   div(class="ddcogs0",span(icon(verify_fa = FALSE,name=NULL,class="fas fa-cog"),style="padding-left: 6px")),
                   div(class='ddlinks_all',
                       div(class="ddlinks",
                           actionLink("ddcogs1","Rename Variables")
                       ),
                       div(class="ddlinks",
                           actionLink("ddcogs3","Edit changes")
                       ),
                       div(class="ddlinks",
                           actionLink("ddcogs2","Download table")
                       )
                   )


                 )

               )
            )),

        column(12,style=" background: white;",
               uiOutput("data_attr")
        )

      )


    })
  })
  output$bank_tools<-renderUI({
    column(12,id="bank_tools",
           inline(
             fluidRow(id="bank_tools_in",
                      div(
                        span(
                          inline(
                            uiOutput("bank_input1")
                          ),

                          inline(
                            uiOutput("tools_bank")
                          )
                        )




                      ))
           ))
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
  output$bank_input1<-renderUI({

    div(
      pickerInput("data_bank",NULL,choices=names(vals$saved_data), selected=vals$cur_data, width="200px")
    )
  })
  output$tools_bank<-renderUI({
    data<-getdata_bank()

    choices<-list(
      "data","factors","coords",
      "extra_shape",
      if(!is.null(attr(data,"som"))){"som"},
      if(check_model0(data,"rf")){"rf"},
      if(check_model0(data,"nb")){"nb"},
      if(check_model0(data,"svm")){"svm"},
      if(check_model0(data,"knn")){"knn"},
      if(check_model0(data,"sgboost")){"sgboost"},
      if(check_model0(data,"xyf")){"xyf"},
      if(!is.null(attr(data,"notes"))){"notes"}
    )

    attributes<-list(
      div(id="001",icon(verify_fa = FALSE,name=NULL,class="fas fa-archive")),
      div(id="c002",icon(verify_fa = FALSE,name=NULL,class="fas fa-boxes")),
      div(id="003",icon(verify_fa = FALSE,name=NULL,class="fas fa-map-marker-alt")),
      div(id="004",icon(verify_fa = FALSE,name=NULL,class="far fa-map")),
      if(!is.null(attr(data,"som"))){ div(id="006",icon(verify_fa = FALSE,name=NULL,class="fas fa-braille"))},
      if(check_model0(data,"rf")){ div(id="007",span(icon(verify_fa = FALSE,name=NULL,class="fas fa-tree"),icon(verify_fa = FALSE,name=NULL,class="fas fa-tree", style = "margin-left: -8px;"),icon(verify_fa = FALSE,name=NULL,class="fas fa-tree", style = "margin-left: -8px;")))},
      if(check_model0(data,"nb")) { div(id="008", img(src=nb_icon2,height='20',width='20'))},
      if(check_model0(data,"svm")) { div(id="009", img(src=svm_icon2,height='20',width='20'))},
      if(check_model0(data,"knn")) { div(id="010", img(src=knn_icon2,height='20',width='20'))},
      if(check_model0(data,"sgboost")) { div(id="011", img(src=sgboost_icon2,height='20',width='20'))},
      if(check_model0(data,"xyf")) { div(id="013",icon(verify_fa = FALSE,name=NULL,class="fas fa-ellipsis-v"),span("~",style="font-size: 8px; padding: 0px; margin: 0px"),icon(verify_fa = FALSE,name=NULL,class="fas fa-braille"))},
      if(!is.null(attr(data,"notes"))) { div(id="012",icon(verify_fa = FALSE,name=NULL,class="fas fa-comment"))}
    )


    attributes<-attributes[which(unlist(lapply(attributes, function(x) !is.null(x))))]
    choices<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]
    if(is.null(vals$curview_databank)){vals$curview_databank<-"data"}
    div(

      # renderPrint(names(attr(data,"rf"))),
      inline(
        radioGroupButtons(
          "view_datalist",NULL,
          choiceNames =attributes,
          choiceValues =choices,status="view_datalist",selected = vals$curview_databank

        )
      ),
      bsPopover("001",NULL,paste0(span("Numeric-Attribute", style="color: red"))),
      bsPopover("c002",NULL,"Factor-Attribute"),
      bsPopover("003",NULL,"Coords-Attribute"),
      bsPopover("004",NULL,"Shapes-Attribute"),
      bsPopover("006",NULL,"Som-Attribute"),
      bsPopover("007",NULL,"RF-Attribute"),
      bsPopover("008",NULL,"NB-Attribute"),
      bsPopover("009",NULL,"SVM-Attribute"),
      bsPopover("010",NULL,"KNN-Attribute"),
      bsPopover("011",NULL,"GBM-Attribute"),
      bsPopover("013",NULL,"xyf-Attribute"),
      bsPopover("012",NULL,"Comments")
    )

  })
  output$viewsom<-renderUI({
    req(length(attr(getdata_bank(),"som"))>0)
    div(
      splitLayout(
        cellWidths = c("30%","50%"),
        h5(
          span(strong("Som-Attribute"),
               tipify(actionButton("downcenter_som",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")),"Download table")
          ),
          popify(bsButton("trash_som",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"), style="button_active"),NULL,"Select and remove a som")
        )
      ),
      uiOutput("supersom_summary")
      #renderTable(combsom(), rownames = T,striped=T, spacing ="xs")

    )
  })
  output$supersom_summary<-renderUI({
    mlist<-attr(vals$saved_data[[input$data_bank]],"som")
    res<-lapply(mlist,function(m) supersom_summaries(m))

    row1<-data.frame(rownames(res[[1]][[1]]))
    row2<-data.frame(rownames(res[[1]][[2]]))
    colnames(row1)<-colnames(row2)<-"Param"
    res2<-res
    res2$row<-list(row1,row2)
    res2<-res2[c(length(res2),1:(length(res2)-1))]
    res_som<-lapply(names(res2),function(i){
      if(i==names(res2)[1]){
        rownames=F
        colnames=F
        title=em("SOM-Sumaries")
      } else{
        rownames=F
        colnames=T
        title=i
      }
      div(title,style="overflow-x: scroll",
          div(renderTable(res2[[i]][[1]],rownames = rownames,colnames=T)),
          div(renderTable(res2[[i]][[2]],rownames = rownames,colnames=T))
      )
    })

    do.call(splitLayout,res_som)

  })
  observeEvent(ignoreInit = T,input$trash_som ,{
    showModal(
      modalDialog(easyClose = T,
                  title=strong("Remove Som-Attribute"),
                  div(
                    div(
                      div(style="overflow-y: scroll;height: 200px;overflow-x: scroll; padding-top: 10px",
                          checkboxGroupInput("remove_som", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"som")))
                      )
                    ),
                    bsButton("delete_som",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                  )
      )
    )

  })
  observeEvent(ignoreInit = T,input$downcenter_som,{
    vals$hand_down<-"som"
    showModal(downcenter())
  })
  combsom<-reactive({
    soms<-attr(getdata_bank(),"som")
    req(length(soms)>0)
    combsom<-do.call("cbind",lapply(soms,train.summary_fun))
    colnames(combsom)<-names(soms)
    combsom
  })
  combsom_down<-reactive({
    combsom_down<-do.call("cbind",lapply(attr(vals$saved_data[[input$data_bank]],"som"),train.summary_fun))
    colnames(combsom_down)<-names(attr(vals$saved_data[[input$data_bank]],"som"))
    data.frame(combsom_down)
  })
  observeEvent(ignoreInit = T,input$view_datalist,
               {vals$curview_databank<-input$view_datalist})
  output$data_attr<-renderUI({
    validate(need(ncol(getdata_bank())<1000,"Preview not available for data with more than 1000 columns"))
    div(
      DT::dataTableOutput("DT_data")
    )
    #screenshot(scale=3, timer=2)

  })
  output$DT_data<-DT::renderDataTable(
    getdata_bank(),
    extensions = c('FixedColumns',"FixedHeader"),
    options = list(
      pageLength = 15,
      info = FALSE,
      lengthMenu = list(c(15, -1), c( "15","All")),
      autoWidth=F,
      scrollX = TRUE,
      scrollY = "400px",
      fixedHeader=TRUE,
      fixedColumns = list(leftColumns = 1, rightColumns = 0)),
    rownames = TRUE,
    class ='cell-border compact stripe',
    editable=T)
  observeEvent(ignoreInit = T,input$DT_data_cell_edit, {
    row <-input$DT_data_cell_edit$row
    clmn<-input$DT_data_cell_edit$col
    value<-if(input$DT_data_cell_edit$value==""){NA}else{as.numeric(input$DT_data_cell_edit$value)}
    vals$saved_data[[input$data_bank]][row, clmn]<-value
  })
  getdata_bank<-reactive({
    req(length(vals$saved_data)>0)
    req(input$data_bank)
    vals$saved_data[[input$data_bank]]
  })

  #global knn
  {
    getglobal_knn<-reactive({
      getglobal_model(vals$saved_data[[input$data_bank]],'knn')
    })
    output$knn_remove<-renderUI({
      div(
        popify(bsButton("trash_knn",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"), style="button_active"),NULL,"Select and remove a knn")

      )
    })
    observeEvent(ignoreInit = T,input$trash_knn ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove KNN-Attribute"),
                    div(
                      div(
                        div(style="oveknnlow-y: scroll;height: 200px;oveknnlow-x: scroll; padding-top: 10px",
                            checkboxGroupInput("remove_knn", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"knn")))
                        )
                      ),
                      bsButton("delete_knn",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })

    observeEvent(ignoreInit = T,input$delete_knn,{
      attr(vals$saved_data[[input$data_bank]],"knn")[input$remove_knn]<-NULL
      removeModal()
    })
    output$viewknn<-renderUI({
      knn_regs<-getglobal_knn()$regs
      knn_class<-getglobal_knn()$class

      div(
        h4(strong("knn-Attribute"),inline(uiOutput('knn_remove'))),
        numericInput("knn_round","Round table:",value=3, step=1, min=1,width="100px"),
        if(length(knn_class)>0){
          column(12,style="background: white",

                 div(span(
                   strong("Classification models"),inline(
                     div(actionButton("down_knntable_class",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                   )
                 )),
                 tags$style('#knntab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#knntab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput('knntab_class'))

          )},
        hr(),
        if(length(knn_regs)>0){
          column(12,style="background: white",
                 div(span(
                   strong("Regression models"),inline(
                     div(actionButton("down_knntable_reg",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                   )
                 )),
                 tags$style('#knntab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#knntab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput('knntab_regs'))
          )
        }
      )
    })

    output$knntab_class<-DT::renderDataTable({
      vals$knntable_class<-table<-getglobal_knn()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$knn_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't',scrollX = TRUE, scrollY = "400px")),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    }, rownames = TRUE,class ='compact cell-border')
    output$knntab_regs<-DT::renderDataTable({
      vals$knntable_reg<-table<-getglobal_knn()$regs
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$knn_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(table,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't',scrollX = TRUE, scrollY = "400px")),
          c(1), `border-left` = "solid 1px"
        ),       c(9,13), `border-right` = "solid 1px"
      )

    }, rownames = TRUE,class ='compact cell-border')
    observeEvent(ignoreInit = T,input$down_knntable_reg,{
      vals$hand_down<-"knn_stats_reg"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$down_knntable_class,{
      vals$hand_down<-"knn_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
  }

  #global sgboost
  {
    getglobal_sgboost<-reactive({
      getglobal_model(vals$saved_data[[input$data_bank]],'sgboost')
    })
    output$sgboost_remove<-renderUI({
      div(
        popify(bsButton("trash_sgboost",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"), style="button_active"),NULL,"Select and remove a GBM model")
      )
    })

    observeEvent(ignoreInit = T,input$trash_sgboost ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove GBM-Attribute"),
                    div(
                      div(
                        div(style="ovesgboostlow-y: scroll;height: 200px;ovesgboostlow-x: scroll; padding-top: 10px",
                            checkboxGroupInput("remove_sgboost", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"sgboost")))
                        )
                      ),
                      bsButton("delete_sgboost",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })

    observeEvent(ignoreInit = T,input$delete_sgboost,{
      attr(vals$saved_data[[input$data_bank]],"sgboost")[input$remove_sgboost]<-NULL
      removeModal()
    })
    output$viewsgboost<-renderUI({
      sgboost_regs<-getglobal_sgboost()$regs
      sgboost_class<-getglobal_sgboost()$class

      div(
        h4(strong("GBM-Attribute"),inline(uiOutput('sgboost_remove'))),
        numericInput("sgboost_round","Round table:",value=3, step=1, min=1,width="100px"),
        if(length(sgboost_class)>0){
          column(12,style="background: white",

                 div(span(
                   strong("Classification models"),inline(
                     div(actionButton("down_sgboosttable_class",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                   )
                 )),
                 tags$style('#sgboosttab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#sgboosttab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput('sgboosttab_class'))

          )},
        hr(),
        if(length(sgboost_regs)>0){
          column(12,style="background: white",
                 div(span(
                   strong("Regression models"),inline(
                     div(actionButton("down_sgboosttable_reg",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                   )
                 )),
                 tags$style('#sgboosttab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#sgboosttab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput('sgboosttab_regs'))
          )
        }
      )
    })

    output$sgboosttab_class<-DT::renderDataTable({
      vals$sgboosttable_class<-table<-getglobal_sgboost()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$sgboost_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    }, rownames = TRUE,class ='compact cell-border')
    output$sgboosttab_regs<-DT::renderDataTable({
      vals$sgboosttable_reg<-table<-getglobal_sgboost()$regs
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$sgboost_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(table,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(9,13), `border-right` = "solid 1px"
      )

    }, rownames = TRUE,class ='compact cell-border')
    observeEvent(ignoreInit = T,input$down_sgboosttable_reg,{
      vals$hand_down<-"sgboost_stats_reg"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$down_sgboosttable_class,{
      vals$hand_down<-"sgboost_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
  }


  #global xyf
  {
    getglobal_xyf<-reactive({
      getglobal_model(vals$saved_data[[input$data_bank]],'xyf')
    })
    output$xyf_remove<-renderUI({
      div(
        popify(bsButton("trash_xyf",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"), style="button_active"),NULL,"Select and remove a xyf")
      )
    })

    observeEvent(ignoreInit = T,input$trash_xyf ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove xyf-Attribute"),
                    div(
                      div(
                        div(style="ovexyflow-y: scroll;height: 200px;ovexyflow-x: scroll; padding-top: 10px",
                            checkboxGroupInput("remove_xyf", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"xyf")))
                        )
                      ),
                      bsButton("delete_xyf",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })

    observeEvent(ignoreInit = T,input$delete_xyf,{
      attr(vals$saved_data[[input$data_bank]],"xyf")[input$remove_xyf]<-NULL
      removeModal()
    })
    output$viewxyf<-renderUI({
      xyf_regs<-getglobal_xyf()$regs
      xyf_class<-getglobal_xyf()$class

      div(
        h4(strong("xyf-Attribute"),inline(uiOutput('xyf_remove'))),
        numericInput("xyf_round","Round table:",value=3, step=1, min=1,width="100px"),
        if(length(xyf_class)>0){
          column(12,style="background: white",

                 div(span(
                   strong("Classification models"),inline(
                     div(actionButton("down_xyftable_class",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                   )
                 )),
                 tags$style('#xyftab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#xyftab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput('xyftab_class'))

          )},
        hr(),
        if(length(xyf_regs)>0){
          column(12,style="background: white",
                 div(span(
                   strong("Regression models"),inline(
                     div(actionButton("down_xyftable_reg",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                   )
                 )),
                 tags$style('#xyftab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#xyftab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput('xyftab_regs'))
          )
        }
      )
    })

    output$xyftab_class<-DT::renderDataTable({
      vals$xyftable_class<-table<-getglobal_xyf()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$xyf_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    }, rownames = TRUE,class ='compact cell-border')
    output$xyftab_regs<-DT::renderDataTable({
      vals$xyftable_reg<-table<-getglobal_xyf()$regs
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$xyf_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(table,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(9,13), `border-right` = "solid 1px"
      )

    }, rownames = TRUE,class ='compact cell-border')
    observeEvent(ignoreInit = T,input$down_xyftable_reg,{
      vals$hand_down<-"xyf_stats_reg"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$down_xyftable_class,{
      vals$hand_down<-"xyf_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
  }





  #######


  observeEvent(ignoreInit = T,input$data_div,{
    vals$cur_data<-input$data_div})

  observeEvent(ignoreInit = T,input$data_map,{
    vals$cur_data=input$data_map
  })
  observeEvent(ignoreInit = T,input$data_hc,{
    vals$cur_data=input$data_hc
  })



  output$menu_nb_out<-renderUI({

    vals$module<-"Naive bayes"
    res<-module_ui_nb("module_nb")
    mod1<-callModule(module_server_nb, "module_nb",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })

  output$menu_kmeans_out<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found"))
    vals$module<-"kmeans"

    res<-module_ui_kmeans("module_kmeans")
    mod_kmeans<-callModule(module_server_kmeans, "module_kmeans",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })




  output$menu_ensemble_out<-renderUI({
    res<-module_ui_comp2("ensemble")
    callModule(module_server_comp2, "ensemble",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    res
  })



  output$menu_compare_out<-renderUI({
    res<-module_ui_comp("module_comp")
    callModule(module_server_comp, "module_comp",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs,df_symbol=df_symbol)
    res
  })

  output$menu_sgboost_out<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found"))

    res<-module_ui_sgboost("module_sgboost")
    mod_sgboost<-callModule(module_server_sgboost, "module_sgboost",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })
  output$menu_knn_out<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found"))


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



    res<-module_ui_rf("module_rf")
    mod_rf<-callModule(module_server_rf, "module_rf",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    removeModal()
    res
  })


  output$map_panel<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(
      style="background-color: white",
      column(12,uiOutput("map_part1")),
      column(12,id="side_map",class="map_control_style",
             uiOutput("automap")
      )
      ,
      #column(12,uiOutput("map_teste")),

      uiOutput("map_part2")

    )



  })


  output$automap<-renderUI({
    req(input$saved_maps=='new map'|isTRUE(input$stack_map))
    column(12,

           span("+ Auto-refresh",
                inline(
                  switchInput(
                    inputId = "automap",
                    label = NULL,
                    value = TRUE,
                    onStatus = "success",
                    size ="mini",inline=T
                  )
                ), inline(uiOutput("gomap")),
                inline(uiOutput("automap_war"))
           )
    )
  })
  output$gomap<-renderUI({
    req(isFALSE(input$automap))
    inline(
      bsButton('gomap',span(icon(verify_fa = FALSE,name=NULL,class="fas fa-arrow-alt-circle-right"),"Render Map"), style="button_active", width="100px")
    )
  })



  observeEvent(ignoreInit = T,input$pt_palette,{vals$pt_palette<-input$pt_palette})
  observeEvent(ignoreInit = T,input$showcoords,{vals$showcoords<-input$showcoords})
  observeEvent(ignoreInit = T,input$showfactors,{vals$showfactors<-input$showfactors})
  observeEvent(ignoreInit = T,input$nmax,{vals$nmax<-input$nmax})
  observeEvent(ignoreInit = T,input$nmax_idp,{vals$nmax_idp<-input$nmax_idp})
  observeEvent(ignoreInit = T,input$nmin_idp,{vals$nmin_idp<-input$nmin_idp})
  observeEvent(ignoreInit = T,input$omax,{vals$omax<-input$omax})
  observeEvent(ignoreInit = T,input$maxdist,{vals$maxdist<-input$maxdist})
  observeEvent(ignoreInit = T,input$idp,{vals$idp<-input$idp})
  observeEvent(ignoreInit = T,input$scalesize_color,{vals$scalesize_color<-input$scalesize_color})
  observeEvent(ignoreInit = T,input$scalesize_size,{vals$scalesize_size<-input$scalesize_size})
  observeEvent(ignoreInit = T,input$pt_factor,{vals$pt_factor<-input$pt_factor})
  observeEvent(ignoreInit = T,input$pt_coords,{vals$pt_coords<-input$pt_coords})
  observeEvent(ignoreInit = T,input$pt_points_min,{vals$pt_points_min<-input$pt_points_min})
  observeEvent(ignoreInit = T,input$pt_points,{vals$pt_points<-input$pt_points})
  observeEvent(ignoreInit = T,input$pt_legend,{vals$pt_legend<-input$pt_legend})
  observeEvent(ignoreInit = T,input$showguides,{vals$showguides<-input$showguides})
  observeEvent(ignoreInit = T,input$col_guides,{vals$col_guides<-input$col_guides})
  observeEvent(ignoreInit = T,input$lty_guides,{vals$lty_guides<-input$lty_guides})
  observeEvent(ignoreInit = T,input$lwd_guides,{vals$lwd_guides<-input$lwd_guides})
  observeEvent(ignoreInit = T,input$base_col,vals$base_col<-input$base_col)
  observeEvent(ignoreInit = T,input$base_lighten,vals$base_lighten<-input$base_lighten)
  observeEvent(ignoreInit = T,input$base_col_border,vals$base_col_border<-input$base_col_border)
  observeEvent(ignoreInit = T,input$pt_symbol,{vals$pt_symbol<-input$pt_symbol})
  observeEvent(ignoreInit = T,input$scale_map,{vals$scale_map<-input$scale_map})
  observeEvent(ignoreInit = T,input$colored_map,{vals$colored_map<-input$colored_map})
  observeEvent(ignoreInit = T,input$layer_col,vals$layer_col<-input$layer_col)
  observeEvent(ignoreInit = T,input$layer_lighten,vals$layer_lighten<-input$layer_lighten)
  observeEvent(ignoreInit = T,input$layer_col_border,vals$layer_col_border<-input$layer_col_border)
  observeEvent(ignoreInit = T,input$col_coords,{vals$col_coords<-input$col_coords})
  observeEvent(ignoreInit = T,input$col_factor,{vals$col_factor<-input$col_factor})



  observeEvent(ignoreInit = T,input$order_stack,{
    showModal(
      modalDialog(
        easyClose = T,
        footer = column(12,inline(actionButton("run_order_stack","Apply order")),modalButton("close")),
        column(12,
               fluidRow(
                 rank_list(
                   text = "Drag the layers in any desired order",
                   labels = names(get_stacklist()),
                   input_id = "rank_list_3",
                   class="custom-sortable"
                 ),
                 tags$style(
                   HTML("
          .custom-sortable .rank-list-item {
            height: 25px;
          padding:5px;
           background-color: #BDB;
          border: 1px solid white;
          }
        ")
                 )
               ))
      )
    )
  })

  observeEvent(ignoreInit = T,input$run_order_stack,{

    vals$saved_maps<-vals$saved_maps[input$rank_list_3]
    removeModal()
    reac_stack_order$df<-T


  })
  get_stacklist<-reactive({
    #req(input$saved_maps!='new map')
    req(length(vals$saved_maps)>1)
    res_list<-lapply(vals$saved_maps,function(x) attr(x,"my_rst"))
    nuls<-which(unlist(lapply(res_list, function (x)
      !is.null(x))))
    res_list<-res_list[nuls]
    names(res_list)<-names(vals$saved_maps)[nuls]
    res_list
  })
  observeEvent(ignoreInit = T,input$add_map,{
    updatePickerInput(session,'saved_maps',selected="new map",choices=c("new map",names(vals$saved_maps)))
    if(isTRUE(input$edit_map)){
      updateButton(session,"edit_map",value=F)
    }

  })
  observeEvent(ignoreInit = T,input$cmap,{

    vals$cur_cmap<-input$cmap
  })

  observeEvent(ignoreInit = T,input$save_map,{
    data<-vals$saved_data[[input$data_map]]
    p<-getmap()
    factors<-attr(getdata_map(),"factors")
    var_name=input$var_map
    if(input$cmap=="discrete"){
      vals$hand_save<-"Save discrete model"}
    if(input$cmap=="interpolation"){
      vals$hand_save<-"Save interpolation model"}
    if(input$cmap=="raster"){
      vals$hand_save<-"Save raster"}
    vals$hand_save3<-div(
      span("Warging:",style="color: red"),
      "Maps are exclusively saved within the current session and are not stored in savepoints."
    )
    showModal(
      hand_save_modal()
    )
  })
  output$add_map<-renderUI({
    req(input$saved_maps!='new map')
    inline(bsButton("add_map",tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-plus"),"create a new map"), style="button_active"))

  })
  output$show_saved_maps<-renderUI({
    pickerInput("saved_maps",NULL, choices=c("new map",names(vals$saved_maps)), width="150px")

  })
  output$save_map<-renderUI({
    req(input$saved_maps=='new map'|isTRUE(input$edit_map))

    bsButton('save_map',tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-save"),"Save Map"), style="button_active")
  })
  output$mantel_map<-renderUI({
    req(input$saved_maps=="new map")
    bsButton('mantel_map',tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-chart-line"),"Mantel correlogram"), style="button_active", type="toggle")
  })
  stop_map_war<-reactive({
    div(
      p(strong("Warning: Auto-refresh disabled", style="color:red"),"Use the button 'Render Map' to proceed with Discretization/Interpolation. The selected datalist has", nrow(getdata_map()), "observations. Discretizating/Interpolating big data can be time consuming. The tool Raster can be faster. ")
    )
  })



  output$scatter_3d<-renderUI({
    req(input$cmap=='discrete')
    req(input$choices_map=="Numeric-Attribute")
    inline(tipify(bsButton("scatter_3d",icon(verify_fa = FALSE,name=NULL,class="fas fa-cube"), style="button_active", type="toggle"),"3D mode"))})
  output$stack_scatter_3d<-renderUI({

    req(input$cmap=='discrete')
    # req(input$choices_map=="Numeric-Attribute")
    inline(tipify(bsButton("stack_scatter_3d",icon(verify_fa = FALSE,name=NULL,class="fas fa-layer-group"), style="button_active", type="toggle"),"3D stack"))
  })
  observeEvent(ignoreInit = T,input$scatter_3d,{
    if(input$scatter_3d %% 2){
      updateButton(session,'stack_scatter_3d',value=F)
    }
  })
  observeEvent(ignoreInit = T,input$stack_scatter_3d,{
    if(input$stack_scatter_3d %% 2){
      updateButton(session,'scatter_3d',value=F)
    }
  })
  output$trash_map<-renderUI({
    req(input$saved_maps!='new map')
    inline(tipify(bsButton("trash_map",icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"), style="button_active"),"3D mode"))})
  observeEvent(ignoreInit = T,input$trash_map,{
    if(input$trash_map%%2){
      vals$saved_maps[[input$saved_maps]]<-NULL
      if(length(vals$saved_maps)>0){
        updatePickerInput(session,"saved_maps",selected=names(vals$saved_maps)[1])}}
  })
  output$edit_map<-renderUI({
    req(input$saved_maps!='new map')
    inline(bsButton("edit_map",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"), style="button_active", type="toggle"))})

  output$surface_map<-renderUI({
    req(input$saved_maps!='new map')
    req(!isTRUE(input$edit_map))
    req(!is.null(attr(vals$saved_maps[[input$saved_maps]],"my_rst")))
    bsButton('surface_map',tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-mountain"),"create a surface Map"), style="button_active", type="toggle", value=cur_surface$df)
  })
  observeEvent(ignoreInit = T,input$surface_map,{
    cur_surface$df<-input$surface_map
  })
  output$stack_map<-renderUI({

    req(input$cmap=='interpolation'|input$cmap=='raster')
    #req(!isTRUE(input$edit_map))

    if(!length(vals$saved_maps)>1){
      tags$div(datatitle="Create stacked raster map. Requires more than one saved interpolated map.",style="z-index:100;  delay:0",bsButton('stack_map',icon(verify_fa = FALSE,name=NULL,class="fas fa-layer-group"), style="button_active", type="toggle", disabled = T))
    } else {
      res_list<-get_stacklist()
      if(length(res_list)>1){
        tags$div(datatitle="Create stacked raster map",style="z-index:100;  delay:0",bsButton('stack_map',icon(verify_fa = FALSE,name=NULL,class="fas fa-layer-group"), style="button_active", type="toggle", disabled = F))
      } else{
        tags$div(datatitle="Create stacked raster map. Requires more than one saved interpolated map.",style="z-index:100;  delay:0",bsButton('stack_map',icon(verify_fa = FALSE,name=NULL,class="fas fa-layer-group"), style="button_active", type="toggle", disabled = T))
      }
    }






  })
  output$rgl_map<-renderUI({

    req(input$saved_maps!='new map')
    req(!is.null(attr(vals$saved_maps[[input$saved_maps]],"my_rst")))
    req(isTRUE(input$surface_map))
    bsButton('rgl_map',tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-video"),"Show RGL"), style="button_active", type="toggle")
  })
  output$ss_rgl<-renderUI({
    req(input$cmap=='discrete')
    req(length(vals$ss_map)>1)
    bsButton('ss_rgl',tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-video"),"Show RGL"), style="button_active", type="toggle")
  })
  output$sr_rgl<-renderUI({
    req(isTRUE(input$stack_map))
    bsButton('sr_rgl',tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-video"),"Show RGL"), style="button_active", type="toggle")
  })
  observeEvent(ignoreInit = T,input$sr_rgl,{
    delay(100,

          if(isTRUE(input$sr_rgl)){
            updateButton(session,'sr_rgl',value=F)
          }

    )
  })
  observeEvent(ignoreInit = T,input$ss_rgl,{
    delay(100,

          if(isTRUE(input$ss_rgl)){
            updateButton(session,'ss_rgl',value=F)
          }

    )
  })
  output$map_ssrgl<-renderUI({

    req(isTRUE(input$ss_rgl))
    is_ggplot3_available<-require("rgl")
    validate(need(is_ggplot3_available,"This feature requires the 'rgl' package. Before closing iMESc, please create a savepoint. Then, close the application, install the package, and reopen iMESc. Remember to reload the savepoint to continue where you left off"))

    div(
      renderPrint(rgl.dev.list()),
      rglwidgetOutput("ssrgl_out",  width = 600, height = 600)
    )
  })
  output$map_srrgl<-renderUI({
    req(isTRUE(input$sr_rgl))
    is_ggplot3_available<-require("rgl")
    validate(need(is_ggplot3_available,"This feature requires the 'rgl' package. Before closing iMESc, please create a savepoint. Then, close the application, install the package, and reopen iMESc. Remember to reload the savepoint to continue where you left off"))


    div(
      renderPrint(rgl.dev.list()),
      rglwidgetOutput("srrgl_out",  width = 600, height = 600)
    )
  })


  observe({
    req(isTRUE(input$surface_map))
    is_ggplot3_available<-require("rgl")
    if(isTRUE(is_ggplot3_available)){
      output$m3d_map<-renderRglwidget({

        req(input$saved_maps!="new map")
        req(isTRUE(input$surface_map))

        p1<-vals$saved_maps[[input$saved_maps]]
        zlab<-attr(p1,'args')['var_map']
        p1<- attr(p1,"my_rst")
        p2<-if(length(vals$saved_maps)>1){
          attr(vals$saved_maps[[input$saved_maps2]],"my_rst")
        } else{NULL}

        rgl.open()
        rgl.bg(color=c('white','white'))
        anim_4D(r1=p1,r2=p2,r3=NULL,colors=input$pt_palette, exp=input$surf_exp,   newcolhabs=vals$newcolhabs)
        rglwidget()

      })
    }



  })
  observe({
    req(isTRUE(input$sr_rgl))
    is_ggplot3_available<-require("rgl")

    if(isTRUE(is_ggplot3_available )){
      output$srrgl_out<-renderRglwidget({

        req(isTRUE(input$sr_rgl))

        res_list<-get_stacklist()
        res_list<-res_list[input$stack_layers]
        validate(need(length(res_list)>1,"Requires at least two layers"))
        data<-getdata_map()
        coords<-attr(getdata_map(),"coords")

        zs<-c()
        co<-c()
        labs<-list()
        showlab<-c()
        for(i in 1:length(names(get_stacklist())))
        {
          zs[i]<-input[[paste0("stack_z",i)]]
          co[i]<-input[[paste0("stack_co",i)]]
          showlab[i]<-input[[paste0("stack_lab",i)]]
          labs[[i]]<-attr(getdata_map(),"factors")[rownames(coords), input[[paste0("stack_labels",i)]]]


        }
        layer_shape=if(isTRUE(input$srmap_layer)){
          attr(data,"layer_shape") } else{ NULL}
        srcol_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$srlayer_col,1), input$srlayer_lighten)


        #options(rgl.useNULL = TRUE)
        rgl.bg(color=c('white','white'))

        anim_stack1(res_list,
                    zvalue=zs,
                    xlim=c(input$srlong_xmin,input$srlong_xmax),
                    ylim=c(input$srlat_xmin,input$srlat_xmax),
                    layer_shape=layer_shape,
                    col_layer=srcol_layer,
                    zmin=input$stack_zmin,
                    zmax=input$stack_zmax,
                    newcolhabs=vals$newcolhabs,
                    coords=coords,
                    col.labels=input$col_factor_stack,
                    cex.labels=input$pt_factor_stack,
                    labels=labs,
                    col.coords=input$col_coords_stack,
                    cex.coords=input$pt_coords_stack,
                    show_coords=co,
                    show_labels=showlab,
                    texmipmap=F,texmagfilter="nearest",texminfilter="nearest.mipmap.nearest",texenvmap=F,size=1,point_antialias=F,line_antialias=F,depth_mask=F,lit =F,top=F
        )




      })
      output$ssrgl_out<-renderRglwidget({

        req(isTRUE(input$ss_rgl))
        data=vals$ss_map
        coords=  attr(vals$ss_map,"coords")
        base_shape=if(isTRUE(input$ssmap_base)){
          attr(data,"base_shape") } else{ NULL}
        layer_shape=if(isTRUE(input$ssmap_layer)){
          attr(data,"layer_shape") } else{ NULL}
        sscol_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$sslayer_col,1), input$sslayer_lighten)
        sscol_base=adjustcolor(getcolhabs(vals$newcolhabs,input$ssbase_col,1), input$ssbase_lighten)

        pal<-c()
        cex<-c()
        zs<-c()
        co<-c()
        labs<-list()
        showlab<-c()

        for(i in 1:length(vals$ss_map))
        {
          pal[i]<-input[[paste0("pt_palette",i)]]
          cex[i]<-input[[paste0("ss_cex",i)]]
          zs[i]<-input[[paste0("ss_z",i)]]
          labs[[i]]<-attr(getdata_map(),"factors")[rownames(coords), input[[paste0("ss_labels",i)]]]
          co[i]<-input[[paste0("ss_co",i)]]
          showlab[i]<-input[[paste0("ss_lab",i)]]

        }


        #options(rgl.useNULL = TRUE)
        rgl.bg(color=c('white','white'))

        stack_scatter_rgl(data,coords,base_shape,layer_shape,
                          col.palette=pal,
                          newcolhabs=vals$newcolhabs,
                          pt_cex=cex,
                          spacing=1,
                          expand=input$ss3d_exp,
                          theta=input$ss3d_theta,
                          phi=input$ss3d_phi,
                          r=input$ss3d_eye,
                          d=input$ss3d_d,
                          xlim=c(input$sslong_xmin,input$sslong_xmax),
                          ylim=c(input$sslat_xmin,input$sslat_xmax),
                          col_base=sscol_base,
                          col_layer=sscol_layer,
                          z=zs,
                          zmin=input$ss_zmin,
                          zmax=input$ss_zmax,
                          ticktype=input$ss_ticktype,
                          xlab=input$ss_xlab,
                          ylab=input$ss_ylab,
                          zlab=input$ss_zlab,
                          leglab.adj=input$ss_leglab.pos,
                          legtit.posy=input$ss_legtitle.posy,
                          breaks=input$ss_breaks+1,
                          legwidth=50,
                          legtit.posx=input$ss_legtitle.posx,
                          title.srt=0,
                          lab.srt=0,
                          col.labels=input$col_factor_ss,
                          cex.labels=input$pt_factor_ss,
                          labels=labs,
                          col.coords=input$col_coords_ss,
                          cex.coords=input$pt_coords_ss,
                          show_coords=co,
                          show_labels=showlab,
                          texmipmap=F,texmagfilter="nearest",texminfilter="nearest.mipmap.nearest",texenvmap=F,front="filled", back="points",size=1,point_antialias=F,line_antialias=F,depth_mask=F,lit =F,top=F)




      })
    }

  })
  observeEvent(ignoreInit = T,input$save_map,{
    if(input$saved_maps!='new map'|isTRUE(input$edit_map)){
      updateRadioButtons(session,"hand_save", selected ='over')
    }
    if(input$saved_maps=='new map'){
      updateRadioButtons(session,"hand_save", selected ='create')
    }
  })
  observeEvent(ignoreInit = T,input$surface_map,{
    if(isTRUE(input$surface_map)){
      updateButton(session,"edit_map",value=F)
    }
  })
  observeEvent(ignoreInit = T,input$edit_map,{
    if(isTRUE(input$edit_map)){
      updateButton(session,"surface_map",value=F)
    }
  })
  observeEvent(ignoreInit = T,input$load_savepoint_yes,{
    removeModal()
  })
  observeEvent(ignoreInit = T,input$colored_map,{
    if(isTRUE(input$colored_map)){
      updateCheckboxInput(session,"scalesize_color","Color", F)}
  })
  observeEvent(ignoreInit = T,input$scalesize_color,{
    if(isTRUE(input$scalesize_color)){
      updateCheckboxInput(session,"colored_map",value=F)}
  })
  observeEvent(ignoreInit = T,input$data_map,{
    vals$data_map<-input$data_map
  })
  output$map_part1<-renderUI({
    req(input$saved_maps=="new map"|isTRUE(input$edit_map))

    column(12,
           span(
             inline(pickerInput("data_map",
                                "Y Datalist:",
                                choices =    names(vals$saved_data),

                                selected=vals$data_map, width="200px")),
             inline(
               pickerInput(
                 "choices_map",
                 "Attribute:",
                 choices = c("Numeric-Attribute","Factor-Attribute"),
                 selected=vals$cur_choices_map, width="130px")
             ),
             inline( uiOutput("map_get")),
             inline( uiOutput("map_filter1")),
             inline( uiOutput("map_filter2")),
             inline(uiOutput("ss_add"))
           )
    )
  })
  output$ss_add<-renderUI({
    req(isTRUE(input$stack_scatter_3d))
    tags$div(datatitle="Click to include the variable in the stacked scatter map",
             actionButton("add_scatter_stack",icon(verify_fa = FALSE,name=NULL,class="fas fa-plus"),style = "animation: glowing 1000ms infinite;")
    )
  })

  output$map_filter1<-renderUI({
    req(input$data_map)
    data<-vals$saved_data[[input$data_map]]
    factors<-attr(data,"factors")
    inline(pickerInput("var_map_filter1",label = "Filter",choices = c("None",colnames(factors)), width="150px", selected=cur_map_filter1$df))
  })
  observeEvent(ignoreInit = T,input$var_map_filter1,{
    cur_map_filter1$df<-input$var_map_filter1
  })

  output$map_filter2<-renderUI({
    req(input$var_map_filter1!="None")
    data<-vals$saved_data[[input$data_map]]
    factors<-attr(data,"factors")
    fac<-factors[,input$var_map_filter1]
    inline(pickerInput("var_map_filter2",label = "Level",choices = levels(fac), width="150px", selected = cur_map_filter2$df))
  })
  observeEvent(ignoreInit = T,input$var_map_filter2,{
    cur_map_filter2$df<-input$var_map_filter2
  })
  output$map_get<-renderUI({
    choices=  switch(input$choices_map,
                     'Factor-Attribute'=rev(colnames(attr(getdata_map(),"factors"))),
                     'Numeric-Attribute'=colnames(getdata_map()))
    inline(pickerInput("var_map",label = "Variable:",choices = choices,selected=vals$cur_var_map, width="200px"))
  })

  observe({
    data<-getdata_map()
    vals$vars_fac<-rev(colnames(attr(data,"factors")))
    vals$vars_data<-colnames(data)
  })
  observeEvent(ignoreInit = T,input$saved_maps,{
    if(input$saved_maps=="new map"){
      if(isTRUE(reac_stack_order$df)){
        reac_stack_order$df<-F
        updateButton(session,"stack_map",value=T)} else{
          updateButton(session,"stack_map",value=F)
        }

      updateButton(session,"surface_map", value=F)

    }
  })
  observeEvent(ignoreInit = T,input$stack_map,{
    if(input$stack_map %% 2){
      updatePickerInput(session,'saved_maps',choices=c(names(vals$saved_maps)))
    }

  })



  ##map part
  output$map_part2<-renderUI({
    div(
      uiOutput("side_map00"),
      uiOutput("side_map0_stack"),
      column(8,
             column(12,uiOutput("map_options")),
             column(12,  withSpinner(type=8,color="SeaGreen",uiOutput("map_out"))),
             column(12,  withSpinner(type=8,color="SeaGreen",uiOutput("map05"))))



    )
  })
  get_mapdisc<-reactive({

    res_list<-lapply(vals$saved_maps,function(x) attr(x,"my_rst"))
    nuls<-which(unlist(lapply(res_list, function (x)
      is.null(x))))
    res_list<-res_list[nuls]
    names(res_list)<-names(vals$saved_maps)[nuls]
    res_list


  })
  output$smr_layers_out<-renderUI({
    column(4,id="sidebar_map",class="well2",
           fluidRow(class="map_control_style",
                    column(12,class="well2",
                           fluidRow(
                             p(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Select the layers to stack", placement = "bottom", options=list(container="body")),'+ Layers:'),
                             div(uiOutput("stack_raster")))),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("stack_zlim")
                           )),

                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("srmap_layer")
                           )),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("sr_limits")
                           )),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("show_map_coords_stack")
                           )),
                    uiOutput("show_map_labels_stack"),
                    div(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Click to order the stack layers", placement = "bottom", options=list(container="body")), actionLink("order_stack","+ Order stack")),
                    column(12,
                           fluidRow(class="well2",
                                    div("+ Legend adjustment",
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Legend width", placement = "bottom", options=list(container="body")),
                                               '+ Width:',inline(numericInput("stack_width.legend",NULL, 40, width="75px",step=1))),
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Legend y position", placement = "bottom", options=list(container="body")),
                                               '+ Y pos:',inline(numericInput("stack_legadj",NULL, 0, width="75px",step=.1))),
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Legend bar height", placement = "bottom", options=list(container="body")),
                                               '+ Bar height:',inline(numericInput("stack_legbar.h",NULL, 0.2, width="75px",step=.1))),
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Adjustment of the titles along the y-axis:", placement = "bottom", options=list(container="body")),
                                               '+ Title pos:',inline(numericInput("stack_legtitle.pos",NULL, 0.3, width="75px",step=.1))),
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Adjustment of the labels along the y-axis:", placement = "bottom", options=list(container="body")),
                                               '+ Label pos:',inline(numericInput("stack_leglab.pos",NULL, 0.3, width="75px",step=.1)))

                                    ))
                    )

           )
    )
  })


  output$side_map0_stack<-renderUI({
    req(isTRUE(input$stack_map))
    column(4,id="sidebar_map",class="well2",
           fluidRow(class="map_control_style",
                    column(12,class="well2",
                           fluidRow(
                             p(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Select the layers to stack", placement = "bottom", options=list(container="body")),'+ Layers:'),
                             div(uiOutput("stack_raster")))),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("stack_zlim")
                           )),

                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("srmap_layer")
                           )),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("sr_limits")
                           )),
                    column(12,
                           fluidRow(
                             class="well2",
                             uiOutput("show_map_coords_stack")
                           )),
                    uiOutput("show_map_labels_stack"),
                    div(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Click to order the stack layers", placement = "bottom", options=list(container="body")), actionLink("order_stack","+ Order stack")),
                    column(12,
                           fluidRow(class="well2",
                                    div("+ Legend adjustment",
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Legend width", placement = "bottom", options=list(container="body")),
                                               '+ Width:',inline(numericInput("stack_width.legend",NULL, 40, width="75px",step=1))),
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Legend y position", placement = "bottom", options=list(container="body")),
                                               '+ Y pos:',inline(numericInput("stack_legadj",NULL, 0, width="75px",step=.1))),
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Legend bar height", placement = "bottom", options=list(container="body")),
                                               '+ Bar height:',inline(numericInput("stack_legbar.h",NULL, 0.2, width="75px",step=.1))),
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Adjustment of the titles along the y-axis:", placement = "bottom", options=list(container="body")),
                                               '+ Title pos:',inline(numericInput("stack_legtitle.pos",NULL, 0.3, width="75px",step=.1))),
                                        column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Adjustment of the labels along the y-axis:", placement = "bottom", options=list(container="body")),
                                               '+ Label pos:',inline(numericInput("stack_leglab.pos",NULL, 0.3, width="75px",step=.1)))

                                    ))
                    ),


           )
    )
  })
  output$sr_3dcontrol<-renderUI({
    req(isTRUE(input$stack_map))

    tips<-list(
      tiphelp("a expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction","left"),
      tiphelp("azimuthal direction","left"),
      tiphelp("colatitude direction","left"),
      tiphelp("rhe distance of the eyepoint from the centre of the plotting box","left"),
      tiphelp("a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it","left"),
      tiphelp("Transparency of the layers","left")
    )

    div(

      div(class="well2",style="min-width: 250px",
          "+ 3D control",
          div(
            numericInput("stack_exp",span(tips[[1]],'+ exp:'), 1,  step=.1),
            numericInput("stack_theta",span(tips[[2]],'+ theta:',img(src=icon_360_hor,height='12',width='12')), 0, step=10),
            numericInput("stack_phi",span(tips[[3]],'+ phi:', img(src=icon_360_ver,height='12',width='12')), 40,  step=5),
            numericInput("stack_eye",span(tips[[4]],'+ Eye point:'), 1.73),
            numericInput("stack_d",span(tips[[5]],'+ persp strength:'), 1),
            numericInput("stack_alpha",span(tips[[6]],'+ Transparency:'), 0, step=1, max=1, min=0)
          ))
    )
  })
  observeEvent(ignoreInit = T,input$sr_zlab,vals$sr_zlab<-input$sr_zlab)
  observeEvent(ignoreInit = T,input$sr_ticktype,vals$sr_ticktype<-input$sr_ticktype)










  output$sr_label_options<-renderUI({
    req(isTRUE(input$stack_map))
    tips<-list(
      tiphelp("simple- draws just an arrow parallel to the axis to indicate direction of increase; detailed- draws normal ticks as per 2D plots","left"),
      tiphelp("X axis label","left"),
      tiphelp("Y axis label","left"),
      tiphelp("Z axis label","left"),
      tiphelp("the (approximate) number of tick marks to draw on the axes","left"),
      tiphelp("Show axes","left"),
      tiphelp("Show borders","left"),
      tiphelp("x-adjustment of key-labels","left"),
      tiphelp("z-adjustment of key-palettes","left"),
      tiphelp("z-adjustment of key-texts","left")

    )
    div(class="well2",style="min-width: 250px; padding-right: 10px",
        "+ Axis options",
        pickerInput("sr_ticktype",span(tips[[1]],"+ Ticktype:"),choices=c("detailed", "simple")),
        textInput("sr_xlab",span(tips[[2]],"+ xlab:"),"Longitude"),
        textInput("sr_ylab",span(tips[[3]],"+ ylab:"),"Latitude"),

        textInput("sr_zlab",span(tips[[4]],"+ zlab:")),
        numericInput("sr_nticks",span(tips[[5]],"+ sr_nticks:"), value=5, step=1),
        checkboxInput("box_all",span(tips[[6]],"+ Show axes:"),T),
        checkboxInput("box_border",span(tips[[7]],"+ Show border:"),F),
        numericInput("sr_lab_xadj",span(tips[[8]],"+ key-lab-adj-x:"), value=0, step=0.01),
        numericInput("sr_legpal_zajd",span(tips[[9]],"+ key-palette-adj-z"), value=0.03, step=0.01),
        numericInput("sr_labaxis_zadj",span(tips[[10]],"+  key-axis-text-adj-z"),value=-.1, step=0.01)
    )
  })


  output$sr_limits<-renderUI({
    base_shape=attr(getdata_map(),"base_shape")
    layer_shape=attr(getdata_map(),"layer_shape")
    limits<-get_limits(limits=NULL,base_shape, layer_shape, coords = attr(getdata_map(),"coords"))

    div(div(span("+ Limits:",style="color: #05668D"),
            div(style="margin-top: -20px;border-bottom: 1px solid SeaGreen",
                div(span("Long",style="margin-left: 70px"),span("Lat",style="margin-left: 60px")),
                div(
                  splitLayout("+ min:",cellWidths = c("50px",'150px',"150px"),
                              numericInput("srlong_xmin",NULL, value=limits[1,1], width="150px", step=0.01),
                              numericInput("srlat_xmin",NULL, limits[1,2], width="150px", step=0.01))),
                div(
                  splitLayout("+ max:",cellWidths = c("50px",'150px',"150px"),
                              numericInput("srlong_xmax",NULL, value=limits[2,1], width="150px", step=0.01),
                              numericInput("srlat_xmax",NULL, limits[2,2], width="150px", step=0.01)))


            )))
  })
  output$srmap_layer<-renderUI({
    req(length(attr(getdata_map(),"layer_shape"))>0)

    column(12,
           fluidRow(
             class="well2",
             span(
               "+",
               checkboxInput("srmap_layer","Layer Shape", T, width="120px"),
               div(class="palette",
                   span("+ Color:",
                        inline(tags$div(class = "ident-picker",


                                        pickerInput(inputId = "srlayer_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="80px", selected="gray")))
                   ),
                   span("+ Transp:",
                        numericInput("srlayer_lighten",NULL,value=0.6, width="50px", min=0, max=1, step=0.1)
                   )
               )


             )
           ))
  })
  output$stack_zlim<-renderUI({
    zlen<-length(names(get_stacklist()))


    div(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Z limits", placement = "bottom", options=list(container="body")),
        span("+ zmin:",
             inline(numericInput("stack_zmin",NULL, 1, width="80px",step=1))
        ),
        span("zmax:",
             inline(numericInput("stack_zmax",NULL, zlen*1.15, width="80px",step=1))
        ))
  })
  output$stack_raster<-renderUI({
    res<-list()
    imax=3
    stacklist<-get_stacklist()
    imax<-length(names(stacklist))



    for( i in 1:imax){
      res[[i]]<-div(width="250px",
                    names(get_stacklist())[i],
                    div(
                      span("+ z-value",
                           numericInput(paste0("stack_z",i),NULL, i, width="200px",step=0.1)
                      )
                    ),

                    div(
                      span("+ Key-text",
                           tags$div(style="width: 200px; display: inline-block",
                                    textInput(paste0("stack_text",i),NULL, names(get_stacklist())[i])
                           )
                      )
                    ),
                    div(span("+ key-text-adj-x:",
                             inline(numericInput(paste0("stack_key_adjx_lab",i),NULL, value=0.2, width="100px", step=0.01)))),
                    div(
                      span("+ key-palette-adj-x",
                           inline(numericInput(paste0("stack_key_adjz_pal",i),NULL, value=0.03, width="100px", step=0.01)))
                    ),
                    div(
                      span("+  key-values-adj-x",
                           inline(numericInput(paste0("stack_key_adjz_text",i),NULL, value=0.03, width="100px", step=0.01)))
                    ),


                    div(
                      span("+",
                           inline(checkboxInput(paste0("stack_co",i),"coords", F, width="55px"))
                      )
                    ),
                    div(
                      span("+",
                           inline(checkboxInput(paste0("stack_lab",i),"labels", F, width="55px")),
                           inline(
                             conditionalPanel(paste0("input.",paste0("stack_lab",i)," % 2"),{
                               pickerInput(paste0("stack_labels",i),NULL, choices=colnames( attr(getdata_map(),"factors")),options=list(container="body"), width="75px", inline=T)
                             })
                           )
                      )
                    )
      )
    }

    checkboxGroupInput("stack_layers",NULL, choiceValues =names(get_stacklist()),choiceNames=res,selected=names(get_stacklist()), width="75px")
  })
  output$show_map_coords_stack<-renderUI({
    co<-c()
    for(i in 1:length(names(get_stacklist())))
    {co[i]<-input[[paste0("stack_co",i)]]}
    req(any(co))
    div("+ Coords style",
        column(12,class="palette",' + Color:',
               pickerInput(inputId = "col_coords_stack",
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="120px")),
        column(12,
               '+ Size:',
               inline(numericInput("pt_coords_stack",NULL, 1, width="100px", step=0.1))
        )
    )
  })
  output$show_map_labels_stack<-renderUI({
    showlab<-c()
    for(i in 1:length(names(get_stacklist())))
    {showlab[i]<-input[[paste0("stack_lab",i)]]}

    req(any(showlab))
    column(12,
           fluidRow(
             class="well2",
             div(' + Labels style:',
                 column(12,class="palette",' + Color:',
                        pickerInput(inputId = "col_factor_stack",
                                    label = NULL,
                                    choices =   vals$colors_img$val[getsolid_col()],
                                    choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
                 column(12,
                        '+ Size:',
                        inline(numericInput("pt_factor_stack",NULL, 1, width="100px", step=0.1))
                 )
             )
           ))


  })

  output$side_map00<-renderUI({

    req(input$saved_maps=="new map"|isTRUE(input$surface_map)|isTRUE(input$mantel_map)|isTRUE(input$edit_map))
    column(4,id="sidebar_map",class="well2", style="margin-bottom: 150px",


           uiOutput("cmap"),
           uiOutput("side_map"),
           uiOutput("side_disc_stack")
    )
  })
  output$side_map<-renderUI({
    if(length(input$stack_scatter_3d)>0){
      req(isFALSE(input$stack_scatter_3d))}
    fluidRow(id="side_map",class="map_control_style",
             fluidRow(
               uiOutput("map_00_palette")),
             uiOutput("map_side01"),
             uiOutput("map_side02"),
             div(style="margin-top: 5px",uiOutput("map_title_out")),
             uiOutput('map_axes'),
             uiOutput("map_side04"),
             uiOutput("map_side05")
    )
  })
  output$side_disc_stack<-renderUI({
    req(isTRUE(input$stack_scatter_3d))

    div(class="map_control_style",

        column(12,
               div(class="well2",
                   actionLink('smd_layers',"+ Layers"),
                   div(id='smd_layers_id',uiOutput('added_scatter_stack'))
               )),

        uiOutput('ss_war'),
        uiOutput("ssmap_control")

    )
  })

  observeEvent(ignoreInit = T,input$smd_layers,{
    if(input$smd_layers%% 2){
      shinyjs::show('smd_layers_id')
      updateActionLink(session,'smd_layers',"- Layers")
    } else{
      shinyjs::hide('smd_layers_id')
      updateActionLink(session,'smd_layers',"+ Layers")
    }

  })

  observeEvent(ignoreInit = T,input$smd_3dcontrol,{
    if(input$smd_3dcontrol%% 2){
      shinyjs::show('smd_3dcontrol_id')
      updateActionLink(session,'smd_3dcontrol',"- 3D control")
    } else{
      shinyjs::hide('smd_3dcontrol_id')
      updateActionLink(session,'smd_3dcontrol',"+ 3D control")
    }

  })


  output$ssmap_control<-renderUI({
    req(
      length(vals$ss_map)>1
    )
    base_shape=attr(getdata_map(),"base_shape")
    layer_shape=attr(getdata_map(),"layer_shape")
    limits<-get_limits(limits=NULL,base_shape, layer_shape, coords = attr(getdata_map(),"coords"))

    div(

      div(
        actionLink('smd_3dcontrol',"+ 3D control"),
        column(12,id='smd_3dcontrol_id',uiOutput('ss_3dcontrol'))),

      column(12,
             fluidRow(
               class="well2",
               uiOutput("show_map_coords_ss")
             )),
      column(12,
             fluidRow(
               class="well2",
               uiOutput("show_map_labels_ss")
             )),
      column(12,
             fluidRow(class="well2",
                      uiOutput("ssmap_zlim")
             )),
      column(12,
             fluidRow(class="well2",
                      uiOutput("ssmap_leg")
             )),

      column(12,
             fluidRow(class="well2",
                      uiOutput("ssmap_base")
             )),



      column(12,
             fluidRow(class="well2",
                      uiOutput("ssmap_layer")
             )),


      column(12,
             fluidRow(class="well2",
                      div(span("+ Limits:",style="color: #05668D"),
                          div(style="margin-top: -20px;",
                              div(span("Long",style="margin-left: 70px"),span("Lat",style="margin-left: 60px")),
                              div(
                                splitLayout("+ min:",cellWidths = c("50px",'90px',"90px"),
                                            numericInput("sslong_xmin",NULL, value=limits[1,1], width="87px", step=0.01),
                                            numericInput("sslat_xmin",NULL, limits[1,2], width="87px", step=0.01))),
                              div(
                                splitLayout("+ max:",cellWidths = c("50px",'90px',"90px"),
                                            numericInput("sslong_xmax",NULL, value=limits[2,1], width="87px", step=0.01),
                                            numericInput("sslat_xmax",NULL, limits[2,2], width="87px", step=0.01)))


                          ))
             ))

    )})

  observeEvent(ignoreInit = T,input$ss3d_exp,vals$ss3d_exp<-input$ss3d_exp)
  observeEvent(ignoreInit = T,input$ss3d_theta,vals$ss3d_theta<-input$ss3d_theta)
  observeEvent(ignoreInit = T,input$ss3d_phi,vals$ss3d_phi<-input$ss3d_phi)
  observeEvent(ignoreInit = T,input$ss3d_eye,vals$ss3d_eye<-input$ss3d_eye)
  observeEvent(ignoreInit = T,input$ss3d_d,vals$ss3d_d<-input$ss3d_d)

  output$ss_3dcontrol<-renderUI({

    if(is.null(vals$ss3d_exp)){ vals$ss3d_exp<-1}
    if(is.null(vals$ss3d_theta)){vals$ss3d_theta<-0}
    if(is.null(vals$ss3d_phi)){ vals$ss3d_phi<-40}
    if(is.null(vals$ss3d_eye)){vals$ss3d_eye<-1.73}
    if(is.null(vals$ss3d_d)){vals$ss3d_d<-1}

    column(12,id="smr_3dcontrol",
           fluidRow(class="well2",
                    "+ 3D control",
                    column(12,
                           div(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"a expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction", placement = "bottom", options=list(container="body")),
                               '+ exp:',
                               inline(numericInput("ss3d_exp",NULL, vals$ss3d_exp, width="75px", step=.1))
                           ),
                           div(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"azimuthal direction", placement = "bottom", options=list(container="body")),
                               '+ theta:',
                               inline(numericInput("ss3d_theta",NULL, vals$ss3d_theta, width="75px",step=10))
                           ),
                           div(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"colatitude direction", placement = "bottom", options=list(container="body")),
                               '+ phi:',
                               inline(numericInput("ss3d_phi",NULL, vals$ss3d_phi, width="75px", step=5))
                           ),
                           div(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"rhe distance of the eyepoint from the centre of the plotting box", placement = "bottom", options=list(container="body")),
                               '+ Eye point:',
                               inline(numericInput("ss3d_eye",NULL, vals$ss3d_eye, width="75px"))
                           ),
                           div(
                             tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it", placement = "bottom", options=list(container="body")),
                             '+ persp strength:',
                             inline(numericInput("ss3d_d",NULL, vals$ss3d_d, width="50px"))
                           )
                    )))
  })
  output$ssmap_leg<-renderUI({
    fluidRow(
      column(12,"+ Legend adustment",
             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Number of breaks for continuous values", placement = "bottom", options=list(container="body")),
                    '+ Breaks:',inline(numericInput("ss_breaks",NULL, 4, width="75px",step=1))),

             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Adjustment of the titles along the y-axis:", placement = "bottom", options=list(container="body")),
                    '+ Title y-pos:',inline(numericInput("ss_legtitle.posy",NULL, 1.15, width="75px",step=.1))),
             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Adjustment of the titles along the x-axis:", placement = "bottom", options=list(container="body")),
                    '+ Title x-pos:',inline(numericInput("ss_legtitle.posx",NULL, 1, width="75px",step=.1))),


             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Adjustment of the labels along the y-axis:", placement = "bottom", options=list(container="body")),
                    '+ Label pos:',inline(numericInput("ss_leglab.pos",NULL, 0.85, width="75px",step=.1)))

      ))
  })


  output$ssmap_zlim<-renderUI({
    zs<-c()
    for(i in 1:length(vals$ss_map))
    {
      zs[i]<-input[[paste0("ss_z",i)]]

    }

    fluidRow(
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Z limits", placement = "bottom", options=list(container="body")),
             span("+ zmin:",
                  inline(numericInput("ss_zmin",NULL, min(zs), width="80px",step=1))
             ),
             span("zmax:",
                  inline(numericInput("ss_zmax",NULL, value=max(zs)*1.5, width="80px", step=0.01))
             )),
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"'simple' draws just an arrow parallel to the axis to indicate direction of increase; 'detailed' draws normal ticks as per 2D plots", placement = "bottom", options=list(container="body")),
             span("+ Ticktype:",
                  pickerInput("ss_ticktype",NULL,choices=c("detailed", "simple"),inline=T, width="75px"))
      ),
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"X axis label", placement = "bottom", options=list(container="body")),
             span("+ xlab:",
                  textInput("ss_xlab",NULL,"Longitude", width="100px"))
      ),
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Y axis label", placement = "bottom", options=list(container="body")),
             span("+ ylab:",
                  textInput("ss_ylab",NULL,"Latitude", width="100px"))
      ),
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Z axis label", placement = "bottom", options=list(container="body")),
             span("+ zlab:",
                  textInput("ss_zlab",NULL,"Layer", width="100px"))
      )
    )
  })
  output$show_map_coords_ss<-renderUI({
    co<-c()
    for(i in 1:length(vals$ss_map))
    {co[i]<-input[[paste0("ss_co",i)]]}
    req(any(co))
    div("+ Coords style",
        column(12,class="palette",' + Color:',
               pickerInput(inputId = "col_coords_ss",
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="100px")),
        column(12,
               '+ Size:',
               inline(numericInput("pt_coords_ss",NULL, 1, width="100px", step=0.1))
        )
    )
  })
  output$show_map_labels_ss<-renderUI({
    showlab<-c()
    for(i in 1:length(vals$ss_map))
    {showlab[i]<-input[[paste0("ss_lab",i)]]}

    req(any(showlab))
    column(12,
           fluidRow(
             class="well2",
             div(' + Labels style:',
                 column(12,class="palette",' + Color:',
                        pickerInput(inputId = "col_factor_ss",
                                    label = NULL,
                                    choices =   vals$colors_img$val[getsolid_col()],
                                    choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
                 column(12,
                        '+ Size:',
                        inline(numericInput("pt_factor_ss",NULL, 1, width="100px", step=0.1))
                 )
             )
           ))


  })

  output$added_scatter_stack<-renderUI({
    req(!is.null( vals$ss_map))
    lapply(vals$ss_map, function(x){
      i=which(names(vals$ss_map)%in%attr(x,"name"))
      name<-attr(x,"name")
      observeEvent(ignoreInit = T,input[[paste0("ss_del",name)]],{
        vals$ss_map[name]<-NULL
      })




    })
    div(
      added_ss3d$df)
  })

  observe({
    vals$ss_names<-unlist(lapply(1:length(vals$ss_map), function(x) input[[paste0("ss_name",x)]]))
  })

  observeEvent(vals$ss_map, {
    delay(10,
          added_ss3d$df<- added_ss3d$df[names(vals$ss_map)]

    )
  })

  bag_name_ss<-reactive({
    bag<-1
    name0<-paste(input$var_map,input$var_map_filter2)
    name1<-paste(name0,bag)
    if(name1%in%names(vals$ss_map))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$ss_map)) break
      }

    }
    paste(name0,bag)
  })
  ss3d_add<-reactive({
    if(input$choices_map=="Numeric-Attribute"){
      ss<-getdata_map()[filtermap(),input$var_map, drop=F]
    } else{
      ss<-attr(getdata_map(),"factors")[filtermap(),input$var_map, drop=F]
    }



    coords<-attr(getdata_map(),"coords")
    attr(ss,"name")<-bag_name_ss()
    vals$ss_map[[bag_name_ss()]]<-ss
    attr(vals$ss_map,"coords")<-coords
    attr(vals$ss_map,"base_shape")<-attr(getdata_map(),"base_shape")
    attr(vals$ss_map,"layer_shape")<-attr(getdata_map(),"layer_shape")
  })
  observeEvent(ignoreInit = T,input$add_scatter_stack,{
    ss3d_add()

  } )
  observeEvent(ignoreInit = T,input$add_scatter_stack,{
    lapply(vals$ss_map,function(x){
      i=which(names(vals$ss_map)%in%attr(x,"name"))
      added_ss3d$df[[attr(x,"name")]]<-isolate(list(
        div(

          span("+",inline( textInput(paste0("ss_name",i),NULL,names(vals$ss_map)[i], width='150px' ))),
          column(12,class="palette",span("+ Palette:",
                                         pickerInput(inputId = paste0("pt_palette",i),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),inline=T, width="55px")
          )),
          column(12,span("+ Size:",
                         numericInput(paste0("ss_cex",i),NULL, 1, width="100px",step=0.1)
          )),
          column(12,span("+ Z-value:",
                         numericInput(paste0("ss_z",i),NULL, i, width="100px",step=0.1)
          )),
          column(12,
                 span("+",
                      inline(checkboxInput(paste0("ss_co",i),"coords", T, width="100px"))
                 )
          ),
          column(12,
                 span("+",
                      inline(checkboxInput(paste0("ss_lab",i),"labels", F, width="60px")),
                      inline(
                        conditionalPanel(paste0("input.",paste0("ss_lab",i)," % 2"),{
                          pickerInput(paste0("ss_labels",i),NULL, choices=colnames( attr(getdata_map(),"factors")),options=list(container="body"), width="75px", inline=T)
                        })
                      )
                 )),

          actionLink(paste0("ss_del",attr(x,"name")),icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"))

        )
      ))
    }

    )
  })
  output$ssmap_base<-renderUI({

    req(!is.null(attr(getdata_map(),"base_shape")))
    column(12,
           fluidRow(
             class="well2",
             span(
               "+",
               checkboxInput("ssmap_base","Base Shape", T, width="120px"),
               div(class="palette",
                   span("+ Color:",
                        inline(tags$div(class = "ident-picker", pickerInput(inputId = "ssbase_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="80px", selected="white")))
                   ),
                   span("+ Transp:",
                        numericInput("ssbase_lighten",NULL,value=0.6, width="50px", min=0, max=1, step=0.1)
                   )
               )
             )
           ))
  })
  output$map_extra_layer<-renderUI({
    req(input$data_map)
    shp<-attr(vals$saved_data[[input$data_map]],"extra_shape")
    shapes<-names(shp)

    req(length(shapes)>0)

    res<-list()


    for(i in 1:length(shapes)){
      atributos_shp<-attributes(shp[[i]])$names

      res[[i]]<-list(
        span(
          span(
            "+",
            checkboxInput(paste0("map_extra_layer",i),paste("Extra-Layer",i), T, width="150px")
          ),
          div(class="palette",
              span("+ Color:",
                   inline(tags$div(
                     class = "ident-picker",
                     pickerInput(inputId = paste0("ssextra_layer_col",i),label = NULL,choices =   vals$colors_img$val,choicesOpt = list(content =   vals$colors_img$img),options=list(container="body"),inline=F, width="100px", selected="gray")
                   ))
              ),
              span("+ Transp:",
                   numericInput(paste0("ssextra_layer_lighten",i),NULL,value=0.6, width="100px", min=0, max=1, step=0.1)
              )


          ),
          div(
            span("+", paste("Label",i), inline(
              pickerInput(paste0("feat_extra",i),NULL,choices=c("None",atributos_shp),inline=T, width="150px")
            ) ),
            span("+ size",numericInput(paste0("feat_extra_size",i),NULL,value=2, width="100px", min=0, max=1, step=0.1))
          )
        )
      )
    }

    column(12,
           fluidRow(
             class="well2",
             res,
             inline(uiOutput("data_depth")),
             div(
               span("+ Remove layer", inline(
                 pickerInput("remove_extra",NULL,choices=c(shapes),inline=T, width="150px")),
                 actionLink("del_extra",icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt")))
             )
           ))
  })
  output$data_depth<-renderUI({
    req(input$cmap=="discrete")
    shp<-attr(vals$saved_data[[input$data_map]],"extra_shape")
    span("+ Layer depth ",tiphelp("Controls the depth of the data in relation to the base shape, layer shape, and extra layers. For example, if Data depth =3, then data will be plotted bellow extra shapes."),
         numericInput("data_depth",NULL,value=3+(length(shp)*2), width="50px", min=1, step=1)
    )
  })
  observeEvent(ignoreInit = T,input$del_extra,{
    attr(vals$saved_data[[input$data_map]],"extra_shape")[[input$remove_extra]]<-NULL
  })
  output$ssmap_layer<-renderUI({
    req(length(attr(getdata_map(),"layer_shape"))>0)
    if(is.null(vals$sslayer_lighten)){vals$sslayer_lighten<-0.6}
    if(is.null(vals$ssmap_layer)){vals$ssmap_layer<-'gray'}
    column(12,
           fluidRow(
             class="well2",
             span(
               "+",
               checkboxInput("ssmap_layer","Layer Shape", T, width="120px"),
               div(class="palette",
                   span("+ Color:",
                        inline(tags$div(class = "ident-picker", pickerInput(inputId = "sslayer_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="80px", selected=vals$sslayer_col)))
                   ),
                   span("+ Transp:",
                        numericInput("sslayer_lighten",NULL,value=vals$sslayer_lighten, width="80px", min=0, max=1, step=0.1)
                   )
               )


             )
           ))
  })
  observeEvent(ignoreInit = T,input$sslayer_lighten,vals$sslayer_lighten<-input$sslayer_lighten)
  observeEvent(ignoreInit = T,input$sslayer_col,vals$sslayer_col<-input$sslayer_col)
  output$ss3d<-renderUI({
    req(input$ss3d_d)
    validate(need(input$ss3d_d>0, "error: requires persp strength >0 "))
    req(isTRUE(input$stack_scatter_3d))
    validate(need(length(vals$ss_map)>1,"Add at least two variables to generate the plot"))


    data=vals$ss_map
    coords=  na.omit(attr(vals$ss_map,"coords"))
    base_shape=if(isTRUE(input$ssmap_base)){
      attr(data,"base_shape") } else{ NULL}
    layer_shape=if(isTRUE(input$ssmap_layer)){
      attr(data,"layer_shape") } else{ NULL}
    sscol_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$sslayer_col,1), input$sslayer_lighten)
    sscol_base=adjustcolor(getcolhabs(vals$newcolhabs,input$ssbase_col,1), input$ssbase_lighten)

    pal<-c()
    cex<-c()
    zs<-c()
    co<-c()
    labs<-list()
    showlab<-c()
    df<-getdata_map()
    for(i in 1:length(vals$ss_map))
    {


      pal[i]<-input[[paste0("pt_palette",i)]]
      cex[i]<-input[[paste0("ss_cex",i)]]
      zs[i]<-input[[paste0("ss_z",i)]]
      labs[[i]]<-attr(df,"factors")[rownames(coords), input[[paste0("ss_labels",i)]]]
      co[i]<-input[[paste0("ss_co",i)]]
      showlab[i]<-input[[paste0("ss_lab",i)]]
    }


    #data<-do.call(cbind,data)
    div(


      renderPlot({
        p<-stack_scatter3D(data=vals$ss_map,coords=coords,base_shape=base_shape,layer_shape=layer_shape,
                           col.palette=pal,
                           newcolhabs=vals$newcolhabs,
                           pt_cex=cex,
                           spacing=1,
                           expand=input$ss3d_exp,
                           theta=input$ss3d_theta,
                           phi=input$ss3d_phi,
                           r=input$ss3d_eye,
                           d=input$ss3d_d,
                           xlim=c(input$sslong_xmin,input$sslong_xmax),
                           ylim=c(input$sslat_xmin,input$sslat_xmax),
                           col_base=sscol_base,
                           col_layer=sscol_layer,
                           z=zs,
                           zmin=input$ss_zmin,
                           zmax=input$ss_zmax,
                           ticktype=input$ss_ticktype,
                           xlab=input$ss_xlab,
                           ylab=input$ss_ylab,
                           zlab=input$ss_zlab,
                           leglab.adj=input$ss_leglab.pos,
                           legtit.posy=input$ss_legtitle.posy,
                           breaks=input$ss_breaks+1,
                           legwidth=50,
                           legtit.posx=input$ss_legtitle.posx,
                           title.srt=0,
                           lab.srt=0,
                           col.labels=input$col_factor_ss,
                           cex.labels=input$pt_factor_ss,
                           labels=labs,
                           col.coords=input$col_coords_ss,
                           cex.coords=input$pt_coords_ss,
                           show_coords=co,
                           show_labels=showlab,
                           custom_legend=vals$ss_names)
        vals$map_res<-p
        p
      })
    )

  })
  output$cmap<-renderUI({
    req(!isTRUE(input$mantel_map))
    req(!isTRUE(input$surface_map))
    if(is.null(vals$cur_cmap)){vals$cur_cmap="discrete"}
    fluidRow(
      radioGroupButtons(
        "cmap", choiceNames = list(
          tipify(span(icon(verify_fa = FALSE,name=NULL,class="fas fa-map-marker-alt"),"Discrete"),"Discrete"),
          tipify(span(icon(verify_fa = FALSE,name=NULL,class="fas fa-route"),"Interpolation"),"Interpolation"),
          tipify(span(icon(verify_fa = FALSE,name=NULL,class="fas fa-th"),"Raster"), "Create a Raster using the target data and its coordinates without model")
        ),choiceValues =list("discrete","interpolation","raster"), status="button_active",justified =T, selected=vals$cur_cmap)
    )
  })
  output$map_00_palette<-renderUI({
    req(!isTRUE(input$mantel_map))
    div(class="palette",
        div(
          class="well2",style="padding-top:1px;
           padding-bottom:2px;
          ",
          span("+ Data palette:"),pickerInput(inputId = "pt_palette",label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),inline=T, selected=vals$pt_palette,width="120px"))
    )
  })

  output$map_side01<-renderUI({
    req(!isTRUE(input$surface_map))
    req(!isTRUE(input$mantel_map))
    fluidRow(id="map_side01",


             uiOutput("map_1b_discrete"),
             column(12,uiOutput("scatter_colorby")),
             conditionalPanel("input.cmap=='interpolation'",{
               column(12,
                      div(class="well2",
                          fluidRow(
                            column(12,"+ Model"),
                            column(12,

                                   uiOutput("map_1c_grid"),
                                   uiOutput("map_1d_idw"),
                                   uiOutput("map_1d_knn")
                            )
                          )
                      ))
             }),
             column(12, uiOutput('map_coords')),
             column(12,uiOutput('map_labels')),
             uiOutput("map_1a_base"),
             uiOutput("map_1f_layer"),
             column(12,uiOutput("map_extra_layer")),
             uiOutput("map_1e_limits")



    )
  })
  observeEvent(ignoreInit = T,input$cmap,{
    if(input$cmap=="interpolation"|input$cmap=="raster"){
      updateButton(session,'scatter_3d', value=F)
      updateButton(session,'stack_scatter_3d', value=F)
    }
  })
  output$map_side02<-renderUI({
    req(isTRUE(input$surface_map)|isTRUE(input$scatter_3d))
    req(!isTRUE(input$mantel_map))
    if(input$cmap=="interpolation"|input$cmap=="raster"){
      req(input$saved_maps!="new map")} else{
        req(isTRUE(input$scatter_3d))
      }
    fluidRow(
      column(12,
             span("+ z-colors:",style="vertical-align: top;",
                  uiOutput("show_z_colors", inline = T)),

             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"a expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction", placement = "bottom", options=list(container="body")),
                    '+ exp:',
                    inline(numericInput("surf_exp",NULL, 0.3, width="75px", step=.1))
             ),
             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"azimuthal direction", placement = "bottom", options=list(container="body")),
                    '+ theta:',
                    inline(numericInput("surf_theta",NULL, 0, width="75px",step=10))
             ),
             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"colatitude direction", placement = "bottom", options=list(container="body")),
                    '+ phi:',
                    inline(numericInput("surf_phi",NULL, 40, width="75px", step=5))
             ),
             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"rhe distance of the eyepoint from the centre of the plotting box", placement = "bottom", options=list(container="body")),
                    '+ Eye point:',
                    inline(numericInput("surf_r",NULL, 1.73, width="75px"))
             ),
             column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"simple - draws just an arrow parallel to the axis to indicate direction of increase; detailed - draws normal ticks as per 2D plots.", placement = "bottom", options=list(container="body")),
                    '+ ticktype',
                    inline(pickerInput("surf_tick",NULL, c("simple","detailed"), width="100px"))
             ),


             column(12,
                    tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it", placement = "bottom", options=list(container="body")),
                    '+ persp strength:',
                    inline(numericInput("surf_d",NULL, 1, width="50px"))
             ),
             uiOutput("leg_surface")


      )
    )


  })
  output$leg_surface<-renderUI({
    req(isTRUE(input$surface_map))
    div(
      column(12,
             '+ Leg width:',
             inline(numericInput("surface_width.legend",NULL, 40, width="75px",step=1))),
      column(12,
             '+ Leg height:',
             inline(numericInput("surface_height.legend",NULL, 20, width="75px",step=1)))
    )
  })
  output$map_labels<-renderUI({
    req(!isTRUE(input$mantel_map))
    div(class="well2",
        fluidRow(
          column(12,

                 div("+",checkboxInput("showfactors","Labels", vals$showfactors, width="75px")),uiOutput("show_map_labels")
          )
        )
    )

  })
  output$map_coords<-renderUI({
    req(!isTRUE(input$mantel_map))
    div(class="well2",
        fluidRow(
          column(12,

                 div("+",checkboxInput("showcoords","Coords", vals$showcoords, width="75px")),uiOutput("show_map_coords")
          )
        )
    )
  })
  output$map_axes<-  renderUI({
    div(class="well2",
        fluidRow(
          column(12,span("+",checkboxInput("showguides","Guides", vals$showguides, width="75px"))),

          uiOutput("show_guides_options"),

          uiOutput("show_map_axes")
        )
    )
  })
  lines_vals<-c("solid","dashed","dotted")
  dflines<-data.frame(vals=1:3)
  for(i in  1:length(lines_vals))  {
    dflines$img[i]<- sprintf(paste(
      div(style=paste('border-top: 1px',lines_vals[i],'SeaGreen;',"padding: 2px; color: black"))))}
  output$show_guides_options<-renderUI({
    req(isTRUE(input$surface_map))
    column(12,
           column(12,class="palette",' + Color:',
                  pickerInput(inputId = "col_guides",
                              label = NULL,
                              choices =   vals$colors_img$val[getsolid_col()],selected=vals$col_guides,
                              choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="120px")),
           column(12,
                  '+ line:',
                  inline(
                    pickerInput("lty_guides",NULL, choices = dflines$val,selected=vals$lty_guides,
                                choicesOpt=list(content = dflines$img), width="75px",
                    )
                  )
           ),
           column(12,
                  '+ Width:',
                  inline(numericInput("lwd_guides",NULL, vals$lwd_guides, width="75px",step=0.5))
           )

    )
  })
  observeEvent(ignoreInit = T,input$pt_titlemap,{
    vals$pt_titlemap<-input$pt_titlemap
  })
  observeEvent(ignoreInit = T,input$cex.key,{
    vals$cex.key<-input$cex.key
  })
  observeEvent(ignoreInit = T,input$key.height,{
    vals$key.height<-input$key.height
  })
  observeEvent(ignoreInit = T,input$keyscale,{
    vals$keyscale<-input$keyscale
  })

  observeEvent(ignoreInit = T,input$scabarsize,{
    vals$scabarsize<-input$scabarsize
  })
  observeEvent(ignoreInit = T,input$scabartextsize,{
    vals$scabartextsize<-input$scabartextsize
  })

  observe({
    req(is.null(vals$cex.key))
    vals$cex.key<-12
    vals$key.height<-12
    vals$keyscale<-30
    vals$scabarsize<-0.2
    vals$scabartextsize<-1
    vals$pt_titlemap<-12
  }, autoDestroy = T)




  output$out_pt_legend<-renderUI({
    column(12,
           '+ Size:',
           inline(numericInput("pt_legend", NULL, vals$pt_legend, width="100px"))
    )
  })
  output$out_pt_titlemap<-renderUI({
    column(12,
           '+ Title size:',
           inline(numericInput("pt_titlemap", NULL, vals$pt_titlemap, width="100px"))
    )
  })
  output$out_map_legend<-renderUI({
    column(12,
           '+ label',
           inline(textInput("map_legend", NULL, input$var_map, placeholder="legend", width="150px"))
    )
  })
  output$out_cex_key<-renderUI({
    column(12,
           '+ Size:',
           inline(numericInput("cex.key", NULL, vals$cex.key, width="100px"))
    )
  })
  output$out_key_height<-renderUI({
    column(12,
           '+ key height:',
           inline(numericInput("key.height", NULL, vals$key.height, width="100px"))
    )
  })
  output$out_keyscale<-renderUI({
    column(12,
           '+ North size:',
           inline(numericInput("keyscale", NULL, vals$keyscale, width="100px"))
    )
  })
  output$out_scabarsize<-renderUI({
    column(12,
           '+ Scale bar size:',
           inline(numericInput("scabarsize", NULL, vals$scabarsize, width="100px"))
    )
  })
  output$out_scabartextsize<-renderUI({
    column(12,
           '+ Scale bar text size:',
           inline(numericInput("scabartextsize", NULL, vals$scabartextsize, width="100px"))
    )
  })
  output$show_map_axes<-renderUI({
    req(input$var_map)
    req(input$var_map_filter1)
    div(
      column(12, ' + Axes:', uiOutput("out_pt_legend")),
      column(12, uiOutput("out_pt_titlemap")),
      column(12, ' + Legend:',
             div(style="padding-left: 15px",
                 uiOutput("out_map_legend"),
                 uiOutput("out_cex_key"),
                 uiOutput("out_key_height"),
                 uiOutput("out_keyscale"),
                 uiOutput("out_scabarsize"),
                 uiOutput("out_scabartextsize"),
                 column(12, uiOutput("breaks_map"))
             )
      )
    )
  })



  getmap_title<-reactive({
    req(input$var_map)
    req(input$var_map_filter1)
    res<-paste0(input$var_map,if(input$var_map_filter1!="None"){
      req(input$var_map_filter2)
      paste("-",input$var_map_filter1,input$var_map_filter2)
    })
    res
  })

  output$map_title_out<-renderUI({
    vals$map_title<-NULL
    title<-getmap_title()
    column(12,' +	Title:',
           inline(textInput("map_title",NULL, title, placeholder = "Title", width="150px"))
    )
  })

  observeEvent(ignoreInit = T,input$var_map_filter2,{
    vals$map_title<-NULL
  })
  observeEvent(ignoreInit = T,input$var_map_filter1,{
    vals$map_title<-NULL
  })

  observeEvent(ignoreInit = T,input$map_title,{
    vals$map_title<-input$map_title
  })

  observeEvent(ignoreInit = T,input$var_map,{
    output$breaks_map<-renderUI({
      #req(input$cmap=="discrete")
      req(input$choices_map=="Numeric-Attribute")
      div(inline(uiOutput("breaks_map1")),
          inline(uiOutput("breaks_map2")))


    })
  })
  output$breaks_map2<-renderUI({

    req(input$cmap)
    req(length(input$breaks_map_length)>0)
    n<-input$breaks_map_length
    req(input$choices_map=="Numeric-Attribute")
    req(input$var_map)
    pic<-filtermap()
    data<-getdata_map()
    pic<-1:nrow(data)
    req(input$var_map%in%colnames(data))
    vector<-na.omit(data[,input$var_map])
    my<-pretty(vector)

    vector<-na.omit(data[pic,input$var_map])
    if(input$cmap=="discrete"){
      breaks<-create_pretty_breaks(vector, n_breaks = n)
    } else{
      vector<-seq(min(vector), max(vector),length.out=100)
      round<-decimalplaces(pretty(vector)[1])
      breaks<-round(quantile(vector,seq(0,1,length.out=n)),round)

      if( min(vector)<min(breaks)){
        repeat({
          round<-round+1
          breaks<-round(quantile(vector,seq(0,1,length.out=n)),round)
          if(!min(vector)<min(breaks)){
            break()
          }
        })
      }

      if( max(vector)>max(breaks)){
        repeat({
          round<-round+1
          breaks<-round(quantile(vector,seq(0,1,length.out=n)),round)
          if(!max(vector)>max(breaks)){
            break()
          }
        })
      }


    }




    div(
      tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Enter a vector of breaks (comma delimited, within the data range)", options=list(container="body")),
      "+ Breaks custom",
      textInput('breaks_map', NULL, paste(breaks, collapse = ", "), width="200px")
    )
  })
  output$breaks_map1<-renderUI({
    span("+ Breaks length",numericInput("breaks_map_length",NULL,5, step=1, width="100px"))
  })

  output$map_side05<-renderUI({
    req(isTRUE(input$mantel_map))
    column(12,class="map_control_style",
           p(strong("Mantel Correlogram"),tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Estimates the spatial dependence at discrete distance classes. The analysis uses progressive correction of multiple-testing, as described in Legendre and Legendre (1998, p. 721). The p-values are corrected by the method holm.", placement = "bottom", options=list(container="body"))),
           style="margin-left: 0px;",
           column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Number of classes. If n.class=0, the Sturges equation will be used.", placement = "bottom", options=list(container="body")),
                  ' + nclass:',
                  numericInput("mantel_nclass",label=NULL,value=0,width="75px")
           ),
           column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"For the second half of the distance classes, cutoff = TRUE limits the correlogram to the distance classes that include all points. If cutoff = FALSE, the correlogram includes all distance classes.", placement = "bottom", options=list(container="body")),
                  ' + cutoff:',
                  pickerInput("mantel_cutoff",label=NULL,choices=c("true", "false"),width="75px")
           ),
           column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Type of correlation in calculation of the Mantel statistic", options=list(container="body")),
                  ' + r.type:',
                  pickerInput("mantel_r.type",label=NULL,choices=c("pearson", "spearman","kendall"),width="75px")
           ),
           column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Number of permutations for the tests of significance. Default: nperm=999. For large data files, permutation tests are rather slow.", placement = "bottom", options=list(container="body")),
                  ' + nperm:',
                  numericInput("mantel_nperm",label=NULL,value=99,width="75px")
           ),
           column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"Significance level for the points drawn with black symbols in the correlogram", placement = "bottom", options=list(container="body")),
                  ' + alpha:',
                  numericInput("mantel_alpha",label=NULL,value=0.05,width="75px", min=0, max=1)
           ),
           inline(radioButtons("spatial_stats","Show", choiceValues =list("plot",'mantel.res','break.pts'),choiceNames =list(
             span("Plot"),
             tipify(span('Results'), "
A table with the distance classes as rows and the class indices, number of distances per class, Mantel statistics, and p-values as columns. A positive Mantel statistic indicates positive spatial correlation. ", placement = "right"),
tipify(span('Breakpoints'), "The break points computed", placement = "right")



           )))
    )


  })



  output$map_1a_base<-renderUI({
    if(is.null(vals$base_lighten)){vals$base_lighten<-0.6}
    if(is.null(vals$base_col)){vals$base_col<-"white"}
    if(is.null(vals$base_col_border)){vals$base_col_border<-"gray"}
    req(!is.null(attr(getdata_map(),"base_shape")))
    column(12,
           div(
             class="well2",
             span(
               "+",
               checkboxInput("map_1a_base","Base Shape", T, width="120px"),
               div(style="padding-left: 15px",
                   div(class="palette",
                       span("+ Color:",
                            inline(div(
                              class = "ident-picker",
                              pickerInput(inputId = "base_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="80px", selected=vals$base_col)))
                       ),
                       span("+ Transp:",
                            numericInput("base_lighten",NULL,value=vals$base_lighten, width="80px", min=0, max=1, step=0.1)
                       )
                   ),
                   div(
                     class="palette",
                     span("+ Border color:",
                          inline(div(
                            class = "ident-picker",
                            pickerInput(inputId = "base_col_border",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="80px", selected=vals$base_col_border)))
                     )
                   )

               )
             )
           ))
  })
  output$map_1b_discrete<-renderUI({

    req(input$cmap=="discrete")
    #req(isFALSE(input$surface_map)|is.null(input$surface_map))
    column(12,

           div(class="well2",
               span(" + Points:"),
               column(12,' + Shape:',
                      pickerInput(inputId = "pt_symbol",
                                  label = NULL,
                                  choices = df_symbol$val,
                                  selected=vals$pt_symbol,
                                  choicesOpt = list(content = df_symbol$img), inline=T, width="100px")),

               column(12,
                      '+ Size:',inline(uiOutput("map_pt_range"))),
               uiOutput("map_scalepoints"),

               uiOutput("map_pizza")

           )
    )


  })
  output$map_scalepoints<-renderUI({
    req(input$choices_map=='Numeric-Attribute' & isFALSE(input$scatter_3d))
    fluidRow(
      column(12,
             div(span("+",checkboxInput("scale_map","Scale", vals$scale_map, width="100px"))),
             div(style="padding-left: 15px",
                 uiOutput("scalesize_size"),
                 uiOutput("scalesize_color")
             )

      ),

      column(12,"+",checkboxInput("colored_map","Color by:", vals$colored_map, width="100px"),
             uiOutput("mapfac",inline=T))
    )
  })


  output$map_pizza<-renderUI({
    if(is.null(vals$pizza_map)){
      vals$pizza_map<-F
    }
    column(12,"+",checkboxInput("pizza_map","Pizza", vals$pizza_map, width="100px"),
           uiOutput("pizza_fac",inline=T))
  })

  observeEvent(ignoreInit = T,input$pizza_map,{
    vals$pizza_map<-input$pizza_map
  })

  output$pizza_fac<-renderUI({
    req(isTRUE(input$pizza_map))
    factors<-attr(vals$saved_data[[input$data_map]],"factors")
    choices<-colnames(factors)
    pickerInput("pizza_fac",NULL, choices=choices,options=list(container="body"), width="200px")
  })
  observeEvent(ignoreInit = T,input$pizza_fac,{
    vals$pizza_fac<-input$pizza_fac
  })


  output$map_pt_range<-renderUI({
    if(input$choices_map=="Numeric-Attribute"){
      div(
        "min:",
        inline(numericInput("pt_points_min",NULL,vals$pt_points_min, width="80px", min=0, step=0.1)),
        "max:",
        inline(numericInput("pt_points",NULL, vals$pt_points, width="80px"))

      )} else{
        inline(numericInput("pt_points",NULL, vals$pt_points, width="80px"))
      }
  })
  output$map_1c_grid<-renderUI({

    req(input$cmap=="interpolation")
    #req(isFALSE(input$surface_map)|is.null(input$surface_map))

    column(12," + Grid",
           column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray")," sample size for sampling point locations within the map area", placement = "bottom", options=list(container="body")),
                  ' + resolution:',
                  inline(uiOutput("res_map"))
           )


    )


  })

  output$res_map<-renderUI({
    numericInput(
      "res_map", NULL, res_map_cur$df, min = 1, width="75px"
    )
  })
  observeEvent(ignoreInit = T,input$res_map,{
    res_map_cur$df<-input$res_map
  })
  output$map_1d_idw<-renderUI({
    req(input$cmap=="interpolation")
    req(input$choices_map=="Numeric-Attribute")
    #req(isFALSE(input$surface_map)|is.null(input$surface_map))
    column(12," + IDW ",
           column(12, uiOutput("interp_inputs")))
  })


  output$map_1f_layer<-renderUI({
    req(!isTRUE(input$surface_map))
    req(!isTRUE(input$mantel_map))
    req(input$saved_maps=="new map")
    req(length(attr(getdata_map(),"layer_shape"))>0)
    if(is.null(vals$layer_col)){vals$layer_col<-"gray"}
    if(is.null(vals$layer_lighten)){vals$layer_lighten<-0.6}
    if(is.null(vals$layer_col_border)){vals$layer_col_border<-"gray"}
    column(12,
           div(
             class="well2",
             span(
               "+",
               checkboxInput("map_1f_layer","Layer Shape", T, width="120px"),
               div(style="padding-left: 15px",
                   div(class="palette",
                       span("+ Color:",
                            inline(div(
                              class = "ident-picker",
                              pickerInput(inputId = "layer_col",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="80px", selected=vals$layer_col)))
                       ),
                       span("+ Transp:",
                            numericInput("layer_lighten",NULL,value=vals$layer_lighten, width="80px", min=0, max=1, step=0.1)
                       )
                   ),
                   div(
                     class="palette",
                     span("+ Border color:",
                          inline(div(
                            class = "ident-picker",
                            pickerInput(inputId = "layer_col_border",label = NULL,choices =   vals$colors_img$val[getsolid_col()],choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]),options=list(container="body"),inline=F, width="80px", selected=vals$layer_col_border)))
                     )
                   )


               )

             )))
  })
  output$map_1e_limits<-renderUI({
    base_shape=attr(getdata_map(),"base_shape")
    layer_shape=attr(getdata_map(),"layer_shape")
    limits<-get_limits(limits=NULL,base_shape, layer_shape, coords = attr(getdata_map(),"coords"))
    column(12,
           div(class="well2",


               div(
                 div(
                   inline(div("+ Limits:",style="color: #05668D;width:60px")),
                   inline(div("Long",style="width:120px")),
                   inline(div("Lat",style="width:120px"))),
                 div(
                   inline(div("+ min", style="width: 50px")),
                   inline(numericInput("long_xmin",NULL, value=limits[1,1], width="120px", step=0.01)),
                   inline(numericInput("lat_xmin",NULL, limits[1,2], width="120px", step=0.01))
                 )),
               div(
                 inline(div("+ max", style="width: 50px")),
                 inline(numericInput("long_xmax",NULL, value=limits[2,1], width="120px", step=0.01)),
                 inline(numericInput("lat_xmax",NULL, limits[2,2], width="120px", step=0.01)))
           ))
  })
  output$show_z_coords<-renderUI({
    req(isTRUE(input$surface_map))
    my_rst<-attr(vals$saved_maps[[input$saved_maps]],'my_rst')

    z<-max(my_rst@data@values, na.rm=T)
    column(12,
           span("+ z: ",numericInput("z_coords",NULL, z, width="75px")))

  })
  output$show_z_colors<-renderUI({
    req(length(names(vals$saved_maps))>0)
    inline(pickerInput("saved_maps2",NULL, choices=names(vals$saved_maps),options=list(container="body"), width="130px"))  })
  output$show_map_coords<-renderUI({
    req(isTRUE(input$showcoords))
    div(style="padding-left: 15px",
        column(12,class="palette",' + Color:',
               pickerInput(inputId = "col_coords",
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           selected=vals$col_coords,
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="75px")),
        column(12,
               '+ Size:',
               inline(numericInput("pt_coords",NULL, vals$pt_coords, width="100px"))
        ),
        uiOutput("show_z_coords")
    )
  })
  observeEvent(ignoreInit = T,input$labels_coords,{
    vals$labels_coords<-input$labels_coords
  })
  output$show_map_labels<-renderUI({
    req(isTRUE(input$showfactors))
    div(style="padding-left: 15px",
        column(12,' + Factor:',
               pickerInput("labels_coords",NULL, choices=colnames( attr(getdata_map(),"factors")),options=list(container="body"), width="75px", inline=T, selected=vals$labels_coords)),
        #

        column(12,class="palette",' + Color:',
               pickerInput(inputId = "col_factor",
                           selected=vals$col_factor,
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), inline=T, width="120px")),
        column(12,
               '+ Size:',
               inline(numericInput("pt_factor",NULL, vals$pt_factor, width="100px"))
        ),
        if(isTRUE(input$surface_map)){my_rst<-attr(vals$saved_maps[[input$saved_maps]],'my_rst')
        z<-max(my_rst@data@values, na.rm=T)
        column(12, span("+",numericInput("z_labels",NULL, z, width="100px")))
        }
    )
  })
  output$scalesize_size<-renderUI({
    req(isTRUE(input$scale_map))
    column(12,"+",checkboxInput("scalesize_size","Size", vals$scalesize_size, width="100px"))
  })
  output$scalesize_color<-renderUI({
    req(isTRUE(input$scale_map))
    column(12,"+",checkboxInput("scalesize_color","Color", vals$scalesize_color, width="100px")
    )
  })
  output$interp_inputs<-renderUI({
    req(input$choices_map=="Numeric-Attribute")
    fluidRow(
      column(12, tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"for local kriging, the number of nearest observations that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations. By default (empty), all observations are used", placement = "bottom", options=list(container="body")),"+ nmax:",
             numericInput(
               "nmax_idp", NULL, NA, min = 0, width="75px"
             )
      ),
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"for local kriging, if the number of nearest observations within distance maxdist is less than nmin, a missing value will be generated- see maxdist", placement = "bottom", options=list(container="body")),"+ nmin:",
             numericInput(
               "nmin_idp", NULL, vals$nmin_idp, min = 0, width="75px"
             )
      ),
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"maximum number of observations to select per octant (3D) or quadrant (2D); only relevant if maxdist has been defined as well", placement = "bottom", options=list(container="body")),"+ omax:",
             numericInput(
               "omax", NULL, vals$omax, min = 0, width="75px"
             )
      ),
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"for local kriging, only observations within a distance of maxdist from the prediction location are used for prediction; if combined with nmax, both criteria apply", placement = "bottom", options=list(container="body")),"+ maxdist:",
             numericInput(
               "maxdist", NULL, vals$maxdist, min = 0, width="75px"
             )
      ),
      column(12,tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle",style="color: gray"),"numeric; specify the inverse distance weighting power", placement = "bottom", options=list(container="body")),
             "+ idp:",
             numericInput(
               "idp", NULL, vals$idp, min = 0, width="75px"
             )
      )
    )
  })
  output$map_1d_knn<-renderUI({
    req(input$choices_map=="Factor-Attribute")
    req(input$cmap=="interpolation")
    column(12," + KNN ",
           column(12, tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle", style="color: gray"), "number of neighbours considered.", options=list(containder="body")),
                  "+ k:",
                  numericInput(
                    "nmax", NULL, vals$nmax, min = 0, width="75px"
                  )
           ))


  })
  output$mapfac<-renderUI({
    req(input$colored_map %% 2)
    pickerInput("map_lab",NULL, choices=colnames(attr(getdata_map(),"factors")),options=list(container="body"), width="75px", inline=T)})
  observeEvent(ignoreInit = T,input$mantel_map,{
    if(isTRUE(input$mantel_map)){
      updateButton(session,"edit_map", value=F)
      updateButton(session,"surface_map", value=F)
      updateButton(session,"stack_map", value=F)
    }
  })
  observeEvent(ignoreInit = T,input$edit_map,{
    if(isTRUE(input$edit_map)){
      updateButton(session,"mantel_map", value=F)
      updateButton(session,"surface_map", value=F)
      updateButton(session,"stack_map", value=F)
    }
  })
  observeEvent(ignoreInit = T,input$surface_map,{
    if(isTRUE(input$surface_map)){
      updateButton(session,"mantel_map", value=F)
      updateButton(session,"edit_map", value=F)
      updateButton(session,"stack_map", value=F)
    }
  })
  observeEvent(ignoreInit = T,input$stack_map,{
    if(isTRUE(input$stack_map)){
      updateButton(session,"mantel_map", value=F)
      updateButton(session,"edit_map", value=F)
      updateButton(session,"surface_map", value=F)
    }
  })
  output$map_options<-renderUI({
    fluidRow(
      inline(div(style="margin-top:10px",uiOutput("add_map", inline=T))),

      inline(div(style="margin-top:10px",uiOutput("show_saved_maps", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("mantel_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("edit_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("scatter_3d", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("stack_scatter_3d", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("save_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("surface_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("rgl_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("ss_rgl", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("sr_rgl", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("stack_map", inline=T))),
      inline(div(style="margin-top:10px",uiOutput("trash_map", inline=T))),
      bsButton('downp_map',tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-download"),"Download plot"), style="button_active"),
      #actionLink('down_disc_loop',"*download loop", style="button_active"),
      inline(uiOutput("down_raster_btn")),
      inline(div(style="margin-top:10px",uiOutput("stop_map", inline=T))),

    )
  })
  output$down_raster_btn<-renderUI({
    req(input$cmap=="raster")
    div(

      downloadButton('down_raster',"geotif", style="button_active"),
      #actionButton('down_maps_loop',"*download loop", style="button_active")


    )})
  observeEvent(ignoreInit = T,input$down_maps_loop,{
    map_raster_loop()
  })
  observeEvent(ignoreInit = T,input$down_disc_loop,{
    map_discfac_loop()
  })


  map_raster_loop<-reactive({
    datao<-data<-vals$saved_data[[input$data_map]]
    withProgress(max=ncol(data),message="Running",{


      for(i in 1:ncol(data)) {

        mybreaks<-pretty(datao[,colnames(data)[i]])
        validate(need(input$choices_map=="Numeric-Attribute","This functionality is currently only available for Numeric-Attribute"))


        get<-colnames(datao)[i]
        data<-getdata_map()[filtermap(),,drop=F]
        coords<-attr(data,"coords")[rownames(data),]
        base_shape =if(isTRUE(input$map_1a_base)){attr(data,"base_shape") } else { NULL}
        limits<-as.matrix( cbind(
          c(input$long_xmin,  input$lat_xmin),
          c(input$long_xmax,  input$lat_xmax)
        ))
        req(get%in%colnames(data))
        p0<-p<-mapraster(data,get,coords, base_shape,limits)



        data<-getdata_map()[filtermap(),,drop=F]
        coords<-attr(data,"coords")[rownames(data),]
        layer_shape =if(isTRUE(input$map_1f_layer)){attr(data,"layer_shape") } else { NULL}

        p<-map_style(data=data,
                     get=get,
                     coords=coords,
                     p=p,
                     main = paste0(get,if(input$var_map_filter1!="None"){paste("-",input$var_map_filter1,input$var_map_filter2)}),
                     subtitle="",
                     cex.axes=input$pt_legend,
                     cex.lab=input$pt_legend,
                     cex.main=input$pt_titlemap,
                     cex.sub=14,
                     cex.leg=input$cex.key,
                     factors=labcoords(),
                     cex.fac=input$pt_factor+2,
                     col.fac=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                     showcoords=input$showcoords,
                     col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
                     cex.coords=input$pt_coords+1,
                     showguides=input$showguides,
                     layer_shape=layer_shape,
                     col.palette=input$pt_palette,
                     layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
                     lighten=input$layer_lighten,
                     leg="",
                     newcolhabs=vals$newcolhabs,
                     extralayers=extralayers(),
                     layer_shape_border=getcolhabs(vals$newcolhabs,input$layer_col_border,1),
                     breaks=mybreaks,
                     key.height=input$key.height,
                     keyscale=input$keyscale,  width_hint=input$scabarsize,cex_scabar=input$scabartextsize)
        attr(p,"args")<-l1()
        attr(p,"limits")<-attr(p0,"limits")
        attr(p,"base_shape")<-attr(p0,"base_shape")
        attr(p,"layer_shape")<-attr(p0,"layer_shape")
        my_rst<-attr(p0,"my_rst")
        attr(my_rst,"col.palette")<-input$pt_palette
        attr(p,"my_rst")<-my_rst
        attr(p,"data_z")<-attr(p0,"data_z")
        vals$map_res<-p


        file<-paste0(get,if(input$var_map_filter1!="None"){paste("-",input$var_map_filter1,input$var_map_filter2)})

        pdf(paste0(file,".pdf"), height=vals$cur_fheight, width=vals$cur_fwidth, pointsize = vals$cur_pointsize)


        plot(vals$map_res)
        graphics.off()
        Sys.sleep(0.5)

        png(paste0(file,".png"), height=vals$cur_fheight, width=vals$cur_fwidth, pointsize = vals$cur_pointsize, units="cm",res=300,type ="cairo-png")

        plot(vals$map_res)
        graphics.off()
        Sys.sleep(0.5)


        raster::writeRaster(my_rst, paste0(file,".tif"),format="GTiff")
        beep()
        graphics.off()
        Sys.sleep(0.5)
        incProgress(1)


      }
    })

  })

  map_discfac_loop<-reactive({
    datao<-data<-attr(vals$saved_data[[input$data_map]],"factors")
    withProgress(max=ncol(data),message="Running",{

      col_names<-colnames(datao)
      if(input$var_map_filter1!="None"){
        col_names<-col_names[-c(1:2)]
      }

      for(i in 1:ncol(data)) {

        data<-attr(getdata_map(),"factors")[filtermap(),,drop=F]
        coords<-attr(getdata_map(),"coords")[rownames(data),]
        get<-col_names[i]
        cat("\n",get)
        col=input$pt_palette

        req(get%in%colnames(data))
        file<-paste0(as.character(get),if(input$var_map_filter1!="None"){paste("-",input$var_map_filter1,input$var_map_filter2)})

        p<-suppressWarnings(
          map_discrete_variable(
            data = data,
            coords = coords,
            base_shape =if(isTRUE(input$map_1a_base)){attr(getdata_map(),"base_shape") } else { NULL},
            layer_shape =if(isTRUE(input$map_1f_layer)){attr(getdata_map(),"layer_shape") } else { NULL},
            get = get,
            main = file,
            factors=labcoords(),
            showcoords=input$showcoords,
            cex.pt = input$pt_points+6,
            cexmin.pt=input$pt_points_min,
            cex.coords=input$pt_coords+1,
            cex.fac=input$pt_factor+2,
            col.fac=input$col_factor,
            cex.axes=input$pt_legend,
            cex.lab=input$pt_legend,
            cex.leg=input$cex.key,
            leg="",
            col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,5)[1] } else{ NULL},
            col.palette=col,
            symbol=as.numeric(input$pt_symbol),
            scalesize_size= F,
            scalesize_color=F,
            points=T, input$pt_factor+6,
            as_factor=F,
            bmu=F,
            colored_by_factor=attr(getdata_map(),"factors")[as.character(get)][filtermap(),, drop=F],
            showguides=input$showguides,
            limits=as.matrix( cbind(
              c(input$long_xmin,  input$lat_xmin),
              c(input$long_xmax,  input$lat_xmax)
            )),
            layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
            lighten=input$layer_lighten,
            base_col=getcolhabs(vals$newcolhabs,input$base_col,1),
            base_lighten=input$base_lighten,
            newcolhabs=vals$newcolhabs,
            extralayers=extralayers(),
            data_depth=input$data_depth,
            layer_shape_border=getcolhabs(vals$newcolhabs,input$layer_col_border,1),
            base_shape_border= getcolhabs(vals$newcolhabs,input$base_col_border,1),
            cex.main=input$pt_titlemap,
            key.height=input$key.height,
            keyscale=input$keyscale,  width_hint=input$scabarsize,cex_scabar=input$scabartextsize
          )
        )






        Sys.sleep(0.5)
        png(paste0(file,".png"), height=vals$cur_fheight, width=vals$cur_fwidth, pointsize = vals$cur_pointsize, units="cm",res=300,type ="cairo-png")
        plot(p)
        graphics.off()
        incProgress(1)
        Sys.sleep(0.5)
        vals$map_res<-NULL
      }
    })

  })

  map_disc_loop<-reactive({})

  output$down_raster<-downloadHandler(
    filename = "raster.tif",
    content = function(file) {
      raster::writeRaster(attr(vals$map_res,"my_rst"), file,format="GTiff")
    }
  )



  automap_war<-reactive({
    renderUI({
      div(em(icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"Click to apply input changes"))
    })
  })
  disc_data_map<-reactive({
    if(isTRUE(input$automap)|isTRUE(vals$gomap)){
      if(isFALSE(vals$stopmap)){
        vals$map_res<-map_data_disc()}}
    update_automap()
  })
  interp_data_map<-reactive({
    if(isTRUE(input$automap)|isTRUE(vals$gomap)){
      if(isFALSE(vals$stopmap)){map_data_interp()

      }}
    update_automap()
  })
  raster_map<-reactive({
    if(isTRUE(input$automap)|isTRUE(vals$gomap)){map_raster()}
    update_automap()
  })
  disc_fac_map<-reactive({
    if(isTRUE(input$automap)|isTRUE(vals$gomap)){
      if(isFALSE(vals$stopmap)){map_fac_disc()}}

  })

  map_fac_disc<-reactive({
    args<-map_factor_args()


    m<-do.call(map_discrete_variable,args)
    attr(m,"args")<-l1()
    vals$map_res<-m
  })
  map_factor_args<-reactive({
    req(input$var_map)
    req(vals$vars_fac%in%input$var_map)
    get<-vals$vars_fac[which(vals$vars_fac==input$var_map)]
    data<-attr(getdata_map(),"factors")[filtermap(),,drop=F]
    req(nrow(data)>0)

    req(input$pt_legend)

    req(input$pt_titlemap)

    req(input$cex.key)
    req(input$key.height)
    req(input$keyscale)
    req(input$scabarsize)
    req(input$scabartextsize)
    # req(input$breaks_map)
    #mybreaks<-as.numeric(unlist(strsplit(input$breaks_map,",")))
    # req(length(mybreaks))>0
    req(input$pt_points)
    req(input$long_xmin)
    req(input$long_xmax)
    req(input$lat_xmin)
    req(input$lat_xmax)

    coords<-attr(getdata_map(),"coords")[rownames(data),]
    req(get%in%colnames(data))
    pizza_fac<-NULL
    if(isTRUE(input$pizza_map)){
      req(input$pizza_fac)
      factors<-attr(vals$saved_data[[input$data_map]],"factors")
      pizza_fac<-factors[filtermap(),input$pizza_fac]
    }
    colored_by_factor=attr(getdata_map(),"factors")[as.character(input$var_map)][filtermap(),, drop=F]

    args<-list(
      data = data,
      coords = coords,
      base_shape =if(isTRUE(input$map_1a_base)){attr(getdata_map(),"base_shape") } else { NULL},
      layer_shape =if(isTRUE(input$map_1f_layer)){attr(getdata_map(),"layer_shape") } else { NULL},
      get = get,
      main = input$map_title,
      factors=labcoords(),
      showcoords=input$showcoords,
      cex.pt = input$pt_points+6,
      cexmin.pt=input$pt_points_min,
      cex.coords=input$pt_coords+1,
      cex.fac=input$pt_factor+2,
      col.fac=input$col_factor,
      cex.axes=input$pt_legend,
      cex.lab=input$pt_legend,
      cex.leg=input$cex.key,
      leg=input$map_legend,
      col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,5)[1] } else{ NULL},
      col.palette=input$pt_palette,
      symbol=as.numeric(input$pt_symbol),
      scalesize_size= F,
      scalesize_color=F,
      points=T,

      as_factor=F,
      bmu=F,
      colored_by_factor=colored_by_factor,
      showguides=input$showguides,
      limits=as.matrix( cbind(
        c(input$long_xmin,  input$lat_xmin),
        c(input$long_xmax,  input$lat_xmax)
      )),
      layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
      lighten=input$layer_lighten,
      base_col=getcolhabs(vals$newcolhabs,input$base_col,1),
      base_lighten=input$base_lighten,
      newcolhabs=vals$newcolhabs,
      extralayers=extralayers(),
      data_depth=input$data_depth,
      layer_shape_border=getcolhabs(vals$newcolhabs,input$layer_col_border,1),
      base_shape_border= getcolhabs(vals$newcolhabs,input$base_col_border,1),
      cex.main=input$pt_titlemap,
      key.height=input$key.height,
      keyscale=input$keyscale,  width_hint=input$scabarsize,cex_scabar=input$scabartextsize,
      pie=input$pizza_map,
      facpizza=pizza_fac
    )
    args
  })
  interp_fac_map<-reactive({
    if(isTRUE(input$automap)|isTRUE(vals$gomap)){
      if(isFALSE(vals$stopmap)){map_fac_interp()}}
    update_automap()

  })
  stack_map<-reactive({
    try({

      if(isTRUE(input$automap)|isTRUE(vals$gomap)){
        if(isFALSE(vals$stopmap)){get_stackmap()}}
      update_automap()


    })
  })

  observeEvent(ignoreInit = T,input$gomap,{
    req(isTRUE(input$stack_map))
    get_stackmap()
  })


  update_automap<-reactive({
    if(!is.null(vals$map_res)){
      if(isTRUE(vals$gomap)){
        vals$gomap<-F
        if(nrow(getdata_map())>1000){
          vals$stopmap<-T
        }
      }
    }
  })



  observeEvent(ignoreInit = T,input$gomap,{
    vals$gomap<-T
    if(isTRUE(vals$stopmap)){
      vals$stopmap<-F
    }
    output$automap_war<-renderUI({NULL})
  })
  observeEvent(ignoreInit = T,input$automap,{
    vals$autoload<-input$automap
  })

  observe({

    req(length(vals$saved_data)>0)
    req(input$data_map)
    data<-vals$saved_data[[input$data_map]]
    req(length(data)>0)
    if(nrow(data)>1000){
      vals$map_res<-NULL

      output$stop_map<-renderUI(stop_map_war())
      req(input$automap)
      updateSwitchInput(session,"automap",value=F)
      vals$autoload<-F
      vals$stopmap<-T
      output$automap_war<-automap_war()
    } else{
      vals$stopmap<-F
      warbigmap$df<-F
      output$stop_map<-renderUI(NULL)
    }

  })

  observe({
    req(input$choices_map)
    req(input$cmap)
    req(input$var_map_filter1)
    if(input$var_map_filter1!="None"){
      req(input$var_map_filter2)
    }
    if(input$choices_map=="Numeric-Attribute"){
      if(input$cmap=='discrete'){try(disc_data_map(), silent=T)} else if(input$cmap=='interpolation'){try(interp_data_map(), silent = T)} else if(input$cmap=='raster'){try(raster_map(), silent=T)}
    } else  if(input$choices_map=='Factor-Attribute'){
      if(input$cmap=='discrete'){try(disc_fac_map(), silent=T)} else if(input$cmap=='interpolation'){try(interp_fac_map(), silent=T)}
    }
    if(length(input$stack_map)>0){
      if(isTRUE(input$stack_map)){
        #   savereac()
        stack_map()}
    }

  })

  output$map_out<-renderUI({
    req(input$choices_map)
    req(input$cmap)



    if(input$cmap=='raster'){
      validate(need(input$choices_map=="Numeric-Attribute","Raster tool currently only available for the Numeric-Attribute"))}
    res<-div(

      uiOutput('map_out1'),
      uiOutput("scatter_out"),
      uiOutput("ss3d"),
      uiOutput("map_ssrgl"),
      uiOutput("map_srrgl"),
      uiOutput("map_out2"),
      uiOutput("map_stack"),
      uiOutput("stack_control")

    )

    res
  })

  output$stack_control<-renderUI({
    column(12,class="map_control_style2",
           column(6,uiOutput("sr_3dcontrol")),
           column(6,uiOutput("sr_label_options")))
  })

  output$map_out1<-renderUI({
    if(length(input$stack_scatter_3d)>0){
      req(isFALSE(input$stack_scatter_3d))}
    req(!isTRUE(input$scatter_3d))
    req(!isTRUE(input$mantel_map))
    #req(input$saved_maps=="new map"|isTRUE(input$edit_map))
    div(renderPlot(try(vals$map_res, silent=T)))
  })
  output$scatter_colorby<-renderUI({
    req(isTRUE(input$scatter_3d))
    span("+",
         checkboxInput("scatter_4D"," 4D", F, width="50px"),inline(uiOutput("colorzby")),
         uiOutput("scatter_4D"))


  })
  output$colorzby<-renderUI({
    req(isTRUE(input$scatter_4D))
    em(a("color z by:"))
  })
  output$scatter_4D<-renderUI({
    selected<-if(input$choices_map=="Numeric-Attribute"){
      "Variable"
    } else{'Factor' }
    req(isTRUE(input$scatter_4D))
    column(12,"Datalist:",
           inline(pickerInput("scatter_datalistz",NULL, choices=names(vals$saved_data),options=list(container="body"), width="125px", inline=T,
                              selected=input$data_map)),
           span(pickerInput("scatter_colorby",NULL, choices=c("Variable","Factor"),options=list(container="body"), width="75px", inline=T, selected=selected), "::",inline(uiOutput("scatter_colorz"))),
    )
  })
  output$scatter_colorz<-renderUI({
    data<-vals$saved_data[[input$scatter_datalistz]]
    choices<-if(input$scatter_colorby=="Variable"){colnames(data)} else{
      colnames(attr(data,"factors"))
    }
    pickerInput("scatter_colorz",NULL, choices=choices,options=list(container="body"), width="90px", inline=T, selected=input$var_map)
  })

  output$scatter_out<-renderUI({
    req(input$surf_d)
    validate(need(input$surf_d>0, "error: requires persp strength >0 "))
    req(isTRUE(input$scatter_3d))
    data<-getdata_map()
    factors<-attr(data,"factors")
    coords<-attr(data,"coords")
    base_shape=if(isTRUE(input$map_1a_base)){
      attr(data,"base_shape") } else{ NULL}
    layer_shape=if(isTRUE(input$map_1f_layer)){
      attr(data,"layer_shape") } else{ NULL}
    z<-data[,input$var_map]

    if(isTRUE(input$scatter_4D)){
      data<-vals$saved_data[[input$scatter_datalistz]]
      leg<-input$scatter_colorz
      colvar<-colvar0<-if(input$scatter_colorby=="Variable"){
        data[,input$scatter_colorz]
      } else{
        factors<-attr(data,"factors")
        factors[,input$scatter_colorz]
      }
    } else{ leg=input$var_map}

    z00<-z0<-if(isTRUE(input$scatter_4D)){
      colvar} else{z}
    col= getcolhabs(vals$newcolhabs,input$pt_palette,n=100)
    z0<-scales::rescale(as.numeric(z0),c(1,100))
    pt_col<-col[cut(z0, breaks=max(z0))]

    if(is.factor(z00)){
      lab=levels(z00)
      at=1:length(lab)

    } else{
      lab=pretty(z00)
      at=pretty(z00)
    }
    at=scales::rescale(as.numeric(at),c(.3,.8))

    col_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$layer_col,1), input$layer_lighten)
    col_base=adjustcolor(getcolhabs(vals$newcolhabs,input$base_col,1), input$base_lighten)
    div(
      renderPlot({
        layout(matrix(1:2,ncol=2), widths = c(6,1),heights = c(1,1))
        get_scatter3D(z,
                      base_shape,
                      layer_shape,
                      coords,
                      col.grid  = "gray",
                      colkey=F,
                      symbol=as.numeric(input$pt_symbol),
                      scale_points = F,
                      pt_cex=input$pt_points,
                      fac_cex = input$pt_factor,
                      col_base=col_base,
                      col_layer=col_layer,
                      showlabels=input$showfactors,
                      showpoints=T,
                      xlim=c(input$long_xmin,input$long_xmax),
                      ylim=c(input$lat_xmin,input$lat_xmax),
                      labels= factors[,input$labels_coords],
                      pt_col=pt_col,
                      fac_col=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                      zlab=input$var_map,
                      theta=input$surf_theta,
                      phi=input$surf_phi,
                      r=input$surf_r,
                      d=input$surf_d,
                      exp=input$surf_exp
        )
        legend_image<-as.raster(matrix(col, ncol=1))
        par(mar=c(5,0,5,2), xpd=T)
        plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
        mtext(leg,3, line=-3)
        rasterImage(legend_image, 0, .3, .8,.8)
        text(x=1.6, y = as.numeric(rev(at)), labels = lab)
        vals$map_res<-recordPlot()
      })
    )
  })
  output$map01<-renderUI({
    req(input$saved_maps!="new map")
    req(!isTRUE(input$surface_map))
    req(!isTRUE(input$stack_map))
    req(!isTRUE(input$mantel_map))
    renderPlot(vals$saved_maps[[input$saved_maps]])

  })
  output$map02<-renderUI({
    req(!isTRUE(input$mantel_map))
    req(isTRUE(input$surface_map))
    req(!isTRUE(input$rgl_map))

    column(12,uiOutput("coarser"),
           uiOutput("persp_out"))
  })
  output$coarser<-renderUI({
    coarser<-attr( vals$map_res,"coarser")
    req(!is.null(coarser))
    coa<- if(coarser==1){
      input$saved_maps
    } else{
      input$saved_maps2

    }
    p(strong("Warning:", style="color: red"),"The selected rasters have different resolutions and were united to the coarser resolution (",coa,")")






  })
  output$map03<-renderUI({
    req(!isTRUE(input$mantel_map))
    req(isTRUE(input$surface_map))
    req(isTRUE(input$rgl_map))
    is_ggplot3_available<-require("rgl")
    validate(need(is_ggplot3_available,"This feature requires the 'rgl' package. Before closing iMESc, please create a savepoint. Then, close the application, install the package, and reopen iMESc. Remember to reload the savepoint to continue where you left off"))

    rglwidgetOutput("m3d_map",  width = 600, height = 600)
  })

  output$map_out2<-renderUI({
    req(input$saved_maps!="new map")
    fluidRow(
      uiOutput("map01"),
      uiOutput("map02"),
      uiOutput("map03")


    )})

  output$map05<-renderUI({
    req(input$choices_map)
    req(isTRUE(input$mantel_map))
    req(length(input$var_map)>0)

    req(input$mantel_nclass)
    req(input$mantel_r.type)
    req(input$mantel_nperm)
    req(input$mantel_cutoff)

    data<-getdata_map()
    coords<-attr(data,"coords")
    data=  switch(input$choices_map,
                  'Factor-Attribute'=rev(attr(data,"factors")),
                  'Numeric-Attribute'=data)
    colnames(coords)<-c ("x", "y")
    zvalues<-data[,input$var_map]
    geo.dist<-geodist(x=coords, paired=T, measure="geodesic")
    dist.mat<-dist(zvalues)
    if(input$choices_map=="Factor-Attribute"){dist.mat<-dist(
      kohonen::classvec2classmat(as.matrix(zvalues)))}

    vals$correlog<-vegan::mantel.correlog(dist.mat, geo.dist, n.class=input$mantel_nclass, break.pts=NULL,cutoff=input$mantel_cutoff, r.type=input$mantel_r.type, nperm=input$mantel_nperm, mult="holm", progressive=TRUE )
    # Comparing
    div(
      plotOutput("mantel_plot"),
      uiOutput("mantel_res"),
      uiOutput("mantel_break.pts")
    )
  })
  output$mantel_break.pts<-renderUI({
    req(input$spatial_stats)
    req(input$spatial_stats=="break.pts")
    renderPrint(data.frame(breaks=vals$correlog$break.pts))
  })

  output$mantel_res<-renderUI({
    req(input$spatial_stats)
    req(input$spatial_stats=="mantel.res")
    renderPrint(vals$correlog$mantel.res)

  })

  output$mantel_plot<-renderPlot({
    req(input$spatial_stats)
    req(input$spatial_stats=="plot")
    plot(vals$correlog,input$mantel_alpha, xlab="Distance class (m)")
    vals$mantel_plot<-mantel_plot$df<-recordPlot()
  })



  observeEvent(ignoreInit = T,input$tabs,{
    #req(is.null(vals$goup))
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






  gettitle2<-function(x) {
    if(length(x)>0){
      switch(x,
             "menu_intro"={"Introduction"},
             "menu_upload"={"Data Bank"},
             'menu_explore'={"Descriptive tools"},
             'menu_nb'={"Naive Bayes"},
             'menu_svm'={"Support Vector Machine"},
             'menu_som'={"Unsupervised Self-Organizing Maps"},
             'menu_som2'={"Supervised Self-Organizing Maps"},
             'menu_knn'={"K-Nearest Neighbor"},
             'menu_sgboost'={"Stochastic Gradient Boosting"},
             "menu_compare"={"Compare models"},
             "menu_ensemble"={"Ensemble models"},
             "menu_spd"={"Explore Saved Models"},
             'menu_kmeans'={"K-Means"},
             'menu_hc'={"Hierarchical Clustering"},
             'menu_rf'={"Random Forest"},
             'menu_maps'={"Spatial tools"},
             'menu_box'={"Box plots"},
             'menu_div'={"Biodiversity tools"},
             'menu_down'={"Download Center"}

      )
    }


  }

  position<-reactiveVal(1)

  menu_steps<-reactiveValues(df="menu_intro", num=1, cur=NULL)
  output$menutitle<-renderUI({
    column(12,
           uiOutput("bug"),
           fluidRow(style="margin-left: -35px; margin-right: -45px; margin-top: -25px",
                    div(
                      div(
                        div(viposition= gettitle2(menu_steps$df[position()-1]),
                            uiOutput('prevmenu')
                            ,
                            style="text-align:left;"
                        ),class = "col-sm-6", style = "padding-left:0"
                      )
                      ,
                      div(
                        div(viposition2=gettitle2(menu_steps$df[position()+1]),
                            uiOutput('nextmenu')
                            ,
                            style="text-align:right; "
                        ),class = "col-sm-6", style = "padding-right:0"
                      )
                      ,
                      class="col-sm-12")
           ),
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
  observeEvent(ignoreInit = T,input$next_menu, {
    position(position()+1)
    menu_steps$cur<-menu_steps$num[position()]
    menu_steps$anext<-T
    #delay(500,updateTextInput(session, "tabs", value = menu_steps$df[menu_steps$cur]))

  })

  observeEvent(position(),{
    updateTextInput(session, "tabs", value = menu_steps$df[position()])
  })
  observeEvent(ignoreInit = T,input$prev_menu, {
    position(position() - 1)
    menu_steps$cur<-menu_steps$num[position()]
    menu_steps$prev<-T
    #delay(500,updateTextInput(session, "tabs", value = menu_steps$df[menu_steps$cur]))

  })



  output$prevmenu<-renderUI({
    req(position()>1)
    tags$div(class='button_position',actionButton("prev_menu","<<"))


  })

  output$nextmenu<-renderUI({
    req(position()!=length(menu_steps$df))
    tags$div(class='button_position',actionButton("next_menu",">>"))


  })

  observe(vals$saved_divset_value<-isolate(length(vals$saved_data)+1))
  getdata_hc<-reactive({
    req(input$data_hc)
    data=vals$saved_data[[input$data_hc]]
    validate(need(length(data)>0,"no data found"))

    data
  })
  getdata_map<-reactive({
    req(input$data_map)
    req(length(vals$saved_data[[input$data_map]])>0)
    data=vals$saved_data[[input$data_map]]
    #req(nrow(data)>0)
    #req(input$var_map_filter1)
    #factors<-attr(data, "factors")
    #coords<-attr(data,"coords")
    data
  })
  filtermap<-reactive({
    data<-getdata_map()
    pic<-rownames(data)
    if(length(input$var_map_filter1)>0){
      if(input$var_map_filter1!="None"){
        req(input$var_map_filter2)
        factors<-attr(data,"factors")
        filtro<-as.character(input$var_map_filter1)
        filtro2<-as.character(input$var_map_filter2)
        pic0<-which(as.character(factors[, filtro]) == filtro2)
        pic<-pic[pic0]
      }
    }
    pic
  })

  observeEvent(ignoreInit = T,input$bmu_p_bgpalette,
               {bmu_p_bgpalette$df<-input$bmu_p_bgpalette})
  observeEvent(ignoreInit = T,input$bmu_p_dotlabel,{
    bmu_p_dotlabel$df<-input$bmu_p_dotlabel
  })
  observeEvent(ignoreInit = T,input$bmu_p_training,{
    bmu_p_training$df<-input$bmu_p_training
  })
  observeEvent(ignoreInit = T,input$bmu_p_test,{
    bmu_p_test$df<-input$bmu_p_test
  })
  observeEvent(ignoreInit = T,input$bmu_p_symbol,{
    bmu_p_symbol$df<-input$bmu_p_symbol
  })
  observeEvent(ignoreInit = T,input$bmu_p_symbol_size,{
    vals$bmu_p_symbol_size<-input$bmu_p_symbol_size
  })
  observeEvent(ignoreInit = T,input$bmu_p_bg_transp,{
    vals$bmu_p_bg_transp<-input$bmu_p_bg_transp
  })
  observeEvent(ignoreInit = T,input$bmu_p_border_grid,{
    bmu_p_border_grid$df<-input$bmu_p_border_grid
  })
  observeEvent(ignoreInit = T,input$bmu_p_factors,{
    vals$bmu_p_factors<-input$bmu_p_factors
  })
  observeEvent(ignoreInit = T,input$bmu_bgpalette,{
    bmu_bgpalette$df<-input$bmu_bgpalette
  })
  observeEvent(ignoreInit = T,input$bmu_dotlabel,{
    bmu_dotlabel$df<-input$bmu_dotlabel
  })
  observeEvent(ignoreInit = T,input$bmu_facpalette,{
    bmu_facpalette$df<-input$bmu_facpalette
  })
  observeEvent(ignoreInit = T,input$bmu_symbol,{bmu_symbol$df<-input$bmu_symbol})
  observeEvent(ignoreInit = T,input$bmu_symbol_size,{
    vals$bmu_symbol_size<-input$bmu_symbol_size
  })
  observeEvent(ignoreInit = T,input$bmu_cexvar,{vals$bmu_cexvar<-input$bmu_cexvar})
  observeEvent(ignoreInit = T,input$bmu_bg_transp,{
    vals$bmu_bg_transp<-input$bmu_bg_transp
  })
  observeEvent(ignoreInit = T,input$bmu_border_grid,{
    bmu_border_grid$df<-input$bmu_border_grid
  })
  observeEvent(ignoreInit = T,input$bmu_factors,{
    vals$cur_bmu_factors<-input$bmu_factors
  })
  observeEvent(ignoreInit = T,input$bmu_insertx,{vals$bmu_insertx<-input$bmu_insertx})
  observeEvent(ignoreInit = T,input$bmu_inserty,{vals$bmu_inserty<-input$bmu_inserty})
  observeEvent(ignoreInit = T,input$bmu_ncol,{vals$bmu_ncol<-input$bmu_ncol})
  observeEvent(ignoreInit = T,input$bmu_leg_transp,{
    vals$bmu_leg_transp<-input$bmu_leg_transp
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
               'menu_maps'={
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




  output$upload_control<-renderUI({
    req(length(getdatalist())>0)
    bsButton("next_upload","Continue", width="125px")
  })
  output$back_upload<-renderUI({
    actionButton("back_upload","Back", width="125px")
  })


  output$textbreak<-renderText("This action creates a single binary column per factor level, with 1 indicating the class of that particular observation")



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
    choices<-c("median/mode","Knn","bagImpute","medianImpute")
    if(input$na_targ=="Factor-Attribute"){
      choices<-c("median/mode")
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
  output$info_save_changes<-renderUI({
    req(!sum(vals$bagdata==c(T,T))==2)
    div(
      icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-right"),"Save your changes"
    )
  })










  observeEvent(ignoreInit = T,input$match,{
    if(!input$match %% 2)
      updateSelectInput(session,"filter_data", selected="none")
  })

  output$tools_split<-renderUI({
    validate(need(sum(vals$bagdata==c(T,T))==2,"You have unsaved changes. Save the changes to perform the split"))

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

  ##### Build column


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
  output$tochar_prev<-renderUI({
    renderPrint({
      char_picvar()
    })
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

    form3<-gsub("√","sqrt",form)
    fun<-function(charvar,form3){
      res<-try({
        with(charvar,eval(parse(text=paste0(form3, collapse=""))))
      },silent =T)
      validate(need(class(res)!="try-error","Check your formula. if you are using square root, make sure that the object you want to take the root of is enclosed in parentheses."))


      res
    }
    fun(charvar,form3)


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
  output$tochar_gsbub<-renderUI({
    req(input$char_fun)
    req(input$char_fun=="fr")
    div(style="vertical-align: text-top;display: table;",
        div(



        )

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
                     icon_list2(c("+","-","*","/","(",")","√",'^'))
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


  #######
  #######
  #######


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
  output$editmodel_print<-renderUI({


    renderPrint(vals$oldmodelsnames)
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
      fileInput("newfactorfile", label="2. Upload a '.csv' file:",accept = c(".csv")),
      uiOutput("newfactors_missing")
    )
  })
  choices_buck<-reactive({
    data<-vals$saved_data[[input$picker_bucked_cols]]
    if(is.null(vals$choices_bag)){
      choices_a=colnames(data)
      choices_b=NULL
    } else{
      choices_a=vals$choices_bag[[input$picker_bucked_cols]][[1]]
      choices_b=vals$choices_bag[[input$picker_bucked_cols]][[2]]
    }
    list(choices_a,choices_b)
  })
  observeEvent(ignoreInit = T,input$picker_bucked_cols,
               output$bucked_cols<-renderUI({
                 req(input$picker_bucked_cols)
                 choices_a<-choices_buck()[[1]]
                 choices_b<-choices_buck()[[2]]
                 div(
                   div(
                     class="bucked_col",
                     width = 12,
                     bucket_list(class="buck_rank_cols",
                                 header = NULL,
                                 group_name = 'colbuck_name',
                                 orientation = "horizontal",
                                 add_rank_list(
                                   text = "Drag from here",
                                   labels = choices_a,
                                   input_id = 'rank_list_cols_1'

                                 ),
                                 add_rank_list(
                                   text = "to here",
                                   labels = choices_b,
                                   input_id = 'rank_list_cols_2'
                                 )
                     )
                   )
                 )
               }))
  observeEvent(vals$saved_data,{
    choices_a<-lapply(vals$saved_data, function (x) colnames(x))
    choices_b<-lapply(vals$saved_data, function (x) NULL)
    vals$choices_bag<-mapply(list, choices_a,choices_b, SIMPLIFY=FALSE)


  })
  observeEvent(list(input$rank_list_cols_1, input$rank_list_cols_2),{
    req(input$picker_bucked_cols)
    req(input$rank_list_cols_1)
    req(input$rank_list_cols_2)
    ranks<-list(input$rank_list_cols_1, input$rank_list_cols_2)
    vals$choices_bag[[input$picker_bucked_cols]]<-ranks

  })
  observeEvent(vals$choices_bag,{
    choices_togther<-do.call(c,lapply(vals$choices_bag, function (x) x[[2]]))


    vals$choices_togther<-choices_togther

  })
  output$results_1<-renderPrint({input$rank_list_cols_1 })
  output$results_2<-renderPrint({input$rank_list_cols_2})
  output$results_3<-renderPrint({vals$choices_togther})
  output$merge_button<-renderUI({
    div( inline(uiOutput("merge_btn_cancel")),
         inline(uiOutput("merge_btn_prev")),

         inline(uiOutput("merge_btn_next")),
         inline(uiOutput("merge_btn_end")))
  })
  observeEvent(ignoreInit = T,input$merge_datalist_cancel,
               removeModal())
  output$editfac_datalist<-renderUI({
    div(
      pickerInput("editfac","Datalist:",choices=names(vals$saved_data), width="200px")
    )
  })
  output$editfac_btns<-renderUI({
    div(
      actionButton("editfac_go","Confirm"),
      actionButton("editfac_end","Cancel")
    )
  })
  output$editfac_cols<-renderUI({
    bag_newfac$df<-0
    cols<-colnames(attr(vals$saved_data[[input$editfac]],"factors"))
    lbin<-lapply(cols,function(x){uiOutput(paste("newcolfac",x))})
    res<-div(style="height:200px;overflow-y: scroll;overflow-x: scroll",
             checkboxGroupInput("select_factor", "Factors:",
                                choiceValues =cols,
                                choiceNames = lbin,
                                selected = cols))
    bag_newfac$df<-1
    res
  })
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
        uiOutput("editdat_cols"),
        uiOutput("teste")

    )
  })
  output$teste<-renderUI({
    div("teste",

        renderPrint(Clicked())
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
  output$teste<-renderText({paste0("You Selected Row: ",Clicked())})
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
  output$ddcogs1_targ<-renderUI({
    input$ddcogs1_pick
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
  output$ddcogs1_pick<-renderUI({
    pickerInput("ddcogs1_pick", div("Select a variable"),choices=colnames(vals$saved_data[[input$data_bank]]), selected=vals$ddcogs1_pick, width="200px")
  })
  observeEvent(ignoreInit = T,input$ddcogs1_pick,{
    vals$ddcogs1_pick<-input$ddcogs1_pick
  })
  observeEvent(ignoreInit = T,input$dfcogs1_pick,{
    vals$dfcogs1_pick<-input$dfcogs1_pick
  })
  output$dfcogs1_pick<-renderUI({
    pickerInput("dfcogs1_pick", div("Select a factor"),choices=colnames(attr(vals$saved_data[[input$data_bank]],"factors")), selected=vals$dfcogs1_pick, width="200px")
  })
  output$ddcogs3_pick<-renderUI({
    span(strong("Datalist:"),em(input$data_bank,style='color: SeaGreen'))
  })
  output$ddcogs3_edit<-renderUI({
    pickerInput("ddcogs3_edit", div("Edit Changes:"),colnames(attr(vals$saved_data[[input$data_bank]],"transf")),width="200px", selected=vals$ddcogs3_edit)
  })
  observeEvent(ignoreInit = T,input$ddcogs3_edit,{
    vals$ddcogs3_edit<-input$ddcogs3_edit
  })
  observeEvent(ignoreInit = T,input$ddcogs3,{

    showModal(
      removechange()
    )
  })
  removechange<-reactive({

    modalDialog(size="m",easyClose = T,footer =NULL,
                div(
                  inline(uiOutput(
                    'ddcogs3_pick'
                  )),
                  span(
                    inline(uiOutput('ddcogs3_edit')),
                    bsButton("ddcogs1_rename_remove",icon(verify_fa = FALSE,name=NULL,class="fas fa-trash-alt")), modalButton("Dismiss")
                  ),
                  div(
                    renderPrint(attr(vals$saved_data[[input$data_bank]],"transf"))
                  )
                )
    )
  })
  observeEvent(ignoreInit = T,input$ddcogs1_rename_remove,{
    #pic<-colnames(attr(vals$saved_data[[input$data_bank]],"transf"))==input$ddcogs3_edit
    attr(vals$saved_data[[input$data_bank]],"transf")<-data.frame(attr(vals$saved_data[[input$data_bank]],"transf"))
    attr(vals$saved_data[[input$data_bank]],"transf")[input$ddcogs3_edit]<-NULL
  })
  observeEvent(ignoreInit = T,input$ddcogs1,{
    showModal(
      modalDialog(size="m",easyClose = T,footer =NULL,
                  div(
                    inline(uiOutput(
                      'ddcogs1_pick'
                    )),
                    span(
                      inline(textInput("ddcogs1_rename", div("Rename Variable:",em(inline(uiOutput('ddcogs1_targ')), style="color: SeaGreen")),placeholder = "New name", width="200px")),
                      bsButton("ddcogs1_rename_go","Rename"), modalButton("Dismiss")
                    )
                  )
      )
    )
  })
  observeEvent(ignoreInit = T,input$dfcogs1,{
    showModal(
      modalDialog(size="m",easyClose = T,footer =NULL,
                  div(
                    inline(uiOutput(
                      'dfcogs1_pick'
                    )),
                    span(
                      inline(textInput("dfcogs1_rename", div("Rename Factors:",em(inline(uiOutput('dfcogs1_targ')), style="color: SeaGreen")),placeholder = "New name", width="200px")),
                      bsButton("dfcogs1_rename_go","Rename"), modalButton("Dismiss")
                    )
                  )
      )
    )
  })
  observeEvent(ignoreInit = T,input$ddcogs1_rename_go,{
    req(input$ddcogs1_rename!="")
    pic<- which( colnames(vals$saved_data[[input$data_bank]])==input$ddcogs1_pick)
    colnames(vals$saved_data[[input$data_bank]])[pic]<-input$ddcogs1_rename
  })
  observeEvent(ignoreInit = T,input$dfcogs1_rename_go,{
    req(input$dfcogs1_rename!="")
    pic<- which( colnames(attr(vals$saved_data[[input$data_bank]],"factors"))==input$dfcogs1_pick)
    colnames(attr(vals$saved_data[[input$data_bank]],"factors"))[pic]<-input$dfcogs1_rename
  })
  output$ddcogs1_tar<-renderUI({
    ddcogs1_tar
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
  ## Arrumar factor to factor
  ##
  ##

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
  output$ddcogs3<-renderUI({
    #req(length(attr(vals$saved_data[[input$data_bank]],"transf")>0))
    div(class="ddlinks",
        actionLink("ddcogs3","Edit changes")
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

    output$newfactors_missing<-renderUI({
      validate(need(length(grep(".csv",input$newfactorfile$datapath))==length(input$newfactorfile$datapath),"Error: the uploaded file is not an csv file"))
    })
    validate(need(length(grep(".csv",input$newfactorfile$datapath))==length(input$newfactorfile$datapath),"Error: the uploaded file is not an csv file"))
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
  output$viewshapes<-renderUI({
    choices<-layers_choices()
    column(12,style="margin-top: 10px",
           span(
             inline(
               pickerInput("pick_elayers","Shapes:",  choices, width="200px")
             ),
             actionButton("delete_elayer_shape",icon(verify_fa = FALSE,name=NULL,class="fas fa-trash-alt"))

           ),


           uiOutput('viewbase'),
           uiOutput('viewlayer'),
           uiOutput("elayers")
    )

  })
  observeEvent(ignoreInit = T,input$delete_elayer_shape,{
    req(input$pick_elayers!='Base Shape')
    req(input$pick_elayers!='Layer Shape')
    attr(vals$saved_data[[input$data_bank]],"extra_shape")[[input$pick_elayers]]<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  observeEvent(ignoreInit = T,input$delete_elayer_shape,{
    req(input$pick_elayers=='Base Shape')
    attr(vals$saved_data[[input$data_bank]],"base_shape")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  observeEvent(ignoreInit = T,input$delete_elayer_shape,{
    req(input$pick_elayers=='Layer Shape')
    attr(vals$saved_data[[input$data_bank]],"layer_shape")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  output$elayers<-renderUI({
    req(input$pick_elayers)
    div(

      #renderPrint(attr(vals$saved_data[[input$data_bank]],"extra_shape")),
      renderPlot({
        shape<-
          attr(vals$saved_data[[input$data_bank]],"extra_shape")[[input$pick_elayers]]
        req(!is.null(shape))
        ggplot(st_as_sf(shape)) + geom_sf()+
          theme(panel.background = element_rect(fill = "white"),
                panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))



      })
    )
  })
  output$rf_remove<-renderUI({
    div(
      popify(bsButton("trash_rf",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"), style="button_active"),NULL,"Select and remove a rf")

    )
  })
  observeEvent(ignoreInit = T,input$trash_rf ,{
    showModal(
      modalDialog(easyClose = T,
                  title=strong("Remove RF-Attribute"),
                  div(
                    div(
                      div(style="overflow-y: scroll;height: 200px;overflow-x: scroll; padding-top: 10px",
                          checkboxGroupInput("remove_rf", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"rf")))
                      )
                    ),
                    bsButton("delete_rf",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                  )
      )
    )

  })
  observeEvent(ignoreInit = T,input$delete_rf,{
    attr(vals$saved_data[[input$data_bank]],"rf")[input$remove_rf]<-NULL
    removeModal()
  })

  output$nb_remove<-renderUI({
    div(
      popify(bsButton("trash_nb",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"), style="button_active"),NULL,"Select and remove a nb")
    )
  })
  observeEvent(ignoreInit = T,input$trash_nb ,{
    showModal(
      modalDialog(easyClose = T,
                  title=strong("Remove NB-Attribute"),
                  div(
                    div(
                      div(style="ovenblow-y: scroll;height: 200px;ovenblow-x: scroll; padding-top: 10px",
                          checkboxGroupInput("remove_nb", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"nb")))
                      )
                    ),
                    bsButton("delete_nb",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                  )
      )
    )

  })
  observeEvent(ignoreInit = T,input$delete_nb,{
    attr(vals$saved_data[[input$data_bank]],"nb")[input$remove_nb]<-NULL
    removeModal()
  })

  output$svm_remove<-renderUI({
    div(
      popify(bsButton("trash_svm",icon(verify_fa = FALSE,name=NULL,class="fas fa-edit"), style="button_active"),NULL,"Select and remove a svm")
    )
  })
  observeEvent(ignoreInit = T,input$trash_svm ,{
    showModal(
      modalDialog(easyClose = T,
                  title=strong("Remove SVM-Attribute"),
                  div(
                    div(
                      div(style="ovesvmlow-y: scroll;height: 200px;ovesvmlow-x: scroll; padding-top: 10px",
                          checkboxGroupInput("remove_svm", NULL, choices=names(attr(vals$saved_data[[input$data_bank]],"svm")))
                      )
                    ),
                    bsButton("delete_svm",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                  )
      )
    )

  })
  observeEvent(ignoreInit = T,input$delete_svm,{
    attr(vals$saved_data[[input$data_bank]],"svm")[input$remove_svm]<-NULL
    removeModal()
  })


  getglobal_nb<-reactive({
    getglobal_model(vals$saved_data[[input$data_bank]],'nb')
  })
  output$viewnb<-renderUI({
    div(
      h4(strong("NB-Attribute"),inline(uiOutput('nb_remove'))),
      numericInput("nb_round","Round table:",value=3, step=1, min=1,width="100px"),
      column(12,style="background: white",

             div(span(
               strong("Naive Bayes models"),inline(
                 div(actionButton("down_nbtable_class",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
               )
             )),
             tags$style('#nbtab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
             tags$style('#nbtab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
             inline(DT::dataTableOutput('nbtab_class'))

      )

    )
  })
  output$nbtab_class<-DT::renderDataTable({
    vals$nbtable_class<-table<-getglobal_nb()$class
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],3)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(as.matrix(table),
                      container =container,
                      options=list(lengthMenu = list(c(-1), c("All")),
                                   rownames=T,
                                   info=FALSE,autowidth=F,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  observeEvent(ignoreInit = T,input$down_nbtable_class,{
    vals$hand_down<-"nb_stats_class"
    module_ui_downcenter("downcenter")
    mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  getglobal_rf<-reactive({
    getglobal_model(vals$saved_data[[input$data_bank]],'rf')
  })
  output$viewrf<-renderUI({
    rf_regs<-getglobal_rf()$regs
    rf_class<-getglobal_rf()$class

    div(
      h4(strong("RF-Attribute"),inline(uiOutput('rf_remove'))),
      numericInput("rf_round","Round table:",value=3, step=1, min=1,width="100px"),
      if(length(rf_class)>0){
        column(12,style="background: white",

               div(span(
                 strong("Classification models"),inline(
                   div(actionButton("down_rftable_class",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                 )
               )),

               inline(DT::dataTableOutput('rftab_class'))

        )},
      hr(),
      if(length(rf_regs)>0){
        column(12,style="background: white",
               div(span(
                 strong("Regression models"),inline(
                   div(actionButton("down_rftable_reg",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                 )
               )),

               div(DT::dataTableOutput('rftab_regs'))
        )
      }
    )
  })
  output$rftab_class<-DT::renderDataTable({
    vals$rftable_class<-table<-getglobal_rf()$class
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],input$rf_round)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(as.matrix(table),
                      container =container,
                      options=list(lengthMenu = list(c(-1), c("All")),
                                   rownames=T,
                                   info=FALSE,autoWidth=F,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  output$rftab_regs<-DT::renderDataTable({
    vals$rftable_reg<-table<-getglobal_rf()$regs
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],input$rf_round)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(table,
                      container =container,
                      options=list(lengthMenu = list(c(-1), c("All")),
                                   rownames=T,
                                   info=FALSE,autoWidth=F,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(9,13), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  observeEvent(ignoreInit = T,input$down_rftable_reg,{
    vals$hand_down<-"rf_stats_reg"
    module_ui_downcenter("downcenter")
    mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_rftable_class,{
    vals$hand_down<-"rf_stats_class"
    module_ui_downcenter("downcenter")
    mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
  })



  getglobal_svm<-reactive({
    getglobal_model(vals$saved_data[[input$data_bank]],'svm')
  })
  output$viewsvm<-renderUI({
    svm_regs<-getglobal_svm()$regs
    svm_class<-getglobal_svm()$class

    div(
      h4(strong("SVM-Attribute"),inline(uiOutput('svm_remove'))),
      numericInput("svm_round","Round table:",value=3, step=1, min=1,width="100px"),
      if(length(svm_class)>0){
        column(12,style="background: white",

               div(span(
                 strong("Classification models"),inline(
                   div(actionButton("down_svmtable_class",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                 )
               )),
               tags$style('#svmtab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               tags$style('#svmtab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               inline(DT::dataTableOutput('svmtab_class'))

        )},
      hr(),
      if(length(svm_regs)>0){
        column(12,style="background: white",
               div(span(
                 strong("Regression models"),inline(
                   div(actionButton("down_svmtable_reg",icon(verify_fa = FALSE,name=NULL,class="fas fa-download")))
                 )
               )),
               tags$style('#svmtab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               tags$style('#svmtab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               inline(DT::dataTableOutput('svmtab_regs'))
        )
      }
    )
  })
  output$svmtab_class<-DT::renderDataTable({
    vals$svmtable_class<-table<-getglobal_svm()$class
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],input$svm_round)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(as.matrix(table),
                      container =container,
                      options=list(lengthMenu = list(c(-1), c("All")),
                                   rownames=T,
                                   info=FALSE,autoWidth=F,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  output$svmtab_regs<-DT::renderDataTable({
    vals$svmtable_reg<-table<-getglobal_svm()$regs
    ncol_base<-max(attr(table,"ncol_train"))
    container<-container_global_caret(table)
    pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
    table[,pic]<-round(table[,pic],input$svm_round)
    table
    DT::formatStyle(
      DT::formatStyle(
        DT::datatable(table,
                      container =container,
                      options=list(lengthMenu = list(c(-1), c("All")),
                                   rownames=T,
                                   info=FALSE,autowidth=F,dom = 't')),
        c(1), `border-left` = "solid 1px"
      ),       c(9,13), `border-right` = "solid 1px"
    )

  }, rownames = TRUE,class ='compact cell-border')
  observeEvent(ignoreInit = T,input$down_svmtable_reg,{
    vals$hand_down<-"svm_stats_reg"
    module_ui_downcenter("downcenter")
    mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_svmtable_class,{
    vals$hand_down<-"svm_stats_class"
    module_ui_downcenter("downcenter")
    mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
  })


  observeEvent(ignoreInit = T,input$delete_coords,{
    showModal(
      modalDialog(
        title="Are you sure?",
        column(12, style="margin-top: 20px; margin-bottom: 20px",
               h5(strong("Remove coords from",style="color: red"),span("Datalist:"),em(input$data_bank,style="color: gray")),
               bsButton("delete_coords_yes",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Confirm", style="button_active",block=T))
        ,
        easyClose = T,
        size = "s"

      )
    )
  })
  observeEvent(ignoreInit = T,input$delete_coords_yes,{
    attr(vals$saved_data[[input$data_bank]],"coords")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="coords")


  })
  output$add_coords_button<-renderUI({
    req(length(input$add_coords_file$datapath)>0)
    actionButton("add_coords",icon=icon(verify_fa = FALSE,name=NULL,class="fas fa-arrow-right"),
                 strong(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-warehouse"),"Click to save to the Datalist"),
                        "Add", style="button_active"))
  })
  output$add_coords_intru<-renderUI({
    column(12,
           column(12,style="margin-top: 20px",
                  strong("1. Use the button below to upload one.")),
           if(length(input$add_coords_file$datapath)>0){
             fluidRow(
               column(12,style="margin-top: 20px",
                      strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_bank, style="color: SeaGreen"))),
               column(12,
                      uiOutput("error_coords"))
             )
           }
    )
  })
  output$add_base_button<-renderUI({
    req(length(input$add_base_file$datapath)>0)
    actionButton("add_base",icon=icon(verify_fa = FALSE,name=NULL,class="fas fa-arrow-right"),
                 strong(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-warehouse"),"Click to save to the Datalist"),
                        "Add", style="button_active"))
  })
  output$add_base_intru<-renderUI({
    column(12,
           column(12,style="margin-top: 20px",
                  strong("1. Use the button below to upload one.")),
           if(length(input$add_base_file$datapath)>0){
             column(12,style="margin-top: 20px",
                    strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_bank, style="color: SeaGreen")))
           }
    )
  })
  output$add_layer_button<-renderUI({
    req(length(input$add_layer_file$datapath)>0)
    actionButton("add_layer",icon=icon(verify_fa = FALSE,name=NULL,class="fas fa-arrow-right"),
                 strong(tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-warehouse"),"Click to save to the Datalist"),
                        "Add", style="button_active"))
  })
  output$add_layer_intru<-renderUI({
    column(12,
           column(12,style="margin-top: 20px",
                  strong("1. Use the button below to upload one.")),
           if(length(input$add_layer_file$datapath)>0){
             column(12,style="margin-top: 20px",
                    strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_bank, style="color: SeaGreen")))
           }

    )
  })
  observeEvent(ignoreInit = T,input$delete_base_shape,{
    showModal(
      modalDialog(
        title="Are you sure?",
        column(12, style="margin-top: 20px; margin-bottom: 20px",
               h5(strong("Remove base_shape from",style="color: red"),span("Datalist:"),em(input$data_bank,style="color: gray")),
               bsButton("delete_base_shape_yes",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Confirm", style="button_active",block=T)),
        easyClose = T,
        size = "s"

      )
    )
  })
  observeEvent(ignoreInit = T,input$delete_base_shape_yes,{
    attr(vals$saved_data[[input$data_bank]],"base_shape")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  observeEvent(ignoreInit = T,input$delete_layer_shape,{
    showModal(
      modalDialog(
        title="Are you sure?",
        column(12, style="margin-top: 20px; margin-bottom: 20px",
               h5(strong("Remove layer_shape from",style="color: red"),span("Datalist:"),em(input$data_bank,style="color: gray")),
               bsButton("delete_layer_shape_yes",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Confirm", style="button_active",block=T)),
        easyClose = T,
        size = "s"

      )
    )
  })
  observeEvent(ignoreInit = T,input$delete_layer_shape_yes,{
    attr(vals$saved_data[[input$data_bank]],"layer_shape")<-NULL
    removeModal()
    updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
  })
  observeEvent(ignoreInit = T,input$delete_som,{
    showModal(
      modalDialog(
        title="Are you sure?",
        column(12, style="margin-top: 20px; margin-bottom: 20px",
               h5(strong("Remove som",style="color: red"),em(paste0(input$remove_som,collapse="; "),style="color: gray")),
               bsButton("delete_som_yes",icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Confirm", style="button_active",block=T)),
        easyClose = T,
        size = "s"

      )
    )
  })
  observeEvent(ignoreInit = T,input$delete_som_yes,{
    attr(vals$saved_data[[input$data_bank]],"som")[input$remove_som]<-NULL
    removeModal()

  })
  observeEvent(ignoreInit = T,input$add_base,{

    t<-try({get(gsub(" ", "", capture.output(
      load(input$add_base_file$datapath, verbose = T)
    )[2]))})
    if("try-error" %in% class(t)) {t<-readRDS(input$add_base_file$datapath) }

    attr(vals$saved_data[[input$data_bank]],"base_shape")<-t
    updateRadioGroupButtons(
      session,"view_datalist", selected="extra_shape"
    )

  })
  observeEvent(ignoreInit = T,input$add_layer,{

    t<-try({get(gsub(" ", "", capture.output(
      load(input$add_layer_file$datapath, verbose = T)
    )[2]))})
    if("try-error" %in% class(t)) {t<-readRDS(input$add_layer_file$datapath) }
    attr(vals$saved_data[[input$data_bank]],"layer_shape")<-t

    updateRadioGroupButtons(
      session,"view_datalist", selected="extra_shape"
    )
    t
  })
  observeEvent(ignoreInit = T,input$add_coords_file,{
    output$error_coords<-renderUI("bank compleated")
  })
  observeEvent(ignoreInit = T,input$add_coords,{

    coords<-data.frame(fread(input$add_coords_file$datapath))
    rownames(coords)<-coords[, 1]
    coords[, 1]<-NULL
    if(ncol(coords)!=2){ output$error_coords<-
      renderUI({
        column(12, strong(
          "Invalid Entries. The first column must contain the name of the observations. The second and third columns must contain the logitude and latitude respectively", style="color: red"))
      })}

    if(any(rownames(coords)%in%rownames(vals$saved_data[[input$data_bank]]))==F) {
      output$error_coords<-
        renderUI({
          column(12, strong(
            "None of the IDs of the banked coordinates are compatible with the ids of the selected datalist. Please bank coodinates with valid IDs", style="color: red"))
        })
    }

    req(any(rownames(coords)%in%rownames(vals$saved_data[[input$data_bank]])))

    attr(vals$saved_data[[input$data_bank]],"coords")<-na.omit(
      coords[rownames(vals$saved_data[[input$data_bank]]),]
    )
    updateRadioGroupButtons(
      session,"view_datalist", selected="coords"
    )

  })



  observeEvent(ignoreInit = T,input$DT_factors_cell_edit, {
    factors<-attr(vals$saved_data[[input$data_bank]],"factors")
    row <-input$DT_factors_cell_edit$row
    clmn<-input$DT_factors_cell_edit$col
    value<-input$DT_factors_cell_edit$value
    if(!value%in%levels(factors[,clmn])){
      levels(factors[,clmn])<-c(levels(factors[,clmn]), value)
      factors[row, clmn]<-value
      attr(vals$saved_data[[input$data_bank]],"factors")<-factors

    } else{
      attr(vals$saved_data[[input$data_bank]],"factors")[row, clmn]<-input$DT_factors_cell_edit$value
    }

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
  savecodebook<-reactive({
    req(input$hand_save)
    data<-getdata_hc()
    m<-getmodel_hc()
    codes<-data.frame(do.call(cbind,m$codes))
    somC<-cutsom.reactive()
    factors<-data.frame(somC$som.hc)
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
      #  vals$cur_data<-input$data_newname
    } else{
      req(input$codebook_over)

      vals$saved_data[[input$codebook_over]]<-temp
      # vals$cur_data<-input$data_over

    }
    vals$new_facts<-NULL
    #runjs("Shiny.setInputValue('last_btn', false);")
    status_changes$df<-c(T,T)
    data_cogs$df<-temp
    shinyjs::reset("change_head")

    runjs("Shiny.setInputValue('last_btn', 'fade');")




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

           "Save interpolation model"=saveinterp(),
           "Save discrete model"=savediscrete(),
           "Save raster"=saveinterp(),
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
  bag_mapname<-reactive({
    bag<-1
    name0<-input$var_map
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
  saveinterp<-reactive({
    req(input$interps_newname)
    p<-vals$map_res
    if(input$hand_save=="create"){
      cur<-input$interps_newname
      vals$saved_maps[[cur]]<-p
      vals$cur_saved_maps<-cur
    } else{
      cur<-input$interps_over
      vals$saved_maps[[cur]]<-p
      vals$cur_saved_maps<-cur
    }
    updatePickerInput(session,"saved_maps",selected=cur, choices=names(vals$saved_maps))

  })
  savediscrete<-reactive({
    req(input$discrete_newname)
    p<-vals$map_res
    if(input$hand_save=="create"){
      cur<-input$discrete_newname
      vals$saved_maps[[cur]]<-p
      vals$cur_saved_maps<-cur
    } else{
      cur<-input$discrete_over
      vals$saved_maps[[cur]]<-p
      vals$cur_saved_maps<-cur
    }
    updatePickerInput(session,"saved_maps",selected=cur, choices=names(vals$saved_maps))

  })

  addlayer<-reactive({
    if(is.null(attr(vals$saved_data[[input$data_map]],"extra_shape"))){
      attr(vals$saved_data[[input$data_map]],"extra_shape")<-list()
    }
    if(input$hand_save=="create"){
      attr(vals$saved_data[[input$data_map]],"extra_shape")[[input$extra_layer_newname]]<-filtershp()} else{
        attr(vals$saved_data[[input$data_map]],"extra_shape")[[input$extra_layer_over]]<-filtershp()
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
  hand_save_out<-reactive({
    modalDialog()
  })
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
  bag_mp<-reactive({

    args<-vals[[ns_tab5("somplot_args")]]
    name0<-paste0('HC',vals$customKdata)
    if(length(input$fixname)>0){
      if(isTRUE(input$fixname)){
        name0<-paste0(input$data_hc,'_HC', args$newdata)
      }
    }

    data<-attr(vals$saved_data[[args$newdata]],"factors")
    name1<-make.unique(c(colnames(data),name0), sep="_")
    name1[ncol(data)+1]


  })
  output$nointerpsmodel<-renderPrint({    cat("no saved map to overwritte")  })

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
  observeEvent(ignoreInit = T,input$shp_back,{
    insert_modal()
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
              fileInput(inputId = "filedata",label = NULL,accept = c(".csv",".xlsx"), placeholder=if(length(input$filedata$datapath)>0){input$filedata$name})
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
            require(readxl)
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
            require(readxl)
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
            require(readxl)
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

    data <-data.frame(fread("inst/app/www/envi_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
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
  #observe({


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
    delay(500,{
      menu_steps$df<-newvals$cur_tab
      menu_steps$num<-1
      position(1)
      menu_steps$cur<-NULL})
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

    delay(500,{
      menu_steps$df<-newvals$cur_tab
      menu_steps$num<-1
      position(1)
      menu_steps$cur<-NULL})

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

  observeEvent(ignoreInit = T,input$var_map,{
    vals$cur_var_map<-input$var_map
  })
  observeEvent(ignoreInit = T,input$var_map,{
    vals$cur_var_map<-input$var_map
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
  choices_hc2<-reactive({
    req(input$data_hc)
    a<-if (length(   names(vals$saved_data) > 0)) {
      "Data"
    } else {
      NULL
    }

    b<-   if(length(attr(vals$saved_data[[input$data_hc]],"som"))>0){"Som codebook"}else{NULL}
    res<-c(a, b)
    res
  })
  observeEvent(ignoreInit = T,input$desc_options,{
    vals$bag_submenu<-switch(input$desc_options,
                             'Boxplot'="boxplot",
                             'MDS'="mds",
                             'RDA'="rda",
                             'PCA'="pca")
  })
  border_alert<-reactive({
    if(sum(vals$bagdata==c(T,T))==2){col="gray"} else{ col="#0093fcff"}
    style<-paste("border: 3px solid",col)
    style
  })
  observeEvent(ignoreInit = T,input$summ_options,
               {vals$curview_summ_options<-input$summ_options})

  output$stats_cpca<-renderUI({
    column(12,style="background: white",
           p(strong("Principal Component Analysis"))

    )
  })
  output$stats_cmds<-renderUI({
    column(12,style="background: white",
           p(strong("Nonmetric Multidimensional Scaling "))
    )
  })


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
  output$shp_back<-renderUI({
    req(vals$shptool=="upload")
    actionButton("shp_back","back")
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

  observeEvent(ignoreInit = T,input$ddcogs2,{
    vals$hand_down<-"Data-Attribute"
    vals$data_bank_data<-vals$saved_data[[input$data_bank]]
    module_ui_downcenter("downcenter")
    mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals,name=paste0(input$data_bank,"_","Numeric"))
  })

  observeEvent(ignoreInit = T,input$dfcogs2,{
    vals$hand_down<-"Factor-Attribute"
    vals$data_bank_data<-attr(vals$saved_data[[input$data_bank]],"factors")
    module_ui_downcenter("downcenter")
    mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals,name=paste0(input$data_bank,"_","Factors"))
  })

  observeEvent(ignoreInit = T,input$downcenter_coords,{
    vals$hand_down<-"Coords-Attribute"
    vals$data_bank_data<-attr(vals$saved_data[[input$data_bank]],"coords")
    module_ui_downcenter("downcenter")
    mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals,name=paste0(input$data_bank,"_","Coords"))
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
      data <-data.frame(fread("inst/app/www/nema_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
      #data <-data.frame(fread("DADOS SANSED_OFICIAL CENPES.csv", stringsAsFactors = T))
      rownames(data)<-data[, 1]
      data[, 1]<-NULL


    } else {
      # input<-list()
      # input$filedata$datapath<-"data.xlsx"
      # input$sheet_data<-"data"
      validate(need(length(input$filedata$datapath)!=0,"Data file is required"))
      if(length(grep(".xlsx",input$filedata$datapath))>0) {
        require(readxl)
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
          require(readxl)
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
        labels <-data.frame(fread("inst/app/www/factors_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
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
    if (input$up_or_ex == 'use example data') {
      coords <-data.frame(fread("inst/app/www/coords_araca.csv"))
      rownames(coords)<-coords[, 1]
      coords[, 1]<-NULL
    } else {
      #input<-list()
      #input$coords$datapath<-'data - Copia.xlsx'
      #input$sheet_fac<-1
      if(length(input$coords$datapath)>0){
        if(length(grep(".xlsx",input$coords$datapath))>0) {
          require(readxl)
          df<-read_excel(input$coords$datapath, sheet = input$sheet_coord,na=c("","NA"))
          data<-data.frame(df)
          cnames<-iconv(colnames(df), from = 'UTF-8', to = 'ASCII//TRANSLIT')
          colnames(data)<-cnames
          coords<-data
          rownames(coords)<-coords[, 1]
          coords[, 1]<-NULL
        } else{
          coords<-data.frame(fread(input$coords$datapath))
          rownames(coords)<-coords[, 1]
          coords[, 1]<-NULL
        }


      } else{coords=NULL}}

    #coords<-read.csv("04_coords.csv",sep=";", row.names=1)
    coords
  })
  base_shape<-reactive({
    if (input$up_or_ex == 'use example data') {
      get(gsub(" ","",capture.output(load("inst/app/www/base_shape_araca",verbose=T))[2]))
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
      get(gsub(" ","",capture.output(load("inst/app/www/layer_shape_araca",verbose=T))[2]))
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
      envi <-data.frame(fread("inst/app/www/envi_araca.csv", stringsAsFactors = T,na.strings=c("","NA")))
      rownames(envi)<-envi[, 1]
      envi[, 1]<-NULL
      envi<-data_migrate(datalist[[1]],envi,"envi_araca")
      vals$saved_data[[length(vals$saved_data)+1]]<-envi
      names(vals$saved_data)[[length(vals$saved_data)]]<-"envi_araca"
    }
    names(vals$saved_data)<-make.unique(names(vals$saved_data))


    removeModal()

    updateSelectInput(session,"data_bank",selected=names(vals$saved_data)[[length(vals$saved_data)]])
    vals$cur_data<-input$data_name
    if(input$tabs=="menu_intro"){
      delay(500,    updateTextInput(session,"tabs",value="menu_upload"))
      position(1)
    }
    runjs("Shiny.setInputValue('last_btn', 'fade');")

  })
  output$filterdata<-renderUI({
    filterdata()
  })




  output$rfdata<-renderUI({
    column(
      12,
      br(),
      selectInput(
        "rfdata",
        "Select the data for the Random Forest analysis",
        choices =    names(vals$saved_data)
      )
    )
  })
  cutdata.reactive<-reactive({
    req(input$customKdata)
    req(input$method.hc0)
    req(input$disthc)
    req(input$hcdata_palette)
    m<-getdata_hc()
    #m<-getmodel_hc()
    #savereac()
    args<-list(m=m, k= input$customKdata,hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc)
    somC<-do.call(cutsom_new,args)
    names(somC$somC)<-rownames(m)
    somC$somC<-factor(somC$somC,levels=unique(somC$somC))
    somC$som.hc<- somC$somC
    somC
  })






  output$hcut_labels<-renderUI({
    req(input$model_or_data)
    req(input$model_or_data=="data")
    data<-getdata_hc()
    choices<-c(colnames(attr(data,"factors")))
    pickerInput("hcut_labels","+ Factor",
                choices = c("rownames",choices),selected=vals$hcut_labels)



  })

  observeEvent(input$hcut_labels,{
    vals$hcut_labels<-input$hcut_labels
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


  # pred<-attr(vals$saved_data[[input$data_map]],"predictions")
  #d<-if(length(pred)>0){"predictions"} else {NULL}
  observeEvent(ignoreInit = T,input$scale_map,{
    if(isFALSE(input$scale_map)){

      updateCheckboxInput(session,"scalesize_size","Size", F)
      updateCheckboxInput(session,"scalesize_color","Color", F)
    }
  })
  scalesize_size<-reactive({

    res<-if(isFALSE(input$scale_map)){F} else{input$scalesize_size}
  })
  scalesize_color<-reactive({
    if(isFALSE(input$scale_map)| isTRUE(input$colored_map)){F} else{input$scalesize_color}
  })
  l1<-reactive({
    list(
      data_map=input$data_map,
      choices_map=input$choices_map,
      var_map= input$var_map,
      showfactors= input$showfactors,
      labels_coords= input$labels_coords,
      colored_map= input$colored_map,
      map_lab= input$map_lab,
      map_1a_base= input$map_1a_base,
      map_1f_layer= input$map_1f_layer,
      showcoords= input$showcoords,
      pt_points=input$pt_points+6,
      pt_coords=input$pt_coords+1,
      pt_factor= input$pt_factor+2,
      col_factor= if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
      pt_legend=input$pt_legend,
      pt_legend= input$pt_legend,
      map_legend=input$map_legend,
      col_coords= if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
      pt_palette= input$pt_palette,
      pt_symbol=  input$pt_symbol,
      scale_map=input$scale_map,
      scalesize_size= input$scalesize_size,
      scalesize_color=input$scalesize_color,
      pt_factor=input$pt_factor,
      showguides=input$showguides,
      long_xmin=input$long_xmin,
      lat_xmin=input$lat_xmin,
      long_xmax=input$long_xmax,
      lat_xmax= input$lat_xmax,
      var_map=input$var_map,
      res_map=input$res_map,
      nmax=input$nmax,
      idp=input$idp,
      cex.main=input$pt_titlemap
    )
  })
  observeEvent(ignoreInit = T,input$edit_map,{

    req(length(vals$saved_maps)>0)
    req(isTRUE(input$edit_map))

    p<-vals$saved_maps[[input$saved_maps]]
    l<-attr(p,"args")

    for(i in names(l)){
      try(updateTextInput(session,i, value=l[[i]]))
    }
  })
  idw_reac<-reactive({
    req(length(input$maxdist)>0)
    maxdist=if(is.na(input$maxdist)){Inf}else{as.numeric(input$maxdist)}
    nmax=if(is.na(input$nmax_idp)){Inf}else{as.numeric(input$nmax_idp)}

    c(maxdist,nmax)
  })
  observeEvent(ignoreInit = T,input$maxdist,{

    if(!is.na(input$maxdist)){
      if(is.na(input$nmax_idp)){
        updateNumericInput(session,"nmax_idp", value=vals$nmax_idp)
      }
    }
  })



  getmap_scale<-reactive({
    list( scalesize_size= scalesize_size(),
          scalesize_color=scalesize_color())
  })

  map_data_disc_args<-reactive({
    req(input$long_xmin)
    req(input$lat_xmin)
    req(input$long_xmax)
    req(input$lat_xmax)
    req(!is.null(vals$map_title))
    get<-vals$vars_data[which(vals$vars_data==input$var_map)]
    data<-getdata_map()
    coords<-attr(data,'coords')
    data<-data[filtermap(),,drop=F]
    req(nrow(data)>0)
    mapscale<-getmap_scale()
    scalesize_size=mapscale$scalesize_size
    scalesize_color=mapscale$scalesize_color
    req(isTRUE(scalesize_size)|isFALSE(scalesize_size))
    req(isTRUE(scalesize_color)|isFALSE(scalesize_color))
    req(get%in%colnames(data))

    data_go<-na.omit(data[,get,drop=F])



    colored_by_factor<-
      if(isTRUE(input$colored_map)){
        attr(data,"factors")[filtermap(),as.character(input$map_lab),drop=F]

      } else {NULL}
    req(input$breaks_map)
    if(!isTRUE(input$stack_scatter_3d)){
      mybreaks<-as.numeric(unlist(strsplit(input$breaks_map,",")))
    }else{
      mybreaks<-NULL
    }

    pizza_fac<-NULL
    if(isTRUE(input$pizza_map)){
      factors<-attr(vals$saved_data[[input$data_map]],"factors")
      pizza_fac<-factors[filtermap(),input$pizza_fac]
    }

    args<-
      list(

        data = data_go,
        coords = coords[rownames(data_go),],
        base_shape = if(isTRUE(input$map_1a_base)){
          attr(data,"base_shape")
        },
        layer_shape =   if(isTRUE(input$map_1f_layer)){
          attr(data,"layer_shape")
        } else{ NULL},
        get = get,
        main = input$map_title,
        factors=labcoords(),
        showcoords=input$showcoords,
        cex.pt = input$pt_points+6,
        cexmin.pt=input$pt_points_min,
        cex.coords=input$pt_coords+1,
        cex.fac=input$pt_factor+2,
        col.fac=input$col_factor,
        cex.axes=input$pt_legend,
        cex.leg=input$cex.key,
        cex.lab=input$pt_legend,
        leg=input$map_legend,
        col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
        col.palette=input$pt_palette,
        symbol=as.numeric(input$pt_symbol),
        scalesize_size= scalesize_size,
        scalesize_color=scalesize_color,
        points=T,
        as_factor=F,
        bmu=F,
        colored_by_factor=  colored_by_factor,
        showguides=input$showguides,
        limits= cbind(
          c(input$long_xmin,  input$lat_xmin),
          c(input$long_xmax,  input$lat_xmax)
        ),
        layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
        lighten=input$layer_lighten,
        base_col=getcolhabs(vals$newcolhabs,input$base_col,1),
        base_lighten=input$base_lighten,
        newcolhabs=vals$newcolhabs,
        extralayers=extralayers(),
        data_depth=input$data_depth,
        breaks_len=input$breaks_len,
        mybreaks=mybreaks,
        key.height=input$key.height,
        keyscale=input$keyscale,
        width_hint=input$scabarsize,
        cex_scabar=input$scabartextsize,
        layer_shape_border=getcolhabs(vals$newcolhabs,input$layer_col_border,1),
        base_shape_border= getcolhabs(vals$newcolhabs,input$base_col_border,1),
        cex.main=input$pt_titlemap,
        pie=input$pizza_map,
        facpizza=pizza_fac

      )
    args
  })


  # args<-readRDS("args.rds")
  map_data_disc<-reactive({
    #req(isFALSE(stopmap$df))

    args<-map_data_disc_args()


    m<-do.call(map_discrete_variable,args)
    attr(m,"args")<-l1()
    vals$map_res<-m

  })
  extralayers<-reactive({
    shapes<-attr(vals$saved_data[[input$data_map]],"extra_shape")
    if(length(names(shapes))>0){
      trulayers<-c()
      for(i in 1:length(shapes)){
        trulayers[i]<-input[[paste0("map_extra_layer",i)]]
      }
      req(is.logical(trulayers))

      layers<-list()
      colors=c()
      alphas=c()
      labels<-c()
      sizes<-c()
      for(i in 1:length(shapes))
      {
        layers[[i]]<-attr(vals$saved_data[[input$data_map]],"extra_shape")[[i]]
        colors[i]<-input[[paste0("ssextra_layer_col",i)]]
        alphas[i]<-input[[paste0("ssextra_layer_lighten",i)]]
        labels[i]<-input[[paste0("feat_extra",i)]]
        sizes[i]<-input[[paste0("feat_extra_size",i)]]

      }
      if(length(colors[which(trulayers)])>0){

        extralayers<-list(colors=colors[which(trulayers)],alphas=alphas[which(trulayers)], layers=layers[which(trulayers)], labels=labels[which(trulayers)], sizes=sizes[which(trulayers)])}else{     extralayers<-NULL}



    } else{
      extralayers<-NULL
    }

    extralayers
  })






  labcoords<-reactive({
    labcoords<-if(!isTRUE(input$showfactors)){NULL} else{
      attr(getdata_map(),"factors")[filtermap(),as.character(input$labels_coords)]
    }
    labcoords
  })
  model_idw_data<-reactive({
    #req(isFALSE(stopmap$df))
    maxdist=idw_reac()[1]

    nmax=idw_reac()[2]
    get<-vals$vars_data[which(vals$vars_data==input$var_map)]
    data<-getdata_map()[filtermap(),,drop=F]
    coords<-attr(data,"coords")[rownames(data),]

    base_shape = if(length(input$map_1a_base)>0){
      if(isTRUE(input$map_1a_base)){attr(data,"base_shape")
      } else{NULL}
    }else{NULL}

    req(get%in%colnames(data))


    args<-list(
      data=data,
      get=get,
      coords=coords,
      base_shape=base_shape,
      layer_shape=NULL,
      res=input$res_map,
      k=input$nmax,
      idp=input$idp,
      limits=as.matrix( cbind(
        c(input$long_xmin,  input$lat_xmin),
        c(input$long_xmax,  input$lat_xmax)
      )),
      nmax=nmax,
      nmin=as.numeric(input$nmin_idp),
      omax=as.numeric(input$omax),
      maxdist=maxdist

    )


    p<-Map_model(data=data,
                 get=get,
                 coords=coords,
                 base_shape=base_shape,
                 layer_shape=NULL,
                 res=input$res_map,
                 k=input$nmax,
                 idp=input$idp,
                 limits=as.matrix( cbind(
                   c(input$long_xmin,  input$lat_xmin),
                   c(input$long_xmax,  input$lat_xmax)
                 )),
                 nmax=nmax,
                 nmin=as.numeric(input$nmin_idp),
                 omax=as.numeric(input$omax),
                 maxdist=maxdist
    )


    vals$map_res<-p
    p
  })
  map_data_interp<-reactive({

    p0<-p<-model_idw_data()

    get<-vals$vars_data[which(vals$vars_data==input$var_map)]
    data<-getdata_map()[filtermap(),,drop=F]
    coords<-attr(data,"coords")[rownames(data),]
    layer_shape =if(isTRUE(input$map_1f_layer)){attr(data,"layer_shape") } else { NULL}
    mybreaks<-as.numeric(unlist(strsplit(input$breaks_map,",")))
    p<-map_style(data=data,
                 get=get,
                 coords=coords,
                 p=p,
                 main = input$map_title,
                 subtitle="",
                 cex.axes=input$pt_legend,
                 cex.lab=input$pt_legend,
                 cex.main=input$pt_titlemap,
                 cex.sub=14,
                 cex.leg=input$cex.key,
                 factors=labcoords(),
                 cex.fac=input$pt_factor+2,
                 col.fac=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                 showcoords=input$showcoords,
                 col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
                 cex.coords=input$pt_coords+1,
                 showguides=input$showguides,
                 layer_shape=layer_shape,
                 col.palette=input$pt_palette,
                 layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
                 lighten=input$layer_lighten,
                 leg=input$map_legend,
                 newcolhabs=vals$newcolhabs,
                 extralayers=extralayers(),
                 layer_shape_border=getcolhabs(vals$newcolhabs,input$layer_col_border,1),
                 breaks=mybreaks,
                 key.height=input$key.height,
                 keyscale=input$keyscale,  width_hint=input$scabarsize,cex_scabar=input$scabartextsize
    )
    attr(p,"args")<-l1()
    attr(p,"limits")<-attr(p0,"limits")
    attr(p,"base_shape")<-attr(p0,"base_shape")
    attr(p,"layer_shape")<-attr(p0,"layer_shape")
    attr(p,"interp_model")<-attr(p0,"interp_model")
    my_rst<-attr(p0,"my_rst")
    attr(my_rst,"col.palette")<-input$pt_palette
    attr(p,"my_rst")<-my_rst
    attr(p,"data_z")<-attr(p0,"data_z")
    vals$map_res<-p

  })
  model_knn_data<-reactive({
    #req(isFALSE(stopmap$df))
    data <-attr(getdata_map(),"factors")[filtermap(),,drop=F]
    get<-vals$vars_fac[which(vals$vars_fac==input$var_map)]
    coords<-attr(getdata_map(),"coords")[rownames(data),]
    base_shape =if(isTRUE(input$map_1a_base)){attr(getdata_map(),"base_shape") } else { NULL}
    req(get%in%colnames(data))
    p<-Map_model(data=data,
                 get=get,
                 coords=coords,
                 base_shape=base_shape,
                 layer_shape=NULL,
                 res=input$res_map,
                 k=input$nmax,
                 idp=input$idp,
                 limits=as.matrix( cbind(
                   c(input$long_xmin,  input$lat_xmin),
                   c(input$long_xmax,  input$lat_xmax)
                 ))
    )


    vals$map_res<-p
    p
  })
  map_fac_interp<-reactive({
    #req(isFALSE(stopmap$df))
    p0<-p<-model_knn_data()
    data <-attr(getdata_map(),"factors")[filtermap(),,drop=F]
    get<-vals$vars_fac[which(vals$vars_fac==input$var_map)]
    coords<-attr(getdata_map(),"coords")[rownames(data),]
    layer_shape =if(isTRUE(input$map_1f_layer)){attr(getdata_map(),"layer_shape") } else { NULL}
    p<-map_style(data=data,
                 get=get,
                 coords=coords,
                 p=p,
                 main = input$map_title,
                 subtitle="",
                 cex.axes=input$pt_legend,
                 cex.lab=input$pt_legend,
                 cex.main=input$pt_titlemap,
                 cex.sub=14,
                 cex.leg=input$cex.key,
                 factors=labcoords(),
                 cex.fac=input$pt_factor+2,
                 col.fac=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                 col.palette=input$pt_palette,
                 showcoords=input$showcoords,
                 col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
                 cex.coords=input$pt_coords+1,
                 showguides=input$showguides,
                 layer_shape=layer_shape,
                 layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
                 lighten=input$layer_lighten,
                 leg=input$map_legend,
                 newcolhabs=vals$newcolhabs,
                 extralayers=extralayers(),
                 layer_shape_border=getcolhabs(vals$newcolhabs,input$layer_col_border,1),
                 key.height=input$key.height,
                 keyscale=input$keyscale,  width_hint=input$scabarsize,cex_scabar=input$scabartextsize
    )
    attr(p,"args")<-l1()
    attr(p,"limits")<-attr(p0,"limits")
    attr(p,"base_shape")<-attr(p0,"base_shape")
    attr(p,"layer_shape")<-attr(p0,"layer_shape")
    attr(p,"interp_model")<-attr(p0,"interp_model")
    my_rst<-attr(p0,"my_rst")
    attr(my_rst,"col.palette")<-isolate(input$pt_palette)
    attr(p,"my_rst")<-my_rst
    attr(p,"data_z")<-attr(p0,"data_z")
    vals$map_res<-p

  })
  data_raster<-reactive({
    get<-vals$vars_data[which(vals$vars_data==input$var_map)]
    data<-getdata_map()[filtermap(),,drop=F]
    coords<-attr(data,"coords")[rownames(data),]
    base_shape =if(isTRUE(input$map_1a_base)){attr(data,"base_shape") } else { NULL}
    limits<-as.matrix( cbind(
      c(input$long_xmin,  input$lat_xmin),
      c(input$long_xmax,  input$lat_xmax)
    ))
    req(get%in%colnames(data))
    p<-mapraster(data,get,coords, base_shape,limits)
    vals$map_res<-p
    p
  })
  map_raster<-reactive({
    validate(need(input$choices_map=="Numeric-Attribute","This functionality is currently only available for Numeric-Attribute"))

    p0<-p<-data_raster()


    get<-vals$vars_data[which(vals$vars_data==input$var_map)]
    data<-getdata_map()[filtermap(),,drop=F]
    coords<-attr(data,"coords")[rownames(data),]
    layer_shape =if(isTRUE(input$map_1f_layer)){attr(data,"layer_shape") } else { NULL}
    mybreaks<-as.numeric(unlist(strsplit(input$breaks_map,",")))
    p<-map_style(data=data,
                 get=get,
                 coords=coords,
                 p=p,
                 main = input$map_title,
                 subtitle="",
                 cex.axes=input$pt_legend,
                 cex.lab=input$pt_legend,
                 cex.main=input$pt_titlemap,
                 cex.sub=14,
                 cex.leg=input$cex.key,
                 factors=labcoords(),
                 cex.fac=input$pt_factor+2,
                 col.fac=if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL},
                 showcoords=input$showcoords,
                 col.coords=if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL},
                 cex.coords=input$pt_coords+1,
                 showguides=input$showguides,
                 layer_shape=layer_shape,
                 col.palette=input$pt_palette,
                 layer_col=getcolhabs(vals$newcolhabs,input$layer_col,1),
                 lighten=input$layer_lighten,
                 leg=input$map_legend,
                 newcolhabs=vals$newcolhabs,
                 extralayers=extralayers(),
                 layer_shape_border=getcolhabs(vals$newcolhabs,input$layer_col_border,1),
                 breaks=mybreaks,
                 key.height=input$key.height,
                 keyscale=input$keyscale,  width_hint=input$scabarsize,cex_scabar=input$scabartextsize
    )
    attr(p,"args")<-l1()
    attr(p,"limits")<-attr(p0,"limits")
    attr(p,"base_shape")<-attr(p0,"base_shape")
    attr(p,"layer_shape")<-attr(p0,"layer_shape")
    my_rst<-attr(p0,"my_rst")
    attr(my_rst,"col.palette")<-input$pt_palette
    attr(p,"my_rst")<-my_rst
    attr(p,"data_z")<-attr(p0,"data_z")


    vals$map_res<-p

  })

  output$persp_out<-renderUI({
    req(input$saved_maps!="new map")
    req(isTRUE(input$surface_map))
    req(input$surf_d)
    validate(need(input$surf_d>0, "error: requires persp strength >0 "))

    p1<-vals$saved_maps[[input$saved_maps]]

    zlab<-attr(p1,'args')['var_map']
    p1<- attr(p1,"my_rst")
    p2<-if(length(vals$saved_maps)>1){
      attr(vals$saved_maps[[input$saved_maps2]],"my_rst")
    } else{NULL}

    fluidRow(
      renderPlot({
        my_rst<-p1
        data_z<- attr(p,"data_z")

        if(is.factor(data_z)){
          colors<- getcolhabs(vals$newcolhabs,input$pt_palette,nlevels(data_z))
        } else {
          colors<- getcolhabs(vals$newcolhabs,input$pt_palette,length(my_rst@data@values))
        }

        my_rst2=p2
        pmat<-get_4D(my_rst,my_rst2=my_rst2,colors, theta=input$surf_theta, phi=input$surf_phi,r=input$surf_r,d=input$surf_d,xlab="long", ylab="Latitude", zlab=zlab, exp=input$surf_exp,wlegend=input$surface_width.legend,hlegend=input$surface_height.legend,tictype=input$surf_tick)
        if(isTRUE(input$showcoords)){
          coords<-attr(getdata_map(),"coords")
          labels<-attr(getdata_map(),"factors")[rownames(coords),input$labels_coords]
          xx<-coords[,1]
          yy<-coords[,2]
          zz<-rep(input$z_coords, nrow(coords))
          mypoints<-trans3d(xx,yy,zz,pmat = pmat)
          points(mypoints,pch = 3,col = if(length(input$col_coords)>0){   getcolhabs(vals$newcolhabs,input$col_coords,1) } else{ NULL}, cex=input$pt_coords)
        }
        if(isTRUE(input$showfactors)){
          coords<-attr(getdata_map(),"coords")
          labels<-attr(getdata_map(),"factors")[rownames(coords),input$labels_coords]
          xx<-coords[,1]
          yy<-coords[,2]
          zz<-rep(input$z_labels, nrow(coords))
          mypoints<-trans3d(xx,yy,zz,pmat = pmat)
          text(mypoints,labels=labels,pch = 3,col = if(length(input$col_factor)>0){   getcolhabs(vals$newcolhabs,input$col_factor,1) } else{ NULL}, cex=input$pt_factor)
        }
        if(isTRUE(input$showguides)){
          grid4D(coords,my_rst, col= getcolhabs(vals$newcolhabs,input$col_guides,1), pmat=pmat,lty=as.numeric(input$lty_guides),lwd=input$lwd_guides)
        }
        persp<-recordPlot()
        attr(persp,"coarser")<-attr(pmat,"coarser")
        vals$map_res<-persp

      })
    )



  })
  getmap<-reactive({
    if(input$cmap=='discrete'){
      if(input$choices_map=="Numeric-Attribute"){  p<-vals$map_res}
      if(input$choices_map=="Factor-Attribute"){  p<-vals$map_res}

    }

    if(input$cmap=='interpolation') {
      if(input$choices_map=="Numeric-Attribute"){  p<-vals$map_res}
      if(input$choices_map=="Factor-Attribute"){  p<-vals$map_res}
    }

    if(input$cmap=='raster'){  p<-vals$map_res}
    suppressWarnings(print(p))
  })


  get_stackmap<-reactive({
    req(input$stack_d)
    validate(need(input$stack_d>0, "error: requires persp strength >0 "))
    rlist<-get_stacklist()[input$stack_layers]
    res_list<-get_stacklist()[input$stack_layers]
    validate(need(length(res_list)>1,"Requires at least two layers"))
    coords<-attr(getdata_map(),"coords")
    data<-getdata_map()

    layer_shape=if(isTRUE(input$srmap_layer)){
      attr(data,"layer_shape") } else{ NULL}
    srcol_layer=adjustcolor(getcolhabs(vals$newcolhabs,input$srlayer_col,1), input$srlayer_lighten)


    zs<-c()
    co<-c()
    zt<-c()
    labs<-list()
    showlab<-c()


    kal<-c()
    kap<-c()
    kat<-c()


    for(i in 1:length(names(get_stacklist())))
    {
      zt[i]<-input[[paste0("stack_text",i)]]
      zs[i]<-input[[paste0("stack_z",i)]]
      co[i]<-input[[paste0("stack_co",i)]]
      kal[i]<-input[[paste0("stack_key_adjx_lab",i)]]
      kap[i]<-input[[paste0("stack_key_adjz_pal",i)]]
      kat[i]<-input[[paste0("stack_key_adjz_text",i)]]

      showlab[i]<-input[[paste0("stack_lab",i)]]
      labs[[i]]<-attr(getdata_map(),"factors")[rownames(coords), input[[paste0("stack_labels",i)]]]
    }

    args<-list(
      rlist=res_list,
      legtitle.pos=input$stack_legtitle.pos,
      leglabpos=input$stack_leglab.pos,
      legbar.h=input$stack_legbar.h,
      legy.adj=input$stack_legadj,
      width.legend=input$stack_width.legend,
      theta=input$stack_theta,
      phi=input$stack_phi,
      stack_eye=input$stack_eye,
      d=input$stack_d,
      exp=input$stack_exp,
      transp=abs(1-input$stack_alpha),
      layer_shape=layer_shape,
      box_all=input$box_all,
      box_border=input$box_border,
      colgrid=input$stack_colguides,
      coords=coords,
      labels=labs,
      col.labels=if(length(input$col_factor_stack)>0){   getcolhabs(vals$newcolhabs,input$col_factor_stack,1) } else{ NULL},
      cex.labels=input$pt_factor_stack,
      col.coords=if(length(input$col_coords_stack)>0){   getcolhabs(vals$newcolhabs,input$col_coords_stack,1) } else{ NULL},
      cex.coords=input$pt_coords_stack,
      show_coords=co,
      show_labels=showlab,
      newcolhabs=vals$newcolhabs,
      z.value=zs,
      zmin=input$stack_zmin,
      zmax=input$stack_zmax,
      xlim=c(input$srlong_xmin,input$srlong_xmax),
      ylim=c(input$srlat_xmin,input$srlat_xmax),
      ticktype=input$sr_ticktype,
      xlab=input$sr_xlab,
      ylab=input$sr_ylab,
      zlab=input$sr_zlab,
      col_layer=srcol_layer,
      nticks=input$sr_nticks,
      z_text=zt,
      lab_xadj=input$sr_lab_xadj,
      legpal_zajd=input$sr_legpal_zajd,
      labaxis_zadj=input$sr_labaxis_zadj,
      kal=kal,
      kap=kap,
      kat=kat
    )
    #args<- readRDS('args.rds')

    # attach(args)
    pmat<-do.call(stack4D_2,args)


    if(isTRUE(input$showguides_stack)){
      grid4D(coords,res_list[[1]], col=input$col_guides_stack, pmat=pmat,lty=as.numeric(input$lty_guides_stack),lwd=input$lwd_guides_stack)
    }

    vals$map_res<-vals$map_stack<-recordPlot()


  })

  output$map_stack<-renderUI({
    req(isTRUE(input$stack_map))
    renderPlot(
      vals$map_stack
    )
  })







  {




  }
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


  #Propertymodal

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
  #interpmodal
  {
    interpmodal<-function() {
      modalDialog(
        textinterp(),
        title =  h4(strong("Interpolation")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(ignoreInit = T,input$interphelp, {
      showModal(interpmodal())
    })

    output$interphelp<-renderText({
      if (input$interph %% 2) {
        paste0(
          br(),
          p(
            icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"),
            "only the argument ",
            code("idp"),
            " is available to be passed to ",
            code("idw"),
            " function. The the remaining arguments are fixed to their default values;"
          ),
          h4("idw {gstat}"),
          getHelp('krige')
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


  output$codechunk_base<-renderPrint({
    codebase()
  })
  output$codechunk_layer<-renderPrint({
    codelayer()
  })
  smw_labinfo<-reactive({
    strong(
      "split moving window",
      tipify(
        actionLink("bphelp", icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),
        "Performs split moving window to detect significant discontinuities in the relationship between the number of clusters and WSS values. Click for more information."
      )
    )
  })
  observeEvent(input$show_smw_hc,{
    vals$show_smw_hc<-input$show_smw_hc
  })

  output$show_smw_hc<-renderUI({
    req(!is.null(vals$screeplot_results))
    if(is.null(vals$show_smw_hc)){vals$show_smw_hc<-F}

    inline(checkboxInput("show_smw_hc", value = vals$show_smw_hc, smw_labinfo()))
  })

  output$smw_hc_out<-renderUI({
    renderUI({
      div(style="margin-top: 20px; border-top: 1px solid gray",
          div(

            inline(uiOutput("show_smw_hc")),
            inline(uiOutput("hc_smw_run"))
          ),
          uiOutput("hc_smw_control")

      )

    })

  })

  output$smw_hc_seed_out<-renderUI({
    numericInput("smw_hc_seed","+ Seed",NA)
  })
  output$smw_hc_w_out<-renderUI({
    ws=seq(2,length(vals$screeplot_results0$WSS)/2,by=2)
    tags$div(id="smw_hc_pool",
             textInput(
               "smw_hc_w",
               span("+ Window sizes",tiphelp("comma-delimeted")),
               value = paste0(ws,collapse=", ")
             )
    )
  })
  output$smw_hc_rand_out<-renderUI({
    if(is.null(vals$smw_hc_rand)){vals$smw_hc_rand<-50}
    numericInput("smw_hc_rand","+ Nº randomizations",vals$smw_hc_rand)
  })
  output$smw_hc_tol_out<-renderUI({
    if(is.null(vals$smw_hc_tol)){vals$smw_hc_tol<-1.5}
    numericInput("smw_hc_tol", span("+ tol", tiphelp("Adjusts sensitivity when identifying potential breakpoints. If the dissimilarity score (DS) exceeds -tol- times the standard deviation, a breakpoint is suggested.")), vals$smw_hc_tol, step=0.1)
  })

  observeEvent(input$smw_hc_rand,{
    vals$smw_hc_rand<-input$smw_hc_rand
  })
  observeEvent(input$smw_hc_tol,{
    vals$smw_hc_tol<-input$smw_hc_tol
  })
  output$hc_smw_control<-renderUI({
    req(isTRUE(input$show_smw_hc))
    div(
      # uiOutput("smw_hc_seed_out"),
      uiOutput("smw_hc_w_out"),
      uiOutput("smw_hc_rand_out"),
      uiOutput("smw_hc_tol_out")
    )
  })
  observeEvent(ignoreInit = T,input$model_or_data,{
    vals$screeplot_results<-NULL
  })




  output$model_WSS_out<-renderUI({

    div(inline(uiOutput("screeplot_hc_inputs")),

        div(uiOutput('smw_hc_out'))
    )


  })





  output$screeplot_control2<-renderUI({
    fluidRow(
      conditionalPanel(
        "input.model_or_data=='data'",
        {

        }
      ),
      conditionalPanel(
        "input.model_or_data=='som codebook'",
        {

        }
      )



    )
  })

  get_hc_screeplot<-reactive({
    req(input$model_or_data)

    if(input$model_or_data=="som codebook"){
      m<- getmodel_hc()
      dist<-object.distances(m,"codes")
      data<-cmdscale(dist, k=30)
    } else{
      data<-  getdata_hc()
      dist<-vegdist(data,input$disthc)
    }
    p<-fviz_nbclust(data, hcut, method = "wss", k.max = input$screeplot_hc_k, diss=dist) + theme_minimal() + ggtitle("the Elbow Method")
    p$data$clusters<-as.numeric(p$data$clusters)
    re<-p$data
    colnames(re)<-c("Clusters","WSS")
    vals$screeplot_results0<-vals$screeplot_results<-re
    vals$scree_plot_hc<- vals$scree_plot_hc0<-p
  })

  observeEvent(ignoreInit = T,input$run_screeplot_hc, {

    get_hc_screeplot()

  })

  picK<-reactive({input$customKdata})
  observeEvent(ignoreInit = T,input$customKdata,
               vals$saved_kcustom<-input$customKdata)


  output$customKdata<-renderUI({
    req(input$hc_tab!="hc_tab6")
    if(is.null(vals$saved_kcustom)){vals$saved_kcustom=2 }
    k<-vals$saved_kcustom
    numericInput("customKdata",
                 "+ Number of clusters: ",
                 value = k,
                 step = 1)

  })
  observeEvent(ignoreInit = T,input$hcdata_palette,{vals$hcdata_palette<-input$hcdata_palette})



  output$hc_side3<-renderUI({
    req(input$hc_tab!="hc_tab1"&input$hc_tab!="hc_tab2")
    div(class="map_control_style2",style="color: #05668D",
        div(
          div(uiOutput("customKdata")),

          div(uiOutput('hc_palette')),
          div(uiOutput('hcut_labels')),
          div(uiOutput('hcut_theme')),
          div(uiOutput('hcut_cex')),
          div(uiOutput('hcut_lwd')),
          div(uiOutput('hcut_main')),
          div(uiOutput('hcut_ylab')),
          div(uiOutput('hcut_xlab')),
          div(uiOutput('hcut_xlab_adj')),
          div(uiOutput('hcut_offset')),
          div(uiOutput('hcut_legend_type'))

        )
    )
  })
  output$hcut_legend_type<-renderUI({
    req(input$hc_tab=="hc_tab3")
    radioButtons("hcut_legend_type","+ Legend:",c("inside","outside"), inline=T)


  })

  output$hcut_theme<-renderUI({
    req(input$hc_tab=="hc_tab3")
    pickerInput("hcut_theme","+ Theme:",c('theme_minimal','theme_grey','theme_linedraw','theme_light','theme_bw','theme_classic'))


  })


  output$hcut_offset<-renderUI({
    req(input$hc_tab=="hc_tab3")
    numericInput("hcut_offset",
                 "+ Line width",
                 value = -.1,
                 step = 0.05)
  })
  output$hcut_lwd<-renderUI({
    req(input$hc_tab=="hc_tab3")
    numericInput("hcut_lwd",
                 "+ Line width",
                 value = .5,
                 step = .5)
  })
  output$hcut_cex<-renderUI({
    req(input$hc_tab=="hc_tab3")
    numericInput("hcut_cex",
                 "+ Size",
                 value = 12,
                 step = 1)
  })

  output$hcut_main<-renderUI({
    req(input$hc_tab=="hc_tab3")
    textInput("hcut_main",
              "+ Title",
              "Cluster Dendrogram")
  })

  output$hcut_xlab<-renderUI({
    req(input$hc_tab=="hc_tab3")
    textInput("hcut_xlab",
              "+ x label",
              "Observations")
  })


  output$hcut_xlab_adj<-renderUI({
    req(input$hc_tab=="hc_tab3")
    numericInput("hcut_xlab_adj",
                 "+ xlab v-adj",
                 value = 30,
                 step = 1)
  })
  output$hcut_ylab<-renderUI({
    req(input$hc_tab=="hc_tab3")
    textInput("hcut_ylab",
              "+ y label",
              "Height")
  })




  # ws=c(2,4)
  #input$smw_hc_tol=1.5
  # input$smw_hc_rand=50
  observeEvent(input$run_smw_hc,{
    vals$screeplot_results<-vals$screeplot_results0
    vals$scree_plot_hc<- vals$scree_plot_hc0
    #input<-readRDS('input.rds')
    # vals<-readRDS('savepoint.rds')
    req(length(input$show_smw_hc)>0)
    req(length(input$smw_hc_w)>0)
    req(!is.null(vals$screeplot_results))
    req(isTRUE(input$show_smw_hc))
    req(input$smw_hc_rand>2)
    result<-vals$screeplot_results
    n.rand=input$smw_hc_rand
    ws<-as.numeric(unlist(strsplit(input$smw_hc_w,",")))
    #session=MockShinySession$new()
    smw<-smwhc(result, n.rand,ws,
               #session=session
    )
    vals$screeplot_results<-smw
    #savereac()

  })

  getsmw_plot<-reactive({
    tol=input$smw_hc_tol
    p<-   vals$scree_plot_hc0
    if(ncol(vals$screeplot_results)>2){
      smw<-vals$screeplot_results
      smw2<-get_screeplot_smw_sig(smw,tol)
      df<-get_ggdata_screesms(smw2,tol)
      p<-scree_smw_ggplot(df)
    }
    p

  })

  output$plot_hc_screeplot <-renderUI({
    vals$scree_plot_hc<-getsmw_plot()
    renderPlot({print(vals$scree_plot_hc)})

  })











  sugg<-reactive({
    if (input$smw_elbow %% 2)
    {
      smw_bp(elbow(), ws = input$window_elbow)
    } else {
      NULL
    }
  })




  output$screeplot_hc_inputs<-renderUI({
    if(input$model_or_data=="som codebook"){
      m<- getmodel_hc()
      data<-m$codes[[1]]

    } else{
      data<-  getdata_hc()
    }


    div(
      tags$div(style="display: inline-block; vertical-align: top;", numericInput("screeplot_hc_k", span(strong("+ k"), tiphelp("maximum number of clusters to be tested")), value = round(nrow(data)/2))),
      tags$div(id='run_screeplot_hc_btn',style="display: inline-block; vertical-align: top;", class="save_changes",actionButton("run_screeplot_hc", "Run screeplot"))
    )
  })

  observeEvent(input$run_screeplot_hc,ignoreInit = T,{
    shinyjs::removeCssClass('run_screeplot_hc_btn',"save_changes")
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


  output$hc_smw_run<-renderUI({


    req(isTRUE(input$show_smw_hc))
    req(length(input$smw_hc_w)>0)
    ws<-as.numeric(unlist(strsplit(input$smw_hc_w,",")))
    max<-input$screeplot_hc_k

    validate(need(!any(ws>max),
                  "Running SMW is unavailable ecause the maximum size should not exceed half of K."))



    validate(need(!any(ws%%2 == 1),"Running SMW is unavailable as all window sizes must be even."))

    class=if(isTRUE(vals$run_smw_hc)){"div"} else{'save_changes'}
    tags$div(class=class,actionButton("run_smw_hc","RUN smw"))
  })

  observeEvent(input$run_smw_hc,{
    vals$run_smw_hc<-T
  })






  output$clustering_panel<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(
      div(class='choosechannel',
          uiOutput("hc_control")
      ),
      br(),
      div(style="background: white",
          div(id="hc_radio_btn",
              uiOutput("hc_radio")),
          uiOutput("hc_panels")
      )
    )
  })
  observeEvent(ignoreInit = T,input$hc_tab,{
    vals$cur_hc_tab<-input$hc_tab
  })
  output$hc_side1<-renderUI({
    req(input$hc_tab=='hc_tab1')
    value=if(input$model_or_data=="som codebook"){
      paste0(input$data_hc,"(SOM codebook)")
    } else{ input$data_hc}
    div(
      div(span("+ Title",
               inline(textInput("hc_title", NULL, value =
                                  value, width="200px"))
      )),


      uiOutput('hclabs_control'),

    )
  })

  output$down_hc_model<-downloadHandler(
    filename = function() {
      paste0("HC","_", Sys.Date(),".rds")
    }, content = function(file) {
      saveRDS(phc(),file)
    })

  output$hc_tab1_out<-renderUI({
    req(input$hc_tab=='hc_tab1')
    div(
      uiOutput("hcdata_plot"),
      uiOutput("hcmodel_plot")
    )
  })
  output$hc_save_tab34<-renderUI({
    req(input$hc_tab)
    req(input$hc_tab=="hc_tab3"|input$hc_tab=="hc_tab4")
    div(
      div(inline(uiOutput("saveHC")))
    )
  })



  output$hc_side2<-renderUI({
    req(input$hc_tab=='hc_tab2')
    div(
      uiOutput("data_WSS_out"),
      uiOutput("model_WSS_out"),
      div(
        actionLink('down_results_screeplot',"+ Download Results", style="button_active")
      )
    )
  })

  output$hc_tab2_out<-renderUI({
    req(input$hc_tab=='hc_tab2')
    req(!is.null(vals$screeplot_results))
    div(

      div(strong(
        "Scree plot", actionLink('screeplothelp', icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"))
      ), style = "margin-bottom: 5px;"),
      div(uiOutput('plot_hc_screeplot'))


    )
  })



  hcut_argsplot<-reactive({
    req(input$hc_tab=='hc_tab3')
    req(input$model_or_data)
    req(input$customKdata)
    req(input$method.hc0)
    req(input$hcdata_palette)
    req(input$hcut_theme)
    req(input$hcut_cex)
    req(input$hcut_lwd)
    req(input$hcut_main)
    req(input$hcut_ylab)
    req(input$hcut_xlab)
    req(input$hcut_xlab_adj)
    req(input$hcut_offset)
    req(input$hcut_legend_type)
    somC<-phc()
    args<-list(
      somC=somC,
      col=getcolhabs(vals$newcolhabs,input$hcdata_palette,input$customKdata),
      labels=get_hcut_labels(),
      lwd=input$hcut_lwd,
      base_size=input$hcut_cex,
      main=input$hcut_main,
      xlab=input$hcut_xlab,
      ylab=input$hcut_ylab,
      theme=input$hcut_theme,
      offset_labels=input$hcut_offset,
      xlab_adj=input$hcut_xlab_adj,
      legend=input$hcut_legend_type
    )
    args
  })

  output$hc_tab3_out<-renderUI({
    req(input$hc_tab=='hc_tab3')

    div(
      renderPlot({
        args<-hcut_argsplot()
        p<-do.call('hc_plot',args)
        #saveRDS(p,"p.rds")
        # p<-readRDS("p.rds")
        req(class(p)[1]=="gg")
        vals$hc_tab3_plot<- p
        print(p)
        # vals$hc_tab3_plot
      })
    )
  })




  output$hc_radio<-renderUI({
    req(input$model_or_data)
    if(input$model_or_data=="data"){
      choiceNames=c("1. Dendrogram",
                    "2. Scree Plot",
                    '3. Cut Dendrogram')
      choiceValues =c("hc_tab1",
                      "hc_tab2",
                      "hc_tab3")
    } else{
      choiceNames=c("1. Dendrogram",
                    "2. Scree Plot",
                    '3. Cut Dendrogram',
                    '4. Codebook clusters',
                    #  '5. Map new data on codebook',
                    '5. Codebook screeplot'
      )
      choiceValues =c("hc_tab1",
                      "hc_tab2",
                      "hc_tab3",
                      "hc_tab4",
                      # "hc_tab5",
                      "hc_tab6")
    }

    radioGroupButtons(
      "hc_tab",
      choiceNames=choiceNames,
      choiceValues =choiceValues,
      selected=vals$cur_hc_tab,
      status ="radio_hc"
    )
  })

  output$hc_panels<-renderUI({
    #req(input$model_or_data)
    div(style="margin-left:5px;background: white",

        sidebarLayout(
          sidebarPanel(
            div(
              uiOutput("hc_save_tab34"),
              uiOutput("hc_save_tab5"),
              div(class="map_control_style2",style="color: #05668D",

                  uiOutput("hc_side1"),
                  uiOutput("hc_side2"),
                  uiOutput("hc_side3"),
                  uiOutput("hc_ordcluster"),
                  uiOutput("hc_side4"),
                  # uiOutput("hc_side5"),

                  uiOutput("hc_side6"),
                  uiOutput("showerrors_som"),
                  div(
                    actionLink('down_hc_plot', '+ Download plot')
                  )
              )
            )
          ),
          mainPanel(
            uiOutput('main_hc'),
            uiOutput('predsom_hc'),
            uiOutput("hc_tab1_out"),
            uiOutput("hc_tab2_out"),
            uiOutput("hc_tab3_out"),
            uiOutput("hc_tab4_out"),
            # uiOutput("hc_tab5_out"),
            uiOutput("hc_tab6_out")
          )
        ))
  })



  #uiOutput("mapcode_class")



  observeEvent(ignoreInit = T,input$hc_ord_datalist,{
    vals$hc_ord_datalist<-input$hc_ord_datalist
  })



  observeEvent(ignoreInit = T,input$hc_sort,{
    vals$hc_sort<-input$hc_sort
  })

  output$hc_ordcluster<-renderUI({
    req(input$hc_tab!="hc_tab1")
    req(input$hc_tab!="hc_tab2")
    req(input$hc_tab!="hc_tab6")
    div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; padding-bottom: 5px',
        div(
          checkboxInput("hc_sort",span("+ Sort clusters",tiphelp("Sort clusters by a  variable")),value=vals$hc_sort,width="120px"),
          div(style="margin-left: 40px",
              inline(uiOutput("hc_sort_datalist")) ,

              inline(uiOutput("hc_ord_factor")))

        )
    )
  })
  getdata_for_hc<-reactive({
    req(input$data_hc)
    datalist<-vals$saved_data
    data<-vals$saved_data[[input$data_hc]]
    req(length(data)>0)
    res0<-unlist(
      lapply(datalist, function (x){
        sum(rownames(x)%in%rownames(data))==nrow(x)
      })
    )
    names(res0[res0==T])
  })



  output$hc_sort_datalist<-renderUI({
    req(isTRUE(input$hc_sort))
    div(
      div("Datalist:"),
      inline(pickerInput("hc_ord_datalist",NULL,choices = c(names(vals$saved_data[getdata_for_hc()])),selected=vals$hc_ord_datalist,width="200px")),  "::"
    )
  })
  output$hc_ord_factor<-renderUI({
    req(isTRUE(input$hc_sort))
    req(input$hc_ord_datalist)
    data<-vals$saved_data[[input$hc_ord_datalist]]

    choices=c(colnames(data))

    div(
      div("Variable:"),
      pickerInput("hc_ord_factor",NULL,
                  choices = choices,selected=vals$hc_ord_factor,width="120px")
    )

  })

  observeEvent(ignoreInit = T,input$hc_ord_factor,{
    vals$hc_ord_factor<-input$hc_ord_factor
  })







  get_hc<-reactive({
    req(input$model_or_data)
    req(input$method.hc0)
    if (input$model_or_data == "som codebook") {
      hc<-cutsom.reactive()
    } else if (input$model_or_data == "data") {
      hc<-cutdata.reactive()
      hc$som.hc<- hc$somC
    }

    hc
  })


  hc_newlevels<-reactive({
    req(input$data_hc)
    req(input$hc_ord_datalist)
    req(input$hc_ord_factor)

    data_o<-getdata_hc()
    data<-vals$saved_data[[input$hc_ord_datalist]][rownames(data_o),]
    hc<-get_hc()

    validate(need(sum(rownames(data_o)%in%rownames(data))==nrow(data_o),"The IDs of the sorted data chosen do not match those of the training data."))
    data<-data[rownames(data_o),]
    fac<-data[names(hc$somC),input$hc_ord_factor,drop=F]
    clusters<-hc$somC
    newlevels<-names(sort(tapply(fac[,1],as.numeric(as.character(clusters)),mean)))

    newlevels
  })

  phc<-reactive({
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


  })

  output$hc_palette<-renderUI({
    req(!input$hc_tab%in%c('hc_tab4','hc_tab6'))
    pickerInput(inputId = "hcdata_palette",
                label = "+ HC Palette:",
                choices = vals$colors_img$val,
                choicesOpt=list(content=vals$colors_img$img),
                selected=vals$hcdata_palette)
  })
  output$hc_side4<-renderUI({

    req(input$hc_tab=='hc_tab4')
    req(input$model_or_data == "som codebook")


    div(style="max-height:300px;overflow-y: scroll;",
        uiOutput("hcs_side_somplot"),
        div(actionLink('create_codebook',"+ Create Datalist with the Codebook and HC class")),
        div(div(
          tipify(downloadLink('down_hc_model',"+ Download HC model", style="button_active"),"Download file as .rds")
        ))
    )
  })
  output$hcs_side_somplot<-renderUI({
    div(class="map_control_style2",style="color: #05668D",
        div(
          #uiOutput("somcut_display"),

          uiOutput('hcs_palette'),


          div(
            style='border-bottom: 1px solid gray',
            uiOutput('out_pclus_addpoints'),
            div(
              style="margin-left: 10px",
              uiOutput("pclus_points_inputs")
            )
          ),

          div(style='border-bottom: 1px solid gray',
              uiOutput('out_pclus_addtext'),
              div(
                style="margin-left: 10px",
                uiOutput("pclus_text_inputs")
              )),
          div(
            uiOutput("vfm_check")
          ),
          uiOutput("hcs_theme"),
          uiOutput("hcs_title")


        ))
  })

  output$hcs_palette<-renderUI({
    choices=getgrad_col()

    div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',
        pickerInput(inputId = "bg_palette",
                    label ="+ Background palette",
                    choices =  vals$colors_img$val[choices],
                    choicesOpt = list(
                      content =  vals$colors_img$img[choices] ),
                    options=list(container="body")),

        tipify(numericInput("pcodes_bgalpha","+ Background lightness",value = 0,min = 0,max = 1,step = .1),"symbol size"),

        pickerInput("pclus_border",
                    label ='+ Border:',
                    choices =  vals$colors_img$val[getsolid_col()] ,
                    choicesOpt = list(
                      content =  vals$colors_img$img[getsolid_col()] ),
                    selected= "white"),

        tipify(numericInput("base_size","+ Base size",value = 12),"symbol size")


    )
  })
  output$out_pclus_addpoints<-renderUI({
    checkboxInput("pclus_addpoints","+ Points",
                  value=T)
  })
  output$out_pclus_addtext<-renderUI({

    checkboxInput("pclus_addtext","+ Labels",
                  value=F)
  })
  output$pclus_points_inputs<-renderUI({
    req(isTRUE(input$pclus_addpoints))
    div(
      uiOutput("pclus_points_palette"),
      uiOutput("pclus_points_factor_out"),
      uiOutput("pclus_points_shape"),
      uiOutput("pclus_points_size")
    )
  })
  output$pclus_points_palette<-renderUI({
    req(length(input$hcsom_newdata)>0)

    choices = vals$colors_img$val
    choicesOpt = list(content = vals$colors_img$img)
    tipify(pickerInput(inputId = "pclus_points_palette",
                       label ="+ Palette",
                       choices =choices,
                       choicesOpt =choicesOpt,
                       selected="black",
                       options=list(container="body")),
           "Symbol colors","right"
    )
  })
  output$pclus_points_factor_out<-renderUI({
    req(input$pclus_points_palette)
    req(length(input$hcsom_newdata)>0)
    data<-getdata_hc()
    choices<-colnames(attr(data,"factors"))
    if(isTRUE(input$hcsom_newdata)){
      choices<-c("Training/New data","Training")
    }
    cols<-vals$newcolhabs[[input$pclus_points_palette]](8)
    if(cols[1]!=cols[2]){
      pickerInput("pclus_points_factor","+ Factor",
                  choices = c(choices))

    } else{

      tags$div(
        class="form-group shiny-input-container",
        tags$label(class = "control-label", " + Factor"),
        tags$div(class="dummy-input",
                 "Choose a gradient palette for adding a factor",style="color: gray"
        )
      )
    }
  })
  output$pclus_points_shape<-renderUI({
    tipify(pickerInput(inputId = "pclus_symbol",
                       label = "+ Shape",
                       choices = df_symbol$val,
                       choicesOpt = list(content = df_symbol$img),
                       options=list(container="body"))
           ,"symbol shape",'right')
  })
  output$pclus_points_size<-renderUI({

    tipify(numericInput("pclus_points_size","+ Size",value = 1,min = 0.1,max = 3,step = .1),"symbol size","right")

  })
  output$pclus_text_inputs<-renderUI({
    req(isTRUE(input$pclus_addtext))
    div(
      uiOutput("pclus_text_palette"),
      uiOutput("pclus_text_factor_out"),
      uiOutput("pclus_text_size")
    )
  })
  output$pclus_text_palette<-renderUI({
    tipify(pickerInput(inputId = "pclus_text_palette",
                       label ="+ Palette",
                       choices =  vals$colors_img$val[getsolid_col()],
                       selected="black",
                       choicesOpt = list(
                         content =  vals$colors_img$img[getsolid_col()] ),
                       options=list(container="body")),
           "Symbol colors","rigth"
    )
  })
  output$pclus_text_factor_out<-renderUI({
    req(input$pclus_text_palette)
    req(length(input$hcsom_newdata)>0)
    data<-getdata_hc()
    choices<-colnames(attr(data,"factors"))

    pickerInput("pclus_text_factor","+ Factor",
                choices = c(choices))



  })
  output$pclus_text_size<-renderUI({

    tipify(numericInput("pclus_text_size","+ Size",value = 1,min = 0.1,max = 3,step = .1),"symbol size","rigth")

  })
  output$vfm_check<-renderUI({

    div(style='border-bottom: 1px solid gray',
        checkboxInput("varfacmap_action", span("+ Variable factor map",actionLink("varfacmap", tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value =T),
        div(style="margin-left: 10px",uiOutput("varfac_out"))
    )
  })
  output$vfm_type_out<-renderUI({
    my_choices<-list("Highest"='var', "Clockwise"="cor","Cluster"="cor_hc")
    pickerInput("vfm_type","+ Show correlation:",
                choices =my_choices


    )
  })
  output$varfac_out<-renderUI({
    req(isTRUE(input$varfacmap_action))

    div(
      uiOutput('vfm_type_out'),
      uiOutput('npic_out'))})
  output$npic_out<-renderUI({
    div(
      tipify(
        numericInput("npic", "+ Number", value = 10, min = 2),"Number of variables to display","right"
      ),

      numericInput("pclus.cex.var", "+ Var size", value = 1, min = 2),
      div(class="palette",
          pickerInput(inputId = "p.clus.col.text",
                      label = "+ Var text color",
                      choices = vals$colors_img$val[getsolid_col()],
                      choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),
                      selected="black")),
      pickerInput(inputId = "var_bg",
                  label = "+ Var background",
                  choices = vals$colors_img$val[getsolid_col()],
                  choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),
                  selected="white"),
      tipify(
        numericInput("var_bg_transp", "+ Var transparency", value = 0, min = 2),"Number of variables to display","right"
      )

    )
  })
  output$hcs_title<-renderUI({
    textInput("hcs_title", "+ Title: ", "")
  })
  output$hcs_theme<-renderUI({

    div(style='border-bottom: 1px solid gray',
        inline(checkboxInput("hcs_theme",
                             label = "+ show neuron coordinates",
                             value=F,
                             width="200px"
        ))
    )
  })


  hcs_getobs_mapcode<-reactive({
    datalist<-vals$saved_data
    #som_names<-names(attr(vals$saved_data[[vals$cur_data]],"som"))
    #req(length(som_names)>0)

    models<- m<-getmodel_hc()
    req(length(models)>0)
    req(vals$cur_som_hc%in%names(models))
    m<-models[[as.character(vals$cur_som_hc)]]
    res0<-unlist(
      lapply(datalist, function (x){
        identical(colnames(x),colnames(do.call(cbind,m$data)))
      })
    )
    choices<-names(res0[res0==T])
    res0<-unlist(
      lapply(datalist[choices], function (x){
        !anyNA(x)
      })
    )


    choices<-names(res0[res0==T])
    choices
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
  get_network<-reactive({
    backtype=NULL
    property=NULL
    m<-getmodel_hc()
    hc<-phc()$som.hc
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
  observeEvent(ignoreInit = T,input$hc_ord_factor,{
    vals$hc_ord_factor<-input$hc_ord_factor
  })
  observeEvent(ignoreInit = T,input$hc_ord_datalist,{
    vals$hc_ord_datalist<-input$hc_ord_datalist
  })
  observeEvent(ignoreInit = T,input$hc_sort,{
    vals$hc_sort<-input$hc_sort
  })
  observeEvent(ignoreInit = T,input$hcs_theme,{
    vals$hcs_theme<-input$hcs_theme})
  observeEvent(ignoreInit = T,input$pclus_addtext,{
    vals$pclus_addtext<-input$pclus_addtext
  })
  observeEvent(ignoreInit = T,input$pclus_addpoints,{
    vals$pclus_addpoints<-input$pclus_addpoints
  })
  observeEvent(ignoreInit = T,input$pp_labels,{
    vals$pp_labels<-input$pp_labels
  })
  observeEvent(ignoreInit = T,input$next_property,{
    data = data.frame(vals$saved_data[[input$data_hc]],"factors")
    pnext<-colnames(data)[which(colnames(data)==input$variable_pproperty)+1]
    updateSelectInput(session,'variable_pproperty',selected=pnext)

  })
  observeEvent(ignoreInit = T,input$prev_property,{
    data = data.frame(vals$saved_data[[input$data_hc]],"factors")
    pprev<-colnames(data)[which(colnames(data)==input$variable_pproperty)-1]
    updateSelectInput(session,'variable_pproperty',selected=pprev)
  })
  observeEvent(ignoreInit = T,input$variable_pproperty,{
    vals$variable_pproperty<-input$variable_pproperty
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

  argsplot_somplot<-reactive({
    req(input$hc_tab=='hc_tab4')
    req(input$pclus_points_palette)
    req(input$pcodes_bgalpha)


    indicate=indicate_hc()
    m<-getmodel_hc()
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
               hc=phc()$som.hc
    )

    args

  })

  output$hc_tab4_out<-renderUI({
    req(input$hc_tab=='hc_tab4')
    req(input$model_or_data == "som codebook")
    div(

      uiOutput("BMU_PLOT"),
      uiOutput('hcsom_newdata_control'),
    )
  })
  get_error<-reactive({
    NULL
  })

  observeEvent(argsplot_somplot(),{

  })


  observe({
    vals$bmu_update<-"save_changes"
  }, autoDestroy = T)



  observeEvent(argsplot_somplot(),{
    vals$bmu_update<-"save_changes"
  })


  output$BMU_PLOT<-renderUI({
    req(input$data_hc)
    bmu_reactive()
  })

  bmu_reactive<-reactive({

    req(input$hc_tab=='hc_tab4')
    req(input$model_or_data == "som codebook")
    m<-getmodel_hc()
    req(length(m)>0)
    div(


      renderPlot({

        somC<-phc()
        args<-argsplot_somplot()

        args$hc<-phc()$som.hc
        p<-do.call(bmu_plot_hc,args)
        p
      })
    )

  })





  savemapcode<-reactive({
    hc<-phc()

    data<-vals$saved_data[[input$data_mapcode]]
    somC<-cutsom.reactive()
    pred<-mapcode()
    hcut<-somC$som.hc

    vals$bmus_newdata

    newclass<-pred$unit.classif


    for(i in 1:length(hcut)) {newclass[newclass==i]<-rep(hcut[i],sum(  newclass==i))}
    names(newclass)<-rownames(data)

    temp<-newclass

    if(input$hand_save=="create"){
      attr(vals$saved_data[[input$data_mapcode]],"factors")[names(temp),input$mc_newname]<-as.factor(temp)
    } else{
      attr(vals$saved_data[[input$data_mapcode]],"factors")[input$mc_over]<-as.factor(temp)
    }




  })


  output$somcut_display<-renderUI({
    div(span("+ Display:",inline(
      radioButtons(
        "dot_label_clus", NULL, choices = c("labels", "symbols"), inline=T, width="100px", selected=vals$dot_label_clus)
    )))
  })








  ##hc side6

  output$hc_side6<-renderUI({
    # req(input$hc_tab=='hc_tab6')
    #div(class="map_control_style", pickerInput("data_mapcode_tab6","+ Map New Data",choices=names(vals$saved_data[getobs_mapcode()]), selected=vals$data_mapcode, width="200px"))

  })



  output$showerrors_som<-renderUI({
    req(input$hc_tab)
    req(input$hc_tab=='hc_tab6')
    div(
      checkboxGroupInput("show_mapcode_errors", '+ Show error',choices=c("Within Sum of Squares","Dendrogram Height"), width="200px", selected=vals$show_mapcode_errors),
      textInput('code_screeplot_title',"+ Title",input$som_hc),
      pickerInput('code_screeplot_agg',"Aggregate Errors",c("Mean","Median","Sum"))


    )




  })




  output$hc_save_tab5<-renderUI({
    req(input$hc_tab=="hc_tab5")
    div(class="save_changes",
        bsButton("savemapcode", icon(verify_fa = FALSE,name=NULL,class="fas fa-save"),style  = "button_active", type="action",value=FALSE), span(style="font-size: 12px",icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"), "Save Clusters in Datalist", strong("'Map New data'"))
    )
  })

  observeEvent(ignoreInit = T,input$down_hc_plot,{
    vals$hand_plot<-switch (input$hc_tab,
                            "hc_tab1" = "Dendrogram",
                            "hc_tab2" = "Scree plot",
                            "hc_tab3" = "Hcut",
                            "hc_tab4" = "codebook clusters",
                            "hc_tab5" = "mapcodes predictions",
                            "hc_tab6" = "Codebook screeplot"

    )
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
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


  output$testet<-renderUI({
    req(input$data_hc)
    cond<-length(names(attr(vals$saved_data[[input$data_hc]],"som")))>0
    renderPrint(cond)
  })
  observeEvent(ignoreInit = T,input$hc_results,{
    vals$hc_results<-input$hc_results
  })
  output$hcut_panels<-renderUI({})
  getobs_mapcode<-reactive({
    datalist<-vals$saved_data
    m<-getmodel_hc()
    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(m$data[[1]])
        sum(res)==ncol(m$data[[1]])
      })
    )
    names(res0[res0==T])
  })
  #### Map code
  ####



  observeEvent(ignoreInit = T,input$savemapcode,{
    if(input$savemapcode %% 2) {
      vals$hand_save<-"Save new data clusters"
      vals$hand_save2<-p(p(em(input$data_mapcode,style="color: gray"),strong("::"),em("Factor-Attribute",style="color: gray"), strong("::")))
      vals$hand_save3<-NULL
      showModal(
        hand_save_modal()
      )
    }
  })



  output$hc_tab6_out<-renderUI({
    req(input$hc_tab=='hc_tab6')
    div(
      div(
        inline(numericInput("mapcode_loop_K", "K",20,width="100px")),
        inline(actionButton("mapcode_loop_go","Run loop")),

      ),
      uiOutput("mapcode_loop"))
  })

  observeEvent(ignoreInit = T,input$mapcode_loop_go,{

    somC<-phc()
    vals$mapcode_loop_res<-NULL
    m<-getmodel_hc()

    k.max<-input$mapcode_loop_K
    hc_fun=input$hc_fun;hc_method=input$method.hc0
    result<-screeplot_som(m,k.max,hc_fun,hc_method)
    dend_hei<-somC$hc.object$height
    result$dh<-rev(dend_hei)[1:k.max]
    colnames(result)<-c("k","Within Sum of Squares","Dendrogram Height")
    #qe2[,2:3]<-decostand(qe[,2:3],"max",2)
    result<-reshape2::melt(result,"k")

    attr(result,"class_result")<-"som screeplot"
    attr(attr(vals$saved_data[[input$data_hc]],"som")[[input$som_hc]],"codebook_screeplot")<-result

  })

  output$mapcode_loop<-renderUI({
    req(input$data_hc)
    req(input$som_hc)
    req(input$mapcode_loop_K)
    k.max<-input$mapcode_loop_K
    result<-attr(attr(vals$saved_data[[input$data_hc]],"som")[[input$som_hc]],"codebook_screeplot")

    req(input$show_mapcode_errors%in%result$variable)
    df<-result[    result$variable%in%input$show_mapcode_errors,]

    div(
      renderPlot({
        p<-ggplot(df)+geom_line(aes(k,value))+facet_wrap(~variable,scales="free")+xlab("Number of Clusters")



        #legend("topl",lty=c(1,2),legend=c(input$data_mapcode,input$data_mapcode2), bty="n")
        vals$hc_tab6_plot<-p
        p

      })
    )


  })


  observeEvent(ignoreInit = T,input$data_hc,{
    vals$data_mapcode<-input$data_hc
  })



  observeEvent(ignoreInit = T,input$data_mapcode,{
    vals$data_mapcode<-input$data_mapcode
  })

  observeEvent(ignoreInit = T,input$data_hc,{
    vals$show_mapcode_errors<-c("Within Sum of Squares","Dendrogram Height")
  })






  observeEvent(ignoreInit = T,input$mapcode_title,{vals$mapcode_title<-input$mapcode_title})
  observeEvent(ignoreInit = T,input$show_mapcode_errors,{vals$show_mapcode_errors<-input$show_mapcode_errors})
  observeEvent(ignoreInit = T,input$mapcode_p_factors2,{
    vals$mapcode_p_factors2<-input$mapcode_p_factors2})






  observeEvent(ignoreInit = T,input$mapcode_p_symbol,
               vals$mapcode_p_symbol<-input$mapcode_p_symbol)
  output$mapcodep_symbol<-renderUI({
    req(input$mapcode_p_dotlabel)
    req(input$mapcode_p_dotlabel=='symbols')
    if(is.null(vals$mapcode_p_symbol)){vals$mapcode_p_symbol<-df_symbol$val[1]}
    div(span("+ Shape:", inline(
      pickerInput("mapcode_p_symbol",
                  label = NULL,
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),

                  selected=vals$mapcode_p_symbol, width = '75px')
    )))

  })
  observeEvent(ignoreInit = T,input$mapcode_p_dotlabel,
               vals$mapcode_p_dotlabel<-input$mapcode_p_dotlabel)


  observeEvent(ignoreInit = T,input$pcorr_obscolor,{
    vals$pcorr_obscolor<-input$pcorr_obscolor
  })

  observeEvent(ignoreInit = T,input$mapcode_p_symbol_size,{
    vals$mapcode_p_symbol_size<-input$mapcode_p_symbol_size
  })
  output$mapcode_p_symbol_size_out<-renderUI({
    if(is.null(vals$mapcode_p_symbol_size)){vals$mapcode_p_symbol_size<-1}
    div(span("+ Size:", inline(tipify(
      numericInput("mapcode_p_symbol_size",NULL,value = vals$mapcode_p_symbol_size,min = 0.1,max = 3,step = .1, width = '100px'),"symbol size"
    ))))
  })

  observeEvent(ignoreInit = T,input$mapcode_p_bg_transp,{
    vals$mapcode_p_bg_transp<-input$mapcode_p_bg_transp
  })
  output$mapcode_p_bg_transp_out<-renderUI({
    if(is.null(vals$mapcode_p_bg_transp)){vals$mapcode_p_bg_transp<-1}
    div(span("+ Background Transparency:",inline(
      tipify(
        numericInput("mapcode_p_bg_transp",NULL,
                     value=vals$mapcode_p_bg_transp, min=0, max=1,step=0.1, width = '75px'),
        "Background transparency")
    )))
  })
  observeEvent(ignoreInit = T,input$mapcode_p_border_grid,{
    vals$mapcode_p_border_grid<-input$mapcode_p_border_grid
  })
  output$mapcode_p_border_grid_out<-renderUI({
    if(is.null(vals$mapcode_p_border_grid)) {vals$mapcode_p_border_grid<-vals$colors_img$val[getsolid_col()][7]}
    div(span(span('+ Border:'),inline(
      pickerInput("mapcode_p_border_grid",
                  label =NULL,
                  choices =  vals$colors_img$val[getsolid_col()] ,
                  choicesOpt = list(
                    content =  vals$colors_img$img[getsolid_col()] ),
                  selected= vals$mapcode_p_border_grid, width = '75px')
    )))
  })
  mapcode_p_factors_reac<-reactive({
    data1<-vals$saved_data[[input$data_mapcode]]
    factors1<-attr(data1,"factors")
    fac1<-factors1[rownames(data1),input$mapcode_p_factors2]
    c(fac1)

  })

  mapcode_pred_points<-reactive({
    req(input$mapcode_p_dotlabel)
    if(input$mapcode_p_dotlabel == 'symbols'){ T} else {F}
  })
  mapcode<-reactive({
    kohonen::map(getmodel_hc(),as.matrix(vals$saved_data[[input$data_mapcode]]))
  })



  #######
  #######






  Tabs_HC<-reactive({
    if (anyNA(getdata_hc())) {
      column(
        12,
        strong(
          "The selected dataset contais missing values which are not allowed in this functionality. Please switch the dataset or go to the Upload menu for creating a new dataset without missing values.",
          style = "color: red;"
        )
      )
    }
  })
  output$new_fac_hc<-renderPrint({
    attr(getdata_hc(),"factors")
  })
  output$factor_bank_hc<-renderUI({
    column(12,
           br(),
           br(),
           column(
             12,
             strong("Factor bank", style = "color: Green;"),
             br(),
             div(verbatimTextOutput("new_fac_hc"), style = "width: 600;height:200px;overflow-y: scroll; overflow-x: scroll"),
             br(),
             br()
           ))
  })
  output$Tabs_HC<-renderUI({
    Tabs_HC()
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
  observeEvent(ignoreInit = T,input$help_hc_fun, {
    modal_help("hclust", intro=div(
      "iMESC implements ", tags$code("Hierarchical Clustering"), "analysis using the ", tags$code("hcut"), " function, which is part of the  ", tags$code("factoextar"), "package. The parameters that can be customized in iMESc are ",tags$code("hc_func"),"(clustering function),",  tags$code("k"), " (number of groups), ", tags$code("hc_method"), ", and ", tags$code("hc_metric"), " (distance measure for clustering numeric attributes). When clustering som codebook, iMESc uses the same distance metric used to train the SOM. The remaining parameters of the ", tags$code("hclust()"), " function are set to their default values. For more information regarding ",tags$code("hc_func"),"argument, refer to their documentation: ",
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

  output$hc_control<-renderUI({
    req(length(vals$saved_data)>0)
    div(class="well3",
        div(class="align_top2",
            div(style="height: 85px;vertical-align: text-top; display: table",
                inline(div(class="align_hc",
                           inline(div(style="padding: 5px",strong("X:")))
                           ,inline(uiOutput("data_hc")))),
                inline(div(class="align_hc",uiOutput("choicesHC"))),
                inline(div(class="align_hc",uiOutput("somHC"))),
                inline(
                  div(class="align_hc",
                      div("HC function:", actionLink("help_hc_fun",icon("fas fa-question-circle"))),
                      div(pickerInput("hc_fun",NULL,
                                      choices = list("Hierarchical Clustering"="hclust",
                                                     "Agglomerative Nesting"="agnes",
                                                     "Divisive hierarchical clustering"="diana"),
                                      selected=vals$hc_fun)
                      ))
                ),

                inline(div(class="align_hc",uiOutput("disthc"))),
                inline(div(class="align_hc",
                           div("Method:"),
                           div(
                             pickerInput(
                               "method.hc0",
                               NULL,
                               choices = c("ward.D2", "ward.D", "single", "complete", "average","mcquitty","median","centroid"), width="110px", selected=vals$method.hc0
                             )
                           )))

                #
            ))

    )





  })

  observeEvent(ignoreInit = T,input$hc_fun,{
    vals$hc_fun<-input$hc_fun
  })

  output$data_hc<-renderUI({
    tags$div(style="margin: 0px; padding: 0px",
             div("Training Datalist:"),
             div(
               pickerInput("data_hc",
                           NULL,
                           choices =    names(vals$saved_data),
                           width="250px", selected=vals$cur_data)
             )
    )
  })
  choices_hc_names<-reactive({
    req(input$data_hc)
    a<-if (length(   names(vals$saved_data) > 0)) {
      "Numeric-Attribute"
    } else {
      NULL
    }

    b<-   if(length(attr(vals$saved_data[[input$data_hc]],"som"))>0){"SOM-codebook"}else{NULL}
    res<-c(a, b)
    res
  })

  output$choicesHC<-renderUI({
    choices<-choices_hc()
    selected=NULL
    div(style="padding-left: 5px",
        div(
          div("Clustering target:"),
          radioButtons("model_or_data", NULL, choiceValues  = choices, choiceNames=choices_hc_names(),selected=vals$cur_model_or_data)
        )
    )
  })

  observeEvent(ignoreInit = T,input$model_or_data,{
    vals$cur_model_or_data<-input$model_or_data
  })

  observe({
    req(vals$cur_model_or_data)
    if(!vals$cur_model_or_data%in%choices_hc()){
      vals$cur_model_or_data<-NULL
    }
  })


  output$saveHC<-renderUI({
    clu_al<-cluster_already()
    if(length(cluster_already())>0){
      class1<-"button_normal"
      class2<-"div"
      class3<-'divnull'
    } else{
      class1<-"save_changes"
      class2<-"divnull"
      class3<-'div'
    }

    div(
      div(
        inline(
          div(class=class1,
              actionButton("tools_savehc", icon(verify_fa = FALSE,name=NULL,class="fas fa-save"),style  = "button_active", type="action",value=FALSE),
          )
        ), span(style="font-size: 12px",icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"), "Save Clusters in Datalist ", strong("X"), class=class3)
      ),


      div(style="margin-bottom: 5px",class=class2,
          em(paste0("The current clustering is saved in the Factor-Attribute as '",paste0(clu_al, collapse = "; "),"'"))
      )

    )
  })

  cluster_already<-reactive({
    req(input$data_hc)
    datao<-vals$saved_data[[input$data_hc]]
    factors<-attr(datao,"factors")
    hc<-as.character(phc()$somC[rownames(factors)])
    fac<-as.list(factors)
    fac<-lapply(fac,function(x) as.character(x))
    cluster_already<-which(sapply(fac, function(x) identical(x, hc)))
    names(cluster_already)
  })




  saveclusters<-reactive({
    vals$baghc0<-vals$baghc0+1
    hc<-phc()
    temp<-hc$somC
    if(input$hand_save=="create"){
      attr(vals$saved_data[[input$data_hc]],"factors")[names(temp),input$hc_newname]<-temp
    } else{
      attr(vals$saved_data[[input$data_hc]],"factors")[names(temp),input$hc_over]<-temp
    }

  })



  observeEvent(ignoreInit = T,input$data_hc,{
    req(input$hc_tab)
    req(input$hc_tab%in%c("hc_tab4","hc_tab5","hc_tab6"))
    updateRadioGroupButtons(session,'hc_tab',selected='hc_tab3')


  })


  output$somHC<-renderUI({
    req(input$model_or_data=='som codebook')
    div(
      inline(
        div(
          div("Som model:"),
          pickerInput(
            "som_hc",
            NULL,
            choices = names(attr(getdata_hc(),"som")),
            width="150px",
            selected=vals$cur_som_hc
          )
        )
      ),

      inline(
        div(

          #uiOutput("exclude_empty_neu")
        )
      )
    )
  })
  getmodel_hc<-reactive({
    req(input$data_hc)
    req(input$som_hc)
    data<-getdata_hc()
    m<-attr(data,"som")[[as.character(input$som_hc)]]
    m
  })

  getmodel_hc0<-reactive({
    req(input$data_hc)
    req(input$som_hc)
    data<-getdata_hc()
    m<-attr(data,"som")[[as.character(input$som_hc)]]
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

  get_copoints_newdata<-reactive({
    m<-predsupersom_hc()
    copoints<-getcopoints(m)
    vals$copoints_hc<-copoints
    copoints
  })
  copoints_scaled_newdata<-reactive({
    points_tomap=rescale_copoints(hexs=get_network(),copoints=get_copoints_newdata())
    data<-vals$saved_data[[input$data_hc]]
    factors<-attr(data,"factors")
    if(length(input$pclus_newdata_text_factor)>0){
      req(input$pclus_newdata_text_factor%in%colnames(factors))
      text_factor= factors[rownames(data),input$pclus_newdata_text_factor, drop=F]
      points_tomap$label<-text_factor[rownames(points_tomap),]
    }
    if(length(input$pclus_newdata_points_factor)>0){
      req(input$pclus_newdata_points_factor%in%colnames(factors))
      points_factor= factors[rownames(data),input$pclus_newdata_points_factor, drop=F]
      points_tomap$point<-points_factor[rownames(points_tomap),]
      attr(points_tomap,"namepoints")<-input$pclus_newdata_points_factor
    }
    vals$hc_mapsom<-points_tomap
    points_tomap
  })


  ##





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

  observeEvent(ignoreInit = T,input$pclus_newdata_text_palette,{
    vals$pclus_newdata_text_palette<-input$pclus_newdata_text_palette
  })
  observeEvent(ignoreInit = T,input$pclus_newdata_text_factor,{
    vals$pclus_newdata_text_factor<-input$pclus_newdata_text_factor
  })
  observeEvent(ignoreInit = T,input$pclus_newdata_text_size,{
    vals$pclus_newdata_text_size<-input$pclus_newdata_text_size
  })
  ##

  observe({
    req(is.null(vals$hcsom_newdata))
    vals$hcsom_newdata<-F
  })
  output$hcsom_newdata_control<-renderUI({
    splitLayout(class="well3",
                div(style=' border-bottom: 1px solid',
                    div(
                      inline(checkboxInput("hcsom_newdata", span("+ Map new data", tiphelp("Check to add new data points to the trained SOM","right"),uiOutput("hcsom_newdata_mess")),vals$hcsom_newdata, width = '250px'))
                    ),

                    uiOutput("out_hcsom_whatmap"),

                )

    )
  })

  output$hcsom_newdata_mess<-  renderUI({
    req(isTRUE(input$hcsom_newdata))
    span("select a gradient palette in '+ Points' to differentiate between training and the new data", style="color: gray")

  })

  output$out_hcsom_whatmap<-renderUI({
    req(input$hcsom_newdata)
    req(isTRUE(input$hcsom_newdata))
    req(input$model_or_data)
    req(input$model_or_data=="som codebook")
    layers<-getsom_layers()
    div(style="margin-left: 20px;",
        strong("Layers:"),
        lapply(layers,function(x){
          div(class="map_control_style2",style="color: #05668D",
              inline(checkboxInput(paste0("hcsom_layer",x),x,T)),
              inline(uiOutput(paste0("hcsom_piclayer",x)))
          )
        })
    )
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
          pickerInput(paste0("hcsom_newdata_layer",x), NULL, choices_temp)
        }
      })
    })
  })




  observeEvent(ignoreInit = T,input$hcsom_whatmap,{
    vals$hcsom_whatmap<-input$hcsom_whatmap
  })

  observeEvent(ignoreInit = T,input$hcsom_newdata,{
    vals$hcsom_newdata<-input$hcsom_newdata
  })



  observeEvent(ignoreInit = T,input$som_hc,{
    vals$cur_som_hc<-input$som_hc
  })
  observeEvent(ignoreInit = T,input$fixname,{
    vals$fixname<-input$fixname
  })
  observeEvent(ignoreInit = T,input$fixmodel,{
    vals$fixmodel<-input$fixmodel
  })

  observeEvent(ignoreInit = T,input$tools_savehc,{
    if(is.null(vals$fixname)){
      vals$fixname<-F
    }
    if(input$tools_savehc %% 2) {
      vals$hand_save<-"Save Clusters"
      vals$hand_save2<-p(p(em(input$data_hc,style="color: gray"),strong("::"),em("Factor-Attribute",style="color: gray"), strong("::")))
      vals$hand_save3<-div(
        checkboxInput("fixname","Fix the datalist name",vals$fixname),
        checkboxInput("fixmodel","Fix the model name",vals$fixmodel),
      )
      showModal(
        hand_save_modal()
      )
    }
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
        sommodel<-paste0(input$som_hc,"_")
      }
    }
    name0<-paste0(datalist,sommodel,name0)
    data<-attr(vals$saved_data[[input$data_hc]],"factors")
    name1<-make.unique(c(colnames(data),name0), sep="_")
    name1[ncol(data)+1]


  })
  output$disthc<-renderUI({
    req(input$model_or_data=="data")
    div(
      div("Distance:"),
      div(
        pickerInput(
          "disthc",
          NULL,
          choices = c('bray', "euclidean", 'jaccard'),
          width="120px"
        )
      )
    )
  })
  sc_rf_elbow<-reactive({
    fluidRow(
      column(12,
             uiOutput("elbowcontrol"))






    )
  })
  labhc<-reactive({
    req(input$labhc)
    as.character(attr(getdata_hc(),"factors")[rownames(getdata_hc()),as.character(input$labhc)])
  })
  output$hcdata_plot<-renderUI({
    req(input$model_or_data=='data')
    renderPlot({

      req(input$method.hc0)

      hc<-cutdata1(data=getdata_hc(), method.hc=input$method.hc0, dist=input$disthc)
      plot(hc,labels = as.character(labhc()),main = input$hc_title)
      vals$hc_tab1_plot<-recordPlot()
      vals$hc_tab1_plot
    })
  })
  output$hcmodel_plot<-renderUI({
    req(input$model_or_data=='som codebook')
    renderPlot({
      plot(cutm(getmodel_hc(), input$method.hc0), main = input$hc_title)
      vals$hc_tab1_plot<-recordPlot()
      vals$hc_tab1_plot
    })
  })

  observeEvent(ignoreInit = T,input$labhc,{
    vals$labhc<-input$labhc
  })
  output$hclabs_control <-renderUI({
    req(input$model_or_data=='data')
    pickerInput(
      "labhc",
      "Labels",
      choices = c(colnames(attr(getdata_hc(),"factors"))),
      width="200px",
      selected=vals$labhc
    )

  })



  observeEvent(ignoreInit = T,input$fheight,{vals$cur_fheight<-input$fheight})
  observeEvent(ignoreInit = T,input$fwidth,{vals$cur_fwidth<-input$fwidth})
  observeEvent(ignoreInit = T,input$fres,{vals$cur_fres<-input$fres})
  observeEvent(ignoreInit = T,input$pointsize,{vals$cur_pointsize<-input$pointsize})
  observeEvent(ignoreInit = T,input$fformat,{vals$cur_fformat<-input$fformat})

  downplot_center<-reactive({
    modalDialog(
      column(12,
             splitLayout(
               numericInput("fheight", "Height (cm)", min=2, max=15, step=1, value = vals$cur_fheight),
               numericInput("fwidth", "Width (cm)", min=2, max=15, step=1, value = vals$cur_fwidth),
               selectInput("fres", "Res", choices=c("100","200","300"), selected = vals$cur_fres),
               numericInput("pointsize", "pointsize",value=vals$cur_pointsize,step=1),
               selectInput("fformat", "File type", choices=c("png","tiff","jpeg","pdf","svg"), selected = vals$cur_fformat, multiple = FALSE, selectize = TRUE),
               div(style="margin-top: 25px",downloadButton("bn_download",NULL, style="button_active")),

             ),

             column(12, withSpinner(type=8,color="SeaGreen",imageOutput("plotoutput")))
      ),
      title=p(strong("action:"),"download", vals$hand_plot),
      easyClose = T
    )
  })
  get_plot<-reactive({

  })
  fn_downloadf<-reactive({
    if(input$fformat=="svg") filename<-paste0(vals$cur_data,"_",vals$hand_plot,".svg",sep="")
    if(input$fformat=="png") filename<-paste0(vals$cur_data,"_",vals$hand_plot,".png",sep="")
    if(input$fformat=="tiff") filename<-paste0(vals$cur_data,"_",vals$hand_plot,".tif",sep="")
    if(input$fformat=="jpeg") filename<-paste0(vals$cur_data,"_",vals$hand_plot,".jpg",sep="")
    if(input$fformat=="pdf") filename<-paste0(vals$cur_data,"_",vals$hand_plot,".pdf",sep="")
    return(filename)
  })







  output$bn_download<-downloadHandler(
    filename = fn_downloadf,
    content = function(file) {
      fn_download()
      dev.off()
      file.copy(fn_downloadf(), file, overwrite=T)
    }
  )

  ## observes PLOT





  observeEvent(ignoreInit = T,input$downp_map,{
    vals$hand_plot<-"Map"
    if(isTRUE(input$surface_map)){
      vals$hand_plot<-"Surface map"
    }
    if(isTRUE(input$scatter_3d)){
      vals$hand_plot<-"Scatter 3D"
    }

    if(isTRUE(input$mantel_map)){
      vals$hand_plot<-"mantel plot"
    }
    if(isTRUE(input$stack_map)){
      vals$hand_plot<-"stacked raster map"
    }
    if(isTRUE(input$stack_scatter_3d)){
      vals$hand_plot<-"stacked scatter map"
    }

    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,file=input$var_map)
  })
  #outputOptions(output, "tbl_order", suspendWhenHidden = FALSE)



  ## transformations

  getdata_upload<-reactive({
    req(is.data.frame(data_cogs$df))
    data_cogs$df
  })










  nul<-eventReactive(input$cogs,{
    req(isTRUE(input$cogs))

    showModal(
      div(
        id="change_modal",
        modalDialog({
          div(
            uiOutput("histo"),
            uiOutput("tools_upload")



          )
        },
        footer = NULL,
        size="s"
        )
      )
    )


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
    if(is.null(vals$hist_tabs)){vals$hist_tabs<-"Summary"}
    req(ncol(data_cogs$df)<10000)
    column(12,style="padding: 5px; font-size: 12px",
           column(12,em("Track changes"),style="background: SeaGreen; color: white"),
           tabsetPanel(
             id="hist_tabs", selected=vals$hist_tabs,
             tabPanel("Changes",uiOutput("track_change")),
             tabPanel("Summary",uiOutput("hist_d0")),
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
    renderPrint({
      req(is.data.frame(data_cogs$df))
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
    req(is.data.frame(data_cogs$df))
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




  leave_changes<-reactive({

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

  ###
  ###


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
  output$attibutes_tree<-renderUI({
    choices<-vals$dl_datalistnames
    saved_ensemble<-vals$dl_saved_ensemble
    div(class="divnull",
        pickerInput("data_dlmX","Saved datalist",choices = c('nenhum_daqui', choices),selected = "nenhum_daqui"))
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

  ##
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
  output$newpalette_created<-renderUI({
    req(!is.null(vals$created_pal))
    div(
      div(em("Palette successfully created"))

    )
  })
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
  output$maxcolor<-renderUI({
    req(maxcol()>=48)
    em("The Maximum number of colors allowed has been reached")
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
    palette1<-dataURI(file =outfile,mime = "image/png")
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
      validate(need(nrow(vals$saved_data[[vals$cur_data]])<=1000,"This functionality is only available for data with less than 1000 observations"))
      req(vals$cur_data)
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
            #uiOutput("filter_teste")
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
    library(caret)

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

  output$filter_teste<-renderUI({
    renderPrint({
      get_corrdata()
    })
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
  output$cutlevel_obs<-renderUI({
    div(id="filter_cut",
        column(12,checkboxGroupInput('cutlevel_obs',NULL,levels(
          attr(vals$saved_data[[vals$cur_data]],"factors")
        ))),


        bsTooltip("filter_cut","The factor level to restric the observations")


    )
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
      vals$cur_data,
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
  }

  )


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
      newdata<-data.frame(vegan::rrarefy(data,input$rrarefy_sample))
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

           div(style="margin-top: 25px;",
               tabsetPanel(
                 type = "hidden",
                 tabPanel("intro",
                          tabItems(
                            tabItem(
                              tabName = "menu_intro",
                              column(8,style="margin-top: -65px; background: white;",
                                     div(style="margin-top: 25px; background: white;",
                                         tabsetPanel(
                                           tabPanel(value="intro1",
                                                    strong("Welcome"),
                                                    textintro(),
                                                    uiOutput("license")
                                           ),
                                           tabPanel(strong("Authors"),value="intro2", textauthors()),
                                           tabPanel(  strong("Version"),value="intro5",
                                                      fluidRow(
                                                        style="background: white",
                                                        column(12, style="margin-top 25px;",
                                                               p(h4(style="margin-top 25px",span("iMESc", style="font-family: 'Alata', sans-serif;")),version),
                                                               p(strong("Last update:"),last_update)

                                                        ),
                                                        column(12, h5(strong("Technical Development Specifications:")),
                                                               uiOutput("sys_info")
                                                        ),
                                                        column(12,em("developed by Danilo C Vieira"))


                                                      )
                                           ),
                                           tabPanel(
                                             strong("References"),value="intro5",
                                             column(12,style="margin-top: 50px",
                                                    actionLink("imesc_help2", span('iMESc help',icon("fas fa-question-circle")))
                                             ))

                                         )
                                     ))),
                            # training panel

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
                                    column(12,uiOutput('clustering_panel'))),
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
                            tabItem(tabName = "menu_sim",
                                    uiOutput('menu_sim_out')),
                            tabItem(tabName = "menu_kmeans",
                                    uiOutput('menu_kmeans_out')),
                            tabItem(  tabName = "menu_maps",
                                      column(12,uiOutput('map_panel'))

                            ),
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


  output$license<-renderUI({
    column(12,
           column(12, h4(strong('Copyright Notice'), style="color: #05668D;"),
                  p('© 2023 Danilo Candido Vieira and Gustavo Fonseca. All rights reserved.'),
                  p('The content, coding, and related assets associated with the iMESc project are the sole property of the authors, Danilo Candido Vieira and Gustavo Fonseca. Redistribution, modification, commercial use, or any other form of utilization of this project is strictly prohibited without the express written consent of the authors.'),

                  h4(strong('License'), style="color: #05668D;"),
                  p("This project is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International (CC BY-NC-ND 4.0) license. This license allows others to download the project and share it with others as long as they credit the authors, but they can't change it in any way or use it commercially."),

                  h4(strong("Disclaimer"), style="color: #05668D;"),
                  p("The iMESc application ('the software') is provided 'as-is', without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose, and non-infringement. In no event shall the authors or copyright holders be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of, or in connection with the software or the use or other dealings in the software."),

                  p("While efforts have been made to ensure the accuracy and reliability of the software, the authors cannot guarantee that the software will always operate error-free or that it is completely secure. Users are advised to exercise their own skill and care with respect to their use of the software."),

                  p("The software may contain links to other websites or resources. The authors are not responsible for the availability of such external sites or resources, and do not endorse and are not responsible or liable for any content, advertising, products, or other materials on or available from such sites or resources.")
           ))
  })
  output$menu_spd2_out<-renderUI({
    res<module_ui_spd("spd")

    mod_spd2<-callModule(module_server_spd,"spd",vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    res
  })

  output$menu_spd_out<-renderUI({

    res<-module_ui_spd("module_spd")
    mod_spd<-callModule(module_server_spd, "module_spd",  vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs,df_symbol=df_symbol)
    removeModal()
    res
  })

  ##Simulate
  output$menu_sim_out<-renderUI({


    sidebarLayout(
      sidebarPanel(width = 3,
                   div(class="map_control_style",

                       uiOutput("data_sim"),
                       uiOutput("model_sim"),
                       actionButton("sim_run","RUN"),
                       splitLayout(

                         actionButton("sel_all_M","sel_all_M"),
                         actionButton("unsel_all_M","unsel_all_M"),
                         actionButton("sel_all_R","sel_all_R"),
                         actionButton("unsel_all_R","unsel_all_R"),
                       ),
                       #div(DT::dataTableOutput("sim_vars2")),
                       div(
                         style="height:800px;overflow-y: scroll",uiOutput("sim_vars")
                       ),

                       actionButton("sim_reset","Reset"),
                       #uiOutput("sim_results0"),
                       uiOutput("sim_results")






                   )),
      mainPanel(
        uiOutput('sim_mapvar'),
        uiOutput("sim_latlong"),


      )
    )
  })

  output$sim_latlong<-renderUI({
    req(input$data_sim)
    data<-vals$saved_data[[input$data_sim]]
    coords<-attr(data,"coords")
    longco<-c(range(coords$LONG),sort(c(max(coords$LONG),mean(coords$LONG))))
    latco<-c(range(coords$LAT),sort(c(max(coords$LAT),mean(coords$LAT))))

    column(12,
           column(10,noUiSliderInput("long","Long",min=longco[1],max= longco[2],c(longco[3] ,longco[4]), width="100%"),offset = 2),
           column(2,noUiSliderInput("lat","Lat",min=latco[1],max= latco[2],c(latco[3] ,latco[4]),orientation = "vertical",height ="400px",direction="rtl")),
           column(10,uiOutput("map_pic")))
  })
  vals_sim<-reactiveValues()
  output$data_sim<-renderUI({
    rfs<-lapply(vals$saved_data,function(x) attr(x,"rf"))
    choices=names(which(!unlist(lapply(rfs,is.null))))

    pickerInput("data_sim","Data",choices, selected=vals$cur_data_sim)
  })

  observeEvent(ignoreInit = T,input$data_sim,{
    vals$cur_data_sim<-input$data_sim
  })
  getsim_params<-reactive({
    req(input$data_sim)
    data<-vals$saved_data[[input$data_sim]]
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    imp<-varImp(m)
    variables<-names(sort(apply(imp$importance,1,mean),decreasing=T))


    res0<-data.frame(vars=variables)
    res0$sim_m<-unlist(lapply(variables,function(x){input[[paste0("sim_m",x)]]}))
    res0$sim_r<-unlist(lapply(variables,function(x){input[[paste0("sim_r",x)]]}))
    res0$sim_a<-unlist(lapply(variables,function(x){input[[paste0("sim_a",x)]]}))
    req(length(res0$sim_m)>0)
    req(length(res0$sim_r)>0)
    req(length(res0$sim_a)>0)

    vals_sim$sim_res0<-res0
    res0

  })
  sim_picsample<-reactive({
    data<-vals$saved_data[[input$data_sim]]
    coords<-attr(data,"coords")
    rang_x<-input$long
    rang_y<-input$lat
    df<-data.frame(x=between(coords[,1],rang_x[1],rang_x[2]),
                   y=between(coords[,2],rang_y[1],rang_y[2]))
    rownames(data)[which(rowSums(df)==2)]

  })
  output$sim_results0<-renderUI({
    req(input$data_sim)


    div(

      renderPrint(data.frame( vals_sim$sim_res0))

    )})
  output$sim_mapvar<-renderUI({
    req(input$data_sim)
    req(input$data_sim%in%names(vals$saved_data))
    data<-vals$saved_data[[input$data_sim]]
    req(length(attr(data,"rf"))>0)
    req(input$model_sim%in%names(attr(data,"rf")))

    m<-attr(data,"rf")[[input$model_sim]][[1]]
    imp<-varImp(m)
    variables<-names(sort(apply(imp$importance,1,mean),decreasing=T))
    splitLayout(
      pickerInput("sim_mapvar","Variable map",variables, options=list(container="body")),
      uiOutput("sim_map_filter1"),
      uiOutput("sim_map_filter2"),
      uiOutput("sim_rfresult")
    )
  })

  filtersim_map<-reactive({
    data<-vals$saved_data[[input$data_sim]]
    pic<-rownames(data)
    if(length(input$var_sim_map_filter1)>0){
      if(input$var_sim_map_filter1!="None"){
        factors<-attr(data,"factors")
        filtro<-as.character(input$var_sim_map_filter1)
        filtro2<-as.character(input$var_sim_map_filter2)
        pic0<-which(as.character(factors[, filtro]) == filtro2)
        pic<-pic[pic0]
      }
    }
    pic
  })

  output$sim_map_filter1<-renderUI({
    req(input$data_sim)
    data<-vals$saved_data[[input$data_sim]]
    factors<-attr(data,"factors")
    inline(pickerInput("var_sim_map_filter1",label = "Filter",choices = c("None",colnames(factors)), width="150px"))
  })

  output$sim_map_filter2<-renderUI({
    req(input$var_sim_map_filter1!="None")
    data<-vals$saved_data[[input$data_sim]]
    factors<-attr(data,"factors")
    fac<-factors[,input$var_sim_map_filter1]
    inline(pickerInput("var_sim_map_filter2",label = "Level",choices = levels(fac), width="150px"))
  })


  output$sim_mapfilter<-renderUI({

  })
  output$sim_vars<-renderUI({
    req(input$data_sim)
    data<-vals$saved_data[[input$data_sim]]
    req(length(attr(data,"rf"))>0)
    req(input$model_sim%in%names(attr(data,"rf")))

    m<-attr(data,"rf")[[input$model_sim]][[1]]
    req(length(m)>0)
    imp<-varImp(m)
    variables<-names(sort(apply(imp$importance,1,mean),decreasing=T))
    lapply(variables,function(x){
      div(
        div(strong(x)),
        div(
          inline(checkboxInput(paste0("sim_m",x),'Multiply by')),
          inline(numericInput(paste0("sim_a",x),NULL,1.1, width="75px")),
          inline(checkboxInput(paste0("sim_r",x),'Randomize'))

        ))
    })
  })


  output$model_sim<-renderUI({
    req(input$data_sim)
    data<-vals$saved_data[[input$data_sim]]

    choices<-names(attr(data,"rf"))
    pickerInput("model_sim","model",choices)

  })
  output$sim_results<-renderUI({
    req(input$data_sim)
    data<-vals$saved_data[[input$data_sim]]
    req(length(attr(data,"rf"))>0)
    req(input$model_sim%in%names(attr(data,"rf")))
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    renderPrint(m)
  })
  output$sim_rfresult<-renderUI({
    req(input$data_sim)
    data<-vals$saved_data[[input$data_sim]]
    req(length(attr(data,"rf"))>0)
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    newdata<-vals_sim$newdata
    pred<-predict(m,as.matrix(newdata))
    names(pred)<-rownames(newdata)
    obs1<-m$trainingData[,1]
    names(obs1)<-rownames(m$trainingData)
    obs<-factor(c(obs1,attr(m,"sup_test")))
    df<-data.frame(cbind(obs,pred))
    rownames(df)<-rownames(newdata)

    vals_sim$pred<-df
    vals_sim$error<-df[,2]!=df[,1]
    res<-postResample( factor(df[,2]),
                       factor(df[,1]))
    renderPrint(res)
  })
  output$map_pic<-renderUI({
    req(input$data_sim)
    data0<-data<-vals$saved_data[[input$data_sim]]
    coords<-attr(data,"coords")
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    data<-vals_sim$newdata
    xx<-c(input$long[1],input$long[1],input$long[2],input$long[2])
    yy<-c(input$lat[1],input$lat[2],input$lat[2],input$lat[1])


    var<-data[,input$sim_mapvar]
    cex=var/max(var)*5
    req(is.numeric(cex))
    cutcex<-cut(cex,10)
    cols<-vals$newcolhabs[["turbo"]](10)


    rang_x<-input$long
    rang_y<-input$lat

    observeEvent(ignoreInit = T,input$sim_tab,{
      vals$sim_tab_cur<-input$sim_tab
    })
    df<-data.frame(x=between(coords[,1],rang_x[1],rang_x[2]),
                   y=between(coords[,2],rang_y[1],rang_y[2]))

    div(
      tabsetPanel(id="sim_tab",selected=vals$sim_tab_cur,
                  tabPanel("Value",value="tab_01",
                           renderPlot({
                             plot(coords[rownames(data),], cex=cex, col=cols[cutcex])
                             polygon(xx,yy)
                             legend("topl",legend=levels(cutcex),col = cols, pch=1)
                           },height=550, width=800)),
                  tabPanel("Accuracy",value="tab_02",
                           renderPlot({
                             plot(coords[rownames(data),],  col= c("blue","red")[as.factor(vals_sim$error)], pch=16)

                             polygon(xx,yy)
                             legend("topl",legend=levels(cutcex),col = cols, pch=1)


                           },height=550, width=800)),

                  tabPanel("Map",value="tab_03",
                           renderPlot({
                             df<-vals_sim$pred[filtersim_map(),,drop=F]
                             colnames(df)<-c("obs",'pred')
                             df$pred<-factor(df$pred)
                             pizza_fac<-attr(data0,"factors")[rownames(df),"EST"]


                             map_discrete_variable(df,get="pred",coords=coords,base_shape=attr(data0,'base_shape'),layer_shape=attr(data0,"layer_shape"),points=T,colored_by_factor=df["pred"], newcolhabs = vals$newcolhabs,symbol =16 ,factors=NULL,
                                                   scalesize_size = F,
                                                   scalesize_color=F,
                                                   as_factor=F,
                                                   bmu=F,
                                                   pie=T,
                                                   facpizza=pizza_fac
                             )
                             # polygon(xx,yy)
                             # legend("topl",legend=levels(cutcex),col = cols, pch=1)
                           },height=550, width=800),
                           renderPrint({
                             df<-vals_sim$pred
                             colnames(df)<-c("obs",'pred')
                             df
                           })

                  ),
                  tabPanel("Map2",value="tab_04",
                           renderPlot({
                             df<-vals_sim$pred[filtersim_map(),,drop=F]
                             colnames(df)<-c("obs",'pred')
                             df$pred<-factor(df$pred)
                             re<-df$pred==df$obs
                             re[which(re==T)]<-"Acertos"
                             re[which(re==F)]<-"Erros"
                             re<-data.frame(re)
                             rownames(re)<-rownames(df)
                             colnames(re)<-"pred"
                             re$pred<-factor(re$pred)
                             pizza_fac<-attr(data0,"factors")[rownames(df),"EST"]
                             df$pred<-re$pred


                             map_discrete_variable(df,get="pred",coords=coords,base_shape=attr(data0,'base_shape'),layer_shape=attr(data0,"layer_shape"),points=T,colored_by_factor=re["pred"], newcolhabs = vals$newcolhabs,symbol =16 ,factors=NULL,
                                                   scalesize_size = F,
                                                   scalesize_color=F,
                                                   as_factor=F,
                                                   bmu=F,
                                                   pie=T,
                                                   facpizza=pizza_fac
                             )
                             # polygon(xx,yy)
                             # legend("topl",legend=levels(cutcex),col = cols, pch=1)
                           },height=550, width=800),
                           renderPrint({
                             df<-vals_sim$pred
                             colnames(df)<-c("obs",'pred')
                             df
                           })

                  )

      ),

      splitLayout(renderPlot(boxplot(vals$saved_data[[input$data_sim]][sim_picsample(),input$sim_mapvar])),
                  renderPlot( boxplot(data[sim_picsample(),input$sim_mapvar]))),

      renderPrint({
        res<-sim_picsample()

        vals_sim$sim_ids<-res
        res
      })


    )
  })
  observeEvent(ignoreInit = T,input$sel_all_M,{
    data<-vals$saved_data[[input$data_sim]]
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    imp<-varImp(m)
    variables<-names(sort(apply(imp$importance,1,mean),decreasing=T))
    res0<-data.frame(vars=variables)
    for(x in variables){
      updateTextInput(session,paste0("sim_m",x), value=T)

    }
  })
  observeEvent(ignoreInit = T,input$unsel_all_M,{
    data<-vals$saved_data[[input$data_sim]]
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    imp<-varImp(m)
    variables<-names(sort(apply(imp$importance,1,mean),decreasing=T))
    res0<-data.frame(vars=variables)
    for(x in variables){
      updateTextInput(session,paste0("sim_m",x), value=F)

    }
  })
  observeEvent(ignoreInit = T,input$sel_all_R,{
    data<-vals$saved_data[[input$data_sim]]
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    imp<-varImp(m)
    variables<-names(sort(apply(imp$importance,1,mean),decreasing=T))
    res0<-data.frame(vars=variables)
    for(x in variables){
      updateTextInput(session,paste0("sim_r",x), value=T)

    }
  })
  observeEvent(ignoreInit = T,input$unsel_all_R,{
    data<-vals$saved_data[[input$data_sim]]
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    imp<-varImp(m)
    variables<-names(sort(apply(imp$importance,1,mean),decreasing=T))
    res0<-data.frame(vars=variables)
    for(x in variables){
      updateTextInput(session,paste0("sim_r",x), value=F)

    }
  })
  observeEvent(ignoreInit = T,input$sim_run,{
    vals_sim$newdata<-vals$saved_data[[input$data_sim]]

    vals_sim$sim_res0<-res0<-getsim_params()

    res0<-vals_sim$sim_res0
    ids<-vals_sim$sim_ids

    data<-vals$saved_data[[input$data_sim]]
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    obs<-factor(c(m$trainingData[,1],attr(m,"sup_test")))
    newdata<-rbind(m$trainingData[-1],attr(m,"test"))



    for(i in 1:nrow(res0)){
      if(any(res0[c('sim_m','sim_r')][i,])){
        picvar<-as.character(res0$vars[i])
        if(isTRUE(res0[i,'sim_m'])){
          newdata[ids,picvar]<-newdata[ids,picvar]*res0$sim_a[i]
        }
        if(isTRUE(res0[i,'sim_r'])){
          newdata[ids,picvar]<-sample(newdata[ids,picvar])
        }
      }
      vals_sim$newdata<-newdata
    }

  })
  observeEvent(ignoreInit = T,input$sim_reset,{
    vals_sim$newdata<-vals$saved_data[[input$data_sim]]
  })
  observe({
    req(input$data_sim)
    if(is.null(vals_sim$newdata)){
      vals_sim$newdata<-vals$saved_data[[input$data_sim]]
    }
  })
  observeEvent(ignoreInit = T,input$data_sim,{
    vals_sim$newdata<-NULL
  })
  observeEvent(ignoreInit = T,input$sim_vars2_cell_edit, {
    row <-input$sim_vars2_cell_edit$row
    clmn<-input$sim_vars2_cell_edit$col
    value<-if(input$sim_vars2_cell_edit$value==""){NA}else{as.numeric(input$sim_vars2_cell_edit$value)}
    vals_sim$sim_par[row, clmn]<-value
  })
  observe({
    req(input$data_sim)
    req(input$model_sim)
    data<-vals$saved_data[[input$data_sim]]
    m<-attr(data,"rf")[[input$model_sim]][[1]]
    imp<-varImp(m)
    variables<-names(sort(apply(imp$importance,1,mean),decreasing=T))
    n<-length(variables)
    df<-data.frame(Randomize=rep('Randomize', n),Multiply=rep('Multiply', n),Amount=rep(5,n))
    rownames(df)<-variables
    vals_sim$sim_par<-df

  })
  ####


  output$menu_div_out<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found"))


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










  bag_name_new<-reactive({
    bag<-1
    name0<-input$data_bank
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste(name0,bag)

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


  output$data_create<-renderUI({
    req(length(vals$hand_save)>0)
    req(input$hand_save=="create")

    data_store$df<-F

    res<-switch (vals$hand_save,
                 "create_codebook"=textInput("codebook_newname", NULL,paste0(input$data_hc,"Codebook")),
                 "Transpose Datalist"=textInput("transp_newname", NULL,bag_name()),
                 "Save new som in" = textInput("model_newname", NULL,bag_somname()),
                 "Save changes"= textInput("data_newname", NULL,bag_name()),
                 "Include binary columns in the Numeric-Attribute"= textInput("data_newname", NULL,bag_name_new()),
                 "Include ordinal variables in the Numeric-Attribute"= textInput("data_newname", NULL,bag_name_new()),
                 "Save Clusters"= textInput("hc_newname", NULL,bag_hc()),
                 "Save new data clusters"= textInput("mc_newname", NULL,bag_mp()),
                 "Create data list from aggregation results"= textInput("agg_newname", NULL,bag_agg()),
                 "Add an Extra-Layer-Attribute to the Datalist"=textInput("extra_layer_newname", NULL,bag_extralayer()),

                 "Save diversity results"= textInput("div_newname", NULL,bag_divname()),
                 "Save rarefied data"= textInput("rare_newname", NULL,bag_name()),

                 "add partition"= textInput("split_newname", NULL,bag_partition()),
                 "Save som predictions"= textInput("predsom_newname", NULL,predsom_name()),
                 "Save errors from som predictions (X)"= textInput("predsom_eX_newname", NULL,bag_sompred_eX()),
                 "Save errors from som predictions (Y)"= textInput("predsom_eY_newname", NULL,bag_sompred_eY()),


                 "Save interpolation model"=
                   {

                     textInput("interps_newname", NULL,bag_mapname())
                   },
                 "Save discrete model"=
                   {

                     textInput("discrete_newname", NULL,bag_mapname())
                   },
                 "Save raster"=
                   {

                     textInput("interps_newname", NULL,bag_mapname())
                   },
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
                 'Save Clusters' = selectInput("hc_over", NULL,choices=c(colnames(attr(getdata_hc(),"factors"))),selectize=T),
                 'Save new data clusters' = selectInput("mc_over", NULL,choices=c(colnames(attr(vals$saved_data[[vals[[ns_tab5("somplot_args")]]$newdata]],"factors"))),selectize=T),




                 'Create data list from aggregation results' = selectInput("agg_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save diversity results' = selectInput("div_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save rarefied data' = selectInput("rare_over", NULL,choices=c(names(vals$saved_data)),selectize=T),



                 'Save errors from som predictions (Y)' = selectInput("predsom_eY_over", NULL,choices=c(names(vals$saved_data)),selectize=T),
                 'Save errors from som predictions (X)' = selectInput("predsom_eX_over", NULL,choices=c(names(vals$saved_data)),selectize=T),


                 'Add an Extra-Layer-Attribute to the Datalist' = selectInput("extra_layer_over", NULL,choices=c(names(attr(vals$saved_data[[input$data_map]],"extra_shape"))),selectize=T),

                 'add partition' = selectInput("split_over", NULL,choices=colnames(attr(vals$saved_data[[input$data_upload]],"factors")),selectize=T),
                 'Save som predictions' = {
                   if(length(attr(vals$saved_data[[input$data_som]],"factors","predictions"))>0){
                     selectInput("predsom_over", NULL,choices=c(names(attr(vals$saved_data[[input$data_som]],"factors","predictions"))),selectize=T)
                   }else{
                     column(12,verbatimTextOutput("nopredictions"))
                   }

                 },

                 'Save interpolation model'= {
                   if(length(vals$saved_maps)>0){
                     selectInput("interps_over", NULL,choices=c(names(vals$saved_maps)),selectize=T, selected=input$saved_maps)
                   }else{
                     column(12,verbatimTextOutput("nointerpsmodel"))
                   }

                 },
                 'Save discrete model'= {
                   if(length(vals$saved_maps)>0){
                     selectInput("discrete_over", NULL,choices=c(names(vals$saved_maps)),selectize=T, selected=input$saved_maps)
                   }else{
                     column(12,verbatimTextOutput("nointerpsmodel"))
                   }

                 },
                 'Save raster'={
                   if(length(vals$saved_maps)>0){
                     selectInput("interps_over", NULL,choices=c(names(vals$saved_maps)),selectize=T, selected=input$saved_maps)
                   }else{
                     column(12,verbatimTextOutput("nointerpsmodel"))
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
  ## menu_explore
  ##

  observeEvent(ignoreInit = T,input$radio_cogs,{
    vals$impute<-F
  })








  observeEvent(ignoreInit = T,input$saved_maps,{
    if(input$saved_maps!="new map"){
      shinyjs::hide("map_out1")
      shinyjs::show("map_out2")
    } else{
      shinyjs::show("map_out1")
      shinyjs::hide("map_out2")
    }
  })


  fn_download<-function()
  {

    fheight<-input$fheight
    fwidth<-input$fwidth
    fres<-as.numeric(input$fres)

    if(input$fformat=="pdf"|input$fformat=="svg") fheight<-round(fheight*0.3937,2)
    if(input$fformat=="pdf"|input$fformat=="svg") fwidth<-round(fwidth*0.3937,2)

    if(input$fformat=="png") png(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize,type ="cairo-png")
    if(input$fformat=="tiff") tiff(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm",compression="lzw", pointsize = input$pointsize,type ="cairo")
    if(input$fformat=="jpeg") jpeg(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm",quality=100, pointsize = input$pointsize,type ="cairo")
    if(input$fformat=="pdf") pdf(fn_downloadf(), height=fheight, width=fwidth, pointsize = input$pointsize)
    if(input$fformat=="svg") svg(fn_downloadf(), height=fheight, width=fwidth, pointsize = input$pointsize)

    switch (vals$hand_plot,
            "variable summary" = {
              data=getdata_upload0()
              numerics<-data[,unlist(lapply(data,is.numeric))]
              replayPlot(vals$varplot)
            },
            "Dendrogram"={replayPlot(vals$pdend_plot)},



            "Hcut"={ hc_plot(phc(),col=getcolhabs(vals$newcolhabs,input$hcdata_palette,input$customKdata),labels=get_hcut_labels())},
            "codebook clusters"= {plot(vals$pclus_plot)},
            "Minimal Depth distribution"={plot(vals$rfd_res)},
            "Multi-way importance"={plot(vals$rfm_res)},

            "Map"={
              if(input$saved_maps=="new map"){plot(vals$map_res)} else{
                plot(vals$saved_maps[[input$saved_maps]])
              }
            },

            "Confusion Matrix - test RF"={plot(vals$conf_rf)},
            "Confusion Matrix RF"={plot(vals$cm_rf)},
            "Surface map"={replayPlot(vals$map_res)},
            "mantel plot"=replayPlot(mantel_plot$df),
            "Scatter 3D"=replayPlot(vals$map_res),
            "stacked raster map"=replayPlot(vals$map_res),
            "RF interactions"=plot(rf_inter$df),
            "RF ranking comparations"=replayPlot(rf_rank$df),
            "RF measure comparations"=replayPlot(rf_rel$df),
            "RF - Partial Dependence"=plot(rfbiplot1$df)
    )
  }





  cutsom.reactive<-reactive({
    req(input$model_or_data=="som codebook")
    req( input$customKdata)
    req(input$method.hc0)

    # req(input$hcdata_palette)
    m<-getmodel_hc()
    req(length(m)>0)
    #savereac()

    args<-list(m=m, k= input$customKdata,hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=NULL)

    somC<-do.call(cutsom_new,args)
    #print(somC)
    somC
  })

  getdown<-reactive({
    switch(vals$hand_down,
           "data"=vals$saved_data[[input$data_bank]],
           "factors"=attr(vals$saved_data[[input$data_bank]],"factors"),
           "coords"=attr(vals$saved_data[[input$data_bank]],"coords"),

           "som"=combsom_down()
    )
  })



  outputOptions(output, "side_map00", suspendWhenHidden = FALSE)





  runjs("Shiny.setInputValue('last_btn', 'main_panel');")

  #### PREVIEW-PLOT

  ## DOWNLOAD FUNCTION
  savereac<-reactive({
    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img
    tosave$ssom_tab0<-vals$ssom_tab0
    tosave$saved_maps<-vals$saved_maps
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


  #runjs("Shiny.setInputValue('radio_cogs', 'tools_drop5');")
  #runjs("Shiny.setInputValue('dcogs2', true);")
  # runjs("Shiny.setInputValue('bank_button', true);")
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



  observeEvent(ignoreInit = T,input$imesc_help2, {
    #shinyjs::hide("runButton")  # Hide the button
    browseURL("https://danilocvieira.github.io/iMESc_help")  # Redirect to the new application
  })

  observeEvent(ignoreInit = T,input$imesc_help, {
    shinyjs::hide("runButton")  # Hide the button
    browseURL("https://danilocvieira.github.io/iMESc_help")  # Redirect to the new application
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

  output$plotoutput<-renderImage({

    fheight<-input$fheight
    fwidth<-input$fwidth
    fres<-as.numeric(input$fres)
    png(paste0(vals$hand_plot,".png",sep=""), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize,type ="cairo-png")

    if(vals$hand_plot=="Training plot"){pchanges(vals$som_results)}
    if(vals$hand_plot=="Couting plot"){  pcounts(vals$som_results)}
    if(vals$hand_plot=="uMatrix"){   pUmatrix(vals$som_results)}
    if(vals$hand_plot=="BMUs"){replayPlot(vals$bmus_plot)}
    if(vals$hand_plot=="BMUs predictions"){replayPlot(vals$bmus_pred_plot)}
    if(vals$hand_plot=="property plot") {replayPlot(vals$pprop_plot)}
    if(vals$hand_plot=="Dendrogram") {replayPlot(vals$pdend_plot)}




    if(vals$hand_plot=="Hcut"){ hc_plot(phc(),col=getcolhabs(vals$newcolhabs,input$hcdata_palette,input$customKdata),labels=get_hcut_labels())}

    if(vals$hand_plot=="codebook clusters"){vals$pclus_plot}
    if(vals$hand_plot=="Minimal Depth distribution"){plot(vals$rfd_res)}

    if(vals$hand_plot=="Multi-way importance"){
      plot(vals$rfm_res)
    }
    if(vals$hand_plot=="RF - Partial Dependence"){
      plot(rfbiplot1$df)
    }

    if(vals$hand_plot=="Map"){
      if(input$saved_maps=="new map"){plot(vals$map_res)} else{
        plot(vals$saved_maps[[input$saved_maps]])
      }
    }



    if(vals$hand_plot=="Confusion Matrix SOM"){plot(vals$conf_som)}
    if(vals$hand_plot=="Confusion Matrix - test RF"){plot(vals$conf_rf)}
    if(vals$hand_plot=="Confusion Matrix RF"){plot(vals$cm_rf)}
    if(vals$hand_plot=="Surface map"){replayPlot(vals$map_res)}
    if(vals$hand_plot=="mantel plot"){replayPlot(mantel_plot$df)}
    if(vals$hand_plot=="Scatter 3D"){replayPlot(vals$map_res)}
    if(vals$hand_plot=="stacked raster map"){replayPlot(vals$map_res)}
    if(vals$hand_plot=="stacked scatter map"){replayPlot(vals$map_res)}
    if(vals$hand_plot=="RF interactions"){plot(rf_inter$df)}
    if(vals$hand_plot=="RF ranking comparations"){replayPlot(rf_rank$df)}
    if(vals$hand_plot=="RF measure comparations"){replayPlot(rf_rel$df)}


    dev.off()

    return(list(src = paste0(vals$hand_plot,".png",sep=""),
                contentType = "image/png",
                width = round((input$fwidth*as.numeric(input$fres))/2.54, 0),
                height = round((input$fheight*as.numeric(input$fres))/2.54, 0),
                alt = "plot"))
  },deleteFile=TRUE)
  ## Databank


  ## Descriptive tools





  ##

  output$menu_som_out<-renderUI({
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


  ##
  output$menu_som2_out<-renderUI({
    res<-module_ui_som2("som2")
    mod_som2<-callModule(module_server_som2,"som2",vals=vals, df_colors=vals$colors_img,newcolhabs=newcolhabs)
    res
  })


  observeEvent(input$dimension,{
    delay(500,{vals$dimension_display<-input$dimension})
  })







  output$sys_info<-renderUI({
    sys_info<-readRDS('inst/app/www/sys_info.rds')
    do.call('div',args=list(sys_info))
  })

  #final <- Sys.time()
  # tempo_carregamento_pacotes <- middle - inicio
  #tempo_carregamento_server <- final - middle
  exportTestValues(saved_data=vals$saved_data,data_cogs=data_cogs)


}
