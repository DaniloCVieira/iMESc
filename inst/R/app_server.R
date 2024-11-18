

#' @export
#' @noRd

app_server<-server<-function(input, output, session) {


  session$sendCustomMessage("update_loading_message", "initializing packages...")

  base_packs<-c("ggplot2","sf","kohonen","raster")
  sapply(seq_along(base_packs),function(i){
    message <- paste0(
      "<span>Initializing packages... <span style='color: orange;'>",
      base_packs[[i]],
      "</span></span>"
    )
    session$sendCustomMessage("update_loading_message", message)
    library(base_packs[[i]],character.only = T)

  })


  session$sendCustomMessage("update_loading_message", "almost ready...")



  t0<-Sys.time()
  # init_server<-Sys.time()
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
   session$onSessionEnded(function() {    stopApp()  })

  if ( dir.exists("inst2")) {
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  #server_init<-Sys.time()

  #ns_tab5<-NS('hc_tab5')
  once_load<-reactiveValues(df=0)


  shinyjs::onevent("mouseenter", "savepoint_out", hide(selector=".tooltip"))

  observeEvent(input$savepoint_out,ignoreInit = T,{

    vals$update_pp<-'tool10'
    updateTabsetPanel(session,"disable_tool",selected=vals$update_pp)
  })
  options(shiny.maxRequestSize=100*1024^3)



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
      "turbo" = viridis::turbo,
      "matlab.like2"= colorRamps::matlab.like2,
      "viridis" = viridis::viridis,
      "plasma" = viridis::plasma,
      "Rushmore1"=colorRampPalette(wesanderson::wes_palette("Rushmore1",100,type ="continuous")),
      "FantasticFox1"=colorRampPalette(wesanderson::wes_palette("FantasticFox1",100,type ="continuous")),
      "inferno"=viridis::inferno,
      "heat"=grDevices::heat.colors,
      "Blues"=colorRampPalette(RColorBrewer::brewer.pal(9,"Blues")),
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
      "goldenrod3" =  colorRampPalette("goldenrod3"),
      'spectral'=colorRampPalette(RColorBrewer::brewer.pal(11,name='Spectral')),
      'RdYlGn'=colorRampPalette(RColorBrewer::brewer.pal(11,name='RdYlGn')),
      'RdYlBu'=colorRampPalette(RColorBrewer::brewer.pal(11,name='RdYlBu')),
      'RdGy'=colorRampPalette(RColorBrewer::brewer.pal(11,name='RdGy')),
      'RdBu'=colorRampPalette(RColorBrewer::brewer.pal(11,name='RdBu')),
      'PuOr'=colorRampPalette(RColorBrewer::brewer.pal(11,name='PuOr')),
      'PRGn'=colorRampPalette(RColorBrewer::brewer.pal(11,name='PRGn')),
      'PiYG'=colorRampPalette(RColorBrewer::brewer.pal(11,name='PiYG')),
      'BrBG'=colorRampPalette(RColorBrewer::brewer.pal(11,name='BrBG')),



      "BuGn"=colorRampPalette(RColorBrewer::brewer.pal(9,"BuGn")),
      "BuPu"=colorRampPalette(RColorBrewer::brewer.pal(9,"BuPu")),
      "GnBu"=colorRampPalette(RColorBrewer::brewer.pal(9,"GnBu")),


      "Oranges"=colorRampPalette(RColorBrewer::brewer.pal(9,"Oranges")),
      "OrRd"=colorRampPalette(RColorBrewer::brewer.pal(9,"OrRd")),
      "PuBu"=colorRampPalette(RColorBrewer::brewer.pal(9,"PuBu")),


      "PuBuGn"=colorRampPalette(RColorBrewer::brewer.pal(9,"PuBuGn")),
      "PuRd"=colorRampPalette(RColorBrewer::brewer.pal(9,"PuRd")),
      "Purples"=colorRampPalette(RColorBrewer::brewer.pal(9,"Purples")),

      "RdPu"=colorRampPalette(RColorBrewer::brewer.pal(9,"RdPu")),
      "Reds"=colorRampPalette(RColorBrewer::brewer.pal(9,"Reds")),
      "YlGn"=colorRampPalette(RColorBrewer::brewer.pal(9,"YlGn")),


      "YlGnBu"=colorRampPalette(RColorBrewer::brewer.pal(9,"YlGnBu")),
      "YlOrBr"=colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrBr")),
      "YlOrRd"=colorRampPalette(RColorBrewer::brewer.pal(9,"YlOrRd"))









    ) ## in global
  )



  invert_color <- function(color) {
    # Convert the color to RGB
    rgb_val <- col2rgb(color)

    # Invert the RGB values (255 - original value)
    inverted_rgb <- 255 - rgb_val

    # Convert the inverted RGB values back to hexadecimal color
    inverted_hex <- rgb(inverted_rgb[1], inverted_rgb[2], inverted_rgb[3], maxColorValue = 255)

    return(inverted_hex)
  }

  get_colorimages<-reactive({

    vals$colors_img<-NULL
    for(i in 1:length(vals$newcolhabs)) {
      palname<-names(vals$newcolhabs)[i]
      palette_name<-paste("palette",i)
      outfile<-tempfile(fileext = ".png")

      png(outfile, height =70, width=300)
      par(mar=c(0,0,0,0),mai=c(0,0,0,0),mgp=c(0,0,0),oma=c(0,0,0,0),omi=c(0,0,0,0),xpd=T,ann=F, bg=NA, fg=NA)
      colors<-getcolhabs(vals$newcolhabs,names(vals$newcolhabs)[i],256)
      image(t(matrix(seq(0,1,length.out=256), nrow=1)), axes=F,col=  colors)
     # rect(0.3,0.3,0.7,0.8, col=adjustcolor("white",0.5))
     # text(0.5,.5,label=i,cex=5,col="black")

      style=   paste0("background:",adjustcolor("white",0.3),";position: absolute; left: 10px; z-index: 9999; top: 3px; padding: 3px; font-size:11px;color: black")





      dev.off()
      palette1<-base64enc::dataURI(file =outfile,mime = "image/png")
      vals$colors_img<-rbind(vals$colors_img,
                             data.frame(
                               val=palname,img= sprintf(
                                 HTML(
                                   paste0(
                                     img(src = palette1, height = '20',width = '100',style="margin-top:-40px;margin-bottom:-40px;margin-left:-20px;margin-right:-20px;"),
                                     div(em(palname),style=style)
                                   )
                                 )
                               )
                             ))
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


  output$imesc_title<-renderUI({
    gettitle()
  })
  gettitle<-reactive({
    div(style="color: #05668D", id="menu-title",


        switch(input$tabs,
               "menu_intro"={""},
               "menu_upload"={},
               'menu_explore'={},
               'menu_sl'={},

               'menu_som'={},

               'menu_som2'={},


               "menu_compare"={},

               'menu_kmeans'={},
               'menu_hc'={},

               'menu_maps2'={},
               'menu_box'={},

               'menu_div'={},
               'menu_down'={
                 h4(strong("Download Center"))
               }

        )

    )
  })








  output$map_header<-renderUI({
    sptools_data$server('sptools_data',vals)
    NULL
  })

  output$map_panels<-renderUI({
    div(
      sptools_data$ui("sptools_data"),
      uiOutput('map_header'),
      sptools_panels$ui("sptools_panels"),
      uiOutput("sptools_panels_server")
    )


  })

  output$sptools_panels_server<-renderUI({
    sptools_panels$server('sptools_panels',vals=vals)
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
    beepr::beep(10)

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
        inline(tipify_ui(uiOutput("qnote"),"Creates a note for the Datalist", placement = 'top'))
    )
  })
  output$imesc_footer<-renderUI({

    div(id="qsave_btns",
        inline(uiOutput("qsave_btn")),
        #inline(uiOutput("qload_btn")),
        inline(uiOutput("qcomment_btn")),
        shinyBS::bsTooltip('flashsave', "Creates savepoint file in your working directory", 'top'),
        shinyBS::bsTooltip('quick_load',  "Load savepoint file from your working directory", 'top'),
        shinyBS::bsTooltip('qnote_open',  "Creates a comment for the Datalist", 'top')
    )
  })


  observeEvent(input$dimension,{
    delay(500,{vals$dimension_display<-input$dimension})
  })




  output$preprocess<-renderUI({
    pre_process$server("preproc", vals)
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

  validate_datatalist<-reactive({
    if(!length(vals$saved_data)>0){

      div(style="padding: 40px",
          div(icon("triangle-exclamation", style="color: gold"),emgray('No',strong('Datalist',style="color: #05668D"),' found.')),
          div(emgray('iMESc requires at least one Datalist to access its modules.')),
          div(emgray('Please',strong('create a Datalist',style="color: #05668D"),' using the plus button in the',strong("Pre-Processing Tools",style="color: #05668D"))),
      )

    } else{
      NULL
    }
  })

  session$sendCustomMessage("update_loading_message", "Almost done...")
  observe({

    cond<-length(vals$saved_data)>0
    shinyjs::toggle('module_databank',condition=cond)
    shinyjs::toggle('module_desctools',condition=cond)
    shinyjs::toggle('module_divtools',condition=cond)
    shinyjs::toggle('module_spatialtools',condition=cond)
    shinyjs::toggle('module_supersom',condition=cond)
    shinyjs::toggle('module_hc',condition=cond)
    shinyjs::toggle('module_kmeans',condition=cond)
    shinyjs::toggle('module_sl',condition=cond)
    shinyjs::toggle('module_compare',condition=cond)

  })

  output$validate_databank<-renderUI({validate_datatalist()})
  output$validate_desctools<-renderUI({validate_datatalist()})
  output$validate_divtools<-renderUI({validate_datatalist()})
  output$validate_spatialtools<-renderUI({validate_datatalist()})
  output$validate_supersom<-renderUI({validate_datatalist()})
  output$validate_hc<-renderUI({validate_datatalist()})
  output$validate_kmeans<-renderUI({validate_datatalist()})
  output$validate_sl<-renderUI({validate_datatalist()})
  output$validate_comp<-renderUI({validate_datatalist()})
  output$menu_bank_out<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    div(

      databank_module$ui("module_databank"),
      uiOutput("databank_module_server")
    )



  })
  output$databank_module_server<-renderUI({
    databank_module$server('module_databank', vals=vals)

    NULL
  })



  output$menu_div_out<-renderUI({
    div(
      diversity_tool$ui("module_div"),
      uiOutput("module_div_server")
    )

  })

  output$module_div_server<-renderUI({
    mod_div<-diversity_tool$server("module_div",  vals=vals)
    NULL
  })
  output$menu_hc_out<-renderUI({

    div( hc_module$ui("module_hc"),
         uiOutput('module_hc_server'))

  })
  output$module_hc_server<-renderUI({
    hc_module$server('module_hc', vals=vals)
    NULL
  })
  output$menu_kmeans_out<-renderUI({
    div(
      k_means_module$ui("module_kmeans"),
      uiOutput("module_kmeans_out")
    )

  })
  output$module_kmeans_out<-renderUI({
    k_means_module$server('module_kmeans',vals)
    NULL
  })


  output$menu_upload_out<-renderUI({
    div(
      desctools$ui("module_desctools"),
      uiOutput("module_desctools_server")
    )

  })


  output$module_desctools_server<-renderUI({
    desctools$server("module_desctools",  vals=vals)
    NULL
  })



  output$menu_sl_out<-renderUI({
    div(
      caret_models$ui("module_sl"),
      uiOutput("module_sl_server")
    )

  })
output$module_sl_server<-renderUI({
  caret_models$server("module_sl",vals)
  NULL
})



  output$menu_som_out<-renderUI({
    div(imesc_supersom$ui("module_supersom",vals),
        uiOutput('som_out'))


  })
  output$som_out<-renderUI({
    imesc_supersom$server("module_supersom",vals=vals)
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

    column(12,
      compare_models$ui("module_comp"),
      uiOutput("module_comp_server")
    )

  })


  output$module_comp_server<-renderUI({
    compare_models$server("module_comp",  vals)
    NULL
  })

  observeEvent(vals$update_curtab,{
    updateTabsetPanel(session,"tabs",selected=vals$update_curtab)
    vals$update_curtab<-NULL
  })

  once_savepoint<-reactiveVal(F)
  observeEvent(input$savepoint_yes,{
    req(file.exists("savepoint.rds"))
    req(isFALSE(once_savepoint()))
    once_savepoint(TRUE)


    newvals0<-newvals<-readRDS('savepoint.rds')

    newvals <-newvals0[!names(newvals)%in%c('newcolhabs','colors_img')]


    newvals$module_states<-NULL
    newvals$update_state<-NULL


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



  observeEvent(vals$collectInputs,{
    req(vals$collectInputs==1)
    input_ids<- names(input)
    state <- collectInputs(session, input_ids)  # Collect all inputs dynamically
    vals$input_state<-state  # Save the current state
    vals$collectInputs<-2

  })

  module_ids<-c('module_databank','module_desctools','module_div', 'sptools_data','sptools_panels','module_supersom','module_hc','module_kmeans','module_sl','module_comp')
  observe({
    if(is.null(vals$fheight)){vals$fheight<-15}
    if(is.null(vals$fwidth)){vals$fwidth<-20}
    if(is.null(vals$fres)){vals$fres<-100}
    if(is.null(vals$pointsize)){vals$pointsize<-12}
    if(is.null(vals$fformat)){vals$fformat<-"png"}
  })

  observeEvent(vals$saved_data,{
    datalist_names_in<-as.character(sapply(vals$saved_data,function(x){
      attr(x,"datalist")
    }))
    if(any(datalist_names_in!=names(vals$saved_data))){
      for(i in seq_along(vals$saved_data) ){
        attr(vals$saved_data[[i]],"datalist")<-names(vals$saved_data)[i]
      }
    }
  })

  output$preprocessing<-renderUI({

  })


 # time000<-readRDS('inst/www/time000.rds')
  #timeend<-Sys.time()
  #print(timeend-time000)
  session$sendCustomMessage("hideSpinner", list())
  message("Ready")
}


