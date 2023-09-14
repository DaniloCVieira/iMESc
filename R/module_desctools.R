## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.


#' @export
module_ui_desctools<-function(id){

  ns<-NS(id)
  tagList(
    column(12,style="background: white",
         #  actionLink(ns("teste_comb"),"SAVE"),
          # uiOutput(ns("bug")),
           uiOutput(ns("upload_selection")),
           uiOutput(ns("panel_main"))
           )
  )

}

# Server
#' @export
module_server_desctools<-function (input,output,session,vals,df_colors,newcolhabs,df_symbol ){
  source("inst/app/www/funs_ordination_plot.R")
  ns<-session$ns
  pw_icon <- base64enc::dataURI(file = "inst/app/www/pwRDA_icon.png", mime = "image/png")
  smw_icon <- base64enc::dataURI(file = "inst/app/www/smw_icon.png", mime = "image/png")
  observeEvent(ignoreInit = T,input$teste_comb,{
    savereac()
  })
  symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
  df_symbol <- data.frame(
    val = c(16,15,17,18,8,1,5,3)
  )
  for(i in 1:length(symbols))
  {
    symbol1<-base64enc::dataURI(file = paste0('inst/app/www/pch',i,".png"), mime = "image/png")

    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}

  box_y_cur<-reactiveValues(df=1)
  runval<-reactiveValues(
    box="save_changes_nice",
    rid="save_changes_nice",
    pair="save_changes_nice",
    corr="save_changes_nice",
    mds="save_changes_nice",
    pca="save_changes_nice",
    rda="save_changes_nice"
  )
  filter_box2_cur<-reactiveValues(df=1)
  filter_box1_cur<-reactiveValues(df=1)
  boxplot_X_cur<-reactiveValues(df=1)
  bag_smw<-reactiveValues(df=F)
  aggreg_reac<-reactiveValues(df=0)
  updp<-reactiveValues(df=F)
  updp0<-reactiveValues(df=F)
  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic
  })

  observe({
    req(input$desc_options)
    if(input$desc_options%in%c('tab_scatter','tab_segrda','tab_rda',"tab_omi")){
      shinyjs::hide('upload_selection')
    }
    if(!input$desc_options%in%c('tab_scatter','tab_segrda','tab_rda',"tab_omi")){
      shinyjs::show('upload_selection')
    }
  })
  output$upload_selection<-renderUI({

    validate(need(length(vals$saved_data)>0,"No Datalist found"))
    column(12,
           strong("Datalist:"),
           inline(pickerInput(ns("data_descX"),NULL,choices=names(vals$saved_data),  selected=vals$cur_data, options=list(container="body","style-base" = "form-control", style = ""))))

  })

  observeEvent(ignoreInit = T,input$desc_options,{
    vals$cur_desc_options<-input$desc_options
  })

  observe({
    req(is.null(vals$cur_desc_options))
    vals$cur_desc_options<-'tab1'
  })

  observe({
    req(input$desc_options)
    req(!is.null(vals$cur_desc_options))
    req(!vals$cur_desc_options%in%input$desc_options)
    vals$cur_desc_options<-'tab1'
  })


  output$dtab_boxplot<-renderUI({
    column(12,uiOutput(ns("stats_cbox")),
           div(
             uiOutput(ns('boxplot_out'))
           ))
  })
  output$dtab_corr<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          div(class="map_control_style2",
              style="color: #05668D",
              uiOutput(ns('corr_side')))
        ),
        mainPanel(

          uiOutput(ns("corr_plot"))
        )
      )
    )
  })

  output$dtab_mds<-renderUI({
    tabsetPanel(
      header= uiOutput(ns("omds_dist")),
      id=ns('mds_options'),
      tabPanel( "6.1. Plot",
                value="mds_plot",
                div(

                  sidebarLayout(
                    sidebarPanel(
                      div(class="map_control_style2",
                          style="color: #05668D",
                          class="sidebar_float",
                          uiOutput(ns("side_mds")))),
                    mainPanel(
                      uiOutput(ns("plot_mds"))
                    )
                  )
                )

      ),
      tabPanel(
        "6.2. Summary",
        value="mds_summary",
        sidebarLayout(
          sidebarPanel(
            div(class="map_control_style2",style="color: #05668D",
                uiOutput(ns('mds_summary')))),
          mainPanel(
            inline(DT::dataTableOutput(ns("summary_mds")))
          )
        )
      )
    )
  })
  output$omds_dist<-renderUI({
    if(is.null(vals$cur_dist_mds)){vals$cur_dist_mds="Choose one"}

    div(style="margin: 0px; padding: 0px",class="col-sm-12",
        div(class="col-sm-4",style="margin: 0px; padding: 0px;",
            div(class="well2",style="padding-left: 10px; margin-right: 19px ",align="right",
                fluidRow(
                  column(12,
                         column(8,style="margin: 0px; padding: 0px",
                             div(class="map_control_style2",
                                 style="color: #05668D",
                                 pickerInput(ns("mds_distance"),"+ Distance:",choices = c('bray', "euclidean", 'jaccard'), selected=vals$cur_dist_mds)
                             )
                         ),
                         column(4,align="right",style="margin: 0px; padding: 0px",
                             uiOutput(ns("mds_button")))
                  )
                )

            )
        ),


    )
  })





  output$mds_button<-renderUI({
    div(class=vals$mds_btn_class,
        actionButton(ns("run_mds"),"RUN")
    )
  })
  output$mds_summary<-renderUI({
    div(
      tags$div(
        pickerInput(ns("show_mds_results"),"Show Scores:", list("Sites"="sites","Species"="species")),style="width: var(--parentHeight);"
      ),
      actionLink(
        ns('down_mds_results'),span("+ Download",icon("fas fa-table")))
    )


  })
  observeEvent(ignoreInit = T,input$down_mds_results,{
    vals$hand_down<-"mds result"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  output$summary_mds<-DT::renderDataTable({
    req(!is.null(vals$mds))
    saveRDS(vals$mds,"mds.rds")
   # mds<-readRDS('mds.rds')
    #class(mds)
   # vegan::scores(mds,"species")
    vals$mmds_summaryvals$mds_results<-data.frame(vegan::scores(vals$mds,input$show_mds_results))
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = T,class ='cell-border compact stripe')

  output$mds_points_out<-renderUI({
    req(isTRUE(input$mds_points))
    choices=colnames(attr(getdata_descX(),"factors"))
    mds_scale_shape=tiphelp3("+ Scale Shape", "Use different shapes to represent points based on the selected factor")
    div(
      color_input(ns('mds_points_palette') ,tiphelp3("+ Palette","Choose a gradient to represent colors based on the selected factor"),selected='turbo', vals),
      pickerInput(ns('mds_points_factor')  ,"+ Factor:",choices=choices,selected= NULL),
      pickerInput(inputId = ns("mds_points_shape"),
                  label = "+ Shape:",
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),
                  selected=16,
                  options = list(container = "body"),
                  width = "100px"),
      checkboxInput(ns('mds_scale_shape') ,mds_scale_shape,value=F),
      numericInput (ns('mds_points_size') ,"+ Size:",value=2),

    )
  })
  output$mds_text_out<-renderUI({
    req(isTRUE(input$mds_text))
    choices=colnames(attr(getdata_descX(),"factors"))
    div(
      color_input(ns('mds_text_palette') ,"+ Palette:",selected="gray",vals),
      pickerInput(ns('mds_text_factor') ,"+ Factor:",choices=choices,selected=NULL),

      numericInput(ns('mds_text_size') ,"+ Size:",value=4),
    )
  })
  output$side_mds<-renderUI({
    div(
      div(class="div_fundo",
          numericInput(ns('mds_base_size') ,"+ Base size:",value=12),
          pickerInput(ns('mds_theme') ,"+ Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
          textInput(ns('mds_title') ,"+ Title:",value="Multidimensional scaling"),
          numericInput(ns("mds_expandX"),"+ Expand X Axis",value=0.1,step=0.05),
          numericInput(ns("mds_expandY"),"+ Expand Y Axis",value=0.1,step=0.05),
      ),

      div(class="div_fundo",
          checkboxInput(ns('mds_points') ,tiphelp3(strong("+ Scores - Points"),"Display mds scores as points"),value=T),
          uiOutput(ns("mds_points_out"))
      ),
      div(class="div_fundo",
          checkboxInput(ns('mds_text') ,strong("+ Scores - Text"),F),
          uiOutput(ns("mds_text_out")),
      ),

      checkboxInput(ns('mds_show_intercept') ,"+ Intercepts",value=T),
      checkboxInput(ns('mds_stress') ,"+ Display Stress",value=T),
      div(
        actionLink(
          ns('mds_downp'),span("+ Download Plot",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })
  mds_points_factor<-function(){
    req(length(input$mds_points)>0)
    if(isTRUE(input$mds_points)){
      req(input$mds_points_factor)
      data<-getdata_descX()
      factors<-attr(data,"factors")
      factors[rownames(data),input$mds_points_factor, drop=F]
    } else{NULL}

  }
  mds_text_factor<-function(){
    req(length(input$mds_text)>0)
    if(isTRUE(input$mds_text)){
      req(input$mds_text_factor)
      data<-getdata_descX()
      factors<-attr(data,"factors")
      factors[rownames(data),input$mds_text_factor, drop=F]
    } else{ NULL}

  }
  get_mds_ggplot<-reactive({
    text_palette<-input$mds_text_palette
    points_palette<-input$mds_points_palette
    if(is.null(text_palette)){text_palette<-"black"}
    if(is.null(points_palette)){points_palette<-"black"}
    req(length(input$mds_show_intercept)>0)
    model<-vals$mds
    validate(need(class(model)[1]=="metaMDS","You need to run MDS analysis first"))
    args<-list(
      model=model,
      points_factor=mds_points_factor(),
      points_palette=vals$newcolhabs[[points_palette]],
      points_shape=as.numeric(input$mds_points_shape),
      points_size=input$mds_points_size,
      text_factor=mds_text_factor(),
      text_palette=vals$newcolhabs[[text_palette]],
      text_size=input$mds_text_size,
      base_size=input$mds_base_size,
      theme=input$mds_theme,
      title=input$mds_title,
      show_intercept=input$mds_show_intercept,
      points=input$mds_points,
      text=input$mds_text,
      scale_shape=input$mds_scale_shape,
      mds_stress=input$mds_stress,
      expandX=input$mds_expandX,
      expandY=input$mds_expandY
    )
  })
  output$plot_mds<-renderUI({
    validate(need(!anyNA(getdata_descX()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
    args<-get_mds_ggplot()
    renderPlot({
      vals$pmds_plot<-do.call(ggmds,args)
      vals$pmds_plot
    })
  })




  output$dtab_pca<-renderUI({
    validate(need(!anyNA(getdata_descX()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))

    div(
      tabsetPanel(
        header= uiOutput(ns("pca_btn")),
        id=ns('pca_options'),selected=vals$pca_options,
        tabPanel( "7.1. Plot",
          value="pca_plot",
          div(

            sidebarLayout(
              sidebarPanel(
                div(class="map_control_style2",
                    style="color: #05668D",
                    class="sidebar_float",
                    uiOutput(ns("side_pca")))),
              mainPanel(
                uiOutput(ns("plot_pca"))
              )
          )
          )

        ),
        tabPanel(
          "7.2. Summary",
          value="pca_summary",
          sidebarLayout(
            sidebarPanel(
              div(class="map_control_style2",style="color: #05668D",

                  uiOutput(ns('pca_summary')))),
            mainPanel(
              inline(DT::dataTableOutput(ns("summary_pca")))
            )
          )
        )
      )

    )
  })
  output$pca_points_out<-renderUI({
    req(isTRUE(input$pca_points))
    choices=colnames(attr(getdata_descX(),"factors"))
    pca_scale_shape=tiphelp3("+ Scale Shape", "Use different shapes to represent points based on the selected factor")
    div(
      color_input(ns('pca_points_palette') ,tiphelp3("+ Palette","Choose a gradient to represent colors based on the selected factor"),selected='turbo', vals),
      pickerInput(ns('pca_points_factor')  ,"+ Factor:",choices=choices,selected= NULL),
      pickerInput(inputId = ns("pca_points_shape"),
                  label = "+ Shape:",
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),
                  selected=16,
                  options = list(container = "body"),
                  width = "100px"),
      checkboxInput(ns('pca_scale_shape') ,pca_scale_shape,value=F),
      numericInput (ns('pca_points_size') ,"+ Size:",value=2),

    )
  })
  output$pca_text_out<-renderUI({
    req(isTRUE(input$pca_text))
    choices=colnames(attr(getdata_descX(),"factors"))
    div(
      color_input(ns('pca_text_palette') ,"+ Palette:",selected="gray",vals),
      pickerInput(ns('pca_text_factor') ,"+ Factor:",choices=choices,selected=NULL),

      numericInput(ns('pca_text_size') ,"+ Size:",value=4),
    )
  })
  output$pca_biplot_out<-renderUI({
    value=ncol(getdata_descX())
    req(isTRUE(input$pca_biplot))

    tips<-list(
      biplot_n=tiphelp3("+ Number of Variables", "Set the number of variables to display"),
      biplot_arrow_color="+ Arrow Color",
      biplot_color="+ Text Color",
      biplot_type=tiphelp3("+ Sorting Method", "Select the sorting method for variables in the pca biplot. When clockwise_quadrants is chosen, variables are sorted based on quadrants in a clockwise direction. For clockwise_distance, variables are chosen based on their distance from the biplot center in the selected direction."),
      biplot_size="+ Text Size"
    )

    div(
      numericInput(ns('pca_biplot_n') ,tips$biplot_n,value=value, min=1),
      div(colourpicker::colourInput(ns('pca_biplot_color') ,tips$biplot_color,value="blue","background")),
      div(colourpicker::colourInput(ns('pca_biplot_arrow_color') ,tips$biplot_arrow_color,value="blue","background")),
      pickerInput(ns('pca_biplot_type') ,tips$biplot_type,choices=c("clockwise_quadrants","clockwise_distance")),
      numericInput(ns('pca_biplot_size') ,tips$biplot_size,value=4),
      checkboxInput(ns('pca_loading_axis'),"+ Axes",T),
      checkboxInput(ns('pca_lo_axis_color'),"+ Axes color as Arrows",T),
      textInput(ns('pca_lo_x.text'),"+ Loading Axis-X Label","PC1 loadings"),
      textInput(ns('pca_lo_y.text'),"+ Loading Axis-Y Label","PC2 loadings")


    )
  })
  output$side_pca<-renderUI({
    div(
      div(class="div_fundo",
        numericInput(ns('pca_base_size') ,"+ Base size:",value=12),
        pickerInput(ns('pca_theme') ,"+ Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
        textInput(ns('pca_title') ,"+ Title:",value="Principal Component analysis"),
        numericInput(ns("pca_expandX"),"+ Expand X Axis",value=0.1,step=0.05),
        numericInput(ns("pca_expandY"),"+ Expand Y Axis",value=0.1,step=0.05),
      ),

      div(class="div_fundo",
          checkboxInput(ns('pca_points') ,tiphelp3(strong("+ Scores - Points"),"Display PCA scores as points"),value=T),
          uiOutput(ns("pca_points_out"))
      ),
      div(class="div_fundo",
          checkboxInput(ns('pca_text') ,"+ Text",F),
          uiOutput(ns("pca_text_out")),
      ),

      div(class="div_fundo",
          checkboxInput(ns('pca_biplot') ,strong("+ Loadings (Biplot)"),value=T),
          uiOutput(ns("pca_biplot_out")),
      ),

      checkboxInput(ns('pca_show_intercept') ,"+ Intercepts",value=T),
      div(
        actionLink(
          ns('pca_downp'),span("+ Download Plot",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })

  pca_points_factor<-reactive({
    req(length(input$pca_points)>0)
    if(isTRUE(input$pca_points)){
      req(input$pca_points_factor)
      data<-getdata_descX()
      factors<-attr(data,"factors")
      factors[rownames(data),input$pca_points_factor, drop=F]
    } else{NULL}

  })
  pca_text_factor<-reactive({
    req(length(input$pca_text)>0)
    if(isTRUE(input$pca_text)){
      req(input$pca_text_factor)
      data<-getdata_descX()
      factors<-attr(data,"factors")
      factors[rownames(data),input$pca_text_factor, drop=F]
    } else{ NULL}

  })
  pca_model<-reactiveVal()

  args_pca<-reactive({
    text_palette<-input$pca_text_palette
    points_palette<-input$pca_points_palette
    if(is.null(text_palette)){text_palette<-"black"}
    if(is.null(points_palette)){points_palette<-"black"}
    req(length(input$pca_show_intercept)>0)
    model<-pca_model()
    req(length(model)>0)
    args<-list(
      model=model,
      points_factor=pca_points_factor(),
      points_palette=vals$newcolhabs[[points_palette]],
      points_shape=as.numeric(input$pca_points_shape),
      points_size=input$pca_points_size,
      text_factor=pca_text_factor(),
      text_palette=vals$newcolhabs[[text_palette]],
      text_size=input$pca_text_size,
      biplot_n=input$pca_biplot_n,
      biplot_type=input$pca_biplot_type,
      biplot_size=input$pca_biplot_size,
      biplot_color=input$pca_biplot_color,
      biplot_arrow_color=input$pca_biplot_arrow_color,
      base_size=input$pca_base_size,
      theme=input$pca_theme,
      title=input$pca_title,
      show_intercept=input$pca_show_intercept,
      constr=input$pca_constr,
      points=input$pca_points,
      text=input$pca_text,
      biplot=input$pca_biplot,
      loading_axis=input$pca_loading_axis,
      lo_axis_color=input$pca_lo_axis_color,
      lo_x.text=input$pca_lo_x.text,
      lo_y.text=input$ pca_lo_y.text,
      scale_shape=input$pca_scale_shape,
      expandX=input$pca_expandX,
      expandY=input$pca_expandY
    )
  })

  observeEvent(input$run_pca,{
    pca_model(prcomp(getdata_descX(), center=F, scale=F))
    runval$pca<-"btn_nice"
  })
  observeEvent(getdata_descX(),{
    req(!is.null(vals$ppca_plot))
    runval$pca<-"save_changes_nice"
  })

  output$plot_pca<-renderUI({

    args<-args_pca()
    vals$ppca_plot<-do.call(ggpca,args)

    div(

      renderPlot({
        vals$ppca_plot
      })

    )
  })
  is_centered <- function(data) {
    if (all(abs(colMeans(data)) < 1e-5))
      TRUE
    else FALSE

  }
  output$pca_warning<-renderUI({
        centered<-is_centered(getdata_descX())

    if(isTRUE(centered)){NULL} else{p("Warning: The data appears not to be centered. PCA typically requires centered data for accurate results. Adjustments can be made in the Transformations section of the Pre-processing tools.",style="color: grey; padding-left: 10px; font-size: 12px")}

  })



  output$pca_btn<-renderUI({
    div(style="margin: 0px; padding: 0px",class="col-sm-12",
      div(class="col-sm-4",style="margin: 0px; padding: 0px",
        div(class="well2",style="padding-left: 10px;  padding-right: 0px; ",align="right",
            inline(div(
              class=runval$pca,
              actionButton(ns("run_pca"),"RUN", icon=icon("fas fa-sync")))),

        )
      ),

      div(class="col-sm-8",style="margin: 0px; padding: 0px",
          uiOutput(ns("pca_warning")))
    )
  })

  output$stats_panels_rda<-renderUI({
    req(input$rda_X)
    validate(need(!anyNA(vals$saved_data[[input$rda_X]]), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
    tabsetPanel(
      id=ns("rda_view"),selected=vals$rda_view,
      tabPanel("8.1. Plot",
               div(

                 sidebarLayout(
                   sidebarPanel(
                     div(class="map_control_style2",style="color: #3c8dbc;",
                         class="sidebar_float",
                         uiOutput(ns("side_rda")),)),
                   mainPanel(
                     uiOutput(ns("rda_plot"))
                   )
                 )
               )),
      tabPanel("8.2. Summary",
               sidebarLayout(
                 sidebarPanel(
                   div(class="map_control_style",style="color: #3c8dbc;",
                       uiOutput(ns("rda_view_summary")))),
                 mainPanel(
                   verbatimTextOutput(ns("rda_print"))
                 )
               ))

    )
  })





  output$rda_points_out<-renderUI({
    req(isTRUE(input$rda_points))
    choices=colnames(attr(getdata_descX(),"factors"))
    div(
      color_input(ns('rda_points_palette') ,tiphelp3("+ Palette","Choose a gradient to represent colors based on the selected factor"),selected='turbo', vals),
      pickerInput(ns('rda_points_factor')  ,"+ Factor:",choices=choices,selected= NULL),
      pickerInput(inputId = ns("rda_points_shape"),
                  label = "+ Shape:",
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),
                  selected=16,
                  options = list(container = "body"),
                  width = "100px"),
      numericInput (ns('rda_points_size') ,"+ Size:",value=2),

    )
  })
  output$rda_text_out<-renderUI({
    req(isTRUE(input$rda_text))
    choices=colnames(attr(getdata_descX(),"factors"))
    div(
      color_input(ns('rda_text_palette') ,"+ Palette:",selected="gray",vals),
      pickerInput(ns('rda_text_factor') ,"+ Factor:",choices=choices,selected=NULL),

      numericInput(ns('rda_text_size') ,"+ Size:",value=4),
    )
  })
  output$rda_biplot_out<-renderUI({
    value=ncol(vals$saved_data[[input$rda_Y]])
    req(isTRUE(input$rda_biplot))

    tips<-list(
      biplot_n=tiphelp3("+ Number of Variables", "Set the number of variables to display"),
      biplot_arrow_color="+ Arrow Color",
      biplot_color="+ Text Color",
      biplot_type=tiphelp3("+ Sorting Method", "Select the sorting method for variables in the RDA biplot. When clockwise_quadrants is chosen, variables are sorted based on quadrants in a clockwise direction. For clockwise_distance, variables are chosen based on their distance from the biplot center in the selected direction."),
      biplot_size="+ Text size"
    )
    shinyBS:::buildTooltipOrPopoverOptionsList

    div(
      numericInput(ns('rda_biplot_n') ,tips$biplot_n,value=value, min=1),
      div(colourpicker::colourInput(ns('rda_biplot_color') ,tips$biplot_color,value="blue","background")),
      div(colourpicker::colourInput(ns('rda_biplot_arrow_color') ,tips$biplot_arrow_color,value="blue","background")),

      pickerInput(ns('rda_biplot_type') ,tips$biplot_type,choices=c("clockwise_quadrants","clockwise_distance")),
      numericInput(ns('rda_biplot_size') ,tips$biplot_size,value=4)

    )
  })
  output$rda_species_out<-renderUI({
    value=ncol(vals$saved_data[[input$rda_X]])
    req(isTRUE(input$rda_species))
    div(
      div(
        colourpicker::colourInput(ns('rda_species_color') ,"+ Colour:",value="red","background")
      ),
      pickerInput(ns('rda_species_plot') ,"+ Display:",choices=c("points","text")),
      pickerInput(inputId = ns("rda_species_shape"),
                  label = "+ Shape:",
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),
                  selected=df_symbol$val[8],
                  options = list(container = "body"),
                  width = "100px"),
      numericInput(ns('rda_species_n') ,"+ sp number:",value=value),
      pickerInput(ns('rda_species_type') ,"+ sp.n direction:",choices=c("clockwise_quadrants","clockwise_distance")),
      numericInput(ns('rda_species_size') ,"+ Size:",value=2),

    )
  })
  output$side_rda<-renderUI({
    div(
      div(class="div_fundo",
        numericInput(ns('rda_base_size') ,"+ Base size:",value=12),
        pickerInput(ns('rda_theme') ,"+ Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
        textInput(ns('rda_title') ,"+ Title:",value="Redundancy analysis"),
        numericInput(ns("rda_expandX"),"+ Expand X Axis",value=0.1,step=0.05),
        numericInput(ns("rda_expandY"),"+ Expand Y Axis",value=0.1,step=0.05),

      ),
      div(class="div_fundo",
          checkboxInput(ns('rda_points') ,tiphelp3(strong("+ Observation Scores - Points"),"Display Predictor Scores as points"),value=T),
          uiOutput(ns("rda_points_out"))
      ),
      div(class="div_fundo",
          checkboxInput(ns('rda_text') ,tiphelp3(strong("+ Observation Scores - Text"),"Display Predictor Scores as Text"),F),
          uiOutput(ns("rda_text_out")),
      ),

      div(class="div_fundo",
          checkboxInput(ns('rda_biplot') ,strong("+ Predictor Scores (Biplot)"),value=T),
          uiOutput(ns("rda_biplot_out")),
      ),

      div(class="div_fundo",
          checkboxInput(ns('rda_species') ,tiphelp3(strong("+ Response Scores"),"Display Response Scores as points"),value=T),
          uiOutput(ns("rda_species_out"))
      ),
      checkboxInput(ns('rda_show_intercept') ,"+ Intercepts",value=T),
      div(
        actionLink(
          ns('rda_downp'),span("+ Download Plot",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })
  rda_points_factor<-reactive({
    req(length(input$rda_points)>0)
    if(isTRUE(input$rda_points)){
      req(input$rda_points_factor)
      data<-getdata_descX()
      factors<-attr(data,"factors")
      factors[rownames(data),input$rda_points_factor, drop=F]
    } else{NULL}

  })
  rda_text_factor<-reactive({
    req(length(input$rda_text)>0)
    if(isTRUE(input$rda_text)){
      req(input$rda_text_factor)
      data<-getdata_descX()
      factors<-attr(data,"factors")
      factors[rownames(data),input$rda_text_factor, drop=F]
    } else{ NULL}

  })

  get_rda_model<-reactive({
    req(input$rda_X)
    req(input$rda_Y)

    data<-
      vals$saved_data[[input$rda_X]]
    x<-as.matrix(data)
    colnames(x)<-colnames(data)
    if(length(input$rda_Y)>0){
      y<-na.omit(vals$saved_data[[input$rda_Y]][rownames(x),,drop=F])
      colnames(y)<-colnames(vals$saved_data[[input$rda_Y]])
      x<-na.omit(x[rownames(y),])
      dim(data.frame(y))
      dim(x)
      model=vegan::rda(x~.,data=data.frame(y) ,scale=input$rda_scale)
    } else{model= vegan::rda(x,scale=input$rda_scale)}
    model})
  rda_model<-reactiveVal()
  rda_args<-reactive({

    text_palette<-input$rda_text_palette
    points_palette<-input$rda_points_palette
    if(is.null(text_palette)){text_palette<-"black"}
    if(is.null(points_palette)){points_palette<-"black"}
    req(length(input$rda_show_intercept)>0)
    args<-list(
      model=rda_model(),
      points_factor=rda_points_factor(),
      points_palette=vals$newcolhabs[[points_palette]],
      points_shape=as.numeric(input$rda_points_shape),
      points_size=input$rda_points_size,
      text_factor=rda_text_factor(),
      text_palette=vals$newcolhabs[[text_palette]],
      text_size=input$rda_text_size,
      biplot_n=input$rda_biplot_n,
      biplot_type=input$rda_biplot_type,
      biplot_size=input$rda_biplot_size,
      biplot_color=input$rda_biplot_color,
      biplot_arrow_color=input$rda_biplot_arrow_color,
      species_n=input$rda_species_n,
      species_type=input$rda_species_type,
      species_plot=input$rda_species_plot,
      species_size=input$rda_species_size,
      species_shape=as.numeric(input$rda_species_shape),
      species_color=input$rda_species_color,
      base_size=input$rda_base_size,
      theme=input$rda_theme,
      title=input$rda_title,
      show_intercept=input$rda_show_intercept,
      constr=input$rda_constr,
      points=input$rda_points,
      text=input$rda_text,
      biplot=input$rda_biplot,
      species=input$rda_species,
      expandX=input$rda_expandX,
      expandY=input$rda_expandY

    )


  })



  output$rda_btn<-renderUI({
    div(class=runval$rda,actionButton(ns("run_rda"),"RUN", icon=icon("fas fa-sync")))
  })



  observeEvent(input$run_rda,{
    rda_model(get_rda_model())
    runval$rda<-"btn_nice"
  })

  observeEvent(list(input$rda_X,
                    input$rda_Y,
                    input$rda_scale),ignoreInit = T,{

                      runval$rda<-"save_changes_nice"
                    })


  output$rda_plot<-renderUI({
    args<-rda_args()
    req(length(args$model)>0)
    vals$rda_plot<-do.call(ggrda,args)
    renderPlot({

      vals$rda_plot
    })
  })


  output$dtab_rda<-renderUI({


    div(
      div(uiOutput(ns("rda_head"))),
      div(uiOutput(ns("stats_panels_rda"))),

    )


  })
  output$segrda_X<-renderUI({
    pickerInput(ns("segrda_X"),span(strong("Y"), tiphelp("Predictors")), choices=names(vals$saved_data), selected=vals$cur_segrda_X)

  })
  output$segrda_Y<-renderUI({
    pickerInput(ns("segrda_Y"),span(strong("~ X"), tiphelp("Response data")), choices=names(vals$saved_data), selected=vals$cur_segrda_Y)
  })
  output$dtab_segrda<-renderUI({
    column(12,
      div(class='well3',
        p(strong("Segmented Redundancy Analysis")),
        inline(uiOutput(ns('segrda_X'))),
        inline(uiOutput(ns('segrda_Y')))
      ),
      uiOutput(ns('segrda_panels'))
    )
  })
  output$panel_main<-renderUI({
    validate(need(length(vals$saved_data)>0,"No Datalist found"))
    req(input$data_descX)
    column(12,
           tabsetPanel(
             id=ns('desc_options'),selected = vals$cur_desc_options,
             tabPanel('1. Summaries',
                      value="tab1",
                      uiOutput(ns('dtab_summaries'))),
             tabPanel('2. Boxplot',
                      value="tab_box",
                      uiOutput(ns("dtab_boxplot"))),
             tabPanel('3. Ridges',
                      value="tab2",
                      uiOutput(ns('dtab_rid'))),
             # tabPanel('Scatter',value="tab_scatter",uiOutput(ns('dtab_scatter'))),
             tabPanel(
               '4. Pair plot',
               value="tab_ggpair",
               uiOutput(ns('gg_pairs_panel'))
             ),
             #  tabPanel('Histogram',value="tab_histo",uiOutput(ns("dtab_histogram"))),

             tabPanel('5. Correlation plot',
                      value="tab_corr",
                      uiOutput(ns("dtab_corr"))),
             tabPanel('6. MDS',
                      value="tab_mds",
                      uiOutput(ns("dtab_mds"))),
             tabPanel('7. PCA',
                      value="tab_pca",
                      uiOutput(ns("dtab_pca"))),
             tabPanel('8. RDA',
                      value="tab_rda",
                      uiOutput(ns("dtab_rda"))),
             tabPanel('9. segRDA',
                      value="tab_segrda",
                      uiOutput(ns("dtab_segrda")))

           )

    )

  })

  output$segrda_points_out<-renderUI({
    req(isTRUE(input$segrda_points))
    choices=colnames(attr(getdata_descX(),"factors"))
    div(
      color_input(ns('segrda_points_palette') ,tiphelp3("+ Palette","Choose a gradient to represent colors based on the selected factor"),selected='turbo', vals),
      pickerInput(ns('segrda_points_factor')  ,"+ Factor:",choices=choices,selected= NULL),
      pickerInput(inputId = ns("segrda_points_shape"),
                  label = "+ Shape:",
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),
                  selected=16,
                  options = list(container = "body"),
                  width = "100px"),
      numericInput (ns('segrda_points_size') ,"+ Size:",value=2),

    )
  })
  output$segrda_text_out<-renderUI({
    req(isTRUE(input$segrda_text))
    choices=colnames(attr(getdata_descX(),"factors"))
    div(
      color_input(ns('segrda_text_palette') ,"+ Palette:",selected="gray",vals),
      pickerInput(ns('segrda_text_factor') ,"+ Factor:",choices=choices,selected=NULL),

      numericInput(ns('segrda_text_size') ,"+ Size:",value=4),
    )
  })
  output$segrda_biplot_out<-renderUI({
    value=ncol(vals$saved_data[[input$segrda_Y]])
    req(isTRUE(input$segrda_biplot))

    tips<-list(
      biplot_n=tiphelp3("+ Number of Variables", "Set the number of variables to display"),
      biplot_arrow_color="+ Arrow Color",
      biplot_color="+ Text Color",
      biplot_type=tiphelp3("+ Sorting Method", "Select the sorting method for variables in the pwRDA biplot. When clockwise_quadrants is chosen, variables are sorted based on quadrants in a clockwise direction. For clockwise_distance, variables are chosen based on their distance from the biplot center in the selected direction."),
      biplot_size="+ Text size"
    )
    shinyBS:::buildTooltipOrPopoverOptionsList

    div(
      numericInput(ns('segrda_biplot_n') ,tips$biplot_n,value=value, min=1),
      div(colourpicker::colourInput(ns('segrda_biplot_color') ,tips$biplot_color,value="blue","background")),
      div(colourpicker::colourInput(ns('segrda_biplot_arrow_color') ,tips$biplot_arrow_color,value="blue","background")),

      pickerInput(ns('segrda_biplot_type') ,tips$biplot_type,choices=c("clockwise_quadrants","clockwise_distance")),
      numericInput(ns('segrda_biplot_size') ,tips$biplot_size,value=4)

    )
  })
  output$segrda_species_out<-renderUI({
    value=ncol(vals$saved_data[[input$segrda_X]])
    req(isTRUE(input$segrda_species))
    div(
      div(
        colourpicker::colourInput(ns('segrda_species_color') ,"+ Colour:",value="red","background")
      ),
      pickerInput(ns('segrda_species_plot') ,"+ Display:",choices=c("points","text")),
      pickerInput(inputId = ns("segrda_species_shape"),
                  label = "+ Shape:",
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),
                  selected=df_symbol$val[8],
                  options = list(container = "body"),
                  width = "100px"),
      numericInput(ns('segrda_species_n') ,"+ sp number:",value=value),
      pickerInput(ns('segrda_species_type') ,"+ sp.n direction:",choices=c("clockwise_quadrants","clockwise_distance")),
      numericInput(ns('segrda_species_size') ,"+ Size:",value=2),

    )
  })
  output$side_segrda<-renderUI({
    div(
      div(class="div_fundo",
        checkboxInput(ns("segrda_scaling"),span("+ Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T),
        numericInput(ns('segrda_base_size') ,"+ Base size:",value=12),
        pickerInput(ns('segrda_theme') ,"+ Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
        textInput(ns('segrda_title') ,"+ Title:",value="Piecewise Redundancy analysis"),
        numericInput(ns("segrda_expandX")," + X Axis Expansion",value=0.1,step=0.05),
        numericInput(ns("segrda_expandY")," + Y Axis Expansion",value=0.1,step=0.05),
      ),

      div(class="div_fundo",
          checkboxInput(ns('segrda_points') ,tiphelp3(strong("+ Observation Scores - Points"),"Display Predictor Scores as points"),value=T),
          uiOutput(ns("segrda_points_out"))
      ),
      div(class="div_fundo",
          checkboxInput(ns('segrda_text') ,tiphelp3(strong("+ Observation Scores - Text"),"Display Predictor Scores as Text"),F),
          uiOutput(ns("segrda_text_out")),
      ),

      div(class="div_fundo",
          checkboxInput(ns('segrda_biplot') ,strong("+ Predictor Scores (Biplot)"),value=T),
          uiOutput(ns("segrda_biplot_out")),
      ),

      div(class="div_fundo",
          checkboxInput(ns('segrda_species') ,tiphelp3(strong("+ Response Scores"),"Display Response Scores as points"),value=T),
          uiOutput(ns("segrda_species_out"))
      ),
      checkboxInput(ns('segrda_show_intercept') ,"+ Intercepts",value=T),
      div(
        actionLink(
          ns('segrda_downp'),span("+ Download Plot",icon("fas fa-download")), style="button_active"
        )

      )

    )
  })
  segrda_points_factor<-reactive({
    req(length(input$segrda_points)>0)
    if(isTRUE(input$segrda_points)){
      req(input$segrda_points_factor)
      data<-getdata_descX()
      factors<-attr(data,"factors")
      factors[rownames(data),input$segrda_points_factor, drop=F]
    } else{NULL}

  })
  segrda_text_factor<-reactive({
    req(length(input$segrda_text)>0)
    if(isTRUE(input$segrda_text)){
      req(input$segrda_text_factor)
      data<-getdata_descX()
      factors<-attr(data,"factors")
      factors[rownames(data),input$segrda_text_factor, drop=F]
    } else{ NULL}

  })
  get_segrda_ggplot<-reactive({
    req(input$segrda_X)
    req(input$pwRDA_models)
    all_models<-attr(vals$saved_data[[input$segrda_X]],"pwRDA")
   # input$pwRDA_models<-1
     req(length(all_models)>0)
    pwRDA_model<-all_models[[input$pwRDA_models]]
    req(length(pwRDA_model$rda.pw)>0)

    text_palette<-input$segrda_text_palette
    points_palette<-input$segrda_points_palette

    points_factor=segrda_points_factor()
    text_factor=segrda_text_factor()

    text_factor<-text_factor[rownames(scores(pwRDA_model$rda.pw)$sites),,drop=F]
    points_factor<-points_factor[rownames(scores(pwRDA_model$rda.pw)$sites),,drop=F]

    if(is.null(text_palette)){text_palette<-"black"}
    if(is.null(points_palette)){points_palette<-"black"}
    req(length(input$segrda_show_intercept)>0)
    args<-list(
      model=pwRDA_model$rda.pw,
      points_factor=points_factor,
      points_palette=vals$newcolhabs[[points_palette]],
      points_shape=as.numeric(input$segrda_points_shape),
      points_size=input$segrda_points_size,
      text_factor=text_factor,
      text_palette=vals$newcolhabs[[text_palette]],
      text_size=input$segrda_text_size,
      biplot_n=input$segrda_biplot_n,
      biplot_type=input$segrda_biplot_type,
      biplot_size=input$segrda_biplot_size,
      biplot_color=input$segrda_biplot_color,
      biplot_arrow_color=input$segrda_biplot_arrow_color,
      species_n=input$segrda_species_n,
      species_type=input$segrda_species_type,
      species_plot=input$segrda_species_plot,
      species_size=input$segrda_species_size,
      species_shape=as.numeric(input$segrda_species_shape),
      species_color=input$segrda_species_color,
      base_size=input$segrda_base_size,
      theme=input$segrda_theme,
      title=input$segrda_title,
      show_intercept=input$segrda_show_intercept,
      constr=input$segrda_constr,
      points=input$segrda_points,
      text=input$segrda_text,
      biplot=input$segrda_biplot,
      species=input$segrda_species,
      expandX=input$segrda_expandX,
      expandY=input$segrda_expandY

    )


  })
  output$segrda_plot<-renderUI({
    args<-get_segrda_ggplot()
    renderPlot({
      vals$seg_rda_plot<-do.call(ggrda,args)
      vals$seg_rda_plot
    })
  })
  observeEvent(ignoreInit = T,input$segrda_downp,{

    vals$hand_plot<-"segrda"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })





  ####

  observeEvent(getdata_descX(),{
    data=getdata_descX()
    vals$ggpair.variables<-colnames(data)[1:3]
  })

  observeEvent(ignoreInit = T,input$run_pair,{

    vals$ggpair.variables<-input$ggpair.variables

  })
  output$gg_pairs_panel<-renderUI({
    req(input$data_descX)
    data=getdata_descX()
    req(length(data)>0)
    if(is.null(vals$ggpair.variables)){
      vals$ggpair.variables<-colnames(data)[1:3]
    }
    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
          div(
              inline(pickerInput(ns("ggpair.variables"),span("+ Variables:",class='text_alert'),colnames(data), multiple = T,options=list(`actions-box` = TRUE), selected=vals$ggpair.variables)),
              inline(uiOutput(ns("pair_btn")))

              ),

          div(
            class='sidebar_float',
            uiOutput(ns("side_msp")),
            uiOutput(ns("side_msp_pairs"))
          )
        )
      ),
      mainPanel(
        withSpinner(uiOutput(ns("msp_pairs")),8)
      )
    )

  })


  output$side_msp<-renderUI({
    div(class="div_fundo",

      pickerInput(inputId = ns("fm_palette"),
                  label = '+ Palette',
                  choices =     vals$colors_img$val,
                  choicesOpt = list(content =vals$colors_img$img),
                  selected=vals$cm_palette,
                  options=list(container="body")),
      numericInput(ns("msp_plot_width"), "+ Plot width",550),
      numericInput(ns("msp_plot_height"), "+ Plot height",400),
      numericInput(ns("msp_plot_base_size"),"+ Base size",12),
    )

  })



  output$side_msp_pairs <- renderUI({
    req(input$msp_plot_base_size)

    if (is.null(vals$ggpair.box.include)) {
      vals$ggpair.box.include <- FALSE
    }

    div(
      div(
        class='div_fundo',
        div(strong("+ Panels:")),
        uiOutput(ns("output_ggpair_upper")),
        uiOutput(ns("output_ggpair_lower")),
        uiOutput(ns("output_ggpair_diag")),
        div("+", inline(checkboxInput(ns("ggpair.box.include"), "Y boxplot", vals$ggpair.box.include))),
        uiOutput(ns("ggpair.y.variable")),
        uiOutput(ns("output_ggpair_method"))
      ),
      uiOutput(ns("output_ggpair_round")),
      uiOutput(ns("output_ggpair_switch")),
      uiOutput(ns("output_ggpair_varnames_size")),
      uiOutput(ns("output_ggpair_cor_size")),
      uiOutput(ns("output_ggpair_pch")),
      uiOutput(ns("output_ggpair_points_size")),
      uiOutput(ns("output_ggpair_legend_text_size")),
      uiOutput(ns("output_ggpair_legend_title_size")),
      uiOutput(ns("output_ggpair_alpha_curve")),
      uiOutput(ns("output_ggpair_title")),
      uiOutput(ns("output_ggpair_plot_title_size")),
      uiOutput(ns("output_ggpair_xlab")),
      uiOutput(ns("output_ggpair_ylab")),
      uiOutput(ns("output_ggpair_axis_text_size")),
      uiOutput(ns("output_ggpair_axis_title_size")),
      div(inline(uiOutput(ns("output_ggpair_title_corr"))), style = "width: 100%"),
      actionLink(
        ns("fm_downplot4"), span("+ Download plot 1", icon("fas fa-download"), icon("fas fa-image")), style = "button_active"
      )
    )
  })

  output$output_ggpair_upper <- renderUI({
    pickerInput(ns("ggpair.upper"), "+ Upper",
                choices = list(
                  "Correlation" = "corr",
                  "Corr + group" = "corr+group",
                  "none" = "blank"
                ),
                selected = vals$ggpair.upper)
  })
  output$output_ggpair_lower <- renderUI({
    pickerInput(ns("ggpair.lower"), "+ Lower",
                choices = list(
                  "Points" = "points",
                  "Points + group" = "points+group",
                  "none" = "blank"
                ),
                selected = vals$ggpair.lower)
  })
  output$output_ggpair_diag <- renderUI({
    pickerInput(ns("ggpair.diag"), "+ Diagonal",
                choices = list(
                  "Density" = "density",
                  "Density + group" = "density+group",
                  "Hist" = "hist",
                  "Hist+group" = "hist+group",
                  "none" = "blank"
                ),
                selected = vals$ggpair.diag)
  })
  output$output_ggpair_method <- renderUI({
    req(input$ggpair.upper!='blank')
    pickerInput(ns("ggpair.method"), "+ Correlation method:", c("pearson", "kendall", "spearman", "none"))
  })
  output$output_ggpair_round <- renderUI({
    numericInput(ns("ggpair.round"), "+ Digits:", 3)
  })
  output$output_ggpair_switch <- renderUI({
    pickerInput(ns("ggpair.switch"), "+ Switch:", list("default" = NULL, "x" = "x", "y" = "y", "both" = "both"))
  })
  output$output_ggpair_varnames_size <- renderUI({
    req(input$msp_plot_base_size)
    bs<-round(input$msp_plot_base_size/12, 2)
    numericInput(ns("ggpair.varnames.size"), "+ Variable name size:", bs*1.4)
  })
  output$output_ggpair_cor_size <- renderUI({
    req(input$msp_plot_base_size)

    numericInput(ns("ggpair.cor.size"), "+ Corr size:", 2)
  })
  output$output_ggpair_pch <- renderUI({
    pickerInput(inputId = ns("ggpair.pch"),
                label = "+ Point shape",
                choices = df_symbol$val,
                choicesOpt = list(content = df_symbol$img),
                options = list(container = "body"),
                width = "100px",
                selected = vals$xyf_symbol)
  })
  output$output_ggpair_points_size <- renderUI({
    req(input$msp_plot_base_size)
    bs<-round(input$msp_plot_base_size/12, 2)
    numericInput(ns("ggpair.points.size"), "+ Points size", bs)
  })
  output$output_ggpair_legend_text_size <- renderUI({
    req(input$msp_plot_base_size)
    bs<-round(input$msp_plot_base_size/12, 2)
    numericInput(ns("ggpair.legend.text.size"), "+ legend.text.size:", bs)
  })
  output$output_ggpair_legend_title_size <- renderUI({
    req(input$msp_plot_base_size)
    bs<-round(input$msp_plot_base_size/12, 2)
    numericInput(ns("ggpair.legend.title.size"), "+ legend.title.size:", bs)
  })
  output$output_ggpair_alpha_curve <- renderUI({
    numericInput(ns("ggpair.alpha.curve"), "+ Curve transparency:", 0.8)
  })
  output$output_ggpair_title <- renderUI({
    textInput(ns("ggpair.title"), "+ Title:", "")
  })
  output$output_ggpair_plot_title_size <- renderUI({
    req(input$msp_plot_base_size)
    bs<-round(input$msp_plot_base_size/12, 2)
    numericInput(ns("ggpair.plot.title.size"), "+ Title size:", bs)
  })
  output$output_ggpair_xlab <- renderUI({
    textInput(ns("ggpair.xlab"), "+ xlab:", "")
  })
  output$output_ggpair_ylab <- renderUI({
    textInput(ns("ggpair.ylab"), "+ ylab:", "")
  })
  output$output_ggpair_axis_text_size <- renderUI({
    req(input$msp_plot_base_size)
    bs<-round(input$msp_plot_base_size/12, 2)
    numericInput(ns("ggpair.axis.text.size"), "+ Axis tick size:", bs)
  })
  output$output_ggpair_axis_title_size <- renderUI({
    bs<-round(input$msp_plot_base_size/12, 2)
    numericInput(ns("ggpair.axis.title.size"), "+ Axis label size:", bs)
  })
  output$output_ggpair_title_corr <- renderUI({
    div(inline(textInput(ns("ggpair.title_corr"), "+ Title corr:", "")), style = "width: 100%")
  })


  output$ggpair.y.variable<-renderUI({
    req(isTRUE(yclude_y()))
    req(input$data_descX)

    data0<-getdata_descX()
    req(length(data0)>0)

    data<-vals$saved_data[[input$data_descX]]
    req(length(data)>0)

    factors<-attr(data,"factors")
    div(
      style="border-bottom: 1px solid; margin-bottom: 5px; margin-left: 20px",
      pickerInput(ns("ggpair.y.variable"),strong("+ Grouping Factor:", class='text_alert'),colnames(factors), selected=vals$ggpair.y.variable, width="220px")
    )
  })
  observeEvent(input$ggpair.y.variable,{
    vals$ggpair.y.variable<-input$ggpair.y.variable
  })
  output$ggpair.title_corr<-renderUI({
    req(input$ggpair.method)
    textInput(ns("ggpair.title_corr"),"+ title_corr:",paste(input$ggpair.method,"corr"))
  })
  observeEvent(ignoreInit = T,input$fm_downplot4,{
    vals$hand_plot<-"Pairs-plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })


  output$pair_btn<-renderUI({
    div(class=runval$pair,actionButton(ns("run_pair"),"RUN", icon=icon("fas fa-sync")))

  })

  observeEvent(get_ggpair(),{
    req(input$run_pair)
    runval$pair<-"save_changes_nice"
  })


  observeEvent(ignoreInit = T,input$run_pair,{
    vals$desc_pairplot<-get_ggpair()
    runval$pair<-"btn_nice"
  })




  yclude_y<-reactive({
    req(input$ggpair.lower)
    req(input$ggpair.upper)
    req(input$ggpair.diag)
    req(length(input$ggpair.box.include)>0)
    input$ggpair.lower=='points+group'|input$ggpair.upper=='corr+group'|input$ggpair.diag%in%c("density+group","hist+group")| isTRUE( input$ggpair.box.include)
  })




  get_ggpair_args<-reactive({
    args<-try(silent = T,{
      req(input$data_descX)

      req(input$ggpair.variables)
      req(input$fm_palette)
      newdata<-getdata_descX()
      req(length(newdata)>0)
      req(input$ggpair.variables%in%colnames(newdata))
      data<-newdata[,input$ggpair.variables]
      pred<-y<-NULL
      df=data
      cols<-vals$newcolhabs[[input$fm_palette]](1)
      my_cols<-cols



      if(yclude_y()){
        req(input$data_descX)
        req(input$ggpair.y.variable)
        req(input$data_descX %in% names(vals$saved_data))
        factors<-attr(vals$saved_data[[input$data_descX]],"factors")
        req(input$ggpair.y.variable %in% colnames(factors))
        y<-pred<-factors[rownames(data),input$ggpair.y.variable, drop=F]
        #validate(need(nlevels(pred[,1])<=50))
        df=cbind(data,pred)
        cols<-vals$newcolhabs[[input$fm_palette]](nlevels(pred[,1]))
        my_cols<-cols[pred[,1]]
      }
      include.y<-input$ggpair.box.include



      # library(GGally)

      size=input$msp_plot_base_size*.09

      req(input$ggpair.method)

      args<-list(x=data,y=y,
                 cols=cols,
                 method=input$ggpair.method,
                 round=input$ggpair.round,
                 switch=input$ggpair.switch,
                 plot.title.size=input$ggpair.plot.title.size,
                 axis.text.size=input$ggpair.axis.text.size,
                 axis.title.size=input$ggpair.axis.title.size,
                 cor.size=input$ggpair.cor.size,
                 varnames.size=input$ggpair.varnames.size,
                 points.size=input$ggpair.points.size,
                 legend.text.size=input$ggpair.legend.text.size,
                 legend.title.size=input$ggpair.legend.title.size,
                 alpha.curve=input$ggpair.alpha.curve,

                 title=input$ggpair.title,
                 xlab=input$ggpair.xlab,
                 ylab=input$ggpair.ylab,

                 title_corr=input$ggpair.title_corr,
                 include.y=include.y,
                 pch=as.numeric(input$ggpair.pch),
                 upper=input$ggpair.upper,
                 lower=input$ggpair.lower,
                 diag=input$ggpair.diag


      )
      #  saveRDS(args,"args.rds")

      req(   !any(sapply(args[-2],length)<1))
      # attach(args)
      class(args)<-'iggpair'


      args

    })
    req(class(args)=="iggpair")
    args
  })
  get_ggpair<-reactive({
    args<-get_ggpair_args()
    class(args)=="iggpair"
    p<-do.call(gg_pairplot2,args)

    p
  })

  output$msp_pairs<-renderUI({

    res<-div(

      renderPlot(vals$desc_pairplot,  width=input$msp_plot_width,height=input$msp_plot_height),
      em(attr(vals$desc_pairplot,"row1"), style="color: gray")
    )
    vals$show_ggrun<-F
    res
  })


  ###
  output$corr_cutoff<-renderUI({
    req(input$cutoff_hl!="all")
    if(is.null(vals$cor_cutoff)){vals$cor_cutoff<-0.9}
   div(
     inline(numericInput(ns("cor_cutoff"),"+ Cutoff",value = vals$cor_cutoff,min = 0.1,max = 1,step = .1)),
     inline(div(em(attr(get_corrdata(),"war"), tyle="color: gray"))),


   )
    })

  observeEvent(ignoreInit = T,input$cor_cutoff,{
    vals$cor_cutoff<-input$cor_cutoff
  })


  output$cor_method_out<-renderUI({
    cor_method = c("pearson", "kendall", "spearman")
    div(
      inline(
        div(
          pickerInput(ns("cor_method"),
                      choices = cor_method,
                      tiphelp3("+ Correlation",
                               "Select the correlation coefficient to be computed."))
        )
      ),
      inline(div(align="right",
                 uiOutput(ns("corr_btn"))
      ))
    )
  })

  output$corr_side<-renderUI({

    cor_use=c( "complete.obs","everything", "all.obs", "na.or.complete", "pairwise.complete.obs")
    cor_dendogram = c("both","row","column","none")
    cor_scale = c("none","row", "column")
    cor_Rowv = c('TRUE','FALSE')
    cor_Colv=c('Rowv',T,F)
    cor_revC=c('TRUE','FALSE')
    cor_na.rm=c('TRUE','FALSE')
    cor_labRow=c('TRUE','FALSE')
    cor_labCol=c('TRUE','FALSE')
    cor_cellnote=c('TRUE','FALSE')
    cor_density.info=c("histogram","density","none")
    div(uiOutput(ns('cor_method_out')),
      div(
        inline(pickerInput(ns('cutoff_hl'),
                           tiphelp3("+ Filter correlations:",
                                    "Choose the pair-wise absolute correlation cutoff."),
                           choices = list("All" = "all", "Lower than" = "lower", "Higher than" = "higher"))),
        inline(uiOutput(ns('corr_cutoff')))
      ),

      pickerInput(ns("cor_use"),
                  tiphelp3("+ Use",
                           "Select the method for computing covariances in the presence of missing values."),
                  choices = cor_use),
      pickerInput(ns("cor_dendogram"),
                  tiphelp3("+ Dendogram",
                           "Choose whether to draw none, row, column, or both dendrograms."),
                  choices = cor_dendogram),
      pickerInput(ns("cor_scale"),
                  tiphelp3("+ Scale",
                           "Indicate if the values should be centered and scaled in the row direction, column direction, or none."),
                  choices = cor_scale),
      pickerInput(ns("cor_Rowv"),
                  tiphelp3("+ Rowv",
                           "If TRUE, the dendrogram is computed and reordered based on row means."),
                  choices = cor_Rowv),
      pickerInput(ns("cor_Colv"),
                  tiphelp3("+ Colv",
                           "Colv='Rowv' means that columns should be treated identically to the rows. If TRUE, the dendrogram is computed and reordered based on column means."),
                  choices = cor_Colv),
      pickerInput(ns("cor_revC"),
                  tiphelp3("+ revC",
                           "Indicate if the column order should be reversed for plotting."),
                  choices = cor_revC),
      pickerInput(ns("cor_na.rm"),
                  tiphelp3("+ na.rm",
                           "Indicate if NAs should be removed."),
                  choices = cor_na.rm),
      pickerInput(ns("cor_labRow"),
                  tiphelp3("+ labRow",
                           "Choose whether to show observation labels."),
                  choices = cor_labRow),
      pickerInput(ns("cor_labCol"),
                  tiphelp3("+ labCol",
                           "Choose whether to show variable labels."),
                  choices = cor_labCol),
      pickerInput(ns("cor_density.info"),
                  tiphelp3("+ density.info",
                           "Indicate whether to superimpose a histogram, a density plot, or no plot on the color-key."),
                  choices = cor_density.info),
      pickerInput(inputId = ns("cor_palette"),
                  label = "+ Palette:",
                  choices = vals$colors_img$val,
                  choicesOpt = list(content = vals$colors_img$img), selected = vals$cor_palette),
      numericInput(ns("cor_mar_row"),
                   "+ X margin",
                   value = 5, min = 0, step = 1),
      numericInput(ns("cor_mar_col"),
                   "+ Y margin",
                   value = 5, min = 0, step = 1),
      numericInput(ns("cor_sepwidth_a"),
                   tiphelp3("+ sep row width:",
                            "Set the space between rows."),
                   value = 0.05, min = 0.1, max = 1, step = .01),
      numericInput(ns("cor_sepwidth_b"),
                   tiphelp3("+ sep col width:",
                            "Set the space between columns."),
                   value = 0.05, min = 0.1, max = 1, step = .01),
      colourpicker::colourInput(ns("cor_sepcolor"),
                                label = tiphelp3("+ Sep color:",
                                                 "Choose the color between rows and columns."),
                                value = "black", showColour = "background"),
      colourpicker::colourInput(ns("cor_na.color"),
                                label = tiphelp3("+ NA color:",
                                                 "Choose the color to use for missing value."),
                                value = "gray", showColour = "background"),
      pickerInput(ns("cor_cellnote"),
                  tiphelp3("+ Cell note",
                           "Choose whether to show correlation values as cell notes."),
                  choices = cor_cellnote),
      colourpicker::colourInput(ns("cor_noteco"),
                                label = tiphelp3("+ Note color:",
                                                 "Choose the color of the correlation value."),
                                value = "black", showColour = "background"),
      numericInput(ns("cor_notecex"),
                   tiphelp3("+ Note size:",
                            "Set the size of the correlation value."),
                   value = 1, step = 0.1),
      div(
        actionLink(ns('corr_downp'), "+ Download plot", style = "button_active")
      ),
      div(
        actionLink(ns('corr_down_results'), span("+ Download Results", icon("fas fa-table")), style = "button_active")
      )
    )

  })

  observeEvent(ignoreInit = T,input$corr_down_results,{
    vals$hand_down<-"Corr result"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  get_corrdata<-reactive({
    req(input$cor_method)
    req(input$cor_use)
    req(input$cutoff_hl)
    try({
      args<-list(data=getdata_descX(),cor_method=input$cor_method,cor_cutoff=input$cor_cutoff,cor_use=input$cor_use,ret=input$cutoff_hl)
      # saveRDS(args,"args.rds")

      #args<-readRDS("args.rds")
      # attach(args)
      cordata<-do.call(cordata_filter,args)
      cordata
    })

  })
  output$corr_btn<-renderUI({
    div(class=runval$corr,actionButton(ns("run_cor"),"RUN", icon=icon("fas fa-sync")))
  })



  observeEvent(args_corrplot(),{
    graphics.off()
    req(input$run_cor)
    runval$corr<-"save_changes_nice"
  })

  observeEvent(input$run_cor,{
    args<-args_corrplot()
    try({
      do.call(i_corplot,args)
      vals$plot_correlation<-recordPlot()
    })
    runval$corr<-"btn_nice"
  })
  args_corrplot<-reactive({
    cordata=get_corrdata()



    args<-list(cordata=cordata,
               newcolhabs=vals$newcolhabs,
               cor_palette=input$cor_palette,
               cor_sepwidth_a=input$cor_sepwidth_a,
               cor_sepwidth_b=input$cor_sepwidth_b,
               cor_notecex=input$cor_notecex,
               cor_noteco=input$cor_noteco,
               cor_na.color=input$cor_na.color,
               cor_sepcolor=input$cor_sepcolor,
               cor_dendogram=input$cor_dendogram,
               cor_scale=input$cor_scale,
               cor_Rowv=input$cor_Rowv,
               cor_Colv=input$cor_Colv,
               cor_revC=input$cor_revC,
               cor_na.rm=input$cor_na.rm,
               cor_labRow=input$cor_labRow,
               cor_labCol=input$cor_labCol,
               cor_cellnote=input$cor_cellnote,
               cor_density.info=input$cor_density.info,
               margins = c(input$cor_mar_row, input$cor_mar_col))
    req(!any(unlist(lapply(args,is.null))))

    args
  })

  output$corr_plot<-renderUI({
    div(

      renderPlot({
        vals$plot_correlation

      })
    )
  })


  observeEvent(ignoreInit = T,input$corr_downp,{
    vals$hand_plot<-"Correlation Plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)

  })


  getmissing<-reactive({
    vals<-readRDS("savepoint.rds")
    data<-vals$saved_data$zeu
    req(is.data.frame(vals$saved_data[[input$data_descX]]))
    data=vals$saved_data[[input$data_descX]]

    image(as.matrix(data))

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
      pic<-colnames(vals$saved_data[[input$data_descX]])[which(colnames(vals$saved_data[[input$data_descX]])%in%res[,1])]
      res[,1]<-NULL
      if(length(pic)>0)
        res[pic,, drop=F]
    }



  })
  get_dataord<-reactive({
    req(input$missing_reorder!="N missing")
    data=vals$saved_data[[input$data_descX]]
    dataord<-if(input$missing_reorder=="Factor"){
      attr(data,"factors")    } else{data}
    dataord
  })



  observeEvent(ignoreInit = T,input$missing_id1,{
    vals$missing_id1<-input$missing_id1
  })
  observeEvent(ignoreInit = T,input$missing_id2,{
    vals$missing_id2<-input$missing_id2
  })
  observeEvent(ignoreInit = T,input$missing_var1,{
    vals$missing_var1<-input$missing_var1
  })
  observeEvent(ignoreInit = T,input$missing_var2,{
    vals$missing_var2<-input$missing_var2
  })
  observeEvent(ignoreInit = T,input$missing_reorder,{
    vals$missing_reorder<-input$missing_reorder
  })

  observeEvent(ignoreInit = T,input$missing_ord,{
    vals$missing_ord<-input$missing_ord
  })


  output$missing_data<-renderUI({
    sidebarLayout(
      sidebarPanel(uiOutput(ns('missing_side'))),
      mainPanel(uiOutput(ns('missing_plot')))
    )
  })

  output$missing_side<-renderUI({
    data=vals$saved_data[[input$data_descX]]
    if(is.null(vals$missing_id1)){
      ob1<-rownames(data)[c(1,nrow(data))]
      va1<-colnames(data)[c(1,ncol(data))]
      vals$missing_id1<-ob1[1]
      vals$missing_id2<-ob1[2]
      vals$missing_var1<-va1[1]
      vals$missing_var2<-va1[2]
    }



    div(class="map_control_style",style="color: #05668D",
        div("Row:",
            inline(pickerInput(ns("missing_id1"), NULL,rownames(data), selected=vals$missing_id1, width="100px")), strong("to"),inline(
              pickerInput(ns("missing_id2"), NULL,rownames(data), selected=vals$missing_id2, width="100px")
            )
        ),
        div("Col:",
            inline(pickerInput(ns("missing_var1"), NULL,colnames(data), selected=vals$missing_var1, width="100px")), strong("to"),inline(
              pickerInput(ns("missing_var2"), NULL,colnames(data), selected=vals$missing_var2, width="100px")
            )
        ),
        div("Reorder",
            inline(pickerInput(ns("missing_reorder"), NULL,c(
              "N missing","Variable","Factor"
            ), selected=vals$missing_reorder, width="100px")),
            inline(uiOutput(ns("missing_ord")))
        ),
        div("+ Palette",
            pickerInput(inputId=ns("missing_palette"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px')
        ),
        div(
          actionLink(ns('split_missing'),"+ Split into missing and non-missing", style="button_active")
        ),
        div(
          actionLink(ns('missing_downp'),"+ Download plot", style="button_active")
        ),

        actionButton(ns("save_teste"),"SAVE")
    )

  })

  observeEvent(ignoreInit = T,input$split_missing,{

    data=vals$saved_data[[input$data_descX]]
    req(any(is.na(data)))
    factors<-attr(data,"factors")
    coords<-attr(data,"coords")
    colmiss<-getcol_missing(data)[,1]
    comi<-which(colnames(data)%in%as.character(colmiss))
    romi<-which(rownames(data)%in%as.character(getrow_missing(data)[,1]))

    ylist<-list()
    for(i in 1:length(comi)){
      romipic<-which(is.na(data[,comi[i]]))
      X=data[-romipic,-comi, drop=F]
      attr(X,"factors")<-factors[rownames(X),]
      attr(X,"coords")<-coords[rownames(X),]

      Y=data[-romipic,comi[i], drop=F]
      fac<-factors[rownames(Y),]
      n_sample<-round(nrow(Y)*20/100)
      part<-sample(1:nrow(Y),n_sample)
      name0<-paste0("Partition_",colnames(Y))
      name1<-make.unique(c(colnames(factors),name0), sep="_")
      name_part<-name1[ncol(factors)+1]
      fac[name_part]<-NA
      fac[rownames(Y)[as.vector(part)] ,name_part]<-"test"
      fac[rownames(Y)[-as.vector(part)] ,name_part]<-"training"
      fac[name_part]<-factor(fac[,name_part])
      attr(Y,"factors")<-fac
      attr(Y,"coords")<-coords[rownames(Y),]
      ylist[[i]]<-Y

      newdata=data[romipic,-comi, drop=F]
      attr(newdata,"factors")<-factors[rownames(newdata),]
      attr(newdata,"coords")<-coords[rownames(newdata),]


      name0<-paste0(input$data_descX,"_COMP_X_to_", colnames(Y))
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      namemissing<-name1[length(vals$saved_data)+1]
      vals$saved_data[[namemissing]]<-X

      #name0<-paste0(input$data_descX,"_COMP_Y_", colnames(Y))
      # name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      #namemissing<-name1[length(vals$saved_data)+1]
      #vals$saved_data[[namemissing]]<-Y

      name0<-paste0(input$data_descX,"_MISS_newX_to_",colnames(Y))
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      namemissing<-name1[length(vals$saved_data)+1]
      vals$saved_data[[namemissing]]<-newdata
    }
    datY<-mergedatacol(ylist)
    name0<-paste0(input$data_descX,"_COMP_Y_")
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    namemissing<-name1[length(vals$saved_data)+1]
    vals$saved_data[[namemissing]]<-datY
  })




  observeEvent(ignoreInit = T,input$missing_downp,{

    vals$hand_plot<-"Missing plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)

  })

  output$missing_ord<-renderUI({
    choices<-colnames(get_dataord())
    pickerInput(ns("missing_ord"), NULL,choices, selected=vals$missing_ord, width="100px")
  })




  output$missing_plot<-renderUI({
    req(input$missing_reorder)
    data=vals$saved_data[[input$data_descX]]


    ob1<-c(input$missing_id1,input$missing_id2)
    obs<-which(rownames(data)%in%ob1)
    va1<-c(input$missing_var1,input$missing_var2)
    var<-which(colnames(data)%in%va1)
    pic_var<-seq(var[1],var[2])
    pic_obs<-seq(obs[1],obs[2])
    data<-data[pic_obs,pic_var]
    renderPlot({
      df<-data.frame(data)
      df$nmissing<-apply(data,1,function(x) sum(is.na(x)))

      if(input$missing_reorder=='N missing'){
        a<-reshape2::melt(data.frame(id=rownames(df),df), c("id","nmissing"))
        p<-ggplot(a,aes(reorder(variable,nmissing),reorder(id,nmissing)))+  geom_tile(aes(fill=value), color="black")+scale_fill_gradientn(colours=  vals$newcolhabs[[input$missing_palette]](100),na.value="black")
      } else {
        df<-data.frame(data)
        df$nmissing<-apply(data,1,function(x) sum(is.na(x)))

        req(input$missing_reorder)
        dataord<-get_dataord()
        ordvar<-dataord[,input$missing_ord]
        df$ordvar<-ordvar
        a<-reshape2::melt(data.frame(id=rownames(df),df), c("id","nmissing","ordvar"))

        p<-ggplot(a,aes(reorder(variable,ordvar),reorder(id,ordvar)))+  geom_tile(aes(fill=value), color="black")+scale_fill_gradientn(colours=  vals$newcolhabs[[input$missing_palette]](100),na.value="black")
      }
      p<-p+theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Variables")+ylab("Observations")

      vals$missing_plot<-p
      vals$missing_plot
    })
  })



  observeEvent(ignoreInit = T,input$save_teste,{
    saveRDS(reactiveValuesToList(vals),"savepoint.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()
    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')

  })



  output$cor_downplot<-renderUI({
    req(!is.null(vals$corplot))
    div(
      actionLink(ns('cordown'),"+ Download plot", style="button_active")
    )
  })

  observeEvent(ignoreInit = T,input$data_descX,{
    vals$corplot<-NULL
  })


  observeEvent(ignoreInit = T,input$cordown,{

    vals$hand_plot<-"Corr plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)

  })


  pic_pca_results<-reactive({
    req(input$show_pca_results)
    switch (input$show_pca_results,
            'Standard deviations' = 'sdev',
            'Rotation'='rotation',
            'Centering'='center',
            'Scaling'='scale',
            'Scores'='x',
            'Importance'='importance',
    )
  })

  output$pca_summary<-renderUI({
    req(input$pca_options=="pca_summary")
    div(
      tags$div(
        pickerInput(ns("show_pca_results"),"Show result:", c('Importance','Scores',"Standard deviations","Rotation","Centering","Scaling")),style="width: var(--parentHeight);"
      ),
      actionLink(
        ns('down_pca_results'),span("+ Download",icon("fas fa-table")))
    )


  })

  observeEvent(ignoreInit = T,input$down_pca_results,{
    vals$hand_down<-"PCA result"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })

  observeEvent(ignoreInit = T,input$pca_options,{
    vals$pca_options<-input$pca_options
  })
  observeEvent(ignoreInit = T,input$scatter_y_datalist,{
    vals$scatter_y_datalist<-input$scatter_y_datalist
  })
  output$scatter_y_datalist<-renderUI({
    pickerInput(ns("scatter_y_datalist"),'Datalist Y', choices=names(vals$saved_data), selected=vals$scatter_y_datalist)
  })
  observeEvent(ignoreInit = T,input$scatter_x_datalist,{
    vals$scatter_x_datalist<-input$scatter_x_datalist
  })
  output$scatter_x_datalist<-renderUI({
    pickerInput(ns("scatter_x_datalist"),'Datalist X', choices=names(vals$saved_data), selected=vals$scatter_x_datalist)
  })
  output$dtab_scatter<-renderUI({
    div(style="background: white",
        p(strong("Scatter plot")),

        sidebarLayout(
          sidebarPanel(
            div( class="map_control_style",
                 style="color: #05668D",

                 div(inline(uiOutput(ns("scatter_x_datalist"))),
                     inline(uiOutput(ns("scatter_x")))),

                 div(
                   inline(uiOutput(ns("scatter_y_datalist"))),
                   inline(uiOutput(ns("scatter_y_input")))
                 ),
                 uiOutput(ns('scatter_side'))
            )
          ),
          mainPanel(uiOutput(ns("scatter_plot"))))
    )
  })

  output$scatter_side<-renderUI({
    req(input$scatter_x)
    req(input$scatter_y)
    div(


      div(span("+ Shape:",
               inline(pickerInput(inputId=ns("scatter_symbol"),
                                  label = NULL,
                                  choices = df_symbol$val,
                                  options=list(container="body"),
                                  choicesOpt = list(content = df_symbol$img), width='75px')))),
      div(
        span("+ Size:",
             inline(numericInput(ns("scatter_cexpoint"),NULL,value = 1,min = 0.1,max = 3,step = .1, width='75px')
             ))
      ),
      div(span('+ X label:',
               inline(
                 textInput(ns("scatter_xlab"),NULL , value=input$scatter_x, width="120px")
               )
      )),
      div(span('+ Y label:',
               inline(
                 textInput(ns("scatter_ylab"),NULL , value=input$scatter_y, width="120px"))
      )),
      div(
        actionLink(ns('scatter_downplot'),"+ Download plot", style="button_active")
      )

    )
  })


  observeEvent(ignoreInit = T,input$scatter_downplot,{

    vals$hand_plot<-"Scatter plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)

  })
  #vals<-readRDS("savepoint.rds")

  # vals$saved_data$ID_tempo_GF2$Ano==
  #vals$saved_data$ID_Variáveis_GF2$Ano

  output$scatter_plot<-renderUI({
    datax<-vals$saved_data[[input$scatter_x_datalist]]
    datay<-vals$saved_data[[input$scatter_y_datalist]]
    dataxy<-data.frame(datax[input$scatter_x],datay[input$scatter_y])

    renderPlot({
      plot(dataxy, pch=as.numeric(input$scatter_symbol), cex=input$scatter_cexpoint, xlab=input$scatter_xlab, ylab=input$scatter_ylab)
      vals$scatter_plot<-recordPlot()
    })
  })

  output$scatter_y_input<-renderUI({
    req(input$scatter_y_datalist)
    data<-vals$saved_data[[input$scatter_y_datalist]]
    div(

      pickerInput( ns("scatter_y"),'Y',choices =colnames(data),selected= vals$scatter_y, width="200px"
      ))


  })

  output$scatter_x<-renderUI({

    req(input$scatter_x_datalist)
    data<-vals$saved_data[[input$scatter_x_datalist]]
    div(

      pickerInput( ns("scatter_x"),"X",choices = colnames(data),selected= vals$scatter_x, width="200px"
      ))



  })

  observeEvent(ignoreInit = T,input$scatter_x,{
    vals$scatter_x<-input$scatter_x
  })
  observeEvent(ignoreInit = T,input$scatter_y,{
    vals$scatter_y<-input$scatter_y
  })

  output$bug<-renderUI({
    renderPrint({
      input$box_linecol
    })
  })

  observeEvent(input$box_reset,{
    shinyjs::reset("side_boxplot")
  })


  output$side_boxplot<-renderUI({

    div(class="map_control_style2",style="color: #05668D",

        div(class="sidebar_float",
        div(class="div_fundo",
            uiOutput(ns('sidebox_horiz')),
          uiOutput(ns('sidebox_theme')),

          uiOutput(ns('sidebox_palette')),
          uiOutput(ns('sidebox_alpha')),
          uiOutput(ns('sidebox_linecol')),
          uiOutput(ns('sidebox_linewidth')),

          uiOutput(ns('sidebox_base_size')),
          uiOutput(ns('sidebox_cex.main')),
          uiOutput(ns('sidebox_cex.label_panel')),

          uiOutput(ns('sidebox_cex.lab')),
          uiOutput(ns('sidebox_cex.axes')),
          uiOutput(ns('sidebox_title')),
          uiOutput(ns('sidebox_xlab')),
          uiOutput(ns('sidebox_xlab_rotate')),

          uiOutput(ns('sidebox_ylab')),
          uiOutput(ns('sidebox_ylab_rotate')),

          uiOutput(ns('sidebox_grid')),
          uiOutput(ns('sidebox_varwidth')),
          uiOutput(ns('sidebox_violin')),

          uiOutput(ns('sidebox_width')),
          uiOutput(ns('sidebox_height'))

        )

            ))
  })

  output$sidebox_width<-renderUI({
    numericInput(ns('box_width'),"+ Plot widht:",550, step=50)


  })

  output$sidebox_height<-renderUI({
    numericInput(ns('box_heigth'),"+ Plot heigth:",400, step=50)

  })



  output$sidebox_xlab_rotate<-renderUI({
    numericInput(ns('box_xlab_rotate'),"+ x text angle:", 0,step=5)
  })
  output$sidebox_ylab_rotate<-renderUI({
    numericInput(ns('box_ylab_rotate'),"+ y text angle:", 0,step=5)
  })

  output$sidebox_theme<-renderUI({
    pickerInput(ns("box_theme"),"+ Theme:",c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'))


  })


  output$sidebox_horiz<-renderUI({
    checkboxInput(ns('box_horiz'),"+ Horizontal:",value=F)
  })
  output$sidebox_palette<-renderUI({
    pickerInput(ns("box_palette"),
                label = "+ Palette:",
                choices = vals$colors_img$val,
                choicesOpt = list(content = vals$colors_img$img),
                options=list(container="body"))
  })
  output$sidebox_alpha<-renderUI({
    numericInput(ns('box_alpha'),"+ Lighten:", .3, step=0.05)
  })
  output$sidebox_linecol<-renderUI({


    div(
      colourpicker::colourInput(ns("box_linecol"),
                                label = "+ Line color:",
                                value ="black",showColour="background")
    )

  })




  output$sidebox_linewidth<-renderUI({
    numericInput(ns('box_linewidth'),"+ Line width:", .5,step=.1)
  })

  output$sidebox_base_size<-renderUI({
    numericInput(ns('box_base_size'),"+ Base size:", 12,step=1)
  })
  output$sidebox_cex.axes<-renderUI({
    numericInput(ns('box_cex.axes'),"+ Axis size:", 1.5,step=.1)
  })
  output$sidebox_cex.lab<-renderUI({
    numericInput(ns('box_cex.lab'),"+ Label size:", 1.5,step=.1)
  })
  output$sidebox_cex.main<-renderUI({
    numericInput(ns('box_cex.main'),"+ Title size:", 1.5,step=.1)
  })

  output$sidebox_cex.label_panel<-renderUI({
    req(input$box_y)
    req(length(input$box_y)>1)
    numericInput(ns('box_cex.label_panel'),"+ Panel Title Size:", 1.5,step=.1)
  })



  output$sidebox_title<-renderUI({
    req(input$boxplot_X)
    req(input$box_y)
    value=ifelse(length(input$box_y)>1,"",paste(input$box_y,"~",input$boxplot_X))
    textInput(ns('box_title'),"+ Title:",value)
  })
  output$sidebox_xlab<-renderUI({
    req(input$boxplot_X)
    req(input$box_y)
    value=ifelse(length(input$box_y)>1,"",input$boxplot_X)
    textInput(ns('box_xlab'),"+ x label:",value)
  })
  output$sidebox_ylab<-renderUI({
    req(input$box_y)
    value=ifelse(length(input$box_y)>1,"Value",input$box_y)

    textInput(ns('box_ylab'),"+ y label:",value)

  })
  output$sidebox_grid<-renderUI({
    checkboxInput(ns('box_grid'),"+ Grid lines:",value=T)
  })

  output$sidebox_violin<-renderUI({
    checkboxInput(ns('box_violin'),"+ Violin:",value=F)
  })
  output$sidebox_varwidth<-renderUI({
    checkboxInput(ns("box_varwidth"),span("+ Varwidth:",tiphelp("Drawn boxes with widths proportional to the square-roots of the number of observations in the groups","right")),F, width="95px")
  })




  args_boxplot<-reactive({
    if(length(input$box_cex.label_panel)>0){
      cex.label_panel=input$box_cex.label_panel*input$box_base_size
    } else{
      cex.label_panel=1
    }
    res=getbox()
    req(length(res)>0)
    base_size=input$box_base_size
    args<-list(
      res=res,
      violin=input$box_violin,
      horiz=input$box_horiz,
      base_size=base_size,
      cex.axes=input$box_cex.axes*base_size,
      cex.lab=input$box_cex.lab*base_size,
      cex.main=input$box_cex.main*base_size,
      pal=input$box_palette,
      box_alpha=input$box_alpha,
      main=input$box_title,
      xlab=input$box_xlab,
      ylab=input$box_ylab,
      box_linecol=input$box_linecol,
      varwidth=input$box_varwidth,
      cex.label_panel=cex.label_panel,
      linewidth=input$box_linewidth,
      theme=input$box_theme,
      grid=input$box_grid,
      xlab_rotate=input$box_xlab_rotate,
      ylab_rotate=input$box_ylab_rotate,
      newcolhabs=vals$newcolhabs
    )
    args
  })

  output$box_btn<-renderUI({
    div(class=runval$box,actionButton(ns("run_boxplot"),"RUN",icon=icon("fas fa-sync")))
  })
  observeEvent(args_boxplot(),{
    req(!is.null(vals$pbox_plot))
    runval$box<-"save_changes_nice"
  })
  observeEvent(input$run_boxplot,{
    args<-args_boxplot()
    vals$pbox_plot<-do.call(ggbox,args)
    runval$box<-"btn_nice"
  })



  output$stats_pbox<-renderUI({
    req(input$box_width)
    req(input$box_width>10)
    req(input$box_heigth)
    req(input$box_heigth>10)

    #args<-readRDS("args.rds")
    div(
      column(12,renderPlot({

        vals$pbox_plot
      },
      width =input$box_width,
      height =input$box_heigth
      ))
    )
  })













  observe({
    if(is.null(vals$mds)){
      vals$mds_btn_class<-"save_changes_nice"
    } else{ vals$mds_btn_class<-"btn_nice"}
  })

  observeEvent(list(input$data_descX,input$mds_distance),ignoreInit = T,{
    vals$mds<-NULL
  })



  observeEvent(ignoreInit = T,input$mds_distance,{
    vals$cur_dist_mds<-input$mds_distance
  })






  output$stats_var<-renderUI({

    div(
      sidebarLayout(
        sidebarPanel(
          width=4,
          div(
            class="map_control_style2",
            style="color: #05668D",
            uiOutput(ns("varhisto_out")),
            uiOutput(ns("varhisto_metric")),
            uiOutput(ns("varhisto_colors")),
            uiOutput(ns("varhisto_sizeplot")),
            div(
              actionLink(ns('downp_summ_num'),tipify(span("+ Download",icon("fas fa-download")), "Download Plot"), style="button_active")
            ))),
        mainPanel(uiOutput(ns("histovar_main")))
      )
    )
  })
  output$varhisto_out<-renderUI({
    div(class="cogs_in",style="margin-bottom: 10px; padding:5px;",
        actionLink(ns("show_histovar"),"+ Select the Variables"),
        DT::dataTableOutput(ns('histo_var_x'))
        #uiOutput(ns('varhisto_out3'))
    )
  })
  output$varhisto_metric<-renderUI({
    div(class="div_fundo",
        div(actionLink(ns("show_metrics_act"),"+ Metrics")),
        uiOutput(ns('show_metrics')))
  })
  output$varhisto_colors<-renderUI({
    div(class="div_fundo",
        actionLink(ns("show_histo_colors_act"),'+ Plot params'),
        uiOutput(ns('show_histo_color'))

    )
  })
  output$varhisto_sizeplot<-renderUI({
    div(class="div_fundo",
        actionLink(ns("show_varhisto_sizeplot_act"),'+ Plot size'),
        uiOutput(ns('show_varhisto_sizeplot')))
  })
  output$show_metrics<-renderUI({
    div(style="padding-top: 10px;",
        checkboxGroupInput(ns("varhisto_metric"),NULL,c(
          'Min.' ,'1st Qu.',"Mean",'Median','3rd Qu.','Max.'
        ), selected=vals$varhisto_metric,inline = F),
      numericInput(ns("varhisto_ps_round"),"+ Round:", value=vals$varhisto_ps_round, step=010)
    )
  })
  output$show_histo_color<-renderUI({
    div(
      style="margin-left: 10px",
      colourpicker::colourInput(inputId=ns("varhisto_palette"),label = "+ Background:",showColour ="background",allowTransparent =T, value="black"),
      colourpicker::colourInput(inputId=ns("varhisto_border_col"),label = "+ Border",showColour ="background",allowTransparent =T, value="black"),
      uiOutput(ns("varhisto_cex")),
      uiOutput(ns("varhisto_w1")),
      uiOutput(ns("varhisto_w2")),
      uiOutput(ns("varhisto_w3"))
    )
  })
  output$show_varhisto_sizeplot<-renderUI({
    req(!is.null(vals$varhisto_ps_height))
    div(
      numericInput(ns("varhisto_ps_width"),"+ Width", value=  vals$varhisto_ps_width, step=010),
      numericInput(ns("varhisto_ps_height"),"+ Height", value=  vals$varhisto_ps_height, step=10)
    )
  })

   output$varhisto_cex<-renderUI({
    req(length(input$histo_var_x_rows_selected)>0)
    numericInput(ns("cextext"),"+ Text size:", value= 2, step=1)
  })
  output$varhisto_w1<-renderUI({
    numericInput(ns("varhisto_w1"),"+ Var width", value=  vals$varhisto_w1, step=0.05)
  })
  output$varhisto_w2<-renderUI({
    numericInput(ns("varhisto_w2"),"+ Metric width", value= vals$varhisto_w2, step=0.05)
  })
  output$varhisto_w3<-renderUI({
    numericInput(ns("varhisto_w3"),"+ Histo width", value= vals$varhisto_w3, step=0.05)
  })

  observeEvent(ignoreInit = T,input$downp_summ_num,{
    vals$hand_plot<-"variable summary"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_stats_fac,{
    vals$hand_plot<-"Factor plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_hist,{
    vals$hand_plot<-"histogram"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$mds_downp,{
    vals$hand_plot<-"mds"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$pca_downp,{
    vals$hand_plot<-"pca"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  output$dtab_summaries<-renderUI({
    if(is.null(vals$curview_summ_options)){vals$curview_summ_options<-'Data'}
    column(class="side_results",
           12, offset = 0,
           tabsetPanel(
             id=ns("summ_options"),selected=vals$summ_options,
             tabPanel(
               value="Datalist",
               title = "1.1. Datalist",
               datalist_render(getdata_descX())),
             tabPanel(
               value="Variables",
               title = "1.2. Numeric-Attribute",
               uiOutput(ns("stats_data")),
               uiOutput(ns("stats_var"))
             ),
             tabPanel(
               value="3. Factor-Attribute",
               title = "Factors",
               uiOutput(ns("stats_fac"))
             )
           )


    )
  })
  observeEvent(input$summ_options,{
    vals$summ_options<-input$summ_options
  })
  output$dtab_histogram<-renderUI({
    fluidRow(
      column(12,actionButton(ns("downp_hist"),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
      column(12,renderPlot({

        vals$phist<-phist(getdata_descX())
        vals$phist
      }))
    )
  })
  observeEvent(ignoreInit = T,input$cextext,{
    vals$cextext<-input$cextext
  })
  observeEvent(ignoreInit = T,input$show_metrics_act,{
    shinyjs::toggle("show_metrics")
  })
  observeEvent(ignoreInit = T,input$show_histovar,{
    shinyjs::toggle("histo_var_x")
  })
  observeEvent(ignoreInit = T,input$varhisto_w3,{

    shinyjs::hide('show_histo_color')

  },once=T)
  observeEvent(ignoreInit = T,input$histo_var_x_rows_selected,{

    shinyjs::hide('show_metrics')

  },once=T)
  observeEvent(input$data_descX,{
    data<-getdata_descX()
    vals$desc_maxhistvar<-ifelse(ncol(data)>10,10,ncol(data))
  })
  output$histo_var_x = DT::renderDataTable({
      req(is.numeric(vals$desc_maxhistvar))
      data<-vals$saved_data[[input$data_descX]]
      table=data.frame(Variables=colnames(data))
      DT::datatable(table, options=list(
        dom="t",

        lengthMenu = list(c(-1), c("All")),
        scrollX = TRUE,
        scrollY = "200px",
        autoWidth=F

      ), class ='compact cell-border',rownames=F,  colnames="",

      selection = list(mode = 'multiple', selected = c(1:vals$desc_maxhistvar)))
    })
  observe({
    req(is.null(vals$varhisto_ps_round))

    vals$varhisto_ps_round<-2

  })
  observeEvent(ignoreInit = T,input$varhisto_ps_round,{
    vals$varhisto_ps_round<-input$varhisto_ps_round
  })
  observe({
    req(is.null(vals$varhisto_metric))
    vals$varhisto_metric<-c('Min.' ,"Mean",'Max.')
  })
  observeEvent(ignoreInit = T,input$varhisto_metric,{
    vals$varhisto_metric<-input$varhisto_metric
  })
  observe({
    req(is.null(vals$varhisto_ps_width))
    vals$varhisto_ps_width<-550
  })
  observe({
    req(is.null(vals$varhisto_ps_height))
    vals$varhisto_ps_height<-400
  })
  observeEvent(ignoreInit = T,input$varhisto_ps_height,{
    vals$varhisto_ps_height<-input$varhisto_ps_height
  })
  observeEvent(ignoreInit = T,input$varhisto_ps_width,{
    vals$varhisto_ps_width<-input$varhisto_ps_width
  })
  observeEvent(ignoreInit = T,input$show_varhisto_sizeplot_act,{
    shinyjs::toggle("show_varhisto_sizeplot")
  })
  observeEvent(ignoreInit = T,input$varhisto_ps_height,{
    shinyjs::hide('show_varhisto_sizeplot')

  },once=T)
  observeEvent(ignoreInit = T,input$show_histo_colors_act,{
    shinyjs::toggle("show_histo_color")
  })
  observe({
    if(is.null(vals$varhisto_w1)){
      vals$varhisto_w1<-0.2
      vals$varhisto_w2<-vals$varhisto_w3<-0.35
    }
  })
  observeEvent(ignoreInit = T,input$varhisto_w1,{
    vals$varhisto_w1<-input$varhisto_w1
  })
  observeEvent(ignoreInit = T,input$varhisto_w2,{
    vals$varhisto_w2<-input$varhisto_w2
  })
  observeEvent(ignoreInit = T,input$varhisto_w3,{
    vals$varhisto_w3<-input$varhisto_w3
  })
  output$histovar_main<-renderUI({
    req(input$varhisto_ps_height)
    req(input$varhisto_ps_width)
    req(input$varhisto_ps_height>10)
    req(input$varhisto_ps_width>10)
    column(12,plotOutput(ns('histovar_plot'), height=paste0(input$varhisto_ps_height,"px"), width=paste0(input$varhisto_ps_width,"px")))
  })
  output$histovar_plot<-renderPlot({
    req(input$varhisto_palette)
    req(input$varhisto_border_col)
    req(input$varhisto_metric)
    req(input$varhisto_w1)
    req(input$varhisto_w2)
    req(input$varhisto_w3)
    req(input$varhisto_w3)
    req(input$cextext)
    data=getdata_descX()
    req(length(input$histo_var_x_rows_selected)>0)
    col<-input$varhisto_palette
    col_border<-input$varhisto_border_col
    selected<-colnames(data)[input$histo_var_x_rows_selected]
    data<-data[,selected, drop=F]
    str_numerics(data, cextext=input$cextext, col=col, border=col_border, show=input$varhisto_metric,width_varname=input$varhisto_w1, width_metrics=input$varhisto_w2, width_histo=input$varhisto_w3, round=input$varhisto_ps_round)
    vals$varplot<-recordPlot()
  })
  observeEvent(ignoreInit = T,input$varhisto_border_col,{
    vals$varhisto_border_col<-input$varhisto_border_col
  })
  observeEvent(ignoreInit = T,input$varhisto_palette,{
    vals$cur_varhisto_palette<-input$varhisto_palette
  })
  output$stats_fac<-renderUI({
   div(
     div(
       sidebarLayout(
         sidebarPanel(uiOutput(ns("pfac_side"))),
         mainPanel(uiOutput(ns("factorsplot")))
       )
     ),
     div(
       h5(strong("Factors:")),
       h5(strong("Structure:")),
       verbatimTextOutput(ns('strlabels')))
     )
  })


  output$out_pfac_col_lev<-renderUI({
    req(isTRUE(input$pfac_show_levels))
    colourpicker::colourInput(ns('pfac_col_lev'),"","lightsteelblue","background")
  })

  output$out_pfac_col_obs<-renderUI({
    req(isTRUE(input$pfac_show_levels))
    colourpicker::colourInput(ns('pfac_col_obs'),NULL,"lightcyan","background")
  })
  output$pfac_side<-renderUI({
    div(
      class="map_control_style2",
      style="color: #05668D",
      numericInput(ns('pfac_width'),'+ Bar width:',value=0.6, step=0.05, max=1),
      numericInput(ns('pfac_base_size'),"+ Base size:",value=12),
      div(inline(checkboxInput(ns('pfac_show_levels'),"+ Show levels:",value=T)),
          inline(uiOutput(ns("out_pfac_col_lev")))),

      div(
        inline(checkboxInput(ns('pfac_show_obs'),"+ Show obs/levels:",value=T)),
        inline(uiOutput(ns("out_pfac_col_obs")))
      ),
      pickerInput(
        inputId = ns('pfac_border_palette'),
        label = '+ Border:',
        choices =     vals$colors_img$val,
        choicesOpt = list(content =vals$colors_img$img),
        selected="turbo",
        options=list(container="body")
      ),
      pickerInput(
        inputId = ns('pfac_fill_palette'),
        label = '+ Fill:',
        choices =     vals$colors_img$val,
        choicesOpt = list(content =vals$colors_img$img),
        selected="turbo",
        options=list(container="body")
      ),
      numericInput(ns('pfac_pastel'),"+ Fill light:",0.4),
      textInput(ns('pfac_title'),'+ Title:',value="Observation Totals by Factor and Level"),
      textInput(ns('pfac_xlab'),"+ x lab:",value="Factors"),
      textInput(ns('pfac_ylab'),"+ y lab:",value='Number of Observations'),

      actionLink(ns("downp_stats_fac"),span("+ Download plot",icon("fas fa-download")), style="button_active")
    )
  })
  prepare_ggfactors<-reactive({
    prepare_factors(attr(getdata_descX(),"factors"))
  })
  pfactors<-reactive({
    req(input$pfac_pastel)
    args<-list(
      ggfactors=prepare_ggfactors(),
      width=input$pfac_width,
      base_size=input$pfac_base_size,
      show_levels=input$pfac_show_levels,
      col_lev=input$pfac_col_lev,
      show_obs=input$pfac_show_obs,
      col_obs=input$pfac_col_obs,
      title=input$pfac_title,
      xlab=input$pfac_xlab,
      ylab=input$pfac_ylab,
      border_palette=vals$newcolhabs[[input$pfac_border_palette]],
      fill_palette=vals$newcolhabs[[input$pfac_fill_palette]],
      pastel=input$pfac_pastel
    )
    vals$factorsplot<-do.call(gg_factors,args)
    vals$factorsplot

  })

  output$factorsplot<-renderUI({
    div(
      renderPlot(pfactors())
    )
  })

  output$psummary<-renderPrint({
    data=getdata_descX()
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




  output$summary_pca<-DT::renderDataTable({


    req(!is.null(pca_model()))
    res<- summary(pca_model())
    center<-res$center
    scale<-res$scale
    sdev<-res$sdev
    res$center<-data.frame(center)
    res$scale<-data.frame(scale)
    res$sdev<-data.frame(sdev)
    res<-lapply(res, data.frame)
    vals$pca_out<-res[[pic_pca_results()]]
    vals$pca_out
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = T,class ='cell-border compact stripe')












  output$stats_data<-renderUI({
    data=getdata_descX()
    df<-data.frame(nas=sum(is.na(unlist(data))),
                   nrow=nrow(data),
                   ncol=ncol(data))
    colnames(df)<-c("Missing values",
                    "Number of rows",
                    "Number of columns")
    rownames(df)<-""
    renderTable(df)


  })
  output$Locate_NA<-renderUI({
    req(anyNA(unlist(getdata_descX())))
    div(
      column(12,strong("Missing Values:")),
      column(12,
             div(  tags$style('#missing_values td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                   tags$style('#missing_values th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),

                   inline(
                     DT::dataTableOutput(ns("missing_values"))
                   )
             ))
    )
  })
  output$missing_values<-DT::renderDataTable({
    data=getdata_descX()
    res0<-res<-which(is.na(data), arr.ind=TRUE)
    req(nrow(res)>0)
    for(i in 1:nrow(res)){
      res0[i,1]<-rownames(data)[res[i,1]]
      res0[i,2]<-colnames(data)[res[i,2]]
    }
    colnames(res0)<-c("ID","Variable")
    rownames(res0)<-NULL
    res0
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = F,class ='cell-border compact stripe')







  output$strlabels<-renderPrint({

    ppsummary("----------------")
    ppsummary(paste("Missing values:",sum(is.na(attr(getdata_descX(),"factors")))))
    ppsummary("----------------")
    str(attr(getdata_descX(),"factors")[rownames(getdata_descX()),,drop=F])
  })
  ##
  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
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



  output$save_breakpoints<-renderUI({
    if(is.null(vals$bag_smw)){
      class="novo"
    } else{ class="save_changes_nice"}

    if(!isFALSE(vals$splitBP)){
      if(!any(unlist(lapply(attr(vals$saved_data[[input$segrda_X]],"factors"), function (x) identical(x,as.vector(vals$splitBP)))))){
        popify(
          div(class=class,id=ns("save_changes_nice_bp"),
              bsButton(ns('tools_saveBP'),icon("fas fa-save"),style='save_button')
          ),"Save breakpoints from DP",
          "this action divides the observations according to the breakpoints and assigns a factor to each split"
        )

      }
    }

  })









  output$dtab_rid<-renderUI({
    sidebarLayout(
      sidebarPanel(
        div(
          class="map_control_style2",
          style="color: #05668D",
          uiOutput(ns('rid_side'))
        )),
      mainPanel(

        uiOutput(ns('rid_out'))
      )
    )
  })

  output$run_ridges_btn<-renderUser({
    div(class=runval$rid,
        actionButton(ns('run_ridges'), 'RUN', icon=icon("fas fa-sync")),
    )
  })

  args_ridges<-reactive({
    req(input$rid_heigth)
    req(input$rid_y)
    req(input$data_descX)
    data=vals$saved_data[[input$data_descX]]
    factors<-attr(data,"factors")

    req(length(input$rid_x_rows_selected)>0)
    data<-data[,input$rid_x_rows_selected, drop=F]

    fac<-factors[,input$rid_y]
    #savereac()
    data$class<-fac

    args<-list(data=data,
               fac=input$rid_y,
               palette=input$rid_col,
               newcolhabs=vals$newcolhabs,
               ncol=input$rid_ncol,
               title=input$rid_tittle,
               base_size=input$rid_base_size)
    args
  })
  observeEvent(input$run_ridges,{
    args<-args_ridges()
    vals$rid_plot<-do.call(plot_ridges,args)
    runval$rid<-"btn_nice"
  })

  observeEvent(args_ridges(),{
    req(input$rid_heigth)
    req(input$run_ridges)
    runval$rid<-"save_changes_nice"
  })



  output$rid_side<-renderUI({

    ##here
    req(input$data_descX)
    data<-vals$saved_data[[input$data_descX]]
    factors<-attr(data,"factors")


    div(
      div(align='right',style="margin-top: -19px; margin-right: -19px",
        uiOutput(ns('run_ridges_btn'))
      ),

      div(class="div_fundo",
          pickerInput(inputId=ns("rid_y"),label = "+ X (factor)",
                      choices =rev(colnames(factors)),selected=vals$rid_y),
          div(actionLink(ns("show_obs_selection"),"+ Y (numeric)")),
          div(uiOutput(ns('rid_x_variables')),)
      ),

      uiOutput(ns("ridplot_options"))
    )
  })

  output$ridplot_options<-renderUI({
    div(class="sidebar_float",
      div(class="div_fundo",
        pickerInput(inputId=ns("rid_col"),label = "+ Palette:",choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1]),
        textInput(ns('rid_tittle'),"+ Title",""),
        numericInput(ns('rid_base_size'),"+ Base size",11),
        numericInput(ns('rid_ncol'),"+ Nº columns",3),
        numericInput(ns('rid_width'),"+ Plot widht",700),
        numericInput(ns('rid_heigth'),"+ Plot heigth",300),


        div(
          actionLink(ns("rid_downp"),"Download plot", style="button_active")
        )


      )
      )
  })


  output$rid_x_variables<-renderUI({
    div(
        fluidPage(

          DT::dataTableOutput(ns('rid_x'))


        )
    )

  })

  observeEvent(ignoreInit = T,input$show_obs_selection,{
    shinyjs::toggle("rid_x")
  })


  output$rid_x = DT::renderDataTable(
    {      data<-vals$saved_data[[input$data_descX]]
    table=data.frame(Variables=colnames(data))
    DT::datatable(table, options=list(
      dom="t",

      lengthMenu = list(c(-1), c("All")),
      scrollX = TRUE,
      scrollY = "200px",
      autoWidth=F

    ), class ='compact cell-border',rownames=F,  colnames="",

    selection = list(mode = 'multiple', selected = c(1:3)))
    })







  output$rid_out<-renderUI({
    req(!is.null(vals$rid_plot))
    width=paste0(input$rid_width,"px")
    height=paste0(input$rid_heigth,"px")
    renderPlot(vals$rid_plot, width=input$rid_width,height=input$rid_heigth)

  })




  observeEvent(list(input$rid_x_rows_selected,
                    input$data_descX,
                    input$rid_col,
                    input$rid_y,
                    input$rid_col,
                    input$rid_ncol
  ) ,ignoreInit = T,{
    req(length(input$rid_x_rows_selected)>0)
    req(!is.null(vals$rid_plot))

  })

  output$boxplot_out<-renderUI({
    sidebarLayout(
      sidebarPanel(
        div(
          div(uiOutput(ns('side_boxplot'))),
          div(style="border-top: 1px solid #05668D;",
              actionLink(
                ns("downp_box"),span("+ Download",icon("fas fa-download")), style="button_active"
              )

          )

        )
      ),
      mainPanel(uiOutput(ns("stats_pbox")))
    )
  })
  output$stats_cbox<-renderUI({
    div(class="well2",style="padding-top: 5px; padding-bottom: 2px;padding-left: 10px ",
      div(class="map_control_style2",style="color: #05668D; display: inline-block;  vertical-align: middle;",
          inline(uiOutput(ns("box_y_input"))),
          inline(uiOutput(ns("boxplot_X"))),
          inline(uiOutput(ns("filter_box1"))),
          inline(uiOutput(ns("filter_box2")))
      ),
      inline(
        tags$div(
          uiOutput(ns("box_btn"))
        )
      )
    )
  })





  output$box_y_input<-renderUI({
    data<-getdata_descX()
    div(
      div(tipify(
        strong("Y ~"),
        " y is the data values to be split into groups according to the grouping variable", options = list(container="body")
      )),
      pickerInput( ns("box_y"),NULL,choices = colnames(data),selected= colnames(data)[1], multiple=T))

  })

  output$boxplot_X<-renderUI({
    div(
      div(strong("Factor:")),
      pickerInput(ns("boxplot_X"),NULL,
                  choices =rev(colnames(attr(vals$saved_data[[input$data_descX]],
                                             "factors"))),
                  selected=vals$boxplot_X, width="200px")
    )
  })
  observeEvent(ignoreInit = T,input$boxplot_X,{vals$box_y<-input$box_y})
  observeEvent(ignoreInit = T,input$boxplot_X,{vals$boxplot_X<-input$boxplot_X})
  output$filter_box1<-renderUI({
    div(
      div(strong("Filter:")),
      pickerInput(ns("filter_box1"),NULL,choices = c("none", colnames(attr(getdata_descX(),"factors"))),selected=filter_box1_cur$df, width="200px"))
  })
  output$filter_box2<-renderUI({
    req(input$filter_box1)
    if (input$filter_box1 != "none") {
      data = getdata_descX()
      labels<-attr(data,"factors")[rownames(data), input$filter_box1]
      div(
        div(strong("Class:")),
        pickerInput(ns("filter_box2"),
                    NULL,
                    choices = c(levels(as.factor(labels))),
                    selected=filter_box2_cur$df, width="200px")
      )
    }
  })




  getdata_descX<-reactive({
    req(input$data_descX)
    vals$saved_data[[input$data_descX]]})


  getbox<-reactive({
    req(input$boxplot_X)
    req(input$box_y)
    req(input$filter_box1)
    data=getdata_descX()
    labels<-attr(data,"factors")
    pic<-1:nrow(data)
    req(any(input$boxplot_X%in%colnames(labels)))
    x<-labels[input$boxplot_X]
    req(any(input$box_y%in%colnames(data)))
    y<-data[input$box_y]

    if (input$filter_box1 != "none") {
      filtro<-as.character(input$filter_box1)
      filtro2<-as.character(input$filter_box2)
      pic<-which(as.character(labels[, filtro]) == filtro2)


    }
    res = data.frame(x,y)[pic,]
    res[,1]<-res[,1]
    res
    # saveRDS(res,"res.rds")
    #re<-readRDS('res.rds')
    # levels(re$Consenso)
    #vals<-readRDS("savepoint.rds")
    # levels(attr(vals$saved_data[["SANTOS_C1"]],"factors")$Consenso)
  })



  observeEvent(ignoreInit = T,input$data_descX,{
    req(length(vals$saved_data)>0)
    vals$cur_data<-input$data_descX
  })
  observeEvent(ignoreInit = T,input$rid_downp,{
    vals$hand_plot<-"Ridge plot"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals)})
  observeEvent(ignoreInit = T,input$rid_y,
               vals$rid_y<-input$rid_y)
  observeEvent(ignoreInit = T,input$box_y,{
    box_y_cur$df<-input$box_y
  })
  observeEvent(ignoreInit = T,input$filter_box2,{
    filter_box2_cur$df<-input$filter_box2
  })
  observeEvent(ignoreInit = T,input$filter_box1,{
    filter_box1_cur$df<-input$filter_box1
  })
  observeEvent(ignoreInit = T,input$boxplot_X,{
    boxplot_X_cur$df<-input$boxplot_X
  })

  observeEvent(ignoreInit = T,input$downp_box,{
    vals$hand_plot<-'boxplot'
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })

  observeEvent(input$run_mds,{
    req(input$mds_distance)
    withProgress(message="Running...",{
      vals$mds<-metaMDS(getdata_descX(), distance = input$mds_distance)
    })


  })




  observeEvent(ignoreInit = T,input$downcenter_rda,{
    vals$hand_down<-"rda"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })


  ##################
  ###  SEGRDA    ###
  ### ###############

  output$segrda_panels<-renderUI({
    req(input$segrda_X)
    req(input$segrda_Y)

    column(12,

           tabsetPanel(id=ns("segrda_panels"),
                       selected=vals$segrda_panels,

                       tabPanel("9.1. Split Moving window",
                                uiOutput(ns("segrda_smw"))

                       ),
                       tabPanel("9.2. Dissimilarity Profile",
                                uiOutput(ns("segrda_dp"))

                       ),

                       tabPanel("9.3. Piecewise Redundancy Analysis",
                                p(strong("Piecewise RDA")),
                                uiOutput(ns("pw_out")),
                                uiOutput(ns("pwRDA_out"))
                                )))
  })


  observeEvent(ignoreInit = T,input$segrda_Y,{
    vals$cur_segrda_Y<-input$segrda_Y})


  observeEvent(ignoreInit = T,input$segrda_X,{
    vals$cur_segrda_X<-input$segrda_X})

  output$databank_storage<-renderUI({

    div(
      column(12,
             div(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")),
             div(vals$hand_save2,style="color: gray"),
             div(vals$hand_save3))

    )
  })
  output$save_confirm<-renderUI({
    actionButton(ns("data_confirm"),strong("confirm"))
  })
  output$ord_side<-renderUI({
    fluidRow(class="map_control_style",style="color: #05668D",
             div(
               span("+",
                    checkboxInput(ns("segrda_scale"),span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T, width = "100px")
               )
             ),

             div(
               span("+",
                    inline(
                      checkboxInput(ns("segrda_ord"),strong("Axis ordination",pophelp(NULL,"Both the SMW and pwRDA analyses depend on ordered datasets. Defaults to ordering both response and explanatory matrices using one of the axes of the RDA model. If unchecked,please make sure all your inputs are already ordered.")), value=T)
                    ),
                    inline(uiOutput(ns("ord_check")))
               )
             ),
             uiOutput(ns("ord_sure"))


    )
  })
  output$ord_check<-renderUI({
    req(isTRUE(input$segrda_ord))
    inline(numericInput(ns("axis_ord_segrda"),NULL, value=1, step=1, width="75px"))
  })
  output$ord_sure<-renderUI({
    req(isFALSE(input$segrda_ord))
    div(style='white-space: normal;',
        strong("Wargning:", style="color: SeaGreen"),"Make sure both X and Y are previously ordered")
  })
  output$ordplot_matrix<-renderUI({
    req(isTRUE(input$segrda_ord))

    #mybreaks<-vals$window_pool

    fluidRow(
      renderPlot({
        sim1o<-getord()
        #sim1o<-readRDS('sim1o.rds')
        mybreaks<-vals$window_pool
        #mybreaks<-c(2,50,141)
        xo<-sim1o$xo ## ordered explanatory matrix.
        yo<-sim1o$yo ## ordered community matrix (untransformed).
        x<-sim1o$y
        par(mfrow = c(1, 2), mgp = c(1, 1, 0), cex = 0.9)
        image(x, main = "Original response data", col = topo.colors(100), axes = F,

              xlab = "Observations", ylab = "Variable values")
        # abline(v=scales::rescale(mybreaks,c(0,1)), col="red")


        image(yo, main = "Ordered response data", col = topo.colors(100), axes = F,
              xlab = "Observations", ylab = "Variable values")
        #abline(v=scales::rescale(mybreaks,c(0,1)), col="red")


      })
    )
  })
  output$segrda_smw<-renderUI({
    tabsetPanel(id=ns("smw_panels"),
      tabPanel("9.1.1 Data ordination",value="swm_1",
        sidebarLayout(
          sidebarPanel(
            uiOutput(ns('side_smw')),
            br(),
            uiOutput(ns('run_smw'))
          ),
          mainPanel(
            uiOutput(ns("ordplot_matrix"))
          )
        )

      ),
      tabPanel("9.1.2 SMW results",
               value="swm_2",
               uiOutput(ns("go_smw")))
    )
  })

  output$run_smw<-renderUI({
    div(style="margin-left: 20px",
        class="go_smw_btn save_changes_nice",
        actionButton(ns("go_smw"),strong( img(src=smw_icon,height='20',width='20'),"run SMW"), style="button_active")
    )
  })

  observeEvent(input$go_smw,{
    shinyjs::removeCssClass(selector=".go_smw_btn",class="save_changes_nice")
  })

  observeEvent(list(input$segrda_X,
                    input$segrda_Y,
                    input$segrda_scale,
                    input$segrda_ord,
                    input$axis_ord_segrda,
                    input$custom_windows,
                    input$smw_dist,
                    input$smw_rand_help,
                    input$smw_rand,
                    input$smw_nrand),ignoreInit = T, {
    shinyjs::addCssClass(selector=".go_smw_btn",class="save_changes_nice")
  })




  output$ord_windows<-renderUI({
    div(
      tipify(icon("fas fa-question-circle",style="color: gray"),"Enter a vector of breakpoints (comma delimited, within the data range)"),
      "+ Windows",
      textInput(ns('custom_windows'), NULL, paste0(get_windows(),collapse=", "), width="200px"),

    )
  })

  get_windows<-reactive({
    req(input$segrda_X)
    data<-vals$saved_data[[input$segrda_X]]
    req(nrow(data)>0)
    w<-1:(nrow(data)/2)
    w<-  w[which(( w %% 2) == 0)]
    w<-round(seq(10,w[length(w)], length.out=5))
    w[which(( w %% 2) != 0)]<-w[which(( w %% 2) != 0)]+1
    w
  })
  is.even<-function(x){ x %% 2 == 0}
  getpool<-reactive({
    mybreaks<-NULL
    req(input$segrda_X)
    req(length(input$custom_windows)>0)
    req(!is.na(input$custom_windows))
    mybreaks<-as.numeric(unlist(strsplit(input$custom_windows,",")))
    data<-vals$saved_data[[input$segrda_X]]
    cond0<-length(mybreaks)>0
    validate(need(cond0,"The windows vector is empty"))
    cond1<-sum(sapply(mybreaks,is.even))==length(mybreaks)
    cond2<-min(mybreaks)>=2
    cond2_res<-paste("The maximum window size cannot exceed the number of observations:", nrow(data))
    validate(need(cond1,"Window sizes must be even"))
    validate(need(cond2,"The minimum allowed size of the windows is 2"))
    validate(need(max(mybreaks)<=nrow(data),cond2_res))
    mybreaks

  })
  output$smw_tuning<-renderUI({
    div(
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"Dissimilarity index"),"+ Distance:"),
             inline(
               pickerInput(ns("smw_dist"),NULL, choices=c("bray","euclidean","manhattan","jaccard"), width="100px")
             )
        )
      ),
      div(
        span(span(tipify(actionLink(ns("smw_rand_help"),icon("fas fa-question-circle")),"The type of randomization for significance computation. Click for details"),"+ Randomization:"),
             inline(
               pickerInput(ns("smw_rand"),NULL, choices=c("shift","plot"), width="70px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The number of randomizations"),"+ n.rand:"),
             inline(
               numericInput(ns("smw_nrand"),NULL, value=10, width="100px")
             )
        )
      ),
      uiOutput(ns("down_we_out"))
    )
  })
  output$down_we_out<-renderUI({
    req(length(vals$smw_dp)>0)
    tipify(
      actionLink(
        ns('downp_we'),span("+ Download",icon("fas fa-download")), style="button_active"
      ),
      "Download plot"
    )
  })

  output$side_smw<-renderUI({
    div(
      id="smw_params",
      class="map_control_style",style="color: #05668D",
              uiOutput(ns("ord_side")),
              uiOutput(ns("ord_windows")),
              uiOutput(ns("smw_tuning"))
    )
  })
  output$go_smw<-renderUI({
    getpool()
    validate(need(!is.null(vals$window_pool),"The window pool is empty. Use the arrow button to include the window sizes"))
    validate(need(length(vals$smw_dp)>0,"Please click 'run SMW' button"))



    fluidRow(
      column(12,

             renderPlot({



               if(length(vals$window_pool)>1){w.effect=TRUE
               main="Window size effect"} else{w.effect=F
               main="Dissimilary Profile"}

               suppressWarnings(plot( vals$smw_dp, w.effect =w.effect  , main=main))
               vals$plot_we<-recordPlot()

             })
      )
    )
  })

  observeEvent(vals$max_seq,{

    req(!is.null(vals$max_seq))
    vals$dp_seq.sig<-attr(max_seq(),"min")
  })



  output$plot_dp<-renderPlot({

    req(input$dp_seq.sig)
    req(input$dp_cex)
    req(input$dp_BPs)
    max_seq<-max_seq()
    validate(need(input$dp_seq.sig<=max_seq,paste(
      "DP shows '", sum(DP_smw()[,5]!="ns"),"' significant dissimilarity values but no breakpoint could be determined for seq.sig='",input$dp_seq.sig,"The maximum value for this input must be","max_seq"
    )))
    smw<- vals$smw_dp
    getDP()
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    par(cex=input$dp_cex)
    suppressWarnings(
      suppressMessages(
        plot(smw,w=dp_w, sig=input$dp_sig, z=input$dp_z,   BPs=dp_BPs,
             seq.sig=input$dp_seq.sig, bg= getcolhabs(vals$newcolhabs,input$dp_palette,nlevels(as.factor(vals$splitBP[,1]))),bg_alpha=input$dp_bg,cols=c(getcolhabs(vals$newcolhabs,input$dp_dcol,1),getcolhabs(vals$newcolhabs,input$dp_scol,1),getcolhabs(vals$newcolhabs,input$dp_bcol,1)))
      )
    )



    vals$plot_dp<-recordPlot()
  })
  output$dp_extract<-renderUI({

    dp<-getDP()
    fluidRow(
      column(12,renderPrint({dp}))
      #renderPrint({vals$splitBP})
    )
  })
  output$dp_view_dp<-renderUI({

    tipify(
      actionLink(
        ns('downcenter_dp_smw'),span("+ Download",icon("fas fa-table")), style="button_active"
      ),
      "Download DP results"
    )
  })
  observeEvent(ignoreInit = T,input$dp_seq.sig,{
    vals$dp_seq.sig<-input$dp_seq.sig
  })
  output$side_dp<-renderUI({
    if(is.null(vals$dp_seq.sig)){
      vals$dp_seq.sig<-3
    }

    validate(need(length(vals$smw_dp)>0,"You need to run SMW analysis first"))
    fluidRow(
      class="map_control_style",style="color: #05668D",
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"A target window size from which results will be extracted. If empty return z-scores averaged over the set of window sizes"),'+ w:'),
             inline(
               numericInput(ns("dp_w"),NULL, value=NULL, width="75px")
             )
        )
      ),
      div(
        span(span(actionLink(ns("dp_index_help"),tipify(icon("fas fa-question-circle"),"The result to be extracted. Click for details")),'+ index:'),
             inline(
               pickerInput(ns("dp_index"),NULL,choices=c("dp","rdp","md","sd","oem","osd","params"), width="75px")
             )
        )
      ),
      div(
        span(span(actionLink(ns("dp_sig_help"),tipify(icon("fas fa-question-circle"),"Significance test for detecting dissimilarity values that differs significantly from those appearing in a random pattern. Click for details")),'+ sig'),
             inline(
               pickerInput(ns("dp_sig"),NULL,choices=c("z","sd","sd2","tail1"), width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The critical value for the significance of z-values"),"+ z:"),
             inline(
               numericInput(ns("dp_z"),NULL, value=1.85,step=0.01, width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"Defines if the breakpoints should be chosen as those sample positions corresponding to the maximum dissimilarity in a sequence of significant values (max) or as those sample positions corresponding to the median position of the sequence (median). Defaults to BPs=max. If empty the breakpoints are not computed"),"+ BPs:"),
             inline(
               pickerInput(ns("dp_BPs"),NULL,choices=c("","max","median"), selected = "max", width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle", style="color: gray"),"The maximum length of consecutive, significant values of dissimilarity that will be considered in defining the community breakpoints"),"+ seq.sig:"),
             inline(
               numericInput(ns("dp_seq.sig"),NULL, value=vals$dp_seq.sig,step=1, width="75px", min=1)
             )
        )
      ),


      div(uiOutput(ns("dp_view_plot")))


    )

  })
  output$dp_view_plot<-renderUI({

    div(
      div(class="palette",
          span("+ Palette",
               inline(
                 pickerInput(inputId=ns("dp_palette"),
                             label = NULL,
                             choices =     vals$colors_img$val[getgrad_col()],
                             choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), width="75px")
               )
          )
      ),

      div(
        span("+ size",
             inline(
               numericInput(ns("dp_cex"), NULL,value=1,min=0.1,step=0.1, width="75px")
             )
        )
      ),

      div(
        span("+ diss col",
             inline(
               pickerInput(inputId=ns("dp_dcol"),
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), width="75px")
             )
        )
      ),
      div(
        span("+ sig col",
             inline(
               pickerInput(inputId=ns("dp_scol"),
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), selected=  vals$colors_img$val[getsolid_col()][4], width="75px")
             )
        )
      ),
      div(
        span("+ bp col",
             inline(
               pickerInput(inputId=ns("dp_bcol"),
                           label = NULL,
                           choices =   vals$colors_img$val[getsolid_col()],
                           choicesOpt = list(content =   vals$colors_img$img[getsolid_col()]), selected=  vals$colors_img$val[getsolid_col()][3], width="75px")
             )
        )
      ),

      div(
        span("+ bg",
             inline(numericInput(ns("dp_bg"), NULL,value=0.5,min=0,max=1,step=0.05, width="75px"))
        )
      ),
      tipify(
        actionLink(
          ns('downp_dp'),span("+ Download",icon("fas fa-download"))
        ),
        "Download plot"
      )
    )
  })
  output$segrda_dp<-renderUI({
    validate(need(length(vals$smw_dp)>0,"You need to run SMW analysis first"))
    div(
      div(strong("Dissimilary Profile"),
          inline(uiOutput(ns("save_breakpoints")))),
      tabsetPanel(id=ns("dp_view"),selected=vals$dp_view,
                  tabPanel("9.2.1. Plot", value="Plot",
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput(ns("side_dp"))
                             ),
                             mainPanel(
                               plotOutput(ns("plot_dp"))
                           )
                           )),
                  tabPanel("9.2.1. DP results", value="DP results",
                           div(uiOutput(ns("dp_view_dp"))),
                           uiOutput(ns("dp_extract"))))
    )
  })
  observeEvent(ignoreInit = T,input$dp_view,
               vals$dp_view<-input$dp_view)
  get_breaks_from_factor<-reactive({
    x<-vals$saved_data[[input$segrda_X]]
    y<-vals$saved_data[[input$segrda_Y]]
    bp<-attr(x,"factors")[,input$bp_column]
    xord<-x[order(as.numeric(bp)),]
    yord<-y[order(as.numeric(bp)),]
    breaks<-bp[order(as.numeric(bp))]
    breaks<-which(diff(as.numeric(breaks))==1)
    pw_in<-list(
      breaks=breaks,
      xord=xord,
      yord=yord
    )
  })

  observeEvent(ignoreInit = T,input$segrda_X,{
    x<-attr(vals$saved_data[[input$segrda_X]],"factors")
  })

  choices_bp_column<-reactive({
    req(input$segrda_X)
    x<-attr(vals$saved_data[[input$segrda_X]],"factors")
    colnames(x)
  })

  observeEvent(ignoreInit = T,input$bp_column,
               vals$bp_column<-input$bp_column)

  output$user_bp<-renderPrint(vals$bag_user_bp)
  output$getDP<-renderPrint({
    req(!isFALSE(vals$splitBP))
    suppressWarnings(bp(getDP()))})


  output$pwRDA_splitref<-renderUI({
    div(
      tipify(icon("fas fa-question-circle",style="color: gray"),"Enter a vector of breakpoints (comma delimited, within the data range)"),
      "+ Split reference [Y Datalist]",
      pickerInput(ns('bp_column'), NULL,choices_bp_column() , width="200px", selected=vals$bp_column)
    )
  })
  output$pw_out<-renderUI({

    column(12,class='well3',
      div(
        span(
          inline(uiOutput(ns("pwRDA_splitref"))),
          inline(numericInput(ns("pw_nrand"), 'n.rand', value=99, width="75px")),
          inline(uiOutput(ns("run_pwRDA"))),
          inline(uiOutput(ns('pwRDA_models_out'))))
      )
      )
  })

output$run_pwRDA<-renderUI({
  class="run_pwRDA_button save_changes_nice"
  if(isTRUE(vals$pwmodel_ok)){
    class="run_pwRDA_button"
  }
  div(class=class,
      actionButton(ns("run_pwRDA"),strong(img(src=pw_icon,height='20',width='20'),"run pwRDA"))
  )
})



observe({
  res<-F
  if(lenght_ok(input$segrda_X)){
    models<-attr(vals$saved_data[[input$segrda_X]],"pwRDA")
    if(lenght_ok(models)){
      if(lenght_ok(input$pwRDA_models)){
        model<-models[[input$pwRDA_models]]
        if(lenght_ok(model)) {
          res=T
        }
      }
    }
  }
  vals$pwmodel_ok<-res

  res
})



lenght_ok<-function(x){
  length(x)>0
}
observeEvent(input$run_pwRDA,{
  shinyjs::removeCssClass(selector=".run_pwRDA_button",class="save_changes_nice")
})


observeEvent(ignoreInit = T,
  list(
    input$segrda_X,
    input$segrda_Y,
    input$bp_column,
    input$pw_nrand
  ),{
    req(input$segrda_X)
    req(input$segrda_Y)
    req(input$bp_column)
    req(input$pw_nrand)
    req(input$pwRDA_models)
    shinyjs::addCssClass(selector=".run_pwRDA_button",class="save_changes_nice")
    attr(vals$saved_data[[input$segrda_X]],"pwRDA")[["pwRDA (unsaved)"]]<-list()
    selected<-"pwRDA (unsaved)"
    choices<-c(names(attr(vals$saved_data[[input$segrda_X]],"pwRDA")),selected)
    updatePickerInput(session,'pwRDA_models',choices=choices,selected=selected)

  }
)


  output$pwRDA_models_out<-renderUI({
    choices<-names(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))
    div(
      inline(pickerInput(ns('pwRDA_models'),"Results",choices, width="200px", selected=vals$pwRDA_models)),
      inline(uiOutput(ns('save_pw'))),
      inline(actionButton(ns("delete_pwRDA_models"),icon("fas fa-trash-alt")))
    )
  })

  observeEvent(ignoreInit = T,input$delete_pwRDA_models,{
    attr(vals$saved_data[[input$segrda_X]],"pwRDA")[[input$pwRDA_models]]<-NULL
  })
  output$save_pw<-renderUI({
    req(input$pwRDA_models)
    req(isTRUE(vals$pwmodel_ok))
    class="novo"
    if(input$pwRDA_models=="pwRDA (unsaved)"){
      class="save_changes_nice"
    }
    div(class=class,
        actionButton(ns("save_pwRDA"),icon("fas fa-save"), style="button_active")
    )
  })
  observeEvent(ignoreInit = T,input$pwRDA_models,
               vals$pwRDA_models<-input$pwRDA_models)
  observeEvent(ignoreInit = T,input$save_pwRDA,{
    vals$hand_save<-"Save pwRDA model in"
    vals$hand_save2<-div(span("Target:",em(input$segrda_X,style="color: SeaGreen")))
    vals$hand_save3<-NULL
    showModal(module_desctools())
  })
  observeEvent(ignoreInit = T,input$disegrda_sp_summ,
               vals$disegrda_sp_summ<-input$disegrda_sp_summ)

  output$segrda_side_results<-renderUI({
    div(
      div(
        span(
          "+ Results:",
          inline(
            pickerInput(ns("disegrda_sp_summ"),NULL,choices=c('Summary stats','Importance (unconstrained)',"Importance (constrained)",'Variable scores','Observation scores','Linear constraints','Biplot'), selected=vals$disegrda_sp_summ,  options=list(container="body"), width="200px")
          )
        )
      ),
      div(
        span("+ Axes",
             inline(
               numericInput(ns("segrda_axes"),NULL, value=2)
             )
        )
      ),
      div(
        tipify(
          actionLink(
            ns('downcenter_segrda'),span("+ Download selected result",icon("fas fa-table")), style="button_active"
          ),
          "Download selected results"
        )

      ),
      div(
        tipify(
          downloadLink(
            ns('downmodel_segrda'),span("+ Download model"), style="button_active"),
          "Download model as .rds file"
        )
      )
    )
  })



  output$downmodel_segrda<-{
    downloadHandler(
      filename = function() {
        paste0("pwRDA","_", Sys.Date(),".rds")
      }, content = function(file) {
        model<-attr(vals$saved_data[[input$segrda_X]],"pwRDA")[[input$pwRDA_models]]
        saveRDS(model,file)
      })

  }

  observeEvent(ignoreInit = T,input$segrda_view,
               vals$segrda_view<-input$segrda_view)





  output$segrda_print<-renderPrint({segrda_summary()})




  output$pwRDA_out<-renderUI({
    req(input$segrda_X)
    req(input$pwRDA_models)
    models<-attr(vals$saved_data[[input$segrda_X]],"pwRDA")
    req(!is.null(models))
    model<-models[[input$pwRDA_models]]
    req(!is.null(model))
    tabsetPanel(id=ns("segrda_view"),selected=vals$segrda_view,
      tabPanel("9.3.1. Plot", value="Plot",
               div(

                 sidebarLayout(
                   sidebarPanel(
                     div(class="map_control_style2",style="color: #05668D",
                         class="sidebar_float",
                         uiOutput(ns("side_segrda")))),
                   mainPanel(uiOutput(ns("segrda_plot")))
                 )
               )
      ),
      tabPanel("9.3.2. Summary", value="Summary",
        sidebarLayout(
          sidebarPanel(
            div(class="map_control_style",style="color: #05668D",
                uiOutput(ns("segrda_side_results")))),
          mainPanel(verbatimTextOutput(ns("segrda_print")))
        )
      )
    )
  })

  max_seq<-reactive({
    validate(need(any(DP_smw()[,5]!='ns'),"DP without any significant dissimilarity value: no breakpoint could be determined."))

    df<-DP_smw()[,c(1,5)]
    colnames(df)<-c("a","b")
    new=transform(df, Counter = ave(a, rleid(b), FUN = seq_along))
    max_seq<-max(new[new[,2]=="*",3])
    attr(max_seq,"min")<-if(max_seq!=1){
      min( new[new[,2]=="*",3][new[new[,2]=="*",3]!=1])} else{
        min(new[new[,2]=="*",3])
      }



    vals$max_seq<-max_seq
  })
  DP_smw<-reactive({
    req(input$dp_BPs)

    # savereac()
    smw<- vals$smw_dp
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    res<-suppressWarnings(
      suppressMessages(
        extract(smw,w=dp_w,
                index=input$dp_index,
                sig=input$dp_sig,
                z=input$dp_z,
                BPs=dp_BPs,
                seq.sig=input$dp_seq.sig)
      )
    )
    vals$dp_smw<-res
    vals$dp_smw
  })

  getDP<-reactive({
    req(length(input$dp_BPs)>0)
    req(length(input$dp_w)>0)
    smw<- vals$smw_dp
    dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
    dp_w<-if(is.na(input$dp_w)){NULL} else{input$dp_w}
    dp<-DP_smw()
    colnames(dp)[2]<-"SampleID"

    sim1o<-getord()
    yo<-data.frame(sim1o$yo)
    bps<-suppressWarnings(bp(dp))

    to_split<-c(1,rep(1:(length(bps)+1),
                      diff(c(1,bps, nrow(yo)))))
    splits<-lapply(split(yo,to_split),function(x) rownames(x))
    data_empty<-data.frame(attr(vals$saved_data[[input$segrda_X]],"factors")[,1,drop=F])
    data_empty[,1]<-as.numeric(data_empty[,1])
    for(i in 1:length(splits)){
      data_empty[splits[[i]],1]<-i
    }


    vals$splitBP<- data_empty
    dp
  })
  getord<-reactive({
    req(input$segrda_Y)
    req(input$segrda_X)
    req(input$axis_ord_segrda)
    x<-as.matrix(vals$saved_data[[input$segrda_X]])
    colnames(x)<-colnames(vals$saved_data[[input$segrda_X]])
    y<-na.omit(as.matrix(vals$saved_data[[input$segrda_Y]][rownames(x),,drop=F]))
    colnames(y)<-colnames(vals$saved_data[[input$segrda_Y]])
    x<-na.omit(x[rownames(y),])
    if(isTRUE(input$segrda_ord)){
      sim1o<-OrdData(x=y,y=x, axis=input$axis_ord_segrda,scale=input$segrda_scale)} else{
        sim1o<-list()
        sim1o$xo<-y
        sim1o$yo<-x
      }
    sim1o

  })


  save_bpfac<-reactive({
    vals$bagbp0<-vals$bagbp0+1
    dp<-getDP()
    newfac<-vals$splitBP
    factors<-attr(vals$saved_data[[input$segrda_X]],"factors")

    if(input$hand_save=="create") {
      attr(vals$saved_data[[input$segrda_X]],"factors")[rownames(vals$splitBP),input$newdatalist]<-as.factor(vals$splitBP[,1])
    } else{
      attr(vals$saved_data[[input$segrda_X]],"factors")[rownames(vals$splitBP),input$over_datalist]<-as.factor(vals$splitBP[,1])
    }

    vals$bag_smw<-NULL

  })


  getBP<-reactive({
    pw_in<-get_breaks_from_factor()
    breaks=pw_in$breaks
    breaks
  })
  segrda_summary<-reactive({
    req(input$disegrda_sp_summ)
    res<-summary(vals$segrda_model$rda.pw)
    res<-switch(input$disegrda_sp_summ,
                "Summary stats"= vals$segrda_model$summ,
                "Variable scores"=res$species,
                "Observation scores"=res$sites,
                "Linear constraints"=res$constraints,
                "Biplot"=res$biplot,

                "Importance (unconstrained)"=res$cont$importance,
                "Importance (constrained)"=res$concont$importance

    )
    vals$segrda_summary<-res[,1:input$segrda_axes]
    vals$segrda_summary

  })


  observeEvent(ignoreInit = T,input$segrda_panels,{
    vals$segrda_panels<-input$segrda_panels
  })



  observeEvent(ignoreInit = T,input$dp_index_help,{
    showModal(
      modalDialog(
        column(12,
               p(strong("dp:"),span("The dissimilarity profile (DP) table containing significant discontinuities and suggested breakpoints")),
               p(strong("rdp:"),span("data frame containing the randomized DP;")),
               p(strong("md:"),span("mean dissimilarity of the randomized DP")),
               p(strong("sd:"),span("standard deviation for each sample position")),
               p(strong("ospan:"),span("overall expected mean dissimilarity;")),
               p(strong("osd:"),span("average standard deviation for the dissimilarities;")),
               p(strong("params:"),span("list with input arguments"))
        ),
        easyClose = T,
        size="m",
        title="index for extracting SMW results"
      )
    )

  })
  observeEvent(ignoreInit = T,input$dp_sig_help,{
    showModal(
      modalDialog(
        column(12,
               p(strong("z:"),span("consider normalized dissimilarity (z-scores) discontinuities that exceed a z critical value")),
               p(strong("sd:"),span("consider dissimilarity discontinuities that exceed mean plus one standard deviation")),
               p(strong("sd2:"),span("consider dissimilarity discontinuities that exceed mean plus two standard deviation")),
               p(strong("tail1:"),span("Consider dissimilarity discontinuities that exceed 95 percent confidence limits"))

        ),
        easyClose = T,
        size="m",
        title="Significance test fort the SMW results"
      )
    )

  })
  observeEvent(ignoreInit = T,input$inc_bp,{    vals$bag_user_bp<-c(vals$bag_user_bp,input$pw_user)
  })
  observeEvent(ignoreInit = T,input$remove_breaks,{    vals$bag_user_bp<-NULL
  })
  observeEvent(ignoreInit = T,input$downcenter_segrda,{
    vals$hand_down<-"segRDA"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  observeEvent(ignoreInit = T,input$go_smw,{
    vals$window_pool<-getpool()
    updateTabsetPanel(session,"smw_panels",selected='swm_2')
    vals$bag_smw<-T
    bag_smw$df<-T

    req(length(vals$window_pool)>0)
    sim1o<-getord()

    xo<-sim1o$xo ## ordered explanatory matrix.
    yo<-sim1o$yo ## ordered community matrix (untransformed)
    y=yo;ws=vals$window_pool; dist=input$smw_dist;rand=input$smw_rand;n.rand=input$smw_nrand
    if (n.rand < 2) {
      stop("number of randomizations not alowed")
    }
    if (any(ws%%2 == 1)) {
      stop("all Window sizes must be enven")
    }
    rand<-match.arg(rand, c("shift", "plot"))
    argg<-c(as.list(environment()), list())
    smw<-list()
    session=getDefaultReactiveDomain()
    #session=MockShinySession$new()


    withProgress(message = paste0("SMW analysis (", 1, "/", length(ws), "); w =",
                                  ws[1]),session=session,
                 min = 1,
                 max = length(ws),
                 {
                   for (j in 1:length(ws)) {
                     w1<-ws[j]
                     DPtable<-smw.root2(yo, w1, dist)
                     OB<-DPtable[, 3]
                     rdp<-data.frame(rep(NA, length(OB)))
                     seq_yo<-1:nrow(yo)
                     withProgress(message="randomizing",min = 1,session=session,
                                  max = n.rand,{
                                    for (b in 1:n.rand) {
                                      if (rand == "shift") {

                                        comm.rand<-apply(yo, 2, function(sp) sp[sample(seq_yo)])
                                        rdp[b]<-smw.root2(data.frame(comm.rand),
                                                          w1, dist)[3]

                                      } else if (rand == "plot") {

                                        comm.rand<-t(apply(yo, 1, function(sp) sp[sample(seq_yo)]))
                                        rdp[b]<-smw.root2(data.frame(comm.rand),
                                                          w1, dist)[3]
                                      }

                                      incProgress(1,session=session)
                                    }
                                  })

                     rownames(rdp)<-DPtable[,1]
                     Dmean<-apply(rdp, 1, mean)
                     SD<-apply(rdp, 1, sd)
                     oem<-sum(Dmean)/(nrow(yo) - w1)
                     osd<-sum(SD)/(nrow(yo) - w1)
                     Dz<-(OB - oem)/osd
                     DPtable$zscore<-Dz
                     smw[[j]]<-list(dp = data.frame(DPtable), rdp = matrix(rdp),
                                    md = Dmean, sd = SD, oem = oem, osd = osd, params = argg)
                     class(smw[[j]])<-c("smw")
                     incProgress(1, message=paste0("SMW analysis (", j+1, "/", length(ws), "); w =",ws[j+1]),session=session)
                   }
                 })


    names(smw)<-paste("w", ws, sep = "")
    class(smw)<-c("smw")
    vals$smw_dp<-isolate(smw)


  })

  observeEvent(vals$smw_dp,{
    getDP()
  })



  observeEvent(ignoreInit = T,input$downcenter_dp_smw,{
    vals$hand_down<-"DP smw"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$tools_saveBP,{
    vals$hand_save<-"Create factor using breakpoints from the dissimilarity profile"
    vals$hand_save2<-div(span("Target:",em(input$segrda_X,style="color: SeaGreen")))
    vals$hand_save3<-NULL
    showModal(module_desctools())
  })
  savenames<-reactive({
    switch(
      vals$hand_save,
      "Create factor using breakpoints from the dissimilarity profile"= {c(paste0("BP_"),nlevels(as.factor(vals$splitBP[,1])))},
      "Save pwRDA model in"={
        name_models<-names(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))
        newname<-paste0(input$segrda_X,"~",input$segrda_Y,"[factor:",input$bp_column,"]")
        name_models<-make.unique(c(name_models,newname))
        name_models<-name_models[length(name_models)]
        name_models
      })
  })

  observeEvent( input$data_confirm,{
    req(!is.null(vals$hand_save))
    switch(
      vals$hand_save,
      "Create factor using breakpoints from the dissimilarity profile"= {save_bpfac()},
      "Save pwRDA model in"={
        save_pwRDA()
      })
    removeModal()

  })
  save_pwRDA<-reactive({
    attr(vals$saved_data[[input$segrda_X]],"pwRDA")[[input$newdatalist]]<-vals$segrda_model
    attr(vals$saved_data[[input$segrda_X]],"pwRDA")[["pwRDA (unsaved)"]]<-NULL
    updatePickerInput(session,'pwRDA_models', selected=input$newdatalist)
    vals$bag_pw<-NULL
  })
  observeEvent(ignoreInit = T,input$downp_we,{
    vals$hand_plot<-"we"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_dp,{

    vals$hand_plot<-"dp"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })

  observe({
    req(input$pwRDA_models)
    names(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))
    vals$segrda_model<-attr(vals$saved_data[[input$segrda_X]],"pwRDA")[[input$pwRDA_models]]
  })



  observeEvent(ignoreInit = T,input$run_pwRDA,{
    updatePickerInput(session,'pwRDA_models',selected="pwRDA (unsaved)")
    vals$bag_pw<-T
    pw_in<-get_breaks_from_factor()
    breaks=pw_in$breaks
    sim1o<-list()
    sim1o$xo<-pw_in$yord
    sim1o$yo<-pw_in$xord

    model=suppressMessages(
      try(
        pwRDA2(sim1o$xo,sim1o$yo , BPs=breaks,n.rand = input$pw_nrand)
      )
    )
    if(is.null(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))){
      attr(vals$saved_data[[input$segrda_X]],"pwRDA")<-list()
    }
    attr(vals$saved_data[[input$segrda_X]],"pwRDA")[["pwRDA (unsaved)"]]<-NULL
    attr(vals$saved_data[[input$segrda_X]],"pwRDA")[["pwRDA (unsaved)"]]<-model
  })

  savereac<-reactive({
    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img


    saveRDS(tosave,"savepoint.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()
  })










  #####
  ## RDA
  ##
  ##


  output$rda_head<-renderUI({
    validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
    div(class="well3",
      inline(div(class="map_control_style2",
                 style="color: #05668D",
                 inline(uiOutput(ns("rda_Y"))),
                 inline(uiOutput(ns("rda_X"))),
                 inline(checkboxInput(ns("rda_scale"),span("+ Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T, width="250px")),


      )),inline( uiOutput(ns('rda_btn')))
    )
  })
  output$rda_X<-renderUI({
    pickerInput(ns("rda_Y"),span("~ X Data", tiphelp("Predictors")), choices=names(vals$saved_data), selected=vals$cur_rda_Y)
  })
  output$rda_Y<-renderUI({
    req(input$rda_Y)
    pickerInput(ns("rda_X"),span("Y Data", tiphelp("Response data")), choices=names(vals$saved_data), selected=vals$cur_rda_X)
  })



  observeEvent(ignoreInit = T,input$disp_rda_summ,
               vals$disp_rda_summ<-input$disp_rda_summ)
  output$rda_view_summary<-renderUI({
    div(
      div(
        span(
          "+ Results:",
          inline(
            pickerInput(ns("disp_rda_summ"),NULL,choices=c('Importance (unconstrained)',"Importance (constrained)",'Variable scores','Observation scores','Linear constraints','Biplot'), selected=vals$disp_rda_summ,  options=list(container="body"), width="200px")
          )
        )
      ),
      div(
        span("+ Axes",
             inline(
               numericInput(ns("rda_axes"),NULL, value=2)
             )
        )
      ),
      div(
        tipify(
          actionLink(
            ns('downcenter_rda'),span("+ Download",icon("fas fa-download")), style="button_active"
          ),
          "Download selected results"
        )

      )
    )

  })

  observeEvent(input$rda_X,{
    ncol<-ncol(vals$saved_data[[input$rda_X]])
    value=10
    if(value>ncol){
      vals$rda_spnum<-ncol
    }
  })

  color_input<-function(id,label,selected=NULL,vals){
    pickerInput(
      inputId = id,
      label = label,
      choices =     vals$colors_img$val,
      choicesOpt = list(content =vals$colors_img$img),
      selected=selected,
      options=list(container="body")
    )

  }








  output$rda_print<-renderPrint({rda_summary()})


  observeEvent(ignoreInit = T,input$rda_view,
               vals$rda_view<-input$rda_view)




  rda_summary<-reactive({
    req(input$disp_rda_summ)
    res<-summary(rda_model())
    res<-switch(input$disp_rda_summ,
                "Variable scores"=res$species,
                "Observation scores"=res$sites,
                "Linear constraints"=res$constraints,
                "Biplot"=res$biplot,

                "Importance (unconstrained)"=res$cont$importance,
                "Importance (constrained)"=res$concont$importance

    )
    vals$rda_summary<-res[,1:input$rda_axes]
    vals$rda_summary

  })


  observeEvent(ignoreInit = T,input$rda_downp,{
    vals$hand_plot<-"rda"
    module_ui_figs("downfigs")
    mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$rda_X,
               vals$cur_rda_X<-input$rda_X)
  observeEvent(ignoreInit = T,input$rda_Y,
               vals$cur_rda_Y<-input$rda_Y)


  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      "Create Datalist: Niche results"={name_niche()},
      "Create factor using breakpoints from the dissimilarity profile"= {c(paste0("BP_"),nlevels(as.factor(vals$splitBP[,1])))},
      "Save pwRDA model in"={bag_pwrda()}
    )})



  bag_pwrda<-reactive({
    name_models<-names(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))
    newname<-paste0(input$segrda_X,"~",input$segrda_Y,"[factor:",input$bp_column,"]")
    name_models<-make.unique(c(name_models,newname))
    name_models<-name_models[length(name_models)]
    name_models

  })

  output$data_over<-renderUI({
    data_overwritte$df<-F
    data<-vals$saved_data[[input$data_descX]]
    choices<-c(names(vals$saved_data))
    if(vals$hand_save=="Create factor using breakpoints from the dissimilarity profile"){choices<-colnames(attr(data,"factors"))}
    req(input$hand_save=="over")
    res<-pickerInput(ns("over_datalist"), NULL,choices, width="350px")
    data_overwritte$df<-T
    inline(res)
  })
  output$data_create<-renderUI({
    req(newname$df!=0)
    data_store$df<-F
    req(input$hand_save=="create")
    res<-textInput(ns("newdatalist"), NULL, newname$df, width="350px")
    data_store$df<-T
    inline(res)
  })
  observeEvent( input$data_confirm,{
    req(!is.null(vals$hand_save))
    switch(
      vals$hand_save,
      "Create Datalist: Niche results"={saveniche()},
      "Create factor using breakpoints from the dissimilarity profile"= {save_bpfac()},
      "Save pwRDA model in"={save_pwRDA()}

    )
    removeModal()

  })

  module_desctools <- function() {
    ns <- session$ns
    modalDialog(
      uiOutput(ns("databank_storage")),
      title=strong(icon("fas fa-save"),'Save'),
      footer=column(12,
                    fluidRow(modalButton(strong("cancel")),
                             inline(uiOutput(ns("save_confirm")))
                    )
      ),

      easyClose = T
    )

  }
  output$databank_storage<-renderUI({
    req(!is.null(vals$hand_save))
    newname$df<-0
    get_newname()
    div(
      column(12,
             div(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")),
             div(vals$hand_save2,style="color: gray"),
             div(vals$hand_save3)),
      column(12,style="margin-top: 20px",
             radioButtons(ns("hand_save"),NULL,
                          choiceNames= list(div(style="height: 40px",span("Create", style="margin-right: 15px"), inline(uiOutput(ns("data_create")))),
                                            div(style="height: 40px",span("Overwrite", style="margin-right: 15px"), inline(uiOutput(ns("data_over"))))),
                          choiceValues=list('create',"over"), width="800px")
      )


    )
  })
  output$save_confirm<-renderUI({
    req(isTRUE(data_store$df)|isTRUE(data_overwritte$df))
    actionButton(ns("data_confirm"),strong("confirm"))
  })


}
