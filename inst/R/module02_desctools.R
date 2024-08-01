
#' @importFrom colorspace lighten
#' @importFrom data.table rleid
#' @importFrom segRDA OrdData bp
#' @noRd

color_input<-function(id,label,selected=NULL,vals){
  pickerInput_fromtop(
    inputId = id,
    label = label,
    choices =     vals$colors_img$val,
    choicesOpt = list(content =vals$colors_img$img),
    selected=selected
  )

}


desctools_tab1<-list()
desctools_tab1$ui<-function(id){
  ns<-NS(id)
  div(
    column(class="side_results",
           12, offset = 0,
           tabsetPanel(
             id=ns("summ_options"),
             tabPanel(
               value="Datalist",
               title = "1.1. Datalist",
               uiOutput(ns('render_datalist'))),
             tabPanel(
               value="Variables",
               title = "1.2. Numeric-Attribute",
               uiOutput(ns("stats_data")),

               column(4,class="mp0",style="max-height: 400px; overflow-y: auto",
                      box_caret(
                        ns("box_tab1_2_a"),
                        color="#c3cc74ff",
                        title="Select the Variables",
                        div(DT::dataTableOutput(ns('histo_var_x')))
                      ),
                      box_caret(
                        ns("box_tab1_2_b"),
                        color="#c3cc74ff",
                        title="Metrics",
                        div(
                          checkboxGroupInput(ns("varhisto_metric"),NULL,c(
                            'Min.' ,'1st Qu.',"Mean",'Median','3rd Qu.','Max.'
                          ), selected=c('Min.' ,"Mean",'Max.'),inline = F),
                          numericInput(ns("varhisto_ps_round"),"Round:", value=2, step=010)
                        )
                      ),
                      box_caret(
                        ns("box_tab1_2_c"),
                        color="#c3cc74ff",
                        title="Plot params",
                        div(

                          colourpicker::colourInput(inputId=ns("varhisto_palette"),label = "Background:",showColour ="background",allowTransparent =T, value="black"),
                          colourpicker::colourInput(inputId=ns("varhisto_border_col"),label = "Border",showColour ="background",allowTransparent =T, value="black"),
                          numericInput(ns("cextext"),"Text size:", value= 2, step=1),

                          numericInput(ns("varhisto_w1"),"Var width", value=  0.2, step=0.05),
                          numericInput(ns("varhisto_w2"),"Metric width", value= 0.35, step=0.05),
                          numericInput(ns("varhisto_w3"),"Histo width", value= 0.35, step=0.05)


                        )
                      ),
                      box_caret(
                        ns("box_tab1_2_d"),
                        color="#c3cc74ff",
                        title="Plot size",
                        div(
                          numericInput(ns("varhisto_ps_width"),"Width", value=  550, step=10),
                          numericInput(ns("varhisto_ps_height"),"Height", value=  400, step=10)
                        )
                      )

               ),
               column(8,class="mp0",
                      box_caret(
                        ns("box_tab1_2_e"),
                        title="Plot",button_title =actionLink(ns('downp_summ_num'),"Download",icon("download")),
                        div(uiOutput(ns("histovar_main")))
                      )
               )


             ),
             tabPanel(
               value="3. Factor-Attribute",
               title = "1.3. Factor-Attribute",
               div(
                 column(
                   4,class="mp0",
                   box_caret(
                     ns("box_tab1_3_a"),
                     color="#c3cc74ff",
                     title="Options",
                     div(
                       div(

                         numericInput(ns('pfac_width'),'Bar width:',value=0.6, step=0.05, max=1),
                         numericInput(ns('pfac_base_size'),"Base size:",value=12),
                         div(style="display: flex",
                             checkboxInput(ns('pfac_show_levels'),"Show levels:",value=T, width="130px"),
                             div(colourpicker::colourInput(ns('pfac_col_lev'),"","lightsteelblue","background"),style="width: 80px")
                         ),

                         div(style="display: flex",
                             checkboxInput(ns('pfac_show_obs'),"Show obs/levels:",value=T, width="150px"),
                             div(colourpicker::colourInput(ns('pfac_col_obs'),NULL,"lightcyan","background"), style="width: 80px")
                         ),
                         pickerInput_fromtop(
                           inputId = ns('pfac_border_palette'),
                           label = 'Border:',
                           choices=NULL),
                         pickerInput_fromtop(
                           inputId = ns('pfac_fill_palette'),
                           label = 'Fill:',
                           choices =    NULL),
                         numericInput(ns('pfac_pastel'),"Fill light:",0.4),
                         textInput(ns('pfac_title'),'Title:',value="Observation Totals by Factor and Level"),
                         textInput(ns('pfac_xlab'),"lab:",value="Factors"),
                         textInput(ns('pfac_ylab'),"lab:",value='Number of Observations'),


                       )
                     )
                   )
                 ),
                 column(8,class="mp0",
                        box_caret(
                          ns("box_tab1_2_e"),
                          title="Plot",button_title =actionLink(ns('downp_stats_fac'),"Download",icon("download")),
                          div(uiOutput(ns("factorsplot")))
                        )
                 ),


                 div(
                   h5(strong("Factors:")),
                   h5(strong("Structure:")),
                   verbatimTextOutput(ns('strlabels')))
               )
             )
           )


    )
  )
}
desctools_tab1$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    getdata_descX<-reactive({
      req(vals$desc_data_x)
      vals$desc_data_x
    })
    observeEvent(ignoreInit = T,input$downp_stats_fac,{
      vals$hand_plot<-"Factor plot"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })

    observeEvent(vals$newcolhabs,{
      choices =     vals$colors_img$val
      choicesOpt = list(content =vals$colors_img$img)

      updatePickerInput(session,"pfac_border_palette",choices=choices,choicesOpt=choicesOpt)
      updatePickerInput(session,"pfac_fill_palette",choices=choices,choicesOpt=choicesOpt)
    })

    box_caret_server('box_tab1_2_a')
    box_caret_server('box_tab1_2_b')
    box_caret_server('box_tab1_2_c')
    box_caret_server('box_tab1_2_d')
    box_caret_server('box_tab1_2_e')


    output$render_datalist<-renderUI({

      datalist_render(getdata_descX())
    })
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


    output$histo_var_x = DT::renderDataTable({
      req(is.numeric(vals$desc_maxhistvar))
      data<-getdata_descX()
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



    observeEvent(ignoreInit = T,input$downp_summ_num,{
      vals$hand_plot<-"variable summary"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
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
    output$strlabels<-renderPrint({

      ppsummary("----------------")
      ppsummary(paste("Missing values:",sum(is.na(attr(getdata_descX(),"factors")))))
      ppsummary("----------------")
      str(attr(getdata_descX(),"factors")[rownames(getdata_descX()),,drop=F])
    })




    observeEvent(getdata_descX(),{
      data<-getdata_descX()
      vals$desc_maxhistvar<-ifelse(ncol(data)>10,10,ncol(data))
    })




    observeEvent(ignoreInit = T,input$varhisto_border_col,{
      vals$varhisto_border_col<-input$varhisto_border_col
    })
    output$histovar_main<-renderUI({
      plotOutput(ns('histovar_plot'), height=paste0(input$varhisto_ps_height,"px"), width=paste0(input$varhisto_ps_width,"px"))
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









    observe({
      shinyjs::toggle('pfac_col_obs', condition=isTRUE(input$pfac_show_levels))
      shinyjs::toggle('pfac_col_lev', condition=isTRUE(input$pfac_show_levels))
    })


    observeEvent(ignoreInit = T,input$downp_hist,{
      vals$hand_plot<-"histogram"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })







  })
}
desctools_tab2<-list()
desctools_tab2$ui<-function(id){
  ns<-NS(id)
  div(div(class="model_setup",
          box_caret(ns("box_setup2"),inline=F,
                    color="#374061ff",
                    title="Setup",
                    div(
                      div(style="display: flex;gap: 10px;height: 50px",class="setup_box",
                          div(class="picker-flex picker-before-x",
                              pickerInput_fromtop(ns("bbox_x_datalist"),tipify(span("Datalist:"),"Datalist for selecting the Factor"),choices = NULL)
                          ),

                          div(class="picker-flex",
                              pickerInput_fromtop(ns("boxplot_X"),"Factor",choices =NULL)
                          ),
                          div(class="picker-flex",
                              pickerInput_fromtop(ns("filter_box1"),"Filter:",choices = NULL)),
                          div(class="picker-flex",
                              pickerInput_fromtop(ns("filter_box2"),
                                          "Class:",choices=NULL))
                      ),
                      div(style="display: flex;gap: 10px;height: 50px",class="setup_box picker-flex",
                          div(class="setup_box picker-flex picker-before-y", pickerInput_fromtop(ns("bbox_y_datalist"),tipify(
                            span(""),
                            "Datalist for selecting the Numeric-Value",
                          ),choices = NULL)
                          ),

                          div(class="picker-flex",
                              pickerInput_fromtop( ns("box_y"),tipify(
                                span("Variable"),
                                "the numeric values to be split into groups according to the grouping variable"),choices = NULL, multiple=T))




                      )

                    )
          )
  ),




  column(4,class='mp0',
         box_caret(ns('box_2a'),auto_overflow=T,
                   title="Plot options",
                   color="#c3cc74ff",
                   div(style="height:  calc(100vh - 230px)",
                       div(id=ns("side_boxplot"),

                           div(
                             div(
                               checkboxInput(ns('box_horiz'),"Horizontal:",value=F),

                               pickerInput_fromtop(ns("box_theme"),"Theme:",c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic')),

                               pickerInput_fromtop(ns("box_palette"),
                                           label = "Palette:",
                                           choices=NULL),
                               numericInput(ns('box_alpha'),"Lighten:", .3, step=0.05),
                               colourpicker::colourInput(ns("box_linecol"),label = "Line color:",value ="black",showColour="background"),
                               numericInput(ns('box_linewidth'),"Line width:", .5,step=.1),

                               numericInput(ns('box_base_size'),"Base size:", 12,step=1),
                               numericInput(ns('box_cex.main'),"Title size:", 1.5,step=.1),
                               numericInput(ns('box_cex.label_panel'),"Panel Title Size:", 1.5,step=.1),
                               selectInput(ns("box_title_font"),"Title font",c("italic","bold","plain")),

                               numericInput(ns('box_cex.lab'),"Label size:", 1.5,step=.1),
                               numericInput(ns('box_cex.axes'),"Axis size:", 1.5,step=.1),
                               textInput(ns('box_title'),"Title:",NULL),

                               numericInput(ns('box_xlab_rotate'),"x text angle:", 0,step=5),
                               numericInput(ns('box_ylab_rotate'),"y text angle:", 0,step=5),

                               textInput(ns('box_xlab'),"x label:",NULL),
                               textInput(ns('box_ylab'),"y label:",NULL),
                               checkboxInput(ns('box_grid'),"Grid lines:",value=T),

                               checkboxInput(ns("box_varwidth"),span("Varwidth:",tiphelp("Drawn boxes with widths proportional to the square-roots of the number of observations in the groups","right")),F),
                               checkboxInput(ns('box_violin'),"Violin:",value=F),

                               numericInput(ns('box_width'),"Plot widht:",550, step=50),
                               numericInput(ns('box_heigth'),"Plot heigth:",400, step=50),
                               numericInput(ns('box_ncol'),"N columns",2),
                               numericInput(ns('box_nrow'),"N rows",NA),)
                           ))
                   )

         )

  ),
  column(8,class="mp0",
         box_caret(
           ns("box_2b"),
           title="Plot",

           button_title = actionLink(
             ns("downp_box"),span("Download",icon("fas fa-download"))),
           div(uiOutput(ns("box_btn")),
               uiOutput(ns("stats_pbox")))
         )
  )

  )
}
desctools_tab2$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_setup2")
    box_caret_server("box_2a")
    box_caret_server("box_2b")
    ns<-session$ns
    bbox_x_datalist<-reactive({
      req(input$bbox_x_datalist)
      vals$saved_data[[input$bbox_x_datalist]]

    })

    getbox<-reactive({

    })
    getbox<-reactive({
      req(length(vals$saved_data)>0)

      req(input$boxplot_X)
      req(input$box_y)
      req(input$filter_box1)

      result<-try(silent=T,{

        datax<-bbox_x_datalist()
        labels<-factors<-attr(datax,"factors")

        data<-datay<-bbox_y_datalist()[rownames(datax),,drop=F]

        pic<-1:nrow(data)
        req(any(input$boxplot_X%in%colnames(labels)))
        x<-labels[input$boxplot_X]
        req(any(input$box_y%in%colnames(datay)))
        y<-data[input$box_y]
        common_ids<-intersect(rownames(x),rownames(y))
        req(length(common_ids)>0)
        x<-x[common_ids,,drop=F]
        y<-y[common_ids,,drop=F]

        if (input$filter_box1 != "none") {
          filtro<-as.character(input$filter_box1)
          filtro2<-as.character(input$filter_box2)
          pic<-which(as.character(labels[, filtro]) == filtro2)


        }
        res = data.frame(x,y)[pic,,drop=F]
        colnames(res)[-1]<-colnames(y)
        res[,1]<-res[,1]
        res
        # saveRDS(res,"res.rds")
        #re<-readRDS('res.rds')
        # levels(re$Consenso)
        #vals<-readRDS("savepoint.rds")
        # levels(attr(vals$saved_data[["SANTOS_C1"]],"factors")$Consenso)

      })
      req(!inherits(result,"try-error"))
      result

      })
    bbox_y_datalist<-reactive({
      req(input$bbox_y_datalist)
      req(input$bbox_y_datalist%in%names(vals$saved_data))
      res<-vals$saved_data[[input$bbox_y_datalist]]
      req(res)

    })

    observeEvent(ignoreInit = T,input$downp_box,{
      vals$hand_plot<-'boxplot'
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })

    output$stats_pbox<-renderUI({
      req(input$box_width)
      req(input$box_width>10)
      req(input$box_heigth)
      req(input$box_heigth>10)

      #args<-readRDS("args.rds")
      div(renderPlot({

        vals$pbox_plot
      },
      width =input$box_width,
      height =input$box_heigth
      ))
    })
    get_samerows_datalist<-function(current,list_data){
      cid<-rownames(current)
      lid<-lapply(list_data,rownames)
      pic<-sapply(lid,function(x) sum(x%in%cid))==length(cid)
      names(list_data[pic])
    }

    observeEvent(bbox_x_datalist(),{
      datax<-attr(bbox_x_datalist(),"factors")
      choices<-get_samerows_datalist(datax,vals$saved_data)
      updatePickerInput(session,'bbox_y_datalist',choices=choices)
    })
    observeEvent(bbox_x_datalist(),{
      data<-bbox_x_datalist()
      factors<-attr(data,"factors")
      choices = c("none", colnames(factors))
      updatePickerInput(session,'filter_box1',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(bbox_x_datalist(),{
      choices=rev(colnames(attr(bbox_x_datalist(),"factors")))
      updatePickerInput(session,'boxplot_X',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(bbox_y_datalist(),{
      data<-bbox_y_datalist()
      choices = colnames(data)
      updatePickerInput(session,'box_y',choices=choices,selected= colnames(data)[1],options=shinyWidgets::pickerOptions(liveSearch=T))
    })

    observeEvent(input$filter_box1,{
      req(input$filter_box1 != "none")
      data<-bbox_x_datalist()
      factors<-attr(data,"factors")
      labels<-factors[rownames(data), input$filter_box1]
      choices = c(levels(as.factor(labels)))
      updatePickerInput(session,'filter_box2',choices=choices)
    })
    observeEvent(vals$saved_data,{
      updatePickerInput(session,'bbox_x_datalist',choices=names(vals$saved_data))
    })

    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'box_palette', choices = vals$colors_img$val,
                        choicesOpt = list(content = vals$colors_img$img),)
    })
    observeEvent(list(input$boxplot_X,input$box_y),{
      req(input$boxplot_X)
      req(input$box_y)
      value=ifelse(length(input$box_y)>1,"",paste(input$box_y,"~",input$boxplot_X))
      updateTextInput(session,'box_title',value=value)
    })
    observeEvent(list(input$boxplot_X,input$box_y),{
      req(input$boxplot_X)
      req(input$box_y)
      value=ifelse(length(input$box_y)>1,"",input$boxplot_X)
      updateTextInput(session,'box_xlab',value=value)
    })
    observeEvent(input$box_y,{

      req(input$box_y)
      value=ifelse(length(input$box_y)>1,"Value",input$box_y)
      updateTextInput(session,'box_ylab',value=value)
    })
    observeEvent(input$box_reset,{
      shinyjs::reset("side_boxplot")
    })
    observeEvent(input$run_boxplot,{
      args<-args_boxplot()
      vals$pbox_plot<-do.call(ggbox,args)
      runval$box<-"btn_nice"
    })

    output$box_btn<-renderUI({
      div(class=runval$box,actionButton(ns("run_boxplot"),"RUN",icon=icon("fas fa-sync")))
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
        newcolhabs=vals$newcolhabs,
        nrow=input$box_nrow,
        ncol=input$box_ncol,
        box_title_font=input$box_title_font
      )
      #saveRDS(args,"args_box.rds")
      args
    })
    observeEvent(args_boxplot(),{
      req(!is.null(vals$pbox_plot))
      runval$box<-"save_changes_nice"
    })

    runval<-reactiveValues(
      box="save_changes_nice"
    )
    observe(shinyjs::toggle("filter_box2",condition = input$filter_box1!="none"))
  })
}

desctools_tab3<-list()
desctools_tab3$ui<-function(id){
  ns<-NS(id)
  div(
    column(4,class="mp0",  style="height:  calc(100vh - 200px);overflow: auto",
           box_caret(ns("box_3a"),
                     title="Setup",
                     color="#c3cc74ff",
                     div(

                       div(
                         div(
                           pickerInput_fromtop(inputId=ns("rid_y"),label = "X (factor)",
                                       choices =NULL),
                           div(actionLink(ns("show_obs_selection"),"Y (numeric)")),
                           div(uiOutput(ns('rid_x_variables')),)
                         ),



                       )
                     )),
           box_caret(ns("box_3b"),
                     title="Plot Options",
                     color="#c3cc74ff",
                     div(
                       pickerInput_fromtop(inputId=ns("rid_col"),label = "Palette:",NULL),
                       textInput(ns('rid_tittle'),"Title",""),
                       numericInput(ns('rid_base_size'),"Base size",11),
                       numericInput(ns('rid_ncol'),"N columns",3),
                       numericInput(ns('rid_width'),"Plot widht",700),
                       numericInput(ns('rid_heigth'),"Plot heigth",300),




                     )
           )

    ),
    column(8,box_caret(
      ns('box_3c'),
      title="Ridges plot",

      button_title =
        actionLink(ns("rid_downp"),"Download",icon("download")),
      div(uiOutput(ns('run_ridges_btn')),
          uiOutput(ns('rid_out')))
    ))
  )
}
desctools_tab3$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_3a")
    box_caret_server("box_3b")
    box_caret_server('box_3c')
    ns<-session$ns
    getdata_descX<-reactive({
      req(vals$desc_data_x)
      vals$desc_data_x
    })
    runval<-reactiveValues(
      rid="save_changes_nice"
    )
    output$run_ridges_btn<-renderUser({
      div(class=runval$rid,
          actionButton(ns('run_ridges'), 'RUN', icon=icon("fas fa-sync")),
      )
    })

    output$rid_x_variables<-renderUI({
      div(
        fluidPage(

          DT::dataTableOutput(ns('rid_x'))


        )
      )

    })

    output$rid_out<-renderUI({
      req(!is.null(vals$rid_plot))
      width=paste0(input$rid_width,"px")
      height=paste0(input$rid_heigth,"px")
      renderPlot(vals$rid_plot, width=input$rid_width,height=input$rid_heigth)

    })
    output$rid_x = DT::renderDataTable(
      {      data<-getdata_descX()
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

    observeEvent(args_ridges(),{
      req(input$rid_heigth)
      req(input$run_ridges)
      runval$rid<-"save_changes_nice"
    })

    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'rid_col',choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img))
    })
    observe({
     # print(input$rid_x_rows_selected)
    })

    args_ridges<-reactive({
      req(input$rid_heigth)
      req(input$rid_y)

      result<-try({

        data=getdata_descX()
        factors<-attr(data,"factors")
        #input<-readRDS("input.rds")
        #vals<-readRDS('vals.rds')
        #saveRDS(reactiveValuesToList(input),"input.rds")
        #saveRDS(reactiveValuesToList(vals),"vals.rds")
        req(length(input$rid_x_rows_selected)>0)
        # req(input$rid_x_rows_selected%in%colnames(data))
        x<-data<-data[,input$rid_x_rows_selected, drop=F]
        req(input$rid_y%in%colnames(factors))
        y<-factors[input$rid_y]

        common_ids<-intersect(rownames(x),rownames(y))
        req(length(common_ids)>0)
        data<-data[common_ids,,drop=F]
        fac<-factors[common_ids,input$rid_y]
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
      req(!inherits(result,"try-error"))
      result
      })

    observeEvent(getdata_descX(),{
      data<-getdata_descX()
      factors<-attr(data,"factors")
      choices =rev(colnames(factors))
      updatePickerInput(session,'rid_y',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(ignoreInit = T,input$show_obs_selection,{
      shinyjs::toggle("rid_x")
    })


    observeEvent(input$run_ridges,{
      args<-args_ridges()
      vals$rid_plot<-do.call(plot_ridges,args)
      runval$rid<-"btn_nice"
    })
    observeEvent(ignoreInit = T,input$rid_downp,{
      vals$hand_plot<-"Ridge plot"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals)})

  })
}

desctools_tab4<-list()
desctools_tab4$ui<-function(id){
  ns<-NS(id)
  div(
    column(4,class="mp0",
           style="height:  calc(100vh - 200px);overflow: auto",

           box_caret(ns("box_4a"),

                     title=strong("Variables & Panels"),
                     color="#c3cc74ff",
                     div(

                       pickerInput_fromtop(ns("ggpair.variables"),span("Variables:",class='text_alert'),NULL, multiple = T,options=list(`actions-box` = TRUE)),



                       pickerInput_fromtop(ns("ggpair.upper"), "Upper panel",
                                   choices = list(
                                     "Correlation" = "corr",
                                     "Corr + group" = "corr+group",
                                     "none" = "blank"
                                   )),
                       pickerInput_fromtop(ns("ggpair.lower"), "Lower panel",
                                   choices = list(
                                     "Points" = "points",
                                     "Points + group" = "points+group",
                                     "none" = "blank"
                                   )),
                       pickerInput_fromtop(ns("ggpair.diag"), "Diagonal panel",
                                   choices = list(
                                     "Density" = "density",
                                     "Density + group" = "density+group",
                                     "Hist" = "hist",
                                     "Hist+group" = "hist+group",
                                     "none" = "blank"
                                   )),
                       checkboxInput(ns("ggpair.box.include"), "Y boxplot",F),
                       div(
                         style="border-bottom: 1px solid; margin-bottom: 5px; margin-left: 20px",
                         pickerInput_fromtop(ns("ggpair.y.variable"),strong("Grouping Factor:", class='text_alert'),NULL)
                       ),
                       pickerInput_fromtop(ns("ggpair.method"), "Correlation method:", c("pearson", "kendall", "spearman", "none"))
                     )),
           box_caret(ns("box_4b"),
                     title = "General Options",
                     color="#c3cc74ff",
                     div(

                       pickerInput_fromtop(inputId = ns("fm_palette"),
                                   label = '+ Palette',
                                   choices=NULL),
                       numericInput(ns("msp_plot_width"), "Plot width",550),
                       numericInput(ns("msp_plot_height"), "Plot height",400),
                       numericInput(ns("msp_plot_base_size"),"Base size",12),

                       numericInput(ns("ggpair.round"), "Digits:", 3),
                       pickerInput_fromtop(ns("ggpair.switch"), "Switch:", list("default" = NULL, "x" = "x", "y" = "y", "both" = "both")),
                       numericInput(ns("ggpair.varnames.size"), "Variable name size:", 1),
                       numericInput(ns("ggpair.cor.size"), "Corr size:", 2),
                       pickerInput_fromtop(inputId = ns("ggpair.pch"),
                                   label = "Point shape",NULL),
                       numericInput(ns("ggpair.points.size"), "Points size", 1),
                       numericInput(ns("ggpair.legend.text.size"), "legend.text.size:", 1),
                       numericInput(ns("ggpair.legend.title.size"), "legend.title.size:", 1),
                       numericInput(ns("ggpair.alpha.curve"), "Curve transparency:", 0.8)
                     )
           ),

           box_caret(ns("box_4c"),
                     title="Title, Axes and Labels",
                     color="#c3cc74ff",
                     div(
                       textInput(ns("ggpair.title"), "Title:", ""),
                       numericInput(ns("ggpair.plot.title.size"), "Title size:", 1),
                       textInput(ns("ggpair.xlab"), "xlab:", ""),
                       textInput(ns("ggpair.ylab"), "ylab:", ""),
                       numericInput(ns("ggpair.axis.text.size"), "Axis tick size:", 1),
                       numericInput(ns("ggpair.axis.title.size"), "Axis label size:", 1),
                       textInput(ns("ggpair.title_corr"), "Title corr:", "")


                     ))
    ),
    column(8,class="mp0",
           box_caret(ns("box_4d"),
                     button_title = actionLink(
                       ns("fm_downplot4"), span("Download plot", icon("fas fa-download"), icon("fas fa-image")), style = "button_active"
                     ),
                     title="Pair plot",

                     div(
                       uiOutput(ns("pair_btn")),
                       withSpinner(uiOutput(ns("msp_pairs")),8)
                     )
           ))
  )
}
desctools_tab4$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    box_caret_server("box_4a")
    box_caret_server("box_4b")
    box_caret_server("box_4c")
    box_caret_server("box_4d")
    runval<-reactiveValues(pair="save_changes_nice")

    output$pair_btn<-renderUI({
      div(class=runval$pair,actionButton(ns("run_pair"),"RUN", icon=icon("fas fa-sync")))

    })

    observeEvent(get_ggpair(),ignoreInit = T,{
      req(input$run_pair)
      runval$pair<-"save_changes_nice"
    })

    observeEvent(ignoreInit = T,input$run_pair,{
      req(get_ggpair())
      vals$desc_pairplot<-get_ggpair()
      runval$pair<-"btn_nice"
    })
    getdata_descX<-reactive({
      req(vals$desc_data_x)
      vals$desc_data_x
    })

    observeEvent(ignoreInit = T,input$fm_downplot4,{
      vals$hand_plot<-"Pairs-plot"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })
    observeEvent(getdata_descX(),{
      data<-getdata_descX()
      req(length(data)>0)

      factors<-attr(data,"factors")
      choices=colnames(factors)
      updatePickerInput(session,'ggpair.y.variable',choices,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observe(shinyjs::toggle("ggpair.y.variable",condition = isTRUE(yclude_y())))

    observeEvent(df_symbol$val,{
      updatePickerInput(session,'ggpair.pch', choices = df_symbol$val,
                        choicesOpt = list(content = df_symbol$img))
    })
    get_ggpair<-eventReactive(input$run_pair,ignoreInit = T,{
      args<-get_ggpair_args()
      class(args)=="iggpair"
      p<-do.call(gg_pairplot2,args)

      p
    })

    observeEvent(input$msp_plot_base_size,{
      bs<-round(input$msp_plot_base_size/12, 2)

      updateNumericInput(session,'ggpair.varnames.size',value=bs*1.4)
      updateNumericInput(session,'ggpair.points.size',value=bs)
      updateNumericInput(session,'ggpair.legend.text.size',value=bs)
      updateNumericInput(session,'ggpair.legend.title.size',value=bs)

      updateNumericInput(session,'ggpair.plot.title.size',value=bs)
      updateNumericInput(session,'ggpair.axis.text.size',value=bs)
      updateNumericInput(session,'ggpair.axis.title.size',value=bs)



    })
    output$msp_pairs<-renderUI({
      req(vals$desc_pairplot)
      req(input$msp_plot_width)
      req(input$msp_plot_height)
      res<-div(

        renderPlot(vals$desc_pairplot,  width=input$msp_plot_width,height=input$msp_plot_height),
        em(attr(vals$desc_pairplot,"row1"), style="color: gray")
      )
      vals$show_ggrun<-F
      res
    })
    output$ggpair.title_corr<-renderUI({
      req(input$ggpair.method)
      textInput(ns("ggpair.title_corr"),"title_corr:",paste(input$ggpair.method,"corr"))
    })
    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'fm_palette', choices =     vals$colors_img$val,choicesOpt = list(content =vals$colors_img$img),)
    })
    observe({
      shinyjs::toggle("ggpair.method",condition=input$ggpair.upper!='blank')
    })
    yclude_y<-reactive({
      req(input$ggpair.lower)
      req(input$ggpair.upper)
      req(input$ggpair.diag)
      req(length(input$ggpair.box.include)>0)
      input$ggpair.lower=='points+group'|input$ggpair.upper=='corr+group'|input$ggpair.diag%in%c("density+group","hist+group")| isTRUE( input$ggpair.box.include)
    })
    observeEvent(getdata_descX(),{
      choices=colnames(getdata_descX())
      updatePickerInput(session,'ggpair.variables',choices=choices,selected=choices[1:3],options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    get_ggpair_args<-reactive({
      args<-try(silent = T,{
        req(getdata_descX())
        saveRDS(reactiveValuesToList(input),"input.rds")

        req(input$ggpair.variables)
        req(input$fm_palette)
        newdata<-getdata_descX()
        req(length(newdata)>0)
        req(input$ggpair.variables%in%colnames(newdata))
        data<-newdata[,input$ggpair.variables, drop=F]
        pred<-y<-NULL
        df=data
        cols<-vals$newcolhabs[[input$fm_palette]](1)
        my_cols<-cols



        if(yclude_y()){
          req(getdata_descX())
          req(input$ggpair.y.variable)

          factors<-attr(getdata_descX(),"factors")
          req(input$ggpair.y.variable %in% colnames(factors))
          y<-pred<-factors[rownames(data),input$ggpair.y.variable, drop=F]
          #validate(need(nlevels(pred[,1])<=50))
          df=cbind(data,pred)
          cols<-vals$newcolhabs[[input$fm_palette]](nlevels(pred[,1]))
          my_cols<-cols[pred[,1]]
        }
        include.y<-input$ggpair.box.include


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

       # req(   !any(sapply(args[-2],length)<1))
        # attach(args)
        #class(args)<-'iggpair'


        args

      })

      req(!inherits(args,"try-error"))

      args

    })

  })
}

desctools_tab5<-list()
desctools_tab5$ui<-function(id){
  ns<-NS(id)
  div(
    column(4,class='mp0', style="height:  calc(100vh - 200px);overflow: auto",
           box_caret(
             ns("box_5a"),
             title="Options",
             color="#c3cc74ff",
             div(
               div(style="display: flex",
                   pickerInput_fromtop(ns("cor_method"),choices = c("pearson", "kendall", "spearman"),tiphelp3("Correlation","Select the correlation coefficient to be computed."))


               ),
               div(style="display: flex",
                   pickerInput_fromtop(ns('cutoff_hl'),tiphelp3("Filter correlations:",
                                                        "Choose the pair-wise absolute correlation cutoff."),choices = list("All" = "all", "Lower than" = "lower", "Higher than" = "higher")),
                   uiOutput(ns('corr_cutoff'))
               ),

               pickerInput_fromtop(ns("cor_use"),
                           tiphelp3("Use","Select the method for computing covariances in the presence of missing values."),choices = c( "complete.obs","everything", "all.obs", "na.or.complete", "pairwise.complete.obs")),
               pickerInput_fromtop(ns("cor_dendogram"),tiphelp3("Dendogram","Choose whether to draw none, row, column, or both dendrograms."),choices = c("both","row","column","none")),
               pickerInput_fromtop(ns("cor_scale"),tiphelp3("Scale","Indicate if the values should be centered and scaled in the row direction, column direction, or none."),
                           choices = c("none","row", "column")),
               pickerInput_fromtop(ns("cor_Rowv"),
                           tiphelp3("Rowv","If TRUE, the dendrogram is computed and reordered based on row means."),choices = c('TRUE','FALSE')),
               pickerInput_fromtop(ns("cor_Colv"),tiphelp3("Colv","<code>Rowv</code> means that columns should be treated identically to the rows. If <code>TRUEv</code>, the dendrogram is computed and reordered based on column means."),choices = c('Rowv',T,F)),
               pickerInput_fromtop(ns("cor_revC"),tiphelp3("revC","Indicate if the column order should be reversed for plotting."),choices = c('TRUE','FALSE')),
               pickerInput_fromtop(ns("cor_na.rm"),tiphelp3("na.rm","Indicate if NAs should be removed."),choices = c('TRUE','FALSE')),
               pickerInput_fromtop(ns("cor_labRow"),tiphelp3("labRow","Choose whether to show observation labels."),choices = c('TRUE','FALSE')),
               pickerInput_fromtop(ns("cor_labCol"),tiphelp3("labCol","Choose whether to show variable labels."),choices = c('TRUE','FALSE')),
               pickerInput_fromtop(ns("cor_density.info"),tiphelp3("density.info","Indicate whether to superimpose a histogram, a density plot, or no plot on the color-key."),
                           choices = c("histogram","density","none")),
               pickerInput_fromtop(inputId = ns("cor_palette"),label = "Palette:",choices = NULL),
               numericInput(ns("cor_mar_row"),"X margin",value = 5, min = 0, step = 1),
               numericInput(ns("cor_mar_col"),"Y margin",value = 5, min = 0, step = 1),
               numericInput(ns("cor_sepwidth_a"),tiphelp3("sep row width:","Set the space between rows."),value = 0.05, min = 0.1, max = 1, step = .01),
               numericInput(ns("cor_sepwidth_b"),tiphelp3("sep col width:","Set the space between columns."),value = 0.05, min = 0.1, max = 1, step = .01),
               colourpicker::colourInput(ns("cor_sepcolor"),label = tiphelp3("Sep color:","Choose the color between rows and columns."),value = "black", showColour = "background"),
               colourpicker::colourInput(ns("cor_na.color"),label = tiphelp3("NA color:","Choose the color to use for missing value."),value = "gray", showColour = "background"),
               pickerInput_fromtop(ns("cor_cellnote"),tiphelp3("Cell note","Choose whether to show correlation values as cell notes."),choices = c('TRUE','FALSE')),
               colourpicker::colourInput(ns("cor_noteco"),label = tiphelp3("Note color:","Choose the color of the correlation value."),value = "black", showColour = "background"),
               numericInput(ns("cor_notecex"),tiphelp3("Note size:","Set the size of the correlation value."),value = 1, step = 0.1),
               div(),
               div(
                 actionLink(ns('corr_down_results'), span("Download Results", icon("fas fa-table")), style = "button_active")
               )
             )

           )
    ),
    column(8,class='mp0',

           box_caret(ns('box_5b'),
                     title="Correlation Plot",

                     button_title =                                       actionLink(ns('corr_downp'), "Download",icon("download")),

                     div(uiOutput(ns("corr_btn")),
                         uiOutput(ns("corr_plot")))

           )
    )



  )
}
desctools_tab5$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    getdata_descX<-reactive({
      req(vals$desc_data_x)
      vals$desc_data_x
    })
    box_caret_server("box_5a")
    box_caret_server("box_5b")


    get_corrdata<-reactive({
      req(getdata_descX())
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
      div(id=ns("run_cor_btn"),class='save_changes',actionButton(ns("run_cor"),"RUN", icon=icon("fas fa-sync")))
    })
    output$corr_cutoff<-renderUI({
      req(input$cutoff_hl!="all")
      if(is.null(vals$cor_cutoff)){vals$cor_cutoff<-0.9}
      div(
        inline(numericInput(ns("cor_cutoff"),"Cutoff",value = vals$cor_cutoff,min = 0.1,max = 1,step = .1)),
        inline(div(em(attr(get_corrdata(),"war"), tyle="color: gray"))),


      )
    })
    corplot_val<-reactiveVal()
    output$corr_plot<-renderUI({
      req(corplot_val())
      renderPlot({
        do.call(i_corplot,corplot_val())
        vals$plot_correlation<-recordPlot()
      })
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
    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'cor_palette', choices = vals$colors_img$val,
                        choicesOpt = list(content = vals$colors_img$img),)
    })
    observeEvent(ignoreInit = T,input$corr_down_results,{
      vals$hand_down<-"Corr result"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)

    })
    observeEvent(ignoreInit = T,input$corr_downp,{
      vals$hand_plot<-"Correlation Plot"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)

    })
    observeEvent(input$run_cor,ignoreInit = T,{
      args<-args_corrplot()
      corplot_val(args)
      shinyjs::removeClass("run_cor_btn",'save_changes')
    })
    observeEvent(args_corrplot(),{
      shinyjs::addClass("run_cor_btn",'save_changes')
    })

  })
}


desctools_tab6<-list()
desctools_tab6$ui<-function(id){
  ns<-NS(id)
  div(tabsetPanel(
    header=column(12, class="mp0",
                  column(4,class="mp0",
                         box_caret(ns("box_6a"),title="Distance",
                                   color="#c3cc74ff",
                                   div(style="display: flex",
                                       pickerInput_fromtop(ns("mds_distance"),"Distance:",choices = c('bray', "euclidean", 'jaccard')),
                                       uiOutput(ns("mds_button"))
                                   ))
                  )),
    tabPanel("6.1. Plot",
             value="mds_plot",
             column(4,class="mp0",
                    style="height:  calc(100vh - 200px);overflow: auto",
                    box_caret(
                      ns("box_6b"),title="Plot options",
                      color="#c3cc74ff",
                      div(


                        numericInput(ns('mds_base_size') ,"Base size:",value=12),
                        pickerInput_fromtop(ns('mds_theme') ,"Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
                        textInput(ns('mds_title') ,"Title:",value="Multidimensional scaling"),
                        numericInput(ns("mds_expandX"),"Expand X Axis",value=0.1,step=0.05),
                        numericInput(ns("mds_expandY"),"Expand Y Axis",value=0.1,step=0.05),
                        checkboxInput(ns('mds_show_intercept') ,"Intercepts",value=T),
                        checkboxInput(ns('mds_stress') ,"Display Stress",value=T)             )),
                    box_caret(
                      ns("box_6c"),title="Points",
                      color="#c3cc74ff",
                      div(checkboxInput(ns('mds_points') ,tiphelp3(strong("Scores - Points"),"Display mds scores as points"),value=T),
                          uiOutput(ns("mds_points_out"))
                      )

                    ),
                    box_caret(
                      ns("box_6d"),title="Text",
                      color="#c3cc74ff",
                      div(checkboxInput(ns('mds_text') ,strong("Scores - Text"),F),
                          uiOutput(ns("mds_text_out")),
                      )
                    )

             ),
             column(8,class="mp0",style="margin-top:-80px",
                    box_caret(
                      ns("box_6e"),title="MDS plot",

                      button_title = actionLink(
                        ns('mds_downp'),span("Download",icon("fas fa-download"))),
                      div(
                        div(

                          uiOutput(ns("plot_mds")))
                      )



                    ))


    ),
    tabPanel(
      "6.2. Summary",
      value="mds_summary",

      column(4,
             class='mp0',
             box_caret(
               ns("box_6f"),title="Options",
               color="#c3cc74ff",
               div(
                 pickerInput_fromtop(ns("show_mds_results"),"Show Scores:", list("Sites"="sites","Species"="species"))
               )
             )),
      column(8,class="mp0",style="margin-top: -80px",
             box_caret(
               ns("box_6g"),title="Results",

               button_title =actionLink(
                 ns('down_mds_results'),span("Download",icon("fas fa-table"))),
               div(uiOutput(ns("summary_mds")))
             ))
    )
  ))
}
desctools_tab6$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_6a")
    box_caret_server("box_6b")
    box_caret_server("box_6c")
    box_caret_server("box_6d")
    box_caret_server("box_6e")
    box_caret_server("box_6f")
    box_caret_server("box_6g")
    ns<-session$ns
    getdata_descX<-reactive({
      req(vals$desc_data_x)
      vals$desc_data_x
    })
    runval<-reactiveValues(mds="save_changes_nice")

    output$mds_button<-renderUI({

      div(class=vals$mds_btn_class,
          actionButton(ns("run_mds"),"RUN")
      )
    })

    output$summary_mds<-renderUI({
      req(!is.null(vals$mds))
      vals$mmds_summaryvals$mds_results<-data.frame(scores(vals$mds,input$show_mds_results))
      div(class="half-drop-inline",
          fixed_dt(vals$mmds_summaryvals$mds_results)
      )
    })
    output$mds_points_out<-renderUI({
      req(isTRUE(input$mds_points))
      choices=colnames(attr(getdata_descX(),"factors"))
      mds_scale_shape=tiphelp3("Scale Shape", "Use different shapes to represent points based on the selected factor")
      div(
        color_input(ns('mds_points_palette') ,tiphelp3("Palette","Choose a gradient to represent colors based on the selected factor"),selected='turbo', vals),
        pickerInput_fromtop(ns('mds_points_factor')  ,"Factor:",choices=choices,selected= NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),
        pickerInput_fromtop(inputId = ns("mds_points_shape"),
                    label = "Shape:",
                    choices = df_symbol$val,
                    choicesOpt = list(content = df_symbol$img),
                    selected=16,
                    width = "100px"),
        checkboxInput(ns('mds_scale_shape') ,mds_scale_shape,value=F),
        numericInput (ns('mds_points_size') ,"Size:",value=2),

      )
    })
    output$mds_text_out<-renderUI({
      req(isTRUE(input$mds_text))
      choices=colnames(attr(getdata_descX(),"factors"))
      div(
        color_input(ns('mds_text_palette') ,"Palette:",selected="gray",vals),
        pickerInput_fromtop(ns('mds_text_factor') ,"Factor:",choices=choices,selected=NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),

        numericInput(ns('mds_text_size') ,"Size:",value=4),
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

    observeEvent(ignoreInit = T,input$mds_downp,{
      vals$hand_plot<-"mds"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })
    observeEvent(list(getdata_descX(),input$mds_distance),ignoreInit = T,{
      vals$mds<-NULL
    })
    observeEvent(input$run_mds,{
      req(input$mds_distance)

      validate(need(!anyNA(getdata_descX()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
      withProgress(message="Running...",
                   min=NA,max=NA,{
        mds<-try(metaMDS(getdata_descX(), distance = input$mds_distance))
      })
      if(!inherits(mds,"try-error")){
      vals$mds<-mds}
    })

    observeEvent(ignoreInit = T,input$down_mds_results,{
      vals$hand_down<-"mds result"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)

    })

    observe({
      if(is.null(vals$mds)){
        vals$mds_btn_class<-"save_changes_nice"
      } else{ vals$mds_btn_class<-"btn_nice"}
    })


  })
}


desctools_tab7<-list()
desctools_tab7$ui<-function(id){
  ns<-NS(id)
  div(

    div(
      tabsetPanel(
        header= uiOutput(ns("pca_btn")),
        id=ns('pca_options'),
        tabPanel(
          "7.1. Plot",
          value="pca_plot",
          column(4,class="mp0",style="height:  calc(100vh - 200px);overflow: auto",

                 box_caret(ns("box_7a"),title="Plot Options",
                           color="#c3cc74ff",
                           div(
                             div(
                               numericInput(ns('pca_base_size') ,"Base size:",value=12),
                               pickerInput_fromtop(ns('pca_theme') ,"Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
                               textInput(ns('pca_title') ,"Title:",value="Principal Component analysis"),
                               numericInput(ns("pca_expandX"),"Expand X Axis",value=0.1,step=0.05),
                               numericInput(ns("pca_expandY"),"Expand Y Axis",value=0.1,step=0.05),
                             ),


                             checkboxInput(ns('pca_show_intercept') ,"Intercepts",value=T)


                           )
                 ),
                 box_caret(ns("box_7b"),title="Points",
                           color="#c3cc74ff",div(
                             checkboxInput(ns('pca_points') ,span("Show Points",tipright("Display PCA scores as points")),value=T),
                             uiOutput(ns("pca_points_out"))

                           )),
                 box_caret(ns("box_7c"),title="Text",
                           color="#c3cc74ff",div(
                             checkboxInput(ns('pca_text') ,"Text",F),
                             uiOutput(ns("pca_text_out")),
                           )),
                 box_caret(ns("box_7d"),title="Biplot",
                           color="#c3cc74ff",div(

                             checkboxInput(ns('pca_biplot') ,"Show Loadings",value=T),
                             uiOutput(ns("pca_biplot_out")),

                           ))



          ),
          column(8,class='mp0',
                 box_caret(
                   ns("box_7e"),title="PCA plot",
                   button_title =  actionLink(
                     ns('pca_downp'),span("Download Plot",icon("fas fa-download"))
                   ),
                   div(uiOutput(ns("plot_pca")))
                 )),


        ),
        tabPanel(
          "7.2. Summary",
          value="pca_summary",
          column(4,class="mp0",
                 box_caret(ns("box7_f"),
                           color="#c3cc74ff",
                           title="Options",
                           div(


                             pickerInput_fromtop(ns("show_pca_results"),"Show result:", c('Importance','Scores',"Standard deviations","Rotation","Centering","Scaling")),style="width: var(--parentHeight);"



                           )
                 )),
          column(8,class="mp0",
                 box_caret(ns("box7_g"),
                           title="Table",
                           button_title =  actionLink(
                             ns('down_pca_results'),span("Download",icon("fas fa-table"))),
                           uiOutput(ns('summary_pca'))
                 ))

        )
      )

    )


  )
}
desctools_tab7$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    getdata_descX<-reactive({
      req(vals$desc_data_x)
      vals$desc_data_x
    })
    runval<-reactiveValues(pca="save_changes_nice")

    box_caret_server("box_7a")
    box_caret_server("box_7b")
    box_caret_server("box_7c")
    box_caret_server("box_7d")
    box_caret_server("box_7e")
    box_caret_server("box_7f")
    box_caret_server("box_7g")
    output$pca_points_out<-renderUI({
      req(isTRUE(input$pca_points))
      choices=colnames(attr(getdata_descX(),"factors"))
      pca_scale_shape=tiphelp3("Scale Shape", "Use different shapes to represent points based on the selected factor")
      div(
        color_input(ns('pca_points_palette') ,tiphelp3("Palette","Choose a gradient to represent colors based on the selected factor"),selected='turbo', vals),
        pickerInput_fromtop(ns('pca_points_factor')  ,"Factor:",choices=choices,selected= NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),
        pickerInput_fromtop(inputId = ns("pca_points_shape"),
                    label = "Shape:",
                    choices = df_symbol$val,
                    choicesOpt = list(content = df_symbol$img),
                    selected=16,

                    width = "100px"),
        checkboxInput(ns('pca_scale_shape') ,pca_scale_shape,value=F),
        numericInput (ns('pca_points_size') ,"Size:",value=2),

      )
    })
    output$pca_text_out<-renderUI({
      req(isTRUE(input$pca_text))
      choices=colnames(attr(getdata_descX(),"factors"))
      div(
        color_input(ns('pca_text_palette') ,"Palette:",selected="gray",vals),
        pickerInput_fromtop(ns('pca_text_factor') ,"Factor:",choices=choices,selected=NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),

        numericInput(ns('pca_text_size') ,"Size:",value=4),
      )
    })
    output$pca_biplot_out<-renderUI({
      value=ncol(getdata_descX())
      req(isTRUE(input$pca_biplot))


      div(
        numericInput(ns('pca_biplot_n') ,tiphelp3("Number of Variables", "Set the number of variables to display"),value=value, min=1),
        div(colourpicker::colourInput(ns('pca_biplot_color') ,"Text Color",value="blue","background")),
        div(colourpicker::colourInput(ns('pca_biplot_arrow_color') ,"Arrow Color",value="blue","background")),
        numericInput(ns('pca_biplot_size') ,"Text Size",value=4),
        checkboxInput(ns('pca_loading_axis'),"Axes",T),
        checkboxInput(ns('pca_lo_axis_color'),"Axes color as Arrows",T),
        textInput(ns('pca_lo_x.text'),"Loading Axis-X Label","PC1 loadings"),
        textInput(ns('pca_lo_y.text'),"Loading Axis-Y Label","PC2 loadings")


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

    output$plot_pca<-renderUI({

      args<-args_pca()

      vals$ppca_plot<-do.call(ggpca,args)

      div(

        renderPlot({
          vals$ppca_plot
        })

      )
    })
    is_centered<-function(data) {
      if (all(abs(colMeans(data)) < 1e-5))
        TRUE
      else FALSE

    }

    output$pca_warning<-renderUI({
      centered<-is_centered(getdata_descX())

      if(isTRUE(centered)){NULL} else{p("Warning: The data appears not to be centered. PCA typically requires centered data for accurate results. Adjustments can be made in the Transformations section of the Pre-processing tools.",style="color: grey; padding-left: 10px; font-size: 12px")}

    })
    output$pca_btn<-renderUI({
      validate(need(!anyNA(getdata_descX()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))

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

    output$summary_pca<-renderUI({


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
      div(class="half-drop-inline",
          fixed_dt(vals$pca_out)
      )
    })


    observeEvent(getdata_descX(),{

      req(!is.null(vals$ppca_plot))
      runval$pca<-"save_changes_nice"
    })

    observeEvent(ignoreInit = T,input$down_pca_results,{
      vals$hand_down<-"PCA result"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)

    })
    observeEvent(ignoreInit = T,input$pca_options,{
      vals$pca_options<-input$pca_options
    })

    observeEvent(ignoreInit = T,input$pca_downp,{
      vals$hand_plot<-"pca"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })

    observeEvent(input$run_pca,ignoreInit = T,{
      pca<-try(prcomp(getdata_descX(), center=F, scale=F))
      if(!inherits(pca,"try-error"))
      pca_model(pca)
      runval$pca<-"btn_nice"
    })


  })
}


desctools_tab8<-list()
desctools_tab8$ui<-function(id){
  ns<-NS(id)
  div(div(class="model_setup",

          box_caret(ns("box_setup3"),inline=F,
                    color="#374061ff",
                    title="Model Setup",
                    div(
                      div(style="display: flex; gap: 10px; margin-top: -10px",class="setup_box picker-flex",

                          div(style="display: flex;",
                              div(class="setup_box picker-flex picker-before-y",pickerInput_fromtop(ns("rda_X"),"", choices=NULL)

                              ),
                          ),
                          div(style="height: 30px; margin-top: 20px",
                              div(strong("~")),
                              div(actionLink(ns("rev_rda"),icon("arrow-right-arrow-left")))),
                          div(style="display: flex;",
                              div(
                                class="setup_box picker-flex picker-before-x", pickerInput_fromtop(ns("rda_Y"),"", choices=NULL)
                              )
                          ),
                          div(style="margin-top: 20px",
                            uiOutput(ns('rda_btn'))
                          )

                      ),
                      div(
                        style="",
                        checkboxInput(ns("rda_scale"),span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T))

                    )
          )
  ),
  tabsetPanel(
    id=ns("rda_view"),
    tabPanel(
      "8.1. Plot",
      div(
        column(4,class="mp0",style="height:  calc(100vh - 200px);overflow: auto",
               box_caret(
                 ns("box_8a"),title="General options",
                 color="#c3cc74ff",
                 div(

                   numericInput(ns('rda_base_size') ,"Base size:",value=12),
                   pickerInput_fromtop(ns('rda_theme') ,"Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
                   textInput(ns('rda_title') ,"Title:",value="Redundancy analysis"),
                   numericInput(ns("rda_expandX"),"Expand X Axis",value=0.1,step=0.05),
                   numericInput(ns("rda_expandY"),"Expand Y Axis",value=0.1,step=0.05),

                   checkboxInput(ns('rda_show_intercept') ,"Intercepts",value=T)


                 )
               ),

               box_caret(
                 ns("box_8b"),title="Points",
                 color="#c3cc74ff",
                 div(
                   checkboxInput(ns('rda_points') ,tiphelp3("Observation Scores","Display Predictor Scores as points"),value=T),
                   uiOutput(ns("rda_points_out"))

                 )

               ),

               box_caret(
                 ns("box_8c"),title="Text",
                 color="#c3cc74ff",
                 div(
                   checkboxInput(ns('rda_text') ,tiphelp3("Observation Scores","Display Predictor Scores as Text"),F),
                   uiOutput(ns("rda_text_out")))
               ),

               box_caret(
                 ns("box_8d"),title="Biplot",
                 color="#c3cc74ff",
                 div(checkboxInput(ns('rda_biplot') ,"Predictor Scores",value=T),
                     uiOutput(ns("rda_biplot_out"))
                 )

               ),
               box_caret(
                 ns("box_8e"),title="Response scores",
                 color="#c3cc74ff",
                 div( checkboxInput(ns('rda_species') ,tiphelp3(strong("Response Scores"),"Display Response Scores as points"),value=T),
                      uiOutput(ns("rda_species_out"))
                 ))

        ),

        column(8,class="mp0",
               box_caret(
                 ns("box_8f"),title="RDA plot",

                 button_title = actionLink(
                   ns('rda_downp'),span("Download Plot",icon("fas fa-download"))),
                 div(uiOutput(ns("rda_plot")))
               ))


      )
    ),
    tabPanel("8.2. Summary",

             div(
               column(4,class="mp0",
                      box_caret(
                        ns("box_8g"),title="Options",
                        color="#c3cc74ff",
                        div(
                          pickerInput_fromtop(ns("disp_rda_summ"),"Results:",choices=c('Importance (unconstrained)',"Importance (constrained)",'Variable scores','Observation scores','Linear constraints','Biplot')),
                          numericInput(ns("rda_axes"),"Axes", value=2)


                        ))
               ),
               column(8,class="mp0",
                      box_caret(
                        ns("box_8h"),title="Print",button_title = actionLink(ns('downcenter_rda'),span("Download",icon("fas fa-download"))),
                        verbatimTextOutput(ns("rda_print"))
                      )
               )
             ))

  )


  )
}
desctools_tab8$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_setup3")
    box_caret_server("box_8a")
    box_caret_server("box_8b")
    box_caret_server("box_8c")
    box_caret_server("box_8d")
    box_caret_server("box_8e")
    box_caret_server("box_8f")
    box_caret_server("box_8g")
    box_caret_server("box_8h")
    ns<-session$ns

    runval<-reactiveValues(rda="save_changes_nice")

    output$rda_points_out<-renderUI({
      req(isTRUE(input$rda_points))
      choices=colnames(attr(get_rdaX(),"factors"))
      div(
        color_input(ns('rda_points_palette') ,tiphelp3("Palette","Choose a gradient to represent colors based on the selected factor"),selected='turbo', vals),
        pickerInput_fromtop(ns('rda_points_factor')  ,"Factor:",choices=choices,selected= NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),
        pickerInput_fromtop(inputId = ns("rda_points_shape"),
                    label = "Shape:",
                    choices = df_symbol$val,
                    choicesOpt = list(content = df_symbol$img),
                    selected=16,

                    width = "100px"),
        numericInput (ns('rda_points_size') ,"Size:",value=2),

      )
    })
    output$rda_text_out<-renderUI({
      req(isTRUE(input$rda_text))
      choices=colnames(attr(get_rdaX(),"factors"))
      div(
        color_input(ns('rda_text_palette') ,"Palette:",selected="gray",vals),
        pickerInput_fromtop(ns('rda_text_factor') ,"Factor:",choices=choices,selected=NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),

        numericInput(ns('rda_text_size') ,"Size:",value=4),
      )
    })
    output$rda_biplot_out<-renderUI({
      req(input$rda_Y)
      value=ncol(vals$saved_data[[input$rda_Y]])
      req(isTRUE(input$rda_biplot))

      tips<-list(
        biplot_n=tiphelp3("Number of Variables", "Set the number of variables to display"),
        biplot_arrow_color="Arrow Color",
        biplot_color="Text Color",
        biplot_size="Text size"
      )


      div(
        numericInput(ns('rda_biplot_n') ,tips$biplot_n,value=value, min=1),
        div(colourpicker::colourInput(ns('rda_biplot_color') ,tips$biplot_color,value="blue","background")),
        div(colourpicker::colourInput(ns('rda_biplot_arrow_color') ,tips$biplot_arrow_color,value="blue","background")),

        numericInput(ns('rda_biplot_size') ,tips$biplot_size,value=4)

      )
    })
    rda_points_factor<-reactive({
      req(length(input$rda_points)>0)
      if(isTRUE(input$rda_points)){
        req(input$rda_points_factor)
        data<-get_rdaX()
        factors<-attr(data,"factors")
        factors[rownames(data),input$rda_points_factor, drop=F]
      } else{NULL}

    })
    rda_text_factor<-reactive({
      req(length(input$rda_text)>0)
      if(isTRUE(input$rda_text)){
        req(input$rda_text_factor)
        data<-get_rdaX()
        factors<-attr(data,"factors")
        factors[rownames(data),input$rda_text_factor, drop=F]
      } else{ NULL}

    })
    get_rda_model<-reactive({

      req(input$rda_X)
      req(input$rda_Y)

      model<-try({
        data<-vals$saved_data[[input$rda_X]]
        x<-as.matrix(data)
        colnames(x)<-colnames(data)
        if(length(input$rda_Y)>0){
          y<-na.omit(vals$saved_data[[input$rda_Y]][rownames(x),,drop=F])
          colnames(y)<-colnames(vals$saved_data[[input$rda_Y]])
          x<-na.omit(x[rownames(y),,drop=F])
          dim(data.frame(y))
          dim(x)
          rda(x~.,data=data.frame(y) ,scale=input$rda_scale)
        } else{ rda(x,scale=input$rda_scale)}
      })
      req(!inherits(model,"try-error"))

      model
    })
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

        biplot_size=input$rda_biplot_size,
        biplot_color=input$rda_biplot_color,
        biplot_arrow_color=input$rda_biplot_arrow_color,
        species_n=input$rda_species_n,

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

    output$rda_plot<-renderUI({
      req(input$rda_X%in%names(vals$saved_data))
      validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
      validate(need(!anyNA(vals$saved_data[[input$rda_X]]), "Missing values (Datalist Y) not allowed"))

      validate(need(!anyNA(vals$saved_data[[input$rda_Y]]), "Missing values (Datalist X) not allowed"))

      args<-rda_args()
      req(length(args$model)>0)
      vals$rda_plot<-do.call(ggrda,args)
      renderPlot({

        vals$rda_plot
      })
    })

    output$rda_print<-renderPrint({rda_summary()})

    rda_summary<-reactive({
      req(input$disp_rda_summ)
      req(rda_model())
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
    get_rdaX<-function(){
      req(input$rda_X)
      vals$saved_data[[input$rda_X]]
    }
    output$rda_species_out<-renderUI({
      value=ncol(get_rdaX())
      req(isTRUE(input$rda_species))
      div(
        div(
          colourpicker::colourInput(ns('rda_species_color') ,"Colour:",value="red","background")
        ),
        pickerInput_fromtop(ns('rda_species_plot') ,"Display:",choices=c("points","text")),
        pickerInput_fromtop(inputId = ns("rda_species_shape"),
                    label = "Shape:",
                    choices = df_symbol$val,
                    choicesOpt = list(content = df_symbol$img),
                    selected=df_symbol$val[8],

                    width = "100px"),
        numericInput(ns('rda_species_n') ,"sp number:",value=value),

        numericInput(ns('rda_species_size') ,"Size:",value=2),

      )
    })
    observeEvent(ignoreInit = T,input$downcenter_rda,{
      vals$hand_down<-"rda"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)

    })

    observeEvent(ignoreInit = T,input$disp_rda_summ,
                 vals$disp_rda_summ<-input$disp_rda_summ)
    observeEvent(ignoreInit = T,input$rda_view,
                 vals$rda_view<-input$rda_view)
    observeEvent(ignoreInit = T,input$rda_downp,{
      vals$hand_plot<-"rda"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })
    observeEvent(input$rda_X,{
      ncol<-ncol(get_rdaX())
      value=10
      if(value>ncol){
        vals$rda_spnum<-ncol
      }
    })

    observeEvent(input$rev_rda,{
      updatePickerInput(session,"rda_Y",selected=input$rda_X)
      updatePickerInput(session,"rda_X",selected=input$rda_Y)
    })

    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      updatePickerInput(session,'rda_Y',choices=choices,selected=choices[2])
      updatePickerInput(session,'rda_X',choices=choices,selected=choices[1])
    })

    observeEvent(input$run_rda,{
      validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
      validate(need(!anyNA(vals$saved_data[[input$rda_X]]), "Missing values (Datalist Y) not allowed"))

      validate(need(!anyNA(vals$saved_data[[input$rda_Y]]), "Missing values (Datalist X) not allowed"))


      rda_model(get_rda_model())
      runval$rda<-"btn_nice"
    })

    observeEvent(list(input$rda_X,
                      input$rda_Y,
                      input$rda_scale),ignoreInit = T,{

                        runval$rda<-"save_changes_nice"
                      })

  })
}

desctools_tab9<-list()
desctools_tab9$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(
      ns("box_setup4"),inline=F,
      color="#374061ff",
      title="Model Setup",
      div(
        div(style="display: flex; gap: 10px; margin-top: -10px; margin-bottom: 10px",class="setup_box picker-flex",

            div(style="display: flex;",
                div(class="setup_box picker-flex picker-before-y",pickerInput_fromtop(ns("segrda_X"),"", choices=NULL)

                ),
            ),
            div(style="height: 30px; margin-top: 20px",
                div(strong("~")),
                div(actionLink(ns("rev_segrda"),icon("arrow-right-arrow-left")))),
            div(style="display: flex;",
                div(class="setup_box picker-flex picker-before-x",pickerInput_fromtop(ns("segrda_Y"),"", choices=NULL)),
            ),
            uiOutput(ns('segrda_btn'))

        )

      )

    ),
    tabsetPanel(
      id=ns("segrda_panels"),
      tabPanel(
        "9.1. Split Moving window",
        tabsetPanel(
          id=ns("smw_panels"),
          tabPanel("9.1.1 Data ordination",value="swm_1",
                   column(4,class='mp0',style="height:  calc(100vh - 200px);overflow: auto",
                          box_caret(
                            ns("box_9a"),title="Analysis setup",
                            color="#c3cc74ff",
                            div(
                              checkboxInput(ns("segrda_scale"),span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T),
                              div(style="display: flex",
                                  checkboxInput(ns("segrda_ord"),strong("Axis ordination",pophelp(NULL,"Both the SMW and pwRDA analyses depend on ordered datasets. Defaults to ordering both response and explanatory matrices using one of the axes of the RDA model. If unchecked,please make sure all your inputs are already ordered.")), value=T),
                                  numericInput(ns("axis_ord_segrda"),NULL, value=1, step=1)),
                              uiOutput(ns("ord_sure")),

                              textInput(ns('custom_windows'), span(tipify(icon("fas fa-question-circle",style="color: gray"),"Enter a vector of breakpoints (comma delimited, within the data range)"),"Windows"), NULL),


                              pickerInput_fromtop(ns("smw_dist"),span(tipify(icon("fas fa-question-circle", style="color: gray"),"Dissimilarity index"),"Distance:"), choices=c("bray","euclidean","manhattan","jaccard")),
                              pickerInput_fromtop(ns("smw_rand"),span(tipify(actionLink(ns("smw_rand_help"),icon("fas fa-question-circle")),"The type of randomization for significance computation. Click for details"),"Randomization:"), choices=c("shift","plot")),

                              numericInput(ns("smw_nrand"),span(tipify(icon("fas fa-question-circle", style="color: gray"),"The number of randomizations"),"n.rand:"), value=10),
                              uiOutput(ns("down_we_out"))


                              ,

                              uiOutput(ns('run_smw'))
                            )
                          ),
                   ),
                   column(8, class="mp0",
                          box_caret(
                            ns("box_9b"),title="Matrix plot",
                            button_title = actionLink(
                              ns("downp_matrixplot"),span("Download",icon("fas fa-download"))),

                            div( plotOutput(ns("ordplot_matrix")))

                          ))

          ),
          tabPanel("9.1.2 SMW results",
                   value="swm_2",
                   actionLink(ns("downp_windowpool"),"Download plot"),
                   plotOutput(ns("poolplot")))

        )


      ),
      tabPanel(
        "9.2. Dissimilarity Profile",

        div(strong("Save break points"),
            inline(uiOutput(ns("save_breakpoints")))),
        tabsetPanel(
          id=ns("dp_view"),
          tabPanel("9.2.1. Plot", value="Plot",

                   column(4,class='mp0',style="height:  calc(100vh - 200px);overflow: auto",
                          box_caret(
                            ns("box_9c"),title="Plot options",
                            color="#c3cc74ff",
                            div(

                              selectInput(ns("dp_w"),span(tipify(icon("fas fa-question-circle", style="color: gray"),"A target window size from which results will be extracted. If <code>average</code> return z-scores averaged over the set of window sizes"),'+ w:'), 'average'),

                              pickerInput_fromtop(ns("dp_index"),span(actionLink(ns("dp_index_help"),tipify(icon("fas fa-question-circle"),"The result to be extracted. Click for details")),'+ index:'),choices=c("dp","rdp","md","sd","oem","osd","params")),

                              pickerInput_fromtop(ns("dp_sig"),span(actionLink(ns("dp_sig_help"),tipify(icon("fas fa-question-circle"),"Significance test for detecting dissimilarity values that differs significantly from those appearing in a random pattern. Click for details")),'+ sig'),choices=c("z","sd","sd2","tail1")),

                              numericInput(ns("dp_z"),span(tipify(icon("fas fa-question-circle", style="color: gray"),"The critical value for the significance of z-values"),"z:"), value=1.85,step=0.01),

                              pickerInput_fromtop(ns("dp_BPs"),span(tipify(icon("fas fa-question-circle", style="color: gray"),"Defines if the breakpoints should be chosen as those sample positions corresponding to the maximum dissimilarity in a sequence of significant values (max) or as those sample positions corresponding to the median position of the sequence (median). Defaults to BPs=max. If empty the breakpoints are not computed"),"BPs:"),choices=c("","max","median"), selected = "max"),
                              uiOutput(ns('dp_seq.sig')),


                              pickerInput_fromtop(inputId=ns("dp_palette"),
                                          label = "Palette",NULL),
                              numericInput(ns("dp_cex"), "size",value=1,min=0.1,step=0.1),
                              colourpicker::colourInput(inputId=ns("dp_dcol"),
                                                        label = "diss col",
                                                        value="Grey"
                              ),
                              colourpicker::colourInput(inputId=ns("dp_scol"),
                                                        label = "sig col",
                                                        value="darkblue"
                              ),
                              colourpicker::colourInput(inputId=ns("dp_bcol"),
                                                        label = "bp col",
                                                        value="black"
                              ),

                              div(numericInput(ns("dp_bg"), "bg",value=0.5,min=0,max=1,step=0.05))



                            )
                          )
                   ),
                   column(8, class="mp0",
                          box_caret(
                            ns("box_9d"),title="Dissimilariry profile plot",
                            button_title =   actionLink(
                              ns('downp_dp'),span("Download",icon("fas fa-download"))),
                            div(plotOutput(ns("plot_dp")))
                          ))

          ),
          tabPanel("9.2.1. DP results", value="DP results",
                   div(uiOutput(ns("dp_view_dp"))),
                   uiOutput(ns("dp_extract")))
        )



      ),

      tabPanel(
        "9.3. pWRDA",

        box_caret(
          ns("box_setup5"),inline=F,
          color="#374061ff",
          title = "Piecewise Redundacy Analysis",
          div(style="height: 65px",
              div(style="display: flex;",class="setup_box picker-flex",



                  div(style="display: flex;",
                      div(tipify(tags$div("BP",class="trailab"),"Split reference (Y Datalist)")),
                      pickerInput_fromtop(ns('bp_column'), NULL,NULL)),
                  div(style="display: flex;;margin-right: 20px",
                      div(

                        tipify(tags$div("n.rand",class="trailab"),"Number of randomizations")),
                      div(class="half-drop",
                          numericInput(ns("pw_nrand"), NULL, value=99))),

                  div(style="display: flex;;margin-left: 20px",
                      div(tipify(tags$div("Results",class="trailab"),"Predictors")),
                      div(style="display: flex",
                          pickerInput_fromtop(ns("pwRDA_models"),NULL, choices=NULL),

                      )),
                  actionButton(ns("delete_pwRDA_models"),icon("fas fa-trash-alt")),

              ),
              div( div(style="display: flex;",

                       div(id=ns("run_pwRDA_btn"),
                           class="save_changes",
                           actionButton(ns('run_pwRDA'),"RUN pwRDA >>",style="margin-top: 5px;margin-left: 0px;padding-left: 5px;padding-right: 5px")
                       )
                       ,
                       div(style="margin-top: 5px",
                           id=ns("save_pwRDA_btn"),class="save_changes",
                           actionButton(ns("save_pwRDA"),icon("fas fa-save"), style="button_active")
                       )

              ))

          )

        ),



        tabsetPanel(
          id=ns("segrda_view"),

          tabPanel(
            "9.3.1. Plot", value="Plot",
            div(

              column(
                4,
                class="mp0",  style="height:  calc(100vh - 200px);overflow: auto",
                box_caret(
                  ns("box_9e"),
                  title="Options",
                  color="#c3cc74ff",


                  div(
                    div(
                      div(
                        checkboxInput(ns("segrda_scaling"),span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=T),
                        numericInput(ns('segrda_base_size') ,"Base size:",value=12),
                        pickerInput_fromtop(ns('segrda_theme') ,"Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
                        textInput(ns('segrda_title') ,"Title:",value="Piecewise Redundancy analysis"),
                        numericInput(ns("segrda_expandX")," + X Axis Expansion",value=0.1,step=0.05),
                        numericInput(ns("segrda_expandY")," + Y Axis Expansion",value=0.1,step=0.05),
                        checkboxInput(ns('segrda_show_intercept') ,"Intercepts",value=T)
                      )
                    )
                  )

                ),
                box_caret(
                  ns("box_9f"),title="Points",
                  color="#c3cc74ff",
                  div(
                    checkboxInput(ns('segrda_points') ,tiphelp3("Observation Scores","Display Predictor Scores as points"),value=T),
                    uiOutput(ns("segrda_points_out")) )
                ),
                box_caret(
                  ns("box_9g"),title="Text",
                  color="#c3cc74ff",
                  div(
                    checkboxInput(ns('segrda_text') ,tiphelp3("Observation Scores","Display Predictor Scores as Text"),F),
                    uiOutput(ns("segrda_text_out")))
                ),
                box_caret(
                  ns("box_9h"),title="Biplot",
                  color="#c3cc74ff",
                  div(checkboxInput(ns('segrda_biplot') ,"Predictor Scores",value=T),
                      uiOutput(ns("segrda_biplot_out"))
                  )
                ),
                box_caret(
                  ns("box_9i"),title="Response scores",
                  color="#c3cc74ff",
                  div( checkboxInput(ns('segrda_species') ,tiphelp3(strong("Response Scores"),"Display Response Scores as points"),value=T),
                       uiOutput(ns("segrda_species_out"))
                  ))

              ),
              column(
                8,class="mp0",
                box_caret(ns("box_9j"),

                          title="pwRDA Plot",
                          button_title = actionLink(ns('segrda_downp'),span("Download Plot",icon("fas fa-download"))),
                          div(uiOutput(ns("segrda_plot")))
                )


              )

            )

          ),
          tabPanel("9.3.2. Summary", value="Summary",
                   column(4,class="mp0",
                          box_caret(ns("box_9k"),
                                    title="Options",
                                    color="#c3cc74ff",
                                    div(

                                      div(
                                        pickerInput_fromtop(ns("disegrda_sp_summ"),"Results:",choices=c('Summary stats','Importance (unconstrained)',"Importance (constrained)",'Variable scores','Observation scores','Linear constraints','Biplot'))),
                                      numericInput(ns("segrda_axes"),"Axes", value=2),

                                      div(tipify(downloadLink(ns('downmodel_segrda'),span("Download model"), style="button_active"),"Download model as .rds file"))

                                    )

                          )),
                   column(8,class="mp0",
                          box_caret(ns("box_9l"),
                                    title="Results",
                                    button_title = div(tipify(actionLink(ns('downcenter_segrda'),span("Download",icon("fas fa-table"))),"Download selected results")),
                                    div(verbatimTextOutput(ns("segrda_print")))

                          ))

          )

        )



      )
    )

  )
}
desctools_tab9$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_setup4")
    box_caret_server("box_setup5")
    box_caret_server("box_9a")
    box_caret_server("box_9b")
    box_caret_server("box_9c")
    box_caret_server("box_9d")
    box_caret_server("box_9e")
    box_caret_server("box_9f")
    box_caret_server("box_9g")
    box_caret_server("box_9h")
    box_caret_server("box_9i")
    box_caret_server("box_9j")
    box_caret_server("box_9k")
    box_caret_server("box_9l")
    ns<-session$ns



    ns<-session$ns

    getdata_descX<-reactive({
      vals$saved_data[[input$segrda_Y]]
    })


    pwmodels<-reactive({
      req(input$segrda_X)
      names(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))
    })
    matrixplot<-reactive({
      req(isTRUE(input$segrda_ord))
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
      res<-recordPlot()
      res
    })
    output$ordplot_matrix<-renderPlot({
      req(input$segrda_X)
      req(input$segrda_Y)
      validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
      validate(need(!anyNA(vals$saved_data[[input$segrda_X]]), "Missing values (Datalist Y) not allowed"))

      validate(need(!anyNA(vals$saved_data[[input$segrda_Y]]), "Missing values (Datalist X) not allowed"))
      matrixplot()


    })
    saved_data_names<-reactiveVal()
    pw_icon<-base64enc::dataURI(file = "inst/www/pwrda_icon.png", mime = "image/png")
    smw_icon<-base64enc::dataURI(file = "inst/www/smw_icon.png", mime = "image/png")
    symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
    df_symbol<-data.frame(
      val = c(16,15,17,18,8,1,5,3)
    )
    for(i in 1:length(symbols))
    {
      symbol1<-base64enc::dataURI(file = paste0('inst/www/pch',i,".png"), mime = "image/png")

      df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}





    bag_smw<-reactiveValues(df=F)

    output$segrda_points_out<-renderUI({
      req(isTRUE(input$segrda_points))
      choices=colnames(attr(vals$saved_data[[input$segrda_X]],"factors"))


      div(
        color_input(ns('segrda_points_palette') ,tiphelp3("Palette","Choose a gradient to represent colors based on the selected factor"),selected='turbo', vals),
        pickerInput_fromtop(ns('segrda_points_factor')  ,"Factor:",choices=choices,selected= NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),
        pickerInput_fromtop(inputId = ns("segrda_points_shape"),
                    label = "Shape:",
                    choices = df_symbol$val,
                    choicesOpt = list(content = df_symbol$img),
                    selected=16,

                    width = "100px"),
        numericInput (ns('segrda_points_size') ,"Size:",value=2),

      )
    })
    output$segrda_text_out<-renderUI({
      req(isTRUE(input$segrda_text))
      choices=colnames(attr(getdata_descX(),"factors"))
      div(
        color_input(ns('segrda_text_palette') ,"Palette:",selected="gray",vals),
        pickerInput_fromtop(ns('segrda_text_factor') ,"Factor:",choices=choices,selected=NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),

        numericInput(ns('segrda_text_size') ,"Size:",value=4),
      )
    })
    output$segrda_biplot_out<-renderUI({
      value=ncol(vals$saved_data[[input$segrda_Y]])
      req(isTRUE(input$segrda_biplot))

      tips<-list(
        biplot_n=tiphelp3("Number of Variables", "Set the number of variables to display"),
        biplot_arrow_color="Arrow Color",
        biplot_color="Text Color",

        biplot_size="Text size"
      )

      div(
        numericInput(ns('segrda_biplot_n') ,tips$biplot_n,value=value, min=1),
        div(colourpicker::colourInput(ns('segrda_biplot_color') ,tips$biplot_color,value="blue","background")),
        div(colourpicker::colourInput(ns('segrda_biplot_arrow_color') ,tips$biplot_arrow_color,value="blue","background")),

        numericInput(ns('segrda_biplot_size') ,tips$biplot_size,value=4)

      )
    })
    output$segrda_species_out<-renderUI({
      value=ncol(vals$saved_data[[input$segrda_X]])
      req(isTRUE(input$segrda_species))
      div(
        div(
          colourpicker::colourInput(ns('segrda_species_color') ,"Colour:",value="red","background")
        ),
        pickerInput_fromtop(ns('segrda_species_plot') ,"Display:",choices=c("points","text")),
        pickerInput_fromtop(inputId = ns("segrda_species_shape"),
                    label = "Shape:",
                    choices = df_symbol$val,
                    choicesOpt = list(content = df_symbol$img),
                    selected=df_symbol$val[8],

                    width = "100px"),
        numericInput(ns('segrda_species_n') ,"sp number:",value=value),

        numericInput(ns('segrda_species_size') ,"Size:",value=2),

      )
    })
    segrda_points_factor<-reactive({
      req(length(input$segrda_points)>0)
      if(isTRUE(input$segrda_points)){
        req(input$segrda_points_factor)
        data<-vals$saved_data[[input$segrda_X]]
        factors<-attr(data,"factors")
        factors[rownames(data),input$segrda_points_factor, drop=F]
      } else{NULL}

    })
    segrda_text_factor<-reactive({
      req(length(input$segrda_text)>0)
      if(isTRUE(input$segrda_text)){
        req(input$segrda_text_factor)
        data<-vals$saved_data[[input$segrda_X]]
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

        biplot_size=input$segrda_biplot_size,
        biplot_color=input$segrda_biplot_color,
        biplot_arrow_color=input$segrda_biplot_arrow_color,
        species_n=input$segrda_species_n,

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
      validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))

      validate(need(!anyNA(vals$saved_data[[input$segrda_X]]), "Missing values (Datalist Y) not allowed"))

      validate(need(!anyNA(vals$saved_data[[input$segrda_Y]]), "Missing values (Datalist X) not allowed"))

      args<-get_segrda_ggplot()
      renderPlot({
        vals$seg_rda_plot<-do.call(ggrda,args)
        vals$seg_rda_plot
      })
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
    output$ord_sure<-renderUI({
      req(isFALSE(input$segrda_ord))
      div(style='white-space: normal;',
          strong("Wargning:", style="color: SeaGreen"),"Make sure both X and Y are previously ordered")
    })
    output$run_smw<-renderUI({
      div(style="margin-left: 20px",
          class="go_smw_btn save_changes_nice",
          actionButton(ns("go_smw"),strong( img(src=smw_icon,height='20',width='20'),"run SMW"), style="button_active")
      )
    })
    get_windows<-eventReactive(input$segrda_X,{

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
    output$down_we_out<-renderUI({
      req(length(vals$smw_dp)>0)
      tipify(
        actionLink(
          ns('downp_we'),span("Download",icon("fas fa-download")), style="button_active"
        ),
        "Download plot"
      )
    })
    windowpoolplot<-reactive({

      if(length(vals$window_pool)>1){w.effect=TRUE
      main="Window size effect"} else{w.effect=F
      main="Dissimilary Profile"}

      suppressWarnings(plot( vals$smw_dp, w.effect =w.effect  , main=main))
      res<-recordPlot()
      res
    })
    output$poolplot<-renderPlot({
      getpool()
      validate(need(!is.null(vals$window_pool),"The window pool is empty. Use the arrow button to include the window sizes"))
      validate(need(length(vals$smw_dp)>0,"Please click 'run SMW' button"))

      windowpoolplot()




    })
    max_seq<-reactive({
      req(DP_smw())
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
    output$dp_seq.sig<-renderUI({
      numericInput(ns("dp_seq.sig"),span(tipify(icon("fas fa-question-circle", style="color: gray"),"The maximum length of consecutive, significant values of dissimilarity that will be considered in defining the community breakpoints"),"seq.sig:"), value=attr(max_seq(),"min"),step=1, min=1)
    })
    output$plot_dp<-renderPlot({
      validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
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
      dp_w<-if(input$dp_w=="average"){NULL} else{as.numeric(input$dp_w)}
      par(cex=input$dp_cex)
      suppressWarnings(
        suppressMessages(
          plot(smw,w=dp_w, sig=input$dp_sig, z=input$dp_z,   BPs=dp_BPs,
               seq.sig=input$dp_seq.sig, bg= getcolhabs(vals$newcolhabs,input$dp_palette,nlevels(as.factor(vals$splitBP[,1]))),bg_alpha=input$dp_bg,cols=c(input$dp_dcol,input$dp_scol,input$dp_bcol))
        )
      )


      vals$plot_dp<-recordPlot()
    })
    output$dp_extract<-renderUI({

      dp<-getDP()
      div(class="half-drop-inline",
          renderTable(dp)
      )

    })
    output$dp_view_dp<-renderUI({

      tipify(
        actionLink(
          ns('downcenter_dp_smw'),span("Download",icon("fas fa-table")), style="button_active"
        ),
        "Download DP results"
      )
    })
    get_breaks_from_factor<-reactive({
      x<-vals$saved_data[[input$segrda_X]]
      y<-vals$saved_data[[input$segrda_Y]]
      bp<-attr(x,"factors")[,input$bp_column]
      xord<-x[order(as.numeric(bp)),,drop=F]
      yord<-y[order(as.numeric(bp)),,drop=F]
      breaks<-bp[order(as.numeric(bp))]
      breaks<-which(diff(as.numeric(breaks))==1)
      pw_in<-list(
        breaks=breaks,
        xord=xord,
        yord=yord
      )
    })
    choices_bp_column<-reactive({
      req(input$segrda_X)
      x<-attr(vals$saved_data[[input$segrda_X]],"factors")
      colnames(x)
    })
    output$user_bp<-renderPrint(vals$bag_user_bp)
    output$getDP<-renderPrint({
      req(!isFALSE(vals$splitBP))
      suppressWarnings(bp(getDP()))})
    lenght_ok<-function(x){
      length(x)>0
    }
    output$downmodel_segrda<-{
      downloadHandler(
        filename = function() {
          paste0("pwRDA","_", Sys.Date(),".rds")
        }, content = function(file) {
          model<-attr(vals$saved_data[[input$segrda_X]],"pwRDA")[[input$pwRDA_models]]
          saveRDS(model,file)
        })

    }
    output$segrda_print<-renderPrint({segrda_summary()})
    DP_smw<-reactive({
      try({

        req(input$dp_BPs)

        req(vals$smw_dp)
        # savereac()
        smw<- vals$smw_dp

        dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
        dp_w<-if(input$dp_w=="average"){NULL} else{as.numeric(input$dp_w)}
        res<-suppressWarnings(
          suppressMessages(
            segRDA::extract(smw,w=NULL,
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
    })
    getDP<-reactive({
      req(length(input$dp_BPs)>0)
      req(length(input$dp_w)>0)
      smw<- vals$smw_dp
      dp_BPs<-if(input$dp_BPs==""){NULL} else{input$dp_BPs}
      dp_w<-if(input$dp_w=="average"){NULL} else{as.numeric(input$dp_w)}
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
      x<-na.omit(x[rownames(y),,drop=F])
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
    save_pwRDA<-reactive({
      attr(vals$saved_data[[input$segrda_X]],"pwRDA")[[input$newdatalist]]<-vals$segrda_model
      attr(vals$saved_data[[input$segrda_X]],"pwRDA")[["pwRDA (unsaved)"]]<-NULL
      updatePickerInput(session,'pwRDA_models', selected=input$newdatalist)
      vals$bag_pw<-NULL
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
    output$data_create<-renderUI({
      req(newname$df!=0)
      data_store$df<-F
      req(input$hand_save=="create")
      res<-textInput(ns("newdatalist"), NULL, newname$df, width="350px")
      data_store$df<-T
      inline(res)
    })
    module_desctools<-function() {
      ns<-session$ns
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

    output$data_over<-renderUI({
      data_overwritte$df<-F
      data<-getdata_descX()
      choices<-c(names(vals$saved_data))
      if(vals$hand_save=="Create factor using breakpoints from the dissimilarity profile"){choices<-colnames(attr(data,"factors"))}
      req(input$hand_save=="over")
      res<-pickerInput_fromtop(ns("over_datalist"), NULL,choices, width="350px",options=shinyWidgets::pickerOptions(liveSearch=T))
      data_overwritte$df<-T
      inline(res)
    })
    get_dataord<-reactive({
      req(input$missing_reorder!="N missing")
      data=getdata_descX()
      dataord<-if(input$missing_reorder=="Factor"){
        attr(data,"factors")    } else{data}
      dataord
    })

    observeEvent(pwmodels(),{

      choices<-names(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))
      updatePickerInput(session ,'pwRDA_models',choices=choices )
    })

    observeEvent(input$bp_column,{
      vals$cur_bp_column<-input$bp_column
    })
    observeEvent(choices_bp_column(),{
      updatePickerInput(session ,'bp_column',choices=choices_bp_column(),selected= vals$cur_bp_column)
    })
    observeEvent(saved_data_names(),{
      choices=names(vals$saved_data)
      updatePickerInput(session,'segrda_Y',choices=choices,selected=choices[2])
      updatePickerInput(session,'segrda_X',choices=choices,selected=choices[1])
    })
    observeEvent(ignoreInit = T,input$save_teste,{
      saveRDS(reactiveValuesToList(vals),"savepoint.rds")
      saveRDS(reactiveValuesToList(input),"input.rds")
      beep()
      #vals<-readRDS("vals.rds")
      #input<-readRDS('input.rds')

    })
    observeEvent(ignoreInit = T,input$segrda_Y,{
      vals$corplot<-NULL
    })
    observeEvent(input$summ_options,{
      vals$summ_options<-input$summ_options
    })
    observeEvent(ignoreInit = T,input$cextext,{
      vals$cextext<-input$cextext
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
    observeEvent(ignoreInit = T,input$downp_windowpool,{
      req(windowpoolplot())
      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=windowpoolplot()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Window size effect plot", name_c="winsize_plot")
    })
    observeEvent(vals$newcolhabs,{
      choices =vals$colors_img$val[getgrad_col()]
      choicesOpt = list(content =     vals$colors_img$img[getgrad_col()])
      updatePickerInput(session,'dp_palette', choices=choices,choicesOpt)
    })
    observeEvent(ignoreInit = T,input$dp_view,
                 vals$dp_view<-input$dp_view)
    observeEvent(ignoreInit = T,input$segrda_X,{
      x<-attr(vals$saved_data[[input$segrda_X]],"factors")
    })
    observeEvent(ignoreInit = T,input$bp_column,
                 vals$bp_column<-input$bp_column)
    observeEvent(input$run_pwRDA,{
      validate(need(!anyNA(vals$saved_data[[input$segrda_X]]), "Missing values (Datalist Y) not allowed"))

      validate(need(!anyNA(vals$saved_data[[input$segrda_Y]]), "Missing values (Datalist X) not allowed"))

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

                 }
    )
    observeEvent(ignoreInit = T,input$delete_pwRDA_models,{
      attr(vals$saved_data[[input$segrda_X]],"pwRDA")[[input$pwRDA_models]]<-NULL
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
    observeEvent(vals$smw_dp,{
      updateSelectInput(session,'dp_w',choices=c("average",gsub("w","",names(vals$smw_dp))))
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
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)

    })
    observeEvent(input$go_smw,{
      validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
      validate(need(!anyNA(vals$saved_data[[input$segrda_X]]), "Missing values (Datalist Y) not allowed"))

      validate(need(!anyNA(vals$saved_data[[input$segrda_Y]]), "Missing values (Datalist X) not allowed"))
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
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$tools_saveBP,{
      vals$hand_save<-"Create factor using breakpoints from the dissimilarity profile"
      vals$hand_save2<-div(span("Target:",em(input$segrda_X,style="color: SeaGreen")))
      vals$hand_save3<-NULL
      showModal(module_desctools())
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
    observeEvent(input$run_pwRDA,{
      validate(need(length(vals$saved_data)>1, "This functionality requires at least two datalist as explanatory and response data."))
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
    observeEvent(ignoreInit = T,input$boxplot_X,{vals$box_y<-input$box_y})
    observeEvent(ignoreInit = T,input$boxplot_X,{vals$boxplot_X<-input$boxplot_X})
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
    observeEvent(ignoreInit = T,input$split_missing,{

      data=getdata_descX()
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
        attr(X,"factors")<-factors[rownames(X),,drop=F]
        attr(X,"coords")<-coords[rownames(X),,drop=F]

        Y=data[-romipic,comi[i], drop=F]
        fac<-factors[rownames(Y),,drop=F]
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
        attr(Y,"coords")<-coords[rownames(Y),,drop=F]
        ylist[[i]]<-Y

        newdata=data[romipic,-comi, drop=F]
        attr(newdata,"factors")<-factors[rownames(newdata),,drop=F]
        attr(newdata,"coords")<-coords[rownames(newdata),,drop=F]


        name0<-paste0(input$segrda_Y,"_COMP_X_to_", colnames(Y))
        name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
        namemissing<-name1[length(vals$saved_data)+1]
        vals$saved_data[[namemissing]]<-X

        #name0<-paste0(input$segrda_Y,"_COMP_Y_", colnames(Y))
        # name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
        #namemissing<-name1[length(vals$saved_data)+1]
        #vals$saved_data[[namemissing]]<-Y

        name0<-paste0(input$segrda_Y,"_MISS_newX_to_",colnames(Y))
        name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
        namemissing<-name1[length(vals$saved_data)+1]
        vals$saved_data[[namemissing]]<-newdata
      }
      datY<-mergedatacol(ylist)
      name0<-paste0(input$segrda_Y,"_COMP_Y_")
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      namemissing<-name1[length(vals$saved_data)+1]
      vals$saved_data[[namemissing]]<-datY
    })
    observeEvent(ignoreInit = T,input$downp_matrixplot,{
      req(matrixplot())
      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=matrixplot()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Matrix plot", name_c="matrixplot")
    })
    observeEvent(input$rev_segrda,{
      updatePickerInput(session,"segrda_Y",selected=input$segrda_X)
      updatePickerInput(session,"segrda_X",selected=input$segrda_Y)
    })
    observeEvent(vals$saved_data,{
      saved_data_names(names(vals$saved_data))
    })

    observeEvent(ignoreInit = T,input$segrda_downp,{

      vals$hand_plot<-"segrda"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$cor_cutoff,{
      vals$cor_cutoff<-input$cor_cutoff
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

    observe({
      updateTextInput(session,'custom_windows',value=    paste0(get_windows(),collapse=", "))


    })
    observe({
      shinyjs::toggle('axis_ord_segrda',condition=isTRUE(input$segrda_ord))
    })
    observe({
      shinyjs::addClass()
    })
    observe({
      shinyjs::toggleClass('run_pwRDA_btn', class="save_changes",condition=!isTRUE(vals$pwmodel_ok))
    })
    observe({
      shinyjs::toggle("save_pwRDA",condition = isTRUE(vals$pwmodel_ok))
      shinyjs::toggleClass('save_pwRDA_btn', class="save_changes",condition=input$pwRDA_models=="pwRDA (unsaved)")

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
    observe({
      req(input$pwRDA_models)
      names(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))
      vals$segrda_model<-attr(vals$saved_data[[input$segrda_X]],"pwRDA")[[input$pwRDA_models]]
    })

  })
}

#' @export
desctools<-list()
#' @export
desctools$ui<-function(id){

  ns<-NS(id)
  tagList(
    div(
      style="background: white",
      #actionLink(ns("teste_comb"),"SAVE"),
      # uiOutput(ns("bug")),

      div(

        tabsetPanel(
          header=div(id=ns("box_data_descX"),
                     box_caret(ns("box_setup1"),inline=F,show_tittle=F,
                               color="#374061ff",
                               title="Setup",
                               div(
                                 div(class="setup_box picker-flex picker-before-x",

                                     pickerInput_fromtop(ns("data_descX"),tipify(span("Datalist:"),"Datalist for selecting the Factor"),choices = NULL))))
          ),

          id=ns('desc_options'),
          selected="tab1",
          tabPanel('1. Summaries',
                   value="tab1",
                   desctools_tab1$ui(ns("summaries")),
                   uiOutput(ns("desc_tab1"))
          ),
          tabPanel('2. Boxplot',
                   value="tab2",
                   desctools_tab2$ui(ns("boxplot")),
                   uiOutput(ns("desc_tab2"))
          ),
          tabPanel('3. Ridges',
                   value="tab3",
                   desctools_tab3$ui(ns("ridges")),
                   uiOutput(ns("desc_tab3"))
          ),

          tabPanel(
            '4. Pair plot',
            value="tab4",
            desctools_tab4$ui(ns("ggpair")),
            uiOutput(ns("desc_tab4"))
          ),
          #  tabPanel('Histogram',value="tab_histo",uiOutput(ns("dtab_histogram"))),

          tabPanel('5. Correlation plot',
                   value="tab5",
                   desctools_tab5$ui(ns("corr")),
                   uiOutput(ns("desc_tab5"))
          ),
          tabPanel(
            '6. MDS',
            value="tab6",
            desctools_tab6$ui(ns("mds")),
            uiOutput(ns("desc_tab6"))

          ),
          tabPanel(
            '7. PCA',
            value="tab7",
            desctools_tab7$ui(ns("pca")),
            uiOutput(ns("desc_tab7"))
          ),
          tabPanel(
            '8. RDA',
            value="tab8",
            desctools_tab8$ui(ns("rda")),
            uiOutput(ns("desc_tab8"))
          ),
          tabPanel(
            '9. segRDA',
            value="tab9",
            desctools_tab9$ui(ns("segrda")),
            uiOutput(ns("desc_tab9"))

          )


        )


      )

    )
  )

}
#' @export
desctools$server<-function (id,vals ){

  moduleServer(id,function(input,output,session){
    box_caret_server("box_setup1")

    ns<-session$ns





    output$desc_tab1<-renderUI({
      desctools_tab1$server("summaries",vals)
      NULL
    })
    output$desc_tab2<-renderUI({
      desctools_tab2$server("boxplot",vals)
      NULL
    })
    output$desc_tab3<-renderUI({
      desctools_tab3$server("ridges",vals)
      NULL
    })
    output$desc_tab4<-renderUI({
      desctools_tab4$server("ggpair",vals)
      NULL
    })
    output$desc_tab5<-renderUI({
      desctools_tab5$server("corr",vals)
      NULL
    })
    output$desc_tab6<-renderUI({
      desctools_tab6$server('mds',vals)
      NULL
    })
    output$desc_tab7<-renderUI({
      desctools_tab7$server('pca',vals)
      NULL
    })
    output$desc_tab8<-renderUI({
      desctools_tab8$server('rda',vals)
      NULL
    })
    output$desc_tab9<-renderUI({
      desctools_tab9$server('segrda',vals)
      NULL
    })

    observe({
      shinyjs::toggle('box_data_descX',condition=!input$desc_options%in%c('tab9','tab8',"tab_omi",'tab2'))
    })
    getdata_descX<-reactive({
      req(input$data_descX)
      vals$saved_data[[input$data_descX]]})
    observeEvent(input$data_descX,{
      vals$desc_data_x<-vals$saved_data[[input$data_descX]]
    })
    observeEvent(vals$saved_data,{
      updatePickerInput(session,'data_descX',choices=names(vals$saved_data), selected=vals$cur_data)
    })



  })
}

