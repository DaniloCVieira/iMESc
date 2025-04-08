#' The desctools module provides a comprehensive suite of tools for descriptive analysis and visualization of datasets in the iMESc application.
#'
#' ## Key Features:
#' 1. Interactive Descriptive Analysis:
#'    - Modular structure for various descriptive statistics and visualizations.
#'    - Includes tabs for summaries, boxplots, ridge plots, pair plots, correlation plots, MDS, PCA, RDA, and segmented RDA (segRDA).
#'
#' 2. Dynamic Data Handling:
#'    - Seamlessly integrates with saved datasets for reactive and interactive analysis.
#'    - Automatically updates the available datasets when changes are made to the saved data.
#'
#' 3. Advanced Visualization Tools:
#'    - Summaries: Display numeric summaries of datasets.
#'    - Boxplots: Visualize distributions across variables.
#'    - Ridge Plots: Compare distributions across multiple categories.
#'    - Pair Plots: Generate scatterplots and correlations for variable pairs.
#'    - Correlation Plots: Visualize relationships between variables.
#'    - MDS (Multidimensional Scaling): Project high-dimensional data into 2D space.
#'    - PCA (Principal Component Analysis): Decompose data variance into principal components.
#'    - RDA (Redundancy Analysis): Perform constrained ordination.
#'    - segRDA: Advanced segmented redundancy analysis.
#'
#' 4. User Interaction:
#'    - Select datasets for analysis through a user-friendly dropdown menu.
#'    - Toggle visibility of tools and inputs dynamically based on the selected analysis tab.
#'
#' 5. Extensibility and Integration:
#'    - Each descriptive tool is implemented as a separate submodule, enabling easy customization and extension.
#'    - Fully integrated with the iMESc application for preprocessing, visualization, and advanced analytics workflows.
#'
#' @export
desctools<-list()
#' @export
desctools$ui<-function(id){
  module_progress("Loading module: Descriptive Tools")
  ns<-NS(id)
  tagList(
    div(



      h4("Descriptive Tools", class="imesc_title"),

      div(
        style="background: white",
        #actionLink(ns("teste_comb"),"SAVE"),
        # uiOutput(ns("bug")),

        div(

          tabsetPanel(
            id=ns('desc_options'),
            selected="tab1",
            tabPanel('1. Summaries',
                     value="tab1",
                     desctools_tab1$ui(ns("summaries")),
                     uiOutput(ns("desc_tab1"))
            ),
            tabPanel('2. Boxplot',
                     value="tab2",

                     uiOutput(ns("desc_tab2"))
            ),
            tabPanel('3. Ridges',
                     value="tab3",

                     uiOutput(ns("desc_tab3"))
            ),

            tabPanel(
              '4. Pair plot',
              value="tab4",
              uiOutput(ns("desc_tab4"))
            ),
            #  tabPanel('Histogram',value="tab_histo",uiOutput(ns("dtab_histogram"))),

            tabPanel('5. Correlation plot',
                     value="tab5",

                     uiOutput(ns("desc_tab5"))
            ),
            tabPanel(
              '6. MDS',
              value="tab6",

              uiOutput(ns("desc_tab6"))

            ),
            tabPanel(
              '7. PCA',
              value="tab7",

              uiOutput(ns("desc_tab7"))
            ),
            tabPanel(
              '8. RDA',
              value="tab8",

              uiOutput(ns("desc_tab8"))
            ),
            tabPanel(
              '9. segRDA',
              value="tab9",

              uiOutput(ns("desc_tab9"))

            )


          )


        )


      ))
  )

}
#' @export
desctools$server<-function (id,vals ){

  moduleServer(id,function(input,output,session){
    box_caret_server("box_setup1")

    ns<-session$ns

    vals$rid_plot<-NULL
    vals$rda_plot <-NULL
    vals$smw_dp<-NULL
    vals$segrda_model<-NULL
    vals$mds<-NULL
    vals$corr_plot<-NULL
    vals$plot_dp<-NULL
    vals$seg_rda_plot<-NULL
    vals$pbox_plot<-NULL
    vals$dp_smw<-NULL
    vals$splitBP<-NULL




    output$desc_tab1<-renderUI({
      desctools_tab1$server("summaries",vals)
      NULL
    })


    output$desc_tab2<-renderUI({
      div(
        desctools_tab2$ui(ns("boxplot")),
        uiOutput(ns('boxplot_server'))
      )
    })
    output$boxplot_server<-renderUI({
      desctools_tab2$server("boxplot",vals)
      NULL
    })



    output$desc_tab3<-renderUI({
      div(
        desctools_tab3$ui(ns("ridges")),
        uiOutput(ns('ridges_server'))
      )
    })
    output$ridges_server<-renderUI({
      desctools_tab3$server("ridges",vals)
      NULL
    })

    output$desc_tab4<-renderUI({
      div(
        desctools_tab4$ui(ns("ggpair")),
        uiOutput(ns('ggpair_server'))
      )
    })
    output$ggpair_server<-renderUI({
      desctools_tab4$server("ggpair",vals)
      NULL
    })


    output$desc_tab5<-renderUI({
      div(
        desctools_tab5$ui(ns("corr")),
        uiOutput(ns('corr_server'))
      )
    })
    output$corr_server<-renderUI({
      desctools_tab5$server("corr",vals)
      NULL
    })


    output$desc_tab6<-renderUI({
      div(
        desctools_tab6$ui(ns("mds")),
        uiOutput(ns('mds_server'))
      )
    })
    output$mds_server<-renderUI({
      desctools_tab6$server("mds",vals)
      NULL
    })

    output$desc_tab7<-renderUI({
      div(
        desctools_tab7$ui(ns("pca")),
        uiOutput(ns('pca_server'))
      )
    })
    output$pca_server<-renderUI({
      desctools_tab7$server("pca",vals)
      NULL
    })



    output$desc_tab8<-renderUI({
      div(
        desctools_tab8$ui(ns("rda")),
        uiOutput(ns('rda_server'))
      )
    })
    output$rda_server<-renderUI({
      desctools_tab8$server("rda",vals)
      NULL
    })

    output$desc_tab9<-renderUI({
      div(
        desctools_tab9$ui(ns("segrda")),
        uiOutput(ns('segrda_server'))
      )
    })
    output$segrda_server<-renderUI({
      desctools_tab9$server("segrda",vals)
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
      choices=names(vals$saved_data)
      selected=vals$cur_data
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'data_descX',choices=choices, selected=selected)
    })

    observeEvent(input$data_descX,{
      vals$cur_data<-input$data_descX
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


## Logic for Tranforming data
transform_module<-list()
transform_module$ui<-function(id, label="Transformation"){
  ns<-NS(id)
  transf_label<-sapply(transf_df,function(x) x$label)
  transf_value<-sapply(transf_df,function(x) x$value)
  names(transf_value)<-transf_label
  pickerInput_fromtop_live(ns("transf"),label,choices=transf_value)
}
transform_module$server<-function(id,data){
  moduleServer(id,function(input,output,session){



    return(transf_data(data,input$transf,colnames(data)))

  })
}

## Logic for tab 1 - Summaries
desctools_tab1<-list()
desctools_tab1$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(ns("box_setup1"),inline=F,show_tittle=F,
              color="#374061ff",
              title="Setup",
              div(style="display: flex; gap: 20px;align-items: center",
                  div(class="setup_box picker-flex",style="margin-top: -10px",

                      pickerInput_fromtop(ns("data_descX"),strong("Datalist:"),choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T))),
                  uiOutput(ns("summary_all"))

              )),
    div(tabsetPanel(
      id=ns("summ_options"),
      tabPanel(
        value="Datalist",
        title = "1.1. Datalist",
        uiOutput(ns('render_datalist'))),
      tabPanel(
        value="Variables",
        title = "1.2. Numeric-Attribute",
        #uiOutput(ns("stats_data")),

        column(4,class="mp0",style="max-height: 400px; overflow-y: auto",
               box_caret(
                 ns("box_tab1_2_a"),
                 color="#c3cc74ff",
                 title="Select the Variables",
                 div(
                   class="virtual-130",
                   virtualPicker(ns('histo_var_x'),NULL,choices=NULL,searchPlaceholderText=NULL,styles="font-size: 11px")
                 )

               ),
               box_caret(
                 ns("box_tab1_2_b"),
                 color="#c3cc74ff",
                 title="Metrics",
                 div(
                   virtualPicker_unique(ns("varhisto_metric"),NULL,choices=c(
                     'Min.' ,'1st Qu.',"Mean",'Median','3rd Qu.','Max.'
                   ), selected=c('Min.' ,"Mean",'Max.'),multiple = T),
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
                  pickerInput_fromtop_live(
                    inputId = ns('pfac_border_palette'),
                    label = 'Border:',
                    choices=NULL),
                  pickerInput_fromtop_live(
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
    ))
  )
}
desctools_tab1$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected=vals$cur_summ_x
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'data_descX',choices=choices, selected=selected)
    })


    output$summary_all<-renderUI({
      get_summary_datalist(getdata_descX())
    })


    observeEvent(input$data_descX,{
      vals$cur_summ_x<-input$data_descX
    })
    getdata_descX<-reactive({
      req(input$data_descX)
      vals$saved_data[[input$data_descX]]
    })
    observeEvent(ignoreInit = T,input$downp_stats_fac,{
      vals$hand_plot<-"Factor plot"
      module_ui_figs("downfigs")

      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,datalist_name=attr(getdata_descX(),"datalist"))
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
      datalist_overview(getdata_descX(),available_models)

      #datalist_render(getdata_descX())
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



    observeEvent(getdata_descX(),{
      data<-getdata_descX()

    })

    observeEvent(input$histo_var_x,{
      vals$cur_histo_var_x<-input$histo_var_x
    })

    observe({

    })

    observeEvent(getdata_descX(),{
      data<-getdata_descX()
      if(is.null(vals$cur_histo_var_x)){
        vals$cur_histo_var_x<-colnames(data)[1:10]


      }
      selected=get_selected_from_choices(vals$cur_histo_var_x,colnames(data))


      shinyWidgets::updateVirtualSelect("histo_var_x",choices=colnames(data),selected=selected)
    })





    observeEvent(ignoreInit = T,input$downp_summ_num,{
      vals$hand_plot<-"variable summary"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,datalist_name=attr(getdata_descX(),"datalist"))
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
      req(length(input$histo_var_x)>0)

      col<-input$varhisto_palette
      col_border<-input$varhisto_border_col
      selected<-input$histo_var_x
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
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,datalist_name=attr(getdata_descX(),"datalist"))
    })







  })
}

## Logic for tab 2 - Boxplots
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
                              pickerInput_fromtop(ns("bbox_x_datalist"),tipify_ui(span("Datalist:"),"Datalist for selecting the Factor"),choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T))
                          ),

                          div(class="picker-flex",
                              pickerInput_fromtop(ns("boxplot_X"),"Factor",choices =NULL, options=shinyWidgets::pickerOptions(liveSearch =T))
                          ),
                          div(class="picker-flex",
                              pickerInput_fromtop(ns("filter_box1"),"Filter:",choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T))),
                          div(class="picker-flex",
                              pickerInput_fromtop(ns("filter_box2"),
                                                  "Class:",choices=NULL, options=shinyWidgets::pickerOptions(liveSearch =T)))
                      ),
                      div(style="display: flex;gap: 10px;height: 50px",class="setup_box picker-flex",
                          div(class="setup_box picker-flex picker-before-y", pickerInput_fromtop(ns("bbox_y_datalist"),tipify_ui(
                            span(""),
                            "Datalist for selecting the Numeric-Value",
                          ),choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T))
                          ),

                          div(class="picker-flex",
                              virtualPicker_unique( ns("box_y"),tipify_ui(
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

                               transform_module$ui(ns("box_transf"),span("Transf Y", tiphelp6("Apply a transformation to the Y-axis values."))),
                               checkboxInput(ns('box_horiz'),span("Horizontal:", tiphelp6("Display the box plot horizontally.")),value=F),

                               pickerInput_fromtop(ns("box_theme"),span("Theme:", tiphelp6("Select a theme to customize the plot\\'s appearance.")),c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic')),
                               pickerInput_fromtop(ns("box_facetwrap"),span("Facet wrap:", tiphelp6("Choose a variable to create separate panels for subsets of the data.")),c("None")),

                               pickerInput_fromtop_live(ns("box_palette"),
                                                        label = span("Palette:", tiphelp6("Select a color palette for the box plot.")),
                                                        choices=NULL),
                               numericInput(ns('box_alpha'),span("Lighten:", tiphelp6("Adjust the transparency of the box plot colors.")), .3, step=0.05),
                               colourpicker::colourInput(ns("box_linecol"),label =span("Line color:", tiphelp6("Specify the color for the box plot\\'s outline.")),value ="black",showColour="background"),
                               numericInput(ns('box_linewidth'),span("Line width:", tiphelp6("Set the width of the box plot\\'s outline.")), .5,step=.1),

                               numericInput(ns('box_base_size'),span("Base size:", tiphelp6("Define the base font size for the plot text.")), 12,step=1),
                               numericInput(ns('box_cex.main'),span("Title size:", tiphelp6("Adjust the font size for the plot title.")), 1.5,step=.1),
                               numericInput(ns('box_cex.label_panel'),span("Panel Title Size:", tiphelp6("Set the font size for the facet panel titles.")), 1.5,step=.1),
                               selectInput(ns("box_title_font"),span("Title font", tiphelp6("Choose the font style for the plot title.")),c("italic","bold","plain")),

                               numericInput(ns('box_cex.lab'),span("Label size:", tiphelp6("Adjust the font size for the axis labels.")), 1.5,step=.1),
                               numericInput(ns('box_cex.axes'),"Axis size:", 1.5,step=.1),
                               textInput(ns('box_title'),span("Title:", tiphelp6("Enter a title for the box plot.")),NULL),

                               numericInput(ns('box_xlab_rotate'),span("x text angle:", tiphelp6("Set the angle of the X-axis text labels.")), 0,step=5),
                               numericInput(ns('box_ylab_rotate'),span("y text angle:", tiphelp6("Set the angle of the Y-axis text labels.")), 0,step=5),

                               textInput(ns('box_xlab'),span("x label:", tiphelp6("Specify a label for the X-axis.")),NULL),
                               textInput(ns('box_ylab'),span("y label:", tiphelp6("Specify a label for the Y-axis.")),NULL),
                               checkboxInput(ns('box_grid'),span("Grid lines:", tiphelp6("Toggle the display of grid lines on the plot.")),value=T),

                               checkboxInput(ns("box_varwidth"),span("Varwidth:",tiphelp6("Drawn boxes with widths proportional to the square-roots of the number of observations in the groups","right")),F),
                               checkboxInput(ns('box_violin'),span("Violin:", tiphelp6("Display a violin plot overlaid on the box plot.")),value=F),

                               numericInput(ns('box_width'),span("Plot width:", tiphelp6("Set the width of the plot in pixels.")),550, step=50),
                               numericInput(ns('box_heigth'),span("Plot height:", tiphelp6("Set the height of the plot in pixels.")),400, step=50),
                               numericInput(ns('box_ncol'),span("N columns", tiphelp6("Specify the number of columns for arranging facets.")),2),
                               numericInput(ns('box_nrow'),span("N rows", tiphelp6("Specify the number of rows for arranging facets.")),NA),)
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

    observeEvent(bbox_x_datalist(),{
      data<-bbox_x_datalist()
      selected=vals$cur_box_facetwrap
      choices<-c("None",colnames(attr(data,"factors")))
      selected<-get_selected_from_choices(selected,choices)
      updatePickerInput(session,'box_facetwrap',choices=choices,selected=selected)
    })

    observeEvent(input$box_facetwrap,{
      vals$cur_box_facetwrap<-input$box_facetwrap
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


        res[2:ncol(res)]<-transform_module$server('box_transf',data=res[2:ncol(res)])
        colnames(res)[-1]<-colnames(y)
        res[,1]<-res[,1]


        res

      })
      if(input$box_facetwrap!="None"){
        result$group<-attr(bbox_x_datalist(),"factors")[rownames(result),input$box_facetwrap]
        attr(result,"group_name")<-input$box_facetwrap
      }

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
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,datalist_name=attr(bbox_y_datalist(),"datalist"))
    })

    output$stats_pbox<-renderUI({
      req(input$box_width)
      req(input$box_width>10)
      req(input$box_heigth)
      req(input$box_heigth>10)


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

      selected=vals$cur_bbox_y_datalist
      selected=get_selected_from_choices(selected,choices)

      updatePickerInput(session,'bbox_y_datalist',choices=choices,selected=selected)
    })
    observeEvent(input$bbox_y_datalist,{
      vals$cur_bbox_y_datalist<-input$bbox_y_datalist
    })




    observeEvent(bbox_x_datalist(),{
      data<-bbox_x_datalist()
      factors<-attr(data,"factors")
      choices = c("none", colnames(factors))
      selected=vals$cur_filter_box1
      selected=get_selected_from_choices(selected,choices)

      updatePickerInput(session,'filter_box1',choices=choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })

    observeEvent(input$filter_box1,{
      vals$cur_filter_box1<-input$filter_box1
    })
    observeEvent(bbox_x_datalist(),{
      choices=rev(colnames(attr(bbox_x_datalist(),"factors")))
      selected=vals$cur_boxplot_X
      selected=get_selected_from_choices(selected,choices)


      updatePickerInput(session,'boxplot_X',choices=choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })

    observeEvent(input$boxplot_X,{
      vals$cur_boxplot_X<-input$boxplot_X
    })
    observeEvent(bbox_y_datalist(),{
      data<-bbox_y_datalist()
      choices = colnames(data)
      selected=vals$cur_box_y
      selected=get_selected_from_choices(selected,choices)
      if(is.null(selected)){
        selected=choices[1]
      }
      shinyWidgets::updateVirtualSelect('box_y',choices=choices,selected= selected)
    })

    observeEvent(input$box_y,{
      vals$cur_box_y<-input$box_y
    })

    observeEvent(input$filter_box1,{
      req(input$filter_box1 != "none")
      data<-bbox_x_datalist()
      factors<-attr(data,"factors")
      labels<-factors[rownames(data), input$filter_box1]
      choices = c(levels(as.factor(labels)))
      selected=vals$cur_filter_box2
      selected=get_selected_from_choices(selected,choices)


      updatePickerInput(session,'filter_box2',choices=choices,selected = selected)
    })

    observeEvent(input$filter_box2,{
      vals$cur_filter_box2<-input$filter_box2
    })
    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected=vals$cur_bbox_x_datalist
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'bbox_x_datalist',choices=choices,selected=selected)
    })

    observeEvent(input$bbox_x_datalist,{
      vals$cur_bbox_x_datalist<-input$bbox_x_datalist
    })

    observeEvent(vals$newcolhabs,{
      selected<-get_selected_from_choices(vals$cur_boxplot_args$cur_box_palette,vals$colors_img$val)

      updatePickerInput(session,'box_palette', choices = vals$colors_img$val,
                        choicesOpt = list(content = vals$colors_img$img),selected=selected)
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


## Logic for tab 3 - Ridge Plot
desctools_tab3<-list()
desctools_tab3$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(ns("box_setup1"),inline=F,show_tittle=F,
              color="#374061ff",
              title="Setup",
              div(
                div(class="setup_box picker-flex picker-before-x",

                    pickerInput_fromtop(ns("data_descX"),tipify_ui(span("Datalist:"),"Datalist for selecting the Factor"),choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T))))),
    column(4,class="mp0",  style="height:  calc(100vh - 200px);overflow: auto",
           box_caret(ns("box_3a"),
                     title="Setup",
                     color="#c3cc74ff",
                     div(

                       div(
                         div(
                           pickerInput_fromtop(inputId=ns("rid_y"),label = "X (factor)",choices =NULL, options=shinyWidgets::pickerOptions(liveSearch =T)),

                           div(tags$label("Y (numeric)")),
                           div(
                             class="virtual-130",
                             virtualPicker(ns("ridge_variables"),label=NULL,NULL,SelectedText="Variables selected",optionHeight='22px',style="font-size: 12px; ")
                           ),
                           transform_module$ui(ns("rid_transf"),span("Transf Y", tiphelp6("Apply a transformation to the Y-axis values."))),
                         ),



                       )
                     )),
           box_caret(ns("box_3b"),
                     title="Plot Options",
                     color="#c3cc74ff",
                     div(
                       pickerInput_fromtop(inputId=ns("rid_col"),label = "Palette:",NULL, options=shinyWidgets::pickerOptions(liveSearch =T)),
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
    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected=vals$cur_x_ridge
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'data_descX',choices=choices, selected=selected)
    })

    observeEvent(input$data_descX,{
      vals$cur_x_ridge<-input$data_descX
    })
    getdata_descX<-reactive({
      req(input$data_descX)
      vals$saved_data[[input$data_descX]]
    })


    runval<-reactiveValues(
      rid="save_changes_nice"
    )
    output$run_ridges_btn<-renderUI({
      div(class=runval$rid,
          actionButton(ns('run_ridges'), 'RUN', icon=icon("fas fa-sync")),
      )
    })






    output$rid_out<-renderUI({
      req(!is.null(vals$rid_plot))
      width=paste0(input$rid_width,"px")
      height=paste0(input$rid_heigth,"px")
      renderPlot(vals$rid_plot, width=input$rid_width,height=input$rid_heigth)

    })

    observeEvent(getdata_descX(),{
      choices=colnames(getdata_descX())
      selected=choices[1:3]
      if(!is.null(vals$cur_ridge_variables)){
        if(all(vals$cur_ridge_variables%in%choices)){
          selected=vals$cur_ridge_variables
        }
      }
      shinyWidgets::updateVirtualSelect('ridge_variables',choices=choices,selected=selected)
    })

    observeEvent(input$ridge_variables,{
      vals$cur_ridge_variables<-input$ridge_variables

    })
    observeEvent(args_ridges(),{
      req(input$rid_heigth)
      req(input$run_ridges)
      runval$rid<-"save_changes_nice"
    })

    observeEvent(vals$newcolhabs,{
      selected<-get_selected_from_choices(vals$cur_rid_args$cur_rid_col,vals$colors_img$val)

      updatePickerInput(session,'rid_col',choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),selected=selected)
    })


    args_ridges<-reactive({
      req(input$rid_heigth)
      req(input$rid_y)
      data=getdata_descX()
      factors<-attr(data,"factors")

      req(input$ridge_variables%in%colnames(data))
      result<-try({



        x<-data<-data[,input$ridge_variables, drop=F]
        req(input$rid_y%in%colnames(factors))
        y<-factors[input$rid_y]
        data<-transform_module$server('rid_transf',data=x)
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
        #args<-readRDS("args_ridges.rds")
        args

      })
      req(!inherits(result,"try-error"))

      result
    })

    observeEvent(getdata_descX(),{
      data<-getdata_descX()
      factors<-attr(data,"factors")
      choices =rev(colnames(factors))
      selected=get_selected_from_choices(vals$cur_rid_y,choices)
      updatePickerInput(session,'rid_y',choices=choices,selected=selected)
    })

    observeEvent(input$rid_y,{
      vals$cur_rid_y<-input$rid_y
    })


    observeEvent(input$run_ridges,{
      args<-args_ridges()

      vals$rid_plot<-do.call(plot_ridges,args)
      runval$rid<-"btn_nice"
    })
    observeEvent(ignoreInit = T,input$rid_downp,{
      vals$hand_plot<-"Ridge plot"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,datalist_name=attr(getdata_descX(),"datalist"))})

  })
}

## Logic for tab 4 - Pair Plots
desctools_tab4<-list()
desctools_tab4$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(ns("box_setup1"),inline=F,show_tittle=F,
              color="#374061ff",
              title="Setup",
              div(
                div(class="setup_box picker-flex picker-before-x",

                    pickerInput_fromtop(ns("data_descX"),tipify_ui(span("Datalist:"),"Datalist for selecting the Factor"),choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T))))),
    column(4,class="mp0",
           style="height:  calc(100vh - 200px);overflow: auto",

           box_caret(ns("box_4a"),

                     title=strong("Variables & Panels"),
                     color="#c3cc74ff",
                     div(class="half-drop-inline",
                         div(
                           class="virtual-100",
                           div(span("Variables:",class='text_alert')),

                           virtualPicker(ns("ggpair.variables"),label=NULL,NULL,SelectedText="Variables selected")
                         ),



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
                           pickerInput_fromtop(ns("ggpair.y.variable"),strong("Grouping Factor:", class='text_alert'),NULL, options=shinyWidgets::pickerOptions(liveSearch =T))
                         ),
                         pickerInput_fromtop(ns("ggpair.method"), "Correlation method:", c("pearson", "kendall", "spearman", "none"))
                     )),
           box_caret(ns("box_4b"),
                     title = "General Options",
                     color="#c3cc74ff",
                     div(

                       pickerInput_fromtop_live(inputId = ns("fm_palette"),
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

                     div(class="run_pair_btn save_changes",
                         actionButton(ns("run_pair"),"RUN", icon=icon("fas fa-sync")),
                         shinycssloaders::withSpinner(uiOutput(ns("msp_pairs")),8)
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


    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected=vals$cur_pair_x
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'data_descX',choices=choices, selected=selected)
    })

    observeEvent(input$data_descX,{
      vals$cur_pair_x<-input$data_descX
    })
    getdata_descX<-reactive({
      req(input$data_descX)
      vals$saved_data[[input$data_descX]]
    })



    observeEvent(ignoreInit = T,input$fm_downplot4,{
      vals$hand_plot<-"generic_ggmatrix"
      generic=get_ggpair()
      message<-name_c<-"Pairs-plot"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,message=message, name_c=name_c,generic=generic,datalist_name=attr(getdata_descX(),"datalist"))
    })
    observeEvent(getdata_descX(),{
      data<-getdata_descX()
      req(length(data)>0)

      factors<-attr(data,"factors")
      choices=colnames(factors)
      updatePickerInput(session,'ggpair.y.variable',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observe(shinyjs::toggle("ggpair.y.variable",condition = isTRUE(yclude_y())))

    observeEvent(df_symbol$val,{
      updatePickerInput(session,'ggpair.pch', choices = df_symbol$val,
                        choicesOpt = list(content = df_symbol$img))
    })
    get_ggpair<-eventReactive(input$run_pair,ignoreInit = T,{
      shinyjs::removeClass("run_pair_btn","save_changes")
      args<-get_ggpair_args()
      class(args)=="iggpair"
      p<-do.call(gg_pairplot2,args)


      p
    })


    output$msp_pairs<-renderUI({
      req(get_ggpair())
      #req(input$msp_plot_width)
      #req(input$msp_plot_height)
      res<-div(


        renderPlot(get_ggpair()),
        em(attr(get_ggpair(),"row1"), style="color: gray")
      )
      # vals$show_ggrun<-F
      res
    })
    output$ggpair.title_corr<-renderUI({
      req(input$ggpair.method)
      textInput(ns("ggpair.title_corr"),"title_corr:",paste(input$ggpair.method,"corr"))
    })
    observeEvent(vals$newcolhabs,{
      selected<-get_selected_from_choices(vals$cur_ggpair_args$cur_fm_palette,vals$colors_img$val)

      updatePickerInput(session,'fm_palette', choices =     vals$colors_img$val,choicesOpt = list(content =vals$colors_img$img),selected=selected)
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
      selected=choices[1:3]
      if(!is.null(vals$cur_ggpair.variables)){
        if(all(vals$cur_ggpair.variables%in%choices)){
          selected=vals$cur_ggpair.variables
        }
      }
      shinyWidgets::updateVirtualSelect('ggpair.variables',choices=choices,selected=selected)


    })
    get_ggpair_args<-reactive({
      args<-try(silent = T,{
        req(getdata_descX())


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
        shinyjs::addClass("run_pair_btn","save_changes")

        args

      })

      req(!inherits(args,"try-error"))

      args

    })




  })
}

## Logic for tab 5 - Correlation plot
desctools_tab5<-list()
desctools_tab5$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(ns("box_setup1"),inline=F,show_tittle=F,
              color="#374061ff",
              title="Setup",
              div(
                div(class="setup_box picker-flex picker-before-x",

                    pickerInput_fromtop(ns("data_descX"),tipify_ui(span("Datalist:"),"Datalist for selecting the Factor"),choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T))))),
    column(4,class='mp0', style="height:  calc(100vh - 200px);overflow: auto",
           box_caret(
             ns("box_5aa"),
             title="Correlation",
             color="#c3cc74ff",
             div(

               div(style="display: flex",
                   pickerInput_fromtop(ns("cor_method"),choices = c("pearson", "kendall", "spearman"),tiphelp5("Correlation","Select the correlation coefficient to be computed."))


               ),
               div(
                 pickerInput_fromtop(ns('cutoff_hl'),tiphelp5("Filter correlations:","Choose the pair-wise absolute correlation cutoff."),choices = list("All" = "all", "Lower than" = "lower", "Higher than" = "higher")),
                 uiOutput(ns('corr_cutoff'))
               ),

               pickerInput_fromtop(ns("cor_use"),
                                   tiphelp5("Use","Select the method for computing covariances in the presence of missing values."),choices = c( "complete.obs","everything", "all.obs", "na.or.complete", "pairwise.complete.obs")))

           ),
           box_caret(
             ns("box_5a"),
             title="Plot Options",
             color="#c3cc74ff",
             div(
               div(
                 id=ns("heatmap_options"),
                 pickerInput_fromtop(ns("cor_dendogram"),tiphelp5("Dendogram","Choose whether to draw none, row, column, or both dendrograms."),choices = c("both","row","column","none")),
                 pickerInput_fromtop(ns("cor_scale"),tiphelp5("Scale","Indicate if the values should be centered and scaled in the row direction, column direction, or none."),
                                     choices = c("none","row", "column")),
                 pickerInput_fromtop(ns("cor_Rowv"),
                                     tiphelp5("Rowv","If TRUE, the dendrogram is computed and reordered based on row means."),choices = c('TRUE','FALSE')),
                 pickerInput_fromtop(ns("cor_Colv"),tiphelp5("Colv","<code>Rowv</code> means that columns should be treated identically to the rows. If <code>TRUEv</code>, the dendrogram is computed and reordered based on column means."),choices = c('Rowv',T,F)),
                 pickerInput_fromtop(ns("cor_revC"),tiphelp5("revC","Indicate if the column order should be reversed for plotting."),choices = c('TRUE','FALSE')),
                 pickerInput_fromtop(ns("cor_na.rm"),tiphelp5("na.rm","Indicate if NAs should be removed."),choices = c('TRUE','FALSE')),
                 pickerInput_fromtop(ns("cor_labRow"),tiphelp5("labRow","Choose whether to show observation labels."),choices = c('TRUE','FALSE')),
                 pickerInput_fromtop(ns("cor_labCol"),tiphelp5("labCol","Choose whether to show variable labels."),choices = c('TRUE','FALSE')),
                 pickerInput_fromtop(ns("cor_density.info"),tiphelp5("density.info","Indicate whether to superimpose a histogram, a density plot, or no plot on the color-key."),
                                     choices = c("histogram","density","none")),
                 pickerInput_fromtop_live(inputId = ns("cor_palette"),label = "Palette:",choices = NULL),
                 numericInput(ns("cor_mar_row"),"X margin",value = 5, min = 0, step = 1),
                 numericInput(ns("cor_mar_col"),"Y margin",value = 5, min = 0, step = 1),
                 numericInput(ns("cor_sepwidth_a"),tiphelp5("sep row width:","Set the space between rows."),value = 0.05, min = 0.1, max = 1, step = .01),
                 numericInput(ns("cor_sepwidth_b"),tiphelp5("sep col width:","Set the space between columns."),value = 0.05, min = 0.1, max = 1, step = .01),
                 colourpicker::colourInput(ns("cor_sepcolor"),label = tiphelp5("Sep color:","Choose the color between rows and columns."),value = "black", showColour = "background"),
                 colourpicker::colourInput(ns("cor_na.color"),label = tiphelp5("NA color:","Choose the color to use for missing value."),value = "gray", showColour = "background"),
                 pickerInput_fromtop(ns("cor_cellnote"),tiphelp5("Cell note","Choose whether to show correlation values as cell notes."),choices = c('TRUE','FALSE')),
                 colourpicker::colourInput(ns("cor_noteco"),label = tiphelp5("Note color:","Choose the color of the correlation value."),value = "black", showColour = "background"),
                 numericInput(ns("cor_notecex"),tiphelp5("Note size:","Set the size of the correlation value."),value = 1, step = 0.1),
                 div(),
                 div(
                   actionLink(ns('corr_down_results'), span("Download Results", icon("fas fa-table")), style = "button_active")
                 )

               ),
               div(
                 id=ns("corrplot_options"),
                 corrplot_module$ui(ns("module_corrplot"))
               )

             )

           )
    ),
    column(
      8,class='mp0',

      box_caret(
        ns('box_5b'),
        title="Correlation Plot",
        button_title2=
          radioGroupButtons(
            ns("corr_engenier"),NULL,
            c("heatmap","corrplot"),selected='heatmap'
          )
        ,button_title =actionLink(ns('corr_downp'), "Download",icon("download")),

        div(
          tabsetPanel(type="hidden",
                      id=ns('tab_corr'),
                      header=div(style="display: flex",
                                 div(id=ns("run_cor_btn"),class='save_changes',actionButton(ns("run_cor"),"RUN", icon=icon("fas fa-sync"))),
                                 hidden(div(id=ns("run_corrplot_btn"),class='save_changes',actionButton(ns("run_corrplot"),"RUN", icon=icon("fas fa-sync")))),
                                 hidden(div(id=ns("run_ggcorrplot_btn"),class='save_changes',actionButton(ns("run_ggcorrplot"),"RUN", icon=icon("fas fa-sync"))))
                      ),
                      tabPanel(
                        "heatmap",
                        div(
                          uiOutput(ns('corr_warning')),

                          uiOutput(ns("corr_plot"))
                        )

                      ),
                      tabPanel(
                        "corrplot",
                        div(
                          uiOutput(ns('corrplot'))
                        )

                      ),
                      tabPanel(
                        "ggcorrplot",
                        div(
                          uiOutput(ns('ggcorrplot'))
                        )

                      ))
        )


      )

    )



  )
}
desctools_tab5$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns



    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected=vals$cur_corr_x
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'data_descX',choices=choices, selected=selected)
    })

    observeEvent(input$data_descX,{
      vals$cur_corr_x<-input$data_descX
    })
    getdata_descX<-reactive({
      req(input$data_descX)
      vals$saved_data[[input$data_descX]]
    })


    box_caret_server("box_5a")
    box_caret_server("box_5b")

    observe({
      shinyjs::toggle('run_cor_btn',condition=input$tab_corr=="heatmap")
      shinyjs::toggle('run_corrplot_btn',condition=input$tab_corr=="corrplot")
      shinyjs::toggle('run_ggcorrplot_btn',condition=input$tab_corr=="ggcorrplot")
    })






    observeEvent(input$corr_engenier,{
      updateTabsetPanel(session,"tab_corr",selected=input$corr_engenier)
    })
    observe({
      shinyjs::toggle('heatmap_options',condition=input$corr_engenier=='heatmap')
      shinyjs::toggle('corrplot_options',condition=input$corr_engenier=='corrplot')


    })


    plot_corrplot_args<-reactive({
      cordata<-get_corrdata()
      corrplot_module$server('module_corrplot',cordata,vals)
    })

    observeEvent(plot_corrplot_args(),{
      shinyjs::addClass('run_corrplot_btn',"save_changes")
    })

    plot_corrplot<-eventReactive(input$run_corrplot,ignoreInit = T,{
      #print("run")
      args<-plot_corrplot_args()
      # print(head(args$corr))
      shinyjs::removeClass('run_corrplot_btn',"save_changes")
      title<-args$title
      args$title<-NULL
      mixed<-args$mixed
      args$mixed<-NULL


      if(mixed){

        args$col<-NULL
        args$method<-NULL
        args$diag<-NULL
        args$type<-NULL
        args$cl.pos<-NULL

        do.call(corrplot::corrplot.mixed,args)

      } else{
        do.call(corrplot:::corrplot,args)

      }






    })

    output$corrplot<-renderUI({


      renderPlot({
        plot_corrplot()
        vals$corr_plot<-recordPlot()
      })
    })

    corrplot_module$server_update('module_corrplot',vals)



    get_corrdata<-reactive({
      req(getdata_descX())
      req(input$cor_method)
      req(input$cor_use)
      req(input$cutoff_hl)

      args<-list(data=getdata_descX(),cor_method=input$cor_method,cor_cutoff=input$cor_cutoff,cor_use=input$cor_use,ret=input$cutoff_hl)

      cordata<-do.call(cordata_filter,args)
      cordata


    })

    output$corr_cutoff<-renderUI({
      req(input$cutoff_hl!="all")
      if(is.null(vals$cor_cutoff)){vals$cor_cutoff<-0.9}
      div(
        inline(numericInput(ns("cor_cutoff"),"Cutoff",value = vals$cor_cutoff,min = 0.1,max = 1,step = .1)),



      )
    })

    observeEvent(input$cor_cutoff,{
      vals$cor_cutoff<-input$cor_cutoff
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
      selected<-get_selected_from_choices(vals$cur_corr_args$cur_cor_palette,vals$colors_img$val)


      updatePickerInput(session,'cor_palette', choices = vals$colors_img$val,
                        choicesOpt = list(content = vals$colors_img$img),selected=selected)
    })
    observeEvent(ignoreInit = T,input$corr_down_results,{
      vals$hand_down<-"Corr result"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)

    })
    observeEvent(ignoreInit = T,input$corr_downp,{
      if(input$corr_engenier=="corrplot"){

        vals$hand_plot<-"generic_replay"
        generic=vals$corr_plot
        message<-"corrplot"
        name_c<-'corrplot'
        datalist_name=attr(getdata_descX(),"datalist")
        module_ui_figs("downfigs")
        mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,message=message, name_c=name_c,generic=generic,datalist_name=datalist_name)
        return()
      } else if(input$corr_engenier=="ggcorrplot"){
        vals$hand_plot<-"generic_gg"
        generic=ggplot_corrplot()
        message<-"ggcorrplot"
        name_c<-'ggcorrplot'
        datalist_name=attr(getdata_descX(),"datalist")
        module_ui_figs("downfigs")
        mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,message=message, name_c=name_c,generic=generic,datalist_name=datalist_name)
        return()
      }
      vals$hand_plot<-"Correlation Plot"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,datalist_name=attr(getdata_descX(),"datalist"))

    })


    output$corr_warning<-renderUI({

      req(cor_war())
      div(style="padding: 5px; font-size:11px",
          class = "alert_warning",
          icon("triangle-exclamation",style="color: Dark yellow3"),
          strong("Note:"),cor_war()
      )

    })
    cor_war<-reactiveVal()
    observeEvent(input$run_cor,ignoreInit = T,{
      args<-args_corrplot()
      cor_war(attr(get_corrdata(),"war"))


      corplot_val(args)
      shinyjs::removeClass("run_cor_btn",'save_changes')
    })
    observeEvent(args_corrplot(),{
      shinyjs::addClass("run_cor_btn",'save_changes')
    })





  })
}
corrplot_module<-list()
corrplot_module$ui<-function(id){
  ns<-NS(id)

  div(
    checkboxInput(ns('mixed'),span('Mixed',tipright("whether use mixed methods to visualize the correlation matrix")),TRUE),
    pickerInput(ns('lower'),span('lower',tipright("lower visualization method")),choices=c("circle", "square", "ellipse", "number", "shade", "color", "pie"),selected="number"),
    pickerInput(ns('upper'),span('upper',tipright("upper visualization method")),choices=c("circle", "square", "ellipse", "number", "shade", "color", "pie"),selected="circle"),
    pickerInput_fromtop_live(
      inputId = ns('lower.col'),
      label = 'Lower palette:',
      choices="RdBu",
      selected='RdBu'),
    pickerInput_fromtop_live(
      inputId = ns('upper.col'),
      label = 'Upper palette:',
      choices="RdBu",
      selected='RdBu'),

    pickerInput(ns('method'),span('method',tipright("visualization method")),choices=c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
    pickerInput_fromtop_live(
      inputId = ns('col'),
      label = 'Palette:',
      choices="RdBu",
      selected='RdBu'),
    pickerInput(ns('type'),'type',choices=c("full", "lower", "upper")),
    colourpicker::colourInput(ns('bg'),'bg',value="white"),
    textInput(ns("title"),"title",""),
    checkboxInput(ns('diag'),span('diag',tipright("whether display the correlation coefficients on the principal diagonal")),TRUE),
    checkboxInput(ns('outline'),span('outline',tipright("whether plot outline")),FALSE),
    pickerInput(ns('order'),span('order',tipright("<p>The ordering method of the correlation matrix.<li><strong>original</strong>for original order (default).</li><li><strong>AOE</strong>for the angular order of the eigenvectors.</li><li><strong>FPC</strong>for the first principal component order.</li><li><strong>hclust</strong>for the hierarchical clustering order.</li><li><strong>alphabet</strong>for alphabetical order.</li></p>")),choices=c("original", "AOE", "FPC", "hclust", "alphabet")),
    pickerInput(ns('hclust.method'),span('hclust.method',tipright('the agglomeration method to be used when order is hclust')),choices=c("complete", "ward", "ward.D", "ward.D2", "single", "average","mcquitty", "median", "centroid")),
    numericInput(ns('addrect'),span('addrect',tipright(" the number of rectangles draws on the graph according to the hierarchical cluster, only valid when order is hclust.")),NA),
    colourpicker::colourInput(ns('rect.col'),span('rect.col',tipright('
Color for rectangle border(s), only valid when addrect is equal or greater than 1')),value="black"),
    numericInput(ns('rect.lwd'),span('rect.lwd',tipright(' line width for borders for rectangle border(s), only valid when addrect is equal or greater than 1')),2),

    pickerInput(ns('tl.pos'),span('tl.pos',tipright('position of text labels')),
                choices=c("default",
                          'left'='l',
                          'topleft'='lt',

                          "diagonal"='d' ,
                          "None"='n'
                )),

    numericInput(ns('tl.cex'),span('tl.cex',tipright("size of text label (variable names)")),1),
    colourpicker::colourInput(ns('tl.col'),span('tl.col',tipright("The color of text label")),value="red"),
    numericInput(ns('tl.offset'),span('tl.offset',tipright("text label offset")),0.4),
    numericInput(ns('tl.srt'),span('tl.srt',tipright('text label rotation in degrees')),90),
    pickerInput(ns('cl.pos'),span('cl.pos',tipright("position of color-legend")),choices=c("default","right"="r", "bottom"="b", "None"="n")),
    numericInput(ns('cl.length'),span('cl.length',tipright("the number of number-text in color-legend")),NA),
    numericInput(ns('cl.cex'),span('cl.cex',tipright("text size of number-label in color-legend")),0.8),
    numericInput(ns('cl.ratio'),span('cl.ratio',tipright("<p>Numeric, to justify the width of color-legend, 0.1~0.2 is suggested.</p>")),0.15),
    pickerInput(ns('cl.align.text'),
                span('cl.align.text',tipright("alignment for number-label in color-legend")),
                choices=c(
                  "center"="c",
                  "left"="l",
                  "right"="r"
                )),
    numericInput(ns('cl.offset'),span('cl.offset',tipright("offset for number-label in color-legend")),0.5),
    numericInput(ns('number.cex'),span('number.cex',tipright("text size for correlation coefficients")),1),
    numericInput(ns('number.font'),span('number.font',tipright("text font for correlation coefficients")),2),
    numericInput(ns('number.digits'),span('number.digits',tipright("indicating the number of decimal digits to be added into the plot")),NA),
    pickerInput(ns('addshade'),span('addshade',tipright('shade style')),choices=c("negative", "positive", "all")),
    numericInput(ns('shade.lwd'),span('shade.lwd',tipright("line width of shade")),1),
    colourpicker::colourInput(ns('shade.col'),span('shade.col',tipright("color of shade line")),value="white"),
    numericInput(ns('sig.level'),span('sig.level',tipright("
Significant level,")),0.05),
    pickerInput(ns('insig'),span('insig',tipright('Character, specialized insignificant correlation coefficients')),choices=c("pch", "p-value", "blank", "n", "label_sig")),
    pickerInput(inputId = ns("pch"),
                label = span("pch:",tipright("<p>Add symbol on the glyphs of insignificant correlation coefficients (only valid when insig is \\'pch\\')</p>")),
                choices = df_symbol$val,
                choicesOpt = list(content = df_symbol$img),
                selected=4,
                width = "100px"),
    colourpicker::colourInput(ns('pch.col'),span('pch.col',tipright("<p>The color of pch (only valid when insig is \\'pch\\')</p>")),value="black"),
    numericInput(ns('pch.cex'),span('pch.cex',tipright("<p>The cex of pch (only valid when insig is \\'pch\\')</p>")),3),
    #pickerInput(ns('plotCI'),span('plotCI',tipright("<p>method of ploting confidence interval. If \\'n\\', don\\'t plot confidence interval. If \\'rect\\', plot rectangles whose upper side means upper bound and lower side means lower bound, respectively. If \\'circle\\', first plot a circle with the bigger absolute bound, and then plot the smaller. Warning: if the two bounds are the same sign, the smaller circle will be wiped away, thus forming a ring. Method \\'square\\' is similar to \\'circle\\'.</p>")),choices=c("n", "square", "circle", "rect")),
    textInput(ns('na.label'),span('na.label',tipright("<p>Label to be used for rendering NA cells. Default is \\'?\\'. If \\'square\\', then the cell is rendered as a square with the na.label.col color</p>")),"?"),
    colourpicker::colourInput(ns('na.label.col'),span('na.label.col',tipright("Color used for rendering NA cells.")),value="black"),
    numericInput(ns('win.asp'),span('win.asp',tipright("Aspect ration for the whole plot.")),1)
  )


}
corrplot_module$server<-function(id,corr,vals){
  moduleServer(id,function(input,output,session){

    get_args<-reactive({
      req(input$col%in%names(vals$newcolhabs))

      args=list(
        mixed=input$mixed,
        corr=corr,
        col=vals$newcolhabs[[input$col]](256),
        method=input$method,
        type=input$type,
        title=input$title,
        diag=input$diag,
        outline=input$outline,
        order=input$order,
        hclust.method=input$hclust.method,
        addrect=if(is.na(input$addrect)){NULL} else{input$addrect},
        bg=input$bg,

        rect.col=input$rect.col,
        rect.lwd=input$rect.lwd,
        tl.pos=if(input$tl.pos=="default"){NULL} else{input$tl.pos},
        tl.cex=input$tl.cex,
        tl.col=input$tl.col,
        tl.offset=input$tl.offset,
        tl.srt=input$tl.srt,
        cl.pos=if(input$cl.pos=="default"){NULL} else{input$cl.pos},
        cl.length=if(is.na(input$cl.length)){NULL} else{input$cl.length},
        cl.cex=input$cl.cex,
        cl.ratio=input$cl.ratio,
        cl.align.text=input$cl.align.text,
        cl.offset=input$cl.offset,
        number.cex=input$number.cex,
        number.font=input$number.font,
        number.digits=if(is.na(input$number.digits)){NULL} else{input$number.digits},

        addshade=input$addshade,
        shade.lwd=input$shade.lwd,
        shade.col=input$shade.col,
        sig.level=input$sig.level,
        insig=input$insig,
        pch=input$pch,
        pch.col=input$pch.col,
        pch.cex=input$pch.cex,
        plotCI=input$plotCI,
        na.label=input$na.label,
        na.label.col=input$na.label.col,
        win.asp=input$win.asp

      )
      if(input$mixed){
        args$lower<-input$lower
        args$upper<-input$upper
        args$lower.col<-vals$newcolhabs[[input$lower.col]](256)
        args$upper.col<-vals$newcolhabs[[input$upper.col]](256)
      }


      args
    })
    return(get_args())








  })
}
corrplot_module$server_update<-function(id,vals){
  moduleServer(id,function(input,output,session){




    observeEvent(vals$newcolhabs,{
      choices =     vals$colors_img$val
      choicesOpt = list(content =vals$colors_img$img)

      selected<-get_selected_from_choices(vals$cur_corrplot_args$cur_col,vals$colors_img$val)
      updatePickerInput(session,"col",choices=choices,choicesOpt=choicesOpt,selected=selected)
      selected<-get_selected_from_choices(vals$cur_corrplot_args$cur_lower.col,vals$colors_img$val)
      updatePickerInput(session,"lower.col",choices=choices,choicesOpt=choicesOpt,selected='RdBu')
      selected<-get_selected_from_choices(vals$cur_corrplot_args$cur_upper.col,vals$colors_img$val)
      updatePickerInput(session,"upper.col",choices=choices,choicesOpt=choicesOpt,selected='RdBu')
    })








    observe({




      shinyjs::toggle('col',condition=isFALSE(input$mixed))
      shinyjs::toggle('method',condition=isFALSE(input$mixed))
      shinyjs::toggle('type',condition=isFALSE(input$mixed))
      shinyjs::toggle('diag',condition=isFALSE(input$mixed))
      shinyjs::toggle('cl.pos',condition=isFALSE(input$mixed))
      shinyjs::toggle('lower.col',condition=isTRUE(input$mixed))
      shinyjs::toggle('upper.col',condition=isTRUE(input$mixed))
      shinyjs::toggle('upper',condition=isTRUE(input$mixed))
      shinyjs::toggle('lower',condition=isTRUE(input$mixed))
      shinyjs::toggle('addrect',condition=input$order=="hclust")
      shinyjs::toggle('rect.lwd',condition=!is.na(input$addrect))
      shinyjs::toggle('rect.col',condition=!is.na(input$addrect))
    })

    return(NULL)

  })
}

## Logic for tab 6 - MDS
desctools_tab6<-list()
desctools_tab6$ui<-function(id){
  ns<-NS(id)
  div(
    column(12,class="mp0",
           box_caret(ns("box_setup1"),inline=F,show_tittle=F,
                     color="#374061ff",
                     title="Setup",
                     div(
                       div(class="setup_box picker-flex picker-before-x",

                           pickerInput_fromtop(ns("data_descX"),tipify_ui(span("Datalist:"),"Datalist for selecting the Factor"),choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T)))))),
    column(12,class="mp0",



           column(4,class="mp0",
                  box_caret(ns("box_6a"),title="Distance",
                            color="#c3cc74ff",
                            div(style="display: flex",
                                pickerInput_fromtop(ns("mds_distance"),# id: mds_distance
                                                    span("Distance:", tiphelp6("Select the distance metric for calculating dissimilarities in the MDS analysis.")),choices = c('bray', "euclidean", 'jaccard')),
                                uiOutput(ns("mds_button"))
                            )),

                  div(id=ns("options_plot"),
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
                        div(checkboxInput(ns('mds_points') ,tiphelp5(strong("Scores - Points"),"Display mds scores as points"),value=T),
                            div(
                              id=ns('mds_points_out'),
                              pickerInput_fromtop_live(
                                inputId = ns('mds_points_palette'),
                                label = tiphelp5("Palette","Choose a gradient to represent colors based on the selected factor"),
                                choices =     NULL
                              ),
                              pickerInput_fromtop(ns('mds_points_factor')  ,"Factor:",choices=NULL,selected= NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),
                              pickerInput_fromtop(inputId = ns("mds_points_shape"),
                                                  label = "Shape:",
                                                  choices = df_symbol$val,
                                                  choicesOpt = list(content = df_symbol$img),
                                                  selected=16,
                                                  width = "100px")
                            ),
                            checkboxInput(ns('mds_scale_shape') ,tiphelp5("Scale Shape", "Use different shapes to represent points based on the selected factor"),value=F),
                            numericInput (ns('mds_points_size') ,"Size:",value=2)
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
                  div(
                    id=ns('options_summary'),

                    box_caret(
                      ns("box_6f"),title="Options",
                      color="#c3cc74ff",
                      div(
                        pickerInput_fromtop(ns("show_mds_results"),"Show Scores:", list("Sites"="sites","Species"="species"))
                      )
                    )
                  )
           ),

           column(8,class="mp0",
                  box_caret(
                    ns("box_6e"),title="Results",

                    button_title = div(
                      actionLink(
                        ns('mds_downp'),span("Download",icon("fas fa-download"))),
                      actionLink(
                        ns('down_mds_results'),span("Download",icon("fas fa-table")))
                    ),
                    button_title2=radioGroupButtons(ns("mds_results"),NULL,c("Plot"="plot","Summary"="summary")),
                    tabsetPanel(
                      id=ns("mds_view"),
                      type="hidden",
                      tabPanel("plot",
                               uiOutput(ns("plot_mds")),
                      ),
                      tabPanel(
                        "summary",
                        uiOutput(ns("summary_mds"))
                      )
                    )
                  )),


    )


  )
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
    observe({
      shinyjs::toggle('mds_downp',condition=input$mds_results=="plot")
      shinyjs::toggle('down_mds_results',condition=input$mds_results=="summary")
      shinyjs::toggle('options_plot',condition=input$mds_results=="plot")
      shinyjs::toggle('options_summary',condition=input$mds_results=="summary")
    })

    observeEvent(input$mds_results,{
      updateTabsetPanel(session,"mds_view",selected=input$mds_results)
    })
    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected=vals$cur_mds_x
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'data_descX',choices=choices, selected=selected)
    })

    observeEvent(input$data_descX,{
      vals$cur_mds_x<-input$data_descX
    })
    getdata_descX<-reactive({
      req(input$data_descX)
      vals$saved_data[[input$data_descX]]
    })
    runval<-reactiveValues(mds="save_changes_nice")

    output$mds_button<-renderUI({

      div(class=vals$mds_btn_class,
          actionButton(ns("run_mds"),"RUN")
      )
    })

    output$summary_mds<-renderUI({
      req(!is.null(vals$mds))
      vals$mmds_summaryvals$mds_results<-data.frame(vegan::scores(vals$mds,input$show_mds_results))
      div(class="half-drop-inline",
          fixed_dt(vals$mmds_summaryvals$mds_results)
      )
    })
    observe({
      shinyjs::toggle("mds_points_out",condition = isTRUE(input$mds_points))
    })

    observe({
      choices =vals$colors_img$val
      choicesOpt = list(content =vals$colors_img$img)
      updatePickerInput(session,'mds_points_palette', choices=choices,choicesOpt=choicesOpt)
    })

    observeEvent(input$mds_points_factor,{
      vals$cur_mds_points_factor<-input$mds_points_factor
    })

    observeEvent(attr(getdata_descX(),"factors"),{
      choices=colnames(attr(getdata_descX(),"factors"))
      selected=get_selected_from_choices(vals$cur_mds_points_factor,choices)
      updatePickerInput(session,'mds_points_factor', choices=choices,selected=selected)
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
    mds_points_factor<-reactive({
      req(length(input$mds_points)>0)
      if(isTRUE(input$mds_points)){
        req(input$mds_points_factor)
        data<-getdata_descX()
        factors<-attr(data,"factors")
        factors[rownames(data),input$mds_points_factor, drop=F]
      } else{NULL}

    })
    mds_text_factor<-reactive({
      req(length(input$mds_text)>0)
      if(isTRUE(input$mds_text)){
        req(input$mds_text_factor)
        data<-getdata_descX()
        factors<-attr(data,"factors")
        factors[rownames(data),input$mds_text_factor, drop=F]
      } else{ NULL}

    })
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

      renderPlot({
        plot_mds()
      })
    })

    observeEvent(list(getdata_descX(),input$mds_distance),ignoreInit = T,{
      vals$mds<-NULL
    })

    plot_mds<-reactive({
      if(is.null(vals$mds)){
        return(NULL)
      }
      args<-get_mds_ggplot()
      do.call(ggmds,args)
    })

    observeEvent(ignoreInit = T,input$mds_downp,{
      vals$hand_plot<-"generic_gg"
      generic=plot_mds()
      message<-"MDS"
      name_c<-paste0("MDS-",input$mds_distance)
      datalist_name=attr(getdata_descX(),"datalist")
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,message=message, name_c=name_c,generic=generic,datalist_name=datalist_name)

    })


    observeEvent(input$run_mds,{
      req(input$mds_distance)

      validate(need(!anyNA(getdata_descX()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
      withProgress(message="Running...",
                   min=NA,max=NA,{
                     mds<-try(vegan::metaMDS(getdata_descX(), distance = input$mds_distance))
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

## Logic for tab 7 - PCA
desctools_tab7<-list()
desctools_tab7$ui<-function(id){
  ns<-NS(id)
  div(
    column(12,class="mp0",
           box_caret(ns("box_setup1"),inline=F,show_tittle=F,
                     color="#374061ff",
                     title="Setup",
                     div(
                       div(class="setup_box picker-flex picker-before-x",

                           pickerInput_fromtop(ns("data_descX"),tipify_ui(span("Datalist:"),"Datalist for selecting the Factor"),choices = NULL, options=shinyWidgets::pickerOptions(liveSearch =T)))))),
    div(
      column(4,class="mp0",style="height:  calc(100vh - 200px);overflow: auto",
             id=ns("options_plot"),
             box_caret(ns("box_7a"),title="General Options",
                       color="#c3cc74ff",
                       div(
                         div(
                           numericInput(ns('pca_base_size') ,"Base size:",value=12),
                           pickerInput_fromtop(ns('pca_theme') ,"Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
                           textInput(ns('pca_title') ,"Title:",value="Principal Component analysis")
                         )

                       )
             ),
             box_caret(ns("box_7axes"),title="Score Axes",
                       color="#c3cc74ff",
                       hide_content = T,
                       div(
                         numericInput(ns("pca_expandX"),"Expand X Axis",value=0.1,step=0.05),
                         numericInput(ns("pca_expandY"),"Expand Y Axis",value=0.1,step=0.05),
                         checkboxInput(ns('pca_show_intercept') ,"Intercepts",value=T),
                         textInput(ns("xlab"), "xlab:", "PC I"),
                         textInput(ns("ylab"), "ylab:", "PC II"),
                         checkboxInput(ns("show_axis_explain"),"Show Explained Variance",T)

                       )
             ),
             box_caret(ns("box_7b"),
                       title=span(style="display: inline-block",
                                  class="checktitle",

                                  checkboxInput(ns("pca_points") ,label =strong("Points"),T,width="150px")
                       ),
                       color="#c3cc74ff",div(
                         id=ns("pca_points_out"),

                         pickerInput_fromtop_live(ns('pca_points_palette') ,tiphelp5("Palette","Choose a gradient to represent colors based on the selected factor"),"turbo"),
                         pickerInput_fromtop(ns('pca_points_factor')  ,"Factor:",choices=NULL),
                         pickerInput_fromtop(inputId = ns("pca_points_shape"),
                                             label = "Shape:",
                                             choices = df_symbol$val,
                                             choicesOpt = list(content = df_symbol$img),
                                             selected=16,

                                             width = "100px"),
                         checkboxInput(ns('pca_scale_shape') ,tiphelp5("Scale Shape", "Use different shapes to represent points based on the selected factor"),value=F),
                         numericInput (ns('pca_points_size') ,"Size:",value=2),
                         checkboxInput(ns("show_points_legend"),"Show legend",T)



                       )),
             box_caret(ns("box_7c"),
                       title=span(style="display: inline-block",
                                  class="checktitle",

                                  checkboxInput(ns("pca_text") ,label =strong("Text"),F,width="150px")
                       ),
                       color="#c3cc74ff",div(
                         id=ns('pca_text_out'),
                         pickerInput_fromtop_live(ns('pca_text_palette') ,"Palette:","gray"),
                         pickerInput_fromtop_live(ns('pca_text_factor') ,"Factor:",choices=NULL,selected=NULL),

                         numericInput(ns('pca_text_size') ,"Size:",value=4),

                       )),
             box_caret(ns("box_7d"),
                       title=span(style="display: inline-block",
                                  class="checktitle",

                                  checkboxInput(ns("pca_biplot") ,label =strong("Biplot"),T,width="150px")
                       ),
                       color="#c3cc74ff",div(
                         id=ns('pca_biplot_out'),

                         numericInput(ns('pca_biplot_n') ,tiphelp5("Number of Variables", "Set the number of variables to display"),value=10, min=1),
                         div(colourpicker::colourInput(ns('pca_biplot_color') ,"Text Color",value="blue","background")),
                         div(colourpicker::colourInput(ns('pca_biplot_arrow_color') ,"Arrow Color",value="blue","background")),
                         numericInput(ns('pca_biplot_size') ,"Text Size",value=4),
                         checkboxInput(ns('pca_loading_axis'),"Axes",T),
                         checkboxInput(ns('pca_lo_axis_color'),"Axes color as Arrows",T),
                         textInput(ns('pca_lo_x.text'),"xlab","PC1 loadings"),
                         textInput(ns('pca_lo_y.text'),"ylab","PC2 loadings")




                       ))



      ),
      column(4,class="mp0",
             id=ns("options_summary"),
             box_caret(ns("box7_f"),
                       color="#c3cc74ff",
                       title="Options",
                       div(


                         pickerInput_fromtop(ns("show_pca_results"),"Show result:", c('Importance','Scores',"Standard deviations","Rotation","Centering","Scaling")),style="width: var(--parentHeight);"



                       )
             )),
      column(8,
             class='mp0',
             box_caret(
               ns("box_7e"),
               title="Results",
               button_title =  div(
                 actionLink(ns('pca_downp'),span("Download Plot",icon("fas fa-download"))),
                 actionLink(ns('down_pca_results'),span("Download",icon("fas fa-table")))
               ),
               button_title2=radioGroupButtons(ns("pca_results"),NULL,c("Plot"="plot","Summary"="summary")),

               div(

                 actionButton(ns("run_pca"),"RUN", icon=icon("fas fa-sync")),
                 tabsetPanel(
                   id=ns('pca_options'),
                   type="hidden",

                   tabPanel(
                     "plot",
                     uiOutput(ns("plot_pca"))
                   ),
                   tabPanel(
                     "summary",
                     uiOutput(ns('summary_pca'))

                   )
                 )
               )
             ))



    )


  )
}
desctools_tab7$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns


    observeEvent(input$pca_results,ignoreInit = T,{
      updateTabsetPanel(session,"pca_options",selected=input$pca_results)
    })
    observe({
      shinyjs::toggle('options_plot',condition=input$pca_results=="plot")
      shinyjs::toggle('options_summary',condition=input$pca_results=="summary")
      shinyjs::toggle('pca_downp',condition=input$pca_results=="plot")
      shinyjs::toggle('down_pca_results',condition=input$pca_results=="summary")
    })
    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected=vals$cur_pca_x
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'data_descX',choices=choices, selected=selected)
    })

    observeEvent(input$data_descX,{
      vals$cur_pca_x<-input$data_descX
    })
    getdata_descX<-reactive({
      req(input$data_descX)
      vals$saved_data[[input$data_descX]]
    })


    runval<-reactiveValues(pca="save_changes_nice")

    box_caret_server("box_7a")
    box_caret_server("box_7axes",hide_content=T)

    box_caret_server("box_7b")
    box_caret_server("box_7c")
    box_caret_server("box_7d")
    box_caret_server("box_7e")
    box_caret_server("box_7f")
    box_caret_server("box_7g")

    observeEvent(getdata_descX(),{
      choices=colnames(attr(getdata_descX(),"factors"))
      selected=get_selected_from_choices(vals$cur_pca_points_factor,choices)
      updatePickerInput(session,'pca_points_factor'  ,choices=choices,selected= selected)
    })

    observeEvent(getdata_descX(),{
      choices=colnames(attr(getdata_descX(),"factors"))
      selected=get_selected_from_choices(vals$cur_pca_text_factor,choices)
      updatePickerInput(session,'pca_text_factor'  ,choices=choices,selected= selected)
    })

    observeEvent(input$pca_text_factor,{
      vals$cur_pca_text_factor<-input$pca_text_factor
    })



    observe({
      shinyjs::toggle('pca_points_out',condition = isTRUE(input$pca_points))
      shinyjs::toggle('pca_text_out',condition = isTRUE(input$pca_text))
      shinyjs::toggle("pca_biplot_out",condition = isTRUE(input$pca_biplot))

    })



    observeEvent(vals$newcolhabs,{
      choices =vals$colors_img$val
      choicesOpt = list(content =vals$colors_img$img)

      selected<-get_selected_from_choices(vals$cur_pca_args$cur_pca_text_palette,vals$colors_img$val)


      updatePickerInput(session,'pca_text_palette', choices=choices,choicesOpt=choicesOpt,selected=selected)


      selected<-get_selected_from_choices(vals$cur_pca_args$cur_pca_points_palette,vals$colors_img$val)

      updatePickerInput(session,'pca_points_palette', choices=choices,choicesOpt=choicesOpt,selected=selected)

    })

    observeEvent(input$pca_points_factor,{
      vals$cur_pca_points_factor<-input$pca_points_factor
    })


    observeEvent(getdata_descX(),{
      value=ncol(getdata_descX())
      updateNumericInput(session,'pca_biplot_n',value=value)
    })


    pca_points_factor<-reactive({
      req(length(input$pca_points)>0)
      if(isTRUE(input$pca_points)){
        req(input$pca_points_factor)
        data<-getdata_descX()
        factors<-attr(data,"factors")
        req(input$pca_points_factor%in%colnames(factors))
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




    observeEvent(input$pca_points_palette,{
      pal<-vals$newcolhabs[[input$pca_points_palette]](10)
      if(pal[1]==pal[2]){
        shinyjs::hide("pca_points_factor")
      } else{
        shinyjs::show("pca_points_factor")
      }


    })



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
        expandY=input$pca_expandY,
        points_legend=input$show_points_legend,
        xlab=input$xlab,
        ylab=input$ylab,
        show_axis_explain=input$show_axis_explain
      )
    })

    get_plot_pca<-reactive({
      args<-args_pca()
      do.call(ggpca,args)
    })

    output$plot_pca<-renderUI({

      div(
        renderPlot({
          get_plot_pca()
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
      div(
        class=runval$pca,

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
      req(!is.null(get_plot_pca()))
      runval$pca<-"save_changes_nice"
    })



    observeEvent(ignoreInit = T,input$down_pca_results,{
      vals$hand_down<-"PCA result"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)

    })





    observeEvent(ignoreInit = T,input$pca_downp,{
      vals$hand_plot<-"generic_gg"
      generic=get_plot_pca()
      message<-name_c<-"PCA"
      datalist_name=attr(getdata_descX(),"datalist")
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,message=message, name_c=name_c,generic=generic,datalist_name=datalist_name)
    })


    observeEvent(input$run_pca,ignoreInit = T,{
      pca<-try(prcomp(getdata_descX(), center=F, scale=F))
      if(!inherits(pca,"try-error"))
        pca_model(pca)
      runval$pca<-"btn_nice"
    })



    return(NULL)


  })
}

## Logic for tab 8 - RDA
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
                              div(class="setup_box picker-flex picker-before-y",pickerInput_fromtop(ns("rda_X"),"", choices=NULL, options=shinyWidgets::pickerOptions(liveSearch =T))

                              ),
                          ),
                          div(style="height: 30px; margin-top: 20px",
                              div(strong("~")),
                              div(actionLink(ns("rev_rda"),icon("arrow-right-arrow-left")))),
                          div(style="display: flex;",
                              div(
                                class="setup_box picker-flex picker-before-x", pickerInput_fromtop(ns("rda_Y"),"", choices=NULL, options=shinyWidgets::pickerOptions(liveSearch =T))
                              )
                          ),
                          div(style="margin-top: 20px",
                              uiOutput(ns('rda_btn'))
                          )

                      ),
                      div(
                        style="",
                        checkboxInput(ns("rda_scale"),span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=F))

                    )
          )
  ),
  column(4,class="mp0",style="height:  calc(100vh - 200px);overflow: auto",
         id=ns("options_plot"),
         box_caret(
           ns("box_8a"),title="General options",
           color="#c3cc74ff",
           div(

             numericInput(ns('rda_base_size') ,"Base size:",value=12),
             pickerInput_fromtop(ns('rda_theme') ,"Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
             textInput(ns('rda_title') ,"Title:",value="Redundancy analysis")



           )
         ),
         box_caret(ns("box_8axes"),title="Score Axes",
                   color="#c3cc74ff",
                   hide_content = T,
                   div(
                     numericInput(ns("rda_expandX"),"Expand X Axis",value=0.1,step=0.05),
                     numericInput(ns("rda_expandY"),"Expand Y Axis",value=0.1,step=0.05),
                     checkboxInput(ns('rda_show_intercept') ,"Intercepts",value=T),
                     textInput(ns("xlab"), "xlab:", "RDA I"),
                     textInput(ns("ylab"), "ylab:", "RDA II"),
                     checkboxInput(ns("show_axis_explain"),"Show Explained Variance",T)

                   )
         ),

         box_caret(
           ns("box_8b"),
           color="#c3cc74ff",
           title=span(style="display: inline-block",
                      class="checktitle",
                      tip="Display Predictor Scores as Points",
                      checkboxInput(ns("rda_points") ,label =strong("Points"),T,width="150px")
           ),
           div(

             div(
               id=ns('rda_points_out'),

               pickerInput_fromtop_live(ns('rda_points_palette') ,tiphelp5("Palette","Choose a gradient to represent colors based on the selected factor"),NULL),
               pickerInput_fromtop(ns('rda_points_factor')  ,"Factor:",choices="turbo"),
               pickerInput_fromtop(inputId = ns("rda_points_shape"),
                                   label = "Shape:",
                                   choices = df_symbol$val,
                                   choicesOpt = list(content = df_symbol$img),
                                   selected=16,

                                   width = "100px"),
               numericInput (ns('rda_points_size') ,"Size:",value=2),
               checkboxInput(ns("show_points_legend"),"Show legend",T),
               textInput(ns("rda_legend_points"),"Legend title:","")

             )


           )

         ),

         box_caret(
           ns("box_8c"),
           title=span(style="display: inline-block",
                      class="checktitle",
                      tip="Display Predictor Scores as Text",
                      checkboxInput(ns("rda_text") ,label =strong("Text"),F,width="150px")
           ),

           color="#c3cc74ff",
           div(

             div(id=ns('rda_text_out'),
                 pickerInput_fromtop_live(ns('rda_text_palette') ,"Palette:","gray"),
                 pickerInput_fromtop_live(ns('rda_text_factor') ,"Factor:",choices=NULL,selected=NULL),

                 numericInput(ns('rda_text_size') ,"Size:",value=4),
             ))
         ),

         box_caret(
           ns("box_8d"),
           color="#c3cc74ff",
           title=span(style="display: inline-block",
                      class="checktitle",
                      tip=NULL,
                      checkboxInput(ns("rda_biplot") ,label =strong("Predictor Scores"),T,width="200px")
           ),
           div(
             div(
               id=ns('rda_biplot_out'),
               numericInput(ns('rda_biplot_n') ,tiphelp5("Number of Variables", "Set the number of variables to display"),value=10, min=1),
               div(colourpicker::colourInput(ns('rda_biplot_color') ,"Text Color",value="blue","background")),
               div(colourpicker::colourInput(ns('rda_biplot_arrow_color') ,"Arrow Color",value="blue","background")),

               numericInput(ns('rda_biplot_size') ,"Text Color",value=4)

             )
           )

         ),
         box_caret(
           ns("box_8e"),
           color="#c3cc74ff",
           title=span(style="display: inline-block",
                      class="checktitle",
                      tip=NULL,
                      checkboxInput(ns("rda_species") ,label =strong("Response Scores"),T,width="200px")
           ),
           div(
             div(
               id=ns("rda_species_out"),
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
               numericInput(ns('rda_species_n') ,"sp number:",value=10),

               numericInput(ns('rda_species_size') ,"Size:",value=2),
               checkboxInput(ns("show_response_legend"),"Show legend",T),
               textInput(ns("rda_legend_response"),"Legend title:","Response")
             )
           ))

  ),
  column(4,class="mp0",
         id=ns("options_summary"),
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
           ns("box_8f"),title="Results",

           button_title = div(
             actionLink(ns('rda_downp'),span("Download Plot",icon("fas fa-download"))),
             actionLink(ns('downcenter_rda'),span("Download",icon("fas fa-download")))
           ),
           button_title2=radioGroupButtons(ns("rda_results"),NULL,c("Plot"="plot","Summary"="summary")),
           div(
             tabsetPanel(
               id=ns("rda_view"),
               type="hidden",
               tabPanel(
                 "8.1. Plot",value="plot",
                 div(
                   # actionLink(ns('save_bug'),"save_bug"),
                   uiOutput(ns("rda_plot"))


                 )
               ),
               tabPanel("8.2. Summary",value="summary",
                        uiOutput(ns("rda_print")))

             )
           )
         ))



  )
}
desctools_tab8$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    observeEvent(input$rda_results,{
      updateTabsetPanel(session,'rda_view',selected=input$rda_results)
    })
    observe({
      shinyjs::toggle('options_plot',condition=input$rda_results=="plot")
      shinyjs::toggle('options_summary',condition=input$rda_results=="summary")
      shinyjs::toggle('rda_downp',condition=input$rda_results=="plot")
      shinyjs::toggle('downcenter_rda',condition=input$rda_results=="summary")
    })
    box_caret_server("box_setup3")
    box_caret_server("box_8a")
    box_caret_server("box_8b")
    box_caret_server("box_8axes",hide_content = T)

    box_caret_server("box_8c")
    box_caret_server("box_8d")
    box_caret_server("box_8e")
    box_caret_server("box_8f")
    box_caret_server("box_8g")
    box_caret_server("box_8h")
    observeEvent(input$save_bug,{
      saveRDS(reactiveValuesToList(input),"input.rds")
      print("saved input")
    })
    ns<-session$ns
    runval<-reactiveValues(rda="save_changes_nice")
    observeEvent(input$rda_points_factor,{
      vals$cur_rda_points_factor<-input$rda_points_factor
    })
    observeEvent(vals$newcolhabs,{
      choices =vals$colors_img$val
      choicesOpt = list(content =     vals$colors_img$img)
      selected<-get_selected_from_choices(vals$cur_rda_args$cur_rda_text_palette,vals$colors_img$val)


      updatePickerInput(session,'rda_text_palette', choices=choices,choicesOpt=choicesOpt,selected=selected)
      selected<-get_selected_from_choices(vals$cur_rda_args$cur_rda_points_palette,vals$colors_img$val)
      updatePickerInput(session,'rda_points_palette', choices=choices,choicesOpt=choicesOpt,selected=selected)

    })
    observeEvent(input$rda_Y,{
      req(input$rda_Y%in%names(vals$saved_data))
      updateNumericInput(session,'rda_biplot_n',value=ncol(vals$saved_data[[input$rda_Y]]))
    })
    observeEvent(input$rda_points_factor,{
      vals$cur_rda_points_factor<-input$rda_points_factor
    })
    observeEvent(input$rda_text_factor,{
      vals$cur_rda_text_factor<-input$rda_text_factor
    })
    getdata_descX<-reactive({
      req(input$rda_X)
      vals$saved_data[[input$rda_X]]
    })
    observeEvent(attr(getdata_descX(),"factors"),{
      choices=colnames(attr(getdata_descX(),"factors"))
      selected<-get_selected_from_choices(vals$cur_rda_text_factor,choices)
      updatePickerInput(session,'rda_text_factor',choices=choices,selected=selected)
      selected=get_selected_from_choices(vals$cur_rda_points_factor,choices)
      updatePickerInput(session,'rda_points_factor',choices=choices,selected=selected)

    })
    observe({
      shinyjs::toggle('rda_points_out',condition = isTRUE(input$rda_points))
      shinyjs::toggle('rda_text_out',condition = isTRUE(input$rda_text))
      shinyjs::toggle("rda_biplot_out",condition = isTRUE(input$rda_biplot))
      shinyjs::toggle("rda_species_out",condition = isTRUE(input$rda_species))

    })
    rda_points_factor<-reactive({
      req(length(input$rda_points)>0)
      if(isTRUE(input$rda_points)){
        req(input$rda_points_factor)
        data<-get_rdaX()
        factors<-attr(data,"factors")
        req(input$rda_points_factor%in%colnames(factors))
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
          vegan::rda(x~.,data=data.frame(y) ,scale=input$rda_scale)
        } else{ vegan::rda(x,scale=input$rda_scale)}
      })
      req(!inherits(model,"try-error"))

      model
    })
    rda_model<-reactiveVal()
    observeEvent(rda_model(),{
      req(rda_model())
      r2<-data.frame(vegan::RsquareAdj(rda_model()))[1]
      updateTextInput(session,"rda_title",value=paste0("RDA(R²=",round(r2,3),")"))
    })

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
        expandY=input$rda_expandY,
        legend_points=input$rda_legend_points,
        legend_response=input$rda_legend_response,
        show_response_legend=input$show_response_legend,
        show_points_legend=input$show_points_legend,
        xlab=input$xlab,
        ylab=input$ylab,
        show_axis_explain=input$show_axis_explain

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
    output$rda_print<-renderUI({

      div(
        div(

          renderPrint(data.frame(vegan::RsquareAdj(rda_model()))),
        ),
        renderPrint(rda_summary())
      )

    })
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
    get_rdaX<-reactive({
      req(input$rda_X)
      vals$saved_data[[input$rda_X]]
    })
    observeEvent(get_rdaX(),{
      updateNumericInput(session,'rda_species_n',   value=ncol(get_rdaX()))
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
      datalist_name=attr(get_rdaX(),"datalist")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,datalist_name=datalist_name)
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
    observe({
      if(is.null(vals$rda_Y)){
        if(length(vals$saved_data)>1){
          vals$rda_Y<-names(vals$saved_data)[2]
        } else{
          vals$rda_Y<-names(vals$saved_data)[1]
        }
      }
      if(is.null(vals$rda_X)){
        vals$rda_X<-names(vals$saved_data)[1]
      }

    })
    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected_y=get_selected_from_choices(vals$rda_Y,choices)
      selected_x=get_selected_from_choices(vals$rda_X,choices)
      updatePickerInput(session,'rda_Y',choices=choices,selected=selected_y)
      updatePickerInput(session,'rda_X',choices=choices,selected=selected_x)
    })
    observeEvent(input$rda_Y,{
      vals$rda_Y<-input$rda_Y
    })
    observeEvent(input$rda_X,{
      vals$rda_X<-input$rda_X
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

## Logic for tab 9 - segRDA
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
                div(class="setup_box picker-flex picker-before-y",pickerInput_fromtop(ns("segrda_X"),"", choices=NULL, options=shinyWidgets::pickerOptions(liveSearch =T))

                ),
            ),
            div(style="height: 30px; margin-top: 20px",
                div(strong("~")),
                div(actionLink(ns("rev_segrda"),icon("arrow-right-arrow-left")))),
            div(style="display: flex;",
                div(class="setup_box picker-flex picker-before-x",pickerInput_fromtop(ns("segrda_Y"),"", choices=NULL, options=shinyWidgets::pickerOptions(liveSearch =T))),
            ),
            uiOutput(ns('segrda_btn'))

        )

      )

    ),
    tabsetPanel(
      id=ns("segrda_panels"),
      tabPanel(
        "9.1. Split Moving window",
        column(
          4,class='mp0',style="height:  calc(100vh - 200px);overflow: auto",
          box_caret(
            ns("box_9a"),title="Analysis setup",
            color="#c3cc74ff",
            div(
              div(strong("Data Ordination")),

              checkboxInput(ns("segrda_scale"),span("Scale variables",tiphelp("Scale variables to unit variance (like correlations)")), value=F),
              div(
                style="display: flex",

                checkboxInput(ns("segrda_ord"),strong("Axis",tipright("Axis Ordination. Both the SMW and pwRDA analyses depend on ordered datasets. Defaults to ordering both response and explanatory matrices using one of the axes of the RDA model. If unchecked,please make sure all your inputs are already ordered.")), value=T),

                numericInput(ns("axis_ord_segrda"),NULL, value=1, step=1),div(
                  id=ns("segrda_ord_run_btn"),
                  class="save_changes",
                  actionButton(ns('segrda_ord_run'),"Ordinate>>",style="height: 25px; padding: 3px")
                )
              ),
              div(id=ns("ord_out"),style="display: none",
                  div(strong("Split Moving Window")),

                  uiOutput(ns("ord_sure")),

                  textInput(ns('custom_windows'), span(tipify_ui(icon("fas fa-question-circle",style="color: gray"),"Enter a vector of breakpoints (comma delimited, within the data range)"),"Windows"), NULL),


                  pickerInput_fromtop(ns("smw_dist"),span(tipify_ui(icon("fas fa-question-circle", style="color: gray"),"Dissimilarity index"),"Distance:"), choices=c("bray","euclidean","manhattan","jaccard")),
                  pickerInput_fromtop(ns("smw_rand"),span(tipify_ui(actionLink(ns("smw_rand_help"),icon("fas fa-question-circle")),"The type of randomization for significance computation. Click for details"),"Randomization:"), choices=c("shift","plot")),

                  numericInput(ns("smw_nrand"),span(tipify_ui(icon("fas fa-question-circle", style="color: gray"),"The number of randomizations"),"n.rand:"), value=10),
                  uiOutput(ns("down_we_out"))


                  ,

                  uiOutput(ns('run_smw'))
              )
            )
          ),

        ),
        column(8, class="mp0",
               box_caret(
                 ns("box_9b"),title=NULL,
                 button_title2= radioGroupButtons(ns("ord_results"),NULL,c("Matrix plot"="swm_1","Window size effect "="swm_2")),
                 button_title = div(actionLink(
                   ns("downp_matrixplot"),span("Download",icon("fas fa-download"))),     actionLink(ns("downp_windowpool"),"Download plot")),
                 tabsetPanel(
                   id=ns("smw_panels"),
                   type="hidden",
                   tabPanel(
                     "9.1.1 Data ordination",value="swm_1",
                     div( plotOutput(ns("ordplot_matrix")))
                   ),
                   tabPanel("9.1.2 SMW results",
                            value="swm_2",

                            plotOutput(ns("poolplot")))

                 )


               ))



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

                              selectInput(ns("dp_w"),span(tipify_ui(icon("fas fa-question-circle", style="color: gray"),"A target window size from which results will be extracted. If <code>average</code> return z-scores averaged over the set of window sizes"),'+ w:'), 'average'),

                              pickerInput_fromtop(ns("dp_index"),span(actionLink(ns("dp_index_help"),tipify_ui(icon("fas fa-question-circle"),"The result to be extracted. Click for details")),'+ index:'),choices=c("dp","rdp","md","sd","oem","osd","params")),

                              pickerInput_fromtop(ns("dp_sig"),span(actionLink(ns("dp_sig_help"),tipify_ui(icon("fas fa-question-circle"),"Significance test for detecting dissimilarity values that differs significantly from those appearing in a random pattern. Click for details")),'+ sig'),choices=c("z","sd","sd2","tail1")),

                              numericInput(ns("dp_z"),span(tipify_ui(icon("fas fa-question-circle", style="color: gray"),"The critical value for the significance of z-values"),"z:"), value=1.85,step=0.01),

                              pickerInput_fromtop(ns("dp_BPs"),span(tipify_ui(icon("fas fa-question-circle", style="color: gray"),"Defines if the breakpoints should be chosen as those sample positions corresponding to the maximum dissimilarity in a sequence of significant values (max) or as those sample positions corresponding to the median position of the sequence (median). Defaults to BPs=max. If empty the breakpoints are not computed"),"BPs:"),choices=c("","max","median"), selected = "max"),
                              uiOutput(ns('dp_seq.sig')),


                              pickerInput_fromtop_live(inputId=ns("dp_palette"),
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
                      div(tipify_ui(tags$div("BP",class="trailab"),"Split reference (Y Datalist)")),
                      pickerInput_fromtop(ns('bp_column'), NULL,NULL)),
                  div(style="display: flex;;margin-right: 20px",
                      div(

                        tipify_ui(tags$div("n.rand",class="trailab"),"Number of randomizations")),
                      div(class="half-drop",
                          numericInput(ns("pw_nrand"), NULL, value=99))),

                  div(style="display: flex;;margin-left: 20px",
                      div(tipify_ui(tags$div("Results",class="trailab"),"Predictors")),
                      div(style="display: flex",
                          pickerInput_fromtop(ns("pwRDA_models"),NULL, choices=NULL, options=shinyWidgets::pickerOptions(liveSearch =T)),

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

        column(4,class="mp0",style="height:  calc(100vh - 200px);overflow: auto",
               id=ns("options_plot"),
               box_caret(
                 ns("box_8a"),title="General options",
                 color="#c3cc74ff",
                 div(

                   numericInput(ns('segrda_base_size') ,"Base size:",value=12),
                   pickerInput_fromtop(ns('segrda_theme') ,"Theme:",choices=c('theme_bw','theme_grey','theme_linedraw','theme_light','theme_minimal','theme_classic'),selected='theme_bw'),
                   textInput(ns('segrda_title') ,"Title:",value="Redundancy analysis")

                 )
               ),
               box_caret(ns("box_8axes"),title="Score Axes",
                         color="#c3cc74ff",
                         hide_content = T,
                         div(
                           numericInput(ns("segrda_expandX"),"Expand X Axis",value=0.1,step=0.05),
                           numericInput(ns("segrda_expandY"),"Expand Y Axis",value=0.1,step=0.05),
                           checkboxInput(ns('segrda_show_intercept') ,"Intercepts",value=T),
                           textInput(ns("xlab"), "xlab:", "RDA I"),
                           textInput(ns("ylab"), "ylab:", "RDA II"),
                           checkboxInput(ns("show_axis_explain"),"Show Explained Variance",T)

                         )
               ),

               box_caret(
                 ns("box_8b"),
                 color="#c3cc74ff",
                 title=span(style="display: inline-block",
                            class="checktitle",
                            tip="Display Predictor Scores as Points",
                            checkboxInput(ns("segrda_points") ,label =strong("Points"),T,width="150px")
                 ),
                 div(

                   div(
                     id=ns('segrda_points_out'),

                     pickerInput_fromtop_live(ns('segrda_points_palette') ,tiphelp5("Palette","Choose a gradient to represent colors based on the selected factor"),"turbo"),
                     pickerInput_fromtop(ns('segrda_points_factor')  ,"Factor:",choices=NULL),
                     pickerInput_fromtop(inputId = ns("segrda_points_shape"),
                                         label = "Shape:",
                                         choices = df_symbol$val,
                                         choicesOpt = list(content = df_symbol$img),
                                         selected=16,

                                         width = "100px"),
                     numericInput (ns('segrda_points_size') ,"Size:",value=2),
                     checkboxInput(ns("show_points_legend"),"Show legend",T),
                     textInput(ns("segrda_legend_points"),"Legend title:","")


                   )


                 )

               ),

               box_caret(
                 ns("box_8c"),
                 title=span(style="display: inline-block",
                            class="checktitle",
                            tip="Display Predictor Scores as Text",
                            checkboxInput(ns("segrda_text") ,label =strong("Text"),F,width="150px")
                 ),

                 color="#c3cc74ff",
                 div(

                   div(id=ns('segrda_text_out'),
                       pickerInput_fromtop_live(ns('segrda_text_palette') ,"Palette:","gray"),
                       pickerInput_fromtop_live(ns('segrda_text_factor') ,"Factor:",choices=NULL,selected=NULL),

                       numericInput(ns('segrda_text_size') ,"Size:",value=4),
                   ))
               ),

               box_caret(
                 ns("box_8d"),
                 color="#c3cc74ff",
                 title=span(style="display: inline-block",
                            class="checktitle",
                            tip=NULL,
                            checkboxInput(ns("segrda_biplot") ,label =strong("Predictor Scores"),T,width="200px")
                 ),
                 div(
                   div(
                     id=ns('segrda_biplot_out'),
                     numericInput(ns('segrda_biplot_n') ,tiphelp5("Number of Variables", "Set the number of variables to display"),value=10, min=1),
                     div(colourpicker::colourInput(ns('segrda_biplot_color') ,"Text Color",value="blue","background")),
                     div(colourpicker::colourInput(ns('segrda_biplot_arrow_color') ,"Arrow Color",value="blue","background")),

                     numericInput(ns('segrda_biplot_size') ,"Text Color",value=4)

                   )
                 )

               ),
               box_caret(
                 ns("box_8e"),
                 color="#c3cc74ff",
                 title=span(style="display: inline-block",
                            class="checktitle",
                            tip=NULL,
                            checkboxInput(ns("segrda_species") ,label =strong("Response Scores"),T,width="200px")
                 ),
                 div(
                   div(
                     id=ns("segrda_species_out"),
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
                     numericInput(ns('segrda_species_n') ,"sp number:",value=10),

                     numericInput(ns('segrda_species_size') ,"Size:",value=2),
                     checkboxInput(ns("show_response_legend"),"Show legend",T),
                     textInput(ns("segrda_legend_response"),"Legend title:","Response")
                   )
                 ))

        ),
        column(4,class="mp0",id=ns("options_summary"),
               box_caret(ns("box_9k"),
                         title="Options",
                         color="#c3cc74ff",
                         div(

                           div(
                             pickerInput_fromtop(ns("disegrda_sp_summ"),"Results:",choices=c('Summary stats','Importance (unconstrained)',"Importance (constrained)",'Variable scores','Observation scores','Linear constraints','Biplot'))),
                           numericInput(ns("segrda_axes"),"Axes", value=2),

                           div(tipify_ui(downloadLink(ns('downmodel_segrda'),span("Download model"), style="button_active"),"Download model as .rds file"))

                         )

               )),
        column(
          8,class="mp0",
          box_caret(ns("box_9j"),

                    title="pwRDA Plot",
                    button_title = div(
                      actionLink(ns('segrda_downp'),span("Download Plot",icon("fas fa-download"))),div(tipify_ui(actionLink(ns('downcenter_segrda'),span("Download",icon("fas fa-table"))),"Download selected results"))
                    ),
                    button_title2=radioGroupButtons(ns("segrda_results"),NULL,c("Plot"="plot","Summary"="summary")),
                    tabsetPanel(
                      id=ns("segrda_view"),
                      type="hidden",

                      tabPanel(
                        "9.3.1. Plot", value="plot",
                        div(uiOutput(ns("segrda_plot")))

                      ),
                      tabPanel("9.3.2. Summary", value="summary",
                               div(uiOutput(ns("segrda_print")))

                      )

                    )


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
    box_caret_server("box_8a")
    box_caret_server("box_8b")
    box_caret_server("box_8c")
    box_caret_server("box_8d")
    box_caret_server("box_8e")

    box_caret_server("box_8axes",hide_content = T)
    box_caret_server("box_9f")
    box_caret_server("box_9g")
    box_caret_server("box_9h")
    box_caret_server("box_9i")
    box_caret_server("box_9j")
    box_caret_server("box_9k")
    box_caret_server("box_9l")
    ns<-session$ns
    observeEvent(input$ord_results,{
      updateTabsetPanel(session,'smw_panels',selected=input$ord_results)
      shinyjs::toggle('downp_matrixplot',condition=input$ord_results=="swm_1")
      shinyjs::toggle('downp_windowpool',condition=input$ord_results=="swm_2")
    })


    observeEvent(input$segrda_results,{
      updateTabsetPanel(session,'segrda_view',selected=input$segrda_results)
    })

    observe({
      shinyjs::toggle('options_plot',condition=input$segrda_results=="plot")
      shinyjs::toggle('options_summary',condition=input$segrda_results=="summary")
      shinyjs::toggle('segrda_downp',condition=input$segrda_results=="plot")
      shinyjs::toggle('downcenter_segrda',condition=input$segrda_results=="summary")
    })



    ns<-session$ns

    getdata_descX<-reactive({
      req(input$segrda_Y)
      vals$saved_data[[input$segrda_Y]]
    })
    get_segrdaX<-reactive({
      req(input$segrda_X)
      vals$saved_data[[input$segrda_X]]
    })


    observeEvent(get_segrdaX(),{
      updateNumericInput(session,'segrda_species_n',   value=ncol(get_segrdaX()))
    })

    observe({
      shinyjs::toggle('segrda_points_out',condition = isTRUE(input$segrda_points))
      shinyjs::toggle('segrda_text_out',condition = isTRUE(input$segrda_text))
      shinyjs::toggle("segrda_biplot_out",condition = isTRUE(input$segrda_biplot))
      shinyjs::toggle("segrda_species_out",condition = isTRUE(input$segrda_species))

    })

    observeEvent(attr(get_segrdaX(),"factors"),{
      choices=colnames(attr(get_segrdaX(),"factors"))
      selected<-get_selected_from_choices(vals$cur_segrda_text_factor,choices)
      updatePickerInput(session,'segrda_text_factor',choices=choices,selected=selected)
      selected=get_selected_from_choices(vals$cur_segrda_points_factor,choices)
      updatePickerInput(session,'segrda_points_factor',choices=rev(choices),selected=selected)

    })

    observeEvent(vals$newcolhabs,{
      choices =vals$colors_img$val
      choicesOpt = list(content =     vals$colors_img$img)

      selected<-get_selected_from_choices(vals$cur_rda_args$cur_rda_text_palette,vals$colors_img$val)

      updatePickerInput(session,'segrda_text_palette', choices=choices,choicesOpt=choicesOpt,selected=selected)
      selected<-get_selected_from_choices(vals$cur_rda_args$cur_rda_points_palette,vals$colors_img$val)
      updatePickerInput(session,'segrda_points_palette', choices=choices,choicesOpt=choicesOpt,selected=selected)

    })


    pwmodels<-reactive({
      req(input$segrda_X)
      names(attr(vals$saved_data[[input$segrda_X]],"pwRDA"))
    })
    matrixplot<-reactive({
      req(isTRUE(input$segrda_ord))
      sim1o<-getord()

      mybreaks<-vals$window_pool

      xo<-sim1o$xo ## ordered explanatory matrix.
      yo<-sim1o$yo ## ordered community matrix (untransformed).
      x<-sim1o$y

      x<-apply(x,1,rev)
      yo<-apply(yo,1,rev)

      par(mfrow = c(1, 2), mgp = c(1, 1, 0), cex = 0.9)
      raster::image(raster::raster(as.matrix(x)), main = "Original response data", col = topo.colors(100), axes = F,

                    xlab = "Observations", ylab = "Variable values")
      # abline(v=scales::rescale(mybreaks,c(0,1)), col="red")


      raster::image(raster::raster(as.matrix(yo)), main = "Ordered response data", col = topo.colors(100), axes = F,
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

    observeEvent(input$segrda_points_factor,{
      vals$cur_segrda_points_factor<-input$segrda_points_factor
    })





    observeEvent(vals$segrda_model$rda.pw,{
      req(vals$segrda_model$rda.pw)
      r2<-data.frame(vegan::RsquareAdj(vals$segrda_model$rda.pw))[1]
      updateTextInput(session,"segrda_title",value=paste0("segrda(R²=",round(r2,3),")"))
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

      text_factor<-text_factor[rownames(vegan::scores(pwRDA_model$rda.pw)$sites),,drop=F]
      points_factor<-points_factor[rownames(vegan::scores(pwRDA_model$rda.pw)$sites),,drop=F]

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
        expandY=input$segrda_expandY,
        legend_points=input$segrda_legend_points,
        legend_response=input$segrda_legend_response,
        show_points_legend=input$show_points_legend,
        show_response_legend=input$show_response_legend,
        xlab=input$xlab,
        ylab=input$ylab,
        show_axis_explain=input$show_axis_explain


      )


    })
    output$segrda_plot<-renderUI({
      req(input$segrda_X)
      req(input$segrda_Y)
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
          shinyBS::popify(
            div(class=class,id=ns("save_changes_nice_bp"),
                shinyBS::bsButton(ns('tools_saveBP'),icon("fas fa-save"),style='save_button')
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
      tipify_ui(
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
      #saveRDS(vals$smw_dp,"smw_dp.rds")

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
      numericInput(ns("dp_seq.sig"),span(tipify_ui(icon("fas fa-question-circle", style="color: gray"),"The maximum length of consecutive, significant values of dissimilarity that will be considered in defining the community breakpoints"),"seq.sig:"), value=3,step=1, min=1)
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

      tipify_ui(
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
      suppressWarnings(segRDA::bp(getDP()))})
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
    output$segrda_print<-renderPrint({
      div(
        div(

          renderPrint(data.frame(vegan::RsquareAdj(vals$segrda_model$rda.pw,scrollY = NULL)))
        ),
        renderPrint(segrda_summary())
      )

    })
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
      req(ncol(dp)>1)
      colnames(dp)[2]<-"SampleID"

      sim1o<-getord()
      yo<-data.frame(sim1o$yo)
      bps<-suppressWarnings(segRDA::bp(dp))

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
    observeEvent(list(input$segrda_X,
                      input$segrda_X,
                      input$axis_ord_segrda,
                      input$segrda_scale),{
                        shinyjs::addClass('segrda_ord_run_btn',"save_changes")
                      })




    observe({
      shinyjs::toggle('ord_out',condition=length(names(getord()))>0)
    })
    getord<-eventReactive(input$segrda_ord_run,ignoreInit = T,{
      shinyjs::removeClass('segrda_ord_run_btn',"save_changes")
      req(input$segrda_Y)
      req(input$segrda_X)
      req(input$axis_ord_segrda)
      x<-as.matrix(vals$saved_data[[input$segrda_X]])
      colnames(x)<-colnames(vals$saved_data[[input$segrda_X]])
      y<-na.omit(as.matrix(vals$saved_data[[input$segrda_Y]][rownames(x),,drop=F]))
      colnames(y)<-colnames(vals$saved_data[[input$segrda_Y]])
      x<-na.omit(x[rownames(y),,drop=F])
      if(isTRUE(input$segrda_ord)){
        sim1o<-segRDA::OrdData(x=y,y=x, axis=input$axis_ord_segrda,scale=input$segrda_scale)} else{
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
          newname<-paste0("pwRDA (",input$bp_column,")")
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
      beepr::beep()
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
      data<-get_segrdaX()
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
      choices=choices_bp_column()
      selected=get_selected_from_choices(vals$cur_bp_column,choices)
      updatePickerInput(session ,'bp_column',choices=choices,selected= selected)
    })

    observe({
      if(is.null(vals$segrda_Y)){
        if(length(vals$saved_data)>1){
          vals$segrda_Y<-names(vals$saved_data)[2]
        } else{
          vals$segrda_Y<-names(vals$saved_data)[1]
        }
      }
      if(is.null(vals$segrda_X)){
        vals$segrda_X<-names(vals$saved_data)[1]
      }

    })
    observeEvent(saved_data_names(),{
      choices=names(vals$saved_data)
      selected_y=get_selected_from_choices(vals$segrda_Y,choices)
      selected_x=get_selected_from_choices(vals$segrda_X,choices)
      updatePickerInput(session,'segrda_Y',choices=choices,selected=selected_y)
      updatePickerInput(session,'segrda_X',choices=choices,selected=selected_x)
    })
    observeEvent(input$segrda_Y,{
      vals$segrda_Y<-input$segrda_Y
    })
    observeEvent(input$segrda_X,{
      vals$segrda_X<-input$segrda_X
    })
    observeEvent(ignoreInit = T,input$save_teste,{
      saveRDS(reactiveValuesToList(vals),"savepoint.rds")
      saveRDS(reactiveValuesToList(input),"input.rds")
      beepr::beep()

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

      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Window size effect plot", name_c="winsize_plot",  datalist_name=attr(getdata_descX(),"datalist"))
    })
    observeEvent(vals$newcolhabs,{
      choices =vals$colors_img$val[getgrad_col()]
      choicesOpt = list(content =     vals$colors_img$img[getgrad_col()])
      updatePickerInput(session,'dp_palette', choices=choices,choicesOpt=choicesOpt)
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
      argg$sim1o<-NULL
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


    observeEvent(vals$smw_dp,{
      shinyjs::addClass("save_dp_btn","save_changes")
    })


    observeEvent( input$save_dp,ignoreInit = T,{
      shinyjs::removeClass("save_dp_btn","save_changes")
      req(vals$smw_dp)
      req(input$segrda_X)

      attr(vals$saved_data[[input$segrda_X]],"dp")<-vals$smw_dp
    })



    observeEvent((input$segrda_X),{
      req(input$segrda_X)
      if(!is.null( attr(vals$saved_data[[input$segrda_X]],"dp"))) {
        vals$smw_dp<-attr(vals$saved_data[[input$segrda_X]],"dp")
      }
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
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,  datalist_name=attr(getdata_descX(),"datalist"))
    })
    observeEvent(ignoreInit = T,input$downp_dp,{

      vals$hand_plot<-"dp"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,  datalist_name=attr(getdata_descX(),"datalist"))
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
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Matrix plot", name_c="matrixplot",  datalist_name=attr(getdata_descX(),"datalist"))
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
      datalist_name=attr(getdata_descX(),"datalist")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,datalist_name=datalist_name)
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

# Auxiliar functions
datalist_overview<-function(data,available_models){


  dfl<-list(
    'Numeric-Attribute'=data,
    'Factor-Attribute'=attr(data,"factors"),
    'Coords-Attribute'=attr(data,"coords")
  )
  dfl<-dfl[sapply(dfl,length)>0]


  shapel<-list(
    'Base-Shape'=attr(data,"base_shape"),
    'Layer-Shape'=attr(data,"layer_shape"),
    'Extra-Shapes'=attr(data,"extra_shape")
  )



  attrs<-c(as.character(available_models),"pwRDA","som","kmeans")
  modelsl<-sapply(attrs,function(x){

    models<-attr(data,x)

    m1<-NULL
    if(length(models)>0){
      m1<-lapply(models,function(m){
        nclusters=NULL

        if(!x%in%c("som","kmeans","hc","pwRDA")){
          m<-m$m
        }
        hc=attr(m,"hc.objec")
        if(x=="som"){
          class=if(length(m$data)>1){
            "supersom"
          } else{
            "som"
          }
          class=paste0(class(m),"-",class)

        } else if(x%in%c("pwRDA","kmeans","hc")){
          class="pwRDA"
        } else {
          class=paste(class(m)[1],class(m$finalModel))
        }
        data.frame(class=class)




      })


      result<-data.frame(table(sapply(m1,function(x) x$class)))

      result<-lapply(1:nrow(result),function(i){
        div(paste0(result[i,1]," (models:",result[i,2],")"))
      })
      result
    }
  })

  (modelsl<-modelsl[sapply(modelsl,length)>0])



  div(style="padding: 20px; background: white",
      div(
        strong("Sheets:",style="color:royalblue"),

        div(style=";font-size: 11px",
            do.call(div,lapply(seq_along(dfl),function(i){
              data<-dfl[[i]]
              tags$li(
                strong(names(dfl)[i]),
                div(style="padding-left: 20px",span(class(data),
                                                    if(inherits(data,"data.frame")){
                                                      span(paste0("(",nrow(data),"x",ncol(data),")"))
                                                    }))
              )
            }))
        )
      ),
      div(
        strong("Shapes:",style="color:royalblue"),
        div(
          style=";font-size: 11px",
          lapply(seq_along(shapel),function(i){
            data<-shapel[[i]]
            div(
              tags$li(strong(names(shapel)[i]))
            )
          })
        )
      ),

      div(
        strong("Models:",style="color:royalblue"),
        div(
          style="padding-left: 20px;font-size: 11px",
          if(!length(modelsl)>0){
            "No saved model"
          } else{
            modelsl
          }

        )
      )

  )

}
update_inputs<-function(inputs,input,restored,session){
  updated<-F
  for(i in seq_along(inputs) ){
    cur_input<-inputs[[i]]
    cur_value<-cur_restored<-restored[[paste0("cur_",cur_input)]]
    if(!is.null(cur_restored))
      if(!is.null(input[[cur_input]]))
        if(length(input[[cur_input]])==length(cur_restored))
          if(!is.null(input[[cur_input]])){
            if(!is.null(cur_value)){
              if(!identical(input[[cur_input]],cur_restored)){
                if(is.numeric(cur_value)){
                  updated<-T
                  updateNumericInput(session,cur_input,value=cur_value)
                } else if(is.logical(cur_value)){
                  updated<-T
                  updateCheckboxInput(session,cur_input,value=cur_value)
                } else if(is.character(cur_value)) {
                  updated<-T
                  updateTextInput(session,cur_input,value=cur_value)
                }
              }
            }
          }


  }

  updated
}
color_input<-function(id,label,selected=NULL,vals){
  pickerInput_fromtop_live(
    inputId = id,
    label = label,
    choices =     vals$colors_img$val,
    choicesOpt = list(content =vals$colors_img$img),
    selected=selected
  )

}
get_summary_datalist<-function(datalist, which=c("numeric","factors","coords")){


  numeric<-NULL
  factors<-NULL
  coords<-NULL
  if('coords'%in%which){
    coords<-attr(datalist,"coords")
    if(!is.null(coords))
      attr(coords,"name")<-"Coords"
  }
  if('factors'%in%which){
    factors<-attr(datalist,"factors")

    if(!is.null(factors))
      attr(factors,"name")<-"Factor"
  }
  if('numeric'%in%which){
    numeric<-datalist
    attr(numeric,"name")<-"Numeric"
  }

  res<-list(numeric,factors,coords)
  res<-res[sapply(res,length)>0]

  df<-data.frame(do.call(rbind,lapply(res,function(data){
    c(attr(data,"name"),  nrow(data),  ncol(data), sum(is.na(data)))
  })))
  colnames(df)<-c("Attributes","nrows","ncols","NAs")

  na_styles<-lapply(1:nrow(df),function(i){
    if(df$NAs[i]>0){
      tags$style(HTML(paste0(".table0 tr:nth-child(",i,") > td:nth-child(4){
                   color: red
                 }")))
    }
  })


  div(
    na_styles,
    tags$style(HTML('.table0 .table{
                  margin-bottom: 0px;
                  color: blue
  }
                  .table0 .shiny-table.spacing-s>thead>tr>th{
                  color: black;
                  padding-top: 2px;
    padding-bottom: 0px;
                  }
                 .table0 tr:nth-child(1) > td:nth-child(1){
                 font-style: italic;
                   color: black
                 }
                    .table0 tr:nth-child(2) > td:nth-child(1){
                  font-style: italic;
                    color: black
                    }
                    .table0 tr:nth-child(3) > td:nth-child(1){
                  font-style: italic;
                    color: black
                  }

                  .table0  .table.shiny-table>thead>tr>th{
                  padding-right: 12px;
    padding-left: 5px;
                  }


                  ')),

    div(class="half-drop-inline table0",style=" margin-bottom: 0px;margin-top: -10px",
        renderTable(df)
    )
  )



}
tiphelp5<-function(title,text,placement ="bottom"){
  span(class="tip-80",
       style="color: #3c8dbc;",
       title,tipify_ui(icon("fas fa-question-circle"),text,placement =placement ))
}
tiphelp6<-function(text,placement ="bottom"){
  span(class="tip-80",
       style="color: #3c8dbc;",
       tipify_ui(icon("fas fa-question-circle"),text,placement =placement ))
}
