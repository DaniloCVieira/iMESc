
hc_module<-list()
#' @export
hc_module$ui<-function(id){
  ns<-NS(id)
  column(12,class="mp0",style="width: 100%",
         box_caret(ns("box_setup"),
                   title="Model setup",
                   color="#374061ff",
                   inline=F,
                   fluidRow(style="display: flex; flex-flow: row wrap;",
                            column(12,div(style="gap: 10px",class="som_grid",
                                          pickerInput_fromtop(ns("data_hc"),
                                                              strong("Datalist:"),
                                                              choices = NULL),
                                          radioButtons(ns("model_or_data"), strong("Clustering target:"), choiceValues = c("data", "som codebook"), choiceNames = c("Numeric-Attribute", "SOM-codebook"),width="150px"),
                                          pickerInput_fromtop(ns("som_model_name"), strong("Som model:"), choices=NULL, selected=NULL),
                                          pickerInput_fromtop(ns("hc_fun"), strong("HC function:", actionLink(ns("help_hc_fun"), icon("fas fa-question-circle"))), choices = list("Hierarchical Clustering" = "hclust", "Agglomerative Nesting" = "agnes", "Divisive hierarchical clustering" = "diana")),
                                          div(id=ns("disthc_id"),
                                              pickerInput_fromtop(ns("disthc"), strong("Distance:"), choices = c('bray', "euclidean", 'jaccard'))
                                          ),
                                          pickerInput_fromtop(ns("method.hc0"),strong( "Method:"), choices = c("ward.D2", "ward.D", "single", "complete", "average", "mcquitty", "median", "centroid"))
                            ))
                   )
         ),
         uiOutput(ns('teste')),
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
                                 uiOutput(ns("saveHC"))),


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

                         div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; padding-bottom: 5px',
                             div(
                               checkboxInput(ns("hc_sort"),span("Sort clusters",tiphelp("Sort clusters by a  variable")),value=F),
                               div(style="margin-left: 15px;",
                                   uiOutput(ns("hc_sort_datalist")) ,

                                   uiOutput(ns("hc_ord_factor")))

                             )
                         ),
                         pickerInput_fromtop(inputId = ns("hcdata_palette"),label = "HC Palette:",NULL),
                         pickerInput_fromtop(ns("hcut_labels"),"Factor",NULL),
                         div(
                           pickerInput_fromtop(ns("hcut_theme"),"Theme:",c('theme_minimal','theme_grey','theme_linedraw','theme_light','theme_bw','theme_classic')),
                           numericInput(ns("hcut_cex"),"Size",value = 12,step = 1),
                           numericInput(ns("hcut_lwd"),"Line width",value = .5,step = .5),
                           textInput(ns("hcut_main"),"Title","Cluster Dendrogram"),
                           textInput(ns("hcut_ylab"),"y label","Height"),
                           textInput(ns("hcut_xlab"),"x label","Observations"),
                           numericInput(ns("hcut_xlab_adj"),"xlab v-adj",value = 30,step = 1),
                           numericInput(ns("hcut_offset"),"offset",value = -.1,step = 0.05),
                           radioButtons(ns("hcut_legend_type"),"Legend:",c("inside","outside"), inline=T)
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
                              pickerInput_fromtop(ns("bg_palette"),label ="Palette",NULL),
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
                                pickerInput_fromtop(inputId = ns("pclus_points_palette"),label ="Palette",choices =NULL),
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
                                pickerInput_fromtop(ns("pclus_text_palette"),label ="Palette",NULL),
                                pickerInput_fromtop(ns("pclus_text_factor"),"Factor",choices = NULL),
                                numericInput(ns("pclus_text_size"),"Size",value = 1,min = 0.1,max = 3,step = .1),
                                checkboxInput(ns("text_repel"),"Repel Labels",F),
                                numericInput(ns("max.overlaps"),"max.overlaps",value = 10,min = 1,step = 1)
                            )),
                          box_caret(
                            ns("box4_vfm"),
                            color="#c3cc74ff",
                            button_title=tipify(actionLink(ns("varfacmap"), icon("fas fa-question-circle")),"Click for details","right"),
                            title=span(style="display: inline-block",
                                       class="checktitle",

                                       checkboxInput(ns("varfacmap_action"),span("Variable factor map"),value =T,width="210px"),

                            ),
                            div(id=ns('varfac_out'),
                                pickerInput_fromtop(ns("vfm_type"),"Show correlation:",choices =list("Highest"='var', "Chull"="cor","Cluster"="cor_hc")),

                                numericInput(ns("npic"), span(tiphelp("Number of variables to display"),"Number"), value = 10, min = 2),
                                numericInput(ns("pclus.cex.var"), "Var size", value = 1, min = 2),
                                div(class="palette",
                                    pickerInput_fromtop(ns("p.clus.col.text"),label = "Var text color",choices =NULL )),
                                pickerInput_fromtop(ns("var_bg"),label = "Var background",choices = NULL),
                                numericInput(ns("var_bg_transp"), "Var transparency", value = 0, min = 2))
                          ),
                          box_caret(
                            ns("box_var_pie"),
                            color="#c3cc74ff",
                            button_title=tipify(actionLink(ns("var_pie_help"), icon("fas fa-question-circle")),"Click for details","right"),
                            title=span(style="display: inline-block",
                                       class="checktitle",

                                       checkboxInput(ns("var_pie"),strong("Variable pies"),value =F,width="210px"),

                            ),

                            div(id=ns('var_pie_out'),
                                pickerInput_fromtop(ns("var_pie_type"),"Show:",choices =list("Top importance by cluster"='top_hc', "Top importance"="top","Top weight"="top_w","Manual"="manual")),
                                pickerInput_fromtop(ns("var_pie_layer"),"Layer",NULL),
                                div(class="virtual-130",
                                    virtualPicker(ns("var_pie_manual"),"variables selected")
                                ),

                                numericInput(ns("var_pie_n"), span(tipright("Number of variables to display"),"Number"), value = 10, min = 2),
                                pickerInput_fromtop(ns("var_pie_bg"),label = "Palette",choices = NULL),
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
                              div(tipify(downloadLink(ns('down_hc_model'),"Download HC model", style="button_active"),"Download file as .rds"))
                            )
                          )

                      )),
                    column(
                      8,class="mp0",style="right: 12px;margin-top: -80px; padding-left: 15px",
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

    output$importance_results<-renderUI({
      imp_results<-attr(hcplot4(),"imp_results")
      req(imp_results)
      actionLink(ns("show_hc_imp"),"Importance results",icon("expand"))
    })
    output$create_importance_results<-renderUI({
      p<-hcplot4()
      imp_results<-attr(p,"imp_results")
      imp_layer<-attr(p,"imp_layer")
      req(imp_layer%in%names(vals$saved_data))
      req(imp_results)
      div(actionLink(ns("create_hc_imp"),span("Create Datalist",tiphelp("Create Datalist with selected variables for pies")),icon("creative-commons-share")))
    })

    observeEvent(input$create_hc_imp,{

      p<-hcplot4()
      imp_results<-attr(p,"imp_results")
      imp_layer<-attr(p,"imp_layer")
      imp_vars<-attr(p,"imp_vars")
      req(imp_layer)
      req(imp_vars)
      req(imp_layer%in%names(vals$saved_data))

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
      data<-attr(hcplot4(),"imp_results")
      req(data)
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"som_imp_results"
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Permutation Importance Results",data=data, name=name)

    })
    observeEvent(input$var_pie_layer,{
      m<-getmodel_hc()

      choices<-colnames(m$data[[input$var_pie_layer]])

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
            p("The variables to display through pies can be selected though four options:"),
            tags$ul(
              tags$li(strong("Top importance by cluster"),
                      p("The groups based on the hierarchical clustering (HC) results assigned to neurons are used to split the codebook. For each group, the sum of the codebook weights for each variable is calculated, providing a measure of the absolute importance of each variable within each group. These importance scores are normalized by the total importance of each variable across all groups, resulting in a relative importance score for each variable within each group. The top variables with the highest relative importance scores for each group are identified and plotted using pies of their weights in the codebook.")
              ),
              tags$li(strong("Top importance"),
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
      toggle("var_pie_out",condition=isTRUE(input$var_pie))
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
      updateTabsetPanel(session,"tabs",selected=input$tabs_view)
    })
    observeEvent(input$model_or_data,{
      if(input$model_or_data== "data"){
        removeTab("tabs_view","tab4")
        removeTab("tabs_view","tab5")
      } else{
        insertTab("tabs_view",tabPanel('4. Codebook clusters',value="tab4",selected=T))
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
            ".label_none label{display: none}"
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
    cluster_on<-reactiveVal(F)


    hcplot3<-reactiveVal()

    observeEvent(input$run_hc,{
      cluster_on(T)

      args<-hcut_argsplot()


      p<-do.call('hc_plot',args)

      shinyjs::removeClass("hcut_btn","save_changes")
      hcplot3(p)
    })



    cutsom.reactive<-get_hc<-reactive({

      req(input$model_or_data)
      req(input$method.hc0)
      args<-list(data=getdata_hc(), k= input$customKdata,hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc,model_name=as.character(input$som_model_name),target=input$model_or_data)



      somC<-do.call(imesc_hclutering,args)
      vals$hc_messages<-attr(somC,"logs")

      somC

    })


    output$BMU_PLOT<-renderPlot({hcplot4()})
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
    output$hc_sort_datalist<-renderUI({
      req(isTRUE(input$hc_sort))
      div(

        pickerInput_fromtop(ns("hc_ord_datalist"),"Datalist:",choices = c(names(vals$saved_data[getdata_for_hc()])),selected=vals$cur_hc_ord_datalist,width="200px"))
    })
    output$hc_ord_factor<-renderUI({
      req(isTRUE(input$hc_sort))
      req(input$hc_ord_datalist)
      data<-vals$saved_data[[input$hc_ord_datalist]]

      choices=c(colnames(data))

      div(

        pickerInput_fromtop(ns("hc_ord_factor"),"Variable:",
                            choices = choices,selected=vals$cur_hc_ord_factor,options=shinyWidgets::pickerOptions(liveSearch=T))
      )

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
          tipify(bsButton(ns("savemapcode"), icon(verify_fa = FALSE, name = NULL, class = "fas fa-save"), style = "button_active", type = "action", value = FALSE),"Create Datalist with prediction results","right"), span(style = "font-size: 12px", icon(verify_fa = FALSE, name = NULL, class = "fas fa-hand-point-left"), "Create Datalist")
      )
    })
    output$saveHC<-renderUI({
      req(cluster_on())
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
    hcut_argsplot<-reactive({


      somC<-phc()
      req(somC$hc.object)
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

    hc_screeplot<-function(data,model_or_data="som codebook",model_name=1,disthc,screeplot_hc_k){
      cmd_log_type<-d_log_type<-p_log_type<-NULL
      cmd_log_message<-d_log_message<-p_log_message<-NULL

      if(model_or_data=="som codebook"){
        m<- attr(data,"som")[[model_name]]
        dist<-object.distances(m,"codes")
        data_log<-capture_log1(cmdscale)(dist, k=30)
        data<-data_log[[1]]
        cmd_log_message<-sapply(data_log$logs,function(x) x$message)
        if(length(cmd_log_message)==0){
          cmd_log_message<-NULL
          cmd_log_message<-NULL
        }
        cmd_log_type<-sapply(data_log$logs,function(x) x$type)
      } else{
        dist_log<-capture_log1(vegdist)(data,disthc)
        dist<-dist_log[[1]]
        d_log_message<-sapply(dist_log$logs,function(x) x$message)
        if(length(d_log_message)==0){
          d_log_message<-NULL
          d_log_message<-NULL
        }
        d_log_type<-sapply(dist_log$logs,function(x) x$type)
      }
      if(!is.null(dist)){
        p_log<-capture_log1(fviz_nbclust)(data, hcut, method = "wss", k.max = screeplot_hc_k, diss=dist)




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


    get_hc_screeplot<-reactive({

      args<-list(
        data=getdata_hc(),
        model_or_data=input$model_or_data,
        model_name=input$som_model_name,
        disthc=input$disthc,
        screeplot_hc_k=input$screeplot_hc_k
      )

      re<-do.call(hc_screeplot,args)

      vals$smw_message<-attr(re,"logs")
      req(!isFALSE(re[1]))


      result<-attr(re,"result")
      vals$screeplot_results0<-vals$screeplot_results<-result
      vals$scree_plot_hc<- vals$scree_plot_hc0<-re
    })
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
    phc<-eventReactive(list(input$run_hc,input$run_bmu),{
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
    argsplot_somplot<-reactive({


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
                 hc=phc()$som.hc,
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
    getmodel_hc<-reactive({
      req(input$data_hc)
      req(input$som_model_name)
      data<-getdata_hc()
      m<-attr(data,"som")[[as.character(input$som_model_name)]]
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
                 withSpinner(type=8,color="SeaGreen",uiOutput(ns("databank_storage"))),
                 title=span(icon(verify_fa = FALSE,name=NULL,class="fas fa-save"),'Save'),
                 footer=column(12,class="needed",
                               fluidRow(bsButton(ns("cancel_save"),"Cancel"),
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
                                                withSpinner(type=8,color="SeaGreen",uiOutput(ns("data_create")))),
                                         div(style="height: 50px",
                                             withSpinner(type=8,color="SeaGreen",uiOutput(ns("data_over")))))

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
      somC<-phc()
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
    output$hcut_plot<-renderUI({
      renderPlot({
        hcplot3()
      })
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

    args_hc1<-reactive({
      list(data=getdata_hc(), k= 1,hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc,model_name=as.character(input$som_model_name),target=input$model_or_data)
    })
    observeEvent(args_hc1(),{
      vals$hc_messages<-NULL
      vals$hc_tab1_plot<-NULL
      shinyjs::addClass("hcut_btn1","save_changes")
    })
    observeEvent(args_hc2(),{
      vals$hc_messages<-NULL
      hcplot3(NULL)
      shinyjs::addClass("hcut_btn","save_changes")

    })




    observeEvent(argsplot_somplot(),{
      vals$hc_messages<-NULL
      shinyjs::addClass("run_bmu_btn","save_changes")
    })
    hcplot4<-reactiveVal()
    observeEvent(hcplot4(),{
      shinyjs::removeClass("run_bmu_btn","save_changes")
    })
    observeEvent(input$run_bmu,ignoreInit = T,{
      hcplot4(NULL)
      m<-getmodel_hc()
      somC<-phc()
      args<-argsplot_somplot()
      args$hc<-phc()$som.hc
      hcplot4(do.call(bmu_plot_hc,args))

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
        xlab_adj=input$hcut_xlab_adj,
        legend=input$hcut_legend_type,
        hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc,model_name=input$som_model_name,target=input$model_or_data

      )
    })

    args_hc3<-reactive({
      list(data=getdata_hc(),
           input$customKdata,
           hc_fun=input$hc_fun,
           hc_method=input$method.hc0,
           distance_metric=input$disthc,
           model_name=as.character(input$som_model_name),
           target=input$model_or_data,
           input$hcdata_palette,
           lwd=input$hcut_lwd,
           base_size=input$hcut_cex,
           main=input$hcut_main,
           xlab=input$hcut_xlab,
           ylab=input$hcut_ylab,
           theme=input$hcut_theme,
           offset_labels=input$hcut_offset,
           xlab_adj=input$hcut_xlab_adj,
           legend=input$hcut_legend_type)
    })



    observeEvent(input$run_hc1,ignoreInit = T,{
      shinyjs::removeClass("hcut_btn1","save_changes")
      req(input$model_or_data)
      req(input$method.hc0)
      args<-args_hc1()


      somC<-do.call(imesc_hclutering,args)
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



    observeEvent(vals$saved_data,{
      updatePickerInput(session,"data_hc",choices=names(vals$saved_data), selected=vals$cur_data)
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


    observe({
      choices= names(attr(getdata_hc(), "som"))
      selected=vals$cur_som_model_name

      selected<-get_selected_from_choices(selected,choices)

      updatePickerInput(session,'som_model_name',selected=selected,choices=choices)
    })


    observeEvent(ignoreInit = T,input$download_plot3,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=hcplot3()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Dendrogram - cut", name_c="dendcut")
    })
    observeEvent(ignoreInit = T,input$download_plot4,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=hcplot4()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="som codebook - HC", name_c="codebook_hc")
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
      attr(attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]],"codebook_screeplot")<-result


    })
    observeEvent(ignoreInit = T,input$data_hc,{
      vals$show_mapcode_errors<-c("Within Sum of Squares","Dendrogram Height")
    })
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
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Scree plot", name_c="screeplot")
    })
    observeEvent(input$download_plot5,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=hcplot5()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="SOM- Scree plot", name_c="som_screeplot")
    })
    observeEvent(ignoreInit = T,input$download_plot1,{
      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=vals$hc_tab1_plot
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Dendrogram", name_c="dendplot")
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
    observe({ shinyjs::toggle('som_model_name',condition=input$model_or_data=="som codebook")})
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



    observeEvent(vals$hc_tab3_plot,{

    })

    observeEvent(list(choices_hc(),vals$cur_model_or_data),{
      req(choices_hc())
      if(!any(choices_hc()==vals$cur_model_or_data))
        vals$cur_model_or_data<-NULL
    })


  })
}


