#' @noRd
#' @export
fixed_dt_con<-list()
#' @export
fixed_dt_con$ui<-function(id,data,max_length=100){
  if(is.null(data)){
    return(NULL)
  }
  vecs<-split_vector_max_elements(1:ncol(data),max_length)
  choices_containers<-sapply(vecs,function(x) paste(range(x),collapse="-"))
  choices_names<-names(choices_containers)
  names(choices_names)<-choices_containers
  ns<-NS(id)
  div(
    pickerInput(ns("data_container"),"Show columns" , choices_names)

  )
}
#' @export
fixed_dt_con$server<-function(id,data,max_length=100){
  moduleServer(id,function(input,output,session){

    if(is.null(data)){
      return(NULL)
    }
    vecs<-split_vector_max_elements(1:ncol(data),max_length)
    data_containers<-lapply(vecs,function(x) data[,x])
    output$data_render<-renderUI({
      div(
        class="half-drop-inline",
        style="max-width: 100%; overflow-x: auto",
        fixed_dt(data_containers[[input$data_container]],scrollY = "300px",scrollX=T)
      )
    })
  })
}
split_vector_max_elements <- function(vec, max_length) {
  split(vec, ceiling(seq_along(vec) / max_length))
}
#' @export
imesc_supersom<-list()
#' @export
imesc_supersom$ui<-function(id, vals){
  ns<-NS(id)
  div(
    div(   tags$style(HTML("
                           .inline_pickers2 .dropdown .btn{
                           height: 30px;

}
                           .inline_pickers2 .form-control{
                           height: 30px;
                           border-radius: 0px;
                           padding: 0px;
padding-left: 3px;
                           margin: 0px
                           }
                    .inline_pickers2 {display: flex}
                    .inline_pickers2 .shiny-input-container{
                     margin-right: 5px;
                    padding: 0px;
    }"
    )),
div(
  #actionLink(ns("save_bug"),"save bug",style="font-size:10px"),

  column(8,class="mp0",
         box_caret(ns("box_setup1"),
                   color="#374061ff",
                   inline=F,
                   title="Model Setup",
                   button_title =
                     switchInput(ns("mysupersom"),"supersom",size="mini", inline=T,labelWidth="75px",handleWidth="30px"),

                   div(

                     div(style="vertical-align: text-top;display: flex;",
                         div(style="display: flex;",
                             div(
                               uiOutput(ns("data_som_out")),

                             ),
                             div(style="margin-top: 25px",
                                 uiOutput(ns("saved_som_print")))
                         ),
                         uiOutput(ns("supersom_layers")),
                         popify(
                           div(class="save_changes",style="margin-top: 25px",
                               bsButton(ns("tools_savesom"), div(icon("fas fa-save")),style  = "animation: glowing 1000ms infinite;", type="action",value=FALSE)
                           )
                           , NULL, "Save the som model in the Datalist"
                         )
                     )
                   )

         )
  ),

  column(4,id=ns('som_models_panel'),
         uiOutput(ns("som_models_panel"))
  ),

  column(4,id=ns('partition_panel'),
         box_caret(ns('box_setup3'),
                   title="Partition",
                   color="#374061ff",
                   inline = F,
                   button_title =
                     switchInput(ns("usepartition"),"Use partition",size="mini", inline=T,labelWidth="75px",handleWidth="30px"),


                   div(id=ns("partition_on"),class="inline_pickers",
                       uiOutput(ns("data_somY_out"))

                   )))
)),

column(12,class="mp0",tabsetPanel(
  id = ns("som_tab"),

  #selected='som_tab3',

  tabPanel(
    value = "som_tab1",

    strong("1. Training"),

    div(
      column(6,class="mp0",
             box_caret(ns("box_setup4"),inline=F,
                       color="#c3cc74ff",
                       title="1.1. Set the grid",
                       tip=span(actionLink(ns("somgridhelp"), tipify(icon("fas fa-question-circle"), "Click for more details")),
                                actionLink(ns("resettopo"), icon("fas fa-undo"),style="position: absolute;right: 20px;top: 4px")),
                       div(
                         div(checkboxInput(ns("sugtopo"), span('suggested topology',tipify(actionLink(ns("sugtopohelp"), icon("fas fa-question-circle")), "Click for more details")), value =T)),
                         div(id = ns("topocontrol"),
                             div(style="display: flex;width: 100%",class="inline_pickers2",

                                 numericInput(ns("xdim"),"xdim",value =5,min = 0,step = 1),
                                 numericInput(ns("ydim"),"ydim",value = 5,min = 0,step = 1)
                                 ,
                                 pickerInput(ns("topo"),"Topology",choices = c("hexagonal", "rectangular")),
                                 pickerInput(ns("neighbourhood.fct"),label ="neigh.fct" ,choices = c("gaussian","bubble")),
                                 pickerInput(ns("toroidal"),label = "toroidal",choices = c(F, T))
                             ),
                             div(
                               style="text-align: center; width: 350px",
                               uiOutput(ns("showgrid"))
                             )
                         )



                       )
             )
      ),
      column(6,
             box_caret(
               ns("box_setup4"),
               color="#c3cc74ff",
               inline=F,
               title="1.2. Set the training parameters",
               tip=span(actionLink(ns("supersomhelp"), tipify(
                 icon("fas fa-question-circle"), "Click for more details"
               )),actionLink(ns("resetsom"), icon("fas fa-undo"),style="position: absolute;right: 20px;top: 4px")),
               div(
                 div(class="inline_pickers2",
                     pickerInput(ns("distmethod"),strong("dist.fcts",tipify(icon("fas fa-question-circle"),"Distance measure between each neuron and input data")),
                                 choices = c("BrayCurtis","euclidean","sumofsquares","manhattan","tanimoto")),
                     pickerInput(ns("normalizeDataLayers"),"normalizeDataLayers",
                                 choices = c("TRUE","FALSE")),
                     numericInput(ns("rlen"),strong("rlen",tipify(icon("fas fa-question-circle"),"The number of times the complete dataset will be presented to the network")),value =500,min = 1,step = 1),
                     numericInput(ns("seed"), strong("seed",tipify(icon("fas fa-question-circle"),"A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the som analysis.")), value =NA, min=0, step=1)
                 ),
                 div(align = "center",
                     br(),
                     actionButton(
                       ns("trainSOM"),
                       h4(icon("fas fa-braille"),"train SOM",icon("fas fa-arrow-circle-right")), style = "background: #05668D; color: white")
                 ),

                 div(
                   span(class="finesom_btn",
                        tipify(actionLink(ns("finesom"),"Fine tuning*"),"show all parameters available")
                   )),
                 div(id=ns("finetuning_som"),
                     div(id="finesom_out",class="map_control_style2",


                         div(style="display: flex",
                             numericInput(ns("a1"),
                                          label =span(tiphelp("Learning rate: two numbers indicating the amount of change. Not used for the batch algorithm.","left"), "Alpha:"),value = 0.05,step = 0.01),
                             numericInput(ns("a2"),label = NULL,value = 0.01,step = 0.01)
                         ),

                         div(style="display: flex",
                             numericInput(ns("r1"),
                                          label = span(tiphelp("the start and stop of the radius of the neighbourhood.  the radius will change linearly; as soon as the neighbourhood gets smaller than one only the winning unit will be updated.","left"),"Radius:"),value = 0),
                             numericInput(ns("r2"),
                                          label = NULL,value = 0,step = 0.01 )
                         ),
                         pickerInput(ns("mode"), span(tiphelp("type of learning algorithm","left"),"mode"), choices = c("online","batch", "pbatch")),
                         numericInput(ns("maxna"),span("maxNA.fraction", tiphelp("the maximal fraction of values that may be NA to prevent the row to be removed. Not applicable for BrayCurtis.","right")),value = 0.001,step = 0.01)
                     )

                 )
               )

             )
      ),
      bsTooltip(ns('resettopo'),"Reset parameters"),
      bsTooltip(ns('resetsom'),"Reset parameters")
    )

  ),
  tabPanel(
    value = "som_tab2",
    strong("2. Results"),
    div(
      style = "background: WhiteSmoke;",
      tabsetPanel(
        id=ns("som_res"),


        tabPanel(
          value= 'train_tab1',
          "2.1. Parameters",
          column(
            6,class="mp0",
            box_caret(
              ns("tbox1"),
              title="Quality Measures",
              tip=actionLink(ns("som_quality_help"),icon("fas fa-question-circle")),
              uiOutput(ns("som_errors"))
            ),
            box_caret(
              ns("tbox1"),
              title="Downloads",
              color="#c3cc74ff",
              div(
                div(div(tipify(actionLink(ns('create_codebook'),span("Create Datalist",icon("fas fa-file-signature")), style="button_active"),"Create a datalist  with the codebook vectors"))),
                div(tipify(actionLink(ns('save_bmu'),span("Save BMUs",icon("fas fa-file-signature")), style="button_active"),"Add the BMUs to the Factor-Attribute (training Data)")),
                div(actionLink(ns("down_pcodes_results"), span("Download codebook results"))),
                div(tipify(downloadLink(ns("down_kohonen_results"), "Download model"),"download kohonen object as rds file containing the object of class kohonen with components")))
            )
          ),
          column(
            6,class="mp0",
            box_caret(
              ns("tbox2"),
              title="Training Parameters",
              tableOutput(ns("train.summary"))
            )
          )
        ),
        tabPanel(
          "2.2. Changes & Counting",value = "train_tab2",
          column(6,class="mp0",box_caret(
            ns("ccbox1"),
            title="Changes",
            button_title = actionLink(ns("downp_pchanges"),"Download",icon('download')),
            plotOutput(ns("pchanges"))
          )),
          column(6,class="mp0",box_caret(
            ns("ccbox2"),
            title="Couting",
            button_title = actionLink(ns("downp_pcounts"),"Download",icon('download')),
            plotOutput(ns("pcounts"))
          ))

        ),
        tabPanel(

          "2.3. BMUs", value = "train_tab5",
          column(
            4,class="mp0",
            box_caret(
              ns("rbox1"),
              color="#c3cc74ff",
              title="Background",
              div(
                div(class="radio_search radio_yellow",
                    radioGroupButtons(ns("ss1_somback_value"), span("Unit value:"), choices = c("None"="None","U-Matrix"="uMatrix","Property"="property"))
                ),

                pickerInput(ns("ss1_property_layer"),label = "Layer:",choices = NULL),
                div(id=ns('ss1_var_pproperty'),style="display: flex",
                    actionButton(ns("ss1_prev_property"),"<<"),
                    pickerInput(ns("ss1_variable_pproperty"),label = "Variable:",choices = NULL),
                    actionButton(ns("ss1_next_property"),">>")

                ),

                pickerInput(inputId = ns("ss1_bg_palette"),"Palette",NULL),
                numericInput(ns("ss1_pcodes_bgalpha"), "Unit lightness",value = 0,min = 0,max = 1,step = .1),
                pickerInput(ns("ss1_pclus_border"),label ='Border:',NULL)
              )
            ),
            box_caret(
              ns("rbox2"),
              color="#c3cc74ff",
              title=span(style="display: inline-block",
                         class="checktitle",
                         checkboxInput(ns("ss1_pclus_addpoints"),strong("Points"),value=T,width="80px")
              ),
              div(
                div(id=ns("ss1_pclus_points_inputs"),
                    pickerInput(inputId = ns("ss1_pclus_points_palette"),
                                label ="Palette",
                                choices = NULL),
                    pickerInput(ns("ss1_pclus_points_factor"),"Factor",
                                choices = NULL),
                    pickerInput(inputId = ns("ss1_pclus_symbol"),
                                label = "Shape",
                                choices = NULL),
                    numericInput(ns("ss1_pclus_points_size"),"Size",value = 1,min = 0.1,max = 3,step = .1)
                )
              )
            ),
            box_caret(
              ns("rbox3"),
              color="#c3cc74ff",
              title=span(style="display: inline-block",
                         class="checktitle",
                         checkboxInput(ns("ss1_pclus_addtext"),strong("Labels"),value=F,width="80px")
              ),
              div(id=ns('ss1_pclus_text_inputs'),
                  pickerInput(inputId = ns("ss1_pclus_text_palette"),
                              label ="Palette",
                              choices =  NULL),
                  pickerInput(ns("ss1_pclus_text_factor"),"Factor",
                              choices = NULL),
                  numericInput(ns("ss1_pclus_text_size"),"Size",value = 1,min = 0.1,max = 3,step = .1)
              )
            ),
            box_caret(
              ns("rbox4"),
              color="#c3cc74ff",
              title=span(style="display: inline-block",
                         class="checktitle",
                         checkboxInput(ns("ss1_varfacmap_action"),strong("Variable factor map"),value=T,width="210px")
              ),
              tip = actionLink(ns("ss1_varfacmap"), tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details")),
              div(id=ns('ss1_varfac_out'),
                  pickerInput(ns("ss1_vfm_type"),"Show correlation:",
                              choices =list("Highest"='var', "Chull"="cor")

                  ),

                  div(id=ns('ss1_vfm_out'),
                      div(tipify(numericInput(ns("ss1_npic"), "Number", value = 10, min = 2),"Number of variables to display")),
                      numericInput(ns("ss1_pclus.cex.var"), "Size", value = 1, min = 2),
                      pickerInput(inputId = ns("ss1_p.clus.col.text"),
                                  label = "Color",
                                  NULL
                      ),
                      pickerInput(inputId = ns("ss1_var_bg"),
                                  label = "Background",
                                  choices =NULL),
                      numericInput(ns("ss1_var_bg_transp"), "Transparency", value = 0, min = 2,),
                      div(actionLink(ns('down_pcorr_results'),"Download VFM results")),
                      div(actionLink(ns('create_vfm_results'),"Create Datalist using VFM"))
                  )
              )
            ),
            box_caret(
              ns("rbox5"),
              title = "General options",
              color="#c3cc74ff",
              div(
                numericInput(ns("ss1_base_size"),"Base size",value = 12),
                checkboxInput(ns("ss1_theme"),
                              label = "show neuron coordinates",
                              value=F
                ),
                textInput(ns("ss1_title"), "Title: ", "")
              )
            )
          ),
          column(
            8,class="mp0",
            box_caret(
              ns('rbox6'),
              title="Best-Matching units",
              button_title = actionLink(ns('downp_bmu'),"Download",icon("download")),
              div(uiOutput(ns("bmu_plot")))
            )
          )
        ),
        tabPanel("2.4. Network plot", value = "train_tab_net",
                 column(
                   6,class="mp0",
                   box_caret(ns("box_setup5"),
                             color="#c3cc74ff",
                             title="Snap-Based Model Retraining",
                             tip=tipright("The panel enables users to recreate the model at pre-determined snaps. The snap parameter determines the iterations at which the model will be recreated. For example, if a model with 500 iterations is saved, setting snap to 250 will recreate the model at the 250th and 500th iterations."),
                             div(class="inline_pickers",
                                 numericInput(ns("rlen_snap2"), span(tipright("Number of snapshots or intermediate models to generate"), "snap"), 3),
                                 div(actionButton(ns("run_play2"), "Run >>",style="height: 30px;padding: 5px")),

                                 hidden(div(style="padding-top: 10px",
                                            class="anim_opt2",
                                            div(strong('Animation options')),
                                            numericInput(ns("play_interval2"), span(tipright("Interval between each training iteration in milliseconds."), "interval"), 100),
                                            checkboxInput(ns("loop2"), "Loop", FALSE)
                                 ))
                             )),

                   box_caret(ns("box_setup7"),
                             title="Plot options",
                             color="#c3cc74ff",
                             div(checkboxInput(ns("gwnp_show_labels"),"Show Neuron Labels",T),
                                 checkboxInput(ns("gwnp_show_points"),"Show Observations",F)),

                   )


                 ),
                 column(6,box_caret(ns("box_setup6"),
                                    inline=F,
                                    button_title = actionLink(ns("download_gwnp"),
                                                              "Download",icon("download")),
                                    title="Grid-Weighted Neuron Plot",
                                    div(uiOutput(ns("play_out2")),
                                        uiOutput(ns("plot_animation2")))
                 ))

        )



      )

    )

  ),
  tabPanel(
    value = "som_tab3",style="background: white",
    strong("3. Predict"),
    column(
      4,class="mp0",
      box_caret(
        ns("pred_header"),
        color="#c3cc74ff",
        title="New Data",
        div(
          div(class="radio_search radio_yellow",
              radioGroupButtons(ns("sompred_type"),"New data (X):",choices=c("Partition","Training","Datalist"))
          ),
          div(id=ns('sompred_type_datalist'),
              style="padding-left: 10px",
              div(strong("New data layers:")),
              uiOutput(ns("whatmap_newdata"))

          ),
          div(id=ns('run_predSOM_btn'),
              align="right",
              class="save_changes",
              actionButton(ns('run_predSOM'),"RUN >>",style="height: 25px; padding-top: 2px; padding-bottom: 2px;"))
        )

      ),
      div(
        uiOutput(ns("pred_result_options")),
        uiOutput(ns("pred_perf_options")),
        div(
          id=ns('pred_bmu_options'),
          box_caret(
            ns("pbox1"),
            color="#c3cc74ff",
            title="Background",
            div(
              div(class="radio_search radio_yellow",
                  radioGroupButtons(ns("ss2_somback_value"), span("Unit value:"), choices = c("None"="None","U-Matrix"="uMatrix","Property"="property"))
              ),

              pickerInput(ns("ss2_property_layer"),label = "Layer:",choices = NULL),
              div(id=ns('ss2_var_pproperty'),style="display: flex",
                  actionButton(ns("ss2_prev_property"),"<<"),
                  pickerInput(ns("ss2_variable_pproperty"),label = "Variable:",choices = NULL),
                  actionButton(ns("ss2_next_property"),">>")

              ),

              pickerInput(inputId = ns("ss2_bg_palette"),"Palette",NULL),
              numericInput(ns("ss2_pcodes_bgalpha"), "Unit lightness",value = 0,min = 0,max = 1,step = .1),
              pickerInput(ns("ss2_pclus_border"),label ='Border:',NULL)
            )
          ),
          box_caret(
            ns("pbox2"),
            color="#c3cc74ff",
            title=span(style="display: inline-block",
                       class="checktitle",
                       checkboxInput(ns("ss2_pclus_addpoints"),strong("Points"),value=T,width="80px")
            ),
            div(
              div(id=ns("ss2_pclus_points_inputs"),
                  pickerInput(inputId = ns("ss2_pclus_points_palette"),
                              label ="Palette",
                              choices = NULL),
                  pickerInput(ns("ss2_pclus_points_factor"),"Factor",
                              choices = NULL),
                  pickerInput(inputId = ns("ss2_pclus_symbol"),
                              label = "Shape",
                              choices = NULL),
                  numericInput(ns("ss2_pclus_points_size"),"Size",value = 1,min = 0.1,max = 3,step = .1)
              )
            )
          ),
          box_caret(
            ns("pbox3"),
            color="#c3cc74ff",
            title=span(style="display: inline-block",
                       class="checktitle",
                       checkboxInput(ns("ss2_pclus_addtext"),strong("Labels"),value=F,width="80px")
            ),
            div(id=ns('ss2_pclus_text_inputs'),
                pickerInput(inputId = ns("ss2_pclus_text_palette"),
                            label ="Palette",
                            choices =  NULL),
                pickerInput(ns("ss2_pclus_text_factor"),"Factor",
                            choices = NULL),
                numericInput(ns("ss2_pclus_text_size"),"Size",value = 1,min = 0.1,max = 3,step = .1)
            )
          ),
          box_caret(
            ns("pbox4"),
            color="#c3cc74ff",
            title=span(style="display: inline-block",
                       class="checktitle",
                       checkboxInput(ns("ss2_varfacmap_action"),strong("Variable factor map"),value=T,width="210px")
            ),
            tip = actionLink(ns("ss2_varfacmap"), tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details")),
            div(id=ns('ss2_varfac_out'),
                pickerInput(ns("ss2_vfm_type"),"Show correlation:",
                            choices =list("Highest"='var', "Chull"="cor")

                ),

                div(id=ns('ss2_vfm_out'),
                    div(tipify(numericInput(ns("ss2_npic"), "Number", value = 10, min = 2),"Number of variables to display")),
                    numericInput(ns("ss2_pclus.cex.var"), "Size", value = 1, min = 2),
                    pickerInput(inputId = ns("ss2_p.clus.col.text"),
                                label = "Color",
                                NULL
                    ),
                    pickerInput(inputId = ns("ss2_var_bg"),
                                label = "Background",
                                choices =NULL),
                    numericInput(ns("ss2_var_bg_transp"), "Transparency", value = 0, min = 2,),
                    div(actionLink(ns('down_pcorr_results_pred'),"Download VFM results")),
                    div(actionLink(ns('create_vfm_results_pred'),"Create Datalist using VFM"))
                )
            )
          ),
          box_caret(
            ns("pbox5"),
            title = "General options",
            color="#c3cc74ff",
            div(
              numericInput(ns("ss2_base_size"),"Base size",value = 12),
              checkboxInput(ns("ss2_theme"),
                            label = "show neuron coordinates",
                            value=F
              ),
              textInput(ns("ss2_title"), "Title: ", "")
            )
          )
        )

      )
    ),
    column(8,class="mp0",
           uiOutput(ns("teste")),
           tabsetPanel(
             id=ns("predsom_tab"),
             tabPanel(
               strong("3.1. Results"),style="background: white",
               value = "predsom_tab01",

               uiOutput(ns('som_predict_results'))

             ),
             tabPanel(
               strong("3.2. Performace"),style="background: white",
               value = "predsom_tab01b",
               uiOutput(ns("pred_performace"))
             ),
             tabPanel(
               strong("3.3. BMUs"),style="background: white",
               value = "predsom_tab01c",
               box_caret(
                 ns('pbox6'),
                 title="BMU plot",
                 button_title = actionLink(ns('download_pbox6'),"Download",icon("download")),
                 div(uiOutput(ns("predbmu_plot")))
               )
             )


           ),
           div(
             class="map_control_style",

             uiOutput(ns("newdata_summ"))

           )
    )

  )

))

  )

}
#' @export
imesc_supersom$server<-function (id,vals ){



  moduleServer(id,function(input, output, session){


    observeEvent(ignoreInit = T,input$down_pcorr_results,{
      vals$hand_down<-"generic"


      module_ui_downcenter("downcenter")
      name<-"Biplot results"
      data<- data.frame(vals$biplot_som)
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download VFM results",data=data, name=name)
    })



    box_caret_server("rbox1")
    box_caret_server("rbox2")
    box_caret_server("rbox3")
    box_caret_server("rbox4")
    box_caret_server("rbox5")
    box_caret_server("rbox6")
    box_caret_server("pbox1")
    box_caret_server("pbox2")
    box_caret_server("pbox3")
    box_caret_server("pbox4")
    box_caret_server("pbox5")
    box_caret_server("pbox6")
    ns<-session$ns

    output$pred_result_options<-renderUI({
      box_caret(
        ns('box_pred_results'),
        title="Options",
        color="#c3cc74ff",
        div(
          selectInput(ns('pred_results'),"Prediction results:",choices=c(
            "Best-matching units"='unit.classif',
            "Data"="predictions",
            "Codebook"="unit.predictions"
          ),selected=vals$cur_tab_pred_result),
          uiOutput(ns("pick_layer_result")),
          uiOutput(ns("ui_pred_result_newdata")),
          div(style="padding-top: 5px",
              uiOutput(ns("link_predsom_newdata")),
              uiOutput(ns("link_predsom_codebook"))
          )
        )

      )
    })
    output$pred_perf_options<-renderUI({})



    observe(shinyjs::toggle('pred_bmu_options',condition=input$predsom_tab=='predsom_tab01c'))


    observe(shinyjs::toggle('pred_result_options',condition=input$predsom_tab=='predsom_tab01'))




    output$whatmap_newdata<-renderUI({
      m<-current_som_model()
      datas<-attr(m,"Datalist")
      lapply(m$whatmap,function(w){
        choices<-names(which(sapply(vals$saved_data,function(x){
          all(colnames(x)%in%colnames(m$data[[w]]))
        })))
        if(w>1){
          choices=c("None",choices)
        }
        pickerInput(ns(paste0("newdata_w",w)),datas[[w]],choices)
      })
    })

    get_newdata<-reactive({
      req(input$sompred_type)
      req(input$sompred_type=="Datalist")
      m<-current_som_model()
      datas<-attr(m,"Datalist")
      newdata_names<-sapply(m$whatmap,function(w){
        req(input[[paste0("newdata_w",w)]])
        input[[paste0("newdata_w",w)]]
      })
      #newdata_names<-readRDS("newdata_names.rds")
      #newdata_names<-unlist(newdata_names)
      newdata_names<-newdata_names[which(newdata_names!="None")]
      news<-vals$saved_data[newdata_names]
      res<-lapply(news,as.matrix)
      names(res)<-names(m$data)[which(newdata_names!="None")]
      res
    })

    supersom_newdata<-reactive({
      req(input$sompred_type)
      m<-current_som_model()
      if(input$sompred_type%in%"Partition"){
        return(attr(m,"test"))
      }  else if(input$sompred_type%in%'Training'){
        return(current_som_model()$data)
      } else if(input$sompred_type%in%'Datalist'){
        return(get_newdata())
      }

    })
    get_sompred<-reactiveVal()


    observeEvent(current_som_model(),{
      get_sompred(NULL)
      shinyjs::addClass('run_predSOM_btn',"save_changes")
    })


    observeEvent(supersom_newdata(),{
      get_sompred(NULL)
      shinyjs::addClass('run_predSOM_btn',"save_changes")
    })


    observeEvent(input$run_predSOM,ignoreInit = T,{
      m<-current_som_model()
      req(m)
      newdata<-supersom_newdata()

      if(length(m$data)==1){
        names(newdata)<-NULL
      }
      whatmap<-NULL
      if(input$sompred_type%in%'Datalist'){
        newdata_names<-sapply(m$whatmap,function(w){
          req(input[[paste0("newdata_w",w)]])
          input[[paste0("newdata_w",w)]]
        })
        whatmap<-which(newdata_names!="None")
      }

      pred1<-predict(m,newdata,trainingdata = newdata,
                     whatmap = whatmap)
      pred<-predict(m,newdata,
                    #trainingdata = newdata,
                    whatmap = NULL)



      if(!is.null(whatmap)){
        pred$predictions[[whatmap]]<-pred1$predictions[[whatmap]]
        #names(pred$predictions)<-names(m$data)[whatmap]
        # names(pred$unit.predictions)<-names(m$data)[whatmap]
      } else{
        pred$predictions<-pred1$predictions
        names(pred$predictions)<-names(m$data)
        names(pred$unit.predictions)<-names(m$data)
      }
      get_sompred(pred)
      shinyjs::removeClass('run_predSOM_btn',"save_changes")
    })






    output$teste<-renderUI({
      # div("teste",style="height: 100px;overflow-y: auto",renderPrint(names(get_sompred()$predictions))     )
    })



    get_overhall_peform<-reactive({
      newdata<-supersom_newdata()
      pred<-get_sompred()$predictions
      m<-current_som_model()


      res<-lapply(names(newdata),function(i){
        postResample(pred[[i]],newdata[[i]])
      })
      names(res)<-names(newdata)

      do.call(rbind,res)
    })
    get_obs_peform<-reactive({
      #newdata<-readRDS("newdata.rds")
      #res<-readRDS("res.rds")
      newdata<-do.call(cbind,supersom_newdata())
      pred<-do.call(cbind,get_sompred()$predictions)


      obs_mean=apply(newdata,1,function(x) mean(x,na.rm=T))
      pred_mean= apply(pred,1,function(x) mean(x,na.rm=T))

      res<-data.frame(
        obs_mean=obs_mean,
        pred_mean=pred_mean,
        do.call(rbind,lapply(1:nrow(newdata),function(i){
          postResample(pred[i,],newdata[i,])
        }))
      )
      rownames(res)<-rownames(get_sompred()$predictions[[1]])
      res

    })
    get_var_peform<-reactive({
      #newdata<-readRDS("newdata.rds")
      #res<-readRDS("res.rds")
      newdata_list<-supersom_newdata()
      pred_list<-get_sompred()$predictions
      #saveRDS(newdata_list,'newdata.rds')
      #saveRDS(get_sompred(),'pred.rds')

      result<-lapply(seq_along(get_sompred()$predictions),function(layer){
        newdata<-newdata_list[[layer]]
        pred<-pred_list[[layer]]
        res<-data.frame(layer=names(get_sompred()$predictions)[layer],
                        variable=colnames(pred),
                        obs_mean=sapply(data.frame(newdata),function(x) mean(x,na.rm=T)),
                        pred_mean=sapply(data.frame(pred),function(x) mean(x,na.rm=T)),
                        do.call(rbind,
                                lapply(1:ncol(newdata),function(i){
                                  pr<-pred[,i]
                                  ob<-newdata[,i]

                                  data.frame(
                                    #rmse_means=RMSE(mean(pr,na.rm=T),mean(ob,na.rm=T)),

                                    as.list(postResample(pr,ob)))
                                })))
        #unlist(sapply(get_sompred()$predictions,colnames)) |>  print()



        res
      })
      do.call(rbind,result)






    })

    get_pred_newdata<-reactive({
      req(input$layer_result)
      req(input$layer_result%in%names(get_sompred()$predictions))
      get_sompred()$predictions[[input$layer_result]]
    })

    output$ui_pred_result_newdata<-renderUI({
      req(input$layer_result)
      req(input$tab_pred_result)
      table<-get_pred_newdata()
      div(class="half-drop-inline",
          fixed_dt_con$ui(ns('pred_results'),table,max_length=100)
      )

    })


    output$out_pred_result_newdata<-renderUI({
      table<-get_pred_newdata()
      fixed_dt_con$server('pred_results',table,max_length=100)
    })

    get_pred_bmu<-reactive({
      res<-get_sompred()
      res_temp<-data.frame(bmu=res[['unit.classif']])
      rownames(res_temp)<-rownames(res[["predictions"]][[1]])
      res_temp
    })

    output$out_pred_result_bmu<-renderUI({
      #req(input$tab_pred_result)
      table<-get_pred_bmu()
      div(class="half-drop-inline",
          fixed_dt(table,dom='lt',pageLength=20)
      )

    })
    get_pred_codebook<-reactive({
      req(input$layer_result)
      res<-get_sompred()
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }
      req(input$layer_result%in%names(res[["unit.predictions"]]))
      res<-res[["unit.predictions"]][[input$layer_result]]
      rownames(res)<-paste0("neu",1:nrow(res))
      res
    })

    observeEvent(input$tab_pred_result,{
      table<-switch(input$tab_pred_result,
                    "unit.classif"=get_pred_bmu(),
                    "predictions"=get_pred_newdata(),
                    "unit.predictions"=get_pred_codebook()
      )
      vals$som_predict_result<-table

    })
    output$out_pred_result_codebook<-renderUI({
      table<-get_pred_codebook()
      fixed_dt_con$server('pred_results',table,max_length=100)
    })

    output$data_som_out<-renderUI({
      pickerInput(ns("data_som"),uiOutput(ns('label_data_som')),choices =names(vals$saved_data), selected=vals$cur_data)
    })

    output$data_somY_out<-renderUI({
      div(
        pickerInput(ns("data_somY"),"Datalist:",choices = get_match_datalists()),
        uiOutput(ns("partition_column"))
      )
    })



    get_datalist_for_som<-reactive({
      if(isTRUE(input$mysupersom)){
        get_training_list()[[1]]
      } else{
        req(input$data_som)
        vals$saved_data[[input$data_som]]
      }
    })


    get_match_datalists<-reactive({
      data<-get_datalist_for_som()

      truedatalists<-sapply(vals$saved_data,function(x){
        all(rownames(x)%in%rownames(data))
      })
      names(vals$saved_data)[truedatalists]
    })





    output$partition_column<-renderUI({
      req(input$data_somY)
      choices=c(colnames(attr(vals$saved_data[[input$data_somY]],"factors")))
      selected=vals$cur_partition_column
      selected<-get_selected_from_choices(selected,choices)
      div(
        pickerInput(ns("partition_column"),span("Partition:",tiphelp("choose a factor as reference for the partition")), choices,selected=selected),
        uiOutput(ns("partition_test"))

      )
    })

    output$partition_test<-renderUI({
      req(input$data_somY)
      req(input$partition_column)
      fac<-attr(vals$saved_data[[input$data_somY]],"factors")[,input$partition_column]
      choices<-levels(fac)
      selected=vals$cur_partition_ref
      selected<-get_selected_from_choices(selected,choices)
      pickerInput(ns("partition_ref"),span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions")), choices=choices,selected=selected)
    })

    box_caret_server("box_setup1")
    box_caret_server("box_setup2")
    box_caret_server("box_setup3")
    box_caret_server("box_setup4")
    box_caret_server("box_setup5")
    box_caret_server("box_setup6")
    box_caret_server("box_setup7")
    box_caret_server("box_setup8")
    output$showgrid<-renderUI({
      dim = try(topo.reactive(),silent=T )
      if(inherits(dim,"try-error")){updateCheckboxInput(session,"sugtopo",value=F)}
      validate(need(!inherits(dim,"try-error"),"Error: suggested topology has been disabled; check for inconsistency in data, such as columns with variance 0. "))

      renderPlot({
        if(isTRUE(input$splitdata_som)){data=training_data$df}else{
          data = getdata_som()}
        validate(need(input$xdim!="", ""))
        validate(need(input$ydim!="", ""))
        validate(need( (input$xdim*input$ydim)<=nrow(data), "The number of map units must be less than or equal to the number of observations. Please decrease the 'xdim' and/or 'ydim' dimensions"))
        try({
          par(mar = c(0, 0, 0, 0))

          grid<-kohonen::somgrid(input$xdim, input$ydim, topo = input$topo, neighbourhood.fct=input$neighbourhood.fct, toroidal=toroidal())
          plot.som_grid(grid)
        },silent=T )
      },
      width = 200,
      height = 200)
    })
    output$play_out2<-renderUI({
      req(modelplay2())


      div(
        div(sliderInput(session$ns("animation2"), "Animation:",
                        min = 1, max = length(modelplay2()),
                        value = 1, step = 1,
                        animate =
                          animationOptions(interval = input$play_interval2,
                                           playButton=tags$div(
                                             tags$span("Play >> ",style="font-size: 14px;font-weight: bold; background:  #05668D;color: white; padding: 3px;"),style=" margin-top: 5px"
                                           ),
                                           loop = input$loop2)))
      )
    })
    modelplay2<-reactiveVal(NULL)
    get_plot_animation2<-reactive({
      if(is.null(modelplay2())){
        plotnetwork_list2(current_som_model(), label=input$gwnp_show_labels,show_points=input$gwnp_show_points)
      } else {

        plotnetwork_list2(modelplay2()[[input$animation2]], label=input$gwnp_show_labels,show_points=input$gwnp_show_points)


      }

    })
    output$plot_animation2<-renderUI({
      renderPlot({get_plot_animation2()})

    })
    data_example<-reactive({
      switch(input$data_example,
             "nema_araca"={
               as.matrix(data.frame(fread("inst/www/nema_araca.csv"))[-1])
             },
             "nema_hellinguer"={
               data<-data.frame(fread("inst/www/nema_araca.csv"))[-1]
               as.matrix(decostand(data,"hell"))
             },
             "envi_araca"={

               as.matrix(data.frame(fread("inst/www/envi_araca.csv"))[-1])

             },
             "envi_araca_scaled"={

               data<-data.frame(fread("inst/www/nema_araca.csv"))[-1]
               scale(data)

             },
             "user-defined"=as.matrix(getdata_som())

      )
    })
    modelplay<-reactiveVal()
    output$play_out<-renderUI({
      req(modelplay())


      div(
        div(sliderInput(session$ns("animation"), "Animation:",
                        min = 1, max = length(modelplay()),
                        value = 1, step = 1,
                        animate =
                          animationOptions(interval = input$play_interval,
                                           playButton=tags$div(
                                             tags$span("Play >> ",style="font-size: 14px;font-weight: bold; background:  #05668D;color: white; padding: 3px;"),style=" margin-top: 5px"
                                           ),
                                           loop = input$loop))),
        uiOutput(ns("plot_animation"))
      )
    })
    output$plot_animation<-renderUI({
      renderPlot({
        plotnetwork_list2(modelplay()[[input$animation]])

      })
    })
    # Define the scatter_hexagon function
    scatter_hexagon<-function(x, y, radius = 0.1, fill="gray") {
      # Create a scatter plot

      # Draw hexagons around each point
      for(i in 1:length(x)) {
        draw_hexagon(x[i], y[i], radius, fill[i])
      }
    }
    # Define draw_hexagon function
    draw_hexagon<-function(x, y, radius,fill) {
      angles<-seq(0, 2*pi, length.out = 7) + pi/6  # Rotated by 90 degrees
      x_hex<-x + radius * cos(angles)
      y_hex<-y + radius * sin(angles)

      polygon(x_hex, y_hex,col=fill, border=fill)
    }
    plotnetwork_list2<-function(m,label=T, main="", show_points=T){
      col<-"Gray"
      mds<-lapply(m$codes,function(x){
        x<-sqrt(x^2)
        res<-m$grid$pts*apply(x,1,mean)
        # res<-m$grid$pts*apply(x,1,sum)
        res[,1]<-scales::rescale(res[,1],range(m$grid$pts[,1]))
        res[,2]<-scales::rescale(res[,2],range(m$grid$pts[,2]))
        res
      })
      matdis2<-matdis<-unit.distances(m$grid)
      dec<-decimalplaces(max(matdis2))
      matdis2<-round(matdis2,dec)
      neighs<-lapply(1:nrow(matdis2),function(i){
        which(matdis2[i,]==1)
      })


      dist<-data.frame(as.matrix(object.distances(m)))
      dist$unit<-m$unit.classif
      splited<-split(dist,dist$unit)

      mean_diss<-sapply(as.character(1:nrow(m$grid$pts)),function(i){
        x<-  splited[[i]]
        suppressWarnings(mean(unlist(x)))
      })



      cut<-cut(mean_diss,breaks=nrow(m$grid$pts))
      cols<-viridis(nlevels(cut))[cut]

      cols[   which(is.na(mean_diss))]<-"darkgray"

      res<-data.frame(do.call(rbind,mds))
      som_qualist<-errors_som(m)

      plot(res,type="n", ann=F,axes=F        )
      title(main= paste0("Interaction: ",nrow(m$changes)),"\n",
            paste("err.quant:",round(as.numeric(unlist(som_qualist$som_quality[1,1])),3)))

      for(j in 1:length(mds)){
        for(i in 1:length(neighs)) {
          nei<-neighs[[i]]
          p0<-mds[[j]][i,]
          p1<-mds[[j]][nei,]
          # points(p0[1],p0[2], pch=16, col=col[j],cex=2)
          segments(  p0[1], p0[2], p1[,1], p1[,2],col=col[j])


        }


      }

      kohonen:::plot.kohmapping

      res$unit<-NA
      res$unit[m$unit.classif]<-m$unit.classif

      points<-do.call(rbind,lapply(1:nrow(res),function(i){
        x<-res[i,]
        n<-sum(m$unit.classif%in%i)
        data.frame(x=x$x+ rnorm(n,0,0.06),
                   y=x$y+ rnorm(n,0,0.06))


      }))


      if(m$grid$topo=="rectangular"){
        points(res, xlab="Distance", ylab="Distance",
               pch=15,main=main,col=cols,cex=3)
      } else{
        scatter_hexagon(res$x, res$y, radius = 0.2, fill=cols)

      }


      if(isTRUE(show_points)){
        points(points[1:2],pch=16,cex=.8)
      }
      if(isTRUE(label)){
        text(res$x,res$y, labels=1:nrow(res), col="white")
      }
    }

    ns<-session$ns
    ns_som3<-NS('predsom')
    ns_som2<-NS('som2')
    ns_som<-NS('som')


    output$label_data_som<-renderUI({
      get_label_datasom()
    })
    get_label_datasom<-reactive({
      label<-"~ Training Datalist:"
      if(isTRUE(input$mysupersom)&input$som_tab!="som_tab1"){
        label<-span("Target Datalist",tiphelp("the supersom model will always be saved in the datalist selected in the first layer.",placement ="right"))
      }
      label
    })


    get_training_list<-reactive({

      layer_table<-ssom_reac()
      layers<-vals$saved_data[layer_table$Datalist]
      layers<-lapply(layers,function(x){
        x[rownames(layers[[1]]),]
      })
      training_list<-lapply(layers,function(x) as.matrix(x))
      training_list
    })
    ssom_reac<-reactive({
      req(length(vals$ssom_tab0)>1)
      req(nrow(vals$ssom_tab0)>1)

      req(!is.null(vals$ssom_tab))
      req(length(vals$ssom_tab)==nrow(vals$ssom_tab0))

      Datalist=c(sapply(1:length(vals$ssom_tab), function(i) input[[paste0("ssom_layer", i)]]))
      Weights=c(sapply(1:length(vals$ssom_tab), function(i) input[[paste0("ssom_wei", i)]]))
      Distances=c(sapply(1:length(vals$ssom_tab), function(i) input[[paste0("ssom_dist", i)]]))



      inp_list<-list(Datalist=Datalist,
                     Weights=Weights,
                     Distances=Distances

      )
      notgo<-any(unlist(lapply(inp_list,function(x){
        lapply(x,function(i){
          length(i)

        })
      }))==0)
      req(isFALSE(notgo))
      res<- data.frame(
        Datalist=Datalist,
        Weights=Weights,
        Distances=Distances)
      req(nrow(res)>0)

      res

    })
    somval<-reactiveValues(df=F)

    output$supersom_layers<-renderUI({
      div(class=show_dataX(),
          div("~ Training layers"),
          column(12,uiOutput(ns('ssom_table')))
      )
    })
    output$ssom_table<-renderUI({
      res<-vals$ssom_tab

      column(12,class="small_picker",style="background-color: white",
             splitLayout(
               div(

                 div(strong("Layers"),class="numlayers"),
                 div(strong("Datalist"), class="ensemble_labels", style="color: #05668D"),
                 div(strong("Weights"), class="layers_wei", style="color: #05668D"),
                 div(strong("Distances"), class="layers_dist", style="color: #05668D"),

                 div( class="layers_wei",
                      actionLink(ns('supersom_reset'),span(icon("fas fa-undo"),"reset"))
                 )


               )
             ),
             res,
             div(style="padding: 0px; padding-left: 10px",
                 tipify(actionLink(ns("ssom_add"),icon("fas fa-plus")),"Add layer","right"))

      )
    })
    output$som_cur<-renderUI({
      req(input$som_tab!="som_tab1")
      div(
        div(strong("X:"), em(paste0(names(current_som_model()$data),collapse = "; "))),
        div(strong("Method:"),em(attr(current_som_model(),"Method"))),
        div(strong("Som-Attribute:"), em(input$som_models))
      )
    })
    show_dataX<-reactive({
      req(input$som_tab)
      req(length(input$mysupersom)>0)
      req(isTRUE(input$mysupersom))


      if(input$som_tab!='som_tab1'){
        class='som_class'
      } else{
        class='som_class0'
      }
    })





    getinit_supersom<-reactive({
      name1<-names(vals$saved_data)[1]
      choices_datalist<-unlist(lapply(vals$saved_data[-1],function(x){
        sum( rownames(x)%in%rownames(vals$saved_data[[name1]]))==nrow(x)
      }))
      name2<-names(choices_datalist)[which(choices_datalist)][1]
      ssom_input<-data.frame(Datalist=c(name1,name2),
                             Weights= 0.5,
                             Distances="euclidean")
      rownames(ssom_input)<-paste0("Layer",1:nrow(ssom_input))
      ssom_input

    })
    output$ssom_df = renderUI({
      div(

        renderPrint(ssom_reac()),


      )
    })



    symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
    df_symbol<-data.frame(
      val = c(16,15,17,18,8,1,5,3)
    )
    for(i in 1:length(symbols))
    {
      symbol1<-base64enc::dataURI(file = paste0('inst/www/pch',i,".png"), mime = "image/png")

      df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}



    newdata_som<-reactive({
      req(input$sompred_type)
      m<-current_som_model()
      newdata<-switch(input$sompred_type,
                      "Partition"={
                        attr(m,"test")},
                      "Datalist"={
                        req(input$predsom_new)
                        vals$saved_data[[input$predsom_new]]
                      },
                      "Training"={current_som_model()$data})


      if(length(newdata)==1){
        newdata<-newdata[[1]]
      }


      newdata
    })





    output$pick_layer_result<-renderUI({
      req(input$sompred_type)
      #req(input$sompred_type%in%c("Partition","Training"))

      #req(input$tab_pred_result%in%c("predictions","unit.predictions"))
      m<-current_som_model()
      req(inherits(m,"kohonen"))

      req(get_sompred())
      pred<-get_sompred()
      choices=names(pred$predictions)

      div(

        selectInput(ns("layer_result"), "Show layer:",choices=choices, selected=vals$cur_layer_result)
      )
    })

    observe({

      shinyjs::toggle('ui_pred_result_newdata',condition=input$tab_pred_result%in%c('predictions','unit.predictions'))
      shinyjs::toggle('pick_layer_result',condition=input$tab_pred_result%in%c('predictions','unit.predictions'))
    })



    {
      output$ss1_plus_umatrix<-renderUI({
        if(is.null(vals$plus_umatrix)){vals$plus_umatrix<-F}
        checkboxInput(ns("ss1_plus_umatrix"),strong("+ U-Matrix:"), value=vals$plus_umatrix)
      })
      ss1_getdata_layer<-reactive({
        choices0 = names(getsom()$data)
        if(length(choices0)>0){
          req(input$ss1_property_layer)
          getsom()$data[[input$ss1_property_layer]]
        } else{
          getsom()$data[[1]]
        }

      })
      ss1_indicate_hc<-reactive({
        npic<-NULL
        indicate<-NULL
        if(isTRUE(input$ss1_varfacmap_action)){

          npic<- input$ss1_npic
          indicate<- input$ss1_vfm_type
        }
        iind=list(indicate=indicate,npic=npic)
        iind
      })
      ss1_bp_som<-reactive({
        iind=ss1_indicate_hc()
        m<-getsom()
        bp<-getbp_som2(m=m,indicate=iind$indicate,npic=iind$npic,hc=vals$cutsom)
        vals$ss1_bp_som<-bp
        bp
      })
      ss1_get_network<-reactive({
        req(input$ss1_somback_value)
        backtype=NULL
        property=NULL
        if(input$ss1_somback_value=="property"){
          req(input$ss1_variable_pproperty)
          backtype="property"
          property=input$ss1_variable_pproperty
        } else if(input$ss1_somback_value=="uMatrix"){
          backtype="uMatrix"
        }



        m<-getsom()
        hexs<-get_neurons(m,background_type=backtype,property=property, hc=NULL)
        vals$ss1_hc_network<-hexs
        hexs
      })
      ss1_get_copoints<-reactive({
        m<-getsom()
        copoints<-getcopoints(m)
        vals$copoints_hc<-copoints
        copoints
      })
      ss1_copoints_scaled<-reactive({
        ss1_get_network()
        ss1_get_copoints()
        points_tomap=rescale_copoints(hexs=vals$ss1_hc_network,copoints=vals$copoints_hc)
        data<-vals$saved_data[[input$data_som]]

        factors<-attr(data,"factors")
        if(length(input$ss1_pclus_text_factor)>0){
          req(input$ss1_pclus_text_factor%in%colnames(factors))
          text_factor= factors[rownames(data),input$ss1_pclus_text_factor, drop=F]
          points_tomap$label<-text_factor[rownames(points_tomap),]
        }
        if(length(input$ss1_pclus_points_factor)>0){
          req(input$ss1_pclus_points_factor%in%colnames(factors))
          points_factor= factors[rownames(data),input$ss1_pclus_points_factor, drop=F]
          points_tomap$point<-points_factor[rownames(points_tomap),]
          attr(points_tomap,"namepoints")<-input$ss1_pclus_points_factor
        }
        vals$ss1_hc_mapsom<-points_tomap
        points_tomap
      })
      ss1_get_choices_pal<-reactive({

        req(input$ss1_somback_value)
        title="+ Unit palette"
        if(input$ss1_somback_value=="None"){

          choices=getsolid_col()
        } else {


          choices=getgrad_col()

        }


        attr(choices,"title")<-title
        choices
      })


      ss1_argsplot<-reactive({

        req(input$ss1_pcodes_bgalpha)
        if(isTRUE(input$ss1_pclus_addpoints)){
          req(input$ss1_pclus_points_factor)
          points_factor= NULL }
        if(isTRUE(input$ss1_pclus_addtext)){
          req(input$ss1_pclus_text_factor)
          text_factor= NULL }

        indicate=ss1_indicate_hc()


        m<-current_som_model()

        tryco<-try(ss1_copoints_scaled(), silent = T)

        req(!inherits(tryco,"try-error"))
        #savereac()
        trybp<-try( ss1_bp_som(), silent = T)

        req(!inherits(trybp,"try-error"))

        errors<-NULL




        copoints2<-vals$copoints2
        copoints3<-copoints2
        #opoints2$point<-args$points_factor
        #attach(vals$args)


        args<-list(m=m,
                   hexs=vals$ss1_hc_network,
                   points_tomap=vals$ss1_hc_mapsom,
                   bp=vals$ss1_bp_som,
                   points=input$ss1_pclus_addpoints,
                   points_size=input$ss1_pclus_points_size,
                   points_palette=input$ss1_pclus_points_palette,
                   pch=as.numeric(input$ss1_pclus_symbol),
                   text=input$ss1_pclus_addtext,
                   text_size=input$ss1_pclus_text_size,
                   text_palette=input$ss1_pclus_text_palette,
                   bg_palette=input$ss1_bg_palette,
                   newcolhabs=vals$newcolhabs,
                   bgalpha=input$ss1_pcodes_bgalpha,
                   border=input$ss1_pclus_border,
                   indicate=indicate$indicate,
                   cex.var=as.numeric(input$ss1_pclus.cex.var),
                   col.text=input$ss1_p.clus.col.text,
                   col.bg.var=input$ss1_var_bg,
                   col.bg.var.alpha=1-input$ss1_var_bg_transp,
                   show_error=errors,
                   base_size=input$ss1_base_size,
                   show_neucoords=input$ss1_theme,
                   newdata=input$ss1_newdata,
                   title=input$ss1_title,
                   hc=somval$hc
        )

        args

      })
    }

    output$bmu_plot<-renderUI({


      renderPlot({

        args<-ss1_argsplot()

        bp<-args$bp
        bp$id=NULL
        vals$biplot_som<-bp
        args$points_palette

        args
        vals$bmus_plot<-do.call(bmu_plot,args)
        vals$bmus_plot
      })

    })


    {










      output$ss2_plus_umatrix<-renderUI({
        if(is.null(vals$plus_umatrix)){vals$plus_umatrix<-F}
        checkboxInput(ns("ss2_plus_umatrix"),strong("+ U-Matrix:"), value=vals$plus_umatrix)
      })




      ss2_getdata_layer<-reactive({
        choices0 = names(getsom()$data)
        if(length(choices0)>0){
          req(input$ss2_property_layer)
          getsom()$data[[input$ss2_property_layer]]
        } else{
          getsom()$data[[1]]
        }

      })

      ss2_indicate_hc<-reactive({
        npic<-NULL
        indicate<-NULL
        if(isTRUE(input$ss2_varfacmap_action)){

          npic<- input$ss2_npic
          indicate<- input$ss2_vfm_type
        }
        iind=list(indicate=indicate,npic=npic)
        iind
      })
      ss2_bp_som<-reactive({
        iind=ss2_indicate_hc()
        m<-getsom()
        bp<-getbp_som2(m=m,indicate=iind$indicate,npic=iind$npic,hc=vals$cutsom)
        vals$ss2_bp_som<-bp
        bp
      })
      ss2_get_network<-reactive({
        req(input$ss2_somback_value)
        backtype=NULL
        property=NULL
        if(input$ss2_somback_value=="property"){
          req(input$ss2_variable_pproperty)
          backtype="property"
          property=input$ss2_variable_pproperty
        } else if(input$ss2_somback_value=="uMatrix"){
          backtype="uMatrix"
        }



        m<-getsom()
        hexs<-get_neurons(m,background_type=backtype,property=property, hc=NULL)
        vals$ss2_hc_network<-hexs
        hexs
      })
      ss2_get_copoints<-reactive({
        m<-get_som_model_pred()
        copoints<-getcopoints(m)
        vals$copoints_hc2<-copoints
        copoints
      })
      ss2_copoints_scaled<-reactive({
        ss2_get_network()
        ss2_get_copoints()
        points_tomap=rescale_copoints(hexs=vals$ss2_hc_network,copoints=vals$copoints_hc2)
        data<-vals$saved_data[[input$data_som]]

        factors<-attr(data,"factors")
        if(length(input$ss2_pclus_text_factor)>0){
          req(input$ss2_pclus_text_factor%in%colnames(factors))
          text_factor= factors[rownames(data),input$ss2_pclus_text_factor, drop=F]
          points_tomap$label<-text_factor[rownames(points_tomap),]
        }
        if(length(input$ss2_pclus_points_factor)>0){
          req(input$ss2_pclus_points_factor%in%colnames(factors))
          points_factor= factors[rownames(data),input$ss2_pclus_points_factor, drop=F]
          points_tomap$point<-points_factor[rownames(points_tomap),]
          attr(points_tomap,"namepoints")<-input$ss2_pclus_points_factor
        }
        vals$ss2_hc_mapsom<-points_tomap
        points_tomap
      })
      ss2_get_choices_pal<-reactive({


        req(length(input$ss2_somback_value)>0)
        title="+ Unit palette"
        if(input$ss2_somback_value=="None"){
          vals$somplot_bg<-"gray"
          choices=getsolid_col()
        } else {

          vals$somplot_bg<-"viridis"
          choices=getgrad_col()

        }


        attr(choices,"title")<-title
        choices
      })







      ss2_argsplot<-reactive({

        req(input$ss2_pcodes_bgalpha)
        if(isTRUE(input$ss2_pclus_addpoints)){
          req(input$ss2_pclus_points_factor)
          points_factor= NULL }
        if(isTRUE(input$ss2_pclus_addtext)){
          req(input$ss2_pclus_text_factor)
          text_factor= NULL }

        indicate=ss2_indicate_hc()


        m<-current_som_model()

        tryco<-try(ss2_copoints_scaled(), silent = F)

        req(!inherits(tryco,"try-error"))
        #savereac()
        trybp<-try( ss2_bp_som(), silent = T)

        req(!inherits(trybp,"try-error"))

        errors<-NULL




        copoints2<-vals$copoints2
        copoints3<-copoints2
        #opoints2$point<-args$points_factor
        #attach(vals$args)


        args<-list(m=m,
                   hexs=vals$ss2_hc_network,
                   points_tomap=vals$ss2_hc_mapsom,
                   bp=vals$ss2_bp_som,
                   points=input$ss2_pclus_addpoints,
                   points_size=input$ss2_pclus_points_size,
                   points_palette=input$ss2_pclus_points_palette,
                   pch=as.numeric(input$ss2_pclus_symbol),
                   text=input$ss2_pclus_addtext,
                   text_size=input$ss2_pclus_text_size,
                   text_palette=input$ss2_pclus_text_palette,
                   bg_palette=input$ss2_bg_palette,
                   newcolhabs=vals$newcolhabs,
                   bgalpha=input$ss2_pcodes_bgalpha,
                   border=input$ss2_pclus_border,
                   indicate=indicate$indicate,
                   cex.var=as.numeric(input$ss2_pclus.cex.var),
                   col.text=input$ss2_p.clus.col.text,
                   col.bg.var=input$ss2_var_bg,
                   col.bg.var.alpha=1-input$ss2_var_bg_transp,
                   show_error=errors,
                   base_size=input$ss2_base_size,
                   show_neucoords=input$ss2_theme,
                   newdata=input$ss2_newdata,
                   title=input$ss2_title,
                   hc=somval$hc
        )

        args

      })
    }


    getbmu_plot<-reactive({
      args<-ss2_argsplot()
      #saveRDS(args,"args.rds")

      req(length(args)>0)
      bp<-args$bp
      bp$id=NULL
      vals$biplot_som<-bp
      do.call(bmu_plot,args)
    })

    output$predbmu_plot<-renderUI({
      renderPlot({getbmu_plot()})

    })

    observeEvent(input$pred_performace,ignoreInit = T,{
      updateTabsetPanel(session,'get_predsom_perf',selected=input$pred_performace)
    })
    output$pred_performace<-renderUI({
      box_caret(
        ns("box_pred_perf_out"),
        title="Table",
        button_title = uiOutput(ns('header_perf')),
        tabsetPanel(
          id=ns("get_predsom_perf"),

          tabPanel(
            value="predsom_perf_overhall",
            title = "Overall performace",
            uiOutput(ns("overhall_peform"))),
          tabPanel(
            value="predsom_perf_obs",
            title = "Performace by observation",
            uiOutput(ns("obs_peform"))
          ),
          tabPanel(
            value="predsom_perf_var",
            title = "Performace by variable",
            uiOutput(ns("var_peform"))
          )
        )
      )


    })
    output$obs_peform<-renderUI({
      vals$som_perf_result<-table<-get_obs_peform()
      div(class="half-drop-inline",
          fixed_dt(table,dom='lt',pageLength=20)
      )




    })
    output$var_peform<-renderUI({
      vals$som_perf_result<-table<-get_var_peform()
      div(class="half-drop-inline",
          fixed_dt(table,dom='lt',pageLength=20)
      )




    })
    output$overhall_peform<-renderUI({
      vals$som_perf_result<-table<-get_overhall_peform()
      div(class="half-drop-inline",
          fixed_dt(table,dom='t')
      )


    })









    output$header_perf<-renderUI({
      div(
        actionLink(ns('downtable_perf_result'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-table")))
        ),
        uiOutput(ns("link_predsom_perf_obs"))

      )
    })
    output$link_predsom_perf_obs<-renderUI({
      req(input$get_predsom_perf=="predsom_perf_obs")
      actionLink(ns('link_predsom_perf_obs_create'),span("+ Create a Datalist"))
    })
    output$link_predsom_newdata<-renderUI({
      req(input$tab_pred_result=="predictions")
      actionLink(ns('link_predsom_newdata_create'),span("+ Create a Datalist"))
    })
    output$link_predsom_codebook<-renderUI({
      req(input$tab_pred_result=="unit.predictions")
      actionLink(ns('link_predsom_codebook_create'),span("+ Create a Datalist"))
    })
    create_predsom_codebook<-reactive({
      temp<-data.frame(vals$som_predict_result)
      attr(temp,"factors")<-data.frame(id=rownames(temp))
      coords<-data.frame(current_som_model()$grid$pts)
      colnames(coords)<-c("x","y")
      rownames(coords)<-rownames(temp)
      attr(temp,"coords")<-coords
      if(input$hand_save=="create") {
        vals$saved_data[[input$newdatalist]]<-temp
      } else{
        vals$saved_data[[input$over_datalist]]<-temp
      }
    })
    create_predsom_newdata<-reactive({
      data_o<-switch(
        input$sompred_type,
        "Partition"={
          vals$saved_data[[input$data_som]][rownames(attr(current_som_model(),"test")[[1]]),]
        },
        "Datalist"={vals$saved_data[[input$predsom_new]]},
        "Training"={vals$saved_data[[input$data_som]]}

      )

      temp<-vals$som_predict_result
      if(input$hand_save=="create") {
        temp<-data_migrate(data_o,temp,input$newdatalist)
        vals$saved_data[[input$newdatalist]]<-temp
      } else{
        temp<-data_migrate(data_o,temp,input$over_datalist)
        vals$saved_data[[input$over_datalist]]<-temp
      }

    })

    observeEvent(input$pred_results,ignoreInit = T,{
      updateTabsetPanel(session,'tab_pred_result',selected =input$pred_results)
    })
    output$som_predict_results<-renderUI({
      #req(get_sompred())
      box_caret(ns("box_pred_results_out"),

                title="Table",
                button_title = div(

                  div(
                    actionLink(ns('downtable_predict_result'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-table"))))
                  )
                ),
                tabsetPanel(
                  type="hidden",
                  id=ns("tab_pred_result"),
                  tabPanel(
                    title = "Best-matching units",
                    value="unit.classif",
                    uiOutput(ns("out_pred_result_bmu"))
                  ),
                  tabPanel(
                    title = "New Data (X)",
                    value="predictions",

                    uiOutput(ns("out_pred_result_newdata"))
                  ),
                  tabPanel(
                    title = "Codebook",
                    value="unit.predictions",
                    uiOutput(ns('ui_pred_result_codebook')),
                    uiOutput(ns("out_pred_result_codebook"))
                  )
                ))

    })





    palette=c(
      "turbo",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"
    )
    symbols<-c("pch1","pch2","pch3","pch4")

    dfcolors<-data.frame(
      val = palette
    )


    saved_sompred<-reactiveValues()
    re<-reactiveValues(df=NULL)



    observeEvent(ignoreInit = T,input$downp_pchanges,{

      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=g_pchanges()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Changes plot", name_c="som_changes")
    })

    observeEvent(ignoreInit = T,input$downp_pcounts,{

      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=g_pcounts()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Counting plot", name_c="som_counting")
    })
    g_pchanges<-reactive({
      pchanges(current_som_model())
      res<-recordPlot()
      res
    })
    g_pcounts<-reactive({
      pcounts(current_som_model())
      res<-recordPlot()
      res
    })
    output$pchanges<-renderPlot({
      req(length(current_som_model())>0)
      g_pchanges()
    })
    output$pcounts<-renderPlot({
      req(length(current_som_model())>0)
      g_pcounts()
    })
    output$pUmatrix<-renderPlot({
      req(length(current_som_model())>0)
      pUmatrix(current_som_model())
    })
    output$pproperty<-renderPlot({
      req(input$variable_pproperty)
      pproperty(current_som_model(),
                input$variable_pproperty,
                input$variable_pproperty)
      vals$pprop_plot<-recordPlot()
    })

    output$textsomgrid<-renderUI({
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

    div(column(12,
               p("This functionality uses the ",code('somgrid')," function from the",
                 code("kohonen"),"package."),
               p(icon("fas fa-exclamation-circle"),"All arguments are passed to the function;"),
               p(icon("fas fa-exclamation-circle"), "Arguments are described 'in the ",actionLink(ns("somgridh"),"help page")," of the function;")
    ),
    column(12,
           htmlOutput(ns("somgridhelp"))
    )))
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

    div(column(12,
               p("This functionality uses the ",code('som')," function from the",
                 actionLink(ns("kohonen"),"kohonen"),"package."),
               p(icon("fas fa-exclamation-circle"), "arguments are described 'in the ",actionLink(ns("supersomh"),"help page")," of the function;")
    ),
    column(12,
           htmlOutput(ns("supersomhelp"))
    )))

    })
    output$textsugtopohelp<-renderUI({

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

    div(column(12,
               'The number of map nodes and the side length ratio is performed with the following steps (Vesanto, 2000 ):',
               column(12, style="margin-left: 10px; margin-top: 5px;",
                      p(strong("1."),"Determine the number of map nodes using the heuristic recommendation:",withMathJax(helpText(
                        "$$ M = 5{\\sqrt{N}}$$"
                      )),"where N is the number of observations in the input data set ( Vesanto, 2000 ),"),
                      p(strong("2."),"Determine the eigenvectors and eigenvalues in the data from the autocorrelation matrix,"),
                      p(strong("3."),"Set the ratio between the two sides of the grid equivalent to the ratio between the two largest eigenvalues, and "),
                      p(strong("4."),"Scale the side lengths so that their product (xdim * ydim) is as close as possible to the number of map units determined above."))
    )))

    })
    savereac<-reactive({



      tosave<-isolate(reactiveValuesToList(vals))
      tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
      tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
      tosave$saved_data<-vals$saved_data
      tosave$newcolhabs<-vals$newcolhabs
      tosave$colors_img<-vals$colors_img
      tosave$ssom_tab<-vals$ssom_tab
      tosave$ssom_tab0<-vals$ssom_tab0
      tosave$som_results<-current_som_model()
      tosave$SOM_MODEL<-vals$SOM_MODEL
      saveRDS(tosave,"savepoint.rds")
      saveRDS(reactiveValuesToList(input),"input.rds")
      beep()


    })


    output$train_som_button<-renderUI({
      if(input$distmethod=="BrayCurtis"){
        validate(need(anyNA(getdata_som())==F, "Missing values are not allowed in the Bray method. Change the distance or use the preprocessing tools to impute or remove the missing values"))
      }


      if(isTRUE(input$mysupersom)){
        validate_supersom()
        lab<-h4(icon("fas fa-braille"),"train superSOM",icon("fas fa-arrow-circle-right"))
      }


    })


    topo.reactive<-reactiveVal()
    toroidal<-reactive({
      switch (input$toroidal,
              'TRUE' = TRUE,
              "FALSE" = FALSE)
    })
    output$train.summary<-renderTable({
      train.summary()
    }, rownames = T, colnames = F,striped=T, spacing ="xs")


    getobs_somX<-reactive({
      datalist<-vals$saved_data
      m<-current_som_model()
      colnames_list<-lapply(datalist,colnames)
      colnames_som<-lapply(m$data,colnames)
      names(colnames_som)<-names(m$data)
      do.call(c,lapply(colnames_som,function(j){
        names(which(sapply(colnames_list,function(i){
          identical(sort(i), sort(j))
        })))
      }))

    })
    checkpredsom<-reactive({
      req(input$predsom_new)
      check<-vals$saved_data[[input$data_som]]
      res<-res0<-lapply(vals$saved_data,function(x) match_col(x,check) )
      res<-names(which(unlist(res)==T))
      choices=names(unlist(c(res0[res0==T],res0[ res0==F])))
      #validate(need(isTRUE(res0[[input$predsom_new]]),paste("Variables from Datalist",input$predsom_new, "incompatible with the Training data", input$data_som)))
      choices
    })
    getobs_som2<-reactive({
      datalist<-vals$saved_data
      m<-current_som_model()


      res0<-unlist(
        lapply(datalist, function (x){
          res<-colnames(x)%in%colnames(m$data[[2]])
          sum(res)==ncol(m$data[[2]])
        })
      )
      names(res0[res0==T])
    })




    getpred_model<-reactive({

      res0<-res<-get_sompred()

      pic_result<-input$ss2_property_layer

      res$predictions
      res<-res[["unit.predictions"]][[pic_result]]
      rownames(res)<-paste0("neu",1:nrow(res))
      unit.predictions<-res

      res<-res0
      res_temp<-data.frame(bmu=res[['unit.classif']])
      rownames(res_temp)<-rownames(res[["predictions"]][[1]])
      res<-res_temp
      unit.classif<-res

      res<-res0
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }

      predictions<-res[["predictions"]][[pic_result]]

      list(predictions=predictions,unit.classif=unit.classif,unit.predictions=unit.predictions,pic_result=pic_result)


    })

    get_som_model_pred<-reactive({
      m_pred<-current_som_model()
      req(inherits(m_pred,'kohonen'))

      res<-getpred_model()
      #res$pic_result<-input$ss2_property_layer
      unit.classif<-res$unit.classif[,1]
      names(unit.classif)<-rownames(res$unit.classif)


      m_pred$codes<-list(res$unit.predictions)
      names(m_pred$codes)<-res$pic_result

      m_pred$unit.classif<-unit.classif

      m_pred$data<-list(as.matrix(res$predictions))
      m_pred$whatmap<-res$pic_result
      names(m_pred$data)<-res$pic_result

      picmap<-which(names(current_som_model()$data)%in%res$pic_result)
      m_pred$dist.fcts<- m_pred$dist.fcts[picmap]
      m_pred$distance.weights<- m_pred$distance.weights[picmap]
      m_pred$user.weights<-m_pred$user.weights[picmap]
      m_pred$whatmap=1
      m_pred




    })
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
    output$som_errors<-renderUI({
      som_qualist<-errors_som(current_som_model())
      res<-data.frame(som_qualist$som_quality)

      res$mean=apply(data.frame(lapply(res,function(x)as.numeric(x))),1,mean)
      neu.uti<-data.frame(som_qualist$neu.uti)
      rownames(neu.uti)<-"neu.uti"
      res_names<-rownames(res)
      div(


        renderTable(res,rownames =T),
        renderTable(neu.uti,rownames =T,colnames =F),
      )
    })

    observeEvent(ignoreInit = T,input$create_codebook,{
      vals$hand_save<-"Create Datalist with Codebooks"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
      showModal(module_som())
    })


    observeEvent(ignoreInit = T,input$down_pcodes_results,{
      vals$hand_down<-"generic"


      module_ui_downcenter("downcenter")
      name<-"Codebook results"
      data<- data.frame( kohonen::getCodes(current_som_model()))
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Codebook results",data=data, name=name)
    })

    output$down_kohonen_results<-{
      downloadHandler(
        filename = function() {
          paste0("Kohonen","_", Sys.Date(),".rds")
        }, content = function(file) {
          saveRDS(current_som_model(),file)
        })
    }

    getdata_som<-reactive({
      req(input$data_som)
      data_o<-data<-vals$saved_data[[input$data_som]]

      data
    })

    bmu_pred_points<-reactive({
      req(input$bmu_p_dotlabel)
      if(input$bmu_p_dotlabel == 'symbols'){ T} else {F}
    })
    bmu_p_factors_reac<-reactive({
      factors<-attr(vals$saved_data[[input$data_som]],"factors","factors")
      m<-current_som_model()
      if (length(input$bmu_p_factors)>0) {
        c(factors[rownames(vals$saved_data[[input$data_som]],"factors"), input$bmu_p_factors],
          if(length(attr(m,"test"))>0) {factors[rownames(attr(m,"test")), input$bmu_p_factors]})
      } else{c(factors[rownames(vals$saved_data[[input$data_som]],"factors"),1],
               if(length(attr(m,"test"))>0) {factors[rownames(attr(m,"test")), ]})}
    })

    bmu_factors_reac<-reactive({
      factors<-attr(vals$saved_data[[input$data_som]],"factors","factors")
      if (length(input$bmu_factors)>0) {
        factors[rownames(vals$saved_data[[input$data_som]],"factors"), input$bmu_factors]
      } else{factors[,1]}
    })
    train.summary<-reactive({
      m<-current_som_model()

      req(inherits(m,"kohonen"))
      traindata<-data.frame(m$data[[1]])
      mean = round(mean(unlist(traindata)), 2)
      n.obs = nrow(traindata)
      n.variables = ncol(traindata)
      summ<-m$grid[-1]
      summ$neighbourhood.fct<-as.character(summ$neighbourhood.fct)
      summ<-do.call(rbind, summ)
      mode<-attr(m,"mode")
      alpha = paste0(m$alpha, collapse = "; ")
      radius = paste0(round(m$radius, 3), collapse = "; ")
      # user.weights = m$user.weights
      maxNA.fraction = m$maxNA.fraction
      dist.fcts =   paste0(m$dist.fcts,collapse="; ")
      user.weights =   paste0(round(m$user.weights,4),collapse="; ")
      Layers =   paste0(names(m$data),collapse="; ")
      normalizeDataLayers<-attr(m,"normalizeDataLayers")





      Parameters<-
        rbind(
          Layers,
          dist.fcts,
          user.weights,
          n.obs,
          n.variables,
          summ,
          alpha,
          radius,
          #user.weights,
          maxNA.fraction,
          normalizeDataLayers=normalizeDataLayers,

          mode
        )
      result<-data.frame(Parameters)


      result
    })
    coodebook_name<-reactive({
      name0<-paste0("Codebook_",input$data_som,"::",input$som_models)
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      name1[length(vals$saved_data)+1]
    })
    data_overwritte<-reactiveValues(df=F)
    data_store<-reactiveValues(df=F)
    newname<-reactiveValues(df=0)
    get_newname<-reactive({
      req(!is.null(vals$hand_save))
      newname$df<-switch(
        vals$hand_save,
        ##RF,
        "Create Datalist  - Variables from VFM"=bag_vfm(),
        "Create Datalist  - SOM predictions for New Data (X)"=bag_tab_pred_result(),
        "Create Datalist  - SOM codebook predictions"=bag_tab_pred_result(),
        "Create Datalist  - SOM performace"=bag_predsom_errors(),
        "Save new som in"=bag_somname(),

        "Save errors from som predictions (X)"=bag_sompred_eX(),
        "Save errors from som predictions (Y)"=bag_sompred_eY(),
        "Create Datalist with Codebooks"=coodebook_name()
      )})
    bag_predsom_errors<-reactive({
      name0<-paste(input$data_som,input$get_predsom_perf, sep="_")
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      name1[length(vals$saved_data)+1]
    })
    bag_tab_pred_result<-reactive({
      name0<-paste(input$data_som,input$tab_pred_result,input$layer_result, sep="_")
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      name1[length(vals$saved_data)+1]
    })


    predsom_name<-reactive({
      zero<-"Som_Pred"
      if(input$som_models!="new som (unsaved)"){
        zero=paste0(input$som_models,"_Pred")
      }
      name<-if(!length(saved_sompred$df)>0){
        zero
      } else{
        paste(zero,length(saved_sompred$df))
      }
      name
    })
    bag_sompred_eX<-reactive({
      bag<-1
      name0<-paste("Som_pred_errorsX")
      name1<-paste(name0,bag)
      if(name1%in%names(vals$saved_data))
      {
        repeat{
          bag<-bag+1
          name1<-paste(name0,bag)
          if(!name1%in%names(vals$saved_data)) break
        }
      }
      paste("Som_pred_errorsX",bag)

    })
    bag_sompred_eY<-reactive({
      bag<-1
      name0<-paste("Som_pred_errorsY")
      name1<-paste(name0,bag)
      if(name1%in%names(vals$saved_data))
      {
        repeat{
          bag<-bag+1
          name1<-paste(name0,bag)
          if(!name1%in%names(vals$saved_data)) break
        }
      }
      paste("Som_pred_errorsY",bag)

    })
    module_som<-function() {
      ns<-session$ns

      modalDialog(
        uiOutput(ns("databank_storage")),
        title=strong(icon("fas fa-save"),'Save'),
        footer=column(12,
                      div(modalButton(strong("cancel")),
                          inline(uiOutput(ns("save_confirm")))
                      )
        ),

        easyClose = T
      )

    }
    output$data_over<-renderUI({
      data_overwritte$df<-F
      choices<-c(names(vals$saved_data))
      req(input$hand_save=="over")
      if(vals$hand_save=='Save new som in'){choices<-names(attr(vals$saved_data[[input$data_som]],'som'))}
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

    create_som_vfm<-reactive({

      data_o<-vals$saved_data[[input$data_som]]
      m<-getsom()
      dd<-do.call(cbind,m$data)
      colnames(dd)<-unlist(sapply(m$data,colnames))
      temp<-data.frame(dd[,rownames(vals$biplot_som),drop=F])
      colnames(temp)<-rownames(vals$biplot_som)
      if(input$hand_save=="create") {
        temp<-data_migrate(data_o,temp,input$newdatalist)
        vals$saved_data[[input$newdatalist]]<-temp
      } else{
        temp<-data_migrate(data_o,temp,input$over_datalist)
        vals$saved_data[[input$over_datalist]]<-temp
      }




    })
    bag_vfm<-reactive({
      name0<-paste0(input$data_som,"_",input$ss1_npic,"vars")
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      name1[length(vals$saved_data)+1]
    })
    create_predsom_errors<-reactive({

      data_o<-switch(input$sompred_type,
                     "Partition"={
                       vals$saved_data[[input$data_som]][rownames(attr(current_som_model(),"test")[[1]]),]
                     },
                     "Datalist"={vals$saved_data[[input$predsom_new]]},
                     "Training"={vals$saved_data[[input$data_som]]}
      )

      temp<-vals$som_perf_result
      if(input$hand_save=="create") {
        temp<-data_migrate(data_o,temp,input$newdatalist)
        vals$saved_data[[input$newdatalist]]<-temp
      } else{
        temp<-data_migrate(data_o,temp,input$over_datalist)
        vals$saved_data[[input$over_datalist]]<-temp
      }




    })


    savebmu<-reactive({
      temp<-current_som_model()
      bmu<-temp$unit.classif
      names(bmu)<-rownames(temp$data[[1]])
      bmu<-data.frame(bmu=as.factor(bmu))
      factors<-attr(vals$saved_data[[input$data_som]],"factors")
      factors<-data.frame(factors,bmu)
      attr(vals$saved_data[[input$data_som]],"factors")<-factors
    })

    datalist_som_errorsX<-reactive({
      temp<-data.frame(get_sompred_results())
      if(input$hand_save=="create") {
        temp<-data_migrate(vals$saved_data[[input$data_som]],temp,input$newdatalist)
        vals$saved_data[[input$newdatalist]]<-temp
      } else{
        temp<-data_migrate(vals$saved_data[[input$data_som]],temp,input$over_datalist)
        vals$saved_data[[input$over_datalist]]<-temp
      }
    })
    datalist_som_errorsY<-reactive({
      temp<-data.frame(get_sompred_results())
      if(input$hand_save=="create") {
        temp<-data_migrate(vals$saved_data[[input$data_som]],temp,input$newdatalist)
        vals$saved_data[[input$newdatalist]]<-temp
      } else{
        temp<-data_migrate(vals$saved_data[[input$data_som]],temp,input$over_datalist)
        vals$saved_data[[input$over_datalist]]<-temp
      }
    })



    validate_supersom<-reactive({
      layers = get_training_list()
      ids<-names(layers)
      dists<-ssom_reac()
      dists$id<-rownames(dists)
      dd<-sapply(layers,function(x) {any(rowSums(x)==0)})
      dists$notval<-dd
      if(any(dists$Distances=="BrayCurtis")){
        notval<-which(apply(dists,1,function(x) x[3]=='BrayCurtis'&x[5]==T))

        if(length(notval)>0){
          paste(
            "Error: Empty rows detected. SOM cannot be trained using the 'Bray' method for the layers",ifelse(length(notval) == 1, notval,
                                                                                                              ifelse(length(notval) == 2, paste0(notval, collapse = " and "),
                                                                                                                     paste0(paste0(notval[1:(length(notval)-1)], collapse = ", "), " and ", notval[length(notval)])))
          )
        } else{ NULL}



      }

    })

    getsom<-reactive({
      req(input$som_models)
      req(length(attr(getdata_som(),"som"))>0)
      m<-attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]
      req(inherits(m,"kohonen"))
      m
    })
    output$saved_som_print<-renderUI({
      req(input$som_tab!="som_tab1")
      req(input$data_som)
      names_som<-names(attr(vals$saved_data[[input$data_som]],"som"))
      req(length(names_som)>0)
      pic<-which(names_som%in%"new som (unsaved)")
      if(length(pic)>0){
        names_som<-names_som[-pic]
      }
      req(length(names_som)>0)
      div(class="saved_models",
          icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names_som)), "saved model(s)")
    })
    bag_somname<-reactive({

      m<-current_som_model()
      met<-attr(m,"Method")
      bagname<-"SOM ("
      if(length(met)>0){
        if(length(met)==1)
          if(met=="superSOM"){
            bagname<-"superSOM ("
          }
      }

      paste0(bagname,length((attr(vals$saved_data[[input$data_som]],"som"))),")")

    })


    som_model_names<-reactive({
      req(input$data_som)
      names(attr(vals$saved_data[[input$data_som]],"som"))
    })

    savecoodebok<-reactive({
      m<-attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]
      grid<-m$grid$pts
      bmus<-m$unit.classif
      res_fac<-do.call(rbind,lapply(1:nrow(grid),function(i){
        x<-rep(0,length(bmus))
        x[which(bmus==i)]<-1
        factor(x)
      }))
      res_fac<-data.frame(res_fac)
      colnames(res_fac)<-names(bmus)
      rownames(res_fac)<-paste("unit",1:nrow(grid))
      res=data.frame(current_som_model()$codes[[1]])
      rownames(res)<-paste("unit",1:nrow(grid))
      data=res
      res_fac<-data.frame(neuron=paste0("unit_",1:nrow(grid)))
      attr(data,"factors")<-res_fac
      coords<-data.frame(grid)
      colnames(coords)<-c("x","y")
      rownames(coords)<-rownames(data)
      attr(data,"coords")<-coords

      neu<-do.call(rbind,get_neurons(m))
      my_df<-neu[,1:3]
      df<-data.frame(group_by( st_as_sf(my_df,coords = c("x", "y"),crs = 4326),neu))
      li<-lapply(split(df,df$neu),function(x){
        x$geometry<-  st_combine(x$geometry)
        x
      })
      base_shape<-st_cast(st_sf(do.call(rbind,li)),"POLYGON")


      attr(data,"base_shape")<-base_shape
      if(input$hand_save=="create"){
        vals$saved_data[[input$newdatalist]]<-data
      } else {
        vals$saved_data[[input$over_datalist]]<-data}
    })
    savesom<-reactive({
      curtab<-vals$cursomtab
      data<-getdata_som()
      temp<-current_som_model()
      if(input$hand_save=="create"){
        temp<-list(temp)
        names(temp)<-input$newdatalist
        attr(vals$saved_data[[input$data_som]],"som")[input$newdatalist]<-c(temp,attr(vals$saved_data[[input$data_som]],"som")[[input$newdatalist]])
        cur<-input$newdatalist
      } else{
        temp<-list(temp)
        names(temp)<-input$over_datalist
        attr(vals$saved_data[[input$data_som]],"som")[input$over_datalist]<-temp

        cur<-input$over_datalist

      }

      vals$cur_som_models<-cur
      delay(500,{updateTabsetPanel(session, "som_tab", curtab)
        updateTabsetPanel(session, "som_res", vals$som_res)})

      attr(vals$saved_data[[input$data_som]],"som")[['new som (unsaved)']]<-NULL

    })
    observeEvent(input$som_model_delete,ignoreInit = T,{
      attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]<-NULL

    })


    current_som_model<-reactive({
      res<-getsom()
      req(inherits(res,"kohonen"))
      if(length(res$data)==1){
        if(names(res$data)=="X"){
          names(res$data)<-attr(res,"Datalist")
        }
      }
      res
    })

    get_partition<-reactive({
      if(isFALSE(input$usepartition)){
        data<-getdata_som()
        return(list(train=rownames(data),test=NULL))
      }

      data<-vals$saved_data[[input$data_somY]]


      factors<-attr(data,"factors")
      partition_column<-factors[input$partition_column]
      test_ids<-which(partition_column[,1]%in%input$partition_ref)
      train_ids<-rownames(data)[-test_ids]
      test_ids<-rownames(data)[test_ids]
      return(list(train=train_ids,test=test_ids))
    })



    observeEvent(input$trainSOM,ignoreInit = T,{
      req(isTRUE(input$mysupersom))
      attr(vals$saved_data[[input$data_som]],"som")[["new som (unsaved)"]]<-NULL



      if(is.null(attr(vals$saved_data[[input$data_som]],"som"))){
        attr(vals$saved_data[[input$data_som]],"som")<-list()
      }


      layers = get_training_list()
      layer_table<-ssom_reac()
      weights<-layer_table$Weights
      distances<-layer_table$Distances
      data1<-vals$saved_data[layer_table$Datalist][[1]]
      if(isTRUE(input$usepartition)){
        #req(input$data_somY)
        factors<-attr(vals$saved_data[[input$data_somY]],"factors")[rownames(data1),]
        pic_split<-which(factors[,input$partition_column]%in%input$partition_ref)
        test_ids<-rownames(factors)[pic_split]
        train_ids<-rownames(factors)[-pic_split]
        parts=list(train=train_ids,test=test_ids)
        train<-parts$train
        test<-parts$test
        training_list<-lapply(layers,function(x){
          x[train,]
        })
        test_list<-lapply(layers,function(x){
          x[test,]
        })
      } else{
        training_list<-layers
        test_list<-"None"
      }

      ncodes<-input$xdim*input$ydim
      seed<-input$seed
      seed<-input$seed
      if(is.na(seed)){
        seed<-sample(.Random.seed,1)}



      ncodes<-input$xdim*input$ydim
      set.seed(seed)
      starters<-sample(1:nrow(training_list[[1]]), ncodes, replace = FALSE)
      init<-lapply(training_list, function(x) x[starters, , drop = FALSE])

      withProgress(
        message = "Running som... the time taken will depend on the size of the data and the training.",
        min = 1,
        max = 1,
        {
          if(is.na(input$seed)==F){set.seed(input$seed)}
          args<-list(
            data=training_list,
            #whatmap = 1,
            grid = kohonen::somgrid(
              input$xdim,
              input$ydim,
              topo = input$topo,
              toroidal = toroidal(),
              neighbourhood.fct=input$neighbourhood.fct
            ),
            rlen = input$rlen,
            dist.fcts = distances,
            user.weights=weights,
            alpha = c(input$a1, input$a2),
            radius = c(input$r1, input$r2),
            mode = input$mode,
            maxNA.fraction = input$maxna,
            normalizeDataLayers=as.logical(input$normalizeDataLayers),
            init=init
          )

          m<-do.call(supersom,args)
          attr(m,'mode')<-m$mode<-input$mode
          m$seed<-seed
          m$normalizeDataLayers<-input$normalizeDataLayers
          m$init<-init
          names(m$unit.classif)<-rownames(m$data[[1]])
          attr(m,"Method")<-"superSOM"
          attr(m,"Datalist")<-names(m$data)
          attr(m,"test")<-test_list
          attr(m,"normalizeDataLayers")<-input$normalizeDataLayers

          newmodesl<-c(list(m),attr(vals$saved_data[[input$data_som]],"som"))
          names(newmodesl)[1]<-"new som (unsaved)"
          attr(vals$saved_data[[input$data_som]],"som")<-newmodesl

          updateTabsetPanel(session, "som_tab", "som_tab2")
          updateTabsetPanel(session, "som_tab", "train_tab2")
          updateTabsetPanel(session, "som_res", "train_tab1")
          vals$cur_som_models<-"new som (unsaved)"
        }
      )
    })

    observeEvent(input$trainSOM,ignoreInit = T,{
      req(isFALSE(input$mysupersom))
      vals$som_unsaved<-NULL
      attr(vals$saved_data[[input$data_som]],"som")[["new som (unsaved)"]]<-NULL


      traindat=data=data_o<-data.frame(vals$saved_data[[input$data_som]])
      traindat=data[get_partition()$train,]


      withProgress(
        message = "Running som... the time taken will depend on the size of the data and the training.",
        min = 1,
        max = 1,



        {
          datalist<-list(as.matrix(traindat))
          names(datalist)<-input$data_som
          seed<-input$seed
          if(is.na(seed)){
            seed<-sample(.Random.seed,1)}



          ncodes<-input$xdim*input$ydim
          set.seed(seed)
          starters<-sample(1:nrow(datalist[[1]]), ncodes, replace = FALSE)
          init<-lapply(datalist, function(x) x[starters, , drop = FALSE])
          set.seed(seed)
          m<-try(
            supersom(
              datalist,
              grid = kohonen::somgrid(
                input$xdim,
                input$ydim,
                topo = input$topo,
                toroidal = toroidal(),
                neighbourhood.fct=input$neighbourhood.fct
              ),
              rlen = input$rlen,
              dist.fcts = input$distmethod,
              alpha = c(input$a1, input$a2),
              radius = c(input$r1, input$r2),
              mode = input$mode,
              maxNA.fraction = input$maxna,
              init=init,
              normalizeDataLayers=as.logical(input$normalizeDataLayers)
            )
          )

          if (!inherits(m,"kohonen"))        {
            validate(paste(m[[1]], "Please decrease your alpha (learning rate)"))
          }
          attr(m,'mode')<-m$mode<-input$mode
          m$seed<-seed
          m$normalizeDataLayers<-input$normalizeDataLayers
          m$init<-init
          names(m$unit.classif)<-rownames(m$data[[1]])
          attr(m,"test_partition")<-"None"
          test_list<-list(as.matrix(data[get_partition()$test,]))
          names(test_list)<-input$data_som
          attr(m,"test")<-test_list
          attr(m,"Method")<-"Unsupervised"
          attr(m,"Datalist")<-input$data_som
          attr(m,"normalizeDataLayers")<-input$normalizeDataLayers
          attr(m,"coords")<-attr(data,"coords")[rownames(traindat),]
          vals$som_unsaved<-m
          newmodesl<-c(list(m),attr(vals$saved_data[[input$data_som]],"som"))
          names(newmodesl)[1]<-"new som (unsaved)"
          attr(vals$saved_data[[input$data_som]],"som")<-newmodesl





          updateTabsetPanel(session, "som_tab", "som_tab2")
          updateTabsetPanel(session, "som_tab", "train_tab2")
          updateTabsetPanel(session, "som_res", "train_tab1")
          vals$cur_som_models<-"new som (unsaved)"
        }
      )
    })











    observeEvent(current_som_model(),{
      m<-current_som_model()
      test<-attr(m,"test")
      choices=c("Training","Datalist")
      if(inherits(test,"list")){
        choices=c("Partition","Training","Datalist")
      }
      selected=vals$cur_sompred_type
      selected<-get_selected_from_choices(selected,choices)

      updateRadioGroupButtons(session,'sompred_type',choices=choices,selected=selected)
    })

    observeEvent(input$sompred_type,{
      vals$cur_sompred_type<-input$sompred_type
    })
    observeEvent(list(input$xdim,input$ydim),{
      req(input$xdim)
      req(input$ydim)
      value=as.vector(
        quantile(
          unit.distances(
            kohonen::somgrid(input$xdim,input$ydim,topo = input$topo,toroidal = toroidal(), neighbourhood.fct=input$neighbourhood.fct)), 2 / 3
        )
      )
      updateNumericInput(session,"r1",value=value)
      fineonce<-reactiveVal(F)
      if(isFALSE(fineonce())){
        shinyjs::hide("finetuning_som")
        fineonce(T)
      }


    })
    observeEvent(ignoreInit = T,input$data_som,{
      if(anyNA(getdata_som())){
        updateSelectInput(session,"distmethod",selected="euclidean")
      }
    })
    observeEvent(ignoreInit = T,input$som_res,{
      vals$som_res<-input$som_res
    })
    observeEvent(ignoreInit = T,input$resettopo, {
      shinyjs::reset("topocontrol")
      shinyjs::reset("topocosugtopontrol")
    })
    observeEvent(ignoreInit = T,input$save_bmu,{
      savebmu()
    })
    observeEvent(ignoreInit = T,input$supersomhelp, {
      showModal( modalDialog(
        div(
          uiOutput(ns("textsupersom")))
        ,
        title = "Self-Organizing Maps",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      ))
    })
    observeEvent(ignoreInit = T,input$sugtopohelp, {
      showModal(

        modalDialog(
          uiOutput(ns('textsugtopohelp')),
          title = "Suggested topology",
          footer = modalButton("close"),
          size = "m",
          easyClose = TRUE
        )

      )
    })
    observeEvent(ignoreInit = T,input$supersomh,{
      output$supersomhelp<-renderText({
        paste0(
          br(),
          h4("supersom {kohonen}"),
          p(
            icon("fas fa-exclamation-circle"),
            "Argument",
            code("X"),
            "fixed to your uploaded and pre-treated data;"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            code("a1"),
            "and" ,
            code("a2") ,
            "refers to the",
            code("alpha"),
            "argument"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            code("r1"),
            "and" ,
            code("r2") ,
            "refers to the",
            code("radius"),
            "argument"
          ),
          p(
            icon("fas fa-exclamation-circle"),
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
    observeEvent(ignoreInit = T,input$ss1_varfacmap, {
      showModal(modalDialog(
        uiOutput(ns("textvarfacmap")),
        title = h4(strong("Variable factor map")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      ))
    })
    observeEvent(ignoreInit = T,input$ss2_varfacmap, {
      showModal(modalDialog(
        uiOutput(ns("textvarfacmap")),
        title = h4(strong("Variable factor map")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      ))
    })
    observeEvent(ignoreInit = T,input$somgridh,{
      output$somgridhelp<-renderText({
        paste0(br(),
               h4("somgrid {kohonen}"),
               getHelp('unit.distances'))
      })
    })
    observeEvent(ignoreInit = T,input$somgridhelp, {
      showModal(

        modalDialog(
          uiOutput(ns("textsomgrid")),
          title = h4(strong("somgrid")),
          footer = modalButton("close"),
          size = "m",
          easyClose = TRUE
        )

      )
    })
    observeEvent(ignoreInit = T,input$partition_column,{
      vals$cur_partition_column<-input$partition_column
    })
    observeEvent(ignoreInit = T,input$partition_ref,{
      vals$cur_partition_ref<-input$partition_ref
    })
    observeEvent(ignoreInit = T,input$pred_results,{
      vals$cur_tab_pred_result<-input$pred_results
    })
    observeEvent(ignoreInit = T,input$layer_result,{
      vals$cur_layer_result<-input$layer_result
    })
    observeEvent(ignoreInit = T,input$link_predsom_codebook_create,{
      vals$hand_save<-"Create Datalist  - SOM codebook predictions"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
      showModal(module_som())
    })
    observeEvent(ignoreInit = T,input$link_predsom_newdata_create,{
      vals$hand_save<-"Create Datalist  - SOM predictions for New Data (X)"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
      showModal(module_som())
    })
    observeEvent(ignoreInit = T,input$downtable_perf_result,{
      vals$hand_down<-"som_perf_result"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals, name=paste(input$data_som,input$get_predsom_perf, sep="_"))

    })
    observeEvent(ignoreInit = T,input$downtable_predict_result,{
      req(input$layer_result)
      vals$hand_down<-"som_predict_result"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals, name=paste(input$data_som,input$tab_pred_result,input$layer_result, sep="_"))

    })
    observeEvent(ignoreInit = T,input$link_predsom_perf_obs_create,{
      vals$hand_save<-"Create Datalist  - SOM performace"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
      showModal(module_som())
    })
    observeEvent(ignoreInit = T,input$download_pbox6,{

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=getbmu_plot()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="BMU plot - predictions", name_c="bmu_pred")
    })

    observeEvent(ignoreInit = T,input$downp_bmu,{

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=vals$bmus_plot
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="BMU plot", name_c="bmu_plot")
    })


    observeEvent(vals$newcolhabs,{
      choices = vals$colors_img$val[getsolid_col()]
      choicesOpt=list(content=vals$colors_img$img[getsolid_col()])

      updatePickerInput(session,'ss1_p.clus.col.text',
                        choices=choices,
                        choicesOpt=choicesOpt,
                        selected="black"
      )

      updatePickerInput(session,'ss1_var_bg',
                        choices=choices,
                        choicesOpt=choicesOpt,
                        selected="white"
      )


      updatePickerInput(session,'ss1_pclus_border',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "white")

      updatePickerInput(session,'ss2_pclus_border',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "white"
      )
      updatePickerInput(session,'ss2_pclus_text_palette',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "black"
      )
      updatePickerInput(session,'ss1_pclus_text_palette',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "black"
      )
      updatePickerInput(session,'ss1_pclus_points_palette',
                        choices = vals$colors_img$val,
                        choicesOpt = list(content = vals$colors_img$img),
                        selected="black"
      )
      updatePickerInput(session,'ss2_pclus_points_palette',
                        choices = vals$colors_img$val,
                        choicesOpt = list(content = vals$colors_img$img),
                        selected="black"
      )

      updatePickerInput(session,'ss2_p.clus.col.text',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "black"
      )

      updatePickerInput(session,'ss2_var_bg',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "white"
      )
    })

    observeEvent(ss1_getdata_layer(),{
      updatePickerInput(session,'ss1_variable_pproperty',choices=colnames(ss1_getdata_layer()))
    })
    observeEvent(getsom(),{
      choices = names(getsom()$data)
      updatePickerInput(session,'ss1_property_layer',choices=choices)
      updatePickerInput(session,'ss2_property_layer',choices=choices)
    })
    observeEvent(ss1_get_choices_pal(),{
      choices<-ss1_get_choices_pal()
      title<-attr(choices,"title")
      req(input$ss1_somback_value)
      selected<-ifelse(input$ss1_somback_value=="None","gray","viridis")

      updatePickerInput(session,'ss1_bg_palette',
                        choices =  vals$colors_img$val[choices],
                        selected=selected,
                        choicesOpt = list(
                          content =  vals$colors_img$img[choices] ))
    })
    observeEvent(ignoreInit = T,input$create_vfm_results,{
      vals$hand_save<-"Create Datalist  - Variables from VFM"
      m<-getsom()
      vals$hand_save2<-NULL
      if(length(m$data)>1){
        vals$hand_save2<-"Note that the trained SOM is composed by multiple layers. Variables from different layers will be merged (cbind) into a single Datalist"
      }

      vals$hand_save3<-NULL
      showModal(module_som())
    })
    observeEvent(ignoreInit = T,input$supersom_reset,{
      if(input$supersom_reset%%2){

        vals$cur_somdataXlist_supersom<- NULL
        vals$cur_ssom_wei<-rep("0.5",2)
        vals$cur_ssom_dist<-rep("euclidean",2)
        vals$ssom_tab0<-getinit_supersom()

      }
    })
    observeEvent(ignoreInit = T,input$ssom_add, {
      res<-rbind(vals$ssom_tab0, vals$ssom_tab0[nrow(vals$ssom_tab0),])
      #rownames(res)<-paste0("Layer",1:nrow(res))
      vals$cur_ssom_wei<-c(vals$cur_ssom_wei,vals$cur_ssom_wei[length(vals$cur_ssom_wei)])
      vals$cur_ssom_dist<-c(vals$cur_ssom_dist,vals$cur_ssom_dist[length(vals$cur_ssom_dist)])
      vals$cur_somdataXlist_supersom<-c(vals$cur_somdataXlist_supersom,vals$cur_somdataXlist_supersom[length(vals$cur_somdataXlist_supersom)])

      vals$ssom_tab0<-res
    })
    observeEvent(list(vals$cur_somtab, vals$cur_tab, ssom_reac()[,1]),{
      try({
        req(!is.null(vals$ssom_tab))
        req(!is.null(ssom_reac()))
        df<-ssom_reac()
        vals$cur_ssom_wei<-df[,2]
        vals$cur_ssom_dist<-df[,3]
        vals$cur_somdataXlist_supersom<-df[,1]

        vals$ssom_tab0<-df

      }, silent=T)
    })
    observe({
      req(length(input$supersom_reset)>0)
      if(is.null(vals$ssom_tab0)){
        vals$cur_somdataXlist_supersom<- NULL
        vals$cur_ssom_wei<-rep("0.5",2)
        vals$cur_ssom_dist<-rep("euclidean",2)
        vals$ssom_tab0<-getinit_supersom()
      }
    })
    observeEvent(ignoreInit = T,input$ssom_minus,{
      vals$ssom_tab0<-vals$ssom_tab0[-nrow(vals$ssom_tab0),]
    })
    observeEvent(list(input$som_part,input$data_som,input$mysupersom),{
      req(input$som_tab)
      req(length(input$mysupersom)>0)
      req(input$data_som)
      req(input$som_part)
      updateTabsetPanel(session,"som_tab","som_tab1")
    })
    observeEvent(ignoreInit = T,input$som_tab,{
      somval$df_go<-F
      vals$cur_somtab_before<-vals$cur_somtab
      vals$cur_somtab<-input$som_tab
      somval$df_go<-T
    })
    output$som_models_panel<-renderUI({
      box_caret(ns('box_setup2'),
                title="Model Results",
                div(
                  div(
                    style="display: flex",

                    selectInput(ns("som_models"),
                                span(strong("Results:", tiphelp("SOM results. Click to see SOM results saved in the selected Datalist"))),
                                choices=som_model_names(),
                                selected=vals$cur_som_models),
                    tipify( actionButton(ns("som_model_delete"),icon("fas fa-trash-alt")),"Remove model")

                  ),
                  uiOutput(ns("som_cur"))
                ))
    })

    observeEvent(input$som_models,{
      vals$cur_som_models<-input$som_models
    })

    observeEvent(input$run_play2,ignoreInit = T,{

      m<-current_som_model()
      seed<-m$seed
      ms<-list()
      rlen=nrow(m$changes)

      seq<-    round(seq(1,rlen,length.out=input$rlen_snap2))

      withProgress(min=1,max=length(seq),message="Running...",{
        for( i in seq) {

          set.seed(seed)
          m2<-supersom(m$data,m$grid,
                       radius = m$radius,
                       alpha =m$alpha,
                       whatmap = m$whatmap,
                       user.weights = m$user.weights,
                       maxNA.fraction = m$maxNA.fraction,
                       dist.fcts = m$dist.fcts,
                       mode = m$mode,
                       rlen=i,
                       init=m$init,
                       normalizeDataLayers = m$normalizeDataLayers)


          ms[[length(ms)+1]]<-m2
          incProgress(1)
        }
      })

      modelplay2(ms)

    })
    observeEvent(modelplay(),{
      shinyjs::toggle(selector=".anim_opt",condition=length(modelplay())>0)
    })
    observeEvent(modelplay2(),{
      shinyjs::toggle(selector=".anim_opt2",condition=length(modelplay2())>0)
    })
    observeEvent(input$run_play,ignoreInit = T,{
      data<-data_example()
      datalist<-list(as.matrix(data))
      names(datalist)<-input$data_som
      if(is.na(input$seed)==F){set.seed(input$seed)}
      seed<-input$seed
      if(is.na(seed)){
        seed<-1
      }
      message(paste0('seed',seed))

      ms<-list()
      set.seed(seed)
      set.seed(seed)
      ncodes<-input$xdim*input$ydim
      starters<-sample(1:nrow(datalist[[1]]), ncodes, replace = FALSE)
      init<-lapply(datalist, function(x) x[starters, , drop = FALSE])

      seq<-round(seq(1,input$rlen_play,length.out=input$rlen_snap))
      withProgress(min=1,max=length(seq),message="Running...",{
        for( i in seq) {

          set.seed(seed)
          m<-supersom(
            datalist,
            grid = kohonen::somgrid(
              input$xdim,
              input$ydim,
              topo = input$topo,
              toroidal = toroidal(),
              neighbourhood.fct=input$neighbourhood.fct
            ),
            rlen = i,
            init=init,
            dist.fcts = input$distmethod,
            alpha = c(input$a1, input$a2),
            radius = c(input$r1, input$r2),
            mode = input$mode,
            maxNA.fraction = input$maxna,
            normalizeDataLayers=as.logical(input$normalizeDataLayers)
          )
          ms[[length(ms)+1]]<-m
          incProgress(1)
        }
      })

      modelplay(ms)

    })
    observeEvent(input$ss1_somback_value,{
      if(input$ss1_somback_value=="uMatrix"){
        updatePickerInput(session,"ss1_bg_palette",selected="viridis")
      }
    })
    observeEvent(input$ss2_somback_value,{
      if(input$ss2_somback_value=="uMatrix"){
        updatePickerInput(session,"ss2_bg_palette",selected="viridis")
      }
    })
    observeEvent(ignoreInit = T,input$download_gwnp,{
      req(get_plot_animation2())
      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=get_plot_animation2()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Grid-Weighted Neuron Plot", name_c="grid_weig_neuronplot")
    })
    observeEvent(ss2_get_choices_pal(),{
      choices<-ss2_get_choices_pal()
      updatePickerInput(session,'ss2_bg_palette',
                        choices =  vals$colors_img$val[choices],
                        choicesOpt = list(
                          content =  vals$colors_img$img[choices] )
      )
    })
    observeEvent(ss2_getdata_layer(),{
      choices<-colnames(ss2_getdata_layer())
      updatePickerInput(session,'ss2_variable_pproperty',choices=choices)
    })
    observeEvent(df_symbol$val,{
      updatePickerInput(session,'ss1_pclus_symbol',
                        choices = df_symbol$val,
                        choicesOpt = list(content = df_symbol$img))
      updatePickerInput(session,'ss2_pclus_symbol',
                        choices = df_symbol$val,
                        choicesOpt = list(content = df_symbol$img))
    })
    observeEvent(ignoreInit = T,input$som_quality_help, {

      pkgs<-'aweSOM'
      citations<-do.call('c',lapply(pkgs, citation))
      citations<- lapply(citations,function(x){
        format(x, style = "html")
      })

      showModal(
        modalDialog(
          title = "Quality Measures",
          easyClose = TRUE,
          size = "l",
          column(12,class="modal_indent",
                 hr(),
                 p(

                   p("Quality measures computed using the aweSOM package (Boelaert, 2022). For multiple layers trained, iMESc implements these measures individually by data layer, except for Neuron utilization."),
                   p(h4("Quantization error:"),
                     div(
                       "This measure calculates the average squared distance between the data points and the map prototypes to which they are mapped. A lower value indicates better quality."
                     )),
                   p(h4("Percentage of explained variance:"),
                     div(
                       "Similar to other clustering methods, this measure represents the share of total variance that is explained by the clustering. It is calculated as 1 minus the ratio of quantization error to total variance. Higher values indicate better quality."
                     )),
                   p(h4("Topographic error:"),
                     div(
                       "This measure evaluates how well the topographic structure of the data is preserved on the map. It measures the proportion of observations for which the best-matching node is not a neighbor of the second-best matching node on the map. A lower value indicates better topographic representation. A value of 0 indicates excellent topographic representation, where all best and second-best matching nodes are neighbors, while a value of 1 represents the maximum error, where the best and second-best nodes are never neighbors."
                     )),
                   p(h4("Kaski-Lagus error:"),
                     div(
                       "The Kaski-Lagus error combines aspects of quantization and topographic error. It is computed as the sum of the mean distance between points and their best-matching prototypes, and the mean geodesic distance (pairwise prototype distances following the SOM grid) between the points and their second-best matching prototype."
                     )),
                   p(h4("Neuron Utilization error:"),
                     div(
                       "Neuron Utilization represents the percentage of neurons that are not the Best Matching Unit (BMU) of any observation. It provides insight into the utilization of neurons in the SOM."
                     )),
                   hr(),
                   h4("Reference:"),
                   p(HTML(paste0(citations)))
                 )
          )
        )
      )
    })
    observeEvent( input$data_confirm,{
      req(!is.null(vals$hand_save))
      switch(
        vals$hand_save,
        "Create Datalist  - Variables from VFM"=create_som_vfm(),
        "Create Datalist  - SOM predictions for New Data (X)"=create_predsom_newdata(),
        "Create Datalist  - SOM codebook predictions"=create_predsom_codebook(),
        "Create Datalist  - SOM performace"=create_predsom_errors(),
        "Create Datalist with Codebooks"=savecoodebok(),
        "Save new som in"=savesom(),

        "Save errors from som predictions (X)"=datalist_som_errorsX(),
        "Save errors from som predictions (Y)"=datalist_som_errorsY()
      )
      removeModal()

    })
    observeEvent(ignoreInit = T,input$data_som,{
      vals$cur_data<-vals$cur_somdataX<-input$data_som
    })

    observeEvent(input$ssom_layer1,{
      vals$cur_data<-input$ssom_layer1
    })


    observe({
      req(input$data_som%in%names(vals$saved_data))
      req(getdata_som())
      df=data.frame(na.omit(getdata_som()))
      N<-nrow(getdata_som())
      SIZE<-sqrt(5*sqrt(N))
      L<- floor(SIZE)
      res<-if(!nrow(df)>0){
        c(L*L,L,L)
      } else{
        topology(getdata_som(), dist = input$distmethod)
      }
      topo.reactive(res)
      if(isTRUE(input$sugtopo)){
        dim<-as.numeric(res[c(2,3)])

        updateNumericInput(session,"xdim",value= dim[1])
        updateNumericInput(session,"ydim",value= dim[2])
      }

    })
    observeEvent(list(input$xdim, input$ydim), {
      req(input$xdim)
      req(input$ydim)
      if (length( names(vals$saved_data)) > 0) {
        dim = try(topo.reactive(),silent=T )
        req(!inherits(dim,"try-error"))
        ydim<-dim[[3]]
        xdim<-dim[[2]]
        req(xdim)
        req(ydim)
        if (input$xdim != xdim|input$ydim != ydim) {
          updateCheckboxInput(session, "sugtopo", NULL, FALSE)
        } else{
          updateCheckboxInput(session, "sugtopo", NULL, TRUE)
        }

      }
    })






    observeEvent(input$data_som,ignoreInit = T,{
      choices<-c(colnames(attr(vals$saved_data[[input$data_som]],"factors")))
      updatePickerInput(session,'ss2_pclus_points_factor',choices=choices)
      updatePickerInput(session,'ss1_pclus_text_factor',choices=choices)
      updatePickerInput(session,'ss2_pclus_text_factor',choices=choices)
      updatePickerInput(session,'ss1_pclus_points_factor',choices=choices)
    })
    observe({
      if(!is.null(vals$ssom_tab0)){

        choices_datalist<-unlist(lapply(vals$saved_data,function(x){
          sum( rownames(x)%in%rownames(vals$saved_data[[vals$ssom_tab0[1,1]]]))==nrow(x)
        }))
        choices_datalist<-names(choices_datalist)[which(choices_datalist)]
        req(!is.null(vals$ssom_tab0))
        temp<-vals$ssom_tab0
        temp<-lapply(1:nrow(vals$ssom_tab0),function(i){

          div(class="small_picker",style="color: #05668D; border-top: 1px dashed gray",


              div(
                class="numlayers",
                if(i==nrow(vals$ssom_tab0)){
                  inline(div(tipify(actionLink(ns("ssom_minus"),icon("fas fa-minus"),style="margin-right: 20px;"),"Remove layer","top")))


                },strong(i)
              ),
              div(class="ensemble_labels",
                  pickerInput(ns(paste0("ssom_layer", i)), "", choices = if(i!=1){choices_datalist}else{names(vals$saved_data)},selected=vals$cur_somdataXlist_supersom[i], options=list(container="body"))
              )
              ,
              div(class="layers_wei",

                  numericInput(ns(paste0("ssom_wei", i)), "",
                               value = vals$cur_ssom_wei[i])
              ),
              div(class="layers_dist",

                  pickerInput(ns(paste0("ssom_dist", i)), "", choices = c("BrayCurtis","euclidean","sumofsquares" ,"manhattan", "tanimoto"),selected=vals$cur_ssom_dist[i], options=list(container="body")))


          )
        })
        vals$ssom_tab<-temp

      }})



    observeEvent(ignoreInit = T,input$tools_savesom,{
      if(input$tools_savesom %% 2){
        vals$hand_save<-"Save new som in"
        vals$hand_save2<-column(12,div(em(input$data_som, style="color:gray"),strong("::"), em("Som-Attribute", style="color:gray"),strong("::")
        ))
        vals$hand_save3<-em("Both 'create' and 'overwrite' options add the resulting BMU to the 'Factor-Attribute' of the corresponding Datalist."
        )
        showModal(
          module_som())}
    })



    observeEvent(input$mysupersom,{
      shinyjs::toggle('distmethod',condition=isFALSE(input$mysupersom))
    })
    observeEvent(input$finesom,{
      shinyjs::toggle('finetuning_som')
    })
    observeEvent(input$sompred_type,{
      shinyjs::toggle("sompred_type_datalist", condition = input$sompred_type =='Datalist')
    })
    observeEvent(input$ss1_varfacmap_action,shinyjs::toggle("ss1_varfac_out",condition=isTRUE(input$ss1_varfacmap_action)))
    observeEvent(input$ss1_pclus_addtext,{
      shinyjs::toggle("ss1_pclus_text_inputs",condition=isTRUE(input$ss1_pclus_addtext))
    })
    observe({
      data = ss1_getdata_layer()
      shinyjs::toggle('ss1_prev_property',condition=which(colnames(data)==input$ss1_variable_pproperty)!=1)

      shinyjs::toggle('ss1_next_property',condition=which(colnames(data)!=input$ss1_variable_pproperty)==ncol(data))
    })
    observeEvent(input$ss1_pclus_addpoints,{
      shinyjs::toggle("ss1_pclus_points_inputs",condition=isTRUE(input$ss1_pclus_addpoints))
    })
    observeEvent(input$ss1_somback_value,{
      shinyjs::toggle("ss1_var_pproperty",condition=input$ss1_somback_value=="property")
      shinyjs::toggle("ss1_property_layer",condition=input$ss1_somback_value=="property")
    })
    observe({
      condition=input$som_models%in%"new som (unsaved)"&input$som_tab!="som_tab1"
      shinyjs::toggle('tools_savesom', condition=condition)
    })
    observeEvent(input$som_tab,{
      shinyjs::toggle('partition_panel',condition=input$som_tab=="som_tab1")
      shinyjs::toggle("som_models_panel",condition=input$som_tab!="som_tab1")
    })
    observe({
      req(length(input$mysupersom)>0)
      condition=if(isTRUE(input$mysupersom)&input$som_tab=="som_tab1"){F} else{T}
      shinyjs::toggle('label_data_som',condition=condition)
      shinyjs::toggle('data_som_out',condition=condition)
    })
    observeEvent(input$usepartition,{
      shinyjs::toggle("partition_on",condition=input$usepartition)
    })
    observeEvent(input$ss2_somback_value,{
      shinyjs::toggle("ss2_property_layer_out",condition=input$ss2_somback_value=="property")
    })
    observe({
      data = ss2_getdata_layer()
      condition=which(colnames(data)==input$ss2_variable_pproperty)!=1
      shinyjs::toggle('ss2_prev_property',condition=condition)
      condition=which(colnames(data)!=input$ss2_variable_pproperty)==ncol(data)
      shinyjs::toggle('ss2_next_property',condition=condition)
    })
    observeEvent(input$ss2_pclus_addpoints,{
      shinyjs::toggle("ss2_pclus_points_inputs",condition=isTRUE(input$ss2_pclus_addpoints))
    })
    observeEvent(input$ss2_pclus_addtext,{
      shinyjs::toggle('ss2_pclus_text_inputs',condition=isTRUE(input$ss2_pclus_addtext))
    })
    observeEvent(input$ss2_varfacmap_action,{
      shinyjs::toggle("ss2_varfac_out",condition=isTRUE(input$ss2_varfacmap_action))
    })
    observeEvent(input$save_bug,{





      saveRDS(reactiveValuesToList(vals),"savepoint.rds")
      saveRDS(reactiveValuesToList(input),"input.rds")
      #saveRDS(vals$cur_caret_model,"m.rds")

      beep(10)
    })

  })



}
