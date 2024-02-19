options(shiny.autoload.r=FALSE)
#' @noRd



#' @export
hc_module<-list()
#' @export
hc_module$ui<-function(id){
  ns<-NS(id)
  fluidPage(


    tags$style(HTML("

                    #hc-model_or_data {

                    width: 100%;
                    padding-right: 15px;
                    }")),
    div(class = "well3",
        div(class = "choosechannel",
            div(style = "display: flex; flex-wrap: wrap;",
                div(class = "shiny-input-container", style = "padding-right: 15px;",
                    pickerInput(ns("data_hc"),
                                "Training Datalist (X):",
                                choices = NULL)
                ),
                inline(radioButtons(ns("model_or_data"), "Clustering target:", choiceValues = c("data", "som codebook"), choiceNames = c("Numeric-Attribute", "SOM-codebook"))),
                div(id=ns('som_hc_id'),class = "shiny-input-container", style = "padding-right: 15px;",
                    pickerInput(ns("som_hc"), "Som model:", choices = NULL)
                ),
                div(class = "shiny-input-container", style = "padding-right: 15px;width: 250px;",
                    pickerInput(ns("hc_fun"), span("HC function:", actionLink(ns("help_hc_fun"), icon("fas fa-question-circle"))), choices = list("Hierarchical Clustering" = "hclust", "Agglomerative Nesting" = "agnes", "Divisive hierarchical clustering" = "diana"))
                ),
                div(id=ns("disthc_id"),
                    class = "shiny-input-container", style = "padding-right: 15px;width: 150px",
                    pickerInput(ns("disthc"), "Distance:", choices = c('bray', "euclidean", 'jaccard'))
                ),
                div(class = "shiny-input-container", style = "padding-right: 15px;;width: 150px",
                    pickerInput(ns("method.hc0"), "Method:", choices = c("ward.D2", "ward.D", "single", "complete", "average", "mcquitty", "median", "centroid"))
                )
            )
        )

    ),
    radioGroupButtons(
      ns("hc_tab"),
      choiceValues =c("hc_tab1","hc_tab2","hc_tab3","hc_tab4","hc_tab6"),
      choiceNames=c("1. Dendrogram","2. Scree Plot",'3. Cut Dendrogram','4. Codebook clusters','5. Codebook screeplot'),
      status ="radio_hc",
      selected='hc_tab4'
    ),
    div(style="margin-left:5px;background: white",

        sidebarLayout(
          sidebarPanel(
            div(
              uiOutput(ns("hc_save_tab34")),
              uiOutput(ns("hc_save_tab5")),
              div(class="map_control_style2",style="color: #05668D",

                  uiOutput(ns("hc_side1")),
                  uiOutput(ns("hc_side2")),
                  uiOutput(ns("hc_side3")),
                  uiOutput(ns("hc_ordcluster")),
                  uiOutput(ns("hc_side4")),
                  # uiOutput("hc_side5"),

                  uiOutput(ns("hc_side6")),
                  uiOutput(ns("showerrors_som")),
                  div(
                    actionLink(ns('down_hc_plot'), '+ Download plot')
                  )
              )
            )
          ),
          mainPanel(
            uiOutput(ns('main_hc')),
            uiOutput(ns('predsom_hc')),
            uiOutput(ns("hc_tab1_out")),
            uiOutput(ns("hc_tab2_out")),
            uiOutput(ns("hc_tab3_out")),
            uiOutput(ns("hc_tab4_out")),
            uiOutput(ns("hc_tab6_out"))
          )
        )),
    uiOutput(ns('clustering_panel'))
  )
}

#' @export
hc_module$server<-function(id, vals){
  moduleServer(id,function(input, output, session) {
    ns<-session$ns


    observeEvent(vals$saved_data,{
      updatePickerInput(session,"data_hc",choices=names(vals$saved_data), selected=vals$cur_data)
    })



    observeEvent(list(choices_hc(),vals$cur_model_or_data),{
      req(choices_hc())
      if(!any(choices_hc()==vals$cur_model_or_data))
        vals$cur_model_or_data<-NULL
    })

    observeEvent(getdata_hc(),{
      choiceValues<-choices_hc()
      choiceNames<-choices_hc_names()

      updateRadioButtons(session,"model_or_data",choiceNames =choiceNames,choiceValues =choiceValues,selected=vals$cur_model_or_data )
    })

    observe({
      if(is.null(vals$cur_hc_tab)){
        vals$cur_hc_tab<-"hc_tab1"
      }
    })

    observeEvent(input$hc_tab,{

      vals$cur_hc_tab<-input$hc_tab
    })

    observeEvent(list(input$hc_tab,input$model_or_data), {
      req(input$hc_tab)
      req(input$model_or_data)
      if(input$model_or_data!="som codebook"){
        if(!is.null(vals$cur_hc_tab)){
          if(vals$cur_hc_tab%in%c('hc_tab4','hc_tab6')){
            vals$cur_hc_tab<-NULL
          }
        } else{
          vals$cur_hc_tab<-"hc_tab1"
        }

      }

    })
    observe({
      req(input$hc_tab)
      req(vals$cur_hc_tab)
      req(input$data_hc)
      req(input$model_or_data)
      choiceValues =c("hc_tab1","hc_tab2","hc_tab3","hc_tab4","hc_tab6")
      choiceNames=c("1. Dendrogram","2. Scree Plot",'3. Cut Dendrogram','4. Codebook clusters','5. Codebook screeplot')
      if(input$model_or_data=="som codebook"){
        choices= names(attr(getdata_hc(), "som"))
        updatePickerInput(session,"som_hc", choices=choices, selected=vals$cur_som_hc)
        shinyjs::show("som_hc_id")
        shinyjs::hide("disthc_id")
        updateRadioGroupButtons(session,'hc_tab',choiceNames=choiceNames,choiceValues=choiceValues, selected=vals$cur_hc_tab)


      } else{
        updateRadioGroupButtons(session,'hc_tab',choiceNames=choiceNames[1:3],choiceValues=choiceValues[1:3], selected=vals$cur_hc_tab)
        shinyjs::show("disthc_id")
        shinyjs::hide("som_hc_id")
      }

    })





    output$hcsom_newdata_control<-renderUI({
      splitLayout(class = "well3",
                  div(style = ' border-bottom: 1px solid',
                      div(
                        inline(checkboxInput(ns("hcsom_newdata"), span("+ Map new data", tiphelp("Check to add new data points to the trained SOM", "right"), uiOutput(ns("hcsom_newdata_mess"))), vals$hcsom_newdata, width = '250px'))
                      ),

                      uiOutput(ns("out_hcsom_whatmap")),

                  )

      )
    })
    output$out_hcsom_whatmap<-renderUI({
      req(input$hcsom_newdata)
      req(isTRUE(input$hcsom_newdata))
      req(input$model_or_data)
      req(input$model_or_data == "som codebook")
      layers<-getsom_layers()
      div(style = "margin-left: 20px;",
          strong("Layers:"),
          lapply(layers, function(x) {
            div(class = "map_control_style2", style = "color: #05668D",
                inline(checkboxInput(ns(paste0("hcsom_layer", x)), x, TRUE)),
                inline(uiOutput(ns(paste0("hcsom_piclayer", x))))
            )
          })
      )
    })

    output$hclabs_control <-renderUI({
      req(input$model_or_data=='data')
      pickerInput(
        ns("labhc"),
        "Labels",
        choices = c(colnames(attr(getdata_hc(),"factors"))),
        width="200px",
        selected=vals$labhc
      )

    })
    output$hcut_labels<-renderUI({
      req(input$model_or_data)
      req(input$model_or_data=="data")
      data<-getdata_hc()
      choices<-c(colnames(attr(data,"factors")))
      pickerInput(ns("hcut_labels"),"+ Factor",
                  choices = c("rownames",choices),selected=vals$hcut_labels)
    })

    output$clustering_panel<-renderUI({
      req(getdata_hc())
      div(

        br(),
        div(style="background: white",
            uiOutput(ns("hc_panels"))
        )
      )
    })



    #tab2
    output$hc_side2<-renderUI({

      div(
        uiOutput(ns("model_WSS_out")),
        div(
          actionLink(ns('down_results_screeplot'),"+ Download Results", style="button_active")
        )
      )
    })
    output$model_WSS_out<-renderUI({
      div(inline(uiOutput(ns("screeplot_hc_inputs"))),
          div(uiOutput(ns('smw_hc_out'))) )
    })
    output$screeplot_hc_inputs<-renderUI({
      if(input$model_or_data=="som codebook"){
        m<- getmodel_hc()
        data<-m$codes[[1]]

      } else{
        data<-  getdata_hc()
      }


      div(
        tags$div(style="display: inline-block; vertical-align: top;", numericInput(ns("screeplot_hc_k"), span(strong("+ k"), tiphelp("maximum number of clusters to be tested")), value = round(nrow(data)/2))),
        tags$div(id='run_screeplot_hc_btn',style="display: inline-block; vertical-align: top;", class="save_changes",actionButton(ns("run_screeplot_hc"), "Run screeplot"))
      )
    })
    output$smw_hc_out<-renderUI({
      renderUI({
        div(style="margin-top: 20px; border-top: 1px solid gray",
            div(
              inline(uiOutput(ns("show_smw_hc"))),
              inline(uiOutput(ns("hc_smw_run")))
            ),
            uiOutput(ns("hc_smw_control")))
      })

    })
    output$show_smw_hc<-renderUI({
      req(!is.null(vals$screeplot_results))
      if(is.null(vals$show_smw_hc)){vals$show_smw_hc<-F}
      inline(checkboxInput(ns("show_smw_hc"), value = vals$show_smw_hc, smw_labinfo()))
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
      tags$div(class=class,actionButton(ns("run_smw_hc"),"RUN smw"))
    })
    output$hc_smw_control<-renderUI({
      req(isTRUE(input$show_smw_hc))
      div(
        # uiOutput("smw_hc_seed_out"),
        uiOutput(ns("smw_hc_w_out")),
        uiOutput(ns("smw_hc_rand_out")),
        uiOutput(ns("smw_hc_tol_out"))
      )
    })
    output$smw_hc_w_out<-renderUI({
      ws=seq(2,length(vals$screeplot_results0$WSS)/2,by=2)
      tags$div(id="smw_hc_pool",
               textInput(
                 ns("smw_hc_w"),
                 span("+ Window sizes",tiphelp("comma-delimeted")),
                 value = paste0(ws,collapse=", ")
               )
      )
    })
    output$smw_hc_rand_out<-renderUI({
      if(is.null(vals$smw_hc_rand)){vals$smw_hc_rand<-50}
      numericInput(ns("smw_hc_rand"),"+ N randomizations",vals$smw_hc_rand)
    })
    output$smw_hc_tol_out<-renderUI({
      if(is.null(vals$smw_hc_tol)){vals$smw_hc_tol<-1.5}
      numericInput(ns("smw_hc_tol"), span("+ tol", tiphelp("Adjusts sensitivity when identifying potential breakpoints. If the dissimilarity score (DS) exceeds -tol- times the standard deviation, a breakpoint is suggested.")), vals$smw_hc_tol, step=0.1)
    })
    output$smw_hc_seed_out<-renderUI({
      numericInput(ns("smw_hc_seed"),"+ Seed",NA)
    })

    #tab3
    output$hc_palette<-renderUI({
      req(!input$hc_tab%in%c('hc_tab4','hc_tab6'))
      pickerInput(inputId = ns("hcdata_palette"),
                  label = "+ HC Palette:",
                  choices = vals$colors_img$val,
                  choicesOpt=list(content=vals$colors_img$img),
                  selected=vals$hcdata_palette)
    })
    output$hcs_side_somplot<-renderUI({
      div(class="map_control_style2",style="color: #05668D",
          div(
            uiOutput(ns('hcs_palette')),
            div(
              style='border-bottom: 1px solid gray',
              uiOutput(ns('out_pclus_addpoints')),
              div(
                style="margin-left: 10px",
                uiOutput(ns("pclus_points_inputs"))
              )
            ),
            div(style='border-bottom: 1px solid gray',
                uiOutput(ns('out_pclus_addtext')),
                div(
                  style="margin-left: 10px",
                  uiOutput(ns("pclus_text_inputs"))
                )),
            div(
              uiOutput(ns("vfm_check"))
            ),
            uiOutput(ns("hcs_theme")),
            uiOutput(ns("hcs_title"))
          ))
    })
    output$hcs_palette<-renderUI({
      choices=getgrad_col()

      div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',
          pickerInput(inputId = ns("bg_palette"),
                      label ="+ Background palette",
                      choices =  vals$colors_img$val[choices],
                      choicesOpt = list(
                        content =  vals$colors_img$img[choices] ),
                      options=list(container="body")),

          tipify(numericInput(ns("pcodes_bgalpha"),"+ Background lightness",value = 0,min = 0,max = 1,step = .1),"symbol size"),

          pickerInput(ns("pclus_border"),
                      label ='+ Border:',
                      choices =  vals$colors_img$val[getsolid_col()] ,
                      choicesOpt = list(
                        content =  vals$colors_img$img[getsolid_col()] ),
                      selected= "white"),

          tipify(numericInput(ns("base_size"),"+ Base size",value = 12),"symbol size")


      )
    })
    output$out_pclus_addpoints<-renderUI({
      checkboxInput(ns("pclus_addpoints"),"+ Points",
                    value=T)
    })
    output$pclus_points_inputs<-renderUI({
      req(isTRUE(input$pclus_addpoints))
      div(
        uiOutput(ns("pclus_points_palette")),
        uiOutput(ns("pclus_points_factor_out")),
        uiOutput(ns("pclus_points_shape")),
        uiOutput(ns("pclus_points_size"))
      )
    })
    output$out_pclus_addtext<-renderUI({

      checkboxInput(ns("pclus_addtext"),"+ Labels",
                    value=F)
    })

    output$hc_tab3_out<-renderUI({

      div(
        div(id=ns("hcut_btn"),
            actionButton(ns("run_hc"),"RUN >>")
        ),
        uiOutput(ns("hcut_out"))

      )
    })



    observeEvent(input$run_hc,{
      args<-hcut_argsplot()
      #saveRDS(args,"args.rds")
      p<-do.call('hc_plot',args)
      #saveRDS(p,"p.rds")
      # p<-readRDS("p.rds")
      #req(class(p)[1]=="gg")
      vals$cur_hc_plot<-vals$hc_tab3_plot<- p

      output$hcut_out<-renderUI({

        withSpinner(type=8,color="SeaGreen",plotOutput(ns("hcut_plot")))
      })
      shinyjs::removeClass("hcut_btn","save_changes")

    })





    output$customKdata<-renderUI({

      if(is.null(vals$saved_kcustom)){vals$saved_kcustom=2 }
      k<-vals$saved_kcustom
      numericInput(ns("customKdata"),
                   "+ Number of clusters: ",
                   value = k,
                   step = 1)

    })
    output$hc_side3<-renderUI({

      div(class="map_control_style2",style="color: #05668D",
          div(
            div(uiOutput(ns("customKdata"))),

            div(uiOutput(ns('hc_palette'))),
            div(uiOutput(ns('hcut_labels'))),
            div(uiOutput(ns('hcut_theme')))


          )
      )
    })

    output$hcut_theme<-renderUI({

      div(
        pickerInput(ns("hcut_theme"),"+ Theme:",c('theme_minimal','theme_grey','theme_linedraw','theme_light','theme_bw','theme_classic')),
        numericInput(ns("hcut_cex"),"+ Size",value = 12,step = 1),
        numericInput(ns("hcut_lwd"),"+ Line width",value = .5,step = .5),
        textInput(ns("hcut_main"),"+ Title","Cluster Dendrogram"),
        textInput(ns("hcut_ylab"),"+ y label","Height"),
        textInput(ns("hcut_xlab"),"+ x label","Observations"),
        numericInput(ns("hcut_xlab_adj"),"+ xlab v-adj",value = 30,step = 1),
        numericInput(ns("hcut_offset"),"+ offset",value = -.1,step = 0.05),
        radioButtons(ns("hcut_legend_type"),"+ Legend:",c("inside","outside"), inline=T)
      )
    })

    output$hc_side1<-renderUI({

      value=if(input$model_or_data=="som codebook"){
        paste0(input$data_hc,"(SOM codebook)")
      } else{ input$data_hc}
      div(
        div(span("+ Title",
                 inline(textInput(ns("hc_title"), NULL, value =value, width="200px"))
        )),
        uiOutput(ns('hclabs_control'))
      )
    })
    output$hc_tab1_out<-renderUI({

      div(
        uiOutput(ns("hcdata_plot")),
        uiOutput(ns("hcmodel_plot"))
      )
    })
    output$hc_save_tab34<-renderUI({

      req(input$hc_tab=="hc_tab3"|input$hc_tab=="hc_tab4")
      if(input$hc_tab=="hc_tab4"){
        req(isFALSE(input$hcsom_newdata))
        req(!is.null(args_bmu()))
      } else{
        req(!is.null(vals$hc_tab3_plot))
      }


      div(
        div(inline(uiOutput(ns("saveHC"))))
      )
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

    output$hc_panels<-renderUI({
      #req(input$model_or_data)

    })
    output$hc_ordcluster<-renderUI({
      div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; padding-bottom: 5px',
          div(
            checkboxInput(ns("hc_sort"),span("+ Sort clusters",tiphelp("Sort clusters by a  variable")),value=vals$hc_sort,width="120px"),
            div(style="margin-left: 40px",
                inline(uiOutput(ns("hc_sort_datalist"))) ,

                inline(uiOutput(ns("hc_ord_factor"))))

          )
      )
    })
    output$hc_sort_datalist<-renderUI({
      req(isTRUE(input$hc_sort))
      div(
        div("Datalist:"),
        inline(pickerInput(ns("hc_ord_datalist"),NULL,choices = c(names(vals$saved_data[getdata_for_hc()])),selected=vals$hc_ord_datalist,width="200px")),  "::"
      )
    })
    output$hc_ord_factor<-renderUI({
      req(isTRUE(input$hc_sort))
      req(input$hc_ord_datalist)
      data<-vals$saved_data[[input$hc_ord_datalist]]

      choices=c(colnames(data))

      div(
        div("Variable:"),
        pickerInput(ns("hc_ord_factor"),NULL,
                    choices = choices,selected=vals$hc_ord_factor,width="120px")
      )

    })

    output$hc_side4<-renderUI({

      req(input$model_or_data == "som codebook")


      div(style="max-height:300px;overflow-y: scroll;",
          uiOutput(ns("hcs_side_somplot")),
          div(actionLink(ns('create_codebook'),"+ Create Datalist with the Codebook and HC class")),
          div(div(
            tipify(downloadLink(ns('down_hc_model'),"+ Download HC model", style="button_active"),"Download file as .rds")
          ))
      )
    })




    output$pclus_points_palette<-renderUI({
      req(length(input$hcsom_newdata)>0)
      choices = vals$colors_img$val
      choicesOpt = list(content = vals$colors_img$img)
      tipify(pickerInput(inputId = ns("pclus_points_palette"),
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
        pickerInput(ns("pclus_points_factor"),"+ Factor",
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
      tipify(pickerInput(inputId = ns("pclus_symbol"),
                         label = "+ Shape",
                         choices = df_symbol$val,
                         choicesOpt = list(content = df_symbol$img),
                         options=list(container="body"))
             ,"symbol shape",'right')
    })
    output$pclus_points_size<-renderUI({

      tipify(numericInput(ns("pclus_points_size"),"+ Size",value = 1,min = 0.1,max = 3,step = .1),"symbol size","right")

    })
    output$pclus_text_inputs<-renderUI({
      req(isTRUE(input$pclus_addtext))
      div(
        uiOutput(ns("pclus_text_palette")),
        uiOutput(ns("pclus_text_factor_out")),
        uiOutput(ns("pclus_text_size"))
      )
    })
    output$pclus_text_palette<-renderUI({
      tipify(pickerInput(inputId = ns("pclus_text_palette"),
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
      pickerInput(ns("pclus_text_factor"),"+ Factor",
                  choices = c(choices))
    })
    output$pclus_text_size<-renderUI({

      tipify(numericInput(("pclus_text_size"),"+ Size",value = 1,min = 0.1,max = 3,step = .1),"symbol size","rigth")

    })
    output$vfm_check<-renderUI({

      div(style='border-bottom: 1px solid gray',
          checkboxInput(ns("varfacmap_action"),
                        span("+ Variable factor map",
                             actionLink(ns("varfacmap"), tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value =T),
          div(style="margin-left: 10px",
              uiOutput(ns("varfac_out")))
      )
    })
    output$vfm_type_out<-renderUI({
      my_choices<-list("Highest"='var', "Clockwise"="cor","Cluster"="cor_hc")
      pickerInput(ns("vfm_type"),"+ Show correlation:",
                  choices =my_choices


      )
    })
    output$varfac_out<-renderUI({
      req(isTRUE(input$varfacmap_action))

      div(
        uiOutput(ns('vfm_type_out')),
        uiOutput(ns('npic_out')))})
    output$npic_out<-renderUI({
      div(
        tipify(
          numericInput(ns("npic"), "+ Number", value = 10, min = 2),"Number of variables to display","right"
        ),

        numericInput(ns("pclus.cex.var"), "+ Var size", value = 1, min = 2),
        div(class="palette",
            pickerInput(ns("p.clus.col.text"),
                        label = "+ Var text color",
                        choices = vals$colors_img$val[getsolid_col()],
                        choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),
                        selected="black")),
        pickerInput(ns("var_bg"),
                    label = "+ Var background",
                    choices = vals$colors_img$val[getsolid_col()],
                    choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),
                    selected="white"),
        tipify(
          numericInput(ns("var_bg_transp"), "+ Var transparency", value = 0, min = 2),"Number of variables to display","right"
        )

      )
    })
    output$hcs_title<-renderUI({
      textInput(ns("hcs_title"), "+ Title: ", "")
    })
    output$hcs_theme<-renderUI({
      div(style = 'border-bottom: 1px solid gray',
          inline(checkboxInput(ns("hcs_theme"),
                               label = "+ show neuron coordinates",
                               value = F,
                               width = "200px"
          ))
      )
    })
    output$hc_tab4_out<- renderUI({

      req(input$model_or_data == "som codebook")
      div(
        div(id=ns("run_bmu_btn"),
            actionButton(ns("run_bmu"),"RUN >>")
        ),
        uiOutput(ns("BMU_PLOT")),
        uiOutput(ns('hcsom_newdata_control'))
      )
    })




    args_bmu<-reactiveVal()
    observeEvent(input$run_bmu,{
      m<-getmodel_hc()
      somC<-phc()
      args<-argsplot_somplot()
      args$hc<-phc()$som.hc
      args_bmu(args)
      shinyjs::removeClass("run_bmu_btn","save_changes")
      shinyjs::show()
    })
    observeEvent(args_bmu_inputs(),{
      shinyjs::addClass("run_bmu_btn","save_changes")
      vals$hc_tab4_plot<-NULL
      args_bmu(NULL)
    })

    args_bmu_inputs<-reactive({

      args<-list(
        getmodel_hc(),phc(),indicate_hc(),vals$cutsom,input$hcsom_newdata,getsom_layers(),hcsom_whatmap(),getmodel_hc0(),hcsom_active_layers(),
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

        cex.var=as.numeric(input$pclus.cex.var),
        col.text=input$p.clus.col.text,
        col.bg.var=input$var_bg,
        col.bg.var.alpha=1-input$var_bg_transp,
        base_size=input$base_size,
        show_neucoords=input$hcs_theme,
        newdata=input$newdata_hc,
        title=input$hcs_title

      )

      args
    })






    output$somcut_display<-renderUI({
      div(span("+ Display:", inline(
        radioButtons(
          ns("dot_label_clus"), NULL, choices = c("labels", "symbols"), inline = TRUE, width = "100px", selected = vals$dot_label_clus)
      )))
    })
    output$showerrors_som<-renderUI({

      div(style="padding-top: 20px",
          checkboxGroupInput(ns("show_mapcode_errors"), '+ Show error: ',
                             choices = c("Within Sum of Squares", "Dendrogram Height"), width = "200px", selected = vals$show_mapcode_errors),
          textInput(ns('code_screeplot_title'), "+ Title", input$som_hc),
          pickerInput(ns('code_screeplot_agg'), "Aggregate Errors", c("Mean", "Median", "Sum"))
      )
    })
    output$hc_save_tab5<-renderUI({
      req(isTRUE(input$hcsom_newdata))
      req(input$hc_tab=="hc_tab4")
      req(!is.null(args_bmu()))
      req(!is.null(args_bmu()))
      div(class = "save_changes",
          bsButton(ns("savemapcode"), icon(verify_fa = FALSE, name = NULL, class = "fas fa-save"), style = "button_active", type = "action", value = FALSE), span(style = "font-size: 12px", icon(verify_fa = FALSE, name = NULL, class = "fas fa-hand-point-left"), "Create Datalist")
      )
    })
    output$hc_tab6_out<-renderUI({

      div(
        div(
          inline(numericInput(ns("mapcode_loop_K"), "K", 20, width = "100px")),
          inline(actionButton(ns("mapcode_loop_go"), "Run loop")),
        ),
        uiOutput(ns("mapcode_loop"))
      )
    })
    output$hc_control<-renderUI({})

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

      div(
        div(
          inline(
            div(class = class1,
                actionButton(ns("tools_savehc"), icon(verify_fa = FALSE, name = NULL, class = "fas fa-save"), style = "button_active", type = "action", value = FALSE),
            )
          ), span(style = "font-size: 12px", icon(verify_fa = FALSE, name = NULL, class = "fas fa-hand-point-left"), "Save Clusters in Datalist ", strong("X"), class = class3)
        ),
        div(style = "margin-bottom: 5px", class = class2,
            em(paste0("The current clustering is saved in the Factor-Attribute as '", paste0(clu_al, collapse = "; "), "'"))
        )

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
      span("select a gradient palette in '+ Points' to differentiate between training and the new data", style="color: gray")

    })


    hcut_argsplot<-reactive({

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
    getdata_hc<-reactive({
      req(input$data_hc)
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
    smw_labinfo<-reactive({
      strong(
        "split moving window",
        tipify(
          actionLink("bphelp", icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),
          "Performs split moving window to detect significant discontinuities in the relationship between the number of clusters and WSS values. Click for more information."
        )
      )
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
                 hc=phc()$som.hc
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
    labhc<-reactive({
      req(input$labhc)
      as.character(attr(getdata_hc(),"factors")[rownames(getdata_hc()),as.character(input$labhc)])
    })
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
    observeEvent(input$run_smw_hc,{
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

    observeEvent(ignoreInit = T,input$hc_ord_datalist,{
      vals$hc_ord_datalist<-input$hc_ord_datalist
    })
    observeEvent(ignoreInit = T,input$hc_sort,{
      vals$hc_sort<-input$hc_sort
    })
    observeEvent(ignoreInit = T,input$hc_ord_factor,{
      vals$hc_ord_factor<-input$hc_ord_factor
    })
    observeEvent(ignoreInit = T,input$hcs_theme,{
      vals$hcs_theme<-input$hcs_theme})
    observeEvent(ignoreInit = T,input$pclus_addtext,{
      vals$pclus_addtext<-input$pclus_addtext
    })
    observeEvent(ignoreInit = T,input$pclus_addpoints,{
      vals$pclus_addpoints<-input$pclus_addpoints
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
      attr(attr(vals$saved_data[[input$data_hc]],"som")[[input$som_hc]],"codebook_screeplot")<-result


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
            pickerInput(ns(paste0("hcsom_newdata_layer",x)), NULL, choices_temp)
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
    gosave<-reactiveValues(df=0,  modal=F)
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
    observeEvent(ignoreInit = T,input$data_confirm,{
      vals$cur_hc_plot<-vals$hc_tab3_plot
      save_switch()
      removeModal()
    })
    observeEvent(hcut_argsplot(),{
      if(is.null(vals$cur_hc_plot)){
        shinyjs::addClass("hcut_btn","save_changes")
      }

      vals$hc_tab3_plot<-vals$cur_hc_plot
      vals$cur_hc_plot<-NULL

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

      args<-args_bmu()
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


    observeEvent(ignoreInit = T,input$cancel_save,{
      removeModal()
    })


    output$hcut_plot<-renderPlot({
      print(vals$hc_tab3_plot)
      # vals$hc_tab3_plot
    })
    output$BMU_PLOT<-renderUI({
      args<-args_bmu()
      req(args)
      renderPlot({
        vals$hc_tab4_plot<- p<-do.call(bmu_plot_hc,args)
        p
      })


    })
    output$plot_hc_screeplot <-renderUI({
      vals$scree_plot_hc<-getsmw_plot()
      renderPlot({print(vals$scree_plot_hc)})

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
          vals$hc_tab6_plot<-p
          p

        })
      )


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


    observeEvent(input$hc_tab,{
      if(input$hc_tab=='hc_tab1'){
        shinyjs::show('hc_side1')
        shinyjs::show('hc_tab1_out')

      } else{
        shinyjs::hide('hc_tab1_out')
        shinyjs::hide('hc_side1')
      }
      if(input$hc_tab=="hc_tab6"){
        shinyjs::show('showerrors_som')
        shinyjs::show('hc_tab6_out')


      } else{
        shinyjs::hide('hc_tab6_out')
        shinyjs::hide('showerrors_som')

      }

      if(input$hc_tab=="hc_tab4"){


        shinyjs::show('hc_side4')
        shinyjs::show('hc_tab4_out')

      } else{

        shinyjs::hide('hc_tab4_out')
        shinyjs::hide('hc_side4')
      }

      if(input$hc_tab=="hc_tab2"){
        shinyjs::show('hc_side2')
        shinyjs::show('hc_tab2_out')
      } else{
        shinyjs::hide('hc_side2')
        shinyjs::hide('hc_tab2_out')
      }

      if(input$hc_tab=="hc_tab3"){
        shinyjs::show('hcut_theme')


        shinyjs::show('hcut_out')
        shinyjs::show('hc_tab3_out')
        shinyjs::show('hcut_main')
      } else{
        shinyjs::hide('hcut_theme')
        shinyjs::hide('hcut_out')
        shinyjs::hide('hc_tab3_out')
        shinyjs::hide('hcut_main')
      }
      if(input$hc_tab!="hc_tab6"){
        shinyjs::show("customKdata")
      } else{
        shinyjs::hide("customKdata")
      }
      if(input$hc_tab!="hc_tab1"&input$hc_tab!="hc_tab2"){
        shinyjs::show("hc_side3")
      } else{
        shinyjs::hide("hc_side3")
      }

      if(input$hc_tab==c('hc_tab3')){
        shinyjs::show('hc_ordcluster')
      } else{
        shinyjs::hide('hc_ordcluster')
      }
    })

  })
}






