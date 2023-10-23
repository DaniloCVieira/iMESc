#' @export


## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.

module_ui_comp2 <- function(id){
  ns <- NS(id)

  div(class='choosechannel',
      includeCSS("inst/app/www/styles.css"),

      div(
    # uiOutput(ns("bug")),
  #inline( actionButton(ns("teste_comb"),"SAVE")),
        # inline(uiOutput(ns("teste_comb")))
      ),
      uiOutput(ns('COMB_comp2')))

}

#' @export
module_server_comp2 <- function (input, output, session,vals,df_colors,newcolhabs){
  ns <- session$ns

  cur_step<-reactiveValues(df=4)

  insert_npages <- 3

  output$COMB_comp2<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found"))
    #
    #
    #

    column(12,id="nav_ensemble",
           tags$div(style="",
                    navbarPage(NULL,
                               header=uiOutput(ns("ensemble_control")),
                               id=ns("ensemble_pages"),
                               tabPanel("1. Select the models",value="page1",
                                        uiOutput(ns("page1"))),
                               tabPanel("2. Ensemble Parameters",value="page2",
                                        uiOutput(ns("pagina2"))),
                               tabPanel("3. Results",value="page3",
                                        uiOutput(ns("page3")) )
                    ),
                    div(class="well3",
                        div(style="position: absolute; right: 0px; top: 0px; z-index: 999;width: 450px",
                          class="ensemble_in",

                          uiOutput(ns('weis_out')),


                        )
                    )
           ),

    )


  })

  output$ensemble_control<-renderUI({
    column(12,class="ensemble_control",
           column(6,
                  style = "padding-left:0;",
                  uiOutput(ns("comp2_prev_bnt"))),
           column(6, align="right",
                  style = "padding-right:0;",
                  uiOutput(ns("comp2_next_bnt"))
           )
    )
  })
  output$comp2_prev_bnt<-renderUI({
    req(input$ensemble_pages!="page1")
    actionButton(ns("comp2_prev"),span(
      div(strong("<< Prev")),

    ), width="70px", style="height: 30px; font-size: 11px;padding: 2px;margin: 0px;box-shadow: 0 0px 8px 0 rgba(0,0,0,0.2), 0 6px 0px 0 #f8f8f8;;")

  })
  output$comp2_next_bnt<-renderUI({
    req(input$ensemble_pages!="page3")
    div(
      actionButton(ns("comp2_next"),span(
        div(strong("Next >>")),

      ), width="70px", style="height: 30px; font-size: 11px;padding: 2px;margin: 0px;box-shadow: 0 0px 8px 0 rgba(0,0,0,0.2), 0 0px 10px 0 #f8f8f8;;")
    )
  })

  observeEvent(ignoreInit = T,input$comp2_prev,{
    if(input$ensemble_pages=="page2"){
      updateTabsetPanel(session,'ensemble_pages',"page1")
    } else if(input$ensemble_pages=="page3"){
      updateTabsetPanel(session,'ensemble_pages',"page2")
    }
  })
  observeEvent(ignoreInit = T,input$comp2_next,{
    if(input$ensemble_pages=="page1"){
      updateTabsetPanel(session,'ensemble_pages',"page2")
    } else if(input$ensemble_pages=="page2"){
      updateTabsetPanel(session,'ensemble_pages',"page3")
    }
  })



  observeEvent(ignoreInit = T,input$ensemble_models,{
    if(input$ensemble_models!="New Ensemble"){

      if(is.null(input$data_ensemble_X)){
        updateTabsetPanel(session,'ensemble_pages',"page2")
        delay(500,updateTabsetPanel(session,'ensemble_pages',"page3"))
      }

    }

  })

  output$selectallmodels<-renderUI({

    div(inline(checkboxInput(ns("models_check"),'Select/unselect all models from the selected box',T,width="325px")))
  })



  observeEvent(ignoreInit = T,input$Ensemble_tab2,{
    vals$Ensemble_tab2<-input$Ensemble_tab2
  })

  observeEvent(ignoreInit = T,input$Ensemble_tab3,{
    vals$Ensemble_tab3<-input$Ensemble_tab3
  })

  output$page3<-renderUI({

    # validate(need(,"The New Ensemble is empty."))
    #cat("\n",vals$Ensemble_tab2)
    # cat("\n",vals$ensemble_tab)
    #req(is.data.frame(get_predtab()))

    fluidPage(
      uiOutput(ns('out_step10_ensemble')),
      column(12,style="background: white",
             fluidRow(
               tabsetPanel(

                 id=ns("Ensemble_tab3"),

                 tabPanel("4.1. Pool results",
                          value="tab_pool",
                          uiOutput(ns("entab_2"))

                 ),

                 tabPanel("4.2. Select the Ensemble",
                          value="tab_sele",
                          uiOutput(ns("entab_1"))
                 ),


                 tabPanel("4.3. Model Predictions",
                          value="tab_model",
                          uiOutput(ns("entab_3"))
                 ),
                 tabPanel("4.4. Predictions",
                          value="tab_pred",
                          uiOutput(ns("entab_4"))
                 ),
                 tabPanel("4.5. Performace",
                          value="tab_perf",
                          uiOutput(ns("entab_5"))
                 ),
                 tabPanel("4.6. Observation errors",
                          value="tab_obserr",
                          uiOutput(ns("entab_6"))
                 ),
                 tabPanel("4.7. Plot model predictions",
                          value="tab_plotpred",
                          uiOutput(ns("entab_7"))
                 ),
                 tabPanel(strong("4.8. Feature importance"),
                          value="tab_feat",
                          uiOutput(ns("entab_8"))
                 ),

                 tabPanel(strong("4.9. Interactions"),
                          value="tab_iter",
                          uiOutput(ns("entab_9"))

                 )
               )
             ))

    )
  })


  output$entab_1<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))

    em<-get_the_model2()
    validate(need(sum(!sapply(em$pool_metrics,is.null))==2,"Results are not accessible unless the 'Search Best Ensemble' option is enabled prior to running the analysis."))


    column(12,
           column(5,class='well3',
                  div(class="map_control_style2",style="color: #05668D",
                      uiOutput(ns("pool_selection_side")))
           ),
           column(7,
                  uiOutput(ns("pool_selection_out"))),
           column(12,class="map_control_style2",
                  div(strong("Filtered pool list:")),
                  numericInput(ns("round_poollist"),"Round", 3),
                  div(actionLink(ns("downFilteredlist"),"+ Download table")),
                  DT::dataTableOutput(ns('pool_filtered_list'))
           )
    )
  })

  observeEvent(ignoreInit = T,input$downFilteredlist,{

    vals$hand_down<- "Filtered pool list"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)


  })

  output$entab_2<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))

    em<-get_the_model2()
    validate(need(sum(!sapply(em$pool_metrics,is.null))==2,"Results are not accessible unless the 'Search Best Ensemble' option is enabled prior to running the analysis."))


    sidebarLayout(
           sidebarPanel(
                  div(class="map_control_style2",style="color: #05668D",
                      uiOutput(ns("side_download_table_pool")))
           ),
           mainPanel(
                  uiOutput(ns("Ensemble_tab1")))
    )
  })

  output$entab_3<-renderUI({
    # validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))
    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            uiOutput(ns("side_round_model")),
            uiOutput(ns('side_download_table_model')),
        )
      ),
      mainPanel(
        uiOutput(ns("Model_tab1")))
    )

  })

  output$entab_4<-renderUI({
    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            uiOutput(ns("side_create_preds")),
            uiOutput(ns("side_download_table_pred"))
        )
      ),
      mainPanel(
        uiOutput(ns("Ensemble_tab2")))
    )

  })

  output$entab_5<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))
    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            uiOutput(ns("side_palette")),
            uiOutput(ns("side_round_perf")),
            uiOutput(ns("cm_title")),
            uiOutput(ns("side_download_table_perf")),
            uiOutput(ns("side_downp_perf"))
        )
      ),
      mainPanel(
        uiOutput(ns("out_perf")))
    )

  })

  output$entab_6<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))

    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            uiOutput(ns("side_round_obserr")),
            uiOutput(ns("side_create_errors")),
            uiOutput(ns("side_download_table_obserr"))
        )
      ),
      mainPanel(
        uiOutput(ns("Ensemble_tab4")))
    )


  })

  output$entab_7<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))
     sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            uiOutput(ns("side_qclass")),

            uiOutput(ns("side_downp_plotpred"))
        )
      ),
      mainPanel(
        uiOutput(ns("Ensemble_tab5")))
    )


  })

  output$entab_8<-renderUI({
    uiOutput(ns("permutation_importance"))

  })

  output$entab_9<-renderUI({
    column(12,
           column(3,class="well3",
                  div(class="map_control_style",style="color: #05668D",
                      uiOutput(ns('side_interactions')),
                      uiOutput(ns('side_download_table_iter')),
                      uiOutput(ns('side_downp_iter'))

                  )
           ),
           column(9,

                  div(
                    inline(uiOutput(ns("inter_method"))),
                    inline(uiOutput(ns('run_inter')))
                  ),
                  uiOutput(ns("Ensemble_tab8")),
                  uiOutput(ns("Ensemble_tab8_plot"))

           ))
  })




  output$pool_selection_out<-renderUI({


    best_table<-get_best_table_pool()
    em<-get_the_model2()

    pool_df<-em$pool_metrics[[2]]
    acc<-input$optimal_ensemble
    models_grid<-em$pool_metrics[[1]]
    res<-pool_table<-data.frame(id=1:length(models_grid),len= sapply(models_grid,length),acc=pool_df[,acc])
    accs<-unique(round(sort(res$acc, decreasing=T),2))
    veclen<-sort(unique(res$len))
    vecacc<-unique(round(sort(res$acc, decreasing=T),2))

    m0<-sapply(split(res,res$len),function(x){
      table(factor(round(x$acc,2),levels=vecacc))
    })

    listall<-lapply(1:length(models_grid),function(x){
      list(models=models_grid[[x]],acc=round(pool_table[x,'acc'],2),len=pool_table[x,"len"])

    })
    m0<-cbind(ids=round(as.numeric(rownames(m0)),2),m0)
    colnames(m0)[1]<-""


    grid<-em$pool_metrics[[1]][res$id]
    reste<-lapply(1:nrow(m0), function(x){
      do.call(splitLayout,lapply(1:length(m0[x,]),function(xx){
        if(xx==1){
          div( m0[x,xx], style="border-right: 1px solid;background: Lavender")
        } else{
          pic<-which(sapply(listall,function(nx) nx$acc)==accs[x] & sapply(listall,function(nx) nx$len)==xx-1)
          li<-listall[sapply(listall,function(nx) nx$acc)==accs[x]]

          goptip<-"xxxxx"

          if(length(pic)>0){
            gridlist<-lapply(listall[pic],function(x) x$models)
            goptip<-paste0(do.call(HTML,lapply(1:length(gridlist),function(x) HTML(paste0(div(HTML(paste0(strong(pic[x]),">>",paste0(gridlist[[x]],collapse="+"),";"))))))))
          } else{
            goptip<-paste0(c(x,"_",xx))

          }

          if( m0[x,xx]==0){div("")} else{ popify(div( m0[x,xx], style="background: LightCyan"),NULL,goptip)}
        }

      }))

    })
    w2=1/ncol(m0)*(ncol(m0)-1)
    w1=1-w2
    cellWidths=c(
      paste0(round(w1*100,3),"%"),
      paste0(round(w2*100,3),"%")

    )
    column(12,style="font-size: 10px",
           splitLayout(div("-",style="border-right: 1px solid"),div("Lenght", align="center", style="background: LightYellow"), cellWidths = cellWidths, style="border-bottom: 1px solid"),

           div(do.call(splitLayout,lapply(1:ncol(m0),
                                          function(x) {
                                            if(x==1){
                                              div(input$optimal_ensemble,style="border-right: 1px solid black;background: Lavender")
                                            } else{
                                              div(colnames(m0)[x],style='background: LightYellow')
                                            }

                                          }
           )),style="border-bottom: 1px solid black"),
           reste,
           div(
             style="padding: 20px",
             div(inline(div(style="height: 20px;width: 40px; background: LightCyan")),span("Number of Ensembles. Hover to display the Ensembles"))

           )


    )
  })




  output$pool_filtered_list<-DT::renderDataTable(
    {

      req(input$round_poollist)
      best_table<-round(get_best_table_pool(),input$round_poollist)

      vals$filtered_pool_list<-table<-get_pool_metrics_df()[best_table$id,]
      # saveRDS(table,'table.rds')
      #table<-readRDS('table.rds')
      table[ ,which(sapply(table,is.numeric))]<-round(  table[ ,which(sapply(table,is.numeric))],input$round_poollist)



      data.frame(id=rownames(table),table)
    },
    extensions = c('FixedColumns',"FixedHeader"),
    options = list(
      pageLength = 15,
      info = FALSE,
      dom="lt",
      lengthMenu = list(c(15, -1), c( "15","All")),
      autoWidth=F,
      scrollX = TRUE,
      scrollY = "400px",
      fixedHeader=TRUE,
      fixedColumns = list(leftColumns = 2, rightColumns = 0)),
    rownames = F)


  observeEvent(ignoreInit = T,input$download_table_model,{
    vals$down_ensemble<-data.frame(get_predtab())
    vals$hand_down<- "ensemble_model_predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })

  observeEvent(ignoreInit = T,input$download_table_pred,{
    vals$down_ensemble<-data.frame(get_pred())
    vals$hand_down<- "ensemble_predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })


  observeEvent(ignoreInit = T,input$download_table_perf,{
    vals$down_ensemble<-data.frame(vals$ensemble_confusion)
    vals$hand_down<- "ensemble_confusion"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })

  observeEvent(ignoreInit = T,input$download_table_obserr,{
    vals$down_ensemble<-data.frame(vals$comb_down_errors_train)
    vals$hand_down<- "ensemble_obs_errors"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })

  output$side_download_table_pred<-renderUI({
    req(input$data_ensemble_X)
    div(
      tipify(
        actionLink(
          ns('download_table_pred'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
        ),
        "+ Download Table", options=list(container="body")
      ),
      hr()
    )
  })
  output$side_download_table_perf<-renderUI({
    req(input$data_ensemble_X)
    div(
      tipify(
        actionLink(
          ns('download_table_perf'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
        ),
        "+ Download Table", options=list(container="body")
      ),
      hr()
    )
  })
  output$side_downp_perf<-renderUI({

    div(
      tipify(
        actionLink(
          ns('downp_perf'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
        ),
        "+ Download plot", options=list(container="body")
      )
    )
  })
  output$side_downp_plotpred<-renderUI({

    div(
      tipify(
        actionLink(
          ns('downp_plotpred'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
        ),
        "+ Download plot", options=list(container="body")
      )
    )
  })

  output$side_download_table_model<-renderUI({
    div(
      tipify(
        actionLink(
          ns('download_table_model'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
        ),
        "+ Download Table", options=list(container="body")
      ),
      hr()
    )
  })
  observeEvent(ignoreInit = T,input$show_pool_results,{
    vals$show_pool_results<-input$show_pool_results
  })

  output$side_download_table_pool<-renderUI({
    div(
      numericInput(ns("round_performace_pool"),"Round",2),
      tipify(
        actionLink(
          ns('download_table_pool'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
        ),
        "+ Download Table", options=list(container="body")
      ),
      hr()
    )
  })
  output$side_download_table_iter<-renderUI({
    req(!is.null(get_inter_res0()))
    req(input$inter_show=='Results')

    req(input$data_ensemble_X)
    div(
      tipify(
        actionLink(
          ns('download_table_iter'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
        ),
        "+ Download Table", options=list(container="body")
      ),
      hr()
    )
  })
  output$side_downp_iter<-renderUI({
    req(input$inter_show=='Plot')
    div(
      tipify(
        actionLink(
          ns('downp_iter'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
        ),
        "+ Download plot", options=list(container="body")
      )
    )
  })


  output$side_download_table_obserr<-renderUI({
    req(input$data_ensemble_X)
    div(
      tipify(
        actionLink(
          ns('download_table_obserr'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
        ),
        "+ Download Table", options=list(container="body")
      ),
      hr()
    )
  })





  observeEvent(ignoreInit = T,input$downp_iter,{
    vals$plot_ensemble<-vals$intercomb_plot
    vals$hand_plot<-"ensemble_interactions_plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })

  observeEvent(ignoreInit = T,input$downp_perf,{
    vals$plot_ensemble<-vals$combcm
    vals$hand_plot<-"ensemble_confusion_plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })

  observeEvent(ignoreInit = T,input$downp_plotpred,{
    em<-get_the_model2()
    modelist=attr(em,"modelist")


    vals$plot_ensemble<-vals$vartrees
    vals$hand_plot<-
      if(modelist[[1]]$modelType=="Classification"){
        "ensemble_predictions_class_plot"
      } else{
        "ensemble_predictions_reg_plot"
      }
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })

  output$side_round_model<-renderUI({
    div(

      inline(numericInput(ns('round_summ_model'),  "+ Round",3))
    )
  })
  output$side_round_plotpred<-renderUI({
    div(

      inline(numericInput(ns('round_summ_plotpred'), "+ Round",3))
    )
  })


  output$side_round_perf<-renderUI({
    numericInput(ns('round_summ_perf'),"+ Round",3)
  })


  output$side_round_obserr<-renderUI({
    numericInput(ns('round_summ_obserr'),"+ Round",3)
  })

  ##
  output$permutation_importance<-renderUI({
    div(class="well3",style='choosechannel',
        uiOutput(ns('header_feature')),
        tabsetPanel(id=ns("pi_result"),

                    tabPanel("4.1. Feature Importance",value="tab_pred",
                             div(
                               sidebarLayout(
                                 sidebarPanel(
                                   uiOutput(ns("feature_importance_side"))
                                 ),
                                 mainPanel(  uiOutput(ns("feature_importance_plot")))
                               )
                             )

                    ),
                    tabPanel("4.2. Confusion Matrix Post-Shuffling",value="tab2",
                             uiOutput(ns('feature_cm'))
                    ),
                    # tabPanel("Importance by class",value="tab3", uiOutput(ns('feature_class'))),
                    tabPanel("4.3. Results",value="tab4",
                             div(
                               actionLink(
                                 ns('ensemble_varImp'),span("+ Download table"),style="button_active"
                               )
                             ),
                             DT::dataTableOutput(ns('feature_results'))
                    )
        )
    )
  })
  observeEvent(ignoreInit = T,input$featens_help,{
    showModal(
      modalDialog(
        title="Permutation Feature Importance",
        size ="l",
        easyClose = T,
        fluidPage(

          tags$div(
            tags$p('the following steps are used by iMESc to perform feature importance permutation:'),
            # Step 1

            tags$h4("Step 1: Hypothesis Definition"),
            tags$ul(
              tags$li("Null hypothesis (H0): The feature has no importance or predictive power."),
              tags$li("Alternative hypothesis (H1): The feature has importance or predictive power.")
            ),

            # Step 2
            tags$h4("Step 2: Observed Metric"),
            tags$p("The observed metric is the performance metric used to evaluate the feature's importance"),
            tags$ul(
              tags$li("For classification: Accuracy"),
              tags$li("For regression: R-squared")
            ),

            # Step 3
            tags$h4("Step 3: Permutation Process"),
            tags$p("The values of the feature are randomly shuffled while keeping other variables unchanged. This process disrupts the relationship between the feature and the target variable."),

            # Step 4
            tags$h4("Step 4: Null Distribution Creation"),
            tags$p("The permutation process is repeated a number of times (as specified by the user) to create a null distribution. Each repetition yields a permuted metric."),

            # Step 5
            tags$h4("Step 5: Comparison with Null Distribution"),
            tags$p("The observed metric are compared to the null distribution. It determines where the observed metric falls within the null distribution. If the observed metric is extreme compared to the null distribution, it suggests evidence against the null hypothesis."),

            # Step 6
            tags$h4("Step 6: Calculation of P-value"),
            tags$p("The proportion of permuted metrics that are as extreme or more extreme than the observed metric is calculated. This proportion represents the p-value, which indicates the statistical significance of the feature's importance"),

            # Step 7
            tags$h4("Step 7: User Decision"),
            tags$p("Based on the calculated p-value and a predetermined significance level (e.g., 0.05), the user can make a decision whether to reject or fail to reject the null hypothesis.")
          )

        )
      )
    )
  })
  output$header_feature<-renderUI({
    div(
      h4("Permutation importance", tipify(actionLink(ns("featens_help"),icon("fas fa-question-circle")),"Click for more details")),
      actionButton(ns("run_featens_shuf"),"RUN"),
      inline(div(
        span("+ Iterations:",tiphelp("Number of iterations for randomizing each variable"),
             inline(
               numericInput(ns("featens_rep"),NULL, value=2, width="75px", step=1)
             )
        )
      )),
      inline(div(
        span("+ Seed:",tiphelp("seed for genereating reproducible results"),
             inline(
               numericInput(ns("featens_seed"),NULL, value=1, width="75px", step=1)
             )
        )
      ))
    )
  })

  observeEvent(ignoreInit = T,input$down_ensemble_tab3_1,{
    vals$hand_down<-"ensemble - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  output$feature_importance_side<-renderUI({
    div(class="map_control_style",style="color: #05668D",

        uiOutput(ns('side_filter_featens_plot_nvars')),
        uiOutput(ns('featens_side_palette')),
        uiOutput(ns('side_featens_axes_plot')),

        div(
          actionLink(
            ns('ensemble_varImp_plot'),span('+ Download plot'),style="button_active"
          )
        )

    )
  })
  output$featens_side_palette<-renderUI({
    div(
      div(
        span("+ Palette",
             inline(
               pickerInput(inputId = ns("featens_pal_feature"),
                           label = NULL,
                           choices =     vals$colors_img$val,
                           choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
             )
        )
      )
    )
  })
  output$side_featens_axes_plot<-renderUI({
    div(
      div("+ Axes size",inline(numericInput(ns("featens_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("featens_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("featens_cex.leg"),NULL, value=13, width="75px", step=1))),


    )
  })
  output$side_filter_featens_plot_nvars<-renderUI({


    div( div(
      span("+ Number of variables:",tiphelp("Filter variables with the highest mean importance"),
           inline(
             numericInput(ns("featens_npic"),NULL, value=20, width="75px", step=1)
           )
      )
    ),
    div(
      span("+ Significance level",
           inline(
             numericInput(ns("sig_feature"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    ))
  })
  observeEvent(ignoreInit = T,input$ensemble_varImp_plot,{
    vals$hand_plot<-"ensemble - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })


  output$feature_cm<-renderUI({
    em<-get_the_model2()
    newdata<-em$newdata

    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style",style="color: #05668D",
            div(
              inline(h4("Confusion Matrix Post-Shuffling")),
            ),
            div("+ Variable:",

                pickerInput(inputId = ns("var_feature_cm"),
                            label = NULL,
                            choices = colnames(newdata)    ,selected=vals$var_feature_cm, options=list(container="body"), width='200px')

            ),
            div("+ Palette:",
                inline(
                  pickerInput(inputId = ns("featens_pal_feature_cm"),
                              label = NULL,
                              choices =     vals$colors_img$val,
                              choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
                )
            ),
            div(
              actionLink(
                ns('downp_feature_cm'),span('+ Download plot'),style="button_active"
              )
            )


        )
      ),

      mainPanel(uiOutput(ns("feature_cm_main")))
    )
  })
  observeEvent(ignoreInit = T,input$downp_feature_cm,{
    vals$hand_plot<-"Confusion Matrix Post-Shuffling"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals, file=input$var_feature_cm)
  })
  get_cm_impact<-function(predtable,var){
    req(is.data.frame(predtable))
    req(nrow(predtable)>1)

    rands_lis<-split(predtable,predtable$var)
    res<-rands_lis[[var]]
    dft<-table(res$pred,res$obs)
    dft
  }
  output$feature_cm_main<-renderUI({

    div(

      uiOutput(ns("feature_cm_plot")),

    )
  })
  observeEvent(ignoreInit = T,input$var_feature_cm,{
    vals$var_feature_cm<-input$var_feature_cm
  })
  output$feature_cm_plot<-renderUI({
    model_list<- vals$saved_ensemble[[input$ensemble_models]]
    req(length(model_list)>0)
    predtable<-model_list$feature_rands
    dft<-get_cm_impact(predtable, var=input$var_feature_cm)

    renderPlot({
      vals$feature_cm_plot<- plotCM(dft, palette=input$featens_pal_feature_cm,vals$newcolhabs, font_color="black", title="", round_cm=3)
      vals$feature_cm_plot
    })
  })
  getmetric<-reactive({
    em<-get_the_model2()
    m<-attr(em,"modelist")[[1]]
    metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
    metric
  })
  get_sumamry_permute_importance<-reactive({
    req(input$sig_feature)
    model_list<- vals$saved_ensemble[[input$ensemble_models]]
    sig_level<-input$sig_feature
    req(length(model_list)>0)
    df<-model_list$feature_rands
    m<-model_list[[1]]
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    df<-permimp_metric_ensemble(df,metric=getmetric())

    req(length(df)>0)
    req(is.data.frame(df))

    vals$ensemble_varImp<-perm_summary<-sig_feature_ensemble(df,em,sig_level)
    perm_summary
  })
  output$feature_results<-DT::renderDataTable({
    table<-get_sumamry_permute_importance()
    DT::datatable(table,options=list(pageLength = 20,info = FALSE,lengthMenu = list(c(20,-1),c( "20","All")),autoWidth=T,dom = 'lt'),rownames = TRUE,class ='compact cell-border')

  })



  output$featens_side_palette<-renderUI({
    div(
      div(
        span("+ Palette",
             inline(
               pickerInput(inputId = ns("featens_pal_feature"),
                           label = NULL,
                           choices =     vals$colors_img$val,
                           choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
             )
        )
      )
    )
  })
  observeEvent(ignoreInit = T,input$run_featens_shuf,{
    #cl <- makeCluster(cores[1]-1)
    # registerDoParallel(cl)
    library(doParallel)
    library(foreach)
    cores=detectCores()
    cl <- makeCluster(cores[1]-1)
    registerDoSNOW(cl)
    em<-get_the_model2()
    # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(progress$close())
    # progress <- shiny::Progress$new()
    #progress <- progress$set(message = "Making plot", value = 0)
    rep=input$featens_rep
    seed=input$featens_seed
    if(is.na(seed)){seed=NULL}
    predtable<- permute_importance_foreach_ensemble(em, rep, seed=seed,show_progress=T)


    #res<-sig_feature(lis, metric, values)


    stopCluster(cl)
    unregister_dopar()


    vals$saved_ensemble[[input$ensemble_models]]$feature_rands<-predtable




  })
  output$feature_importance_plot<-renderUI({
    model_list<- vals$saved_ensemble[[input$ensemble_models]]
    req(input$featens_npic)
    sig_level<-input$sig_feature

    req(length(model_list)>0)
    df<-model_list$feature_rands
    em<-get_the_model2()
    newdata<-em$newdata
    predtab<-em$tabpred
    modelist=attr(em,"modelist")
    obc=em$obc
    pred=em$pred



    df<-permimp_metric_ensemble(df,metric=getmetric())

    req(length(df)>0)
    req(is.data.frame(df))

    rands<-df
    perm_summary<-sig_feature_ensemble(df,em,sig_level)
    sig_names<-rownames(perm_summary)[perm_summary$sig=="*"]
    df<-get_feature_data_plot_ensemble(df,em, npic=input$featens_npic)
    col<-vals$newcolhabs[[input$featens_pal_feature]](2)
    req(nrow(df)>1)
    df$sig<-"non_sig"
    df$sig[    df$var%in%sig_names    ]<-"sig"
    df$var[ which( df$var%in%sig_names)]<-paste0("*",df$var[  which( df$var%in%sig_names)])
    req(length(df)>0)
    renderPlot({
      df<-df

      p<-ggplot(df, aes(reorder(var,Metric), Metric)) +
        geom_boxplot(aes(fill=sig))+coord_flip()+labs(x="Variables", y='Performance decay')+ggtitle("Feature Shuffling Impact")+theme(
          axis.text=element_text(size=input$featens_cex.axes),
          axis.title=element_text(size=input$featens_cex.lab)
        )+scale_fill_manual(values=col)

      vals$ensemble_varImp_plot<-p
      vals$ensemble_varImp_plot

    })
  })

  ##


  output$Ensemble_tab1<-renderUI({
    em<-get_the_model2()
    validate(need(sum(!sapply(em$pool_metrics,is.null))==2,"Results are not accessible unless the 'Search Best Ensemble' option is enabled prior to running the analysis."))


    inital_pool<-em$inital_pool
    optimal_ensemble<-names(attr(em,"modelist"))
    en_method= em$en_method_label
    weights_from=em$weights_from
    weis<-attr(em$tabpred,"weis")
    column(12,style="margin-top: 10px",
           div(
             div("Initial pool",em(paste0(inital_pool,collapse="; "))),
             div("Selected ensemble",paste0(optimal_ensemble,collapse=" + "))
           ),
           div(style="min-height: 100px; background: white",
               DT::dataTableOutput(ns('pool_result'))

           ),
           #div(uiOutput(ns('rda_pool')))
    )
  })

  output$rda_plot<-renderUI({
    req(input$rda_pic_metric)

    renderPlot({
      metric<-input$optimal_ensemble
      em<-get_the_model2()
      models<-data.frame(t(sapply(em$pool_metrics[[1]], function(x)
        table(factor(x, levels=em$pool_metrics[[1]][[length(em$pool_metrics[[1]])]])))))

      data<-get_pool_metrics_df()[-1]
      cuts<-cut(data[,metric],6)


      prda(vegan::rda(X=data[-1][input$rda_pic_metric],Y=models,scale=F), newcolhabs = vals$newcolhabs, key=cuts,palette="turbo",scaling=2, title=metric)


    })

  })


  output$rda_pool<-renderUI({
    em<-get_the_model2()
    div(
      pickerInput(ns("rda_pic_metric"),"Metrics for the RDA", colnames(em$pool_metrics[[2]]),selected= colnames(em$pool_metrics[[2]]), multiple = T),
      uiOutput(ns('rda_plot')))
  })

  show_pool_results<-reactive({
    em<-get_the_model2()
    req(!is.null(em$pool_metrics))
    best_table<-get_best_table_pool()
    switch(input$show_pool_results,
           'All'={
             vals$filter_pool<-'top'
             get_pool_metrics_df()
           },
           'Filtered list'={
             vals$filter_pool<-'none'
             get_pool_metrics_df()[best_table$id,]
           },
           'Selected ensemble'={
             vals$filter_pool<-'none'
             get_pool_metrics_df()[ensemble_selected(),]
           }


    )

  })



  output$pool_result<-DT::renderDataTable(
    {
      metrics<-get_pool_metrics_df()
      metrics[2:ncol(metrics)]<-round(metrics[2:ncol(metrics)],input$round_performace_pool)
      df<-data.frame(id=rownames(metrics),metrics)
      df
      },
    extensions = c('FixedColumns',"FixedHeader"),
    options = list(
      pageLength = 15,
      info = FALSE,
      lengthMenu = list(c(15, -1), c( "15","All")),
      dom="lt",
      autoWidth=F,
      scrollX = TRUE,
      scrollY = "600px",
      fixedHeader=TRUE,
      fixedColumns = list(leftColumns = 1, rightColumns = 0)),
    filter="top",
    rownames = F)

  observeEvent(ignoreInit = T,input$download_table_pool,{
    vals$down_ensemble_pool_metrics<-data.frame(get_pool_metrics_df())
    vals$hand_down<- "ensemble_pool_predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  get_pool_metrics_df<-reactive({
    em<-get_the_model2()
    obc=em$obc
    req(length(em$pool_metrics)==2)

    df<-data.frame(
      pool=unlist(lapply(em$pool_metrics[[1]],function(x) paste(x, collapse="; "))),
      length=sapply(em$pool_metrics[[1]],function(x) length(x)),
      em$pool_metrics[[2]]

    )
    rownames(df)<-1:nrow(df)
    df
  })






  observeEvent(ignoreInit = T,input$run_inter,{

    withProgress(min=0,max=3,message="Running...",{


      session=getDefaultReactiveDomain()
      em<-get_the_model2()
      predtab<-em$tabpred
      modelist<-attr(em,"modelist")
      newdata<-em$newdata



      weis<-attr(predtab,'weis')
      obc<-em$obc
      imp<-varImp(modelist[[1]], useModel=F, scale=F)$importance
      timp<-data.frame(imp)
      w<-colSums(timp)/sum(colSums(timp))
      re<-apply(timp,1,function(x) weighted.mean(x,w=w))
      tops<-names(sort(re,decreasing=T))
      library(doParallel)
      library(foreach)
      cores=detectCores()
      cl <- makeCluster(cores[1]-1)
      registerDoSNOW(cl)

      seed=NULL
      if(!is.null(seed)){
        if(is.na(seed)){seed=NULL}
      }
      show_progress=F
      show_progress=T
      grid<-getinter_grid(input$inter_method,em$newdata,tops, input$inter_root)
      grid<-if(is.data.frame(grid)){  as.list(data.frame(  t(grid)))}
      grid_root<-data.frame(var=unique(sapply(grid,function(x) x[1])))
      grid_root_list<-lapply(as.list(t(grid_root)),function(t) data.frame(root=t))


      incProgress(1,message="Calculating Root...")
      rands_root<-data.frame(do.call(rbind,lapply(grid_root_list,function(x){
        res<-data.frame(interact_ensemble_foreach(em, input$inter_method,x,rep=input$inter_rep, seed=seed, show_progress=show_progress,session=session))

        res$inter<-unlist(x)
        res
      })))
      incProgress(1,message="Calculating Interactions")
      rands_inter<-interact_ensemble_foreach(em, input$inter_method,grid,rep=input$inter_rep, seed=seed, show_progress=show_progress,session=session)


      stopCluster(cl)


      error<-try({

        metric_tabs<-get_inter_metrics_tabs(rands_root,rands_inter, grid,metric="Accuracy", obc)
        result<-t.test_inter_ensemble(metric_tabs)
        result_df<-rand_inter_ensemble(metric_tabs)
        vals$saved_ensemble[[input$ensemble_models]]$result_inter1<-result
        vals$saved_ensemble[[input$ensemble_models]]$result_inter2<-result_df

      })


      updateTabsetPanel(session,"ensemble_tab","tab_perf")
      updateTabsetPanel(session,"Ensemble_tab3",vals$Ensemble_tab3)




    })
  })
  output$interactions_plot<-renderUI({


    npic<-input$npic_inter
    result=vals$saved_ensemble[[input$ensemble_models]]$result_inter1
    req(length(result)>0)
    result_df=vals$saved_ensemble[[input$ensemble_models]]$result_inter2



    req(length(result_df)>0)
    df<-result
    col<-vals$newcolhabs[[input$pal_inter]](2)
    req(nrow(df)>1)
    df0<-result_df
    df0$sig<-df$sig<-"non_sig"
    df0$var<-df$var<-rownames(df)

    df0$sig[  which(result$p_value<input$inter_sig)   ]<-"sig"
    result$p_value[is.na(result$p_value)]<-1
    df0$var[ which(result$p_value<input$inter_sig)]<-paste0("*",df0$var[result$p_value<input$inter_sig])


    df<-reshape2::melt(df0,c("var","sig"))

    req(length(df)>0)

    df<-df
    df$sig<-factor(df$sig, levels=c("sig","non_sig"))
    picvar<-names(sort(tapply(df$value,df$var, mean), decreasing=T)[1:npic])
    df2<-df[ which(df$var%in%picvar),]


    p<-ggplot(df2, aes(reorder(var,value), value)) +
      geom_boxplot(aes(fill=sig))+coord_flip()+labs(x="Variables", y='Performace Difference (Root Variable vs. Both)')+ggtitle("Interaction Shuffling Impact")+theme(
        axis.text=element_text(size=input$inter_cex.axes),
        axis.title=element_text(size=input$inter_cex.lab)
      )+scale_fill_manual(values=col)

    vals$intercomb_plot<-p
    renderPlot({

      vals$intercomb_plot


    })
  })


  output$sig_forward<-renderUI({
    renderPrint({
      req(input$data_ensemble_X)
      getsigs2_loss(get_inter_res0(),sig=input$inter_sig)

    })
  })

  output$Ensemble_tab8_plot<-renderUI({
    req(input$inter_show)
    if(input$inter_show=="Plot"){
      uiOutput(ns('interactions_plot'))
    } else{ uiOutput(ns("interactions_result")) }
  })


  output$pool_selection_side<-renderUI({
    em<-get_the_model2()
    req(sum(!sapply(em$pool_metrics,is.null))==2)
    lab1<-span("Metric for the filtered pool list",pophelp(NULL,"Select the metric to rank the model combination with fewest models"))

    div(
      div(
          pickerInput(ns("optimal_ensemble"), lab1,choices=colnames(get_pool_metrics()), selected=vals$optimal_ensemble)),
      uiOutput(ns("pool_result_side")),
      uiOutput(ns("pool_save_optimal_ensemble")),

    )
  })

  output$pool_save_optimal_ensemble<-renderUI({
    req(input$ensemble_models)
    if(input$ensemble_models=="New Ensemble"){
    span(icon("fas fa-lightbulb"),"First, save the 'Ensemble List' so you can then save the 'Optimal Ensemble' and make predictions based on it.",style="color: SeaGreen")
    }else{
      div(class='button_min',
          span(
            inline(div(class='save_changes',
                       tipify(actionButton(ns("save_selected_ensemble"), icon("fas fa-save")), "Save the selected ensemble model for result recovery and predicting unknown data","right")

            )),icon("fas fa-hand-point-left"),"Save the Optimal Ensemble"
          )
      )
  }

  })

  get_best_table_pool<-reactive({
    em<-get_the_model2()
    req(!is.null(em$pool_metrics))
    optimal_ensemble<-names(attr(em,"modelist"))
    pool_df<-em$pool_metrics[[2]]
    req(input$optimal_ensemble)
    acc<-input$optimal_ensemble
    req(input$optimal_ensemble%in%colnames(pool_df))
    models_grid<-em$pool_metrics[[1]]
    pool_table<-data.frame(id=1:length(models_grid),len= sapply(models_grid,length),acc=pool_df[,acc])
    best_table<-pool_table[pool_table$acc==max(pool_table$acc),]
    best_model<-best_table[best_table$len==min(best_table$len),"id"]
    best_table
  })



  output$pool_result_side<-renderUI({
    em<-get_the_model2()
    selected=getfrom_ensemble_from(em,input$ensemble_models,vals)

    div(
      radioButtons(
        ns("ensemble_select_from"),   # Namespace prefix for the input ID
        "Select Ensemble from:",      # Label for the radio buttons
        choiceNames = list(           # Display names for each choice
          div(span("Filtered pool list:",pophelp(NULL, "Choose this option to select the Ensemble ID from the filtered pool list.")), inline(uiOutput(ns('en_selfrom1')))),
          div(span("Ensemble ID:",pophelp(NULL, "Choose this option to enable a textbox for entering the Ensemble ID.")), inline(uiOutput(ns('en_selfrom_id')))),
          div(span("Custom search:",pophelp(NULL, "Choose this option to individually select models from the suspend list and select the corresponding ensemble.")), inline(uiOutput(ns('en_selfrom2'))))
        ),
        choiceValues = list("list", "id", "custom"),   # Values associated with each choice
        selected = selected   # Initially selected choice
      )

    )


  })

  observeEvent(ignoreInit = T,input$ensemble_select_from,{
    vals$ensemble_select_from<-input$ensemble_select_from
  })


  observe({
    req(input$Ensemble_tab3)
    req(input$optimal_ensemble)
    req(input$ensemble_select_from=="list")
    req(input$ensemble_select_from_list)
    req(input$ensemble_models)
    shinyjs::show('en_selfrom1')
    shinyjs::hide('en_selfrom_id')
    shinyjs::hide('en_selfrom2')

  })



  observe({
    req(input$Ensemble_tab3)
    req(input$optimal_ensemble)
    req(input$ensemble_select_from=="id")
    req(input$ensemble_select_from_id)
    req(input$ensemble_models)
    shinyjs::hide('en_selfrom1')
    shinyjs::show('en_selfrom_id')
    shinyjs::hide('en_selfrom2')

  })
  observe({
    req(input$Ensemble_tab3)
    req(input$optimal_ensemble)
    req(input$ensemble_models)
    req(input$ensemble_select_from=="custom")
    req(input$ensemble_select_from_custom)

    shinyjs::hide('en_selfrom1')
    shinyjs::hide('en_selfrom_id')
    shinyjs::show('en_selfrom2')


  })






  getfrom_ensemble_from<-function(em,ensemble_models,vals){
    if(ensemble_models!="New Ensemble"){
      if(length(em)>0){
        listargs<-   em$args_selected_model
        if(length(listargs)>0){
          return(listargs$ensemble_select_from)
        } else{
          return( vals$ensemble_select_from)
        }
      }
    } else{
      return(vals$ensemble_select_from)
    }
  }
  getfrom_ensemble_from_list<-function(em,ensemble_models,vals){
    if(ensemble_models!="New Ensemble"){
      if(length(em)>0){
        listargs<-   em$args_selected_model
        if(length(listargs)>0){
          listargs$ensemble_select_from_list
        } else{
          vals$ensemble_select_from_list
        }
      }
    } else{
      vals$ensemble_select_from_list
    }
  }
  getfrom_ensemble_from_id<-function(em,ensemble_models,vals){
    if(ensemble_models!="New Ensemble"){
      if(length(em)>0){
        idargs<-   em$args_selected_model
        if(length(idargs)>0){
          idargs$ensemble_select_from_id
        } else{
          vals$ensemble_select_from_id
        }
      }
    } else{
      vals$ensemble_select_from_id
    }
  }
  getfrom_ensemble_from_custom<-function(em,ensemble_models,vals){
    if(ensemble_models!="New Ensemble"){
      if(length(em)>0){
        customargs<-   em$args_selected_model
        if(length(customargs)>0){
          customargs$ensemble_select_from_custom
        } else{
          vals$ensemble_select_from_custom
        }
      }
    } else{
      vals$ensemble_select_from_custom
    }
  }

  GET_best_table_pool<-function(em){
    req(!is.null(em$pool_metrics))
    optimal_ensemble<-names(attr(em,"modelist"))
    pool_df<-em$pool_metrics[[2]]
    req(input$optimal_ensemble)
    acc<-input$optimal_ensemble
    req(input$optimal_ensemble%in%colnames(pool_df))
    models_grid<-em$pool_metrics[[1]]
    pool_table<-data.frame(id=1:length(models_grid),len= sapply(models_grid,length),acc=pool_df[,acc])
    best_table<-pool_table[pool_table$acc==max(pool_table$acc),]
    best_model<-best_table[best_table$len==min(best_table$len),"id"]
    best_table
  }



  output$en_selfrom1<-renderUI({
    if(is.null(vals$ensemble_select_from_id)){
      best_table<-get_best_table_pool()
      vals$ensemble_select_from_list<-best_table$id[1]
    }

    req(input$ensemble_select_from)
    style=ifelse(input$ensemble_select_from=="list","","display: none")

    em<-get_the_model2()
    req(!is.null(em$pool_metrics))
    best_table<-GET_best_table_pool(em)

    div(style=style,

        pickerInput(ns("ensemble_select_from_list"),NULL,best_table$id, selected=vals$ensemble_select_from_list)
    )
  })

  output$en_selfrom_id<-renderUI({
    if(is.null(vals$ensemble_select_from_id)){
      best_table<-get_best_table_pool()
      vals$ensemble_select_from_id<-best_table$id[1]
    }

    req(input$ensemble_select_from)
    em<-get_the_model2()
    selected=getfrom_ensemble_from_id(em,input$ensemble_models,vals)

    style=ifelse(input$ensemble_select_from=="id","","display: none")

    div(style=style,

        numericInput(ns("ensemble_select_from_id"),NULL,value=vals$ensemble_select_from_id)
    )
  })

  output$en_selfrom2<-renderUI({
    req(input$ensemble_select_from)
    em<-get_the_model2()
    if(is.null(vals$ensemble_select_from_custom)){
      top_ensemble<-GET_best_table_pool(em)$id[1]
      selected=em$pool_metrics[[1]][[top_ensemble]]
      vals$ensemble_select_from_custom<-selected
    }

    style=ifelse(input$ensemble_select_from=="custom","","display: none")

    req(!is.null(em$pool_metrics))

    choices=em$pool_metrics[[1]][[length(em$pool_metrics[[1]])]]

    if(!any(vals$ensemble_select_from_custom%in%choices)){
      top_ensemble<-GET_best_table_pool(em)$id[1]
      selected=em$pool_metrics[[1]][[top_ensemble]]
      vals$ensemble_select_from_custom<-selected
    }

    selected=getfrom_ensemble_from_custom(em,input$ensemble_models,vals)
    div(style=style,class="max_width",
        pickerInput(ns("ensemble_select_from_custom"),NULL,choices,multiple =T, selected=vals$ensemble_select_from_custom, width='150px'),
        ##uiOutput(ns("ensemble_select_id"))
    )

  })

  observeEvent(ignoreInit = T,input$ensemble_select_from_id,{
    req(input$ensemble_select_from)
    vals$ensemble_select_from_id<-input$ensemble_select_from_id
  })


  observeEvent(ignoreInit = T,input$ensemble_select_from_list,{
    req(input$ensemble_select_from)
    vals$ensemble_select_from_list<-input$ensemble_select_from_list
  })


  observeEvent(ignoreInit = T,input$ensemble_select_from_custom,{
    req(input$ensemble_select_from)
    vals$ensemble_select_from_custom<-input$ensemble_select_from_custom
  })



  output$ensemble_select_id<-renderUI({
    div(
      "Selected ensemble:", strong("ID:"),pick_ensemble_from_custom()
    )
  })

  pick_ensemble_from_custom<-reactive({
    req(input$ensemble_select_from_custom)
    em<-get_the_model2()
    req(sum(!sapply(em$pool_metrics,is.null))==2)

    models_grid<-  em$pool_metrics[[1]]
    which(sapply(models_grid,function(x){
      identical(sort(x),sort(input$ensemble_select_from_custom))
    }))
  })


  ensemble_selected<-reactive({
    req(input$ensemble_select_from)
    switch(
      input$ensemble_select_from,
      "custom"={
        pick_ensemble_from_custom()
      },
      'list'={
        req(input$ensemble_select_from_list)
        input$ensemble_select_from_list
      },
      "id"={
        req(input$ensemble_select_from_id)
        input$ensemble_select_from_id
      }
    )
  })





  get_selected_ensemble<-reactive({
    em<-get_the_model2()
    models_grid<-  em$pool_metrics[[1]]
    req(!is.null(models_grid))
    predtab<-em$predtab0
    weis<-attr(predtab,"wei")
    best_model<-as.numeric(ensemble_selected())
    req(length(best_model)>0)
    names(models_grid)<-1:length(models_grid)


    req(best_model%in%names(models_grid))
    best_model_name<-models_grid[[best_model]]
    req(length(best_model_name)>0)
    best_model_name
  })



  observeEvent(ignoreInit = T,input$optimal_ensemble,{
    vals$optimal_ensemble<-input$optimal_ensemble
  })





  get_pool_metrics<-reactive({
    em<-get_the_model2()
    req(length(em$pool_metrics)>0)
    req(length(em$pool_metrics[[2]])>0)
    em$pool_metrics[[2]]
  })





  output$search_lenghgt_out<-renderUI({
    em<-get_the_model2()
    req(length(em$pool_metrics)>0)
    req(length(em$pool_metrics[[2]])>0)

    div(style="font-size: 12px",
      div(strong("Search length:"), em(span(nrow(get_pool_metrics()),style="color: SeaGreen"))),
      div(strong("Selected ensemble:"), em(span(em$best_model_id,style="color: SeaGreen")))
    )
  })

  output$weis_out<-renderUI({
    req(input$ensemble_pages=='page3')
    em<-get_the_model2()
    req(is.data.frame(get_predtab()))

    req(!is.null(get_predtab()))
    column(12,
      splitLayout(
        uiOutput(ns("search_lenghgt_out")),

        div(
          div(strong("weights",style="font-size: 12px",)),
          #inline(strong("Weights results:")),
          div(style="font-size: 11px;width: 200px; overflow-y: scroll; overflow-x: scroll; background: white; height: 120px",id="weistab",
              DT::dataTableOutput(ns("weis_table"))
          )

        ))
    )
  })


  output$weis_table<-DT::renderDataTable({
    req(length(attr(get_predtab(),'weis'))>0)
    data.frame(weights=round(attr(get_predtab(),'weis'),2))
  },
  class="cell-border compact stripe",
  options=list(ordering=FALSE,scrollX = FALSE,deferRender = TRUE,dom = 't',
               columnDefs = list(list(className = 'dt-center', targets = "_all")),
               fixedColumns = list(leftColumns = 1))
  ,rownames=T, colnames=''




  )
  output$weis_df<-DT::renderDataTable({},options = list(info = FALSE, autoWidth=T,dom = 't'), rownames = F)


  output$Ensemble_tab8<-renderUI({
    req(input$data_ensemble_X)
    # req(!is.null(get_inter_res0()))
    div(strong("+ Show:"),
        inline(radioButtons(ns('inter_show'),NULL,c('Plot', 'Results'), inline=T))
    )
  })


  observeEvent(ignoreInit = T,input$help_weig2,{
    showModal(
      weig_help()
    )
  })
  weig_help<-reactive({
    modalDialog(
      title='Weights for ensembling models',
      easyClose =T,
      size="l",
      div(uiOutput(ns("votes_help")),
          uiOutput(ns("wmean_help")))
    )
  })
  observeEvent(ignoreInit = T,input$help_weig,{
    showModal(
      weig_help()
    )
  })





  output$interactions_result<-renderUI({
    div(style="overflow-x: scroll",
        div(
          "Table"
        ),
        inline(DT::dataTableOutput(ns('interactions_df')))

        # inline(DT::dataTableOutput(ns('interactions_df')))

    )
  })

  output$interactions_df<-DT::renderDataTable({
    req(length(vals$saved_ensemble[[input$ensemble_models]]$result_inter1)>0)
    data.frame(vals$saved_ensemble[[input$ensemble_models]]$result_inter1)

  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE)


  observeEvent(ignoreInit = T,input$download_table_iter,{
    vals$down_ensemble_inter_table<- data.frame(vals$saved_ensemble[[input$ensemble_models]]$result_inter1)

    vals$hand_down<- "ensemble_inter_table"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })



  output$inter_root<-renderUI({
    div(class="cogs_in_div",style="margin-bottom: 10px; padding:5px",
        fluidPage(style="max-height: 250px;overflow-y: scroll",
                  actionLink(ns("show_interroot"),"+ Root"),
                  uiOutput(ns('out_interroot'))


        ))

  })
  observeEvent(ignoreInit = T,input$show_interroot,{
    shinyjs::toggle("out_interroot")
  })



  output$out_interroot<-renderUI({

    em<-get_the_model2()
    predtab<-em$tabpred
    modelist<-attr(em,"modelist")
    newdata<-em$newdata
    weis<-attr(predtab,'weis')
    obc<-em$obc
    imp<-varImp(modelist[[1]], useModel=F, scale=T)$importance
    timp<-data.frame(imp)
    w<-colSums(timp)/sum(colSums(timp))
    re<-apply(timp,1,function(x) weighted.mean(x,w=w))
    tops<-names(sort(re,decreasing=T))


    checkboxGroupInput(ns("inter_root"),NULL,na.omit(tops), selected=tops[1])
  })



  output$inter_sig<-renderUI({
    div("+ Significance level",inline(numericInput(ns("inter_sig"),NULL, value=0.05, width="75px", step=0.02))
    )
  })
  output$run_inter<-renderUI({

    div(class="save_changes",actionButton(ns("run_inter"),"+ Calculate Interactions"))
  })

  observeEvent(ignoreInit = T,input$inter_rep,{
    vals$inter_rep<-input$inter_rep
  })

  output$inter_rep<-renderUI({
    if(is.null(vals$inter_rep)){vals$inter_rep<-5}
    div(
      span("+ Repetions:",
           inline(
             numericInput(ns("inter_rep"),NULL, value=vals$inter_rep, width="75px", step=2, min=2)
           )
      )
    )
  })
  output$inter_ggplot<-renderUI({
    div(
      div("+ Title size",inline(numericInput(ns("inter_cex.main"),NULL, value=15, width="75px", step=1))),
      div("+ Axes size",inline(numericInput(ns("inter_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("inter_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("inter_cex.leg"),NULL, value=13, width="75px", step=1))),
      div("+ Xlab",inline(textInput(ns("inter_ylab"), NULL, 'Importance', width="200px")),
          div("+ Ylab",inline(textInput(ns("inter_xlab"), NULL, 'Variables', width="200px"))),
      ),
      div("+ Legend title",inline(textInput(ns("inter_leg"), NULL, "", width="200px")))
    )
  })
  output$inter_palette<-renderUI({
    span("+ Palette",
         inline(
           pickerInput(inputId = ns("pal_inter"),
                       label = NULL,
                       choices =     vals$colors_img$val,
                       choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
         )
    )
  })
  output$side_interactions<-renderUI({
    req(input$Ensemble_tab3=="tab_iter")
    div(
      hr(),
      div(
        div(strong("Interactions:", pophelp(NULL,"Interaction I (X, Y) is defined as the decrease in uncertainty about X (root) after observing Y. Low, close to zero, I means that the variables are close to independent. The larger the I, the larger the reduction of uncertainty in X when Y is known."))),

        uiOutput(ns('inter_rep')),
        uiOutput(ns('inter_root'))

      ),
      hr(),
      uiOutput(ns('inter_plot_params'))


    )
  })
  observeEvent(ignoreInit = T,input$inter_method,{
    vals$inter_method<-input$inter_method
  })
  output$inter_method<-renderUI({
    div(div(strong("3. Method:")),
        div( pickerInput(ns("inter_method"),NULL,c("Paired","Forward")
                         ,selected=vals$inter_method

        )))

  })

  output$inter_plot_params<-renderUI({
    div(class="cogs_in_div",style="margin-bottom: 10px; padding:5px",
        fluidPage(
          actionLink(ns("show_inter_plotparam"),"+ Plot parameters:"),
          uiOutput(ns('out_inter_plotparams'))


        ))

  })
  observeEvent(ignoreInit = T,input$show_inter_plotparam,{
    shinyjs::toggle("out_inter_plotparams")
  })
  observeEvent(ignoreInit = T,input$inter_leg,{
    shinyjs::hide("out_inter_plotparams")
  }, once=T)
  output$out_inter_plotparams<-renderUI({
    req(input$data_ensemble_X)
    #req(!is.null(get_inter_res0()))
    div(
      span("+ Number of variables:",tiphelp("Filter variables with the loss"),
           inline(
             numericInput(ns("npic_inter"),NULL, value=20, width="75px", step=1)
           )
      )
      ,
      uiOutput(ns('inter_sig')),
      uiOutput(ns('inter_palette')),
      uiOutput(ns('inter_ggplot'))
    )

  })


  output$side_performace_loss<-renderUI({


    div(style="margin-top: 5px; margin-bottom: 5px",
        div(
          checkboxInput(ns("en_scale"), "+ Scale importances", vals$en_scale)
        ),
        hr(),
        uiOutput(ns('perm_imp'))

    )
  })


  observeEvent(ignoreInit = T,input$en_scale,
               vals$en_scale<-input$en_scale)

  observeEvent(ignoreInit = T,input$score_loss,
               vals$score_loss<-input$score_loss)


  observeEvent(ignoreInit = T,input$help_loss,{
    showModal(
      modalDialog(
        div(p("The permutation feature importance algorithm is based on Fisher, Rudin, and Dominici (2018). "),
            p("For each predictor ",em('j')," in the dataset:"),
            div(style="margin-left: 10px",
                p(
                  p("1. Permute feature ",em('j')," in the data X."),
                  p('2. Estimate the  performace metric based on the predictions of the permuted data.'),
                  p("3. Calculate permutation feature importance ",  HTML(paste0("P",tags$sub("j")))," as the difference between prediction errors before and after the feature is permuted.")
                )),

            p("In iMESC, this process is repeated ",em("n")," times, and the significance of the feature is calculated using a one-sided t-test,  which tests if the mean feature metric ",  HTML(paste0("P",em(tags$sub("j")))),"is higher than the average of the remaining feature metrics",  HTML(paste0("P",em(tags$sub("-j")))),"."
            )
        ),
        title="Permutation-based variable importance",
        easyClose =T
      )
    )
  })



  colby<-reactive({

    if(isFALSE(input$score_loss)){
      'models'
    } else{ "acculoss"}
  })



  output$side_sig_aculoss<-renderUI({
    div(
      div(
        span(
          column(12,"+ Sig level:"),
          div(inline(numericInput(ns("aculoss_sig"),NULL, value=0.05, width="75px", step=0.02)), inline(tiphelp("perfomace loss higher then")))


        )
      )
    )
  })


  observeEvent(get_newdata(),{
    modelist<-get_modelist_from_em()
    req(!is.null(modelist))
    newdata<-get_newdata()
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    # saveRDS(modelist,'modelist.rds')
    # modelist<-readRDS('modelist.rds')
    newdata<-newdata[,colnames(modelist[[1]]$trainingData[-class])]
    vals$aculoss_npic<-ncol(newdata)
  })


  observeEvent(ignoreInit = T,input$aculoss_npic,{
    vals$aculoss_npic<-input$aculoss_npic
  })


  observeEvent(ignoreInit = T,input$aculoss_rep,{
    vals$aculoss_rep<-input$aculoss_rep
  })

  output$en_rep<-renderUI({

    if(is.null(vals$aculoss_rep)){vals$aculoss_rep<-5}
    div(
      span("+ Repetions:",
           inline(
             numericInput(ns("aculoss_rep"),NULL, value=vals$aculoss_rep, width="75px", step=2)
           )
      )
    )
  })
  output$run_aculoss<-renderUI({

    #req(is.null(vals$aculoss))
    div(class="save_changes",actionButton(ns("run_aculoss"),"+ Calculate Permutation Importance"))
  })









  comb_feaimp.reac<-reactive({
    allimportance<- comb_feaimp(get_modelist_from_em(),  scale=input$en_scale,progress = T)
  })





  observeEvent(ignoreInit = T,input$comp2_results,{
    vals$comp2_results<-input$comp2_results
  })

  getacc<-reactive({
    vals$saved_ensemble[[input$ensemble_models]]$ensemble_resampling
  })
  getacc_models<-reactive({
    acc<-vals$saved_ensemble[[input$ensemble_models]]$models_resampling
    acc
  })








  output$Ensemble_tab7<-renderUI({
    req(!is.null(vals$saved_ensemble))
    acc<-vals$saved_ensemble[[ input$ensemble_models]]$ensemble_resampling
    validate(need(length(acc)>0,"Click in the flashing blue button 'Calculate Permutation Importance'"))

    args<-getargs_plot_scoreloss()

    p<-do.call(plot_model_features,args)
    vals$ensemble_permimp<-p
    renderPlot({vals$ensemble_permimp})
  })






  output$page1<-renderUI({

    div(style="     background-color: #f8f8f8;",
        uiOutput(ns("step1_ensemble")),
        uiOutput(ns("step2_ensemble_error")),
        uiOutput(ns("step2_ensemble")),


    )
  })








  output$step3_ensemble1<-renderUI({
    req(input$use_partition)
    choices=list("New predictions + Validation"="new_val",
                 "New predictions (unknown data)"="new_unkn",
                 "Use saved model"="use_model")

    div(class='step2_dummy',pickerInput(ns("ensemble_predtype"),NULL,choices, selected=vals$ensemble_predtype))
  })


  observeEvent(ignoreInit = T,input$ensemble_predtype,{
    vals$ensemble_predtype<-input$ensemble_predtype
  })
  observeEvent(ignoreInit = T,input$ensemble_saved,{
    vals$ensemble_saved<-input$ensemble_saved
  })
  output$step3_ensemble2<-renderUI({
    req(input$data_ensemble_X)
    req(input$ensemble_predtype)
    req(input$ensemble_predtype=="use_model")

    div(class='step2_dummy',pickerInput(ns("ensemble_saved"),NULL,choices=  names(vals$saved_ensemble), selected=vals$ensemble_saved))
  })

  get_usemodel<-reactive({
    req(input$ensemble_saved)
    vals$saved_ensemble[[input$ensemble_saved]]

  })
  observeEvent(ignoreInit = T,input$data_ensemble_X,{
    attr(vals$saved_data[[input$data_ensemble_X]],"ensemble")<-NULL
  }, once=T)




  observe({
    req(input$ensemble_predtype)
    if(input$ensemble_predtype=="use_model"){
      cur_step$df<-(0)
    } else{
      cur_step$df<-ifelse(input$ensemble_predtype=="new_unkn",3,4)

    }

  })



  output$out_step5_ensemble<-renderUI({
    req(input$ensemble_predtype)
    req(input$ensemble_predtype!="use_model")
    # req(length(names(vals$en_Ydatalists))>0)

    req(input$ensemble_predtype!='new_unkn')

    step<-cur_step$df+1
    lab5<-span(paste0(step,". Validation data (Y)"),tipify(icon("fas fa-question-circle"),"Validation dataset. Data containing observed values to be compared with predicted values"))

    div(class="ensemble_labels0",
        div(class='ensemble_labels',lab5),
        inline(uiOutput(ns('step5_ensemble'))),inline(uiOutput(ns('step5_Y'))),
    )
  })
  output$step5_Y<-renderUI({
    req(length(get_modelist_from_em())>0)
    req(input$ensemble_predtype)

    y<-attr(get_modelist_from_em()[[1]],"supervisor")
    choices=switch(input$ensemble_predtype,
                   "use_model"="None",
                   "new_unkn"="None",
                   "new_val"=y)



    req(input$use_partition)
    if(input$use_partition=="New Data"){
      lab<-paste0("Column:")
      if(!length(names(vals$en_Ydatalists))>0){
        lab="No matching Datalist found for Y:"
        choices=y
      }
    } else{
      lab<-"From partition:"
    }


    div(class='step2_dummy',
        div(class="dummyY",
            em(paste0(lab),
               style="color: gray"),
            em(choices,
               style="color: gray"),
            tiphelp("Only for viewing the column used as supervisor of the selected models", "right")

        ))



  })

  output$out_step6_ensemble<-renderUI({
    req(input$ensemble_predtype)
    req(input$ensemble_predtype!="use_model")
    step<-cur_step$df+1+1
    lab6<-span(strong(step,". Ensembling Method:"),tipify(actionLink(ns("help_weig"),icon("fas fa-question-circle")),"Click for details"))
    div(class="ensemble_labels0",
        div(class='ensemble_labels',lab6),
        div(class='step2_dummy',uiOutput(ns('step6_ensemble')))
    )
  })

  output$out_step7_ensemble<-renderUI({
    req(input$ensemble_predtype)
    req(input$ensemble_predtype!="use_model")
    req(input$en_method)
    step<-cur_step$df+1+1+1
    req(input$en_method!='non-weighted')


    lab7<-span(step,". Estimate weights using:")
    div(class="ensemble_labels0",
        div(class='ensemble_labels',lab7),
        div(class='step2_dummy',uiOutput(ns('step7_ensemble')))
    )
  })
  output$step_ensemble_left<-renderUI({
    div(
      uiOutput(ns('out_step5_ensemble')),
      uiOutput(ns('out_step6_ensemble')),
      uiOutput(ns('out_step7_ensemble'))
    )
  })


  output$out_step8_ensemble<-renderUI({
    req(input$ensemble_predtype)
    req(input$ensemble_predtype=="new_val"
    )
    step<-cur_step$df+1+1+1+1
    lab8<-span(step, ". Search best ensemble",pophelp(NULL,"Calculate performance metrics for the pool of models, which can be used by the user to select the optimal ensemble model."))
    div(class="ensemble_labels0",
        div(class='ensemble_labels',lab8),
        div(class='step2_dummy',uiOutput(ns('step8_search')))
    )

  })

  output$out_step9_ensemble<-renderUI({
    div(class="ensemble_labels0",align="center",style="padding: 15px",
        div(class='ensemble_labels',
            inline(uiOutput(ns('run_preds_btn')))
        )
    )
  })

  output$run_preds_btn<-renderUI({
    # class=ifelse(input$ensemble_models=="New Ensemble", "save_changes","")
    tags$div(
      class = "save_changes",
      id = ns("run_preds_id"),
      actionButton(
        ns("run_preds"),
        paste0("->> Click to Make Predictions"),
        style = "height: 35px; background-color: #4CAF50; color: white; font-size: 14px; border-radius: 5px; padding: 6px 12px; cursor: pointer;"
      )
    )

  })



  output$out_step10_ensemble<-renderUI({
    lab10<-
      span( "Results:", tiphelp("location for storing and accessing the results"))

    div(class="ensemble_labels0",
        div(class='ensemble_labels',lab10),
        div(class='step2_dummy',
            inline(uiOutput(ns('ensemble_results_menu'))),
            inline(uiOutput(ns('save_ensemble_button'))),
            inline(uiOutput(ns('del_ensemble_button')))
        )
    )
  })



  output$step8_search<-renderUI({
    req(input$use_partition)
    if(input$use_partition=='New Data'){

      validate(need(length((vals$en_Ydatalists))>0,"Unavailable for predicting unknown data."))

    }


    div(class='step2_dummy',
        inline(uiOutput(ns('check_pool'))),
        inline(uiOutput(ns('check_pool_metrics')))
    )
  })




  output$check_pool<-renderUI({
    req(input$use_partition)

    switchInput(ns("best_comb"),NULL, T,size ="mini")


  })


  observeEvent(ignoreInit = T,input$search_ensemble_metric_help,{
    showModal(
      tags$div(
        id="modal_gpt",
        modalDialog(
          title = "Metrics Selection for Search",
          easyClose = TRUE,
          size = "m",
          div(
            p("Metrics to be included during the search. Please note that including AUC will increase the analysis runtime."),
            p(
              strong("postResample:"),
              "Accuracy and Kappa calculated using caret's", code('postResample'), "function."
            ),
            p(
              strong("multiClassSummary:"),
              "Accuracy, Kappa, and various statistical averages (F1, Sensitivity, Specificity, Precision) calculated using caret's", code("multiClassSummary"), "function."
            ),
            p(
              strong("... + AUC:"),
              "Area Under the Curve (AUC) calculated using pROC's", code('multiclass.roc'), "function."
            )
          )
        )
      )

    )



  })




  output$check_pool_metrics<-renderUI({
    req(isTRUE(input$best_comb))
    req(!is.null(get_modellist_0()))
    choices= if(get_modellist_0()[[1]]$modelType=="Classification"){
      c("postResample","postResample + AUC","multiClassSummary","multiClassSummary + AUC")
    } else{
      c("postResample")
    }
    lab8.1<-span("Metrics:",tipify(actionLink(ns("search_ensemble_metric_help"),icon('fas fa-question-circle')),"Click for details"))

    div(
      div(class='ensemble_labels',lab8.1, style="width: 75px"),
      div(class='step2_dummy',
          inline(pickerInput(ns("ensemble_metrics"),NULL ,
                             choices =    choices,
                             selected=vals$ensemble_metrics))
      )
    )



  })


  output$search_params<-renderUI({
    req(isTRUE(input$best_comb))
    req(get_modellist_0()[[1]]$modelType=="Classification")
    choiceValues <-list("postResample",'multiClassSummary')
    choiceNames <-list(
      span("postResample", pophelp(NULL,"Metrics from postResample function from caret: Accuracy and Kappa")),
      span('multiClassSummary', pophelp(NULL,"Metrics from multiClassSummary function from caret: Accuracy, Kappa and several averages of statistics such as  F1, Sensivity, Specifity, Precision... "))
    )

    div(style="margin-left: 20px; margin-top: -10px",
        div(radioButtons(ns("ensemble_metrics"), span("Metrics:",pophelp("","Select the metrics to be included in the search, taking into consideration that adding the AUC option will prolong the analysis running time")), choiceNames=choiceNames, choiceValues=choiceValues,selected=vals$ensemble_metrics)),
        div(style="margin-top: -10px; margin-bottom: -10px",
            checkboxInput(ns("AUC"),span('AUC',pophelp(NULL,'Include Multi-class AUC calculation using the multiclass.roc from pROC package.')),value=T)
        )
    )

  })


  output$step4_ensemble<-renderUI({
    if(length(input$use_partition)>0){
      req(input$use_partition=="New Data")}

    pickerInput(ns("data_ensemble_X"),NULL,names(vals$saved_data[getobs_models()]),selected=vals$data_ensemble_X
    )


  })
  output$step4_ensemble_test<-renderUI({
    some_list<-lapply(get_modellist_0(), function(x) rownames(attr(x,"test")))
    if(any(!sapply(some_list,length)>0)){
      choices="New Data"
    } else{
      test_true<-all(sapply(some_list, function(x){
        identical(sort(x), sort(some_list[[1]]))
      }))
      if(isTRUE(test_true)){
        choices=c("Partition","New Data")
      } else{
        choices="New Data"
      }
    }

    div(class='step2_dummy',
        pickerInput(ns("use_partition"), NULL,choices, selected=vals$use_partition))


  })


  observeEvent(ignoreInit = T,input$use_partition,{
    vals$use_partition<-input$use_partition
  })



  output$step2_dummy<-renderUI({
    req(input$ensemble_predtype)
    #  req(input$ensemble_models)
    if(input$ensemble_predtype=="use_model"){
      em<-get_usemodel()
      active<-em$ensemble_active
      active_box<-em$box_pool
      allmodels<-lapply(vals$saved_data[input$data_tosel_ensemble],function(x)get_saved_models(x))[[1]]
      modelist=allmodels[[active_box]][active]
      choices=paste0(names(modelist), collapse="; ")
    } else{

      models=       names( get_modellist_0())
      choices=paste0(models, collapse="; ")
    }

    lab2<-span("2. Initial model pool:",tiphelp("For viewing only. Select the initial pool on tab 1"), style="color: gray")
    div(class="ensemble_labels0",
        div(class='ensemble_labels',lab2),
        div(class='step2_dummy',
            pickerInput(ns("dummy"),NULL,choices=choices))
    )
  })


  output$pagina2<-renderUI({

    lab3<-"3. Prediction type:"

    lab4<-"4. Data for predictions:"
    fluidPage(class='well3',
              column(12,class="small_picker",style="color: #05668D",
                     id="ttt",
                     uiOutput(ns("step2_dummy")),


                     div(class="ensemble_labels0",
                         div(class='ensemble_labels',lab3),
                         inline(uiOutput(ns('step3_ensemble1'))),inline(uiOutput(ns('step3_ensemble2'))),
                     ),
                     div(class="ensemble_labels0",
                         div(class='ensemble_labels',lab4),
                         inline(uiOutput(ns('step4_ensemble_test'))),inline(uiOutput(ns('step4_ensemble')))
                     ),
                     uiOutput(ns("step_ensemble_left"))
                     ,

                     uiOutput(ns('out_step8_ensemble')),
                     uiOutput(ns('out_step9_ensemble')),
                     uiOutput(ns('error_version'))


              )
    )

  })





  observeEvent(ignoreInit = T,input$ensemble_metrics,{
    vals$ensemble_metrics<-input$ensemble_metrics
  })



  val_check_pool<-reactiveValues(df=F)
  observe({

    req(input$ensemble_predtype)
    req(input$use_partition)
    val_check_pool$df<- if(input$use_partition=="New Data"){
      if(is.null(input$obc)){
        F
      } else{
        input$best_comb
      }
    } else{
      input$best_comb
    }

    if(input$use_partition=="New Data"& length((vals$en_Ydatalists))==0){
      val_check_pool$df<- F
    }
    if(input$ensemble_predtype=="new_unkn"){
      val_check_pool$df<- F
    }


  })

  observeEvent(ignoreInit = T,input$data_ensemble_X,{
    vals$data_ensemble_X<-input$data_ensemble_X
  })

  output$Ensemble_tab4<-renderUI({

    div(
      div(
        inline(DT::dataTableOutput(ns('observation_errors_reg'))),


      ),
      div(
        inline(DT::dataTableOutput(ns('observation_errors_class')))
      )
    )

  })

  output$side_create_errors<-renderUI({


    tipify(
      actionLink(
        ns('comb_create_errors_train2'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
      ),
      "Create a datalist with the comb training errors", options=list(container="body")
    )
  })
  output$out_perf<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))
    div(
      uiOutput(ns("cmcomb")),
      uiOutput(ns("regcomb"))
    )

  })

  output$cmcomb<-renderUI({
    em<-get_the_model2()
    modelist=attr(em,"modelist")
    req(modelist[[1]]$modelType=="Classification")
    div(
      div(
        inline(DT::dataTableOutput(ns('errors_class_global')))
      ),
      uiOutput(ns("conf_matrix")),
      uiOutput(ns("conf_matrix_print"))

    )
  })

  output$regcomb<-renderUI({
    em<-get_the_model2()
    modelist=attr(em,"modelist")
    req(modelist[[1]]$modelType=="Regression")
    div(
      div(
        p(strong("Global:")),
        inline(DT::dataTableOutput(ns('comb_tab_errors0_train')))
      )

    )

  })
  observeEvent(ignoreInit = T,input$comb_create_errors_train2,{
    vals$hand_save<-"Create Datalist: comb training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_combb())
  })




  ensemble_errors_regression<-reactive({
    em<-get_the_model2()
    newdata<-em$newdata
    predtab<-em$tabpred
    modelist=attr(em,"modelist")
    req(modelist[[1]]$modelType=="Regression")
    obc=em$obc
    pred<-unlist(em$pred)
    predtab$pred<-pred
    res<-data.frame(t(
      apply(predtab,1,function(x){
        p<-x[length(x)]
        o<-x[-length(x)]
        postResample(p,o)
      })
    ))
    res$obs<-obc
    res$pred<-pred
    res$Rsquared<-NULL
    vals$comb_down_errors_train<-res
    vals$comb_down_errors_train
  })


  output$observation_errors_reg<-DT::renderDataTable({
    req(get_modelist_from_em()[[1]]$modelType=="Regression")
    ensemble_errors_regression()
    res<-vals$comb_down_errors_train
    DT::datatable(round(res,input$round_summ_obserr), options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })

  output$comb_tab_errors0_train<-DT::renderDataTable({
    DT::datatable({

      em<-get_the_model2()
      obs=em$obc
      pred=unlist(em$pred)
      table<-caret::postResample(pred,obs)
      data.frame(as.list(table))
    }, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })


  predall_comp<-reactive({
    res<-do.call(data.frame,get_predtab())
    colnames(res)<-input$data_ensemble_X
    res
  })
  comp_observed2<-reactive({
    req(length(input$obc)>0)
    em<-get_the_model2()
    modelist=attr(em,"modelist")

    if(modelist[[1]]$modelType=="Classification"){
      factors<-attr(vals$saved_data[[input$obc]],'factors')
    } else {
      factors<-vals$saved_data[[input$obc]]}

    res<-factors[,attr(modelist[[1]],"supervisor")]
    names(res)<-rownames(factors[attr(modelist[[1]],"supervisor")])
    res

  })
  get_qclass<-reactive({
    req(input$q_class_val)
    em<-get_the_model2()
    predtab=em$tabpred
    weis<-attr(predtab,"weis")
    modelist=attr(em,"modelist")

    m<-modelist[[1]]
    req(m$modelType=="Regression")
    pred<-em$pred
    model_data<-em$obc
    obs_data<-em$obc
    obs_data<-data.frame(em$obc)
    ref1<-rownames(model_data)
    ref2<-rownames(obs_data)
    ref1<-ref2
    lo<-split(obs_data,ref2)

    pred_interval=input$q_class_val
    pred.comp.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )


    lpred<-split(pred,ref1)
    lp<-split(pred.comp.int,ref1)
    lop<-mapply(list, lp,lo,lpred,SIMPLIFY=FALSE)

    x<-lop[[1]]
    res<-do.call(rbind,lapply(lop,function(x){
      interval<-x[[1]]
      obs<-unlist(x[[2]])
      cbind(x[[2]],do.call(rbind,lapply(obs, function(xx){

        res<-if(isTRUE(between(xx,interval[[1]],interval[[2]] ))){"as expected"} else{
          if(xx<=interval[[1]]){"lower"} else if(xx>=interval[[1]]){"higher"} else{"as expected"}
        }

        interval$q_class<-res
        wil_res<-interval
        wil_res
      })))
    }))

    colnames(res)[c(2,3)]<-c("q0.025",'q0.975')
    rownames(res)<-rownames(obs_data)
    res$q_class<-factor(res$q_class, levels=c("lower","as expected","higher"), labels=c("lower","as expected","higher"))
    vals$comp_treetest<-res
    vals$comp_treetest
  })
  output$comp_tree_show<-renderUI({
    val=if(get_modelist_from_em()[[1]]$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$comp_tree_pred<-renderUI({
    req(input$nhist_tree)


    div(

      actionButton(ns('downp_summ_trees'),tipify(icon("fas fa-download"), "Download Plot"), style="button_active")
    )
  })
  output$qclass_legend<-renderUI({
    req(get_modelist_from_em()[[1]]$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),paste(getensemble_method(),'predictions'))

        ))
  })

  getensemble_method<-reactive({
    if(input$en_method=="non-weighted"){'Mean'} else{
      'Weighted mean'
    }
  })
  output$forest_regression<-renderUI({
    req(get_modelist_from_em()[[1]]$modelType=="Regression")
    plotOutput(ns("plot_forest_regression"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$Ensemble_tab5<-renderUI({
    validate(need(length(get_obc())>0,"Validation data for performance calculation not found."))
    choices<-if(get_modelist_from_em()[[1]]$modelType=="Classification"){
      c('Accuracy','Error')
    } else{
      c('RMSE','MAE')
    }
    myList <- as.list(c("Accuracy","Error"))
    names(myList)<-choices



    div(style="background: white",

        inline(pickerInput(ns("sort_df"),
                           "Sort data by",
                           c(myList,
                             colnames(attr(get_newdata(),"factors"))
                           ),

                           width="200px"
        )),
        inline(uiOutput(ns("comp_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("forest_regression")),
        uiOutput(ns("forest_byclass")))
  })









  output$forest_byclass<-renderUI({
    req(input$qclass_height)
    req(input$qclass_width)
    req(get_modelist_from_em()[[1]]$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })







  output$forest_margin<-renderUI({
    req(get_modelist_from_em()[[1]]$modelType=="Regression")

    numericInput(ns("q_class_val"),"+ Margin:", value=0.05, step=0.05)

  })
  output$forest_col<-renderUI({

    req(get_modelist_from_em()[[1]]$modelType=="Classification")
    pickerInput(inputId = ns("forest_col"),
                label = "+ Palette",
                choices =     vals$colors_img$val[getgrad_col()],
                choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"))
  })
  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })

  test_comp<-reactive({
    req(length(input$obc)>0)
    if(get_modelist_from_em()[[1]]$modelType=="Classification"){
      factors<-attr(vals$saved_data[[input$obc]],'factors')
    } else {
      factors<-vals$saved_data[[input$obc]]}

    res<-factors[,attr(get_modelist_from_em()[[1]],"supervisor")]
    names(res)<-rownames(factors[attr(get_modelist_from_em()[[1]],"supervisor")])
    res

  })


  output$eplot_obs_selection<-renderUI({
    div(class="cogs_in_div",style="margin-bottom: 10px; padding:5px",
        fluidPage(
          actionLink(ns("show_obs_selection"),"+ Select the observations"),
          DT::dataTableOutput(ns('observation_selection'))


        )
    )

  })

  output$eplot_plot_params<-renderUI({
    div(class="cogs_in_div",style="margin-bottom: 10px; padding:5px",
        fluidPage(
          actionLink(ns("show_plot_params"),"+ Plot params"),
          uiOutput(ns('out_eplot_plot_params'))


        ))

  })
  observeEvent(ignoreInit = T,input$show_plot_params,{
    shinyjs::toggle("out_eplot_plot_params")
  })


  output$out_eplot_plot_params<-renderUI({
    div(
      uiOutput(ns('forest_margin')),
      uiOutput(ns('forest_col')),
      numericInput(ns("qclass_sizeplot"),"+ Size:", value=1.5),
      numericInput(ns("qclass_width"),"+ Width", value=800),
      numericInput(ns("qclass_height"),"+ Height", value=700),
      uiOutput(ns("side_round_plotpred")),
      uiOutput(ns("side_qclass_axes_plot"))
    )
  })

  observeEvent(ignoreInit = T,input$qclass_height,{

    shinyjs::hide('out_eplot_plot_params')

  },once=T)


  observeEvent(ignoreInit = T,input$show_obs_selection,{
    shinyjs::toggle("observation_selection")
  })



  output$observation_selection = DT::renderDataTable(
    {  em<-get_the_model2()
    table=data.frame(id=rownames(em$newdata))
    DT::datatable(table, options=list(
      dom="t",
      colnames="",
      lengthMenu = list(c(-1), c("All")),
      scrollX = TRUE,
      scrollY = "200px",
      autoWidth=F

    ), class ='compact cell-border',rownames=F,

    selection = list(mode = 'multiple', selected = c(1:20)))
    }

  )


  output$forestbyclass<- renderPlot({
    req(length(input$observation_selection_rows_selected)>0)
    req(input$forest_col)
    em<-get_the_model2()
    modelist=attr(em,"modelist")
    req(modelist[[1]]$modelType=="Classification")
    newdata<-em$newdata
    selected=rownames(newdata)[input$observation_selection_rows_selected]
    predtab<-em$tabpred[selected,]

    obc=em$obc[selected]
    pred=em$pred[selected,,drop=F]

    sortby=input$sort_df

    res_forest<-  resforest()[selected,]
    data_factors<-data_factors()[selected,]
    ord<-get_qclass_ord(res_forest,obc, sortby,data_factors=data_factors)

    #args<-readRDS("ord.rds")

    result_forest<-res_forest[ord,]

    pred_res<-pred[rownames(result_forest),,drop=F]
    obc_res<-obc[rownames(result_forest)]

    args<-list(
      result_forest=result_forest,
      pred=pred_res,
      palette=input$forest_col,
      newcolhabs=vals$newcolhabs,
      ylab=input$qclass_ylab,
      xlab=input$qclass_xlab,
      leg=input$qclass_leg,
      leg2=input$qclass_leg2,
      cex.axes=input$qclass_cex.axes,
      cex.lab=input$qclass_cex.lab,
      cex.main=input$qclass_cex.main,
      cex.leg=input$qclass_cex.leg,
      textcolor=input$qclass_textcolor,
      main=input$qclass_main,
      obc=obc_res,
      size_points=input$qclass_size_points
    )


    vals$vartrees<-do.call(plotforest_qclass,args)
    vals$vartrees
  })


  output$sel_obs_fordddplot<-DT::renderDataTable({
    em<-get_the_model2()
    table=data.frame(id=rownames(em$newdata))

    DT::datatable({table}, options=list( scrollX = TRUE,
                                         scrollY = "400px",
                                         dom = 't', rownames = F,class ='compact cell-border'))

  },

  selection = list(mode = 'multiple', selected = rownames(table))

  )


  output$side_qclass<-renderUI({



    div(
      class="map_control_style",style="color: #05668D",
      div(strong("Plot parameters:"),
          uiOutput(ns('eplot_obs_selection')),
          uiOutput(ns('eplot_plot_params'))),

      uiOutput(ns("forest_saves"))


    )
  })
  output$forest_saves<-renderUI({
    req(get_modelist_from_em()[[1]]$modelType=="Regression")
    div(
      div(actionLink(ns('resamp_pred_gdownp'),"+ Download table")),
      div( actionLink(ns('resamp_pred_gcreate'),"Create Datalist"))
    )
  })
  data_factors<-reactive({
    attr(vals$saved_data[[input$data_ensemble_X]],"factors")[rownames(vals$saved_data[[input$data_ensemble_X]]),,drop=F]
  })

  output$plot_forest_regression<-renderPlot({
    em<-get_the_model2()
    modelist=attr(em,"modelist")
    req(modelist[[1]]$modelType=="Regression")
    req(length(input$observation_selection_rows_selected)>0)
    selected<-rownames(em$newdata)[input$observation_selection_rows_selected]
    get_qclass()
    req(input$q_class_val)

    ensemble_errors_regression()
    df<-vals$comb_down_errors_train[selected,]


    data_factors<-data_factors()[selected,]
    ord<-get_qclass_ord(df,em$obc, input$sort_df,data_factors=data_factors)
    pred<-em$tabpred[ord,]
    data=t(pred)

    data<-data[,selected, drop=F]
    obs_data<-em$obc[selected]
    obs_data<-data.frame(obs_data)
    obs=obs_data[ colnames(data),]

    pred <- pred
    pred_interval=input$q_class_val
    q_class=vals$comp_treetest[colnames(data),]

    str_numerics3(numerics=data,
                  obs=obs_data[ colnames(data),],
                  pred_interval=pred_interval,
                  q_class=q_class,
                  cex=input$qclass_sizeplot,
                  pred_ensemble=unlist(em$pred[selected,]))
    vals$vartrees<-recordPlot()
  })

  getsolid_col<-reactive({

    res<-lapply(vals$newcolhabs, function(x) x(10))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic

  })

  output$side_qclass_axes_plot<-renderUI({


    div(
      pickerInput(inputId = ns("qclass_textcolor"),
                  label = "+ Text Color",
                  choices =     vals$colors_img$val[getsolid_col()],
                  selected="black",
                  choicesOpt = list(content =     vals$colors_img$img[getsolid_col()]), options=list(container="body")),

      numericInput(ns("qclass_size_points"),"+ Plot size", value=3, step=1),
      textInput(ns("qclass_main"), "+ Plot title", ""),
      numericInput(ns("qclass_cex.main"),"+ Title size", value=13, step=1),
      numericInput(ns("qclass_cex.axes"),"+ Axes size", value=13, step=1),
      numericInput(ns("qclass_cex.lab"),"+ Label size", value=14, step=1),
      numericInput(ns("qclass_cex.leg"),"+ Label size", value=13, step=1),
      textInput(ns("qclass_xlab"), "+ Xlab", 'Accuracy'),
      textInput(ns("qclass_ylab"), "+ ylab", 'Observations'),
      textInput(ns("qclass_leg"), "+ Legend title", "Model predictions"),
      textInput(ns("qclass_leg2"), "+ Legend title", "Final prediction"))
  })


  getord<-reactive({
    req(input$sortby)
    predtab=get_predtab()
    modelist=get_modelist_from_em()
    obc=get_obc()
    pred=get_pred()
    round=input$round_summ_plotpred
    en_method=input$en_method
    res_forest<-resforest()
    result<-get_obs_erros_ensemble_class(res_forest,predtab=predtab,modelist=modelist,obc=obc,pred=pred, round=5, en_method=en_method)
    vals$comb_down_errors_train<-res
    if(input$sortby=="Accuracy"){
      ord<-order(result$Accuracy, decreasing = T)
    } else{
      ord<- order(result$Error, decreasing = T)
    }
    ord<-rownames(result[ord,])
    ord
  })


  observeEvent(ignoreInit = T,input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_combb())

  })
  observeEvent(ignoreInit = T,input$resamp_pred_gdownp,{

    vals$hand_down<-"ensemble_qclass"
    vals$down_ensemble<-data.frame(resample_data())

    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  output$comb_err_class<-renderUI({
    div(

      renderPrint({
        res<-do.call(data.frame,get_predtab())
        colnames(res)<-input$data_ensemble_X
        res
      })


    )
  })

  getobs_reg<-reactive({
    sup_test<- attr(get_modelist_from_em()[[1]],'supervisor')
    datalist<-vals$saved_data
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })




  savereac<-reactive({
    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img
    tosave$modellist0<-vals$modellist0
    tosave$modellist2<-vals$modellist2
    tosave$box_pool<-vals$box_pool
    tosave$comb_down_errors_train<-vals$comb_down_errors_train
    tosave$saved_ensemble<-vals$saved_ensemble
    #tosave$ensemble_args<-getargs_root()
    #tosave$ensemble_args_plot<-getargs_plot_scoreloss()




    saveRDS(tosave,"savepoint.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()


  })
  observeEvent(ignoreInit = T,input$teste_comb,{
    savereac()
  })


  get_conf_matrix<-reactive({
    em<-get_the_model2()
    obs=em$obc
    pred=unlist(em$pred)
    #req(length(as.vector(pred))==length(as.vector(obs)))
    conf<-table(unlist(pred),obs)

    conf
  })

  output$conf_matrix_print<-renderUI({
    conf<-get_conf_matrix()
    vals$ensemble_confusion<-data.frame(conf)
    renderPrint(confusionMatrix(conf))
  })


  output$cm_title<-renderUser({
    textInput(ns("cm_title"), "+ Title", 'Confusion Matrix')
  })
  output$conf_matrix<-renderUI({
    conf<-get_conf_matrix()
    req(input$cm_title)
    req(input$pal_combine)
    renderPlot({
      res<-plotCM(conf/sum(conf)*100,input$pal_combine, newcolhabs=vals$newcolhabs, title=input$cm_title, round_cm=input$round_summ_perf)
      vals$combcm<-res
      vals$combcm
    })
  })

  output$errors_class_global<-DT::renderDataTable({
    pred<-get_pred()
    obs<-get_obc()
    vals$comb_perfomace_class<-postResample(pred,obs)
    DT::datatable({round( data.frame(as.list(vals$comb_perfomace_class)),input$round_summ_perf)}, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })



  resforest<-reactive({
    em<-get_the_model2()
    predtab=em$tabpred
    weis<-attr(predtab,"weis")
    modelist=attr(em,"modelist")



    resforest<-ensemble_find_probs(list(colnames(predtab)),modelist,weis,predtab,  show_progress=T, pararell=F,
                                   # session=getDefaultReactiveDomain()
                                   session=MockShinySession$new()
    )[[1]]
    resforest
  })

  output$observation_errors_class<-DT::renderDataTable({

    em<-get_the_model2()
    modelist=attr(em,"modelist")
    req(modelist[[1]]$modelType=="Classification")
    newdata<-em$newdata
    predtab<-em$tabpred

    obc=em$obc
    pred=em$pred
    round=input$round_summ_obserr
    en_method=input$en_method
    resforest<-resforest()


    res<-get_obs_erros_ensemble_class(resforest,predtab=predtab,modelist=modelist,obc=obc,pred=pred, round=5, en_method=en_method)
    vals$comb_down_errors_train<-res
    res
    DT::datatable(res, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })

  getobs_bcm<-reactive({

    sup_test<- attr(get_modelist_from_em()[[1]],'supervisor')
    datalist<-vals$saved_data
    datalist=lapply(datalist,function(x) attr(x,"factors"))
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })


  weitype<-reactive({
    req(input$en_method)
    data_weis<-if(input$en_method=="weighted"){
      req(input$wei_datatype)
      input$wei_datatype
    } else if(input$en_method=="accu_weighted"){
      req(input$wei_datatype)
      input$wei_datatype}
  })




  get_en_method_choices<-reactive({
    req(!is.null(get_modellist_0()))
    if(get_modellist_0()[[1]]$modelType=="Classification"){
      if(is.null(input$obc) & input$use_partition=="New Data"){

        choiceNames<-c(
          "Weighted votes (overall performace)",
          "Majority votes"
        )
        choiceValues=c("accu_weighted","non-weighted")

      } else {
        choiceNames<-c(
          "Weighted votes (adaptative)",
          "Weighted votes (overall performace)",
          "Majority votes"
        )
        choiceValues=c("weighted","accu_weighted","non-weighted")
      }

    } else{
      choiceNames<-c(
        "Weighted Mean (overall performace)",
        "Mean"
      )
      choiceValues=c("accu_weighted","non-weighted")
    }
    mylist<-as.list(choiceValues)
    names(mylist)<-choiceNames
    mylist
  })

  output$step6_ensemble<-renderUI({



    pickerInput(ns("en_method"), NULL,get_en_method_choices(), selected=vals$en_method)


  })

  observeEvent(ignoreInit = T,input$en_method,
               vals$en_method<-input$en_method)





  output$step7_ensemble<-renderUI({
    req(input$use_partition)
    req(input$ensemble_predtype)
    req(input$en_method=="weighted"|input$en_method=="accu_weighted")

    choices<-if( length(names(vals$en_Ydatalists))==0&input$use_partition=="New Data"){
      c(list("Training data"="training"))
    } else{
      c(list('New data/validation set'="test"),
        list("Training data"="training"))
    }
    if(input$ensemble_predtype=="new_unkn"){
      choices=   c(list("Training data"="training"))
    }



    pickerInput(
      ns("wei_datatype"),NULL,
      choices,selected=vals$wei_datatype
    )

  })
  observeEvent(ignoreInit = T,input$wei_datatype,
               vals$wei_datatype<-input$wei_datatype)






  output$wmean_help<-renderUI({
    req(get_modellist_0()[[1]]$modelType=="Regression")
    column(12,
           p(h4(strong("1. Mean")),
             p("The ensemble prediction is calculated as the average of the observation predictions. Each ensemble member contributes an equal amount to the final prediction."),
             hr(),
             h4(strong("2. Weighted Mean")),

             p("A weighted ensemble is an extension of a model averaging ensemble where the contribution of each member to the final prediction is weighted by the performance of the model (Rsquared) which can be estimated either using the  training data (i. e., the original X and Y data on which the models were trained) or the new data X and Y.")),
           p("Finding the weights using the same training set used to fit the ensemble members will likely result in an overfit mode. A more robust approach is to use a data set unseen by the ensemble members during training."),
           p("The sum of all weights equals one, allowing the weights to indicate the percentage of trust or expected performance from each model.")
    )
  })
  output$votes_help<-renderUI({
    em<-get_the_model2()
    modelist=attr(em,"modelist")
    req(modelist[[1]]$modelType=="Classification")
    column(12,

           p(h4(strong("1. Marjority Votes")),
             p("In this method, each  every individual model votes for a class, and the majority wins."),
             hr(),
             h4(strong("2. Marjority weighted votes (overall performace)")),
             p("The models have varying degrees of effect on the final prediction. Each classifier is associated with a weight proportional to its classification performance on a validation set (Accuracy).The sum of all weights equals one, allowing the weights to indicate the percentage of trust or expected performance from each model. The final decision is made by summing up all weighted votes and by selecting the class with the highest aggregate. "),
             p("Wheighs can be estimated either using the  training data (i. e., the original X and Y data on which the models were trained) or the new data X and Y."),
             p("Finding the weights using the same training set used to fit the ensemble members will likely result in an overfit mode. A more robust approach is to use a data set unseen by the ensemble members during training."),
             hr(),
             h4(strong("3. Majority weighted votes (adaptative)")),
             p("Similar to the method described above, with the difference that, the models that correctly classify observations which are not correctly classified by most of the classifiers gain more weights in the ensemble. In this approach, there are three phases:"
             ),


             p(strong('(1) Determine the weights of the classifiers using validation set'),"In this phase, each classifier generates a decision pointing to predicted class label of a single instance and then these decisions are evaluated to update weights. In this approach, the weights are updated for each instance in the validation set. Initially, all weights are equal to 1. All instances in the validation dataset are traversed and processed through ",em("n")," classifiers once. The weights of the classifiers that correctly predict class label of an instance are incremented by the ratio of the number of incorrectly predicting classifiers to the whole number of classifiers (",em("n"),"). The output can be outlined with the following equation:"),
             p(
               withMathJax(
                 helpText('$$w_{ij}=\\begin{cases}
               w_{i-1,j}+\\alpha_i,  & \\text{if $j^{th}$ classifier makes a correct prediction for $i^{th}$ instance} \\\\
               w_{i-1,j}, & \\text{if $j^{th}$ classifier makes an incorrect prediction for $i^{th}$ instance}
               \\end{cases}\\!$$')),
               div("where",inline(helpText("$$w_{ij}$$")),"is the weight of",inline(helpText("$$j^{th}$$"))," classifier as a result of the operation realized on",inline(helpText("$$i^{th}$$")),"instance, where", inline(helpText("$$i={1,2,...,m}$$")),HTML('&nbsp;'), "and", inline(helpText("$$j={1,2,...,n}$$"))),inline(";"),inline(helpText("$$\\alpha_i$$")),"is the change in weight and calculated as ",inline(helpText("$$\\alpha_i=\\frac{Y_i}{n}$$"))," where ",inline(helpText("$$Y_i$$"))," is the number of incorrect predictions for ",inline(helpText("$$i^{th}$$"))," instance and ",inline(helpText("$$n$$"))," is the number of classifiers"
             ),

             p(strong('(2) Combine the outputs of individual classifiers by considering their weights.'),"In the final decision, all weighted votes are summed for each class and the class receiving the most weighted votes becomes predicted class of an instance to be classified, as
given in:",
p(
  withMathJax(
    helpText("$$\\displaystyle{\\max_{1\\geqslant{j}\\geqslant{k}}}{\\sum_{t = 1}^{n} {w_t} {d_{t,j}}} $$"))
))
           ))
  })

  observeEvent(ignoreInit = T,input$help_weig,{
    showModal(
      modalDialog(
        title='Weights for ensembling models',
        easyClose =T,
        size="l",
        div(uiOutput(ns("votes_help")),
            uiOutput(ns("wmean_help")))
      )
    )
  })




  observe({
    vals$cur_obc<-input$cur_obc})




  observe({
    vals$cur_obc<-input$cur_obc})



  output$step5_ensemble<-renderUI({
    req(input$data_ensemble_X)
    req(input$use_partition)
    req(input$use_partition=="New Data")
    req(length(names(vals$en_Ydatalists))>0)
    div(class='step2_dummy',pickerInput(ns("obc"),NULL,names(vals$en_Ydatalists), selected=vals$cur_obc))
  })


  observe({

    req(input$data_ensemble_X)
    req(length(get_modellist_0())>0)
    newdata<-vals$saved_data[[input$data_ensemble_X]]
    #vals<-readRDS('savepoint_lu.rds')
    #input<-readRDS('input.rds')
    sup_test<-attr(get_modellist_0()[[1]],"supervisor")



    req(length(sup_test)>0)
    datalist<-vals$saved_data
    if(get_modellist_0()[[1]]$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}


    res1<-unlist(
      lapply(datalist, function (x){
        nrow(x)==nrow(newdata)
      })
    )
    req(length(res1)>0)
    datalist<-datalist[which(res1)]
    # rownames(datalist[[1]])<-1:nrow(datalist[[1]])

    res2<-unlist(
      lapply(datalist, function (x){
        if(length(newdata)>0){
          if(length(x)>0) {

            if(nrow(x)==nrow(newdata)){
              sum(rownames(x)==rownames(newdata))==nrow(newdata)
            }


          }
        }
      })
    )

    datalist<-datalist[which(res2)]
    #colnames(datalist[[1]])<-1:ncol(datalist[[1]])
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)
      })
    )

    vals$en_Ydatalists<-res0[which(res0)]


  })












  output$side_create_preds<-renderUI({


    div(
      tipify(
        actionLink(
          ns('create_ensemble_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
        ),
        "Create a datalist with results", options=list(container="body")
      )
    )
  })


  output$side_palette<-renderUI({
    pickerInput(inputId = ns("pal_combine"),
                label = "+ Palette",
                choices =     vals$colors_img$val,
                choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"))
  })

  output$Ensemble_tab2<-renderUI({
    req(!is.null(get_predtab()))
    inline(DT::dataTableOutput(ns('ensemble_predictions')))
  })

  output$Model_tab1<-renderUI({
    req(!is.null(get_predtab()))
    div(style="max-width: 600px;overflow-x: scroll",
        inline(DT::dataTableOutput(ns('Model_tab1_table')))
    )
  })

  output$Model_tab1_table<-DT::renderDataTable({
    req(input$round_summ_model)


    accs<-if(length(get_obc())>0)
    { res<-get_model_performaces()
    round(res,input$round_summ_model)
    } else{
      NULL
    }

    rbind(accs,get_predtab())
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE)







  output$ensemble_predictions<-DT::renderDataTable({
    get_pred()
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE)



  get_model_performaces<-reactive({
    em<-get_the_model2()
    modelist<-attr(em,"modelist")
    newdata<-em$newdata
    if(is.null(newdata)){
      class=which(colnames(newdata[[1]]$trainingData)==".outcome")
      newdata<-modelist[[1]]$trainingData[,-class, drop=F]
    }

    obc<-get_obc()


    validate(need(length(obc)==nrow(newdata),"Error: The observed and predicted values have different lengths."))


    li<-lapply(modelist,function(m){
      accu<-postResample(predict(m,newdata=newdata),obc)
      if(m$modelType=="Classification"){accu["Accuracy"]
      } else{accu['Rsquared']}

    })

    res<-do.call(cbind,li)
    res

  })




  observeEvent(ignoreInit = T,input$create_ensemble_predictions,{

    vals$hand_save<- "Create Datalist: ensemble predictions"

    vals$hand_save3<-NULL
    showModal(module_combb())
  })

  getobs_models<-reactive({
    datalist<-vals$saved_data
    data<-vals$saved_data[[input$data_tosel_ensemble]]
    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(data)
        sum(res)==ncol(data)
      })
    )
    names(res0[res0==T])
  })

  output$step1_ensemble<-renderUI({
    div(
      h5(strong("1. Select the Datalist")),
      pickerInput(ns("data_tosel_ensemble"),NULL,choices=names(vals$saved_data),  selected=vals$cur_data_comp, options=list(container="body","style-base" = "form-control", style = "")))
  })
  observeEvent(ignoreInit = T,input$data_tosel_ensemble,{
    vals$cur_data_comp<-input$data_tosel_ensemble
  })
  getdata_tosel_ensemble<-reactive({

    req(input$data_tosel_ensemble)
    vals$saved_data[[input$data_tosel_ensemble]]

  })






  get_box_pool<-reactive({

    data<-getdata_tosel_ensemble()
    choices<-list(
      if(check_model_caret(data,"rf")){"rf"},
      if(check_model_caret(data,"nb")){"nb"},
      if(check_model_caret(data,"svm")){"svm"},
      if(check_model_caret(data,"knn")){"knn"},
      if(check_model_caret(data,"sgboost")){"sgboost"},
      if(check_model_caret(data,"xyf")){"xyf"}
    )

    attributes<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]

    validate(need(length(attributes)>0,"No models saved in selected datalist"))
    limodels2<-lapply(attributes,function(x) attr(data,x))
    names0<-unlist(lapply(limodels2,names))
    limodels2<- flattenlist(limodels2)
    linames<-unlist(lapply(attributes,function(x) names(attr(data,x))))
    limodels2<-limodels2[which(sapply(limodels2, function(x) class(x)[1]=="train"))]

    resmethos<-unlist(lapply(limodels2,function(x) paste0("Type:",x$modelType,"; ")))
    ressup<-unlist(lapply(limodels2,function(x) attr(x,"supervisor")))


    dftype<-data.frame(type=as.vector(resmethos),sup=as.vector(ressup))
    resmethos<-as.vector(apply(dftype,1,function(x) {
      paste0("Y:",x,collapse="")
    }))

    #if(length(pic_not)>0){limodels2<-limodels2[-pic_not]}
    liequals<-split(limodels2,as.vector(unlist(lapply(limodels2,function(x)nrow(x$resample)))))
    liequals<-lapply(liequals,function(x) names(x))
    box_pool<-split(limodels2,resmethos)
    listanames<-split(names0,resmethos)
    for(i in 1:length(box_pool)){
      names(box_pool[[i]])<-listanames[[i]]
    }

    box_pool

  })


  output$step2_ensemble<-renderUI({




    choiceValues<-names(get_box_pool())
    choiceNames=lapply(seq_along(names(get_box_pool())),function(x){
      div(class="card",style="width: 400px;",uiOutput(ns(paste0('liequals',x))))
    })


    req(length(choiceValues)>0)
    req(length(choiceNames)>0)






    div(


      div(
        inline(h5(strong('2. Select the models', tiphelp("comparisons are only allowed for models with compatible resampling methods.")))),
        inline(div(style="margin-left: 30px",inline(uiOutput(ns("selectallmodels")))))
      ),
      div(id='comp2_choices',
          radioButtons(ns("limodels2"), NULL, choiceValues=choiceValues,choiceNames =choiceNames, inline=T, selected=vals$cur_limodels2)
      )

    )
  })




  models_selected<-reactive({
    req(input$limodels2)
    box_pool<-get_box_pool()
    req(!is.null(box_pool))
    liequals<-lapply(box_pool, names)
    names(liequals)<-1:length(liequals)
    res<-lapply(seq_along(names(box_pool)),function(x){
      input[[paste0("equals",names(liequals)[x])]]

    })
    names(res)<-names(box_pool)
    res[[input$limodels2]]

  })



  observe({
    data<-getdata_tosel_ensemble()
    choices<-list(
      if(check_model_caret(data,"rf")){"rf"},
      if(check_model_caret(data,"nb")){"nb"},
      if(check_model_caret(data,"svm")){"svm"},
      if(check_model_caret(data,"knn")){"knn"},
      if(check_model_caret(data,"sgboost")){"sgboost"},
      if(check_model_caret(data,"xyf")){"xyf"}
    )
    attributes<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]

    validate(need(length(attributes)>0,"No models saved in selected datalist"))
    req(input$limodels2)
    req(length(input$limodels2)>0)
    box_pool<-  get_box_pool()
    req(!is.null(box_pool))


    liequals<-lapply(box_pool, names)
    names(liequals)<-1:length(liequals)
    vals$liequals<-liequals
    ress<-list()
    lapply(seq_along(names(box_pool)),function(x){
      output[[paste0('liequals',x)]]<-renderUI({
        div(class='card',h5(strong(names(box_pool)[x])),width=12,
            if(input$limodels2==names(box_pool)[x]){
              div(
                checkboxGroupInput(ns(paste0("equals",names(liequals)[x])), NULL, choices=liequals[[x]], selected=liequals[[x]]), style="max-width: 190px; white-space: normal;font-size: 12px; padding:0px"
              )
            })
      })
    })

  })



  observeEvent(ignoreInit = T,input$models_check,{
    box_pool<-get_box_pool()
    liequals<-vals$liequals
    if(isTRUE(input$models_check)){
      lapply(seq_along(names(box_pool)), function(x){
        updateCheckboxGroupInput(session,paste0("equals",names(liequals)[x]),choices=liequals[[x]], selected=liequals[[x]])
      })

    } else{
      lapply(seq_along(names(box_pool)), function(x){
        updateCheckboxGroupInput(session,paste0("equals",names(liequals)[x]),choices=liequals[[x]])
      })
    }
  })









  observeEvent(ignoreInit = T,input$insert_prev, navPage(-1))
  observeEvent(ignoreInit = T,input$insert_next, navPage(1))
  output$comp2_pages<-renderUI({

    div(
      div(class = "page2",
          id = ns('insert_step1'),
          # uiOutput(ns("page1"))
      ),

      div(class = "page2",
          id = ns('insert_step2'),
          # uiOutput(ns("pagina2"))
      ),
      div(class = "page2",
          id = ns('insert_step3'),
          #uiOutput(ns("page3"))
      )

    )
  })



  comb_create_training_errors<-reactive({
    datao<-vals$saved_data[[input$data_tosel_ensemble]]
    em<-get_the_model2()


    obc=em$obc
    req(length(obc)>0)
    modelist<-attr(em,"modelist")
    temp<-vals$comb_down_errors_train

    newdata<- em$newdata
    newdata<-vals$saved_data[[input$data_tosel_ensemble]][rownames(newdata),]


    temp<-data_migrate(datao,temp,"newdatalist")
    if(modelist[[1]]$modelType=="Classification"){
      facs<-temp[,c('obs',"pred")]
      facs$pred<-factor(facs$pred, levels=levels(obc))
      factors<-attr(datao,"factors")[rownames(newdata),]
      factors<-data.frame(factors,facs)
      temp<-temp[,1:2]
      attr(temp,"factors")<-factors
    }
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })



  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      ##RF
      "Save results in"= name_save_ensemble(),
      "Create Datalist: ensemble predictions"={ name_combb_pred()},
      "Create Datalist: model predictions"={ name_combb_pred()},
      "resample_create"={ resample_create_name()},
      "Create Datalist: comb training errors -obs"={name_comb_train_errors()},
    )})


  resample_data<-reactive({
    req(length(input$obc)>0)
    m<-get_modelist_from_em()[[1]]
    var<-attr(m,"supervisor")
    temp<-vals$comp_treetest

    factors<-attr(temp,"factors")

    factors<-temp[c('q_class')]
    colnames(factors)<-paste(var,colnames(factors),sep="_")
    temp[c('w_class','sig','q_class')]<-NULL
    temp
  })

  resample_create<-reactive({

    datao<-vals$saved_data[[input$data_tosel_ensemble]]
    temp<-resample_data()
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      attr(temp,"factors")<-cbind(attr(temp,"factors"),temp)
      vals$saved_data[[input$newdatalist]]<-temp

    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      attr(temp,"factors")<-cbind(attr(temp,"factors"),temp)
      vals$saved_data[[input$over_datalist]]<-temp

    }

  })





  combb_create_pred<-reactive({

    datao<-vals$saved_data[[input$data_tosel_ensemble]]
    temp<-get_pred()
    newdata<- get_newdata_source()
    if(input$ensemble_predtype=='new_val'){
      newdata<-vals$saved_data[[input$data_tosel_ensemble]][rownames(newdata),]
    } else{

      newdata<- get_newdata_source()
      newdata[colnames(temp)]<-temp

      }
    modelist<-get_modelist_from_em()
    temp<-data_migrate(newdata,temp)

    if(modelist[[1]]$modelType=="Regression"){
    }else{
      obcs<-get_obc()
      facs<- attr(newdata,"factors")[names(obcs),, drop=F]
      facs<-cbind(facs,temp)
      facs[,colnames(temp)]<-factor(facs[,colnames(temp)], levels=levels(obcs))
      attr(temp,"factors")<-facs
    }

    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }
  })



  output$data_over<-renderUI({
    data_overwritte$df<-F
    choices<-c(names(vals$saved_data))
    req(input$hand_save=="over")
    if(vals$hand_save=='Save results in'){choices<-names(vals$saved_ensemble)}
    res<-pickerInput(ns("over_datalist"), NULL,choices, width="350px")
    data_overwritte$df<-T
    inline(res)
  })

  output$ensemble_results_menu<-renderUI({

    saved_models<-names(vals$saved_ensemble)
    #  req(length(saved_models)>0)


    pickerInput(ns("ensemble_models"),NULL, choices= unique(c('New Ensemble',saved_models)), selected=vals$ensemble_models)  })




  observeEvent(ignoreInit = T,input$ensemble_models,
               vals$ensemble_models<-input$ensemble_models)

  output$save_ensemble_button<-renderUI({
    req(input$data_ensemble_X)
    req(input$ensemble_models=="New Ensemble")
    models<-vals$saved_ensemble
    req(length(models[["New Ensemble"]])>1)
    div(class='button_min',
        div(class='save_changes',
            tipify(actionButton(ns("save_ensemble"), icon("fas fa-save")), "Save the ensemble model")

        )
    )
  })

  output$del_ensemble_button<-renderUI({
    req(length(input$ensemble_models)>0)
    req(input$ensemble_models!="New Ensemble")
    div(class='button_min',
        tipify(actionButton(ns("del_ensemble"), icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt")), "Remove model")

    )
  })

  observeEvent(ignoreInit = T,input$del_ensemble,{
    vals$saved_ensemble[[input$ensemble_models]]<-NULL
  })

  wei_datatype='weitype'

  get_wei<-function(pred_tab,newdata,modelist,obc,en_method,wei_datatype){

    class_whei<-if(modelist[[1]]$modelType=="Classification"){
      getClass_wei(
        pred_tab,
        obc,
        modelist,
        en_method=en_method,
        weitype=wei_datatype,
        newdata=newdata
      )
    } else{
      args<-list(modelist=modelist,newdata=newdata,obc=obc,en_method=en_method,weitype=wei_datatype)
      do.call(getReg_wei,args)
    }
    if(!is.null(class_whei)){
      class_whei<-as.vector(class_whei)
      names(class_whei)<-names(modelist)
      return(class_whei)
    }
  }




  getmodelist0<-reactive({
    result_list<-getmodelist00()
    vals$attributes2<-result_list$attributes2
    vals$choices_models_equals2<-result_list$choices_models_equals2
    vals$box_pool<- result_list$box_pool
    vals$modellist0<-vals$modellist2<-result_list$modellist0

  })

  observeEvent(ignoreInit = T,input$ensemble_models,{
    req(input$ensemble_models!="New Ensemble")
    try( getmodelist())
  })



  observeEvent(ignoreInit = T,input$ensemble_models,{
    req(input$ensemble_models!="New Ensemble")
    em<-get_the_model2()
    modelist=attr(em,"modelist")
    vals$modellist0<-modelist
    vals$modellist2<-modelist

  })
  observe({
    req(input$ensemble_models)

    if(input$ensemble_models!='New Ensemble'){
      em<-get_the_model2()
      active<-em$ensemble_active
      allmodels<-lapply(vals$saved_data[input$data_tosel_ensemble],function(x)get_saved_models(x))[[1]]
      vals$box_pool<-em$box_pool
      vals$modellist0<-vals$modellist2<-allmodels[[em$box_pool]][active]
      listargs<-   em$args_selected_model
      if(length(listargs)>0){
        vals$ensemble_select_from<-listargs$ensemble_select_from
        vals$ensemble_select_from_list<-listargs$ensemble_select_from_list
        vals$ensemble_select_from_id<-listargs$ensemble_select_from_id
        vals$ensemble_select_from_custom<-listargs$ensemble_select_from_custom

      }


    } else{
      result_list<-getmodelist00()
      vals$attributes2<-result_list$attributes2
      vals$choices_models_equals2<-result_list$choices_models_equals2
      vals$box_pool<- result_list$box_pool
      vals$modellist0<-vals$modellist2<-result_list$modellist0

    }



  })






  get_newdata_source<-reactive({
    req(input$ensemble_predtype)
    if(length(input$use_partition)>0){
      if(input$use_partition=="New Data"|input$ensemble_predtype=="use_model"){
        vals$saved_data[[input$data_ensemble_X]]
      } else{
        some_list<-lapply(get_modellist_0(), function(x) rownames(attr(x,"test")))
        test_true<-all(sapply(some_list, function(x){
          identical(sort(x), sort(some_list[[1]]))
        }))
        req(isTRUE(test_true))
        attr(get_modellist_0()[[1]],"test")
      }

    } else{
      vals$saved_data[[input$data_ensemble_X]]

    }

  })


  en_method_label<-reactive({
    if(get_modellist_0()[[1]]$modelType=="Classification"){
      switch(input$en_method,
             'weighted' ="Weighted votes (adaptative)",
             'accu_weighted'="Weighted votes (overall performace)",
             "non-weighted"="Majority votes"
      )
    } else{
      switch(input$en_method,
             'accu_weighted'="Weighted Mean (overall performace)",
             'non-weighted'="Mean"
      )

    }
  })


  weights_from<-reactive({
    if(is.null(input$wei_datatype)){
      return("non-weighted")
    } else{
      switch(input$wei_datatype,
             "test"='New data/validation set',
             "training"="Training data"
      )
    }

  })




  observeEvent(ignoreInit = T,input$run_preds,{
    vals$ensemble_models<-'New Ensemble'
    updatePickerInput(session,"ensemble_models", selected=vals$ensemble_models)
    start<-Sys.time()
    models_grid<-metrics_final<-NULL
    tres<-try({
      data_ensemble_X=input$data_ensemble_X
      if(input$use_partition=="Partition"){
        data_ensemble_X="Partition"
      }
      vals$saved_ensemble[["New Ensemble"]]<-list()
      pool_result<-NULL
      newdata<-get_newdata_source()

      if(input$ensemble_predtype=="use_model"){
        em<-get_usemodel()
        active<-em$ensemble_active
        active_box<-em$box_pool
        allmodels<-lapply(vals$saved_data[input$data_tosel_ensemble],function(x)get_saved_models(x))[[1]]
        modelist=allmodels[[active_box]][colnames(em$tabpred)]
        obc<-NULL
        en_method<-get_en_method(em$en_method_label)
        weitype=get_weights_from(em$weights_from)
        predtab0<-em$predtab0
        predtab<-em$tabpred
        weis<-attr(predtab,"weis")
        final_modelist<-modelist
        en_method_label<-em$en_method_label
        weights_from<-em$weights_from
        predtab0<-predtab<-predcomb_table(modelist,newdata, progress=T)
        attr(predtab0,'weis')<- attr(predtab,'weis')<-weis
        use_model_name<- base::strsplit(input$ensemble_saved,"::")[[1]]
        vals$saved_ensemble[["New Ensemble"]]$use_model<-use_model_name
        use_model_data=c(ensemble_saved=input$ensemble_saved, use.names=T)
        step5=c(obc=NULL, use.names=T)
      } else{

        use_model_data=c(ensemble_saved=NULL, use.names=T)
        modelist<-vals$modellist2<-get_modellist_0()
        if(input$ensemble_predtype=='new_unkn'|!length(names(vals$en_Ydatalists))>0){
          obc=NULL
          step5=c(obc=NULL, use.names=T)
        } else{
          obc= get_OBC()
          step5=c(obc=input$obc, use.names=T)
        }

        en_method=input$en_method
        weitype=input$wei_datatype
        predtab0<-predtab<-predcomb_table(modelist,newdata, progress=F)

        if(en_method!="non-weighted"){
          weis<-get_wei(predtab,newdata,modelist,obc,en_method,weitype)
        } else{
          weis<-rep(1,nrow(predtab))
          names(weis)<-colnames(predtab)
        }

        attr(predtab0,'weis')<- attr(predtab,'weis')<-weis
        if(isTRUE(val_check_pool$df)) {
          en_method=input$en_method
          models<-colnames(predtab)
          models_grid<-do.call("c", lapply(seq_along(models), function(i) combn(models, i, FUN = list)))
          i=20
          n_steps=2
          fun<-ifelse(input$ensemble_metrics%in%c('postResample','postResample + AUC'),"postResample","multiClassSummary")

          auc_go<-ifelse(input$ensemble_metrics%in%c('postResample + AUC','multiClassSummary + AUC'),T,F)

          if(isTRUE(auc_go)){
            n_steps=4
          }

          withProgress(min=1,message=paste0("step 1/",n_steps),{
            res<-ensemble_find(models_grid,modelist,weis,predtab,newdata,obc,  en_method,
                               show_progress=T,
                               pararell=F,
                               session=getDefaultReactiveDomain()
                               #  session=MockShinySession$new()
            )
          })
          df_pred<-data.frame(res, stringsAsFactors = T)
          withProgress(min=1,message=paste0("step 2/",n_steps),{
            metrics_caret<-ensemble_find_metrics(modelist,df_pred,obc,
                                                 show_progress=T,
                                                 pararell=F,
                                                 session=getDefaultReactiveDomain(),
                                                 #  session=MockShinySession$new(),
                                                 fun=fun)
          })
          metrics_final<-data.frame(t(metrics_caret))
          if(modelist[[1]]$modelType=="Classification"){
            if(isTRUE(auc_go)) {

              withProgress(min=1,message=paste0("step 3/",n_steps),{
                tabprob<-ensemble_find_probs(models_grid,modelist,weis,predtab,  show_progress=T, pararell=F, session=getDefaultReactiveDomain())
              })
              withProgress(min=1,message=paste0("step 4/",n_steps),{
                aucs<-ensemble_find_auc(df_pred,tabprob,obc, show_progress=T, pararell=F, session=getDefaultReactiveDomain())
              })

              metrics_final$multi_class_auc<-aucs
            }
          }
        }
        final_modelist<-vals$modellist2
        en_method_label<-en_method_label()
        weights_from<-weights_from()
      }





      ensemble_active<-names(vals$modellist2)
      args<-list(
        predtab=predtab,
        modelist=final_modelist,
        en_method=input$en_method,
        obc=obc,
        weitype=NULL,
        newdata=newdata,
        weis=weis
      )
      pred0<-pred<-do.call(get_ensemble_pred,args)

      pred<-if(args$modelist[[1]]$modelType=="Classification"){
        factor(pred[,1],levels =args$modelist[[1]]$levels)}else{
          pred
        }
      pred<-unlist(pred)

      names(pred)<-rownames(pred0)

      listargs<-list(
        step1=c(data_tosel_ensemble=input$data_tosel_ensemble, use.names=T),
        step2=c(dummy=input$dummy, use.names=T),
        step3=c(ensemble_predtype=input$ensemble_predtype, use.names=T),
        tep3b=use_model_data,
        step4=c(data_ensemble_X=data_ensemble_X, use.names=T),
        step4b<-c(use_partition=input$use_partition, use.names=T),
        step5=step5,
        step6=c(en_method=input$en_method, use.names=T),
        step7=c(wei_datatype=input$wei_datatype, use.names=T),
        step8=c(best_comb=input$best_comb, use.names=T)
      )


      res=try({
        vals$saved_ensemble[["New Ensemble"]]$listargs<- listargs
        vals$saved_ensemble[["New Ensemble"]]$pred_type<- input$ensemble_predtype
        vals$saved_ensemble[["New Ensemble"]]$pool_metrics<-list(
          models_grid,metrics_final
        )
        vals$saved_ensemble[["New Ensemble"]]$predtab0<-predtab0
        vals$saved_ensemble[["New Ensemble"]]$tabpred<-predtab
        vals$saved_ensemble[["New Ensemble"]]$pred<-data.frame(pred)
        vals$saved_ensemble[["New Ensemble"]]$inital_pool<-colnames(predtab0)
        vals$saved_ensemble[["New Ensemble"]]$en_method_label<-en_method_label
        vals$saved_ensemble[["New Ensemble"]]$weights_from<-weights_from
        vals$saved_ensemble[["New Ensemble"]]$newdata<-newdata
        vals$saved_ensemble[["New Ensemble"]]$ens_modeltype<-vals$ens_modeltype
        vals$saved_ensemble[["New Ensemble"]]$ensemble_active<-ensemble_active
        vals$saved_ensemble[["New Ensemble"]]$box_pool<-input$limodels2
        #vals$saved_ensemble<-list_results
        vals$saved_ensemble[["New Ensemble"]]$obc<-obc



      })

      validate(need(class(res)!='try-error','Predicions failded: Check models compatibility against new X data'))



    })



    end<-Sys.time()
    message(end-start)
    vals$run_ensemble_pred<-F
    shinyjs::removeClass('run_preds_id', 'save_changes')
    updateTabsetPanel(session,'ensemble_pages',"page3")
    if(isTRUE(input$best_comb)){
      updateTabsetPanel(session,'Ensemble_tab3',"tab_pool")
    } else{
      updateTabsetPanel(session,'Ensemble_tab3',"tab_model")
    }


  })





  get_predtab<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    predtab<-vals$saved_ensemble[[input$ensemble_models]]$tabpred
    predtab
  })
  get_newdata<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    newdata<-vals$saved_ensemble[[ input$ensemble_models]]$newdata
    #req(is.data.frame(predtab))
    newdata
  })
  get_inter_res0<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    inter_res0<-vals$saved_ensemble[[ input$ensemble_models]]$inter_res0
    inter_res0
  })
  scoreloss_models<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    vals$saved_ensemble[[ input$ensemble_models]]$scoreloss_models
  })
  scoreloss_ensemble<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    aculoss=vals$saved_ensemble[[ input$ensemble_models]]$scoreloss_ensemble
    aculoss
  })

  get_obc<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    obc<-vals$saved_ensemble[[ input$ensemble_models]]$obc
    obc
  })



  observeEvent(ignoreInit = T,input$run_aculoss,{
    get_scoreloss_ensemble()
  })
  getargs_root<-reactive({
    req(input$ensemble_models)
    predtab<-get_predtab()
    obc<-get_obc()
    pred<-get_pred()
    weis=attr(predtab,"weis")
    newdata<-get_newdata()

    modelist<-get_modelist_from_em()
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]




    accu<-postResample(pred,obc)
    m<-modelist[[1]]
    accu<-if(m$modelType=="Classification"){accu["Accuracy"]
    } else{accu['Rsquared']}


    args<-list(
      newdata=newdata,
      obc=obc,
      reps=input$aculoss_rep,
      modelist=modelist,
      weis=weis,
      accu=accu,
      scale=input$en_scale,
      en_method=input$en_method,
      top.features=ncol(newdata),
      root=NULL,
      inter_method="Paired",
      progress=T,
      ms=NULL,
      type="modelist",
      feaimp=F
    )
    args
  })
  get_scoreloss_ensemble<-reactive({
    args<-getargs_root()
    vals$ensemble_args<-args
    acc<-do.call(getroot,args)

    rep200<-getsig_aculoss(acc,accu=NULL,sig=input$aculoss_sig)
    vals$saved_ensemble[[ input$ensemble_models]]$ensemble_resampling<-acc
    vals$saved_ensemble[[ input$ensemble_models]]$scoreloss_ensemble<-rep200
    updateTabsetPanel(session,"ensemble_tab","tab_perf")


  })
  get_scoreloss_models<-reactive({
    predtab<-get_predtab()
    modelist<-get_modelist_from_em()
    newdata<-get_newdata()
    class=which(colnames(modelist[[1]]$trainingData)==".outcome")
    newdata<-newdata[,colnames(modelist[[1]]$trainingData)[-1]]
    obc<-get_obc()

    weis=rep(1,length(modelist))
    accu<-get_model_performaces()


    withProgress(min=0,max=length(modelist), message="Calculating Permutation Importance ...",{
      resultnovo<-lapply(1:length(modelist),function(i){
        m<-modelist[[i]]
        ac<-accu[[i]]
        acc<-getroot(newdata=newdata,
                     obc=obc,
                     reps=input$aculoss_rep,
                     modelist=modelist,
                     weis=weis,
                     accu=ac,
                     scale=input$en_scale,
                     en_method=input$en_method,
                     top.features=ncol(newdata),
                     root=NULL,
                     inter_method="Paired",
                     progress=T,
                     ms=m,
                     type="model",
                     feaimp=F)
        res<-getsig_aculoss(acc, NULL,sig=input$aculoss_sig)
        incProgress(1, message=paste0('model:',names(modelist)[i]))
        list(res, acc)
      })

    })
    result<-lapply(resultnovo, function(x) x[[1]])
    acc<-lapply(resultnovo, function(x) x[[2]])
    sigvars<-lapply(resultnovo, function(x) attr(x[[1]],"sigvars"))
    result<-unlist(result)
    attr(result,"sigvars")<-sigvars


    vals$saved_ensemble[[ input$ensemble_models]]$models_resampling<-acc
    vals$saved_ensemble[[ input$ensemble_models]]$scoreloss_models<-result
    updateTabsetPanel(session,"ensemble_tab","tab_pred")
    updateTabsetPanel(session,"Ensemble_tab2","tab_obserr")
  })

  get_OBC<-reactive({
    if(length(input$use_partition)>0){
      if(input$use_partition=="New Data"){
        get_OBC_obs()
      } else{
        some_list<-lapply(get_modellist_0(), function(x) rownames(attr(x,"test")))
        test_true<-all(sapply(some_list, function(x){
          identical(sort(x), sort(some_list[[1]]))
        }))
        req(isTRUE(test_true))
        attr(get_modellist_0()[[1]],"sup_test")
      }

    } else{
      get_OBC_obs()

    }
  })

  get_OBC_obs<-reactive({
    if(length(input$obc)>0){
      modelist<-get_modellist_0()
      data<-vals$saved_data[[input$obc]]
      var<-attr(modelist[[1]],"supervisor")
      if(modelist[[1]]$modelType=="Regression"){
        req(var%in%colnames(data))
        obc<-data[,var]
        names(obc)<-rownames(data)
      } else {
        factors<-attr(data,"factors")
        req(var%in%colnames(factors))
        obc<-factors[,var]
        names(obc)<-rownames(factors)
      }
      obc

    } else{NULL}

  })



  get_pred<-reactive({
    em<-get_the_model2()
    em$pred


  })

  observeEvent(list(input$en_method, input$wei_datatype),{
    req(input$en_method)
    req(input$wei_datatype)

    get_OBC()

  })

  getmodels<-reactive({
    pic<-input$limodels2
    req(length(vals$choices_models_equals2)>0)
    choices<-vals$choices_models_equals2
    vals$ens_modeltype<-which(choices==input$limodels2)



    modellist2<-vals$box_pool[[vals$ens_modeltype]][vals$ensemble_active]
    vals$modellist0<-vals$modellist2<-modellist2
  })


  getmodelist<-reactive({
    req(input$data_ensemble_X)

    data<-getdata_tosel_ensemble()
    choices<-list(
      if(check_model_caret(data,"rf")){"rf"},
      if(check_model_caret(data,"nb")){"nb"},
      if(check_model_caret(data,"svm")){"svm"},
      if(check_model_caret(data,"knn")){"knn"},
      if(check_model_caret(data,"sgboost")){"sgboost"},
      if(check_model_caret(data,"xyf")){"xyf"}
    )
    attributes<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]
    vals$attributes2<-attributes
    validate(need(length(vals$attributes2)>0,"No models saved in selected datalist"))
    limodels2<-lapply(attributes,function(x) attr(data,x))
    names0<-unlist(lapply(limodels2,names))
    limodels2<-flattenlist(limodels2)
    linames<-unlist(lapply(attributes,function(x) names(attr(data,x))))
    names(limodels2)<-linames
    ensemble_active<-vals$saved_ensemble[[input$ensemble_models]]$ensemble_active
    req(length(ensemble_active)>0)
    vals$modellist0<-vals$modellist2<-limodels2[ensemble_active]
    if(input$ensemble_models=="New Ensemble"){
      vals$saved_ensemble[[input$ensemble_models]]$box_pool<-input$limodels2}
  })










  output$teste_comb<-renderUI({})



  ensemble_save<-reactive({
    m<-vals$saved_ensemble[["New Ensemble"]]
    attr(m,"training_datalist")<-input$data_tosel_ensemble
    name<-if(input$hand_save=="create"){
      input$newdatalist}else{input$over_datalist}
    vals$saved_ensemble[[name]]<-m
    vals$saved_ensemble[["New Ensemble"]]<-NULL
    if(input$hand_save=="create"){
      vals$ensemble_models<-input$newdatalist}else{vals$ensemble_models<-input$over_datalist}

  })


  observeEvent(ignoreInit = T,input$save_ensemble,{
    vals$hand_save<- "Save results in"
    showModal(module_combb())
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
      "Save results in"={ensemble_save()},
      "Create Datalist: ensemble predictions"={combb_create_pred()},
      "Create Datalist: model predictions"={combb_create_pred()},
      "resample_create"={resample_create()},
      "Create Datalist: comb training errors -obs"=comb_create_training_errors(),

    )
    removeModal()

  })
  module_combb <- function() {
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



  name_save_ensemble<-reactive({
    name0<-paste0("Ensemble")
    names<-names(vals$saved_ensemble)
    if(length(names)>0){
      name1<-make.unique(c(names,name0), sep="_")
      name1[length(names)+1]
    } else{
      name1<-name0
    }


  })
  resample_create_name<-reactive({    bag<-1
  var<-attr(get_modelist_from_em()[[1]],"supervisor")
  name0<-paste("comb",var,"qclass", sep="_")
  name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
  name1[length(vals$saved_data)+1]


  })
  name_combb_pred<-reactive({
    name0<- "Ensemble predictions"
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]

  })
  name_comb_train_errors<-reactive({
    name0<-paste0("Ensemble errors")
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]

  })


  get_the_model<-reactive({
    req(input$data_ensemble_X)
    req(input$ensemble_models)
    req(length(vals$saved_ensemble)>0)
    res<-c(vals$saved_ensemble)[[input$ensemble_models]]
    res
  })


  observeEvent(ignoreInit = T,input$limodels2,{
    em<-get_the_model()
    active<-em$ensemble_active
    active_box<-ifelse(length(em$box_pool)==1, em$box_pool, input$limodels2)
    allmodels<-lapply(vals$saved_data[input$data_tosel_ensemble],function(x)get_saved_models(x))[[1]]
    req(input$ensemble_models)
    req(isFALSE(any(em$ensemble_active%in%names(allmodels[[active_box]]))))
    # updatePickerInput(session,"ensemble_models",selected="New Ensemble")
    if(input$ensemble_models=="New Ensemble"){
      vals$saved_ensemble[["New Ensemble"]]$ensemble_active<-input$limodels2}
  })




  get_the_model2<-reactive({
    req(input$data_tosel_ensemble)
    data_o<-vals$saved_data[[input$data_tosel_ensemble]]
    em<-get_the_model()
    attr(em,"modelist")
    if(class(em)[1]!='train'){
      active<-if(length(em$box_pool)==1){
        em$box_pool
      } else{
        input$limodels2
      }
      # x<-allmodels[[1]]
      allmodels<-lapply(vals$saved_data[input$data_tosel_ensemble],function(x)get_saved_models(x))
      em_active<-em$ensemble_active
      if(is.null(em_active)){
        em_active<-colnames(em$tabpred)
      }
      req(!is.null(em_active))

      allmodels2<-lapply(allmodels,function(x){
        check<-any(em_active%in%names( x[[active]]))
        if(isFALSE(check)){
          req(input$ensemble_models=="New Ensemble")

        } else{
          x[[active]][em_active]
        }

      })
      #names(allmodels)
      # x<-allmodels[[1]]
      # names(x)
      #em$box_pool

      pic<-names(which(unlist(lapply(allmodels2, function(x){
        sum(em_active%in%unlist(names(x)))==length(em_active)
      }))))
      req(length(pic)>0)
      attr(em,"modelist")<-   allmodels2[[pic]]
      em$modelType<-"Ensemble"
    } else{
      attr(em,"modelist")<-  list(em)
    }


    attr(em,"en_method")<-input$en_method
    em


    })




  observe({
    req(length(vals$cur_limodels2)>0)
    req(length(get_box_pool())>0)
    req(!vals$cur_limodels2%in%names(get_box_pool()))
    vals$cur_limodels2<-NULL
  })



  getmodel_ui<-reactive({
    vals$box_pool<-NULL

    data<-getdata_tosel_ensemble()
    if(length(data)>0){
      choices<-list(
        if(check_model_caret(data,"rf")){"rf"},
        if(check_model_caret(data,"nb")){"nb"},
        if(check_model_caret(data,"svm")){"svm"},
        if(check_model_caret(data,"knn")){"knn"},
        if(check_model_caret(data,"sgboost")){"sgboost"},
        if(check_model_caret(data,"xyf")){"xyf"}
      )

      attributes<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]

      validate(need(length(attributes)>0,"No models saved in selected datalist"))

      if(length(attributes)>0) {
        output$step2_ensemble_error<-renderUI({NULL})
        limodels2<-lapply(attributes,function(x) attr(data,x))
        names0<-unlist(lapply(limodels2,names))
        limodels2<-flattenlist(limodels2)


        linames<-unlist(lapply(attributes,function(x) names(attr(data,x))))
        resmethos<-unlist(lapply(limodels2,function(x) paste0("Type:",x$modelType,"; ")))
        x<-limodels2[[1]]

        ressup<-unlist(lapply(limodels2,function(x) attr(x,"supervisor")))

        dftype<-data.frame(type=as.vector(resmethos),sup=as.vector(ressup))
        resmethos<-as.vector(apply(dftype,1,function(x) {
          paste0("Y:",x,collapse="")
        }))


        #pic_not<-which(unlist(lapply(limodels2,function(x) is.null(x$resample))))
        #if(length(pic_not)>0){limodels2<-limodels2[-pic_not]}
        liequals<-split(limodels2,as.vector(unlist(lapply(limodels2,function(x)nrow(x$resample)))))
        liequals<-lapply(liequals,function(x) names(x))
        box_pool<-split(limodels2,resmethos)
        listanames<-split(names0,resmethos)
        for(i in 1:length(box_pool)){
          names(box_pool[[i]])<-listanames[[i]]
        }
        vals$choices_models_equals2<-names(box_pool)
        vals$box_pool<-box_pool
        vals$attributes2<-attributes

      } else{
        output$step2_ensemble_error<-renderUI({
          em("No saved models found in the selected datalist.")
        })
      }
    }

  })





  getmodelist00<-reactive({
    req(input$data_tosel_ensemble)
    req(input$limodels2)
    req(!is.null(get_box_pool()))
    req(is.list(get_box_pool()))
    # data<-getdata_tosel_ensemble()
    data<-vals$saved_data[[input$data_tosel_ensemble]]
    choices<-list(
      if(check_model_caret(data,"rf")){"rf"},
      if(check_model_caret(data,"nb")){"nb"},
      if(check_model_caret(data,"svm")){"svm"},
      if(check_model_caret(data,"knn")){"knn"},
      if(check_model_caret(data,"sgboost")){"sgboost"},
      if(check_model_caret(data,"xyf")){"xyf"}
    )

    attributes<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]

    validate(need(length(attributes)>0,"No models saved in selected datalist"))
    limodels2<-lapply(attributes,function(x) attr(data,x))
    names0<-unlist(lapply(limodels2,names))
    limodels2<-flattenlist(limodels2)
    pic<-which(sapply(limodels2,function(x) class(x)[1])=="train")
    limodels2<-limodels2[pic]
    names0<-gsub(".*\\.","",names(limodels2))

    linames<-names0

    linames<-names0
    resmethos<-unlist(lapply(limodels2,function(x) paste0("Type:",x$modelType,"; ")))
    x<-limodels2[[1]]

    ressup<-unlist(lapply(limodels2,function(x) attr(x,"supervisor")))

    dftype<-data.frame(type=as.vector(resmethos),sup=as.vector(ressup))
    resmethos<-as.vector(apply(dftype,1,function(x) {
      paste0("Y:",x,collapse="")
    }))


    #pic_not<-which(unlist(lapply(limodels2,function(x) is.null(x$resample))))
    #if(length(pic_not)>0){limodels2<-limodels2[-pic_not]}
    liequals<-split(limodels2,as.vector(unlist(lapply(limodels2,function(x)nrow(x$resample)))))
    liequals<-lapply(liequals,function(x) names(x))
    box_pool<-split(limodels2,resmethos)
    listanames<-split(names0,resmethos)
    for(i in 1:length(box_pool)){
      names(box_pool[[i]])<-listanames[[i]]
    }


    result_list<-list(attributes2=attributes,
                      choices_models_equals2=names(box_pool),
                      box_pool=box_pool,
                      modellist0=get_box_pool()[[input$limodels2]][models_selected()])
    return(result_list)

  })

  get_modellist_0<-reactive({
    req(input$limodels2)
    get_box_pool()[[input$limodels2]][models_selected()]
  })

  get_modelist_from_em<-reactive({
    em<-get_the_model2()
    modelist=attr(em,"modelist")
    modelist
  })







  observeEvent(ignoreInit = T,input$save_selected_ensemble,{
    args_selected_model<-list(
      ensemble_select_from=input$ensemble_select_from,
      ensemble_select_from_list=input$ensemble_select_from_list,
      ensemble_select_from_id=input$ensemble_select_from_id,
      ensemble_select_from_custom=input$ensemble_select_from_custom
    )


    req(length(get_selected_ensemble())>0)
    em<-get_the_model2()
    req(length(em$pool_metrics[[1]])>0)

    models_grid<-  em$pool_metrics[[1]]
    predtab<-em$predtab0
    weis<-attr(predtab,"wei")
    best_model_name<-get_selected_ensemble()
    predtab<-predtab[,best_model_name,drop=F]
    newdata<-em$newdata
    obc<-em$obc
    attr(predtab,'weis')<-weis[best_model_name]

    active<-em$ensemble_active
    active_box<-ifelse(length(em$box_pool)==1, em$box_pool, input$limodels2)
    allmodels<-lapply(vals$saved_data[input$data_tosel_ensemble],function(x)get_saved_models(x))[[1]]


    vals$modellist2<- allmodels[[active_box]][best_model_name]
    ensemble_active<-colnames(predtab)
    vals$saved_ensemble[[input$ensemble_models]]$tabpred<-predtab
    vals$saved_ensemble[[input$ensemble_models]]$ensemble_active<-ensemble_active
    vals$saved_ensemble[[input$ensemble_models]]$best_model_id<-as.numeric(ensemble_selected())
    vals$saved_ensemble[[input$ensemble_models]]$args_selected_model<-args_selected_model
    args<-list(
      predtab=predtab,
      modelist=vals$modellist2,
      en_method=input$en_method,
      obc=obc,
      weitype=NULL,
      newdata=newdata,
      weis=attr(predtab,'weis')
    )
    pred0<-pred<-do.call(get_ensemble_pred,args)
    if(is.factor(em$obc)){
      pred[,1]<-factor(pred[,1],levels=levels(em$obc))
    }

    vals$saved_ensemble[[input$ensemble_models]]$pred<-pred

    beep(10)

  })






}

#input$ensemble_select_from
#input$ensemble_select_from_list
#input$ensemble_select_from_custom
#input$ensemble_select_from_id
