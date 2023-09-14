

## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.

#' @export
module_ui_sgboost <- function(id){

  ns <- NS(id)
  tagList(
    #inline( actionButton(ns("teste_comb"),"SAVE")),
    uiOutput(ns("sgboost_panels"))
  )

}


#' @export
module_server_sgboost <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns

  output$sgboost_panels<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    column(12,style="background: white",
           uiOutput(ns("sgboost_inputs")),
           uiOutput(ns("war_sgboost")),
           column(12,
                  tabsetPanel(id = ns("sgboost_tab"),
                              tabPanel(strong("1. Training"),value='sgboost_tab1',
                                       uiOutput(ns("sgboost_params"))),
                              tabPanel(strong("2. Results"),value='sgboost_tab2',
                                uiOutput(ns("results_sgboost"))),
                              tabPanel(strong("3. Predict"),value='sgboost_tab3',
                                uiOutput(ns("predict_sgboost")))
                  )
           )

    )
  })
  insert_sgboost_resutls<-reactiveValues(df=F)




  output$predict_sgboost<-renderUI({
    column(12,style="background: white", uiOutput(ns("sgboost_pred_type")),
           tabsetPanel(id="predsgboost_tab",
                       tabPanel(
                         strong("3.1. Results"), value="predsgboost_tab01",
                         fluidRow(style="background: white",
                                  uiOutput(ns("sgboost_tab3_1")))),
                       tabPanel(
                         strong("3.2. Performace"),
                         fluidRow(style="background: white",
                                  uiOutput(ns("sgboost_tab3_2")))
                       )


           )
    )
  })
  ##
  output$sgboost_tab3_2<-renderUI({
    sup_test<-attr(vals$sgboost_results,"supervisor")
    supname<-paste0("Observed Y [",sup_test,"]")
    lab1<-span("+",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
               span(supname))

    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            pickerInput(ns("predsgboost_newY"),
                        lab1,names(vals$saved_data[getobssgboost()])),
            uiOutput(ns("sgboost_pred_pal")),
            numericInput(ns("round_pred_sgboost"),"+ Round",3,step=1),
            actionLink(ns("down_metrics"),span("+ Donwload Metrics",icon("fas fa-table"))),
            uiOutput(ns("down_cm_btn")),

        )),
      mainPanel(
        fluidRow(
          column(3,style="margin: 0px; padding: 0px",
                 inline(DT::dataTableOutput(ns('sgboost_tab_errors0_test')))
          ),
          column(9,uiOutput(ns("confusion_matrix_sgboost"))
          )
        )
      )
    )
  })
  get_sgboost_prederrors<-reactive({
    req(length(input$sgboostpred_which)>0)

    m<-vals$sgboost_results
    pred<-pred_sgboost()[,1]
    # x_t<-attr(m,"test")
    obs<-if(input$sgboostpred_which=="Partition"){
      attr(m,"sup_test")} else{
        req(length(input$predsgboost_newY)>0)

        if(m$modelType=="Classification"){
          req(length(attr(m,"supervisor"))>0)
          req(length(input$predsgboost_newY)>0)
          req(input$predsgboost_newY%in%names(vals$saved_data))
          sup<-attr(m,"supervisor")
          factors<-attr(vals$saved_data[[input$predsgboost_newY]],"factors")
          req(sup%in%colnames(factors))
          factors[,sup]} else{
            vals$saved_data[[input$predsgboost_newY]][,attr(m,"supervisor")]
          }
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })
  test_sgboost<-reactive({
    req(input$sgboostpred_which)
    m<-vals$sgboost_results
    pred_tab<-if(input$sgboostpred_which=="Partition"){attr(m,"test")} else if(input$sgboostpred_which=="Datalist"){
      req(input$predsgboost_new)
      pred_tab<-vals$saved_data[[input$predsgboost_new]]
    }
    model_data<-pred_tab
    req(sum(colnames(pred_tab)%in%colnames(getdata_model(m)))==ncol(pred_tab))
    pred_tab

  })
  output$sgboost_tab_errors0_test<-DT::renderDataTable({
    req(input$round_pred_sgboost)
    table<-pred_test_sgboost()
    vals$sup_metrics<-table<-round(table,input$round_pred_sgboost)
    #colnames(table)<-NULL
    DT::datatable(table,
                  options=list(rownames=T,info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'),colnames=c(""))
  })
  output$confusion_sgboost_pred<-renderUI({
    req(vals$sgboost_results$modelType=="Classification")

    conf<-get_cm_pred()
    column(12,
           renderPlot({
             res<-plotCM(conf/sum(conf)*100, input$sgboostpalette_pred,  newcolhabs=vals$newcolhabs,round_cm=input$round_pred_sgboost,title=input$sgboost_cmpred_title)
             vals$sgboost_cm_pred<-res
             res
           }))

  })
  output$confusion_sgboost2_pred<-renderPrint({
    req(vals$sgboost_results$modelType=="Classification")
    res<-confusionMatrix(get_cm_pred())
    vals$sgboost_cm_test<-  as.data.frame.matrix(res$table)
    res
  })
  output$sgboost_pred_type<-renderUI({
    choices=if(length(attr(vals$sgboost_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(class="well2",style="padding: 10px",
        div(class="map_control_style2",style="color: #05668D",
            inline(radioButtons(ns("sgboostpred_which"),strong("New data (X):"),choices=choices,inline=T)), inline(uiOutput(ns("datalist_pred_sgboost")))
        )
    )
  })
  output$datalist_pred_sgboost<-renderUI({
    req(input$sgboostpred_which =='Datalist')
    div(
      pickerInput(ns("predsgboost_new"),"->",names(vals$saved_data[getnewdatasgboost()]), width = '300px', selected=vals$predsgboost_new)
    )
  })
  pred_test_sgboost<-reactive({
    obs<-get_sgboost_prederrors()$obs
    pred<-get_sgboost_prederrors()$pred
    m<-vals$sgboost_results

    table<-stats_ml(obs,pred,m)


    table
  })

  output$confusion_matrix_sgboost<-renderUI({
    req(vals$sgboost_results$modelType=="Classification")
    div(

      style="overflow-x: scroll",
      uiOutput(ns("confusion_sgboost_pred")),
      verbatimTextOutput(ns("confusion_sgboost2_pred"))

    )
  })
  gettile_cmpred<-reactive({
    req(input$sgboostpred_which)
    ifelse(input$sgboostpred_which=="Partition","Test","Confusion Matrix")
  })
  output$sgboost_cmpred_title<-renderUI({
    textInput(ns("sgboost_cmpred_title"),"+ CM Title",gettile_cmpred())
  })
  output$sgboost_pred_pal<-renderUI({
    req(vals$sgboost_results$modelType=="Classification")
    lab2<-span("+",tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"),"Palette")
    div(
      uiOutput(ns('sgboost_cmpred_title')),
      pickerInput(inputId = ns("sgboostpalette_pred"),
                  label = lab2,
                  choices =     vals$colors_img$val,
                  choicesOpt = list(content =     vals$colors_img$img),options=list(container="body"))
    )
  })
  observeEvent(ignoreInit = T,input$down_metrics,{
    vals$hand_down<-"download metrics"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })
  output$down_cm_btn<-renderUI({
    req(vals$sgboost_results$modelType=="Classification")

    div(
      div(actionLink(ns("dowcenter_cmsgboost_pred"),span("+ Download Confusion",icon("fas fa-table")))),
      div(actionLink(ns("downp_cmsgboost_pred"),span("+ Donwload plot"))
      )
    )

  })
  ##


  observeEvent(ignoreInit = T,input$teste_comb,{
    savereac()
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
    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')

  })
  predall_sgboost<-reactive({
    validate(need(!anyNA(test_sgboost()),"NAs not allowed in the prediction Datalist"))
    m<-vals$sgboost_results
    #factors<-attr(vals$saved_data[[input$predsgboost_new]],"factors")
    #m<-readRDS("m.rds")
    ctr<-data.frame(id=rownames(m$trainingData))
    rownames(ctr)<-1:nrow(m$trainingData)
    res0<-lapply(split(m$pred$pred,m$pred$rowIndex),function(x)data.frame(x))

    res<-t(do.call(cbind,res0))
    rownames(res)<-names(res0)
    rownames(res)<-ctr[rownames(res),]

    #pred_tab<-readRDS("pred_tab.rds")
    # m$control
    #predict(m,predict.all=T)

    pred <- res
    pred
  })
  sgboost_observed2<-reactive({
    req(input$sgboostpred_which)


    obs_data<-if(input$sgboostpred_which=="Datalist"){
      if(vals$sgboost_results$modelType=="Classification"){
        factors<-attr(vals$saved_data[[input$predsgboost_newY_tree]],'factors')
      } else {
        factors<-vals$saved_data[[input$predsgboost_newY_tree]]}

      res<-factors[,attr(vals$sgboost_results,"supervisor")]
      names(res)<-rownames(factors[attr(vals$sgboost_results,"supervisor")])
      res
    } else{
      attr(vals$sgboost_results,"sup_test")
    }
    obs_data
  })
  get_qclass<-reactive({
    m<-vals$sgboost_results
    req(m$modelType=="Regression")
    pred<-predall_sgboost()
    model_data<-test_sgboost()
    obs_data<-sgboost_observed2()
    obs_data<-data.frame(obs_data)
    ref1<-rownames(model_data)
    ref2<-rownames(obs_data)
    ref1<-ref2
    lo<-split(obs_data,ref2)

    pred_interval=input$q_class_val
    pred.sgboost.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )


    lpred<-split(pred,ref1)
    lp<-split(pred.sgboost.int,ref1)
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
    vals$sgboost_treetest<-res
    vals$sgboost_treetest
  })
  output$sgboost_tree_show<-renderUI({
    val=if(vals$sgboost_results$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$sgboost_tree_pred<-renderUI({
    req(input$nhist_tree)
    data=t(predall_sgboost())
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))

    div(
      inline(pickerInput(ns('splitdata_trees'),"Observations:", choices=options_show, width="200px")),
      actionButton(ns('downp_summ_trees'),tipify(icon("fas fa-download"), "Download Plot"), style="button_active")
    )
  })
  output$qclass_legend<-renderUI({
    req(vals$sgboost_results$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),'Mean predictions')

        ))
  })
  output$plot_forest<-renderUI({
    req(vals$sgboost_results$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({
    req(input$qclass_width)
    req(input$qclass_height)
    div(style="background: white",
        inline(uiOutput(ns("sgboost_tree_show"))),
        inline(uiOutput(ns("sgboost_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))

  })
  output$forest_byclass<-renderUI({
    req(vals$sgboost_results$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })
  get_forest_class<-reactive({
    req(input$predsgboost_newY_tree)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$sgboost_results
    req(m$modelType=="Classification")
    pred<-predall_sgboost()
    pred.ind <- pred
    model_data<-test_sgboost()
    obs_data<-sgboost_observed2()
    obs_data<-data.frame(obs_data)
    data=t(pred.ind)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs=obs_data[ colnames(data),, drop=F]
    pred_fac<-do.call(data.frame,lapply(data.frame(data), function(x) factor(x, labels=m$levels, levels = m$levels)))
    res<-data.frame(do.call(rbind,lapply(pred_fac, function(x) table(x))))
    rownames(res)<-colnames(data)
    colnames(res)<-m$levels
    attr(res,'ntree')<-ncol(pred.ind)
    attr(res,'obs')<-obs
    vals$get_forest_class<-res_forest<-res
    vals$get_forest_class
  })
  output$pick_sgboostobs_pred<-renderUI({
    req(input$sgboostpred_which=="Datalist")
    sup_test<-attr(vals$sgboost_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("obs_cm_pred"),
                       NULL,names(vals$saved_data[getobssgboost()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })
  output$predsgboost_newY_ref<-renderUI({
    req(input$predsgboost_newY_tree)
    req(input$sgboostpred_which)
    req(vals$sgboost_results$modelType=="Regression")
    factors<-attr(vals$saved_data[[input$predsgboost_newY_tree]],"factors")
    choices=if(input$sgboostpred_which=="Datalist"){
      c('rownames',colnames(factors))} else{
        'rownames'
      }
    div(
      span("+ ref",inline(pickerInput(ns('sgboost_reftest'),NULL,choices, width="200px", selected=vals$sgboost_reftest)))

    )})
  output$forest_margin<-renderUI({
    req(vals$sgboost_results$modelType=="Regression")
    div(
      span("+ Margin:",
           inline(
             numericInput(ns("q_class_val"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    )
  })
  output$forest_col<-renderUI({
    req(vals$sgboost_results$modelType=="Classification")
    div(class="palette",
        span("+ Palette",
             inline(
               pickerInput(inputId = ns("forest_col"),
                           label = NULL,
                           choices =     vals$colors_img$val[getgrad_col()],
                           choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"), width="75px")
             )
        )
    )
  })
  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })
  output$pick_sgboostobs<-renderUI({
    sup_test<-attr(vals$sgboost_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("predsgboost_newY_tree"),
                       NULL,names(vals$saved_data[getobssgboost()]), width="200px",selected=vals$predsgboost_newY_tree
           ))
    )
  })
  output$resample_out<-renderUI({
    m<-vals$sgboost_results
    validate(need(m$control$savePredictions=="all","This model was generated in an earlier version of iMESc. Please retrain the data to enable this functionality."))
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("qclass_side"))
      ),
      mainPanel(
        uiOutput(ns("qclass_out"))
      )
    )
  })
  output$qclass_side<-renderUI({

    div(
      class="map_control_style",style="color: #05668D",
      uiOutput(ns("pick_sgboostobs")),
      uiOutput(ns("predsgboost_newY_ref")),
      uiOutput(ns('forest_margin')),
      uiOutput(ns('forest_col')),

      div(
        span("+ Size:",
             inline(
               numericInput(ns("qclass_sizeplot"),NULL, value=1.5, width="75px")
             )
        )
      ),
      div(
        span("+ Width",
             inline(
               numericInput(ns("qclass_width"),NULL, value=800, width="75px")
             )

        )),
      div(
        span("+ Height",
             inline(
               numericInput(ns("qclass_height"),NULL, value=700, width="75px")
             )
        )
      ),

      uiOutput(ns("forest_saves"))


    )
  })
  output$forest_saves<-renderUI({
    req(vals$sgboost_results$modelType=="Regression")
    div(
      div(actionLink(ns('resamp_pred_gdownp'),"Download table")),
      div( actionLink(ns('resamp_pred_gcreate'),"Create Datalist"))
    )
  })
  output$summ_trees<-renderPlot({
    req(vals$sgboost_results$modelType=="Regression")

    get_qclass()
    req(input$q_class_val)

    req(input$splitdata_trees)
    req(input$nhist_tree)
    pred<-predall_sgboost()
    data=t(pred)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs_data<-sgboost_observed2()
    obs_data<-data.frame(obs_data)
    obs=obs_data[ colnames(data),]

    pred <- pred
    pred_interval=input$q_class_val
    q_class=vals$sgboost_treetest[colnames(data),]
    #data<-readRDS('data.rds')
    #pred_interval<-readRDS('pred_interval.rds')
    #q_class<-readRDS('q_class.rds')
    #obs<-readRDS('obs.rds')
    #beep(1)
    str_numerics3(numerics=data,
                  obs=obs_data[ colnames(data),],
                  pred_interval=pred_interval,
                  q_class=q_class,
                  cex=input$qclass_sizeplot)
    vals$vartrees<-recordPlot()
  })
  output$forestbyclass<- renderPlot({
    res_forest<-get_forest_class()

    plot_florest_class(res_forest,vals$newcolhabs, palette=input$forest_col, ylab="Resample")
  })
  observeEvent(ignoreInit = T,input$predsgboost_newY_tree,{
    vals$predsgboost_newY_tree<-input$predsgboost_newY_tree
  })
  observeEvent(ignoreInit = T,input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_sgboost())

  })
  observeEvent(ignoreInit = T,input$resamp_pred_gdownp,{

    vals$hand_down<-"sgboost_qclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })




  observeEvent(ignoreInit = T,input$predsgboost_new,{
    vals$predsgboost_new<-input$predsgboost_new
  })





  observeEvent(ignoreInit = T,input$dowcenter_cmsgboost_pred,{
    vals$hand_down<-"GBM_cm_test"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })





  output$sgboost_tab3_1<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            class="map_control_style",style="color: #05668D",
            div(
              tipify(
                actionLink(
                  ns('sgboost_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                ),
                "Create a datalist with the GBM predictions", options=list(container="body")
              )
            ),
            div(
              tipify(
                actionLink(
                  ns('down_sgboost_tab3_1'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                ),
                "Download Table", options=list(container="body")
              )
            )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('sgboosttab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('sgboosttab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          inline(DT::dataTableOutput(ns('sgboosttab_pred')))
        )
      )
    )
  })
  output$sgboosttab_pred<-DT::renderDataTable({
    table<-vals$sgboosttab_pred<-pred_sgboost()
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))

  }, rownames = TRUE,class ='compact cell-border')


  output$sgboost_inputs <- renderUI({
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",
        if(input$sgboost_tab=="sgboost_tab1"){
          div(
            div(class='well3',
                strong("Type:"),inline(radioButtons(ns('sgboost_type'),NULL,choices=c("Classification","Regression"),inline =T, selected=vals$cur_model_type))
            ),
            div(class="well3",
                span(strong("X:"),
                     inline(uiOutput(ns('data_sgboostX_out')))
                )
            ),
            div(class="well3",
                span(strong("Y:"),
                     inline(
                       uiOutput(ns("data_sgboostY_out"))
                     ),
                     "::",
                     inline(uiOutput(ns('sgboost_Y'))),

                     inline(uiOutput(ns("sgboost_partition"))),
                     "::",
                     inline(uiOutput(ns("sgboost_test_ref")))
                )
            )


          )
        } else {
          column(12,
                 inline( uiOutput(ns('data_sgboostX_out'))),
                 inline(uiOutput(ns("sgboost_results_menu"))),
                 inline(uiOutput(ns("savesgboost_button"))),
                 inline(uiOutput(ns("rmsgboost_button"))),
          )
        }
    )
  })
  output$data_sgboostY_out<-renderUI({
    div(
      div("Datalist:"),
      div(pickerInput(ns("data_sgboostY"),NULL,choices =  names(vals$saved_data), width="150px", selected=vals$cur_dataY))
    )
  })
  output$sgboost_Y <- renderUI({
    req(input$data_sgboostY)
    req(input$sgboost_type)

    data <- vals$saved_data[[input$data_sgboostY]]
    labels <- attr(data,"factors")
    choices<-if(input$sgboost_type=="Classification"){rev(colnames(labels))} else{
      colnames(data)
    }

    div(well="class2",
        div("Variable"),
        div(tipify(pickerInput(
          ns("sgboost_sup"),
          NULL,
          choices =choices ,
          width="150px", selected=vals$cur_response
        ),"The response vector"))
    )

  })
  output$sgboost_partition<-renderUI({
    req(input$data_sgboostX)

    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("sgboost_test_partition"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_sgboostY]],"factors"))), width="150px", selected=vals$cur_test_partition))
    )
  })
  output$sgboost_test_ref<-renderUI({
    req(input$sgboost_test_partition!="None")
    req(input$data_sgboostY)

    fac<-attr(vals$saved_data[[input$data_sgboostY]],"factors")[,input$sgboost_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_sgboost"),NULL, choices=choices, width="200px", selected=vals$cur_testdata)
    )
  })
  output$data_sgboostX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_sgboostX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
        )
      ),      inline(uiOutput(ns("saved_sgboosts")))

    )
  })

  output$saved_sgboosts<-renderUI({

    req(input$data_sgboostX)
    names_sgboost<-names(attr(vals$saved_data[[input$data_sgboostX]],"sgboost"))
    req(length(names_sgboost)>0)
    pic<-which(names_sgboost%in%"new GBM (unsaved)")
    if(length(pic)>0){
      names_sgboost<-names_sgboost[-pic]
    }
    req(length(names_sgboost)>0)
    div(class="saved_models",
        icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names_sgboost)), "saved model(s)")
  })




  output$sgboost_default<-renderUI({
    choices<-c("user-defined","grid","random")
    div(
      div(
        tipify(icon("fas fa-question-circle"),"Tuning search", options = list(container="body")),
        "+ search:",
        inline(
          pickerInput(ns("sgboost_search"), NULL, choices = choices, width="110px", selected=vals$cur_sgboost_search)
        )
      ),
      uiOutput(ns("sgboost_search_length"))
    )
  })
  observeEvent(ignoreInit = T,input$sgboost_search,{
    vals$cur_sgboost_search<-input$sgboost_search
  })
  output$sgboost_search_length<-renderUI({
    req(input$sgboost_search!='user-defined')
    if(is.null(vals$cur_sgboost_tuneLength)){vals$cur_sgboost_tuneLength<-5}

    div(
      tipify(icon("fas fa-question-circle"),"the maximum number of different combinations of hyperparameters to evaluate during the tuning process", options = list(container="body")),
      "+ tuneLength:",
      inline(
        numericInput(ns("sgboost_tuneLength"), NULL, value = vals$cur_sgboost_tuneLength, width="82px")
      )
    )
  })
  observeEvent(ignoreInit = T,input$sgboost_tuneLength,{
    vals$cur_sgboost_tuneLength<-input$sgboost_tuneLength
  })
  output$sgboost_custom<-renderUI({
    req(input$sgboost_search)
    req(input$sgboost_search=="user-defined")
    if(is.null(vals$n.trees)){vals$n.trees<-c("250, 500")}
    if(is.null(vals$interaction.depth)){vals$interaction.depth<-c("1, 2")}
    if(is.null(vals$shrinkage)){vals$shrinkage<-c("0.1")}
    if(is.null(vals$n.minobsinnode)){vals$n.minobsinnode<-c("10")}
    div(
      div(
        tipify(icon("fas fa-question-circle"),"Integer or a comma-delimited vector- the total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion", options = list(container="body")),
        "+ n.trees:",
        inline(
          textInput(ns("n.trees"), NULL, value=vals$n.trees, width="110px")
        )
      ),
      div(
        tipify(icon("fas fa-question-circle"),"Integer or a comma-delimited vector-  the maximum depth of each tree (i.e., the highest level of variable interactions allowed). A value of 1 implies an additive model, a value of 2 implies a model with up to 2-way interactions, etc. Default is 1.", options = list(container="body")),
        "+ interaction.depth:",
        inline(
          textInput(ns("interaction.depth"), NULL, value=vals$interaction.depth, width="110px")
        )
      ),
      div(
        tipify(icon("fas fa-question-circle"),"Integer or a comma-delimited vector:  the minimum number of observations in the terminal nodes of the trees", options = list(container="body")),
        "+ n.minobsinnode:",
        inline(textInput(ns("n.minobsinnode"), NULL, value=vals$n.minobsinnode, width="110px"))
      ),
      div(
        tipify(icon("fas fa-question-circle"),"Integer or a comma-delimited vector: shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction", options = list(container="body")),
        "+ shrinkage:",
        inline(
          textInput(ns("shrinkage"), NULL, value=vals$shrinkage, width="110px")))

    )
  })

  output$sgboost_params<- renderUI({

    column(12,class="well2",
           div(class="map_control_style",style="color: #05668D; margin-top: 20px",
               uiOutput(ns("sgboost_default")),
               uiOutput(ns("sgboost_custom")),



               div(
                 tipify(icon("fas fa-question-circle"),textseed(), options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seedsgboost"), NULL, value = NULL, width="122px")
                 )
               ),

               div(popify(icon("fas fa-question-circle"),NULL,
                          HTML(paste0(
                            div(HTML(paste0(strong("repeatedcv:")," repeated k-fold cross-validation;"))),
                            div(HTML(paste0(strong("boot:")," bootstraping;"))),

                            div(HTML(paste0(strong("LOOCV:")," Leave one out;"))),
                            div(HTML(paste0(strong("LGOCV:")," Leave-group out")))

                          )), options=list(container="body")

               ),
               "+ Resampling method:",
               pickerInput(ns("sgboost_res_method"),NULL, choices=list("repeatedcv","boot","LOOCV","LGOCV"), width = "100px")
               ),
               uiOutput(ns("sgboost_resampling"))
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("sgboost_war"))
           ),

           column(12, align = "center",
                  popify(actionButton(ns("trainsgboost"), h4(img(src=sgboost_icon,height='20',width='20'),"train Stochastic Gradient Boosting",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")
           ),
           column(12,align="right",
                  div(style="white-space: normal;max-width: 150px",
                      actionLink(ns("gotrain_loop"),"+ Train all variables in Datalist Y")
                  )
           )


    )
  })
  observeEvent(ignoreInit = T,input$gotrain_loop,{
    showModal(
      modalDialog(
        title=div("Train models using variables in",input$data_sgboostY),
        easyClose=T,

        div(
          column(12,
                 "This action will train",strong(ncol(getyloop()), style="color: red")," models, using the columns of Datalist: ",strong(input$data_sgboostY),"as Y. The given name of the models will be (",input$data_sgboostY,"::Y~Datalist_X), and any model already saved with this name(s) will be replaced.",em("We suggest using this tool from a Datalist without any saved GBM model", style="color: red")),
          column(12,
                 div(strong("Click to proceed:")),
                 actionButton(ns("train_loop"),"Train_loop_YDatalist"))
        )
      )
    )

  })

  observeEvent(ignoreInit = T,input$train_loop,{
    removeModal()
    trainsgboost_loop()
  })

  getyloop<-reactive({
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #input<-readRDS("input.rds")
    #vals<-readRDS("vals.rds")
    y<-vals$saved_data[[input$data_sgboostY]]
    if(input$sgboost_type== 'Classification'){y<-attr(y,"factors")}
    if(input$sgboost_test_partition!="None"){
      if(input$sgboost_type== 'Classification'){
        pic<-which(colnames(y)==input$sgboost_test_partition)
        if(length(pic)>0){
          y<-y[,-pic, drop=F]}
      }
    }
    y_loop<-y
    y_loop
  })

  trainsgboost_loop<-reactive({
    #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #vals<-readRDS("vals.rds")
    #input<-readRDS("input.rds")
    y_loop<-getyloop()
    withProgress(message="Running",max=ncol(y_loop),{
      i=1
      for(i in 1:ncol(y_loop)) {
        t<-try({
          data<-getdata_sgboostX()
          x<-x_o<-data.frame(data)
          y<-y_o<-y_loop[,i]

          output$sgboost_war<-renderUI({
            column(12,style="color: red",align="center",
                   if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
                   if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
            )
          })
          validate(need(anyNA(x)==F,"NAs not allowed in X"))
          validate(need(anyNA(y)==F,"NAs not allowed in Y"))
          req(input$sgboost_test_partition)
          if(input$sgboost_test_partition!="None"){
            parts<-get_parts_sgboost()
            train<-parts$train
            test<-parts$test
            x<-data.frame(getdata_sgboostX()[train,])
            y<-y_o[train]
          }


          seed<-if (!is.na(input$seedsgboost)) { input$seedsgboost} else{
            NULL
          }

          search<-"grid"
          tuneLength=NULL
          if(input$sgboost_search=="user-defined"){
            interaction.depth=as.numeric(unlist(strsplit(input$interaction.depth,",")))
            n.trees=as.numeric(unlist(strsplit(input$n.trees,",")))
            shrinkage=as.numeric(unlist(strsplit(input$shrinkage,",")))
            n.minobsinnode=as.numeric(unlist(strsplit(input$n.minobsinnode,",")))
            search<-"grid"
            grid<-expand.grid(interaction.depth=interaction.depth,
                              n.trees = n.trees,
                              shrinkage=shrinkage,
                              n.minobsinnode=n.minobsinnode)

          } else{
            grid=NULL
            search<-input$sgboost_search
            tuneLength =input$sgboost_tuneLength
          }

          colnames(x)<-gsub(" ",".", colnames(x))
          if (!is.na(input$seedsgboost)) {set.seed(input$seedsgboost)}

          sgboost<-train(x,y,'gbm',
                         trControl=trainControl(
                           method = input$sgboost_res_method,## resampling method
                           number = input$cvsgboost,
                           repeats = input$repeatssgboost,
                           p=input$pleavesgboost/100,
                           savePredictions = "all",
                           search=search
                         ),
                         tuneGrid=grid,
                         tuneLength=tuneLength

          )
          attr(sgboost,"test_partition")<-paste("Test data:",input$sgboost_test_partition,"::",input$testdata_sgboost)
          attr(sgboost,"Y")<-paste(input$data_sgboostY,"::",colnames(y_loop)[i])
          attr(sgboost,"Datalist")<-paste(input$data_sgboostX)

          vals$sgboost_unsaved<-sgboost

          if(input$sgboost_test_partition!="None"){
            attr(vals$sgboost_unsaved,'test')<-x_o[test,]
            attr(vals$sgboost_unsaved,"sup_test")<-y_o[test]
          } else{ attr(vals$sgboost_unsaved,'test')<-c("None")}
          vals$bag_sgboost<-T
          attr(vals$sgboost_unsaved,"supervisor")<-colnames(y_loop)[i]
          attr(vals$sgboost_unsaved,"inputs")<-list(
            Ydatalist=input$data_sgboostY,
            Y=colnames(y_loop)[i],
            Xdatalist=input$data_sgboostX
          )

          attr(vals$sgboost_unsaved,"Y")<-paste0(input$data_sgboostY,"::",colnames(y_loop)[i])
          savesgboost_loop()
          beep(10)

        })



        if("try-error" %in% class(t)){
          output$sgboost_war<-renderUI({
            column(12,em(style="color: gray",
                         "Error in training the GBM model. Check if the number of observations in X and Y are compatible"
            ))
          })

        } else{
          output$sgboost_war<-NULL
        }
        incProgress(1)
      }
      updateTabsetPanel(session,"sgboost_tab","sgboost_tab2")
    })

  })





  savesgboost_loop<-reactive({
    temp<-vals$sgboost_unsaved
    name_model<-paste0(attr(temp,"Y"),"~",input$data_sgboostX)
    temp<-list(temp)
    names(temp)<-name_model
    attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[[name_model]]<-temp
    cur<-name_model
    attr(vals$saved_data[[input$data_sgboostX]],"sgboost")['new GBM (unsaved)']<-NULL
    vals$bag_sgboost<-F
    vals$cur_sgboost_models<-cur
    vals$cur_sgboost<-cur
    vals$sgboost_unsaved<-NULL
  })

  savesgboost<-reactive({
    req(vals$cur_sgboost_models=='new GBM (unsaved)')
    temp<-vals$sgboost_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[[input$newdatalist]]<-temp
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[[input$over_datalist]]<-temp
      cur<-input$over_datalist
    }
    attr(vals$saved_data[[input$data_sgboostX]],"sgboost")['new GBM (unsaved)']<-NULL
    vals$bag_sgboost<-F
    vals$cur_sgboost_models<-cur
    vals$cur_sgboost<-cur

  })


  output$sgboost_resampling<-renderUI({
    div(
      inline(
        if(input$sgboost_res_method=='cv'|input$sgboost_res_method=='repeatedcv')
        {
          div(

            tipify(icon("fas fa-question-circle"),"number of folds", options=list(container="body")),"+ cv:",
            numericInput(ns("cvsgboost"), NULL, value = 5,width="140px")

          )
        }
      ),
      div(
        inline(
          if(input$sgboost_res_method=='repeatedcv')
          {
            inline(
              div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute", options=list(container="body")),
                  "+ repeats:",
                  numericInput(ns("repeatssgboost"),NULL, value = 1,width="109px")
              )
            )

          }
        )
      ),
      inline(
        if(input$sgboost_res_method=='boot'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the number of resampling iterations", options=list(container="body")),
              "+ number:",
              numericInput(ns("cvsgboost"), NULL, value = 5,width="100px")

            )
          )
        }
      ),
      inline(
        if(input$sgboost_res_method=='LGOCV'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the training percentage", options=list(container="body")),
              "+ percentage:",
              numericInput(ns("pleavesgboost"), NULL, value = 10,width="100px")
            )
          )
        }
      )
    )
  })


  output$sgboost_results_menu<-renderUI({
    if(is.null(vals$cur_sgboost_models)){vals$cur_sgboost_models<-1}
    div(pickerInput(ns("sgboost_models"),strong("GBM results:", tiphelp("Stochastic Gradient Boosting Results. Click to select GBM results saved in the Training Datalist (X).")), choices= names(attr(vals$saved_data[[input$data_sgboostX]],"sgboost")), width="200px", selected = vals$cur_sgboost_models)
    )
  })
  output$rmsgboost_button<-renderUI({
    req(input$sgboost_models!="new GBM (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmsgboost"), icon("far fa-trash-alt")), "Remove the GBM model from the training Datalist (X)")
    )

  })
  output$sgboost_tab_2_1<-renderUI({
    m<-vals$sgboost_results
    div(
      renderPrint(m),
      div(style="height: 100px",
          popify(downloadButton(ns("down_sgboost_results"),label=span("Download GBM model",img(src=sgboost_icon,height='20',width='20')),style = "button_active"),NULL,
                 HTML(paste0(
                   div(HTML(paste0("Download model as ",em('rds')," file to be read in R as", code('base::readRDS(file.rds)'))))
                 )))
      )

    )
  })
  output$down_sgboost_results <- {
    downloadHandler(
      filename = function() {
        paste0("GBM","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$sgboost_results,file)
      })
  }
  observeEvent(ignoreInit = T,input$sgboost_tab2,{
    vals$sgboost_tab2<-input$sgboost_tab2
  })

  output$results_sgboost<-renderUI({
    validate(need(length(vals$sgboost_results)>0,"No models have been trained yet"))

    if(is.null(vals$sgboost_tab2)){vals$sgboost_tab2<-'sgboost_tab2_1'}
    #saveRDS(vals$sgboost_results,"sgboost.rds")
    req(length(vals$sgboost_results))
    m<-vals$sgboost_results
    #m<-readRDS("sgboost.rds")
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    column(12,
           div(
             p(
               strong("Model:"),em(getmodel_sgboost())
             )
           ),
           tabsetPanel(id=ns("sgboost_tab2"),selected=vals$sgboost_tab2,
                       tabPanel("2.1. Summary",value="sgboost_tab2_1",
                                uiOutput(ns("sgboost_tab_2_1"))
                       ),
                       tabPanel("2.2. Performace",value="sgboost_tab2_2",
                                uiOutput(ns("sgboost_tab_2_2"))
                       ),
                       tabPanel("2.3. Permutation importance",value="sgboost_tab2_3",
                                uiOutput(ns("permutation_importance"))

                       ),
                       tabPanel("2.4. Confusion matrix",value="sgboost_tab2_4",
                                uiOutput(ns("sgboost_tab_2_4"))
                       )
           )

    )
  })

  ##
  output$permutation_importance<-renderUI({
    req(input$sgboost_models)
    validate(need(input$sgboost_models!="new GBM (unsaved)","
Please save your model to proceed with permutation importance analysis."))
    div(class="well3",style='choosechannel',
        uiOutput(ns('header_feature')),
        tabsetPanel(id=ns("pi_result"),

                    tabPanel("4.1. Feature Importance",value="tab1",
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
                                 ns('sgboost_varImp'),span("+ Download table"),style="button_active"
                               )
                             ),
                             DT::dataTableOutput(ns('feature_results'))
                    )
        )
    )
  })
  observeEvent(ignoreInit = T,input$feaimp_help,{
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
      h4("Permutation importance", tipify(actionLink(ns("feaimp_help"),icon("fas fa-question-circle")),"Click for more details")),
      actionButton(ns("run_feat_shuf"),"RUN"),
      inline(div(
        span("+ Iterations:",tiphelp("Number of iterations for randomizing each variable"),
             inline(
               numericInput(ns("feat_rep"),NULL, value=2, width="75px", step=1)
             )
        )
      )),
      inline(div(
        span("+ Seed:",tiphelp("seed for genereating reproducible results"),
             inline(
               numericInput(ns("feat_seed"),NULL, value=1, width="75px", step=1)
             )
        )
      ))
    )
  })

  observeEvent(ignoreInit = T,input$down_sgboost_tab3_1,{
    vals$hand_down<-"GBM- predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  output$feature_importance_side<-renderUI({
    div(class="map_control_style",style="color: #05668D",

        uiOutput(ns('side_filter_feature_plot_nvars')),
        uiOutput(ns('side_palette')),
        uiOutput(ns('side_feature_axes_plot')),

        div(
          actionLink(
            ns('sgboost_varImp_plot'),span('+ Download plot'),style="button_active"
          )
        )

    )
  })
  output$side_palette<-renderUI({
    div(
      div(
        span("+ Palette",
             inline(
               pickerInput(inputId = ns("pal_feature"),
                           label = NULL,
                           choices =     vals$colors_img$val,
                           choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
             )
        )
      )
    )
  })
  output$side_feature_axes_plot<-renderUI({
    div(
      div("+ Axes size",inline(numericInput(ns("feaimp_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("feaimp_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("feaimp_cex.leg"),NULL, value=13, width="75px", step=1))),


    )
  })
  output$side_filter_feature_plot_nvars<-renderUI({


    div( div(
      span("+ Number of variables:",tiphelp("Filter variables with the highest mean importance"),
           inline(
             numericInput(ns("feat_npic"),NULL, value=20, width="75px", step=1)
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
  observeEvent(ignoreInit = T,input$sgboost_varImp_plot,{
    vals$hand_plot<-"GBM - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })



  output$feature_cm<-renderUI({
    m<-vals$sgboost_results
    newdata<-getdata_model(m)

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
                  pickerInput(inputId = ns("pal_feature_cm"),
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
    model_list<- attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[[input$sgboost_models]]
    req(length(model_list)>0)
    predtable<-model_list$feature_rands
    dft<-get_cm_impact(predtable, var=input$var_feature_cm)

    renderPlot({
      vals$feature_cm_plot<- plotCM(dft, palette=input$pal_feature_cm,vals$newcolhabs, font_color="black", title="", round_cm=3)
      vals$feature_cm_plot
    })
  })
  getmetric<-reactive({
    m<-vals$sgboost_results
    metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
    metric
  })
  get_sumamry_permute_importance<-reactive({
    req(input$sig_feature)
    model_list<- attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[[input$sgboost_models]]
    sig_level<-input$sig_feature
    req(length(model_list)>0)
    df<-model_list$feature_rands
    m<-model_list[[1]]
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    df<-permimp_metric(df,metric=getmetric(), class=NULL,m, newdata,obc)

    req(length(df)>0)
    req(is.data.frame(df))

    vals$sgboost_varImp<-perm_summary<-sig_feature(df,m, newdata, obc,sig_level)
    perm_summary
  })
  output$feature_results<-DT::renderDataTable({
    table<-get_sumamry_permute_importance()
    DT::datatable(table,options=list(pageLength = 20,info = FALSE,lengthMenu = list(c(20,-1),c( "20","All")),autoWidth=T,dom = 'lt'),rownames = TRUE,class ='compact cell-border')

  })


  output$side_feature_axes_plot<-renderUI({
    div(
      div("+ Axes size",inline(numericInput(ns("feaimp_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("feaimp_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("feaimp_cex.leg"),NULL, value=13, width="75px", step=1))),


    )
  })
  output$side_palette<-renderUI({
    div(
      div(
        span("+ Palette",
             inline(
               pickerInput(inputId = ns("pal_feature"),
                           label = NULL,
                           choices =     vals$colors_img$val,
                           choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
             )
        )
      )
    )
  })
  observeEvent(ignoreInit = T,input$run_feat_shuf,{
    library(doParallel)
    library(foreach)
    cores=detectCores()
    cl <- makeCluster(cores[1]-1)
    registerDoSNOW(cl)
    m<-vals$sgboost_results
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    rep=input$feat_rep
    seed=input$feat_seed
    if(is.na(seed)){seed=NULL}
    predtable<- permute_importance_foreach_progress(m,newdata,obc, rep, seed=seed)
    stopCluster(cl)
    unregister_dopar()
    attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[[input$sgboost_models]]$feature_rands<-predtable
  })
  output$feature_importance_plot<-renderUI({
    req(input$data_sgboostX)
    req(input$sgboost_models)
    model_list<- attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[[input$sgboost_models]]
    req(length(model_list)>0)
    req(input$feat_npic)
    sig_level<-input$sig_feature

    req(length(model_list)>0)
    df<-model_list$feature_rands
    validate(need(length(df)>0,"Analyses pending"))
    m<-model_list[[1]]
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")

    df<-permimp_metric(df,metric=getmetric(), class=NULL, m,newdata,obc)

    req(length(df)>0)
    req(is.data.frame(df))

    rands<-df
    perm_summary<-sig_feature(df,m, newdata, obc,sig_level)
    sig_names<-rownames(perm_summary)[perm_summary$sig=="*"]
    df<-get_feature_data_plot(df,m,newdata,obc, npic=input$feat_npic)
    col<-vals$newcolhabs[[input$pal_feature]](2)
    req(nrow(df)>1)
    df$sig<-"non_sig"
    df$sig[    df$var%in%sig_names    ]<-"sig"
    df$var[ which( df$var%in%sig_names)]<-paste0("*",df$var[  which( df$var%in%sig_names)])
    req(length(df)>0)
    renderPlot({
      df<-df

      p<-ggplot(df, aes(reorder(var,Metric), Metric)) +
        geom_boxplot(aes(fill=sig))+coord_flip()+labs(x="Variables", y='Performance decay')+ggtitle("Feature Shuffling Impact")+theme(
          axis.text=element_text(size=input$feaimp_cex.axes),
          axis.title=element_text(size=input$feaimp_cex.lab)
        )+scale_fill_manual(values=col)

      vals$sgboost_varImp_plot<-p
      vals$sgboost_varImp_plot

    })
  })

  ##
  output$sgboost_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('sgboost_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the sgboost training errors", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('sgboost_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "Download Table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('sgboost_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('sgboost_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          tags$style(
            paste(paste0("#",ns('sgboost_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('sgboost_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          div(
            p(strong("Global:")),
            inline(DT::dataTableOutput(ns('sgboost_tab_errors0_train')))
          ),

          div(
            p(strong("Observations:")),
            inline(DT::dataTableOutput(ns('sgboost_tab_errors_train')))
          )



        )
      )
    )
  })
  output$sgboost_tab_errors0_train<-DT::renderDataTable({
    m<-vals$sgboost_results
    table<- m$results[rownames(m$bestTune),]
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })
  output$sgboost_tab_errors_train<-DT::renderDataTable({
    m<-vals$sgboost_results
    table<-accu_rf_class(m)
    vals$sgboost_down_errors_train<-table
    table
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })

  output$sgboost_tab_2_4<-renderUI({

    # validate(need(is.factor( vals$sgboost_results$finalModel$y), "Confusion matrices are only valid for classification (factor) models."))
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          class="map_control_style",style="color: #05668D",
          div(
            span(
              "+ Type: ",
              pickerInput(inputId = ns("sgboost_cm_type"),
                          label = NULL,
                          choices = c("Resampling","Optimal Model"),

                          width="100px"
              )
            )
          ),
          div(
            span(
              "+ Palette",
              pickerInput(inputId = ns("sgboostpalette"),
                          label = NULL,
                          choices = vals$colors_img$val,
                          choicesOpt = list(
                            content = vals$colors_img$img
                          ),
                          options=list(container="body"),
                          selected=vals$colors_img$val[1],
                          width="75px"
              )
            )
          ),
          div(
            popify(actionLink(ns("downp_cmsgboost"), span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot")
          ),
          div(
            popify(actionLink(ns("dowcenter_cmsgboost"), span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM plot")
          )

        )

      ),
      mainPanel(
        plotOutput(ns("confusion_sgboost")),
        verbatimTextOutput(ns("confusion_sgboost2"))
      )
    )


  })

  observeEvent(ignoreInit = T,input$dowcenter_cmsgboost,{
    vals$hand_down<-"GBM_cm"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  output$confusion_sgboost <- renderPlot({
    validate(need(vals$sgboost_results$modelType=="Classification","Confusion matrices are only valid for classification models."))
    m<-vals$sgboost_results
    if(input$sgboost_cm_type=="Resampling"){
      res<-plotCM(m, input$sgboostpalette,  newcolhabs=vals$newcolhabs)
    } else{
      cm<-table(predict(vals$sgboost_results),vals$sgboost_results$trainingData[,'.outcome'])
      res<-plotCM(cm,input$sgboostpalette,vals$newcolhabs)
    }
    vals$cm_sgboost<-res
    res
  })
  output$confusion_sgboost2<-renderPrint({
    validate(need(vals$sgboost_results$modelType=="Classification","Confusion matrices are only valid for classification models."))

    res<-if(input$sgboost_cm_type=="Resampling"){
      confusionMatrix(vals$sgboost_results$pred$pred,vals$sgboost_results$pred$obs)
    } else{
      confusionMatrix(predict(vals$sgboost_results),vals$sgboost_results$trainingData[,'.outcome'])
    }
    vals$sgboost_cm<-as.data.frame.matrix(res$table)
    res
  })



  output$savesgboost_button<-renderUI({
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_savesgboost"), icon("fas fa-save")), "Save the sgboost model in the training Datalist (X)")
    )
  })

  getnewdatasgboost<-reactive({
    datalist<-vals$saved_data

    datalist_comp<-lapply(
      vals$saved_data, function (x) {
        colnames(x)<-gsub(" ",".", colnames(x))
        x
      }

    )

    datalist_comp<-vals$saved_data




    m<-vals$sgboost_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-Y])
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })
  get_supervisor_sgboost <- reactive({
    m<-vals$sgboost_results
    att<-if(m$modelType=="Classification")
    {
      data <- vals$saved_data[[input$data_sgboostY]]
      labels <- attr(data,"factors")
      labels[input$sgboost_sup]
    } else{
      data <- vals$saved_data[[input$data_sgboostY]]
      data[input$sgboost_sup]
    }

  })
  get_cm_pred<-reactive({
    obs<-get_sgboost_prederrors()$obs
    pred<-get_sgboost_prederrors()$pred
    conf<-table(obs, pred)
    conf
  })


  getobssgboost<-reactive({
    sup_test<-attr(vals$sgboost_results,"supervisor")

    datalist<-vals$saved_data
    if(vals$sgboost_results$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}

    m<-vals$sgboost_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })

  pred_sgboost<-reactive({
    validate(need(!anyNA(test_sgboost()),"NAs not allowed in the prediction Datalist"))
    m<-vals$sgboost_results


    sgboost_pred <- predict(m,newdata = test_sgboost())
    res<-data.frame(Predictions= sgboost_pred)
    rownames(res)<-rownames(test_sgboost())
    #colnames(res)<-attr(vals$sgboost_results,"supervisor")

    # attr(res,"obs")<-test_sgboost()
    res



  })
  get_parts_sgboost<-reactive({
    req(input$data_sgboostY)
    req(input$testdata_sgboost)
    req(input$sgboost_test_partition)
    data<-getdata_sgboostX()
    factors<-attr(vals$saved_data[[input$data_sgboostY]],"factors")[rownames(data),]
    if(input$sgboost_test_partition!='None'){
      lis<-split(rownames(factors),factors[,input$sgboost_test_partition])
      names(lis[[1]])<-lis[[1]]
      names(lis[[2]])<-lis[[2]]
      test_i<-which(names(lis)==input$testdata_sgboost)
      tra_i<-which(names(lis)!=input$testdata_sgboost)
      parts=list(train=lis[[tra_i]],test=lis[[test_i]])} else{
        parts=list(train=NULL,test=NULL)
      }

    parts

  })
  getdata_sgboostX<-reactive({
    req(input$data_sgboostX)
    data=vals$saved_data[[input$data_sgboostX]]
    data
  })
  getdata_sgboostY<-reactive({
    req(input$data_sgboostY)
    req(input$sgboost_sup)

    data <- vals$saved_data[[input$data_sgboostY]]
    if(input$sgboost_type=="Classification"){
      labels <- attr(data,"factors")
      res<-labels[rownames(vals$saved_data[[input$data_sgboostX]]),input$sgboost_sup]
      names(res)<-rownames(vals$saved_data[[input$data_sgboostX]])
      res
    } else{
      res<-data[rownames(vals$saved_data[[input$data_sgboostX]]),input$sgboost_sup]
      names(res)<-rownames(vals$saved_data[[input$data_sgboostX]])
      res
    }

  })
  getmodel_sgboost<-reactive({
    m<-vals$sgboost_results
    att<-if(m$modelType=="Classification"){
      "Factor-Attribute"
    } else{ "Data-Attribute"}
    attri<-attr(m,'inputs')
    res<-c(attri$Ydatalist,
           att,
           attri$Y,
           attri$Xdatalist)
    model<-  paste( paste0(res[1],"::",res[2],"::",res[3]),"~",paste0(res[4]))
    model

  })


  observeEvent(ignoreInit = T,input$predsgboost_new,{
    vals$predsgboost_new<-input$predsgboost_new
  })
  observeEvent(ignoreInit = T,input$sgboostpred_which,{
    vals$sgboostpred_which<-input$sgboostpred_which
  })
  observeEvent(ignoreInit = T,input$sgboost_down_errors_train,{
    vals$hand_down<-"GBM - training errors (observations)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$sgboost_varImp,{
    vals$hand_down<-"sgboost - variable importance"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_sgboost_tab3_1,{
    vals$hand_down<-"GBM - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$data_sgboostY,{
    vals$cur_dataY<-input$data_sgboostY
  })
  observeEvent(ignoreInit = T,input$sgboost_type,{
    vals$cur_model_type<-input$sgboost_type
  })

  observeEvent(ignoreInit = T,input$sgboost_method,{
    vals$cur_sgboost_method<-input$sgboost_method
  })
  observeEvent(ignoreInit = T,input$sgboost_sup,{
    vals$cur_response<-input$sgboost_sup
  })
  observeEvent(ignoreInit = T,input$data_sgboostX,{
    vals$cur_data<-input$data_sgboostX
  })
  observeEvent(ignoreInit = T,input$sgboost_test_partition,{
    vals$cur_test_partition<-input$sgboost_test_partition
  })
  observeEvent(ignoreInit = T,input$testdata_sgboost,{
    vals$cur_testdata<-input$testdata_sgboost
  })
  observeEvent(ignoreInit = T,input$sgboost_models,{
    vals$cur_sgboost_models<-input$sgboost_models
  })
  observeEvent(ignoreInit = T,input$trainsgboost,{
    try({


      x<-x_o<-data.frame(getdata_sgboostX())
      y<-y_o<-getdata_sgboostY()

      output$sgboost_war<-renderUI({
        column(12,style="color: red",align="center",
               if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
               if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
        )
      })
      validate(need(anyNA(x)==F,"NAs not allowed in X"))
      validate(need(anyNA(y)==F,"NAs not allowed in Y"))
      req(input$sgboost_test_partition)
      if(input$sgboost_test_partition!="None"){
        parts<-get_parts_sgboost()
        train<-parts$train
        test<-parts$test
        x<-data.frame(getdata_sgboostX()[train,])
        y<-getdata_sgboostY()[train]
      }
      #readrd(y,"y.rds")
      #saveRDS(x,"x.rds")
      #saveRDS(reactiveValuesToList(input),"input.rds")
      #y<-readRDS("y.rds")
      #x<-readRDS("x.rds")
      #input<-readRDS("input.rds")


      seed<-if (!is.na(input$seedsgboost)) { input$seedsgboost} else{
        NULL
      }

      search<-"grid"
      tuneLength=NULL
      if(input$sgboost_search=="user-defined"){
        interaction.depth=as.numeric(unlist(strsplit(input$interaction.depth,",")))
        n.trees=as.numeric(unlist(strsplit(input$n.trees,",")))
        shrinkage=as.numeric(unlist(strsplit(input$shrinkage,",")))
        n.minobsinnode=as.numeric(unlist(strsplit(input$n.minobsinnode,",")))
        grid<-expand.grid(interaction.depth=interaction.depth,
                          n.trees = n.trees,
                          shrinkage=shrinkage,
                          n.minobsinnode=n.minobsinnode)

      } else{
        grid=NULL
        search<-input$sgboost_search
        tuneLength =input$sgboost_tuneLength
      }

      colnames(x)<-gsub(" ",".", colnames(x))
      if (!is.na(input$seedsgboost)) {set.seed(input$seedsgboost)}
      withProgress(message = "Running Stochastic gradient boosting ...",
                   min = 1,
                   max = 1,
                   {
                     sgboost<-train(x,y,'gbm',
                                    trControl=trainControl(
                                      method = input$sgboost_res_method,## resampling method
                                      number = input$cvsgboost,
                                      repeats = input$repeatssgboost,
                                      p=input$pleavesgboost/100,
                                      savePredictions = "all",
                                      search=search
                                    ),
                                    tuneGrid=grid,
                                    tuneLength=tuneLength

                     )
                     attr(sgboost,"test_partition")<-paste("Test data:",input$sgboost_test_partition,"::",input$testdata_sgboost)
                     attr(sgboost,"Y")<-paste(input$data_sgboostY,"::",input$sgboost_sup)
                     attr(sgboost,"Datalist")<-paste(input$data_sgboostX)

                     vals$sgboost_unsaved<-sgboost

                     if(input$sgboost_test_partition!="None"){
                       attr(vals$sgboost_unsaved,'test')<-x_o[test,]
                       attr(vals$sgboost_unsaved,"sup_test")<-y_o[test]
                     } else{ attr(vals$sgboost_unsaved,'test')<-c("None")}
                     vals$bag_sgboost<-T
                     attr(vals$sgboost_unsaved,"supervisor")<-input$sgboost_sup
                     attr(vals$sgboost_unsaved,"inputs")<-list(
                       Ydatalist=input$data_sgboostY,
                       Y=input$sgboost_sup,
                       Xdatalist=input$data_sgboostX
                     )
                     updateTabsetPanel(session,"sgboost_tab","sgboost_tab2")
                   })
      beep(10)
      attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[['new GBM (unsaved)']]<-vals$sgboost_unsaved
      vals$cur_sgboost_models<-"new GBM (unsaved)"
      vals$bag_sgboost<-T
      #saveRDS(vals$sgboost_unsaved,"sgboost.rds")

    })
  })
  observeEvent(ignoreInit = T,input$downp_cmsgboost,{
    vals$hand_plot<-"sgboost - Confusion Matrix"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$downp_cmsgboost_pred,{
    vals$hand_plot<-"sgboost - Confusion Matrix (predictions)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})

  observeEvent(ignoreInit = T,input$downp_sgboost_dens,{
    vals$hand_plot<-"sgboost - DM plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observe({
    req(input$sgboost_models)
    if(input$sgboost_models=="new GBM (unsaved)"){
      vals$sgboost_results<-vals$sgboost_unsaved } else{
        vals$sgboost_results<-attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[[input$sgboost_models]][[1]]
      }
  })
  observeEvent(ignoreInit = T,input$sgboost_models,{
    vals$cur_sgboost<-input$sgboost_models
  })
  observeEvent(ignoreInit = T,input$tools_rmsgboost,{
    attr(vals$saved_data[[input$data_sgboostX]],"sgboost")[input$sgboost_models]<-NULL
  })
  observeEvent(ignoreInit = T,input$tools_savesgboost,{
    if(input$tools_savesgboost %% 2){
      vals$hand_save<-"Save GBM model in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_sgboostX, style="color:gray"),strong("::"), em("GBM-Attribute", style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL

      showModal(module_sgboost())

    }
  })
  observeEvent(ignoreInit = T,input$sgboost_create_errors_train,{
    vals$hand_save<-"Create Datalist: GBM training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_sgboost())
  })
  observeEvent(ignoreInit = T,input$sgboost_create_predictions,{
    vals$hand_save<- "Create Datalist: GBM predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute, and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL

    showModal(module_sgboost())
  })




  resample_create<-reactive({

    m<-vals$sgboost_results
    var<-attr(m,"supervisor")
    temp<-vals$sgboost_treetest
    datao<-vals$saved_data[[input$data_sgboostX]]
    factors<-attr(temp,"factors")

    factors<-temp[c('q_class')]
    colnames(factors)<-paste(var,colnames(factors),sep="_")
    temp[c('w_class','sig','q_class')]<-NULL



    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      attr(temp,"factors")<-factors
      vals$saved_data[[input$newdatalist]]<-temp

    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      attr(temp,"factors")<-factors
      vals$saved_data[[input$over_datalist]]<-temp

    }

  })
  name_resample_create<-reactive({

    bag<-1
    var<-attr(vals$sgboost_results,"supervisor")
    name0<-paste("GBM",var,"qclass", sep="_")
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
  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      ##RF
      "Save GBM model in"= { name_save_sgboost()},
      "Create Datalist: GBM training errors -obs"={name_sgboost_train_errors()},
      "Create Datalist: GBM predictions"={ name_sgboost_pred()},
      "resample_create"={name_resample_create()}
    )})

  sgboost_create_training_errors<-reactive({
    temp<-vals$sgboost_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$data_sgboostX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  sgboost_create_pred<-reactive({
    temp<-data.frame(vals$sgboosttab_pred)
    temp[,1]<-as.numeric( temp[,1])
    attr(temp,"factors")<-temp

    datao<-if(vals$sgboostpred_which=="Datalist"){
      vals$saved_data[[vals$predsgboost_new]]
    } else{ vals$saved_data[[input$data_sgboostX]]}
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  name_save_sgboost<-reactive({
    bag<-1
    name0<-paste0("GBM")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[input$data_sgboostX]],"sgboost"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[input$data_sgboostX]],"sgboost"))) break
      }}
    paste(name0,bag)
  })
  name_sgboost_train_errors<-reactive({
    bag<-1
    name0<-paste0("GBM training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_sgboost_pred<-reactive({
    bag<-1
    name0<-paste0("GBM predictions")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  output$data_over<-renderUI({
    data_overwritte$df<-F
    choices<-c(names(vals$saved_data))
    req(input$hand_save=="over")
    if(vals$hand_save=='Save GBM model in'){choices<-names(attr(vals$saved_data[[input$data_sgboostX]],'sgboost'))}
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
      "Save GBM model in"= {savesgboost()},
      "Create Datalist: GBM training errors -obs"=sgboost_create_training_errors(),
      "Create Datalist: GBM predictions"={sgboost_create_pred()},
      'resample_create'={resample_create()}

    )
    removeModal()

  })
  module_sgboost <- function() {
    ns <- session$ns
    modalDialog(
      uiOutput(ns("databank_storage")),
      title=strong(icon("fas fa-save"),'Save'),
      footer=column(12,
                    uiOutput(ns('saverf_teste')),
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
