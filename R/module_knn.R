

## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.

#' @export
module_ui_knn <- function(id){

  ns <- NS(id)
uiOutput(ns("knn_panels"))

}


#' @export
module_server_knn <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns

  output$knn_panels<-renderUI({
   # validate(need(length(vals$saved_data)>0,"No Datalist found"))
   div(
    # includeCSS("meta/styles_mod.css"),
     column(12,style="background: white",
            uiOutput(ns("knn_inputs")),
            uiOutput(ns("war_knn")),
            column(12,
                   tabsetPanel(id = ns("knn_tab"),
                               tabPanel(strong("1. Training"),value='knn_tab1',
                                        uiOutput(ns("knn_params"))),
                               tabPanel(strong("2. Results"),value='knn_tab2',
                                        uiOutput(ns("results_knn"))),
                               tabPanel(strong("3. Predict"),value='knn_tab3',
                                        uiOutput(ns("predict_knn")))
                   )
            )

     )


   )
    })
  insert_knn_resutls<-reactiveValues(df=F)




  output$predict_knn<-renderUI({
    column(12,style="background: white", uiOutput(ns("knn_pred_type")),
           tabsetPanel(id="predknn_tab",
                       tabPanel(
                         strong("3.1. Results"), value="predknn_tab01",
                         fluidRow(style="background: white",
                                  uiOutput(ns("knn_tab3_1")))),
                       tabPanel(
                         strong("3.2. Performace"),
                         fluidRow(style="background: white",
                                  uiOutput(ns("knn_tab3_2")))
                       )

           )
    )
  })
  output$knn_tab3_2<-renderUI({
    sup_test<-attr(vals$knn_results,"supervisor")
    supname<-paste0("Observed Y [",sup_test,"]")
    lab1<-span("+",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
               span(supname))

    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            pickerInput(ns("predknn_newY"),
                        lab1,names(vals$saved_data[getobsknn()])),
            uiOutput(ns("knn_pred_pal")),
            numericInput(ns("round_pred_knn"),"+ Round",3,step=1),
            actionLink(ns("down_metrics"),span("+ Donwload Metrics",icon("fas fa-table"))),
            uiOutput(ns("down_cm_btn")),

        )),
      mainPanel(
        fluidRow(
          column(3,style="margin: 0px; padding: 0px",
                 inline(DT::dataTableOutput(ns('knn_tab_errors0_test')))
          ),
          column(9,uiOutput(ns("confusion_matrix_knn"))
          )
        )
      )
    )
  })
  get_knn_prederrors<-reactive({
    req(length(input$knnpred_which)>0)

    m<-vals$knn_results
    pred<-pred_knn()[,1]
    # x_t<-attr(m,"test")
    obs<-if(input$knnpred_which=="Partition"){
      attr(m,"sup_test")} else{
        req(length(input$predknn_newY)>0)

        if(m$modelType=="Classification"){
          req(length(attr(m,"supervisor"))>0)
          req(length(input$predknn_newY)>0)
          req(input$predknn_newY%in%names(vals$saved_data))
          sup<-attr(m,"supervisor")
          factors<-attr(vals$saved_data[[input$predknn_newY]],"factors")
          req(sup%in%colnames(factors))
          factors[,sup]} else{
            vals$saved_data[[input$predknn_newY]][,attr(m,"supervisor")]
          }
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })
  test_knn<-reactive({
    req(input$knnpred_which)
    m<-vals$knn_results
    pred_tab<-if(input$knnpred_which=="Partition"){attr(m,"test")} else if(input$knnpred_which=="Datalist"){
      req(input$predknn_new)
      pred_tab<-vals$saved_data[[input$predknn_new]]
    }
    model_data<-pred_tab
    req(sum(colnames(pred_tab)%in%colnames(getdata_model(m)))==ncol(pred_tab))
    pred_tab

  })
  output$knn_tab_errors0_test<-DT::renderDataTable({
    req(input$round_pred_knn)
    table<-pred_test_knn()
    vals$sup_metrics<-table<-round(table,input$round_pred_knn)
    #colnames(table)<-NULL
    DT::datatable(table,
                  options=list(rownames=T,info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'),colnames=c(""))
  })
  output$confusion_knn_pred<-renderUI({
    req(vals$knn_results$modelType=="Classification")

    conf<-get_cm_pred()
    column(12,
           renderPlot({
             res<-plotCM(conf/sum(conf)*100, input$knnpalette_pred,  newcolhabs=vals$newcolhabs,round_cm=input$round_pred_knn,title=input$knn_cmpred_title)
             vals$knn_cm_pred<-res
             res
           }))

  })
  output$confusion_knn2_pred<-renderPrint({
    req(vals$knn_results$modelType=="Classification")
    res<-confusionMatrix(get_cm_pred())
    vals$knn_cm_test<-  as.data.frame.matrix(res$table)
    res
  })
  output$knn_pred_type<-renderUI({
    choices=if(length(attr(vals$knn_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(class="well2",style="padding: 10px",
        div(class="map_control_style2",style="color: #05668D",
            inline(radioButtons(ns("knnpred_which"),strong("New data (X):"),choices=choices,inline=T)), inline(uiOutput(ns("datalist_pred_knn")))
        )
    )
  })
  output$datalist_pred_knn<-renderUI({
    req(input$knnpred_which =='Datalist')
    div(
      pickerInput(ns("predknn_new"),"->",names(vals$saved_data[getnewdataknn()]), width = '300px', selected=vals$predknn_new)
    )
  })
  pred_test_knn<-reactive({
    obs<-get_knn_prederrors()$obs
    pred<-get_knn_prederrors()$pred
    m<-vals$knn_results

    table<-stats_ml(obs,pred,m)


    table
  })

  output$confusion_matrix_knn<-renderUI({
    req(vals$knn_results$modelType=="Classification")
    div(

      style="overflow-x: scroll",
      uiOutput(ns("confusion_knn_pred")),
      verbatimTextOutput(ns("confusion_knn2_pred"))

    )
  })
  gettile_cmpred<-reactive({
    req(input$knnpred_which)
    ifelse(input$knnpred_which=="Partition","Test","Confusion Matrix")
  })
  output$knn_cmpred_title<-renderUI({
    textInput(ns("knn_cmpred_title"),"+ CM Title",gettile_cmpred())
  })
  output$knn_pred_pal<-renderUI({
    req(vals$knn_results$modelType=="Classification")
    lab2<-span("+",tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"),"Palette")
    div(
      uiOutput(ns('knn_cmpred_title')),
      pickerInput(inputId = ns("knnpalette_pred"),
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
    req(vals$knn_results$modelType=="Classification")

    div(
      div(actionLink(ns("dowcenter_cmknn_pred"),span("+ Download Confusion",icon("fas fa-table")))),
      div(actionLink(ns("downp_cmknn_pred"),span("+ Donwload plot"))
      )
    )

  })
  ##


  predall_knn<-reactive({
    validate(need(!anyNA(test_knn()),"NAs not allowed in the prediction Datalist"))
    m<-vals$knn_results
    #factors<-attr(vals$saved_data[[input$predknn_new]],"factors")
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
  knn_observed2<-reactive({
    req(input$knnpred_which)


    obs_data<-if(input$knnpred_which=="Datalist"){
      if(vals$knn_results$modelType=="Classification"){
        factors<-attr(vals$saved_data[[input$predknn_newY_tree]],'factors')
      } else {
        factors<-vals$saved_data[[input$predknn_newY_tree]]}

      res<-factors[,attr(vals$knn_results,"supervisor")]
      names(res)<-rownames(factors[attr(vals$knn_results,"supervisor")])
      res
    } else{
      attr(vals$knn_results,"sup_test")
    }
    obs_data
  })
  get_qclass<-reactive({
    m<-vals$knn_results
    req(m$modelType=="Regression")
    pred<-predall_knn()
    model_data<-test_knn()
    obs_data<-knn_observed2()
    obs_data<-data.frame(obs_data)
    ref1<-rownames(model_data)
    ref2<-rownames(obs_data)
    ref1<-ref2
    lo<-split(obs_data,ref2)

    pred_interval=input$q_class_val
    pred.knn.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )


    lpred<-split(pred,ref1)
    lp<-split(pred.knn.int,ref1)
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
    vals$knn_treetest<-res
    vals$knn_treetest
  })
  output$knn_tree_show<-renderUI({
    val=if(vals$knn_results$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$knn_tree_pred<-renderUI({
    req(input$nhist_tree)
    data=t(predall_knn())
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
    req(vals$knn_results$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),'Mean predictions')

        ))
  })
  output$plot_forest<-renderUI({
    req(vals$knn_results$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({
    req(input$qclass_width)
    req(input$qclass_height)
    div(style="background: white",
        inline(uiOutput(ns("knn_tree_show"))),
        inline(uiOutput(ns("knn_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))

  })
  output$forest_byclass<-renderUI({
    req(vals$knn_results$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })
  get_forest_class<-reactive({
    req(input$predknn_newY_tree)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$knn_results
    req(m$modelType=="Classification")
    pred<-predall_knn()
    pred.ind <- pred
    model_data<-test_knn()
    obs_data<-knn_observed2()
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
  output$pick_knnobs_pred<-renderUI({
    req(input$knnpred_which=="Datalist")
    sup_test<-attr(vals$knn_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("obs_cm_pred"),
                       NULL,names(vals$saved_data[getobsknn()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })
  output$predknn_newY_ref<-renderUI({
    req(input$predknn_newY_tree)
    req(input$knnpred_which)
    req(vals$knn_results$modelType=="Regression")
    factors<-attr(vals$saved_data[[input$predknn_newY_tree]],"factors")
    choices=if(input$knnpred_which=="Datalist"){
      c('rownames',colnames(factors))} else{
        'rownames'
      }
    div(
      span("+ ref",inline(pickerInput(ns('knn_reftest'),NULL,choices, width="200px", selected=vals$knn_reftest)))

    )})
  output$forest_margin<-renderUI({
    req(vals$knn_results$modelType=="Regression")
    div(
      span("+ Margin:",
           inline(
             numericInput(ns("q_class_val"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    )
  })
  output$forest_col<-renderUI({
    req(vals$knn_results$modelType=="Classification")
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
  output$pick_knnobs<-renderUI({
    sup_test<-attr(vals$knn_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("predknn_newY_tree"),
                       NULL,names(vals$saved_data[getobsknn()]), width="200px",selected=vals$predknn_newY_tree
           ))
    )
  })
  output$resample_out<-renderUI({
    m<-vals$knn_results
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
      uiOutput(ns("pick_knnobs")),
      uiOutput(ns("predknn_newY_ref")),
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
    req(vals$knn_results$modelType=="Regression")
    div(
      div(actionLink(ns('resamp_pred_gdownp'),"+ Download Table")),
      div( actionLink(ns('resamp_pred_gcreate'),"Create Datalist"))
    )
  })
  output$summ_trees<-renderPlot({
    req(vals$knn_results$modelType=="Regression")

    get_qclass()
    req(input$q_class_val)

    req(input$splitdata_trees)
    req(input$nhist_tree)
    pred<-predall_knn()
    data=t(pred)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs_data<-knn_observed2()
    obs_data<-data.frame(obs_data)
    obs=obs_data[ colnames(data),]

    pred <- pred
    pred_interval=input$q_class_val
    q_class=vals$knn_treetest[colnames(data),]
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
  observeEvent(ignoreInit = T,input$predknn_newY_tree,{
    vals$predknn_newY_tree<-input$predknn_newY_tree
  })
  observeEvent(ignoreInit = T,input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_knn())

  })
  observeEvent(ignoreInit = T,input$resamp_pred_gdownp,{

    vals$hand_down<-"knn_qclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })




  observeEvent(ignoreInit = T,input$predknn_new,{
    vals$predknn_new<-input$predknn_new
  })



  observeEvent(ignoreInit = T,input$dowcenter_cmknn_pred,{
    vals$hand_down<-"knn_cm_test"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })



  output$knn_tab3_1<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('knn_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the knn predictions", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('down_knn_tab3_1'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "+ Download Table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('knntab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('knntab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          inline(DT::dataTableOutput(ns('knntab_pred')))
        )
      )
    )
  })
  output$knntab_pred<-DT::renderDataTable({
    table<-vals$knntab_pred<-pred_knn()
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))

  }, rownames = TRUE,class ='compact cell-border')


  output$knn_inputs <- renderUI({
    if(is.null(vals$cur_model_type)){vals$cur_model_type<-"Classification"}
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",
             if(input$knn_tab=="knn_tab1"){
               div(
                 div(class='well3',
                     strong("Type:"),inline(radioButtons(ns('knn_type'),NULL,choices=c("Classification","Regression"),inline =T, selected=vals$cur_model_type))
                 ),
                 div(class="well3",
                     span(strong("X:"),
                          inline(uiOutput(ns('data_knnX_out')))

                     )
                 ),
                 div(class="well3",
                     span(strong("Y:"),
                          inline(
                            uiOutput(ns("data_knnY_out"))
                          ),
                          "::",
                          inline(uiOutput(ns('knn_Y'))),

                          inline(uiOutput(ns("knn_partition"))),
                          "::",
                          inline(uiOutput(ns("knn_test_ref")))
                     )
                 )


               )
             } else {
               column(12,
                      inline( uiOutput(ns('data_knnX_out'))),
                      inline(uiOutput(ns("knn_results_menu"))),
                      inline(uiOutput(ns("saveknn_button"))),
                      inline(uiOutput(ns("rmknn_button"))),
               )
             }
    )
  })
  output$data_knnY_out<-renderUI({
    div(
      div("Datalist:"),
      div(pickerInput(ns("data_knnY"),NULL,choices =  names(vals$saved_data), width="150px", selected=vals$cur_dataY))
    )
  })
  output$knn_Y <- renderUI({
    req(input$data_knnY)
    req(input$knn_type)

    data <- vals$saved_data[[input$data_knnY]]
    labels <- attr(data,"factors")
    choices<-if(input$knn_type=="Classification"){rev(colnames(labels))} else{
      colnames(data)
    }

    div(well="class2",
        div("Variable"),
        div(tipify(pickerInput(
          ns("knn_sup"),
          NULL,
          choices =choices ,
          width="150px", selected=vals$cur_response
        ),"The response vector"))
    )

  })
  output$knn_partition<-renderUI({
    req(input$data_knnX)

    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("knn_test_partition"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_knnY]],"factors"))), width="150px", selected=vals$cur_test_partition))
    )
  })
  output$knn_test_ref<-renderUI({
    req(input$knn_test_partition!="None")
    req(input$data_knnY)

    fac<-attr(vals$saved_data[[input$data_knnY]],"factors")[,input$knn_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_knn"),NULL, choices=choices, width="200px", selected=vals$cur_testdata)
    )
  })
  output$data_knnX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_knnX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
        )
      ),      inline(uiOutput(ns("saved_knns")))

    )
  })

  output$saved_knns<-renderUI({

    req(input$data_knnX)
    names_knn<-names(attr(vals$saved_data[[input$data_knnX]],"knn"))
    req(length(names_knn)>0)
    pic<-which(names_knn%in%"new knn (unsaved)")
    if(length(pic)>0){
      names_knn<-names_knn[-pic]
    }
    req(length(names_knn)>0)
    div(class="saved_models",
        icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names_knn)), "saved model(s)")
  })

  output$knn_params<- renderUI({
    column(12,class="well2",
           div(class="map_control_style",style="color: #05668D; margin-top: 20px",
               div(
                 div(
                   tipify(icon("fas fa-question-circle"),"Tuning search", options = list(container="body")),
                   "+ search:",
                   inline(
                     pickerInput(ns("knn_search"), NULL, choices = c("grid","random"), width="110px")
                   )
                 ),
                 uiOutput(ns("knn_search_length")),
               )               ,
               div(
                 tipify(icon("fas fa-question-circle"),textseed(), options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seedknn"), NULL, value = NULL, width="122px")
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
               pickerInput(ns("knn_res_method"),NULL, choices=list("repeatedcv","boot","LOOCV","LGOCV"), width = "100px")
               ),
               uiOutput(ns("knn_resampling"))
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("knn_war"))
           ),

           column(12, align = "center",
                  popify(actionButton(ns("trainknn"), h4(img(src=knn_icon,height='20',width='20'),"train KNN",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")
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
        title=div("Train models using variables in",input$data_knnY),
        easyClose=T,

        div(
          column(12,
                 "This action will train",strong(ncol(getyloop()), style="color: red")," models, using the columns of Datalist: ",strong(input$data_knnY),"as Y. The given name of the models will be (",input$data_knnY,"::Y~Datalist_X), and any model already saved with this name(s) will be replaced.",em("We suggest using this tool from a Datalist without any saved knn model", style="color: red")),
          column(12,
                 div(strong("Click to proceed:")),
                 actionButton(ns("train_loop"),"Train_loop_YDatalist"))
        )
      )
    )

  })

  observeEvent(ignoreInit = T,input$train_loop,{
    removeModal()
    trainknn_loop()
  })

  getyloop<-reactive({
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #input<-readRDS("input.rds")
    #vals<-readRDS("vals.rds")
    y<-vals$saved_data[[input$data_knnY]]
    if(input$knn_type== 'Classification'){y<-attr(y,"factors")}
    if(input$knn_test_partition!="None"){
      if(input$knn_type== 'Classification'){
        pic<-which(colnames(y)==input$knn_test_partition)
        if(length(pic)>0){
          y<-y[,-pic, drop=F]}
      }
    }
    y_loop<-y
    y_loop
  })

  trainknn_loop<-reactive({
    #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #vals<-readRDS("vals.rds")
    #input<-readRDS("input.rds")
    y_loop<-getyloop()
    withProgress(message="Running",max=ncol(y_loop),{
      i=1
      for(i in 1:ncol(y_loop)) {
        t<-try({
          data<-getdata_knnX()
          x<-x_o<-data.frame(data)
          y<-y_o<-y_loop[,i]

          output$knn_war<-renderUI({
            column(12,style="color: red",align="center",
                   if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
                   if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
            )
          })
          validate(need(anyNA(x)==F,"NAs not allowed in X"))
          validate(need(anyNA(y)==F,"NAs not allowed in Y"))
          req(input$knn_test_partition)
          if(input$knn_test_partition!="None"){
            parts<-get_parts_knn()
            train<-parts$train
            test<-parts$test
            x<-data.frame(getdata_knnX()[train,])
            y<-y_o[train]
          }
          #readrd(y,"y.rds")
          #saveRDS(x,"x.rds")
          #saveRDS(reactiveValuesToList(input),"input.rds")
          #y<-readRDS("y.rds")
          #x<-readRDS("x.rds")
          #input<-readRDS("input.rds")

          seed<-if (!is.na(input$seedknn)) { input$seedknn} else{
            NULL
          }
          knn_search<-input$knn_search

          colnames(x)<-gsub(" ",".", colnames(x))
          if (!is.na(input$seedknn)) {set.seed(input$seedknn)}


          knn<-train(x,y,'knn',

                     trControl=trainControl(

                       method = input$knn_res_method,
                       number = input$cvknn,
                       repeats = input$repeatsknn,
                       p=input$pleaveknn/100,
                       savePredictions = "all",
                       search=input$knn_search


                     ),
                     tuneLength=input$knn_tuneLength
          )
          attr(knn,"test_partition")<-paste("Test data:",input$knn_test_partition,"::",input$testdata_knn)
          attr(knn,"Y")<-paste(input$data_knnY,"::",colnames(y_loop)[i])
          attr(knn,"Datalist")<-paste(input$data_knnX)

          vals$knn_unsaved<-knn

          if(input$knn_test_partition!="None"){
            attr(vals$knn_unsaved,'test')<-x_o[test,]
            attr(vals$knn_unsaved,"sup_test")<-y_o[test]
          } else{ attr(vals$knn_unsaved,'test')<-c("None")}
          vals$bag_knn<-T
          attr(vals$knn_unsaved,"supervisor")<-colnames(y_loop)[i]
          attr(vals$knn_unsaved,"inputs")<-list(
            Ydatalist=input$data_knnY,
            Y=colnames(y_loop)[i],
            Xdatalist=input$data_knnX
          )

          attr( vals$knn_unsaved,"Y")<-paste0(input$data_knnY,"::",colnames(y_loop)[i])
          saveknn_loop()
          beep(10)
          #saveRDS(vals$nb_unsaved,"nb.rds")
        })



        if("try-error" %in% class(t)){
          output$knn_war<-renderUI({
            column(12,em(style="color: gray",
                         "Error in training the knn model. Check if the number of observations in X and Y are compatible"
            ))
          })

        } else{
          output$knn_war<-NULL
        }
        incProgress(1)
      }
      updateTabsetPanel(session,"knn_tab","knn_tab2")
    })

  })


  saveknn_loop<-reactive({
    temp<-vals$knn_unsaved
    name_model<-paste0(attr(temp,"Y"),"~",input$data_knnX)
    temp<-list(temp)
    names(temp)<-name_model
    attr(vals$saved_data[[input$data_knnX]],"knn")[[name_model]]<-temp
    cur<-name_model
    attr(vals$saved_data[[input$data_knnX]],"knn")['new knn (unsaved)']<-NULL
    vals$bag_knn<-F
    vals$cur_knn_models<-cur
    vals$cur_knn<-cur
    vals$knn_unsaved<-NULL
  })
  saveknn<-reactive({
    req(vals$cur_knn_models=='new knn (unsaved)')
    temp<-vals$knn_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[input$data_knnX]],"knn")[[input$newdatalist]]<-temp
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[input$data_knnX]],"knn")[[input$over_datalist]]<-temp
      cur<-input$over_datalist
    }
    attr(vals$saved_data[[input$data_knnX]],"knn")['new knn (unsaved)']<-NULL
    vals$bag_knn<-F
    vals$cur_knn_models<-cur
    vals$cur_knn<-cur

  })

  output$knn_search_length<-renderUI({
    if(is.null(vals$cur_knn_tuneLength)){vals$cur_knn_tuneLength<-5}

    div(
      tipify(icon("fas fa-question-circle"),"The maximum number of tuning- combinations that will be generated", options = list(container="body")),
      "+ tuneLength:",
      inline(
        numericInput(ns("knn_tuneLength"), NULL, value = vals$cur_knn_tuneLength, width="82px")
      )
    )
  })
  output$knn_resampling<-renderUI({
    div(
      inline(
        if(input$knn_res_method=='cv'|input$knn_res_method=='repeatedcv')
        {
          div(

            tipify(icon("fas fa-question-circle"),"number of folds", options=list(container="body")),"+ cv:",
            numericInput(ns("cvknn"), NULL, value = 5,width="140px")

          )
        }
      ),
      div(
        inline(
          if(input$knn_res_method=='repeatedcv')
          {
            inline(
              div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute", options=list(container="body")),
                  "+ repeats:",
                  numericInput(ns("repeatsknn"),NULL, value = 1,width="109px")
              )
            )

          }
        )
      ),
      inline(
        if(input$knn_res_method=='boot'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the number of resampling iterations", options=list(container="body")),
              "+ number:",
              numericInput(ns("cvknn"), NULL, value = 5,width="100px")

            )
          )
        }
      ),
      inline(
        if(input$knn_res_method=='LGOCV'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the training percentage", options=list(container="body")),
              "+ percentage:",
              numericInput(ns("pleaveknn"), NULL, value = 10,width="100px")
            )
          )
        }
      )
    )
  })


  output$knn_results_menu<-renderUI({
    if(is.null(vals$cur_knn_models)){vals$cur_knn_models<-1}
    div(pickerInput(ns("knn_models"),strong("knn results:", tiphelp("KNN Results. Click to select knn results saved in the Training Datalist (X).")), choices= names(attr(vals$saved_data[[input$data_knnX]],"knn")), width="200px", selected = vals$cur_knn_models)
    )
  })
  output$rmknn_button<-renderUI({
    req(input$knn_models!="new knn (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmknn"), icon("far fa-trash-alt")), "Remove the knn model from the training Datalist (X)")
    )

  })
  output$knn_tab_2_1<-renderUI({
    m<-vals$knn_results
    div(
      renderPrint(m),
      div(style="height: 100px",
        popify(downloadButton(ns("down_knn_results"),label=span("Download knn model",img(src=knn_icon,height='20',width='20')),style = "button_active"),NULL,
               HTML(paste0(
                 div(HTML(paste0("Download model as ",em('rds')," file to be read in R as", code('base::readRDS(file.rds)'))))
               )))
      )

    )
  })


  output$down_knn_results <- {
    downloadHandler(
      filename = function() {
        paste0("knn","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$nb_results,file)
      })
  }
  observeEvent(ignoreInit = T,input$knn_tab2,{
    vals$knn_tab2<-input$knn_tab2
  })

  output$results_knn<-renderUI({
    validate(need(length(vals$knn_results)>0,"No models have been trained yet"))
    if(is.null(vals$knn_tab2)){vals$knn_tab2<-'knn_tab2_1'}
    #saveRDS(vals$knn_results,"knn.rds")
    req(length(vals$knn_results))
    m<-vals$knn_results
    #m<-readRDS("knn.rds")
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    column(12,
           div(
             p(
               strong("Model:"),em(getmodel_knn())
             )
           ),
           tabsetPanel(id=ns("knn_tab2"),selected=vals$knn_tab2,
                       tabPanel("2.1. Summary",value="knn_tab2_1",
                                uiOutput(ns("knn_tab_2_1"))
                       ),
                       tabPanel("2.2. Performace",value="knn_tab2_2",
                                uiOutput(ns("knn_tab_2_2"))
                       ),
                       tabPanel("2.3. Permutation importance",value="knn_tab2_3",
                                uiOutput(ns("permutation_importance"))

                       ),
                       tabPanel("2.4. Confusion matrix",value="knn_tab2_4",
                                uiOutput(ns("knn_tab_2_4"))
                       )
           )

    )
  })




  ##
  output$permutation_importance<-renderUI({
    req(input$knn_models)
    validate(need(input$knn_models!="new knn (unsaved)","
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
                                 ns('svm_varImp'),span("+ Download table"),style="button_active"
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

  observeEvent(ignoreInit = T,input$down_svm_tab3_1,{
    vals$hand_down<-"SVM - predictions"
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
            ns('svm_varImp_plot'),span('+ Download plot'),style="button_active"
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
  observeEvent(ignoreInit = T,input$svm_varImp_plot,{
    vals$hand_plot<-"svm - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })
  output$feature_class<-renderUI({

    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("feac_side"))
      ),
      mainPanel(  uiOutput(ns("feac_plot")))
    )

  })
  output$feac_side<-renderUI({
    m<-vals$knn_results
    obs<-getdata_model(m,'test')

    div(class="map_control_style",style="color: #05668D",
        div("+ Class:",

            pickerInput(inputId = ns("feac_class"),
                        label = NULL,
                        choices = levels(obs)    ,selected=vals$feac_class, options=list(container="body"), width='200px')

        ),
        uiOutput(ns('side_filter_feat_class_nvars')),
        uiOutput(ns('side_feat_class_pal_plot')),
        uiOutput(ns('side_feat_class_axes_plot')),

        div(
          actionLink(
            ns('downp_feat_class'),span('+ Download plot'),style="button_active"
          )
        )

    )
  })
  observeEvent(ignoreInit = T,input$feac_class,{
    vals$feac_class<-input$feac_class
  })
  output$side_feat_class_pal_plot<-renderUI({
    div(
      div(
        span("+ Palette",
             inline(
               pickerInput(inputId = ns("pal_featc"),
                           label = NULL,
                           choices =     vals$colors_img$val,
                           choicesOpt = list(content =     vals$colors_img$img), options=list(container="body"), width="75px")
             )
        )
      )
    )
  })
  output$side_feat_class_axes_plot<-renderUI({
    div(
      div("+ Axes size",inline(numericInput(ns("featc_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("featc_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",inline(numericInput(ns("featc_cex.leg"),NULL, value=13, width="75px", step=1))),


    )
  })
  output$side_filter_feat_class_nvars<-renderUI({


    div( div(
      span("+ Number of variables:",tiphelp("Filter variables with the highest mean importance"),
           inline(
             numericInput(ns("featc_npic"),NULL, value=20, width="75px", step=1)
           )
      )
    ),
    div(
      span("+ Significance level",
           inline(
             numericInput(ns("featc_sig"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    ))
  })
  observeEvent(ignoreInit = T,input$downp_feat_class,{
    vals$hand_plot<-"svm - Variable Importance by class"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })
  output$feac_plot<-renderUI({

    model_list<- attr(vals$saved_data[[input$data_svmX]],"svm")[[input$svm_models]]
    req(input$featc_npic)
    sig_level<-input$featc_sig

    req(length(model_list)>0)
    m<-model_list[[1]]
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    predtable<-df<-model_list$feature_rands
    df<-permimp_metric(df,metric=getmetric(), class=input$feac_class,m,newdata,obc)

    req(length(df)>0)
    req(is.data.frame(df))


    rands<-df
    perm_summary<-sig_feature(df,m, newdata, obc,sig_level)
    sig_names<-rownames(perm_summary)[perm_summary$sig=="*"]
    df<-get_feature_data_plot(df,m,newdata,obc, npic=input$featc_npic)
    col<-vals$newcolhabs[[input$pal_featc]](2)
    req(nrow(df)>1)
    df$sig<-"non_sig"
    df$sig[    df$var%in%sig_names    ]<-"sig"
    df$var[ which( df$var%in%sig_names)]<-paste0("*",df$var[  which( df$var%in%sig_names)])
    req(length(df)>0)
    renderPlot({
      df<-df

      p<-ggplot(df, aes(reorder(var,Metric), Metric)) +
        geom_boxplot(aes(fill=sig))+coord_flip()+labs(x="Variables", y='Performance decay')+ggtitle("Feature Shuffling Impact")+theme(
          axis.text=element_text(size=input$featc_cex.axes),
          axis.title=element_text(size=input$featc_cex.lab)
        )+scale_fill_manual(values=col)

      vals$svm_varImp_plot<-p
      vals$svm_varImp_plot

    })

  })
  output$feature_cm<-renderUI({
    m<-vals$knn_results
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
    model_list<- attr(vals$saved_data[[input$data_svmX]],"svm")[[input$svm_models]]
    req(length(model_list)>0)
    predtable<-model_list$feature_rands
    dft<-get_cm_impact(predtable, var=input$var_feature_cm)

    renderPlot({
      vals$feature_cm_plot<- plotCM(dft, palette=input$pal_feature_cm,vals$newcolhabs, font_color="black", title="", round_cm=3)
      vals$feature_cm_plot
    })
  })
  getmetric<-reactive({
    m<-vals$knn_results
    metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
    metric
  })
  get_sumamry_permute_importance<-reactive({
    req(input$sig_feature)
    model_list<- attr(vals$saved_data[[input$data_svmX]],"svm")[[input$svm_models]]
    sig_level<-input$sig_feature
    req(length(model_list)>0)
    df<-model_list$feature_rands
    m<-model_list[[1]]
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    df<-permimp_metric(df,metric=getmetric(), class=NULL,m, newdata,obc)

    req(length(df)>0)
    req(is.data.frame(df))

    vals$svm_varImp<-perm_summary<-sig_feature(df,m, newdata, obc,sig_level)
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
    m<-vals$knn_results
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    rep=input$feat_rep
    seed=input$feat_seed
    if(is.na(seed)){seed=NULL}
    predtable<- permute_importance_foreach_progress(m,newdata,obc, rep, seed=seed)
    stopCluster(cl)
    unregister_dopar()
    attr(vals$saved_data[[input$data_knnX]],"knn")[[input$knn_models]]$feature_rands<-predtable
  })
  output$feature_importance_plot<-renderUI({

    req(input$data_knnX)
    req(input$knn_models)
    model_list<- attr(vals$saved_data[[input$data_knnX]],"knn")[[input$knn_models]]
    req(length(model_list)>0)
    req(input$feat_npic)
    sig_level<-input$sig_feature

    req(length(model_list)>0)
    df<-model_list$feature_rands
    validate(need(length(df)>0,"Analyses pending"))
    m<-model_list[[1]]
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
    df<-permimp_metric(df,metric=metric, class=NULL, m,newdata,obc)

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

      vals$knn_varImp_plot<-p
      vals$knn_varImp_plot

    })
  })

  ##

  output$knn_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('knn_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the knn training errors", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('knn_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "+ Download Table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('knn_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('knn_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          tags$style(
            paste(paste0("#",ns('knn_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('knn_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          div(
            p(strong("Global:")),
            inline(DT::dataTableOutput(ns('knn_tab_errors0_train')))
          ),

          div(
            p(strong("Observations:")),
            inline(DT::dataTableOutput(ns('knn_tab_errors_train')))
          )



        )
      )
    )
  })
  output$knn_tab_errors0_train<-DT::renderDataTable({
    m<-vals$knn_results
    table<- m$results[rownames(m$bestTune),]
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })
  output$knn_tab_errors_train<-DT::renderDataTable({
    m<-vals$knn_results
    table<-accu_rf_class(m)
    vals$knn_down_errors_train<-table
    table
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })

  output$knn_tab_2_4<-renderUI({

    # validate(need(is.factor( vals$knn_results$finalModel$y), "Confusion matrices are only valid for classification (factor) models."))
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          class="map_control_style",style="color: #05668D",
          div(
            span(
              "+ Type: ",
              pickerInput(inputId = ns("knn_cm_type"),
                          label = NULL,
                          choices = c("Resampling","Optimal Model"),

                          width="100px"
              )
            )
          ),
          div(
            span(
              "+ Palette",
              pickerInput(inputId = ns("knnpalette"),
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
            actionLink(ns("downp_cmknn_pred"),span("+ Donwload plot"),style  = "button_active")
          ),
          div(
            actionLink(ns("dowcenter_cmknn_pred"),span("+ Download Table"),style  = "button_active"))

        )

      ),
      mainPanel(
        plotOutput(ns("confusion_knn")),
        verbatimTextOutput(ns("confusion_knn2"))
      )
    )


  })

  observeEvent(ignoreInit = T,input$dowcenter_cmknn,{
    vals$hand_down<-"knn_cm"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  output$confusion_knn <- renderPlot({
    validate(need(vals$knn_results$modelType=="Classification","Confusion matrices are only valid for classification models."))
    m<-vals$knn_results
    if(input$knn_cm_type=="Resampling"){
      res<-plotCM(m, input$knnpalette,  newcolhabs=vals$newcolhabs)
    } else{
      cm<-table(predict(vals$knn_results),vals$knn_results$trainingData[,'.outcome'])
      res<-plotCM(cm,input$knnpalette,vals$newcolhabs)
    }
    vals$cm_knn<-res
    res
  })
  output$confusion_knn2<-renderPrint({
    validate(need(vals$knn_results$modelType=="Classification","Confusion matrices are only valid for classification models."))

    res<-if(input$knn_cm_type=="Resampling"){
      confusionMatrix(vals$knn_results$pred$pred,vals$knn_results$pred$obs)
    } else{
      confusionMatrix(predict(vals$knn_results),vals$knn_results$trainingData[,'.outcome'])
    }
    vals$knn_cm<-as.data.frame.matrix(res$table)
    res
  })



  output$saveknn_button<-renderUI({
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_saveknn"), icon("fas fa-save")), "Save the knn model in the training Datalist (X)")
    )
  })

  getnewdataknn<-reactive({
    datalist<-vals$saved_data

    datalist_comp<-lapply(
      vals$saved_data, function (x) {
        colnames(x)<-gsub(" ",".", colnames(x))
        x
      }

    )

    datalist_comp<-vals$saved_data




    m<-vals$knn_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-Y])
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })
  get_supervisor_knn <- reactive({
    m<-vals$knn_results
    att<-if(m$modelType=="Classification")
    {
      data <- vals$saved_data[[input$data_knnY]]
      labels <- attr(data,"factors")
      labels[input$knn_sup]
    } else{
      data <- vals$saved_data[[input$data_knnY]]
      data[input$knn_sup]
    }

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
  get_cm_pred<-reactive({
    obs<-get_knn_prederrors()$obs
    pred<-get_knn_prederrors()$pred

    conf<-table(obs, pred)
    conf
  })


  getobsknn<-reactive({
    sup_test<-attr(vals$knn_results,"supervisor")

    datalist<-vals$saved_data
    if(vals$knn_results$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}

    m<-vals$knn_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })

  pred_knn<-reactive({
    validate(need(!anyNA(test_knn()),"NAs not allowed in the prediction Datalist"))
    m<-vals$knn_results

    pred_tab= test_knn()
    knn_pred <- predict(m,newdata=as.matrix(pred_tab))
    res<-data.frame(Predictions= knn_pred)
    rownames(res)<-rownames(test_knn())
    #colnames(res)<-attr(vals$knn_results,"supervisor")

    # attr(res,"obs")<-test_knn()
    res



  })

  getmodel_knn<-reactive({
    m<-vals$knn_results
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


  observeEvent(ignoreInit = T,input$predknn_new,{
    vals$predknn_new<-input$predknn_new
  })
  observeEvent(ignoreInit = T,input$knnpred_which,{
    vals$knnpred_which<-input$knnpred_which
  })
  observeEvent(ignoreInit = T,input$knn_down_errors_train,{
    vals$hand_down<-"knn - training errors (observations)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$knn_varImp,{
    vals$hand_down<-"knn - variable importance"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_knn_tab3_1,{
    vals$hand_down<-"knn - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$data_knnY,{
    vals$cur_dataY<-input$data_knnY
  })
  observeEvent(ignoreInit = T,input$knn_type,{
    vals$cur_model_type<-input$knn_type
  })
  observeEvent(ignoreInit = T,input$knn_tuneLength,{
    vals$cur_knn_tuneLength<-input$knn_tuneLength
  })

  observeEvent(ignoreInit = T,input$knn_sup,{
    vals$cur_response<-input$knn_sup
  })
  observeEvent(ignoreInit = T,input$data_knnX,{
    vals$cur_data<-input$data_knnX
  })
  observeEvent(ignoreInit = T,input$knn_test_partition,{
    vals$cur_test_partition<-input$knn_test_partition
  })
  observeEvent(ignoreInit = T,input$testdata_knn,{
    vals$cur_testdata<-input$testdata_knn
  })
  observeEvent(ignoreInit = T,input$knn_models,{
    vals$cur_knn_models<-input$knn_models
  })

  getdata_knnX<-reactive({
    req(input$data_knnX)
    data=vals$saved_data[[input$data_knnX]]
    data
  })
  getdata_knnY<-reactive({
    req(input$data_knnY)
    req(input$knn_sup)

    data <- vals$saved_data[[input$data_knnY]]
    if(input$knn_type=="Classification"){
      labels <- attr(data,"factors")
      res<-labels[rownames(vals$saved_data[[input$data_knnX]]),input$knn_sup]
      names(res)<-rownames(vals$saved_data[[input$data_knnX]])
      res
    } else{
      res<-data[rownames(vals$saved_data[[input$data_knnX]]),input$knn_sup]
      names(res)<-rownames(vals$saved_data[[input$data_knnX]])
      res
    }

  })
  get_parts_knn<-reactive({
    req(input$data_knnY)
    req(input$testdata_knn)
    req(input$knn_test_partition)
    data<-getdata_knnX()
    factors<-attr(vals$saved_data[[input$data_knnY]],"factors")[rownames(data),]
    if(input$knn_test_partition!='None'){
      lis<-split(rownames(factors),factors[,input$knn_test_partition])
      names(lis[[1]])<-lis[[1]]
      names(lis[[2]])<-lis[[2]]
      test_i<-which(names(lis)==input$testdata_knn)
      tra_i<-which(names(lis)!=input$testdata_knn)
      parts=list(train=lis[[tra_i]],test=lis[[test_i]])} else{
        parts=list(train=NULL,test=NULL)
      }

    parts

  })

  observeEvent(ignoreInit = T,input$trainknn,{
    t<-try({

      vals$cur_knn_models<-"new knn (unsaved)"
      updatePickerInput(session,"knn_models",selected= vals$cur_knn_models)
      x<-x_o<-data.frame(getdata_knnX())
      y<-y_o<-getdata_knnY()

      output$knn_war<-renderUI({
        column(12,style="color: red",align="center",
               if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
               if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
        )
      })
      validate(need(anyNA(x)==F,"NAs not allowed in X"))
      validate(need(anyNA(y)==F,"NAs not allowed in Y"))
      req(input$knn_test_partition)
      if(input$knn_test_partition!="None"){
        parts<-get_parts_knn()
        train<-parts$train
        test<-parts$test
        x<-data.frame(getdata_knnX()[train,])
        y<-getdata_knnY()[train]
      }
      #readrd(y,"y.rds")
      #saveRDS(x,"x.rds")
      #saveRDS(reactiveValuesToList(input),"input.rds")
      #y<-readRDS("y.rds")
      #x<-readRDS("x.rds")
      #input<-readRDS("input.rds")

      seed<-if (!is.na(input$seedknn)) { input$seedknn} else{
        NULL
      }
      knn_search<-input$knn_search

      colnames(x)<-gsub(" ",".", colnames(x))
      if (!is.na(input$seedknn)) {set.seed(input$seedknn)}
      withProgress(message = "Running Support Vector Machine ...",
                   min = 1,
                   max = 1,
                   {
                     knn<-train(x,y,'knn',

                                trControl=trainControl(

                                  method = input$knn_res_method,
                                  number = input$cvknn,
                                  repeats = input$repeatsknn,
                                  p=input$pleaveknn/100,
                                  savePredictions = "all",
                                  search=input$knn_search


                                ),
                                tuneLength=input$knn_tuneLength
                     )
                     attr(knn,"test_partition")<-paste("Test data:",input$knn_test_partition,"::",input$testdata_knn)
                     attr(knn,"Y")<-paste(input$data_knnY,"::",input$knn_sup)
                     attr(knn,"Datalist")<-paste(input$data_knnX)

                     vals$knn_unsaved<-knn

                     if(input$knn_test_partition!="None"){
                       attr(vals$knn_unsaved,'test')<-x_o[test,]
                       attr(vals$knn_unsaved,"sup_test")<-y_o[test]
                     } else{ attr(vals$knn_unsaved,'test')<-c("None")}
                     vals$bag_knn<-T
                     attr(vals$knn_unsaved,"supervisor")<-input$knn_sup
                     attr(vals$knn_unsaved,"inputs")<-list(
                       Ydatalist=input$data_knnY,
                       Y=input$knn_sup,
                       Xdatalist=input$data_knnX
                     )
                     updateTabsetPanel(session,"knn_tab","knn_tab2")
                   })
      beep(10)
      #attr(vals$saved_data[[input$data_knnX]],"knn")[['new knn (unsaved)']]<-vals$knn_unsaved
      vals$cur_knn_models<-"new knn (unsaved)"
      vals$bag_knn<-T
      attr(vals$saved_data[[input$data_knnX]],"knn")[['new knn (unsaved)']]<-vals$knn_unsaved

      #saveRDS(vals$knn_unsaved,"knn.rds")

    })
    if("try-error" %in% class(t)){
      output$knn_war<-renderUI({
        column(12,em(style="color: gray",
                     "Error in training the knn model. Check if the number of observations in X and Y are compatible"
        ))
      })

    } else{
      output$knn_war<-NULL
    }
  })
  observeEvent(ignoreInit = T,input$downp_cmknn,{
    vals$hand_plot<-"knn - Confusion Matrix"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$downp_cmknn_pred,{
    vals$hand_plot<-"knn - Confusion Matrix (predictions)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$knn_varImp_plot,{
    vals$hand_plot<-"knn - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_knn_dens,{
    vals$hand_plot<-"knn - DM plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observe({
    req(input$knn_models)
    if(input$knn_models=="new knn (unsaved)"){
      vals$knn_results<-vals$knn_unsaved } else{
        vals$knn_results<-attr(vals$saved_data[[input$data_knnX]],"knn")[[input$knn_models]][[1]]
      }
  })
  observeEvent(ignoreInit = T,input$knn_models,{
    vals$cur_knn<-input$knn_models
  })
  observeEvent(ignoreInit = T,input$tools_rmknn,{
    attr(vals$saved_data[[input$data_knnX]],"knn")[input$knn_models]<-NULL
  })
  observeEvent(ignoreInit = T,input$tools_saveknn,{
    if(input$tools_saveknn %% 2){
      vals$hand_save<-"Save knn model in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_knnX,style="color:gray"),strong("::"),em("knn-Attribute",style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL
      showModal(module_knn())
    }
  })
  observeEvent(ignoreInit = T,input$knn_create_errors_train,{
    vals$hand_save<-"Create Datalist: knn training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_knn())
  })
  observeEvent(ignoreInit = T,input$knn_create_predictions,{
    vals$hand_save<- "Create Datalist: knn predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute, and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL
    showModal(module_knn())
  })



  resample_create<-reactive({

    m<-vals$knn_results
    var<-attr(m,"supervisor")
    temp<-vals$knn_treetest
    datao<-vals$saved_data[[input$data_knnX]]
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
    var<-attr(vals$knn_results,"supervisor")
    name0<-paste("knn",var,"qclass", sep="_")
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
      "Save knn model in"= { name_save_knn()},
      "Create Datalist: knn training errors -obs"={name_knn_train_errors()},
      "Create Datalist: knn predictions"={ name_knn_pred()},
      "resample_create"={name_resample_create()}
    )})

  knn_create_training_errors<-reactive({
    temp<-vals$knn_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$data_knnX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  knn_create_pred<-reactive({
    temp<-data.frame(vals$knntab_pred)
    temp[,1]<-as.numeric( temp[,1])
    attr(temp,"factors")<-temp

    datao<-if(vals$knnpred_which=="Datalist"){
      vals$saved_data[[vals$predknn_new]]
    } else{ vals$saved_data[[input$data_knnX]]}
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  name_save_knn<-reactive({
    bag<-1
    name0<-paste0("knn")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[input$data_knnX]],"knn"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[input$data_knnX]],"knn"))) break
      }}
    paste(name0,bag)
  })
  name_knn_train_errors<-reactive({
    bag<-1
    name0<-paste0("knn training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_knn_pred<-reactive({
    bag<-1
    name0<-paste0("knn predictions")
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
    if(vals$hand_save=='Save knn model in'){choices<-names(attr(vals$saved_data[[input$data_knnX]],'knn'))}
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
      "Save knn model in"= {saveknn()},
      "Create Datalist: knn training errors -obs"=knn_create_training_errors(),
      "Create Datalist: knn predictions"={knn_create_pred()},
      'resample_create'={resample_create()}

    )
    removeModal()

  })
  module_knn <- function() {
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
