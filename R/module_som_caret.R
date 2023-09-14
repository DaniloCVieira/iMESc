
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
module_ui_som2 <- function(id){

  ns <- NS(id)
  tagList(
    includeCSS("inst/app/www/styles.css"),
    #inline( actionButton(ns("teste_comb"),"SAVE")),uiOutput(ns("bug")),

    uiOutput(ns("som_panels"))
  )

}


#' @export
module_server_som2 <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns
  ns_som2 <- NS('som2')


  symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
  df_symbol <- data.frame(
    val = c(16,15,17,18,8,1,5,3)
  )
  for(i in 1:length(symbols))
  {
    symbol1<-dataURI(file = paste0('inst/app/www/pch',i,".png"), mime = "image/png")

    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}

  tunesom<-reactiveValues(
    finetopo=F,
    finesom=F,
    dim=c(5,5),
    seed=NA,
    rlen=500,
    distmethod="BrayCurtis",
    toroidal="FALSE",
    neighbourhood.fct='bubble',
    a1=0.05,
    a2=0.01,
    r1=0,
    r2=0,
    mode="online",
    maxna=0.001,
    topo="hexagonal",
    sugtopo=T
  )


  observeEvent(ignoreInit = T,input$teste_comb,{
    savereac()
  })



  toroidal<-reactive({
    switch (tunesom$toroidal,
            'TRUE' = TRUE,
            "FALSE" = FALSE)
  })
  output$som_panels<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    column(12,style="background: white",
           uiOutput(ns("som_inputs")),
           uiOutput(ns("war_som")),
           column(12,
                  tabsetPanel(id = ns("som_tab"),
                              tabPanel(
                                strong("1. Training"),value='som_tab1',
                                splitLayout(uiOutput(ns('som_sideleft')),
                                            uiOutput(ns("som_sideright")))
                              ),
                              tabPanel(strong("2. Results"),value='som2_results',
                                       uiOutput(ns("results_som"))),
                              tabPanel(strong("3. Predict"),value='som_tab3',
                                       uiOutput(ns("predict_som")))
                  )
           )

    )
  })
  insert_som_resutls<-reactiveValues(df=F)




  output$predict_som<-renderUI({
    column(12,style="background: white", uiOutput(ns("som_pred_type")),
           tabsetPanel(id="predsom_tab",
                       tabPanel(
                         strong("3.1. Results"), value="predsom_tab01",
                         fluidRow(style="background: white",
                                  uiOutput(ns("som_tab3_1")))),
                       tabPanel(
                         strong("3.2. Performace"),
                         fluidRow(style="background: white",
                                  uiOutput(ns("som_tab3_2")))
                       )

           )
    )
  })
  output$som_tab3_2<-renderUI({
    sup_test<-attr(vals$xyf_results,"supervisor")
    supname<-paste0("Observed Y [",sup_test,"]")
    lab1<-span("+",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
               span(supname))

    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            pickerInput(ns("predsom_newY"),
                        lab1,names(vals$saved_data[getobssom()])),
            uiOutput(ns("som_pred_pal")),
            numericInput(ns("round_pred_som"),"+ Round",3,step=1),
            actionLink(ns("down_metrics"),span("+ Donwload Metrics",icon("fas fa-table"))),
            uiOutput(ns("down_cm_btn")),

        )),
      mainPanel(
        fluidRow(
          column(3,style="margin: 0px; padding: 0px",
                 inline(DT::dataTableOutput(ns('som_tab_errors0_test')))
          ),
          column(9,uiOutput(ns("confusion_matrix_som"))
          )
        )
      )
    )
  })
  get_som_prederrors<-reactive({
    req(length(input$sompred_which)>0)

    m<-vals$xyf_results
    pred<-pred_som()[,1]
    # x_t<-attr(m,"test")
    obs<-if(input$sompred_which=="Partition"){
      attr(m,"sup_test")} else{
        req(length(input$predsom_newY)>0)

        if(m$modelType=="Classification"){
          req(length(attr(m,"supervisor"))>0)
          req(length(input$predsom_newY)>0)
          req(input$predsom_newY%in%names(vals$saved_data))
          sup<-attr(m,"supervisor")
          factors<-attr(vals$saved_data[[input$predsom_newY]],"factors")
          req(sup%in%colnames(factors))
          factors[,sup]} else{
            vals$saved_data[[input$predsom_newY]][,attr(m,"supervisor")]
          }
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })
  test_som<-reactive({
    req(input$sompred_which)
    m<-vals$xyf_results
    pred_tab<-if(input$sompred_which=="Partition"){attr(m,"test")} else if(input$sompred_which=="Datalist"){
      req(input$predsom_new)
      pred_tab<-vals$saved_data[[input$predsom_new]]
    }
    model_data<-pred_tab
    req(sum(colnames(pred_tab)%in%colnames(getdata_model(m)))==ncol(pred_tab))
    pred_tab

  })
  output$som_tab_errors0_test<-DT::renderDataTable({
    req(input$round_pred_som)
    table<-pred_test_som()
    vals$sup_metrics<-table<-round(table,input$round_pred_som)
    #colnames(table)<-NULL
    DT::datatable(table,
                  options=list(rownames=T,info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'),colnames=c(""))
  })
  output$confusion_som_pred<-renderUI({
    req(vals$xyf_results$modelType=="Classification")

    conf<-get_cm_pred()
    column(12,
           renderPlot({
             res<-plotCM(conf/sum(conf)*100, input$sompalette_pred,  newcolhabs=vals$newcolhabs,round_cm=input$round_pred_som,title=input$som_cmpred_title)
             vals$som_cm_pred<-res
             res
           }))

  })
  output$confusion_som2_pred<-renderPrint({
    req(vals$xyf_results$modelType=="Classification")
    res<-confusionMatrix(get_cm_pred())
    vals$som_cm_test<-  as.data.frame.matrix(res$table)
    res
  })
  output$som_pred_type<-renderUI({
    choices=if(length(attr(vals$xyf_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(class="well2",style="padding: 10px",
        div(class="map_control_style2",style="color: #05668D",
            inline(radioButtons(ns("sompred_which"),strong("New data (X):"),choices=choices,inline=T)), inline(uiOutput(ns("datalist_pred_som")))
        )
    )
  })
  output$datalist_pred_som<-renderUI({
    req(input$sompred_which =='Datalist')
    div(
      pickerInput(ns("predsom_new"),"->",names(vals$saved_data[getnewdatasom()]), width = '300px', selected=vals$predsom_new)
    )
  })
  pred_test_som<-reactive({
    obs<-get_som_prederrors()$obs
    pred<-get_som_prederrors()$pred
    m<-vals$xyf_results

    table<-stats_ml(obs,pred,m)


    table
  })

  output$confusion_matrix_som<-renderUI({
    req(vals$xyf_results$modelType=="Classification")
    div(

      style="overflow-x: scroll",
      uiOutput(ns("confusion_som_pred")),
      verbatimTextOutput(ns("confusion_som2_pred"))

    )
  })
  gettile_cmpred<-reactive({
    req(input$sompred_which)
    ifelse(input$sompred_which=="Partition","Test","Confusion Matrix")
  })
  output$som_cmpred_title<-renderUI({
    textInput(ns("som_cmpred_title"),"+ CM Title",gettile_cmpred())
  })
  output$som_pred_pal<-renderUI({
    req(vals$xyf_results$modelType=="Classification")
    lab2<-span("+",tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"),"Palette")
    div(
      uiOutput(ns('som_cmpred_title')),
      pickerInput(inputId = ns("sompalette_pred"),
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
    req(vals$xyf_results$modelType=="Classification")

    div(
      div(actionLink(ns("dowcenter_cmsom_pred"),span("+ Download Confusion",icon("fas fa-table")))),
      div(actionLink(ns("downp_cmsom_pred"),span("+ Donwload plot"))
      )
    )

  })

  predall_som<-reactive({
    validate(need(!anyNA(test_som()),"NAs not allowed in the prediction Datalist"))
    m<-vals$xyf_results

    ctr<-data.frame(id=rownames(m$trainingData))
    rownames(ctr)<-1:nrow(m$trainingData)
    res0<-lapply(split(m$pred$pred,m$pred$rowIndex),function(x)data.frame(x))

    res<-t(do.call(cbind,res0))
    rownames(res)<-names(res0)
    rownames(res)<-ctr[rownames(res),]

    pred <- res
    pred
  })
  som_observed2<-reactive({
    req(input$sompred_which)


    obs_data<-if(input$sompred_which=="Datalist"){
      if(vals$xyf_results$modelType=="Classification"){
        factors<-attr(vals$saved_data[[input$predsom_newY_tree]],'factors')
      } else {
        factors<-vals$saved_data[[input$predsom_newY_tree]]}

      res<-factors[,attr(vals$xyf_results,"supervisor")]
      names(res)<-rownames(factors[attr(vals$xyf_results,"supervisor")])
      res
    } else{
      attr(vals$xyf_results,"sup_test")
    }
    obs_data
  })
  get_qclass<-reactive({
    m<-vals$xyf_results
    req(m$modelType=="Regression")
    pred<-predall_som()
    model_data<-test_som()
    obs_data<-som_observed2()
    obs_data<-data.frame(obs_data)
    ref1<-rownames(model_data)
    ref2<-rownames(obs_data)
    ref1<-ref2
    lo<-split(obs_data,ref2)

    pred_interval=input$q_class_val
    pred.som.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )


    lpred<-split(pred,ref1)
    lp<-split(pred.som.int,ref1)
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
    vals$som_treetest<-res
    vals$som_treetest
  })
  output$som_tree_show<-renderUI({
    val=if(vals$xyf_results$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$som_tree_pred<-renderUI({
    req(input$nhist_tree)
    data=t(predall_som())
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
    req(vals$xyf_results$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),'Mean predictions')

        ))
  })
  output$plot_forest<-renderUI({
    req(vals$xyf_results$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({
    req(input$qclass_width)
    req(input$qclass_height)
    div(style="background: white",
        inline(uiOutput(ns("som_tree_show"))),
        inline(uiOutput(ns("som_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))

  })
  output$forest_byclass<-renderUI({
    req(vals$xyf_results$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })
  get_forest_class<-reactive({
    req(input$predsom_newY_tree)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$xyf_results
    req(m$modelType=="Classification")
    pred<-predall_som()
    pred.ind <- pred
    model_data<-test_som()
    obs_data<-som_observed2()
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
  output$pick_somobs_pred<-renderUI({
    req(input$sompred_which=="Datalist")
    sup_test<-attr(vals$xyf_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("obs_cm_pred"),
                       NULL,names(vals$saved_data[getobssom()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })
  output$predsom_newY_ref<-renderUI({
    req(input$predsom_newY_tree)
    req(input$sompred_which)
    req(vals$xyf_results$modelType=="Regression")
    factors<-attr(vals$saved_data[[input$predsom_newY_tree]],"factors")
    choices=if(input$sompred_which=="Datalist"){
      c('rownames',colnames(factors))} else{
        'rownames'
      }
    div(
      span("+ ref",inline(pickerInput(ns('som_reftest'),NULL,choices, width="200px", selected=vals$som_reftest)))

    )})
  output$forest_margin<-renderUI({
    req(vals$xyf_results$modelType=="Regression")
    div(
      span("+ Margin:",
           inline(
             numericInput(ns("q_class_val"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    )
  })
  output$side_qclass_axes_plot<-renderUI({


    div(
      div("+ Text Color",
          inline(
            pickerInput(inputId = ns("qclass_textcolor"),
                        label = NULL,
                        choices =     vals$colors_img$val[getsolid_col()],
                        selected="black",
                        choicesOpt = list(content =     vals$colors_img$img[getsolid_col()]), options=list(container="body"), width="75px")
          )),
      div("+ Plot size",
          inline(numericInput(ns("qclass_size_points"),NULL, value=3, width="75px", step=1))),
      div("+ Plot title",
          inline(textInput(ns("qclass_main"), NULL, "", width="200px"))),
      div("+ Title size",
          inline(numericInput(ns("qclass_cex.main"),NULL, value=13, width="75px", step=1))),
      div("+ Axes size",
          inline(numericInput(ns("qclass_cex.axes"),NULL, value=13, width="75px", step=1))),
      div("+ Label size",
          inline(numericInput(ns("qclass_cex.lab"),NULL, value=14, width="75px", step=1))),
      div("+ Label size",
          inline(numericInput(ns("qclass_cex.leg"),NULL, value=13, width="75px", step=1))),
      div("+ Xlab",
          inline(textInput(ns("qclass_xlab"), NULL, 'Accuracy', width="200px"))),
      div("+ ylab",
          inline(textInput(ns("qclass_ylab"), NULL, 'Observations', width="200px"))),
      div("+ Legend title",
          inline(textInput(ns("qclass_leg"), NULL, "Model predictions", width="200px"))),
      div("+ Legend title",
          inline(textInput(ns("qclass_leg2"), NULL, "Final prediction", width="200px")))
    )
  })
  output$forest_col<-renderUI({
    req(vals$xyf_results$modelType=="Classification")
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
  output$pick_somobs<-renderUI({
    sup_test<-attr(vals$xyf_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("predsom_newY_tree"),
                       NULL,names(vals$saved_data[getobssom()]), width="200px",selected=vals$predsom_newY_tree
           ))
    )
  })
  output$resample_out<-renderUI({
    m<-vals$xyf_results
    validate(need(m$control$savePredictions=="all","This model was generated in an earlier version of iMESc. Please retrain the data to enable this functionality."))
    sidebarLayout(
      sidebarPanel(
        div(

          class="map_control_style",style="color: #05668D",
          uiOutput(ns("qclass_side")),
          uiOutput(ns("side_qclass_axes_plot")),

        )
      ),
      mainPanel(
        uiOutput(ns("qclass_out"))
      )
    )
  })
  output$qclass_side<-renderUI({

    div(

      uiOutput(ns("pick_somobs")),
      uiOutput(ns("predsom_newY_ref")),
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
    req(vals$xyf_results$modelType=="Regression")
    div(
      div(actionLink(ns('resamp_pred_gdownp'),"Download table")),
      div( actionLink(ns('resamp_pred_gcreate'),"Create Datalist"))
    )
  })
  output$summ_trees<-renderPlot({
    req(vals$xyf_results$modelType=="Regression")

    get_qclass()
    req(input$q_class_val)

    req(input$splitdata_trees)
    req(input$nhist_tree)
    pred<-predall_som()
    data=t(pred)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs_data<-som_observed2()
    obs_data<-data.frame(obs_data)
    obs=obs_data[ colnames(data),]

    pred <- pred
    pred_interval=input$q_class_val
    q_class=vals$som_treetest[colnames(data),]

    str_numerics3(numerics=data,
                  obs=obs_data[ colnames(data),],
                  pred_interval=pred_interval,
                  q_class=q_class,
                  cex=input$qclass_sizeplot)
    vals$vartrees<-recordPlot()
  })

  getsolid_col<-reactive({

    res<-lapply(vals$newcolhabs, function(x) x(10))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic

  })


  get_obc<-reactive({
    m<-attr(getdata_xyfX(),"xyf")[[as.character(input$xyf_models)]][[1]]
    switch (input$sompred_which,
            "Partition" = {
              obc<-data.frame(obs=attr(m,"sup_test"))
              rownames(obc)<-rownames(attr(m,"test"))
              obc
            },
            "Datalist" = {
              req(input$predsom_newY)
              data<-vals$saved_data[[input$predsom_newY]]
              var<-attr(m,"supervisor")
              if(m$modelType=="Regression"){
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
            }
    )






  })

  output$forestbyclass<- renderPlot({
    res_forest<-get_forest_class()
    obs<-get_obc()[,1]
    names(obs)<-rownames(get_obc())

    m<-attr(getdata_xyfX(),"xyf")[[as.character(input$xyf_models)]][[1]]
    pred<-pred_som()


    #args<-readRDS("args.rds")
    #args$pred
    result_forest<-t(apply(res_forest,1,function(x) x/sum(x)))

    obs<-obs[rownames(result_forest)]
    pred<-pred[rownames(result_forest),,drop=F]
    args<-list(
      result_forest=result_forest,
      pred=pred,
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
      obc=obs,
      size_points=input$qclass_size_points
    )
    #saveRDS(args,"args2.rds")
    # args<-readRDS('args2.rds')
    #args$m$pred

    #dim(args$result_forest)
    #args$pred

    #args0<-readRDS('args.rds')
    #args0$obc
    #args$obc
    #m<-args$m
    #beep()
    #plot_florest_class(res_forest,vals$newcolhabs, palette=input$forest_col, ylab="Resample")

    do.call(plotforest_qclass,args)


  })
  observeEvent(ignoreInit = T,input$predsom_newY_tree,{
    vals$predsom_newY_tree<-input$predsom_newY_tree
  })
  observeEvent(ignoreInit = T,input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_som())

  })
  observeEvent(ignoreInit = T,input$resamp_pred_gdownp,{

    vals$hand_down<-"som_qclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })

  observeEvent(ignoreInit = T,input$predsom_new,{
    vals$predsom_new<-input$predsom_new
  })


  observeEvent(ignoreInit = T,input$dowcenter_cmsom_pred,{
    vals$hand_down<-"som_cm_test"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })

  output$confusion_som2_pred<-renderPrint({
    validate(need(vals$xyf_results$modelType=="Classification","Confusion matrices are only valid for classification models."))
    res<-confusionMatrix(get_cm_pred())
    vals$som_cm_test<-  as.data.frame.matrix(res$table)
    res
  })

  output$som_tab3_1<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            class="map_control_style",style="color: #05668D",
            div(
              tipify(
                actionLink(
                  ns('som_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                ),
                "Create a datalist with the som predictions", options=list(container="body")
              )
            ),
            div(
              tipify(
                actionLink(
                  ns('down_som_tab3_1'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                ),
                "Download Table", options=list(container="body")
              )
            )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('somtab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('somtab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          inline(DT::dataTableOutput(ns('somtab_pred')))
        )
      )
    )
  })
  output$somtab_pred<-DT::renderDataTable({
    table<-vals$somtab_pred<-pred_som()
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))

  }, rownames = TRUE,class ='compact cell-border')


  output$som_inputs <- renderUI({

    if(is.null(vals$cur_model_type)){vals$cur_model_type<-"Classification"}
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",
        if(input$som_tab=="som_tab1"){
          div(
            div(class='well3',
                strong("Type:"),inline(radioButtons(ns('som_type'),NULL,choices=c("Classification","Regression"),inline =T, selected=vals$cur_model_type))
            ),
            div(class="well3",
                span(strong("X:"),
                     inline(uiOutput(ns('data_xyfX_out')))
                )
            ),
            div(class="well3",
                span(strong("Y:"),
                     inline(
                       uiOutput(ns("data_somY_out"))
                     ),
                     "::",
                     inline(uiOutput(ns('som_Y'))),

                     inline(uiOutput(ns("som_partition"))),
                     "::",
                     inline(uiOutput(ns("som_test_ref")))
                )
            )


          )
        } else {
          column(12,
                 inline( uiOutput(ns('data_xyfX_out'))),
                 inline(uiOutput(ns("xyf_results_menu"))),
                 inline(uiOutput(ns("savesom_button"))),
                 inline(uiOutput(ns("rmsom_button"))),
          )
        }
    )
  })
  output$data_somY_out<-renderUI({
    div(
      div("Datalist:"),
      div(pickerInput(ns("data_somY"),NULL,choices =  names(vals$saved_data), width="150px", selected=vals$cur_dataY))
    )
  })
  output$som_Y <- renderUI({
    req(input$data_somY)
    req(input$som_type)

    data <- vals$saved_data[[input$data_somY]]
    labels <- attr(data,"factors")
    choices<-if(input$som_type=="Classification"){rev(colnames(labels))} else{
      colnames(data)
    }

    div(well="class2",
        div("Variable"),
        div(tipify(pickerInput(
          ns("som_sup"),
          NULL,
          choices =choices ,
          width="150px", selected=vals$cur_response
        ),"The response vector"))
    )

  })
  output$som_partition<-renderUI({
    req(input$data_xyfX)

    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("som_test_partition"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_somY]],"factors"))), width="150px", selected=vals$cur_test_partition))
    )
  })
  output$som_test_ref<-renderUI({
    req(input$som_test_partition!="None")
    req(input$data_somY)

    fac<-attr(vals$saved_data[[input$data_somY]],"factors")[,input$som_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_som"),NULL, choices=choices, width="200px", selected=vals$cur_testdata)
    )
  })
  output$data_xyfX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_xyfX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
        )
      ),      inline(uiOutput(ns("saved_soms")))

    )
  })

  output$saved_soms<-renderUI({

    req(input$data_xyfX)
    names_som<-names(attr(vals$saved_data[[input$data_xyfX]],"xyf"))
    req(length(names_som)>0)
    pic<-which(names_som%in%"new som (unsaved)")
    if(length(pic)>0){
      names_som<-names_som[-pic]
    }
    req(length(names_som)>0)
    div(class="saved_models",
        icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names_som)), "saved model(s)")
  })

  observeEvent(ignoreInit = T,input$distance,{
    vals$cur_xyf_distance<-input$distance
  })
  observeEvent(ignoreInit = T,input$topology,{
    vals$cur_xyf_topology<-input$topology
  })

  observeEvent(ignoreInit = T,input$len,{
    vals$cur_xyf_len<-input$len
  })

  output$som_dim<-renderUI({

    if(is.null(vals$cur_xyf_xd)&is.null(vals$cur_xyf_yd)){
      L<-topo.reactive()
      vals$cur_xyf_yd<-vals$cur_xyf_xd<- L
    }
    req(!is.null(vals$cur_xyf_yd))

    cur_xyf_xd<- paste0(seq(vals$cur_xyf_xd-1,vals$cur_xyf_xd), collapse = ", ")
    cur_xyf_yd<- paste0(seq(vals$cur_xyf_yd-1,vals$cur_xyf_yd), collapse = ", ")
    div(
      div(
        tipify(icon("fas fa-question-circle"),"Integer or a comma-delimited vector:  number of neurons in the x dimension", options = list(container="body")),
        "+ x dims:",
        inline(textInput(ns("xd"), NULL, value=cur_xyf_xd, width="110px"))
      ),
      div(
        tipify(icon("fas fa-question-circle"),"Integer or a comma-delimited vector:  number of neurons in the y dimension", options = list(container="body")),
        "+ y dims:",
        inline(textInput(ns("yd"), NULL, value=cur_xyf_yd, width="110px"))
      )

    )
  })

  observeEvent(ignoreInit = T,input$data_xyfX,{
    L<-topo.reactive()
    vals$cur_xyf_xd<-vals$cur_xyf_yd<-L

  })

  output$xyf_custom<-renderUI({
    req(input$xyf_search)
    req(input$xyf_search=="user-defined")
    div(

      uiOutput(ns('som_dim')),
      uiOutput(ns('som_wei')),
      uiOutput(ns('som_nei')),
      uiOutput(ns('som_toro')),


      pickerInput(ns("topology"),strong("Topology"),
                  multiple = T,
                  width="150px",
                  choices = c("hexagonal","rectangular"),
                  selected=vals$cur_xyf_topology)

    )
  })
  output$xyf_default<-renderUI({
    choices<-c("user-defined","grid","random")
    div(
      div(
        tipify(icon("fas fa-question-circle"),"Tuning search", options = list(container="body")),
        "+ search:",
        inline(
          pickerInput(ns("xyf_search"), NULL, choices = choices, width="110px", selected=vals$cur_xyf_search)
        )
      ),
      uiOutput(ns("xyf_search_length"))
    )
  })
  observeEvent(ignoreInit = T,input$xyf_search,{
    vals$cur_xyf_search<-input$xyf_search
  })
  output$xyf_search_length<-renderUI({
    req(input$xyf_search!='user-defined')
    if(is.null(vals$cur_xyf_tuneLength)){vals$cur_xyf_tuneLength<-5}

    div(
      tipify(icon("fas fa-question-circle"),"the maximum number of different combinations of hyperparameters to evaluate during the tuning process", options = list(container="body")),
      "+ tuneLength:",
      inline(
        numericInput(ns("xyf_tuneLength"), NULL, value = vals$cur_xyf_tuneLength, width="82px")
      )
    )
  })
  observeEvent(ignoreInit = T,input$xyf_tuneLength,{
    vals$cur_xyf_tuneLength<-input$xyf_tuneLength
  })
  output$som_sideleft<-renderUI({

    req(input$som_type)
    if(is.null(vals$cur_xyf_len)){vals$cur_xyf_len<- 500}

    if(is.null(vals$cur_xyf_distanceY)){vals$cur_xyf_distanceY<- paste0("tanimoto")}
    if(is.null(vals$cur_xyf_distance)){vals$cur_xyf_distance<- paste0("euclidean")}
    if(is.null(vals$cur_xyf_topology)){vals$cur_xyf_topology<- paste0("hexagonal")}
    if(is.null(  vals$cur_xyf_maxna)){  vals$cur_xyf_maxna<-0.001}




    div(div(strong("Tunning parameters")),
        column(12,class="well2",
               div(
                 class="map_control_style",style="color: #05668D; margin-top: 20px",
                 uiOutput(ns("xyf_default")),
                 uiOutput(ns("xyf_custom")),
                 uiOutput(ns("xyf_distanceX")),
                 uiOutput(ns("xyf_distanceY")),
                 uiOutput(ns('out_maxNA.fraction')),
                 uiOutput(ns("xyf_len")),
                 uiOutput(ns("grid_xy"))
               )))
  })

  output$xyf_len<-renderUI({
    div(
      tipify(icon("fas fa-question-circle"),"Integer or a comma-delimited vector:  the times the complete data set will be presented to the network", options = list(container="body")),
      "+ Training length:",
      inline(textInput(ns("len"), NULL, value=vals$cur_xyf_len, width="110px"))
    )
  })

  output$xyf_distanceX<-renderUI({
    choicesX<-c("euclidean","BrayCurtis","sumofsquares","manhattan",
                "tanimoto")
    div(
      pickerInput(ns("distance"),strong("Distance X",tipify(icon("fas fa-question-circle"),"Distance measure between each neuron and input data")),

                  width="150px",
                  choices = choicesX,
                  selected=vals$cur_xyf_distance)
    )
  })
  output$xyf_distanceY<-renderUI({
    req(input$som_type)
    choicesY<-choicesX<-c("euclidean","BrayCurtis","sumofsquares","manhattan",
                          "tanimoto")
    if(input$som_type=="Regression"){
      choicesY<-c("euclidean","BrayCurtis","sumofsquares","manhattan")
    }

    div(
      pickerInput(ns("distanceY"),strong("Distance Y",tipify(icon("fas fa-question-circle"),"Distance measure between each neuron and input data")),

                  width="150px",
                  choices = choicesY,
                  selected=vals$cur_xyf_distanceY)
    )
  })
  output$out_maxNA.fraction<-renderUI({
    div(
      tipify(icon("fas fa-question-circle"),"the maximal fraction of values that may be NA to prevent the row to be removed. Not applicable for BrayCurtis.", options = list(container="body")),
      "+ maxNA.fraction",
      inline(numericInput(ns("maxNA.fraction"), NULL, value=vals$cur_xyf_maxna, width="110px"))
    )
  })



  observeEvent(ignoreInit = T,input$nei,{
    vals$cur_xyf_nei<-input$nei
  })
  observeEvent(ignoreInit = T,input$toro,{
    vals$cur_xyf_toro<-input$toro
  })
  output$som_nei<-renderUI({
    if(is.null(vals$cur_xyf_nei)){vals$cur_xyf_nei<-"gaussian"}
    pickerInput(ns("nei"),strong("neighbourhood.fct"),
                multiple = T,
                width="150px",
                choices = c("gaussian","bubble"),
                selected=vals$cur_xyf_nei)
  })

  output$som_toro<-renderUI({
    if(is.null(vals$cur_xyf_toro)){vals$cur_xyf_toro<-"FALSE"}
    pickerInput(ns("toro"),strong("toroidal"),
                multiple = T,
                width="150px",
                choices = c("FALSE","TRUE"),
                selected=vals$cur_xyf_toro)
  })

  output$som_wei<-renderUI({
    if(is.null(vals$cur_lay_wei)){
      vals$cur_lay_wei<-c("0.5, 0.8")
    }
    div(
      tipify(icon("fas fa-question-circle"),"Integer or a comma-delimited vector:  relative weight of the outcome. For example, 0.75 indicates that the outcome layer has 3 times the weight as the predictor layer.", options = list(container="body")),
      "+ user-weights:",
      inline(textInput(ns("wei"), NULL, value=vals$cur_lay_wei, width="110px"))
    )
  })


  output$som_sideright<- renderUI({

    div(
      div(strong("Resampling parameters")),
      column(12,class="well2",
             div(class="map_control_style",style="color: #05668D; margin-top: 20px",

                 div(
                   tipify(icon("fas fa-question-circle"),textseed(), options = list(container="body")),
                   "+ seed:",
                   inline(
                     numericInput(ns("seedsom"), NULL, value = NULL, width="122px")
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
                 pickerInput(ns("som_res_method"),NULL, choices=list("repeatedcv","boot","LOOCV","LGOCV"), width = "100px")
                 ),
                 uiOutput(ns("som_resampling"))
             ),

             column(12,style='white-space: normal;',
                    uiOutput(ns("som_war"))
             ),

             column(12, align = "center",
                    popify(actionButton(ns("trainsom"), h4(icon("fas fa-braille"),"train supervised SOM",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")
             ),
             column(12,align="right",
                    div(style="white-space: normal;max-width: 150px",
                        actionLink(ns("gotrain_loop"),"+ Train all variables in Datalist Y")
                    )
             )



      )
    )
  })
  #tunesom<-list(dim=c(7,7))

  output$grid_xy<-renderUI({
    req(input$xyf_search=="user-defined")


    div(style="overflow-x: scroll; width: 700px; padding-top: 10px",
        div(em("Tunning preview")),
        inline(renderUI({
          uiOutput(ns('df_grid'))
        }))
    )

  })



  getsom_grid<-reactive({
    req(is.character(input$xd))
    req(is.character(input$yd))
    req(is.character(input$wei))
    req(input$nei)
    req(length(input$toro)>0)
    xs=as.numeric(unlist(strsplit(input$xd,",")))
    ys=as.numeric(unlist(strsplit(input$yd,",")))
    wei=as.numeric(unlist(strsplit(input$wei,",")))
    rlen=as.numeric(unlist(strsplit(input$len,",")))

    topology=input$topology
    nei=input$nei
    toro=as.logical(input$toro)
    res<-data.frame(expand.grid(xs, ys,wei,topology,nei,toro,rlen))
    colnames(res)<-c("xdim","ydim","user.weights","topo","neighbourhood.fct","toroidal","rlen")
    dims<-apply(res[,1:2],1,function(x) x[1]*x[2])

    reK<-if(input$som_test_partition!="None"){
      parts<-get_parts_som()
      train<-parts$train
      length(train)} else{nrow(vals$saved_data[[input$data_xyfX]])}

    reK<-reK*(1-(1/input$cvsom))
    res[which(dims<reK),]



  })


  output$df_grid<-renderUI({
    renderPrint({

      res<-getsom_grid()
      validate(need(nrow(res)>0,"The dimensions X*Y cannot exceed the number of observations considered in the training"))
      colnames(res)[3]<-"weight"
      data.frame(res)

    })
  })


  observeEvent(ignoreInit = T,input$wei,{
    vals$cur_lay_wei<-input$wei
  })

  savereac<-reactive({
    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img
    saveRDS(tosave,"vals.rds")
    saveRDS(reactiveValuesToList(input),"input_pd.rds")
    beep()


  })



  topo.reactive <- reactive({

    df=data.frame(na.omit(getdata_xyfX()))
    N<-nrow(getdata_xyfX())
    SIZE<-sqrt(5*sqrt(N))
    L<- floor(SIZE)
    L



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

    fluidRow(column(12,
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












  observeEvent(ignoreInit = T,input$gotrain_loop,{
    showModal(
      modalDialog(
        title=div("Train models using variables in",input$data_somY),
        easyClose=T,

        div(
          column(12,
                 "This action will train",strong(ncol(getyloop()), style="color: red")," models, using the columns of Datalist: ",strong(input$data_somY),"as Y. The given name of the models will be (",input$data_somY,"::Y~Datalist_X), and any model already saved with this name(s) will be replaced.",em("We suggest using this tool from a Datalist without any saved som model", style="color: red")),
          column(12,
                 div(strong("Click to proceed:")),
                 actionButton(ns("train_loop"),"Train_loop_YDatalist"))
        )
      )
    )

  })

  observeEvent(ignoreInit = T,input$train_loop,{
    removeModal()
    trainsom_loop()
  })

  getyloop<-reactive({

    y<-vals$saved_data[[input$data_somY]]
    if(input$som_type== 'Classification'){y<-attr(y,"factors")}
    if(input$som_test_partition!="None"){
      if(input$som_type== 'Classification'){
        pic<-which(colnames(y)==input$som_test_partition)
        if(length(pic)>0){
          y<-y[,-pic, drop=F]}
      }
    }
    y_loop<-y
    y_loop
  })

  trainsom_loop<-reactive({

    y_loop<-getyloop()

    grid<-getsom_grid()
    distance=c(input$distance,input$distanceY)
    len=input$len
    search<-"grid"
    tuneLength=NULL
    if(input$xyf_search=="user-defined"){
      grid<-getsom_grid()
      search="random"
    } else{
      grid=NULL
      search<-input$xyf_search
      tuneLength =input$xyf_tuneLength
    }

    withProgress(message="Running",max=ncol(y_loop),{
      i=1
      for(i in 1:ncol(y_loop)) {
        t<-try({
          data<-getdata_xyfX()
          x<-x_o<-data.frame(data)
          y<-y_o<-y_loop[,i]

          output$som_war<-renderUI({
            column(12,style="color: red",align="center",
                   if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
                   if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
            )
          })
          validate(need(anyNA(x)==F,"NAs not allowed in X"))
          validate(need(anyNA(y)==F,"NAs not allowed in Y"))
          req(input$som_test_partition)
          if(input$som_test_partition!="None"){
            parts<-get_parts_som()
            train<-parts$train
            test<-parts$test
            x<-data.frame(getdata_xyfX()[train,])
            y<-y_o[train]
          }
          seed<-if (!is.na(input$seedsom)) { input$seedsom} else{
            NULL
          }



          if (!is.na(input$seedsom)) {set.seed(input$seedsom)}
          som<-train(x,y,imesc_xyf,
                     trControl=trainControl(
                       method = input$som_res_method,## resampling method
                       number = input$cvsom,
                       repeats = input$repeatssom,
                       p=input$pleavesom/100,
                       savePredictions = "all",
                       search=search

                     ),
                     tuneLength=tuneLength,
                     tuneGrid =grid,
                     dist.fcts=distance,
                     maxNA.fraction=input$maxNA.fraction
          )


          attr(som,"test_partition")<-paste("Test data:",input$som_test_partition,"::",input$testdata_som)
          attr(som,"Y")<-paste(input$data_somY,"::",colnames(y_loop)[i])
          attr(som,"Datalist")<-paste(input$data_xyfX)



          if(input$som_test_partition!="None"){
            attr(som,'test')<-x_o[test,]
            attr(som,"sup_test")<-y_o[test]
          } else{ attr(som,'test')<-c("None")}
          vals$bag_som<-T
          attr(som,"supervisor")<-colnames(y_loop)[i]
          attr(som,"inputs")<-list(
            Ydatalist=input$data_somY,
            Y=colnames(y_loop)[i],
            Xdatalist=input$data_xyfX
          )

          attr(som,"Y")<-paste0(input$data_somY,"::",colnames(y_loop)[i])
          vals$som_unsaved<-som
          savesom_loop()
          beep(10)

        })



        if("try-error" %in% class(t)){
          output$som_war<-renderUI({
            column(12,em(style="color: gray",
                         "Error in training the som model. Check if the number of observations in X and Y are compatible"
            ))
          })

        } else{
          output$som_war<-NULL
        }
        incProgress(1)
      }
      updateTabsetPanel(session,"som_tab","som2_results")
    })

  })





  savesom_loop<-reactive({
    temp<-vals$som_unsaved
    name_model<-paste0(attr(temp,"Y"),"~",input$data_xyfX)
    temp<-list(temp)
    names(temp)<-name_model
    attr(vals$saved_data[[input$data_xyfX]],"xyf")[[name_model]]<-temp
    cur<-name_model
    attr(vals$saved_data[[input$data_xyfX]],"xyf")['new som (unsaved)']<-NULL
    vals$bag_som<-F
    vals$cur_xyf_models<-cur
    vals$cur_som<-cur
    vals$som_unsaved<-NULL
  })

  savesom<-reactive({
    req(vals$cur_xyf_models=='new som (unsaved)')
    temp<-vals$xyf_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[input$data_xyfX]],"xyf")[[input$newdatalist]]<-temp
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[input$data_xyfX]],"xyf")[[input$over_datalist]]<-temp
      cur<-input$over_datalist
    }
    attr(vals$saved_data[[input$data_xyfX]],"xyf")['new som (unsaved)']<-NULL
    vals$bag_som<-F
    vals$cur_xyf_models<-cur
    vals$cur_som<-cur

  })


  output$som_resampling<-renderUI({
    div(
      inline(
        if(input$som_res_method=='cv'|input$som_res_method=='repeatedcv')
        {
          div(

            tipify(icon("fas fa-question-circle"),"number of folds", options=list(container="body")),"+ cv:",
            numericInput(ns("cvsom"), NULL, value = 5,width="140px")

          )
        }
      ),
      div(
        inline(
          if(input$som_res_method=='repeatedcv')
          {
            inline(
              div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute", options=list(container="body")),
                  "+ repeats:",
                  numericInput(ns("repeatssom"),NULL, value = 1,width="109px")
              )
            )

          }
        )
      ),
      inline(
        if(input$som_res_method=='boot'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the number of resampling iterations", options=list(container="body")),
              "+ number:",
              numericInput(ns("cvsom"), NULL, value = 5,width="100px")

            )
          )
        }
      ),
      inline(
        if(input$som_res_method=='LGOCV'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the training percentage", options=list(container="body")),
              "+ percentage:",
              numericInput(ns("pleavesom"), NULL, value = 10,width="100px")
            )
          )
        }
      )
    )
  })


  output$xyf_results_menu<-renderUI({
    if(is.null(vals$cur_xyf_models)){vals$cur_xyf_models<-1}
    div(pickerInput(ns("xyf_models"),strong("som results:", tiphelp("Stochastic Gradient Boosting Results. Click to select som results saved in the Training Datalist (X).")), choices= names(attr(vals$saved_data[[input$data_xyfX]],"xyf")), width="200px", selected = vals$cur_xyf_models)
    )
  })
  output$rmsom_button<-renderUI({
    req(input$xyf_models!="new som (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmsom"), icon("far fa-trash-alt")), "Remove the som model from the training Datalist (X)")
    )

  })
  output$som_tab_2_1<-renderUI({
    m<-vals$xyf_results
    div(
      splitLayout(
        renderPrint(m),
        renderPrint(m$finalModel$param)
      ),
      div(style="height: 100px",
          popify(downloadButton(ns("down_xyf_results"),label=span("Download som model",icon("fas fa-braille")),style = "button_active"),NULL,
                 HTML(paste0(
                   div(HTML(paste0("Download model as ",em('rds')," file to be read in R as", code('base::readRDS(file.rds)'))))
                 )))
      )

    )
  })
  output$down_xyf_results <- {
    downloadHandler(
      filename = function() {
        paste0("som","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$xyf_results,file)
      })
  }
  observeEvent(ignoreInit = T,input$som2_results,{
    vals$som2_results<-input$som2_results
  })

  output$results_som<-renderUI({
    validate(need(length(vals$xyf_results)>0,"No models have been trained yet"))

    if(is.null(vals$som2_results)){vals$som2_results<-'tab1'}

    req(length(vals$xyf_results))
    m<-vals$xyf_results

    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    column(12,
           div(
             p(
               strong("Model:"),em(getmodel_som())
             )
           ),
           tabsetPanel(id=ns("som2_results"),selected=vals$som2_results,
                       tabPanel("2.1. Summary",value="tab1",
                                uiOutput(ns("som_tab_2_1"))
                       ),
                       tabPanel("2.2. Performace",value="tab2",
                                uiOutput(ns("som_tab_2_2"))
                       ),
                       tabPanel("2.3. Permutation importance",value="tab3",
                                uiOutput(ns("permutation_importance"))

                       ),
                       tabPanel("2.4. Changes",value="tab4",
                                uiOutput(ns("plot_train"))
                       ),
                       tabPanel("2.5. Counts",value="tab5",
                                uiOutput(ns("plot_counts"))
                       ),
                       tabPanel("2.6. BMUs",value="tab6",
                                uiOutput(ns("bmus"))
                       ),
                       tabPanel("2.7. Confusion matrix",value="tab7",
                                uiOutput(ns("som_tab_2_4"))
                       )
           )

    )
  })
  ##

  ##
  output$permutation_importance<-renderUI({
    req(input$xyf_models)
    validate(need(input$xyf_models!="new xyf (unsaved)","
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
                                 ns('xyf_varImp'),span("+ Download table"),style="button_active"
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

  observeEvent(ignoreInit = T,input$down_xyf_tab3_1,{
    vals$hand_down<-"xyf - predictions"
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
            ns('xyf_varImp_plot'),span('+ Download plot'),style="button_active"
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
  observeEvent(ignoreInit = T,input$xyf_varImp_plot,{
    vals$hand_plot<-"xyf - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })



  output$feature_cm<-renderUI({
    m<-vals$xyf_results
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
    model_list<- attr(vals$saved_data[[input$data_xyfX]],"xyf")[[input$xyf_models]]
    req(length(model_list)>0)
    predtable<-model_list$feature_rands
    dft<-get_cm_impact(predtable, var=input$var_feature_cm)

    renderPlot({
      vals$feature_cm_plot<- plotCM(dft, palette=input$pal_feature_cm,vals$newcolhabs, font_color="black", title="", round_cm=3)
      vals$feature_cm_plot
    })
  })
  getmetric<-reactive({
    m<-vals$xyf_results
    metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
    metric
  })
  get_sumamry_permute_importance<-reactive({
    req(input$sig_feature)
    model_list<- attr(vals$saved_data[[input$data_xyfX]],"xyf")[[input$xyf_models]]
    sig_level<-input$sig_feature
    req(length(model_list)>0)
    df<-model_list$feature_rands
    m<-model_list[[1]]
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    df<-permimp_metric(df,metric=getmetric(), class=NULL,m, newdata,obc)

    req(length(df)>0)
    req(is.data.frame(df))

    vals$xyf_varImp<-perm_summary<-sig_feature(df,m, newdata, obc,sig_level)
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
    m<-vals$xyf_results
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    rep=input$feat_rep
    seed=input$feat_seed
    if(is.na(seed)){seed=NULL}
    predtable<- permute_importance_foreach_progress(m,newdata,obc, rep, seed=seed)
    stopCluster(cl)
    unregister_dopar()
    attr(vals$saved_data[[input$data_xyfX]],"xyf")[[input$xyf_models]]$feature_rands<-predtable
  })
  output$feature_importance_plot<-renderUI({
    req(input$data_xyfX)
    req(input$xyf_models)
    model_list<- attr(vals$saved_data[[input$data_xyfX]],"xyf")[[input$xyf_models]]
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

      vals$xyf_varImp_plot<-p
      vals$xyf_varImp_plot

    })
  })
  ##
  output$plot_train<-renderUI({
    m<-getsom()
    div(actionLink(ns("download_changes"),"+ Download"),
        renderPlot({
          plot(m,type="changes")
          p<-recordPlot()
          vals$xyf_changes<-p
          vals$xyf_changes
        })
    )
  })
  output$plot_counts<-renderUI({
    m<-getsom()
    div(actionLink(ns("download_counts"),"+ Download"),
        renderPlot({
          p<-plot(m,type="counts")
          vals$xyf_counts<-p
          vals$xyf_counts

        })
    )
  })
  observeEvent(ignoreInit = T,input$download_changes,{
    vals$hand_plot<-"som changes (xyf)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$download_counts,{
    vals$hand_plot<-"som counts (xyf)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })

  output$bmus<-renderUI({

    req(length(getsom())>0)
    sidebarLayout(
      sidebarPanel(uiOutput(ns("bmu_plot_side"))),
      mainPanel(uiOutput(ns("bmu_plot")))
    )
  })


  getsom<-reactive({
    req(input$xyf_models)

    m<-vals$xyf_results
    m<-m$finalModel
    names(m$unit.classif)<-rownames(m$data[[1]])
    req( class(m) == "kohonen")
    m
  })


  output$bmu_plot_side<-renderUI({
    # req(input$model_or_data=="som codebook")
    div(class="map_control_style",style="color: #05668D",
        div(span("+"),inline(checkboxInput(ns("plotY"),"Map Y", T))),
        uiOutput(ns("xyf_side_somplot")),
        uiOutput(ns("bmu_down_links")))
  })
  {
    output$xyf_side_somplot<-renderUI({



      div(class="map_control_style",style="color: #05668D",
          div(


            uiOutput(ns('xyf_umapro')),
            uiOutput(ns("xyf_hc_ordcluster")),
            uiOutput(ns('xyf_hc_palette')),


            div(
              style='border-bottom: 1px solid gray',
              uiOutput(ns('xyf_out_pclus_addpoints')),
              div(
                style="margin-left: 10px",
                uiOutput(ns("xyf_pclus_points_inputs"))
              )
            ),

            div(style='border-bottom: 1px solid gray',
                uiOutput(ns('xyf_out_pclus_addtext')),
                div(
                  style="margin-left: 10px",
                  uiOutput(ns("xyf_pclus_text_inputs"))
                )),
            div(
              uiOutput(ns("xyf_vfm_check"))
            ),
            uiOutput(ns("xyf_showerrors_som")),
            uiOutput(ns("xyf_theme")),
            uiOutput(ns("xyf_title"))


          ))
    })
    output$xyf_hc_palette<-renderUI({
      choices<-xyf_get_choices_pal()
      title<-attr(choices,"title")
      div(style='border-bottom: 1px solid gray;',
          div(class="palette",
              title,
              pickerInput(inputId = ns("xyf_bg_palette"),
                          label =NULL,
                          choices =  vals$colors_img$val[choices],
                          selected=vals$somplot_bg,
                          choicesOpt = list(
                            content =  vals$colors_img$img[choices] ),
                          options=list(container="body"), width="100px")),
          uiOutput(ns("xyf_pcodes_bgalpha"))

      )
    })
    output$xyf_out_pclus_addpoints<-renderUI({
      if(is.null(vals$xyf_addpoints)){
        vals$xyf_addpoints<-T
      }
      checkboxInput(ns("xyf_pclus_addpoints"),"+ Points",
                    value=vals$xyf_addpoints)
    })
    output$xyf_out_pclus_addtext<-renderUI({
      if(is.null(vals$xyf_text_inputs)){
        vals$xyf_text_inputs<-F
      }
      checkboxInput(ns("xyf_pclus_addtext"),"+ Labels",
                    value=vals$xyf_addtext)
    })
    output$xyf_title<-renderUI({
      div("+ Title: ",
          inline(textInput(ns("xyf_title"), NULL, "", width="200px"))
      )
    })
    output$xyf_theme<-renderUI({
      if(is.null(vals$theme)){
        vals$theme<-F
      }
      div(style='border-bottom: 1px solid gray',
          inline(checkboxInput(ns("xyf_theme"),
                               label = "+ show neuron coordinates",
                               value=vals$theme,
                               width="200px"
          ))
      )
    })
    output$xyf_umapro<-renderUI({


      div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',
          inline(uiOutput(ns('xyf_somback_value')))
      )
    })
    output$xyf_plus_umatrix <- renderUI({
      if(is.null(vals$plus_umatrix)){vals$plus_umatrix<-F}
      checkboxInput(ns("xyf_plus_umatrix"),strong("+ U-Matrix:"), value=vals$plus_umatrix)
    })
    observeEvent(ignoreInit = T,input$xyf_somback_value,{
      vals$xyf_somback_value<-input$xyf_somback_value
    })
    output$xyf_somback_value <- renderUI({



      div(
        style="font-size: 14px; margin-bottom: 5px",
        div(strong("+ Background value:"), style="margin-bottom: 5px; height: 24px"),
        radioButtons(ns("xyf_somback_value"),NULL,list("None"="None","U-Matrix"="uMatrix","Property"="property"), selected=vals$xyf_somback_value),
        uiOutput(ns("xyf_var_pproperty"))

      )
    })
    output$xyf_var_pproperty<-renderUI({
      req(input$xyf_somback_value=="property")

      div(style="width: 250px;margin-left: 25px; margin-bottom: 10px",class='small_picker',
          div(style="","Layer:",inline(uiOutput(ns('xyf_property_layer')))),
          div(style="width: 300px",
              "Variable:",
              inline(
                div(
                  inline(uiOutput(ns('xyf_prev_property'))),
                  inline(
                    uiOutput(ns('xyf_out_variable_pproperty'))),
                  inline(uiOutput(ns('xyf_next_property')))

                )
              ))


      )
    })
    observeEvent(ignoreInit = T,input$xyf_property_layer,{
      vals$xyf_property_layer<-input$xyf_property_layer
    })
    output$xyf_property_layer<-renderUI({
      choices = names(getsom()$data)
      req(length(choices)>0)
      pickerInput(ns("xyf_property_layer"),
                  label = NULL,
                  choices = choices,
                  selected=vals$xyf_property_layer
      )
    })
    xyf_getdata_layer<-reactive({
      choices0 = names(getsom()$data)
      if(length(choices0)>0){
        req(input$xyf_property_layer)
        getsom()$data[[input$xyf_property_layer]]
      } else{
        getsom()$data[[1]]
      }

    })
    output$xyf_out_variable_pproperty<-renderUI({
      choices<-colnames(xyf_getdata_layer())
      div(id="ssom_property",
          pickerInput(ns("xyf_variable_pproperty"),
                      label = NULL,
                      choices = choices,
                      selected=vals$variable_pproperty
          )
      )
    })
    output$xyf_prev_property<-renderUI({
      data = xyf_getdata_layer()
      req(which(colnames(data)==input$xyf_variable_pproperty)!=1)
      actionButton(ns("xyf_prev_property"),"<<")
    })
    output$xyf_next_property<-renderUI({
      data = xyf_getdata_layer()
      req(which(colnames(data)!=input$xyf_variable_pproperty)==ncol(data))
      actionButton(ns("xyf_next_property"),">>")
    })
    output$xyf_pclus_points_inputs<-renderUI({
      req(isTRUE(input$xyf_pclus_addpoints))
      div(
        div("+ Palette",inline(uiOutput(ns("xyf_pclus_points_palette")))),
        div("+ Factor",inline(uiOutput(ns("xyf_pclus_points_factor_out")))),
        div("+ Shape",inline(uiOutput(ns("xyf_pclus_points_shape")))),
        div("+ Size",inline(uiOutput(ns("xyf_pclus_points_size"))))
      )
    })
    output$xyf_pclus_points_palette<-renderUI({

      if(is.null(vals$xyf_points_palette)){vals$xyf_points_palette<-"black"}
      inline(
        tipify(pickerInput(inputId = ns("xyf_pclus_points_palette"),
                           label =NULL,
                           choices = vals$colors_img$val,
                           choicesOpt = list(content = vals$colors_img$img),
                           selected=vals$xyf_points_palette,
                           options=list(container="body"), width="75px"),
               "Symbol colors"
        )
      )
    })
    output$xyf_pclus_points_factor_out<-renderUI({
      req(input$xyf_pclus_points_palette)
      choices<-c(colnames(attr(vals$saved_data[[vals$cur_data]],"factors")))


      inline(
        pickerInput(ns("xyf_pclus_points_factor"),NULL,
                    choices = c(choices),selected=vals$xyf_points_factor,width="150px")
      )


    })
    output$xyf_pclus_points_shape<-renderUI({
      tipify(pickerInput(inputId = ns("xyf_pclus_symbol"),
                         label = NULL,
                         choices = df_symbol$val,
                         choicesOpt = list(content = df_symbol$img),
                         options=list(container="body"), width="100px",
                         selected=vals$xyf_symbol)
             ,"symbol shape")
    })
    output$xyf_pclus_points_size<-renderUI({
      if(is.null(vals$xyf_points_size)){vals$xyf_points_size<-1}
      inline(
        tipify(numericInput(ns("xyf_pclus_points_size"),NULL,value = vals$xyf_points_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
      )
    })
    output$xyf_pclus_text_inputs<-renderUI({
      req(isTRUE(input$xyf_pclus_addtext))
      div(
        div("+ Palette",inline(uiOutput(ns("xyf_pclus_text_palette")))),
        div("+ Factor",inline(uiOutput(ns("xyf_pclus_text_factor_out")))),
        div("+ Size",inline(uiOutput(ns("xyf_pclus_text_size"))))
      )
    })
    output$xyf_pclus_text_palette<-renderUI({

      if(is.null(vals$xyf_text_palette)){vals$xyf_text_palette<-"black"}
      inline(
        tipify(pickerInput(inputId = ns("xyf_pclus_text_palette"),
                           label =NULL,
                           choices =  vals$colors_img$val[getsolid_col()],
                           selected=vals$xyf_text_palette,
                           choicesOpt = list(
                             content =  vals$colors_img$img[getsolid_col()] ),
                           options=list(container="body"), width="75px"),
               "Symbol colors"
        )
      )
    })
    output$xyf_pclus_text_factor_out<-renderUI({
      req(input$xyf_pclus_text_palette)
      choices<-c(colnames(attr(vals$saved_data[[vals$cur_data]],"factors")))
      inline(
        pickerInput(ns("xyf_pclus_text_factor"),NULL,
                    choices = c(choices),selected=vals$xyf_text_factor,width="150px")
      )


    })
    output$xyf_pclus_text_size<-renderUI({
      if(is.null(vals$xyf_text_size)){vals$xyf_text_size<-1}
      inline(
        tipify(numericInput(ns("xyf_pclus_text_size"),NULL,value = vals$xyf_text_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
      )
    })
    output$xyf_vfm_check<-renderUI({
      if(is.null(vals$xyf_varfacmap_action)){vals$xyf_varfacmap_action<-T}
      div(style='border-bottom: 1px solid gray',
          span("+ ",
               inline(checkboxInput(ns("xyf_varfacmap_action"), span("Variable factor map",actionLink(ns("xyf_varfacmap"), tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value =vals$xyf_varfacmap_action, width="100px"))),
          div(style="margin-left: 10px",uiOutput(ns("xyf_varfac_out")))
      )
    })
    output$xyf_varfac_out<-renderUI({
      req(isTRUE(input$xyf_varfacmap_action))
      if(is.null(vals$xyf_border)){
        vals$xyf_border<-"white"
      }
      div(
        uiOutput(ns('xyf_vfm_type_out')),
        uiOutput(ns('xyf_npic_out')))})
    output$xyf_pcodes_bgalpha<-renderUI({
      if(is.null(vals$xyf_border)){vals$xyf_border<-"white"}
      if(is.null(vals$pcodes_bgalpha)){

        vals$pcodes_bgalpha<-0


      }
      if(is.null(vals$base_size)){
        vals$base_size<-12
      }
      div(span(
        "+ Background lightness",inline(
          tipify(numericInput(ns("xyf_pcodes_bgalpha"),NULL,value = vals$pcodes_bgalpha,min = 0,max = 1,step = .1, width="75px"),"symbol size")
        )
      ),
      div(span(span('+ Border:'),inline(
        div(class="palette", pickerInput(ns("xyf_pclus_border"),
                                         label =NULL,
                                         choices =  vals$colors_img$val[getsolid_col()] ,
                                         choicesOpt = list(
                                           content =  vals$colors_img$img[getsolid_col()] ),
                                         selected= vals$xyf_border, width = '75px'))
      ))),

      div(
        "+ Base size",inline(
          tipify(numericInput(ns("xyf_base_size"),NULL,value = vals$base_size, width="75px"),"symbol size")
        )
      )
      )
    })
    output$xyf_vfm_type_out<-renderUI({
      my_choices<-list("Highest"='var', "Clockwise"="cor")

      div(span("+ Show correlation:",inline(
        pickerInput(ns("xyf_vfm_type"),NULL,
                    choices =my_choices,
                    selected=vals$vfm_type,
                    width="150px"
        ))))
    })
    output$xyf_npic_out<-renderUI({
      if(is.null(vals$npic)){vals$npic<-10}
      if(is.null(vals$xyf.cex.var)){vals$xyf.cex.var=1}
      if(is.null(vals$p.clus.col.text)){vals$p.clus.col.text<-"black"}
      if(is.null(vals$var_bg)){vals$var_bg<-"white"}
      if(is.null(vals$var_bg_transp)){vals$var_bg_transp=0}
      div(
        div(
          span("+ Number",
               inline(
                 tipify(
                   numericInput(ns("xyf_npic"), NULL, value = vals$npic, min = 2, width="75px"),"Number of variables to display"
                 )))
        ),
        div(
          span("+ Var size",
               inline(
                 numericInput(ns("xyf_pclus.cex.var"), NULL, value = vals$xyf.cex.var, min = 2, width="75px")))
        ),
        div(
          span("+ Var text color",
               inline(
                 div(class="palette",
                     pickerInput(inputId = ns("xyf_p.clus.col.text"),
                                 label = NULL,
                                 choices = vals$colors_img$val[getsolid_col()],
                                 choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),
                                 selected=vals$p.clus.col.text,
                                 width="100px"))))
        ),
        div(
          span("+ Var background",
               inline(
                 div(class="palette",
                     pickerInput(inputId = ns("xyf_var_bg"),
                                 label = NULL,
                                 choices = vals$colors_img$val[getsolid_col()],
                                 choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),
                                 selected=vals$var_bg,
                                 width="100px"))))
        ),
        div(
          span("+ Var transparency",
               inline(
                 tipify(
                   numericInput(ns("xyf_var_bg_transp"), NULL, value = vals$var_bg_transp, min = 2, width="75px"),"Number of variables to display"
                 )))
        )
      )
    })
    xyf_indicate_hc<-reactive({
      npic<-NULL
      indicate<-NULL
      if(isTRUE(input$xyf_varfacmap_action)){

        npic<- input$xyf_npic
        indicate<- input$xyf_vfm_type
      }
      iind=list(indicate=indicate,npic=npic)
      iind
    })
    xyf_bp_som<-reactive({
      iind=xyf_indicate_hc()
      m<-getsom()
      bp<-getbp_som(m=m,indicate=iind$indicate,npic=iind$npic,hc=vals$cutsom)
      vals$xyf_bp_som<-bp
      bp
    })
    xyf_get_network<-reactive({
      req(input$xyf_somback_value)
      backtype=NULL
      property=NULL
      if(input$xyf_somback_value=="property"){
        req(input$xyf_variable_pproperty)
        backtype="property"
        property=input$xyf_variable_pproperty
      } else if(input$xyf_somback_value=="uMatrix"){
        backtype="uMatrix"
      }



      m<-getsom()
      hexs<-get_neurons(m,background_type=backtype,property=property, hc=NULL)
      vals$xyf_hc_network<-hexs
      hexs
    })
    xyf_get_copoints<-reactive({
      m<-getsom()
      copoints<-getcopoints(m)
      vals$copoints_hc<-copoints
      copoints
    })

    xyf_copoints_scaled<-reactive({

      xyf_get_network()

      xyf_get_copoints()

      points_tomap=rescale_copoints(hexs=vals$xyf_hc_network,copoints=vals$copoints_hc)

      data<-vals$saved_data[[input$data_xyfX]]

      factors<-attr(data,"factors")
      if(length(input$xyf_pclus_text_factor)>0){
        req(input$xyf_pclus_text_factor%in%colnames(factors))
        text_factor= factors[rownames(data),input$xyf_pclus_text_factor, drop=F]
        points_tomap$label<-text_factor[rownames(points_tomap),]
      }
      if(length(input$xyf_pclus_points_factor)>0){
        req(input$xyf_pclus_points_factor%in%colnames(factors))
        points_factor= factors[rownames(data),input$xyf_pclus_points_factor, drop=F]
        points_tomap$point<-points_factor[rownames(points_tomap),]
        attr(points_tomap,"namepoints")<-input$xyf_pclus_points_factor
      }
      vals$xyf_hc_mapsom<-points_tomap
      points_tomap
    })
    xyf_get_choices_pal<-reactive({


      req(length(input$xyf_somback_value)>0)
      title="+ Background palette"
      if(input$xyf_somback_value=="None"){
        vals$somplot_bg<-"gray"
        choices=getsolid_col()
      } else {

        vals$somplot_bg<-"viridis"
        choices=getgrad_col()

      }


      attr(choices,"title")<-title
      choices
    })
    observeEvent(ignoreInit = T,input$xyf_hc_ord_factor,{
      vals$hc_ord_factor<-input$xyf_hc_ord_factor
    })
    observeEvent(ignoreInit = T,input$xyf_hc_ord_datalist,{
      vals$hc_ord_datalist<-input$xyf_hc_ord_datalist
    })
    observeEvent(ignoreInit = T,input$xyf_hc_sort,{
      vals$hc_sort<-input$xyf_hc_sort
    })
    observeEvent(ignoreInit = T,input$xyf_theme,{
      vals$theme<-input$xyf_theme
    })
    observeEvent(ignoreInit = T,input$xyf_pclus_addtext,{
      vals$xyf_addtext<-input$xyf_pclus_addtext
    })
    observeEvent(ignoreInit = T,input$xyf_pclus_addpoints,{
      vals$xyf_addpoints<-input$xyf_pclus_addpoints
    })
    observeEvent(ignoreInit = T,input$xyf_pp_labels,{
      vals$pp_labels<-input$xyf_pp_labels
    })
    observeEvent(ignoreInit = T,input$xyf_next_property,{
      data = xyf_getdata_layer()
      pnext<-colnames(data)[which(colnames(data)==input$xyf_variable_pproperty)+1]
      updateSelectInput(session,'xyf_variable_pproperty',selected=pnext)

    })
    observeEvent(ignoreInit = T,input$xyf_prev_property,{
      data = xyf_getdata_layer()
      pprev<-colnames(data)[which(colnames(data)==input$xyf_variable_pproperty)-1]
      updateSelectInput(session,'xyf_variable_pproperty',selected=pprev)
    })
    observeEvent(ignoreInit = T,input$xyf_variable_pproperty,{
      vals$variable_pproperty<-input$xyf_variable_pproperty
    })
    observeEvent(ignoreInit = T,input$xyf_round_error,{
      vals$round_error<-input$xyf_round_error
    })
    observeEvent(ignoreInit = T,input$xyf_show_mapcode_errors,{
      vals$show_mapcode_errors<-input$xyf_show_mapcode_errors
    })
    observeEvent(ignoreInit = T,input$xyf_teste_comb,{
      savereac()
    })
    observeEvent(ignoreInit = T,input$xyf_bg_palette,{
      vals$somplot_bg<-input$xyf_bg_palette
    })
    observeEvent(ignoreInit = T,input$xyf_pclus_text_palette,{
      vals$xyf_text_palette<-input$xyf_pclus_text_palette
    })
    observeEvent(ignoreInit = T,input$xyf_pclus_text_factor,{
      vals$xyf_text_factor<-input$xyf_pclus_text_factor
    })
    observeEvent(ignoreInit = T,input$xyf_pclus_border,{
      vals$xyf_border<-input$xyf_pclus_border
    })
    observeEvent(ignoreInit = T,input$xyf_vfm_type,{
      vals$vfm_type<-input$xyf_vfm_type
    })
    observeEvent(ignoreInit = T,input$xyf_npic,{
      vals$npic<-input$xyf_npic
    })
    observeEvent(ignoreInit = T,input$xyf_pclus.cex.var,{
      vals$xyf.cex.var<-input$xyf_pclus.cex.var
    })
    observeEvent(ignoreInit = T,input$xyf_p.clus.col.text,{
      vals$p.clus.col.text<-input$xyf_p.clus.col.text
    })
    observeEvent(ignoreInit = T,input$xyf_var_bg,{
      vals$var_bg<-input$xyf_var_bg
    })
    observeEvent(ignoreInit = T,input$xyf_var_bg_transp,{
      vals$var_bg_transp.alpha<-input$xyf_var_bg_transp
    })
    observeEvent(ignoreInit = T,input$xyf_pclus_points_palette,{
      vals$xyf_points_palette<-input$xyf_pclus_points_palette
    })
    observeEvent(ignoreInit = T,input$xyf_insertx_pclus,{
      vals$insertx_pclus<-input$xyf_insertx_pclus
    })
    observeEvent(ignoreInit = T,input$xyf_inserty_pclus,{
      vals$inserty_pclus<-input$xyf_inserty_pclus
    })
    observeEvent(ignoreInit = T,input$xyf_ncol_pclus,{
      vals$ncol_pclus<-input$xyf_ncol_pclus
    })
    observeEvent(ignoreInit = T,input$xyf_bgleg_pclus,{
      vals$bgleg_pclus<-input$xyf_bgleg_pclus
    })
    observeEvent(ignoreInit = T,input$xyf_pclus_symbol,{
      vals$xyf_symbol<-input$xyf_pclus_symbol
    })
    observeEvent(ignoreInit = T,input$xyf_dot_label_clus,{
      vals$dot_label_clus<-input$xyf_dot_label_clus
    })
    observeEvent(ignoreInit = T,input$xyf_varfacmap_action,{
      vals$xyf_varfacmap_action<-input$xyf_varfacmap_action
    })
    observeEvent(ignoreInit = T,input$xyf_pclus_points_size,{
      vals$xyf_points_size<-input$xyf_pclus_points_size
    })
    observeEvent(ignoreInit = T,input$xyf_pcodes_bgalpha,{
      vals$pcodes_bgalpha<-input$xyf_pcodes_bgalpha
    })
    xyf_argsplot<-reactive({

      req(input$xyf_pcodes_bgalpha)
      if(isTRUE(input$xyf_pclus_addpoints)){
        req(input$xyf_pclus_points_factor)
        points_factor= NULL }
      if(isTRUE(input$xyf_pclus_addtext)){
        req(input$xyf_pclus_text_factor)
        text_factor= NULL }

      indicate=xyf_indicate_hc()



      m<-getsom()

      tryco<-try(xyf_copoints_scaled(), silent = T)

      req(class(tryco)!='try-error')

      #savereac()
      trybp<-try( xyf_bp_som(), silent = T)



      errors<-NULL




      copoints2<-vals$copoints2
      copoints3<-copoints2
      #opoints2$point<-args$points_factor
      #attach(vals$args)


      args<-list(m=m,
                 hexs=vals$xyf_hc_network,
                 points_tomap=vals$xyf_hc_mapsom,
                 bp=vals$xyf_bp_som,
                 points=input$xyf_pclus_addpoints,
                 points_size=input$xyf_pclus_points_size,
                 points_palette=input$xyf_pclus_points_palette,
                 pch=as.numeric(input$xyf_pclus_symbol),
                 text=input$xyf_pclus_addtext,
                 text_size=input$xyf_pclus_text_size,
                 text_palette=input$xyf_pclus_text_palette,
                 bg_palette=input$xyf_bg_palette,
                 newcolhabs=vals$newcolhabs,
                 bgalpha=input$xyf_pcodes_bgalpha,
                 border=input$xyf_pclus_border,
                 indicate=indicate$indicate,
                 cex.var=as.numeric(input$xyf_pclus.cex.var),
                 col.text=input$xyf_p.clus.col.text,
                 col.bg.var=input$xyf_var_bg,
                 col.bg.var.alpha=1-input$xyf_var_bg_transp,
                 show_error=errors,
                 base_size=input$xyf_base_size,
                 show_neucoords=input$xyf_theme,
                 newdata=input$xyf_newdata,
                 title=input$xyf_title,
                 hc=NULL
      )

      args

    })
  }

  output$bmu_plot<-renderUI({
    renderPlot({
      args<-xyf_argsplot()

      args$plotY<-input$plotY

      req(length(args)>0)
      bp<-args$bp
      bp$id=NULL
      vals$biplot_som<-bp
      req(length(args$hexs)>0)

      #saveRDS(args,'args.rds')
      #args<-readRDS("args.rds")
      vals$xyf_bmu<-do.call(bmu_plot,args)
      vals$xyf_bmu
    })

  })
  output$bmu_down_links<-renderUI({
    div(
      div(
        tipify(
          actionLink(
            ns('downp_bmu'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-image")))),"download BMU plot",options=list(container="body")
        )
      ),

      div(
        tipify(
          actionLink(
            ns('down_pcorr_results'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-table")))
          ),
          "download variable factor results",     options=list(container="body")
        )
      )
    )

  })
  observeEvent(ignoreInit = T,input$downp_bmu,{
    vals$hand_plot<-"som (xyf)"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_pcorr_results,{
    vals$hand_down<-"pcorr (sup)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  output$feat_type<-renderUI({
    req(length(vals$xyf_results))
    m<-vals$xyf_results
    req(m$modelType=="Classification")
    choices<-c("Mean","side-by-side", "stacked")
    div("+ Plot type:", inline(pickerInput(ns("feat_type"),NULL,choices=c(choices,m$levels),width="100px")))
  })





  output$som_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(class="map_control_style",style="color: #05668D",
                   div(
                     tipify(
                       actionLink(
                         ns('som_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                       ),
                       "Create a datalist with the som training errors", options=list(container="body")
                     )
                   ),
                   div(
                     tipify(
                       actionLink(
                         ns('som_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                       ),
                       "Download Table", options=list(container="body")
                     )
                   )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('som_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('som_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          tags$style(
            paste(paste0("#",ns('som_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('som_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          div(
            p(strong("Global:")),
            inline(DT::dataTableOutput(ns('som_tab_errors0_train')))
          ),

          div(
            p(strong("Observations:")),
            inline(DT::dataTableOutput(ns('som_tab_errors_train')))
          )



        )
      )
    )
  })
  output$som_tab_errors0_train<-DT::renderDataTable({
    m<-vals$xyf_results
    table<- m$results[rownames(m$bestTune),]
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })
  output$som_tab_errors_train<-DT::renderDataTable({
    m<-vals$xyf_results
    table<-accu_som_class(m)
    vals$som_down_errors_train<-table
    table
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })

  output$som_tab_2_4<-renderUI({

    # validate(need(is.factor( vals$xyf_results$finalModel$y), "Confusion matrices are only valid for classification (factor) models."))
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          class="map_control_style",style="color: #05668D",
          div(
            span(
              "+ Type: ",
              pickerInput(inputId = ns("som_cm_type"),
                          label = NULL,
                          choices = c("Resampling","Optimal Model"),

                          width="100px"
              )
            )
          ),
          div(
            span(
              "+ Palette",
              pickerInput(inputId = ns("sompalette"),
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
            popify(actionLink(ns("downp_cmsom"), span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot")
          ),
          div(
            popify(actionLink(ns("dowcenter_cmsom"), span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM plot")
          )

        )

      ),
      mainPanel(
        plotOutput(ns("confusion_som")),
        verbatimTextOutput(ns("confusion_som2"))
      )
    )


  })

  observeEvent(ignoreInit = T,input$dowcenter_cmsom,{
    vals$hand_down<-"som_cm"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  output$confusion_som <- renderPlot({
    validate(need(vals$xyf_results$modelType=="Classification","Confusion matrices are only valid for classification models."))
    m<-vals$xyf_results
    if(input$som_cm_type=="Resampling"){
      res<-plotCM(m, input$sompalette,  newcolhabs=vals$newcolhabs)
    } else{
      m<-attr(getdata_xyfX(),"xyf")[[as.character(input$xyf_models)]][[1]]
      cm<-table(predict(m$finalModel),vals$xyf_results$trainingData[,'.outcome'])
      res<-plotCM(cm,input$sompalette,vals$newcolhabs)
    }
    vals$cm_som<-res
    res
  })
  output$confusion_som2<-renderPrint({
    validate(need(vals$xyf_results$modelType=="Classification","Confusion matrices are only valid for classification models."))
    res<-if(input$som_cm_type=="Resampling"){
      confusionMatrix(vals$xyf_results$pred$pred,vals$xyf_results$pred$obs)
    } else{
      m<-attr(getdata_xyfX(),"xyf")[[as.character(input$xyf_models)]][[1]]
      confusionMatrix(predict(m$finalModel),vals$xyf_results$trainingData[,'.outcome'])
    }
    vals$som_cm<-as.data.frame.matrix(res$table)
    res
  })



  output$savesom_button<-renderUI({
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_savesom"), icon("fas fa-save")), "Save the som model in the training Datalist (X)")
    )
  })

  getnewdatasom<-reactive({
    datalist<-vals$saved_data

    datalist_comp<-lapply(
      vals$saved_data, function (x) {
        colnames(x)<-gsub(" ",".", colnames(x))
        x
      }

    )

    datalist_comp<-vals$saved_data




    m<-vals$xyf_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-Y])
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })
  get_supervisor_som <- reactive({
    m<-vals$xyf_results
    att<-if(m$modelType=="Classification")
    {
      data <- vals$saved_data[[input$data_somY]]
      labels <- attr(data,"factors")
      labels[input$som_sup]
    } else{
      data <- vals$saved_data[[input$data_somY]]
      data[input$som_sup]
    }

  })
  get_cm_pred<-reactive({
    obs<-get_som_prederrors()$obs
    pred<-get_som_prederrors()$pred
    conf<-table(obs, pred)
    conf
  })


  getobssom<-reactive({
    sup_test<-attr(vals$xyf_results,"supervisor")

    datalist<-vals$saved_data
    if(vals$xyf_results$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}

    m<-vals$xyf_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })

  get_model_caret<-reactive({
    attr(getdata_xyfX(),"xyf")[[as.character(input$xyf_models)]][[1]]
  })


  pred_som<-reactive({
    validate(need(!anyNA(test_som()),"NAs not allowed in the prediction Datalist"))
    m<-attr(getdata_xyfX(),"xyf")[[as.character(input$xyf_models)]][[1]]
    newdata = test_som()
    #pk<-predictY_kohonen(m$finalModel,as.matrix(newdata),whatmap = 1)
    som_pred <-pk<-predict(m,as.matrix(newdata))
    #pk<-kohonen:::predict.kohonen(m$finalModel,as.matrix(newdata),whatmap = 1)
    # som_pred <-    pk$predictions$Y
    res<-data.frame(Predictions= som_pred)
    rownames(res)<-rownames(test_som())
    #colnames(res)<-attr(vals$xyf_results,"supervisor")

    # attr(res,"obs")<-test_som()
    res



  })
  get_parts_som<-reactive({
    req(input$data_somY)
    req(input$testdata_som)
    req(input$som_test_partition)
    data<-getdata_xyfX()
    factors<-attr(vals$saved_data[[input$data_somY]],"factors")[rownames(data),]
    if(input$som_test_partition!='None'){
      lis<-split(rownames(factors),factors[,input$som_test_partition])
      names(lis[[1]])<-lis[[1]]
      names(lis[[2]])<-lis[[2]]
      test_i<-which(names(lis)==input$testdata_som)
      tra_i<-which(names(lis)!=input$testdata_som)
      parts=list(train=lis[[tra_i]],test=lis[[test_i]])} else{
        parts=list(train=NULL,test=NULL)
      }

    parts

  })
  getdata_xyfX<-reactive({
    req(input$data_xyfX)
    data=vals$saved_data[[input$data_xyfX]]
    data
  })
  getdata_somY<-reactive({
    req(input$data_somY)
    req(input$som_sup)
    data <- vals$saved_data[[input$data_somY]][rownames(getdata_xyfX()),]
    if(input$som_type=="Classification"){
      data <- attr(data,"factors")[rownames(getdata_xyfX()),]
      y<-data[rownames(vals$saved_data[[input$data_xyfX]]),input$som_sup]
    } else{
      y<-data[rownames(vals$saved_data[[input$data_xyfX]]),input$som_sup]

    }
    names(y)<-rownames(data)
    y
  })
  getmodel_som<-reactive({
    m<-vals$xyf_results
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


  observeEvent(ignoreInit = T,input$predsom_new,{
    vals$predsom_new<-input$predsom_new
  })
  observeEvent(ignoreInit = T,input$sompred_which,{
    vals$sompred_which<-input$sompred_which
  })
  observeEvent(ignoreInit = T,input$som_down_errors_train,{
    vals$hand_down<-"som - training errors (observations)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$som_varImp,{
    vals$hand_down<-"som - variable importance"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_som_tab3_1,{
    vals$hand_down<-"som - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$data_somY,{
    vals$cur_dataY<-input$data_somY
  })
  observeEvent(ignoreInit = T,input$som_type,{
    vals$cur_model_type<-input$som_type
  })

  observeEvent(ignoreInit = T,input$som_method,{
    vals$cur_som_method<-input$som_method
  })
  observeEvent(ignoreInit = T,input$som_sup,{
    vals$cur_response<-input$som_sup
  })
  observeEvent(ignoreInit = T,input$data_xyfX,{
    vals$cur_data<-input$data_xyfX
  })
  observeEvent(ignoreInit = T,input$som_test_partition,{
    vals$cur_test_partition<-input$som_test_partition
  })
  observeEvent(ignoreInit = T,input$testdata_som,{
    vals$cur_testdata<-input$testdata_som
  })
  observeEvent(ignoreInit = T,input$xyf_models,{
    vals$cur_xyf_models<-input$xyf_models
  })
  observeEvent(ignoreInit = T,input$trainsom,{
    try({


      x<-x_o<-data.frame(getdata_xyfX())
      y<-y_o<-getdata_somY()

      output$som_war<-renderUI({
        column(12,style="color: red",align="center",
               if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
               if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
        )
      })
      validate(need(anyNA(x)==F,"NAs not allowed in X"))
      validate(need(anyNA(y)==F,"NAs not allowed in Y"))
      req(input$som_test_partition)
      if(input$som_test_partition!="None"){
        parts<-get_parts_som()
        train<-parts$train
        test<-parts$test
        x<-data.frame(getdata_xyfX()[train,])
        y<-getdata_somY()[train]
      }



      if (!is.na(input$seedsom)) {set.seed(input$seedsom)}

      search<-"grid"
      tuneLength=NULL
      if(input$xyf_search=="user-defined"){
        grid<-getsom_grid()
        search="random"
      } else{
        grid=NULL
        search<-input$xyf_search
        tuneLength =input$xyf_tuneLength
      }



      distance=c(input$distance,input$distanceY)
      len=input$len

      #saveRDS(args,"args_t.rds")

      withProgress(message = "Running supervised SOM ...",
                   min = 1,
                   max = 1,
                   {
                     som<-train(x,y,imesc_xyf,
                                trControl=trainControl(
                                  method = input$som_res_method,## resampling method
                                  number = input$cvsom,
                                  repeats = input$repeatssom,
                                  p=input$pleavesom/100,
                                  savePredictions = "all",
                                  search = search

                                ),
                                tuneGrid =grid,
                                dist.fcts=distance,
                                tuneLength=tuneLength,

                                maxNA.fraction=input$maxNA.fraction





                     )

                     attr(som,"test_partition")<-paste("Test data:",input$som_test_partition,"::",input$testdata_som)
                     attr(som,"Y")<-paste(input$data_somY,"::",input$som_sup)
                     attr(som,"Datalist")<-paste(input$data_xyfX)



                     if(input$som_test_partition!="None"){
                       attr(som,'test')<-x_o[test,]
                       attr(som,"sup_test")<-y_o[test]
                     } else{ attr(som,'test')<-c("None")}
                     vals$bag_som<-T
                     attr(som,"supervisor")<-input$som_sup
                     attr(som,"inputs")<-list(
                       Ydatalist=input$data_somY,
                       Y=input$som_sup,
                       Xdatalist=input$data_xyfX
                     )
                     vals$som_unsaved<-som


                     updateTabsetPanel(session,"som_tab","som2_results")
                   })
      beep(10)
      attr(vals$saved_data[[input$data_xyfX]],"xyf")[['new som (unsaved)']]<-vals$som_unsaved
      vals$cur_xyf_models<-"new som (unsaved)"
      vals$bag_som<-T


    })
  })
  observeEvent(ignoreInit = T,input$downp_cmsom,{
    vals$hand_plot<-"som - Confusion Matrix"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$downp_cmsom_pred,{
    vals$hand_plot<-"som - Confusion Matrix (predictions)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$som_varImp_plot,{
    vals$hand_plot<-"som - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_som_dens,{
    vals$hand_plot<-"som - DM plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observe({
    req(input$xyf_models)
    if(input$xyf_models=="new som (unsaved)"){
      vals$xyf_results<-vals$som_unsaved } else{
        if(length(input$data_xyfX)>0){
          vals$xyf_results<-attr(vals$saved_data[[input$data_xyfX]],"xyf")[[input$xyf_models]][[1]]
        }

      }
  })
  observeEvent(ignoreInit = T,input$xyf_models,{
    vals$cur_som<-input$xyf_models
  })
  observeEvent(ignoreInit = T,input$tools_rmsom,{
    attr(vals$saved_data[[input$data_xyfX]],"xyf")[input$xyf_models]<-NULL
  })
  observeEvent(ignoreInit = T,input$tools_savesom,{
    if(input$tools_savesom %% 2){
      vals$hand_save<-"Save som model in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_xyfX, style="color:gray"),strong("::"), em("som-Attribute", style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL

      showModal(module_som())

    }
  })
  observeEvent(ignoreInit = T,input$som_create_errors_train,{
    vals$hand_save<-"Create Datalist: som training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_som())
  })
  observeEvent(ignoreInit = T,input$som_create_predictions,{
    vals$hand_save<- "Create Datalist: som predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute, and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL

    showModal(module_som())
  })




  resample_create<-reactive({

    m<-vals$xyf_results
    var<-attr(m,"supervisor")
    temp<-vals$som_treetest
    datao<-vals$saved_data[[input$data_xyfX]]
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
    var<-attr(vals$xyf_results,"supervisor")
    name0<-paste("som",var,"qclass", sep="_")
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
      "Save som model in"= { name_save_som()},
      "Create Datalist: som training errors -obs"={name_som_train_errors()},
      "Create Datalist: som predictions"={ name_som_pred()},
      "resample_create"={name_resample_create()}
    )})

  som_create_training_errors<-reactive({
    temp<-vals$som_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$data_xyfX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  som_create_pred<-reactive({
    temp<-data.frame(vals$somtab_pred)
    temp[,1]<-as.numeric( temp[,1])
    attr(temp,"factors")<-temp

    datao<-if(vals$sompred_which=="Datalist"){
      vals$saved_data[[vals$predsom_new]]
    } else{ vals$saved_data[[input$data_xyfX]]}
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  name_save_som<-reactive({
    bag<-1
    name0<-paste0("xyf")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[input$data_xyfX]],"xyf"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[input$data_xyfX]],"xyf"))) break
      }}
    paste(name0,bag)
  })
  name_som_train_errors<-reactive({
    bag<-1
    name0<-paste0("som training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_som_pred<-reactive({
    bag<-1
    name0<-paste0("som predictions")
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
    if(vals$hand_save=='Save som model in'){choices<-names(attr(vals$saved_data[[input$data_xyfX]],"som"))}
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
      "Save som model in"= {savesom()},
      "Create Datalist: som training errors -obs"=som_create_training_errors(),
      "Create Datalist: som predictions"={som_create_pred()},
      'resample_create'={resample_create()}

    )
    removeModal()

  })
  module_som <- function() {
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
