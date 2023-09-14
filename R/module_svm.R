
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
#' @export
svmGrid <- function(x, y, len = NULL, search = "grid") {

  ## This produces low, middle and high values for sigma
  ## (i.e. a vector with 3 elements).
  sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
  ## To use grid search:
  if(search == "grid") {
    out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                       C = 2 ^((1:len) - 3))
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    rng <- extendrange(log(sigmas), f = .75)
    out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                      C = 2^runif(len, min = -5, max = 8))
  }
  out
}
#' @export
plot_dens_svm<-function(m,var="Chl",newcolhabs, palette="viridis", lwd=1, xlab="Variables"){
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1), widths = c("70","30"))
  colors<-getcolhabs(newcolhabs,palette,length(m$levels))
  tables<-m$finalModel$tables[[var]]
  if(lapply(m$finalModel$tables, class)[[1]]=="table"){
    par(mar=c(5,5,5,3), las=1)
    colors<-getcolhabs(newcolhabs,palette,length(levels(m$trainingData[[var]])))
    spineplot(tables, col=colors, las=1,ylab="", xlab=paste("Class:",attr(m,"supervisor")),off=1, axes =T,yaxlabels=F)
    par(mar=c(0,0,5,0), xpd=T)
    plot.new()
    legend(0,1, legend=levels(m$trainingData[[var]]), pch=15, col=colors, bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)

  } else{

    par(mar=c(5,5,5,0))
    res<-lapply(tables, function(x)
      c(
        minx=min(x$x),
        maxx=max(x$x),
        miny=min(x$y),
        maxy=max(x$y)
      ))
    res<-do.call(rbind,res)
    minx<-min(res[,1])
    maxx<-max(res[,2])
    miny<-min(res[,3])
    maxy<-max(res[,4])
    plot(m$finalModel$tables[[var]][[1]], xlim=c(minx,maxx), ylim=c(miny,maxy), type="n", main=var)
    i=1
    for(i in 1:length(tables)){
      lines(tables[[i]], col=colors[i], lwd=lwd)
    }

    par(mar=c(0,0,5,0), xpd=T)
    plot.new()
    legend(0,1, legend=m$levels, lty=1, col=colors, bty="n")
    on.exit(par(opar),add=TRUE,after=FALSE)
  }


}

#' @export
module_ui_svm <- function(id){

  ns <- NS(id)
  uiOutput(ns("svm_panels"))

}

#' @export
module_server_svm <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns
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
    tosave$ssom_tab<-vals$ssom_tab
    tosave$ssom_tab0<-vals$ssom_tab0
    tosave$som_results<-vals$som_results
    saveRDS(tosave,"savepoint.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()
    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')

  })
  output$svm_panels<-renderUI({
    # validate(need(length(vals$saved_data)>0,"No Datalist found")),

    div(
      #actionButton(ns("teste_comb"),"SAVE"),

      column(12,style="background: white",
             uiOutput(ns("svm_inputs")),
             uiOutput(ns("war_svm")),
             column(12,
                    tabsetPanel(id = ns("svm_tab"),
                                tabPanel(strong("1. Training"),value='svm_tab1',
                                         uiOutput(ns("svm_params"))),
                                tabPanel(strong("2. Results"),value='svm_tab2',
                                         uiOutput(ns("results_svm"))),
                                tabPanel(strong("3. Predict"),value='svm_tab3',
                                         uiOutput(ns("predict_svm")))
                    )
             )

      )


    )})
  insert_svm_resutls<-reactiveValues(df=F)




  output$predict_svm<-renderUI({
    column(12,style="background: white", uiOutput(ns("svm_pred_type")),
           tabsetPanel(id="predsvm_tab",
                       tabPanel(
                         strong("3.1. Results"), value="predsvm_tab01",
                         fluidRow(style="background: white",
                                  uiOutput(ns("svm_tab3_1")))),
                       tabPanel(
                         strong("3.2. Performace"),
                         fluidRow(style="background: white",
                                  uiOutput(ns("svm_tab3_2")))
                       )

           )
    )
  })


  ##
  output$svm_tab3_2<-renderUI({
    sup_test<-attr(vals$svm_results,"supervisor")
    supname<-paste0("Observed Y [",sup_test,"]")
    lab1<-span("+",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
               span(supname))

    sidebarLayout(
      sidebarPanel(
        div(class="map_control_style2",style="color: #05668D",
            pickerInput(ns("predsvm_newY"),
                        lab1,names(vals$saved_data[getobssvm()])),
            uiOutput(ns("svm_pred_pal")),
            numericInput(ns("round_pred_svm"),"+ Round",3,step=1),
            actionLink(ns("down_metrics"),span("+ Donwload Metrics",icon("fas fa-table"))),
            uiOutput(ns("down_cm_btn")),

        )),
      mainPanel(
        fluidRow(
          column(3,style="margin: 0px; padding: 0px",
                 inline(DT::dataTableOutput(ns('svm_tab_errors0_test')))
          ),
          column(9,uiOutput(ns("confusion_matrix_svm"))
          )
        )
      )
    )
  })
  get_svm_prederrors<-reactive({
    req(length(input$svmpred_which)>0)

    m<-vals$svm_results
    pred<-pred_svm()[,1]
    # x_t<-attr(m,"test")
    obs<-if(input$svmpred_which=="Partition"){
      attr(m,"sup_test")} else{
        req(length(input$predsvm_newY)>0)

        if(m$modelType=="Classification"){
          req(length(attr(m,"supervisor"))>0)
          req(length(input$predsvm_newY)>0)
          req(input$predsvm_newY%in%names(vals$saved_data))
          sup<-attr(m,"supervisor")
          factors<-attr(vals$saved_data[[input$predsvm_newY]],"factors")
          req(sup%in%colnames(factors))
          factors[,sup]} else{
            vals$saved_data[[input$predsvm_newY]][,attr(m,"supervisor")]
          }
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })
  test_svm<-reactive({
    req(input$svmpred_which)
    m<-vals$svm_results
    pred_tab<-if(input$svmpred_which=="Partition"){attr(m,"test")} else if(input$svmpred_which=="Datalist"){
      req(input$predsvm_new)
      pred_tab<-vals$saved_data[[input$predsvm_new]]
    }
    model_data<-pred_tab
    req(sum(colnames(pred_tab)%in%colnames(getdata_model(m)))==ncol(pred_tab))
    pred_tab

  })
  output$svm_tab_errors0_test<-DT::renderDataTable({
    req(input$round_pred_svm)
    table<-pred_test_svm()
    vals$sup_metrics<-table<-round(table,input$round_pred_svm)
    #colnames(table)<-NULL
    DT::datatable(table,
                  options=list(rownames=T,info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'),colnames=c(""))
  })
  output$confusion_svm_pred<-renderUI({
    req(vals$svm_results$modelType=="Classification")

    conf<-get_cm_pred()
    column(12,
           renderPlot({
             res<-plotCM(conf/sum(conf)*100, input$svmpalette_pred,  newcolhabs=vals$newcolhabs,round_cm=input$round_pred_svm,title=input$svm_cmpred_title)
             vals$svm_cm_pred<-res
             res
           }))

  })
  output$confusion_svm2_pred<-renderPrint({
    req(vals$svm_results$modelType=="Classification")
    res<-confusionMatrix(get_cm_pred())
    vals$svm_cm_test<-  as.data.frame.matrix(res$table)
    res
  })
  output$svm_pred_type<-renderUI({
    choices=if(length(attr(vals$svm_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(class="well2",style="padding: 10px",
        div(class="map_control_style2",style="color: #05668D",
            inline(radioButtons(ns("svmpred_which"),strong("New data (X):"),choices=choices,inline=T)), inline(uiOutput(ns("datalist_pred_svm")))
        )
    )
  })
  output$datalist_pred_svm<-renderUI({
    req(input$svmpred_which =='Datalist')
    div(
      pickerInput(ns("predsvm_new"),"->",names(vals$saved_data[getnewdatasvm()]), width = '300px', selected=vals$predsvm_new)
    )
  })
  pred_test_svm<-reactive({
    obs<-get_svm_prederrors()$obs
    pred<-get_svm_prederrors()$pred
    m<-vals$svm_results

    table<-stats_ml(obs,pred,m)


    table
  })

  output$confusion_matrix_svm<-renderUI({
    req(vals$svm_results$modelType=="Classification")
    div(

      style="overflow-x: scroll",
      uiOutput(ns("confusion_svm_pred")),
      verbatimTextOutput(ns("confusion_svm2_pred"))

    )
  })
  gettile_cmpred<-reactive({
    req(input$svmpred_which)
    ifelse(input$svmpred_which=="Partition","Test","Confusion Matrix")
  })
  output$svm_cmpred_title<-renderUI({
    textInput(ns("svm_cmpred_title"),"+ CM Title",gettile_cmpred())
  })
  output$svm_pred_pal<-renderUI({
    req(vals$svm_results$modelType=="Classification")
    lab2<-span("+",tipify(icon("fas fa-question-circle"),"Confusion Matrix Palette"),"Palette")
    div(
      uiOutput(ns('svm_cmpred_title')),
      pickerInput(inputId = ns("svmpalette_pred"),
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
    req(vals$svm_results$modelType=="Classification")

    div(
      div(actionLink(ns("dowcenter_cmsvm_pred"),span("+ Download Confusion",icon("fas fa-table")))),
      div(actionLink(ns("downp_cmsvm_pred"),span("+ Donwload plot"))
      )
    )

  })
##







  output$svm_tab_2_4<-renderUI({

    sidebarLayout(
      sidebarPanel(
        fluidRow(
          class="map_control_style",style="color: #05668D",
          div(
            span(
              "+ Type: ",
              pickerInput(inputId = ns("svm_cm_type"),
                          label = NULL,
                          choices = c("Resampling","Optimal Model"),

                          width="100px"
              )
            )
          ),
          div(
            span(
              "+ Palette",
              pickerInput(inputId = ns("svmpalette"),
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
            popify(actionLink(ns("downp_cmsvm"),span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style  = "button_active"),NULL,"download CM plot")
          ),
          div(
            popify(actionLink(ns("dowcenter_cmsvm"),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM table")
          )

        )

      ),
      mainPanel(
        plotOutput(ns("confusion_svm")),
        verbatimTextOutput(ns("confusion_svm2"))
      )
    )

  })
  predall_svm<-reactive({
    validate(need(!anyNA(test_svm()),"NAs not allowed in the prediction Datalist"))
    m<-vals$svm_results
    #factors<-attr(vals$saved_data[[input$predsvm_new]],"factors")
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
  svm_observed2<-reactive({
    req(input$svmpred_which)


    obs_data<-if(input$svmpred_which=="Datalist"){
      if(vals$svm_results$modelType=="Classification"){
        factors<-attr(vals$saved_data[[input$predsvm_newY_tree]],'factors')
      } else {
        factors<-vals$saved_data[[input$predsvm_newY_tree]]}

      res<-factors[,attr(vals$svm_results,"supervisor")]
      names(res)<-rownames(factors[attr(vals$svm_results,"supervisor")])
      res
    } else{
      attr(vals$svm_results,"sup_test")
    }
    obs_data
  })
  get_qclass<-reactive({
    m<-vals$svm_results
    req(m$modelType=="Regression")
    pred<-predall_svm()
    model_data<-test_svm()
    obs_data<-svm_observed2()
    obs_data<-data.frame(obs_data)
    ref1<-rownames(model_data)
    ref2<-rownames(obs_data)
    ref1<-ref2
    lo<-split(obs_data,ref2)

    pred_interval=input$q_class_val
    pred.svm.int <- data.frame(
      t(apply(pred, 1, function(x) {
        c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
      }))
    )


    lpred<-split(pred,ref1)
    lp<-split(pred.svm.int,ref1)
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
    vals$svm_treetest<-res
    vals$svm_treetest
  })
  output$svm_tree_show<-renderUI({
    val=if(vals$svm_results$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$svm_tree_pred<-renderUI({
    req(input$nhist_tree)
    data=t(predall_svm())
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
    req(vals$svm_results$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),'Mean predictions')

        ))
  })
  output$plot_forest<-renderUI({
    req(vals$svm_results$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({
    req(input$qclass_width)
    req(input$qclass_height)
    div(style="background: white",
        inline(uiOutput(ns("svm_tree_show"))),
        inline(uiOutput(ns("svm_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))

  })
  output$forest_byclass<-renderUI({
    req(vals$svm_results$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })
  get_forest_class<-reactive({
    req(input$predsvm_newY_tree)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$svm_results
    req(m$modelType=="Classification")
    pred<-predall_svm()
    pred.ind <- pred
    model_data<-test_svm()
    obs_data<-svm_observed2()
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




  output$predsvm_newY_ref<-renderUI({
    req(input$predsvm_newY_tree)
    req(input$svmpred_which)
    req(vals$svm_results$modelType=="Regression")
    factors<-attr(vals$saved_data[[input$predsvm_newY_tree]],"factors")
    choices=if(input$svmpred_which=="Datalist"){
      c('rownames',colnames(factors))} else{
        'rownames'
      }
    div(
      span("+ ref",inline(pickerInput(ns('svm_reftest'),NULL,choices, width="200px", selected=vals$svm_reftest)))

    )})
  output$forest_margin<-renderUI({
    req(vals$svm_results$modelType=="Regression")
    div(
      span("+ Margin:",
           inline(
             numericInput(ns("q_class_val"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    )
  })
  output$forest_col<-renderUI({
    req(vals$svm_results$modelType=="Classification")
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
  output$pick_svmobs<-renderUI({
    sup_test<-attr(vals$svm_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("predsvm_newY_tree"),
                       NULL,names(vals$saved_data[getobssvm()]), width="200px",selected=vals$predsvm_newY_tree
           ))
    )
  })
  output$resample_out<-renderUI({
    m<-vals$svm_results
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
      uiOutput(ns("pick_svmobs")),
      uiOutput(ns("predsvm_newY_ref")),
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
    req(vals$svm_results$modelType=="Regression")
    div(
      div(actionLink(ns('resamp_pred_gdownp'),"+ Download table")),
      div( actionLink(ns('resamp_pred_gcreate'),"Create Datalist"))
    )
  })
  output$summ_trees<-renderPlot({
    req(vals$svm_results$modelType=="Regression")

    get_qclass()
    req(input$q_class_val)

    req(input$splitdata_trees)
    req(input$nhist_tree)
    pred<-predall_svm()
    data=t(pred)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
    options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
    data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
    obs_data<-svm_observed2()
    obs_data<-data.frame(obs_data)
    obs=obs_data[ colnames(data),]

    pred <- pred
    pred_interval=input$q_class_val
    q_class=vals$svm_treetest[colnames(data),]
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
  observeEvent(ignoreInit = T,input$predsvm_newY_tree,{
    vals$predsvm_newY_tree<-input$predsvm_newY_tree
  })
  observeEvent(ignoreInit = T,input$resamp_pred_gcreate,{

    vals$hand_save<-"resample_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_svm())

  })
  observeEvent(ignoreInit = T,input$resamp_pred_gdownp,{

    vals$hand_down<-"svm_qclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })






  observeEvent(ignoreInit = T,input$predsvm_new,{
    vals$predsvm_new<-input$predsvm_new
  })


  observeEvent(ignoreInit = T,input$dowcenter_cmsvm_pred,{
    vals$hand_down<-"svm_cm_test"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter,"downcenter", vals=vals)
  })




  output$svm_tab3_1<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            class="map_control_style",style="color: #05668D",
            div(
              tipify(
                actionLink(
                  ns('svm_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                ),
                "Create a datalist with the svm predictions", options=list(container="body")
              )
            ),
            div(
              tipify(
                actionLink(
                  ns('down_svm_tab3_1'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                ),
                "+ Download table", options=list(container="body")
              )
            )

          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('svmtab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svmtab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          inline(DT::dataTableOutput(ns('svmtab_pred')))
        )
      )
    )
  })
  output$svmtab_pred<-DT::renderDataTable({
    table<-vals$svmtab_pred<-pred_svm()
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))

  }, rownames = TRUE,class ='compact cell-border')


  output$svm_inputs <- renderUI({
    if(is.null(vals$cur_svm_method)){vals$cur_svm_method<-"svmLinear"}

    if(is.null(vals$cur_model_type)){vals$cur_model_type<-"Classification"}
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",
        if(input$svm_tab=="svm_tab1"){
          div(
            div(class='well3',
                strong("Type:"),inline(radioButtons(ns('svm_type'),NULL,choices=c("Classification","Regression"),inline =T, selected=vals$cur_model_type))
            ),
            div(class="well3",
                span(strong("X:"),
                     inline(uiOutput(ns('data_svmX_out'))),
                     inline(
                       div(
                         div("Method:"),
                         pickerInput(ns("svm_method"),NULL,choices=c("svmLinear","svmRadial"), width="150px", selected=vals$cur_svm_method)
                       )
                     )
                )
            ),
            div(class="well3",
                span(strong("Y:"),
                     inline(
                       uiOutput(ns("data_svmY_out"))
                     ),
                     "::",
                     inline(uiOutput(ns('svm_Y'))),

                     inline(uiOutput(ns("svm_partition"))),
                     "::",
                     inline(uiOutput(ns("svm_test_ref")))
                )
            )


          )
        } else {
          column(12,
                 inline( uiOutput(ns('data_svmX_out'))),
                 inline(uiOutput(ns("svm_results_menu"))),
                 inline(uiOutput(ns("savesvm_button"))),
                 inline(uiOutput(ns("rmsvm_button"))),
          )
        }
    )
  })
  output$data_svmY_out<-renderUI({
    div(
      div("Datalist:"),
      div(pickerInput(ns("data_svmY"),NULL,choices =  names(vals$saved_data), width="150px", selected=vals$cur_dataY))
    )
  })
  output$svm_Y <- renderUI({
    req(input$data_svmY)
    req(input$svm_type)

    data <- vals$saved_data[[input$data_svmY]]
    labels <- attr(data,"factors")
    choices<-if(input$svm_type=="Classification"){rev(colnames(labels))} else{
      colnames(data)
    }

    div(well="class2",
        div("Variable"),
        div(tipify(pickerInput(
          ns("svm_sup"),
          NULL,
          choices =choices ,
          width="150px", selected=vals$cur_response
        ),"The response vector"))
    )

  })
  output$svm_partition<-renderUI({
    req(input$data_svmX)

    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("svm_test_partition"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_svmY]],"factors"))), width="150px", selected=vals$cur_test_partition))
    )
  })
  output$svm_test_ref<-renderUI({
    req(input$svm_test_partition!="None")
    req(input$data_svmY)

    fac<-attr(vals$saved_data[[input$data_svmY]],"factors")[,input$svm_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_svm"),NULL, choices=choices, width="200px", selected=vals$cur_testdata)
    )
  })
  output$data_svmX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_svmX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
        )
      ),      inline(uiOutput(ns("saved_svms")))

    )
  })

  output$saved_svms<-renderUI({

    req(input$data_svmX)
    names_svm<-names(attr(vals$saved_data[[input$data_svmX]],"svm"))
    req(length(names_svm)>0)
    pic<-which(names_svm%in%"new svm (unsaved)")
    if(length(pic)>0){
      names_svm<-names_svm[-pic]
    }
    req(length(names_svm)>0)
    div(class="saved_models",
        icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names_svm)), "saved model(s)")
  })

  output$svm_params<- renderUI({
    column(12,class="well2",
           div(class="map_control_style",style="color: #05668D; margin-top: 20px",
               div(
                 div(
                   tipify(icon("fas fa-question-circle"),"Tuning search", options = list(container="body")),
                   "+ search:",
                   inline(
                     pickerInput(ns("svm_search"), NULL, choices = c("grid","random","user-defined"), width="110px")
                   )
                 ),
                 uiOutput(ns("svm_search_length")),
               ),
               uiOutput(ns("svm_grid"))
               ,
               div(
                 tipify(icon("fas fa-question-circle"),textseed(), options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seedsvm"), NULL, value = NULL, width="122px")
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
               pickerInput(ns("svm_res_method"),NULL, choices=list("repeatedcv","boot","LOOCV","LGOCV"), width = "100px")
               ),
               uiOutput(ns("svm_resampling"))
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("svm_war"))
           ),

           column(12, align = "center",
                  popify(actionButton(ns("trainsvm"), h4(img(src=svm_icon,height='20',width='20'),"train Support Vector Machine",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")
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
        title=div("Train models using variables in",input$data_svmY),
        easyClose=T,

        div(
          column(12,
                 "This action will train",strong(ncol(getyloop()), style="color: red")," models, using the columns of Datalist: ",strong(input$data_svmY),"as Y. The given name of the models will be (",input$data_svmY,"::Y~Datalist_X), and any model already saved with this name(s) will be replaced.",em("We suggest using this tool from a Datalist without any saved svm model", style="color: red")),
          column(12,
                 div(strong("Click to proceed:")),
                 actionButton(ns("train_loop"),"Train_loop_YDatalist"))
        )
      )
    )

  })


  observeEvent(ignoreInit = T,input$train_loop,{
    removeModal()
    trainsvm_loop()
  })
  getyloop<-reactive({
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #input<-readRDS("input.rds")
    #vals<-readRDS("vals.rds")
    y<-vals$saved_data[[input$data_svmY]]
    if(input$svm_type== 'Classification'){y<-attr(y,"factors")}
    if(input$svm_test_partition!="None"){
      if(input$svm_type== 'Classification'){
        pic<-which(colnames(y)==input$svm_test_partition)
        if(length(pic)>0){
          y<-y[,-pic, drop=F]}
      }
    }
    y_loop<-y
    y_loop
  })

  trainsvm_loop<-reactive({
    #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #vals<-readRDS("vals.rds")
    #input<-readRDS("input.rds")
    y_loop<-getyloop()
    withProgress(message="Running",max=ncol(y_loop),{
      i=1
      for(i in 1:ncol(y_loop)) {
        t<-try({
          data<-getdata_svmX()
          x<-x_o<-data.frame(data)
          y<-y_o<-y_loop[,i]
          output$svm_war<-renderUI({
            column(12,style="color: red",align="center",
                   if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
                   if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
            )
          })
          validate(need(anyNA(x)==F,"NAs not allowed in X"))
          validate(need(anyNA(y)==F,"NAs not allowed in Y"))
          req(input$svm_test_partition)
          if(input$svm_test_partition!="None"){
            parts<-get_parts_svm()
            train<-parts$train
            test<-parts$test
            x<-data.frame(getdata_svmX()[train,])
            y<-y_o[train]
          }

          if(input$svm_search=="user-defined"){
            svm_search<-"grid"
            cost_svm<-as.numeric(unlist(strsplit(input$cost_svm,",")))
            if(input$svm_method!="svmLinear"){

              sigma_svm<-as.numeric(unlist(strsplit(input$sigma_svm,",")))
              grid <- expand.grid(C=c(cost_svm),sigma=c(sigma_svm))
            } else{
              grid <- expand.grid(C=c(cost_svm))
            }


          } else{
            grid<-svmGrid(x,y, len=input$svm_tuneLength, input$svm_search)
            if(input$svm_method=="svmLinear"){
              grid<-grid["C"]
            }
          }

          seed<-if (!is.na(input$seedsvm)) { input$seedsvm} else{
            NULL
          }
          svm_search<-input$svm_search

          colnames(x)<-gsub(" ",".", colnames(x))

          if (!is.na(input$seedsvm)) {set.seed(input$seedsvm)}


          svm<-train(x,y,input$svm_method,

                     trControl=trainControl(

                       method = input$svm_res_method,
                       number = input$cvsvm,
                       repeats = input$repeatssvm,
                       p=input$pleavesvm/100,
                       savePredictions = "all"

                     ),
                     tuneGrid=grid
          )
          attr(svm,"test_partition")<-paste("Test data:",input$svm_test_partition,"::",input$testdata_svm)
          attr(svm,"Y")<-paste(input$data_svmY,"::",colnames(y_loop)[i])
          attr(svm,"Datalist")<-paste(input$data_svmX)

          vals$svm_unsaved<-svm

          if(input$svm_test_partition!="None"){
            attr(vals$svm_unsaved,'test')<-x_o[test,]
            attr(vals$svm_unsaved,"sup_test")<-y_o[test]
          } else{ attr(vals$svm_unsaved,'test')<-c("None")}
          vals$bag_svm<-T
          attr(vals$svm_unsaved,"supervisor")<-colnames(y_loop)[i]
          attr(vals$svm_unsaved,"inputs")<-list(
            Ydatalist=input$data_svmY,
            Y=colnames(y_loop)[i],
            Xdatalist=input$data_svmX
          )






          attr(vals$saved_data[[input$data_svmX]],"svm")[['new svm (unsaved)']]<-vals$svm_unsaved
          vals$cur_svm_models<-"new svm (unsaved)"
          vals$bag_svm<-T

          attr( vals$svm_unsaved,"Y")<-paste0(input$data_svmY,"::",colnames(y_loop)[i])
          savesvm_loop()


          beep(10)
          #saveRDS(vals$nb_unsaved,"nb.rds")


        })



        if("try-error" %in% class(t)){
          output$svm_war<-renderUI({
            column(12,em(style="color: gray",
                         "Error in training the svm model. Check if the number of observations in X and Y are compatible"
            ))
          })

        } else{
          output$svm_war<-NULL
        }
        incProgress(1)
      }
      updateTabsetPanel(session,"svm_tab","svm_tab2")
    })

  })

  savesvm_loop<-reactive({

    temp<-vals$svm_unsaved
    name_model<-paste0(attr(temp,"Y"),"~",input$data_svmX, "(",input$svm_method,")")
    temp<-list(temp)
    names(temp)<-name_model
    attr(vals$saved_data[[input$data_svmX]],"svm")[[name_model]]<-temp
    cur<-name_model
    attr(vals$saved_data[[input$data_svmX]],"svm")['new svm (unsaved)']<-NULL
    vals$bag_svm<-F
    vals$cur_svm_models<-cur
    vals$cur_svm<-cur
    vals$svm_unsaved<-NULL
  })
  savesvm<-reactive({
    req(vals$cur_svm_models=='new svm (unsaved)')
    temp<-vals$svm_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[input$data_svmX]],"svm")[[input$newdatalist]]<-temp
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[input$data_svmX]],"svm")[[input$over_datalist]]<-temp
      cur<-input$over_datalist
    }
    attr(vals$saved_data[[input$data_svmX]],"svm")['new svm (unsaved)']<-NULL
    vals$bag_svm<-F
    vals$cur_svm_models<-cur
    vals$cur_svm<-cur

  })

  output$svm_grid<-renderUI({
    req(input$svm_search=="user-defined")
    div(
      div(
        tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for the cost regularization parameter. This parameter controls the smoothness of the fitted function, essentially higher values for C lead to less smooth functions (if a vector, comma delimited)", options=list(container="body")),
        "+ Cost",
        textInput(ns('cost_svm'), NULL, value ='1', width="200px")),
      if(input$svm_method=="svmRadial") {
        div(
          tipify(icon("fas fa-question-circle",style="color: gray"),"one or multiple numeric values for bandwidth of kernel function. If the sigma value is very small, then the decision boundary is highly non-linear. If a vector must be comma delimited between 0 and 1)", options=list(container="body")),
          "+ Sigma",
          textInput(ns('sigma_svm'), NULL, value ='1', width="200px")

        )
      }
    )
  })
  output$svm_search_length<-renderUI({
    req(input$svm_search!='user-defined')
    if(is.null(vals$cur_svm_tuneLength)){vals$cur_svm_tuneLength<-5}

    div(
      tipify(icon("fas fa-question-circle"),"The maximum number of mtry- combinations that will be generated", options = list(container="body")),
      "+ tuneLength:",
      inline(
        numericInput(ns("svm_tuneLength"), NULL, value = vals$cur_svm_tuneLength, width="82px")
      )
    )
  })
  output$svm_resampling<-renderUI({
    div(
      inline(
        if(input$svm_res_method=='cv'|input$svm_res_method=='repeatedcv')
        {
          div(

            tipify(icon("fas fa-question-circle"),"number of folds", options=list(container="body")),"+ cv:",
            numericInput(ns("cvsvm"), NULL, value = 5,width="140px")

          )
        }
      ),
      div(
        inline(
          if(input$svm_res_method=='repeatedcv')
          {
            inline(
              div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute", options=list(container="body")),
                  "+ repeats:",
                  numericInput(ns("repeatssvm"),NULL, value = 1,width="109px")
              )
            )

          }
        )
      ),
      inline(
        if(input$svm_res_method=='boot'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the number of resampling iterations", options=list(container="body")),
              "+ number:",
              numericInput(ns("cvsvm"), NULL, value = 5,width="100px")

            )
          )
        }
      ),
      inline(
        if(input$svm_res_method=='LGOCV'){
          inline(
            div(
              tipify(icon("fas fa-question-circle"),"the training percentage", options=list(container="body")),
              "+ percentage:",
              numericInput(ns("pleavesvm"), NULL, value = 10,width="100px")
            )
          )
        }
      )
    )
  })


  output$svm_results_menu<-renderUI({
    if(is.null(vals$cur_svm_models)){vals$cur_svm_models<-1}
    div(pickerInput(ns("svm_models"),strong("svm results:", tiphelp("Support Machine Vector Results. Click to select svm results saved in the Training Datalist (X).")), choices= names(attr(vals$saved_data[[input$data_svmX]],"svm")), width="200px", selected = vals$cur_svm_models)
    )
  })
  output$rmsvm_button<-renderUI({
    req(input$svm_models!="new svm (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmsvm"), icon("far fa-trash-alt")), "Remove the svm model from the training Datalist (X)")
    )

  })
  output$svm_tab_2_1<-renderUI({
    m<-vals$svm_results
    div(
      renderPrint(m),
      div(style="height: 100px",
          popify(downloadButton(ns("down_svm_results"),label=span("Download svm model",img(src=svm_icon,height='20',width='20')),style = "button_active"),NULL,
                 HTML(paste0(
                   div(HTML(paste0("Download model as ",em('rds')," file to be read in R as", code('base::readRDS(file.rds)'))))
                 )))
      )

    )
  })
  output$down_svm_results <- {
    downloadHandler(
      filename = function() {
        paste0("SVM","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$nb_results,file)
      })
  }
  observeEvent(ignoreInit = T,input$svm_tab2,{
    vals$svm_tab2<-input$svm_tab2
  })

  output$results_svm<-renderUI({
    validate(need(length(vals$svm_results)>0,"No models have been trained yet"))
    if(is.null(vals$svm_tab2)){vals$svm_tab2<-'svm_tab2_1'}
    #saveRDS(vals$svm_results,"svm.rds")
    req(length(vals$svm_results))
    m<-vals$svm_results
    #m<-readRDS("svm.rds")
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    column(12,
           div(
             p(
               strong("Model:"),em(getmodel_svm())
             )
           ),
           tabsetPanel(id=ns("svm_tab2"),selected=vals$svm_tab2,
                       tabPanel("2.1. Summary",value="svm_tab2_1",
                                uiOutput(ns("svm_tab_2_1"))
                       ),
                       tabPanel("2.2. Performace",value="svm_tab2_2",
                                uiOutput(ns("svm_tab_2_2"))
                       ),
                       tabPanel("2.3. Permutation importance",value="svm_tab2_3",
                                uiOutput(ns("permutation_importance"))

                       ),
                       tabPanel("2.4. Confusion matrix",value="svm_tab2_4",
                                uiOutput(ns("svm_tab_2_4"))
                       )
           )

    )
  })
  ##
  output$permutation_importance<-renderUI({
    req(input$svm_models)
    validate(need(input$svm_models!="new svm (unsaved)","
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
    m<-vals$svm_results
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
    m<-vals$svm_results
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
    m<-vals$svm_results
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
    m<-vals$svm_results
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    rep=input$feat_rep
    seed=input$feat_seed
    if(is.na(seed)){seed=NULL}
    predtable<- permute_importance_foreach_progress(m,newdata,obc, rep, seed=seed)
    stopCluster(cl)
    unregister_dopar()
    attr(vals$saved_data[[input$data_svmX]],"svm")[[input$svm_models]]$feature_rands<-predtable
  })
  output$feature_importance_plot<-renderUI({

    req(input$data_svmX)
    req(input$svm_models)
    model_list<- attr(vals$saved_data[[input$data_svmX]],"svm")[[input$svm_models]]
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

      vals$svm_varImp_plot<-p
      vals$svm_varImp_plot

    })
  })

  ##


  output$svm_tab_2_3side<-renderUI({
    req(length(vals$svm_results))
    m<-vals$svm_results
    Y<-which(colnames(m$trainingData)==".outcome")
    nvars<-ncol(m$trainingData[,-Y])
    req(length(nvars)>0)
    val<-if(nvars>10){10} else{
      nvars
    }
    if(m$modelType=="Classification"){    choices<-c("default","gg")} else{
      choices<-c("default")
    }


    div(
      class="map_control_style",style="color: #05668D",
      div("+ Number of variables:",
          numericInput(ns("varImp_n_svm"),NULL,value=val,step=1,max=nvars,width="80px")
      ),
      div("+ Plot type:", inline(pickerInput(ns("varimp_type"),NULL,choices=choices,width="100px"))),

      div(uiOutput(ns("varimp_gg"))),
      br(),
      div(
        tipify(
          actionLink(
            ns('svm_varImp'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style="button_active"
          ),
          "Download Table (importances)",options=list(container="body")
        )
      ),
      div(
        tipify(
          actionLink(
            ns('svm_varImp_plot'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")),style="button_active"
          ),
          "Download plot",options=list(container="body")
        )
      )


    )
  })

  output$varimp_gg<-renderUI({
    #req(input$varimp_type=='gg')
    div(class="palette",
        div(
          span("+ Palette:",
               inline(
                 pickerInput(inputId = ns("varimp_col"),label = NULL,choices = vals$colors_img$val,choicesOpt = list(content = vals$colors_img$img),options=list(container="body"),selected=vals$colors_img$val[1], width='120px')))),
        div(class="palette",
            div("+ display", inline(pickerInput(ns("varimp_pos"),NULL,c("dodge","stack"),width="100px"))
            ))
    )
  })

  output$svm_tab_2_3<-renderUI({
    req(vals$svm_results)
    req(input$varImp_n_svm)
    renderPlot({
      m<-vals$svm_results
      vals$svm_varImp<-caret::varImp(m)$importance
      vals$svm_varImp_plot<-plot_varimport(m,palette=input$varimp_col,vals$newcolhabs,input$varimp_pos, type=input$varimp_type, nvars=input$varImp_n_svm)
      vals$svm_varImp_plot
    })
  })


  output$svm_tab_2_2<-renderUI({
    div(
      sidebarLayout(
        sidebarPanel(
          div(class="map_control_style2",style="color: #05668D",
              numericInput(ns("round_performace"),"Round",2),
              div(
              tipify(
                actionLink(
                  ns('svm_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                ),
                "Create a datalist with the svm training errors", options=list(container="body")
              )
            ),
            div(
              tipify(
                actionLink(
                  ns('svm_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                ),
                "+ Download Table", options=list(container="body")
              )
            )


          )
        ),
        mainPanel(
          tags$style(
            paste(paste0("#",ns('svm_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svm_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          tags$style(
            paste(paste0("#",ns('svm_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
          ),
          tags$style(
            paste0("#",ns('svm_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
          ),
          div(
            p(strong("Global:")),
            inline(DT::dataTableOutput(ns('svm_tab_errors0_train')))
          ),

          div(
            p(strong("Observations:")),
            inline(DT::dataTableOutput(ns('svm_tab_errors_train')))
          )



        )
      )
    )
  })
  output$svm_tab_errors0_train<-DT::renderDataTable({
    req(input$round_performace)
    m<-vals$svm_results
    table<- round(m$results[rownames(m$bestTune),],input$round_performace)
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })
  output$svm_tab_errors_train<-DT::renderDataTable({
    m<-vals$svm_results
    table<-accu_rf_class(m)
    vals$svm_down_errors_train<-table
    table
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })



  observeEvent(ignoreInit = T,input$dowcenter_cmsvm,{
    vals$hand_down<-"svm_cm"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  output$confusion_svm <- renderPlot({

    validate(need(vals$svm_results$modelType=="Classification","Confusion matrices are only valid for classification models."))
    m<-vals$svm_results
    if(input$svm_cm_type=="Resampling"){
      res<-plotCM(m, input$svmpalette,  newcolhabs=vals$newcolhabs)
    } else{
      cm<-table(predict(vals$svm_results),vals$svm_results$trainingData[,'.outcome'])
      res<-plotCM(cm,input$svmpalette,vals$newcolhabs)
    }
    vals$cm_svm<-res
    res
  })
  output$confusion_svm2<-renderPrint({
    validate(need(vals$svm_results$modelType=="Classification","Confusion matrices are only valid for classification models."))
    res<-if(input$svm_cm_type=="Resampling"){
      confusionMatrix(vals$svm_results$pred$pred,vals$svm_results$pred$obs)
    } else{
      confusionMatrix(predict(vals$svm_results),vals$svm_results$trainingData[,'.outcome'])
    }
    vals$svm_cm<-as.data.frame.matrix(res$table)
    res
  })



  output$savesvm_button<-renderUI({
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_savesvm"), icon("fas fa-save")), "Save the svm model in the training Datalist (X)")
    )
  })

  getnewdatasvm<-reactive({
    datalist<-vals$saved_data

    datalist_comp<-lapply(
      vals$saved_data, function (x) {
        colnames(x)<-gsub(" ",".", colnames(x))
        x
      }

    )

    datalist_comp<-vals$saved_data




    m<-vals$svm_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-Y])
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })
  get_supervisor_svm <- reactive({
    m<-vals$svm_results
    att<-if(m$modelType=="Classification")
    {
      data <- vals$saved_data[[input$data_svmY]]
      labels <- attr(data,"factors")
      labels[input$svm_sup]
    } else{
      data <- vals$saved_data[[input$data_svmY]]
      data[input$svm_sup]
    }

  })
  get_cm_pred<-reactive({

    obs<-get_svm_prederrors()$obs
    pred<-get_svm_prederrors()$pred
    conf<-table(obs, pred)
    conf
  })






  getobssvm<-reactive({
    sup_test<-attr(vals$svm_results,"supervisor")

    datalist<-vals$saved_data
    if(vals$svm_results$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))}

    m<-vals$svm_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })

  pred_svm<-reactive({
    validate(need(!anyNA(test_svm()),"NAs not allowed in the prediction Datalist"))
    m<-vals$svm_results


    svm_pred <- predict(m$finalModel,newdata = test_svm())
    res<-data.frame(Predictions= svm_pred)
    rownames(res)<-rownames(test_svm())
    #colnames(res)<-attr(vals$svm_results,"supervisor")

    # attr(res,"obs")<-test_svm()
    res



  })

  getmodel_svm<-reactive({
    m<-vals$svm_results
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


  observeEvent(ignoreInit = T,input$predsvm_new,{
    vals$predsvm_new<-input$predsvm_new
  })
  observeEvent(ignoreInit = T,input$svmpred_which,{
    vals$svmpred_which<-input$svmpred_which
  })
  observeEvent(ignoreInit = T,input$svm_down_errors_train,{
    vals$hand_down<-"SVM - training errors (observations)"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$svm_varImp,{
    vals$hand_down<-"SVM - variable importance"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_svm_tab3_1,{
    vals$hand_down<-"SVM - predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$data_svmY,{
    vals$cur_dataY<-input$data_svmY
  })
  observeEvent(ignoreInit = T,input$svm_type,{
    vals$cur_model_type<-input$svm_type
  })
  observeEvent(ignoreInit = T,input$svm_tuneLength,{
    vals$cur_svm_tuneLength<-input$svm_tuneLength
  })
  observeEvent(ignoreInit = T,input$svm_method,{
    vals$cur_svm_method<-input$svm_method
  })
  observeEvent(ignoreInit = T,input$svm_sup,{
    vals$cur_response<-input$svm_sup
  })
  observeEvent(ignoreInit = T,input$data_svmX,{
    vals$cur_data<-input$data_svmX
  })
  observeEvent(ignoreInit = T,input$svm_test_partition,{
    vals$cur_test_partition<-input$svm_test_partition
  })
  observeEvent(ignoreInit = T,input$testdata_svm,{
    vals$cur_testdata<-input$testdata_svm
  })
  observeEvent(ignoreInit = T,input$svm_models,{
    vals$cur_svm_models<-input$svm_models
  })
  getdata_svmX<-reactive({
    req(input$data_svmX)
    data=vals$saved_data[[input$data_svmX]]
    data
  })
  getdata_svmY<-reactive({
    req(input$data_svmY)
    req(input$svm_sup)

    data <- vals$saved_data[[input$data_svmY]]
    if(input$svm_type=="Classification"){
      labels <- attr(data,"factors")
      res<-labels[rownames(vals$saved_data[[input$data_svmX]]),input$svm_sup]
      names(res)<-rownames(vals$saved_data[[input$data_svmX]])
      res
    } else{
      res<-data[rownames(vals$saved_data[[input$data_svmX]]),input$svm_sup]
      names(res)<-rownames(vals$saved_data[[input$data_svmX]])
      res
    }

  })
  get_parts_svm<-reactive({
    req(input$data_svmY)
    req(input$testdata_svm)
    req(input$svm_test_partition)
    data<-getdata_svmX()
    factors<-attr(vals$saved_data[[input$data_svmY]],"factors")[rownames(data),]
    if(input$svm_test_partition!='None'){
      lis<-split(rownames(factors),factors[,input$svm_test_partition])
      names(lis[[1]])<-lis[[1]]
      names(lis[[2]])<-lis[[2]]
      test_i<-which(names(lis)==input$testdata_svm)
      tra_i<-which(names(lis)!=input$testdata_svm)
      parts=list(train=lis[[tra_i]],test=lis[[test_i]])} else{
        parts=list(train=NULL,test=NULL)
      }

    parts

  })
  observeEvent(ignoreInit = T,input$trainsvm,{
    try({

      x<-x_o<-data.frame(getdata_svmX())
      y<-y_o<-getdata_svmY()



      output$svm_war<-renderUI({
        column(12,style="color: red",align="center",
               if(anyNA(x)){"Error: Missing values are not allowed in X"} else{NULL},
               if(anyNA(y)){"Error: Missing values are not allowed in Y"} else{NULL}
        )
      })
      validate(need(anyNA(x)==F,"NAs not allowed in X"))
      validate(need(anyNA(y)==F,"NAs not allowed in Y"))
      req(input$svm_test_partition)
      if(input$svm_test_partition!="None"){
        parts<-get_parts_svm()
        train<-parts$train
        test<-parts$test
        x<-data.frame(getdata_svmX()[train,])
        y<-getdata_svmY()[train]
      }
      #readrd(y,"y.rds")
      #saveRDS(x,"x.rds")
      #saveRDS(reactiveValuesToList(input),"input.rds")
      #y<-readRDS("y.rds")
      #x<-readRDS("x.rds")
      #input<-readRDS("input.rds")
      if(input$svm_search=="user-defined"){
        nb_search<-"grid"
        cost_svm<-as.numeric(unlist(strsplit(input$cost_svm,",")))
        if(input$svm_method!="svmLinear"){

          sigma_svm<-as.numeric(unlist(strsplit(input$sigma_svm,",")))
          grid <- expand.grid(C=c(cost_svm),sigma=c(sigma_svm))
        } else{
          grid <- expand.grid(C=c(cost_svm))
        }


      } else{
        grid<-svmGrid(x,y, len=input$svm_tuneLength, input$svm_search)
        if(input$svm_method=="svmLinear"){
          grid<-grid["C"]
        }
      }

      seed<-if (!is.na(input$seedsvm)) { input$seedsvm} else{
        NULL
      }
      svm_search<-input$svm_search

      colnames(x)<-gsub(" ",".", colnames(x))
      if (!is.na(input$seedsvm)) {set.seed(input$seedsvm)}
      withProgress(message = "Running Support Vector Machine ...",
                   min = 1,
                   max = 1,
                   {
                     svm<-train(x,y,input$svm_method,

                                trControl=trainControl(

                                  method = input$svm_res_method,
                                  number = input$cvsvm,
                                  repeats = input$repeatssvm,
                                  p=input$pleavesvm/100,
                                  savePredictions = "all"

                                ),
                                tuneGrid=grid
                     )
                     attr(svm,"test_partition")<-paste("Test data:",input$svm_test_partition,"::",input$testdata_svm)
                     attr(svm,"Y")<-paste(input$data_svmY,"::",input$svm_sup)
                     attr(svm,"Datalist")<-paste(input$data_svmX)

                     vals$svm_unsaved<-svm

                     if(input$svm_test_partition!="None"){
                       attr(vals$svm_unsaved,'test')<-x_o[test,]
                       attr(vals$svm_unsaved,"sup_test")<-y_o[test]
                     } else{ attr(vals$svm_unsaved,'test')<-c("None")}
                     vals$bag_svm<-T
                     attr(vals$svm_unsaved,"supervisor")<-input$svm_sup
                     attr(vals$svm_unsaved,"inputs")<-list(
                       Ydatalist=input$data_svmY,
                       Y=input$svm_sup,
                       Xdatalist=input$data_svmX
                     )
                     updateTabsetPanel(session,"svm_tab","svm_tab2")
                   })
      beep(10)
      attr(vals$saved_data[[input$data_svmX]],"svm")[['new svm (unsaved)']]<-vals$svm_unsaved
      vals$cur_svm_models<-"new svm (unsaved)"
      vals$bag_svm<-T
      #saveRDS(vals$svm_unsaved,"svm.rds")

    })
  })
  observeEvent(ignoreInit = T,input$downp_cmsvm,{
    vals$hand_plot<-"SVM - Confusion Matrix"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$downp_cmsvm_pred,{
    vals$hand_plot<-"SVM - Confusion Matrix (predictions)"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$svm_varImp_plot,{
    vals$hand_plot<-"SVM - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_svm_dens,{
    vals$hand_plot<-"SVM - DM plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observe({
    req(input$svm_models)
    if(input$svm_models=="new svm (unsaved)"){
      vals$svm_results<-vals$svm_unsaved } else{
        vals$svm_results<-attr(vals$saved_data[[input$data_svmX]],"svm")[[input$svm_models]][[1]]
      }
  })
  observeEvent(ignoreInit = T,input$svm_models,{
    vals$cur_svm<-input$svm_models
  })
  observeEvent(ignoreInit = T,input$tools_rmsvm,{
    attr(vals$saved_data[[input$data_svmX]],"svm")[input$svm_models]<-NULL
  })
  observeEvent(ignoreInit = T,input$tools_savesvm,{
    if(input$tools_savesvm %% 2){
      vals$hand_save<-"Save SVM model in"
      vals$hand_save2<-column(12,fluidRow(em(input$data_svmX, style="color:gray"),strong("::"), em("svm-Attribute", style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL

      showModal(module_svm())

    }
  })
  observeEvent(ignoreInit = T,input$svm_create_errors_train,{
    vals$hand_save<-"Create Datalist: SVM training errors -obs"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_svm())
  })
  observeEvent(ignoreInit = T,input$svm_create_predictions,{
    vals$hand_save<- "Create Datalist: SVM predictions"
    vals$hand_save2<-"Posterior probabilities will be saved as Data-Attribute, and final classifications as Factor-Attribute"
    vals$hand_save3<-NULL

    showModal(module_svm())
  })



  resample_create<-reactive({

    m<-vals$svm_results
    var<-attr(m,"supervisor")
    temp<-vals$svm_treetest
    datao<-vals$saved_data[[input$data_svmX]]
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
    var<-attr(vals$svm_results,"supervisor")
    name0<-paste("svm",var,"qclass", sep="_")
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
      ##svm
      "Save SVM model in"= { name_save_svm()},
      "Create Datalist: SVM training errors -obs"={name_svm_train_errors()},
      "Create Datalist: SVM predictions"={ name_svm_pred()},
      "resample_create"={name_resample_create()}
    )})

  svm_create_training_errors<-reactive({
    temp<-vals$svm_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$data_svmX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  svm_create_pred<-reactive({
    temp<-data.frame(vals$svmtab_pred)
    temp[,1]<-as.numeric( temp[,1])
    attr(temp,"factors")<-temp

    datao<-if(vals$svmpred_which=="Datalist"){
      vals$saved_data[[vals$predsvm_new]]
    } else{ vals$saved_data[[input$data_svmX]]}
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  name_save_svm<-reactive({
    bag<-1
    name0<-paste0("SVM")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[input$data_svmX]],"svm"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[input$data_svmX]],"svm"))) break
      }}
    paste(name0,bag)
  })
  name_svm_train_errors<-reactive({
    bag<-1
    name0<-paste0("SVM training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_svm_pred<-reactive({
    bag<-1
    name0<-paste0("SVM predictions")
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
    if(vals$hand_save=='Save SVM model in'){choices<-names(attr(vals$saved_data[[input$data_svmX]],'svm'))}
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
      "Save SVM model in"= {savesvm()},
      "Create Datalist: SVM training errors -obs"=svm_create_training_errors(),
      "Create Datalist: SVM predictions"={svm_create_pred()},
      'resample_create'={resample_create()}

    )
    removeModal()

  })
  module_svm <- function() {
    ns <- session$ns
    modalDialog(
      uiOutput(ns("databank_storage")),
      title=strong(icon("fas fa-save"),'Save'),
      footer=column(12,
                    uiOutput(ns('savesvm_teste')),
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
