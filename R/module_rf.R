
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
#' @export
module_ui_rf <- function(id){

  ns <- NS(id)
  tagList(
    tags$head(tags$style(
      HTML("
           .well3 {
 min-height: 20px;
   padding-left:5px;
    padding-top:5px;
    padding-bottom:5px;
    margin-bottom: 5px;
    background-color: #f5f5f5;
    border: 1px solid #e3e3e3;
    border-radius: 4px;
    -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
    box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%)
}"))),
#inline( actionButton(ns("teste_comb"),"SAVE")),
uiOutput(ns("rf_panels"))
  )

  }


#' @export
module_server_rf <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns

  output$rf_panels<-renderUI({

    column(12,
           uiOutput(ns("rf_inputs")),
           uiOutput(ns("war_rf")),
           div(
             tabsetPanel(id = ns("rf_tab"),
                         tabPanel(strong("1. Training"),value='rf_tab1',
                                  uiOutput(ns("rf_params"))),
                         tabPanel(
                           strong("2. Results"),value="rf_tab2",
                           uiOutput(ns("rf_results"))
                         ),
                         tabPanel(strong("3. Predict"),value="rf_tab3",style="background: white",
                                  uiOutput(ns("RF_predictions"))
                         )

             )
           )
    )
  })

observeEvent(ignoreInit = T,input$rf_tab,{
  vals$rf_tab<-input$rf_tab
})

  output$rf_inputs <- renderUI({

    if(is.null(vals$cur_model_type)){vals$cur_model_type<-"Classification"}
    #req(length(vals$saved_data)>0)
    div(style="color: #05668D;",

             if(input$rf_tab=="rf_tab1"){
               column(12,
                      uiOutput(ns("train_test")),
                 div(class='well3',
                     strong("Type:"),inline(radioButtons(ns('rf_type'),NULL,choices=c("Classification","Regresion"),inline =T, selected=vals$cur_model_type))
                 ),
                 div(class="well3",
                     span(strong("X:"),
                          inline(uiOutput(ns('data_rfX_out')))




                     )
                 ),

                 div(class="well3",
                     span(strong("Y:"),
                          inline(
                            div(
                              div("Datalist:"),
                              div(pickerInput(ns("data_rfY"),NULL,choices =  names(vals$saved_data), width="150px", selected=vals$cur_dataY))
                            )
                          ),
                          "::",
                          inline(uiOutput(ns('rf_supervisor'))),

                          inline(uiOutput(ns("rf_test_part"))),
                          "::",
                          inline(uiOutput(ns("rf_partition")))
                     )
                 )


               )
             } else {
               column(12,
                      inline( uiOutput(ns('data_rfX_out'))),
                      inline(uiOutput(ns("rf_results_menu"))),
                      inline(uiOutput(ns("saverf_button"))),
                      inline(uiOutput(ns("rmrf_button"))),
               )
             }
    )
  })
  output$rf_params <- renderUI({
    column(12,class="well2",

           div(class="map_control_style",style="color: #05668D; margin-top: 20px",

               div(
                 tipify(icon("fas fa-question-circle"),"Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times", options = list(container="body")),
                 "+ ntree:",
                 inline(
                   numericInput(ns("ntree"), NULL, value = 50, width="120px")
                 )
               ),
               div(
                 tipify(icon("fas fa-question-circle"),"how the mtry parameter (i.e. the number of variables randomly sampled as candidates at each split) is determined", options = list(container="body")),
                 "+ search:",
                 inline(
                   pickerInput(ns("rf_search"), NULL, choices = c("grid","random","user-defined"), width="110px")
                 )
               ),

               uiOutput(ns("rf_mtry"))
               ,

               uiOutput(ns("rf_grid_search")),

               div(
                 tipify(icon("fas fa-question-circle"),textseed(), options = list(container="body")),
                 "+ seed:",
                 inline(
                   numericInput(ns("seedrf"), NULL, value = NULL, width="122px")
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
               pickerInput(ns("res_method"),NULL, choices=list("repeatedcv","boot","LOOCV","LGOCV"), width = "100px")
               ),
               div(
                 uiOutput(ns("rf_resampling"))
               )
           ),

           column(12,style='white-space: normal;',
                  uiOutput(ns("rf_war"))
           ),

           column(12, align = "center",
                  uiOutput(ns("train_RF_button")),

           ),
           column(12,align="right",
             div(style="white-space: normal;max-width: 150px",
               actionLink(ns("gotrain_loop"),"+ Train all variables in Datalist Y")
             )
           ),
           #column(12,h4("Experimental*"),uiOutput(ns("RFloop")))


    )
  })

  observeEvent(ignoreInit = T,input$gotrain_loop,{
    showModal(
      modalDialog(
        title=div("Train models using variables in",input$data_rfY),
        easyClose=T,

        div(
          column(12,
                 "This action will train",strong(ncol(getyloop()), style="color: red")," models, using the columns of Datalist: ",strong(input$data_rfY),"as Y. The given name of the models will be (",input$data_rfY,"::Y~Datalist_X), and any model already saved with this name(s) will be replaced.",em("We suggest using this tool from a Datalist without any saved RF model", style="color: red")),
          column(12,
                 div(strong("Click to proceed:")),
                 actionButton(ns("train_loop"),"Train_loop_YDatalist"))
        )
      )
    )

  })

  observeEvent(ignoreInit = T,input$train_loop,{
    removeModal()
    trainrf_loop()
  })




  getyloop<-reactive({
   #saveRDS(reactiveValuesToList(input),"input.rds")
   #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #input<-readRDS("input.rds")
    #vals<-readRDS("vals.rds")
    y<-vals$saved_data[[input$data_rfY]]
    if(input$rf_type== 'Classification'){y<-attr(y,"factors")}
    if(input$rf_test_partition!="None"){
      if(input$rf_type== 'Classification'){
        pic<-which(colnames(y)==input$rf_test_partition)
        if(length(pic)>0){
        y<-y[,-pic, drop=F]}
      }
    }
    y
  })
  trainrf_loop<-reactive({
    req(input$rf_type)
    y<-getyloop()
    withProgress(message="Running",max=ncol(y),{
      i=1
      for(i in 1:ncol( y)) {
        t<-try({
          data<-getdata_rfX()
          rf_supervisor<-colnames(y)[i]
          envi<-envi_o<-data.frame(data)
          if (input$rf_type == 'Classification') {
            labels <- attr(vals$saved_data[[input$data_rfY]],"factors")
            re<-labels[i]
          } else {

            data <- vals$saved_data[[input$data_rfY]]
            re<-data[i]
          }
          sup<-sup_o<-re

          if(input$rf_test_partition!="None"){
            parts<-get_parts_rf()
            train<-parts$train
            test<-parts$test
            envi<-data.frame(envi_o[names(train),])
            sup<-sup_o[names(train),, drop=F]
          }


          if (input$rf_type == 'Classification') {
            sup[1] <- as.factor(sup[, 1])
          }
          seed<-if (!is.na(input$seedrf)) { input$seedrf} else{
            NULL
          }



          join <- na.omit(cbind(sup, envi[rownames(sup), ,drop=F]))
          envi <- join[-1]
          somC <- join[1]
          if(input$rf_search=="user-defined"){
            rf_search<-"grid"
            validate(need(length(vals$mtry_pool)>0,"Requires at least one mtry value"))
            tuneGrid=data.frame(mtry=vals$mtry_pool)
          } else{
            rf_search<-input$rf_search
            tuneGrid=NULL
          }
          withProgress(message = "Running Random Forest.... the time taken will depend on the model tuning",
                       min = 1,
                       max = 1,
                       {
                         RF = wrapRF(
                           envi,
                           data.frame(somC),
                           supervisor = "clusters",
                           prev.idw = F,
                           seed = seed,
                           ntree = input$ntree,

                           trainControl.args = list(
                             method = input$res_method,
                             number = input$cvrf,
                             repeats = input$repeatsrf,
                             p=input$pleaverf/100,
                             savePredictions = "final",
                             search=rf_search
                           ),
                           tuneLength=input$tuneLength,
                           tuneGrid=tuneGrid
                         )
                         attr(RF,"test_partition")<-paste("Test data:",input$rf_test_partition,"::",input$testdata_rf)
                         attr(RF,"Y")<-paste(input$data_rfY,"::",rf_supervisor)
                         attr(RF,"Datalist")<-paste(input$data_rfX)

                         if(input$rf_test_partition!="None"){
                           attr(RF,'test')<-data.frame(envi_o[names(test),])
                           sup_test<-sup_o[names(test),]
                           names(sup_test)<-names(test)
                           attr(RF,"sup_test")<-sup_test
                         } else{ attr(RF,'test')<-c("None")}

                         attr(RF,"supervisor")<-rf_supervisor
                         vals$cur_rftab2<-"rftab2_1"
                         updateTabsetPanel(session,"rf_tab","rf_tab2")
                         updateTabsetPanel(session,"rftab2","rftab2_1")
                         vals$rf_unsaved<-RF

                       })
          attr(vals$saved_data[[input$data_rfX]],"rf")[['new rf (unsaved)']]<-vals$rf_unsaved
          vals$cur_rf_models<-"new rf (unsaved)"

          attr(vals$saved_data[[input$data_rfX]],"rf")[[paste0(attr(RF,"Y"),"~",input$data_rfX)]]<-list(vals$rf_unsaved)
          beep(10)

        })



        if("try-error" %in% class(t)){
          output$rf_war<-renderUI({
            column(12,em(style="color: gray",
                         "Error in training the RF model. Check if the number of observations in X and Y are compatible"
            ))
          })

        } else{
          output$rf_war<-NULL
        }
        incProgress(1)
      }

    })

  })


  output$rf_resampling<-renderUI({
    div(
      if(input$res_method=='cv'|input$res_method=='repeatedcv'){
        div(
          tipify(icon("fas fa-question-circle"),"number of folds", options=list(container="body")),"+ cv:",
          numericInput(ns("cvrf"), NULL, value = 5,width="140px")

        )
      },
      if(input$res_method=='repeatedcv'){
        inline(
          div(tipify(icon("fas fa-question-circle"),"the number of complete sets of folds to compute", options=list(container="body")),
              "+ repeats:",
              numericInput(ns("repeatsrf"),NULL, value = 1,width="109px")
          )
        )},
      if(input$res_method=='boot'){
        inline(
          div(
            tipify(icon("fas fa-question-circle"),"the number of resampling iterations", options=list(container="body")),
            "+ number:",
            numericInput(ns("cvrf"), NULL, value = 5,width="100px")

          )
        )

      },
      if(input$res_method=='LGOCV'){
        inline(
          div(
            tipify(icon("fas fa-question-circle"),"the training percentage", options=list(container="body")),
            "+ percentage:",
            numericInput(ns("pleaverf"), NULL, value = 10,width="100px")
          )
        )
      }
    )


  })
  observeEvent(ignoreInit = T,input$predrf_tab,{
    vals$rftab3<-input$predrf_tab
  })
  output$RFloop<-renderUI({
    if(input$rf_search=="user-defined" ){
      req(length(vals$mtry_pool)>0)
    }
    column(12,

      inline(numericInput(ns('RFloop_n'),"Repeats",10, width="100px")),
      inline(numericInput(ns('rf_decay'),"Decay",10, width="100px")),
      popify(actionButton(ns("RFloop_go"), "Train loop", style = "background:  #05668D; color: white"),NULL,"Click to run"),
      uiOutput(ns("rf_bestmodel"))

    )
  })
  output$rf_bestmodel<-renderUI({
    renderPrint(
      attr(vals$saved_data[[input$data_rfX]],"rf")[[paste(name_saverf(),"best_model")]]
    )
  })

  observeEvent(ignoreInit = T,input$RFloop_go,{
    models<-c()
    envi<-envi_o<-getdata_rfX()
    sup<-sup_o<-get_supervisor()

    if(input$rf_test_partition!="None"){
      parts<-get_parts_rf()
      train<-parts$train
      test<-parts$test
      envi<-envi_o[names(train),]
      sup<-sup_o[names(train),, drop=F]
    }


    if (input$rf_type == 'Classification') {
      sup[1] <- as.factor(sup[, 1])
    }
    seed<-if (!is.na(input$seedrf)) { input$seedrf} else{
      NULL
    }
    if(input$rf_search=="user-defined"){
      rf_search<-"grid"
      validate(need(length(vals$mtry_pool)>0,"Requires at least one mtry value"))
      tuneGrid=data.frame(mtry=vals$mtry_pool)
    } else{
      rf_search<-input$rf_search
      tuneGrid=NULL
    }

    attr(vals$saved_data[[input$data_rfX]],"rf")<-list()
    withProgress(message="Running",min=0, max=input$RFloop_n,{
      for(i in 1:input$RFloop_n){
        i<-trainrf()
        temp<-list(vals$rf_unsaved)
        names(temp)<-name_saverf()
        models[i]<-name_saverf()
        attr(vals$saved_data[[input$data_rfX]],"rf")[[name_saverf()]]<-temp

        #rf_models[[i]]<-temp

        incProgress(1)

      }
      vals$rf_models<-models
    })

    data<-vals$saved_data[[input$data_rfX]]

    rfs<-names(attr(data,'rf'))
    rfs<-lapply(names(attr(data,"rf")), function(x){
      getrf(data, x)
    })

    res<-lapply(rfs,function(x) {
      res0<-varImp(x)
      sort(apply(res0$importance,1,mean), decreasing=T)
    })

    var_rank0<-apply(do.call(rbind,res),2,mean)
    train=rfs[[1]]$trainingData
    i=1
    df=train

    res_stepwise<-list()
    maxx<-floor(ncol(df)/input$rf_decay)
    #df<-readRDS("df.rds")



    withProgress(max=maxx, min=0, message="Running...",{
      for(i in 1:(maxx)) {
        traindata<-df[-1]
        resdata<-df[1]

        #incProgress(1)
        m<- wrapRF(
          traindata,
          data.frame(resdata),
          supervisor = "clusters",
          prev.idw = F,
          seed = seed,
          ntree = input$ntree,

          trainControl.args = list(
            method = input$res_method,
            number = input$cvrf,
            repeats = input$repeatsrf,
            p=input$pleaverf/100,
            savePredictions = "final",
            search=rf_search
          ),
          tuneLength=input$tuneLength,
          tuneGrid=tuneGrid
        )
        var_rank0= names(sort(apply(varImp(m$finalModel),1,mean))[1:input$rf_decay])
        df<-df[,-which(colnames(df)%in%var_rank0)]
        res_stepwise[[i]]<-m
        cat("\n",i)
        incProgress(1)
        beep()
      }
    })

    accs<-lapply(res_stepwise, function(x){
      conf<-x$finalModel$confusion
      conf<-conf[,-ncol(conf)]
      acc<-sum(diag(conf))/sum(conf)
    })
   #RF<-res_stepwise[[which.max(unlist(accs))]]
   RF<-res_stepwise[[length(res_stepwise)]]
   attr(RF,"test_partition")<-paste("Test data:",input$rf_test_partition,"::",input$testdata_rf)
   attr(RF,"Y")<-paste(input$data_rfY,"::",input$rf_sup)
   attr(RF,"Datalist")<-paste(input$data_rfX)

   if(input$rf_test_partition!="None"){
     attr(RF,'test')<-data.frame(envi_o[names(test),])
     sup_test<-sup_o[names(test),]
     names(sup_test)<-names(test)
     attr(RF,"sup_test")<-sup_test
   } else{ attr(RF,'test')<-c("None")}

   attr(RF,"supervisor")<-input$rf_sup
   vals$cur_rftab2<-"rftab2_1"
   updateTabsetPanel(session,"rf_tab","rf_tab2")
   updateTabsetPanel(session,"rftab2","rftab2_1")
   vals$rf_unsaved<-RF

    temp<-list(RF)

    names(temp)<-name_saverf()
    models[i]<-name_saverf()
    attr(vals$saved_data[[input$data_rfX]],"rf")[[paste(name_saverf(),"best_model")]]<-temp



  })





  output$train_RF_button<-renderUI({
    if(input$rf_search=="user-defined" ){
      req(length(vals$mtry_pool)>0)
    }

    popify(actionButton(ns("trainRF"), h4(icon("fas fa-tree"),icon("fas fa-tree", style = "margin-left: -10px;"),icon("fas fa-tree", style = "margin-left: -10px;"),"train RF",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")
  })
  observeEvent(ignoreInit = T,input$data_rfX,{
    output$RF_predictions<-renderUI({
      column(12,style="background: white", uiOutput(ns("rf_pred_type")),


             tabsetPanel(id=ns("predrf_tab"),
                         tabPanel(
                           strong("3.1. Results"), value="rftab3_1",
                           div(style="background: white",
                                    uiOutput(ns("rf_tab3_1")))),
                         tabPanel(
                           strong("3.2. Performace"), value="rftab3_3",
                           div(style="background: white",
                                    uiOutput(ns("rf_errors")))
                         ),
                         tabPanel(
                           strong("3.3. Confusion Matrix"), value="rftab3_4",
                           div(style="background: white",
                               uiOutput(ns("rf_metrics")))
                         )
             )
      )
    })
  })


  output$rf_pred_type<-renderUI({
    choices=if(length(attr(vals$rf_results,'test'))==1){c("Datalist")
    } else{c("Partition","Datalist")}
    div(class="well2",style="padding: 10px",
        div(class="map_control_style2",style="color: #05668D",
            inline(radioButtons(ns("rfpred_which"),strong("New data (X):"),choices=choices,inline=T)), inline(uiOutput(ns("datalist_pred_rf")))
        )
    )
  })
  output$datalist_pred_rf<-renderUI({
    req(input$rfpred_which =='Datalist')
    div(
      pickerInput(ns("predrf_new"),"->",names(vals$saved_data[getnewdatarf()]), width = '300px', selected=vals$predrf_new)
    )
  })







  observe({
    req(input$rf_models)
    if(input$rf_models=="new rf (unsaved)"){
      vals$rf_results<-vals$rf_unsaved } else{
        rf_attr<-attr(vals$saved_data[[input$data_rfX]],"rf")
        if(is.list(rf_attr[[input$rf_models]][[1]])){
          vals$rf_results<-rf_attr[[input$rf_models]][[1]]
        } else{
          vals$rf_results<-rf_attr[[input$rf_models]]
        }
      }

  })






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


  observeEvent(ignoreInit = T,input$rftab2,{
    vals$cur_rftab2<-input$rftab2
  })

  output$rf_results<-renderUI({
    if(is.null(vals$cur_rftab2)){vals$cur_rftab2<-"rftab2_1"}
    validate(need(length(vals$rf_results)>0,"Please train a RF model in "))
    tabsetPanel(
      id=ns("rftab2"),selected =vals$cur_rftab2,
      tabPanel(value="rftab2_1",
               strong("2.1. Summary"),
               div(style="background: white",
                      uiOutput(ns("rfsummary")),
                      splitLayout(
                        div(renderPrint(gettrain(vals$rf_results))),
                        div(renderPrint(vals$rf_results$results))
                      ),
                      plotOutput(ns("rf_mtry_plot"))

               )),
      tabPanel(value="rftab2_2",
               strong("2.2. Performace "),
               uiOutput(ns("rf_table"))),
      tabPanel("2.3. Permutation importance",value="tab3",
               uiOutput(ns("permutation_importance"))

      ),
      tabPanel(strong("2.4. RandomForest Explainer"),value='rftab2_3',
               uiOutput(ns("RFexp"))),
      tabPanel(value="rftab2_4",
               strong("2.5. Confusion Matrix"),
               uiOutput(ns("rftab2_4_res")))


    )
  })
  ##
  output$permutation_importance<-renderUI({
    req(input$rf_models)
    validate(need(input$rf_models!="new rf (unsaved)","
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
                                 ns('rf_varImp'),span("+ Download table"),style="button_active"
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

  observeEvent(ignoreInit = T,input$down_rf_tab3_1,{
    vals$hand_down<-"rf - predictions"
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
            ns('rf_varImp_plot'),span('+ Download plot'),style="button_active"
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
  observeEvent(ignoreInit = T,input$rf_varImp_plot,{
    vals$hand_plot<-"rf - Variable Importance plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
  })


  output$feature_cm<-renderUI({
    m<-vals$rf_results
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
    model_list<- attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]]
    req(length(model_list)>0)
    predtable<-model_list$feature_rands
    dft<-get_cm_impact(predtable, var=input$var_feature_cm)

    renderPlot({
      vals$feature_cm_plot<- plotCM(dft, palette=input$pal_feature_cm,vals$newcolhabs, font_color="black", title="", round_cm=3)
      vals$feature_cm_plot
    })
  })
  getmetric<-reactive({
    m<-vals$rf_results
    metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
    metric
  })
  get_sumamry_permute_importance<-reactive({
    req(input$sig_feature)
    model_list<- attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]]
    sig_level<-input$sig_feature
    req(length(model_list)>0)
    df<-model_list$feature_rands
    m<-model_list[[1]]
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    df<-permimp_metric(df,metric=getmetric(), class=NULL,m, newdata,obc)

    req(length(df)>0)
    req(is.data.frame(df))

    vals$rf_varImp<-perm_summary<-sig_feature(df,m, newdata, obc,sig_level)
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
    m<-vals$rf_results
    newdata<-getdata_model(m)
    obc<-getdata_model(m,"test")
    rep=input$feat_rep
    seed=input$feat_seed
    if(is.na(seed)){seed=NULL}
    predtable<- permute_importance_foreach_progress(m,newdata,obc, rep, seed=seed)
    stopCluster(cl)
    unregister_dopar()
    attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]]$feature_rands<-predtable
  })
  output$feature_importance_plot<-renderUI({
    req(input$data_rfX)
    req(input$rf_models)
    model_list<- attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]]
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

      vals$rf_varImp_plot<-p
      vals$rf_varImp_plot

    })
  })

  ##

  output$rf_mtry_plot<-renderPlot({
    req(length(vals$rf_results)>0)
    m<-vals$rf_results
    res<-m$results

    plot(res[,2]~res$mtry, type="n", col="darkblue", las=1, xlab="# Randomly Selected Predictors",ylab=paste(colnames(res)[2],resampName(m,F)), pch=16, ann=F, axes=F, xaxp=NULL)
    grid()
    par(new=T)
    plot(res[,2]~res$mtry, type="b", col="darkblue", las=1, xlab="# Randomly Selected Predictors",ylab=paste(colnames(res)[2],resampName(m,F)), pch=16)
    abline(v=res[which(res[,1]==m$finalModel$mtry),1], lty=2,col="red")


  })


  output$rftab2_4_res <- renderUI({

    validate(need(is.factor( vals$rf_results$finalModel$y), "Confusion matrices are only valid for classification (factor) models."))
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          class="map_control_style",style="color: #05668D",
          uiOutput(ns("rf_cm_type")),
          uiOutput(ns("rfpalette")),
          uiOutput(ns("cm_train_title")),

          div(actionLink(ns("downp_cmrf"), span("+ Download plot"),style  = "button_active")),
          div(
            actionLink(ns("dowcenter_cmrf"), span("+ Download Confusion Matrix"),style  = "button_active")
          ),
          div(actionLink(ns("dc_cmbyclass"), span("+ Download Statistics by Class"),style  = "button_active")),

          div(
            popify(actionLink(ns("dc_cmoverall"), span("+ Download Overall statistics"),style  = "button_active"),NULL,"download table")
          )

        )

      ),
      mainPanel(
        plotOutput(ns("CM")),
        verbatimTextOutput(ns("Confusion_RF"))
      )
    )
  })
  output$rf_cm_type<-renderUI({
    div(
      span(
        "+ Type: ",
        pickerInput(inputId = ns("rf_cm_type"),
                    label = NULL,
                    choices = c("Resampling","Optimal Model"),

                    width="100px"
        )
      )
    )
  })
  output$rfpalette<-renderUI({
    div(
      span(
        "+ Palette",
        pickerInput(inputId = ns("rfpalette"),
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
    )
  })

  output$cm_train_title<-renderUI({
    if(is.null(vals$cm_train_title)){vals$cm_train_title<-  "Confusion Matrix"}

    div(
      span("+ Title:",
           inline(
             textInput(ns("cm_train_title"),NULL, value="Training", width="200px")
           )
      )
    )
  })

  observeEvent(ignoreInit = T,input$dc_cmbyclass,{
    vals$hand_down<-"rf_cmbyclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })

  observeEvent(ignoreInit = T,input$dc_cmoverall,{
    vals$hand_down<-"rf_cmoverall"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })


  observeEvent(ignoreInit = T,input$dc_cmTest_byclass,{
    vals$hand_down<-"rf_cmTest_byclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })



  observeEvent(ignoreInit = T,input$dc_cmTest_overall,{
    vals$hand_down<-"rf_cmTest_overall"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })


  observeEvent(ignoreInit = T,input$dowcenter_cmTest_rf,{
    vals$hand_down<-"rf_cmTest_rf"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })

  output$CM <- renderPlot({
    if(input$rf_cm_type=="Resampling"){
    m<-vals$rf_results} else{
      m<-vals$rf_results$finalModel
    }
    attr(m,'title')<- attr(getConfusion(m),'title')
    res<-plotCM(m, input$rfpalette,  newcolhabs=vals$newcolhabs, title=input$cm_train_title)
    vals$cm_rf<-res
    res
  })

  observeEvent(ignoreInit = T,input$rftab2_3,{
    vals$rftab2_3<-input$rftab2_3
  })

  output$Confusion_RF<-renderPrint({
    res<-if(input$rf_cm_type=="Resampling"){
      res<-confusionMatrix(vals$rf_results$pred$pred,vals$rf_results$pred$obs)
    } else{
      confusionMatrix(predict(vals$rf_results$finalModel),vals$rf_results$trainingData[,1])
    }

    vals$rf_stat_byclass<-res$byClass
    vals$rf_stat_overall<-res$overall

    vals$rf_cm<-as.data.frame.matrix(res$table)

    res
    })


  output$RFexp<-renderUI({
    if(is.null(vals$rftab2_3)){vals$rftab2_3<-"rftab2_3_1"}
    div(
        tabsetPanel(
          id=ns('rftab2_3'),selected =vals$rftab2_3,
          tabPanel(value="rftab2_3_1",
            span("2.3.1. Measures"),
            div(
                uiOutput(ns("rf_measure"))
            )
          ),
          tabPanel(value="rftab2_3_2",
            span("2.3.2. Min Depth Distr."),

            div(
                uiOutput(ns("rf_mindepth"))
            )
          ),
          tabPanel(value="rftab2_3_3",
            span("2.3.3. Multi-way"),
            div(
                uiOutput(ns("rf_multi"))
            )
          ),
          tabPanel(value="rftab2_3_4",
                   "2.3.4. Relationships",
                   uiOutput(ns("rf_relations"))

          ),


          tabPanel(
            value="rftab2_3_5",
            span("2.3.5. Interactions"),
            div(
                tabsetPanel( id=ns('inter_frame_tab'),selected =vals$inter_frame_tab,
                  tabPanel(span("2.3.5.1. Variable Interactions"),
                           value="inter_frame_tab1",
                           div(uiOutput(ns("rftab2_3_5_1")))),
                  tabPanel(span("2.3.5.2. Biplot Interactions"),
                           value='inter_frame_tab2',
                           div(uiOutput(ns("rftab2_3_5_2")))),

                )

            )
          )
        )

    )

  })
  observeEvent(ignoreInit = T,input$inter_frame_tab,{
    vals$inter_frame_tab<-input$inter_frame_tab
  })
  output$PD_LOOP<-renderUI({

    m<-vals$rf_results
    res_pd<-attr(m,"best_pds")
    req(!is.null(res_pd))


    vals$rfbiplot2<-plot_pd_groups(res_pd,m,
                                   pd_size_plot=input$pd_size_plot,
                                   pd_palette=input$pd_palette,
                                   newcolhabs=vals$newcolhabs)
    renderPlot(vals$rfbiplot2)
  })






  observeEvent(ignoreInit = T,input$pdloop_go,{


    m<-vals$rf_results
    interactions_frame=attr(m,"interframe")
    #saveRDS(m,"m.rds")
    #saveRDS(interactions_frame,"interactions_frame.rds")
 # savereac()
    #interactions_frame<-readRDS("interactions_frame.rds")
    #m<-readRDS('m.rds')
    #input<-readRDS('input.rds')
    #vals<-readRDS('savepoint.rds')
    var0=as.character(input$pd_var1)
    var1=as.character(input$pd_var2)
    res<-loop_pd3(m,interactions_frame,var0=var0,var1=var1, grid=input$rfbi_grid)
    attr(attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]][[1]],"best_pds")<-res
    attr(vals$rf_results,"best_pds")<-res

  })

  observeEvent(ignoreInit = T,input$pd_var2,{
    vals$pd_var2<-input$pd_var2
  })

  output$pd_varxy<-renderUI({
    interactions_frame<-attr(vals$rf_results,"interframe")
    req(!is.null(interactions_frame))
    div(
      div("Root:"),
      div(
        inline(pickerInput(ns("pd_var1"), NULL,choices=as.character(unique(interactions_frame$root_variable)), selected=vals$pd_var1, width="200px")),

      )
    )
  })
  observeEvent(ignoreInit = T,input$pd_var1,{
    vals$pd_var1<-input$pd_var1
  })
  output$pd_var2<-renderUI({
    req(input$pd_var1)
    interactions_frame<-attr(vals$rf_results,"interframe")
    selected=input$pd_var1
    choices=unique(interactions_frame$variable[interactions_frame$variable!=selected])
    pickerInput(ns("pd_var2"), "Variable:",choices=choices, selected=vals$pd_var2, width="200px")
  })


  output$rftab2_3_5_2<-renderUI({
    sidebarLayout(
      sidebarPanel(
        strong("Partial Dependence",tipify(icon("fas fa-question-circle"),"Plot partial dependence of two variables(i.e., marginal effects) for the randomForest", options =list(container="body")), style="font-size: 16px"),
        uiOutput(ns("side_rf_grid"))),
      mainPanel(style="background: white",
                tabsetPanel( id=ns('pd_tab'),selected =vals$pd_tab,
                  tabPanel("Biplot",
                           value="pd_tab1",
                           withSpinner(type=8,color="SeaGreen",div(style="margin-top: 10px",plotOutput(ns("rfbiplot1"))))),
                  tabPanel(span("Classes side by side"),
                           value="pd_tab2",
                           uiOutput(ns("pd_sidebyside")))
                )

      )
    )
  })


  output$pd_sidebyside<-renderUI({
    validate(need(vals$rf_results$modelType=="Classification","Functionality only valid for classification models"))
    div(
      inline( uiOutput(ns("pd_varxy"))),
      inline(uiOutput(ns("pd_var2"))),
      inline( actionButton(ns("pdloop_go"),"RUN")),
      uiOutput(ns("PD_LOOP")))
  })

  observeEvent(ignoreInit = T,input$pd_tab,{
    vals$pd_tab<-input$pd_tab
  })
  output$rf_interbiplot2<-renderUI({
    frame<-attr(vals$rf_results,"interframe")
    pic<-which(frame[,1]==frame[,2])
    choices<-frame$interaction[-pic]

    if(input$rf_useinter=="Interaction Frame"){
      div(
        span("+ Interaction:",
             div(
               pickerInput(ns("rf_interaction"), NULL, choices=choices, width="200px",options =list(container="body"))
             )
        )
      )
    } else{
      div(
        div(
          span("+ Variable 1",
               inline(
                 pickerInput(ns("rf_grid_var1"), NULL, choices=c(colnames(vals$rf_results$trainingData[-1])), width="100px")
               )
          )
        ),
        div(
          span("+ Variable 2",
               inline(
                 pickerInput(ns("rf_grid_var2"), NULL, choices=c(colnames(vals$rf_results$trainingData[-1])), width="100px", selected=colnames(vals$rf_results$trainingData[-1])[2])
               )
          )
        )
      )
    }
  })
  observeEvent(ignoreInit = T,input$rf_useinter,{
    vals$rf_useinter<-input$rf_useinter
  })
  observeEvent(ignoreInit = T,input$rfbi_grid,{
    vals$rfbi_grid<-input$rfbi_grid
  })
  output$side_rf_grid<-renderUI({
    if(is.null(vals$rfbi_grid)){vals$rfbi_grid<-8}

    fluidRow(class="map_control_style",style="color: #05668D",

             div(
               span("+ Use",
                    inline(
                      pickerInput(ns("rf_useinter"), NULL, choices=c("Interaction Frame","Custom Variables"), width="150px", selected=vals$rf_useinter)
                    )
               )
             ),
             uiOutput(ns("rf_interbiplot2")),
             if(vals$rf_results$modelType=="Classification"){
               div(
                 span("+ Class",
                      inline(
                        pickerInput(ns("rf_biplotclass"), NULL, choices=levels(vals$rf_results$trainingData[,1]), width="150px")

                      )
                 )
               )
             },
             div(
               span(span(tipify(icon("fas fa-question-circle"),"Integer giving the number of equally spaced points to use for the continuous variables", options =list(container="body")),'+ Grid resolution'),
                    inline(
                      numericInput(ns('rfbi_grid'),NULL,vals$rfbi_grid, width="75px")
                    )
               )
             ),
             uiOutput(ns(
               "pd_plot_custom"
             ))


    )
  })
observeEvent(ignoreInit = T,input$pd_size_plot,{
  vals$pd_size_plot<-input$pd_size_plot
})
observeEvent(ignoreInit = T,input$pd_palette,{
  vals$pd_palette<-input$pd_palette
})
output$title_pd<-renderUI({
  req(input$pd_tab=='pd_tab1')
  div(
    span("+ Title:",
         inline(
           textInput(ns("title_pd"),NULL, value=NULL, width="200px")
         )
    )
  )
})
  output$pd_plot_custom<-renderUI({
    var1=picvars_pd()$var1
    var2=picvars_pd()$var2
    if(is.null(vals$pd_size_plot)){vals$pd_size_plot<-10}
    if(is.null(vals$pd_palette)){vals$pd_palette<-vals$colors_img$val[2]}
    div(
      class="well3",
      uiOutput(ns("title_pd")),

      div(
        span("+ X label:",
             inline(
               textInput(ns("xlab_pd"),NULL, value=var1, width="200px")
             )
        )
      ),
      div(
        span("+ Y label:",
             inline(
               textInput(ns("ylab_pd"),NULL, value=var2, width="200px")
             )
        )
      ),


      div(
        span("+ Palette:",
             inline(
               pickerInput(inputId = ns("pd_palette"),
                           label = NULL,
                           selected=vals$pd_palette,
                           choices =vals$colors_img$val[getgrad_col()],
                           choicesOpt = list(
                             content =vals$colors_img$img[getgrad_col()]),
                           options=list(container="body"),
                           width='100px')
             )
        )
      ),
      div(
        span("+ Size:",
             inline(
               numericInput(ns("pd_size_plot"),NULL, value=vals$pd_size_plot, width="75px")
             )
        )
      ),
      div(
        actionLink(ns('downp_rfbi'),span("+ Download plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active")
      )
    )
  })
  picvars_pd<-reactive({
req(input$rf_useinter)
    if(input$rf_useinter=='Interaction Frame'){
      req(input$rf_interaction)
      #isolate( saveRDS(reactiveValuesToList(input),"input.rds"))
      # isolate( saveRDS(reactiveValuesToList(vals),"vals.rds"))
      #
      pic<-which(attr(vals$rf_results,"interframe")$interaction==input$rf_interaction)
      var1<-attr(vals$rf_results,"interframe")$variable[pic]
      var2<-as.character(attr(vals$rf_results,"interframe")$root_variable[pic])
    }else{
      #req(input$rf_grid_var1)
      var1<-input$rf_grid_var1
      var2<-input$rf_grid_var2
    }
    list(var1=var1,var2=var2)
  })

  output$rfbiplot1<-renderPlot({
    req(input$rf_useinter)
    req(input$rfbi_grid)
    req(input$ylab_pd)
    req(input$xlab_pd)
    req(input$pd_size_plot)
    req(input$pd_palette)
     #req(input$rfbi_prob)
#isolate(saveRDS(reactiveValuesToList(vals),"vals.rds"))
#isolate(saveRDS(reactiveValuesToList(input),"input.rds"))
  #input<-readRDS("input.rds")
 # vals<-readRDS("vals.rds")
    m<-vals$rf_results
    var1=picvars_pd()$var1
    var2=picvars_pd()$var2
    if(vals$rf_results$modelType=="Classification"){
      req(input$rf_biplotclass)
      pd <- pdp::partial(m, pred.var = c(var1,var2), type="classification",which.class=input$rf_biplotclass,
                         grid.resolution=input$rfbi_grid,

                         prob=T,
                         chull=T)
      p1 <- autoplot(pd, contour = TRUE, main = paste("Class:",input$rf_biplotclass),legend.title = "Partial\ndependence (prob)")
    } else{
      pd<-pdp::partial(m, pred.var = c(var1,var2), type="regression",grid.resolution=input$rfbi_grid,
                       chull=T)


      p1 <- autoplot(pd, contour = TRUE, main = "",legend.title = "Partial\ndependence")}
    p1<- p1 + scale_fill_gradientn(colours =scale_color_2(input$pd_palette,vals$newcolhabs),name="Partial\ndependence (prob)")
    p1<-p1+theme_bw(base_size = input$pd_size_plot)
    p1<-p1+ ggtitle(input$title_pd)
    p1<-p1+labs(y = input$ylab_pd, x=input$xlab_pd)


    vals$rfbiplot1<-p1
    p1
  })
  output$rf_relations<-renderUI({

    validate(need(length(attr(vals$rf_results,"mindepth"))>0,"Use '+ Click to Measure Importance' in '2.3.1 Measures' panel before generating the plot  "))
    tabsetPanel(
      tabPanel(
        span("2.3.4.1. Between importances"),
        div(style="background: white",
            div(
              span(
                strong("Compare measures",tipify(icon("fas fa-question-circle"),"Plot selected importance measures pairwise against each other", options =list(container="body")), style="font-size: 16px")
              )
            ),
            uiOutput(ns("rf_rel"))

        )
      ),
      tabPanel(
        span("2.3.4.2. Between rankings"),
        div(style="background: white",
            div(
              span(

                strong("Compare rankings",tipify(icon("fas fa-question-circle"),"Plot against each other rankings of variables according to various measures of importance", options =list(container="body")), style="font-size: 16px")

              )
            ),
            uiOutput(ns("rf_rank"))
        )
      )
    )
  })
  output$rf_mindepth<-renderUI({
    validate(need(length(attr(vals$rf_results,"mindepth"))>0,"Use '+ Click to Measure Importance' in '2.3.1 Measures' panel before generating the plot  "))
    sidebarLayout(
      sidebarPanel(
        strong("Minimal depth distribution",tipify(icon("fas fa-question-circle"),"Plot the distribution of minimal depth in a random forest", options =list(container="body")), style="font-size: 16px"),
        uiOutput(ns("side_rf_mindeph"))),
      mainPanel(
        style="background: white",
        div(style="margin-top: 10px",
          plotOutput(ns("prf"))
        ))
    )
  })
  output$rf_measure<-renderUI({
    sidebarLayout(
      sidebarPanel(
        div(
          strong("Measures",tipify(icon("fas fa-question-circle"),"Creates a data frame with various measures of importance of variables in a random forest", placement="right"), style="font-size: 16px"),
          uiOutput(ns("rf_sidemeasure"))
        )
      ),
      mainPanel(style="background: white",
        div(style="margin-top: 15px",

            tags$style(
              paste(paste0("#",ns('rf_measure_out')),"td {
                    padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}")
            ),
            tags$style(
              paste0("#",ns('rf_measure_out')),"th {
                padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}"
            ),
          inline(
            DT::dataTableOutput(ns("rf_measure_out"))
          )
        )
      )
    )
  })
  output$rf_measure_out<-DT::renderDataTable(data.frame( attr(vals$rf_results,"mindepth")[[2]]),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  output$rf_sidemeasure<-renderUI({
    fluidRow(
      class="map_control_style",style="color: #05668D",
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The sample of trees on which conditional mean minimal depth is calculated", options =list(container="body")),'+ mean_sample'),
             inline(
               pickerInput(ns('rfmeasure_mean_sample'),NULL,choices=c("all_trees", "top_trees", "relevant_trees"),selected='top_trees', width="140px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle")," the measures of importance to be used", options =list(container="body")),'+ measures:'),
             p(
               fluidRow(
                 style="margin-left: 15px",
                 inline(checkboxGroupInput(ns('rf_measures'),NULL,choices=c(rf_inputsmeasure()),selected=rf_inputsmeasure(),width="140px"))

               )
             )
        )
      ),
      div(
        (actionLink(ns('go_rfplot'),span("+ Click to Measure Importance ",icon("fas fa-arrow-circle-right")), style = "animation: glowing3 1000ms infinite;"))
      ),
      if(length(attr(vals$rf_results,"mindepth"))>0){
        div(
          tipify(
            actionLink(
              ns('downcenter_rfdepth'),span("+ Download Results",icon("fas fa-table")), style="button_active"
            ),
            "Download Minimal Depth distribution results", options=list(container="body")
          )
        )
      }




    )
  })
  output$rftab2_3_5_1<-renderUI({
    validate(need(length(attr(vals$rf_results,"mindepth"))>0,"Use '+ Click to Measure Importance' in '2.3.1 Measures' panel before generating the plot  "))
    sidebarLayout(
      sidebarPanel(

       div(
         span(
           strong("Variable interactions",tipify(icon("fas fa-question-circle"),"Investigate interactions with respect to the  set of most important variables", options =list(container="body")), style="font-size: 16px"),

           actionLink(ns('go_rfinter'),span("+ Click to Run ",icon("fas fa-arrow-circle-right")), style = "animation: glowing3 1000ms infinite;")
         )
       ),

        uiOutput(ns("rf_sideinter"))

      ),
      mainPanel(
        style="background: white",
        div(style="margin-top: 10px",
            tabsetPanel(
              id=ns('rf_intermode'),
              tabPanel("Plot",value="Plot",
                       plotOutput(ns("rf_inter_out"))),
              tabPanel("Table",value="Table",
                       fluidRow(
                         tags$style(
                           paste(paste0("#",ns('rf_inter_tab')),"td {
                    padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}")
                         ),
                    tags$style(
                      paste0("#",ns('rf_inter_tab')),"th {
                padding-left: 4px;
                    padding-right: 4px;
                    padding-top: 2px;
                    padding-bottom: 2px;
                     text-align: left;
                     font-size:12px}"
                    ),
                inline(
                  DT::dataTableOutput(ns("rf_inter_tab"))
                ))
              )
            )
        )
      )
    )
  })
  observeEvent(ignoreInit = T,input$inter_size_plot,{
    vals$inter_size_plot<-input$inter_size_plot
  })
  observeEvent(ignoreInit = T,input$inter_palette,{
    vals$inter_palette<-input$inter_palette
  })
  observeEvent(ignoreInit = T,input$inter_labang,{
    vals$inter_labang<-input$inter_labang
  })
  output$inter_plot_options<-renderUI({
    if(is.null(vals$inter_labang)){vals$inter_labang<-c(-90)}
    if(is.null(vals$inter_size_plot)){vals$inter_size_plot<-10}
    if(is.null(vals$inter_palette)){vals$inter_palette<-vals$colors_img$val[2]}
    div(


      div(
        span("+ Palette:",
             inline(
               pickerInput(inputId = ns("inter_palette"),
                           label = NULL,
                           selected=vals$inter_palette,
                           choices =vals$colors_img$val[getgrad_col()],
                           choicesOpt = list(
                             content =vals$colors_img$img[getgrad_col()]),
                           options=list(container="body"),
                           width='100px')
             )
        )
      ),
      div(
        span("+ Size:",
             inline(
               numericInput(ns("inter_size_plot"),NULL, value=vals$inter_size_plot, width="75px")
             )
        )
      ),
      div(
        span("+ Label rotation (x):",
             inline(
               numericInput(ns("inter_labang"),NULL, value=vals$inter_labang, width="75px")
             )
        )
      )
    )
  })

  observeEvent(ignoreInit = T,input$go_rfinter,{
    #req(is.null(attr(vals$rf_results,"interframe")))
    req(input$rfinter_k)
    req(input$rfinter_mean_sample)
    req(input$uncond_mean_sample)
    req(input$rfinter_k2)
    req(length(attr(vals$rf_results,"mindepth"))>0)
    withProgress(message = "Running ...",
                 min = 1,
                 max = 1,
                 {
                   importance_frame<- attr(vals$rf_results,"mindepth")[[2]]
                   vars <- important_variables(importance_frame, k = input$rfinter_k, measures = c("mean_min_depth", "no_of_trees"),ties_action="draw")
                   if(class(vals$rf_results)[1]=="randomForest"){
                     forest=vals$rf_results } else {forest=vals$rf_results$finalModel}
                   interactions_frame <- suppressWarnings(min_depth_interactions(forest, vars, mean_sample=input$rfinter_mean_sample, uncond_mean_sample=input$uncond_mean_sample))
                   vals$rf_interactions_frame
                   res<-interactions_frame[ order(interactions_frame$occurrences,decreasing=T),]

                   if(input$rf_models=="new rf (unsaved)"){
                     attr(vals$rf_results,"interframe")<-res
                   } else{
                     attr(attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]][[1]],"interframe")<-res
                   }
                   updateTabsetPanel(session,"rftab2","rftab2_3")
                   updateTabsetPanel(session,"rftab2_3","rftab2_3_5")
                 })
  })

  output$rf_inter_tab<-DT::renderDataTable(data.frame(attr(vals$rf_results,"interframe")),options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = F,class ='cell-border compact stripe')
  output$rf_inter_out<-renderPlot({
    req(!is.null(attr(vals$rf_results,"interframe")))
    req(input$rfinter_k2)
    k2<-input$rfinter_k2
    if(is.na(k2)){
      k2<-NULL
    }
    p<-plot_min_depth_interactions(attr(vals$rf_results,"interframe"),k=k2)+scale_fill_gradientn(colours =scale_color_2(input$inter_palette,vals$newcolhabs)) +
      theme_bw(base_size = input$inter_size_plot)+theme(axis.text.x=element_text(angle = input$inter_labang, hjust = 0))
    vals$rf_inter<-p
    p


  })







  output$rf_sideinter<-renderUI({
    div(
      class="map_control_style",style="color: #05668D",

      div(
        span(span(tipify(icon("fas fa-question-circle"),"The number of variables to extract", options =list(container="body")),'+ k'),
             inline(
               numericInput(ns('rfinter_k'),NULL,value=5,step=1, width="75px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The sample of trees on which conditional mean minimal depth is calculated", options =list(container="body")),'+ mean_sample'),
             inline(
               pickerInput(ns('rfinter_mean_sample'),NULL,choices=c("all_trees", "top_trees", "relevant_trees"),selected='top_trees', width="100px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The sample of trees on which unconditional mean minimal depth is calculated", options =list(container="body")),'+ unc_mean_sample'),
             inline(
               pickerInput(ns('uncond_mean_sample'),NULL,choices=c("all_trees", "top_trees", "relevant_trees"),selected='mean_sample', width="100px")
             )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The number of best interactions to plot, if empty then all plotted", options =list(container="body")),'+ N inter'),
             inline(
               numericInput(ns('rfinter_k2'),NULL,value=30,step=1, width="75px")
             )
        )
      ),
      uiOutput(ns("inter_plot_options")),
      uiOutput(ns("varinter_saves"))


    )
  })


  output$varinter_saves<-renderUI({
    #req(length(attr(vals$rf_results,"interframe"))>0)
    div(
      div(
        tipify(
          actionLink(
            ns('create_rfinter'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
          ),
          "Create a datalist with the variables included in the N most frequent interactions in the RF", options=list(container="body")
        )
      ),

      div(
        (actionLink(ns('rfinter_downp'),span("+ Download plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active"))
      ),
      div(
        tipify(
          actionLink(
            ns('downcenter_rfinter'),span("+ Download Interaction Results",icon("fas fa-table")), style="button_active"
          ),
          "Download Interaction frame", options=list(container="body")
        )
      )

    )

    })
  observeEvent(ignoreInit = T,input$rfinter_k2,
               vals$rfinter_k2<-input$rfinter_k2)
  output$rf_rank<-renderUI({
    div(
      renderPlot({


        vals$rf_rank<- plot_importance_rankings( attr(vals$rf_results,"mindepth")[[2]])
        vals$rf_rank


      })
    )
  })
  output$rf_rel<-renderUI({
    div(
      renderPlot({
        withProgress(message = "Running ...",
                     min = 1,
                     max = 1,
                     {
                       vals$rf_rel<-plot_importance_ggpairs( attr(vals$rf_results,"mindepth")[[2]])
                       vals$rf_rel

                     })

      })
    )
  })

  observeEvent(ignoreInit = T,input$sigprf,{
    vals$cur_sigprf<-input$sigprf})
  observeEvent(ignoreInit = T,input$rf_depth,{
    vals$cur_rf_depth<-input$rf_depth})

  observeEvent(ignoreInit = T,input$depth_min_no_of_trees,{
    vals$cur_depth_min_no_of_trees<-input$depth_min_no_of_trees})
  observeEvent(ignoreInit = T,input$size_mmd,{vals$size_mmd<-input$size_mmd})
  observeEvent(ignoreInit = T,input$labrfsize,{vals$labrfsize<-input$labrfsize})
  observeEvent(ignoreInit = T,input$ylab_prf,{vals$ylab_prf<-input$ylab_prf})
  observeEvent(ignoreInit = T,input$xlab_prf,{vals$xlab_prf<-input$xlab_prf})

  observeEvent(ignoreInit = T,input$depth_mean_sample,{
    vals$cur_depth_mean_sample<-input$depth_mean_sample})


  output$side_rf_mindeph<-renderUI({


    div(class="map_control_style2",style="color: #05668D",
             div(class="well3",
                 uiOutput(ns("ui_rf_depth")),
                 inline(uiOutput(ns("npicrf"))),
                 uiOutput(ns("ui_depth_min_no_of_trees")),
                 uiOutput(ns("ui_depth_mean_sample")),
                 uiOutput(ns("ui_sigprf")),
                 uiOutput(ns("ui_create_rf"))
             ),
             div(class="well3",
                 uiOutput(ns('rfe_title')),
                 uiOutput(ns("ui_xlab_prf")),
                 uiOutput(ns("ui_ylab_prf")),
                 uiOutput(ns("ui_mdd_palette")),
                 uiOutput(ns("ui_labrfsize")),
                 uiOutput(ns("ui_size_mmd")),
                 uiOutput(ns("ui_downp_prf"))
             )
    )


  })

  output$ui_rf_depth <- renderUI({
    div(
      span(
           inline(checkboxInput(ns("rf_depth"), "+ Only significant variables:", T))
      )
    )
  })

  output$npicrf <- renderUI({
    req (isFALSE(input$rf_depth))
    if(is.null(vals$cur_n_var_rf)){vals$cur_n_var_rf<-10}
    lab<-span(tipify(icon("fas fa-question-circle", style="color: gray"),"The maximal number of variables with lowest mean minimal depth to be used for plotting", options =list(container="body")),
              '+ Number of variables:')
  numericInput(ns("n_var_rf"),
               lab,
               value = vals$cur_n_var_rf,
               step = 1)



  })
  output$rfe_title<-renderUI({
    textInput(ns("title_prf"),"+ Title:", value=paste0(
      attr(vals$rf_results,"Y"),"~",attr(vals$rf_results,"Datalist")
    ))
  })

  output$ui_depth_min_no_of_trees <- renderUI({
    lab<-span(tipify(icon("fas fa-question-circle"),"The minimal number of trees in which a variable has to be used for splitting to be used for plotting", options =list(container="body") ),
              '+ Min_n_trees:')
    numericInput(ns('depth_min_no_of_trees'),lab,value=0,step=1)
  })
  output$ui_depth_mean_sample <- renderUI({
    lab<-span(tipify(icon("fas fa-question-circle"),"The sample of trees on which mean minimal depth is calculated", options =list(container="body")),'+ Mean_sample:')
    pickerInput(ns('depth_mean_sample'),lab,choices=c( "top_trees","all_trees","relevant_trees"), selected= "top_trees")


  })
  output$ui_sigprf <- renderUI({
    lab<-span(tipify(icon("fas fa-question-circle"),"Significance level", options =list(container="body")),'+ Sig:')
    numericInput(ns("sigprf"), lab, 0.05, step=.05, max=1, min=0)
  })
  output$ui_create_rf <- renderUI({
    div(
      tipify(
        actionLink(ns('create_rf'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"),
        "Create a datalist with the variables selected in the Random Forest Explainer.", options=list(container="body")
      )
    )
  })
  output$ui_xlab_prf <- renderUI({
    textInput(ns("xlab_prf"),"+ X label:", value="Variable")
  })
  output$ui_ylab_prf <- renderUI({
    textInput(ns("ylab_prf"),"+ Y label:", value="Number of Trees")
  })
  output$ui_mdd_palette <- renderUI({
    pickerInput(
      inputId = ns("mdd_palette"),
      label = "+ Palette:",
      selected=vals$cur_mdd_palette,
      choices =vals$colors_img$val[getgrad_col()],
      choicesOpt = list(content = vals$colors_img$img[getgrad_col()]), options=list(container="body")
    )
  })
  output$ui_labrfsize <- renderUI({
    numericInput(ns("labrfsize"),"+ Size:", value=10)
  })
  output$ui_size_mmd <- renderUI({
    numericInput(ns("size_mmd"),"+ Text-in size:", value=4)
  })
  output$ui_downp_prf <- renderUI({
    div(
      actionLink(ns('downp_prf'),span("+ Download Plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active")
    )
  })


  observeEvent(ignoreInit = T,input$mdd_palette,{
    vals$cur_mdd_palette<-input$mdd_palette
  })
  observeEvent(ignoreInit = T,input$n_var_rf,{
    vals$cur_n_var_rf<-input$n_var_rf})


  output$rf_mtry<-renderUI({
    data<-getdata_rfX()
    req(input$rf_search=='user-defined')
    div(
      div(
        tipify(icon("fas fa-question-circle"),"the number of variables randomly sampled as candidates at each split", options = list(container="body")),
        "+ mtry:",
        inline(
          numericInput(ns("mtry"), NULL, value = 2, width="100px", max=ncol(data))
        ),

        inline(actionButton(ns("mtry_include"),tipify(icon("fas fa-arrow-right"),"Include mtry in the grid search", options = list(container="body")))),

        inline(span(verbatimTextOutput(ns("mtry_grid")))),
        inline(actionButton(ns("remove_mtry"),tipify(icon("fas fa-eraser"),"restart mtry"), style="button_active"))
      )


    )



  })
  output$rf_grid_search<-renderUI({
    req(input$rf_search!='user-defined')
    div(
      tipify(icon("fas fa-question-circle"),"The maximum number of mtry- combinations that will be generated"),
      "+ tuneLength:",
      inline(
        numericInput(ns("tuneLength"), NULL, value = 5, width="82px")
      )
    )




  })
  output$mtry_grid<-renderPrint({vals$mtry_pool})




  output$rf_supervisor <- renderUI({
    req(input$data_rfY)
    req(input$rf_type)

    data <- vals$saved_data[[input$data_rfY]]
    labels <- attr(data,"factors")
    choices<-if(input$rf_type=="Classification"){rev(colnames(labels))} else{
      colnames(data)
    }

    div(well="class2",
        div("Variable"),
        div(tipify(pickerInput(
          ns("rf_sup"),
          NULL,
          choices =choices ,
          width="150px", selected=vals$cur_response
        ),"The response vector"))
    )

  })
  observeEvent(ignoreInit = T,input$rf_sup,{
    vals$cur_response<-input$rf_sup
  })
  output$data_rfX_out<-renderUI({
    if(is.null(vals$cur_data)){vals$cur_data<-1}
    div(
      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_rfX"),NULL,choices =names(vals$saved_data),width="150px", selected=vals$cur_data)
        )
      ),      inline(uiOutput(ns("saved_rfs")))

    )
  })
output$saved_rfs<-renderUI({

  req(input$data_rfX)
  names_rf<-names(attr(vals$saved_data[[input$data_rfX]],"rf"))
  req(length(names_rf)>0)
  pic<-which(names_rf%in%"new rf (unsaved)")
  if(length(pic)>0){
    names_rf<-names_rf[-pic]
  }
  req(length(names_rf)>0)
  div(class="saved_models",
      icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(length(names_rf)), "saved model(s)")
})

  observeEvent(ignoreInit = T,input$data_rfX,{
    vals$cur_data<-input$data_rfX
  })
  output$rf_test_part<-renderUI({
    req(input$data_rfX)

    div(
      div(span("Partition:",tiphelp("choose a factor as reference for the partition"))),
      div(pickerInput(ns("rf_test_partition"),NULL, choices=c("None", colnames(attr(vals$saved_data[[input$data_rfY]],"factors"))), width="150px", selected=vals$cur_test_partition))
    )
  })
  observeEvent(ignoreInit = T,input$rf_test_partition,{
    vals$cur_test_partition<-input$rf_test_partition
  })
  output$rf_partition<-renderUI({
    req(input$rf_test_partition!="None")
    req(input$data_rfY)

    fac<-attr(vals$saved_data[[input$data_rfY]],"factors")[,input$rf_test_partition]
    choices<-levels(fac)
    div(
      div("::",span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions"))),
      pickerInput(ns("testdata_rf"),NULL, choices=choices, width="200px", selected=vals$cur_testdata)
    )
  })
  observeEvent(ignoreInit = T,input$testdata_rf,{
    vals$cur_testdata<-input$testdata_rf
  })
  output$rf_results_menu<-renderUI({
    div(pickerInput(ns("rf_models"),strong("rf results:", tiphelp("Random Forest Results. Click to select rf results saved in the Training Datalist (X).")), choices= c(names(attr(vals$saved_data[[input$data_rfX]],"rf"))), width="300px", selected = vals$cur_rf_models)
    )
  })
  observeEvent(ignoreInit = T,input$rf_models,{
    vals$cur_rf_models<-input$rf_models
  })


  observeEvent(ignoreInit = T,input$data_rfY,{
    vals$cur_dataY<-input$data_rfY
  })



  output$saverf_button<-renderUI({
    req(input$rf_models=="new rf (unsaved)")
    div(style="margin-top: 20px",class="save_changes",
        tipify(actionButton(ns("tools_saverf"), icon("fas fa-save")), "Save the rf model in the training Datalist (X)")

    )
  })


  observeEvent(ignoreInit = T,input$teste,{
    savereac()
  })

  output$rmrf_button<-renderUI({
    req(input$rf_models!="new rf (unsaved)")
    div(style="margin-top: 20px",
        tipify(actionButton(ns("tools_rmrf"), icon("far fa-trash-alt")), "Remove the rf model from the training Datalist (X)")
    )

  })
  output$rf_errors<-renderUI({
    #saveRDS(reactiveValuesToList(vals),"vals.rds")
    #saveRDS(reactiveValuesToList(input),"input.rds")
    #vals<-readRDS('vals.rds')
    #input<-readRDS('input.rds')
    column(12,
           sidebarLayout(
             sidebarPanel(
               uiOutput(ns("RF_prederrors_side"))),
             mainPanel(
               div(
                 tags$style(
                   paste(paste0("#",ns('rf_tab_errors0_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
                 ),
                 tags$style(
                   paste0("#",ns('rf_tab_errors0_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
                 ),
                 tags$style(
                   paste(paste0("#",ns('rf_tab_errors_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
                 ),
                 tags$style(
                   paste0("#",ns('rf_tab_errors_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
                 ),
                 div(
                   p(strong("Global:")),
                   inline(DT::dataTableOutput(ns('rf_tab_errors0_pred')))
                 ),
                 hr(),
                 div(
                   p(strong("Observations (Top trees):")),
                   inline(DT::dataTableOutput(ns('rf_tab_errors_pred')))
                 )
               )
             )
           )


    )
  })





  output$rf_tab_errors0_pred<-DT::renderDataTable({
    req(input$rf_round_3_2)

    vals$rf_preddatalist<-table<-round(pred_test_rf(),input$rf_round_3_2)
        DT::datatable(table,
                  options=list(rownames=T,info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'),colnames=c(""))
  })

  output$rf_tab_errors_pred<-DT::renderDataTable({
    req(input$rf_round_3_2)

    table<-round(rf_prederrors(),input$rf_round_3_2)
    rownames(table)<-rownames(pred_rf())
    vals$rf_prederrors<-table
    req(length(table)>0)
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'))


  })


  observeEvent(ignoreInit = T,input$predrf_newY,{
    vals$predrf_newY<-input$predrf_newY
  })
  output$predrf_newY_out<-renderUI({
    req(input$rfpred_which=='Datalist')
    sup_test<-attr(vals$rf_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    lab<-span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),)
    pickerInput(ns("predrf_newY"),
                lab,names(vals$saved_data[getobsRF()]),selected=vals$predrf_newY
    )
  })
  observeEvent(ignoreInit = T,input$predrf_newY_tree,
               vals$predrf_newY_tree<-input$predrf_newY_tree)
  output$RF_prederrors_side<-renderUI({
   lab2<-"+ Number of trees:"
   lab2<-tiphelp3(lab2,"Selects the N top trees with best R squared")


    fluidRow( class="map_control_style2",style="color: #05668D",
              uiOutput(ns("predrf_newY_out")),


              numericInput(ns("rf_round_3_2"),"+ Round:",value=3),

              numericInput(ns("q_class_toptrees2"),lab2, value=vals$q_class_toptrees ),

              div(
                tipify(
                  actionLink(
                    ns('down_rf_errors'),span("+ Download",icon("fas fa-download")), style="button_active"
                  ),
                  "Download error results",options=list(container="body")
                )

              ),
              div(
                actionLink(
                  ns('down_rf_preddatalist'),span("+ Download Global Stats",icon("fas fa-download")), style="button_active"
                )

              ),

              div(
                tipify(
                  actionLink(
                    ns('create_rfreg'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                  ),
                  "Create a datalist with the prediction errors", options=list(container="body")
                )
              )
    )

  })



  observeEvent(ignoreInit = T,input$down_rf_preddatalist,{
    vals$hand_down<-"Global Stats - RF pred Datalist"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })

  observeEvent(ignoreInit = T,input$rf_reftest,{
    vals$rf_reftest<-input$rf_reftest
  })
  rf_prederrors<-reactive({

    pred<-predall_rf()
    obs<-RF_observed()
    m<-vals$rf_results
    vals$tree_rs<-data.frame(do.call(rbind,lapply(data.frame(pred$individual),function(x)
      postResample(x,obs))))
    if(m$modelType=="Regression"){
      pic<-order(vals$tree_rs$Rsquared, decreasing=T)[1:input$q_class_toptrees2]
    } else{
      pic<-order(vals$tree_rs$Accuracy, decreasing=T)[1:input$q_class_toptrees2]
    }

    pred$individual<-data.frame(pred$individual[,pic])
    rownames(pred$individual)<-rownames(test_rf())
    req(length(pred)>0)
    req(length(obs)>0)


    temp<-switch(vals$rf_results$modelType,
                 "Regression"=RFerror_reg(pred,obs),
                 "Classification" =data.frame(RFerror_class(pred,obs)))
    temp
  })


  output$RF_prederrors<-renderUI({
    div(
      if(input$rf_type=='Classification'){
        div(
          renderPrint({
            vals$rf_prederrors<-rf_prederrors()
            vals$rf_prederrors

          })
        )
      } else{div(

        div("rmse:", em("Root Mean square error")),
        div("mse:", em("Mean square error")),
        div("mae:", em("Mean absolute error")),
        div("mape:", em("Mean absolute percentage error")),
        renderPrint({
          vals$rf_prederrors<-rf_prederrors()
          vals$rf_prederrors

        })

      )}

    )
  })

  output$pick_rfobs_pred<-renderUI({
    req(input$rfpred_which=="Datalist")
    sup_test<-attr(vals$rf_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("obs_cm_pred"),
                       NULL,names(vals$saved_data[getobsRF()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })
  observeEvent(ignoreInit = T,input$obs_cm_pred,{
    vals$obs_cm_pred<-input$obs_cm_pred
  })



  output$cm_suprf_palette<-renderUI({
    div(
      "+ Palette",
      inline(pickerInput(inputId = ns("cm_suprf_palette"),
                         label = NULL,
                         choices =     vals$colors_img$val[getgrad_col()],
                         choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"), width = "120px"))
    )
  })

  output$rf_metrics<-renderUI({
    if(is.null(vals$cm_test_title)){vals$cm_test_title<- "Confusion Matrix"}

    validate( need(vals$rf_results$modelType=="Classification","Confusion matrices are only valid for classification models"))

sidebarLayout(
  sidebarPanel(
    div(class="map_control_style",style="color: #05668D",
        uiOutput(ns('pick_rfobs_pred')),
        uiOutput(ns('cm_suprf_palette')),
      br(),
      uiOutput(ns('cm_test_title')),
      div(
        actionLink(ns("downp_confrf"), span("+ Download plot",icon("fas fa-download")),style  = "button_active")
      ),
      div(actionLink(ns("dowcenter_cmTest_rf"), span("+ Download Confusion Matrix"),style  = "button_active")),
      div(actionLink(ns("dc_cmTest_byclass"), span("+ Download Statistics by Class"),style  = "button_active")),

      div(actionLink(ns("dc_cmTest_overall"), span("+ Download Overall statistics"),style  = "button_active"))



    )
  ),
  mainPanel(uiOutput(ns("conf_rf")))

)
  })

  output$cm_test_title<-renderUI({
    div(
      span("+ Title:",
           inline(
             textInput(ns("cm_test_title"),NULL, value="Test", width="200px")
           )
      )
    )
  })


  output$rf_tab3_1<-renderUI({


    sidebarLayout(
      sidebarPanel(
        fluidRow(class="map_control_style",style="color: #05668D",
                 div(
                   tipify(
                     actionLink(
                       ns('rf_create_predictions'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                     ),
                     "Create a datalist with the RF predictions"
                   )
                 ),
                 div(
                   tipify(
                     actionLink(
                       ns('down_rf_tab3_1'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                     ),
                     "Download Table", options=list(container="body")
                   )
                 )

        )
      ),
      mainPanel(
        tags$style(
          paste(paste0("#",ns('rftab_pred')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
        ),
        tags$style(
          paste0("#",ns('rftab_pred')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
        ),
        inline(DT::dataTableOutput(ns('rftab_pred')))


      )
    )


  })

  output$rftab_pred<-DT::renderDataTable({
    table<-vals$rftab_pred<-pred_rf()
    DT::datatable(table,options=list(pageLength = 20,info = FALSE,lengthMenu = list(c(20,-1),c( "20","All")),autoWidth=T,dom = 'lt'))

  },rownames = TRUE,class ='compact cell-border')




  observeEvent(ignoreInit = T,input$predrf_new,{
    vals$predrf_new<-input$predrf_new
  })

  observeEvent(ignoreInit = T,input$rfpred_which,{
    vals$rfpred_which<-input$rfpred_which
  })

  get_rf_prederrors<-reactive({
    req(length(input$rfpred_which)>0)

    m<-vals$rf_results
    pred<-pred_rf()[,1]
    # x_t<-attr(m,"test")
    obs<-if(input$rfpred_which=="Partition"){
      attr(m,"sup_test")} else{
        req(length(input$predrf_newY)>0)

        if(m$modelType=="Classification"){
          req(length(attr(m,"supervisor"))>0)
          req(length(input$predrf_newY)>0)
          req(input$predrf_newY%in%names(vals$saved_data))
          sup<-attr(m,"supervisor")
          factors<-attr(vals$saved_data[[input$predrf_newY]],"factors")
          req(sup%in%colnames(factors))
          factors[,sup]} else{
            vals$saved_data[[input$predrf_newY]][,attr(m,"supervisor")]
          }
      }
    return(
      list(
        obs=obs,
        pred=pred
      )
    )
  })

  pred_test_rf<-reactive({
    obs<-get_rf_prederrors()$obs
    pred<-get_rf_prederrors()$pred
    m<-vals$rf_results

    table<-stats_ml(obs,pred,m)


    table
  })
  getnewdatarf<-reactive({
    datalist<-vals$saved_data
    datalist_comp<-lapply(
      vals$saved_data, function (x) {
        colnames(x)<-gsub(" ",".", colnames(x))
        x
      }
    )
    datalist_comp<-vals$saved_data
    m<-vals$rf_results
    Y<-which(colnames(m$trainingData)==".outcome")
    res0<-unlist(
      lapply(datalist_comp, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-Y])
        sum(res)==ncol(m$trainingData[-Y])
      })
    )


    names(datalist[res0==T])
  })
  predall_rf<-reactive({
    validate(need(!anyNA(test_rf()),"NAs not allowed in the prediction Datalist"))
    m<-vals$rf_results
    #factors<-attr(vals$saved_data[[input$predrf_new]],"factors")
    pred_tab=test_rf()
    pred <- predict(m$finalModel,newdata = pred_tab, predict.all=T)
    pred
  })
  test_rf<-reactive({
    req(input$rfpred_which)
    newdata<-if(input$rfpred_which=="Partition"){attr(vals$rf_results,"test")} else if(input$rfpred_which=="Datalist"){
      req(input$predrf_new)
      newdata<-vals$saved_data[[input$predrf_new]]
    }
    model_data<-newdata
    vals$rf_newdata<-newdata
    newdata

  })

  pred_rf<-reactive({
    validate(need(!anyNA(test_rf()),"NAs not allowed in the prediction Datalist"))
    m<-vals$rf_results
    newdata<- test_rf()
    rf_pred <- predict(m$finalModel,newdata =newdata)
    res<-data.frame(Predictions= rf_pred)
    rownames(res)<-rownames(test_rf())
    colnames(res)<-attr(vals$rf_results,"supervisor")
    pred_rf<-res
    vals$rf_predtab<-res
    # attr(res,"obs")<-test_rf()
    res

  })

  RF_observed<-reactive({
    req(input$rfpred_which)
    if(vals$rf_results$modelType=="Regression"){
      observed<-if(input$rfpred_which=="Datalist"){
        req(input$predrf_newY)
        newdata=vals$saved_data[[input$predrf_new]]
        factors<-vals$saved_data[[input$predrf_newY]][rownames(newdata),, drop=F]
        factors[,attr(vals$rf_results,"supervisor")]
      } else{
        attr(vals$rf_results,"sup_test")
      }
    } else{
      observed<-if(input$rfpred_which=="Datalist"){
        req(input$predrf_newY)
        factors<-attr(vals$saved_data[[input$predrf_newY]],"factors")
        newdata=vals$saved_data[[input$predrf_new]]
        factors<-factors[rownames(newdata),, drop=F]
        factors[,attr(vals$rf_results,"supervisor")]
      } else{
        attr(vals$rf_results,"sup_test")
      }
    }
    observed

  })
  output$conf_rf<-renderUI({
    pred_rf<-unlist(pred_rf())
    obs<-unlist(RF_observed())
    conf<-table(obs, pred_rf)
    res<-confusionMatrix(pred_rf,obs)
    vals$rf_test_stat_byclass<-res$byClass
    vals$rf_test_stat_overall<-res$overall
    vals$rf_test_cm<-as.data.frame.matrix(res$table)

    #palette<-readRDS("palette.rds")

    column(12,

           renderPlot({
             res<-plotCM(conf/sum(conf)*100, input$cm_suprf_palette,  newcolhabs=vals$newcolhabs, title=input$cm_test_title)
             vals$conf_rf<-res
             res
           }),


           renderPrint(
             confusionMatrix(conf)

           ))

  })

  output$rf_sidemulti<-renderUI({
    if(is.null(vals$multi_x_measure)){vals$multi_x_measure<-"mean_min_depth"}
    if(is.null(vals$multi_y_measure)){vals$multi_y_measure<-"times_a_root"}
    if(is.null(vals$multi_z_measure)){vals$multi_z_measure<-"accuracy_decrease"}
    fluidRow(
      class="map_control_style",style="color: #05668D",
      div(
        span("+",
             inline(checkboxInput(ns("rf_sigmulti"), "Only significant variables", T))
        )
      ),
      inline(
        div(
          span(
            inline(uiOutput(ns("multi_no_labels")))
          )
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The measure of importance to be shown on the X axis", options =list(container="body")),'+ X:'),
             inline(pickerInput(ns('multi_x_measure'),NULL,choices=c('mean_min_depth','times_a_root','no_of_trees','no_of_nodes','accuracy_decrease','gini_decrease'), selected=vals$multi_x_measure,width="140px"))
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"The measure of importance to be shown on the Y axis", options =list(container="body")),'+ Y:'),
             inline(pickerInput(ns('multi_y_measure'),NULL,choices=c('mean_min_depth','times_a_root','no_of_trees','no_of_nodes','accuracy_decrease','gini_decrease'), selected=vals$multi_y_measure,width="140px"))
        )
      ),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"Optional measure for gradient color scale", options =list(container="body")),'+ z:'),
             inline(pickerInput(ns('multi_z_measure'),NULL,choices=c("None",'mean_min_depth','times_a_root','no_of_trees','no_of_nodes','accuracy_decrease','gini_decrease'), selected=vals$multi_z_measure,width="140px"))
        )
      ),
      uiOutput(ns("interframe_palette")),
      div(
        span(span(tipify(icon("fas fa-question-circle"),"Exclude text labels that overlap too many things", options =list(container="body")),'+ max.overlaps:'),
             inline(numericInput(ns('max.overlaps'),NULL,10, width="80px"))
        )
      ),

      div(
        span(span(tipify(icon("fas fa-question-circle"),"Significance level", options =list(container="body")),'+ Sig:'),
             inline(numericInput(ns("sigmrf"), NULL, 0.05, width="75px"))
        )
      ),
      div(
        span("+ Title:",
             inline(
               textInput(ns("interframe_title"),NULL, value="Multi-way importance", width="200px")
             )
        )
      ),
      #div(uiOutput(ns("interframe_xlims"))),

      div(
        checkboxInput(ns("interframe_logy"),"+ Log Y axis:", T, width="100px")
      ),
      div(
        checkboxInput(ns("interframe_logx"),"+ Log X axis:", F, width="100px")
      ),
      div(
        span("+ Point size:",
             inline(
               numericInput(ns("interframe_size"),NULL, value=2, width="75px")
             )
        )
      ),
      div(
        span("+ label size:",
             inline(
               numericInput(ns("interframe_label_size"),NULL, value=5, width="75px")
             )
        )
      ),
      div(
        span("+ Axis size:",
             inline(
               numericInput(ns("interframe_axis_size"),NULL, value=12, width="75px")
             )
        )
      ),
      div(
        (actionLink(ns('downp_mrf'),span("+ Download plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active"))
      )

    )
  })

  output$interframe_xlims<-renderUI({
    req(input$multi_x_measure)
    req(input$multi_y_measure)
    frame<- attr(vals$rf_results,"mindepth")[[2]]
    x = input$multi_x_measure
    y = input$multi_y_measure
    xlim<-round(range(frame[,x], na.rm=T),3)
    ylim<-round(range(frame[,y], na.rm=T),3)
    div(

      div(inline(div("+ Limits:",style="color: #05668D;width:55px")),inline(div("X",style="width:80px")),inline(div("Y",style="width:80px"))),
      div(
        splitLayout("+ min:",cellWidths = c("50px",'80px',"80px"),
                    numericInput(ns("interframe_xmin"),NULL, value= xlim[1], width="80px", step=0.01),
                    numericInput(ns("interframe_ymin"),NULL,  ylim[1], width="80px", step=0.01))),
      div(
        splitLayout("+ max:",cellWidths = c("50px",'80px',"80px"),
                    numericInput(ns("interframe_xmax"),NULL, value= xlim[2], width="80px", step=0.01),
                    numericInput(ns("interframe_ymax"),NULL,  ylim[2], width="80px", step=0.01)))



    )
  })


  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(10))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic
  })
  output$interframe_palette<-renderUI({
    req(input$multi_z_measure)
    if(input$multi_z_measure=="None"){
      pic<-getsolid_col()
    } else{
      pic<-getgrad_col()
    }
    div(
      span("+ Palette:",
           inline(
             pickerInput(
               inputId = ns("interframe_palette"),
               label = NULL,
               selected=vals$cur_mdd_palette,
               choices =vals$colors_img$val[pic],
               choicesOpt = list(content =     vals$colors_img$img[pic]), options=list(container="body"),width='100px'
             )
           )
      )
    )
  })
  output$rf_multi<-renderUI({
    validate(need(length(attr(vals$rf_results,"mindepth"))>0,"Use '+ Click to Measure Importance' in '2.3.1 Measures' panel before generating the plot  "))
    sidebarLayout(
      sidebarPanel(
        strong("Multi-way importance",tipify(icon("fas fa-question-circle"),"Plot two measures of importance of variables in a random fores", options =list(container="body")), style="font-size: 16px"),
        uiOutput(ns("rf_sidemulti"))),
      mainPanel(
        style="background: white",
        div(style="margin-top: 10px",plotOutput(ns("rf_multi_out")))
        )
    )
  })
  output$rf_multi_out<-renderPlot({
    rf_multi()
  })
  output$multi_no_labels<- renderUI({
    req(isFALSE(input$rf_sigmulti))
    div(
      span(span(tipify(icon("fas fa-question-circle"),"The approximate number of best variables (according to all measures plotted) to be labeled (more will be labeled in case of ties)", options =list(container="body")),'+ No_of_labels'),
           inline(
             numericInput(ns('multi_no_of_labels'),NULL,value=10,step=1, width="75px")
           )
      )
    )

  })
  output$rf_tab_errors0_train<-DT::renderDataTable({
    m<-vals$rf_results
    table<- m$results[rownames(m$bestTune),]
    rownames(table)<-"Optimal model"
    table<-round(table,input$rf_round_2_2)
    DT::datatable(table, options=list(
      rownames=T,
      info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

  })
  output$rf_table <- renderUI({

    div(
      tags$style(
        paste(paste0("#",ns('rf_tab_errors0_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
      ),
      tags$style(
        paste0("#",ns('rf_tab_errors0_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
      ),
      tags$style(
        paste(paste0("#",ns('rf_tab_errors_train')),"td {padding: 3px;
                     text-align: left;
                     font-size:12px}")
      ),
      tags$style(
        paste0("#",ns('rf_tab_errors_train')),"th {padding: 3px;
                     text-align: left;
                     font-size:12px}"
      ),
      div(
        sidebarLayout(
          sidebarPanel( width = 3,
            fluidRow( class="map_control_style",style="color: #05668D",
                      div(
                        span(
                          "+ Round:",
                          numericInput(ns("rf_round_2_2"),NULL,value=3,width="75px")
                        )
                      ),
                      div(
                        tipify(
                          actionLink(
                            ns('downcenter_intererror'),span("+ Download Obs. Errors",icon("fas fa-download")), style="button_active"
                          ),
                          "Download error results",options=list(container="body")
                        )

                      ),
                      div(
                        tipify(
                          actionLink(
                            ns('create_rfreg_errorinter'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                          ),
                          "Create a datalist with the observation errors", options=list(container="body")
                        )
                      )
            )
          ),
          mainPanel( width = 9,style="background: white",
            div(style="margin-top: 5px",
              div(
                p(strong("Global:")),
                inline(DT::dataTableOutput(ns('rf_tab_errors0_train')))
              ),
              hr(),
              div(
                p(strong("Observations:")),
                inline(DT::dataTableOutput(ns('rf_tab_errors_train')))
              )


            ))
        )
      )
    )
  })
  output$rf_tab_errors_train<-DT::renderDataTable({
    m<-vals$rf_results
    table<-if(vals$rf_results$modelType=="Regression"){
      accu_rf_reg_model(m)} else{accu_rf_class(m)}
    vals$rf_down_errors_train<-table
    table<-round(table,input$rf_round_2_2)
    table
    DT::datatable(table, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')

  })



  output$rfsummary <- renderUI({
    req(length(vals$rf_results)>0)
    div(
      inline(
        div(class="well3",
           div(style="margin: 5px",
             div(strong('Training data (X):'),em(attr(vals$rf_results,"Datalist"))),
             div(strong('Independent variable (Y):'),em(attr(vals$rf_results,"Y"))),
             div(strong('Partition:'),em(attr(vals$rf_results,"test_partition")))



           )
           )
      ),
      inline(
        div(popify(downloadButton(ns("down_rf_results"), span(icon("fas fa-tree"),icon("fas fa-tree", style = "margin-left: -10px;"),icon("fas fa-tree", style = "margin-left: -10px;")),style = "button_active"),NULL,"download rf results as rds file"))
      )


      )
  })
  output$down_rf_results <- {
    downloadHandler(
      filename = function() {
        paste0("rf","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$rf_results,file)
      })
  }
  output$prf <- renderPlot({
    p<-prf.reactive()
    vals$rfd_res<-p
    p
  })



  get_parts_rf<-reactive({
    req(input$data_rfY)
    req(input$testdata_rf)
    req(input$rf_test_partition)
    data<-getdata_rfX()
    factors<-attr(vals$saved_data[[input$data_rfY]],"factors")[rownames(data),]
    if(input$rf_test_partition!='None'){
      lis<-split(rownames(factors),factors[,input$rf_test_partition])
      names(lis[[1]])<-lis[[1]]
      names(lis[[2]])<-lis[[2]]
      test_i<-which(names(lis)==input$testdata_rf)
      tra_i<-which(names(lis)!=input$testdata_rf)
      parts=list(train=lis[[tra_i]],test=lis[[test_i]])} else{
        parts=list(train=NULL,test=NULL)
      }

    parts

  })

  getdata_rfX <- reactive({
    req(input$data_rfX)
    envi<-envi_o<-data<-vals$saved_data[[input$data_rfX]]
    data
  })
  get_supervisor <- reactive({
    req(input$data_rfX)
    req(input$rf_type)
    req(input$data_rfY)
    if (input$rf_type == 'Classification') {
      data <- vals$saved_data[[input$data_rfY]]
      labels <- attr(data,"factors")[rownames(vals$saved_data[[input$data_rfX]]),,drop=F]
      re<-labels[input$rf_sup]
    } else {

      data <- vals$saved_data[[input$data_rfY]][rownames(vals$saved_data[[input$data_rfX]]),,drop=F]
      re<-data[input$rf_sup]
    }
    sup<-sup_o<-re
    re
  })

  trainrf<-reactive({


    req(input$rf_type)
    t<-try({
      envi<-envi_o<-data.frame(getdata_rfX())
      sup<-sup_o<-get_supervisor()

      if(input$rf_test_partition!="None"){
        parts<-get_parts_rf()
        train<-parts$train
        test<-parts$test
        envi<-data.frame(envi_o[names(train),])
        sup<-sup_o[names(train),, drop=F]
      }


      if (input$rf_type == 'Classification') {
        sup[1] <- as.factor(sup[, 1])
      }
      seed<-if (!is.na(input$seedrf)) { input$seedrf} else{
        NULL
      }



      join <- na.omit(cbind(sup, envi[rownames(sup), ,drop=F]))
      envi <- join[-1]
      somC <- join[1]
      if(input$rf_search=="user-defined"){
        rf_search<-"grid"
        validate(need(length(vals$mtry_pool)>0,"Requires at least one mtry value"))
        tuneGrid=data.frame(mtry=vals$mtry_pool)
      } else{
        rf_search<-input$rf_search
        tuneGrid=NULL
      }
      withProgress(message = "Running Random Forest.... the time taken will depend on the model tuning",
                   min = 1,
                   max = 1,
                   {
                     RF = wrapRF(
                       envi,
                       data.frame(somC),
                       supervisor = "clusters",
                       prev.idw = F,
                       seed = seed,
                       ntree = input$ntree,

                       trainControl.args = list(
                         method = input$res_method,
                         number = input$cvrf,
                         repeats = input$repeatsrf,
                         p=input$pleaverf/100,
                         savePredictions = "final",
                         search=rf_search
                       ),
                       tuneLength=input$tuneLength,
                       tuneGrid=tuneGrid
                     )
                     attr(RF,"test_partition")<-paste("Test data:",input$rf_test_partition,"::",input$testdata_rf)
                     attr(RF,"Y")<-paste(input$data_rfY,"::",input$rf_sup)
                     attr(RF,"Datalist")<-paste(input$data_rfX)

                     if(input$rf_test_partition!="None"){
                       attr(RF,'test')<-data.frame(envi_o[names(test),])
                       sup_test<-sup_o[names(test),]
                       names(sup_test)<-names(test)
                       attr(RF,"sup_test")<-sup_test
                     } else{ attr(RF,'test')<-c("None")}

                     attr(RF,"supervisor")<-input$rf_sup
                     vals$cur_rftab2<-"rftab2_1"
                     updateTabsetPanel(session,"rf_tab","rf_tab2")
                     updateTabsetPanel(session,"rftab2","rftab2_1")
                     vals$rf_unsaved<-RF

                   })
      attr(vals$saved_data[[input$data_rfX]],"rf")[['new rf (unsaved)']]<-vals$rf_unsaved
      vals$cur_rf_models<-"new rf (unsaved)"

      beep(10)

    })

    if("try-error" %in% class(t)){
      output$rf_war<-renderUI({
        column(12,em(style="color: gray",
                     "Error in training the RF model. Check if the number of observations in X and Y are compatible"
        ))
      })

    } else{
      output$rf_war<-NULL
    }

  })

  observeEvent(ignoreInit = T,input$trainRF,{
    trainrf()
  })






  observe({
    req(!input$trainRF %% 2)
    req(input$rf_search=='user-defined')
    if(input$rf_search=="user-defined" & !length(vals$mtry_pool)>0) {
      output$rf_war<-renderUI({
        column(12,em(style="color: gray",
                     "Requires at least one mtry value when 'search' is 'user-defined'"
        ))
      })
    } else {
      output$rf_war<-renderUI({
        NULL
      })
    }
  })
  observeEvent(ignoreInit = T,input$go_rfplot,{
    try({

      if(input$rf_models=="new rf (unsaved)"){
        attr(vals$rf_results,"mindepth")<-mindeaphrf()
      } else{
        attr(attr(vals$saved_data[[input$data_rfX]],"rf")[[input$rf_models]][[1]],"mindepth")<-mindeaphrf()
      }
      updateTabsetPanel(session,"rftab2","rftab2_3")

    })
  })
  observeEvent(ignoreInit = T,input$rf_type,{
    vals$cur_model_type<-input$rf_type
  })


  observeEvent(ignoreInit = T,input$rf_sup,{vals$cur_rfsup<-input$rf_sup})

  observeEvent(ignoreInit = T,input$rf_test_partition,{vals$cur_partrf<-input$rf_test_partition
  })
  observeEvent(ignoreInit = T,input$data_rfX,{
    vals$cur_data=input$data_rfX
  })
  observeEvent(ignoreInit = T,input$rf_tab,{
    vals$currftab<-switch(input$rf_tab,
                          "rf_tab1"="rf_tab1",
                          "rf_tab2"="rf_tab2",
                          "rf_tab3"="rf_tab3")
  })
  observeEvent(ignoreInit = T,input$mtry_include,{
    vals$mtry_pool<-c(vals$mtry_pool,input$mtry)
  })
  observeEvent(ignoreInit = T,input$remove_mtry,{
    vals$mtry_pool<-c()
  })
  observeEvent(ignoreInit = T,input$rf_type,{
    updateTabsetPanel(session,"rf_tab", selected="rf_tab1")
  })

  observeEvent(ignoreInit = T,input$testdata_rf,{
    updateTabsetPanel(session,"rf_tab",'rf_tab1')
  })
  observeEvent(ignoreInit = T,input$tools_rmrf,{
    attr(vals$saved_data[[input$data_rfX]],"rf")[input$rf_models]<-NULL
  })
  observeEvent(ignoreInit = T,input$tools_saverf,{
    #isolate(saveRDS(reactiveValuesToList(vals),"vals.rds"))
    #isolate(saveRDS(reactiveValuesToList(input),"input.rds"))
    req(input$rf_models=="new rf (unsaved)")
    vals$hand_save<-"Save RF model in"
    vals$hand_save2<-column(12,fluidRow(em(input$data_rfX, style="color:gray"),strong("::"), em("RF-Attribute", style="color:gray"),strong("::")
    ))
    vals$hand_save3<-NULL

    showModal(module_rf())
  })

### Forest prediction

  output$r_forests<-renderUI({
    m<-vals$rf_results
    req(m$modelType=="Regression")
   req(length(vals$tree_rs$Rsquared)>0)
   div(
      renderPlot({
        hist(vals$tree_rs$Rsquared, main="R squares of the forest", xlab="R-squared value", border=NA, col='SeaGreen')

      })
    )
  })

  RF_observed2<-reactive({
    req(input$rfpred_which)


    obs_data<-if(input$rfpred_which=="Datalist"){
      newdata=vals$saved_data[[input$predrf_new]]
      if(vals$rf_results$modelType=="Classification"){
        factors<-attr(vals$saved_data[[input$predrf_newY_tree]],'factors')[rownames(newdata),, drop=F]

      } else {
        factors<-vals$saved_data[[input$predrf_newY_tree]][rownames(newdata),, drop=F]

        }

      res<-factors[,attr(vals$rf_results,"supervisor")]
      names(res)<-rownames(factors[attr(vals$rf_results,"supervisor")])
      res
    } else{
      attr(vals$rf_results,"sup_test")
    }
    obs_data
  })

get_rtrees<-reactive({
  pred<-predall_rf()
  obs_data<-RF_observed2()
  vals$tree_rs<-data.frame(do.call(rbind,lapply(data.frame(pred),function(x)
    postResample(x,obs_data))))
})

observe({


})


output$summ_trees<-renderPlot({
  req(input$predrf_newY_tree)
  req(input$rfpred_which)
  m<-vals$rf_results
  #req(m$modelType=="Regression")
  pred<-predall_rf()
  pred <- pred$individual
  model_data<-test_rf()
  obs_data<-RF_observed2()
  #pred<-readRDS("pred.rds")
  # obs_data<-readRDS("obs_data.rds")
  # input<-readRDS('input.rds')
  # vals<-readRDS("vals.rds")
  req(!is.null(vals$tree_rs))
  req(input$q_class_toptrees)

  if(m$modelType=="Regression"){
  pic<-order(vals$tree_rs$Rsquared, decreasing=T)[1:input$q_class_toptrees]
  } else{
    pic<-order(vals$tree_rs$Accuracy, decreasing=T)[1:input$q_class_toptrees]
}

  pred<-data.frame(pred[,pic])
  ref1<-rownames(model_data)
  lo<-split(data.frame(obs_data),ref1)
  pred_interval=input$q_class_val
  pred.rf.int <- data.frame(
    t(apply(pred, 1, function(x) {
      c(quantile(x, c(pred_interval/2,   1-(pred_interval/2))))
    }))
  )
  lpred<-split(data.frame(pred),ref1)
  lp<-split(pred.rf.int,ref1)
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
  vals$rf_treetest<-res

  data=t(pred)
  d=1:ncol(data)
  res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
  options_num<-lapply(res,function (x) range(x))
  options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))
  options_num_sel<-options_num[[which(options_show==input$splitdata_trees)]]
  data<-data[,options_num_sel[1]:options_num_sel[2], drop=F]
  obs_data<-data.frame(obs_data)
  obs=obs_data[ colnames(data),]
  pred_interval=input$q_class_val



  q_class_temp=vals$rf_treetest
  rownames(q_class_temp)<-rownames(model_data)
  q_class=q_class_temp[colnames(data),]


  numerics=data
  obs=obs_data[ colnames(data),]
  pred_interval=pred_interval
  q_class=q_class


  str_numerics3(numerics=numerics,
                obs=obs,
                pred_interval=pred_interval,
                q_class=q_class,
                cex=input$qclass_sizeplot)
  vals$plot_florest<-recordPlot()


})



  output$rf_tree_show<-renderUI({
    val=if(vals$rf_results$modelType=="Classification"){ 50} else { 20}
    numericInput(ns("nhist_tree"),"show", val, width = "100px")
  })
  output$rf_tree_pred<-renderUI({
    req(input$nhist_tree)
    data=t(predall_rf()$individual)
    d=1:ncol(data)
    res<-split(d, ceiling(seq_along(d)/input$nhist_tree))
    options_num<-lapply(res,function (x) range(x))
    options_show=as.vector(do.call(c,lapply(options_num, function(x) paste(x[1], x[2], sep = "-"))))

    div(
      inline(pickerInput(ns('splitdata_trees'),"Observations:", choices=options_show, width="200px")),
      actionButton(ns('downp_summ_trees'),tipify(icon("fas fa-download"), "Download Plot"), style="button_active")
    )
  })

  observeEvent(ignoreInit = T,input$downp_summ_trees,{
    vals$hand_plot<- if(vals$rf_results$modelType=="Regression"){"florest plot"
    } else{'florest plot (class)'}

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)

    })
  output$qclass_legend<-renderUI({
    req(vals$rf_results$modelType=="Regression")
    div(style="text-align: right",
        div(
          div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
          div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),'Mean predictions')

        ))
  })
  output$plot_forest<-renderUI({
    req(vals$rf_results$modelType=="Regression")
    plotOutput(ns("summ_trees"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))
  })
  output$qclass_out<-renderUI({
    req(input$qclass_width)
    req(input$qclass_height)
    div(style="background: white",
        inline(uiOutput(ns("rf_tree_show"))),
        inline(uiOutput(ns("rf_tree_pred"))),
        uiOutput(ns("qclass_legend")),
        uiOutput(ns("plot_forest")),
        uiOutput(ns("forest_byclass")))

  })
  output$forest_byclass<-renderUI({
    req(vals$rf_results$modelType=="Classification")

    plotOutput(ns("forestbyclass"), width = paste0(input$qclass_width,"px"), height =paste0(input$qclass_height,"px"))


  })


  get_forest_class<-reactive({
    req(input$predrf_newY_tree)
    req(input$splitdata_trees)
    req(input$nhist_tree)
    m<-vals$rf_results
    req(m$modelType=="Classification")
    pred<-predall_rf()
    pred <- pred$individual
    model_data<-test_rf()
    obs_data<-RF_observed2()


    pic<-order(vals$tree_rs$Accuracy, decreasing=T)[1:input$q_class_toptrees]
    pred<-data.frame(pred[,pic])

    obs_data<-data.frame(obs_data)
    data=t(pred)
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
    attr(res,'ntree')<-ncol(pred)
    attr(res,'obs')<-obs
    vals$get_forest_class<-res_forest<-res
    vals$get_forest_class
  })
  output$pick_rfobs_pred<-renderUI({
    req(input$rfpred_which=="Datalist")
    sup_test<-attr(vals$rf_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("obs_cm_pred"),
                       NULL,names(vals$saved_data[getobsRF()]), width="200px",selected=vals$obs_cm_pred
           ))
    )
  })

  observeEvent(vals$rf_results,{
    m<-vals$rf_results
    vals$q_class_toptrees<-round(m$finalModel$ntree/2)
  })
  output$predrf_newY_ref<-renderUI({
    req(input$predrf_newY_tree)
    req(input$rfpred_which)
    get_rtrees()
    req(!is.null(vals$tree_rs))

    #tree_rs<-data.frame(do.call(rbind,lapply(data.frame(pred),function(x)
      #postResample(x,obs_data))))
    #order(tree_rs$Rsquared, decreasing = T)[]

    div("+ Top trees:",
        inline(
          numericInput(ns("q_class_toptrees"),NULL, value=vals$q_class_toptrees , width="100px")
        ),
        bsTooltip('q_class_toptrees',"Selects the N top trees with best R squared")
    )})

  observeEvent(ignoreInit = T,input$q_class_toptrees,{
    vals$q_class_toptrees<-input$q_class_toptrees
  })
  output$forest_margin<-renderUI({
    req(vals$rf_results$modelType=="Regression")
    div(
      span("+ Margin:",
           inline(
             numericInput(ns("q_class_val"),NULL, value=0.05, width="75px", step=0.05)
           )
      )
    )
  })
  output$forest_col<-renderUI({
    req(vals$rf_results$modelType=="Classification")
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
  output$pick_rfobs<-renderUI({
    sup_test<-attr(vals$rf_results,"supervisor")
    supname<-paste0("+ Observed Y [",sup_test,"]")
    div(
      span(supname,tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values"),
           pickerInput(ns("predrf_newY_tree"),
                       NULL,names(vals$saved_data[getobsRF()]), width="200px",selected=vals$predrf_newY_tree
           ))
    )
  })
  output$qclass_side<-renderUI({

    div(
      class="map_control_style",style="color: #05668D",
      uiOutput(ns("pick_rfobs")),
      uiOutput(ns("predrf_newY_ref")),
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
    req(vals$rf_results$modelType=="Regression")
    div(
      div(actionLink(ns('wilcox_gdownp'),"Download table")),
      div( actionLink(ns('wilcox_gcreate'),"Create Datalist"))
    )
  })






  output$forestbyclass<- renderPlot({
    res_forest<-get_forest_class()
    vals$plot_florest_class<-plot_florest_class(res_forest,vals$newcolhabs, palette=input$forest_col)
    vals$plot_florest_class
  })
  observeEvent(ignoreInit = T,input$predrf_newY_tree,{
    vals$predrf_newY_tree<-input$predrf_newY_tree
  })
  observeEvent(ignoreInit = T,input$wilcox_gcreate,{

    vals$hand_save<-"wilcox_create"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_rf())

  })
  observeEvent(ignoreInit = T,input$wilcox_gdownp,{

    vals$hand_down<-"rf_qclass"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })





  ## downplot
  observeEvent(ignoreInit = T,input$downp_cmrf,{
    vals$hand_plot<-"Confusion Matrix RF"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$downp_confrf,{
    vals$hand_plot<-"Confusion Matrix - test RF"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})


  observeEvent(ignoreInit = T,input$downp_prf,{
    vals$hand_plot<-"Minimal Depth distribution"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$downp_mrf,{
    vals$hand_plot<-"Multi-way importance"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$downp_rfbi,{

    vals$hand_plot<-switch(input$pd_tab,
                              "pd_tab1"="RF - Partial Dependence",
                              "pd_tab2"="RF - Partial Dependence (classes)"
    )
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$rfinter_downp,{
    vals$hand_plot<-"RF interactions"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$rfcomp_rank_downp,{
    vals$hand_plot<-"RF ranking comparations"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})
  observeEvent(ignoreInit = T,input$rfcomp_meas_downp,{
    vals$hand_plot<-"RF measure comparations"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})


  ## create
  observeEvent(ignoreInit = T,input$create_rfinter,{
    vals$hand_save<-"Create Datalist: RF frequent interactions"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(module_rf())
  })
  observeEvent(ignoreInit = T,input$create_rf,{
    vals$hand_save<-"Create Datalist: RF top variables"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(module_rf())
  })
  observeEvent(ignoreInit = T,input$create_rfreg,{
    vals$hand_save<-"Create Datalist: RF prediction errors"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(module_rf())
  })
  observeEvent(ignoreInit = T,input$rf_create_predictions,{
    vals$hand_save<-"Create Datalist: RF predictions"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL

    showModal(module_rf())
  })
  observeEvent(ignoreInit = T,input$create_rfreg_errorinter,{
    vals$hand_save<-"Create Datalist: RF training errors"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_rf())
  })


  ## downcenter
  observeEvent(ignoreInit = T,input$dowcenter_cmrf,{
    vals$hand_down<-"rf_cm"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })

   observeEvent(ignoreInit = T,input$downcenter_rfdepth,{
    vals$hand_down<-"rfdepth"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downcenter_rfinter,{
    vals$hand_down<-"rfinter"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_rf_tab3_1,{
    vals$hand_down<-"rf_predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downcenter_intererror,{
    pic<-switch(vals$rf_results$modelType,
                "Regression"='rf_reg_obsErrs',
                "Classification" ='rf_class_obsErrs')
    vals$hand_down<-pic
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$down_rf_errors,{
    pic<-switch(vals$rf_results$modelType,
                "Regression"='rf_reg_predErrs',
                "Classification" ='rf_class_predErrs')
    vals$hand_down<-pic
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })



  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })
  rf_inputsmeasure<-reactive({
    if(vals$rf_results$modelType=="Classification"){
      choices=c("mean_min_depth",
                "accuracy_decrease",
                "gini_decrease" ,
                "no_of_nodes",
                "times_a_root")
    } else{
      choices=c('mean_min_depth', 'mse_increase', 'node_purity_increase', 'no_of_nodes', 'times_a_root')
    }
  })
  mindeaphrf<- reactive({try({

    req(length(vals$rf_results)>0)
    withProgress(message = "Running ...",
                 min = 1,
                 max = 1,
                 {
                   res <- multipimp(vals$rf_results,measures=c(input$rf_measures,'p_value','no_of_trees'), mean_sample=input$rfmeasure_mean_sample)
                 })
    res

  })})

  getnewdataRF<-reactive({
    datalist<-vals$saved_data

    m<-vals$rf_results
    res00<-unlist(lapply(
      datalist, function(x){
        ncol(x)==ncol(m$trainingData[-1])
      }
    ))
    datalist<-datalist[res00==T]
    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(m$trainingData[-1])
        sum(res)==ncol(m$trainingData[-1])
      })
    )
    names(res0[res0==T])
  })
  getobsRF<-reactive({
    sup_test<-attr(vals$rf_results,"supervisor")

    #vals$rf_results<-attr(vals$saved_data$abio,"rf")[[2]][[1]]
    datalist<-vals$saved_data
    if(vals$rf_results$modelType=="Classification"){
      datalist=lapply(datalist,function(x) attr(x,"factors"))} else{}


    m<-vals$rf_results
    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)==sup_test)

      })
    )
    names(res0[res0==T])
  })
  rf_multi<-reactive({
req(input$interframe_palette)
    if(!isTRUE(input$rf_sigmulti)){
      req(input$multi_no_of_labels)
    }
    #req(input$interframe_xmin)
    #req(input$interframe_ymin)
    #req(input$interframe_xmax)
    #req(input$interframe_ymax)
    #xlim<-c(input$interframe_xmin,input$interframe_xmax)
    #ylim<-c(input$interframe_ymin,input$interframe_ymax)

    #savereac()

    frame<- data.frame(attr(vals$rf_results,"mindepth")[[2]])
    x = input$multi_x_measure
    y = input$multi_y_measure
    z=input$multi_z_measure

    p<-plot_interframe(
      data.frame(frame),
      x = x,
      y = y,
      z=z,
      sig=input$sigmrf,
      xlab=x,
      ylab=y,
      zlab=z,
      xlim=xlim,
      ylim=ylim,
      palette=input$interframe_palette,
      newcolhabs=vals$newcolhabs,
      main=input$interframe_title,
      logx=input$interframe_logx,
      logy=input$interframe_logy,
      cex.axes=input$interframe_axis_size,
      cex.lab=input$interframe_axis_size,
      cex.main=input$interframe_axis_size,
      cex.leg=input$interframe_axis_size,
      size=input$interframe_size,
      label.size=input$interframe_label_size,
      max.overlaps=input$max.overlaps

    )

    vals$rfm_res<-p
    p

  })
  prf.reactive <- reactive({

    if(!isTRUE(input$rf_depth)){
      req(input$n_var_rf)
    }
    mean_scale<-if(isFALSE(input$mean_scale)){F}else{T}
    res<-prf( attr(vals$rf_results,"mindepth"), sigs =     if (input$rf_depth == TRUE) { TRUE} else {input$n_var_rf},
             input$sigprf,
             size_plot=input$labrfsize,
             min_no_of_trees = input$depth_min_no_of_trees,
             mean_sample = input$depth_mean_sample,
             newcolhabs=vals$newcolhabs,
             palette=input$mdd_palette,
             size_mmd=input$size_mmd)
    res<- res +
      ggtitle(input$title_prf)+
      labs(x = input$ylab_prf, y=input$xlab_prf)


    vals$rf_sigs<-attr(res,"sigs")

    res
  })
  getrf_errors<-reactive({

    pred<-pred_rf()
    newpred<-pred[,1]
    names(newpred)<-rownames(pred)
    predicted<-newpred
    factors<-vals$saved_data[[input$predrf_new]]
    observed<-if(input$rfpred_which=="Datalist"){
      factors[,attr(vals$rf_results,"supervisor")]
    } else{
      attr(vals$rf_results,"sup_test")
    }

    accu_rf_reg(observed,predicted)


  })



  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      ##RF
      "Save RF model in"= {name_saverf()},
      "Create Datalist: RF frequent interactions"={ name_saverf_int()},
      "Create Datalist: RF top variables"={ name_saverf_top()},
      "Create Datalist: RF prediction errors"= { name_saverf_predErr()},
      "Create Datalist: RF predictions"= { name_saverf_pred()},
      "Create Datalist: RF training errors"= { name_saverf_err()},
      "wilcox_create"={name_wilcox_pred()}
    )})
  saverf<-reactive({
    #req(vals$cur_rf_models=='new rf (unsaved)')
    temp<-vals$rf_results
    if(input$hand_save=="create"){
      temp<-list(temp)
      names(temp)<-input$newdatalist
      attr(vals$saved_data[[input$data_rfX]],"rf")[[input$newdatalist]]<-temp
      cur<-input$newdatalist
    } else{
      temp<-list(temp)
      names(temp)<-input$over_datalist
      attr(vals$saved_data[[input$data_rfX]],"rf")[input$over_datalist]<-temp
      cur<-input$over_datalist
    }
    vals$cur_rf_models<-cur
    attr(vals$saved_data[[input$data_rfX]],"rf")[['new rf (unsaved)']]<-NULL
    #vals$bag_rf<-F
    #vals$cur_rf<-cur

  })
  datalistrf<-reactive({
    #res<-vals$rf_results
   # saveRDS(res,"res.rds")
   # saveRDS(reactiveValuesToList(input),"input.rds")
   # saveRDS(reactiveValuesToList(vals),"savepoint.rds")
    #input<-readRDS('input.rds')
   # vals<-readRDS('savepoint.rds')
    frame<-data.frame(attr(vals$rf_results,"mindepth")[[2]])

    datao<-vals$saved_data[[input$data_rfX]]
    temp<-datao[,as.character(frame$variable[which(frame$p_value<input$sigprf)])]


    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
      vals$cur_data<-input$newdatalist
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
      vals$cur_data<-input$over_datalist
    }

  })
  wicox_create<-reactive({

    m<-vals$rf_results
    var<-attr(m,"supervisor")
    temp<-vals$rf_treetest
    if(input$rfpred_which=="Partition"){
      datao<-vals$saved_data[[input$data_rfX]]
      coords<-attr(vals$saved_data[[input$data_rfX]],"coords")[rownames(attr(vals$rf_results,"test")),]

    }else{
      datao<-vals$saved_data[[input$predrf_newY_tree]]
      coords<-attr(vals$saved_data[[input$predrf_newY_tree]],"coords")

    }
    rownames(temp)<-rownames(coords)
    factors<-attr(temp,"factors")

    factors<-temp[c('q_class')]
    rownames(factors)<-rownames(temp)
    colnames(factors)<-paste(var,colnames(factors),sep="_")
    temp[c('w_class','sig','q_class')]<-NULL

    temp<-data_migrate(datao,temp,"new")
    attr(temp,"factors")<-factors
    attr(temp,"coords")<-coords

    if(input$hand_save=="create") {

      vals$saved_data[[input$newdatalist]]<-temp

    } else{
      vals$saved_data[[input$over_datalist]]<-temp

    }

  })
  datalistrfinter<-reactive({
    vals$rf_interactions_frame<-attr(vals$rf_results,"interframe")
    a<-vals$rf_interactions_frame[1:vals$rfinter_k2,"variable"]
    b<-as.character(vals$rf_interactions_frame[1:vals$rfinter_k2,"root_variable"])
    pic<-unique(c(a,b))

    temp<- vals$saved_data[[input$data_rfX]][, pic]
    temp<-data_migrate( vals$saved_data[[input$data_rfX]],temp,"newdata_rf")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp

    } else{
      vals$saved_data[[input$over_datalist]]<-temp

    }

  })
  datalistrf_prederrors<-reactive({
    temp<-vals$rf_prederrors
    datao<-if(vals$rfpred_which=="Datalist"){
      vals$saved_data[[vals$predrf_newY]]
    } else{ vals$saved_data[[input$data_rfX]]}

    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }


  })
  datalistrf_predicions<-reactive({
    #input<-readRDS('input.rds')
    #vals<-readRDS('vals.rds')

    temp_f<-temp<-data.frame(vals$rftab_pred)
    colnames(temp_f)<-colnames(temp)<-paste0("pred_",colnames(temp))
    temp[,1]<-as.numeric( temp[,1])
    temp_f[,1]<-factor( temp[,1])

    datao<-if(vals$rfpred_which=="Datalist"){
      vals$saved_data[[input$predrf_new]]
    } else{ vals$saved_data[[input$data_rfX]]}
    req(!is.null(attr(datao,'factors')))
    faco<-attr(datao,'factors')[rownames(temp),]
    temp_f<-cbind(faco,temp_f)
    attr(temp,"factors")<-temp_f
    attr(temp,"coords")<-attr(datao,"coords")[rownames(datao),]
    attr(temp,"base_shape")<-attr(datao,"base_shape")
    attr(temp,"layer_shape")<-attr(datao,"layer_shape")
    attr(temp,"extra_shape")<-attr(datao,"extra_shape")
    if(input$hand_save=="create") {
  vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  datalistrferrors<-reactive({
    req(!is.null(vals$hand_save))
    temp<-switch(vals$rf_results$modelType,
                 "Regression"=data.frame(accu_rf_reg_model(vals$rf_results)),
                 "Classification" =data.frame(accu_rf_class(vals$rf_results)))
    temp<-data_migrate( vals$saved_data[[input$data_rfX]],temp,"newdata_rf")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  name_saverf<-reactive({
    name0<-paste0('RF')
    bag<-1
    # name0<-paste0("rf")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[input$data_rfX]],"rf"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[input$data_rfX]],"rf"))) break
      }}
    paste(name0,bag)
  })
  name_saverf_top<-reactive({
    bag<-1
    name0<-paste("RF_sigs")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("RF_sigs",bag)
  })
  name_saverf_int<-reactive({
    bag<-1
    name0<-paste("rf_inter")
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
  name_saverf_err<-reactive({

    bag<-1
    name0<-paste("RFmodel_errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("RFmodel_errors",bag)

  })
  name_saverf_predErr<-reactive({


    bag<-1
    name0<-paste0('RF_errors (',attr(vals$rf_results,"supervisor"),
                 "~",
                 input$data_rfX,")")
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
  name_saverf_pred<-reactive({


    if(input$rfpred_which=="Datalist"){
      name0<- paste0(
        'RF_pred (',attr(vals$rf_results,"supervisor"),
        "~",
        input$predrf_new,")"
      )
    } else{
      test_partition<-attr(vals$rf_results,"test_partition")
      x<-attr(vals$rf_results,"Datalist")
      todo<-paste(gsub("Test data:| ","",test_partition))
      b<-gsub(".*::","",todo)
      name0<-paste0(
        'RF_pred (',attr(vals$rf_results,"supervisor"),
        "~",
        paste0(x,"[",b,"]")
      )
      #test_partition<-paste("Test data:","Campanha","::",01)
      #a<- gsub("::.*","",todo)

    }

    bag<-1
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
  name_wilcox_pred<-reactive({

    bag<-1
    var<-attr(vals$rf_results,"supervisor")
    name0<-paste("rf",var,"qclass", sep="_")
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
  output$data_over<-renderUI({
    data_overwritte$df<-F
    choices<-c(names(vals$saved_data))
    req(input$hand_save=="over")
    if(vals$hand_save=='Save RF model in'){choices<-names(attr(vals$saved_data[[input$data_rfX]],'rf'))}
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
      "Create Datalist: RF top variables"= {datalistrf()},
      "Create Datalist: RF frequent interactions"= {datalistrfinter()},
      "Create Datalist: RF prediction errors"= {datalistrf_prederrors()},
      "Create Datalist: RF predictions"= {datalistrf_predicions()},
      "Create Datalist: RF training errors"= {datalistrferrors()},
      "Save RF model in"= {saverf()},
      "wilcox_create"={wicox_create()}
    )
    removeModal()

  })
  module_rf <- function() {
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
