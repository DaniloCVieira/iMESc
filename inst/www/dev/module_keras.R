#' @export
module_keras<-list()
#' @export
module_keras$ui<-function(id){

  library(keras)
  library(tensorflow)
  library(MLmetrics)

  ns<-NS(id)
  fluidPage(style="padding: 20px",

            div(actionLink(ns("save_teste"),"SAVE"),style="position: fixed; top:0px; left: 0px"),
            uiOutput(ns("header"))



  )
}
#' @export
module_keras$server<-function( id,vals) {
  moduleServer(id,function(input, output, session){

    ns<-session$ns

    output$bug<-renderUI({
      renderPrint(input$y_newdata)
    })



    output$out_saved_model<-renderUI({
      req(input$new_or_load=="saved_model")

      choices<-sapply(vals$saved_data,function(x){
        length(attr(x,"keras_model"))>0
      })
      choices<-names(vals$saved_data)[choices]
      validate(need(length(choices)>0,"No datalist containing a saved model found."))
      div(
        # inline(pickerInput(ns("datalist_model"),"Datalist:",choices=choices)),
        inline(uiOutput(ns("o_model")))
      )
    })
    observeEvent(get_model_name(),{
      vals$cur_keras_model<-get_model_name()
    })
    observeEvent(input$save_model,{
      vals$hand_save<-"Save keras_model model in"
      vals$hand_save2<-column(12,fluidRow(em(input$x_datalist, style="color:gray"),strong("::"), em("keras_model-Attribute", style="color:gray"),strong("::")
      ))
      vals$hand_save3<-NULL
      showModal(module_keras_model())
    })
    savekeras_model<-reactive({
      withProgress(message="Saving...",min=1,max=1,{

        temp_root<-"temp_keras"
        if(!dir.exists(temp_root))
          dir.create(temp_root)
        path_datalist<-paste0(temp_root,'\\',input$x_datalist)
        if(!dir.exists(path_datalist))
          dir.create(path_datalist)
        path_model<-paste0(path_datalist,"\\" , input$newdatalist)
        if(!dir.exists(path_model))
          dir.create(path_model)
        tf$keras$models$save_model(deep_model$df,path_model)
        ## to save in the savepoint
        bin<-keras_bin()
        # Remover o diretório interativamente
        unlink(temp_root,  recursive  = TRUE)

        attr(vals$saved_data[[input$x_datalist]],"keras_model")[[input$newdatalist]][[1]]<-attributes(deep_model$df)
        attr(vals$saved_data[[input$x_datalist]],"keras_model")[[input$newdatalist]][[2]]<-bin


      })
      vals$save_changes_keras<-"div"
    })
    output$page1<-renderUI({
      req(input$new_or_load=="new_model")
      div(
        shinydashboardPlus::box(style="height: 280px",
            width =4,
            h4("1.1. Training Data"),
            uiOutput(ns("header_inputs"))
        ),

        shinydashboardPlus::box(width=8,
            div(
              h4("1.2. Hidden Layers"),
              div(class='map_control_style2',numericInput(ns("nlay"),"Number of Layers",3)),
              style="height: 280px; overflow-y: scroll;",
              uiOutput(ns("out_hidden_layers")))
        ),
        shinydashboardPlus::box(
          width =4,
          fluidRow(
            column(6, h4("1.3. Build architecture")),
            column(6,align="right", div(actionButton(ns("build_model"), "Build -- >> ")))
          ),
          renderPrint(deep_model$df)
        ),

        shinydashboardPlus::box(width=8,
            uiOutput(ns('build_plot'))
        )




      )
    })
    cur_keras<-reactiveValues(tab="tab1")
    observeEvent(input$tab,{
      vals$cur_keras_tab<-input$tab
    })
    observeEvent(input$new_or_load,{
      vals$cur_keras_new_or_load<-input$new_or_load
    })
    output$save_button<-renderUI({
      req(input$new_or_load=="new_model")
      req(!is.null(deep_model$df))

      req(vals$save_changes_keras=="save_changes")

      inline(
        div(class=vals$save_changes_keras,
            actionButton(ns("save_model"),icon("fas fa-save"))
        )
      )
    })
    output$header<-renderUI({

      fluidRow(style="height: 600px; overflow-y: scroll;background: white",
               style="padding:20px;",
               fluidRow(
                 inline(uiOutput(ns('save_button'))),
                 inline(radioGroupButtons(ns("new_or_load"),NULL,choiceNames =c("New Model", "Saved models"),choiceValues =c('new_model',"saved_model"), selected =vals$cur_keras_new_or_load)),
                 inline(uiOutput(ns('out_saved_model'))),
                 inline(uiOutput(ns('remove_model_btn')))
               ),
               div(style="background: white",
                   tabsetPanel(id=ns("tab"),selected=vals$cur_keras_tab,
                               tabPanel(value ="tab1",
                                        "1. Network",
                                        uiOutput(ns('page1')),
                                        uiOutput(ns('page1_saved'))
                               ),
                               tabPanel(value ="tab2",
                                        "2. Training",
                                        uiOutput(ns("page2")),
                                        uiOutput(ns('page2_saved'))
                               ),
                               tabPanel(value ="tab3",
                                        "3. Results",
                                        uiOutput(ns("page3"))
                               ),
                               tabPanel(value ="tab4",
                                        "4. Predict",
                                        uiOutput(ns("page4"))
                               ),
                               tabPanel(value ="tab4",
                                        "5. Spatial Predictions",
                                        uiOutput(ns("page5"))
                               )
                   )
               )

      )
    })
    output$page5<-renderUI({
      req(!is.null(deep_model$df))
      validate(need(!is.null(cur_keras$pred),"Run the predictions in tab 4 before proceeding"))
      data<-get_datalist_with_predictions()
      coords<-req(attr(data,"coords"))
      req(!is.null(coords))
      renderPlot(
        withProgress(message="Predicting",{
          plot_raster(data)
        })

      )

    })
    output$remove_model_btn<-renderUI({
      req(input$new_or_load)
      req(input$new_or_load=="saved_model")
      req(get_model_name())
      inline(actionButton(ns("remove_model"),icon("fas fa-trash")))
    })
    output$page2_saved<-renderUI({
      req(input$new_or_load=="saved_model")
      div(
        "Saved models cannot be parameterized. Proceed to the Results or Predict tab.",
        renderPlot({
          plot_network(deep_model$df)
        })
      )
    })
    output$page1_saved<-renderUI({
      req(input$new_or_load=="saved_model")
      div(
        "Saved models cannot be parameterized. Proceed to the Results or Predict tab.",
        renderPlot({
          plot_network(deep_model$df)
        })
      )
    })
    observeEvent(newdata_x(),{
      cur_keras$pred<-NULL
    })
    observeEvent(input$new_or_load,{
      req(input$new_or_load=="new_model")
      temp_root<-'temp_keras'
      deep_model$df<-NULL
      unlink(temp_root,  recursive  = TRUE)
    })
    # vals<-readRDS("savepoint.rds")
    #attr(vals$saved_data$environ_mean_scaled,"keras_model")<-NULL
    #saveRDS(vals,"savepoint.rds")
    pred_type<-reactive({
      req(!is.null(deep_model$df))
      train_id<-attr(deep_model$df,"test_data")
      if(is.null(train_id)){"Datalist"} else {c("Datalist","Partition")}
    })
    output$save_pred_button<-renderUI({
      req(!is.null(deep_model$df))
      inline(actionButton(ns("save_pred_model"),icon("fas fa-save")))
    })
    observeEvent(input$save_pred_model,{
      vals$hand_save<-"Create Datalist with Predictions"
      vals$hand_save2<-column(12,
                              fluidRow(em(get_datalist_tosave(), style="color:gray")
                              )
      )
      vals$hand_save3<-NULL
      showModal(module_keras_model())
    })
    output$page4<-renderUI({
      req(!is.null(deep_model$df))
      div(style="height: 800px; overflow-y: scroll;",
          sidebarLayout(
            sidebarPanel(
              width=4,
              class="well3",
              inline(div(class='map_control_style2',
                         inline(radioButtons(ns("pred_type"),"New Data (X):", pred_type())),
                         inline(uiOutput(ns("ox_newdata"))),
                         inline(uiOutput(ns("oy_newdata")))

              )),
              uiOutput(ns("pred_war"))
            ),
            mainPanel(
              actionButton(ns("run_pred"),"RUN >>"),
              uiOutput(ns('page4_main')),
            )
          )

      )
    })
    observeEvent(input$run_pred,{

      withProgress(message="Predicting",{
        cur_keras$pred<-getpred()
      })
    })
    output$page4_main<-renderUI({
      req(!is.null(cur_keras$pred))
      req(input$pred_type)
      req(input$x_newdata)
      #req(input$y_newdata)
      div(


        uiOutput(ns('predictions_class')),
        uiOutput(ns('predictions_reg')),
        uiOutput(ns('preddf')),

        #renderPrint(predictions_data.frame())
        #shinydashboardPlus::box(renderPrint({get_pred_rinal()}))
      )
    })
    output$preddf<-renderUI({
      req(!is.null(cur_keras$pred))
      div(
        h4("Predictions:",inline(uiOutput(ns('save_pred_button')))),
        div(inline(DT::dataTableOutput(ns("pred_table"))))
      )
    })
    output$predictions_class<-renderUI({
      req(input$y_newdata)
      req(attr(deep_model$df,"modelType")=="Classification")
      div(
        h4("Metrics:"),
        inline(DT::dataTableOutput(ns("pred_results_classification"))))
    })
    output$predictions_reg<-renderUI({
      req(input$y_newdata)
      req(attr(deep_model$df,"modelType")=="Regression")
      div(
        h4("Metrics:"),
        inline(DT::dataTableOutput(ns("pred_results_regression"))))
    })
    output$pred_table<-  myDT(predictions_data.frame())
    output$pred_war <- renderUI({
      sc <- list(
        center = attr(newdata_x(), "scaled:center"),
        scale = attr(newdata_x(), "scaled:scale")
      )

      if (is.null(sc$scale) && is.null(sc$center)) {
        return(NULL)
      }
      if(!is.null(sc$scale) &&
         !is.null(sc$center)){
        warning_message <- "Warning: The training data was scaled and centered before training.New data will be automatically scaled and centered."
      }
      if(is.null(sc$scale) &&
         !is.null(sc$center)){
        warning_message <- "Warning: The training data was centered before training.New data will be automatically centered."
      }
      if(!is.null(sc$scale) &&
         is.null(sc$center)){
        warning_message <- "Warning: The training data was scaled before training.New data will be automatically scaled."
      }

      div(em(warning_message,style="color: gray"))
    })
    class_stats<-reactive({
      req(!is.null(deep_model$df))
      req(attr(deep_model$df,"modelType")=="Classification")
      req(!is.null(cur_keras$pred))
      pred<-cur_keras$pred
      req(pred)
      obs<-newdata_y()
      req(obs)
      if(!is.null(attr(deep_model$df,"test_data"))){
        if(input$pred_type=="Partition")
          obs<-classmat2classvec(obs)
      }
      classification_stats(obs,pred)
    })
    output$pred_results_classification<-myDT(class_stats(),scrollY=F, info=F)
    reg_stats<-reactive({

      req(!is.null(deep_model$df))
      req(!is.null(cur_keras$pred))
      pred<-cur_keras$pred
      obs<-newdata_y()
      req(nrow(obs)==nrow(pred))
      req(ncol(obs)==ncol(pred))
      #saveRDS(list(pred,obs),"teste.rds")


      req(attr(deep_model$df,"modelType")=="Regression")
      result<-lapply(1:ncol(pred),function(i){
        pred<-cur_keras$pred[,i]
        obs<-newdata_y()[,i]
        regression_stats(obs,pred)
      })

      result<-data.frame(do.call(cbind,result))
      colnames(result)<-attr(deep_model$df,"train_labels")
      print(result)

      result

    })
    output$pred_results_regression<-myDT(reg_stats(),scrollY=F, info=F)

    get_pred_rinal<-reactive({
      req(!is.null(cur_keras$pred))
      return(cur_keras$pred)})
    newdata_y_binary<-reactive({
      if(attr(deep_model$df,"modelType")=="Classification"){
        to_binary(newdata_y())} else{newdata_y()}
    })
    getpred<-reactive({
      req(newdata_x())
      req(!is.null(deep_model$df))
      train_labels<-attr(deep_model$df,"testdata")
      req(train_labels)
      lev<-attr(train_labels,"dimnames")[[2]]
      model_info<-extract_model_info(deep_model$df)
      model<-deep_model$df
      if(attr(deep_model$df,"modelType")=="Classification"){
        test_predictions<-model %>% predict(as.matrix(newdata_x())) %>% k_argmax() %>% as.integer()+1
        test_predictions<-factor(test_predictions,levels=lev)
      } else{
        test_predictions<-model %>% predict(as.matrix(newdata_x()))
      }

      test_predictions
    })
    get_metric_model<-reactive({
      req(!is.null(cur_keras$pred))
      pred<-cur_keras$pred
      obs<-newdata_y()
      sapply(1:ncol(pred),function(i){
        MLmetrics::R2_Score(pred[,i], obs[,i])
      })


    })
    output$ox_newdata<-renderUI({
      req(input$pred_type)
      req(input$pred_type=="Datalist")
      req(!is.null(deep_model$df))
      train_vars<-colnames(attr(deep_model$df,"data"))
      req(!is.null(train_vars))
      pic<-sapply(vals$saved_data,function(x){
        sum(colnames(x)%in%train_vars)==length(train_vars)
      })
      pickerInput(ns('x_newdata'),"->",names(vals$saved_data)[pic],names(vals$saved_data)[pic][4])
    })

    newdata_x<-reactive({
      req(!is.null(deep_model$df))
      req(input$pred_type)
      if(input$pred_type=="Partition"){
        newdata<-attr(deep_model$df,"test_data")$x
        req(newdata)
      } else{
        print(input$x_newdata)
        req(input$x_newdata)
        datao<-newdata<-vals$saved_data[[input$x_newdata]]
        req(datao)
        scale_attr<-attr(attr(deep_model$df,"data"),"scale")
        if(is.null(attr(newdata,"scale"))){
          if(!is.null(scale_attr)){
            req(length(scale_attr$center)==ncol(newdata))
            newdata<-scale(newdata,center=scale_attr$center,scale=scale_attr$scale)
            newdata<-data_migrate(datao,newdata,"")
          }
        }

      }
      req(newdata)

      newdata

    })
    observeEvent(input$epochs,{
      vals$cur_epochs<-input$epochs
    })
    observeEvent(input$batch_size,{
      vals$cur_batch_size<-input$batch_size
    })
    observeEvent(input$validation_split,{
      vals$cur_validation_split<-input$validation_split
    })
    observe({
      req(is.null(vals$cur_epochs))
      vals$cur_epochs<-50
      vals$cur_batch_size<-32
      vals$cur_validation_split<-0.2
    })

    output$page2<-renderUI({
      req(input$new_or_load=="new_model")
      validate(need(!is.null(deep_model$df),
                    "Please build the network in tab 1 before proceeding."))
      fluidPage(

        style="height: 800px; overflow-y: scroll;",
        shinydashboardPlus::box(uiOutput(ns("compile_inputs"))),
        shinydashboardPlus::box(
          class='map_control_style2',
          h4("2.3. Training engine"),
          numericInput(ns('epochs'),'epochs',vals$cur_epochs,step=5),
          numericInput(ns('batch_size'),'batch_size',vals$cur_batch_size,step=10),
          numericInput(ns('validation_split'),'validation_split',vals$cur_validation_split, min=0,max=1,step=0.1)
        ),
        shinydashboardPlus::box(
          h4("2.2. Optimizer parameters"),
          uiOutput(ns("optimizer_inputs"))),
        shinydashboardPlus::box(
          fluidRow(
            column(6, h4("2.4. Train the model")),
            column(6,align="right", div(actionButton(ns("run"), "RUN -- >> ")))
          ),
          uiOutput(ns("train_data_summary"))
        )

      )
    })
    output$optimizer_inputs<-renderUI({
      div(class='map_control_style2',
          uiOutput(ns('o_optimizer_adam')),
          uiOutput(ns('o_optimizer_adamax')),
          uiOutput(ns('o_optimizer_sgd')),
          uiOutput(ns('o_optimizer_nadam')),
          uiOutput(ns('o_optimizer_adagrad')),
          uiOutput(ns('o_optimizer_adadelta')),
          uiOutput(ns('o_optimizer_rmsprop')),
          uiOutput(ns('o_optimizer_ftrl'))
      )
    })
    x_summary<-reactive({
      style2="border-bottom: 1px solid gray"
      style="border-bottom: 1px dashed gray"
      train<-data_x_train() # X
      is_sc<-is_scaled(train)
      shinydashboardPlus::box(width = 6,
          h5(strong("X"),style=style2),
          div(style="background:white",
              p("Type: Numeric",style=style),
              p("Number of missing values:", sum(is.na(train)),style=style),
              p("Number of rows:", nrow(train),style=style),
              p("Number of columns:", ncol(train),style=style),
              p("Scaled:", is_sc$scale,style=style),
              p("Centered:", is_sc$center,style=style),
              if(input$y_partition!="None"){
                p("Number of rows - test:",  length(get_train_id()$test),style=style)
              }

          ))
    })
    y_summary<-reactive({
      style="border-bottom: 1px dashed gray"
      style2="border-bottom: 1px solid gray"
      train_labels<-data_y_train() # Y
      lev<-attr(train_labels,"dimnames")[[2]]
      levs<-paste(lev,collapse=", ")
      is_class<-input$mtype=="Classification"
      shinydashboardPlus::box(width = 6,
          h5(strong("Y"),style=style2),
          div(style="background:white",
              p("Type:",ifelse(is_class,"Categorical","Numeric"),style=style),
              p("Number of missing values:", sum(is.na(train_labels)),style=style),
              p("Number of rows:", nrow(train_labels),style=style),
              if(is_class){
                div(
                  p("Number of levels:", ncol(train_labels),style=style),
                  p("Levels:", levs,style=style)
                )
              },
              if(!is_class){
                p("Number of columns:", ncol(train_labels),style=style)
              },
              if(input$y_partition!="None"){
                p("Number of rows - test:",  length(get_train_id()$test),style=style)
              }


          ))
    })
    output$train_data_summary<-renderUI({
      fluidRow(x_summary(),y_summary())
    })
    output$o_optimizer_ftrl<-renderUI({
      req(input$optimizer=="ftrl" )
      div(
        numericInput(ns("ftrl_lr"),"learning_rate",0.001, step=0.001),
        numericInput(ns("ftrl_learning_rate_power"),
                     span("learning_rate_power",
                          tiphelp4("Must be less or equal to zero. Controls how the learning rate decreases during training. Use zero for a fixed learning rate.","right")),-0.5, step=0.1),
        numericInput(ns("ftrl_initial_accumulator_value"),
                     span("initial_accumulator_value", tiphelp4('The starting value for accumulators. Only zero or positive values are allowed',"right")),
                     0.1, step=0.1),
        numericInput(ns("ftrl_l1_regularization_strength"),
                     span("l1_regularization_strength", tiphelp4("A float value, must be greater than or equal to zero. Defaults to 0.0","right")),
                     0, step=0.05),
        numericInput(ns("ftrl_l2_regularization_strength"),
                     span("l2_regularization_strength", tiphelp4("A float value, must be greater than or equal to zero. Defaults to 0.0","right")),
                     0, step=0.05),
        numericInput(ns("ftrl_l2_shrinkage_regularization_strength"),
                     span("l2_shrinkage_regularization_strength", tiphelp4("A float value, must be greater than or equal to zero. This differs from L2 above in that the L2 above is a stabilization penalty, whereas this L2 shrinkage is a magnitude penalty. When input is sparse shrinkage will only happen on the active weights","right")),
                     0, step=0.05),
        numericInput(ns("ftrl_beta"),
                     span("beta", tiphelp4("A float value, representing the beta value from the paper. Defaults to 0.0.","right")),
                     0, step=0.05)
      )
    })
    output$o_optimizer_adam<-renderUI({
      req(input$optimizer=="adam" )
      div(
        numericInput(ns("adam_lr"),"learning_rate",0.001, step=0.001),
        numericInput(ns("adam_beta_1"),span("beta_1",tiphelp4("The exponential decay rate for the 1st moment estimates. Defaults to 0.9.","right")),0.9, step=0.1),
        numericInput(ns("adam_beta_2"),span("beta_2",tiphelp4("The exponential decay rate for the 2nd moment estimates. Defaults to 0.999","right")),0.999, step=0.01),
        numericInput(ns("adam_epsilon"),span("epsilon",tiphelp4("A small constant for numerical stability. This epsilon is -epsilon hat- in the Kingma and Ba paper (in the formula just before Section 2.1), not the epsilon in Algorithm 1 of the paper","right")),0.0000001, step=0.0000001)
      )
    })
    output$o_optimizer_adamax<-renderUI({
      req(input$optimizer=="adamax" )
      div(
        numericInput(ns("adamax_lr"),"learning_rate",0.001, step=0.001),
        numericInput(ns("adamax_beta_1"),span("beta_1",tiphelp4("The exponential decay rate for the 1st moment estimates. Defaults to 0.9.","right")),0.9, step=0.1),
        numericInput(ns("adamax_beta_2"),span("beta_2",tiphelp4("The exponential decay rate for the 2nd moment estimates. Defaults to 0.999","right")),0.999, step=0.01),
        numericInput(ns("adamax_epsilon"),span("epsilon",tiphelp4("A small constant for numerical stability. This epsilon is -epsilon hat- in the Kingma and Ba paper (in the formula just before Section 2.1), not the epsilon in Algorithm 1 of the paper","right")),0.0000001, step=0.0000001)
      )
    })
    output$o_optimizer_sgd<-renderUI({
      req(input$optimizer=="sgd" )
      div(
        numericInput(ns("sgd_lr"),"learning_rate",0.001, step=0.001),
        numericInput(ns("sgd_momentum"),span("momentum",tiphelp4("float hyperparameter >= 0 that accelerates gradient descent in the relevant direction and dampens oscillations.","right")),0, step=0.1),
        checkboxInput(ns("sgd_nesterov"),span("nesterov",tiphelp4("Whether to apply Nesterov momentum","right")),F)
      )
    })
    output$o_optimizer_nadam<-renderUI({
      req(input$optimizer=="nadam" )
      div(
        numericInput(ns("nadam_lr"),"learning_rate",0.001, step=0.001),
        numericInput(ns("nadam_beta_1"),span("beta_1",tiphelp4("The exponential decay rate for the 1st moment estimates. Defaults to 0.9.","right")),0.9, step=0.1),
        numericInput(ns("nadam_beta_2"),span("beta_2",tiphelp4("The exponential decay rate for the 2nd moment estimates. Defaults to 0.999","right")),0.999, step=0.01),
        numericInput(ns("nadam_epsilon"),span("epsilon",tiphelp4("A small constant for numerical stability. This epsilon is -epsilon hat- in the Kingma and Ba paper (in the formula just before Section 2.1), not the epsilon in Algorithm 1 of the paper","right")),0.0000001, step=0.0000001)
      )
    })
    output$o_optimizer_adagrad<-renderUI({
      req(input$optimizer=="adagrad" )
      div(
        numericInput(ns("adagrad_lr"),"learning_rate",0.001, step=0.001),
        numericInput(ns("adagrad_initial_accumulator_value"),"initial_accumulator_value",0.1, step=0.1),
        numericInput(ns("adagrad_epsilon"),span("epsilon",tiphelp4("A small constant for numerical stability. This epsilon is -epsilon hat- in the Kingma and Ba paper (in the formula just before Section 2.1), not the epsilon in Algorithm 1 of the paper","right")),0.0000001, step=0.0000001)
      )
    })
    output$o_optimizer_adadelta<-renderUI({
      req(input$optimizer=="adadelta" )

      div(
        numericInput(ns("adadelta_lr"),"learning_rate",0.001, step=0.001),
        numericInput(ns("adadelta_rho"),span("rho",tiphelp4("A floating point value. The decay rate. Defaults to 0.95.","right")),0.95, step=0.05),
        numericInput(ns("adadelta_epsilon"),span("epsilon",tiphelp4("A small constant for numerical stability. This epsilon is -epsilon hat- in the Kingma and Ba paper (in the formula just before Section 2.1), not the epsilon in Algorithm 1 of the paper","right")),0.0000001, step=0.0000001)
      )
    })
    output$o_optimizer_rmsprop<-renderUI({
      req(input$optimizer=="rmsprop" )
      div(
        numericInput(ns("rmsprop_lr"),"learning_rate",0.001, step=0.001),
        numericInput(ns("rmsprop_rho"),span("rho",tiphelp4("float, defaults to 0.9. Discounting factor for the old gradients.","right")),0.9, step=0.05),
        numericInput(ns("rmsprop_momentum"),span("momentum",tiphelp4("float, defaults to 0.0. If not 0.0., the optimizer tracks the momentum value, with a decay rate equals to 1 - momentum.","right")),0, step=0.01),
        numericInput(ns("rmsprop_epsilon"),span("epsilon",tiphelp4("A small constant for numerical stability. This epsilon is -epsilon hat- in the Kingma and Ba paper (in the formula just before Section 2.1), not the epsilon in Algorithm 1 of the paper","right")),0.0000001, step=0.0000001),
        checkboxInput(ns("rmsprop_centered"),span("centered",tiphelp4("If TRUE, gradients are normalized by the estimated variance of the gradient; if FALSE, by the uncentered second moment. Setting this to TRUE may help with training, but is slightly more expensive in terms of computation and memory. Defaults to FALSE.","right")),F)
      )
    })
    get_optimizer<-function(input) {
      optimizer_args<-switch(
        input$optimizer,
        adam = list(
          learning_rate = input$adam_lr,
          beta_1 = input$adam_beta_1,
          beta_2 = input$adam_beta_2,
          epsilon = input$adam_epsilon
        ),
        adamax = list(
          learning_rate = input$adamax_lr,
          beta_1 = input$adamax_beta_1,
          beta_2 = input$adamax_beta_2,
          epsilon = input$adamax_epsilon
        ),
        sgd = list(
          learning_rate = input$sgd_lr,
          momentum = input$sgd_momentum,
          nesterov = input$sgd_nesterov
        ),
        nadam = list(
          learning_rate = input$nadam_lr,
          beta_1 = input$nadam_beta_1,
          beta_2 = input$nadam_beta_2,
          epsilon = input$nadam_epsilon
        ),
        adagrad = list(
          learning_rate = input$adagrad_lr,
          initial_accumulator_value = input$adagrad_initial_accumulator_value,
          epsilon = input$adagrad_epsilon
        ),
        adadelta = list(
          learning_rate = input$adadelta_lr,
          rho = input$adadelta_rho,
          epsilon = input$adadelta_epsilon
        ),
        rmsprop = list(
          learning_rate = input$rmsprop_lr,
          rho = input$rmsprop_rho,
          momentum = input$rmsprop_momentum,
          epsilon = input$rmsprop_epsilon,
          centered = input$rmsprop_centered
        ),
        ftrl = list(
          learning_rate = input$ftrl_lr,
          learning_rate_power = input$ftrl_learning_rate_power,
          initial_accumulator_value = input$ftrl_initial_accumulator_value,
          l1_regularization_strength = input$ftrl_l1_regularization_strength,
          l2_regularization_strength = input$ftrl_l2_regularization_strength,
          l2_shrinkage_regularization_strength = input$ftrl_l2_shrinkage_regularization_strength,
          beta = input$ftrl_beta
        ),
        stop("Otimizador não reconhecido")
      )

      do.call(paste0("optimizer_", input$optimizer), optimizer_args)
    }
    get_train_id<-reactive({
      req(input$y_datalist)
      data<-get_datalist_y()
      if(input$mtype=="Regression"){
        data<-attr(data,'factors')
      }

      req(input$y_partition)
      if(input$y_partition=="None"){
        if(input$mtype=="Classification"){
          result<-list(train=rownames(data),test=rownames(data))
        } else{
          result<-list(train=rownames(data),test=NULL)
        }

      } else{
        req(input$y_partition)
        req(input$y_partition%in%colnames(data))
        fac<-data[,input$y_partition]
        inn<-fac%in%input$y_partition_targ
        train<-rownames(data)[which(!inn)]
        test<-rownames(data)[which(inn)]
        result<-list(train=train,test=test)

      }
      result
    })
    observeEvent(input$mtype,{
      #savereac()
    })

    data_x_train<-reactive({
      req(input$x_datalist)
      train<-vals$saved_data[[input$x_datalist]]
      train<-train[get_train_id()$train,]
      train
    })
    data_x_test<-reactive({
      req(input$x_datalist)
      test<-vals$saved_data[[input$x_datalist]]
      test[get_train_id()$test,]
    })
    get_datalist_y<-reactive({
      req(input$y_datalist)
      data<-vals$saved_data[[input$y_datalist]]
      if(input$mtype=='Classification'){
        data<-attr(data,"factors")
      }
      data
    })
    output$oy_datalist<-renderUI({
      pickerInput(ns('y_datalist'),"Datalist Y",match_with_x(), selected=vals$cur_dataY)
    })
    match_with_x<-reactive({
      req(input$x_datalist)
      train<-vals$saved_data[[input$x_datalist]]
      names(which(sapply(vals$saved_data,function(x){
        sum(rownames(x)%in%rownames(train))==nrow(train)
      })))
    })
    output$oy_target<-renderUI({
      req(input$y_datalist)
      data<-get_datalist_y()
      choices=colnames(data)
      multiple=if(input$mtype=="Classification"){F}else{T}
      if(!is.null(vals$cur_response)){
        if(!sum(vals$cur_response%in%choices)==length(vals$cur_response)){
          vals$cur_response<-choices[1]
        }
      }




      pickerInput(ns('y_target'),"Response",choices, vals$cur_response, multiple = multiple, options=list(`actions-box` = TRUE))
    })
    output$oy_newdata<-renderUI({
      req(!is.null(deep_model$df))
      req(newdata_x())
      req(input$pred_type)
      req(input$pred_type=="Datalist")

      supervisor<-attr(deep_model$df,"supervisor")
      req(supervisor)
      train_vars<-colnames(attr(deep_model$df,"data"))
      # saveRDS(supervisor,"supervisor.rds")
      # saveRDS(train_vars,"train_vars.rds")
      # supervisor<-readRDS("supervisor.rds")
      #train_vars<-readRDS("train_vars.rds")


      req(!is.null(train_vars))
      li<-vals$saved_data
      if(attr(deep_model$df,"modelType")=="Classification"){
        li<-lapply(li,function(x) attr(x,"factors"))
      }
      pic<-sapply(li,function(x){
        vars=sum(colnames(x)%in%supervisor)==length(supervisor)
        rows=sum(rownames(x)%in%rownames(newdata_x()))==nrow(newdata_x())
        all(vars,rows)
      })
      if(length(supervisor)>1){
        supervisor<-"having-y"
      }

      div(
        pickerInput(ns('y_newdata'),paste0('Datalist Y[',supervisor,"]"),names(vals$saved_data)[pic]),

      )
    })
    data_y_train<-reactive({
      req(input$x_datalist)
      req(input$y_target)
      train<-vals$saved_data[[input$x_datalist]]
      datay<-get_datalist_y()
      req(input$y_target%in%colnames(datay))
      data<-datay[rownames(train),input$y_target,drop=F]
      train_labels<-data[get_train_id()$train,,drop=F]
      if(input$mtype=='Classification'){
        train_labels<-to_binary(train_labels)
      }
      train_labels
    })
    data_y_test<-reactive({
      req(input$y_target)
      test_labels<-get_datalist_y()[input$y_target]
      test_labels<-test_labels[get_train_id()$test,,drop=F]
      if(input$mtype=='Classification'){
        test_labels<-to_binary(test_labels)
      }
      test_labels
    })

    newdata_y<-reactive({
      newx<-newdata_x()
      if(input$pred_type=="Partition"){
        data<-attr(deep_model$df,"test_data")$y
      } else{
        req(length(input$y_newdata)>0)
        data<-get_newdata_y()
        supervisor<-attr(deep_model$df,"supervisor")
        if(attr(deep_model$df,"modelType")=="Classification"){
          data<-data[rownames(newx),supervisor]
        } else{
          data<-data[rownames(newx),supervisor, drop=F]
        }

      }

      if(attr(deep_model$df,"modelType")=="Classification"){
        #data<-to_binary(data)
      }
      data
    })
    #get_newdata_y()
    get_newdata_y<-reactive({
      data<-vals$saved_data[[input$y_newdata]]
      if(attr(deep_model$df,"modelType")=="Classification"){
        data<-attr(data,"factors")
      }
      data
    })
    #get_newdata_y()

    gethistory<-reactiveVal()
    observeEvent(input$run,{
      try({
        # vals$deep_model<-deep_model$df
        model<-initialize_model()
        train<-data_x_train()
        train_labels<-data_y_train()



        withProgress(message="Running...",{
          history <- model %>% fit(
            as.matrix(train),
            as.matrix(train_labels),
            epochs = input$epochs,
            batch_size = input$batch_size,
            validation_split = input$validation_split,
            verbose = 2
          )
          attr(model,"data")<-train
          attr(model,"supervisor")<-input$y_target
          attr(model,"train_labels")<-colnames(data_y_train())
          attr(model,"testdata")<-data_y_train()
          attr(model,"levels")<-levels(get_datalist_y()[,input$y_target])
          if(input$y_partition!="None"){
            attr(model,"test_data")<-list(x=data_x_test(), y=data_y_test())
          } else{
            attr(model,"test_data")<-NULL
          }

          attr(model,"metrics")<- history$metrics

          attr(model,"modelType")<-input$mtype
        })

        #vals$deep_model<-model
        #saveRDS(reactiveValuesToList(input),'input.rds')
        # readRDS(reactiveValuesToList(vals),'savepoint.rds')
        #input<-readRDS('input.rds')
        #vals<-readRDS('savepoint.rds')

        deep_model$df<-model
        gethistory(history)
        updateTabsetPanel(session,inputId="tab",selected="tab3")

        vals$save_changes_keras<-"save_changes"
      })

    })
    get_pred_train<-reactive({
      train_labels<-attr(deep_model$df,"testdata")
      train<-attr(deep_model$df,"data")
      lev<-attr(train_labels,"dimnames")[[2]]
      model_info<-extract_model_info(deep_model$df)
      model<-deep_model$df
      if(attr(deep_model$df,"modelType")=="Classification"){
        test_predictions<-model %>% predict(as.matrix(train)) %>% k_argmax() %>% as.integer()+1
        test_predictions<-factor(test_predictions,levels=lev)
      } else{
        test_predictions<-model %>% predict(as.matrix(train))
      }

      test_predictions
    })


    output$results_classification<-renderUI({
      req(attr(deep_model$df,"modelType")=="Classification")
      pred<-get_pred_train()
      obs<- attr(deep_model$df,"testdata")
      #saveRDS(list(pred,obs),"teste.rds")
      obs<-classmat2classvec(obs)
      div(
        renderPrint(
          classification_stats(obs,pred)
        )
      )
    })

    output$results_regression<-renderUI({

      req(attr(deep_model$df,"modelType")=="Regression")
      pred0<-get_pred_train()
      obs0<- attr(deep_model$df,"testdata")

      result<-lapply(1:ncol(pred0),function(i){
        pred<-pred0[,i]
        obs<-obs0[,i]
        regression_stats(obs,pred)
      })
      names(result)<-attr(deep_model$df,"train_labels")
      div(
        renderPrint(result)
      )

    })
    output$page3<-renderUI({
      req(!is.null(deep_model$df))
      metrics<-attr(deep_model$df,"metrics")
      div(
        shinydashboardPlus::box(
          uiOutput(ns('results_classification')),
          uiOutput(ns('results_regression')),
          renderPrint({
            model<-deep_model$df
            #attributes(model)
            train<-as.matrix(attr(model,"data"))
            test<-as.matrix(attr(model,"testdata"))
            res<-deep_model$df %>% evaluate(train,test)
            res['loss']
          })
        ),


        shinydashboardPlus::box(
          pickerInput(ns("history_metrics"),"Metric:", names(metrics), multiple = T, selected=names(metrics)[1:2]),
          uiOutput(ns('history_out'))

        ),
        #box(renderPlot(plot(gethistory())))

      )

    })
    output$history_out<-renderUI({
      req(input$history_metrics)
      metrics<-attr(deep_model$df,"metrics")
      div(
        renderPlot({
          plot_history_keras(metrics[input$history_metrics])
        })
      )
    })
    train_pred<-reactive({
      model<-deep_model$df
      train<-data_x_train()
      train_labels<-data_y_train()
      train_predictions<-model %>% predict(as.matrix(train))
      train_predictions<-data.frame(train_predictions)
      train_predictions<-data.frame(apply(train_predictions,1,mean))
      r_train<-MLmetrics::R2_Score(train_predictions, train_labels)
      r_train
    })
    observeEvent(input$mtype,{
      vals$cur_model_type<-input$mtype
    })
    output$o_mtype<-renderUI({
      if(is.null(vals$cur_model_type)){vals$cur_model_type<-'Classification'}
      radioButtons(ns("mtype"),"Type",c("Regression","Classification"),selected =vals$cur_model_type ,inline=T)
    })

    output$header_inputs<-renderUI({
      div(class='map_control_style2',
          div(uiOutput(ns("o_mtype"))),
          div(uiOutput(ns("ox_datalist"))),
          div(uiOutput(ns("oy_datalist"))),
          div(uiOutput(ns("oy_target"))),
          div(uiOutput(ns("oy_partition"))),
          div(uiOutput(ns("oy_partition_targ"))),
          div(
            div(numericInput(ns('seed'),'seed',123))

          )

      )
    })
    output$oy_partition<-renderUI({
      req(input$y_datalist)
      data<-get_datalist_y()
      if(input$mtype=="Regression"){
        data<-attr(data,"factors")
      }
      choices<-c("None",colnames(data))
      pickerInput(ns('y_partition'),"Partition:",choices,vals$cur_test_partition)
    })
    output$oy_partition_targ<-renderUI({
      req(input$y_datalist)
      data<-get_datalist_y()
      if(input$mtype=="Regression"){
        data<-attr(data,"factors")
      }

      req(input$y_partition)
      req(input$y_partition!="None")
      choices<-levels(data[,input$y_partition])
      pickerInput(ns('y_partition_targ'),"Test reference:",choices,vals$cur_testdata)
    })
    output$ox_datalist<-renderUI({
      pickerInput(ns('x_datalist'),"Datalist X",names(vals$saved_data),vals$cur_data)
    })
    observeEvent(input$nlay,{

      output$out_hidden_layers<-renderUI({

        train<-data_x_train()
        train_labels<-data_y_train()
        lis<-lapply(1:input$nlay,function(i){
          ac <- if (i == input$nlay && input$mtype == "Classification") "softmax" else "relu"
          u <- if (i == 1) 32 else if (i == input$nlay) ncol(train_labels) else 10
          # v <- if (i == 1) ncol(train) else 1






          shinydashboardPlus::box(style="border: 1px solid;margin:5px; padding: 10px",class='map_control_style2',width=4,
              h4(paste("Layer",i)),
              # numericInput(ns(paste0('input_shape',i)), "Input_shape",value=v, step=1),
              numericInput(ns(paste0('units',i)), "Units",u),
              pickerInput(ns(paste0('activation',i)),'activation',c("relu","softmax","sigmoid","linear","exponential","softsign"),selected=ac, width="150px")
          )
        })
        do.call(fluidRow,lis)
      })
    })


    # UI para optimizer
    output$o_optimizer<-renderUI({
      data<-data_y_train()
      choices<-if (input$mtype == "Regression") {
        c('rmsprop', 'adam', 'sgd', 'adagrad', 'adadelta', 'nadam', 'adamax','ftrl')
      } else if (input$mtype == "Classification") {
        c('adam', 'adamax', 'sgd', 'nadam', 'adagrad', 'adadelta','ftrl')
      }
      pickerInput(ns('optimizer'), 'optimizer', choices)
    })

    # UI para loss
    output$o_loss<-renderUI({
      data<-data_y_train()
      choices<-if (input$mtype == "Regression") {
        c('mean_squared_error', 'mean_absolute_error', 'mean_squared_logarithmic_error')
      } else if (input$mtype == "Classification") {
        c('categorical_crossentropy','binary_crossentropy','sparse_categorical_crossentropy', 'logcosh')
      }
      pickerInput(ns('loss'), 'loss', choices)
    })

    savereac<-reactive({
      #tf$keras$models$save_model(deep_model$df,dir_base)
      #saveRDS(vals,paste0(tempdir("teste"),"/teste.rds"))
      saveRDS(reactiveValuesToList(input),"input.rds")
      saveRDS(reactiveValuesToList(vals),"savepoint.rds")
      beep()
    })
    # UI para metrics
    output$o_metrics<-renderUI({
      data<-data_y_train()
      choices <- if (input$mtype == "Regression") {
        c('mean_absolute_error', 'mean_squared_error', 'mean_absolute_percentage_error')
      } else if (input$mtype == "Classification") {
        c('acc')
      }
      pickerInput(ns('metrics'), 'Metrics:', choices,selected=choices,  multiple = T)
    })
    output$compile_inputs<-renderUI({
      div(class='map_control_style2',
          h4("2.1. Compile parameters"),
          uiOutput(ns("o_loss")),
          uiOutput(ns("o_metrics")),
          uiOutput(ns("o_optimizer")),
      )
    })
    deep_model<-reactiveValues(df=NULL)
    initialize_model<-function(){
      if (!is.null(deep_model$df)) {
        gc()  # Coleta de lixo para liberar recursos
        deep_model$df<-NULL
      }
      train<-data_x_train()
      set.seed(input$seed)
      set_random_seed(input$seed)
      i<- getinputs(input)
      optimizer<-get_optimizer(input)  # Crie um novo otimizador
      model<-keras_model_sequential() %>%
        layer_dense(units = i[[1]]$units[1], activation = i[[1]]$activation[1], input_shape = ncol(train))
      for(j in 2:input$nlay){
        model %>% layer_dense(units = i[[j]]$units[1], activation = i[[j]]$activation[1])
      }
      model %>% compile(
        loss = input$loss,
        optimizer = optimizer,
        metrics = input$metrics
      )




    }
    getmodel<-reactive({
      set.seed(input$seed)
      deep_model$df<-NULL
      set_random_seed(input$seed)
      i<- getinputs(input)
      train<-data_x_train()
      model<-keras_model_sequential() %>%
        layer_dense(units = i[[1]]$units[1], activation = i[[1]]$activation[1], input_shape = ncol(train))
      for(j in 2:input$nlay){
        model<-model %>% layer_dense(units = i[[j]]$units[1], activation = i[[j]]$activation[1])
      }


      deep_model$df<-model
      model




    })
    observeEvent(input$build_model,{
      #savereac()
      withProgress(message="Building...",{
        getmodel()
      })


    })

    observeEvent(input$next_tab2,{
      updateTabsetPanel(session,inputId="tab",selected="tab2")
    })
    output$build_plot<-renderUI({
      req(!is.null(deep_model$df))
      div(h4("2.4. Preview"),
          div(
            style="position: absolute; top: 20px; right: 20px",
            actionButton(ns("next_tab2"), "Next >>")),
          splitLayout(
            renderPlot({
              plot_network(deep_model$df)
            })
          )
      )
    })
    output$build_out<-renderUI({
      req(input$nlay)
      renderPrint(
        getinputs(input)
      )
    })
    observeEvent(input$x_datalist,{
      vals$cur_data<-input$x_datalist
    })
    observeEvent(input$y_datalist,{
      vals$cur_dataY<-input$y_datalist
    })
    observeEvent(input$y_target,{
      vals$cur_response<-input$y_target
    })
    observeEvent(input$y_partition,{
      vals$cur_test_partition<-input$y_partition
    })
    observeEvent(input$y_partition_targ,{
      vals$cur_testdata<-input$y_partition_targ
    })
    observeEvent(input$save_teste,{
      savereac()
    })
    data_overwritte<-reactiveValues(df=F)
    data_store<-reactiveValues(df=F)
    newname<-reactiveValues(df=0)
    get_newname<-reactive({
      req(!is.null(vals$hand_save))
      newname$df<-switch(
        vals$hand_save,
        ##keras_model
        "Save keras_model model in"= { name_save_keras_model()},
        "Create Datalist with Predictions"= { name_pred_create()}
      )})
    name_pred_create<-reactive({
      base_name<-paste0("Pred_keras")
      #supervisor<-attr(deep_model$df,"supervisor")
      if(ncol(predictions_data.frame())==1){
        base_name=paste0(base_name,"_",colnames(predictions_data.frame()))
      }
      names<-makeUniqueCustom(c(names(vals$saved_data),base_name))
      names[length(names)]
    })
    name_save_pred_class<-reactive({
      names<-makeUniqueCustom(c(names(vals$saved_data),get_datalist_tosave()))
      names[length(names)]
    })
    name_save_keras_model<-reactive({
      name0<-paste0("keras_model")
      names<-makeUniqueCustom(c(names(attr(vals$saved_data[[get_datalist_tosave()]],"keras_model")),name0))

      names[length(names)]
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
        "Save keras_model model in"= {savekeras_model()},
        "Create Datalist with Predictions"={create_pred()})
      removeModal()

    })
    predictions_data.frame<-reactive({
      req(!is.null(cur_keras$pred))
      datao<-newdata_x()
      pred<-cur_keras$pred
      if(attr(deep_model$df,"modelType")=="Classification"){
        pred<-data.frame(pred)
        rownames(pred)=rownames(datao)
      } else{
        pred<-data.frame(pred)
        colnames(pred)<-  attr(deep_model$df,"train_labels")
        rownames(pred)=rownames(datao)
      }
      pred


    })

    get_datalist_with_predictions<-reactive({
      datao<-newdata_x()
      pred<-predictions_data.frame()
      if(attr(deep_model$df,"modelType")=="Classification"){
        newdatalist<-data_migrate(datao,pred,"")
        attr(newdatalist,"factors")$pred<-pred[rownames(newdatalist),]
      } else{
        newdatalist<-data_migrate(datao,pred,"")
        attr(newdatalist,"coords")
      }
      newdatalist

    })

    create_pred<-reactive({
      newdatalist<-get_datalist_with_predictions()
      vals$saved_data[[input$newdatalist]]<-newdatalist
    })
    module_keras_model <- function() {
      ns <- session$ns
      modalDialog(
        uiOutput(ns("databank_storage")),
        title=strong(icon("fas fa-save"),'Save'),
        footer=column(12,
                      uiOutput(ns('savekeras_model_teste')),
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
    ###
    ###

    get_kdf<-reactive({
      kms<-sapply(vals$saved_data, function(x) names(attr(x,"keras_model")))
      kmodels<-kms[sapply(kms,length)>0]
      req(length(kmodels)>0)
      kdf<-data.frame(reshape2::melt(kmodels))
      values<-apply(kdf,1,function(x) paste(x, collapse="-"))
      cleaned_values <- make.unique(gsub("[^A-Za-z]", "", values))
      colnames(kdf)<-c("model_name","datalist")
      kdf$value<-cleaned_values
      kdf
    })
    get_datalist_model_name<-reactive({
      kdf<-get_kdf()
      kdf$datalist[kdf$value%in%input$saved_keras]
    })

    get_model_name<-reactive({
      kdf<-get_kdf()
      kdf$model_name[kdf$value%in%input$saved_keras]
    })
    observeEvent(input$remove_model,{
      req(get_datalist_model_name())
      req(get_model_name())
      datalist<-get_datalist_model_name()
      model<-get_model_name()
      attr(vals$saved_data[[datalist]],"keras_model")[[model]]<-NULL
      if(!length(attr(vals$saved_data[datalist],"keras_model"))>0){
        deep_model$df<-NULL
      }
    })
    get_keras_models<-reactive({
      req(input$new_or_load)
      req(input$new_or_load=="saved_model")
      req(get_datalist_model_name())
      req(get_model_name())
      keras_models<-attr(vals$saved_data[[get_datalist_model_name()]],"keras_model")
      req(get_model_name()%in%names(keras_models))
      deep_model$df<-NULL
      bin<-keras_models[[get_model_name()]][[2]]
      req(length(bin)>0)
      req(class(bin)=="raw")

      # print(bin)
      bin
    })
    observeEvent(get_keras_models(),{

      cur_tab<-vals$cur_keras_tab
      updateTabsetPanel(session,inputId="tab",selected="tab3")
      withProgress(message="Loading",min=1,max=1,{
        bin<-get_keras_models()
        req(class(bin)=="raw")
        unzip_keras(bin)
        dir_base<-paste0("temp_keras","\\",get_datalist_model_name(),"\\",get_model_name())
        loaded_model <-  tf$keras$models$load_model(dir_base)
        at<-attr(vals$saved_data[[get_datalist_model_name()]],"keras_model")[[get_model_name()]][[1]]
        at_names<-c("data","supervisor","train_labels", "testdata","levels","test_data" ,"modelType",'metrics' )


        for(i in at_names ){
          attr(loaded_model,i)<-at[[i]]
        }
        deep_model$df<-loaded_model
      })
      updateTabsetPanel(session,inputId="tab",selected=cur_tab)
    })
    get_datalist_tosave<-reactive({
      if(input$new_or_load=="new_model"){
        input$x_datalist
      } else{
        get_datalist_model_name()
      }
    })
    output$o_model<-renderUI({
      #req(get_datalist_model_name())
      kdf<-get_kdf()
      tkdf<-data.frame(t(kdf))
      choicesNames<-lapply(tkdf,function(x) {
        re<-HTML(paste0(span(x[1],em(x[2], style="color: DodgerBlue; font-size: 10px"))))
      })
      pickerInput(ns("saved_keras"),label = NULL,choicesOpt = list(content =  choicesNames),choices=kdf$value, selected=vals$cur_keras_saved_keras)
    })
    observeEvent(input$saved_keras,{
      vals$cur_keras_saved_keras<-input$saved_keras
    })



  })
  }


