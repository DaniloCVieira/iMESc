
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
ui_supersom <- function(id){

  ns <- NS(id)
  tagList(
    includeCSS("inst/app/www/styles.css"),
    #actionLink(ns("teste_comb"),"SAVE"),
    div(
      #uiOutput(ns("bug")),
        uiOutput(ns("som_header")),
        uiOutput(ns("som_panels")))

  )

}


#' @export
server_supersom <- function (input, output, session,vals,df_colors,newcolhabs ){
  ns <- session$ns
  ns_som3 <- NS('predsom')
  ns_som2 <- NS('som2')
  ns_som <- NS('som')
  observeEvent(ignoreInit = T,input$teste_comb,{
    savereac()
  })




  output$som_panels<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))

    column(12,

           tabsetPanel(id = ns("som_tab"),selected=vals$cur_somtab,
                       tabPanel( value = "som_tab1",
                                 strong("1. Training"),
                                 uiOutput(ns("training_panel"))
                       ),
                       tabPanel(value = "som_tab2",
                                strong("2. Results"),
                                uiOutput(ns("results_panel"))),
                       tabPanel(value = "som_tab3",style="background: white",
                                strong("3. Predict"),
                                div(
                                  div(
                                    uiOutput(ns("predsom_panel"))
                                  ),
                                  div(
                                    class="map_control_style",

                                    uiOutput(ns("newdata_summ"))

                                  )
                                )
                       )
           )
    )})



  output$som_header<-renderUI({
    div(
      uiOutput(ns("som_inputs")),
      uiOutput(ns("som_models_panel")),
      uiOutput(ns('partition_panel'))
    )

  })


  output$som_inputs<-renderUI({
    #validate(need(length(vals$saved_data)>0,"No Datalist found"))
    column(8,style='choosechannel',
           div(class="well3",style = "height:210px;overflow-y: scroll; overflow-x: scroll;",
               inline(
                 div(
                   div(uiOutput(ns("supersom_switch"))),
                   div(style="vertical-align: text-top;display: table;",
                       inline(uiOutput(ns("data_somX_out"))),
                       inline(uiOutput(ns("som_table_panel"))),
                       inline(uiOutput(ns("save_som")))
                   ))
               )



           )
    )
  })
  output$som_models_panel<-renderUI({
    req(input$som_tab)
    style<-if(input$som_tab=="som_tab1"){
      "padding: 10px; display: none"
    } else{
      "padding: 10px; display: block"
    }
    column(4,class="well3",style=style,
           div(
             div(
               uiOutput(ns('som_models')),
               uiOutput(ns("som_cur"))
             )
           )
    )
  })




  output$partition_panel<-renderUI({
    req(input$som_tab)
    style=if(input$som_tab!="som_tab1"){
      'display: none; padding: 10px'
    } else{
      'display: block; padding: 10px'
    }

    column(4,style=style,class="well3",
           div(class="map_control_style2",style="color: #05668D",
               uiOutput(ns("partition_switch")),
               uiOutput(ns("partition_on"))
           )
    )
  })



  observe({
    if(is.null(vals$cur_train)){vals$cur_train<-"new som (unsaved)"}
  })

  output$som_models<-renderUI({
    req(input$som_tab)
    req(input$data_som)
    choices<-names(attr(vals$saved_data[[input$data_som]],"som"))
    div(
      div(
        div(strong("Som results:", tiphelp("SOM results. Click to see SOM results saved in the selected Datalist"))),
        div(
          inline(pickerInput(ns("som_models"),
                             NULL,
                             choices=choices,
                             selected=vals$cur_train, width='150px')),
          inline( bsButton(ns("som_model_delete"),icon("fas fa-trash-alt")))
        )
      )
    )
  })
  output$supersom_switch<-renderUI({
    req(!is.null(vals$cur_mysupersom))
    div(
      switchInput(ns("mysupersom"),"supersom",value=vals$cur_mysupersom,size="mini", inline=T,labelWidth="75px",handleWidth="30px")
    )
  })
  output$partition_switch<-renderUI({
    req(!is.null(vals$cur_usepartition))
    div(switchInput(ns("usepartition"),"Use partition",value=vals$cur_usepartition,size="mini", inline=T,labelWidth="90px",handleWidth="30px"))
  })
  output$partition_on<-renderUI({
    req(isTRUE(input$usepartition))
    div(
      column(12,style="width: 45%; display: inline-block; margin: 0px; padding: 0px",
             div(style='  white-space: nowrap;margin 0px; padding: 0px ',
                 div(class='shiny-input-container',
                     "Datalist:"),
                 div(class='shiny-input-container',
                     span("Partition:",tiphelp("choose a factor as reference for the partition"))),
                 div(class='shiny-input-container',
                     span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions")))
             )
      ),
      column(12,style="width: 55%;display: inline-block;; margin: 0px; padding: 0px",
             div(
               uiOutput(ns("data_somY_out")),
               uiOutput(ns("som_partition")),
               uiOutput(ns("som_partition_ref"))
             )
      )
    )
  })
  output$data_somY_out<-renderUI({
    div(
      pickerInput(ns("data_somY"),NULL,choices =  names(vals$saved_data),selected=vals$cur_somdataX_somY)
    )
  })
  output$som_partition<-renderUI({
    req(input$data_somY)
    req(input$data_som)
    if(is.null(vals$cur_partition_column)){vals$cur_partition_column<-1}
    div(
      pickerInput(ns("partition_column"),NULL, choices=c(colnames(attr(vals$saved_data[[input$data_somY]],"factors"))), selected=vals$cur_partition_column)
    )
  })
  output$som_partition_ref<-renderUI({
    req(isTRUE(input$usepartition))
    req(input$data_somY)
    req(input$partition_column)
    if(is.null(vals$cur_partition_ref)){vals$cur_partition_ref<-1}
    fac<-attr(vals$saved_data[[input$data_somY]],"factors")[,input$partition_column]
    choices<-levels(fac)
    pickerInput(ns("partition_ref"),NULL, choices=choices, selected=vals$cur_partition_ref)
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



    #teste<-readRDS("teste.rds")

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

  observe({
    req(input$som_tab)
    req(input$data_som)
    req(length(input$mysupersom)>0)

    if(isTRUE(input$mysupersom)){
      style='display: none'
      if(input$som_tab!="som_tab1"){
        style='display: block'
      }
      vals$showdataXsom<-style
      vals$showdataXsom_label<-span("Target Datalist",tiphelp("the supersom model will always be saved in the datalist selected in the first layer.",placement ="right"))
      vals$showdataXsom_X<-""
      return()
    }else{
      style='display: block'
      vals$showdataXsom<-style
      vals$showdataXsom_label<-"~ Training Datalist:"
      vals$showdataXsom_X<-"X:"
      return()
    }

  })



  observe({
    req(input$som_tab)
    if(input$som_tab!="som_tab1"){
      style='display: block'
      vals$showdataXsom<-style
    }

  })





  somval<-reactiveValues(df=F)




  output$saved_soms<-renderUI({
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

  output$som_table_panel<-renderUI({
    div(class=show_dataX(),
        div("~ Training layers"),
        column(12,uiOutput(ns('ssom_table')))
    )
  })

  #vals<-readRDS("savepoint.rds")
  #input<-readRDS("input.rds")





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


  output$save_som<-renderUI({
    req(input$som_models)
    req(input$som_models=="new som (unsaved)")
    req(input$som_tab!="som_tab1")
    popify(
      div(class="save_changes",
          bsButton(ns("tools_savesom"), div(icon("fas fa-save")),style  = "animation: glowing 1000ms infinite;", type="action",value=FALSE)
      )
      , NULL, "Save the som model in the Datalist"
    )
  })
  output$som_cur<-renderUI({
    req(input$som_tab!="som_tab1")
    div(
      div(strong("X:"), em(paste0(names(vals$som_results$data),collapse = "; "))),
      div(strong("Method:"),em(attr(vals$som_results,"Method"))),
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
  observeEvent(ignoreInit = T,input$som_tab,{
    somval$df_go<-F
    vals$cur_somtab_before<-vals$cur_somtab
    vals$cur_somtab<-input$som_tab
    somval$df_go<-T
  })



  observeEvent(list(input$som_part,input$data_som,input$mysupersom),{
    req(input$som_tab)
    req(length(input$mysupersom)>0)
    req(input$data_som)
    req(input$som_part)
    updateTabsetPanel(session,"som_tab","som_tab1")
  })










  observeEvent(ignoreInit = T,input$ssom_minus,{
    vals$ssom_tab0<-vals$ssom_tab0[-nrow(vals$ssom_tab0),]
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

  observe({
    req(length(input$supersom_reset)>0)
    if(is.null(vals$ssom_tab0)){
      vals$cur_somdataXlist_supersom<- NULL
      vals$cur_ssom_wei<-rep("0.5",2)
      vals$cur_ssom_dist<-rep("euclidean",2)
      vals$ssom_tab0<-getinit_supersom()
    }
  })

  observeEvent(ignoreInit = T,input$supersom_reset,{
    if(input$supersom_reset%%2){

      vals$cur_somdataXlist_supersom<- NULL
      vals$cur_ssom_wei<-rep("0.5",2)
      vals$cur_ssom_dist<-rep("euclidean",2)
      vals$ssom_tab0<-getinit_supersom()

    }
  })
  #vals<-readRDS("savepoint.rds")

  # saveRDS(vals,"savepoint.rds")




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


  output$ssom_df = renderUI({
    div(

      renderPrint(ssom_reac()),


    )
  })
  observeEvent(input$trainSOM,{
    req(isTRUE(input$mysupersom))
    train_supersom()
  })





  train_supersom<-reactive({
    try({

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


      withProgress(
        message = "Running som... the time taken will depend on the size of the data and the training.",
        min = 1,
        max = 1,
        {


          m<-try({
            if(is.na(input$seed)==F){set.seed(input$seed)}

            supersom2(
              data=training_list,
              #whatmap = 1,
              grid = kohonen::somgrid(
                input$xdim,
                input$ydim,
                topo = input$topo,
                toroidal = toroidal(),
                neighbourhood.fct=tunesom$neighbourhood.fct
              ),
              rlen = input$rlen,
              dist.fcts = distances,
              user.weights=weights,
              alpha = c(tunesom$a1, tunesom$a2),
              radius = c(tunesom$r1, tunesom$r2),
              mode = tunesom$mode,
              maxNA.fraction = tunesom$maxna,
              normalizeDataLayers=as.logical(input$normalizeDataLayers)
            )
          })



          if (class(m) != "kohonen")
          {
            validate(paste(m[[1]], "Please decrease your alpha (learning rate)"))
          }
          attr(m,'mode')<-input$mode
          names(m$unit.classif)<-rownames(m$data[[1]])
          attr(m,"Method")<-"superSOM"
          attr(m,"Datalist")<-names(m$data)
          attr(m,"test")<-test_list
          attr(m,"normalizeDataLayers")<-input$normalizeDataLayers
          vals$som_unsaved<-m
          attr(vals$saved_data[[input$data_som]],"som")[["new som (unsaved)"]]<-m
          beep(10)
          m


        }
      )


    })
  })



  symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
  df_symbol <- data.frame(
    val = c(16,15,17,18,8,1,5,3)
  )
  for(i in 1:length(symbols))
  {
    symbol1<-dataURI(file = paste0('inst/app/www/pch',i,".png"), mime = "image/png")

    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}


  observe({
    if(is.null(vals$cur_mysupersom)){vals$cur_mysupersom<-F}
  })

  observe({
    if(is.null(vals$cur_usepartition)){vals$cur_usepartition<-F}
  })





  observeEvent(ignoreInit = T,input$mysupersom,{
    vals$cur_mysupersom<-input$mysupersom
  })

  observeEvent(ignoreInit = T,input$usepartition,{
    vals$cur_usepartition<-input$usepartition
  })








  output$sompred_type<-renderUI({
    m<-vals$som_results
    req(class(m)=="kohonen")
    test<-attr(m,"test")

    div(
      "New data (X):",
      inline(span(

        if(class(test)=="list"){
          inline(radioButtons(ns("sompred_type"),NULL,choices=c("Partition","Training","Datalist"), inline=T, selected=vals$sompred_type, width="200px"))
        } else{inline( radioButtons(ns("sompred_type"),NULL,choices=c("Training","Datalist"),inline=T, selected="Datalist", width="200px") )}

      ))
    )
  })
  test_som<-reactive({
    req(input$sompred_type)

    pred_tab<-switch(input$sompred_type,
                     "Partition"={
                       attr(vals$som_results,"test")},
                     "Datalist"={vals$saved_data[[input$predsom_new]]},
                     "Training"={vals$som_results$data})

  })



  newdata_som<-reactive({
    req(input$sompred_type)
    m<-vals$som_results
    newdata<-switch(input$sompred_type,
                    "Partition"={
                      attr(m,"test")},
                    "Datalist"={
                      req(input$predsom_new)
                      vals$saved_data[[input$predsom_new]]
                    },
                    "Training"={vals$som_results$data})


    if(length(newdata)==1){
      newdata<-newdata[[1]]
    }


    newdata
  })

  get_sompred<-reactive({
    req(input$sompred_type)
    m<-vals$som_results
    newdata<-newdata_som()


    req(length(newdata)>0)
    req(class(m)=="kohonen")
    pred<-if(input$sompred_type%in%c("Training","Partition")){
      pred<-predict(m,newdata,trainingdata =newdata)
      names(pred$predictions)<-names(m$data)
      names(pred$unit.predictions)<-names(m$data)

      pred
    } else{
      req(input$whatmap)
      req(input$whatmap%in%names(m$data))
      mtemp<-m
      newdata_m<-as.matrix(newdata)

      names(newdata_m)<-input$whatmap
      reord<-names(mtemp$codes)==input$whatmap
      mtemp$codes<-mtemp$codes[c(names(mtemp$codes)[reord], names(mtemp$codes)[-reord])]
      mtemp$data<-mtemp$data[c(names(mtemp$data)[reord], names(mtemp$data)[-reord])]



      whatmap=if(length(m$data)==1){NULL} else{input$whatmap}
      pred<-predict(mtemp,newdata_m,trainingdata =newdata_m,
                    whatmap=whatmap)

      names(pred$predictions)<-input$whatmap
      names(pred$unit.predictions)<-input$whatmap

      pred

    }

    if(is.null(input$whatmap)){
      pred$whatmap<-input$layer_result
    } else{
      pred$whatmap<-input$whatmap
    }

    pred
  })



  output$pick_layer_result<-renderUI({
    req(input$sompred_type)
    req(input$sompred_type%in%c("Partition","Training"))
    req(input$predsom_results)
    req(input$predsom_results%in%c("predictions","unit.predictions"))
    m<-vals$som_results
    req(class(m)=="kohonen")
    choices=names(m$data)

    div(
      "Show layer:",
      pickerInput(ns("layer_result"),NULL,choices=choices, selected=vals$cur_layer_result, width="150px")
    )
  })

  observeEvent(ignoreInit = T,input$predsom_tab,
               vals$cur_predsom_tab<-input$predsom_tab)
  output$predom_tabs<-renderUI({

    tabsetPanel(id=ns("predsom_tab"),selected= vals$cur_predsom_tab,
                tabPanel(
                  strong("3.1. Results"),style="background: white",
                  value = "predsom_tab01",
                  uiOutput(ns("pred_som_eval"))
                ),
                tabPanel(
                  strong("3.2. Performace"),style="background: white",
                  value = "predsom_tab01b",
                  uiOutput(ns("pred_performace"))
                ),
                tabPanel(
                  strong("3.3. BMUs"),style="background: white",
                  value = "predsom_tab01c",
                  uiOutput(ns("plot_predbmu"))
                )

    )
  })



  output$pred_bmu00<-renderUI({
    renderPlot({
      plot(get_som_model_pred(), shape="straight",type ="mapping")
    })
  })











  output$plot_predbmu<-renderUI({

    sidebarLayout(
      sidebarPanel(uiOutput(ns("predbmu_plot_side"))),
      mainPanel(uiOutput(ns("predbmu_plot")))
    )
  })
  output$bmu_plot_side<-renderUI({

    # req(input$model_or_data=="som codebook")
    div(class="map_control_style",style="color: #05668D",
        uiOutput(ns("ss1_side_somplot")),
        uiOutput(ns("bmu_down_links")))
  })



  {
    output$ss1_side_somplot<-renderUI({



      div(class="map_control_style",style="color: #05668D",
          div(


            uiOutput(ns('ss1_umapro')),
            uiOutput(ns("ss1_hc_ordcluster")),
            uiOutput(ns('ss1_hc_palette')),


            div(
              style='border-bottom: 1px solid gray',
              uiOutput(ns('ss1_out_pclus_addpoints')),
              div(
                style="margin-left: 10px",
                uiOutput(ns("ss1_pclus_points_inputs"))
              )
            ),

            div(style='border-bottom: 1px solid gray',
                uiOutput(ns('ss1_out_pclus_addtext')),
                div(
                  style="margin-left: 10px",
                  uiOutput(ns("ss1_pclus_text_inputs"))
                )),
            div(
              uiOutput(ns("ss1_vfm_check"))
            ),
            uiOutput(ns("ss1_showerrors_som")),
            uiOutput(ns("ss1_theme")),
            uiOutput(ns("ss1_title"))


          ))
    })
    output$ss1_hc_palette<-renderUI({
      choices<-ss1_get_choices_pal()
      title<-attr(choices,"title")
      div(style='border-bottom: 1px solid gray;',
          div(class="palette",
              title,
              pickerInput(inputId = ns("ss1_bg_palette"),
                          label =NULL,
                          choices =  vals$colors_img$val[choices],
                          selected=vals$somplot_bg,
                          choicesOpt = list(
                            content =  vals$colors_img$img[choices] ),
                          options=list(container="body"), width="100px")),
          uiOutput(ns("ss1_pcodes_bgalpha"))

      )
    })
    output$ss1_out_pclus_addpoints<-renderUI({
      if(is.null(vals$pclus_addpoints)){
        vals$pclus_addpoints<-T
      }
      checkboxInput(ns("ss1_pclus_addpoints"),"+ Points",
                    value=vals$pclus_addpoints)
    })
    output$ss1_out_pclus_addtext<-renderUI({
      if(is.null(vals$pclus_text_inputs)){
        vals$pclus_text_inputs<-F
      }
      checkboxInput(ns("ss1_pclus_addtext"),"+ Labels",
                    value=vals$pclus_addtext)
    })
    output$ss1_title<-renderUI({
      div("+ Title: ",
          inline(textInput(ns("ss1_title"), NULL, "", width="200px"))
      )
    })
    output$ss1_theme<-renderUI({
      if(is.null(vals$theme)){
        vals$theme<-F
      }
      div(style='border-bottom: 1px solid gray',
          inline(checkboxInput(ns("ss1_theme"),
                               label = "+ show neuron coordinates",
                               value=vals$theme,
                               width="200px"
          ))
      )
    })
    output$ss1_umapro<-renderUI({


      div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',
          inline(uiOutput(ns('ss1_somback_value')))
      )
    })
    output$ss1_plus_umatrix <- renderUI({
      if(is.null(vals$plus_umatrix)){vals$plus_umatrix<-F}
      checkboxInput(ns("ss1_plus_umatrix"),strong("+ U-Matrix:"), value=vals$plus_umatrix)
    })
    observeEvent(ignoreInit = T,input$ss1_somback_value,{
      vals$ss1_somback_value<-input$ss1_somback_value
    })
    output$ss1_somback_value <- renderUI({



      div(
        style="font-size: 14px; margin-bottom: 5px",
        div(strong("+ Background value:"), style="margin-bottom: 5px; height: 24px"),
        radioButtons(ns("ss1_somback_value"),NULL,list("None"="None","U-Matrix"="uMatrix","Property"="property"), selected=vals$ss1_somback_value),
        uiOutput(ns("ss1_var_pproperty"))

      )
    })
    output$ss1_var_pproperty<-renderUI({
      req(input$ss1_somback_value=="property")

      div(style="width: 250px;margin-left: 25px; margin-bottom: 10px",class='small_picker',
          div(style="","Layer:",inline(uiOutput(ns('ss1_property_layer')))),
          div(style="width: 300px",
              "Variable:",
              inline(
                div(
                  inline(uiOutput(ns('ss1_prev_property'))),
                  inline(
                    uiOutput(ns('ss1_out_variable_pproperty'))),
                  inline(uiOutput(ns('ss1_next_property')))

                )
              ))


      )
    })
    observeEvent(ignoreInit = T,input$ss1_property_layer,{
      vals$ss1_property_layer<-input$ss1_property_layer
    })
    output$ss1_property_layer<-renderUI({
      choices = names(getsom()$data)
      req(length(choices)>0)
      pickerInput(ns("ss1_property_layer"),
                  label = NULL,
                  choices = choices,
                  selected=vals$ss1_property_layer
      )
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
    output$ss1_out_variable_pproperty<-renderUI({
      choices<-colnames(ss1_getdata_layer())
      div(id="ssom_property",
          pickerInput(ns("ss1_variable_pproperty"),
                      label = NULL,
                      choices = choices,
                      selected=vals$variable_pproperty
          )
      )
    })
    output$ss1_prev_property<-renderUI({
      data = ss1_getdata_layer()
      req(which(colnames(data)==input$ss1_variable_pproperty)!=1)
      actionButton(ns("ss1_prev_property"),"<<")
    })
    output$ss1_next_property<-renderUI({
      data = ss1_getdata_layer()
      req(which(colnames(data)!=input$ss1_variable_pproperty)==ncol(data))
      actionButton(ns("ss1_next_property"),">>")
    })
    output$ss1_pclus_points_inputs<-renderUI({
      req(isTRUE(input$ss1_pclus_addpoints))
      div(
        div("+ Palette",inline(uiOutput(ns("ss1_pclus_points_palette")))),
        div("+ Factor",inline(uiOutput(ns("ss1_pclus_points_factor_out")))),
        div("+ Shape",inline(uiOutput(ns("ss1_pclus_points_shape")))),
        div("+ Size",inline(uiOutput(ns("ss1_pclus_points_size"))))
      )
    })
    output$ss1_pclus_points_palette<-renderUI({

      if(is.null(vals$pclus_points_palette)){vals$pclus_points_palette<-"black"}
      inline(
        tipify(pickerInput(inputId = ns("ss1_pclus_points_palette"),
                           label =NULL,
                           choices = vals$colors_img$val,
                           choicesOpt = list(content = vals$colors_img$img),
                           selected=vals$pclus_points_palette,
                           options=list(container="body"), width="75px"),
               "Symbol colors"
        )
      )
    })
    output$ss1_pclus_points_factor_out<-renderUI({
      req(input$ss1_pclus_points_palette)
      choices<-c(colnames(attr(vals$saved_data[[input$data_som]],"factors")))


      inline(
        pickerInput(ns("ss1_pclus_points_factor"),NULL,
                    choices = c(choices),selected=vals$pclus_points_factor,width="150px")
      )


    })
    output$ss1_pclus_points_shape<-renderUI({
      tipify(pickerInput(inputId = ns("ss1_pclus_symbol"),
                         label = NULL,
                         choices = df_symbol$val,
                         choicesOpt = list(content = df_symbol$img),
                         options=list(container="body"), width="100px",
                         selected=vals$pclus_symbol)
             ,"symbol shape")
    })
    output$ss1_pclus_points_size<-renderUI({
      if(is.null(vals$pclus_points_size)){vals$pclus_points_size<-1}
      inline(
        tipify(numericInput(ns("ss1_pclus_points_size"),NULL,value = vals$pclus_points_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
      )
    })
    output$ss1_pclus_text_inputs<-renderUI({
      req(isTRUE(input$ss1_pclus_addtext))
      div(
        div("+ Palette",inline(uiOutput(ns("ss1_pclus_text_palette")))),
        div("+ Factor",inline(uiOutput(ns("ss1_pclus_text_factor_out")))),
        div("+ Size",inline(uiOutput(ns("ss1_pclus_text_size"))))
      )
    })
    output$ss1_pclus_text_palette<-renderUI({

      if(is.null(vals$pclus_text_palette)){vals$pclus_text_palette<-"black"}
      inline(
        tipify(pickerInput(inputId = ns("ss1_pclus_text_palette"),
                           label =NULL,
                           choices =  vals$colors_img$val[getsolid_col()],
                           selected=vals$pclus_text_palette,
                           choicesOpt = list(
                             content =  vals$colors_img$img[getsolid_col()] ),
                           options=list(container="body"), width="75px"),
               "Symbol colors"
        )
      )
    })
    output$ss1_pclus_text_factor_out<-renderUI({
      req(input$ss1_pclus_text_palette)
      choices<-c(colnames(attr(vals$saved_data[[input$data_som]],"factors")))
      inline(
        pickerInput(ns("ss1_pclus_text_factor"),NULL,
                    choices = c(choices),selected=vals$pclus_text_factor,width="150px")
      )


    })
    output$ss1_pclus_text_size<-renderUI({
      if(is.null(vals$pclus_text_size)){vals$pclus_text_size<-1}
      inline(
        tipify(numericInput(ns("ss1_pclus_text_size"),NULL,value = vals$pclus_text_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
      )
    })
    output$ss1_vfm_check<-renderUI({
      if(is.null(vals$pclus_varfacmap_action)){vals$pclus_varfacmap_action<-T}
      div(style='border-bottom: 1px solid gray',
          span("+ ",
               inline(checkboxInput(ns("ss1_varfacmap_action"), span("Variable factor map",actionLink(ns("ss1_varfacmap"), tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value =vals$pclus_varfacmap_action, width="100px"))),
          div(style="margin-left: 10px",uiOutput(ns("ss1_varfac_out")))
      )
    })
    output$ss1_varfac_out<-renderUI({
      req(isTRUE(input$ss1_varfacmap_action))
      if(is.null(vals$pclus_border)){
        vals$pclus_border<-"white"
      }
      div(
        uiOutput(ns('ss1_vfm_type_out')),
        uiOutput(ns('ss1_npic_out')))})
    output$ss1_pcodes_bgalpha<-renderUI({
      if(is.null(vals$pclus_border)){vals$pclus_border<-"white"}
      if(is.null(vals$pcodes_bgalpha)){

        vals$pcodes_bgalpha<-0


      }
      if(is.null(vals$base_size)){
        vals$base_size<-12
      }
      div(span(
        "+ Background lightness",inline(
          tipify(numericInput(ns("ss1_pcodes_bgalpha"),NULL,value = vals$pcodes_bgalpha,min = 0,max = 1,step = .1, width="75px"),"symbol size")
        )
      ),
      div(span(span('+ Border:'),inline(
        div(class="palette", pickerInput(ns("ss1_pclus_border"),
                                         label =NULL,
                                         choices =  vals$colors_img$val[getsolid_col()] ,
                                         choicesOpt = list(
                                           content =  vals$colors_img$img[getsolid_col()] ),
                                         selected= vals$pclus_border, width = '75px'))
      ))),

      div(
        "+ Base size",inline(
          tipify(numericInput(ns("ss1_base_size"),NULL,value = vals$base_size, width="75px"),"symbol size")
        )
      )
      )
    })
    output$ss1_vfm_type_out<-renderUI({
      my_choices<-list("Highest"='var', "Clockwise"="cor")

      div(span("+ Show correlation:",inline(
        pickerInput(ns("ss1_vfm_type"),NULL,
                    choices =my_choices,
                    selected=vals$vfm_type,
                    width="150px"
        ))))
    })
    output$ss1_npic_out<-renderUI({
      if(is.null(vals$npic)){vals$npic<-10}
      if(is.null(vals$pclus.cex.var)){vals$pclus.cex.var=1}
      if(is.null(vals$p.clus.col.text)){vals$p.clus.col.text<-"black"}
      if(is.null(vals$var_bg)){vals$var_bg<-"white"}
      if(is.null(vals$var_bg_transp)){vals$var_bg_transp=0}
      div(
        div(
          span("+ Number",
               inline(
                 tipify(
                   numericInput(ns("ss1_npic"), NULL, value = vals$npic, min = 2, width="75px"),"Number of variables to display"
                 )))
        ),
        div(
          span("+ Var size",
               inline(
                 numericInput(ns("ss1_pclus.cex.var"), NULL, value = vals$pclus.cex.var, min = 2, width="75px")))
        ),
        div(
          span("+ Var text color",
               inline(
                 div(class="palette",
                     pickerInput(inputId = ns("ss1_p.clus.col.text"),
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
                     pickerInput(inputId = ns("ss1_var_bg"),
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
                   numericInput(ns("ss1_var_bg_transp"), NULL, value = vals$var_bg_transp, min = 2, width="75px"),"Number of variables to display"
                 )))
        )
      )
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
      bp<-getbp_som(m=m,indicate=iind$indicate,npic=iind$npic,hc=vals$cutsom)
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


      req(length(input$ss1_somback_value)>0)
      title="+ Background palette"
      if(input$ss1_somback_value=="None"){
        vals$somplot_bg<-"gray"
        choices=getsolid_col()
      } else {

        vals$somplot_bg<-"viridis"
        choices=getgrad_col()

      }


      attr(choices,"title")<-title
      choices
    })
    observeEvent(ignoreInit = T,input$ss1_hc_ord_factor,{
      vals$hc_ord_factor<-input$ss1_hc_ord_factor
    })
    observeEvent(ignoreInit = T,input$ss1_hc_ord_datalist,{
      vals$hc_ord_datalist<-input$ss1_hc_ord_datalist
    })
    observeEvent(ignoreInit = T,input$ss1_hc_sort,{
      vals$hc_sort<-input$ss1_hc_sort
    })
    observeEvent(ignoreInit = T,input$ss1_theme,{
      vals$theme<-input$ss1_theme
    })
    observeEvent(ignoreInit = T,input$ss1_pclus_addtext,{
      vals$pclus_addtext<-input$ss1_pclus_addtext
    })
    observeEvent(ignoreInit = T,input$ss1_pclus_addpoints,{
      vals$pclus_addpoints<-input$ss1_pclus_addpoints
    })
    observeEvent(ignoreInit = T,input$ss1_pp_labels,{
      vals$pp_labels<-input$ss1_pp_labels
    })
    observeEvent(ignoreInit = T,input$ss1_next_property,{
      data = ss1_getdata_layer()
      pnext<-colnames(data)[which(colnames(data)==input$ss1_variable_pproperty)+1]
      updateSelectInput(session,'ss1_variable_pproperty',selected=pnext)

    })
    observeEvent(ignoreInit = T,input$ss1_prev_property,{
      data = ss1_getdata_layer()
      pprev<-colnames(data)[which(colnames(data)==input$ss1_variable_pproperty)-1]
      updateSelectInput(session,'ss1_variable_pproperty',selected=pprev)
    })
    observeEvent(ignoreInit = T,input$ss1_variable_pproperty,{
      vals$variable_pproperty<-input$ss1_variable_pproperty
    })
    observeEvent(ignoreInit = T,input$ss1_round_error,{
      vals$round_error<-input$ss1_round_error
    })
    observeEvent(ignoreInit = T,input$ss1_show_mapcode_errors,{
      vals$show_mapcode_errors<-input$ss1_show_mapcode_errors
    })
    observeEvent(ignoreInit = T,input$ss1_teste_comb,{
      savereac()
    })
    observeEvent(ignoreInit = T,input$ss1_bg_palette,{
      vals$somplot_bg<-input$ss1_bg_palette
    })
    observeEvent(ignoreInit = T,input$ss1_pclus_text_palette,{
      vals$pclus_text_palette<-input$ss1_pclus_text_palette
    })
    observeEvent(ignoreInit = T,input$ss1_pclus_text_factor,{
      vals$pclus_text_factor<-input$ss1_pclus_text_factor
    })
    observeEvent(ignoreInit = T,input$ss1_pclus_border,{
      vals$pclus_border<-input$ss1_pclus_border
    })
    observeEvent(ignoreInit = T,input$ss1_vfm_type,{
      vals$vfm_type<-input$ss1_vfm_type
    })
    observeEvent(ignoreInit = T,input$ss1_npic,{
      vals$npic<-input$ss1_npic
    })
    observeEvent(ignoreInit = T,input$ss1_pclus.cex.var,{
      vals$pclus.cex.var<-input$ss1_pclus.cex.var
    })
    observeEvent(ignoreInit = T,input$ss1_p.clus.col.text,{
      vals$p.clus.col.text<-input$ss1_p.clus.col.text
    })
    observeEvent(ignoreInit = T,input$ss1_var_bg,{
      vals$var_bg<-input$ss1_var_bg
    })
    observeEvent(ignoreInit = T,input$ss1_var_bg_transp,{
      vals$var_bg_transp.alpha<-input$ss1_var_bg_transp
    })
    observeEvent(ignoreInit = T,input$ss1_pclus_points_palette,{
      vals$pclus_points_palette<-input$ss1_pclus_points_palette
    })
    observeEvent(ignoreInit = T,input$ss1_insertx_pclus,{
      vals$insertx_pclus<-input$ss1_insertx_pclus
    })
    observeEvent(ignoreInit = T,input$ss1_inserty_pclus,{
      vals$inserty_pclus<-input$ss1_inserty_pclus
    })
    observeEvent(ignoreInit = T,input$ss1_ncol_pclus,{
      vals$ncol_pclus<-input$ss1_ncol_pclus
    })
    observeEvent(ignoreInit = T,input$ss1_bgleg_pclus,{
      vals$bgleg_pclus<-input$ss1_bgleg_pclus
    })
    observeEvent(ignoreInit = T,input$ss1_pclus_symbol,{
      vals$pclus_symbol<-input$ss1_pclus_symbol
    })
    observeEvent(ignoreInit = T,input$ss1_dot_label_clus,{
      vals$dot_label_clus<-input$ss1_dot_label_clus
    })
    observeEvent(ignoreInit = T,input$ss1_varfacmap_action,{
      vals$pclus_varfacmap_action<-input$ss1_varfacmap_action
    })
    observeEvent(ignoreInit = T,input$ss1_pclus_points_size,{
      vals$pclus_points_size<-input$ss1_pclus_points_size
    })
    observeEvent(ignoreInit = T,input$ss1_pcodes_bgalpha,{
      vals$pcodes_bgalpha<-input$ss1_pcodes_bgalpha
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


      m<-vals$som_results

      tryco<-try(ss1_copoints_scaled(), silent = T)

      req(class(tryco)!='try-error')
      #savereac()
      trybp<-try( ss1_bp_som(), silent = T)

      req(class(trybp)!='try-error')

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
      #saveRDS(args,"args.rds")
      # args<-readRDS("args.rds")
      #req(length(args)>0)
      bp<-args$bp
      bp$id=NULL
      vals$biplot_som<-bp
      args$points_palette

      args
      vals$bmus_plot<-do.call(bmu_plot,args)
      vals$bmus_plot
    })

  })
  output$predbmu_plot_side<-renderUI({

    # req(input$model_or_data=="som codebook")
    div(class="map_control_style",style="color: #05668D",
        uiOutput(ns("ss2_side_somplot")),
        uiOutput(ns("predbmu_down_links")))
  })

  {output$ss2_side_somplot<-renderUI({



    div(class="map_control_style",style="color: #05668D",
        div(


          uiOutput(ns('ss2_umapro')),
          uiOutput(ns("ss2_hc_ordcluster")),
          uiOutput(ns('ss2_hc_palette')),


          div(
            style='border-bottom: 1px solid gray',
            uiOutput(ns('ss2_out_pclus_addpoints')),
            div(
              style="margin-left: 10px",
              uiOutput(ns("ss2_pclus_points_inputs"))
            )
          ),

          div(style='border-bottom: 1px solid gray',
              uiOutput(ns('ss2_out_pclus_addtext')),
              div(
                style="margin-left: 10px",
                uiOutput(ns("ss2_pclus_text_inputs"))
              )),
          div(
            uiOutput(ns("ss2_vfm_check"))
          ),
          uiOutput(ns("ss2_showerrors_som")),
          uiOutput(ns("ss2_theme")),
          uiOutput(ns("ss2_title"))


        ))
  })
    output$ss2_hc_palette<-renderUI({
      choices<-ss2_get_choices_pal()
      title<-attr(choices,"title")
      div(style='border-bottom: 1px solid gray;',
          div(class="palette",
              title,
              pickerInput(inputId = ns("ss2_bg_palette"),
                          label =NULL,
                          choices =  vals$colors_img$val[choices],
                          selected=vals$somplot_bg,
                          choicesOpt = list(
                            content =  vals$colors_img$img[choices] ),
                          options=list(container="body"), width="100px")),
          uiOutput(ns("ss2_pcodes_bgalpha"))

      )
    })
    output$ss2_out_pclus_addpoints<-renderUI({
      if(is.null(vals$pclus_addpoints)){
        vals$pclus_addpoints<-T
      }
      checkboxInput(ns("ss2_pclus_addpoints"),"+ Points",
                    value=vals$pclus_addpoints)
    })
    output$ss2_out_pclus_addtext<-renderUI({
      if(is.null(vals$pclus_text_inputs)){
        vals$pclus_text_inputs<-F
      }
      checkboxInput(ns("ss2_pclus_addtext"),"+ Labels",
                    value=vals$pclus_addtext)
    })
    output$ss2_title<-renderUI({
      div("+ Title: ",
          inline(textInput(ns("ss2_title"), NULL, "", width="200px"))
      )
    })
    output$ss2_theme<-renderUI({
      if(is.null(vals$theme)){
        vals$theme<-F
      }
      div(style='border-bottom: 1px solid gray',
          inline(checkboxInput(ns("ss2_theme"),
                               label = "+ show neuron coordinates",
                               value=vals$theme,
                               width="200px"
          ))
      )
    })
    output$ss2_umapro<-renderUI({


      div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',
          inline(uiOutput(ns('ss2_somback_value')))
      )
    })
    output$ss2_plus_umatrix <- renderUI({
      if(is.null(vals$plus_umatrix)){vals$plus_umatrix<-F}
      checkboxInput(ns("ss2_plus_umatrix"),strong("+ U-Matrix:"), value=vals$plus_umatrix)
    })
    observeEvent(ignoreInit = T,input$ss2_somback_value,{
      vals$ss2_somback_value<-input$ss2_somback_value
    })
    output$ss2_somback_value <- renderUI({



      div(
        style="font-size: 14px; margin-bottom: 5px",
        div(strong("+ Background value:"), style="margin-bottom: 5px; height: 24px"),
        radioButtons(ns("ss2_somback_value"),NULL,list("None"="None","U-Matrix"="uMatrix","Property"="property"), selected=vals$ss2_somback_value),
        uiOutput(ns("ss2_var_pproperty"))

      )
    })
    output$ss2_var_pproperty<-renderUI({
      req(input$ss2_somback_value=="property")

      div(style="width: 250px;margin-left: 25px; margin-bottom: 10px",class='small_picker',
          div(style="","Layer:",inline(uiOutput(ns('ss2_property_layer')))),
          div(style="width: 300px",
              "Variable:",
              inline(
                div(
                  inline(uiOutput(ns('ss2_prev_property'))),
                  inline(
                    uiOutput(ns('ss2_out_variable_pproperty'))),
                  inline(uiOutput(ns('ss2_next_property')))

                )
              ))


      )
    })
    observeEvent(ignoreInit = T,input$ss2_property_layer,{
      vals$ss2_property_layer<-input$ss2_property_layer
    })
    output$ss2_property_layer<-renderUI({
      choices = names(get_som_model_pred()$data)
      req(length(choices)>0)
      pickerInput(ns("ss2_property_layer"),
                  label = NULL,
                  choices = choices,
                  selected=vals$ss2_property_layer
      )
    })
    ss2_getdata_layer<-reactive({
      choices0 = names(get_som_model_pred()$data)
      if(length(choices0)>0){
        req(input$ss2_property_layer)
        get_som_model_pred()$data[[input$ss2_property_layer]]
      } else{
        get_som_model_pred()$data[[1]]
      }

    })
    output$ss2_out_variable_pproperty<-renderUI({
      choices<-colnames(ss2_getdata_layer())
      div(id="ssom_property",
          pickerInput(ns("ss2_variable_pproperty"),
                      label = NULL,
                      choices = choices,
                      selected=vals$variable_pproperty
          )
      )
    })
    output$ss2_prev_property<-renderUI({
      data = ss2_getdata_layer()
      req(which(colnames(data)==input$ss2_variable_pproperty)!=1)
      actionButton(ns("ss2_prev_property"),"<<")
    })
    output$ss2_next_property<-renderUI({
      data = ss2_getdata_layer()
      req(which(colnames(data)!=input$ss2_variable_pproperty)==ncol(data))
      actionButton(ns("ss2_next_property"),">>")
    })
    output$ss2_pclus_points_inputs<-renderUI({
      req(isTRUE(input$ss2_pclus_addpoints))
      div(
        div("+ Palette",inline(uiOutput(ns("ss2_pclus_points_palette")))),
        div("+ Factor",inline(uiOutput(ns("ss2_pclus_points_factor_out")))),
        div("+ Shape",inline(uiOutput(ns("ss2_pclus_points_shape")))),
        div("+ Size",inline(uiOutput(ns("ss2_pclus_points_size"))))
      )
    })
    output$ss2_pclus_points_palette<-renderUI({

      if(is.null(vals$pclus_points_palette)){vals$pclus_points_palette<-"black"}
      inline(
        tipify(pickerInput(inputId = ns("ss2_pclus_points_palette"),
                           label =NULL,
                           choices = vals$colors_img$val,
                           choicesOpt = list(content = vals$colors_img$img),
                           selected=vals$pclus_points_palette,
                           options=list(container="body"), width="75px"),
               "Symbol colors"
        )
      )
    })
    output$ss2_pclus_points_factor_out<-renderUI({
      req(input$ss2_pclus_points_palette)
      choices<-c(colnames(attr(vals$saved_data[[input$data_som]],"factors")))


      inline(
        pickerInput(ns("ss2_pclus_points_factor"),NULL,
                    choices = c(choices),selected=vals$pclus_points_factor,width="150px")
      )


    })
    output$ss2_pclus_points_shape<-renderUI({
      tipify(pickerInput(inputId = ns("ss2_pclus_symbol"),
                         label = NULL,
                         choices = df_symbol$val,
                         choicesOpt = list(content = df_symbol$img),
                         options=list(container="body"), width="100px",
                         selected=vals$pclus_symbol)
             ,"symbol shape")
    })
    output$ss2_pclus_points_size<-renderUI({
      if(is.null(vals$pclus_points_size)){vals$pclus_points_size<-1}
      inline(
        tipify(numericInput(ns("ss2_pclus_points_size"),NULL,value = vals$pclus_points_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
      )
    })
    output$ss2_pclus_text_inputs<-renderUI({
      req(isTRUE(input$ss2_pclus_addtext))
      div(
        div("+ Palette",inline(uiOutput(ns("ss2_pclus_text_palette")))),
        div("+ Factor",inline(uiOutput(ns("ss2_pclus_text_factor_out")))),
        div("+ Size",inline(uiOutput(ns("ss2_pclus_text_size"))))
      )
    })
    output$ss2_pclus_text_palette<-renderUI({

      if(is.null(vals$pclus_text_palette)){vals$pclus_text_palette<-"black"}
      inline(
        tipify(pickerInput(inputId = ns("ss2_pclus_text_palette"),
                           label =NULL,
                           choices =  vals$colors_img$val[getsolid_col()],
                           selected=vals$pclus_text_palette,
                           choicesOpt = list(
                             content =  vals$colors_img$img[getsolid_col()] ),
                           options=list(container="body"), width="75px"),
               "Symbol colors"
        )
      )
    })
    output$ss2_pclus_text_factor_out<-renderUI({
      req(input$ss2_pclus_text_palette)
      choices<-c(colnames(attr(vals$saved_data[[input$data_som]],"factors")))
      inline(
        pickerInput(ns("ss2_pclus_text_factor"),NULL,
                    choices = c(choices),selected=vals$pclus_text_factor,width="150px")
      )


    })
    output$ss2_pclus_text_size<-renderUI({
      if(is.null(vals$pclus_text_size)){vals$pclus_text_size<-1}
      inline(
        tipify(numericInput(ns("ss2_pclus_text_size"),NULL,value = vals$pclus_text_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
      )
    })
    output$ss2_vfm_check<-renderUI({
      if(is.null(vals$pclus_varfacmap_action)){vals$pclus_varfacmap_action<-T}
      div(style='border-bottom: 1px solid gray',
          span("+ ",
               inline(checkboxInput(ns("ss2_varfacmap_action"), span("Variable factor map",actionLink(ns("ss2_varfacmap"), tipify(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value =vals$pclus_varfacmap_action, width="100px"))),
          div(style="margin-left: 10px",uiOutput(ns("ss2_varfac_out")))
      )
    })
    output$ss2_varfac_out<-renderUI({
      req(isTRUE(input$ss2_varfacmap_action))
      if(is.null(vals$pclus_border)){
        vals$pclus_border<-"white"
      }
      div(
        uiOutput(ns('ss2_vfm_type_out')),
        uiOutput(ns('ss2_npic_out')))})
    output$ss2_pcodes_bgalpha<-renderUI({
      if(is.null(vals$pclus_border)){vals$pclus_border<-"white"}
      if(is.null(vals$pcodes_bgalpha)){

        vals$pcodes_bgalpha<-0


      }
      if(is.null(vals$base_size)){
        vals$base_size<-12
      }
      div(span(
        "+ Background lightness",inline(
          tipify(numericInput(ns("ss2_pcodes_bgalpha"),NULL,value = vals$pcodes_bgalpha,min = 0,max = 1,step = .1, width="75px"),"symbol size")
        )
      ),
      div(span(span('+ Border:'),inline(
        div(class="palette", pickerInput(ns("ss2_pclus_border"),
                                         label =NULL,
                                         choices =  vals$colors_img$val[getsolid_col()] ,
                                         choicesOpt = list(
                                           content =  vals$colors_img$img[getsolid_col()] ),
                                         selected= vals$pclus_border, width = '75px'))
      ))),

      div(
        "+ Base size",inline(
          tipify(numericInput(ns("ss2_base_size"),NULL,value = vals$base_size, width="75px"),"symbol size")
        )
      )
      )
    })
    output$ss2_vfm_type_out<-renderUI({
      my_choices<-list("Highest"='var', "Clockwise"="cor")

      div(span("+ Show correlation:",inline(
        pickerInput(ns("ss2_vfm_type"),NULL,
                    choices =my_choices,
                    selected=vals$vfm_type,
                    width="150px"
        ))))
    })
    output$ss2_npic_out<-renderUI({
      if(is.null(vals$npic)){vals$npic<-10}
      if(is.null(vals$pclus.cex.var)){vals$pclus.cex.var=1}
      if(is.null(vals$p.clus.col.text)){vals$p.clus.col.text<-"black"}
      if(is.null(vals$var_bg)){vals$var_bg<-"white"}
      if(is.null(vals$var_bg_transp)){vals$var_bg_transp=0}
      div(
        div(
          span("+ Number",
               inline(
                 tipify(
                   numericInput(ns("ss2_npic"), NULL, value = vals$npic, min = 2, width="75px"),"Number of variables to display"
                 )))
        ),
        div(
          span("+ Var size",
               inline(
                 numericInput(ns("ss2_pclus.cex.var"), NULL, value = vals$pclus.cex.var, min = 2, width="75px")))
        ),
        div(
          span("+ Var text color",
               inline(
                 div(class="palette",
                     pickerInput(inputId = ns("ss2_p.clus.col.text"),
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
                     pickerInput(inputId = ns("ss2_var_bg"),
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
                   numericInput(ns("ss2_var_bg_transp"), NULL, value = vals$var_bg_transp, min = 2, width="75px"),"Number of variables to display"
                 )))
        )
      )
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
      m<-get_som_model_pred()
      bp<-getbp_som(m=m,indicate=iind$indicate,npic=iind$npic,hc=vals$cutsom)
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



      m<-get_som_model_pred()
      hexs<-get_neurons(m,background_type=backtype,property=property, hc=NULL)
      vals$ss2_hc_network<-hexs
      hexs
    })
    ss2_get_copoints<-reactive({
      m<-get_som_model_pred()
      copoints<-getcopoints(m)
      vals$copoints_hc<-copoints
      copoints
    })
    ss2_copoints_scaled<-reactive({
      ss2_get_network()
      ss2_get_copoints()
      points_tomap=rescale_copoints(hexs=vals$ss2_hc_network,copoints=vals$copoints_hc)
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
      title="+ Background palette"
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
    observeEvent(ignoreInit = T,input$ss2_hc_ord_factor,{
      vals$hc_ord_factor<-input$ss2_hc_ord_factor
    })
    observeEvent(ignoreInit = T,input$ss2_hc_ord_datalist,{
      vals$hc_ord_datalist<-input$ss2_hc_ord_datalist
    })
    observeEvent(ignoreInit = T,input$ss2_hc_sort,{
      vals$hc_sort<-input$ss2_hc_sort
    })
    observeEvent(ignoreInit = T,input$ss2_theme,{
      vals$theme<-input$ss2_theme
    })
    observeEvent(ignoreInit = T,input$ss2_pclus_addtext,{
      vals$pclus_addtext<-input$ss2_pclus_addtext
    })
    observeEvent(ignoreInit = T,input$ss2_pclus_addpoints,{
      vals$pclus_addpoints<-input$ss2_pclus_addpoints
    })
    observeEvent(ignoreInit = T,input$ss2_pp_labels,{
      vals$pp_labels<-input$ss2_pp_labels
    })
    observeEvent(ignoreInit = T,input$ss2_next_property,{
      data = ss2_getdata_layer()
      pnext<-colnames(data)[which(colnames(data)==input$ss2_variable_pproperty)+1]
      updateSelectInput(session,'ss2_variable_pproperty',selected=pnext)

    })
    observeEvent(ignoreInit = T,input$ss2_prev_property,{
      data = ss2_getdata_layer()
      pprev<-colnames(data)[which(colnames(data)==input$ss2_variable_pproperty)-1]
      updateSelectInput(session,'ss2_variable_pproperty',selected=pprev)
    })
    observeEvent(ignoreInit = T,input$ss2_variable_pproperty,{
      vals$variable_pproperty<-input$ss2_variable_pproperty
    })
    observeEvent(ignoreInit = T,input$ss2_round_error,{
      vals$round_error<-input$ss2_round_error
    })
    observeEvent(ignoreInit = T,input$ss2_show_mapcode_errors,{
      vals$show_mapcode_errors<-input$ss2_show_mapcode_errors
    })
    observeEvent(ignoreInit = T,input$ss2_teste_comb,{
      savereac()
    })
    observeEvent(ignoreInit = T,input$ss2_bg_palette,{
      vals$somplot_bg<-input$ss2_bg_palette
    })
    observeEvent(ignoreInit = T,input$ss2_pclus_text_palette,{
      vals$pclus_text_palette<-input$ss2_pclus_text_palette
    })
    observeEvent(ignoreInit = T,input$ss2_pclus_text_factor,{
      vals$pclus_text_factor<-input$ss2_pclus_text_factor
    })
    observeEvent(ignoreInit = T,input$ss2_pclus_border,{
      vals$pclus_border<-input$ss2_pclus_border
    })
    observeEvent(ignoreInit = T,input$ss2_vfm_type,{
      vals$vfm_type<-input$ss2_vfm_type
    })
    observeEvent(ignoreInit = T,input$ss2_npic,{
      vals$npic<-input$ss2_npic
    })
    observeEvent(ignoreInit = T,input$ss2_pclus.cex.var,{
      vals$pclus.cex.var<-input$ss2_pclus.cex.var
    })
    observeEvent(ignoreInit = T,input$ss2_p.clus.col.text,{
      vals$p.clus.col.text<-input$ss2_p.clus.col.text
    })
    observeEvent(ignoreInit = T,input$ss2_var_bg,{
      vals$var_bg<-input$ss2_var_bg
    })
    observeEvent(ignoreInit = T,input$ss2_var_bg_transp,{
      vals$var_bg_transp.alpha<-input$ss2_var_bg_transp
    })
    observeEvent(ignoreInit = T,input$ss2_pclus_points_palette,{
      vals$pclus_points_palette<-input$ss2_pclus_points_palette
    })
    observeEvent(ignoreInit = T,input$ss2_insertx_pclus,{
      vals$insertx_pclus<-input$ss2_insertx_pclus
    })
    observeEvent(ignoreInit = T,input$ss2_inserty_pclus,{
      vals$inserty_pclus<-input$ss2_inserty_pclus
    })
    observeEvent(ignoreInit = T,input$ss2_ncol_pclus,{
      vals$ncol_pclus<-input$ss2_ncol_pclus
    })
    observeEvent(ignoreInit = T,input$ss2_bgleg_pclus,{
      vals$bgleg_pclus<-input$ss2_bgleg_pclus
    })
    observeEvent(ignoreInit = T,input$ss2_pclus_symbol,{
      vals$pclus_symbol<-input$ss2_pclus_symbol
    })
    observeEvent(ignoreInit = T,input$ss2_dot_label_clus,{
      vals$dot_label_clus<-input$ss2_dot_label_clus
    })
    observeEvent(ignoreInit = T,input$ss2_varfacmap_action,{
      vals$pclus_varfacmap_action<-input$ss2_varfacmap_action
    })
    observeEvent(ignoreInit = T,input$ss2_pclus_points_size,{
      vals$pclus_points_size<-input$ss2_pclus_points_size
    })
    observeEvent(ignoreInit = T,input$ss2_pcodes_bgalpha,{
      vals$pcodes_bgalpha<-input$ss2_pcodes_bgalpha
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


      m<-vals$som_results

      tryco<-try(ss2_copoints_scaled(), silent = T)

      req(class(tryco)!='try-error')
      #savereac()
      trybp<-try( ss2_bp_som(), silent = T)

      req(class(trybp)!='try-error')

      errors<-NULL



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

  output$predbmu_plot<-renderUI({


    #args<-readRDS("args.rds")

    renderPlot({

      args<-ss2_argsplot()
      #saveRDS(args,"args.rds")

      req(length(args)>0)
      bp<-args$bp
      bp$id=NULL
      vals$biplot_som<-bp

      vals$pred_bmus_plot<-do.call(bmu_plot,args)
      vals$pred_bmus_plot
    })

  })

  output$plot_bmu<-renderUI({
    sidebarLayout(
      sidebarPanel(uiOutput(ns("bmu_plot_side"))),
      mainPanel(uiOutput(ns("bmu_plot")))
    )
  })



  output$pred_performace<-renderUI({
    fluidPage(

      column(class="side_results",
             12, offset = 0,
             navlistPanel(
               header=uiOutput(ns('header_perf')),
               id=ns("get_predsom_perf"),

               tabPanel(
                 value="predsom_perf_overhall",
                 title = "Overall performace",
                 DT::dataTableOutput(ns("overhall_peform"))),
               tabPanel(
                 value="predsom_perf_obs",
                 title = "Performace by observation",
                 DT::dataTableOutput(ns("obs_peform"))
               ),
               tabPanel(
                 value="predsom_perf_var",
                 title = "Performace by variable",
                 DT::dataTableOutput(ns("var_peform"))
               )
             )


      )

    )


  })


  output$obs_peform<-DT::renderDataTable({
    vals$som_perf_result<-table<-get_obs_peform()
    DT::datatable(

      table, options = list(

        autoWidth=T,
        lengthMenu = list(c(15, -1), c( "15","All")),
        scrollY = "250px",
        fixedHeader=TRUE,
        scrollX="300px",


        fixedColumns = list(leftColumns = 1, rightColumns = 0)
      ),
      extensions = c('FixedColumns',"FixedHeader"),
      class ='cell-border compact stripe')




  })


  output$var_peform<-DT::renderDataTable({
    vals$som_perf_result<-table<-get_var_peform()
    DT::datatable(

      table, options = list(

        autoWidth=T,
        lengthMenu = list(c(15, -1), c( "15","All")),
        scrollY = "250px",
        fixedHeader=TRUE,
        scrollX="300px",


        fixedColumns = list(leftColumns = 1, rightColumns = 0)
      ),
      extensions = c('FixedColumns',"FixedHeader"),
      class ='cell-border compact stripe')




  })

  output$overhall_peform<-DT::renderDataTable({
    vals$som_perf_result<-table<-get_overhall_peform()
    DT::datatable(

      table, options = list(dom="t"),
      class ='cell-border compact stripe')

  })





  output$predsom_panel<-renderUI({

    column(12,
           column(8,class="well3",

                  div(class="map_control_style",style="color: #05668D",
                      uiOutput(ns('sompred_type')),
                      uiOutput(ns('sompred_type_datalist'))


                  )
           ),
           column(4,class="well3",
                  uiOutput(ns("newdata_info"))
           ),
           column(12,uiOutput(ns("predom_tabs")))

    )

  })

  output$newdata_info<-renderUI({


    resM<-get_newdata_infto()
    req(is.data.frame(resM))
    div(
      div(em("new data:"),resM[1,1]),
      div(em("nrows:"),resM[3,1]),
      div(em("ncols:"),resM[2,1])
    )




  })


  observeEvent(ignoreInit = T,input$whatmap,{
    vals$cur_whatmap<-input$whatmap
  })


  getobs_whatmap<-reactive({
    req(input$sompred_type)
    req(input$sompred_type=="Datalist")
    m<-vals$som_results
    newdata<-newdata_som()

    datalist<-m$data
    j=1
    x<-datalist[[1]]
    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(newdata)
        sum(res)==ncol(newdata)
      })
    )
    names(res0[res0==T])
  })

  output$whatmap<-renderUI({
    m<-vals$som_results
    req(class(m)=="kohonen")
    div(style="display: none",
        inline(
          pickerInput(ns("whatmap"),NULL,choices=names(m$data[getobs_whatmap()]), selected=vals$cur_whatmap, width="250px")
        ))
  })

  output$sompred_type_datalist<-renderUI({
    req(input$sompred_type)
    req(input$sompred_type =='Datalist')
    splitLayout(div(
      inline(
        div(style="width: 70px",
            div("Datalist:")
        )
      ),
      #getobs_somX
      inline(
        div(
          div(
            inline(
              pickerInput(ns("predsom_new"),NULL,choices=names(vals$saved_data[getobs_somX()]), selected=vals$predsom_new, width="250px")
            )),

          uiOutput(ns('whatmap'))

        )
      )
    ))
  })




  observeEvent(ignoreInit = T,input$predsom_results,{
    vals$cur_predsom_results<-input$predsom_results
  })


  output$pred_som_eval<-renderUI({
    div(
      uiOutput(ns('som_predict_results'))
    )
  })


  output$result_predictions<-DT::renderDataTable({
    req(input$predsom_results)
    res<-get_sompred()
    if(is.null(res$predictions)){
      res$predictions<-res$data
    }
    vals$som_predict_result<-table<-res[["predictions"]][[pic_result()]]
    DT::datatable(
      table, options = list(
        autoWidth=T,
        lengthMenu = list(c(15, -1), c( "15","All")),
        scrollY = "250px",
        fixedHeader=TRUE,
        scrollX="300px",
        fixedColumns = list(leftColumns = 1, rightColumns = 0)
      ),
      extensions = c('FixedColumns',"FixedHeader"),
      class ='cell-border compact stripe')
  })
  output$result_bmu<-DT::renderDataTable({
    req(input$predsom_results)
    res<-get_sompred()
    if(is.null(res$predictions)){
      res$predictions<-res$data
    }
    res_temp<-data.frame(bmu=res[['unit.classif']])
    rownames(res_temp)<-rownames(res[["predictions"]][[1]])
    res<-res_temp
    vals$som_predict_result<-table<-res

    DT::datatable(
      table, options = list(
        autoWidth=T,
        lengthMenu = list(c(15, -1), c( "15","All")),
        scrollY = "250px",
        fixedHeader=TRUE,
        scrollX="300px",
        fixedColumns = list(leftColumns = 1, rightColumns = 0)
      ),
      extensions = c('FixedColumns',"FixedHeader"),
      class ='cell-border compact stripe')
  })

  output$result_codebook<-DT::renderDataTable({
    req(input$predsom_results)
    res<-get_sompred()
    if(is.null(res$predictions)){
      res$predictions<-res$data
    }
    res<-res[["unit.predictions"]][[pic_result()]]
    rownames(res)<-paste0("neu",1:nrow(res))
    vals$som_predict_result<-table<-res

    DT::datatable(
      table, options = list(
        autoWidth=T,
        lengthMenu = list(c(15, -1), c( "15","All")),
        scrollY = "250px",
        fixedHeader=TRUE,
        scrollX="300px",
        fixedColumns = list(leftColumns = 1, rightColumns = 0)
      ),
      extensions = c('FixedColumns',"FixedHeader"),
      class ='cell-border compact stripe')
  })


  output$header_results<-renderUI({
    div(style="margin-bottom: 15px",
        inline(uiOutput(ns("pick_layer_result"))),
        inline(
          div(
            div(
              actionLink(ns('downtable_predict_result'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-table"))))
            ),
            uiOutput(ns("link_predsom_newdata")),
            uiOutput(ns("link_predsom_codebook"))
          )
        )
    )
  })

  output$header_perf<-renderUI({
    div(style="margin-bottom: 15px",
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
    req(input$predsom_results=="predictions")
    actionLink(ns('link_predsom_newdata_create'),span("+ Create a Datalist"))
  })
  output$link_predsom_codebook<-renderUI({
    req(input$predsom_results=="unit.predictions")
    actionLink(ns('link_predsom_codebook_create'),span("+ Create a Datalist"))
  })

  observeEvent(ignoreInit = T,input$link_predsom_perf_obs_create,{
    vals$hand_save<-"Create Datalist  - SOM performace"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_som())
  })
  create_predsom_codebook<-reactive({
    temp<-data.frame(vals$som_predict_result)
    attr(temp,"factors")<-data.frame(id=rownames(temp))
    coords<-data.frame(vals$som_results$grid$pts)
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
        vals$saved_data[[input$data_som]][rownames(attr(vals$som_results,"test")[[1]]),]
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
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, name=paste(input$data_som,input$get_predsom_perf, sep="_"))

  })

  observeEvent(ignoreInit = T,input$downtable_predict_result,{
    vals$hand_down<-"som_predict_result"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, name=paste(input$data_som,input$predsom_results,input$layer_result, sep="_"))

  })

  output$som_predict_results<-renderUI({

    fluidPage(
      column(
        class="side_results",
        12, offset = 0,
        navlistPanel(
          header=uiOutput(ns("header_results")),
          id=ns("predsom_results"),selected=vals$cur_predsom_results,
          tabPanel(
            title = "Best-matching units",
            value="unit.classif",
            DT::dataTableOutput(ns("result_bmu"))),
          tabPanel(
            title = "New Data (X)",
            value="predictions",
            DT::dataTableOutput(ns("result_predictions"))
          ),
          tabPanel(
            title = "Codebook",
            value="unit.predictions",
            DT::dataTableOutput(ns("result_codebook"))
          )
        ))
    )

  })


  pic_result<-reactive({
    req(input$sompred_type)
    if(input$sompred_type=="Datalist"){
      req(input$whatmap)
      input$whatmap
    } else{
      req(input$layer_result)
      input$layer_result
    }
  })



  observeEvent(ignoreInit = T,input$layer_result,{
    vals$cur_layer_result<-input$layer_result
  })











  observeEvent(ignoreInit = T,input$predsom_results,{
    vals$cur_predsom_results<-input$predsom_results
  })

  observeEvent(ignoreInit = T,input$partition_column,{
    vals$cur_partition_column<-input$partition_column
  })
  observeEvent(ignoreInit = T,input$partition_ref,{
    vals$cur_partition_ref<-input$partition_ref
  })

  output$geosom_print<-renderUI({
    renderPrint(input$geosom)
  })

  output$geosom_switch<-renderUI({
    data = getdata_som()
    coords<-attr(data,"coords")
    if(length(coords)>0){
      value=vals$cur_geosom
      disabled=F
    } else{
      value=F
      disabled=T
    }
    div(inline(switchInput(ns("geosom"),"GeoSOM", value=vals$cur_geosom,disabled=disabled,  onStatus = "success",size ="mini",inline=T)),inline(uiOutput(ns('k_geosom'))))
  })

  observeEvent(ignoreInit = T,input$geosom,{
    vals$cur_geosom<-input$geosom
  })

  output$k_geosom<-renderUI({
    req(isTRUE(input$geosom))
    if(is.null(vals$cur_k_geosom)){
      vals$cur_k_geosom<-0.1
    }
    div(class="map_control_style",style="color: #05668D",
        numericInput(ns('k_geosom'),'K',vals$cur_k_geosom, width="100px")
    )
  })

  observeEvent(ignoreInit = T,input$k_geosom,{
    vals$cur_k_geosom<-input$k_geosom
  })
  palette=c(
    "turbo",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"
  )
  symbols<-c("pch1","pch2","pch3","pch4")

  dfcolors <- data.frame(
    val = palette
  )

  ns<-session$ns

  tunesom<-reactiveValues(
    normalizeDataLayers=T,
    finetopo=F,
    finesom=F,
    dim=c(5,5),
    seed=NA,
    rlen=500,
    distmethod="BrayCurtis",
    toroidal="FALSE",
    neighbourhood.fct='gaussian',
    a1=0.05,
    a2=0.01,
    r1=0,
    r2=0,
    mode="online",
    maxna=0.001,
    topo="hexagonal",
    sugtopo=T
  )
  saved_sompred<-reactiveValues()
  re<-reactiveValues(df=NULL)
  cur_predsom_res1<-reactiveValues(df="Data predictions (X)")
  cur_predsom_res2<-reactiveValues(df="Data predictions (X)")
  cur_predsom_res3<-reactiveValues(df="Data predictions (X)")
  predsom_war<-reactiveValues(df=F)
  bmu_p_bgpalette<-reactiveValues(df=dfcolors$val[16])
  bmu_bgpalette<-reactiveValues(df=dfcolors$val[16])
  bmu_p_training<-reactiveValues(df=dfcolors$val[1])
  bmu_facpalette<-reactiveValues(df=dfcolors$val[11])
  bmu_p_test<-reactiveValues(df=dfcolors$val[14])
  bmu_p_border_grid<-reactiveValues(df=dfcolors$val[4])
  bmu_border_grid<-reactiveValues(df=dfcolors$val[4])
  bmu_p_dotlabel<-reactiveValues(df='symbols')

  bmu_p_symbol<-reactiveValues(df= df_symbol$val[1])
  bmu_symbol<-reactiveValues(df= df_symbol$val[1])
  output$showgrid <- renderUI({
    dim = try(topo.reactive(),silent=T )
    if(class(dim)=="try-error"){updateCheckboxInput(session,"sugtopo",value=F)}
    validate(need(class(dim)!="try-error","Error: suggested topology has been disabled; check for inconsistency in data, such as columns with variance 0. "))

    renderPlot({
      if(isTRUE(input$splitdata_som)){data=training_data$df}else{
        data = getdata_som()}
      validate(need(input$xdim!="", ""))
      validate(need(input$ydim!="", ""))
      validate(need( (input$xdim*input$ydim)<=nrow(data), "The number of map units must be less than or equal to the number of observations. Please decrease the 'xdim' and/or 'ydim' dimensions"))
      try({
        par(mar = c(0, 0, 0, 0))

        grid<-kohonen::somgrid(input$xdim, input$ydim, topo = input$topo, neighbourhood.fct=tunesom$neighbourhood.fct, toroidal=toroidal())
        plot.som_grid(grid)
      },silent=T )
    },
    width = 200,
    height = 200)
  })
  output$var_pproperty <- renderUI({
    data = data.frame(vals$saved_data[[input$data_som]],"factors")
    column(
      12,
      selectInput(ns("variable_pproperty"),
                  label = "select a variable",
                  choices = colnames(data),
                  selected=vals$variable_pproperty
      )
    )
  })

  observeEvent(ignoreInit = T,input$variable_pproperty,{
    vals$variable_pproperty<-input$variable_pproperty
  })
  observeEvent(ignoreInit = T,input$npic,{
    vals$npic<-input$npic
  })
  observeEvent(ignoreInit = T,input$vfm_type,{
    vals$vfm_type<-input$vfm_type
  })
  output$vfm_type_out<-renderUI({
    my_choices<-list("Highest"='var', "Clockwise"="cor","Cluster"="cor_hc")


    div(span("+ Show correlation:",inline(
      pickerInput(ns("vfm_type"),NULL,
                  choices =my_choices,
                  selected=somplot_args$vfm_type,
                  width="150px"
      ))))
  })
  output$npic_out<-renderUI({
    if(is.null(vals$npic)){vals$npic<-10}
    div(
      span("+ Number",
           inline(
             tipify(
               numericInput(ns("npic"), NULL, value = vals$npic, min = 2, width="75px"),"Number of variables to display"
             )))
    )
  })
  output$varfac_out<-renderUI({
    req(isTRUE(input$varfacmap_action))
    column(12,
           uiOutput(ns('vfm_type_out')),
           uiOutput(ns('npic_out')))})


  output$bmu_shape<-renderUI({
    req(input$bmu_dotlabel=='symbols')
    if(is.null(vals$bmu_symbol)){vals$bmu_symbol<-df_symbol$val[1]}

    div(span("+ Shape",inline(
      tipify(
        pickerInput(ns("bmu_symbol"),
                    label = NULL,
                    choices = df_symbol$val,
                    choicesOpt = list(content = df_symbol$img),

                    selected=vals$bmu_symbol, width="75px")
        ,"symbol shape"
      )
    )))
  })
  observeEvent(ignoreInit = T,input$bmu_symbol,{
    vals$bmu_symbol<-input$bmu_symbol
  })
  observeEvent(ignoreInit = T,input$varfacmap_action,
               vals$varfacmap_action<-input$varfacmap_action)
  observeEvent(ignoreInit = T,input$bmu_dotlabel,{
    vals$bmu_dotlabel<-input$bmu_dotlabel
  })
  observeEvent(ignoreInit = T,input$bmu_facpalette,
               vals$bmu_facpalette<-input$bmu_facpalette)
  observeEvent(ignoreInit = T,input$bmu_symbol_size,{
    vals$bmu_symbol_size<-input$bmu_symbol_size
  })
  output$vfm_check<-renderUI({
    if(is.null(vals$varfacmap_action)){vals$varfacmap_action<-T}
    div(
      span("+ ",
           inline(checkboxInput(ns("varfacmap_action"), span("Variable factor map",actionLink(ns("varfacmap"), tipify(icon("fas fa-question-circle"), "Click for more details"))),value =vals$varfacmap_action, width="100px")))
    )
  })
  output$bmu_display_out<-renderUI({
    if(is.null(vals$bmu_dotlabel)){vals$bmu_dotlabel<-'symbols'}
    div(span("+ Display:",inline(
      radioButtons(
        ns("bmu_dotlabel"),NULL , choices = c("labels", "symbols"),selected=vals$bmu_dotlabel,inline=T, width="100px")
    )))
  })
  output$bmu_facpalette_out<-renderUI({
    if(is.null(vals$bmu_facpalette)){vals$bmu_facpalette<-dfcolors$val[11]}
    div(span("+ Obs color:",inline(
      tipify(
        pickerInput(ns("bmu_facpalette"),
                    label =NULL,
                    choices = vals$colors_img$val,
                    choicesOpt = list(content = vals$colors_img$img),
                    selected=vals$bmu_facpalette, width="75px"),
        "Symbol colors. Choose a gradient to color observations by a factor"
      )
    )))
  })

  output$bmu_symbol_size_out<-renderUI({
    div(span("+ Size:",
             inline(
               tipify(numericInput(ns("bmu_symbol_size"),strong(),value = vals$bmu_symbol_size,min = 0.1,max = 3,step = .1, width="75px"),"symbol size")
             )))
  })
  output$bmu_bgpalette_out<-renderUI({
    if(is.null(vals$bmu_bgpalette)){vals$bmu_bgpalette<-dfcolors$val[16]}
    div(span("+ Background",
             inline(
               tipify(
                 pickerInput(ns("bmu_bgpalette"),
                             label = NULL,
                             choices = vals$colors_img$val,
                             choicesOpt = list(content = vals$colors_img$img),

                             selected=vals$bmu_bgpalette, width="75px"),
                 "Color of grid units"
               )
             )
    ))
  })
  observeEvent(ignoreInit = T,input$bmu_bgpalette,
               vals$bmu_bgpalette<-input$bmu_bgpalette)


  output$bmu_bg_transp_out<-renderUI({
    div(span("+ Transparency:",
             inline( tipify(
               numericInput(ns("bmu_bg_transp"),NULL,
                            value=vals$bmu_bg_transp, min=0, max=1,step=0.1, width="75px"),
               "Background transparency"))
    ))
  })
  output$bmu_border_grid_out<-renderUI({
    if(is.null(vals$bmu_border_grid)) {vals$bmu_border_grid<-vals$colors_img$val[getsolid_col()] [7]}
    div(span("+ Border", inline(
      tipify(
        pickerInput(ns("bmu_border_grid"),
                    label =NULL,
                    choices =  vals$colors_img$val[getsolid_col()] ,
                    choicesOpt = list(
                      content =  vals$colors_img$img[getsolid_col()] ),
                    selected= vals$bmu_border_grid, width="75px"),
        "Grid border color"
      )
    )))
  })
  output$bmu_var_color_out<-renderUI({
    if(is.null(vals$bmu_var_color)){vals$bmu_var_color<-"black"}
    div(span("+ Var color:",inline(
      tipify(
        pickerInput(ns("bmu_var_color"),
                    label =NULL,
                    choices =  vals$colors_img$val[getsolid_col()] ,
                    choicesOpt = list(
                      content =  vals$colors_img$img[getsolid_col()]), width="75px",
                    options=list(container="body"),
                    selected=vals$bmu_var_color
        ),
        "Variable color"
      )
    )))
  })

  output$bmu_cexvar_out<-renderUI({
    div(span("+ Var size",
             inline(
               tipify(numericInput(ns("bmu_cexvar"),NULL,value = vals$bmu_cexvar,min = 0.1,max = 3,step = .1, width="75px"),"variable text size (only for the variable factor map)")
             )))
  })

  output$bmu_down_links<-renderUI({
    div(
      div(
        tipify(
          actionLink(
            ns('downp_bmu'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-image")))
          ),
          "download BMU plot",     options=list(container="body")
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
  output$pop_pcorr<-renderUI({


    div(class="map_control_style",style="color: #05668D",
        uiOutput(ns("vfm_check")),
        uiOutput(ns("varfac_out")),
        br(),
        uiOutput(ns("bmu_display_out")),
        uiOutput(ns("bmu_facpalette_out")),
        uiOutput(ns("bmu_fac_control")),
        uiOutput(ns("bmu_shape")),
        uiOutput(ns("bmu_symbol_size_out")),
        uiOutput(ns("bmu_bgpalette_out")),
        uiOutput(ns("bmu_bg_transp_out")),
        uiOutput(ns("bmu_border_grid_out")),
        uiOutput(ns("bmu_var_color_out")),
        uiOutput(ns("bmu_cexvar_out")),
        uiOutput(ns("bmu_legend_out")),
        uiOutput(ns("bmu_down_links"))
    )

  })
  observeEvent(ignoreInit = T,input$bmu_var_color,{
    vals$bmu_var_color<-input$bmu_var_color
  })
  output$bmu_legend_out<-renderUI({
    req(input$bmu_dotlabel=="symbols")
    col<-getcolhabs(vals$newcolhabs,input$bmu_facpalette,2)
    req(col[1]!=col[2])

    div(
      actionLink(ns("bmu_legend") ,h5("+ Legend adjustment:",style="color: blue")),
      uiOutput(ns("pcorr_legcontrol"))
    )
  })
  output$pcorr_legcontrol<-renderUI({
    #req(isTRUE(vals$bmuleg))
    column(12,
           div(span("+ leg x",inline(
             tipify(numericInput(ns("bmu_insertx"),NULL,value=vals$bmu_insertx,step=0.05, width="75px"),"legend position relative to the x location")
           ))),
           div(span("+ leg y",
                    inline(
                      tipify(numericInput(ns("bmu_inserty"),NULL,value=vals$bmu_inserty,step=0.05, width="75px"),"legend position relative to the y location")
                    ))),
           div(span("+ ncol leg",
                    inline(
                      tipify(numericInput(ns("bmu_ncol"),NULL,value=vals$bmu_ncol,step=1, width="75px"),"the number of columns in which to set the legend items")
                    ))),
           div(span("+ bg leg",
                    inline(
                      tipify(numericInput(ns("bmu_leg_transp"),NULL,value=vals$bmu_leg_transp,step=0.05, max=1, width="75px"),"Legend background transparency")
                    )))
    )


  })
  observeEvent(ignoreInit = T,input$bmu_leg_transp,{
    vals$bmu_leg_transp<-input$bmu_leg_transp
  })
  observeEvent(ignoreInit = T,input$bmu_bg_transp,{
    vals$bmu_bg_transp<-input$bmu_bg_transp
  })


  observeEvent(ignoreInit = T,input$bmu_ncol,{
    vals$bmu_ncol<-input$bmu_ncol
  })
  observeEvent(ignoreInit = T,input$bmu_insertx,{
    vals$bmu_insertx<-input$bmu_insertx
  })
  observeEvent(ignoreInit = T,input$bmu_insertx,{
    vals$bmu_inserty<-input$bmu_inserty
  })

  output$pcorr_control <- renderUI({
    column(12,style="background: white",
           p(strong(h4("Best matching units"))),
           sidebarLayout(
             sidebarPanel(uiOutput(ns("pop_pcorr"), width=300)),
             mainPanel(
               div( plotOutput(ns("pCorrCodes")),
                    #renderPrint({
                    #m=vals$som_results
                    #grid<-m$grid$pts
                    #dtopo<-unit.distances(m$grid)
                    #dcodes<-object.distances(m,"codes")
                    #res<-unlist(lapply(codes, function (x)  sum(dist(x)/dist(dtopo))))
                    #sort(res, dec=T)})

               )
             )
           ))
  })
  observeEvent(ignoreInit = T,input$bmu_factors,{
    vals$bmu_factors<-input$bmu_factors
  })
  output$bmu_fac_control<-renderUI({
    req(input$bmu_facpalette)
    req(input$bmu_dotlabel)
    col<-getcolhabs(vals$newcolhabs,input$bmu_facpalette,2)
    if(input$bmu_dotlabel=='labels'|col[1]!=col[2])
    {span("+ Labels",
          inline(
            tipify(pickerInput(ns("bmu_factors"),NULL,
                               choices = c(colnames(attr(vals$saved_data[[input$data_som]],"factors","factors"))), selected=vals$bmu_factors, width="150px"),"color observations by factor")
          )
    )
    }})
  output$pCorrCodes <- renderPlot({
    #req(input$varfacmap_action)
    #req(input$npic)
    p<-BMUs()
    vals$pcorr_results<-data.frame(attr(p,"result"))
    p
  })
  output$pchanges <- renderPlot({
    pchanges(vals$som_results)
  })
  output$pcounts <- renderPlot({
    pcounts(vals$som_results)
  })
  output$pUmatrix <- renderPlot({
    pUmatrix(vals$som_results)
  })
  output$pproperty <- renderPlot({
    req(input$variable_pproperty)
    pproperty(vals$som_results,
              input$variable_pproperty,
              input$variable_pproperty)
    vals$pprop_plot<-recordPlot()
  })
  observeEvent(ignoreInit = T,input$somgridh,{
    output$somgridhelp <- renderText({
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
             p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("clock-wise correlations")," returns",code("npic")," variables with the highest correlation considering along the different directions of the codebook")
      )

    )
    )

  })
  observeEvent(ignoreInit = T,input$varfacmap, {
    showModal(modalDialog(
      uiOutput(ns("textvarfacmap")),
      title = h4(strong("Variable factor map")),
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    ))
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

  output$side_predsom_cm<-renderUI({
    div(class="map_control_style",style="color: #05668D",
        div(class="palette",
            span("+ Palette:",
                 inline(
                   pickerInput(ns("cm_supsom_palette"),
                               label = NULL,
                               choices =     vals$colors_img$val[getgrad_col()],
                               choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"),width='100px')
                 )
            )
        ),
        div(
          span("+ Y Datalist:",tipify(icon("fas fa-question-circle"),"Data containing observed values to be compared with predicted values",
                                      options=list(container="body")),
               inline(
                 pickerInput(ns("predsom_newY"),
                             NULL,names(vals$saved_data[getobs_som()])
                             ,width='100px')
               )
          )
        ),
        div(
          span("+ Y variable:",
               inline(
                 pickerInput(ns("predsom_y"),NULL, choices=rev(names(pred_som()$predictions[-1])),width='100px')
               )
          )
        ),
        div(
          actionLink(
            ns('downp_confsom'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
          )

        )
    )
  })






  output$bmu_p_tools<-renderUI({
    column(12,
           div(
             popify(bsButton(ns("downp_bmu_pred"), span(icon("fas fa-download"),icon("fas fa-image")),style = "button_active"),NULL,"download BMU plot",
                    options=list(container="body")

             )
           )
    )
  })
  output$pred_bmu0<-renderUI({
    column(12,style="background: white",
           p(strong(h4("Predictions - Best matching units"))),
           uiOutput(ns("bmu_p_tools")),
           sidebarLayout(
             sidebarPanel(
               uiOutput(ns("pop_bmu_p"))
             ),  mainPanel(
               plotOutput(ns("bmu_pCodes"))
             )
           ))


  })
  observeEvent(ignoreInit = T,input$bmu_p_factors,{
    vals$bmu_p_factors<-input$bmu_p_factors
  })
  output$bmup_labels<-renderUI({
    req(input$bmu_p_dotlabel)
    req(input$bmu_p_dotlabel=='labels')
    div(span("+ Labels:",inline(
      pickerInput(ns("bmu_p_factors"),NULL,choices = c(colnames(attr(vals$saved_data[[input$data_som]],"factors"))),width = '75px', selected=vals$bmu_p_factors)
    )))

  })
  observeEvent(ignoreInit = T,input$bmu_p_symbol,
               vals$bmu_p_symbol<-input$bmu_p_symbol)
  output$bmup_symbol<-renderUI({
    req(input$bmu_p_dotlabel)
    req(input$bmu_p_dotlabel=='symbols')
    if(is.null(vals$bmu_p_symbol)){vals$bmu_p_symbol<-df_symbol$val[1]}
    div(span("+ Shape:", inline(
      pickerInput(ns("bmu_p_symbol"),
                  label = NULL,
                  choices = df_symbol$val,
                  choicesOpt = list(content = df_symbol$img),

                  selected=vals$bmu_p_symbol, width = '75px')
    )))

  })
  observeEvent(ignoreInit = T,input$bmu_p_dotlabel,
               vals$bmu_p_dotlabel<-input$bmu_p_dotlabel)
  output$bmu_p_dotlabel_out<-renderUI({
    if(is.null(vals$bmu_p_dotlabel)){vals$bmu_p_dotlabel<-'symbols'}
    div(span("+ Display:",
             inline(radioButtons(ns("bmu_p_dotlabel"), NULL, choices = c("labels", "symbols"),selected=vals$bmu_p_dotlabel, inline=T, width="100px"
             ))

    ))
  })
  observeEvent(ignoreInit = T,input$bmu_p_training,{
    vals$bmu_p_training<-input$bmu_p_training
  })
  output$bmu_p_training_out<-renderUI({
    if(is.null(vals$bmu_p_training)){vals$bmu_p_training<-dfcolors$val[1]}
    div(span("+ Training color:",inline(
      pickerInput(ns("bmu_p_training"),
                  label =NULL,
                  choices =  vals$colors_img$val[getsolid_col()] ,
                  choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                  selected=vals$bmu_p_training, width = '75px'
      )
    )))
  })
  observeEvent(ignoreInit = T,input$bmu_p_test,{
    vals$bmu_p_test<-input$bmu_p_test
  })
  output$bmu_p_test_out<-renderUI({
    if(is.null(vals$bmu_p_test)){vals$bmu_p_test<-dfcolors$val[14]}
    div(span("+ Test color:", inline(
      pickerInput(ns("bmu_p_test"),
                  label ="",
                  choices =  vals$colors_img$val[getsolid_col()] ,
                  choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                  selected=vals$bmu_p_test, width = '75px')
    )))
  })
  observeEvent(ignoreInit = T,input$bmu_p_symbol_size,{
    vals$bmu_p_symbol_size<-input$bmu_p_symbol_size
  })
  output$bmu_p_symbol_size_out<-renderUI({
    div(span("+ Size:", inline(tipify(
      numericInput(ns("bmu_p_symbol_size"),NULL,value = vals$bmu_p_symbol_size,min = 0.1,max = 3,step = .1, width = '75px'),"symbol size"
    ))))
  })
  observeEvent(ignoreInit = T,input$bmu_p_bgpalette,{
    vals$bmu_p_bgpalette<-input$bmu_p_bgpalette
  })
  output$bmu_p_bgpalette_out<-renderUI({
    if(is.null(vals$bmu_p_bgpalette)){vals$bmu_p_bgpalette<-dfcolors$val[16]}
    div(span("+ Background:",
             inline(
               pickerInput(ns("bmu_p_bgpalette"),
                           label = NULL,
                           choices = vals$colors_img$val,
                           choicesOpt = list(content = vals$colors_img$img),

                           selected=vals$bmu_p_bgpalette, width = '75px')
             )))
  })
  observeEvent(ignoreInit = T,input$bmu_p_bg_transp,{
    vals$bmu_p_bg_transp<-input$bmu_p_bg_transp
  })
  output$bmu_p_bg_transp_out<-renderUI({
    div(span("+ Transparency:",inline(
      tipify(
        numericInput(ns("bmu_p_bg_transp"),NULL,
                     value=vals$bmu_p_bg_transp, min=0, max=1,step=0.1, width = '75px'),
        "Background transparency")
    )))
  })
  observeEvent(ignoreInit = T,input$bmu_p_border_grid,{
    vals$bmu_p_border_grid<-input$bmu_p_border_grid
  })
  output$bmu_p_border_grid_out<-renderUI({
    if(is.null(vals$bmu_p_border_grid)) {vals$bmu_p_border_grid<-vals$colors_img$val[getsolid_col()][7]}
    div(span(span('+ Border:'),inline(
      pickerInput(ns("bmu_p_border_grid"),
                  label =NULL,
                  choices =  vals$colors_img$val[getsolid_col()] ,
                  choicesOpt = list(
                    content =  vals$colors_img$img[getsolid_col()] ),
                  selected= vals$bmu_p_border_grid, width = '75px')
    )))
  })




  output$pop_bmu_p<-renderUI({
    div(
      class="map_control_style",style="color: #05668D",
      uiOutput(ns('bmu_p_dotlabel_out')),
      uiOutput(ns("bmup_labels")),
      uiOutput(ns("bmu_p_training_out")),
      uiOutput(ns("bmu_p_test_out")),
      uiOutput(ns("bmup_symbol")),
      uiOutput(ns('bmu_p_symbol_size_out')),
      uiOutput(ns("bmu_p_bgpalette_out")),
      uiOutput(ns("bmu_p_bg_transp_out")),
      uiOutput(ns("bmu_p_border_grid_out"))
    )

  })
  output$bmu_p_fac_control<-renderUI({
    div(
      splitLayout(
        column(12,
               p(strong("Test color", style="color: #0D47A1;")),
               tipify(
                 pickerInput(ns("bmu_p_test"),
                             label =NULL,
                             choices =  vals$colors_img$val[getsolid_col()] ,
                             choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                             selected= vals$colors_img$val[getsolid_col()] [4],
                             options=list(container="body")),
                 "Color of the testing observations",options=list(container="body"))),
        div(
          p(strong("Training color", style="color: #0D47A1;")),
          tipify(
            pickerInput(ns("bmu_p_training"),
                        label =NULL,
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ),
                        selected= vals$colors_img$val[getsolid_col()] [2],
                        options=list(container="body")),
            "Color of the training observations",
            options=list(container="body")))
      )
    )
  })
  output$bmu_pCodes<-renderPlot({vals$bmus_pred_plot})
  output$bmu_pCodes<-renderPlot({
    p<-BMUs_pred()
    vals$bmu_p_results<-data.frame(attr(p,"result"))
    p
  })


  observe({

    shinyjs::hide('som_mode')
  })


  observeEvent(ignoreInit = T,input$som_models,{
    vals$cur_train<-input$som_models
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
    tosave$SOM_MODEL<-vals$SOM_MODEL
    saveRDS(tosave,"savepoint.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()


  })
  #input<- readRDS('input.rds')
  #vals<-readRDS('savepoint.rds')



  observeEvent(ignoreInit = T,input$selecfac,vals$selecfac<-input$selecfac)

  output$selecfac<-renderUI({
    column(12,
           checkboxGroupInput(ns("selecfac"), "Select the factors", choices=colnames(attr(vals$saved_data[[input$data_somY]],"factors"))))
  })




  output$parameters_grid<-renderUI({
    div(style="margin-left: 10px",

        br(),

        div(strong("1.1. Set the grid",
                   actionLink(ns("somgridhelp"), tipify(icon("fas fa-question-circle"), "Click for more details"))),actionLink(ns("resettopo"), icon("fas fa-undo"))
        ),

        div(
          div(checkboxInput(ns("sugtopo"), span('suggested topology',tipify(actionLink(
            ns("sugtopohelp"), icon("fas fa-question-circle")
          ), "Click for more details")), value =tunesom$sugtopo, width="160px")),
          div(
            withSpinner(type=8,color="SeaGreen",uiOutput(ns("somgridtopo")))
          )


        )
    )
  })

  output$parameters_train<-renderUI({
    div(br(),
        strong("1.2. Set the training parameters",
               actionLink(ns("supersomhelp"), tipify(
                 icon("fas fa-question-circle"), "Click for more details"
               ))),actionLink(ns("resetsom"), icon("fas fa-undo")),
        div(
          uiOutput(ns("somcontrol"))



        )
    )
  })
  output$training_panel<-renderUI({
    div(id = "train_tab0",
        div(

          column(12,
                 column(6,style="padding: 5px",
                        div(
                          class="well3",
                          uiOutput(ns("parameters_grid"))

                        )),
                 column(6,style="padding: 5px",
                        div(
                          class="well3",
                          uiOutput(ns("parameters_train"))

                        ),

                        div(

                          uiOutput(ns("som_or_supersom"))

                        ),
                        div(
                          uiOutput(ns("finetuning_train"))

                        ))),

          bsTooltip(ns('resettopo'),"Reset parameters", "right"),
          bsTooltip(ns('resetsom'),"Reset parameters", "right")
        )
    )
  })

  output$results_panel<-renderUI({
    validate(need(length(vals$som_results)>0,"No trained model in selected Datalist"))

    req(input$data_som)
    req(input$som_models)
    validate(need(!is.null(vals$saved_data),"no data found"))
    div(style = "background: WhiteSmoke;",
        tabsetPanel(id=ns("som_res"),selected=vals$som_res,
                    tabPanel(
                      value= 'train_tab1',
                      "2.1. Parameters",
                      column(12, style = "background: white; margin-top: 10px",
                             column(12,
                                    splitLayout(
                                      uiOutput(ns("som_errors")),
                                      div(
                                        p(
                                          div(
                                            tipify(
                                              actionLink(
                                                ns('create_codebook'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                                              ),
                                              "Create a datalist  with the codebook vectors"
                                            )
                                          )
                                        ),
                                        p(
                                          div(
                                            tipify(
                                              actionLink(
                                                ns('save_bmu'),span("+ Save BMUs",icon("fas fa-file-signature")), style="button_active"
                                              ),
                                              "Add the BMUs to the Factor-Attribute (training Data)"
                                            )
                                          )),

                                        p(popify(bsButton(ns("down_pcodes_results"), span(icon("fas fa-download"),icon("fas fa-table")),style = "button_active"),NULL,"download codebook results",options=list(container="body"))),
                                        p(popify(downloadButton(ns("down_kohonen_results"), icon("fas fa-braille"),style = "button_active"),NULL,"download kohonen object as rds file containing the object of class kohonen with components")))
                                    )
                             ),
                             column(12, style="margin-top: 10px",splitLayout(cellWidths = c("50%","10%"),div(p(strong("Training Parameters")),tableOutput(ns("train.summary")))))
                      )
                    ),
                    tabPanel("2.2. Changes",value = "train_tab2",
                             column(12,
                                    style = "background: white;",
                                    column(12,actionButton(ns("downp_pchanges"),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                                    column(12,plotOutput(ns("pchanges"))))
                    ),
                    tabPanel("2.3. Couting", value = "train_tab3",
                             column(12,style = "background: white;",
                                    column(12,actionButton(ns('downp_pcounts'),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
                                    column(12, plotOutput(ns("pcounts"))))),
                    tabPanel("2.4. BMUs", value = "train_tab5",
                             uiOutput(ns("plot_bmu"))
                    ),
                    tabPanel("2.5. Network plot", value = "train_tab_net",
                             uiOutput(ns("netpanel"))
                    )

        )
    )
  })

  output$netpanel<-renderUI({
    sidebarLayout(
      sidebarPanel(
        column(12,
               class="map_control_style",
               style="color: #05668D",
               checkboxInput(ns("net_label"),"+ Label"),
               pickerInput(ns("netpalette"),
                           label =NULL,
                           choices = vals$colors_img$val,
                           choicesOpt = list(content = vals$colors_img$img),
                           selected=vals$bmu_facpalette)


        )
      ),
      mainPanel(uiOutput(ns("netplot")))
    )

  })

  output$netplot<-renderUI({

    m<-vals$som_results
    if(length(m$data)>1){



      div(
        renderPlot({
          plotnetwork_list(m, palette=as.character(input$netpalette),newcolhabs=vals$newcolhabs, label=input$net_label, main="")
        }),
       # renderPlot(plotnetwork_list3(m, palette=as.character(input$netpalette),newcolhabs=vals$newcolhabs, label=input$net_label, main=""))
      )
    } else{
      m<-getsom()

      col<-vals$newcolhabs[[input$netpalette]](1)
      renderPlot( plotnetwork(m, col=col, label=input$net_label))
    }




  })
  observeEvent(ignoreInit = T,input$save_bmu,{
    savebmu()
  })


  getsom<-reactive({
    req(input$som_models)
    req(length(attr(getdata_som(),"som"))>0)
    m<-attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]
    req( class(m) == "kohonen")
    m
  })

  output$som_umatrix<-renderUI({
    column(12,style = "background: white;",
           column(12,actionButton(ns("downp_pmatrix"),icon("fas fa-image"),icon("fas fa-download"), style="button_active")),
           column(12, plotOutput(ns("pUmatrix")))
    )
  })
  observeEvent(ignoreInit = T,input$sompred_type,{
    vals$sompred_type<-input$sompred_type
  })

  observeEvent(ignoreInit = T,input$predsom_new,{
    vals$predsom_new<-input$predsom_new
  })

  output$predsom_var<-renderUI({
    selectInput(ns("predsom_var"),"variable",colnames(test_som()))
  })




  output$train_som_button<-renderUI({
    lab<- h4(icon("fas fa-braille"),"train SOM",icon("fas fa-arrow-circle-right"))
    if(isTRUE(input$mysupersom)){
      lab<-h4(icon("fas fa-braille"),"train superSOM",icon("fas fa-arrow-circle-right"))
    }
    actionButton(
      ns("trainSOM"),
     lab, style = "background: #05668D; color: white")

  })





  output$som_or_supersom<-renderUI({
    div(align = "center",
        br(),
        uiOutput(ns("train_som_button")),
        uiOutput(ns("train_supersom_button"))
    )
  })


  output$finetuning_train<-renderUI({

    div(style="margin-top: 20px",
        div(
          span(class="finesom_btn",
               tipify(bsButton(ns("finesom"),"Fine tuning*", type="toggle", value=tunesom$finesom),"show all parameters available")
          )),
        div(

          uiOutput(ns("finetuning_som")))
    )

  })
  output$finetuning_som<-renderUI({
    req(isTRUE(input$finesom))
    div(          class='well3',div(id="finesom_out",    class="map_control_style",
                                    style="color: #05668D",
                                    div(
                                      span("Alpha:"),
                                      inline(
                                        div(
                                          numericInput(ns("a1"),label = NULL,value = tunesom$a1,step = 0.01, width="85px")
                                        )
                                      ),
                                      inline(
                                        div(
                                          numericInput(ns("a2"),label = NULL,value = tunesom$a2,step = 0.01, width="85px")
                                        )
                                      )
                                    ),
                                    div(   span("Radius:"),
                                           inline(
                                             div(
                                               numericInput(ns("r1"),label = NULL,value = tunesom$r1, width="85px")
                                             )
                                           ),
                                           inline(
                                             div(
                                               numericInput(ns("r2"),label = NULL,value = tunesom$r2,step = 0.01, width="85px" )
                                             )
                                           )
                                    ),
                                    div(
                                      span("mode"),
                                      inline(pickerInput(ns("mode"), NULL, choices = c("online","batch", "pbatch"), selected=tunesom$mode, width="85px"))
                                    ),
                                    div(
                                      inline(
                                        div(
                                          span("maxNA.fraction"),
                                          numericInput(ns("maxna"),NULL,value = tunesom$maxna,step = 0.01, width="85px")
                                        )
                                      )
                                    )))

  })

  output$dist_som<-renderUI({
    req(isFALSE(input$mysupersom))
    div(style='color: #05668D',
        div(strong("dist.fcts",tipify(icon("fas fa-question-circle"),"Distance measure between each neuron and input data"))),
        pickerInput(ns("distmethod"),NULL,
                    choices = c("BrayCurtis","euclidean","sumofsquares","manhattan",
                                "tanimoto"),selected=tunesom$distmethod,width='150px')

    )
  })





  output$somcontrol<-renderUI({

    div(
      div(
        div(br()),
        div(br()),
        inline(uiOutput(ns("dist_som"))),
        inline(
          div(

            div("normalizeDataLayers"),
            pickerInput(ns("normalizeDataLayers"),NULL,
                        choices = c("TRUE","FALSE"),selected=tunesom$normalizeDataLayers,width='150px')

          )
        ),
        inline(
          numericInput(ns("rlen"),strong("rlen",tipify(icon("fas fa-question-circle"),"The number of times the complete dataset will be presented to the network")),value = tunesom$rlen,min = 1,step = 1, width='100px')
        ),
        inline(
          numericInput(ns("seed"), strong("seed",tipify(icon("fas fa-question-circle"),"A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the som analysis.")), value =tunesom$seed, min=0, step=1, width='100px')
        )
      ))

  })

  output$somgridtopo<-renderUI({topocontrol()})
  output$train.summary<-renderTable({
    train.summary()
  }, rownames = T, colnames = F,striped=T, spacing ="xs")
  output$nopredictions<-renderPrint({
    cat("no predictions in \n'",input$data_som,"' \n to overwritte")
  })
  output$nosommodel<-renderPrint({
    cat("no som model in \n'",input$data_som,"' \n to overwritte")
  })










  output$pred_property<-renderUI({



    column(12,style="background: white",

           uiOutput(ns("predsom_var")),
           plotOutput(ns("predsom_property")))
  })
  output$predsom_property<-renderPlot({
    req( input$predsom_var)
    som_pred<-pred_som()
    plot(vals$som_results, type = "property", main = input$predsom_var, property = som_pred$unit.predictions[[1]][,input$predsom_var], palette.name=coolBlueHotRed,cex=0.5,shape="straight",keepMargins = TRUE)})

  getobs_somX<-reactive({
    datalist<-vals$saved_data
    m<-vals$som_results
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
    m<-vals$som_results


    res0<-unlist(
      lapply(datalist, function (x){
        res<-colnames(x)%in%colnames(m$data[[2]])
        sum(res)==ncol(m$data[[2]])
      })
    )
    names(res0[res0==T])
  })
  correctsom<-reactive({
    som_pred<-pred_som()
    m<-vals$som_results
    newdata=as.matrix(newdata_som())
    pred_som<-predict(m,newdata=newdata, whatmap = 1)

    res<-get_correct_predsom(m,pred_som,newdata)
    res
  })

  pred_som<-reactive({
    validate(need(!anyNA(newdata_som()),"NAs not allowed in the prediction Datalist"))
    m<-vals$som_results
    som_pred<-predict(m,newdata = as.matrix(newdata_som()), whatmap = 1)
    #names(som_pred$predictions)<-c("X","Y")
    #names(som_pred$unit.classif)<-rownames(som_pred$predictions[[1]])
    attr(som_pred,"coords")<-attr(m,"coords")
    som_pred
  })








  get_overhall_peform<-reactive({
    newdata<-newdata_som()
    names<-if(is.data.frame(newdata)){ input$predsom_new} else{  names(newdata)    }
    req(length(newdata)>0)
    if(!is.data.frame(newdata)){
      res<-get_sompred()
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }

      pred<-res$predictions
    } else{
      res<-get_sompred()

      if(is.null(res$data)){
        res$data<-res$predictions
      }
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }
      pred<-res$predictions[[res$whatmap]]


    }



    req(length(pred)>0)
    if(is.list(pred)){
      lis<-mapply(list,pred,newdata,SIMPLIFY=F)
      res<-data.frame(do.call(rbind,lapply(lis,function (x){
        postResample(x[[1]],x[[2]])
      })))

    } else{
      peformace<-postResample(pred,newdata)
      res<-data.frame(as.list(peformace))
      colnames(res)<-names(peformace)
      rownames(res)<-names
    }

    res
  })


  get_obs_peform<-reactive({
    newdata<-newdata_som()
    names<-if(is.data.frame(newdata)){ input$predsom_new} else{  names(newdata)    }
    req(length(newdata)>0)
    if(!is.data.frame(newdata)){
      res<-get_sompred()
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }

      pred<-res$predictions
    } else{
      res<-get_sompred()

      if(is.null(res$data)){
        res$data<-res$predictions
      }
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }
      pred<-res$predictions[[res$whatmap]]


    }

    req(length(pred)>0)
    if(is.list(pred)){
      lis<-mapply(list,pred,newdata,SIMPLIFY=F)
      x<-lis[[1]]

      res<-data.frame(do.call(data.frame,lapply(lis,function (x){
        do.call(rbind,lapply(rownames(x[[1]]),function(i){
          postResample(x[[1]][i,],x[[2]][i,])
        }))

      })))
      rownames(res)<-rownames(newdata[[1]])
      res
    } else{
      i=1
      res<-do.call(rbind,lapply(rownames(newdata),function(i){
        postResample(pred[i,],unlist(newdata[i,]))
      }))
      rownames(res)<-rownames(newdata)

    }

    res
  })

  get_var_peform<-reactive({
    newdata<-newdata_som()
    names<-if(is.data.frame(newdata)){ input$predsom_new} else{  names(newdata)    }
    req(length(newdata)>0)
    if(!is.data.frame(newdata)){
      res<-get_sompred()
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }

      pred<-res$predictions
    } else{
      res<-get_sompred()

      if(is.null(res$data)){
        res$data<-res$predictions
      }
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }
      pred<-res$predictions[[res$whatmap]]


    }

    req(length(pred)>0)
    if(is.list(pred)){
      lis<-mapply(list,pred,newdata,SIMPLIFY=F)
      x<-lis[[1]]

      res<-data.frame(do.call(rbind,lapply(lis,function (x){
        res<-do.call(rbind,lapply(colnames(x[[1]]),function(i){
          postResample(x[[1]][,i],x[[2]][,i])
        }))
        rownames(res)<-colnames(x[[1]])
        res

      })))

      res
    } else{
      i=1
      res<-do.call(rbind,lapply(colnames(newdata),function(i){
        postResample(pred[,i],unlist(newdata[,i]))
      }))
      rownames(res)<-colnames(newdata)

    }

    res
  })

  getpred_model<-reactive({

    res0<-res<-get_sompred()
    pic_result<-res0$whatmap

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
    m_pred<-vals$som_results
    req(class(m_pred)=="kohonen")

    res<-getpred_model()
    unit.classif<-res$unit.classif[,1]
    names(unit.classif)<-rownames(res$unit.classif)


    m_pred$codes<-list(res$unit.predictions)
    names(m_pred$codes)<-res$pic_result

    m_pred$unit.classif<-unit.classif

    m_pred$data<-list(as.matrix(res$predictions))
    m_pred$whatmap<-res$pic_result
    names(m_pred$data)<-res$pic_result

    picmap<-which(names(vals$som_results$data)%in%res$pic_result)
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
    som_qualist<-errors_som(vals$som_results)
    res<-data.frame(som_qualist$som_quality)

    res$mean=apply(data.frame(lapply(res,function(x)as.numeric(x))),1,mean)
    neu.uti<-data.frame(som_qualist$neu.uti)
    rownames(neu.uti)<-"neu.uti"
    res_names<-rownames(res)
    div(
      strong("Quality measures:", actionLink(ns("som_quality_help"),icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"))),
      renderTable(res,rownames =T),
      renderTable(neu.uti,rownames =T,colnames =F),
      )
  })






  observeEvent(ignoreInit = T,input$som_quality_help, {

    pkgs<-'aweSOM'
    citations <- do.call('c',lapply(pkgs, citation))
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

  output$down_kohonen_results <- {
    downloadHandler(
      filename = function() {
        paste0("Kohonen","_", Sys.Date(),".rds")
      }, content = function(file) {
        saveRDS(vals$som_results,file)
      })
  }


  bmu_points<-reactive({
    req(input$bmu_dotlabel)
    if(input$bmu_dotlabel == 'symbols'){ T} else {F}
  })

  BMUs<-reactive({
    req(input$npic)
    req(input$vfm_type)
    req(length(input$varfacmap_action)>0)
    req(input$bmu_bgpalette)
    req(input$bmu_facpalette)
    req(input$bmu_border_grid)
    req(input$bmu_var_color)
    if (isTRUE(input$varfacmap_action)) {
      npic=as.numeric(input$npic)} else{
        npic = 0
      }
    p <-pcorr(
      m=vals$som_results,
      npic = npic,
      indicate = input$vfm_type,
      pch = as.numeric(input$bmu_symbol),
      labels.ind = bmu_factors_reac(),
      cex =as.numeric(input$bmu_symbol_size),
      bg_palette = as.character(input$bmu_bgpalette),
      factor.pal=as.character(input$bmu_facpalette),
      points=bmu_points(),
      cex.var=input$bmu_cexvar,
      ncol=input$bmu_ncol,
      insetx=input$bmu_insertx,
      insety=input$bmu_inserty,
      alpha.legend=input$bmu_leg_transp,
      alpha_bg=input$bmu_bg_transp,
      border=getcolhabs(vals$newcolhabs,input$bmu_border_grid,1),

      col.text= getcolhabs(vals$newcolhabs,input$bmu_var_color,1),
      newcolhabs=vals$newcolhabs,

    )
    vals$bmus_plot<-recordPlot()
    p



  })
  bmu_symbol.reac <- reactive({
    if (input$topo == "hexagonal") {
      16
    } else if (input$topo == 'rectangular') {
      15
    }
  })

  topo.reactive <- reactive({

    df=data.frame(na.omit(getdata_som()))
    N<-nrow(getdata_som())
    SIZE<-sqrt(5*sqrt(N))
    L<- floor(SIZE)
    if(!nrow(df)>0){
      c(L*L,L,L)
    } else{
      topology(getdata_som(), dist = input$distmethod)
    }
  })
  getdata_som<-reactive({


    req(input$data_som)
    data_o<-data<-vals$saved_data[[input$data_som]]
    factors<-attr(data,"factors")

    data
  })

  getobs_som<-reactive({
    datalist<-vals$saved_data
    datalist=lapply(datalist,function(x) attr(x,"factors"))

    res0<-unlist(
      lapply(datalist, function (x){
        res<-any(colnames(x)%in%names(pred_som()$predictions[-1]))

      })
    )
    names(res0[res0==T])
  })

  bmu_pred_points<-reactive({
    req(input$bmu_p_dotlabel)
    if(input$bmu_p_dotlabel == 'symbols'){ T} else {F}
  })
  bmu_p_factors_reac<-reactive({
    factors<-attr(vals$saved_data[[input$data_som]],"factors","factors")
    m<-vals$som_results
    if (length(input$bmu_p_factors)>0) {
      c(factors[rownames(vals$saved_data[[input$data_som]],"factors"), input$bmu_p_factors],
        if(length(attr(m,"test"))>0) {factors[rownames(attr(m,"test")), input$bmu_p_factors]})
    } else{c(factors[rownames(vals$saved_data[[input$data_som]],"factors"),1],
             if(length(attr(m,"test"))>0) {factors[rownames(attr(m,"test")), ]})}
  })
  BMUs_pred<-reactive({
    req(input$bmu_p_bg_transp)
    req(input$bmu_p_symbol)
    req(input$bmu_p_bgpalette)
    req(input$bmu_p_training)
    req(input$bmu_p_border_grid)
    req(input$bmu_p_test)
    p<-pcorr(
      m=vals$som_results,
      npic = 0,
      pch = as.numeric(input$bmu_p_symbol),
      labels.ind = bmu_p_factors_reac(),
      cex =as.numeric(input$bmu_p_symbol_size),
      bg_palette = as.character(input$bmu_p_bgpalette),
      factor.pal=as.character(input$bmu_p_training),
      points=bmu_pred_points(),
      legend=F,
      predict=pred_som(),
      alpha_bg=input$bmu_p_bg_transp,
      border=getcolhabs(vals$newcolhabs,input$bmu_p_border_grid,1),
      pred_col=as.character(input$bmu_p_test),
      newcolhabs=vals$newcolhabs
    )
    vals$bmus_pred_plot<-recordPlot()
    p
  })

  output$xdim_topo<-renderUI({
    numericInput(ns("xdim"),"xdim",value = tunesom$dim[[1]],min = 0,step = 1, width="100px")
  })
  output$ydim_topo<-renderUI({
    numericInput(ns("ydim"),"ydim",value = tunesom$dim[[2]],min = 0,step = 1, width="100px")
  })
  topocontrol<-reactive({
    div(id = "topocontrol",
        div(
          inline(uiOutput(ns('xdim_topo'))),
          inline(uiOutput(ns('ydim_topo'))),
          inline(
            div(style='color: #05668D',
                div(strong("Topology")),
                pickerInput(ns("topo"),NULL,choices = c("hexagonal", "rectangular"),selected=tunesom$topo, width="110px")

            )
          ),
          inline(
            div(
              div("neigh.fct"),
              pickerInput(ns("neighbourhood.fct"),label =NULL ,choices = c("gaussian","bubble"),selected=tunesom$neighbourhood.fct, width="110px")
            )
          ),
          inline(
            div(
              div("toroidal"),
              pickerInput(ns("toroidal"),label = NULL,choices = c(F, T), selected=tunesom$toroidal, width="100px")
            )
          )


        ),
        div(
          style="text-align: center; width: 350px",
          uiOutput(ns("showgrid")))
    )
  })
  toroidal<-reactive({
    switch (tunesom$toroidal,
            'TRUE' = TRUE,
            "FALSE" = FALSE)
  })

  bmu_factors_reac<-reactive({
    factors<-attr(vals$saved_data[[input$data_som]],"factors","factors")
    if (length(input$bmu_factors)>0) {
      factors[rownames(vals$saved_data[[input$data_som]],"factors"), input$bmu_factors]
    } else{factors[,1]}
  })

  train.summary<-reactive({
    m<-vals$som_results

    req(class(m)=="kohonen")
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


  observeEvent(ignoreInit = T,input$showNA_predsom,{

  })
  observeEvent(ignoreInit = T,input$sugtopo,{
    tunesom$sugtopo<-input$sugtopo
  })
  observeEvent(ignoreInit = T,input$topo,{
    tunesom$topo<-input$topo
  })
  observeEvent(ignoreInit = T,input$maxna,{
    tunesom$maxna<-input$maxna
  })
  observeEvent(ignoreInit = T,input$mode,{
    tunesom$mode<-input$mode
  })
  observeEvent(ignoreInit = T,input$r2,{
    tunesom$r2<-input$r2
  })
  observeEvent(ignoreInit = T,input$r1,{
    tunesom$r1<-input$r1
  })
  observeEvent(ignoreInit = T,input$a2,{
    tunesom$a2<-input$a2
  })
  observeEvent(ignoreInit = T,input$a1,{
    tunesom$a1<-input$a1
  })
  observeEvent(ignoreInit = T,input$neighbourhood.fct,{
    tunesom$neighbourhood.fct<-input$neighbourhood.fct
  })
  observeEvent(ignoreInit = T,input$toroidal,{
    tunesom$toroidal<-input$toroidal
  })
  observeEvent(ignoreInit = T,input$distmethod,{
    tunesom$distmethod<-input$distmethod
  })
  observeEvent(ignoreInit = T,input$xdim,{
    tunesom$dim[[1]]<-input$xdim
  })
  observeEvent(ignoreInit = T,input$ydim,{
    tunesom$dim[[2]]<-input$ydim
  })
  observeEvent(ignoreInit = T,input$seed,{
    tunesom$seed<-input$seed
  })
  observeEvent(ignoreInit = T,input$rlen,{
    tunesom$rlen<-input$rlen
  })
  observeEvent(ignoreInit = T,input$data_som,{
    if(anyNA(getdata_som())){
      updateSelectInput(session,"distmethod")
    }
  })
  observeEvent(ignoreInit = T,input$finetopo,{
    tunesom$finetopo<-input$finetopo
  })
  observe({
    req(input$xdim)
    req(input$ydim)
    tunesom$r1<-as.vector(quantile(unit.distances(
      kohonen::somgrid(input$xdim,input$ydim,topo = input$topo,toroidal = toroidal(), neighbourhood.fct=input$neighbourhood.fct)), 2 / 3))
  })


  observeEvent(ignoreInit = T,input$data_somY,{
    vals$cur_somdataXsomY<-input$data_somY
  })


  observeEvent(ignoreInit = T,input$finesom,{
    tunesom$finesom<-input$finesom
  })
  observeEvent(ignoreInit = T,input$som_res,{
    vals$som_res<-input$som_res
  })



  observeEvent(ignoreInit = T,input$bmu_legend,{
    vals$bmuleg<-if(isFALSE(vals$bmuleg)){T} else if(isTRUE(vals$bmuleg)){F}

  })

  observeEvent(  list(getdata_som(),input$sugtopo),{

    req(input$sugtopo)
    if(isTRUE(input$sugtopo)){
      dim = try(topo.reactive(),silent=T )

      validate(need(class(dim)!="try-error","error in topology calculation; check for inconsistency in data, such as columns with variance 0"))
      tunesom$dim<-c(dim[[2]],dim[[3]])
    }
  })
  observeEvent(list(input$xdim, input$ydim), {
    if (length( names(vals$saved_data)) > 0) {
      dim = try(topo.reactive(),silent=T )

      validate(need(class(dim)!="try-error","E"))
      ydim <- dim[[3]]
      xdim <- dim[[2]]
      if (input$xdim != xdim|input$ydim != ydim) {
        updateCheckboxInput(session, "sugtopo", NULL, FALSE)
      } else{
        updateCheckboxInput(session, "sugtopo", NULL, TRUE)
      }

    }
  })




  observeEvent(ignoreInit = T,input$som_type,{
    vals$cur_somtype<-input$som_type
  })
  observeEvent(ignoreInit = T,input$som_model_delete,{
    attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]<-NULL
  })



  observeEvent(ignoreInit = T,input$resetsom, {
    tunesom$rlen=500
    tunesom$distmethod="BrayCurtis"
    tunesom$seed=NA
    tunesom$a1=0.05
    tunesom$a2=0.01
    tunesom$r1=0
    tunesom$r2=0
    tunesom$mode="online"
    tunesom$maxna=0.001
  })
  observeEvent(ignoreInit = T,input$resettopo, {
    tunesom$sugtopo=T

    tunesom$toroidal="FALSE"
    tunesom$topo="hexagonal"
    tunesom$neighbourhood.fct='bubble'
  })

  observeEvent(ignoreInit = T,input$trainSOM,{
    req(isFALSE(input$mysupersom))
    if(is.null(attr(vals$saved_data[[input$data_som]],"som"))){
      attr(vals$saved_data[[input$data_som]],"som")<-list()
    }
    data = data.frame(getdata_som())
    data_o<-data.frame(vals$saved_data[[input$data_som]])
    test<-which(!rownames(data_o)%in%rownames(data))
    withProgress(
      message = "Running som... the time taken will depend on the size of the data and the training.",
      min = 1,
      max = 1,



      {
        datalist<-list(as.matrix(data))
        names(datalist)<-input$data_som
        if(is.na(input$seed)==F){set.seed(input$seed)}
        m<-try(
          supersom2(
            datalist,
            grid = kohonen::somgrid(
              input$xdim,
              input$ydim,
              topo = input$topo,
              toroidal = toroidal(),
              neighbourhood.fct=tunesom$neighbourhood.fct
            ),
            rlen = input$rlen,
            dist.fcts = input$distmethod,
            alpha = c(tunesom$a1, tunesom$a2),
            radius = c(tunesom$r1, tunesom$r2),
            mode = tunesom$mode,
            maxNA.fraction = tunesom$maxna,
            normalizeDataLayers=as.logical(input$normalizeDataLayers)
          )
        )

        if (class(m) != "kohonen")        {
          validate(paste(m[[1]], "Please decrease your alpha (learning rate)"))
        }
        attr(m,'mode')<-input$mode

        names(m$unit.classif)<-rownames(m$data[[1]])

        attr(m,"test_partition")<-"None"

        attr(m,"Method")<-"Unsupervised"
        attr(m,"Datalist")<-input$data_som
        attr(m,"normalizeDataLayers")<-input$normalizeDataLayers
        attr(m,"coords")<-attr(data,"coords")
        vals$som_unsaved<-m

        attr(vals$saved_data[[input$data_som]],"som")[["new som (unsaved)"]]<-m

        m


      }
    )

  })




  observeEvent(ignoreInit = T,input$trainSOM, {
    vals$cur_train<-"new som (unsaved)"
    updateTabsetPanel(session, "som_tab", "som_tab2")
    updateTabsetPanel(session, "som_tab", "train_tab2")
    updateTabsetPanel(session, "som_res", "train_tab1")
    # updateSelectInput(session,"som_models",                      choices=c("new som (unsaved)", names(attr(vals$saved_data[[input$data_som]],"som"))))
    #updateSelectInput(session,"som_models",selected="new som (unsaved)")
  })
  observeEvent(ignoreInit = T,input$resettopo, {
    output$somgridtopo<-renderUI({topocontrol()})})



  observe({
    if(is.null(vals$showdataXsom_label)){
      vals$showdataXsom_label<-"~ Training Datalist:"
    }
    if(is.null(vals$showdataXsom_X)){
      vals$showdataXsom_X<-'X:'
    }
  })





  observe({

    req(input$data_som)
    req(input$som_models)
    if(input$som_models=="new som (unsaved)"){
      res<-vals$som_unsaved

    } else{
      res<-attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]
    }
    req(class(res)=="kohonen")
    if(length(res$data)==1){
      if(names(res$data)=="X"){

        names(res$data)<-attr(res,"Datalist")
      }
    }


    vals$som_results<-res
  })



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



  observeEvent(ignoreInit = T,input$downp_pchanges,{
    vals$hand_plot<-"Training plot"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_pcounts,{
    vals$hand_plot<-"Couting plot"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_pmatrix,{
    vals$hand_plot<-"uMatrix"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_bmu,{
    vals$hand_plot<-"BMUs"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_bmu_pred,{
    vals$hand_plot<-"BMUs predictions"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_pproperty,{
    vals$hand_plot<-"property plot"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$downp_confsom,{
    vals$hand_plot<-"Confusion Matrix SOM"

    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })

  observeEvent(ignoreInit = T,input$down_pcorr_results,{
    vals$hand_down<-"pcorr"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

  })
  observeEvent(ignoreInit = T,input$down_som_pred_results,{
    vals$hand_down<-"som predictions"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)


  })
  observeEvent(ignoreInit = T,input$down_pcodes_results,{
    vals$hand_down<-"pcodes"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })
  observeEvent(ignoreInit = T,input$create_codebook,{
    vals$hand_save<-"Create Datalist with Codebooks"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_som())
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
      ##RF
      "Create Datalist  - SOM predictions for New Data (X)"=bag_predsom_results(),
      "Create Datalist  - SOM codebook predictions"=bag_predsom_results(),
      "Create Datalist  - SOM performace"=bag_predsom_errors(),
      "Save new som in"=bag_somname(),
      "Save som predictions"=predsom_name(),
      "Save errors from som predictions (X)"=bag_sompred_eX(),
      "Save errors from som predictions (Y)"=bag_sompred_eY(),
      "Create Datalist with Codebooks"=coodebook_name()
    )})


  bag_predsom_errors<-reactive({
    name0<-paste(input$data_som,input$get_predsom_perf, sep="_")
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]
  })
  bag_predsom_results<-reactive({
    name0<-paste(input$data_som,input$predsom_results,input$layer_result, sep="_")
    name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
    name1[length(vals$saved_data)+1]
  })
  bag_somname<-reactive({

    m<-vals$som_results
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

  bag_somname_obd<-reactive({



    soms<-names(attr(vals$saved_data[[input$data_som]],"som"))
    if(is.null(soms)){
      bag=1
    } else{ bag=length(soms)+1}


    name0<-"SOM"
    name1<-paste0(name0," (",bag,")")
    if(name1%in%soms)
    {
      repeat{
        bag<-bag+1
        name1<-paste0(name0," (",bag,")")
        if(!name1%in%soms) break
      }
    }
    paste0(name0," (",bag,")")


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
  module_som <- function() {
    ns <- session$ns

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
  observeEvent( input$data_confirm,{
    req(!is.null(vals$hand_save))
    switch(
      vals$hand_save,
      "Create Datalist  - SOM predictions for New Data (X)"=create_predsom_newdata(),
      "Create Datalist  - SOM codebook predictions"=create_predsom_codebook(),
      "Create Datalist  - SOM performace"=create_predsom_errors(),
      "Create Datalist with Codebooks"=savecoodebok(),
      "Save new som in"=savesom(),
      "Save som predictions"=savesompred(),
      "Save errors from som predictions (X)"=datalist_som_errorsX(),
      "Save errors from som predictions (Y)"=datalist_som_errorsY()
    )
    removeModal()

  })
  create_predsom_errors<-reactive({

    data_o<-switch(input$sompred_type,
                   "Partition"={
                     vals$saved_data[[input$data_som]][rownames(attr(vals$som_results,"test")[[1]]),]
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
    res=data.frame(vals$som_results$codes[[1]])
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
  savesompred<-reactive({
    data=vals$saved_data[[input$data_som]]
    som_pred<-pred_som()
    predX<- correctsom()$predX
    predX<-data_migrate(data,predX,input$newdatalist)
    factors<-attr(data,"factors")[rownames(predX),]
    coords<-attr(som_pred,"coords")[rownames(predX),]
    attr(predX,"factors")<-factors
    attr(predX,"coords")<-coords
    newsaveX<-paste0(input$newdatalist,"_X")
    saved_sompred$df[[newsaveX]]<-1
    vals$saved_data[[newsaveX]]<-predX

    if(input$hand_save=="create"){
      if(attr(vals$som_results,"Method")!="Unsupervised"){
        predY<- correctsom()$predY
        if(attr(vals$som_results,"som_type2")=="Numeric"){
          newsave<-paste(input$newdatalist,"_Y")
          predY<-data_migrate(data,predY,newsave)
          vals$saved_data[[newsave]]<-predY
        } else{

          attr(vals$saved_data[[newsaveX]],"factors")<-predY

        }
      }
    } else {
      pred_data<-data.frame(pred_data)
      pred_data<-data_migrate(data,pred_data,input$over_datalist)
      factors<-attr(pred_data,"factors")[rownames(pred_data),]
      factors[,input$over_datalist]<-pred_factors
      coords<-attr(test_data,"coords")[rownames(pred_data),]
      attr(pred_data,"factors")<-factors
      attr(pred_data,"coords")<-coords
      attr(pred_data,"transf")<-attr(data,"transf")
      vals$saved_data[[input$over_datalist]]<-pred_data}
    updateTabsetPanel(session,'som_tab','som_tab3')
  })

  savebmu<-reactive({
    temp<-vals$som_results
    bmu<-temp$unit.classif
    names(bmu)<-rownames(temp$data[[1]])
    bmu<-data.frame(bmu=as.factor(bmu))
    factors<-attr(vals$saved_data[[input$data_som]],"factors")
    factors<-data.frame(factors,bmu)
    attr(vals$saved_data[[input$data_som]],"factors")<-factors
  })
  savesom<-reactive({
    curtab<-vals$cursomtab
    data<-getdata_som()
    temp<-vals$som_results
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
    vals$cur_train<-cur

    updatePickerInput(session,
                      "som_models",
                      choices=c(names(attr(vals$saved_data[[vals$cur_train]],"som"))),
                      selected=cur)
    delay(500,{updateTabsetPanel(session, "som_tab", curtab)
      updateTabsetPanel(session, "som_res", vals$som_res)})

    attr(vals$saved_data[[input$data_som]],"som")[['new som (unsaved)']]<-NULL

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


  get_newdata_infto<-reactive({

    newdata<-newdata_som()
    if(is.data.frame(newdata)){
      ncols=ncol(newdata)
      nrows=nrow(newdata)
      nlayers=input$whatmap


    } else{
      nlayers=names(newdata)
      ncols=unlist(lapply(newdata,ncol))
      nrows=unlist(lapply(newdata,nrow))
    }

    res<-data.frame(
      do.call(rbind,list(
        layers=nlayers,
        ncols=ncols,
        nrows=nrows
      ))
    )
    colnames(res)<-NULL
    res


  })


  output$data_somX_out<-renderUI({
    req(input$som_tab)
    style=vals$showdataXsom
    label<-vals$showdataXsom_label
    X<-vals$showdataXsom_X


    selected<-vals$cur_somdataX
    choices<-names(vals$saved_data)
    div(style=style,
        span(strong(X),
             inline(
               div(
                 inline(
                   div(
                     div(label),
                     pickerInput(ns("data_som"),NULL,choices =choices,width="150px", selected=selected, options=list(container="body"))
                   )
                 ),
                 inline(uiOutput(ns("saved_soms")))
               )
             )
        )
    )

  })
  observeEvent(ignoreInit = T,input$data_som,{
    valscur_data<-vals$cur_somdataX<-input$data_som
  })


  observe({
    req(input$som_tab)
    req(input$data_som)
    req(length(input$mysupersom)>0)
    if(isTRUE(input$mysupersom)){
      datax<-ssom_reac()[1,1]

      if(input$som_tab=="som_tab1"){
        vals$cur_somdataX<-datax
        updatePickerInput(session,"data_som",selected=datax, choices=datax)
      } else{
        updatePickerInput(session,"data_som",choices=names(vals$saved_data), selected=vals$cur_somdataX)
      }




    } else{

      updatePickerInput(session,"data_som",selected=vals$cur_somdataX, choices=names(vals$saved_data))

    }

  })

}


