## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
#' @export
module_ui_spd<-function(id){

  ns<-NS(id)
  tagList(
    includeCSS("inst/app/www/styles.css"),
    div(
      class='choosechannel',
      #uiOutput(ns("bug")),
      #inline( actionButton(ns("teste_comb"),"SAVE")),
      uiOutput(ns("spd_header1")),

      uiOutput(ns("spd_panels"))

    )
  )

}

#' @export

module_server_spd<-function (input,output,session,vals,df_colors,newcolhabs,df_symbol ){
  ns <- session$ns

  symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
  df_symbol <- data.frame(
    val = c(16,15,17,18,8,1,5,3)
  )
  for(i in 1:length(symbols))
  {
    symbol1<-dataURI(file = paste0('inst/app/www/pch',i,".png"), mime = "image/png")

    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}

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

  train_style="
.train_style label{
  display: table-cell;
  text-align: left;
  vertical-align: middle;
  font-weight: normal;
  color: #05668D;
    white-space: nowrap;
}
.train_style .form-group {
  display: table-row;
 vertical-align: middle;

}


.train_style .checkbox  {
  margin: 0px;
  border-radius: 0px 0px 0px 0px;
  min-height: 35px;

}

.train_style .form-control  {
  margin: 2px;
  border-radius: 0px 0px 0px 0px;
  white-space: normal;
  min-height: 35px;
  padding-left: 5px;
  background: white;
 vertical-align: middle;
min-width: 125px;
border: 1px solid #ccc;
box-shadow: inset 0 1px 1px rgba(0,0,0,.075);
transition: border-color ease-in-out .15s,box-shadow ease-in-out .15s,-webkit-box-shadow ease-in-out .15s;
}
.train_style .shiny-input-container{
  border: 0px solid white;
  margin: 0px;
  padding: 0;
  margin-top: 2px;
 vertical-align: middle;
}
.train_style .btn  {
  border-radius: 0px 0px 0px 0px;
  border: 1px solid #E8E8E8;
padding-left: 5px;
background: white;
    border: 0px solid black;
 vertical-align: middle;

}



.train_style .btn-default:focus,.btn-radio_cogs.active {
    margin: 2px;
  border-radius: 0px 0px 0px 0px;
  white-space: normal;
  max-height: 35px;
  padding-left: 5px;
  background:  #05668D;
  color: white;
  border: 0px;
}
"

output$spd_header1<-renderUI({
  div(
    tags$style(train_style),
    class="well3",
    div(
      class='train_style',

      inline(
        div(
          div("~ Training Datalist:"),
          pickerInput(ns("data_spdX"),NULL,choices =names(vals$saved_data), selected=vals$cur_data)
        )
      ),

      inline(div(
        div("Model-Attribute:"),
        pickerInput(ns("model_attr"),NULL,choices =c("nb","svm","knn","rf","sgboost","xyf"), selected=vals$model_attr)
      )),
      inline(uiOutput(ns("model_out"))),
      inline(uiOutput(ns("stepX_spd_usepart"))),
      inline(uiOutput(ns("stepX_spd_valdata"))),
      inline(div(width="100%", style="border: 1px solid red"))



    ))
})



output$stepX_spd_usepart<-renderUI({
  m<-get_model()
  choices<-if(length(attr(m,'test'))==1){
    list(
      "Training Data"="train",
      "New Data"='newdata')
  } else{
    list(
      "Training Data"="train",
      "Partition"="partition",
      "Training + Partition"="trainpart",
      "New Data"="newdata"
    )
  }

  div(
    inline(
      div(
        "Predict:",
        pickerInput(ns("use_partition"), NULL,choices, selected=vals$use_partition),

      )
    ),inline(uiOutput(ns('stepX_spd_newdataX')))
  )

})

observeEvent(ignoreInit = T,input$use_partition,{
  vals$use_partition<-input$use_partition
})

getnewdata<-reactive({
  m<-get_model()
  req(input$use_partition)
  req(input$use_partition%in%c("newdata","partition","train","trainpart"))
  newdata=switch(input$use_partition,
                 "newdata"={
                   req(input$newdataX)
                   vals$saved_data[[input$newdataX]]},
                 "partition"={attr(m,"test")},
                 "train"={getdata_model(m)},
                 "trainpart"={rbind( getdata_model(m),attr(m,"test"))}
  )

  newdata
})


getobc_new<-reactive({
  m<-get_model()
  data<-vals$saved_data[[input$obc]]
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
})
getobc<-reactive({
  m<-get_model()
  req(input$use_partition)
  req(input$use_partition%in%c("newdata","partition","train","trainpart"))
  newdata=switch(input$use_partition,
                 "newdata"={getobc_new()},
                 "partition"={attr(m,"sup_test")},
                 "train"={getdata_model(m,"test")},
                 "trainpart"={c( getdata_model(m,"test"),attr(m,"sup_test"))}
  )

  newdata
})
get_pred<-reactive({
  m<-get_model()
  newdata=getnewdata()
  pred<-predict(m,newdata=newdata)
  preddf<-data.frame(pred=pred)
  rownames(preddf)<-rownames(newdata)
  preddf
})

output$stepX_spd_newdataX<-renderUI({
  if(length(input$use_partition)>0){
    req(input$use_partition=="newdata")}
  div(
    "Datalist:",
    pickerInput(ns("newdataX"),NULL,names(vals$saved_data[getobs_models()]),selected=vals$newdataX
    )
  )
})

observeEvent(ignoreInit = T,input$newdataX,{
  vals$newdataX<-input$newdataX
})


output$stepX_spd_valdata<-renderUI({
  req(input$use_partition)

  lab5<-span(paste0("Validation data (Y)"),tipify(icon("fas fa-question-circle"),"Validation dataset. Data containing observed values to be compared with predicted values"))
  div(

    inline(
      div(
        lab5,
        uiOutput(ns('out_spd_obc'))
      )
    ),
    inline(uiOutput(ns('out_spd_obcY'))),

  )
})
output$out_spd_obc<-renderUI({
  req(input$use_partition)
  req(input$use_partition=="newdata")
  req(input$newdataX)
  req(length(names(vals$spd_Ydatalists))>0)
  div(class='step2_dummy',
      pickerInput(ns("obc"),NULL,names(vals$spd_Ydatalists), selected=vals$cur_obc)
  )
})


observeEvent(ignoreInit = T,input$obc,{
  vals$cur_obc<-input$obc
})

output$out_spd_obcY<-renderUI({

  m<-get_model()
  supervisor<-attr(m,"supervisor")
  lab=switch(input$use_partition,
             "newdata"={"Column:"},
             "partition"={'From partition:'},
             "train"={'From training:'},
             "trainpart"={"From train+part:"}
  )


  div(class='step2_dummy',
      div(class="dummyY",
          em(paste0(lab),
             style="color: gray"),
          em(supervisor,
             style="color: gray"),
          tiphelp("Only for viewing the column used as supervisor of the selected models", "right")
      ))
})

observe({

  req(input$newdataX)
  newdata<-vals$saved_data[[input$newdataX]]
  m<-get_model()
  #vals<-readRDS('savepoint_lu.rds')
  #input<-readRDS('input.rds')
  sup_test<-attr(m,"supervisor")
  req(length(sup_test)>0)
  datalist<-vals$saved_data
  if(m$modelType=="Classification"){
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
  vals$spd_Ydatalists<-res0[which(res0)]
})


getobs_models<-reactive({
  datalist<-vals$saved_data
  m<-get_model()
  data<-getdata_model(m)
  res0<-unlist(
    lapply(datalist, function (x){
      res<-colnames(x)%in%colnames(data)
      sum(res)==ncol(data)
    })
  )
  names(res0[res0==T])
})




observeEvent(ignoreInit = T,input$model,{
  vals$cur_model_spd<-input$model
})
observeEvent(ignoreInit = T,input$model_attr,{
  vals$model_attr<-input$model_attr
})
observeEvent(ignoreInit = T,input$data_spdX,{
  vals$cur_data<-input$data_spdX
})
output$model_out<-renderUI({
  div(
    div("Saved models"),
    pickerInput(ns("model"),NULL,choices =names(models_from_attr()),width="150px", selected= vals$cur_model_spd)
  )
})
models_from_attr<-reactive({
  req(input$data_spdX)
  req(input$model_attr)
  data<-vals$saved_data[[input$data_spdX]]
  models<-attr(data,input$model_attr)
  models
})
get_model<-reactive({
  saved_models<-models_from_attr()
  req(input$model)
  m<-saved_models[[input$model]][[1]]
  switch(class(m$finalModel),
         "gbm"={

           vals$fm_downplot3<-NULL
         },
         "NaiveBayes"={
           vals$fm_downplot2<-NULL
           vals$fm_downplot3<-NULL
         },
         "knn"={
           vals$fm_downplot2<-NULL
           vals$fm_downplot3<-NULL
         },
         "xyf"={
           vals$fm_downplot2<-NULL
           vals$fm_downplot3<-NULL
         },
         "svm"={
           vals$fm_downplot2<-NULL
           vals$fm_downplot3<-NULL
         }




  )
  m
})

#

observeEvent(ignoreInit = T,input$tab_spd,{
  req(!is.null(input$tab_spd))
  vals$tab_spd<-input$tab_spd
})

output$spd_panels<-renderUI({
  div(style="background: white",
    tabsetPanel(
      id=ns("tab_spd"),selected=vals$tab_spd,
      tabPanel("1. Summary",value="tab1",
               uiOutput(ns("spd1_summary"))
      ),
      tabPanel("2. Performace",value="tab2",
               uiOutput(ns("spd2_performace"))
      ),
      tabPanel("3. Predictions",value="tab3",
               uiOutput(ns("spd2_predictions"))
      ),
      tabPanel("4. Permutation importance",value="tab4",
               uiOutput(ns("spd4_permimp"))
      ),
      tabPanel("5. Partial dependence",value="tab5",
               uiOutput(ns("spd5_pd"))
      ),
      tabPanel("6. Explore model features",value="tab6",
               uiOutput(ns("spd3_msp_rf"))
      ),
      tabPanel("7. Resampling plot",value="tab7",
               uiOutput(ns("resampling_panel"))
      )
    )
  )
})


##
output$resampling_panel<-renderUI({
  m<-get_model()
  validate(need(m$modelType == "Classification", "Currently only available for Classification models. Coming soon for Regression type."))
  column(12,
         column(3,class="well3",
                div(class="map_control_style",style="color: #05668D",
                    uiOutput(ns("resampling_side")),
                )),
         column(9,
                uiOutput(ns("plot_resampling"))
         )
  )
})
output$resampling_side<-renderUI({
  div(
    class="map_control_style2",style="color: #05668D",
    strong("Plot parameters:"),
    uiOutput(ns('eplot_obs_selection')),
    uiOutput(ns('eplot_plot_params')),
    uiOutput(ns("forest_saves")),
    actionLink(ns('downp_plotpred'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")))
  )
})
output$forest_saves<-renderUI({
  req(get_model()$modelType=="Regression")
  div(
    div(actionLink(ns('resamp_pred_gdownp'),"+ Download table")),
    div( actionLink(ns('resamp_pred_gcreate'),"Create Datalist"))
  )
})
output$eplot_obs_selection<-renderUI({
  div(class="cogs_in_div",style="margin-bottom: 10px; padding:5px",
      fluidPage(
        actionLink(ns("resampling_show_id_selection"),"+ Select the observations"),
        DT::dataTableOutput(ns('resampling_id_selection'))
      )
  )
})
observeEvent(ignoreInit = T,input$resampling_show_id_selection,{
  shinyjs::toggle("resampling_id_selection")
})
observeEvent(ignoreInit = T,input$resampling_class_height,{
  shinyjs::hide('resampling_params')
},once=T)
output$resampling_id_selection = DT::renderDataTable({
  m<-get_model()
  table=data.frame(id=rownames(getdata_model(m)))
  DT::datatable(table, options=list(
    dom="t",
    colnames="",
    lengthMenu = list(c(-1), c("All")),
    scrollX = TRUE,
    scrollY = "200px",
    autoWidth=F

  ), class ='compact cell-border',rownames=F,
  selection = list(mode = 'multiple', selected = c(1:20)))
})
output$eplot_plot_params<-renderUI({
  div(class="cogs_in_div",style="margin-bottom: 10px; padding:5px",
      fluidPage(
        actionLink(ns("resampling_showparams"),"+ Plot params"),
        uiOutput(ns('resampling_params'))
      ))
})
observeEvent(ignoreInit = T,input$resampling_showparams,{
  shinyjs::toggle("resampling_params")
})
output$resampling_params<-renderUI({
  div(
    uiOutput(ns('resampling_margin')),
    uiOutput(ns('resampling_class_palette')),
    numericInput(ns("resampling_class_sizeplot"),"+ Size:", value=1.5),
    numericInput(ns("resampling_class_width"),"+ Width", value=800),
    numericInput(ns("resampling_class_height"),"+ Height", value=700),
    numericInput(ns('resampling_round'),"+ Round",3),
    pickerInput(inputId = ns("resampling_class_textcolor"),
                label = "+ Text Color",
                choices =     vals$colors_img$val[getsolid_col()],
                selected="black",
                choicesOpt = list(content =     vals$colors_img$img[getsolid_col()]), options=list(container="body")),
    numericInput(ns("resampling_class_size_points"),"+ Plot size", value=3, step=1),
    textInput(ns("resampling_class_main"), "+ Plot title", ""),
    numericInput(ns("resampling_class_cex.main"),"+ Title size", value=13, step=1),
    numericInput(ns("resampling_class_cex.axes"),"+ Axes size", value=13, step=1),
    numericInput(ns("resampling_class_cex.lab"),"+ Label size", value=14, step=1),
    numericInput(ns("resampling_class_cex.leg"),"+ Label size", value=13, step=1),
    textInput(ns("resampling_class_xlab"), "+ Xlab", 'Accuracy'),
    textInput(ns("resampling_class_ylab"), "+ ylab", 'Observations'),
    textInput(ns("resampling_class_leg"), "+ Legend title", "Model predictions"),
    textInput(ns("resampling_class_leg2"), "+ Legend title", "Final prediction")

  )
})
output$resampling_margin<-renderUI({
  req(get_model()$modelType=="Regression")
  numericInput(ns("resampling_reg_margin"),"+ Margin:", value=0.05, step=0.05)
})
output$resampling_class_palette<-renderUI({
  req(get_model()$modelType=="Classification")
  pickerInput(inputId = ns("resampling_class_palette"),
              label = "+ Palette",
              choices =     vals$colors_img$val[getgrad_col()],
              choicesOpt = list(content =     vals$colors_img$img[getgrad_col()]), options=list(container="body"), width="75px")
})
output$plot_resampling<-renderUI({

  choices<-if(get_model()$modelType=="Classification"){
    c('Accuracy','Error')
  } else{
    c('RMSE','MAE')
  }
  datao<-vals$saved_data[[input$data_spdX]]
  myList <- as.list(c("Accuracy","Error"))
  names(myList)<-choices
  div(style="background: white",
      pickerInput(ns("resampling_sort"),"Sort data by",c(myList,colnames(attr(datao,"factors"))),width="200px"),
      actionButton(ns('downp_summ_trees'),tipify(icon("fas fa-download"), "Download Plot"), style="button_active"),
      uiOutput(ns("resampling_class_legend")),
      uiOutput(ns("resampling_plot_regression")),
      uiOutput(ns("resampling_plot_class")))
})
output$resampling_plot_class<-renderUI({
  req(input$resampling_class_height)
  req(input$resampling_class_width)
  req(get_model()$modelType=="Classification")
  plotOutput(ns("resampling_plot_classification"), width = paste0(input$resampling_class_width,"px"), height =paste0(input$resampling_class_height,"px"))
})
resampling_accuracies<-reactive({
  m<-get_model()
  metric<-ifelse(m$modelType=="Classification","Accuracy","Rsquared")
  traindata<-getdata_model(m)
  re<-do.call(rbind,lapply(split(m$pred,m$pred$rowIndex),function(x){
    postResample(x$pred,x$obs)[metric]
  }))
  re<-data.frame(re)
  rownames(re)<-rownames(traindata)
  re$Error<-1-re[,metric]
  re
})
resampling_predall<-reactive({
  m<-get_model()
  metric<-ifelse(m$modelType=="Classification","Accuracy","Rsquared")
  traindata<-getdata_model(m)
  re<-lapply(split(m$pred,m$pred$Resample),function(x){
    df=data.frame(pred=rep(NA,nrow(traindata)))
    rownames(df)<-rownames(traindata)
    df[x$rowIndex,]<-x$pred
    df
  })
  dd<-data.frame(re)
  res<-data.frame(t(apply(dd,1,function(x){
    table(factor(x, levels=m$levels))
  })))
  colnames(res)<-m$levels
  res
})
output$resampling_plot_classification<- renderPlot({
  req(length(input$resampling_id_selection_rows_selected)>0)
  req(input$resampling_class_palette)
  m<-get_model()
  req(m$modelType=="Classification")
  newdata<-getdata_model(m)
  selected=rownames(newdata)[input$resampling_id_selection_rows_selected]
  # selected=1:5
  obc=getdata_model(m,"test")
  names(obc)<-rownames(newdata)
  obc<-obc[selected]
  all_pred=resampling_predall()
  pred=data.frame(pred=factor(apply(all_pred,1,which.max)[selected],levels=m$levels))
  predtab<-all_pred[selected,]
  sortby=input$resampling_sort
  # sortby="Accuracy"
  res_forest<- predtab[selected,]
  datao<-vals$saved_data[[input$data_spdX]]
  data_factors<-  attr(datao,"factors")[rownames(datao),][selected,]
  # sortby<-"Accuracy"
  resacc<-resampling_accuracies()
  if(sortby%in%c("Accuracy","Error")) {
    ord<-switch (sortby,
                 "Accuracy" =order(resacc[rownames(res_forest),"Accuracy"], decreasing = T),
                 "Error"=order(resacc[rownames(res_forest),"Error"], decreasing = T))
  } else {
    sortby<-data_factors[rownames(res_forest),sortby, drop=F]
    df<-cbind(res_forest,sortby)
    ord<-order(df[,ncol(df)])
  }
  ord<-rownames(res_forest)[ord]
  #args<-readRDS("ord.rds")
  result_forest<-res_forest[ord,]
  pred_res<-pred[rownames(result_forest),,drop=F]
  obc_res<-obc[rownames(result_forest)]
  args<-list(
    result_forest=result_forest,
    pred=pred_res,
    palette=input$resampling_class_palette,
    newcolhabs=vals$newcolhabs,
    ylab=input$resampling_class_ylab,
    xlab=input$resampling_class_xlab,
    leg=input$resampling_class_leg,
    leg2=input$resampling_class_leg2,
    cex.axes=input$resampling_class_cex.axes,
    cex.lab=input$resampling_class_cex.lab,
    cex.main=input$resampling_class_cex.main,
    cex.leg=input$resampling_class_cex.leg,
    textcolor=input$resampling_class_textcolor,
    main=input$resampling_class_main,
    obc=obc_res,
    size_points=input$resampling_class_size_points
  )
  # plotforest_qclass(result_forest,    pred=pred_res, palette="turbo", newcolhabs=vals$newcolhabs, obc=obc_res)
  vals$vartrees<-do.call(plotforest_qclass,args)
  vals$vartrees
})
output$resampling_class_legend<-renderUI({
  req(get_model()$modelType=="Regression")
  div(style="text-align: right",
      div(
        div(inline(div(style="border-bottom: 2px dashed darkblue; width: 60px")),'Observed'),
        div(inline(div(style="border-bottom: 2px dashed SeaGreen; width: 60px")),paste("Mean predictions"))

      ))
})
output$resampling_plot_regression<-renderUI({
  req(get_model()$modelType=="Regression")
  plotOutput(ns("plot_resampling_plot_regression"), width = paste0(input$resampling_class_width,"px"), height =input$resampling_class_height)
})
output$plot_resampling_plot_regression<-renderPlot({})

##

output$spd2_predictions<-renderUI({
  sidebarLayout(
         sidebarPanel(
                div(class="map_control_style2",style="color: #05668D",
                    uiOutput(ns("predictions_side")),

                )),
         mainPanel(
                uiOutput(ns("predictions_main"))
         ))
})


output$predictions_side<-renderUI({
  div(
    actionLink(
      ns('predictions_create'),span("+ Create Datalist",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
    )
  )
})
####
data_overwritte<-reactiveValues(df=F)
data_store<-reactiveValues(df=F)
newname<-reactiveValues(df=0)
get_newname<-reactive({
  req(!is.null(vals$hand_save))
  newname$df<-switch(
    vals$hand_save,
    "Create Datalist with model predictions"={ name_datalist_predictions()}
  )})
module_create_spd <- function() {
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
           radioButtons(
             ns("hand_save"),NULL,
             choiceNames= list(
               div(style="height: 40px",
                   span("Create", style="margin-right: 15px"),
                   inline(uiOutput(ns("data_create")))),
               div(style="height: 40px",
                   span("Overwrite", style="margin-right: 15px"),
                   inline(uiOutput(ns("data_over"))))
             ),
             choiceValues=list('create',"over"), width="800px"
           )
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
name_datalist_predictions<-reactive({
  name0<- "Ensemble predictions"
  name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
  name1[length(vals$saved_data)+1]
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
    "Create Datalist with model predictions"={create_predictions()}
  )
  removeModal()

})
observeEvent(ignoreInit = T,input$predictions_create,{
  vals$hand_save<- "Create Datalist with model predictions"
  vals$hand_save3<-NULL
  showModal(module_create_spd())
})
create_predictions<-reactive({
  pred<-get_pred()
  datao<-vals$saved_data[[input$data_spdX]]
  newdata<-datao[rownames(pred),]
  factors<-attr(datao,"factors")[rownames(pred),]
  coords<-attr(datao,"factors")[rownames(pred),]
  base_shape<-attr(datao,"base_shape")
  layer_shape<-attr(datao,"layer_shape")
  extra_shape<-attr(datao,"extra_shape")
  name_factor<-paste0("Pred",input$model_attr,input$model, collapse=".")
  if(is.factor(unlist(pred))){
    factors[name_factor]<-pred[,1]
  } else{
    newdata[name_factor]<-pred[,1]
  }
  newdata<-data.frame(newdata)
  attr(newdata,"factors")<-factors
  attr(newdata,"coords")<-coords
  attr(newdata,"base_shape")<-base_shape
  attr(newdata,"layer_shape")<-layer_shape
  attr(newdata,"extra_shape")<-extra_shape
  if(input$hand_save=="create") {
    vals$saved_data[[input$newdatalist]]<-newdata
  } else{
    vals$saved_data[[input$over_datalist]]<-newdata
  }
  beep(10)
})
###

output$predictions_main<-renderUI({
  column(12,
         column(8,inline(DT::dataTableOutput(ns('predictions_table')))),
         column(4,uiOutput(ns('predictions_summary')))

  )
})

output$predictions_summary<-renderUI({
  Predicted<-unlist(get_pred())
  req(is.factor(Predicted))
  Observed<-unlist(getobc())
  obs_nlevels<-levels(Observed)
  pred_nlevels<-levels( droplevels(Predicted))

  match<-sum(Predicted==Observed)
  wrong<-length(Predicted)-match
  div(

    div(strong("Observed levels:"),em( paste(obs_nlevels, collapse = ";"))),
    div(strong("Predicted levels:"),em( paste(pred_nlevels, collapse = ";"))),
    div(strong("Correct predictions:"),em(match)),
    div(strong("Incorrect predictions:"),em(wrong))


  )
})

output$predictions_table<-DT::renderDataTable({
  Observed<-getobc()
  Predicted<-get_pred()
  if(is.factor(Predicted[,1])){
    Match<-unlist(Observed)==unlist(Predicted)
    df<-data.frame(Predicted,Observed, Match)
  } else{
    df<-data.frame(Predicted,Observed)
    RMSE<- apply(df,1,function(x)RMSE(x[1],x[2]) )
    df$RMSE<-RMSE
    df
  }

  df},options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='cell-border compact stripe')



##
output$spd3_msp_rf<-renderUI({
  m<-get_model()
  validate(need(m$modelType == "Classification", "Currently only available for Classification models. Coming soon for Regression type."))
  column(12,
         column(3,class="well3",

                div(class="map_control_style2",style="color: #05668D",
                    uiOutput(ns("side_msp")),
                    uiOutput(ns("side_msp_rf")),
                    uiOutput(ns("side_msp_nb")),
                    uiOutput(ns("side_msp_pairs")),
                    uiOutput(ns("side_msp_sgboost")),
                    uiOutput(ns("side_msp_xyf")),
                    uiOutput(ns('msp_downplot_links'))

                )),
         column(9,
                uiOutput(ns("msp_plots"))
         ))
})

output$msp_plots<-renderUI({
  div(
    uiOutput(ns("msp_choices")),
    uiOutput(ns("msp_nb")),
    uiOutput(ns("msp_rf")),
    uiOutput(ns("msp_sgboost")),
    uiOutput(ns("msp_svm")),
    uiOutput(ns("msp_xyf")),
    withSpinner(uiOutput(ns("msp_pairs")),8)


  )
})
choices_msp_names<-reactive({
  switch(input$model_attr,
         "nb"={c("Pairs","Densities")},
         "svm"={c("Pairs","Feature importance")},
         "knn"={c("Pairs")},
         "rf"={c("Pairs","Individual tree from RF","Feature importance")},
         "sgboost"={c("Pairs","Feature importance","Error vs ntrees")},
         "xyf"={c("Pairs","Codebook")}
  )
})


choices_msp<-reactive({
  switch(input$model_attr,
         "nb"={paste0("Plot",1:2)},
         "svm"={paste0("Plot",1:2)},
         "knn"={paste0("Plot",1)},
         "rf"={paste0("Plot",1:3)},
         "sgboost"={paste0("Plot",1:3)},
         "xyf"={paste0("Plot",1:2)}
  )
})

output$msp_choices<-renderUI({

  radioGroupButtons(ns("msp_radio"),"Plot type:",choiceValues =choices_msp(),choiceNames =choices_msp_names())



})



output$side_msp_pairs<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot1")
  m<-get_model()
  data=getdata_model(m)
  req(input$msp_plot_base_size)
  bs=round(input$msp_plot_base_size/12,2)





  req(class(m)[1]=="train")
  div(

    pickerInput(ns("ggpair.variables"),span("+ Variables:",class='text_alert'),colnames(data), multiple = T,options=list(`actions-box` = TRUE), selected=colnames(data)[1:3], width="220px"),


    pickerInput(ns("ggpair.include.y"),"+ Y plot:",list("omit"=F,"include"=T)),
    pickerInput(ns("ggpair.method"),"+ Correlation method:",c("none","pearson", "kendall", "spearman")),
    numericInput(ns("ggpair.round"),"+ Digits:",3),

    pickerInput(ns("ggpair.switch"),"+ Switch:",list("default"=NULL,"x"="x","y"="y","both"="both")),


    numericInput(ns("ggpair.varnames.size"),"+ Variable name size:",bs*1.4),

    numericInput(ns("ggpair.cor.size"),"+ Corr size:",bs*1.4),

    pickerInput(inputId = ns("ggpair.pch"),
                label = "+ Point shape",
                choices = df_symbol$val,
                choicesOpt = list(content = df_symbol$img),
                options=list(container="body"), width="100px",
                selected=vals$xyf_symbol),
    numericInput(ns("ggpair.points.size"),"+ Points size",bs),
    #numericInput(ns("ggpair.plot.subtitle.size"),"+ plot.subtitle.size",bs),
    numericInput(ns("ggpair.legend.text.size"),"+ legend.text.size:",bs),
    numericInput(ns("ggpair.legend.title.size"),"+ legend.title.size:",bs),
    numericInput(ns("ggpair.alpha.curve"),"+ Curve transparency:",.8),

    textInput(ns("ggpair.title"),"+ Title:",""),
    numericInput(ns("ggpair.plot.title.size"),"+ Title size:",bs),
    textInput(ns("ggpair.xlab"),"+ xlab:",""),
    textInput(ns("ggpair.ylab"),"+ ylab:",""),
    numericInput(ns("ggpair.axis.text.size"),"+ Axis tick size:",bs),
    numericInput(ns("ggpair.axis.title.size"),"+ Axis label size:",bs),
    inline(uiOutput(ns('ggpair.title_corr')))


    #checkboxInput(ns("knn_plotgrid"),"+ Grid",F),



  )
})
output$ggpair.title_corr<-renderUI({
  req(input$ggpair.method)
  textInput(ns("ggpair.title_corr"),"+ title_corr:",paste(input$ggpair.method,"corr"))
})





output$gg_run_btn<-renderUI({

  req(is.null(vals$gg_run))
  div(class="save_changes",
      actionButton(ns('gg_run'), 'Update plot', icon=icon("fas fa-sync"))
  )


})
observeEvent(ignoreInit = T,input$gg_run,{
  vals$fm_downplot4<-get_ggpair()
  vals$gg_run<-F
})

observeEvent(get_ggpair_args(),{
  vals$gg_run<-NULL

})

observeEvent(get_ggpair_args(),{
  vals$gg_run<-F
  vals$fm_downplot4<-get_ggpair()
}, once=T)

observeEvent(get_model(),{
  vals$fm_downplot4<-NULL
  vals$fm_downplot4<-get_ggpair()
  vals$gg_run<-F
})

output$bug<-renderUI({
  renderPrint({
    length(input$ggpair.title)
  })
})
get_ggpair_args<-reactive({

  req(input$msp_radio)
  req(input$msp_radio=="Plot1")
  req(input$ggpair.variables)
  req(input$fm_palette)
  m<-get_model()
  req(class(m)[1]=="train")
  data<-getnewdata()[,input$ggpair.variables]
  y<-pred<-get_pred()
  colnames(y)<-colnames(pred)<-attr(m,"supervisor")
  cols<-vals$newcolhabs[[input$fm_palette]](length(m$levels))
  my_cols<-cols[pred$pred]
  # library(GGally)
  df=cbind(data,pred)
  size=input$msp_plot_base_size*.09

  if(input$ggpair.method=="none"){
    upper="blank"
  } else{
    upper="cor"
  }
  args<-list(x=data,y=y,
             cols=cols,
             method=input$ggpair.method,
             round=input$ggpair.round,
             switch=input$ggpair.switch,
             plot.title.size=input$ggpair.plot.title.size,
             axis.text.size=input$ggpair.axis.text.size,
             axis.title.size=input$ggpair.axis.title.size,
             cor.size=input$ggpair.cor.size,
             varnames.size=input$ggpair.varnames.size,
             points.size=input$ggpair.points.size,
             legend.text.size=input$ggpair.legend.text.size,
             legend.title.size=input$ggpair.legend.title.size,
             alpha.curve=input$ggpair.alpha.curve,

             title=input$ggpair.title,
             xlab=input$ggpair.xlab,
             ylab=input$ggpair.ylab,
             upper=upper,
             title_corr=input$ggpair.title_corr,
             include.y=input$ggpair.include.y,
             pch=as.numeric(input$ggpair.pch)


  )

  req(   !any(sapply(args,length)<1))
  # attach(args)
  args

})

get_ggpair<-reactive({
  args<-get_ggpair_args()
  # attach(args)
  p<-do.call(gg_pairplot,args)

  p
})


observeEvent(ignoreInit = T,input$ggpair.variables,{
  req(length(input$ggpair.variables)>0)
  vals$fm_downplot4<-get_ggpair()
  vals$gg_run<-T
}, once=T)




output$msp_pairs<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot1")
  res<-div(
    uiOutput(ns("gg_run_btn")),
    renderPlot(vals$fm_downplot4,  width=input$msp_plot_width,height=input$msp_plot_height),
    em(attr(vals$fm_downplot4,"row1"), style="color: gray")
  )
  vals$show_ggrun<-F
  res
})
output$msp_svm<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")
  base_size =input$msp_plot_base_size
  m<-get_model()
  svm<-m$finalModel
  req(class(svm)=="ksvm")
  cols <- vals$newcolhabs[[input$fm_palette]](length(m$levels))
  df <- do.call(rbind, lapply(1:length(svm@coef), function(i) {
    coefs <- m$finalModel@coef[[i]]
    mat <- m$finalModel@xmatrix[[i]]
    coefs %*% mat
  }))

  feature_importance <- apply(df, 2, function(x) sum(abs(x)))
  # Normalize the feature importance values
  normalized_importance <- feature_importance / sum(feature_importance)

  library(ggplot2)
  # Create a data frame of feature importance
  feature_importance_df <- data.frame(Feature = names(feature_importance), Importance = normalized_importance)
  # Sort the data frame by importance values in descending order
  feature_importance_df <- feature_importance_df[order(feature_importance_df$Importance, decreasing = TRUE), ]

  p1 <- ggplot(feature_importance_df, aes(x = Importance, y = reorder(Feature, Importance), fill = Importance)) +
    geom_bar(stat = "identity") +
    labs(x = "Feature Importance (sum of absolute SVM Coefficients)", y = "Feature", title = "") +
    theme(
      plot.tag.position = "bottom",
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = NA, color = "white"),
      panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
      axis.line = element_line(),
      axis.text = element_text(size = base_size),
      axis.title = element_text(size = base_size),
      plot.title = element_text(size = base_size)
    ) + scale_fill_gradientn(colours = cols)
  vals$fm_downplot<-p1
  column(12,
         tags$style(".popup_svm { display: none; position: absolute; background-color: white; border: 1px solid gray; padding: 10px; width: 400px}"),
         h4("Feature importance",
            icon('fas fa-question-circle'),
            id = "svm_fi_help",
            onmouseover = "document.getElementById('svm_fi_help_text').style.display = 'block'",
            onmouseout = "document.getElementById('svm_fi_help_text').style.display = 'none'"
         ),
         renderPlot(p1, width=input$msp_plot_width,height=input$msp_plot_height),
         div(
           id = "svm_fi_help_text",
           class = "popup_svm",
           style = "left: 200px; top: 25px;",
           p("This plot calculates feature importance by multiplying the SVM coefficients with the corresponding feature values. The resulting values are summed to quantify the overall importance of each feature across all classes in the SVM model. The feature importance values are then normalized to allow for a relative comparison among different features."),

         )
  )


})
output$side_msp_sgboost<-renderUI({
  m<-get_model()
  gbm<-m$finalModel
  req(class(gbm)=="gbm")
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")
  div(

    numericInput(ns("gbm_nvars"), "+ Number of variables",20)

  )
})
output$msp_sgboost<-renderUI({
  m<-get_model()
  gbm<-m$finalModel
  req(class(gbm)=="gbm")
  div(
    uiOutput(ns("msp_sgboost_plot1")),
    uiOutput(ns("msp_sgboost_plot2"))
  )
})

output$msp_sgboost_plot1<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")
  graphics.off()
  req(input$msp_plot_width)
  req(input$msp_plot_height)
  m<-get_model()
  gbm<-m$finalModel
  req(class(gbm)=="gbm")
  gbm_inf<-na.omit(summary(gbm)[1:input$gbm_nvars,])

  cols<-vals$newcolhabs[[input$fm_palette]](length(m$levels))
  p<-ggplot(gbm_inf,aes(x=rel.inf,y=reorder(var,rel.inf), fill=rel.inf))+geom_bar(stat="identity")+xlab("Relative influence")+ylab("Variables") +scale_fill_gradientn(colours=cols)
  vals$fm_downplot<-p
  renderPlot({
    vals$fm_downplot
  })
})



output$msp_sgboost_plot2<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot3")
  renderPlot({
    m<-get_model()
    newdata<-getnewdata()
    obc<-getobc()
    gbm<-m$finalModel
    nmax_trees=gbm$tuneValue$n.trees-1
    n.trees=1:nmax_trees

    metric<-ifelse(m$modelType=="Classification","Accuracy","Rsquared")

    predlist<-predict(gbm, newdata, type = "response", n.trees =n.trees)
    df_pred<-data.frame(do.call(rbind,lapply(1:dim(predlist)[3],function(x){
      predd=apply(predlist[,,x],1,function(xx) which.max(xx))
      predd

    })))
    test.error<-1-apply(df_pred,1,function(x) postResample(factor(x),obc))[metric,]

    df<-data.frame(x=n.trees,y=test.error)
    p<-ggplot(df,aes(x,y), color="blue")+geom_line()+xlab("Number of Trees")+ylab("Test Error")+ggtitle("Perfomance on predictions")
    vals$fm_downplot3<-p
    p
  })
})

output$msp_rf_feat<-renderUI({})


output$side_msp_nb<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")

  m<-get_model()
  req(class(m$finalModel)=="NaiveBayes")
  div(class="cogs_in_div",style="margin-bottom: 10px;",
      fluidPage(
        pickerInput(ns("msp_nb_vars"),"+ Select the variable",colnames(getdata_model(m))),


        numericInput(ns("nb_lwd"), "+ Line width",2),

        checkboxInput(ns('nb_wrap'),"+ Wrap", T),
        pickerInput(ns("nb_type_plot"),"Type:",c("identity","stack","fill"))



      )
  )


})




output$msp_nb<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")

  m<-get_model()
  req(class(m$finalModel)=="NaiveBayes")
  req(input$msp_plot_width)
  req(input$msp_plot_height)
  nb<-m$finalModel
  req(length(input$msp_nb_vars)>0)
  cols<-vals$newcolhabs[[input$fm_palette]](length(m$levels))
  newdata<-getnewdata()
  probs<-predict(m,newdata,"prob")
  pred<-get_pred()
  prediction<-unlist(pred)
  obc<-getobc()
  var=  input$msp_nb_vars



  #x=newdata[vars],y=pred[,1],input$nb_type_plot, pch=16, col=cols, cex=input$msp_plot_base_size
  renderPlot({
    nb_densities<-pnb(nb,vars=var)[[1]]
    colnames(nb_densities)
    nb_densities$class<-factor(nb_densities$class)
    nb_densities$var<-factor(nb_densities$var)
    df<-nb_densities
    df$class<-factor(df$class)
    p<- ggplot(df, aes(x=x, y=y, fill=class)) +
      geom_area(position=input$nb_type_plot,alpha=0.6 , size=.5, colour="white")+scale_fill_manual(values=cols)
    if(isTRUE(input$nb_wrap)){
      p<- p  +facet_wrap(~class, scales = "free",ncol=1)+xlab(var)+ylab("Density")
    }
    p<-p+ggtitle(var)+theme_bw(base_size =input$msp_plot_base_size)+theme(
      panel.grid.major = element_blank(),
      panel.background=element_rect(fill=NA, color="white"),
      panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),

      axis.line=element_line(),
      axis.text=element_text(size=input$msp_plot_base_size),
      axis.title=element_text(size=input$msp_plot_base_size),
      plot.title=element_text(size=input$msp_plot_base_size),
      plot.subtitle=element_text(size=input$msp_plot_base_size),
      legend.text=element_text(size=input$msp_plot_base_size),
      legend.title=element_text(size=input$msp_plot_base_size)
    )
    vals$fm_downplot<-p
    vals$fm_downplot

    #p<-pnb(nb,vars=var)
    ##plot(   p[[1]][2:3], type="n", xlab=var, ylab="Density")
    # pl<-split( p[[1]], p[[1]]$class)
    # lapply(1:length(pl), function(i){lines(pl[[i]][2:3], col=cols[i], lwd=input$nb_lwd)})
  }, width=input$msp_plot_width,height=input$msp_plot_height)


})

output$msp_rf<-renderUI({
  m<-get_model()
  req(class(m$finalModel)=="randomForest")
  div(
    uiOutput(ns('spdfm_plottree')),
    uiOutput(ns('spdfm_cm')),
    uiOutput(ns('msp_rf_feature'))
  )
})

output$msp_rf_feature<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot3")
  req(input$rf_nvars)

  m<-get_model()
  req(m$modelType=="Classification")
  req(class(m$finalModel)=="randomForest")
  imp<-data.frame(m$finalModel$importance)
  imp<-imp['MeanDecreaseGini']
  imp<-na.omit(imp[order(imp[,1], decreasing=T),,drop=F][1:input$rf_nvars,, drop=F])
  colnames(imp)<-"value"
  imp<-data.frame(var=rownames(imp),imp)

  cols<-vals$newcolhabs[[input$fm_palette]](length(m$levels))
  p<-ggplot(imp,aes(x=value,y=reorder(var,value), fill=value))+geom_bar(stat="identity")+xlab("Mean Decrease Gini")+ylab("Variables") +scale_fill_gradientn(colours=cols)+theme(

    panel.grid.major = element_blank(),
    panel.background=element_rect(fill=NA, color="white"),
    panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),

    axis.line=element_line(),
    axis.text=element_text(size=input$msp_plot_base_size),
    axis.title=element_text(size=input$msp_plot_base_size),
    plot.title=element_text(size=input$msp_plot_base_size),
    plot.subtitle=element_text(size=input$msp_plot_base_size),
    legend.text=element_text(size=input$msp_plot_base_size),
    legend.title=element_text(size=input$msp_plot_base_size)

  )
  vals$fm_downplot3<-p

  div(
    p(strong("Feature plot from Training Data")),
    renderPlot({
      vals$fm_downplot3
    }, width=input$msp_plot_width,height=input$msp_plot_height)
  )

})

output$side_msp_rf<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")
  m<-get_model()
  req(class(m$finalModel)=="randomForest")
  predall<-predall_rf()

  div(class="cogs_in_div",style="margin-bottom: 20px;",
      pickerInput(ns("fm_tree"),"+ Tree",colnames(predall)),

      numericInput(ns('fm_round'),'+ Round',2),
      pickerInput(ns("edge_type"), "+ Edge type", choices=list("Link"="link",'Fluid'="fluid")),

      pickerInput(ns("tree_type"), "+ Tree type", choices=list("Dendrogram"="dendrogram","Tree"='tree')),
      uiOutput(ns('rf_nvars_side'))

  )
})


output$rf_nvars_side<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot3")
  numericInput(ns('rf_nvars'),'+ Number of variables (feature plot)',20)
})
output$side_msp<-renderUI({
  div(
    pickerInput(inputId = ns("fm_palette"),
                label = '+ Palette',
                choices =     vals$colors_img$val,
                choicesOpt = list(content =vals$colors_img$img),
                selected=vals$cm_palette,
                options=list(container="body")),
    numericInput(ns("msp_plot_width"), "+ Plot width",800),
    numericInput(ns("msp_plot_height"), "+ Plot height",600),
    numericInput(ns("msp_plot_base_size"),"+ Base size",12),
  )

})



output$msp_downplot_links<-renderUI({
  div(
    uiOutput(ns("msp_downplot1")),
    uiOutput(ns("msp_downplot2")),
    uiOutput(ns("msp_downplot3")),
    uiOutput(ns("msp_downplot4"))


  )
})

output$msp_downplot1<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")
  actionLink(
    ns('fm_downplot'),span("+ Download plot 2",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
  )
})
output$msp_downplot2<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")
  m<-get_model()
  req(m$modelType=="Classification")
  req(class(m$finalModel)=="randomForest")

  div( actionLink(
    ns('fm_downplot2'),span("+ Download plot 2.2",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
  ))
})
output$msp_downplot3<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot3")
  div(actionLink(
    ns('fm_downplot3'),span("+ Download plot 3",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
  ))
})

output$msp_downplot4<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot1")
  div(  actionLink(
    ns('fm_downplot4'),span("+ Download plot 1",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
  ))
})

predall_rf<-reactive({
  m<-get_model()
  req(class(m$finalModel)=="randomForest")
  obc<-getobc()
  newdata=getnewdata()
  treepred=predict(m$finalModel,newdata,predict.all=T)$individual
  colnames(treepred)<-paste0("tree",1:ncol(treepred))
  rownames(treepred)<-rownames(newdata)
  acc_trees<-apply(treepred,2,function(x) postResample(x,obc))
  metric<-ifelse(m$modelType=="Classification","Accuracy","Rsquared")
  neword<-order(acc_trees[metric,], decreasing = T)
  tree_predictions<-data.frame(treepred[,neword])
  tree_predictions<-data.frame(lapply(tree_predictions,function(x) factor(x,levels=levels(m))))
  rownames(tree_predictions)<-rownames(newdata)
  tree_predictions
})

output$spdfm_plottree<-renderUI({
  vals$msp_tree<-F
  req(input$msp_plot_width)
  req(input$msp_plot_height)
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")
  m<-get_model()
  req(class(m$finalModel)=="randomForest")
  predall=predall_rf()
  req(length(predall)>0)
  req(input$fm_tree)
  tree_num=as.numeric(gsub("tree","",colnames(predall[input$fm_tree])))

  obc<-getobc()
  pred<-unlist(get_pred())
  acc<-postResample(pred,obc)
  div(style="margin-bottom: 20px",
      div(

        renderPlot({
          vals$fm_downplot<-NULL
          vals$fm_downplot<-tree_func(final_model = m$finalModel,

                                      acc=round(acc,input$fm_round),
                                      base_size=input$msp_plot_base_size,
                                      tree_num=tree_num,
                                      newcolhabs=vals$newcolhabs,
                                      palette=input$fm_palette,
                                      round=input$fm_round,
                                      tree_type=input$tree_type,
                                      edge_type=input$edge_type
          )
          vals$fm_downplot
        }, width=input$msp_plot_width,height=input$msp_plot_height)
      )

  )

})

output$spdfm_cm<-renderUI({
  req(input$msp_radio)
  req(input$msp_radio=="Plot2")
  m<-get_model()
  req(m$modelType=="Classification")
  req(class(m$finalModel)=="randomForest")

  newdata=getnewdata()
  obs<-getobc()
  req(input$fm_tree)
  pred=predall_rf()[,input$fm_tree]
  conf<-table(unlist(pred),obs)
  nobs<-nrow(newdata)
  supervisor<-attr(m,"supervisor")
  req(!is.null(vals$fm_downplot))
  lab0=switch(input$use_partition,
              "newdata"={paste0("New Data: ",input$newdataX ,"(",paste0(nobs)," obs.)")},
              "partition"={paste0('Partition Data (',paste0(nobs)," obs.)")},
              "train"={paste0("Training Data ",input$newdataX ,"(",paste0(nobs)," obs.)")},
              "trainpart"={paste0("Training + Partion Data ",input$newdataX ,"(",paste0(nobs)," obs.)")}
  )

  div(style="margin-top: 20px;",
      div(
        em("Confusion Matrix of ",strong(input$fm_tree)," in predicting ",strong(supervisor)," from ",strong(lab0))
      ),
      renderPlot({
        p<-plotCM(conf/sum(conf)*100,input$fm_palette, newcolhabs=vals$newcolhabs, title=input$fm_tree, round_cm=input$fm_round)
        vals$fm_downplot2<-p
        p
      })
  )

})

observeEvent(ignoreInit = T,input$fm_downplot,{
  vals$hand_plot<-"Feature plot"
  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
})


observeEvent(ignoreInit = T,input$fm_downplot2,{
  vals$hand_plot<-"Feature plot 2"
  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
})


observeEvent(ignoreInit = T,input$fm_downplot3,{
  vals$hand_plot<-"Feature plot 3"
  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
})

observeEvent(ignoreInit = T,input$fm_downplot4,{
  vals$hand_plot<-"Pairs plot"
  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
})


##

output$spd1_summary<-renderUI({
  m<-get_model()
  req(length(m)>0)
  div(style="background: white",

      div(
        inline(
          div(class="well3",
              div(style="margin: 5px",
                  div(strong('Training data (X):'),em(attr(m,"Datalist"))),
                  div(strong('Independent variable (Y):'),em(attr(m,"Y"))),
                  div(strong('Partition:'),em(attr(m,"test_partition")))



              )
          )
        ),
        inline(
          div(popify(actionLink(ns("down_rf_results"), "Download model",style = "button_active"),NULL,"download rf results as rds file"))
        )


      ),
      splitLayout(
        div(renderPrint(gettrain(get_model()))),
        div(renderPrint(get_model()$results))
      ),
      plotOutput(ns("summary_plot"))

  )


})

output$summary_plot<-renderPlot({
  req(length(get_model())>0)
  m<-get_model()
  res<-m$results

  plot(get_model())


})

output$down_rf_results <- {
  downloadHandler(
    filename = function() {
      paste0(paste0(input$model_attr,"_",input$model),"_", Sys.Date(),".rds")
    }, content = function(file) {
      saveRDS(get_model(),file)
    })
}

##
output$spd2_performace<-renderUI({
  validate(need(length(getobc())>0,"Validation data for performance calculation not found."))
  sidebarLayout(
         sidebarPanel(
                div(class="map_control_style2",style="color: #05668D",

                    pickerInput(ns("cm_from"),"+ Show perfomace from",choices =c("Predictions","Resampling"), selected=vals$cur_data)

                    ,
                    pickerInput(inputId = ns("cm_palette"),
                                label = '+ Palette',
                                choices =     vals$colors_img$val,
                                choicesOpt = list(content =vals$colors_img$img),
                                selected=vals$cm_palette,
                                options=list(container="body")),
                    numericInput(ns('cm_round'),'+ Round',3),
                    div(uiOutput(ns("sidecm_title")), style="width: 100%"),
                    uiOutput(ns("sidecm_down_table")),
                    uiOutput(ns("sidecm_down_plot"))
                )
         ),
         mainPanel(
                uiOutput(ns("out_perf"))
         ))
})



output$sidecm_title<-renderUser({
  req(input$cm_from)
  title_a<-switch(input$use_partition,
                  "newdata"={"New Data"},
                  "partition"={"Partition"},
                  "train"={"Training Data"},
                  "trainpart"={"Training +Test data"}
  )

  title_a<-switch(input$cm_from,
                  "Predictions"={title_a},
                  "Resampling"={"Resampling"})

  inline(textInput(ns("cm_title"), "+ Title",title_a ))
})
output$sidecm_down_table<-renderUI({  div(
  tipify(
    actionLink(
      ns('download_table_perf'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
    ),
    "+ Download Table", options=list(container="body")
  ),
  hr()
)
})
output$sidecm_down_plot<-renderUI({
  div(
    tipify(
      actionLink(
        ns('down_cm_plot'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
      ),
      "+ Download plot", options=list(container="body")
    )
  )
})
output$out_perf<-renderUI({
  validate(need(length(getobc())>0,"Validation data for performance calculation not found."))
  div(
    uiOutput(ns("perf_classif")),
    uiOutput(ns("perf_regression"))
  )

})
output$perf_classif<-renderUI({
  m<-get_model()
  req(m$modelType=="Classification")
  div(
    div(
      inline(DT::dataTableOutput(ns('perf_global_classif')))
    ),
    uiOutput(ns("conf_matrix")),
    uiOutput(ns("conf_matrix_print"))

  )
})
output$perf_global_classif<-DT::renderDataTable({
  m<-get_model()
  req(input$cm_from)
  pred<-switch(input$cm_from,
               "Predictions"={get_pred()},
               "Resampling"={m$pred$pred})
  obc<-switch(input$cm_from,
              "Predictions"={getobc()},
              "Resampling"={m$pred$obs})

  vals$comb_perfomace_class<-postResample(pred,obc)
  DT::datatable({round( data.frame(as.list(vals$comb_perfomace_class)),input$cm_round)}, options=list(
    rownames=T,
    info=FALSE,autoWidth=T,dom = 't', rownames = TRUE,class ='compact cell-border'))

})
output$perf_regression<-renderUI({
  m<-get_model()
  req(m$modelType=="Regression")
  div(
    div(
      p(strong("Global:")),
      inline(DT::dataTableOutput(ns('perf_global_regression')))
    )

  )

})
get_conf_matrix<-reactive({
  m<-get_model()
  req(input$cm_from)
  pred<-switch(input$cm_from,
               "Predictions"={get_pred()},
               "Resampling"={m$pred$pred})
  obs<-switch(input$cm_from,
              "Predictions"={getobc()},
              "Resampling"={m$pred$obs})
  #req(length(as.vector(pred))==length(as.vector(obs)))
  pred=unlist(pred)
  conf<-table(obs,pred)
  conf
})
output$conf_matrix_print<-renderUI({
  conf<-get_conf_matrix()
  vals$ensemble_confusion<-data.frame(conf)
  renderPrint(confusionMatrix(conf))
})
output$conf_matrix<-renderUI({
  conf<-get_conf_matrix()
  req(input$cm_title)
  req(input$cm_palette)

  res<-switch(input$cm_from,
              "Predictions"={
                plotCM(conf/sum(conf)*100,input$cm_palette, newcolhabs=vals$newcolhabs, title=input$cm_title, round_cm=input$cm_round)
              },
              "Resampling"={
                m<-get_model()
                attr(m,'title')<- attr(getConfusion(m),'title')
                plotCM(m, input$cm_palette,  newcolhabs=vals$newcolhabs, title=input$cm_title, round_cm=input$cm_round)
              })
  renderPlot({
    vals$combcm<-res
    vals$combcm
  })
})
output$perf_global_regression<-DT::renderDataTable({
  DT::datatable({
    req(input$cm_from)
    pred<-switch(input$cm_from,
                 "Predictions"={get_pred()},
                 "Resampling"={m$pred$pred})
    obc<-switch(input$cm_from,
                "Predictions"={getobc()},
                "Resampling"={m$pred$obs})
    table<-caret::postResample(pred,obc)
    data.frame(as.list(table))
  }, options=list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T,dom = 'lt'), rownames = TRUE,class ='compact cell-border')
})


observeEvent(ignoreInit = T,input$cm_palette,{
  vals$cm_palette<-input$cm_palette})
observeEvent(ignoreInit = T,input$cm_round,{
  vals$cm_round<-input$cm_round})
observeEvent(ignoreInit = T,input$download_table_perf,{
  vals$down_ensemble<-data.frame(vals$ensemble_confusion)
  vals$hand_down<- "confusion"
  module_ui_downcenter("downcenter")
  mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
})
observeEvent(ignoreInit = T,input$down_cm_plot,{
  vals$plot_ensemble<-vals$combcm
  vals$hand_plot<-"Confusion Matrix"
  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
})

##
output$spd4_permimp<-renderUI({
  req(input$model)

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
                  tabPanel("4.3. Results",value="tab4",
                           div(
                             actionLink(
                               ns('spd_varImp'),span("+ Download table"),style="button_active"
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
    )),
    inline(uiOutput(ns('side_filter_feature_plot_nvars')))
  )
})
output$feature_importance_side<-renderUI({
  div(class="map_control_style2",style="color: #05668D",


      numericInput(ns("feat_npic"),span("+ Number of variables:",tiphelp("Filter variables with the highest mean importance")), value=20, step=1),


      pickerInput(inputId = ns("pal_feature"),
                  label = "+ Palette",
                  choices =     vals$colors_img$val,
                  choicesOpt = list(content =     vals$colors_img$img), options=list(container="body")),

      numericInput(ns("feaimp_cex.axes"),"+ Axes size", value=13, step=1),
      numericInput(ns("feaimp_cex.lab"),"+ Label size",value=14,step=1),
      numericInput(ns("feaimp_cex.leg"),"+ Label size", value=13, step=1),




      div(
        actionLink(
          ns('spd_varImp_plot'),span('+ Download plot'),style="button_active"
        )
      )

  )
})

output$side_filter_feature_plot_nvars<-renderUI({

  div(
    span("+ Significance level",
         inline(
           numericInput(ns("sig_feature"),NULL, value=0.05, width="75px", step=0.05)
         )
    )
  )
})
observeEvent(ignoreInit = T,input$spd_varImp_plot,{
  vals$hand_plot<-'Feature Shuffling'
  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
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

  vals$hand_plot<- paste0(input$model_attr," - Variable Importance by class")
  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs,"downfigs", vals=vals)
})

output$feature_cm<-renderUI({
  m<-get_model()
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
  vals$hand_plot<-"Confusion Matrix"
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
  model_list<- attr(vals$saved_data[[input$data_spdX]],input$model_attr)[[input$model]]
  req(length(model_list)>0)
  predtable<-model_list$feature_rands
  dft<-get_cm_impact(predtable, var=input$var_feature_cm)

  renderPlot({
    vals$feature_cm_plot<- plotCM(dft, palette=input$pal_feature_cm,vals$newcolhabs, font_color="black", title="", round_cm=3)
    vals$feature_cm_plot
  })
})
getmetric<-reactive({
  m<-get_model()
  metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
  metric
})
get_sumamry_permute_importance<-reactive({
  req(input$sig_feature)
  sig_level<-input$sig_feature
  model_list<- attr(vals$saved_data[[input$data_spdX]],input$model_attr)[[input$model]]
  req(length(model_list)>0)

  req(length(model_list)>0)
  predtable<-model_list$feature_rands
  df<-model_list$feature_rands
  m<-get_model()
  newdata=getnewdata()
  obc<-getobc()
  df<-permimp_metric(df,metric=getmetric(), class=NULL,m, newdata,obc)

  req(length(df)>0)
  req(is.data.frame(df))

  vals$spd_varImp<-perm_summary<-sig_feature(df,m, newdata, obc,sig_level)
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
  m<-get_model()
  newdata=getnewdata()
  obc<-getobc()
  rep=input$feat_rep
  seed=input$feat_seed
  if(is.na(seed)){seed=NULL}

  predtable<- permute_importance_foreach_progress(m,newdata,obc, rep, seed=seed)


  stopCluster(cl)
  unregister_dopar()
  attr(vals$saved_data[[input$data_spdX]],input$model_attr)[[input$model]]$feature_rands<-predtable
})
output$feature_importance_plot<-renderUI({
  req(input$data_spdX)
  req(input$model)
  model_list<- attr(vals$saved_data[[input$data_spdX]],input$model_attr)[[input$model]]
  req(length(model_list)>0)
  req(input$feat_npic)
  sig_level<-input$sig_feature

  req(length(model_list)>0)
  df<-model_list$feature_rands
  validate(need(length(df)>0,"Analyses pending"))
  m<-model_list[[1]]
  newdata=getnewdata()
  obc<-getobc()
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

    vals$spd_varImp_plot<-p
    vals$spd_varImp_plot

  })
})
##
##
##
##

output$side_pdp<-renderUI({
  sidebarPanel(

    strong("Partial Dependence",tipify(icon("fas fa-question-circle"),"Plot partial dependence of two variables(i.e., marginal effects) for the randomForest", options =list(container="body")), style="font-size: 16px"),
    uiOutput(ns("side_rf_grid"))
  )
})
output$main_pdp<-renderUI({
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
})
output$spd5_pd<-renderUI({
  div(
    actionButton(ns('run_pd'),"RUN"),

    sidebarLayout(

      uiOutput(ns("side_pdp")),
      uiOutput(ns("main_pdp"))

    ))
})
observeEvent(ignoreInit = T,input$pd_models,{
  vals$pd_models<-input$pd_models
})

getgrad_col<-reactive({
  res<-lapply(vals$newcolhabs, function(x) x(2))
  res1<-unlist(lapply(res, function(x) x[1]==x[2]))
  grad<-names(res1[res1==F])
  pic<-which(vals$colors_img$val%in%grad)
  pic
})
output$rf_useinter<-renderUI({
  div(
    span("+ Use",
         inline(
           pickerInput(ns("rf_useinter"), NULL, choices=c("Custom Variables"), width="150px", selected=vals$rf_useinter)
         )
    )
  )
})
output$side_rf_grid<-renderUI({



  fluidRow(class="map_control_style",style="color: #05668D",
           uiOutput(ns("rf_useinter")),
           uiOutput(ns("rf_interbiplot2")),
           uiOutput(ns("pd_which_class")),
           uiOutput(ns("pd_grid_resolution")),
           uiOutput(ns("pd_plot_custom"))


  )
})
output$pd_grid_resolution<-renderUI({

  div(
    span(span(tipify(icon("fas fa-question-circle"),"Integer giving the number of equally spaced points to use for the continuous variables", options =list(container="body")),'+ Grid resolution'),
         inline(
           numericInput(ns('grid.resolution'),NULL,4, width="75px")
         )
    )
  )
})
observeEvent(ignoreInit = T,input$grid.resolution,{
  vals$grid.resolution<-input$grid.resolution
})
output$pd_which_class<-renderUI({
  m<-get_model()
  req(m$modelType=="Classification")
  div(
    span("+ Class",
         inline(
           pickerInput(ns("which.class"), NULL, choices=m$levels, width="150px")

         )
    )
  )
})
get_saved_models<-function(data){
  choices<-list(
    if(check_model(data,"rf")){"rf"},
    if(check_model(data,"nb")){"nb"},
    if(check_model(data,"svm")){"svm"},
    if(check_model(data,"knn")){"knn"},
    if(check_model(data,"sgboost")){"sgboost"}
  )
  attributes<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]
  if(!length(attributes)>0){ return(NULL)}
  limodels2<-lapply(attributes,function(x) attr(data,x))
  names0<-unlist(lapply(limodels2,names))
  limodels2<-flattenlist(limodels2)
  resmethos<-unlist(lapply(limodels2,function(x) paste0("Type:",x$modelType,"; ")))
  linames<-unlist(lapply(attributes,function(x) names(attr(data,x))))
  x<-limodels2[[1]]
  ressup<-unlist(lapply(limodels2,function(x) attr(x,"supervisor")))
  dftype<-data.frame(type=as.vector(resmethos),sup=as.vector(ressup))
  resmethos<-as.vector(apply(dftype,1,function(x) {
    paste0("Y:",x,collapse="")
  }))
  liequals<-split(limodels2,as.vector(unlist(lapply(limodels2,function(x)nrow(x$resample)))))
  liequals<-lapply(liequals,function(x) names(x))
  listamodels2<-split(limodels2,resmethos)
  listanames<-split(names0,resmethos)
  for(i in 1:length(listamodels2)){
    names(listamodels2[[i]])<-listanames[[i]]
  }
  listamodels2
}
get_the_model<-reactive({
  c(getmodelist_pd1(vals$saved_data),
    getmodelist_pd2(vals$saved_data))[[input$pd_models]]
})
get_the_model2<-reactive({

  em<-get_the_model()
  if(class(em)[1]!='train'){

    allmodels<-lapply(vals$saved_data,function(x)get_saved_models(x))
    allmodels2<-lapply(allmodels,function(x){
      x[[em$listamodels2]][em$ensemble_active]
    })
    pic<-names(which(unlist(lapply(allmodels2, function(x){
      sum(em$ensemble_active%in%unlist(names(x)))==length(em$ensemble_active)
    }))))
    attr(em,"modelist")<-   allmodels2[[pic]]
    em$modelType<-"Ensemble"
  } else{
    attr(em,"modelist")<-  list(em)
  }
  em
})
get_pd<-reactive({
  m<-get_model()
  type<-switch(m$modelType,
               "Ensemble"="Ensemble",
               "Classification"="classification",
               "Regression"="regression")

  var1=input$rf_grid_var1
  var2=input$rf_grid_var2


  type2<-switch(m$modelType,
                "Ensemble"="Ensemble",
                "Classification"="classification",
                "Regression"="regression")


  train=m$trainingData
  pic<-which(colnames(train)%in%".outcome")
  train<-train[-pic]
  if(type=="Ensemble"){
    pd_enres<-pdp_ensemble(m, train=train,pred.var = c(var1,var2), pred.fun =pd_predfun_ensemble, which.class=input$which.class,grid.resolution=input$grid.resolution,prob=T, chull=T,type=type2)
    pd_enres} else{
      pd_enres<-pdp::partial(m, train=train,pred.var = c(var1,var2), which.class=input$which.class,grid.resolution=input$grid.resolution,prob=T, chull=T,type=type2)
    }
  vals$pd_enres<-pd_enres

})
observeEvent(ignoreInit = T,input$run_pd,{
  get_pd()
})
output$rfbiplot1<-renderPlot({
  req(!is.null(vals$pd_enres))
  p1<-autoplot(vals$pd_enres)

  p1<- p1 + scale_fill_gradientn(colours =scale_color_2(input$pd_palette,vals$newcolhabs),name="Partial\ndependence (prob)")
  p1<-p1+theme_bw(base_size = input$pd_size_plot)
  p1<-p1+ ggtitle(input$title_pd)
  p1<-p1+labs(y = input$ylab_pd, x=input$xlab_pd)


  vals$rfbiplot1<-p1
  p1
})
observeEvent(ignoreInit = T,input$pdloop_go,{
  m<-get_model()
  type<-switch(m$modelType,
               "Ensemble"="Ensemble",
               "Classification"="classification",
               "Regression"="regression")

  var1=input$rf_grid_var1
  var2=input$rf_grid_var2


  type2<-switch(m$modelType,
                "Ensemble"="Ensemble",
                "Classification"="classification",
                "Regression"="regression")


  train=m$trainingData
  pic<-which(colnames(train)%in%".outcome")
  train<-train[-pic]
  levs<-m$levels
  withProgress(min=0,max=length(levs),message="Running...",{
    pds_classes<-lapply(levs,function(x){
      incProgress(1,message =paste0("Running...; Class:",x))
      if(type=="Ensemble"){
        pd_enres<-pdp_ensemble(m, train=train,pred.var = c(var1,var2), pred.fun =pd_predfun_ensemble, which.class=x,grid.resolution=input$grid.resolution,prob=T, chull=T,type=type2)
        pd_enres} else{
          pd_enres<-pdp::partial(m, train=train,pred.var = c(var1,var2), which.class=x,grid.resolution=input$grid.resolution,prob=T, chull=T,type=type2)
        }
      pd_enres
    })


  })

  vals$pds_classes<-pds_classes

})
observeEvent(ignoreInit = T,input$pd_var2,{
  vals$pd_var2<-input$pd_var2
})
output$pd_sidebyside<-renderUI({

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

  m<-get_model()
  train=m$trainingData
  pic<-which(colnames(train)%in%".outcome")
  train<-train[-pic]
  choices<-colnames(train)

  div(
    div(
      span("+ Variable 1",
           inline(
             pickerInput(ns("rf_grid_var1"), NULL, choices=choices, width="100px")
           )
      )
    ),
    div(
      span("+ Variable 2",
           inline(
             pickerInput(ns("rf_grid_var2"), NULL, choices=choices, width="100px", selected=choices[2])
           )
      )
    )
  )

})
observeEvent(ignoreInit = T,input$rf_useinter,{
  vals$rf_useinter<-input$rf_useinter
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
  var1=input$rf_grid_var1
  var2=input$rf_grid_var2


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
             numericInput(ns("pd_size_plot"),NULL, value=10, width="75px")
           )
      )
    ),
    div(
      actionLink(ns('downp_rfbi'),span("+ Download plot",icon("fas fa-image"),icon("fas fa-download")), style="button_active")
    )
  )
})
output$PD_LOOP<-renderUI({
  res_pd<-vals$pds_classes
  req(!is.null(res_pd))
  vals$rfbiplot2<-plot_pd_groups(res_pd,m=NULL,
                                 pd_size_plot=input$pd_size_plot,
                                 pd_palette=input$pd_palette,
                                 newcolhabs=vals$newcolhabs)
  renderPlot(vals$rfbiplot2)
})
observeEvent(ignoreInit = T,input$downp_rfbi,{

  vals$hand_plot<-switch(input$pd_tab,
                         "pd_tab1"="RF - Partial Dependence",
                         "pd_tab2"="RF - Partial Dependence (classes)"
  )
  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)})


###


getsom<-reactive({

  m<-get_model()
  m<-m$finalModel
  names(m$unit.classif)<-rownames(m$data[[1]])
  req( class(m) == "kohonen")
  m
})


output$side_msp_xyf<-renderUI({
  m<-get_model()
  xyf<-m$finalModel
  req(class(xyf)=="kohonen")
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
                                       selected= "gray", width = '75px'))
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
    copoints<-getcopoints(m, newdata=NULL)
    vals$copoints_hc<-copoints
    copoints
  })

  xyf_copoints_scaled<-reactive({

    xyf_get_network()

    xyf_get_copoints()

    points_tomap=rescale_copoints(hexs=vals$xyf_hc_network,copoints=vals$copoints_hc)

    data<-vals$saved_data[[input$data_spdX]]

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
      vals$somplot_bg<-"white"
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
getsolid_col<-reactive({

  res<-lapply(vals$newcolhabs, function(x) x(10))
  res1<-unlist(lapply(res, function(x) x[1]==x[2]))
  solid<-names(res1[res1==T])
  pic<-which(vals$colors_img$val%in%solid)
  pic

})

output$msp_xyf<-renderUI({

  m<-get_model()
  xyf<-m$finalModel
  req(class(xyf)=="kohonen")
  args<-xyf_argsplot()

  args$plotY<-input$plotY

  req(length(args)>0)
  bp<-args$bp
  bp$id=NULL
  vals$biplot_som<-bp
  req(length(args$hexs)>0)

  #saveRDS(args,'args.rds')
  #args<-readRDS("args.rds")
  p<-do.call(bmu_plot,args)
  vals$fm_downplot<-p


  div(
    strong(em("Codebook for the training data")),
    renderPlot(p, width=input$msp_plot_width,height=input$msp_plot_height)
  )


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
  vals$hand_plot<-"BMUs (sup)"

  module_ui_figs("downfigs")
  mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
})
observeEvent(ignoreInit = T,input$down_pcorr_results,{
  vals$hand_down<-"pcorr (sup)"
  module_ui_downcenter("downcenter")
  mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)

})



}
