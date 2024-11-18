module_save_changes<-list()
module_save_changes$ui<-function(id,vals=NULL){
  ns<-NS(id)
  showModal(
    modalDialog(
      title="Save Changes",

      footer=div(actionButton(ns("data_confirm"),strong("confirm")),
                 modalButton("Cancel")),
      div(style="padding: 20px",

          div(class="half-drop",
              style="display: flex",
              div(style="width: 20%",
                  radioButtons(ns("create_replace"),NULL,c("Create","Replace"))
              ),
              div(style="padding-top: 10px",
                  textInput(ns("newdatalit"),NULL,attr(vals$newdatalist,"bag")),
                  hidden(selectInput(ns("overdatalist"),NULL,names(vals$saved_data)))
              )
          ),
          div(style="padding-left: 30px",
              uiOutput(ns('newdata')),

          ),
          div(style="padding-left: 30px",
              uiOutput(ns('message')),

          )
      )
    )
  )
}
module_save_changes$server<-function(id,vals,update_tab1=NULL,tab1=NULL,update_tab2=NULL,tab2=NULL,message=NULL){
  moduleServer(id,function(input,output,session){
    ns<-session$ns

    output$message<-renderUI({
      message
    })

    newdata<-reactive({
      req(vals$newdatalist)
      data<-vals$newdatalist
      attr(data,"datalist")<-datalist()
      data
    })

    name0<-reactive({
      req(vals$newdatalist)
      bag<-attr(vals$newdatalist,"bag")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      newnames[length(newnames)]


    })



    observeEvent(input$create_replace,{
      shinyjs::toggle("newdatalit",condition=input$create_replace=="Create")
      shinyjs::toggle("overdatalist",condition=input$create_replace=="Replace")
    })
    datalist<-reactive({
      req(input$create_replace)
      if(input$create_replace=="Create"){
        req(input$newdatalit)
        input$newdatalit
      } else {
        req(input$overdatalist)
        req(input$overdatalist%in%names(vals$saved_data))
        input$overdatalist
      }
    })


    output$newdata<-renderUI({
      req(newdata())
      basic_summary2(newdata())
    })


    observeEvent(input$data_confirm,ignoreInit = T,{


      vals$saved_data[[datalist()]]<-newdata()
      if(!is.null(update_tab1)){
        vals[[update_tab1]]<-tab1
      }
      if(!is.null(update_tab2)){
        vals[[update_tab2]]<-tab2
      }
      removeModal()
    })
  })


}



reshape_args<-function(x,y,sl_formals,sl_ctl_formals,model){
  req(model)
  args<-c(sl_formals[[model]],sl_ctl_formals[[model]])
  args<-args[unique(names(args))]
  args$x<-x
  args$y<-y
  argslist<-lapply(args,function(x) try(eval(x,envir=args),silent = T))
  argslist[c("x","y")]<-NULL
  classes<-sapply(argslist,class)
  argslist<-argslist[classes%in%c("numeric","integer","character","factor","logical","NULL")]
  len<-sapply(argslist,length)
  classes<-sapply(argslist,class)
  classes[classes=="integer"]<-"numeric"
  list(
    classes=classes,
    argslist=argslist,
    len=len
  )
}
model_control<-list()
model_control$ui<-function(id,vals){
  vals$cur_model_params<-NULL

  x<-vals$cur_xtrain
  y<-vals$cur_var_y[,1]
  model<-vals$cmodel
  req(model)

  ns<-NS(id)
  re<-reshape_args(x,y,sl_formals,sl_ctl_formals,model)
  classes=re$classes
  argslist=re$argslist
  len=re$len
  req(len)
  req(classes)

  if(all(names(vals$box_caret3_args)%in%names(argslist))){
    for(i in names(vals$box_caret3_args)){
      argslist[[i]]<-vals$box_caret3_args[[i]]
    }
  }
  lapply(seq_along(argslist),function(i){
    id<-names(argslist)[i]
    lab<-span(id,tipright(HTML(paste0(sl_tips[[model]][id,1]))))
    value=argslist[[i]]
    if(len[[i]]==0){
      if(classes[i]=="NULL"){
        value=NA
      }
      numericInput(ns(id),lab,value)
    } else  if(len[[i]]==1){


      switch(classes[i],
             "logical"=checkboxInput(ns(id),lab,as.logical(value)),
             "numeric"=numericInput(ns(id),lab,as.numeric(value))
      )
    } else{
      switch(classes[i],
             "character"=selectInput(ns(id),lab,value),
             "numeric"=textInput(ns(id),lab,paste(value,collapse=", ")),
             "NULL"=
      )
    }
  })}
model="mlpML"
msp_earth<-list()
msp_earth$ui<-function(id,vals,model){
  ns<-NS(id)
  div(
    column(4,
           box_caret(
             ns("box_a"),
             title="Plot Options",
             color="#c3cc74ff",
             div(
               pickerInput_fromtop(ns('which'),"Plot to draw",
                                   options=shinyWidgets::pickerOptions(windowPadding="top"),choices=c('Model selection plot'=1,'Cumulative distribution of abs residuals'=2,'Residuals vs fitted'=3,'QQ plot'=4,'Abs residuals vs fitted'=5,'Sqrt abs residuals vs fitted'=6,'Abs residuals vs log fitted'=7,'Cube root of the squared residuals vs log fitted'=8,'Log abs residuals vs log fitted'=9)),


               selectInput(ns('nresponse'),span("Response",tipright("Response level")),model$levels),
               selectizeInput(ns("legend.pos"),"legend.pos",c("Automatic"="auto","Ommit"=0,"bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center","user-defined")),
               div(
                 id=ns('legend.pos_user'),style="display: none",
                 numericInput(ns("legend.pos_user1"),NULL,6),
                 numericInput(ns("legend.pos_user2"),NULL,.75)
               )
             )
           )

    ),



    column(6,
           box_caret(ns("box_b"),
                     title="Plot",
                     button_title = actionLink(ns("download_plot"),
                                               "Download",icon("download")),
                     div(
                       div(id=ns('run_btn'),class="save_changes",

                           style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                       uiOutput(ns('msp_earth')))

           ))
  )

}
msp_earth$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){

    box_caret_server("box_a")
    box_caret_server("box_b")

    observe({
      shinyjs::toggle('legend.pos_user',condition=input$legend.pos=="user-defined")
    })



    data_x<-attr(model,"Datalist")
    observe({
      shinyjs::toggle('nresponse',condition=model$modelType=="Classification")
    })

    args<-reactive({
      req(model$modelType)
      nresponse=input$nresponse
      if(model$modelType!="Classification"){
        nresponse=NA
      }
      legend.pos=input$legend.pos
      req(legend.pos)
      if(  legend.pos=="user-defined"){
        legend.pos<-c(input$legend.pos_user1,input$legend.pos_user2)
      }
      if(  legend.pos=="auto"){
        legend.pos<-NULL
      }

      list(
        model$finalModel,
        nresponse=nresponse,
        legend.pos=legend.pos,
        which=as.numeric(input$which))
    })

    observeEvent(args(),{
      shinyjs::addClass("run_btn","save_changes")
    })

    earth_plot<-eventReactive(input$run,ignoreInit = T,{

      shinyjs::removeClass("run_btn","save_changes")
      args<-args()


      withProgress( do.call(plot,args),NA,NA,message="Running...")
      p<-recordPlot()

      p

    })


    observeEvent(ignoreInit = T,input$download_plot,{
      req(earth_plot())
      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=earth_plot()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Eath plot",datalist_name=datalist_name,name_c="earth_plot")
    })



    output$msp_earth<-renderUI({
      renderPlot(earth_plot())
    })
    return(NULL)


  })}
msp_dnn<-list()
msp_dnn$ui<-function(id,vals){
  ns<-NS(id)
  div(
    column(4,style='overflow: auto; max-height: 400px',
           box_caret(
             ns("box_a"),
             title="Plot Options",
             color="#c3cc74ff",
             div(
               numericInput(ns("neu_radius"), "Neuron size", value =0.05,step=0.01),
               colourpicker::colourInput(ns('neuron_fill'),"Neuron fill",value="Grey"),
               pickerInput_fromtop_live(inputId = ns("weight_palette"),
                                        label ="Weight Palette",
                                        choices =  vals$colors_img$val,
                                        options=shinyWidgets::pickerOptions(windowPadding="top"),
                                        choicesOpt = list(
                                          content =  vals$colors_img$img)),
               div(style="display: flex",
                   numericInput(ns("xlim1"), "Xlims", value =-0.5,step=0.1),
                   numericInput(ns("xlim2"), "to", value =1.5,step=0.1)
               ),
               numericInput(ns("text_size"), "Text size", value =4,step=0.1),
               numericInput(ns("linewidth"), "Line width", value =1,step=0.1),
               numericInput(ns("size.layers.text"), "Layers title size",5, step=0.1),
               numericInput(ns("width"), "Plot width",500),
               numericInput(ns("height"), "Plot height",300)

             )
           )

    ),
    column(6,
           box_caret(ns("box_b"),
                     title="Network Plot",
                     button_title = actionLink(ns("download_plot"),
                                               "Download",icon("download")),
                     div(
                       div(id=ns('run_btn'),class="save_changes",

                           style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                       uiOutput(ns('msp_dnn')))

           ))
  )

}
msp_dnn$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_a")
    box_caret_server("box_b")

    data_x<-attr(model,"Datalist")


    args<-reactive({
      list(
        modelo=model$finalModel,
        neuron_fill=input$neuron_fill,
        neuron_border="Grey",
        neu_radius=input$neu_radius,
        weight_palette=vals$newcolhabs[[input$weight_palette]],
        xlim=c(input$xlim1,input$xlim2),
        text_size=input$text_size,
        linewidth=input$linewidth,
        size.layers.text=input$size.layers.text
      )
    })

    observeEvent(args(),{
      shinyjs::addClass("run_btn","save_changes")
    })

    dnn_plot<-eventReactive(input$run,ignoreInit = T,{

      shinyjs::removeClass("run_btn","save_changes")

      args<-args()


      do.call(plot_ggnet,args)
    })

    observeEvent(ignoreInit = T,input$download_plot,{
      req(dnn_plot())
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=dnn_plot()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Network plot",datalist_name=datalist_name,name_c="dnn_plot")
    })



    output$msp_dnn<-renderUI({
      renderPlot(dnn_plot(),height = input$height,width=input$width)
    })

  })}
msp_dnn$server_avNNet<-function(id,model,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_a")
    box_caret_server("box_b")

    req(model)




    data_x<-attr(model,"Datalist")

    m<-model$finalModel
    attr(m,"supervisor")<-attr(model,"supervisor")
    args<-reactive({
      m<-m
      dfs<-dfs_avNNet(m=m)
      dfneu<-dfs$dfneu
      df_segments<-dfs$df_segments

      list(
        m=m,
        df_segments=df_segments,
        dfneu=dfneu,
        neuron_fill=input$neuron_fill,
        neuron_border="Grey",
        neu_radius=input$neu_radius,
        weight_palette=vals$newcolhabs[[input$weight_palette]],
        xlim=c(input$xlim1,input$xlim2),
        text_size=input$text_size,
        linewidth=input$linewidth,
        size.layers.text=input$size.layers.text
      )
    })

    observeEvent(args(),{
      shinyjs::addClass("run_btn","save_changes")
    })

    avNNet_plot<-reactiveVal()
    observeEvent(input$run,ignoreInit = T,{

      shinyjs::removeClass("run_btn","save_changes")
      args<-args()

      p<-do.call(ggplot_avNNet,args)
      avNNet_plot(p)
      p
    })

    observeEvent(ignoreInit = T,input$download_plot,{
      req(avNNet_plot())
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=avNNet_plot()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Network plot",datalist_name=datalist_name,name_c="avNNet_plot")
    })



    output$msp_dnn<-renderUI({
      renderPlot(avNNet_plot(),height = input$height,width=input$width)
    })

  })}
msp_dnn$server_nnet<-function(id,model,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_a")
    box_caret_server("box_b")
    req(model)
    data_x<-attr(model,"Datalist")

    args<-reactive({


      if(inherits(model$finalModel,"pcaNNet")){
        mod1<-model$finalModel$model
        var_names=paste0("PC",1:model$finalModel$pc$numComp)
      } else{
        mod1<-model$finalModel
        var_names=model$finalModel$xNames
      }

      if(model$method=="mlpML"){
        dfs<-dfs_mlpML(model)

      } else if(model$method=="monmlp"){

        dfs<-dfs_monMLP(m=model$finalModel)

      } else{
        dfs<-dfs_avNNet(mod1=mod1,var_names=var_names)
      }


      dfneu<-dfs$dfneu
      df_segments<-dfs$df_segments



      if(model$modelType=="Regression"){
        if(!is.null(mod1$residuals)){
          colnames(mod1$residuals)<-attr(model,"supervisor")} else{
            mod1$residuals<-matrix(NA,nrow=1,dimnames = list(1,attr(model,"supervisor")))
          }
      }


      args<-list(
        mod1=mod1,
        df_segments=df_segments,
        dfneu=dfneu,
        neuron_fill=input$neuron_fill,
        neuron_border="Grey",
        neu_radius=input$neu_radius,
        weight_palette=vals$newcolhabs[[input$weight_palette]],
        xlim=c(input$xlim1,input$xlim2),
        text_size=input$text_size,
        linewidth=input$linewidth,
        size.layers.text=input$size.layers.text,
        legname="Weight"
      )

      args
    })

    observeEvent(args(),{
      shinyjs::addClass("run_btn","save_changes")
    })

    avNNet_plot<-reactiveVal()
    observeEvent(input$run,ignoreInit = T,{

      shinyjs::removeClass("run_btn","save_changes")
      args<-args()

      p<-do.call(ggplot_avNNet,args)
      avNNet_plot(p)
      p
    })

    observeEvent(ignoreInit = T,input$download_plot,{
      req(avNNet_plot())
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=avNNet_plot()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Network plot",datalist_name=datalist_name,name_c="avNNet_plot")
    })



    output$msp_dnn<-renderUI({
      renderPlot(avNNet_plot(),height = input$height,width=input$width)
    })

  })}
msp_monmlp<-list()
msp_monmlp$ui<-function(id,vals){
  model<-vals$cur_caret_model
  choices<-colnames(getdata_model(model))
  ns<-NS(id)
  div(
    column(4,style='overflow: auto; max-height: 400px',
           box_caret(
             ns("box_a"),
             title="Plot Options",
             color="#c3cc74ff",
             div(
               pickerInput_fromtop(ns("column"), "Variable",choices=choices,multiple = T,selected=choices[1:4], options=shinyWidgets::pickerOptions(windowPadding="top",liveSearch=T)),
               pickerInput_fromtop_live(inputId = ns("palette"),
                                        label ="Palette",
                                        choices =  vals$colors_img$val,
                                        choicesOpt = list(
                                          content =  vals$colors_img$img),
                                        options=shinyWidgets::pickerOptions(windowPadding="top")),
               div(style="display: flex",
                   numericInput(ns("ncol"), "ncol", value =2,step=0.1),
                   numericInput(ns("nrow"), "nrow", value =NA,step=0.1)
               ),
               numericInput(ns("base_size"), "Base size", value =11,step=0.1),
               numericInput(ns("seg_length"), "Seg length",0.05),
               numericInput(ns("line_width"), "Line width",0.5),


               numericInput(ns("width"), "Plot width",500),
               numericInput(ns("height"), "Plot height",500)

             )
           )

    ),
    column(6,
           box_caret(ns("box_b"),
                     title="GAM style plot",
                     button_title = actionLink(ns("download_plot"),
                                               "Download",icon("download")),
                     div(
                       div(id=ns('run_btn'),class="save_changes",

                           style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                       uiOutput(ns('msp_monmlp')))

           ))
  )

}
msp_monmlp$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_a")
    box_caret_server("box_b")

    model<-vals$cur_caret_model
    geteffec<-function(x,y,w,columns,seg.len){
      allresults<-lapply(columns,function(column){

        res<-gam.style(x, weights = w, column =column,plot=F,return.results=T,seg.len=seg.len)
        effects<-res$effects
        colnames(effects)<-colnames(y)
        partials<-res$partials

        x.var <- x[, column]
        if (is.null(colnames(x)))
          colnames(x) <- paste("x", seq(ncol(x)), sep = "")
        xlab <- colnames(x)[column]

        resplots<-lapply (seq(ncol(effects)),function(predictand){
          ylab <- paste("Effects: predictand", predictand)
          theta <- atan(partials[, predictand])
          ymin <- min(effects[, predictand])
          ymax <- max(effects[, predictand])
          xmin <- min(x.var)
          xmax <- max(x.var)
          aspect <- (ymax - ymin)/(xmax - xmin)
          xdev <- seg.len * (xmax - xmin) * cos(theta)
          ydev <- seg.len * (xmax - xmin) * sin(theta)
          scale <- sqrt(xdev^2 + (ydev/aspect)^2)
          xdev <- xdev * (seg.len * (xmax - xmin))/scale
          ydev <- ydev * (seg.len * (xmax - xmin))/scale

          do.call(rbind,lapply (seq_along(x.var),function(case){
            xi <- x.var[case]
            yi <- effects[case, predictand]
            xd <- xdev[case]
            yd <- ydev[case]
            data.frame(x0=xi, y0=yi, x1=xi + xd, y1=yi + yd, col=column,case=case,effects=predictand)
          }))

        })


        result<-data.frame(do.call(rbind,resplots))
        result$var<-column
        result

      })

      result<-do.call(rbind,allresults)
      result
    }

    monImp<-function(object,seg.len=0.05,column=NULL) {
      x <- attr(object$model,"x")
      y <- attr(object$model,"y")
      w <- object$model
      if(is.null(column)){
        column<-colnames(x)
      }

      result<-geteffec(x,y,w,column,seg.len)
      x<-result[1,]
      i=1
      yr<-lapply(1:nrow(result),function(i) {

        data.frame(
          xmean=mean(c(result$x0[i],result$x1[i])),
          yrange=abs(diff(c(result$y1[i],result$y0[i]))),
          var=result$var[i],
          effects=result$effects[i]
        )

      })

      yr2<-do.call(rbind,yr)
      list(yr2,result)
    }

    observeEvent(ignoreInit = T,input$download_plot,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=plot_effect()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Gam Style plot",datalist_name=datalist_name,name_c="gam.style")
    })

    observeEvent(list(input$column,
                      input$palette,
                      input$ncol,
                      input$nrow,
                      input$base_size,
                      input$seg_length,
                      input$line_width),{
                        shinyjs::addClass("run_btn","save_changes")
                      })

    plot_effect<-eventReactive(input$run,ignoreInit = T,{
      shinyjs::removeClass("run_btn","save_changes")
      if(model$modelType=="Regression"){
        v=100
      } else{
        v=length(model$levels)
      }
      col_values=vals$newcolhabs[[input$palette]](v)
      res<-monImp(model$finalModel,input$seg_length,column=input$column)
      result<-res[[2]]
      nrow=input$nrow
      if(is.na(nrow)){
        nrow<-NULL
      }


      p<-ggplot(result)
      p<-p+geom_segment(aes(x=x0, y=y0, xend=x1, yend=y1,color=as.factor(effects)),data=result,linewidth=input$line_width)+scale_color_manual(name=attr(model,"supervisor"),values=col_values)
      p<-p+facet_wrap(vars(var),scales="free",nrow=nrow,ncol=input$ncol)
      p<-p+ylab("Effects.predictand")+xlab("")+theme_bw(base_size = input$base_size)
      p
    })

    observeEvent(input$gamstyle_help,{
      showModal(
        modalDialog(
          easyClose = T,
          div(
            p(strong('GAM-style effects plots for interpreting MLP and MONMLP models')),
            p("GAM-style effects plots provide a graphical means of interpreting fitted covariate/response relationships.  The effect of the ",code('ith')," input variable at a particular input point ",code("Delta.i.x"),"is the change in ",code('f')," resulting from changing ",code("X1"),"to ",code("x1")," from ",code("b1")," (the variable mean) while keeping the other inputs constant. The effects are plotted as short line segments, centered at (x.i, Delta.i.x), where the slope of the segment is given by the partial derivative. Variables that strongly influence the function value have a large total vertical range of effects. Functions without interactions appear as possibly broken straight lines (linear functions) or curves (nonlinear functions). Interactions show up as vertical spread at a particular horizontal location, that is, a vertical scattering of segments. Interactions are present when the effect of a variable depends on the values of other variables."),
            p(strong("References")),
            p(
              'Cannon, A.J. and I.G. McKendry, 2002. A graphical sensitivity analysis for interpreting statistical climate models: Application to Indian monsoon rainfall prediction by artificial neural networks and multiple linear regression models. International Journal of Climatology, 22:1687-1708.

Plate, T., J. Bert, J. Grace, and P. Band, 2000. Visualizing the function computed by a feedforward neural network. Neural Computation, 12(6): 1337-1354.'
            )
          )
        )
      )
    })

    output$msp_monmlp<-renderUI({
      p<-plot_effect()
      req(p)

      div(
        h5(strong('GAM-style effects plots'),actionLink(session$ns('gamstyle_help'),icon("fas fa-question-circle"))),
        renderPlot(p,width=input$width,height = input$height))

    })



  })}
msp_rpart<-list()
msp_rpart$ui<-function(id,vals){
  model<-vals$cur_caret_model
  choices<-colnames(getdata_model(model))
  ns<-NS(id)
  div(
    column(4,style='overflow: auto; max-height: 400px',
           box_caret(
             ns("box_a"),
             title="Plot Options",
             color="#c3cc74ff",
             div(
               div(
                 div(
                   div(actionLink(ns("on_args_general"),
                                  strong("+ General"))),
                   div(class="map_side",
                       id=ns('args_general'),
                       textInput(ns('title'),"Title","Tree"),
                       numericInput(ns("width"),
                                    div("Plot width",
                                        tipright("in pixels or empty for auto width"))
                                    ,NA),
                       numericInput(ns("height"), div("Plot height",
                                                      tipright("in pixels or empty for auto height")),NA))
                 ),

                 div(
                   div(actionLink(ns("on_args_terminal"),
                                  strong("+ Terminal Plot"))),
                   div(class="map_side",id=ns('args_terminal'),
                       selectInput(ns('type'),"Type",choices=c('simple',"pies","barplot")),

                       pickerInput_fromtop_live(
                         inputId = ns("palette"),
                         label ="Palette",
                         choices =  vals$colors_img$val,
                         choicesOpt = list(
                           content =  vals$colors_img$img),
                         options=shinyWidgets::pickerOptions(windowPadding="top")
                       )
                   )
                 ),



                 div(id=ns('bar_args'),
                     div(actionLink(ns("on_args_barplot"),
                                    strong("+ Bar plot"))),
                     div(class="map_side",
                         id=ns('args_barplot'),
                         checkboxInput(ns('bar_show_x'),"Show Axis X",F),
                         checkboxInput(ns('bar_show_y'),"Show Axis Y",T),
                         textInput(ns('bar_xlab'),"xlab",NULL),
                         textInput(ns('bar_ylab'),"ylab",NULL),
                         numericInput(ns("bar_base_size"), "Base size", value =8,step=0.1),
                         selectInput(ns('bar_theme'),"Theme",choices=c('theme_classic',"theme_bw","theme_grey","theme_void","theme_minimal"))
                     )),
                 div(
                   div(actionLink(ns("on_args_nodes"),
                                  strong("+ Nodes"))),
                   div(class="map_side",
                       id=ns('args_nodes'),
                       colourpicker::colourInput(ns("node_fill"),"Fill","gray"),
                       colourpicker::colourInput(ns("node_color"),"Text Color","white"),
                       numericInput(ns("node_text.size"), "Text size", value =3,step=0.1))
                 ),
                 div(
                   div(actionLink(ns("on_args_line"),
                                  strong("+ Lines"))),
                   div(class="map_side",
                       id=ns('args_line'),
                       selectInput(ns("linetype"),"Line type",choices=list("dashed"=2,solid=1)),
                       colourpicker::colourInput(ns("linecolor"),"Line color","Grey")
                   )
                 ),
                 div(
                   div(actionLink(ns("on_args_values"),
                                  strong("+ Values"))),
                   div(class="map_side",
                       id=ns('args_values'),
                       numericInput(ns("values.size"), "Text size", value =3,step=0.1),
                       numericInput(ns("round"), "Round", value =3,step=1))
                 )

               )



             )
           )

    ),
    column(8,
           box_caret(ns("box_b"),
                     title="Tree Plot",
                     button_title = actionLink(ns("download_plot"),
                                               "Download",icon("download")),
                     div(
                       div(id=ns('run_btn'),class="save_changes",

                           style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                       uiOutput(ns('msp_rpart')))

           ))
  )

}
msp_rpart$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){
    box_caret_server("box_a")
    box_caret_server("box_b")

    observe({
      shinyjs::toggle('bar_args',condition=input$type=="barplot")
    })

    observeEvent(input$on_args_terminal, shinyjs::toggle('args_terminal'))
    observeEvent(input$on_args_nodes, shinyjs::toggle('args_nodes'))
    observeEvent(input$on_args_line, shinyjs::toggle('args_line'))
    observeEvent(input$on_args_values, shinyjs::toggle('args_values'))
    observeEvent(input$on_args_barplot, shinyjs::toggle('args_barplot'))
    observeEvent(input$on_args_general, shinyjs::toggle('args_general'))

    once<-reactiveVal(NULL)
    observe({
      req(is.null(once()))
      shinyjs::toggle('args_nodes')
      shinyjs::toggle('args_line')
      shinyjs::toggle('args_barplot')
      shinyjs::toggle('args_values')
      once(T)
    })

    model<-vals$cur_caret_model

    ggparty_fun<-function(model,ct,
                          type="barplot",
                          round=3,
                          title="Decision Tree",

                          palette=viridis::turbo,
                          base_size=12,
                          values.size = 3,
                          node_text.size=3,
                          node_fill="gray",
                          node_color="white",
                          linetype=2,
                          linecolor="Grey",
                          bar_xlab="",
                          bar_ylab="",
                          bar_base_size=5,
                          bar_theme='theme_classic',
                          bar_show_x=F,
                          bar_show_y=T

    ){
      if(model$modelType=="Classification"){
        ncol<-nlevels(ct$data[,".outcome"])
        colors<-palette(ncol)
      } else{
        ncol<-length(aggregate(
          predict(ct),
          list(predict(ct, type = "node")), FUN = function(x) round(mean(x),round)
        )$x)
        colors<-palette(ncol)
      }






      ct_node <- as.list(ct$node)
      for(i in 1:length(ct_node)){
        breaks<-ct_node[[i]]$split$breaks
        if(!is.null(breaks))
          ct_node[[i]]$split$breaks<-round(breaks,round)

      }
      ct$node <- partykit::as.partynode(ct_node)
      legname<-attr(model,"supervisor")
      p<-ggparty::ggparty(ct) +
        ggparty::geom_edge(linetype=linetype,color=linecolor) +
        ggparty::geom_edge_label(size = values.size) +
        ggparty::geom_node_splitvar(size = node_text.size,fill=node_fill, color=node_color)
      type=match.arg(type,c("barplot","simple","pies"))
      if(type=="barplot"){

        theme_args<-list()
        linetype_x  =ifelse(bar_show_x,1,0)
        linetype_y  =ifelse(bar_show_y,1,0)
        text_x  =ifelse(bar_show_x,1,0)
        text_y  =ifelse(bar_show_y,1,0)


        if(model$modelType=="Classification"){
          p2<-p+ggparty::geom_node_plot(gglist =  list(
            geom_bar(aes(x = "",
                         fill = .outcome),

                     position = position_fill()),
            scale_y_continuous(limits =c(0,1), n.breaks=5,labels=c(0,25,50,75,100)),
            xlab(bar_xlab),
            ylab(bar_ylab),
            scale_fill_manual(values=colors, name=legname),
            do.call(bar_theme,list(base_size=bar_base_size)),
            theme(

              axis.line.x=if(!bar_show_x){element_blank()}else{NULL},
              axis.line.y=if(!bar_show_y){element_blank()}else{NULL},
              axis.text.x=if(!bar_show_x){element_blank()}else{NULL},
              axis.text.y=if(!bar_show_y){element_blank()}else{NULL},
              axis.ticks.x=if(!bar_show_x){element_blank()}else{NULL},
              axis.ticks.y=if(!bar_show_y){element_blank()}else{NULL}

            )
          ))
        } else{
          colors<-palette(1)
          p2<-p+ggparty::geom_node_plot(gglist =  list(
            geom_boxplot(aes(x = "",
                             y = .outcome),
                         fill=colors
            ),

            xlab(bar_xlab),
            ylab(bar_ylab),
            do.call(bar_theme,list(base_size=bar_base_size)),
            theme(
              axis.line.x=if(!bar_show_x){element_blank()}else{NULL},
              axis.line.y=if(!bar_show_y){element_blank()}else{NULL},
              axis.text.x=if(!bar_show_x){element_blank()}else{NULL},
              axis.text.y=if(!bar_show_y){element_blank()}else{NULL},
              axis.ticks.x=if(!bar_show_x){element_blank()}else{NULL},
              axis.ticks.y=if(!bar_show_y){element_blank()}else{NULL}

            )
          ))

        }
      } else if(type=='simple') {
        if(model$modelType=="Classification"){
          pred <- aggregate(
            predict(ct),
            list(predict(ct, type = "node")), FUN = function(x) which.max(table(x))
          )
          prelab<-levels(predict(ct))[pred[,2]]
        } else{
          pred <- aggregate(
            predict(ct),
            list(predict(ct, type = "node")), FUN = function(x) round(mean(x),round)
          )
          prelab<-pred$x
        }



        terminal_color="white"
        na_id<-which(p$data$kids==0)

        p$data$splitvar[na_id]<-prelab
        p2<-p+ggparty::geom_node_info(
          ids=na_id,
          color=terminal_color,
          mapping=aes(label=splitvar, fill=splitvar),

        )+scale_fill_manual(values=colors) +
          guides(
            fill = guide_legend(
              title =attr(model,"supervisor"),
              override.aes = aes(label = "")
            )
          )
      } else if(type=="pies"){
        p2<-p+ ggparty::geom_node_plot(gglist = list(
          geom_bar(aes(x = "", fill = .outcome),
                   position = position_fill()),
          scale_fill_manual(values=colors, name=legname),
          coord_polar("y"),
          theme_void()
        ),
        size = "nodesize")
      }


      p2+ggtitle(title)

    }


    observeEvent(list(type=input$type,
                      round=input$round,
                      title=input$title,
                      node_fill=input$node_fill,
                      node_color=input$node_color,
                      node_text.size=input$node_text.size,
                      linetype=as.numeric(input$linetype),
                      linecolor=input$linecolor,
                      values.size = input$values.size,
                      palette=input$palette,
                      bar_xlab<-input$bar_xlab,
                      bar_ylab<-input$bar_ylab,
                      bar_base_size=input$bar_base_size,
                      bar_theme=input$bar_theme,
                      bar_show_x=input$bar_show_x,
                      bar_show_y=input$bar_show_y

    ),{
      shinyjs::addClass("run_btn","save_changes")

    })



    output$msp_rpart<-renderUI({
      width=input$width
      height=input$height
      if(is.na(width)){
        width="auto"
      }
      if(is.na(height)){
        height="auto"
      }
      renderPlot({
        plotree()
      },width=width,height=height)
    })

    plotree<-eventReactive(input$run,ignoreInit = T,{
      shinyjs::removeClass("run_btn","save_changes")

      mparty<-model$finalModel
      if(!inherits(mparty,"party")){
        mparty<-partykit::as.party(mparty)
      }
      args<-list(model=model,
                 ct=mparty,
                 type=input$type,
                 round=input$round,
                 title=input$title,
                 node_fill=input$node_fill,
                 node_color=input$node_color,
                 node_text.size=input$node_text.size,
                 linetype=as.numeric(input$linetype),
                 linecolor=input$linecolor,
                 values.size = input$values.size,
                 palette=vals$newcolhabs[[input$palette]],
                 bar_xlab=input$bar_xlab,
                 bar_ylab=input$bar_ylab,
                 bar_base_size=input$bar_base_size,
                 bar_theme=input$bar_theme,
                 bar_show_x=input$bar_show_x,
                 bar_show_y=input$bar_show_y

      )

      do.call(ggparty_fun,args)

    })

    observeEvent(ignoreInit = T,input$download_plot,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=plot_tree()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Decision Tree",datalist_name=datalist_name,name_c="dt_plot")
    })

  })}

model_control$server<-function(id,vals){
  moduleServer(id,function(input,output,session){


    observeEvent(vals$cur_model_params,{
      for(i in names(vals$cur_model_params)){
        updateTextInput(session,i,value= vals$cur_model_params[[i]])
      }

    })
    observe({
      for(i in names(input)){
        vals$cur_model_params[[i]]<-input[[i]]
      }

    })

    args0<-reactive({
      req(vals$trainSL_args)
      vals$trainSL_args
    })
    x<-reactive({args0()$x_train})
    y<-reactive({args0()$y_train[,1]})
    result0<-function(){
      req(vals$cmodel)
      model<-vals$cmodel
      x<-x()
      y<-y()


      args<-names(c(sl_formals[[model]],sl_ctl_formals[[model]]))
      re<-reshape_args(x,y,sl_formals,sl_ctl_formals,model)
      classes=re$classes
      argslist=re$argslist
      len=re$len
      relist<-list()
      input_names<-args
      x<-input_names[[1]]
      for(x in input_names ){
        if(length(input[[x]])>0){
          res<-input[[x]]
          if(is.na(res)){res<-NULL}
          relist[[x]]<-res}
      }

      relist
    }


    result<-reactive({
      req(result0())
      relist2<- relist<-result0()
      new<-lapply(seq_along(relist2),function(i){
        if(is.character( relist2[[i]])){
          if(grepl(', ', relist2[[i]])){
            as.numeric(strsplit(  relist2[[i]],", ")[[1]])
          } else{
            relist2[[i]]
          }
        }  else{
          relist2[[i]]
        }
      })
      names(new)<-names(relist2)
      new
    })


    observeEvent(input$linout,ignoreInit = T,{
      choices<-c('linout', 'entropy', 'softmax','censored' )
      if(isTRUE(input$linout)){

        lapply(choices[!choices%in%"linout"],function(x){
          updateCheckboxInput(session,x,value=F)
        })

      }
    })
    observeEvent(input$entropy,ignoreInit = T,{

      choices<-c('linout', 'entropy', 'softmax','censored' )
      if(isTRUE(input$entropy)){
        lapply(choices[!choices%in%"entropy"],function(x){
          updateCheckboxInput(session,x,value=F)
        })

      }
    })
    observeEvent(input$softmax,ignoreInit = T,{

      choices<-c('linout', 'entropy', 'softmax','censored' )
      if(isTRUE(input$softmax)){
        lapply(choices[!choices%in%"softmax"],function(x){
          updateCheckboxInput(session,x,value=F)
        })

      }
    })
    observeEvent(input$censored,ignoreInit = T,{
      choices<-c('linout', 'entropy', 'softmax','censored' )
      if(isTRUE(input$censored)){
        lapply(choices[!choices%in%"censored"],function(x){
          updateCheckboxInput(session,x,value=F)
        })

      }
    })




    observeEvent(result(),{

      vals$box_caret3_args<-result()


    })
    return(NULL)

  })
}
msp_xyf<-list()
msp_xyf$ui<-function(id,vals){
  ns<-NS(id)
  div(
    column(4,style='overflow: auto; max-height: 400px',
           box_caret(
             ns("box_a"),
             title="Background Options",
             div(
               checkboxInput(ns("plotY"),"Map Y", T),
               div(class="radio_search radio-btn-green",
                   radioGroupButtons(ns("xyf_somback_value"), "Background", choices=c("None"="None","U-Matrix"="uMatrix","Property"="property"))),

               div(id=ns("xyf_var_pproperty"),style="display: none; padding-left: 20px",
                   selectInput(ns("xyf_property_layer"),label = "Layer:",choices = NULL),
                   div(style="display: flex; width: 80%",
                       actionButton(ns("xyf_prev_property"),"<<",
                                    style="height: 23px; margin-top: 21px;padding: 1px; padding-left: 3px;padding-right: 3px"),
                       div(tags$div("Variable",style="margin-bottom: -5px"),style="max-width: 80%;",
                           selectInput(ns("xyf_variable_pproperty"),
                                       label = NULL,
                                       choices = NULL
                           )
                       ),
                       actionButton(ns("xyf_next_property"),">>",
                                    style="height: 23px; margin-top: 21px;padding: 1px; padding-left: 3px;padding-right: 3px")

                   )
               ),
               pickerInput_fromtop_live(inputId = ns("xyf_bg_palette"),
                                        label ="Palette",
                                        choices =  vals$colors_img$val,
                                        choicesOpt = list(
                                          content =  vals$colors_img$img),
                                        options=shinyWidgets::pickerOptions(windowPadding="top")),
               numericInput(ns("xyf_pcodes_bgalpha"),'Lightness',value = 0,min = 0,max = 1,step = .1),
               pickerInput_fromtop(ns("xyf_pclus_border"),
                                   label ='Border:',
                                   choices = NULL,
                                   options=shinyWidgets::pickerOptions(windowPadding="top")),
               checkboxInput(ns("xyf_theme"),
                             label = "show neuron coordinates",
                             value=F
               ),
               textInput(ns("xyf_title"), "Title:", ""),
               numericInput(ns("xyf_base_size"),'Base size',value = 12),
             )
           ),
           box_caret(
             ns("box_b"),
             title="Point Options",
             div(
               checkboxInput(ns("xyf_pclus_addpoints"),"Points",
                             value=T),
               div(id=ns("xyf_pclus_points_inputs"),style="padding-left: 20px",
                   pickerInput_fromtop_live(inputId = ns("xyf_pclus_points_palette"),
                                            label ="Palette:",
                                            choices = vals$colors_img$val,
                                            choicesOpt = list(content = vals$colors_img$img),
                                            options=shinyWidgets::pickerOptions(windowPadding="top"),
                                            selected="black"),
                   selectInput(ns("xyf_pclus_points_factor"),"Factor:",
                               choices =NULL),
                   pickerInput_fromtop(inputId = ns("xyf_pclus_symbol"),
                                       label = "Shape:",
                                       choices = df_symbol$val,
                                       choicesOpt = list(content = df_symbol$img),
                                       options=shinyWidgets::pickerOptions(windowPadding="top")),
                   numericInput(ns("xyf_pclus_points_size"),"Size:",value =1,min = 0.1,max = 3,step = .1),
               )
             )
           ),
           box_caret(
             ns("box_c"),
             title="Text Options",
             div(
               div(
                 checkboxInput(ns("xyf_pclus_addtext"),"Labels",F),
                 div(id=ns('xyf_pclus_text_inputs'),style="display: none;padding-left: 20px",
                     pickerInput_fromtop_live(inputId = ns("xyf_pclus_text_palette"),
                                              label ="Color",
                                              choices = NULL,
                                              options=shinyWidgets::pickerOptions(windowPadding="top")),
                     pickerInput_fromtop(ns("xyf_pclus_text_factor"),"Factor:",
                                         choices = NULL,
                                         options=shinyWidgets::pickerOptions(windowPadding="top",liveSearch = T)),
                     numericInput(ns("xyf_pclus_text_size"),'Size:',value = 1,min = 0.1,max = 3,step = .1)
                 )
               )
             )
           ),
           box_caret(
             ns("box_d"),
             title="Variable Factor Options",
             div(
               div(


                 checkboxInput(ns("xyf_varfacmap_action"), span("Variable factor map",actionLink(ns("xyf_varfacmap"), tipify_ui(icon("fas fa-question-circle"),'Click for more details',placement ="right"))),value =T),
                 div(style="padding-left: 20px",
                     id=ns('xyf_varfac_out'),
                     pickerInput_fromtop(ns("xyf_vfm_type"),"Show correlation:",
                                         choices =list("Highest"='var', "Chull"="cor"),
                                         options=shinyWidgets::pickerOptions(windowPadding="top")
                     ),


                     numericInput(ns("xyf_npic"), span('Number',tipright("Number of variables to display")), value =10),

                     numericInput(ns("xyf_pclus.cex.var"), "Var size", value =1),
                     pickerInput_fromtop(inputId = ns("xyf_p.clus.col.text"),
                                         label ="Var text color",
                                         choices = NULL,
                                         options=shinyWidgets::pickerOptions(windowPadding="top")),
                     pickerInput_fromtop(inputId = ns("xyf_var_bg"),
                                         label ="Var background",
                                         choices = NULL,
                                         options=shinyWidgets::pickerOptions(windowPadding="top")),
                     numericInput(ns("xyf_var_bg_transp"), 'Var transparency', value = 0, min = 2),
                     actionLink(ns("down_vfm"),"Download Results")
                 )
               )
             )
           ),

    ),
    column(6,
           box_caret(ns("box_e"),
                     title="Kohonen Plot",
                     button_title = actionLink(ns("download_plot"),
                                               "Download",icon("download")),
                     div(
                       div(id=ns('run_btn'),class="save_changes",

                           style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                       uiOutput(ns('msp_xyf')))

           ))
  )

}
msp_xyf$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    box_caret_server("box_a")
    box_caret_server("box_b")
    box_caret_server("box_c")
    box_caret_server("box_d")
    box_caret_server("box_e")
    data_x<-attr(model,"Datalist")
    model<-model$finalModel
    names(model$unit.classif)<-rownames(model$data[[1]])

    getsolid_col<-reactive({

      res<-lapply(vals$newcolhabs, function(x) x(10))
      res1<-unlist(lapply(res, function(x) x[1]==x[2]))
      solid<-names(res1[res1==T])
      pic<-which(vals$colors_img$val%in%solid)
      pic

    })

    observeEvent(vals$newcolhabs,{
      choices<-getsolid_col()
      updatePickerInput(session,'xyf_pclus_text_palette',
                        choices =  vals$colors_img$val[getsolid_col()],
                        selected="black",
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ))
      updatePickerInput(session,'xyf_var_bg',
                        choices =  vals$colors_img$val[getsolid_col()],
                        selected="white",
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ))
      updatePickerInput(session,'xyf_p.clus.col.text',
                        choices =  vals$colors_img$val[getsolid_col()],
                        selected="black",
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ))
    })



    getgrad_col<-reactive({
      res<-lapply(vals$newcolhabs, function(x) x(2))
      res1<-unlist(lapply(res, function(x) x[1]==x[2]))
      grad<-names(res1[res1==F])
      pic<-which(vals$colors_img$val%in%grad)
      pic
    })
    observeEvent(xyf_get_choices_pal(),{
      choices<-xyf_get_choices_pal()

      updatePickerInput(session,'xyf_bg_palette',
                        choices =  vals$colors_img$val[choices],
                        choicesOpt = list(
                          content =  vals$colors_img$img[choices] )
      )
    })
    observeEvent(input$xyf_somback_value,{
      shinyjs::toggle("xyf_var_pproperty",condition=input$xyf_somback_value=="property")
    })

    observeEvent(model,{
      updateSelectInput(session,'xyf_property_layer',choices= names(model$data))
    })

    xyf_getdata_layer<-reactive({
      choices0 = names(model$data)
      if(length(choices0)>0){
        req(input$xyf_property_layer)
        model$data[[input$xyf_property_layer]]
      } else{
        model$data[[1]]
      }

    })
    observeEvent(xyf_getdata_layer(),{
      updateSelectInput(session,'xyf_variable_pproperty',choices=colnames(xyf_getdata_layer()))
    })
    observe({
      data = xyf_getdata_layer()
      condition=which(colnames(data)==input$xyf_variable_pproperty)!=1
      shinyjs::toggle('xyf_prev_property',condition=condition)
    })
    observe({
      data = xyf_getdata_layer()
      condition=which(colnames(data)!=input$xyf_variable_pproperty)==ncol(data)
      shinyjs::toggle('xyf_next_property',condition=condition)
    })
    observe({
      shinyjs::toggle("xyf_pclus_points_inputs",condition=isTRUE(input$xyf_pclus_addpoints))
    })
    observeEvent(input$xyf_pclus_addtext,{
      req(data_x)
      choices<-c(colnames(attr(vals$saved_data[[data_x]],"factors")))
      updatePickerInput(session,'xyf_pclus_text_factor',choices=choices)

    })

    observeEvent(input$xyf_pclus_addpoints,{
      req(data_x)
      choices<-c(colnames(attr(vals$saved_data[[data_x]],"factors")))
      updateSelectInput(session,'xyf_pclus_points_factor',choices=choices)
    })
    observe({
      shinyjs::toggle('xyf_pclus_text_inputs',condition=isTRUE(input$xyf_pclus_addtext))
    })
    observe({
      shinyjs::toggle('xyf_varfac_out',condition=isTRUE(input$xyf_varfacmap_action))
    })


    output$textvarfacmap<-renderUI({

      div(



        div(
          column(12,
                 h4("Variable factor map"),
                 p("The chart is very similar to the variable factor map obtained from the principal component analysis (PCA). It calculates the weighted correlation for each variable using the coordinates (x, y) of the neurons and their weights (number of instances). The codebooks vectors of the cells correspond to an estimation of the conditional averages, calculating their variance for each variable is equivalent to estimating the between-node variance of the variable, and hence their relevance."),
                 p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("Chull correlations")," returns",code("npic")," variables with the highest correlation considering the convex hull, while also ensuring that the points are ordered by their proximity to codebook center")
          )

        )
      )

    })

    observeEvent(ignoreInit = T,input$xyf_varfacmap, {
      showModal(modalDialog(
        uiOutput(ns("textvarfacmap")),
        title = h4(strong("Variable factor map")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      ))
    })


    observeEvent(vals$newcolhabs,{
      choices<-getsolid_col()
      updatePickerInput(session,'xyf_pclus_border',

                        choices =  vals$colors_img$val[choices],
                        selected="white",
                        choicesOpt = list(
                          content =  vals$colors_img$img[choices] )
      )
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
      m<-model
      hexs<-get_neurons(m,background_type=backtype,property=property, hc=NULL)
      hexs
    })
    xyf_get_copoints<-reactive({
      copoints<-getcopoints(model)
      copoints
    })
    xyf_copoints_scaled<-reactive({
      points_tomap=rescale_copoints(hexs=xyf_get_network(),copoints=xyf_get_copoints())
      data<-vals$saved_data[[data_x]]
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

      points_tomap
    })
    xyf_get_choices_pal<-reactive({


      req(length(input$xyf_somback_value)>0)

      if(input$xyf_somback_value=="None"){
        vals$somplot_bg<-"white"
        choices=getsolid_col()
      } else {

        vals$somplot_bg<-"viridis"
        choices=getgrad_col()

      }

      choices
    })
    xyf_bp_som<-reactive({
      req(model)
      iind=xyf_indicate_hc()
      req(iind)
      m<-model

      args<-list(m=m,indicate=iind$indicate,npic=iind$npic,hc=NULL)


      bp<-getbp_som2(m=m,indicate=iind$indicate,npic=iind$npic,hc=NULL)

      bp
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
    xyf_argsplot<-reactive({
      req(input$xyf_pcodes_bgalpha)
      if(isTRUE(input$xyf_pclus_addpoints)){
        req(input$xyf_pclus_points_factor)
        points_factor= NULL }
      if(isTRUE(input$xyf_pclus_addtext)){
        req(input$xyf_pclus_text_factor)
        text_factor= NULL }
      indicate=xyf_indicate_hc()
      m<-model
      args<-list(m=m,
                 hexs=xyf_get_network(),
                 points_tomap=xyf_copoints_scaled(),
                 bp=xyf_bp_som(),
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
                 show_error=NULL,
                 base_size=input$xyf_base_size,
                 show_neucoords=input$xyf_theme,
                 newdata=input$xyf_newdata,
                 title=input$xyf_title,
                 hc=NULL
      )
      args

    })

    observeEvent(ignoreInit = T,input$down_vfm,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"VFM resutls"
      data<-xyf_bp_som()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Variable Factor Map Results",data=data, name=name)
    })


    observeEvent(list(
      input$plotY,
      input$xyf_somback_value,
      input$xyf_property_layer,
      input$xyf_variable_pproperty,
      input$xyf_next_property,
      input$xyf_bg_palette,
      input$xyf_pcodes_bgalpha,
      input$xyf_pclus_border,
      input$xyf_theme,
      input$xyf_title,
      input$xyf_base_size,
      input$xyf_pclus_addpoints,
      input$xyf_pclus_points_palette,
      input$xyf_pclus_points_factor,
      input$xyf_pclus_symbol,
      input$xyf_pclus_points_size,
      input$xyf_pclus_addtext,
      input$xyf_pclus_text_palette,
      input$xyf_pclus_text_factor,
      input$xyf_pclus_text_size,
      input$xyf_varfacmap_action,
      input$xyf_vfm_type,
      input$xyf_npic,
      input$xyf_pclus.cex.var,
      input$xyf_p.clus.col.text,
      input$xyf_var_bg,
      input$xyf_var_bg_transp
    ),{
      shinyjs::addClass("run_btn","save_changes")
    })

    xyfplot<-reactiveVal()
    xyf_plot<-eventReactive(input$run,ignoreInit = T,{
      shinyjs::removeClass("run_btn","save_changes")
      m<-model
      args<-xyf_argsplot()

      args$plotY<-input$plotY
      req(length(args)>0)
      bp<-args$bp
      bp$id=NULL
      vals$biplot_som<-bp
      req(length(args$hexs)>0)

      p<-do.call(bmu_plot,args)
      xyfplot(p)
      p
    })
    output$msp_xyf<-renderUI({renderPlot(xyf_plot())})
    observeEvent(ignoreInit = T,input$download_plot,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=xyfplot()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Kohonen plot",datalist_name=datalist_name,name_c="kohonen_plot")
    })

  })
}
msp_rf<-list()
msp_rf$ui<-function(id,vals){
  ns<-NS(id)
  column(12,class="mp0",
         column(4,class="mp0",
                box_caret(ns("box_a"),
                          title="Analysis options",
                          color="#c3cc74ff",

                          div(
                            div(id=ns('run_btn'),class="save_changes",
                                align="right",
                                style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                            pickerInput_fromtop(ns("fm_tree"),"+ Tree",NULL,
                                                options=shinyWidgets::pickerOptions(windowPadding="top",liveSearch = T)),
                            numericInput(ns('fm_round'),'+ Round',2),
                            pickerInput_fromtop(
                              ns("edge_type"), "+ Edge type",
                              choices=list("Link"="link",'Fluid'="fluid"),
                              options=shinyWidgets::pickerOptions(windowPadding="top")
                            ),
                            pickerInput_fromtop(ns("tree_type"), "+ Tree type", choices=list("Dendrogram"="dendrogram","Tree"='tree'))

                          )
                ),


                box_caret(ns('box_b'),
                          title="Plot options",
                          color="#c3cc74ff",
                          div(
                            pickerInput_fromtop_live(inputId = ns("fm_palette"),
                                                     label = 'Palette',
                                                     choices =     vals$colors_img$val,
                                                     choicesOpt = list(content =vals$colors_img$img),
                                                     selected=vals$cm_palette,
                                                     options=shinyWidgets::pickerOptions(windowPadding="top")),
                            numericInput(ns("msp_plot_base_size"),"Base size",12),
                            numericInput(ns("text_size"),"Text size",8),
                            numericInput(ns("msp_plot_width"), "Plot width",600),
                            numericInput(ns("msp_plot_height"), "Plot height",350),
                          )
                )
         ),
         column(8,class="mp0",
                box_caret(
                  ns('box_c'),
                  title="Plot",
                  button_title = actionLink(ns("download_plot"),
                                            "Download",icon("download")),
                  div(
                    style="overflow:auto;max-height: 300px",
                    uiOutput(ns("treeplot"))
                  )
                ),
                box_caret(
                  ns('box_d'),
                  title="Confusion Matrix",
                  button_title = actionLink(ns("download_plot_cm"),
                                            "Download",icon("download")),
                  div(
                    style="overflow:auto;max-height: 300px",
                    uiOutput(ns("treecm"))
                  )
                )



         )
  )
}
msp_rf$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){



    box_caret_server('box_a')
    box_caret_server('box_b')
    box_caret_server('box_c')
    box_caret_server('box_d')


    observeEvent(ignoreInit = T,input$download_plot_cm,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=getcm()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Confusion Matrix - Tree",datalist_name=datalist_name,name_c="cm_tree")
    })
    observeEvent(ignoreInit = T,input$download_plot,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=gettree()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Tree from RandomForest",datalist_name=datalist_name,name_c="tree")
    })
    getcm<-reactive({
      m<-model
      predall=predall_rf()
      req(predall)
      req(input$fm_tree)
      obc<-getdata_model(m,"test")
      pred<-predall[,input$fm_tree]
      tree_num=as.numeric(gsub("tree","",colnames(predall[input$fm_tree])))
      cm<-table(pred,obc)

      plotCM(cm, palette=input$fm_palette,vals$newcolhabs, font_color="black", title=paste0("Tree",tree_num), round_cm=3)
    })
    output$treecm<-renderUI({


      renderPlot(getcm())
    })

    predall_rf<-eventReactive(input$run,ignoreInit = T,{
      m<-model
      req(class(m$finalModel)=="randomForest")
      shinyjs::removeClass("run_btn","save_changes")

      newdata<-getdata_model(m)
      obc<-getdata_model(m,"test")
      treepred=predict(m$finalModel,newdata= newdata,predict.all=T)$individual
      colnames(treepred)<-paste0("tree",1:ncol(treepred))
      rownames(treepred)<-rownames(newdata)
      acc_trees<-apply(treepred,2,function(x) caret::postResample(x,obc))
      metric<-ifelse(m$modelType=="Classification","Accuracy","Rsquared")
      neword<-order(acc_trees[metric,], decreasing = T)
      tree_predictions<-data.frame(treepred[,neword])
      tree_predictions<-data.frame(lapply(tree_predictions,function(x) factor(x,levels=levels(m))))
      rownames(tree_predictions)<-rownames(newdata)
      tree_predictions
    })
    observeEvent(predall_rf(),{
      updatePickerInput(session,'fm_tree',choices=colnames(predall_rf()),options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(list(
      input$fm_tree,
      input$fm_round,
      input$msp_plot_base_size,
      input$newcolhabs,
      input$fm_palette,
      input$fm_round,
      input$tree_type,
      input$edge_type
    ),{
      shinyjs::addClass("run_btn","save_changes")
    })

    gettree<-reactive({

      m<-model
      predall=predall_rf()
      tree_num=as.numeric(gsub("tree","",colnames(predall[input$fm_tree])))

      req(input$fm_tree)

      newdata<-getdata_model(m)
      obc<-getdata_model(m,"test")
      pred<-predall[,input$fm_tree]
      acc<-caret::postResample(pred,obc)


      tree_func(final_model = m$finalModel,
                acc=round(acc,input$fm_round),
                base_size=input$text_size,
                tree_num=tree_num,
                newcolhabs=vals$newcolhabs,
                palette=input$fm_palette,
                round=input$fm_round,
                tree_type=input$tree_type,
                edge_type=input$edge_type
      )+theme_bw(base_size = input$msp_plot_base_size)
    })
    output$treeplot<-renderUI({

      m<-model
      req(input$msp_plot_width)
      req(input$msp_plot_height)

      req(class(m$finalModel)=="randomForest")

      div(
        div(

          renderPlot({
            gettree()
          }, width=input$msp_plot_width,height=input$msp_plot_height)
        )

      )

    })
    return(NULL)
  })
}
msp_cforest<-list()
msp_cforest$ui<-function(id,vals){
  model<-vals$cur_caret_model
  choices<-colnames(getdata_model(model))
  ns<-NS(id)
  choices_newdata=c("Training","Partition","Datalist")


  div(
    #  shinydashboard::dashboardPage(dashboardHeader(),sidebar,body),

    column(4,style='overflow: auto; max-height: 400px',


           box_caret(
             ns("box_a"),
             title="Plot Options",
             color="#c3cc74ff",
             div(
               div(class="radio_search radio-btn-green",
                   radioGroupButtons(ns("newdata"),"New data",choices_newdata),
                   selectInput(ns("newdata_dl"),"Datalist",NULL),
               ),
               div(
                 actionLink(ns("rank_tree"),">> Rank trees"),tipright("Click to order the trees based on their performance. The <code>Tree</code> suspended menu will be updated from highest to lowest performing.")
               ),
               div(style="display: flex",
                   pickerInput_fromtop(ns("tree"),"Tree",choices=1:length(model$finalModel@ensemble),options=shinyWidgets::pickerOptions(liveSearch =T,windowPadding="top")),
                   div(id=ns('run_btn'),class="save_changes",style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px"),tipright("Click to get results from the selected tree."))
               ),


               textInput(ns('title'),"Title","Tree"),
               numericInput(ns("width"),
                            div("Plot width",
                                tipright("in pixels or empty for auto width"))
                            ,NA),
               numericInput(ns("height"), div("Plot height",
                                              tipright("in pixels or empty for auto height")),NA),

               selectInput(ns("type"),"Type",choices=c("extended","simple")),

               numericInput(ns("base_size"),"Base size",10),
               numericInput(ns("digits"),"Digits:",3),
               pickerInput_fromtop_live(
                 inputId = ns("palette"),
                 label ="Palette",
                 choices =  vals$colors_img$val,
                 choicesOpt = list(
                   content =  vals$colors_img$img),
                 options=shinyWidgets::pickerOptions(windowPadding="top")
               )
             )
           )

    ),
    column(8,style="overflow: auto; max-height: 100vh",
           box_caret(ns("box_b"),
                     title="Tree Plot",
                     button_title = actionLink(ns("download_plot"),
                                               "Download",icon("download")),
                     div(
                       uiOutput(ns('msp_cforest')),
                     )

           ),
           box_caret(ns("box_c"),
                     title="Confusion Matrix - Single Tree",
                     button_title = actionLink(ns("download_cm"),
                                               "Download",icon("download")),
                     uiOutput(ns('cm_cforest'))

           )
    )
  )

}
msp_cforest$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){

    box_caret_server("box_a")
    box_caret_server("box_b")
    box_caret_server("box_c")
    model<-vals$cur_caret_model


    get_newdata_obs<-reactive({
      if(input$newdata=='Datalist'){
        newdata=vals$saved_data[[input$newdata_dl]]
        obs<-attr(newdata,"factors")[,attr(model,"supervisor")]

      } else if(input$newdata=="Partition"){
        newdata<-attr(model,"test")
        obs<-attr(model,"sup_test")[,1]

      } else if(input$newdata=="Training"){
        newdata=train
        obs<-getdata_model(model,"test")
      }
      return(list(obs=obs,newdata=newdata))

    })

    observeEvent(input$rank_tree,{
      ntrees<-length(model$finalModel@ensemble)
      obs=get_newdata_obs()$obs
      newdata=get_newdata_obs()$newdata

      withProgress(min=1,max=ntrees,message="Running",{
        res<-sapply(1:ntrees,function(i){
          ct<-get_cTree(model$finalModel,i)
          pred<-predict_cforest_tree(ct@tree,newdata)
          metric<-caret::postResample(pred,obs)[model$metric]
          incProgress(1)

          metric
        })

        updatePickerInput(session,"tree",
                          choices=c(1:ntrees)[order(res,decreasing=T)],options=shinyWidgets::pickerOptions(liveSearch=T))




      })
    })


    train<-getdata_model(model)
    choices<-names(which(sapply(vals$saved_data,function(x){
      all(colnames(x)%in%colnames(train))
    })))
    updateSelectInput(session,'newdata_dl',choices=choices)




    observe({
      shinyjs::toggle("newdata_dl",condition=input$newdata=="Datalist")
    })
    get_cTree <- function(cf, k=1) {
      dt <- cf@data@get("input")
      tr <- party:::prettytree(cf@ensemble[[k]], names(dt))
      tr_updated <- update_tree(tr, dt)
      new("BinaryTree", tree=tr_updated, data=cf@data, responses=cf@responses,
          cond_distr_response=cf@cond_distr_response, predict_response=cf@predict_response)
    }
    update_tree <- function(x, dt) {
      x <- update_weights(x, dt)
      if(!x$terminal) {
        x$left <- update_tree(x$left, dt)
        x$right <- update_tree(x$right, dt)
      }
      x
    }
    update_weights <- function(x, dt) {
      splt <- x$psplit
      spltClass <- attr(splt,"class")
      spltVarName <- splt$variableName
      spltVar <- dt[,spltVarName]
      spltVarLev <- levels(spltVar)
      if (!is.null(spltClass)) {
        if (spltClass=="nominalSplit") {
          attr(x$psplit$splitpoint,"levels") <- spltVarLev
          filt <- spltVar %in% spltVarLev[as.logical(x$psplit$splitpoint)]
        } else {
          filt <- (spltVar <= splt$splitpoint)
        }
        x$left$weights <- as.numeric(filt)
        x$right$weights <- as.numeric(!filt)
      }
      x
    }
    cforet_tree<-function(model,tree=1,base_size=10,
                          type="extended",
                          digits=3,
                          palette=viridis::turbo,
                          inner_fill="white",
                          title="Tree"){

      object<-model$finalModel
      ncol<-ifelse(model$modelType=="Classification",length(model$levels),1)
      colors<-   palette(ncol)

      res<-get_cTree(object,tree)


      grid::grid.newpage()
      grid::pushViewport(grid::viewport(gp=grid::gpar(
        fontsize=base_size,
        ann=F,
        col.axis="black"
      )))
      party:::plot.BinaryTree(
        res,type=type,
        #main=title,
        terminal_panel = NULL,
        tp_args = list(fill=colors,id=F),
        inner_panel = party::node_inner,
        ip_args = list(digits=digits,fill=inner_fill),
        edge_panel = party::edge_simple,
        # ep_args = list(fill="red"),
        newpage =F,
        pop=F
      )
    }

    get_cm<-eventReactive(input$run,ignoreInit = T,{
      if(input$newdata=='Datalist'){
        newdata=vals$saved_data[[input$newdata_dl]]
        obs<-attr(newdata,"factors")[,attr(model,"supervisor")]

      } else if(input$newdata=="Partition"){
        newdata<-attr(model,"test")
        obs<-attr(model,"sup_test")[,1]

      } else if(input$newdata=="Training"){
        newdata=train
        obs<-getdata_model(model,"test")
      }

      ct<-get_cTree(model$finalModel,as.numeric(input$tree))
      pred<-predict_cforest_tree(ct@tree,newdata)

      cm<-table(pred,obs)
      res<-plotCM(cm,input$palette,vals$newcolhabs,title=paste("Tree",input$tree))
      res
    })

    observeEvent(ignoreInit = T,input$download_cm,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=get_cm()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="CM-Single Treee",datalist_name=datalist_name,name_c=paste0("cm_tree",input$tree))
    })

    output$cm_cforest<-renderUI({
      validate(need(model$modelType=="Classification","Confusion matrices are only valid for classification models."))
      renderPlot({
        get_cm()
      })
    })


    observeEvent(list(
      input$tree,
      input$title,
      input$type,
      input$base_size,
      input$digits,
      input$palette),{
        shinyjs::addClass("run_btn","save_changes")

      })



    output$msp_cforest<-renderUI({
      req( plotree())
      width=input$width
      height=input$height
      if(is.na(width)){
        width="auto"
      }
      if(is.na(height)){
        height="auto"
      }
      renderPlot({
        plotree()
      },width=width,height=height)
    })

    plotree<-eventReactive(input$run,ignoreInit = T,{
      shinyjs::removeClass("run_btn","save_changes")


      args<-list(model=model,
                 tree=as.numeric(input$tree),base_size=input$base_size,
                 type=input$type,
                 digits=input$digits,
                 palette=vals$newcolhabs[[input$palette]],
                 inner_fill="white",
                 title=input$title)

      do.call(cforet_tree,args)
      res<-recordPlot()
      res
    })

    observeEvent(ignoreInit = T,input$download_plot,{
      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=plotree()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Decision Tree",datalist_name=datalist_name,name_c="dt_plot")
    })

  })}


panel_box_caret2<-list()
panel_box_caret2$ui<-function(id){
  ns<-NS(id)

  uiOutput(ns("grid_parameters"))



}
panel_box_caret2$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns



    output$grid_parameters<-renderUI({
      x<-vals$cur_xtrain
      y<-vals$cur_var_y
      model<-vals$cmodel
      req(x)
      req(y)
      req(model)
      req(!model%in%"rfGA")
      res<-run_gridcaret(x,y,model)
      param<-res$param
      classes<-res$classes
      lapply(1:ncol(param),function(i){
        switch (classes[[i]],
                'numeric' = {


                  value<-paste0(unique(paste0(param[,i])),collapse=", ")


                  textInput(ns(colnames(param)[i]),
                            span(colnames(param)[i],tipright(model_parameters[[model]]$label[i])),
                            value)
                },
                'factor' = {

                  choices<-paste0(unique(paste0(param[,i])),collapse=", ")
                  if(model=="xyf"){
                    choices=c("hexagonal","rectangular")
                  }
                  selectInput(ns(colnames(param)[i]),
                              span(colnames(param)[i],tipright(model_parameters[[model]]$label[i])),
                              choices=choices,multiple = T,selected=choices)
                },
                'logical' = {
                  choices<-c('TRUE','FALSE')
                  selectInput(ns(colnames(param)[i]),
                              span(colnames(param)[i],tipright(model_parameters[[model]]$label[i])),
                              choices=choices,multiple = T,selected=choices)

                }


        )

      })

    })


    result<-reactive({
      try({


        x<-vals$cur_xtrain
        y<-vals$cur_var_y
        model<-vals$cmodel
        req(x)
        req(y)
        req(model)
        req(!model%in%"rfGA")
        res0<-res<-run_gridcaret(x,y,model)
        classes<-res$classes
        param<-res$param
        req(classes)
        reslist<-list()
        for(x in colnames(res0[[2]])){
          req(input[[x]])

          if(classes[[x]]%in%c("numeric","integer")){
            vec<-gsub(" ","",strsplit(input[[x]],",")[[1]])
            vec<-as.numeric(vec)
          } else{
            vec<-input[[x]]
          }
          reslist[[x]]<-vec
        }
        reslist

      })
    })
    observeEvent(result(),{
      req(result())

      vals$box_caret2_args<-result()

    })



    return(NULL)


  })
}
panel_box_caret4<-list()
panel_box_caret4$ui<-function(id){
  lab_resamling<-span(
    class="tip_html150",
    strong("Method:"),
    tipright(
      HTML(paste0(
        tags$p(class="title",
               "Resampling Methods:"),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "repeatedcv:"
          ),
          'Repeated K-fold cross validation'
        ))),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "boot:"
          ),
          'Bootstrap for cross validation'
        ))),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "LOOCV:"
          ),
          'Leave one out cross validation'
        ))),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "LGOCV:"
          ),
          'Leave group out cross validation'
        ))),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "adaptive_cv:"
          ),
          'Adaptative cross validation'
        )))








      )
      ))
  )

  ns<-NS(id)
  div(
    id=ns("resamp_parameters"),
    div(

      pickerInput_fromtop(ns("method"),lab_resamling, choices=list("Repeated Cross-Validation"="repeatedcv",'Boot'="boot","Leave one out"="LOOCV","Leave-group out"="LGOCV","Adaptive CV"="adaptive_cv"),
                          options=shinyWidgets::pickerOptions(windowPadding="top")),
      div(id=ns('args_adapt'),class="map_side",
          numericInput(ns("adap_min"),"min",5),
          numericInput(ns("adap_alpha"),"alpha",0.05),
          selectInput(ns("adap_method"),"method",c('gls',"BT")),
          checkboxInput(ns("adap_complete"),"complete",T)

      ),
      pickerInput_fromtop(ns("selfinal"),
                          span("Select Final",
                               tipify_ui(actionLink(ns("selfinal_help"),
                                                    icon("fas fa-question-circle")),"Click for details","right")),
                          choices=c("best","oneSE","tolerance"),
                          options=shinyWidgets::pickerOptions(windowPadding="top")),



      numericInput(ns("cv"), uiOutput(ns('label_cv')), value = 5),
      numericInput(ns("repeats"),span("Repeat",tipright("the number of complete sets of folds to compute")), value = 1),
      numericInput(ns("pleaves"), span( "Percentage:",tipright('the training percentage')), value = 10),
      numericInput(ns("seed"), span("Seed",tipright(textseed())), value = NA)
    )

  )
}
panel_box_caret4$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns

    observe({
      shinyjs::toggle("args_adapt",condition=input$method=="adaptive_cv")
    })

    observeEvent(input$selfinal_help,{
      showModal(modalDialog(
        div(h4("Methods for Selecting the Final Model:"),
            tags$ul(
              tags$li("Default: Chooses the model with the largest performance value (or smallest, for mean squared error in regression models)."),
              tags$li("OneSE: Identifies the model with the best performance value and estimates the standard error of performance using resampling. The final model selected is the simplest model within one standard error of the best model, following the approach proposed by Breiman et al. (1984). This method is particularly suitable for simple trees to prevent overfitting."),
              tags$li("Tolerance: Selects the least complex model within a certain percentage tolerance of the best value.")
            )
        ),
        easyClose = T
      ))
    })
    observe({
      shinyjs::toggle("cv",condition=input$method%in%c('cv','repeatedcv','boot','adaptive_cv'))
      shinyjs::toggle('repeats', condition=input$method%in%c('repeatedcv','adaptive_cv'))
      shinyjs::toggle("pleaves",condition=input$method=='LGOCV')
    })



    output$label_cv<-renderUI({
      req(input$method)
      if(input$method=='boot'){
        label=span("Number",tipright("the number of resampling iterations"))
        value=99
      } else{
        label=span("CV folds",tipright("number of folds"))
        value=5
      }
      label

    })


    result<-reactive({
      list(
        seed=input$seed,
        method=input$method,
        cv=input$cv,
        repeats=input$repeats,
        pleaves=input$pleaves,
        selfinal=input$selfinal,
        adap_min=input$adap_min,
        adap_alpha=input$adap_alpha,
        adap_method=input$adap_method,
        adap_complete=input$adap_complete
      )
    })

    observeEvent(result(),{
      vals$box_caret4_args<-result()
    })

    return(NULL)

  })
}



get_cm_impact<-function(predtable,var){
  req(is.data.frame(predtable))
  req(nrow(predtable)>1)

  rands_lis<-split(predtable,predtable$var)
  res<-rands_lis[[var]]
  dft<-table(res$pred,res$obs)
  dft
}



rf_oob_pred_prob <- function(forest, X) {

  # Get individual predictions
  preds = predict(forest, X, predict.all = TRUE)

  # Identify OOB samples
  oob = forest$inbag == 0
  predind <- preds$individual
  predind[!oob] <- NA

  # Calculate class probabilities
  class_levels <- levels(forest$y)
  prob_matrix <- matrix(NA, nrow = nrow(X), ncol = length(class_levels))
  colnames(prob_matrix) <- class_levels

  for (i in seq_along(class_levels)) {
    class <- class_levels[i]
    prob_matrix[, i] <- rowMeans(predind == class, na.rm = TRUE)
  }

  return(prob_matrix)
}

rf_oob_pred<-function(forest, X) {
  if(is.null(forest$inbag)){
    return(predict(forest, X))
  }
  if(forest$problemType=="Classification"){
    preds = predict(forest, X, predict.all=TRUE)
    oob = forest$inbag==0
    predind<-  preds$individual
    predind[which(!oob)]<-NA
    res<-apply(predind,1,function(x){
      names(which.max(table(x[!is.na(x)])))
    })
    res<-factor(res,levels=forest$obsLevels)
    return(res)
  } else{
    preds = predict(forest, X, predict.all=TRUE)
    oob = forest$inbag==0
    oob[which(!oob)] = NA
    preds.oob = oob*preds$individual
    preds.oob
  }

}
permutation_importance3<-function(model, n_permutations = 99, num_cores = 1,session=MockShinySession$new(),seed=NA,replace=F) {
  if (num_cores < 1) {
    stop("num_cores deve ser um número inteiro positivo.")
  }
  if(is.na(seed)){seed=NULL}

  data <- model$trainingData
  predictors <- colnames(data)[colnames(data) != ".outcome"]
  obs <- data[,".outcome"]
  train <- data[predictors]
  var_metrics <- vector("list", length(predictors))
  max <- length(predictors)*n_permutations
  start <- 1
  set.seed(seed) # Setting seed for reproducibility
  seeds<-runif(length(predictors)*n_permutations,min=min(.Random.seed),max=max(.Random.seed))
  seeds<-split(seeds,rep(seq_along(predictors),each=length(n_permutations)))


  if(inherits(model$finalModel,"randomForest")){
    pred_train <- rf_oob_pred(model$finalModel, train)
  } else{
    pred_train <- predict(model, train)
  }

  withProgress(min=1,max=max,message="Running... ",session =session ,{
    var_metrics<-list()
    for (i in seq_along(predictors)) {

      var <- predictors[i]
      metrics <- vector("list", n_permutations)

      rj<-list()
      for(j in seq_len(n_permutations)){
        incProgress(1,paste0("Randomizing...",var),session =session )
        newdata <- train
        set.seed(seeds[[i]][j])
        newdata[, var] <- sample(train[, var],replace=replace)
        if(inherits(model$finalModel,"randomForest")){
          pred <- rf_oob_pred(model$finalModel, newdata)
        } else{
          pred <- predict(model, newdata)
        }
        df<-data.frame(var = var, pred = pred, obs = obs, rep = j,pred_train=pred_train)
        rj[[j]]<-df
      }

      var_metrics[[i]] <- do.call(rbind,rj)
    }
  })


  predtable<-do.call(rbind, var_metrics)
  predtable
}
permimp_metric<-function(predtable, m,metric="Accuracy", class=F, newdata,obc,session=MockShinySession$new()){
  actual<-NULL
  metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')

  if(!'pred_train'%in%colnames(predtable)){
    train<-getdata_model(m)
    if(inherits(m$finalModel,"randomForest")){
      pred_train <- rf_oob_pred(m$finalModel, train)
    } else{
      pred_train <- predict(m, train)
    }
    predtable$pred_train<-rep(rep(pred_train,max(predtable$rep)),ncol(train))
  }


  if(isFALSE(class)){
    to_split<-predtable[c("var",'rep')]
  } else{
    to_split<-predtable[c("var",'obs','rep')]
  }
  rands_lis<-split(predtable,to_split)
  withProgress(
    min=1,
    max=length(rands_lis),
    session=session,
    message="Calculating metrics...",{
      remetric<-list()
      for(i in seq_along(rands_lis)){

        incProgress(1,   session=session)
        #print(paste0(i,"/",length(rands_lis)))
        xx<-rands_lis[[i]]
        pred_metric=as.vector(c(caret::postResample(xx$pred,xx$obs)[metric]))
        var=xx$var
        rep=xx$rep
        obs=xx$obs
        pred_train=xx$pred_train
        actual_metric<-as.vector(c(caret::postResample(pred_train,obs)[metric]))
        remetric[[i]]<-data.frame(var,pred_metric,actual_metric,rep,obs,pred_train)


      }

    })

  metrics<-data.frame(do.call(rbind,remetric))



  metrics
}
sig_feature<-function(permimp,m){
  metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
  obs<- getdata_model(m,"test")
  testdata<-attr(m,"test")

  if(!'pred_train'%in%colnames(permimp)){
    train<-getdata_model(m)
    if(inherits(m$finalModel,"randomForest")){
      pred_train <- rf_oob_pred(m$finalModel, train)
    } else{
      pred_train <- predict(m, train)
    }
    permimp$pred_train<-rep(rep(pred_train,max(permimp$rep)),ncol(train))
  }
  colnames(permimp)[2]<-metric

  lis<-perfs<-split(permimp,permimp$var)

  res<-list()
  for(i in seq_along(lis)){
    x<-lis[[i]]
    permuted_values<-x[,metric]
    actual_value<-unique(x$actual_metric)
    p_value<-pnorm(actual_value,mean=mean(permuted_values), sd=sd(permuted_values), lower.tail = F)
    p_result<-p_value
    p_result
    xmean<-mean(permuted_values)
    res[[i]]<-data.frame(var=x$var[1],
                         actual_value=actual_value,
                         mean=xmean,
                         sd=sd(permuted_values),
                         dff=actual_value-xmean,
                         p_value=p_result)


  }

  #res<-sapply(lis,function(x){})
  perm_summary<-data.frame(do.call(rbind,res))

  colnames(perm_summary)<-c(
    "var",
    'obs_metric',
    paste0("mean_perm_",metric),
    paste0("sd_perm",metric),
    paste0("obs_metric - ",paste0("mean_perm",metric)),
    "p_value")

  perm_summary

}
perm_add_sig<-function(perm_summary,sig,sig_value){
  perm_summary$sig<-""

  sigs<-which(perm_summary$p_value<sig_value)
  if(length(sigs)>0){
    perm_summary$sig[sigs]<-"*"
  }
  perm_summary
}


permutation_importance<-list()
permutation_importance$ui<-function(id){
  ns<-NS(id)


  div(
    strong("Permutation importance", tipify_ui(actionLink(ns("feaimp_help"),icon("fas fa-question-circle")),"Click for more details", "right")),
    div(style="display: flex",class="inline_pickers well3 color_box",
        div(style="width: 20px;heigth: 20px;background:#c3cc74ff;display:inline-block;margin-top: -5px;margin-left: -5px"),
        numericInput(ns("feat_rep"),span("+ Iterations:",tipright("Number of iterations for randomizing each variable")), value=99, step=1,width='200px'
        ),
        numericInput(ns("feat_seed"),span("+ Seed:",tipright("seed for genereating reproducible results")), value=1, step=1,width='180px'),

        div(id=ns('run_btn'),actionButton(ns("run_feat_shuf"),"RUN"),class="save_changes")

    ),

    tabsetPanel(
      id=ns("pi_result"),
      tabPanel(
        "4.1. Feature Importance",value="tab1",
        column(
          4,class="mp0",
          box_caret(
            ns("18"),
            title="Options",
            color="#c3cc74ff",
            div(
              numericInput(ns("feat_npic"),span("+ nvars:",tipright("Number of variables to display")), value=20,  step=1),
              div(
                id=ns("feat_wrap_inputs"),

                uiOutput(ns('feat_class')),
                numericInput(ns("feat_wrap_nrow"),"nrows",
                             value=2, step=1),


              ),


              numericInput(ns("sig_feature"),span("+ Sig",tipright("Significance level")), value=0.05,  step=0.05),
              pickerInput_fromtop_live(inputId = ns("pal_feature"),
                                       label = "+ Palette",
                                       choices =    NULL,
                                       options=shinyWidgets::pickerOptions(windowPadding="top")),
              textInput(ns("feat_title"),"+ Title",NULL),

              numericInput(ns("cex.title.panel"),"+ Title size",
                           value=12, step=1),
              numericInput(ns("feaimp_cex.axes"),"+ Axes size",
                           value=13, step=1),
              numericInput(ns("feaimp_cex.lab"),"+ Label size",
                           value=14, step=1)



            )


          )

        ),
        column(7,class="mp0",
               box_caret(ns("19"),
                         title="Plot",
                         button_title=
                           actionLink(ns('caret_varImp_plot'),'Download',icon("download")),
                         div(
                           uiOutput(ns("feature_importance_plot")),
                           uiOutput(ns("feature_importance_plot_wrap"))
                         )
               )
        )


      ),
      tabPanel(
        "4.2. Confusion Matrix Post-Shuffling",value="tab2",

        div(style="display: flex",
            box_caret(ns('20'),title="Options",
                      color="#c3cc74ff",
                      div(class="inline_pickers",style="color: #05668D",
                          selectizeInput(inputId = ns("var_feature_cm"),label = "+ Variable:",choices =NULL),

                          pickerInput_fromtop_live(inputId = ns("pal_feature_cm"),label = "+ Palette:",choices =NULL,
                                                   options=shinyWidgets::pickerOptions(windowPadding="top")),
                          textInput(ns("title_cm"),"Title", value=NULL)
                      ),

            ),

            column(5,
                   box_caret(ns('21'),title="Plot",click=F,
                             button_title = actionLink(ns('downp_feature_cm'),'Download',icon("download") ),
                             uiOutput(ns("feature_cm_plot"))))
        )

      ),
      tabPanel(
        "4.3. Results",value="tab4",

        column(4,class="mp0",
               box_caret(ns('22'),
                         title="Options",
                         color="#c3cc74ff",
                         numericInput(ns("round_feat"),"round",3)
               )
        ),
        column(7,class="mp0",

               box_caret(ns('23'),
                         title="Table",
                         button_title = actionLink(ns('caret_varImp'),
                                                   'Download',icon("download")),
                         uiOutput(ns('feature_results'))

               ))




      )

    )
  )
}
box21_help_content <- "Confusion Matrix after randomizing the selected variable"



permutation_importance$server<-function(id,vals){
  moduleServer(id,function(input,output,session){


    observeEvent(input$feat_wrap,{
      value="Feature Shuffling Impact"
      value=paste0(value,"- Group ",input$feat_wrap)
      updateTextInput(session,'feat_title',value=value)
    })




    ns<-session$ns
    m<-reactive({
      m<-vals$cur_caret_model
      req(inherits(m,"train"))
      m
    })
    data_x<-reactive({
      attr(m(),"Datalist")
    })
    model_name<-reactive({
      attr(m(),"model_name")
    })
    model_type<-reactive({})
    output$feat_class<-renderUI({
      choices<-m()$levels
      pickerInput_fromtop(ns('feat_wrap'),"Group:",choices=c("All",choices))
    })
    observe({
      shinyjs::toggle('feat_wrap_inputs',
                      condition=m()$modelType=="Classification")    })

    observed_train<-reactive({
      req(m()$modelType=="Classification")
      obs<-getdata_model(m(),"test")
      obs
    })

    observeEvent(observed_train(),{
      req(m()$modelType=="Classification")
      obs<-observed_train()
      updateNumericInput(session,'feat_wrap_nrow',value=nlevels(obs))
    })



    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'pal_feature',choices =vals$colors_img$val,choicesOpt = list(content =vals$colors_img$img))
      updatePickerInput(session,'pal_feature_cm',choices =vals$colors_img$val,choicesOpt = list(content =vals$colors_img$img))
    })


    observeEvent(m(),{
      updateSelectInput(session,'var_feature_cm',choices= colnames(getdata_model(m())))
    })
    box_caret_server("18")
    box_caret_server("19")
    box_caret_server("20")
    box_caret_server("21")
    box_caret_server("22")
    box_caret_server("23")


    observe({
      req(m())
      req(vals$cmodel)
      predtable<-attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$feature_rands
      shinyjs::toggleClass("run_btn",class="save_changes",condition=is.null(predtable))
    })



    observeEvent(input$run_feat_shuf,ignoreInit = T,{
      attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$feature_rands<-NULL
      attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics<-NULL
      attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics_class<-NULL
      newdata<-getdata_model(m())
      obc<-getdata_model(m(),"test")
      rep=input$feat_rep
      seed=input$feat_seed
      attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$perm_summary<-NULL
      predtable<-permutation_importance3(
        m(),rep,
        session=getDefaultReactiveDomain(),
        seed=seed
      )
      attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$feature_rands<-predtable

    })
    #' @export

    observe({

      req(input$feat_wrap)
      req(input$feat_wrap=="All")
      req(input$pal_feature)
      req(data_x())
      req(model_name())
      req(length(m())>0)
      req(input$feat_npic)
      predtable<-df<-attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$feature_rands
      req(predtable)
      validate(need(length(df)>0,"Analyses pending"))
      req(is.null(attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics))
      permimp<-permimp_metric(
        predtable,metric=metric,  m(),newdata,obc,
        session=getDefaultReactiveDomain(),
        class=F
      )

      attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics<-permimp
    })


    observe({
      req(m()$modelType=="Classification")
      req(input$feat_wrap)
      req(input$feat_wrap!="All")
      req(input$pal_feature)
      req(data_x())
      req(model_name())
      req(length(m())>0)
      req(input$feat_npic)
      predtable<-df<-attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$feature_rands
      req(predtable)
      validate(need(length(df)>0,"Analyses pending"))
      req(is.null(attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics_class))
      permimp<-permimp_metric(
        predtable,metric=metric,  m(),newdata,obc,
        session=getDefaultReactiveDomain(),
        class=T
      )

      attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics_class<-permimp
    })

    get_sig_feature<-reactive({
      if(input$feat_wrap=="All"){
        permimp<-attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics
        req(permimp)
        perm_summary<-sig_feature(permimp,m())
        req(perm_summary)

      } else{

        pmc<-attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics_class
        pmc_obs<-split(pmc,pmc$obs)
        perm_summary_obs<-lapply(pmc_obs,function(x){
          perm_summary_obs<-sig_feature(x,m())
          perm_summary_obs
        })
        perm_summary<-do.call(rbind,perm_summary_obs)
        req(perm_summary)

      }
      perm_summary

    })
    get_summary_permute_importance<-reactive({
      perm_summary<-get_sig_feature()
      perm_sigs<-perm_add_sig(perm_summary,sig_value=input$sig_feature) #input$sig_feature
      vals$caret_varImp<-perm_sigs
      perm_sigs
    })

    get_permimp_metrics<-reactive({
      if(input$feat_wrap=="All"){
        attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics
      } else{
        attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$permimp_metrics_class
      }
    })
    feature_data<-reactive({
      permimp<-get_permimp_metrics()
      req(permimp)
      perm_summary<-get_summary_permute_importance()

      sig_names<-perm_summary$var[perm_summary$sig=="*"]
      df<-get_feature_data_plot(permimp,m(), npic=input$feat_npic)
      req(input$pal_feature)

      req(nrow(df)>1)
      df$sig<-"non_sig"
      df$sig[    df$var%in%sig_names    ]<-"sig"
      df$var[ which( df$var%in%sig_names)]<-paste0("*",df$var[  which( df$var%in%sig_names)])
      df
    })
    feature_data_list<-reactive({
      df<-feature_data()
      split(df,df$obs)
    })
    output$feature_importance_plot<-renderUI({

      req(input$feat_wrap=="All")


      df<-feature_data()

      req(length(df)>0)
      renderPlot({
        df<-df
        col<-vals$newcolhabs[[input$pal_feature]](2)
        p<-ggplot(df, aes(reorder(var,Metric), Metric)) +
          geom_boxplot(aes(fill=sig))+coord_flip()+labs(x="Variables", y='Performance decay')+scale_fill_manual(values=col)+theme(
            axis.text=element_text(size=input$feaimp_cex.axes),
            plot.title=element_text(size=input$cex.title.panel)


          )+ggtitle(input$feat_title)





        vals$caret_varImp_plot<-p
        vals$caret_varImp_plot

      })
    })


    output$feature_importance_plot_wrap<-renderUI({
      req(input$feat_wrap!="All")
      df<-feature_data_list()
      #saveRDS(df,"df.rds")
      col<-vals$newcolhabs[[input$pal_feature]](2)


      df<-df[[input$feat_wrap]]
      req(length(df)>0)
      renderPlot({
        df<-df

        p<-ggplot(df, aes(reorder(var,Metric), Metric)) +
          geom_boxplot(aes(fill=sig))+coord_flip()+labs(x="Variables", y='Performance decay')+scale_fill_manual(values=col)+theme(
            axis.text=element_text(size=input$feaimp_cex.axes),
            plot.title=element_text(size=input$cex.title.panel)


          )+ggtitle(input$feat_title)



        vals$caret_varImp_plot<-p
        vals$caret_varImp_plot

      })
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

    observeEvent(ignoreInit = T,input$caret_varImp_plot,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=vals$caret_varImp_plot
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Permutation Importance plot",datalist_name=datalist_name,name_c="perm_Imp_plot")
    })

    observeEvent(ignoreInit = T,input$downp_feature_cm,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      generic<-vals$feature_cm_plot
      callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Permutation Importance plot",datalist_name=datalist_name,name_c="CM-post-shuff",file=input$var_feature_cm)
    })


    observeEvent(ignoreInit = T,input$caret_varImp,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"perm_imp_results"
      data<-get_summary_permute_importance()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Permutation Importance Results",data=data, name=name)
    })




    observeEvent(ignoreInit = T,input$downp_feature_cm,{
      vals$hand_plot<-"Confusion Matrix Post-Shuffling"
      module_ui_figs("downfigs")
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals, file=input$var_feature_cm)
    })


    observeEvent(ignoreInit = T,input$var_feature_cm,{
      vals$var_feature_cm<-input$var_feature_cm
    })


    output$feature_cm_plot<-renderUI({
      uiOutput(ns("feature_cm_plot"))
      req(length(m())>0)
      predtable<-attr(vals$saved_data[[data_x()]],vals$cmodel)[[model_name()]]$feature_rands
      req(predtable)
      dft<-get_cm_impact(predtable, var=input$var_feature_cm)

      renderPlot({
        vals$feature_cm_plot<-plotCM(dft, palette=input$pal_feature_cm,vals$newcolhabs, font_color="black", title=input$title_cm, round_cm=3)
        vals$feature_cm_plot
      })
    })
    getmetric<-reactive({
      metric<-ifelse(m()$modelType=='Regression','Rsquared','Accuracy')
      metric
    })

    output$feature_results<-renderUI({
      table<-get_summary_permute_importance()
      req(table)
      table[1:5]<-round( table[1:5],input$round_feat)
      div(class="half-drop-inline",
          inline(fixed_dt(
            table,pageLength = 20,dom="ltp",
            round=NULL,scrollY = "200px",
            extensions = c("FixedHeader"))))

    })


    output$side_feature_axes_plot<-renderUI({
      div(
        div("+ Axes size",inline(numericInput(ns("feaimp_cex.axes"),NULL, value=13, width="75px", step=1))),
        div("+ Label size",inline(numericInput(ns("feaimp_cex.lab"),NULL, value=14, width="75px", step=1))),
        div("+ Label size",inline(numericInput(ns("feaimp_cex.leg"),NULL, value=13, width="75px", step=1))),


      )
    })





    ##
  })
}
caret_pairs<-list()
caret_pairs$ui<-function(id){

  ns<-NS(id)

  div(
    column(4,class='mp0',
           box_caret(ns("msp_pairs_a"),
                     title="Analysis options",
                     color="#c3cc74ff",
                     div(
                       div(id=ns('gg_run_btn'),class="save_changes",align="right",actionButton(ns('gg_run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                       pickerInput_fromtop(ns("ggpair.variables"),span("Variables:",class='text_alert'),NULL, multiple = T,options=shinyWidgets::pickerOptions(actionsBox  = TRUE,windowPadding="top",liveSearch = T)),


                       pickerInput_fromtop(ns("ggpair.method"),"Correlation method:",c("none","pearson", "kendall", "spearman"))

                     )
           ),
           box_caret(ns("msp_pairs_b"),
                     title="Plot options",
                     color="#c3cc74ff",
                     div(

                       textInput(ns("ggpair.title"),"Title:",""),
                       pickerInput_fromtop_live(inputId = ns("fm_palette"),
                                                label = 'Palette',
                                                choices =  NULL,
                                                options=shinyWidgets::pickerOptions(windowPadding="top")),
                       numericInput(ns("msp_plot_width"), "Plot width",800),
                       numericInput(ns("msp_plot_height"), "Plot height",600),
                       numericInput(ns("msp_plot_base_size"),"Base size",12),
                       numericInput(ns("ggpair.varnames.size"),"Variable name size:",1.4),
                       numericInput(ns("ggpair.plot.title.size"),"Title size:",1),
                       textInput(ns("ggpair.xlab"),"xlab:",""),
                       textInput(ns("ggpair.ylab"),"ylab:",""),
                       numericInput(ns("ggpair.axis.text.size"),"Axis text size:",1),
                       numericInput(ns("ggpair.axis.title.size"),"Axis label size:",1),
                       textInput(ns("ggpair.title_corr"),"Title_corr:",NULL),
                       numericInput(ns("ggpair.cor.size"),"Corr size:",1.4),
                       numericInput(ns("ggpair.legend.text.size"),"legend.text.size:",1),
                       numericInput(ns("ggpair.legend.title.size"),"legend.title.size:",1),
                       numericInput(ns("ggpair.points.size"),"Points size",1),
                       #numericInput(ns("ggpair.plot.subtitle.size"),"plot.subtitle.size",bs),
                       numericInput(ns("ggpair.alpha.curve"),"Curve transparency:",.8),

                       numericInput(ns("ggpair.round"),"Digits:",3)


                     ))

    ),
    column(8,class='mp0',
           box_caret(ns('msp_pairs_c'),
                     title="Plot",
                     button_title=actionLink(ns('down_ggpair'),'Download',icon('download')),
                     uiOutput(ns('msp_pairs')))
    )
  )




}
caret_pairs$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){

    m<-model
    req(m)
    choices<-colnames(getdata_model(model))
    updatePickerInput(session,"ggpair.variables",choices=choices,selected=choices[1:3],options=shinyWidgets::pickerOptions(liveSearch=T))
    data<-getdata_model(model)
    box_caret_server('msp_pairs_a')
    box_caret_server('msp_pairs_b')
    box_caret_server('msp_pairs_c')

    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'fm_palette',
                        choices =     vals$colors_img$val,
                        choicesOpt = list(content =vals$colors_img$img))
    })

    observeEvent(input$ggpair.method,{
      updateNumericInput(session,'ggpair.title_corr',value=paste(input$ggpair.method,"corr"))
    })

    get_ggpair<-reactiveVal()

    observeEvent(input$gg_run,ignoreInit = T,{
      validate(need(length(input$ggpair.variables)>1,"Requires at least two variables"))
      args<-get_ggpair_args()
      #saveRDS(args,"args.rfds")

      # dettach(args)
      p<-do.call(gg_pairplot,args)
      get_ggpair(p)

    })


    get_ggpair_args<-reactive({


      req(input$ggpair.variables)
      req(input$fm_palette)
      m<-model
      req(class(m)[1]=="train")
      data<-data[,input$ggpair.variables]
      y<-pred<-data.frame(pred=predict(m))
      ncolors<-length(m$levels)
      if(m$modelType=="Classification"){
        cols<-vals$newcolhabs[[input$fm_palette]](ncolors)
      } else{
        cols<-vals$newcolhabs[["turbo"]](length(unique(getdata_model(m,"test"))))
      }


      #my_cols<-cols[pred$pred]
      colnames(y)<-colnames(pred)<-attr(m,"supervisor")


      df=cbind(data,pred)
      size=input$msp_plot_base_size*.09

      req(input$ggpair.method)
      if(input$ggpair.method=="none"){
        upper="blank"
      } else{
        upper="cor"
      }
      args<-list(x=data,y=y,
                 cols=cols,
                 method=input$ggpair.method,
                 round=input$ggpair.round,
                 switch="x",
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
                 title_corr=input$ggpair.title_corr

      )

      req(   !any(sapply(args,length)<1))
      # attach(args)
      args

    })

    observeEvent(input$down_ggpair,{

      vals$hand_plot<-"generic_ggmatrix"
      module_ui_figs("downfigs")
      generic=get_ggpair()
      name_c<-"ggpair"
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="GGpair",datalist_name=datalist_name,name_c=name_c)



    })



    output$msp_pairs<-renderUI({
      validate(need(length(input$ggpair.variables)>1,"Requires at least two variables"))
      res<-div(
        renderPlot(get_ggpair(),  width=input$msp_plot_width,height=input$msp_plot_height)
      )
      res
    })
    return(NULL)

  })
}
pd<-list()
pd$ui<-function(id){
  ns<-NS(id)
  div(
    column(4,class="mp0",

           box_caret(ns("41_a"),
                     title="Analysis Options",
                     color="#c3cc74ff",
                     tip=tipright("Plot partial dependence of two variables(i.e., marginal effects)"),
                     div(
                       div(class="radio_search radio-btn-green",
                           radioGroupButtons(ns("rf_useinter"), span(tipright("<li> <code> Custom Variables</code>: custom defining x and y ;</li> <li> <code> Interaction Frame </code>: only available for Random Forest after running Interactions (randomFlorestExplainer). Allow use a list ranked by most frequent interactions; </li>"),"Use:"), choices=c("Interaction Frame","Custom Variables"))),

                       pickerInput_fromtop_live(ns("rf_interaction"), "Interaction:", choices=NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),
                       pickerInput_fromtop_live(ns("rf_grid_var1"), "Variable 1", choices=NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),
                       pickerInput_fromtop_live(ns("rf_grid_var2"), "Variable 2", choices=NULL,options=shinyWidgets::pickerOptions(liveSearch=T)),

                       pickerInput_fromtop(ns("rf_biplotclass"), "+ Class", choices=NULL,multiple = T,
                                           options=shinyWidgets::pickerOptions(windowPadding="top")),


                       numericInput(ns('rfbi_grid'),span(tipright("Integer giving the number of equally spaced points to use for the continuous variables"),'Grid resolution'),8),
                     )),
           box_caret(ns("41_b"),
                     title="Plot Options",
                     color="#c3cc74ff",
                     div(

                       div(
                         pickerInput_fromtop_live(inputId = ns("pd_palette"),
                                                  label = "Palette:",
                                                  choices =NULL,
                                                  options=shinyWidgets::pickerOptions(windowPadding="top")),
                         textInput(ns("title_pd"),"Title:", value=NULL),
                         numericInput(ns('title_pd_size'),
                                      "Title size:",12),

                         textInput(ns("legend_pd"),"Legend:", value=NULL),
                         textInput(ns("xlab_pd"),"X label:", value=NULL),
                         textInput(ns("ylab_pd"),"Y label:", value=NULL),

                         numericInput(ns("pd_size_plot"),"Size:", value=12),
                         numericInput(ns('axis.size'),
                                      "Axis size:",1),
                         numericInput(ns('axis.text.size'),
                                      "Axis label size:",1))

                     )
           )
    ),
    column(8,class="mp0",
           box_caret(ns("42_a"),
                     title="Biplot",
                     button_title=actionLink(ns('down_plot_pd'),"Download",icon("download")),
                     div(
                       div(id=ns('run_pd_btn'),class="save_changes",

                           actionButton(ns('run_pd'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                       plotOutput(ns("pd_plot"))
                     )
           ),
           box_caret(ns("42_b"),
                     title="Table",
                     button_title=actionLink(ns('down_table_pd'),"Download",icon("download")),
                     div(
                       div(
                         style="position: absolute; top: 27px;right: 0px;padding: 5px;background:white;padding-top:0px",
                         tipify_ui(
                           actionLink(ns('create_pd'),span("Create Datalist"),icon("fas fa-file-signature")),
                           "Create a datalist with the pd results","right"
                         )
                       ),

                       uiOutput(ns("pd_table"))
                     )
           ))

  )

}
pd$server<-function(id,model,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    m<-model

    observe({
      value<- if(m$modelType=="Regression"){"Value"} else{"Prob"}
      updateTextInput(session,"legend_pd",value=value)
    })

    box_caret_server('41_a')
    box_caret_server('41_b')
    box_caret_server('42_a')
    box_caret_server('42_b')
    data_x<-attr(model,"Datalist")
    model_name<-attr(model,"model_name")
    interframe<-reactive({
      validate(need(inherits(model,"train"),"No trained  models found"))
      attr(attr(vals$saved_data[[data_x]],"rf")[[model_name]][[1]],"interframe")})


    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,"pd_palette",  choices =vals$colors_img$val,
                        choicesOpt = list(content =vals$colors_img$img),
                        options=shinyWidgets::pickerOptions(windowPadding="top"))
    })



    updatePickerInput(session,'rf_grid_var1',choices=colnames(getdata_model(model)))
    updatePickerInput(session,'rf_grid_var2',choices=colnames(getdata_model(model)),selected=colnames(getdata_model(model))[2])
    updatePickerInput(session,'rf_biplotclass',choices=model$levels,selected=model$levels[1])




    observe({
      validate(need(inherits(model,"train"),"No trained  models found"))
      if(is.null(interframe())){
        updateRadioGroupButtons(session,'rf_useinter',choices="Custom Variables")
      } else{
        updateRadioGroupButtons(session,'rf_useinter',choices=c("Interaction Frame","Custom Variables"))
      }
    })


    observeEvent(input$create_pd,ignoreInit = T,{
      pd0<-data<-get_pd()
      data_x<-attr(model,"Datalist")
      req(data_x)
      data<-data_migrate(vals$saved_data[[data_x]],data)
      attr(data,"coords")<-NULL
      attr(data,"factors")<-data.frame(id=factor(rownames(pd0)))

      bag<-attr(m,'model_name')
      if(is.null(bag)){
        bag<-attr(m,"model_tag")
      }
      bag<-paste0(bag,"_pd_results")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,"bag")<-bag
      vals$newdatalist<-data
      module_save_changes$ui(ns("explainer-create"), vals)
    })
    module_save_changes$server("explainer-create",vals,
                               "update_tab_caret",'tab2',
                               'update_tab_results',"t5")
    observe({
      req(input$rf_useinter)
      shinyjs::toggle('rf_interaction',condition=input$rf_useinter=='Interaction Frame')
      shinyjs::toggle('rf_biplotclass',condition=model$modelType=='Classification')


    })

    observe({
      shinyjs::toggle('rf_grid_var1',condition=input$rf_useinter!='Interaction Frame')
      shinyjs::toggle('rf_grid_var2',condition=input$rf_useinter!='Interaction Frame')
    })

    observeEvent(interframe(),{
      frame<-interframe()
      pic<-which(frame[,1]==frame[,2])
      choices<-frame$interaction[-pic]
      updatePickerInput(session,'rf_interaction',choices=choices)
    })
    observeEvent(picvars_pd(),{
      updateTextInput(session,'xlab_pd',value=picvars_pd()$var1)
      updateTextInput(session,'ylab_pd',value=picvars_pd()$var2)
    })

    picvars_pd<-reactive({
      req(input$rf_useinter)
      if(input$rf_useinter=='Interaction Frame'){
        req(input$rf_interaction)
        pic<-which(interframe()$interaction==input$rf_interaction)
        var1<-interframe()$variable[pic]
        var2<-as.character(interframe()$root_variable[pic])
      }else{
        var1<-input$rf_grid_var1
        var2<-input$rf_grid_var2
      }
      list(var1=var1,var2=var2)
    })

    fun_pd<-function(picvars_pd, model,rfbi_grid,rf_biplotclass=NULL){
      m<-model
      req(m)
      var1=picvars_pd$var1
      var2=picvars_pd$var2
      if(model$modelType=="Classification"){
        req(rf_biplotclass)
        pd<-pdp::partial(m, pred.var = c(var1,var2), type="classification",which.class=rf_biplotclass,
                         grid.resolution=rfbi_grid,

                         prob=T,
                         chull=T)

      } else {
        pd<-pdp::partial(m, pred.var = c(var1,var2), type="regression",grid.resolution=rfbi_grid,
                         chull=T)

      }
      pd
    }



    get_pd_df<-reactive({
      if(inherits(get_pd(),"list")){
        do.call(rbind,grups_pds(get_pd(),model))
      } else{
        get_pd()
      }
    })



    get_pd<-reactiveVal()
    observeEvent(input$run_pd,ignoreInit = T,{
      req(model)

      if(model$modelType=="Classification"){
        if(length(input$rf_biplotclass)>1){
          res<-lapply(input$rf_biplotclass,function(x) fun_pd(picvars_pd(), model,input$rfbi_grid,x))
          get_pd(res)
          return()
        }
      }
      res<-fun_pd(picvars_pd(), model,input$rfbi_grid,input$rf_biplotclass)
      get_pd(res)
      shinyjs::removeClass("run_pd_btn","save_changes")

    })

    observeEvent(list(
      input$rf_useinter,
      input$rf_interaction,
      input$ rf_grid_var1,
      input$rf_grid_var2,
      input$rf_biplotclass,
      input$rfbi_grid
    ),{
      shinyjs::addClass("run_pd_btn","save_changes")
      get_pd(NULL)
    })



    observeEvent(input$down_table_pd,ignoreInit = T,{

      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"pd_results"
      data<-get_pd_df()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Partial Dependence Results",data=data, name=name)

    })
    output$pd_table<-renderUI({
      div(class="half-drop-inline",style="overflow-x: auto",
          fixed_dt(
            get_pd_df(),pageLength = 20,dom="ltp",
            round=NULL,scrollY = "200px",scrollX = "300px",
            extensions = c("FixedHeader"))
      )
    })
    pd_plot<-function(model,pd,rf_biplotclass,pd_palette,legend_pd,pd_size_plot,ylab_pd,xlab_pd,axis.size,title_pd,axis.text.size,title_size=12){
      req(pd)
      m<-model
      req(m)
      if(model$modelType=="Classification"){
        if(length(rf_biplotclass)>1){

          p1<-plot_pd_groups(pd,model,NULL,newcolhabs = vals$newcolhabs, pd_palette =pd_palette )

        } else{
          p1<-autoplot(pd, contour = TRUE, main = paste("Class:",rf_biplotclass),legend.title = legend_pd)
        }
      } else {
        p1<-autoplot(pd, contour = TRUE, main = "",legend.title = "Partial\ndependence")}
      suppressWarnings({

        p1<-p1 + scale_fill_gradientn(colours =scale_color_2(pd_palette,vals$newcolhabs),name=legend_pd)


      })



      p1<-p1+theme_bw(base_size = pd_size_plot)
      p1<-p1+ ggtitle(title_pd)
      p1<-p1+labs(y = ylab_pd, x=xlab_pd)
      p1<-p1+
        theme(axis.text=element_text(size=axis.size*10),
              plot.title=element_text(size=title_size),


              axis.title=element_text(size=axis.text.size*10)
        )

      p1


    }

    observeEvent(model_name,{
      updateTextInput(session,'title_pd',value="model_name")
    })
    pd_args<-reactive({
      list(
        model=model,
        pd=get_pd(),
        rf_biplotclass=input$rf_biplotclass,
        pd_palette=input$pd_palette,
        legend_pd=input$legend_pd,
        pd_size_plot=input$pd_size_plot,
        title_pd=input$title_pd,
        title_size=input$title_pd_size,
        ylab_pd=input$ylab_pd,
        xlab_pd=input$xlab_pd,
        axis.size=input$axis.size,
        axis.text.size=input$axis.text.size

      )
    })

    pd_biplot<-reactive({
      args<-pd_args()

      do.call(pd_plot,args)
    })
    output$pd_plot<-renderPlot({
      validate(need(inherits(model,"train"),"No trained  models found"))
      pd_biplot()
    })
    observeEvent(input$down_plot_pd,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      name_c="partial_dependence"
      generic=pd_biplot()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Partial Dependendence Plot",  name_c=name_c)
    })

  })
}
msp_nb<-list()
msp_nb$ui<-function(id,model){
  req(inherits(model$finalModel,"NaiveBayes"))
  ns<-NS(id)

  div(
    column(4,class='mp0',
           box_caret(ns("msp_nb_a"),
                     title="Anaysis options",
                     color="#c3cc74ff",
                     div(
                       div(id=ns('run_btn'),class="save_changes",
                           align="right",
                           style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                       pickerInput_fromtop(ns("msp_nb_vars"),"+ Select the variable",colnames(getdata_model(model)),
                                           options=shinyWidgets::pickerOptions(windowPadding="top",liveSearch=T)),

                       checkboxInput(ns('nb_wrap'),"+ Wrap", T),
                       pickerInput_fromtop(ns("nb_type_plot"),"Type:",c("identity","stack","fill"),
                                           options=shinyWidgets::pickerOptions(windowPadding="top"))



                     )
           ),
           box_caret(ns('msp_nb_b'),
                     title="Plot options",
                     color="#c3cc74ff",
                     div(
                       pickerInput_fromtop_live(inputId = ns("palette"),
                                                label = 'Palette',
                                                choices =  NULL,
                                                options=shinyWidgets::pickerOptions(windowPadding="top")),
                       numericInput(ns("base_size"),"Base size",12),
                       numericInput(ns("width"), "Plot width",800),
                       numericInput(ns("height"), "Plot height",600),
                       textInput(ns("title"),"Title:",""),
                       textInput(ns("xlab"),"xlab:",""),
                       textInput(ns("ylab"),"ylab:",""),
                       numericInput(ns("axis.text"),"Axis size:",11),
                       numericInput(ns("axis.title.size"),"Axis title size:",11),

                     )

           )

    ),
    column(8,class='mp0',
           box_caret(
             ns('msp_nb_c'),
             title="Plot",
             button_title = actionLink(ns("download_plot"),
                                       "Download",icon("download")),
             uiOutput(ns("densities_plot"))

           )

    )
  )




}
msp_nb$server<-function(id,model,  vals){
  moduleServer(id,function(input,output,session){

    box_caret_server('msp_nb_a')
    box_caret_server('msp_nb_b')
    box_caret_server('msp_nb_c')
    m<-model

    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,"palette",
                        choices = vals$colors_img$val,
                        choicesOpt = list(
                          content = vals$colors_img$img
                        ),

                        selected=vals$colors_img$val[1])
    })


    msp_nb<-eventReactive(input$run,ignoreInit = T,{
      shinyjs::removeClass("save_changes","run")
      req(input$palette)
      req(class(m$finalModel)=="NaiveBayes")

      nb<-m$finalModel
      req(length(input$msp_nb_vars)>0)
      cols<-vals$newcolhabs[[input$palette]](length(m$levels))
      var=  input$msp_nb_vars

      nb_densities<-  pnb(nb,vars=var)[[1]]


      nb_densities$class<-factor(nb_densities$class)
      nb_densities$var<-factor(nb_densities$var)
      df<-nb_densities
      df$class<-factor(df$class)
      p<- ggplot(df, aes(x=x, y=y, fill=class)) +
        geom_area(position=input$nb_type_plot,alpha=0.6 , size=.5, colour="white")+scale_fill_manual(values=cols)
      if(isTRUE(input$nb_wrap)){
        p<- p  +facet_wrap(~class, scales = "free",ncol=1)+xlab(var)+ylab("Density")
      }
      p<-p+theme(
        panel.grid.major = element_blank(),
        panel.background=element_rect(fill=NA, color="white"),
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid")

      )
      p+
        theme_bw(base_size=input$base_size)+
        ggtitle(input$title)+xlab(input$xlab)+ylab(input$ylab)+
        theme(
          axis.text=element_text(size=input$axis.text),
          axis.title=element_text(size=input$axis.title.size),
        )



    })

    output$densities_plot<-renderUI({
      renderPlot(msp_nb())
    })
    observeEvent(input$download_plot,ignoreInit = T,{

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=msp_nb()
      name_c<-"Densities"
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="NB-Densities",datalist_name=datalist_name,name_c=name_c)


    })



    return(NULL)


  })
}
confusion_module<-list()
confusion_module$ui<-function(id){
  ns<-NS(id)
  div(

    column(4,class="mp0",
           box_caret(ns('24'),
                     title="Options",
                     color="#c3cc74ff",
                     div(
                       pickerInput(inputId = ns("caret_cm_type"),
                                   label = "+ Type: ",
                                   choices = c("Resampling","finalModel")),
                       pickerInput_fromtop_live(inputId = ns("caretpalette"),
                                                label = "+ Palette",NULL,
                                                options=shinyWidgets::pickerOptions(windowPadding="top")),
                       textInput(ns('cm_title'),"Title","Training"),

                       div(
                         shinyBS::popify(actionLink(ns("downtable_cm_train"),span("+ Download",icon("fas fa-download"),icon("fas fa-table")),style  = "button_active"),NULL,"download CM table")
                       )))),
    column(8,class="mp0",
           box_caret(
             ns("24"),
             title="Plot",
             button_title = actionLink(ns("downplot_cm_train"),
                                       "Download",icon("download")),
             div(
               plotOutput(ns("confusion_caret")),
               verbatimTextOutput(ns("confusion_caret2"))
             )
           ))


  )
}
confusion_module$server<-function(id, vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    model<-reactive({vals$cur_caret_model})


    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,"caretpalette",
                        choices = vals$colors_img$val,
                        choicesOpt = list(
                          content = vals$colors_img$img
                        ),

                        selected=vals$colors_img$val[1])
    })
    observeEvent(ignoreInit = T,input$downplot_cm_train,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=vals$caret_cm_train_plot
      name_c<-"cm"
      datalist_name<-attr(model(),"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Confusion Matrix",datalist_name=datalist_name,name_c=name_c)

    })
    observeEvent(ignoreInit = T,input$downtable_cm_train,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-paste(attr(model(),"model_tag"),"_","cm")
      data<-vals$caret_cm_train_table
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Confusion Matrix",data=data, name=name)
    })
    output$confusion_caret2<-renderPrint({
      validate(need(model()$modelType=="Classification","Confusion matrices are only valid for classification models."))
      req(input$caret_cm_type)
      res<-if(input$caret_cm_type=="Resampling"){
        caret::confusionMatrix(model())
      } else{
        caret::confusionMatrix(predict(model()),model()$trainingData[,'.outcome'])
      }
      vals$caret_cm_train_table<-as.data.frame.matrix(res$table)
      res
    })
    output$confusion_caret <- renderPlot({
      req(input$caretpalette)
      validate(need(model()$modelType=="Classification","Confusion matrices are only valid for classification models."))
      m<-model()
      req(input$caret_cm_type)
      if(input$caret_cm_type=="Resampling"){
        cm<-table(m$pred$pred,m$pred$obs)

        res<-plotCM(m, input$caretpalette,  newcolhabs=vals$newcolhabs,title=input$cm_title)
      } else  {
        train<-getdata_model(m)
        cm<-table(predict(model()$finalModel),model()$trainingData[,'.outcome'])
        cm<-cm/sum(cm)*100
        res<-plotCM(cm,input$caretpalette,vals$newcolhabs,title=input$cm_title)

      }
      vals$caret_cm_train_plot<-res
      res
    })
  })
}
confusion_module$server_obspred<-function(id, vals,pred,obs){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    shinyjs::hide("caret_cm_type")

    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,"caretpalette",
                        choices = vals$colors_img$val,
                        choicesOpt = list(
                          content = vals$colors_img$img
                        ),
                        selected=vals$colors_img$val[1])
    })
    observeEvent(ignoreInit = T,input$downplot_cm_train,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=cm_plot()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Confusion Matrix",datalist_name=datalist_name,name_c="cm")

    })
    observeEvent(ignoreInit = T,input$downtable_cm_train,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"cm"
      data<-cmprint()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Confusion Matrix",data=data, name=name)
    })

    cmprint<-reactiveVal()
    output$confusion_caret2<-renderPrint({
      res<-caret::confusionMatrix(table(pred,obs))
      cmprint(as.data.frame.matrix(res$table))
      res
    })

    cm_plot<-reactive({
      cm<-table(pred,obs)
      res<-plotCM(cm,input$caretpalette,vals$newcolhabs)
      res
    })
    output$confusion_caret <- renderPlot({
      cm_plot()
    })
    return(NULL)
  })
}
model_results<-list()
model_results$ui<-function(id){
  ns<-NS(id)
  div(
    tabsetPanel(
      NULL,id=ns("tab2"),selected='t1',
      tabPanel("2.1. Summary",value="t1",

               uiOutput(ns("tab2_1"))
      ),
      tabPanel(
        "2.2. Performace",value="t2",
        div(
          column(6,class="mp0",

                 box_caret(ns('12'),
                           title="Resampling",
                           div(style="overflow-x: auto",
                               uiOutput(ns('global_metrics'))
                           )),
                 box_caret(ns('12f'),
                           title="Final model:",
                           div(style="overflow-x: auto",
                               uiOutput(ns('final_metrics'))
                           )),
                 box_caret(ns('11'),
                           title="Table options",
                           color="#c3cc74ff",
                           div(
                             numericInput(ns("round_performace"),"Round",2),
                             div(
                               tipify_ui(
                                 actionLink(
                                   ns('caret_create_errors_train'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
                                 ),
                                 "Create a datalist with the model training errors", "right"
                               )
                             ),
                             div(
                               tipify_ui(
                                 actionLink(
                                   ns('caret_down_errors_train'),span("+ Download",icon("fas fa-download"),icon("fas fa-table")), style="button_active"
                                 ),
                                 "+ Download Table", "right"
                               )
                             )
                           )
                 )),
          column(6,class="mp0 half-drop-inline",
                 box_caret(ns('13'),
                           title="Observation metrics",
                           div(
                             uiOutput(ns('observation_metrics'))
                           )))
        )
      ),
      tabPanel(
        "2.3. Confusion matrix",value="t3",
        confusion_module$ui(ns("cm_training")),
        uiOutput(ns("cm_training"))

      ),
      tabPanel(
        "2.4. Importance",value="t4",
        tabsetPanel(
          NULL,
          id=ns('importance_tabs'),

          tabPanel(
            '2.4.1. Variable Importance',value="tab1",
            column(12,class='mp0',
                   column(4,class="mp0",
                          box_caret(ns("15"),
                                    title="Options",
                                    color="#c3cc74ff",

                                    div(
                                      div(id=ns('run_feature_btn'),class="save_changes",

                                          align="right",actionButton(ns('run_feature'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                                      div(class="radio_search",
                                          radioGroupButtons(ns("useModel"), span("Use Model:",tipright(paste0("Use a model based technique for measuring variable importance?. TRUE is only available for ", code('rf','gbm','glm','cforest', 'avNNet','nnet','rpart')))), choices = FALSE)
                                      ),
                                      numericInput(ns("nvars"),span("+ nvars:",tipright("Number of variables to display")), value=20,  step=1),
                                      colourpicker::colourInput(ns("feat_palette"),"Palette:","#191970"),

                                      numericInput(ns("feat_size"),'+ size', value=12,  step=1),
                                      numericInput(ns("feat_barhei"),'+ bar height', value=.95,  step=.1),
                                      checkboxInput(ns("stack"),"+ Multi-Importance",F),
                                      checkboxInput(ns("influence"),span("+ Influence Score",tipright("The influence score, reflecting both positive and negative impacts, is calculated by multiplying variable importance by the correlation coefficient between the data and class probabilities.")),F),
                                      uiOutput(ns("xvar_out")),
                                      textInput(ns("title"),"Title", value=NULL),
                                      numericInput(ns("title_size"),"Title size", value=12),

                                      textInput(ns("xlab"),"+ xlab", value=NULL),
                                      textInput(ns('ylab'),"+ ylab", "Variables"),
                                      numericInput(ns("width"), span("Plot width",tipright("in pixels or empty for auto width")),350),
                                      numericInput(ns("height"), span("Plot height",tipright("in pixels or empty for auto height")),NA)

                                    )
                          )),
                   column(
                     8,class='mp0',
                     box_caret(ns("16"),
                               title="Plot",
                               button_title=actionLink(ns('down_plot_feat'),
                                                       "Download",
                                                       icon('download')),
                               div(style="width: 100vh; overflow-x: auto",
                                   div(style="position: absolute; right: 0px; top: 30px",
                                       actionLink(ns("create_from_feature"),"Create Datalist",icon("fas fa-file-signature"))
                                   ),
                                   uiOutput(ns('importance_plot'))
                               )),
                     box_caret(ns('17'),
                               title="Table",
                               button_title=actionLink(ns('down_table_feat'),"Download",icon('download')),
                               uiOutput(ns('importance_table')))
                   )

            )




          ),
          tabPanel("2.4.2. Permutation Importance",value="tab2",
                   permutation_importance$ui(ns("caret")),
                   uiOutput(ns('out_permutation_importance'))
          )


        )


      ),
      tabPanel(
        "2.5. Partial Dependence",
        value='t7',
        pd$ui(ns("partial_dependence")),
        uiOutput(ns('pd_out'))
      ),
      tabPanel(
        "2.6. GGpairs",value="t6",
        caret_pairs$ui(ns("ggpairs")),
        uiOutput(ns('pair_out'))
      )

    )
  )
}
model_results$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    observe({
      vals$update_tab_results<-'t1'
    },autoDestroy = T)
    model<-reactive({
      model<-vals$cur_caret_model
      req(model)
      model
    })



    observeEvent(model()$modelType,{
      req(model()$modelType)
      if(model()$modelType=="Classification"){

      } else{
        updateCheckboxInput(session,"stack",value=F)

      }
    })
    observe({
      shinyjs::toggle("influence",condition=isTRUE(input$stack)&model()$modelType=="Classification")
      shinyjs::toggle("stack",condition=model()$modelType=="Classification")
    })

    output$cm_training<-renderUI({
      confusion_module$server('cm_training', vals)
      NULL
    })


    observeEvent(vals$update_tab_results,{
      req(vals$update_tab_results)
      updateTabsetPanel(session,"tab2",selected=vals$update_tab_results)
      vals$update_tab_results<-NULL
    })









    observeEvent(list(input$stack,input$xvar),{
      if(isFALSE(input$stack)){
        updateNumericInput(session,"width",value=350)
      } else{
        req(input$xvar)
        updateNumericInput(session,"width",value=(300*length(input$xvar)))
      }


    })













    observe({
      req(vals$cmodel)
      if(!is.null(model_varImp[[vals$cmodel]])){
        updateRadioGroupButtons(session,"useModel",choices=c(TRUE,FALSE))
      } else{
        updateRadioGroupButtons(session,"useModel",choices=c(FALSE))
      }
    })



    box_caret_server("12")
    box_caret_server("13")
    observeEvent(input$tab2,ignoreInit = T,{
      vals$cur_model_results<-input$tab2
    })

    box_caret_server('7')
    box_caret_server('8')
    box_caret_server('9')
    box_caret_server('10')

    box_caret_server("15")
    box_caret_server("16")
    box_caret_server("17")

    plot_train<-reactiveVal()

    observeEvent(input$down_plot_train,{

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=plot_train()
      name_c<-"train"
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Training plot",datalist_name=datalist_name,name_c=name_c)

    })

    importance_plot<-reactiveVal()
    observeEvent(ignoreInit = T,input$down_plot_feat,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=importance_plot()
      name_c<-"varImp"
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Variable Importance",datalist_name=datalist_name,name_c=name_c)
    })
    observeEvent(ignoreInit = T,input$down_table_feat,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-paste(attr(model(),"model_tag"),"_","varImp")
      data<-get_imp()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Variable Importance",data=data, name=name)
    })

    rf_impotance2<-function(rf,npic,influence=T,scale=T,positive=T,relative=F,only_sign=T,classes=rf$levels){



      traindata<-getdata_model(rf)
      hc<-getdata_model(rf,"y")
      prob_matrix <-  predict(rf$finalModel,type="prob")
      #colnames(prob_matrix)<-rf$levels

      imp0<-caret::varImp(rf,scale=scale)$importance
      imp0<-imp0[,classes,drop=F]
      prob_matrix<-prob_matrix[,classes,drop=F]
      if(isTRUE(relative))
        imp0<-vegan::decostand(imp0,"total")
      influences<-do.call(rbind,lapply(colnames(traindata),function(var){

        inf=1
        if(isTRUE(influence)){
          inf<-sapply(classes,function(i){
            cor(traindata[,var], prob_matrix[,i], use = "complete.obs")
          } )
          if(isTRUE(only_sign))
            inf<-   sapply(inf,function(x) ifelse(x>0,1,-1))
        }


        imp0[var,]*inf

      }))
      rownames(influences)<-colnames(traindata)
      imp_inf<-data.frame(influences)
      res<-lapply(imp_inf,function(x){
        vec<-x
        if(isFALSE(positive)){
          vec<-sqrt(x^2)
        }

        ord<-order(vec,decreasing=T)[1:npic]
        data.frame(var=rownames(imp_inf)[ord],inf=x[ord])
      })
      names(res)<-classes
      indicators<-do.call(c,lapply(names(res),function(i){
        vec<-rep(i,each=npic)
        names(vec)<-res[[i]]$var
        vec
      }))
      re<-reshape::melt(res)[c(1,3,4)]
      colnames(re)[3]<-"group"
      attr(re,"indicators")<-indicators
      attr(re,"influences")<-influences
      re
    }



    cur_vars_feature<-reactiveVal()
    multimp_importance<-reactive({
      m<-rf<- model()
      rfimp0<-rfimp<-rf_impotance2(rf,positive=!input$influence,scale=T,npic=input$nvars,only_sign=F, classes=input$xvar)
      cur_vars_feature(unique(rfimp0$var))
      rfimp0<-na.omit(rfimp0)
      impdf<-rfimp<-rfimp0
      impdf<-do.call(rbind,lapply(split(impdf,impdf$group),function(x){
        x$var2<-paste0(x$group,"_",x$var)
        x
      }))

      impdf$class2<-factor(reorder(impdf$group,impdf$value),levels=rf$levels)
      impdf$var2<-reorder(impdf$var2,impdf$value)
      vars0<-as.character(impdf$var)
      vars2<-as.character(impdf$var2)
      names(vars0)<-vars2
      fill=input$feat_palette
      p<-ggplot(impdf,aes(x=value,y=var2))+geom_bar(stat="identity",show.legend = F,fill=fill)

      p<-p+facet_wrap(vars(class2),scales="free",ncol=length(m$levels)) +ylab("Variables")+xlab("Influence score")
      p+ scale_y_discrete(labels=impdf$var,breaks=impdf$var2)
    })

    output$xvar_out<-renderUI({
      if(!isTRUE(input$stack)){
        choices<-colnames(get_imp0())
        multiple=F
        selected<-choices[1]
      } else{
        choices<-selected<-model()$levels
        multiple<-T
      }


      pickerInput_fromtop(ns("xvar"),"+ X", choices=choices,selected=selected,multiple =multiple,options=shinyWidgets::pickerOptions(windowPadding="top",liveSearch = T))
    })





    get_imp<-reactive({
      req(input$xvar)
      req(input$xvar%in%colnames(get_imp0()))
      get_imp0()[input$xvar]
    })
    output$model_print<-renderUI({
      validate(need(inherits(model(),"train"),"No trained  models found"))
      render_list(model())
    })
    output$print_inside_m<-renderUI({
      validate(need(inherits(model(),"train"),"No trained  models found"))
      result<-model()[[input$inside_m]]
      render_list(result)

    })
    output$print_inside_final<-renderUI({
      validate(need(inherits(model(),"train"),"No trained  models found"))
      req(input$inside_final)
      if(input$inside_final=="Print"){
        result<-   model()$finalModel
      } else{
        result<-  model()$finalModel[[input$inside_final]]
      }

      render_list(result)

    })

    output$final_metrics<-renderUI({

      m<-model()
      if(inherits(m$finalModel,"randomForest")){
        pred<-rf_oob_pred(m$finalModel,getdata_model(m))
      } else{
        pred<-predict(m)

      }

      div(class="half-drop-inline",
          renderTable(
            data.frame(rbind(caret::postResample(pred,model()$trainingData[,'.outcome'])))
          )
      )
    })
    output$global_metrics<-renderUI({
      validate(need(inherits(model(),"train"),"No trained  models found"))
      req(input$round_performace)
      m<-model()
      #table<- round(m$results[rownames(m$bestTune),],input$round_performace)
      table<-m$results[rownames(m$bestTune),]
      div(
        class="half-drop-inline",
        renderTable(table,digits=input$round_performace)
      )
    })

    output$observation_metrics<-renderUI({
      validate(need(inherits(model(),"train"),"No trained  models found"))
      m<-model()
      table<-accu_rf_class(m)
      vals$caret_down_errors_train<-table
      div(
        inline(fixed_dt(
          table,pageLength = 20,dom="ltp",extensions = c("FixedHeader"))))
    })

    output$pd_out<-renderUI({
      pd$server('partial_dependence',model(),vals)
      NULL
    })
    output$out_permutation_importance<-renderUI({
      permutation_importance$server("caret",vals)
      NULL
    })
    output$explainer_out<-renderUI({
      rf_explainer$server('explain',vals)
      NULL
    })


    observeEvent(vals$cmodel,{
      req(vals$cmodel)
      removeTab('tab2',"rfe")
      req(vals$cmodel)
      if(vals$cmodel=='rf'){
        insertTab('tab2',
                  tabPanel("2.7. randomForestExplainer",
                           value="rfe",
                           div(style="margin-top: -10px",
                               rf_explainer$ui(ns("explain")),
                               uiOutput(ns("explainer_out")
                               ))
                  )
        )

      }


    })

    observeEvent(vals$cur_caret_model,{

      removeTab('tab2',"msp_nb")
      removeTab('tab2',"msp_rf")
      removeTab('tab2',"msp_xyf")
      removeTab('tab2',"msp_dnn")
      removeTab('tab2',"msp_earth")
      removeTab('tab2',"msp_avNNet")
      removeTab('tab2',"msp_nnet")
      removeTab('tab2',"msp_monmlp")
      removeTab('tab2',"msp_rpart")
      removeTab('tab2',"msp_cforest")
      req(length(model()$finalModel)>0)

      if(inherits(model()$finalModel,"NaiveBayes")){
        insertTab('tab2',
                  tabPanel("2.7 Densities",value="msp_nb",
                           msp_nb$ui(ns("nb"),model()),
                           uiOutput(ns("nb_density"))
                  ))
      } else if(inherits(model()$finalModel,"randomForest")){
        insertTab('tab2',
                  tabPanel("2.8 Trees",value="msp_rf",
                           msp_rf$ui(ns("rf"),vals),
                           uiOutput(ns("rf_trees"))
                  ))
      } else if(inherits(model()$finalModel,"kohonen")) {
        insertTab('tab2',
                  tabPanel("2.7 BMU plot",value="msp_xyf",
                           msp_xyf$ui(ns("xyf"),vals),
                           uiOutput(ns("xyf_plot"))
                  ))
      } else if(model()$modelInfo$label=="Stacked AutoEncoder Deep Neural Network") {
        insertTab('tab2',
                  tabPanel("2.7 Network",value="msp_dnn",
                           msp_dnn$ui(ns("dnn"),vals),
                           uiOutput(ns("dnn_plot"))
                  ))
      } else if(inherits(model()$finalModel,"earth")) {
        insertTab('tab2',select=T,
                  tabPanel("2.7 Earth",value="msp_earth",
                           msp_earth$ui(ns("earth"),vals,model()),
                           uiOutput(ns("earth_plot"))
                  ))
      } else if(inherits(model()$finalModel,"avNNet")) {
        insertTab('tab2',
                  tabPanel("2.7 Network",value="msp_avNNet",
                           msp_dnn$ui(ns("avNNet"),vals),
                           uiOutput(ns("avNNet_plot"))
                  ))

      } else if(model()$method%in%c('nnet','pcaNNet','monmlp','mlpML')) {
        insertTab('tab2',
                  tabPanel("2.7 Network",value="msp_nnet",
                           msp_dnn$ui(ns("nnet"),vals),
                           uiOutput(ns("nnet_plot"))
                  ))
      }
      if(model()$method%in%c('monmlp')){
        insertTab('tab2',
                  tabPanel("2.8 Gam.Syte",value="msp_monmlp",
                           msp_monmlp$ui(ns("monmlp"),vals),
                           uiOutput(ns("monmlp_plot"))
                  ))
      } else if(model()$method%in%c('rpart','evtree')) {
        insertTab('tab2',
                  tabPanel("2.7 Tree Plot",value="msp_rpart",
                           msp_rpart$ui(ns("rpart"),vals),
                           uiOutput(ns("rpart_plot"))
                  ))
      } else if(model()$method%in%c("cforest")) {
        insertTab('tab2',
                  tabPanel("2.7 Tree Plot",value="msp_cforest",
                           msp_cforest$ui(ns("cforest"),vals),
                           uiOutput(ns("cforest_plot"))
                  ))
      }



    })

    output$cforest_plot<-renderUI({
      msp_cforest$server("cforest",model(),vals)
      NULL
    })
    output$rpart_plot<-renderUI({
      msp_rpart$server("rpart",model(),vals)
      NULL
    })

    output$monmlp_plot<-renderUI({
      msp_monmlp$server("monmlp",model(),vals)
      NULL
    })

    output$earth_plot<-renderUI({
      msp_earth$server("earth",model(),vals)
      NULL
    })

    output$dnn_plot<-renderUI({
      msp_dnn$server("dnn",model(),vals)
      NULL
    })

    output$avNNet_plot<-renderUI({
      msp_dnn$server_avNNet("avNNet",model(),vals)
      NULL
    })

    output$nnet_plot<-renderUI({
      msp_dnn$server_nnet("nnet",model(),vals)
      NULL
    })

    output$xyf_plot<-renderUI({
      msp_xyf$server("xyf",model(),vals)
      NULL
    })

    output$rf_trees<-renderUI({
      msp_rf$server("rf",model(),vals)
      NULL
    })


    output$nb_density<-renderUI({
      msp_nb$server("nb",model(),vals)
      NULL
    })
    output$pair_out<-renderUI({
      caret_pairs$server('ggpairs',model(),vals)
      NULL
    })


    model_training_metrics<-function(m){
      req(length(m$modelType)>0)
      metric<-if(m$modelType=="Classification"){
        c( "Accuracy","Kappa")

      } else{
        c( "Rsquared","RMSE")
      }
      metric<-data.frame(
        metric=metric,
        value=unlist(m$results[rownames(m$bestTune),metric])
      )
    }
    output$prin_model_args<-renderUI({
      model<-vals$cur_caret_model
      req(model)
      param<-model$finalModel
      # acc<-model$results[rownames(model$bestTune),model$metric]
      metric<-model_training_metrics(model)

      div(
        div(style="font-size: 11px",

            strong_forest("Running time"),
            em(format(attr(model,"run_time")))



        ),
        div(style="display: flex; gap: 3px; font-size: 11px",

            div(strong_forest(paste0("Resampling performace - "))),
            div(style="display: flex; gap: 6px",

                emgray(paste0(metric$metric[1],':')),
                strong(round(metric$value[1],4)),
                emgray(paste0(metric$metric[2],':')),
                strong(round(metric$value[2],4))

            )




        )
      )
    })
    output$tab2_1<-renderUI({
      column(12, class="mp0 inline_pickers",

             #uiOutput(ns('prin_model_wei')),

             uiOutput(ns('prin_model_args')),


             column(6,class="mp0",
                    box_caret(ns('7'),click=F,title="Model Print",
                              button_title = shinyBS::popify(
                                downloadLink(ns("down_caret_results"),label=span(icon("download"),"Download model")),NULL,
                                HTML(paste0(
                                  div(HTML(paste0("Download model as ",em('rds')," file to be read in R as", code('base::readRDS(file.rds)'))))
                                ))
                              ),
                              tip=tipright("Displays the default print of the trained model."),
                              div(id=ns('box7_content'),class="hei300",
                                  uiOutput(ns('model_print'))
                              )),
                    box_caret(ns('8'),click=F,
                              title="Model content",
                              tip=tipright("Displays specific objects within the trained model"),
                              div(
                                id=ns('box8_content'),


                                class="hei300",
                                selectInput(ns("inside_m"),"model$",c(names(model())),selected="results"),
                                uiOutput(ns('print_inside_m'))
                              )
                    )

             ),
             column(6,class="mp0",
                    box_caret(ns('10'),title="Training Plot",
                              button_title=actionLink(ns('down_plot_train'),'Download',icon('image')),
                              div(
                                id=ns('box10_content'),class="hei300",


                                uiOutput(ns('model_plot'))

                              )
                    ),
                    box_caret(ns('9'),click=F,
                              title="finalModel content",
                              tip=tipright("Displays specific objects within the finalModel"),
                              div(
                                id=ns('box9_content'),class="hei300",
                                selectInput(ns("inside_final"),">>",c("Print",names(model()$finalModel))),

                                uiOutput(ns('print_inside_final')))

                    )),





      )
    })
    output$model_plot<-renderUI({
      req(model())
      m<-model()
      res<-m$results
      p<-plot(model())
      plot_train(p)
      div(
        renderPlot({
          p
        },height=280)
      )
    })

    output$importance_table<-renderUI({
      req(importance_plot())
      validate(need(inherits(model(),"train"),"No trained  models found"))
      table<-get_imp()
      div(class="half-drop-inline",
          inline(fixed_dt(
            table,pageLength = 20,dom="ltp",
            round=NULL,scrollY = "200px",
            extensions = c("FixedHeader"))))
    })

    observe({

      shinyjs::toggle('create_from_feature',
                      condition=!is.null(cur_vars_feature())
      )
    })
    observeEvent(input$create_from_feature,ignoreInit = T,{
      m<-model()
      data_x<-attr(m,"Datalist")
      data_o<-data<-vals$saved_data[[data_x]]
      data<-data[cur_vars_feature()]
      data<-data_migrate(data_o,data)

      bag<-attr(m,'model_name')
      if(is.null(bag)){
        bag<-attr(m,"model_tag")
      }
      bag<-paste0(bag,"_top_feature")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,'bag')<-bag
      vals$newdatalist<-data
      module_save_changes$ui(session$ns("feature_create"),vals)
    })

    module_save_changes$server("feature_create",vals)



    observe({
      imp<-get_imp()
      max<-input$nvars
      req(max)
      if(max>nrow(imp)){
        max=nrow(imp)
      }
      plot_hei<-50+(max*20)
      updateNumericInput(session,"height",value=plot_hei)
    })


    observe({
      if(!isTRUE(input$stack)){
        updateTextInput(session,"xlab",value=colnames(get_imp()))}else{
          updateTextInput(session,"xlab",value='Influence Score')
        }
    })

    args_feature<-reactive({
      args<-list(m=model(),
                 useModel=input$useModel,
                 nvars=input$nvars,
                 stack=input$stack,
                 feat_palette=input$feat_palette,
                 feat_barhei=input$feat_barhei,
                 title=input$title,
                 title_size=input$title_size,
                 xlab=input$xlab,
                 ylab=input$ylab,
                 feat_size=input$feat_size,
                 influence=input$influence,
                 xvar=input$xvar,
                 newcolhabs=vals$newcolhabs
      )
      args
    })

    observeEvent(args_feature(),{
      shinyjs::addClass("run_feature_btn","save_changes")
      importance_plot(NULL)
      cur_vars_feature(NULL)
    })




    get_importance_0<-function(m,useModel){

      model_tag<-attr(m,'model_tag')
      if(model_tag=="rf"){
        if(isTRUE(as.logical(useModel))){
          mimp<-randomForest::importance(m$finalModel)
          imp<-data.frame(mimp)
          colnames(imp)<-colnames(mimp)
          imp<-imp[rev(colnames(imp))]
          return(imp)
        }

      }

      imp<-caret::varImp(m,useModel=as.logical(useModel))
      req(m$modelType)
      if(m$modelType=="Classification"){
        mean_imp<-apply(imp$importance, 1, mean)
        imp<-data.frame(mean_importance=mean_imp,imp$importance)
      } else{
        imp<-data.frame(imp$importance)
      }
      imp
    }


    gg_feature<-function(m,useModel=T,nvars=5,stack=F,feat_palette="turbo",newcolhabs=list(turbo=viridis::turbo),feat_barhei=.95,title="",title_size=12,xlab="",ylab="",feat_size=12,influence=F,xvar=m$levels){
      validate(need(inherits(m,"train"),"No trained  models found"))
      imp<-get_importance_0(m,useModel)
      max<-nvars
      req(max)
      req(imp)
      if(max>nrow(imp)){
        max=nrow(imp)
      }
      if(isTRUE(stack)){


        m<-rf<- model()
        rfimp0<-rfimp<-rf_impotance2(rf,positive=!influence,scale=T,npic=nvars,only_sign=F, classes=xvar)
        top_vars<-unique(rfimp0$var)
        rfimp0<-na.omit(rfimp0)
        impdf<-rfimp<-rfimp0
        impdf<-do.call(rbind,lapply(split(impdf,impdf$group),function(x){
          x$var2<-paste0(x$group,"_",x$var)
          x
        }))

        impdf$class2<-factor(reorder(impdf$group,impdf$value),levels=rf$levels)
        impdf$var2<-reorder(impdf$var2,impdf$value)
        vars0<-as.character(impdf$var)
        vars2<-as.character(impdf$var2)
        names(vars0)<-vars2
        fill=feat_palette
        p<-ggplot(impdf,aes(x=value,y=var2))+geom_bar(stat="identity",show.legend = F,fill=fill)

        p<-p+facet_wrap(vars(class2),scales="free",ncol=length(m$levels)) +ylab("Variables")+xlab("Influence score")
        p<-p+ scale_y_discrete(labels=impdf$var,breaks=impdf$var2)

      } else{
        fill=feat_palette
        imp<-imp[order(imp[,1], decreasing=T),,drop=F][1:max,1, drop=F]
        colnames(imp)<-"value"
        imp<-data.frame(var=rownames(imp),imp)
        top_vars<-unique(imp$var)

        p<-ggplot(imp,aes(x=value,y=reorder(var,value)))+geom_bar(stat="identity",fill=fill,width=feat_barhei)+ggtitle(title)


      }


      cex.label_panel<-title_size
      p<-p+xlab(xlab)+ylab(ylab)+theme(
        panel.grid.major = element_blank(),
        panel.background=element_rect(fill=NA, color="white"),
        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
        strip.text.x = element_text(size = title_size),
        axis.line=element_line(),
        axis.text=element_text(size=feat_size),
        axis.title=element_text(size=feat_size),
        plot.title=element_text(size=title_size),
        plot.subtitle=element_text(size=feat_size),
        legend.text=element_text(size=feat_size),
        legend.title=element_text(size=feat_size)

      )


      attr(p,'top_vars')<-top_vars
      p




    }



    get_gg_feature<-eventReactive(input$run_feature,{

      args<-args_feature()
      p<-do.call(gg_feature,args)
      importance_plot(p)
      cur_vars_feature(attr(p,'top_vars'))
      shinyjs::removeClass("run_feature_btn","save_changes")
      p
    })

    get_imp0<-reactive({

      req(model())

      imp<-caret::varImp(model(),useModel=as.logical(input$useModel))
      m<-model()
      req(vals$cmodel)

      if(vals$cmodel=="rf"){
        if(isTRUE(as.logical(input$useModel))){
          mimp<-randomForest::importance(model()$finalModel)
          imp<-data.frame(mimp)
          colnames(imp)<-colnames(mimp)
          imp<-imp[rev(colnames(imp))]
          return(imp)
        }

      }
      req(model()$modelType)

      if(model()$modelType=="Classification"){
        mean_imp<-apply(imp$importance, 1, mean)
        imp<-data.frame(mean_importance=mean_imp,imp$importance)
      } else{

        imp<-data.frame(imp$importance)
      }



      imp
    })
    output$importance_plot<-renderUI({

      p<-get_gg_feature()


      width=input$width
      height=input$height
      if(is.na(width)){
        width="auto"
      }
      if(is.na(height)){
        height="auto"
      }

      renderPlot(p,height =height,width=width)

    })
    output$down_caret_results <-{

      downloadHandler(
        filename = function() {
          paste0(attr(model(),"model_tag"),"_", Sys.Date(),".rds")
        }, content = function(file) {
          saveRDS(model(),file)
        })

    }




    observeEvent(input$caret_create_errors_train,ignoreInit = T,{
      m<-model()
      data<-vals$caret_down_errors_train
      data_x<-attr(m,"Datalist")
      req(data_x)
      data<-data_migrate(vals$saved_data[[data_x]],data)

      bag<-attr(m,'model_name')
      if(is.null(bag)){
        bag<-attr(m,"model_tag")
      }
      bag<-paste0(bag,"_obs_errors")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,"bag")<-bag
      vals$newdatalist<-data

      module_save_changes$ui(ns("caret_errors_train"),vals)


    })

    module_save_changes$server("caret_errors_train",vals,
                               "update_tab_caret",'tab2',
                               'update_tab_results',"t2")
    observeEvent(ignoreInit = T,input$caret_down_errors_train,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-paste(attr(model(),"model_tag"),"_","obs_perform")
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Observation Performace",data=vals$caret_down_errors_train, name=name)
    })




  })
}
model_predic<-list()
model_predic$ui<-function(id){
  ns<-NS(id)





  column(12,class="mp0",
         column(4,class="mp0",
                box_caret(ns("box_predsetup"),
                          color="#c3cc74ff",
                          show_tittle=F,
                          title="Setup",
                          div(
                            div(class="radio_search radio_yellow",
                                #tags$labe(),
                                radioGroupButtons(
                                  ns("svmpred_which"), NULL,
                                  choices = c("Partition"='Partition',
                                              "Training"='Training',
                                              "New Data"="Datalist")
                                )

                            ),

                            div(class="inline_pickers",style=" margin-left: 10px",
                                pickerInput_fromtop_live(ns('predSL_new'),"->",choices=NULL,selected=NULL),



                            ),
                            div(style="position: absolute; top: 0px; right: 0px",
                                id=ns("run_pred_btn"),
                                actionButton(ns("run_pred"),"RUN>>")
                            )
                          )

                ),
                div(id=ns("pred_options"),
                    box_caret(ns("27"),
                              title="Options",
                              color="#c3cc74ff",
                              div(
                                uiOutput(ns('sl_observed')),
                                uiOutput(ns('out_predSL_newY'),

                                ),
                                div(
                                  id=ns("cm_options"),
                                  uiOutput(ns('sl_pred_pal')),
                                  numericInput(ns("round_predictionSL"),"+ Round",3,step=1),
                                  div(id=ns('down_cm_btn'),
                                      div(actionLink(ns("dowcenter_cmsl_pred"),span("+ Download Table",icon("fas fa-table")))),
                                      div(
                                      )
                                  ),


                                ),
                                div(id=ns("metric_options"),
                                    numericInput(ns("round_metric"),"+ Round",3,step=1),)
                              )

                    )
                )

         ),
         column(
           8,
           class='mp0',
           box_caret(
             ns("box_prediction_results"),
             title="Results",
             button_title=div(
               actionLink(ns("down_metrics"),"Download",icon('download')),
               actionLink(ns("down_svm_tab3_1"),"Download",icon('download')),
               actionLink(ns("downp_cmsl_pred"),"Download",icon('download'))
             ),
             button_title2 =radioGroupButtons(
               ns("radio_pred"),
               choiceNames=c("3.1. Metrics",
                             "3.2 Predictions",
                             "3.3. Confusion Matrix"),
               choiceValues=c("tab_metric","tab_pred","tab_cm")
             ) ,
             div(
               tabsetPanel(
                 id=ns("predSL_tab"),
                 type="hidden",
                 tabPanel(
                   "3.1. Metrics",
                   value="tab_metric",
                   uiOutput(ns('predict_metrics'))
                 ),
                 tabPanel("3.2 Predictions",value='tab_pred',
                          uiOutput(ns('svmtab_pred'))
                 ),
                 tabPanel(
                   "3.3. Confusion Matrix",value="tab_cm",

                   uiOutput(ns("confusion_sl_pred")),
                   verbatimTextOutput(ns("confusion_svm2_pred"))

                 )

               ),


               div(
                 div(style="position: absolute; right: 5px;top: 30px",align="right",
                     div(
                       actionLink(ns('svm_create_predictions'),span("Create Datalist"),icon("fas fa-file-signature"))
                     ),
                     div(style="display: flex;gap:3px",
                         id=ns("pred_loop_btn"),
                         uiOutput(ns("tip_pred")),
                         actionLink(ns("pred_loop"),"Predict all",icon("fas fa-file-signature"))
                     )
                 )

               )
             )

           )
         )


  )
}
model_predic$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    model<-reactive(vals$cur_caret_model)

    observe({
      m<-model()
      model_tag<-attr(m,"model_tag")
      shinyjs::toggle('rf_oob',condition=model_tag%in%"rf")

    })

    observeEvent(model(),{
      m<-model()

      updateActionLink(session,"pred_loop",paste("Predict all",attr(m,"model_tag"),"models"))

    })
    output$tip_pred<-renderUI({
      m<-model()
      model_tag<-attr(m,"model_tag")
      tiphelp(paste0(
        "<p>Run predictions for all <code>",model_tag,"</code> models saved in the training Datalist and combine the results into a single Datalist.</p>"
      ))
    })

    observeEvent(input$radio_pred,{

      updateTabsetPanel(session,'predSL_tab',selected=input$radio_pred)
    })



    observe({


      shinyjs::toggle("predSL_new",condition=input$svmpred_which=="Datalist")
      shinyjs::toggle("down_metrics",condition=input$predSL_tab=="tab_metric")
      shinyjs::toggle("down_svm_tab3_1",condition=input$predSL_tab=="tab_pred")
      shinyjs::toggle("svm_create_predictions",condition=input$predSL_tab=="tab_pred")
      shinyjs::toggle("pred_loop_btn",condition=input$predSL_tab=="tab_pred")
      shinyjs::toggle("downp_cmsl_pred",condition=input$predSL_tab=="tab_cm")

    })
    observeEvent(attr(model(),'test'),{
      choices=if(length(attr(model(),'test'))==1){c(
        "Training"='Training',
        "New Data"="Datalist"
      )
      } else{c("Partition"='Partition',
               "Training"='Training',
               "New Data"="Datalist")}
      updateRadioGroupButtons(session,"svmpred_which",choices=choices)

    })



    observe({
      shinyjs::toggle('cm_options',condition=input$predSL_tab=="tab_cm")
      shinyjs::toggle('metric_options',condition=input$predSL_tab=="tab_metric")
      shinyjs::toggle('pred_options',condition=input$predSL_tab!="tab_pred")




    })


    box_caret_server('25')
    box_caret_server('26')
    box_caret_server('27')
    box_caret_server('28')

    observeEvent(ignoreInit = T,input$down_metrics,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-paste(attr(model(),"model_tag"),"_","pred_metrics")
      data<-vals$sup_metrics
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Prediction metrics",data=data, name=name)
    })


    observeEvent(ignoreInit = T,input$dowcenter_cmsl_pred,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-paste(attr(model(),"model_tag"),"_","obs_perform")
      data<-vals$svm_cm_test
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Observation Performace",data=data, name=name)
    })
    observeEvent(ignoreInit = T,input$downp_cmsl_pred,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=vals$svm_cm_pred
      name_c<-"cm_pred"
      datalist_name<-attr(model(),"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Confusion Matrix - Predictions",datalist_name=datalist_name,name_c=name_c)

    })
    output$confusion_svm2_pred<-renderPrint({
      req(model()$modelType=="Classification")
      res<-caret::confusionMatrix(get_cm_pred())
      vals$svm_cm_test<-  as.data.frame.matrix(res$table)
      res
    })
    observe({
      shinyjs::toggle('down_cm_btn',condition=model()$modelType=="Classification")
    })
    get_cm_pred<-reactive({

      obs<-SL_predobs()$obs
      pred<-SL_predobs()$pred
      conf<-table(obs, pred)
      conf
    })
    output$confusion_sl_pred<-renderUI({
      req(model())
      req(model()$modelType=="Classification")

      req(input$svmpalette_pred)
      conf<-get_cm_pred()
      div(

        renderPlot({
          res<-plotCM(conf/sum(conf)*100, input$svmpalette_pred,  newcolhabs=vals$newcolhabs,round_cm=input$round_predictionSL,title=input$svm_cmpred_title)
          vals$svm_cm_pred<-res
          res
        })
      )

    })



    SL_predobs<-reactive({
      req(predictionSL())
      req(length(input$svmpred_which)>0)

      m<-model()
      req(m)
      pred<-predictionSL()[,1]
      # x_t<-attr(m,"test")
      if(input$svmpred_which=="Partition"){
        obs<-attr(m,"sup_test")
      } else if(input$svmpred_which=="Datalist"){
        req(length(input$predSL_newY)>0)
        if(m$modelType=="Classification"){
          sup<-attr(m,"supervisor")
          factors<-attr(vals$saved_data[[input$predSL_newY]],"factors")
          req(sup%in%colnames(factors))
          obs<-factors[,sup]
        } else{
          obs<-vals$saved_data[[input$predSL_newY]][,attr(m,"supervisor")]
        }
      } else if(input$svmpred_which=="Training"){
        obs<-getdata_model(m,"test")
      }

      return(
        list(
          obs=unlist(obs),
          pred=pred
        )
      )
    })
    errors_predictionSL<-reactive({
      obs<-SL_predobs()$obs
      pred<-SL_predobs()$pred

      req(length(obs)==length(pred))

      m<-model()
      NULL

      table<-stats_ml(obs,pred,m)


      table
    })

    output$predict_metrics<-renderUI({
      if(input$svmpred_which=="Datalist"){
        req(input$predSL_newY!="None")
        validate(need(length(getobssvm())>0,paste(
          "Observed Data was not found for the Datalist",input$predSL_new
        )))



      }
      validate(need(inherits(model(),"train"),"No trained  models found"))

      vals$sup_metrics<-table<-errors_predictionSL()
      div(
        div(class="half-drop-inline",
            inline(renderTable(table,digits=input$round_metric, rownames = T))),
        renderPrint(str(predictionSL()))
      )

    })
    output$sl_pred_pal<-renderUI({
      req(model())
      req(model()$modelType=="Classification")
      lab2<-span("+",tipify_ui(icon("fas fa-question-circle"),"Confusion Matrix Palette"),"Palette", "right")

      div(
        textInput(ns("svm_cmpred_title"),"+ CM Title",gettile_cmpred()),
        pickerInput_fromtop_live(inputId = ns("svmpalette_pred"),
                                 label = lab2,
                                 choices =     vals$colors_img$val,
                                 choicesOpt = list(content =     vals$colors_img$img),
                                 options=shinyWidgets::pickerOptions(windowPadding="top"))


      )
    })

    gettile_cmpred<-reactive({
      req(input$svmpred_which)
      switch(input$svmpred_which,
             "Partition"="Test",
             "Datalist"=input$predSL_new,
             "Training"="Training")
    })




    getobssvm<-reactive({
      req(input$predSL_new)
      req(model())
      sup_test<-attr(model(),"supervisor")

      datalist<-vals$saved_data
      if(model()$modelType=="Classification"){
        datalist=lapply(datalist,function(x) attr(x,"factors"))}

      m<-model()
      res0<-
        sapply(datalist, function (x){
          any(colnames(x)==sup_test)&
            all(
              rownames(x)%in%rownames(vals$saved_data[[input$predSL_new]])

            )

        })
      names(which(res0))


    })


    output$sl_observed<-renderUI({

      sup_test<-attr(model(),"supervisor")
      from<-paste0(" ",input$svmpred_which)
      if(input$svmpred_which=="Datalist"){
        req(length(getobssvm())>0)
        from=span(emgreen("Datalist: "),tipright(
          paste0("<p>Data containing <code>",sup_test,"</code> column to be compared with predicted values</p>")
        ))
      }


      lab<-span(tags$label("Observed:"),emgreen(sup_test),tags$label(span("from",from)))
      lab

    })

    output$out_predSL_newY<-renderUI({
      req(input$svmpred_which)
      req(input$svmpred_which=="Datalist")
      req(getobssvm()%in%names(vals$saved_data))
      choices<-c(names(vals$saved_data[getobssvm()]))
      sup_test<-attr(model(),"supervisor")
      div(
        style="margin-left: 70px",
        pickerInput_fromtop_live(ns("predSL_newY"),NULL,choices),

      )



    })


    getnewdatasvm<-reactive({
      req(input$svmpred_which )
      req(input$svmpred_which =='Datalist')

      datalist<-vals$saved_data
      datalist_comp<-lapply(
        vals$saved_data, function (x) {
          colnames(x)<-gsub(" ",".", colnames(x))
          x
        }
      )
      datalist_comp<-vals$saved_data
      m<-model()
      Y<-which(colnames(m$trainingData)==".outcome")
      res0<-unlist(
        lapply(datalist_comp, function (x){
          res<-colnames(x)%in%colnames(m$trainingData[-Y])
          sum(res)==ncol(m$trainingData[-Y])
        })
      )
      names(datalist[res0==T])
    })




    observeEvent(getnewdatasvm(),{
      choices<-names(vals$saved_data[getnewdatasvm()])
      selected=vals$cur_data_sl
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'predSL_new',choices=choices,selected=selected)
    })


    observeEvent(input$svmpred_which ,ignoreInit = T,{
      shinyjs::toggle('newdata_datalist',condition=input$svmpred_which=="Datalist")
    })



    get_new_data<-reactive({

      req(input$svmpred_which)
      m<-model()
      data_o<-vals$saved_data[[vals$cur_data_sl]]
      req(model())
      if(input$svmpred_which=="Partition"){
        newdata<-attr(m,"test")
        newdata<-data_migrate(data_o,newdata)
      } else if(input$svmpred_which=="Datalist"){

        req(input$predSL_new)
        data_o<-newdata<-vals$saved_data[[input$predSL_new]]


      } else if(input$svmpred_which=="Training"){
        newdata<-getdata_model(m)
        newdata<-data_migrate(data_o,newdata)

      }
      model_vars<-colnames(getdata_model(m))
      new_vars<-colnames(newdata)
      req(all(model_vars%in%new_vars))
      newdata<-newdata[,model_vars,drop=F]

      newdata<-data_migrate3.0(data_o,newdata)
      newdata

    })
    data_migrate3.0<-function(data,newdata){


      attr(newdata,"factors")<-attr(data,"factors")
      attr(newdata,"coords")<-attr(data,"coords")
      attr(newdata,"base_shape")<-attr(data,"base_shape")
      attr(newdata,"layer_shape")<-attr(data,"layer_shape")
      attr(newdata,"extra_shape")<-attr(data,"extra_shape")
      newdata
    }
    predictionSL<-reactiveVal()
    observeEvent(input$run_pred,ignoreInit = T,{

      validate(need(!anyNA(get_new_data()),"NAs not allowed in the prediction Datalist"))
      m<-model()

      sl_pred <- predict(m,newdata = get_new_data())


      res<-data.frame(Predictions= sl_pred)


      rownames(res)<-rownames(get_new_data())
      shinyjs::removeClass('run_pred_btn',"save_changes")
      predictionSL(res)
    })

    observeEvent(input$svmpred_which,{
      predictionSL(NULL)

    })


    args_pred<-reactive({
      list(
        input$svmpred_which,
        input$predSL_new,
        input$predSL_newY
      )
    })

    observeEvent(args_pred(),{
      shinyjs::addClass('run_pred_btn',"save_changes")
    })




    output$svmtab_pred<-renderUI({
      validate(need(inherits(model(),"train"),"No trained  models found"))

      table<-vals$svmtab_pred<-predictionSL()
      div(class="half-drop-inline",
          inline(fixed_dt(
            table,pageLength = 20,dom="ltp",
            round=NULL,scrollY = "200px",
            extensions = c("FixedHeader"))))
    })






    output$pred_model_inputs<-renderUI({
      m<-model()

      datalist<-attr(m,"Datalist")
      model_tag<-attr(m,"model_tag")
      models<-attr(vals$saved_data[[datalist]],model_tag)
      choices<-names(models)
      selected=choices
      div(style="display: flex",
          div(class="picker_open",
              virtualPicker(
                id = ns("pred_model_loop"),
                label="Saved Models",
                "Models selected",
                choices=choices,

                selected=selected
              )

          )
      )
    })

    modal_pred_loop<-reactive({
      modalDialog(
        title="Combine prediction for several models",
        uiOutput(ns("pred_model_inputs")),
        div(align="right",
            actionButton(ns("run_loop_pred"),"RUN & Create >>")
        ),
        easyClose = T
      )
    })

    observeEvent(input$run_loop_pred,ignoreInit = T,{
      model_names=input$pred_model_loop
      removeModal()
      try({
        withProgress(min=0,max=length(model_names),message="Creating predictions....",{



          m<-model()
          data_x<-attr(m,"Datalist")



          data<-vals$saved_data[[data_x]]

          newdata= get_new_data()
          req(newdata)

          models<-lapply(model_names,function(name){
            attr(data,"rf")[[name]]$m
          })

          preds_list<-list()


          for(i in seq_along(model_names) ) {
            incProgress(0,message=paste0('Prediction ',i,"/",length(model_names),':',model_names[[i]]))

            m<-models[[i]]
            x<-m$trainingData
            preds<-predict(m,newdata=newdata)

            var=attr(m,"supervisor")
            if(m$modelType=="Classification"){
              preds<-factor(preds,levels=m$levels)
            }
            preds<-data.frame(preds)
            colnames(preds)<-var

            preds_list[[i]]<-preds
            incProgress(1)
          }

          predtable<-data.frame(preds_list)


          rownames(predtable)<-rownames(newdata)
          facs<-names(which(sapply(predtable, is.factor)))
          nums<-names(which(sapply(predtable, is.numeric)))
          if(!length(nums)>0){
            preds_num<-newdata
          } else{
            preds_num<-predtable[nums]
            rownames(predtable)

            preds_num<-data_migrate3.0(data=newdata,newdata=preds_num)
          }

          if(length(facs)>0){
            preds_fac<-predtable[facs]
            attr(preds_num,"factors")<-cbind(attr(preds_num,"factors"),preds_fac)
          }

          data<-preds_num


          bag<-paste0(data_x,"-Predictions")
          newnames<-make.unique(c(names(vals$saved_data),bag))
          bag<-newnames[length(newnames)]
          attr(data,'bag')<-bag
          vals$newdatalist<-data
          module_save_changes$ui(session$ns("caret_predictions_loop"),vals)


        })


      })
    })
    module_save_changes$server("caret_predictions_loop",vals,"update_tab_caret",'tab3')

    observeEvent(input$pred_loop,{
      showModal(
        modal_pred_loop()
      )
    })

    observeEvent(input$svm_create_predictions,ignoreInit = T,{


      req(predictionSL())
      req(input$svmpred_which)
      m<-model()
      data<-predictionSL()
      supervisor<-attr(m,"supervisor")
      model_tag<-attr(m,"model_tag")
      model_name<-attr(m,'model_name')
      if(input$svmpred_which=="Datalist"){
        data_x<- input$predSL_new
        tag<-data_x
      } else{
        data_x<-vals$cur_data_sl
        tag<-"test"
      }
      model_names<-names(attr(vals$saved_data[[vals$cur_data_sl]],model_tag))
      model_num<-which(model_names==model_name)
      bag<-paste0(model_name,"-",tag,"-predictions")

      data_o<-vals$saved_data[[data_x]]
      req(data_x)
      factors<-attr(data_o,"factors")
      factors<-factors[rownames(data),,drop=F]
      if(is.factor(data[,1])){
        factors[paste0("pred_",supervisor)]<-data[,1]
        data<-data_o[rownames(data),]
      }
      data<-data_migrate(data_o,data)
      attr(data,"factors")<-factors
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,'bag')<-bag
      vals$newdatalist<-data
      module_save_changes$ui(session$ns("caret_predictions"),vals)
    })

    module_save_changes$server("caret_predictions",vals,"update_tab_caret",'tab3')



    observeEvent(input$down_svm_tab3_1,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"prediction_results"
      data<-vals$svmtab_pred
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Predictions",data=data, name=name)

    })



  })
}
get_params_unique<-function(x,ns){
  req(nrow(x)>0)
  lapply(1:nrow(x),function(i){
    mg<-x$model_tag[[i]]
    lab<-span(x$name[i],tipright(x$help[i]))
    ui<-switch( x$class[i],
                "logical"=checkboxInput(ns(x$name[i]),lab,as.logical(x$value[i])),
                "numeric"=numericInput(ns(x$name[i]),lab,as.numeric(x$value[i])),
                "char"=selectInput(ns(x$name[i]),lab,
                                   choices=strsplit(x$choices[i],"_br_")[[1]],
                                   selected=gsub("\\'","",x$value[i]))
    )
  })
}
get_params_expand<-function(x,ns){
  req(nrow(x)>0)
  lapply(1:nrow(x),function(i){
    mg<-x$model_tag[[i]]
    lab<-span(x$name[i],tipright(x$help[i]))
    ui<-switch( x$class[i],
                "logical"=selectInput(ns(x$name[i]),
                                      lab,
                                      as.logical(x$value[i]),
                                      multiple = T,
                                      selected=as.logical(x$value[i])),
                "numeric"=textInput(ns(x$name[i]),lab,x$value[i]),
                "char"={
                  selectInput(ns(x$name[i]),lab,
                              choices=strsplit(x$choices[i],"_br_")[[1]],
                              selected=gsub("\\'","",x$value[i]),
                              multiple = T)
                }
    )
  })
}
panel_box_caret3<-list()
panel_box_caret3$ui<-function(id,x,search=NULL){
  ns<-NS(id)
  #get_params_unique(x,ns)
}
panel_box_caret1<-list()
panel_box_caret1$ui<-function(id){
  ns<-NS(id)
  choices<-c("grid","random","custom-grid")
  div(
    div(class="radio_search",
        radioGroupButtons(ns("search"), span("Search:",tipright("Tuning search")), choices = choices)
    ),
    div(
      numericInput(ns("tuneLength"), span("tuneLength:",tipright("The maximum number of mtry- combinations that will be generated")), value = 5)
    )

  )


}
panel_box_caret1$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns


    observeEvent(input$search,{
      vals$cur_caret_search<-   input$search
    })

    observe({
      shinyjs::toggle("tuneLength",condition=input$search!="custom-grid")

    })

    observeEvent(vals$cmodel,{
      req(vals$cmodel)
      choices<-c("grid","random","custom-grid")
      if(vals$cmodel=="glm"){
        choices=choices[1:2]
      }
      selected<-get_selected_from_choices(vals$cur_caret_search,choices)
      updateRadioGroupButtons(session,"search",choices=choices,selected=selected)

    })


    result<-reactive({
      list(search=input$search,
           tuneLength=input$tuneLength)
    })

    observeEvent(result(),{
      vals$box_caret1_args<-result()
    })
    return(NULL)

  })
}
caret_models<-list()
box_title<-function(...){


  div(...,class="box_title")
}

fs<-list()
fs$ui <- function(id){
  lab_resamling<-span(
    class="tip_html150",
    strong("Method:"),
    tipright(
      HTML(paste0(
        tags$p(class="title",
               "Resampling Methods:"),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "repeatedcv:"
          ),
          'Repeated K-fold cross validation'
        ))),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "boot:"
          ),
          'Bootstrap for cross validation'
        ))),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "LOOCV:"
          ),
          'Leave one out cross validation'
        ))),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "LGOCV:"
          ),
          'Leave group out cross validation'
        ))),
        tags$li(HTML(paste(
          tags$strong(
            class="topic",
            "adaptive_cv:"
          ),
          'Adaptative cross validation'
        )))

      )
      ))
  )
  ns<-NS(id)
  div(class='mp0',style="overflow: auto; height: 100vh",

      column(4,
             class="mp0",
             box_caret(
               ns('box_control1'),
               color="#c3cc74ff",
               title="Model Parameters",
               div(


                 div(id=ns('run_btn'),class="save_changes",align="right",
                     style="",actionButton(ns('run'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                 numericInput(ns("ntree"), span("ntree",tipright("Number of Trees")), value = 50),
                 numericInput(ns("iters"),span("iters",tipright("number of search iterations")),1),
                 div(

                   numericInput(ns("popSize"),span("popSize",tipright("number of subsets evaluated at each iteration")),50),
                   numericInput(ns("pcrossover"),span("pcrossover",tipright("the crossover probability")),0.8),
                   numericInput(ns("pmutation"),span("pmutation",tipright("the mutation probability")),0.1),
                   numericInput(ns("elite"),span("elite",tipright("the number of best subsets to survive at each generation")),0),

                 ),
               )
             ),

             box_caret(
               ns("box_control"),
               color="#e6af83ff",
               title="Resampling",
               div(

                 selectInput(ns("method"), "Method",c("cv","repeatedcv","boot")),
                 numericInput(ns("cv"), uiOutput(ns("cv_label")), value = 5),
                 numericInput(ns("repeats"),span("Repeat",tipright("the number of complete sets of folds to compute")), value = 1),

                 numericInput(ns("seed"), span("Seed",tipright(textseed())), value = NA)
               )

             )),
      column(8,class="mp0",
             uiOutput(ns("training_failed")),

             box_caret(ns("box_a"),
                       title="Results",
                       button_title=tipify_ui(
                         actionLink(ns('create_dl'),span("Create Datalist"),icon("fas fa-file-signature")),
                         "Create a datalist with the variables selected by rfGA"
                       ),

                       uiOutput(ns("result"))),
             div(class='mp0',column(6,class="mp0",
                                    box_caret(ns("a"),
                                              button_title = actionLink(ns("download_plot1"),"Download",icon("download")),
                                              title="Performace plot",
                                              uiOutput(ns('plot1')))),
                 column(6,class="mp0",
                        box_caret(ns("b"),
                                  button_title = actionLink(ns("download_plot2"),"Download",icon("download")),
                                  title="Fit plot",
                                  uiOutput(ns('plot2')))))
      )



  )

}
fs$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    box_caret_server("box_control1")
    box_caret_server("box_control")
    box_caret_server("box_a")
    box_caret_server("a")
    box_caret_server("b")


    output$cv_label<-renderUI({
      req(input$method)
      if(input$method=="boot"){
        span("Number",tipright("number of resampling iterations"))
      } else{
        span("CV folds",tipright("number of folds"))
      }

    })
    observe(shinyjs::toggle("repeats",condition=input$method=="repeatedcv"))

    args_GA<-reactive({
      x<-vals$cur_xtrain
      y<-vals$cur_var_y[,1]
      repeats = ifelse(grepl("[d_]cv$", input$method), input$repeats, NA)

      list(
        x = x, y = y,
        iters = input$iters,
        gafsControl = caret::gafsControl(functions = caret::rfGA,
                                         method = input$method,
                                         number = input$cv,
                                         repeats = repeats,
                                         verbose=T,
                                         genParallel=T
        ),
        popSize =input$popSize ,
        pcrossover =input$pcrossover,
        pmutation =input$pmutation,
        verbose=T,
        ntree=input$ntree

      )
    })
    observeEvent(args_GA(),{
      vals$args_GA<-args_GA()
    })

    observeEvent(input$run,ignoreInit = T,{

      rf_ga <-vals$model_error<-try(withProgress(do.call('gafs',vals$args_GA),min=NA,max=NA,message="Running"))
      req(!inherits(rf_ga,"try-error"))

      req(vals$cur_data_sl)
      if(is.null(attr(vals$saved_data[[vals$cur_data_sl]],"rfGA"))){
        attr(vals$saved_data[[vals$cur_data_sl]],"rfGA")<-list()
      }
      attr(vals$saved_data[[vals$cur_data_sl]],"rfGA")[["new model"]]<-list(m=rf_ga)
      vals$cur_model_name<-"new model"
      rf_ga


    })

    output$training_failed<-renderUI({
      req(inherits(vals$model_error,"try-error"))
      div(
        class="alert alert-danger",role="alert",
        style="overflow: auto; max-height: 150px",
        icon("triangle-exclamation"),strong('Training failed'),"error message:",
        renderPrint({cat(
          as.character(vals$model_error)
        )})
      )
    })

    observeEvent(list(vals$trainSL_args,
                      vals$cmodel,
                      input$ntree,
                      input$iters,
                      input$popSize,
                      input$pcrossover,
                      input$pmutation,
                      input$elite,
                      input$method,
                      input$cv,
                      input$repeats
    ),ignoreInit = T,{
      req(vals$cmodel)
      vals$model_error<-NULL
    })

    output$results_print<-renderUI({
      req(input$results)
      if(input$results=="Print"){
        return(renderPrint(run_ga()))
      }
      renderPrint(run_ga()[[input$results]])
    })
    output$plot1<-renderUI({
      req(run_ga())
      renderPlot(plot(run_ga()) + theme_bw(),height=300)
    })
    plot2<-reactiveVal()
    output$plot2<-renderUI({
      req(run_ga())

      renderPlot({
        plot(run_ga()$fit)
        plot2(recordPlot())
        plot2()
      },height=300)
    })
    output$result<-renderUI({
      div(
        selectInput(session$ns("results"),"Results",choices=
                      c("Print",names(run_ga()))),
        uiOutput(session$ns("results_print"))

      )

    })
    observeEvent(ignoreInit = T,input$download_plot1,{

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=plot(run_ga()) + theme_bw()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Fit plot - rfGA",datalist_name=datalist_name,name_c="fitplot_rfGA")
    })
    observeEvent(ignoreInit = T,input$download_plot2,{

      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=plot2()

      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="perfplot-rfGA",datalist_name=datalist_name,name_c="rfGA_plot1")
    })
    observeEvent(input$create_dl,ignoreInit = T,{
      opt<-run_ga()$optVariables

      newdata=vals$saved_data[[vals$cur_data_sl]][opt]
      data_x<-vals$saved_data[[vals$cur_data_sl]]
      data<-data_migrate(data_x,newdata)
      bag<-paste0(vals$cur_data_sl,"_GAsel")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,"bag")<-bag
      vals$newdatalist<-data

      module_save_changes$ui(session$ns("ga-create"),vals)

    })
    module_save_changes$server("ga-create",vals)
    run_ga<-reactive({
      req(vals$cur_data_sl%in%names(vals$saved_data))
      res<-attr(vals$saved_data[[vals$cur_data_sl]],"rfGA")[[vals$cur_model_name]]$m
      req(res)
      res
    })



  })
}
caret_models$ui<-function(id){
  ns<-NS(id)
  div(
    h4("Supervised Algorithms", class="imesc_title"),




    div(class="model_setup",
        box_caret(ns('0'),inline=F,
                  title="Model setup",
                  div(
                    caret_train$ui(ns("caret_train"))

                  ),
        )),
    uiOutput(ns("caret_train_out"))
  )

}
caret_models$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns



    observeEvent(names(vals$saved_data),{
      for(k in 1:length(vals$saved_data)){
        model_names<-names(attr(vals$saved_data[[k]],"rf"))
        if(length(model_names)>0)
          for(i in 1:length(model_names)){
            m<-attr(vals$saved_data[[k]],"rf")[[i]]$m
            attr(m,"Datalist")<-names(vals$saved_data)[k]

            attr(vals$saved_data[[k]],"rf")[[i]]$m<-m
          }
        if(length(model_names)>0)
          for(i in 1:length(model_names)){
            m<-attr(vals$saved_data[[k]],"rf")[[i]]$m
            attr(m,"model_name")<-model_names[i]

            attr(vals$saved_data[[k]],"rf")[[i]]$m<-m
          }


      }
    })
    output$print_train<-renderUI({

      req(vals$cur_xtrain)
      req(vals$cur_ytrain)

      div(

        div(style="display: flex; font-size: 11px; margin-top: 20px",
            div(style="margin-left: 10px; margin-top: 15px",
                div(strong('x:')),
                div(strong('y:'))),
            div(style="margin-left: 10px",
                div(strong("Training:")),
                div(emgreen(paste0(dim(vals$cur_xtrain),collapse=" x "))),
                div(emgreen(paste0(dim(vals$cur_ytrain),collapse=" x ")))),
            if(dim(vals$cur_xtest)[1]>0)
              div(style="margin-left: 10px",
                  div(strong("Test:")),
                  div(emgreen(paste0(dim(vals$cur_xtest),collapse=" x "))),
                  div(emgreen(paste0(dim(vals$cur_ytest),collapse=" x "))))

        )
      )
    })


    get_grid<-reactive({
      req(!vals$cmodel%in%'rfGA')
      req(vals$cur_caret_search)
      x=vals$cur_xtrain
      y=vals$cur_var_y
      model=vals$cmodel
      req(x)
      req(y)
      req(model)

      if(vals$cur_caret_search=="custom-grid"){
        req(vals$box_caret2_args)
        param<-expand.grid(vals$box_caret2_args)
      } else{
        req(vals$box_caret1_args$tuneLength)

        req(nrow(x)>0)
        req(ncol(x)>0)
        req(nrow(y)>0)
        req(ncol(y)>0)

        res<-run_gridcaret(x,y,model, len=vals$box_caret1_args$tuneLength)
        param<-res$param
      }
      param
    })
    get_wei<-reactive({
      req(input$wei_datalist)
      data<-vals$saved_data[[input$wei_datalist]]
      req(input$wei_var%in%colnames(data))
      data[,input$wei_var]
    })
    accept_wei<-reactive({
      req(vals$cmodel)
      tags<-model_tags[[vals$cmodel]]
      pc<-grep('Accepts',tags)
      length(pc)>0
    })
    to_install<-reactive({
      req(vals$cmodel)
      reqs<- sapply(model_library[[vals$cmodel]],is_installed)
      if(any(reqs=="FALSE")){
        names(reqs[reqs=="FALSE"])
      } else{
        NULL
      }
    })
    user_defined_args<-reactive({
      req(vals$box_caret2_args)
      expand.grid(vals$box_caret2_args)
    })
    output$training_failed<-renderUI({
      req(inherits(vals$model_error,"try-error"))
      div(class="alert alert-danger",role="alert",
          style="overflow: auto; max-height: 150px",
          icon("triangle-exclamation"),strong('Training failed'),"error message:",
          renderPrint({cat(
            as.character(vals$model_error)
          )})
      )
    })
    output$training_failed0<-renderUI({
      req(inherits(vals$model_error0,"try-error"))
      req(is.null(vals$model_error))
      div(class="alert alert-danger",role="alert",
          style="overflow: auto; max-height: 150px",
          icon("triangle-exclamation"),strong('Training failed:'),
          renderPrint({cat(
            as.character(vals$model_error0)
          )})
      )
    })
    output$caret_tabs<-renderUI({

    })

    output$print_interactions<-renderUI({
      param<-get_grid()

      div(div(
        class="inline_pickers ",
        tipright('Final number of interactions excluding resampling:'),strong("Nº Interactions:"),
        strong(nrow(param))

      ))
    })
    output$print_grid<-renderUI({

      param<-get_grid()

      div(class="half-drop-inline",style="max-height: 150px;max-width: 250px; overflow: auto",
          render_list(param,F)
      )

    })
    output$panel_green<-renderUI({

      req(vals$cmodel)
      req(!vals$cmodel%in%'rfGA')
      lib<-model_library[[vals$cmodel]][1]

      div(
        p(tiphelp("Model name and tag"),strong("Model:"),em(model_labels[[vals$cmodel]]),strong_forest(paste0("(",vals$cmodel,")"))),
        if(length(lib)>0)
          if(lib!="NULL")
            p(tiphelp("R package"),strong('Library:'),style="white-space: normal;",
              strong_forest(paste0(lib,collapse=", ")),
              tipify_ui(actionLink(ns("model_help"),icon('fas fa-question-circle')),"Access the R documentation of the model","right"),

            ),
        if(vals$cmodel!='glm')
          p(style="display: flex",class="mp0",
            tiphelp(paste0("Parameters included in the tuning grid search:</p> argument name (e.g. mtry), the type of data in the parameter grid (e.g. numeric, character) and textual labels for the parameter.")),
            strong("Tunning:"),
            renderTable(model_parameters[[vals$cmodel]],colnames =F)
          ),
        p(tiphelp("If variable importance metrics is available for the model"),strong("Variable Importance:"),
          strong_forest(length(model_varImp[[vals$cmodel]])>0)),
        p(tipify_ui(icon('tag'),"Subjects associated with the model"),strong('tags:'),style="white-space: normal;",
          emforest(paste0(model_tags[[vals$cmodel]],collapse=", ")))

      )
    })
    output$panel_box_caret3<-renderUI({
      div(
        model_control$ui(ns("arg3"),vals),
        uiOutput(ns("box3_output"))
      )
    })
    output$panel_wei<-renderUI({
      choices<-names(vals$saved_data)
      choices<-choices[!choices%in%vals$cur_data_sl]
      selected<-get_selected_from_choices(vals$cur_caret_dl,choices)

      div(

        div(style="display: flex",
            hidden(pickerInput_fromtop(ns("wei_datalist"),"Datalist",choices,selected=selected, options=shinyWidgets::pickerOptions(liveSearch =T))),
            hidden(pickerInput_fromtop(ns("wei_var"),label=NULL,NULL, options=shinyWidgets::pickerOptions(liveSearch =T))))
      )
    })
    output$validate_twoclass<-renderUI({
      req(vals$cmodel)
      tags<-model_tags[[vals$cmodel]]
      pc<-grep('Two Class',tags)
      vals$two_class<-T
      if(length(pc)>0)
        if(!is.null(vals$cur_var_y))
          if(is.factor(vals$cur_var_y[,1]))
            if(nlevels(vals$cur_var_y[,1])>2){
              vals$two_class<-F
              column(12,class = "alert_warning",
                     "This model is not avaliable for modelling variables with more than two levels"
              )
            }

    })
    output$prin_model_fit<-renderUI({
      req(vals$cmodel)
      renderPrint(
        model_fits[[vals$cmodel]]
      )
    })
    output$formals<-renderUI({
      renderPrint(formals_from_sring(train_functions[[input$sel_help_model]]))
    })
    output$model_help_page<-renderText({
      getHelp(do.call(utils::help,train_functions2[[input$sel_help_model]]))
    })
    output$pkg_installed<-renderUI({

      missing_pkgs<-to_install()
      req(missing_pkgs)
      req(missing_pkgs!="NULL")
      div(
        id=ns('pkg_install'),
        style="display: flex",
        div(
          p("The selected model requires the following packages:"),
          renderPrint(missing_pkgs)
        ),
        div(style="padding-top: 35px; margin-left: 20px",
            actionButton(ns("install_pkg"),"Install")
        )
      )
    })
    observe({
      shinyjs::toggle("panel_ga",condition=vals$cmodel%in%'rfGA')
    })
    observe({

      shinyjs::toggle("panel_train",condition=!vals$cmodel%in%'rfGA')
      shinyjs::toggle("tab",condition=!vals$cmodel%in%'rfGA')



    })
    observe({
      box_caret_server('0')
      box_caret_server('1')
      box_caret_server('2')
      box_caret_server('3')
      box_caret_server('4')
      box_caret_server('5')
      box_caret_server('6')
      box_caret_server('6b')

    })
    observe({
      shinyjs::toggle("grid_box",condition = vals$cur_caret_search=="custom-grid")
    })
    observe({
      shinyjs::toggle("panel_train",condition = input$tab=="tab1")
    })
    observe({
      shinyjs::toggle("use_wei",condition=accept_wei())
    })
    observe({
      req(is.null(to_install()))
      req(vals$cmodel)
      req(is_installed(model_library[[vals$cmodel]]))
      req(model_library[[vals$cmodel]])
      libs<-model_library[[vals$cmodel]]
      sapply(libs,function(x) library(x,character.only=T))
      message("attached:",libs)

    })
    observe({
      req(vals$cmodel)
      tags<-model_tags[[vals$cmodel]]
      pc<-grep('Ensemble',tags)
      shinyjs::toggle("ntree",condition=length(pc)>0)
    })
    observe({
      condition=is.null(vals$two_class)|isTRUE(vals$two_class)
      shinyjs::toggle('run_train',condition=condition)
    })
    observe({
      condition1<-length(to_install())>0
      condition2<-to_install()!='NULL'
      condition<-all(c(condition1,condition2))
      shinyjs::toggle("pkg_installed",condition=condition)
    })
    observe({
      condition=is.null(to_install())|to_install()=="NULL"
      shinyjs::toggle("run_train",condition=condition)
    })
    observe({vals$cur_caret_tab<-input$tab})
    observeEvent(input$fine_tunning,{
      shinyjs::toggle("fine_params")
    })
    observeEvent(input$wei_datalist,{
      vals$cur_caret_dl<-input$wei_var
      choices<-colnames(vals$saved_data[[input$wei_datalist]])
      selected<-get_selected_from_choices(vals$cur_caret_wei,choices)
      updatePickerInput(session,"wei_var",choices=choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(input$wei_var,{
      vals$cur_caret_wei<-input$wei_var
    })
    observeEvent(input$model_help,{
      selected<-vals$cmodel
      if(grepl("knn",selected)){
        selected='knn_class'
      }
      showModal(
        modalDialog(
          easyClose = T,
          div(class="help_page",
              h4(model_library[[vals$cmodel]]),
              pickerInput_fromtop(ns("sel_help_model"),"Model",
                                  choices =names(train_functions2),
                                  selected=selected,

                                  choicesOpt =list(subtext =model_labels3),
                                  options=shinyWidgets::pickerOptions(windowPadding="top", options=shinyWidgets::pickerOptions(liveSearch =T))
              ),
              div(style="white-space: normal",

                  htmlOutput(ns("model_help_page"))
              )
          )

        )
      )
    })
    observeEvent(input$sel_help_model,{
      selected<-input$sel_help_model
      if(grepl("knn",selected)){
        selected='knn'
      }
      vals$cmodel<-selected
    })
    observeEvent(input$install_pkg,{
      missing_pkgs<-to_install()
      withProgress(min=0,max=length(missing_pkgs),message="Intalling packages",{
        lapply(missing_pkgs, function(x){
          install_packages(x)
          incProgress(1)
          library(x,character.only=T)
        }
        )
      })
      shinyjs::hide('pkg_installed')

    })
    observeEvent(vals$update_tab_caret,{
      req(vals$update_tab_caret)
      updateTabsetPanel(session,"tab",selected=vals$update_tab_caret)
      vals$update_tab_caret<-NULL
    })
    observe({
      shinyjs::toggle('wei_datalist',condition=isTRUE(input$wei_use))
      shinyjs::toggle('wei_var',condition=isTRUE(input$wei_use))
    })
    observeEvent(input$run_train,ignoreInit = T,{
      vals$model_error0<-try({
        req(vals$cmodel)
        args<-vals$trainSL_args
        args_r<-vals$box_caret4_args
        x<-args$x_train
        y<-args$y_train[,1]
        if(is.null(attr(vals$saved_data[[args$data_x]],vals$cmodel))){
          attr(vals$saved_data[[args$data_x]],vals$cmodel)<-list()
        }
        validate(need(!anyNA(x),"NAs not allowed in X"))
        validate(need(!anyNA(y),"NAs not allowed in Y"))
        seed<-if (!is.na(args_r$seed)) { args_r$seed} else{NULL}
        req(vals$cmodel)
        search=vals$box_caret1_args$search
        req(vals$cur_caret_search)
        if(vals$cur_caret_search=="custom-grid"){
          args_r$search<-"grid"
        } else{
          args_r$search<-vals$cur_caret_search
        }
        trControl_list=list(
          method = args_r$method,
          number = args_r$cv,
          repeats = args_r$repeats,
          p=args_r$pleaves/100,
          savePredictions = "all",
          search =args_r$search,verboseIter=F,
          selectionFunction =args_r$selfinal

        )
        if(args_r$method=="adaptive_cv") {
          trControl_list$adaptive=list(
            min=args_r$adap_min,
            alpha=args_r$adap_alpha,
            method=args_r$adap_method,
            complete=args_r$adap_complete
          )
        }
        trControl=do.call(caret::trainControl,trControl_list)
        method_train<-vals$cmodel
        if(method_train=="gaussprRadial"){
          method_train=SL_models$gaussprRadial
        } else if(method_train=="svmLinear"){
          method_train=SL_models$svmLinear
        } else if(method_train=="svmRadial"){
          method_train=SL_models$svmRadial
        } else if(method_train=="avNNet"){
          method_train=SL_models$avNNet
        }
        args_train<-list(
          as.matrix(x),y,method_train,
          trControl=trControl,
          tuneLength =vals$box_caret1_args$tuneLength
        )
        if(accept_wei()){
          if(length(input$wei_use)>0)
            if(input$wei_use){
              args_train$weights<-get_wei()
            }
        }
        req(vals$cur_caret_search)

        if(vals$cur_caret_search=="custom-grid"){
          args_train$tuneGrid<-user_defined_args()
        }
        box_caret3<-vals$box_caret3_args
        if(vals$cmodel=='rpart'){
          box_caret3<-list(control=do.call(
            rpart.control ,box_caret3))
        }
        if(vals$cmodel=='cforest'){
          box_caret3<-list(controls=do.call(
            cforest_control ,box_caret3))
        }
        args_train<-c(args_train,box_caret3)
        if(vals$cmodel=="rf"){
          args_train$localImp = TRUE
          args_train$keep.forest=TRUE
          args_train$keep.inbag=T
        }
        withProgress(
          message = paste("Running",vals$cmodel),
          min = NA,
          max = NA,
          {
            time0<-Sys.time()

            vals$model_error<-m<-try(suppressWarnings({
              set.seed(seed)

              do.call(caret::train,args_train)}))
            time1<-Sys.time()
            req(!inherits(m,"try-error"))
            attr(m,"test_partition")<-paste("Test data:",args$partition,"::",args$partition_ref)
            attr(m,"Y")<-paste(args$data_y,"::",args$var_y)
            attr(m,"Datalist")<-paste(args$data_x)
            if(args$partition!="None"){
              totest<-data.frame(args$x_test)
              attr(m,'test')<-totest
              attr(m,"sup_test")<-args$y_test
            } else{ attr(m,'test')<-c("None")}
            attr(m,"supervisor")<-args$var_y
            attr(m,"inputs")<-list(
              Ydatalist=args$data_y,
              Y=args$var_y,
              Xdatalist=args$data_x
            )
          }
        )

        #train
        attr(m,"model_tag")<-vals$cmodel
        attr(m,"model_name")<-"new model"
        attr(m,"run_time")<-difftime(time1,time0, units='secs')

        attr(vals$saved_data[[args$data_x]],vals$cmodel)[["new model"]]<-list(m=m)
        vals$cur_model_name<-"new model"
        updateTabsetPanel(session,"tab",selected="tab2")
        updateTabsetPanel(session,"caret_results-tab2",selected="tab1")
        vals$cur_model_results<-"t1"

      })
    })


    observeEvent(vals$train_loop,ignoreInit = T,{
      req(isTRUE(vals$train_loop))
      vals$train_loop<-FALSE
      #m_rsq<-c()
      vals$model_error0<-try({
        var_y_df<-vals$y_loop
        partition_df<-vals$partition_df
        vals$y_loop<-NULL
        args<-vals$trainSL_args
        model_names<-paste0(colnames(var_y_df),'~',args$data_x)
        model_results<-list()
        withProgress(
          message = paste0("Running..."),
          min = 1,
          max = ncol(var_y_df),
          {
            for(i in 1:ncol(var_y_df)) {
              partition=partition_df[[i]]$partition
              partition_ref<- partition_df[[i]]$partition_ref
              train_ids=partition_df[[i]]$train_ids
              test_ids<- partition_df[[i]]$test_ids
              model_name<-model_names[i]
              var_y<-colnames(var_y_df)[i]
              incProgress(1, message=paste(paste0(i,"/",ncol(var_y_df)),"Running",vals$cmodel,"> var-",model_name))
              x0<-vals$cur_datax
              x<-x0[train_ids,]
              y<-var_y_df[train_ids,var_y]
              y_test<-var_y_df[test_ids,var_y,drop=F]
              x_test<-x0[test_ids,]



              args_r<-vals$box_caret4_args


              if(is.null(attr(vals$saved_data[[args$data_x]],vals$cmodel))){
                attr(vals$saved_data[[args$data_x]],vals$cmodel)<-list()
              }
              validate(need(!anyNA(x),"NAs not allowed in X"))
              validate(need(!anyNA(y),"NAs not allowed in Y"))
              seed<-if (!is.na(args_r$seed)) { args_r$seed} else{NULL}
              req(vals$cmodel)
              search=vals$box_caret1_args$search
              req(vals$cur_caret_search)
              if(vals$cur_caret_search=="custom-grid"){
                args_r$search<-"grid"
              } else{
                args_r$search<-vals$cur_caret_search
              }
              trControl_list=list(
                method = args_r$method,
                number = args_r$cv,
                repeats = args_r$repeats,
                p=args_r$pleaves/100,
                savePredictions = "all",
                search =args_r$search,verboseIter=F,
                selectionFunction =args_r$selfinal

              )
              if(args_r$method=="adaptive_cv") {
                trControl_list$adaptive=list(
                  min=args_r$adap_min,
                  alpha=args_r$adap_alpha,
                  method=args_r$adap_method,
                  complete=args_r$adap_complete
                )
              }
              trControl=do.call(caret::trainControl,trControl_list)
              method_train<-vals$cmodel
              if(method_train=="gaussprRadial"){
                method_train=SL_models$gaussprRadial
              } else if(method_train=="svmLinear"){
                method_train=SL_models$svmLinear
              } else if(method_train=="svmRadial"){
                method_train=SL_models$svmRadial
              } else if(method_train=="avNNet"){
                method_train=SL_models$avNNet
              }
              args_train<-list(
                as.matrix(x),y,method_train,
                trControl=trControl,
                tuneLength =vals$box_caret1_args$tuneLength
              )
              if(accept_wei()){
                if(length(input$wei_use)>0)
                  if(input$wei_use){
                    args_train$weights<-get_wei()
                  }
              }
              req(vals$cur_caret_search)
              req(vals$cmodel)
              if(vals$cur_caret_search=="custom-grid"){
                args_train$tuneGrid<-user_defined_args()
              }
              box_caret3<-vals$box_caret3_args
              if(vals$cmodel=='rpart'){
                box_caret3<-list(control=do.call(
                  rpart.control ,box_caret3))
              }
              if(vals$cmodel=='cforest'){
                box_caret3<-list(controls=do.call(
                  cforest_control ,box_caret3))
              }
              args_train<-c(args_train,box_caret3)
              if(vals$cmodel=="rf"){
                args_train$localImp = TRUE
                args_train$keep.forest=TRUE
                args_train$keep.inbag=T
              }



              time0<-Sys.time()

              vals$model_error<-m<-try(suppressWarnings({
                set.seed(seed)
                do.call(train,args_train)}))



              time1<-Sys.time()
              req(!inherits(m,"try-error"))
              attr(m,"test_partition")<-paste("Test data:",partition,"::",partition_ref)
              attr(m,"Y")<-paste(args$data_y,"::",var_y)
              attr(m,"Datalist")<-paste(args$data_x)
              if(partition!="None"){
                attr(m,'test')<-x_test
                attr(m,"sup_test")<-y_test
              } else{ attr(m,'test')<-c("None")}
              attr(m,"supervisor")<-var_y
              attr(m,"inputs")<-list(
                Ydatalist=args$data_y,
                Y=var_y,
                Xdatalist=args$data_x
              )


              #train
              attr(m,"model_tag")<-vals$cmodel
              attr(m,"model_name")<-model_name
              attr(m,"run_time")<-difftime(time1,time0, units='secs')

              model_results[[i]]<-list(m=m)


            }





          }
        )


        attr(vals$saved_data[[args$data_x]],vals$cmodel)[model_names]<-model_results

        vals$cur_model_name<-model_name
        updateTabsetPanel(session,"tab",selected="tab2")
        updateTabsetPanel(session,"caret_results-tab2",selected="tab1")
        vals$cur_model_results<-"t1"

      })

      if(!inherits(vals$model_error0,"try-error")){
        showModal(
          modalDialog(
            easyClose = T,
            size="s",
            title=NULL,
            h4(emgreen("Success!")),
            div(length(model_names), "models were created:"),
            lapply(model_names,function(x)
              tags$li(em(x)))
          )
        )
      }
    })


    observeEvent(list(vals$trainSL_args,
                      vals$box_caret1_args,
                      vals$box_caret4_args,
                      vals$cmodel,
                      vals$box_caret1_args$tuneLength,
                      input$wei_use,
                      get_wei(),
                      user_defined_args(),
                      vals$box_caret3_args),{
                        vals$model_error<-NULL
                        vals$model_error0<-NULL
                      })


    caret_train$server("caret_train",vals=vals)
    output$caret_train_out<-renderUI({

      div(
        div(id=ns('panel_ga'),
            fs$ui(ns("fsga")),
            uiOutput(ns("fsout"))
        ),


        column(12,class="mp0 nav_caret",
               tabsetPanel(NULL,id=ns("tab"),
                           tabPanel("1. Training",value="tab1",
                                    div(style="display: flex;",
                                        div(
                                          uiOutput(ns('pkg_installed')),
                                          uiOutput(ns('validate_twoclass')),
                                          #uiOutput(ns('prin_model_fit')),
                                        ),
                                        uiOutput(ns("tab1_out"))



                                    )),
                           tabPanel("2. Results",value="tab2",

                                    model_results$ui(ns("caret_results")),
                                    uiOutput(ns('tab2_out'))),
                           tabPanel("3. Predict",value="tab3",
                                    div(
                                      model_predic$ui(ns("caret_pred")),
                                      uiOutput(ns('tab3_out'))
                                    ))

               )),

        column(12, class="mp0",id=ns('panel_train'),
               div(class="mp0 train_button",
                   actionButton(ns("run_train"),span("Train",icon("fas fa-angles-right"))),


               ),
               column(4,class="mp0",
                      box_caret(ns('1'),
                                title="Tuning",
                                tip=tipright("<p>Choose the tuning search method using this panel.</p><p>If <code>Grid</code> or <code>Random</code> is selected, the app will create a tuning grid using the <code>grid</code> function assigned to the model within the caret package.</p><p>Select <code>custom-grid</code> to define your own tuning parameters, and IMESc will generate the corresponding combinations.</p>"),
                                div(id=ns('box1_content'),
                                    panel_box_caret1$ui(ns("panel_box_caret1"))
                                )
                      ),
                      div(
                        id=ns('grid_box'),
                        box_caret(ns('2'),
                                  div(id=ns('box2_content'),
                                      panel_box_caret2$ui(ns("grid")),
                                      uiOutput(ns("box2_output"))
                                  ))
                      ),
                      box_caret(ns('3'),
                                title="Model parameters",
                                tip=tipright("Specify model-specific parameters that are not included in the tuning search"),
                                div(id=ns('box3_content'),
                                    uiOutput(ns("panel_box_caret3"))
                                )),

                      box_caret(ns('4'),
                                title="Resampling",
                                tip=tipright("<p>Use this panel to specify resampling parameters, which determine how the data is repeatedly sampled and used for model validation. This includes selecting the resampling method (e.g., cross-validation, bootstrapping) and setting the number of repeats.</p>"),
                                div(id=ns('box4_content'),
                                    panel_box_caret4$ui(ns("panel_box_caret4"))
                                ))
               ),
               column(7,class='mp0',
                      uiOutput(ns("training_failed")),
                      uiOutput(ns("training_failed0")),
                      div(id=ns("use_wei"),
                          box_caret(ns("6"),
                                    title=inline(div(class="check_title",
                                                     checkboxInput(ns("wei_use"),span("Use weights",tipright("Use case weights")),F)
                                    )),
                                    div(style="padding: 10px",
                                        uiOutput(ns('panel_wei')),
                                    )
                          )),
                      box_caret(ns("5"),class="train_box box_deep",
                                title="Summary",
                                tip=tipright("<p>This panel provides a summary of the current selection of the model.</p><p>It serves as a quick reference for the user to review and confirm their chosen settings before running the analysis.</p>"),
                                div(style="padding: 10px",
                                    div(style="position: absolute; top: 0px;right: 0px; padding: 20px",uiOutput(ns('print_train'))),
                                    uiOutput(ns("validate_train")),
                                    uiOutput(ns('panel_green')),
                                )
                      ),

                      box_caret(ns('6b'),
                                title="Current tuning",
                                div(style="padding: 10px;",
                                    uiOutput(ns('print_interactions')),
                                    uiOutput(ns("print_grid"))
                                ))
               )

        )


      )

    })

    output$fsout<-renderUI({
      fs$server("fsga",vals)
      NULL
    })
    output$tab1_out<-renderUI({
      panel_box_caret1$server('panel_box_caret1',vals)
      panel_box_caret2$server("grid",vals)
      model_control$server("arg3",vals)
      panel_box_caret4$server('panel_box_caret4',vals)
      NULL
    })
    output$tab2_out<-renderUI({
      model_results$server("caret_results",vals)
      NULL
    })
    output$tab3_out<-renderUI({
      model_predic$server("caret_pred",vals)
      NULL
    })





    return(NULL)


  })



}
detach_package <- function(pkg, character.only = T)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
detach_package<-function(...){

}
#' @export
caret_train<-list()

#' @export
caret_train$ui<-function(id){
  ns<-NS(id)
  div(
    div(class="inline_pickers",id=ns("model_type_panel"),
        radioButtons(ns('model_type'),"Model type:",choices=c("Classification","Regression"),inline =T)),
    div(
      div(
        style="display: flex;gap: 10px;align-items: flex-start",class="setup_box",
        #div(tags$div("X",class="trailab")),
        div(class="picker-flex picker-before-x",
            uiOutput(ns('data_x')),

        ),
        div(style="display: flex;gap: 10px;",
            div(class="picker-flex",
                caret_model$ui(ns('model'))),
            div(style="display: flex;",id=ns("model_model_name"),
                div(class="picker-flex",
                    uiOutput(ns('model_name'))),
                div(class="save_changes",
                    style="padding-top: 20px",
                    tipify_ui(actionButton(ns("save_model"),icon("fas fa-save",style="color: #374061ff")),"Save the model in the current Datalist X", "right")),
                div(style="padding-top: 20px; margin-left: 10px",
                    tipify_ui(actionButton(ns("trash_model"),icon("trash")),"Click to remove the model","right"))
            )

        )

      ),
      div(style="margin-left: 20px;margin-top: -5px",align="left",shinyWidgets::dropMenu(actionLink(ns("filter_x"),"+ select columns",style="font-size: 11px;font-style: italic "),uiOutput(ns("filter_var"))))
    ),
    div(
      style="display: flex; gap: 10px; align-items: flex-start",class="setup_box",
      id=ns("model_y_panel"),
      div(
        div(class="picker-flex picker-before-y",
            uiOutput(ns('data_y')),

        ),
        div(style="margin-left: 20px;margin-top: -5px",align="left",actionLink(ns("gotrain_loop"),"+ Train multiple models",style="font-size: 11px;font-style: italic "))
      ),

      tags$label("::",style="padding-top: 30px;"),
      div(class="picker-flex",
          uiOutput(ns("var_y"))

      ),
      div(class="picker-flex",
          uiOutput(ns("partition"))

      ),
      div(class="picker-flex",
          uiOutput(ns("partition_ref_out"))
      )

    ),
    div(style="position: absolute; top: 0px;right: 0px; padding: 20px",
        uiOutput(ns('print_train')),
        uiOutput(ns('print_results'))
    )

  )

}

#' @export
caret_train$server<-function(id,vals=NULL){
  moduleServer(id,function(input,output,session){

    ns<-session$ns


    output$loop_part<-renderUI({
      req(input$data_y)
      y<-vals$saved_data[[input$data_y]]
      factors<-attr(y,"factors")
      part_ref<-NULL
      if(input$partition!="None"){
        part_ref<-paste0("[",input$partition_ref,"]")
      }

      label_partition<-paste0(input$partition,part_ref)
      value_partition<-"training"
      names(value_partition)<-label_partition
      yon<-input$yloop
      req(length(yon)>0)
      div(
        div(
          div(class="half-drop-inline",style="margin-top: 30px",
              pickerInput_fromtop(
                ns("yloop_partition"),
                span("Partition:",tiphelp("Use a global partition defined in the Model Setup panel, or specify them individually.")),
                choices=c(value_partition,"Variable-specific"="Custom"),
                selected=vals$cur_yloop_partition
              ),
          ),
          div(
            class="picker_table",
            uiOutput(ns("yloop_part_labels")),
            uiOutput(ns("loop_part_pickers"))
          )

        )
      )
    })




    output$yloop_part_labels<-renderUI({
      req(input$yloop_partition=="Custom")
      div(style="margin-top: -22px",
          column(6,class="mp0",tags$label("Partition")),
          column(6,class="mp0",tags$label("Test reference"))
      )
    })
    observeEvent(input$yloop_partition,{
      vals$cur_yloop_partition<-input$yloop_partition
    })


    output$loop_part_pickers<-renderUI({
      req(input$data_y)
      req(input$yloop_partition=="Custom")
      y<-vals$saved_data[[input$data_y]]
      factors<-attr(y,"factors")
      req(input$model_type)
      if(input$model_type== 'Classification'){y<-factors}
      part_cols<-colnames(factors)[grepl("Partition_",colnames(factors))]
      yon<-input$yloop
      req(length(yon)>0)

      res<-lapply(1:ncol(y), function(i){
        var<-colnames(y)[i]
        part_col<-paste0("Partition_",var)
        selected="None"
        if(input$yloop_partition=="Custom"){
          if(part_col%in%colnames(factors)){
            selected=part_col
          }
        }

        if(var%in%yon){
          column(12,class='mp0',
                 column(6,class='mp0',pickerInput_fromtop(ns(paste0("part_loop_",i)),NULL,choices=c("None",colnames(factors)), selected=selected, options=shinyWidgets::pickerOptions(liveSearch =T,windowPadding="top"))),
                 column(6,class='mp0',uiOutput(ns(paste0("part_loop_test_out",i))))
          )} else{
            column(12,class='mp0',emgray("Not training"),class="picker_fake")
          }
      })
      do.call(div,list(res))
    })
    observe({

      req(input$data_y)
      req(input$yloop_partition=="Custom")
      y<-vals$saved_data[[input$data_y]]
      factors<-attr(y,"factors")
      req(input$model_type)
      if(input$model_type== 'Classification'){y<-factors}
      yon<-input$yloop
      req(length(yon)>0)
      try({
        lapply(1:ncol(y), function(i){
          var<-colnames(y)[i]
          col<-input[[paste0("part_loop_",i)]]

          if(length(col)>0)
            if(col%in%colnames(factors)){
              fac<-factors[,col]
              output[[paste0("part_loop_test_out",i)]]<-renderUI({
                pickerInput_fromtop(ns(paste0("part_loop_test",i)),NULL,choices=levels(fac), options=shinyWidgets::pickerOptions(liveSearch =T,windowPadding="top"))})

            } else{
              output[[paste0("part_loop_test_out",i)]]<-renderUI({ NULL})
            }
        })
      })

    })




    output$loop_vars<-renderUI({
      y<-vals$saved_data[[input$data_y]]
      factors<-attr(y,"factors")
      req(input$model_type)
      if(input$model_type== 'Classification'){y<-factors}

      choices<-colnames(y)
      div(class="picker_open",
          shinyWidgets::virtualSelectInput(
            inputId = ns("yloop"),
            label = "Y:",
            optionHeight='24px',
            choices = choices,
            optionsCount = length(choices),
            search = F,
            keepAlwaysOpen = TRUE,
            multiple =T,
            hideClearButton=T,
            alwaysShowSelectedOptionsCount=T,
            optionsSelectedText="Variables selected",
            optionSelectedText="Variables selected"
          )
      )

    })







    observeEvent(ignoreInit = T,input$gotrain_loop,{
      showModal(
        modalDialog(
          title=div("Train multiple models"),
          easyClose=T,
          column(12,class="mp0 picker_loop",style="height: 340px; overflow-y: auto;",

                 div(id=ns("yloop_page1"),
                     column(6,class="mp0",style="",
                            uiOutput(ns('loop_vars'))),
                     column(6,class="mp0",style="max-width: 300px",

                            uiOutput(ns('loop_part')))
                 ),
                 div(id=ns("yloop_page2"),style="display: none",
                     uiOutput(ns("teste")),
                     uiOutput(ns("yloop_page2_out"))
                 )),
          footer=div(actionButton(ns('yloop_cancel'),"Cancel"),
                     hidden(actionButton(ns('yloop_prev'),"<< Previous")),
                     actionButton(ns('yloop_next'),"Next >>"))

        )
      )

    })
    partition_df<-reactive({
      y<-vals$saved_data[[input$data_y]]
      req(input$model_type)
      factors<-attr(y,"factors")
      train_ids<-rownames(factors)
      test_ids<-rownames(factors)
      if(input$model_type== 'Classification'){y<-factors}
      yon<-input$yloop
      req(length(yon)>0)
      res<-lapply(1:ncol(y), function(i){
        if(input$yloop_partition=="None"){
          partition<-"None"
          partition_ref<-"None"

        } else if(input$yloop_partition=="Custom"){
          partition<-input[[paste0("part_loop_",i)]]
          partition_ref<-input[[paste0("part_loop_test",i)]]
          if(!is.null(partition)){
            if(partition!="None"){
              test<-factors[,partition]==partition_ref
              train_ids<-rownames(factors)[!test]
              test_ids<-rownames(factors)[test]
            }
          }


        } else{
          partition<-input$partition
          partition_ref<-input$partition_ref
          if(partition!="None"){
            test<-factors[,partition]==partition_ref
            train_ids<-rownames(factors)[!test]
            test_ids<-rownames(factors)[test]
          }
        }
        list(partition=partition,partition_ref=partition_ref,train_ids=train_ids,test_ids=test_ids)

      })
      names(res)<-colnames(y)
      vals$partition_df<-res[input$yloop]
      vals$y_loop<-y[input$yloop]
      NULL
    })


    output$teste<-renderUI({
      partition_df()
    })
    output$yloop_page2_out<-renderUI({
      div(
        div(
          div(class = "alert_warning",style="",
              div(strong(icon("triangle-exclamation",style="color: Dark yellow3"),"Warning:"),"Training multiple models can be time-consuming"),
              div(style="padding: 10px; background: white; color: black;border: 5px solid #ffffcc",

                  div(
                    tags$div(icon("caret-right"),"Total number of models:",strong(length(input$yloop), style="color: red")),
                    tags$div(icon("caret-right"),strong("New model names:"),
                             div(
                               style="max-height: 100px; overflow-y: auto; padding-left: 20px; background: #F0F0F0",
                               uiOutput(ns("model_loop_names"))
                             )

                    ),


                    tags$div(icon("caret-right"),"Any model already saved with this name(s) will be replaced. ", em("We suggest using this tool with a Datalist that does not have any saved RF models."))
                  )

              ))

        ),
        div(align="center",
            div(strong("Click to proceed:")),
            actionButton(ns("train_loop"),em(
              "Train the",strong(length(input$yloop),style=
                                   'color: red'),"models"
            )))

      )
    })

    observeEvent(input$yloop_cancel,{
      removeModal()
    })

    yloop_pages<-reactiveVal(1)
    observeEvent(input$gotrain_loop,{
      yloop_pages(1)
    })
    observeEvent(input$yloop_next,{
      req(yloop_pages()<2)
      yloop_pages(yloop_pages()+1)
    })
    observeEvent(input$yloop_prev,{
      req(yloop_pages()==2)
      yloop_pages(yloop_pages()-1)
    })


    observe({
      shinyjs::toggle('yloop_next', condition=length(input$yloop)>0&yloop_pages()==1)
      shinyjs::toggle('yloop_prev', condition=yloop_pages()==2)
      shinyjs::toggle('yloop_cancel', condition=yloop_pages()==1)
      shinyjs::toggle('yloop_page1', condition=yloop_pages()==1)
      shinyjs::toggle('yloop_page2', condition=yloop_pages()==2)
    })

    yloop_vars<-reactive({
      req(input$model_type)
      y<-vals$saved_data[[input$data_y]]
      factors<-attr(y,"factors")
      if(input$model_type== 'Classification'){y<-factors}
      yvars<-y[,input$yloop,drop=F]
      yvars
    })

    yloop_partition<-reactive({
      y<-vals$saved_data[[input$data_y]]
      factors<-attr(y,"factors")

    })


    output$model_loop_names<-renderUI({
      lapply(paste0(input$yloop,'~',input$data_x),function(x) tags$li(em(x)))

    })


    observeEvent(input$model_name,ignoreInit = T,{
      vals$update_tab_results<-vals$cur_model_results
    })

    observeEvent(input$data_x,ignoreInit = T,{
      vals$update_tab_results<-vals$cur_model_results
    })



    observeEvent(input$train_loop,ignoreInit = T,{
      vals$train_loop<-TRUE
    })

    model_type<-reactiveVal()

    caret_model$server('model',vals)
    get_model<-reactive({
      req(vals$cmodel)
      req(input$model_name)
      attr(data_x(),vals$cmodel)[[input$model_name]]$m
    })
    datalist_y<-reactive({
      req(input$data_y%in%names(vals$saved_data))
      data<-vals$saved_data[[input$data_y]]
      data
    })
    output$print_train<-renderUI({
      req(vals$cur_caret_tab)
      req(vals$cur_caret_tab=="tab1")
      req(x_train())
      req(y_train())

      div(

        div(style="display: flex; font-size: 11px; margin-top: 20px",
            div(style="margin-left: 10px; margin-top: 15px",
                div(strong('x:')),
                div(strong('y:'))),
            div(style="margin-left: 10px",
                div(strong("Training:")),
                div(emgreen(paste0(dim(x_train()),collapse=" x "))),
                div(emgreen(paste0(dim(y_train()),collapse=" x ")))),
            if(dim(x_test())[1]>0)
              div(style="margin-left: 10px",
                  div(strong("Test:")),
                  div(emgreen(paste0(dim(x_test()),collapse=" x "))),
                  div(emgreen(paste0(dim(y_test()),collapse=" x "))))

        )
      )
    })

    output$validate_train<-renderUI({
      x<-x_train()
      y<-y_train()[,1]
      validate(need(!anyNA(x),"NAs not allowed in X"))
      validate(need(!anyNA(y),"NAs not allowed in Y"))
    })
    get_partition<-reactive({
      data<-y_factors()
      req(input$partition)
      if(input$partition=="None"){
        return(list(train=rownames(data),test=NULL))
      } else{
        req(input$partition%in%colnames(data))
        part<-data[,input$partition]

        validate(need(!anyNA(part),"Na not allowed"))
        req(any(part%in%input$partition_ref))
        test_ids<-which(part%in%input$partition_ref)
        test=rownames(data)[test_ids]
        train=rownames(data)[-test_ids]
        return(list(train=train,test=test))
      }

    })
    output$filter_var<-renderUI({

      div(class="picker_open",
          shinyWidgets::virtualSelectInput(
            inputId = session$ns("filter"),
            label = "Select the columns",
            optionHeight='24px',
            choices = colnames(data_x()),
            selected=colnames(data_x()),
            search = TRUE,
            keepAlwaysOpen = TRUE,
            multiple =T,
            hideClearButton=T,
            alwaysShowSelectedOptionsCount=T,
            searchPlaceholderText="Select all",
            optionsSelectedText="IDs selected",
            optionSelectedText="IDs selected"
          )
      )


    })
    data_x<-reactive({
      req(input$data_x)
      req(input$data_x%in%names(vals$saved_data))
      data<-vals$saved_data[[input$data_x]]
      data
    })

    observe({
      res<-try({
        data<-data_x()
        filter<-colnames(data)
        if(length(input$filter)>0){
          filter<-input$filter
        }
        req(filter%in%colnames(data))
        data[,filter,drop=F]
      })
      req(!inherits(res,"try-error"))

      vals$cur_datax<-res
    })

    x_train<-reactive({


      data<-data_x()
      filter<-colnames(data)
      if(length(input$filter)>0){
        filter<-input$filter
      }
      data<-data[get_partition()$train,,drop=F]
      req(filter%in%colnames(data))
      data<-data[,filter,drop=F]
      data
    })
    x_test<-reactive({

      data<-data_x()
      filter<-colnames(data)
      if(length(input$filter)>0){
        filter<-input$filter
      }
      data<-data[get_partition()$test,,drop=F]
      req(filter%in%colnames(data))
      data<-data[,filter,drop=F]
      data
    })
    y_train<-reactive({
      req(input$var_y)
      data<-get_data_y()
      req(input$var_y%in%colnames(data))
      data[get_partition()$train,input$var_y,drop=F]
    })
    y_test<-reactive({
      req(input$var_y)
      data<-get_data_y()
      req(input$var_y%in%colnames(data))
      data[get_partition()$test,input$var_y,drop=F]
    })
    y_factors<-reactive({
      data<-datalist_y()
      attr(data,"factors")
    })
    get_data_y<-reactive({
      req(input$model_type)
      if(input$model_type=="Classification"){
        y_factors()
      } else{
        datalist_y()
      }
    })
    available_models<-reactive({

      req(input$data_x!="")

      req(vals$cmodel)
      attr(vals$saved_data[[input$data_x]],vals$cmodel)
    })





    observeEvent(input$model_name,{
      vals$cur_model_name<-input$model_name
    })
    observeEvent(input$filter,{
      vals$cur_filter_model<-input$filter
    })
    observeEvent(input$trash_model,ignoreInit = T,{
      choices<-names(attr(vals$saved_data[[input$data_x]],vals$cmodel))
      ns<-session$ns
      showModal(
        modalDialog(
          easyClose = T,
          title="Remove models",
          div(
            shinyWidgets::virtualSelectInput(
              inputId = ns("trash_picker"),
              label = "Select the columns",
              optionHeight='24px',
              choices = choices,
              search = TRUE,
              keepAlwaysOpen = TRUE,
              multiple =T,
              hideClearButton=T,
              alwaysShowSelectedOptionsCount=T,
              searchPlaceholderText="Select all",
              optionsSelectedText="Models selected",
              optionSelectedText="Models selected"
            ),
            actionButton(ns("trash_confirm"),"Remove Models",icon("trash"))
          ),
          footer=div(modalButton("Close"))
        )
      )
    })

    observeEvent(input$trash_confirm,ignoreInit = T,{
      req(vals$cmodel)
      vals$update_tab_caret<-"tab1"
      attr(vals$saved_data[[input$data_x]],vals$cmodel)[input$trash_picker]<-NULL
      removeModal()
    })



    observeEvent(input$save_model,ignoreInit = T,{

      m<-vals$cur_caret_model
      model_names<-names(attr(vals$saved_data[[input$data_x]],vals$cmodel))



      model_name<-paste0(vals$trainSL_args$var_y,'~',vals$trainSL_args$data_x)

      bag<-paste(vals$cmodel,'-',model_name)
      new_names<-make.unique(c(model_names,bag))
      name0<-new_names[length(new_names)]
      if(length(model_names)==1){
        choices<-"Create"
      } else{
        choices<-c("Create","Replace")
      }

      model_names<-model_names[-which(model_names=="new model")]

      showModal(
        modalDialog(
          title=paste("Save",vals$cmodel,"model"),

          footer=div(actionButton(ns("data_confirm"),strong("confirm")),
                     modalButton("Cancel")),
          div(style="padding: 20px",

              div(class="half-drop",
                  style="display: flex",
                  div(style="width: 20%",
                      radioButtons(ns("create_replace"),NULL,choices)
                  ),
                  div(style="padding-top: 10px",
                      textInput(ns("newdatalit"),NULL,name0),
                      hidden(selectInput(ns("overdatalist"),NULL,model_names))
                  )
              ),
              div(style="padding-left: 30px",
                  uiOutput(ns('newdata')),

              )
          )
        )
      )


    })
    observeEvent(input$create_replace,{
      shinyjs::toggle("newdatalit",condition=input$create_replace=="Create")
      shinyjs::toggle("overdatalist",condition=input$create_replace=="Replace")
    })
    observeEvent(input$data_confirm,ignoreInit = T,{


      model<-vals$cmodel

      m<-vals$cur_caret_model

      if(is.null( attr(vals$saved_data[[input$data_x]],model))){
        attr(vals$saved_data[[data_x()]],model)<-list()
      }

      if(input$create_replace=="Create"){
        attr(m,"model_name")<-input$newdatalit
        attr(vals$saved_data[[input$data_x]],model)[[input$newdatalit]]<-list(m=m)
        attr(vals$saved_data[[input$data_x]],model)[["new model"]]<-NULL
        vals$cur_model_name<-input$newdatalit

      } else{
        attr(m,"model_name")<-input$overdatalist
        attr(vals$saved_data[[input$data_x]],model)[[input$overdatalist]]<-list(m=m)
        attr(vals$saved_data[[input$data_x]],model)[["new model"]]<-NULL
        vals$cur_model_name<-input$overdatalist
      }
      vals$newmodel<-NULL
      removeModal()



    })
    observeEvent(input$data_x,{
      req(input$data_x%in%names(vals$saved_data))
      vals$cur_data_sl<-input$data_x
    })

    observeEvent(input$model_type,{
      vals$cur_model_type<-input$model_type
    })
    observeEvent(input$var_y,ignoreInit = T,{
      vals$cur_response<-input$var_y
    })
    observeEvent(input$partition,ignoreInit = T,{
      vals$cur_test_partition<-input$partition
    })
    observeEvent(input$partition_ref,ignoreInit = T,{
      vals$cur_testdata<-input$partition_ref
    })
    observe({
      shinyjs::toggle("save_model",condition=input$model_name=="new model")
    })
    observe({
      condition<-length(names(available_models())>0)
      shinyjs::toggle("model_model_name", condition=condition)
    })
    observe({
      choices<-names(available_models())
      if(is.null(choices)){
        vals$update_tab_caret<-"tab1"
      }
    })
    observe({
      shinyjs::toggle("partition_ref",condition=input$partition!="None")
    })
    observeEvent(model_type(),ignoreInit = T,{
      updateRadioButtons(session,'model_type',choices=model_type(),inline =T, selected=vals$cur_model_type)
    })



    observeEvent(get_model(),{
      vals$cur_caret_model<-get_model()
    })



    output$model_name<-renderUI({

      choices<-names(available_models())

      selected<-get_selected_from_choices(vals$cur_model_name,choices)
      pickerInput_fromtop_live(ns("model_name"),span("Custom Name",tipright("Select the name of the saved model corresponding to the chosen dataset and method, if available. Only models from the selected Training Datalist and Method will be displayed.")),choices =choices,selected=selected)
    })




    observeEvent(x_train(),{
      vals$cur_xtrain<-x_train()

    })
    observeEvent(y_train(),{
      vals$cur_var_y<-y_train()
    })

    observeEvent(vals$cur_caret_tab,{
      shinyjs::toggle("model_type_panel",condition=vals$cur_caret_tab=="tab1")
      shinyjs::toggle("model_y_panel",condition=vals$cur_caret_tab=="tab1")
    })
    observeEvent(data_x(),{
      choices<-colnames(data_x())
      #selected<-get_all_selected_from_choices(vals$cur_filter_model,choices)
      selected=choices

      shinyWidgets::updateVirtualSelect(
        "filter",
        choices=choices,
        selected=selected
      )
    })



    observeEvent(vals$cmodel,{
      req(vals$cmodel)
      type=sort(caret::getModelInfo(vals$cmodel,regex = F)[[1]]$type)
      model_type(type)
    })





    output$data_y<-renderUI({
      pickerInput_fromtop_live(ns("data_y"),span("Datalist",tipright("Select the Datalist containing the response variable (Y)")),choices =names(vals$saved_data),selected=vals$cur_data_sl_y)
    })



    observe({


      shinyjs::toggle(selector=".choices_tip_show",
                      condition=isTRUE(input$show_nmodels))
    })





    get_dfmodels<-function(){
      req(length(vals$saved_data)>0)
      dfmodels<-sapply(names(vals$saved_data),function(data_x){
        dx<-vals$saved_data[[data_x]]
        as.character(lapply(c(models,"rfGA"),function(attr) {
          n<-length(names(attr(dx,attr)))
          if(n>0)
            paste0(attr," (",n," trained models)")
        }))

      })
      dfmodels<-data.frame(dfmodels)
      colnames(dfmodels)<-names(vals$saved_data)
      col=9
      tips<-lapply(1:ncol(dfmodels),function(col) {
        i<-colnames(dfmodels)[col]
        x<-dfmodels[,i]
        n<-x[x!="NULL"]

        div(
          class="choices_tip",
          div(i),
          div(class="choices_tip_show",
              if(length(n)>0)
                div(lapply(n,div),class="picker_tip") else{div("0 trained models",class="picker_tip tip_show")}
          )

        )
      })
      names(tips)<-names(vals$saved_data)
      tips
    }

    output$data_x<-renderUI({


      dflist<-get_dfmodels()
      tips<-lapply(dflist, function(x) HTML(paste0(x)))

      choices<-names(vals$saved_data)
      div(

        div(class="data_x",
            uiOutput(ns("data_x_active")),
            pickerInput_fromtop_live(ns("data_x"),
                                     span("~ Training Datalist",tiphelp("Choose the Datalist containing your independent variables (X).")),
                                     selected=vals$cur_data_sl,
                                     choices=choices,
                                     choicesOpt  =list(
                                       content=tips
                                     ),
                                     options=shinyWidgets::pickerOptions(
                                       header=uiOutput(ns("sho_saved_models"))
                                     )



            )
        )
      )
    })

    output$sho_saved_models<-renderUI({
      checkboxInput(ns("show_nmodels"),actionLink(ns('show_nmodels_label'),span("Show trained models",tipright("Displays the number of saved models in each Datalist"))),value=F)
    })

    observeEvent(input$show_nmodels_label,{
      value=ifelse(isFALSE(input$show_nmodels),T,F)
      updateCheckboxInput(session,'show_nmodels',value=value)
    })

    observeEvent(input$show_nmodels,{
      vals$show_nmodels<-input$show_nmodels
    })

    observe({
      if(is.null(vals$cur_show_nmodels)){
        vals$cur_show_nmodels<-F
      }
    })




    output$var_y<-renderUI({
      data<-get_data_y()
      choices<-colnames(data)

      selected<-get_selected_from_choices(vals$cur_response,choices)

      pickerInput_fromtop_live(ns("var_y"),
                               span("Variable",tipright("Choose the response Variable (Y)")),
                               choices = choices,selected=selected)
    })

    output$partition<-renderUI({
      data<-y_factors()
      vals$cur_data_sl_y<-input$data_y
      choices<-c("None",colnames(data))
      selected<-get_selected_from_choices(vals$cur_test_partition,choices)


      pickerInput_fromtop_live(ns("partition"),span("Partition",tipright("Select a factor to use as a reference for partitioning the data into training and testing sets")), choices=choices,selected=selected)
    })

    output$partition_ref_out<-renderUI({
      req(input$partition!="None")
      data<-y_factors()
      choices<-levels(data[,input$partition])
      selected<-get_selected_from_choices(vals$cur_testdata,choices)
      pickerInput_fromtop_live(ns("partition_ref"),span("Test reference:",tipright("Choose the level of the selected factor to serve as a reference for the test data. Data corresponding to this level will be excluded from the training set and can be used later for model evaluation or prediction generation")), choices=choices, selected=selected)
    })


    result<-reactive({
      req(input$var_y)
      list(
        x_train=x_train(),
        y_train=y_train(),
        x_test=x_test(),
        y_test=y_test(),
        data_x=input$data_x,
        data_y=input$data_y,
        var_y=input$var_y,
        partition=input$partition,
        partition_ref=input$partition_ref
      )
    })

    observeEvent(result(),{
      vals$trainSL_args<-result()
    })


    return(NULL)

  })
}

#' @export
caret_model<-list()
#' @export
caret_model$ui<-function(id){
  module_progress("Loading module: Supervised Algorithms")
  ns<-NS(id)

  div(

    uiOutput(ns("model"))
  )

}
#' @export
caret_model$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    output$model<-renderUI({

      div(
        pickerInput_fromtop(ns("model"),span("Model",tipright("Select the machine learning algorithm for model training. If there are saved models corresponding to the selected Training Datalist, they will be displayed in the Custom Name dropdown")),
                            options=shinyWidgets::pickerOptions(liveSearch =T,windowPadding="top"),
                            choices =grupos_modelos(),
                            selected=vals$cmodel,
                            choicesOpt =list(content =get_model_subtext())

        )
      )
    })

    grupos_modelos <- function() {
      grupos <- list()
      model_list<-c(models,"rfGA")
      # Agrupamento dos modelos
      grupos <- list(
        "Tree-based" = c("rf","gbm", "cforest", "rpart", "evtree"),
        "Miscellaneous Methods" = c("nb", "knn","xyf", "glm"),
        "Kernel" = c("gaussprRadial", "svmLinear", "svmRadial", "svmRadialCost"),

        "Neural network" = c("dnn", "avNNet", "nnet", "pcaNNet", "monmlp", "mlpML"),
        "Feature selection"=c("rfGA")
      )

      # Mapeamento dos modelos para os grupos
      grupos_modelos <- list()
      for (grupo in names(grupos)) {
        modelos_grupo <- grupos[[grupo]]
        modelos_no_grupo <- model_list[model_list %in% modelos_grupo]
        grupos_modelos[[grupo]] <- modelos_no_grupo
      }

      return(grupos_modelos)
    }

    get_model_subtext<-reactive({


      models_names<-c(names(models),'rfGA - Feature Selection using randomForest Genetic Algorithm')

      names(models_names)<-c(models,'rfGA')
      models_names<-models_names[ as.character(unlist(grupos_modelos()))]
      names(models_names)<-NULL
      mn<-strsplit(models_names," - ")


      req(vals$trainSL_args$data_x)


      data_x<-vals$trainSL_args$data_x

      lapply(mn,function(x){
        attr<-x[1]
        n=length(attr(vals$saved_data[[data_x]],attr))
        saved_models<-NULL
        if(n>0){
          saved_models<-HTML(paste0(span(span(class="model_ico",icon("hand-point-left")),n, " trained models",class="model_subtext")))}
        HTML(paste(span(x[1],span(class="model_tag",x[2]),saved_models )))

      })
    })

    observe({
      vals$cmodel<-input$model
    })




    observeEvent(input$model,ignoreInit = T,{
      #vals$cur_caret_model<-NULL
      if(!is.null( vals$cmodel)){
        pkg<-model_library[[vals$cmodel]]
        sapply(pkg,detach_package)
        message(paste0("detached:",pkg))

      }



    })
    observe({
      req(vals$update_state)
      update_state<-vals$update_state
      ids<-names(update_state)
      update_on<-grepl(id,ids)
      names(update_on)<-ids
      to_loop<-names(which(update_on))
      withProgress(min=1,max=length(to_loop),message="Restoring",{
        for(i in to_loop) {
          idi<-gsub(paste0(id,"-"),"",i)
          incProgress(1)
          restored<-restoreInputs2(session, idi, update_state[[i]])

          if(isTRUE(restored)){
            vals$update_state[[i]]<-NULL
          }

        }
      })

    })
    return(NULL)


  })
}
