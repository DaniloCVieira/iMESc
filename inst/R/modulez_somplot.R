#' @noRd


#' @export
#'
module_ui_somplot<-function(id){
  ns<-NS(id)
  div(
    class='choosechannel',
   # inline( actionButton(ns("teste_comb"),"SAVE_somplot")),
    uiOutput(ns("side_somplot"))
  )
}

#' @export
#'
module_server_somplot<-function(input, output, session,vals,data_target,som_model,background_type="hc",property=NULL,hc=NULL,
                                  map_newdata=F,df_symbol,df_colors,col_grad=NULL, sort_cluster=F) {
  ns<-session$ns



  somplot_args<-reactiveValues(df=NULL,vals_module=isolate({
    valist<-reactiveValuesToList(vals)
    valist$cur_data<-data_target
    valist
  }))



  somval<-reactiveValues(hc=hc)
  req(data_target%in%names(somplot_args$vals_module$saved_data))
  data<-somplot_args$vals_module$saved_data[[data_target]]
  somplot_args$vals_module$cur_data<-data_target


  getsolid_col<-reactive({
    res<-lapply(somplot_args$vals_module$newcolhabs, function(x) x(10))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(somplot_args$vals_module$colors_img$val%in%solid)
    pic
  })
  get_model<-reactive({

    if(  inherits(somplot_args$vals_module$SOM_MODEL,"kohonen")){
      return(somplot_args$vals_module$SOM_MODEL)
    }



    if(  inherits(som_model,"kohonen")){
      return(som_model)
    }

    if(isFALSE(map_newdata)){
      m<-attr(data,"som")[[as.character(som_model)]]


      req(!is.null(m))
      if(  isFALSE(inherits(m,"kohonen"))){
        supervisor<-attr(m[[1]],"supervisor")
        m<-m[[1]]$finalModel
        req(inherits(m,"kohonen"))
        names(m$unit.classif)<-rownames(do.call(cbind,m$data))
        if(ncol(m$codes[[2]])==length(supervisor)){
          colnames(m$codes[[2]])<-supervisor}
        attr(m,"supervisor")<-supervisor
      }
      req(inherits(m,"kohonen"))
      m

    } else{
      #input<-list()
      #input$newdata<-choices[1]
      #m0<-m<-attr(somplot_args$vals_module$saved_data$`fisico_quimico(i)_scaled`,"som")[[1]]

    }



  })

  getgrad_col<-reactive({
    res<-lapply(somplot_args$vals_module$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(somplot_args$vals_module$colors_img$val%in%grad)
    pic
  })
  get_choices_pal<-reactive({
    if(!is.null(col_grad)){
      title="+ Background palette"
      somplot_args$somplot_bg<-"turbo"
      choices=getgrad_col()
      attr(choices,"title")<-title
      return(choices)
    }


    if(background_type!="hc") {
      req(length(input$plus_umatrix)>0)
      req(length(input$plus_property)>0)
      title="+ Background palette"
      if(sum(input$plus_umatrix,input$plus_property)==0){
        somplot_args$somplot_bg<-"gray"
        choices=getsolid_col()
      } else {

        somplot_args$somplot_bg<-"viridis"
        choices=getgrad_col()

      }

    } else {
      title="+ Cluster palette"
      somplot_args$somplot_bg<-"turbo"
      choices=getgrad_col()

    }
    attr(choices,"title")<-title
    choices
  })
  output$hc_palette<-renderUI({
    choices<-get_choices_pal()
    title<-attr(choices,"title")
    div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',
        div(class="palette",
            title,
            pickerInput(inputId = ns("bg_palette"),
                        label =NULL,
                        choices =  somplot_args$vals_module$colors_img$val[choices],
                        selected=somplot_args$somplot_bg,
                        choicesOpt = list(
                          content =  somplot_args$vals_module$colors_img$img[choices] ),
                        options=list(container="body"), width="100px")),
        uiOutput(ns("pcodes_bgalpha"))

    )
  })

  output$out_pclus_addpoints<-renderUI({
    if(is.null(somplot_args$pclus_addpoints)){
      somplot_args$pclus_addpoints<-T
    }
    checkboxInput(ns("pclus_addpoints"),"+ Points",
                  value=somplot_args$pclus_addpoints)
  })

  output$out_pclus_addtext<-renderUI({
    if(is.null(somplot_args$pclus_text_inputs)){
      somplot_args$pclus_text_inputs<-F
    }
    checkboxInput(ns("pclus_addtext"),"+ Labels",
                  value=somplot_args$pclus_addtext)
  })

  output$side_somplot<-renderUI({



    div(class="map_control_style",style="color: #05668D",
        div(

          uiOutput(ns('newdata')),
          #uiOutput("somcut_display"),
          uiOutput(ns("hc_ordcluster")),
          uiOutput(ns('hc_palette')),

          uiOutput(ns('umapro')),
          div(
            style='border-bottom: 1px solid gray',
            uiOutput(ns('out_pclus_addpoints')),
            div(
              style="margin-left: 10px",
              uiOutput(ns("pclus_points_inputs"))
            )
          ),

          div(style='border-bottom: 1px solid gray',
              uiOutput(ns('out_pclus_addtext')),
              div(
                style="margin-left: 10px",
                uiOutput(ns("pclus_text_inputs"))
              )),
          div(
            uiOutput(ns("vfm_check"))
          ),
          uiOutput(ns("showerrors_som")),
          uiOutput(ns("theme")),
          uiOutput(ns("title"))


        ))
  })



  output$hc_ordcluster<-renderUI({
    req(isTRUE(sort_cluster))
    div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; padding-bottom: 5px',
        div("+",
            inline(checkboxInput(ns("hc_sort"),span("Sort clusters",tiphelp("Sort clusters by a  variable")),value=somplot_args$vals_module$hc_sort,width="120px")),
            div(style="margin-left: 40px",
                inline(uiOutput(ns("hc_sort_datalist"))) ,

                inline(uiOutput(ns("hc_ord_factor"))))

        )
    )
  })
  output$hc_sort_datalist<-renderUI({
    req(isTRUE(input$hc_sort))
    div(
      div("Datalist:"),
      inline(pickerInput(ns("hc_ord_datalist"),NULL,choices = c(names(somplot_args$vals_module$saved_data[getdata_for_hc()])),selected=somplot_args$vals_module$hc_ord_datalist,width="200px")),  "::"
    )
  })
  output$hc_ord_factor<-renderUI({
    req(isTRUE(input$hc_sort))
    req(input$hc_ord_datalist)
    data<-somplot_args$vals_module$saved_data[[input$hc_ord_datalist]]

    choices=c(colnames(data))

    div(
      div("Variable:"),
      pickerInput(ns("hc_ord_factor"),NULL,
                  choices = choices,selected=somplot_args$vals_module$hc_ord_factor,width="120px")
    )

  })
  getdata_for_hc<-reactive({

    datalist<-somplot_args$vals_module$saved_data

    req(length(data)>0)
    res0<-unlist(
      lapply(datalist, function (x){
        sum(rownames(x)%in%rownames(data))==nrow(x)
      })
    )
    names(res0[res0==T])
  })
  observeEvent(ignoreInit = T,input$hc_ord_factor,{
    somplot_args$vals_module$hc_ord_factor<-input$hc_ord_factor
  })
  observeEvent(ignoreInit = T,input$hc_ord_datalist,{
    somplot_args$vals_module$hc_ord_datalist<-input$hc_ord_datalist
  })
  observeEvent(ignoreInit = T,input$hc_sort,{
    somplot_args$vals_module$hc_sort<-input$hc_sort
  })
  output$title<-renderUI({
    div("+ Title: ",
        inline(textInput(ns("title"), NULL, "", width="200px"))
    )
  })
  output$theme<-renderUI({
    if(is.null(somplot_args$theme)){
      somplot_args$theme<-F
    }
    div(style='border-bottom: 1px solid gray',
        inline(checkboxInput(ns("theme"),
                             label = "+ show neuron coordinates",
                             value=somplot_args$theme,
                             width="200px"
        ))
    )
  })
  observeEvent(ignoreInit = T,input$theme,{
    somplot_args$theme<-input$theme
  })
  observeEvent(ignoreInit = T,input$pclus_addtext,{
    somplot_args$pclus_addtext<-input$pclus_addtext
  })
  observeEvent(ignoreInit = T,input$pclus_addpoints,{
    somplot_args$pclus_addpoints<-input$pclus_addpoints
  })
  output$umapro<-renderUI({
    req(is.null(col_grad))
    req(background_type!="hc")
    div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; ',

        inline(uiOutput(ns('plus_umatrix'))),
        inline(uiOutput(ns('plus_property')))
    )
  })
  output$plus_umatrix<-renderUI({
    if(is.null(somplot_args$plus_umatrix)){somplot_args$plus_umatrix<-F}
    checkboxInput(ns("plus_umatrix"),strong("+ U-Matrix:"), value=somplot_args$plus_umatrix)
  })
  output$plus_property<-renderUI({
    if(is.null(somplot_args$plus_property)){somplot_args$plus_property<-F}
    div(
      style="font-size: 14px",
      checkboxInput(ns("plus_property"),strong("+ Property:"), value=somplot_args$plus_property),
      uiOutput(ns("var_pproperty"))

    )
  })

  output$out_variable_pproperty<-renderUI({
    choices = colnames(get_model()$data[[1]])
    pickerInput(ns("variable_pproperty"),
                label = NULL,
                choices = choices,
                selected=somplot_args$variable_pproperty,
                width="250px"
    )
  })
  output$var_pproperty<-renderUI({
    req(isTRUE(input$plus_property))

    div(
      inline(uiOutput(ns('prev_property'))),
      inline(uiOutput(ns('out_variable_pproperty'))),
      inline(uiOutput(ns('next_property')))


    )
  })
  output$prev_property<-renderUI({
    data = data.frame(data,"factors")
    req(which(colnames(data)==input$variable_pproperty)!=1)
    actionButton(ns("prev_property"),"<<")
  })
  output$next_property<-renderUI({
    data = data.frame(data,"factors")
    req(which(colnames(data)!=input$variable_pproperty)==ncol(data))

    data = data.frame(data,"factors")
    actionButton(ns("next_property"),">>")
  })
  observeEvent(ignoreInit = T,input$plus_umatrix,{
    somplot_args$plus_umatrix<-input$plus_umatrix
    if(isTRUE(input$plus_umatrix)){
      somplot_args$plus_property<-F
      somplot_args$backtype<-"uMatrix"
    }
  })
  observeEvent(ignoreInit = T,input$plus_property,{
    somplot_args$plus_property<-input$plus_property
    if(isTRUE(input$plus_property)){
      somplot_args$plus_umatrix<-F
      somplot_args$backtype<-"property"
    }
  })
  observeEvent(ignoreInit = T,input$pp_labels,{
    somplot_args$pp_labels<-input$pp_labels
  })
  observeEvent(ignoreInit = T,input$next_property,{
    data = data.frame(data,"factors")
    pnext<-colnames(data)[which(colnames(data)==input$variable_pproperty)+1]
    updateSelectInput(session,'variable_pproperty',selected=pnext)

  })
  observeEvent(ignoreInit = T,input$prev_property,{
    data = data.frame(data,"factors")
    pprev<-colnames(data)[which(colnames(data)==input$variable_pproperty)-1]
    updateSelectInput(session,'variable_pproperty',selected=pprev)
  })
  observeEvent(ignoreInit = T,input$variable_pproperty,{
    somplot_args$variable_pproperty<-input$variable_pproperty
  })
  output$showerrors_som<-renderUI({
    req(background_type!="uMatrix")
    div(
      div(strong("+ Show error"),
          pickerInput(ns("show_mapcode_errors"), NULL,choices=c('None',"Topographic Error","Quantization Error","Explained Variance","Kaski-Lagus Errors"), width="200px", selected=somplot_args$show_mapcode_errors)
      ),
      div(
        "+ Round errors",inline(
          tipify_ui(numericInput(ns("round_error"),NULL,value = 3,min = 0,max = 1,step = 1, width="75px"),"symbol size")
        )
      )
    )

  })
  observeEvent(ignoreInit = T,input$round_error,{
    somplot_args$round_error<-input$round_error
  })
  observeEvent(ignoreInit = T,input$show_mapcode_errors,{
    somplot_args$show_mapcode_errors<-input$show_mapcode_errors
  })

  getmodel_hc0<-reactive({
    attr(data,"som")[[as.character(som_model)]]
  })

  getsom_layers<-reactive({
    m<-getmodel_hc0()
    layers<-names(m$data)
    layers

  })

  predsupersom_hc<-reactive({
    layers<-getsom_layers()
    whatmap=layers[hcsom_whatmap()]
    m<-getmodel_hc0()
    newdatas<-vals$saved_data[hcsom_active_layers()]
    newdata_matrices<-lapply(newdatas,function(x) as.matrix(x))
    pred<-predict(m,newdata_matrices,unit.predictions=m$codes,whatmap=whatmap)
    m2<-m
    m2$data<-pred$predictions
    m2$codes<-pred$unit.predictions
    bmus<-pred$unit.classif
    names(bmus)<-rownames(pred$predictions[[1]])
    m2$unit.classif<-bmus
    m2$whatmap<-pred$whatmap
    m<-m2
    m


  })
  hcsom_whatmap<-reactive({
    layers<-getsom_layers()
    sapply(layers,function(x) {isTRUE(input[[paste0("hcsom_layer",x)]])})
  })
  hcsom_active_layers<-reactive({
    layers<-getsom_layers()
    active_layers<-sapply(layers,function(x) {
      if(isTRUE(input[[paste0("hcsom_layer",x)]])){
        input[[paste0("hcsom_newdata_layer",x)]]
      } else{NULL}

    })
    unlist(active_layers)
  })

  output$bug<-renderUI({
    div(
      style="width: 700px",
      renderPrint({predsupersom_hc()})
    )
  })

  output$newdata<-renderUI({
    req(isTRUE(map_newdata))
    div(

      div(uiOutput(ns('bug'))),
      div(uiOutput(ns('out_hcsom_whatmap')))

    )})

  output$out_hcsom_whatmap<-renderUI({
    layers<-getsom_layers()
    div(
      "+ New Data:",
      lapply(layers,function(x){
        div(class="map_control_style2",style="color: #05668D",
            inline(checkboxInput(ns(paste0("hcsom_layer",x)),x,T)),
            inline(uiOutput(ns(paste0("hcsom_piclayer",x))))

        )
      })
    )
  })

  observe({
    layers<-getsom_layers()
    m<-getmodel_hc0()
    lapply(layers,function(x){
      output[[paste0("hcsom_piclayer",x)]]<-renderUI({
        if( isTRUE(input[[paste0("hcsom_layer",x)]])){
          choices_temp<-names(which(sapply(vals$saved_data,function(xx){
            identical(sort(colnames(xx)),
                      sort(colnames(m$data[[x]])))
          })))
          pickerInput(ns(paste0("hcsom_newdata_layer",x)), NULL, choices_temp)
        }
      })
    })
  })
  savereac<-reactive({
    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-somplot_args$vals_module$saved_data
    tosave$newcolhabs<-somplot_args$vals_module$newcolhabs
    tosave$colors_img<-somplot_args$vals_module$colors_img
    tosave$args<-argsplot()
    saveRDS(tosave,"savepoint_som.rds")
    saveRDS(reactiveValuesToList(input),"input_som.rds")
    beepr::beep()
    #vals<-readRDS("vals.rds")
    #input<-readRDS('input.rds')

  })
  observeEvent(ignoreInit = T,input$teste_comb,{
    savereac()
  })

  getobs_mapcode<-reactive({
    datalist<-somplot_args$vals_module$saved_data
    #som_names<-names(attr(somplot_args$vals_module$saved_data[[somplot_args$vals_module$cur_data]],"som"))
    #req(length(som_names)>0)

    models<- m<-get_model()
    req(length(models)>0)
    req(somplot_args$vals_module$cur_som_hc%in%names(models))
    m<-models[[as.character(somplot_args$vals_module$cur_som_hc)]]
    res0<-unlist(
      lapply(datalist, function (x){
        identical(colnames(x),colnames(do.call(cbind,m$data)))
      })
    )
    choices<-names(res0[res0==T])
    res0<-unlist(
      lapply(datalist[choices], function (x){
        !anyNA(x)
      })
    )


    choices<-names(res0[res0==T])
    choices
  })
  observeEvent(ignoreInit = T,input$bg_palette,{
    somplot_args$somplot_bg<-input$bg_palette
  })
  output$pclus_points_inputs<-renderUI({
    req(isTRUE(input$pclus_addpoints))
    div(
      div("+ Palette",inline(uiOutput(ns("pclus_points_palette")))),
      div("+ Factor",inline(uiOutput(ns("pclus_points_factor_out")))),
      div("+ Shape",inline(uiOutput(ns("pclus_points_shape")))),
      div("+ Size",inline(uiOutput(ns("pclus_points_size"))))
    )
  })
  output$pclus_points_palette<-renderUI({

    if(is.null(somplot_args$pclus_points_palette)){somplot_args$pclus_points_palette<-"black"}
    inline(
      tipify_ui(pickerInput(inputId = ns("pclus_points_palette"),
                         label =NULL,
                         choices = somplot_args$vals_module$colors_img$val,
                         choicesOpt = list(content = somplot_args$vals_module$colors_img$img),
                         selected=somplot_args$pclus_points_palette,
                         options=list(container="body"), width="75px"),
             "Symbol colors"
      )
    )
  })
  output$pclus_points_factor_out<-renderUI({
    req(input$pclus_points_palette)
    choices<-c(colnames(attr(somplot_args$vals_module$saved_data[[somplot_args$vals_module$cur_data]],"factors")))


    inline(
      pickerInput(ns("pclus_points_factor"),NULL,
                  choices = c(choices),selected=somplot_args$vals_module$pclus_points_factor,width="150px")
    )


  })
  output$pclus_points_shape<-renderUI({
    tipify_ui(pickerInput(inputId = ns("pclus_symbol"),
                       label = NULL,
                       choices = df_symbol$val,
                       choicesOpt = list(content = df_symbol$img),
                       options=list(container="body"), width="100px",
                       selected=somplot_args$pclus_symbol)
           ,"symbol shape")
  })
  output$pclus_points_size<-renderUI({
    if(is.null(somplot_args$pclus_points_size)){somplot_args$pclus_points_size<-1}
    inline(
      tipify_ui(numericInput(ns("pclus_points_size"),NULL,value = somplot_args$pclus_points_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
    )
  })
  output$pclus_text_inputs<-renderUI({
    req(isTRUE(input$pclus_addtext))
    div(
      div("+ Palette",inline(uiOutput(ns("pclus_text_palette")))),
      div("+ Factor",inline(uiOutput(ns("pclus_text_factor_out")))),
      div("+ Size",inline(uiOutput(ns("pclus_text_size"))))
    )
  })
  output$pclus_text_palette<-renderUI({

    if(is.null(somplot_args$vals_module$pclus_text_palette)){somplot_args$vals_module$pclus_text_palette<-"black"}
    inline(
      tipify_ui(pickerInput(inputId = ns("pclus_text_palette"),
                         label =NULL,
                         choices =  somplot_args$vals_module$colors_img$val[getsolid_col()],
                         selected=somplot_args$vals_module$pclus_text_palette,
                         choicesOpt = list(
                           content =  somplot_args$vals_module$colors_img$img[getsolid_col()] ),
                         options=list(container="body"), width="75px"),
             "Symbol colors"
      )
    )
  })
  observeEvent(ignoreInit = T,input$pclus_text_palette,{
    somplot_args$vals_module$pclus_text_palette<-input$pclus_text_palette
  })
  output$pclus_text_factor_out<-renderUI({
    req(input$pclus_text_palette)
    choices<-c(colnames(attr(somplot_args$vals_module$saved_data[[somplot_args$vals_module$cur_data]],"factors")))
    inline(
      pickerInput(ns("pclus_text_factor"),NULL,
                  choices = c(choices),selected=somplot_args$vals_module$pclus_text_factor,width="150px")
    )


  })
  observeEvent(ignoreInit = T,input$pclus_text_factor,{
    somplot_args$vals_module$pclus_text_factor<-input$pclus_text_factor
  })
  output$pclus_text_size<-renderUI({
    if(is.null(somplot_args$pclus_text_size)){somplot_args$pclus_text_size<-1}
    inline(
      tipify_ui(numericInput(ns("pclus_text_size"),NULL,value = somplot_args$pclus_text_size,min = 0.1,max = 3,step = .1, width="100px"),"symbol size")
    )
  })
  output$vfm_check<-renderUI({
    if(is.null(somplot_args$pclus_varfacmap_action)){somplot_args$pclus_varfacmap_action<-T}
    div(style='border-bottom: 1px solid gray',
        span("+ ",
             inline(checkboxInput(ns("varfacmap_action"), span("Variable factor map",actionLink(ns("varfacmap"), tipify_ui(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value =somplot_args$pclus_varfacmap_action, width="100px"))),
        div(style="margin-left: 10px",uiOutput(ns("varfac_out")))
    )
  })
  observeEvent(ignoreInit = T,input$pclus_border,{
    somplot_args$pclus_border<-input$pclus_border
  })
  output$varfac_out<-renderUI({
    req(isTRUE(input$varfacmap_action))
    if(is.null(somplot_args$pclus_border)){
      somplot_args$pclus_border<-"white"
    }
    div(
      uiOutput(ns('vfm_type_out')),
      uiOutput(ns('npic_out')))})
  output$pcodes_bgalpha<-renderUI({
    if(is.null(somplot_args$pclus_border)){somplot_args$pclus_border<-"white"}
    if(is.null(somplot_args$pcodes_bgalpha)){
      if(background_type!="hc"){
        somplot_args$pcodes_bgalpha<-0
      } else{
        somplot_args$pcodes_bgalpha<-0.5
      }

    }
    if(is.null(somplot_args$base_size)){
      somplot_args$base_size<-12
    }
    div(span(
      "+ Background lightness",inline(
        tipify_ui(numericInput(ns("pcodes_bgalpha"),NULL,value = somplot_args$pcodes_bgalpha,min = 0,max = 1,step = .1, width="75px"),"symbol size")
      )
    ),
    div(span(span('+ Border:'),inline(
      div(class="palette", pickerInput(ns("pclus_border"),
                                       label =NULL,
                                       choices =  somplot_args$vals_module$colors_img$val[getsolid_col()] ,
                                       choicesOpt = list(
                                         content =  somplot_args$vals_module$colors_img$img[getsolid_col()] ),
                                       selected= somplot_args$pclus_border, width = '75px'))
    ))),

    div(
      "+ Base size",inline(
        tipify_ui(numericInput(ns("base_size"),NULL,value = somplot_args$base_size, width="75px"),"symbol size")
      )
    )
    )
  })
  output$hc_tab4_out<-renderUI({
    div( div(class="map_control_style",style="color: #05668D",
             uiOutput(ns('custom_levels'))
    ),
    column(12,plotOutput(ns("pclus_code")))
    )
  })
  bmu_clus_points<-reactive({
    req(input$dot_label_clus)
    if(input$dot_label_clus == 'symbols'){ T} else {F}
  })
  output$vfm_type_out<-renderUI({
    my_choices<-if(background_type=="uMatrix"){
      list("Highest"='var', "Clockwise"="cor")
    } else{
      list("Highest"='var', "Clockwise"="cor","Cluster"="cor_hc")

    }

    div(span("+ Show correlation:",inline(
      pickerInput(ns("vfm_type"),NULL,
                  choices =my_choices,
                  selected=somplot_args$vfm_type,
                  width="150px"
      ))))
  })
  output$npic_out<-renderUI({
    if(is.null(somplot_args$npic)){somplot_args$npic<-10}
    if(is.null(somplot_args$pclus.cex.var)){somplot_args$pclus.cex.var=1}
    if(is.null(somplot_args$p.clus.col.text)){somplot_args$p.clus.col.text<-"black"}
    if(is.null(somplot_args$var_bg)){somplot_args$var_bg<-"white"}
    if(is.null(somplot_args$var_bg_transp)){somplot_args$var_bg_transp=0}
    div(
      div(
        span("+ Number",
             inline(
               tipify_ui(
                 numericInput(ns("npic"), NULL, value = somplot_args$npic, min = 2, width="75px"),"Number of variables to display"
               )))
      ),
      div(
        span("+ Var size",
             inline(
               numericInput(ns("pclus.cex.var"), NULL, value = somplot_args$pclus.cex.var, min = 2, width="75px")))
      ),
      div(
        span("+ Var text color",
             inline(
               div(class="palette",
                   pickerInput(inputId = ns("p.clus.col.text"),
                               label = NULL,
                               choices = somplot_args$vals_module$colors_img$val[getsolid_col()],
                               choicesOpt=list(content=somplot_args$vals_module$colors_img$img[getsolid_col()]),
                               selected=somplot_args$p.clus.col.text,
                               width="100px"))))
      ),
      div(
        span("+ Var background",
             inline(
               div(class="palette",
                   pickerInput(inputId = ns("var_bg"),
                               label = NULL,
                               choices = somplot_args$vals_module$colors_img$val[getsolid_col()],
                               choicesOpt=list(content=somplot_args$vals_module$colors_img$img[getsolid_col()]),
                               selected=somplot_args$var_bg,
                               width="100px"))))
      ),
      div(
        span("+ Var transparency",
             inline(
               tipify_ui(
                 numericInput(ns("var_bg_transp"), NULL, value = somplot_args$var_bg_transp, min = 2, width="75px"),"Number of variables to display"
               )))
      )
    )
  })
  indicate_hc<-reactive({
    npic<-NULL
    indicate<-NULL
    if(isTRUE(input$varfacmap_action)){

      npic<- input$npic
      indicate<- input$vfm_type
    }
    iind=list(indicate=indicate,npic=npic)
    iind
  })
  bp_som<-reactive({
    iind=indicate_hc()
    m<-get_model()
    bp<-getbp_som(m=m,indicate=iind$indicate,npic=iind$npic,hc=somplot_args$vals_module$cutsom)
    somplot_args$bp_som<-bp
    bp
  })
  get_network<-reactive({
    backtype=NULL
    property=NULL
    if(background_type=="hc"){
      backtype="hc"
      property=NULL

    } else{
      if(length(input$plus_umatrix>0)){
        if(length(input$plus_property>0)){
          if(isFALSE(input$plus_umatrix)&isFALSE(input$plus_property)){
            backtype=NULL
            property=NULL
          } else {
            req(length(somplot_args$backtype)>0)
            backtype=somplot_args$backtype
            property=NULL

            if(backtype=="property"){
              req(length(input$variable_pproperty)>0)
              property=input$variable_pproperty}
          }
        }
      }


    }



    m<-get_model()
    hexs<-get_neurons(m,background_type=backtype,property=property, hc=somval$hc)
    somplot_args$hc_network<-hexs
    hexs
  })
  get_copoints<-reactive({
    m<-get_model()
    copoints<-getcopoints(m)
    somplot_args$copoints_hc<-copoints
    copoints
  })
  copoints_scaled<-reactive({
    get_network()
    get_copoints()
    points_tomap=rescale_copoints(hexs=somplot_args$hc_network,copoints=somplot_args$copoints_hc)
    data<-somplot_args$vals_module$saved_data[[data_target]]

    factors<-attr(data,"factors")
    if(length(input$pclus_text_factor)>0){
      req(input$pclus_text_factor%in%colnames(factors))
      text_factor= factors[rownames(data),input$pclus_text_factor, drop=F]
      points_tomap$label<-text_factor[rownames(points_tomap),]
    }
    if(length(input$pclus_points_factor)>0){
      req(input$pclus_points_factor%in%colnames(factors))
      points_factor= factors[rownames(data),input$pclus_points_factor, drop=F]
      points_tomap$point<-points_factor[rownames(points_tomap),]
      attr(points_tomap,"namepoints")<-input$pclus_points_factor
    }
    somplot_args$hc_mapsom<-points_tomap
    points_tomap
  })
  observeEvent(ignoreInit = T,input$vfm_type,{
    somplot_args$vfm_type<-input$vfm_type
  })
  observeEvent(ignoreInit = T,input$npic,{
    somplot_args$npic<-input$npic
  })
  observeEvent(ignoreInit = T,input$pclus.cex.var,{
    somplot_args$pclus.cex.var<-input$pclus.cex.var
  })
  observeEvent(ignoreInit = T,input$p.clus.col.text,{
    somplot_args$p.clus.col.text<-input$p.clus.col.text
  })
  observeEvent(ignoreInit = T,input$var_bg,{
    somplot_args$var_bg<-input$var_bg
  })
  observeEvent(ignoreInit = T,input$var_bg_transp,{
    somplot_args$var_bg_transp.alpha<-input$var_bg_transp
  })
  observeEvent(ignoreInit = T,input$pclus_points_palette,{
    somplot_args$pclus_points_palette<-input$pclus_points_palette
  })
  observeEvent(ignoreInit = T,input$insertx_pclus,{
    somplot_args$vals_module$insertx_pclus<-input$insertx_pclus
  })
  observeEvent(ignoreInit = T,input$inserty_pclus,{
    somplot_args$vals_module$inserty_pclus<-input$inserty_pclus
  })
  observeEvent(ignoreInit = T,input$ncol_pclus,{
    somplot_args$vals_module$ncol_pclus<-input$ncol_pclus
  })
  observeEvent(ignoreInit = T,input$bgleg_pclus,{
    somplot_args$vals_module$bgleg_pclus<-input$bgleg_pclus
  })
  observeEvent(ignoreInit = T,input$pclus_symbol,{
    somplot_args$pclus_symbol<-input$pclus_symbol
  })
  observeEvent(ignoreInit = T,input$dot_label_clus,{
    somplot_args$vals_module$dot_label_clus<-input$dot_label_clus
  })
  observeEvent(ignoreInit = T,input$varfacmap_action,{
    somplot_args$pclus_varfacmap_action<-input$varfacmap_action
  })
  observeEvent(ignoreInit = T,input$pclus_points_size,{
    somplot_args$pclus_points_size<-input$pclus_points_size
  })
  observeEvent(ignoreInit = T,input$pcodes_bgalpha,{
    somplot_args$pcodes_bgalpha<-input$pcodes_bgalpha
  })

  get_error<-reactive({
    #somplot_args<-args
    if(input$show_mapcode_errors=="None"){
      res<-NULL
    } else {
      #print(somplot_args$vals_module$cutsom)
      m<-get_model()
      #args<-readRDS("args_err.rds")
      #attach(args)
      #saveRDS(list(m=m,cutsom=somplot_args$vals_module$cutsom,cutsom_samples=somplot_args$vals_module$cutsom_samples),"args_err.rds")
      # vals<-list()
      #somplot_args$vals_module$cutsom<-args$cutsom
      # length(somplot_args$vals_module$cutsom)

      #length(m$grid[[1]][,1])
      x<-tapply(m$grid[[1]][,1],somplot_args$vals_module$cutsom, mean)
      y<-tapply(m$grid[[1]][,2],somplot_args$vals_module$cutsom, mean)
      errors<-somQuality4(m,somplot_args$vals_module$cutsom_samples)

      res<-data.frame(cbind(x,y,errors))
      colnames(res)[3:6]<-c("Quantization Error","Explained Variance","Topographic Error","Kaski-Lagus Errors")
      print(res)


    }
    res
  })
  geterror_plot<-reactive({
    errors<-get_error()
    errors
  })
  argsplot<-reactive({

    req(input$pcodes_bgalpha)
    if(isTRUE(input$pclus_addpoints)){
      req(input$pclus_points_factor)
      points_factor= NULL }
    if(isTRUE(input$pclus_addtext)){
      req(input$pclus_text_factor)
      text_factor= NULL }

    indicate=indicate_hc()


    m<-get_model()

    tryco<-try(copoints_scaled(), silent = T)

    req(!inherits(tryco,"try-error"))
    #savereac()
    trybp<-try( bp_som(), silent = T)

    req(!inherits(trybp,"try-error"))

    errors<-NULL




    copoints2<-somplot_args$copoints2
    copoints3<-copoints2
    #opoints2$point<-args$points_factor
    #attach(somplot_args$vals_module$args)


    args<-list(m=m,
               hexs=somplot_args$hc_network,
               points_tomap=somplot_args$hc_mapsom,
               bp=somplot_args$bp_som,
               points=input$pclus_addpoints,
               points_size=input$pclus_points_size,
               points_palette=input$pclus_points_palette,
               pch=as.numeric(input$pclus_symbol),
               text=input$pclus_addtext,
               text_size=input$pclus_text_size,
               text_palette=input$pclus_text_palette,
               bg_palette=input$bg_palette,
               newcolhabs=somplot_args$vals_module$newcolhabs,
               bgalpha=input$pcodes_bgalpha,
               border=input$pclus_border,
               indicate=indicate$indicate,
               cex.var=as.numeric(input$pclus.cex.var),
               col.text=input$p.clus.col.text,
               col.bg.var=input$var_bg,
               col.bg.var.alpha=1-input$var_bg_transp,
               show_error=errors,
               base_size=input$base_size,
               show_neucoords=input$theme,
               newdata=input$newdata,
               title=input$title,
               hc=somval$hc
    )

    args

  })
  observeEvent(argsplot(),{

    vals[[ns("somplot_args")]]<-argsplot()
    #args<-readRDS('args.rds')
    #saveRDS(argsplot(),"args.rds")
    #saveRDS(ns("somplot_args"),"name.rds")
    #readRDS('name.rds')


  })
  vals[[ns("somplot_args")]]
}



