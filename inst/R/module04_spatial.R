#' @noRd
#'
#' @importFrom gstat vgm gstat variogramLine
#' @importFrom webshot webshot
#' @importFrom geodist geodist
#' @importFrom dplyr summarise group_by across
#' @importFrom leaflet.minicharts popupArgs addMinicharts
#' @importFrom scatterpie geom_scatterpie
#' @importFrom raster crs rasterToPoints coordinates extent ratify raster rasterize `crs<-` values `values<-` `extent<-`
#' @importFrom sp spsample `coordinates<-` CRS `proj4string<-` `gridded<-` `fullgrid<-` proj4string SpatialGridDataFrame zerodist

plotly_download<-list()

plotly_download$ui<-function(id){
  ns<-NS(id)
  showModal(
    modalDialog(
      title="Download plotly",
      div(
        div(style="display: flex; gap: 10px;max-width: 600px",
            numericInput(ns("fheight"), "Height (px)", min=2,  step=1, value = 400),
            numericInput(ns("fwidth"), "Width (px)", min=2,  step=1, value = 400),
            numericInput(ns("fscale"), "Scale", step=1, value = 10),
            selectInput(ns("fformat"), "File type", choices=c("png","svg","jpeg")),
            div(style="margin-top: 25px",class="save_changes",
                actionButton(ns("plotly_download"),"Download"))


        )
      ),
      easyClose = T

    )
  )
}

plotly_download$server<-function(id,out_id,session_in){
  moduleServer(id,function(input,output,session){

    observeEvent(input$plotly_download,ignoreInit = T,{

      withProgress(min=NA,max=NA,message="Taking snapshop - this may take a few seconds... ",{
        shinyjs::runjs(paste0(
          "
      var plot = document.getElementById('",paste0(session_in,out_id),"');
      if (plot) {
      Plotly.downloadImage(plot, {
        format: '",input$fformat,"',
        filename: 'plot_snapshot',
        width: ",input$fwidth,",
        height: ",input$fheight,",
        scale: ",input$fscale,"  // Adjust the scale factor as needed
      });
    }
    "
        ))
        removeModal()
      })

    })

    output$teste<-renderUI({
      renderPrint({})
    })

  })
}
check_inputs<-function(input,pattern){
  res<-names(input)[grep(pattern,names(input))]
  res
}
ll_data<-list()
ll_data$ui<-function(id){
  ns<-NS(id)
  div(
    id=ns('map_setup'),
    box_caret(ns("box_setup"),
              title="Data setup",
              color="#374061ff",
              inline=F,
              div(

                div(class="inline_pickers2",
                    pickerInput_fromtop(ns("data_map"),"Datalist:",
                                choices =NULL,inline=T),
                    pickerInput_fromtop(ns("choices_map"),"Attribute:",
                                choices = c("Numeric-Attribute","Factor-Attribute"),inline=T),
                    pickerInput_fromtop(ns("var_map"),label = "Variable:",choices = NULL,inline=T),
                    pickerInput_fromtop(ns("factor_filter"),label = "Filter",choices = NULL,inline=T),
                    pickerInput_fromtop(ns("level_filter"),label = "Level",choices = NULL,inline=T)

                )

              )
    )
  )




}
ll_data$server<-function(id,vals){
  moduleServer(id,function(input, output, session) {

    observe({
      shinyjs::toggle('map_setup',condition = !vals$cur_mode_map%in%c("surface","stack"))
    })

    observeEvent(vals$saved_data,{
      selected<-NULL
      if(!is.null(vals$cur_data)){
        if(vals$cur_data%in%names(vals$saved_data)){
          selected<-vals$cur_data
        }
      }

      updatePickerInput(session,"data_map",choices=names(vals$saved_data), selected=selected)
    })


    observeEvent(input$data_map,{
      choices<-colnames(get_factors())
      selected<-NULL
      if(!is.null(vals$cur_factor_filter)){
        if(vals$cur_factor_filter%in%choices){
          selected<-vals$cur_factor_filter
        }
      }
      updatePickerInput(session,"factor_filter",choices=c("None",choices),selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(data0(),{

      choices<-colnames(data0())
      selected<-NULL
      if(!is.null(vals$cur_var_map)){
        if(vals$cur_var_map%in%choices){
          selected<-vals$cur_var_map
        }
      }
      updatePickerInput(session,"var_map",choices=choices, selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))

    })
    observeEvent(input$factor_filter,{
      fac<-get_filter()[,1]
      choices<-levels(fac)
      selected<-NULL
      if(!is.null(vals$cur_level_filter)){
        if(vals$cur_level_filter%in%choices){
          selected<-vals$cur_level_filter
        }
      }

      updatePickerInput(session,"level_filter",choices=choices, selected=selected)
    })

    data0<-reactive({
      req(input$data_map)
      req(input$data_map%in%names(vals$saved_data))
      req(input$choices_map)
      data0<-vals$saved_data[[input$data_map]]
      if(input$choices_map=='Factor-Attribute'){
        data_temp<-attr(data0,"factors")
        coords<-attr(data0,"coords")
        factors<-attr(data0,"factors")
        attr(data_temp,"factors")<-factors
        attr(data_temp,"coords")<-coords
        data0<-data_temp
      }
      data0
    })
    get_factors<-reactive({
      req(input$data_map)
      req(input$data_map%in%names(vals$saved_data))
      data0<-vals$saved_data[[input$data_map]]
      factors<-attr(data0,"factors")
      factors
    })
    get_filter<-reactive({
      req(input$data_map)
      req(input$data_map%in%names(vals$saved_data))
      data0<-vals$saved_data[[input$data_map]]
      factors<-get_factors()
      req(input$factor_filter%in%colnames(factors))
      fac<-factors[input$factor_filter]
      fac
    })

    observeEvent(input$data_map,ignoreInit = T,{
      vals$cur_data<-input$data_map
    })
    observeEvent(input$var_map,ignoreInit = T,{
      req(input$var_map!="")
      vals$cur_var_map<-input$var_map
    })
    observeEvent(input$factor_filter,ignoreInit = T,{
      vals$cur_factor_filter<-input$factor_filter
    })
    observeEvent(input$filter_level,ignoreInit = T,{
      vals$cur_level_filter<-input$filter_level
    })

    observe({
      toggle('level_filter',condition=input$factor_filter!="None")
    })
    data_inputs<-reactive({

      list(
        name=input$data_map,
        attr=input$choices_map,
        var=input$var_map,
        filter=input$factor_filter,
        filter_level=input$level_filter,
        saved_data=vals$saved_data
      )
    })
    observeEvent(input$choices_map,ignoreInit = T,{
      vals$cur_choices_map<-input$choices_map
    })


    observeEvent(data_inputs(),{
      saved_data<-vals$saved_data
      req(input$data_map%in%names(saved_data))
      req(input$var_map%in%colnames(data0()))
      args<-data_inputs()
      res<-do.call(get_data_map,args)
      req(nrow(res)>0)
      vals$data_map<-res
    })

    return(NULL)
  })
}

ll_colors<-list()
ll_colors$ui<-function(id){
  ns<-NS(id)
  div(

    div(
      tags$div(
        pickerInput_fromtop(inputId = ns("palette"),
                    label ="Palette",
                    NULL),
        numericInput(ns("fillOpacity"),'Fill opacity',min=0,max=1,0.8,step=0.1),
        numericInput(ns("light"),'Lightning',min=0,max=1,0,step=0.1)

      ),
      div(
        checkboxInput(ns("reverse_palette"),"Reverse palette",F)
      )
    )



  )

}
ll_colors$server<-function(id,vals){
  moduleServer(id,function(input, output, session) {

    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,"palette", choices =  vals$colors_img$val,
                        choicesOpt = list(
                          content =  vals$colors_img$img )
      )
    })

    return(
      reactive({
        list(pal=input$palette,fillOpacity=input$fillOpacity,reverse_palette=input$reverse_palette,light=input$light)
      })
    )
  })
}
ll_circles<-list()
ll_circles$ui<-function(id){
  ns<-NS(id)
  div(
    tags$div(id=ns("circle_options"),
             checkboxInput(ns("scale_radius"), "Scale radius",T)
    )
  )
}
ll_circles$server<-function(id,vals){
  moduleServer(id,function(input, output, session) {


    observeEvent(vals$cur_choices_map,{
      shinyjs::toggle('scale_radius',condition=vals$cur_choices_map!="Factor-Attribute")
    })



    return(
      list(

        scale_radius=input$scale_radius

      )
    )

  })
}
ll_radius<-list()
ll_radius$ui<-function(id){
  ns<-NS(id)
  div(class="inline_pickers2",style="display: flex;",
      numericInput(ns("min_radius") ,label ='Min',1),
      numericInput(ns("max_radius") ,label =" Max",5)
  )
}
ll_radius$server<-function(id){
  moduleServer(id,function(input, output, session) {
    return(
      list(
        min_radius=input$min_radius,
        max_radius=input$max_radius
      )
    )

  })
}
ll_breaks<-list()
ll_breaks$ui<-function(id){
  ns<-NS(id)
  div(
    tags$div(
      numericInput(ns("nbreaks"), span("Nº breaks",tiphelp("approximate number of breaks")),5, width="200px"))
  )
}
ll_breaks$server<-function(id){
  moduleServer(id,function(input, output, session) {
    return(
      list(
        nbreaks=input$nbreaks
      )
    )

  })
}
ll_pie<-list()
ll_pie$ui<-function(id){
  ns<-NS(id)
  div(
    div(inline(checkboxInput(ns("addMinicharts"),span("Pie Chart" ),T))),
    tags$div(id=ns("chart_options"),
             div(
               numericInput(ns("buffer_zize") ,
                            label =span("buffer (km)",tiphelp("Must be higher than 0")),
                            0.001)
             ),
             div(pickerInput_fromtop(ns("factor_chart") ,
                             label ="factor",
                             NULL))

    )
  )
}

ll_pie$server<-function(id){
  moduleServer(id,function(input, output, session) {

    observeEvent(input$addMinicharts,ignoreInit = T,{
      if(isTRUE(input$addMinicharts)){
        shinyjs::show('chart_options')
      } else{
        shinyjs::hide('chart_options')
      }
    })

    observeEvent(input$buffer_zize,ignoreInit = T,{
      if(input$buffer_zize<=0){
        updateNumericInput(session,"buffer_zize",value=0.00001)
      }
    })
    result<-reactive({
      list(
        addMinicharts=input$addMinicharts,
        factor_chart=input$factor_chart,
        buffer_zize=input$buffer_zize
      )
    })

    return(result())

  })
}
ll_shapes<-list()
ll_shapes$ui<-function(id,sf_in,base_on=T, surface=F,stack=F){

  ns<-NS(id)
  div(
    if(isFALSE(stack))
      div(id=ns("show_base"),
        box_caret(ns("box_base"),
                  color="#c3cc74ff",
                  title=span(style="display: inline-block",
                             class="checktitle",
                             checkboxInput(ns("base_shape") ,label =strong("Base Shape"),base_on,width="150px")
                  ),
                  div(
                    id=ns("base_options"),

                    if(isTRUE(surface))
                      uiOutput(ns("base_z_out")),

                    colourpicker::colourInput(ns('base_color'),"Background","whitesmoke"),

                    div(style="display: flex; align-items:  baseline ;  justify-content:flex-start",
                        checkboxInput(ns("base_fill") ,"Fill",T,width="40px"),
                        numericInput(ns("base_fillOpacity") ,label ="Opacity",1, min=0,max=1)


                    ),
                    div(id=ns('base_border'),
                        checkboxInput(ns("base_stroke") ,label ="Border",T,width="65px"),
                        div(style="padding-left: 20px",


                            colourpicker::colourInput(ns("base_border_color"),"Color","darkgray")
                            ,
                            numericInput(ns("base_weight"),'Width',min=0,0.5,step=.5)


                        )),
                  )
        )
      ),

    div(id=ns("show_layer"),
      box_caret(ns("box_layer"),
                color="#c3cc74ff",
                title=span(style="display: inline-block",
                           class="checktitle",
                           checkboxInput(ns("layer_shape") ,label =strong("Layer Shape"),base_on,width="150px")
                ),
                div(
                  id=ns("layer_options"),
                  if(isTRUE(surface))
                    uiOutput(ns('layer_z_out')),

                  colourpicker::colourInput(ns('layer_color'),"Background","#C0C2C1"),
                  div(style="display: flex; align-items:  baseline; ",
                      checkboxInput(ns("layer_fill") ,label ="Fill",T,width="40px"),
                      numericInput(ns("layer_fillOpacity") ,label ="Opacity",1, min=0,max=1)

                  ),
                  div(id=ns('layer_border'),
                      checkboxInput(ns("layer_stroke") ,label ="Border",T,width="65px"),
                      div(style="padding-left: 20px",


                          colourpicker::colourInput(ns("layer_border_color"),"Color","gray20")
                          ,
                          numericInput(ns("layer_weight"),'Width',min=0,0.5,step=.5)


                      ))
                )
      )
    )
  )
}

map_labels<-map_coords<-map_ggoptions<-list()
map_labels$ui<-function(id, surface=F,stack=F,scatter3d=F){
  ns<-NS(id)

  div(
    div(
      textInput(ns("main"),label="Title"),
      numericInput(ns("plot.title_size"),label="Title size",value=12),
      if(isFALSE(surface))
        div(
          textInput(ns("subtitle"),label="Subtitle"),
          numericInput(ns("plot.subtitle_size"),label="Subtitle size",value=11)
        ),
      textInput(ns("xlab"),label="x-label", "Longitude"),
      textInput(ns("ylab"),label="y-label","Latitude"),
      if(isTRUE(any(surface,stack,scatter3d)))
        textInput(ns("zlab"),label="z-label"),
      numericInput(ns("axis.title_size"),label="Label Size",value=11),
      numericInput(ns("axis.text_size"),label="Axis Size",value=11)

    )
  )
}
map_coords$ui<-function(id){
  ns<-NS(id)


  div(
    div(id=ns("coords_options"),
        colourpicker::colourInput(ns("col.coords"),label="Color",value="black"),
        numericInput(ns("cex.coords"),label="Size",value=3)

    )
  )
}
map_ggoptions$ui<-function(id){
  ns<-NS(id)


  div(
    numericInput(ns("legend.text_size"),label="Legend Text Size",value=13),
    checkboxInput(ns("show_guides"),label="Show Guides",value=F),
    numericInput(ns("keyscale"),label="North Arrow Size",value=30),
    numericInput(ns("width_hint"),label="Width Scale",value=0.15),
    numericInput(ns("cex_scabar"),label="Size Scalebar",value=0.7)
  )
}
map_labels$server_update<-function(id,vals,scatter3d=F){
  moduleServer(id,function(input,output,session){

    if(isTRUE(scatter3d)){
      updateTextInput(session,'zlab',value=colnames(vals$data_map))
    } else{
      updateTextInput(session,'zlab',value=attr(vals$cur_rst,"z_name"))
    }


    updateTextInput(session,'main',value=attr(vals$cur_rst,"z_name"))
    return(NULL)
  })
}

map_labels$server<-function(id){
  moduleServer(id,function(input,output,session){



    result<-list(
      main=input$main,
      plot.title_size=input$plot.title_size,
      subtitle=input$subtitle,
      plot.subtitle_size=input$plot.subtitle_size,
      axis.text_size=input$axis.text_size,
      axis.title_size=input$axis.title_size,
      xlab=input$xlab,
      ylab=input$ylab,
      zlab=input$zlab
    )
    return(result)

  })
}
map_coords$server<-function(id){
  moduleServer(id,function(input,output,session){



    result<-list(

      col.coords=input$col.coords,
      cex.coords=input$cex.coords)
    return(result)
  })
}
map_ggoptions$server<-function(id){
  moduleServer(id,function(input,output,session){


    result<-list(
      legend.text_size=input$legend.text_size,
      show_guides=input$show_guides,
      keyscale=input$keyscale,
      width_hint=input$width_hint,
      cex_scabar=input$cex_scabar
    )
    return(result)

  })
}


box_caret<-function(id,content=NULL,inline=T, class="train_box",click=T,title=NULL,button_title=NULL,tip=NULL,auto_overflow=F, color=NULL,show_tittle=T,hide_content=F){
  ns<-NS(id)

  id0<-strsplit(id,"-")[[1]]
  id0<-id0[length(id0)]
  if(is.null(color)){
    color=getcolorbox(id0)
  }
  background=paste0("background: ",color,";")
  # border=paste0("box-shadow: 0 0px 2px ","#303030ff","; ")
  color=paste0("color: ",color,";")
  style<-paste0(background)

  if(is.null(title))
    title<-getboxtitle(id0)
  if(isTRUE(inline)){
    class=paste("inline_pickers",class)
  }
  if(is.null(tip)){
    tip<-getboxhelp(id0,ns,click)
  }
  style_over='padding-top: 5px;'
  if(auto_overflow){
    style_over='padding-top: 5px; overflow:auto;'
  }
  if(isTRUE(hide_content)){
    hide_content<-"+"
  } else{
    hide_content="-"
  }
  div_title<-NULL
  if(isTRUE(show_tittle)){
    div_title<-div(class="box_title",
                   actionButton(ns("show_hide"),hide_content,style=style),
                   title,tip)
  } else{
    class='train_box ptop0'
  }

  div(class=class,
      #style=border,
      div_title,div(button_title,style="position: absolute; top: -1px;right: 0px; padding: 3px"),
      div(id=ns('content'),style=style_over,
          content
      )
  )
}
ll_shapes$server_update1<-function(id, data,sf_in,vals){
  moduleServer(id,function(input, output, session) {
    ns<-session$ns


    observe({
      shinyjs::toggle("layer_border",condition= !vals$cur_modeplot%in%"plotly")
      shinyjs::toggle("base_border",condition= !vals$cur_modeplot%in%"plotly")
      shinyjs::toggle("base_fill",condition= !vals$cur_mode_map%in%"surface")
      shinyjs::toggle("layer_fill",condition= !vals$cur_mode_map%in%"surface")


    })


    output$base_z_out<-renderUI({
      req(vals$cur_rst)
      z<-values(vals$cur_rst)
      numericInput(ns("base_z"),"Z",value=min(z,na.rm=T))
    })
    output$layer_z_out<-renderUI({
      req(vals$cur_rst)
      z<-values(vals$cur_rst)
      div(numericInput(ns("layer_z"),"Z",value=min(z,na.rm=T)))
    })

    observeEvent(input$layer_fill,{
      if(isFALSE(input$layer_fill)){
        shinyjs::hide('layer_fillOpacity')
      } else{
        shinyjs::show('layer_fillOpacity')
      }
    })

    observeEvent(input$base_fill,{
      if(isFALSE(input$base_fill)){
        shinyjs::hide('base_fillOpacity')
      } else{
        shinyjs::show('base_fillOpacity')
      }
    })

    observeEvent(input$layer_stroke,{
      if(isFALSE(input$layer_stroke)){
        shinyjs::hide('layer_weight')
      } else{
        shinyjs::show('layer_weight')
      }
    })

    observeEvent(input$base_stroke,{
      if(isFALSE(input$base_stroke)){
        shinyjs::hide('base_weight')
      } else{
        shinyjs::show('base_weight')
      }
    })



    observe({
      shinyjs::toggle('base_options', condition = isTRUE(input$base_shape))
      shinyjs::toggle('layer_options', condition = isTRUE(input$layer_shape))
    })



    observeEvent(vals$data_map,{
      data<-vals$data_map

      if(!is.null(data)) {
        base_shape<-attr(data,"base_shape")
        layer_shape<-attr(data,"layer_shape")
        if(is.null(base_shape)){
          updateCheckboxInput(session,'base_shape',value=F)
        }
        if(is.null(layer_shape)){
          updateCheckboxInput(session,'layer_shape',value=F)

        }
      }
    })


  })
}
ll_shapes$server<-function(id,vals=NULL){
  moduleServer(id,function(input, output, session) {


    observe({
      data<-vals$data_map
      cond_base<-length(attr(data,"base_shape"))>0
      cond_layer<-length(attr(data,"layer_shape"))>0
      shinyjs::toggle('show_base',condition = cond_base)
      shinyjs::toggle('show_layer',condition = cond_layer)
    })

    ns<-session$ns





    box_caret_server('box_base')
    box_caret_server('box_layer')


    observeEvent(input$base_shape,{
      shinyjs::toggle(selector=".base_options",condition=isTRUE(input$base_shape))

    })
    observeEvent(input$layer_shape,{
      shinyjs::toggle(selector=".layer_options",condition=isTRUE(input$layer_shape))

    })
    result<-reactive({
      list(
        base_shape_args=list(
          fillOpacity=input$base_fillOpacity,
          shape=input$base_shape,
          color=input$base_color,
          weight=input$base_weight,
          stroke=input$base_stroke,
          fill=input$base_fill,
          border_col=input$base_border_color,
          z=input$base_z
        ),
        layer_shape_args=list(
          fillOpacity=input$layer_fillOpacity,
          shape=input$layer_shape,
          color=input$layer_color,
          weight=input$layer_weight,
          stroke=input$layer_stroke,
          fill=input$layer_fill,
          border_col=input$layer_border_color,
          z=input$layer_z
        )
      )
    })


    return(result())

  })
}
ll_options<-list()
ll_options$ui<-function(id){
  ns<-NS(id)
  div(
    div(class="map_providers",

        selectInput(ns("providers"),
                    label ="+ Map Style",
                    selected="Stadia.StamenTerrain",
                    choices =c('OpenStreetMap.Mapnik',
                               'OpenStreetMap.DE',
                               'OpenStreetMap.France',
                               'OpenStreetMap.HOT',
                               'OPNVKarte',
                               'Stadia.AlidadeSmooth',
                               'Stadia.OSMBright',
                               'Stadia.Outdoors',
                               'Stadia.StamenWatercolor',
                               'Stadia.StamenTerrain',
                               'Stadia.StamenTerrainBackground',

                               'CyclOSM',

                               'Esri.WorldStreetMap',
                               'Esri.WorldTopoMap',
                               'Esri.WorldImagery',
                               'MtbMap',

                               'Esri.OceanBasemap',
                               'Esri.NatGeoWorldMap',
                               'Esri.WorldPhysical',
                               'USGS.USImagery'
                    ))
        # numericInput(ns("fillOpacity"),'+ Fill Opacity',min=0,max=1,0.8,step=0.1)



    )
  )

}
ll_options$server<-function(id){
  moduleServer(id,function(input, output, session) {
    return(
      reactive({
        list(
          providers=input$providers
        )
      })
    )
  })
}
ll_down_modal<-list()
ll_down_modal$ui<-function(id){
  ns<-NS(id)
  div(
    inline(numericInput(ns("zoom"),span('zoom', tiphelp("A number specifying the zoom factor. A zoom factor of 2 will result in twice as many pixels vertically and horizontally. ")), 1, width="100px")),
    inline(numericInput(ns("vwidth"),span('width', tiphelp("Viewport width. This is the width of the browser window")), 800, width="100px")),
    inline(numericInput(ns("vheight"),span('height',tiphelp("Viewport height This is the height of the browser window")), 600, width="100px")),
    inline(numericInput(ns("delay"),span('delay', tiphelp("Time to wait before taking screenshot, in seconds. Sometimes a longer delay is needed for all assets to display properly.")), 0.2, width="100px")),
    inline(downloadButton(ns("download"), icon("download"))),

  )

}
ll_down_modal$server<-function(id, map,file_name='leaflet_'){


  moduleServer(id,function(input, output, session) {
    output$download<-{
      downloadHandler(
        filename = function() {
          paste(file_name,Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          withProgress(message="Preparing...",min=NA,max=NA,{
            htmlwidgets::saveWidget(map, "map.html", selfcontained = FALSE)
            webshot::webshot("map.html", file = file, delay = input$delay,zoom=input$zoom,
                             vwidth=input$vwidth,
                             vheight=input$vheight)
          })

          file.remove("map.html")

        }
      )
    }
  })
}
ll_down_plot<-list()
ll_down_plot$ui<-function(id){
  ns<-NS(id)
  div(
    actionLink(ns("download_ll"),"Download",icon("download"))
  )

}
ll_down_plot$server<-function(id, map){
  moduleServer(id,function(input, output, session) {
    vplot<-reactiveValues(newggplot=NULL)
    ns<-session$ns

    observeEvent(input$download_ll,ignoreInit = T,{
      showModal(
        modalDialog(
          title='Download as png file',
          ll_down_modal$ui(ns("download")),
          easyClose = T
        )
      )
    })

    ll_down_modal$server("download",map)
  })
}
ll_down_gg<-list()
ll_down_gg$ui<-function(id){
  ns<-NS(id)
  div(
    actionButton(ns("download_gg"),span(icon("download")))
  )

}
ll_down_gg$server<-function(id, map){
  moduleServer(id,function(input, output, session) {
    vplot<-reactiveValues(newggplot=NULL)
    ns<-session$ns
    observeEvent(input$download_gg,ignoreInit = T,{
      vplot$hand_plot<-NULL
      module_ui_figs(session$ns("downfigs"))
      req(map)
      vplot$newggplot<-map
      vplot$hand_plot<-"ggplot map"

    })

    observeEvent(vplot$hand_plot,ignoreInit=T,{
      req(vplot$hand_plot)
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vplot)
    })



  })
}
ll_interp<-list()
ll_interp$ui<-function(id, coki=F){
  choices=list(
    'Krigging'="krige",
    'IDW'='idw',
    'KNN'='knn',
    'svmRadial'='svmRadial',
    'gaussprRadial'='gaussprRadial',
    'svmRadialCost'='svmRadialCost'
  )
  ns<-NS(id)
  div(class="radio_interp",style="padding: 5px;",

      div(style="display: flex",
          pickerInput_fromtop(ns("radio_interp_num"),"Method",choices, width="200px"),
          pickerInput_fromtop(ns("radio_interp_fac"),"Method",choices[-c(1:2)], width="200px"),
          uiOutput(ns("interp_help"))
      )
      ,

      tabsetPanel(id=ns("tabs_interp"),type="hidden",
                  tabPanel('Krigging',value="krige",
                           ll_vgm$ui(ns("vgm"))) ,
                  tabPanel('IDW',value="idw",
                           tags$div(id=ns("interp_params"),
                                    numericInput(ns("nmax"), lab_nmax, NA, min = NA),
                                    numericInput(ns("nmin"), lab_nmin, 3, min = 0),
                                    numericInput(ns("omax"), lab_omax, 0, min = 0),
                                    numericInput(ns("maxdist"), lab_maxdist, NA, min = 0),
                                    numericInput(ns("idp"), lab_idp, 4, min = 0)
                           )),
                  tabPanel('KNN',value="caret",
                           tags$div(
                             numericInput(ns("tuneLength"), 'tuneLength', 5, min = 0),
                           ))

      ),

      tags$div(id=ns('cv_inputs'),
               numericInput(ns("cv"), 'K-Folds', 5, min = 0),
               numericInput(ns("seed_caret"),span("Seed",tiphelp(textseed(),"right")),NA)),
      div(
        numericInput(ns("resolution"), lab_resolution, 5000, min = 20)
      ),
      uiOutput(ns("out_vgm")))

}
ll_interp$server<-function(id,vals){



  moduleServer(id,function(input, output, session) {

    data<-reactive({
      req(vals$data_map)
      vals$data_map
    })
    radio_interp<-reactive({

      data<-data()
      if(is.factor(data[,1])){
        input$radio_interp_fac
      } else{
        input$radio_interp_num
      }
    })
    output$interp_help<-renderUI({
      help_interp(radio_interp())
    })

    observe({
      shinyjs::toggle("cv_inputs",condition=radio_interp()!="idw")
    })

    observe({
      data<-data()
      cond<-is.factor(data[,1])

      shinyjs::toggle("radio_interp_num",condition=!cond)
      shinyjs::toggle("radio_interp_fac",condition=cond)
    })





    observeEvent(radio_interp(),{
      if(!radio_interp()%in%c("krige","idw")){
        selected<-"caret"
      } else{
        selected<-radio_interp()
      }
      updateTabsetPanel(session,'tabs_interp',selected=selected)
    })

    curtab<-reactiveVal()
    observeEvent(input$tabs_interp,{
      curtab(input$tabs_interp)
    })

    args<-reactive({
      list(
        nmax=input$nmax,
        nmin=input$nmin,
        omax=input$omax,
        maxdist=input$maxdist,
        idp=input$idp,
        tuneLength=input$tuneLength,
        seed=input$seed_caret,
        resolution=input$resolution,
        interp_type=radio_interp(),
        cv=input$cv
      )
    })
    observeEvent(args(),{
      req(data())
      vals$interp_args<-args()
      NULL
    })



    output$out_vgm<-renderUI({

      NULL
    })

    ll_vgm$server("vgm",vals)

    return(NULL)

  })
}

shapes_extra<-list()
shapes_extra$ui<-function(id,data_map=NULL){
  if(is.null(data_map)){
    return(NULL)
  }
  ns<-NS(id)
  shp<-attr(data_map,"extra_shape")
  shapes<-names(shp)
  req(length(shapes)>0)
  res<-list()
  #shapes=1:2
  for(i in 1:length(shapes)){
    atributos_shp<-attributes(shp[[i]])$names
    res[[i]]<-list(
      box_caret(ns("box_extra"),
                color="#c3cc74ff",
                title=span(style="display: inline-block",
                           class="checktitle",
                           checkboxInput(ns(paste0("map_extra_layer",i)),
                                         paste("Extra-Shapes",i), T,width="180px")
                ),
                tags$div(
                  tags$div(id=ns(paste0("eshape_id",i)),
                           colourpicker::colourInput(ns(paste0("ssextra_layer_col",i)),label="Color",value="gray40"),
                           numericInput(ns(paste0("ssextra_layer_lighten",i)),
                                        'Transp',value=0.6,  min=0, max=1,step=0.1),
                           pickerInput_fromtop(ns(paste0("feat_extra",i)),
                                       paste("Label",i),choices=c("None",atributos_shp)),
                           numericInput(ns(paste0("feat_extra_size",i)),
                                        'size',value=2,  min=0, max=1, step=0.1),
                           uiOutput(ns("data_depth"))
                  )


                )
      )

    )


  }

  div(
    res,
    tags$div(

      numericInput(ns("data_depth"),
                   span("Layer depth ",tiphelp("Control the layering order of your data relative to shapes")),
                   value=3,
                   min=1, step=1
      )
    )
  )

}
shapes_extra$server<-function(id,data_map){
  moduleServer(id,function(input,output,session){


    box_caret_server("box_extra")

    extralayers<-NULL
    shapes<-attr(data_map,"extra_shape")
    if(length(names(shapes))>0){
      extralayers<-lapply(seq_along(shapes),function(i){

        layers=input[[paste0("map_extra_layer",i)]]
        if(isTRUE(layers)){
          shinyjs::show(paste0("eshape_id",i))
        } else{
          shinyjs::hide(paste0("eshape_id",i))
        }
        c(
          layers=layers,
          colors=input[[paste0("ssextra_layer_col",i)]],
          alphas=input[[paste0("ssextra_layer_lighten",i)]],
          labels=input[[paste0("feat_extra",i)]],
          sizes=input[[paste0("feat_extra_size",i)]]
        )
      })
      extralayers<-do.call(rbind,extralayers)
    }

    return(list(
      args_extra=extralayers,
      data_depth=input$data_depth
    ))


  })
}


gg_control<-list()
gg_control$ui<-function(id) {
  ns<-NS(id)

  div(
    div(class="nav_map well",

        div(
          div(
            style="height: calc(100vh - 315px); overflow-y: auto",

            textInput(ns("main"),label="Title"),

            numericInput(ns("plot.title_size"),label="Size",value=13),
            textInput(ns("subtitle"),label="Subtitle"),
            numericInput(ns("plot.subtitle_size"),label="Size",value=13),
            numericInput(ns("axis.text_size"),label="Axis text Size",value=13)
            ,
            numericInput(ns("axis.title_size"),label="Axis title Size",value=13),



            checkboxInput(ns("show_coords"),label="Show Coordinates",value=FALSE),
            div(id=ns("show_coords_on"),

                colourpicker::colourInput(ns("col.coords"),label="Color",value="black"),
                numericInput(ns("cex.coords"),label="Size",value=3)
            ),
            numericInput(ns("legend.text_size"),label="Legend Text Size",value=13),
            checkboxInput(ns("show_guides"),label="Show Guides",value=F),
            numericInput(ns("keyscale"),label="North Arrow Size",value=30),
            numericInput(ns("width_hint"),label="Width Scale",value=0.15),
            numericInput(ns("cex_scabar"),label="Size Scalebar",value=0.7)
          )
        )

    )
  )
}



showlabels<-list()
showlabels$ui<-function(id,surface=F){
  ns<-NS(id)
  box_caret(ns("box_labels"),
            color="#c3cc74ff",
            title=span(style="display: inline-block",class="checktitle",checkboxInput(ns("show_labels"),strong("Text"), F,width="100px")),
            div(

              tags$div(id=ns("show_labels_on"),style="display: none",
                       if(isTRUE(surface)){
                         div(

                           pickerInput_fromtop(ns("datalist"),"Datalist:",
                                       choices =NULL),
                           uiOutput(ns("z_label_value"))

                         )
                       },
                       div(
                         pickerInput_fromtop(ns("labels"),label="Use factor",choices=c(NULL))
                       ),
                       div(
                         numericInput(ns("cex.fac"),label="Size",value=5)
                       ),
                       div(
                         colourpicker::colourInput(ns("col.fac"),label="Color",value="black")
                       )
              )
            )

  )

}
showlabels$server_update<-function(id,vals,selected_labels=NULL,surface=F){
  moduleServer(id,function(input,output,session){

    ns<-session$ns

    output$z_label_value<-renderUI({
      req(input$datalist)
      data<-vals$saved_data[[input$datalist]]

      choices<-colnames(data)
      selected=vals$cur_surf_ap_vars
      selected=get_selected_from_choices(selected,choices)

      pickerInput_fromtop(ns("z_label_value"),"Z-Value:",
                  choices =choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })

    observeEvent(vals$saved_data,{
      choices<-names(vals$saved_data)
      selected=vals$cur_surf_ap_datalist
      selected=get_selected_from_choices(selected,choices)

      updatePickerInput(session,"datalist",choices=choices, selected=selected)
    })
    observeEvent(input$show_labels,{
      if(isTRUE(input$show_labels)){
        shinyjs::show('show_labels_on')
      } else{
        shinyjs::hide('show_labels_on')
      }
    })


    observeEvent(vals$data_map,{
      req(isFALSE(surface))
      factors<-attr(vals$data_map,"factors")

      selected<-NULL
      if(!is.null(selected_labels)){
        selected=selected_labels
        choices<-colnames(factors)
        if(!is.null(selected)) {if(!selected %in% choices){
          selected<-NULL
        }}
      }
      updatePickerInput(session,'labels',choices=colnames(factors),selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))

    })






    return(
      reactive({
        list(
          labels=input$labels
        )
      })
    )
  })
}
showlabels$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    observeEvent(input$datalist,{
      data<-vals$saved_data[[input$datalist]]

      factors<-attr(data,'factors')
      choices=colnames(factors)
      selected=vals$cur_surf_ap_vars
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'labels',choices=choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })

    return(
      reactive({
        list(
          show_labels=input$show_labels,
          labels=input$labels,
          cex.fac=input$cex.fac,
          col.fac=input$col.fac,
          datalist=input$datalist,
          z_value=input$z_label_value
        )
      })
    )
  })
}


ll_vgm<-list()
ll_vgm$ui<-function(id){

  models<-gstat::vgm()
  models<-models[c(2:nrow(models),1),][1:17,]
  models[,2]<-  gsub(".*\\(|\\)","",models[,2])
  choices<-sapply(1:nrow(models),function(i){
    val<-as.character(models[i,1])
    names(val)<-as.character(models[i,2])
    val
  })


  ns<-NS(id)
  tags$div(
    div(
      div(
        pickerInput_fromtop(ns("model"), span("Model type",help_autofit()),choices, selected="Gau",width="250px")
      ),
      class="gstat",
      div(style="font-size: 12px; ",
          div(
            div(
              class="side_vgm",
              div(style="display: flex",
                  div(class="switch_box",switchInput(ns("vgm_autofit"),"Variogram autofit",value=T,size="mini",labelWidth="100px")),
                  actionLink(ns("run_vgm_autofit"),">>RUN>>",style="margin-top: 5px; margin-left: -20px")
              ),
              div(

                numericInput(ns("spsample"),span(
                  "Subsample",tipright("Specify the number of spatial sample points. Leave empty to bypass subsampling. Note: Autofitting the variogram may be computationally intensive for large datasets, making subsampling advisable.")
                ),NA)
              ),
              div(
                id=ns("vgm_fine_btn"),class="save_changes",style="padding-top: 0px;",
                div(
                  style="padding-left: 0px",
                  actionLink(ns("vgm_fine"),tags$label("+ Show Variogram tuning", style="cursor: pointer;"))


                )
              ),
              tags$div(
                style="margin-left: 50px",
                id=ns("vgm_fine_params"),

                numericInput(ns("nugget"), "nugget",5),
                numericInput(ns("psill"), "psill",5, step=0.01),
                numericInput(ns("range"), "range",5),
                numericInput(ns("kappa"), span("kappa", tiphelp("smoothness parameter for the Matern class of variogram models")),.5)
              )
            )
          )

      )
    )
  )
}
ll_vgm$server<-function(id,vals){
  moduleServer(id,function(input, output, session) {

    observe(shinyjs::addClass('fit_vgm',"save_changes"))

    data<-reactive({
      req(vals$data_map)
      vals$data_map
    })

    run_autofit<-reactiveVal()

    observeEvent(input$run_vgm_autofit,ignoreInit = T,{
      run_autofit(TRUE)
    })

    observeEvent(input$vgm_autofit,{
      run_autofit(input$vgm_autofit)
    })

    observeEvent(data(),{

      if(nrow(vals$data_map)>10000){
        run_autofit(FALSE)
        updateNumericInput(session,'spsample',value=10000)
        shinyWidgets::updateSwitchInput(session,'vgm_autofit',value=FALSE)
      } else{
        updateNumericInput(session,'spsample',value=NA)
        # shinyWidgets::updateSwitchInput(session,'vgm_autofit',value=T)

      }
    })




    observe({
      shinyjs::toggle('vgm_fine_params',condition=input$vgm_fine%%2)
    })

    variogram_parameters<-function(df,model="Gau",var=1, log=F, g=NULL){



      log_params=NULL
      if(isTRUE(log)){
        log_var<-decostand(df[,var],"log")
        log_params<-attr(log_var,"parameters")
        df[,var]<-log_var
      }
      coords<-attr(df,"coords")[rownames(df),]
      colnames(coords)<-c("x","y")
      z<-df[var]
      if(!is.null(g)){
        if(length(g$data)>0){
          newnames<-make.unique(c(names(g$data),var))
          var<-newnames[length(newnames)]
          colnames(z)<-var
        }
      }
      dd<-cbind(coords,z)
      formula<-as.formula(paste(var,"~ 1"))
      dsp<-to_spatial(dd)



      if(nrow(sp::zerodist(dsp))>0){
        dsp <- dsp[-sp::zerodist(dsp)[,1],]
      }
      TheVariogram=NULL

      auto_log<-capture_log1(automap::autofitVariogram)(formula, dsp,model=model)
      auto_z1<-auto_log[[1]]
      vgm_z1<-auto_z1[[1]]
      model_z1<-auto_z1[[2]]
      result<-list(variogram=vgm_z1,auto_z1=auto_z1, formula=formula,dsp=dsp, id=var, log=log,log_params=log_params)
      attr(result,"logs")<-auto_log[[2]]
      return(result)
    }

    get_autofit<-reactive({
      req(is.numeric( vals$data_map[,1]))
      req(isTRUE(run_autofit()))
      data<-data()


      vals$interp_vc<-NULL

      if(!is.na(input$spsample)){
        if(input$spsample<nrow(data()))

          data<-data[sample(1:nrow(data),input$spsample),,drop=F]
      }

      args1<-list(df=data,model=input$model,var=colnames(data),g=NULL)
      withProgress({
        var=colnames(data)

        vgm_result<-do.call(variogram_parameters,args1)

        #vgm_result<-do.call(variogram_parameters,args1)
      },min=NA,max=NA,message="Running autofit...")
      #vals$vgm_error<-attr(vgm_result,"logs")
      vgm_result


    })

    observe({

      vgm_result<-get_autofit()
      vals$interp_vc<-vgm_result
      nug<-vgm_result$auto_z1$var_model[1,2]
      ps<-vgm_result$auto_z1$var_model[2,2]
      ra<-vgm_result$auto_z1$var_model[2,3]
      req(is.numeric(nug))
      req(is.numeric(ps))
      req(is.numeric(ra))
      nugget =round(nug,3)
      psill=round(ps,3)
      range=round(ra,3)
      updateNumericInput(session,'nugget',value =nugget )
      updateNumericInput(session,'psill',value =psill )
      updateNumericInput(session,'range',value =range )

    })







    vgm_args<-reactive({

      list(
        psill=input$psill, model=input$model, nugget=input$nugget, range=input$range, kappa = input$kappa
      )
    })
    get_krige_model<-reactive({
      vc<-vals$interp_vc
      req(vc)
      vgm_model<-try(do.call(vgm,vgm_args()))
      g<-try({gstat(NULL,id=vc$id,form= vc$formula,data= vc$dsp,model=vals$interp_vgm)})
      list(vgm_model,g)
    })


    observeEvent(get_krige_model(),{
      vals$interp_vgm<-get_krige_model()[[1]]
      vals$interp_gstat<-get_krige_model()[[2]]
    })


    return(NULL)

  })
}
plot_variogram <- function(v, m, sized=F, show_vgm_lines=T) {
  preds = variogramLine(m, maxdist = max(v$dist))
  g<-ggplot()
  if(isTRUE(sized)){
    g<-g+geom_point(data = v, aes(x = dist, y = gamma, size=np))
  } else{
    g<-g+geom_point(data = v, aes(x = dist, y = gamma))
  }

  g<-g+geom_line(data = preds, aes(x = dist, y = gamma))+
    xlab("Distance")+ ylab("Semivariance")



  g
}


ll_map<-list()
ll_map$ui<-function(id, circles=F, pie=F, radius=F,raster=F,interp=F, coki=F, shapes=T,surface=F,stack=F,scatter3d=F){
  sf_in=any(c(interp,coki))
  ns<-NS(id)
  title_colors<-"Colors"
  if(isTRUE(surface)){
    title_colors<-"Z-Control"
  }
  div(
    class="inline_pickers",
    div(
      column(
        4,class="mp0",style="height: calc(100vh - 200px);overflow-y: auto",

        div(
          if(isTRUE(interp)|raster){
            box_caret(
              ns("box_interp"),
              color="#c3cc74ff",
              title="Model",
              div(
                if(isTRUE(raster)){
                  div(
                    numericInput(ns("raster_resolution"),span("Resolution",tipright("Define the resolution of the raster. Higher values will result in a coarser raster with less detail, while lower values will produce a finer raster with more detail.")),0.1,step=0.01),
                    checkboxInput(ns("down_grade_raster"),span("Aggregate resolution",tipright("Downscales the final raster by the specified factor. This reduces the resolution, aggregating the data by a specified factor.")))
                  )
                }
                ,
                div(style="padding-left: 25px",
                    numericInput(ns("down_grade"),"Factor",3)
                ),
                if(isTRUE(interp)){
                  div(
                    ll_interp$ui(ns('interp1'), coki=coki),
                    uiOutput(ns("interp1_out"))
                  )
                }

              )

            )
          },
          if(isTRUE(surface)){
            box_caret(
              ns("surface_savedmaps"),
              title="Saved maps",
              color="#c3cc74ff",
              uiOutput(ns('surface_saved_maps'))
            )
          },
          if(isTRUE(stack)){
            box_caret(
              ns("stack_control"),
              title="Saved maps",
              color="#c3cc74ff",
              div(
                uiOutput(ns('stack_layers'))
              )
            )
          },
          box_caret(ns("box_color"),
                    color="#c3cc74ff",
                    title=title_colors,
                    div(
                      ll_colors$ui(ns('color1')),
                      div(id=ns("show_breaks"),

                          ll_breaks$ui(ns('breaks1')),
                          uiOutput(ns("custom_breaks"))
                      )
                    )),
          if(isTRUE(surface)) {

            div(
              box_caret(
                ns("surface_Points"),

                title=span(style="display: inline-block",class="checktitle",checkboxInput(ns("surf_addpoints"),strong("Points"), F,width="100px")),

                color="#c3cc74ff",
                div(style="padding-left: 10px",
                    uiOutput(ns("surf_ap_datalist"))
                )

              )
            )

          },

          if(isTRUE(circles)|isTRUE(pie))
            box_caret(ns("box_circles"),
                      color="#c3cc74ff",
                      title=inline(uiOutput(ns("box_circles_title"))),

                      div(
                        if(isTRUE(circles)){
                          ll_circles$ui(ns('circles1'))
                        },
                        if(isTRUE(radius)){
                          ll_radius$ui(ns('radius1'))
                        },
                        if(isTRUE(pie)){
                          div(

                            ll_pie$ui(ns("pie1")),
                            uiOutput(ns("pie_out"))
                          )
                        },
                      )

            ),
          uiOutput(ns('scatter3d_points')),
          if(isFALSE(stack))
            showlabels$ui(ns('leaflet_label_map'),surface),
          uiOutput(ns('surface_3d_control')),

          if(isFALSE(scatter3d))
            div(
              id=ns("shapes_leaflet"),
              ll_shapes$ui(ns("shapes1"),sf_in,base_on=F, surface=surface,stack=stack)
            ),

          div(id=ns("map_labels_id"),
              box_caret(
                ns("box_map_labels"),
                color="#c3cc74ff",
                title="Title and Labels",
                hide_content=T,
                div(map_labels$ui(ns("map_labelss"), surface,stack,scatter3d))

              )
          ),
          div(id=ns("map_coords"),
              box_caret(
                ns("box_coords"),
                color="#c3cc74ff",
                title=span(style="display: inline-block",class="checktitle",checkboxInput(ns("show_coords"),label=strong("Coordinates"),value=FALSE,width="150px")),
                hide_content=F,
                div(
                  id=ns('coords_options'),
                  map_coords$ui(ns("map_coords")))

              )

          ),
          div(id=ns("map_ggoptions"),
              box_caret(
                ns("box_ggoptions"),
                color="#c3cc74ff",
                title="Additional options",
                hide_content=T,
                div(
                  map_ggoptions$ui(ns("map_ggoptions"))
                )

              )
          ),
          uiOutput(ns('extraLayers_output'))

        )
      ),
      column(
        8,class="mp0",
        div(style="position: absolute; right:0px; top: 30px ",
        ),
        box_caret(ns("boxmap"),
                  title="Plot Engenier:",
                  button_title=div(
                    div(id=ns("down_btn_ll"),
                        ll_down_plot$ui(ns("save_png"))),
                    hidden(div(id=ns("down_btn_plotly"),
                               actionLink(ns("down_plotly"),"Download",icon("download")))),

                    hidden(div(id=ns("down_btn_gg"),
                               actionLink(ns("down_ggplot"), "Download",icon("download")))),
                    hidden(div(id=ns("down_btn_generic"),
                               tipify(actionLink(ns("down_plot3D_btn"), "Download",icon("download")),"Download")
                    ))

                  ),
                  fluidRow(div(style="margin-top:-30px;margin-left: 130px",
                               tags$style(HTML(
                                 "
                .navmap2 .navbar{

                width: fit-content
                }
                .navmap2 .container-fluid {

                margin-left: -130px;
                width: 120vh
                }

                .navmap2 .navbar, .navmap2  .navbar li,.navmap2  .navbar li a,.navmap2 ul {
            height: 26px;
            min-height: 26px;
            max-height: 26px;

            }"
                               )),
            div(
              class="navmap2",
              navbarPage(
                id=ns("mode_plot"),

                header=div(
                  div( style="display: flex;",
                       div(
                         class="save_changes",

                         id=ns("run_map_btn"),
                         actionButton(ns("run_map"),
                                      span(icon("angles-right"),icon("map")))
                       ),
                       div(style="padding: 10px",
                           emgray(icon("fas fa-hand-point-right"),"Click to Create the map")
                       ),
                       div(style="position: absolute; right: 2px",
                           tipify(uiOutput(ns("save_map_btn")),"Save the Raster for using in Surface and Stack plots"),
                           uiOutput(ns("save_geotiff"))
                       )

                  ),
                  uiOutput(ns("vgm_error"))
                ),

                title=NULL,
                if(isFALSE(any(surface,stack,scatter3d)))
                  tabPanel("Leaflet",value="leaflet",

                           span(id=ns("zoom_leaflet_btn"),style="display: none",
                                inline(ll_options$ui(ns("options1"))),
                                actionButton(ns("zoom_leaflet"),icon("magnifying-glass-plus"))
                           ),
                           bsTooltip(ns('zoom_leaflet'),"Shows the Leafmap map in a modal Window"),

                           leaflet::leafletOutput(ns("plot_from_leaflet"))),
                if(isFALSE(any(surface,stack,scatter3d)))
                  tabPanel(
                    "GGplot",value="ggplot",
                    div( plotOutput(ns("plot_from_ggplot")))

                  ),

                if(isTRUE(any(surface,stack,scatter3d)))
                  tabPanel("plotly",value="plotly",
                           div(id=ns("plotly_page"),
                               uiOutput(ns('plotly_validate')),
                               uiOutput(ns('plotly_surface')),
                               uiOutput(ns('plotly_stack')),
                               uiOutput(ns('plotly_scatter3d'))
                           )
                  ),
                if(isTRUE(any(surface,stack)))
                  tabPanel("plot3D",value="plot3D",
                           uiOutput(ns('plot3D_validate')),
                           uiOutput(ns('plot3D_surface')),
                           uiOutput(ns('plot3D_stack'))

                  ),
              )
            )



                  )))

      )
    )
  )
}
ll_map$server<-function(id, raster=F, interp=F, coki=F,pie=F,circles=F,vals,surface=F,stack=F,scatter3d=F){
  moduleServer(id,function(input, output, session) {
    ns<-session$ns

    plotly_installed<-reactiveVal(is_installed("plotly"))
    plot3D_installed<-reactiveVal(is_installed("plot3D"))


    output$save_geotiff<-renderUI({
      req(rst())
      tipify(downloadLink(session$ns("down_geotif"), span(icon("download"),"GeoTiff")),"Download the GeoTiff file with georeferenced raster data for GIS applications")
    })

    output$down_geotif<-downloadHandler(
      filename = "raster.tif",
      content = function(file) {
        raster::writeRaster(rst(), file,format="GTiff")
      }
    )
    output$plotly_validate<-renderUI({
      req(!plotly_installed())
      div(
        div(
          emgray("You need to install the 'plotly' package to continue")
        ),
        actionButton(ns("install_plotly"),"Install")
      )


    })

    output$plot3D_validate<-renderUI({
      req(!plot3D_installed())
      div(
        div(
          emgray("You need to install the 'plot3D' package to continue")
        ),
        actionButton(ns("install_plot3D"),"Install")
      )


    })

    observeEvent(input$install_plot3D,{
      install.packages("plot3D")
      plot3D_installed(TRUE)

    })
    observeEvent(input$install_plotly,{
      install.packages("plotly")
      plotly_installed(TRUE)

    })

    output$plotly_surface<-renderUI({

      req(isTRUE(plotly_installed()))
      plotly::plotlyOutput(ns('plotly_surface_out'))
    })

    observe({
      req(isTRUE(plotly_installed()))
      output$plotly_surface_out<-plotly::renderPlotly({
        run_map_surface_plotly()
      })
    })


    output$plotly_stack<-renderUI({

      req(isTRUE(plotly_installed()))
      plotly::plotlyOutput(ns("plotly_stack_out"))

    })

    output$plotly_scatter3d<-renderUI({

      req(isTRUE(plotly_installed()))
      plotly::plotlyOutput(ns("plotly_scatter3d_out"))

    })
    observe({
      req(isTRUE(plotly_installed()))
      output$plotly_scatter3d_out<-plotly::renderPlotly({
        run_map_scatter3d_plotly()
      })
    })


    ###
    observeEvent(input$s3d_points_zcolor,{
      vals$cur_s3d_points_zcolor<-input$s3d_points_zcolor
    })
    observeEvent(input$s3d_points_z_scale_datalist,{
      vals$cur_s3d_points_z_scale_datalist<-input$s3d_points_z_scale_datalist
    })
    observe({
      shinyjs::toggle("s3d_points_z_scale_args",condition=isTRUE(input$s3d_points_scale))
    })

    observeEvent(input$s3d_points_zdatalist,{
      vals$cur_s3d_points_zdatalist<-input$s3d_points_zdatalist
    })

    observeEvent(input$s3d_zvariable,{
      vals$cur_s3d_zvariable<-input$s3d_zvariable
    })
    output$s3d_points_zcolor<-renderUI({
      req(input$s3d_zvariable!="Z-Value")
      req(input$s3d_points_zdatalist)
      data<-vals$saved_data[[input$s3d_points_zdatalist]]
      if(input$s3d_zvariable=="4th variable (factor)"){
        data<-attr(data,"factors")
      }
      choices2<-colnames(data)
      selected2=vals$cur_s3d_points_zcolor
      selected2=get_selected_from_choices(selected2,choices2)
      selectInput(ns("s3d_points_zcolor"),"Variable", choices2,selected=selected2,options=shinyWidgets::pickerOptions(liveSearch=T) )
    })


    s3d_points_color_data<-reactive({
      data<-vals$saved_data[[input$s3d_points_zdatalist]]
      if(input$s3d_zvariable=="4th variable (factor)"){
        data<-attr(data,"factors")
      }
      req(input$s3d_points_zcolor%in%colnames(data))
      zdata<-data[rownames(vals$data_map),input$s3d_points_zcolor]
      attr(zdata,"name")<-input$s3d_points_zcolor
      zdata

    })

    observe({
      shinyjs::toggle("s3d_points_zdatalist",condition=input$s3d_zvariable!="Z-Value")
    })
    output$s3d_points_z_scale_out<-renderUI({

      div(style="padding-left: 10px;display: none",id=ns("s3d_points_z_scale_args"),
          uiOutput(ns("s3d_points_z_scale_datalist_out")),

          uiOutput(ns("s3d_points_z_scale_vars")),
          numericInput(ns('s3d_points_minsize'),"Min-size",0.5)
      )
    })

    output$s3d_points_z_scale_datalist_out<-renderUI({
      choices3=names(vals$saved_data)

      pic<-which(sapply(choices3,function(x){
        all(rownames(vals$saved_data[[x]])%in%rownames(vals$data_map))
      }))
      choices3<-choices3[pic]
      selected3=vals$cur_s3d_points_z_scale_datalist
      selected3=get_selected_from_choices(selected3,choices3)
      pickerInput_fromtop(ns("s3d_points_z_scale_datalist"),"Datalist", choices3,selected=selected3)
    })




    output$s3d_points_zdatalist_out<-renderUI({
      data<-vals$data_map

      name_data<-attr(vals$data_map,'args')$name
      req(name_data)
      choices1<-c(names(vals$saved_data))
      pic<-which(sapply(choices1,function(x){
        all(rownames(vals$saved_data[[x]])%in%rownames(vals$saved_data[[name_data]]))
      }))
      choices1<-choices1[pic]
      selected1=vals$cur_s3d_points_zdatalist
      selected1=get_selected_from_choices(selected1,choices1)
      selectInput(ns("s3d_points_zdatalist"),"Datalist", choices1,selected=selected1 )
    })
    output$scatter3d_points<-renderUI({
      req(isTRUE(scatter3d))




      box_caret(
        ns("s3d_points"),
        title="Points",
        color="#c3cc74ff",
        div(

          selectInput(ns("s3d_zvariable"),"Z-Color", c("Z-Value","4th variable (numeric)","4th variable (factor)")),
          uiOutput(ns('s3d_points_zdatalist_out')),
          uiOutput(ns('s3d_points_zcolor')),

          numericInput(ns('s3d_points_size'),"Size",1.5),
          checkboxInput(ns("s3d_points_scale"),"Scale Size", F ),
          uiOutput(ns('s3d_points_z_scale_out'))


        )
      )
    })
    observeEvent(input$s3d_points_z_scale_vars,{
      vals$cur_s3d_points_z_scale_vars<-input$s3d_points_z_scale_vars
    })
    output$s3d_points_z_scale_vars<-renderUI({
      req(input$s3d_points_z_scale_datalist)
      choices<-colnames(vals$saved_data[[input$s3d_points_z_scale_datalist]])
      selected=vals$cur_s3d_points_z_scale_vars
      selected=get_selected_from_choices(selected,choices)

      pickerInput_fromtop(ns("s3d_points_z_scale_vars"),"Z-Value", choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T) )
    })

    get_plot3Dpoints<-reactive({
      data<-vals$data_map
      coords<-attr(data,"coords")
      factors<-attr(data,"factors")
      req(coords)
      z=data[,1]
      points<-data.frame(coords,z)
      colnames(points)<-c("x","y","z")
      points$z_name<-colnames(data)
      req(color_args1()$pal)
      pal<-vals$newcolhabs[[color_args1()$pal]]
      fillOpacity<-color_args1()$fillOpacity
      light<-color_args1()$light
      col_zvalue<-col_fac<-z
      points$color_var<-colnames(vals$data_map)
      if(input$s3d_zvariable!="Z-Value"){
        col_zvalue<-col_fac<-s3d_points_color_data()
        points$color_var<-attr(col_zvalue,"name")
      }
      n_colors<-if(is.factor(col_fac)){nlevels(col_fac)}else{5}
      if(is.numeric(col_fac)){
        col_fac<-cut(col_fac,5)
      }
      colors<-pal(n_colors)
      reverse_palette<-color_args1()$reverse_palette
      if(isTRUE(reverse_palette)){
        colors<-rev(colors)
      }
      colors<-lighten(colors,light)
      col_poins<-colors[col_fac]
      points$color<-col_poins
      points$col_factor<-col_fac
      points$col_zvalue<-col_zvalue
      points$size_value<-z
      points$size<-input$s3d_points_size
      points$size_var<-colnames(vals$data_map)

      if(isTRUE(input$s3d_points_scale)){
        req(input$s3d_points_z_scale_datalist)
        data_size<-vals$saved_data[[input$s3d_points_z_scale_datalist]]

        req(input$s3d_points_z_scale_vars%in%colnames(data_size))
        tosize<-data_size[,input$s3d_points_z_scale_vars]
        points$size_value<-tosize
        # print(class(tosize))
        newsize<-scales::rescale(tosize,c(input$s3d_points_minsize,input$s3d_points_size))


        points$size<-newsize
        points$size_var<-input$s3d_points_z_scale_vars
      }

      points
    })


    get_plot3Dtext<-reactive({
      al<-args_labels()
      if(isFALSE(al$show_labels)){
        return(NULL)
      }
      data<-vals$data_map
      coords<-attr(data,"coords")
      factors<-attr(data,"factors")
      req(coords)
      z=data[,1]
      labels<-data.frame(coords,z)
      colnames(labels)<-c("x","y","z")
      labels$labels<-factors[,al$labels]
      labels$size<-al$cex.fac
      labels$color<-al$col.fac
      labels

    })



    ##

    scatter_3dplotly<-function(points,text,shape_args=NULL,xlab="x",ylab="y",zlab="z
",plot.title_size=12,axis.text_size=12,axis.title_size=12,main="",...){

      p <- plotly::plot_ly( points,    showlegend = T)
      lpoints<-split(points,points$col_factor)
      p1<-p


      for(i in seq_along(lpoints)) {
        pts<-lpoints[[i]]
        hover_text <- paste0(
          paste(pts$z_name[1],pts$z,sep=":"),
          "\n",
          paste(pts$size_var[1],pts$size_value,sep=":"),
          "\n",
          paste(pts$color_var,pts$col_zvalue,sep=":")
        )
        p1<-plotly::add_trace(
          p1,
          data = pts,
          x = pts$x,
          y =pts$y,
          z=pts$z,
          type = 'scatter3d',
          mode = 'markers',
          name= names(lpoints)[[i]],
          marker=list(

            size=pts$size*5,
            color = pts$color ,
            line = list(
              width = 0
            )
          ),
          text = hover_text, # Add custom hover text
          hoverinfo = 'text' # Specify that hover info should include custom text


        )


        #shape_args$base_shape_args$z<-min(pts$z)
        # shape_args$layer_shape_args$z<-min(pts$z)
        # p1<-add_shape_plotly(p1,vals$data_map,shape_args$base_shape_args,which_shape="base_shape")
        # p1<-add_shape_plotly(p1,vals$data_map,shape_args$layer_shape_args,which_shape="layer_shape")
      }


      if(!is.null(text)) {
        p1<-plotly::add_trace(
          p1,
          data = text,
          x = text$x,
          y =text$y,
          z=text$z,
          type = 'scatter3d',
          mode = 'text',
          text=text$labels,
          showlegend = F,
          textfont =list(
            size=text$size,
            color = text$color
          )

        )
      }
      p1<-plotly::layout(
        p1,
        title = list(
          text=main,
          font=list(
            size=plot.title_size
          )
        ),

        scene = list(
          zaxis = list(
            title=list(
              text=zlab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )

          ),
          xaxis=list(
            title=list(
              text=xlab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )
          ),
          yaxis=list(
            title=list(
              text=ylab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )
          )
        )
      )
      p1

    }
    get_args_scatter3d<-reactive({

      points<-get_plot3Dpoints()
      text<-get_plot3Dtext()
      shape_args<-get_shapes()
      args=list(points=points,text=text,shape_args=NULL)
      label_args<-map_labels$server("map_labelss")
      args<-c(args,label_args)
      args
    })
    run_map_scatter3d_plotly<-eventReactive(input$run_map,ignoreInit = T,{


      req(isTRUE(scatter3d))
      req(input$mode_plot=="plotly")

      args<-get_args_scatter3d()




      do.call(scatter_3dplotly,args)

    })



    observe({
      req(isTRUE(plotly_installed()))
      output$plotly_stack_out<-plotly::renderPlotly({
        run_map_stack_plotly()
      })
    })



    observeEvent(vals$cur_mode_map,{
      shinyjs::toggle("plotly_surface",condition=vals$cur_mode_map%in%"surface")
      shinyjs::toggle("plotly_stack",condition=vals$cur_mode_map%in%"stack")
    })



    observe({

      shinyjs::toggle('map_labels',condition = input$mode_plot!="leaflet")
      shinyjs::toggle('map_coords',condition = input$mode_plot=="ggplot")
      shinyjs::toggle('map_ggoptions',condition = input$mode_plot=="ggplot")
    })
    box_caret_server('box_plotsize',hide_content=T)

    box_caret_server("box_map_labels",hide_content=T)
    box_caret_server("box_coords")
    box_caret_server("box_ggoptions",
                     hide_content=T)


    box_caret_server("surface_Points")
    box_caret_server("surface_control")
    box_caret_server("surface_savedmaps")
    box_caret_server("box_surface",
                     hide_content=T)


    current_datalist<-reactiveVal()
    observeEvent(vals$data_map,{
      data_list_name<-attr(vals$data_map,"args")$name
      current_datalist(data_list_name)
    })

    observeEvent(current_datalist(),{
      data<-vals$data_map
      res<-get_mean_resolution(data)
      updateNumericInput(session,"raster_resolution",value=res)
    })

    observeEvent(vals$data_map,{
      vals$vgm_error<-NULL
      map_result1_final(NULL)
      map_result2_final(NULL)
    })




    observeEvent(rst(),{
      r<-rst()
      req(r)
      if(!all(is.na(r@data@values))){
        vals$vgm_error<-NULL
      } else{
        met<-attr(r,"method")
        error_message<-paste(met,"failed")
        if(met=="krige"){
          error_message<-paste0(error_message,": check you variogram model.")
        }
        vals$vgm_error<-div(
          style="overflow: auto; max-height: 150px; background: #e68e83",
          strong(icon("triangle-exclamation",style="color: brown"),"Error:"),
          renderPrint({error_message})
        )

      }
    })

    output$vgm_error<-renderUI({
      req(vals$vgm_error)

      vals$vgm_error

    })


    breaks_args1<-reactive({
      ll_breaks$server("breaks1")
    })
    observe({
      shinyjs::toggle("down_grade",condition = isTRUE(input$down_grade_raster))
    })


    output$save_map_btn<-renderUI({
      req(isTRUE(interp)|isTRUE(raster))
      req(length(rst())>0)


      actionLink(ns("save_map"),"Save map",icon("fas fa-save"))
    })
    observe({
      if(is.null(vals$saved_maps)){
        vals$saved_maps<-list()
      }
    })
    observeEvent(input$save_map,ignoreInit = T,{

      map_type=vals$cur_mode_map
      interp_type<-NULL
      if(map_type!="raster"){
        interp_type<-vals$interp_args$interp_type
      }

      var_name<-attr(map_to_save(),"z_name")
      map_name<-c(map_type,interp_type,var_name)

      name0<-paste0(map_name,collapse = "-")
      map_names<-make.unique(c(names(vals$saved_maps),name0))
      name<-map_names[length(map_names)]

      showModal(
        modalDialog(
          easyClose = T,
          textInput(ns("map_name"),"Map Name",name),
          footer=div(modalButton("Cancel"), actionButton(ns("save_map_confirm"), "Confirm"))
        )
      )
    })



    map_to_save<-reactive({
      myrst<-rst()
      attr(myrst,"data_z")<-vals$data_map[,1]
      attr(myrst,"z_name")<-colnames(vals$data_map)
      base<-attr(vals$data_map,"base_shape")
      layer<-attr(vals$data_map,"layer_shape")
      shape_args<-get_shapes()
      if(isTRUE(shape_args$base_shape_args$shape)){
        if(!is.null(base)){
          raster::mask( raster::crop(myrst, raster::extent(base)), base)
        }
      }
      attr(myrst,"base_shape")<-base
      attr(myrst,"layer_shape")<-layer
      myrst
    })

    observeEvent(input$save_map_confirm,ignoreInit = T,{
      myrst<-map_to_save()
      vals$saved_maps[[input$map_name]]<-myrst
      removeModal()
    })




    output$surface_z_control<-renderUI({

      req(isTRUE(interp))

      box_caret(
        ns("box_surface"),
        color="#c3cc74ff",
        title="Z options",
        div()
      )
    })

    output$stack_layers<-renderUI({

      div(numericInput(ns('stack_layers'),"Nº layers",2, max=length(vals$saved_maps)),
          numericInput(ns('sep_factor'),"z-expansion",1),
          numericInput(ns('surf_exp'),"surface-expansion",1),
          div(class="switch_box",switchInput(ns("stack_surface"),"Surface",value=F,size="mini",labelWidth="60px")),
          div("Drag and drop to reorder the layers",style="font-size: 10px"),
          uiOutput(ns('stack_layers_out')))

    })


    observe({
      shinyjs::toggle(
        "surf_exp",
        condition=isTRUE(input$stack_surface)&
          vals$cur_modeplot%in%"plotly"
      )
      shinyjs::toggle(
        "sep_factor",
        condition=vals$cur_modeplot%in%"plotly"
      )


    })

    output$stack_layers_out<-renderUI({

      maps<-vals$saved_maps

      layers<-lapply(1:input$stack_layers,function(i){

        div(
          tags$style(HTML(
            ".stack_order .default-sortable div,.stack_order .rank-list-container{
      margin: 0px; padding: 0px;
font-size:12px;
min-height:24px
      }"
          )),
div(style="display: flex",
    div(paste0(i,"break-stk-ord"),style="width: 0px; height: 0px;color: transparent"),
    pickerInput_fromtop(ns(paste0('stack_layer',i)),NULL,choices=names(vals$saved_maps),selected=names(vals$saved_maps)[i],
                options=list(container="body"),
                width="80%"

    ),
    pickerInput_fromtop(ns(paste0("stack_pal",i)),NULL, choices =  vals$colors_img$val,   options=list(container="body"),
                selected=vals$colors_img$val[-2][i],
                choicesOpt = list(
                  content =  vals$colors_img$img ),width="30px"
    )
)
        )

      })

      ord<-lapply(1:input$stack_layers,function(i){
        div(paste0("Layer",i),style=" border-bottom: 1px dashed gray;width: 60px;padding: 2px;margin-bottom: 2px;margin-top: 2px", class="")
      }

      )


      div(style="max-height: 200px; overflow-y: auto;display: flex",
          div(class="mp0",ord),
          div(class="mp0",
              class="stack_order",
              rank_list(
                labels=layers,
                input_id=ns("stack_order")
              )
          )
      )
    })

    get_z_stack_order<-reactive({
      req(input$stack_order)
      rev(as.numeric(gsub("break-stk-ord.*","",input$stack_order)))
    })


    get_stack_maps<-reactive({
      req(input$stack_order)
      maps<-lapply(1:input$stack_layers,function(i){
        vals$saved_maps[[input[[paste0('stack_layer',i)]]]]
      })
      maps[get_z_stack_order()]

    })


    get_stack_pals<-reactive({
      req(input$stack_order)
      stack_pals<-lapply(1:input$stack_layers,function(i){
        vals$newcolhabs[[input[[paste0('stack_pal',i)]]]]
      })
      res<-stack_pals[get_z_stack_order()]
      names(res)<-1:input$stack_layers
      res

    })

    get_stack_palettes<-reactive({
      pals<-get_stack_pals()
      lapply(seq_along(get_stack_maps()),function(i) {
        my_rst<-get_stack_maps()[[i]]
        data_z<-attr(my_rst,"data_z")
        if(is.factor(data_z)){

          colors<- pals[[i]](nlevels(data_z))
        } else {
          colors<- pals[[i]](100)
        }

        colors
      })
    })

    get_z_stack_names<-reactive({
      sapply(get_stack_maps(),function(x) attr(x,"z_name"))
    })









    output$surface_saved_maps<-renderUI({
      div(
        div(style="display: flex",
            selectInput(ns("saved_maps"),"Saved map (Z)", names(vals$saved_maps)),
            actionLink(ns("trash_map"), icon('fas fa-trash'))
        ),
        selectInput(ns("saved_maps2"),"Saved map (Color)", names(vals$saved_maps)),

        uiOutput(ns("print_ap_color"))
      )
    })
    output$surf_ap_datalist<-renderUI({
      req(isTRUE(input$surf_addpoints))

      choices=names(vals$saved_data)
      selected=vals$cur_surf_ap_datalist
      selected=get_selected_from_choices(selected,choices)
      div(selectInput(ns("surf_ap_datalist"),"Datalist", choices,selected=selected),
          uiOutput(ns("surf_ap_vars")))
    })

    observeEvent(input$surf_ap_datalist,{
      vals$cur_surf_ap_datalist<-input$surf_ap_datalist
    })
    observeEvent(input$surf_ap_vars,{
      vals$cur_surf_ap_vars<-input$surf_ap_vars
    })



    observeEvent(input$surf_ap_z_scale_datalist,{
      vals$cur_surf_ap_z_scale_datalist<-input$surf_ap_z_scale_datalist
    })

    output$surf_ap_vars<-renderUI({
      req(input$surf_ap_datalist)
      data<-vals$saved_data[[input$surf_ap_datalist]]

      validate(need(length(attr(data,"coords"))>0,"No coordinates found in the selected Data"))

      choices<-colnames(data)
      selected=vals$cur_surf_ap_vars
      selected=get_selected_from_choices(selected,choices)
      choices2<-c("Z-Value",colnames(attr(data,"factors")))
      selected2=vals$cur_surf_ap_colfac
      selected2=get_selected_from_choices(selected2,choices2)





      div(

        pickerInput_fromtop(ns("surf_ap_vars"),"Z-Value", choices,selected=selected ,options=shinyWidgets::pickerOptions(liveSearch=T)),
        selectInput(ns("surf_ap_colfac"),"Z-Color", choices2,selected=selected2 ),

        pickerInput_fromtop(ns("surf_ap_palette"), "Palette",choices =  vals$colors_img$val,
                    choicesOpt = list(
                      content =  vals$colors_img$img )
        ),
        numericInput(ns('surf_ap_size'),"Size",1.5),
        checkboxInput(ns("surf_z_scale"),"Scale Size", F ),
        uiOutput(ns("surf_z_scale_args")),


      )
    })


    observe({
      shinyjs::toggle("surf_z_scale_args",condition=isTRUE(input$surf_z_scale))
      shinyjs::toggle("surf_ap_minsize",condition=isTRUE(input$surf_z_scale))


    })

    output$surf_z_scale_args<-renderUI({
      req(isTRUE(input$surf_z_scale))
      choices3=names(vals$saved_data)
      req(input$surf_ap_datalist)
      data<-vals$saved_data[[input$surf_ap_datalist]]
      pic<-which(sapply(choices3,function(x){
        all(rownames(vals$saved_data[[x]])%in%rownames(data))
      }))
      choices3<-choices3[pic]
      selected3=vals$cur_surf_ap_z_scale_datalist
      selected3=get_selected_from_choices(selected3,choices3)
      div(style="padding-left: 10px;",
          selectInput(ns("surf_ap_z_scale_datalist"),"Datalist", choices3,selected=selected3),
          uiOutput(ns("surf_ap_z_scale_vars")),
          numericInput(ns('surf_ap_minsize'),"Min-size",0.5)
      )
    })


    observeEvent(input$surf_ap_z_scale_vars,{
      vals$cur_surf_ap_z_scale_vars<-input$surf_ap_z_scale_vars
    })
    output$surf_ap_z_scale_vars<-renderUI({
      choices<-colnames(vals$saved_data[[input$surf_ap_z_scale_datalist]])
      selected=vals$cur_surf_ap_z_scale_vars
      selected=get_selected_from_choices(selected,choices)

      pickerInput_fromtop(ns("surf_ap_z_scale_vars"),"Z-Value", choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T) )
    })





    get_surftext<-reactive({
      args<-args_labels()
      if(isFALSE(args$show_labels)){
        return(NULL)
      }
      req(args$z_value)
      data0<-data<-vals$saved_data[[args$datalist]]
      req(args$z_value%in%colnames(data))
      validate(need(length(attr(data,"coords"))>0,"No coordinates found in the selected Data"))

      z_value<-data[args$z_value]
      coords<-attr(data,"coords")
      data<-attr(data,"factors")


      req(args$labels%in%colnames(data))
      var=data[args$labels]
      points<-data.frame(coords,var,z_value)
      colnames(points)<-c("x","y","label","z")
      points$color<-args$col.fac
      points$size<-args$cex.fac

      points
    })
    get_surfpoints<-reactive({
      if(isFALSE(input$surf_addpoints)){
        return(NULL)
      }
      req(input$surf_ap_datalist)
      data0<-data<-vals$saved_data[[input$surf_ap_datalist]]
      validate(need(length(attr(data,"coords"))>0,"No coordinates found in the selected Data"))
      coords<-attr(data,"coords")
      req(input$surf_ap_vars)
      req(input$surf_ap_vars%in%colnames(data))
      var=data[input$surf_ap_vars]
      points<-data.frame(coords,var)
      pal<-vals$newcolhabs[[input$surf_ap_palette]]

      if(input$surf_ap_colfac=="Z-Value"){

        cut_var<-cut(var[,1],length(unique(var[,1])))
        points$color<-pal(nlevels(cut_var))[cut_var]
      } else{
        cut_var<-attr(data0,"factors")[,input$surf_ap_colfac]
        points$color<-pal(nlevels(cut_var))[cut_var]
      }


      points$size<-input$surf_ap_size
      if(isTRUE(input$surf_z_scale)){
        req(input$surf_ap_z_scale_datalist)
        data_size<-vals$saved_data[[input$surf_ap_z_scale_datalist]]
        req(input$surf_ap_z_scale_vars%in%colnames(data_size))
        size<-data_size[,input$surf_ap_z_scale_vars]

        newsize<-scales::rescale(size,c(input$surf_ap_minsize,input$surf_ap_size))


        points$size<-newsize
      }
      points


    })

    output$print_ap_color<-renderUI({
      # p<-get_surfpoints()
      #renderPrint(head(p))



    })

    observeEvent(input$surf_ap_colfac,{
      vals$cur_surf_ap_colfac<-input$surf_ap_colfac
    })





    observe({
      shinyjs::toggle('saved_maps2', condition=length(vals$saved_maps)>1)
    })

    observeEvent(input$trash_map,{
      vals$saved_maps[[input$saved_maps]]<-NULL
    })


    observeEvent(input$mode_plot,{
      toggle("surf_d",condition=input$mode_plot=="plot3D")
      toggle("leg_surface",condition=input$mode_plot=="plot3D")
      toggle("surf_tick",condition=input$mode_plot=="plot3D")

    })

    observeEvent(input$reset_zcontrol,{

      inputs<-c("surf_exp",
                'surf_exp',
                'surf_theta',
                'surf_r',
                'surf_tick',
                'surf_d')
      sapply(inputs, shinyjs::reset)


    })
    output$surface_3d_control<-renderUI({

      req(vals$cur_modeplot)
      req(isTRUE(any(c(surface,stack))))
      req(!vals$cur_modeplot%in%"plotly")
      box_caret(

        ns("box_surface"),
        color="#c3cc74ff",
        title="3D control",
        hide_content=T,
        div(id=ns("d3_control"),
            div(align="right",
                actionLink(ns("reset_zcontrol"),"reset",style="font-size: 12px;")
            ),

            numericInput(ns("surf_exp"),span('+ exp:',tipright("a expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction")), 0.8,  step=.1),
            numericInput(ns("surf_theta"),span('+ theta:',tipright("azimuthal direction")),0,max=360,step=10),
            numericInput(ns("surf_phi"),span('+ phi:',tipright("colatitude direction")),40,min=180,step=10),

            numericInput(ns("surf_r"),span('+ Eye point:',tipright("rhe distance of the eyepoint from the centre of the plotting box")), 1.73),
            if(!vals$cur_modeplot%in%"plotly")
              pickerInput_fromtop(ns("surf_tick"),span('+ ticktype',tipright("simple - draws just an arrow parallel to the axis to indicate direction of increase; detailed - draws normal ticks as per 2D plots.")), c("simple","detailed")),
            numericInput(ns("surf_d"),span('+ persp strength:',tipright("a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it")), 1)



        )

      )
    })



    crop_raters<-function(my_rst1,my_rst2){
      r2 <- raster::crop(my_rst1,extent(my_rst2))
      r3<-raster::crop(my_rst2,extent(my_rst1))
      extent(r2)<-extent(r3)
      r3e<-r3@ncols*r3@nrows
      r2e<-r2@ncols*r2@nrows
      if(r3e!=r2e){

        r23<-list(r2=r2,r3=r3)

        min_res<-which.min(c(r2e,r3e))
        max_res<-which.max(c(r2e,r3e))
        r_change<-r23[[max_res]]
        r_base<-r23[[min_res]]
        coarser=min_res
        e <- extent(r_base)
        r_new <- raster(e, ncol=r_base@ncols, nrow=r_base@nrows)
        r_tochange<-rasterToPoints(r_change)

        r_new2 <- rasterize( r_tochange[,1:2], r_new, r_tochange[,3], fun=mean)
        r23[[max_res]]<-r_new2
        r2<-r23$r2
        r3<-r23$r3
      }
      s<-raster::stack(r2,r3)

      s

    }

    transform_z <- function(z, exp) {
      # log(z + 1)
      z
    }

    stack_plotly<-function(maps,pals,surface=F,sep_factor=1,xlab="x",ylab="y",zlab="z",main="",plot.title_size=12,axis.text_size=12,axis.title_size=12,shape_args=NULL,z_names=NULL,surf_exp=0.5,...){
      if(isFALSE(surface)){
        surf_exp<-1
      }


      if(is.null(z_names)){
        z_names<-rep("",length(maps))
      }
      fac_maps<-sapply(maps,function(x){
        ncol( x@data@attributes[[1]])==2
      })
      maps2<-resize_rasters(maps)

      p <- plotly::plot_ly(showscale = FALSE)
      i=2
      init=0

      init_i<-1
      init_m<-c(init_i+sep_factor)
      r_ranges<-list()
      r_ranges[[1]]<-c(init_i,init_m)
      for(i in 2:length(maps)){
        newmin<-(max(r_ranges[[i-1]])+sep_factor)
        newmax<-(newmin+surf_exp)
        r_ranges[[i]]<-c(newmin,newmax)}




      y_colorbar<-seq(0.05,.8,len=length(maps))
      for(i in seq_along(maps2)){
        map<-maps2[[i]]
        xyz<-get_rst_xyz_plotly(map)
        z_color<-xyz$z
        xyz$z<-z1<-scales::rescale(xyz$z,c(r_ranges[[i]][[1]],r_ranges[[i]][[2]]))


        zmap<-xyz$z

        if(isTRUE(fac_maps[i])|isFALSE(surface)){
          zmap<-scales::rescale(xyz$z,c(r_ranges[[i]][[1]],r_ranges[[i]][[1]]))
        }

        color_pal<-pals[[i]]
        p<-plotly::add_trace(
          p,z=zmap,
          x=xyz$x,
          y=xyz$y,
          surfacecolor =z_color,
          type = 'surface',
          showscale = T,
          colorscale = list(c(seq(0,1,len=length(color_pal))),color_pal),
          colorbar=list(
            title=list(
              text=z_names[[i]],
              font=list(
                size=8
              )
            ),
            tickfont=list(
              size=8
            ),
            orientation="v",
            y=y_colorbar[[i]],
            yanchor="bottom"
          ),
          contours =list(
            z=list(
              usecolormap=T
            ),
            y=list(
              usecolormap=T
            ),
            x=list(
              usecolormap=T,
              color="red",
              highlight=T
            )
          )


        )
        shape_args$base_shape_args$z<-r_ranges[[i]][[1]]
        shape_args$layer_shape_args$z<-r_ranges[[i]][[1]]
        # p<-add_shape_plotly(p,maps[[i]],shape_args$base_shape_args,which_shape="base_shape")
        p<-add_shape_plotly(p,maps[[i]],shape_args$layer_shape_args,which_shape="layer_shape")


      }
      xyz<-get_rst_xyz_plotly(maps2[[1]])
      x<-xyz$x
      y<-xyz$y
      z<-c(1,max(unlist(r_ranges)))
      if(isTRUE(surface)){
        z<-c(1,max(unlist(r_ranges)))
      }

      p<-plotly::layout(
        p,
        title = list(
          text=main,
          font=list(
            size=plot.title_size
          )
        ),

        scene = list(
          zaxis = list(
            #range=range(z),

            title=list(
              text=zlab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )

          ),
          xaxis=list(
            range=range(x),
            title=list(
              text=xlab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )
          ),
          yaxis=list(
            range=range(y),
            title=list(
              text=ylab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )

          )
          #spectratio = list(z = exp)
        )
      )
      return( p)

    }
    # my_rst1<-args$my_rst1
    # my_rst2<-args$my_rst2
    surface_plotly<-function(
    my_rst1,my_rst2,addpoints,colors,exp=0.5,theta,phi,r,custom_breaks,
    xlab='x',
    ylab="y",
    zlab="z",
    axis.title_size=1,
    axis.text_size=1,
    main="",
    plot.title_size=1,
    shape_args=NULL,
    addtext=NULL,

    ...){

      my_rst1_0<-my_rst1
      stack_rasters<-crop_raters(my_rst1,my_rst2)
      my_rst1<-stack_rasters[[1]]
      my_rst2=stack_rasters[[2]]

      rst<-my_rst1

      zval<-rst@data@values
      z=matrix(zval,ncol=rst@nrows,nrow=rst@ncols)
      z<-apply(z,1,rev)
      x<-seq(rst@extent[1],rst@extent[2],len=ncol(z))
      y<-seq(rst@extent[3],rst@extent[4],len=nrow(z))
      if(!is.null(addpoints))
        colnames(addpoints)<-c("x",'y',"z","col","size")



      rst2<-my_rst2
      #rst2<-crop_raters(my_rst1,my_rst2)
      z2val<-rst2@data@values
      z2=matrix(z2val,ncol=rst2@nrows,nrow=rst2@ncols)
      z2<-apply(z2,1,rev)

      color_pal<-colorRampPalette(colors)(100)


      p<-plotly::plot_ly(x = x, y = y, z =z)

      p<-add_shape_plotly(p,my_rst1_0,shape_args$base_shape_args,which_shape="base_shape")
      p<-add_shape_plotly(p,my_rst1_0,shape_args$layer_shape_args,which_shape="layer_shape")
      p<-plotly::add_surface(p,
                             surfacecolor =z2,
                             colorscale = list(c(seq(0,1,len=100)),color_pal))
      p

      #p<-plotly::add_trace(p,x = x, y = y,z=z2,colorscale = list(c(seq(0,1,len=100)),rev(color_pal)))

      p<-plotly::add_trace(p,
                           data = addpoints,
                           x = addpoints$x,
                           y =addpoints$y,
                           z=addpoints$z,
                           type = 'scatter3d',
                           mode = 'markers',
                           marker=list(
                             size=addpoints$size*5,
                             color = addpoints$col,
                             line = list(
                               width = 0
                             )

                           )

      )
      text=addtext
      if(!is.null(text)) {
        p<-plotly::add_trace(
          p,
          data = text,
          x = text$x,
          y =text$y,
          z=text$z,
          type = 'scatter3d',
          mode = 'text',
          text=text$label,
          showlegend = F,
          textfont =list(
            size=text$size,
            color = text$color
          )

        )
      }


      cam<-get_camera(theta-80,
                      phi+25,
                      r)
      p<-plotly::layout(
        p,
        title = list(
          text=main,
          font=list(
            size=plot.title_size
          )
        ),

        scene = list(
          zaxis = list(
            range=range(z),
            tickvals = custom_breaks,
            title=list(
              text=zlab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )

          ),
          xaxis=list(
            range=range(x),
            title=list(
              text=xlab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )
          ),
          yaxis=list(
            range=range(y),
            title=list(
              text=ylab,
              font=list(
                size=axis.title_size
              )
            ),
            tickfont=list(
              size=axis.text_size
            )

          ),
          camera = list(eye = list(x = cam$x, y = cam$y, z = cam$z)),
          aspectratio = list(z = exp))
      )
      p
    }


    get_camera<- function(theta, phi, r) {

      # Convertendo os ângulos de graus para radianos
      theta_rad <- theta * pi / 180
      phi_rad <- phi * pi / 180

      # Calculando as coordenadas cartesianas
      x <- r * sin(phi_rad) * cos(theta_rad)
      y <- r * sin(phi_rad) * sin(theta_rad)
      z <- r * cos(phi_rad)

      # Retornando uma lista com as coordenadas
      list(x = x, y = y, z = z)
    }

    observeEvent(input$mode_plot,{
      vals$cur_modeplot<-input$mode_plot
    })

    resize_rasters<-function(maps){

      names(maps)<-paste0("map_",1:length(maps))
      min_res<-which.min(
        sapply(maps,function(x){
          nrow(x)*ncol(x)
        }))
      fine_res<-maps[[min_res]]


      to_decrease<-maps[-min_res]


      to_decrease<-lapply(to_decrease,function(x){
        r2 <- raster::crop(x,extent(fine_res))
        extent(x)<-extent(fine_res)
        x
      })

      r_new <- raster(extent(fine_res), ncol=fine_res@ncols, nrow=fine_res@nrows)

      map_decreased<-lapply(to_decrease,function(x){

        if(ncol(x@data@attributes[[1]])==2){
          fun=function(x,...){
            as.numeric(names(which.max(table(x))))
          }
        } else{
          fun=mean
        }
        r_tochange<-rasterToPoints(x)
        r_new2 <- rasterize( r_tochange[,1:2], r_new, r_tochange[,3], fun=fun)
        r_new2
      })
      names(map_decreased)<-names(to_decrease)
      new_maps<-c(map_decreased,list(fine_res))

      names(new_maps)[length(new_maps)]<-names(maps)[min_res]

      new_maps<-new_maps[names(maps)]
      new_maps
    }


    #rm(list=ls())
    get_rst_xyz_plotly<-function(rst){
      zval<-rst@data@values
      z=matrix(zval,ncol=rst@nrows,nrow=rst@ncols)
      z<-apply(z,1,rev)
      x<-seq(rst@extent[1],rst@extent[2],len=ncol(z))
      y<-seq(rst@extent[3],rst@extent[4],len=nrow(z))
      list(x=x,y=y,z=z)
    }









    observeEvent(input$saved_maps,{
      vals$cur_rst<-vals$saved_maps[[input$saved_maps]]
    })

    get_args_surface<-reactive({

      req(isTRUE(surface))
      req(input$custom_breaks)
      p1<-vals$saved_maps[[input$saved_maps]]
      shape_args<-get_shapes()


      p2<-vals$saved_maps[[input$saved_maps2]]
      palette<-color_args1()$pal
      light<-color_args1()$light


      my_rst<-p1
      data_z<- attr(p1,"data_z")

      if(is.factor(data_z)){

        colors<- vals$newcolhabs[[palette]](nlevels(data_z))
      } else {
        colors<- vals$newcolhabs[[palette]](length(my_rst@data@values))
      }
      colors<-lighten(colors,light)

      my_rst2=p2
      args<-list(
        my_rst1=my_rst,
        my_rst2=my_rst2,
        colors=colors,
        theta=input$surf_theta,
        phi=input$surf_phi,
        r=input$surf_r,
        d=input$surf_d,
        exp=input$surf_exp,
        tictype=input$surf_tick,
        wlegend=input$surface_width.legend,
        hlegend=input$surface_height.legend,

        shape_args=shape_args,
        addpoints=get_surfpoints(),
        addtext=get_surftext(),
        custom_breaks=as.numeric(strsplit(input$custom_breaks,",")[[1]])
      )
      label_args<-map_labels$server("map_labelss")
      args<-c(args,label_args)
    })



    observeEvent(get_args_surface(), {
      addClass("run_map_btn", "save_changes")
    })

    observeEvent(get_args_stack(), {
      addClass("run_map_btn", "save_changes")
    })

    observe({
      req(vals$cur_modeplot%in%"plotly")
      shinyjs::toggle("run_map_btn",condition=isTRUE(plotly_installed()))
    })
    observe({
      req(vals$cur_modeplot%in%"plot3D")
      shinyjs::toggle("run_map_btn",condition=isTRUE(plot3D_installed()))
    })


    get_args_stack<-reactive({

      req(isTRUE(stack))
      maps<-get_stack_maps()
      z_names<-get_z_stack_names()
      pals<-get_stack_palettes()
      shape_args<-get_shapes()

      args<-list(
        maps=maps,
        z_names=z_names,
        pals=pals,
        sep_factor=input$sep_factor,
        surf_exp=input$surf_exp,
        shape_args=shape_args,
        #  addpoints=get_surfpoints(),
        # addtext=get_surftext(),
        surface=input$stack_surface,
        theta=input$surf_theta,
        phi=input$surf_phi,
        r=input$surf_r,
        d=input$surf_d,
        exp=input$surf_exp,
        tictype=input$surf_tick

      )
      label_args<-map_labels$server("map_labelss")
      args<-c(args,label_args)

      args
    })


    run_map_surface_plot3D<-eventReactive(input$run_map,ignoreInit = T,{
      req(isTRUE(surface))
      req(input$mode_plot=="plot3D")


      args<-get_args_surface()

      withProgress(min=NA,max=NA,message="Running",{
        do.call(get_4D,args)
      })


    })
    run_map_surface_plotly<-eventReactive(input$run_map,ignoreInit = T,{

      req(isTRUE(surface))
      req(input$mode_plot=="plotly")

      args<-get_args_surface()
      req(args)
      shinyjs::removeClass("run_map_btn", "save_changes")

      do.call(surface_plotly,args)

    })
    run_map_stack_plotly<-eventReactive(input$run_map,ignoreInit = T,{

      req(isTRUE(stack))
      req(input$mode_plot=="plotly")

      args<-get_args_stack()
      req(args)
      shinyjs::removeClass("run_map_btn", "save_changes")
      withProgress(do.call(stack_plotly,args),min=NA,max=NA,message="Running...")


    })

    run_map_stack_plot3D<-eventReactive(input$run_map,ignoreInit = T,{
      req(isTRUE(stack))
      req(input$mode_plot=="plot3D")

      args<-get_args_stack()
      do.call(stack_plot3D,args)

    }
    )

    output$plot3D_stack<-renderUI({
      graphics.off()
      req(input$mode_plot=="plot3D")
      req(isTRUE(stack))
      renderPlot({
        run_map_stack_plot3D()
        persp<-recordPlot()
        vals$map_res<-persp
        vals$map_res
      })
    })




    output$plot3D_surface<-renderUI({
      req(input$mode_plot=="plot3D")
      req(isTRUE(surface))
      renderPlot({
        run_map_surface_plot3D()
        persp<-recordPlot()
        vals$map_res<-persp
        vals$map_res
      })
    })


    get_xyz_persp3D<-function(my_rst1){
      r1<-raster::as.matrix(my_rst1,transpose=F)
      ex<-raster::extent(my_rst1)
      z<-t(apply(r1,2,rev))
      x<-seq(ex[1],ex[2],len=nrow(z))
      y<-seq(ex[3],ex[4],len=ncol(z))
      return(list(
        x=x,y=y,z=z
      ))
    }
    stack_plot3D<-function(maps,z_names=NULL,pals,  sep_factor=2,surf_exp=3,shape_args=NULL,surface=F,main="",
                           exp=0.2,tictype='detailed',theta=0,phi=40,r=1.73,d=1,xlab="",ylab="",zlab="",plot.title_size=1,axis.title_size=1,axis.text_size=1,...){



      if(is.null(z_names)){
        z_names<-rep("",length(maps))
      }
      fac_maps<-sapply(maps,function(x){
        ncol( x@data@attributes[[1]])==2
      })
      maps2<-resize_rasters(maps)
      names(args)
      xyz1<-get_xyz_persp3D(maps[[1]])

      n<-length(maps)

      init_i<-1
      init_m<-c(init_i+sep_factor)
      r_ranges<-list()
      r_ranges[[1]]<-c(init_i,init_m)
      for(i in 2:length(maps)){
        newmin<-max(r_ranges[[i-1]])+sep_factor
        newmax<-newmin+surf_exp
        r_ranges[[i]]<-c(newmin,newmax)}



      len_colkeys=1/(n*2)

      zlim<-c(1,max(unlist(r_ranges))-1)
      if(isTRUE(surface)){
        zlim<-c(1,max(unlist(r_ranges))+1)
      }
      pmat<-plot3D::perspbox(x=xyz1$x,y=xyz1$y,z=1, exp=exp,ticktype=tictype,theta=theta,phi=phi,r=r,d=d,xlab=xlab,ylab=ylab,zlim=zlim,
                             zlab=zlab, main=main,cex.main=plot.title_size/12,cex.axis=axis.text_size/12,cex.lab=axis.title_size/12)



      leg_positions<-  seq(-.2,.2,len=n)
      clab_positions<-  seq(-.2,.2,len=n)
      for(i in 1:n){
        xyz<-get_xyz_persp3D(maps2[[i]])
        z2<-xyz$z
        if(isTRUE(fac_maps[i])|isFALSE(surface)){
          z1<-r_ranges[[i]][[1]]
        } else{
          z1<-scales::rescale(xyz$z,c(r_ranges[[i]][[1]],r_ranges[[i]][[2]]))
        }
        colfunc<-colorRampPalette(pals[[i]])
        layer_args<-shape_args$layer_shape_args
        layer_args$z<-min(z1)
        add_3dpolygon(maps2[[i]],layer_args, pmat,which_shape="layer_shape")

        plot3D::persp3D(x=xyz$x,y=xyz$y,z=z1, colvar=z2, col=colfunc(length(z2)),border=NA,exp=exp,ticktype=tictype,theta=theta,phi=phi,r=r,d=d,xlab=xlab,ylab=ylab,add=T,clab=attr(maps[[i]],'z_name'),NAcol=NA,
                        colkey=list(
                          line.clab=0.2,
                          length=len_colkeys,
                          shift=leg_positions[[i]],
                          cex.clab=0.5,
                          lwd=0,
                          font=1,
                          #col.axis = "white",
                          col.box = "white",
                          tcl =1,
                          lwd.ticks=0,
                          cex.axis=0.5
                        ))

      }
    }

    get_polygon_coordinates<-function(shape){
      result<-lapply(shape$geometry,function(x) {
        lapply(x,function(xx){
          xx[[1]]
        })
      })
      result<-unlist(result,recursive=F)
      result
    }

    add_shape_plotly<-function(p,my_rst1,shape_args,which_shape="layer_shape"){
      if(isFALSE(shape_args$shape)){
        return(p)
      }
      shape<-attr(my_rst1,which_shape)
      if(is.null(shape)){
        return(p)
      }
      ext<-raster::extent(shape)
      rpoly<-raster::raster(ext,  nrows=360,ncols=360)
      r_shape<-rasterize(to_spatial(data.frame(coordinates(rpoly))),rpoly,shape_args$z)
      rp0<-raster::mask( raster::crop(r_shape, ext), shape)


      zp0=matrix(rp0@data@values,ncol=rp0@nrows,nrow=rp0@ncols)
      np_new<-zp0
      np_new[]
      zp<-apply(zp0,1,rev)
      color_matrix <- matrix(1, nrow = nrow(zp), ncol = ncol(zp))
      xp=seq(rp0@extent[1],rp0@extent[2],len=ncol(zp))
      yp=seq(rp0@extent[3],rp0@extent[4],len=nrow(zp))
      color<-adjustcolor(shape_args$color,shape_args$fillOpacity)
      p1<-plotly::add_trace(p,x=xp,y=yp,
                            type = 'surface', z = zp,
                            surfacecolor = color_matrix,
                            colorscale = list(c(0, 1), c(color, color)),
                            showscale = FALSE
      )

      p1
    }

    add_3dpolygon<-function(my_rst1,args_shape, pmat,which_shape="base_shape"){
      if(isTRUE(args_shape$shape)) {
        shape<-attr(my_rst1,which_shape)
        fill<-args_shape$fill
        if(isTRUE(fill)){
          fill_color<-adjustcolor(args_shape$color,args_shape$fillOpacity)
        } else{
          fill_color<-NA
        }

        if(!is.null(shape)){
          poly_list<-get_polygon_coordinates(shape)
          poly_list<-poly_list[ sapply(sapply(poly_list,dim),length)>0]
          poly_list<-poly_list[sapply(poly_list,length)>0]
          lapply(poly_list, function(poly){

            polygon(trans3d(poly[,1] , y=poly[,2], z=args_shape$z,  pmat=pmat),col=fill_color, border=args_shape$border_col, lwd=args_shape$weight)
          })

        }


      }
    }


    get_graphics_download<-reactive({
      if(isTRUE(stack)){
        args<-get_args_stack()
        fun=stack_plot3D
      }
      if(isTRUE(surface)){
        args<-get_args_surface()
        fun=get_4D
      }
      return(list(args=args,fun=fun))


    })


    observeEvent(ignoreInit = T,input$down_ggplot,{
      generic<-map_result2()
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")

      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Map", name_c="Map",fun=get_graphics_download()$fun,args_plot=get_graphics_download()$args)
    })




    observeEvent(ignoreInit = T,input$down_plot3D_btn,{

      vals$hand_plot<-"plot_funcion"
      module_ui_figs("downfigs")

      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Map", name_c="Map",fun=get_graphics_download()$fun,args_plot=get_graphics_download()$args)
    })


    add_ticks<-function(x,y,z, pmat,len=0.10,cex=1){
      x<-round(x,2)
      y<-round(y,2)
      z<-round(z,2)
      min.x  <- min(x, na.rm = T)
      max.x  <- max(x, na.rm = T)
      x.axis <- seq(min.x,max.x,len=5) # by = 2 will get you 5 ticks
      min.y  <- min(y, na.rm = T)
      max.y  <- max(y, na.rm = T)
      y.axis <- seq(min.y,max.y, len=5) # by = 5 will get you 2 ticks
      min.z  <- round(min(z, na.rm = T))
      max.z  <- round(max(z, na.rm = T))
      z.axis <- seq(min.z, max.z,len=5) # by = 5 will get you 7 ticks

      tick.start <- trans3d(x.axis, min.y, min.z, pmat)
      tick.end   <- trans3d(x.axis, (min.y - len), min.z, pmat)
      segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)



      #Note the (min.y - len) in the calculation of tick.end. This places the second line, parallel to the X axis, at the position -len on the Y axis (i.e., into negative/unplotted space).

      #The tick marks on the Y and Z axes can be handled similarly:

      tick.start <- trans3d(max.x, y.axis, min.z, pmat)
      tick.end   <- trans3d(max.x + len, y.axis, min.z, pmat)
      segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

      tick.start <- trans3d(min.x, min.y, z.axis, pmat)
      tick.end <- trans3d(min.x, (min.y - len), z.axis, pmat)
      segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)

      labels <- as.character(x.axis)
      label.pos <- trans3d(x.axis, (min.y - 0.25), min.z, pmat)
      text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), srt=270, cex=cex)

      #The adj=c(0, NA) expression is used to left-justify the labels, the srt=270 expression is used to rotate the labels 270°, and the cex=0.5 expression is used to scale the label text to 75% of its original size.

      #The labels on the Y and Z axes are produced similarly:
      labels <- as.character(y.axis)
      label.pos <- trans3d((max.x + 0.25), y.axis, min.z, pmat)
      text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA), cex=cex)

      labels <- as.character(z.axis)
      label.pos <- trans3d(min.x, (min.y - 0.5), z.axis, pmat)
      text(label.pos$x, label.pos$y, labels=labels, adj=c(1, NA), cex=cex)

    }

    get_4D<-function(my_rst1,my_rst2=NULL,colors,exp=0.2,wlegend=20,hlegend=20,tictype='detailed',shape_args=NULL,addpoints=NULL,addtext=NULL,legend=T,slices=F,bg_points=T,theta=0,phi=40,r=1.73,d=1,xlab="",ylab="",
                     zlab="",main=NULL,plot.title_size=1,
                     axis.title_size=1,
                     axis.text_size=1,
                     ...)
    {


      #graphics.off()
      col0<-colors
      my_rst2_0<-my_rst2
      stack_rasters<-crop_raters(my_rst1,my_rst2)
      my_rst1<-stack_rasters[[1]]
      my_rst2=stack_rasters[[2]]
      ex<-extent(my_rst1)
      r1<-raster::as.matrix(my_rst1,transpose=F)
      z1<-t(apply(r1,2,rev))
      r2<-raster::as.matrix(my_rst2,transpose=F)
      z2<-t(apply(r2,2,rev))
      x<-seq(ex[1],ex[2],len=nrow(z1))
      y<-seq(ex[3],ex[4],len=ncol(z1))
      colfunc = colorRampPalette(colors)
      color_apl<-colfunc(100)
      z.facet.center <- (z2[-1, -1] + z2[-1, -ncol(z2)] + z2[-nrow(z2), -1] + z2[-nrow(z2), -ncol(z2)])/4
      # Range of the facet center on a 100-scale (number of colors)
      z.facet.range<-cut(z.facet.center, 100, include.lowest=T)


      pmat<-plot3D::perspbox(x=x,y=y,z=z1, exp=exp,ticktype=tictype,theta=theta,phi=phi,r=r,d=d,xlab=xlab,ylab=ylab,zlim=range(z1[!is.na(z1)]),
                             zlab=zlab, main=main,cex.main=plot.title_size/12,cex.axis=axis.text_size/12,cex.lab=axis.title_size/12)


      base_args<-shape_args$base_shape_args
      layer_args<-shape_args$layer_shape_args
      add_3dpolygon(my_rst1,base_args, pmat,which_shape="base_shape")
      add_3dpolygon(my_rst1,layer_args, pmat,which_shape="layer_shape")



      plot3D::persp3D(x=x,y=y,z=z1, colvar=z2, col=colfunc(length(z2)),border=NA,exp=exp,ticktype=tictype,theta=theta,phi=phi,r=r,d=d,xlab=xlab,ylab=ylab,add=T,clab=attr(my_rst2_0,'z_name'))

      if(!is.null(addpoints)){
        if(isFALSE(bg_points)){
          points(trans3d(addpoints[,1] , y=addpoints[,2], z=addpoints[,3],  pmat=pmat), col=addpoints[,4], pch=16,cex=addpoints[,"size"])
        } else{
          pal<-colfunc(length(unique(addpoints[,3])))[cut(addpoints[,3],length(unique(addpoints[,3])))]
          points(trans3d(addpoints[,1] , y=addpoints[,2], z=addpoints[,3],  pmat=pmat),col=pal ,bg=addpoints[,4], pch=21,cex=addpoints[,"size"])
        }

      }

      if(!is.null(addtext)){
        text(trans3d(addtext$x , y=addtext$y, z=addtext$z,  pmat=pmat),col=addtext$color , cex=addtext$size/5, labels=addtext$label)

      }



      return(pmat)

    }



    box_caret_server('box_labels')
    box_caret_server('box_color')
    box_caret_server('box_circles')
    box_caret_server('box_interp')


    output$custom_breaks<-renderUI({
      if(isTRUE(surface)){
        req(input$saved_maps)
        p1<-vals$saved_maps[[input$saved_maps]]
        z<- attr(p1,"data_z")
        n<-breaks_args1()$nbreaks
        values<-pretty(z,n=n)
        values<-paste(values,collapse = ",")

      } else{
        req(is.numeric(vals$data_map[,1]))
        z<-vals$data_map[,1]
        n<-breaks_args1()$nbreaks
        req(z)
        req(n)

        values<-scales::cbreaks(range(na.omit(z)), breaks =scales::extended_breaks(n))$breaks
        values[which.min(values)]<-min(z)
        values[which.max(values)]<-max(z)
        values<-paste(values,collapse = ",")

      }




      textInput(session$ns("custom_breaks"),span("Break values",tiphelp("comma-delimited values")),values)

    })
    observe({
      shinyjs::toggle(selector=".border_color",condition = input$mode_plot=="ggplot")
    })


    output$box_circles_title<-renderUI({
      if(isTRUE(circles)){
        "Circles"
      } else{
        span("Pies",tipify(actionLink(ns("pie_chart_help"),icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Click for details"))
      }


    })


    observe({
      condition<-!is.factor(vals$data_map[,1])
      shinyjs::toggle("show_breaks", condition = condition)
    })



    output$vgm_plot<-renderUI({
      req(isTRUE(interp))
      if(vals$interp_args$interp_type=="krige"){
        validate(need(!inherits(vals$interp_vgm,"try-error"),vals$interp_vgm[1]))
        validate(need(!inherits(vals$interp_gstat,"try-error"),vals$interp_gstat[1]))
      }

      ns<-session$ns
      vc<-vals$interp_vc
      req(vc)
      TheVariogram<-vc$variogram
      model<-vals$interp_vgm
      req(model)
      vals$g_final<-vals$interp_gstat
      req(vals$g_final)
      #vc<-readRDS('vc.rds')
      # input<-readRDS('input.rds')
      #div(actionLink(session$ns("down_gg_vgm"), icon("download"),icon=icon('image'))),
      column(12,style="display: flex",
             renderPlot({
               g<-plot_variogram(TheVariogram, model)
               g
             },height=275, width=350),
             column(12,
                    uiOutput(ns('cv_g_summary')),)
      )
    })
    output$caret_summary<-renderUI({
      req(isTRUE(interp))
      renderPrint( attr(rst(),"m"))
    })

    output$cv_g_summary<-renderUI({
      req(isTRUE(interp))
      if(vals$interp_args$interp_type=="krige"){
        validate(need(!inherits(vals$interp_vgm,"try-error"),vals$interp_vgm[1]))
        validate(need(!inherits(vals$interp_gstat,"try-error"),vals$interp_gstat[1]))
      }
      g<-vals$g_final_cv
      req(g)
      req(!is.null(attr(g,"cv")))
      g<-attr(g,"cv")
      req(g)

      div(g_cross_results(attr(vals$g_final_cv,"cv")))
    })

    output$interp1_out<-renderUI({
      req(isTRUE(interp))
      data<-vals$data_map
      req(is.data.frame(data))
      req(nrow(data)>0)

      NULL
    })

    if(isTRUE(interp)){
      ll_interp$server("interp1", vals)
    }


    observeEvent(vals$interp_args$interp_type,{
      removeTab('mode_plot','tab_gstat')


      if(isTRUE(interp))
        if(!vals$interp_args$interp_type%in%c("idw")){
          shiny::insertTab("mode_plot",
                           tabPanel("Model Results",value="tab_gstat",
                                    tabsetPanel(NULL,id=ns("interp_results"),
                                                type="hidden",
                                                tabPanel('krige',uiOutput(session$ns("vgm_plot"))),
                                                tabPanel('caret',uiOutput(session$ns("caret_summary")))
                                    )


                           ),

                           position="after",target='ggplot',select =F)
        }


    })

    observeEvent(vals$interp_args$interp_type,{
      interp_type<-vals$interp_args$interp_type
      req(interp_type)
      if(!interp_type%in%c("krige","idw")){
        selected<-"caret"
      } else{
        selected<-interp_type
      }
      updateTabsetPanel(session,'interp_results',selected=selected)
    })



    interpolation_args<-reactive({
      req(vals$interp_args)
      args<-vals$interp_args
      if (isTRUE(interp)) {
        req(args)
        args$data<-vals$data_map
        shape_args<-get_shapes()
        args$cut_shape<-shape_args$base_shape_args$shape
        args$g<-vals$interp_gstat

        rst<-do.call(interp_leaflet2, args)

        attr(rst,"method")<-args$interp_type


        rst(rst)
      } else{
        rst(NULL)
      }

    })

    condition_cokrige<-reactive({
      is.factor(vals$data_map[,1])|
        length(vals$g_final)>0
    })




    extra_shapes<-reactive({
      shapes_extra$server("extra", vals$data_map)
    })
    output$extraLayers_output<-renderUI({
      shapes_extra$ui(session$ns("extra"), vals$data_map)
    })
    observe({

      sf_in = any(c(interp, coki))
      ll_shapes$server_update1("shapes1", data=vals$data_map, sf_in,vals=vals)


    })
    circle_args1<-reactive({

      if(isTRUE(circles)|isTRUE(pie)){
        ll_circles$server("circles1", vals)
      } else{
        NULL
      }

      #if(is.null(vals$data_map)) {return(NULL)}

    })
    raster_args<-reactive({
      # if(is.null(vals$data_map)) {return(NULL)}
      if (isTRUE(raster)) {
        shape_args<-get_shapes()
        cut_shape<-shape_args$base_shape_args$shape
        args<-list(data=vals$data_map, cut_shape = cut_shape, resolution=input$raster_resolution)


        rst<-do.call(map_rst,args)
        rst<-migrate_rst(rst, vals$data_map)
        if(isTRUE(input$down_grade_raster)){
          rst <- raster::aggregate(rst, fact=input$down_grade)


        }
        rst(rst)
      }
    })








    args_gg<-reactive({
      args<-c(map_labels$server("map_labelss"),
              map_coords$server("map_coords"),
              map_ggoptions$server("map_ggoptions"))
      args$show_coords<-input$show_coords
      args
    })

    observe({
      shinyjs::toggle("coords_options", condition=isTRUE(input$show_coords))
    })
    args_for_map1<-reactive({
      # if(is.null(vals$data_map)) {return(NULL)}
      n<-vals$data_map[, 1] |> unique() |> length()
      req(color_args1())
      palette<-color_args1()$pal
      fillOpacity<-color_args1()$fillOpacity
      reverse_palette<-color_args1()$reverse_palette
      light=color_args1()$light
      req(palette %in% names(vals$newcolhabs))
      pal<-vals$newcolhabs[[palette]](n)
      if (isTRUE(reverse_palette)) {
        pal<-rev(pal)
      }
      shape_args<-get_shapes()
      if (isTRUE(raster)) {
        req(rst())
      }
      args<-c(list(data = vals$data_map, pal = pal, fillOpacity = fillOpacity, rst = rst()), circle_args1(), radius_args1(), breaks_args1(), leaflet_opts1(), args_pie1(), shape_args,light=light)
      args$args_extra_shape<-extra_shapes()$args_extra
      if(isTRUE(pie)){
        args$addCircles<-F
      }
      if(isTRUE(circles)){
        args$addCircles<-T
      }

      args$palette<-palette
      args$newcolhabs<-vals$newcolhabs
      args$data_depth<-extra_shapes()$data_depth
      args$args_labels<-args_labels()
      if(!is.factor(vals$data_map[,1])){
        req(input$custom_breaks)
        args$custom_breaks<-strsplit(input$custom_breaks,",")[[1]]
      }

      args
    })
    args_for_map2<-reactive({
      # req(args_for_map1())
      if(is.null(vals$data_map)) {return(NULL)}
      args_ggplot<-NULL
      args_ggplot<-c(args_gg())
      if (isTRUE(interp) | isTRUE(coki) | isTRUE(raster)) {
        args = args_for_map1()["rst"]
        req(rst())
        color_args<-color_ggplot()
        shape_args<-get_shapes()
        args<-c(args, color_args, shape_args, args_ggplot)
      } else {
        args = args_for_map1()
        args$rst<-vals$data_map
        args$pal<-NULL
        args$fillOpacity<-NULL
        color_args<-color_ggplot()
        args<-c(args, color_args, args_ggplot)
      }
      args<-c(args,args_labels())
      args$args_extra_shape<-extra_shapes()
      if(!is.factor(vals$data_map[,1])){
        req(input$custom_breaks)
        args$custom_breaks<-strsplit(input$custom_breaks,",")[[1]]
      }


      args[unique(names(args))]
    })



    output$plot_from_leaflet<-renderLeaflet({
      validate(need(!inherits(args_map1()$rst,"try-error"),args_map1()$rst[1]))
      if(isTRUE(interp)){
        req(vals$interp_args)
        if(vals$interp_args$interp_type=="krige"){
          validate(need(!inherits(vals$interp_vgm,"try-error"),vals$interp_vgm[1]))
          validate(need(!inherits(vals$interp_gstat,"try-error"),vals$interp_gstat[1]))
        }
      }


      #validate_data(vals$data_map)
      map_result1_final()
    })

    output$plot_from_leaflet2<-renderLeaflet({
      #validate_data(vals$data_map)
      map_result1_final()
    })

    observe({
      if(is.null(map_result1_final())){
        shinyjs::hide('zoom_leaflet_btn')
      } else{
        shinyjs::show('zoom_leaflet_btn')
      }
    })
    observeEvent(input$zoom_leaflet,{
      showModal(
        tags$div(id="modal_leaflet",
                 modalDialog(
                   size ="xl",
                   leafletOutput(ns("plot_from_leaflet2"), height  = 600),
                   easyClose = T
                 )
        )

      )
    })


    observeEvent(vals$data_map,{
      #req(vals$data_map)
      map_labels$server_update("map_labelss",vals,scatter3d)
    })






    output$plot_from_ggplot<-renderPlot({
      #validate_data(vals$data_map)
      p<-map_result2_final()
      p

    })

    observeEvent(vals$data_map,{
      #req(vals$data_map)
      ggcontrol<-showlabels$server_update("leaflet_label_map",vals=vals,selected_gglabel())
      selected_gglabel(ggcontrol()$labels)
    })





    ##

    observeEvent(input$mode_plot,{
      id_base<-names(input)[grep("shapes1-base_shape",names(input))]
      id_base_fill<-names(input)[grep("shapes1-base_fillOpacity",names(input))]
      id_base_w<-names(input)[grep("shapes1-base_weight",names(input))]
      id_layer<-names(input)[grep("shapes1-layer_shape",names(input))]
      if(isTRUE(interp)){
        updateCheckboxInput(session,id_base,value=T)
        updateCheckboxInput(session,id_base,value=T)
        updateNumericInput(session,id_base_fill,value=0)
        updateNumericInput(session,id_base_w,value=0)
        if(input$mode_plot=='ggplot'){
          updateCheckboxInput(session,id_layer,value=T)
        } else{updateCheckboxInput(session,id_layer,value=F)}

      } else if(isTRUE(raster)){
        updateCheckboxInput(session,id_base,value=F)
        if(input$mode_plot=='ggplot'){
          updateCheckboxInput(session,id_layer,value=T)
        } else{updateCheckboxInput(session,id_layer,value=F)}
      } else{
        if(input$mode_plot=='ggplot'){
          updateCheckboxInput(session,id_base,value=T)
          updateCheckboxInput(session,id_layer,value=T)
        } else{
          updateCheckboxInput(session,id_base,value=F)
          updateCheckboxInput(session,id_layer,value=F)
        }
      }

    })
    observeEvent(input$pie_chart_help,{
      showModal(
        modalDialog(
          title="Pie chart",
          pie_chart_help,
          easyClose = T
        )
      )
    })

    ##

    get_shapes<-reactive({
      ll_shapes$server("shapes1", vals)
    })
    color_args1<-ll_colors$server('color1',vals)
    leaflet_opts1<-ll_options$server("options1")
    chart_args1<-reactiveVal()
    radius_args1<-reactive({ll_radius$server("radius1")})

    rst<-reactiveVal()




    rst_interp<-reactiveVal()
    factor_chart<-reactiveVal()

    observe({
      req(isTRUE(pie))
      args_pie1(ll_pie$server("pie1"))
      facpie<-args_pie1()$factor_chart
      factor_chart(facpie)
    })
    args_pie1<-reactiveVal()
    args_map1<-reactiveVal()
    args_map2<-reactiveVal()
    observeEvent(args_for_map1(), {
      addClass("run_map_btn", "save_changes")
    })


    krige_cv<-reactive({

      interp_type<-vals$interp_args$interp_type
      req(interp_type)
      req(vals$interp_gstat)
      if(interp_type=="krige")
        withProgress(min=NA,max=NA,message="Running...",{

          g<-vals$interp_gstat
          observed<-data.frame(lapply(g$data,function(x) data.frame(x$data)[3]))
          if(is.na(seed)){
            set.seed(NULL)
          } else{
            set.seed(seed)}
          out = gstat::gstat.cv(g,nfold = input$nfold,all.residuals=T)
          attr(out,"obs")<-observed
          attr(g,"cv")<-out
          vals$g_final_cv<-g

        })

    })

    cvkrige<-reactive({
      req(vals$interp_args)
      req(vals$interp_gstat)
      if(vals$interp_args$interp_type%in%"krige"){
        g<-vals$interp_gstat
        observed<-data.frame(lapply(g$data,function(x) data.frame(x$data)[3]))
        seed<-vals$interp_args$seed
        cv<-vals$interp_args$cv
        if(is.na(seed)){
          set.seed(NULL)
        } else{
          set.seed(seed)}
        out = gstat::gstat.cv(g,nfold = cv,residuals=T)
        attr(out,"obs")<-observed
        attr(g,"cv")<-out
        vals$g_final_cv<-g


      }
    })

    observeEvent(input$run_map,ignoreInit = T,{
      req(isFALSE(surface))

      if (isTRUE(raster)) {

        withProgress(message = "Running", min = NA, max = NA, isolate(raster_args()))

      }
      if (isTRUE(interp)) {
        # krige_cv()
        withProgress(message = "Running", min = NA, max = NA, interpolation_args())
        cvkrige()
      }
      if (input$mode_plot == "leaflet") {

        args_map1(args_for_map1())
      } else if (input$mode_plot == "ggplot") {
        args_map2(args_for_map2())
      }

      shinyjs::show("save_png")
      shinyjs::show("all_download_buttons")


    })
    observeEvent(args_map1(), {
      removeClass("run_map_btn", "save_changes")
    })
    observeEvent(args_map2(), {
      removeClass("run_map_btn", "save_changes")
    })
    observeEvent(args_map1(), {
      args<-args_map1()

      validate(need(!inherits(args_map1()$rst,"try-error"),args_map1()$rst[1]))


      map_result1(do.call(map_discrete, args))
      map_result1_final(map_result1())
    })
    observeEvent(args_map2(), {
      args<-args_map2()
      args$factor<-is.factor(vals$data_map[,1])
      args$data_o<-vals$data_map




      map_result2(do.call(gg_rst, args))
      map_result2_final(map_result2())
    })
    map_result1<-reactiveVal()
    map_result1_final<-reactiveVal()
    map_ggplot<-reactiveVal()
    map_ggplot_final<-reactiveVal()
    map_result2_final<-reactiveVal()
    map_result2<-reactiveVal()


    ##


    triggler_snap_plotly<-  function(id,out_id){
      shinyjs::runjs(paste0(
        "
      var plot = document.getElementById('",paste0(id,"-",'out_id'),"');
      if (plot) {
        Plotly.downloadImage(plot, {format: 'png', filename: 'plot_snapshot'});
      }
    "
      ))
    }




    observeEvent(input$down_plotly,ignoreInit = T,{
      if(isTRUE(surface)){
        out_id='plotly_surface_out'
      } else if(isTRUE(stack)){
        out_id='plotly_stack_out'

      } else if(isTRUE(scatter3d)){
        out_id='plotly_scatter3d_out'
      }
      plotly_download$ui(ns("d-plotly"))
      plotly_download$server("d-plotly",out_id=out_id,session_in=session$ns(""))
    })



    observe({
      shinyjs::toggle("down_btn_generic", condition=input$mode_plot=="plot3D")
      shinyjs::toggle("down_btn_ll", condition=input$mode_plot=="leaflet")
      shinyjs::toggle("down_btn_plotly", condition=input$mode_plot=="plotly")
      shinyjs::toggle("down_btn_gg", condition=input$mode_plot=="ggplot")
    })

    observeEvent(map_result2_final(),{
      removeClass("divnull","down_btn_gg")
    })
    ll_down_plot$server('save_png', map=map_result1_final())
    observeEvent(map_result2_final(),{
      ll_down_gg$server('gg_png', map=map_result2_final())
    })
    color_ggplot<-reactive({
      pal<-color_args1()$pal
      fillOpacity<-color_args1()$fillOpacity
      reverse_palette<-color_args1()$reverse_palette
      light<-color_args1()$light
      newcolhabs<-vals$newcolhabs
      list(
        pal=pal,
        fillOpacity=fillOpacity,
        reverse_palette=reverse_palette,
        newcolhabs=vals$newcolhabs,
        light=light
      )})
    selected_gglabel<-reactiveVal()



    args_labels<-showlabels$server('leaflet_label_map',vals)




    return(NULL)

  })

}




#' @export
llet<-list()
#' @export
llet$ui<-function(id){
  ns<-NS(id)
  div(    # actionButton(ns("savebug"),"save bug"),
    tags$style(HTML(
      "



      .switch_box {

          margin: 0px;
        padding: 0px;
        }

        .checktitle .checkbox {


        margin-top: -1px
        }

        .checktitle .shiny-input-container{
  background: transparent;
        margin-left: -30px;
        padding-left: 10px
        }

        .checktitle .checkbox input[type=checkbox] {
        margin-left: -22px;

        }
        .checktitle .checkbox span {
             mdargin-left: -15px;
        }


        .switch_box .shiny-input-container:not(.shiny-input-container-inline) {

        margin: 0px;
        padding: 0px;
  max-width: 180px;
        }

        "
    )),
  tags$div(
    hidden(uiOutput(ns('coordinates_message'))),
    class='nav_map',
    div(id=ns("map_tabs"),
        navbarPage(
          NULL,id=ns("mode"),
          tabPanel("Circles",value="circles",
                   ll_map$ui(ns("circles"), circles=T,radius=T),
                   uiOutput(ns("map_circles"))
          ),
          tabPanel("Pies",value="pies",
                   ll_map$ui(ns("pie"), pie=T,radius=T),
                   uiOutput(ns("map_pie"))
          ),
          tabPanel("Scatter3D",value="scatter3d",
                   div(
                     ll_map$ui(ns("scatter3d"), scatter3d=T),
                     uiOutput(ns("map_scatter3d"))
                   )
          ),

          tabPanel("Raster",value="raster",
                   ll_map$ui(ns("raster"), raster=T),
                   uiOutput(ns("map_raster"))
          ),
          tabPanel("Interpolation",value="interp",
                   ll_map$ui(ns("interp"), interp=T),
                   uiOutput(ns("map_interp"))
          ),
          tabPanel("Surface (3D)",value="surface",
                   uiOutput(ns("map_surface_validate")),
                   div(
                     id=ns("surface_page"),
                     ll_map$ui(ns("surface"), surface=T),
                     uiOutput(ns("map_surface"))
                   )
          ),
          tabPanel("Stack (3D)",value="stack",
                   uiOutput(ns("map_stack_validate")),
                   div(
                     id=ns("stack_page"),
                     ll_map$ui(ns("stack"), stack=T),
                     uiOutput(ns("map_stack"))
                   )
          )



        ))




  )



  )
}
#' @export
llet$server<-function(id,vals){
  moduleServer(id,function(input, output, session) {
    #data_plot=NULL




    observe({
      shinyjs::toggle("surface_page",condition=length(vals$saved_maps)>0)
      shinyjs::toggle("stack_page",condition=length(vals$saved_maps)>0)
    })
    output$map_stack_validate<-renderUI({
      req(!length(vals$saved_maps)>0)
      emgray("This functionality requires at least one saved raster from the Raster or Interpolation tabs.")
    })
    output$map_surface_validate<-renderUI({
      req(!length(vals$saved_maps)>0)
      emgray("This functionality requires at least one saved raster from the Raster or Interpolation tabs.")
    })

    observeEvent(input$mode,{
      vals$cur_mode_map<-input$mode
    })
    output$map_surface<-renderUI({

      ll_map$server("surface",
                    vals=vals,
                    surface=T)


    })
    output$map_stack<-renderUI({

      ll_map$server("stack",
                    vals=vals,
                    stack=T)


    })
    output$map_scatter3d<-renderUI({

      ll_map$server("scatter3d",
                    vals=vals,
                    scatter3d=T)


    })
    output$coordinates_message<-renderUI({
      em("The selected Datalist does not contain any coordinates. Use Databank to include coordinates in your selection.",style="color:gray")
    })



    observeEvent(vals$data_map,{
      coords<-attr(vals$data_map,"coords")
      if(is.null(coords)){
        shinyjs::hide('map_tabs')
        shinyjs::show('coordinates_message')
      } else {
        shinyjs::show('map_tabs')
        shinyjs::hide('coordinates_message')
      }

    })

    data<-reactiveVal()
    cur_vars_choices<-reactiveVal()

    factor_chart<-reactiveVal()
    observeEvent(input[["pie-pie1-factor_chart"]],{
      factor_chart(input[["pie-pie1-factor_chart"]])
    })
    update_factor_chart<-reactive({

      data<-vals$data_map
      factor_chart<-factor_chart()
      factors<-attr(data,"factors")
      selected<-NULL
      if(!is.null(factor_chart)){
        selected=factor_chart
        choices<-colnames(factors)
        if(!is.null(selected)) {if(!selected %in% choices){
          selected<-NULL
        }}
      }
      updatePickerInput(session,'pie-pie1-factor_chart',choices=colnames(factors),selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(list(input[['pie-pie1-factor_chart']], vals$data_map),{
      update_factor_chart()
    })

    output$map_circles<-renderUI({

      # req(is.data.frame(data_map))
      ll_map$server("circles",vals=vals, circles=T, pie=T)

    })
    output$map_pie<-renderUI({

      # req(is.data.frame(data_map))
      ll_map$server("pie",vals=vals, pie=T)

    })
    output$map_interp<-renderUI({
      # data_map<-vals$data_map
      #req(is.data.frame(data_map))
      ll_map$server("interp", vals=vals,interp=T)


    })

    output$map_raster<-renderUI({

      ll_map$server("raster",
                    vals=vals,
                    raster=T)


    })


  })
}



