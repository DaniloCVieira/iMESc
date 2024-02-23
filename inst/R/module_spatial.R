options(shiny.autoload.r=FALSE)
#' @noRd
#' @importFrom webshot webshot
#' @importFrom geodist geodist
#' @importFrom dplyr summarise group_by across
#' @importFrom leaflet.minicharts popupArgs addMinicharts
#' @importFrom scatterpie geom_scatterpie
#' @importFrom raster crs rasterToPoints coordinates extent ratify raster rasterize `crs<-` values `values<-`
#' @importFrom sp spsample `coordinates<-` CRS `proj4string<-` `gridded<-` `fullgrid<-` proj4string SpatialGridDataFrame
check_inputs<-function(input,pattern){
  res<-names(input)[grep(pattern,names(input))]
  res
}
ll_data<-list()
ll_data$ui<-function(id){
  ns<-NS(id)
  div(

    div(class="map_header",
        pickerInput(ns("data_map"),"Y Datalist:",
                    choices =NULL,inline=T),
        pickerInput(ns("choices_map"),"Attribute:",
                    choices = c("Numeric-Attribute","Factor-Attribute"),inline=T),
        pickerInput(ns("var_map"),label = "Variable:",choices = NULL,inline=T),
        pickerInput(ns("factor_filter"),label = "Filter",choices = NULL,inline=T),
        pickerInput(ns("level_filter"),label = "Level",choices = NULL,inline=T)

    )

  )
}
ll_data$server<-function(id, vals){
  moduleServer(id,function(input, output, session) {

    observeEvent(vals$saved_data,{
      selected<-NULL
      if(!is.null(vals$cur_data)){
        if(vals$cur_data%in%names(vals$saved_data)){
          selected<-vals$cur_data
        }
      }
      updatePickerInput(session,"data_map",choices=names(vals$saved_data), selected=selected)

    })
    data0<-reactive({
      req(input$data_map)
      req(input$choices_map)
      data0<-vals$saved_data[[input$data_map]]
      if(input$choices_map=='Factor-Attribute'){
        data0<-attr(data0,"factors")
      }
      data0
    })
    observeEvent(data0(),{

      choices<-colnames(data0())
      selected<-NULL
      if(!is.null(vals$cur_var_map)){
        if(vals$cur_var_map%in%choices){
          selected<-vals$cur_var_map
        }
      }
      updatePickerInput(session,"var_map",choices=choices, selected=selected)

    })


    get_factors<-reactive({
      req(input$data_map)
      data0<-vals$saved_data[[input$data_map]]
      factors<-attr(data0,"factors")
      factors
    })

    observeEvent(input$data_map,{
      choices<-colnames(get_factors())
      selected<-NULL
      if(!is.null(vals$cur_factor_filter)){
        if(vals$cur_factor_filter%in%choices){
          selected<-vals$cur_factor_filter
        }
      }
      updatePickerInput(session,"factor_filter",choices=c("None",choices),selected=selected)
    })

    get_filter<-reactive({
      req(input$data_map)
      data0<-vals$saved_data[[input$data_map]]
      factors<-get_factors()
      req(input$factor_filter%in%colnames(factors))
      fac<-factors[input$factor_filter]
      fac
    })

    observeEvent(input$factor_filter,{
      if(input$factor_filter=="None"){
        shinyjs::hide('level_filter')
      } else{
        shinyjs::show('level_filter')
      }
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
  })
}
ll_data$server_inputs<-function(id){
  moduleServer(id,function(input, output, session) {



    return(list(
      name=input$data_map,
      attr=input$choices_map,
      var=input$var_map,
      filter=input$factor_filter,
      filter_level=input$level_filter
    ))
  })
}
ll_colors<-list()
ll_colors$ui<-function(id,colors_img){
  choices = colors_img$val
  choicesOpt = list(content = colors_img$img)

  ns<-NS(id)
  div(
    strong("Colors", style="color: #3c8dbc"),
    tags$div(class="map_side",
             pickerInput(inputId = ns("palette"),
                         label ="Palette",
                         choices =choices,
                         choicesOpt =choicesOpt,
                         selected=NULL,
                         options=list(container="body")),
             numericInput(ns("fillOpacity"),'Fill opacity',min=0,max=1,0.8,step=0.1),
             checkboxInput(ns("reverse_palette"),"Reverse palette",F)
    )



  )

}
ll_colors$server<-function(id){
  moduleServer(id,function(input, output, session) {
    return(
      reactive({
        list(pal=input$palette,fillOpacity=input$fillOpacity,reverse_palette=input$reverse_palette)
      })
    )
  })
}
ll_circles<-list()
ll_circles$ui<-function(id){
  ns<-NS(id)
  div(
    checkboxInput(ns("addCircles"),"Circles",T),
    tags$div(id=ns("circle_options"),class="map_side",
             checkboxInput(ns("scale_radius"), "Scale radius",T)
    )
  )
}
ll_circles$server<-function(id,data){
  moduleServer(id,function(input, output, session) {
    req(data)
    req(nrow(data)>0)
    if(is.factor(data[,1])){
      shinyjs::hide('scale_radius')

    } else{
      shinyjs::show('scale_radius')

    }

    observeEvent(input$addCircles,{
      if(isTRUE(input$addCircles)){
        shinyjs::show('circle_options')
      } else{
        shinyjs::hide('circle_options')
      }
    })

    return(
      list(
        addCircles=input$addCircles,
        scale_radius=input$scale_radius

      )
    )

  })
}
ll_radius<-list()
ll_radius$ui<-function(id){
  ns<-NS(id)
  div(
    strong("Radius", style="color: #3c8dbc"),
    tags$div(class='map_side',

             div(

               numericInput(ns("min_radius") ,label ="Min",1)
             ),
             div(

               numericInput(ns("max_radius") ,label ="Max",5)
             )
    )
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
    strong("Breaks", style="color: #3c8dbc"),
    tags$div(class="map_side",
             numericInput(ns("nbreaks"), "n",5))
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
    tags$div(id=ns("chart_options"),class="map_side",
             numericInput(ns("buffer_zize") ,
                          label =span("buffer (km)",tiphelp("Must be higher than 0")),
                          0.001),
             pickerInput(ns("factor_chart") ,
                         label ="factor",
                         NULL)

    )
  )
}
ll_pie$server_update<-function(id,data,factor_chart=NULL){
  moduleServer(id,function(input, output, session) {})




}
ll_pie$server<-function(id){
  moduleServer(id,function(input, output, session) {
    observeEvent(input$addMinicharts,{
      if(isTRUE(input$addMinicharts)){
        shinyjs::show('chart_options')
      } else{
        shinyjs::hide('chart_options')
      }
    })

    observeEvent(input$buffer_zize,{
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
ll_shapes$ui<-function(id,sf_in,base_on=T){
  lab<-"Shapes"
  if(isTRUE(sf_in)){
    lab<-c("Clip")
  }
  ns<-NS(id)
  div(

    strong(lab,syle="color",style="color: #3c8dbc"),
    tags$div(class="map_side",   id=ns("shape_options"),
             checkboxInput(ns("base_shape") ,label ="Base Shape",base_on),

             tags$div(id=ns('base_options'), class="map_side",style="display:none",
                      colourpicker::colourInput(ns('base_color'),"color","white"),
                      checkboxInput(ns("base_fill") ,label ="Fill",T),
                      tags$div(class="map_side",numericInput(ns("base_fillOpacity") ,label ="Opacity",1, min=0,max=1))
                      ,
                      checkboxInput(ns("base_stroke") ,label ="Border",T),
                      tags$div(class="map_side",
                               numericInput(ns("base_weight"),'Width',min=0,1,step=.5))

             ),
             checkboxInput(ns("layer_shape") ,label ="Layer Shape",F),
             tags$div(id=ns('layer_options'), class="map_side",style="display:none",

                      colourpicker::colourInput(ns('layer_color'),"color","#C0C2C1"),
                      checkboxInput(ns("layer_fill") ,label ="Fill",T),
                      tags$div(class="map_side",numericInput(ns("layer_fillOpacity") ,label ="Opacity",1)),
                      checkboxInput(ns("layer_stroke") ,label ="Border",T),
                      tags$div(class="map_side",numericInput(ns("layer_weight"),'Width',min=0,1,step=.5))

             )



    )
  )
}
ll_shapes$server_update1<-function(id, data,sf_in){
  moduleServer(id,function(input, output, session) {

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

    if(!is.null(data)){
      base_shape<-attr(data,"base_shape")
      layer_shape<-attr(data,"layer_shape")
      if(is.null(base_shape) & is.null(layer_shape) ){
        shinyjs::hide('shape_options')
      } else{
        shinyjs::show('shape_options')
      }

      if(is.null(base_shape)){
        updateCheckboxInput(session,'base_shape',value=F)
      }
      if(is.null(layer_shape)){
        updateCheckboxInput(session,'layer_shape',value=F)

      } else{

      }
    }


  })
}
ll_shapes$server<-function(id){
  moduleServer(id,function(input, output, session) {


    observeEvent(input$base_shape,{
      if(isTRUE(input$base_shape)){
        shinyjs::show('base_options')
      } else{
        shinyjs::hide('base_options')
      }
    })
    observeEvent(input$layer_shape,{
      if(isTRUE(input$layer_shape)){
        shinyjs::show('layer_options')
      } else{
        shinyjs::hide('layer_options')
      }
    })
    result<-reactive({
      list(
        base_shape_args=list(
          fillOpacity=input$base_fillOpacity,
          shape=input$base_shape,
          color=input$base_color,
          weight=input$base_weight,
          stroke=input$base_stroke,
          fill=input$base_fill
        ),
        layer_shape_args=list(
          fillOpacity=input$layer_fillOpacity,
          shape=input$layer_shape,
          color=input$layer_color,
          weight=input$layer_weight,
          stroke=input$layer_stroke,
          fill=input$layer_fill
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
    pickerInput(ns("providers"),
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
ll_down_modal$server<-function(id, map){


  moduleServer(id,function(input, output, session) {
    output$download<-{
      downloadHandler(
        filename = function() {
          paste("leaflet",Sys.Date(), ".png", sep = "")
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
    actionButton(ns("download_ll"),span(icon("download")))
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
    observeEvent(input$download_gg,{
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

  ns<-NS(id)
  div(
    div(id=ns("i_numeric"),
        strong("Krige",tiphelp("Uses function idw from gstat package")),
        tags$div(class="map_side",
                 numericInput(ns("resolution"), lab_resolution, 5000, min = 20)),
        tags$div(class="map_side",id=ns("interp_params"),
                 numericInput(ns("nmax"), lab_nmax, NA, min = NA),
                 numericInput(ns("nmin"), lab_nmin, 3, min = 0),
                 numericInput(ns("omax"), lab_omax, 0, min = 0),
                 numericInput(ns("maxdist"), lab_maxdist, NA, min = 0),
                 numericInput(ns("idp"), lab_idp, 4, min = 0)
        )),
    div(id=ns("i_factor"),
        strong("KNN"),
        tags$div(class="map_side",
                 numericInput(ns("k"), lab_k, 4, min = 0)))
  )

}
ll_interp$server<-function(id,data){

  moduleServer(id,function(input, output, session) {



    if(!is.null(data[,1])){
      if(is.factor(data[,1])){
        shinyjs::hide('i_numeric')
        shinyjs::show('i_factor')
      } else{

        shinyjs::show('i_numeric')
        shinyjs::hide('i_factor')

      }
    }



    result<-reactive({
      list(
        nmax=input$nmax,
        nmin=input$nmin,
        omax=input$omax,
        maxdist=input$maxdist,
        idp=input$idp,
        k=input$k,
        resolution=input$resolution
      )
    })

    return(result)

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
  for(i in 1:length(shapes)){
    atributos_shp<-attributes(shp[[i]])$names
    res[[i]]<-list(
      tags$div(
        class="map_side",
        checkboxInput(ns(paste0("map_extra_layer",i)),
                      paste("Extra-Layer",i), F),
        tags$div(class="map_side",id=ns(paste0("eshape_id",i)),
                 colourpicker::colourInput(ns(paste0("ssextra_layer_col",i)),label="Color",value="#2E8B5740"),
                 numericInput(ns(paste0("ssextra_layer_lighten",i)),
                              'Transp',value=0.6,  min=0, max=1,step=0.1),
                 pickerInput(ns(paste0("feat_extra",i)),
                             paste("Label",i),choices=c("None",atributos_shp)),
                 numericInput(ns(paste0("feat_extra_size",i)),
                              'size',value=2,  min=0, max=1, step=0.1),
                 uiOutput(ns("data_depth"))
        )


      )
    )


  }

  div(
    res,
    tags$div(
      class="map_side",
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
          div(class="map_control_style2",
              style="height: 400px; overflow-y: scroll",

              textInput(ns("main"),label="Title"),
              tags$div(class="map_side",
                       numericInput(ns("plot.title_size"),label="Size",value=13)),
              textInput(ns("subtitle"),label="Subtitle"),
              tags$div(class="map_side",
                       numericInput(ns("plot.subtitle_size"),label="Size",value=13)),

              actionLink(ns('axis'),"Axes"),
              tags$div(class="map_side",
                       numericInput(ns("axis.text_size"),label="Text Size",value=13),
                       numericInput(ns("axis.title_size"),label="Title Size",value=13),

              ),

              checkboxInput(ns("show_coords"),label="Show Coordinates",value=FALSE),
              tags$div(class="map_side", id=ns("show_coords_on"),
                       colourpicker::colourInput(ns("col.coords"),label="Color",value="black"),
                       numericInput(ns("cex.coords"),label="Size",value=3))
              ,
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
gg_control$update_server<-function(id,data_map){
  moduleServer(id,function(input, output, session) {
    updateTextInput(session,'main',value=colnames(data_map))

  })
}
gg_control$server<-function(id){
  moduleServer(id,function(input, output, session) {

    observeEvent(input$show_coords,{
      if(isTRUE(input$show_coords)){
        shinyjs::show('col.coords')
        shinyjs::show('cex.coords')
      } else{
        shinyjs::hide('col.coords')
        shinyjs::hide('cex.coords')
      }
    })
    return(
      reactive({
        list(

          main=input$main,
          subtitle=input$subtitle,
          axis.text_size=input$axis.text_size,
          axis.title_size=input$axis.title_size,
          plot.title_size=input$plot.title_size,
          plot.subtitle_size=input$plot.subtitle_size,
          legend.text_size=input$legend.text_size,
          show_coords=input$show_coords,
          col.coords=input$col.coords,
          cex.coords=input$cex.coords,
          show_guides=input$show_guides,
          lighten=input$lighten,
          keyscale=input$keyscale,
          width_hint=input$width_hint,
          cex_scabar=input$cex_scabar,
          show=input$show_ggcontrol
        )
      })
    )

  })
}

showlabels<-list()
showlabels$ui<-function(id){
  ns<-NS(id)
  div(
    checkboxInput(ns("show_labels"),label=strong("Labels"),value=FALSE),
    tags$div(class="map_side", id=ns("show_labels_on"),style="display: none",
             pickerInput(ns("labels"),label="Use factor",choices=c(NULL)),
             numericInput(ns("cex.fac"),label="Size",value=5),
             colourpicker::colourInput(ns("col.fac"),label="Color",value="black")
    )
  )
}
showlabels$server_update<-function(id,vals,selected_labels=NULL){
  moduleServer(id,function(input,output,session){

    observeEvent(input$show_labels,{
      if(isTRUE(input$show_labels)){
        shinyjs::show('show_labels_on')
      } else{
        shinyjs::hide('show_labels_on')
      }
    })




    factors<-attr(vals$data_map,"factors")

    selected<-NULL
    if(!is.null(selected_labels)){
      selected=selected_labels
      choices<-colnames(factors)
      if(!is.null(selected)) {if(!selected %in% choices){
        selected<-NULL
      }}
    }

    observeEvent(vals$data_map,{
      updatePickerInput(session,'labels',choices=colnames(factors),selected=selected)
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
showlabels$server<-function(id){
  moduleServer(id,function(input,output,session){
    return(
      reactive({
        list(
          show_labels=input$show_labels,
          labels=input$labels,
          cex.fac=input$cex.fac,
          col.fac=input$col.fac
        )
      })
    )
  })
}


ll_map<-list()
ll_map$ui<-function(id, circles=F, pie=F, radius=F,raster=F,interp=F, coki=F, shapes=T,colors_img){
  sf_in=any(c(interp,coki))
  ns<-NS(id)
  div(

    sidebarLayout(

      sidebarPanel(style="min-height: 600px;",
                   div(style="position: absolute; top: 0px; right: 0px",
                       div(class="save_changes",id=ns("run_leaflet_btn1"),actionButton(ns("run_leaflet1"),span(icon("angles-right"),icon("map")),class='button_run_map'))),
                   hidden(div(

                     id=ns('all_download_buttons'),
                     div(style="position: absolute; top: 0px; right: -195%; z-index: 999",
                         hidden(div(id=ns("down_btn_ll"),
                                    ll_down_plot$ui(ns("save_png"))))
                     ),
                     div(style="background: red;position: absolute; top: 0px; right: -195%; z-index: 999",
                         hidden(div(id=ns("down_btn_gg"),
                                    ll_down_gg$ui(ns("gg_png"))))
                     )
                   )),

                   div(class="map_control_style2",style="height: 450px; overflow-y: scroll",
                       div(
                         if(isTRUE(interp)){
                           ll_interp$ui(ns('interp1'), coki=coki)
                         },
                         if(isTRUE(pie)){
                           strong("Pie chart",tipify(actionLink(ns("pie_chart_help"),icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Click for details"))
                         },

                         ll_colors$ui(ns('color1'),colors_img=colors_img),
                         if(isTRUE(radius)){
                           ll_radius$ui(ns('radius1'))
                         },

                         div(id=ns("show_breaks"),
                             ll_breaks$ui(ns('breaks1'))
                         ),
                         showlabels$ui(ns('leaflet_label_map')),
                         if(isTRUE(circles)){
                           ll_circles$ui(ns('circles1'))
                         },
                         if(isTRUE(pie)){
                           ll_pie$ui(ns("pie1"))},

                         div(
                           id=ns("shapes_leaflet"),
                           ll_shapes$ui(ns("shapes1"),sf_in,base_on=F)
                         ),

                         uiOutput(ns('extraLayers_output'))

                       )


                   )),
      mainPanel(
        div(
          navbarPage(id=ns('mode_plot'),
                     title=NULL,selected="leaflet",
                     tabPanel("Leaflet",value="leaflet",

                              span(id=ns("zoom_leaflet_btn"),style="display: none",
                                   inline(ll_options$ui(ns("options1"))),
                                actionButton(ns("zoom_leaflet"),icon("magnifying-glass-plus"))
                              ),
                              bsTooltip(ns('zoom_leaflet'),"Shows the Leafmap map in a modal Window"),



                              leafletOutput(ns("plot_from_leaflet"), height  = 500)),
                     tabPanel("GGplot",value="ggplot",
                              checkboxInput(ns("show_ggcontrol"),"Customize",F),
                              div(class="div",
                                  div(
                                    id=ns('side_gg'),
                                    gg_control$ui(ns('gg_control1'))
                                  ),
                                  div(class='col-sm-12',
                                      id=ns('main_gg'),
                                      uiOutput(ns("plot_from_ggplot")))
                              )
                     )
          )



        )
      )
    ))
}
ll_map$server<-function(id, raster=F, interp=F, coki=F,vals){
  moduleServer(id,function(input, output, session) {

    observeEvent(vals$data_map,{
      if(!is.null(vals$data_map)){
        if(is.factor(vals$data_map[,1]))
          shinyjs::hide('show_breaks')
      } else{
        shinyjs::show('show_breaks')
      }

    })

    ns<-session$ns
    ##
    extra_shapes<-reactive({
      #if(is.null(vals$data_map)) {return(NULL)}
      shapes_extra$server("extra", vals$data_map)
    })
    output$extraLayers_output<-renderUI({
      #if(is.null(vals$data_map)) {return(NULL)}
      shapes_extra$ui(session$ns("extra"), vals$data_map)
    })
    observe({

      sf_in = any(c(interp, coki))
      ll_shapes$server_update1("shapes1", vals$data_map, sf_in)


    })
    circle_args1<-reactive({
      #if(is.null(vals$data_map)) {return(NULL)}
      ll_circles$server("circles1", vals$data_map)
    })
    raster_args<-reactive({
      # if(is.null(vals$data_map)) {return(NULL)}
      if (isTRUE(raster)) {
        shape_args<-get_shapes()
        cut_shape<-shape_args$base_shape_args$shape
        args<-list(data=vals$data_map, cut_shape = cut_shape)
        #args<-readRDS("args.rds")
        #print("saved")
        rst<-do.call(map_rst,args)
        rst<-migrate_rst(rst, vals$data_map)
        rst(rst)
      }
    })
    observeEvent(vals$data_map, {
      # if(is.null(vals$data_map)) {return(NULL)}
      ll_interp$server("interp1", vals$data_map)
    })
    interpolation_args<-reactive({
      # if(is.null(vals$data_map)) {return(NULL)}
      if (isTRUE(interp)) {
        args<-interp_args()
        args$data<-vals$data_map
        if (is.na(args$nmax)) {
          args$nmax<-NULL
        }
        if (is.na(args$maxdist)) {
          args$maxdist<-NULL
        }
        shape_args<-get_shapes()
        args$cut_shape<-shape_args$base_shape_args$shape
        rst<-do.call(interp_leaflet, args)
        rst<-migrate_rst(rst, vals$data_map)
        rst(rst)
      }
    })
    args_for_map1<-reactive({
      # if(is.null(vals$data_map)) {return(NULL)}
      n<-vals$data_map[, 1] |> unique() |> length()
      req(color_args1())
      palette<-color_args1()$pal
      fillOpacity<-color_args1()$fillOpacity
      reverse_palette<-color_args1()$reverse_palette
      req(palette %in% names(vals$newcolhabs))
      pal<-vals$newcolhabs[[palette]](n)
      if (isTRUE(reverse_palette)) {
        pal<-rev(pal)
      }
      shape_args<-get_shapes()
      if (isTRUE(raster)) {
        req(rst())
      }
      args<-c(list(data = vals$data_map, pal = pal, fillOpacity = fillOpacity, rst = rst()), circle_args1(), radius_args1(), breaks_args1(), leaflet_opts1(), args_pie1(), shape_args)
      args$args_extra_shape<-extra_shapes()$args_extra
      args$data_depth<-extra_shapes()$data_depth
      args$args_labels<-args_labels()
      args
    })
    args_for_map2<-reactive({
      # req(args_for_map1())
      if(is.null(vals$data_map)) {return(NULL)}
      args_ggplot<-NULL
      if (isTRUE(input$show_ggcontrol)) {

        args_ggplot<-c(args_gg())

      }
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
      args
    })
    output$plot_from_leaflet<-renderLeaflet({
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
      ggcontrol<-gg_control$update_server("gg_control1",vals$data_map)
    })
    output$plot_from_ggplot<-renderUI({
      #validate_data(vals$data_map)
      p<-map_result2_final()
      renderPlot(p)
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
    observeEvent(input$show_ggcontrol,{
      if(isTRUE(input$show_ggcontrol)){
        removeClass('side_gg','divnull')
        addClass('side_gg','col-sm-4')
        removeClass('main_gg','col-sm-12')
        addClass('main_gg','col-sm-8')

      } else{
        removeClass('side_gg','col-sm-4')
        addClass('side_gg','divnull')
        removeClass('main_gg','col-sm-8')
        addClass('main_gg','col-sm-12')
      }
    })
    ##

    get_shapes<-reactive({
      ll_shapes$server("shapes1")
    })
    color_args1<-ll_colors$server('color1')
    leaflet_opts1<-ll_options$server("options1")
    chart_args1<-reactiveVal()
    radius_args1<-reactive({
      ll_radius$server("radius1")
    })
    breaks_args1<-reactive({
      ll_breaks$server("breaks1")
    })
    rst<-reactiveVal()
    interp_args<-ll_interp$server("interp1", NULL)
    rst_interp<-reactiveVal()
    factor_chart<-reactiveVal()
    observeEvent(ll_pie$server("pie1"), {
      args_pie1(ll_pie$server("pie1"))
      facpie<-args_pie1()$factor_chart
      factor_chart(facpie)
    })
    args_pie1<-reactiveVal()
    args_map1<-reactiveVal()
    args_map2<-reactiveVal()
    observeEvent(args_for_map1(), {
      addClass("run_leaflet_btn1", "save_changes")
    })
    observeEvent(input$run_leaflet1, {

      if (isTRUE(raster)) {

        withProgress(message = "Running", min = NA, max = NA, isolate(raster_args()))

      }
      if (isTRUE(interp)) {

        withProgress(message = "Running", min = NA, max = NA, interpolation_args())

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
      removeClass("run_leaflet_btn1", "save_changes")
    })
    observeEvent(args_map2(), {
      removeClass("run_leaflet_btn1", "save_changes")
    })
    observeEvent(args_map1(), {
      args<-args_map1()


      map_result1(do.call(map_discrete, args))

      map_result1_final(map_result1())
    })
    observeEvent(args_map2(), {
      args<-args_map2()
      args$data<-vals$data_map
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

    observe({
      if(input$mode_plot=="ggplot"){
        shinyjs::show("down_btn_gg")
        shinyjs::hide("down_btn_ll")
      } else{
        shinyjs::hide("down_btn_gg")
        shinyjs::show("down_btn_ll")

      }
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
      newcolhabs<-vals$newcolhabs
      list(
        pal=pal,
        fillOpacity=fillOpacity,
        reverse_palette=reverse_palette,
        newcolhabs=vals$newcolhabs
      )})
    selected_gglabel<-reactiveVal()
    args_gg<-gg_control$server("gg_control1")

    args_labels<-showlabels$server('leaflet_label_map')




    return(NULL)

  })

}

#' @export
llet<-list()
#' @export
llet$ui<-function(id,colors_img){
  ns<-NS(id)
  fluidPage(
    # actionButton(ns("savebug"),"save bug"),

    tags$style(HTML(".map_side {

    border-left: 1px dashed #3c8dbc;

    }
    .map_side label {
    position: relative;
    padding-left: 15px;
    margin-top: 1px

    }

.map_side label::before {
    content: '';
    position: absolute;
    top: -3px;
    bottom: calc(50% + 3px);
    left: 0;

      border-bottom: 1px dashed #3c8dbc;
    width: 10px;
}
.map_side{
  margin-left: 15px;

}


"

    )),


tags$div(
  hidden(uiOutput(ns('coordinates_message'))),
  class='nav_map',
  div(id=ns("map_tabs"),
      navbarPage(
        NULL,selected ="circles",id=ns("mode"),
        tabPanel("Circles",value="circles",
                 ll_map$ui(ns("circles"), circles=T,radius=T,colors_img=colors_img),
                 uiOutput(ns("map_circles"))
        ),
        tabPanel("Pies",value="pies",
                 ll_map$ui(ns("pie"), pie=T,radius=T,colors_img=colors_img),
                 uiOutput(ns("map_pie"))
        ),
        tabPanel("Kriging",value="interp",
                 ll_map$ui(ns("interp"), interp=T,colors_img=colors_img),
                 uiOutput(ns("map_interp"))
        ),

        tabPanel("Raster",value="raster",
                 ll_map$ui(ns("raster"), raster=T,colors_img=colors_img),
                 uiOutput(ns("map_raster"))
        )



      ))




)



  )
}
#' @export
llet$server<-function(id,vals){
  moduleServer(id,function(input, output, session) {
    #data_plot=NULL

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
      updatePickerInput(session,'pie-pie1-factor_chart',choices=colnames(factors),selected=selected)
    })
    observeEvent(list(input[['pie-pie1-factor_chart']], vals$data_map),{
      update_factor_chart()
    })

    output$map_circles<-renderUI({

      # req(is.data.frame(data_map))
      ll_map$server("circles",vals=vals)

    })
    output$map_pie<-renderUI({

      # req(is.data.frame(data_map))
      ll_map$server("pie",vals=vals)

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
