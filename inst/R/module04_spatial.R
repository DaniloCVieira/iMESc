
#' ## Overview of the Spatial Tools Module
#' The Spatial Tools module provides a  framework for visualizing and analyzing spatially explicit datasets within the iMESc application.
#'
#' ### Key Functionalities:
#' 1. Map-Based Visualizations:
#'    - Circles: Visualize numeric variables using circle markers on a map.
#'    - Pies: Display categorical variables using pie charts at spatial coordinates.
#'    - Scatter3D: Visualize 3D relationships among spatial variables.
#'    - Raster: Render spatial datasets as rasters for detailed spatial analysis.
#'    - Interpolation: Generate interpolated surfaces from point data for spatial predictions.
#'    - 3D Surface and Stack: Create three-dimensional visualizations for spatial layers.
#'
#' 2. Data Preprocessing and Validation:
#'    - Flexible data input configurations, including numeric and categorical attributes.
#'    - Automatic validation of geographic coordinates (e.g., longitude, latitude).
#'    - Filter and subset data using factor attributes.
#'
#' 3. Interactive and Configurable Outputs:
#'    - Customizable visualization settings, including color palettes, radius sizes, and variable selection.
#'    - Support for saving and reloading raster or interpolation outputs for future analysis.
#'
#' 4. Error Handling and Validation:
#'    - Comprehensive checks for coordinate validity (e.g., missing values, range validation for longitude and latitude).
#'    - Real-time feedback when datasets do not meet spatial requirements.
#'
#' ### Workflow:
#' - Data Setup: Select a dataset and attribute type (numeric or factor) to prepare the data for spatial visualization.
#' - Visualization Modes: Choose from various map types (e.g., Circles, Pies, Raster) to explore spatial patterns.
#' - Advanced Visualization:
#'    - Interpolation: Create continuous surfaces for variables based on point data.
#'    - 3D Visualizations: Generate surface plots or stacked layers for spatial data insights.
#'
#' ### Additional Features:
#' - Validation messages to ensure data readiness for spatial analysis.
#' - Modular design enables future expansion, such as additional spatial models or custom visualizations.
#'
#' ### Notes:
#' - Ensure that spatial datasets include valid geographic coordinates.
#' - Use appropriate filters and subsets to refine visualizations and analyses.



#' @export
sptools_data<-list()
sptools_data$ui<-function(id){
  ns<-NS(id)

  div(
    h4("Spatial Tools", class="imesc_title"),
    div(class="spatial_tools spatial_setup",
        id=ns('map_setup'),

        box_caret(ns("box_setup"),
                  title="Data setup",
                  color="#374061ff",
                  inline=F,
                  div(

                    div(class="inline_pickers2",
                        pickerInput_fromtop_live(ns("data_map"),"Datalist:",
                                                 choices =NULL,inline=T),
                        pickerInput_fromtop(ns("choices_map"),"Attribute:",
                                            choices = c("Numeric-Attribute","Factor-Attribute"),inline=T),
                        pickerInput_fromtop_live(ns("var_map"),label = "Variable:",choices = NULL,inline=T),
                        pickerInput_fromtop_live(ns("factor_filter"),label = "Filter",choices = NULL,inline=T),
                        pickerInput_fromtop_live(ns("level_filter"),label = "Level",choices = NULL,inline=T)



                    )


                  )
        )
    )





  )}
sptools_data$server<-function(id,vals){
  moduleServer(id,function(input, output, session){
    requireNamespace("leaflet")


    observeEvent(input$data_map,{

      updatePickerInput(session,"choices_map",selected= vals$cur_choices_map)

    })
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
      selected<-get_selected_from_choices(vals$cur_factor_filter,choices)
      updatePickerInput(session,"factor_filter",choices=c("None",choices),selected=selected)
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
    observeEvent(list(input$factor_filter,get_factors()),{
      req(input$factor_filter)
      fac<-get_filter()[,1]
      choices<-levels(fac)
      selected<-get_selected_from_choices(vals$cur_level_filter,choices)

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

      req(data0)
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

    observe({
      req(input$data_map)
      req(input$choices_map)
      req(input$var_map)
      vals$cur_data<-input$data_map
      vals$cur_var_map<-input$var_map
      vals$cur_factor_filter<-input$factor_filter
      vals$cur_level_filter<-input$level_filter
      vals$cur_choices_map<-input$choices_map



    })


    observe({
      shinyjs::toggle('level_filter',condition=input$factor_filter!="None")
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


    observeEvent(data_inputs(),{


      saved_data<-vals$saved_data
      req(input$data_map%in%names(saved_data))
      req(input$var_map%in%colnames(data0()))
      args<-data_inputs()
      res<-do.call(get_data_map,args)
      req(nrow(res)>0)
      vals$data_map<-res


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
#' @export
sptools_panels<-list()
#' @export
sptools_panels$ui<-function(id){
  module_progress("Loading module: Spatial Tools")
  ns<-NS(id)
  fluidRow( class="spatial_tools",


            column(12,
                   #
                   #hidden(uiOutput(ns('coordinates_message'))),
                   class='nav_map',
                   div(id=ns("map_tabs"),
                       navbarPage(
                         NULL,id=ns("mode"),selected='circles',
                         tabPanel(
                           "Circles",value="circles",
                           div(
                             uiOutput(ns('map_circles_validate')),
                             div(
                               id=ns("circles_page"),

                               uiOutput(ns("map_circles"))
                             )
                           )
                         ),
                         tabPanel(
                           "Pies",value="pies",
                           div(
                             uiOutput(ns('map_pies_validate')),
                             div(
                               id=ns("pies_page"),

                               uiOutput(ns("map_pie"))
                             )
                           )
                         ),
                         tabPanel(
                           "Scatter3D",value="scatter3d",
                           div(
                             uiOutput(ns('map_scatter3D_validate')),
                             div(
                               id=ns("scatter3D_page"),

                               uiOutput(ns("map_scatter3d"))
                             )
                           )

                         ),

                         tabPanel(
                           "Raster",value="raster",
                           div(
                             uiOutput(ns('map_raster_validate')),
                             div(
                               id=ns("raster_page"),

                               uiOutput(ns("map_raster"))
                             )
                           )
                         ),
                         tabPanel(
                           "Interpolation",value="interp",
                           div(
                             uiOutput(ns('map_interp_validate')),
                             div(id=ns("interp_page"),
                                 sptools_tab$ui(ns("interp"), interp=T),
                                 uiOutput(ns("map_interp"))
                             )
                           )
                         ),
                         tabPanel(
                           "Surface (3D)",value="surface",
                           div(

                             div(
                               id=ns("surface_page"),

                               uiOutput(ns("map_surface"))
                             )
                           )),
                         tabPanel(
                           "Stack (3D)",value="stack",
                           div(

                             div(
                               id=ns("stack_page"),

                               uiOutput(ns("map_stack"))
                             )
                           ))



                       ))




            )



  )
}
#' @export
sptools_panels$server<-function(id,vals){
  moduleServer(id,function(input, output, session){
    #data_plot=NULL


    ns<-session$ns



    observe({
      valid_map<-is.null(validate_coords())

      shinyjs::toggle('circles_page',condition=valid_map)
      shinyjs::toggle('pies_page',condition=valid_map)
      shinyjs::toggle('scatter3D_page',condition=valid_map)
      shinyjs::toggle('raster_page',condition=valid_map)
      shinyjs::toggle('interp_page',condition=valid_map)
    })



    validate_coords <- reactive({
      res <- NULL
      data <- vals$data_map

      if (is.null(data)) {
        res <- "Data not found"
      } else {
        coords <- attr(data, "coords")

        if (length(coords) == 0) {
          res <- 'The selected Datalist has no coordinates.'
        } else if (ncol(coords) != 2) {
          res <- "Error in Coordinates: Number of columns not equal to 2."
        } else if (anyNA(coords)) {
          res <- "Check your Coordinates: NAs not allowed."
        } else {
          # Check longitude (first column) and latitude (second column)
          lon_invalid <- any(coords[, 1] < -180 | coords[, 1] > 180)
          lat_invalid <- any(coords[, 2] < -90 | coords[, 2] > 90)

          error_messages <- character() # Initialize an empty character vector to collect error messages

          if (lon_invalid) {
            error_messages <- c(error_messages, "Error: Longitude values must be in decimal degrees (between -180 and 180).")
          }

          if (lat_invalid) {
            error_messages <- c(error_messages, "Error: Latitude values must be in decimal degrees (between -90 and 90).")
          }

          if (length(error_messages) > 0) {
            res <- lapply(error_messages, div) # Combine all error messages into a single string
          }
        }
      }

      if (!is.null(res)) {
        res <- div(
          style="display: flex; align-items: center;background-color: #f2dede; color: #d2322d",
          icon("triangle-exclamation",style="font-size: 18px;color: #d2322d"),
          div(embrown(res),style="margin-left: 6px")
        )
      }

      res
    })






    output$map_circles_validate<-renderUI({
      validate_coords()
    })

    output$map_pies_validate<-renderUI({
      validate_coords()
    })

    output$map_scatter3D_validate<-renderUI({
      validate_coords()
    })

    output$map_raster_validate<-renderUI({
      validate_coords()
    })

    output$map_interp_validate<-renderUI({
      validate_coords()
    })



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

      div(
        sptools_tab$ui(ns("surface"), surface=T),
        uiOutput(ns("map_surface_server"))
      )



    })

    output$map_surface_server<-renderUI({
      sptools_tab$server("surface",
                         vals=vals,
                         surface=T)
      NULL
    })
    output$map_stack<-renderUI({

      div(
        sptools_tab$ui(ns("stack"), stack=T),
        uiOutput(ns("map_stack_server"))
      )



    })

    output$map_stack_server<-renderUI({
      sptools_tab$server("stack",
                         vals=vals,
                         stack=T)
      NULL
    })
    output$map_scatter3d<-renderUI({

      div(
        sptools_tab$ui(ns("tab-scatter3d"), scatter3d=T),
        uiOutput(ns("map_scatter3d_server"))
      )




    })

    output$map_scatter3d_server<-renderUI({
      sptools_tab$server("tab-scatter3d",
                         vals=vals,
                         scatter3d=T)
      NULL
    })
    output$coordinates_message<-renderUI({
      em("The selected Datalist does not contain any coordinates. Use Databank to include coordinates in your selection.",style="color:gray")
    })





    data<-reactiveVal()
    cur_vars_choices<-reactiveVal()




    output$map_circles<-renderUI({
      div(
        sptools_tab$ui(ns("tab-circles"), circles=T,radius=T),
        uiOutput(ns("map_circles_server"))
      )
      # req(is.data.frame(data_map))


    })

    output$map_circles_server<-renderUI({
      sptools_tab$server("tab-circles",vals=vals, circles=T, pie=T)
      NULL
    })

    output$map_pie<-renderUI({
      div(
        sptools_tab$ui(ns("pie"), pie=T,radius=T),
        uiOutput(ns("map_pie_server"))
      )
      # req(is.data.frame(data_map))


    })
    output$map_pie_server<-renderUI({
      sptools_tab$server("pie",vals=vals, pie=T)
      NULL
    })
    output$map_interp<-renderUI({
      # data_map<-vals$data_map
      #req(is.data.frame(data_map))
      div(

        uiOutput(ns("map_interp_server"))
      )
      sptools_tab$server("interp", vals=vals,interp=T)


    })
    output$map_interp_server<-renderUI({

    })



    output$map_raster<-renderUI({
      div(
        sptools_tab$ui(ns("raster"), raster=T),
        uiOutput(ns("map_raster_server"))
      )



    })

    output$map_raster_server<-renderUI({
      sptools_tab$server("raster",
                         vals=vals,
                         raster=T)
      NULL
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





  })
}
sptools_tab<-list()
sptools_tab$ui<-function(id, circles=F, pie=F, radius=F,raster=F,interp=F,  shapes=T,surface=F,stack=F,scatter3d=F){

  ns<-NS(id)
  title_colors<-"Colors"
  if(isTRUE(surface)){
    title_colors<-"Z-Control"
  }
  div(style="margin-top: 1px",
      class="inline_pickers",
      fluidRow(
        column(
          4,class="mp0",style="height: 80vh;overflow-y: auto",

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
                      ll_interp$ui(ns('interp1')),
                      uiOutput(ns("interp1_out"))
                    )
                  }

                )

              )
            },
            if(isTRUE(surface)){
              surface_saved_maps$ui(ns('surf_maps'))
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
                        sptools_colors$ui(ns('sptools_colors')),
                        div(id=ns("show_breaks"),
                            sptools_breaks$ui(ns('sptools_breaks')),
                            uiOutput(ns("custom_breaks"))
                        )
                      )),
            if(isTRUE(surface)) {
              surface_add_points$ui(ns('surf_addpoints'))
            },

            if(isTRUE(circles)|isTRUE(pie))
              box_caret(ns("box_circles"),
                        color="#c3cc74ff",
                        title=inline(uiOutput(ns("box_circles_title"))),

                        div(
                          if(isTRUE(circles)){
                            sptools_circles$ui(ns('sptools_circles'))
                          },
                          if(isTRUE(radius)){
                            sptools_radius$ui(ns('sptools_radius'))
                          },
                          if(isTRUE(pie)){
                            div(

                              sptools_pie$ui(ns("sptools_pie")),
                              uiOutput(ns("pie_out"))
                            )
                          },
                        )

              ),
            if(isTRUE(scatter3d))
              box_caret(
                ns("s3d_points"),
                title="Points",
                color="#c3cc74ff",
                div(

                  selectInput(ns("s3d_zvariable"),"Z-Color", c("Z-Value","4th variable (numeric)","4th variable (factor)")),
                  uiOutput(ns('s3d_points_zdatalist_out')),


                  numericInput(ns('s3d_points_size'),"Size",1.5),
                  checkboxInput(ns("s3d_points_scale"),"Scale Size", F ),
                  uiOutput(ns('s3d_points_z_scale_out'))


                )
              ),
            if(isFALSE(stack))
              sptools_text$ui(ns('sptools_text'),surface),

            if(isTRUE(any(c(surface,stack))))
              div(
                id=ns('surface_3d_control'),
                surface_3d_control$ui(ns('surf_control'))
              ),

            if(isFALSE(scatter3d))
              div(
                id=ns("shapes_leaflet"),
                sptools_shapes$ui(ns("sptools_shapes"),surface=surface,stack=stack)
              ),

            div(id=ns("map_coords"),
                sptools_coords$ui(ns("sptools_coords"))

            ),

            div(id=ns("ggplot_titles"),
                div(sptools_gg_opts$ui(ns("sptools_gg_opts")))


            ),
            div(id=ns("ggplot_scalebar"),
                sptools_gg_scalebar$ui(ns("sptools_gg_scalebar"))
            ),
            div(id=ns("ggplot_north"),

                sptools_gg_north$ui(ns("sptools_gg_north"))

            ),
            div(id=ns("ggplot_legend"),

                sptools_gg_legend$ui(ns("sptools_gg_legend"))

            ),
            uiOutput(ns('extraLayers_output'))

          )
        ),
        column(
          8,class="mp0",
          div(style="max-height: 80px; overflow-y: auto",
              uiOutput(ns("error_map"))
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
                                 tipify_ui(actionLink(ns("down_plot3D_btn"), "Download",icon("download")),"Download")
                      ))

                    ),
                    fluidRow(div(style="margin-top:-31px;margin-left: 130px",

                                 div(
                                   class="navmap2",
                                   navbarPage(
                                     id=ns("mode_plot"),


                                     header=div(
                                       div( style="display: flex;",
                                            div(
                                              class="run_map_btn save_changes",

                                              id=ns("run_map_btn"),
                                              actionButton(ns("run_map"),
                                                           span(icon("angles-right"),icon("map")),style="height: 30px; padding-top: 3px;padding-bottom: 3px")
                                            ),
                                            div(style="padding: 5px;width:100px;font-size: 11px",
                                                emgray(icon("fas fa-hand-point-right"),"Click to Create the map")
                                            ),
                                            div(style="position: absolute; right: 2px;z-index:999",
                                                tipify_ui(uiOutput(ns("save_map_btn")),"Save the Raster for using in Surface and Stack plots"),
                                                uiOutput(ns("save_geotiff")),
                                                actionLink(ns("create_grid"),"Grid Maps")
                                            ),
                                       ),

                                       uiOutput(ns("vgm_error"))
                                     ),

                                     title=NULL,
                                     if(isFALSE(any(surface,stack,scatter3d)))
                                       tabPanel("Leaflet",value="leaflet",
                                                div(sptools_providers$ui(ns("sptools_providers"))),

                                                div(id=ns("zoom_leaflet_btn"),
                                                    div(
                                                      style="position: absolute; top: 150px; left: 40px; z-index: 9999;",
                                                      actionLink(ns("zoom_leaflet"),icon("magnifying-glass-plus",style="color: #05668D;"),style="font-size: 18px;")
                                                    ),
                                                    div(
                                                      # style="display: flex; position: absolute;top: 30px; right: 0px;gap: 15px; align-items: center",


                                                    )),
                                                shinyBS::bsTooltip(ns('zoom_leaflet'),"Shows the Leafmap map in a modal Window"),

                                                div(
                                                  class="sptools_plot",
                                                  leaflet::leafletOutput(ns("plot_from_leaflet"),width="50vw")
                                                )),
                                     if(isFALSE(any(surface,stack,scatter3d)))
                                       tabPanel(
                                         "GGplot",value="ggplot",
                                         div( uiOutput(ns("plot_from_ggplot")))

                                       ),

                                     if(isTRUE(any(surface,stack,scatter3d)))
                                       tabPanel("plotly",value="plotly",
                                                div(id=ns("plotly_page"),
                                                    class="sptools_plot",
                                                    uiOutput(ns('plotly_validate')),
                                                    uiOutput(ns('plotly_surface')),
                                                    uiOutput(ns('plotly_stack')),
                                                    uiOutput(ns('plotly_scatter3d'))
                                                )
                                       ),
                                     if(isTRUE(any(surface,stack)))
                                       tabPanel("plot3D",value="plot3D",
                                                div(
                                                  class="sptools_plot",
                                                  uiOutput(ns('plot3D_validate')),
                                                  uiOutput(ns('plot3D_surface')),
                                                  uiOutput(ns('plot3D_stack'))
                                                )

                                       ),
                                   )
                                 )



                    )))

        )
      )
  )
}
sptools_tab$server<-function(id, raster=F, interp=F, pie=F,circles=F,vals,surface=F,stack=F,scatter3d=F){
  moduleServer(id,function(input, output, session){

    ns<-session$ns



    box_caret_server('box_plotsize',hide_content=T)

    box_caret_server('box_labels')
    box_caret_server('box_color')
    box_caret_server('box_circles')


    box_caret_server('box_interp')
    box_caret_server("grid_titles",hide_content=F)
    sptools_colors$server_update('sptools_colors',vals)
    sptools_extra_shape$server("sptools_extra_shape", vals)
    sptools_gg_opts$server('sptools_gg_opts')
    sptools_gg_legend$server_update('sptools_gg_legend',vals)

    sptools_gg_scalebar$server_update('sptools_gg_scalebar',vals)
    sptools_shapes$server_update1("sptools_shapes", vals=vals)
    sptools_coords$server_update("sptools_coords")
    sptools_gg_opts$server_update('sptools_gg_opts',vals,scatter3d=F,surface=F,stack=F)
    sptools_gg_north$server_update("sptools_gg_north")

    if(isTRUE(scatter3d)){
      sptools_gg_opts$server_update_scatter3d('sptools_gg_opts',vals)
    }
    if(isTRUE(stack)){
      sptools_gg_opts$server_update_stack('sptools_gg_opts',vals)
    }
    if(isTRUE(surface)){
      sptools_gg_opts$server_update_surface('sptools_gg_opts',vals)
    }







    sptools_text$server_update("sptools_text",vals=vals)
    if(isTRUE(interp)){
      ll_interp$server("interp1", vals)
    }
    args_sptools_pie<-reactiveVal()


    color_args1<-sptools_colors$server('sptools_colors')
    leaflet_opts1<-sptools_providers$server("sptools_providers")
    args_labels<-sptools_text$server('sptools_text')
    rst<-reactiveVal()
    get_shapes<-reactive({
      sptools_shapes$server("sptools_shapes",vals)
    })


    map_result1<-reactiveVal()

    gg_title_args<-reactive({
      sptools_gg_opts$server_titles('sptools_gg_opts')
    })
    gg_axes_args<-reactive({
      sptools_gg_opts$server_axes('sptools_gg_opts')
    })
    observeEvent(ignoreInit = T,input$down_ggplot,{

      generic<-map_result2()
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      name_c=paste0(colnames(vals$data_map))
      req(vals$cur_factor_filter)
      if(vals$cur_factor_filter!="None"){
        name_c=paste0(name_c,"-",vals$cur_level_filter)
      }
      datalist_name=attr(vals$data_map,"datalist")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Map", name_c=name_c,datalist_name=datalist_name)
    })

    observe({
      condition<-!is.factor(vals$data_map[,1])|isTRUE(surface)
      shinyjs::toggle("show_breaks", condition = condition)
    })

    output$custom_breaks<-renderUI({
      if(isTRUE(surface)){
        req(surface_args_saved_maps()$saved_maps)
        p1<-vals$saved_maps[[surface_args_saved_maps()$saved_maps]]
        z<- attr(p1,"data_z")
      } else{
        req(is.numeric(vals$data_map[,1]))
        z<-na.omit(vals$data_map[,1])}
      n<-breaks_args1()$nbreaks
      req(z)
      req(n)
      values<-get_breaks_input (z,n)
      textInput(session$ns("custom_breaks"),span("Break values",tiphelp("comma-delimited values")),values)
    })
    breaks_args1<-reactive({
      sptools_breaks$server("sptools_breaks")
    })

    observeEvent(vals$cur_modeplot,{
      updateTabsetPanel(session,'mode_plot',selected=vals$cur_modeplot)
    })
    observeEvent(vals$data_map,{
      data_list_name<-attr(vals$data_map,"args")$name
      current_datalist(data_list_name)
      vals$vgm_error<-NULL
      map_result1_final(NULL)
      map_result2_final(NULL)
    })

    current_datalist<-reactiveVal()
    chart_args1<-reactiveVal()
    args_map1<-reactiveVal()
    args_map2<-reactiveVal()
    map_result1_final<-reactiveVal()
    map_ggplot<-reactiveVal()
    map_ggplot_final<-reactiveVal()
    map_result2_final<-reactiveVal()
    map_result2<-reactiveVal()
    observe({
      sptools_pie$server_update("sptools_pie",vals)
    })


    observe({
      cond_ggplot<-input$mode_plot=="ggplot"
      not_leaflet<-input$mode_plot!="leaflet"
      shinyjs::toggle('create_grid',condition = !vals$cur_factor_filter%in%"None"&cond_ggplot)
      shinyjs::toggle("down_btn_generic", condition=input$mode_plot=="plot3D")
      shinyjs::toggle("down_btn_ll", condition=input$mode_plot=="leaflet")
      shinyjs::toggle("down_btn_plotly", condition=input$mode_plot=="plotly")
      shinyjs::toggle("down_btn_gg", condition=cond_ggplot)

      shinyjs::toggle('ggplot_options',condition = not_leaflet)
      shinyjs::toggle('ggplot_titles',condition = not_leaflet)

      shinyjs::toggle('map_coords',condition = cond_ggplot)
      shinyjs::toggle('sptools_gg_legend',condition = cond_ggplot)
      shinyjs::toggle('ggplot_scalebar',condition = cond_ggplot)

      shinyjs::toggle('ggplot_north',condition = cond_ggplot)
      shinyjs::toggle('ggplot_legend',condition = cond_ggplot)
      shinyjs::toggle(selector=".border_color",condition = cond_ggplot)
    })

    if(pie&!circles){
      pie_reactive<-reactive({
        sptools_pie$server("sptools_pie")
      })
      factor_chart<-reactiveVal()
      observeEvent(pie_reactive(),{
        args_sptools_pie(pie_reactive())
        facpie<-args_sptools_pie()$factor_chart
        factor_chart(facpie)
      })
    }
    if(any(circles,pie,interp,raster)){
      sptools_radius$server_update("sptools_radius",vals)
      args_scale_bar<-reactive({
        if(input$mode_plot == "ggplot")
          sptools_gg_scalebar$server('sptools_gg_scalebar')

      })
      if(any(c(circles,pie))){
        sptools_circles$server_update("sptools_circles", vals)
      }
      circle_args1<-reactive({
        if(any(c(circles,pie))){
          sptools_circles$server("sptools_circles")
        } else{
          NULL
        }

      })
      args_gg<-reactive({
        req(input$mode_plot == "ggplot")
        args<-c(sptools_coords$server("sptools_coords"))

        args
      })
      radius_args1<-reactive({
        if(!any(c(circles,pie))){
          return(NULL)
        }
        radius0<-sptools_radius$server("sptools_radius")
        radius<-radius0[1:2]
        if(is.numeric(vals$data_map[,1])) {
          if(vals$cur_factor_filter!="None"){
            req(radius0$scale_range)
            if(radius0$scale_range=="unfiltered"){
              unfilt<-unfiltered_data()[,1]
              unfilt<-scales::rescale(unfilt,c(radius$min_radius,radius$max_radius))
              names(unfilt)<-rownames(unfiltered_data())
              min<-min(unfilt[rownames(vals$data_map)], na.rm=T)
              max<-max(unfilt[rownames(vals$data_map)], na.rm=T)

              radius[[1]]<-min
              radius[[2]]<-max
            }
          }
        }
        radius

      })
      gg_north_args<-reactive({
        req(input$mode_plot == "ggplot")
        sptools_gg_north$server("sptools_gg_north")
      })
      color_ggplot<-reactive({
        req(input$mode_plot == "ggplot")
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

      extra_shapes<-reactive({
        vals$extra_shapes
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
        args<-c(list(data = vals$data_map, pal = pal, fillOpacity = fillOpacity, rst = rst()), circle_args1(), radius_args1(), breaks_args1(), leaflet_opts1(), args_sptools_pie(), shape_args,light=light)

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
        req(input$mode_plot=="ggplot")

        # req(args_for_map1())
        if(is.null(vals$data_map)) {return(NULL)}
        args_ggplot<-NULL
        args_ggplot<-c(args_gg())
        if (isTRUE(interp)| isTRUE(raster)) {
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
        args$data_o<-vals$data_map
        args$factor<-is.factor(vals$data_map[,1])
        if(is.factor(vals$data_map[,1])){
          args$custom_breaks=levels(vals$data_map[,1])
        }
        args[unique(names(args))]
      })
      grid_letteres<-reactive({
        req(input$letter_grid)
        levs0<-levs<-levels(get_factor_filter())
        lets<-letters
        if(input$letter_grid=="uppercase"){
          lets<-LETTERS
        }
        leton<-NULL

        leton<-paste0(rep(lets, length.out = length(levs)),")")
        leton
      })
      get_grid_order<-reactive({
        req(input$rank_grid)
        input$add_letter_grid
        as.numeric(gsub("\\\n.*","",input$rank_grid))
      })
      grid_titles<-reactive({
        levs<-levels(get_factor_filter())
        leton<-NULL
        if(isTRUE(input$add_letter_grid)){

          leton<-grid_letteres()
        }

        lapply(seq_along(levs),function(i){
          paste(leton[i],input[[paste0("grid_label_",i)]])
        })
      })
      get_layout_grid<-reactive({
        levs<-levels(get_factor_filter())
        ncol=input$ncol_grid
        nrow=ceiling(length(levs)/ncol)
        m<-matrix(1:(nrow*ncol),nrow,ncol,byrow =T,)

        p_heights<-(1-input$grid_main_height)/nrow
        p_heights<-rep(p_heights,nrow)

        m<-m+1
        m<-rbind(rep(1,ncol),m)

        t_height<-input$grid_main_height
        heights = c(t_height,p_heights)
        return(list(m=m,heights=heights))

      })
      crete_ggrid_modal<-reactive({
        modalDialog(
          title="Create Grid maps",
          easyClose = T,
          footer = div(
            modalButton("Dismiss"),
            inline(div(actionButton(ns("run_grid"),"RUN >>"),class="save_changes")),
          ),
          div(
            column(
              12,class='mp0',

              column(
                6,class='mp0',style="max-height: 400px;overflow-y: auto",
                div(uiOutput(ns("grid_ui")))
              ),
              column(
                6,class='mp0',style="max-height: 400px;overflow-y: auto",

                box_caret(ns("box_layout"),
                          title="Preview",
                          div(uiOutput(ns("grid_preview")))
                )
              )

            ),
            div(style="position:absolute;right: 0px",

            )
          )
        )
      })
      get_breaks_filter<-reactive({
        data<-unfiltered_data()


        filt_data<-    vals$data_map
        req(input$nbreaks_grid)
        req(filt_data)
        req(data)


        if(isTRUE(input$unif_breaks_grid)){
          x<-data[,1]
          lapply(split(data,get_factor_filter()),function(a){
            if(is.factor(x)){
              levels(x)[levels(x)%in%x]
            } else{

              round(seq(min(x,na.rm=T),max(x,na.rm=T),len=input$nbreaks_grid),T)
            }
          })
        } else{

          lapply(split(data,get_factor_filter()),function(x){
            if(is.factor(x[,1])){
              x<-x[,1]
              levels(x)[levels(x)%in%x]

            } else{
              round(seq(min(x,na.rm=T),max(x,na.rm=T),len=input$nbreaks_grid),T)
            }


          })
        }




      })
      unfiltered_data<-reactive({
        req(vals$data_map)
        req(vals$cur_data)
        req(vals$cur_choices_map)
        req(vals$cur_var_map)
        data_inputs<- list(
          name=vals$cur_data,
          attr=vals$cur_choices_map,
          var=vals$cur_var_map,
          filter="None",
          filter_level=NULL,
          saved_data=vals$saved_data
        )

        args<-data_inputs
        res<-do.call(get_data_map,args)
        res
      })
      get_factor_filter<-reactive({
        data<-unfiltered_data()
        factors<-attr(data,"factors")

        req(vals$cur_factor_filter!="None")
        req(vals$cur_factor_filter%in%colnames(factors))
        factor_filter=factors[,vals$cur_factor_filter]
        factor_filter
      })
      get_grid_layout<-reactive({
        levs<-levels(get_factor_filter())
        ncol=input$ncol_grid
        nrow=ceiling(length(levs)/ncol)
        c(nrow,ncol)
      })
      args_for_map3<-reactive({
        req(input$mode_plot=="ggplot")
        # req(args_for_map1())
        if(is.null(vals$data_map)) {return(NULL)}
        args_ggplot<-NULL
        args_ggplot<-c(args_gg())
        if (isTRUE(interp)| isTRUE(raster)) {
          args = args_for_map1()["rst"]
          color_args<-color_ggplot()
          color_args$pal=vals$cur_mappalette
          shape_args<-get_shapes()
          args<-c(args, color_args, shape_args, args_ggplot)
        } else {
          args = args_for_map1()
          args$rst<-vals$data_map
          args$pal<-NULL
          args$fillOpacity<-NULL
          color_args<-color_ggplot()
          color_args$pal=vals$cur_mappalette
          args<-c(args, color_args, args_ggplot)
        }
        args<-c(args,args_labels())
        args$args_extra_shape<-extra_shapes()
        if(!is.factor(vals$data_map[,1])){
          req(input$custom_breaks)
          args$custom_breaks<-strsplit(input$custom_breaks,",")[[1]]
        }
        args$data_o<-vals$data_map
        args$factor<-is.factor(vals$data_map[,1])
        args[unique(names(args))]
      })
      args_gg_legend<-reactive({
        sptools_gg_legend$server("sptools_gg_legend")
      })
      args_ggplot<-reactive({
        req(input$mode_plot == "ggplot")
        args<-args_for_map2()
        args[names(gg_title_args())]<-gg_title_args()
        args[names(args_scale_bar())]<-args_scale_bar()
        args[names(gg_axes_args())]<-gg_axes_args()
        args[names(gg_north_args())]<-gg_north_args()
        args[names(args_gg_legend())]<-args_gg_legend()




        args
      })


      observeEvent(input$down_grade_raster,{
        shinyjs::toggle("down_grade",condition = isTRUE(input$down_grade_raster))
      })

      observeEvent(input$add_letter_grid,ignoreInit = T,{
        shinyjs::toggle('letter_grid',condition = isTRUE(input$add_letter_grid))
      })
      observeEvent(input$mode_plot,ignoreInit = T,{
        if(!input$mode_plot%in%vals$cur_modeplot)
          vals$cur_modeplot<-input$mode_plot
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
      observeEvent(input$create_grid,ignoreInit = T,{
        req(vals$data_map)

        showModal(
          crete_ggrid_modal()
        )
      })
      output$box_circles_title<-renderUI({
        if(isTRUE(circles)){
          "Circles"
        } else{
          span("Pies",tipify_ui(actionLink(ns("pie_chart_help"),icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Click for details"))
        }


      })
      output$extraLayers_output<-renderUI({
        req(input$mode_plot == "ggplot")
        sptools_extra_shape$ui(session$ns("sptools_extra_shape"), vals$data_map)
      })
      output$grid_ui<-renderUI({
        if(isTRUE(interp))
          validate(need( vals$interp_args$interp_type!="krige",'Not available for krigging method'))
        div(
          "This tool allows you to create grid maps using ggplot and the GridExtra package. The data will be subset based on the selected filter to generate maps that will be displayed in a grid.",
          div(strong("Current Filter:"),emgreen(vals$cur_factor_filter),style="font-size: 14px"
          ),
          div(
            box_caret(
              ns("grid_titles"),
              color="#c3cc74ff",
              title="Layout",
              hide_content=F,
              div(        numericInput(ns("ncol_grid"),"Number of columns",2),)

            ),
            box_caret(
              ns("grid_titles"),
              color="#c3cc74ff",
              title="Title",
              hide_content=F,
              div(
                textInput(ns("main_grid"),"Main Title",value=colnames(vals$data_map)),
                numericInput(ns("grid_main_size"),"Size",12),
                numericInput(ns("grid_main_height"),"Height",.1),
                numericInput(ns("grid_main_hadj"),span("hadj",tipright("horizontal adjustment")),0),

              )

            )
          ),
          box_caret(
            ns("grid_titles"),
            color="#c3cc74ff",
            title="Subtitles",
            hide_content=F,
            div(
              div(style="display: flex",
                  checkboxInput(ns("add_letter_grid"),"Add letter",T,width="100px"),
                  pickerInput(ns("letter_grid"),NULL,c("lowercase","uppercase"))
              ),
              uiOutput(ns("titles_grid")),
              numericInput(ns("grid_sub_hadj"),span("hadj",tipright("horizontal adjustment")),0)
            )

          ),

          box_caret(
            ns("grid_titles"),
            color="#c3cc74ff",
            title="Breaks",
            hide_content=F,
            div(

              checkboxInput(ns("unif_breaks_grid"),span("Unify breaks",tiphelp("If checked, all maps in the grid will use break ranges based on the entire dataset for uniformity. If unchecked, the breaks for each map will be determined by the data range of the filtered subset.")),T),
              numericInput(ns("nbreaks_grid"),"Number of breaks",5),
              numericInput(ns("round_grid"),"Round",2)
            )

          )






        )

      })
      output$titles_grid<-renderUI({
        levs<-levels(get_factor_filter())

        outputs<-lapply(seq_along(levs),function(i){
          div(style="display: flex",div(
            i,style="font-size:0px",
          ),textInput(ns(paste0("grid_label_",i)),NULL,value=NULL))
        })
        names(values)<-outputs
        div(span(tags$label("Edit/Drag and drop to order")),
            div(style="display: flex",

                sortable::rank_list(
                  text = NULL,
                  labels = lapply(grid_letteres(),function(x) div(x,class="shiny-input-container leton",style="width: fit-content;   margin-right: 2px")),
                  input_id = ns("rank_grid_letters"),
                  options = sortable::sortable_options(sort=F),
                  class="rank_grid"
                ),
                sortable::rank_list(
                  text = NULL,
                  labels = outputs,
                  input_id = ns("rank_grid"),

                  class="rank_grid"
                ),
                uiOutput(ns("render_grid_labels"))
            )
        )

      })
      output$grid_preview<-renderUI({

        levs<-levels(get_factor_filter())
        breaks<-lapply(get_breaks_filter(),function(x) paste(x,collapse=", "))
        titles<-grid_titles()
        m<-get_layout_grid()$m
        heights<-get_layout_grid()$heights

        psg<-gg_grid_preview_list(levs,breaks,titles,grid_sub_hadj=input$grid_sub_hadj)[get_grid_order()]
        names(psg)<-c(levs)
        height=sum(rep(400,nrow(m)))
        width=sum(rep(600,ncol(m)))
        dims=resize_with_aspect_ratio(new_width =width/(ncol(m)+1) ,height=height,width=width)
        height=dims$new_height

        width=dims$new_width
        # resize_with_aspect_ratio(new_width = 250, width = 200, height = 300)

        renderPlot({
          ml<-gg_grid2(psg,m,heights,main=input$main_grid,grid_main_hadj=input$grid_main_hadj,grid_main_size=input$grid_main_size/(ncol(m)))
          grid::grid.draw(ml)
        },width = width,height=height)


      })
      output$plot_from_ggplot<-renderUI({

        req(map_result2_final())

        height=vals$cur_ggheight
        width=vals$cur_ggwidth
        column(12,
               div(class="sptools_plot",
                   renderPlot({
                     withProgress(message="Generating map...",min=NA,max=NA,{
                       p<-map_result2_final()
                       req(p)
                       if(inherits(p,"gtable")){
                         grid::grid.draw(p)
                       } else{
                         plot(p)
                       }


                     })
                   },width = width,height = height)
               ))
      })
      output$plot_from_leaflet<-leaflet::renderLeaflet({
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
      output$plot_from_leaflet2<-leaflet::renderLeaflet({
        #validate_data(vals$data_map)
        map_result1_final()
      })


      if(isTRUE(interp)){

        output$vgm_error<-renderUI({
          req(vals$vgm_error)
          vals$vgm_error
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

        observeEvent(vals$interp_args$interp_type,{
          removeTab('mode_plot','tab_gstat')
          if(isTRUE(interp))
            if(!vals$interp_args$interp_type%in%c("idw")){
              shiny::insertTab(
                "mode_plot",
                tabPanel("Model Results",value="tab_gstat",
                         tabsetPanel(NULL,id=ns("interp_results"),
                                     type="hidden",
                                     tabPanel('krige',uiOutput(session$ns("vgm_plot"))),
                                     tabPanel('caret',uiOutput(session$ns("caret_summary")))
                         )


                ),

                position="after",target='ggplot',select =F
              )
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

      }

      if(any(c(interp,raster))){


        asptools_shapes<-reactive({
          req(vals$data_map)
          req(length(get_shapes())>0)
          req(length(vals$extra_shapes)>0)
          req(vals$extra_shapes)
          req('base_shape_args'%in%names(get_shapes()))
          req('layer_shape_args'%in%names(get_shapes()))

          list(
            data=vals$data_map,
            base=get_shapes()$base_shape_args,
            layer=get_shapes()$layer_shape_args,
            extra=vals$extra_shapes
          )
        })
        observeEvent(asptools_shapes(),{

          base_shape_args=asptools_shapes()$base
          layer_shape_args=asptools_shapes()$layer
          args_extra_shape=asptools_shapes()$extra

          data<-vals$data_map
          all_coords<-get_all_coords(data,base_shape_args,layer_shape_args,args_extra_shape)


          res<-get_mean_resolution(rbind(all_coords,attr(data,"coords")))


          updateNumericInput(session,"raster_resolution",value=res)
        })


        raster_model_args<-reactive({
          list(
            raster_resolution=input$raster_resolution,
            down_grade_raster=input$down_grade_raster,
            down_grade=input$down_grade
          )
        })
        observeEvent(raster_model_args(),{
          shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
        })
        observeEvent(rst(),{
          r<-rst()
          vals$cur_raster<-r
          req(r)
          if(!all(is.na(r@data@values))){
            vals$vgm_error<-NULL
          } else{
            met<-attr(r,"method")
            error_message<-paste(met,"failed")
            if(!is.null(met)){
              if(met=="krige") {
                error_message<-paste0(error_message,": check you variogram model.")
              }
              vals$vgm_error<-div(
                style="overflow: auto; max-height: 150px; background: #e68e83",
                strong(icon("triangle-exclamation",style="color: brown"),"Error:"),
                renderPrint({error_message})
              )
            }


          }
        })
        interpolation_args<-reactive({
          req(vals$interp_args)
          args<-vals$interp_args
          if (isTRUE(interp)) {
            req(args)
            args$data<-vals$data_map
            args$g<-vals$interp_gstat
            req('base_shape_args'%in%names(get_shapes()))
            req('layer_shape_args'%in%names(get_shapes()))
            args$base_shape_args=get_shapes()$base_shape_args
            args$layer_shape_args=get_shapes()$layer_shape_args
            args$args_extra_shape=vals$extra_shapes

            rst<- do.call(capture_log2(interp_leaflet2),args)
            vals$error_rst<-attr(rst,"logs")
            req(!inherits(rst,"error"))
            if(is.numeric(vals$data_map[,1])){

              # custom_breaks=as.numeric(strsplit(input$custom_breaks,",")[[1]])

              # rst@data@values<- scales::rescale( rst@data@values,c(min(custom_breaks),max(custom_breaks)))
            }


            attr(rst,"method")<-args$interp_type
            vals$cur_raster<-rst
            rst(rst)
          } else{
            rst(NULL)
          }

        })
        condition_cokrige<-reactive({
          is.factor(vals$data_map[,1])|
            length(vals$g_final)>0
        })
        raster_args<-reactive({
          # if(is.null(vals$data_map)) {return(NULL)}
          if (isTRUE(raster)) {
            shape_args<-get_shapes()
            req('base_shape_args'%in%names(get_shapes()))
            req('layer_shape_args'%in%names(get_shapes()))
            base_shape_args=get_shapes()$base_shape_args
            layer_shape_args=get_shapes()$layer_shape_args
            args_extra_shape=vals$extra_shapes

            args<-list(data=vals$data_map, resolution=input$raster_resolution,base_shape_args=base_shape_args,layer_shape_args=layer_shape_args,args_extra_shape=args_extra_shape)


            rst<-do.call(map_rst,args)
            rst<-migrate_rst(rst, vals$data_map)


            if(isTRUE(input$down_grade_raster)){
              rst <- raster::aggregate(rst, fact=input$down_grade)


            }
            vals$cur_raster<-rst
            rst(rst)
          }
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
              # out = gstat::gstat.cv(g,nfold = input$nfold,all.residuals=T)
              # attr(out,"obs")<-observed
              #attr(g,"cv")<-out
              # vals$g_final_cv<-g

            })

        })
        cvkrige<-reactive({

          if(vals$interp_args$interp_type%in%"krige"){
            req(vals$interp_args)
            req(vals$interp_gstat)
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
      }
      observeEvent(input$zoom_leaflet,ignoreInit = T,{
        showModal(
          tags$div(id="modal_leaflet",class="zoom_leaflet",
                   modalDialog(
                     size ="xl",
                     leaflet::leafletOutput(ns("plot_from_leaflet2"), height  = 520, width='70vw'),
                     easyClose = T
                   )
          )

        )
      })
      observeEvent(input$run_map,ignoreInit = T,{

        args_map1(NULL)
        args_map2(NULL)
        try({
          vals$cur_ggheight<-400
          vals$cur_ggwidth<-600
          req(isFALSE(surface))


          if (isTRUE(raster)) {

            withProgress(message = "Rasterizing...", min = NA, max = NA, isolate(raster_args()))

          }
          if (isTRUE(interp)) {
            # krige_cv()
            withProgress(message = "Interpolating...", min = NA, max = NA, interpolation_args())
            cvkrige()
          }
          if (input$mode_plot == "leaflet") {

            args_map1(args_for_map1())
          } else if (input$mode_plot == "ggplot") {

            args_map2(args_ggplot())
          }

          shinyjs::show("save_png")
          shinyjs::show("all_download_buttons")



        })
      })
      observeEvent(input$run_grid,ignoreInit = T,{

        removeModal()
        vals$cur_ggheight<-250*get_grid_layout()[1]
        vals$cur_ggwidth<-400*get_grid_layout()[2]
        vals$hw_changed<-T
        vals$fheight<-round(Pixel_to_cm(vals$cur_ggheight))
        vals$fwidth<-round(Pixel_to_cm(vals$cur_ggwidth))


        data<-unfiltered_data()

        factor_filter=get_factor_filter()

        args_map<-args_for_map3()




        args<-list(
          cur_data=vals$cur_data,
          cur_choices_map=vals$cur_choices_map,
          cur_var_map=vals$cur_var_map,
          filter="None",
          filter_level=NULL,
          saved_data=vals$saved_data[vals$cur_data],
          args=args_map,
          factor_filter=factor_filter,
          ncol=input$ncol_grid,
          padding=input$padding_grid,
          main=input$main_grid,
          round=2,
          breaks_grid=input$unif_breaks_grid,
          nbreaks=input$nbreaks_grid,
          raster_resolution=input$raster_resolution,
          down_grade_raster=input$down_grade_raster,
          down_grade=input$down_grade,
          shape_args=get_shapes(),
          raster=raster,
          circles=circles,
          pie=pie,
          interp_args=vals$interp_args,
          interp_gstat=vals$interp_gstat,
          interp=interp,
          session=getDefaultReactiveDomain(),
          custom_breaks=get_breaks_filter(),
          breaks_on=!input$unif_breaks_grid,
          titles=grid_titles(),
          grid_sub_hadj=input$grid_sub_hadj
        )


        args$args$data_o<-NULL
        args_title_axes<-c(gg_title_args(),gg_axes_args())

        args$args[names(args_title_axes)]<-args_title_axes
        args$args_barscale<-args_scale_bar()
        args$args[names(gg_north_args())]<-gg_north_args()

        withProgress(min=NA,max=NA,message="Creating Grid Layout....",{
          args2<-args
          args2$session<-NULL
          ps<-do.call(gg_plotList,args)
          levs<-levels(get_factor_filter())
          breaks<-lapply(get_breaks_filter(),function(x) paste(x,collapse=", "))
          titles<-grid_titles()
          m<-get_layout_grid()$m
          heights<-get_layout_grid()$heights
          p<-gg_grid2(ps,m,heights,main=input$main_grid,grid_main_hadj=input$grid_main_hadj,grid_main_size=input$grid_main_size/(ncol(m)))
        })
        map_result2(p)
        map_result2_final(map_result2())

      })
      observeEvent(list(input$rank_grid,input$create_grid),{
        req(input$rank_grid)
        req(input$create_grid)

        levs0<-levs<-levels(get_factor_filter())

        leton<-grid_letteres()
        lapply(seq_along(levs0),function(i){
          updateTextInput(session,paste0("grid_label_",i),NULL,value=levs0[i])

        })

      })
      observe({
        if(is.null(vals$cur_ggheight)){
          vals$cur_ggheight<-400
        }
        if(is.null(vals$cur_ggwidth)){
          vals$cur_ggwidth<-600
        }

        if(is.null(map_result1_final())){
          shinyjs::hide('zoom_leaflet_btn')
        } else{
          shinyjs::show('zoom_leaflet_btn')
        }


      })

      observeEvent(args_map1(), {
        shinyjs::removeClass(selector=".run_map_btn", class="save_changes")
      })
      observeEvent(args_map2(), {
        shinyjs::removeClass(selector=".run_map_btn", class="save_changes")
      })

      observeEvent(args_map1(), {
        args<-args_map1()
        validate(need(!inherits(args_map1()$rst,"try-error"),args_map1()$rst[1]))

        args$base_shape_args<-NULL
        args$layer_shape_args<-NULL
        args$args_extra_shape<-NULL
        args$addCircles<-isTRUE(circles)
        p<-do.call(map_discrete, args)
        req(!inherits(p,"try-error"))
        map_result1(p)
        map_result1_final(map_result1())
      })
      observeEvent(args_map2(), {
        shinyjs::removeClass(selector=".run_map_btn", class="save_changes")
      })
      observeEvent(args_map2(),ignoreInit = T,{
        req(args_map2())
        args<-args_map2()
        if(is.null(args$data)){
          args$data<- args$data_o
        }
        try({

          p<-do.call(gg_rst, args)
          args$p<-p
          p<-do.call(gg_add_titles,args)
          args$p<-p
          p<-do.call(gg_style_axes,args)
          args$p<-p
          p<-do.call(add_bar_scale,args)
          args$p<-p
          p<-do.call(gg_add_north,args)
          map_result2(p)
          map_result2_final(map_result2())
        })




      })
      observeEvent(map_result2_final(),{
        shinyjs::removeClass("divnull","down_btn_gg")
      })





    }
    surface_saved_maps$server_update('surf_maps',vals)
    sptools_shapes$server_update_ir("sptools_shapes", vals=vals)
    output$save_map_btn<-renderUI({
      req(isTRUE(interp)|isTRUE(raster))
      req(length(rst())>0)


      actionLink(ns("save_map"),"Save map",icon("fas fa-save"))
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
    observeEvent(input$save_map_confirm,ignoreInit = T,{
      myrst<-map_to_save()
      vals$saved_maps[[input$map_name]]<-myrst
      removeModal()
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
          mask( raster::crop(myrst, extent(base)), base)
        }
      }
      attr(myrst,"base_shape")<-base
      attr(myrst,"layer_shape")<-layer
      myrst
    })

    if(any(scatter3d,stack,surface)){

      surface_3d_control$server_update('surf_control')

      box_caret_server("surface_Points")
      box_caret_server("surface_control")
      box_caret_server("surface_savedmaps")



      all_ggplot_options_args<-reactive({
        formals_args<-c(
          names(formals(stack_plotly)),
          names(formals(map_discrete)),
          names(formals(scatter_3dplotly)),
          names(formals(get_4D)))
        args<-c(gg_title_args(),gg_axes_args())
        args[names(args)%in%formals_args]
      })
      plotly_installed<-reactiveVal(is_installed("plotly"))
      plot3D_installed<-reactiveVal(is_installed("plot3D"))
      surface_args_saved_maps<-reactive({

        surface_saved_maps$server('surf_maps')
      })
      surface_args_addpoints<-reactive({

        surface_add_points$server('surf_addpoints')
      })
      surface_args_3dcontrol<-reactive({

        surface_3d_control$server('surf_control',vals)
      })
      observe({
        if(is.null(vals$saved_maps)){
          vals$saved_maps<-list()
        }
      })
      observe({
        req(isTRUE(plotly_installed()))
        output$plotly_surface_out<-plotly::renderPlotly({
          run_map_surface_plotly()
        })
      })
      observe({
        cond_plotly<-input$mode_plot%in%"plotly"
        shinyjs::toggle("surf_exp",condition=isTRUE(input$stack_surface)&cond_plotly)
        shinyjs::toggle("sep_factor",condition=cond_plotly)
      })
      observe({
        shinyjs::toggle('saved_maps2', condition=length(vals$saved_maps)>1)
      })
      observe({
        req(input$mode_plot%in%"plotly")
        shinyjs::toggle("run_map_btn",condition=isTRUE(plotly_installed()))
      })
      observe({
        req(input$mode_plot%in%"plot3D")
        shinyjs::toggle("run_map_btn",condition=isTRUE(plot3D_installed()))
      })
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
      observeEvent(input$mode_plot,{

        shinyjs::toggle('surface_3d_control',condition=!input$mode_plot%in%"plotly")

        shinyjs::toggle('plot3D_surface',condition=input$mode_plot%in%"plot3D")
        shinyjs::toggle('plot3D_stack',condition=input$mode_plot%in%"plot3D")})
      observeEvent(vals$data_map,{
        vals$error_rst<-NULL
        rst(NULL)
      })
      observeEvent(input$install_plot3D,{
        install.packages("plot3D")
        plot3D_installed(TRUE)

      })
      observeEvent(input$install_plotly,{
        install.packages("plotly")
        plotly_installed(TRUE)

      })
      observe({
        shinyjs::toggle("plotly_surface",condition=isTRUE(surface))
        shinyjs::toggle("plotly_stack",condition=isTRUE(stack))
      })


      observeEvent(input$surf_ap_colfac,{
        vals$cur_surf_ap_colfac<-input$surf_ap_colfac
      })
      observeEvent(input$mode_plot,{
        shinyjs::toggle("surf_d",condition=input$mode_plot=="plot3D")
        shinyjs::toggle("leg_surface",condition=input$mode_plot=="plot3D")
        shinyjs::toggle("surf_tick",condition=input$mode_plot=="plot3D")

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




      observeEvent(ignoreInit = T,input$down_plot3D_btn,{

        vals$hand_plot<-"plot_funcion"
        module_ui_figs("downfigs")
        generic<-vals$map_res

        mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Map", name_c="Map",fun=get_graphics_download()$fun,args_plot=get_graphics_download()$args)
      })
      output$save_geotiff<-renderUI({
        req(rst())
        tipify_ui(downloadLink(session$ns("down_geotif"), span(icon("download"),"GeoTiff")),"Download the GeoTiff file with georeferenced raster data for GIS applications")
      })
      output$error_map<-renderUI({
        render_message(
          vals$error_rst
        )
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

      output$print_ap_color<-renderUI({



      })
      output$plot3D_surface<-renderUI({

        req(isTRUE(surface))
        renderPlot({

          run_map_surface_plot3D()
          persp<-recordPlot()
          vals$map_res<-persp
          vals$map_res

        })
      })
      get_surfpoints<-reactive({

        if(isFALSE(surface_args_addpoints()$surf_addpoints)){
          return(NULL)
        }
        req(surface_args_addpoints()$surf_points_datalist)

        data0<-data<-vals$saved_data[[surface_args_addpoints()$surf_points_datalist]]
        validate(need(length(attr(data,"coords"))>0,"No coordinates found in the selected Data"))

        coords<-attr(data,"coords")
        req(surface_args_addpoints()$surf_points_z_value)
        req(surface_args_addpoints()$surf_points_z_value%in%colnames(data))
        var=data[surface_args_addpoints()$surf_points_z_value]

        points<-data.frame(coords,var)
        req(surface_args_addpoints()$surf_ap_palette)
        pal<-vals$newcolhabs[[surface_args_addpoints()$surf_ap_palette]]

        if(surface_args_addpoints()$surf_points_z_color=="Z-Value"){

          cut_var<-cut(var[,1],length(unique(var[,1])),include.lowest = T)
          points$color<-pal(nlevels(cut_var))[cut_var]
        } else{
          cut_var<-cbind(data0,attr(data0,"factors"))[,surface_args_addpoints()$surf_points_z_color]
          if(is.numeric(cut_var)){
            cut_var<-cut(cut_var,5,include.lowest = T)

          }
          points$color<-pal(nlevels(cut_var))[cut_var]
        }

        points$size_value<-surface_args_addpoints()$surf_ap_size
        points$size<-surface_args_addpoints()$surf_ap_size
        if(isTRUE(surface_args_addpoints()$surf_z_scale)){
          req(surface_args_addpoints()$surf_points_datalist)
          data_size<-vals$saved_data[[surface_args_addpoints()$surf_points_datalist]]
          req(surface_args_addpoints()$surf_ap_z_scale_vars%in%colnames(data_size))
          size<-data_size[,surface_args_addpoints()$surf_ap_z_scale_vars]

          newsize<-scales::rescale(size,c(surface_args_addpoints()$surf_ap_minsize,surface_args_addpoints()$surf_ap_size))
          points$size_value<-size

          points$size<-newsize
        }
        points


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
        div(
          pickerInput_fromtop_live(ns("s3d_points_zdatalist"),"Datalist", choices1,selected=selected1 ),
          uiOutput(ns('s3d_points_zcolor'))
        )
      })

      if(isTRUE(scatter3d)){
        observeEvent(input$s3d_zvariable,{
          shinyjs::toggle("s3d_points_zdatalist",condition=input$s3d_zvariable!="Z-Value")
        })
        observeEvent(input$s3d_points_scale,{
          shinyjs::toggle("s3d_points_z_scale_args",condition=isTRUE(input$s3d_points_scale))
        })
        observeEvent(input$s3d_points_zcolor,{
          vals$cur_s3d_points_zcolor<-input$s3d_points_zcolor
        })
        observeEvent(input$s3d_points_z_scale_datalist,{
          vals$cur_s3d_points_z_scale_datalist<-input$s3d_points_z_scale_datalist
        })
        observeEvent(input$s3d_points_zdatalist,{
          vals$cur_s3d_points_zdatalist<-input$s3d_points_zdatalist
        })
        observeEvent(input$s3d_zvariable,{
          vals$cur_s3d_zvariable<-input$s3d_zvariable
        })
        observeEvent(input$s3d_points_z_scale_vars,{
          vals$cur_s3d_points_z_scale_vars<-input$s3d_points_z_scale_vars
        })
        output$plotly_scatter3d<-renderUI({

          req(isTRUE(plotly_installed()))
          plotly::plotlyOutput(ns("plotly_scatter3d_out"))

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
          pickerInput_fromtop_live(ns("s3d_points_zcolor"),"Variable", choices2,selected=selected2 )
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
          pickerInput_fromtop_live(ns("s3d_points_z_scale_datalist"),"Datalist", choices3,selected=selected3)
        })

        output$s3d_points_z_scale_vars<-renderUI({
          req(input$s3d_points_z_scale_datalist)
          choices<-colnames(vals$saved_data[[input$s3d_points_z_scale_datalist]])
          selected=vals$cur_s3d_points_z_scale_vars
          selected=get_selected_from_choices(selected,choices)

          pickerInput_fromtop_live(ns("s3d_points_z_scale_vars"),"Z-Value", choices,selected=selected)
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
        get_args_scatter3d<-reactive({
          points<-get_plot3Dpoints()
          text<-get_plot3Dtext()
          shape_args<-get_shapes()
          args=list(points=points,text=text,shape_args=NULL)
          label_args<-all_ggplot_options_args()
          args<-c(args,label_args)
          args
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
            col_fac<-cut(col_fac,5,include.lowest = T)
          }
          colors<-pal(n_colors)
          reverse_palette<-color_args1()$reverse_palette
          if(isTRUE(reverse_palette)){
            colors<-rev(colors)
          }
          colors<-colorspace::lighten(colors,light)
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
            newsize<-scales::rescale(tosize,c(input$s3d_points_minsize,input$s3d_points_size))
            points$size<-newsize
            points$size_var<-input$s3d_points_z_scale_vars
          }

          points
        })
        observeEvent(get_plot3Dtext(),{
          shinyjs::addClass(selector=".run_map_btn", class="save_changes")
        })
        observeEvent(get_plot3Dpoints(),{
          shinyjs::addClass(selector=".run_map_btn", class="save_changes")
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
        run_map_scatter3d_plotly<-eventReactive(input$run_map,ignoreInit = T,{
          req(isTRUE(scatter3d))
          req(input$mode_plot=="plotly")
          args<-get_args_scatter3d()
          shinyjs::removeClass(selector=".run_map_btn", class="save_changes")
          do.call(scatter_3dplotly,args)

        })
        observe({
          req(isTRUE(plotly_installed()))
          output$plotly_scatter3d_out<-plotly::renderPlotly({
            run_map_scatter3d_plotly()
          })
        })
      }
      if(isTRUE(stack)){
        req(isTRUE(plotly_installed()))
        observe({
          output$plotly_stack_out<-plotly::renderPlotly({
            run_map_stack_plotly()
          })
        })
        observeEvent(get_args_stack(),{

          shinyjs::addClass(selector=".run_map_btn",class= "save_changes")

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
            surf_exp=surface_args_3dcontrol()$surf_exp,
            shape_args=shape_args,
            #  addpoints=get_surfpoints(),
            # addtext=get_surftext(),
            surface=input$stack_surface,
            theta=surface_args_3dcontrol()$surf_theta,
            phi=surface_args_3dcontrol()$surf_phi,
            r=surface_args_3dcontrol()$surf_r,
            d=surface_args_3dcontrol()$surf_d,
            exp=surface_args_3dcontrol()$surf_exp,
            tictype=surface_args_3dcontrol()$surf_tick

          )
          label_args<-all_ggplot_options_args()
          args<-c(args,label_args)

          args
        })
        run_map_stack_plotly<-eventReactive(input$run_map,ignoreInit = T,{

          req(isTRUE(stack))
          req(input$mode_plot=="plotly")
          args<-get_args_stack()
          req(args)
          shinyjs::removeClass("run_map_btn", "save_changes")
          shinyjs::removeClass(selector=".run_map_btn", class="save_changes")
          withProgress(do.call(stack_plotly,args),min=NA,max=NA,message="Running...")

        })

        get_stack_maps<-reactive({
          req(input$stack_order)
          req(input$stack_layers)
          maps<-lapply(1:input$stack_layers,function(i){
            req(input[[paste0('stack_layer',i)]])
            vals$saved_maps[[input[[paste0('stack_layer',i)]]]]
          })
          maps[get_z_stack_order()]
        })
        get_stack_pals<-reactive({
          req(input$stack_order)
          stack_pals<-lapply(1:input$stack_layers,function(i){
            req(input[[paste0('stack_pal',i)]])
            vals$newcolhabs[[input[[paste0('stack_pal',i)]]]]
          })
          res<-stack_pals[get_z_stack_order()]
          req(length(res)==input$stack_layers)
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
        get_z_stack_order<-reactive({
          req(input$stack_order)
          rev(as.numeric(gsub("break-stk-ord.*","",input$stack_order)))
        })

        get_z_stack_names<-reactive({
          sapply(get_stack_maps(),function(x) attr(x,"z_name"))
        })
        output$stack_layers<-renderUI({

          div(numericInput(ns('stack_layers'),"Nº layers",2, max=length(vals$saved_maps)),
              numericInput(ns('sep_factor'),"z-expansion",1),
              numericInput(ns('surf_exp'),"surface-expansion",1),
              div(class="switch_box",switchInput(ns("stack_surface"),"Surface",value=F,size="mini",labelWidth="60px")),
              div("Drag and drop to reorder the layers",style="font-size: 10px"),
              uiOutput(ns('stack_layers_out')))

        })
        output$stack_layers_out<-renderUI({
          maps<-vals$saved_maps
          layers<-lapply(1:input$stack_layers,function(i){

            div(
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
            div(paste0("Layer",i),style=" border-bottom: 1px dashed gray;width: 60px;padding: 2px;height:24px", class="")
          })
          div(style="max-height: 200px; overflow-y: auto;display: flex",
              div(class="mp0",ord),
              div(class="mp0",
                  class="stack_order",
                  sortable::rank_list(
                    labels=layers,
                    input_id=ns("stack_order")
                  )
              )
          )
        })
        output$plotly_stack<-renderUI({

          req(isTRUE(plotly_installed()))
          plotly::plotlyOutput(ns("plotly_stack_out"))

        })

        output$plot3D_stack<-renderUI({
          graphics.off()

          req(isTRUE(stack))
          renderPlot({

            run_map_stack_plot3D()
            persp<-recordPlot()
            vals$map_res<-persp
            vals$map_res

          })
        })
        run_map_stack_plot3D<-eventReactive(input$run_map,ignoreInit = T,{

          req(isTRUE(stack))
          req(input$mode_plot=="plot3D")
          args<-get_args_stack()
          shinyjs::removeClass(selector=".run_map_btn", class="save_changes")
          do.call(stack_plot3D,args)

        }
        )
      }
      if(isTRUE(surface)){


        surface_add_points$server_update('surf_addpoints',vals)

        get_args_surface<-reactive({


          req(surface_args_saved_maps()$saved_maps)
          req(isTRUE(surface))
          req(input$custom_breaks)
          p1<-vals$saved_maps[[surface_args_saved_maps()$saved_maps]]
          shape_args<-get_shapes()
          req(surface_args_saved_maps()$saved_maps2)



          p2<-vals$saved_maps[[surface_args_saved_maps()$saved_maps2]]
          req(p1)
          req(p2)

          palette<-color_args1()$pal
          light<-color_args1()$light


          my_rst<-p1
          data_z<- attr(p1,"data_z")

          if(is.factor(data_z)){

            colors<- vals$newcolhabs[[palette]](nlevels(data_z))
          } else {
            colors<- vals$newcolhabs[[palette]](length(my_rst@data@values))
          }
          colors<-colorspace::lighten(colors,light)

          req(input$custom_breaks)
          my_rst2=p2
          args<-list(
            my_rst1=my_rst,
            my_rst2=my_rst2,
            colors=colors,
            theta=surface_args_3dcontrol()$surf_theta,
            phi=surface_args_3dcontrol()$surf_phi,
            r=surface_args_3dcontrol()$surf_r,
            d=surface_args_3dcontrol()$surf_d,
            exp=surface_args_3dcontrol()$surf_exp,
            tictype=surface_args_3dcontrol()$surf_tick,
            wlegend=input$surface_width.legend,
            hlegend=input$surface_height.legend,
            x.intersp=surface_args_addpoints()$surf_x.intersp,
            y.intersp=surface_args_addpoints()$surf_y.intersp,
            inset_x=surface_args_addpoints()$surf_inset_x,
            inset_y=surface_args_addpoints()$surf_inset_y,
            shape_args=shape_args,
            addpoints=get_surfpoints(),
            addtext=get_surftext(),
            custom_breaks=as.numeric(strsplit(input$custom_breaks,",")[[1]])
          )
          label_args<-all_ggplot_options_args()
          label_args<-gg_axes_args()

          args<-c(args,label_args)

          args
        })

        output$plotly_surface<-renderUI({

          req(isTRUE(plotly_installed()))
          plotly::plotlyOutput(ns('plotly_surface_out'))
        })
        run_map_surface_plot3D<-eventReactive(input$run_map,ignoreInit = T,{
          req(isTRUE(surface))
          req(input$mode_plot=="plot3D")
          args<-get_args_surface()
          shinyjs::removeClass(selector=".run_map_btn", class="save_changes")
          withProgress(min=NA,max=NA,message="Running",{
            do.call(get_4D,args)
          })


        })
        run_map_surface_plotly<-eventReactive(input$run_map,ignoreInit = T,{



          withProgress(min=NA,max=NA,message="Running...",{
            req(isTRUE(surface))
            req(input$mode_plot=="plotly")
            args<-get_args_surface()
            shinyjs::removeClass("run_map_btn", "save_changes")
            do.call(surface_plotly,args)
          })



        })
      }



    }


    ll_down_plot$server('save_png', map=map_result1_final())


    return(NULL)

  })

}


# Circles tab
sptools_circles<-list()
sptools_circles$ui<-function(id){
  ns<-NS(id)
  div(
    tags$div(id=ns("circle_options"),
             checkboxInput(ns("scale_radius"), "Scale radius",T)
    )
  )
}
sptools_circles$server<-function(id,vals){
  moduleServer(id,function(input, output, session){
    return(
      list(
        scale_radius=input$scale_radius

      )
    )

  })
}
sptools_circles$server_update<-function(id,vals){
  moduleServer(id,function(input, output, session){

    result<-reactive({
      list(
        scale_radius=input$scale_radius

      )
    })

    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })
    observeEvent(vals$cur_choices_map,{
      shinyjs::toggle('scale_radius',condition=vals$cur_choices_map!="Factor-Attribute")
    })
    return(NULL)

  })
}
sptools_radius<-list()
sptools_radius$ui<-function(id){
  ns<-NS(id)
  div(
    div(class="inline_pickers2",style="display: flex;",

        numericInput(ns("min_radius") ,label ='Min',1),
        numericInput(ns("max_radius") ,label =" Max",5)
    ),
    radioButtons(ns("scale_range"),"Size Range:",c(
      "filtered data"="filtered",
      "unfiltered data"="unfiltered"
    ),inline=T,width="400px")
  )
}
sptools_radius$server<-function(id){
  moduleServer(id,function(input, output, session){



    return(
      list(
        min_radius=input$min_radius,
        max_radius=input$max_radius,
        scale_range=input$scale_range
      )
    )

  })
}
sptools_radius$server_update<-function(id,vals){
  moduleServer(id,function(input, output, session){
    observe({
      cond=is.numeric(vals$data_map[,1])&vals$cur_factor_filter!="None"
      shinyjs::toggle('scale_range',condition = cond)
    })

    result<-reactive({
      list(
        min_radius=input$min_radius,
        max_radius=input$max_radius,
        scale_range=input$scale_range
      )
    })
    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })

    return(NULL)

  })
}

# Pie tab
sptools_pie<-list()
sptools_pie$ui<-function(id){
  ns<-NS(id)
  div(

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
sptools_pie$server<-function(id){
  moduleServer(id,function(input, output, session){

    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })

    observeEvent(input$buffer_zize,ignoreInit = T,{
      if(input$buffer_zize<=0){
        updateNumericInput(session,"buffer_zize",value=0.00001)
      }
    })
    result<-reactive({
      list(
        addMinicharts=T,
        factor_chart=input$factor_chart,
        buffer_zize=input$buffer_zize
      )
    })

    return(result())

  })
}
sptools_pie$server_update<-function(id,vals){
  moduleServer(id,function(input, output, session){


    factors<-reactive({
      attr(vals$data_map,"factors")
    })



    observe({
      updatePickerInput(session,'factor_chart',choices=colnames(factors()))
    })


  })
}


# Module for handling shapes
sptools_shapes<-list()
sptools_shapes$ui<-function(id, surface=F,stack=F){

  ns<-NS(id)
  div(
    if(isFALSE(stack))
      div(id=ns("show_base"),
          box_caret(ns("box_base"),
                    color="#c3cc74ff",
                    title=span(style="display: inline-block",
                               class="checktitle",
                               checkboxInput(ns("base_shape") ,label =strong("Base Shape"),F,width="150px")
                    ),
                    div(
                      id=ns("base_options"),

                      if(isTRUE(surface))
                        uiOutput(ns("base_z_out")),

                      colourpicker::colourInput(ns('base_color'),"Background","whitesmoke"),


                      div(


                        colourpicker::colourInput(ns("base_border_color"),"Border Color","darkgray")
                        ,
                        numericInput(ns("base_weight"),'Border Width',min=0,0.5,step=.5)



                      ),
                    )
          )
      ),

    div(id=ns("show_layer"),
        box_caret(ns("box_layer"),
                  color="#c3cc74ff",
                  title=span(style="display: inline-block",
                             class="checktitle",
                             checkboxInput(ns("layer_shape") ,label =strong("Layer Shape"),F)
                  ),
                  div(
                    id=ns("layer_options"),
                    if(isTRUE(surface))
                      uiOutput(ns('layer_z_out')),

                    colourpicker::colourInput(ns('layer_color'),"Background","#C0C2C1"),

                    div(


                      colourpicker::colourInput(ns("layer_border_color"),"Border Color","gray20")
                      ,
                      numericInput(ns("layer_weight"),'Border Width',min=0,0.5,step=.5)



                    )
                  )
        )
    )
  )
}
sptools_shapes$server_update1<-function(id, vals){
  moduleServer(id,function(input, output, session){
    ns<-session$ns

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
    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })
    mode_plot<-reactive({
      req(vals$cur_modeplot)
      vals$cur_modeplot
    })
    data<-reactive({
      req(vals$data_map)
      vals$data_map
    })
    mode<-reactive({
      req(vals$cur_mode_map)
      vals$cur_mode_map
    })
    cur_rst<-eventReactive(vals$cur_rst,{
      req(vals$cur_rst)

      req(inherits(vals$cur_rst,"RasterLayer"))
      vals$cur_rst
    })
    observeEvent(mode_plot(),{
      shinyjs::toggle("layer_border",condition= !mode_plot()%in%"plotly")
      shinyjs::toggle("base_border",condition= !mode_plot()%in%"plotly")
      shinyjs::toggle("base_fill",condition= !mode()%in%"surface")
      shinyjs::toggle("layer_fill",condition= !mode()%in%"surface")


    })
    observeEvent(cur_rst(),{
      output$base_z_out<-renderUI({
        # req(inherits(vals$cur_rst,"RasterLayer"))
        z<-raster::values(cur_rst())
        numericInput(ns("base_z"),"Z",value=min(z,na.rm=T))
      })
      output$layer_z_out<-renderUI({
        # req(inherits(vals$cur_rst,"RasterLayer"))
        z<-raster::values(cur_rst())
        div(numericInput(ns("layer_z"),"Z",value=min(z,na.rm=T)))
      })
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
    observeEvent(mode_plot(),{
      shinyjs::toggle('show_base',condition = !mode_plot()%in%"leaflet")
      shinyjs::toggle('show_layer',condition = !mode_plot()%in%"leaflet")

    })
    observeEvent(data(),{
      data<-data()

      if(!is.null(data)) {
        base_shape<-attr(data,"base_shape")
        layer_shape<-attr(data,"layer_shape")
        if(!is.null(base_shape)){
          updateCheckboxInput(session,'base_shape',value=T)
        }
        if(!is.null(layer_shape)){
          updateCheckboxInput(session,'layer_shape',value=T)

        }
      }
    })

    return(NULL)


  })
}
sptools_shapes$server_update_ir<-function(id, vals){
  moduleServer(id,function(input, output, session){
    ns<-session$ns


    return(NULL)

  })
}
sptools_shapes$server<-function(id,vals){
  moduleServer(id,function(input, output, session){
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
sptools_extra_shape<-list()
sptools_extra_shape$ui<-function(id,data_map=NULL){
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

  div(id=ns('show_extra'),
      res,
      tags$div(

        numericInput(ns("data_depth"),
                     span("Layer depth ",tipright("Control the layering order of your data relative to shapes")),
                     value=3,
                     min=1, step=1
        )
      )
  )

}
sptools_extra_shape$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    data<-reactive({
      vals$data_map
    })
    box_caret_server("box_extra")
    observeEvent(vals$cur_modeplot,{
      shinyjs::toggle('show_extra',condition = !vals$cur_modeplot%in%"leaflet")


    })
    get_extralayers<-reactive({
      extralayers<-NULL
      shapes<-attr(data(),"extra_shape")
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
      extralayers
    })

    result<-reactive({
      list(
        args_extra=get_extralayers(),
        data_depth=input$data_depth
      )
    })
    observeEvent(result(),{
      vals$extra_shapes<-result()
    })
    return(NULL)


  })
}

# Module for handling coordinates
sptools_coords<-list()
sptools_coords$ui<-function(id){
  ns<-NS(id)
  box_caret(
    ns("box_coords"),
    color="#c3cc74ff",
    hide_content=T,
    title="Coordinates",
    div(
      pickerInput_fromtop(ns("show_coords"),label=strong("Symbol"),choices=c("None",rev(df_symbol$val)),choicesOpt = list(content =  c("None",rev(df_symbol$img) )),selected="None"),
      div(
        id=ns('coords_options'),
        colourpicker::colourInput(ns("col.coords"),label="Color",value="black"),
        numericInput(ns("cex.coords"),label="Size",value=3)

      )
    )
  )


}
sptools_coords$server_update<-function(id){
  moduleServer(id,function(input,output,session){

    box_caret_server("box_coords",hide_content=T)

    observe({
      shinyjs::toggle("coords_options", condition=input$show_coords!='None')
    })
    result<-reactive({
      list(
        show_coords=input$show_coords,
        col.coords=input$col.coords,
        cex.coords=input$cex.coords)
    })
    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })
    return(NULL)
  })
}
sptools_coords$server<-function(id){
  moduleServer(id,function(input,output,session){
    result<-list(
      show_coords=input$show_coords,
      col.coords=input$col.coords,
      cex.coords=input$cex.coords)
    return(result)
  })
}

# Module for handling ggplot legend
sptools_gg_legend<-list()
sptools_gg_legend$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(
      ns("gg_box_legend"),
      color="#c3cc74ff",
      title="Legend",
      hide_content=T,
      div(
        pickerInput(ns("legend.position"),"Position",c("right","bottom","left","top","none")),
        textInput(ns("leg_title"),"Legend title",NULL),
        numericInput(ns("legend.text_size"),label="Legend Text Size",value=13)
      )

    )
  )
}
sptools_gg_legend$server<-function(id){
  moduleServer(id,function(input,output,session){


    result<-list(
      legend.position=input$legend.position,
      leg_title=input$leg_title,
      legend.text_size=input$legend.text_size

    )
    return(result)

  })
}
sptools_gg_legend$server_update<-function(id,vals){
  moduleServer(id,function(input,output,session){
    data<-reactive({
      req(vals$data_map)
      vals$data_map
    })
    result<-reactive({
      list(
        legend.position=input$legend.position,
        leg_title=input$leg_title,
        legend.text_size=input$legend.text_size

      )
    })
    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })
    observeEvent(data(),{
      updateTextInput(session,'leg_title',value=colnames(data()))
    })

  })
}


# GGplot options module
sptools_gg_opts<-list()
sptools_gg_opts$ui<-function(id){
  ns<-NS(id)

  div(
    box_caret(
      ns("box_map_titles"),
      color="#c3cc74ff",
      title="Title",
      hide_content=T,
      div(
        textInput(ns("main"),label="Title"),
        numericInput(ns("plot.title_size"),label="Title size",value=12),
        selectInput(ns("title.face"),"Title font",c("plain","italic","bold"))
      )

    ),

    div(id=ns("show_box_map_subtitles"),
        box_caret(
          ns("box_map_subtitles"),
          color="#c3cc74ff",
          title="Subtitle",
          hide_content=T,
          div(
            textInput(ns("subtitle"),label="Subtitle"),
            numericInput(ns("plot.subtitle_size"),label="Subtitle size",value=11),
            selectInput(ns("subtitle.face"),"Title font",c("plain","italic","bold"))
          )

        )),
    box_caret(
      ns("box_map_axes"),
      color="#c3cc74ff",
      title="Axes",
      hide_content=T,
      div(
        div(
          id=ns('div_axis_style'),
          pickerInput(ns("axis_style"),"Style",c("B&W Blocks"="bw_blocks","Classic"="default"),selected="default"),
          numericInput(ns("axis_width"),label="Width",value=0.1)
        ),

        textInput(ns("xlab"),label="x-label", "Longitude"),
        numericInput(ns("x.n.breaks"),label="x-nbreaks",value=5),
        textInput(ns("ylab"),label="y-label","Latitude"),
        numericInput(ns("y.n.breaks"),label="y-nbreaks",value=5),
        textInput(ns("zlab"),label="z-label"),
        numericInput(ns("axis.title_size"),label="Label Size",value=11),
        numericInput(ns("axis.text_size"),label="Axis Size",value=11)
      )

    ),
    div(
      id=ns('box_guides'),
      box_caret(
        ns("box_map_guides"),
        color="#c3cc74ff",
        title="Guides",
        hide_content=T,
        div(
          checkboxInput(ns("show_guides"),label="Show Guides",value=F),
          colourpicker::colourInput(ns("guides_color"),label="Color",value="black"),
          pickerInput(ns('guides_linetype'),"Line type",c("dashed","dotted","solid")),
          numericInput(ns('guides_linewidth'),"Line Width",value=0.15)
        )
      )
    )

  )
}
sptools_gg_opts$server_update<-function(id,vals,scatter3d=F,surface=F,stack=F){
  moduleServer(id,function(input,output,session){
    observe({

      shinyjs::toggle('zlab',condition=isTRUE(any(surface,stack,scatter3d)))
    })
    observe({
      shinyjs::toggle('div_axis_style',condition=vals$cur_modeplot%in%'ggplot')
      shinyjs::toggle('box_guides',condition=vals$cur_modeplot%in%'ggplot')

    })



    results<-reactive({
      list(


        main=input$main,
        plot.title_size=input$plot.title_size,
        title.face=input$title.face,
        subtitle=input$subtitle,
        plot.subtitle_size=input$plot.subtitle_size,
        subtitle.face=input$subtitle.face,
        xlab=input$xlab,
        ylab=input$ylab,
        zlab=input$zlab,
        axis.text_size=input$axis.text_size,
        axis.title_size=input$axis.title_size,
        axis_style=input$axis_style,
        axis_width=input$axis_width,
        x.n.breaks=input$x.n.breaks,
        y.n.breaks=input$y.n.breaks,
        show_guides=input$show_guides,
        guides_color=input$guides_color,
        guides_linetype=input$guides_linetype,
        guides_linewidth=input$guides_linewidth)


    })
    observeEvent(results(),{

      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })
    return(NULL)
  })
}
sptools_gg_opts$server_update_scatter3d<-function(id,vals){
  moduleServer(id,function(input,output,session){

    data<-reactive({
      req(vals$data_map)
      vals$data_map
    })
    cur_rst<-reactive({
      req(inherits(vals$cur_rst,"RasterLayer"))
      vals$cur_rst
    })
    observe({

      formalss<-formals(scatter_3dplotly)
      tohide<-names(input)[!names(input)%in%names(formalss)]
      tohide<-tohide[!grepl("box",tohide)]
      lapply(tohide,
             function(x) shinyjs::hide(x))
      hide('show_box_map_subtitles')


    })

    observeEvent(get_title(),{
      updateTextInput(session,"main",value=get_title())
    })
    get_title<-reactive({
      value0=value=colnames(data())
      if(!is.null(vals$cur_level_filter)) {
        if(vals$cur_factor_filter!="None"){
          value=paste0(value," - ",vals$cur_level_filter)
        }
      }
      return(value)
    })


    observe({
      updateTextInput(session,'zlab',value=colnames(data()))
    })
    return(NULL)

  })
}
sptools_gg_opts$server_update_stack<-function(id,vals){
  moduleServer(id,function(input,output,session){

    data<-reactive({
      req(vals$data_map)
      vals$data_map
    })
    get_title<-reactive({
      value0=value=colnames(data())
      if(!is.null(vals$cur_level_filter)) {
        if(vals$cur_factor_filter!="None"){
          value=paste0(value," - ",vals$cur_level_filter)
        }
      }
      return(value)
    })
    observeEvent(get_title(),{
      updateTextInput(session,"main",value=get_title())
    })
    observe({
      formalss<-c(formals(stack_plot3D),formals(stack_plotly))
      tohide<-names(input)[!names(input)%in%names(formalss)]
      tohide<-tohide[!grepl("box",tohide)]
      lapply(tohide,
             function(x) shinyjs::hide(x))
      hide('show_box_map_subtitles')

    })


    return(NULL)

  })
}
sptools_gg_opts$server_update_surface<-function(id,vals){
  moduleServer(id,function(input,output,session){

    data<-reactive({
      req(vals$data_map)
      vals$data_map
    })
    cur_rst<-reactive({
      req(inherits(vals$cur_rst,"RasterLayer"))
      vals$cur_rst
    })
    observe({

      formalss<-c(formals(surface_plotly),formals(get_4D))
      tohide<-names(input)[!names(input)%in%names(formalss)]
      tohide<-tohide[!grepl("box",tohide)]
      lapply(tohide,
             function(x) shinyjs::hide(x))
      hide('show_box_map_subtitles')

    })
    observeEvent(get_title(),{
      updateTextInput(session,"main",value=get_title())
    })
    get_title<-reactive({
      value0=value=colnames(data())
      if(isTRUE(vals$cur_surf_addpoints)){
        value=vals$cur_surf_ap_colfac
      } else{value=attr(cur_rst(),"z_name")}
      return(value)


    })
    return(NULL)

  })
}
sptools_gg_opts$server_titles<-function(id){
  moduleServer(id,function(input,output,session){
    result<-list(main=input$main,
                 plot.title_size=input$plot.title_size,
                 title.face=input$title.face,
                 subtitle=input$subtitle,
                 plot.subtitle_size=input$plot.subtitle_size,
                 subtitle.face=input$subtitle.face)
    return(result)

  })
}
sptools_gg_opts$server<-function(id){
  moduleServer(id,function(input,output,session){

    box_caret_server("box_map_titles",hide_content=T)
    box_caret_server("box_map_subtitles",hide_content=T)
    box_caret_server("box_map_axes",hide_content=T)
    box_caret_server("box_map_guides",hide_content=T)


  })
}
sptools_gg_opts$server_axes<-function(id){
  moduleServer(id,function(input,output,session){
    result<-
      list(
        xlab=input$xlab,
        ylab=input$ylab,
        zlab=input$zlab,
        axis.text_size=input$axis.text_size,
        axis.title_size=input$axis.title_size,
        axis_style=input$axis_style,
        axis_width=input$axis_width,
        x.n.breaks=input$x.n.breaks,
        y.n.breaks=input$y.n.breaks,
        show_guides=input$show_guides,
        guides_color=input$guides_color,
        guides_linetype=input$guides_linetype,
        guides_linewidth=input$guides_linewidth
      )

    return(result)

  })
}

# GGplot north module
sptools_gg_north<-list()
sptools_gg_north$ui<-function(id){
  ns<-NS(id)


  div(
    box_caret(
      ns("box_north"),
      color="#c3cc74ff",
      title="North arrow",
      hide_content=T,
      div(

        pickerInput(ns("n_location"),
                    label="North Position",
                    c("bottom-right"="br",
                      "bottoml-left"="bl",
                      "top-right"="tr",
                      "top-left"="tl",
                      "none"="none"),selected="tl"),
        pickerInput(ns("n_which_north"),label="Wich North",c("grid","true")),
        numericInput(ns("n_width"),label="Width",value=40,step=5),
        numericInput(ns("n_height"),label="Heigth",value=40,step=5),
        numericInput(ns("n_pad_x"),label="pad_x",value=0.1),
        numericInput(ns("n_pad_y"),label="pad_y",value=0.15),
        numericInput(ns("n_cex.text"),label="Label Size",value=10,step=1)

      )
    )
  )


}
sptools_gg_north$server<-function(id){
  moduleServer(id,function(input,output,session){



    result<-list(
      n_location=input$n_location,
      n_which_north=input$n_which_north,
      n_width=input$n_width,
      n_height=input$n_height,
      n_pad_x=input$n_pad_x,
      n_pad_y=input$n_pad_y,
      n_cex.text=input$n_cex.text
    )
    return(result)

  })
}
sptools_gg_north$server_update<-function(id){
  moduleServer(id,function(input,output,session){
    box_caret_server('box_north',hide_content=T)
    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })

    result<-reactive({
      list(
        n_location=input$n_location,
        n_which_north=input$n_which_north,
        n_width=input$n_width,
        n_height=input$n_height,
        n_pad_x=input$n_pad_x,
        n_pad_y=input$n_pad_y,
        n_cex.text=input$n_cex.text
      )
    })
    return(NULL)

  })
}

# Leaflet providers module
sptools_providers<-list()
sptools_providers$ui<-function(id){
  ns<-NS(id)
  div(class="mp0",
      div(
        class="map_providers",style="display: flex; align-items: center; position: absolute; right: 0px; top: 30px",

        pickerInput_fromtop_live(
          ns("providers"),
          label ="+ Map Style",
          selected="Stadia.StamenTerrain",
          options=list(container="body"),
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
          ),
        ),
        numericInput(ns("zoomSnap"),span('+ zoomSnap',tiphelp("defines the interval at which zoom levels are snapped")),min=0,max=1,0.25,step=0.01,width='160px')



      )
  )

}
sptools_providers$server<-function(id){
  moduleServer(id,function(input, output, session){
    return(
      reactive({
        list(
          providers=input$providers,
          zoomSnap=input$zoomSnap
        )
      })
    )
  })
}

## Map download module
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


  moduleServer(id,function(input, output, session){
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
  moduleServer(id,function(input, output, session){
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

# Interpolation module
ll_interp<-list()
ll_interp$ui<-function(id){
  lab_nmax<-span("nmax:", tiphelp("for local kriging, the number of nearest observations that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations. By default (empty), all observations are used"))
  lab_nmin<-span( "nmin:",tiphelp("for local kriging, if the number of nearest observations within distance maxdist is less than nmin, a missing value will be generated- see maxdist"))
  lab_omax<-span("omax:",tiphelp("maximum number of observations to select per octant (3D) or quadrant (2D); only relevant if maxdist has been defined as well"))
  lab_maxdist<-span("maxdist:",tiphelp("for local kriging, only observations within a distance of maxdist from the prediction location are used for prediction; if combined with nmax, both criteria apply"))
  lab_idp<-span("idp:",tiphelp("numeric; specify the inverse distance weighting power"))
  lab_resolution<-span("resolution:",tiphelp("Interpolation resolution"))
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



  moduleServer(id,function(input, output, session){

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
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
      NULL
    })
    output$out_vgm<-renderUI({

      NULL
    })
    ll_vgm$server("vgm",vals)

    return(NULL)

  })
}

# Module for handling text (labels) on the map
sptools_text<-list()
sptools_text$ui<-function(id,surface=F){
  ns<-NS(id)
  box_caret(ns("box_labels"),
            color="#c3cc74ff",
            title=div(style="display: inline-block",class="checktitle",checkboxInput(ns("show_labels"),strong("Text"), F,width="120px")),
            div(

              tags$div(id=ns("show_labels_on"),style="display: none",
                       if(isTRUE(surface)){
                         div(

                           pickerInput_fromtop_live(ns("datalist"),"Datalist:",
                                                    choices =NULL),
                           uiOutput(ns("z_label_value"))

                         )
                       },
                       div(
                         pickerInput_fromtop_live(ns("labels"),label="Use factor",choices=c(NULL))
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
sptools_text$server_update<-function(id,vals,surface=F){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    observeEvent(input$datalist,{
      data<-vals$saved_data[[input$datalist]]

      factors<-attr(data,'factors')
      choices=colnames(factors)
      selected=vals$cur_surf_ap_vars
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'labels',choices=choices)
    })

    result<-reactive({
      list(
        show_labels=input$show_labels,
        labels=input$labels,
        cex.fac=input$cex.fac,
        col.fac=input$col.fac,
        datalist=input$datalist,
        z_value=input$z_label_value
      )
    })
    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })

    output$z_label_value<-renderUI({
      req(input$datalist)
      req(vals$cur_z_label_value)
      data<-vals$saved_data[[input$datalist]]

      choices<-colnames(data)
      selected=vals$cur_z_label_value
      selected=get_selected_from_choices(selected,choices)

      pickerInput_fromtop_live(ns("z_label_value"),"Z-Value:",
                               choices =choices,selected=selected)
    })

    observeEvent(input$z_label_value,{
      vals$cur_z_label_value<-input$z_label_value
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
      choices=colnames(factors)
      selected<-vals$cur_ggplot_options
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'labels',choices=choices,selected=selected)

    })

    observeEvent(input$labels,{
      vals$cur_ggplot_options<-input$labels
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
sptools_text$server<-function(id){
  moduleServer(id,function(input,output,session){



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

# module for variogram
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
  moduleServer(id,function(input, output, session){

    observeEvent(all_args(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })

    all_args<-reactive({
      list(
        model=input$model,
        vgm_autofit=input$vgm_autofit,
        spsample=input$spsample,
        nugget=input$nugget,
        psill=input$psill,
        range=input$range,
        kappa=input$kappa
      )
    })

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
      vgm_model<-try(do.call(gstat::vgm,vgm_args()))
      g<-try({gstat::gstat(NULL,id=vc$id,form= vc$formula,data= vc$dsp,model=vals$interp_vgm)})
      list(vgm_model,g)
    })
    observeEvent(get_krige_model(),{
      vals$interp_vgm<-get_krige_model()[[1]]
      vals$interp_gstat<-get_krige_model()[[2]]
    })
    return(NULL)

  })
}

# module for handling the ggplot scalebar
sptools_gg_scalebar<-list()
sptools_gg_scalebar$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(
      ns("box_scalebar"),
      color="#c3cc74ff",
      title="Scale Bar",
      hide_content=T,
      div(
        pickerInput(ns("position"),label="Bar Position",c("bottomright","bottomleft","topright","topleft","none")),
        numericInput(ns("n_bins"),label="Bins",value=2),
        pickerInput(ns("unit"),"Unit",c(
          "Kilometers"='km',
          "meters"="m",
          "centimeters"="cm")),
        numericInput(ns("bins_km"),label=span("Bin length",tipright("bar block size in the specified unit")),value=100),
        numericInput(ns("bar_height"),label="Bar height",value=0.2),
        numericInput(ns("pad_x"),label="pad_x",value=0.05),
        numericInput(ns("pad_y"),label="pad_y",value=0.025),
        pickerInput(ns("position_label"),label="Label position",c("above","below")),
        numericInput(ns("size_scalebar_text"),label="Label size",value=3)
      )
    )
  )
}
sptools_gg_scalebar$server<-function(id){
  moduleServer(id,function(input,output,session){



    result<-list(position=input$position,
                 n_bins=input$n_bins,
                 bins_km=input$bins_km,
                 bar_height=input$bar_height,
                 pad_x=input$pad_x,
                 pad_y=input$pad_y,
                 position_label=input$position_label,
                 size_scalebar_text=input$size_scalebar_text,
                 unit=input$unit
    )
    return(result)

  })
}
sptools_gg_scalebar$server_update<-function(id,vals){
  moduleServer(id,function(input,output,session){

    box_caret_server('box_scalebar',hide_content=T)
    result<-reactive({
      list(position=input$position,
           n_bins=input$n_bins,
           bins_km=input$bins_km,
           bar_height=input$bar_height,
           pad_x=input$pad_x,
           pad_y=input$pad_y,
           position_label=input$position_label,
           size_scalebar_text=input$size_scalebar_text,
           unit=input$unit
      )
    })
    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })
    data<-reactive({
      vals$data_map
    })
    get_shapes<-reactive({
      vals$shape_args
    })




    observeEvent(data(),{
      data<-data()


      base_shape_args=vals$shape_args$base_shape_args
      layer_shape_args=vals$shape_args$layer_shape_args
      args_extra_shape=vals$extra_shapes



      value<-step_scale_bar(data ,base_shape_args,
                            layer_shape_args,
                            args_extra_shape)
      value_unit<-attr(value,'unit')
      updatePickerInput(session,'unit',selected=value_unit)
      updateNumericInput(session,'bins_km',value=value)
      attr(value,"unit")<-value_unit
      cur_unit(value)

    })

    cur_unit<-reactiveVal()


    observeEvent(input$unit,{
      req(cur_unit())

      if(attr(cur_unit(),"unit")!=input$unit){

        bins_km<-input$bins_km
        attr(bins_km,"unit")<-attr(cur_unit(),"unit")
        value=format_distance_to(bins_km,input$unit)
        updateNumericInput(session,'bins_km',value=value)
        attr(bins_km,"unit")<-input$unit
        cur_unit(bins_km)      }


    })



  })
}

# Functionalities for creating data breaks
sptools_breaks<-list()
sptools_breaks$ui<-function(id){
  ns<-NS(id)
  div(
    tags$div(
      numericInput(ns("nbreaks"), span("Nº breaks",tiphelp("approximate number of breaks")),5, width="200px"))
  )
}
sptools_breaks$server<-function(id){
  moduleServer(id,function(input, output, session){
    return(
      list(
        nbreaks=input$nbreaks
      )
    )

  })
}

# Module for handling palettes
sptools_colors<-list()
sptools_colors$ui<-function(id){
  ns<-NS(id)
  div(

    div(
      tags$div(
        pickerInput_fromtop_live(inputId = ns("palette"),
                                 label ="Palette",
                                 NULL),
        radioButtons(ns("scale_color"),"Color Range:",c(
          "filtered data"="filtered",
          "unfiltered data"="unfiltered"
        ),inline=T,width="400px"),

        numericInput(ns("fillOpacity"),'Fill opacity',min=0,max=1,0.8,step=0.1),
        numericInput(ns("light"),'Lightning',min=0,max=1,0,step=0.1)

      ),
      div(
        checkboxInput(ns("reverse_palette"),"Reverse palette",F)
      )
    )



  )

}
sptools_colors$server<-function(id){
  moduleServer(id,function(input, output, session){

    return(
      reactive({

        list(pal='temp_palette',fillOpacity=input$fillOpacity,reverse_palette=input$reverse_palette,light=input$light,scale_color=input$scale_color)
      })
    )
  })
}
sptools_colors$server_update<-function(id,vals){
  moduleServer(id,function(input, output, session){

    result<-reactive({
      list(
        pal=input$palette,fillOpacity=input$fillOpacity,reverse_palette=input$reverse_palette,light=input$light,scale_color=input$scale_color
      )
    })

    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })


    observeEvent(input$palette,{
      vals$cur_mappalette<-input$palette
    })
    observe({
      updatePickerInput(session,"palette", choices =  vals$colors_img$val,
                        choicesOpt = list(
                          content =  vals$colors_img$img )
      )
    })



    color_reac<-reactive({
      list(
        vals$data_map[,1],
        vals$cur_factor_filter,
        input$scale_color,
        input$palette
      )
    })


    observe({
      cond=is.numeric(vals$data_map[,1])&vals$cur_factor_filter!="None"
      shinyjs::toggle('scale_color',condition = cond)
    })

    unfiltered_data<-reactive({
      req(vals$data_map)
      req(vals$cur_data)
      req(vals$cur_choices_map)
      req(vals$cur_var_map)
      data_inputs<- list(
        name=vals$cur_data,
        attr=vals$cur_choices_map,
        var=vals$cur_var_map,
        filter="None",
        filter_level=NULL,
        saved_data=vals$saved_data
      )

      args<-data_inputs
      res<-do.call(get_data_map,args)
      res
    })


    observeEvent(color_reac(),{
      req(input$palette)
      pal<-vals$newcolhabs[[input$palette]]
      if(is.numeric(vals$data_map[,1])){
        if(vals$cur_factor_filter!="None"){
          unfilt<-unfiltered_data()[,1]
          if(input$scale_color=="unfiltered"){
            # fac<-factor(unfilt,levels=unique(unfilt))
            cols<-vals$newcolhabs[[input$palette]](length(unique(unfilt)))
            names(cols)<-1:length(cols)

            cutn<-cut(vals$data_map[,1],breaks=sort(unique(unfilt)),include.lowest=T)
            cols<-cols[cutn]
            cols<-cols[order(as.numeric(names(cols)))]
            plot(rep(1,length(cols)),1:length(cols),col=cols,pch=15,cex=2)
            pal<-colorRampPalette(cols)

          }
        }
      }
      vals$newcolhabs[["temp_palette"]]<-pal

    })



    return(NULL)
  })
}

# Modules for the surface plot
surface_add_points<-list()
surface_add_points$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(
      ns("surface_Points"),
      title=span(style="display: inline-block",class="checktitle",checkboxInput(ns("surf_addpoints"),strong("Points"), F,width="100px")),

      color="#c3cc74ff",
      div(style="padding-left: 10px",
          id=ns("points_out"),
          uiOutput(ns("datalist_out")),
          uiOutput(ns("zvalue_out")),
          uiOutput(ns("zcolor_out")),



          pickerInput_fromtop_live(ns("surf_ap_palette"),"Palette", choices=NULL),

          numericInput(ns('surf_ap_size'),"Size",1.5),
          checkboxInput(ns("surf_z_scale"),"Scale Size", F ),
          uiOutput(ns("zscale_vars")),

          numericInput(ns('surf_ap_minsize'),"Min-size",0.5),
          div(id=ns("legendplot3D"),
              style="margin-left: 15px",
              numericInput(ns('surf_y.intersp'),'Legend y.intersp',1.5),
              numericInput(ns('surf_x.intersp'),'Legend x.intersp',1.5),
              numericInput(ns('surf_inset_x'),'Legend inset_x',0),
              numericInput(ns('surf_inset_y'),'Legend inset_y',0),


          )),

    )
  )

}
surface_add_points$server<-function(id){
  moduleServer(id,function(input,output,session){

    result<-reactive({
      list(
        surf_addpoints=input$surf_addpoints,
        surf_points_datalist=input$surf_points_datalist,
        surf_points_z_value=input$surf_points_z_value,
        surf_points_z_color=input$surf_points_z_color,
        surf_ap_palette=input$surf_ap_palette,
        surf_y.intersp=input$surf_y.intersp,
        surf_x.intersp=input$surf_x.intersp,
        surf_inset_x=input$surf_inset_x,
        surf_inset_y=input$surf_inset_y,
        surf_ap_size=input$surf_ap_size,
        surf_z_scale=input$surf_z_scale,
        surf_ap_z_scale_vars=input$surf_ap_z_scale_vars,
        surf_ap_minsize=input$surf_ap_minsize
      )
    })
    return(result())

  })
}
surface_add_points$server_update<-function(id,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    observe({
      shinyjs::toggle("legendplot3D",condition=vals$cur_modeplot%in%"plot3D")

      shinyjs::toggle('points_out',condition=isTRUE(input$surf_addpoints))
      shinyjs::toggle('surf_ap_z_scale_vars',condition=isTRUE(input$surf_z_scale))
      shinyjs::toggle('surf_ap_minsize',condition=isTRUE(input$surf_z_scale))
      shinyjs::toggle('legendplot3D',condition=isTRUE(input$surf_z_scale))






    })

    result<-reactive({
      list(
        surf_addpoints=input$surf_addpoints,
        surf_points_datalist=input$surf_points_datalist,
        surf_points_z_value=input$surf_points_z_value,
        surf_points_z_color=input$surf_points_z_color,
        surf_ap_palette=input$surf_ap_palette,
        surf_y.intersp=input$surf_y.intersp,
        surf_x.intersp=input$surf_x.intersp,
        surf_inset_x=input$surf_inset_x,
        surf_inset_y=input$surf_inset_y,
        surf_ap_size=input$surf_ap_size,
        surf_z_scale=input$surf_z_scale,
        surf_ap_z_scale_vars=input$surf_ap_z_scale_vars,
        surf_ap_minsize=input$surf_ap_minsize
      )
    })

    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })
    data<-reactive({
      req(input$surf_points_datalist)
      data<-vals$saved_data[[input$surf_points_datalist]]
      data
    })

    observeEvent(input$surf_points_datalist,{
      vals$cur_surf_points_datalist<-input$surf_points_datalist
    })

    output$datalist_out<-renderUI({
      choices=names(vals$saved_data)
      selected=vals$cur_surf_points_datalist
      selected=get_selected_from_choices(selected,choices)
      pickerInput_fromtop_live(ns("surf_points_datalist"),"Datalist",
                               choices=choices,
                               selected=selected)
    })

    observeEvent(input$surf_points_z_value,{
      vals$cur_surf_points_z_value<-input$surf_points_z_value
    })
    output$zvalue_out<-renderUI({
      data<-data()
      choices=colnames(data)
      selected=vals$cur_surf_points_z_value
      selected=get_selected_from_choices(selected,choices)
      pickerInput_fromtop_live(ns("surf_points_z_value"),"Z-Value",
                               selected=selected,choices=choices)
    })
    observeEvent(input$surf_points_z_color,{
      vals$cur_surf_points_z_color<-input$surf_points_z_color
    })
    output$zcolor_out<-renderUI({
      data<-data()
      choices<-c("Z-Value",colnames(cbind(data,attr(data,"factors"))))
      selected=vals$cur_surf_points_z_color
      selected=get_selected_from_choices(selected,choices)
      pickerInput_fromtop_live(ns("surf_points_z_color"),"Z-Color",   selected=selected,choices=choices)
    })
    observeEvent(input$surf_ap_z_scale_vars,{
      vals$cur_surf_ap_z_scale_vars<-input$surf_ap_z_scale_vars
    })
    output$zscale_vars<-renderUI({
      data<-data()
      choices=colnames(data)
      selected=vals$cur_surf_ap_z_scale_vars
      selected=get_selected_from_choices(selected,choices)
      pickerInput_fromtop_live(ns("surf_ap_z_scale_vars"),"Z-Value", choices=choices,  selected=selected )
    })

    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,"surf_ap_palette", choices =  vals$colors_img$val,choicesOpt = list(content =  vals$colors_img$img )
      )
    })



    return(NULL)

  })
}
surface_3d_control<-list()
surface_3d_control$ui<-function(id){
  ns<-NS(id)
  div(
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
          pickerInput_fromtop(ns("surf_tick"),span('+ ticktype',tipright("simple - draws just an arrow parallel to the axis to indicate direction of increase; detailed - draws normal ticks as per 2D plots.")), c("simple","detailed")),

          numericInput(ns("surf_d"),span('+ persp strength:',tipright("a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it")), 1)
      )

    )
  )

}
surface_3d_control$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    observe({


      shinyjs::toggle('surf_tick',condition = !vals$cur_modeplot%in%"plotly")
    })


    result<-reactive(
      list(
        surf_exp=input$surf_exp,
        surf_theta=input$surf_theta,
        surf_phi=input$surf_phi,
        surf_r=input$surf_r,
        surf_tick=input$surf_tick,
        surf_d=input$surf_d
      )
    )

    return(result())

  })
}
surface_3d_control$server_update<-function(id,vals){
  moduleServer(id,function(input,output,session){

    box_caret_server("box_surface",hide_content=T)


    result<-reactive(
      list(
        surf_exp=input$surf_exp,
        surf_theta=input$surf_theta,
        surf_phi=input$surf_phi,
        surf_r=input$surf_r,
        surf_tick=input$surf_tick,
        surf_d=input$surf_d
      )
    )
    observeEvent(result(),{
      shinyjs::addClass(selector=".run_map_btn",class= "save_changes")
    })

    return(NULL)

  })
}
surface_saved_maps<-list()
surface_saved_maps$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(
      ns("surface_savedmaps"),
      title="Saved maps",
      color="#c3cc74ff",

      div(
        div(style="display: flex",
            pickerInput_fromtop_live(ns("saved_maps"),"Saved map (Z)", NULL),
            actionLink(ns("trash_map"), icon('fas fa-trash'))
        ),
        pickerInput_fromtop_live(ns("saved_maps2"),"Saved map (Color)", NULL)
      )


    )
  )

}
surface_saved_maps$server_update<-function(id,vals){
  moduleServer(id,function(input,output,session){
    observeEvent(vals$saved_maps,{
      updatePickerInput(session,'saved_maps2',choices=names(vals$saved_maps))

      updatePickerInput(session,'saved_maps',choices=names(vals$saved_maps))
    })

    observeEvent(input$trash_map,ignoreInit = T,{
      req(input$saved_maps)
      req(vals$saved_maps)
      vals$saved_maps[[input$saved_maps]]<-NULL
    })
    observeEvent(input$saved_maps,{
      vals$cur_rst<-vals$saved_maps[[input$saved_maps]]
    })
    return(NULL)
  })
}
surface_saved_maps$server<-function(id){
  moduleServer(id,function(input,output,session){



    result<-reactive(
      list(
        saved_maps=input$saved_maps,
        saved_maps2=input$saved_maps2
      )
    )

    return(
      result()
    )


  })
}


## Modules for plotly donwload
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

collectInputs <- function(session, input_ids) {
  state <- list()
  for (id in input_ids) {
    state[[id]] <- session$input[[id]]
  }
  return(state)
}
