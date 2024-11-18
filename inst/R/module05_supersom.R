box_caret_server_show<-function(id, hide_content=F){
  moduleServer(id,function(input,output,session){
    updateActionLink(session,'show_hide',"+")
    shinyjs::show("content")

  })
}
box_caret_server_hide<-function(id, hide_content=F){
  moduleServer(id,function(input,output,session){
    updateActionLink(session,'show_hide',"-")
    shinyjs::hide("content")

  })
}

gethex3<-function(m){
  grid<-data.frame(m$grid$pts)
  pxpy<-getpxpy2(m)
  px<-pxpy[[1]][1]
  py<-pxpy[[1]][2]
  py2<-py
  rang<-pxpy[[2]]
  rang2<-pxpy[[3]]
  i=1
  hexs<-lapply(1:nrow(grid), function(i){
    a=grid[i,1]
    b=a-px
    c=a+px
    x=c(a,b,b,a,c,c,a)
    a=grid[i,2]
    b=a-(py2)
    c=a-py
    d=a+py
    e=a+(py2)
    y=c(b,c,d,e,d,c,b)
    res<-data.frame(x,y, neu=i)
    res[-c(1,4,7),1:2]

  })
  attr(hexs,'pxpy')<-pxpy
  hexs
}
hexagon_to_triangles<-function(x, y, z, i) {
  # Calcula o ponto central do hexágono no espaço 3D, colocando-o na posição externa
  center_x <- mean(x)
  center_y <- mean(y)
  center_z <- mean(z)
  res<-list()
  for (j in 1:6) {
    tri_x <- c(center_x, x[j] , x[j + 1] )
    tri_y <- c(center_y, y[j] , y[j + 1] )
    tri_z <- c(center_z, z[j] , z[j + 1] )
    res[[j]]<-data.frame(x=tri_x,y=tri_y,z=tri_z,j=j, i=i,center_x=center_x,center_y=center_y, center_z=center_z)
  }
  do.call(rbind,res)
}
get_torus<-function(som, R = 4, r = 3) {
  pts <- som$grid$pts
  hex_radius <- 0.5
  max_x <- max(pts[, 1]-0.5)
  max_y <- max(pts[, 2])
  hexs<-if(som$grid$topo=="hexagonal"){gethex(som)} else{gethex3(som) }


  result<-list()
  for (i in 1:nrow(pts)) {
    hex <- hexs[[i]]
    alpha <- (hex$x / max_x) * 2 * pi
    beta <- (hex$y / max_y) * 2 * pi
    x <- (R + r * cos(beta)) * cos(alpha)
    y <- (R + r * cos(beta)) * sin(alpha)
    z <- r * sin(beta)
    center_x <- mean(x)
    center_y <- mean(y)
    center_z <- mean(z) * 1.2
    result[[i]]<-hexagon_to_triangles(x, y, z,i)
  }

  result

}

plot_torus_from_som <- function(som, R = 3, r = 1) {
  # Extrai as dimensões da grade
  xdim <- som$grid$xdim
  ydim <- som$grid$ydim

  # Cria uma sequência de valores entre 0 e 2*pi com base nas dimensões da grade
  alpha_seq <- seq(0, 2 * pi, length.out = xdim)
  beta_seq <- seq(0, 2 * pi, length.out = ydim)

  # Cria uma malha completa usando expand.grid
  grid <- expand.grid(alpha = alpha_seq, beta = beta_seq)

  # Calcula as coordenadas cartesianas para o toro completo
  x <- (R + r * cos(grid$beta)) * cos(grid$alpha)
  y <- (R + r * cos(grid$beta)) * sin(grid$alpha)
  z <- r * sin(grid$beta)

  # Converte as coordenadas em matrizes para usar com persp3D
  x_matrix <- matrix(x, nrow = ydim, ncol = xdim, byrow = TRUE)
  y_matrix <- matrix(y, nrow = ydim, ncol = xdim, byrow = TRUE)
  z_matrix <- matrix(z, nrow = ydim, ncol = xdim, byrow = TRUE)

  # Plota o toro usando persp3D
  plot3D::persp3D(x = x_matrix, y = y_matrix, z = z_matrix, col = "white", theta = 20, phi = 40, expand = 0.7,colkey = FALSE, shade = 0.5, border="black",bty="n")
}
plot_torus <- function(m, R = 4, r = 3) {
  if(m$grid$topo=="rectangular"){
    return(plot_torus_from_som(m,R,r))
  }
  torus <- get_torus(m, R, r)
  df <- do.call(rbind, torus)

  # Inicializa a caixa de plotagem
  plot3D::perspbox(z = df$z, xlim = range(df$x), ylim = range(df$y), bty = "n", phi = 50)

  # Separa os dados por hexágono usando a coluna `i`
  dfl <- split(df, df$i)

  # Itera sobre cada hexágono e desenha seus polígonos com variação de cor
  for (i in seq_along(dfl)) {
    dfi <- dfl[[i]]

    # Remove o ponto central para desenhar apenas os vértices do hexágono
    vertices <- dfi[dfi$x != dfi$center_x | dfi$y != dfi$center_y | dfi$z != dfi$center_z, c("x", "y", "z")]

    # Calcula a média de z para simular sombra
    mean_z <- mean(vertices$z)

    # Define a cor baseada na altura z para criar um efeito de sombra
    color <- grDevices::gray(0.5 + 0.5 * (mean_z - min(df$z)) / (max(df$z) - min(df$z)))

    # Desenha o polígono com a cor simulada de iluminação
    plot3D::polygon3D(vertices$x, vertices$y, vertices$z, add = TRUE, col = color, border = "black")


  }
}


#' @export
training_som<-list()
#' @export


training_som$server<-function(id,vals,datalist_name,mysupersom,usepartition,partition_column,partition_ref){
  moduleServer(id,function(input,output,session){})
}

table_results_som<-list()
table_results_tab1<-list()
table_results_tab2<-list()
table_results_tab3<-list()
table_results_tab4<-list()
table_results_som$ui<-function(id){
  ns<-NS(id)
  div(

    style = "background: WhiteSmoke;",
    tabsetPanel(
      id=ns("som_res"),
      selected='train_tab1',
      tabPanel(
        value= 'train_tab1',
        "2.1. Parameters",
        table_results_tab1$ui(ns("som-tab1")),
        uiOutput(ns("out_tab1"))),
      tabPanel(
        "2.2. Changes & Counting",value = "train_tab2",
        table_results_tab2$ui(ns("som-tab2")),
        uiOutput(ns("out_tab2"))
      ),
      tabPanel(
        "2.3. BMUs", value = "train_tab5",
        table_results_tab3$ui(ns("som-tab3")),
        uiOutput(ns('out_tab3'))
      ),
      tabPanel("2.4. Network plot", value = "train_tab_net",
               table_results_tab4$ui(ns("som-tab4")),
               uiOutput(ns('out_tab4'))
      )




    )
  )
}
table_results_tab1$ui<-function(id){
  ns<-NS(id)

  div(

    column(
      6,class="mp0",
      box_caret(
        ns("tbox1"),
        title="Quality Measures",
        tip=actionLink(ns("som_quality_help"),icon("fas fa-question-circle")),
        uiOutput(ns("som_errors"))
      ),
      box_caret(
        ns("tbox1"),
        title="Downloads",
        color="#c3cc74ff",
        div(class='tip-80',
          div(div(tipify_ui(actionLink(ns('create_codebook'),span("Create Datalist",icon("fas fa-file-signature")), style="button_active"),"Create a datalist  with the codebook vectors"))),
          div(tipify_ui(actionLink(ns('save_bmu'),span("Save BMUs",icon("fas fa-file-signature")), style="button_active"),"Add the BMUs to the Factor-Attribute (training Data)")),
          div(actionLink(ns("down_pcodes_results"), span("Download codebook results"))),
          div(tipify_ui(downloadLink(ns("down_kohonen_results"), "Download model"),"download kohonen object as rds file containing the object of class kohonen with components")))
      )
    ),
    column(
      6,class="mp0",
      box_caret(
        ns("tbox2"),
        title="Training Parameters",
        tableOutput(ns("out_train.summary"))
      )
    )

  )
}
table_results_tab2$ui<-function(id){
  ns<-NS(id)

  div(
    column(6,class="mp0",box_caret(
      ns("ccbox1"),
      title="Changes",
      button_title = actionLink(ns("downp_pchanges"),"Download",icon('download')),
      plotOutput(ns("pchanges"))
    )),
    column(6,class="mp0",box_caret(
      ns("ccbox2"),
      title="Counting",
      button_title = actionLink(ns("downp_pcounts"),"Download",icon('download')),
      plotOutput(ns("pcounts"))
    ))


  )
}
table_results_tab3$ui<-function(id){
  ns<-NS(id)
  div(
    column(
      4,class="mp0",style="height: calc(100vh - 200px);overflow-y: auto",
      box_caret(
        ns("rbox1"),
        color="#c3cc74ff",
        title="Neurons",
        div(
          div(class="radio_search radio_yellow",
              radioGroupButtons(ns("codebook_somback_value"), span("Unit value:"), choices = c("None"="None","U-Matrix"="uMatrix","Property"="property"))
          ),

          pickerInput_fromtop(ns("codebook_property_layer"),label = "Layer:",choices = NULL),
          div(id=ns('codebook_var_pproperty'),style="display: flex; align-items: center; padding-left: 20px",class="half-drop-inline",
              div(
                actionLink(ns("codebook_prev_property"),"<<")),
              div("Variable:",style="max-width: 150px",
                  pickerInput_fromtop(ns("codebook_variable_pproperty"),label = NULL,choices = NULL)
              ),
              div(actionLink(ns("codebook_next_property"),">>"))

          ),

          pickerInput_fromtop_live(inputId = ns("codebook_bg_palette"),"Palette",NULL),
          numericInput(ns("border_width"),"Border width",value = 0.5,step=0.1),
          numericInput(ns("codebook_pcodes_bgalpha"), "Unit lightness",value = 0,min = 0,max = 1,step = .1),
          pickerInput_fromtop(ns("codebook_border"),label ='Border:',NULL),
          textInput(ns("codebook_neuron_legend"),"Legend text",NULL)
        )
      ),
      box_caret(
        ns("rbox2"),
        color="#c3cc74ff",
        hide_content = T,
        title=span(style="display: inline-block",
                   class="checktitle2",
                   checkboxInput(ns("codebook_addpoints"),strong("Points"),value=T,width="80px")
        ),
        div(
          div(id=ns("codebook_points_inputs"),
              pickerInput_fromtop_live(inputId = ns("codebook_points_palette"),
                                       label ="Palette",
                                       choices = NULL),
              pickerInput_fromtop(ns("codebook_points_factor"),"Factor",
                                  choices = NULL),
              pickerInput_fromtop(inputId = ns("codebook_symbol"),
                                  label = "Shape",
                                  choices = NULL),
              numericInput(ns("codebook_points_size"),"Size",value = 1,min = 0.1,max = 3,step = .1),
              checkboxInput(ns("show_legend"),'Show legend',T),
              textInput(ns("codebook_points_legend"),"Legend text","Observations")
          )
        )
      ),
      box_caret(
        ns("rbox3"),
        color="#c3cc74ff",
        hide_content = T,
        title=span(style="display: inline-block",
                   class="checktitle2",
                   checkboxInput(ns("codebook_addtext"),strong("Labels"),value=F,width="80px")
        ),
        div(id=ns('codebook_text_inputs'),
            pickerInput_fromtop_live(inputId = ns("codebook_text_palette"),
                                     label ="Palette",
                                     choices =  NULL),
            pickerInput_fromtop(ns("codebook_text_factor"),"Factor",
                                choices = NULL),
            numericInput(ns("codebook_text_size"),"Size",value = 1,min = 0.1,max = 3,step = .1),
            checkboxInput(ns("text_repel"),"Repel Labels",F),
            numericInput(ns("max.overlaps"),"max.overlaps",value = 10,min = 1,step = 1),

        )
      ),
      box_caret(
        ns("rbox4"),
        color="#c3cc74ff",
        hide_content = T,
        title=span(style="display: inline-block",
                   class="checktitle2",
                   checkboxInput(ns("codebook_addvfm"),strong("Variable factor map",actionLink(ns("codebook_varfacmap"), tipify_ui(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value=T,width="210px")
        ),

        div(id=ns('codebook_varfac_out'),

            pickerInput_fromtop(ns("codebook_vfm_type"),"Show correlation:",
                                choices =list("Highest"='var', "Chull"="cor")

            ),
            pickerInput_fromtop(ns("vfm_layer"),"Layer:",
                                choices =NULL,multiple=T),

            div(id=ns('codebook_vfm_out'),
                div(tipify_ui(numericInput(ns("codebook_npic"), "Number", value = 10, min = 2),"Number of variables to display")),
                div(numericInput(ns("vfm_max.overlaps"), "Max.Overlap", value = 10, min = 1)),


                numericInput(ns("codebook_pclus.cex.var"), "Size", value = 1, min = 2),
                pickerInput_fromtop(inputId = ns("codebook_p.clus.col.text"),
                                    label = "Color",
                                    NULL
                ),
                pickerInput_fromtop(inputId = ns("codebook_var_bg"),
                                    label = "Background",
                                    choices =NULL),
                numericInput(ns("codebook_var_bg_transp"), "Transparency", value = 0, min = 2,),
                div(actionLink(ns('down_pcorr_results'),"Download VFM results")),
                div(actionLink(ns('create_vfm_results'),"Create Datalist using VFM"))
            )
        )
      ),

      div(id=ns("show_box_var_pie"),
          box_caret(
            ns("box_var_pie"),
            color="#c3cc74ff",
            hide_content = T,
            title=span(style="display: inline-block",
                       class="checktitle2",

                       checkboxInput(ns("codebook_addpie"),strong("Variable pies",tipify_ui(actionLink(ns("var_pie_help"), icon("fas fa-question-circle")),"Click for details","right")),value =F,width="160px"),

            ),

            div(id=ns('var_pie_out'),
                pickerInput_fromtop(ns("var_pie_type"),"Rank:",choices =list("Top importance"="rsquared","Relative importance"="top","Absolute Value"="top_w","Manual"="manual")),
                pickerInput_fromtop(ns("var_pie_layer"),"Layer",NULL),
                div(class="virtual-130",
                    virtualPicker(ns("var_pie_manual"),"variables selected")
                ),

                numericInput(ns("var_pie_n"), span(tipright("Number of variables to display"),"Number"), value = 10, min = 2),
                pickerInput_fromtop_live(ns("var_pie_bg"),label = "Palette",choices = NULL),
                numericInput(ns("var_pie_transp"), "Transparency", value = 0, min = 2))
          )
      ),
      box_caret(
        ns("rbox5"),
        title = "General options",
        color="#c3cc74ff",
        div(
          numericInput(ns("codebook_base_size"),"Base size",value = 12),
          checkboxInput(ns("codebook_theme"),
                        label = "show neuron coordinates",
                        value=F
          ),
          textInput(ns("codebook_title"), "Title: ", "")
        )
      )
    ),
    column(
      8,class="mp0",
      box_caret(
        ns('rbox6'),
        title="Best-Matching units",
        button_title = actionLink(ns('downp_bmu'),"Download",icon("download")),
        div(uiOutput(ns("bmu_plot")))
      )
    )

  )
}
table_results_tab4$ui<-function(id){
  ns<-NS(id)

  div(
    column(
      6,class="mp0",
      box_caret(ns("box_setup5"),
                color="#c3cc74ff",
                title="Snap-Based Model Retraining",
                tip=tipright("The panel enables users to recreate the model at pre-determined snaps. The snap parameter determines the iterations at which the model will be recreated. For example, if a model with 500 iterations is saved, setting snap to 250 will recreate the model at the 250th and 500th iterations."),
                div(class="inline_pickers",
                    numericInput(ns("rlen_snap2"), span(tipright("Number of snapshots or intermediate models to generate"), "snap"), 3),
                    div(actionButton(ns("run_play2"), "Run >>",style="height: 30px;padding: 5px")),

                    hidden(div(style="padding-top: 10px",
                               class="anim_opt2",
                               div(strong('Animation options')),
                               numericInput(ns("play_interval2"), span(tipright("Interval between each training iteration in milliseconds."), "interval"), 100),
                               checkboxInput(ns("loop2"), "Loop", FALSE)
                    ))
                )),

      box_caret(ns("box_setup7"),
                title="Plot options",
                color="#c3cc74ff",
                div(checkboxInput(ns("gwnp_show_labels"),"Show Neuron Labels",T),
                    checkboxInput(ns("gwnp_show_points"),"Show Observations",F)),

      )


    ),
    column(6,box_caret(ns("box_setup6"),
                       inline=F,
                       button_title = actionLink(ns("download_gwnp"),
                                                 "Download",icon("download")),
                       title="Grid-Weighted Neuron Plot",
                       div(uiOutput(ns("play_out2")),
                           uiOutput(ns("plot_animation2")))
    ))


  )
}
table_results_tab1$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    current_som_model<-reactive({
      req(inherits(vals$cur_kohonen,"kohonen"))
      vals$cur_kohonen
    })
    data_x<-reactive({
      attr(current_som_model(),"Datalist")[1]
    })



    create_coodebok_datalist<-reactive({
      m<-current_som_model()
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
      res=data.frame(current_som_model()$codes[[1]])
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
      df<-data.frame(dplyr::group_by( sf::st_as_sf(my_df,coords = c("x", "y"),crs = 4326),neu))
      li<-lapply(split(df,df$neu),function(x){
        x$geometry<-  sf::st_combine(x$geometry)
        x
      })
      base_shape<-sf::st_cast(st_sf(do.call(rbind,li)),"POLYGON")


      attr(data,"base_shape")<-base_shape
      data
    })

    observeEvent(input$create_codebook,ignoreInit = T,{
      pd0<-data<-create_coodebok_datalist()
      m<-current_som_model()

      req(data_x())
      data<-data_migrate(vals$saved_data[[data_x()]],data)
      attr(data,"coords")<-NULL
      attr(data,"factors")<-data.frame(id=factor(rownames(pd0)))

      bag<-attr(m,'model_name')
      if(is.null(bag)){
        bag<-attr(m,"model_tag")
      }
      bag<-paste0(bag,"_codebook")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,"bag")<-bag
      vals$newdatalist<-data
      module_save_changes$ui(ns("codebook-create"), vals)
    })

    module_save_changes$server("codebook-create", vals)

    output$som_errors<-renderUI({
      som_qualist<-errors_som(current_som_model())
      res<-data.frame(som_qualist$som_quality)

      res$mean=apply(data.frame(lapply(res,function(x)as.numeric(x))),1,mean)
      neu.uti<-data.frame(som_qualist$neu.uti)
      rownames(neu.uti)<-"neu.uti"
      res_names<-rownames(res)
      div(


        renderTable(res,rownames =T),
        renderTable(neu.uti,rownames =T,colnames =F),
      )
    })


    observeEvent(ignoreInit = T,input$save_bmu,{
      savebmu()
    })

    savebmu<-reactive({
      temp<-current_som_model()

      bmu<-temp$unit.classif
      names(bmu)<-rownames(temp$data[[1]])
      bmu<-data.frame(bmu=as.factor(bmu))
      factors<-attr(vals$saved_data[[data_x()]],"factors")
      factors<-data.frame(factors,bmu)
      attr(vals$saved_data[[data_x()]],"factors")<-factors
    })

    observeEvent(ignoreInit = T,input$down_pcodes_results,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"Codebook results"
      data<- data.frame( kohonen::getCodes(current_som_model()))
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Codebook results",data=data, name=name)
    })
    output$down_kohonen_results<-{
      downloadHandler(
        filename = function() {
          paste0("Kohonen","_", Sys.Date(),".rds")
        }, content = function(file) {
          saveRDS(current_som_model(),file)
        })
    }


    output$out_train.summary<-renderTable({
      train.summary()
    }, rownames = T, colnames = F,striped=T, spacing ="xs")
    train.summary<-reactive({
      m<-current_som_model()

      req(inherits(m,"kohonen"))
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

    observeEvent(ignoreInit = T,input$som_quality_help, {

      pkgs<-'aweSOM'
      citations<-do.call('c',lapply(pkgs, citation))
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

    return(NULL)

  })
}
table_results_tab2$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    current_som_model<-reactive({
      req(inherits(vals$cur_kohonen,"kohonen"))
      vals$cur_kohonen
    })
    data_x<-reactive({
      attr(current_som_model(),"Datalist")[1]
    })

    observeEvent(ignoreInit = T,input$downp_pchanges,{

      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=g_pchanges()
      datalist_name=attr(current_som_model(),'model_name')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Changes plot", name_c="changes",datalist_name=datalist_name)
    })
    g_pchanges<-reactive({
      pchanges(current_som_model())
      res<-recordPlot()
      res
    })
    output$pchanges<-renderPlot({
      req(length(current_som_model())>0)
      g_pchanges()
    })
    output$pcounts<-renderPlot({
      req(length(current_som_model())>0)
      g_pcounts()
    })
    g_pcounts<-reactive({
      gg_pcounts(current_som_model())

    })
    observeEvent(ignoreInit = T,input$downp_pcounts,{

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=g_pcounts()
      datalist_name=attr(current_som_model(),'model_name')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Counting plot", name_c="counting",datalist_name=datalist_name)
    })
    return(NULL)

  })
}
table_results_tab3$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns

    current_som_model<-reactive({
      req(inherits(vals$cur_kohonen,"kohonen"))
      vals$cur_kohonen
    })

    observeEvent(current_som_model(),{
      m<-current_som_model()
      choices=names(m$data)
      updatePickerInput(session,'vfm_layer',choices=choices,selected=choices)
    })


    data_x<-reactive({
      attr(current_som_model(),"Datalist")[1]
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

    box_caret_server("box_var_pie")
    box_caret_server("rbox1")
    box_caret_server("rbox2",hide_content = T)
    box_caret_server("rbox3",hide_content = T)
    box_caret_server("rbox4",hide_content = T)
    box_caret_server("rbox5",hide_content = T)
    box_caret_server("rbox6")

    output$importance_results<-renderUI({
      imp_results<-attr(hcplot4(),"imp_results")
      req(imp_results)
      actionLink(ns("show_hc_imp"),"Importance results",icon("expand"))
    })
    output$create_importance_results<-renderUI({
      p<-hcplot4()
      imp_results<-attr(p,"imp_results")
      imp_layer<-attr(p,"imp_layer")
      req(imp_layer%in%names(vals$saved_data))
      req(imp_results)
      div(actionLink(ns("create_hc_imp"),span("Create Datalist",tiphelp("Create Datalist with selected variables for pies")),icon("creative-commons-share")))
    })

    observeEvent(input$create_hc_imp,{

      p<-hcplot4()
      imp_results<-attr(p,"imp_results")
      imp_layer<-attr(p,"imp_layer")
      imp_vars<-attr(p,"imp_vars")
      req(imp_layer)
      req(imp_vars)
      req(imp_layer%in%names(vals$saved_data))

      data_o<-vals$saved_data[[imp_layer]]
      req(data_o)
      req(imp_vars%in%colnames(data_o))
      data<-data_o[,imp_vars,drop=F]
      req(data)
      data<-data_migrate(data_o,data)

      bag<-paste0(imp_layer,"_som_top_vars")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,"bag")<-bag
      vals$newdatalist<-data
      module_save_changes$ui(ns("som-imp-create"), vals)

    })
    module_save_changes$server("som-imp-create", vals)
    observeEvent(input$show_hc_imp,{
      data<-attr(hcplot4(),"imp_results")
      req(data)
      showModal(
        modalDialog(
          title="SOM Variable Importance Results",
          easyClose = T,
          div(class="half-drop-inline",
              div(actionLink(ns("download_hc_imp"),"Download",icon("download"))),

              fixed_dt(data,dom = 'lt',
                       pageLength=20,
                       lengthMenu = list(c(20, -1), c( "20","All")))
          )


        )
      )
    })
    observeEvent(input$download_hc_imp,{
      data<-attr(hcplot4(),"imp_results")
      req(data)
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"som_imp_results"
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Permutation Importance Results",data=data, name=name)

    })
    observeEvent(input$var_pie_layer,{
      m<-current_som_model()

      choices<-colnames(m$data[[input$var_pie_layer]])

      shinyWidgets::updateVirtualSelect(
        "var_pie_manual",
        choices=choices,
        selected=choices[1:4]
      )
    })
    observe({
      shinyjs::toggle('var_pie_manual',condition=input$var_pie_type%in%"manual")
      shinyjs::toggle('var_pie_n',condition=!input$var_pie_type%in%"manual")
    })
    observeEvent(input$var_pie_help, {
      showModal(
        modalDialog(
          title = "Variable Pies in SOM Codebook",
          easyClose = TRUE,
          div(
            p("The variables to display in the pie plots can be ranked using three methods:"),
            p("The pie plots represent variables from the trained codebook, where each value is calculated as the square root of the squared codebook weights. This allows for a straightforward comparison of variable contributions within each SOM unit."),
            tags$ul(
              tags$li(strong("Top importance"),
                      p("Variables are ranked based on their ability to explain the clustering of the SOM. Importance is calculated by evaluating the coefficient of determination (R²) for each variable, representing the proportion of variance explained relative to the data associated with each classification unit in the SOM. Variables with the highest R² values are selected, highlighting those that contribute most to the SOM’s data structure.")
              ),
              tags$li(strong("Relative importance"),
                      p("The importance of each variable is determined based on the codebook weights for each neuron. Relative importance scores are calculated by normalizing each variable's codebook weights by the sum of weights for each neuron. Variables are ranked based on the sum of their relative importance scores across all neurons.")
              ),
              tags$li(strong("Absolute value"),
                      p("The absolute importance of each variable is based on the sum of its codebook weights across all neurons, providing a direct measure of overall weight. Variables are ranked by the highest absolute weights.")
              ),
              tags$li(strong("Manual"),
                      p("Select variables to display manually.")
              )
            )
          )
        )
      )
    })

    observeEvent(current_som_model(),{
      updatePickerInput(session,'var_pie_layer',choices=names(current_som_model()$data))

    })
    observe({
      shinyjs::toggle('var_pie_layer',condition=length(current_som_model()$data)>1)
    })
    observeEvent(input$codebook_addpie,{
      if(isTRUE(input$codebook_addpie)){
        lapply(c('codebook_addpoints','codebook_addvfm'),function(x){
          updateCheckboxInput(session,x,value=F)
        })


      }
    })

    observeEvent(input$codebook_somback_value,{
      if(input$codebook_somback_value%in%c('uMatrix','property')){
        updateCheckboxInput(session,'codebook_addpie',value=F)
        updateCheckboxInput(session,'codebook_addpoints',value=T)
      }
    })

    observe({
      shinyjs::toggle("codebook_neuron_legend",condition=input$codebook_somback_value!='None')
    })
    observe({
      shinyjs::toggle('show_box_var_pie',condition=input$codebook_somback_value=='None')
    })

    observeEvent(input$codebook_addpie,ignoreInit = T,{
      if(isTRUE(input$codebook_addpie)){
        updatePickerInput(session,"codebook_points_palette",
                          selected="black",
                          choices =  vals$colors_img$val[getsolid_col()],
                          choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ))
      } else{
        updatePickerInput(session,"codebook_points_palette",
                          choices =  vals$colors_img$val,
                          choicesOpt = list(content =  vals$colors_img$img ),selected=vals$pclus_points_palette)
      }
    })
    observe({
      req(is.null(vals$pclus_points_palette))

      vals$pclus_points_palette<-"black"

    })
    observe({
      shinyjs::toggle("codebook_points_factor",condition = isFALSE(input$codebook_addpie))

    })
    observeEvent(input$codebook_points_palette,{
      cols<-vals$newcolhabs[[input$codebook_points_palette]](8)
      shinyjs::toggle('codebook_points_factor',condition = cols[1]!=cols[2])
      if(cols[1]==cols[2]){
        updateTextInput(session,'codebook_points_legend',value="Observations")
      } else{
        updateTextInput(session,'codebook_points_legend',value=input$codebook_points_factor)
      }

    })

    observe({
      shinyjs::toggle("var_pie_out",condition=isTRUE(input$codebook_addpie))
    })
    observe({
      updatePickerInput(session,'var_pie_bg',choices = vals$colors_img$val[getgrad_col()],selected="viridis",choicesOpt = list(content =  vals$colors_img$img[getgrad_col()]))

    })



    output$codebook_plus_umatrix<-renderUI({
      if(is.null(vals$plus_umatrix)){vals$plus_umatrix<-F}
      checkboxInput(ns("codebook_plus_umatrix"),strong("+ U-Matrix:"), value=vals$plus_umatrix)
    })
    codebook_getdata_layer<-reactive({
      choices0 = names(current_som_model()$data)
      if(length(choices0)>0){
        req(input$codebook_property_layer)
        current_som_model()$data[[input$codebook_property_layer]]
      } else{
        current_som_model()$data[[1]]
      }

    })
    codebook_indicate_hc<-reactive({
      npic<-NULL
      indicate<-NULL
      if(isTRUE(input$codebook_addvfm)){

        npic<- input$codebook_npic
        indicate<- input$codebook_vfm_type
      }
      iind=list(indicate=indicate,npic=npic)
      iind
    })
    codebook_bp_som<-reactive({
      iind=codebook_indicate_hc()
      m<-current_som_model()
      req(input$vfm_layer)
      m$data<-m$data[input$vfm_layer]
      m$codes<-m$codes[input$vfm_layer]
      bp<-getbp_som2(m=m,indicate=iind$indicate,npic=iind$npic,hc=vals$cutsom)
      vals$biplot_som<-bp
      bp
    })
    codebook_get_network<-reactive({
      req(input$codebook_somback_value)
      backtype=NULL
      property=NULL
      if(input$codebook_somback_value=="property"){
        req(input$codebook_variable_pproperty)
        backtype="property"
        property=input$codebook_variable_pproperty
      } else if(input$codebook_somback_value=="uMatrix"){
        backtype="uMatrix"
      }
      m<-current_som_model()
      hexs<-get_neurons(m,background_type=backtype,property=property, hc=NULL)
      updateTextInput(session,'codebook_neuron_legend',value=attr(hexs,"leg_name"))
      hexs
    })
    bmu_text_points<-function(data,text_factor,points_factor,points_tomap){
      factors<-attr(data,"factors")
      if(length(text_factor)>0){
        req(text_factor%in%colnames(factors))
        text_factor= factors[rownames(data),text_factor, drop=F]
        points_tomap$label<-text_factor[rownames(points_tomap),]
      }
      if(length(points_factor)>0){
        req(points_factor%in%colnames(factors))
        df= factors[rownames(data),points_factor, drop=F]
        points_tomap$point<-df[rownames(points_tomap),]
        attr(points_tomap,"namepoints")<-points_factor
      }
      points_tomap
    }

    points_tomap<-reactive({
      m<-current_som_model()
      pm=rescale_copoints(hexs=codebook_get_network(),copoints=getcopoints(m))
      data<-vals$saved_data[[data_x()]]
      pm=bmu_text_points(data,input$codebook_text_factor,input$codebook_points_factor,pm)
      pm
    })

    observeEvent(input$codebook_addpoints,ignoreInit = T,{
      if(isTRUE(input$codebook_addpoints)){
        box_caret_server_show('rbox2')
      } else{
        box_caret_server_hide('rbox2')
      }
    })

    observeEvent(input$codebook_addtext,ignoreInit = T,{
      if(isTRUE(input$codebook_addtext)){
        box_caret_server_show('rbox3')
      } else{
        box_caret_server_hide('rbox3')
      }
    })


    observeEvent(input$codebook_addvfm,ignoreInit = T,{
      if(isTRUE(input$codebook_addvfm)){
        box_caret_server_show('rbox4')
      } else{
        box_caret_server_hide('rbox4')
      }
    })
    observeEvent(input$codebook_addpie,ignoreInit = T,{
      if(isTRUE(input$codebook_addpie)){
        box_caret_server_show('box_var_pie')
      } else{
        box_caret_server_hide('box_var_pie')
      }
    })


    codebook_argsplot<-reactive({
      req(input$codebook_pcodes_bgalpha)
      if(isTRUE(input$codebook_addpoints)){
        req(input$codebook_points_factor)
        points_factor= NULL }
      if(isTRUE(input$codebook_addtext)){
        req(input$codebook_text_factor)
        text_factor= NULL }
      indicate=codebook_indicate_hc()
      m<-current_som_model()
      errors<-NULL
      args<-list(m=m,
                 hexs=codebook_get_network(),
                 points_tomap=points_tomap(),
                 bp=codebook_bp_som(),
                 points=input$codebook_addpoints,
                 points_size=input$codebook_points_size,
                 points_palette=input$codebook_points_palette,
                 pch=as.numeric(input$codebook_symbol),
                 text=input$codebook_addtext,
                 text_size=input$codebook_text_size,
                 text_palette=input$codebook_text_palette,
                 bg_palette=input$codebook_bg_palette,
                 newcolhabs=vals$newcolhabs,
                 bgalpha=input$codebook_pcodes_bgalpha,
                 border=input$codebook_border,
                 indicate=indicate$indicate,
                 cex.var=as.numeric(input$codebook_pclus.cex.var),
                 col.text=input$codebook_p.clus.col.text,
                 col.bg.var=input$codebook_var_bg,
                 col.bg.var.alpha=1-input$codebook_var_bg_transp,
                 show_error=errors,
                 base_size=input$codebook_base_size,
                 show_neucoords=input$codebook_theme,
                 newdata=input$codebook_newdata,
                 title=input$codebook_title,
                 hc=NULL,
                 var_pie=input$codebook_addpie,
                 var_pie_type=input$var_pie_type,
                 n_var_pie=input$var_pie_n,
                 Y_palette=input$var_pie_bg,
                 var_pie_transp=input$var_pie_transp,
                 var_pie_layer=input$var_pie_layer,
                 pie_variables=input$var_pie_manual,
                 border_width=input$border_width,
                 fill_neurons=input$fill_neurons,
                 text_repel=input$text_repel,
                 max.overlaps=input$max.overlaps,
                 show_legend=input$show_legend,
                 points_legend=input$codebook_points_legend,
                 neuron_legend=input$codebook_neuron_legend,
                 vfm_max.overlaps=input$vfm_max.overlaps
      )

      args

    })

    observe({
      shinyjs::toggle('max.overlaps',condition=isTRUE(input$text_repel))
    })
    output$bmu_plot<-renderUI({


      renderPlot({


        args<-codebook_argsplot()

        bp<-args$bp
        bp$id=NULL
        vals$biplot_som<-bp
        args$points_palette

        p<-try(do.call(bmu_plot,args))
        req(!inherits(p,"try-error"))
        vals$bmus_plot<-p
        vals$bmus_plot
      })

    })
    bag_vfm<-reactive({

      name0<-paste0(data_x(),"_",input$codebook_npic,"vars")
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      name1[length(vals$saved_data)+1]
    })
    observeEvent(ignoreInit = T,input$codebook_varfacmap, {
      showModal(
        modalDialog(
          uiOutput(session$ns("textvarfacmap")),
          title = h4(strong("Variable factor map")),
          footer = modalButton("close"),
          size = "m",
          easyClose = TRUE
        )
      )
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
          div(

            p("The chart is very similar to the variable factor map obtained from the principal component analysis (PCA). It calculates the weighted correlation for each variable using the coordinates (x, y) of the neurons and their weights (number of instances). The codebooks vectors of the cells correspond to an estimation of the conditional averages, calculating their variance for each variable is equivalent to estimating the between-node variance of the variable, and hence their relevance."),
            p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("Chull correlations")," returns",code("npic")," variables with the highest correlation considering the convex hull, while also ensuring that the points are ordered by their proximity to codebook center")
          )

        )
      )

    })
    observeEvent(vals$newcolhabs,{
      choices = vals$colors_img$val[getsolid_col()]
      choicesOpt=list(content=vals$colors_img$img[getsolid_col()])

      updatePickerInput(session,'codebook_p.clus.col.text',
                        choices=choices,
                        choicesOpt=choicesOpt,
                        selected="black"
      )

      updatePickerInput(session,'codebook_var_bg',
                        choices=choices,
                        choicesOpt=choicesOpt,
                        selected="white"
      )


      updatePickerInput(session,'codebook_border',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "white")



      updatePickerInput(session,'codebook_text_palette',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "black"
      )
      updatePickerInput(session,'codebook_points_palette',
                        choices = vals$colors_img$val,
                        choicesOpt = list(content = vals$colors_img$img),
                        selected="black"
      )
    })
    observeEvent(codebook_getdata_layer(),{
      updatePickerInput(session,'codebook_variable_pproperty',choices=colnames(codebook_getdata_layer()),options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(current_som_model(),{
      choices = names(current_som_model()$data)
      updatePickerInput(session,'codebook_property_layer',choices=choices)
    })
    df_symbol<-data.frame(
      val = c(16,15,17,18,8,1,5,3)
    )
    for(i in 1:length(symbols))
    {
      symbol1<-base64enc::dataURI(file = paste0('inst/www/pch',i,".png"), mime = "image/png")

      df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}
    observeEvent(df_symbol$val,{
      updatePickerInput(session,'codebook_symbol',
                        choices = df_symbol$val,
                        choicesOpt = list(content = df_symbol$img))

    })


    observeEvent(data_x(),{
      choices<-c(colnames(attr(vals$saved_data[[data_x()]],"factors")))
      updatePickerInput(session,'codebook_text_factor',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))

      updatePickerInput(session,'codebook_points_factor',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observeEvent(input$codebook_addvfm,shinyjs::toggle("codebook_varfac_out",condition=isTRUE(input$codebook_addvfm)))
    observeEvent(input$codebook_addtext,{
      shinyjs::toggle("codebook_text_inputs",condition=isTRUE(input$codebook_addtext))
    })
    observe({
      data = codebook_getdata_layer()
      shinyjs::toggle('codebook_prev_property',condition=which(colnames(data)==input$codebook_variable_pproperty)!=1)

      shinyjs::toggle('codebook_next_property',condition=which(colnames(data)!=input$codebook_variable_pproperty)==ncol(data))
    })


    cur_propert<-reactiveVal(1)




    observeEvent(input$codebook_prev_property,{
      cur<-cur_propert()
      req(cur>1)
      cur_propert(cur-1)
    })
    observeEvent(input$codebook_next_property,{
      cur<-cur_propert()
      req(cur<ncol(codebook_getdata_layer()))
      cur_propert(cur+1)
    })

    stop_update_propperty<-reactiveVal(F)
    observeEvent(cur_propert(),{
      req(input$codebook_variable_pproperty)
      #req(isFALSE(stop_update_propperty()))
      # stop_update_propperty(T)
      choices<-colnames(codebook_getdata_layer())
      if(choices[cur_propert()]!=input$codebook_variable_pproperty)
        updatePickerInput(session,'codebook_variable_pproperty', selected=choices[cur_propert()])
      #stop_update_propperty(F)
    })

    observeEvent(input$codebook_variable_pproperty,{
      #req(isFALSE(stop_update_propperty()))
      #stop_update_propperty(T)
      pic<-which(colnames(codebook_getdata_layer())%in%input$codebook_variable_pproperty)
      if(pic!=cur_propert())
        cur_propert(pic)
      #stop_update_propperty(F)
    })


    observeEvent(input$codebook_addpoints,{
      shinyjs::toggle("codebook_points_inputs",condition=isTRUE(input$codebook_addpoints))
    })
    observeEvent(input$codebook_somback_value,{
      shinyjs::toggle("codebook_var_pproperty",condition=input$codebook_somback_value=="property")
      shinyjs::toggle("codebook_property_layer",condition=input$codebook_somback_value=="property")
    })
    observeEvent(input$codebook_somback_value,{
      if(input$codebook_somback_value=="uMatrix"){
        updatePickerInput(session,"codebook_bg_palette",selected="viridis")
      }
    })
    codebook_get_choices_pal<-reactive({

      req(input$codebook_somback_value)
      title="+ Unit palette"
      if(input$codebook_somback_value=="None"){

        choices=getsolid_col()
      } else {


        choices=getgrad_col()

      }


      attr(choices,"title")<-title
      choices
    })
    observeEvent(codebook_get_choices_pal(),{
      choices<-codebook_get_choices_pal()
      title<-attr(choices,"title")
      req(input$codebook_somback_value)
      selected<-ifelse(input$codebook_somback_value=="None","gray","viridis")

      updatePickerInput(session,'codebook_bg_palette',
                        choices =  vals$colors_img$val[choices],
                        selected=selected,
                        choicesOpt = list(
                          content =  vals$colors_img$img[choices] ))
    })

    observeEvent(ignoreInit = T,input$down_pcorr_results,{
      vals$hand_down<-"generic"


      module_ui_downcenter("downcenter")
      name<-"Biplot results"
      data<- data.frame(vals$biplot_som)
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download VFM results",data=data, name=name)
    })

    create_som_vfm<-reactive({
      data_o<-vals$saved_data[[data_x()]]
      m<-current_som_model()
      dd<-do.call(cbind,m$data)
      colnames(dd)<-unlist(sapply(m$data,colnames))
      temp<-data.frame(dd[,rownames(vals$biplot_som),drop=F])
      colnames(temp)<-rownames(vals$biplot_som)
      temp
    })



    observeEvent(input$create_vfm_results,ignoreInit = T,{
      m<-current_som_model()
      pd0<-data<-create_som_vfm()
      head(create_som_vfm())
      req(data_x())
      data<-data_migrate(vals$saved_data[[data_x()]],data)

      bag<-attr(m,'model_name')
      if(is.null(bag)){
        bag<-attr(m,"model_tag")
      }
      bag<-paste0(bag,"_vars_vfm")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,"bag")<-bag
      vals$newdatalist<-data


      module_save_changes$ui(ns("vfm-create"), vals)
    })

    module_save_changes$server("vfm-create", vals,message="Note that the trained SOM is composed by multiple layers. Variables from different layers will be merged (cbind) into a single Datalist")

    observeEvent(ignoreInit = T,input$downp_bmu,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=vals$bmus_plot
      name_c<-'bmu_plot'
      if(input$codebook_somback_value=="property"){

        name_c=  gsub("[^[:alnum:]]","-", input$codebook_variable_pproperty)
      }
      datalist_name=attr(current_som_model(),'model_name')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="BMU plot", name_c=name_c,datalist_name=datalist_name)
    })

    return(NULL)

  })
}
table_results_tab4$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    current_som_model<-reactive({
      req(inherits(vals$cur_kohonen,"kohonen"))
      vals$cur_kohonen
    })
    data_x<-reactive({
      attr(current_som_model(),"Datalist")[1]
    })
    box_caret_server("box_setup5")
    box_caret_server("box_setup6")
    box_caret_server("box_setup7")

    output$play_out2<-renderUI({
      req(modelplay2())


      div(
        div(sliderInput(session$ns("animation2"), "Animation:",
                        min = 1, max = length(modelplay2()),
                        value = 1, step = 1,
                        animate =
                          animationOptions(interval = input$play_interval2,
                                           playButton=tags$div(
                                             tags$span("Play >> ",style="font-size: 14px;font-weight: bold; background:  #05668D;color: white; padding: 3px;"),style=" margin-top: 5px"
                                           ),
                                           loop = input$loop2)))
      )
    })

    output$plot_animation2<-renderUI({
      renderPlot({get_plot_animation2()})

    })




    observeEvent(input$run_play2,ignoreInit = T,{
      m<-current_som_model()

      ms<-recreate_som(m,length.out=input$rlen_snap2,session = getDefaultReactiveDomain())
      modelplay2(ms)
    })
    modelplay2<-reactiveVal(NULL)
    get_plot_animation2<-reactive({
      if(is.null(modelplay2())){
        plotnetwork_list2(current_som_model(), label=input$gwnp_show_labels,show_points=input$gwnp_show_points)
      } else {

        plotnetwork_list2(modelplay2()[[input$animation2]], label=input$gwnp_show_labels,show_points=input$gwnp_show_points)


      }

    })
    observeEvent(modelplay2(),{
      shinyjs::toggle(selector=".anim_opt2",condition=length(modelplay2())>0)
    })

    observeEvent(ignoreInit = T,input$download_gwnp,{
      req(get_plot_animation2())
      vals$hand_plot<-"generic_replay"
      module_ui_figs("downfigs")
      generic=get_plot_animation2()
      datalist_name=attr(current_som_model(),'model_name')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Grid-Weighted Neuron Plot", name_c="grid_weig_neuronplot",datalist_name=datalist_name)
    })
    return(NULL)

  })
}
table_results_som$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    output$out_tab1<-renderUI({
      table_results_tab1$server("som-tab1",vals)
      NULL
    })
    output$out_tab2<-renderUI({
      table_results_tab2$server("som-tab2",vals)
      NULL
    })
    output$out_tab3<-renderUI({
      table_results_tab3$server("som-tab3",vals)
      NULL
    })
    output$out_tab4<-renderUI({
      table_results_tab4$server("som-tab4",vals)
      NULL
    })


  })
}
table_predict_som<-list()
table_predict_som$ui<-function(id){
  ns<-NS(id)

  div(

    column(
      4,class="mp0",
      box_caret(
        ns("pred_header"),
        color="#c3cc74ff",
        title="New Data",
        div(
          div(class="radio_search radio_yellow",
              uiOutput(ns("out_sompred_type"))


          ),
          div(id=ns('sompred_type_datalist'),
              style="padding-left: 10px",
              div(strong("New data layers:")),
              uiOutput(ns("whatmap_newdata"))

          ),
          div(id=ns('sompred_type_traintest'),
              style="padding-left: 10px",
              class="virtual-120",

              virtualPicker(ns("newdata_layer"),"Layer selected",label=NULL,choices=NULL,styles=' font-size: 12px',allOptionsSelectedText="All layers",search=F,selectAllText="Select All layers",alwaysShowSelectedOptionsCount=F,showSelectedOptionsFirst=F)


          ),
          div(uiOutput(ns("validate_newdata"))),
          div(id=ns('run_predSOM_btn'),
              align="right",
              class="save_changes",
              actionButton(ns('run_predSOM'),"RUN >>",style="height: 25px; padding-top: 2px; padding-bottom: 2px;"))
        )

      ),
      div(
        div(id=ns("pred_result_options"),
            box_caret(
              ns('box_pred_results'),
              title="Options",
              color="#c3cc74ff",
              div(
                selectInput(ns('pred_results'),"Prediction results:",choices=c(
                  "Best-matching units"='unit.classif',
                  "Data"="predictions",
                  "Codebook"="unit.predictions"
                )),
                uiOutput(ns("pick_layer_result")),
                uiOutput(ns("ui_pred_result_newdata")),
                div(style="padding-top: 5px",
                    uiOutput(ns("link_predsom_newdata")),
                    uiOutput(ns("link_predsom_codebook"))
                )
              )

            )
        ),
        uiOutput(ns("pred_perf_options")),
        div(
          id=ns('pred_bmu_options'),
          box_caret(
            ns("pbox1"),
            hide_content = T,
            color="#c3cc74ff",
            title="Neurons",
            div(
              div(class="radio_search radio_yellow",
                  radioGroupButtons(ns("predcode_somback_value"), span("Unit value:"), choices = c("None"="None","U-Matrix"="uMatrix","Property"="property"))
              ),

              pickerInput_fromtop(ns("predcode_property_layer"),label = "Layer:",choices = NULL),
              div(id=ns('predcode_var_pproperty'),style="display: flex",
                  actionButton(ns("predcode_prev_property"),"<<"),
                  pickerInput_fromtop(ns("predcode_variable_pproperty"),label = "Variable:",choices = NULL),
                  actionButton(ns("predcode_next_property"),">>")

              ),

              pickerInput_fromtop_live(inputId = ns("predcode_bg_palette"),"Palette",NULL),
              numericInput(ns("predcode_bgalpha"), "Unit lightness",value = 0,min = 0,max = 1,step = .1),
              pickerInput_fromtop(ns("predcode_border"),label ='Border:',NULL)
            )
          ),
          box_caret(
            ns("pbox2"),
            color="#c3cc74ff",
            title=span(style="display: inline-block",
                       class="checktitle2",
                       checkboxInput(ns("predcode_addpoints"),strong("Points"),value=T,width="80px")
            ),
            div(
              div(id=ns("predcode_points_inputs"),
                  checkboxInput(ns("show_training"),"Show Training",value=T),
                  pickerInput_fromtop_live(inputId = ns("predcode_points_palette"),
                                           label ="Palette",
                                           choices = NULL),
                  pickerInput_fromtop(ns("predcode_points_factor"),"Factor",
                                      choices = NULL),
                  pickerInput_fromtop(inputId = ns("predcode_symbol"),
                                      label = "Shape",
                                      choices = NULL),
                  numericInput(ns("predcode_points_size"),"Size",value = 1,min = 0.1,max = 3,step = .1),

                  textInput(ns("bmu_legend"),"Legend text",NULL)
              )
            )
          ),
          box_caret(
            ns("pbox3"),
            hide_content = T,
            color="#c3cc74ff",
            title=span(style="display: inline-block",
                       class="checktitle2",
                       checkboxInput(ns("predcode_addtext"),strong("Labels"),value=F,width="80px")
            ),
            div(id=ns('predcode_text_inputs'),
                pickerInput_fromtop_live(inputId = ns("predcode_text_palette"),
                                         label ="Palette",
                                         choices =  NULL),
                pickerInput_fromtop(ns("predcode_text_factor"),"Factor",
                                    choices = NULL),
                numericInput(ns("predcode_text_size"),"Size",value = 1,min = 0.1,max = 3,step = .1)
            )
          ),
          box_caret(
            ns("pbox4"),
            color="#c3cc74ff",
            hide_content = T,
            title=span(style="display: inline-block",
                       class="checktitle2",
                       checkboxInput(ns("predcode_addvfm"),strong("Variable factor map",actionLink(ns("predcode_varfacmap"), tipify_ui(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"), "Click for more details"))),value=F,width="210px")
            ),

            div(id=ns('predcode_varfac_out'),
                pickerInput_fromtop(ns("predcode_vfm_type"),"Show correlation:",
                                    choices =list("Highest"='var', "Chull"="cor")

                ),

                div(id=ns('predcode_vfm_out'),
                    div(tipify_ui(numericInput(ns("predcode_npic"), "Number", value = 10, min = 2),"Number of variables to display")),
                    numericInput(ns("predcode_pclus.cex.var"), "Size", value = 1, min = 2),
                    pickerInput_fromtop(inputId = ns("predcode_p.clus.col.text"),
                                        label = "Color",
                                        NULL
                    ),
                    pickerInput_fromtop(inputId = ns("predcode_var_bg"),
                                        label = "Background",
                                        choices =NULL),
                    numericInput(ns("predcode_var_bg_transp"), "Transparency", value = 0, min = 2,),
                    div(actionLink(ns('down_pcorr_results_pred'),"Download VFM results")),
                    div(actionLink(ns('create_vfm_results_pred'),"Create Datalist using VFM"))
                )
            )
          ),
          box_caret(
            ns("pbox5"),
            title = "General options",
            color="#c3cc74ff",
            hide_content = T,
            div(
              numericInput(ns("predcode_base_size"),"Base size",value = 12),
              checkboxInput(ns("predcode_theme"),
                            label = "show neuron coordinates",
                            value=F
              ),
              textInput(ns("predcode_title"), "Title: ", "")
            )
          )
        )

      )
    ),
    column(8,class="mp0",
           uiOutput(ns("teste")),
           tabsetPanel(
             id=ns("predsom_tab"),

             tabPanel(
               strong("3.1. Results"),style="background: white",
               value = "predsom_tab01",

               uiOutput(ns('som_predict_results'))

             ),
             tabPanel(
               strong("3.2. Performace"),style="background: white",
               value = "predsom_tab01b",
               uiOutput(ns("pred_performace"))
             ),
             tabPanel(
               strong("3.3. BMUs"),style="background: white",
               value = "predsom_tab01c",
               box_caret(
                 ns('pbox6'),
                 title="BMU plot",
                 button_title = actionLink(ns('download_pbox6'),"Download",icon("download")),
                 div(uiOutput(ns("predbmu_plot")))
               )
             )


           ),
           div(
             class="map_control_style",

             uiOutput(ns("newdata_summ"))

           )
    )


  )
}
table_predict_som$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    current_som_model<-reactive({
      req(inherits(vals$cur_kohonen,"kohonen"))
      vals$cur_kohonen
    })
    data_x<-reactive({
      attr(current_som_model(),"Datalist")[1]
    })
    get_sompred<-reactiveVal()
    box_caret_server("pred_header")
    box_caret_server("pbox1",hide_content=T)
    box_caret_server("pbox2")
    box_caret_server("pbox3",   hide_content = T)
    box_caret_server("pbox4",   hide_content = T)
    box_caret_server("pbox5",   hide_content = T)
    box_caret_server("pbox6")

    observeEvent(input$predcode_addpoints,ignoreInit = T,{
      if(isTRUE(input$predcode_addpoints)){
        box_caret_server_show('pbox2')
      } else{
        box_caret_server_hide('pbox2')
      }
    })

    observeEvent(input$predcode_addtext,ignoreInit = T,{
      if(isTRUE(input$predcode_addtext)){
        box_caret_server_show('pbox3')
      } else{
        box_caret_server_hide('pbox3')
      }
    })


    observeEvent(input$predcode_addvfm,ignoreInit = T,{
      if(isTRUE(input$predcode_addvfm)){
        box_caret_server_show('pbox4')
      } else{
        box_caret_server_hide('pbox4')
      }
    })



    observe({
      choices = vals$colors_img$val[getsolid_col()]
      choicesOpt=list(content=vals$colors_img$img[getsolid_col()])



      updatePickerInput(session,'predcode_border',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "white"
      )
      updatePickerInput(session,'predcode_text_palette',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "black"
      )

      updatePickerInput(session,'predcode_points_palette',
                        choices = vals$colors_img$val,
                        choicesOpt = list(content = vals$colors_img$img),
                        selected="black"
      )

      updatePickerInput(session,'predcode_p.clus.col.text',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "black"
      )

      updatePickerInput(session,'predcode_var_bg',
                        choices =  vals$colors_img$val[getsolid_col()] ,
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] ),
                        selected= "white"
      )
    })


    output$teste<-renderUI({
      # div("teste",style="height: 100px;overflow-y: auto",renderPrint(names(get_sompred()$predictions))     )
    })
    output$ui_pred_result_newdata<-renderUI({
      req(input$layer_result)
      req(input$tab_pred_result)
      table<-get_pred_newdata()
      div(class="half-drop-inline",
          fixed_dt_con$ui(ns('pred_results'),table,max_length=100)
      )

    })
    output$som_predict_results<-renderUI({
      #req(get_sompred())
      box_caret(ns("box_pred_results_out"),

                title="Table",
                button_title = div(

                  div(
                    actionLink(ns('downtable_predict_result'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-table"))))
                  )
                ),
                tabsetPanel(
                  type="hidden",
                  id=ns("tab_pred_result"),
                  tabPanel(
                    title = "Best-matching units",
                    value="unit.classif",
                    uiOutput(ns("out_pred_result_bmu"))
                  ),
                  tabPanel(
                    title = "New Data (X)",
                    value="predictions",

                    uiOutput(ns("out_pred_result_newdata"))
                  ),
                  tabPanel(
                    title = "Codebook",
                    value="unit.predictions",
                    uiOutput(ns('ui_pred_result_codebook')),
                    uiOutput(ns("out_pred_result_codebook"))
                  )
                ))

    })
    output$out_pred_result_newdata<-renderUI({
      table<-get_pred_newdata()
      fixed_dt_con$server('pred_results',table,max_length=100)
    })
    output$out_pred_result_bmu<-renderUI({
      #req(input$tab_pred_result)
      table<-get_pred_bmu()
      div(class="half-drop-inline",
          fixed_dt(table,dom='lt',pageLength=20)
      )

    })

    output$pred_result_options<-renderUI({

    })
    output$out_pred_result_codebook<-renderUI({
      table<-get_pred_codebook()
      fixed_dt_con$server('pred_results',table,max_length=100)
    })
    output$pick_layer_result<-renderUI({
      req(input$sompred_type)
      #req(input$sompred_type%in%c("Partition","Training"))

      #req(input$tab_pred_result%in%c("predictions","unit.predictions"))
      m<-current_som_model()
      req(inherits(m,"kohonen"))

      req(get_sompred())
      pred<-get_sompred()
      choices=names(pred$predictions)

      div(

        selectInput(ns("layer_result"), "Show layer:",choices=choices, selected=vals$cur_layer_result)
      )
    })
    output$pred_performace<-renderUI({
      box_caret(
        ns("box_pred_perf_out"),
        title="Table",
        button_title = uiOutput(ns('header_perf')),
        tabsetPanel(
          id=ns("get_predsom_perf"),

          tabPanel(
            value="predsom_perf_overhall",
            title = "Overall performace",
            uiOutput(ns("overhall_peform"))),
          tabPanel(
            value="predsom_perf_obs",
            title = "Performace by observation",
            uiOutput(ns("obs_peform"))
          ),
          tabPanel(
            value="predsom_perf_var",
            title = "Performace by variable",
            uiOutput(ns("var_peform"))
          )
        )
      )


    })
    output$obs_peform<-renderUI({
      vals$som_perf_result<-table<-get_obs_peform()
      div(class="half-drop-inline",
          fixed_dt(table,dom='lt',pageLength=20)
      )




    })
    output$var_peform<-renderUI({
      vals$som_perf_result<-table<-get_var_peform()
      div(class="half-drop-inline",
          fixed_dt(table,dom='lt',pageLength=20)
      )




    })
    output$overhall_peform<-renderUI({
      vals$som_perf_result<-table<-get_overhall_peform()
      div(class="half-drop-inline",
          fixed_dt(table,dom='t')
      )


    })
    output$header_perf<-renderUI({
      div(
        actionLink(ns('downtable_perf_result'),span("+ Download",span(icon("fas fa-download"),icon("fas fa-table")))
        ),
        uiOutput(ns("link_predsom_perf_obs"))

      )
    })
    output$link_predsom_perf_obs<-renderUI({
      req(input$get_predsom_perf=="predsom_perf_obs")
      actionLink(ns('link_predsom_perf_obs_create'),span("+ Create a Datalist"))
    })


    observeEvent(input$link_predsom_perf_obs_create,{
      temp<-vals$som_perf_result
      data_o<-vals$saved_data[[data_x()]]
      data<-data_migrate(data_o,temp)
      attr(data,"bag")<-paste0('som_',data_x(),'perf_pred_obs')
      vals$newdatalist<-data
      module_save_changes$ui(ns("pred-obs"), vals)

    })
    module_save_changes$server("pred-obs", vals)
    output$link_predsom_newdata<-renderUI({
      req(input$tab_pred_result=="predictions")
      actionLink(ns('link_predsom_newdata_create'),span("+ Create a Datalist"))
    })

    observeEvent(input$link_predsom_codebook_create,ignoreInit = T,{
      data<-get_pred_codebook()
      #showModal(module_som())
      req(data_x())
      attr(data,"factors")<-data.frame(id=rownames(data))
      coords<-data.frame(current_som_model()$grid$pts)
      colnames(coords)<-c("x","y")
      rownames(coords)<-rownames(data)
      attr(data,"coords")<-coords
      attr(data,"bag")<-paste0('som_',data_x(),'codebook_pred')
      vals$newdatalist<-data
      module_save_changes$ui(ns("pred-codebook"), vals)
    })
    module_save_changes$server("pred-codebook", vals)

    observeEvent(ignoreInit = T,input$link_predsom_newdata_create,{
      data<-get_pred_newdata()
      #showModal(module_som())
      req(data_x())
      data<-data_migrate(vals$saved_data[[data_x()]],data)
      attr(data,"bag")<-paste0('som_',data_x(),'_predictions')
      vals$newdatalist<-data
      module_save_changes$ui(ns("pred-data"), vals)
    })
    module_save_changes$server("pred-data", vals)
    output$link_predsom_codebook<-renderUI({
      req(input$tab_pred_result=="unit.predictions")
      actionLink(ns('link_predsom_codebook_create'),span("+ Create a Datalist"))
    })
    output$predbmu_plot<-renderUI({
      renderPlot({getbmu_plot()})

    })
    output$whatmap_newdata<-renderUI({
      m<-current_som_model()
      datas<-attr(m,"Datalist")
      lapply(m$whatmap,function(w){
        choices<-names(which(sapply(vals$saved_data,function(x){
          all(colnames(x)%in%colnames(m$data[[w]]))
        })))

        pickerInput_fromtop(ns(paste0("newdata_w",w)),datas[[w]],c("None",choices))
      })
    })
    output$predcode_plus_umatrix<-renderUI({
      if(is.null(vals$plus_umatrix)){vals$plus_umatrix<-F}
      checkboxInput(ns("predcode_plus_umatrix"),strong("+ U-Matrix:"), value=vals$plus_umatrix)
    })


    get_newdata<-reactive({
      req(input$sompred_type)
      req(input$sompred_type=="Datalist")
      m<-current_som_model()
      datas<-attr(m,"Datalist")
      newdata_names<-sapply(m$whatmap,function(w){
        req(input[[paste0("newdata_w",w)]])
        input[[paste0("newdata_w",w)]]
      })

      newdata_names<-newdata_names[which(newdata_names!="None")]
      news<-vals$saved_data[newdata_names]
      res<-lapply(news,as.matrix)
      names(res)<-names(m$data)[which(newdata_names!="None")]
      res
    })
    supersom_newdata0<-reactive({
      req(input$sompred_type)
      m<-current_som_model()
      if(input$sompred_type%in%"Partition"){
        return(attr(m,"test"))
      }  else if(input$sompred_type%in%'Training'){
        return(current_som_model()$data)
      } else if(input$sompred_type%in%'Datalist'){
        return(get_newdata())
      }

    })

    supersom_newdata<-reactive({
      res<-supersom_newdata0()
      if(input$sompred_type%in%c("Training","Partition")){
        res<-res[input$newdata_layer]
      }
      res
    })

    output$validate_newdata<-renderUI({
      m<-current_som_model()
      req(input$sompred_type)
      if(input$sompred_type =='Datalist'){
        newdata_names<-sapply(m$whatmap,function(w){
          req(input[[paste0("newdata_w",w)]])
          input[[paste0("newdata_w",w)]]
        })
        validation<-!all(sapply(newdata_names,function(x) x=="None"))
      } else{
        validation<-length(input$newdata_layer)>0
      }

      if(validation){
        shinyjs::show('run_predSOM')
      } else{
        shinyjs::hide('run_predSOM')
      }

      validate(need(validation,"Please supply new data for at least one SOM layer"))
    })

    get_overhall_peform<-reactive({

      newdata<-supersom_newdata0()
      pred<-get_sompred()$predictions
      m<-current_som_model()

      lapply(pred,dim)

      res<-lapply(names(newdata),function(i){
        caret::postResample(pred[[i]],newdata[[i]])
      })
      names(res)<-names(newdata)

      do.call(rbind,res)
    })
    get_obs_peform<-reactive({

      newdata<-do.call(cbind,supersom_newdata())
      req(newdata)
      req(get_sompred())
      pred<-do.call(cbind,get_sompred()$predictions)
      req(pred)
      obs_mean=apply(newdata,1,function(x) mean(x,na.rm=T))
      pred_mean= apply(pred,1,function(x) mean(x,na.rm=T))

      res<-data.frame(
        obs_mean=obs_mean,
        pred_mean=pred_mean,
        do.call(rbind,lapply(1:nrow(newdata),function(i){
          caret::postResample(pred[i,],newdata[i,])
        }))
      )
      rownames(res)<-rownames(get_sompred()$predictions[[1]])
      res

    })
    get_var_peform<-reactive({

      newdata_list<-supersom_newdata0()
      pred_list<-get_sompred()$predictions

      result<-lapply(names(get_sompred()$predictions),function(layer){
        newdata<-newdata_list[[layer]]
        pred<-pred_list[[layer]]
        res<-data.frame(layer=layer,
                        variable=colnames(pred),
                        obs_mean=sapply(data.frame(newdata),function(x) mean(x,na.rm=T)),
                        pred_mean=sapply(data.frame(pred),function(x) mean(x,na.rm=T)),
                        do.call(rbind,
                                lapply(1:ncol(newdata),function(i){
                                  pr<-pred[,i]
                                  ob<-newdata[,i]

                                  data.frame(


                                    as.list(caret::postResample(pr,ob)))
                                })))




        res
      })
      do.call(rbind,result)






    })
    get_pred_newdata<-reactive({
      req(input$layer_result)
      req(input$layer_result%in%names(get_sompred()$predictions))
      get_sompred()$predictions[[input$layer_result]]
    })
    get_pred_bmu<-reactive({
      res<-get_sompred()
      res_temp<-data.frame(bmu=res[['unit.classif']])
      rownames(res_temp)<-rownames(res[["predictions"]][[1]])
      res_temp
    })
    get_pred_codebook<-reactive({
      req(input$layer_result)
      res<-get_sompred()
      if(is.null(res$predictions)){
        res$predictions<-res$data
      }
      req(input$layer_result%in%names(res[["unit.predictions"]]))
      res<-res[["unit.predictions"]][[input$layer_result]]
      req(nrow(res)>0)
      rownames(res)<-paste0("neu",1:nrow(res))
      res
    })
    bag_tab_pred_result<-reactive({
      name0<-paste(data_x(),input$tab_pred_result,input$layer_result, sep="_")
      name1<-make.unique(c(names(vals$saved_data),name0), sep="_")
      name1[length(vals$saved_data)+1]
    })

    create_predsom_errors<-reactive({

      data_o<-switch(input$sompred_type,
                     "Partition"={
                       vals$saved_data[[data_x()]][rownames(attr(current_som_model(),"test")[[1]]),]
                     },
                     "Datalist"={vals$saved_data[[input$predsom_new]]},
                     "Training"={vals$saved_data[[data_x()]]}
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
    predcode_getdata_layer<-reactive({
      choices0 = names(current_som_model()$data)
      if(length(choices0)>0){
        req(input$predcode_property_layer)
        current_som_model()$data[[input$predcode_property_layer]]
      } else{
        current_som_model()$data[[1]]
      }

    })
    predcode_indicate_hc<-reactive({
      npic<-NULL
      indicate<-NULL
      if(isTRUE(input$predcode_addvfm)){

        npic<- input$predcode_npic
        indicate<- input$predcode_vfm_type
      }
      iind=list(indicate=indicate,npic=npic)
      iind
    })
    predcode_bp_som<-reactive({
      iind=predcode_indicate_hc()
      m<-current_som_model()
      bp<-getbp_som2(m=m,indicate=iind$indicate,npic=iind$npic,hc=vals$cutsom)
      vals$predcode_bp_som<-bp
      bp
    })
    predcode_get_network<-reactive({
      req(input$predcode_somback_value)
      backtype=NULL
      property=NULL
      if(input$predcode_somback_value=="property"){
        req(input$predcode_variable_pproperty)
        backtype="property"
        property=input$predcode_variable_pproperty
      } else if(input$predcode_somback_value=="uMatrix"){
        backtype="uMatrix"
      }



      m<-current_som_model()
      hexs<-get_neurons(m,background_type=backtype,property=property, hc=NULL)
      vals$predcode_hc_network<-hexs
      hexs
    })
    predcode_get_copoints<-reactive({
      m<-get_som_model_pred()
      copoints<-getcopoints(m)
      vals$copoints_hc2<-copoints
      copoints
    })

    observeEvent(input$predcode_points_palette,{
      vals$cur_predcode_points_palette<-input$predcode_points_palette
    })
    observeEvent(input$show_training,{

      if(isTRUE(input$show_training)){
        selected=NULL
        pic=getgrad_col()
      } else{
        selected="black"
        pic=1:length(vals$colors_img$val)}


      choices =  vals$colors_img$val[pic]
      choicesOpt = list(
        content =  vals$colors_img$img[pic])


      updatePickerInput(session,'predcode_points_palette',choices=choices,selected=selected,choicesOpt=choicesOpt)

    })
    predcode_copoints_scaled<-reactive({
      predcode_get_network()
      predcode_get_copoints()
      points_tomap=rescale_copoints(hexs=vals$predcode_hc_network,copoints=vals$copoints_hc2,jitter=T)
      data<-vals$saved_data[[data_x()]]

      if(isFALSE(input$show_training)){
        vals$predcode_hc_mapsom<-points_tomap
        return(points_tomap)
      }

      factors<-attr(data,"factors")
      if(length(input$predcode_text_factor)>0){
        req(input$predcode_text_factor%in%colnames(factors))
        text_factor= factors[rownames(data),input$predcode_text_factor, drop=F]
        points_tomap$label<-text_factor[rownames(points_tomap),]
      }

      if(length(input$predcode_points_factor)>0){
        req(input$predcode_points_factor%in%colnames(factors))
        points_factor= factors[rownames(data),input$predcode_points_factor, drop=F]
        points_tomap$point<-points_factor[rownames(points_tomap),]
        attr(points_tomap,"namepoints")<-input$predcode_points_factor
      }



      vals$predcode_hc_mapsom<-points_tomap
      points_tomap
    })
    predcode_get_choices_pal<-reactive({


      req(length(input$predcode_somback_value)>0)
      title="+ Unit palette"
      if(input$predcode_somback_value=="None"){
        vals$somplot_bg<-"gray"
        choices=getsolid_col()
      } else {

        vals$somplot_bg<-"viridis"
        choices=getgrad_col()

      }


      attr(choices,"title")<-title
      choices
    })




    observe({
      req(length(input$show_training)>0)
      value="Predictions"
      req(input$sompred_type)
      if(input$sompred_type=="Partition"){
        value="Test"
      }

      updateTextInput(session,"bmu_legend",value=value)
    })


    predcode_argsplot<-reactive({

      req(input$predcode_bgalpha)

      if(isTRUE(input$predcode_addpoints)){
        req(input$predcode_points_factor)
        points_factor= NULL }


      if(isTRUE(input$predcode_addtext)){
        req(input$predcode_text_factor)
        text_factor= NULL }

      indicate=predcode_indicate_hc()


      m<-current_som_model()

      tryco<-try(predcode_copoints_scaled(), silent = F)

      req(!inherits(tryco,"try-error"))
      #savereac()
      trybp<-try( predcode_bp_som(), silent = T)

      req(!inherits(trybp,"try-error"))

      errors<-NULL




      copoints2<-vals$copoints2
      copoints3<-copoints2
      #opoints2$point<-args$points_factor
      #attach(vals$args)

      vals$predcode_hc_mapsom$point<-input$bmu_legend

      args<-list(m=m,
                 hexs=vals$predcode_hc_network,
                 points_tomap=vals$predcode_hc_mapsom,
                 bp=vals$predcode_bp_som,
                 points=input$predcode_addpoints,
                 points_size=input$predcode_points_size,
                 points_palette=input$predcode_points_palette,
                 pch=as.numeric(input$predcode_symbol),
                 text=input$predcode_addtext,
                 text_size=input$predcode_text_size,
                 text_palette=input$predcode_text_palette,
                 bg_palette=input$predcode_bg_palette,
                 newcolhabs=vals$newcolhabs,
                 bgalpha=input$predcode_bgalpha,
                 border=input$predcode_border,
                 indicate=indicate$indicate,
                 cex.var=as.numeric(input$predcode_pclus.cex.var),
                 col.text=input$predcode_p.clus.col.text,
                 col.bg.var=input$predcode_var_bg,
                 col.bg.var.alpha=1-input$predcode_var_bg_transp,
                 show_error=errors,
                 base_size=input$predcode_base_size,
                 show_neucoords=input$predcode_theme,
                 newdata=input$predcode_newdata,
                 title=input$predcode_title,
                 hc=NULL,
                 points_legend=input$bmu_legend
      )

      args

    })
    getbmu_plot<-reactive({
      args<-predcode_argsplot()

      req(length(args)>0)
      bp<-args$bp
      bp$id=NULL
      vals$biplot_som<-bp
      p<-do.call(bmu_plot,args)

      if(isTRUE(input$show_training)){
        ptrain<-points_training()

        ptrain$point<-"Training"
        print(head(ptrain))
        colors<-vals$newcolhabs[[input$predcode_points_palette]](2)
        p<- p+geom_point(data=ptrain,aes(x_pt,y_pt,color=point),size=input$predcode_points_size+2)+scale_color_manual(values=c(colors),name="")
      }

      p
    })

    observe({
      shinyjs::toggle('predcode_points_factor',condition=isFALSE(input$show_training))
    })

    points_training<-reactive({
      backtype<-NULL
      m<-current_som_model()
      hexs<-get_neurons(m,background_type=backtype,property=NULL, hc=NULL)
      rescale_copoints(hexs=hexs,copoints=getcopoints(m))
    })


    getpred_model<-reactive({

      res0<-res<-get_sompred()

      pic_result<-input$predcode_property_layer

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


    observe({
      shinyjs::toggle('pred_result_options',condition=input$predsom_tab=='predsom_tab01')

      shinyjs::toggle('ui_pred_result_newdata',condition=input$tab_pred_result%in%c('predictions','unit.predictions'))
      shinyjs::toggle('pick_layer_result',condition=input$tab_pred_result%in%c('predictions','unit.predictions'))
    })
    observe(shinyjs::toggle('pred_bmu_options',condition=input$predsom_tab=='predsom_tab01c'))

    observe({
      data = predcode_getdata_layer()
      condition=which(colnames(data)==input$predcode_variable_pproperty)!=1
      shinyjs::toggle('predcode_prev_property',condition=condition)
      condition=which(colnames(data)!=input$predcode_variable_pproperty)==ncol(data)
      shinyjs::toggle('predcode_next_property',condition=condition)
    })



    output$out_sompred_type<-renderUI({
      m<-current_som_model()
      test<-attr(m,"test")


      choices<-c("Training","Datalist")
      if(length(test)>0){
        if(test[1]!="None"){
          if(any(sapply(test,nrow)>0)){
            choices=c("Partition","Training","Datalist")
          }

        }
      }

      selected=vals$cur_sompred_type
      selected<-get_selected_from_choices(selected,choices)
      div(
        radioGroupButtons(ns("sompred_type"),NULL,choices=c(choices),selected=selected)
      )
    })

    observeEvent(input$sompred_type,{
      vals$cur_sompred_type<-input$sompred_type
    })
    observeEvent(input$sompred_type,{
      shinyjs::toggle("sompred_type_datalist", condition =input$sompred_type =='Datalist')
      shinyjs::toggle("sompred_type_traintest",condition=input$sompred_type!='Datalist')
    })

    observeEvent(current_som_model(),{
      choices=names(current_som_model()$data)
      shinyWidgets::updateVirtualSelect('newdata_layer',choices=choices,selected=choices)
    })


    observeEvent(current_som_model(),{
      get_sompred(NULL)
      shinyjs::addClass('run_predSOM_btn',"save_changes")
    })
    observeEvent(supersom_newdata(),{
      get_sompred(NULL)
      shinyjs::addClass('run_predSOM_btn',"save_changes")
    })
    observeEvent(input$run_predSOM,ignoreInit = T,{


      m<-current_som_model()
      req(m)

      newdata<-supersom_newdata()


      if(length(m$data)==1){
        names(newdata)<-NULL
      }
      whatmap<-NULL
      if(input$sompred_type%in%'Datalist'){
        newdata_names<-sapply(m$whatmap,function(w){
          req(input[[paste0("newdata_w",w)]])
          input[[paste0("newdata_w",w)]]
        })
        whatmap<-which(newdata_names!="None")
      }

      pred<-predict(m,newdata,whatmap=whatmap)
      get_sompred(pred)
      shinyjs::removeClass('run_predSOM_btn',"save_changes")
    })

    get_som_model_pred<-reactive({
      pred<-get_sompred()
      m_pred<-m<-current_som_model()
      m_pred$data<-pred$predictions[input$predcode_property_layer]
      m_pred$codes<-pred$unit.predictions[input$predcode_property_layer]
      unit.classif<-pred$unit.classif
      names(unit.classif)<-rownames(pred$predictions[[1]])
      m_pred$unit.classif<-unit.classif
      picmap<-which(names(m$data)==input$predcode_property_layer)
      m_pred$dist.fcts<- m$dist.fcts[picmap]
      m_pred$distance.weights<- m$distance.weights[picmap]
      m_pred$user.weights<-m$user.weights[picmap]
      m_pred$whatmap=1
      m_pred

    })


    observeEvent(input$pred_results,ignoreInit = T,{
      updateTabsetPanel(session,'tab_pred_result',selected =input$pred_results)
    })
    observeEvent(input$tab_pred_result,{
      table<-switch(input$tab_pred_result,
                    "unit.classif"=get_pred_bmu(),
                    "predictions"=get_pred_newdata(),
                    "unit.predictions"=get_pred_codebook()
      )
      vals$som_predict_result<-table

    })
    observeEvent(ignoreInit = T,input$pred_results,{
      vals$cur_tab_pred_result<-input$pred_results
    })
    observeEvent(ignoreInit = T,input$layer_result,{
      vals$cur_layer_result<-input$layer_result
    })
    observeEvent(ignoreInit = T,input$downtable_predict_result,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"SOM prediction results"
      data<- data.frame(vals$som_predict_result)

      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Prediction results",data=data, name=name)
    })
    observeEvent(input$pred_performace,ignoreInit = T,{
      updateTabsetPanel(session,'get_predsom_perf',selected=input$pred_performace)
    })
    observeEvent(ignoreInit = T,input$download_pbox6,{

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=getbmu_plot()
      datalist_name=attr(current_som_model(),'model_name')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="BMU plot - predictions", name_c="bmu_pred",datalist_name=datalist_name)
    })

    message_download_perfomace<-reactive({
      switch(
        input$get_predsom_perf,
        "predsom_perf_overhall"="Overall performace",
        "predsom_perf_obs"="Performace by observation",
        "predsom_perf_var"="Performace by variable"
      )
    })

    observeEvent(ignoreInit = T,input$downtable_perf_result,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-paste0(input$get_predsom_perf)
      data<- data.frame(vals$som_perf_result)
      message<-paste0("Download SOM predictions (",message_download_perfomace(),")")


      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message=message,data=data, name=name)
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
                 p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("Chull correlations")," returns",code("npic")," variables with the highest correlation considering the convex hull, while also ensuring that the points are ordered by their proximity to codebook center")
          )

        )
      )

    })

    observeEvent(ignoreInit = T,input$predcode_varfacmap, {
      showModal(modalDialog(
        uiOutput(ns("textvarfacmap")),
        title = h4(strong("Variable factor map")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      ))
    })

    observeEvent(input$predcode_somback_value,{
      if(input$predcode_somback_value=="uMatrix"){
        updatePickerInput(session,"predcode_bg_palette",selected="viridis")
      }
    })
    observeEvent(predcode_get_choices_pal(),{
      choices<-predcode_get_choices_pal()
      updatePickerInput(session,'predcode_bg_palette',
                        choices =  vals$colors_img$val[choices],
                        choicesOpt = list(
                          content =  vals$colors_img$img[choices] )
      )
    })
    observeEvent(predcode_getdata_layer(),{
      choices<-colnames(predcode_getdata_layer())
      updatePickerInput(session,'predcode_variable_pproperty',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    df_symbol<-data.frame(
      val = c(16,15,17,18,8,1,5,3)
    )
    for(i in 1:length(symbols))
    {
      symbol1<-base64enc::dataURI(file = paste0('inst/www/pch',i,".png"), mime = "image/png")

      df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}
    observeEvent(df_symbol$val,{

      updatePickerInput(session,'predcode_symbol',
                        choices = df_symbol$val,
                        choicesOpt = list(content = df_symbol$img))
    })
    observeEvent(data_x(),{
      choices<-c(colnames(attr(vals$saved_data[[data_x()]],"factors")))
      updatePickerInput(session,'predcode_points_factor',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))

      updatePickerInput(session,'predcode_text_factor',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))

    })
    observeEvent(input$predcode_somback_value,{
      shinyjs::toggle("predcode_property_layer_out",condition=input$predcode_somback_value=="property")
    })
    observeEvent(input$predcode_addpoints,{
      shinyjs::toggle("predcode_points_inputs",condition=isTRUE(input$predcode_addpoints))
    })
    observeEvent(input$predcode_addtext,{
      shinyjs::toggle('predcode_text_inputs',condition=isTRUE(input$predcode_addtext))
    })
    observeEvent(input$predcode_addvfm,{
      shinyjs::toggle("predcode_varfac_out",condition=isTRUE(input$predcode_addvfm))
    })
    observeEvent(current_som_model(),{
      choices = names(current_som_model()$data)
      updatePickerInput(session,'predcode_property_layer',choices=choices)
    })





  })
}


#' @export
imesc_supersom<-list()
#' @export
imesc_supersom$ui<-function(id, vals){
  module_progress("Loading module: Self-Organizing Maps")
  ns<-NS(id)
  div(

    h4("Self-Organizing Maps", class="imesc_title"),

    div(   tags$style(HTML("
                           .inline_pickers2 .dropdown .btn{
                           height: 30px;

}
                           .inline_pickers2 .form-control{
                           height: 30px;
                           border-radius: 0px;
                           padding: 0px;
padding-left: 3px;
                           margin: 0px
                           }

                    .inline_pickers2 .shiny-input-container{
                     margin-right: 5px;
                    padding: 0px;
    }"
    )),
    div(
      #actionLink(ns("save_bug"),"save bug",style="font-size:10px"),

      column(8,class="mp0",
             box_caret(ns("box_setup1"),
                       color="#374061ff",
                       inline=F,
                       title="Model Setup",
                       button_title =
                         switchInput(ns("mysupersom"),"supersom",size="mini", inline=T,labelWidth="75px",handleWidth="30px"),

                       div(

                         div(style="vertical-align: text-top;display: flex;",
                             div(style="display: flex;",
                                 div(
                                   uiOutput(ns("data_som_out")),

                                 ),
                                 div(style="margin-top: 25px",
                                     uiOutput(ns("saved_som_print")))
                             ),
                             uiOutput(ns("supersom_layers")),
                             shinyBS::popify(
                               div(class="save_changes",style="margin-top: 25px",
                                   shinyBS::bsButton(ns("tools_savesom"), div(icon("fas fa-save")),style  = "animation: glowing 1000ms infinite;", type="action",value=FALSE)
                               )
                               , NULL, "Save the som model in the Datalist"
                             )
                         )
                       )

             )
      ),

      column(4,class="mp0",id=ns('som_models_panel'),
             uiOutput(ns("som_models_panel"))
      ),

      column(4,class="mp0",id=ns('partition_panel'),
             box_caret(ns('box_setup3'),
                       title="Partition",
                       color="#374061ff",
                       inline = F,
                       button_title =
                         switchInput(ns("usepartition"),"Use partition",size="mini", inline=T,labelWidth="75px",handleWidth="30px"),


                       div(id=ns("partition_on"),class="inline_pickers",
                           uiOutput(ns("data_somY_out"))

                       )))
    )),

    column(12,class="mp0",tabsetPanel(
      id = ns("som_tab"),



      tabPanel(
        value = "som_tab1",

        strong("1. Training"),
        div(
          div(
            column(
              6,class="mp0",
              box_caret(
                ns("box_setup4"),inline=F,
                color="#c3cc74ff",
                title="1.1. Set the grid",
                tip=span(actionLink(ns("somgridhelp"), tipify_ui(icon("fas fa-question-circle"), "Click for more details")),
                         actionLink(ns("resettopo"), icon("fas fa-undo"),style="position: absolute;right: 20px;top: 4px")),
                div(
                  tags$style(
                    HTML(
                      "

.som_train {
width: 100%;
display: flex;flex-flow: row wrap;gap: 5px
}
.som_train .shiny-input-container{
margin: 0px;
padding: 0px;
width: fit-content
}
.som_train .bootstrap-select:not(.input-group-btn){
display: block;
min-width:120px
}
.som_train input{
max-width: 100px;

}
"
                    )
                  ),
                  div(checkboxInput(ns("sugtopo"), span('suggested topology',tipify_ui(actionLink(ns("sugtopohelp"), icon("fas fa-question-circle")), "Click for more details")), value =T)),
                  div(
                    id = ns("topocontrol"),
                    div(
                      style=" ;",class="inline_pickers2 som_grid",
                      numericInput(ns("xdim"),span("xdim",tiphelp6("Number of neurons along the x-axis")),value =5,min = 0,step = 1,width="70px"),
                      numericInput(ns("ydim"),span("ydim",tiphelp("Number of neurons along the y-axis")),
                                   value = 5,min = 0,step = 1,width="70px"),
                      pickerInput_fromtop(ns("topo"),span("Topology",tiphelp("<p>Arrangement of neurons within the grid.<p><code>Hexagonal</code>: each neuron has six neighboring neurons</p><p><code>Rectangular</code>: each neuron has four direct neighbors</p></p>")),
                                          choices = c("hexagonal", "rectangular")),
                      pickerInput_fromtop(ns("neighbourhood.fct"),
                                          label =span("neigh.fct",
                                                      tiphelp("<p>Specifies the neighborhood function, determining how the influence of the best-matching unit (BMU) decays with distance during training:</p><p><code>gaussian</code> - Influence decreases with a Gaussian (bell curve) distribution based on radius, allowing smoother transitions.</p><p><code>bubble</code> - Influence decreases uniformly within a fixed-radius \\'bubble\\' around the BMU, with an abrupt cutoff beyond the radius.</p>")

                                          ) ,
                                          choices = c("gaussian","bubble")),
                      pickerInput_fromtop(ns("toroidal"),
                                          label = span("toroidal",tiphelp("<p><code>toroidal</code>: When set to <code>TRUE</code>, the SOM grid wraps around like a torus, where opposite edges are connected. This allows neurons on the edges to have neighbors on the opposite side, creating a continuous surface without borders. Useful for circular or periodic data structures.</p>")
                                          ),
                                          choices = c(F, T))
                    ),
                    div(
                      style="text-align: center; width: 350px",
                      uiOutput(ns("showgrid"))
                    )

                  )



                )

              )

            ),
            column(6,class="mp0",
                   box_caret(
                     ns("box_setup5"),
                     color="#c3cc74ff",
                     inline=F,
                     title="1.2. Set the training parameters",
                     tip=span(actionLink(ns("supersomhelp"), tipify_ui(
                       icon("fas fa-question-circle"), "Click for more details"
                     )),actionLink(ns("resetsom"), icon("fas fa-undo"),style="position: absolute;right: 20px;top: 4px")),
                     div(
                       div(class="inline_pickers2 som_train",
                           pickerInput_fromtop(ns("distmethod"),div(style="",strong("dist.fcts",tipify_ui(icon("fas fa-question-circle"),"Distance measure between each neuron and input data"))),
                                               choices = c("BrayCurtis","euclidean","sumofsquares","manhattan","tanimoto")),
                           pickerInput_fromtop(ns("normalizeDataLayers"),"normalizeLayers",
                                               choices = c("TRUE","FALSE")),
                           numericInput(ns("rlen"),strong("rlen",tipify_ui(icon("fas fa-question-circle"),"The number of times the complete dataset will be presented to the network")),value =500,min = 1,step = 1),
                           numericInput(ns("seed"), strong("seed",tipify_ui(icon("fas fa-question-circle"),"A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the som analysis.")), value =NA, min=0, step=1)
                       ),
                       div(align = "center",
                           br(),
                           actionButton(
                             ns("trainSOM"),
                             h4(icon("fas fa-braille"),"train SOM",icon("fas fa-arrow-circle-right")), style = "background: #05668D; color: white")
                       ),

                       div(
                         span(class="finesom_btn",
                              tipify_ui(actionLink(ns("finesom"),"Fine tuning*"),"show all parameters available")
                         )),
                       div(id=ns("finetuning_som"),
                           div(id="finesom_out",class="map_control_style2",


                               div(style="display: flex",
                                   numericInput(ns("a1"),
                                                label =span(tiphelp("Learning rate: two numbers indicating the amount of change. Not used for the batch algorithm.","left"), "Alpha:"),value = 0.05,step = 0.01),
                                   numericInput(ns("a2"),label = NULL,value = 0.01,step = 0.01)
                               ),

                               div(style="display: flex",
                                   numericInput(ns("r1"),
                                                label = span(tiphelp("the start and stop of the radius of the neighbourhood.  the radius will change linearly; as soon as the neighbourhood gets smaller than one only the winning unit will be updated.","left"),"Radius:"),value = 0),
                                   numericInput(ns("r2"),
                                                label = NULL,value = 0,step = 0.01 )
                               ),
                               pickerInput_fromtop(ns("mode"), span(tiphelp("type of learning algorithm","left"),"mode"), choices = c("online","batch", "pbatch")),
                               numericInput(ns("maxna"),span("maxNA.fraction", tiphelp("the maximal fraction of values that may be NA to prevent the row to be removed. Not applicable for BrayCurtis.","right")),value = 0.001,step = 0.01)
                           )

                       )
                     )

                   )
            ),
            shinyBS::bsTooltip(ns('resettopo'),"Reset parameters"),
            shinyBS::bsTooltip(ns('resetsom'),"Reset parameters")

          ))

      ),
      tabPanel(
        value = "som_tab2",
        strong("2. Results"),
        div(table_results_som$ui(ns("som-results")),
            uiOutput(ns('tab_result_out')))

      ),
      tabPanel(
        value = "som_tab3",style="background: white",
        strong("3. Predict"),
        div(table_predict_som$ui(ns("som-predict")),
            uiOutput(ns('tab_predict_out')))
      )

    ))

  )

}
#' @export

imesc_supersom$server<-function (id,vals ){



  moduleServer(id,function(input, output, session){
    box_caret_server("box_setup1")
    box_caret_server("box_setup2")
    box_caret_server("box_setup3")

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
    output$tab_result_out<-renderUI({
      table_results_som$server("som-results",vals)
      NULL
    })
    output$tab_predict_out<-renderUI({
      table_predict_som$server("som-predict",vals)
      NULL
    })


    observeEvent(vals$cur_som_tab,{
      updateTabsetPanel(session,"som_tab",selected=vals$cur_som_tab)
    })

    ns<-session$ns



    output$data_som_out<-renderUI({
      pickerInput_fromtop(ns("data_som"),uiOutput(ns('label_data_som')),choices =names(vals$saved_data), selected=vals$cur_data, options=shinyWidgets::pickerOptions(liveSearch =T))
    })
    output$data_somY_out<-renderUI({
      div(
        pickerInput_fromtop(ns("data_somY"),"Datalist:",choices = get_match_datalists(),selected=vals$cur_data_somY),
        uiOutput(ns("partition_column"))
      )
    })

    observeEvent(input$data_somY,{
      vals$cur_data_somY<-input$data_somY
    })
    get_match_datalists<-reactive({
      data<-get_datalist_for_som()

      truedatalists<-sapply(vals$saved_data,function(x){
        all(rownames(x)%in%rownames(data))
      })
      names(vals$saved_data)[truedatalists]
    })
    get_datalist_for_som<-reactive({
      if(isTRUE(input$mysupersom)){
        get_training_list()[[1]]
      } else{
        req(input$data_som)
        vals$saved_data[[input$data_som]]
      }
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
    output$partition_column<-renderUI({
      req(input$data_somY)
      choices=c(colnames(attr(vals$saved_data[[input$data_somY]],"factors")))
      selected=vals$cur_partition_column
      selected<-get_selected_from_choices(selected,choices)
      div(
        pickerInput_fromtop(ns("partition_column"),span("Partition:",tiphelp("choose a factor as reference for the partition")), choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T)),
        uiOutput(ns("partition_test"))

      )
    })
    output$partition_test<-renderUI({
      req(input$data_somY)
      req(input$partition_column)
      fac<-attr(vals$saved_data[[input$data_somY]],"factors")[,input$partition_column]
      choices<-levels(fac)
      selected=vals$cur_partition_ref
      selected<-get_selected_from_choices(selected,choices)
      pickerInput_fromtop(ns("partition_ref"),span("Test reference:",tiphelp("choose the level as reference for the test data. Data referring to the level of the chosen factor will not be considered in the training, and can be be later used to generate predictions")), choices=choices,selected=selected)
    })



    observeEvent(current_som_model(),{

      m<-current_som_model()

      attr(m,"model_name")<-input$som_models
      vals$cur_kohonen<-m
    })

    modelplay<-reactiveVal()
    output$play_out<-renderUI({
      req(modelplay())


      div(
        div(sliderInput(session$ns("animation"), "Animation:",
                        min = 1, max = length(modelplay()),
                        value = 1, step = 1,
                        animate =
                          animationOptions(interval = input$play_interval,
                                           playButton=tags$div(
                                             tags$span("Play >> ",style="font-size: 14px;font-weight: bold; background:  #05668D;color: white; padding: 3px;"),style=" margin-top: 5px"
                                           ),
                                           loop = input$loop))),
        uiOutput(ns("plot_animation"))
      )
    })
    output$plot_animation<-renderUI({
      renderPlot({
        plotnetwork_list2(modelplay()[[input$animation]])

      })
    })
    # Define the scatter_hexagon function


    ns<-session$ns


    output$label_data_som<-renderUI({
      get_label_datasom()
    })
    get_label_datasom<-reactive({
      label<-"~ Training Datalist:"
      if(isTRUE(input$mysupersom)&input$som_tab!="som_tab1"){
        label<-span("Target Datalist",tiphelp("the supersom model will always be saved in the datalist selected in the first layer.",placement ="right"))
      }
      label
    })
    somval<-reactiveValues(df=F)
    output$supersom_layers<-renderUI({
      div(class=show_dataX(),
          div("~ Training layers"),
          column(12,uiOutput(ns('ssom_table')))
      )
    })
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
                 tipify_ui(actionLink(ns("ssom_add"),icon("fas fa-plus")),"Add layer","right"))

      )
    })
    output$som_cur<-renderUI({
      req(input$som_tab!="som_tab1")
      div(
        div(strong("X:"), em(paste0(names(current_som_model()$data),collapse = "; "))),
        div(strong("Method:"),em(attr(current_som_model(),"Method"))),
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
    getinit_supersom<-reactive({
      name1<-names(vals$saved_data)[1]
      choices_datalist<-unlist(lapply(vals$saved_data[-1],function(x){
        sum( rownames(x)%in%rownames(vals$saved_data[[name1]]))==nrow(x)
      }))
      req(is.logical(choices_datalist))
      name2<-names(choices_datalist)[which(choices_datalist)][1]
      ssom_input<-data.frame(Datalist=c(name1,name2),
                             Weights= 0.5,
                             Distances="euclidean")
      rownames(ssom_input)<-paste0("Layer",1:nrow(ssom_input))
      ssom_input

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
                 p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("Chull correlations")," returns",code("npic")," variables with the highest correlation considering the convex hull, while also ensuring that the points are ordered by their proximity to codebook center")
          )

        )
      )

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
    savereac<-reactive({



      tosave<-isolate(reactiveValuesToList(vals))
      tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
      tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
      tosave$saved_data<-vals$saved_data
      tosave$newcolhabs<-vals$newcolhabs
      tosave$colors_img<-vals$colors_img
      tosave$ssom_tab<-vals$ssom_tab
      tosave$ssom_tab0<-vals$ssom_tab0
      tosave$som_results<-current_som_model()
      tosave$SOM_MODEL<-vals$SOM_MODEL
      saveRDS(tosave,"savepoint.rds")
      saveRDS(reactiveValuesToList(input),"input.rds")
      beepr::beep()


    })
    getobs_somX<-reactive({
      datalist<-vals$saved_data
      m<-current_som_model()
      colnames_list<-lapply(datalist,colnames)
      colnames_som<-lapply(m$data,colnames)
      names(colnames_som)<-names(m$data)
      do.call(c,lapply(colnames_som,function(j){
        names(which(sapply(colnames_list,function(i){
          identical(sort(i), sort(j))
        })))
      }))

    })
    getdata_som<-reactive({
      req(input$data_som)
      data_o<-data<-vals$saved_data[[input$data_som]]

      data
    })
    data_overwritte<-reactiveValues(df=F)
    data_store<-reactiveValues(df=F)
    newname<-reactiveValues(df=0)
    get_newname<-reactive({
      req(!is.null(vals$hand_save))
      newname$df<-bag_somname()})


    module_som<-function() {
      ns<-session$ns

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
      res<-pickerInput_fromtop(ns("over_datalist"), NULL,choices, width="350px")
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



    getsom<-reactive({
      req(input$som_models)
      req(length(attr(getdata_som(),"som"))>0)
      m<-attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]
      req(inherits(m,"kohonen"))
      m
    })
    output$saved_som_print<-renderUI({
      req(input$som_tab!="som_tab1")
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
    bag_somname<-reactive({

      m<-current_som_model()
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


    som_model_names<-reactive({
      req(input$data_som)
      names(attr(vals$saved_data[[input$data_som]],"som"))
    })
    savesom<-reactive({
      curtab<-vals$cursomtab
      data<-getdata_som()
      temp<-current_som_model()
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

      vals$cur_som_models<-cur
      delay(500,{updateTabsetPanel(session, "som_tab", curtab)
        updateTabsetPanel(session, "som_res", vals$som_res)})

      attr(vals$saved_data[[input$data_som]],"som")[['new som (unsaved)']]<-NULL

    })
    observeEvent(input$som_model_delete,ignoreInit = T,{
      attr(vals$saved_data[[input$data_som]],"som")[[input$som_models]]<-NULL

    })


    current_som_model<-reactive({
      res<-getsom()
      req(inherits(res,"kohonen"))
      if(length(res$data)==1){
        if(names(res$data)=="X"){
          names(res$data)<-attr(res,"Datalist")
        }
      }
      res
    })

    observeEvent(ignoreInit = T,input$som_res,{
      vals$som_res<-input$som_res
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
    observeEvent(ignoreInit = T,input$somgridh,{
      output$somgridhelp<-renderText({
        paste0(br(),
               h4("somgrid {kohonen}"),
               getHelp('unit.distances'))
      })
    })
    observeEvent(ignoreInit = T,input$partition_column,{
      vals$cur_partition_column<-input$partition_column
    })
    observeEvent(ignoreInit = T,input$partition_ref,{
      vals$cur_partition_ref<-input$partition_ref
    })

    observeEvent(ignoreInit = T,input$layer_result,{
      vals$cur_layer_result<-input$layer_result
    })




    observeEvent(ignoreInit = T,input$supersom_reset,{
      if(input$supersom_reset%%2){

        vals$cur_somdataXlist_supersom<- NULL
        vals$cur_ssom_wei<-rep("0.5",2)
        vals$cur_ssom_dist<-rep("euclidean",2)
        vals$ssom_tab0<-getinit_supersom()

      }
    })
    observeEvent(ignoreInit = T,input$ssom_add, {
      res<-rbind(vals$ssom_tab0, vals$ssom_tab0[nrow(vals$ssom_tab0),])
      #rownames(res)<-paste0("Layer",1:nrow(res))
      vals$cur_ssom_wei<-c(vals$cur_ssom_wei,vals$cur_ssom_wei[length(vals$cur_ssom_wei)])
      vals$cur_ssom_dist<-c(vals$cur_ssom_dist,vals$cur_ssom_dist[length(vals$cur_ssom_dist)])
      vals$cur_somdataXlist_supersom<-c(vals$cur_somdataXlist_supersom,vals$cur_somdataXlist_supersom[length(vals$cur_somdataXlist_supersom)])

      vals$ssom_tab0<-res
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
    observeEvent(ignoreInit = T,input$ssom_minus,{
      vals$ssom_tab0<-vals$ssom_tab0[-nrow(vals$ssom_tab0),]
    })
    observeEvent(list(input$som_part,input$data_som,input$mysupersom),{
      req(input$som_tab)
      req(length(input$mysupersom)>0)
      req(input$data_som)
      req(input$som_part)
      updateTabsetPanel(session,"som_tab","som_tab1")
    })
    observeEvent(ignoreInit = T,input$som_tab,{
      vals$cur_somtab<-input$som_tab

    })
    output$som_models_panel<-renderUI({
      box_caret(ns('box_setup2'),
                title="Model Results",
                div(
                  div(
                    style="display: flex",

                    selectInput(ns("som_models"),
                                span(strong("Results:", tiphelp("SOM results. Click to see SOM results saved in the selected Datalist"))),
                                choices=som_model_names(),
                                selected=vals$cur_som_models),
                    div(style="font-size: 16px; margin-left: -10px; margin-top: 5px",tipify_ui(actionLink(ns("som_model_delete"),icon("fas fa-trash-alt")),"Remove model"))

                  ),
                  uiOutput(ns("som_cur"))
                ))
    })

    observeEvent(input$som_models,{
      vals$cur_som_models<-input$som_models
    })
    observeEvent(modelplay(),{
      shinyjs::toggle(selector=".anim_opt",condition=length(modelplay())>0)
    })
    observeEvent( input$data_confirm,{
      req(!is.null(vals$hand_save))
      savesom()
      removeModal()

    })
    observeEvent(ignoreInit = T,input$data_som,{
      vals$cur_data<-vals$cur_somdataX<-input$data_som
    })
    observeEvent(input$ssom_layer1,{
      vals$cur_data<-input$ssom_layer1
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
                  inline(div(tipify_ui(actionLink(ns("ssom_minus"),icon("fas fa-minus"),style="margin-right: 20px;"),"Remove layer","top")))


                },strong(i)
              ),
              div(class="ensemble_labels",
                  pickerInput_fromtop(ns(paste0("ssom_layer", i)), "", choices = if(i!=1){choices_datalist}else{names(vals$saved_data)},selected=vals$cur_somdataXlist_supersom[i], options=list(container="body"))
              )
              ,
              div(class="layers_wei",

                  numericInput(ns(paste0("ssom_wei", i)), "",
                               value = vals$cur_ssom_wei[i])
              ),
              div(class="layers_dist",

                  pickerInput_fromtop(ns(paste0("ssom_dist", i)), "", choices = c("BrayCurtis","euclidean","sumofsquares" ,"manhattan", "tanimoto"),selected=vals$cur_ssom_dist[i], options=list(container="body")))


          )
        })
        vals$ssom_tab<-temp

      }})
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

    observe({
      condition=input$som_models%in%"new som (unsaved)"&input$som_tab!="som_tab1"
      shinyjs::toggle('tools_savesom', condition=condition)
    })
    observeEvent(input$som_tab,{
      shinyjs::toggle('partition_panel',condition=input$som_tab=="som_tab1")
      shinyjs::toggle("som_models_panel",condition=input$som_tab!="som_tab1")
    })
    observe({
      req(length(input$mysupersom)>0)
      condition=if(isTRUE(input$mysupersom)&input$som_tab=="som_tab1"){F} else{T}
      shinyjs::toggle('label_data_som',condition=condition)
      shinyjs::toggle('data_som_out',condition=condition)
    })
    observeEvent(input$usepartition,{
      shinyjs::toggle("partition_on",condition=input$usepartition)
    })
    observeEvent(input$save_bug,{





      saveRDS(reactiveValuesToList(vals),"savepoint.rds")
      saveRDS(reactiveValuesToList(input),"input.rds")
      #saveRDS(vals$cur_caret_model,"m.rds")

      beepr::beep(10)
    })



    ## traning
    {


      box_caret_server("box_setup4")
      box_caret_server("box_setup5")

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
      observeEvent(ignoreInit = T,input$resettopo, {
        shinyjs::reset("topocontrol")
        shinyjs::reset("topocosugtopontrol")
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

      cur_data_som<-reactive({
        train_data<-try({
          traindat<-if(isTRUE(input$mysupersom)){
            do.call(cbind,get_training_list())
          } else{
            req(input$data_som)
            vals$saved_data[[input$data_som]]
          }
          traindat=traindat[get_partition()$train,,drop=F]
          traindat
        },silent = T)
        req(!inherits(train_data,"try-error"))
        traindat
      })

      observe({


        data<-cur_data_som()

        #req(get_partition()$train%in%rownames(data))

        data<-data[get_partition()$train,,drop=F]

        req(input$data_som%in%names(vals$saved_data))
        req(data)
        df=data.frame(na.omit(data))
        N<-nrow(data)
        SIZE<-sqrt(5*sqrt(N))
        L<- floor(SIZE)
        res<-if(!nrow(df)>0){
          c(L*L,L,L)
        } else{
          topology(data, dist = input$distmethod)
        }
        topo.reactive(res)
        if(isTRUE(input$sugtopo)){
          dim<-as.numeric(res[c(2,3)])

          updateNumericInput(session,"xdim",value= dim[1])
          updateNumericInput(session,"ydim",value= dim[2])
        }

      })
      observeEvent(list(input$xdim, input$ydim), {
        req(input$xdim)
        req(input$ydim)
        if (length( names(vals$saved_data)) > 0) {
          dim = try(topo.reactive(),silent=T )
          req(!inherits(dim,"try-error"))
          ydim<-dim[[3]]
          xdim<-dim[[2]]
          req(xdim)
          req(ydim)
          if (input$xdim != xdim|input$ydim != ydim) {
            updateCheckboxInput(session, "sugtopo", NULL, FALSE)
          } else{
            updateCheckboxInput(session, "sugtopo", NULL, TRUE)
          }

        }
      })
      observe({
        data=cur_data_som()
        condition=(input$xdim*input$ydim)<=nrow(data)
        shinyjs::toggle('trainSOM',condition=condition)
      })
      output$showgrid<-renderUI({
        dim = try(topo.reactive(),silent=T )
        if(inherits(dim,"try-error")){updateCheckboxInput(session,"sugtopo",value=F)}
        validate(need(!inherits(dim,"try-error"),"Error: suggested topology has been disabled; check for inconsistency in data, such as columns with variance 0. "))

        renderPlot({
          data = getdata_som()
          validate(need(input$xdim!="", ""))
          validate(need(input$ydim!="", ""))
          validate(need( (input$xdim*input$ydim)<=nrow(data), "The number of map units must be less than or equal to the number of observations. Please decrease the 'xdim' and/or 'ydim' dimensions"))
          try({
            par(mar = c(0, 0, 0, 0))

            grid<-kohonen::somgrid(input$xdim, input$ydim, topo = input$topo, neighbourhood.fct=input$neighbourhood.fct, toroidal=toroidal())
            if(isFALSE(grid$toroidal)){
              plot.som_grid(grid)
            } else{
              m<-list()
              m$grid<-grid
              plot_torus(m,3,1)

            }


          } )
        },
        width = 200,
        height = 200)
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


      observeEvent(input$trainSOM,ignoreInit = T,{

        req(length(vals$saved_data)>0)

        req(isTRUE(input$mysupersom))
        data_x_o<-names(get_training_list())[[1]]
        attr(vals$saved_data[[data_x_o]],"som")[["new som (unsaved)"]]<-NULL



        if(is.null(attr(vals$saved_data[[data_x_o]],"som"))){
          attr(vals$saved_data[[data_x_o]],"som")<-list()
        }


        layers = get_training_list()
        layer_table<-ssom_reac()
        weights<-layer_table$Weights
        distances<-layer_table$Distances
        data1<-vals$saved_data[layer_table$Datalist][[1]]
        if(isTRUE(input$usepartition)){
          #req(input$data_somY)
          factors<-attr(vals$saved_data[[input$data_somY]],"factors")[rownames(data1),,drop=F]
          pic_split<-which(factors[,input$partition_column]%in%input$partition_ref)
          test_ids<-rownames(factors)[pic_split]
          train_ids<-rownames(factors)[-pic_split]
          parts=list(train=train_ids,test=test_ids)
          train<-parts$train
          test<-parts$test
          training_list<-lapply(layers,function(x){
            x[train,,drop=F]
          })
          test_list<-lapply(layers,function(x){
            x[test,,drop=F]
          })
        } else{
          training_list<-layers
          test_list<-"None"
        }

        ncodes<-input$xdim*input$ydim
        seed<-input$seed
        seed<-input$seed
        if(is.na(seed)){
          seed<-sample(.Random.seed,1)}



        ncodes<-input$xdim*input$ydim
        set.seed(seed)
        starters<-sample(1:nrow(training_list[[1]]), ncodes, replace = FALSE)
        init<-lapply(training_list, function(x) x[starters, , drop = FALSE])

        withProgress(
          message = "Running som... the time taken will depend on the size of the data and the training.",
          min = 1,
          max = 1,
          {
            if(is.na(input$seed)==F){set.seed(input$seed)}
            args<-list(
              data=training_list,
              #whatmap = 1,
              grid = kohonen::somgrid(
                input$xdim,
                input$ydim,
                topo = input$topo,
                toroidal = toroidal(),
                neighbourhood.fct=input$neighbourhood.fct
              ),
              rlen = input$rlen,
              dist.fcts = distances,
              user.weights=weights,
              alpha = c(input$a1, input$a2),
              radius = c(input$r1, input$r2),
              mode = input$mode,
              maxNA.fraction = input$maxna,
              normalizeDataLayers=as.logical(input$normalizeDataLayers),
              init=init
            )

            m<-do.call(supersom,args)
            attr(m,'mode')<-m$mode<-input$mode
            m$seed<-seed
            m$normalizeDataLayers<-input$normalizeDataLayers
            m$init<-init
            names(m$unit.classif)<-rownames(m$data[[1]])
            attr(m,"Method")<-"superSOM"
            attr(m,"Datalist")<-names(m$data)
            attr(m,"test")<-test_list
            attr(m,"normalizeDataLayers")<-input$normalizeDataLayers

            newmodesl<-c(list(m),attr(vals$saved_data[[data_x_o]],"som"))
            names(newmodesl)[1]<-"new som (unsaved)"
            attr(vals$saved_data[[data_x_o]],"som")<-newmodesl

            updateTabsetPanel(session, "som_tab", "som_tab2")
            updateTabsetPanel(session, "som_tab", "train_tab2")
            updateTabsetPanel(session, "som_res", "train_tab1")
            vals$cur_som_models<-"new som (unsaved)"
            vals$cur_data<-data_x_o
          }
        )


      })

      observeEvent(input$trainSOM,ignoreInit = T,{

        req(length(vals$saved_data)>0)
        req(isFALSE(input$mysupersom))
        vals$som_unsaved<-NULL
        req(input$data_som)
        attr(vals$saved_data[[input$data_som]],"som")[["new som (unsaved)"]]<-NULL


        traindat=data=data_o<-data.frame(vals$saved_data[[input$data_som]])
        traindat=data[get_partition()$train,,drop=F]


        withProgress(
          message = "Running som... the time taken will depend on the size of the data and the training.",
          min = 1,
          max = 1,



          {
            datalist<-list(as.matrix(traindat))
            names(datalist)<-input$data_som
            seed<-input$seed
            if(is.na(seed)){seed<-sample(.Random.seed,1)}



            ncodes<-input$xdim*input$ydim
            set.seed(seed)
            starters<-sample(1:nrow(datalist[[1]]), ncodes, replace = FALSE)
            init<-lapply(datalist, function(x) x[starters, , drop = FALSE])
            set.seed(seed)
            m<-try(
              supersom(
                datalist,
                grid = kohonen::somgrid(
                  input$xdim,
                  input$ydim,
                  topo = input$topo,
                  toroidal = toroidal(),
                  neighbourhood.fct=input$neighbourhood.fct
                ),
                rlen = input$rlen,
                dist.fcts = input$distmethod,
                alpha = c(input$a1, input$a2),
                radius = c(input$r1, input$r2),
                mode = input$mode,
                maxNA.fraction = input$maxna,
                init=init,
                normalizeDataLayers=as.logical(input$normalizeDataLayers)
              )
            )

            if (!inherits(m,"kohonen"))        {
              validate(paste(m[[1]], "Please decrease your alpha (learning rate)"))
            }
            attr(m,'mode')<-m$mode<-input$mode
            m$seed<-seed
            m$normalizeDataLayers<-input$normalizeDataLayers
            m$init<-init
            names(m$unit.classif)<-rownames(m$data[[1]])
            attr(m,"test_partition")<-"None"
            test_list<-list(as.matrix(data[get_partition()$test,,drop=F]))
            names(test_list)<-input$data_som
            attr(m,"test")<-test_list
            attr(m,"Method")<-"Unsupervised"
            attr(m,"Datalist")<-input$data_som
            attr(m,"normalizeDataLayers")<-input$normalizeDataLayers
            attr(m,"coords")<-attr(data,"coords")[rownames(traindat),]
            vals$som_unsaved<-m
            newmodesl<-c(list(m),attr(vals$saved_data[[input$data_som]],"som"))
            names(newmodesl)[1]<-"new som (unsaved)"
            attr(vals$saved_data[[input$data_som]],"som")<-newmodesl





            updateTabsetPanel(session, "som_tab", "som_tab2")
            updateTabsetPanel(session, "som_tab", "train_tab2")
            updateTabsetPanel(session, "som_res", "train_tab1")
            vals$cur_som_models<-"new som (unsaved)"
          }
        )


      })
      observeEvent(list(input$xdim,input$ydim),{
        req(input$xdim)
        req(input$ydim)
        value=as.vector(
          quantile(
            unit.distances(
              kohonen::somgrid(input$xdim,input$ydim,topo = input$topo,toroidal = toroidal(), neighbourhood.fct=input$neighbourhood.fct)), 2 / 3
          )
        )
        updateNumericInput(session,"r1",value=value)
        fineonce<-reactiveVal(F)
        if(isFALSE(fineonce())){
          shinyjs::hide("finetuning_som")
          fineonce(T)
        }


      })
      observeEvent(input$run_play,ignoreInit = T,{
        data<-data_example()
        datalist<-list(as.matrix(data))
        names(datalist)<-input$data_som
        if(is.na(input$seed)==F){set.seed(input$seed)}
        seed<-input$seed
        if(is.na(seed)){
          seed<-1
        }
        message(paste0('seed',seed))

        ms<-list()
        set.seed(seed)
        set.seed(seed)
        ncodes<-input$xdim*input$ydim
        starters<-sample(1:nrow(datalist[[1]]), ncodes, replace = FALSE)
        init<-lapply(datalist, function(x) x[starters, , drop = FALSE])

        seq<-round(seq(1,input$rlen_play,length.out=input$rlen_snap))
        withProgress(min=1,max=length(seq),message="Running...",{
          for( i in seq) {

            set.seed(seed)
            m<-supersom(
              datalist,
              grid = kohonen::somgrid(
                input$xdim,
                input$ydim,
                topo = input$topo,
                toroidal = toroidal(),
                neighbourhood.fct=input$neighbourhood.fct
              ),
              rlen = i,
              init=init,
              dist.fcts = input$distmethod,
              alpha = c(input$a1, input$a2),
              radius = c(input$r1, input$r2),
              mode = input$mode,
              maxNA.fraction = input$maxna,
              normalizeDataLayers=as.logical(input$normalizeDataLayers)
            )
            ms[[length(ms)+1]]<-m
            incProgress(1)
          }
        })

        modelplay(ms)

      })


      observeEvent(ignoreInit = T,input$data_som,{
        if(anyNA(getdata_som())){
          updateSelectInput(session,"distmethod",selected="euclidean")
        }
      })
      observeEvent(input$mysupersom,{
        shinyjs::toggle('distmethod',condition=isFALSE(input$mysupersom))
      })

      output$train_som_button<-renderUI({
        if(input$distmethod=="BrayCurtis"){
          validate(need(anyNA(getdata_som())==F, "Missing values are not allowed in the Bray method. Change the distance or use the preprocessing tools to impute or remove the missing values"))
        }


        if(isTRUE(input$mysupersom)){
          validate_supersom()
          lab<-h4(icon("fas fa-braille"),"train superSOM",icon("fas fa-arrow-circle-right"))
        }


      })

      observeEvent(input$finesom,{
        shinyjs::toggle('finetuning_som')
      })

      topo.reactive<-reactiveVal()
      toroidal<-reactive({
        switch (input$toroidal,
                'TRUE' = TRUE,
                "FALSE" = FALSE)
      })
      get_partition<-reactive({
        if(isFALSE(input$usepartition)){
          data<-getdata_som()
          return(list(train=rownames(data),test=NULL))
        }

        req(input$data_somY)
        data<-vals$saved_data[[input$data_somY]]


        factors<-attr(data,"factors")
        req(input$partition_column%in%colnames(factors))
        partition_column<-factors[input$partition_column]
        test_ids<-which(partition_column[,1]%in%input$partition_ref)
        train_ids<-rownames(data)[-test_ids]
        test_ids<-rownames(data)[test_ids]
        return(list(train=train_ids,test=test_ids))
      })
      data_example<-reactive({
        switch(input$data_example,
               "nema_araca"={
                 as.matrix(data.frame(data.table::fread("inst/www/nema_araca.csv"))[-1])
               },
               "nema_hellinguer"={
                 data<-data.frame(data.table::fread("inst/www/nema_araca.csv"))[-1]
                 as.matrix(vegan::decostand(data,"hell"))
               },
               "envi_araca"={

                 as.matrix(data.frame(data.table::fread("inst/www/envi_araca.csv"))[-1])

               },
               "envi_araca_scaled"={

                 data<-data.frame(data.table::fread("inst/www/nema_araca.csv"))[-1]
                 scale(data)

               },
               "user-defined"=as.matrix(getdata_som())

        )
      })
      validate_supersom<-reactive({
        layers = get_training_list()
        ids<-names(layers)
        dists<-ssom_reac()
        dists$id<-rownames(dists)
        dd<-sapply(layers,function(x) {any(rowSums(x)==0)})
        dists$notval<-dd
        if(any(dists$Distances=="BrayCurtis")){
          notval<-which(apply(dists,1,function(x) x[3]=='BrayCurtis'&x[5]==T))

          if(length(notval)>0){
            paste(
              "Error: Empty rows detected. SOM cannot be trained using the 'Bray' method for the layers",ifelse(length(notval) == 1, notval,
                                                                                                                ifelse(length(notval) == 2, paste0(notval, collapse = " and "),
                                                                                                                       paste0(paste0(notval[1:(length(notval)-1)], collapse = ", "), " and ", notval[length(notval)])))
            )
          } else{ NULL}



        }

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



    }


  })



}
