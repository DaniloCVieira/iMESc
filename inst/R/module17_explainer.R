box35_help_content<-"Plot two measures of importance of variables in a random forest"
explainer_ggpair<-list()
explainer_ggpair$ui<-function(id,title=NULL,tip=NULL){
  ns<-NS(id)
  div(
    column(4,class="mp0",
           box_caret(ns("36_a"),
                     title="Display Options",

                     div(
                       div(class="normal-checkbox",
                           div(id=ns('run_between_btn'),class="save_changes",style="position: absolute;right: 0px;top: 30px",

                               actionButton(ns('run_between'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                           checkboxGroupInput(ns('box36_measures'),span(tipright(" the measures of importance to be used"),'measures:'),NULL)
                       )
                     )
           ),
           box_caret(ns("36_b"),
                     title="Plot options",

                     div(
                       textInput(ns('box36_title'),"Title:",NULL),
                       numericInput(ns('box36_base_size'),
                                    "Base size:",12),
                       numericInput(ns('box36_axis.text.size'),
                                    "Axis size:",1),
                       numericInput(ns('box36_axis.title.size'),
                                    "Axis label size:",1)
                     )

           )
    ),
    column(8,class="mp0",
           box_caret(ns("36_c"),
                     title=title,
                     tip=tipright(tip),
                     button_title=actionLink(ns('down_plot_36'),"Download",icon("download")),
                     div(
                       uiOutput(ns("box_36"))
                     )
           )

    )
  )
}
explainer_ggpair$server<-function(id,data,fun="plot_importance_ggpairs",vals){
  moduleServer(id,function(input,output,session){


    ns<-session$ns
    observeEvent(data,{
      choices=colnames(data)[-1]
      choices=choices[-length(choices)]
      updateCheckboxGroupInput(session,"box36_measures",choices=choices,selected=choices)
    })
    output$box_36<-renderUI({
      div(
        #renderPrint(class(run_box36_plot())),
        renderPlot({
          run_box36_plot()

        })
      )
    })
    observeEvent(input$down_plot_36,ignoreInit = T,{
      graphics.off()
      vals$hand_plot<-"generic_ggmatrix"
      module_ui_figs("downfigs")
      generic=run_box36_plot()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Importance Measures - GGpairs", name_c="measures-ggpairs")
    })

    observeEvent(
      list(
        data,
        input$box36_measures,
        method=input$box36_method,
        points.size=input$box36_base_size,
        axis.text.size=input$box36_axis.text.size,
        axis.title.size=input$box36_axis.title.size
      ),{
        shinyjs::addClass("run_between_btn","save_changes")
      }
    )

    run_box36_plot<-eventReactive(input$run_between,ignoreInit = T,{
      x<-data[input$box36_measures]
      method="pearson"
      p<-do.call(fun,list(data,input$box36_measures))
      p<-p+
        ggtitle(input$box36_title)+
        theme(axis.text=element_text(size=input$box36_axis.text.size),

              axis.title=element_text(size=input$box36_axis.title.size)
        )+theme_bw(base_size=input$box36_base_size)

      shinyjs::removeClass("run_between_btn","save_changes")


      p
      #randomForestExplainer::plot_importance_ggpairs( get_measure_table(),measures=input$box36_measures,main=input$box36_title)

    })
  })
}
box30_help_content<-"Creates a data frame with various measures of importance of variables in a random forest"


rf_explainer<-list()
rf_explainer$ui<-function(id){
  ns<-NS(id)
  div(
    div(
      class="rfexpainer nav_caret",
      tabsetPanel(
        id=ns('rftab2_3'),
        tabPanel("1. Measures",value="tab1",
                 div(
                   column(4,class="mp0",
                          box_caret(ns('30'),click=F,title="Analysis options",
                                    div(
                                      selectInput(ns('rfmeasure_mean_sample'),span(tipright("The sample of trees on which conditional mean minimal depth is calculated"),'mean_sample'),choices=c("all_trees", "top_trees", "relevant_trees"),selected='top_trees'),
                                      div(class="normal-checkbox",
                                          checkboxGroupInput(ns('rf_measures'),span(tipright(" the measures of importance to be used"),'measures:'),choices=c("mean_min_depth",
                                                                                                                                                              "accuracy_decrease","gini_decrease" ,"no_of_nodes","times_a_root"),selected=c("mean_min_depth","accuracy_decrease","gini_decrease" ,"no_of_nodes","times_a_root"))
                                      ))

                          )),
                   column(8,class="mp0",
                          box_caret(ns("31"),title="Importance Measures",
                                    button_title=actionLink(ns('downcenter_rfdepth'),"Download",icon("download")),

                                    div(
                                      div(
                                        actionLink(ns('run_measures'),span("Click to Measure Importance ",icon("fas fa-arrow-circle-right")), style = "animation: glowing3 1000ms infinite;")
                                      ),
                                      uiOutput(ns("rf_measure_out"))
                                    )

                          )
                   )
                 )


        ),
        tabPanel("2. Min Depth Distr.",
                 value="tab2",
                 column(4,class="mp0",
                        box_caret(
                          ns("32"),
                          title="Display Options",
                          click=F,
                          div(

                            div(
                              div(class="radio_search radio-btn-green",
                                  radioGroupButtons(ns("rf_depth"), "Display:", choiceNames =c("significant","User-defined","All"),choiceValues =c("sigs","user","all"))
                              ),
                              numericInput(ns("n_var_rf"),span(tipright("The maximal number of variables with lowest mean minimal depth to be used for plotting"),
                                                               'Number of variables:'),value = 10,step = 1),
                              numericInput(ns('depth_min_no_of_trees'),span(tipright("The minimal number of trees in which a variable has to be used for splitting to be used for plotting"),'Min_n_trees:'),value=0,step=1),
                              selectInput(ns('depth_mean_sample'),span(tipright("The sample of trees on which mean minimal depth is calculated"),'Mean_sample:'),choices=c( "top_trees","all_trees","relevant_trees"), selected= "top_trees"),
                              numericInput(ns("sigprf"), span(tipright("Significance level"),'Sig:'), 0.05, step=.05, max=1, min=0)
                            ))
                        ),
                        box_caret(
                          ns("32_2"),
                          title="Plot Options",
                          click=F,
                          div(
                            div(
                              uiOutput(ns("mdd_palette")),
                              numericInput(ns("labrfsize"),"Base size:", value=10),
                              textInput(ns("title_prf"),"Title:", value=NULL),

                              textInput(ns("xlab_prf"),"X label:", value="Variable"),
                              textInput(ns("ylab_prf"),"Y label:", value="Number of Trees"),

                              numericInput(ns('prf_axis_size'),"Axis size", 12),
                              numericInput(ns('prf_label_axis_size'),"Label axis size", 12),

                              numericInput(ns("size_mmd"),"Text-in size:", value=4),
                              div(
                                  numericInput(ns('plot_height'),"Plot height", 300),
                                  numericInput(ns('plot_width'),"Plot width", 400),
                              )

                            ))
                        )

                 ),
                 column(8,class="mp0",
                        box_caret(ns("33"),
                                  title="Minimal depth distribution",
                                  button_title=actionLink(ns('downp_prf'),"Download",icon("download")),
                                  div(
                                    div(
                                      style="position: absolute; top: 27px;right: 0px;padding: 5px;background:white;padding-top:0px",
                                      tipify(
                                        actionLink(ns('create_rf'),span("Create Datalist"),icon("fas fa-file-signature")),
                                        "Create a datalist with the variables selected in the Random Forest Explainer.","right"
                                      )
                                    ),
                                    uiOutput(ns("feature_plot"))
                                  )

                        )
                 )


        ),
        tabPanel("3. Multi-way",
                 value="tab3",
                 column(4,class="mp0",
                        box_caret(ns("34"),
                                  title="Display Options",
                                  div(
                                    div(
                                      class="map_control_style",style="color: #05668D",
                                      div(
                                        class="radio_search radio-btn-green",
                                        radioGroupButtons(ns("rf_sigmulti"), "Display:", choiceNames =c("significant","User-defined","All"),choiceValues =c("sigs","user","all"))
                                      ),
                                      numericInput(ns('multi_no_of_labels'),span(tipright("The approximate number of best variables (according to all measures plotted) to be labeled (more will be labeled in case of ties)"),'No_of_labels'),value=10,step=1),
                                      selectInput(ns('multi_x_measure'),span(tipright("The measure of importance to be shown on the X axis"),'X:'),choices=NULL),
                                      selectInput(ns('multi_y_measure'),span(tipright("The measure of importance to be shown on the Y axis"),'Y:'),choices=NULL),
                                      selectInput(ns('multi_z_measure'),span(tipright("Optional measure for gradient color scale"),'z:'),choices=NULL),
                                    )
                                  )
                        ),
                        box_caret(ns("34_2"),
                                  title="Plot Options",
                                  div(
                                    div(
                                      uiOutput(ns("interframe_palette")),
                                      numericInput(ns('max.overlaps'),span(tipright("Exclude text labels that overlap too many things"),'max.overlaps:'),10),
                                      numericInput(ns("sigmrf"), span(tipright("Significance level"),'Sig:'), 0.05),
                                      textInput(ns("interframe_title"),"Title:", value="Multi-way importance"),
                                      checkboxInput(ns("interframe_logy"),"Log Y axis:", T),
                                      checkboxInput(ns("interframe_logx"),"Log X axis:", F),
                                      numericInput(ns("interframe_size"),"Base size:", value=2),
                                      numericInput(ns("interframe_label_size"),"Label axis size:", value=5),
                                      numericInput(ns("interframe_axis_size"),"Axis size:", value=12)
                                    )
                                  )
                        )
                 ),
                 column(8,class="mp0",
                        box_caret(ns("35"),
                                  title="Multi-way importance",
                                  button_title=actionLink(ns('downp_mrf'),"Download",icon("download")),
                                  plotOutput(ns("rf_multi_out"))))



        ),
        tabPanel("4. Relationships",
                 value="tab4",
                 tabsetPanel(
                   tabPanel("4.1 Between importances",
                            explainer_ggpair$ui(ns('between'),
                                                title="Between importances",
                                                tip="Plot selected importance measures pairwise against each other"),
                            uiOutput(ns("out_between"))


                   ),
                   tabPanel('4.2 Between rankings',
                            explainer_ggpair$ui(ns('rank'),
                                                title="Between rankings",
                                                tip="Plot against each other rankings of variables according to various measures of importance"),
                            uiOutput(ns("out_rank"))


                   )
                 )
        ),
        tabPanel(
          "5. Interactions",
          value="tab5",

          div(
            column(
              4,class="mp0",
              box_caret(
                ns("38"),
                title="Setup",
                color="#c3cc74ff",
                div(
                  div(
                    actionLink(ns('go_rfinter'),span("Click to Run ",icon("fas fa-arrow-circle-right")), style = "animation: glowing3 1000ms infinite;")
                  ),
                  numericInput(ns('rfinter_k'),span(tipright("The number of variables to extract"),'k'),value=5,step=1),
                  selectInput(ns('rfinter_mean_sample'),span(tipright("The sample of trees on which conditional mean minimal depth is calculated"),'mean_sample'),choices=c("all_trees", "top_trees", "relevant_trees"),selected='top_trees'),
                  selectInput(ns('uncond_mean_sample'),span(tipright("The sample of trees on which unconditional mean minimal depth is calculated"),'unc_mean_sample'),choices=c("all_trees", "top_trees", "relevant_trees"),selected='mean_sample'),
                  numericInput(ns('rfinter_k2'),span(tipright("The number of best interactions to plot, if empty then all plotted"),'N inter'),value=30,step=1),
                  div(tipify(actionLink(ns('create_rfinter'),span("Create Datalist",icon("fas fa-file-signature"))),"Create a datalist with the variables included in the N most frequent interactions in the RF","right"))
                )

              ),
              box_caret(
                ns("inter_plotoptions"),
                title="Plot options",
                color="#c3cc74ff",
                div(
                  uiOutput(ns('inter_palette')),

                  textInput(ns("inter_title"),strong("Title:"),NULL),
                    div(style="padding-left: 40px",
                      numericInput(ns("int_cex.title"),'Size',13),
                      pickerInput_fromtop(ns("int_font.title"),'Font:',c("plain", "bold", "italic", "bold.italic")),

                  ),
                  textInput(ns("inter_subtitle"),strong("Title"),NULL),
                  div(style="padding-left: 40px",

                      numericInput(ns("int_cex.subtitle"),'Size:',9),
                      pickerInput_fromtop(ns("int_font.subtitle"),'Font:',c("plain", "bold", "italic", "bold.italic"),selected="italic")
                  ),

                  numericInput(ns("int_cex.axes"),'Axis size',11),
                  numericInput(ns("int_cex.lab"),'Axis label size',11),
                  numericInput(ns("inter_size_plot"),"Size:", value=10),
                  numericInput(ns("inter_labang"),"Label rotation (x):", value=-90),



                )

              )

            ),
            column(
              8,class="mp0",
              box_caret(
                ns("39"),
                tip=tipright("Investigate interactions with respect to the  set of most important variables"),
                title="Variable Interactions",
                button_title=actionLink(ns('down_plot_inter'),"Download",icon("download")),
                plotOutput(ns("rf_inter_out"))
              ),

              box_caret(ns("40"),
                        title="Table",
                        button_title=actionLink(ns('down_table_inter'),"Download",icon("download")),
                        uiOutput(ns("rf_inter_tab")))
            ),


          )

        ),
        tabPanel(
          "6. Sankey plot",  value="tab6",
          div(

            column(
              4,class="mp0",
              box_caret(
                ns("box1"),
                title="Options",
                div(
                  uiOutput(ns("sankey_side")),
                  uiOutput(ns("sankey_print"))
                )
              )
            ),
            column(
              8,class="mp0",
              box_caret(
                ns("box2"),
                title="Plot",
                button_title = actionLink(ns('down_gg_sankey'),"Download",icon("download")),
                div(
                  tabsetPanel(
                    id=ns("sankey_tab"),
                    header=span(actionButton(ns('run_sankey_ploly'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px"),actionButton(ns('run_sankey_gg'),"RUN >>",style="height: 20px;font-size: 11px;padding: 2px")),
                    tabPanel("plotly",
                             uiOutput(ns("sankey_plotly"))
                    ),
                    tabPanel("ggplot",
                             uiOutput(ns("sankey_gg"))
                    )
                  )
                )

              )
            )

          )



        )

      )

    ))
}


rf_explainer$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns

    output$sankey_print<-renderUI({


      links<-get_sankey_links()

      div(
        renderPrint({
          subset(links,variable==input$sankey_variable)[c("Y","value")]
        }))
    })

    gg_sankey<-eventReactive(input$run_sankey_gg,ignoreInit = T,{

      req(input$sankey_tab=="ggplot")
      colnames<-c("Y","variable")
      value_names<-input$sankey_values

      if(input$sankey_values2!="None"){
        colnames<-c("Y",input$sankey_values2,"variable")
        value_names<-c(input$sankey_values,input$sankey_values2)
      }
      md<-get_mdf()[,colnames,drop=F]





      gdf<- get_mdf()


      if(length(value_names)>1){
        md<-md[order(gdf[,'Y'],gdf[,'variable'],gdf[,value_names[1]],value_names[2]),]
      } else{
        md<-md[order(gdf[,'Y'],gdf[,'variable'],gdf[,value_names[1]]),]
      }


      pal_end<-vals$newcolhabs[[input$sankey_palette]]
      pal_y<-vals$newcolhabs[[input$sankey_paletteY]]
      pal_middle<-vals$newcolhabs[[input$sankey_var2]](1)
      pal_links<-vals$newcolhabs[[input$sankey_palette_links]]


      df <- md %>%make_long(colnames)
      sy<-get_sankey_Y()
      cols_y<-pal_y(100)[cut(sy$metric_value,100)]
      plot(1:length(cols_y),pch=16,col=cols_y)
      sy$color<-cols_y
      df$color<-NA
      df$metric<-NA

      mean_var<-tapply(get_mdf()[,input$sankey_values],get_mdf()$variable,mean)

      cols_end<-pal_end(10)[cut(mean_var,10)]


      df$color[df$node%in%names(mean_var)]<-"transparent"
      df$color[df$node%in%sy$Y]<-cols_y[as.factor(df$node[df$node%in%sy$Y])]


      df$metric[df$node%in%names(mean_var)]<-mean_var
      df$metric[df$node%in%sy$Y]<-sy$metric_value

      #df$color[is.na(df$next_node)]<-"gray"

      cols<-df$color[which(df$node%in%sy$Y)]

      plot(seq_along(cols),pch=16,col=cols)






      df0<-df


      {

        df0$node<-factor(df$node,levels=c(unique(sy$Y,md$variable)))
        #df0$color[1:736]<-"#F1CA3AFF"


        p<-ggplot(df, aes(x = x,
                          group=x,
                          next_x = next_x,
                          node = node,
                          next_node = next_node,
                          label = node,
                          fill = factor(color))) +
          geom_sankey(flow.alpha = 0.5)+
          theme_sankey(base_size = 16) +
          scale_fill_manual(guide="none",values=c(pal_y(length(unique(df$color)))),aesthetics='fill',labels = c(paste0(sort(round(sy$metric_value,2))),""))+
          geom_sankey_label(size = input$sankey_size, color = 1,

                            fill=c(cols_y,cols_end),show.legend =F)

        p
        yrange=layer_scales(p)$y$range$range
        xrange<-seq_along(layer_scales(p)$x$range$range)
        y_ann<-scales::rescale(seq_along(round(mean_var,2)),c(yrange[1],yrange[2]))
        y_ann<-c((max(y_ann)+(y_ann[2]-y_ann[1]))*1.1,y_ann)
        lab0<-gsub("mean_mean_","mean_",paste0("mean_",input$sankey_values))
        label=c(lab0, round(mean_var,2))


        if(isTRUE(input$sankey_showlegend)){
          p<-p + annotate("text", x = xrange[2]*1.1, y = y_ann,label =label, parse = TRUE,size=input$sankey_legsize)+
            guides(fill = guide_legend(
              title = unique(sy$metrics_name)[1]

            ))


        }
        #override.aes = list(fill=c(pal_y(length(sy$metric_value)),"transparent"),labels=c(sort(sy$metric_value),""))
        p+xlab("")+ylab("")+
          theme(
            legend.text = element_text(size=10),
            legend.key.size = unit(input$sankey_legsize, 'mm'),
            legend.title=element_text(size=unit(input$sankey_legsize*3, 'mm'))
          )+ggtitle(input$sankey_title)
      }




    })








    output$sankey_gg<-renderUI({
      validate(need(length(measures())>0,"The Measure Importance must be calculated in the '1. Measures' tab."))
      validate(need(is_installed("devtools"),"Requires DevTools package. Please close iMESc and install the package manually in R"))
      validate(need(length(get_rfs()) > 1, "You must have more than one Random Forest model saved in the Datalist."))
      validate(need(length(get_all_mdd()) > 1, "You must calculate the Measures for more than one model."))
      devtools::source_url('https://raw.githubusercontent.com/davidsjoberg/ggsankey/main/R/sankey.R')
      p<- withProgress(gg_sankey(),NA,NA,message="Creating Sankey...")
      renderPlot({
        p
      }, width=input$sankey_width,height=input$sankey_height)

    })


    get_rfs<-reactive({
      data_x<-vals$cur_data_sl
      req(data_x)
      rfs<-attr(vals$saved_data[[data_x]],"rf")

      rfs
    })
    get_all_mdd<-reactive({
      rfs<-get_rfs()
      md<-lapply(rfs,function(x){
        data.frame(attr(x[[1]],'mindepth')[[2]])

      })
      md<-md[sapply(md,nrow)>0]
      md

    })
    get_sankey_Y<-function(){

      rfs<-get_rfs()
      yss<-sapply(rfs,function(x){
        attr(x[[1]],'supervisor')
      })
      metric_value<-sapply(rfs,function(x){
        x$m$results[rownames(x$m$bestTune),input$sankey_metric]
      })
      metrics_name<-sapply(rfs,function(x){
        input$sankey_metric
      })

      mdd_true<-which(sapply(rfs,function(x){length(attr(x[[1]],'mindepth')[[2]])})>0)
      data.frame(Y=yss,metric_value,metrics_name)[mdd_true,]
    }



    get_sankey_nodes<-function(){
      pal_end<-vals$newcolhabs[[input$sankey_palette]]
      pal_y<-vals$newcolhabs[[input$sankey_paletteY]]
      pal_middle<-vals$newcolhabs[[input$sankey_var2]](1)
      sankey_y<-get_sankey_Y()
      layers<- get_sakey_layers()
      layers_metrics<-get_sakey_values()

      ys<-sankey_y$Y



      links<-get_sankey_links()
      variables<-as.character(links$variable)

      var_ids<-sapply(get_mdf()[,layers],function(x) as.character(unique(sort(x))))

      nodes=reshape2::melt(var_ids)
      colnames(nodes)<-c("name","tag")



      nodes$value<-nodes$name
      nodes$color<-NA
      nodes$metric_tag<-nodes$tag

      metric_y<-sankey_y$metric_value[which(sankey_y$Y%in%var_ids$Y)]
      color_y<-pal_y(100)[cut(metric_y,100)]
      metric_tag_y<-sankey_y$metrics_name[which(sankey_y$Y%in%var_ids$Y)]

      nodes$value[nodes$tag%in%'Y']<-metric_y
      nodes$color[nodes$tag%in%'Y']<-color_y
      nodes$metric_tag[nodes$tag%in%'Y']<-metric_tag_y


      metric_end<-layers_metrics[length(layers_metrics)]
      var_mean<- tapply(get_mdf()[,metric_end],get_mdf()$variable,mean)
      nodes$value[nodes$name%in%names(var_mean)]<-var_mean
      nodes$color[nodes$name%in%names(var_mean)]<-pal_end(100)[cut(var_mean,100)]
      nodes$metric_tag[nodes$name%in%names(var_mean)]<-gsub("mean_mean_","mean_",paste0("mean_",metric_end))
      nodes$color[is.na(nodes$color)]<-pal_middle


      nodes$id<-1:nrow(nodes)
      layers_metrics
      linodes<-split(nodes,nodes$tag)

      linodes<-linodes[unique(nodes$tag)]
      xs<-scales::rescale(seq_along(linodes),c(0.1,.9))
      nodes<-do.call(rbind,lapply(seq_along(linodes),function(i){
        x<-linodes[[i]]
        xtemp<-x[order(x$value,decreasing=T),]
        xtemp$x<-xs[[i]]
        xtemp$y<-scales::rescale(1:nrow(xtemp),c(0.1,.9))
        xtemp
      }))
      #nodes<-nodes[order(nodes$id),]






    }
    get_mdf<-function(){
      rfs<-get_rfs()
      var=input$sankey_values
      req(var)
      #pal<-vals$newcolhabs[[input$sankey_palette]]

      mdl<-lapply(seq_along(rfs),function(i){
        x<-rfs[[i]]
        name<-names(rfs)[i]
        mindepth<-attr(x[[1]],'mindepth')[[2]]
        if(length(mindepth)>0){
          res<-data.frame(Model_name=name,Y= attr(x[[1]],'supervisor'),mindepth)
          res[order(res[,var],decreasing=T),]
        }
      })

      mdf<-do.call(rbind,mdl)
      mdf<-do.call(rbind,lapply(split(mdf,mdf$variable),function(x){
        x[order(x[,var],decreasing = T),]
      }))
      rownames(mdf)<-NULL

      if(input$sankey_values2!="None"){
        if(is.numeric(mdf[,input$sankey_values2])){
          if(isTRUE(input$sankey_cuton)){
            cutted<-cut(mdf[,input$sankey_values2],input$sankey_cut)
            newcut<-as.numeric(sapply(cutted,function(x){
              strsplit(gsub("\\)|\\(|\\]|\\[","",x),",")[[1]][2]
            })
            )

            mdf[,input$sankey_values2]<-newcut
          }
        }

      }


      mdf
    }
    get_sankey_links<-function(){
      # input<- readRDS('input.rds')
      # vals<- readRDS('savepoint.rds')
      rfs<-get_rfs()
      req(input$sankey_palette)
      var=input$sankey_values
      layers<- get_sakey_layers()
      layers_metrics<-get_sakey_values()
      pal<-vals$newcolhabs[[input$sankey_palette_links]]
      mdf<-get_mdf()

      ys<-get_sankey_Y()$Y
      value=mdf[,var]

      mdf$times_a_root

      i=layers[1]
      source=list()
      init=0
      for(i in layers){
        source[[i]]<-as.numeric(factor(mdf[,i]))+init
        init=max(source[[i]])
      }
      target=list()
      init=max(source[[1]])
      for(i in layers[-1]){

        target[[i]]<-as.numeric(factor(mdf[,i]))+init
        init=max(source[[i]])
      }

      variable=list()
      for(i in layers[-1]){
        variable[[i]]<-mdf[,i]
      }

      value=as.numeric(unlist(mdf[,layers_metrics]))
      source<-as.numeric(unlist(source[-length(source)]))
      target<-as.numeric(unlist(target))


      links=data.frame(source,target,value,variable=variable, Y=mdf$Y)
      links$source=links$source-1
      links$target=links$target-1

      pal_y<-vals$newcolhabs[[input$sankey_paletteY]]

      links<-links[order(links$source, links$target),]
      links
      mdf$Y<-factor(mdf$Y, levels=sort(get_sankey_Y()$Y))
      color=as.vector(sapply(input$sankey_factor_link,function(i){

        if(i=="Y"){
          sy<-get_sankey_Y()
          cols_y<-pal_y(length(sy$metric_value))[cut(sy$metric_value,length(sy$metric_value))]
          cutv<-cut(links$source,length(sy$metric_value))
          cols_y[cutv]
        } else{

          cutv<-cut(links$value,100)
          pal(100)[cutv]

        }


      }))



      links$color<-color
      links
    }




    observe({
      shinyjs::toggle("sankey_var2",condition=input$sankey_values2!="None")
      shinyjs::toggle("sankey_cut",condition=isTRUE(input$sankey_cuton))
    })



    output$sankey_side<-renderUI({
      md<-get_all_mdd()

      req(length(get_all_mdd())>1)

      choices<-unlist(sapply(md,colnames))
      choices<-unique(choices[!choices%in%"variable"])
      names(choices)<-NULL
      choices2<-do.call(rbind,md)$variable
      m$results

      m<-get_rfs()[[1]]$m
      choices_metric<-if(m$modelType=="Classification"){
        c("Accuracy","Kappa")
      } else{
        c('Rsquared','RMSE',"MAE")
      }
      div(
        pickerInput_fromtop(session$ns("sankey_metric"),"Metric",choices_metric),
        pickerInput_fromtop(session$ns("sankey_values"),"End Value",choices),
        pickerInput_fromtop(session$ns("sankey_values2"),"Middle value",c("None",choices)),
        checkboxInput(ns('sankey_cuton'),"Cut middle values"),
        numericInput(session$ns("sankey_cut"),"Middle breaks",10),


        pickerInput_fromtop(
          inputId = ns("sankey_paletteY"),
          label = "Palette Y:",
          selected="Blues",
          choices =vals$colors_img$val,
          choicesOpt = list(content =vals$colors_img$img)),
        pickerInput_fromtop(
          inputId = ns("sankey_var2"),
          label = "Palette Middle:",
          choices =vals$colors_img$val,
          selected="gray",
          choicesOpt = list(content =vals$colors_img$img)),
        pickerInput_fromtop(
          inputId = ns("sankey_palette_links"),
          label = "Palette link:",
          selected="Grays",
          choices =vals$colors_img$val,
          choicesOpt = list(content =vals$colors_img$img)),
        uiOutput(ns("sankey_factor_link")),
        pickerInput_fromtop(
          inputId = ns("sankey_palette"),
          label = "Palette end:",
          choices =vals$colors_img$val,

          choicesOpt = list(content =vals$colors_img$img)),
        div(id=ns("sankey_gg_args"),style="display: none",
            numericInput(ns("sankey_width"),"Plot width",480),
            numericInput(ns("sankey_height"),"Plot Height",700),
            textInput(ns("sankey_title"),"Title:", value="Sankey plot"),
            numericInput(ns("sankey_size"),"Text size",3),
            checkboxInput(ns("sankey_showlegend"),"Show legend",T),
            numericInput(ns("sankey_legsize"),"Legend size",3)),
        pickerInput_fromtop(session$ns("sankey_variable"),"Value",choices2)





      )
    })


    output$sankey_factor_link<-renderUI({
      pickerInput_fromtop(ns('sankey_factor_link'),"color link by",c("Y",get_sakey_values()))
    })

    observe({
      shinyjs::toggle("sankey_palette_links",condition=input$sankey_tab=='plotly')
      shinyjs::toggle("sankey_gg_args",condition=input$sankey_tab=='ggplot')
      #shinyjs::toggle("down_gg_sankey",condition=input$sankey_tab=='ggplot')
      shinyjs::toggle("run_sankey_ploly",condition=input$sankey_tab=='plotly')
      shinyjs::toggle("run_sankey_gg",condition=input$sankey_tab=='ggplot')

    })

    observeEvent(input$down_gg_sankey,ignoreInit = T,{



      req(input$sankey_tab=="plotly")

      map<-get_ploly_sankey()
      showModal(
        modalDialog(
          title='Download as png file',
          ll_down_modal$ui(ns("sankey")),
          easyClose = T
        )
      )
      ll_down_modal$server("sankey",map,file_name="sankey_plotly_")

    })
    observeEvent(input$down_gg_sankey,ignoreInit = T,{

      req(input$sankey_tab=="ggplot")
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=gg_sankey()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Sankey GGplot", name_c="sankey_gg")
    })




    output$sankey_plotly<-renderUI({
      validate(need(length(get_rfs()) > 1, "You must have more than one Random Forest model saved in the Datalist."))
      validate(need(length(get_all_mdd()) > 1, "You must calculate the Measures for more than one model."))

      div(
        plotly::plotlyOutput(ns('sankey_plot'))
      )
    })


    get_sakey_values<-function(){
      if(input$sankey_values2!="None"){
        c(input$sankey_values,input$sankey_values2)
      } else{
        input$sankey_values
      }
    }


    get_sakey_layers<-function(){
      if(input$sankey_values2!="None"){
        c("Y",input$sankey_values2,"variable")
      } else{
        c("Y","variable")
      }
    }



    get_ploly_sankey<-eventReactive(input$run_sankey_ploly,ignoreInit = T,{
      req(input$sankey_tab=="plotly")

      var=input$sankey_values
      nodes<-get_sankey_nodes()
      links<-get_sankey_links()
      get_sankey_links()$color

      fig <- plotly::plot_ly(
        type = "sankey",
        domain = list(
          x =  c(0,1),
          y =  c(0,1)
        ),
        orientation = "h",
        valueformat = "",
        valuesuffix = "",
        # label<-paste0(nodes$metric_tag,":",nodes$value)
        node = list(
          label = nodes$name,
          color = nodes$color,
          # value=nodes$value,
          customdata = abind::abind(nodes),
          x=nodes$x,
          y=nodes$y,
          #customInfo =nodes$metric_tag,
          pad = 15,
          thickness = 15,
          hovertemplate = "%{customdata[4]}<br><extra>%{customdata[2]:,.4f}</extra>'",

          line = list(
            color = "black",
            width = 0.5
          )
        ),

        link = list(
          source = links$source,
          target = links$target,
          value =  links$value,
          color = links$color,
          hovertemplate = 'Y: %{source.label}<br>Var: %{target.label}<br>Value: %{value}<extra></extra>'
        )
      )
      fig

    })


    output$sankey_plot<-plotly::renderPlotly({

      get_ploly_sankey()
    })








    ####
    model<-reactive({
      m<-vals$cur_caret_model
      req(inherits(m,"train"))
      m

    })










    observeEvent(get_measure_table(),{
      data<-get_measure_table()
      choices=colnames(data)[-1]
      choices=choices[-length(choices)]
      updateSelectInput(session,"multi_x_measure",choices=choices,selected='mean_min_depth')
      updateSelectInput(session,"multi_y_measure",choices=choices,selected='times_a_root')
      if( model()$modelType=="Classification"){
        selected='accuracy_decrease'
      } else{
        selected='mse_increase'
      }
      updateSelectInput(session,"multi_z_measure",choices=choices,selected=selected)

    })

    output$mdd_palette<-renderUI({
      pickerInput_fromtop(
        inputId = ns("mdd_palette"),
        label = "Palette:",
        choices =vals$colors_img$val,
        choicesOpt = list(content =vals$colors_img$img))
    })

    output$inter_palette<-renderUI({
      pickerInput_fromtop(
        inputId = ns("inter_palette"),
        label = "Palette:",
        choices =vals$colors_img$val,
        choicesOpt = list(content =vals$colors_img$img))
    })


    observeEvent( model(),{
      m<-model()
      #value=paste0(attr( model(),"Y"),"~",attr( model(),"Datalist"))
      if(m$modelType=="Regression"){
        value=paste0(" R²=",round(m$results[ rownames(m$bestTune),"Rsquared"],2))} else{
          value=paste0(" Acc=",round(m$results[ rownames(m$bestTune),"Accuracy"],2))
        }

      value=paste0(attr( model(),"supervisor"),";",value)
      updateTextInput(session,'title_prf',value=value)

    })



    data_x<-reactive(attr( model(),"Datalist"))
    model_name<-reactive(attr( model(),"model_name"))

    observe({
      shinyjs::toggle('rfbi_grid',condition= model()$modelType=="Classification")
    })
    getsolid_col<-reactive({
      res<-lapply(vals$newcolhabs, function(x) x(10))
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
    output$interframe_palette<-renderUI({
      req(input$multi_z_measure)
      if(input$multi_z_measure=="None"){
        pic<-getsolid_col()
      } else{
        pic<-getgrad_col()
      }
      pickerInput_fromtop(
        inputId = ns("interframe_palette"),
        label = "Palette:",
        choices =vals$colors_img$val[pic],
        choicesOpt = list(content =     vals$colors_img$img[pic])
      )
    })

    mindeaphrf<- reactive({try({
      req(length( model())>0)
      withProgress(message = "Running ...",
                   min = 1,
                   max = 1,
                   {

                     res<-multipimp( model(),measures=c(input$rf_measures,'p_value','no_of_trees'), mean_sample=input$rfmeasure_mean_sample)
                   })
      res

    })})
    observeEvent(ignoreInit = T,input$run_measures,{
      m<-model()

      try({

        if(model_name()=="new rf (unsaved)"){
          attr(vals$cur_caret_model,"mindepth")<-mindeaphrf()
        } else{
          attr(attr(vals$saved_data[[data_x()]],"rf")[[model_name()]][[1]],"mindepth")<-mindeaphrf()
        }
        vals$update_tab_results<-"rfe"
        updateTabsetPanel(session,"rftab2","rftab2_3")

      })
    })
    get_measures<-reactive({
      res<-attr(attr(vals$saved_data[[data_x()]],"rf")[[model_name()]][[1]],"mindepth")
      req(res)
      res
    })
    get_measure_table<-reactive({
      get_measures()[[2]]
    })


    output$out_between<-renderUI({
      validate(need(length(measures())>0,"The Measure Importance must be calculated in the '1. Measures' tab."))
      explainer_ggpair$server("between",get_measure_table(),fun="plot_importance_ggpairs",vals)
      NULL
    })
    output$out_rank<-renderUI({

      explainer_ggpair$server("rank",get_measure_table(),fun="plot_importance_rankings",vals)
      NULL
    })




    output$rf_measure_out<-renderUI({
      measure_table<-get_measure_table()
      div(class="half-drop-inline",style="overflow-x: auto",
          fixed_dt(
            measure_table,pageLength = 20,dom="ltp",
            round=NULL,scrollY = "200px",scrollX = "300px",
            extensions = c("FixedHeader"))
      )
    })
    observeEvent(ignoreInit = T,input$downcenter_rfdepth,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"imp_measures"
      data<-get_measure_table()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Importance Measures Results",data=data, name=name)
    })


    rf_inputsmeasure<-reactive({
      if( model()$modelType=="Classification"){
        choices=c("mean_min_depth",
                  "accuracy_decrease",
                  "gini_decrease" ,
                  "no_of_nodes",
                  "times_a_root")
      } else{
        choices=c('mean_min_depth', 'mse_increase', 'node_purity_increase', 'no_of_nodes', 'times_a_root')
      }
    })
    prf.reactive<-reactive({

      req(input$rf_depth)
      rf_depth<-switch(input$rf_depth,
                       "sigs"=TRUE,
                       "user"=input$n_var_rf,
                       "all"=ncol( model()$trainingData[-1])
      )


      res_multi=get_measures()

      args<-list(
        res_multi=res_multi, sigs =rf_depth,
        sig.value=input$sigprf,
        size_plot=input$labrfsize,
        min_no_of_trees = input$depth_min_no_of_trees,
        mean_sample = input$depth_mean_sample,
        newcolhabs=vals$newcolhabs,
        palette=input$mdd_palette,
        size_mmd=input$size_mmd
      )
      res<-do.call(prf,args)
      res<- res +
        ggtitle(input$title_prf)+
        labs(x = input$ylab_prf, y=input$xlab_prf)+
        theme(axis.text=element_text(size=input$prf_axis_size),

              axis.title=element_text(size=input$prf_label_axis_size)
        )



      vals$rf_sigs<-attr(res,"sigs")

      res
    })

    measures<-reactive({
      measures<-attr(attr(vals$saved_data[[data_x()]],"rf")[[model_name()]][[1]],"mindepth")
    })
    output$feature_plot<-
      renderUI({

        validate(need(length(measures())>0,"The Measure Importance must be calculated in the '1. Measures' tab."))
        renderPlot({
          p<-prf.reactive()
          p
        },height=input$plot_height,width=input$plot_width)
      })

    n_displaying<-reactive({

      req(input$rf_depth)
      n<-switch(input$rf_depth,
                "sigs"=sum(get_measure_table()$p_value<=input$sigprf),
                "user"=input$n_var_rf,
                "all"=ncol( model()$trainingData[-1])
      )

      (n*20)+150

    })

    observeEvent(n_displaying(),{
      updateNumericInput(session,'plot_height',value=n_displaying())
    })

    observeEvent(input$create_rf,ignoreInit = T,{
      m<-model()
      data<-get_measure_table()
      data_x<-attr( model(),"Datalist")
      req(data_x)
      data_o<-vals$saved_data[[data_x]]
      data<-data_o[,vals$rf_sigs$variable,drop=F]
      data<-data_migrate(data_o,data)

      module_save_changes$ui(ns("explainer-create"))
      bag<-attr(m,'model_name')
      if(is.null(bag)){
        bag<-attr(m,"model_tag")
      }
      bag<-paste0(bag,"_sig_vars")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      attr(data,"bag")<-bag
      vals$newdatalist<-data

      module_save_changes$server("explainer-create",vals)
    })

    observeEvent(rf_inputsmeasure(),{
      updateCheckboxGroupInput(session,"rf_measures",choices=rf_inputsmeasure(),selected=rf_inputsmeasure())
    })
    observe({
      shinyjs::toggle('n_var_rf',condition=input$rf_depth=="user")
      shinyjs::toggle('multi_no_of_labels',condition=input$rf_sigmulti=="user")
    })

    output$rf_multi_out<-renderPlot({
      validate(need(length(measures())>0,"The Measure Importance must be calculated in the '1. Measures' tab."))
      rf_multi()
    })


    observeEvent(input$downp_prf,ignoreInit = T,{
      model_name<-attr(model(),"model_name")
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=prf.reactive()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Minimal depth distribution", name_c=paste0("min-depth-plot-",model_name))
    })
    observeEvent(input$downp_mrf,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=rf_multi()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Multiway Importance", name_c="multi-way-plot")
    })
    rf_multi<-reactive({
      req(input$interframe_palette)
      req(input$rf_sigmulti)
      only_sigs<-switch(input$rf_sigmulti,
                        "sigs"=TRUE,
                        "user"=input$multi_no_of_labels,
                        "all"=ncol( model()$trainingData[-1])
      )

      frame<- data.frame(get_measure_table())
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
        max.overlaps=input$max.overlaps,
        only_sigs=only_sigs

      )


      p

    })






    output$rf_rank<-renderUI({
      div(
        renderPlot({


          vals$rf_rank<- randomForestExplainer::plot_importance_rankings(get_measure_table())
          vals$rf_rank


        })
      )
    })
    observeEvent(ignoreInit = T,input$go_rfinter,{
      vals$update_tab_results<-"rfe"
      #req(is.null(interframe()))
      req(input$rfinter_k)
      req(input$rfinter_mean_sample)
      req(input$uncond_mean_sample)
      req(input$rfinter_k2)
      withProgress(message = "Running ...",
                   min = 1,
                   max = 1,
                   {
                     importance_frame<- get_measure_table()
                     vars<-randomForestExplainer::important_variables(importance_frame, k = input$rfinter_k, measures = c("mean_min_depth", "no_of_trees"),ties_action="draw")
                     forest= model()$finalModel
                     interactions_frame<-suppressWarnings(min_depth_interactions(forest, vars, mean_sample=input$rfinter_mean_sample, uncond_mean_sample=input$uncond_mean_sample))
                     vals$rf_interactions_frame
                     res<-interactions_frame[ order(interactions_frame$occurrences,decreasing=T),]

                     attr(attr(vals$saved_data[[data_x()]],"rf")[[model_name()]][[1]],"interframe")<-res

                   })
      updateTabsetPanel(session,"rftab2_3","tab5")



    })
    interframe<-reactive({

      attr(attr(vals$saved_data[[data_x()]],"rf")[[model_name()]][[1]],"interframe")

    })

    output$rf_inter_tab<-renderUI({
      div(class="half-drop-inline",style="overflow-x: auto",
          fixed_dt(
            interframe(),pageLength = 20,dom="ltp",
            round=NULL,scrollY = "200px",scrollX = "300px",
            extensions = c("FixedHeader"))
      )
    })

    inter_plot<-reactive({

      req(interframe())
      req(input$rfinter_k2)
      k2<-input$rfinter_k2
      if(is.na(k2)){
        k2<-NULL
      }
      p<-randomForestExplainer::plot_min_depth_interactions(interframe(),k=k2,main="")+scale_fill_gradientn(colours =scale_color_2(input$inter_palette,vals$newcolhabs)) +
        theme_bw(base_size = input$inter_size_plot)+
        theme(
          plot.title=element_text(size=input$int_cex.title,face=input$int_font.title),
          plot.subtitle=element_text(size=input$int_cex.subtitle,face=input$int_font.subtitle),
          axis.text=element_text(size=input$int_cex.axes),
          axis.title=element_text(size=input$int_cex.lab),
          axis.text.x=element_text(angle = input$inter_labang, hjust = 0)
        )+
        ggtitle(input$inter_title,subtitle =input$inter_subtitle)


      vals$rf_inter<-p
      p



    })
    output$rf_inter_out<-renderPlot({
      validate(need(length(measures())>0,"The Measure Importance must be calculated in the '1. Measures' tab."))
      inter_plot()
    })

    observeEvent(ignoreInit = T,input$down_table_inter,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"inter_results"
      data<-interframe()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Interactions Results",data=data, name=name)
    })


    observeEvent(input$down_plot_inter,ignoreInit = T,{
      model_name<-attr(model(),"model_name")
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=inter_plot()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Interactions Plot", name_c=paste0("inter-",model_name))
    })


    observeEvent(input$rfinter_k2,{

      updateTextInput(session,"inter_subtitle",value=paste0("Mean minimal depth for ",paste0(input$rfinter_k2, " most frequent interactions")))

    })
    observe({

      model_name<-attr( model(),'model_name')
      updateTextInput(session,'inter_title',value=model_name)

    })

    observeEvent(input$create_rfinter,ignoreInit = T,{
      data<-interframe_n()
      data_x<-attr( model(),"Datalist")
      req(data_x)
      data<-data_migrate(vals$saved_data[[data_x]],data)
      module_save_changes$ui(ns("explainer-create-inter"))
      bag<-attr( model(),'model_name')
      if(is.null(bag)){
        bag<-attr( model(),"model_tag")
      }
      bag<-paste0(bag,"_frequent_inters")
      newnames<-make.unique(c(names(vals$saved_data),bag))
      bag<-newnames[length(newnames)]
      vals$newdatalist<-data
      module_save_changes$server("explainer-create-inter",vals)
    })




    interframe_n<-reactive({
      a<-interframe()[1:input$rfinter_k2,"variable"]
      b<-as.character(interframe()[1:input$rfinter_k2,"root_variable"])
      pic<-unique(c(a,b))
      vals$saved_data[[data_x()]][, pic]
    })
    sapply(c(as.character(30:43),'32_2'),function(x)  box_caret_server(x))
    box_caret_server("inter_plotoptions")



  })
}

