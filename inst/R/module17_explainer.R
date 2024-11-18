

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
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Importance Measures - GGpairs", name_c="measures-ggpairs",datalist_name=datalist_name)
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
        selected="tab1",
        #selected="tab6",
        tabPanel("1. Measures",value="tab1",
                 div(
                   column(4,class="mp0",
                          box_caret(ns('30'),click=F,title="Analysis options",
                                    div(
                                      selectInput(ns('rfmeasure_mean_sample'),span(tipright("The sample of trees on which conditional mean minimal depth is calculated"),'mean_sample'),choices=c("all_trees", "top_trees", "relevant_trees"),selected='top_trees'),
                                      div(class="normal-checkbox",
                                          checkboxGroupInput(ns('rf_measures'),span(tipright(" the measures of importance to be used"),'measures:'),choices=c("mean_min_depth",
                                                                                                                                                              "accuracy_decrease","gini_decrease" ,"no_of_nodes","times_a_root"),selected=c("mean_min_depth","accuracy_decrease","gini_decrease" ,"no_of_nodes","times_a_root"))
                                      ),
                                      checkboxInput(ns("loop_measures"),span("Calculate for all saved models"))
                                    )

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

                              textInput(ns("xlab_prf"),"X label:", value="Number of Trees"),
                              textInput(ns("ylab_prf"),"Y label:", value="Variable"),

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
                                      tipify_ui(
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
                  div(tipify_ui(actionLink(ns('create_rfinter'),span("Create Datalist",icon("fas fa-file-signature"))),"Create a datalist with the variables included in the N most frequent interactions in the RF","right"))
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
                    selected="ggplot",
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


add_color_ggsankey<-function(df,value_names,pal_y,pal_end,pal_middle,ggsankey_colorY="model_metric",var_type=NULL,newcolhabs){



  df1<-data.frame(df)



  Yids<-df1$x%in%'Y'
  Vids<-df1$x%in%'variable'
  if(ggsankey_colorY=="model_metric"){
    if(is.numeric(unlist(df1[Yids,"metric"])))
      #fac_Y<-as.factor(unlist(df1[Yids,"node"]))
      fac_Y<-cut(unlist(df1[Yids,"metric"]),sort(unique(df1[Yids,"metric"])),include.lowest = T)else{

        fac_Y<-as.factor(unlist(df1[Yids,"node"]))

      }


  }


  if(is.null(var_type)){
    fac_V<-as.factor(unlist(df1[Vids,"node"]))

    df1$fac_col[Yids]<-as.character(fac_Y)
    df1$fac_col[Vids]<-as.character(fac_V)
  } else{
    fac_V<-as.factor(unlist(df1[Vids,"var_type"]))
    df1$fac_col[Yids]<-as.character(fac_Y)
    df1$fac_col[Vids]<-as.character(fac_V)
  }
  df1$fac_col<-factor(df1$fac_col,levels=c(levels(fac_Y),levels(fac_V)))
  pals<-list(
    py=pal_y(nlevels(fac_Y)),
    pe=pal_end(nlevels(fac_V))
  )

  if(length(value_names)>1){
    Mids<-df1$x%in%value_names[[2]]
    fac_M<-as.factor(unlist(df1[Mids,"node"]))

    df1$fac_col<-as.character(df1$fac_col)
    df1$fac_col[Mids]<-as.character(fac_M)

    df1$fac_col<-factor(as.character(df1$fac_col),
                        levels=c(levels(fac_Y),
                                 levels(fac_V),
                                 levels(fac_M)
                        ))
    pals$pm<-pal_middle(nlevels(fac_M))
    pals<-pals[c("py","pm","pe")]
  }
  newpals<-unique(unlist(pals))
  df1$color<-unlist(pals)[df1$fac_col]
  df1$color<-factor( df1$color,levels=newpals)
  attr(df1,"colors")<-  pals
  df1

}

gg_sankey_plot<-function(gdf,sy,sankey_values,sankey_values2,sankey_palette,sankey_paletteY,sankey_palette_middle,sankey_size,sankey_showlegend,sankey_legsize, sankey_title,color_var=NULL,ggsankey_colorY='model_metric',var_type=NULL,newcolhabs,node_width=0.3,labels_in=F,hjust0=-0.085,hjust1=0.08,custom_var_labels=NULL,custom_y_labels=NULL,sankey_size2=3,...){

  if(is.null(var_type)){
    levs<-levels(gdf$variable)
    var_type<-levels(gdf$variable)
    names(var_type)<-var_type
  }

  {

    #sankey_values2<-"no_of_trees"

    value_names<-sankey_values
    colnames<-c("Y","variable")
    if(sankey_values2!="None"){
      colnames<-c("Y",sankey_values2,"variable")
      value_names<-c(sankey_values,sankey_values2)
    }

    pal_link<-newcolhabs[["Greens"]]
    pal_end<-newcolhabs[[sankey_palette]]
    pal_y<-newcolhabs[[sankey_paletteY]]
    pal_middle<-newcolhabs[[sankey_palette_middle]]

    metric_node<-sankey_values
    metric_node<-gdf[,c(sankey_values)]
    names(metric_node)<-unique(factor(paste0(gdf$Y,"_",gdf$variable)))

    # df <- md %>%make_long(c(colnames))
    {




      ggdf<-data.frame(gdf[1:3],lapply(gdf[-c(1:3)],function(x) round(scales::rescale(x,c(1,100)))))

      ggdf<-ggdf[c("Y","variable",value_names)]



      ggdf2<-data.frame(Y=unlist(sapply(seq_along(ggdf$Y),function(i) rep(ggdf$Y[i],ggdf[,value_names[1]][i]) )),
                        variable=unlist(sapply(seq_along(ggdf$Y),function(i) rep(ggdf$variable[i],ggdf[,value_names[1]][i]) )))


      names_fac<-paste0(ggdf$Y,"-",ggdf$variable)
      if(length(value_names)>1){
        second_var<-ggdf[,value_names[2]]
        names(second_var)<-names_fac
        ggdf2[,value_names[2]]<-second_var[paste0(ggdf2$Y,"-",ggdf2$variable)]
        ggdf2<-ggdf2[,c(1,3,2)]
      }



      gdf4<-ggdf2<-data.frame(lapply(ggdf2,as.character))
      gdf4$model_name<-paste0(gdf4$Y,"-",gdf4$variable)
      gdf4$model_rev<-paste0(gdf4$variable,"-",gdf4$Y)

      ggdf3 <- ggdf2 %>%
        make_long(colnames(ggdf2))
      ggdf5 <- gdf4 %>%
        make_long(colnames(gdf4)[c(3:4)])
      ggdf3$model_name<-ggdf5$node
      df<-ggdf3




    }

    {
      df$color<-NA
      df$metric<-NA

      mean_var<-tapply(gdf[,sankey_values],gdf$variable,mean)
      first_var<-ggdf[,"variable"]

      sy_metric_value<-sy$metric_value
      if(is.factor(sy_metric_value)){
        sy_metric_value<-as.character(sy_metric_value)
      }

      names(sy_metric_value)<-sy$Y
      sy_metric_value<-list(mean_var,sy_metric_value)


      df$metric<-   unlist(sy_metric_value)[df$node]


    }
  }

  df_temp<-df
  {
    if(!is.null(var_type)){
      names(var_type)<-levels(gdf$variable)
      df_temp$var_type<-NA
      varin<-unique(df$node[df$x%in%"variable"])

      df_temp$var_type[df$x%in%"variable"]<-    as.character(factor(df$node[df$x%in%"variable"],labels=      var_type[varin]))
    }

    df1<-add_color_ggsankey(df_temp,value_names,pal_y,pal_end,pal_middle,var_type=var_type,newcolhabs=newcolhabs)

    pals<-attr(df1,"colors")

    df1$splitY<-df1$next_node
    df1$splitY[!is.na(df1$next_node)]<-  df1$node[!is.na(df1$next_node)]
    df1$splitY[is.na(df1$splitY)]<-  df1$node[!is.na(df1$next_x)]

    df1$splitY<-factor(df1$splitY  , levels=levels(gdf$Y))



    if(!is.factor(gdf$Y))
      gdf$Y<-factor(gdf$Y)

    df1<-do.call(rbind,lapply(split(df1,df1$node),function(x){
      x$nrow<-nrow(x)
      x
    }))
    levr<-unique(factor(df1$nrow))
    df1$nrow<-as.numeric(factor(df1$nrow,levels=levr ,labels=1:length(levr)))
    splitvar<-   factor(var_type[gsub("-","",gsub(paste0(paste0(levels(gdf$Y)),collapse="|"),"",df1$model_name))])
    splitV<-   factor(gsub("-","",gsub(paste0(paste0(levels(gdf$Y)),collapse="|"),"",df1$model_name)),levels=levels(gdf$variable))

    df1$splitV<-splitV
    df1$model_name2<-paste0(df1$splitY,"-",df1$splitV)
    nvars=as.numeric(table(names(splitvar)))

    dl<-data.frame(
      var=levels(gdf$variable),
      nvars=as.numeric(table(names(splitvar))),
      vartype=as.numeric(factor(var_type[levels(gdf$variable)]))
    )
    dl<-do.call(rbind,lapply(split(dl,dl$vartype),function(x){
      x[order(x$nvars,decreasing=F),]
    }))


    dl$nvars<-1:nrow(dl)

    for(i in 1:nrow(dl)){
      df1$nrow[which(df1$splitV%in%dl$var[i])]<-dl$nvars[i]
    }

    df1$nrow[df1$x=="Y"]<-NA

    {

      p<-p<-ggplot(
        df1, aes(x = x,
                 next_x = next_x,
                 node = reorder(node,nrow),
                 next_node = next_node,
                 label = node,
                 fill = fac_col)
      )

      p<-p +
        geom_sankey(flow.alpha = 0.5,width=node_width)+
        theme_sankey(base_size = 16)+
        guides(fill="none",color="none")


      if(isFALSE(labels_in)){
        p<-p+geom_sankey_text(size = sankey_size, color = 1,show.legend =F)

      } else{
        {
          dfy<-df1[df1$x%in%"Y",]
          dfy$label<-dfy$node
          if(!is.null(custom_y_labels)){
            dfy$label<-factor(dfy$label,labels=custom_y_labels)


          }
          p2<-p+geom_sankey_text(data=dfy,mapping=aes(
            x = x,
            next_x = next_x,
            node = reorder(node,nrow),
            next_node = next_node,
            label = label,
            fill = fac_col
          ),size = sankey_size2, color = 1,
          hjust=0,
          show.legend =F,inherit.aes = F,
          position = position_nudge(x=hjust0))
        }


        {

          dfy<-df1[df1$x%in%"variable",]
          dfy$label<-dfy$node
          if(!is.null(custom_var_labels)){
            dfy$label<-factor(dfy$label,labels=custom_var_labels)


          }
          p2<-p2+geom_sankey_text(data=dfy,mapping=aes(
            x = x,
            next_x = next_x,
            node = reorder(node,nrow),
            next_node = next_node,
            label = label,
            fill = fac_col
          )  , type="alluvial",size = sankey_size, color = 1,show.legend =F,inherit.aes = T,space=33,hjust=0,
          position = position_nudge(x=hjust1,y=-1266))
        }
        p<-p2
      }
      #+  geom_sankey_text(size = sankey_size, color = 1,show.legend =F)




    }

    breaks<-NULL
    labels<-NULL
    if(isTRUE(sankey_showlegend)){
      breaks=sort(unique(df1$fac_col[df1$x=="Y"]))
      breaks<-breaks[quantile(1:length(breaks))]
      yrange=layer_scales(p)$y$range$range
      xrange<-seq_along(layer_scales(p)$x$range$range)
      y_ann<-scales::rescale(seq_along(round(mean_var,2)),c(yrange[1],yrange[2]))
      y_ann<-c((max(y_ann)+(y_ann[2]-y_ann[1]))*1.1,y_ann)
      lab0<-gsub("mean_mean_","mean_",paste0("mean_",sankey_values))
      label=c(lab0, round(mean_var,2))
      p<-p + annotate("text", x = xrange[2]*1.1, y = y_ann,label =label, parse = TRUE,size=sankey_legsize)
      labels=sort(round(df1$metric[breaks],2))

    }


    p<-p+
      scale_fill_discrete(
        name=sy$metrics_name[1],
        type=list(colorRampPalette(unlist(pals))(length(unique(df1$fac_col)))),
        breaks=breaks,
        labels=   labels
      )



  }
  #pal_middle<-colorRampPalette(c("black","gray70"))
  {
    p+xlab("")+ylab("")+
      theme(
        legend.text = element_text(size=10),
        legend.key.size = unit(sankey_legsize, 'mm'),
        legend.title=element_text(size=unit(sankey_legsize*3, 'mm'))
      )+ggtitle(sankey_title)
  }
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
      gdf<- get_mdf()
      sy<-get_sankey_Y()
      args<-list(

        gdf=gdf,
        sy=sy,
        sankey_values=input$sankey_values,
        sankey_values2=input$sankey_values2,
        sankey_palette=input$sankey_palette,
        sankey_paletteY=input$sankey_paletteY,
        sankey_palette_middle=input$sankey_palette_middle,
        sankey_size=input$sankey_size,
        sankey_showlegend=T,
        sankey_legsize=input$sankey_legsize,
        sankey_title=input$sankey_title,
        color_var=NULL,
        ggsankey_colorY='model_metric',
        var_type=NULL,
        newcolhabs=vals$newcolhabs,
        node_width=0.3,
        labels_in=F,
        hjust0=-0.085,
        hjust1=0.08,
        custom_var_labels=NULL,
        custom_y_labels=NULL,
        sankey_size2=3

      )

      p<-do.call(gg_sankey_plot,args)
      p



    })








    output$sankey_gg<-renderUI({
      validate(need(length(measures())>0,"The Measure Importance must be calculated in the '1. Measures' tab."))
      validate(need(is_installed("devtools"),"Requires DevTools package. Please close iMESc and install the package manually in R"))
      validate(need(length(get_rfs()) > 1, "You must have more than one Random Forest model saved in the Datalist."))
      validate(need(length(get_all_mdd()) > 1, "You must calculate the Measures for more than one model."))
      p<- withProgress(gg_sankey(),NA,NA,message="Creating Sankey...")
      renderPlot({
        p
      }, width=input$sankey_width,height=input$sankey_height)

    })


    get_rfs<-function(){
      data_x<-vals$cur_data_sl
      req(data_x)
      rfs<-attr(vals$saved_data[[data_x]],"rf")

      rfs
    }
    get_all_mdd<-function(){
      rfs<-get_rfs()
      md<-lapply(rfs,function(x){
        data.frame(attr(x[[1]],'mindepth')[[2]])

      })
      md<-md[sapply(md,nrow)>0]
      md

    }
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
    get_sankey_nodes<-function(){
      pal_end<-vals$newcolhabs[[input$sankey_palette]]
      pal_y<-vals$newcolhabs[[input$sankey_paletteY]]
      pal_middle<-vals$newcolhabs[[input$sankey_palette_middle]](1)
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
      color=NULL



      links$color<-color
      links
    }







    observe({
      shinyjs::toggle("sankey_palette_middle",condition=input$sankey_values2!="None")
      shinyjs::toggle("sankey_cut",condition=isTRUE(input$sankey_cuton))
    })



    output$sankey_side<-renderUI({
      md<-get_all_mdd()

      req(length(get_all_mdd())>1)

      choices<-unlist(sapply(md,colnames))
      choices<-unique(choices[!choices%in%"variable"])
      names(choices)<-NULL
      choices2<-do.call(rbind,md)$variable


      m<-get_rfs()[[1]]$m
      choices_metric<-if(m$modelType=="Classification"){
        c("Accuracy","Kappa")
      } else{
        c('Rsquared','RMSE',"MAE")
      }
      div(
        tags$label("Model"),
        div(style="padding-left: 15px",
            pickerInput_fromtop(session$ns("sankey_metric"),"Metric",choices_metric),
            pickerInput_fromtop_live(
              inputId = ns("sankey_paletteY"),
              label = "Palette:",
              selected="Grays",
              choices =vals$colors_img$val,
              choicesOpt = list(content =vals$colors_img$img)),
            #  uiOutput(ns("ggsankey_colorY")),
        ),
        tags$label("Variables"),
        div(style="padding-left: 15px",
            pickerInput_fromtop(session$ns("sankey_values"),"Value",choices,selected="times_a_root"),
            pickerInput_fromtop_live(
              inputId = ns("sankey_palette"),
              label = "Palette:",
              choices =vals$colors_img$val,

              choicesOpt = list(content =vals$colors_img$img))
        ),
        tags$label("Middle value"),
        div(style="padding-left: 15px",
            pickerInput_fromtop(session$ns("sankey_values2"),"Middle value",c("None",choices)),
            pickerInput_fromtop_live(
              inputId = ns("sankey_palette_middle"),
              label = "Palette Middle:",
              choices =vals$colors_img$val,
              selected="gray",
              choicesOpt = list(content =vals$colors_img$img))
        ),

        # checkboxInput(ns('sankey_cuton'),"Cut middle values"),
        numericInput(session$ns("sankey_cut"),"Middle breaks",10),





        div(id=ns("sankey_gg_args"),
            numericInput(ns("sankey_width"),"Plot width",480),
            numericInput(ns("sankey_height"),"Plot Height",700),
            textInput(ns("sankey_title"),"Title:", value="Sankey plot"),
            numericInput(ns("sankey_size"),"Text size",3),
            # checkboxInput(ns("sankey_showlegend"),"Show legend",T),
            numericInput(ns("sankey_legsize"),"Legend size",3)),
        pickerInput_fromtop(session$ns("sankey_variable"),"Value",choices2)





      )
    })


    output$ggsankey_colorY<-renderUI({
      pickerInput_fromtop(ns('ggsankey_colorY'),"Color by",c("model_metric","model_name"))
    })

    observe({

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
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Sankey GGplot", name_c="sankey_gg",datalist_name=datalist_name)
    })




    output$sankey_plotly<-renderUI({
      validate(need(length(get_rfs()) > 1, "You must have more than one Random Forest model saved in the Datalist."))
      validate(need(length(get_all_mdd()) > 1, "You must calculate the Measures for more than one model."))

      div(
        plotly::plotlyOutput(ns('sankey_plot'))
      )
    })





    get_ploly_sankey<-eventReactive(input$run_sankey_ploly,ignoreInit = T,{
    #  saveRDS(reactiveValuesToList(input),'input.rds')
      print("saved")
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
          customdata = as.matrix(nodes),
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
      pickerInput_fromtop_live(
        inputId = ns("mdd_palette"),
        label = "Palette:",
        choices =vals$colors_img$val,
        choicesOpt = list(content =vals$colors_img$img))
    })

    output$inter_palette<-renderUI({
      pickerInput_fromtop_live(
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
      pickerInput_fromtop_live(
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



    output$measure_models_inputs<-renderUI({
      m<-model()

      datalist<-attr(m,"Datalist")
      models<-attr(vals$saved_data[[datalist]],"rf")
      missing_md<-sapply(models,function(m) !length(attr(m$m,"mindepth"))>0)

      choices<-names(models)
      selected=choices[missing_md]
      div(style="display: flex",
          div(class="picker_open",
              virtualPicker(
                id = ns("measure_model"),
                "Models selected",
                choices=choices,

                selected=selected
              )

          ),
          div(style="padding: 25px",
              div(class="alert alert-danger",role="alert",
                  style="overflow: auto; max-height: 150px",
                  icon("triangle-exclamation"),"Warning:"
              ),
              em('Calculating RF explainer metrics for each model can be very time-consuming. Ensure you specify which models you intend to calculate the explainer measures for.')
          )
      )
    })

    modal_measures_loop<-reactive({
      modalDialog(
        title="Calculate measures for several models",
        uiOutput(ns("measure_models_inputs")),
        div(align="right",
            actionButton(ns("run_loop_measures"),"RUN>>")
        ),
        easyClose = T
      )
    })




    observeEvent(input$run_loop_measures,ignoreInit = T,{
      m<-model()

      removeModal()

      datalist<-attr(m,"Datalist")
      models<-attr(vals$saved_data[[datalist]],"rf")
      models<-models[input$measure_model]
      models<-lapply(models,function(m) m$m)
      measures<-rf_inputsmeasure()

      withProgress(min=0,max=length(models),message="Calculating...",{
        for(i in seq_along(models)) {

          model_name<-names(models)[i]
          incProgress(0,message=paste0("Calculating...",model_name))
          print(paste0(i,'/',length(models)))
          m<-models[[model_name]]
          res<-multipimp( m,measures=c(measures,'p_value','no_of_trees'), mean_sample="top_trees")
          attr(m,"mindepth")<-res
          attr(vals$saved_data[[datalist]],"rf")[[model_name]]$m<-m
          incProgress(1,message=paste0("Calculating...",model_name))
        }
      })


    })

    observeEvent(ignoreInit = T,input$run_measures,{
      if(isTRUE(input$loop_measures)){
        return(showModal(
          modal_measures_loop()
        ))
      }
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
      try({

        m<-model()
        data<-get_measure_table()
        data_x<-attr( model(),"Datalist")
        req(data_x)
        data_o<-vals$saved_data[[data_x]]


        data<-data_o[,as.character(vals$rf_sigs),drop=F]

        data<-data_migrate(data_o,data)
        print(colnames(data))


        bag<-attr(m,'model_name')
        if(is.null(bag)){
          bag<-attr(m,"model_tag")
        }
        bag<-paste0(bag,"_sig_vars")
        newnames<-make.unique(c(names(vals$saved_data),bag))
        bag<-newnames[length(newnames)]
        attr(data,"bag")<-bag
        vals$newdatalist<-data
        module_save_changes$ui(ns("explainer-create"),vals)

      })
    })
    module_save_changes$server("explainer-create",vals)
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

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=prf.reactive()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Minimal depth distribution", name_c="min-depth-plot",datalist_name=datalist_name)
    })
    observeEvent(input$downp_mrf,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=rf_multi()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Multiway Importance", name_c="multi-way-plot",datalist_name=datalist_name)
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
                     interactions_frame<-suppressWarnings(randomForestExplainer::min_depth_interactions(forest, vars, mean_sample=input$rfinter_mean_sample, uncond_mean_sample=input$uncond_mean_sample))
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

      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=inter_plot()
      datalist_name<-attr(vals$cur_caret_model,"model_name")
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Interactions Plot", name_c="interactions",datalist_name=datalist_name)
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
