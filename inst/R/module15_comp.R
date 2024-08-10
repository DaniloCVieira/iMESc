
#' @importFrom shiny NS tagList
#' @export
#'
#' @noRd

#' @export
mod_comp_plot_options<-list()
#' @export
mod_comp_plot_options$ui<-function(id){
  ns<-NS(id)
  div(
    fluidRow(


      column(
        4,
        box_caret(
          ns("box_c"),

          show_tittle = F,
          div(div(id=ns('run_plot_btn'),class="save_changes",
                  align="right",
                  actionButton(ns("run_plot"),"RUN>>",style="height: 24px;width: 60px; padding: 3px; font-size: 12px")
          ),
          div(
            uiOutput(ns("gg_metric_out")),
            numericInput(ns("gg_base_size"),"+ Base size:", value=12),
            numericInput(ns("gg_title_size"),"+ Title size:", value=11),
            numericInput(ns("gg_axis_size"),"+ Axis size:", value=11),
            numericInput(ns("gg_label_size"),"+ Label size:", value=11),
            numericInput(ns("gg_lwd_size"),"+ Line width:", value=1),
            numericInput(ns("gg_text_size"),"+ Cor Size:", value=5),
            numericInput(ns("gg_point_size"),"+ Point Size:", value=3),
            numericInput(ns("gg_fill_lighten"),"+ Lighten:", value=0.3,step=0.1),

            pickerInput_fromtop(inputId = ns("gg_palette"),
                                label = "+ Palette",
                                choices =NULL),
            pickerInput_fromtop(ns('gg_theme'),"+ Theme",choices=c('theme_grey','theme_bw','theme_linedraw','theme_light','theme_dark','theme_minimal','theme_classic','theme_void')),

            actionLink(ns('gg_download'),"Download",icon('download'))
          ))

        )
      ),
      column(
        8,class='mp0',
        uiOutput(ns('plot_output'))
      )

    )
  )
}
#' @export
mod_comp_plot_options$server<-function(id,vals,type="density"){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    output$gg_metric_out<-renderUI({
      multiple = T
      results<-vals$resample_results
      req(results)
      if(type=="ggpairs"){
        multipe=F
      }
      pickerInput_fromtop(ns("gg_metric"),"Metrics:",choices=results$metrics,multiple = multiple,selected =results$metrics )

    })
    observe({
      shinyjs::toggle("run_plot_btn",condition=length(vals$resample_results)>0)
    })
    observe({

      req(type!='ggpairs')
      shinyjs::hide('gg_point_size')
      shinyjs::hide('gg_text_size')
    })

    observeEvent(ignoreInit = T,input$gg_download,{
      vals$hand_plot<-"generic_gg"
      if(type=="ggpairs"){
        vals$hand_plot<- 'Pairs-plot'
        vals$desc_pairplot<-vals$box_metrics
      }
      module_ui_figs("downfigs")
      generic=vals$box_metrics
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message=type, name_c=paste0(vals$cur_data,"_",vals$cur_comp,type,'_'))
    })

    getsolid_col<-reactive({
      if(type=="density"){
        return(1:length(vals$newcolhabs))
      }
      res<-lapply(vals$newcolhabs, function(x) x(2))
      res1<-unlist(lapply(res, function(x) x[1]==x[2]))
      solid<-names(res1[res1==T])
      pic<-which(vals$colors_img$val%in%solid)
      pic
    })
    observeEvent(vals$newcolhabs,{
      choices =vals$colors_img$val[getsolid_col()]
      choicesOpt = list(content =vals$colors_img$img[getsolid_col()])
      if(type%in%c("dotplot","ggpairs")){
        selected="black"
      } else{
        selected=choices[1]
      }
      updatePickerInput(session,'gg_palette',choices=choices,choicesOpt=choicesOpt,selected=selected)
    })
    get_ggcompdata<-function(results,gg_metric=NULL){
      if(!inherits(results,"resamples")){
        req(inherits(results,"list"))
        results<-resamples(results)
      }
      if(is.null(gg_metric)){
        gg_metric<-results$metrics
      }

      df<-reshape::melt(data.frame(results$values),"Resample")
      df$variable<-as.character(df$variable)
      metric_model<-data.frame(do.call(rbind,sapply(df$variable,function(x) strsplit(x,"\\."))))
      colnames(metric_model)<-c("model_name","metric")
      rownames(metric_model)<-NULL
      rownames(df)<-NULL
      df_box<-cbind(df['value'],metric_model)
      df_box$model_name<-factor(df_box$model_name,rev(unique(df_box$model_name)))

      colnames(df_box)<-c("y","x","metric")
      df_box<-df_box[which(df_box$metric%in%gg_metric),]
      req(nrow(df_box)>0)
      df_box
    }

    ggbox_modelmetrics<-function(results,gg_base_size=12,
                                 gg_metric=NULL,gg_axis_size=11,gg_label_size=11,gg_lwd_size=1,
                                 gg_fill_lighten=0.3,gg_palette="turbo",gg_title_size=13,gg_theme="theme_minimal",newcolhabs=list("turbo"=viridis::turbo),type="boxplot", gg_text_size=12,gg_point_size=12) {

      fill<-newcolhabs[[gg_palette]](1)
      fill<-lighten(fill,gg_fill_lighten)
      df_box<-get_ggcompdata(results,gg_metric)


      if(type=="boxplot"){
        p<-ggplot(df_box,aes(x,y))+
          stat_boxplot(geom='errorbar', linetype=1, width=0.3, linewidth=gg_lwd_size)+
          geom_boxplot(fill=fill)+coord_flip()
      } else if(type=="dotplot"){
        df_mean<-aggregate(df_box[1],df_box[2:3],mean)
        p<-ggplot(df_box,aes(x,y))+
          stat_boxplot(geom='errorbar', linetype=1, width=0.3, linewidth=gg_lwd_size, color=gg_palette)+geom_point(data=df_mean,size=gg_base_size/4,aes(x,y), color=gg_palette)+coord_flip()
      } else if(type=="density"){
        colors<-vals$newcolhabs[[input$gg_palette]](length(unique(df_box$x)))
        colors<-lighten(colors,gg_fill_lighten)
        p<-ggplot(df_box)+geom_density(aes(y,colour =x), linewidth=gg_lwd_size)+facet_wrap(~metric,scales="free_x")+xlab("")+ylab("Value")+scale_color_manual(values=colors, name="Model")
      }
      if(type!='ggpairs'){
        p<-p+facet_wrap(~metric,scales="free_x")+xlab("")+ylab("Value")
      } else{
        df<-results$values

        df1<-df[grepl(gg_metric,colnames(df))]
        colnames(df1)<-results$models

        p<-ggpairs(df1,
                   upper = list(
                     continuous = GGally::wrap(
                       "cor",
                       size = gg_text_size,
                       color=fill
                     )
                   ),  # Adjust the size as needed
                   lower = list(
                     continuous = GGally::wrap("points", size = gg_point_size, colour = fill)
                   ),
                   diag = list(
                     continuous = GGally::wrap(
                       "densityDiag",
                       linewidth=gg_lwd_size,
                       color=fill
                     )
                   ))

      }


      p<-switch(gg_theme,
                'theme_grey'={p+theme_grey(gg_base_size)},
                'theme_bw'={p+theme_bw(gg_base_size)},
                'theme_linedraw'={p+theme_linedraw(gg_base_size)},
                'theme_light'={p+theme_light(gg_base_size)},
                'theme_dark'={p+theme_dark(gg_base_size)},
                'theme_minimal'={p+theme_minimal(gg_base_size)},
                'theme_classic'={p+theme_classic(gg_base_size)},
                'theme_void'={p+theme_void(gg_base_size)})

      p+theme(
        axis.text=element_text(size=gg_axis_size),
        axis.title=element_text(size=gg_label_size),
        strip.text = element_text(size = gg_title_size),
      )
    }

    args_plot<-reactive({
      results<-vals$resample_results
      req(!is.null(results))
      list(

        results=results,
        gg_base_size=input$gg_base_size,
        gg_title_size=input$gg_title_size,
        gg_axis_size=input$gg_axis_size,
        gg_label_size=input$gg_label_size,
        gg_lwd_size=input$gg_lwd_size,
        gg_fill_lighten=input$gg_fill_lighten,
        gg_palette=input$gg_palette,
        newcolhabs=vals$newcolhabs,
        gg_theme=input$gg_theme,
        gg_metric=input$gg_metric,
        type=type,
        gg_text_size=input$gg_text_size,
        gg_point_size=input$gg_point_size

      )
    })
    observeEvent(args_plot(),{
      shinyjs::addClass("run_plot_btn","save_changes")
    })
    get_plot<-eventReactive(input$run_plot,{

      args<-args_plot()

      vals$box_metrics<-do.call(ggbox_modelmetrics,args)
      shinyjs::removeClass("run_plot_btn","save_changes")
      vals$box_metrics


    })

    output$plot_output<-renderUI({
      validate(need(length(vals$resample_results)>0,"Error: at least two models are needed"))
      renderPlot({get_plot()})
    })

    return(NULL)
  })
}
#' @export
compare_models<-list()
#' @export
compare_models$ui<-function(id){
  ns<-NS(id)
  div(
    tags$style(HTML(".picker-tip .text-muted{color: SeaGreen; font-style: italic}
                    .virtual12 .vscomp-dropbox{font-size: 12px}")),
    box_caret(
      ns("box_setup1"),inline=F,
      color="#374061ff",
      title="Model Group Selection",
      div(style="display: flex",class="picker-tip",
          div(
            pickerInput_fromtop(ns("data_x"),span("Datalist",tipright("Select the Datalist containing saved models")), choices=NULL, options=shinyWidgets::pickerOptions(liveSearch =T))
          ),
          pickerInput_fromtop(ns("compare"),span("Available comparisons",tipright('Comparisons are only allowed for models with compatible resampling methods.')), choices=NULL,width="400px")
      )
    ),
    column(
      3,class="mp0",
      box_caret(
        ns("box_setup2"),
        title="Model selection",
        color="#c3cc74ff",
        div(
          div(class="virtual12",
              virtualPicker(ns("model_in"),"models selected")
          )

        )
      )
    ),
    column(
      9,class="mp0",
      tags$style(HTML(".small_radio .btn{
                      padding-left: 7px;
 padding-right: 7px;
                      font-size: 12px
                      }
 .radiobox_title{
 display: flex;

 margin-left: 5px;
 margin-right: 5px
 }
  .box_comp_results .box_title{
 display: flex;


  }

 .box_comp_results .radio12 .btn{
 margin-top: -6px;
 display: inline-block;
 color: #333;

 font-size: 12px;
 padding-left: 5px;
 padding-right: 5px
 }
 ")),


 div(class="box_comp_results",
     box_caret(
       ns("box_setup3"),
       title=div(class="radiobox_title",
                 "Results:",
                 div(
                   class="radio12",
                   radioGroupButtons(
                     ns("tab_results"),NULL,
                     choiceValues =paste0("tab",1:6),
                     choiceNames =c("3.1. Summary","3.2. Boxplot","3.3.densityplot","3.4. dotplot","3.5. GGpairs","3.6. Pairwise"),justified =T, width="51.5vw"
                   )
                 )
       ),
       div(

         tabsetPanel(
           id=ns("comp_results"),
           type="hidden",
           tabPanel(
             "3.1. Summary",value="tab1",
             div(
               fluidRow(   style="overflow-x: scroll",
                           column(
                             4,
                             box_caret(
                               ns("box_tab1"),
                               show_tittle = F,
                               div(
                                 pickerInput_fromtop(ns("summary_show"),"+ Show:" , choices=NULL),
                                 numericInput(ns('summary_round'),"+ Round",3),
                                 actionLink(ns('summary_download'),"Download",icon('download'))
                               )
                             )
                           ),

                           column(12,style="margin-top: 30px",
                                  uiOutput(ns('summary'))
                           )
               )
             )
           ),
           tabPanel(
             "3.2. GGplot",value="tab2",
             div(mod_comp_plot_options$ui(ns("boxplot")),
                 uiOutput(ns("box_plot")))
           ),
           tabPanel("3.3. densityplot",value="tab3",
                    mod_comp_plot_options$ui(ns("density")),
                    uiOutput(ns('density_plot'))),
           tabPanel("3.4. dotplot",value="tab4",
                    div(mod_comp_plot_options$ui(ns("dotplot")),
                        uiOutput(ns("dot_plot")))),
           tabPanel("3.5. ggpairs",value="tab5",
                    div(
                      mod_comp_plot_options$ui(ns("ggpairs")),
                      uiOutput(ns('ggpairs_plot'))
                    )),
           tabPanel("3.7. Pairwise comparisons",
                    value="tab6",
                    div(style="overflow-x: auto",
                        column(
                          4,class="mp0",
                          box_caret(
                            ns("box_tab1"),
                            show_tittle = F,
                            div(
                              pickerInput_fromtop(ns("pair_show"),"+ Show:" , choices=NULL),
                              numericInput(ns('pair_round'),"+ Round",3),
                              actionLink(ns('pair_download'),"Download",icon('download'))
                            )
                          )
                        ),
                        column(
                          8,class="mp0",
                          uiOutput(ns('plot6'))
                        )
                    ))
         ),
         uiOutput(ns("teste"))
       ),
     )
 )
    )
  )}
#' @export
compare_models$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns


    box_caret_server('box_setup1')
    box_caret_server('box_setup2')
    box_caret_server('box_setup3')





    observeEvent(input$tab_results,{
      updateTabsetPanel(session,'comp_results',input$tab_results)
    })

    observeEvent(resample_models(),{
      vals$resample_results<-resample_models()
    })


    ##box options
    output$gg_metric_out<-renderUI({
      multiple = T
      results<-resample_models()
      if(type=="ggpairs"){
        multipe=F
      }
      pickerInput_fromtop(ns("gg_metric"),"Metrics:",choices=results$metrics,multiple = multiple,selected =results$metrics )

    })

    ## tab1
    summary_models<-reactive({
      summary(resample_models())
    })
    observeEvent(resample_models(),{
      choices=names(summary_models()$statistics)
      updatePickerInput(session,"summary_show",choices=choices)
    })
    output$summary<-renderUI({
      req(summary_models())
      req(input$summary_show)%in%names(summary_models()$statistics)
      div(class="half-drop-inline",

          fixed_dt(summary_models()$statistics[[input$summary_show]])
      )
    })

    observeEvent(ignoreInit = T,input$summary_download,{
      vals$hand_down<-"generic"
      req(input$summary_show)
      req(summary_models())
      module_ui_downcenter("downcenter")
      name<-"summary_model_compare"
      data<-data.frame(summary_models()$statistics[[input$summary_show]])
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Summary comparisons",data=data, name=name)
    })
    ##tab2

    output$box_plot<-renderUI({

      mod_comp_plot_options$server('boxplot',vals,type="boxplot")

    })


    ##tab3
    output$density_plot<-renderUI({

      mod_comp_plot_options$server('density',vals,type="density")

    })


    output$dot_plot<-renderUI({
      div(
        mod_comp_plot_options$server('dotplot',vals,type="dotplot")

      )
    })
    output$ggpairs_plot<-renderUI({
      results<-resample_models()
      req(!is.null(results))
      div(
        mod_comp_plot_options$server('ggpairs',vals,type="ggpairs")

      )
    })



    output$plot6<-renderUI({
      req(input$pair_show)
      results<-resample_models()

      req(!is.null(results))
      diffs<-try(diff(results))
      if(inherits(diffs,"try-error")){
        return(emgray('not enough resampling observations'))
      }
      reprint<-re<-summary(diffs)
      reprint$call<-NULL
      reprint$table<-NULL
      reprint<- capture.output(reprint)
      res<-apply(data.frame(re$table[[input$pair_show]]),2,function(x)as.numeric(x))
      colnames(res)<-colnames(re$table[[input$pair_show]])
      rownames(res)<-rownames(re$table[[input$pair_show]])
      vals$getsummary_comp<-data.frame(res)
      res<-round(vals$getsummary_comp, input$pair_round)
      div(class="half-drop-inline",
          fixed_dt(res,scrollY = "200px"),
          lapply(reprint[ which(!reprint%in%c("Call:","","NULL"))],function(x) div(em(x)))
      )
    })



    observeEvent(ignoreInit = T,input$pair_download,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-"Pairwise_model_compare"
      data<-vals$getsummary_comp
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Pair-wise model comparisons",data=data, name=name)
    })



    #tab6
    observeEvent(resample_models(),{
      choices=names(summary_models()$statistics)
      updatePickerInput(session,"pair_show",choices=choices)
    })

    get_modelist<-reactive({
      req(input$compare)
      model_boxes<-get_modelist_boxes()
      req(model_boxes)
      boxlist<-model_boxes$model_boxlist
      req(input$compare%in%names(boxlist))
      box_selected<-boxlist[[input$compare]]
      req(input$model_in%in%names(box_selected))
      box_selected[input$model_in]
    })

    resample_models<-reactive({
      model_list<-get_modelist()
      names(model_list)<-gsub(" ","",gsub("[^0-9A-Za-z///' ]", ".", names(model_list), ignore.case = TRUE)) # no libraries needed
      if(!length(model_list)>1){
        vals$resample_results<-NULL
      }
      validate(need(length(model_list)>1,"Error: at least two models are needed"))

      results<-try({
        resamples(model_list)
      },silent=T)
      if(!inherits(results,"try-error")){

        results
      } else{
        NULL
      }
    })
    observeEvent(vals$saved_data,{
      names(available_models)<-available_models
      choices<-sapply(vals$saved_data,function(x){
        sum(sapply(available_models,function(model){
          length(attr(x,model))
        }))
      })
      subtext<-paste0("(",choices[choices>0]," models)")

      choices<-names(choices[choices>0])
      updatePickerInput(
        session,"data_x",
        choices=choices,
        selected=vals$cur_data,
        choicesOpt =list(subtext =subtext),
        options=shinyWidgets::pickerOptions(showSubtext =T)
      )
    })
    observeEvent(input$data_x,{
      vals$cur_data<-input$data_x
    })

    observeEvent(input$compare,{
      choices<-get_modelist_boxes()$model_boxes[[input$compare]]$model_name

      shinyWidgets::updateVirtualSelect(
        'model_in',choices=choices,selected=choices
      )
    })

    data_x<-reactive({
      req(input$data_x%in%names(vals$saved_data))
      vals$saved_data[[input$data_x]]
    })
    #observe(print(get_modelist_boxes()$choices_names))


    observeEvent(get_modelist_boxes(),{
      choices= get_modelist_boxes()$choices_values
      names(choices)<-get_modelist_boxes()$choices_names
      subtext<-get_modelist_boxes()$subtext


      updatePickerInput(
        session,"compare",
        choices=choices,
        choicesOpt =list(subtext =subtext),
        options=shinyWidgets::pickerOptions(showSubtext =T)
      )
    })

    observeEvent(input$compare,{
      vals$cur_comp<-input$compare
    })

    get_modelist_boxes<-reactive({
      data<-data_x()
      choices<-lapply(available_models,function(x){
        if(check_model(data,x))x})
      choices<-choices[sapply(choices,length)>0]
      validate(need(length(choices)>0,"No models saved in selected datalist"))
      names(choices)<-choices
      limodels<-lapply(choices,function(x) {
        attr(data,x)})
      names0<-unlist(lapply(limodels,names))
      model_names<-sapply(limodels,names)
      model_attr<-names(limodels)

      resmethos<-lapply(names(limodels),function(att) {
        x<-limodels[[att]]
        res<-lapply(names(x),function(model_name){
          model<-x[[model_name]][[1]]
          req(model)
          y<-attr(model,"supervisor")
          model_type=model$modelType
          control_method=model$control$method
          control_number=model$control$number
          control_repeats<-model$control$repeats
          model_name<-model_name
          data.frame(
            model_name,model_type,control_method,control_number,control_repeats,
            id_label=paste(model_type,"models"),
            subtext=paste("(Y:",y,";Resamling:",paste0(control_number,"-",control_method,";"),"Repeats:",paste0(control_repeats,")") ),
            id_value=paste0(model_type,y,control_number,control_method,control_repeats )
          )
        })
        names(res)<-names(x)
        do.call(rbind,res)
      })
      restable<-do.call(rbind,resmethos)
      restable<-cbind(id=1:nrow(restable),restable)
      modelist<-lapply(limodels,function(x) {
        res<-lapply(names(x),function(model_name){
          model<-x[[model_name]][[1]]
          model
        })
      })
      model_names<-unlist(sapply(limodels,names))
      modelist<-unlist(modelist,recursive = F)
      model_boxes<-split(restable,as.factor(restable$id_value))

      model_names_boxes<-split(model_names,as.factor(restable$id_value))
      model_names_boxes<-lapply(model_names_boxes,function(x){
        make.unique(as.character(x))
      })
      model_boxlist<-split(modelist,as.factor(restable$id_value))

      lapply(model_boxlist,names)
      i=1
      model_boxlist<-lapply(seq_along(model_boxlist),function(i){
        names(model_boxlist[[i]])<-model_names_boxes[[i]]
        model_boxlist[[i]]
      })

      names(model_boxlist)<-names(model_boxes)
      choices_values<-names(model_boxes)
      choices_names<-as.character(sapply(model_boxes,function(x) x$id_label[1]))
      subtext<-as.character(sapply(model_boxes,function(x) x$subtext[1]))

      result_list<-list(choices_values=choices_values,choices_names=choices_names,model_boxlist=model_boxlist,model_boxes=model_boxes,subtext=subtext,model_names_boxes=model_names_boxes)
      return(result_list)
    })




  })
}

