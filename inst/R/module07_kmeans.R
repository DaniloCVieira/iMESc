#' @export
k_means_module<-list()
#' @export
k_means_module$ui<-function(id){
  ns<-NS(id)
  div(
    box_caret(
      ns("box_setup"),
      title="Model setup",
      color="#374061ff",
      inline=F,
      div(
        div(style="display: flex;column-gap: 20px;flex-flow: wrap  ;flex-grow: 4;",class="setup_box setup_top half-drop-inline;",
            div(style="display: flex",class="setup_box",
                div(
                  style="display: flex;",
                  div(class="setup_box picker-flex picker-before-x",pickerInput_fromtop(ns("data_kmeans"),"~ Training Datalist", choices=NULL))
                )
            ),
            uiOutput(ns('model_or_data')),
            selectInput(ns("som_kmeans"),"Som codebook:",NULL),
            numericInput(ns('km_centers'),span("centers",tiphelp("the number of clusters: a random set of (distinct) rows in data is chosen as the initial centres")), value=5, step=1,width="100px"),
            numericInput(ns('km_itermax'),span("iter.max",tiphelp("the maximum number of iterations allowed")),  value=10,step=1,width="100px"),
            numericInput(ns('km_nstart'),span("nstart",tiphelp("if centers is a number, how many random sets should be chosen?")), value=1, step=1,width="100px"),
        ),
        div(style="display: flex;column-gap: 20px;flex-flow: wrap  ;flex-grow: 4;",class="setup_box setup_top half-drop-inline;",
            selectInput(ns('km_alg'),'Algorithm:',c("Hartigan-Wong", "Forgy/Lloyd","MacQueen"),width="140px"),
            numericInput(ns("kmeans_seed"),"Seed",value=NA,width="100px"),
            div(style="margin-top:20px",
                actionButton(ns("kmeans_run"),"RUN >>")),
            uiOutput(ns("kmeans_models_out")),
            div(style="margin-top:20px",
                actionButton(ns("trash_kmeans"),icon("far fa-trash-alt"))
            ),
            div(
              style="margin-top: 20px;display: flex",
              div(class="save_changes",
                  tipify(actionButton(ns("save_kmeans_models"), icon("fas fa-save"),style="margin-right: 10px"),"Save K-Means Model in the training Datalist X")
              )
            )

        )
      )
    ),

    tabsetPanel(
      id=ns("kmeans_tab"),
      tabPanel(
        "1. K-means clustering", value="kmeans_tab1",
        div(
          column(
            4,class='mp0',style="overflow: auto; height: calc(100vc - 200px)",

            div(
              box_caret(ns("box_a"),
                        title="Options",
                        color="#c3cc74ff",
                        div(
                          div(id="save_kmeansbtn",
                              uiOutput(ns("save_kmeans"))
                          ),
                          div(id=ns("options_plotdata"),

                              radioButtons(ns("dot_label_clus"), 'Display', choices=c("symbols","labels"), inline=T),
                              checkboxInput(ns("hc_sort"),span("Sort clusters",tiphelp("Sort clusters by a  variable")),value=F),
                              div(style="margin-left: 15px",
                                  pickerInput(ns("hc_ord_datalist"),div("Datalist:"),NULL) ,
                                  pickerInput(ns("hc_ord_factor"),div("Variable:"),choices=NULL)),
                              pickerInput(ns("psom_data_palette"),label='Palette',choices=NULL),

                              pickerInput(ns("psom_factors"),'Labels',choices=NULL),
                              pickerInput(inputId=ns("psom_facpalette"),
                                          label =span(tiphelp('Symbol colors. Choose a gradient to color observations by a factor'),"Obs color"),
                                          choices=NULL),
                              numericInput(ns("psom_bgalpha"),span(tiphelp("Transparency"),"Alpha"),value =0.5,min=0,max=1,step=.1),
                              numericInput(ns("psom_symbol_size"),span(tiphelp("symbol shape"),"symbol size"),value=1,min=0.1,max=3,step=.1),
                              pickerInput(inputId=ns("psom_symbol"),label=span(tiphelp("symbol shape"),"Shape"),NULL),
                              div(id=ns('psom_legcontrol'),
                                  numericInput(ns("psom_insertx"),span(tiphelp("legend position relative to the x location"),"+ leg x:"),value=0,step=0.05),
                                  numericInput(ns("psom_inserty"),span(tiphelp("legend position relative to the y location"),"+ leg y:"),value=0.4,step=0.05),
                                  numericInput(ns("psom_ncol"),span(tiphelp("the number of columns in which to set the legend items"),"+ ncol leg:"),value=1,step=1),
                                  numericInput(ns("psom_bgleg"),span(tiphelp("Legend background transparency"),"+ bg leg:"),value=0.85,step=0.05, max=1)
                              )


                          ),
                          actionLink(ns('psom_create_codebook'),"+ Create Datalist with the Codebook and kmeans class"),
                          uiOutput(ns("cluster_print")),
                          #  uiOutput(ns('side_som')),
                          uiOutput(ns("kmeans_down_model")))
              )
            ),
            div(id=ns("options_plotsom"),

                box_caret(
                  ns("box4_a"),
                  title="Background",
                  color="#c3cc74ff",
                  div(

                    pickerInput(ns("bg_palette"),label ="Background palette",NULL),
                    numericInput(ns("pcodes_bgalpha"),"Background lightness",value = 0,min = 0,max = 1,step = .1),
                    pickerInput(ns("pclus_border"),label ='Border:',choices = NULL),
                  )),
                box_caret(
                  ns("box4_points"),
                  color="#c3cc74ff",
                  title=span(style="display: inline-block",
                             class="checktitle",
                             checkboxInput(ns("pclus_addpoints"),"Points",value=T,width="80px")
                  ),
                  div(id=ns("pclus_points_inputs"),
                      pickerInput(inputId = ns("pclus_points_palette"),label ="Palette",choices =NULL),
                      pickerInput(ns("pclus_points_factor"),"Factor",
                                  choices = NULL),
                      tags$div(id=ns("color_factor"),
                               class="form-group shiny-input-container",
                               tags$label(class = "control-label", " + Factor"),
                               tags$div(class="dummy-input",
                                        "Choose a gradient palette for adding a factor",style="color: gray"
                               )
                      ),

                      pickerInput(inputId = ns("pclus_symbol"),label = "Shape",choices=NULL),
                      numericInput(ns("pclus_points_size"),"Size",value = 1,min = 0.1,max = 3,step = .1))
                ),
                box_caret(
                  ns("box4_text"),
                  color="#c3cc74ff",
                  title=span(style="display: inline-block",
                             class="checktitle",
                             checkboxInput(ns("pclus_addtext"),"Labels",value=F,width="80px")
                  ),
                  div(id=ns('pclus_addtext_out'),
                      pickerInput(inputId = ns("pclus_text_palette"),label ="Palette",NULL),
                      pickerInput(ns("pclus_text_factor"),"Factor",choices = NULL),
                      numericInput(ns("pclus_text_size"),"Size",value = 1,min = 0.1,max = 3,step = .1)
                  )),
                box_caret(
                  ns("box4_vfm"),
                  color="#c3cc74ff",
                  title=span(style="display: inline-block",
                             class="checktitle",
                             tip=actionLink(ns("varfacmap"), tipright("Click for more details")),
                             checkboxInput(ns("varfacmap_action"),span("Variable factor map"),value =T,width="150px")
                  ),
                  div(id=ns('varfac_out'),
                      pickerInput(ns("vfm_type"),"Show correlation:",choices =list("Highest"='var', "Clockwise"="cor","Cluster"="cor_hc")),

                      numericInput(ns("npic"), span(tiphelp("Number of variables to display"),"Number"), value = 10, min = 2),
                      numericInput(ns("pclus.cex.var"), "Var size", value = 1, min = 2),
                      div(class="palette",
                          pickerInput(ns("p.clus.col.text"),label = "Var text color",choices =NULL )),
                      pickerInput(ns("var_bg"),label = "Var background",choices = NULL),
                      numericInput(ns("var_bg_transp"), "Var transparency", value = 0, min = 2))
                ),
                box_caret(
                  ns("box4_more"),
                  title = "General options",
                  color="#c3cc74ff",
                  div(
                    numericInput(ns("base_size"),"Base size",value = 12),
                    textInput(ns("hcs_title"), "Title: ", ""),
                    checkboxInput(ns("hcs_theme"),label = "show neuron coordinates",value = F),
                    # div(actionLink(ns('create_codebook'),"Create Datalist with the Codebook and HC class")),
                    #  div(tipify(downloadLink(ns('down_hc_model'),"Download HC model", style="button_active"),"Download file as .rds")),

                  )
                )

            )
          ),
          column(
            8,class="mp0",
            box_caret(ns("box_b"),
                      title="Plot",
                      button_title =   actionLink(ns("downplot_kmeans"),"Download plot",icon("download")),
                      div(
                        uiOutput(ns('pdata_out')),
                        uiOutput(ns('psom_out')),
                        inline( DT::dataTableOutput(ns("k_mean_res")))
                      ))


          )
        )
      ),
      tabPanel("2. Optimal number of clusters",value='kmeans_tab2',
               uiOutput(ns("kmeans_tab2_out")))
    )
  )

}
#' @export
k_means_module$server<-function (id,vals){

  moduleServer(id,function(input, output, session){
    newcolhabs<-vals$newcolhabs
    ns<-session$ns
    ns_kmeans<-NS('kmeans')



    output$cluster_print<-renderUI({
      data<-vals$saved_data[[input$data_kmeans]]
      factors<-attr(data,"factors")
      div(
        renderPrint(str(factors))
      )
    })



    box_caret_server("box_setup")
    box_caret_server("box_a")
    box_caret_server("box4_a")
    box_caret_server("box4_points")
    box_caret_server("box4_text")
    box_caret_server("box4_vfm")
    box_caret_server("box4_more")
    box_caret_server("box_b")




    observe({
      shinyjs::toggle("options_plotsom",condition =input$model_or_data=="som codebook")
      shinyjs::toggle("options_plotdata",condition =input$model_or_data=="data")
    })

    getgrad_col<-reactive({
      res<-lapply(vals$newcolhabs, function(x) x(2))
      res1<-unlist(lapply(res, function(x) x[1]==x[2]))
      grad<-names(res1[res1==F])
      pic<-which(vals$colors_img$val%in%grad)
      pic
    })
    getsolid_col<-reactive({
      res<-lapply(vals$newcolhabs, function(x) x(10))
      res1<-unlist(lapply(res, function(x) x[1]==x[2]))
      solid<-names(res1[res1==T])
      pic<-which(vals$colors_img$val%in%solid)
      pic
    })
    observe({
      shinyjs::toggle("pclus_points_inputs",condition=isTRUE(input$pclus_addpoints))
      shinyjs::toggle("pclus_addtext_out", condition = isTRUE(input$pclus_addtext))
      shinyjs::toggle("varfac_out",condition = isTRUE(input$varfacmap_action))
    })
    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'p.clus.col.text',
                        choices=vals$colors_img$val[getsolid_col()],
                        choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),selected="black")
      updatePickerInput(session,'var_bg',choices=vals$colors_img$val[getsolid_col()],choicesOpt=list(content=vals$colors_img$img[getsolid_col()]),selected="white")
    })

    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,'pclus_points_palette',
                        choices = vals$colors_img$val,
                        choicesOpt=list(content=vals$colors_img$img),
                        selected='black'
      )
      updatePickerInput(session,'pclus_text_palette',
                        choices =  vals$colors_img$val[getsolid_col()],
                        selected="black",
                        choicesOpt = list(
                          content =  vals$colors_img$img[getsolid_col()] )
      )


    })

    getsom_model<-reactive(({
      data=vals$saved_data[[input$data_kmeans]]
      m<-attr(data,"som")[[input$som_kmeans]]
      m
    }))

    get_copoints<-reactive({
      data=vals$saved_data[[input$data_kmeans]]
      m<-attr(data,"som")[[input$som_kmeans]]
      copoints<-getcopoints(m)
      copoints
    })
    points_to_map<-reactive({
      rescale_copoints(hexs=get_network(),copoints=get_copoints())
    })
    copoints_scaled<-reactive({
      points_tomap=points_to_map()
      data<-vals$saved_data[[input$data_kmeans]]
      factors<-attr(data,"factors")
      if(length(input$pclus_text_factor)>0){
        if(input$pclus_text_factor%in%colnames(factors)){
          text_factor= factors[rownames(data),input$pclus_text_factor, drop=F]
          points_tomap$label<-text_factor[rownames(points_tomap),]
        }
      }

      if(length(input$pclus_points_factor)>0){
        if(input$pclus_points_factor%in%colnames(factors)){
          points_factor= factors[rownames(data),input$pclus_points_factor, drop=F]
          points_tomap$point<-points_factor[rownames(points_tomap),]
          attr(points_tomap,"namepoints")<-input$pclus_points_factor
        }
      }

      points_tomap
    })
    get_network<-reactive({
      backtype=NULL
      property=NULL
      m<-getsom_model()
      hc<-phc()$som.hc
      hexs<-get_neurons(m,background_type="hc",property=NULL,hc=hc)
      hexs
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
      data=vals$saved_data[[input$data_kmeans]]
      m<-attr(data,"som")[[input$som_kmeans]]
      iind=indicate_hc()
      hc=phc()$som.hc
      bp<-getbp_som2(m=m,indicate=iind$indicate,npic=iind$npic,hc=hc)
      bp
    })
    argsplot_somplot<-reactive({

      data=vals$saved_data[[input$data_kmeans]]
      m<-attr(data,"som")[[input$som_kmeans]]
      #  points_tomap=copoints_scaled()
      bp=bp_som()
      req(input$pclus_points_palette)
      req(input$pcodes_bgalpha)
      indicate=indicate_hc()
      errors=NULL
      args<-list(
        m=m,
        points_tomap=copoints_scaled(),
        hexs=get_network(),
        bp=bp_som(),
        points=input$pclus_addpoints,
        points_size=input$pclus_points_size,
        points_palette=input$pclus_points_palette,
        pch=as.numeric(input$pclus_symbol),
        text=input$pclus_addtext,
        text_size=input$pclus_text_size,
        text_palette=input$pclus_text_palette,
        bg_palette=input$bg_palette,
        newcolhabs=vals$newcolhabs,
        bgalpha=input$pcodes_bgalpha,
        border=input$pclus_border,
        indicate=indicate$indicate,
        cex.var=as.numeric(input$pclus.cex.var),
        col.text=input$p.clus.col.text,
        col.bg.var=input$var_bg,
        col.bg.var.alpha=1-input$var_bg_transp,
        show_error=errors,
        base_size=input$base_size,
        show_neucoords=input$hcs_theme,
        newdata=input$newdata_hc,
        title=input$hcs_title
      )

      args

    })
    output$psom_out<-renderUI({
      req(input$data_kmeans)
      req(input$model_or_data=="som codebook")
      renderPlot({

        #vals<-readRDS("savepoint.rds")
        # vals<-readRDS("savepoint_kmeans.rds")
        # input<-readRDS("input_kmeans.rds")
        args<-argsplot_somplot()
        req(length(args)>0)
        p<-do.call(bmu_plot,args)
        data=vals$saved_data[[input$data_kmeans]]
        m<-attr(data,"som")[[input$som_kmeans]]
        hc=phc()$som.hc

        newhc<-get_neurons_hc(m,background_type="hc",property=NULL,hc=hc)

        he<-do.call(rbind,args$hexs)
        req(nrow(he)==length(factor(newhc)))
        he$group<-factor(newhc)
        bg_color<-args$newcolhabs[[args$bg_palette]](nlevels(newhc))
        bg_color<-lighten(bg_color,args$bgalpha)
        p2<-p+ggnewscale::new_scale_fill()+geom_polygon(data=he, mapping=aes(x=x, y=y, group=neu, fill=group), col=args$border)+scale_fill_manual(name="",values=bg_color,labels=levels(newhc))
        p2$layers[[1]]<-NULL
        n<-length(p2$layers)
        p2$layers<-p2$layers[c(n,c(1:n)[-n])]
        p<-p2


        vals$psom_plot<-p
        vals$psom_plot


      })


    })



    observeEvent(input$data_kmeans,{
      data=vals$saved_data[[input$data_kmeans]]
      choices<-colnames(attr(data,"factors"))
      updatePickerInput(session,'pclus_text_factor',choices=choices)
      updatePickerInput(session,'pclus_points_factor',choices=choices)
    })
    observeEvent(input$pclus_points_palette,{
      cols<-vals$newcolhabs[[input$pclus_points_palette]](8)
      shinyjs::toggle('pclus_points_factor',condition=cols[1]!=cols[2])
      shinyjs::toggle('color_factor',condition=cols[1]==cols[2])
    })
    observeEvent(df_symbol$val,{
      updatePickerInput(session,'pclus_symbol',choices = df_symbol$val,
                        choicesOpt = list(content = df_symbol$img))
    })
    observeEvent(vals$newcolhabs,{
      choices =  vals$colors_img$val[getgrad_col()]
      choicesOpt = list(content =  vals$colors_img$img[getgrad_col()] )
      updatePickerInput(session,'bg_palette',
                        choices=choices,
                        choicesOpt=choicesOpt
      )
      choices =  vals$colors_img$val[getsolid_col()]
      choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] )
      updatePickerInput(session,'pclus_border',
                        choices=choices,
                        choicesOpt=choicesOpt,
                        selected="white"
      )
    })
    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected<-get_selected_from_choices(vals$cur_data,choices)
      updatePickerInput(session,'data_kmeans',choices=choices,selected=selected)
    })

    symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
    df_symbol<-data.frame(val=c(16,15,17,18,8,1,5,3))
    for(i in 1:length(symbols)) {
      symbol1<-base64enc::dataURI(file=paste0('inst/www/pch',i,".png"), mime="image/png")
      df_symbol$img[i]<- sprintf(paste0(img(src=symbol1, width='10')))}
    observeEvent(ignoreInit=T,input$teste_comb,{
      savereac()
    })



    observe({
      shinyjs::toggle("hc_ord_datalist",condition = isTRUE(input$hc_sort))
    })


    observeEvent(getdata_for_hc(),{
      choices=c(names(vals$saved_data[getdata_for_hc()]))
      selected=vals$hc_ord_datalist
      updatePickerInput(session,'hc_ord_datalist',choices=choices,selected=selected)
    })

    observeEvent(input$hc_ord_datalist,{
      data<-vals$saved_data[[input$hc_ord_datalist]]
      choices=c(colnames(data))
      selected=vals$hc_ord_factor
      updatePickerInput(session,'hc_ord_factor',choices = choices,selected=selected)
    })
    observe({
      shinyjs::toggle("hc_ord_factor",condition=isTRUE(input$hc_sort))
    })

    getdata_for_hc<-reactive({
      req(input$data_kmeans)
      data<-vals$saved_data[[input$data_kmeans]]
      datalist<-vals$saved_data

      req(length(data)>0)
      res0<-unlist(
        lapply(datalist, function (x){
          sum(rownames(x)%in%rownames(data))==nrow(x)
        })
      )
      names(res0[res0==T])
    })
    observeEvent(ignoreInit=T,input$hc_ord_factor,{
      vals$hc_ord_factor<-input$hc_ord_factor
    })
    observeEvent(ignoreInit=T,input$hc_ord_datalist,{
      vals$hc_ord_datalist<-input$hc_ord_datalist
    })
    observeEvent(ignoreInit=T,input$hc_sort,{
      vals$hc_sort<-input$hc_sort
    })





    observeEvent(ignoreInit=T,input$kmeans_models,{

      req(input$data_kmeans)
      req(input$kmeans_models!="new kmeans (unsaved)")
      models<-attr(vals$saved_data[[input$data_kmeans]],"kmeans")

      data<-vals$saved_data[[input$data_kmeans]]
      vals$k_means_results<-getkmodel()
      vals$update_kmeans<-NULL
    })
    observe({
      req(input$kmeans_models)
      req(input$model_or_data)
      target<-if(input$kmeans_models!="new kmeans (unsaved)")    {
        attr(getkmodel(),"target")
      } else{input$model_or_data}
      req(length(target)>0)

      if(target=="som codebook"){
        shinyjs::show('side_som')
        shinyjs::hide('psom_side0')
      }
      if(target=="data")
      {
        shinyjs::hide('side_som')
        shinyjs::show('psom_side0')
      }
    })

    getkmodel<-reactive({

      req(input$data_kmeans)
      req(input$kmeans_models)
      models<-attr(vals$saved_data[[input$data_kmeans]],"kmeans")
      kmodel<- models[[input$kmeans_models]]

      kmodel
    })
    get_hc<-reactive({
      req(input$data_kmeans)
      req(!is.null(getkmodel()))
      k_means_results<-getkmodel()
      hc=as.factor(k_means_results[[1]][,1])
      data=vals$saved_data[[input$data_kmeans]]
      if(input$model_or_data=="som codebook"){
        req(input$som_kmeans)
        m<-attr(data,"som")[[input$som_kmeans]]
        names(hc)<-1:nrow(m$grid$pts)
        somC<-list()
        somC$somC<-getnewclass()
        somC$som.hc<-hc
        somC
      } else{
        somC<-list()
        somC$somC<-getnewclass()
        req(length(rownames(data))==length(somC$somC))
        names(somC$somC)<-rownames(data)
        somC$som.hc<-getnewclass()
        names(somC$som.hc)<-rownames(data)
        somC
      }
    })
    hc_newlevels<-reactive({
      req(input$hc_ord_datalist)
      req(input$hc_ord_factor)
      req(input$data_kmeans)
      data_o<-vals$saved_data[[input$data_kmeans]]
      data<-vals$saved_data[[input$hc_ord_datalist]]
      hc<-get_hc()
      fac<-data[names(hc$somC),input$hc_ord_factor,drop=F]
      validate(need(sum(rownames(data_o)%in%rownames(data))==nrow(data_o),"The IDs of the sorted data chosen do not match those of the training data."))
      clusters<-hc$somC
      newlevels<-names(sort(tapply(fac[,1],as.numeric(clusters),mean)))
      newlevels
    })
    getnewclass<-reactive({
      req(input$data_kmeans)
      temp<-getkmodel()[[1]][,1]
      if(input$model_or_data=="som codebook"){
        data=vals$saved_data[[input$data_kmeans]]
        m<-attr(data,"som")[[input$som_kmeans]]
        newclass<-m$unit.classif
        fac<-temp
        for(i in 1:length(fac)) {newclass[newclass==i]<-rep(fac[i],sum(  newclass==i))}
        names(newclass)<-rownames(data)
        temp<-newclass
      }
      temp<-factor(temp)

      temp
    })
    phc<-reactive({
      req(length(input$hc_sort)>0)
      hc<-get_hc()
      if(isFALSE(input$hc_sort)){
        vals$kmeans_clusters<-hc$somC

      } else{
        som.hc_names<-names(hc$som.hc)
        somC_names<-names(hc$somC)
        newlevels<-hc_newlevels()
        hc$som.hc<-factor(hc$som.hc,levels=newlevels,labels=1:length(newlevels))
        hc$somC<-factor(hc$somC,levels=newlevels,labels=1:length(newlevels))
        hc$som.hc<-hc$som.hc[som.hc_names]
        hc$somC<-hc$somC[somC_names]
        vals$kmeans_clusters<-hc$somC

      }
      hc
    })





    output$side_som<-renderUI({
      tagList(module_ui_somplot(ns("kmeans")))
    })



    output$model_or_data<-renderUI({
      ns<-session$ns
      choices =choices_kmeans()
      selected<-get_selected_from_choices(vals$model_or_data,choices)
      radioButtons(ns("model_or_data"), "Clustering target:", choices=choices,selected=selected,width="150px")
    })




    observeEvent(ignoreInit=T,input$model_or_data,{
      vals$model_or_data<-input$model_or_data
    })
    choices_kmeans_names<-reactive({
      req(input$data_kmeans)
      a<-if (length(   names(vals$saved_data) > 0)) {
        "Numeric-Attribute"
      } else {
        NULL
      }

      b<-   if(length(attr(vals$saved_data[[input$data_kmeans]],"som"))>0){"SOM-codebook"}else{

        NULL}
      res<-c(a, b)
      res
    })
    choices_kmeans<-reactive({
      req(input$data_kmeans)
      choices=c('Numeric-Attribute'="data",'SOM codebook'='som codebook')
      if(!length(attr(vals$saved_data[[input$data_kmeans]],"som"))>0){

        choices<-choices[1]
      }
      choices
    })



    observe({
      req(input$data_kmeans)
      shinyjs::toggle("trash_kmeans",condition= length(attr(vals$saved_data[[input$data_kmeans]],"kmeans"))>0)
    })



    observeEvent(ignoreInit=T,input$trash_kmeans,{
      attr(vals$saved_data[[input$data_kmeans]],"kmeans")[[input$kmeans_models]]<-NULL
    })
    observeEvent(ignoreInit=T,input$kmeans_models,{
      vals$kmeans_models<-input$kmeans_models
    })


    observe({
      if(!is.null(vals$k_means_results)|length(attr(data,"kmeans"))>0){
        shinyjs::show('kmeans_models')
      } else{
        shinyjs::hide('kmeans_models')
      }
    })

    output$kmeans_models_out<-renderUI({
      req(input$data_kmeans)
      ns<-session$ns
      data<-vals$saved_data[[input$data_kmeans]]
      choices<-names(attr(data,"kmeans"))
      selectInput(ns("kmeans_models"),"Saved models",choices=choices,selected=vals$kmeans_models)
    })

    observe({
      req(input$data_kmeans)
      data<-vals$saved_data[[input$data_kmeans]]
      choices<-names(attr(data,"kmeans"))
      condition=length(choices)>0&input$kmeans_tab=='kmeans_tab1'
      shinyjs::toggle("kmeans_models",condition=condition)
    })





    observe({
      shinyjs::toggle('save_kmeans_models',condition=input$kmeans_models%in%"new kmeans (unsaved)")
    })






    observeEvent(ignoreInit=T,input$save_kmeans_models,{
      vals$hand_save<-"Save K-means Model"
      vals$hand_save2<-NULL
      vals$hand_save3<-NULL
      showModal(module_kmeans())

    })

    savekmeans_models<-reactive({
      attr(vals$saved_data[[input$data_kmeans]],"kmeans")[["new kmeans (unsaved)"]]<-NULL

      if(input$hand_save=="create"){
        attr(vals$saved_data[[input$data_kmeans]],"kmeans")[[input$newdatalist]]<-vals$k_means_results
        cur_kmeans_models<-input$newdatalist
      } else{
        attr(vals$saved_data[[input$data_kmeans]],"kmeans")[[input$over_datalist]]<-vals$k_means_results
        cur_kmeans_models<-input$over_datalist
      }
      vals$kmeans_models<-cur_kmeans_models


    })


    observe({
      shinyjs::toggle("kmeans_run", condition=input$kmeans_tab=="kmeans_tab1")
    })



    observeEvent(ignoreInit=T,input$kmeans_tab,{
      vals$kmeans_tab<-input$kmeans_tab
    })


    output$kmeans_tab2_out<-renderUI({

      div(
        column(
          4,class='mp0',
          box_caret(
            ns("box2"),
            title="Analysis options",
            color="#c3cc74ff",
            div(

              pickerInput(ns("kmeans_oc"),"Method",c("Elbow","Gap Statistic","Silhouette")),
              numericInput(ns("k.max"),span("K.max:",tiphelp("the maximum number of clusters to consider, must be at least two","right")),24,step=1,width='100px'),
              numericInput(ns("oc_boot"),span("oc_boot:", tiphelp("number of Monte Carlo (bootstrap) samples","right")),50,step=1),
              actionButton(ns("oc_run"),"RUN >>"),


              uiOutput(ns("oc_gap")),
              uiOutput(ns("oc_sil")),
              uiOutput(ns("oc_ss"))
            ),


          )
        ),
        column(
          8,class="mp0",
          box_caret(
            ns("box2b"),
            title="Plot",
            plotOutput(ns("oc_elbow_out"))
          )
        )
      )
    })

    output$oc_elbow_out<-renderPlot({get_elbow()})
    get_elbow<-eventReactive(input$oc_run,{
      req(input$kmeans_oc=='Elbow')
      p<-fviz_nbclust(getdata_kmeans(), kmeans, method="wss", k.max=input$k.max,iter.max=input$km_itermax, nstart=input$km_nstart,algorithm=km_alg()) + theme_minimal() + ggtitle("the Elbow Method")
      p

    })





    observeEvent(ignoreInit=T,input$k.max,{
      vals$k.max<-input$k.max
    })




    observe({
      shinyjs::toggle("oc_boot",condition=input$kmeans_oc=='Gap Statistic')
    })






    km_alg<-reactive({
      km_alg<-input$km_alg
      if(km_alg=="Forgy/Lloyd"){km_alg='Lloyd'}
      km_alg
    })

    observeEvent(ignoreInit=T,input$oc_run,{
      req(input$kmeans_oc=='Silhouette')
      output$oc_elbow_out<-renderUI({
        req(input$k.max)
        renderPlot({
          p<-fviz_nbclust(getdata_kmeans(), kmeans, method="silhouette", k.max=input$k.max,iter.max=input$km_itermax, nstart=input$km_nstart,algorithm=km_alg()) + theme_minimal() + ggtitle("the Silhouette Method")
          vals$kmeans_elbow<-p
          vals$kmeans_elbow
        })
      })
    })


    observeEvent(ignoreInit=T,input$oc_run,{
      req(input$kmeans_oc=='Gap Statistic')
      output$oc_elbow_out<-renderUI({
        req(input$k.max)
        renderPlot({
          gap_stat<-suppressWarnings(clusGap(getdata_kmeans(), FUN=kmeans, K.max=input$k.max,iter.max=input$km_itermax, nstart=input$km_nstart,algorithm=km_alg(), B=input$oc_boot,verbose=F))
          vals$gap_stat<-gap_stat
          p<-fviz_gap_stat(vals$gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")
          vals$kmeans_elbow<-p
          vals$kmeans_elbow
        })
      })
    })





    observeEvent(vals$newcolhabs,{
      choices = vals$colors_img$val
      choicesOpt = list(content = vals$colors_img$img)
      updatePickerInput(session,"psom_facpalette",choices=choices,
                        choicesOpt=choicesOpt,
                        selected=vals$psom_facpalette)

      updatePickerInput(session,"psom_data_palette",choices=choices,
                        choicesOpt=choicesOpt,
                        selected=vals$psom_data_palette)
    })

    output$kmeans_down_model<-renderUI({
      div(id="kmeans_side",
          class="map_control_style",style="color: #05668D",
          #uiOutput(ns("kmeans_result_side")),
          tipify(
            downloadLink(ns("downModel_kmeans"),"+ Download model"),placement="bottom",
            "Download model as .rds"
          )
      )
    })

    observeEvent(ignoreInit=T,input$downplot_kmeans,{
      vals$hand_plot<-switch(input$model_or_data,
                             "data"="k-means (pca reprentation)",
                             "som codebook"="k-means (codebook)")
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
    })

    output$downModel_kmeans<-{
      downloadHandler(
        filename=function() {
          paste0("kmeans_models","_", Sys.Date(),".rds")
        }, content=function(file) {
          saveRDS(vals$k_means_results,file)
        })
    }
    ### psom module


    observe({
      shinyjs::toggle("psom_create_codebook",condition =input$model_or_data=="som codebook" )
    })


    output$pdata_out<-renderUI({
      req(input$model_or_data=="data")
      req(input$data_kmeans)
      data<-vals$saved_data[[input$data_kmeans]]
      req(data)
      if(ncol(data)==1){
        plotOutput(ns("pdata_plot_box"))
      } else{
        plotOutput(ns("pdata_plot"))
      }

    })

    output$pdata_plot_box<-  renderPlot({
      #saveRDS(reactiveValuesToList(input),"input.rds")
      #saveRDS(reactiveValuesToList(vals),"vals.rds")
      #input<-readRDS('input.rds')
      #vals<-readRDS("vals.rds")
      req(input$model_or_data=="data")
      cluster<-getkmodel()
      #req(input$data_kmeans)
      data<-vals$saved_data[[input$data_kmeans]]
      clu<-vals$k_means_results$cluster$cluster
      req(clu)
      pbox(data.frame(Cluster=clu,data), newcolhabs=vals$newcolhabs, palette="turbo")
      colors<-vals$newcolhabs[['turbo']] (nlevels(clu))[clu]
      points(data.frame(Cluster=clu,data),col="black",bg=colors, pch=21)

    })


    output$pdata_plot<-renderPlot({
      req(input$data_kmeans)
      req(!is.null(vals$k_means_results))
      req(input$model_or_data=="data")
      target<-attr(getkmodel(),"data")
      vals$kmeans_clusters<-phc()$somC


      res.km<-vals$k_means_results
      data<-vals$saved_data[[input$data_kmeans]]
      psom_factors<-psom_factors()
      psom_points<-psom_points()
      pic<- which(unlist(lapply(data,var))==0)
      if(length(pic)>0){
        data<-data[,-pic]
      }
      res.pca<-prcomp(data,center=T)
      cols<-getcolhabs(vals$newcolhabs, input$psom_facpalette, nrow(res.km$centers))
      summ<-summary(res.pca)
      lab<-paste0("(",round(summ$importance[,1:2][2,]*100,2),"%)")
      xlab<-paste("Dim I",lab[1])
      ylab<-paste("Dim II",lab[2])
      # Coordinates of individuals
      ind.coord<-as.data.frame(res.pca$x[,1:2])
      # Add clusters obtained using the K-means algorithm
      ind.coord$cluster<-vals$kmeans_clusters
      colnames(ind.coord)<-c("Dim.1","Dim.2","cluster")
      variance.percent<-round(summ$importance[,1:2][2,]*100,2)
      shape<-if(is.null(input$psom_symbol)){16} else{as.numeric(input$psom_symbol)}




      p<-ggscatter(
        ind.coord, x="Dim.1", y="Dim.2",
        color="cluster", ellipse=TRUE, ellipse.type="convex",
        shape=shape,
        size=input$psom_symbol_size,  legend="right", ggtheme=theme_bw(base_size=input$psom_symbol_size+11),show.legend.text=F,
        xlab=paste0("Dim 1 (", variance.percent[1], "% )" ),
        ylab=paste0("Dim 2 (", variance.percent[2], "% )" ),
        label=NULL, point=psom_points
      )

      if(input$dot_label_clus=='labels'){
        p<-p  +geom_text( data=ind.coord, aes(x=Dim.1, y=Dim.2, label=psom_factors,),size=input$psom_symbol_size, color=cols[ind.coord$cluster])

      }
      vals$kmeans_plot_data<-p+scale_color_manual(values= cols )
      vals$kmeans_plot_data

    })


    observe(shinyjs::toggle("psom_data_palette",condition=input$model_or_data=="som codebook"))

    observeEvent(ignoreInit=T,input$psom_data_palette,{vals$psom_data_palette<-input$psom_data_palette})

    psom_factors<- reactive({
      req(input$data_kmeans)
      if (length(input$psom_factors)>0) {
        if(input$psom_factors=="rownames"){
          rownames(vals$saved_data[[input$data_kmeans]])
        } else{
          as.factor(attr(vals$saved_data[[input$data_kmeans]],"factors")[rownames(vals$saved_data[[input$data_kmeans]]), input$psom_factors])
        }

      } else{NULL}
    })
    psom_points<-reactive({
      req(input$dot_label_clus)
      if(input$dot_label_clus == 'symbols'){ T} else {F}

    })
    output$psom_code<-renderPlot({
      req(!is.null(vals$k_means_results))
      req(input$psom_facpalette)
      req(input$data_kmeans)
      data=vals$saved_data[[input$data_kmeans]]
      m<-attr(data,"som")[[input$som_kmeans]]

      somC<-list(
        som.hc=as.factor(vals$k_means_results[[1]]),
        groups=input$km_centers,
        som.model= m
      )
      pclus(
        somC=somC,
        cex=as.numeric(input$psom_symbol_size),
        factor.pal=as.character(input$psom_facpalette),
        labels.ind=psom_factors(),
        pch=as.numeric(input$psom_symbol),
        points=psom_points(),
        bg_palette=input$psom_data_palette,
        ncol=input$psom_ncol,
        insetx=input$psom_insertx,
        insety=input$psom_inserty,
        alpha.legend=input$psom_bgleg,
        newcolhabs=vals$newcolhabs,
        bgalpha=input$psom_bgalpha
      )
      vals$psom_plot<-recordPlot()
    })



    savecodebook<-reactive({
      req(input$data_kmeans)
      data=vals$saved_data[[input$data_kmeans]]
      m<-attr(data,"som")[[input$som_kmeans]]
      codes<-data.frame(do.call(cbind,m$codes))
      factors<-data.frame(vals$k_means_results[[1]])
      rownames(factors)<-rownames(codes)<-paste0("unit_",1:nrow(codes))
      colnames(factors)<-paste0("Kmeans_",input$km_centers)

      attr(codes,"factors")<-factors
      temp<-codes
      temp<-data_migrate(data,temp,"new")
      attr(temp,"data.factor")<-NULL
      attr(temp,"factors")<-factors
      attr(temp,"datalist")<-NULL
      attr(temp,"filename")<-NULL
      attr(temp,"coords")<-NULL
      attr(temp,"base_shape")<-NULL
      attr(temp,"layer_shape")<-NULL
      attr(temp,"transf")<-NULL
      attr(temp,"nobs_ori")<-NULL
      if(input$hand_save=="create"){
        vals$saved_data[[input$newdatalist]]<-temp
        #  vals$cur_data<-input$data_newname
      } else{

        vals$saved_data[[input$over_datalist]]<-temp
        # vals$cur_data<-input$data_over

      }
    })
    observeEvent(ignoreInit=T,input$psom_insertx,{
      vals$psom_insertx<-input$psom_insertx
    })
    observeEvent(ignoreInit=T,input$psom_inserty,{
      vals$psom_inserty<-input$psom_inserty
    })
    observeEvent(ignoreInit=T,input$psom_ncol,{
      vals$psom_ncol<-input$psom_ncol
    })
    observeEvent(ignoreInit=T,input$psom_bgleg,{
      vals$psom_bgleg<-input$psom_bgleg
    })

    observe({
      req(input$psom_facpalette)
      col<-getcolhabs(vals$newcolhabs,input$psom_facpalette,2)
      shinyjs::toggle("psom_legcontrol",condition = input$model_or_data=="som codebook"&col[1]!=col[2])
    })


    observeEvent(ignoreInit=T,input$psom_symbol,{
      vals$psom_symbol<-input$psom_symbol
    })

    observeEvent(df_symbol$val,{
      choices=df_symbol$val
      choicesOpt=list(content=df_symbol$img)
      selected=vals$psom_symbol
      updatePickerInput(session,'psom_symbol',choices=choices,choicesOpt=choicesOpt,selected=selected)
    })

    observe({
      shinyjs::toggle("psom_symbol",condition=input$dot_label_clus=='symbols')
    })

    observeEvent(ignoreInit=T,input$psom_symbol_size,{
      vals$psom_symbol_size<-input$psom_symbol_size
    })




    observeEvent(ignoreInit=T,input$dot_label_clus,{
      vals$dot_label_clus<-input$dot_label_clus
    })
    observeEvent(ignoreInit=T,input$psom_facpalette,{
      vals$psom_facpalette<-input$psom_facpalette
    })
    observeEvent(ignoreInit=T,input$psom_factors,{vals$psom_factors<-input$psom_factors})



    observeEvent(list(input$data_kmeans,input$psom_facpalette),{
      req(input$data_kmeans)
      req(input$psom_facpalette)
      col<-getcolhabs(vals$newcolhabs,input$psom_facpalette,2)
      shinyjs::toggle("psom_factors",condition=input$dot_label_clus == 'labels'|col[1]!=col[2])

    })

    observeEvent(input$model_or_data,{
      req(input$data_kmeans)
      if(input$model_or_data=='som codebook'){
        choices<-c(colnames(attr(vals$saved_data[[input$data_kmeans]],"factors")))
      } else{
        choices<-c("rownames",colnames(attr(vals$saved_data[[input$data_kmeans]],"factors")))
      }
      selected=vals$psom_factors
      updatePickerInput(session,'psom_factors',choices=choices,selected=selected)

    })



    ###




    observeEvent(list(input$data_kmeans,input$km_centers,input$km_itermax,input$km_nstart,input$km_alg,input$model_or_data, input$k.max),{
      vals$k_means_results<-NULL
      #vals$k_means_results<-NULL
      #vals$kmeans_elbow<-NULL

    })

    cluster_already<-reactive({

      req(input$data_kmeans)
      datao<-vals$saved_data[[input$data_kmeans]]
      factors<-attr(datao,"factors")

      if(is.null(vals$kmeans_clusters)){
        hc<-as.factor(vals$k_means_results$cluster$cluster)

      } else{
        hc<-as.factor(vals$kmeans_clusters)
      }

      fac<-as.list(factors)
      fac<-lapply(fac,function(x) as.character(x))


      pic<-sapply(fac, function(x) identical(as.character(x), as.character(hc)))
      if(length(pic)>0){
        cluster_already<-which(pic)
        names(cluster_already)
      } else{
        NULL
      }


    })

    output$save_kmeans<-renderUI({

      req(class(vals$k_means_results)=="ikmeans")

      clu_al<-cluster_already()
      if (length(cluster_already()) > 0) {
        class1<-"button_normal"
        class2<-"div"
        class3<-'divnull'
      } else {
        class1<-"save_changes"
        class2<-"divnull"
        class3<-'div'
      }
      div(style="display: flex",
          div(style="display: flex",
              div(class = class1,style="padding-right: 5px",
                  actionButton(ns("tools_savekmeans"), icon("fas fa-save"),  type = "action", value = FALSE)), span(style = "font-size: 12px", icon("fas fa-hand-point-left"), "Save Clusters in Datalist ", strong("X"), class = class3)
          )
          ,
          div(style = "margin-bottom: 5px", class = class2,style="text-direction: normal",em(paste0("The current clustering is saved in the Factor-Attribute as '", paste0(clu_al, collapse = "; "), "'"))))


    })

    observeEvent(ignoreInit=T,input$tools_savekmeans,{
      if(input$tools_savekmeans %% 2) {
        vals$hand_save<-"Save K-means Clusters"
        vals$hand_save2<-p(p(em(input$data_kmeans,style="color: gray"),strong("::"),em("Factor-Attribute",style="color: gray"), strong("::")))
        vals$hand_save3<-NULL
        if(input$model_or_data=='som codebook'){
          vals$hand_save3<-"This action classifies the observations according to the clustering of neurons"
        }
        showModal(module_kmeans())
      }
    })

    observeEvent(ignoreInit=T,input$psom_create_codebook,{
      if(input$psom_create_codebook %% 2) {
        vals$hand_save<-"create_codebook_kmeans"
        vals$hand_save2<-NULL
        vals$hand_save3<-NULL
        showModal(module_kmeans())
      }
    })



    output$kmeans_result_side<-renderUI({
      req(class(vals$k_means_results)=="ikmeans")
      pickerInput(ns('kmeans_res'),"show result:",c('cluster','centers','totss','withinss','tot.withinss','betweenss','size','inter','ifault'), width="120px")
    })



    observeEvent(ignoreInit=T,input$kmeans_run,{
      vals$kmeans_clusters<-NULL
      vals$k_means_results<-NULL

      req(input$data_kmeans)
      updatePickerInput(session,"kmeans_models",choices=c("new kmeans (unsaved)",names(attr(vals$saved_data[[input$data_kmeans]],"kmeans"))), selected='new kmeans (unsaved)')


      try({

        if (!is.na(input$kmeans_seed)) {set.seed(input$kmeans_seed)}
        centers<-input$km_centers
        data<-getdata_kmeans()
        kmeans_result<-kmeans(data, centers, iter.max=input$km_itermax, nstart=input$km_nstart,algorithm=km_alg())
        vals$k_means_results<-kmeans_result
        #newclass<-m$unit.classif
        #for(i in 1:length(hcut)) {newclass[newclass==i]<-rep(hcut[i],sum(  newclass==i))}


        # kmeans_result<-readRDS("kmeans.rds")
        kmeans_result<-lapply(kmeans_result, function(x)data.frame(x))
        onecol<-which(unlist(lapply(kmeans_result,ncol))==1)
        for(i in names(onecol)){
          colnames(kmeans_result[[i]])<-i      }
        class(kmeans_result)<-"ikmeans"
        attr(kmeans_result,"target")<-input$model_or_data
        attr(vals$saved_data[[input$data_kmeans]],"kmeans")[["new kmeans (unsaved)"]]<-kmeans_result
        vals$k_means_results<-kmeans_result


      })

    })

    output$k_mean_res<-DT::renderDataTable({
      req(input$kmeans_res)
      vals$k_means_results[[input$kmeans_res]]
    },options=list(pageLength=20, info=FALSE,lengthMenu=list(c(20, -1), c( "20","All")), autoWidth=T), rownames=TRUE,class ='cell-border compact stripe')





    observeEvent(ignoreInit=T,input$km_nstarts,{ vals$km_nstarts<-input$km_nstarts})


    observeEvent(ignoreInit=T,input$km_itermax,{ vals$km_itermax<-input$km_itermax})


    observeEvent(ignoreInit=T,input$km_centers,{ vals$km_centers<-input$km_centers})




    observeEvent(ignoreInit=T,input$data_kmeans,{ vals$cur_data<-input$data_kmeans})

    observeEvent(ignoreInit=T,input$som_kmeans,{ vals$som_kmeans<-input$som_kmeans})

    observeEvent(input$data_kmeans,{
      data=vals$saved_data[[input$data_kmeans]]
      choices=names(attr(data,"som"))
      selected=vals$som_kmeans
      updateSelectInput(session,'som_kmeans',choices=choices,selected=selected)
    })

    observe({
      shinyjs::toggle("som_kmeans",condition=input$model_or_data=='som codebook')
    })




    getdata_kmeans<-reactive({
      req(input$data_kmeans)
      req(input$data_kmeans)
      data=vals$saved_data[[input$data_kmeans]]
      #validate(need(length(data)>0,"no data found"))
      req(input$model_or_data)
      res<-if(input$model_or_data=="data"){
        data
      } else{
        req(length(input$som_kmeans)>0)
        m<-attr(data,"som")[[input$som_kmeans]]

        codemat<-as.matrix(object.distances(m,"codes"))
        codes<-cmdscale(codemat, k=ncol(codemat)-1)
        codes
      }
      res

    })



    savekmeans<-reactive({
      if(is.null(vals$kmeans_clusters)){
        temp<-vals$k_means_results$cluster$cluster
      } else{
        temp<-vals$kmeans_clusters
      }

      req(temp)

      if(input$hand_save=="create"){
        attr(vals$saved_data[[input$data_kmeans]],"factors")[input$newdatalist]<-as.factor(unlist(temp))
      } else{
        attr(vals$saved_data[[input$data_kmeans]],"factors")[input$over_datalist]<-as.factor(unlist(temp))
      }
      removeClass('save_kmeansbtn','save_changes')
      #shinyjs::hide('save_kmeans')
    })

    observeEvent(ignoreInit=T,input$kmeans_run,{
      addClass('save_kmeansbtn','save_changes')
    })

    data_overwritte<-reactiveValues(df=F)
    data_store<-reactiveValues(df=F)
    newname<-reactiveValues(df=0)


    get_newname<-reactive({
      req(!is.null(vals$hand_save))
      newname$df<-switch(
        vals$hand_save,
        "Save K-means Model"= { name_kmeans_models()},
        "Save K-means Clusters"= { name_kmeans_clusters()},
        "create_codebook_kmeans"={name_kmeans_codebook()}
      )})

    name_kmeans_models<-reactive({
      req(input$data_kmeans)
      name0<-paste0("Kmeans")
      names<-names(attr(vals$saved_data[[input$data_kmeans]],"kmeans"))
      pic<-which(names=="new kmeans (unsaved)")
      if(length(pic)>0){
        names<-names[-pic]
      }
      if(length(names)>0){
        name1<-make.unique(c(names,name0), sep="_")
        name1[length(names)+1]
      } else{
        name1<-name0
        name1
      }


    })

    name_kmeans_clusters<-reactive({
      centers<-input$km_centers
      if(length(centers)==1){bag0=centers}else{bag0=length(centers)}
      name0<-paste0("kmeans_",gsub(" codebook","",input$model_or_data),"_",bag0)
      newnames<-make.unique(c(colnames(attr(vals$saved_data[[input$data_kmeans]],"factors")),name0))
      newnames[length(newnames)]


    })
    name_kmeans_codebook<-reactive({
      name0<-paste0("Coodebook+Kmeans:",input$km_centers)
      bag=1
      name1<-paste0(name0," (",bag,")")
      if(name1%in%colnames(attr(vals$saved_data[[input$data_kmeans]],"factors")))
      {
        repeat{
          bag<-bag+1
          name1<-paste0(name0," (",bag,")")
          if(!name1%in%colnames(attr(vals$saved_data[[input$data_kmeans]],"factors"))) break
        }
      }
      paste0(name0," (",bag,")")
    })
    output$data_over<-renderUI({
      data_overwritte$df<-F
      choices<-c(names(vals$saved_data))
      req(input$hand_save=="over")
      choices=switch (vals$hand_save,
                      'Save K-means Model'=names(attr(vals$saved_data[[input$kmeans]],"kmeans")),
                      'Save K-means Clusters'=colnames(attr(vals$saved_data[[input$data_kmeans]],'factors')),
                      "create_codebook_kmeans"=names(vals$saved_data)
      )



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
        "Save K-means Model"= {savekmeans_models()},
        "Save K-means Clusters"= {savekmeans()},
        "create_codebook_kmeans"=savecodebook()
      )
      vals$hand_save<-NULL
      removeModal()

    })
    module_kmeans<-function() {
      ns<-session$ns
      modalDialog(
        uiOutput(ns("databank_storage")),
        title=strong(icon("fas fa-save"),'Save'),
        footer=column(12,
                      uiOutput(ns('saverf_teste')),
                      fluidRow(modalButton(strong("cancel")),
                               inline(uiOutput(ns("save_confirm")))
                      )
        ),

        easyClose=T
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

    savereac<-reactive({

      tosave<-isolate(reactiveValuesToList(vals))

      saveRDS(tosave,"savepoint.rds")
      saveRDS(reactiveValuesToList(input),"input.rds")

      #vals<-readRDS("vals.rds")
      #input<-readRDS('input.rds')

    })

  })
}

