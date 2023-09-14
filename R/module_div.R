
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
#' @export
module_ui_div<-function(id){

  ns<-NS(id)
  div(
  #  inline( actionButton(ns("teste_comb"),"SAVE")),
    uiOutput(ns('divtabs'))
  )

}
#' @export
module_server_div<-function (input,output,session,vals,df_colors,newcolhabs,df_symbol ){
  getdata_div<-reactive({
    req(input$data_div)
    data=vals$saved_data[[input$data_div]]

    data
  })
  palette=c(
    "turbo",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"
  )
  symbols<-c("pch1","pch2","pch3","pch4")

  dfcolors <- data.frame(
    val = palette
  )

  ns<-session$ns
  output$div_results = {DT::renderDataTable({    req(length(divI())>0)
    divI()
  },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')}
  output$divcontrol <- renderUI({
    fluidRow(
      p(
        uiOutput(ns("data_div"))


      )



    )

  })

  output$Nhelp<-renderUI({
    req(input$Nhelp%%2)
    column(12,style="background: white",
           strong("Number of individuals"),
           p("Total number of individuals")
    )
  })
  output$Shelp<-renderUI({
    req(input$Shelp%%2)
    column(12,style="background: white",
           strong("Species richness"),
           p("the total number of species in each observation")
    )
  })
  output$mhelp<-renderUI({
    req(input$mhelp%%2)
    column(12,style="background: white",
           strong("Margalef diversity"),
           p("The total number of species weighted by the logarithm of the total number of individuals", withMathJax(helpText("$$ margalef = \\frac{(S-1)}{lnN}$$")))
    )
  })
  output$Dhelp<-renderUI({
    req(input$Dhelp%%2)
    column(12,style="background: white",
           strong("Simpson diversity"),
           p("the probability that two individuals drawn at random from an infinite community would belong to the same species",withMathJax(helpText("$$ D = \\sum p^2$$")))
    )
  })
  output$Hhelp<-renderUI({
    req(input$Hhelp%%2)
    column(12,style="background: white",
           strong("Shannon diversity"),
           p("This index  considers both species richness and evenness. The uncertainty is measured by the Shannon Function 'H'. This term is the measure corresponding to the entropy concept defined by:",withMathJax(helpText("$$ H = \\sum_{n=1}^n (p_i*\\ln p_i)$$")))
    )
  })

  output$Hloghelp<-renderUI({
    req(input$Hloghelp%%2)
    column(12,style="background: white",
           strong("Shannon diversity (log base 2)"),
           p("Shannon Function 'H' on base 2. This term is the measure corresponding to the entropy concept defined by:",withMathJax(helpText("$$
  H_{log_2}=\\sum_{n = 1}^{n}{(p_i * log_2p_i)} $$")))
    )
  })
  output$Hlog10help<-renderUI({
    req(input$Hlog10help%%2)
    column(12,style="background: white",
           strong("Shannon diversity (log base 10)"),
           p("Shannon Function 'H' on base 10. This term is the measure corresponding to the entropy concept defined by:",withMathJax(helpText("$$
     H_{log_{10}}=\\sum_{n = 1}^{n}{(p_i * log_{10}p_i)} $$")))
    )
  })
  output$Jhelp<-renderUI({
    req(input$Jhelp%%2)
    column(12,style="background: white",
           strong("Shannon evenness J'"),
           p("Evenness is a measure of how different the abundances of the species in a community are from each other. The Shannon evennes is defined by:",
             withMathJax(helpText("$$ J' = \\frac{H}{\\ln(S)}$$")))
    )
  })

  output$Jloghelp<-renderUI({
    req(input$Jloghelp%%2)
    column(12,style="background: white",
           strong("Shannon evenness J (log base 2)'"),
           p("The Shannon evennes on log base 2:",
             withMathJax(helpText("$$ J' = \\frac{H}{\\l2(S)}$$")))
    )
  })

  output$Domhelp<-renderUI({
    req(input$Domhelp%%2)
    column(12,style="background: white",
           strong("Relative Dominance'"),
           p("A simple measure of dominance where",em(HTML(paste0("N",tags$sub("i")))),", the abundance of the most abundant species is divided by N:",
             withMathJax(helpText("$$ Dom_{rel} = \\frac{N_1}{N} $$")))
    )
  })
  output$Skhelp<-renderUI({
    req(input$Skhelp%%2)
    column(12,style="background: white",
           strong("LogSkew'"),
           p("Skew is the third moment of a probability distribution, measuring asymmetry. Right skew (positive numbers) indicates more probability on the right (abundant) side. Left skew (negative numbers) indicates more probability on the left side. All species abundance distributions are strongly right skewed on an arithmetic scale, so the more interesting measure is skew on the log scale:",
             withMathJax(
               helpText("$$ LogSkew = \\frac{\\sum {\\frac{(log(n_i)-\\mu)^3}{S}}}{
                       [\\sum {\\frac{(log(n_i)-\\mu)^2}{S}}]^\\frac{3}{2} \\frac{S}{(S-2)} \\sqrt{[\\frac{(S-1)}{S}]}
               } $$")
             )),p("where ",em(HTML(paste0(HTML("&mu;"),tags$sub("i"))),paste0("is the mean of log("),em(HTML(paste0("n",tags$sub("i")))),")"))
    )
  })
  observeEvent(ignoreInit = T,input$tools_savediv,{
    vals$hand_save<-"Save diversity results"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(
      module_div()
    )
  })
  output$data_div<-renderUI({
    selectInput(ns("data_div"), "Datalist:", choices = names(vals$saved_data), selected=vals$cur_data, selectize=T)
  })
  divI<-reactive({

    req(input$divInds)
    abund=getdata_div()

    validate(need(!any(colSums(sapply(abund,function (x) x<0))>0),"Data must be non-negative"))

        choices=input$divInds
    res=list()
    if("N"%in%choices){res$N<-rowSums(abund)}
    if("S"%in%choices){res$S<-vegan::specnumber(abund)}
    if("margalef"%in%choices){res$margalef=(specnumber(abund)-1)/log(rowSums(abund))}
    if("D"%in%choices){res$D<-vegan::diversity(abund, index="simpson")}
    if("H"%in%choices){res$H<-vegan::diversity(abund)}
    if("Hlog2"%in%choices){res$Hlog2<-vegan::diversity(abund, base=2)}
    if("Hlog10"%in%choices){res$Hlog10<-vegan::diversity(abund, base=10)}
    if("J'"%in%choices){
      H<-vegan::diversity(abund)
      S<-vegan::specnumber(abund)
      res$J <- H/log(S)}

    if("Dom_rel"%in%choices){res$Dom_rel<-apply(decostand(abund, "total"),1,sort,T)[1,]}
    if("Skewness"%in%choices){res$Skewness=apply(abund,1,skewness)}
    res<-data.frame(do.call(cbind,res))

    #rowSums(apply(data,2,function(x)x==1))

    res
  })


  output$divtabs <- renderUI({
    validate(need(length(vals$saved_data)>0,"No Datalist found"))
    tabsetPanel(
      tabPanel(
        "1. Diversity Indexes",
        h4(strong("Biological diversity indices")),
        uiOutput(ns('divindex'))
      ),
      tabPanel('2. Niche Analysis',value="tab_omi",
               uiOutput(ns("omi_header")),
               uiOutput(ns('omi_panels'))
      )
    )
  })

  output$divindex<-renderUI({

    validate(need(length(vals$saved_data)>0,"No Datalist found"))
    req(length(vals$saved_data)>0)
    column(12,
      splitLayout(cellWidths = c("30%","30%","10%"),cellArgs = list(style='white-space: normal;'),
        uiOutput(ns("divcontrol")),
        column(12,style="margin-top: 35px",
          dropdownButton(label = "Diversity indexes", status ="button_active" ,  inline = T, circle=F,tooltip = tooltipOptions(title = "Select the indexes", placement="bottom"),width="350px",
            checkboxGroupInput(ns("divInds"),
              NULL,
              choiceValues =list(
                "N","S","margalef","D","H","Hlog2","Hlog10","J'","Dom_rel","Skewness"
              ),
              selected=c(
                "N","S","margalef","D","H","Hlog2","Hlog10","J'","Dom_rel","Skewness"
              ),
              inline = F,

              choiceNames =list(
                span("N", actionLink(ns('Nhelp'),icon("fas fa-question-circle")), uiOutput(ns("Nhelp"))),
                span("S",actionLink(ns('Shelp'),icon("fas fa-question-circle")), uiOutput(ns("Shelp"))),
                span("margalef",actionLink(ns('mhelp'),icon("fas fa-question-circle")), uiOutput(ns("mhelp"))),
                span("D",actionLink(ns('Dhelp'),icon("fas fa-question-circle")), uiOutput(ns("Dhelp"))),
                span("H",actionLink(ns('Hhelp'),icon("fas fa-question-circle")), uiOutput(ns("Hhelp"))),
                span("Hlog2",actionLink(ns('Hloghelp'),icon("fas fa-question-circle")), uiOutput(ns("Hloghelp"))),
                span("Hlog10",actionLink(ns('Hlog10help'),icon("fas fa-question-circle")), uiOutput(ns("Hlog10help"))),
                span("J",actionLink(ns('Jhelp'),icon("fas fa-question-circle")), uiOutput(ns("Jhelp"))),

                span("Dom_rel",actionLink(ns('Domhelp'),icon("fas fa-question-circle")), uiOutput(ns("Domhelp"))),
                span("Skewness",actionLink(ns('Skhelp'),icon("fas fa-question-circle")),uiOutput(ns("Skhelp")))
              )

            )

          )
        ),
        column(12,style="margin-top: 35px",
               popify(bsButton(ns("tools_savediv"), icon("fas fa-save"),style  = "button_active", type="action",value=FALSE),NULL,"Save diversity results"
               ))

      ),
      column(12,style="overflow-x: scroll; background: white",

             DT::dataTableOutput(ns('div_results')))

    )



  })



  output$omi_header<-renderUI({
    vals$niche_result<-NULL
    div(style="background: white",
        p(strong("Method to Analyse a pair of tables: Environmental and Faunistic Data")),
        span(
          inline(
            span(style="width: 150px",

                 inline(uiOutput(ns("omi_X")))
            )
          ),
          inline(uiOutput(ns("omi_Y"))),
          actionButton(ns('run_niche'),"RUN")
        )
    )
  })
  output$omi_X<-renderUI({
    pickerInput(ns("omi_X"),span("Y Data", tiphelp("Predictors")), choices=names(vals$saved_data), selected=vals$omi_X)
  })
  output$omi_Y<-renderUI({
    req(input$omi_X)
    pickerInput(ns("omi_Y"),span("~ X Data", tiphelp("Response data")), choices=names(vals$saved_data), selected=vals$omi_Y)
  })

  observeEvent(ignoreInit = T,input$omi_X,{
    vals$omi_X<-input$omi_X
  })
  observeEvent(ignoreInit = T,input$omi_Y,{
    vals$omi_Y<-input$omi_Y
  })
  saveniche<-reactive({
    temp<-vals$niche_result
    factors<-data.frame(id=as.factor(rownames(temp)))
    rownames(factors)<-rownames(temp)

    datao<-vals$saved_data[[input$omi_Y]]
    temp<-data_migrate(datao,temp,"newdatalist")
    attr(temp,"factors")<-factors
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })

  name_niche<-reactive({
    bag<-1
    name0<-paste0("Niche results")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }}
    paste(name0,bag)
  })
  output$omi_panels<-renderUI({
    uiOutput(ns("omi_out"))

  })

  observeEvent(ignoreInit = T,input$niche_tabs,{
    vals$niche_tabs<-input$niche_tabs
  })

  output$omi_out<-renderUI({
    sidebarLayout(
      sidebarPanel(uiOutput(ns("omi_side"))),
      mainPanel(
        uiOutput(ns('omi_plot1')),

        uiOutput(ns('omi_tables'))
      )
    )

  })

  output$eplot_obs_selection<-renderUI({
    req(input$show_niche)
    req(input$show_niche=="Plot")
    div(class="cogs_in_div",style="margin-bottom: 10px; padding:5px",
        fluidPage(
          actionLink(ns("show_obs_selection"),"+ Select the observations"),
          DT::dataTableOutput(ns('observation_selection'))


        )
    )

  })


  observeEvent(ignoreInit = T,input$show_obs_selection,{
    shinyjs::toggle("observation_selection")
  })

  output$observation_selection = DT::renderDataTable(
    {
    em<-getebnb()
    table=data.frame(id=rownames(em))
    DT::datatable(table, options=list(
      dom="t",
      colnames="",
      lengthMenu = list(c(-1), c("All")),
      scrollX = TRUE,
      scrollY = "200px",
      autoWidth=F

    ), class ='compact cell-border',rownames=F,

    selection = list(mode = 'multiple', selected = c(1:20)))
    })






  output$omi_plot1<-renderUI({
    req(input$show_niche)
    req(input$show_niche=="Plot")
    req(length(input$observation_selection_rows_selected)>0)

    df0<-getebnb()[input$observation_selection_rows_selected,]


    df<-data.frame(id=rownames(df0),df0)
    p<-ggplot(df,aes(x=NP, y=reorder(id,NP, decreasing=T)))+ geom_point()+
      geom_errorbar(aes(xmin  = NP-( NB/2), xmax  = NP+( NB/2)))+xlab(paste0("Niche Axis",input$ebnb_pc))+ylab("Species")
    vals$plot_niche<-p
    renderPlot(p)


  })




  observeEvent(ignoreInit = T,input$omi_result,{
    if(input$omi_result=="EBNB"){shinyjs::show('ebnb_pc')}else{
      shinyjs::hide('ebnb_pc')
    }
  })

  output$ebnb_pc<-renderUI({
    req(!is.null(vals$omi_result))
    req(input$omi_result=="EBNB")
    span("+ PC",inline( pickerInput(ns("ebnb_pc"),NULL,choices=c(1:ncol(vals$omi_result$ls)), width="50px")))

  })
  observeEvent(ignoreInit = T,input$omi_X,{
    vals$niche_result<-NULL
  })
  observeEvent(ignoreInit = T,input$omi_Y,{
    vals$niche_result<-NULL
  })
  observeEvent(ignoreInit = T,input$run_niche,{
    vals$omi_result<-NULL
    try({
      y<-vals$saved_data[[input$omi_X]]
      x<-vals$saved_data[[input$omi_Y]]
      dudi1 <- dudi.pca(x, scale = F, scan = FALSE)
      nic1 <- niche(dudi1, data.frame(y), scann = FALSE)
      vals$niche_params<-niche.param(nic1)
      vals$omi_result<-nic1
    })
  })

  observe({
    req(input$omi_result)
    req(!is.null(vals$omi_result))
    vals$niche_result<-switch(input$omi_result,
                              'Niche params'={vals$niche_params},
                              "species coordinates"=vals$omi_result$li,
                              "variable coordinates"=vals$omi_result$co,
                              "Site coordinates"={vals$omi_result$ls},
                              "Axis upon niche axis"={vals$omi_result$as},
                              "EBNB"={getebnb()})
  })
  getebnb<-reactive({
    req(input$ebnb_pc)
    y<-vals$saved_data[[input$omi_X]]
    x<-vals$saved_data[[input$omi_Y]]
    EBNB(y,x,as.numeric(input$ebnb_pc))
  })



  output$omi_tables<-renderUI({
    req(input$show_niche)
    req(input$show_niche=="Table")
    req(input$omi_result)
    DT::renderDataTable({
      vals$niche_result
    },options = list(pageLength = 20, info = FALSE,lengthMenu = list(c(20, -1), c( "20","All")), autoWidth=T), rownames = TRUE,class ='cell-border compact stripe')
  })


  observeEvent(ignoreInit = T,input$show_niche,{
    vals$show_niche<-input$show_niche
  })

  observeEvent(ignoreInit = T,input$omi_result,{
    vals$cur_omi_result<-input$omi_result
  })

  output$show_niche_out<-renderUI({
    req(input$omi_result)
    choices<-if(input$omi_result=="EBNB"){
      c("Plot","Table")
    } else{
      "Table"
    }
    div(radioButtons(ns("show_niche"),"+ Show",choices, selected = vals$show_niche))
  })

  output$omi_side<-renderUI({
    div(class="map_control_style",style="color: #05668D",
        uiOutput(ns("show_niche_out")),

        div(
          span("+ Result:",
               inline(
                 pickerInput(ns("omi_result"),NULL,choices=c('Niche params',
                                                             "species coordinates",
                                                             "variable coordinates",
                                                             "Site coordinates",
                                                             "Axis upon niche axis",
                                                             "EBNB"),
                             selected=vals$cur_omi_result,width="120px")
               ),inline(uiOutput(ns("ebnb_pc"))))),
        uiOutput(ns('eplot_obs_selection')),
        br(),
        div(
          tipify(
            actionLink(
              ns('omi_save_datalist'),span("+ Create Datalist",icon("fas fa-file-signature")), style="button_active"
            ),
            "Create a datalist with the results", options=list(container="body")
          )
        ),
        div(
          actionLink(
            ns('omi_down'),span("+ Download result"), style="button_active"
          )
        ),
        uiOutput(ns('side_downp_perf'))
    )
  })
  output$side_downp_perf<-renderUI({
    req(input$show_niche)
    req(input$show_niche=="Plot")

    div(
      tipify(
        actionLink(
          ns('downp_perf'),span("+ Download",icon("fas fa-download"),icon("fas fa-image")), style="button_active"
        ),
        "+ Download plot", options=list(container="body")
      )
    )
  })

  observeEvent(ignoreInit = T,input$downp_perf,{
    vals$hand_plot<-"EBNB"
    module_ui_figs("downfigs")
    mod_downcenter <- callModule(module_server_figs, "downfigs",  vals=vals)
  })

  observeEvent(ignoreInit = T,input$omi_save_datalist,{
    vals$hand_save<-"Create Datalist: Niche results"
    vals$hand_save2<-NULL
    vals$hand_save3<-NULL
    showModal(module_div())
  })

  observeEvent(ignoreInit = T,input$omi_down,{
    vals$hand_down<-"Niche results"
    module_ui_downcenter("downcenter")
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals)
  })




  bag_divname<-reactive({
    bag<-1
    name0<-paste("Div_results")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data))
    {
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(vals$saved_data)) break
      }
    }
    paste("Div_results",bag)

  })

  data_overwritte<-reactiveValues(df=F)
  data_store<-reactiveValues(df=F)
  newname<-reactiveValues(df=0)
  get_newname<-reactive({
    req(!is.null(vals$hand_save))
    newname$df<-switch(
      vals$hand_save,
      ##RF
      "Save diversity results"= {bag_divname()},
      "Create Datalist: Niche results"={name_niche()}

    )})

  savediv<-reactive({
    divInds<-divI()
    data<-getdata_div()
    temp<-data_migrate(data,divInds,input$newdatalist)
    if(input$hand_save=="create"){
      vals$saved_data[[input$newdatalist]]<-temp
     # vals$cur_data<-input$newdatalist
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
     # vals$cur_data<-input$over_datalist
    }
  })
  div_create_training_errors<-reactive({
    temp<-vals$div_down_errors_train
    temp<-data_migrate(vals$saved_data[[input$data_divX]],temp,"newdatalist")
    if(input$hand_save=="create") {
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  div_create_pred<-reactive({
    temp<-data.frame(vals$divtab_pred)
    temp[,1]<-as.numeric( temp[,1])
    attr(temp,"factors")<-temp

    datao<-if(vals$divpred_which=="Datalist"){
      vals$saved_data[[vals$preddiv_new]]
    } else{ vals$saved_data[[input$data_divX]]}
    if(input$hand_save=="create") {
      temp<-data_migrate(datao,temp,input$newdatalist)
      vals$saved_data[[input$newdatalist]]<-temp
    } else{
      temp<-data_migrate(datao,temp,input$over_datalist)
      vals$saved_data[[input$over_datalist]]<-temp
    }

  })
  name_save_div<-reactive({
    bag<-1
    name0<-paste0("div")
    name1<-paste(name0,bag)
    if(name1%in%names(attr(vals$saved_data[[input$data_divX]],"div"))){
      repeat{
        bag<-bag+1
        name1<-paste(name0,bag)
        if(!name1%in%names(attr(vals$saved_data[[input$data_divX]],"div"))) break
      }}
    paste(name0,bag)
  })
  name_div_train_errors<-reactive({
    bag<-1
    name0<-paste0("div training errors")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  name_div_pred<-reactive({
    bag<-1
    name0<-paste0("div predictions")
    name1<-paste(name0,bag)
    if(name1%in%names(vals$saved_data)){
      repeat{bag<-bag+1
      name1<-paste(name0,bag)
      if(!name1%in%names(vals$saved_data)) break}}
    paste(name0,bag)
  })
  output$data_over<-renderUI({
    data_overwritte$df<-F
    choices<-c(names(vals$saved_data))
    req(input$hand_save=="over")
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
      "Save diversity results"= { savediv()},
      "Create Datalist: Niche results"={saveniche()}


    )
    removeModal()

  })
  module_div <- function() {
    ns <- session$ns
    modalDialog(
      uiOutput(ns("databank_storage")),
      title=strong(icon("fas fa-save"),'Save'),
      footer=column(12,
                    uiOutput(ns('saverf_teste')),
                    fluidRow(modalButton(strong("cancel")),
                             inline(uiOutput(ns("save_confirm")))
                    )
      ),

      easyClose = T
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
  observeEvent(ignoreInit = T,input$teste_comb,{
    savereac()
  })


  savereac<-reactive({



    tosave<-isolate(reactiveValuesToList(vals))
    tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
    tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]
    tosave$saved_data<-vals$saved_data
    tosave$newcolhabs<-vals$newcolhabs
    tosave$colors_img<-vals$colors_img
    tosave$niche_params<-vals$niche_params
    tosave$omi_result<-vals$omi_result
    tosave$ebnb<-getebnb()

    #tosave$ensemble_args<-getargs_root()
    #tosave$ensemble_args_plot<-getargs_plot_scoreloss()




    saveRDS(tosave,"savepoint.rds")
    saveRDS(reactiveValuesToList(input),"input.rds")
    beep()


  })
  }

