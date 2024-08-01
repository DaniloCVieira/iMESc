
## Licensed under the CC BY-NC-ND 4.0 license.
#' @noRd

#' @export
#'
diversity_tool<-list()
diversity_tool$ui<-function(id){

  ns<-NS(id)
  div(tabsetPanel(

    tabPanel(
      "1. Diversity indices",
      box_caret(ns("box_setup1"),inline=F,show_tittle=F,
                color="#374061ff",
                title="Setup",
                div(
                  div(style="display: flex;height: 50px",class="setup_box picker-flex",
                      div(
                        style="display: flex;",
                        div(class="setup_box picker-flex picker-before-x",pickerInput_fromtop(ns("data_div"),"", choices=NULL))
                      ),


                      uiOutput(ns('try_chao_fisher'))

                      ))),

      column(
        4,class="mp0",
        box_caret(ns('box1'),
                  title="Diversity indices",
                  color="#c3cc74ff",
                  div(
                    checkboxGroupInput(
                      ns("divInds"),
                      NULL,
                      choiceValues =list(
                        "N","S","margalef","D","H","Hlog2","Hlog10","J'","Dom_rel","Skewness",'Chao1','Fisher','BP'
                      ),
                      selected=c(
                        "N","S","margalef","D","H","Hlog2","Hlog10","J'","Dom_rel","Skewness",'Chao1','Fisher','BP'
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
                        span("Skewness",actionLink(ns('Skhelp'),icon("fas fa-question-circle")),uiOutput(ns("Skhelp"))),
                        span("Chao1",actionLink(ns('Chao1help'),icon("fas fa-question-circle")),uiOutput(ns("Chao1help"))),
                        span("Fisher",actionLink(ns('Fisherhelp'),icon("fas fa-question-circle")),uiOutput(ns("Fisherhelp"))),
                        span("BP",actionLink(ns('BPhelp'),icon("fas fa-question-circle")),uiOutput(ns("BPhelp")))
                      )


                    )
                  )
        )

      ),
      column(
        8,class="mp0",
        box_caret(ns('box2'),
                  title="Table",
                  button_title = actionLink(ns("download_table"),"Download",icon("download")),
                  div(style="width: 100%",


                      div(align="right",
                          span(actionLink(ns("tools_savediv"), "Create Datalist"),tiphelp("Create Datalist with Diversity results"))
                      ),
                      uiOutput(ns('div_results'))


                  )
        )

      )

    ),
    tabPanel('2. Niche Analysis',value="tab_omi",

             box_caret(
               ns("box3"),inline=F,
               color="#374061ff",
               title="Model Setup",
               div(
                 div(style="display: flex;",class="setup_box picker-flex",

                     div(style="display: flex;",
                         div(tipify(tags$div("Y",class="trailab"),"Predictors")),
                         pickerInput(ns("omi_X"),NULL, choices=NULL)),
                     div(
                       div(strong("~")),
                       div(actionLink(ns("rev_rda"),icon("arrow-right-arrow-left"),style="padding-top: -10px")),
                       style="height: 30px; width: 30px; padding-top: -10px;padding-left:10px; margin-left: -20px"),
                     div(style="display: flex;",
                         div(tipify(tags$div("X",class="trailab"),"Response data")),
                         pickerInput(ns("omi_Y"),NULL, choices=NULL)),
                     actionButton(ns('run_niche'),"RUN")

                 )

               )

             ),

             column(
               4,class="mp0",
               box_caret(
                 ns("box4"),
                 title="Options",
                 color="#c3cc74ff",
                 div(div(class="radio_search radio_yellow",
                         radioGroupButtons(ns("show_niche"), "Show", choices = c("Plot","Table"))),

                     pickerInput(ns("omi_result"),'Result',choices=c('Niche params',"species coordinates","variable coordinates","Site coordinates","Axis upon niche axis","EBNB"),selected="EBNB"),
                     uiOutput(ns("ebnb_pc"))
                 )
               ),
               div(id=ns("ebnb_obsel"),
                   box_caret(
                     ns("box6"),
                     title="Observation selection",
                     color="#c3cc74ff",
                     DT::dataTableOutput(ns('observation_selection'))

                   )
               )

             ),
             column(8,class="mp0",
                    box_caret(
                      ns("box5"),
                      title="Results",
                      button_title = span(actionLink(ns('downp_perf'),"Download",icon('download')),actionLink(ns('omi_down'),'Download',icon('download'))),
                      div(align="right", tipify(actionLink(ns('omi_save_datalist'),span("Create Datalist",icon("fas fa-file-signature"))),"Create a datalist with the results", options=list(container="body")),
                          uiOutput(ns('omi_plot1')),
                          uiOutput(ns('omi_tables'))

                      )
                    ))

    )
  ))

}
#' @export
diversity_tool$server<-function (id,vals,df_colors,newcolhabs,df_symbol ){

  moduleServer(id,function(input,output,session){

    observeEvent(input$rev_omi,{
      updatePickerInput(session,"omi_Y",selected=input$omi_X)
      updatePickerInput(session,"omi_X",selected=input$omi_Y)
    })


    getdata_div<-reactive({
      req(input$data_div)
      data=vals$saved_data[[input$data_div]]

      data
    })
    palette=c(
      "turbo",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"
    )
    symbols<-c("pch1","pch2","pch3","pch4")

    dfcolors<-data.frame(
      val = palette
    )




    output$try_chao_fisher<-renderUI({
      tryfisher<-vals$tryfisher
      trychao<-vals$trychao
      req(length(c(tryfisher,trychao))>0)
      war<-paste(paste(c(tryfisher,trychao),collapse=" and "),"indices require integer values and were not calculated due to non-integer data")
      div(style="padding: 5px; font-size:11px",
          class = "alert_warning",
          icon("triangle-exclamation",style="color: Dark yellow3"),
          war
      )

    })

    ns<-session$ns
    output$div_results<-renderUI({
      div(class="half-drop-inline",style="max-width: 100%;overflow-x: auto",
          fixed_dt( divI(),scrollY="300px")
      )
    })

    box_caret_server("box1")
    box_caret_server("box2")
    observeEvent(ignoreInit = T,input$download_table,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      name<-paste0("Diversity_",input$data_div)
      data<-divI()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Diversity restuls",data=data, name=name)
    })
    observeEvent(vals$saved_data,{
      updatePickerInput(session,"data_div",choices = names(vals$saved_data), selected=vals$cur_data)
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
    output$Chao1help <- renderUI({
      req(input$Chao1help %% 2)
      column(12, style = "background: white",
             strong("Chao1 Index"),
             p("The Chao1 index is an estimate of species richness that adjusts for rare species. It accepts only integers (counts). The formula for the Chao1 index is:",
               withMathJax(helpText("$$ \\text{Chao1} = S_{\\text{observed}} + \\frac{F_1^2}{2F_2} $$")),
               p("where \\( S_{\\text{observed}} \\) is the number of observed species, \\( F_1 \\) is the number of species observed only once, and \\( F_2 \\) is the number of species observed twice.")
             )
      )
    })

    # Help for Berger-Parker Index
    output$BPhelp <- renderUI({
      req(input$BPhelp %% 2)
      column(12, style = "background: white",
             strong("Berger-Parker Index"),
             p("The Berger-Parker index measures the dominance of the most abundant species in a community. It is defined as the proportion of the total sample that is composed of the most abundant species. The formula for the Berger-Parker index is:",
               withMathJax(helpText("$$ BP = \\frac{N_{\\text{max}}}{N} $$")),
               p("where \\( N_{\\text{max}} \\) is the number of individuals of the most abundant species, and \\( N \\) is the total number of individuals in the community.")
             )
      )
    })

    # Help for Fisher's Alpha
    output$Fisherhelp <- renderUI({
      req(input$Fisherhelp %% 2)
      column(12, style = "background: white",
             strong("Fisher's Alpha"),
             p("Fisher's alpha is a diversity index that is derived from the log-series distribution. It accepts only integers (counts). The index is calculated using maximum likelihood estimation from the observed species abundance data.")
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

    divI<-reactive({

      req(input$divInds)
      abund=getdata_div()


      validate(need(!any(colSums(sapply(abund,function (x) x<0))>0),"Data must be non-negative"))

      choices=input$divInds
      res=list()
      if("N"%in%choices){res$N<-rowSums(abund)}
      if("S"%in%choices){res$S<-specnumber(abund)}
      if("margalef"%in%choices){res$margalef=(specnumber(abund)-1)/log(rowSums(abund))}
      if("D"%in%choices){res$D<-diversity(abund, index="simpson")}
      if("H"%in%choices){res$H<-diversity(abund)}
      if("Hlog2"%in%choices){res$Hlog2<-diversity(abund, base=2)}
      if("Hlog10"%in%choices){res$Hlog10<-diversity(abund, base=10)}
      if("J'"%in%choices){
        H<-diversity(abund)
        S<-specnumber(abund)
        res$J<-H/log(S)}

      if("Dom_rel"%in%choices){res$Dom_rel<-apply(decostand(abund, "total"),1,sort,T)[1,]}
      if("Skewness"%in%choices){res$Skewness=apply(abund,1,skewness)}
      if ("Chao1" %in% choices) {
        trychao<-try( res$Chao1 <- vegan::estimateR(abund)["S.chao1",] )
        vals$trychao<-NULL
        if(inherits(trychao,"try-error")){
          vals$trychao<-"Chao1"
        }
      }
      if ("Fisher" %in% choices) {
        tryfisher<-try( res$Fisher <- vegan::fisher.alpha(abund) )
        vals$tryfisher<-NULL
        if(inherits(tryfisher,"try-error")){
          vals$tryfisher<-"Fisher"
        }
      }
      if ("BP" %in% choices) {
        res$BP <- apply(abund, 1, function(x) max(x) / sum(x))
      }
      res<-data.frame(do.call(cbind,res))

      #rowSums(apply(data,2,function(x)x==1))

      res
    })





    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      updatePickerInput(session,'omi_Y',choices=choices,selected=choices[2])
      updatePickerInput(session,'omi_X',choices=choices,selected=choices[1])
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


    observeEvent(ignoreInit = T,input$niche_tabs,{
      vals$niche_tabs<-input$niche_tabs
    })


    observe({
      shinyjs::toggle("ebnb_obsel",condition=input$show_niche=="Plot")
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
      span("PC",inline( pickerInput(ns("ebnb_pc"),NULL,choices=c(1:ncol(vals$omi_result$ls)), width="50px")))

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
        dudi1<-dudi.pca(x, scale = F, scannf  = FALSE)
        nic1<-niche(dudi1, data.frame(y), scannf = FALSE)
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

    observeEvent(input$omi_result,{
      choices<-if(input$omi_result=="EBNB"){
        c("Plot","Table")
      } else{
        "Table"
      }
      updateRadioGroupButtons(session,'show_niche',choices=choices)
    })





    observe({
      shinyjs::toggle("omi_down",condition=input$show_niche!="Plot")

      shinyjs::toggle("downp_perf",condition=input$show_niche=="Plot")
    })

    observeEvent(ignoreInit = T,input$downp_perf,{
      vals$hand_plot<-"EBNB"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals)
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
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
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
    module_div<-function() {
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


  })

}

