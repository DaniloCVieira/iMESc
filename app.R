
list.of.packages <- c('shinydashboard','shinydashboardPlus','shinyjs','shiny',"e1071",'readxl','vegan',"party",'caret','viridisLite','aweSOM','sp','raster','rasterVis','Rcpp','rgdal','gstat','ggspatial','ggplot2','sf','class','shinyWidgets', 'randomForestExplainer','data.table',"ggpubr", "shinyBS","terra","purrr","NbClust", "colorRamps","DBI","shinyBS","wesanderson","colorspace","gplots","dendextend","kohonen","shinypanels","writexl","DT","gbRd")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

library("colorspace")
library("writexl")
library(wesanderson)
library(e1071)
library(DBI)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shiny)
library(readxl)
library(vegan)
library(caret)
library(viridisLite)
library(aweSOM)
library(sp)
library(raster)
library(rasterVis)
library(rgdal)
library(gstat)
library(ggspatial)
library(ggplot2)
library(sf)
library(class)
library(shinyWidgets)
library(randomForestExplainer)
library(data.table)
library(purrr)
library(NbClust)
library("party")
library(shinyBS)
source('meta/funs_texts.R')
library(ggpubr)
library("colorRamps")
library(kohonen)

library("gplots")
library(dendextend)
source("meta/funs_pre_treat.R")
source("meta/funs_sominter.R")
source("meta/funs_somplot.R")
source("meta/funs_mapplot.R")
source("meta/funs_rf_inter.R")
source("meta/BC.R")


#file.remove(paste0(getwd(), "/", 'bookmarks.rds'))
js<-'$(document).keyup(function(event) {
    if (event.keyCode == 13) {
        $("#data_confirm").click()'



convertMenuItem <- function(mi, tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if (length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
    mi$attribs$class = NULL
  }
  mi
}

b64 <- base64enc::dataURI(file = "meta/logo.png", mime = "image/png")

Esquema <- base64enc::dataURI(file = "meta/Esquema.png", mime = "image/png")
dbHeader <- dashboardHeader(
  title = span(img(
    src = b64,
    height = '38',
    width = '38',style ="margin-bottom: 7px;",
  ), "iMESc",style = "color: white; margin-left: -10px; font-size: 22px; font-family: 'Alata', sans-serif;margin-bottom: 2px;"),
  titleWidth = 150
)

ui <- function(request){

  dashboardPage(
    skin = "blue",
    dbHeader,
    dashboardSidebar(
      extendShinyjs(
        text = "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}",
functions = 'collapse'
      ),
useShinyjs(),
width = 150,
tags$head(tags$style(
  HTML("
pre { white-space: pre-wrap; word-break: keep-all; }
@import url('https://fonts.googleapis.com/css2?family=Alata&display=swap');
@import url('https://fonts.googleapis.com/css2?family=Bevan&display=swap');


#Select1{border: 1px solid #dd4b39;}


 #transform_button .selectize-control.single .selectize-input:after{
          content: none;}


  .shiny-split-layout>div {overflow: visible;}
      h3{font-weight: bold;}
      label{ color: #0D47A1; font-size: 12 px;}
      .box { margin-left: -5px; margin-right: -10px;  margin-bottom: 10px; margin-top: -3px; }
      .box-body { margin-left: -5px; margin-right: -10px; margin-bottom: 10px;margin-top: 15px;  }
       /* blue */

      .skin-blue .main-header .logo {background-color: #05668D;;}
       /* green */
      .skin-blue .main-header .navbar {background-color: #05668D;}
      .skin-blue .main-sidebar {background-color: #F5F7FA;}

  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              color: #05668D;
                              }
 .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: SeaGreen;
 }
     .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: SeaGreen;
                          color: rgb(0,144,197);font-weight: bold;
     }

  "

  )
)),
sidebarMenu(
  id = "tabs",

  div(
    style = "background-color:  #05668D",
    class = "sidebar-menu",
    menuItem(
      div(icon("fas fa-home"), HTML('&nbsp;'), "Introduction", style = "margin-left: -8px; color: white; font-size: 15px; margin-top: 0px;margin-bottom: 0px;"),
      tabName = "menu_intro"
    ),
    menuItem(
      div(icon("fas fa-database"), HTML('&nbsp;'), "Data Bank", style = "margin-left: -8px; color: white; font-size: 15px; margin-top: 0px;margin-bottom: 0px;"),
      tabName = "menu_upload"
    ),
    menuItem(
      div(
        icon("fas fa-braille"),
        HTML('&nbsp;'),
        "Self-Organing",
        p("Maps", style = "margin-left: 30px"),
        style = "margin-left: -8px; color: white; font-size: 15px; margin-top: 0px;margin-bottom: 0px;"
      ),
      tabName = "menu_som"
    ),
    menuItem(
      div(
        icon("fas fa-network-wired"),
        HTML('&nbsp;'),
        "Hierarchical",
        p("Clustering", style = "margin-left: 30px"),
        style = "margin-left: -8px; color: white; font-size: 15px; margin-top: 0px;margin-bottom: 0px;"
      ),
      tabName = "menu_hc"
    ),
    menuItem(
      div(
        icon("fas fa-tree"),
        icon("fas fa-tree", style = "margin-left: -10px;"),
        HTML('&nbsp;'),
        "Random",
        p("Forest", style = "margin-left: 30px"),
        style = "margin-left: -8px; color: white; font-size: 15px; margin-top: 0px;margin-bottom: 0px;"
      ),
      tabName = "menu_rf"
    ),
    menuItem(
      div(
        icon("fas fa-tree"),
        icon("fas fa-dyalog", style = "margin-left: -20px;  font-size: 15px"),
        HTML('&nbsp;'),
        "Decision",
        p("Tree", style = "margin-left: 30px"),
        style = "margin-left: -8px; color: white; font-size: 15px; margin-top: 0px;margin-bottom: 0px;"
      ),
      tabName = "menu_dt"
    ),

    menuItem(
      div(icon("fas fa-map"), HTML('&nbsp;'), "Spatial tools", style = "margin-left: -8px; color: white; font-size: 15px; margin-top: 0px;margin-bottom: 0px;"),
      tabName = "menu_maps"
    ),

    menuItem(
      div(
        icon("fas fa-pencil-ruler"),
        HTML('&nbsp;'),
        "Diversity tools",
        style = "margin-left: -8px; color: white; font-size: 15px; margin-top: 0px;margin-bottom: 0px;"
      ),
      tabName = "menu_div"
    )
  )
)),
dashboardBody(

  fluidRow(

    div(
      tags$head(tags$style(
        HTML(
          "
          .btn:hover{
  background-color: SeaGreen;
   color: white
          }


  #shp{height: 100px;font-size: 20px}


.btn:focus{
  background-color: SeaGreen;
   color: white
          }


         .btn-button_active.active {
  background-color: SeaGreen;
  color: white;
         }



          .myClass {
        font-size: 15px;
        line-height: 50px;
        text-align: left;
        font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
    }



    "
        )
      )),
    tags$script(
      HTML(
        '
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> An Interactive Machine Learning App for Environmental Science </span>\');
      });



     '
      )
    ),
tags$script("$(document).on('shiny:connected', function(event) {
var myWidth = $(window).width();
Shiny.onInputChange('shiny_width',myWidth)

});"),

tags$script("$(document).on('shiny:connected', function(event) {
var myHeight = $(window).height();
Shiny.onInputChange('shiny_height',myHeight)

});"),

    fluidPage(
      useShinyjs(),
      div(style = "margin-left: -15px; margin-right:-15px;",
          fluidRow(
            column(6,uiOutput("menutitle")),
            column(6,  style = "margin-top: -10px;margin-bottom: 0px; ", align = "right", actionButton("dialog",strong("Savepoints"),style="font-size: 10px; background: SeaGreen; color: white", icon=icon("fas fa-download")))
          ),
          tabsetPanel(
            type = "hidden",
            tabPanel(
              "intro",
              tabItems(
                tabItem(tabName = "menu_intro",
                        tabsetPanel(
                          tabPanel(value="intro1",
                                   strong("Welcome"),
                                   textintro()
                          ),
                          tabPanel(strong("Authors"),value="intro2", textauthors()),
                          tabPanel(strong("Workflow"),value="intro3",
                                   column(
                                     12,
                                     br(),
                                     img(
                                       src = Esquema,
                                       height = '390',
                                       width = '720'
                                     )
                                   )
                          ),
                          tabPanel(
                            strong("Running offline"),value="intro4",
                            column(12,
                                   column(12, br(), textoffline()))

                          ),
                          tabPanel(strong("Version"),value="intro5",
                            column(12, style="margin-top 25px",
                                   h4(style="margin-top 25px",span("iMESc", style="font-family: 'Alata', sans-serif;")),
                                   p(strong("Version: 1.1.0")), p("06/10/2021"),
                              p("developed by Danilo C Vieira in  R, version 4.0.5; RStudio, version 1.4, and Shiny package, version 1.6.0."),



                            )
                          )
                        )
                ),
                # training panel
                tabItem(
                  tabName = "menu_upload",
                  uiOutput("menu_upload_out")

                ),
                tabItem(tabName = "menu_som",
                        column(12,
                               uiOutput("som_control"),

                        ),
                        uiOutput("som_panels")
                ),
                tabItem(tabName = "menu_hc",
                        uiOutput('clustering_panel')
                ),
                tabItem(tabName = "menu_rf",
                        column(12,uiOutput("choices_rf")),

                        uiOutput("rf_panel")
                ),
                tabItem(tabName = "menu_dt",

                        uiOutput("dt_panel")
                ),
                tabItem(
                  tabName = "menu_maps",
                  style = "margin-right: -10px;",
                  column(12, uiOutput("choices_map")),
                  div(),
                  column(12,
                         style="margin-top: 5px",
                         uiOutput("map_control")),
                  column(12, uiOutput("map_out"))
                ),
                tabItem(tabName = "menu_div",
                        column(12,
                               uiOutput("divtabs")))


              )
            )
          )


      )
    )

    )


  )))
}






server <- function(input, output, session) {


  output$som_panels<-renderUI({
    req(length(saved_data$df)>0)
    tabsetPanel(
      id = "som_tab",
      tabPanel(
        strong("1. Training"), value = "som_tab1",
        uiOutput("training_panel")

      ),
      tabPanel(value = "som_tab2",
               strong("2. Results"),
               uiOutput("results_panel"))


    )
  })



  observeEvent(input$train_or_load,{
    switch (input$train_or_load,
            "Train data" =  updateTabsetPanel(session, "som_tab", "som_tab1"),
            "Saved results"=updateTabsetPanel(session, "som_tab", "som_tab2")
    )
  })


  output$tags_data<-renderUI({
    data=saved_data$df[[input$getdata_down]]
    if(input$down_choices=="data"){
      filename<-names(saved_data$df)[which(names(saved_data$df)==input$getdata_down)]
      res<-data.frame(c(filename,attr(data, "fingerprint_data"
      )))
      colnames(res)<-NULL
      res}

  })


  options(shiny.maxRequestSize=30*1024^2)
  saved_base_shape<-  reactiveValues(df=0)
  saved_layer_shape<-  reactiveValues(df=0)
  saved_coords<-  reactiveValues(df=0)
  saved_box<-  reactiveValues(df=0)
  cur_tab<-  reactiveValues(df=0)

  rf_sigs<-reactiveValues(df=0)
  saved_model <- reactiveValues()
  bagdata <- reactiveValues(df = 0)
  bagmodel <- reactiveValues(df = 0)
  baghc <- reactiveValues(df = F)
  baghc0 <- reactiveValues(df = 0)
  saved_labels <- reactiveValues()
  datalist1_book <- reactiveValues(df=F)
  datalist2_book <- reactiveValues(df=F)
  datalist3_book <- reactiveValues(df=F)
  phc_book <- reactiveValues(df=F)
  saved_data <- reactiveValues()
  saved_kdataWSS <- reactiveValues()
  saved_kdataaccRF <- reactiveValues()
  saved_kmodelaccRF <- reactiveValues()
  saved_kmodelWSS <- reactiveValues()
  saved_kcustom <- reactiveValues()
  cur_data<-reactiveValues(df=0)


  saved_kmodelvotes <- reactiveValues()
  saved_kdatavotes <- reactiveValues()
  loaded <- reactiveValues(df=FALSE)

  bagdataraw<-reactiveValues(df=0)
  bagshp<-reactiveValues(df=F)
  insertab2<-reactiveValues(df=F)
  insertab3<-reactiveValues(df=F)
  saved_dataset_value <- reactiveValues(df = 0)
  observe(saved_dataset_value$df<-isolate(length(saved_data$df)+1))
  saved_dataset_names <- reactiveValues(df = 0)

  saved_divset_value <- reactiveValues(df = 0)
  observe(saved_divset_value$df<-isolate(length(saved_data$df)+1))
  saved_divset_names <- reactiveValues(df = 0)

  saved_modelset_value <- reactiveValues(df = 0)
  observe(saved_modelset_value$df<-isolate(length(saved_model$df)+1))
  saved_modelset_names <- reactiveValues(df = 0)



  saved_bmu <- reactiveValues()
  saved_bmu_value <- reactiveValues(df = 0)
  observe(saved_bmu_value$df<-isolate(length(saved_bmu$df)+1))
  saved_bmu_names <- reactiveValues(df = 0)


  savepoint<-reactiveValues(df=0)
  savepoint2<-reactiveValues(df=0)
  urlDF<-reactiveValues(df=0)
  factors_palette<-reactiveValues(df=0)
  upload_bag<-reactiveValues(df=0, df0=0)



  myBookmarks<-reactiveValues(

    saved_model=0,
    bagdata=0,
    bagmodel=0,


    saved_labels=0,
    datalist1_book=0,
    datalist2_book=0,
    datalist3_book=0,
    phc_book=0,
    saved_data=0,
    saved_kdataWSS=0,
    saved_kdataaccRF=0,
    saved_kmodelaccRF=0,
    saved_kmodelWSS=0,
    saved_kcustom=0,
    saved_base_shape=0,
    saved_layer_shape=0,
    saved_coords=0,
    saved_kmodelvotes=0,
    saved_kdatavotes=0,
    loaded=0,
    insertab1=T,
    insertab2=F,
    insertab3=F,
    saved_dataset_value = 0,
    saved_dataset_names =0,
    savepoint=0,
    savepoint2=0,
    urlDF=0,
    factors_palette=0
  )


  getdata_som <- reactive({

    data=saved_data$df[[input$data_som]]
    saved_labels$df<-attr(data, "factors")
    validate(need(length(data)>0,"no data found"))
    cur_data$df<-input$data_som

    data
  })
  getdata_hc <- reactive({
    data=saved_data$df[[input$data_hc]]
    validate(need(length(data)>0,"no data found"))
    saved_labels$df<-attr(data, "factors")
    cur_data$df<-input$data_hc

    data
  })
  getdata_rf01 <- reactive({
    data=saved_data$df[[input$data_rf01]]
    data
  })
  getdata_rf02 <- reactive({
    data=saved_data$df[[input$data_rf02]]
    saved_labels$df<-attr(data, "factors")
    cur_data$df<-input$data_rf02
    data
  })

  getdata_map <- reactive({

    data=saved_data$df[[input$data_map]]
    saved_labels$df<-attr(data, "factors")
    saved_layer_shape$df<-attr(data,"layer_shape")
    saved_base_shape$df<-attr(data,"base_shape")
    saved_coords$df<-attr(data,"coords")

    data
  })

  getdata_div<-reactive({
    data=saved_data$df[[input$data_div]]
    saved_labels$df<-attr(data, "factors")
    cur_data$df<-input$data_div
    data
  })



bagsom<-reactiveValues(df=0)
observe(bagsom$df<-som.reactive())

  getsom <- reactive({
    if(input$train_or_load=="Saved results")
    {
      res=attr(saved_data$df[[input$data_som]],"som")[[input$som_models]]
      validate(need(class(res)=="kohonen",paste("no Som-Attribute saved in", input$data_som)))
    } else {res=bagsom$df
    validate(need(class(res)=="kohonen","Please go back to 'Training' and press the 'train SOM' button"))
    }

    res
  })

  getmodel_hc <- reactive({

    attr(getdata_hc(),"som")[[as.character(input$som_hc)]]
  })



  getbox <- reactive({
    data=getdata_upload()
    labels <- attr(data,"factors")
    pic<-1:nrow(data)
    x <- labels[as.character(input$box_factor)]
    y <- data[input$box_y]

    if (input$filter_box1 != "none") {
      filtro <- as.character(input$filter_box1)
      filtro2 <- as.character(input$filter_box2)
      pic <- which(as.character(labels[, filtro]) == filtro2)


    }
    x[,1]<-as.factor(as.character(unlist(x)))
    res = cbind(x,y)[pic,]


    #rownames(res)<-rownames(saved_labels$df)[pic]

    res



  })





  palette=c(
    "matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat","black","gray","royalblue", "firebrick","forestGreen",'goldenrod3'
  )
  symbols<-c("pch1","pch2","pch3","pch4")

  df <- data.frame(
    val = palette
  )


  for(i in 1:length(palette))  {
    palette1<-base64enc::dataURI(file = paste0('meta/palette',i,".png"),
                                 mime = "image/png")
    df$img[i]<- sprintf(paste0(
      img(src = palette1, height = '20',width = '70',
          style="margin-top:-40px;margin-bottom:-40px;margin-left:-20px;margin-right:-20px;") ), df$val[i])}

  colors_solid<-df[c(8:13),]
  colors_gradient<-df[c(1:7),]

  symbols<-c("pch1","pch2","pch3","pch4")
  df_symbol <- data.frame(
    val = c(16,15,17,18)
  )
  for(i in 1:length(symbols))
  {
    symbol1<-base64enc::dataURI(file = paste0('meta/pch',i,".png"), mime = "image/png")

    df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')), df_symbol$val[i])}




  gettitle<-reactive({
    column(12,style="color: #05668D",
           if(input$tabs=="menu_intro"){h4(strong("Introduction"))} else if(input$tabs=="menu_upload"){h4(strong("Data Bank"))} else if(input$tabs=="menu_som"){
                 h4(strong("Self-Organizing Maps"), actionLink(
                   'introhelp', icon("fas fa-info-circle")
                 ))} else if(input$tabs=="menu_hc"){h4(
                     strong("Hierarchical Clustering"),
                     actionLink('hclusthelp', icon("fas fa-info-circle"))
                   )} else if(input$tabs=="menu_rf"){
                     h4(
                       strong("Random Forest"),
                       actionLink('rfhelp', icon("fas fa-info-circle"))
                     )
                   } else if(input$tabs=="menu_maps"){h4(strong("Spatial tools"))} else if(input$tabs=="menu_box"){h4(strong("Box plots"))} else if(input$tabs=="menu_div"){ h4( strong("Biological diversity indices"))} else if(input$tabs=="menu_down"){h4(strong("Download Center"))} else if(input$tabs=="menu_data"){h4(strong("Data bank"))} else if(input$tabs=="menu_dt"){h4(strong("Decision Tree"),actionLink(
                     "ctreehelp", tipify(icon("fas fa-question-circle"), "Click for more details")
                   ))}
    )
  })



  output$upload_example<-renderUI({
    radioButtons("up_or_ex",NULL,choiceValues  = list("upload", "use example data"),choiceNames   = list(tipify(div("upload"),"upload your own data",placement = "top", options=list(container="body")), tipify(div("use example data"),"Use Nematode datasets from Araca Bay as example",placement = "top", options=list(container="body"))),selected = "upload", inline = T)
  })


  upload_modal <- reactive({

    modalDialog(

      uiOutput("upload_panel"),
      title = strong("Insert Data",
                     popify(actionLink("uphelp0",tipify(icon("fas fa-question-circle"),"click for help")),"Data bank structure",as.character(paste0(
                       "Each Datalist in the Databank requires at least two files: the ",code("Data-Attribute")," (observations) and the ",code('Factor-Attribute'),". Optionally, the user can upload files containing spatial information (coordinates, base and layer shape). More information in the help icons on the label of each input."
                     )),trigger="hover", options=list(container="body"))),
      footer = column(12,uiOutput("upload_control")),
      size = "l",
      easyClose = TRUE
    )

  })
  output$upload_panel<-renderUI({
    column(12,style = "background: white; height:550px",
           br(),
           uiOutput("upload_content")

    )
  })


  output$upload_control<-renderUI({upload_control()})

  upload_control<-reactive({

    if(length(getdatalist())>0){
      disable=F
    } else { disable=T }
    column(6,offset=6,align="right",

           column(6,
                  if(upload_bag$df==1){actionButton("back_upload","Back", width="125px")},
                  if(upload_bag$df==0){column(12,offset=12,

                                              bsButton("next_upload","Continue", width="125px", disabled=disable))}),
           column(6,
                  if(upload_bag$df==1){ uiOutput("upload_insert") }
           )
    )

  })


  observeEvent(input$next_upload, {
    upload_bag$df<-1


  })

  observeEvent(input$back_upload, {
    upload_bag$df<-0
  })


  output$upload_insert<-renderUI({
    actionButton("upload_insert","Insert datalist", width="125px")

  })
  output$upload_content<-renderUI({
    if(upload_bag$df==0){ uiOutput("upload_input")} else if(upload_bag$df==1){ uiOutput("data_list")}
  })

output$textbreak<-renderText("This action creates a single binary column per factor level, with 1 indicating the class of that particular observation")
  output$pop_classmat<-renderUI({
    column(12,style="background: white",
           column(12,
                  h5(column(12,strong("Factors were removed from the Data-Attribute of ",code(input$data_upload))),
                     column(12,actionLink("show_fsummary", "Click here"),
                       " to show/hide diagnosis of the ommited factors."),
                     column(12,"A Data-Attribute can handle only numeric variables"),
                     column(12,"To insert the omitted factors, use the buttom", code("insert_classMat")),
                     column(12,textOutput("textbreak"),
                     tags$style(type="text/css", "#textbreak {white-space: pre-wrap;}"))
                     ,
                     column(12,actionLink("classmat_preview", "Click here"),
                       " to show/hide a preview"),


                  ),

                  column(12,align="center",style="margin-top: 20px",
                         splitLayout(
                           actionButton("insert_classMat",strong('insert_classMat')),
                           column(12,
                                  em())
                         )),
                  #p("Any changes you make in this Panel affects the current Datalist, until you close the Panel"),
                  conditionalPanel("input.classmat_preview % 2",{
                    fluidRow(column(12,
                                    h5(strong(
                                      "Preview"
                                    )),
                                    classmat_out()))
                  }),
                  conditionalPanel("input.show_fsummary % 2",{
                    fluidRow(column(12,
                                    h5(strong(
                                      "Counting of factors:"
                                    )),
                                    renderPlot(pfac(
                                      attr(getdata_upload(), 'data.factors')
                                    ))))
                  })
           )

    )
  })


  datahand<-reactiveValues(df=0)
  datahand2<-reactiveValues(df=NULL)
  datahand3<-reactiveValues(df=NULL)
  datahand_modal<-reactive({

    modalDialog(
                column(12,
                       fluidRow(
                         column(12,p(strong("action:"),em("*",datahand$df,style="color: SeaGreen")), p(datahand2$df,style="color: gray")),
                         column(12,style='margin-top: 10px; margin-left: 10px',
                                splitLayout(cellWidths = c("30%","70%"),
                                            radioButtons("datahand",NULL,
                                                         choiceNames= list(div(style="height: 50px","create"),
                                                                           div(style="height: 50px","overwrite")),
                                                         choiceValues=list('create',"over")),
                                            column(12,div(style="height: 50px",
                                                          uiOutput("data_create")),
                                                   div(style="height: 50px",
                                                       uiOutput("data_over")))
                                )),
                         column(12,datahand3$df)
                       )
                ),
                title=strong('Databank storage',icon("fas fa-warehouse")),
                footer=column(12,
                              fluidRow(tipify(bsButton("preview_button",icon("fas fa-eye"), block=F),"Preview"), modalButton(strong("cancel")),
                                       actionButton("data_confirm",strong("confirm"))
                              ),
                              conditionalPanel("input.preview_button % 2",{
                                column(12,style="margin-top: 20px",align="left",
                                       column(12,strong("Preview")),
                                       uiOutput("preview"))
                              })),
                size="l",
                easyClose = T
    )
  })



  observeEvent(input$insert_classMat,{
    datahand$df<-"Include binary columns in the Data-Attribute"
    showModal(
      datahand_modal()
    )})



  observeEvent(input$tools_save,{
    datahand$df<-"Save changes"
    datahand2$df<-NULL
    datahand3$df<-NULL
    showModal(
      datahand_modal()
    )})



  observeEvent( input$data_confirm,{

    data=getdata_upload()
    if(datahand$df=="Create a datalist from the selected observations")
    {
      facs<-attr(data,"factors")
      coords<-attr(data,"coords")
      data<-data[input$x1_factors_rows_selected,]
      if(!is.null(coords)){coords<-coords[input$x1_factors_rows_selected,]}
      facs<-facs[input$x1_factors_rows_selected,]
      attr(data,"factors")<-facs
      attr(data,"coords")<-coords
      if(input$datahand=="create") {
        saved_data$df[[input$selobs_newname]]<-data
        cur_data$df<-input$selobs_newname
      } else{
        saved_data$df[[input$selobs_over]]<-data
        cur_data$df<-input$selobs_over
      }


    }

    if(datahand$df=="Create a datalist with the variables selected in the Random Forest Explainer"){
      temp<-getdata_rf02()[,rf_sigs$df$variable]
      temp<-data_migrate(getdata_rf02(),temp,"newdata_rf")
      if(input$datahand=="create") {
        saved_data$df[[input$rf_newname]]<-temp
        cur_data$df<-input$rf_newname
      } else{
        saved_data$df[[input$rf_over]]<-temp
        cur_data$df<-input$rf_over
      }
    }

    if(datahand$df=="Save diversity results"){
      divInds<-divI()
      data<-getdata_div()
      temp<-data_migrate(data,divInds,input$div_newname)
      if(input$datahand=="create"){
        saved_data$df[[input$div_newname]]<-temp
        cur_data$df<-input$div_newname
      } else{
        saved_data$df[[input$div_over]]<-temp
        cur_data$df<-input$div_over
      }
    }

    if(datahand$df=="Create data list from aggregation results")
    {
      temp<-aggreg()
      if(input$datahand=="create"){
        saved_data$df[[input$agg_newname]]<-temp
        cur_data$df<-input$agg_newname
      } else{
        saved_data$df[[input$agg_over]]<-temp
        cur_data$df<-input$agg_over
      }
    }


    if(datahand$df=="Include binary columns in the Data-Attribute"){
      temp<-cbind(data,getclassmat(attr(data,"data.factors")))

    }

    if(datahand$df=="Save changes"){
      temp<-getdata_upload()}
    if(datahand$df=="Save newsom in"){
      temp<-getsom()
      bmu<-temp$unit.classif
      names(bmu)<-rownames(temp$data[[1]])
    }

    if(datahand$df=="Save Clusters"){
      baghc0$df<-baghc0$df+1
      hc <- phc()
      temp <- hc$somC
      if(input$datahand=="create"){
        attr(saved_data$df[[input$data_hc]],"factors")[names(temp),input$hc_newname]<-temp
      } else{
        attr(saved_data$df[[input$data_hc]],"factors")[input$hc_over]<-temp
      }

    }



    if(datahand$df=="Save changes"|datahand$df=="Include binary columns in the Data-Attribute"){
      temp<-data_migrate(data,temp,input$data_newname)


      if(input$datahand=="create"){
        saved_data$df[[input$data_newname]]<-temp
        cur_data$df<-input$data_newname
      } else{
        saved_data$df[[input$data_over]]<-temp
        cur_data$df<-input$data_over
      }
    }

    if(datahand$df=="Save newsom in"){

      bagmodel$df<-FALSE
      if(input$datahand=="create"){


        temp<-list(temp)

        names(temp)<-input$model_newname
        attr(saved_data$df[[input$data_som]],"som")[input$model_newname]<-c(temp,attr(saved_data$df[[input$data_som]],"som")[[input$model_newname]])
        attr(saved_data$df[[input$data_som]],"factors")[names(bmu),paste0('bmu_',input$model_newname)]<-bmu

      } else{

        temp<-list(temp)
        names(temp)<-input$model_over
        attr(saved_data$df[[input$data_som]],"som")[input$model_over]<-temp
        attr(saved_data$df[[input$data_som]],"factors")[names(bmu),input$model_over]<-bmu

      }
      updateRadioButtons(session,"train_or_load",NULL,choiceValues =list("Train data","Saved results"),choiceNames =list(
        popify(span("Train data"),NULL,("Train the Data-Attribute from the selected Datalist"), options=list(container="body")),
        popify(span("Saved results"),NULL,"See results already saved in the selected Datalist", options=list(container="body"))
      ), selected="Saved results")
      updateTabsetPanel(session, "som_tab", "som_tab2")
      updateTabsetPanel(session, "som_tab", "train_tab2")





    }

    shinyjs::reset('pop_transform')
    shinyjs::reset('selecvar')
    #shinyBS::updateButton(session,"tools_selvar",style  = "button_active", value=F)
    shinyBS::updateButton(session,"tools_transform",style  = "button_active", value=F)
    showModal(datahand_out())
  }

  )


  datahand_out<-reactive(
    modalDialog(
      if(datahand$df=="Include binary columns in the Data-Attribute"){
        classmat_out()
      } else{
        h4("Success")
      },
      size="s",
      easyClose = T
    )
  )



  output$data_create<-renderUI({
    req(input$datahand=="create")
    if(datahand$df=="Save newsom in"){textInput("model_newname", NULL,paste("Som", length(attr(getdata_som(),"som"))+1))} else
      if(datahand$df=="Save changes"|datahand$df=="Include binary columns in the Data-Attribute"){
        bag<-nrow(attr(getdata_upload(), "transf") )
        if(!length(bag)>0){bag<-1}
      textInput("data_newname", NULL,paste("Datalist_",gsub(".csv","",attr(getdata_upload(), "filename")) ,bag))} else
        if(datahand$df=="Save Clusters"){textInput("hc_newname", NULL,paste("hc", baghc0$df+1))}else
          if(datahand$df=="Create data list from aggregation results"){textInput("agg_newname", NULL,paste("Datalist_agg", length(saved_data$df)+1))}else
            if(datahand$df=="Save diversity results"){
        textInput("div_newname", NULL,paste("Div_results", length(saved_data$df)+1))}else
          if(datahand$df=="Create a datalist with the variables selected in the Random Forest Explainer"){
          textInput("rf_newname", NULL,paste("Rf_sigs", length(saved_data$df)+1))} else if(datahand$df=="Create a datalist from the selected observations"){
            bag<-nrow(attr(getdata_upload(), "transf") )
            if(!length(bag)>0){bag<-1}
            textInput("selobs_newname", NULL,paste("Datalist_",gsub(".csv","",attr(getdata_upload(), "filename")) ,bag))
          }



  })




  output$data_over<-renderUI({
    req(input$datahand=="over")
    if(datahand$df=="Save newsom in"){
      if(length(attr(getdata_som(),"som"))>0){
        selectInput("model_over", NULL,choices=c(names(attr(getdata_som(),"som"))),selectize = F)
      }else{
        column(12,verbatimTextOutput("nosommodel"))
      }
    }else if(datahand$df=="Save changes"|datahand$df=="Include binary columns in the Data-Attribute") { selectInput("data_over", NULL,choices=c(names(saved_data$df)),selectize = F)} else if(datahand$df=="Save Clusters"){
      selectInput("hc_over", NULL,choices=c(colnames(attr(getdata_hc(),"factors"))),selectize = F)
    }else if(datahand$df=="Create data list from aggregation results"){
      selectInput("agg_over", NULL,choices=c(names(saved_data$df)),selectize = F)
    } else if(datahand$df=="Save diversity results"){
      selectInput("div_over", NULL,choices=c(names(saved_data$df)),selectize = F)
    } else if(datahand$df=="Create a datalist with the variables selected in the Random Forest Explainer"){
      selectInput("rf_over", NULL,choices=c(names(saved_data$df)),selectize = F)
    }else if(datahand$df=="Create a datalist from the selected observations"){
      selectInput("selobs_over", NULL,choices=c(names(saved_data$df)),selectize = F)
    }

  })



  output$nosommodel<-renderPrint(
    cat("no som model in \n'",input$data_som,"' \n to overwritte")
  )


  classmat_out <- reactive({
    column(12,
           p(icon("fas fa-exclamation-circle"),"factors transformed into class membership matrix"),
           p(icon("fas fa-exclamation-circle"),"New variables created with class membership  indicated by '1'"),
           verbatimTextOutput("classmat_vars"),
           p(icon("fas fa-exclamation-circle"),"We advise using the scale option in the forward analyses"))
  })
  output$classmat_vars <- renderPrint({
    colnames(getclassmat(attr(getdata_upload(), 'data.factors')))
  })







  observeEvent(input$upload_button, {
    showModal(upload_modal())
  })

  observeEvent(input$layer_back, {
    showModal(upload_modal())
  })
  observeEvent(input$base_back, {
    showModal(upload_modal())
  })





  output$data_list<-renderUI({datalist_render(getdatalist()[[1]],F)})

  output$upload_input<-renderUI({
    column(12,style="background: white",
           uiOutput("upload_example"),
           uiOutput("upload_input2")

    )
  })

  output$upload_input2<-renderUI({
    fluidRow(
      fluidRow(
        splitLayout(
          absolutePanel(

            h5(strong("Required files:"), style="color: SeaGreen;"),
            absolutePanel(style="border-bottom: 2px dashed SeaGreen;border-left: 2px dashed SeaGreen;height: 25px;  margin-left: 20px; margin-right: -100px", width ="500px"),
            br(),
            br(),
            br(),
            h5(strong("Optional files:",style="color: #05668D")),
            em("*Required for the", style="margin-left: 10px"),
            p(em('spatial tools menu'), style="margin-bottom: -15px; margin-left: 10px"),
            absolutePanel(style="border-bottom: 2px dashed #05668D;height: 20px;  margin-left: 20px; margin-right: -60px", width ="100px")

          ),


          column(12,
                 style="color:  SeaGreen;",
                 h5(strong("Name the datalist")),
                 uiOutput("datalistname")
                 ,

                 column(12,div(style=" margin-left: 80px;",absolutePanel(style="border-left: 2px solid Lavender; height:500px;margin-top: -15px"))),

                 column(12,
                        div(style=" margin-left: 80px;",
                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed SeaGreen; height:50px; border-bottom: 2px dashed SeaGreen;", width='50px'),
                              column(12,
                                     h5(strong("Data:",style="color:  SeaGreen"),
                                        popify(actionLink('uphelp', icon("fas fa-question-circle")),"Upload the observations",textupload(), trigger = "hover",options=list(container="body"))),
                                     if(input$up_or_ex=='upload') {
                                       fluidRow(style="margin-bottom:-35px;",
                                              fileInput(inputId = "filedata",label = NULL,accept = c(".csv"), placeholder=if(length(input$filedata$datapath)>0){input$filedata$name})
                                       )
                                     }else{em("nematodes from Araca Bay, Brazil")}),
                              cellWidths = c("16%","84%")),


                        ),



                        div(style=" margin-left: 80px;",

                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed SeaGreen; height:50px; border-bottom: 2px dashed SeaGreen;", width='50px'),
                              column(12,

                                     h5(strong("Factors:",style="color:  SeaGreen"),
                                        popify(actionLink('labhelp', icon("fas fa-question-circle")),"Upload the factors",textlab(), trigger = "hover",options=list(container="body"))),
                                     if(input$up_or_ex=='upload'){
                                       fluidRow(style="margin-bottom:-35px",
                                                fileInput("labels", NULL, accept = c(".csv"),placeholder=if(length(input$labels$datapath)>0){input$labels$name})
                                       )
                                     }else{em("sampling factors")}

                              ),
                              cellWidths = c("16%","84%")),


                        ),
                        div(style=" margin-left: 80px;",
                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed #05668D; height:50px; border-bottom: 2px dashed #05668D;", width='50px'),
                              column(12,style="color:  #05668D;",
                                     h5(strong("Coordinates*:"),
                                        popify(actionLink('cohelp', icon("fas fa-question-circle")),"Upload the coordinates", textcoords(), trigger = "hover",options=list(container="body"))),
                                     if(input$up_or_ex=='upload'){
                                       fluidRow(style="margin-bottom:-35px",
                                                fileInput(inputId = "coords",label =  NULL,accept = c(".csv"),placeholder=if(length(input$coords$datapath)>0){input$coords$name})
                                       )
                                     } else {
                                       em("sampling coordinates")}
                              ),
                              cellWidths = c("16%",'84%')
                            )
                        ),


                        div(style=" margin-left: 80px;  ",
                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed #05668D; height:50px; border-bottom: 2px dashed #05668D;", width='50px'),
                              column(12,style="color:  #05668D;",
                                     h5(strong("Base shape*:",actionLink("basehelp", tipify(icon("fas fa-question-circle"),"Click for more details")))),
                                     if(input$up_or_ex=='upload'){
                                       fluidRow(style="margin-bottom:-35px",
                                                fileInput(inputId = "base_shape",label = NULL,placeholder=if(length(input$base_shape$datapath)>0){input$base_shape$name})
                                       )
                                     }else {em("base shape of the Araca Bay")}
                              ),
                              cellWidths = c("16%",'84%')
                            )
                        ),
                        div(style=" margin-left: 80px; ",
                            splitLayout(
                              absolutePanel(style="border-left: 2px dashed #05668D; height:50px; border-bottom: 2px dashed #05668D;", width='50px'),
                              column(12,style="color:  #05668D;",
                                     h5(strong("Layer shape*:",actionLink("layerhelp", tipify(icon("fas fa-question-circle"),"Click for more details")))),
                                     if(input$up_or_ex=='upload'){
                                       fluidRow(style="margin-bottom:-35px",
                                                fileInput(inputId = "layer_shape",label = NULL,placeholder=if(length(input$layer_shape$datapath)>0){input$layer_shape$name})
                                       )
                                     }else{em("layer shape of the Araca Bay")}

                              ),
                              cellWidths = c("16%",'84%')
                            )
                        )
                 )


          ),
          cellWidths = c("20%","80%")
        )
      )
    )
  })

  output$datalistname<-renderUI({
    textInput("data_name", NULL, value=paste0('Datalist_',if(input$up_or_ex == 'use example data'){'nema_araca'} else {gsub(".csv","",input$filedata$name)}))
  })
  output$menutitle<-renderUI({gettitle()})
  insertab1<-reactiveValues(df=T)



  fingerprint_model <-
    reactive({
      do.call("cbind", lapply(saved_model$df, function (x)
        attr(x, "fingerprint")))
    })


  fingerprint_data <- reactive({
    res <-
      lapply(saved_data$df, function (x)
        attr(x, "fingerprint_data"))

    res
  })


  output$som_control<-renderUI({
    req(length(saved_data$df)>0)
    column(12,splitLayout(
      radioButtons("train_or_load",NULL,choiceValues =list("Train data","Saved results"),choiceNames =list(
        popify(span("Train data"),NULL,("Train the Data-Attribute from the selected Datalist"), options=list(container="body")),
        popify(span("Saved results"),NULL,"See results already saved in the selected Datalist", options=list(container="body"))       )),
      uiOutput("data_som"),
      uiOutput("som_models"),
      cellWidths = c('20%',"40%","40%")

    ))

  })
  observeEvent(input$som_tab,{
    if(input$som_tab=='som_tab1'&input$train_or_load=="Saved results")
    updateRadioButtons(session,"train_or_load",NULL,choiceValues =list("Train data","Saved results"),choiceNames =list(         popify(span("Train data"),NULL,("Train the Data-Attribute from the selected Datalist"), options=list(container="body")),         popify(span("Saved results"),NULL,"See results already saved in the selected Datalist", options=list(container="body"))       ), selected="Train data")
  })


  som_models_reac<-reactive({
    if(isFALSE(bagmodel$df)){
      choices=c(names(attr(getdata_som(),"som")))
      selected=names(attr(getdata_som(),"som"))[length(names(attr(getdata_som(),"som")))]
    } else {choices=c("newsom" = "", names(attr(getdata_som(),"som")))
    selected="newsom"}
    if(length(attr(getdata_som(),"som"))>0){
      column(12,
             strong(style="color: #0D47A1",
               em("Som-Attribute::", input$data_som)),
             tipify(selectInput(
               "som_models",
               NULL,
               choices = choices,
               selectize = F,
               selected=selected
             ),"Select a model to see its results", options=list(container="body"))
      )
    }

  })


  output$som_models<- renderUI({

    som_models_reac()})

  output$data_som <- renderUI({
    if(cur_data$df==0){cur=names(saved_data$df)[1]} else {cur=cur_data$df}
    splitLayout(
      column(12,
             strong(em("Datalist::", style="color: #0D47A1")),
             tipify(selectInput("data_som",NULL, choices =    names(saved_data$df), selectize=F, selected=cur), "Select the Datalist", options=list(container="body"))
      ),
      if(isTRUE(bagmodel$df)) {

        div(style="margin-top:20px",
            popify(bsButton("tools_savesom", div(icon("fas fa-save"), icon("fas fa-arrow-left")),style  = "button_active", type="action",value=FALSE), NULL, "Save the som model in the Datalist", options=list(container="body"))
        )

      },
      cellWidths = c("70%","30%"))
  })

  observeEvent(input$tools_savesom,{
    if(input$tools_savesom %% 2){

      datahand$df<-"Save newsom in"
      datahand2$df<-column(12,fluidRow(em(input$data_som, style="color:gray"),strong("::"), em("Som-Attribute", style="color:gray"),strong("::")
      ))
      datahand3$df<-em("Both 'create' and 'overwrite' options add the resulting BMU to the 'Factor-Attribute' of the corresponding Datalist."
)
      showModal(
        datahand_modal()
      )
    }
  })



  output$preview<-renderUI({
    if(datahand$df=="Save newsom in"){
      bmu<-data.frame(getsom()$unit.classif)
      rownames(bmu)<-rownames(getdata_som())
      colnames(bmu)<-paste0('bmu_',input$model_newname)
      column(12,
             style="overflow-x: scroll;height:300px;overflow-y: scroll",
             renderPrint(cbind(attr(getdata_upload(),"factors")[rownames(bmu),],bmu))
      )

    } else if(datahand$df=="Save changes")
    {
      column(12,
             style="overflow-x: scroll;height:300px;overflow-y: scroll",
             column(12,renderTable(
               getdata_upload(), bordered = T,rownames =T
             )))
    } else if(datahand$df=="Save Clusters")
    {
      hc <- phc()
      temp <- data.frame(hc$somC)
      colnames(temp)<-if(input$datahand=="create"){input$hc_newname}else{input$hc_over}

      column(12,
             style="overflow-x: scroll;height:300px;overflow-y: scroll",
             renderPrint(cbind(attr(getdata_upload(),"factors")[rownames(temp),],temp)))
    }    else if(datahand$df=="Create data list from aggregation results")
    {
      column(12,
             style="overflow-x: scroll;height:300px;overflow-y: scroll",
            column(12,renderTable(
               aggreg(), bordered = T,rownames =T
             )))
    } else if(datahand$df=="Create a datalist with the variables selected in the Random Forest Explainer")
    {
      column(12,
             style="overflow-x: scroll;height:300px;overflow-y: scroll",
             column(12,renderTable(
               getdata_rf02()[,rf_sigs$df$variable]
             )))

    }

  })










  observeEvent(input$bookmark1, {
    reactiveValuesToList(input)
    session$doBookmark()
  })



  onBookmarked(
    fun = function(url) {
      myBookmarks$urlDF <- url
    }
  )




  output$bookmarkBtn <- {
    downloadHandler(
      filename = function() {
        paste(input$savepoint_name, ".rds", sep = "")
      },
      content = function(file) {

        myBookmarks$saved_model=isolate(saved_model$df)

        myBookmarks$bagmodel=isolate(bagmodel$df)


        myBookmarks$saved_labels=isolate(saved_labels$df)
        myBookmarks$datalist1_book=isolate(datalist1_book$df)
        myBookmarks$datalist2_book=isolate(datalist2_book$df)
        myBookmarks$datalist3_book=isolate(datalist3_book$df)
        myBookmarks$phc_book=isolate(phc_book$df)
        myBookmarks$saved_data=isolate(saved_data$df)
        myBookmarks$saved_kdataWSS=isolate(saved_kdataWSS$df)
        myBookmarks$saved_kdataaccRF=isolate(saved_kdataaccRF$df)
        myBookmarks$saved_kmodelaccRF=isolate(saved_kmodelaccRF$df)
        myBookmarks$saved_kmodelWSS=isolate(saved_kmodelWSS$df)
        myBookmarks$saved_kcustom=isolate(saved_kcustom$df)

        myBookmarks$saved_kmodelvotes=isolate(saved_kmodelvotes$df)
        myBookmarks$saved_kdatavotes=isolate(saved_kdatavotes)
        myBookmarks$loaded=isolate(loaded$df)
        myBookmarks$insertab2=isolate(insertab2$df)
        myBookmarks$insertab3=isolate(insertab3$df)
        myBookmarks$saved_dataset_value = isolate(length(saved_data$df)+1)
        myBookmarks$saved_dataset_names =isolate(saved_dataset_names$df)
        myBookmarks$savepoint=isolate(savepoint$df)
        myBookmarks$savepoint2=isolate(savepoint2$df)
        myBookmarks$urlDF=isolate(urlDF$urlDF)
        myBookmarks$factors_palette=isolate(factors_palette$df)

        myBookmarks$saved_base_shape=isolate(saved_base_shape$df)
        myBookmarks$saved_layer_shape=isolate(saved_layer_shape$df)
        myBookmarks$saved_coords=isolate(saved_coords$df)

        saveRDS(myBookmarks, file)
      }
    )
  }


  dialogmodal <- function() {
    modalDialog(
      column(12,style="heigh: 500px",
             column(12,style="background: #ecf6f9;border: 1px solid gray;",
                    br(),
                    column(12,strong(icon("fas fa-thumbtack"),'Create a savepoint:',style="color: #05668D")),
                    column(12,
                           splitLayout(cellArgs = list(style='white-space: normal;'),
                                       column(12,uiOutput("bookout")),
                                       column(12,uiOutput("bookout0")),
                                       column(12,uiOutput("downbook"))
                           ))
             ),


             column(12,style="background: #ecf6f9;border: 1px solid gray;",
                    column(12, style="margin-top: 10px;",

                           tipify(strong(icon("fas fa-external-link-alt"),"Load a savepoint:",style="color: #05668D"),"Restore the dashboard with a previously saved savepoint (.rds file)")),
                    br(),
                    br(),
                    column(6,align = "right", fileInput("load_savepoint", label=NULL,accept =".rds"))

             ),
             br(),
             br()
      ),
      title = p(strong("Save points", style="color: SeaGreen")),
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    )
  }


  observeEvent(input$dialog, {
    showModal(dialogmodal())
  })


  output$bookout<-renderUI({
    fluidRow(

      column(12,p(strong("1. Type the savepoint name"))),
      column(12,textInput("savepoint_name",NULL,placeholder="savepoint name"))

    )
  })

  output$bookout0<-renderUI({

    req(input$savepoint_name)
    fluidRow(
      column(12,p(strong("2. Click to create the savepoint"))),
      column(12,bookmarkButton(id = "bookmark1", style = "font-size: 12px"))
    )
  })


  output$downbook<-renderUI({
    req(input$bookmark1)
    fluidRow(
    column(12,strong("3. Dowload the rds file")),
    column(12,tipify(downloadButton("bookmarkBtn", style = "font-size: 12px", label=paste("Download", input$savepoint_name)),"This action allows you to load the created savepoint later (panel bellow).", options=list(container="body")))
  )})



  observeEvent(input$load_savepoint,
    showModal(
      modalDialog(
        column(12,
               column(12,p(strong("Warning:", style="color: #0093fcff"),"The application will restart with the loaded savepoint and unsaved changes will be lost"))),
        title="Are you sure? ",
        footer = column(12,actionButton("load_savepoint_yes","Proceed"),modalButton("close"))

      )
    )
  )

  #observe({
  observeEvent(input$load_savepoint_yes,{

    myBookmarks<- readRDS(input$load_savepoint$datapath)
    #myBookmarks<- readRDS("bookmarks.rds")
    curmenu<-isolate(as.character(myBookmarks$menu))
    myBookmarks<- readRDS(input$load_savepoint$datapath)

    saved_model$df<-isolate(myBookmarks$saved_model)

    bagmodel$df<-isolate(myBookmarks$bagmodel)

    saved_labels$df<-isolate(myBookmarks$saved_labels)
    datalist1_book$df<-F
    datalist2_book$df<-isolate(myBookmarks$datalist2_book)
    datalist3_book$df<-isolate(myBookmarks$datalist3_book)
    phc_book$df<-isolate(myBookmarks$phc_book)
    saved_data$df<-isolate(myBookmarks$saved_data)
    saved_kdataWSS$df<-isolate(myBookmarks$saved_kdataWSS)
    saved_kdataaccRF$df<-isolate(myBookmarks$saved_kdataaccRF)
    saved_kmodelaccRF$df<-isolate(myBookmarks$saved_kmodelaccRF)
    saved_kmodelWSS$df<-isolate(myBookmarks$saved_kmodelWSS)
    saved_kcustom$df<-isolate(myBookmarks$saved_kcustom)
    saved_kmodelvotes$df<-isolate(myBookmarks$saved_kmodelvotes)
    saved_kdatavotes$df<-isolate(myBookmarks$saved_kdatavotes)
    loaded$df<-isolate(myBookmarks$loaded)
    insertab2<-isolate(myBookmarks$insertab2)
    insertab3<-isolate(myBookmarks$insertab3)
    saved_dataset_value$df<-isolate(myBookmarks$saved_dataset_value)
    saved_dataset_names$df<-isolate(myBookmarks$saved_dataset_names)
    savepoint$df<-isolate(myBookmarks$savepoint)
    savepoint2$df<-isolate(myBookmarks$savepoint2)
    factors_palette$df<-isolate(myBookmarks$factors_palette)
    saved_base_shape$df<-isolate(myBookmarks$saved_base_shape)
    saved_layer_shape$df<-isolate(myBookmarks$saved_layer_shape)
    saved_coords$df<-isolate(myBookmarks$saved_coords)


    #saved_data$df<-isolate(myBookmarks$datasets)
    # saved_model$df<-isolate(myBookmarks$saved_model)
    loaded$df<-isolate(TRUE)
    isolate(updateTabItems(session, "tabs", "menu_intro"))
    isolate(updateTabItems(session, "tabs", "menu_upload"))
    isolate(updateTabItems(session, "upload_tab", "upload_tab1"))
    isolate(updateTabItems(session, "upload_tab", "upload_tab2"))
    # req(req(input$selecvar))
    isolate(updateTabItems(session, "upload_tab", "upload_tab3"))
    isolate(updateTabItems(session, "upload_tab", "upload_tab4"))
    isolate(updateTabItems(session, "tabs", "som"))

    isolate(updateTabItems(session, "tabs", "menu_hc"))
    isolate(updateTabItems(session, "hc_tab", "hc_tab1"))
    isolate(updateTabItems(session, "hc_tab", "hc_tab2"))
    isolate(updateTabItems(session, "hc_tab", "hc_tab3"))
    isolate(updateTabItems(session, "hc_tab", "hc_tab4"))
    isolate(updateTabItems(session, "tabs", "menu_maps"))
    #isolate(updateTabItems(session, "tabs", curmenu))
  })


  observe({
    if(input$tabs=="menu_intro"){  myBookmarks$menu<- "menu_intro" }
    if(input$tabs=="menu_upload"){  myBookmarks$menu<- "menu_upload"  }
    if(input$tabs=="menu_som"){  myBookmarks$menu<- "menu_som"  }
    if(input$tabs=="menu_hc"){  myBookmarks$menu<- "menu_hc"  }
    if(input$tabs=="menu_rf"){  myBookmarks$menu<- "menu_rf"  }
    if(input$tabs=="menu_maps"){  myBookmarks$menu<- "menu_maps"  }
    if(input$tabs=="menu_box"){  myBookmarks$menu<- "menu_box"  }
    if(input$tabs=="menu_div"){  myBookmarks$menu<- "menu_div"  }

  })

  picktab<-reactive({
    cur<-isolate(booktabs$df)
    if(cur==1)  {
      updateTabItems(session, "upload_tab", "upload_tab1")}
    if(cur==2)  {
      updateTabItems(session, "upload_tab", "upload_tab2")}
    if(cur==3)  {
      updateTabItems(session, "upload_tab", "upload_tab3")}
    if(cur==4)  {
      updateTabItems(session, "upload_tab", "upload_tab4")}


  })






  output$divname <- renderUI({textInput("divname",NULL,value = paste0("Div_res", saved_divset_value$df))})

  output$divtabs <- renderUI({
    req(length(saved_data$df)>0)
    column(12,
           splitLayout(cellWidths = c("30%","70%"),cellArgs = list(style='white-space: normal;'),
             uiOutput("divcontrol"),
             column(12,style="overflow-x: scroll",
                    column(12,strong("Results", style = "color: SeaGreen;")),
                    renderPrint(divI()))


           )


    )
  })


  output$down_data <- {
    downloadHandler(
      filename = function() {
        if(input$down_choices=="data"){
          paste0("data_",input$getdata_down,"_", Sys.Date(), ".csv")} else{
            paste0("factors_",input$getdata_down,"_", Sys.Date(), ".csv")
          }
      }, content = function(file) {
        write.table(x=data.frame(get_down()),file,append=T,quote=F,row.names=T,col.names=NA, sep=input$down_data_sep,
                    dec=input$down_data_dec)})}


  output$pop_div<-renderUI({

    checkboxGroupInput("divInds",
                       NULL,
                       choiceValues =list("N","S","margalef","D","H","J'","Dom_rel","Skewness"),
                       selected=c("N","S","margalef","D","H","J'","Dom_rel","Skewness"),
                       inline = F,

                       choiceNames =list(
                         span("N", actionLink('Nhelp',icon("fas fa-question-circle")), conditionalPanel("input.Nhelp % 2",{uiOutput("Nhelp")})),
                         span("S",actionLink('Shelp',icon("fas fa-question-circle")), conditionalPanel("input.Shelp % 2",{uiOutput("Shelp")})),
                         span("margalef",actionLink('mhelp',icon("fas fa-question-circle")), conditionalPanel("input.mhelp % 2",{uiOutput("mhelp")})),
                         span("D",actionLink('Dhelp',icon("fas fa-question-circle")), conditionalPanel("input.Dhelp % 2",{uiOutput("Dhelp")})),
                         span("H",actionLink('Hhelp',icon("fas fa-question-circle")), conditionalPanel("input.Hhelp % 2",{uiOutput("Hhelp")})),
                         span("J",actionLink('Jhelp',icon("fas fa-question-circle")), conditionalPanel("input.Jhelp % 2",{uiOutput("Jhelp")})),
                         span("Dom_rel",actionLink('Domhelp',icon("fas fa-question-circle")), conditionalPanel("input.Domhelp % 2",{uiOutput("Domhelp")})),
                         span("Skewness",actionLink('Skhelp',icon("fas fa-question-circle")),conditionalPanel("input.Skhelp % 2",{uiOutput("Skhelp")})))
    )

  })

  output$divcontrol <- renderUI({
    fluidRow(
      p(
        uiOutput("data_div"),
        column(12,
               fluidRow(bsButton("DIVI","Diversity indexes",icon=icon("fas fa-caret-down"), type ="toggle", style="button_active"),popify(bsButton("tools_savediv", icon("fas fa-save"),style  = "button_active", type="action",value=FALSE),NULL,
                                                                                                                                          "Save diversity results",options=list(container="body")
               )


               ),
               conditionalPanel("input.DIVI % 2",{uiOutput("pop_div")}))
      ),



    )

  })

  output$Nhelp<-renderUI({
    column(12,style="background: white",
      strong("Number of individuals"),
      p("Total number of individuals")
    )
  })
  output$Shelp<-renderUI({
    column(12,style="background: white",
      strong("Species richness"),
      p("the total number of species in each observation")
    )
  })

  output$mhelp<-renderUI({
    column(12,style="background: white",
      strong("Margalef diversity"),
      p("The total number of species weighted by the logarithm of the total number of individuals", withMathJax(helpText("$$ margalef = \\frac{(S-1)}{lnN}$$")))
    )
  })



  output$Dhelp<-renderUI({
    column(12,style="background: white",
      strong("Simpson diversity"),
      p("the probability that two individuals drawn at random from an infinite community would belong to the same species",withMathJax(helpText("$$ D = \\sum p^2$$")))
    )
  })

  output$Hhelp<-renderUI({
    column(12,style="background: white",
           strong("Shannon diversity"),
           p("This index  considers both species richness and evenness. The uncertainty is measured by the Shannon Function 'H'. This term is the measure corresponding to the entropy concept defined by:",withMathJax(helpText("$$ H = \\sum_{n=1}^n (p_i*\\ln p_i)$$")))
    )
  })
  output$Jhelp<-renderUI({
    column(12,style="background: white",
           strong("Shannon evenness J'"),
           p("Evenness is a measure of how different the abundances of the species in a community are from each other. The Shannon evennes is defined by:",
             withMathJax(helpText("$$ J' = \\frac{H}{\\ln(S)}$$")))
    )
  })
  output$Domhelp<-renderUI({
    column(12,style="background: white",
           strong("Relative Dominance'"),
           p("A simple measure of dominance where",em(HTML(paste0("N",tags$sub("i")))),", the abundance of the most abundant species is divided by N:",
             withMathJax(helpText("$$ Dom_{rel} = \\frac{N_1}{N} $$")))
    )
  })

  output$Skhelp<-renderUI({
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


  observeEvent(input$tools_savediv,{
    datahand$df<-"Save diversity results"
    datahand2$df<-NULL
    datahand3$df<-NULL
    showModal(
      datahand_modal()
    )
  })



  bagdiv<-reactiveValues()


  output$data_div<-renderUI({
    if(cur_data$df==0){cur=names(saved_data$df)[1]} else {cur=cur_data$df}
    selectInput("data_div", "Datalist:", choices = names(saved_data$df), selected=cur, selectize = F)
  })




  divI<-reactive({
    abund=getdata_div()
    choices=input$divInds
    res=list()
    if("N"%in%choices){res$N<-rowSums(abund)}
    if("S"%in%choices){res$S<-vegan::specnumber(abund)}
    if("margalef"%in%choices){res$margalef=(specnumber(abund)-1)/log(rowSums(abund))}
    if("D"%in%choices){res$D<-vegan::diversity(abund, index="simpson")}
    if("H"%in%choices){res$H<-vegan::diversity(abund)}
    if("J'"%in%choices){
      H<-vegan::diversity(abund)
      S<-vegan::specnumber(abund)
      res$J <- H/log(S)}
    if("Dom_rel"%in%choices){res$Dom_rel<-apply(decostand(abund, "total"),1,sort,T)[1,]}
    if("Skewness"%in%choices){res$Skewness=apply(abund,1,skewness)}
    res<-data.frame(do.call(cbind,res))

    res
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      paste("iMESc.zip", sep = "")
    },

    content = function(name1) {
      fs <- c()
      fs <-
        c('meta',"code_to_run.R","app.R","iMESc.R"

        )

      zip(zipfile = name1, files = fs)

      if (file.exists(name1)) {
        file.rename(name1, name1)
      }
    },
    contentType = "application/zip"
  )





  choices_maps <- reactive({
    a <- if (length(   names(saved_data$df) > 0)) {
      "variable"
    } else {
      NULL
    }

    c <- "factor"
    res <- c(a, c)
    res
  })



  output$choices_map <- renderUI({
    req(length(saved_data$df)>0)
    column(12,style="background: white;",
             br(),
             column(12,
               splitLayout(cellWidths = c('30%',"30%","30%",'10%'),
                 p(uiOutput("data_map")),
                 p(radioButtons(
                   "choices_map",
                   "Target:",
                   choices = choices_maps(),
                   inline = T
                 )),
                 p(
                   conditionalPanel("input.choices_map=='variable'", {
                     uiOutput("map_vars"
                     )
                   }),
                   conditionalPanel("input.choices_map=='factor'", {
                     uiOutput("factors_map")
                   })
                 ),
                 p(actionButton("shp_create",strong(popify(div("shp",icon("fas fa-map")),NULL,"Click to  acreate base_shape/layer_shape'", options=list(container="body")),"Add", style="button_active")))
               )

             )

    )
  })

  output$factors_map<-renderUI({
    selectInput("map_hc",
                "Factors:",
                choices = rev(colnames(attr(getdata_map(),"factors"))),
                selectize = F)
  })
  output$data_map<-renderUI({
    if(cur_data$df==0){cur=names(saved_data$df)[1]} else {cur=cur_data$df}
    selectInput("data_map",
                "data",
                choices =    names(saved_data$df),
                selectize = F)
  })

  output$map_vars<-renderUI({
    req(input$choices_map=='variable')
    selectInput(
      "var_map",
      label = "select a variable",
      choices = colnames(getdata_map()),
      selectize = F
    )
  })













  box_y_input <- reactive({
    data <- getdata_upload()
    selectInput(
      'box_y',
      tipify(
        strong("y ~"),
        " y is the data values to be split into groups according to the grouping variable"
      ),
      choices = colnames(data),
      selectize = F
    )
  })
  output$box_y_input <- renderUI({box_y_input()  })



  output$filter_box2 <- renderUI({
    filter_box2()
  })

  filter_box2 <- reactive({
    if (input$filter_box1 != "none") {
      data = getdata_upload()
      labels <- attr(data,"factors")[rownames(data), input$filter_box1]
      selectInput('filter_box2',
                  "class:",
                  choices = c(levels(as.factor(labels))),
                  selectize = F)
    }
  })

  output$filter_box1<-renderUI({
    selectInput('filter_box1',"filter:",choices = c("none", colnames(attr(getdata_upload(),"factors"))),selectize = F)
  })


  output$box_factor<-renderUI({
    selectInput('box_factor',"factor",choices = colnames(attr(getdata_upload(),"factors")),selectize = F)
  })

  output$boxplot_control <- renderUI({
    fluidRow(
      column(
        12,
        splitLayout(
          uiOutput("box_y_input"),
          uiOutput("box_factor"),
          splitLayout(
            uiOutput("filter_box1"),
            uiOutput("filter_box2")
          )
        )
      ))
  })




  output$box_outliers<-renderUI({
    if(isTRUE(input$showout)){
      selectInput("out_label",NULL,choices=colnames(attr(getdata_upload(),"factors")))}

  })

  output$out_box<-renderUI({
    checkboxGroupInput("remove_out","remove outliers", choices = attr(plotbox(),"outliers"), inline=T)
  })

  output$editbox<-renderUI({
    res<-na.omit(getdata_upload())
    column(12,style="border-top: 3px solid SeaGreen;border-bottom: 3px solid SeaGreen;background: white; margin-right: 100px; margin-top: -30px;margin-bottom: 20px;",
           column(12,
                  column(12,splitLayout(
                    textInput("titlebox", 'Title', value=titlebox()),
                    column(12,
                           strong("Palette"),
                           pickerInput(inputId = "box_palette",
                                       label = NULL,
                                       choices = df$val,
                                       choicesOpt = list(content = df$img), options=list(container="body"))),
                    column(12,
                           checkboxInput("showout","show outlier labels",F),
                           uiOutput("box_outliers")))),
                  column(12,
                         splitLayout(
                           numericInput("box_cexlab","plot size",value=1.2,step=0.1),
                           numericInput("box_lwd","line width",value=1.2,step=0.1),
                           numericInput("ymin_box","y min",value=floor(min(res[,2])),step=0.1),
                           numericInput("ymax_box","y max",value=ceiling(max(res[,2])),step=0.5),
                           numericInput("insety","leg y",value=0,step=0.1),
                           numericInput("insetx","leg x",value=0,step=0.1)
                         ))

           )

    )
  })


  titlebox<-reactive({
    a<-paste0(colnames(getdata_upload())[2],"~",colnames(getdata_upload())[1])
    if(input$filter_box1!='none'){b=paste0("[",input$filter_box1,"::",input$filter_box2,"]")} else{b=NULL}
    paste(a,b)
  })

  output$outliers<-renderUI({
    outliers
  })




  labbox<-reactive({
    if(isTRUE(input$showout)){data.frame(attr(getdata_upload(),"factors")[as.character(input$out_label)]
    )} else{
      NULL
    }
  })

  plotbox<-reactive({
    res <- getbox()

    if(length(input$box_palette)>0){
      boxp<-pbox(res=res, palette=input$box_palette,  coefic=1.5,lab_out=labbox(),cex.lab=as.numeric(input$box_cexlab), lwd=as.numeric(input$box_lwd), ylim=c(input$ymin_box, input$ymax_box), main=input$titlebox, insetx=input$insetx,insety=input$insety)
    } else{  boxp<-pbox(res) }
    boxp
  })

  output$boxplot_out <- renderPlot({plotbox()})



  output$CMres <- renderUI({
    validate(need(is.factor( runRF()$finalModel$y), "Confusion matrices are only valid for classification (factor) models."))
    list(
      popify(pickerInput(inputId = "rfpalette",
                         label = NULL,
                         choices = df$val,
                         choicesOpt = list(content = df$img),
                         options=list(container="body"),
                         selected=df$val[1]
      ), NULL, "confusion matrix palette",options=list(container="body")),

      fluidRow(plotOutput("CM"))
    )
  })

  accu <- reactive({
    confu <- getConfusion(runRF())
    sum(diag(round(as.matrix(confu[, -ncol(confu)]), 2)))


  })



  rf_depth <- reactive({
    fluidRow(br(),
             column(12,

               splitLayout(cellWidths = c('20%',"20%"),

                           column(12,
                                  conditionalPanel("input.go_rfplot % 2",{
                                    fluidRow(
                                      popify(bsButton("fine_rfdepth",icon("fas fa-sliders-h"),style  = "button_active", type ="toggle"),NULL,"Fine tuning",options=list(container="body")),

                                      popify(bsButton("tools_saverf", icon("fas fa-file-signature"),style  = "button_active", type="action",value=FALSE, block=F),NULL,
                                             "Create a datalist with the variables selected in the Random Forest Explainer.",options=list(container="body")
                                      )

                                    )
                                  }))
               )


             ),
             column(12,
                    conditionalPanel("input.fine_rfdepth % 2",{
                      splitLayout(
                        column(12,uiOutput("npicrf")),
                        column(12,numericInput('depth_min_no_of_trees',strong('min_n_trees', tipify(icon("fas fa-question-circle"),"The minimal number of trees in which a variable has to be used for splitting to be used for plotting", options =list(container="body") )),value=0,step=1)),
                        column(12,selectInput('depth_mean_sample',strong('mean_sample', tipify(icon("fas fa-question-circle"),"The sample of trees on which mean minimal depth is calculated", options =list(container="body"))),choices=c( "top_trees","all_trees","relevant_trees"), selected= "top_trees",selectize=F))                               ,
                        column(12,numericInput("sigprf", strong('sig', tipify(icon("fas fa-question-circle"),"Significance level", options =list(container="body") )), 0.05)),
                        column(12,numericInput("labrfsize","plot size", value=10))


                      )
                    }),
                    column(12,checkboxInput("rf_depth", "show only significant variables", T))
             )
           )
  })


  observeEvent(input$tools_saverf,{
    datahand$df<-"Create a datalist with the variables selected in the Random Forest Explainer"
    datahand2$df<-NULL
    datahand3$df<-NULL
    showModal(
      datahand_modal()
    )
  })

  output$npicrf <- renderUI({
    if (input$rf_depth == F)
      numericInput("n_var_rf",
                   strong('n.vars', tipify(icon("fas fa-question-circle"),"The maximal number of variables with lowest mean minimal depth to be used for plotting", options =list(container="body"))),
                   value = 10,
                   step = 1)

  })

  sigrf <- reactive({
    if (input$rf_depth == TRUE) {
      TRUE
    } else {
      input$n_var_rf
    }
  })

  output$rf_depth <- renderUI({
    rf_depth()
  })
  output$controlrf <- renderUI({
    fluidRow(
      column(
        12,
        splitLayout(
          numericInput("ntree", strong("ntree",popify(a(icon("fas fa-question-circle")),'ntree',"Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times",
                                                      options=list(container="body"))), value = 50),
          selectInput(
            "preProcess",
            strong("preProcess", actionLink('preProcesshelp',tipify(icon("fas fa-question-circle"), "Click for more details"))),
            choices = c(
              "",
              "BoxCox",
              "YeoJohnson",
              "expoTrans",
              "center",
              "scale",
              "range",
              "knnImpute",
              "bagImpute",
              "medianImpute",
              "pca",
              "ica" ,
              "spatialSign"
            ), selectize = F
          ),
          numericInput("seedrf", strong("seed",tipify(a(icon("fas fa-question-circle")), textseed(), options=list(container="body"))), value = NULL)
        ),
        splitLayout(cellWidths = c("30%","30%"),
          column(12,
                 radioButtons("res_method","Resampling method", choiceValues=list("boot","cv","repeatedcv","LOOCV","LGOCV"), choiceNames=list(
                   tipify(div("boot"),"bootstraping", options=list(container="body")),tipify(div("cv"), "k-fold cross-validation", options=list(container="body")),tipify(div("repeatedcv"),"repeated k-fold cross-validation", options=list(container="body")),tipify(div("LOOCV"),"Leave one out", options=list(container="body")), tipify(div("LGOCV"),"leave-group out cross-validation", options=list(container="body"))),
                   selected="repeatedcv"
                   )),
          column(12,
            splitLayout(
              conditionalPanel("input.res_method=='cv'|input.res_method=='repeatedcv'",{
                numericInput("cvrf", strong("cv", tipify(a(icon("fas fa-question-circle")),"number of folds", options=list(container="body"))), value = 5)
              }),
              conditionalPanel("input.res_method=='repeatedcv'",{
                numericInput("repeatsrf", strong("repeats", tipify(a(icon("fas fa-question-circle")),"the number of complete sets of folds to compute", options=list(container="body"))), value = 1)
              })
            ),
            conditionalPanel("input.res_method=='boot'",{
              numericInput("cvrf", strong("number", tipify(a(icon("fas fa-question-circle")),"the number of resampling iterations", options=list(container="body"))), value = 99)
            }),
            conditionalPanel("input.res_method=='LGOCV'",{
              numericInput("pleaverf", strong("p (%)", tipify(a(icon("fas fa-question-circle")),"the training percentage", options=list(container="body"))), value = 10)})
          )
        )
      ),
      column(12, align = "center", popify(actionButton("trainRF", h4(icon("fas fa-tree"),
                                                                  icon("fas fa-tree", style = "margin-left: -10px;"),icon("fas fa-tree", style = "margin-left: -10px;"),"train RF",icon("fas fa-arrow-circle-right")), style = "background:  #05668D; color: white"),NULL,"Click to run")),

    )
  })


  output$choices_rf <- renderUI({
    req(length(saved_data$df)>0)
    column(12,
           column(12,style="margin-left: -20px; margin-bottom: 10px",
                  column(1,strong("Type:",style="color: #0D47A1")),
                  column(11,style="margin-left: -15px; margin-bottom: -15px",
                         radioButtons("rf_type", NULL, c("Classification", "Regression"), inline = T)
                  ),

           ),

           column(12,
                  uiOutput("rf_control")
           )

    )
  })

  output$rf_control<-renderUI({
    fluidRow(
      uiOutput("data_rf01"),
      uiOutput('rf_supervisor'),
      uiOutput('data_rf02')
    )
  })



  output$data_rf01<-renderUI({

    column(4,style="margin-left: -20px",
           strong("Y data:",style="color: #0D47A1"),
           uiOutput('data_rf00')
    )
  })

  output$data_rf00<-renderUI({
    #  external_supv<-names(saved_data$df)[which(names(saved_data$df)!=input$data_rf02)]}
    selectInput("data_rf01",NULL,choices =  names(saved_data$df),selectize = F)})



  data_rf_bag<-reactiveValues(df=0)
  output$data_rf02<-renderUI({
    if(cur_data$df==0){cur=names(saved_data$df)[1]} else {cur=cur_data$df}
    data_rf_bag$df<-names(saved_data$df)

    column(4,style="margin-left: -20px",
           strong("~ Training data (X):",style="color: #0D47A1"),
           selectInput("data_rf02",NULL,choices =data_rf_bag$df,
                       selectize = F, selected=cur)
    )

  })

  output$rf_supervisor <- renderUI({

    data <- saved_data$df[[input$data_rf01]]
    labels <- attr(data,"factors")
    choices<- if(input$rf_type=="Regression"){
      req(input$data_rf01)
      colnames(data)} else {rev(colnames(labels))}
    column(4,style="margin-left: -20px",
           tipify(strong("Independent variable: (Y)",style="color: #0D47A1"),"The response vector"),
           selectInput(
             "rf_sup",
             NULL,
             choices =choices ,
             selectize = F
           )
    )



  })

  get_supervisor <- reactive({
    if (input$rf_type == 'Classification') {
      data <- saved_data$df[[input$data_rf01]]
      labels <- attr(data,"factors")
      labels[input$rf_sup]
    } else {

      data <- saved_data$df[[input$data_rf01]]
      data[input$rf_sup]
    }

  })



  output$rf_panel <- renderUI({

    req(length(saved_data$df)>0)
    column(12,
           validate(need(sum(unlist(lapply(getdata_rf02(), function(x) identical(x,get_supervisor()[,1]))))==0,"The independent variable (Y) cannot be contained in the set of explanatory variables (X).")),

           tabsetPanel(
             id = "rf_tab",

             tabPanel(strong("1. Training"),value="rf_tab1",

                      column(12, uiOutput("controlrf"))),
             tabPanel(
               strong("2. Results"),value="rf_tab2",
               tabsetPanel(
                 tabPanel(strong("2.1 Summary"), verbatimTextOutput("rfsummary")),
                 tabPanel(strong("2.2 Training error "), tableOutput("rf_table")),
                 tabPanel(strong("2.3 RandomForest Explainer"),
                          column(12,
                            column(12,style="margin-top: 20px; margin-bottom: 20px; ",
                                   strong("Minimal depth distribution: ",popify(actionButton("go_rfplot", h5(icon("fab fa-wpexplorer fa-flip-horizontal"),icon("fas fa-arrow-circle-right")),style  = "button_active"),NULL,"Click to Calculate the minimal depth distribution of a random forest",options=list(container="body")),
                                          conditionalPanel("input.go_rfplot % 2",{
                                            popify(actionButton("downcenter_rfdepth",icon("fas fa-download")),NULL,"Download Minimal Depth distribution results", options=list(container="body"))
                                          }))),
                            tabsetPanel(
                              tabPanel(
                                strong("Depth distribution"),
                                uiOutput("rf_depth"),
                                column(12,
                                       plotOutput("prf"))
                              ),
                              tabPanel(
                                strong("Multi-way importance"),
                                uiOutput("rf_multi")
                              )
                            )
                          )),
                 tabPanel(strong("2.5 Confusion Matrix"),
                          uiOutput("CMres"))

               )
             )

           )
    )


  })
  observeEvent(input$downcenter_rfdepth,{
    downcenter_hand$df<-"rfdepth"
    showModal(downcenter())

  })

  output$rf_multi<-renderUI({
    column(12,
           splitLayout(
             selectInput('multi_x_measure',strong('x_measure', tipify(icon("fas fa-question-circle"),"The measure of importance to be shown on the X axis", options =list(container="body"))),choices=c("mean_min_depth","accuracy_decrease"), selected="mean_min_depth",selectize=F),
             selectInput('multi_y_measure',strong('y_measure', tipify(icon("fas fa-question-circle"),"The measure of importance to be shown on the Y axis", options =list(container="body"))),choices=c("times_a_root","gini_decrease"), selected="times_a_root",selectize=F),
             ##selectInput('multi_size_measure ',strong('size_measure', tipify(icon("fas fa-question-circle"),"The measure of importance to be shown as size of points (optional)", options =list(container="body"))),choices=c("","p_value"), selected="times_a_root",selectize=F),
             #numericInput('multi_min_no_of_trees ',strong('min_no_of_trees', tipify(icon("fas fa-question-circle"),"The minimal number of trees in which a variable has to be used for splitting to be used for plotting", options =list(container="body"))),value=0,step=1),
             column(12,numericInput("sigmrf", strong('sig', tipify(icon("fas fa-question-circle"),"Significance level", options =list(container="body") )), 0.05)),
             uiOutput("multi_no_labels")

           ),
           column(12,checkboxInput("rf_sigmulti", "show only significant variables", T)),
           column(12,plotOutput("rf_multi_out"))

           )
  })

  output$rf_multi_out<-renderPlot({
    prf_multi(
      mindeaphrf(), sigs = sigrfmulti(),
      input$sigmrf,
      x_measure = input$multi_x_measure,
      y_measure = input$multi_y_measure
   )

  })
  sigrfmulti <- reactive({
    if (input$rf_sigmulti == TRUE) {
      TRUE
    } else {
      input$multi_no_of_labels
    }
  })

  output$multi_no_labels<- renderUI({
    req(isFALSE(input$rf_sigmulti))

    numericInput('multi_no_of_labels ',strong('no_of_labels', tipify(icon("fas fa-question-circle"),"The approximate number of best variables (according to all measures plotted) to be labeled (more will be labeled in case of ties)", options =list(container="body"))),value=10,step=1)
  })
  {


    choices_hc <- reactive({
      a <- if (length(   names(saved_data$df) > 0)) {
        "data"
      } else {
        NULL
      }
      b <- if(isTRUE(bagHC$df)){"som codebook"}else{NULL}
      res <- c(a, b)
      res
    })

  }



  observeEvent(input$settrain, {
    updateTabsetPanel(session, 'som_tab', "som_tab1")
    updateTabsetPanel(session, 'training_panels', "tab_training_01")
    updateTabsetPanel(session, 'training_panels', "tab_training_02")
  })




  output$results_panel <- renderUI({
    validate(need(!is.null(saved_data$df),"no data found"))




    column(
      12,
      style = "background: WhiteSmoke;",
      tabsetPanel(id="som_res",

                  tabPanel(value= 'train_tab1',
                           "3.1. Parameters",
                           column(12, style = "background: white;",
                                  splitLayout(
                                    tableOutput("train.summary"),
                                    uiOutput("error_som")

                                  ))
                  ),
                  tabPanel(
                    "3.2. Changes",
                    value = "train_tab2",
                    column(
                      12,
                      style = "background: white;",
                      plotOutput('pchanges'),
                      downloadButton('down_changes', 'Download')
                    )
                  ),
                  tabPanel(
                    "3.3. Couting", value = "train_tab3",
                    column(
                      12,
                      style = "background: white;",
                      plotOutput('pcounts'),
                      downloadButton('down_pcounts', 'Download',)
                    )
                  ),
                  tabPanel(
                    "3.4. Umatrix", value = "train_tab4",
                    column(
                      12,
                      style = "background: white;",
                      plotOutput('pUmatrix'),
                      downloadButton('down_pUmatrix', 'Download')
                    )
                  ),
                  tabPanel(
                    "3.5. BMUs", value = "train_tab5",

                    column(12, style = "background: white;", uiOutput("pcorr_control")),


                    column(12, downloadButton('down_BMUs', 'Download'))
                  ),
                  tabPanel(
                    "3.6. Property", value = "train_tab6",
                    column(
                      12,
                      style = "background: white;",
                      br(),
                      column(12, strong(
                        "Property",tipify(icon("fas fa-question-circle"),"Show areas for high values (red) and low values (blue) for the selected variable")
                      )),
                      column(12, uiOutput("var_pproperty")),
                      column(12, plotOutput("pproperty")),
                      column(12, downloadButton('down_pproperty', 'Download'))
                    )
                  )






      )
    )
  })



  training_panel<-reactive({
    validate(need(input$train_or_load=="Train data","Please select the Train mode"))

    fluidRow(id = "train_tab0",

             column(
               12,
               style = "background: white;",
               column(
                 6,
                 br(),
                 strong("1.1. Set the grid", actionLink(
                   "somgridhelp", tipify(icon("fas fa-question-circle"), "Click for more details")
                 )),
                 value = "tab_training_01",
                 column(12,
                        fluidRow(
                          column(
                            12,
                            column(2, checkboxInput("sugtopo", NULL, value =
                                                      TRUE)),
                            column(10, style = "margin-top: 9px; margin-left:-10px;",

                                   p(
                                     "suggested topology",
                                     tipify(actionLink(
                                       "sugtopohelp", icon("fas fa-question-circle")
                                     ), "Click for more details")
                                   ))


                          ),
                          column(12, uiOutput("somgridtopo"))
                        ))

               ),
               column(
                 6,
                 br(),
                 strong(
                   "2.2. Set the training parameters",
                   actionLink("supersomhelp", tipify(
                     icon("fas fa-question-circle"), "Click for more details"
                   ))
                 ),
                 column(12, style = "background: white;",

                        fluidRow(uiOutput("somcontrol")))

               )

             ))
  })
  output$training_panel <- renderUI({

    training_panel()
  })




  observeEvent(input$trainRF, {

    updateTabsetPanel(session, "rf_tab", "som_tab2")



  })


  observeEvent(input$trainSOM, {
    bagmodel$df<-T
    updateTabsetPanel(session, "som_tab", "som_tab2")
    updateTabsetPanel(session, "som_tab", "train_tab2")


  })


  loadlabel<-reactiveValues(df=0)

  labels <- reactive({
    loadlabel$df<-0
    if (input$up_or_ex == 'upload') {
      validate(need(length(input$labels$datapath)!=0,"Factor file is required"))
      labels <-
        data.frame(fread(input$labels$datapath, stringsAsFactors = T))

      rownames(labels) <- labels[, 1]
      #colnames(labels)[1]<-"id"
      labels[, 1] <- NULL
      labels[which(unlist(lapply(labels, is.numeric)))] <-
        do.call("data.frame", lapply(labels[which(unlist(lapply(labels, is.numeric)))], as.factor))




    } else {
      labels <-
        data.frame(fread("meta/factors_araca.csv", stringsAsFactors = T))


      rownames(labels) <- labels[, 1]
      #colnames(labels)[1]<-"id"
      labels[, 1] <- NULL
      labels[which(unlist(lapply(labels, is.numeric)))] <-
        do.call("data.frame", lapply(labels[which(unlist(lapply(labels, is.numeric)))], as.factor))




    }



    factors_palette$df<-isolate(rep("null",ncol(labels)))

    labels

  })





  loadlabel<-reactiveValues(df=0)


  coords<-reactive({

    if (input$up_or_ex == 'use example data') {

      coords <-data.frame(fread("meta/coords_araca.csv"))} else {
        if(length(input$coords$datapath)>0){coords <- data.frame(fread(input$coords$datapath))} else{coords=NULL}


      }


    rownames(coords) <- coords[, 1]
    coords[, 1] <- NULL
    #coords<-read.csv("04_coords.csv",sep=";", row.names=1)
    coords
  })

  base_shape <- reactive({
    if (input$up_or_ex == 'use example data') {
      get(gsub(" ","",capture.output(load("meta/base_shape_araca",verbose=T))[2]))
    } else {
      if(length(input$base_shape$datapath)>0){
        t<-try({get(gsub(" ", "", capture.output(
          load(input$base_shape$datapath, verbose = T)
        )[2]))})
        if("try-error" %in% class(t)) {t<-readRDS(input$base_shape$datapath) }
        t

      }else{NULL}

    }
  })
  layer_shape <- reactive({

    if (input$up_or_ex == 'use example data'){get(gsub(" ","",capture.output(load("meta/layer_shape_araca",verbose=T))[2]))} else {
      if(length(input$layer_shape$datapath)>0){
        t<-try({get(gsub(" ", "", capture.output(
          load(input$layer_shape$datapath, verbose = T)
        )[2]))})
        if("try-error" %in% class(t)) {t<-readRDS(input$layer_shape$datapath) }
        t

      }else{NULL}

    }})





  style_upload_control<-reactive({
    h=50
    if(length(input$tools_transform)>0){
      if(input$tools_transform %% 2 | input$tools_edit %% 2){h=100} else {h=50}
    }
    paste0(h,"px")
  })

  output$menu_upload_out<-renderUI({
    fluidRow(
      column(12, style="margin-left: 10px",
             uiOutput("upload_tools"),
             conditionalPanel('input.tools_transform % 2',{
               uiOutput("pop_transform")
             }),
             conditionalPanel("input.tools_edit % 2",{uiOutput("pop_editplot")}),


             column(12,style="margin-top: -15px; margin-left: -20px",
                    splitLayout(
                      uiOutput("upload_side"),
                      uiOutput("upload_mainpanel"),
                      cellWidths = c("20%","80%")
                    )),
             conditionalPanel("input.tools_selvar % 2",{
               uiOutput("pop_selecvar")
             }),
             conditionalPanel("input.tools_selobs % 2",{
               uiOutput("pop_selecobs")
             })
      )


    )
  })
  output$upload_mainpanel<-renderUI({

    if(isTRUE(bagdata$df)){col="gray"} else{ col="#0093fcff"}
    background<-paste("border: 2px solid",col)

    column(12,style=background,
           conditionalPanel("input.stats0=='stats_classmat'",{uiOutput("pop_classmat")}),
           conditionalPanel("input.stats0=='stats_attributes'",{uiOutput("stats_attributes")}),
           conditionalPanel("input.stats0=='stats_datalist'",{uiOutput("stats_datalist")}),
           conditionalPanel("input.stats0=='stats_sum_data'",{uiOutput("stats_data")}),
           conditionalPanel("input.stats0=='stats_sum_variables'",{uiOutput("stats_var")}),
           conditionalPanel("input.stats0=='stats_sum_factors'",{uiOutput("stats_fac")}),
           conditionalPanel("input.stats0=='stats_desc'",{uiOutput("stats_pdesc")}),
           conditionalPanel("input.stats0=='stats_hist'",{plotOutput("stats_phist")}),
           conditionalPanel("input.stats0=='stats_box'",{uiOutput("stats_pbox")}),
           conditionalPanel("input.stats0=='stats_pca'",{uiOutput("stats_ppca")}),
           conditionalPanel("input.stats0=='stats_mds'",{uiOutput("stats_pmds")}),

    )
  })




  output$stats_pdesc<-renderUI({
    column(12,style="background: white;",
           column(12,style="margin-top:20px",strong("Descriptive statistics")),
           column(12,
                  splitLayout(
                    column(12,

                           p(strong("Aggregate:"),popify(a(icon("fas fa-question-circle")),
                                  "Aggregate","The process involves two stages. First, collate individual cases of the Data-Attribute together with a grouping variable (unselected factors). Second, perform which calculation you want on each group of cases (selected factors)", options=list(container="body"))),

                           p(em(input$data_upload,"::","Factor-Attribute::", style="color: gray")),
                           checkboxGroupInput('fac_descs', NULL, choices=colnames(
                             attr(getdata_upload(),"factors")), selected = colnames(
                               attr(getdata_upload(),"factors"))[1:(length(colnames(
                                 attr(getdata_upload(),"factors")))-1)])

                    ),

                    column(12,
                           selectInput("spread_measures","function:",choices=c("sum","mean","median","var","sd","range"), selectize=F, selected="mean"),
                           popify(bsButton("tools_saveagg", icon("fas fa-file-signature"),style  = "button_active", type="action",value=FALSE, block=T),NULL,
                                  "Create datalist",options=list(container="body")
                           )
                    )
                  )
           ),


           column(12,style="overflow-x: scroll;height:250px;overflow-y: scroll;",uiOutput("desc_dataout"))
    )
  })


  observeEvent(input$tools_saveagg,{
    datahand$df<-"Create data list from aggregation results"
    datahand2$df<-if(!is.null(attr(aggreg(),"coords"))){p("The select Datalist contains a Coords-Attribute. Mean coordinates will be retrieved from the choosen factor group.")} else {NULL}
    datahand3$df<-NULL
    showModal(
      datahand_modal()
    )
  })




  output$desc_facout<-renderUI({
    factors<-attr(getdata_upload(),"factors")
    facs<-colnames(factors)


    colla<-factors[,facs[which(!facs%in%input$fac_descs)]]
    list(
      column(12,strong("Aggregated by:")),
      renderPrint(table(do.call(paste,args=c( colla, sep="_")))))
  })

  output$desc_dataout<-renderUI({
   data<-aggreg()
   factors<-attr(data,"factors")
    #aggregate(data,faclist,call(as.character(input$spread_measures)))
    list(
      DT::renderDataTable({cbind(factors,data)})
    )


  })


  aggreg<-reactive({
    data<-getdata_upload()
    factors<-attr(data,"factors")[rownames(data),,drop=F]
    coords<-attr(data,"coords")



    if(ncol(factors[input$fac_descs])==1){
      df<-do.call("cbind",lapply(data,function(x, fac) tapply(x,factors[,input$fac_descs] ,get(input$spread_measures))))
      if(!is.null(coords)){
      coords<-do.call("cbind",lapply(coords[rownames(data),],function(x, fac) tapply(x,factors[,input$fac_descs] ,mean)))
     # coords<-coords[,which(unlist(lapply(coords,is.numeric)))]
      rownames(coords)<-rownames(df)
      }

      facs<-data.frame(fac=as.factor(rownames(df)))
      colnames(facs)<-input$fac_descs
      rownames(facs)<-rownames(df)
      df<-data_migrate(data,df,"Aggregated_results")
      attr(df,"factors")<-facs
      attr(df,"coords")<-coords


    } else {
      df<- data.frame(aggregate(data,data.frame(factors[,input$fac_descs]),get(input$spread_measures)))
      if(!is.null(coords)){
        coords<- data.frame(aggregate(coords,factors[,input$fac_descs],mean ))
        coords<-coords[,which(unlist(lapply(coords,is.numeric)))]
        rownames(coords)<-rownames(df)
      }
      dfnum<-df[,which(unlist(lapply(df,is.numeric)))]
      dffac<-df[,which(unlist(lapply(df,is.factor)))]
      df<-dfnum
      df<-data_migrate(data,df,"Aggregated_results")
      rownames(dffac)<-rownames(df)


      attr(df,"factors")<-dffac
      attr(df,"coords")<-coords
    }

   #if(anyNA(df)){df<-"Requires groups with at least two counts "}

      df
  })



  output$stats_pbox<-renderUI({
    fluidRow(style="background: white",

             column(12, uiOutput("boxplot_control")),
             column(12, plotOutput("boxplot_out")))
  })

  output$stats_attributes<-renderUI({
    data<-getdata_upload()
    factors<-attr(data,"factors")
    coords<-attr(data,"coords")
    base_shape<-attr(data,"base_shape")
    layer_shape<-attr(data,"layer_shape")
    fluidRow(
      column(12,
             h5(strong("Datalist:"),em(input$data_upload,style="color: gray40")),
             if(is.null(attr(getdata_upload(),"som"))){
               radioGroupButtons("view_datalist",NULL,
                                 choiceNames =list(
                                   popify(icon("fas fa-archive"),NULL,"Data-Attribute",options=list(container="body")),
                                   popify(icon("fas fa-boxes"),NULL,"Factor-Attribute",options=list(container="body")),
                                   popify(icon("fas fa-map-marker-alt"),NULL,"Coords-Attribute",options=list(container="body")),
                                   popify(div("B",icon("fas fa-map")),NULL,"base_shape-Attribute",options=list(container="body")),
                                   popify(div("L",icon("far fa-map")),NULL,"layer_shape-Attribute",options=list(container="body"))
                                 ),
                                 choiceValues =list(
                                   "data","factors","coords","base_shape","layer_shape"

                                 ),status="button_active"
               )
             } else{

               radioGroupButtons("view_datalist",NULL,
                                 choiceNames =list(
                                   popify(icon("fas fa-archive"),NULL,"Data-Attribute",options=list(container="body")),
                                   popify(icon("fas fa-boxes"),NULL,"Factor-Attribute",options=list(container="body")),
                                   popify(icon("fas fa-map-marker-alt"),NULL,"Coords-Attribute",options=list(container="body")),
                                   popify(div("B",icon("fas fa-map")),NULL,"base_shape-Attribute",options=list(container="body")),
                                   popify(div("L",icon("far fa-map")),NULL,"layer_shape-Attribute",options=list(container="body")),
                                   popify(icon("fas fa-braille"),NULL,"Som-Attribute",options=list(container="body"))
                                 ),
                                 choiceValues =list(
                                   "data","factors","coords","base_shape","layer_shape","som"

                                 ),status="button_active"
               )

             }
      ),


      column(12,uiOutput("view_out"))
    )
  })

  output$stats_datalist<-renderUI({
    list(
      datalist_render(getdata_upload(),bagdata$df)
         # ,column(12, "getdata_upload",renderPrint(lapply(attributes(getdata_upload()), head))),
         #column(12, 'saved_data',renderPrint(lapply(attributes(saved_data$df[[input$data_upload]]), head)))
    )
  })


  output$view_out<-renderUI(column(12,

    conditionalPanel("input.view_datalist=='data'",{
      uiOutput("viewdata")
    }),
    conditionalPanel("input.view_datalist=='factors'",{
      uiOutput("viewfactors")
    }),
    conditionalPanel("input.view_datalist=='coords'",{
      uiOutput("viewcoords")
    }),
    conditionalPanel("input.view_datalist=='base_shape'",{
      uiOutput("viewbase")
    }),
    conditionalPanel("input.view_datalist=='layer_shape'",{
      uiOutput("viewlayer")
    }),
    conditionalPanel("input.view_datalist=='som'",{
      uiOutput("viewsom")
    })

  ))

  output$tools_viewdata<-renderUI({
    radioGroupButtons(
      "view_data_tools",NULL,
      choiceNames =list(
        popify(span(icon("fas fa-eye")),NULL,"View",options=list(container="body")),
        popify(span(icon("fas fa-ruler-vertical"),icon("fas fa-caret-right")),NULL,"Data Summary",options=list(container="body")),
        popify(span(div(icon("fas fa-ruler-vertical fa-rotate-90"), style="margin-top: -6px"),div(icon("fas fa-caret-down"), style="margin-top: -14px")),NULL,"Variable summary",options=list(container="body")),
        popify(div(icon("fas fa-chart-bar"),style="margin-left: -5px"),NULL,"Histogram",options=list(container="body")),
        popify(div(strong("pca")),NULL,"Principal Component Analysis",options=list(container="body")),
        popify(div(strong("mds")),NULL,"Nonmetric Multidimensional Scaling",options=list(container="body")),
        popify(span(div(icon("fas fa-minus fa-rotate-90"), style="margin-top: -5px"),div(icon("fas fa-square"), style="margin-top: -17px"),div(icon("fas fa-minus fa-rotate-90"), style="margin-top: -16px; margin-bottom: -2px")),NULL,"Box plot",options=list(container="body")),
        popify(div(strong("agg")),NULL,"Aggregate",options=list(container="body"))

      ),
      choiceValues =list(
        "view","summ_data","summ_fac","hist","pca","mds","box","agg"
      ),status="button_active"

    )
  })

  output$viewdata<-renderUI({fluidRow(style="overflow-x: scroll;",

    h5(strong("Data-Attribute"),tipify(actionButton("downcenter_data",icon("fas fa-download")),"Download table", options=list(container="body"))),


    DT::renderDataTable(getdata_upload())

  )})
  output$viewfactors<-renderUI({fluidRow(style="overflow-x: scroll;",

    h5(strong("Factor-Attribute"),tipify(actionButton("downcenter_factors",icon("fas fa-download")),"Download table", options=list(container="body"))),
    column(12,style="margin-left: -20px",
           splitLayout(cellWidths = c(
             '5%',"95%"
           ),
             div(
               div(popify(bsButton("selobs_all", icon("fas fa-check-square"),style  = "button_active", type="action",value=FALSE, block=F),NULL,
                        "Select all",options=list(container="body")
               )),
               div(popify(bsButton("selobs_none", icon("fas fa-minus-square"),style  = "button_active", type="action",value=FALSE, block=F),NULL,
                        "unselect all",options=list(container="body")
               )),
               div(popify(bsButton("selobs", icon("fas fa-file-signature"),style  = "button_active", type="action",value=FALSE, block=F),NULL,
                        "Create a datalist with the selected observations",options=list(container="body")
               ))
             ),
             DT::dataTableOutput('x1_factors')
           )),
    uiOutput("selobs_fac")


  )})

  output$selobs_fac<-renderUI({
    fluidRow(


      column(12,style="overflow-x: scroll;",
             renderPrint({
               s = input$x1_factors_rows_selected
               if (length(s)) {
                 cat('Selected observations:\n\n')
                 cat(rownames(getdata_upload())[s], sep = ', ')
               }
             })
      )



    )
  })

  observeEvent(input$selobs,{
    if(input$selobs %% 2) {
      datahand$df<-"Create a datalist from the selected observations"
      datahand2$df<-NULL
      datahand3$df<-NULL
      showModal(
        datahand_modal()
      )
    }
  })
  selall<-reactiveValues(df=NULL)


  observeEvent(input$selobs_all,{
    req(length(saved_data$df)>0)
    selall$df<-1:nrow(getdata_upload())
  })
  observeEvent(input$selobs_none,{
    req(length(saved_data$df)>0)
    selall$df<-NULL
  })

  output$x1_factors = DT::renderDataTable( {

    attr(getdata_upload(),"factors")
  }, server = FALSE ,filter = "top",
  selection = list(mode = 'multiple', selected = c(selall$df)),
  options=list(pageLength = 10, info = FALSE,lengthMenu = list(c(10,20, -1), c("15", "20","All")) ))

  output$viewcoords<-renderUI({
    if(is.null(attr(getdata_upload(),"coords"))) {fluidRow(

      column(12,
             p(strong("No coords found in Datalist:",style="color: red"),em(input$data_upload))),
      uiOutput("add_coords_intru"),
      column(12,style="margin-top: 20px",
             splitLayout(
               cellWidths = c("30%","20%"),
               column(12,
                      fileInput(inputId = "add_coords_file",label = NULL)),
               column(12,
                      uiOutput("add_coords_button"))

             ))

    )} else{
      fluidRow(
        h5(strong("Coords-Attribute"),tipify(actionButton("downcenter_coords",icon("fas fa-download")),"Download table", options=list(container="body"))),
        DT::renderDataTable(attr(getdata_upload(),"coords"))

      )
    }

  })

  {

    output$add_coords_button<-renderUI({
      req(length(input$add_coords_file$datapath)>0)
      actionButton("add_coords",icon=icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"),"Click to save to the Datalist", options=list(container="body")),
                          "Add", style="button_active"))
    })

    output$add_coords_intru<-renderUI({
      column(12,
             column(12,style="margin-top: 20px",
                    strong("1. Use the button below to upload one.")),
             if(length(input$add_coords_file$datapath)>0){
              fluidRow(
                column(12,style="margin-top: 20px",
                       strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_upload, style="color: SeaGreen"))),
                column(12,
                       uiOutput("error_coords"))


              )
             }




      )
    })

  }
  {
    output$add_base_button<-renderUI({
      req(length(input$add_base_file$datapath)>0)
      actionButton("add_base",icon=icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"),"Click to save to the Datalist", options=list(container="body")),
                          "Add", style="button_active"))
    })

    output$add_base_intru<-renderUI({
      column(12,
             column(12,style="margin-top: 20px",
                    strong("1. Use the button below to upload one.")),
             if(length(input$add_base_file$datapath)>0){
               column(12,style="margin-top: 20px",
                      strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_upload, style="color: SeaGreen")))
             }
      )
    })
  }

  {
    output$add_layer_button<-renderUI({
      req(length(input$add_layer_file$datapath)>0)
      actionButton("add_layer",icon=icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"),"Click to save to the Datalist", options=list(container="body")),
                          "Add", style="button_active"))
    })

    output$add_layer_intru<-renderUI({
      column(12,
             column(12,style="margin-top: 20px",
                    strong("1. Use the button below to upload one.")),
             if(length(input$add_layer_file$datapath)>0){
               column(12,style="margin-top: 20px",
                      strong("2. Click ",strong("Add", style="color: SeaGreen")," to insert it to the Datalist:", em(input$data_upload, style="color: SeaGreen")))
             }

      )
    })
  }


  uploadShpfile<-eventReactive(input$shp, {
    user_shp <- Read_Shapefile(input$shp)
    bagshp$df<-T
    user_shp
   })
  testeshp <- reactive({
    userdir<-getwd()
    shpfile=input$shp$datapath[grep(".shp",input$shp$datapath)]
    shpdir<-gsub('.shp',"",shpfile)
    setwd(shpdir)
    res<-st_read(shpfile)
    setwd(userdir)
    res
  })

observeEvent(input$shp_add_base,{
  attr(saved_data$df[[input$data_map]],"base_shape")<-filtershp()

})
observeEvent(input$shp_add_layer,{

  attr(saved_data$df[[input$data_map]],"layer_shape")<-filtershp()
})
output$save_feature <- {
  downloadHandler(
    filename = function() {
      paste0("feature_shape","_", Sys.Date())
    }, content = function(file) {
      saveRDS(filtershp(),file)
    })

}


observeEvent(input$shp_create,{
 showModal(
   modalDialog(
     column(12,
            fileInput(inputId = "shp", label = strong("Import shapefiles",tiphelp("please upload all files at once")), multiple = TRUE, accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj')),
            uiOutput("shp_feature1"),
            splitLayout(
              cellWidths = c("10%","90%"),
              column(12,
                     p(bsButton("shp_add_base",strong(popify(div("B",icon("fas fa-map")),NULL,paste0(span("Click to add shapefiles into the Datalist as a Base-Shape-Attribute")), options=list(container="body")),"Add", style='button_active'))),
                     p(bsButton("shp_add_layer",strong(popify(div("L",icon("fas fa-map")),NULL,paste0(span("Click to add shapefiles into the Datalist as a Layer-Shape-Attribute")), options=list(container="body")),"Add", style='button_active'))
                     ),
                     p(tipify(downloadButton("save_feature",label =NULL),"save feature as r file", options=list(container="body")),
                     )),
              renderPlot({
                ggplot(st_as_sf(filtershp())) + geom_sf()+
                  theme(panel.background = element_rect(fill = "white"),
                        panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))


              })

            )
     ),
     title="Create a base_shape/layer_shape",
     easyClose = T,
     size = "l"
   )
 )

})

output$shp_feature1<-renderUI({

  req(isTRUE(bagshp$df))
  atributos_shp<-attributes(uploadShpfile())$names
  splitLayout(
    selectInput('shp_feature1',"feature 1",choices=c("None",atributos_shp), selectize = F),
    uiOutput("shp_feature2")
  )
})

filtershp<-reactive({
  bacias<-    uploadShpfile()
  req(isTRUE(bagshp$df))
  if(input$shp_feature1!="None"&input$shp_feature2!="None")
  {
    bacias<-uploadShpfile()
    bacias<-bacias[bacias[[input$shp_feature1]]==input$shp_feature2,]

  }
  bacias
})

output$shp_feature2<-renderUI({
  req(isTRUE(bagshp$df))
  lev_attrs<-unique(uploadShpfile()[[input$shp_feature1]])
  selectInput('shp_feature2',"feature 2",choices=c("None",lev_attrs), selectize = F)
})


  output$viewbase<-renderUI({
    column(12,

           #renderPlot(uploadShpfile()),
      h5(strong("Base-Attribute")),
      if(is.null(attr(getdata_upload(),"base_shape"))) {fluidRow(

        column(12,
               p(strong("No base_shape found in Datalist:",style="color: red"),em(input$data_upload))),
        uiOutput("add_base_intru"),
        column(12,style="margin-top: 20px",
               splitLayout(
                 cellWidths = c("30%","20%"),
                 column(12,
                        fileInput(inputId = "add_base_file",label = NULL)),
                 column(12,
                        uiOutput("add_base_button"))

               ))

      )} else {
        renderPlot({
          base_shape<-attr(getdata_upload(),"base_shape")
          ggplot(st_as_sf(base_shape)) + geom_sf()+
            theme(panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))

        })
      }

    )
  })

  output$viewlayer<-renderUI({
    if(is.null(attr(getdata_upload(),"layer_shape"))) {fluidRow(

      column(12,
             p(strong("No layer_shape found in Datalist:",style="color: red"),em(input$data_upload))),
      uiOutput("add_layer_intru"),
      column(12,style="margin-top: 20px",
             splitLayout(
               cellWidths = c("30%","20%"),
               column(12,
                      fileInput(inputId = "add_layer_file",label = NULL)),
               column(12,
                      uiOutput("add_layer_button"))

             ))

    )} else    {
      fluidRow(
        h5(strong("Layer-Attribute")),
        renderPlot({
          layer_shape<-attr(getdata_upload(),"layer_shape")
          ggplot(st_as_sf(layer_shape)) + geom_sf()+
            theme(panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"))

        })
      )
      }

  })
  output$viewsom<-renderUI({

    fluidRow(
      h4("Som-Attribute",tipify(actionButton("downcenter_som",icon("fas fa-download")),"Download table", options=list(container="body"))),
      renderTable(combsom(), rownames = T)

    )
  })
  observeEvent(input$add_base,{

      t<-try({get(gsub(" ", "", capture.output(
        load(input$add_base_file$datapath, verbose = T)
      )[2]))})
    if("try-error" %in% class(t)) {t<-readRDS(input$add_base_file$datapath) }

      attr(saved_data$df[[input$data_upload]],"base_shape")<-t
    updateRadioGroupButtons(
      session,"view_datalist", selected="base_shape"
    )

  })

  observeEvent(input$add_layer,{

      t<-try({get(gsub(" ", "", capture.output(
        load(input$add_layer_file$datapath, verbose = T)
      )[2]))})
    if("try-error" %in% class(t)) {t<-readRDS(input$add_layer_file$datapath) }
      attr(saved_data$df[[input$data_upload]],"layer_shape")<-t

    updateRadioGroupButtons(
      session,"view_datalist", selected="layer_shape"
    )
    t
  })

  observeEvent(input$add_coords_file,{
    output$error_coords<-renderUI("Upload compleated")
  })

  observeEvent(input$add_coords,{

    coords<-data.frame(fread(input$add_coords_file$datapath))
    rownames(coords) <- coords[, 1]
    coords[, 1] <- NULL
    if(ncol(coords)!=2){ output$error_coords<-
      renderUI({
        column(12, strong(
          "Invalid Entries. The first column must contain the name of the observations. The second and third columns must contain the logitude and latitude respectively", style="color: red"))
      })}

    if(any(rownames(coords)%in%rownames(saved_data$df[[input$data_upload]]))==F) {
      output$error_coords<-
        renderUI({
          column(12, strong(
            "None of the IDs of the uploaded coordinates are compatible with the ids of the selected datalist. Please upload coodinates with valid IDs", style="color: red"))
        })
    }

     req(any(rownames(coords)%in%rownames(saved_data$df[[input$data_upload]])))

    attr(saved_data$df[[input$data_upload]],"coords")<-na.omit(
      coords[rownames(saved_data$df[[input$data_upload]]),]
    )
    updateRadioGroupButtons(
      session,"view_datalist", selected="coords"
    )

  })



  downcenter<-reactive({


    modalDialog(
      if(isFALSE(bagdata$df)){"the selected datalist has unsaved changes.Please save them before continuing."} else{
        column(12,
               h5(strong(getdown_tile())),
               splitLayout(cellWidths = c("30%","70%"),
                 column(12,
                        radioButtons("down_type",strong("format",tipify(icon("fas fa-question-circle"),"file extension", options=list(container="body"))),c(".xlsx",".csv"))),

                 conditionalPanel("input.down_type=='.csv'",{
                   splitLayout(
                     column(12,
                            radioButtons("down_sep",strong("sep",tipify(icon("fas fa-question-circle"),"the field separator string. Values within each row of x are separated by this string.", options=list(container="body"))),
                                         choiceValues =list(",",";"),
                                         choiceNames =list(
                                           "comma",
                                           "semicolon"
                                         )
                            )),
                     column(12,

                            conditionalPanel("input.down_sep==';'",{
                         radioButtons("down_dec",strong("dec",tipify(icon("fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                                      choiceValues =list(".",","),
                                      choiceNames=list(
                                        "dot","comma"))
                       }),
                       conditionalPanel("input.down_sep==','",{
                         column(12,
                                radioButtons("down_dec",strong("dec",tipify(icon("fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                                             choiceValues =list("."),
                                             choiceNames=list("dot")
                                ))
                       })
                     )

                   )
                 })
               ),
               column(12,
                      downloadButton("download_action",NULL,icon=icon("fas fa-download"),style="width: 50%"))

        )
      }
     ,

      title=h4(icon("fas fa-download"),strong("Download")),
      size="m",
      easyClose = T

    )
  })
  downcenter_hand<-reactiveValues(df=0)

  observeEvent(input$downcenter_data,{
    downcenter_hand$df<-"data"
    showModal(downcenter())

  })
  observeEvent(input$downcenter_factors,{

    downcenter_hand$df<-"factors"
    showModal(downcenter())
  })
  observeEvent(input$downcenter_coords,{

    downcenter_hand$df<-"coords"
    showModal(downcenter())
  })
  observeEvent(input$downcenter_som,{

    downcenter_hand$df<-"som"
    showModal(downcenter())
  })


  output$download_action <- {
    downloadHandler(
      filename = function() {
        paste0(input$downcenter_hand,"_", Sys.Date(), input$down_type)
      }, content = function(file) {
        if(input$down_type==".csv"){
          write.table(x=data.frame(getdown()),file,append=T,quote=F,row.names=T,col.names=NA, input$down_sep,
                      dec=input$down_dec)
        }
        if(input$down_type==".xlsx"){
          library('readxl')
          write_xlsx(cbind(id=rownames(getdown()),getdown()), file)
        }


        })

    }




  combsom<-reactive({
    combsom<-do.call("cbind",lapply(attr(getdata_upload(),"som"),train.summary_fun))
    colnames(combsom)<-names(attr(getdata_upload(),"som"))
    combsom
  })

  combsom_down<-reactive({
    combsom_down<-do.call("cbind",lapply(attr(saved_data$df[[input$data_upload]],"som"),train.summary_fun))
    colnames(combsom_down)<-names(attr(saved_data$df[[input$data_upload]],"som"))
    combsom_down
  })



  cur_stats<-reactiveValues(df=0)
  getupload_side<-reactive({
    if(cur_stats$df==0){cur='stats_datalist'} else {cur=cur_stats$df}
    data=getdata_upload()
    if(length(attr(data,"data.factors"))>0)
    {
      radioGroupButtons("stats0",NULL,choiceNames =list(
        strong("WARGNING:", icon("fas fa-exclamation-triangle"),style  = "button_active", type="toggle"),
        "Datalist structure",  "Attributes","Data summary","Variable summary","Factor summary","Descriptive statistics","Histogram","Boxplot","PCA",  "MDS"),
        choiceValues = c("stats_classmat","stats_datalist","stats_attributes","stats_sum_data","stats_sum_variables","stats_sum_factors","stats_desc","stats_hist","stats_box","stats_pca","stats_mds"),direction ="vertical", selected=cur, status="button_active")
    } else {
      radioGroupButtons("stats0",NULL,choiceNames =list(
        "Datalist structure",  "Attributes","Data summary","Variable summary","Factor summary","Descriptive statistics","Histogram","Boxplot","PCA",  "MDS"),
        choiceValues = c("stats_datalist","stats_attributes","stats_sum_data","stats_sum_variables","stats_sum_factors","stats_desc","stats_hist","stats_box","stats_pca","stats_mds"),direction ="vertical", selected=cur, status="button_active")
    }



  })

  output$upload_side<-renderUI({
    req(length(saved_data$df)>0)
    getupload_side()
  })

  #
  saved_data<-reactiveValues()
  output$output_data_upload<-renderUI({
    if(length(saved_data$df)<1) {
      column(12,
             p(style="margin-top: 10px;",
               icon("fas fa-hand-point-left", style="font-size: 30px; color: SeaGreen"),em("Get started by creating a Datalist")
             ))
    } else {

      if(cur_data$df==0){cur=names(saved_data$df)[1]} else {cur=cur_data$df}
      if(isTRUE(bagdata$df)){col="gray"} else{ col="#0093fcff"}
      change_alert<-paste0("#data_upload {border-color:",col,";border-width: 2px;}")
      div(
        tags$style(change_alert),
        selectInput("data_upload",NULL,choices=names(saved_data$df), selectize = F, selected=cur))
    }

  })

  output$upload_tools<-renderUI({

    column(12,
           fluidRow(style="height: 85px; margin-top: 0px; margin-bottom: -20px",

                    splitLayout(
                      splitLayout(
                        absolutePanel(style="margin-left: 0px",
                                      popify(actionButton("upload_button", icon("fas fa-plus")),"Data Input",textinput(),options=list(container="body"))
                        ),
                        column(12,uiOutput("output_data_upload")
                        ),
                        cellWidths = c("10%","90%")

                      )
                      ,
                      absolutePanel(style="margin-top: 0px",
                                    uiOutput("tools_bar")
                      ),
                      cellWidths = c("40%","20%")
                    )
           ))
  })

output$tools_bar<-renderUI({
  req(length(saved_data$df)>0)
  div(id="upload_tools",
      popify(bsButton("tools_selobs", icon("fas fa-list"),style  = "button_active", type="toggle"),NULL,
             "Select observations",options=list(container="body")
      ),
      popify(bsButton("tools_selvar", icon("fas fa-list fa-rotate-90"),style  = "button_active", type="toggle"),NULL,
             "Select variables",options=list(container="body")
      ),
      popify(bsButton("tools_transform", icon("fas fa-hammer"),style  = "button_active", type="toggle"),NULL,
             "Transform data",options=list(container="body")
      ),
      popify(bsButton("tools_edit", icon("fas fa-quidditch"),style  = "button_active", type="toggle",disabled=T, value=F),NULL,
             "Edit plot",options=list(container="body")
      ),
      popify(bsButton("tools_save", icon("fas fa-save"),style  = "button_active", type="action",value=FALSE),"Save changes",
             "Transformed/ filtered data can be saved as a new datalist or overwrite the original datalist",options=list(container="body")
      )
  )
})

  observeEvent(input$tools_transform,{
    if(input$tools_transform %% 2){
      shinyBS::updateButton(session,"tools_selvar",style  = "button_active", value=F)
      shinyBS::updateButton(session,"tools_selobs",style  = "button_active", value=F)
      shinyBS::updateButton(session,"tools_edit",style  = "button_active", value=F,disabled=ative_editpca())

    }
  })


  observeEvent(input$tools_selvar,{
    if(input$tools_selvar %% 2){
      shinyBS::updateButton(session,"tools_transform",style  = "button_active", value=F)
      shinyBS::updateButton(session,"tools_selobs",style  = "button_active", value=F)
      shinyBS::updateButton(session,"tools_edit",style  = "button_active", value=F,disabled=ative_editpca())

    }
  })


  observeEvent(input$tools_selobs,{
    if(input$tools_selobs %% 2){
      shinyBS::updateButton(session,"tools_transform",style  = "button_active", value=F)
      shinyBS::updateButton(session,"tools_selvar",style  = "button_active", value=F)
      shinyBS::updateButton(session,"tools_edit",style  = "button_active", value=F,disabled=ative_editpca())

    }
  })


  observeEvent(input$tools_edit,{
    if(input$tools_edit %% 2){
      shinyBS::updateButton(session,"tools_transform",style  = "button_active", value=F)
      shinyBS::updateButton(session,"tools_selvar",style  = "button_active", value=F)
      shinyBS::updateButton(session,"tools_selobs",style  = "button_active", value=F)

    }
  })



  observeEvent(input$stats0,{
    shinyBS::updateButton(session,"tools_transform",style  = "button_active", value=F)
    shinyBS::updateButton(session,"tools_selvar",style  = "button_active", value=F)
    shinyBS::updateButton(session,"tools_selobs",style  = "button_active", value=F)
    shinyBS::updateButton(session,"tools_edit",style  = "button_active", value=F,disabled=ative_editpca())

  })

  ative_editpca<-reactive({
    if( input$stats0=='stats_pca'|input$stats0=='stats_mds'|input$stats0=='stats_box'){res=F} else{ res=T}
    res
  })


  observeEvent(input$stats0,{
    if(input$stats0=="stats_pca"|input$stats0=="stats_mds"|input$stats0=="stats_box"){
      shinyBS::updateButton(session,"tools_edit",NULL,icon=icon("fas fa-quidditch"),style  = "button_active", value=F,disabled=ative_editpca())} else{
      }
  })

  output$pop_transform<-renderUI({
    column(12,style="border-top: 3px solid SeaGreen;border-bottom: 3px solid SeaGreen;background: white; margin-top: -30px;  margin-bottom:20px",id="pop_transform",
           #width = "680px", draggable = TRUE,height ='65px',style="border: 2px solid SeaGreen;background: white;",top=0,left=-300,
           splitLayout(style="margin-left: 0px;margin-right: 10px; margin-bottom: -20px",
             cellWidths = paste0(c(20,30,25,25),"%"),
             checkboxInput("na.omit", strong("NA.omit"), value = F),
             uiOutput("remove_sp"),
             fluidRow(style="margin-left: 20px;",
               p(p(style="margin-bottom: -2px;",
                   strong("Transformation",actionLink("transfhelp", tipify(icon("fas fa-question-circle"), "Click for more details"), style = "margin-left: 2px"))),selectInput("transf",NULL,choices =c('None','log','total','max','frequency','range','pa','chi.square','hellinger'),selectize = F))
             ),
             fluidRow(style="margin-left: 20px;",
               p(
                 p( p(strong('Scale'),actionLink("scalehelp", tipify(icon("fas fa-question-circle"), "Click for more details"), style = "margin-left: 2px"))),
                 p(
                   splitLayout(
                     checkboxGroupInput("scale", NULL, choices =
                                          c("scale")),
                     conditionalPanel("input.scale=='scale'", {
                       checkboxGroupInput("center", NULL, choices = c("center"))

                     }))
                 )

               )
             )





           )
    )
  })

  output$remove_sp<-renderUI({
    splitLayout(
      p(p(style="margin-bottom: -2px", strong("Remove"), actionLink(
        "Remove", tipify(icon("fas fa-question-circle"), "Click for more details"))),
        selectInput("rares",NULL,choices = c("None", "singletons", "Abund<", "Freq<"),selectize = F)
      ),
      uiOutput("pct_remove")
    )
  })

  removebag<-reactiveValues(df=c("100%","0"))

  size_remove<-reactive({
    res=c("100%","0")
    if(length(input$rares)>0){
      if(input$rares=='Abund<'){ res=c("50%","50%")} else if(input$rares=='Freq<'){
        res=c("50%","50%")
      } else { res=c("100%","0")}
    }
  })

  output$pct_remove<-renderUI({
    if(input$rares=='Abund<'){
      p(p(style="margin-bottom: -2px", strong("%")),
        numericInput('pct_rare', NULL, value =0.001)
      )

    } else if(input$rares=='Freq<'){p(p(style="margin-bottom: -2px", strong("%")),
                                      numericInput('pct_prev', NULL, value =0.001)
    )}

  })
  datarawbag<-reactiveValues(df=0)
  dataraw <- reactive({
    req(input$up_or_ex)
    if (input$up_or_ex == 'use example data') {
      data <-
        data.frame(fread("meta/nema_araca.csv", stringsAsFactors = T))
      rownames(data) <- data[, 1]
      data[, 1] <- NULL


    } else {
      validate(need(length(input$filedata$datapath)!=0,"Data file is required"))
      data <-
        data.frame(fread(input$filedata$datapath, stringsAsFactors = T))
      rownames(data) <- data[, 1]
      data[, 1] <- NULL

    }

    bagdataraw$df<-data
    data
  })

  getdatalist<-reactive({
    data=dataraw()
    if (any(names(datastr(data)$type.vars) == "factor")){#ok
      data.numerics <-data[, which(unlist(lapply(data, function(x)is.numeric(x))))]
      data.factors<-data[, which(unlist(lapply(data, function(x)is.factor(x))))]
      rownames(data.numerics)<-rownames(data)
      data<-data.numerics
      attr(data,"data.factors")<-data.factors}


    if(input$up_or_ex == 'use example data'){
      attr(data,"filename")<-'nema_araca.csv'
      attr(data, "datalist") <- paste("Datalist_nema_araca")
    } else {attr(data,"filename")<-input$filedata$name
    attr(data, "datalist") <- gsub(".csv","",input$filedata$name)

    }

    attr(data, "transf")<-attr(data, "transf")
    #attr(data, "transf")<-newattribs
    attr(data, "factors") <- labels()[rownames(data),,drop=F]
    attr(data,"coords") <- coords()[rownames(data),]
    attr(data,"base_shape") <- base_shape()
    attr(data,"layer_shape") <- layer_shape()

    datalist = list(data)
    names(datalist) <- input$data_name
    datalist
  })

  observeEvent(input$upload_insert,{
    if(input$up_or_ex=="upload"){
      validate(need(length(input$labels$datapath)>0,"error"))
      validate(need(length(input$filedata$datapath)>0,"error"))
    }
    datalist<-getdatalist()
    if(is.null(saved_data$df)){
      saved_data$df<-datalist} else {
        saved_data$df <- c(saved_data$df, datalist)
      }
    upload_bag$df<-0
    cur_data$df<-attr(datalist[[1]],"datalist")
    removeModal()
  })






  observeEvent(input$stats0,{
    cur_stats$df<-input$stats0
  })

  observeEvent(input$data_upload,{
    req(length(saved_data$df)>0)
    cur_data$df<-input$data_upload
  })



  output$filterdata <- renderUI({
    filterdata()
  })
  output$pop_selecvar <- renderUI({
    absolutePanel(draggable = F,top=35,right=100,
                  style = 'overflow-x: scroll;height:250px;overflow-y: scroll; border-top: 3px solid SeaGreen;border-bottom: 3px solid SeaGreen;background: white; margin-right: 40px; margin-bottom:-5px',
                  column(12,
                         h5(strong("Select variables",tiphelp("click to include/exclude the variables"))),
                         checkboxInput("check_fac",'Select/Unselect all',T),
                         hr(),
                         checkboxGroupInput(
                           "selecvar",NULL,
                           choices = colnames(saved_data$df[[input$data_upload]]),
                           selected = colnames(saved_data$df[[input$data_upload]])
                         )
                  ))



  })

  observeEvent(input$check_fac,{
    if(isTRUE(input$check_fac)){
      updateCheckboxGroupInput(session,
                               "selecvar",NULL,
                               choices = colnames(saved_data$df[[input$data_upload]]),
                               selected = colnames(saved_data$df[[input$data_upload]])
      )
    } else{
      updateCheckboxGroupInput(session,
                               "selecvar",NULL,
                               choices = colnames(saved_data$df[[input$data_upload]])

      )
    }

  })

  output$pop_selecobs <- renderUI({
    absolutePanel(draggable = F,top=35,right=100,
                  style = 'border-top: 3px solid SeaGreen;border-bottom: 3px solid SeaGreen;background: white; margin-right: 40px; margin-bottom:-5px; width: 330px',
                  column(12,h5(strong("Select observations",tipify(a(icon("fas fa-question-circle")),"Restrict observations to a factor level", options = list(container="body"))))),
                  column(12,
                         splitLayout(
                           selectInput("filter_data", span("Filter",tipify(a(icon("fas fa-question-circle")),"The factor to be used as filter", options = list(container="body"))),c("none", colnames(attr(saved_data$df[[input$data_upload]],"factors"))), selectize = F),
                           conditionalPanel("input.filter_data != 'none'",{
                             uiOutput("cutlevel_obs")
                           })
                         )
                  ))



  })


  output$cutlevel_obs<-renderUI({
    column(12,
           selectInput("cutlevel_obs", span("cutoff",tipify(icon("fas fa-question-circle"),"The factor level to restric the observations")), levels(attr(saved_data$df[[input$data_upload]],"factors")[, input$filter_data]), selectize = F)
    )
  })





  getdata_upload<-reactive({
    data=data.rares()
    bagdata$df<-identical(as.matrix(data),as.matrix(saved_data$df[[input$data_upload]]))
    data
  })





  seldata <- reactive({
    data <-  saved_data$df[[input$data_upload]]
    factors<-attr(data,"factors")
    if(length(input$filter_data)>0) {
      if (input$filter_data != "none")
      { if(length(input$cutlevel_obs)>0){
        pic <-which(as.character(factors[, input$filter_data]) == as.character(input$cutlevel_obs))
        data = data[pic, ]}}
    }
    if (length(input$selecvar) > 0) {
      data <- data[, input$selecvar]
    }
    data
  })



  data_factors <- reactive({
    getdata_upload()[, which(unlist(lapply(getdata_upload(), function(x)
      is.factor(x))))]
  })




  data.rares<-reactive({

    #bagdata$df <- isolate(0)
    data = seldata()
    if (isTRUE(input$na.omit)) {data <- na.omit(data)}
    if (sum(colSums(data, na.rm=T) == 0) > 0) {data = data[, -which(colSums(data, na.rm=T) == 0)] }
    if (length(input$transf) > 0) {
      if (any(input$scale == "scale")) {
        if (any(input$center == "center")) {
          data = data.frame(scale(data, center = T))
        } else {
          data = data.frame(scale(data, center = F))
        }
      }

    }

    if (length(input$rares) > 0) {

      if (input$rares == "singletons") {data = singles(data)}
      if (input$rares == "Abund<") {data = pctRares(data, input$pct_rare)}
      if (input$rares == "Freq<") {data = pctPrev(data, input$pct_prev)}
    }

    if (length(input$transf) > 0) {
      if (input$transf == "log") {data = decostand(data, "log", na.rm = T)}
      if (input$transf == "total") {data = decostand(data, "total", na.rm = T)}
      if (input$transf == "max") {data = decostand(data, "max", na.rm = T)}
      if (input$transf == "frequency") {data = decostand(data, "frequency", na.rm = T)}
      if (input$transf == "range") {data = decostand(data, "range", na.rm = T)}
      if (input$transf == "pa") {data = decostand(data, "pa", na.rm = T)}
      if (input$transf == "chi.square") {data = decostand(data, "chi.square", na.rm = T)}
      if (input$transf == "hellinger") {data = decostand(data, "hellinger", na.rm = T)}
      data = data.frame(data)

    }
    data<-data_migrate(saved_data$df[[input$data_upload]],data,input$data_upload)
    nrow_o<-nrow(saved_data$df[[input$data_upload]])
    nrow_g<-nrow(data)
    ncol_o<-ncol(saved_data$df[[input$data_upload]])
    ncol_g<-ncol(data)
    newattribs<-isolate(
      data.frame(
        Subobs=if(nrow_g<nrow_o){paste(nrow_o,"::",nrow_g)} else {0},
        Subvars=if(ncol_g<ncol_o){paste(paste(ncol_o,"::",ncol_g))}else{0},
        Transformation = if(is.null(input$transf)){"None"}else{input$transf},
        Removed = if(is.null(input$rares)){"None"}else{input$rares},
        Scale = if(is.null(input$scale)){"None"}else{input$scale},
        Center = if(is.null(input$center)){"FALSE"}else{input$center},
        NA.omit = if(is.null(input$na.omit)){"FALSE"}else{input$na.omit}
      )
    )

    attr_data<-rbind(attr(data, "transf"),newattribs)
    rownames(attr_data)<-paste0("change", 1:nrow(attr_data))
    rownames(attr_data)[nrow(attr_data)]<-"current"


    attr(data, "transf") <-attr_data



    #if(nrow(attr(data, "transf"))==2){attr(data, "transf")<-attr(data, "transf")[-1,]}
    attr(data,"factors")<-attr(data,"factors")[rownames(data),,drop=F]




    # validate(need(!any(unlist(lapply(saved_data$df, function(x) identical(data,x)))),"you already have  the selected results in  your Data bank"))


    data
  })





  output$rfdata <- renderUI({
    column(
      12,
      br(),
      selectInput(
        "rfdata",
        "Select the data for the Random Forest analysis",
        choices =    names(saved_data$df)
      )
    )
  })




  somcontrol <- reactive({
    fluidRow(column(
      12,
      div(br()),
      div(br()),
      br(),
      column(
        4,
        selectInput(
          'distmethod',
          strong(
            "dist.fcts",
            tipify(
              icon("fas fa-question-circle"),
              "Distance measure between each neuron and input data"
            )
          ),
          choices = c(
            "BrayCurtis",
            "euclidean",
            "sumofsquares",
            "manhattan",
            "tanimoto"
          ),
          selectize = F
        )
      ),
      column(4, numericInput(
        "rlen",
        strong(
          "rlen",
          tipify(
            icon("fas fa-question-circle"),
            "The number of times the complete dataset will be presented to the network"
          )
        ),
        value = 500,
        min = 1,
        step = 1
      )),
      column(4, numericInput(
        "seed",
        strong(
          "seed",
          tipify(
            icon("fas fa-question-circle"),
            "A numeric value. If supplied, it ensures that you get the same result if you start with that same seed each time you run the som analysis"
          )
        ),
        value = NULL,
        min = 0,
        step = 1
      )),

      column(
        12,
        column(
          9,
          align = "center",
          br(),
          popify(actionButton("trainSOM", h4(
            icon("fas fa-braille"),
            "train SOM",
            icon("fas fa-arrow-circle-right")
          ), style = "background:  #05668D; color: white"),
          "TrainSOM","Train the Data-Attribute from the selected Datalist")
        ),
        column(3, align = "right", actionButton("resetsom", "reset"))
      ),

      column(12,
             bsCollapse(
               bsCollapsePanel(
                 style = "background: white;",
                 tipify(h5("Fine tuning*"), "show all parameters available"),
                 finesom()
               )
             ))
    ))

  })
  output$somcontrol <- renderUI({
    somcontrol()
  })
  observeEvent(input$resetsom, {
    output$somcontrol <- renderUI({
      somcontrol()
    })
  })
  topocontrol <- reactive({
    if (isTRUE(input$sugtopo)) {
      dim = topo.reactive()
      xdim <- dim[[2]]
      ydim <- dim[[3]]
    } else {
      xdim <- input$xdim
      ydim <- input$ydim
    }
    fluidRow(
      id = "topocontrol",

      column(
        12,
        column(4, numericInput(
          'xdim',
          "xdim",
          value = xdim,
          min = 0,
          step = 1
        )),
        column(4, numericInput(
          "ydim",
          "ydim",
          value = ydim,
          min = 0,
          step = 1
        )),


        column(4, selectInput(
          "topo",
          "topo",
          choices = c("hexagonal", "rectangular"),
          selectize = F
        )),
        column(10, plotOutput("showgrid", height = "100px")),
        column(2, align = "right", actionButton("resettopo", "reset"))
      ),
      column(12,
             bsCollapse(
               bsCollapsePanel(
                 style = "background: white;",
                 tipify(h5("Fine tuning*"), "show all parameters available"),
                 column(
                   6,
                   selectInput(
                     "neighbourhood.fct",
                     label = "neighbourhood.fct",
                     choices = c("bubble","gaussian")
                   )
                 ),
                 column(6, selectInput(
                   "toroidal",
                   label = "toroidal",
                   choices = c("FALSE", "TRUE")
                 ))
               )
             ))
    )


  })
  output$somgridtopo <- renderUI({
    topocontrol()
  })
  observeEvent(input$resettopo, {
    output$somgridtopo <- renderUI({
      topocontrol()
    })
  })
  toroidal <- reactive({
    switch (input$toroidal,
            'TRUE' = TRUE,
            "FALSE" = FALSE)

  })
  finesom <- reactive({
    r1 <-
      as.vector(quantile(unit.distances(
        kohonen::somgrid(
          input$xdim,
          input$ydim,
          topo = input$topo,
          toroidal = toroidal(), neighbourhood.fct=input$neighbourhood.fct
        )
      ), 2 / 3))
    fluidRow(
      column(
        4,
        numericInput(
          inputId = "a1",
          label = "a1",
          value = 0.05,
          step = 0.01
        )
      ),
      column(
        4,
        numericInput(
          inputId = "a2",
          label = "a2",
          value = 0.01,
          step = 0.01
        )
      ),
      column(4, numericInput(
        inputId = "r1",
        label = "r1",
        value = r1
      )),
      column(
        4,
        numericInput(
          inputId = "r2",
          label = "r2",
          value = 0,
          step = 0.01
        )
      ),

      column(6, selectInput(
        "mode", "mode", choices = c("pbatch", "batch", "online")
      )),
      column(
        6,
        numericInput(
          "maxna",
          "maxNA.fraction",
          value = 0.001,
          step = 0.01
        )
      )
    )

  })


  cutsom.reactive <- reactive({

    m <- getmodel_hc()
    somC <- cutsom(m, picK(), method.hc = input$method.hc0, palette=input$hcdata_palette)
    somC
  })

  cutdata.reactive <- reactive({

    somC <-
      cutdata2(getdata_hc(),
               picK(),
               method.hc = input$method.hc0,
               dist = input$disthc, input$hcdata_palette)
    somC
  })


  phc <- reactive({

    if (input$model_or_data == "som codebook") {
      somC <- cutsom.reactive()
    } else if (input$model_or_data == "data") {
      somC <- cutdata.reactive()
    }
    somC

  })



  output$phc <- renderUI({
    hc<-phc()

    list(renderPlot({
      hc_plot(hc)
      phc_book$df<-T
    }),

    column(12, downloadButton('down_phc', 'Download')))


  })

  pclus_factors <- reactive({
    if (length(input$pclus_factors)>0) {
      as.factor(attr(getdata_hc(),"factors")[, input$pclus_factors])
    } else{NULL}
  })


  output$pclus <- renderUI({

    list(column(12,uiOutput("pclus_control")),
         column(12,
                renderPlot({
                  if(length(input$pclus_symbol)>0)
                  {pclus(
                    somC=cutsom.reactive(),
                    cex = as.numeric(input$pclus_symbol_size),
                    factor.pal = as.character(input$pclus_facpalette),
                    labels.ind = pclus_factors(),
                    pch=as.numeric(input$pclus_symbol),
                    points=bmu_clus_points(),
                    bg_palette=input$hcmodel_palette,
                    ncol=input$ncol_pclus,
                    insetx=input$insertx_pclus,
                    insety=input$inserty_pclus,
                    alpha.legend=input$bgleg_pclus
                  )}else{
                    pclus(cutsom.reactive(),  bg_palette=input$hcmodel_palette)
                  }
                })))
  })


  bmu_points<-reactive({
    if(input$dot_label == 'symbols'){ T} else {F}

  })

  bmu_clus_points<-reactive({
    if(input$dot_label_clus == 'symbols'){ T} else {F}

  })


  output$showgrid <- renderPlot({
    data = getdata_som()


    validate(need(input$xdim!="", ""))
    validate(need(input$ydim!="", ""))
    validate(need( (input$xdim*input$ydim)<=nrow(data), "The number of map units must be less than or equal to the number of observations. Please decrease the 'xdim' and/or 'ydim' dimensions"))
    par(mar = c(0, 0, 0, 0))
    m <-
      supersom(
        as.matrix(data),
        grid = kohonen::somgrid(input$xdim, input$ydim, topo = input$topo, neighbourhood.fct=input$neighbourhood.fct),
        rlen = 5
      )
    plot(
      m,
      shape = "straight",
      type = "mapping",
      pch = NA,
      main = ""
    )
  })
  output$var_pproperty <- renderUI({
    data = data.frame(getdata_som())
    column(
      12,
      selectInput(
        "variable_pproperty",
        label = "select a variable",
        choices = colnames(data)
      )
    )
  })


  output$map_control<-renderUI({
    req(length(saved_data$df)>0)
    fluidRow(
      column(12, style="background: white;",
             fluidRow(style="margin-top: 15px;",
                      column(6,style="background: Snow; border-right: 1px solid gray10;",

                             fluidRow(style="margin-right: -40px;",
                                      column(6,
                                             selectInput('maptype',strong("Map type:",popify(icon("fas fa-question-circle"),title = NULL,textdisplayas(),placement = "bottom",trigger = "hover",options=list(container="body"))),choices = c("discrete", "interpolation"), selectize = F)
                                      ),
                                      column(6,style="margin-left: -20px;margin-top: 2px",
                                             uiOutput("pt_palette")
                                      )),
                             conditionalPanel("input.maptype=='discrete'",{
                               uiOutput("scale_options00")
                             })

                      ),

                      column(6,
                             fluidRow(style="background: #ecf6f9",
                                      column(12, strong("Show:",style="color: #0D47A1")),
                                      column(6,
                                             div(style="margin-top: -10px",
                                                 checkboxInput("showcoords",strong("Coordinates"), T),
                                                 column(12,style="margin-top:-20px",
                                                        conditionalPanel("input.maptype=='discrete'",{checkboxInput("showguides","guides", F)})),
                                                 uiOutput("pt_coords")
                                             ),
                                             div(style="background: Snow", uiOutput('map_legend'))
                                      ),
                                      column(6,
                                             style="margin-top: -10px",
                                             checkboxInput("showfactors",strong("Labels"), F),
                                             uiOutput("pt_factor")),
                             )
                      )
             )
      ),
      column(12, style="margin-top: 5px",
             conditionalPanel("input.maptype=='discrete'",
                              {
                                uiOutput("discrete_control")
                              }),
             conditionalPanel("input.maptype=='interpolation'",
                              {
                                uiOutput("interp_control")
                              })
      )
    )
  })

  output$scale_options00<-renderUI({
    fluidRow(
      style="margin-right: -40px;",
      column(12,strong("Symbol", style="color: #0D47A1;")),
      column(5,conditionalPanel("input.maptype=='discrete'",
                                uiOutput("map_symbol"))),
      conditionalPanel("input.choices_map=='variable'",{
        column(6,
               checkboxGroupInput("scale_map_values",NULL,choices = c("scale",
                                                                      "colored"), inline = T, selected=c("scale")),
               fluidRow(
                 column(6,
                        uiOutput("scale_options")
                 ),
                 column(6,
                        uiOutput("mapfac")

                 )
               )
        )

      })
    )

  })

  output$mapfac<-renderUI({
    req("colored"%in%input$scale_map_values)
    fluidRow(p("by factor", style="color: #0D47A1; margin-top:-10px"),
             div(style=" margin-top:-10px",selectInput("map_lab",NULL, choices=colnames(attr(getdata_map(),"factors")), selectize = F, width="120%")))
  })
  output$scale_options<-renderUI({
    if("scale"%in%input$scale_map_values){
      fluidRow(
        style="margin-top: -10px; margin-left: 10px;",
        div(style="margin-top: -5px;margin-bottom: -5px",
            checkboxInput("scalesize_size","size",T)),
        if(!"colored"%in%input$scale_map_values){
          div(style="margin-top: -10px;margin-bottom: -5px",
              checkboxInput("scalesize_color","color",T))}

      )
    }
  })
  output$labels_coords<-renderUI({
    req(isTRUE(input$showfactors))
    fluidRow(

      p("factor", style="color: #0D47A1;margin-bottom: -2px;"),
      selectInput("labels_coords",NULL, colnames( attr(getdata_map(),"factors")),
                  selectize = F)

    )
  })






  output$pt_palette<-renderUI({
    pickerInput(inputId = "pt_palette",
                label = "Col palette",
                choices = df$val,
                choicesOpt = list(content = df$img))
  })






  output$map_symbol<-renderUI({
    fluidRow(style="margin-right: -40px;",
             column(6,p("shape", style="color: #0D47A1;margin-bottom: -2px;"),
                    pickerInput(inputId = "pt_symbol",
                                label = NULL,
                                choices = df_symbol$val,
                                choicesOpt = list(content = df_symbol$img))
             ),
             column(6,style="margin-left: -20px;",
                    p("size", style="color: #0D47A1;margin-bottom: -2px;"),
                    numericInput("pt_points",NULL, 1))
    )
  })
  output$map_legend<-renderUI({
    fluidRow(
      column(12,
             strong("Legend:", style="color: #0D47A1;")),
      fluidRow(
        style="margin-right: -20px; margin-left: 10px",
        column(6,
               p("label", style="color: #0D47A1;margin-bottom: -2px;"),
               textInput("map_legend",NULL, NULL, placeholder = "legend")),
        column(6,style="margin-left: -20px;",
               p("size", style="color: #0D47A1;margin-bottom: -2px;"),
               numericInput("pt_legend",NULL, 1))
      )

    )
  })







  output$pt_factor<-renderUI({
    req(isTRUE(input$showfactors))
    fluidRow(style="margin-top=-2px",
             fluidRow(
               column(6,
                      p("size", style="color: #0D47A1;margin-bottom: -2px;"),
                      numericInput("pt_factor",NULL, 1)),
               column(6,style="margin-left: -20px;",
                      p("color", style="color: #0D47A1;margin-bottom: -2px;"),
                      pickerInput(inputId = "col_factor",
                                  label = NULL,
                                  choices = colors_solid$val,
                                  choicesOpt = list(content = colors_solid$img)))
             ),

             column(8,uiOutput("labels_coords"))
    )
  })




  output$pt_coords<-renderUI({
    req(isTRUE(input$showcoords))
    fluidRow(style="margin-top=-2px",
             column(6,
                    p("size", style="color: #0D47A1;margin-bottom: -2px;"),
                    numericInput("pt_coords",NULL, 1)),
             column(6,style="margin-left: -20px;",
                    p("color", style="color: #0D47A1;margin-bottom: -2px;"),
                    pickerInput(inputId = "col_coords",
                                label = NULL,
                                choices = colors_solid$val,
                                choicesOpt = list(content = colors_solid$img)))
    )
  })


  icon("fas fa-circle")
  output$interp_control<-renderUI({
    column(12,
           column(3,uiOutput("maps_inputs")),
           column(3, uiOutput('interp_inputs')),

           if(input$choices_map=="variable"){ plotOutput("map_data_interp")},

           if(input$choices_map=="factor"){
             fluidRow(
               column(10, plotOutput("map_fac_interp"), style = "margin-left: -20px"),
               column(2, style = "margin-left: -30px;margin-right: -30px;",
                      plotOutput("maplegend", width = "200%"))
             )
           }
    )
  })
  output$discrete_control<-renderUI({
    fluidRow(

      if(input$choices_map=="variable"){ plotOutput("map_data_disc")},
      if(input$choices_map=="factor"){fluidRow(
        column(12, plotOutput("map_fac_disc"), style = "margin-left: -20px")

      )}
    )
  })
  output$interp_inputs<-renderUI({
    if(input$maptype=='interpolation'){
      if(input$choices_map=="variable"){
        numericInput(
          "idp", strong("idp", actionLink(
            "idphelp", icon("fas fa-question-circle")
          )), 4, min = 0
        )
      } else  {
        numericInput(
          "nmax", strong("nmax", actionLink(
            "nmaxhelp", icon("fas fa-question-circle")
          )), 4, min = 0
        )
      }}
  })
  output$maps_inputs<-renderUI({
    fluidRow(
      numericInput(
        "res_map", strong(
          "resolution",
          popify(
            icon("fas fa-question-circle"),
            title = NULL,
            textres(),
            placement = "bottom",
            trigger = "hover"
          )
        ), 40000, min = 1
      )
    )
  })




  scalesize_size<-reactive({
    if("colored"%in%input$scale_map_values){F} else{input$scalesize_size}
  })
  scalesize_color<-reactive({
    if("colored"%in%input$scale_map_values){F} else{input$scalesize_color}
  })

  output$map_data_disc <- renderPlot({
    data <- getdata_map()


    map_discrete_variable(
      data = data,
      coords = attr(data,"coords"),
      base_shape = attr(data,"base_shape"),
      layer_shape = attr(data,"layer_shape"),
      get = input$var_map,
      main = input$var_map,
      factors=show_labcoords(),
      showcoords=input$showcoords,
      cex.pt = input$pt_points+6,
      cex.coords=input$pt_coords+1,
      cex.fac=input$pt_factor+2,
      col.fac = input$col_factor,
      cex.axes=input$pt_legend+12,
      cex.lab=input$pt_legend+14,
      leg=input$map_legend,
      col.coords=input$col_coords,
      col.palette=input$pt_palette,
      symbol=as.numeric(input$pt_symbol),
      scalesize_size= input$scalesize_size,
      scalesize_color=scalesize_color(),
      points=T, input$pt_factor+6,
      as_factor=F,
      bmu=F,
      colored_by_factor=  colored_by_factor(),
      showguides=input$showguides)
  })
  output$map_data_interp <- renderPlot({
    data <- getdata_map()


    map_interp_variables(
      data = data,
      coords = saved_coords$df[rownames(data), ],
      base_shape = saved_base_shape$df,
      layer_shape = saved_layer_shape$df,
      get = input$var_map,
      main = input$var_map,
      factors=show_labcoords(),
      showcoords=input$showcoords,
      cex.coords=input$pt_coords+1,
      cex.fac=input$pt_factor+2,
      col.fac = input$col_factor,
      cex.axes=input$pt_legend+12,
      cex.lab=input$pt_legend+14,
      leg=input$map_legend,
      col.coords=input$col_coords,
      col.palette=input$pt_palette,
      as_factor=F,
      bmu=F,
      res=input$res_map,
      k=input$nmax,
      idp=input$idp,
      showguides=input$showguides)
  })
  output$map_fac_disc<-renderPlot({
    col=input$pt_palette
    data <- attr(getdata_map(),"factors")
    coords <- attr(getdata_map(),"coords")[rownames(data),]
    map_discrete_variable(
      data = data,
      coords = coords,
      base_shape =  attr(getdata_map(),"base_shape"),
      layer_shape =  attr(getdata_map(),"layer_shape"),
      get = as.character(input$map_hc),
      main = as.character(input$map_hc),
      factors=show_labcoords(),
      showcoords=input$showcoords,
      cex.pt = input$pt_points+6,
      cex.coords=input$pt_coords+1,
      cex.fac=input$pt_factor+2,
      col.fac = input$col_factor,
      cex.axes=input$pt_legend+12,
      cex.lab=input$pt_legend+14,
      leg=input$map_legend,
      col.coords=input$col_coords,
      col.palette=col,
      symbol=as.numeric(input$pt_symbol),
      scalesize_size= F,
      scalesize_color=F,
      points=T, input$pt_factor+6,
      as_factor=F,
      bmu=F,
      colored_by_factor=attr(getdata_map(),"factors")[as.character(input$map_hc)],
      showguides=input$showguides)

  })
  show_labcoords<-reactive({

    if(isFALSE(input$showfactors)){res=NULL} else {res<-attr(getdata_map(),"factors")[,as.character(input$labels_coords)]}

    res
  })

  output$map_fac_interp<-renderPlot({
    data <-    attr(getdata_map(),"factors")
    if(is.null(show_labcoords()))
    {col_labs="gray"} else{   col_labs<-input$col_factor}

    map_interp_variables(
      data = data,
      coords = saved_coords$df[rownames(data), ],
      base_shape = saved_base_shape$df,
      layer_shape = saved_layer_shape$df,
      get = as.character(input$map_hc),
      main = as.character(input$map_hc),
      factors=show_labcoords(),
      showcoords=input$showcoords,
      cex.fac=input$pt_factor+2,
      col.fac = col_labs,
      cex.coords=input$pt_coords+1,

      cex.axes=input$pt_legend+12,
      cex.lab=input$pt_legend+14,
      leg=input$map_legend,
      col.coords=input$col_coords,
      col.palette=input$pt_palette,
      as_factor=T,
      bmu=F,
      res=input$res_map,
      k=input$nmax,
      idp=input$idp,
      showguides=input$showguides)
  })



  colored_by_factor<-reactive({
    if("colored"%in%input$scale_map_values){
      attr(getdata_map(),"factors")[as.character(input$map_lab)]

    } else {NULL}})









  observeEvent(input$trainRF,
               {
                 runRF()
               })
  runRF <- reactive({
    envi <- getdata_rf02()
    sup =get_supervisor()

    if (input$rf_type == 'Classification') {
      sup[1] <- as.factor(sup[, 1])
    }

    join <- na.omit(cbind(sup, envi[rownames(sup), ]))
    envi <- join[-1]
    somC <- join[1]
    withProgress(message = "Running rf the time taken will depend on the random forest tuning",
                 min = 1,
                 max = 1,
                 {
                   RF = wrapRF(
                     envi,
                     data.frame(somC),
                     supervisor = "clusters",
                     prev.idw = F,
                     seed = input$seedrf,
                     ntree = input$ntree,
                     preProcess = input$preProcess,
                     trainControl.args = list(
                       method = input$res_method,
                       number = input$cvrf,
                       repeats = input$repeatsrf,
                       p=input$pleaverf/100
                     )
                   )
                   RF
                 })

  })



  mindeaphrf <- eventReactive(input$go_rfplot, {
    withProgress(message = "Running ...",
                 min = 1,
                 max = 1,
                 {
                   res <- multipimp(runRF())
                 })
    res
  })

  prf.reactive <- reactive({
    mean_scale<-if(isFALSE(input$mean_scale)){F}else{T}
    if(length(input$depth_min_no_of_trees)>0)
    {res<-prf(mindeaphrf(), sigs = sigrf(),
             input$sigprf,
             size_plot=input$labrfsize,
             min_no_of_trees = input$depth_min_no_of_trees,
             mean_sample = input$depth_mean_sample
    )} else{
      res<-prf(mindeaphrf(), sigs = sigrf())
         }
    rf_sigs$df<-attr(res,"sigs")
    res
  })

  output$rf_table <- renderTable({
    runRF()$results
  })
  output$rfsummary <- renderPrint({
    gettrain(runRF())
  })
  output$prf <- renderPlot({
    prf.reactive()
  })




  output$CM <- renderPlot({
    plotCM(runRF(), input$rfpalette)
  })

  topo.reactive <- reactive({
    topology(getdata_som(), dist = input$distmethod)
  })

  observeEvent(input$xdim, {
    if (length(   names(saved_data$df)) > 0) {
      dim = topo.reactive()
      xdim <- dim[[2]]
      if (input$xdim != xdim) {
        updateCheckboxInput(session, "sugtopo", NULL, FALSE)
      } else{
        updateCheckboxInput(session, "sugtopo", NULL, TRUE)
      }

    }
  })

  observeEvent(input$ydim, {
    if (length(   names(saved_data$df)) > 0) {
      dim = topo.reactive()
      ydim <- dim[[3]]
      if (input$ydim != ydim) {
        updateCheckboxInput(session, "sugtopo", NULL, FALSE)
      } else {
        updateCheckboxInput(session, "sugtopo", NULL, TRUE)
      }


    }
  })


  output$pcorr_tools<-renderUI({
    column(12,align="right",
           fluidRow(
             popify(bsButton("tools_editpcorr", icon("fas fa-quidditch"),style  = "button_active", type="toggle",disabled=F, value=F),NULL,"Graphical options",
                    options=list(container="body"))
           )
    )
  })
  output$pclus_tools<-renderUI({
    column(12,align="right",
           fluidRow(
             popify(bsButton("tools_editpclus", icon("fas fa-quidditch"),style  = "button_active", type="toggle",disabled=F, value=F),NULL,"Graphical options",
                    options=list(container="body"))
           )
    )
  })

output$pop_pcorr<-renderUI({
  column(12,style="border-top: 3px solid SeaGreen;border-bottom: 3px solid SeaGreen;background: white; margin-top: 0px; margin-right: 40px; margin-bottom: 20px",
         column(12,
                splitLayout(
                  column(12,
                    p(strong("Background", style="color: #0D47A1;")),
                    tipify(pickerInput(inputId = "pcor_bgpalette",
                                label = NULL,
                                choices = df$val,
                                choicesOpt = list(content = df$img),
                                selected=colors_solid$val[12],
                                options=list(container="body"))
                           ,"Color of grid units",options=list(container="body"))
                  ),
                  radioButtons(
                    "dot_label", "Display:", choices = c("symbols", "factors")),
                  tipify(numericInput("pcor_symbol_size",strong("size"),value = 1,min = 0.1,max = 3,step = .1),"symbol size",options=list(container="body")),
                  tipify(numericInput("cexvar",strong("var size"),value = 1,min = 0.1,max = 3,step = .1),"variable text size (only for the variable factor map)",options=list(container="body")),

                  conditionalPanel("input.dot_label == 'symbols'",{
                    column(12,
                      p(strong("Shape", style="color: #0D47A1;")),
                      tipify(pickerInput(inputId = "pcor_symbol",
                                  label = NULL,
                                  choices = df_symbol$val,
                                  choicesOpt = list(content = df_symbol$img),
                                  options=list(container="body"))
                             ,"symbol shape",options=list(container="body"))
                    )
                  }),
                  column(12,
                    p(strong( "symb color", style="color: #0D47A1;")),
                    tipify(pickerInput(inputId = "pcor_facpalette",
                                label =NULL,
                                choices = df$val,
                                choicesOpt = list(content = df$img),
                                selected=colors_solid$val[2],
                                options=list(container="body")),
                           "Symbol colors. Choose a gradient to use a factor as classification",options=list(container="body")
                           )
                  )
                ),
                column(12,
                       splitLayout(cellWidths = c("40%",'60%'),
                         uiOutput("pcor_fac_control"),


                         uiOutput('pcorr_legcontrol')
                       ))

                )
         )

})

output$pop_pclus<-renderUI({
  column(12,style="border-top: 3px solid SeaGreen;border-bottom: 3px solid SeaGreen;background: white; margin-top: 0px; margin-right: 40px; margin-bottom: 20px",
         column(12,
                splitLayout(
                  radioButtons(
                    "dot_label_clus", "Display:", choices = c("symbols", "factors")),
                  tipify(numericInput("pclus_symbol_size",strong("size"),value = 1,min = 0.1,max = 3,step = .1),"symbol size",options=list(container="body")),
                  conditionalPanel("input.dot_label_clus == 'symbols'",{
                    column(12,
                           p(strong("Shape", style="color: #0D47A1;")),
                           tipify(pickerInput(inputId = "pclus_symbol",
                                              label = NULL,
                                              choices = df_symbol$val,
                                              choicesOpt = list(content = df_symbol$img),
                                              options=list(container="body"))
                                  ,"symbol shape",options=list(container="body"))
                    )
                  }),
                  column(12,
                         p(strong( "symb color", style="color: #0D47A1;")),
                         tipify(pickerInput(inputId = "pclus_facpalette",
                                            label =NULL,
                                            choices = df$val,
                                            choicesOpt = list(content = df$img),
                                            selected=colors_solid$val[2],
                                            options=list(container="body")),
                                "Symbol colors. Choose a gradient to use a factor as classification",options=list(container="body")
                         )
                  )
                ),
                column(12,
                       splitLayout(cellWidths = c("40%",'60%'),
                                   uiOutput("pclus_fac_control"),


                                   uiOutput('pclus_legcontrol')
                       ))

         )
  )

})

output$pcorr_legcontrol<-renderUI({
  req(as.character(input$pcor_facpalette)%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat"))
  {legend
    splitLayout(
      tipify(numericInput("insertx_pcorr","leg x",value=0,step=0.05),"legend position relative to the x location",options=list(container="body")),
      tipify(numericInput("inserty_pcorr","leg y",value=0.4,step=0.05),"legend position relative to the y location",options=list(container="body")),
      tipify(numericInput("ncol_pcorr","ncol leg",value=1,step=1),"the number of columns in which to set the legend items",options=list(container="body")),
      tipify(numericInput("bgleg_pcorr","bg leg",value=0.85,step=0.05, max=1),"Legend background transparency",options=list(container="body"))

    )
  }
})

output$pclus_legcontrol<-renderUI({
  req(as.character(input$pclus_facpalette)%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat"))
  {legend
    splitLayout(
      tipify(numericInput("insertx_pclus","leg x",value=0,step=0.05),"legend position relative to the x location",options=list(container="body")),
      tipify(numericInput("inserty_pclus","leg y",value=0.4,step=0.05),"legend position relative to the y location",options=list(container="body")),
      tipify(numericInput("ncol_pclus","ncol leg",value=1,step=1),"the number of columns in which to set the legend items",options=list(container="body")),
      tipify(numericInput("bgleg_pclus","bg leg",value=0.85,step=0.05, max=1),"Legend background transparency",options=list(container="body"))

    )
  }
})

  output$pcorr_control <- renderUI({
    column(12,
      fluidRow(
        splitLayout(cellWidths = c("40%","60%"),
          column(12,style="margin: 20px 20px 20px 20px",
                 checkboxInput("varfacmap_action", strong("show variable factor map",actionLink("varfacmap", tipify(
                   icon("fas fa-question-circle"), "Click for more details"
                 ))),value =T)




          ),
          conditionalPanel("input.varfacmap_action % 2", {
            column(12,style="margin-top: 30px",
                   splitLayout(cellWidths = c("60%","20%","20%"),
                     selectInput("var_corr_codes","show",
                                 choices = c("most important correlations", "clockwise-correlations"), selectize = F
                     ),
                     numericInput("npic", 'number', value = 10, min = 2),
                     column(12,style="margin-top: 22px",
                       popify(bsButton("down_pcorr_results", icon("fas fa-download"),style  = "button_active"),NULL,"download variable factor results",
                              options=list(container="body")

                       )
                     )

                   )
            )
          })
        )

      ),
      uiOutput("pcorr_tools"),
      fluidRow(
        conditionalPanel("input.tools_editpcorr % 2",{
          uiOutput("pop_pcorr")
        })

      ),
      column(12,
             p(strong(h4("Best matching units"))),
             plotOutput('pCorrCodes'))





    )


  })

  observeEvent(input$down_pcorr_results,{
    downcenter_hand$df<-"pcorr"
    showModal(downcenter())

  })




  output$pclus_control <- renderUI({
    column(12,
           uiOutput("pclus_tools"),
           fluidRow(
             conditionalPanel("input.tools_editpclus % 2",{
               uiOutput("pop_pclus")
             })

           ),
           column(12,
                  p(strong(h4("Best matching units"))),
                  column(12,uiOutput('pclusCodes')))





    )


  })
  output$pcor_fac_control<-renderUI({
    if(input$dot_label == 'factors'|input$pcor_facpalette %in% c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat"))
      selectInput("pcor_factors","Factor",
                  choices = c(colnames(attr(getdata_som(),"factors"))),
                  selectize = F)
  })



  output$pclus_fac_control<-renderUI({
    if(input$dot_label_clus == 'factors'|input$pclus_facpalette %in% c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat"))
      selectInput("pclus_factors","Factor",
                  choices = c(colnames(attr(getdata_hc(),"factors"))),
                  selectize = F)

  })

  BMUs <- reactive({
    if (input$varfacmap_action %% 2) {
      npic=as.numeric(input$npic)} else{
        npic = 0
      }
    if(length(input$pcor_symbol)>0){
      p <-codes_corr_plot(
        m=getsom(),
        npic = npic,
        indicate = var_corr_codes.reactive(),
        pch = as.numeric(input$pcor_symbol),
        labels.ind = pcor_factors(),
        cex =as.numeric(input$pcor_symbol_size),
        bg_palette = as.character(input$pcor_bgpalette),
        factor.pal=as.character(input$pcor_facpalette),
        points=bmu_points(),
        cex.var=input$cexvar,
        ncol=input$ncol_pcorr,
        insetx=input$insertx_pcorr,
        insety=input$inserty_pcorr,
        alpha.legend=input$bgleg_pcorr
      )
    } else{
      p<-codes_corr_plot(getsom(), npic,indicate = var_corr_codes.reactive(),)
    }

    p



  })


  output$pCorrCodes <- renderPlot({
    p<-BMUs()
    pcoor_results$df<-data.frame(attr(p,"result"))
    p
  })
  output$pchanges <- renderPlot({
    pchanges(getsom())
  })
  output$pcounts <- renderPlot({
    pcounts(getsom())
  })
  output$pUmatrix <- renderPlot({
    pUmatrix(getsom())
  })

  output$pproperty <- renderPlot({
    pproperty(getsom(),
              input$variable_pproperty,
              input$variable_pproperty)
  })


  bmu_symbol.reac <- reactive({
    if (input$topo == "hexagonal") {
      16
    } else if (input$topo == 'rectangular') {
      15
    }
  })

  center.reative <- reactive({
    switch(input$scale,
           "center" = T)

  })











  output$editpca<-renderUI({
    column(12,style="border-top: 3px solid SeaGreen;border-bottom: 3px solid SeaGreen;background: white; margin-top: -30px; margin-right: 40px; margin-bottom: 20px",

           splitLayout(cellWidths = c('58%','42%'),
                       column(12,
                              checkboxInput(
                                'show_symbols',
                                strong("Symbol", style="color: #0D47A1;"),T),
                              uiOutput("symbol_control")
                       ),
                       column(12,
                              checkboxInput(
                                'show_labels',
                                strong("Labels", style="color: #0D47A1;"),F),
                              splitLayout(style="margin-left: 5px;margin-top: -10px",
                                          uiOutput("controlfactor"),
                                          uiOutput("textsize"),
                                          uiOutput("textcolor")
                              ))
           )


    )
  })


  {


    output$pop_editplot<-renderUI({
      column(12,

             conditionalPanel("input.stats0=='stats_pca'|input.stats0=='stats_mds'",{
               uiOutput("editpca")
             }),
             conditionalPanel("input.stats0=='stats_box'",{
               uiOutput("editbox")

             })

      )
    })


    output$symbol_control<-renderUI({
      req(isTRUE(input$show_symbols))
      splitLayout(style="margin-top: -10px",
                  uiOutput("pt_symbol0"),
                  uiOutput("symbolsize"),
                  uiOutput("palette_mds"),
                  uiOutput("fac_palette_mds")
      )
    })
    output$pt_symbol0<-renderUI({
      popify( pickerInput(inputId = "pt_symbol0",
                          label = NULL,
                          choices = df_symbol$val,
                          options=list(container="body"),
                          choicesOpt = list(content = df_symbol$img)), NULL, "symbol shape",options=list(container="body"))
    })
    output$symbolsize<-renderUI({
      popify(numericInput("cexpoint",NULL,value = 1,min = 0.1,max = 3,step = .1), NULL, "symbol size",options=list(container="body"))
    })
    output$palette_mds<-renderUI({
      popify(pickerInput(inputId = "colpalette",
                         label = NULL,
                         choices = df$val,
                         choicesOpt = list(content = df$img),
                         options=list(container="body"),
                         selected=colors_solid$val[1]
      ), NULL, "symbol palette",options=list(container="body"))
    })
    output$fac_palette_mds<-renderUI({
      req(input$colpalette%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat"))
      column(12,
        popify(selectInput("symbol_factor",
                           NULL,
                           choices = c(colnames(attr(getdata_upload(),"factors"))),
                           selectize = F), NULL, "symbol classification factor",options=list(container="body"))
      )

    })
    output$textsize <- renderUI({
      req(isTRUE(input$show_labels))
      popify(numericInput("cextext",NULL,value = 1,min = 0.1,max = 3,step = .1), NULL, "label text size",options=list(container="body"))


    })
    output$textcolor <- renderUI({
      req(isTRUE(input$show_labels))

      popify(pickerInput(inputId = "textcolor",
                         label = NULL,
                         choices = colors_solid$val,
                         options=list(container="body"),
                         choicesOpt = list(content = colors_solid$img), selected=colors_solid$val[1]), NULL, "label color",options=list(container="body"))


    })
    controlfactor <- reactive({
      if (isTRUE(input$show_labels))

        popify(selectInput("factor",
                           NULL,
                           choices = c(colnames(attr(getdata_upload(),"factors"))),
                           selectize = F), NULL, "label classification factor",options=list(container="body"))


    })
    output$controlfactor <- renderUI({
      controlfactor()
    })

  }




  output$factorsplot<-renderPlot({
    pfac(attr(getdata_upload(),"factors")[rownames(getdata_upload()),])
  })




  output$stats_data <- renderUI({
    column(
      12, style = "background: white;",
      fluidRow(

        column(12,
               h5(strong(
                 "numeric variables:"
               ))),
        column(6, verbatimTextOutput("psummary"))

      )
    )
  })
  output$stats_var<-renderUI({

    data=getdata_upload()
    factors<-data[,unlist(lapply(data,is.factor))]
    numerics<-data[,unlist(lapply(data,is.numeric))]
    fluidRow(
      column(12,plotOutput("summ_num", height = ncol(numerics)*15, width = 550

      ))
    )



  })
  output$stats_fac <- renderUI({
    column(12,
           column(12,

                  h5(strong("Factors:"), input$labels$name),
                  h5(strong("Structure:")),
                  verbatimTextOutput('strlabels'),


           ),
           column(12,plotOutput("factorsplot")))
  })

  output$psummary <- renderPrint({
    psummary(getdata_upload())
  })
  output$datalist4 <- renderUI({
    datalist4()
  })




  output$summ_num<-renderPlot({
    data=getdata_upload()
    numerics<-data[,unlist(lapply(data,is.numeric))]
    str_numerics(numerics)
  })

  output$summ_fac<-renderPlot({
    data=getdata_upload()
    factors<-data[,unlist(lapply(data,is.factor))]
    str_factors(factors)

  })

  xy_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  }




  mds.reactive <- reactive({
    req (input$distance %in%c("bray","euclidean","jaccard"))
    withProgress(message = "Running ...",
                 min = 1,
                 max = 1,
                 {
                   mds_data <- metaMDS (getdata_upload(), distance = input$distance)
                 })
  })


  symbol_factor <- reactive({
    if(input$colpalette%in%c("matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1","Blues","heat")){

      data = getdata_upload()
      attr(data,"factors")[rownames(data), input$symbol_factor]

    }else{NULL}
  })
  text_factor <- reactive({
    if(isFALSE(input$show_labels)){NULL} else{
      data = getdata_upload()
      attr(data,"factors")[rownames(data), input$factor]}

  })
  pt_symbol0<-reactive({
    if(isFALSE(input$show_symbols)){NA}else{as.numeric(input$pt_symbol0)}
  })



  bagpca<-reactiveValues(df=0)
  bagpca_once<-reactiveValues(df=0)
  output$mdscustom <- renderPlot({

    validate(need(input$distance!='', "Select a distance measure for the mds"))


    mds_data = mds.reactive()
    if (exists("mds_data")){
      if(length(input$show_symbols)>0)
      {
        res<- pmds(
          mds_data = mds_data,
          key = symbol_factor(),
          points =input$show_symbols,
          text = input$show_labels,
          palette = input$colpalette,
          cex.points = input$cexpoint,
          cex.text = input$cextext,
          pch=pt_symbol0(),
          textcolor=input$textcolor,
          keytext=text_factor()
        )
      } else {
        res<-pmds(mds_data)
      }
    }

    res



  })




  output$stats_ppca <- renderUI({
    validate(need(!anyNA(getdata_upload()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))
    validate(need(length(saved_data$df)>0,"no data found"))
    res<-list(
      column(12,strong("Principal Component Analysis")),
      column(12,renderPlot({
        if(length(input$show_symbols)>0){
          res<-ppca(
            getdata_upload(),
            key = symbol_factor(),
            points = input$show_symbols,
            text = input$show_labels,
            palette = input$colpalette,
            cex.points = input$cexpoint,
            cex.text = input$cextext,
            pch=pt_symbol0(),
            textcolor=input$textcolor,
            keytext=text_factor()
          )
        }else{
          res<-ppca(getdata_upload())
        }



        res
      })),

      downloadButton('down_ppca', 'plot')
    )

  })
  bagmds<-reactiveValues(df=0)
  bagmds_once<-reactiveValues(df=0)

  output$distance_mds<-renderUI({
    selectInput("distance",NULL,choices = c("Choose one" = "", c('bray', "euclidean", 'jaccard')),selectize = F)
  })
  output$stats_pmds <- renderUI({
    validate(need(!anyNA(getdata_upload()), "This functionality does not support missing values; Please use the transformation tool to the handle missing values."))

    res<-list(
      column(12,strong("Nonmetric Multidimensional Scaling ")),
      column(12,strong("distance measure")),
      column(12,
             uiOutput("distance_mds")),
      column(12,plotOutput("mdscustom")),



      downloadButton('down_pmds', 'plot')
    )

    res
  })


  #introhelpmodal
  {
    introhelpmodal <- function() {
      modalDialog(
        div(textsom(), textref())
        ,
        title = "Self-Organizing Maps",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$introhelp, {
      showModal(introhelpmodal())
    })


  }

  #screeplothelpmodal
  {
    screeplothelpmodal <- function() {
      modalDialog(
        div(textscreeplot())
        ,
        title = "Scree plot",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$screeplothelp, {
      showModal(screeplothelpmodal())
    })


  }


  #voteshelpmodal
  {
    voteshelpmodal <- function() {
      modalDialog(
        div(textvotes())
        ,
        title = "Votes on the best number of clusters",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$voteshelp, {
      showModal(voteshelpmodal())
    })


    output$voteshelp <- renderText({
      if (input$votesh %% 2) {
        paste0(
          br(),
          p(
            icon("fas fa-exclamation-circle"),
            code("K"),
            "is passed to the",
            code("max.nc"),
            "argument"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            code("min.nc"),
            "argument is fixed to 2"
          ),
          p(
            icon("fas fa-exclamation-circle"),
            code('index'),
            "argument fixed to:",
            code(
              "kl",
              "ch",
              "hartigan",
              "ccc",
              "scott",
              "marriot",
              "trcovw",
              "tracew",
              "friedman",
              "rubin",
              "cindex",
              "db",
              "silhouette",
              "duda",
              "pseudot2",
              "beale",
              "ratkowsky",
              "ball",
              "ptbiserial",
              "gap",
              "frey",
              "mcclain",
              "gamma",
              "gplus",
              "tau",
              "dunn",
              "sdindex",
              "dindex",
              "sdbw"
            ),
            ". Only indexes running safely are computed."
          ),
          p(
            icon("fas fa-exclamation-circle"),
            "The remaining arguments are fixed to their default values;"
          ),
          h4("NbClust {NbClust}"),
          getHelp('NbClust')
        )
      }
    })
  }



  #hchelpmodal
  {
    hchelpmodal <- function() {
      modalDialog(
        div(textclustering())
        ,
        title = "Hierarchical Clustering",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$hchelp, {
      showModal(hchelpmodal())
    })




  }

  #removemodal
  {
    removemodal <- function() {
      modalDialog(
        textremove(),
        title = "Remove",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }
    observeEvent(input$transfhelp, {
      showModal(transfmodal())
    })

    transfmodal <- function() {
      modalDialog(
        texttransf(),
        title = "Transformation",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$Remove, {
      showModal(removemodal())
    })
  }

  #somgridmodal
  {
  preProcessmodal <- function() {
      modalDialog(
        textpreProcess(),
        title = "preProcess",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$preProcesshelp, {
      showModal(preProcessmodal())
    })

    output$preProcesshelp <- renderText({
      if (input$preProcessh %% 2) {
        paste0(br(),
               h4("preProcess {caret}"),
               getHelp(help(preProcess, "caret")))
      }
    })


  }
  #somgridmodal
  {
    somgridmodal <- function() {
      modalDialog(
        textsomgrid(),
        title = h4(strong("somgrid")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$somgridhelp, {
      showModal(somgridmodal())
    })

    output$somgridhelp <- renderText({
      if (input$somgridh %% 2) {
        paste0(br(),
               h4("somgrid {kohonen}"),
               getHelp('unit.distances'))
      } else if (input$kohonen %% 2) {
        getHelp('kohonen')
      }
    })


  }
  #supersommodal
  {
    supersommodal <- function() {
      modalDialog(
        textsupersom(),
        title = h4(strong("Supersom")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$supersomhelp, {
      showModal(supersommodal())
    })

    output$supersomhelp <- renderText({
      if (input$supersomh %% 2) {
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
      } else if (input$kohonen %% 2) {
        getHelp('kohonen')
      }
    })


  }
  #varfacmapmodal
  {
    varfacmapmodal <- function() {
      modalDialog(
        textvarfacmap(),
        title = h4(strong("Variable factor map")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$varfacmap, {
      showModal(varfacmapmodal())
    })




  }
  #Propertymodal

  #scalehelpmodal
  {
    scalehelpmodal <- function() {
      modalDialog(
        textscale(),
        title =  h4(strong("Scale")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$scalehelp, {
      showModal(scalehelpmodal())
    })

    output$scalehelp_out <- renderText({
      if (input$scalehelph %% 2) {
        paste0(
          br(),
          h4("scale {base}"),
          p(
            icon("fas fa-exclamation-circle"),
            code("scale"),
            "argument",
            " fixed to",
            code("TRUE")
          ),
          getHelp(help(scale, "base"))

        )
      }
    })


  }
  #interpmodal
  {
    interpmodal <- function() {
      modalDialog(
        textinterp(),
        title =  h4(strong("Interpolation")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$interphelp, {
      showModal(interpmodal())
    })

    output$interphelp <- renderText({
      if (input$interph %% 2) {
        paste0(
          br(),
          p(
            icon("fas fa-exclamation-circle"),
            "only the argument ",
            code("idp"),
            " is available to be passed to ",
            code("idw"),
            " function. The the remaining arguments are fixed to their default values;"
          ),
          h4("idw {gstat}"),
          getHelp('krige')
        )
      }
    })


  }
  #hclustmodal
  {
    hclustmodal <- function() {
      modalDialog(
        texthclust(),
        title = h4(strong("Hierarchical Clustering")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$hclusthelp, {
      showModal(hclustmodal())
    })

    output$hclusthelp <- renderText({
      if (input$hclusth %% 2) {
        paste0(
          br(),
          p(
            icon("fas fa-exclamation-circle"),
            "Argument ",
            code("members"),
            " is fixed to =",
            code("NULL")
          ),

          h4("hclust {stats}"),
          getHelp('hclust')
        )
      }
    })


  }


  #rfmodal
  {
    rfmodal <- function() {
      modalDialog(
        textrf(),
        title = h4(strong("Random Forest")),
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$rfhelp, {
      showModal(rfmodal())
    })
    observeEvent(input$rfh,{
      output$rfhelp<-renderText({
        paste0(
          br(),
          h4("train {caret}"),
          getHelp(help(train, "caret"))
        )
      })
    })
    observeEvent(input$rfh2,{
      output$rfhelp<-renderText({
        paste0(
          br(),
          h4("trainControl {caret}"),
          getHelp(help(trainControl, "caret"))
        )
      })
    })

    {
      ctreemodal <- function() {
        modalDialog(
          textctree(),
          title = h4(strong("Decision Tree")),
          footer = modalButton("close"),
          size = "m",
          easyClose = TRUE
        )
      }


      observeEvent(input$ctreehelp, {
        showModal(ctreemodal())
      })
      observeEvent(input$ctreeh,{
        output$ctreehelp<-renderText({
          paste0(
            br(),
            h4("ctree {party}"),
            getHelp(help(ctree, "party"))
          )
        })
      })
      observeEvent(input$ctreeh2,{
        output$ctreehelp<-renderText({
          paste0(
            br(),
            h4("ctree_control {party}"),
            getHelp(help(ctree_control, "party"))
          )
        })
      })


    }





  }




  {
    pcahelpmodal <- function() {
      modalDialog(
        textpcahelp(),
        title = "pcahelp",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$pcahelp, {
      showModal(pcahelpmodal())
    })
    output$pcahelphelp <- renderText({
      if (input$pcahelph %% 2) {
        paste0(
          br(),
          h4("prcomp  {base}"),
          p(
            icon("fas fa-exclamation-circle"),
            "Default for the S3 method. The 'center' and 'scale' arguments are set in the panel",code("3.1 Tranformation"),"from the dashboard."
          ),

          getHelp('prcomp')
        )
      }
    })

  }

  #mdshelpmodal
  {
    mdshelpmodal <- function() {
      modalDialog(
        textmdshelp(),
        title = "mdshelp",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$mdshelp, {
      showModal(mdshelpmodal())
    })

    output$mdshelphelp <- renderText({
      if (input$mdshelph %% 2) {
        paste0(
          br(),
          h4("Multidimensional Scaling  {vegan}"),
          p(
            icon("fas fa-exclamation-circle"),
            "Only",
            code("distance"),
            "argument is passed to",
            code("metaMDS"),
            "function from 'vegan'",
            " . The remaining arguments are fixed to their default values;'"
          ),

          getHelp('metaMDS')
        )
      }
    })


  }
  #sugtopohelpmodal
  {
    sugtopohelpmodal <- function() {
      modalDialog(
        textsugtopohelp(),
        title = "Suggested topology",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$sugtopohelp, {
      showModal(sugtopohelpmodal())
    })



  }
  #rfbphelpmodal
  {
    rfbphelpmodal <- function() {
      modalDialog(
        textrfbphelp(),
        title = "Split moving window",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$rfbphelp, {
      showModal(rfbphelpmodal())
    })




  }

  #bphelpmodal
  {
    bphelpmodal <- function() {
      modalDialog(
        textbphelp(),
        title = "Split moving window",
        footer = modalButton("close"),
        size = "m",
        easyClose = TRUE
      )
    }

    observeEvent(input$bphelp, {
      showModal(bphelpmodal())
    })




  }



  output$decostand <- renderText({
    if (input$decostand %% 2) {
      paste0(
        br(),
        h4("decostand {vegan}"),

        p(
          icon("fas fa-exclamation-circle"),
          "Only",
          code("method"),
          "argument is passed to",
          code("decostand"),
          "The remaining arguments are fixed to their default values;"
        ),
        getHelp('decostand')
      )


    } else if (input$vegan %% 2) {
      paste0(br(),
             h4(h4("vegan {package}")),
             getHelp('vegan'))
    }
  })
  {
    basemodal <- function() {
      modalDialog(
        textbase(),
        title = "base shape",
        footer = column(12,actionButton("base_back","back"),modalButton("close")),
        size = "l",
        easyClose = TRUE
      )
    }

    observeEvent(input$basehelp, {
      showModal(basemodal())
    })

    output$basehelp <- renderText({
      if (input$baseh %% 2) {
        paste0(br(),
               h4("st_read {sf}"),
               getHelp('st_read'))
      }
    })


  }
  {
    layermodal <- function() {
      modalDialog(
        textlayer(),
        title = "layer shape",
        footer =column(12,actionButton("layer_back","back"),modalButton("close")),
        size = "l",
        easyClose = TRUE
      )
    }

    observeEvent(input$layerhelp, {
      showModal(layermodal())
    })

    output$layerhelp <- renderText({
      if (input$layerh %% 2) {
        paste0(br(),
               h4("st_read {sf}"),
               getHelp('st_read'))
      }
    })


  }
  output$codechunk_base <- renderPrint({
    codebase()
  })
  output$codechunk_layer <- renderPrint({
    codelayer()
  })


  output$strlabels <- renderPrint({
    str(attr(getdata_upload(),"factors")[rownames(getdata_upload()),])
  })




  som.reactive <- eventReactive(input$trainSOM,{

    withProgress(message = "Running som... the time taken will depend on the size of the data and the  training.",
                 min = 1,
                 max = 1,
                 {
                   if (!is.na(input$seed)) {

                     set.seed(input$seed)

                   }


                   m <-
                     try(supersom(
                       as.matrix(getdata_som()),
                       grid = somgrid(
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
                       maxNA.fraction = input$maxna
                     ))



                   #updateTabItems(session, "tabs", "training")
                   #updateTabsetPanel(session, "som_tab", "results")
                   #updateTabsetPanel(session, "som_tab", "results")
                   if (class(m) != "kohonen")
                   {
                     validate(paste(m[[1]], "Please decrease your alpha (learning rate)"))
                   }
                   attr(m,'mode')<-input$mode

                   m

                 })

  })


  out_train<-reactiveValues(df=0)





  pcor_factors <- reactive({
    if (length(input$pcor_factors)>0) {
      attr(getdata_som(),"factors")[rownames(getdata_som()), input$pcor_factors]
    } else{NULL}
  })
  var_corr_codes.reactive <- reactive({
    if(input$var_corr_codes=="most important correlations"){res="var"}
    if(input$var_corr_codes=="clockwise-correlations"){res="cor"}
 res
  })



  output$processed_data <- {
    downloadHandler(
      filename = function() {
        paste("processed_data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(getdata_som(), file, sep = ";")
      }
    )
  }
  output$down_changes <- {
    downloadHandler(
      filename = function() {
        paste("pchanges", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 1500,
            height = 1500)
        pchanges(getsom())
        dev.off()
      }
    )
  }
  output$down_pcounts <- {
    downloadHandler(
      filename = function() {
        paste("pcounts", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 1500,
            height = 1500)
        pcounts(getsom())
        dev.off()
      }
    )
  }
  output$down_pUmatrix <- {
    downloadHandler(
      filename = function() {
        paste("pUmatrix", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 1500,
            height = 1500)
        pUmatrix(getsom())
        dev.off()
      }
    )
  }
  output$down_BMUs <- {
    downloadHandler(
      filename = function() {
        paste("codescorr", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 2000)



        if (input$varfacmap_action %% 2) {
          npic=as.numeric(input$npic)} else{
            npic = 0
          }
        if(length(input$pcor_symbol)>0){
          p <-codes_corr_plot(
            m=getsom(),
            npic = npic,
            indicate = var_corr_codes.reactive(),
            pch = as.numeric(input$pcor_symbol),
            labels.ind = pcor_factors(),
            cex =as.numeric(input$pcor_symbol_size),
            bg_palette = as.character(input$pcor_bgpalette),
            factor.pal=as.character(input$pcor_facpalette),
            points=bmu_points(),
            cex.var=input$cexvar,
            ncol=input$ncol_pcorr,
            insetx=input$insertx_pcorr,
            insety=input$inserty_pcorr,
            alpha.legend=input$bgleg_pcorr
          )
        } else{
          p<-codes_corr_plot(getsom(), npic,indicate = var_corr_codes.reactive(),)
        }
        p




        dev.off()
      }
    )
  }

  output$down_pproperty <- {
    downloadHandler(
      filename = function() {
        paste("pproperty", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 2000)


        pproperty(getsom(),
                  input$variable_pproperty,
                  input$variable_pproperty)


        dev.off()
      }
    )
  }


  output$down_pmds <- {
    downloadHandler(
      filename = function() {
        paste("mds", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 2000)

        mds_data = mds.reactive()
        if (exists("mds_data"))
          pmds(
            mds_data = mds_data,
            key = symbol_factor(),
            points = input$show_symbols,
            text = input$show_labels,
            colpalette = input$colpalette,
            cex.points = input$cexpoint,
            cex.text = input$cextext
          )
        dev.off()
      }
    )
  }
  rf2 <- reactiveValues()

  output$down_pca <- {
    downloadHandler(
      filename = function() {
        paste("pca", Sys.Date(), ".RData",sep = "")
      },
      content = function(file) {
        res<-ppca(
          getdata_upload(),
          key = symbol_factor(),
          points = input$show_symbols,
          text = input$show_labels,
          palette = input$colpalette,
          cex.points = input$cexpoint,
          cex.text = input$cextext,
          pch=pt_symbol0(),
          textcolor=input$textcolor,
          keytext=text_factor()
        )

        saveRDS(res,file=file)

      }
    )
  }
  output$down_ppca <- {
    downloadHandler(
      filename = function() {
        paste("pca", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 2000)

        ppca(
          getdata_upload(),
          key = symbol_factor(),
          points = input$show_symbols,
          text = input$show_labels,
          palette = input$colpalette,
          cex.points = input$cexpoint,
          cex.text = input$cextext,
          pch=pt_symbol0(),
          textcolor=input$textcolor,
          keytext=text_factor()
        )

        dev.off()
      }
    )
  }
  output$down_hcdata <- {
    downloadHandler(
      filename = function() {
        paste("hc_data_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 3000,
            height = 2000)
        plot(cutdata(getdata_hc(), input$method.hc0),
             labels = as.character(labhc()))
        dev.off()
      }
    )
  }
  output$down_hcmodel <- {
    downloadHandler(
      filename = function() {
        paste("hc_model_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 3000,
            height = 2000)

        plot(cutm(getmodel_hc(), input$method.hc0))




        dev.off()
      }
    )
  }

  output$down_data_WSS_results <- {
    downloadHandler(
      filename = function() {
        paste("resuls_WSS_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.table(x=data.frame(WSSdata()),file,append=T,
                    row.names=FALSE, sep=input$sep_data_WSS_results,
                    dec=input$dec_data_WSS_results)

      }
    )
  }
  output$down_model_WSS_results <- {
    downloadHandler(
      filename = function() {
        paste("resuls_WSS_model_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.table(x=data.frame(WSSmodel()),file,append=T,
                    row.names=FALSE, sep=input$sep_model_WSS_results,
                    dec=input$dec_model_WSS_results)

      }
    )
  }

  output$down_data_accRF_results <- {
    downloadHandler(
      filename = function() {
        paste("resuls_accRF_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.table(x=data.frame(accRFdata()$acutable),file,append=T,
                    row.names=FALSE, sep=input$sep_data_accRF_results,
                    dec=input$dec_data_accRF_results)

      }
    )
  }
  output$down_model_accRF_results <- {
    downloadHandler(
      filename = function() {
        paste("resuls_accRF_model_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.table(x=data.frame(accRFmodel()$acutable),file,append=T,
                    row.names=FALSE, sep=input$sep_model_accRF_results,
                    dec=input$dec_model_accRF_results)

      }
    )
  }






  output$down_data_WSS_plot <- {
    downloadHandler(
      filename = function() {
        paste("screeplot_WSS_data_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 1500)
        elbow_plot(WSSdata(), sugg = sugg_WSS_data())

        dev.off()
      }
    )
  }
  output$down_model_WSS_plot <- {
    downloadHandler(
      filename = function() {
        paste("screeplot_WSS_model_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 1500)
        elbow_plot(WSSmodel(), sugg = sugg_WSS_model())

        dev.off()
      }
    )
  }
  output$down_data_accRF_plot <- {
    downloadHandler(
      filename = function() {
        paste("screeplot_accRF_data_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 1500)

        plot_accuclus(accRFdata(), sugg = sugg_accRF_data())


        dev.off()
      }
    )
  }
  output$down_model_accRF_plot <- {
    downloadHandler(
      filename = function() {
        paste("screeplot_accRF_model_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 1500)

        plot_accuclus(accRFmodel(), sugg = sugg_accRF_model())


        dev.off()
      }
    )
  }
  output$down_votes <- {
    downloadHandler(
      filename = function() {
        paste("votes_nclusters_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 2000,
            height = 1500)
        barplot(
          res[[1]],
          las = 1,
          main = paste("optimal number of clusters", res[[2]]),
          ylab = "Frequency among all indices",
          xlab = "Number of Clusters "
        )

        dev.off()
      }
    )
  }


  output$down_phc <- {
    downloadHandler(
      filename = function() {
        paste("hcut_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file,
            res = 300,
            width = 3000,
            height = 2000)
        hc_plot(phc())
        dev.off()
      }
    )
  }


  output$error_som<-renderUI({
    m<-getsom()
    errors<-errors_som(m)
    column(12,
      strong("Quality Measures:"),
      p(em("err.quant:"),pophelp("Quantization error","Average squared distance between the data points and the map prototypes to which they are mapped. Lower is better.")),
      p(em("err.varratio:"),pophelp("Percentage of explained variance","Similar to other clustering methods, the share of total variance that is explained by the clustering (equal to 1 minus the ratio of quantization error to total variance). Higher is better.")),
      p(em("err.topo:"),pophelp("Topographic error","Measures how well the topographic structure of the data is preserved on the map. It is computed as the share of observations for which the best-matching node is not a neighbor of the second-best matching node on the map. Lower is better: 0 indicates excellent topographic representation (all best and second-best matching nodes are neighbors), 1 is the maximum error (best and second-best nodes are never neighbors).")),
      p(em("err.kaski"),pophelp("Kaski-Lagus error","Combines aspects of the quantization and topographic error. It is the sum of the mean distance between points and their best-matching prototypes, and of the mean geodesic distance (pairwise prototype distances following the SOM grid) between the points and their second-best matching prototype.")),
      p(em("neu.uti"), pophelp("Neuron Utilization","The percentage of neurons that are not BMU of any observation"))

    )
  })

  train.summary <- reactive({
    m <- getsom()
    traindata <- data.frame(m$data[[1]])
    mean = round(mean(unlist(traindata)), 2)
    n.obs = nrow(traindata)
    n.variables = ncol(traindata)
    summ <- m$grid[-1]
    summ$neighbourhood.fct <- as.character(summ$neighbourhood.fct)
    summ <- do.call(rbind, summ)
    mode <- attr(m,"mode")
    alpha = paste0(m$alpha, collapse = "; ")
    radius = paste0(round(m$radius, 3), collapse = "; ")
    user.weights = m$user.weights
    maxNA.fraction = m$maxNA.fraction
    dist.fcts = m$dist.fcts

    Parameters <-
      rbind(
        errors_som(m),

        n.obs,
        n.variables,
        summ,
        alpha,
        radius,
        user.weights,
        maxNA.fraction,
        dist.fcts,
        mode
      )
colnames(Parameters)<-"Training Parameters"


    data.frame(Parameters)

  })
  output$train.summary <- renderTable({
    train.summary()
  }, rownames = T)
  output$data.summary <- renderTable({
    data <- getdata_upload()
    psummary(data)

  })



  output$stats_phist <- renderPlot({
    phist(getdata_upload())



  }, height = 300)



  smw_labinfo <- reactive({
    strong(
      "split moving window",
      tipify(
        actionLink("bphelp", icon("fas fa-question-circle")),
        "Performs split moving window to detect significant discontinuities in the relationship between the number of clusters and WSS values. Click for more information."
      )
    )
  })
  accrf_labinfo <- reactive({
    strong(
      "split moving window",
      tipify(
        actionLink("bphelp", icon("fas fa-question-circle")),
        "Performs split moving window to detect significant discontinuities in the relationship between the number of clusters and accRF values. Click for more information."
      )
    )
  })


  smw_data_WSS <- reactive({
    conditionalPanel("output.plot_data_WSS",

                     div(
                       column(
                         5,
                         style = "margin-top: 5px;",
                         checkboxInput("smw_data_WSS", value = T, smw_labinfo())
                       ),
                       column(
                         3,
                         tipify(strong("w"), "window size"),
                         numericInput(
                           "w_data_WSS",
                           NULL,
                           value = 6,
                           min = 2,
                           step = 2
                         )
                       )
                     ))
  })
  smw_model_WSS <- reactive({
    conditionalPanel("output.plot_model_WSS", {
      div(
        column(
          5,
          style = "margin-top: 5px;",
          checkboxInput("smw_model_WSS", value = T, smw_labinfo())
        ),
        column(
          3,
          tipify(strong("w"), "window size"),
          numericInput(
            "w_model_WSS",
            NULL,
            value = 6,
            min = 2,
            step = 2
          )
        )
      )
    })

  })


  smw_data_accRF <- reactive({
    conditionalPanel("output.plot_data_accRF", {
      div(
        column(
          5,
          style = "margin-top: 5px;",
          checkboxInput("smw_data_accRF", value = T,
                        accrf_labinfo())
        ),
        column(
          3,
          tipify(strong("w"), "window size"),
          numericInput(
            "w_data_accRF",
            NULL,
            value = 6,
            min = 2,
            step = 2
          )
        )
      )
    })


  })
  smw_model_accRF <- reactive({
    conditionalPanel("output.plot_model_accRF", {
      div(
        column(
          5,
          style = "margin-top: 5px;",
          checkboxInput("smw_model_accRF", value = T, accrf_labinfo())
        ),
        column(
          3,
          tipify(strong("w"), "window size"),
          numericInput(
            "w_model_accRF",
            NULL,
            value = 6,
            min = 2,
            step = 2
          )
        )
      )
    })
  })


  output$data_WSS <- renderUI({
    column(
      12,
      column(2, uiOutput("K_WSS_data")),
      column(
        2,
        actionButton("getWSSdata", "run", style = "margin-top: 20px;")
      ),

      smw_data_WSS(),
      column(12, uiOutput('plot_data_WSS'))


    )
  })
  output$model_WSS <- renderUI({
    column(
      12,
      column(2,
             uiOutput("K_WSS_model")),
      column(
        2,
        actionButton("getWSSmodel", "run", style = "margin-top: 20px;")
      ),

      column(12, smw_model_WSS()),
      column(12, uiOutput('plot_model_WSS'))

    )


  })
  output$data_accRF <- renderUI({
    column(
      12,
      column(3, uiOutput("input_accRF_data")),
      column(2, uiOutput("K_accRF_data")),
      column(
        2,
        actionButton("getaccRFdata", "run", style = "margin-top: 20px;")
      ),
      smw_data_accRF(),
      column(12, uiOutput('plot_data_accRF'))

    )
  })
  output$model_accRF <- renderUI({
    column(
      12,
      column(3, uiOutput("input_accRF_model")),
      column(2, uiOutput("K_accRF_model")),
      column(
        2,
        actionButton("getaccRFmodel", "run", style = "margin-top: 20px;")
      ),
      smw_model_accRF(),
      column(12, uiOutput('plot_model_accRF'))


    )
  })




  screeplot_control <- reactive({
    column(
      12,
      style = "margin-left: 10px; margin-top: 10px",
      div(strong(
        "Scree plot", actionLink('screeplothelp', icon("fas fa-question-circle"))
      ), style = "margin-bottom: 5px;"),
      radioButtons(
        "screeplot_type",
        NULL,
        choices = c("WSS", "accRF"),
        inline = T
      ),
      uiOutput("screeplot_control2")
    )

  })

  output$screeplot_control2 <- renderUI({
    fluidRow(
      conditionalPanel(
        "input.model_or_data=='data' & input.screeplot_type=='WSS'",
        {
          uiOutput("data_WSS")
        }
      ),
      conditionalPanel(
        "input.model_or_data=='som codebook' & input.screeplot_type=='WSS'",
        {
          uiOutput("model_WSS")
        }
      ),
      conditionalPanel(
        "input.model_or_data=='data' & input.screeplot_type=='accRF'",
        {
          uiOutput("data_accRF")
        }
      ),
      conditionalPanel(
        "input.model_or_data=='som codebook' & input.screeplot_type=='accRF'",
        {
          uiOutput("model_accRF")
        }
      )

    )
  })





  WSSdata <- eventReactive(input$getWSSdata, {
    withProgress(message = "Running elbow analysis... this may take a while....",
                 min = 1,
                 max = 1,
                 {
                   getelbow(
                     getdata_hc(),
                     k = input$K_WSS_data,
                     type = "data",
                     method = input$method.hc0,
                     dist = input$disthc
                   )
                 })

  })
  WSSmodel <- eventReactive(input$getWSSmodel, {
    withProgress(message = "Running elbow analysis... this may take a while....",
                 min = 1,
                 max = 1,
                 {
                   getelbow(
                     getmodel_hc(),
                     k = input$K_WSS_model,
                     type = "som",
                     method = input$method.hc0,
                     dist = input$disthc
                   )
                 })

  })


  suggsKmodel <- reactive({
    selectInput(
      "suggsKmodel",
      strong("suggested K"),
      selectize = F,
      choices = c(
        paste(
          saved_kmodelWSS$df,
          attr(saved_kmodelWSS$df, "suggmethod")
        ),
        paste(
          saved_kmodelaccRF$df,
          attr(saved_kmodelaccRF$df, "suggmethod")
        ),
        paste(
          saved_kmodelvotes$df,
          attr(saved_kmodelvotes$df, "suggmethod")
        )
      )
    )
  })


  output$suggsKmodel <- renderUI({
    suggsKmodel()
  })


  picK <- reactive({
    if (input$usesuggK == "suggested")
    {
      if (length(input$suggsKmodel) > 0 | length(input$suggsKdata) > 0) {
        if (input$model_or_data == "data") {
          as.numeric(gsub("([0-9]+).*$", "\\1", input$suggsKdata))
        } else if (input$model_or_data == "som codebook") {
          as.numeric(gsub("([0-9]+).*$", "\\1", input$suggsKmodel))
        }

      }
    } else  if (input$usesuggK == "user-defined") {
      if (input$model_or_data == "data") {
        as.numeric(input$customKdata)
      } else if (input$model_or_data == "som codebook") {
        as.numeric(input$customKmodel)
      }



    }

  })

  suggsKdata <- reactive({
    selectInput(
      "suggsKdata",
      strong("suggested K"),
      selectize = F,
      choices = c(
        paste(saved_kdataWSS$df, attr(saved_kdataWSS$df, "suggmethod")),
        paste(
          saved_kdataaccRF$df,
          attr(saved_kdataaccRF$df, "suggmethod")
        ),
        paste(
          saved_kdatavotes$df,
          attr(saved_kdatavotes$df, "suggmethod")
        )
      )
    )
  })



  observeEvent(input$customK,
               saved_kcustom$df <- isolate(input$customK))

  output$suggsKdata <- renderUI({
    suggsKdata()
  })


  df_hc<-df


  output$customKdata <- renderUI({
    numericInput("customKdata",
                 "Number of clusters",
                 value = 2,
                 step = 1)


  })

  output$customKmodel <- renderUI({
    numericInput("customKmodel",
                 "Number of clusters",
                 value = 2,
                 step = 1)
  })






  usesuggK <- reactive({
    if (length(saved_kdataWSS$df) > 0 |
        length(saved_kmodelWSS$df) > 0 | length(saved_kmodelvotes$df) > 0) {
      radioButtons("usesuggK",
                   "Cluster type",
                   c("user-defined", "suggested"),
                   selected = "suggested")
    } else {
      radioButtons("usesuggK",
                   "Cluster type",
                   c("user-defined"),
                   selected = "user-defined")
    }



  })
  output$usesuggK <- renderUI({
    usesuggK()
  })




  output$suggestedK <- renderUI({
    column(
      12,
      splitLayout(
        column(12,uiOutput('usesuggK')),

        fluidRow(
          conditionalPanel("input.model_or_data=='data'",{
            splitLayout(
              column(12,
                     conditionalPanel("input.usesuggK=='user-defined'",{
                       uiOutput("customKdata")
                     }),

                     conditionalPanel(
                       "input.usesuggK=='suggested'",
                       uiOutput("suggsKdata")
                     )
              )

            )

          }),

          conditionalPanel(
            "input.model_or_data=='som codebook'",  {
              splitLayout(
                column(12,
                       conditionalPanel("input.usesuggK=='user-defined'",{
                         uiOutput("customKmodel")
                       }),
                       conditionalPanel(
                         "input.usesuggK=='suggested'",
                         uiOutput("suggsKmodel")
                       )
                ),
                column(12,
                       p(strong("Palette", style="color: #0D47A1")),
                       pickerInput(inputId = "hcmodel_palette",
                                   label = NULL,
                                   choices = df_hc$val,
                                   choicesOpt = list(content = df_hc$img),
                                   options=list(container="body")
                                   ))
              )

            }
          )
        ),cellWidths = c("40%","60%"),
        column(12,
               p(strong("Palette", style="color: #0D47A1")),
               pickerInput(inputId = "hcdata_palette",
                           label = NULL,
                           choices = df_hc$val,
                           choicesOpt = list(content = df_hc$img)))

      )


    )
  })


  sugg_accRF_data <- reactive({
    if (input$smw_data_accRF %% 2)
    {
      rfs <- accRFdata()
      res <- rfs$acutable
      bp = smw_bp(res, ws = input$w_data_accRF)
      attr(bp, "suggmethod") <- c("(accRF)")
      saved_kdataaccRF$df <- isolate(bp)
      bp
    } else {
      NULL
    }
  })
  sugg_accRF_model <- reactive({
    if (input$smw_model_accRF %% 2)
    {
      rfs <- accRFmodel()
      res <- rfs$acutable
      bp <- smw_bp(res, ws = input$w_model_accRF)
      attr(bp, "suggmethod") <- c("(accRF)")
      saved_kmodelaccRF$df <- isolate(bp)
      bp
    } else {
      NULL
    }
  })


  sugg_WSS_data <- reactive({
    if (input$smw_data_WSS %% 2)
    {
      res <- WSSdata()
      bp <- smw_bp(res, ws = input$w_data_WSS)
      attr(bp, "suggmethod") <- c("(WSS)")
      saved_kdataWSS$df <- isolate(bp)
      bp
    } else {
      NULL
    }
  })



  sugg_WSS_model <- reactive({
    if (input$smw_model_WSS %% 2)
    {
      res <- WSSmodel()
      bp <- smw_bp(res, ws = input$w_model_WSS)
      attr(bp, "suggmethod") <- c("(WSS)")
      saved_kmodelWSS$df <- isolate(bp)
      bp

    } else {
      NULL
    }
  })



  output$plot_data_WSS <- renderUI({
    validate(need(input$getWSSdata %% 2,""))
    list(renderPlot(elbow_plot(WSSdata(), sugg = sugg_WSS_data())),
         column(12,
                column(12,strong("Download")),

                column(3,
                       p(HTML('&nbsp;')),
                       downloadButton('down_data_WSS_plot', 'plot')),
                fluidRow(
                  column(3,
                         p(HTML('&nbsp;')),
                         downloadButton('down_data_WSS_results', 'results')),
                  column(2,selectInput("sep_data_WSS_results", "sep", choices=c(";",",","\t"))),
                  column(2,selectInput("dec_data_WSS_results", "dec", choices=c(".",",")))

                )

         )


    )

  })
  output$plot_model_WSS <- renderUI({
    validate(need(input$getWSSmodel %% 2,""))
    list(renderPlot(elbow_plot(WSSmodel(), sugg = sugg_WSS_model())),
         column(12,
                column(12,strong("Download")),
                column(3,
                       p(HTML('&nbsp;')),
                       downloadButton('down_model_WSS_plot', 'plot')),
                fluidRow(
                  column(3,
                         p(HTML('&nbsp;')),
                         downloadButton('down_model_WSS_results', 'results')),
                  column(2,selectInput("sep_model_WSS_results", "sep", choices=c(";",",","\t"))),
                  column(2,selectInput("dec_model_WSS_results", "dec", choices=c(".",",")))

                )
         )
    )

  })
  output$plot_data_accRF <- renderUI({
    validate(need(input$getaccRFdata %% 2,""))

    list(renderPlot(plot_accuclus(accRFdata(), sugg = sugg_accRF_data())),
         column(12,
                column(12,strong("Download")),
                column(3,
                       p(HTML('&nbsp;')),
                       downloadButton('down_data_accRF_plot', 'plot')),
                fluidRow(
                  column(3,
                         p(HTML('&nbsp;')),
                         downloadButton('down_data_accRF_results', 'results')),
                  column(2,selectInput("sep_data_accRF_results", "sep", choices=c(";",",","\t"))),
                  column(2,selectInput("dec_data_accRF_results", "dec", choices=c(".",",")))

                )
         )

    )
  })
  output$plot_model_accRF <- renderUI({
    validate(need(input$getaccRFmodel %% 2,""))
    list(renderPlot(plot_accuclus(accRFmodel(), sugg = sugg_accRF_model())),
         column(12,
                column(12,strong("Download")),
                fluidRow(
                  column(3,
                         p(HTML('&nbsp;')),
                         downloadButton('down_model_accRF_plot', 'plot')),
                  column(3,
                         p(HTML('&nbsp;')),
                         downloadButton('down_model_accRF_results', 'results')),
                  column(2,selectInput("sep_model_accRF_results", "sep", choices=c(";",",","\t"))),
                  column(2,selectInput("dec_model_accRF_results", "dec", choices=c(".",",")))

                )
         )
    )
  })




  output$input_accRF_data <- renderUI({
    fluidRow(
      tipify(
        strong("accRF data"),
        "Select the data to validate the optimum number of clusters"
      ),
      selectInput(
        "input_accRF_data",
        NULL,
        choices =    names(saved_data$df),
        selectize = F
      )
    )
  })

  output$input_accRF_model <- renderUI({
    fluidRow(
      tipify(
        strong("accRF data"),
        "Select the data to validate the optimum number of clusters"
      ),
      selectInput(
        "input_accRF_model",
        NULL,
        choices =    names(saved_data$df),
        selectize = F
      )
    )
  })


  accRFdata <- eventReactive(input$getaccRFdata, {
    get <- as.character(input$input_accRF_data)
    envi <- data.frame(saved_data$df[[get]])
    machineRF(
      getdata_hc(),
      data = envi,
      k = input$K_accRF_data,
      type = "data",
      method = input$method.hc0,
      dist = input$disthc
    )
  })
  accRFmodel <- eventReactive(input$getaccRFmodel, {
    get <- as.character(input$input_accRF_model)
    envi <- data.frame(saved_data$df[[get]])
    machineRF(
      getmodel_hc(),
      data = envi ,
      k = input$K_accRF_model,
      type = "som",
      method = input$method.hc0,
      dist = input$disthc
    )
  })




  sugg <- reactive({
    if (input$smw_elbow %% 2)
    {
      smw_bp(elbow(), ws = input$window_elbow)
    } else {
      NULL
    }
  })



  K_accRF_data <- reactive({
    fluidRow(strong("k"),
             numericInput("K_accRF_data", NULL, value = Kdata(getdata_hc())))
  })
  K_accRF_model <- reactive({
    fluidRow(strong("k"),
             numericInput("K_accRF_model", NULL, value = Kmodel(getmodel_hc())))
  })
  K_WSS_data <- reactive({
    fluidRow(strong("k"),
             numericInput("K_WSS_data", NULL, value = Kdata(getdata_hc())))
  })
  K_WSS_model <- reactive({
    fluidRow(strong("k"),
             numericInput("K_WSS_model", NULL, value = Kmodel(getmodel_hc())))
  })

  output$K_accRF_data <- renderUI({
    K_accRF_data()
  })
  output$K_accRF_model <- renderUI({
    K_accRF_model()
  })
  output$K_WSS_data <- renderUI({
    K_WSS_data()
  })
  output$K_WSS_model <- renderUI({
    K_WSS_model()
  })



  output$clustering_panel <- renderUI({
    fluidRow(column(12,
                    column(
                      12,
                      uiOutput("hc_control")
                    ),
                    br(),
                    fluidRow(style="margin-left:5px;",
                             tabsetPanel(id="hc_tab",
                                         tabPanel(strong("1. Dendrogram"),value="hc_tab1",
                                                  uiOutput("Dendogram")),

                                         tabPanel(
                                           strong("2. Clustering Tools"),
                                           value = "hc_tab2",
                                           div(style = "background: white", tabsetPanel(
                                             tabPanel(
                                               strong("Scree plot"),value="hc_tab2_1",
                                               column(12, style = "background: white",
                                                      uiOutput('screeplot_control'))
                                             )
                                           ))
                                         ),
                                         tabPanel(
                                           strong('3. Clustering results'),value="hc_tab3",
                                           uiOutput("suggestedK"),
                                           tabsetPanel(id = "hc_results",

                                                       tabPanel(
                                                         "Hcut",value ="Hcut",
                                                         column(12,  style = "background: Snow;",
                                                                uiOutput("phc"))
                                                       ))

                                         )
                             ))))
  })


  Tabs_HC <- reactive({

    if (anyNA(getdata_hc())) {
      column(
        12,
        strong(
          "The selected dataset contais missing values which are not allowed in this functionality. Please switch the dataset or go to the Upload menu for creating a new dataset without missing values.",
          style = "color: red;"
        )
      )
    } else
    {

    }





  })



  output$new_fac_hc <- renderPrint({
    attr(getdata_hc(),"factors")
  })

  output$factor_bank_hc <- renderUI({
    column(12,
           br(),
           br(),
           column(
             12,
             strong("Factor bank", style = "color: Green;"),
             br(),
             div(verbatimTextOutput("new_fac_hc"), style = "width: 600;height:200px;overflow-y: scroll; overflow-x: scroll"),
             br(),
             br()
           ),)
  })

  output$Tabs_HC <- renderUI({
    Tabs_HC()
  })

  insertbmu<-reactiveValues(df=F)
  observeEvent(input$model_or_data, {
    if (input$model_or_data == "som codebook")
    {
      if(isFALSE(insertbmu$df)){
        insertTab(
          inputId = "hc_results",
          tabPanel(
            "SOM codebook clusters",
            value = "bmu_tab",
            style = "background: Snow;",
            column(12, style = "background: Snow;",
                   uiOutput("pclus"))
          ),
          target = "Hcut",position="after",
        )
        insertbmu$df<-T
      }
    }
  })

  observeEvent(input$model_or_data,{
    req(isTRUE(insertbmu$df))
    if(input$model_or_data == "data"){
      removeTab("hc_results","bmu_tab")
      insertbmu$df<-F
    }
  })



  output$screeplot_control <- renderUI(screeplot_control())
  output$votes_control <- renderUI({
    column(
      12,
      br(),

      column(
        12,
        strong(
          "Votes on the best number of clusters",
          actionLink('voteshelp', icon("fas fa-question-circle"))
        )
      ),
      conditionalPanel("input.model_or_data=='data'", {
        column(12,
               column(2, uiOutput("K_votes_data")),
               column(2,
                      br(),
                      actionButton('getvotesdata', "run")),
               column(12,
                      uiOutput("plot_votesdata")))
      }),
      conditionalPanel("input.model_or_data=='som codebook'", {
        column(
          12,
          column(2, uiOutput("K_votes_model")),
          column(2,
                 br(),
                 actionButton('getvotesmodel', "run")),
          plotOutput("plot_votesmodel")
        )
      })

    )
  })



  votesdata <- eventReactive(input$getvotesdata, {
    data = getdata_hc()


    votes <-
      getGroups(
        m = data,
        dist = input$disthc,
        type = "data",
        group_method = "kmeans",
        n.max = input$K_votes_data
      )
    bp = votes[[2]]
    attr(bp, "suggmethod") <- c("(votes)")
    saved_kdatavotes$df <- isolate(bp)

    votes

  })

  votesmodel <- eventReactive(input$getvotesmodel, {
    data = getmodel_hc()
    votes <-
      getGroups(
        m = data,
        dist = NULL,
        type = "som",
        group_method = "kmeans",
        n.max = input$K_votes_model
      )
    bp = votes[[2]]
    attr(bp, "suggmethod") <- c("(votes)")
    saved_kmodelvotes$df <- isolate(bp)
    votes
  })



  output$K_votes_data <- renderUI({
    fluidRow(strong("k"),
             numericInput("K_votes_data", NULL, value = Kdata(getdata_hc())))
  })

  output$K_votes_model <-
    renderUI({
      fluidRow(strong("k"),
               numericInput("K_votes_model", NULL, value = Kmodel(getmodel_hc())))
    })

  output$plot_votesdata <- renderUI({
    res = votesdata()
    list(renderPlot({
      barplot(
        res[[1]],
        las = 1,
        main = paste("optimal number of clusters", res[[2]]),
        ylab = "Frequency among all indices",
        xlab = "Number of Clusters "
      )
    }),
    column(12, downloadButton('down_votes', 'Download')))

  })


  output$plot_votesmodel <- renderPlot({
    res = votesmodel()
    barplot(
      res[[1]]
      ,
      las = 1,
      main = paste("optimal number of clusters", res[[2]]),
      ylab = "Frequency among all indices",
      xlab = "Number of Clusters L"
    )
  })







  output$hc_control <- renderUI({
    hc_control()
  })


  output$data_hc<-renderUI({
    if(cur_data$df==0){cur=names(saved_data$df)[1]} else {cur=cur_data$df}
    column(12,selectInput("data_hc",
                          "Datalist::",
                          choices =    names(saved_data$df),
                          selectize = F, selected=cur))
  })

  output$choicesHC<-renderUI({

    radioButtons("model_or_data", "Clustering target", choices = choices_hc(), selected=c(choices_hc()[length(choices_hc())]))
  })
  hc_control <- reactive({
    req(length(saved_data$df)>0)
    column(12,
      splitLayout(cellWidths = c("20%","30%","25%","25%"),
        uiOutput("choicesHC"),
        uiOutput("data_hc"),
        fluidRow(
          conditionalPanel("input.model_or_data=='som codebook'",
                           {
                            column(12, uiOutput("somHC"))
                           }),

          conditionalPanel("input.model_or_data=='data'",
                          column(12, uiOutput("disthc")))
        ),

        selectInput(
          "method.hc0",
          "method.hc",
          choices = c("ward.D2", "ward.D", "single", "complete", "average"), selectize = F
        )
      )
      ,uiOutput("saveHC")

    )



  })

  output$saveHC<-renderUI({
    if(isTRUE(baghc$df)) {column(12,offset=8,
                                 absolutePanel(style="margin-top: 8px",popify(bsButton("tools_savehc", icon("fas fa-save"),style  = "button_active", type="action",value=FALSE),NULL,
                                                                              "Save clusters",options=list(container="body")
                                 )))}
  })
  output$somHC<-renderUI({

    selectInput(
      "som_hc",
      "som codebook:",
      choices = names(attr(getdata_hc(),"som")),
      selectize = F
    )
  })

  bagHC<-reactiveValues(df=0)
  observe({
    req(length(saved_data$df)>0)
    req(input$data_hc)
    bagHC$df<-length(names(attr(saved_data$df[[input$data_hc]],"som")))>0
    req(input$model_or_data)
    try({hc<-phc()
    baghc$df<-!any(unlist(lapply(attr(saved_data$df[[input$data_hc]],"factors"), function (x) identical(x,as.vector(hc$somC)))))})
  })

  observeEvent(input$tools_savehc,{
    if(input$tools_savehc %% 2) {
      datahand$df<-"Save Clusters"
      datahand2$df<-p(p(em(input$data_hc,style="color: gray"),strong("::"),em("Factor-Attribute",style="color: gray"), strong("::")))
      datahand3$df<-NULL
      showModal(
        datahand_modal()
      )
    }
  })

  output$disthc <- renderUI({
    selectInput(
      "disthc",
      "distance",
      choices = c('bray', "euclidean", 'jaccard'),
      selectize = F
    )
  })

  sc_rf_elbow <- reactive({
    fluidRow(
      conditionalPanel("input.screeplot_type=='WSS'", {
        column(12,
               uiOutput("elbowcontrol"))
      }),
      conditionalPanel("input.screeplot_type=='accRF'", {
        column(12,
               uiOutput('rfloopcontrol'))
      })





    )
  })


  labhc <- reactive({
    as.character(attr(getdata_hc(),"factors")[rownames(getdata_hc()),as.character(input$labhc)])
  })






  output$hcdata_plot <- renderPlot({
    plot(
      cutdata(getdata_hc(), input$method.hc0),
      labels = as.character(labhc()),
      main = "Dendogram"
    )
  })

  output$hcmodel_plot <- renderPlot({
    plot(cutm(getmodel_hc(), input$method.hc0), main = "Dendogram")
  })

  hc0panel <- reactive({
    column(12,
           br(),
           column(
             12,
             conditionalPanel(
               "input.model_or_data=='data'",
               uiOutput('hclabs_control')
             ),
             conditionalPanel("input.model_or_data=='data'", {
               fluidRow(column(12, plotOutput("hcdata_plot")),
                        column(12, downloadButton('down_hcdata', 'Download')))

             }),
             conditionalPanel("input.model_or_data=='som codebook'", {
               fluidRow(column(12, plotOutput("hcmodel_plot")),
                        column(12, downloadButton('down_hcmodel', 'Download')))
             }),
           ))

  })

  output$hclabs_control <-
    renderUI({
      column(6,
             selectInput(
               "labhc",
               "Labels",
               choices = c(colnames(attr(getdata_hc(),"factors"))),
               selectize = F
             ))

    })


  output$Dendogram <- renderUI({
    hc0panel()
  })
  output$dt_panel<-renderUI({
    req(length(saved_data$df)>0)
    column(12,style="background: white; margin: 20 20 20 20",
           column(12,style="margin-top: 20px",
                  splitLayout(
                    radioButtons("dt_type","Type",c("Classification","Regression")),
                    uiOutput("data_dtY"),
                    uiOutput("sup_dt"),
                    uiOutput("data_dtX")
                  )),
           column(12,uiOutput("ptree_control")),
           column(12,plotOutput("ptree"))
    )

  })
  getdata_dtY<-reactive({
    data<-saved_data$df[[input$data_dtY]]
    data
  })
  getdata_dtX<-reactive({
    data<-saved_data$df[[input$data_dtX]]
    data
  })

  output$data_dtY<-renderUI({
    selectInput("data_dtY",strong("Y Data:"),choices=names(saved_data$df), selectize = F)
  })
  output$data_dtX<-renderUI({
    if(cur_data$df==0){cur=names(saved_data$df)[1]} else {cur=cur_data$df}
    selectInput("data_dtX",strong("~ Training data (X)"),choices=names(saved_data$df), selectize = F, selected=cur)
  })
  output$sup_dt<-renderUI({
    if(input$dt_type=="Classification")
    {
      selectInput("sup_fac_dt",tipify(strong("Independent variable (Y):"),"The response vector"),choices=colnames(attr(getdata_dtY(),"factors")), selectize = F)
    } else {
      selectInput("sup_data_dt",tipify(strong("Independent variable (Y):"),"The response vector"),choices=colnames(getdata_dtY()), selectize = F)

    }
  })
  get_supdt<-reactive({
   res<- if(input$dt_type=="Classification"){
      cbind(prev=as.factor(attr(getdata_dtY(),"factors")[rownames(getdata_dtX()),input$sup_fac_dt]),getdata_dtX())
    } else
    {
        cbind(prev=getdata_dtY()[rownames(getdata_dtX()),input$sup_data_dt],getdata_dtX())
    }
   res
  })

  decision_tree <- reactive({
    rfs = get_supdt()
    if(!is.null(input$testtype))
    {ptree(
      rfs,
      teststat = input$teststat,
      testtype = input$testtype,
      mincriterion = input$mincriterion,
      minsplit = input$minsplit,
      minbucket = input$minbucket,
      nresample = input$nresample,
      maxsurrogate = input$maxsurrogate,
      mtry = input$mtry,
      maxdepth = input$maxdepth,
      palette=input$dt_palette
    )} else{
      ptree(rfs, teststat = input$teststat,palette=input$dt_palette)
    }
  })

  output$ptree <- renderPlot({
    decision_tree()

  })

  output$ptree_control <- renderUI({
    column(
      12,style="margin: 20 20 20 20",
      strong("ctree control"),
      splitLayout(cellWidths = c("20%",'10%','70%'),
        column(12,
               selectInput(
                 "teststat",
                 strong("teststat",tipify(icon("fas fa-question-circle"),"type of the test statistic to be applied.", options=list(container="body"))),
                 choices = c("quad", "max"),
                 selectize = F
               ),
               fluidRow(style="margin-right: -100px; margin-top:",
                 p(
                   p(strong("Palette", style="color: #0D47A1")),
                   pickerInput(inputId = "dt_palette",
                               label = NULL,
                               choices = df$val,
                               choicesOpt = list(content = df$img), options=list(container="body"))
                 )
               )),

        column(12,style="margin-top: 22px",popify(bsButton("fine_ptree",icon("fas fa-sliders-h"),style  = "button_active", type ="toggle"),NULL,"Fine tuning",options=list(container="body"))),
        conditionalPanel("input.fine_ptree % 2",{
          column(12,
                 splitLayout(
                   selectInput(
                     "testtype",
                     strong("testtype",tipify(icon("fas fa-question-circle"),"how to compute the distribution of the test statistic.", options=list(container="body"))),
                     choices = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
                     selectize = F
                   ),
                   numericInput("mincriterion", strong("mincriterion",tipify(icon("fas fa-question-circle"),"the value of the test statistic", options=list(container="body"))), 0.95, step=0.01),
                   numericInput("minsplit", strong("minsplit",tipify(icon("fas fa-question-circle"),"the minimum sum of weights in a node in order to be considered for splitting", options=list(container="body"))), 20, step = 1),
                   numericInput("minbucket", strong("minbucket",tipify(icon("fas fa-question-circle"),"the minimum sum of weights in a terminal node.", options=list(container="body"))), 7, step = 1)

                 ),
                 splitLayout(
                   numericInput("nresample", strong("nresample",tipify(icon("fas fa-question-circle"),"number of MonteCarlo replications to use when the distribution of the test statistic is simulated.", options=list(container="body"))), 9999, step = 1),
                   numericInput("maxsurrogate", strong("maxsurrogate",tipify(icon("fas fa-question-circle"),"number of surrogate splits to evaluate. Note the currently only surrogate splits in ordered covariables are implemented.", options=list(container="body"))), 0, step = 0.1),
                   numericInput("mtry",strong("mtry",tipify(icon("fas fa-question-circle"),"number of input variables randomly sampled as candidates at each node for random forest like algorithms. The default mtry = 0 means that no random selection takes place.", options=list(container="body"))), 0 , step = 1),
                   numericInput("maxdepth", strong("maxdepth",tipify(icon("fas fa-question-circle"),"maximum depth of the tree. The default maxdepth = 0 means that no restrictions are applied to tree sizes.", options=list(container="body"))), 0 , step = 1)
                 )

          )
        })

        )



    )
  })


  pcoor_results<-reactiveValues(df=0)


  getdown<-reactive({
    switch(downcenter_hand$df,
           "data"=saved_data$df[[input$data_upload]],
           "factors"=attr(saved_data$df[[input$data_upload]],"factors"),
           "coords"=attr(saved_data$df[[input$data_upload]],"coords"),
           "som"=combsom_down(),
           "rfdepth"=data.frame(mindeaphrf()[[2]]),
           "pcorr"=pcoor_results$df
    )
  })


  getdown_tile<-reactive({
    switch(downcenter_hand$df,
           "data"="Data-Attribute",
           "factors"="Factors-Attribute",
           "coords"="Coords-Attribute",
           "som"="Som-Attribute",
           "rfdepth"="Random-Forest Minimal depth distribution",
           "pcorr"="Variable Factor Results"


           )
  })


  outputOptions(output, "plot_data_WSS", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_model_WSS", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_data_accRF", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_model_accRF", suspendWhenHidden = FALSE)
  outputOptions(output, "pchanges", suspendWhenHidden = FALSE)

  }



enableBookmarking(store = "url")
shinyApp(ui, server)


