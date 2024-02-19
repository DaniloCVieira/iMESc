options(shiny.autoload.r=FALSE)
#' @noRd

#' @export
databank_module<-list()
#' @export
databank_module$ui<-function(id){
  ns<-NS(id)
  fluidPage(


    column(12, class = "databank_page", style = "margin-left: 5px; background: white",
           div(id = ns("bank_tools"),
                  div(id = ns("bank_tools_in"),
                      div(
                        span(
                          inline(
                            uiOutput(ns("bank_input1"))
                          ),
                          inline(
                            uiOutput(ns("tools_bank"))
                          )
                        )
                      )
                  )),
           uiOutput(ns("view_out"))
    )
    )
   }

#' @export
databank_module$server<-function(id, vals){
  moduleServer(id,function(input, output, session) {
    ns<-session$ns
    getdata_bank<-reactive({
      req(length(vals$saved_data)>0)
      req(input$data_bank)
      vals$saved_data[[input$data_bank]]
    })


    output$bank_input1<-renderUI({
      div(
        pickerInput(ns("data_bank"), NULL, choices = names(vals$saved_data), selected = vals$cur_data, width = "200px")
      )
    })
    output$tools_bank<-renderUI({
      data<-getdata_bank()

      choices<-list(
        "data", "factors", "coords",
        "extra_shape",
        if (!is.null(attr(data, "som"))) {
          "som"
        },
        if (check_model0(data, "rf")) {
          "rf"
        },
        if (check_model0(data, "nb")) {
          "nb"
        },
        if (check_model0(data, "svm")) {
          "svm"
        },
        if (check_model0(data, "knn")) {
          "knn"
        },
        if (check_model0(data, "sgboost")) {
          "sgboost"
        },
        if (check_model0(data, "xyf")) {
          "xyf"
        },
        if (!is.null(attr(data, "notes"))) {
          "notes"
        }
      )

      attributes<-list(
        div(id = ns("001"), icon("fas fa-archive")),
        div(id = ns("c002"), icon("fas fa-boxes")),
        div(id = ns("003"), icon("fas fa-map-marker-alt")),
        div(id = ns("004"), icon("far fa-map")),
        if (!is.null(attr(data, "som"))) {
          div(id = ns("006"), icon("fas fa-braille"))
        },
        if (check_model0(data, "rf")) {
          div(id = ns("007"), img(src=rf_icon2,height='17px',width='20'))
        },
        if (check_model0(data, "nb")) {
          div(id = ns("008"), img(src = nb_icon2, height = '17', width = '20'))
        },
        if (check_model0(data, "svm")) {
          div(id = ns("009"), img(src = svm_icon2, height = '17', width = '20'))
        },
        if (check_model0(data, "knn")) {
          div(id = ns("010"), img(src = knn_icon2, height = '17', width = '20'))
        },
        if (check_model0(data, "sgboost")) {
          div(id = ns("011"), img(src = sgboost_icon2, height = '17', width = '20'))
        },
        if (check_model0(data, "xyf")) {
          div(id = ns("013"), icon("fas fa-ellipsis-v"), span("~", style = "font-size: 8px; padding: 0px; margin: 0px"), icon("fas fa-braille"))
        },
        if (!is.null(attr(data, "notes"))) {
          div(id = ns("012"), icon("fas fa-comment"))
        }
      )

      attributes<-attributes[which(unlist(lapply(attributes, function(x) !is.null(x))))]
      choices<-choices[which(unlist(lapply(choices, function(x) !is.null(x))))]

      if (is.null(vals$curview_databank)) {
        vals$curview_databank<-"data"
      }

      div(
        inline(
          radioGroupButtons(
            ns("view_datalist"), NULL,
            choiceNames = attributes,
            choiceValues = choices, status = "view_datalist", selected = vals$curview_databank
          )
        ),
        bsPopover(ns("001"), NULL, paste0(span("Numeric-Attribute", style = "color: red"))),
        bsPopover(ns("c002"), NULL, "Factor-Attribute"),
        bsPopover(ns("003"), NULL, "Coords-Attribute"),
        bsPopover(ns("004"), NULL, "Shapes-Attribute"),
        bsPopover(ns("006"), NULL, "Som-Attribute"),
        bsPopover(ns("007"), NULL, "RF-Attribute"),
        bsPopover(ns("008"), NULL, "NB-Attribute"),
        bsPopover(ns("009"), NULL, "SVM-Attribute"),
        bsPopover(ns("010"), NULL, "KNN-Attribute"),
        bsPopover(ns("011"), NULL, "GBM-Attribute"),
        bsPopover(ns("013"), NULL, "xyf-Attribute"),
        bsPopover(ns("012"), NULL, "Comments")
      )
    })
    output$viewdata<-renderUI({
      div(
        div(class = "datalist_tools",
            h5(strong("Numeric-Attribute"),
               inline(
                 div(
                   id = 'ddcogs',
                   div(class = "ddcogs0", span(icon("fas fa-cog"), style = "padding-left: 6px")),
                   div(class = 'ddlinks_all',
                       div(class = "ddlinks",
                           actionLink(ns("ddcogs1"), "Rename Variables")
                       ),

                       div(class = "ddlinks",
                           actionLink(ns("ddcogs2"), "Download table")
                       )
                   )
                 )
               )
            )),
        column(12, style = " background: white;",
               uiOutput(ns("data_attr"))
        )
      )
    })
    output$viewshapes<-renderUI({
      choices<-layers_choices()
      column(12, style = "margin-top: 10px",
             span(
               inline(
                 pickerInput(ns("pick_elayers"), "Shapes:",  choices, width = "200px")
               ),
               actionButton(ns("delete_elayer_shape"), icon("fas fa-trash-alt"))
             ),
             uiOutput(ns('viewbase')),
             uiOutput(ns('viewlayer')),
             uiOutput(ns("elayers"))
      )
    })
    output$ddcogs3<-renderUI({
      div(class = "ddlinks",
          actionLink(ns("ddcogs3"), "Edit changes")
      )
    })
    output$viewsom<-renderUI({
      req(length(attr(getdata_bank(), "som")) > 0)
      div(
        splitLayout(
          cellWidths = c("30%", "50%"),
          h5(
            span(strong("Som-Attribute"),
                 tipify(actionButton(ns("downcenter_som"), icon("fas fa-download")), "Download table")
            ),
            popify(bsButton(ns("trash_som"), icon("fas fa-edit"), style = "button_active"), NULL, "Select and remove a som")
          )
        ),
        uiOutput(ns("supersom_summary"))
      )
    })

    output$supersom_summary<-renderUI({
      mlist<-attr(getdata_bank(), "som")
      res_som<-lapply(names(mlist),function(i){
        m=mlist[[i]]
        supersom_summaries2(m, i)
      })


      do.call(splitLayout, res_som)
    })
    output$view_out<-renderUI({
      req(input$view_datalist)
      column(12, id = ns("view_out"),
             div(style = "margin-top: 20px;background:white",
                 switch(input$view_datalist,
                        'data' = uiOutput(ns("viewdata")),
                        'factors' = uiOutput(ns("viewfactors")),
                        'coords' = uiOutput(ns("viewcoords")),
                        'extra_shape' = uiOutput(ns("viewshapes")),
                        'som' = uiOutput(ns("viewsom")),
                        'rf' = uiOutput(ns("viewrf")),
                        'nb' = uiOutput(ns("viewnb")),
                        'svm' = uiOutput(ns("viewsvm")),
                        'knn' = uiOutput(ns("viewknn")),
                        'sgboost' = uiOutput(ns("viewsgboost")),
                        'xyf' = uiOutput(ns("viewxyf")),
                        'notes' = uiOutput(ns("comments"))
                 )
             )
      )
    })
    output$comments<-renderUI({
      div(
        h4(strong("Comments"),
           inline(uiOutput(ns('comments_edit'))),
           inline(uiOutput(ns('comments_remove'))),
           inline(uiOutput(ns('comments_down')))
        ),
        div(uiOutput(ns('comment_data')))
      )
    })
    output$comments_down<-renderUI({
      downloadButton(ns('comments_downs'), label = NULL)
    })
    output$comments_remove<-renderUI({
      div(
        popify(bsButton(ns('comments_remove'), icon("fas fa-trash"), style = "button_active"), NULL, "Delete comment")
      )
    })
    output$comments_edit<-renderUI({
      actionButton(ns('comments_edit'), icon("fas fa-edit"))
    })
    output$viewfactors<-renderUI({

      div(
        div(class = "datalist_tools", h5(
          span(strong("Factor-Attribute")),
          actionButton(ns("dfcogs2"), tipify(icon("fas fa-download"), "Download table")),
          # tipify(actionButton("replace_factors",icon(verify_fa = FALSE,name=NULL,class="fas fa-file-upload")),"Replace Factor-Attribute")
        )),

        column(12, style = " background: white;",
               uiOutput(ns("factor_attr"))
        )

      )

    })
    output$viewcoords<-renderUI({

      if (is.null(attr(getdata_bank(), "coords"))) {
        fluidRow(

          column(12,
                 p(strong("No coords found in Datalist:", style = "color: red"), em(input$data_bank))),
          uiOutput(ns("add_coords_intru")),
          column(12, style = "margin-top: 20px",
                 splitLayout(
                   cellWidths = c("30%", "20%"),
                   column(12,
                          fileInput(inputId = ns("add_coords_file"), label = NULL)),
                   column(12,
                          uiOutput(ns("add_coords_button"))
                   )

                 ))

        )
      } else {
        div(
          h5(id = ns("databank_coords"), strong("Coords-Attribute"), tipify(actionButton(ns("downcenter_coords"), icon("fas fa-download")), "Download table"),
             actionButton(ns("delete_coords"), icon("fas fa-trash-alt"))),
          tags$style('#DTcoords td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
          tags$style('#DTcoords th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
          inline(
            DT::dataTableOutput(ns("DTcoords"))
          )
        )
      }

    })


    output$viewbase<-renderUI({
      req(input$pick_elayers)
      req(input$pick_elayers == "Base Shape")

      column(12,
             # renderPlot(bankShpfile()),
             if (is.null(attr(getdata_bank(), "base_shape"))) {
               fluidRow(

                 column(12,
                        p(strong("No base_shape found in Datalist:", style = "color: red"), em(input$data_bank))),
                 uiOutput(ns("add_base_intru")),
                 column(12, style = "margin-top: 20px",
                        splitLayout(
                          cellWidths = c("30%", "20%"),
                          column(12,
                                 fileInput(inputId = ns("add_base_file"), label = NULL)),
                          column(12,
                                 uiOutput(ns("add_base_button"))
                          )

                        ))
               )
             } else {
               renderPlot({
                 base_shape<-attr(getdata_bank(), "base_shape")
                 ggplot(st_as_sf(base_shape)) + geom_sf() +
                   theme(panel.background = element_rect(fill = "white"),
                         panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))
               })
             }

      )
    })
    output$viewlayer<-renderUI({

      req(input$pick_elayers)
      req(input$pick_elayers == "Layer Shape")
      div(
        if (is.null(attr(getdata_bank(), "layer_shape"))) {
          fluidRow(

            column(12,
                   p(strong("No layer_shape found in Datalist:", style = "color: red"), em(input$data_bank))),
            uiOutput(ns("add_layer_intru")),
            column(12, style = "margin-top: 20px",
                   splitLayout(
                     cellWidths = c("30%", "20%"),
                     column(12,
                            fileInput(inputId = ns("add_layer_file"), label = NULL)),
                     column(12,
                            uiOutput(ns("add_layer_button"))
                     )

                   ))

          )
        } else {
          fluidRow(

            renderPlot({
              layer_shape<-attr(getdata_bank(), "layer_shape")
              ggplot(st_as_sf(layer_shape)) + geom_sf() +
                theme(panel.background = element_rect(fill = "white"),
                      panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))

            })
          )
        }


      )
    })
    output$factor_attr<-renderUI({
      validate(need(ncol(attr(getdata_bank(), "factors")) < 1000, "Preview not available for data with more than 1000 columns"))
      div(
        DT::dataTableOutput(ns("DT_factors"))
      )
      # screenshot(scale=3, timer=2)

    })
    output$comments_downs<-{
      downloadHandler(
        filename = function() {
          paste(input$data_bank, "_comment", Sys.Date(), ".txt", sep = "")

        },
        content = function(file) {
          res<-attr(getdata_bank(), "notes")
          res<-gsub("<br/>", "\n", res)
          writeLines(res, file, sep = '\n')
          beep(10)
        }
      )
    }
    output$comment_data<-renderUI({
      HTML(attr(getdata_bank(), "notes"))
    })
    output$data_attr<-renderUI({
      validate(need(ncol(getdata_bank()) < 1000, "Preview not available for data with more than 1000 columns"))
      div(
        DT::dataTableOutput(ns("DT_data"))
      )
      # screenshot(scale=3, timer=2)

    })
    output$knn_remove<-renderUI({
      div(
        popify(bsButton(ns("trash_knn"), icon("fas fa-edit"), style = "button_active"), NULL, "Select and remove a knn")

      )
    })
    output$viewknn<-renderUI({
      knn_regs<-getglobal_knn()$regs
      knn_class<-getglobal_knn()$class

      div(
        h4(strong("knn-Attribute"), inline(uiOutput(ns('knn_remove')))),
        numericInput(ns("knn_round"), "Round table:", value = 3, step = 1, min = 1, width = "100px"),
        if (length(knn_class) > 0) {
          column(12, style = "background: white",

                 div(span(
                   strong("Classification models"), inline(
                     div(actionButton(ns("down_knntable_class"), icon("fas fa-download")))
                   )
                 )),
                 tags$style('#knntab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#knntab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput(ns('knntab_class')))

          )
        },
        hr(),
        if (length(knn_regs) > 0) {
          column(12, style = "background: white",
                 div(span(
                   strong("Regression models"), inline(
                     div(actionButton(ns("down_knntable_reg"), icon("fas fa-download")))
                   )
                 )),
                 tags$style('#knntab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#knntab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput(ns('knntab_regs')))
          )
        }
      )
    })
    output$sgboost_remove<-renderUI({
      div(
        popify(bsButton(ns("trash_sgboost"), icon("fas fa-edit"), style = "button_active"), NULL, "Select and remove a GBM model")
      )
    })
    output$viewsgboost<-renderUI({
      sgboost_regs<-getglobal_sgboost()$regs
      sgboost_class<-getglobal_sgboost()$class

      div(
        h4(strong("GBM-Attribute"), inline(uiOutput(ns('sgboost_remove')))),
        numericInput(ns("sgboost_round"), "Round table:", value = 3, step = 1, min = 1, width = "100px"),
        if (length(sgboost_class) > 0) {
          column(12, style = "background: white",

                 div(span(
                   strong("Classification models"), inline(
                     div(actionButton(ns("down_sgboosttable_class"), icon("fas fa-download")))
                   )
                 )),
                 tags$style('#sgboosttab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#sgboosttab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput(ns('sgboosttab_class')))

          )
        },
        hr(),
        if (length(sgboost_regs) > 0) {
          column(12, style = "background: white",
                 div(span(
                   strong("Regression models"), inline(
                     div(actionButton(ns("down_sgboosttable_reg"), icon("fas fa-download")))
                   )
                 )),
                 tags$style('#sgboosttab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#sgboosttab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput(ns('sgboosttab_regs')))
          )
        }
      )
    })
    output$xyf_remove<-renderUI({
      div(
        popify(bsButton(ns("trash_xyf"), icon("fas fa-edit"), style = "button_active"), NULL, "Select and remove a xyf")
      )
    })
    output$viewxyf<-renderUI({
      xyf_regs<-getglobal_xyf()$regs
      xyf_class<-getglobal_xyf()$class

      div(
        h4(strong("xyf-Attribute"), inline(uiOutput(ns('xyf_remove')))),
        numericInput(ns("xyf_round"), "Round table:", value = 3, step = 1, min = 1, width = "100px"),
        if (length(xyf_class) > 0) {
          column(12, style = "background: white",

                 div(span(
                   strong("Classification models"), inline(
                     div(actionButton(ns("down_xyftable_class"), icon("fas fa-download")))
                   )
                 )),
                 tags$style('#xyftab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#xyftab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput(ns('xyftab_class')))

          )
        },
        hr(),
        if (length(xyf_regs) > 0) {
          column(12, style = "background: white",
                 div(span(
                   strong("Regression models"), inline(
                     div(actionButton(ns("down_xyftable_reg"), icon("fas fa-download")))
                   )
                 )),
                 tags$style('#xyftab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#xyftab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput(ns('xyftab_regs')))
          )
        }
      )
    })
    output$ddcogs1_pick<-renderUI({
      pickerInput(ns("ddcogs1_pick"), div("Select a variable"), choices = colnames(getdata_bank()), selected = vals$ddcogs1_pick, width = "200px")
    })
    output$dfcogs1_pick<-renderUI({
      pickerInput(ns("dfcogs1_pick"), div("Select a factor"), choices = colnames(attr(getdata_bank(), "factors")), selected = vals$dfcogs1_pick, width = "200px")
    })
    output$ddcogs3_pick<-renderUI({
      span(strong("Datalist:"), em(input$data_bank, style = 'color: SeaGreen'))
    })
    output$ddcogs3_edit<-renderUI({
      pickerInput(ns("ddcogs3_edit"), div("Edit Changes:"), colnames(attr(getdata_bank(), "transf")), width = "200px", selected = vals$ddcogs3_edit)
    })
    output$ddcogs1_targ<-renderUI({
      input$ddcogs1_pick
    })
    output$elayers<-renderUI({
      req(input$pick_elayers)
      div(

        # renderPrint(attr(getdata_bank(),"extra_shape")),
        renderPlot({
          shape <-
            attr(getdata_bank(), "extra_shape")[[input$pick_elayers]]
          req(!is.null(shape))
          ggplot(st_as_sf(shape)) + geom_sf() +
            theme(panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))



        })
      )
    })
    output$rf_remove<-renderUI({
      div(
        popify(bsButton(ns("trash_rf"), icon("fas fa-edit"), style = "button_active"), NULL, "Select and remove a rf")

      )
    })
    output$nb_remove<-renderUI({
      div(
        popify(bsButton(ns("trash_nb"), icon("fas fa-edit"), style = "button_active"), NULL, "Select and remove a nb")
      )
    })
    output$svm_remove<-renderUI({
      div(
        popify(bsButton(ns("trash_svm"), icon("fas fa-edit"), style = "button_active"), NULL, "Select and remove a svm")
      )
    })
    output$viewnb<-renderUI({
      div(
        h4(strong("NB-Attribute"), inline(uiOutput(ns('nb_remove')))),
        numericInput(ns("nb_round"), "Round table:", value = 3, step = 1, min = 1, width = "100px"),
        column(12, style = "background: white",

               div(span(
                 strong("Naive Bayes models"), inline(
                   div(actionButton(ns("down_nbtable_class"), icon("fas fa-download")))
                 )
               )),
               tags$style('#nbtab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               tags$style('#nbtab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
               inline(DT::dataTableOutput(ns('nbtab_class')))

        )

      )
    })
    output$viewrf<-renderUI({
      rf_regs<-getglobal_rf()$regs
      rf_class<-getglobal_rf()$class

      div(
        h4(strong("RF-Attribute"), inline(uiOutput(ns('rf_remove')))),
        numericInput(ns("rf_round"), "Round table:", value = 3, step = 1, min = 1, width = "100px"),
        if (length(rf_class) > 0) {
          column(12, style = "background: white",

                 div(span(
                   strong("Classification models"), inline(
                     div(actionButton(ns("down_rftable_class"), icon("fas fa-download")))
                   )
                 )),

                 inline(DT::dataTableOutput(ns('rftab_class')))

          )
        },
        hr(),
        if (length(rf_regs) > 0) {
          column(12, style = "background: white",
                 div(span(
                   strong("Regression models"), inline(
                     div(actionButton(ns("down_rftable_reg"), icon("fas fa-download")))
                   )
                 )),

                 div(DT::dataTableOutput(ns('rftab_regs')))
          )
        }
      )
    })
    output$viewsvm<-renderUI({
      svm_regs<-getglobal_svm()$regs
      svm_class<-getglobal_svm()$class

      div(
        h4(strong("SVM-Attribute"), inline(uiOutput(ns('svm_remove')))),
        numericInput(ns("svm_round"), "Round table:", value = 3, step = 1, min = 1, width = "100px"),
        if (length(svm_class) > 0) {
          column(12, style = "background: white",

                 div(span(
                   strong("Classification models"), inline(
                     div(actionButton(ns("down_svmtable_class"), icon("fas fa-download")))
                   )
                 )),
                 tags$style('#svmtab_class td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#svmtab_class th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput(ns('svmtab_class')))

          )
        },
        hr(),
        if (length(svm_regs) > 0) {
          column(12, style = "background: white",
                 div(span(
                   strong("Regression models"), inline(
                     div(actionButton(ns("down_svmtable_reg"), icon("fas fa-download")))
                   )
                 )),
                 tags$style('#svmtab_regs td {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 tags$style('#svmtab_regs th {padding: 3px;
                     text-align: left;
                     font-size:12px}'),
                 inline(DT::dataTableOutput(ns('svmtab_regs')))
          )
        }
      )
    })
    output$add_coords_button<-renderUI({
      req(length(input$add_coords_file$datapath) > 0)
      actionButton(ns("add_coords"), icon = icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"), "Click to save to the Datalist"),
                          "Add", style = "button_active"))
    })
    output$add_coords_intru<-renderUI({
      column(12,
             column(12, style = "margin-top: 20px",
                    strong("1. Use the button below to upload one.")),
             if (length(input$add_coords_file$datapath) > 0) {
               fluidRow(
                 column(12, style = "margin-top: 20px",
                        strong("2. Click ", strong("Add", style = "color: SeaGreen"), " to insert it to the Datalist:", em(input$data_bank, style = "color: SeaGreen"))),
                 column(12,
                        uiOutput(ns("error_coords")))
               )
             }
      )
    })
    output$add_base_button<-renderUI({
      req(length(input$add_base_file$datapath) > 0)
      actionButton(ns("add_base"), icon = icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"), "Click to save to the Datalist"),
                          "Add", style = "button_active"))
    })
    output$add_base_intru<-renderUI({
      column(12,
             column(12, style = "margin-top: 20px",
                    strong("1. Use the button below to upload one.")),
             if (length(input$add_base_file$datapath) > 0) {
               column(12, style = "margin-top: 20px",
                      strong("2. Click ", strong("Add", style = "color: SeaGreen"), " to insert it to the Datalist:", em(input$data_bank, style = "color: SeaGreen")))
             }
      )
    })
    output$add_layer_button<-renderUI({
      req(length(input$add_layer_file$datapath) > 0)
      actionButton(ns("add_layer"), icon = icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"), "Click to save to the Datalist"),
                          "Add", style = "button_active"))
    })
    output$add_layer_intru<-renderUI({
      column(12,
             column(12, style = "margin-top: 20px",
                    strong("1. Use the button below to upload one.")),
             if (length(input$add_layer_file$datapath) > 0) {
               column(12, style = "margin-top: 20px",
                      strong("2. Click ", strong("Add", style = "color: SeaGreen"), " to insert it to the Datalist:", em(input$data_bank, style = "color: SeaGreen")))
             }
      )
    })

    output$DT_data<-{DT::renderDataTable(
      getdata_bank(),
      extensions = c('FixedColumns',"FixedHeader"),
      options = list(
        pageLength = 15,
        info = FALSE,
        lengthMenu = list(c(15, -1), c( "15","All")),
        autoWidth=F,
        scrollX = TRUE,
        scrollY = "400px",
        fixedHeader=TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 0)),
      rownames = TRUE,
      class ='cell-border compact stripe',
      editable=T)}
    output$DT_factors<-{DT::renderDataTable(attr(getdata_bank(),"factors"),
                                            options = list(pageLength = 15,lengthMenu = list(c(15, -1), c( "15","All")), autoWidth=F,scrollX = TRUE, scrollY = "400px"), rownames = TRUE,class ='cell-border compact stripe', editable=T)}
    output$DTcoords<-{DT::renderDataTable(data.frame(attr(getdata_bank(),"coords")),options = list(
      pageLength = 15, info = FALSE,lengthMenu = list(c(15, -1), c( "15","All")), autoWidth=T,dom = 'lt',scrollX = TRUE, scrollY = "400px"), rownames = TRUE,class ='cell-border compact stripe')}

    output$nbtab_class<-{DT::renderDataTable({
      vals$nbtable_class<-table<-getglobal_nb()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],3)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    })}
    output$svmtab_class<-{DT::renderDataTable({
      vals$svmtable_class<-table<-getglobal_svm()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$svm_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autoWidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    })}
    output$svmtab_regs<-{DT::renderDataTable({
      vals$svmtable_reg<-table<-getglobal_svm()$regs
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$svm_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(table,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(9,13), `border-right` = "solid 1px"
      )

    })}
    output$knntab_class<-{DT::renderDataTable({
      vals$knntable_class<-table<-getglobal_knn()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$knn_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't',scrollX = TRUE, scrollY = "400px")),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    })}
    output$knntab_regs<-{DT::renderDataTable({
      vals$knntable_reg<-table<-getglobal_knn()$regs
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$knn_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(table,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't',scrollX = TRUE, scrollY = "400px")),
          c(1), `border-left` = "solid 1px"
        ),       c(9,13), `border-right` = "solid 1px"
      )

    })}
    output$rftab_class<-{DT::renderDataTable({
      vals$rftable_class<-table<-getglobal_rf()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$rf_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autoWidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    })}
    output$rftab_regs<-{DT::renderDataTable({
      vals$rftable_reg<-table<-getglobal_rf()$regs
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$rf_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(table,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autoWidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(9,13), `border-right` = "solid 1px"
      )

    })}
    output$sgboosttab_class<-{DT::renderDataTable({
      vals$sgboosttable_class<-table<-getglobal_sgboost()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$sgboost_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    })}
    output$sgboosttab_regs<-{DT::renderDataTable({
      vals$sgboosttable_reg<-table<-getglobal_sgboost()$regs
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$sgboost_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(table,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(9,13), `border-right` = "solid 1px"
      )

    })}
    output$xyftab_class<-{DT::renderDataTable({
      vals$xyftable_class<-table<-getglobal_xyf()$class
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$xyf_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )

    })}
    output$xyftab_regs<-{DT::renderDataTable({
      vals$xyftable_reg<-table<-getglobal_xyf()$regs
      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],input$xyf_round)
      table
      DT::formatStyle(
        DT::formatStyle(
          DT::datatable(table,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),
                                     rownames=T,
                                     info=FALSE,autowidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(9,13), `border-right` = "solid 1px"
      )

    })}


    getglobal_svm<-reactive({
      getglobal_model(getdata_bank(),'svm')
    })

    removechange<-reactive({

      modalDialog(size="m",easyClose = T,footer =NULL,
                  div(
                    inline(uiOutput(ns('ddcogs3_pick'))),
                    span(
                      inline(uiOutput(ns('ddcogs3_edit'))),
                      bsButton(ns("ddcogs1_rename_remove"),icon(verify_fa = FALSE,name=NULL,class="fas fa-trash-alt")), modalButton("Dismiss")
                    ),
                    div(
                      renderPrint(attr(getdata_bank(),"transf"))
                    )
                  )
      )
    })
    bag_name_new<-reactive({
      bag<-1
      name0<-input$data_bank
      name1<-paste(name0,bag)
      if(name1%in%names(vals$saved_data))
      {
        repeat{
          bag<-bag+1
          name1<-paste(name0,bag)
          if(!name1%in%names(vals$saved_data)) break
        }
      }
      paste(name0,bag)

    })
    getdown<-reactive({
      switch(vals$hand_down,
             "data"=getdata_bank(),
             "factors"=attr(getdata_bank(),"factors"),
             "coords"=attr(getdata_bank(),"coords"),

             "som"=combsom_down()
      )
    })
    getglobal_nb<-reactive({
      getglobal_model(getdata_bank(),'nb')
    })
    getglobal_rf<-reactive({
      getglobal_model(getdata_bank(),'rf')
    })
    combsom<-reactive({
      soms<-attr(getdata_bank(),"som")
      req(length(soms)>0)
      combsom<-do.call("cbind",lapply(soms,train.summary_fun))
      colnames(combsom)<-names(soms)
      combsom
    })
    combsom_down<-reactive({
      combsom_down<-do.call("cbind",lapply(attr(getdata_bank(),"som"),train.summary_fun))
      colnames(combsom_down)<-names(attr(getdata_bank(),"som"))
      data.frame(combsom_down)
    })
    layers_choices<-reactive({
      base_shape<-attr(getdata_bank(),"base_shape")
      layer_shape<-attr(getdata_bank(),"layer_shape")
      eshape<-attr(getdata_bank(),"extra_shape")

      pic<-which(unlist(lapply(list(base_shape,layer_shape),function(x)length(x)>0)))
      choices=c(c("Base Shape","Layer Shape"),names(eshape))
      choices
    })
    getglobal_knn<-reactive({
      getglobal_model(getdata_bank(),'knn')
    })
    getglobal_sgboost<-reactive({
      getglobal_model(getdata_bank(),'sgboost')
    })
    getglobal_xyf<-reactive({
      getglobal_model(getdata_bank(),'xyf')
    })

    observeEvent(ignoreInit = T,input$down_rftable_reg,{
      vals$hand_down<-"rf_stats_reg"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$down_rftable_class,{
      vals$hand_down<-"rf_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })

    observeEvent(ignoreInit = T,input$down_svmtable_reg,{
      vals$hand_down<-"svm_stats_reg"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$down_svmtable_class,{
      vals$hand_down<-"svm_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$delete_coords,{
      showModal(
        modalDialog(
          title="Are you sure?",
          column(12, style="margin-top: 20px; margin-bottom: 20px",
                 h5(strong("Remove coords from",style="color: red"),span("Datalist:"),em(input$data_bank,style="color: gray")),
                 bsButton(ns("delete_coords_yes"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Confirm", style="button_active",block=T))
          ,
          easyClose = T,
          size = "s"

        )
      )
    })
    observeEvent(ignoreInit = T,input$delete_coords_yes,{
      attr(vals$saved_data[[input$data_bank]],"coords")<-NULL
      removeModal()
      updateRadioGroupButtons(session,"view_datalist", selected="coords")


    })
    observeEvent(ignoreInit = T,input$delete_som,{
      showModal(
        modalDialog(
          title="Are you sure?",
          column(12, style="margin-top: 20px; margin-bottom: 20px",
                 h5(strong("Remove som",style="color: red"),em(paste0(input$remove_som,collapse="; "),style="color: gray")),
                 bsButton(ns("delete_som_yes"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Confirm", style="button_active",block=T)),
          easyClose = T,
          size = "s"

        )
      )
    })
    observeEvent(ignoreInit = T,input$delete_som_yes,{
      vals$saved_data[[input$data_bank]][input$remove_som]<-NULL
      removeModal()

    })
    observeEvent(ignoreInit = T,input$add_base,{

      t<-try({get(gsub(" ", "", capture.output(
        load(input$add_base_file$datapath, verbose = T)
      )[2]))})
      if("try-error" %in% class(t)) {t<-readRDS(input$add_base_file$datapath) }

      attr(vals$saved_data[[input$data_bank]],"base_shape")<-t
      updateRadioGroupButtons(
        session,"view_datalist", selected="extra_shape"
      )

    })
    observeEvent(ignoreInit = T,input$add_layer,{

      t<-try({get(gsub(" ", "", capture.output(
        load(input$add_layer_file$datapath, verbose = T)
      )[2]))})
      if("try-error" %in% class(t)) {t<-readRDS(input$add_layer_file$datapath) }
      attr(vals$saved_data[[input$data_bank]],"layer_shape")<-t

      updateRadioGroupButtons(
        session,"view_datalist", selected="extra_shape"
      )
      t
    })
    observeEvent(ignoreInit = T,input$add_coords_file,{
      output$error_coords<-renderUI("bank compleated")
    })
    observeEvent(ignoreInit = T,input$add_coords,{

      coords<-data.frame(fread(input$add_coords_file$datapath))
      rownames(coords)<-coords[, 1]
      coords[, 1]<-NULL
      if(ncol(coords)!=2){ output$error_coords<-
        renderUI({
          column(12, strong(
            "Invalid Entries. The first column must contain the name of the observations. The second and third columns must contain the logitude and latitude respectively", style="color: red"))
        })}

      if(any(rownames(coords)%in%rownames(getdata_bank()))==F) {
        output$error_coords<-
          renderUI({
            column(12, strong(
              "None of the IDs of the banked coordinates are compatible with the ids of the selected datalist. Please bank coodinates with valid IDs", style="color: red"))
          })
      }

      req(any(rownames(coords)%in%rownames(getdata_bank())))

      attr(vals$saved_data[[input$data_bank]],"coords")<-na.omit(
        coords[rownames(getdata_bank()),]
      )
      updateRadioGroupButtons(
        session,"view_datalist", selected="coords"
      )

    })
    observeEvent(ignoreInit = T,input$DT_factors_cell_edit, {
      factors<-attr(getdata_bank(),"factors")
      row <-input$DT_factors_cell_edit$row
      clmn<-input$DT_factors_cell_edit$col
      value<-input$DT_factors_cell_edit$value
      if(!value%in%levels(factors[,clmn])){
        levels(factors[,clmn])<-c(levels(factors[,clmn]), value)
        factors[row, clmn]<-value
        attr(vals$saved_data[[input$data_bank]],"factors")<-factors

      } else{
        attr(vals$saved_data[[input$data_bank]],"factors")[row, clmn]<-input$DT_factors_cell_edit$value
      }

    })
    observeEvent(ignoreInit = T,input$delete_elayer_shape,{
      req(input$pick_elayers=='Base Shape')
      attr(vals$saved_data[[input$data_bank]],"base_shape")<-NULL
      removeModal()
      updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
    })
    observeEvent(ignoreInit = T,input$ddcogs2,{
      vals$hand_down<-"Data-Attribute"
      vals$data_bank_data<-getdata_bank()
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals,name=paste0(input$data_bank,"_","Numeric"))
    })
    observeEvent(ignoreInit = T,input$trash_som ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove Som-Attribute"),
                    div(
                      div(
                        div(style="overflow-y: scroll;height: 200px;overflow-x: scroll; padding-top: 10px",
                            checkboxGroupInput(ns("remove_som"), NULL, choices=names(attr(getdata_bank(),"som")))
                        )
                      ),
                      bsButton(ns("delete_som"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })
    observeEvent(ignoreInit = T,input$delete_elayer_shape,{
      req(input$pick_elayers=='Layer Shape')
      attr(vals$saved_data[[input$data_bank]],"layer_shape")<-NULL
      removeModal()
      updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
    })
    observeEvent(ignoreInit = T,input$dfcogs2,{
      vals$hand_down<-"Factor-Attribute"
      vals$data_bank_data<-attr(getdata_bank(),"factors")
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals,name=paste0(input$data_bank,"_","Factors"))
    })
    observeEvent(ignoreInit = T,input$downcenter_coords,{
      vals$hand_down<-"Coords-Attribute"
      vals$data_bank_data<-attr(getdata_bank(),"coords")
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals,name=paste0(input$data_bank,"_","Coords"))
    })
    observeEvent(ignoreInit = T,input$data_bank,{
      vals$cur_data<-input$data_bank
    })
    observeEvent(ignoreInit = T,input$downcenter_som,{
      vals$hand_down<-"SOM - summary table"

      mlist<-attr(getdata_bank(), "som")

      res<-lapply(names(mlist),function(i){
        m=mlist[[i]]
        supersom_summaries3(m, i)
      })
      res2<-data.frame(res)
      colnames(res2)<-NULL


      vals$comb_som<-res2
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)


    })
    observeEvent(ignoreInit = T,input$view_datalist,
                 {vals$curview_databank<-input$view_datalist})
    observeEvent(ignoreInit = T,input$DT_data_cell_edit, {
      row <-input$DT_data_cell_edit$row
      clmn<-input$DT_data_cell_edit$col
      value<-if(input$DT_data_cell_edit$value==""){NA}else{as.numeric(input$DT_data_cell_edit$value)}
      vals$saved_data[[input$data_bank]][row, clmn]<-value
    })
    observeEvent(ignoreInit = T,input$ddcogs1_pick,{
      vals$ddcogs1_pick<-input$ddcogs1_pick
    })
    observeEvent(ignoreInit = T,input$dfcogs1_pick,{
      vals$dfcogs1_pick<-input$dfcogs1_pick
    })
    observeEvent(ignoreInit = T,input$ddcogs3_edit,{
      vals$ddcogs3_edit<-input$ddcogs3_edit
    })
    observeEvent(ignoreInit = T,input$ddcogs3,{

      showModal(
        removechange()
      )
    })
    observeEvent(ignoreInit = T,input$ddcogs1_rename_remove,{
      #pic<-colnames(attr(getdata_bank(),"transf"))==input$ddcogs3_edit
      attr(vals$saved_data[[input$data_bank]],"transf")<-data.frame(attr(vals$saved_data[[input$data_bank]],"transf"))
      attr(vals$saved_data[[input$data_bank]],"transf")[input$ddcogs3_edit]<-NULL
    })
    observeEvent(ignoreInit = T,input$ddcogs1,{
      showModal(
        modalDialog(size="m",easyClose = T,footer =NULL,
                    div(
                      inline(uiOutput(ns('ddcogs1_pick'))),
                      span(
                        inline(textInput(ns("ddcogs1_rename"), div("Rename Variable:",em(inline(uiOutput(ns('ddcogs1_targ'))), style="color: SeaGreen")),placeholder = "New name", width="200px")),
                        bsButton(ns("ddcogs1_rename_go"),"Rename"), modalButton("Dismiss")
                      )
                    )
        )
      )
    })
    observeEvent(ignoreInit = T,input$dfcogs1,{
      showModal(
        modalDialog(size="m",easyClose = T,footer =NULL,
                    div(
                      inline(uiOutput(ns('dfcogs1_pick'))),
                      span(
                        inline(textInput(ns("dfcogs1_rename"), div("Rename Factors:",em(inline(uiOutput(ns('dfcogs1_targ'))), style="color: SeaGreen")),placeholder = "New name", width="200px")),
                        bsButton(ns("dfcogs1_rename_go"),"Rename"), modalButton("Dismiss")
                      )
                    )
        )
      )
    })
    observeEvent(ignoreInit = T,input$ddcogs1_rename_go,{

      updateTextInput(session,'view_datalist', value="data")
      req(input$ddcogs1_rename!="")
      pic<- which( colnames(getdata_bank())==input$ddcogs1_pick)
      colnames(vals$saved_data[[input$data_bank]])[pic]<-input$ddcogs1_rename
    })
    observeEvent(ignoreInit = T,input$dfcogs1_rename_go,{
      req(input$dfcogs1_rename!="")
      pic<- which( colnames(attr(getdata_bank(),"factors"))==input$dfcogs1_pick)
      colnames(attr(vals$saved_data[[input$data_bank]],"factors"))[pic]<-input$dfcogs1_rename
    })
    observeEvent(ignoreInit = T,input$comments_remove,{
      attr(vals$saved_data[[input$data_bank]],"notes")<-NULL
    })
    observeEvent(ignoreInit = T,input$delete_elayer_shape,{
      req(input$pick_elayers!='Base Shape')
      req(input$pick_elayers!='Layer Shape')
      attr(vals$saved_data[[input$data_bank]],"extra_shape")[[input$pick_elayers]]<-NULL
      removeModal()
      updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
    })
    observeEvent(ignoreInit = T,input$trash_rf ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove RF-Attribute"),
                    div(
                      div(
                        div(style="overflow-y: scroll;height: 200px;overflow-x: scroll; padding-top: 10px",
                            checkboxGroupInput(ns("remove_rf"), NULL, choices=names(attr(getdata_bank(),"rf")))
                        )
                      ),
                      bsButton(ns("delete_rf"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })
    observeEvent(ignoreInit = T,input$delete_rf,{
      attr(vals$saved_data[[input$data_bank]],"rf")[input$remove_rf]<-NULL
      removeModal()
    })
    observeEvent(ignoreInit = T,input$trash_nb ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove NB-Attribute"),
                    div(
                      div(
                        div(style="ovenblow-y: scroll;height: 200px;ovenblow-x: scroll; padding-top: 10px",
                            checkboxGroupInput(ns("remove_nb"), NULL, choices=names(attr(getdata_bank(),"nb")))
                        )
                      ),
                      bsButton(ns("delete_nb"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })
    observeEvent(ignoreInit = T,input$delete_nb,{
      attr(vals$saved_data[[input$data_bank]],"nb")[input$remove_nb]<-NULL
      removeModal()
    })
    observeEvent(ignoreInit = T,input$trash_svm ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove SVM-Attribute"),
                    div(
                      div(
                        div(style="ovesvmlow-y: scroll;height: 200px;ovesvmlow-x: scroll; padding-top: 10px",
                            checkboxGroupInput(ns("remove_svm"), NULL, choices=names(attr(getdata_bank(),"svm")))
                        )
                      ),
                      bsButton(ns("delete_svm"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })
    observeEvent(ignoreInit = T,input$delete_svm,{
      attr(vals$saved_data[[input$data_bank]],"svm")[input$remove_svm]<-NULL
      removeModal()
    })
    observeEvent(ignoreInit = T,input$down_nbtable_class,{
      vals$hand_down<-"nb_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$trash_knn ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove KNN-Attribute"),
                    div(
                      div(
                        div(style="oveknnlow-y: scroll;height: 200px;oveknnlow-x: scroll; padding-top: 10px",
                            checkboxGroupInput(ns("remove_knn"), NULL, choices=names(attr(getdata_bank(),"knn")))
                        )
                      ),
                      bsButton(ns("delete_knn"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })
    observeEvent(ignoreInit = T,input$delete_knn,{
      attr(vals$saved_data[[input$data_bank]],"knn")[input$remove_knn]<-NULL
      removeModal()
    })
    observeEvent(ignoreInit = T,input$down_knntable_reg,{
      vals$hand_down<-"knn_stats_reg"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$down_knntable_class,{
      vals$hand_down<-"knn_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$trash_sgboost ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove GBM-Attribute"),
                    div(
                      div(
                        div(style="ovesgboostlow-y: scroll;height: 200px;ovesgboostlow-x: scroll; padding-top: 10px",
                            checkboxGroupInput(ns("remove_sgboost"), NULL, choices=names(attr(getdata_bank(),"sgboost")))
                        )
                      ),
                      bsButton(ns("delete_sgboost"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })
    observeEvent(ignoreInit = T,input$delete_sgboost,{
      attr(vals$saved_data[[input$data_bank]],"sgboost")[input$remove_sgboost]<-NULL
      removeModal()
    })
    observeEvent(ignoreInit = T,input$down_sgboosttable_reg,{
      vals$hand_down<-"sgboost_stats_reg"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$down_sgboosttable_class,{
      vals$hand_down<-"sgboost_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$trash_xyf ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove xyf-Attribute"),
                    div(
                      div(
                        div(style="ovexyflow-y: scroll;height: 200px;ovexyflow-x: scroll; padding-top: 10px",
                            checkboxGroupInput(ns("remove_xyf"), NULL, choices=names(attr(getdata_bank(),"xyf")))
                        )
                      ),
                      bsButton(ns("delete_xyf"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })

    observeEvent(ignoreInit = T,input$delete_xyf,{
      attr(vals$saved_data[[input$data_bank]],"xyf")[input$remove_xyf]<-NULL
      removeModal()
    })
    observeEvent(ignoreInit = T,input$down_xyftable_reg,{
      vals$hand_down<-"xyf_stats_reg"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$down_xyftable_class,{
      vals$hand_down<-"xyf_stats_class"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter",  vals=vals)
    })
    observeEvent(ignoreInit = T,input$comments_edit,{
      showModal( modal_comment())
    })

    })
}



