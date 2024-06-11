options(shiny.autoload.r=FALSE)
#' @noRd
#'
#' @export
module_ui_downcenter<-function(id){
  ns<-NS(id)


}

#' @export
module_server_downcenter<-function (input, output, session,vals, name=NULL,message=NULL,data=NULL){
  ns<-session$ns
  if(is.null(message)){
    message<-vals$hand_down
  }

  getdown<-reactive({
    switch(vals$hand_down,
           "generic"=data,
           "SOM - summary table"=vals$comb_som,
           "gstat model - prediction results"=vals$gstat_cross_validation,
           "gstat model - cross validation results"=vals$gstat_cross_validation,
           "download metrics"=vals$sup_metrics,
           "Download dataframe"={vals$down_data.frame},
           'confusion'={data.frame(vals$down_ensemble)},
           "Filtered pool list"=data.frame(vals$filtered_pool_list),
           "Coords-Attribute"=data.frame(vals$data_bank_data),
           "Factor-Attribute"=data.frame(vals$data_bank_data),
           "Data-Attribute"=data.frame(vals$data_bank_data),
           "ensemble_inter_table"= data.frame(vals$down_ensemble_inter_table),
           'ensemble_pool_predictions'=data.frame(vals$down_ensemble_pool_metrics),
           "ensemble_bucket_predictions"=data.frame(vals$bucket_results),
           "Corr result"=data.frame(vals$corr_results),
           "som_predict_result"=data.frame(vals$som_predict_result),
           "som_perf_result"=data.frame(vals$som_perf_result),
           "Model Comparation (summary)"=data.frame(vals$getsummary_comp),
           "Model Comparation (pairwise table)"=data.frame(vals$getsummary_comp),
           "Niche results"=data.frame(vals$niche_result),
           "screeplot"=vals$screeplot_results,
           "rda"={data.frame(vals$rda_summary)},
           "segRDA"={data.frame(vals$segrda_summary)},
           "DP smw"={data.frame(vals$dp_smw)},
           "PCA result"=vals$pca_out,

           'nb_stats_class'=data.frame(vals$nbtable_class),
           'rf_stats_class'=data.frame(vals$rftable_class),
           'rf_stats_reg'=data.frame(vals$rftable_reg),
           'svm_stats_class'=data.frame(vals$svmtable_class),
           'svm_stats_reg'=data.frame(vals$svmtable_reg),

           'knn_stats_class'=data.frame(vals$knntable_class),
           'knn_stats_reg'=data.frame(vals$knntable_reg),
           'sgboost_stats_class'=data.frame(vals$sgboosttable_class),
           'sgboost_stats_reg'=data.frame(vals$sgboosttable_reg),

           "rfdepth"=data.frame(attr(vals$RF_results,"mindepth")[[2]]),
           "rfinter"=data.frame(attr(vals$RF_results,"interframe")),
           "rf_predictions"={data.frame( vals$rftab_pred)},
           "rf_class_obsErrs"=data.frame(accu_rf_class(vals$RF_results)),
           "rf_reg_obsErrs"=data.frame(accu_rf_reg_model(vals$RF_results)),
           "rf_reg_predErrs"=data.frame(vals$rf_prederrors),
           "rf_class_predErrs"=data.frame(vals$rf_prederrors),
           "rf_cm"=data.frame(vals$rf_cm),
           "rf_qclass"=data.frame(vals$rf_treetest),
           "rf_cmTest_rf"=data.frame(vals$rf_test_cm),
           "rf_cmbyclass"=data.frame(vals$rf_stat_byclass),
           "rf_cmoverall"=data.frame(vals$rf_stat_overall),
           "rf_cmTest_byclass"=data.frame(vals$rf_test_stat_byclass),
           "rf_cmTest_overall"=data.frame(vals$rf_test_stat_overall),
           'Global Stats - RF pred Datalist'=data.frame(vals$rf_preddatalist),
           "knn - training errors (observations)"=   data.frame(vals$knn_down_errors_train),
           "knn - predictions"=data.frame(vals$knntab_pred),
           "knn - variable importance"=data.frame(vals$knn_varImp),
           "knn - table"=data.frame(vals$knn_tableClass),
           "knn_cm"=data.frame(vals$knn_cm),
           "knn_cm_test"=data.frame(vals$knn_cm_test),

           "NB - training errors (observations)"=   data.frame(vals$nb_down_errors_train),
           "NB - predictions"=data.frame(vals$nbtab_pred),
           "NB - variable importance"=data.frame(vals$nb_varImp),
           "NB - table"=data.frame(vals$nb_tableClass),
           "nb_cm"=data.frame(vals$nb_cm),
           "nb_cm_test"=data.frame(vals$nb_cm_test),


           "SVM - training errors (observations)"=   data.frame(vals$svm_down_errors_train),
           "SVM - predictions"=data.frame(vals$svmtab_pred),
           "SVM - variable importance"=data.frame(vals$svm_varImp),
           "SVM - table"=data.frame(vals$svm_tableClass),
           "svm_cm"=data.frame(vals$svm_cm),
           "svm_cm_test"=data.frame(vals$svm_cm_test),

           "som - training errors (observations)"=   data.frame(vals$som_down_errors_train),
           "som - predictions"=data.frame(vals$somtab_pred),
           "som_cm"=data.frame(vals$som_cm),
           "som_cm_test"=data.frame(vals$som_cm_test),

           "GBM - training errors (observations)"=   data.frame(vals$sgboost_down_errors_train),
           "GBM - predictions"=data.frame(vals$sgboosttab_pred),
           "GBM - variable importance"=data.frame(vals$sgboost_varImp),
           "GBM_cm"=data.frame(vals$sgboost_cm),
           "GBM_cm_test"=data.frame(vals$sgboost_cm_test),


           "pcorr"=vals$biplot_som,
           "pcodes"={
             res=data.frame(vals$som_results$codes[[1]])
             rownames(res)<-paste("unit",1:nrow(res))
             res},
           "som predictions"={data.frame(vals$predsom_reults)},

           "comp_qclass"={data.frame(vals$down_ensemble)},
           'ensemble_model_predictions'={data.frame(vals$down_ensemble)},
           'ensemble_predictions'={data.frame(vals$down_ensemble)},
           'ensemble_confusion'={data.frame(vals$down_ensemble)},
           'ensemble_obs_errors'={data.frame(vals$down_ensemble)},
           "ensemble_interactions"={data.frame(vals$down_ensemble)}


    )
  })

  observeEvent(input$down_sep,{
    vals$down_sep<-input$down_sep
  })
  output$csv_format<-renderUI({
    req(input$down_type=='.csv')
    splitLayout(
      column(12,
             radioButtons(ns("down_sep"),strong("sep",tipify(icon("fas fa-question-circle"),"the field separator string. Values within each row of x are separated by this string.", options=list(container="body"))),
                          choiceValues =list(",",";"),
                          choiceNames =list(
                            "comma",
                            "semicolon"
                          ),
                          selected=vals$down_sep
             )),
      column(12,
             uiOutput(ns('down_sep_semi')) ,

             uiOutput(ns('down_sep_comma'))

      )

    )
  })


  observeEvent(input$down_dec,{
    vals$down_dec<-input$down_dec
  })
  output$down_sep_semi<-renderUI({
    req(input$down_sep==';')
    radioButtons(ns("down_dec"),strong("dec",tipify(icon("fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                 choiceValues =list(".",","),
                 choiceNames=list(
                   "dot","comma"), selected=vals$down_dec)

  })

  cur_down_dec<-reactiveValues(df=NULL)
  observeEvent(input$down_dec,{
    vals$down_dec<-input$down_dec
  })

  output$down_sep_comma<-renderUI({
    req(input$down_sep==',')
    column(12,
           radioButtons(ns("down_dec"),strong("dec",tipify(icon("fas fa-question-circle"), "the string to use for decimal points in columns", options=list(container="body"))),
                        choiceValues =list("."),
                        choiceNames=list("dot"),
                        selected=vals$down_dec
           ))

  })

  output$download_action<-{
    downloadHandler(
      filename = function() {
        if(!is.null(name)){
          paste0(name,"_", Sys.Date(), input$down_type)
        } else{
          paste0(vals$hand_down,"_", Sys.Date(), input$down_type)
        }

      }, content = function(file) {
        if(input$down_type==".csv"){
          write.table(x=data.frame(getdown()),file,append=T,quote=F,row.names=T,col.names=NA, input$down_sep,
                      dec=input$down_dec)
        }
        if(input$down_type==".xlsx"){
          write_xlsx(cbind(id=rownames(getdown()),getdown()), file)
        }
        removeModal()

      })

  }

  observeEvent(input$down_type,{
    vals$down_type<-input$down_type
  })

  tags$div(class="downcenter",
           showModal({

             modalDialog(

               column(12,
                      h5(strong(message)),
                      splitLayout(cellWidths = c("30%","70%"),
                                  column(12,
                                         radioButtons(ns("down_type"),
                                                      strong("format",tipify(icon("fas fa-question-circle"),"file extension", options=list(container="body"))),c(".xlsx",".csv")),
                                         selected=vals$down_type
                                  ),
                                  uiOutput(ns("csv_format"))
                      ),
                      column(12,
                             downloadButton(ns("download_action"),NULL,icon=icon("fas fa-download"),style="width: 50%"))

               )

               ,
               footer = actionButton(ns("remove_dc"),"Dismiss"),
               title=h4(icon("fas fa-download"),strong("Download")),
               size="m",
               easyClose = T

             )
           })
  )

  observeEvent(input$remove_dc,{
    removeModal()
    if(isTRUE(vals$task_open)){
      vals$task_open_restore<-T
    }
  })

}
