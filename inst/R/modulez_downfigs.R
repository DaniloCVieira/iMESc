options(shiny.autoload.r=FALSE)
#' @noRd
print.ggmatrix<-GGally:::print.ggmatrix


#GGally:::make_ggmatrix_plot_obj(plot)
#' @noRd

#' @export
module_ui_figs<- function(id){
  ns<-NS(id)
  uiOutput(ns("go"))
}



#' @export
module_server_figs<-function (input, output, session,vals,file="",datalist_name=T, message=NULL, name_c=NULL, generic=NULL,fun=NULL,args_plot=NULL){
  ns<-session$ns
  if(is.null(message)){
    message<-vals$hand_plot
  }
  if(is.null(name_c)){
    name_c<-vals$hand_plot
  }
  if(vals$hand_plot%in%c("generic_gg","generic_replay")){
    req(generic)
  }


  if(isTRUE(datalist_name)){
    datalist_name=vals$cur_data
  }

  output$go<-renderUI({
    div(tags$style(figdim()))
  })

  fn_download<-function()
  {
    vals$fres<-input$fres
    vals$fwidth<-input$fwidth
    vals$fheight<-input$fheight
    vals$pointsize<-input$pointsize
    vals$fformat<-input$fformat
    vals$fig_preview<-input$fig_preview

    fn_downloadf<-fn_downloadf()
    #saveRDS(fn_downloadf,"fn_downloadf.rds")
    #saveRDS(reactiveValuesToList(input),'input.rds')


    #fn_downloadf<-readRDS("fn_downloadf.rds")
    #input<-readRDS("input.rds")
    #vals<-readRDS("vals.rds")
    fheight<-input$fheight
    fwidth<-input$fwidth
    fres<-as.numeric(input$fres)

    if(input$fformat=="pdf"|input$fformat=="svg") fheight<-round(fheight*0.3937,2)
    if(input$fformat=="pdf"|input$fformat=="svg") fwidth<-round(fwidth*0.3937,2)

    if(input$fformat=="png") png(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize)
    if(input$fformat=="tiff") tiff(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm",compression="lzw", pointsize = input$pointsize)
    if(input$fformat=="jpeg") jpeg(fn_downloadf(), height=fheight, width=fwidth, res=fres, units="cm",quality=100, pointsize = input$pointsize)
    if(input$fformat=="pdf") pdf(fn_downloadf(), height=fheight, width=fwidth, pointsize = input$pointsize)
    if(input$fformat=="svg") svg(fn_downloadf(), height=fheight, width=fwidth, pointsize = input$pointsize)



    switch (
      vals$hand_plot,
      "plot_funcion"=do.call(fun,args_plot),
      "generic_ggmatrix"={print.ggmatrix(generic)},
      "generic_gg"={plot(generic)},
      "generic_replay"={replayPlot(generic)},
      "ggplot map"={plot(vals$newggplot)},
      "Variogram"={plot(vals$g_variogram)},
      "colSums Histogram"=plot(vals$gghist_colsums),
      "Feature Shuffling"={plot(vals$spd_varImp_plot)},

      "Feature Shuffling"={plot(vals$spd_varImp_plot)},
      "Pairs-plot"={print.ggmatrix(vals$desc_pairplot)},

      "Pairs plot"=print.ggmatrix(vals$fm_downplot4),
      "Feature plot"=plot(vals$fm_downplot),
      "Feature plot 2"=plot(vals$fm_downplot2),
      "Feature plot 3"=plot(vals$fm_downplot3),

      'Confusion Matrix'=plot(vals$plot_ensemble),
      "Surface map"=replayPlot(vals$map_res),
      "Scatter 3D"=replayPlot(vals$map_res),
      "mantel plot"=replayPlot(vals$mantel_plot),
      "stacked raster map"=replayPlot(vals$map_res),
      "stacked scatter map"=replayPlot(vals$map_res),
      "Map"=plot(vals$map_res),
      "EBNB"=plot(vals$plot_niche),
      'ensemble - Variable Importance plot'=plot(vals$ensemble_varImp_plot),
      'Confusion Matrix Post-Shuffling'=plot(vals$feature_cm_plot),
      'Correlation Plot'=replayPlot(vals$plot_correlation),
      'Missing plot'=plot(vals$missing_plot),
      'Scatter plot'=replayPlot(vals$scatter_plot),
      'Corr plot'=replayPlot(vals$corplot),
      "Download plot - Model comparation"=plot(vals$comp_plot),
      "k-means (pca reprentation)"= plot(vals$kmeans_plot_data),
      "k-means (codebook)"=plot(vals$psom_plot),
      "Dendrogram"=replayPlot(vals$hc_tab1_plot),
      "Scree plot"=plot(vals$scree_plot_hc),
      "Codebook screeplot"=plot(vals$hc_tab6_plot),
      "Hcut"=plot(vals$hc_tab3_plot),
      "codebook clusters"=plot(vals$hc_tab4_plot),
      "mapcodes predictions"=plot(vals$hc_tab5_plot),
      "Ridge plot"=plot(vals$rid_plot),
      "variable summary"={
        replayPlot(vals$varplot)
      },
      "Factor plot"={plot(vals$factorsplot)},
      "histogram"={replayPlot(vals$phist)},
      "boxplot"={plot(vals$pbox_plot)},
      "pca"=   {plot(vals$ppca_plot)},
      "mds"= {plot(vals$pmds_plot)},
      "rda"={plot(vals$rda_plot)},
      "dp"={replayPlot(vals$plot_dp)},
      "we"={replayPlot(vals$plot_we)},
      "segrda"={plot(vals$seg_rda_plot)},

      "Confusion Matrix RF"={plot(vals$cm_rf)},
      "Confusion Matrix - test RF"={plot(vals$conf_rf)},
      "Minimal Depth distribution"={plot(vals$rfd_res)},
      "Multi-way importance"={plot(vals$rfm_res)},
      "RF interactions"={plot(vals$rf_inter)},
      "RF - Partial Dependence"=plot(vals$rfbiplot1),
      "RF ranking comparations"=plot(rf_rank$df),
      "RF measure comparations"=plot(rf_rel$df),
      "RF - Partial Dependence (classes)"=plot(vals$rfbiplot2),
      "florest plot"=replayPlot(vals$plot_florest),
      'florest plot (class)'=plot(vals$plot_florest_class),


      "NB - Confusion Matrix"={plot(vals$cm_nb)},
      "NB - Variable Importance plot"=plot(vals$nb_varImp_plot),
      "NB - Confusion Matrix (predictions)"=plot(vals$nb_cm_pred),
      "NB - DM plot"=replayPlot(vals$nb_densplot),

      "SVM - Confusion Matrix"={plot(vals$cm_svm)},
      "SVM - Variable Importance plot"=plot(vals$svm_varImp_plot),
      "SVM - Confusion Matrix (predictions)"=plot(vals$svm_cm_pred),
      "SVM - DM plot"=replayPlot(vals$svm_densplot),

      "knn - Confusion Matrix"={plot(vals$cm_knn)},
      "knn - Variable Importance plot"=plot(vals$knn_varImp_plot),
      "knn - Confusion Matrix (predictions)"=plot(vals$knn_cm_pred),
      "knn - DM plot"=replayPlot(vals$knn_densplot),

      "sgboost - Confusion Matrix"={plot(vals$cm_sgboost)},
      "sgboost - Variable Importance plot"=plot(vals$sgboost_varImp_plot),
      "sgboost - Confusion Matrix (predictions)"=plot(vals$sgboost_cm_pred),
      "sgboost - DM plot"=replayPlot(vals$sgboost_densplot),

      "som - Confusion Matrix"={plot(vals$cm_som)},
      "som - Confusion Matrix (predictions)"=plot(vals$som_cm_pred),
      "som (xyf)"=plot(vals$xyf_bmu),
      "som counts (xyf)"=replayPlot(vals$xyf_counts),
      "som changes (xyf)"=replayPlot(vals$xyf_changes),

      "Training plot"={pchanges(vals$som_results)},
      "Couting plot"={  pcounts(vals$som_results)},
      "uMatrix"={   pUmatrix(vals$som_results)},
      "BMUs"={plot(vals$bmus_plot)},
      "BMUs predictions"={replayPlot(vals$bmus_pred_plot)},
      "property plot"={replayPlot(vals$pprop_plot)},
      "Confusion Matrix SOM"={plot(vals$conf_som)},

      "feature_importance_models"={plot(vals$plot_ensemble)},
      "ensemble_confusion_plot"={plot(vals$plot_ensemble)},
      "ensemble_predictions_class_plot"={plot(vals$plot_ensemble)},
      "ensemble_predictions_reg_plot"={plot(vals$plot_ensemble)},
      "ensemble_feature_importance_plot"={plot(vals$plot_ensemble)},
      "ensemble_interactions_plot"={plot(vals$plot_ensemble)}

    )


  }






  output$bn_download<-downloadHandler(
    filename = fn_downloadf,
    content = function(file) {
      removeModal()

      fn_download()
      dev.off()

      file.copy(fn_downloadf(), file, overwrite=T)

    }
  )
  fn_downloadf<-reactive({
    name_a<-ifelse(!is.null(datalist_name),paste0(datalist_name,"_"),"")
    name_b<-ifelse(file!="",paste0(file,"_"),"")

    name_file<-paste0(name_a,name_b,name_c)
    if(input$fformat=="png") filename<-paste0(name_file,".png",sep="")
    if(input$fformat=="tiff") filename<-paste0(name_file,".tif",sep="")
    if(input$fformat=="jpeg") filename<-paste0(name_file,".jpg",sep="")
    if(input$fformat=="pdf") filename<-paste0(name_file,".pdf",sep="")
    if(input$fformat=="svg") filename<-paste0(name_file,".svg",sep="")
    return(filename)
  })




  output$plotoutput<-renderImage({
    req(length(input$fig_preview)>0)
    req(isTRUE(input$fig_preview))
    req(length(vals$hand_plot)>0)
    req(input$fheight)
    req(input$fwidth)
    req(input$fres)
    req(input$pointsize)
    withProgress( message = "Rendering preview",
                  {


                    fheight<-input$fheight
                    fwidth<-input$fwidth
                    fres<-as.numeric(input$fres)
                    png(paste0(name_c,".png",sep=""), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize)
                    switch(
                      vals$hand_plot,
                      "plot_funcion"=do.call(fun,args_plot),
                      "generic_ggmatrix"={print.ggmatrix(generic)},
                      "generic_gg"={plot(generic)},
                      "generic_replay"={replayPlot(generic)},

                      "ggplot map"={plot(vals$newggplot)},
                      "Variogram"={plot(vals$g_variogram)},
                      "data Histogram"=plot(vals$gghist_unlist_data),
                      "rowSums Histogram"=plot(vals$gghist_rowSums),
                      "colSums Histogram"=plot(vals$gghist_colsums),
                      "Feature Shuffling"={plot(vals$spd_varImp_plot)},
                      "Pairs-plot"={print.ggmatrix(vals$desc_pairplot)},

                      "Pairs plot"=print.ggmatrix(vals$fm_downplot4),
                      "Feature plot"=plot(vals$fm_downplot),
                      "Feature plot 2"=plot(vals$fm_downplot2),
                      "Feature plot 3"=plot(vals$fm_downplot3),
                      'Confusion Matrix'=plot(vals$plot_ensemble),
                      "Surface map"=replayPlot(vals$map_res),
                      "Scatter 3D"=replayPlot(vals$map_res),
                      "mantel plot"=replayPlot(vals$mantel_plot),
                      "stacked raster map"=replayPlot(vals$map_res),
                      "stacked scatter map"=replayPlot(vals$map_res),
                      "Map"=plot(vals$map_res),
                      "EBNB"=plot(vals$plot_niche),
                      'ensemble - Variable Importance plot'=plot(vals$ensemble_varImp_plot),
                      'Confusion Matrix Post-Shuffling'=plot(vals$feature_cm_plot),
                      'Correlation Plot'=replayPlot(vals$plot_correlation),
                      'Missing plot'=plot(vals$missing_plot),
                      'Scatter plot'=replayPlot(vals$scatter_plot),
                      'Corr plot'=replayPlot(vals$corplot),
                      "Download plot - Model comparation"=plot(vals$comp_plot),
                      "k-means (pca reprentation)"= plot(vals$kmeans_plot_data),
                      "k-means (codebook)"=plot(vals$psom_plot),
                      "Dendrogram"=replayPlot(vals$hc_tab1_plot),
                      "Scree plot"=plot(vals$scree_plot_hc),
                      "Codebook screeplot"=plot(vals$hc_tab6_plot),
                      "Hcut"=plot(vals$hc_tab3_plot),
                      "codebook clusters"=plot(vals$hc_tab4_plot),
                      "mapcodes predictions"=plot(vals$hc_tab5_plot),
                      "Ridge plot"=plot(vals$rid_plot),
                      "variable summary"={
                        replayPlot(vals$varplot)
                      },
                      "Factor plot"={plot(vals$factorsplot)},
                      "histogram"={replayPlot(vals$phist)},
                      "boxplot"={plot(vals$pbox_plot)},
                      "pca"=   {plot(vals$ppca_plot)},
                      "mds"= {plot(vals$pmds_plot)},
                      "rda"={plot(vals$rda_plot)},
                      "dp"={replayPlot(vals$plot_dp)},
                      "we"={replayPlot(vals$plot_we)},
                      "segrda"={plot(vals$seg_rda_plot)},

                      "Confusion Matrix RF"={plot(vals$cm_rf)},
                      "Confusion Matrix - test RF"={plot(vals$conf_rf)},
                      "Minimal Depth distribution"={plot(vals$rfd_res)},
                      "Multi-way importance"={plot(vals$rfm_res)},
                      "RF interactions"={plot(vals$rf_inter)},
                      "RF - Partial Dependence"=plot(vals$rfbiplot1),
                      "RF ranking comparations"=plot(rf_rank$df),
                      "RF measure comparations"=plot(rf_rel$df),
                      "RF - Partial Dependence (classes)"=plot(vals$rfbiplot2),
                      "florest plot"=replayPlot(vals$plot_florest),
                      'florest plot (class)'=plot(vals$plot_florest_class),


                      "NB - Confusion Matrix"={plot(vals$cm_nb)},
                      "NB - Variable Importance plot"=plot(vals$nb_varImp_plot),
                      "NB - Confusion Matrix (predictions)"=plot(vals$nb_cm_pred),
                      "NB - DM plot"=replayPlot(vals$nb_densplot),

                      "SVM - Confusion Matrix"={plot(vals$cm_svm)},
                      "SVM - Variable Importance plot"=plot(vals$svm_varImp_plot),
                      "SVM - Confusion Matrix (predictions)"=plot(vals$svm_cm_pred),
                      "SVM - DM plot"=replayPlot(vals$svm_densplot),

                      "knn - Confusion Matrix"={plot(vals$cm_knn)},
                      "knn - Variable Importance plot"=plot(vals$knn_varImp_plot),
                      "knn - Confusion Matrix (predictions)"=plot(vals$knn_cm_pred),
                      "knn - DM plot"=replayPlot(vals$knn_densplot),

                      "sgboost - Confusion Matrix"={plot(vals$cm_sgboost)},
                      "sgboost - Variable Importance plot"=plot(vals$sgboost_varImp_plot),
                      "sgboost - Confusion Matrix (predictions)"=plot(vals$sgboost_cm_pred),
                      "sgboost - DM plot"=replayPlot(vals$sgboost_densplot),

                      "som - Confusion Matrix"={plot(vals$cm_som)},
                      "som - Confusion Matrix (predictions)"=plot(vals$som_cm_pred),
                      "som (xyf)"=plot(vals$xyf_bmu),
                      "som counts (xyf)"=replayPlot(vals$xyf_counts),
                      "som changes (xyf)"=replayPlot(vals$xyf_changes),


                      "Training plot"={pchanges(vals$som_results)},
                      "Couting plot"={  pcounts(vals$som_results)},
                      "uMatrix"={   pUmatrix(vals$som_results)},
                      "BMUs"={plot(vals$bmus_plot)},
                      "BMUs predictions"={replayPlot(vals$bmus_pred_plot)},
                      "property plot"={replayPlot(vals$pprop_plot)},
                      "Confusion Matrix SOM"={plot(vals$conf_som)},


                      "feature_importance_models"={plot(vals$plot_ensemble)},
                      "ensemble_confusion_plot"={plot(vals$plot_ensemble)},
                      "ensemble_predictions_class_plot"={plot(vals$plot_ensemble)},
                      "ensemble_predictions_reg_plot"={plot(vals$plot_ensemble)},
                      "ensemble_feature_importance_plot"={plot(vals$plot_ensemble)},
                      "ensemble_interactions_plot"={plot(vals$plot_ensemble)}

                    )


                    dev.off()

                    list(src = paste0(name_c,".png",sep=""),
                         contentType = "image/png",
                         width = round((input$fwidth*as.numeric(input$fres))/2.54, 0),
                         height = round((input$fheight*as.numeric(input$fres))/2.54, 0),
                         alt = "plot")



                  }
    )

  },deleteFile=TRUE)

  observeEvent(input$fheight,{})
  observeEvent(input$fwidth,{})
  observeEvent(input$fres,{

  })
  observeEvent(input$pointsize,{})
  observeEvent(input$fformat,{})
  observeEvent(input$fig_preview,{

    vals$fig_preview<-input$fig_preview
  })

  output$out_fheight<-renderUI({})
  output$out_fwidth<-renderUI({

  })



  output$fig_specifications<-renderUI({
    splitLayout(

      numericInput(ns("fheight"), "Height (cm)", min=2,  step=1, value = vals$fheight),
      numericInput(ns("fwidth"), "Width (cm)", min=2,  step=1, value = vals$fwidth),
      numericInput(ns("fres"), "Res", value=vals$fres,step=50),
      numericInput(ns("pointsize"), "pointsize",value=vals$pointsize,step=1),
      selectInput(ns("fformat"), "File type", choices=c("png","tiff","jpeg","pdf","svg"),selected=vals$fformat),
      div(style="margin-top: 25px",class="save_changes",downloadButton(ns("bn_download"),"Download")),

    )
  })

  output$figview<-renderUI({
    dims<-vals$dimension_display



    div(tags$script(HTML(
      "$(document).on('shown.bs.modal','#shiny-modal', function () {
       Shiny.setInputValue(id = 'modal_visible', value = true);
      });
     $(document).on('hidden.bs.modal','#shiny-modal', function () {
       Shiny.setInputValue(id = 'modal_visible', value = false);
     });"
    )),
    div(uiOutput(ns('fig_specifications'))),
    div(uiOutput(ns('prevbutton'))),
    div( id="figview_body",
         div(style="margin: 4px",
             imageOutput(ns("plotoutput")))
    )

    )
  })





  modal_downfig<-function(){
    showModal({
      if(is.null(vals$fheight)){vals$fheight<-15}
      if(is.null(vals$fwidth)){vals$fwidth<-20}
      if(is.null(vals$fres)){vals$fres<-100}
      if(is.null(vals$pointsize)){vals$pointsize<-12}
      if(is.null(vals$fformat)){vals$fformat<-"pdf"}
      if(is.null(vals$fig_preview)){vals$fig_preview<-T}
      dims<-vals$dimension_display
      # req(length(dims)>0)
      xdim<-dims[1]*.65
      ydim=dims[2]*.6
      #ydim=dims[2]*xdim/dims[1]

      output$header<-renderUI({
        div(p(strong("action:"),"download", message),
            div(class="remove_modal", actionLink(ns("remove_modal"),"[x] close",style="position: absolute; top: 0px; right: 0px; padding: 10px; ")))
      })

      observeEvent(input$remove_modal,ignoreInit = T,{
        if(input$remove_modal%%2)
          removeModal()
      })

      tags$div(
        tags$div(id="figview_dialog",
                 modalDialog(
                   uiOutput(ns("figview")),
                   title=uiOutput(ns("header")),
                   footer=NULL,
                   easyClose = T

                 )
        ),

        tags$style(paste0(
          "#figview_dialog .modal-dialog  {
        width:",xdim,"px; height:",ydim,"px;

        }"
        )),
        tags$style(paste0(
          "#figview_dialog .modal-content > div,#figview_dialog .modal-body > div {
        min-width:",xdim,"px; min-height:",ydim,"px;background: white

        }"
        )),
        tags$style(paste0(
          "#figview_body  {background: white;
        width:",xdim,"px; height:",ydim,"px;
        max-width:",xdim,"px; max-height:",ydim,"px;
        min-width:",xdim,"px; min-height:",ydim,"px;
        }"
        ))
      )
    })
  }


  modal_downfig()



  output$prevbutton<-renderUI({
    #req(length(vals$hand_plot)>0)
    fheight<-input$fheight
    fwidth<-input$fwidth
    fres<-as.numeric(input$fres)
    div(
      div(checkboxInput(ns("fig_preview"),span("Render preview"),value=T)),

    )

  })





}
