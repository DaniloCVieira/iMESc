

module_ui_figs<- function(id){
  ns<-NS(id)
  uiOutput(ns("go"))
}



#' @export
module_server_figs<-function (input, output, session,vals,file="",datalist_name=T, message=NULL, name_c=NULL, generic=NULL,fun=NULL,args_plot=NULL,gif=FALSE,formats=c("png","tiff","jpeg","pdf","svg")){
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



  fn_download<-function()
  {
    vals$fres<-input$fres
    vals$fwidth<-input$fwidth
    vals$fheight<-input$fheight
    vals$pointsize<-input$pointsize
    vals$fformat<-input$fformat
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



    if(inherits(generic,'gtable')){
      return(grid::grid.draw(generic))
    }
    switch (
      vals$hand_plot,
      "plot_funcion"=do.call(fun,args_plot),
      "generic_ggmatrix"={print.ggmatrix(generic)},
      "generic_gg"={plot(generic)},
      "generic_replay"={replayPlot(generic)},
      "Feature plot"=plot(vals$fm_downplot),
      "EBNB"=plot(vals$plot_niche),
      'Confusion Matrix Post-Shuffling'=plot(vals$feature_cm_plot),
      'Correlation Plot'=replayPlot(vals$plot_correlation),
      'Corr plot'=replayPlot(vals$corplot),
      "k-means (pca reprentation)"= plot(vals$kmeans_plot_data),
      "k-means (codebook)"=plot(vals$psom_plot),
      "codebook clusters"=plot(vals$hc_tab4_plot),
      "Ridge plot"=plot(vals$rid_plot),
      "variable summary"={
        replayPlot(vals$varplot)
      },
      "Factor plot"={plot(vals$factorsplot)},
      "boxplot"={plot(vals$pbox_plot)},
      "mds"= {plot(vals$pmds_plot)},
      "rda"={plot(vals$rda_plot)},
      "dp"={replayPlot(vals$plot_dp)},
      "segrda"={plot(vals$seg_rda_plot)}
    )


  }






  output$bn_download<-downloadHandler(
    filename = fn_downloadf,
    content = function(file) {
      removeModal()

      fn_download()
      dev.off()

      if(isTRUE(vals$hw_changed)){
        vals$fheight<-20
        vals$fwidth<-15
        vals$hw_changed<-F
      }
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




  get_args_dims<-reactive({
    fheight<-input$fheight
    req(fheight)
    fwidth<-input$fwidth
    req(fwidth)
    c(fwidth,fheight)
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


                    fheight<-get_args_dims()[2]
                    fwidth<-get_args_dims()[1]
                    fres<-as.numeric(input$fres)
                    png(paste0(name_c,".png",sep=""), height=fheight, width=fwidth, res=fres, units="cm", pointsize = input$pointsize)
                    if(inherits(generic,'gtable')){
                      grid::grid.draw(generic)
                    } else{
                      switch(
                        vals$hand_plot,
                        "plot_funcion"=do.call(fun,args_plot),
                        "generic_ggmatrix"={print.ggmatrix(generic)},
                        "generic_gg"={plot(generic)},
                        "generic_replay"={replayPlot(generic)},
                        "Feature plot"=plot(vals$fm_downplot),
                        "EBNB"=plot(vals$plot_niche),
                        'Confusion Matrix Post-Shuffling'=plot(vals$feature_cm_plot),
                        'Correlation Plot'=replayPlot(vals$plot_correlation),
                        'Corr plot'=replayPlot(vals$corplot),
                        "k-means (pca reprentation)"= plot(vals$kmeans_plot_data),
                        "k-means (codebook)"=plot(vals$psom_plot),
                        "codebook clusters"=plot(vals$hc_tab4_plot),
                        "Ridge plot"=plot(vals$rid_plot),
                        "variable summary"={
                          replayPlot(vals$varplot)
                        },
                        "Factor plot"={plot(vals$factorsplot)},
                        "boxplot"={plot(vals$pbox_plot)},
                        "mds"= {plot(vals$pmds_plot)},
                        "rda"={plot(vals$rda_plot)},
                        "dp"={replayPlot(vals$plot_dp)},
                        "segrda"={plot(vals$seg_rda_plot)}
                      )
                    }



                    dev.off()

                    list(src = paste0(name_c,".png",sep=""),
                         contentType = "image/png",
                         width = "auto",   # Scale it to fit the modal
                         height = "100%" ,
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


  output$out_fheight<-renderUI({})
  output$out_fwidth<-renderUI({

  })



  output$fig_setup<-renderUI({
    dims<-vals$dimension_display

    div(
      tags$script(HTML(
        "$(document).on('shown.bs.modal','#shiny-modal', function () {
       Shiny.setInputValue(id = 'modal_visible', value = true);
      });
     $(document).on('hidden.bs.modal','#shiny-modal', function () {
       Shiny.setInputValue(id = 'modal_visible', value = false);
     });"
      )),
      div(
        box_caret(
          ns("box_fig"),
          title="Data setup",
          color="#374061ff",
          inline=F,
          show_tittle = F,
          div(
            div( class="picker-flex fig_setup",style="display: flex; gap: 5px",class="spatial_tools spatial_setup",
                 numericInput(ns("fheight"), strong("Height (cm)"), min=2,  step=1, value = vals$fheight),
                 numericInput(ns("fwidth"), strong("Width (cm)"), min=2,  step=1, value = vals$fwidth),
                 numericInput(ns("fres"), strong("Resolution",tipright("The nominal resolution in ppi which will be recorded in the bitmap file")), value=vals$fres,step=50),
                 numericInput(ns("pointsize"), strong("Pointsize",tipright("The units in which height and width are given")),value=vals$pointsize,step=1),
                 pickerInput(ns("fformat"), strong("File type"), choices=formats,selected=vals$fformat),
            ),
            div(style="display: flex; align-items: center; gap:3px;",
                class="fig_tools",


            )
          )
        )
      )
    )
  })


  output$figview<-renderUI({
    box_caret("box_fig",
              title=NULL,
              show_tittle = T,
              button_title = div(style="display: inline-block",class="fig-btn-down save_changes",downloadButton(ns("bn_download"),"Download")),
              ab = inline(div(
                style="display: flex; align-items: center; gap:3px;",
                class="fig_tools",
                div(strong("Preview"),style="margin-left: 15px; margin-right: 10px"),
                uiOutput(ns("btn_view")),
                uiOutput(ns("btn_zoom")),
              )),
              div(class="figview_body",
                  imageOutput(ns("plotoutput")))
    )

  })



  observe({
    if(is.null(vals$fheight)){vals$fheight<-15}
    if(is.null(vals$fwidth)){vals$fwidth<-20}
    if(is.null(vals$fres)){vals$fres<-100}
    if(is.null(vals$pointsize)){vals$pointsize<-12}
    if(is.null(vals$fformat)){vals$fformat<-"png"}
  })
  observeEvent(input$remove_modal,ignoreInit = T,{
    if(input$remove_modal%%2)
      removeModal()
  })
  output$header<-renderUI({
    div(p(strong("action:"),"download", message),
        div(class="remove_modal", actionLink(ns("remove_modal"),"[x] close",style="position: absolute; top: 0px; right: 0px; padding: 10px; ")))
  })

  observeEvent(input$zoom_full,{
    vals$zoom_full<-input$zoom_full
  })


  observeEvent(input$zoom_full,{
    update
  })



  observe({
    if(is.null(vals$zoom_full)){
      vals$zoom_full<-F
    }
  })
  output$btn_zoom<-renderUI({

    div(
      actionButton(ns("toggle_zoom"), div(
        class="i_zoom_full0 zoom_full",tip="Preview at final size",
        icon(NULL, class="fas fa-solid fa-compress-arrows-alt")
      )),
      tags$style(HTML(
        "


div[tip]:hover:after{
content: attr(tip);

color: white;
position: absolute;
width: fit-content;
top: 23px;
left: 15px;
display: block;
z-index: 9999999;
background-color: MidnightBlue;
font-size: 12px;
padding: 3px;
padding-left: 10px;
padding-right: 10px;
border-radius: 3px 3px 3px 3px;
}
.i_zoom_full0 {
position: relative
}



        "
      )),

      tags$script(HTML(paste0("
        let scale = 1;  // Initial scale value
        let isFullSize = false;  // Track the zoom state (false = fit to screen, true = full size)

        // Scroll-based zooming
        document.addEventListener('wheel', function(event) {
          const image = document.querySelector('.figview_body img');
          if (image) {
            event.preventDefault();
            if (event.deltaY < 0) {
              // Scrolling up, zoom in
              scale += 0.5;
            } else {
              // Scrolling down, zoom out
              scale -= 0.5;
              if (scale < 1) scale = 1;  // Prevent zoom out below original size
            }
            image.style.transform = 'scale(' + scale + ')';
          }
        });

        // Toggle zoom when the button is clicked
        document.getElementById('",ns('toggle_zoom'),"').addEventListener('click', function() {
          const image = document.querySelector('.figview_body img');
          if (image) {
            if (isFullSize) {
              // Reset to initial scale
              scale = 1;
              image.style.transform = 'scale(1)';
              // Update button icon and toggle state
              document.getElementById('",ns('toggle_zoom'),"').innerHTML = '<div class=\"i_zoom_full0\"><div  class=\"i_zoom_full\"  tip=\"Preview at final size\"><i class=\"fas fa-solid fa-compress-arrows-alt\"></i></div></div>';
              isFullSize = false;
            } else {
              // Zoom to full size
              const originalWidth = image.naturalWidth;
              const displayedWidth = image.clientWidth;
              const fullScale = originalWidth / displayedWidth;

              scale = fullScale;
              image.style.transform = 'scale(' + fullScale + ')';
              // Update button icon and toggle state
              document.getElementById('",ns('toggle_zoom'),"').innerHTML = '<div class=\"i_zoom_full0\"><div  class=\"i_zoom_expand\" tip=\"Preview fitting to screen\"><i class=\"fas fa-solid fa-expand\"></i></div></div>';
              isFullSize = true;
            }
          }
        });
      ")))
    )
  })
  modal_downfig<-function(){
    showModal({
      tags$div(
        tags$div(class="figview_dialog",
                 modalDialog(

                   div(
                     tags$head(tags$style(HTML(
                       ".figview_body  {background: white;
          height: calc(100vh - 240px);

margin: 4px; overflow-y: auto; overflow-x: auto;

          }
.fig_setup .form-control {
height: 30px;
border-radius: 0px;
}
  .box_title .fig_tools .btn {
height: 20px;
width: 30px;
font-size: 13px;
 # color: #696969;
 }
  .box_title .fig_tools .btn i{
 color: #696969;
padding-top: 2px;
font-size: 13px
 }

.fig-btn-down .btn  {
width: 100px;
margin-top: -2px;
margin-right: -3px;
box-sizing: content-box;
border:0px solid;
padding: 3px;
height: 20px;
}


"
                     ))),
                     uiOutput(ns('fig_setup')),
                     uiOutput(ns("figview")),

                   ),
                   title=uiOutput(ns("header")),

                   tags$style(HTML("
        .figview_body img {
          transition: transform 0.05s ease;  /* Smooth zoom transition */
          transform-origin: center center;   /* Zoom from the center */
        }
      ")),

                   easyClose = T

                 )
        )
      )
    })
  }



modal_downfig()

observeEvent(input$fig_preview,{
  vals$fig_preview<-input$fig_preview
})
icon_showfig<-reactive({
  i<-if(isFALSE(vals$fig_preview)){
    icon(NULL,class='fas fa-solid fa-eye-slash')
  } else{
    icon(NULL,class='fas fa-solid fa-eye')
  }
  div(class='i_zoom_full0',i,tip="Toggle preview")
})
output$btn_view<-renderUI({
  bsButton(ns("fig_preview"),icon_showfig(),type="toggle",value=vals$fig_preview,class="background: red")
})

observeEvent(input$fig_preview,{
  shinyjs::toggle(selector=".figview_body",condition=isTRUE(vals$fig_preview))
  shinyjs::toggle("btn_zoom",condition=isTRUE(vals$fig_preview))
})
observe({
  if(is.null(vals$fig_preview)){vals$fig_preview<-T}
})







}
