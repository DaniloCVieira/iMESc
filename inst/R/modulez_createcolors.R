#' @export
#' @noRd

module_ui_cc<-function(id){
  ns<-NS(id)
  tagList(
    uiOutput(ns("palette_creation"))
  )

}

#' @export
# Server
module_server_cc<-function (input, output, session,vals,df_colors,newcolhabs){
  ns<-session$ns


  output$newpalette_created<-renderUI({
    req(!is.null(vals$created_pal))
    div(
      div(em("Palette successfully created"))

    )
  })


  output$custom_palette<-renderUI({
    pic<-which(!vals$colors_img$val%in%c(
      "matlab.like2",'viridis', 'plasma',"Rushmore1","FantasticFox1",'Grays',"heat",'Purples',"Blues",'Greens',"black","gray","royalblue", "firebrick","forestGreen",'goldenrod3',"white"
    ))
    req(length(pic)>0)
    div(
      inline(pickerInput(inputId = ns("available_palette"),
                         label = NULL,
                         choices = vals$colors_img$val[pic],
                         choicesOpt = list(content = vals$colors_img$img[pic]),  width="100px")),
      tipify_ui( actionButton(ns("delete_palette"),icon("far fa-trash-alt")),"Click to delete the selected palette")

    )})


  output$color_picker<-renderUI({
    if(is.null(vals$cur_col_cc)){vals$cur_col_cc<-"#3498DB"}
    tipify_ui(
      colourpicker::colourInput(
        ns("col"), NULL, vals$cur_col_cc,allowTransparent=T,
        closeOnClick=T,
      ),"Click to change color"
    )
  })

  output$palette_creation<-renderUI({
    div(

      inline(
        div(class="tools_content",
            div(
              div(class="cogs_in",
                  inline(div(
                    style="padding: 5px; height: 75px;width: 130px; align: center",
                    icon("fas fa-long-arrow-alt-right",verify_fa = FALSE),
                    "1. Pick a color:", class='tool_color',
                    uiOutput(ns('color_picker')))),
                  inline(div(
                    style="padding: 5px; height: 75px;width: 110px; align: center",
                    div("2. Add:", class='tool_color'),
                    tipify_ui(actionButton(ns("add_color"),icon("fas fa-level-down")),"Click to include the color")))
              ),
              div(style="margin-left: 70px",
                span(icon("fas fa-level-down fa-rotate-180",verify_fa = FALSE, style="width: 50px"),style="vertical-align: text-bottom;"),
                span(icon("fas fa-level-down fa-rotate-90",verify_fa = FALSE ),style="vertical-align: text-top;margin-left: 20px")
              )
            ),
            div(class="cogs_in",
                style="padding: 5px; height: 75px;width: 300px; align: center",
                uiOutput(ns('newpalette_img'))
            )
            ,
            div(style=" background:#FBEEE6;margin-top: 1px, width:220px; padding: 5px ",
                p(strong("Created palettes:")),
                uiOutput(ns("custom_palette"))
            )


            #verbatimTextOutput("newpalette_out"),


        )
      )
    )
  })




output$add_palette_button<-renderUI({
  req(length(vals$newpalette_colors)>0)
  tipify_ui(shinyBS::bsButton(ns("add_palette"),span(icon("fas fa-palette"),icon("fas fa-arrow-down"))),"Click to create a palette with the colors above. The new palette will be available in all color inputs")
})
  observeEvent(input$col,{
    vals$cur_col_cc<-input$col
  })

  observe({
    print(length(vals$newcolhabs))
  })

  output$newpalette_img<-renderUI({
    req(length(vals$newpalette_colors)>0)
   div(
     inline(
       div(style=" background: #D1F2EB ; padding: 5px;",
           div(class="tool_color","New palette"),
           div(inline(div(style="width: 140px",
                          renderPlot({
                            par(mar=c(0,0,0,0),mai=c(0,0,0,0),mgp=c(0,0,0),xpd=T,ann=F, bg=NA, fg=NA)
                            image(t(matrix(seq(0,1,length.out=100), nrow=1)), axes=F,col=  colorRampPalette(  vals$newpalette_colors)(100))
                          }, height =30)))
           )

       )
     ),
    inline(
      div(style="width: 135px",
        inline(
          div(
            div(class="tool_color","3. Create palette"),
            inline(uiOutput(ns('add_palette_button'))),
            tipify_ui(actionButton(ns("restart_palette"),span(icon("fas fa-undo"))),"Click to restart")

          )
        )
      )
    )
   )

  })
  output$maxcolor<-renderUI({
    req(maxcol()>=48)
    em("The Maximum number of colors allowed has been reached")
  })
  output$newpalette_out<-renderPrint({
    vals$newpalette_colors
  })
  maxcol<-reactive({
    max<-length(vals$newpalette_colors)
    if(max<10){max=10}
    max
  })
  observeEvent(input$add_color,{
    req(maxcol()<=48)
    vals$newpalette_colors<-c( vals$newpalette_colors,input$col)
  })
  observeEvent(input$add_palette,{
    validate(need(!is.null(vals$newpalette_colors),"Please use the arrow button to include the selected color in the new palette"))
    palette_name<-paste("palette",length(vals$newcolhabs))
    outfile<-tempfile(fileext = ".png")
    png(outfile, height =70)
    par(mar=c(0,0,0,0),mai=c(0,0,0,0),mgp=c(0,0,0),xpd=T,ann=F, bg=NA, fg=NA)
    image(t(matrix(seq(0,1,length.out=100), nrow=1)), axes=F,col=  colorRampPalette(vals$newpalette_colors)(100))
    dev.off()
    palette1<-base64enc::dataURI(file =outfile,mime = "image/png")
    vals$colors_img<-rbind(vals$colors_img,data.frame(val=palette_name,img= sprintf(paste0(img(src = palette1, height = '20',width = '70',style="margin-top:-40px;margin-bottom:-40px;margin-left:-20px;margin-right:-20px;") ))))
    vals$newcolhabs[[palette_name]]<-colorRampPalette(vals$newpalette_colors)
    vals$created_pal<-T
    updatePickerInput(session,'available_palette',selected = palette_name)
    vals$newpalette_colors<-NULL
  })
  observeEvent(input$restart_palette,{
    vals$newpalette_colors<-NULL
  })
  observeEvent(input$delete_palette,{
    pic<-which( names(vals$newcolhabs)==input$available_palette)
    vals$newcolhabs[[pic]]<-NULL
    pic<-which(vals$colors_img$val==input$available_palette)
    vals$colors_img<-vals$colors_img[-pic,]

  })



}
