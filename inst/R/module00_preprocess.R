ui_base<-function(id){
  ns<-NS(id)
  div(

  )
}
server_base<-function(id,data=NULL, vals=NULL){
  moduleServer(id,function(input,output,session){

  })
}
create<-tool2<-pp_data<-toolbar<-tool3<-tool4<-tool5<-tool6<-tool7<-tool8<-tool9<-tool10<-list()
confirm_changes<-list()
confirm_changes$ui<-function(id, message=""){
  ns<-NS(id)
  showModal(
    modalDialog(
      easyClose = F,
      footer=div(actionButton(ns("cancel_changes"),"Cancel"),actionButton(ns("confirm"),"Proceed")),
      div(class = "alert_warning",
          p(message)
      )
    )
  )
}
confirm_changes$server<-function(id){
  moduleServer(id,function(input,output,session){
    return(reactive(input$confirm))})}
confirm_changes$server_cancel<-function(id){
  moduleServer(id,function(input,output,session){
    return(reactive(input$cancel_changes))})}
done_modal<-function(){
  showModal(
    modalDialog(
      easyClose = T,
      size="s",
      title=NULL,
      h4(emgreen("Success!"))
    )
  )
}



quant<-list()
quant$ui<-function(id, label="Show Pre/Post Quantiles"){
  ns<-NS(id)
  div(
    checkboxInput(ns("show_print_impute"),label,F)
  )
}
quant$server<-function(id, data1,data2,height="120px",width="400px",fun='print_transf',d1_before=NULL,d2_before=NULL, colored=F,coords =matrix(1:2,2,2)){
  moduleServer(id,function(input,output,session){



    output$data_table<-renderUI({
      req(isTRUE(input$show_print_impute))
      if(fun=="print_transf"){
        div(class="half-drop-inline",
            fixed_dt(print_transf(data1,data2),height,scrollX = width)
        )
      } else{
        div(class="half-drop-inline",style="background: white",
            div(d1_before),
            fixed_dt(data1,height,colored=colored,coords=coords,scrollX=width,
                     pageLength = 10,dom="tp"),
            div(d2_before,style="margin-top: 15px"),
            fixed_dt(data2,height,scrollX=width)
        )
      }

    })
  })
}
tool1<-list()
tool1$ui<-function(id){
  ns<-NS(id)
  labels_create<-list(

    span(
      strong("Numeric-Attribute:",style="color:  SeaGreen"),
      popify(actionLink(ns('uphelp'), icon("fas fa-question-circle")),"Upload the observations",textupload(),"right"),
    ),
    span(style="color:  #05668D",
         strong("Factor-Attribute:"),
         popify(actionLink(ns('labhelp'), icon("fas fa-question-circle")),"Upload the factors",textlab(),"right")),
    span(style="color:  #05668D",
         strong(span("*"),"Coords-Attribute:"),
         popify(actionLink(ns('cohelp'), icon("fas fa-question-circle")),"Upload the coordinates", textcoords(),"right")),
    span(style="color:  #05668D",
         strong(span("*"),"Base shape:",actionLink(ns("basehelp"), tipify(icon("fas fa-question-circle"),"Click for more details")))),

    span(style="color:  #05668D",
         strong(span("*"),"Layer shape:",actionLink(ns("layerhelp"), tipify(icon("fas fa-question-circle"),"Click for more details"))))

  )
  tags$div(
    class='dl_modal',
    modalDialog(
      div(
        div(style="background: white; margin-left: 20px;padding-top: 10px",class="half-drop dl",
            div(id=ns("dl_page1"),
                h4("Create Datalist",
                   popify(actionLink("uphelp0",icon("fas fa-question-circle")),"Data bank structure",as.character(paste0("Each Datalist in the Databank requires at least one file for the Numeric-Attribute. If a Factor-Attribute is not provided, iMESc automatically generates this attribute as a single-column sheet containing the IDs of the Numeric-Attribute.")),trigger="hover")),

                div(class="insert_radio",
                    radioButtons(ns("up_or_ex"),NULL,choiceValues  = list("upload", "example"),choiceNames   = list(tipify(strong("Upload"),"upload your own data",placement = "bottom", options=list(container="body")), tipify(strong("example"),"Use Nematode datasets from Araca Bay as example",placement = "bottom", options=list(container="body"))),selected = "upload", inline = T)),
                div(class="insert_radio_border_bottom", style="margin-bottom: 10px"),
                div(id=ns("dl_create"),
                    div(class="dl_page",
                        div(class='create_req create_dl to_shake',style="display: flex",
                            id=ns('dl_name'),
                            div(div(class="mlb-wide")
                            ),
                            textInput(ns("data_name"),span('Name the Datalist',style="color: SeaGreen"), value=NULL, width= '320px'),uiOutput(ns("war_dlname"))
                        ),
                        div(class="create_dl",
                            div(
                              class='create_req',
                              div(style="display: flex;",
                                  div(class="mlb mlb-wide",
                                      div(
                                        style="position: absolute; left: 0px;
                color:SeaGreen; margin-top: -30px; margin-left: 30px",
                strong("Required"),
                                      )
                                  ),
                div(class="mlb"),
                fileInput(ns("filedata"),label = labels_create[1],accept = c(".csv",".xlsx",".xls")),
                div(class="sheet_dl",
                    hidden(pickerInput(ns("sheet_data"),"Sheet:", choices=NULL))
                ),
                actionLink(ns("reset_insert_1"),icon("fas fa-undo"),class="btn-input")
                              )
                            ),
                div(class='create_opt',style="padding-top: 10px",
                    div(style="display: flex",
                        div(class="mlb-wide mblue"),
                        div(
                          style="position: absolute; left: 0px;
                color:#05668D; margin-top: -55px; margin-left: 30px",
                strong("Optional"),
                        ),
                div(class="mlb mblue"),
                fileInput(ns("labels"), labels_create[2], accept = c(".csv",".xlsx")),
                div(class="sheet_dl",
                    hidden(pickerInput(ns("sheet_fac"),"Sheet:", choices=NULL))
                ),
                actionLink(ns("reset_insert_2"),icon("fas fa-undo"),class="btn-input")
                    ),
                div(style="display: flex",
                    div(class="mlb-wide"),
                    div(
                      style="position: absolute; left: 0px;width: 120px;
                color:gray; margin-top: -25px; margin-left: 30px;white-space: normal",
                em("*Required for the spatial tools menu"),
                    ),
                div(class="mlb mblue"),
                fileInput(ns("coords"), labels_create[3], accept = c(".csv",".xlsx")),
                div(class="sheet_dl",
                    hidden(pickerInput(ns("sheet_coord"),"Sheet:", choices=NULL))
                ),
                actionLink(ns("reset_insert_3"),icon("fas fa-undo"),class="btn-input")
                ),
                div(style="display: flex",
                    div(class="mlb-wide"),
                    div(class="mlb mblue"),

                    fileInput(ns("base_shape"),labels_create[4]),
                    actionLink(ns("reset_insert_4"),icon("fas fa-undo"),class="btn-input")
                ),
                div(style="display: flex",
                    div(class="mlb-wide"),
                    div(class="mlb mblue"),
                    fileInput(ns("layer_shape"),labels_create[5]),
                    actionLink(ns("reset_insert_5"),icon("fas fa-undo"),class="btn-input")
                )
                ))
                    )),
                div(id=ns("dl_example"),style="display: none",
                    div(class="dl_page",
                        div(class='create_req create_dl',style="display: flex",
                            div(div(class="mlb-wide")),
                            div(class="form-group shiny-input-container",
                                tags$label("Name the Datalist"),
                                div(class="form-control fake_dl",
                                    "nema_araca/envi_araca",style="width: 320px; ;padding: 7px"))
                        ),
                        div(class="create_dl",
                            div(class='create_req',
                                div(style="display: flex;",
                                    div(class="mlb mlb-wide",div(style="position: absolute; left: 0px;color:SeaGreen; margin-top: -30px; margin-left: 30px",strong("Required"),)
                                    ),
                                    div(class="mlb"),
                                    div(class="form-group shiny-input-container",
                                        tags$label("Numeric-Attribute"),
                                        div(class="form-control fake_dl","nematode/abiotic data from Araca Bay, Brazil",style="width: 320px; ;padding: 7px; color: SeaGreen")
                                    )
                                )
                            ),
                            div(class='create_opt',style="padding-top: 10px",
                                div(style="display: flex",
                                    div(class="mlb-wide mblue"),
                                    div(style="position: absolute; left: 0px;
                color:#05668D; margin-top: -55px; margin-left: 30px",
                strong("Optional")),
                div(class="mlb mblue"),
                div(class="form-group shiny-input-container",
                    tags$label("Factor-Attribute"),
                    div(class="form-control fake_dl","sampling factors for both Datalists",style="width: 320px; ;padding: 7px; color: #05668D"))
                                ),
                div(style="display: flex",
                    div(class="mlb-wide"),
                    div(style="position: absolute; left: 0px;width: 120px;color:gray; margin-top: -25px; margin-left: 30px;white-space: normal",em("*Required for the spatial tools menu")),
                    div(class="mlb mblue"),
                    div(class="form-group shiny-input-container",
                        tags$label("Coords-Attribute"),
                        div(class="form-control fake_dl","sampling coordinates for both Datalists",style="width: 320px; ;padding: 7px; color: #05668D")
                    )
                ),
                div(style="display: flex",
                    div(class="mlb-wide"),
                    div(class="mlb mblue"),
                    div(class="form-group shiny-input-container",
                        tags$label("Base shape"),
                        div(class="form-control fake_dl","base shape of the Araca Bay, for both Datalists",style="width: 320px; ;padding: 7px; color: #05668D")
                    )
                ),
                div(style="display: flex",
                    div(class="mlb-wide"),
                    div(class="mlb mblue"),
                    div(class="form-group shiny-input-container",
                        tags$label("Layer shape"),
                        div(class="form-control fake_dl","layer shape of the Araca Bay, for both Datalists",style="width: 320px; ;padding: 7px; color: #05668D"))
                )
                            )
                        )
                    )
                )

            ),


            div(id=ns("dl_page2"),style="display: none",
                uiOutput(ns('insert_page2'))

            ),
            div(id=ns("dl_page3"),style="display: none",
                "Page3"

            ),

            tags$style(HTML("
      @keyframes shake {
        0% { transform: translate(1px, 1px) rotate(0deg); }
        10% { transform: translate(-1px, -2px) rotate(-1deg); }
        20% { transform: translate(-3px, 0px) rotate(1deg); }
        30% { transform: translate(3px, 2px) rotate(0deg); }
        40% { transform: translate(1px, -1px) rotate(1deg); }
        50% { transform: translate(-1px, 2px) rotate(-1deg); }
        60% { transform: translate(-3px, 1px) rotate(0deg); }
        70% { transform: translate(3px, 1px) rotate(-1deg); }
        80% { transform: translate(-1px, -1px) rotate(1deg); }
        90% { transform: translate(1px, 2px) rotate(0deg); }
        100% { transform: translate(1px, -2px) rotate(-1deg); }
      }
      .shake {
        animation: shake 0.5s;
        animation-iteration-count: 1;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('shakeClass', function(message) {
        var elements = document.getElementsByClassName(message.className);
        Array.prototype.forEach.call(elements, function(element) {
          element.classList.add('shake');
          element.addEventListener('animationend', function() {
            element.classList.remove('shake');
          });
        });
      });
    "))

        ))
    ,
    footer=div(
      div(
        class="dl_btns",style='display: inline-block',
        actionButton(ns("dl_cancel"),"Cancel"),
        hidden(actionButton(ns("dl_prev"),"< Previous")),
        inline(uiOutput(ns("insert_buttons"))),

      )
    ),
    easyClose = T
    )

  )

}
tool1$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    curpage<-reactiveVal(1)



    observeEvent(input$data_name,{
      req(input$data_name!="")
      shinyjs::removeClass(id="dl_name",class="alert_warning")
    })

    observe({
      req(input$filedata$datapath)
      req(input$up_or_ex != 'example')
      shinyjs::toggleClass(id="dl_name",class="alert_warning",condition=input$data_name=="")
    })
    observeEvent(input$dl_next,ignoreInit = T,{
      req(input$filedata$datapath)
      req(input$up_or_ex != 'example')
      req(input$data_name=="")
      session$sendCustomMessage(type = 'shakeClass', message = list(className = "to_shake"))

      output$war_dlname<-renderUI({
        req(input$data_name=="")

        div(align="left",
            class = "alert_warning", style="padding-left: 20px;",
            div(strong(icon("triangle-exclamation",style="color: Dark yellow3"),"Warning:")),
            div("Datalist name cannot be empty")
        )

      })
    })





    output$insert_buttons<-renderUI({
      validate_data()
      ns<-session$ns
      div(

        actionButton(ns("dl_next"),"Next >"),
        hidden(actionButton(ns("dl_insert"),strong("Insert Datalist >")))
      )
    })
    observeEvent(input$dl_insert,ignoreInit = T,{
      curpage(1)
      datalist<- getdatalist()

      vals$saved_data[[length(vals$saved_data)+1]]<-datalist[[1]]

      names(vals$saved_data)[length(vals$saved_data)]<-input$data_name
      if(input$up_or_ex == 'example'){
        names(vals$saved_data)[length(vals$saved_data)]<-"nema_araca"
        vals$cur_data<-names(vals$saved_data)[length(vals$saved_data)]
        envi <-getdatalist_envi()[[1]]
        envi<-data_migrate(datalist[[1]],envi,"envi_araca")
        vals$saved_data[[length(vals$saved_data)+1]]<-envi
        names(vals$saved_data)[[length(vals$saved_data)]]<-"envi_araca"
      } else{
        vals$cur_data<-names(vals$saved_data)[length(vals$saved_data)]
      }
      names(vals$saved_data)<-make.unique(names(vals$saved_data))

      vals$newdata<-T
      removeModal()

      curpage(1)
    })


    name_dl<-reactiveVal()
    file_data<-reactiveVal()
    file_coords<-reactiveVal()
    file_factors<-reactiveVal()
    file_base<-reactiveVal()
    file_layer<-reactiveVal()

    observeEvent(name_dl(),{
      updateTextInput(session,"data_name",value=name_dl())

    })

    observeEvent(input$filedata$datapath,{
      file_data(input$filedata$datapath)
      name_dl(gsub("\\.csv|\\.xls|\\.xlsx","",input$filedata$name))
    })

    observeEvent(input$coords$datapath,{
      file_coords(input$coords$datapath)
    })

    observeEvent(input$labels$datapath,{
      file_factors(input$labels$datapath)
    })
    observeEvent(input$base_shape$datapath,{
      file_base(input$base_shape$datapath)
    })
    observeEvent(input$layer_shape$datapath,{
      file_layer(input$layer_shape$datapath)
    })
    observeEvent(input$reset_insert_1,{
      file_data(NULL)
      name_dl(NULL)
      shinyjs::reset('filedata')
      shinyjs::hide('sheet_data')

    })

    observeEvent(input$reset_insert_2,{
      file_factors(NULL)
      shinyjs::reset('labels')
      shinyjs::hide('sheet_fac')
    })
    observeEvent(input$reset_insert_3,{
      file_coords(NULL)
      shinyjs::reset('coords')
      shinyjs::hide('sheet_coord')
    })
    observeEvent(input$reset_insert_4,{
      file_base(NULL)
      shinyjs::reset('base_shape')
    })
    observeEvent(input$reset_insert_5,{
      file_layer(NULL)
      shinyjs::reset('layer_shape')
    })
    observe({
      req(file_data())
      condition=grepl("xls",file_data())>0
      shinyjs::toggle('sheet_data',condition=condition)
    },suspended =T)
    observe({
      req(input$label$datapath)
      condition=grepl("xls",input$label$datapath)>0
      shinyjs::toggle('sheet_fac',condition=condition)
    },suspended =T)
    observe({
      req(file_coords())
      condition=grepl("xls",file_coords())>0
      shinyjs::toggle('sheet_coord',condition=condition)
    },suspended =T)
    observeEvent(input$filedata,{
      req(grepl("xls",file_data()))
      choices_data<-excel_sheets(path = file_data())
      updatePickerInput(session,"sheet_data", choices=choices_data)
    })

    observeEvent(input$up_or_ex,{
      shinyjs::toggle('dl_create',condition=input$up_or_ex=="upload")
      shinyjs::toggle('dl_example',condition=input$up_or_ex=="example")
    })

    observeEvent(input$dl_next,ignoreInit = T,{
      if(input$up_or_ex!="example")
        req(input$data_name!="")
      req(curpage()<3)
      curpage(curpage()+1)
    })
    observeEvent(input$dl_prev,ignoreInit = T,{
      req(curpage()>1)
      curpage(curpage()-1)

    })

    observeEvent(curpage(),{
      if(curpage()==1){
        getdatalist(NULL)
      }
    })

    observeEvent(input$dl_cancel,ignoreInit = T,{
      removeModal()
    })


    output$validate_data<-renderUI({
      validate_data()
      NULL
    })
    observeEvent(curpage(),{
      shinyjs::toggle('dl_next',condition= curpage()==1)
      shinyjs::toggle('dl_prev',condition=curpage()>1)
      shinyjs::toggle('dl_cancel',condition=curpage()==1)
      shinyjs::toggle('dl_insert',condition=curpage()>1)

      shinyjs::toggle('dl_page1',condition=curpage()==1)
      shinyjs::toggle('dl_page2',condition=curpage()==2)
      shinyjs::toggle('dl_page3',condition=curpage()==3)



    })





    validate_data<-reactive({
      req(input$up_or_ex)
      if(input$up_or_ex=="example"){
        return(TRUE)
      }
      path<-file_data()
      req(path)
      if(grepl(".csv",path)){
        data<-data.frame(fread(path, stringsAsFactors = T,na.strings=c("","NA"),header=T,select=1))
        validate(need(!any(duplicated(data[, 1])),"Duplicate values not allowed in the first column (as they will be used as observation IDs)."))
      } else{
        sheet<-input$sheet_data
        if(is.null(sheet)){
          sheets<-excel_sheets(path =path)
          sheet=sheets[1]
        }
        df<-data.frame(read_excel(path, sheet,na=c("","NA"),col_names =F, range = readxl::cell_cols(1)))
        validate(need(!any(duplicated(df[, 1])),"Duplicate values not allowed in the first column (as they will be used as observation IDs)."))
      }
      TRUE
    })

    dataraw<-reactive({
      path<-"inst/www/nema_araca.csv"
      if(input$up_or_ex=='upload'){

        path<-file_data()
      }
      if(length(path)>0)
        imesc_data(path,input$sheet_data,"Numeric")

    })
    read_labels<-reactive({
      datao<-dataraw()
      path<-"inst/www/factors_araca.csv"
      if(input$up_or_ex=='upload'){
        path<-file_factors()
      }
      if(length(path)>0)
        imesc_data(path,input$sheet_fac,"Factors")[rownames(datao),, drop=F]
    })
    read_coords<-reactive({
      datao<-dataraw()
      path<-"inst/www/coords_araca.csv"
      if(input$up_or_ex=='upload'){
        path<-file_coords()
      }
      if(length(path)>0)
        imesc_data(path,input$sheet_coord,"Coords")[rownames(datao),,drop=F]
    })
    read_base<-reactive({
      if (input$up_or_ex=="example") {
        readRDS("inst/www/base_shape_araca.rds")
      } else {
        if(length(file_base())>0){
          t<-try({get(gsub(" ", "", capture.output(
            load(file_base(), verbose = T)
          )[2]))})
          if("try-error" %in% class(t)) {t<-readRDS(file_base()) }
          t

        }else{NULL}

      }
    })
    read_layer<-reactive({

      if (input$up_or_ex=="example"){
        readRDS("inst/www/layer_shape_araca.rds")
        #get(gsub(" ","",capture.output(load("0006_layer_shape",verbose=T))[2]))
      } else {
        if(length(file_layer())>0){
          t<-try({get(gsub(" ", "", capture.output(
            load(file_layer(), verbose = T)
          )[2]))})
          if("try-error" %in% class(t)) {t<-readRDS(file_layer()) }
          t

        }else{NULL}

      }})
    getdatalist_envi<-reactive({

      path<-"inst/www/envi_araca.csv"
      data<-imesc_data(path,input$sheet_data,"Numeric")
      d1<-create_DATALIST(
        up_or_ex= input$up_or_ex,
        data,
        factors=read_labels(),
        coords=read_coords(),
        base_shape=read_base(),
        layer_shape=read_layer(),
        name_example='envi_araca',
        data_name=input$data_name)
      attr(d1[[1]],"datalist")<-'envi_araca'
      d1
    })



    getdatalist<-reactiveVal()
    observeEvent(input$dl_next,{

      d1<-create_DATALIST(
        input$up_or_ex,
        data=dataraw(),
        factors=read_labels(),
        coords=read_coords(),
        base_shape=read_base(),
        layer_shape=read_layer(),
        name_example='nema_araca',
        data_name=input$data_name
      )
      getdatalist(d1)

    })


    output$insert_page2<-renderUI({

      coords<-attr(getdatalist()[[1]],"coords")
      if(!is.null(coords)){
        validate(need(ncol(coords)==2, "Cannot proceed: The coordinates sheet has more columns than allowed. It should only contain IDs followed by 2 columns: longitude and latitude in decimal format. Please correct or remove the coordinates sheet"))
        validate(need(is.numeric(coords[,1]), "Cannot proceed: The longitude column in the coordinates table is not numeric. Please correct or remove the coordinates sheet."))
        validate(need(is.numeric(coords[,2]), "Cannot proceed: The latitude column in the coordinates table is not numeric. Please correct or remove the coordinates sheet."))

      }

      if(input$up_or_ex=="example"){
        splitLayout(
          datalist_render(getdatalist()[[1]],F,width="60px"),
          datalist_render(getdatalist_envi()[[1]],F,width="60px")
        )
      } else{
        div(style="width: 50%",
            datalist_render(getdatalist()[[1]],F,width="60px")
        )
      }

    })

  })
}

tool2_tab1<-list()
tool2_tab1$ui<-function(id){
  ns<-NS(id)
  div(style="display: flex",

      div(
        p(strong("Rename & Reorder Datalist"),tiphelp("Rename Datalists or reorder them by dragging and dropping.")),
        div( style="overflow-y: auto; max-height: calc(100vh - 250px)",
             uiOutput(ns("rename_page")))
      ),
      div(class="half-drop",
          actionButton(ns("run_rename"),"Apply",icon=icon("sync"))
      ),


  )
}
tool2_tab1$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    output$rename_page<-renderUI({

      ns<-session$ns
      choices<-names(vals$saved_data)
      div(
        rank_list(
          labels=lapply(seq_along(vals$saved_data),function(i){
            span(style="display: flex",class="half-drop",span(i,style="font-size: 0px; color: transparent"),inline(textInput(paste0(ns("newname_datalist"),i),NULL,choices[i])))
          } ),
          input_id = ns("ord_saved_data"),
          class="saved_data_rename"
        )

      )

    })
    observeEvent(input$run_rename,ignoreInit = T,{

      newnames<- sapply(seq_along(vals$saved_data),function(i){
        input[[paste0("newname_datalist",i)]]
      } )
      ord<- input$ord_saved_data
      saved_data<-vals$saved_data
      names(saved_data)<-newnames
      saved_data<-saved_data[as.numeric(ord)]
      vals$saved_data<-saved_data
      done_modal()
    })
  })
}
tool2_tab2<-list()
tool2_tab2$ui<-function(id){
  ns<-NS(id)
  div(style="display: flex",
      div(
        p(strong("Merge Datalists by rows or columns")),
        radioButtons(ns("mergeby"),"Merge by:",choiceValues =c("col","row"),choiceNames  =c("Columns","Rows"), inline=T),
        div(class="virtual-180",
            virtualPicker(ns("tomerge"), label="Select the Datalists","selected Datalists")
        ),
        uiOutput(ns('update_tab2'))
      ),
      div(
        div(id=ns("run_merge_tip"),em("Select at least two Datalists", style='color: gray')),
        hidden(div(id=ns("run_merge_btn"),class="save_changes",
                   bsButton(ns("run_merge"), "Merge >")
        )),
        div(checkboxInput(ns("fill"), span("Fill missings",tiphelp("Fills missing columns with NAs. If not checked, restric data to common columns/rows from the selected Datalists")),T))
      )
  )
}
tool2_tab2$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns
    output$update_tab2<-renderUI({
      shinyWidgets::updateVirtualSelect(
        "tomerge",
        choices=names(vals$saved_data),
        selected=names(vals$saved_data)[1:2]
      )
      NULL
    })

    action<-reactive(paste("Merged by",input$mergeby))


    observeEvent(input$run_merge,ignoreInit = T,priority = 0,{


      showModal(
        modalDialog(
          title="Save changes",
          easyClose =T,
          footer=div(actionButton(ns("data_confirm"),strong("confirm")),
                     modalButton("Cancel")),
          div(style="padding: 20px",
              div(class="half-drop",
                  style="display: flex",
                  div(style="width: 20%",
                      radioButtons(ns("create_replace"),
                                   NULL,
                                   c(
                                     "Create"
                                     #,"Replace"
                                   ))
                  ),
                  div(style="padding-top: 10px",
                      uiOutput(ns("out_newdatalit")),
                      uiOutput(ns("out_overdatalist"))
                  )

              ),
              div(style="padding-left: 30px",
                  uiOutput(ns("message")),
                  uiOutput(ns('newdata')),

              )

          )

        )
      )


    })
    getnewdata<-reactive({

      req(r_merge())
      res<-r_merge()
      attr(res,"bag")<-"Merged"
      res
    })
    message<-reactiveVal()
    output$newdata<-renderUI({
      req(getnewdata())
      basic_summary2(getnewdata())
    })
    output$out_newdatalit<-renderUI({
      req(input$create_replace)
      req(input$create_replace=="Create")
      req(getnewdata())
      bag<-attr(getnewdata(),"bag")

      newnames<-make.unique(c(names(vals$saved_data),bag))
      name0<-newnames[length(newnames)]
      textInput(ns("newdatalit"),NULL,name0)
    })
    output$out_overdatalist<-renderUI({

      req(input$create_replace=="Replace")
      selectInput(ns("overdatalist"),NULL,choices=names(vals$saved_data),selected=vals$cur_data)
    })
    datalistnew<-reactive({
      req(input$create_replace)
      #switch(input$create_replace,'Create'={input$newdatalit},'Replace'={ input$overdatalist})
      input$newdatalit
    })
    output$message<-renderUI({
      # message()
    })
    observeEvent(input$data_confirm,ignoreInit = T,{
      req(datalistnew())
      req(vals$saved_data)

      vals$saved_data[[datalistnew()]]<-getnewdata()
      removeModal()
      done_modal()
    })


    r_merge<-reactiveVal()

    merge_args<-reactive({
      req(input$tomerge)
      req(input$tomerge!="")
      req(input$mergeby)
      tomerge<-vals$saved_data[input$tomerge]
      args<-list(to_merge=tomerge,mergeby=input$mergeby,fill=input$fill)
      args
    })
    merge_args_coords<-reactive({
      req(input$tomerge)
      req(input$tomerge!="")
      req(input$mergeby)
      tomerge<-lapply(vals$saved_data[input$tomerge],function(x)attr(x,"coords"))
      args<-list(to_merge=tomerge,mergeby=input$mergeby,fill=input$fill)
      args
    })
    merge_args_factors<-reactive({
      req(input$tomerge)
      req(input$tomerge!="")
      req(input$mergeby)
      tomerge<-lapply(vals$saved_data[input$tomerge],function(x)attr(x,"factors"))
      args<-list(to_merge=tomerge,mergeby=input$mergeby,fill=input$fill)
      args
    })
    observeEvent(input$tomerge,{
      req(input$tomerge!="")
      if(length(input$tomerge)>1){

        shinyjs::hide("run_merge_tip")
        shinyjs::show("run_merge_btn")
      } else{
        shinyjs::show("run_merge_tip")
        shinyjs::hide("run_merge_btn")
      }
    })
    go_merge<-reactiveVal()
    stop_merge<-reactiveVal()
    check_merge<-reactiveVal()
    observeEvent(input$run_merge,{
      r_merge(NULL)
      go_merge(F)
      stop_merge(F)
      args<-merge_args()
      ncols<-sapply(args$to_merge,ncol)
      nrows<-sapply(merge_args()$to_merge,nrow)

      if(any(c(ncols,nrows)>10000)){

        go_merge(F)
        stop_merge(T)
      } else{
        go_merge(T)
        stop_merge(F)
      }

    })
    observeEvent(stop_merge(),{
      req(isTRUE(stop_merge()))
      showModal(

        modalDialog(
          size="s",
          easyClose = T,
          footer=div(
            actionButton(session$ns("proceed_merge"),"Proceed"),
            modalButton("Cancel")
          ),
          div(
            p(strong("Warning:", style="color: red"), "Merging large datasets can be computationally intensive and may result in long processing times. Click 'Proceed' to continue."),



          ))
      )
    })
    observeEvent(input$proceed_merge,{
      go_merge(T)
      stop_merge(F)
      removeModal()
    })

    observeEvent(go_merge(),{
      req(isTRUE(go_merge()))
      req(isFALSE(stop_merge()))
      withProgress({
        df<-do.call(imesc_merge,merge_args())
        df_coords<-do.call(imesc_merge,merge_args_coords())[rownames(df),1:2]
        df_factors<-do.call(imesc_merge,merge_args_factors())[rownames(df),,drop=F]
        df<-data_migrate(merge_args()$to_merge[[1]],df)
        attr(df,"factors")<-df_factors
        attr(df,"coords")<-df_coords
      }, NA,NA,NA,"Merging....")
      r_merge(df)

      shinyjs::removeClass("run_merge_btn","save_changes")
    })
    observeEvent(merge_args(),{
      shinyjs::addClass("run_merge_btn","save_changes")
    })

  })
}
tool2_tab3<-list()

tool2_tab4<-list()
tool2_tab4$ui<-function(id){
  ns<-NS(id)
  div(style="height: calc(100vh - 100px); width: 100%; overflow-y: scroll",class="pp_input",
      div(strong("Replace Attributes")),
      div(class="half-drop-inline",
          uiOutput(ns('replace_data')),

          selectInput(ns("replace_attr"),"Attribute:",choices=c("Numeric","Factors","Coords"))

      ),
      div(style="display: flex;",class="large-input",
          div(fileInput(ns("path"), label="Upload a file:",accept = c(".xls",".xlsx",".csv")),style="width: 50%"),
          div(style="width: 40%",
              uiOutput(ns("sheet_out"))
          )
      ),
      uiOutput(ns("out"))
  )

}
tool2_tab4$server<-function(id,vals){
  moduleServer(id,function(input,output,session){


    output$replace_data<-renderUI({
      selectInput(session$ns("replace_data"),"Datalist:",choices=names(vals$saved_data))
    })



    output$sheet_out<-renderUI({
      ns<-session$ns
      path<-input$path$datapath
      req(path)
      req(grepl(c('.xls'),path))
      sheets<-excel_sheets(path =path)
      selectInput(ns("sheet"),"Sheet",choices=sheets)
    })

    observe({
      path<-input$path$datapath
      if(any(grepl(c('.xls'),path))){
        shinyjs::show('sheet_out')
      } else{
        shinyjs::hide('sheet_out')
      }
    })
    newdata<-reactiveVal()
    war_coords<-reactiveVal()


    observeEvent(list(input$path$datapath,input$sheet),{
      req(input$replace_data)
      req(input$replace_data!="")
      req(input$replace_attr)
      war_coords(NULL)
      path<-input$path$datapath

      req(path)
      if(grepl(c('.xls'),path)){req(input$sheet)}
      data<-imesc_data(path, attr=input$replace_attr, sheet=input$sheet)
      if(input$replace_attr=="Coords"){

        if(ncol(data)>2){
          war_coords("Warning: The new file contains more than two columns in addition to the ID column. Only the ID column and the first two columns will be processed")
        }
      }
      data<-check_data(data, input$replace_data,input$replace_attr,vals$saved_data)
      newdata(data)
    })
    observeEvent(newdata(),{

      req(input$replace_data)
      req(input$replace_data!="")
      req(input$replace_attr)
      showModal(
        modalDialog(
          title=NULL,
          footer=div(
            if(inherits(newdata(),"data.frame")){actionButton(session$ns("run_replace"),"Confirm")},
            modalButton("Cancel")
          ),
          easyClose = T,
          if(inherits(newdata(),"shiny.tag")){
            newdata()
          } else {
            div(
              h4(strong("Replace"),emgreen(paste0(input$replace_attr,"-Attribute")),"in",emgreen(input$replace_data),strong('with the new data?')),
              uiOutput(session$ns("success_replace"))
            )

          } )
      )
    })


    output$success_replace<-renderUI({
      style_numeric<-""
      style_factor<-""
      style_coords<-""
      style0<-"color: #007bff; font-weight: bold"
      if(input$replace_attr=="Numeric"){
        style_numeric<-style0
        name<-"Numeric-Attribute:"
      } else if(input$replace_attr=="Factors"){
        style_factor<-style0
        name<-"Factor-Attribute:"
      } else {
        style_coords<-style0
        name<-"Coords-Attribute:"
      }
      div(
        style="",

        splitLayout(cellWidths = c("47%","5%","47%"),
                    div(style="background: #FFF0F5",
                        div(strong("Data to be replaced:"),style="color: brown"),
                        basic_summary2(data_replace(),name=name)),
                    div( icon("arrow-right", style = "color: #007bff; font-size: 24px;")),
                    div(
                      style="background: #F0FFF0",
                      div(strong("New:"),style="color: #007bff"),
                      basic_summary2(get_new(),style_numeric,
                                     style_factor,
                                     style_coords))
        )

      )
    })
    data_replace<-reactive({
      req(input$replace_data)
      req(input$replace_data!="")
      req(input$replace_data%in%names(vals$saved_data))
      data<-vals$saved_data[[input$replace_data]]
      attr<-tolower(input$replace_attr)
      if(attr%in%c("factors","coords")){
        data<-attr(data,attr)
      }
      data
    })

    get_new<-reactive({
      req(newdata())
      req(input$replace_data)
      req(input$replace_data!="")
      req(input$replace_data%in%names(vals$saved_data))
      data0<-vals$saved_data[[input$replace_data]]
      attr<-tolower(input$replace_attr)
      if(attr%in%c("factors","coords")){
        attr(data0,attr)<-newdata()
        new<-data0
      } else{
        new<-data_migrate(data0,newdata())
        new
      }
      new
    })

    observeEvent(input$run_replace,ignoreInit = T,{
      req(input$replace_data)
      req(input$replace_data!="")
      req(input$replace_data%in%names(vals$saved_data))
      data0<-vals$saved_data[[input$replace_data]]
      attr<-tolower(input$replace_attr)
      new<-get_new()
      vals$saved_data[[input$replace_data]]<-new
      removeModal()
      shinyjs::reset('path')
      shinyjs::hide('sheet_out')
      newdata(NULL)
      done_modal()
    })
    observeEvent(ignoreInit = T,input$newfactorfile,{
      req(input$newfactorfile)
      vals$newfactors_att<-NULL
      req(length(input$newfactorfile$datapath)!=0)

      req(input$newfac_targ)
      req(input$newfac_targ%in%names(vals$saved_data))
      data<-vals$saved_data[[input$newfac_targ]]
      labels <-data.frame(fread(input$newfactorfile$datapath, stringsAsFactors = T,na.strings=c("","NA"), header=T))

      rownames(labels) <-labels[, 1]
      labels[, 1]<-NULL

      labels[which(unlist(lapply(labels, is.numeric)))] <-
        do.call("data.frame", lapply(labels[which(unlist(lapply(labels, is.numeric)))], function(x) as.factor(x)))
      data<-vals$saved_data[[input$newfac_targ]]
      factors_in<-labels[rownames(data),,drop=F]
      notfoundnew<-which(!rownames(data)%in%na.omit(rownames(factors_in)))
      if(length(notfoundnew)>0){
        output$newfactors_missing<-renderUI({
          div(style="overflow-x: scroll;height:180px;overflow-y: scroll",
              div(strong("Error",style="color: red"),"The IDs below exist in the Data-Attribute, but not in the",strong('Factor-Attribute.'),"Please upload a new file that contains IDs compatible with the selected Datalist."),
              renderPrint(data.frame(IDs=rownames(data)[notfoundnew]))
          )
        })
      } else{
        output$newfactors_missing<-renderUI(NULL)

      }
      req(!length(notfoundnew)>0)
      vals$newfactors_att<-labels
    })
  })
}
tool2_tab5<-list()
tool2_tab5$ui<-function(id){
  ns<-NS(id)
  div(class="pp_input p20",

      div(strong("Edit column names")),
      div(
        div(
          div(style="display: flex; width: 100%",
              uiOutput(ns('editdata')),

              selectInput(ns("editattr"),"2. Attribute:",choices=c("Numeric","Factors")),
              div(id=ns("editdat_go_btn"),
                  tipify(actionButton(ns("editdat_go"),icon("fas fa-save"),style="margin-top: 25px"),"Save changes")
              ),
              tipify(actionButton(ns("reset"),icon("undo"),style="margin-top: 25px"),"Reset")

          ),


          div(div(strong("3. Double-click the variable names to edit them:")),
              div(class="half-drop-inline",
                  DT::DTOutput(ns("tabcolnames"))
              )
          ),
          uiOutput(ns("teste"))

        )
      )
  )

}
tool2_tab5$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    observeEvent(ignoreInit = T,input$editdata,{
      vals$previousSelection<-NULL
      vals$previousPage<-NULL
    })
    observeEvent(ignoreInit = T,input$editattr,{
      vals$editattr<-input$editattr
    })
    observeEvent(ignoreInit = T,input$editdata,{
      vals$editdata<-input$editdata
    })

    output$editdata<-renderUI({
      selectInput(ns("editdata"),"1. Select the Datalist:",choices=names(vals$saved_data),selected = vals$editdata)
    })
    # output read checkboxes

    delcol_values<-reactiveValues()
    observeEvent(vals$updeldata,{
      req(length(vals$updeldata)>0)
      pic<-vals$updeldata
      vals$updeldata<-NULL
      data<-attr(delcol_values$tab,'data')[,pic, drop=F]
      colnames(data)<-delcol_values$tab[,1]
      attr(delcol_values$tab,'data')<- data
    })
    output$tabcolnames<-{DT::renderDT(
      data.frame(delcol_values$tab[,-c(2:3)]),
      escape = FALSE,
      editable=T,
      options = list(
        pageLength = 10,
        displayStart = vals$previousPage,
        info = FALSE,
        lengthMenu = list(c(-1), c("All")),
        dom = 'tp'
      ),

      selection = list(mode = "single", target = "row", selected = vals$previousSelection),
      class ='cell-border compact stripe'
    )}
    observeEvent(ignoreInit = T,input$remove_button_Tab1, {
      try({

        tabcolnames<-delcol_values$tab
        s<-as.numeric(strsplit(input$remove_button_Tab1, "_")[[1]][2])
        pic<-tabcolnames$id!=s
        tabcolnames<- tabcolnames[pic,, drop=F]

        #colnames(attr(delcol_values$tab,'data'))<-tabcolnames[pic,1]
        #DT::replaceData(proxyTable, tabcolnames, resetPaging = FALSE)
        delcol_values$tab<-tabcolnames
        vals$updeldata<-pic

      })
    })
    Clicked<-eventReactive(input$tabcolnames_rows_selected,{
      input$tabcolnames_rows_selected
    })

    observeEvent(ignoreInit = T,input$tabcolnames_cell_edit, {
      vals$previousSelection<-input$tabcolnames_rows_selected
      vals$previousPage<-input$tabcolnames_rows_current[1]-1

      row <-input$tabcolnames_cell_edit$row
      clmn<-input$tabcolnames_cell_edit$col
      #delcol_values$tab[row, clmn]<-input$tabcolnames_cell_edit$value
      delcol_values<-delcol_values
      delcol_values$tab[row, clmn]<-input$tabcolnames_cell_edit$value
      newdf<- attr(delcol_values$tab,'data')
      colnames(newdf)<-delcol_values$tab[,1]
      attr(delcol_values$tab,'data')<-newdf
      #delcol_values$row<-input$tabcolnames_rows_selected-4

    })
    observeEvent(ignoreInit = T,input$editdat_go,{

      newdf<-attr(delcol_values$tab,'data')
      if(input$editattr=="Factors"){
        attr(vals$saved_data[[input$editdata]],"factors")<-newdf
      } else{
        data<-vals$saved_data[[input$editdata]]
        newdf<-data_migrate(data,newdf, input$editdata)
        vals$saved_data[[input$editdata]]<-newdf
      }

    })
    observeEvent(ignoreInit = T,input$editattr,{
      vals$previousSelection<-NULL
      vals$previousPage<-NULL
    })

    get_editdata<-reactive({



      req(input$editdata)
      data<-vals$saved_data[[input$editdata]]
      req(input$editattr)
      data<-switch(input$editattr,
                   'Numeric'=data,
                   "Factors"=attr(data,"factors"))

      datavars<-vals$olddatnames<-data.frame(Var_name=colnames(data))
      delcol_values$tab<-data.frame(datavars,data.frame(Row=1:nrow(datavars), id=1:nrow(datavars)),Remove=do.call(rbind,lapply(1:nrow(datavars),function(id) {
        getRemoveButton(id, idS = ns(""), lab = "Tab1")
      })))





      delcol_values$row<-0
      attr(delcol_values$tab,'data')<-data




    })


    observeEvent(list(vals$saved_data,input$editdata,input$editattr),{
      get_editdata()

    })

    observeEvent(input$reset,ignoreInit = T,{
      get_editdata()
    })
    shinyInput<-function(FUN, n, id, ses, ...) {
      as.character(FUN(paste0(id, n), ...))
    }
    getRemoveButton<-function(n, idS = "", lab = "Pit") {
      if (stringr::str_length(idS) > 0) idS<-paste0(idS, "")
      ret<-shinyInput(actionLink, n,
                      'button_', label = icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),
                      onclick = sprintf('Shiny.onInputChange(\"%sremove_button_%s\",  this.id)' ,idS, lab), style="font-size: 15px")
      return (ret)
    }
    observeEvent(delcol_values$tab$Var_name,{
      req(input$editdata)

      condition<-!all(colnames(vals$saved_data[[input$editdata]])%in%delcol_values$tab$Var_name)
      shinyjs::toggleClass("editdat_go_btn",class="save_changes",condition=condition)

    })
  })



}
tool2_tab6<-list()
tool2_tab6$ui<-function(id){
  ns<-NS(id)
  div(
    div(
      p(strong("Rename Models")),
      div(class="half-drop half-drop-inline",
          uiOutput(ns("datalist_out"))



      ),
      div(style="overflow-y: auto; max-height: calc(100vh - 250px)",
          uiOutput(ns("rename_page"))),
      div(class="half-drop",id=ns('run_rename_btn'),
          actionButton(ns("run_rename"),"Apply",icon=icon("sync"))
      ),

    )
  )
}
tool2_tab6$server<-function(id,vals){
  moduleServer(id,function(input,output,session){





    get_model_list<-reactive({
      re1<-sapply(vals$saved_data, function(data){
        res<-lapply(imesc_models,function(model){
          model_names<-names(attr(data,model))
          if(length(model_names)>0){
            data.frame(model_names, model=model)}
        })
        do.call(rbind,res)
      })
      df<-re1[sapply(re1, length)>0]
      df
    })



    output$datalist_out<-renderUI({
      selectInput(session$ns("datalist"),span(tiphelp("It shows only Datalist with saved models", 'left'),"Datalist:"),choices=names(get_model_list()), selected=names(get_model_list())[2])


    })


    model_tip<-function(attr){
      if(is.null(attr)){return(NULL)}
      res<- switch(attr,
                   "som"={span(tiphelp("Self-Organizing Maps","left"),span("SOM"))},
                   "kmeans"={span(tiphelp("K-Means","left"),span("k-Means"))},
                   "rf"={span(tiphelp("Random Forest","left"),span("RF"))},
                   "nb"={span(tiphelp("Naive Bayes","left"),span("NB"))},
                   "svm"={span(tiphelp("Support Vector Machine","left"),span("SVM"))},
                   "knn"={span(tiphelp("K-Nearest Neighbors","left"),span("KNN"))},
                   "sgboost"={span(tiphelp("Stochastic Gradient Boosting","left"),span("GBM"))},
                   "xyf"={span(tiphelp("Supervised Self-Organizing Maps","left"),span("XYF"))}
      )
      return(res)
    }
    model_name<-function(attr){
      res<- switch(attr,
                   "som"={"SOM (usupervised)"},
                   "kmeans"={"k-means"},
                   "rf"={"Random Forest"},
                   "nb"={"Naive Bayes"},
                   "svm"={"Support Machine Vector"},
                   "knn"={"KNN"},
                   "sgboost"={"Stochast. Gradient. Boosting"},
                   "xyf"={"SOM (supervised)"}
      )
      return(res)
    }
    output$rename_page<-renderUI({
      # validate(need(input$datalist!="","No saved models found"))
      ns<-session$ns
      req(input$datalist)
      df<-get_model_list()
      df1<-df[[input$datalist]]
      choices<-df1$model_names
      div(class="text-table",
          lapply(seq_along(choices),function(i){
            div(class="mp0",style="display: flex",
                div(style="min-width: 100px",
                    model_tip(df1$model[i])),
                div(
                  textInput(ns(paste0("newname_datalist_",i)),NULL,choices[i],placeholder ="Type a new name")))
          } )

      )

    })

    newnames<-reactive({
      req(input$datalist)
      df<-get_model_list()
      df1<-df[[input$datalist]]
      newnames<-sapply(seq_along(df1$model_names),function(i){
        input[[paste0("newname_datalist_",i)]]} )
      newnames
    })
    observe({
      req(newnames())
      df<-get_model_list()
      df1<-df[[input$datalist]]
      req(df1)
      req(length(unlist(newnames()))>0)

      if(any(df1$model_names!=newnames())){
        shinyjs:: addClass('run_rename_btn',"save_changes")
      } else{
        shinyjs:: removeClass('run_rename_btn',"save_changes")
      }

    })
    observeEvent(input$run_rename,ignoreInit = T,{

      df<-get_model_list()
      df1<-df[[input$datalist]]
      newnames<-newnames()
      df1$new<-newnames
      df2<-split(df1,df1$model)
      for(i in seq_along(df2)){
        attr<-unique(df2[[i]]$model)
        new<-df2[[i]]$new
        names(attr(vals$saved_data[[input$datalist]],attr))<-new
      }
      shinyjs::removeClass('run_rename_btn',"save_changes")
      done_modal()
    })
  })
}



tool2_tab7<-list()
tool2_tab7$ui<-function(id){
  ns<-NS(id)
  div(class="p10",
      div(strong("Transpose Datalist")),
      div(class="half-drop p20",style="display: flex",
          uiOutput(ns('datalist')),
          actionButton(ns("run_transpose"),"Apply",icon=icon("sync"))

      )
  )
}
tool2_tab7$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns

    output$datalist<-renderUI({
      selectInput(ns("datalist"),"Datalist",names(vals$saved_data))
    })

    getnewdata<-reactive({
      req(input$datalist)
      req(input$datalist!="")
      req(input$datalist%in%names(vals$saved_data))
      data0<-data<-vals$saved_data[[input$datalist]]
      req(data)
      factors<-attr(data,"factors")
      data<-data.frame(t(data))
      colnames(data)<-make.unique(rownames(data0))
      rownames(data)<-make.unique(colnames(data0))
      data<-data_migrate(data0,data)
      tfactors<-data.frame(t(factors))
      colnames(tfactors)<-make.unique(rownames(factors))
      rownames(tfactors)<-make.unique(colnames(factors))

      attr(data,"factors")<-tfactors
      attr(data,"coords")<-NULL
      if(!is.null(attr(data0,"coords"))){
        message(emwarning("Coordinates cannot be transposed and will therefore be removed."))
      }
      attr(data,"bag")<-"Transpose"
      req(data)
      data
    })


    observeEvent(input$run_transpose,ignoreInit = T,priority = 0,{



      showModal(
        modalDialog(
          title="Save changes",
          easyClose =T,
          footer=div(actionButton(ns("data_confirm"),strong("confirm")),
                     modalButton("Cancel")),
          div(style="padding: 20px",
              div(class="half-drop",
                  style="display: flex",
                  div(style="width: 20%",
                      radioButtons(ns("create_replace"),
                                   NULL,
                                   c(
                                     "Create"
                                     #,"Replace"
                                   ))
                  ),
                  div(style="padding-top: 10px",
                      uiOutput(ns("out_newdatalit")),
                      uiOutput(ns("out_overdatalist"))
                  )

              ),
              div(style="padding-left: 30px",
                  uiOutput(ns("message")),
                  uiOutput(ns('newdata')),

              )

          )

        )
      )


    })
    message<-reactiveVal()
    output$newdata<-renderUI({
      req(getnewdata())
      basic_summary2(getnewdata())
    })
    output$out_newdatalit<-renderUI({
      req(input$create_replace)
      req(input$create_replace=="Create")
      req(getnewdata())
      bag<-attr(getnewdata(),"bag")

      newnames<-make.unique(c(names(vals$saved_data),bag))
      name0<-newnames[length(newnames)]
      textInput(ns("newdatalit"),NULL,name0)
    })
    output$out_overdatalist<-renderUI({

      req(input$create_replace=="Replace")
      selectInput(ns("overdatalist"),NULL,choices=names(vals$saved_data),selected=vals$cur_data)
    })
    datalistnew<-reactive({
      req(input$create_replace)
      # switch(input$create_replace,'Create'={input$newdatalit},'Replace'={ input$overdatalist})
      input$newdatalit
    })
    output$message<-renderUI({
      message()
    })
    observeEvent(input$data_confirm,ignoreInit = T,{

      req(datalistnew())
      vals$saved_data[[datalistnew()]]<-getnewdata()
      removeModal()
      done_modal()
    })





  })
}
tool2_tab8<-list()
tool2_tab8$ui<-function(id){
  ns<-NS(id)
  div(class="tool_box8",
      # actionLink(ns("save_bug"),"save bug"),
      div(
        div(class="needed",id="fade",
            div(
              div(style="position: fixed; right: 80vw;top: 50px ",
                  actionButton(ns("exit_tool8"), label = NULL, icon = icon("times"), style = "padding: 0px; font-size: 15px; width: 20px; height: 20px;background: Brown; color: white; border: 0px;"))
            )
        ),
        class="tool8",
        div(strong("SHP toolbox")),

        tabsetPanel(
          tabPanel(
            "Create Shape",value="tab_create",
            div(
              class="shp_box",style="overflow-y: auto;margin-left: -10px",
              column(
                12,class="mp0",
                column(
                  4,class="mp0",
                  box_caret(
                    ns("box_shp1"),
                    title="1. Targets & Upload",
                    button_title = actionLink(ns('reset'),"reset",icon('undo')),
                    color="#c3cc74ff",
                    div(
                      style="height: 135px",
                      column(
                        12,class="mp0",
                        column(10,class="mp0",pickerInput_fromtop(
                          ns("shp_include"),
                          "Target-Attribute:",
                          c("Base-Shape"="base_shape","Layer-Shape"="layer_shape","Extra-Shape"="extra_shape")

                        )),
                        column(2,align="right",class="mp0",div(style="margin-top: 8px; ",tipify(actionLink(ns("trash_open"),icon('trash')),"Remove Current Shape-Attribute")))

                      ),
                      uiOutput(ns("out_shp_datalist")),

                      div(
                        style="padding-left: 15px",
                        textInput(ns("extra_layer_newname"), "Extra-Layer name:",NULL)
                      ),
                      div(style="display: flex;margin-top: 2px",
                          tags$label("Shape Files:",tipright(actionLink(ns("shp_help"),icon("fas fa-question-circle")),"Upload the shapefiles at once")),
                          div(class="large-input",fileInput(inputId = ns("shp"),"", multiple = TRUE, accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj'))),

                      )

                    )
                  )
                ),
                column(
                  8,class="mp0",
                  box_caret(
                    ns('box_cur_shapes'),
                    title="Saved Shapes",

                    div(uiOutput(ns("cur_shape_plot")))
                  )
                )
              ),

              column(

                12,class="mp0",
                column(
                  7,class="mp0",
                  id=ns("filter_crop"),style="display: none",
                  box_caret(
                    ns("box_shp2"),
                    title=div(class="read_shp",style='display: inline-block',"2. Filter & Crop",
                              div(
                                id=ns('read_shp_btn'),
                                class="save_changes",
                                style='display: inline-block',
                                icon("fas fa-hand-point-right"),
                                span(bsButton(ns('read_shp'),"Read shapes"))
                              )

                    ),
                    color="#c3cc74ff",

                    fluidRow(column(
                      4,class="mp0",

                      div(
                        numericInput(ns("st_simplify"),span("Simplify:",tipright('Specify a tolerance (in meters) to simplify geometries for faster visualization')),value=NA,step=0.01),
                        uiOutput(ns('filter_features')),
                        pickerInput(
                          ns("crop_shapes"),
                          span("Crop to:",tipright("<p>Select the shape to crop the area of the new shape.</p><p>If custom is selected, the area will be cropped according to the area of the plot being displayed. Use Plotly interactive features to zoom and specify the crop area precisely.</p>")

                          ),
                          choices=NULL)

                      )

                    ),
                    column(
                      8,class="mp0",
                      style="overflow-y: auto;max-height:45vh ",
                      pickerInput_fromtop(ns("show_layers"),"Show:",choices=c("base_shape","layer_shape","extra_shape"),multiple = T),
                      uiOutput(ns('shp_out'))

                    ))
                  )
                ),
                column(
                  5,class="mp0",
                  id=ns("create_save"),style="display: none",
                  box_caret(
                    ns('box_final_shapes'),
                    title=div(class="read_shp",
                              style='display: inline-block',
                              "3. Create & Save:",
                              div(
                                id=ns('prepare_btn'),
                                class="save_changes",
                                style='display: inline-block',
                                icon("fas fa-hand-point-right"),
                                span(bsButton(ns('prepare'),"Create",icon("fas fa-map")))
                              ),
                              div(
                                id=ns("add_shape_btn"),
                                class='save_changes read_shp_save',

                                style="display: inline-block; position: absolute; right: 5px",
                                bsButton(ns("add_shape"),icon("fas fa-save"), style='width: 50px')
                              )

                    ),

                    div(uiOutput(ns("final_shape_plot")))
                  )
                )
              )

            )
          ),
          tabPanel(
            "View and download",value="tab_view",
            div(
              div(style="max-width: 400px",em(icon("fas fa-lightbulb"),"Optimize your datalist creation speed by downloading shapes as an .rds file. This file format loads quickly and avoids the need to create the shape from scratch each time you need it.")),
              column(
                12,class="mp0",
                column(
                  4,class='mp0',

                  box_caret(
                    ns('box_shp_tab1_1'),
                    title="Options",
                    color="#c3cc74ff",
                    div(
                      uiOutput(ns("shp_data_view_down")),
                      pickerInput_fromtop(
                        ns("shp_attr_view_down"),
                        "2. Select the Attribute:",
                        choices=list("Base-Shape"="base_shape","Layer-Shape"="layer_shape")

                      )
                    )

                  )

                ),
                column(
                  8,class="mp0",
                  box_caret(
                    ns('box_shp_tab1_2'),
                    title="Plot",
                    button_title = downloadLink(ns("download_shape"),"Download",icon("download")),
                    div(

                      uiOutput(ns("shape_view"))

                    )
                  )
                )
              )
            )
          )
        ))


  )
}
tool2_tab8$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns

    observeEvent(input$exit_tool8,{
      vals$exit_tool8<-input$exit_tool8

    })

    data_shp_down<-reactive({
      req(input$shp_data_view_down)
      vals$saved_data[[input$shp_data_view_down]]
    })
    shape_list_down<-reactive({
      data<-data_shp_down()
      base_shape<-attr(data,"base_shape")
      layer_shape<-attr(data,"layer_shape")
      extra_shape<-attr(data,"extra_shape")
      shape_list<-c(list(base_shape=base_shape,layer_shape=layer_shape),extra_shape)
      shape_list
    })
    observeEvent(shape_list_down(),{
      updatePickerInput(session,'shp_attr_view_down',choices=names(shape_list_down()))

    })
    output$shape_view<-renderUI({
      shl<-shape_list_down()
      req(input$shp_data_view_down)
      req(input$shp_attr_view_down)
      shape<-shl[[input$shp_attr_view_down]]
      req(length(shape)>0)
      renderPlot({ggplot(st_as_sf(shape)) + geom_sf()+
          theme(panel.background = element_rect(fill = "white"),
                panel.border = element_rect(fill=NA,color="black", linewidth=0.5, linetype="solid"))}, height=250)
    })

    output$download_shape<-{
      downloadHandler(
        filename = function() {
          paste0(paste0(input$shp_attr_view_down,"_",input$shp_data_view_down),"_", Sys.Date())
        }, content = function(file) {
          shl<-shape_list_down()
          shape<-shl[[input$shp_attr_view_down]]
          saveRDS(shape,file)
        })

    }

    observeEvent(input$shp,{
      shinyjs::show('read_shp_btn')
    })
    shape1_raw<-reactiveVal(NULL)
    shape2_prep<-reactiveVal(NULL)
    shape3_filtered<-reactiveVal(NULL)
    shape4_final<-reactiveVal(NULL)
    observe({
      shinyjs::toggle('add_shape',condition=!is.null(shape4_final()))
    })
    observe({
      shinyjs::toggle('prepare_btn',condition=!is.null(shape2_prep()))
    })

    observe({
      shinyjs::toggle("st_simplify",condition=!is.null(shape1_raw()))
    })
    shp_step<-reactiveVal(0)
    shp_start<-reactive({
      list(input$shp,data_shp())
    })
    observeEvent(input$reset,{

      shp_step(1)
      shape1_raw(NULL)
      shape2_prep(NULL)
      shape3_filtered(NULL)
      shape4_final(NULL)
      plot_step1(NULL)
      plot1_prepare(NULL)
      vals$shp_show_layers<-input$shp_include
      shinyjs::reset('shp')
      shinyjs::hide('read_shp_btn')
      shinyjs::hide('crop_shapes')

    })
    observeEvent(shp_start(),{
      shp_step(0)
      shape1_raw(NULL)
      shape2_prep(NULL)
      shape3_filtered(NULL)
      shape4_final(NULL)
      plot1_prepare(NULL)
      plot_step1(NULL)
      vals$shp_show_layers<-input$shp_include
      shinyjs::addClass('read_shp_btn',"save_changes")
      shinyjs::hide('crop_shapes')
    })
    observe({
      shinyjs::toggle('read_shp_btn',condition=length(input$shp)>0)
    })
    observeEvent(input$shp,ignoreInit = T,{
      shinyjs::show('read_shp_btn')
      shinyjs::addClass('read_shp_btn',"save_changes")
    })
    shp_files<-reactiveVal()
    observeEvent(input$shp,ignoreInit = T,{
      sf<-Read_Shapefile(input$shp)
      shinyjs::show('filter_crop')
      shinyjs::addClass('prepare_btn',"save_changes")
      shp_files(sf)
      updateNumericInput(session,'st_simplify',value= as.numeric(round(get_tolerance(sf),3)))
    })
    observeEvent(shp_rows(),ignoreInit = T,{
      shp<-shape1_raw()
      if(length(shp_rows())<nrow(shp)){
        shp<-shp[shp_rows(),,drop=F]
        updateNumericInput(session,'st_simplify',value= as.numeric(round(get_tolerance(shp),3)))
      }
    })
    observeEvent(input$read_shp,ignoreInit = T,{
      shinyjs::show('crop_shapes')
      req(shp_files())
      user_shp<-shp_files()
      user_shp<-st_transform(user_shp,"+proj=longlat +datum=WGS84 +no_defs")


      user_shp$shape<-"selected"
      shape1_raw(NULL)
      shape2_prep(user_shp)
      shape3_filtered(NULL)
      shape4_final(NULL)
      shape1_raw(user_shp)
      shp_step(2)
      shinyjs::removeClass('read_shp_btn',"save_changes")

    })
    observeEvent(input$shp_feature1,ignoreInit = T,{

      shinyjs::toggle("feature2",condition = input$shp_feature1!="None")
    })

    observe({
      shinyjs::toggle('create_save', condition = !is.null(shape2_prep()))
    })

    #step2
    observeEvent(input$prepare,ignoreInit = T,{



      bacias<-    shape1_raw()
      req(bacias)
      req(input$shp_feature1)

      if(all(c(input$shp_feature1)=="None")){


        shape3_filtered(shape1_raw())
        #shape2_prep(shape1_raw())
        shape4_final(shape1_raw())

      } else     {
        req(input$shp_feature2)
        bacias<-filtered_shp()
        shape2_prep(bacias)
        shape3_filtered(bacias)
      }
      shinyjs::removeClass("prepare_btn","save_changes")
      new_shape<-shape3_filtered()
      req(new_shape)
      req(input$shp_datalist%in%names(vals$saved_data))
      data<-vals$saved_data[[input$shp_datalist]]
      base_shape<-attr(data,"base_shape")

      withProgress(min=NA,max=NA,message="Cropping shape to base_shape",{
        lims1<-rect_coords()
        lims1<-unlist(lims1)
        new_shape<-sf::st_crop(new_shape, lims1)
      })

      shape4_final(new_shape)
      shinyjs::show('add_shape_btn_out')
      shinyjs::addClass("add_shape_btn","save_changes")

    })
    #step3

    #
    output$shp_out<-renderUI({
      req(!is.null(shape2_prep()))
      div(
        div(style="display: flex",

            uiOutput(ns('shp_warning'))
        ),
        plotly::plotlyOutput(ns("full_shape_plot"),height = "300px")

      )
    })
    rect_coords <- reactiveVal(NULL)
    observeEvent(input$trash_open,ignoreInit = T,{
      showModal(
        modalDialog(
          title="Confirm Shape exclusion",
          div(embrown("Are you sure?")),
          footer=div(actionButton(ns("trash_confirm"),"Confirm"),modalButton("Dimiss"))
        )
      )
    })
    observeEvent(input$trash_confirm,ignoreInit = T,{
      attr(vals$saved_data[[input$shp_datalist]],input$shp_include)<-NULL
      removeModal()
    })
    output$add_shape_btn_out<-renderUI({
      req(!is.null(shape4_final()))
      disabled=F
      class="save_changes"

      cond<-!nrow(shape4_final())>0
      if(cond){
        disabled=T
        class="div"
        tip="No features remain in the New Shape"
      }

    })
    output$feature1<-renderUI({
      atributos_shp<-attributes(shape1_raw())$names
      atributos_shp<-atributos_shp[!atributos_shp%in%"geometry"]
      pickerInput_fromtop(ns('shp_feature1'),"Filter by:",choices=c("None",atributos_shp))
    })
    output$feature2<-renderUI({
      req(shape1_raw())
      req(input$shp_feature1)
      lev_attrs<-unique(shape1_raw()[[input$shp_feature1]])


      pickerInput_fromtop(
        ns('shp_feature2'),
        icon("filter"),choices=c(lev_attrs),
        options=shinyWidgets::pickerOptions(liveSearch =T)

      )
    })
    output$crop_feature<-renderUI({

      div(style="margin-left:-5px",
          pickerInput(
            ns("crop_shapes"),
            span("6. Crop limits:",tipright("<p>Select the shape to crop the area of the new shape.</p><p>If custom is selected, the area will be cropped according to the area of the plot being displayed. Use Plotly interactive features to zoom and specify the crop area precisely.</p>")

            ),
            choices=c(names(current_shapes_list()),"Custom"),
            options = list(`selected-text-format` = "count > 3"),
            multiple =T,selected=names(current_shapes_list())[1]
          )
      )


    })
    observeEvent(input$crop_shapes,ignoreInit = T,{
      if ("Custom" %in% input$crop_shapes) {
        updatePickerInput(
          session,
          "crop_shapes",
          selected = "Custom",
          options = list(`selected-text-format` = "count > 3")
        )
      }
    })
    output$filter_features<-renderUI({
      req(!is.null(shape1_raw()))
      div(
        uiOutput(ns('feature1')),
        uiOutput(ns('feature2'))




      )
    })
    current_shapes<-reactive({
      data<-data_shp()
      base<-!is.null(attr(data,"base_shape"))
      layer<-!is.null(attr(data,"layer_shape"))
      extra<-!is.null(attr(data,"extra_shape"))
      choices<-c(base,layer,layer)
      pic<-which(choices)
      pic
    })
    observeEvent(input$save_bug,{
      saveRDS(reactiveValuesToList(input),'input.rds')
      saveRDS(reactiveValuesToList(vals),'vals.rds')

    })
    go_prepare<-reactiveVal(1)
    filtered_shp<-reactive({
      bacias<-shape1_raw()

      if(input$shp_feature1=="None"){
        return(bacias)
      } else{
        req(input$shp_feature2)
      }

      #input$shp_feature2<-c('Bacia de Santos')
      bacias$shape<-"unselected"
      req(input$shp_feature1)
      req(input$shp_feature1%in%names(bacias))
      rows=bacias[[input$shp_feature1]]
      req(input$shp_feature2%in%rows)
      bacias$shape[rows==input$shp_feature2]<-"selected"
      bacias<-bacias[rows==input$shp_feature2,,drop=F]
      bacias
    })
    shp_rows<-reactive({
      req(input$shp_feature1)
      bacias<-shape1_raw()
      req(bacias)

      selected_rows<-1:nrow(bacias)

      if(input$shp_feature1!="None"){
        bacias$shape<-"unselected"
        req(input$shp_feature1)
        req(input$shp_feature1%in%names(bacias))
        rows=bacias[[input$shp_feature1]]
        req(input$shp_feature2%in%rows)
        bacias$shape[rows==input$shp_feature2]<-"selected"
        selected_rows<-which(rows==input$shp_feature2)
        selected_rows
      }
      selected_rows
      #input$shp_feature2<-c('Bacia de Santos')

    })
    observeEvent(input$shp_feature2,ignoreInit = T,{
      shinyjs::addClass("prepare_btn","save_changes")
      shinyjs::hide('add_shape_btn_out')
    })
    shape5_to_add<-reactiveVal(NULL)
    confirm_shp<-reactiveVal(F)
    observeEvent(input$add_shape,ignoreInit = T,{
      cur_shape<-attr(data_shp(),input$shp_include)
      if(is.null(cur_shape)){
        confirm_shp(T)
      } else{
        confirm_modal(session$ns,action=h4("Are you sure?"),
                      data1=NULL,
                      data2= NULL,
                      arrow=F,
                      left=paste("Replace",input$shp_include, 'with the New Shape?'),
                      right="",
                      from='',
                      to='')
      }

      #shape1_raw()


    })
    observeEvent(input$confirm,ignoreInit = T,{
      removeModal()
      confirm_shp(T)
    })
    observeEvent(confirm_shp(),{
      req(isTRUE(confirm_shp()))
      confirm_shp(F)
      shape<-shape4_final()
      req(shape)
      res<-switch(
        input$shp_include,
        "base_shape"={
          attr(vals$saved_data[[input$shp_datalist]],"base_shape")<-shape
        },
        "layer_shape"={
          attr(vals$saved_data[[input$shp_datalist]],"layer_shape")<-shape
        },
        'extra_shape'={
          extra_names<-make.unique(c(
            names(attr(vals$saved_data[[input$shp_datalist]],"extra_shape")),input$extra_layer_newname
          ))


          attr(vals$saved_data[[input$shp_datalist]],"extra_shape")[[ extra_names[length(extra_names)]]]<-shape
        }

      )

      shinyjs::reset("shp")
      shape1_raw(NULL)
      shape2_prep(NULL)
      shape3_filtered(NULL)
      shape4_final(NULL)
      shinyjs::hide('crop_shapes')
      shinyjs::removeClass("add_shape_btn","save_changes")
      shinyjs::hide('read_shp_btn')
      shinyjs::hide('filter_crop')

    })
    observeEvent(input$shp_help,{
      showModal(
        modalDialog(
          title="Shapefiles",
          easyClose = T,
          shp_help()
        )
      )
    })
    data_shp<-reactive({
      req(input$shp_datalist)
      req(input$shp_datalist%in%names(vals$saved_data))
      vals$saved_data[[input$shp_datalist]]
    })
    get_new_shape_attr<-reactive({


      shape_list<-shape_list()
      shp_names<-names(shape_list)[sapply(shape_list,length)>0]

      if(!length(shp_names)>0){
        return('base_shape')
      }

      new="base_shape"
      if(length(shp_names)>0){
        if(!"base_shape"%in%shp_names){
          new="base_shape"

        }
        if(!"layer_shape"%in%shp_names){
          new="layer_shape"

        }
        if(all(c('base_shape',"layer_shape")%in%shp_names)){
          new="extra_shape"

        }
      }
      new
    })
    bag_extralayer<-reactive({
      name0<-'Extra-Layer'
      new<-make.unique(c(names(attr(data_shp(),"extra_shape")),name0))
      new[length(new)]
    })
    output$save_feature<-{
      downloadHandler(
        filename = function() {
          paste0("feature_shape","_", Sys.Date())
        }, content = function(file) {
          req(shape4_final())
          saveRDS(shape4_final(),file)
        })

    }
    observeEvent(input$shp_view,{
      showModal(
        shp_tool()
      )
    })
    shapes_list<-reactive({
      base_shape<-attr(data_shp(),"base_shape")
      layer_shape<-attr(data_shp(),"layer_shape")
      eshape<-attr(data_shp(),"extra_shape")
      pic<-which(unlist(lapply(list(base_shape,layer_shape),function(x)length(x)>0)))
      eshape[['Base Shape']]<-base_shape
      eshape[['Layer Shape']]<-layer_shape
      new=c(eshape)
      new
    })
    observeEvent(input$close_shp,{
      removeModal()
    })
    observe({
      shinyjs::toggle('show_layers',condition=!is.null(shape2_prep()))
      shinyjs::toggle('crop_shapes',condition=!is.null(shape2_prep()))
    })
    observe({
      choices=c(names(current_shapes_list()),"Custom")
      updatePickerInput(session,'crop_shapes',choices=choices,selected=choices[1])
    })
    choices_layers<-reactive({
      choices<-c("Base-Shape"="base_shape","Layer-Shape"="layer_shape","Extra-Shape"="extra_shape")
      names(choices)<-choices
      choices<-choices[sapply(choices,function(x) length(attr(data_shp(),x))>0)]
      choices=c(choices,input$shp_include)
      names(choices)[length(choices)]<-input$shp_include
      choices<-choices[!duplicated(choices)]
      include<-which(choices%in%input$shp_include)
      names(choices)[include]<-paste0("New:",names(choices)[include])
      choices<-c(choices[include],choices[-include])
    })
    observe({
      updatePickerInput(session,'show_layers',
                        choices=choices_layers(),
                        selected=input$shp_include)
    })
    observeEvent(input$shp_include,{
      updatePickerInput(session,'show_layers',selected=input$shp_include)
    })
    output$out_shp_datalist<-renderUI({
      choices=names(vals$saved_data)
      req(length(choices)>0)
      selected=get_selected_from_choices(vals$cur_data,choices)
      pickerInput_fromtop(ns("shp_datalist"),"Target Datalist:",choices=choices,selected=selected)
    })
    observeEvent(input$shp_datalist,ignoreInit = T,{
      vals$cur_data<-input$shp_datalist
    })
    output$shp_data_view_down<-renderUI({
      pickerInput_fromtop(session$ns("shp_data_view_down"),"1. Select the Datalist:",choices=names(vals$saved_data))
    })
    observe({
      shinyjs::toggle('extra_layer_newname',condition=input$shp_include%in%'extra_shape')
    })
    observeEvent(bag_extralayer(),{
      updateTextInput(session,'extra_layer_newname',value=bag_extralayer())
    })
    # choices<-layers_choices2()
    plot_shape<-function(nw_shp){
      req(!is.null(nw_shp))
      req(inherits(nw_shp,"sf"))
      p<-ggplot(st_as_sf(nw_shp))+ geom_sf(fill="gray")+theme_void()
      p

    }

    shape_list<-reactive({
      data<-data_shp()
      base_shape<-attr(data,"base_shape")
      layer_shape<-attr(data,"layer_shape")
      extra_shape<-attr(data,"extra_shape")
      shape_list<-c(list(base_shape=base_shape,layer_shape=layer_shape),extra_shape)
      shape_list
    })

    output$cur_shape_plot<-renderUI({
      req(input$shp_include)
      shpl<-shape_list()
      shpl<-shpl[sapply(shpl,length)>0]

      div(class="plot200 shp_p0",
          strong("Shapes saved in ",input$shp_datalist,":"),
          div(style="display: flex",
              lapply(names(shpl),function(name){
                div(em(name),
                    renderPlot(
                      plot_shape(shpl[[name]]),height= 100,width=100,
                    )
                )
              })
          ))
    })
    plot_shp_function<-function(shp,data,shp_include,show_shapes=c("base_shape","layer_shape","extra_shape")){

      attr(data,shp_include)<-shp
      if(shp_include=="extra_shape"){
        attr(data,"extra_shape")<-list()
        attr(data,"extra_shape")[["new extra-shape"]]<-shp
      }
      base_shape<-attr(data,"base_shape")
      layer_shape<-attr(data,"layer_shape")
      extra_shape<-attr(data,"extra_shape")

      p<-ggplot()+theme_void()

      if('base_shape'%in%show_shapes){
        if(!is.null(base_shape)) {
          base_shape$shape<-"base_shape"
          if(shp_include=="base_shape"){
            base_shape$shape<-paste0("new_shape: base_shape")
          }
          p<-p+geom_sf(data=st_as_sf(base_shape),
                       aes(fill=shape),
                       show.legend=T)
        }
      }
      if('layer_shape'%in%show_shapes){
        if(!is.null(layer_shape)) {

          layer_shape$shape<-"layer_shape"
          if(shp_include=="layer_shape"){
            layer_shape$shape<-paste0("new_shape: layer_shape")
          }
          p<-p+geom_sf(data=st_as_sf(layer_shape),aes(fill=shape),show.legend=T)
        }
      }


      if('extra_shape'%in%show_shapes){
        if(!is.null(extra_shape)) {
          if(is.list(extra_shape))
            for(i in 1:length(extra_shape)){
              extra<-extra_shape[[i]]
              if(inherits(extra,c('sfc','sf'))){
                if(!is.null(extra)) {
                  extra<-st_as_sf(extra)
                  extra$shape<-paste0("extra_shape",i)
                  if(shp_include=="extra_shape"){
                    extra$shape<-paste0("new_shape:",paste0("extra_shape",i))
                  }
                  p<-p+geom_sf(data=extra,aes(fill=shape),show.legend=T)
                }
              }

            }}
      }



      true_layers<-which(sapply(list(base_shape,layer_shape,extra_shape),length)>0)

      colors<-c(base_shape="gray",layer_shape="DarkBlue",extra_shape="Brown")
      colors[shp_include]<-"Green"
      colors<-colors[true_layers]
      colors<-adjustcolor(colors,0.3)

      p<-p+scale_fill_manual(name="",values=as.character(colors))+theme(
        legend.margin=margin(0,0,0,0),
        legend.key.size=unit(9,"pt"),
        legend.position="bottom"
      )
      p
    }
    once<-reactiveVal(F)
    plot1_prepare<-reactiveVal()
    observe({
      shp<-shape1_raw()
      req(shp)
      req(shape2_prep())
      req(input$st_simplify)
      req(!is.na(input$st_simplify))
      data<-data_shp()
      req(data)

      if(length(shp_rows())<nrow(shp)){
        shp<-shp[shp_rows(),,drop=F]
      }



      shp<-st_simplify(shp, dTolerance = input$st_simplify)
      args<-list(shp=shp,data=data, shp_include=input$shp_include, show_shapes=input$show_layers)




      p<-do.call(plot_shp_function,args)

      plot1_prepare(p)
    })
    get_shp_limits<-function(shape_list){

      res<-lapply(names(shape_list), function(i){
        x<-shape_list[[i]]
        res<-data.frame(as.list(st_bbox(x)))
        rownames(res)<-i
        res
      })
      do.call(rbind,res)

    }

    get_limits_shapes<-reactive({
      shape_list<-current_shapes_list()
      res<-get_shp_limits(shape_list)
      shp1_cutted<-shape3_filtered()
      if(is.null(shp1_cutted)){
        shp1_cutted<-shape2_prep()
      }
      req(shp1_cutted)
      new_shape_limts<-data.frame(as.list(st_bbox(shp1_cutted)))
      rownames(new_shape_limts)<-'New Shape'
      res<-rbind(res,new_shape_limts)
      res
    })
    get_tolerance <- function(shp, fraction = 50) {
      # Verificar se a unidade do sistema de coordenadas é adequada


      # Obter as dimensões do bounding box do shapefile em metros
      bbox <- st_bbox(shp)
      width <- bbox$xmax - bbox$xmin
      height <- bbox$ymax - bbox$ymin

      # Calcular a área do bounding box
      area <- width * height

      # Calcular a tolerância como uma fração da área
      tolerance <- sqrt(area) * fraction

      return(tolerance)
    }

    plot_shp_rect<-function(p,data,lims){

      coords<-attr(data,"coords")
      if(!is.null(coords)){
        colnames(coords)[1:2]<-c("x","y")
      }


      if(!is.null(coords)){
        # p<-p+geom_point(data=coords,aes(x,y))
      }

      x<-lims$xmax
      y<-mean(c(lims$ymin,lims$ymax))
      label="Current crop"
      if(!"Custom"%in%input$crop_shapes){
        p<-p+geom_rect(aes(xmin=lims$xmin,xmax=lims$xmax,ymin=lims$ymin,ymax=lims$ymax),color="red", fill=NA,linetype="dashed")
      }
      p
      #+
      #geom_text(data=data.frame(x,y,label),aes(x,y,label=label), hjust = 0)+
      #coord_sf(crs ="+proj=longlat +datum=WGS84 +no_defs")
    }
    observeEvent(get_shape_lims(),{
      rect_coords(list(xmin=get_shape_lims()[['xmin']],
                       xmax=get_shape_lims()[['xmax']],
                       ymin=get_shape_lims()[['ymin']],
                       ymax=get_shape_lims()[['ymax']]))
    })
    plot_step1<-reactiveVal()
    observe({
      p<-plot1_prepare()
      req(p)
      lims<-get_shape_lims()


      args<-list(p=p,lims=lims,data=data)
      p<-do.call(plot_shp_rect,args)
      plot_step1(p)
    })
    plot_ready<-reactiveVal(F)
    simplity_sf_gg<-function(p){
      pic<-which(sapply(p$layers,function(x) inherits(x,"LayerSf")))
      for(i in pic){
        sf1<-p$layers[[i]]$data$shape
        if(nrow(p$layers[[i]]$data)>1){
          new_sf<- st_simplify(p$layers[[i]]$data)
          new_sf$shape<-sf1
          p$layers[[i]]$data<- new_sf
        }}
      p
    }
    output$full_shape_plot<-plotly::renderPlotly({
      p<- plot_step1()
      req(p)
      p<-simplity_sf_gg(p)

      p<-plotly::ggplotly( p, source = "A") %>%
        plotly::layout(legend = list(x = 0, y = 0))%>%
        plotly::config(scrollZoom = TRUE)
      plotly::event_register(p,'plotly_relayout')
      plot_ready(T)
      p <- plotly::style(p, hoverinfo = "skip", traces = seq_along(p$x$data))
      #p<-readRDS("p.rds")
      i=2

      long<-round(p$x$data[[i]]$x,4)
      lat<-round(p$x$data[[i]]$y,4)
      p$x$data[[2]]$hoverinfo <- 'text'
      p$x$data[[2]]$text <- paste0("long: ",long , "<br>lat: ", lat)


      p

    })
    observe({
      req(isTRUE(plot_ready()))
      observeEvent(plotly::event_data("plotly_relayout", source = "A"),ignoreInit = T,{
        req("Custom"%in%input$crop_shapes)
        d <- plotly::event_data("plotly_relayout", source = "A")


        if (!is.null(d)) {
          req(length(d)==4)

          xmin <- d[[1]]
          req(is.numeric(xmin))
          xmax <- d[[2]]
          req(is.numeric(xmax))
          ymin <- d[[3]]
          req(is.numeric(ymin))
          ymax <- d[[4]]
          req(is.numeric(ymax))
          rect_coords(list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))

          if("Custom"%in%input$crop_shapes)
            plotly::plotlyProxy("full_shape_plot") %>%
            plotly::plotlyProxyInvoke("relayout", list(

              shapes = list(
                list(
                  type = "rect",
                  x0 = xmin,
                  x1 = xmax,
                  y0 = ymin,
                  y1 = ymax,
                  line = list(color = "red",dash ="dash",width =4)
                )
              )
            ))

        }


      })
    })

    output$shp_warning<-renderUI({
      req(shape4_final())
      req(!is.null(shape4_final()))
      req(length(shape4_final())>0)
      shp<-shape4_final()
      req(!nrow(shp)>0)
      div(
        render_warning(list(
          span(strong("New Shape"),strong(embrown("is empty"))),
          span(emgray("No features remain in the New Shape within the",embrown(paste(input$crop_shapes,collapse=", ")),emgray("limits."))),
          span(emgray('Consider expanding your crop area to New Shape or use the Custom crop option'))
        ))
      )
    })
    output$final_shape_plot<-renderUI({
      req(shape4_final())
      req(!is.null(shape4_final()))
      req(length(shape4_final())>0)
      shp<-shape4_final()
      req(shp)
      #req(!identical(shp,shape2_prep()))
      if(!nrow(shp)>0){
        return(
          emgray("Final Shape is empty")
        )
      }
      div(class="plot200 shp_p2",
          div(
            div(strong("New Shape:"),emgreen(title_shape())),
            div(strong("Target Datalist:"),emgreen(input$shp_datalist))
          ),
          renderPlot({
            plot_shape(shp)
          },width =300, height=150)
      )
    })
    observeEvent(list(input$shp_feature1,input$shp_feature2,input$crop_shapes,rect_coords()),{
      shape4_final(NULL)
      shinyjs::addClass("prepare_btn","save_changes")
    })
    current_shapes_list<-reactive({
      shp<-shape2_prep()
      data<-data_shp()
      req(input$shp_feature1)
      if(c(input$shp_feature1)!="None"){
        req(input$shp_feature2)
        shp<-filtered_shp()

      }
      # attr(data,input$shp_include)<-shp
      base_shape<-attr(data,"base_shape")
      layer_shape<-attr(data,"layer_shape")
      extra_shape<-attr(data,"extra_shape")
      result<-c(list(base_shape=base_shape,layer_shape=layer_shape),extra_shape,list("New Shape"=shp))
      pic<-which(sapply(result,length)>0)
      shape_list<-result[pic]
      req(length(shape_list)>0)
      shape_list
    })

    get_shape_lims<-reactive({

      lims<-get_limits_shapes()

      req(lims)
      req(nrow(lims)>0)


      if("Custom"%in%input$crop_shapes){
        req(length(input$crop_shapes)==1)
        {
          lims<-lims['New Shape',,drop=F]
        }
      } else{
        lims<-lims[input$crop_shapes,,drop=F]
      }

      req(nrow(lims)>0)
      lims_result<-try({
        if(nrow(lims)==1){
          lims
        } else{
          apply(lims,2,range, na.rm=T)
        }
      },silent = T)
      req(!inherits(lims_result,"try-error"))


      lims_result<-as.matrix(lims_result)
      req(is.matrix(lims_result))


      xmin=min(as.numeric(unlist(lims_result[,1])))
      xmax=max(as.numeric(unlist(lims_result[,3])))
      ymin=min(as.numeric(unlist(lims_result[,2])))
      ymax=max(as.numeric(unlist(lims_result[,4])))
      lims<-list(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)


      return(lims)

    })

    observeEvent(data_shp(),{

      new=get_new_shape_attr()
      print(new)
      updatePickerInput(session,"shp_include",selected=new)
    })


    title_shape<-reactive({
      req(input$shp_include)

      switch(input$shp_include,
             "base_shape"="Base-Shape",
             "layer_shape"="Layer-Shape",
             "extra_shape"="Extra-Shape")
    })
    observe({
      current_shape<-attr(data_shp(),input$shp_include)
      shinyjs::toggle("trash_open",condition=!is.null(current_shape))
    })

  })
}
tool2_tab9<-list()
tool2_tab9$ui<-function(id){
  ns<-NS(id)
  div(
    tags$script(HTML("
      $(document).on('keydown', function(event) {
        if (event.ctrlKey && event.key === 'r') {
          event.preventDefault(); // Impede a ação padrão do Ctrl+R (recarregar a página)
          $('.code_run').first().click(); // Simula um clique no primeiro botão com a classe 'code_run'
        }
      });
    ")),
    tags$script(HTML("
      $(document).on('keydown', function(event) {
        if (event.ctrlKey && event.key === 'l') {
          event.preventDefault(); // Impede a ação padrão do Ctrl+R (recarregar a página)
          $('.clear_script').first().click(); // Simula um clique no primeiro botão com a classe 'clear_script'
        }
      });
    ")),
    h5("Run custom scripts", actionLink(ns("code_help"),icon("fas fa-question-circle"))),
    div(
      div(class="code-style",
          textAreaInput(ns("code"), "Script:", value ="names(saved_data)", width = '500px',height = '150px', cols = NULL, rows = NULL, placeholder = "names(saved_data)",resize = "both")
      )
    ),
    div(class="pp_input",align="right",style="width: 500px; margin-top: -5px",

        actionLink(ns("run_code"),"[run]",class="code_run",icon("angles-right")),
        em("(Ctrl+R)",style="font-size: 10px")
    ),
    div(class="pp_input",align="left",style="width: 500px; margin-top: -15px",
        actionLink(ns("clear_script"),"[clear]",class="clear_script"),
        em("(Ctrl+L)",style="font-size: 10px")),
    div(style="overflow: auto; max-height: 250px",
        tags$label("Console:"),
        uiOutput(ns("print_code"))
    )
  )
}
tool2_tab9$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    r_code<-reactiveVal()
    observeEvent(input$run_code,{
      try({

        saved_data<-vals$saved_data
        t<-try({eval(parse(text = input$code))})
        if(inherits(t,"try-error")){
          for(i in seq_along(t)){
            t[i]<-gsub(".*input\\$code\\)\\) ","",t[i])
          }

        }
        r_code(t)

      })
    })
    observeEvent(input$clear_script,{
      r_code(NULL)
      updateTextAreaInput(session,'code',value=NA)
    })
    observeEvent(input$code_help,{
      showModal(
        modalDialog(
          div(
            p(
              div(class="code_help",
                  h4("Run Custom R Scripts"),
                  div("Execute custom R scripts using user-created Datalists within iMESc. Saved Datalists are accessible from the ", code("saved_data"), " object."),
                  div("Example: ")),
              div(class = "custom-div",
                  code("names(saved_data)"), span(" #Lists the names of the Datalists.",class="comment")),
              div(class = "custom-div",
                  div(code('attr(saved_data[["nema_araca"]],"factors")'),
                      span(class="comment","#acess the Factor-Attribute, where 'nema_araca' is the Datalist name")),
                  div(code('attr(saved_data[["nema_araca"]],"coords")'),
                      span(class="comment","#acess the Coords-Attribute"))
              )),
            hr(),
            p(     h4("Modify permanently iMESc objects"),
                   div(class="code_help",div("Users can also modify Datalists permanently using ", code("vals$saved_data"), "."),
                       div("Example: ")),
                   div(class = "custom-div",
                       code('names(vals$saved_data)[1] <- "new name"'), span(class="comment","#Modifies the name of the first Datalist.")),

                   div(class = "alert_warning",
                       strong("Caution:"), " These modifications are permanent and cannot be undone. Proceed with caution."
                   )
            )
          )
        )
      )
    })

    output$print_code<-renderUI({
      renderPrint(r_code())
    })
  })
}
tool2_tab10<-list()
tool2_tab10$ui<-function(id){
  ns<-NS(id)
  div(
    div(strong("Datalist manager")),
    uiOutput(ns("teste")),

    uiOutput(ns('datalist_total_size')),
    div(
      class="mp0",id=ns("tree_page"),style="display: flex; overflow-y: auto; max-height: calc(100vh - 200px)",
      div(class="mp0",
          shinyTree::shinyTree(ns("tree_gen"),checkbox=T,animation   =F,contextmenu=F, multiple=T)),
      div(class="mp0",
          uiOutput(ns("print_tree_gen")),
          div(align="right",
              hidden(actionButton(ns("remove"),icon("trash")))
          ))
    )
  )
}
tool2_tab10$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    output$tree_gen<-  shinyTree::renderTree({

      attrlist<-getTree_saved_data(vals,F,imesc_attrs,imesc_models)

      attrlist
    })
    full_datalist<-reactive({
      result<-F
      res<-sapply(input$tree_gen,function(x){
        re<-sapply(x,function(xx) length(attr(x,'stselected')))
        sum(re)==length(x)
      } )
      if(any(res)){
        if(length(res)>0){
          result<-which(res)
        }
      }

      return(result)
    })
    any_numfac<-reactive({
      df<-df_selected()[1:2]

      numfac<-df$attr%in%c("numeric","factors")
      res<-if(any(numfac)) {
        list(
          div(    class = "alert_warning",
                  div(strong(icon("triangle-exclamation",style="color: gold"),"Warning:","You have chosen to exclude",embrown(paste(df$attr[numfac],collapse = "/")),"attributes.")),
                  div(em("A DataList requires both numeric and factor attributes")),
                  div(em("Click 'Confirm' to delete the entire Datalist")))
        )

      } else{
        NULL
      }

    })
    observeEvent(input$remove,ignoreInit = T,{
      if(!isFALSE(full_datalist())){
        action<-div(any_numfac(),
                    em(lapply(unique(df_selected()$datalist),div))
        )
        left="Are you sure you want to delete the selected DataLists?"
      } else{
        df<-df_selected()[1:2]
        action<-div(any_numfac(),
                    renderPrint(split(df,df$datalist)))
        left=div("Are you sure you want to delete the selected Attributes?")
        numfac<-df$attr%in%c("numeric","factors")

      }
      confirm_modal(session$ns,action="",left=left,div_right =F,div1_post=action

      )
    })

    which_selected<-reactive({
      req(input$tree_gen)
      selall<-sapply(input$tree_gen,function(x)unlist(sapply(x,function(xx) attr(xx,'stselected'))) )
      if(!length(selall)>0){
        return(NULL)
      }
      selall<-do.call(rbind,selall)
      if(!length(selall)>0){
        return(NULL)
      }
      selall
    })
    observe({
      if(length(which_selected())>0){
        shinyjs::show("remove")
      } else{
        shinyjs::hide("remove")
      }
    })
    all_selected<-reactive({
      selall<-which_selected()
      req(length(selall)>0)
      div(style="font-size: 11px",
          lapply(1:nrow(selall),function(i){
            if(all(selall[i,])){
              rowname<-rownames(selall)[i]
              a<-"Datalist:"
              if(rowname=="Ensemble"){
                a<-"Ensembles"
                ob<-format(object.size(vals$saved_ensemble),"auto")
              } else{
                ob<-format(object.size(vals$saved_data[[rowname]]),"auto")
              }
              div(a,paste0(rowname,":"),strong(emgreen(ob)))
            }
          })
      )

    })
    df_selected<-reactive({
      sel_table<-selected_Tree_attrs(input$tree_gen)
      req(!is.null(sel_table))
      res<-lapply(1:nrow(sel_table),function(i){

        args<-as.list(sel_table[i,])
        args$vals<-vals
        do.call(get_attr_imesc,args)
      })
      df<-data.frame(do.call(rbind,res))
      df
    })

    observeEvent(input$confirm,ignoreInit = T,{
      run_remove()

      removeModal()
    })

    # vals<-readRDS("savepoint.rds")
    run_remove<-reactive({

      df<-df_selected()

      full<-subset(df,attr%in%c("numeric","factors"))
      fulldel<-unique(full$datalist)
      if(length(fulldel)>0){
        pic<-which(df$attr%in%c("numeric","factors"))
        todel<-df$datalist[pic]


        for(i in 1:length(fulldel)){vals$saved_data[[fulldel[i]]]<-NULL}
        df<-df[-which(df$datalist%in%todel),]

      }
      if(nrow(df)>0){
        for(i in 1:nrow(df)){
          if(df$datalist[i]=="Ensemble"){
            vals$saved_ensemble[[df$attr[i]]]<-NULL
          }
          if(length(vals$saved_ensemble)==0){
            vals$saved_ensemble<-NULL
          }
        }
      }
      pic<-which(df$datalist=='Ensemble')
      if(length(pic)>0){
        df<-df[-pic,]
      }

      if(nrow(df)>0){
        dfmodels<-subset(df,attr%in%imesc_models)
        if(nrow(dfmodels)>0){
          for(i in 1:nrow(dfmodels)){
            datalist<-dfmodels$datalist[i]
            attr<-dfmodels$attr[i]
            attr(vals$saved_data[[datalist]],attr)[[dfmodels$model_name[i]]]<-NULL
          }
          if(!length(attr(vals$saved_data[[datalist]],attr))>0)
            attr(vals$saved_data[[datalist]],attr)<-NULL

          df<-subset(df,!attr%in%imesc_models)
        }


      }

      if(nrow(df)>0){
        for(i in 1:nrow(df)){
          datalist<-df$datalist[i]
          attr<-df$attr[i]
          attr(vals$saved_data[[datalist]],attr)<-NULL
        }
      }

    })


    output$print_tree_gen<-renderUI({
      req(df_selected())
      req(length(df_selected())>0)

      div(
        div(class="half-drop-inline small_table",style="overflow:scroll; max-height: 300px",
            all_selected(),
            renderTable({df_selected()[c(1,2,4,3,5)]})
        )
      )
    })

  })
}
tool2_tab11<-list()
tool2_tab11$ui<-function(id){
  ns<-NS(id)
  div(class="p10",
      div(strong("Delete Datalists")),
      div(style="display: flex",
          div(virtualPicker(ns("deldatalist"),"Datalists(s) selected")),
          div(id=ns('run_deldatalist_btn'),
              actionButton(ns("run_deldatalist"),icon("trash")))
      )
  )
}
tool2_tab11$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    observeEvent(vals$saved_data,{
      shinyWidgets::updateVirtualSelect('deldatalist',choices=names(vals$saved_data))
    })

    observeEvent(input$deldatalist,{
      req(input$deldatalist!="")
      shinyjs::addClass('run_deldatalist_btn',"save_changes")
    })
    action<-reactive({
      span("Remove",strong(embrown(length(input$deldatalist))),"Datalists")
    })
    observeEvent(input$run_deldatalist,ignoreInit = T,{
      #
      confirm_modal(session$ns,action=h4("Are you sure?"),
                    data1=NULL,
                    data2= NULL,
                    arrow=F,
                    left=action(),right="",
                    from='',
                    to='')

    })

    observeEvent(input$confirm,{
      shinyjs::removeClass('run_deldatalist_btn',"save_changes")
      vals$saved_data[input$deldatalist]<-NULL
      removeModal()
    })


  })
}

virtualPicker<-function(id,SelectedText="IDs selected", label=NULL,choices=NULL,selected=NULL){
  div(class="picker_open",
      shinyWidgets::virtualSelectInput(
        inputId = id,
        label = label,
        optionHeight='24px',
        choices = choices,
        selected=selected,
        search = TRUE,
        keepAlwaysOpen = TRUE,
        multiple =T,
        hideClearButton=T,
        alwaysShowSelectedOptionsCount=T,
        searchPlaceholderText="Select all",
        optionsSelectedText=SelectedText,
        optionSelectedText=SelectedText
      )
  )
}


get_scale<-function(data,scale,center){
  data0<-data
  if(isTRUE(scale)){
    data<-data[which(sapply(data,function(x) var(x,na.rm=T))>0)]
    req(nrow(data)>0)
    req(ncol(data)>0)
    scaled<-scale(data,center,scale)
    sc<-attr(scaled,"scaled:scale")
    ct<-attr(scaled,"scaled:center")
    df_scale<-data.frame(scaled)
    attr(df_scale,"scaled:scale")<-sc
    attr(df_scale,"scaled:center")<-ct
    attr(df_scale,"transf")<-c(attr(data0,"transf"),
                               scale=scale,
                               center=center)
    df_scale
  } else{
    data
  }


}

check_is_integer <- function(data) {
  sapply(data, function(column) {
    all(column==round(column))
  })
}
tool3$ui<-function(id){
  tips<-list("Remove rows with missing values",
             "Keep only rows matching IDs from another dataset",
             "Filter rows by selected factors.",
             "Select or remove rows manually.",
             "Remove rows with zero variance")
  tips<-get_tips(tips)

  ns<-NS(id)
  div(id="tool3",class="tools_content",
      div(class="nav-tools",
          tags$head(tags$script(HTML('$(function () { $("[data-toggle=\'tooltip\']").tooltip(); });'))),
          navlistPanel(
            id=ns("tabs_tool3"),
            title= NULL,  widths=c(5, 7),
            tabPanel(span("Individual row selection", tips[4]),value="tab4",
                     uiOutput(ns("update_tab4")),
                     div(
                       div(class="picker_open",
                           virtualPicker(ns("selecobs"))

                       )
                     ),
                     uiOutput(ns("print_selecobs"))),
            tabPanel(
              title=span('Remove NAs', tips[1]),value="tab1",
              hidden(checkboxInput(ns("na.omit"), span("NA.omit"), value=F)),
              uiOutput(ns("print_na"))
            ),
            tabPanel(
              title=span('Remove Zero Variance', tips[5]),value="tab5",
              checkboxInput(ns("zero_var"), span("Remove Zero Var"), value=F),
              uiOutput(ns("print_zero"))
            ),
            tabPanel(span("Match IDs with Datalist", tips[2]),value="tab2",
                     div(class="half-drop",
                         p(em()),
                         uiOutput(ns('sync_ids')),
                         uiOutput(ns("print_sync")),
                         hidden(actionLink(ns("unsync_ids"),"Display Unmatched IDs")),
                         uiOutput(ns("update_tab2")),
                     )),
            tabPanel(
              span("Filter by factors", tips[3]),value="tab3",
              div(style="height: 350px; background-color: #e3e3e3;",
                  class="tool3_tab3",
                  tabsetPanel(
                    NULL,
                    tabPanel("Tree",div(
                      div(
                        div(style="height: 250px; overflow-y:scroll;
                    -webkit-box-shadow: inset 0px 0px 5px #797979ff;",
                    div("Click on the Nodes ",tipify( icon(verify_fa=FALSE,name=NULL,class="fas fa-question-circle"),"Click on the nodes to expand and select the factor levels. Only available for factors with less than 100 levels"),
                        uiOutput(ns("validate_tab3")),
                        shinyTree::shinyTree(ns("tree"), checkbox=TRUE,themeIcons=F,themeDots=T)))),
                    uiOutput(ns("print_tree"))
                    )),


                    tabPanel("Subset",
                             div(class="half-drop-inline",
                                 tags$head(tags$script(
                                   "Shiny.addCustomMessageHandler(\"testmessage\",
                                    function(message) {
                                    var x = document.getElementsByClassName(\"filter-option pull-left\");
                                    x[0].innerHTML = message;
                                    }
      );"
                                 )),
      div(class="tool_subset",

          pickerInput(ns("subset_factor"),"Factor",choices=NULL),
          pickerInput(ns("subset_level"),"Levels",
                      choices=NULL,
                      multiple=T,
                      options = list(
                        liveSearch = TRUE,
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 0",
                        `count-selected-text` = "{0}/{1} selected"
                      ),



          )


      ),
      uiOutput(ns('update_tab3_subset')),
      uiOutput(ns('print_subset'))
                             )
                    )
                  )
              )
            )
          ),
      div(style="position: absolute; left: 10px; top: 200px; width: 200px",
          uiOutput(ns("zero_var_print"))
      )

      ))
}
tool3$update_server<-function(id, vals=NULL){
  moduleServer(id,function(input,output,session){





    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })
    factors<-reactive({
      attr(data(),"factors")
    })




  })
}
tool3$server<-function(id, vals=NULL){
  moduleServer(id,function(input,output,session){

    observeEvent(input$zero_var,{
      if(isTRUE(input$zero_var)){
        ids_zv<-rownames(data())[apply(data(),1, function(x) var(x,na.rm=T))==0]
      } else{
        ids_zv<-NULL
      }
      vtool3$rm_zv<-ids_zv

    })

    observe({
      shinyjs::toggle('zero_var', condition = any(apply(data(),1, function(x) var(x,na.rm=T))==0))

    })


    output$print_zero<-renderUI({


      div(
        if(!any(apply(data(),1, function(x) var(x,na.rm=T))==0)){
          em("No rows with zero variance were detected in the data.",style="color: gray")
        },
        if(isTRUE(input$zero_var))
          renderPrint({
            data.frame("Removed IDs"=vtool3$rm_zv)

          })
      )
    })

    output$zero_var_print<-renderUI({
      req(any(apply(data(),1, function(x) var(x,na.rm=T))==0))


      div(
        class = "alert_warning",
        icon("triangle-exclamation",style="color: Dark yellow3"),
        "Warning: Some observations (rows) exhibit zero variance. You can remove them using the 'Remove Zero Variance Tool'."
      )

    })
    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })
    factors<-reactive({
      attr(data(),"factors")
    })

    choices_rows<-reactive({
      rownames(data())
    })




    output$sync_ids<-renderUI({
      selectInput(session$ns("sync_ids"),"Choose a Datalist for ID matching",names(vals$saved_data))
    })

    observeEvent(choices_rows(),{


      shinyWidgets::updateVirtualSelect('selecobs',choices=choices_rows(), selected=choices_rows())

    })

    observeEvent(factors(),{
      updatePickerInput(session,'subset_factor',choices=colnames(factors()))
    })


    valid_levels<-reactive({
      factor_levels<-get_faclevels(factors())
      which(unlist(lapply(factor_levels, function(x) length(x[[1]])))<100)

    })






    factor_tree<-reactiveVal()
    vtool3<-reactiveValues()
    #tab1####
    observeEvent(input$na.omit,{
      if(isTRUE(input$na.omit)){
        ids_rm<-names(which(apply(is.na(data()),1, any)))
      } else{
        ids_rm<-NULL
      }
      vtool3$rm_na<-ids_rm

    })
    output$print_na<-renderUI({
      div(
        if(!anyNA(data())){
          em("No missing values (NA) detected in the data.",style="color: gray")
        },
        renderPrint(c("Removed IDs"=length(vtool3$rm_na)))
      )
    })
    observe({
      if(anyNA(data())){
        shinyjs::show('na.omit')
      } else{
        shinyjs::hide('na.omit')
      }
    })
    observeEvent(input$sync_ids,{
      req(input$sync_ids)
      match<-rownames(data())%in%rownames(vals$saved_data[[input$sync_ids]])
      row_match(match)
    })
    observeEvent(row_match(),{
      if(any(!row_match())){
        shinyjs::show('unsync_ids')
      } else{
        shinyjs::hide('unsync_ids')
      }
    })
    observeEvent(input$unsync_ids,{
      showModal(
        modalDialog(
          easyClose=T,
          print_unsync()
        )
      )
    })

    #tab2####

    print_unsync<-reactive({
      req(any(!row_match()))
      primary_datalist<-attr(data(),"datalist_root")
      secondary_datalist<-input$sync_ids

      div(
        span(
          em(primary_datalist,style="color: SeaGreen"),
          "does not contain the following IDs from",
          em(secondary_datalist,style="color: SeaGreen"), ":"
        ),
        div(style="height: 200px; overflow-y:scroll",
            renderTable(
              data.frame(unmached_is=vtool3$rm_match)
            )
        )

      )
    })
    match_summary<-reactive({
      req(input$sync_ids)
      req(length(row_match())>0)
      primary_datalist<-attr(data(),"datalist_root")
      secondary_datalist<-input$sync_ids
      nd1<-nrow(data())
      nd2<-nrow(vals$saved_data[[input$sync_ids]])
      matches<-sum(row_match())
      nonmaches<-sum(!row_match())

      data.frame(Datalist=c(primary_datalist,secondary_datalist,"",""),
                 row_count=c(nd1,nd2,matches,nonmaches),
                 row.names=c("Primary",
                             "Secondary",
                             "Matched IDs",
                             "Removed IDs")

      )

    })
    row_match<-reactiveVal()
    observeEvent(row_match(),{
      vtool3$rm_match<-rownames(data())[!row_match()]
    })
    output$print_sync<-renderUI({
      req(input$sync_ids)

      renderPrint(match_summary())
    })




    #tab3####

    output$validate_tab3<-renderUI({
      req(  length(valid_levels)<ncol(factors()))
      invalid<-colnames(factors())[-valid_levels()]
      div(
        popify(actionLink("dummy",icon("triangle-exclamation"), style="color: gold"),NULL,
               HTML(paste0(
                 p("Selection of the following factors is disabled within the tree because they contain more than 100 levels"),
                 do.call(paste0,lapply(invalid,function(x) p(HTML(paste0(em(x))))))
               )),trigger="click"
        )



      )

    })
    output$tree<-renderTree({

      req(length(valid_levels())>0)
      factors<-factors()[valid_levels()]

      req(factors)
      factor_name<-colnames(factors)[1]
      lev<-list()
      for(factor_name in colnames(factors)){
        lev_sub<-list()
        levels_factor<-unique(as.character(factors[,factor_name]))
        level<-levels_factor[[1]]
        for(level in levels_factor){
          lev_sub[[level]]<-""
        }
        lev[[factor_name]]<-lev_sub
        attr(lev[[factor_name]],"stselected")<-T
      }

      lev


    })
    output$print_tree<-renderUI({
      renderPrint(c("Removed IDs"=length(vtool3$rm_tree)))
    })
    observeEvent(input$tree,{
      seltree<-get_selected(input$tree)
      if(!length(seltree)>0){
        vtool3$rm_tree<-rownames(factors())
        return()
      }
      selected=get_selfactors(res=seltree,factors())
      factor_ids<-rownames(factors())
      rm_ids<-factor_ids[!factor_ids%in%selected]
      vtool3$rm_tree<-rm_ids
    })

    observeEvent(input$subset_factor,{
      req(input$subset_factor%in%colnames(factors()))
      choices<-levels(factors()[,input$subset_factor])
      updatePickerInput(session,'subset_level',choices=choices, selected=choices)
    })
    rm_subset<-reactiveVal()
    observeEvent(input$subset_level,{
      req(input$subset_factor%in%colnames(factors()))
      choices<-levels(factors()[,input$subset_factor])
      req(input$subset_level%in%choices)
      fac<-factors()[,input$subset_factor]
      rm_ids<-rownames(factors())[!fac%in%input$subset_level]
      vtool3$rm_subset<-rm_ids

    })
    output$print_subset<-renderUI({
      renderPrint(removed_ids(vtool3$rm_subset))
    })


    observeEvent(input$selecobs,{
      ids<-rownames(data())
      res<-if(is.null(input$selecobs)){
        ids
      } else{
        ids_rm<-ids[!ids%in%input$selecobs]
        ids_rm
      }
      vtool3$rm_row<-res



    })
    output$print_selecobs<-renderUI({
      div(

        renderPrint(c("Removed IDs"=length(vtool3$rm_row)))
      )

    })
    result_v3<-reactive({
      list(
        rm_na=vtool3$rm_na,
        rm_match=vtool3$rm_match,
        rm_tree=vtool3$rm_tree,
        rm_subset=vtool3$rm_subset,
        rm_row=vtool3$rm_row,
        rm_zv=vtool3$rm_zv
      )
    })
    v3_data<-reactive({
      data<-vals$pp_data

      # saveRDS(result_v3(),"result_v3.rds")
      if(!any(sapply(result_v3(),length)>0)){
        return(data)
      }
      ids<-unique(unlist(result_v3()))
      pic<-which(rownames(data)%in%ids)

      if(length(pic)>0){
        newdata<-data[-pic,,drop=F]
        data<-data_migrate(data,newdata)
      }
      data
    })

    observeEvent(v3_data(),{
      vals$vtools$tool3<-v3_data()
    })



    return(NULL)

  })
}


tool4$ui<-function(id){
  tips<-list("Remove numeric variables contributing less than a specified percentage of the total sum across all observations.",
             "Manually select numeric variables based on column names.",
             "Removes highly correlated variables using findCorrelation function from the caret package",
             "Identifies and eliminates variables with near-zero variance using the nearZeroVar function from the caret package.",
             "Remove columns with zero variance")
  tips<-get_tips(tips)
  ns<-NS(id)
  div(class="nav-tools",
      navlistPanel(
        id=ns("tabs_tool4"),NULL,  widths=c(5, 7),
        tabPanel(span("Individual selection", tips[2]),
                 div(virtualPicker(ns("selecvar"),"Variable(s) selected")),
                 uiOutput(ns('print_selcol'))
        ),
        tabPanel(span("Value-based removal", tips[1]),
                 div(class="check_input",
                     div(
                       span(tipify( icon(verify_fa=FALSE,name=NULL,class="fas fa-question-circle"),"Remove columns where the values are less than x-percent of their cumulative total","bottom"),
                            inline(checkboxInput(ns("rareabund"),'Abund<',F, width="80px")),
                            inline(hidden(div(id=ns("pc1"),inline(numericInput(ns('pct_abund'), NULL, value =0.1, width="100px")),'%'))))
                     ),
                     uiOutput(ns('print_rareabund')),
                     div(
                       span(tipify( icon(verify_fa=FALSE,name=NULL,class="fas fa-question-circle"),"Remove variables occurring in less than  x-percent of the number of samples"),
                            inline(checkboxInput(ns("rarefreq"),"Freq<",F, width="80px")),
                            inline(hidden(div(id=ns("pc2"),inline(numericInput(ns('pct_freq'), NULL, value =0.1, width="100px")),"%")))
                       ),
                       uiOutput(ns('print_rarefreq'))
                     ),
                     div(
                       tipify( icon(verify_fa=FALSE,name=NULL,class="fas fa-question-circle"),"Requires a counting data. Remove variables occurring only once"),
                       inline(checkboxInput(ns("raresing"),"Singletons",F, width='100px'))
                     ),
                     uiOutput(ns('print_raresing'))
                 )
        ),
        tabPanel(span("Correlation based", tips[3]),
                 div(class="half-drop",style="padding-top: 10px",
                     div(span("Correlation-based remotion",tiphelp("Removes variables using the function findCorrelation from caret package. The absolute values of pair-wise correlations are considered. If two variables have a high correlation, the function looks at the mean absolute correlation of each variable and removes the variable with the largest mean absolute correlation. Argument exact is fixed TRUE, which means that the function re-evaluates the average correlations at each step.")),
                         div(style="display: flex",
                             selectInput(ns("cor_method"),
                                         span("Method",tiphelp("correlation coefficient to be computed")),choices= c("pearson", "kendall", "spearman"),width ="30%"),
                             numericInput(ns("cor_cutoff"),span("Cutoff",tiphelp("The pair-wise absolute correlation cutoff","right")),value =0.9,min=0.1,max=1,step=.1,width ="30%")),
                         div(style="display: flex",
                             selectInput(ns("cor_use"), span("Use",tiphelp("method for computing covariances in the presence of missing values","right")),choices=c( "complete.obs","everything", "all.obs", "na.or.complete", "pairwise.complete.obs"),width ="150px"),
                             div(id=ns('run_cor_btn'),class="save_changes",style="padding-top: 25px; padding-left: 5px",
                                 actionButton(ns("run_cor"),"RUN >>")
                             )),
                         div(icon(verify_fa=FALSE,name=NULL,class="fas fa-lightbulb"),"Explore the correlation plot in the Descriptive tools menu"
                         ),
                         uiOutput(ns("correlation_out"))
                     )
                 )
        ),
        tabPanel(
          title=span('Remove Zero Variance', tips[5]),value="tab5",
          checkboxInput(ns("zero_var"), span("Remove Zero Var"), value=F),
          uiOutput(ns("print_zero"))
        ),
        tabPanel(span("Near zero variance", tips[4]),
                 div(class="pp_input",
                     div(style="padding-top: 10px",
                         span(
                           strong("Remove near zero variance"),
                           actionLink(ns("nzv_help"),icon("fas fa-question-circle"))
                         )
                     ),
                     div(style="display: flex",
                         numericInput(ns("freqCut"),span("+ freqCut",tiphelp("the cutoff for the ratio of the most common value to the second most common value","right")),value =95/5,min=0.1,max=1,step=.1),
                         numericInput(ns("uniqueCut"),span("+ uniqueCut",tiphelp("the cutoff for the ratio of the most common value to the second most common value","right")),value =10,min=0.1,max=1,step=.1),
                         div(id=ns('cut_nvz_btn'),class="save_changes",style="padding-top: 30px; padding-left: 5px",
                             actionButton(ns("cut_nvz"),"Cut >>")
                         )),
                     uiOutput(ns('nzv_message'))
                 )
        )

      ),
      div(style="position: absolute; left: 10px; top: 200px; width: 200px",
          uiOutput(ns("zero_var_print"))
      )
  )
}
tool4$server<-function(id,vals=NULL){

  moduleServer(id,function(input,output,session){

    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })

    observeEvent(input$zero_var,{
      if(isTRUE(input$zero_var)){
        ids_zv<-colnames(data())[apply(data(),2, function(x) var(x,na.rm=T))==0]
      } else{
        ids_zv<-NULL
      }
      rm_zv(ids_zv)

    })

    observe({
      shinyjs::toggle('zero_var', condition = any(apply(data(),2, function(x) var(x,na.rm=T))==0))

    })
    rm_zv<-reactiveVal()

    output$print_zero<-renderUI({

      div(
        if(!any(apply(data(),2, function(x) var(x,na.rm=T))==0)){
          em("No columns with zero variance were detected in the data.",style="color: gray")
        },
        if(isTRUE(input$zero_var))
          renderPrint({
            data.frame("Removed cols"=rm_zv())

          })
      )
    })

    output$zero_var_print<-renderUI({
      req(any(apply(data(),2, function(x) var(x,na.rm=T))==0))


      div(
        class = "alert_warning",
        icon("triangle-exclamation",style="color: Dark yellow3"),
        "Warning: Some variables (columns) exhibit zero variance. You can remove them using the 'Remove Zero Variance Tool'."
      )

    })

    observeEvent(data(),{
      choices<-colnames(data())
      shinyWidgets::updateVirtualSelect('selecvar',choices=choices, selected=choices)
    })



    rm_rareabund<-reactiveVal()
    rm_rarefreq<-reactiveVal()
    rm_raresing<-reactiveVal()
    rm_col<-reactiveVal()

    observeEvent(input$selecvar,{
      ids<-colnames(data())
      if(is.null(input$selecvar)){

        rm_col(ids)
      } else{
        ids_rm<-ids[!ids%in%input$selecvar]
        rm_col(ids_rm)
      }
    })

    output$print_selcol<-renderUI({
      div(
        renderPrint(c("Removed cols"=length(rm_col())))
      )

    })
    output$print_rareabund<-renderUI({
      req(input$rareabund)
      renderPrint(removed_cols(rm_rareabund()))
    })
    output$print_rarefreq<-renderUI({
      req(input$rarefreq)
      renderPrint(removed_cols(rm_rarefreq()))
    })
    output$print_raresing<-renderUI({
      req(input$raresing)
      validate(need(is_binary_df(data()),"The Singletons option requires a counting data"))
      renderPrint(removed_cols(rm_raresing()))
    })

    observeEvent(list(input$rareabund,input$pct_abund),{

      if(isTRUE(input$rareabund)){
        cols_rm<-pp_pctAbund(data(), input$pct_abund/100)
        rm_rareabund(cols_rm)
      } else{
        rm_rareabund(NULL)
      }
    })
    observeEvent(list(input$rarefreq,input$pct_freq),{
      if(isTRUE(input$rarefreq)){
        cols_rm<-pp_pctFreq(data(), input$pct_freq/100)
        rm_rarefreq(cols_rm)
      } else{
        rm_rarefreq(NULL)
      }
    })
    observeEvent(input$raresing,{
      if(isTRUE(input$raresing)){
        cols_rm<-pp_singles(data())
        rm_raresing(cols_rm)
      } else{
        rm_raresing(NULL)
      }
    })
    output$print_valuebased<-renderUI({

    })
    output$print_cols<-renderUI({

    })
    observeEvent(select_subtool(),{
      shinyjs::hide(selector=".show-on")
      shinyjs::show(select_subtool())
    })
    select_subtool<-reactiveVal()
    observeEvent(input$show_selval,{
      select_subtool("on_selval")

    })
    observeEvent(input$show_selcol,{
      select_subtool("on_selcol")
    })
    observeEvent(input$show_cor,{
      select_subtool("on_cor")
    })
    observeEvent(input$show_nzv,{
      select_subtool("on_nzv")
    })
    nzv_df<-reactiveVal()
    nzv_val<-reactiveVal()
    args_nzv<-reactive({
      req(input$freqCut)
      req(input$uniqueCut)
      list(
        x=data(),
        freqCut=input$freqCut,
        uniqueCut=input$uniqueCut,
        saveMetrics=T
      )
    })
    observeEvent(args_nzv(),{
      shinyjs::addClass("cut_nvz_btn","save_changes")
      nzv_df(NULL)
      nzv_val(NULL)
    })
    observeEvent(input$cut_nvz,{
      nz<-do.call(nearZeroVar,args_nzv())
      nz<-nz[order(rownames(nz),nz$nzv),]
      nzv_df(nz)
      shinyjs::removeClass("cut_nvz_btn","save_changes")
      nearzero<-nzv_df()
      res<-rownames(nz)[which(nearzero$nzv)]
      if(!length(res)>0){
        res<-NULL
      }
      nzv_val(sort(res))

    })
    war_nzv<-reactive({
      req(nzv_df())
      if(length(nzv_val()) > 0){
        div(
          span(strong("Result:"), em(length(nzv_val()), " variable(s) identified for removal due to near-zero variance. Confirm and save your changes to permanently remove these variables."), style="white-space: normal;")
        )
      } else {
        div(
          "No variables with near-zero variance detected."

        )
      }
    })
    output$nzv_message<-renderUI({
      req(nzv_df())
      div(style="font-size: 11px; background: white",
          emgray(war_nzv()),
          div(style="max-height: calc(100vh - 300px);overflow-y: scroll; font-size: 11px",
              strong("NZV results:"),
              renderTable({
                df<-nzv_df()
                df[order(df$nzv, decreasing=T),]
              }, rownames=T)

          )
      )
    })


    observeEvent(input$rareabund,{
      if(isTRUE(input$rareabund)){
        shinyjs::show("pc1")
      } else{
        shinyjs::hide("pc1")
      }
    })
    observeEvent(input$rarefreq,{
      if(isTRUE(input$rarefreq)){
        shinyjs::show("pc2")
      } else{
        shinyjs::hide("pc2")
      }
    })

    observeEvent(ignoreInit=T,input$nzv_help, {
      library(caret)
      pkgs<-"caret"
      citations<-do.call('c',lapply(pkgs, citation))
      citations<-lapply(citations,function(x){format(x, style="html")})



      showModal(
        modalDialog(
          title="Identification of Near Zero Variance Predictors",
          easyClose=TRUE,
          div(style="overflow: auto; max-height: 350px; text-indent: 20px",
              p("The 'nearZeroVar' function from the 'caret' package provides functionality for identifying and removing near zero variance predictors. These predictors have either zero variance (i.e., only one unique value) or very few unique values relative to the number of samples, with a large frequency ratio between the most common and second most common values."),
              p(strong(em("Details provided by the function's help documentation:"))),
              p("For example, a near zero variance predictor could be a variable that has only two distinct values out of 1000 samples, with 999 of them being the same value."),
              p("To be flagged as a near zero variance predictor, two conditions must be met. First, the frequency ratio of the most prevalent value over the second most frequent value (referred to as the 'frequency ratio') must exceed a specified threshold (freqCut). Second, the 'percent of unique values' (the number of unique values divided by the total number of samples, multiplied by 100) must be below another specified threshold (uniqueCut)."),
              p("In the example mentioned above, the frequency ratio would be 999 and the percent of unique values would be 0.0001."),
              p("For certain models, such as naive Bayes, it may be necessary to check the conditional distribution of predictors to ensure that each class has at least one data point."),
              p("The 'nzv' function represents the original version of this functionality."),
              h4("Reference:"),
              div(style="padding-top: 20px",
                  HTML(paste0(citations))
              )
          )
        )
      )
    })
    get_corrdata<-reactiveVal()
    observeEvent(args_cor(),{
      shinyjs::addClass("run_cor_btn","save_changes")
      get_corrdata(NULL)
    })
    args_cor<-reactive({
      req(input$cor_method)
      req(input$cor_cutoff)
      req(input$cor_use)
      list(
        x=vals$pp_data,
        method=input$cor_method,
        use=input$cor_use,
        cutoff =input$cor_cutoff
      )
    })
    cor_val<-reactiveVal()
    corr_message<-reactiveVal()
    observeEvent(input$run_cor, ignoreInit = T,{
      corr_message(NULL)
      try({
        args<-args_cor()
        x<-do.call(cor,args[-4])
        find_cor_result<-findCorrelation(x,input$cor_cutoff, exact=T,names =T)
        if(!length(find_cor_result)>0){
          find_cor_result<-NULL
          corr_message(div(class = "alert_warning","No variables found for removal based on correlation criteria"))
        }
        get_corrdata(find_cor_result)

        shinyjs::removeClass("run_cor_btn","save_changes")


      })
    })



    output$correlation_out<-renderUI({
      if(is.null(get_corrdata())){
        return(corr_message())
      }


      req(get_corrdata())
      rep(input$cor_cutoff)
      pic<-colnames(data())%in%get_corrdata()

      up_corr<-data.frame(var=colnames(data())[pic])
      up_corr$remove<-T
      down_corr<-data.frame(var=colnames(data())[!pic])
      up_corr$remove<-F
      df<-data.frame(rbind(up_corr,up_corr))
      colnames(df)[1]<-"Removed cols"
      div(
        head_data$ui(session$ns("table-corr"),1:3),
        head_data$server('table-corr',df[1])
      )

    })


    result_v4<-reactive({
      list(
        rm_rareabund=rm_rareabund(),
        rm_rarefreq=rm_rarefreq(),
        rm_raresing=rm_raresing(),
        rm_col=rm_col(),
        nzv_val=nzv_val(),
        rm_cor=get_corrdata(),
        rm_zv=rm_zv()
      )
    })
    bag_name<-reactive({
      paste0(
        if(length(input$rareabund)>0) {
          if(isTRUE(input$rareabund)) {paste0("[,-",paste0("Abund<",input$pct_abund,"%"),"]")}
        },
        if(length(input$rarefreq)>0) {
          if(isTRUE(input$rarefreq)) {paste0("[,-",paste0("Freq<",input$pct_freq,"%"),"]")}
        },
        if(length(input$raresing)>0) {
          if(isTRUE(input$raresing)) {paste0("[,-",input$raresing,"]")}
        }

      )
    })

    v4_data<-reactive({
      data<-vals$pp_data
      if(!any(sapply(result_v4(),length)>0)){
        return(data)
      }
      cols<-unique(unlist(result_v4()))
      pic<-which(colnames(data)%in%cols)
      if(length(pic)>0){
        newdata<-data[,-pic, drop=F]
        data<-data_migrate(data,newdata)
        attr(data,"bag")<-bag_name()
      }
      data

    })
    observeEvent(v4_data(),{
      vals$vtools$tool4<-v4_data()
    })

    return(NULL)
  })
}
tool5$ui<-function(id){
  tips_transf<-list(
    "Transform numeric attributes using methods from 'vegan' and 'caret' packages.",
    "Scale and/or center numeric attributes using the 'scale' function from R base.")
  tips_transf<-get_tips(tips_transf)
  transf_label<-sapply(transf_df,function(x) x$label)
  transf_value<-sapply(transf_df,function(x) x$value)
  names(transf_value)<-transf_label

  ns<-NS(id)
  div(style="padding: 15px",
      div(class="half-drop half-drop-inline",
          selectInput(ns("transf"),
                      label=span(strong("Transformation"),actionLink(ns("transf_help"),icon("fas fa-question-circle"))),
                      choices=transf_value),

          uiOutput(ns('print_transf'))


      ),

      div(
        div(style="display: flex",
            checkboxInput(ns("scale"), strong('Scale',tiphelp(" If scale is TRUE then scaling is done by dividing the (centered) columns of x by their standard deviations if center is TRUE, and the root mean square otherwise. If scale is FALSE, no scaling is done.")), F),
            hidden( checkboxInput(ns("center"), span('Center',tiphelp("If center is TRUE then centering is done by subtracting the column means (omitting NAs) of x from their corresponding columns, and if center is FALSE, no centering is done.")), T))
        ),
        hidden(checkboxInput(ns("scale_summary"),"Show Pre/Post Summary",T)),
        div(style="background: white",uiOutput(ns('print_scale')))

      ),
      column(6,div(id=ns('tranf_ord_btn'),
                   div(strong("Order of transformations:")),
                   uiOutput(ns("transf_order")))),
      column(6,class = "alert_warning",
             id=ns('transf_out'),style="display: none; margin-top: -15px",
             uiOutput(ns('message_end')),
             quant$ui(ns("transf_final")),
             uiOutput(ns("print_transf_final"))
      )
  )
}
tool5$server<-function(id,vals){

  moduleServer(id,function(input,output,session){
    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })


    observe({
      toggle("transf_out",condition=isTRUE(input$scale)|input$transf!="None")
    })
    observe({
      shinyjs::toggle('tranf_ord_btn', condition=length(cur_transfs())==2)
    })
    message_prep<-reactiveVal()
    r_transf_before<-reactiveVal()
    r_transf_prep<-reactiveVal()

    stop_transf<-reactiveVal(F)
    r_last<-reactiveVal()
    cur_transfs<-reactiveVal()


    observeEvent(data(),{
      if(ncol(data())>500){
        updateCheckboxInput(session,'transf_summary',value=F)
      }
    })

    observeEvent(vals$vtools$tool5,{
      output$print_transf_final<-renderUI({

        div(

          quant$server('transf_final',data(),round(vals$vtools$tool5,5)))

      })
    })

    output$message_end<-renderUI({
      em(transf_message())
    })
    transf_message<-reactive({
      req(isTRUE(input$scale)|input$transf!="None")
      transfs<-attr(r_transf_end(),"transf")
      men<-gsub("ee","e",paste0(names(transfs),"ed"))
      if('center'%in%names(transfs)){
        men<-men[-which(men=="centered")]
        if(isTRUE(transfs["center"])){
          men[men=="scaled"]<-"scaled/centered"
        }}
      if('transformed'%in%men){
        men[men=="transformed"]<-paste(input$transf, "transformed")
      }
      final_message<-if(length(men)==1){
        paste("Data was",men[1])
      } else if(length(men)==2){
        paste0(paste("Data was first",men[1]),paste(', then',men[length(men)]))
      }
      final_message
    })
    observeEvent(input$transf,ignoreInit = T,{
      res<-if(input$transf!="None"){
        if(isTRUE(input$scale)){
          c("Scale","Transf")
        } else{
          "Transf"
        }
      } else {
        if(isTRUE(input$scale)){
          c("Scale")
        } else{
          character(0)
        }
      }
      cur_transfs(res)
    })
    observeEvent(list(input$scale,input$center),ignoreInit = T,{

      res<-if(isTRUE(input$scale)){
        if(input$transf!="None"){
          c("Transf","Scale")
        } else{
          "Scale"
        }

      } else{
        if(input$transf!="None"){
          c("Transf")
        } else{
          character(0)
        }
      }
      cur_transfs(res)


    })


    run_transf<-eventReactive(list(input$transf,input$scale,input$center,input$transf_ord),{
      res<-list()
      args_list<-list(
        Transf=list(transf=input$transf,fun_name='transf_data'),
        Scale=list(scale=input$scale,center=input$center,fun_name='get_scale')

      )
      if(length(cur_transfs())==1){

        return(args_list)
      } else{
        req(input$transf_ord)
        args_list<-args_list[input$transf_ord]

        return(args_list)
      }



    })

    r_transf_end<-reactive({
      data<-data()
      for(i in seq_along(run_transf())){
        args<-run_transf()[[i]]

        fun_name<-args$fun_name


        args$fun_name<-NULL
        args$data<-data
        data<-do.call(fun_name,args)
      }

      data

    })
    output$transf_order<-renderUI({
      div(
        div(
          div(style="display: flex;    padding-top: 0px;",
              div("First",style="width: 100px;"),
              div("Second",style="width: 100px"),
          ),
          class="tr_or",
          rank_list(
            labels=  cur_transfs(),
            orientation ="horizontal",
            input_id =session$ns("transf_ord")
          )
        ))
    })
    observeEvent(input$transf,{

      if(input$transf=="None"){

        shinyjs::hide("transf_summary")
      } else{
        shinyjs::show("transf_summary")
      }
    })
    observeEvent(input$transf_help,{
      tips_transf_met<-lapply(transf_df,function(x) {
        p(strong(x$label,":"),x$tooltip,style="text-indent: 20px")
      })
      showModal(
        modalDialog(
          title="Transformations",
          easyClose = T,
          div(style="height: 350px; overflow-y:scroll",
              tips_transf_met
          )
        )
      )
    })
    observeEvent(input$scale,{
      if(isTRUE(input$scale)){
        shinyjs::show('center')
        shinyjs::show("transf_summary")
      } else{
        shinyjs::hide('center')
        shinyjs::hide("transf_summary")
      }
    })
    observeEvent(list(input$scale,input$center),{
      req(input$scale)})
    bag_name<-reactive({
      paste0(
        if(length(input$transf)>0){
          if(input$transf!="None") {paste0("_",input$transf)}
        },
        if(length(input$scale)>0) {
          if(isTRUE(input$scale)) {paste0("_scaled")}
        })



    })
    result_v5<-reactive({
      if(isFALSE(input$scale)&input$transf=="None"){
        return(data())
      }

      if(!is.null(r_transf_end())){
        newdata<-r_transf_end()
        data<-data_migrate(data(),newdata)
        attr(data,"bag")<-bag_name()
      }

      data
    })
    observeEvent(result_v5(),{
      vals$vtools$tool5<-result_v5()
    })

    return({NULL})
  })
}
tool6<-list()
tool6$ui<-function(id){
  ns<-NS(id)
  div(style="height: 300px; ;display: flex;",class="half-drop-inline",

      div(style="width: 50%;padding: 15px;",class="half-drop",
          selectInput(ns("na_targ"),"Target:", c("Numeric-Attribute","Factor-Attribute")),
          selectInput(ns("na_method"), div("Method:",tipify(actionLink(ns("na_help"),icon("fas fa-question-circle"), type="toggle"),"Click for details","right")),choices=c("knn","bagImpute","medianImpute","pmm","rf","cart")),
          uiOutput(ns('bag_warning')),
          hidden(numericInput(ns("na_knn"), span("K:",tiphelp("the number of nearest neighbors from the training set to use for imputation")),value=5)),
          div(align="right",id=ns("run_na_btn"),
              class="run_na_btn save_changes",
              div(class="tools",
                  actionButton(ns("run_na"),"RUN >>")
              )
          )
      ),
      div(div(style="padding: 15px;margin-right: 20px;  max-width: 275px; background: white",
              class="half-drop-inline",
              uiOutput(ns('na_warning')),
              uiOutput(ns('print_imputation_before')),
              uiOutput(ns('print_imputation_after'))))
  )
}
tool6$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    bag_name<-reactive({paste0("(imp_",input$na_method,")")})


    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })
    observeEvent(input$run_na,ignoreInit = T,{
      try({

        # print("run_na")

        shinyjs::removeClass("run_na_btn","save_changes")
        #req(isTRUE(vals$r_impute))

        data=vals$pp_data
        na_method=input$na_method
        k=input$na_knn
        attr=input$na_targ



        withProgress(
          min=NA,
          max=NA,
          message="Imputing...",{
            newdata0<-newdata<-nadata(data(),na_method,k,attr=attr)
            attr(newdata,"bag")<-bag_name()
            newdata<-data_migrate(data(),newdata)
            if(input$na_targ=="Factor-Attribute"){
              data_o<-data()
              attr(data_o,"factors")<-newdata0
              newdata<-data_o
            }
            #result_v6
            vals$r_imputed<-newdata
          })
        shinyjs::removeClass("run_na_btn","save_changes")
        done_modal()
        vals$done_impute<-T
        vals$r_impute<-NULL


      })
    })


    anyna<-reactiveVal(T)
    validate_na<-reactive({
      anyna(F)
      req(input$na_targ)
      input$na_method
      if(input$na_targ=="Numeric-Attribute"){

        shinyjs::toggle("run_na",condition =anyNA(data()) )
        if(anyNA(data())){
          anyna(T)
        }
        validate(need(anyNA(data()),"No missing values in the Numeric-Attribute"))

      }
      if(input$na_targ=="Factor-Attribute"){
        shinyjs::toggle("run_na",condition =anyNA(attr(data(),'factors')) )
        if(anyNA(attr(data(),'factors'))){
          anyna(T)
        }

        validate(need(anyNA(attr(data(),'factors')),"No missing values in the Factor-Attribute"))
      }


    })

    output$na_warning<-renderUI({
      validate_na()
    })





    observeEvent(input$na_targ,ignoreInit = T,{
      req(isTRUE(vals$r_impute))
      choices<-c("knn","bagImpute","medianImpute","pmm","rf","cart")
      if(input$na_targ=="Factor-Attribute"){
        updateSelectInput(session,"na_method",choices=choices[-c(1:3)])
      } else{
        updateSelectInput(session,"na_method",choices=choices)
      }
    })

    new_number_of_nas<-reactive({
      sum(is.na(vals$r_imputed))
    })

    original_number_of_nas<-reactive({
      sum(is.na(data()))
    })

    output$data_was_inputed<-renderUI({
      if(original_number_of_nas()==new_number_of_nas() ){
        return(NULL)
      } else{
        div(
          div( emgreen(span(original_number_of_nas(),'values were imputed'))),
          div("Save to make permanent")
        )
      }

    })

    output$print_imputation_before<-renderUI({
      "start"
      req(isTRUE(anyna()))
      req(vals$r_imputed)
      r_imp<-vals$r_imputed
      data<-attr(r_imp,"data0")
      df<-r_imp
      xy<-attr(r_imp,"xy")
      df<-data.frame(df)
      df<-df[xy$x,xy$y]
      y<-which(colnames(df)%in%xy$y)+1
      x<-which(rownames(df)%in%xy$x)

      res<-div(
        uiOutput(session$ns("data_was_inputed")),
        quant$ui(session$ns("imputation"),"Show imputed values"),
        div(style="width: 250px; overflow-x: auto",
            quant$server('imputation',
                         data2=NULL,
                         colored=T,
                         coords=cbind(x,y),
                         data1=df,
                         height= '200px',
                         fun="df",
                         width='250px',

                         d1_before="Imputed data"
            )
        )
      )

      res
    })

    imputation_help<-reactive({p_met<-function(method,fun,pakage){
      p(strong(method,":"), code(fun,paste0("{",pakage,"}")))}
    df_p<-data.frame(method=c("knn","bagImpute","medianImpute","pmm","rf","cart"),
                     fun=c(rep("preProcess",3),rep("mice",3)),
                     pakage=c(rep("caret",3),rep("mice",3)))
    titles<-lapply(1:nrow(df_p),function(i){p_met(df_p$method[i],df_p$fun[i],df_p$pakage[i])})
    p_descs<-{c(
      "k-nearest neighbor imputation is only available for Numeric-Attribute. It is carried out by finding the k closest samples (Euclidean distance) in the dataset. This method automatically centers and scales your data.",
      "Only available for Numeric-Attribute. Imputation via bagging fits a bagged tree model for each predictor (as a function of all the others). This method is simple, accurate, and accepts missing values, but it has a much higher computational cost.",
      "Only available for Numeric-Attribute. Imputation via medians takes the median of each predictor in the training set and uses them to fill missing values. This method is simple, fast, and accepts missing values, but treats each predictor independently and may be inaccurate.",
      "Predictive mean matching (PMM) is available for both Numeric and Factor Attributes. It involves selecting observations with the closest predicted values as imputation candidates. This method maintains the distribution and variability of the data, making it suitable for data that is normally distributed.",
      "Random forest imputation is available for both Numeric and Categorical-Attributes. It use an ensemble of decision trees to predict missing values, This non-parametric method can handle complex interactions and nonlinear relationships but may be computationally intensive.",
      "Classification and regression trees (CART) imputation is available for both Numeric and Categorical-Attributes. It is a method that applies decision trees for imputation. It works by splitting the data into subsets which then result in a prediction model."


    )}
    lapply(seq_along(titles),function(i){
      p(
        titles[[i]],
        p_descs[[i]]
      )
    })
    })
    observeEvent(input$na_help,ignoreInit = T,{

      showModal(
        modalDialog(
          easyClose = T,
          div(imputation_help(), style="overflow-y: scroll; height: 300px"),
          title="Data imputation"
        )
      )
    })
    observeEvent(input$na_method,ignoreInit = T,{
      if(input$na_method=="knn"){
        shinyjs::show("na_knn")
      } else{
        shinyjs::hide("na_knn")
      }
    })

    output$bag_warning<-renderUI({
      req(input$na_method=='bagImpute')
      div(style="white-space: normal;",
          strong("warning",style="color: red"),em("This method has higher computational cost and can be very time-consuming.")
      )
    })



    observe({
      if(isTRUE(vals$done_impute)){
        res<-vals$r_imputed

        vals$vtools$tool6<-res
        vals$r_impute<-T
      }
    })




    find_na<-function(x){
      x==""|x=="NA"|is.na(x)
    }

    return(NULL)



  })
}

generate_partiton<-function(data,split_t,split_y,split_p,split_seed,part_type="Balanced"){
  if(part_type=="Random"){
    nobs<-nrow(data)
    p<-split_p/100
    ntest<-round(nobs*p)
    test_vec<-rep("Test",ntest)
    train_vec<-rep("Training",nobs-ntest)
    if(is.na(split_seed)){split_seed=NULL}
    set.seed(split_seed)
    part_vec<-sample(c(train_vec,test_vec))
    df<-data.frame(Partition=part_vec)
    df$Partition<-as.factor(df$Partition)
    rownames(df)<-rownames(data)
    return(df)
  }

  factors<-attr(data,"factors")
  if(split_t=="Classification"){
    data<-factors
  }
  y<-data[,split_y]
  if(is.na(split_seed)){split_seed=NULL}
  set.seed(split_seed)
  p=((100-split_p)/100)
  part<-caret::createDataPartition(y,p=p,list=T)[[1]]
  train=rownames(data)[part]
  test=rownames(data)[-part]
  df<-data.frame(Partition=rep(NA,nrow(factors)),
                 row.names = rownames(factors))
  df[train,1]<-"training"
  df[test,1]<-"test"
  df$Partition<-as.factor(df$Partition)

  df

}
tool7$ui<-function(id){
  ns<-NS(id)
  div(style="height: 300px; ;display: flex;",
      class="half-drop-inline",
      div(
        style="background-color: #f5f5f5;
          width: 50%;padding: 5px;",class="half-drop",
        div(
          class="radio_search radio-btn-green",
          radioGroupButtons(ns("part_type"),span(tiphelp("<li> Balanced partition: Ensures balanced distributions within the splits for classification or regression intended models.</li><li> Random partition: random sampling is done.</li>","left"),"Type:"),c("Balanced", "Random"))
        ),
        div(class="impute",

            uiOutput(ns("part_for")),
            div(class="short",
                style="margin-left: 20px",
                uiOutput(ns("split_y_out"))
            ),
            div(class="medium",
                style="margin-left: 40px",
                numericInput(ns("split_p"),span(tiphelp("the percentage of data that goes to test","left"),"Size (%):"),value=20,min=0,max=99,step=1),
                numericInput(ns("split_seed"),span(tiphelp("seed for genereating reproducible results",'left'),"Seed:"),value=NA)
            ),

            div(id=ns('create_partition_btn'),class="save_changes",align="right",

                actionButton(ns("create_partition"),"Create partition >>"),
                uiOutput(ns("print_partition_summary"))
            )


        )
      ),
      div(style="background: white; padding: 15px;margin-right: 20px;",
          uiOutput(ns("validate_partition")),
          uiOutput(ns("print_partition"))

      )
  )
}
tool7$server<-function(id,vals=NULL){

  moduleServer(id,function(input,output,session){


    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })



    output$part_for<-renderUI({
      req(input$part_type=="Balanced")
      ns<-session$ns
      req(data())
      choices<-c("Classification","Regression")
      # selected<-get_selected_from_choices(vals$cur_partition,choices)
      selectInput(
        ns("split_t"),
        span(pophelp(
          NULL,
          HTML(paste0(
            p(HTML(paste0(strong("Choose the model type for which you are creating a partition")))),
            p(HTML(paste0(tags$li('For classification models, the random sampling is done within the levels of y in an attempt to balance the class distributions within the splits')))),
            p(HTML(paste0(tags$li(
              "For regression regression models, samples are divided into sections based on percentiles of the numeric target variable (Y), with sampling performed within these subgroups. "
            )))),

            p(HTML(paste0(em("Both uses the createDataPartition function from the caret package."))))
          )),"left"
        ),"Partition for:"),choices, selected=vals$cur_partition
      )
    })









    output$split_y_out<-renderUI({
      req(input$part_type=="Balanced")
      req(data())
      req(input$split_t)
      if(input$split_t%in%"Classification"){
        choices_factors<-colnames(attr(data(),"factors"))
      } else{
        choices_factors<-colnames(data())
      }

      # selected<-get_selected_from_choices(vals$cur_split_y,choices_factors)
      #selectInput(session$ns("split_y"),SelectedText="Columns selected", label=span(tiphelp("Select the target variable",'left'),'Y:'),choices=choices_factors,selected=vals$cur_split_y)

      pickerInput(session$ns("split_y"),tags$label(span(tiphelp("Select the target variable. Choose multiple variables for creating multiple partition columns",'left'),'Y:'),style=""),choices_factors,selected=choices_factors[1],multiple =T)
    })

    observeEvent(input$split_y,{
      shinyjs::removeClass('split_y',"half-drop")
    })


    observeEvent(input$split_t,{
      vals$cur_partition<-input$split_t
    })
    observeEvent(input$split_y,{
      req(input$split_t)
      vals$cur_split_y<-input$split_y
    })







    args_part<-reactive({

      req(input$split_t)
      if(input$split_t=="Classification"){
        data0<-attr(data(),"factors")
      } else{
        data0<-data()
      }
      req(input$split_y%in%colnames(data0))

      list(
        data=data(),
        split_t=input$split_t,
        split_y=input$split_y,
        split_p=input$split_p,
        split_seed=input$split_seed,
        part_type=input$part_type
      )
    })

    r_partition<-reactiveVal(NULL)

    output$validate_partition<-renderUI({
      res<-NULL
      args<-args_part()
      if(args$split_t=="Regression"){
        req(args$split_y%in%colnames(args$data))
        y<-args$data[, args$split_y]
        if(anyNA(y)) {
          res<-div(style="padding: 5px",
                   class = "alert_warning",
                   icon("triangle-exclamation",style="color: Dark yellow3"),
                   ' NAs are not allowed when creating a partition for regression models.'
          )
        }
      }
      res

    })


    observeEvent(args_part(),{
      args<-args_part()
      condition<-if(args$split_t=="Classification"){
        TRUE
      } else{
        req(args$split_y%in%colnames(args$data))
        y<-args$data[, args$split_y]
        if(anyNA(y)){
          FALSE
        }else{
          TRUE
        }
      }
      shinyjs::toggle('create_partition',condition = condition)


    })

    observeEvent(args_part(),{
      r_partition(NULL)
      shinyjs::addClass("create_partition_btn","save_changes")
    })
    observeEvent(input$create_partition,ignoreInit = T,{
      args0<-args<-args_part()
      result<-lapply(args$split_y, function(x){
        args0$split_y<-x
        do.call(generate_partiton,args0)
      })

      part<-data.frame(result)

      fac0<-attr(data(),"factors")
      if(input$part_type=="Balanced"){
        partnames<-paste0("Partition_",args$split_y)
        newfacnames<-make.unique(c(colnames(fac0),partnames))
        newfacnames<-newfacnames[(ncol(fac0)+1):length(newfacnames)]
        colnames(part)<-newfacnames
      }



      # part<-do.call(generate_partiton,args)
      r_partition(part)


      vals$vtools$tool7_partition<-part
      shinyjs::removeClass("create_partition_btn","save_changes")
    })
    output$print_partition<-renderUI({
      req(r_partition())
      div(
        head_data$ui(session$ns("table-partition"),c(1,2)),
        head_data$server('table-partition',r_partition(),"300px","250px")
      )
    })


    observeEvent(data(),{
      r_partition(NULL)
    })
    output$print_partition_summary<-renderUI({
      req(r_partition())

      part<-r_partition()
      #req(ncol(r_partition())==1)

      summ0<-lapply(part, table)
      renderTable(do.call(rbind,summ0),rownames =T)



    })
    result_v7<-reactive({
      if(is.null(r_partition())){
        return(data())
      }
      data<-data()
      factors<-attr(data,"factors")
      factors<-cbind(factors,r_partition())
      # factors["New partition"]<-r_partition()
      attr(data,"factors")<-factors
      if(input$part_type=="Balanced"){
        attr(data,"bag")<-paste0("Partition_",input$split_y)
      } else{
        attr(data,"bag")<-paste0("Partition")
      }

      attr(data,"action")<-"factor-column"
      data
    })

    observeEvent(result_v7(),{
      vals$vtools$tool7<-result_v7()
    })



    return(NULL)


  })
}
tool8$ui<-function(id){
  ns<-NS(id)
  div(class="class_tool8",
      div(style="display: flex;",
          class="half-drop-inline",
          div(
            style="background-color: #f5f5f5;
          width: 50%;padding: 5px;",class="half-drop",
          div(strong("Aggregate:"),
              popify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),
                     "Aggregate","The process involves two stages. First, collate individual cases of the Numeric-Attribute together with a grouping variable (unselected factors). Second, perform which calculation you want on each group of cases (selected factors)", options=list(container="body"))),

          div(class="virtual-180",
              virtualPicker(ns('fac_descs'), "factor(s) selected")
          ),
          selectInput(ns("spread_measures"),"Function:",choices=c("sum","mean","median","var","sd","min","max"),  selected="mean"),

          div(id=ns("run_agg_btn"),align="right",class="save_changes",
              em("Click to aggregate",icon("fas fa-hand-point-right")),
              actionButton(ns("run_agg"), span("RUN",img(src=agg_icon2,height='14',width='14')))
          )          ),
          div(style="background: white; padding: 15px; width: 275px",
              uiOutput(ns("summ_agg")),

          )
      ),
      div(style="background: white; padding: 15px; ", class="half-drop-inline",
          uiOutput(ns("war_agg")),
          uiOutput(ns("print_agg"))
      )
  )
}
tool8$server<-function(id, vals=NULL){

  moduleServer(id,function(input,output,session){



    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })

    factors<-reactive({
      attr(data(),"factors")
    })
    coords<-reactive({
      attr(data(),"coords")[rownames(data()),,drop=F]
    })

    observeEvent(factors(),{
      choices_factor<-colnames(factors())

      shinyWidgets::updateVirtualSelect('fac_descs', choices=choices_factor, selected=choices_factor[1:(length(choices_factor)-1)])
    })


    active_fac_descs<-reactiveVal(F)

    active_fac_descs(T)





    agg_args<-reactive({
      req(input$fac_descs)
      req(input$spread_measures)
      list(
        fac_descs=input$fac_descs,
        measure=input$spread_measures
      )
    })

    observeEvent(agg_args(),{
      shinyjs::addClass("run_agg_btn","save_changes")
    })

    observeEvent(input$run_agg,{
      shinyjs::removeClass("run_agg_btn","save_changes")
    })

    r_agg<-reactiveVal()
    observeEvent(input$run_agg,ignoreInit = T,{
      coords<- coords()
      try({
        df<- data.frame(aggregate(data(),data.frame(factors()[,input$fac_descs, drop=F]),get(input$spread_measures), na.rm=T))
        if(!is.null(coords)){
          coords<- data.frame(aggregate(coords,factors()[rownames(coords),input$fac_descs, drop=F],mean , na.rm=T))
          coords<-coords[,which(unlist(lapply(coords,is.numeric))), drop=F]

        }
        dfnum<-df[,which(unlist(lapply(df,is.numeric))), drop=F]
        dffac<-df[,which(!unlist(lapply(df,is.numeric))), drop=F]
        df<-dfnum
        labels<-make.unique(apply(dffac[,input$fac_descs, drop=F], 1 , paste,collapse = "_"))
        rownames(df)<-labels
        df<-data_migrate(data(),df,"Aggregated_results")
        rownames(dffac)<-labels
        if(!is.null(coords)){
          rownames(coords)<-labels
        }
        attr(df,"factors")<-dffac
        attr(df,"coords")<-coords
        r_agg(df)

      })
    })
    output$war_agg<-renderUI({
      req(r_agg())
      req(!is.null(coords()))
      em(icon("triangle-exclamation"), "The Datalist contained a Coords-Attribute, which was aggregated with the selected factors to prevent loss of coordinates.", style="")

    })

    output$summ_agg<-renderUI({
      req(r_agg())
      nrow=nrow(r_agg())
      ncol=ncol(r_agg())
      div(
        div(strong("Original data:")),
        div(em("rows:"), nrow(data())),
        div(em("columns:"), ncol(data())),
        div(
          div(strong("Aggregated data:")),
          div(em("rows:"), nrow),
          div(em("columns:"), ncol)
        ),
        emgreen("Save to make permanent")
      )

    })
    result<-reactive({
      list(
        r_agg=r_agg()
      )
    })

    result_v8<-reactive({
      data<-data()
      if(!is.null(r_agg())){
        newdata<-r_agg()
        attr(newdata,"bag")<-paste0("_",input$spread_measures)
        data<-newdata
      }

      data
    })

    observeEvent(result_v8(),{
      vals$vtools$tool8<-result_v8()
    })

    return(NULL)



  })
}
tool9$ui<-function(id){
  ns<-NS(id)
  div(class="half-drop p20",
      {div(style="display: flex; padding:5px;width: 300px;",class="cogs_in",
           div(style="width: 150px",
               p(icon("fas fa-long-arrow-alt-right"),
                 strong("1. Pick a color:",tiphelp("Click to change color","left"))),
               div(colourpicker::colourInput(ns("col"), NULL, "#3498DB",closeOnClick=F))
           ),
           div(p(icon("fas fa-long-arrow-alt-right"),
                 strong("2. Add:",tiphelp("Click to include the color","left"))),
               div(align="center",actionButton(ns("add_color"),icon("level-down",class="fa-rotate-0")))
           )
      )},
      {div(style="padding-left: 100px; ",
           span(icon("level-down",class="fa-rotate-180", style="width: 50px"),style="vertical-align: text-bottom;"),
           span(icon("level-down",class="fa-rotate-90" ),style="vertical-align: text-top;margin-left: 20px")
      )},
      {
        div(class="cogs_in",
            style="padding: 5px; height: 75px;width: 300px; align: center",
            div(style="position: absolute; left: 240px",
                actionLink(ns("restart_palette"),label="[restart]")),
            div(style="display:flex",
                div(
                  div(class="tool_color",style="",
                      strong("3. Create"),tiphelp("Click to create a palette with the colors above. The new palette will be available in all color inputs")),
                  actionButton(ns("add_palette"),span(icon("palette"),icon("arrow-down")))),
                div(style="background: #D1F2EB ; padding: 5px;margin-right: 5px",
                    div(class="tool_color","New palette"),
                    div(div(style="width: 140px",
                            uiOutput(ns('newpalette_img')))))
            )
        )
      },
      {div(style="background:#FBEEE6;padding: 5px; height: 75px;width: 300px;  ",
           p(strong("Created palettes:")),
           div(style="display: flex",
               pickerInput(inputId = ns('available_palette'),
                           label = NULL,
                           choices = NULL),
               actionButton(ns("delete_palette"),icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"))
           )
      )}

  )

}
tool9$server<-function (id,vals){
  moduleServer(id,function(input, output, session){

    ns<-session$ns

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
      image(t(matrix(seq(0,1,length.out=100), nrow=1)), axes=F,col=  colorRampPalette(vals$newpalette_colors,alpha=T)(100))
      dev.off()
      palette1<-base64enc::dataURI(file =outfile,mime = "image/png")
      vals$colors_img<-rbind(vals$colors_img,data.frame(val=palette_name,img= sprintf(paste0(img(src = palette1, height = '20',width = '70',style="margin-top:-40px;margin-bottom:-40px;margin-left:-20px;margin-right:-20px;") ))))
      vals$newcolhabs[[palette_name]]<-colorRampPalette(vals$newpalette_colors,alpha=T)
      vals$created_pal<-T
      grad_base<-vals$newcolhabs[-c(1:11)]
      newgrads<-gradient_colors(grad_base)
      pic<-which(names(vals$newcolhabs)%in%newgrads)
      req(length(newgrads)>0)

      updatePickerInput(
        session,'available_palette',
        choices=newgrads,selected = palette_name,
        choicesOpt = list(content = vals$colors_img$img[pic])

      )
      vals$newpalette_colors<-NULL
    })


    gradient_colors<-function(newcolhabs){
      res<-lapply(newcolhabs, function(x) x(2))
      res1<-unlist(lapply(res, function(x) x[1]==x[2]))
      grad<-names(res1[res1==F])
      grad
    }
    output$newpalette_img<-renderUI({
      req(length(vals$newpalette_colors)>0)
      renderPlot({
        par(mar=c(0,0,0,0),mai=c(0,0,0,0),mgp=c(0,0,0),xpd=T,ann=F, bg=NA, fg=NA)
        image(t(matrix(seq(0,1,length.out=100), nrow=1)), axes=F,col=  colorRampPalette(  vals$newpalette_colors,alpha=T)(100))
      }, height =30)})


  })
}
tool10$ui<-function(id){
  ns<-NS(id)
  div(class="half-drop p20",
      div(
        div(class="cogs_in",
            div(style="padding: 5px;height: 100px",
                div(strong(icon("fas fa-thumbtack"),'Create a savepoint:',style="color: #05668D")),
                div(style="display: flex",
                    downloadButton(ns("bookmarkBtn"), style = "font-size: 12px", label="Download"),
                    div(class="p20",emgray("Download the Savepoint as rds file. You can latter upload the created savepoint (panel bellow).")))
            )),
        div(style="background: #f7f7f7ff;
-webkit-box-shadow: inset 0px 0px 5px #797979ff;margin-top: 2px",
div(
  div(style="padding: 5px;height: 100px",
      div(
        tipify(strong(icon("fas fa-external-link-alt"),"Load a savepoint:",style="color: #05668D"),"Restore the dashboard with a previously saved savepoint (.rds file)","right")),
      div(id="tool_savepoint", fileInput(ns("load_savepoint"), label=NULL,accept =".rds",multiple =T))))),
uiOutput(ns("validade_savepoint")))


  )

}
tool10$server<-function (id,vals){
  moduleServer(id,function(input, output, session){

    ns<-session$ns

    observeEvent(input$load_savepoint,{

      output$validade_savepoint<-renderUI({
        validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Error: the uploaded file is not an rds file"))
      })

    })

    output$bookmarkBtn<-{
      downloadHandler(
        filename = function() {
          paste("Savepoint_",Sys.Date(), ".rds", sep = "")

        },

        content = function(file) {
          withProgress(
            message = "Preparing the download ...",
            min = 1,
            max = 1,
            {
              #tosave<-isolate(reactiveValuesToList(vals))
              #tosave<-c(tosave["saved_data"],tosave["saved_maps"],tosave["newcolhabs"],tosave['colors_img'],tosave[grep("cur",names(tosave))])
              tosave<-isolate(reactiveValuesToList(vals))
              tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
              tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>1000)]

              tosave$saved_ensemble<-vals$saved_ensemble
              tosave$saved_maps<-vals$saved_maps
              tosave$saved_data<-vals$saved_data
              tosave$newcolhabs<-vals$newcolhabs
              tosave$colors_img<-vals$colors_img
              saveRDS(tosave, file)
              beep(10)

            }
          )

        }
      )
    }

    observeEvent(input$load_savepoint,ignoreInit = T,{
      validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Requires rds file"))
      delay(600,{
        showModal(
          modalDialog(
            uiOutput(ns("savepoint_request")),
            title="",
            footer = div(actionButton(session$ns("load_savepoint_yes"),"Proceed"),modalButton("close"))

          )
        )
      })
    })

    observeEvent(input$load_savepoint_yes,ignoreInit = T,{
      vals$saved_data<-NULL
      vals$cur_tab<-"menu_intro"
      updateTextInput(session, "tabs", value = 'menu_intro')
      validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Requires rds file"))
      if(length(input$load_savepoint$datapath)>1){

        res<-lapply(as.list(input$load_savepoint$datapath), function(x) readRDS(x))
        saved_data<-do.call(c,lapply(res,function(x)    x[["saved_data"]]))
        saved_ensemble<-do.call(c,lapply(res,function(x)    x[["saved_ensemble"]]))
        mybooks<-res[[1]]
        if(length(saved_ensemble)>0){
          names(saved_ensemble) <-make.unique(names(saved_ensemble))
        }

        names(saved_data) <-make.unique(names(saved_data))
        mybooks$saved_ensemble<-saved_ensemble
        mybooks$saved_data<-saved_data
      } else{
        mybooks<-readRDS(input$load_savepoint$datapath)
      }
      vnew<-list()
      vnew$saved_data<-mybooks$saved_data
      for(i in names(vnew$saved_data)) {
        if(length(attr(vnew$saved_data[[i]],"rf"))>0){
          for(j in 1:length(attr(vnew$saved_data[[i]],"rf")) ){
            x<-attr(vnew$saved_data[[i]],"rf")[[j]]
            if(!inherits(x,"list")){
              attr(vnew$saved_data[[i]],"rf")[[j]]<- list(x)
            }
          }
        }
      }
      mybooks$saved_data<-vnew$saved_data
      newvals<-mybooks
      if(is.null(newvals$cur_data)){newvals$cur_data<-names(vals$saved_data)[[1]]}


      newvals <-mybooks[!names(mybooks)%in%c('newcolhabs','colors_img')]
      colors<-mybooks[names(mybooks)%in%c('newcolhabs','colors_img')]
      vals$colors_img[vals$colors_img$val%in%colors$colors_img$val,]<-colors$colors_img
      vals$newcolhabs[names(colors$newcolhabs)]<-colors$newcolhabs


      for (var in names(newvals) ) {
        vals[[var]]<-newvals[[var]]
      }
      delay(500,{
        updateTextInput(session, "tabs", value = newvals$cur_tab)
        runjs("Shiny.setInputValue('last_btn', 'fade');")
      })
      beep(10)
      removeModal()
      vals$update_curtab<-"menu_upload"
      vals$update_pp<-"tool_close"
    })
    output$savepoint_request<-renderUI({
      validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Requires rds file"))
      column(12,
             h5('Are you sure? '),
             column(12,p(strong("Warning:", style="color: #0093fcff"),"The application will restart with the loaded savepoint and unsaved changes will be lost")))

    })
  })
}
head_data<-list()
head_data$ui<-function(id,options=1:4){
  ns<-NS(id)
  div(
    radioGroupButtons(ns("showas"),NULL , c("head","full","str")[options],status="small_GroupBtn")
  )
}
head_data$server<-function(id, data,height="200px", width="300px",dt=F,page_length=10,type="default",dom="tp"){
  moduleServer(id,function(input,output,session){
    style0<-paste0("overflow: auto;max-height: ",height,";max-width:",width)
    get_data<-reactive({
      if(input$showas=="head"){
        data<-head(data)}
      data
    })

    output$data_table<-renderUI({
      data<-get_data()
      d<-if(input$showas=="str"){
        div(
          div(class="small_print",
              renderPrint(str(data))
          )
        )

      } else{
        if(isTRUE(dt)){
          style0<-""
          fixed_dt(data,height, pageLength=page_length,dom=dom)
        } else{
          div(
            class="half-drop-inline small_table",
            renderTable(data,rownames =T)
          )
        }

      }

      inline(div(d,style=style0))
    })

  })
}
pp_data$ui<-function(id){
  ns<-NS(id)
  div(class="pp_datacontrol",style="display: none;",
      div(
        div(class="half-drop",
            div(style="display: flex",
                uiOutput(ns('data_upload'))
            )
        ),
        bsTooltip(ns("change_picker"),"Target Datalist"),



      )
  )
}
pp_data$server<-function(id,vals){
  moduleServer(id,function(input, output,session){

    output$data_upload<-renderUI({
      selectInput(session$ns("data_upload"),NULL,choices=names(vals$saved_data), selected=vals$cur_data, width="250px")
    })

    observeEvent(input$data_upload,{
      vals$cur_data<-input$data_upload
    })

    observeEvent(input$data_upload,{
      shinyjs::addClass(selector=".run_na_btn",class="save_changes")
    })



    data<-reactive({
      data<-try({

        vals$r_impute<-T
        req(input$data_upload)%in%names(vals$saved_data)
        data<-vals$saved_data[[input$data_upload]]
        req(data)
        attr(data,"datalist_root")<-input$data_upload
        data

      },silent = T)
      if(!inherits(data,"try-error")){
        return(data)
      } else{
        return(NULL)
      }

    })



    observeEvent(list(vals$pp_data,vals$tosave),{
      req(!is.null(vals$tosave))
      d0<-vals$pp_data
      d1<-vals$tosave

      req(attr(d1,"datalist_root")==attr(d0,"datalist_root"))

      bagdata<-!identical(d1,d0)
      shinyjs::toggleClass(selector=".tool_page",class="border_alert",condition=bagdata)
      shinyjs::toggleClass(selector=".save_pp",class="save_changes",condition=bagdata)
    })
    observeEvent(data(),{
      vals$pp_data<-data()
    })

    return(NULL)


  })

}

tool2<-list()
tool2$ui<-function(id){
  ns=NS(id)
  div(style="margin-top: -35px",
      div(class="toolkit_items",style="width: 550px; height: 320px;      background: #00000095;; position: fixed;right: 0px; z-index: 9",),
      div(
        class="toolkit_items",id=ns("toolkit"),

        lapply(seq_along(tool2_tabs),function(i){
          style=""
          if(i%in%c(10,11)){
            style="color: brown"
          }
          div(actionButton(ns(paste0('tool_kit_',i)),
                           tool2_tabs[i],style=style,class="toolkit"))
        }),

      ),
      div(class="tool_page tool2-tabs",
          div(
            class="nav-tools",
            tabsetPanel(type ="hidden",selected="none",
                        id=ns("tabs_tool2"),
                        tabPanel(tool2_tabs[1],value="tab1",
                                 tool2_tab1$ui(ns("rename")),
                                 uiOutput(ns('tool2_tab1_out'))



                        ),
                        tabPanel(tool2_tabs[2],value="tab2",
                                 tool2_tab2$ui(ns("merge")),
                                 uiOutput(ns('tool2_tab2_out'))

                        ),
                        tabPanel(tool2_tabs[3],value="tab3",
                                 tool2_tab3$ui(ns("exchange")),
                                 uiOutput(ns('update_tab3_exchange')),
                                 uiOutput(ns('tool2_tab3_out'))

                        ),
                        tabPanel(tool2_tabs[4],value="tab4",
                                 div(tool2_tab4$ui(ns("replace")),
                                     uiOutput(ns('update_tab4'))),
                                 uiOutput(ns('tool2_tab4_out'))
                        ),
                        tabPanel(tool2_tabs[5],value="tab5",
                                 div(tool2_tab5$ui(ns("editcol"))),
                                 uiOutput(ns('tool2_tab5_out'))

                        ),
                        tabPanel(tool2_tabs[6],value="tab6",
                                 div(tool2_tab6$ui(ns("editmod")),
                                     uiOutput(ns('update_tab6'))),
                                 uiOutput(ns('tool2_tab6_out'))
                        ),
                        tabPanel(tool2_tabs[7],value="tab7",
                                 tool2_tab7$ui(ns("transpose")),
                                 uiOutput(ns('tool2_tab7_out'))
                        ),
                        tabPanel(tool2_tabs[8],value="tab8",
                                 tool2_tab8$ui(ns("shp")),
                                 uiOutput(ns('tool2_tab8_out'))),
                        tabPanel(tool2_tabs[9],value="tab9",
                                 tool2_tab9$ui(ns("code")),
                                 uiOutput(ns('tool2_tab9_out'))),
                        tabPanel(tool2_tabs[10],value="tab10",
                                 div(tool2_tab10$ui(ns("gen")),
                                     uiOutput(ns('update_tab10'))),
                                 uiOutput(ns('tool2_tab10_out'))

                        ),
                        tabPanel(tool2_tabs[11],value="tab11",
                                 tool2_tab11$ui(ns("deldatalist")),
                                 uiOutput(ns('tool2_tab11_out'))
                        )
            )
          )
      )

  )
}
tool2$server<-function(id,vals){

  moduleServer(id,function(input,output,session){


    shinyjs::onevent("mouseleave", "toolkit", {
      hide(selector=".toolkit_items")
      show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_1,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab1")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_2,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab2")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_3,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab3")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_4,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab4")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_5,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab5")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_6,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab6")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_7,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab7")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_8,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab8")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_9,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab9")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })
    observeEvent(input$tool_kit_10,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab10")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })

    observeEvent(input$tool_kit_11,{
      updateTabsetPanel(session,"tabs_tool2",selected="tab11")
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })

    tool2_tab3$update_server("exchange",vals)





    output$tool2_tab1_out<-renderUI({
      tool2_tab1$server('rename',vals)
      NULL
    })

    output$tool2_tab2_out<-renderUI({
      tool2_tab2$server('merge',vals)
      NULL
    })
    output$tool2_tab3_out<-renderUI({
      tool2_tab3$server('exchange',vals)
      NULL
    })

    output$tool2_tab4_out<-renderUI({
      tool2_tab4$server('replace',vals)
      NULL
    })

    output$tool2_tab5_out<-renderUI({
      tool2_tab5$server('editcol',vals)
      NULL
    })


    output$tool2_tab6_out<-renderUI({
      tool2_tab6$server('editmod',vals)
      NULL
    })

    output$tool2_tab7_out<-renderUI({
      tool2_tab7$server('transpose',vals)
      NULL
    })
    output$tool2_tab8_out<-renderUI({
      tool2_tab8$server('shp',vals)
      NULL
    })
    output$tool2_tab9_out<-renderUI({
      tool2_tab9$server('code',vals)
      NULL
    })

    output$tool2_tab10_out<-renderUI({
      tool2_tab10$server('gen',vals)
      NULL
    })

    output$tool2_tab11_out<-renderUI({
      tool2_tab11$server('deldatalist',vals)
      NULL
    })


















  })
}

tool2_tab3<-list()
tool2_tab3$ui<-function(id){
  ns<-NS(id)
  div(style="height: calc(100vh - 100px);",

      div(style="position: fixed; top: 60px;right: 0px",

          div(
            style="display: flex; gap:10px",
            bsButton(ns("prev_import"), "< Previous",width='100px'),
            bsButton(ns("next_import"), "Next >",width='100px'))

      ),

      hidden(bsButton(ns("cancel_import"), "Cancel")),

      div(strong("Exchange Factors/Variables")),

      div(id = ns('step1'),

          radioButtons(ns("copy_transfer"),"Action:", c("Copy","Move"),inline=T),
          uiOutput(ns("update_estep1")),
          column(12,  class="mp0",
                 column(2,style="margin:0px;padding: 0px;width: 70px",
                        tags$label('Datalist:', style="padding-top:30px"),
                        tags$label('Attribute:', style="padding-top:25px")),

                 column(4,style="margin:0px;padding: 0px",
                        uiOutput(ns("import_from_data")),

                        pickerInput(ns("import_from_attr"), NULL ,choices=c("Factor-Attribute"="factor","Numeric-Attribute"="numeric"),selected="factor"),
                 ),

                 column(1,style="margin:0px;padding: 0px; width: 30px",align="center",
                        div(actionLink(ns('rev_datalist'),icon("arrow-right-arrow-left")), style="position: absolute; top: 35px;left: 10px "),
                        div(actionLink(ns('rev_attr'),icon("arrow-right-arrow-left")), style="position: absolute;top:80px; left: 10px;"),
                        bsTooltip(ns('rev_datalist'),"Switch","right"),
                        bsTooltip(ns('rev_attr'),"Switch","right")
                 ),

                 column(4,style="margin:0px;padding: 0px",
                        uiOutput(ns("import_to_data")),
                        pickerInput(ns("import_to_attr"), NULL, choices=c("Numeric-Attribute"="numeric","Factor-Attribute"="factor"),selected="numeric")
                 )
          ),
          column(12,  class="mp0",
                 column(6,class="virtual_small",
                        virtualPicker(ns("importvar"), label="Columns","selected" )
                 ),
                 column(6,
                        hidden(
                          radioButtons(
                            ns("hand_facs"),"Convertion type:",
                            choiceValues = list("Binary","Ordinal"),
                            choiceNames = list(div("Binary",span(id="conv_bin",icon("question-circle"))),div("Integer",span(id="conv_ord",icon("question-circle")))),
                            inline=T
                          )
                        ),
                        div(     uiOutput(ns("error_transf0"))),
                 )
          ),
          bsTooltip(ns("conv_bin"),"Creates a single binary column per factor level,with 1 indicating the class of that particular observation", placement = "right",options =list(style="width: 600px")),
          bsTooltip(ns("conv_ord"),"Creates a single column using numeric (integer) representation (values) of the factor levels", placement = "right")
      ),


      div(
        id = ns('step2'),
        uiOutput(ns("from_to")),

        div(style="height:450px;overflow-y: scroll;overflow-x: scroll",
            div(
              checkboxInput(ns("cutfacs"),span("Cut into intervals",tiphelp("divides the range of x into intervals and codes the values in x according to which interval they fall (cut function from R base).", "right")), F),
              uiOutput(ns("cut_fac_method")),
              div(

                div(uiOutput(ns("tofactor")),style="font-size: 11px;"),



              )
            ),

            uiOutput(ns("error_transf")),
            uiOutput(ns("ordinal_page")),
            uiOutput(ns("binary_page"))),

      ),
      div(id = ns('step3'),
          uiOutput(ns("page3")),

      )

  )
}
tool2_tab3$update_server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    observeEvent(input$rev_attr,{
      a=input$import_from_attr
      req(a)
      b=input$import_to_attr
      req(b)
      updatePickerInput(session,'import_from_attr',selected=b)
      updatePickerInput(session,'import_to_attr',selected=a)
    })
    observe({

      #shinyjs::toggle("rev_attr",condition=input$import_from_attr!=input$import_to_attr)
      shinyjs::toggle("rev_datalist",condition=input$import_from_data!=input$import_to_data)
    })
    observeEvent(input$rev_datalist,{
      a=input$import_from_data
      b=input$import_to_data
      updatePickerInput(session,'import_from_data',selected=b)
      updatePickerInput(session,'import_to_data',selected=a)
    })
  })
}
tool2_tab3$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    ns<-session$ns

    output$cut_fac_method<-renderUI({
      req(isTRUE(input$cutfacs))
      div(class="half-drop half-drop-inline",
          selectInput(
            ns("bin_method"),
            label = span("Bin method",tipify(actionLink(ns("bin_method_help"), icon("fas fa-question-circle")), "Click for details")
            ),
            choices = c("Sturges" = "sturge", "Scott" = "scott", "Freedman-Diaconis" = "freedman")
          )
      )
    })

    observeEvent(input$bin_method_help, {

      showModal(
        modalDialog(
          title = "Methods to initial guess of the number of cuts (or bins)",
          easyClose = TRUE,
          fluidRow(class='mp0',
                   tags$style(HTML(".formulas div.MathJax_Display{text-align: left !important;color: gray;white-space:normal;font-size: 11px}")),
                   div(class="formulas",
                       column(6,
                              strong("Sturges' Rule"),
                              div(withMathJax("$$\\text{Number of bins} = \\lceil \\log_2(n) + 1 \\rceil$$")),
                              hr(),
                              strong("Scott's Rule"),
                              div(withMathJax(helpText("$$\\text{Bin width} = \\frac{3.5 \\cdot \\sigma}{n^{1/3}}$$"))),
                              div(withMathJax(helpText("$$\\text{Number of bins} = \\left\\lceil \\frac{\\text{Range of data}}{\\text{Bin width}} \\right\\rceil$$"))),
                              hr(),
                              strong("Freedman-Diaconis Rule"),
                              div(withMathJax(helpText("$$\\text{Bin width} = 2 \\cdot \\frac{\\text{IQR}}{n^{1/3}}$$"))),
                              div(withMathJax(helpText("$$\\text{Number of bins} = \\left\\lceil \\frac{\\text{Range of data}}{\\text{Bin width}} \\right\\rceil$$"))),
                       ),
                       column(6,
                              div("Where:"),
                              div(withMathJax(helpText("$$n=\\text{number of observations}$$"))),
                              div(withMathJax(helpText("$$\\sigma=\\text{the standard deviation of the data}$$"))),

                              div(withMathJax(helpText("$$\\text{IQR}=\\text{the interquartile range of the data}$$")))
                       )






                   ))
        )
      )
    })


    output$from_to<-renderUI({
      div("from",strong(embrown(convert()[1])),"to",strong(emgreen(convert()[2])))
    })

    output$import_to_data<-renderUI({
      pickerInput(session$ns("import_to_data"), "To:", choices=names(vals$saved_data),selected=vals$cur_import_to_data)
    })
    output$import_from_data<-renderUI({
      pickerInput(session$ns("import_from_data"),  "From:", choices=names(vals$saved_data),
                  selected=vals$cur_import_from_data,
                  #selected=names(vals$saved_data)[5]
      )
    })
    observeEvent(input$import_from_data,{
      vals$cur_import_from_data<-input$import_from_data
    })
    observeEvent(input$import_to_data,{
      vals$cur_import_to_data<-input$import_to_data
    })
    observe({
      req(input$import_from_attr)
      req(input$import_from_data)
      req(input$import_from_data!="")
      data<-vals$saved_data[[input$import_from_data]]
      if(input$import_from_attr=="factor"){
        data<-attr(data,"factors")
      }

      shinyWidgets::updateVirtualSelect("importvar",choices=colnames(data), selected=colnames(data)[1])
    })
    gdata_from<-reactive({
      req(input$import_from_data)
      req(input$import_from_attr)
      data<-vals$saved_data[[input$import_from_data]]
      attr(data,"attr")<-"Numeric-Attribute"
      afrom<-input$import_from_attr
      if(afrom=="factor"){
        data<-attr(data,"factors")
        attr(data,"attr")<-"Factor-Attribute"
      }
      attr(data,"name")<-input$import_from_data
      data
    })
    gdata_to<-reactive({
      req(input$import_to_data)
      data<-vals$saved_data[[input$import_to_data]]
      ato=input$import_to_attr
      attr(data,"attr")<-"Numeric-Attribute"
      if(ato=="factor"){
        data<-attr(data,"factors")
        attr(data,"attr")<-"Factor-Attribute"
      }
      attr(data,"name")<-input$import_to_data
      data
    })
    val_transf<-reactive({
      try({

        dfrom<-gdata_from()
        req(dfrom)
        dto<-gdata_to()
        req(dto)
        result<-validate_transf(dfrom,dto)
        result

      },silent=T)
    })
    output$error_transf0<-renderUI({
      result<-val_transf()
      message<-attr(result,"logs")

      div(class="small_print2",
          render_message(message)
      )

    })
    output$error_transf<-renderUI({
      render_message(vals$error_transf)
    })
    page<-reactiveVal(1)
    step_max<-3
    observeEvent(input$next_import,{

      next_page<-page()+1
      page(next_page)
    })
    observe({
      if(page()==1){
        data2(NULL)
        vals$error_transf<-NULL
      }
    })
    observeEvent(input$prev_import,{
      vals$error_transf<-NULL
      data2(NULL)
      req(page()>1)
      next_page<-page()-1
      page(next_page)
    })
    observe({
      shinyjs::toggle( "prev_import", condition=page()>1)
      shinyjs::toggle('next_import',condition=page()<3&isTRUE(val_transf()))
      shinyjs::toggle('step1',condition=page()==1)
      shinyjs::toggle('step2',condition=page()==2)
      shinyjs::toggle('step3',condition=page()==3)
    })
    rank_dfs<-reactive({

      data<-get_data_cutted()

      vars<-colnames(data)

      result<-lapply(seq_along(vars),function(i){

        rank_lev<-input[[paste("rank_lev",vars[i],sep="_")]]

        req(rank_lev)
        level=as.numeric(rank_lev)
        label=sapply(levels(data[,vars[i]]),function(x){
          rank_label=input[[paste("ordrank_label",vars[i],x,sep="_")]]

          req(rank_label)
          rank_label
        })
        data.frame(level,label)
      })
      names(result)<-vars

      result

    })
    go_exchange<-reactive({

      req(page()>2)

      if(all(convert()==c("numeric","numeric"))){

        numeric2numeric()
      }   else if(all(convert()==c("numeric","factor"))){

        numeric2factor()
      } else if(all(convert()==c("factor","numeric"))){
        if(input$hand_facs=="Ordinal"){

          factor2numeric_ordinal()
        } else if (input$hand_facs=="Binary"){

          factor2numeric_binary()
        }
      } else if(all(convert()==c("factor","factor"))){

        factor2factor()
      }
    })
    numeric2numeric<-reactive({
      data<-get_data_cutted()
      newnames<-sapply(colnames(data),function(var){
        input[[paste("newbin",var, sep="_")]]
      })
      colnames(data)<-newnames
      data
    })
    factor2factor<-reactive({
      data<-get_data_cutted()

      vars<-colnames(data)


      rd<-rank_dfs()

      resdf<-data.frame(lapply(names(rd),function(var){
        levels<-levels(data[,var])
        numfac<-as.factor(as.numeric(data[,var]))
        levels0<-levels(numfac)
        ord<-as.numeric(rd[[var]]$level)
        labels=rd[[var]]$label
        fac<-factor(numfac,levels=ord,labels=  labels[ord])
        fac<-data.frame(fac)
        colnames(fac)<-input[[paste0("newcol",var,sep="_")]]
        fac
      }))
      rownames(resdf)<-rownames(gdata_from())

      resdf
    })
    numeric2factor<-reactive({
      factor2factor()
    })
    factor2numeric_binary<-reactive({


      data<-get_data_cutted()
      result<-capture_log2(factor2numeric_fun)(data,input)

      req(!inherits(result,"error"))
      result


    })
    factor2numeric_ordinal<-reactive({
      data<-get_data_cutted()
      vars<-colnames(data)
      result<-lapply(seq_along(vars),function(i){
        num_values<-sapply(seq_along(levels(data[,vars[i]])),function(x){
          input[[paste0("fac2num_value",x)]]
        })


        rank_lev<-input[[paste("rank_lev",vars[i],sep="_")]]
        level_labels= gsub("*.\\\n","",rank_lev)
        res<-as.numeric(as.character(factor(data[,vars[i]],levels=level_labels,labels = num_values)))
        print(res)

        res
      })
      names(result)<-vars
      result<-data.frame(result)
      rownames(result)<-rownames(gdata_from())
      result
    })
    factor2numeric_fun<-function(data, input){

      vars<-colnames(data)
      df<-data.frame(do.call(cbind,lapply(data,function(x) classvec2classmat(x))))
      cols<-colnames(getclassmat(data))
      newnames<-sapply(cols,function(var){
        input[[paste("newbin",var, sep="_")]]
      })
      rownames(df)<-rownames(data)
      colnames(df)<-newnames
      df
    }
    r_exchange<-reactiveVal()
    observeEvent(go_exchange(),{
      new<-go_exchange()
      r_exchange(new)
    })
    ##### page2
    action<-reactive({

      req(input$import_from_attr)
      req(input$import_to_attr)
      action0<-""
      if(input$import_from_attr!=input$import_to_attr){
        action0<- span(input$copy_transfer,
                       embrown(strong(input$import_from_attr)),
                       "to",
                       emgreen(strong(input$import_to_attr)))
      }
      action<-span(strong("Convert"))
      vars<- inline(div(style="max-width: 200px; color: brown; background: Gainsboro",em(paste0(input$importvar,collapse="; "))))
      sub<-NULL
      sub<- if(input$import_from_attr=="numeric"){
        if(input$import_to_attr=="numeric"){} else {
          span("(as factor)")
        }
      }
      sub<- if(input$import_from_attr=="factor"){
        if(input$import_to_attr=="numeric"){
          if(input$hand_facs=="Binary"){
            "(as binary columns)"
          } else{
            "(as value-factor-levels)"
          }
        }
      }

      div(
        div(
          h4(div(action0)),
          action,
          vars,
          span(sub, style="color:  #05668D;"),

        ))
    })
    observeEvent(page(),{
      try({
        if(page()==3){
          prep_exchange()
          page(2)
          if(!is.null(data2())){

            req(nrow(data2())==nrow(get_data_from()))
            confirm_modal(session$ns,action=action(),
                          data1=get_data_from(),
                          data2= data2(),
                          left='Original Data:',right="New Data:",
                          from='',
                          to=''
            )
          }
        }

      })
    })
    observeEvent(input$confirm,ignoreInit = T,{
      req(data2())
      removeModal()
      run_exchange()
      page(1)
      success_modal()
    })
    data_from0<-reactive({
      req(input$import_from_data)
      if(input$import_from_attr=="factor"){
        data<- attr(vals$saved_data[[input$import_from_data]],"factors")
      } else{
        data<-vals$saved_data[[input$import_from_data]]
      }
      data
    })
    data2<-reactiveVal()
    success_modal<-reactive({
      req(input$import_from_data)
      req(input$import_to_data)
      req(is.data.frame(data2()))


      data1=data_from0()
      data2=vals$saved_data[[input$import_to_data]]
      showModal(
        modalDialog(
          easyClose = T,
          h4("Sucess!"),
          get_basic_compare(data1=NULL,data2=data2(),left="",right="New:",to=paste(paste0(input$import_to_data,">"),paste0(first_upper(input$import_to_attr),"-Attribute")))

        )
      )
    })
    prep_exchange_fun<-function(from,to,afrom,ato,saved_data,r_exchange){

      if(ato=="factor"){
        factors<-attr(saved_data[[to]],"factors")[rownames(r_exchange),,drop=F]
        newfac<-cbind(factors,r_exchange)
        colnames(newfac)<-make.unique(colnames(newfac))
        return(newfac)

      } else{
        new<-saved_data[[to]][rownames(r_exchange),,drop=F]
        newdat<-cbind(new,r_exchange)
        colnames(newdat)<-make.unique(colnames(newdat))
        newdat<-data_migrate(saved_data[[from]],newdat)
        return(newdat)
      }
    }
    prep_exchange<-reactive({
      try({

        datalist_name<-input$import_to_data
        result<-capture_log2(prep_exchange_fun)(from=input$import_from_data,to=input$import_to_data,afrom=input$import_from_attr,ato=input$import_to_attr,saved_data=vals$saved_data,r_exchange=r_exchange())
        vals$error_transf<-attr(result,"logs")
        if(!inherits(result,"error")){
          attr(result,"datalist")<-datalist_name
          data2(result)
        }
      })
    })
    run_exchange<-reactive({
      from<-input$import_from_data
      to<-input$import_to_data
      afrom<-input$import_from_attr
      ato<-input$import_to_attr
      if(ato=="factor"){
        newfac<-data2()
        attr(vals$saved_data[[to]],"factors")<-newfac
        if(input$copy_transfer=="Move"){
          attr(vals$saved_data[[to]],"factors")[input$importvar]<-NULL
        }
      } else{
        newdat<-data2()
        vals$saved_data[[to]]<-newdat
        if(input$copy_transfer=="Move"){
          vals$saved_data[[to]][input$importvar]<-NULL
        }
      }
      updatePickerInput(session,"import_from_data",selected=from)
      updatePickerInput(session,"import_to_data",selected=to)
    })
    convert<-reactive({
      list(
        from=input$import_from_attr,
        to=input$import_to_attr
      )
    })
    getconvert_datavars<-reactive({
      req(input$import_from_attr)
      req(input$importvar)
      req(input$import_from_attr)
      data<-vals$saved_data[[input$import_from_data]]
      datalist_name<-input$import_from_data

      if(input$import_from_attr=="factor"){
        data<- attr(vals$saved_data[[input$import_from_data]],"factors")

      }
      if(all(convert()==c("numeric","factor"))){
        res<-do.call(data.frame,lapply(data,function(x) as.factor(x)))
        rownames(res)<-rownames(data)
        data<-res
      }

      attr(data,"datalist")<-datalist_name
      vars<-as.list(input$importvar)
      req(vars%in%colnames(data))

      result<-list(data,vars)
    })
    observeEvent(convert(),{
      if(all(convert()[2]==c("factor"))){
        shinyjs::hide('hand_facs')
      } else if(all(convert()==c("numeric","numeric"))) {
        shinyjs::hide('hand_facs')
      } else{
        shinyjs::show('hand_facs')
      }
    })
    observeEvent(convert(),{
      if(all(convert()==c("factor","numeric"))){
        updateCheckboxInput(session,'cutfacs',value=F)
        shinyjs::hide('cutfacs')
      } else{
        if(all(convert()==c("factor","factor"))){
          updateCheckboxInput(session,'cutfacs',value=F)
          shinyjs::hide('cutfacs')
        } else{
          shinyjs::show('cutfacs')
        }

      }
    })
    output$tofactor<-renderUI({
      ns<-session$ns
      if(all(convert()==c("factor","numeric"))){
        req(input$hand_facs=="Ordinal")
      } else{
        req(all(convert()[2]==c("factor")))
      }
      div(

        div(icon("hand-point-right"),"Drag and drop the levels (green blocks) to define their values",style='white-space: normal;'),
        if(all(convert()[2]==c("factor")))
          div(icon("hand-point-right"),"Edit label names (blue blocks)"),
        if(all(convert()==c("factor","numeric")))
          div(icon("hand-point-right"),"Edit numeric values (blue blocks)"),
        div(uiOutput(ns("dropLevelsEdit_a")),
            style="font-size: 11px;"),
        div(uiOutput(ns('dropLevelsEdit_b'))),
      )
    })


    dropLevelsEdit<-reactive({
      ns<-session$ns
      data<-get_data_cutted()
      vars<-getconvert_datavars()[[2]]
      vars<-unlist(vars)
      l1<-lapply(seq_along(vars),function(i){
        output[[paste0('facord_level',vars[i])]]<-renderUI({i})
        output[[paste0('facord_outl',vars[i])]]<-renderUI({
          valid<-input[[paste0("cutfacs_breaks_",vars[i])]]
          col1<-column(4,class="mp0",
                       lapply(seq_along(levels(data[,vars[i]])),function(x){
                         if(convert()[2]=="numeric"){
                           column(12,class="mp0 ord_label",
                                  div(class="num_label",
                                      numericInput(ns(paste0("fac2num_value",x)),NULL,x,width= '75px')
                                  ))
                         }else{div(x,style="border-top:1px solid; border-bottom:1px solid; height: 24px;margin: 0px;text-align: center")}

                       }))
          col2<-column(8,class="mp0",{

            sortable:: rank_list(
              labels = lapply(1:nlevels(data[,vars[i]]),function(x){
                lev<-levels(data[,vars[i]])[x]
                div(class="ord_label factor_label",x,
                    if(convert()[2]=="factor"){
                      div(textInput(ns(paste("ordrank_label",vars[i],lev,sep="_")),NULL,lev, width= "95px"))
                    } else {div(lev,class='form form-group form-control shiny-input-container',style="padding-top: 5px; ;padding-left: 5px;position: absolute;left: 0px;background: #D0F0C0;")}
                )
              }),
              input_id = ns(paste("rank_lev",vars[i],sep="_")),
              class="rankcol sortable"
            )
          })
          if(convert()[2]=="numeric"){
            div(col2,col1)
          } else{
            div(col1,col2)
          }


        })
      })
      NULL
    })
    output$dropLevelsEdit_a<-renderUI({
      ns<-session$ns
      data<-getconvert_datavars()[[1]]
      vars<-getconvert_datavars()[[2]]
      vars<-unlist(vars)
      getl1<-function(id_text,text_value,div2_before="",div1="Value",div2="Label",class_1="form-control", class_2="form-control", style_1="",style_2="",div1_out="",div2_out=""){
        col1<-column(4,class="mp0",
                     div(div(div1,style="text-align: center"),
                         class="half-drop rankcol",
                         style=style_1),
                     div1_out

        )
        col2<-column(8,class="half-drop rankcol",
                     #column(3,class="mp0",div2_before),
                     column(12,class="mp0",div2,style="text-align: center"))
        column(4,style="padding-right: 5px;",
               column(12,
                      class="half-drop rankcol",
                      textInput(id_text,NULL,text_value)),
               column(12,class="mp0",
                      if(convert()[2]=="numeric"){
                        div(col2,col1)
                      } else{
                        div(col1,col2)
                      },
                      column(12, div2_out,class="mp0")
               )

        )
      }
      data<-get_data_from()
      data_o<-vals$saved_data[[input$import_from_data]]

      div1="Levels"
      col2_name<-"Label"
      if(convert()[2]=="numeric"){
        div1="Level"
        col2_name="Numeric-Value"
      }
      vars<-colnames(data)
      if(isTRUE(input$cutfacs)){
        div2_before="Cut"
        div2=function(data=NULL,var=NULL){
          fun<-switch(input$bin_method,
                      'sturge'=bin_Sturges,
                      'scott'=bin_Scott,
                      'freedman'=bin_Freedman
          )

          div(class="rankcol",
              numericInput(ns(paste0("cutfacs_breaks_",var)),
                           NULL,
                           value =fun(data_o[,var])
              ))
        }
      } else{
        div2_before=""
        div2=function(data=NULL,var=NULL){div(col2_name)}
      }

      l1<-lapply(vars,function(var){
        div(style="margin-top: 5px;margin-bottom: 15px",
            getl1(id_text=ns(paste0("newcol",var,sep="_")),
                  var,
                  div1=div(if(convert()[2]=="numeric"){div2(data,var)}else{div1},style=""),
                  #div2_before=div(div2_before,style="border: 1px solid blue"),
                  div2=div(
                    if(convert()[2]=="numeric"){div1}else{div2(data,var)},style=""
                  ),
                  div2_out= div(style="padding-bottom: 30px",
                                uiOutput(ns(paste0('facord_outl',var)))
                  )

            )


        )
      })
      div(
        id="rank_levels",
        l1)
    })
    output$dropLevelsEdit_b<-renderUI({
      dropLevelsEdit()
    })
    get_factor_cuts<-function(l1){
      do.call(data.frame,lapply(l1, function (x) {
        cut(as.numeric(as.character(x[[1]])),as.numeric(x[[2]]))
      }))
    }
    get_data_from<-reactive({
      ns<-session$ns
      data<-getconvert_datavars()[[1]]
      vars<-getconvert_datavars()[[2]]
      data<-data[,unlist(vars),drop=F]
      datalist_name<-input$import_from_data
      if(convert()[[2]]=="factor"){
        data<-data.frame(lapply(data,as.factor))
      }

      colnames(data)<-vars
      attr(data,"datalist")<-datalist_name
      data
    })
    get_data_cutted<-reactive({
      data<-get_data_from()
      datalist_name<-attr(data,"datalist")
      vars<-colnames(data)
      if(isTRUE(input$cutfacs)){
        res<-lapply(vars, function(var) {
          cut=input[[paste0("cutfacs_breaks_",var)]]
          req(cut)
          cut(as.numeric(as.character(data[,var])),cut)
        })
        res<-data.frame(res)
        colnames(res)<-colnames(data)
        data<-res
      } else{
        if(convert()[2]=="factor"){
          data<-data.frame(lapply(data,as.factor))
        }
      }
      attr(data,"datalist")<-datalist_name
      data
    })
    output$binary_page<-renderUI({
      ns<-session$ns
      req(input$import_from_data)
      req(input$importvar)
      data<-vals$saved_data[[input$import_from_data]]
      if(input$import_from_attr=="factor"){
        req(input$import_to_attr=="numeric")
        req(input$hand_facs=="Binary")
        factors<-attr(data,"factors")[,input$importvar,drop=F]
        cols<-colnames(getclassmat(factors))
      }
      if(input$import_from_attr=="numeric"){
        req(input$import_to_attr=="numeric")
        cols<-colnames(data[,input$importvar,drop=F])
      }

      lbin<-lapply(cols,function(x){
        div(class="new_colnames",textInput(ns(paste("newbin",x, sep="_")), NULL, value=x))
      })

      div(style="overflow-y: auto; max-height: calc(100vh - 200px);margin-top: 20px",
          div(icon("hand-point-right"),"Edit column names",style='white-space: normal;font-size: 11px'),
          h5(strong("New column names:")),
          lbin)
    })
  })
}
toolbar<-list()
toolbar$ui<-function(id){
  ns<-NS(id)
  choiceNames_preprocess<-{list(
    div(id="tools_drop0",class="tool",
        div(cogs_title="Create Datalit",icon("plus")
        )),
    div(
      div(class="tool",
          div(cogs_title="Options",id=ns('toolkit'),
              icon("cog")))),
    div(class="tool",
        div(cogs_title="Filter observations",
            icon("fas fa-list")
        )),
    div(class="tool",
        div(cogs_title="Filter variables",
            icon("list",class="fa-rotate-90")
        )),
    div(class="tool",
        div(cogs_title="Transformation",
            icon("hammer"))
    ),
    div(class="tool",
        div(cogs_title="Data imputation",
            img(src=na_icon,height='14',width='14',class="blur"))
    ),
    div(class="tool",
        div(cogs_title="Data partition",
            img(src=split_icon,height='14',width='14',class="blur"))
    ),
    div(class="tool",
        div(cogs_title="Aggregate",
            img(src=agg_icon2,height='14',width='14',class="blur"))
    ),
    div(class="tool",
        div(cogs_title="Create palette",
            icon(verify_fa=FALSE,name=NULL,class="fas fa-palette"))
    ),
    div(class="tool",style="z-index: 999",
        div(cogs_title="Savepoint",
            icon(verify_fa=FALSE,name=NULL,class="fas fa-thumbtack"))
    ),
    div(class="tool",style="width: 10px; background-color:   #05668D; width: 45px;height: 30px")
  )}

  div(class="preproc_page",
      div(
        uiOutput(ns('pp_title')),

        div(
          div(class="tool_toggle ",id=ns('imput_tabs'),
              navbarPage(
                NULL,id=ns("radio_cogs"),selected="tool_close",
                tabPanel(choiceNames_preprocess[11],value='tool_close',

                ),
                tabPanel(
                  choiceNames_preprocess[2],value='tool2',
                  tool2$ui(ns("tool2")),

                  uiOutput(ns("out_tool2"))
                ),
                tabPanel(choiceNames_preprocess[3],value='tool3',
                         div(class="tool_page0",
                             div(class="tool_page",
                                 tool3$ui(ns("tool3")),
                                 uiOutput(ns("out_tool3")))
                         )
                ),
                tabPanel(choiceNames_preprocess[4],value='tool4',
                         class="tool_page",
                         tool4$ui(ns("tool4")),
                         uiOutput(ns("out_tool4"))
                ),
                tabPanel(choiceNames_preprocess[5],value='tool5',
                         class="tool_page",
                         tool5$ui(ns("tool5")),
                         uiOutput(ns("out_tool5"))
                ),
                tabPanel(choiceNames_preprocess[6],value='tool6',
                         class="tool_page",
                         tool6$ui(ns("tool6")),
                         uiOutput(ns("out_tool6"))
                ),
                tabPanel(choiceNames_preprocess[7],value='tool7',
                         class="tool_page",
                         tool7$ui(ns("tool7")),
                         uiOutput(ns("out_tool7"))
                ),
                tabPanel(choiceNames_preprocess[8],value='tool8',
                         class="tool_page",
                         tool8$ui(ns("tool8")),
                         uiOutput(ns("out_tool8"))
                ),
                tabPanel(choiceNames_preprocess[9],value='tool9',
                         class="tool_page",
                         tool9$ui(ns("tool9")),
                         uiOutput(ns("out_tool9"))
                ),
                tabPanel(choiceNames_preprocess[10],value='tool10',
                         class="tool_page",
                         tool10$ui(ns("tool10")),
                         uiOutput(ns("out_tool10"))

                )
              )
          )))
  )
}

toolbar$server<-function(id, vals=NULL){
  moduleServer(id,function(input, output,session){

    shinyjs::onevent("mouseenter", "toolkit",{
      show(selector=".toolkit_items")

    })

    observeEvent(vals$update_pp,{
      updateTabsetPanel(session,"radio_cogs",selected=vals$update_pp)
      vals$update_pp<-NULL
    })



    observeEvent(input$radio_cogs,ignoreInit = T,{


      shinyjs::removeClass(selector=".tool_page",class="border_alert")
    })

    observe({
      shinyjs::toggle(selector=".fade_pp", condition = input$radio_cogs!="tool_close")

      shinyjs::toggle("pp_title",condition =condition_title())

      toggle(selector=".track_changes_panel",condition=!input$radio_cogs%in%c("tool_close","tool2","tool9","tool10"))
    })


    condition_data_selector<-reactive({
      input$radio_cogs%in%c(paste0("tool",3:8))
    })
    condition_title<-reactive({
      !input$radio_cogs%in%c('tool_close',"tool2")
    })
    observe({
      shinyjs::toggle(selector=".pp_datacontrol",condition =condition_data_selector())

    })
    title<-reactiveVal()
    observeEvent(input$radio_cogs,{
      t<-switch(input$radio_cogs,
                "tool3"={'Filter observations'},
                "tool4"={'Filter variables'},
                "tool5"={'Transformations'},
                "tool6"={'Data imputation'},
                "tool7"={"Data partition"},
                "tool8"={'Aggregate'},
                "tool9"={'Create Palette'},
                "tool10"={"Savepoint"})
      title(t)
    })
    output$pp_title<-renderUI({
      req(input$radio_cogs!='tool_close')
      div(class="pp_title",
          div(class="t_title",title())
      )

    })
    tool_values<-reactiveValues()


    get_tool_result<-reactiveVal()
    observeEvent(vals$pp_data,{
      shinyjs::reset("imput_tabs")
    })
    observe({
      vals$tosave<-vals$vtools[[input$radio_cogs]]


    })



    output$out_tool2<-renderUI({
      tool2$server('tool2',vals)
      NULL
    })

    output$out_tool3<-renderUI({
      tool3$server("tool3",vals)
      NULL
    })

    output$out_tool4<-renderUI({
      tool4$server("tool4",vals)
      NULL
    })


    output$out_tool5<-renderUI({
      tool5$server("tool5",vals)
      NULL
    })

    output$out_tool6<-renderUI({
      tool6$server("tool6",vals)
      NULL
    })

    output$out_tool7<-renderUI({
      tool7$server("tool7",vals)
      NULL
    })

    output$out_tool8<-renderUI({
      tool8$server("tool8",vals)
      NULL
    })

    output$out_tool9<-renderUI({
      tool9$server("tool9",vals)
      NULL
    })


    output$out_tool10<-renderUI({
      tool10$server("tool10",vals)
      NULL
    })


    observeEvent(input$radio_cogs_off,ignoreInit = T,{
      switch(input$radio_cogs,
             "tool2"={ tool2$server('tool2',vals)},
             "tool3"={  tool3$server("tool3",vals)},
             "tool4"={tool4$server("tool4",vals)},
             "tool5"={tool5$server("tool5",vals)
             },
             "tool6"={tool6$server("tool6",vals)},
             "tool7"={ tool7$server("tool7",vals)},
             "tool8"={ tool8$server("tool8",vals)},
             "tool9"={  tool9$server("tool9",vals)},
             "tool10"={ tool10$server("tool10",vals)})
    })







    observeEvent(vals$pp_data,{
      vals$r_imputed<-vals$pp_data
    })



    return(NULL)

  })
}



#' @export
pre_process<-list()
#' @export
pre_process$ui<-function(id){
  ns<-NS(id)
  div(style="width: 50%;", tags$head(tags$script(HTML(paste0("$(document).on('click', '.needed', function () {debugger;
Shiny.onInputChange('",ns('last_btn'),"', this.id);
});")))),


div(class="needed",id="fade",
    div(class="fade_pp pp-full",style="display: none",
        div(style="position: fixed; right: 550px;top: 50px ",
            actionButton(ns("exit_preprocess"), label = NULL, icon = icon("times"), style = "padding: 0px; font-size: 15px; width: 20px; height: 20px;background: Brown; color: white; border: 0px;"))
    )
),
div(class="fade_pp track_changes_panel",
    style="position: fixed; right: 550px;top: 115px;overflow-y:auto;  box-shadow: 0 0px 1px  grey; z-index: 999;font-size: 9px;background: white; padding: 10px;display: none;",
    div(
      div(style="display: flex",align="right",
          actionButton(ns("summary_on"),"Track changes",style="font-size: 11px; padding: 1px;border: 0px solid;font-style: italic;"),
          div(class="swib off on"),
      )),
    div(style="display: none;",id=ns("summary_pp_out"),
        uiOutput(ns('summary_pp'))
    )
),

div(class="needed pp_head",id="not_fade",
    div(
      div(
        div(style="display: flex; gap: 5px",class="preproc_control",
            pp_data$ui(ns('head')),
            div(class="pp_datacontrol half-drop",style="display: none;",
                div(style=" display: flex; gap: 5px",actionButton(ns("reset_change"),icon("undo"), width="40px"),
                    div(
                      class='save_pp',
                      actionButton(ns("tools_save_changes"),icon("fas fa-save"), width="40px")
                    ))
            ),
            bsTooltip(ns("reset_change"),"Reset changes"),
            bsTooltip(ns("tools_save_changes"),"Save Changes")
        )
      ),
      div(class="toolbar",
          id=ns("tool_pages"),
          div(class="dl_btn",
              cogs_title="Create Datalit",
              popify(bsButton(ns("create_datalist"), strong(icon("fas fa-plus")), style=""),"Data Input",textinput())
          ),
          div(

            toolbar$ui(ns("toolbar"))
          )

      )

    ),

    uiOutput(ns("pp_out"))

)
  )
}
#' @export
pre_process$server<-function(id, vals){
  moduleServer(id,function(input,output,session){
    ####
    r_action<-reactive({
      action=attr(vals$tosave,"action")
      if(is.null(action)){
        action<-"datalist"
      }
      action
    })
    r_factors<-reactiveVal()
    observeEvent(input$reset_change,ignoreInit = T,{

      shinyjs::addClass(selector=".run_na_btn",clas="save_changes")
      data<-vals$pp_data
      vals$pp_data<-NULL
      vals$pp_data<-data
      vals$tosave<-data
      vals$vtools$tool6<-data

    })
    output$title<-renderUI({
      req(r_action())
      if(r_action()=="datalist"){
        "Save changes"
      } else if(r_action()=="factor-column"){
        if(ncol(vals$vtools$tool7_partition)==1){"Create/Replace Factor-Attribute Column"} else{
          "Add Partition columns to Factor-Attribute"
        }

      } else if(r_action()=="numeric-column"){
        "Create/Replace Nmeric-Attribute Column"
      }
    })
    output$newdata<-renderUI(basic_summary2(get_tosave()))
    r_choices_over<-reactive({
      action=r_action()
      if(action=="datalist"){
        return(names(vals$saved_data))
      } else if(action=="factor-column"){
        factors<-attr(vals$tosave,"factors")
        factors["New partition"]<-NULL
        choices_over=colnames(factors)
        return(choices_over)
      }
    })
    observeEvent(input$tools_save_changes,ignoreInit = T,{
      ns<-session$ns

      showModal(
        modalDialog(
          title=uiOutput(ns("title")),
          easyClose =T,
          footer=div(actionButton(ns("data_confirm"),strong("confirm")),
                     modalButton("Cancel")),
          div(style="padding: 20px",
              div(class="half-drop",
                  style="display: flex",
                  div(style="width: 20%",
                      uiOutput(ns('create_replace_out'))

                  ),
                  div(style="padding-top: 10px",
                      uiOutput(ns("out_newdatalit")),
                      uiOutput(ns("out_newcolumns")),
                      uiOutput(ns("out_overdatalist"))
                  )

              ),
              div(style="padding-left: 30px",
                  uiOutput(ns("message")),
                  uiOutput(ns('newdata')),

              )

          )

        )
      )
    })

    output$out_newcolumns<-renderUI({
      req(r_action())
      req(r_action()=="factor-column")
      req(ncol(vals$vtools$tool7_partition)>1)
      newnames<-colnames(vals$vtools$tool7_partition)
      emgray(paste(newnames,collapse=", "))

    })

    output$create_replace_out<-renderUI({
      ns<-session$ns
      req(r_action())
      choices<-c("Create","Replace")
      if(r_action()=="factor-column"){
        if(ncol(vals$vtools$tool7_partition)>1){
          choices<-"Create"}
      }
      radioButtons(ns("create_replace"),NULL,choices)
    })
    new_datalist_name<-reactive({
      req(input$create_replace)
      if(input$create_replace=="Create"){
          req(input$newdatalit)
          input$newdatalit
      } else {
        req(input$overdatalist)
        input$overdatalist
      }

    })
    observeEvent(new_datalist_name(),{
      if(r_action()%in%"datalist"){
       # attr(vals$tosave,"new_datalist")<-new_datalist_name()
      } else{
       # attr(vals$tosave,"new_datalist")<-attr(vals$pp_data,'datalist_root')
      }
    })


    output$out_newdatalit<-renderUI({
      req(input$create_replace=="Create")
      ns<-session$ns
      action=r_action()
      if(action=="datalist"){
        newnames<-make.unique(c(names(vals$saved_data),vals$bagname))
        name0<-newnames[length(newnames)]
      } else if(action=="factor-column"){
        req(ncol(vals$vtools$tool7_partition)==1)
        newnames<-make.unique(c(colnames(attr(vals$saved_data[[vals$cur_data]],"factors")),vals$bagname))
        name0<-newnames[length(newnames)]
      }
      textInput(ns("newdatalit"),NULL,name0)
    })




    output$out_overdatalist<-renderUI({
      ns<-session$ns
      req(input$create_replace=="Replace")
      action=r_action()
      if(action=="factor-column"){
        req(ncol(vals$vtools$tool7_partition)==1)}
      selectInput(ns("overdatalist"),NULL,choices=r_choices_over(),selected=vals$cur_data)
    })
    datalistnew<-reactive({
      req(input$create_replace)
      if(input$create_replace=="Create"){
        input$newdatalit
      } else {
        input$overdatalist
      }
    })
    get_tosave<-reactive({
      tosave<-vals$tosave
      attr(tosave,"datalist")<-attr(tosave,"new_datalist")
      attr(tosave,"new_datalist")<-NULL
      tosave
    })
    observeEvent(input$data_confirm,ignoreInit = T,{
      tosave<-get_tosave()
      req(input$data_confirm%%2)
      if(r_action()=="datalist"){
        vals$saved_data[[datalistnew()]]<-tosave
      } else if(r_action()=="factor-column"){
        datalist_root<-attr(tosave,"datalist_root")
        factors<-attr(tosave,"factors")
        if(input$create_replace=="Create"){
          if(ncol(vals$vtools$tool7_partition)==1){
            colnames(factors)[ncol(factors)]<-datalistnew()}
        } else {
          if(ncol(vals$vtools$tool7_partition)==1){
            req(input$overdatalist%in%colnames(factors))
            factors[input$overdatalist]<-factors[ncol(factors)]
            factors[ncol(factors)]<-NULL
          }

        }
        attr(vals$saved_data[[datalist_root]],"factors")<-factors

      }


      tosave<-NULL
      removeModal()
      done_modal()
    })
    observe({
      req(vals$tosave)
      data<-data()
      attr(data,"datalist_root")<-attr(vals$pp_data,"datalist_root")
      vals$bagname<-create_newname(vals$saved_data,data,
                                   bag=attr(vals$tosave,"bag"),
                                   action=attr(vals$tosave,"action")
      )
    })
    ####



    observeEvent(vals$tosave,{
      output$summary_pp<-renderUI({
        data<-vals$tosave
        div(
          #renderPrint(vals$tosave),
          basic_summary2(data)
        )
      })
    })


    observeEvent(input$summary_on,{
      shinyjs::toggle("summary_pp_out")
      shinyjs::toggleClass(selector=".swib",class='off')
    })



    observeEvent(input$create_datalist,ignoreInit = T,{
      showModal(tool1$ui(session$ns("tool1")))
      #vals$cur_dl<-1
    })
    tool1$server("tool1",vals)
    observeEvent(input$last_btn,{
      if(input$last_btn=="fade"){
        updateNavbarPage(session,"toolbar-radio_cogs", selected="tool_close")
        shinyjs::hide(selector=".fade_panel2")
      }

    })

    observeEvent(vals$exit_tool8,{
      updateNavbarPage(session,"toolbar-radio_cogs", selected="tool_close")
      shinyjs::hide(selector=".fade_pp")
    })

    observeEvent(input$save_bug,{
      saveRDS(reactiveValuesToList(input),"input.rds")

    })
    pp_data$server('head',vals)
    toolbar$server("toolbar",vals)
  })
}
