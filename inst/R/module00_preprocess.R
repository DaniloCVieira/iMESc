
# Replace outlier values with NA in the provided dataset based on a list of identified outliers.
remove_outliers<-function(d1,outliers){
  if(is.null(outliers) || !nrow(outliers)){
    return(d1)
  }
  stopifnot(all(c("id","var") %in% names(outliers)))
  row_idx<-match(outliers$id, rownames(d1))
  col_idx<-match(outliers$var, colnames(d1))
  ok<-!is.na(row_idx)&!is.na(col_idx)
  if(any(ok)){
    d1[cbind(row_idx[ok], col_idx[ok])]<-NA
  }
  d1
}
# Identify outliers for each column of a dataset using quantiles and IQR-based thresholds.
get_outliers<-function(data, q1=0.05,q2=0.95, upper_bound=1.5,lower_boud=1.5){
  numeric_columns<-which(vapply(data,is.numeric,logical(1)))
  if(!length(numeric_columns)){
    return(data.frame())
  }
  outs<-lapply(numeric_columns,function(column) {
    detect_outliers(data, column,q1,q2, upper_bound=upper_bound,lower_boud=lower_boud)
  })
  outs<-Filter(Negate(is.null),outs)
  if(!length(outs)){
    return(data.frame())
  }
  do.call(rbind,outs)
}
# Calculate the lower and upper bounds for identifying outliers based on IQR and thresholds.
detect_outliers <- function(data, column,q1,q2, upper_bound=1.5,lower_boud=1.5) {
  values <- data[[column]]
  if(!is.numeric(values)){
    return(NULL)
  }
  Q1 <- quantile(values, q1, na.rm = TRUE, names=FALSE)
  Q3 <- quantile(values, q2, na.rm = TRUE, names=FALSE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - lower_boud * IQR
  upper_bound <- Q3 + upper_bound * IQR
  indices <- which(values < lower_bound | values > upper_bound)
  if(!length(indices)){
    return(NULL)
  }
  var <- colnames(data)[column]
  data.frame(
    variable=var,
    var=var,
    id=rownames(data)[indices],
    value = values[indices],
    Q1=as.numeric(Q1),
    mean=mean(values,na.rm=TRUE),
    Q3=as.numeric(Q3),
    max=max(values,na.rm=TRUE),
    stringsAsFactors=FALSE
  )
}
imesc_outliers<-list()
# Define the user interface for the outlier detection module, including input controls and output panels.
imesc_outliers$ui <- function(id,vals) {
  ns <- NS(id)
  fluidRow(
    column(
      12, class = "mp0",
      column(
        4, class = "mp0",
        box_caret(
          ns("box1"),
          title = "Setup",
          color = "#c3cc74ff",
          div(

            virtualPicker(
              ns("data_x"),
              label=tiphelp5("Datalist", "Select the target datalist"),
              choices=names(vals$saved_data), width='200px',optionHeight="20px",keepAlwaysOpen=F,
              style="height: 24px",multiple = F
            ),
            numericInput(
              ns("q1"),
              tiphelp5("Quantile 1 (Q1)", "Enter the lower quantile threshold for detecting outliers (e.g., 0.05)."),
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.01
            ),
            numericInput(
              ns("q2"),
              tiphelp5("Quantile 2 (Q2)", "Enter the upper quantile threshold for detecting outliers (e.g., 0.95)."),
              value = 0.95,
              min = 0,
              max = 1,
              step = 0.01
            ),
            numericInput(
              ns("upper_bound"),
              tiphelp5("Upper Bound Multiplier", "Set the multiplier for the upper bound (e.g., 1.5)."),
              value = 1.5,
              min = 0,
              step = 0.1
            ),
            numericInput(
              ns("lower_boud"),
              tiphelp5("Lower Bound Multiplier", "Set the multiplier for the lower bound (e.g., 1.5)."),
              value = 1.5,
              min = 0,
              step = 0.1
            ),
            actionButton(ns("run_outlier"),"RUN>>")

          )),
        div(id=ns("remove_control"),
            box_caret(
              ns("box2"),
              color = "#c3cc74ff",
              title="Select targets to remove",
              div(style='display:flex',
                  uiOutput(ns("result3"))

              )
            )
        )
      ),
      column(
        8,class="mp0",
        box_caret(
          ns("box3"),
          title="Plot",
          button_title2=
            radioGroupButtons(
              ns("result"),NULL,
              c("Summary","Result","Remove"),selected='Summary'
            ),
          tabsetPanel(
            title=NULL,
            id=ns("result_panel"),
            type="hidden",
            tabPanel(
              'Summary',
              uiOutput(ns("result1"))
            ),
            tabPanel(
              'Result',
              div(style="overflow-x: auto",
                  uiOutput(ns("result2"))
              )
            ),
            tabPanel(
              'Remove',
              div(style="overflow-x: auto",
                  div(style="display: flex",
                      actionButton(ns('run_remove'),tiphelp5("Pre-RUN","This is a pre-run: selected targets will be replaced with NAs"),style="height: 30px"),
                      div(actionLink(ns('reset'),"[reset]"))
                  ),
                  uiOutput(ns("result6")),
                  checkboxInput(ns('show_boxplot'),tiphelp5("Show boxplot","Generates a boxplot with selected targets")),

                  div(
                    uiOutput(ns('result5'))
                  )
              )
            )
          )
        )
      )
    )
  )
}
# Define the server logic for the outlier detection module, including data processing and interactivity.
imesc_outliers$server<-function (id,vals ){



  moduleServer(id,function(input, output, session){
    observeEvent(input$result,{
      updateTabsetPanel(session,'result_panel',selected=input$result)
    })

    observe({
      shinyjs::toggle('remove_control', condition=input$result=="Remove")
    })


    cur_outliers<-reactiveVal()
    observeEvent(input$run_outlier,ignoreInit = T,{
      data<-vals$saved_data[[input$data_x]]
      outs<-get_outliers(data,
                         q1=input$q1,
                         q2=input$q2,
                         upper_bound=input$upper_bound,
                         lower_boud=input$lower_boud)

      cur_outliers(outs)


    })

    output$result1<-renderUI({
      validate(need(length(cur_outliers())>0,"Outliers were not analysed yet"))
      outs<-  cur_outliers()
      n_outliers<-sapply(split(outs,outs$variable),nrow)


      div(
        div(em("Total number of outlier values detected:"),  strong(sum(n_outliers))),
        div(class="half-drop-inline",
            fixed_dt(   data.frame(n_outliers))

        )
      )
    })


    output$result2<-renderUI({
      validate(need(length(cur_outliers())>0,"Outliers were not analysed yet"))
      div(class="half-drop-inline",
          fixed_dt(  cur_outliers())

      )
    })


    boxplot<-reactive({
      req(input$show_boxplot)
      outs<-cur_outliers()
      req(outs)
      row<-as.numeric(input$out_selected)
      req(length(row)>0)
      outs<-outs[row,]

      data<-pre_run()
      d1<-data[,unique(outs$variable)]

      oi<-split(outs$id, outs$variable)
      d2<-reshape2::melt(data.frame(id=rownames(d1),d1),"id")
      li<-split(d2,d2$variable)
      d3<-do.call(rbind,lapply(names(li),function(i){
        x<-li[[i]]
        x$out_flag<-x$id%in%oi[[i]]
        x
      }))
      res<-data.frame(d3)
      p<-ggplot(res, aes(x=variable, y=value))
      p<-p+stat_boxplot(geom='errorbar', linetype=1, width=0.3,color='gray20')+
        geom_boxplot(fill="white")+  geom_boxplot(varwidth =F,size=1,color="gray20")
      p+ geom_point(
        aes(color = out_flag)
      )+scale_color_manual(values=c("red","darkblue"))+
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
        )
    })

    bag<-reactiveVal(F)
    output$result6<-renderUI({
      req(isTRUE(bag()))
      outs<-cur_outliers()
      row<-as.numeric(input$out_selected)
      req(length(row)>0)

      outs<-outs[row,]

      n_outliers<- nrow(outs)
      div(style="display: flex; gap: 20px",
          div(
            em("Total number of outlier values replaced by NAs:"),  strong(sum(n_outliers))
          ),
          div(class="save_changes",
              actionButton(session$ns("save"),icon("fas fa-save"))
          )
      )
    })

    observeEvent(input$save,{

      data_o<-vals$saved_data[[input$data_x]]
      data<-pre_run()
      data<-data_migrate(data_o,data)
      bag<-paste0(input$data_x,"_","rm_outs")
      attr(data,"bag")<-bag
      vals$newdatalist<-data
      module_save_changes$ui(session$ns("isp-create"), vals)
    })
    module_save_changes$server("isp-create", vals)



    output$result5<-renderUI({



      renderPlot(
        boxplot()

      )
    })

    output$result4<-renderUI({
      outs<-cur_outliers()
      row<-as.numeric(input$out_selected)
      renderPrint(
        outs[row,]
      )
    })

    observe({
      shinyjs::toggle('reset',condition=length(input$out_selected)>0)
      shinyjs::toggle('run_remove',condition=length(input$out_selected)>0)
    })

    observeEvent(input$reset,{
      bag(FALSE)
      pre_run(vals$saved_data[[input$data_x]])
    })
    pre_run<-reactiveVal()


    observeEvent(input$data_x,{
      req(input$data_x%in%names(vals$saved_data))
      pre_run(vals$saved_data[[input$data_x]])
    })

    observeEvent(input$run_remove,{
      outs<-cur_outliers()
      row<-as.numeric(input$out_selected)
      req(length(row)>0)
      outs<-outs[row,]
      data<-vals$saved_data[[input$data_x]]
      newdata<-remove_outliers(data,outs)
      pre_run(newdata)
      bag(T)
    })
    output$result3<-renderUI({
      validate(need(length(cur_outliers())>0,"Outliers were not analysed yet"))
      outs<-cur_outliers()
      outs$input_id<-1:nrow(outs)
      choices=split(outs[c('id' ,"input_id")],outs$variable)

      choices=lapply(choices,function(x){
        v=x$input_id
        names(v)<-x$id
        v
      })



      div(
        virtualPicker(
          session$ns("out_selected"),
          label = NULL,
          choices = choices,
          multiple = TRUE,
          search = TRUE,
          showGroups = TRUE,
          style="font-size: ",
          width="250px"
        )
      )


    })

    observeEvent(input$data_x,{
      vals$cur_data<-input$data_x
    })


  })

}

generate_partiton<-function(data,split_t,split_y,split_p,split_seed,part_type="Balanced", groups=5){
  if(part_type=="Random"){
    nobs<-nrow(data)
    ntest<-round(nobs*split_p/100)
    if(is.na(split_seed)){split_seed=NULL}
    set.seed(split_seed)
    part_vec<-sample(c(rep("Test",ntest),rep("Training",nobs-ntest)))
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
  part<-caret::createDataPartition(y,p=p,list=T, groups=groups)[[1]]
  train=rownames(data)[part]
  test=rownames(data)[-part]
  df<-data.frame(Partition=rep(NA,nrow(factors)),
                 row.names = rownames(factors))
  df[train,1]<-"training"
  df[test,1]<-"test"
  df$Partition<-as.factor(df$Partition)

  df

}



get_scale<-function(data,scale,center){
  data0<-data
  if(isTRUE(scale)){
    data<-data[which(vapply(data,function(x) var(x,na.rm=T)>0,logical(1)))]
    req(nrow(data)>0)
    req(ncol(data)>0)
    scaled<-base::scale(data,center,scale)
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
  vapply(data, function(column) {
    all(column==round(column),na.rm=T)
  },logical(1))
}


ui_base<-function(id){
  ns<-NS(id)
  div(

  )
}
server_base<-function(id,data=NULL, vals=NULL){
  moduleServer(id,function(input,output,session){

  })
}
create<-tool2<-pp_data<-tool3<-tool4<-tool5<-tool6<-tool7<-tool8<-tool9<-list()
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
done_modal<-function(...){
  showModal(
    modalDialog(
      easyClose = T,
      size="s",
      title=NULL,
      h4(emgreen("Success!")),
      div(...)
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
# The tool1$ui function defines the user interface for creating Datalists in iMESc.
# It includes input fields for required (Numeric-Attribute) and optional attributes
# (Factor-Attribute, Coords-Attribute, Base Shape, Layer Shape).
# Users can either upload their own data or choose example datasets.
# The interface provides tooltips and validations to ensure proper data formatting.
tool1$ui <- function(id) {

  ns <- NS(id)

  texttime <- function() {
    tags$div(
      class = "tip_large",
      tags$p(
        tags$strong("Temporal-Attribute"),
        "stores temporal information associated with the observations."
      ),
      tags$p(
        "It can contain dates, hours, date-time combinations, months, days, years, seasons, ",
        "or other temporal descriptors used for temporal filtering, plotting, modelling, ",
        "or time-based validation."
      ),
      tags$p(
        "Examples of valid columns include: ",
        tags$code("date"),
        ", ",
        tags$code("time"),
        ", ",
        tags$code("datetime"),
        ", ",
        tags$code("month"),
        ", ",
        tags$code("day"),
        ", ",
        tags$code("year"),
        "."
      )
    )
  }

  labels_create <- list(
    span(
      strong("Numeric-Attribute:", style = "color:  SeaGreen"),
      tiphelp(
        "csv or xlsx file where rows are the observations, columns are the variables  The first column must contain the observation labels. Columns containing characters are initially omitted and can later be included as binary columns by factor level.", "right")    ),

    span(
      style = "color:  #05668D",
      strong("Factor-Attribute:"),
      tiphelp_icon(
        actionLink(ns("labhelp"), icon("fas fa-question-circle")),
        "csv or xlsx file containg the factors for your data, which will be used for labeling, grouping and viewing the results. It can contain as many factors as you want.", "right"
      )
    ),

    span(
      style = "color:  #05668D",
      strong(span("*"), "Coords-Attribute:"),
      tiphelp_icon(
        actionLink(ns("cohelp"), icon("fas fa-question-circle")),
        "csv or xlsx file with the longitudes and latitudes of the observations. The first column must contain the name of the observations. The second and third columns must contain the longitude and latitude respectively", "right"
      )
    ),

    span(
      style = "color:  #05668D",
      strong("Temporal-Attribute:"),
      tiphelp_icon(
        actionLink(ns("timehelp"), icon("fas fa-question-circle")),
        texttime(), "right"
      )
    ),

    span(
      style = "color:  #05668D",
      strong(
        span("*"),
        "Base shape:",
        tiphelp(
          "<div class='tip_large'>
     <p><strong>Base shape</strong> is the main spatial polygon used as the geographic reference for map generation.</p>
     <p>It can be used to define the map extent, delimit the study area, filter points, and clip interpolated surfaces or other spatial outputs.</p>
     <p>The file must be an <code>.rds</code> object previously created in R, usually from a shapefile read with packages such as <code>sf</code>. The uploaded object should contain a polygon layer.</p>
     <p>Alternatively, users can import shapefiles and add them as base shape later using the <strong>SHP toolbox</strong>, available in the preprocessing tools.</p>
   </div>",
          "right"
        )
      )
    ),

    span(
      style = "color:  #05668D",
      strong(
        span("*"),
        "Layer shape:",
        tiphelp(
          "<p><strong>Layer shape</strong> is an additional spatial layer displayed on top of maps, such as land polygons, coastlines, islands, administrative boundaries, or other reference features.</p>
   <p>Unlike the <strong>Base shape</strong>, it is not used as the main geographic reference, study-area boundary, or clipping mask.</p>
   <p>Upload an <code>.rds</code> file containing a spatial vector object previously created in R, for example from a shapefile read with <code>sf</code>.</p>
   <p>Alternatively, users can import shapefiles and add them as layer shapes later using the <strong>SHP toolbox</strong>, available in the preprocessing tools.</p>",
          "right"
        )
      )
    )
  )

  tags$div(
    class = "dl_modal",

    modalDialog(

      div(

        div(
          style = "background: transparent; margin-left: 20px;padding-top: 10px",
          class = "half-drop dl",

          div(
            id = ns("dl_page1"),

            h4(
              "Create Datalist",
              tiphelp_icon(
                icon("fas fa-question-circle"),
                "Each Datalist in the Databank requires at least one file for the Numeric-Attribute. If a Factor-Attribute is not provided, iMESc automatically generates this attribute as a single-column sheet containing the IDs of the Numeric-Attribute."
              )
            ),

            div(style="display: flex;",
                div(
                  class = "insert_radio",
                  radioButtons(
                    ns("up_or_ex"),
                    NULL,
                    choiceValues = list("upload", "example"),
                    choiceNames = list(
                      tiphelp_icon(
                        strong("Upload"),
                        "upload your own data",
                        placement = "bottom"
                      ),
                      tiphelp_icon(
                        strong("Example"),
                        "Use one of the example datasets",
                        placement = "bottom"
                      )
                    ),
                    selected = "upload",
                    inline = TRUE,
                    width="200px"
                  )
                ),

                div(
                  id = ns("example_dataset_box"),
                  style = "display: none; margin-top: -35px; position: relative;",
                  pickerInput(
                    ns("example_dataset"),
                    "Example dataset",
                    choices = c(
                      "Araca Bay nematodes and abiotic data" = "araca",
                      "FZA satellite temporal data" = "fza"
                    ),
                    selected = "araca"
                  ),
                  actionLink(
                    ns("example_data_help"),
                    icon("fas fa-question-circle"),
                    class = "example-data-help-btn"
                  )
                )
            ),

            div(
              class = "insert_radio_border_bottom",
              style = "margin-bottom: 10px"
            ),

            div(
              id = ns("dl_create"),

              div(
                class = "dl_page",

                div(
                  class = "create_req create_dl to_shake",
                  style = "display: flex",
                  id = ns("dl_name"),

                  div(
                    div(class = "mlb-wide")
                  ),

                  textInput(
                    ns("data_name"),
                    span("Name the Datalist", style = "color: SeaGreen"),
                    value = NULL,
                    width = "320px"
                  ),

                  uiOutput(ns("war_dlname"))
                ),

                div(
                  class = "create_dl",

                  div(
                    class = "create_req",

                    div(
                      style = "display: flex;",

                      div(
                        class = "mlb mlb-wide",
                        div(
                          style = "position: absolute; left: 0px;
                color:SeaGreen; margin-top: -30px; margin-left: 30px",
                          strong("Required")
                        )
                      ),

                      div(class = "mlb"),

                      fileInput(
                        ns("filedata"),
                        label = labels_create[1],
                        accept = c(".csv", ".xlsx", ".xls")
                      ),

                      div(
                        class = "sheet_dl",
                        hidden(
                          pickerInput(
                            ns("sheet_data"),
                            "Sheet:",
                            choices = NULL
                          )
                        )
                      ),

                      actionLink(
                        ns("reset_insert_1"),
                        icon("fas fa-undo"),
                        class = "btn-input"
                      )
                    )
                  ),

                  div(
                    class = "create_opt",
                    style = "padding-top: 10px",

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide mblue"),

                      div(
                        style = "position: absolute; left: 0px;
                color:#05668D; margin-top: -55px; margin-left: 30px",
                        strong("Optional")
                      ),

                      div(class = "mlb mblue"),

                      fileInput(
                        ns("labels"),
                        labels_create[2],
                        accept = c(".csv", ".xlsx", ".xls")
                      ),

                      div(
                        class = "sheet_dl",
                        hidden(
                          pickerInput(
                            ns("sheet_fac"),
                            "Sheet:",
                            choices = NULL
                          )
                        )
                      ),

                      actionLink(
                        ns("reset_insert_2"),
                        icon("fas fa-undo"),
                        class = "btn-input"
                      )
                    ),

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide"),

                      div(
                        style = "position: absolute; left: 0px;width: 120px;
                color:gray; margin-top: -25px; margin-left: 30px;white-space: normal",
                        em("*Required for the spatial tools menu")
                      ),

                      div(class = "mlb mblue"),

                      fileInput(
                        ns("coords"),
                        labels_create[3],
                        accept = c(".csv", ".xlsx", ".xls")
                      ),

                      div(
                        class = "sheet_dl",
                        hidden(
                          pickerInput(
                            ns("sheet_coord"),
                            "Sheet:",
                            choices = NULL
                          )
                        )
                      ),

                      actionLink(
                        ns("reset_insert_3"),
                        icon("fas fa-undo"),
                        class = "btn-input"
                      )
                    ),

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide"),

                      div(class = "mlb mblue"),

                      fileInput(
                        ns("time_attr"),
                        labels_create[4],
                        accept = c(".csv", ".xlsx", ".xls")
                      ),

                      div(
                        class = "sheet_dl",
                        hidden(
                          pickerInput(
                            ns("sheet_time"),
                            "Sheet:",
                            choices = NULL
                          )
                        )
                      ),

                      actionLink(
                        ns("reset_insert_6"),
                        icon("fas fa-undo"),
                        class = "btn-input"
                      )
                    ),

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide"),

                      div(class = "mlb mblue"),

                      fileInput(
                        ns("base_shape"),
                        labels_create[5]
                      ),

                      actionLink(
                        ns("reset_insert_4"),
                        icon("fas fa-undo"),
                        class = "btn-input"
                      )
                    ),

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide"),

                      div(class = "mlb mblue"),

                      fileInput(
                        ns("layer_shape"),
                        labels_create[6]
                      ),

                      actionLink(
                        ns("reset_insert_5"),
                        icon("fas fa-undo"),
                        class = "btn-input"
                      )
                    )
                  )
                )
              )
            ),

            div(
              id = ns("dl_example"),
              style = "display: none; margin-top: -5px",

              div(
                class = "dl_page",

                div(
                  class = "create_req create_dl",
                  style = "display: flex",

                  div(
                    div(class = "mlb-wide")
                  ),

                  div(
                    class = "form-group shiny-input-container",
                    tags$label("Name the Datalist"),
                    uiOutput(ns("example_datalist_label"))
                  )
                ),

                div(
                  class = "create_dl",

                  div(
                    class = "create_req",

                    div(
                      style = "display: flex;",

                      div(
                        class = "mlb mlb-wide",
                        div(
                          style = "position: absolute; left: 0px;color:SeaGreen; margin-top: -30px; margin-left: 30px",
                          strong("Required")
                        )
                      ),

                      div(class = "mlb"),

                      div(
                        class = "form-group shiny-input-container",
                        tags$label("Numeric-Attribute"),
                        uiOutput(ns("example_numeric_label"))
                      )
                    )
                  ),

                  div(
                    class = "create_opt",
                    style = "padding-top: 10px",

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide mblue"),

                      div(
                        style = "position: absolute; left: 0px;
                color:#05668D; margin-top: -55px; margin-left: 30px",
                        strong("Optional")
                      ),

                      div(class = "mlb mblue"),

                      div(
                        class = "form-group shiny-input-container",
                        tags$label("Factor-Attribute"),
                        uiOutput(ns("example_factor_label"))
                      )
                    ),

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide"),

                      div(
                        style = "position: absolute; left: 0px;width: 120px;color:gray; margin-top: -25px; margin-left: 30px;white-space: normal",
                        em("*Required for the spatial tools menu")
                      ),

                      div(class = "mlb mblue"),

                      div(
                        class = "form-group shiny-input-container",
                        tags$label("Coords-Attribute"),
                        uiOutput(ns("example_coords_label"))
                      )
                    ),

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide"),

                      div(class = "mlb mblue"),

                      div(
                        class = "form-group shiny-input-container",
                        tags$label("Temporal-Attribute"),
                        uiOutput(ns("example_time_label"))
                      )
                    ),



                    div(
                      style = "display: flex",

                      div(class = "mlb-wide"),

                      div(class = "mlb mblue"),

                      div(
                        class = "form-group shiny-input-container",
                        tags$label("Base shape"),
                        uiOutput(ns("example_base_shape_label"))
                      )
                    ),

                    div(
                      style = "display: flex",

                      div(class = "mlb-wide"),

                      div(class = "mlb mblue"),

                      div(
                        class = "form-group shiny-input-container",
                        tags$label("Layer shape"),
                        uiOutput(ns("example_layer_shape_label"))
                      )
                    )
                  )
                )
              )
            )
          ),

          div(
            id = ns("dl_page_time"),
            style = "display: none; height: 400px; overflow-y: scroll",
            uiOutput(ns("insert_time_page"))
          ),

          div(
            id = ns("dl_page2"),
            style = "display: none",
            uiOutput(ns("insert_page2"))
          ),

          div(
            id = ns("dl_page3"),
            style = "display: none",
            "Page3"
          ),

          hidden(
            div(
              id = ns("example_data_help_panel"),
              class = "example-data-help-panel",

              div(
                class = "example-data-help-card",

                div(
                  class = "example-data-help-header",
                  h4(uiOutput(ns("example_data_help_title"))),
                  actionLink(
                    ns("example_data_help_close"),
                    icon("fas fa-times"),
                    class = "example-data-help-close"
                  )
                ),

                div(
                  class = "example-data-help-body",
                  uiOutput(ns("example_data_help_content"))
                )
              )
            )
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

      .time_format_card {
        border: 1px solid #ddd;
        border-radius: 6px;
        padding: 10px;
        margin-bottom: 10px;
        background: #fafafa;
      }

      .time_format_preview {
        color: gray;
        margin-bottom: 8px;
        font-size: 90%;
      }

      .example-data-help-btn {
        position: absolute;
        right: -26px;
        top: 25px;
        color: #2f8bc1;
        font-size: 18px;
        line-height: 18px;
      }

      .example-data-help-panel {
        position: absolute;
        inset: 0;
        background: rgba(255, 255, 255, 0.82);
        z-index: 20;
      }

      .example-data-help-card {
        position: absolute;
        top: 55px;
        left: 80px;
        width: 640px;
        max-height: 390px;
        overflow-y: auto;
        background: white;
        border: 1px solid #d8d8d8;
        box-shadow: 0 2px 10px rgba(0,0,0,.22);
        padding: 14px 18px;
      }

      .example-data-help-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        border-bottom: 1px solid #e6e6e6;
        margin-bottom: 12px;
      }

      .example-data-help-header h4 {
        margin: 0 0 8px 0;
        color: #05668D;
      }

      .example-data-help-close {
        color: #555;
        font-size: 16px;
        margin-bottom: 8px;
      }

      .example-data-help-body {
        color: #555;
        font-size: 13px;
      }

      .example-data-help-body p {
        margin-bottom: 8px;
      }

      .example-data-help-body ul {
        padding-left: 18px;
        margin-bottom: 10px;
      }

      .example-data-help-body table {
        width: 100%;
        margin-top: 8px;
        font-size: 12px;
      }

      .example-data-help-body th {
        color: #05668D;
        border-bottom: 1px solid #ddd;
        padding: 4px;
      }

      .example-data-help-body td {
        border-bottom: 1px solid #eee;
        padding: 4px;
        vertical-align: top;
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
        )
      ),

      footer = div(
        div(
          class = "dl_btns",
          style = "display: inline-block",

          actionButton(ns("dl_cancel"), "Cancel"),

          hidden(actionButton(ns("dl_prev"), "< Previous")),

          inline(uiOutput(ns("insert_buttons")))
        )
      ),

      easyClose = TRUE
    )
  )
}
# The tool1$server function handles the server-side logic for managing Datalist creation.
# It processes user uploads, validates inputs, and dynamically generates Datalists.
# The server also ensures that mandatory fields (e.g., Datalist name) are completed before proceeding
# and supports resetting inputs, navigating through modal pages, and saving the Datalist to the app's storage.
tool1$server <- function(id, vals) {

  moduleServer(id, function(input, output, session) {

    curpage <- reactiveVal("page1")

    name_dl <- reactiveVal()

    file_data <- reactiveVal()
    file_coords <- reactiveVal()
    file_time <- reactiveVal()
    file_factors <- reactiveVal()
    file_base <- reactiveVal()
    file_layer <- reactiveVal()

    getdatalist <- reactiveVal()

    example_datasets <- list(
      araca = list(
        name = "nema_araca",
        paired_name = "envi_araca",
        datalist_label = "nema_araca/envi_araca",
        numeric_label = "nematode/abiotic data from Araca Bay, Brazil",
        factor_label = "sampling factors for both Datalists",
        coords_label = "sampling coordinates for both Datalists",
        time_label = "no temporal attribute",
        base_shape_label = "base shape of the Araca Bay, for both Datalists",
        layer_shape_label = "layer shape of the Araca Bay, for both Datalists",
        numeric_path = "inst/www/nema_araca.csv",
        paired_numeric_path = "inst/www/envi_araca.csv",
        factor_path = "inst/www/factors_araca.csv",
        coords_path = "inst/www/coords_araca.csv",
        time_path = NULL,
        base_shape_path = "inst/www/base_shape_araca.rds",
        layer_shape_path = "inst/www/layer_shape_araca.rds"
      ),
      fza = list(
        name = "drivers_fza",
        paired_name = "chl_fza",
        datalist_label = "drivers_fza/chl_fza",
        numeric_label = "environmental drivers and chlorophyll-a (CHL) from Foz do Amazonas, Brazil",
        factor_label = "automatically generated from Numeric-Attribute IDs, for both Datalists",
        coords_label = "satellite pixel coordinates from Foz do Amazonas, for both Datalists",
        time_label = "temporal attribute for the satellite time series, for both Datalists",
        base_shape_label = "base shape of Foz do Amazonas, for both Datalists",
        layer_shape_label = "layer shape of Foz do Amazonas, for both Datalists",
        numeric_path = "inst/www/drivers_fza.csv",
        paired_numeric_path = "inst/www/chl_fza.csv",
        factor_path = NULL,
        coords_path = "inst/www/fza_coords.csv",
        time_path = "inst/www/fza_time.csv",
        base_shape_path = "inst/www/fza_base_shape.rds",
        layer_shape_path = "inst/www/fza_layer_shape.rds"
      )
    )

    selected_example_key <- reactive({
      key <- input$example_dataset

      if (is.null(key) || !key %in% names(example_datasets)) {
        return("araca")
      }

      key
    })

    selected_example <- reactive({
      example_datasets[[selected_example_key()]]
    })

    example_label <- function(field, color = "#05668D") {
      force(field)
      force(color)

      renderUI({
        div(
          class = "form-control fake_dl",
          selected_example()[[field]],
          style = paste0("width: 320px; ;padding: 7px; color: ", color)
        )
      })
    }

    output$example_datalist_label <- example_label("datalist_label", "SeaGreen")
    output$example_numeric_label <- example_label("numeric_label", "SeaGreen")
    output$example_factor_label <- example_label("factor_label")
    output$example_coords_label <- example_label("coords_label")
    output$example_time_label <- example_label("time_label")
    output$example_base_shape_label <- example_label("base_shape_label")
    output$example_layer_shape_label <- example_label("layer_shape_label")

    output$example_data_help_title <- renderUI({
      if (selected_example_key() == "fza") {
        return("FZA satellite temporal data")
      }

      "Araca Bay example data"
    })

    output$example_data_help_content <- renderUI({
      if (selected_example_key() == "fza") {
        return(
          div(
            p(
              strong("FZA satellite temporal data"),
              " is an example from the Foz do Amazonas region, on the northern Brazilian continental margin. ",
              "It was prepared from a NetCDF file containing monthly gridded oceanographic, biogeochemical, optical, wind, and sea-surface temperature variables. ",
              "The original data were provided on a regular longitude-latitude grid."
            ),
            p("This example inserts two Datalists, ready to be used as Y and X in Supervised Algorithms:"),
            tags$ul(
              tags$li(strong("drivers_fza: "), "16 oceanographic, biogeochemical, wind, temperature and light variables that drive chlorophyll-a (the predictors, X)."),
              tags$li(strong("chl_fza: "), "chlorophyll-a concentration (CHL), the response variable (Y).")
            ),
            tags$ul(
              tags$li(strong("Structure: "), "40 spatial points observed through 60 monthly time steps, totaling 2,400 space-time observations."),
              tags$li(strong("Spatial sampling: "), "the 40 points correspond to grid cells with complete data across all variables and months, selected using a spatially distributed sampling procedure."),
              tags$li(strong("Spatial resolution: "), "each sampled point represents the center of one original grid cell and inherits the original grid resolution of 0.0416667 degrees, approximately 4.6 km."),
              tags$li(strong("Temporal coverage: "), "monthly data from 2021-01-01 to 2025-12-31."),
              tags$li(strong("Attributes included: "), "Numeric, Factor, Coords-Attribute, Temporal-Attribute, Base-Shape, and Layer-Shape, in both Datalists."),
              tags$li(strong("Coordinates: "), "longitude and latitude in decimal degrees."),
              tags$li(strong("Temporal attribute: "), "a date column named ", tags$code("Date"), ".")
            ),

            tags$table(
              tags$thead(
                tags$tr(
                  tags$th("Variable"),
                  tags$th("Description"),
                  tags$th("Units")
                )
              ),
              tags$tbody(
                tags$tr(tags$td(tags$code("CHL")), tags$td("Chlorophyll-a concentration"), tags$td("mg m-3")),
                tags$tr(tags$td(tags$code("mlotst")), tags$td("Density ocean mixed layer thickness"), tags$td("m")),
                tags$tr(tags$td(tags$code("fe")), tags$td("Dissolved iron"), tags$td("mmol m-3")),
                tags$tr(tags$td(tags$code("no3")), tags$td("Nitrate"), tags$td("mmol m-3")),
                tags$tr(tags$td(tags$code("po4")), tags$td("Phosphate"), tags$td("mmol m-3")),
                tags$tr(tags$td(tags$code("si")), tags$td("Dissolved silicate"), tags$td("mmol m-3")),
                tags$tr(tags$td(tags$code("so")), tags$td("Salinity"), tags$td("PSU")),
                tags$tr(tags$td(tags$code("uo")), tags$td("Eastward sea-water velocity"), tags$td("m s-1")),
                tags$tr(tags$td(tags$code("vo")), tags$td("Northward sea-water velocity"), tags$td("m s-1")),
                tags$tr(tags$td(tags$code("rotcor")), tags$td("Current vorticity"), tags$td("10^-5 s-1")),
                tags$tr(tags$td(tags$code("divcor")), tags$td("Current divergence"), tags$td("10^-5 s-1")),
                tags$tr(tags$td(tags$code("u")), tags$td("10 m U wind component"), tags$td("m s-1")),
                tags$tr(tags$td(tags$code("v")), tags$td("10 m V wind component"), tags$td("m s-1")),
                tags$tr(tags$td(tags$code("rotwind")), tags$td("Wind vorticity"), tags$td("10^-5 s-1")),
                tags$tr(tags$td(tags$code("divwind")), tags$td("Wind divergence"), tags$td("10^-5 s-1")),
                tags$tr(tags$td(tags$code("analysed_sst")), tags$td("Analysed sea surface temperature"), tags$td(HTML("&deg;C"))),
                tags$tr(tags$td(tags$code("PAR_mean")), tags$td("Photosynthetically available radiation"), tags$td("einstein m-2 day-1"))
              )
            )
          )
        )
      }

      div(
        p(
          "The Araca Bay examples include two Datalists from Araca Bay, located on the southeastern coast of Brazil."
        ),
        tags$ul(
          tags$li(strong("envi_araca: "), "contains 141 samples with 9 environmental variables."),
          tags$li(strong("nema_araca: "), "contains 141 samples with 194 free-living marine nematode species.")
        ),
        p(
          "Both Datalists comprise five attributes: Numeric, Factor, Coords-Attribute, Base-Shape, and Layer-Shape."
        ),
        p(
          "Studies that explored these data include Corte et al., 2017, Checon et al., 2018, and Vieira et al., 2021."
        ),
        h5(strong("References")),
        tags$ul(
          tags$li(
            "Checon, H. H., Vieira, D. C., Corte, G. N., Sousa, E. C. P. M., Fonseca, G., & Amaral, A. C. Z. (2018). ",
            "Defining soft bottom habitats and potential indicator species as tools for monitoring coastal systems: A case study in a subtropical bay. ",
            em("Ocean & Coastal Management"),
            ", 164, 68-78. ",
            tags$a(href = "https://doi.org/10.1016/j.ocecoaman.2018.03.035", target = "_blank", "https://doi.org/10.1016/j.ocecoaman.2018.03.035")
          ),
          tags$li(
            "Corte, G. N., Checon, H. H., Fonseca, G., Vieira, D. C., Gallucci, F., Domenico, M. Di, & Amaral, A. C. Z. (2017). ",
            "Cross-taxon congruence in benthic communities: Searching for surrogates in marine sediments. ",
            em("Ecological Indicators"),
            ", 78, 173-182. ",
            tags$a(href = "https://doi.org/10.1016/j.ecolind.2017.03.031", target = "_blank", "https://doi.org/10.1016/j.ecolind.2017.03.031")
          ),
          tags$li(
            "Vieira, D. C., Gallucci, F., Corte, G. N., Checon, H. H., Zacagnini Amaral, A. C., & Fonseca, G. (2021). ",
            "The relative contribution of non-selection and selection processes in marine benthic assemblages. ",
            em("Marine Environmental Research"),
            ", 163, 105223. ",
            tags$a(href = "https://doi.org/10.1016/j.marenvres.2020.105223", target = "_blank", "https://doi.org/10.1016/j.marenvres.2020.105223")
          )
        )
      )
    })

    observeEvent(input$example_data_help, {
      shinyjs::show("example_data_help_panel")
    }, ignoreInit = TRUE)

    observeEvent(input$example_data_help_close, {
      shinyjs::hide("example_data_help_panel")
    }, ignoreInit = TRUE)

    reset_create_state <- function(reset_inputs = TRUE) {
      curpage("page1")
      name_dl(NULL)
      file_data(NULL)
      file_coords(NULL)
      file_time(NULL)
      file_factors(NULL)
      file_base(NULL)
      file_layer(NULL)
      getdatalist(NULL)
      shinyjs::hide("example_data_help_panel")

      if (isTRUE(reset_inputs)) {
        shinyjs::reset("filedata")
        shinyjs::reset("labels")
        shinyjs::reset("coords")
        shinyjs::reset("time_attr")
        shinyjs::reset("base_shape")
        shinyjs::reset("layer_shape")
        shinyjs::hide("sheet_data")
        shinyjs::hide("sheet_fac")
        shinyjs::hide("sheet_coord")
        shinyjs::hide("sheet_time")
      }

      NULL
    }

    observeEvent(vals$reset_create_datalist, {
      reset_create_state()
    }, ignoreInit = TRUE)

    observeEvent(input$data_name, {
      req(input$data_name != "")
      shinyjs::removeClass(id = "dl_name", class = "alert_warning")
    })

    observe({
      shinyjs::toggle("example_dataset_box", condition = input$up_or_ex == "example")
      if (!identical(input$up_or_ex, "example")) {
        shinyjs::hide("example_data_help_panel")
      }
    })

    observe({
      req(input$filedata$datapath)
      req(input$up_or_ex != "example")

      shinyjs::toggleClass(
        id = "dl_name",
        class = "alert_warning",
        condition = input$data_name == ""
      )
    })

    observeEvent(input$dl_next, ignoreInit = TRUE, {
      req(input$filedata$datapath)
      req(input$up_or_ex != "example")
      req(input$data_name == "")

      session$sendCustomMessage(
        type = "shakeClass",
        message = list(className = "to_shake")
      )

      output$war_dlname <- renderUI({
        req(input$data_name == "")

        div(
          align = "left",
          class = "alert_warning",
          style = "padding-left: 20px;",
          div(
            strong(
              icon("triangle-exclamation", style = "color: Dark yellow3"),
              "Warning:"
            )
          ),
          div("Datalist name cannot be empty")
        )
      })
    })

    output$insert_buttons <- renderUI({
      validate_data()
      ns <- session$ns

      div(
        actionButton(ns("dl_next"), "Next >"),
        hidden(actionButton(ns("dl_insert"), strong("Insert Datalist >")))
      )
    })

    observeEvent(name_dl(), {
      updateTextInput(session, "data_name", value = name_dl())
    })

    observeEvent(input$filedata$datapath, {
      file_data(input$filedata$datapath)
      name_dl(gsub("\\.csv|\\.xls|\\.xlsx", "", input$filedata$name))
    })

    observeEvent(input$coords$datapath, {
      file_coords(input$coords$datapath)
    })

    observeEvent(input$time_attr$datapath, {
      file_time(input$time_attr$datapath)
    })

    observeEvent(input$labels$datapath, {
      file_factors(input$labels$datapath)
    })

    observeEvent(input$base_shape$datapath, {
      file_base(input$base_shape$datapath)
    })

    observeEvent(input$layer_shape$datapath, {
      file_layer(input$layer_shape$datapath)
    })

    observeEvent(input$reset_insert_1, {
      file_data(NULL)
      name_dl(NULL)
      shinyjs::reset("filedata")
      shinyjs::hide("sheet_data")
    })

    observeEvent(input$reset_insert_2, {
      file_factors(NULL)
      shinyjs::reset("labels")
      shinyjs::hide("sheet_fac")
    })

    observeEvent(input$reset_insert_3, {
      file_coords(NULL)
      shinyjs::reset("coords")
      shinyjs::hide("sheet_coord")
    })

    observeEvent(input$reset_insert_6, {
      file_time(NULL)
      shinyjs::reset("time_attr")
      shinyjs::hide("sheet_time")
    })

    observeEvent(input$reset_insert_4, {
      file_base(NULL)
      shinyjs::reset("base_shape")
    })

    observeEvent(input$reset_insert_5, {
      file_layer(NULL)
      shinyjs::reset("layer_shape")
    })

    observe({
      req(file_data())
      condition <- grepl("xls", file_data()) > 0
      shinyjs::toggle("sheet_data", condition = condition)
    }, suspended = TRUE)

    observe({
      req(file_factors())
      condition <- grepl("xls", file_factors()) > 0
      shinyjs::toggle("sheet_fac", condition = condition)
    }, suspended = TRUE)

    observe({
      req(file_coords())
      condition <- grepl("xls", file_coords()) > 0
      shinyjs::toggle("sheet_coord", condition = condition)
    }, suspended = TRUE)

    observe({
      req(file_time())
      condition <- grepl("xls", file_time()) > 0
      shinyjs::toggle("sheet_time", condition = condition)
    }, suspended = TRUE)

    observeEvent(input$filedata, {
      req(grepl("xls", file_data()))

      choices_data <- readxl::excel_sheets(path = file_data())

      updatePickerInput(
        session,
        "sheet_data",
        choices = choices_data
      )
    })

    observeEvent(input$labels, {
      req(grepl("xls", file_factors()))

      choices_fac <- readxl::excel_sheets(path = file_factors())

      updatePickerInput(
        session,
        "sheet_fac",
        choices = choices_fac
      )
    })

    observeEvent(input$coords, {
      req(grepl("xls", file_coords()))

      choices_coord <- readxl::excel_sheets(path = file_coords())

      updatePickerInput(
        session,
        "sheet_coord",
        choices = choices_coord
      )
    })

    observeEvent(input$time_attr, {
      req(grepl("xls", file_time()))

      choices_time <- readxl::excel_sheets(path = file_time())

      updatePickerInput(
        session,
        "sheet_time",
        choices = choices_time
      )
    })

    observeEvent(input$up_or_ex, {
      shinyjs::toggle("dl_create", condition = input$up_or_ex == "upload")
      shinyjs::toggle("dl_example", condition = input$up_or_ex == "example")
    })

    output$validate_data <- renderUI({
      validate_data()
      NULL
    })

    observeEvent(curpage(), {

      shinyjs::toggle("dl_page1", condition = curpage() == "page1")
      shinyjs::toggle("dl_page_time", condition = curpage() == "time")
      shinyjs::toggle("dl_page2", condition = curpage() == "page2")
      shinyjs::toggle("dl_page3", condition = curpage() == "page3")

      shinyjs::toggle("dl_next", condition = curpage() %in% c("page1", "time"))
      shinyjs::toggle("dl_prev", condition = curpage() %in% c("time", "page2", "page3"))
      shinyjs::toggle("dl_cancel", condition = curpage() == "page1")
      shinyjs::toggle("dl_insert", condition = curpage() == "page2")
    })

    observeEvent(input$dl_cancel, ignoreInit = TRUE, {
      reset_create_state()
      removeModal()
    })

    validate_data <- reactive({
      req(input$up_or_ex)

      if (input$up_or_ex == "example") {
        return(TRUE)
      }

      path <- file_data()

      req(path)

      if (grepl(".csv", path)) {

        data <- data.frame(
          data.table::fread(
            path,
            stringsAsFactors = TRUE,
            na.strings = c("", "NA"),
            header = TRUE,
            select = 1
          )
        )

        validate(
          need(
            !any(duplicated(data[, 1])),
            "Duplicate values not allowed in the first column (as they will be used as observation IDs)."
          )
        )

      } else {

        sheet <- input$sheet_data

        if (is.null(sheet)) {
          sheets <- readxl::excel_sheets(path = path)
          sheet <- sheets[1]
        }

        df <- data.frame(
          readxl::read_excel(
            path,
            sheet,
            na = c("", "NA"),
            col_names = FALSE,
            range = readxl::cell_cols(1)
          )
        )

        validate(
          need(
            !any(duplicated(df[, 1])),
            "Duplicate values not allowed in the first column (as they will be used as observation IDs)."
          )
        )
      }

      TRUE
    })

    dataraw <- reactive({
      path <- selected_example()[["numeric_path"]]

      if (input$up_or_ex == "upload") {
        path <- file_data()
      }

      if (length(path) > 0) {
        imesc_data(path, input$sheet_data, "Numeric")
      }
    })

    read_labels <- reactive({
      datao <- dataraw()

      path <- selected_example()[["factor_path"]]

      if (input$up_or_ex == "upload") {
        path <- file_factors()
      }

      if (length(path) > 0) {
        imesc_data(path, input$sheet_fac, "Factors")[rownames(datao), , drop = FALSE]
      }
    })

    read_coords <- reactive({
      datao <- dataraw()

      path <- selected_example()[["coords_path"]]

      if (input$up_or_ex == "upload") {
        path <- file_coords()
      }

      if (length(path) > 0) {
        imesc_data(path, input$sheet_coord, "Coords")[rownames(datao), , drop = FALSE]
      }
    })

    read_time <- reactive({
      datao <- dataraw()

      path <- selected_example()[["time_path"]]

      if (input$up_or_ex == "upload") {
        path <- file_time()
      }

      if (length(path) > 0) {
        imesc_data(path, input$sheet_time, "Time")[rownames(datao), , drop = FALSE]
      }
    })

    read_base <- reactive({
      if (input$up_or_ex == "example") {

        readRDS(selected_example()[["base_shape_path"]])

      } else {

        if (length(file_base()) > 0) {

          t <- try({
            get(
              gsub(
                " ",
                "",
                capture.output(
                  load(file_base(), verbose = TRUE)
                )[2]
              )
            )
          })

          if ("try-error" %in% class(t)) {
            t <- readRDS(file_base())
          }

          t

        } else {

          NULL

        }
      }
    })

    read_layer <- reactive({
      if (input$up_or_ex == "example") {

        readRDS(selected_example()[["layer_shape_path"]])

      } else {

        if (length(file_layer()) > 0) {

          t <- try({
            get(
              gsub(
                " ",
                "",
                capture.output(
                  load(file_layer(), verbose = TRUE)
                )[2]
              )
            )
          })

          if ("try-error" %in% class(t)) {
            t <- readRDS(file_layer())
          }

          t

        } else {

          NULL

        }
      }
    })

    create_current_datalist <- reactive({

      d1 <- create_DATALIST(
        input$up_or_ex,
        data = dataraw(),
        factors = read_labels(),
        coords = read_coords(),
        time = read_time(),
        base_shape = read_base(),
        layer_shape = read_layer(),
        name_example = selected_example()[["name"]],
        data_name = input$data_name
      )

      if (input$up_or_ex == "example") {
        attr(d1[[1]], "datalist") <- selected_example()[["name"]]
        attr(d1[[1]], "filename") <- basename(selected_example()[["numeric_path"]])
      }

      d1
    })

    # examples with a paired Datalist (e.g. nema/envi Araca, chl/envi FZA) insert two
    # Datalists; the paired one receives the attributes of the first (data_migrate)
    has_paired_example <- reactive({
      identical(input$up_or_ex, "example") && !is.null(selected_example()[["paired_numeric_path"]])
    })

    getdatalist_envi <- reactive({

      req(has_paired_example())

      path <- selected_example()[["paired_numeric_path"]]

      data <- imesc_data(path, input$sheet_data, "Numeric")

      d1 <- create_DATALIST(
        up_or_ex = input$up_or_ex,
        data,
        factors = read_labels(),
        coords = read_coords(),
        time = read_time(),
        base_shape = read_base(),
        layer_shape = read_layer(),
        name_example = selected_example()[["paired_name"]],
        data_name = input$data_name
      )

      attr(d1[[1]], "datalist") <- selected_example()[["paired_name"]]
      attr(d1[[1]], "filename") <- basename(selected_example()[["paired_numeric_path"]])

      d1
    })

    time_page_needed <- reactive({
      d1 <- getdatalist()

      if (is.null(d1)) {
        return(FALSE)
      }

      time <- attr(d1[[1]], "time")

      !is.null(time) && ncol(time) > 0
    })

    time_preview_value <- function(x, n = 4) {
      x <- x[!is.na(x)]
      x <- head(x, n)

      if (length(x) == 0) {
        return("NA")
      }

      paste(as.character(x), collapse = ", ")
    }

    formatted_time <- reactive({

      d1 <- getdatalist()
      req(d1)

      time <- attr(d1[[1]], "time")

      if (is.null(time) || ncol(time) == 0) {
        return(NULL)
      }

      time_out <- time

      for (i in seq_len(ncol(time_out))) {

        type_i <- input[[paste0("time_type_", i)]]
        format_i <- input[[paste0("time_format_", i)]]
        custom_i <- input[[paste0("time_custom_", i)]]

        time_out[[i]] <- convert_time_column(
          x = time_out[[i]],
          type = type_i,
          format = format_i,
          custom_format = custom_i
        )
      }

      time_out
    })

    formatted_datalist <- reactive({

      d1 <- getdatalist()
      req(d1)

      d1_out <- d1

      if (time_page_needed()) {
        attr(d1_out[[1]], "time") <- formatted_time()
      }

      d1_out
    })

    output$insert_time_page <- renderUI({

      d1 <- getdatalist()
      req(d1)

      time <- attr(d1[[1]], "time")

      req(!is.null(time))
      req(ncol(time) > 0)

      div(
        style = "width: 95%; padding: 10px;",

        h4(
          "Format Temporal-Attribute",
          tiphelp_icon(
            icon("fas fa-question-circle"),
            "Check each temporal column and define how iMESc should interpret it. This avoids ambiguous date formats such as day/month/year versus month/day/year."
          )
        ),

        p(
          style = "color: gray;",
          "The Temporal-Attribute was detected. Please define the temporal type and format for each column before inserting the Datalist."
        ),

        lapply(seq_len(ncol(time)), function(i) {

          col_name <- colnames(time)[i]

          guess <- guess_time_settings(time[[i]])
          selected_type <- guess$type

          div(
            class = "time_format_card",

            h5(
              strong("Column: "),
              span(col_name, style = "color: #05668D;")
            ),

            div(
              style = "
      background: white;
      border: 1px dashed #ccc;
      padding: 8px;
      margin-bottom: 10px;
      border-radius: 4px;
    ",

              div(
                strong("First value as imported: "),
                code(as.character(time[[i]][which(!is.na(time[[i]]))[1]]))
              ),

              div(
                strong("Current class: "),
                code(paste(class(time[[i]]), collapse = "/"))
              ),

              div(
                strong("Preview values: "),
                em(time_preview_value(time[[i]]))
              )
            ),

            div(
              style = "display: flex; gap: 10px; align-items: flex-end; flex-wrap: wrap;",

              pickerInput(
                session$ns(paste0("time_type_", i)),
                "Temporal type:",
                choices = c(
                  "Keep as imported" = "keep",
                  "Date" = "date",
                  "Date-time" = "datetime",
                  "Time only" = "time",
                  "Year" = "year",
                  "Month" = "month",
                  "Day" = "day"
                ),
                selected = selected_type,
                width = "180px"
              ),

              pickerInput(
                session$ns(paste0("time_format_", i)),
                "Format:",
                choices = c(
                  "Auto / already formatted" = "auto",
                  "YYYY-MM-DD" = "%Y-%m-%d",
                  "DD/MM/YYYY" = "%d/%m/%Y",
                  "DD-MM-YYYY" = "%d-%m-%Y",
                  "MM/DD/YYYY" = "%m/%d/%Y",
                  "YYYY/MM/DD" = "%Y/%m/%d",
                  "YYYY-MM-DD HH:MM:SS" = "%Y-%m-%d %H:%M:%S",
                  "DD/MM/YYYY HH:MM:SS" = "%d/%m/%Y %H:%M:%S",
                  "DD-MM-YYYY HH:MM:SS" = "%d-%m-%Y %H:%M:%S",
                  "YYYY/MM/DD HH:MM:SS" = "%Y/%m/%d %H:%M:%S",
                  "HH:MM:SS" = "%H:%M:%S",
                  "HH:MM" = "%H:%M",
                  "Custom" = "custom"
                ),
                selected = guess$format,
                width = "240px"
              ),

              conditionalPanel(
                condition = paste0("input.time_format_", i, " == 'custom'"),
                ns = session$ns,
                textInput(
                  session$ns(paste0("time_custom_", i)),
                  "Custom format:",
                  value = guess$custom,
                  placeholder = "e.g. %d/%m/%Y",
                  width = "180px"
                )
              )
            )
          )
        }),

        uiOutput(session$ns("time_conversion_warning"))
      )
    })

    output$time_conversion_warning <- renderUI({

      d1 <- getdatalist()
      req(d1)

      time <- attr(d1[[1]], "time")

      req(!is.null(time))
      req(ncol(time) > 0)

      time_out <- formatted_time()

      warnings <- list()

      for (i in seq_len(ncol(time))) {

        type_i <- effective_time_type(
          input[[paste0("time_type_", i)]],
          input[[paste0("time_format_", i)]],
          input[[paste0("time_custom_", i)]]
        )

        if (is.null(type_i) || type_i == "keep") {
          next
        }

        original_non_na <- sum(!is.na(time[[i]]))
        converted_non_na <- sum(!is.na(time_out[[i]]))

        if (original_non_na > 0 && converted_non_na < original_non_na) {
          warnings[[length(warnings) + 1]] <- paste0(
            "Column '",
            colnames(time)[i],
            "' may have been converted incorrectly. ",
            original_non_na - converted_non_na,
            " value(s) became NA. Please check the selected format."
          )
        }

      }

      if (length(warnings) == 0) {
        return(
          div(
            style = "color: #2e7d32; margin-top: 10px;",
            icon("check-circle"),
            " Time settings look valid."
          )
        )
      }

      div(
        class = "alert_warning",
        style = "padding: 10px; margin-top: 10px;",
        div(strong(icon("triangle-exclamation"), " Warning:")),
        lapply(warnings, div)
      )
    })

    validate_time_conversion <- reactive({

      if (!time_page_needed()) {
        return(TRUE)
      }

      d1 <- getdatalist()
      req(d1)

      time <- attr(d1[[1]], "time")
      time_out <- formatted_time()

      for (i in seq_len(ncol(time))) {

        type_i <- effective_time_type(
          input[[paste0("time_type_", i)]],
          input[[paste0("time_format_", i)]],
          input[[paste0("time_custom_", i)]]
        )

        if (is.null(type_i) || type_i == "keep") {
          next
        }

        original_non_na <- sum(!is.na(time[[i]]))
        converted_non_na <- sum(!is.na(time_out[[i]]))

        validate(
          need(
            !(original_non_na > 0 && converted_non_na == 0),
            paste0(
              "Cannot proceed: column '",
              colnames(time)[i],
              "' could not be converted. Please check the selected time format."
            )
          )
        )
      }

      TRUE
    })

    observeEvent(input$dl_next, ignoreInit = TRUE, {

      if (curpage() == "page1") {

        if (input$up_or_ex != "example") {
          req(input$data_name != "")
        }

        d1 <- create_current_datalist()

        getdatalist(d1)

        if (time_page_needed()) {
          curpage("time")
        } else {
          curpage("page2")
        }

      } else if (curpage() == "time") {

        validate_time_conversion()

        curpage("page2")
      }
    })

    observeEvent(input$dl_prev, ignoreInit = TRUE, {

      if (curpage() == "time") {

        curpage("page1")

      } else if (curpage() == "page2") {

        if (time_page_needed()) {
          curpage("time")
        } else {
          curpage("page1")
        }

      } else if (curpage() == "page3") {

        curpage("page2")
      }
    })

    observeEvent(curpage(), {
      if (curpage() == "page1") {
        getdatalist(NULL)
      }
    })

    observeEvent(input$dl_insert, ignoreInit = TRUE, {

      curpage("page1")

      datalist <- formatted_datalist()

      inserted_idx <- length(vals$saved_data) + 1
      vals$saved_data[[length(vals$saved_data) + 1]] <- datalist[[1]]
      names(vals$saved_data)[length(vals$saved_data)] <- input$data_name

      if (input$up_or_ex == "example") {

        names(vals$saved_data)[length(vals$saved_data)] <- selected_example()[["name"]]

        if (has_paired_example()) {
          envi <- getdatalist_envi()[[1]]
          envi <- data_migrate(datalist[[1]], envi, selected_example()[["paired_name"]])

          vals$saved_data[[length(vals$saved_data) + 1]] <- envi
          names(vals$saved_data)[[length(vals$saved_data)]] <- selected_example()[["paired_name"]]
        }

      } else {

        inserted_idx <- length(vals$saved_data)

      }

      names(vals$saved_data) <- make.unique(names(vals$saved_data))
      vals$cur_data <- names(vals$saved_data)[inserted_idx]

      vals$newdata <- TRUE

      reset_create_state()
      removeModal()

    })

    output$insert_page2 <- renderUI({

      d1 <- formatted_datalist()
      req(d1)

      coords <- attr(d1[[1]], "coords")

      if (!is.null(coords)) {

        validate(
          need(
            ncol(coords) == 2,
            "Cannot proceed: The coordinates sheet has more columns than allowed. It should only contain IDs followed by 2 columns: longitude and latitude in decimal format. Please correct or remove the coordinates sheet"
          )
        )

        validate(
          need(
            is.numeric(coords[, 1]),
            "Cannot proceed: The longitude column in the coordinates table is not numeric. Please correct or remove the coordinates sheet."
          )
        )

        validate(
          need(
            is.numeric(coords[, 2]),
            "Cannot proceed: The latitude column in the coordinates table is not numeric. Please correct or remove the coordinates sheet."
          )
        )
      }

      time <- attr(d1[[1]], "time")

      if (!is.null(time)) {

        validate(
          need(
            ncol(time) >= 1,
            "Cannot proceed: The Temporal-Attribute must contain at least one temporal column."
          )
        )
      }

      if (has_paired_example()) {

        div(
          style = "display: flex; gap: 10px",
          datalist_render(d1[[1]], FALSE, width = "60px"),
          datalist_render(getdatalist_envi()[[1]], FALSE, width = "60px")
        )

      } else {

        div(
          style = "width: 50%",
          datalist_render(d1[[1]], FALSE, width = "60px")
        )
      }
    })
  })
}

tool2<-list()
# The tool2$ui function creates the user interface for managing and editing Datalists.
# It provides a toolkit with multiple tabs, each corresponding to a specific operation:
# renaming, merging, exchanging attributes, replacing values, editing columns, modifying datasets,
# transposing data, managing shapefiles, executing custom code, generating outputs, and deleting Datalists.
tool2$ui<-function(id){
  ns=NS(id)
  tool2_tabs<-append(
    tool2_tabs,
    list(span(
      "Spatio-temporal Features",
      icon(
        "fas fa-question-circle",
        class="text-info",
        `data-toggle`="tooltip",
        `data-placement`="right",
        title="Create spatial and spatio-temporal predictors from Numeric-, Coords-, and Temporal-Attributes."
      )
    )),
    after=10
  )
  tool2_tabs<-append(
    tool2_tabs,
    list(span(
      "SMOTE",
      icon(
        "fas fa-question-circle",
        class="text-info",
        `data-toggle`="tooltip",
        `data-placement`="right",
        title="Balance classes by creating synthetic observations from Numeric-Attribute variables."
      )
    )),
    after=11
  )
  div(style="margin-top: -35px",
      div(class="toolkit_items",style="width: 550px; height: 320px;      background: #00000095;; position: fixed;right: 0px; z-index: 9",),
      tags$style(HTML("
      .tool2_tab9 .half-drop .form-control,
.tool2_tab9,
  .tool2_tab9 label,
  .tool2_tab9 .control-label,
  .tool2_tab9 .btn,
  .tool2_tab9 .dropdown-toggle,
  .tool2_tab9 .filter-option,
  .tool2_tab9 .filter-option-inner-inner,
   .tool2_tab9 .dropdown-item {
   min-height: 22px !important;
    height: 22px !important;
    max-height: 22px !important;
    padding-top: 2px;
    font-size: 12px !important;
   }
  .tool2_tab9 button, .tool2_tab9 input, .tool2_tab9 select, .tool2_tab9 textarea {
    padding: 2px 3px !important;
  }

      .tool2_tab10 {
        width: 92vw;
        min-width: 980px;
        max-width: 1450px;
        background: white;
      }


                      ")),
      div(
        class="toolkit_items",id=ns("toolkit"),

        lapply(seq_along(tool2_tabs),function(i){
          style=""
          if(i%in%c(14,15)){
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
                                 tool2_tab1$ui(ns("rename"))



                        ),
                        tabPanel(tool2_tabs[2],value="tab2",
                                 tool2_tab2$ui(ns("merge"))


                        ),
                        tabPanel(tool2_tabs[3],value="tab3",
                                 tool2_tab3$ui(ns("exchange"))

                        ),
                        tabPanel(tool2_tabs[4],value="tab4",
                                 tool2_tab4$ui(ns("replace")),
                        ),
                        tabPanel(tool2_tabs[5],value="tab5",
                                 div(tool2_tab5$ui(ns("editcol")))

                        ),
                        tabPanel(tool2_tabs[6],value="tab6",
                                 tool2_tab6$ui(ns("editmod"))

                        ),
                        tabPanel(tool2_tabs[7],value="tab7",
                                 tool2_tab7$ui(ns("transpose"))
                        ),
                        tabPanel(tool2_tabs[8],value="tab8",
                                 tool2_tab8$ui(ns("shp"))),
                        tabPanel(tool2_tabs[9],value="tab9",
                                 tool2_tab9$ui(ns("time"))),




                        tabPanel(tool2_tabs[10],value="tab10",
                                 tool2_tab10$ui(ns("time_lag"))
                        ),
                        tabPanel(tool2_tabs[11],value="tab11",
                                 tool2_tab11$ui(ns("space_time"))
                        ),
                        tabPanel(tool2_tabs[12],value="tab12",
                                 tool2_tab12$ui(ns("smote"))
                        ),
                        tabPanel(tool2_tabs[13],value="tab13",
                                 tool2_tab13$ui(ns("code"))
                        ),
                        tabPanel(tool2_tabs[14],value="tab14",
                                 tool2_tab14$ui(ns("gen"))),
                        tabPanel(tool2_tabs[15],value="tab15",
                                 tool2_tab15$ui(ns("deldatalist"))
                        )
            )
          )
      )

  )
}
# The tool2$server function implements the server-side logic for the tool2 module.
# It manages the functionality of each toolkit tab, including switching between tabs,
# rendering the corresponding UI components, and calling their respective server logic.
tool2$server<-function(id,vals){

  moduleServer(id,function(input,output,session){

    ns<-session$ns
    tool2_tabs<-append(
      tool2_tabs,
      list(span(
        "Spatio-temporal Features",
        icon(
          "fas fa-question-circle",
          class="text-info",
          `data-toggle`="tooltip",
          `data-placement`="right",
          title="Create spatial and spatio-temporal predictors from Numeric-, Coords-, and Temporal-Attributes."
        )
      )),
      after=10
    )
    tool2_tabs<-append(
      tool2_tabs,
      list(span(
        "SMOTE",
        icon(
          "fas fa-question-circle",
          class="text-info",
          `data-toggle`="tooltip",
          `data-placement`="right",
          title="Balance classes by creating synthetic observations from Numeric-Attribute variables."
        )
      )),
      after=11
    )

    shinyjs::onevent("mouseleave", "toolkit", {
      shinyjs::hide(selector=".toolkit_items")
      shinyjs::show(selector='.tool2-tabs')
    })

    # Each tool's server is created only the first time the tool is opened, so the
    # Options menu opens without starting all tools (and their observers) at once.
    # Order follows tool2_tabs / tool_kit_i / tab i.
    tool_servers<-list(
      function() tool2_tab1$server("rename", vals),
      function() tool2_tab2$server("merge", vals),
      function() {
        tool2_tab3$update_server("exchange", vals)
        tool2_tab3$server("exchange", vals)
      },
      function() tool2_tab4$server("replace", vals),
      function() tool2_tab5$server("editcol", vals),
      function() tool2_tab6$server("editmod", vals),
      function() tool2_tab7$server("transpose", vals),
      function() tool2_tab8$server("shp", vals),
      function() tool2_tab9$server("time", vals),
      function() tool2_tab10$server("time_lag", vals),
      function() tool2_tab11$server("space_time", vals),
      function() tool2_tab12$server("smote", vals),
      function() tool2_tab13$server("code", vals),
      function() tool2_tab14$server("gen", vals),
      function() tool2_tab15$server("deldatalist", vals)
    )
    tool_started<-rep(FALSE, length(tool_servers))
    start_tool<-function(i){
      if(i>length(tool_servers)||isTRUE(tool_started[i])) return(invisible(NULL))
      tool_started[i]<<-TRUE
      tool_servers[[i]]()
      invisible(NULL)
    }

    lapply(seq_along(tool2_tabs), function(i) {
      observeEvent(input[[paste0("tool_kit_", i)]], {
        start_tool(i)
        updateTabsetPanel(session, "tabs_tool2", selected = paste0("tab", i))
        shinyjs::hide(selector = paste0("#", ns("toolkit")))
        shinyjs::show(selector = paste0("#", ns("tool2_tabs_container")))
      })
    })



  })
}

# Rename & Reorder Datalist
tool2_tab1 <- list()
tool2_tab1$ui <- function(id) {
  ns <- NS(id)

  div(
    style = "display: flex",
    div(
      p(
        strong("Rename & Reorder Datalist"),
        tiphelp("Rename Datalists or reorder them by dragging and dropping.")
      ),
      div(
        style = "overflow-y: auto; max-height: calc(100vh - 250px)",
        uiOutput(ns("rename_page"))
      )
    ),
    div(
      class = "half-drop",
      actionButton(ns("run_rename"), "Apply", icon = icon("sync"))
    )
  )
}
tool2_tab1$server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    saved_names <- reactive({
      names(vals$saved_data %||% list())
    })

    output$rename_page <- renderUI({
      choices <- saved_names()

      if (!length(choices)) {
        return(em("No Datalists available.", style = "color: gray"))
      }

      labels <- lapply(seq_along(choices), function(i) {
        span(
          style = "display: flex",
          class = "half-drop",
          span(i, style = "font-size: 0px; color: transparent"),
          inline(textInput(ns(paste0("newname_datalist_", i)), NULL, choices[i]))
        )
      })

      sortable::rank_list(
        labels = labels,
        input_id = ns("ord_saved_data"),
        class = "saved_data_rename"
      )
    })

    observeEvent(input$run_rename, {
      saved_data <- vals$saved_data
      old_names <- names(saved_data)

      req(length(saved_data))

      new_names <- vapply(seq_along(saved_data), function(i) {
        value <- input[[paste0("newname_datalist_", i)]]
        value <- trimws(value %||% "")

        if (!nzchar(value)) {
          old_names[i]
        } else {
          value
        }
      }, character(1))

      new_names <- make.unique(new_names)

      ord_raw <- input$ord_saved_data
      ord <- suppressWarnings(as.integer(sub("^\\s*([0-9]+).*$", "\\1", ord_raw)))

      if (length(ord) != length(saved_data) || anyNA(ord) || any(!ord %in% seq_along(saved_data))) {
        ord <- seq_along(saved_data)
      }

      names(saved_data) <- new_names
      vals$saved_data <- saved_data[ord]

      done_modal()
    }, ignoreInit = TRUE)
  })
}

# Merge Datalists by rows or columns
tool2_tab2 <- list()
tool2_tab2$ui <- function(id) {
  ns <- NS(id)

  div(
    style = "display: flex",
    div(
      p(strong("Merge Datalists by rows or columns")),
      radioButtons(
        ns("mergeby"),
        "Merge by:",
        choiceValues = c("col", "row"),
        choiceNames = c("Columns", "Rows"),
        inline = TRUE
      ),
      div(
        class = "virtual-180",
        virtualPicker(ns("tomerge"), label = "Select the Datalists", "selected Datalists")
      )
    ),
    div(
      div(id = ns("run_merge_tip"), em("Select at least two Datalists", style = "color: gray")),
      hidden(
        div(
          id = ns("run_merge_btn"),
          class = "save_changes",
          bsButton(ns("run_merge"), "Merge >")
        )
      ),
      div(
        checkboxInput(
          ns("fill"),
          span(
            "Fill missings",
            tiphelp("Fills missing columns with NAs. If not checked, restrict data to common columns/rows from the selected Datalists")
          ),
          TRUE
        )
      )
    )
  )
}
tool2_tab2$server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_merge <- reactiveVal(NULL)

    saved_names <- reactive({
      names(vals$saved_data %||% list())
    })

    selected_datalists <- reactive({
      req(vals$saved_data)

      selected <- input$tomerge
      selected <- selected[selected %in% saved_names()]

      req(length(selected) >= 2)

      vals$saved_data[selected]
    })

    merge_args <- reactive({
      list(
        to_merge = selected_datalists(),
        mergeby = req(input$mergeby),
        fill = isTRUE(input$fill)
      )
    })

    observeEvent(saved_names(), {
      choices <- saved_names()
      selected <- intersect(input$tomerge %||% character(0), choices)

      if (length(selected) < 2) {
        selected <- head(choices, min(2, length(choices)))
      }

      shinyWidgets::updateVirtualSelect(
        inputId = "tomerge",
        choices = choices,
        selected = selected,
        session = session
      )
    }, ignoreInit = FALSE)

    observeEvent(input$tomerge, {
      if (length(input$tomerge %||% character(0)) >= 2) {
        shinyjs::hide("run_merge_tip")
        shinyjs::show("run_merge_btn")
      } else {
        shinyjs::show("run_merge_tip")
        shinyjs::hide("run_merge_btn")
      }
    }, ignoreInit = FALSE)

    observeEvent(merge_args(), {
      shinyjs::addClass("run_merge_btn", "save_changes")
      r_merge(NULL)
    }, ignoreInit = TRUE)

    show_save_modal <- function() {
      showModal(
        modalDialog(
          title = "Save changes",
          easyClose = TRUE,
          footer = div(
            actionButton(ns("data_confirm"), strong("confirm")),
            modalButton("Cancel")
          ),
          div(
            style = "padding: 20px",
            div(
              class = "half-drop",
              style = "display: flex",
              div(
                style = "width: 20%",
                radioButtons(ns("create_replace"), NULL, c("Create"))
              ),
              div(
                style = "padding-top: 10px",
                uiOutput(ns("out_newdatalist"))
              )
            ),
            div(
              style = "padding-left: 30px",
              uiOutput(ns("message")),
              uiOutput(ns("newdata"))
            )
          )
        )
      )
    }

    run_merge <- function() {
      args <- merge_args()

      result <- tryCatch({
        withProgress(message = "Merging....", {
          df <- do.call(imesc_merge, args)

          coords_list <- lapply(args$to_merge, function(x) attr(x, "coords"))
          factors_list <- lapply(args$to_merge, function(x) attr(x, "factors"))

          if (all(vapply(coords_list, Negate(is.null), logical(1)))) {
            df_coords <- do.call(
              imesc_merge,
              list(to_merge = coords_list, mergeby = args$mergeby, fill = args$fill)
            )
            df_coords <- df_coords[rownames(df), 1:min(2, ncol(df_coords)), drop = FALSE]
            attr(df, "coords") <- df_coords
          }

          if (all(vapply(factors_list, Negate(is.null), logical(1)))) {
            df_factors <- do.call(
              imesc_merge,
              list(to_merge = factors_list, mergeby = args$mergeby, fill = args$fill)
            )
            df_factors <- df_factors[rownames(df), , drop = FALSE]
            df_factors <- data.frame(lapply(df_factors, as.factor), check.names = FALSE)
            rownames(df_factors) <- rownames(df)
            attr(df, "factors") <- df_factors
          }

          df <- data_migrate(args$to_merge[[1]], df)
          attr(df, "bag") <- "Merged"

          df
        })
      }, error = function(e) {
        showNotification(
          paste("Merge failed:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
        NULL
      })

      req(result)

      r_merge(result)
      shinyjs::removeClass("run_merge_btn", "save_changes")
      show_save_modal()
    }

    observeEvent(input$run_merge, {
      args <- merge_args()

      ncols <- vapply(args$to_merge, ncol, integer(1))
      nrows <- vapply(args$to_merge, nrow, integer(1))

      if (any(c(ncols, nrows) > 10000)) {
        showModal(
          modalDialog(
            size = "s",
            easyClose = TRUE,
            footer = div(
              actionButton(ns("proceed_merge"), "Proceed"),
              modalButton("Cancel")
            ),
            div(
              p(
                strong("Warning:", style = "color: red"),
                "Merging large datasets can be computationally intensive and may result in long processing times. Click 'Proceed' to continue."
              )
            )
          )
        )
      } else {
        run_merge()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$proceed_merge, {
      removeModal()
      run_merge()
    }, ignoreInit = TRUE)

    output$newdata <- renderUI({
      req(r_merge())
      basic_summary2(r_merge())
    })

    output$out_newdatalist <- renderUI({
      req(input$create_replace == "Create")
      req(r_merge())

      bag <- attr(r_merge(), "bag") %||% "Merged"
      new_names <- make.unique(c(saved_names(), bag))
      name0 <- tail(new_names, 1)

      textInput(ns("newdatalist"), NULL, name0)
    })

    output$message <- renderUI({
      NULL
    })

    observeEvent(input$data_confirm, {
      req(r_merge())
      req(input$newdatalist)

      new_name <- trimws(input$newdatalist)

      if (!nzchar(new_name)) {
        showNotification("Choose a valid Datalist name.", type = "error")
        return()
      }

      new_name <- make.unique(c(saved_names(), new_name))
      new_name <- tail(new_name, 1)

      vals$saved_data[[new_name]] <- r_merge()

      removeModal()
      done_modal()
    }, ignoreInit = TRUE)
  })
}

# Exchange Attributes
# Exchange Attributes
tool2_tab3 <- list()

tool2_tab3$ui <- function(id) {
  ns <- NS(id)

  attr_choices <- c(
    "Numeric-Attribute" = "numeric",
    "Factor-Attribute" = "factor",
    "Coords-Attribute" = "coords",
    "Temporal-Attribute" = "time"
  )

  div(
    style = "height: calc(100vh - 100px);",

    div(
      style = "position: fixed; top: 60px; right: 0px",
      div(
        style = "display: flex; gap: 10px",
        bsButton(ns("prev_import"), "< Previous", width = "100px"),
        bsButton(ns("next_import"), "Next >", width = "100px")
      )
    ),

    hidden(bsButton(ns("cancel_import"), "Cancel")),

    div(strong("Exchange Attributes")),

    div(
      id = ns("step1"),

      div(
        class = "radio_search",
        radioGroupButtons(ns("copy_transfer"), span("Action"), choices = c("Copy", "Move"))
      ),

      checkboxInput(
        ns("replace"),
        span(
          "Replace matching columns",
          tipright("If checked, matching columns in the destination are replaced. If unchecked, columns are added with unique names.")
        ),
        FALSE
      ),

      uiOutput(ns("rule_message")),

      column(
        12,
        class = "mp0",
        column(
          2,
          style = "margin:0px;padding:0px;width:70px",
          tags$label("Datalist:", style = "padding-top:30px"),
          tags$label("Attribute:", style = "padding-top:25px")
        ),
        column(
          4,
          style = "margin:0px;padding:0px",
          uiOutput(ns("import_from_data")),
          pickerInput(ns("import_from_attr"), NULL, choices = attr_choices, selected = "factor")
        ),
        column(
          1,
          style = "margin:0px;padding:0px;width:30px",
          align = "center",
          div(actionLink(ns("rev_datalist"), icon("arrow-right-arrow-left")), style = "position:absolute;top:35px;left:10px"),
          div(actionLink(ns("rev_attr"), icon("arrow-right-arrow-left")), style = "position:absolute;top:80px;left:10px"),
          bsTooltip(ns("rev_datalist"), "Switch", "right"),
          bsTooltip(ns("rev_attr"), "Switch", "right")
        ),
        column(
          4,
          style = "margin:0px;padding:0px",
          uiOutput(ns("import_to_data")),
          pickerInput(ns("import_to_attr"), NULL, choices = attr_choices, selected = "numeric")
        )
      ),

      column(
        12,
        class = "mp0",
        column(
          6,
          class = "virtual_small",
          virtualPicker(ns("importvar"), label = "Columns", "selected")
        ),
        column(
          6,
          hidden(
            radioButtons(
              ns("hand_facs"),
              "Conversion type:",
              choiceValues = c("Binary", "Ordinal"),
              choiceNames = list(
                div("Binary", span(id = ns("conv_bin"), icon("question-circle"))),
                div("Integer", span(id = ns("conv_ord"), icon("question-circle")))
              ),
              inline = TRUE
            )
          ),
          div(uiOutput(ns("error_transf0")))
        )
      ),

      bsTooltip(
        ns("conv_bin"),
        "Creates one binary column per factor level, with 1 indicating the class of each observation.",
        placement = "right",
        options = list(style = "width: 600px")
      ),
      bsTooltip(
        ns("conv_ord"),
        "Creates one numeric column using integer values assigned to factor levels.",
        placement = "right"
      )
    ),

    div(
      id = ns("step2"),
      uiOutput(ns("from_to")),

      div(
        style = "height:450px;overflow-y:scroll;overflow-x:scroll",

        checkboxInput(
          ns("cutfacs"),
          span(
            "Cut into intervals",
            tiphelp("Divides numeric values into intervals using base R's cut function.", "right")
          ),
          FALSE
        ),

        uiOutput(ns("cut_fac_method")),
        div(uiOutput(ns("tofactor")), style = "font-size:11px"),
        uiOutput(ns("time_format_page")),
        uiOutput(ns("coords_warning")),
        uiOutput(ns("error_transf")),
        uiOutput(ns("binary_page"))
      )
    ),

    div(id = ns("step3"), uiOutput(ns("page3")))
  )
}

tool2_tab3$update_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$rev_attr, {
      req(input$import_from_attr, input$import_to_attr)

      from_attr <- input$import_from_attr
      to_attr <- input$import_to_attr

      updatePickerInput(session, "import_from_attr", selected = to_attr)
      updatePickerInput(session, "import_to_attr", selected = from_attr)
    }, ignoreInit = TRUE)

    observeEvent(input$rev_datalist, {
      req(input$import_from_data, input$import_to_data)

      from_data <- input$import_from_data
      to_data <- input$import_to_data

      updatePickerInput(session, "import_from_data", selected = to_data)
      updatePickerInput(session, "import_to_data", selected = from_data)
    }, ignoreInit = TRUE)

    observe({
      shinyjs::toggle(
        "rev_datalist",
        condition = !identical(input$import_from_data, input$import_to_data)
      )
    })
  })
}

tool2_tab3$server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%or%` <- function(x, y) {
      if (is.null(x) || length(x) == 0) y else x
    }

    safe_id <- function(x) {
      gsub("[^A-Za-z0-9_]", "_", make.names(as.character(x), unique = FALSE))
    }

    saved_names <- reactive({
      names(vals$saved_data %or% list())
    })

    selected_saved_data <- function(name) {
      req(name)
      req(name %in% saved_names())
      vals$saved_data[[name]]
    }

    convert <- reactive({
      c(
        from = input$import_from_attr %or% "factor",
        to = input$import_to_attr %or% "numeric"
      )
    })

    is_convert <- function(from, to) {
      identical(unname(convert()), c(from, to))
    }

    attr_label <- function(x) {
      switch(
        x,
        numeric = "Numeric-Attribute",
        factor = "Factor-Attribute",
        coords = "Coords-Attribute",
        time = "Temporal-Attribute",
        x
      )
    }




    observeEvent(input$import_from_data,{
      attr_choices <- c(
        "Numeric-Attribute" = "numeric",
        "Factor-Attribute" = "factor"

      )
      req(input$import_from_data)

      data <- selected_saved_data(input$import_from_data)
      coords_exists<-!is.null(attr(data,"coords"))
      if(coords_exists){
        attr_choices<-c(attr_choices,"Coords-Attribute" = "coords")
      }
      time_exists<-!is.null(attr(data,"time"))
      if(time_exists){
        attr_choices<-c(attr_choices,"Temporal-Attribute" = "time")
      }

      updatePickerInput(session,'import_from_attr', choices=attr_choices)

    })

    observe({
      req(input$import_from_attr)
      data <- selected_saved_data(input$import_from_data)




      attr_choices <- c(
        "Numeric-Attribute" = "numeric",
        "Factor-Attribute" = "factor")


      coords_exists<-!is.null(attr(data,"coords"))

      if(coords_exists){
        attr_choices<-c(attr_choices,"Coords-Attribute" = "coords")
      }
      time_exists<-!is.null(attr(data,"time"))
      if(time_exists){
        attr_choices<-c(attr_choices,"Temporal-Attribute" = "time")
      }

      if(input$import_from_attr%in%c('coords','time')){
        choices=c("Numeric-Attribute" = "numeric")
      }
      if(input$import_from_attr%in%c('factor')){
        attr_choices=c("Numeric-Attribute" = "numeric",
                       "Factor-Attribute" = "factor")
      }

      updatePickerInput(session,'import_to_attr', choices=attr_choices)
    })

    get_attr_data <- function(data, attr_type) {
      if (identical(attr_type, "factor")) {
        out <- attr(data, "factors")
        if (is.null(out)) out <- data.frame(row.names = rownames(data))
        return(as.data.frame(out, check.names = FALSE))
      }

      if (identical(attr_type, "coords")) {
        out <- attr(data, "coords")
        if (is.null(out)) out <- data.frame(row.names = rownames(data))
        return(as.data.frame(out, check.names = FALSE))
      }

      if (identical(attr_type, "time")) {
        out <- attr(data, "time")
        if (is.null(out)) out <- data.frame(row.names = rownames(data))
        return(as.data.frame(out, check.names = FALSE))
      }

      as.data.frame(data, check.names = FALSE)
    }

    align_to_target_rows <- function(data, target) {
      target_rows <- rownames(target)

      if (is.null(target_rows) || !length(target_rows)) {
        return(data)
      }

      data[target_rows, , drop = FALSE]
    }

    valid_exchange <- reactive({
      from <- convert()[["from"]]
      to <- convert()[["to"]]

      if (from == "coords" && !to %in% "numeric") {
        return("Coords-Attribute can only be transferred to Numeric-Attribute.")
      }

      if (to == "coords" && !from %in% "numeric") {
        return("Only Numeric-Attribute columns can be transferred to Coords-Attribute.")
      }

      if (from == "time" && !to %in% c("numeric", "factor")) {
        return("Temporal-Attribute can be transferred to Numeric-Attribute or Factor-Attribute.")
      }

      if (to == "time" && !from %in% c("numeric", "factor")) {
        return("Only Numeric-Attribute or Factor-Attribute columns can be transferred to Temporal-Attribute.")
      }

      if (from == "factor" && to %in% "coords") {
        return("Factor-Attribute cannot be transferred to Coords-Attribute.")
      }

      TRUE
    })

    # Numeric or Factor columns going to the Temporal-Attribute share the format page
    is_to_time <- function() {
      identical(convert()[["to"]], "time") && convert()[["from"]] %in% c("numeric", "factor")
    }

    output$rule_message <- renderUI({
      rule <- valid_exchange()

      if (isTRUE(rule)) {
        return(NULL)
      }

      div(
        class = "alert_warning",
        style = "padding: 8px; margin-bottom: 8px;",
        strong(icon("triangle-exclamation"), " Invalid exchange: "),
        rule
      )
    })

    output$import_from_data <- renderUI({
      choices <- saved_names()

      pickerInput_fromtop_live(
        ns("import_from_data"),
        "From:",
        choices = choices,
        selected = vals$cur_import_from_data %or% choices[1]
      )
    })

    output$import_to_data <- renderUI({
      choices <- saved_names()

      pickerInput_fromtop_live(
        ns("import_to_data"),
        "To:",
        choices = choices,
        selected = vals$cur_import_to_data %or% choices[1]
      )
    })

    observeEvent(input$import_from_data, {
      vals$cur_import_from_data <- input$import_from_data
    }, ignoreInit = TRUE)

    observeEvent(input$import_to_data, {
      vals$cur_import_to_data <- input$import_to_data
    }, ignoreInit = TRUE)

    from_data <- reactive({
      req(input$import_from_data, input$import_to_data, input$import_from_attr)

      source <- selected_saved_data(input$import_from_data)
      target <- selected_saved_data(input$import_to_data)

      data <- get_attr_data(source, input$import_from_attr)
      data <- align_to_target_rows(data, target)

      attr(data, "attr") <- attr_label(input$import_from_attr)
      attr(data, "name") <- input$import_from_data

      data
    })

    to_data <- reactive({
      req(input$import_to_data, input$import_to_attr)

      target <- selected_saved_data(input$import_to_data)
      data <- get_attr_data(target, input$import_to_attr)

      attr(data, "attr") <- attr_label(input$import_to_attr)
      attr(data, "name") <- input$import_to_data

      data
    })

    observeEvent(list(input$import_from_data, input$import_from_attr, vals$saved_data), {
      data <- tryCatch(from_data(), error = function(e) NULL)
      choices <- colnames(data %or% data.frame())

      selected <- choices[1]

      if (identical(input$import_to_attr, "coords")) {
        selected <- head(choices, 2)
      }

      shinyWidgets::updateVirtualSelect(
        inputId = "importvar",
        choices = choices,
        selected = selected,
        session = session
      )
    }, ignoreInit = FALSE)

    observeEvent(list(input$importvar, input$import_to_attr), {
      if (identical(input$import_to_attr, "coords") && length(input$importvar %or% character(0)) > 2) {
        shinyWidgets::updateVirtualSelect(
          inputId = "importvar",
          selected = head(input$importvar, 2),
          session = session
        )

        showNotification(
          "Coords-Attribute accepts at most two columns. Only the first two selected columns were kept.",
          type = "warning",
          duration = 6
        )
      }
    }, ignoreInit = TRUE)

    val_transf <- reactive({
      rule <- valid_exchange()

      if (!isTRUE(rule)) {
        return(structure(FALSE, logs = rule))
      }

      tryCatch({
        dfrom <- from_data()
        dto <- to_data()

        if (ncol(dfrom) == 0) {
          return(structure(FALSE, logs = "The selected source attribute has no columns."))
        }

        if (identical(input$import_to_attr, "coords") && length(input$importvar %or% character(0)) > 2) {
          return(structure(FALSE, logs = "Coords-Attribute accepts at most two columns."))
        }

        if (all(convert() %in% c("numeric", "factor"))) {
          result <- validate_transf(dfrom, dto)
          return(result)
        }

        TRUE
      }, error = function(e) {
        structure(FALSE, logs = conditionMessage(e))
      })
    })

    output$error_transf0 <- renderUI({
      message <- attr(val_transf(), "logs")
      div(class = "small_print2", render_message(message))
    })

    output$error_transf <- renderUI({
      render_message(vals$error_transf)
    })

    output$from_to <- renderUI({
      div(
        "from",
        strong(embrown(attr_label(convert()[["from"]]))),
        "to",
        strong(emgreen(attr_label(convert()[["to"]])))
      )
    })

    output$coords_warning <- renderUI({
      req(is_convert("numeric", "coords"))

      div(
        class = "alert_warning",
        style = "padding: 10px; margin-top: 10px;",
        strong(icon("triangle-exclamation"), " Warning: "),
        "The selected numeric columns will replace the destination Coords-Attribute."
      )
    })

    output$cut_fac_method <- renderUI({
      req(isTRUE(input$cutfacs))

      div(
        class = "half-drop half-drop-inline",
        selectInput(
          ns("bin_method"),
          label = span(
            "Bin method",
            tipify_ui(actionLink(ns("bin_method_help"), icon("fas fa-question-circle")), "Click for details")
          ),
          choices = c(
            "Sturges" = "sturge",
            "Scott" = "scott",
            "Freedman-Diaconis" = "freedman"
          )
        )
      )
    })

    observeEvent(input$bin_method_help, {
      showModal(
        modalDialog(
          title = "Methods for estimating the initial number of bins",
          easyClose = TRUE,
          fluidRow(
            class = "mp0",
            tags$style(HTML(".formulas div.MathJax_Display{text-align:left !important;color:gray;white-space:normal;font-size:11px}")),
            div(
              class = "formulas",
              column(
                6,
                strong("Sturges' Rule"),
                div(withMathJax("$$\\text{Number of bins} = \\lceil \\log_2(n) + 1 \\rceil$$")),
                hr(),
                strong("Scott's Rule"),
                div(withMathJax(helpText("$$\\text{Bin width} = \\frac{3.5 \\cdot \\sigma}{n^{1/3}}$$"))),
                div(withMathJax(helpText("$$\\text{Number of bins} = \\left\\lceil \\frac{\\text{Range of data}}{\\text{Bin width}} \\right\\rceil$$"))),
                hr(),
                strong("Freedman-Diaconis Rule"),
                div(withMathJax(helpText("$$\\text{Bin width} = 2 \\cdot \\frac{\\text{IQR}}{n^{1/3}}$$"))),
                div(withMathJax(helpText("$$\\text{Number of bins} = \\left\\lceil \\frac{\\text{Range of data}}{\\text{Bin width}} \\right\\rceil$$")))
              ),
              column(
                6,
                div("Where:"),
                div(withMathJax(helpText("$$n=\\text{number of observations}$$"))),
                div(withMathJax(helpText("$$\\sigma=\\text{standard deviation of the data}$$"))),
                div(withMathJax(helpText("$$\\text{IQR}=\\text{interquartile range of the data}$$")))
              )
            )
          )
        )
      )
    }, ignoreInit = TRUE)

    page <- reactiveVal(1)
    data2 <- reactiveVal(NULL)
    r_exchange <- reactiveVal(NULL)

    observeEvent(input$prev_import, {
      vals$error_transf <- NULL
      data2(NULL)
      r_exchange(NULL)

      if (page() > 1) {
        page(page() - 1)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$next_import, {
      vals$error_transf <- NULL

      if (page() == 1) {
        req(isTRUE(val_transf()))
        page(2)
        return()
      }

      if (page() == 2) {
        prepare_and_confirm_exchange()
      }
    }, ignoreInit = TRUE)

    observe({
      shinyjs::toggle("prev_import", condition = page() > 1)
      shinyjs::toggle("next_import", condition = page() == 2 || isTRUE(val_transf()))
      shinyjs::toggle("step1", condition = page() == 1)
      shinyjs::toggle("step2", condition = page() == 2)
      shinyjs::toggle("step3", condition = FALSE)
    })

    observeEvent(convert(), {
      shinyjs::toggle("hand_facs", condition = is_convert("factor", "numeric"))

      can_cut <- is_convert("numeric", "numeric") || is_convert("numeric", "factor")

      if (can_cut) {
        shinyjs::show("cutfacs")
      } else {
        updateCheckboxInput(session, "cutfacs", value = FALSE)
        shinyjs::hide("cutfacs")
      }
    }, ignoreInit = FALSE)

    get_convert_data_vars <- reactive({
      req(input$import_from_data, input$import_to_data, input$import_from_attr, input$importvar)

      data <- from_data()
      vars <- input$importvar
      vars <- vars[vars %in% colnames(data)]

      req(length(vars) > 0)

      if (is_convert("numeric", "factor")) {
        data <- data.frame(lapply(data, as.factor), check.names = FALSE)
        rownames(data) <- rownames(from_data())
      }

      attr(data, "datalist") <- input$import_from_data

      list(data = data, vars = vars)
    })

    get_data_from <- reactive({
      obj <- get_convert_data_vars()
      data <- obj$data[, obj$vars, drop = FALSE]

      if (identical(convert()[["to"]], "factor")) {
        data <- data.frame(lapply(data, as.factor), check.names = FALSE)
        rownames(data) <- rownames(obj$data)
      }

      attr(data, "datalist") <- input$import_from_data
      data
    })

    get_data_cutted <- reactive({
      data <- get_data_from()

      if (isTRUE(input$cutfacs)) {
        res <- lapply(colnames(data), function(var) {
          breaks <- input[[paste0("cutfacs_breaks_", safe_id(var))]]
          req(breaks)

          cut(as.numeric(as.character(data[, var])), breaks)
        })

        data <- data.frame(res, check.names = FALSE)
        colnames(data) <- colnames(get_data_from())
      } else if (identical(convert()[["to"]], "factor")) {
        data <- data.frame(lapply(data, as.factor), check.names = FALSE)
        rownames(data) <- rownames(get_data_from())
      }

      attr(data, "datalist") <- input$import_from_data
      data
    })

    output$tofactor <- renderUI({
      if (is_convert("factor", "numeric")) {
        req(input$hand_facs == "Ordinal")
      } else {
        req(identical(convert()[["to"]], "factor"))
      }

      div(
        div(icon("hand-point-right"), "Drag and drop the green blocks to define their order.", style = "white-space:normal;"),
        if (identical(convert()[["to"]], "factor")) {
          div(icon("hand-point-right"), "Edit label names in the blue fields.")
        },
        if (is_convert("factor", "numeric")) {
          div(icon("hand-point-right"), "Edit numeric values in the blue fields.")
        },
        div(uiOutput(ns("dropLevelsEdit_a")), style = "font-size:11px"),
        div(uiOutput(ns("dropLevelsEdit_b")))
      )
    })

    rank_dfs <- reactive({
      data <- get_data_cutted()

      result <- lapply(colnames(data), function(var) {
        levels_var <- levels(data[, var])
        req(length(levels_var))

        rank_input <- input[[paste("rank_lev", safe_id(var), sep = "_")]]
        req(rank_input)

        order_index <- suppressWarnings(as.integer(rank_input))
        order_index <- order_index[!is.na(order_index)]

        if (!length(order_index)) {
          order_index <- seq_along(levels_var)
        }

        labels <- vapply(seq_along(levels_var), function(i) {
          label_input <- input[[paste("ordrank_label", safe_id(var), safe_id(levels_var[i]), sep = "_")]]
          label_input %or% levels_var[i]
        }, character(1))

        data.frame(
          level = order_index,
          label = labels,
          stringsAsFactors = FALSE
        )
      })

      names(result) <- colnames(data)
      result
    })

    output$dropLevelsEdit_a <- renderUI({
      data <- get_data_from()
      original_data <- selected_saved_data(input$import_from_data)

      level_title <- if (identical(convert()[["to"]], "numeric")) "Level" else "Levels"
      label_title <- if (identical(convert()[["to"]], "numeric")) "Numeric Value" else "Label"

      bin_function <- function(var) {
        fun <- switch(
          input$bin_method %or% "sturge",
          sturge = bin_Sturges,
          scott = bin_Scott,
          freedman = bin_Freedman,
          bin_Sturges
        )

        numericInput(
          ns(paste0("cutfacs_breaks_", safe_id(var))),
          NULL,
          value = fun(original_data[, var])
        )
      }

      make_header <- function(var) {
        if (isTRUE(input$cutfacs) && identical(convert()[["to"]], "numeric")) {
          div(bin_function(var))
        } else {
          div(level_title)
        }
      }

      div(
        id = "rank_levels",
        lapply(colnames(data), function(var) {
          div(
            style = "margin-top:5px;margin-bottom:15px",
            column(
              4,
              style = "padding-right:5px",
              column(
                12,
                class = "half-drop rankcol",
                textInput(ns(paste0("newcol_", safe_id(var))), NULL, var)
              ),
              column(
                12,
                class = "mp0",
                if (identical(convert()[["to"]], "numeric")) {
                  div(
                    column(8, class = "half-drop rankcol", column(12, class = "mp0", make_header(var), style = "text-align:center")),
                    column(4, class = "mp0", div(label_title, style = "text-align:center"))
                  )
                } else {
                  div(
                    column(4, class = "mp0", div(make_header(var), style = "text-align:center")),
                    column(8, class = "half-drop rankcol", column(12, class = "mp0", label_title, style = "text-align:center"))
                  )
                },
                column(
                  12,
                  div(uiOutput(ns(paste0("facord_outl_", safe_id(var)))), style = "padding-bottom:30px"),
                  class = "mp0"
                )
              )
            )
          )
        })
      )
    })

    drop_levels_edit <- reactive({
      data <- get_data_cutted()

      lapply(colnames(data), function(var) {
        local({
          var_local <- var
          var_id <- safe_id(var_local)

          output[[paste0("facord_outl_", var_id)]] <- renderUI({
            levels_var <- levels(data[, var_local])
            req(length(levels_var))

            value_col <- column(
              4,
              class = "mp0",
              lapply(seq_along(levels_var), function(i) {
                if (identical(convert()[["to"]], "numeric")) {
                  column(
                    12,
                    class = "mp0 ord_label",
                    div(
                      class = "num_label",
                      numericInput(ns(paste0("fac2num_value_", var_id, "_", i)), NULL, i, width = "75px")
                    )
                  )
                } else {
                  div(
                    i,
                    style = "border-top:1px solid;border-bottom:1px solid;height:24px;margin:0px;text-align:center"
                  )
                }
              })
            )

            rank_col <- column(
              8,
              class = "mp0",
              sortable::rank_list(
                labels = lapply(seq_along(levels_var), function(i) {
                  level <- levels_var[i]

                  div(
                    class = "ord_label factor_label",
                    span(i, style = "display:none"),
                    level,
                    if (identical(convert()[["to"]], "factor")) {
                      div(textInput(ns(paste("ordrank_label", var_id, safe_id(level), sep = "_")), NULL, level, width = "95px"))
                    } else {
                      div(
                        level,
                        class = "form form-group form-control shiny-input-container",
                        style = "padding-top:5px;padding-left:5px;position:absolute;left:0px;background:#D0F0C0;"
                      )
                    }
                  )
                }),
                input_id = ns(paste("rank_lev", var_id, sep = "_")),
                class = "rankcol sortable"
              )
            )

            if (identical(convert()[["to"]], "numeric")) {
              div(rank_col, value_col)
            } else {
              div(value_col, rank_col)
            }
          })
        })
      })

      NULL
    })

    output$dropLevelsEdit_b <- renderUI({
      drop_levels_edit()
    })

    output$binary_page <- renderUI({
      req(input$importvar)

      data <- get_data_from()

      if (is_convert("factor", "numeric")) {
        req(input$hand_facs == "Binary")
        cols <- colnames(getclassmat(data))
      } else if (
        is_convert("numeric", "numeric") ||
        is_convert("coords", "numeric") ||
        is_convert("time", "numeric")
      ) {
        cols <- colnames(data)
      } else {
        return(NULL)
      }

      div(
        style = "overflow-y:auto;max-height:calc(100vh - 200px);margin-top:20px",
        div(icon("hand-point-right"), "Edit column names", style = "white-space:normal;font-size:11px"),
        h5(strong("New column names:")),
        lapply(cols, function(x) {
          div(
            class = "new_colnames",
            textInput(ns(paste("newbin", safe_id(x), sep = "_")), NULL, value = x)
          )
        })
      )
    })

    time_preview_value <- function(x, n = 4) {
      x <- x[!is.na(x)]
      x <- head(x, n)

      if (length(x) == 0) {
        return("NA")
      }

      paste(as.character(x), collapse = ", ")
    }

    output$time_format_page <- renderUI({
      req(is_to_time())

      data <- get_data_from()
      req(ncol(data) > 0)

      div(
        style = "width:95%;padding:10px",
        h4(
          "Format Temporal-Attribute",
          tiphelp_icon(
            icon("fas fa-question-circle"),
            "Define how iMESc should interpret each selected column before saving it as Temporal-Attribute."
          )
        ),
        p(
          style = "color:gray",
          "Please define the temporal type and format for each selected column."
        ),
        lapply(seq_len(ncol(data)), function(i) {
          col_name <- colnames(data)[i]
          guess <- guess_time_settings(data[[i]])

          div(
            class = "time_format_card",
            h5(strong("Column: "), span(col_name, style = "color:#05668D")),
            div(
              style = "background:white;border:1px dashed #ccc;padding:8px;margin-bottom:10px;border-radius:4px",
              div(strong("First value as imported: "), code(as.character(data[[i]][which(!is.na(data[[i]]))[1]]))),
              div(strong("Current class: "), code(paste(class(data[[i]]), collapse = "/"))),
              div(strong("Preview values: "), em(time_preview_value(data[[i]])))
            ),
            div(
              style = "display:flex;gap:10px;align-items:flex-end;flex-wrap:wrap",
              pickerInput(
                ns(paste0("time_type_", safe_id(col_name))),
                "Temporal type:",
                choices = c(
                  "Date" = "date",
                  "Date-time" = "datetime",
                  "Time only" = "time",
                  "Year" = "year",
                  "Month" = "month",
                  "Day" = "day"
                ),
                selected = if (identical(guess$type, "keep")) "date" else guess$type,
                width = "180px"
              ),
              pickerInput(
                ns(paste0("time_format_", safe_id(col_name))),
                "Format:",
                choices = c(
                  "Auto / already formatted" = "auto",
                  "YYYY-MM-DD" = "%Y-%m-%d",
                  "DD/MM/YYYY" = "%d/%m/%Y",
                  "DD-MM-YYYY" = "%d-%m-%Y",
                  "MM/DD/YYYY" = "%m/%d/%Y",
                  "YYYY/MM/DD" = "%Y/%m/%d",
                  "YYYY-MM-DD HH:MM:SS" = "%Y-%m-%d %H:%M:%S",
                  "DD/MM/YYYY HH:MM:SS" = "%d/%m/%Y %H:%M:%S",
                  "DD-MM-YYYY HH:MM:SS" = "%d-%m-%Y %H:%M:%S",
                  "YYYY/MM/DD HH:MM:SS" = "%Y/%m/%d %H:%M:%S",
                  "HH:MM:SS" = "%H:%M:%S",
                  "HH:MM" = "%H:%M",
                  "Custom" = "custom"
                ),
                selected = guess$format,
                width = "240px"
              ),
              conditionalPanel(
                condition = paste0("input.time_format_", safe_id(col_name), " == 'custom'"),
                ns = ns,
                textInput(
                  ns(paste0("time_custom_", safe_id(col_name))),
                  "Custom format:",
                  value = guess$custom,
                  placeholder = "e.g. %d/%m/%Y",
                  width = "180px"
                )
              )
            )
          )
        }),
        uiOutput(ns("time_conversion_warning"))
      )
    })

    formatted_time <- reactive({
      req(is_to_time())

      data <- get_data_from()
      out <- data

      for (var in colnames(out)) {
        var_id <- safe_id(var)

        out[[var]] <- convert_time_column(
          x = out[[var]],
          type = input[[paste0("time_type_", var_id)]],
          format = input[[paste0("time_format_", var_id)]],
          custom_format = input[[paste0("time_custom_", var_id)]]
        )
      }

      out
    })

    output$time_conversion_warning <- renderUI({
      req(is_to_time())

      original <- get_data_from()
      converted <- formatted_time()

      warnings <- list()

      for (var in colnames(original)) {
        type_i <- effective_time_type(
          input[[paste0("time_type_", safe_id(var))]],
          input[[paste0("time_format_", safe_id(var))]],
          input[[paste0("time_custom_", safe_id(var))]]
        )

        if (is.null(type_i) || type_i == "keep") {
          next
        }

        original_non_na <- sum(!is.na(original[[var]]))
        converted_non_na <- sum(!is.na(converted[[var]]))

        if (original_non_na > 0 && converted_non_na < original_non_na) {
          warnings[[length(warnings) + 1]] <- paste0(
            "Column '",
            var,
            "' may have been converted incorrectly. ",
            original_non_na - converted_non_na,
            " value(s) became NA."
          )
        }
      }

      if (!length(warnings)) {
        return(
          div(
            style = "color:#2e7d32;margin-top:10px",
            icon("check-circle"),
            " Time settings look valid."
          )
        )
      }

      div(
        class = "alert_warning",
        style = "padding:10px;margin-top:10px",
        div(strong(icon("triangle-exclamation"), " Warning:")),
        lapply(warnings, div)
      )
    })

    validate_time_conversion <- function() {
      if (!is_to_time()) {
        return(TRUE)
      }

      original <- get_data_from()
      converted <- formatted_time()

      for (var in colnames(original)) {
        type_i <- effective_time_type(
          input[[paste0("time_type_", safe_id(var))]],
          input[[paste0("time_format_", safe_id(var))]],
          input[[paste0("time_custom_", safe_id(var))]]
        )

        if (is.null(type_i) || type_i == "keep") {
          next
        }

        original_non_na <- sum(!is.na(original[[var]]))
        converted_non_na <- sum(!is.na(converted[[var]]))

        validate(
          need(
            !(original_non_na > 0 && converted_non_na == 0),
            paste0(
              "Cannot proceed: column '",
              var,
              "' could not be converted. Please check the selected time format."
            )
          )
        )
      }

      TRUE
    }

    numeric_to_numeric <- function() {
      data <- get_data_cutted()

      new_names <- vapply(colnames(data), function(var) {
        input[[paste("newbin", safe_id(var), sep = "_")]] %or% var
      }, character(1))

      colnames(data) <- make.unique(new_names)
      data
    }

    coords_to_numeric <- function() {
      data <- get_data_from()

      new_names <- vapply(colnames(data), function(var) {
        input[[paste("newbin", safe_id(var), sep = "_")]] %or% var
      }, character(1))

      colnames(data) <- make.unique(new_names)
      data
    }

    # Date -> days since 1970-01-01; Date-time -> seconds since 1970-01-01 (UTC);
    # HH:MM(:SS) -> decimal hours. Text/factor columns are parsed only from
    # unambiguous formats (YYYY-MM-DD[ HH:MM[:SS]], YYYY-MM, numbers).
    time_value_to_numeric <- function(x) {
      if (inherits(x, "Date")) return(as.numeric(x))
      if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.numeric(as.POSIXct(x)))
      if (is.numeric(x)) return(as.numeric(x))

      x_chr <- trimws(as.character(x))
      out <- suppressWarnings(as.numeric(x_chr))

      todo <- is.na(out) & !is.na(x_chr)
      dt <- todo & grepl("^\\d{4}-\\d{1,2}-\\d{1,2}[ T]\\d{1,2}:\\d{2}(:\\d{2})?$", x_chr)
      if (any(dt)) {
        dt_chr <- sub("T", " ", x_chr[dt])
        dt_val <- as.POSIXct(dt_chr, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        no_sec <- is.na(dt_val)
        dt_val[no_sec] <- as.POSIXct(dt_chr[no_sec], format = "%Y-%m-%d %H:%M", tz = "UTC")
        out[dt] <- as.numeric(dt_val)
      }

      todo <- is.na(out) & !is.na(x_chr)
      d <- todo & grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", x_chr)
      out[d] <- as.numeric(as.Date(x_chr[d], format = "%Y-%m-%d"))

      todo <- is.na(out) & !is.na(x_chr)
      ym <- todo & grepl("^\\d{4}-\\d{1,2}$", x_chr)
      out[ym] <- as.numeric(as.Date(paste0(x_chr[ym], "-01"), format = "%Y-%m-%d"))

      todo <- is.na(out) & !is.na(x_chr)
      hm <- todo & grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", x_chr)
      if (any(hm)) {
        parts <- strsplit(x_chr[hm], ":")
        out[hm] <- vapply(parts, function(p) {
          p <- as.numeric(p)
          if (p[1] > 23 || p[2] > 59 || (length(p) == 3 && p[3] > 59)) return(NA_real_)
          p[1] + p[2] / 60 + (if (length(p) == 3) p[3] / 3600 else 0)
        }, numeric(1))
      }

      out
    }

    time_to_numeric <- function() {
      data <- get_data_from()

      data <- data.frame(
        lapply(data, time_value_to_numeric),
        check.names = FALSE
      )

      rownames(data) <- rownames(get_data_from())

      source_data <- get_data_from()
      failed <- vapply(colnames(data), function(var) {
        x <- source_data[[var]]
        sum(is.na(data[[var]]) & !is.na(x) & nzchar(trimws(as.character(x))))
      }, numeric(1))
      if (any(failed > 0)) {
        showNotification(
          paste0(
            "Some temporal values could not be converted and were set to NA (",
            paste0(names(failed)[failed > 0], ": ", failed[failed > 0], collapse = "; "),
            "). Use an unambiguous format (YYYY-MM-DD) or format the column as Date in the Databank."
          ),
          type = "warning",
          duration = 12
        )
      }

      new_names <- vapply(colnames(data), function(var) {
        input[[paste("newbin", safe_id(var), sep = "_")]] %or% var
      }, character(1))

      colnames(data) <- make.unique(new_names)
      data
    }

    numeric_to_coords <- function() {
      data <- get_data_from()
      req(ncol(data) <= 2)

      data <- data.frame(lapply(data, function(x) suppressWarnings(as.numeric(as.character(x)))), check.names = FALSE)
      rownames(data) <- rownames(get_data_from())

      validate(
        need(
          ncol(data) %in% c(1, 2),
          "Coords-Attribute must receive one or two numeric columns."
        ),
        need(
          all(vapply(data, is.numeric, logical(1))),
          "Coords-Attribute columns must be numeric."
        )
      )

      if (ncol(data) == 1) {
        colnames(data) <- "x"
      } else {
        colnames(data) <- c("x", "y")
      }

      data
    }

    numeric_to_time <- function() {
      validate_time_conversion()
      formatted_time()
    }

    factor_to_factor <- function() {
      data <- get_data_cutted()
      rd <- rank_dfs()

      result <- lapply(names(rd), function(var) {
        levels_var <- levels(data[, var])
        var_id <- safe_id(var)

        ordered_levels <- rd[[var]]$level
        ordered_levels <- ordered_levels[ordered_levels %in% seq_along(levels_var)]

        if (!length(ordered_levels)) {
          ordered_levels <- seq_along(levels_var)
        }

        labels <- rd[[var]]$label
        labels <- labels[ordered_levels]

        numeric_factor <- factor(as.numeric(data[, var]))
        new_factor <- factor(numeric_factor, levels = ordered_levels, labels = labels)
        new_factor <- factor(new_factor, levels = levels(new_factor)[levels(new_factor) %in% new_factor])

        out <- data.frame(new_factor, check.names = FALSE)
        colnames(out) <- input[[paste0("newcol_", var_id)]] %or% var
        out
      })

      result <- data.frame(result, check.names = FALSE)
      rownames(result) <- rownames(get_data_from())
      result
    }

    factor_to_numeric_binary <- function() {
      data <- get_data_cutted()

      result <- capture_log2(function(data, input) {
        df <- data.frame(do.call(cbind, lapply(data, classvec2classmat)), check.names = FALSE)
        cols <- colnames(getclassmat(data))

        new_names <- vapply(cols, function(var) {
          input[[paste("newbin", safe_id(var), sep = "_")]] %or% var
        }, character(1))

        rownames(df) <- rownames(data)
        colnames(df) <- make.unique(new_names)

        df
      })(data, input)

      req(!inherits(result, "error"))
      result
    }

    factor_to_numeric_ordinal <- function() {
      data <- get_data_cutted()

      result <- lapply(colnames(data), function(var) {
        var_id <- safe_id(var)
        levels_var <- levels(data[, var])

        values <- vapply(seq_along(levels_var), function(i) {
          input[[paste0("fac2num_value_", var_id, "_", i)]] %or% i
        }, numeric(1))

        rank_input <- input[[paste("rank_lev", var_id, sep = "_")]]
        order_index <- suppressWarnings(as.integer(rank_input))
        order_index <- order_index[order_index %in% seq_along(levels_var)]

        if (!length(order_index)) {
          order_index <- seq_along(levels_var)
        }

        ordered_levels <- levels_var[order_index]
        ordered_values <- values[order_index]

        as.numeric(as.character(factor(data[, var], levels = ordered_levels, labels = ordered_values)))
      })

      result <- data.frame(result, check.names = FALSE)
      colnames(result) <- colnames(data)
      rownames(result) <- rownames(get_data_from())

      result
    }

    build_exchange_data <- function() {
      if (is_convert("numeric", "numeric")) return(numeric_to_numeric())
      if (is_convert("coords", "numeric")) return(coords_to_numeric())
      if (is_convert("time", "numeric")) return(time_to_numeric())
      if (is_convert("numeric", "coords")) return(numeric_to_coords())
      if (is_to_time()) return(numeric_to_time())
      if (is_convert("numeric", "factor")) return(factor_to_factor())
      if (is_convert("time", "factor")) return(factor_to_factor())

      if (is_convert("factor", "numeric")) {
        req(input$hand_facs)

        if (input$hand_facs == "Ordinal") {
          return(factor_to_numeric_ordinal())
        }

        return(factor_to_numeric_binary())
      }

      if (is_convert("factor", "factor")) return(factor_to_factor())

      NULL
    }

    action_label <- reactive({
      vars <- inline(
        div(
          style = "max-width:200px;color:brown;background:Gainsboro",
          em(paste0(input$importvar, collapse = "; "))
        )
      )

      suffix <- NULL

      if (is_convert("numeric", "factor")) suffix <- span("(as factor)", style = "color:#05668D;")
      if (is_convert("factor", "numeric")) {
        suffix <- span(
          if (input$hand_facs == "Binary") "(as binary columns)" else "(as integer level values)",
          style = "color:#05668D;"
        )
      }
      if (is_convert("numeric", "coords")) suffix <- span("(replace destination coordinates)", style = "color:#B36B00;")
      if (is_to_time()) suffix <- span("(as temporal columns)", style = "color:#05668D;")
      if (is_convert("time", "factor")) suffix <- span("(dates as factor levels, in chronological order)", style = "color:#05668D;")
      if (is_convert("coords", "numeric")) suffix <- span("(coordinates as numeric columns)", style = "color:#05668D;")
      if (is_convert("time", "numeric")) suffix <- span("(dates as days since 1970-01-01, date-times as seconds, HH:MM as decimal hours)", style = "color:#05668D;")

      div(
        h4(
          span(
            input$copy_transfer,
            embrown(strong(attr_label(input$import_from_attr))),
            "to",
            emgreen(strong(attr_label(input$import_to_attr)))
          )
        ),
        span(strong("Convert")),
        vars,
        suffix
      )
    })

    prepare_destination <- function(to, to_attr, saved_data, exchange_data, replace = FALSE) {
      target <- saved_data[[to]]
      req(is.data.frame(exchange_data))

      if (to_attr == "factor") {
        current <- attr(target, "factors")
        if (is.null(current)) current <- data.frame(row.names = rownames(target))
        current <- current[rownames(exchange_data), , drop = FALSE]

        if (isTRUE(replace)) {
          current[colnames(exchange_data)] <- exchange_data
          result <- current
        } else {
          result <- cbind(current, exchange_data)
        }

        colnames(result) <- make.unique(colnames(result))
        return(result)
      }

      if (to_attr == "coords") {
        return(exchange_data)
      }

      if (to_attr == "time") {
        current <- attr(target, "time")
        if (is.null(current)) current <- data.frame(row.names = rownames(target))
        current <- current[rownames(exchange_data), , drop = FALSE]

        if (isTRUE(replace)) {
          current[colnames(exchange_data)] <- exchange_data
          result <- current
        } else {
          result <- cbind(current, exchange_data)
        }

        colnames(result) <- make.unique(colnames(result))
        return(result)
      }

      current <- target[rownames(exchange_data), , drop = FALSE]

      if (isTRUE(replace)) {
        current[colnames(exchange_data)] <- exchange_data
        result <- current
      } else {
        result <- cbind(current, exchange_data)
      }

      colnames(result) <- make.unique(colnames(result))
      result <- data_migrate(target, result)

      copy_attrs <- c(
        "base_shape", "layer_shape", "extra_shape", "coords", "scale",
        "transf", "filename", "datalist", "datalist_root", "factors", "time"
      )

      for (att in copy_attrs) {
        attr(result, att) <- attr(target, att)
      }

      result
    }

    prepare_and_confirm_exchange <- function() {
      result <- tryCatch({
        req(isTRUE(valid_exchange()))

        exchange_data <- build_exchange_data()
        req(is.data.frame(exchange_data))

        r_exchange(exchange_data)

        destination <- capture_log2(prepare_destination)(
          to = input$import_to_data,
          to_attr = input$import_to_attr,
          saved_data = vals$saved_data,
          exchange_data = exchange_data,
          replace = isTRUE(input$replace)
        )

        vals$error_transf <- attr(destination, "logs")

        if (inherits(destination, "error")) {
          stop(attr(destination, "logs") %or% "Could not prepare destination data.")
        }

        attr(destination, "datalist") <- input$import_to_data
        destination
      }, error = function(e) {
        vals$error_transf <- conditionMessage(e)

        showNotification(
          paste("Exchange failed:", conditionMessage(e)),
          type = "error",
          duration = 8
        )

        NULL
      })

      req(result)

      data2(result)

      confirm_modal(
        session$ns,
        action = action_label(),
        data1 = get_data_from(),
        data2 = result,
        left = "Original Data:",
        right = "New Data:",
        from = "",
        to = ""
      )
    }

    remove_moved_columns <- function(from, from_attr, vars) {
      if (!length(vars)) return()

      if (from_attr == "factor") {
        facs <- attr(vals$saved_data[[from]], "factors")
        if (!is.null(facs)) {
          facs[vars] <- NULL
          attr(vals$saved_data[[from]], "factors") <- facs
        }
        return()
      }

      if (from_attr == "coords") {
        coords <- attr(vals$saved_data[[from]], "coords")
        if (!is.null(coords)) {
          coords[vars] <- NULL
          attr(vals$saved_data[[from]], "coords") <- coords
        }
        return()
      }

      if (from_attr == "time") {
        time <- attr(vals$saved_data[[from]], "time")
        if (!is.null(time)) {
          time[vars] <- NULL
          attr(vals$saved_data[[from]], "time") <- time
        }
        return()
      }

      vals$saved_data[[from]][vars] <- NULL
    }

    run_exchange <- function() {
      from <- input$import_from_data
      to <- input$import_to_data
      from_attr <- input$import_from_attr
      to_attr <- input$import_to_attr
      vars <- input$importvar

      req(from, to, from_attr, to_attr, is.data.frame(data2()))

      if (to_attr == "factor") {
        attr(vals$saved_data[[to]], "factors") <- data2()
      } else if (to_attr == "coords") {
        attr(vals$saved_data[[to]], "coords") <- data2()
      } else if (to_attr == "time") {
        attr(vals$saved_data[[to]], "time") <- data2()
      } else {
        vals$saved_data[[to]] <- data2()
      }

      if (identical(input$copy_transfer, "Move")) {
        if (!identical(from, to) || !identical(from_attr, to_attr)) {
          remove_moved_columns(from, from_attr, vars)
        }
      }

      updatePickerInput(session, "import_from_data", selected = from)
      updatePickerInput(session, "import_to_data", selected = to)
    }

    show_success_modal <- function() {
      req(input$import_to_data, input$import_to_attr)

      new_data <- vals$saved_data[[input$import_to_data]]

      showModal(
        modalDialog(
          easyClose = TRUE,
          h4("Success!"),
          get_basic_compare(
            data1 = NULL,
            data2 = new_data,
            left = "",
            right = "New:",
            to = paste0(input$import_to_data, " > ", attr_label(input$import_to_attr))
          )
        )
      )
    }

    observeEvent(input$confirm, {
      req(is.data.frame(data2()))

      removeModal()
      run_exchange()
      page(1)
      data2(NULL)
      r_exchange(NULL)
      show_success_modal()
    }, ignoreInit = TRUE)
  })
}
# Replace Attributes
tool2_tab4 <- list()

tool2_tab4$ui <- function(id) {

  ns <- NS(id)

  div(
    style = "height: calc(100vh - 100px); width: 100%; overflow-y: scroll",
    class = "pp_input",

    div(strong("Replace Attributes")),

    div(
      class = "half-drop-inline",

      uiOutput(ns("replace_data")),

      selectInput(
        ns("replace_attr"),
        "Attribute:",
        choices = c(
          "Numeric",
          "Factors",
          "Coords",
          "Temporal"
        )
      )
    ),

    div(
      style = "display: flex;",
      class = "large-input",

      div(
        fileInput(
          ns("path"),
          label = "Upload a file:",
          accept = c(".xls", ".xlsx", ".csv")
        ),
        style = "width: 50%"
      ),

      div(
        style = "width: 40%",
        uiOutput(ns("sheet_out"))
      )
    ),

    uiOutput(ns("out"))
  )
}


tool2_tab4$server <- function(id, vals) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$replace_data <- renderUI({
      selectInput(
        session$ns("replace_data"),
        "Datalist:",
        choices = names(vals$saved_data)
      )
    })

    output$sheet_out <- renderUI({

      ns <- session$ns
      path <- input$path$datapath

      req(path)
      req(grepl(".xls", path))

      sheets <- readxl::excel_sheets(path = path)

      selectInput(
        ns("sheet"),
        "Sheet",
        choices = sheets
      )
    })

    observe({

      path <- input$path$datapath

      if (any(grepl(".xls", path))) {
        shinyjs::show("sheet_out")
      } else {
        shinyjs::hide("sheet_out")
      }
    })

    newdata <- reactiveVal()
    war_coords <- reactiveVal()

    first_time_value <- function(x) {

      x <- x[!is.na(x)]

      if (length(x) == 0) {
        return("NA")
      }

      as.character(x[1])
    }

    time_preview_value <- function(x, n = 4) {

      x <- x[!is.na(x)]
      x <- head(x, n)

      if (length(x) == 0) {
        return("NA")
      }

      paste(as.character(x), collapse = ", ")
    }

    formatted_time <- reactive({

      req(input$replace_attr == "Temporal")

      time <- newdata()

      req(time)
      req(is.data.frame(time))

      if (is.null(time) || ncol(time) == 0) {
        return(NULL)
      }

      time_out <- time

      for (i in seq_len(ncol(time_out))) {

        type_i <- input[[paste0("time_type_", i)]]
        format_i <- input[[paste0("time_format_", i)]]
        custom_i <- input[[paste0("time_custom_", i)]]

        time_out[[i]] <- convert_time_column(
          x = time_out[[i]],
          type = type_i,
          format = format_i,
          custom_format = custom_i
        )
      }

      time_out
    })

    output$time_conversion_page <- renderUI({

      req(input$replace_attr == "Temporal")

      time <- newdata()

      req(time)
      req(is.data.frame(time))
      req(ncol(time) > 0)

      div(
        style = "width: 100%; padding: 5px;",

        h4(
          "Format Temporal-Attribute",
          tiphelp_icon(
            icon("fas fa-question-circle"),
            "Check each temporal column and define how iMESc should interpret it. This avoids ambiguous date formats such as day/month/year versus month/day/year."
          )
        ),

        p(
          style = "color: gray;",
          "The Temporal-Attribute was detected. Please define the temporal type and format for each column before replacing the attribute."
        ),

        lapply(seq_len(ncol(time)), function(i) {

          col_name <- colnames(time)[i]

          guess <- guess_time_settings(time[[i]])
          selected_type <- guess$type

          div(
            style = "
              border: 1px solid #ddd;
              border-radius: 6px;
              padding: 10px;
              margin-bottom: 10px;
              background: #fafafa;
            ",

            h5(
              strong("Column: "),
              span(col_name, style = "color: #05668D;")
            ),

            div(
              style = "
                background: white;
                border: 1px dashed #ccc;
                padding: 8px;
                margin-bottom: 10px;
                border-radius: 4px;
              ",

              div(
                strong("First value as imported: "),
                code(first_time_value(time[[i]]))
              ),

              div(
                strong("Current class: "),
                code(paste(class(time[[i]]), collapse = "/"))
              ),

              div(
                strong("Preview values: "),
                em(time_preview_value(time[[i]]))
              )
            ),

            div(
              style = "display: flex; gap: 10px; align-items: flex-end; flex-wrap: wrap;",

              pickerInput(
                session$ns(paste0("time_type_", i)),
                "Temporal type:",
                choices = c(
                  "Keep as imported" = "keep",
                  "Date" = "date",
                  "Date-time" = "datetime",
                  "Time only" = "time",
                  "Year" = "year",
                  "Month" = "month",
                  "Day" = "day"
                ),
                selected = selected_type,
                width = "180px"
              ),

              pickerInput(
                session$ns(paste0("time_format_", i)),
                "Format:",
                choices = c(
                  "Auto / already formatted" = "auto",
                  "YYYY-MM-DD" = "%Y-%m-%d",
                  "DD/MM/YYYY" = "%d/%m/%Y",
                  "DD-MM-YYYY" = "%d-%m-%Y",
                  "MM/DD/YYYY" = "%m/%d/%Y",
                  "YYYY/MM/DD" = "%Y/%m/%d",
                  "YYYY-MM-DD HH:MM:SS" = "%Y-%m-%d %H:%M:%S",
                  "DD/MM/YYYY HH:MM:SS" = "%d/%m/%Y %H:%M:%S",
                  "DD-MM-YYYY HH:MM:SS" = "%d-%m-%Y %H:%M:%S",
                  "YYYY/MM/DD HH:MM:SS" = "%Y/%m/%d %H:%M:%S",
                  "HH:MM:SS" = "%H:%M:%S",
                  "HH:MM" = "%H:%M",
                  "Custom" = "custom"
                ),
                selected = guess$format,
                width = "240px"
              ),

              conditionalPanel(
                condition = paste0("input.time_format_", i, " == 'custom'"),
                ns = session$ns,
                textInput(
                  session$ns(paste0("time_custom_", i)),
                  "Custom format:",
                  value = guess$custom,
                  placeholder = "e.g. %d/%m/%Y",
                  width = "180px"
                )
              )
            )
          )
        }),

        uiOutput(session$ns("time_conversion_warning"))
      )
    })

    output$time_conversion_warning <- renderUI({

      req(input$replace_attr == "Temporal")

      time <- newdata()

      req(time)
      req(is.data.frame(time))
      req(ncol(time) > 0)

      time_out <- formatted_time()

      warnings <- list()

      for (i in seq_len(ncol(time))) {

        type_i <- effective_time_type(
          input[[paste0("time_type_", i)]],
          input[[paste0("time_format_", i)]],
          input[[paste0("time_custom_", i)]]
        )

        if (is.null(type_i) || type_i == "keep") {
          next
        }

        original_non_na <- sum(!is.na(time[[i]]))
        converted_non_na <- sum(!is.na(time_out[[i]]))

        if (original_non_na > 0 && converted_non_na < original_non_na) {

          warnings[[length(warnings) + 1]] <- paste0(
            "Column '",
            colnames(time)[i],
            "' may have been converted incorrectly. ",
            original_non_na - converted_non_na,
            " value(s) became NA. Please check the selected format."
          )
        }
      }

      if (length(warnings) == 0) {
        return(
          div(
            style = "color: #2e7d32; margin-top: 10px;",
            icon("check-circle"),
            " Time settings look valid."
          )
        )
      }

      div(
        class = "alert_warning",
        style = "padding: 10px; margin-top: 10px;",
        div(strong(icon("triangle-exclamation"), " Warning:")),
        lapply(warnings, div)
      )
    })

    validate_time_conversion <- reactive({

      if (input$replace_attr != "Temporal") {
        return(TRUE)
      }

      time <- newdata()

      req(time)
      req(is.data.frame(time))

      time_out <- formatted_time()

      for (i in seq_len(ncol(time))) {

        type_i <- effective_time_type(
          input[[paste0("time_type_", i)]],
          input[[paste0("time_format_", i)]],
          input[[paste0("time_custom_", i)]]
        )

        if (is.null(type_i) || type_i == "keep") {
          next
        }

        original_non_na <- sum(!is.na(time[[i]]))
        converted_non_na <- sum(!is.na(time_out[[i]]))

        validate(
          need(
            !(original_non_na > 0 && converted_non_na < original_non_na),
            paste0(
              "Cannot proceed: column '",
              colnames(time)[i],
              "' may have been converted incorrectly. ",
              original_non_na - converted_non_na,
              " value(s) became NA. Please check the selected time format."
            )
          )
        )
      }

      TRUE
    })

    read_replace_file <- reactive({

      req(input$replace_data)
      req(input$replace_data != "")
      req(input$replace_attr)

      path <- input$path$datapath

      req(path)

      if (grepl(".xls", path)) {
        req(input$sheet)
      }

      attr_to_read <- input$replace_attr

      if (attr_to_read == "Temporal") {
        attr_to_read <- "Time"
      }

      imesc_data(
        path,
        attr = attr_to_read,
        sheet = input$sheet
      )
    })

    check_temporal_data <- function(data, datalist_name) {

      data0 <- vals$saved_data[[datalist_name]]

      if (is.null(data)) {
        return(
          div(
            strong(
              "Invalid Temporal-Attribute. No data found.",
              style = "color: red"
            )
          )
        )
      }

      if (!is.data.frame(data)) {
        data <- data.frame(data)
      }

      validate_ids <- rownames(data0) %in% rownames(data)

      if (!any(validate_ids)) {
        return(
          div(
            style = "overflow-x: scroll;height:180px;overflow-y: scroll",
            div(
              strong("Error", style = "color: red"),
              "None of the IDs of the uploaded Temporal-Attribute are compatible with the IDs of the selected Datalist."
            )
          )
        )
      }

      missing_ids <- rownames(data0)[!rownames(data0) %in% rownames(data)]

      if (length(missing_ids) > 0) {
        return(
          div(
            style = "overflow-x: scroll;height:180px;overflow-y: scroll",
            div(
              strong("Error", style = "color: red"),
              "The IDs below exist in the Numeric-Attribute, but not in the uploaded Temporal-Attribute. Please upload a file containing all IDs of the selected Datalist."
            ),
            renderPrint(data.frame(IDs = missing_ids))
          )
        )
      }

      data[rownames(data0), , drop = FALSE]
    }

    observeEvent(list(input$path$datapath, input$sheet), {

      req(input$replace_data)
      req(input$replace_data != "")
      req(input$replace_attr)

      war_coords(NULL)

      path <- input$path$datapath

      req(path)

      if (grepl(".xls", path)) {
        req(input$sheet)
      }

      data <- read_replace_file()

      if (input$replace_attr == "Coords") {

        if (ncol(data) > 2) {
          war_coords(
            "Warning: The new file contains more than two columns in addition to the ID column. Only the ID column and the first two columns will be processed"
          )
        }
      }

      if (input$replace_attr == "Temporal") {

        data <- check_temporal_data(
          data = data,
          datalist_name = input$replace_data
        )

      } else {

        data <- check_data(
          data,
          input$replace_data,
          input$replace_attr,
          vals$saved_data
        )
      }

      newdata(data)
    })

    observeEvent(newdata(), {

      req(input$replace_data)
      req(input$replace_data != "")
      req(input$replace_attr)

      showModal(
        modalDialog(
          title = NULL,
          size = if (input$replace_attr == "Temporal") "l" else "m",
          footer = div(
            if (inherits(newdata(), "data.frame")) {
              actionButton(
                session$ns("run_replace"),
                "Confirm"
              )
            },
            modalButton("Cancel")
          ),
          easyClose = TRUE,

          if (inherits(newdata(), "shiny.tag")) {

            newdata()

          } else {

            div(
              h4(
                strong("Replace"),
                emgreen(paste0(input$replace_attr, "-Attribute")),
                "in",
                emgreen(input$replace_data),
                strong("with the new data?")
              ),

              if (input$replace_attr == "Temporal") {
                uiOutput(session$ns("time_conversion_page"))
              },

              uiOutput(session$ns("success_replace"))
            )
          }
        )
      )
    })

    data_replace <- reactive({

      req(input$replace_data)
      req(input$replace_data != "")
      req(input$replace_data %in% names(vals$saved_data))

      data <- vals$saved_data[[input$replace_data]]

      attr_name <- tolower(input$replace_attr)

      if (attr_name == "temporal") {
        attr_name <- "time"
      }

      if (attr_name %in% c("factors", "coords", "time")) {
        data <- attr(data, attr_name)
      }

      data
    })

    get_new_temporal <- reactive({

      req(input$replace_attr == "Temporal")

      time <- formatted_time()

      req(time)

      time
    })

    get_new <- reactive({

      req(newdata())
      req(input$replace_data)
      req(input$replace_data != "")
      req(input$replace_data %in% names(vals$saved_data))

      data0 <- vals$saved_data[[input$replace_data]]

      attr_name <- tolower(input$replace_attr)

      if (attr_name == "temporal") {

        attr(data0, "time") <- get_new_temporal()
        new <- data0

      } else if (attr_name %in% c("factors", "coords")) {

        attr(data0, attr_name) <- newdata()
        new <- data0

      } else {

        new <- data_migrate(data0, newdata())
      }

      new
    })

    output$success_replace <- renderUI({

      style_numeric <- ""
      style_factor <- ""
      style_coords <- ""
      style_time <- ""

      style0 <- "color: #007bff; font-weight: bold"

      if (input$replace_attr == "Numeric") {

        style_numeric <- style0
        name <- "Numeric-Attribute:"

      } else if (input$replace_attr == "Factors") {

        style_factor <- style0
        name <- "Factor-Attribute:"

      } else if (input$replace_attr == "Coords") {

        style_coords <- style0
        name <- "Coords-Attribute:"

      } else {

        style_time <- style0
        name <- "Temporal-Attribute:"
      }

      if (input$replace_attr == "Temporal") {

        div(
          style = "",

          splitLayout(
            cellWidths = c("47%", "5%", "47%"),

            div(
              style = "background: #FFF0F5",
              div(
                strong("Data to be replaced:"),
                style = "color: brown"
              ),
              basic_summary2(
                data_replace(),
                name = name
              )
            ),

            div(
              icon(
                "arrow-right",
                style = "color: #007bff; font-size: 24px;"
              )
            ),

            div(
              style = "background: #F0FFF0",
              div(
                strong("New:"),
                style = "color: #007bff"
              ),
              basic_summary2(
                get_new_temporal(),
                name = name
              )
            )
          )
        )

      } else {

        div(
          style = "",

          splitLayout(
            cellWidths = c("47%", "5%", "47%"),

            div(
              style = "background: #FFF0F5",
              div(
                strong("Data to be replaced:"),
                style = "color: brown"
              ),
              basic_summary2(
                data_replace(),
                name = name
              )
            ),

            div(
              icon(
                "arrow-right",
                style = "color: #007bff; font-size: 24px;"
              )
            ),

            div(
              style = "background: #F0FFF0",
              div(
                strong("New:"),
                style = "color: #007bff"
              ),
              basic_summary2(
                get_new(),
                style_numeric,
                style_factor,
                style_coords
              )
            )
          )
        )
      }
    })

    observeEvent(input$run_replace, ignoreInit = TRUE, {

      req(input$replace_data)
      req(input$replace_data != "")
      req(input$replace_data %in% names(vals$saved_data))

      if (input$replace_attr == "Temporal") {
        validate_time_conversion()
      }

      new <- get_new()

      vals$saved_data[[input$replace_data]] <- new

      removeModal()

      shinyjs::reset("path")
      shinyjs::hide("sheet_out")

      newdata(NULL)

      done_modal()
    })

    observeEvent(ignoreInit = TRUE, input$newfactorfile, {

      req(input$newfactorfile)

      vals$newfactors_att <- NULL

      req(length(input$newfactorfile$datapath) != 0)

      req(input$newfac_targ)
      req(input$newfac_targ %in% names(vals$saved_data))

      data <- vals$saved_data[[input$newfac_targ]]

      labels <- data.frame(
        data.table::fread(
          input$newfactorfile$datapath,
          stringsAsFactors = TRUE,
          na.strings = c("", "NA"),
          header = TRUE
        )
      )

      rownames(labels) <- labels[, 1]
      labels[, 1] <- NULL

      labels[which(unlist(lapply(labels, is.numeric)))] <-
        do.call(
          "data.frame",
          lapply(
            labels[which(unlist(lapply(labels, is.numeric)))],
            function(x) as.factor(x)
          )
        )

      data <- vals$saved_data[[input$newfac_targ]]

      factors_in <- labels[rownames(data), , drop = FALSE]

      notfoundnew <- which(!rownames(data) %in% na.omit(rownames(factors_in)))

      if (length(notfoundnew) > 0) {

        output$newfactors_missing <- renderUI({
          div(
            style = "overflow-x: scroll;height:180px;overflow-y: scroll",
            div(
              strong("Error", style = "color: red"),
              "The IDs below exist in the Data-Attribute, but not in the",
              strong("Factor-Attribute."),
              "Please upload a new file that contains IDs compatible with the selected Datalist."
            ),
            renderPrint(data.frame(IDs = rownames(data)[notfoundnew]))
          )
        })

      } else {

        output$newfactors_missing <- renderUI(NULL)
      }

      req(!length(notfoundnew) > 0)

      vals$newfactors_att <- labels
    })
  })
}

# Edit columns
tool2_tab5<-list()
tool2_tab5$ui<-function(id){
  ns<-NS(id)
  tabsetPanel(
    tabPanel(strong("Edit columns"),
             tags$style(HTML(
               "
               .editdata .train_box .virtual-select{
               min-width: 100%
               }
               "
             )),
             div(class="editdata",
                 column(12,class="train_box inline_pickers",style="width: 100%",
                        div(
                          div(
                            uiOutput(ns('editdata')),

                            pickerInput(ns("editattr"),strong("Attribute:"),choices=c("Numeric","Factors")),

                            div(style="display: flex",
                                uiOutput(ns("remove_columns")),

                                div(style="position: absolute;right: 30px",
                                    div(id=ns("trash_cols_btn"),class="save_changes",style="margin-top:5px;",
                                        actionButton(ns("trash_cols"),icon("fas fa-trash"))
                                    ),
                                    uiOutput(ns('validate_remove'))
                                )

                            ),


                            column(12,div(style="display: flex; align-items: flex-start;gap: 10px",
                                          div(
                                            style="min-width: 200px",
                                            tags$label("Edit names:",tipright("Double-click the variable names to edit them:"),style="color: #05668D;"),
                                            div(
                                              class="half-drop-inline",
                                              DT::DTOutput(ns("tabcolnames"))
                                            )
                                          ),
                                          div(id=ns("editdat_go_btn"),
                                              tipify_ui(actionButton(ns("editdat_go"),icon("fas fa-save"),style="margin-top: 25px"),"Save new names")
                                          ),
                                          tipify_ui(actionButton(ns("reset"),icon("undo"),style="margin-top: 25px"),"Reset")
                            ))

                          ),
                          uiOutput(ns("teste"))

                        )

                 )
             )
    ),

    tabPanel("Concatenate Factors",
             div(id=ns("show_conc_out"),
                 fluidRow(class="train_box inline_pickers",

                          column(12,
                                 column(6,class="mp0",
                                        uiOutput(ns('datalist_conc')),
                                        uiOutput(ns('concac_factors')),
                                        textInput(ns("collapse"),span("Collapse", tiphelp("Specify a character to use as a separator when concatenating the selected columns.")),"-"),

                                        checkboxInput(ns('col_as_pref'),span("Use column names as prefix:", tiphelp("Enable this option to use column names as prefixes for the concatenated values.")),F),
                                        div(style="padding-left: 15px",
                                            uiOutput(ns('col_suff'))
                                        ),
                                        span("Add Prefix/Suffix", tiphelp("Enable this option to add custom prefixes or suffixes to the concatenated values.")),
                                        div(style="padding-left: 15px",
                                            textInput(ns("prefix"),"Prefix:",""),
                                            textInput(ns("suffix"),"Suffix:","")
                                        ),
                                        checkboxInput(ns("add_replace"),span("Replace pattern", tiphelp("Enable this option to replace patterns in the concatenated values.")),F),
                                        div(style="padding-left: 15px",
                                            textInput(ns("gsub_pattern"),span("Pattern:", tiphelp("Enter the pattern to search for within the concatenated values.")),""),
                                            textInput(ns("gsub_by"),span("Replacement:", tiphelp("Enter the replacement text for the specified pattern.")),""),
                                        ),

                                        uiOutput(ns('newcolumn')),



                                 ),
                                 column(6,class="mp0",
                                        div(
                                          style="display: flex",
                                          div(id=ns("concatane_go_btn"),
                                              class="save_changes",
                                              actionButton(ns("concatane_go"),"concatenate",icon=icon("code-merge"),style="margin-top: 25px")
                                          ),
                                          div(id=ns("concatane_save_btn"),
                                              class="save_changes",
                                              style="display:none",
                                              actionButton(ns("concatane_save"),icon("fas fa-save"),style="margin-top: 25px")
                                          )
                                        ),
                                        uiOutput(ns("preview_conc")))

                          )
                 )
             ))
  )
}
tool2_tab5$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns

    data_edit<-reactive({
      req(input$editdata)
      data<-vals$saved_data[[input$editdata]]
      req(input$editattr)
      data<-switch(input$editattr,
                   'Numeric'=data,
                   "Factors"=attr(data,"factors"))
    })

    observeEvent(data_edit(),{
      shinyjs::removeClass('editdat_go_btn',"save_changes")

    })

    output$remove_columns<-renderUI({
      req(input$editdata)
      data<-vals$saved_data[[input$editdata]]
      req(input$editattr)
      data<-switch(input$editattr,
                   'Numeric'=data,
                   "Factors"=attr(data,"factors"))
      choices=colnames(data)
      div(

        div(
          shinyWidgets::virtualSelectInput(
            ns("remove_columns"),
            strong("Remove Columns:",tipright("Select and remove columns")),choices=choices,multiple = T,  search =T,
            optionHeight='24px',position="bottom",   optionsSelectedText="Columns selected",

            alwaysShowSelectedOptionsCount=T,
            optionSelectedText="Columns selected"
          )
        ),

      )
    })

    output$validate_remove<-renderUI({
      req(isTRUE(validate_remove()))
      div(style="width: 150px;margin-left: 0px;white-space: normal; background: white",
          embrown("Removing all columns from Factor/Numeric attribute is not allowed")
      )

    })



    validate_remove<-reactive({
      req(input$editdata)
      data0<-data<-vals$saved_data[[input$editdata]]
      req(input$editattr)
      data<-switch(input$editattr,
                   'Numeric'=data,
                   "Factors"=attr(data,"factors"))
      length(input$remove_columns)==ncol(data)
    })
    observeEvent(input$trash_cols,ignoreInit = T,{

      confirm_modal(session$ns,action=h4("Are you sure?"),
                    data1=NULL,
                    data2= NULL,
                    arrow=F,
                    left=paste("Remove", 'selected columns from the',paste0(input$editattr,"-Attribute")),
                    right="",
                    from=renderPrint(input$remove_columns),
                    to='')
    })

    observeEvent(input$confirm,ignoreInit = T,{
      removeModal()
      data0<-data<-vals$saved_data[[input$editdata]]
      req(input$editattr)
      data<-switch(input$editattr,
                   'Numeric'=data,
                   "Factors"=attr(data,"factors"))
      data[,input$remove_columns]<-NULL
      if(input$editattr=="Numeric"){
        data<-data_migrate(data0,data)
        vals$saved_data[[input$editdata]]<-data
      } else{
        attr(vals$saved_data[[input$editdata]],"factors")<-data
      }

    })

    observe({
      shinyjs::toggle('trash_cols_btn',condition=length(input$remove_columns)>0&isFALSE(validate_remove()))
      shinyjs::toggleClass('trash_cols_btn','save_changes',condition=length(input$remove_columns)>0)

      shinyjs::toggle('col_suff',condition=isTRUE(input$col_as_pref))

      shinyjs::toggle('gsub_pattern',condition=isTRUE(input$add_replace))
      shinyjs::toggle('gsub_by',condition=isTRUE(input$add_replace))

      shinyjs::toggle('prefix',condition=isTRUE(input$add_prefsuf))
      shinyjs::toggle('suffix',condition=isTRUE(input$add_prefsuf))
    })
    observeEvent(input$save_bug,{
      saveRDS(reactiveValuesToList(input),"input.rds")
    })


    ##

    observeEvent(ignoreInit = T,input$datalist_conc,{
      vals$datalist_conc<-input$datalist_conc
    })

    factors_to_conc<-function(){


      req(input$datalist_conc%in%names(vals$saved_data))
      data<-vals$saved_data[[input$datalist_conc]]
      factors<-attr(data,"factors")
      req(input$conc_cols%in%colnames(factors))
      #validate(need(length(input$conc_cols)>1,"Select two or more columns to concatanate"))
      fac<-factors[,input$conc_cols,drop=F]
      fac
    }

    output$newcolumn<-renderUI({

      value=paste(colnames(factors_to_conc()),collapse  ="_")
      textInput(ns("newcolumn_name"),"New column name:",value=value)
    })


    output$datalist_conc<-renderUI({
      pickerInput_fromtop(ns("datalist_conc"),span("Datalist", tiphelp("Select the Datalist containing the columns to concatenate.")),choices=names(vals$saved_data),selected = vals$datalist_conc)
    })



    output$concac_factors<-renderUI({
      req(input$datalist_conc)
      data<-vals$saved_data[[input$datalist_conc]]
      factors<-attr(data,"factors")
      choices=colnames(factors)
      shinyWidgets::virtualSelectInput(ns("conc_cols"),span("Columns:",tiphelp("Select the columns to concatenate")),choices=choices, selected=choices[1],multiple = T,position="bottom",    search =T,    optionHeight='24px')
    })

    output$col_suff<-renderUI({
      req(input$datalist_conc)
      fac<-factors_to_conc()

      choices=colnames(fac)
      pickerInput_fromtop(ns("suff_cols"),span("Use:", tiphelp("Choose which columns will be used as level prefixes when concatenating.")),T, multiple = T,choices=choices,selected=choices[1])
    })


    observeEvent(input$factors_conc,{
      shinyjs::addClass("concatane_go_btn", "save_changes")
    })
    # input$datalist_conc<-'Coords+Depth_scaled'
    # input$conc_cols<-c('superSOM (1)_HC8',"cruise")
    concatanete_factors<-function(data,prefix="[",suffix="]",collapse=",",suff_cols=colnames(data)[1],gsub_pattern="",gsub_by=""){
      pref_cols<-colnames(data)
      pref_cols[!colnames(data)%in%suff_cols]<-"no_preff_"
      pref_cols<-as.list(pref_cols)
      result<-apply( data ,1,function(x){
        n<-paste(pref_cols,x,sep = collapse)
        labels<-gsub(paste0('no_preff_',collapse),"",n)
        paste0(paste(labels,collapse = collapse))
      })
      if(prefix!="")
        result<-paste(prefix,as.character(result),sep = collapse)
      if(suffix!="")
        result<-paste(result,suffix,sep = collapse)
      result<-gsub(gsub_pattern,
                   gsub_by,result)
      result
    }
    observeEvent(input$concatane_save,ignoreInit = T,{
      data<-vals$saved_data[[input$datalist_conc]]
      factors<-attr(data,"factors")
      newfac<-run_concate()
      factors<-cbind(factors,newfac)
      colnames(factors)<-make.unique(colnames(factors))
      attr(vals$saved_data[[input$datalist_conc]],"factors")<-factors
      shinyjs::removeClass("concatane_save_btn","save_changes")
      run_concate(NULL)
      updateCheckboxInput(session,"show_conc",value=F)
      done_modal(
        emgray("A new column named "),
        strong(emgreen(input$newcolumn_name)),
        emgray(" was created in the Factor-Attribute of the Datalist "),
        strong(emgreen(input$datalist_conc))
      )
    })

    args_conc<-reactive({
      fac<-factors_to_conc()
      gsub_by=input$gsub_by
      if(isFALSE(input$col_as_pref)){
        suff_cols<-NULL
      }

      list(
        fac=factors_to_conc(),
        add_prefsuf=input$add_prefsuf,
        add_replace=input$add_replace,
        suffix=input$suffix,
        prefix=input$prefix,
        conc_cols=input$conc_cols,
        collapse=input$collapse,
        data_levels=expand.grid(lapply(fac,levels)),
        suff_cols=input$suff_cols,
        gsub_pattern=input$gsub_pattern,
        gsub_by=gsub_by

      )})

    observeEvent(args_conc(),{
      shinyjs::addClass("concatane_go_btn", "save_changes")
      run_concate(NULL)
      shinyjs::hide('concatane_save_btn')
    })
    run_concate<-reactiveVal()
    observeEvent(input$concatane_go,{
      req(input$datalist_conc)
      req(input$newcolumn_name)
      fac<-factors_to_conc()
      suffix=input$suffix
      prefix=input$prefix
      conc_cols=input$conc_cols
      data_levels<-expand.grid(lapply(fac,levels))
      suff_cols=input$suff_cols
      gsub_pattern=input$gsub_pattern
      gsub_by=input$gsub_by
      if(isFALSE(input$col_as_pref)){
        suff_cols<-NULL
      }
      prefix = input$prefix
      suffix = input$suffix
      if(isFALSE(input$add_prefsuf)){
        prefix<-""
        suffix<-""
      }
      if(isFALSE(input$add_replace)){
        gsub_pattern<-""
        gsub_by<-""
      }



      levels<-concatanete_factors(data_levels,
                                  prefix = prefix,
                                  suffix = suffix,
                                  collapse=input$collapse,
                                  suff_cols=suff_cols,
                                  gsub_pattern=gsub_pattern,
                                  gsub_by=gsub_by
      )
      newfac<-concatanete_factors(fac,
                                  prefix = prefix,
                                  suffix = suffix,
                                  collapse=input$collapse,
                                  suff_cols=suff_cols,
                                  gsub_pattern=gsub_pattern,
                                  gsub_by=gsub_by
      )
      newfac<-data.frame(factor(newfac,levels=levels))
      colnames(newfac)<-input$newcolumn_name
      shinyjs::removeClass("concatane_go_btn", "save_changes")
      shinyjs::show('concatane_save_btn')
      run_concate(newfac)

    })
    output$preview_conc<-renderUI({
      req(run_concate())

      div(

        head_data$ui(session$ns("table-conc"),1:3),
        head_data$server('table-conc',run_concate())

      )

    })

    ##
    observeEvent(ignoreInit = T,input$editdata,{
      vals$previousSelection<-NULL
      vals$previousPage<-NULL
    })
    observeEvent(ignoreInit = T,input$editattr,{
      vals$editattr<-input$editattr
    })
    observeEvent(ignoreInit = T,input$editdata,{
      vals$cur_data<-input$editdata
    })

    output$editdata<-renderUI({
      selectInput(ns("editdata"),strong("Datalist:"),choices=names(vals$saved_data),selected = vals$cur_data)
    })
    # output read checkboxes

    delcol_values<-reactiveValues()




    output$tabcolnames<-{DT::renderDT(
      data.frame(Var_name=delcol_values$tab),
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
      delcol_values$tab<-data.frame(datavars)




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

    observeEvent(delcol_values$tab$Var_name,{
      req(input$editdata)

      condition<-!all(colnames(data_edit())%in%delcol_values$tab$Var_name)
      shinyjs::toggleClass("editdat_go_btn",class="save_changes",condition=condition)

    })
  })



}

# Rename Saved Models
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

#Transpose Datalist
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

# Shapefiles toolbox
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
                        column(2,align="right",class="mp0",div(style="margin-top: 8px; ",tipify_ui(actionLink(ns("trash_open"),icon('trash')),"Remove Current Shape-Attribute")))

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

    reset_shape_state <- function(){
      shp_step(1)
      shape1_raw(NULL)
      shape2_prep(NULL)
      shape3_filtered(NULL)
      shape4_final(NULL)
      plot_step1(NULL)
      plot1_prepare(NULL)
      plot_ready(F)
      vals$shp_show_layers<-input$shp_include
      shinyjs::reset('shp')
      shinyjs::hide('read_shp_btn')
      shinyjs::hide('crop_shapes')
      shinyjs::hide('filter_crop')
    }

    update_saved_shape <- function(datalist, shape_attr, shape, extra_name = NULL){
      req(datalist)
      req(datalist %in% names(vals$saved_data))
      req(shape_attr)

      dat <- vals$saved_data[[datalist]]

      if (shape_attr == "extra_shape") {
        extra_shapes <- attr(dat, "extra_shape")

        if (is.null(extra_shapes) || !is.list(extra_shapes)) {
          extra_shapes <- list()
        }

        if (is.null(extra_name) || !nzchar(trimws(extra_name))) {
          extra_name <- "Extra-Layer"
        }

        extra_names <- make.unique(c(names(extra_shapes), trimws(extra_name)))
        extra_shapes[[extra_names[length(extra_names)]]] <- shape
        attr(dat, "extra_shape") <- extra_shapes
      } else {
        attr(dat, shape_attr) <- shape
      }

      vals$saved_data[[datalist]] <- dat
      invisible(dat)
    }

    remove_saved_shape <- function(datalist, shape_attr){
      req(datalist)
      req(datalist %in% names(vals$saved_data))
      req(shape_attr)

      dat <- vals$saved_data[[datalist]]
      attr(dat, shape_attr) <- NULL
      vals$saved_data[[datalist]] <- dat
      invisible(dat)
    }

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
      req(shape_list_down())
      plotOutput(ns("shape_view_plot"), height = "250px")
    })

    output$shape_view_plot<-renderPlot({
      shl<-shape_list_down()
      req(input$shp_data_view_down)
      req(input$shp_attr_view_down)
      shape<-shl[[input$shp_attr_view_down]]
      req(length(shape)>0)
      ggplot(st_as_sf(shape)) + geom_sf()+
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(fill=NA,color="black", linewidth=0.5, linetype="solid"))
    })

    output$download_shape<-downloadHandler(
      filename = function() {
        paste0(paste0(input$shp_attr_view_down,"_",input$shp_data_view_down),"_", Sys.Date())
      }, content = function(file) {
        shl<-shape_list_down()
        shape<-shl[[input$shp_attr_view_down]]
        saveRDS(shape,file)
      })

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

      reset_shape_state()

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
      req(input$shp$datapath)
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
        req(lims1)
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
      remove_saved_shape(input$shp_datalist, input$shp_include)
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
      req(input$shp_feature1!="None")
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
      choices<-c(base,layer,extra)
      pic<-which(choices)
      pic
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
      update_saved_shape(
        datalist = input$shp_datalist,
        shape_attr = input$shp_include,
        shape = shape,
        extra_name = input$extra_layer_newname
      )

      reset_shape_state()
      shinyjs::removeClass("add_shape_btn","save_changes")

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
    output$save_feature<-downloadHandler(
      filename = function() {
        paste0("feature_shape","_", Sys.Date())
      }, content = function(file) {
        req(shape4_final())
        saveRDS(shape4_final(),file)
      })
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

      p<-plotly::ggplotly( p, source = "A")
      p<-plotly::layout(p, legend = list(x = 0, y = 0))
      p<-plotly::config(p, scrollZoom = TRUE)
      p<-plotly::event_register(p,'plotly_relayout')
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
      req("Custom" %in% input$crop_shapes)

      d <- plotly::event_data("plotly_relayout", source = "A")

      req(!is.null(d))
      req(length(d) == 4)

      xmin <- d[[1]]
      xmax <- d[[2]]
      ymin <- d[[3]]
      ymax <- d[[4]]

      req(is.numeric(xmin))
      req(is.numeric(xmax))
      req(is.numeric(ymin))
      req(is.numeric(ymax))

      rect_coords(list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))

      proxy<-plotly::plotlyProxy("full_shape_plot", session = session)
      plotly::plotlyProxyInvoke(proxy, "relayout", list(
          shapes = list(
            list(
              type = "rect",
              x0 = xmin,
              x1 = xmax,
              y0 = ymin,
              y1 = ymax,
              line = list(color = "red", dash = "dash", width = 4)
            )
          )
        ))
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
          plotOutput(ns("final_shape_plot_preview"), width = "300px", height = "150px")
      )
    })

    output$final_shape_plot_preview<-renderPlot({
      shp<-shape4_final()
      req(shp)
      req(nrow(shp)>0)
      plot_shape(shp)
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

## Edit Temporal Attribute
## Derive lower-resolution temporal columns from an existing Temporal-Attribute.
tool2_tab9 <- list()
tool2_tab9$ui <- function(id){

  ns <- NS(id)

  div(
    class = "tool2_tab9",

    div(
      style = "display: flex;",
      class = "half-drop-inline",

      div(
        style = "background-color: #f5f5f5; width: 50%; padding: 5px;",
        class = "half-drop",

        div(
          strong("Edit Temporal-Attribute:"),
          tiphelp(
            HTML(
              "<div>
                <p>
                  Create new temporal columns by changing the resolution of an existing Temporal-Attribute.
                  iMESc keeps the result as a temporal variable, using a representative date for each resolution:
                </p>
                <ul style='padding-left: 18px; margin-bottom: 6px;'>
                  <li><strong>Year:</strong> first day of the year.</li>
                  <li><strong>Month:</strong> first day of the month.</li>
                  <li><strong>Week:</strong> Monday of that week.</li>
                  <li><strong>Quarter:</strong> first day of the quarter.</li>
                  <li><strong>Semester:</strong> first day of the semester.</li>
                  <li><strong>Season:</strong> first day of the corresponding Southern Hemisphere season.</li>
                </ul>
                <p>
                  The original temporal column is preserved, and the new column is added to the Temporal-Attribute.
                </p>
              </div>"
            )
          )
        ),

        pickerInput_fromtop_live(ns("data_x"), "Datalist:", choices = NULL),

        pickerInput_fromtop_live(
          ns("time_column"),
          "Temporal column:",
          choices = NULL
        ),

        pickerInput(
          ns("time_operation"),
          "Convert resolution to:",
          choices = c(
            "Day" = "day",
            "Week" = "week",
            "Month" = "month",
            "Quarter" = "quarter",
            "Semester" = "semester",
            "Year" = "year",
            "Southern hemisphere season" = "season_south"
          ),
          selected = "year"
        ),

        textInput(
          ns("new_time_name"),
          "New column name:",
          value = "time_year"
        ),

        checkboxInput(
          ns("replace_existing"),
          "Replace column if name already exists",
          value = TRUE
        ),

        div(
          id = ns("run_time_edit_btn"),
          align = "right",
          class = "save_changes",

          em(
            "Click to apply",
            icon("fas fa-hand-point-right")
          ),

          actionButton(
            ns("run_time_edit"),
            span("RUN", icon("clock"))
          )
        )
      ),

      div(
        style = "background: white; padding: 15px; width: 275px;",
        uiOutput(ns("summ_time_edit")),
        uiOutput(ns("preview_time_edit"))
      )
    ),

    div(
      style = "background: white; padding: 15px;",
      class = "half-drop-inline",
      uiOutput(ns("war_time_edit"))
    )
  )
}
tool2_tab9$server <- function(id, vals){

  moduleServer(id, function(input, output, session){

    r_time_edit <- reactiveVal(NULL)
    last_message <- reactiveVal(NULL)

    reset_pending <- function(clear_message = TRUE){
      r_time_edit(NULL)

      if (isTRUE(clear_message)) {
        last_message(NULL)
      }

      shinyjs::addClass("run_time_edit_btn", "save_changes")
    }

    data <- reactive({
      req(input$data_x)
      req(input$data_x %in% names(vals$saved_data))
      vals$saved_data[[input$data_x]]
    })

    time_attr <- reactive({

      dat <- data()
      time <- attr(dat, "time")

      if (is.null(time)) {
        return(NULL)
      }

      if (!is.data.frame(time)) {
        time <- data.frame(time, check.names = FALSE)
      }

      if (nrow(time) == nrow(dat)) {
        rownames(time) <- rownames(dat)
        return(time)
      }

      if (!is.null(rownames(time)) && all(rownames(dat) %in% rownames(time))) {
        return(time[rownames(dat), , drop = FALSE])
      }

      time
    })

    observeEvent(vals$saved_data, {

      data_names <- names(vals$saved_data)
      selected_data <- isolate(input$data_x)

      if (is.null(selected_data) || !selected_data %in% data_names) {
        selected_data <- if (length(data_names) > 0) data_names[1] else character(0)
      }

      updatePickerInput(
        session,
        "data_x",
        choices = data_names,
        selected = selected_data
      )

    }, ignoreNULL = FALSE)

    observeEvent(input$data_x, {
      reset_pending()
    }, ignoreInit = TRUE)

    observeEvent(time_attr(), {

      time <- time_attr()
      reset_pending(clear_message = FALSE)

      if (is.null(time) || ncol(time) == 0) {
        updatePickerInput(session, "time_column", choices = NULL, selected = NULL)
        return()
      }

      selected_col <- isolate(input$time_column)

      if (is.null(selected_col) || !selected_col %in% colnames(time)) {
        selected_col <- colnames(time)[1]
      }

      updatePickerInput(
        session,
        "time_column",
        choices = colnames(time),
        selected = selected_col
      )

    }, ignoreNULL = FALSE)

    observeEvent(
      list(input$time_column, input$time_operation),
      {

        req(input$time_column)
        req(input$time_operation)

        updateTextInput(
          session,
          "new_time_name",
          value = paste(input$time_column, input$time_operation, sep = "_")
        )

        reset_pending()
      },
      ignoreInit = TRUE
    )

    observeEvent(input$new_time_name, {
      reset_pending()
    }, ignoreInit = TRUE)

    as_date_safe <- function(x){

      if (inherits(x, "Date")) {
        return(x)
      }

      if (inherits(x, c("POSIXct", "POSIXlt"))) {
        return(as.Date(x))
      }

      if (is.numeric(x)) {
        return(suppressWarnings(as.Date(x, origin = "1970-01-01")))
      }

      if (is.character(x) || is.factor(x)) {

        x_chr <- trimws(as.character(x))
        out <- rep(as.Date(NA), length(x_chr))
        formats <- c(
          "%Y-%m-%d",
          "%Y/%m/%d",
          "%d/%m/%Y",
          "%d-%m-%Y",
          "%m/%d/%Y",
          "%Y-%m-%d %H:%M:%S",
          "%Y/%m/%d %H:%M:%S"
        )

        for (fmt in formats) {
          miss <- is.na(out) & !is.na(x_chr) & nzchar(x_chr)

          if (!any(miss)) {
            break
          }

          out[miss] <- suppressWarnings(as.Date(x_chr[miss], format = fmt))
        }

        return(out)
      }

      rep(as.Date(NA), length(x))
    }

    make_temporal_derivation <- function(x, operation){

      date <- as_date_safe(x)

      if (all(is.na(date))) {
        stop("The selected temporal column could not be interpreted as a date.")
      }

      year <- as.integer(format(date, "%Y"))
      month <- as.integer(format(date, "%m"))

      switch(
        operation,

        day = date,

        week = date - as.integer(format(date, "%u")) + 1,

        month = as.Date(paste0(format(date, "%Y-%m"), "-01")),

        quarter = {
          q_month <- ((month - 1) %/% 3) * 3 + 1

          as.Date(
            paste0(year, "-", sprintf("%02d", q_month), "-01")
          )
        },

        semester = {
          s_month <- ifelse(month <= 6, 1, 7)

          as.Date(
            paste0(year, "-", sprintf("%02d", s_month), "-01")
          )
        },

        year = as.Date(paste0(year, "-01-01")),

        season_south = {
          season_start_month <- ifelse(
            month %in% c(12, 1, 2),
            12,
            ifelse(
              month %in% c(3, 4, 5),
              3,
              ifelse(month %in% c(6, 7, 8), 6, 9)
            )
          )

          season_year <- year
          season_year[month %in% c(1, 2)] <- season_year[month %in% c(1, 2)] - 1

          as.Date(
            paste0(season_year, "-", sprintf("%02d", season_start_month), "-01")
          )
        },

        stop("Invalid temporal operation.")
      )
    }

    format_time_preview <- function(x){

      x <- as.data.frame(x, stringsAsFactors = FALSE)

      for (j in seq_along(x)) {

        if (inherits(x[[j]], "Date")) {
          x[[j]] <- format(x[[j]], "%Y-%m-%d")
        } else if (inherits(x[[j]], c("POSIXct", "POSIXlt"))) {
          x[[j]] <- format(x[[j]], "%Y-%m-%d %H:%M:%S")
        } else if (inherits(x[[j]], "factor")) {
          x[[j]] <- as.character(x[[j]])
        }
      }

      x
    }

    preview_time <- reactive({

      pending <- r_time_edit()

      if (!is.null(pending)) {
        return(attr(pending, "time"))
      }

      time_attr()
    })

    observeEvent(input$run_time_edit, ignoreInit = TRUE, {

      tryCatch({

        time <- time_attr()

        req(time)
        req(ncol(time) > 0)
        req(input$time_column)
        req(input$time_operation)
        req(input$new_time_name)

        if (!input$time_column %in% colnames(time)) {
          stop("Selected temporal column was not found.")
        }

        new_name <- trimws(input$new_time_name)

        if (!nzchar(new_name) || is.na(new_name)) {
          stop("Please provide a valid name for the new temporal column.")
        }

        if (new_name %in% colnames(time) && !isTRUE(input$replace_existing)) {
          stop("A temporal column with this name already exists. Enable replacement or choose another name.")
        }

        time[[new_name]] <- make_temporal_derivation(
          x = time[[input$time_column]],
          operation = input$time_operation
        )

        dat <- data()
        attr(dat, "time") <- time

        r_time_edit(dat)
        shinyjs::removeClass("run_time_edit_btn", "save_changes")

        last_message(
          paste0(
            "Temporal column '",
            new_name,
            "' was created from '",
            input$time_column,
            "'. Click save to make it permanent."
          )
        )

      }, error = function(e) {

        r_time_edit(NULL)
        shinyjs::addClass("run_time_edit_btn", "save_changes")
        last_message(paste0("Temporal edition failed: ", e$message))
      })
    })

    observeEvent(input$time_save, {

      dat <- r_time_edit()
      req(dat)
      req(input$data_x)
      req(input$data_x %in% names(vals$saved_data))

      vals$saved_data[[input$data_x]] <- dat
      r_time_edit(NULL)
      shinyjs::removeClass(id = "time_save", class = "save_changes")

      last_message("Temporal-Attribute saved successfully.")

    }, ignoreInit = TRUE)

    output$war_time_edit <- renderUI({

      time <- time_attr()

      if (is.null(time) || ncol(time) == 0) {
        return(
          div(
            class = "alert_warning",
            icon("triangle-exclamation", style = "color: gold"),
            strong(" No Temporal-Attribute found."),
            div(
              em("This tool is only available for Datalists with a Temporal-Attribute.")
            )
          )
        )
      }

      msg <- last_message()

      if (is.null(msg)) {
        return(NULL)
      }

      if (grepl("^Temporal edition failed", msg)) {
        return(
          div(
            class = "alert_warning",
            icon("triangle-exclamation", style = "color: gold"),
            msg
          )
        )
      }

      div(
        class = "alert_success",
        icon("circle-check"),
        msg
      )
    })

    output$summ_time_edit <- renderUI({

      time <- time_attr()

      if (is.null(time) || ncol(time) == 0) {
        return(
          div(
            strong("Temporal-Attribute:"),
            div(em("not available"))
          )
        )
      }

      pending <- r_time_edit()
      pending_time <- if (!is.null(pending)) attr(pending, "time") else NULL

      div(
        div(strong("Original Temporal-Attribute:")),
        div(em("rows:"), nrow(time)),
        div(em("columns:"), ncol(time)),

        br(),

        if (!is.null(pending_time)) {
          div(
            div(strong("Updated Temporal-Attribute:")),
            div(em("rows:"), nrow(pending_time)),
            div(em("columns:"), ncol(pending_time)),
            div(
              "Save to make permanent",
              div(
                class = "save_changes",
                actionButton(session$ns("time_save"), icon("fas fa-save"), width = "40px")
              )
            )
          )
        } else {
          div(
            div(strong("Selected operation:")),
            div(em(input$time_operation))
          )
        }
      )
    })

    output$preview_time_edit <- renderUI({

      req(preview_time())

      div(
        strong("Current Temporal-Attribute:"),
        div(
          style = "max-height: 300px; overflow-y: scroll; margin-top: 10px;",
          tableOutput(session$ns("preview_time_table"))
        )
      )
    })

    output$preview_time_table <- renderTable({
      req(preview_time())
      format_time_preview(head(preview_time(), 10))
    }, rownames = TRUE)

    return(NULL)
  })
}


# Temporal Feature Builder override
tool2_tab10<-list()
tool2_tab10$ui<-function(id){
  ns<-NS(id)
  nav_tip<-function(label, help){
    span(
      label,
      span(style="margin-left:0px; font-size:12px;", tiphelp(help, "right"))
    )
  }
  div(class='tool2_tab9',
      div(
        class="tool10 half-drop-inline tool2_tab10 ",style="overflow-y: scroll; height: 100vh",
        div(style="position: fixed; right: 80vw;top: 50px ",
            actionButton(ns("exit_tool10"), label = NULL, icon = icon("times"), style = "padding: 0px; font-size: 15px; width: 20px; height: 20px;background: Brown; color: white; border: 0px;")),
        tags$style(HTML("
      .tool2_tab10 .feature-nav .nav-list {
        max-height: 230px;
        overflow-y: auto;
        overflow-x: hidden;

      }
      .tool2_tab10 .feature-nav .tab-content {
        min-height: 170px;
      }
      .tool2_tab10 .feature-subtitle {
        font-weight: 700;
        margin: 1px 0 1px 0;

      }
      .tool2_tab10 .feature-subblock {
        margin-bottom: 12px;

      }

      .nav-time{
      background: whitesmoke; padding: 8px;overflow-y: scroll; height: 200px;
      }

    ")),
        div(
          div(

            h3(strong("Temporal Features")),
            column(6,class="mp0",
                   box_caret(
                     ns('feature-time'),
                     title="Setup",
                     color="#c3cc74ff",
                     div(
                       pickerInput_fromtop_live(
                         ns("data_x"),
                         tiphelp5("Datalist","Select the datalist that will receive the derived variables."),
                         choices=NULL
                       ),
                       pickerInput_fromtop_live(
                         ns("vars"),
                         tiphelp5("Variables","Select numeric variables used to create derived predictors. Leave empty to use all numeric columns."),
                         choices=NULL,
                         multiple=TRUE,
                         options=pickerOptions(actionsBox =T)
                       ),
                       textInput(
                         ns("prefix"),
                         tiphelp5("Prefix","Optional prefix added to the generated variable names."),
                         value=""
                       ),
                       div(
                         uiOutput(ns("time_col_ui")),
                         uiOutput(ns("group_by_coords_ui"))
                       ),




                     )),

                   box_caret(
                     ns('feature-time1'),
                     title="Feature steps",
                     color="#c3cc74ff",
                     div(
                       div(

                         navlistPanel(
                           widths=c(3,9),
                           well=FALSE,
                           selected="memory",
                           tabPanel(
                             nav_tip("Memory","Creates memory-based predictors, including ordinary lags and exponentially weighted memory variables."),
                             value="memory",
                             div(class='nav-time',
                                 div(
                                   class="feature-subblock",
                                   div(class="feature-subtitle","Lags"),
                                   textInput(
                                     ns("lags"),
                                     tiphelp5("Steps","Lag steps separated by commas. Example: 1, 2, 3 creates previous-value predictors for each selected variable."),
                                     value="1, 3"
                                   ),
                                   div(
                                     class="feature-actions save_changes",
                                     actionButton(ns("add_memory"),span("Add",icon("plus")))
                                   )
                                 ),
                                 div(
                                   class="feature-subblock",
                                   div(class="feature-subtitle","Lagged Exponential Decay"),
                                   div(
                                     class="feature-inline",
                                     numericInput(
                                       ns("led_alpha"),
                                       tiphelp5("Alpha","Decay strength for exponentially weighted memory. Higher values give more weight to recent observations."),
                                       value=.3,
                                       min=.01,
                                       max=1,
                                       step=.01
                                     ),
                                     numericInput(
                                       ns("led_init"),
                                       tiphelp5("Initial","Initial value used when the LED series starts. Use NA to start from the first observed value."),
                                       value=NA,
                                       step=.1
                                     )
                                   ),
                                   checkboxInput(
                                     ns("led_center"),
                                     tiphelp5("Center","Center the LED variables after they are created. Useful when models are sensitive to scale."),
                                     value=FALSE
                                   ),
                                   div(
                                     class="feature-actions save_changes",
                                     actionButton(ns("add_led"),span("Add",icon("plus")))
                                   )
                                 )

                             )),
                           tabPanel(
                             nav_tip("Lead","Creates future-shifted variables, such as x(t+1). Use this mainly to define future response targets. Avoid using leads as ordinary predictors because they can leak future information into machine-learning validation."),
                             value="lead",
                             div(class='nav-time',
                                 textInput(
                                   ns("leads"),
                                   tiphelp5("Steps","Lead steps separated by commas. Example: 1, 3 creates future-value variables for one and three observations ahead."),
                                   value="1"
                                 ),
                                 div(
                                   class="feature-actions save_changes",
                                   actionButton(ns("add_lead"),span("Add",icon("plus")))
                                 )
                             )),
                           tabPanel(
                             nav_tip("Trend","Creates rolling summaries over temporal windows, including mean, SD, slope, min, max, median, quantiles and IQR."),
                             value="trend",
                             div(class="nav-time",
                                 div(
                                   textInput(
                                     ns("trend_windows"),
                                     tiphelp5("Windows","Rolling window sizes separated by commas. Example: 3, 7 creates rolling summaries over 3 and 7 observations."),
                                     value="3, 7"
                                   ),
                                   checkboxGroupInput(
                                     ns("trend_stats"),
                                     tiphelp5("Summaries","Rolling summaries to create for each selected variable and window."),
                                     choices=c("Rolling mean"="mean","Rolling SD"="sd","Rolling slope"="slope",
                                               "Rolling min"="min","Rolling max"="max","Rolling median"="median",
                                               "Rolling Q25"="q25","Rolling Q75"="q75","Rolling IQR"="iqr"),
                                     selected=c("mean","sd","slope")
                                   ),
                                   div(
                                     class="feature-actions save_changes",
                                     actionButton(ns("add_trend"),span("Add",icon("plus")))
                                   )

                                 ))),
                           tabPanel(
                             nav_tip("Change","Creates differences and percent changes between the current value and previous lagged values."),
                             value="change",
                             div(class='nav-time',
                                 textInput(
                                   ns("change_lags"),
                                   tiphelp5("Lags","Lag steps used to calculate differences or percent changes."),
                                   value="1, 3"
                                 ),
                                 checkboxGroupInput(
                                   ns("change_types"),
                                   tiphelp5("Types","Difference is x(t)-x(t-lag). Percent change is relative change from x(t-lag)."),
                                   choices=c("Difference"="diff","Percent change"="pct"),
                                   selected=c("diff")
                                 ),
                                 div(
                                   class="feature-actions save_changes",
                                   actionButton(ns("add_change"),span("Add",icon("plus")))
                                 )

                             )),
                           tabPanel(
                             nav_tip("Anomaly","Creates deviations from local rolling baselines and rolling z-scores to highlight unusual values in each temporal series."),
                             value="anomaly",
                             div(class='nav-time',
                                 textInput(
                                   ns("anomaly_windows"),
                                   tiphelp5("Windows","Rolling window sizes used as local baselines."),
                                   value="3, 7"
                                 ),
                                 checkboxGroupInput(
                                   ns("anomaly_types"),
                                   tiphelp5("Types","Deviation subtracts a rolling baseline. Z-score divides that deviation by rolling SD."),
                                   choices=c("Current minus rolling mean"="mean_dev",
                                             "Current minus rolling median"="median_dev",
                                             "Z-score anomaly"="zscore"),
                                   selected=c("mean_dev")
                                 ),
                                 div(
                                   class="feature-actions save_changes",
                                   actionButton(ns("add_anomaly"),span("Add",icon("plus")))
                                 )

                             )),
                           tabPanel(
                             nav_tip("Cumulative","Creates cumulative sum or mean from the beginning of each temporal series up to the current observation."),
                             value="cumulative",
                             div(class='nav-time',
                                 checkboxGroupInput(
                                   ns("cumulative_types"),
                                   tiphelp5("Types","Cumulative summaries calculated from the start of each temporal series up to the current observation."),
                                   choices=c("Sum"="sum","Mean"="mean"),
                                   selected=c("sum","mean")
                                 ),
                                 div(
                                   class="feature-actions save_changes",
                                   actionButton(ns("add_cumulative"),span("Add",icon("plus")))
                                 )

                             )),
                           tabPanel(
                             nav_tip("Seasonality","Creates calendar and cyclic predictors from the temporal column, such as month, day-of-year, week, year and custom cycles."),
                             value="seasonality",
                             div(class='nav-time',
                                 checkboxGroupInput(
                                   ns("seasonal_terms"),
                                   tiphelp5("Terms","Seasonal encodings to create from the selected temporal column."),
                                   choices=c("Month sin/cos"="month_cyclic","Day-of-year sin/cos"="doy_cyclic","Week"="week","Year"="year"),
                                   selected=c("month_cyclic","doy_cyclic")
                                 ),
                                 textInput(
                                   ns("seasonal_periods"),
                                   tiphelp5("Custom cycles","Optional cycle periods in ordered observations, separated by commas. Example: 7, 12, 365."),
                                   value=""
                                 ),
                                 div(
                                   class="feature-actions save_changes",
                                   actionButton(ns("add_seasonality"),span("Add",icon("plus")))
                                 )

                             ))
                         )
                       )
                     )
                   )

            ),
            column(6,

                   class="mp0",
                   box_caret(
                     ns("feature-time-out"),
                     title="Output",
                     div(
                       div(
                         class="feature-section",

                         radioButtons(
                           ns("output_mode"),
                           tiphelp5("Mode","Append keeps the current datalist and adds the derived variables. New datalist saves only the derived variables, without the original numeric columns."),
                           choices=c("Append"="append","New datalist"="new"),
                           selected="append",
                           inline=TRUE
                         ),
                         textInput(
                           ns("new_name"),
                           tiphelp5("New name","Name used when saving the result as a new datalist."),
                           value="derived_features"
                         ),
                         div(
                           class="feature-actions save_changes",
                           actionButton(ns("preview_features"),span("Preview",icon("eye"))),
                           actionButton(ns("run_features"),span("Create",icon("angles-right")))
                         )
                       ),
                       div(
                         class="recipe-box",
                         div(class="feature-section-title","Recipe"),
                         uiOutput(ns("derived_recipe")),
                         div(class="feature-actions",actionLink(ns("clear_recipe"),"clear"))
                       ),
                       uiOutput(ns("derived_summary")),

                       div(style="overflow-y: scroll",
                           uiOutput(ns("derived_preview"))
                       )

                     )

                   ))
          )
        )
      ))
}

tool2_tab10$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    observeEvent(input$exit_tool10,{
      vals$exit_tool10<-input$exit_tool10
    })

    data_x<-reactive({
      vals$saved_data[[input$data_x]]
    })

    observeEvent(vals$saved_data,{
      updatePickerInput(session,'data_x',choices=names(vals$saved_data),selected=vals$cur_data)
    })

    observeEvent(input$data_x,{
      vals$cur_data<-input$data_x
    })


    observeEvent(input$data_x,{
      dat<-data_x()
      num_cols<-colnames(dat)[vapply(dat,is.numeric,logical(1))]
      time_attr<-attr(dat,"time")

      selected<-get_selected_from_choices(vals$cur_time_vars,num_cols)

      updatePickerInput(session,'vars',choices=num_cols,selected=num_cols[1])
    })

    output$time_col_ui<-renderUI({
      dat<-data_x()
      time_attr<-attr(dat,"time")
      if(is.null(time_attr)||!ncol(as.data.frame(time_attr))){
        return(NULL)
      }
      pickerInput_fromtop_live(
        session$ns("time_col"),
        tiphelp5("Temporal column","Temporal attribute used to order lags, rolling summaries, LED, and seasonal predictors."),
        choices=colnames(time_attr),
        selected=colnames(time_attr)[1]
      )
    })

    output$group_by_coords_ui<-renderUI({
      dat<-data_x()
      coords<-attr(dat,"coords")
      if(is.null(coords)||!nrow(as.data.frame(coords))){
        return(NULL)
      }
      checkboxInput(
        session$ns("group_by_coords"),
        tiphelp5("Group by coordinates","Calculate temporal derived variables separately for each coordinate pair. This prevents lags and rolling summaries from crossing locations."),
        value=TRUE
      )
    })



    feature_recipe<-reactiveVal(list())

    feature_vars<-reactive({
      dat<-data_x()
      vars<-input$vars
      if(length(vars)==0){
        vars<-colnames(dat)[vapply(dat,is.numeric,logical(1))]
      }
      vars
    })

    add_recipe_step<-function(type,label,settings){
      steps<-feature_recipe()
      steps[[length(steps)+1]]<-list(
        type=type,
        label=label,
        vars=feature_vars(),
        prefix=input$prefix,
        settings=settings
      )
      feature_recipe(steps)
    }

    parse_numeric_list<-function(x,default=NULL){
      if(is.null(x)||!nzchar(trimws(x))){
        return(default)
      }
      vals<-suppressWarnings(as.numeric(trimws(unlist(strsplit(x,",")))))
      vals<-vals[!is.na(vals)]
      vals
    }

    feature_name<-function(prefix,var,suffix,existing){
      nm<-paste0(ifelse(nzchar(prefix),paste0(prefix,"_"),""),var,"_",suffix)
      make.unique(c(existing,nm))[length(existing)+1]
    }

    safe_stat<-function(x,fun){
      x<-x[!is.na(x)]
      if(!length(x)){
        return(NA_real_)
      }
      val<-suppressWarnings(fun(x))
      if(!length(val)){
        return(NA_real_)
      }
      val<-as.numeric(val[1])
      if(is.na(val)||is.nan(val)||is.infinite(val)){
        return(NA_real_)
      }
      val
    }

    rolling_values<-function(x,k,fun){
      out<-rep(NA_real_,length(x))
      if(k<1||length(x)<1){
        return(out)
      }
      for(i in seq_along(x)){
        idx<-seq(max(1,i-k+1),i)
        z<-x[idx]
        out[i]<-fun(z)
      }
      out
    }

    rolling_slope<-function(x,k){
      rolling_values(x,k,function(z){
        ok<-!is.na(z)
        if(sum(ok)<2){
          return(NA_real_)
        }
        coef(stats::lm(z[ok]~seq_along(z)[ok]))[2]
      })
    }

    lag_values<-function(x,lag_i){
      if(lag_i>=length(x)){
        return(rep(NA,length(x)))
      }
      c(rep(NA,lag_i),head(x,-lag_i))
    }

    lead_values<-function(x,lead_i){
      if(lead_i>=length(x)){
        return(rep(NA,length(x)))
      }
      c(tail(x,-lead_i),rep(NA,lead_i))
    }

    cumulative_values<-function(x,type){
      ok<-!is.na(x)
      x0<-ifelse(ok,x,0)
      if(type=="sum"){
        return(cumsum(x0))
      }
      if(type=="mean"){
        n_ok<-cumsum(ok)
        return(ifelse(n_ok>0,cumsum(x0)/n_ok,NA_real_))
      }
      rep(NA_real_,length(x))
    }

    led_values<-function(x,alpha=.3,initial=NA){
      out<-rep(NA_real_,length(x))
      if(!length(x)){
        return(out)
      }
      alpha<-min(max(alpha,.Machine$double.eps),1)
      state<-suppressWarnings(as.numeric(initial))
      if(!length(state)){
        state<-NA_real_
      }
      has_state<-!is.na(state)
      for(i in seq_along(x)){
        if(is.na(x[i])){
          out[i]<-if(has_state) state else NA_real_
        }else{
          state<-if(has_state) alpha*x[i]+(1-alpha)*state else x[i]
          has_state<-TRUE
          out[i]<-state
        }
      }
      out
    }

    # Calendar parts from a temporal column. Text dates are read with the format detected
    # by guess_time_settings() (same rules as the Databank import; DD/MM is preferred over
    # MM/DD when both fit); values that match no format stay NA and are counted in 'unparsed'.
    parse_time_parts<-function(tt){
      n<-length(tt)
      res<-list(
        date=rep(as.Date(NA),n),
        month=rep(NA_real_,n),
        doy=rep(NA_real_,n),
        week=rep(NA_real_,n),
        year=rep(NA_real_,n),
        unparsed=0
      )
      fill_dates<-function(res,date_val){
        res$date<-date_val
        ok<-!is.na(date_val)
        res$month[ok]<-as.numeric(format(date_val[ok],"%m"))
        res$doy[ok]<-as.numeric(format(date_val[ok],"%j"))
        res$week[ok]<-as.numeric(format(date_val[ok],"%U"))
        res$year[ok]<-as.numeric(format(date_val[ok],"%Y"))
        res
      }
      if(inherits(tt,"Date")){
        return(fill_dates(res,tt))
      }
      if(inherits(tt,"POSIXt")){
        # format() keeps the column's own time zone (as.Date would convert to UTC)
        return(fill_dates(res,as.Date(format(tt,"%Y-%m-%d"))))
      }
      if(is.numeric(tt)){
        tt_num<-as.numeric(tt)
        is_int<-is.na(tt_num)|tt_num==round(tt_num)
        if(all(is_int&(is.na(tt_num)|(tt_num>=1000&tt_num<=9999)))){
          res$year<-tt_num
        }else if(all(is_int&(is.na(tt_num)|(tt_num>=1&tt_num<=12)))){
          res$month<-tt_num
        }else if(all(is_int&(is.na(tt_num)|(tt_num>=1&tt_num<=366)))){
          res$doy<-tt_num
          res$week<-floor((tt_num-1)/7)
        }else if(all(is_int&(is.na(tt_num)|(tt_num>=100001&tt_num<=999912&(tt_num%%100)%in%1:12)))){
          res$year<-floor(tt_num/100)
          res$month<-tt_num%%100
        }else if(all(is_int&(is.na(tt_num)|(tt_num>=10000101&tt_num<=99991231)))){
          res<-fill_dates(res,as.Date(as.character(tt_num),format="%Y%m%d"))
        }
        res$unparsed<-sum(!is.na(tt_num)&is.na(res$year)&is.na(res$month)&is.na(res$doy))
        return(res)
      }
      tt_chr<-trimws(as.character(tt))
      date_val<-convert_time_column(tt,"date","auto")
      res<-fill_dates(res,date_val)
      ym<-grepl("^\\d{4}[-/_]\\d{1,2}$",tt_chr)
      if(any(ym)){
        ym_parts<-strsplit(tt_chr[ym],"[-/_]")
        ym_month<-as.numeric(vapply(ym_parts,`[`,character(1),2))
        ym_month[!ym_month%in%1:12]<-NA
        res$year[ym]<-ifelse(is.na(ym_month),NA,as.numeric(vapply(ym_parts,`[`,character(1),1)))
        res$month[ym]<-ym_month
      }
      res$unparsed<-sum(!is.na(tt)&nzchar(tt_chr)&is.na(res$year)&is.na(res$month)&is.na(res$doy))
      res
    }

    # Temporal column aligned to the rows of 'dat' (matched by row names when available)
    get_time_values<-function(dat,time_col){
      time_attr<-attr(dat,"time")
      if(is.null(time_attr)||length(time_col)!=1||!time_col%in%colnames(time_attr)){
        return(NULL)
      }
      ids<-rownames(dat)
      if(!is.null(ids)&&all(ids%in%rownames(time_attr))){
        return(time_attr[ids,time_col,drop=TRUE])
      }
      if(nrow(time_attr)==nrow(dat)){
        return(time_attr[[time_col]])
      }
      NULL
    }

    get_time_order<-function(dat){
      tt<-get_time_values(dat,input$time_col)
      if(is.null(tt)){
        return(seq_len(nrow(dat)))
      }
      tt_date<-tt
      if(!inherits(tt,c("Date","POSIXt"))&&!is.numeric(tt)){
        # text dates are parsed with an explicit format (never as.Date/as.POSIXct guessing)
        guess<-guess_time_settings(tt)
        if(guess$type%in%c("date","datetime")){
          tt_date<-convert_time_column(tt,guess$type,guess$format,guess$custom)
        }
      }
      order(tt_date,seq_along(tt_date),na.last=TRUE)
    }

    get_series_groups<-function(dat){
      coords<-attr(dat,"coords")
      if(!isTRUE(input$group_by_coords)||is.null(coords)){
        return(factor(rep("all",nrow(dat))))
      }
      coords<-as.data.frame(coords)
      if(nrow(coords)!=nrow(dat)||!ncol(coords)){
        return(factor(rep("all",nrow(dat))))
      }
      coords_key<-data.frame(lapply(coords,function(x){
        if(is.numeric(x)) round(x,6) else x
      }),check.names=FALSE)
      interaction(coords_key,drop=TRUE,lex.order=TRUE)
    }

    apply_by_series<-function(dat,x,fun){
      if(!isTRUE(input$group_by_coords)){
        out<-rep(NA_real_,length(x))
        idx<-get_time_order(dat)
        res<-fun(x[idx])
        if(length(res)!=length(idx)){
          res<-rep(NA_real_,length(idx))
        }
        out[idx]<-res
        return(out)
      }
      out<-rep(NA_real_,length(x))
      groups<-get_series_groups(dat)
      time_order<-get_time_order(dat)
      for(g in levels(groups)){
        idx<-which(groups==g)
        idx<-idx[order(match(idx,time_order),na.last=TRUE)]
        res<-fun(x[idx])
        if(length(res)!=length(idx)){
          res<-rep(NA_real_,length(idx))
        }
        out[idx]<-res
      }
      out
    }

    apply_feature_recipe<-function(dat,steps,include_originals=TRUE){
      if(is.null(dat)){
        return(NULL)
      }
      out<-data.frame(dat,check.names=FALSE)
      original_names<-colnames(out)
      if(!length(steps)){
        return(out)
      }
      notes<-character(0)

      for(step in steps){
        vars<-intersect(step$vars,colnames(out))
        prefix<-step$prefix

        if(step$type=="memory"){
          lags<-parse_numeric_list(step$settings$lags,1)
          lags<-unique(as.integer(lags[lags>0]))
          for(var in vars){
            x<-out[[var]]
            for(lag_i in lags){
              nm<-feature_name(prefix,var,paste0("lag",lag_i),colnames(out))
              out[[nm]]<-apply_by_series(dat,x,function(z)lag_values(z,lag_i))
            }
          }
        }

        if(step$type=="lead"){
          leads<-parse_numeric_list(step$settings$leads,1)
          leads<-unique(as.integer(leads[leads>0]))
          for(var in vars){
            x<-out[[var]]
            for(lead_i in leads){
              nm<-feature_name(prefix,var,paste0("lead",lead_i),colnames(out))
              out[[nm]]<-apply_by_series(dat,x,function(z)lead_values(z,lead_i))
            }
          }
        }

        if(step$type=="trend"){
          windows<-parse_numeric_list(step$settings$windows,3)
          windows<-unique(as.integer(windows[windows>1]))
          stats<-step$settings$summaries
          if(is.null(stats)||!length(stats)){
            stats<-"mean"
          }
          for(var in vars){
            x<-as.numeric(out[[var]])
            for(window_i in windows){
              for(stat_i in stats){
                nm<-feature_name(prefix,var,paste0("roll",window_i,"_",stat_i),colnames(out))
                out[[nm]]<-switch(
                  stat_i,
                  mean=apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,mean))),
                  sd=apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,stats::sd))),
                  slope=apply_by_series(dat,x,function(z)rolling_slope(z,window_i)),
                  min=apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,min))),
                  max=apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,max))),
                  median=apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,stats::median))),
                  q25=apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,function(v)stats::quantile(v,.25,names=FALSE)))),
                  q75=apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,function(v)stats::quantile(v,.75,names=FALSE)))),
                  iqr=apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,stats::IQR))),
                  apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,mean)))
                )
              }
            }
          }
        }

        if(step$type=="change"){
          lags<-parse_numeric_list(step$settings$lags,1)
          lags<-unique(as.integer(lags[lags>0]))
          types<-step$settings$types
          if(is.null(types)||!length(types)){
            types<-"diff"
          }
          for(var in vars){
            x<-as.numeric(out[[var]])
            for(lag_i in lags){
              for(type_i in types){
                suffix<-if(type_i=="pct") paste0("pct_change",lag_i) else paste0("diff",lag_i)
                nm<-feature_name(prefix,var,suffix,colnames(out))
                out[[nm]]<-apply_by_series(dat,x,function(z){
                  lagged<-lag_values(z,lag_i)
                  if(type_i=="pct"){
                    pct<-(z-lagged)/lagged
                    pct[is.nan(pct)|is.infinite(pct)]<-NA_real_
                    return(pct)
                  }
                  z-lagged
                })
              }
            }
          }
        }

        if(step$type=="anomaly"){
          windows<-parse_numeric_list(step$settings$windows,3)
          windows<-unique(as.integer(windows[windows>1]))
          types<-step$settings$types
          if(is.null(types)||!length(types)){
            types<-"mean_dev"
          }
          for(var in vars){
            x<-as.numeric(out[[var]])
            for(window_i in windows){
              roll_mean<-apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,mean)))
              roll_median<-apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,stats::median)))
              roll_sd<-apply_by_series(dat,x,function(z)rolling_values(z,window_i,function(y)safe_stat(y,stats::sd)))
              for(type_i in types){
                nm<-feature_name(prefix,var,paste0("anom_",window_i,"_",type_i),colnames(out))
                out[[nm]]<-switch(
                  type_i,
                  mean_dev=x-roll_mean,
                  median_dev=x-roll_median,
                  zscore={
                    zsc<-(x-roll_mean)/roll_sd
                    zsc[is.nan(zsc)|is.infinite(zsc)]<-NA_real_
                    zsc
                  },
                  x-roll_mean
                )
              }
            }
          }
        }

        if(step$type=="led"){
          alpha<-suppressWarnings(as.numeric(step$settings$alpha))
          if(is.na(alpha)){
            alpha<-.3
          }
          initial<-suppressWarnings(as.numeric(step$settings$initial))
          for(var in vars){
            nm<-feature_name(prefix,var,"led",colnames(out))
            vals_led<-apply_by_series(
              dat,
              as.numeric(out[[var]]),
              function(z)led_values(z,alpha=alpha,initial=initial)
            )
            if(isTRUE(step$settings$center)){
              vals_led<-as.numeric(scale(vals_led,center=TRUE,scale=FALSE))
            }
            out[[nm]]<-vals_led
          }
        }

        if(step$type=="cumulative"){
          types<-step$settings$types
          if(is.null(types)||!length(types)){
            types<-"sum"
          }
          for(var in vars){
            x<-as.numeric(out[[var]])
            for(type_i in types){
              nm<-feature_name(prefix,var,paste0("cum_",type_i),colnames(out))
              out[[nm]]<-apply_by_series(dat,x,function(z)cumulative_values(z,type_i))
            }
          }
        }

        if(step$type=="seasonality"){
          time_col<-step$settings$time_col
          tt<-get_time_values(dat,time_col)
          if(is.null(tt)){
            notes<-c(notes,paste0("Seasonality skipped: the Temporal-Attribute column '",paste(time_col,collapse=", "),"' was not found for all observations of this Datalist."))
          }
          if(!is.null(tt)){
            if(length(tt)==nrow(out)){
              time_parts<-parse_time_parts(tt)
              if(time_parts$unparsed>0){
                notes<-c(notes,paste0("Seasonality: ",time_parts$unparsed," value(s) of '",time_col,"' could not be read as a date and were set to NA. Use an unambiguous format (YYYY-MM-DD) or format the column as Date in the Databank."))
              }
              terms<-step$settings$terms
              if("month_cyclic"%in%terms){
                month_i<-time_parts$month
                out[[feature_name(prefix,time_col,"month_sin",colnames(out))]]<-sin(2*pi*month_i/12)
                out[[feature_name(prefix,time_col,"month_cos",colnames(out))]]<-cos(2*pi*month_i/12)
              }
              if("doy_cyclic"%in%terms){
                # (doy-1)/days-in-year keeps Dec 31 -> Jan 1 one day apart in leap and non-leap years
                year_i<-time_parts$year
                leap<-!is.na(year_i)&((year_i%%4==0&year_i%%100!=0)|year_i%%400==0)
                days_in_year<-ifelse(is.na(year_i),365.25,ifelse(leap,366,365))
                doy_i<-time_parts$doy
                out[[feature_name(prefix,time_col,"doy_sin",colnames(out))]]<-sin(2*pi*(doy_i-1)/days_in_year)
                out[[feature_name(prefix,time_col,"doy_cos",colnames(out))]]<-cos(2*pi*(doy_i-1)/days_in_year)
              }
              if("week"%in%terms){
                out[[feature_name(prefix,time_col,"week",colnames(out))]]<-time_parts$week
              }
              if("year"%in%terms){
                out[[feature_name(prefix,time_col,"year",colnames(out))]]<-time_parts$year
              }
              periods<-parse_numeric_list(step$settings$periods,NULL)
              periods<-unique(periods[!is.na(periods)&periods>1])
              if(length(periods)){
                cyc_idx<-apply_by_series(dat,seq_len(nrow(out)),function(z)seq_along(z))
                for(period_i in periods){
                  period_label<-gsub("\\.","p",as.character(period_i))
                  out[[feature_name(prefix,time_col,paste0("cycle",period_label,"_sin"),colnames(out))]]<-sin(2*pi*cyc_idx/period_i)
                  out[[feature_name(prefix,time_col,paste0("cycle",period_label,"_cos"),colnames(out))]]<-cos(2*pi*cyc_idx/period_i)
                }
              }
            }
          }
        }
      }

      generated_names<-setdiff(colnames(out),original_names)
      if(!isTRUE(include_originals)){
        out<-out[,generated_names,drop=FALSE]
      }

      attrs<-attributes(dat)
      attrs$names<-names(out)
      attrs$row.names<-attr(out,"row.names")
      attrs$class<-class(out)
      attrs$derived_feature_names<-generated_names
      attributes(out)<-attrs
      attr(out,"feature_notes")<-unique(notes)
      out
    }

    preview_data<-reactive({
      tryCatch(
        apply_feature_recipe(
          data_x(),
          feature_recipe(),
          include_originals=identical(input$output_mode,"append")
        ),
        error=function(e)e
      )
    })

    prepare_feature_result<-function(){
      dat<-preview_data()
      if(inherits(dat,"error")){
        showNotification(conditionMessage(dat),type="error",duration=8)
        return(NULL)
      }
      if(!length(feature_recipe())){
        showNotification("Add at least one derived feature step before creating the result.",type="warning",duration=6)
        return(NULL)
      }
      for(note_i in attr(dat,"feature_notes")){
        showNotification(note_i,type="warning",duration=10)
      }
      generated_names<-attr(dat,"derived_feature_names")
      if(is.null(generated_names)||!length(generated_names)){
        showNotification("No derived variable was generated. Check the selected variables and recipe settings.",type="warning",duration=6)
        return(NULL)
      }
      attr(dat,"feature_notes")<-NULL

      attr(dat,"bag")<-if(nzchar(input$new_name))input$new_name else paste0(input$data_x,"_derived_features")
      attr(dat,"action")<-"datalist"
      attr(dat,"datalist_root")<-input$data_x
      attr(dat,"new_datalist")<-input$data_x
      dat
    }

    observeEvent(input$add_memory,{
      add_recipe_step(
        "memory",
        "Memory",
        list(lags=input$lags)
      )
    })

    observeEvent(input$add_lead,{
      add_recipe_step(
        "lead",
        "Lead",
        list(leads=input$leads)
      )
    })

    observeEvent(input$add_trend,{
      add_recipe_step(
        "trend",
        "Trend",
        list(windows=input$trend_windows,summaries=input$trend_stats)
      )
    })

    observeEvent(input$add_change,{
      add_recipe_step(
        "change",
        "Change",
        list(lags=input$change_lags,types=input$change_types)
      )
    })

    observeEvent(input$add_anomaly,{
      add_recipe_step(
        "anomaly",
        "Anomaly",
        list(windows=input$anomaly_windows,types=input$anomaly_types)
      )
    })

    observeEvent(input$add_led,{
      add_recipe_step(
        "led",
        "LED",
        list(alpha=input$led_alpha,initial=input$led_init,center=isTRUE(input$led_center))
      )
    })

    observeEvent(input$add_cumulative,{
      add_recipe_step(
        "cumulative",
        "Cumulative",
        list(types=input$cumulative_types)
      )
    })

    observeEvent(input$add_seasonality,{
      add_recipe_step(
        "seasonality",
        "Seasonality",
        list(time_col=input$time_col,terms=input$seasonal_terms,periods=input$seasonal_periods)
      )
    })

    observeEvent(input$clear_recipe,{
      feature_recipe(list())
    })

    observeEvent(input$preview_features,{
      preview_data()
    })

    observeEvent(input$run_features,{
      dat<-prepare_feature_result()
      req(dat)

      vals$newdatalist<-dat
      vals$tosave<-dat
      vals$bagname<-attr(dat,"bag")

      module_save_changes$ui(session$ns("derived_features_save"),vals)
    })

    module_save_changes$server("derived_features_save",vals)

    output$derived_recipe<-renderUI({
      steps<-feature_recipe()
      if(length(steps)==0){
        return(emgray("No derived feature step added yet."))
      }
      tags$ol(lapply(seq_along(steps),function(i){
        step<-steps[[i]]
        details<-paste(
          paste0("variables: ",length(step$vars)),
          paste0("prefix: ",ifelse(nzchar(step$prefix),step$prefix,"none")),
          sep="; "
        )
        tags$li(strong(step$label),tags$span(style="font-size: 11px; margin-left: 5px",details))
      }))
    })

    output$derived_summary<-renderUI({
      dat<-data_x()
      preview<-preview_data()
      steps<-feature_recipe()
      if(inherits(preview,"error")){
        return(div(strong("Preview error:"),conditionMessage(preview)))
      }
      div(
        div(em("selected variables:"),length(feature_vars())),
        div(em("recipe steps:"),length(steps)),
        lapply(attr(preview,"feature_notes"),function(note_i){
          div(style="color:brown; font-size:11px; line-height:1.2; margin-top:4px",icon("triangle-exclamation"),note_i)
        })
      )
    })

    output$derived_preview<-renderUI({
      dat<-data_x()
      req(dat)
      div(
        style="margin-top: 10px",
        strong("Preview"),
        div(
          style="max-height: 300px; overflow-y: auto; margin-top: 6px;",
          tableOutput(session$ns("derived_preview_table"))
        )
      )
    })

    output$derived_preview_table<-renderTable({
      dat<-preview_data()
      if(inherits(dat,"error")){
        return(data.frame(Error=conditionMessage(dat)))
      }
      head(dat,10)
    },rownames=TRUE)







  })
}

# Spatio-temporal Feature Builder
tool2_tab11<-list()
tool2_tab11$ui<-function(id){
  ns<-NS(id)
  nav_tip<-function(label, help){
    span(label,span(style="margin-left:4px; font-size:12px;",tiphelp(help,"right")))
  }
  leak_warning<-function(text,...){
    div(
      style="color:brown; font-size:11px; line-height:1.2; margin:4px 0 7px 0;",
      icon("triangle-exclamation"),
      span(text,...)
    )
  }
  div(class='tool2_tab9',
      div(
        class="tool11 half-drop-inline tool2_tab10 ",style="overflow-y: scroll; height: 100vh",
        div(style="position: fixed; right: 80vw;top: 50px ",
            actionButton(ns("exit_tool11"), label = NULL, icon = icon("times"), style = "padding: 0px; font-size: 15px; width: 20px; height: 20px;background: Brown; color: white; border: 0px;")),
        tags$style(HTML("
          .tool2_tab11 .st-nav { background: whitesmoke; padding: 8px; overflow-y: scroll; height: 200px; }
          .tool2_tab11 .feature-subtitle { font-weight: 700; margin: 4px 0 6px 0; }
        ")),
        div(class="tool2_tab11",
            h3(strong("Spatio-temporal Features")),
            column(6,class="mp0",
                   box_caret(
                     ns('st-setup'),
                     title="Setup",
                     color="#c3cc74ff",
                     div(
                       pickerInput_fromtop_live(
                         ns("data_x"),
                         tiphelp5("Datalist","Select the datalist used to create spatial or spatio-temporal derived variables."),
                         choices=NULL
                       ),
                       pickerInput_fromtop_live(
                         ns("vars"),
                         tiphelp5("Variables","Select numeric variables used to create contextual predictors. Leave empty to use all numeric columns."),
                         choices=NULL,
                         multiple=TRUE,
                         options=pickerOptions(actionsBox=T)
                       ),
                       textInput(
                         ns("prefix"),
                         tiphelp5("Prefix","Optional prefix added to the generated variable names."),
                         value=""
                       ),
                       uiOutput(ns("time_col_ui")),
                       uiOutput(ns("coords_status_ui"))
                     )
                   ),
                   box_caret(
                     ns('st-steps'),
                     title="Feature steps",
                     color="#c3cc74ff",
                     div(
                       navlistPanel(
                         widths=c(3,9),
                         well=FALSE,
                         selected="nearest",
                         tabPanel(
                           nav_tip("Nearest","Creates nearest-neighbor context, such as nearest value and distance. Neighbor values can leak information if those values would not be available at prediction time."),
                           value="nearest",
                           div(class="st-nav",
                               leak_warning("Leakage risk: nearest-neighbor values should only be used when neighboring values are available at prediction time. Distance-only features are safe.", tiphelp_icon(icon("plus"),"neighbor values can make model validation overly optimistic if they use values from observations that would not be known at prediction time, especially from the test set or future samples. Use them only when neighbor values are truly available. Distance-only features are safe because they use geometry only","right")),
                               numericInput(ns("nearest_k"),tiphelp5("Neighbor rank","Use 1 for the closest neighbor, 2 for the second closest, and so on."),value=1,min=1,step=1),
                               checkboxInput(ns("nearest_value"),tiphelp5("Value","Create the selected variable value observed at the nearest neighbor. Avoid using this for response variables or unavailable predictors."),value=TRUE),
                               checkboxInput(ns("nearest_distance"),tiphelp5("Distance","Create distance to the nearest neighbor."),value=TRUE),
                               div(class="feature-actions save_changes",actionButton(ns("add_nearest"),span("Add",icon("plus"))))
                           )
                         ),
                         tabPanel(
                           nav_tip("Spatial context","Summarizes nearby observations in space using k nearest neighbors or a distance radius. This is a spatial/contextual summary, not a temporal lag. Same-time summaries can leak information in forecasting settings."),
                           value="spatial_lag",
                           div(class="st-nav",
                               leak_warning(
                                 "Leakage risk: same-time neighbor summaries should only be used when neighboring values from the same time step are known at prediction time.",
                                 tiphelp_icon(
                                   icon("plus"),
                                   "Same-time spatial summaries can make model validation overly optimistic if they use values from observations that would not be available when making a real prediction, especially values from the test set or unsampled locations. They are appropriate for interpolation or contextual modelling only when neighboring observations from the same time step are truly known. For forecasting, prefer past space-time lag features.",
                                   "right"
                                 )
                               ),
                               radioButtons(ns("spatial_mode"),tiphelp5("Neighbors","Use k nearest neighbors or all neighbors inside a distance radius."),choices=c("k nearest"="knn","radius"="radius"),selected="knn",inline=TRUE),
                               div(id=ns("spatial_k_box"),
                                   numericInput(ns("spatial_k"),tiphelp5("k","Number of nearest neighbors."),value=5,min=1,step=1)
                               ),
                               div(id=ns("spatial_radius_box"),
                                   numericInput(ns("spatial_radius"),tiphelp5("Radius","Distance radius in coordinate units."),value=1,min=0,step=.1)
                               ),
                               checkboxInput(ns("spatial_same_time"),tiphelp5("Same time","If a temporal column exists, summarize only neighbors observed at the same time. This may leak information in temporal prediction if same-time neighbor values are unknown."),value=FALSE),
                               checkboxGroupInput(ns("spatial_stats"),tiphelp5("Summaries","Summaries calculated from neighbor values."),choices=c("Mean"="mean","Median"="median","SD"="sd","Min"="min","Max"="max"),selected=c("mean")),
                               div(class="feature-actions save_changes",actionButton(ns("add_spatial_lag"),span("Add",icon("plus"))))
                           )
                         ),
                         tabPanel(
                           nav_tip("Local anomaly","Compares each observation with its spatial neighborhood. Use carefully because it depends on the current observation and neighborhood values."),
                           value="spatial_anomaly",
                           div(class="st-nav",
                               leak_warning(
                                 "Leakage risk: local anomaly is mainly diagnostic. Use it as a predictor only when both the current value and neighbor values are available at prediction time.",
                                 tiphelp_icon(
                                   icon("plus"),
                                   "Local anomaly compares the current observation with its spatial neighborhood, for example current value minus neighborhood mean or median. This can leak information when it is computed from the response variable, from test-set observations, or from same-time neighbor values that would not be known in a real prediction. It is safer for exploratory diagnosis or contextual interpretation than for forecasting. For prediction, use it only with predictors that are truly available at prediction time.",
                                   "right"
                                 )
                               ),
                               radioButtons(ns("anom_mode"),tiphelp5("Neighbors","Use k nearest neighbors or all neighbors inside a distance radius."),choices=c("k nearest"="knn","radius"="radius"),selected="knn",inline=TRUE),
                               div(id=ns("anom_k_box"),
                                   numericInput(ns("anom_k"),tiphelp5("k","Number of nearest neighbors."),value=5,min=1,step=1)
                               ),
                               div(id=ns("anom_radius_box"),
                                   numericInput(ns("anom_radius"),tiphelp5("Radius","Distance radius in coordinate units."),value=1,min=0,step=.1)
                               ),
                               checkboxGroupInput(ns("anom_types"),tiphelp5("Types","Current value minus neighborhood mean or median."),choices=c("Current minus spatial mean"="mean","Current minus spatial median"="median"),selected="mean"),
                               div(class="feature-actions save_changes",actionButton(ns("add_spatial_anomaly"),span("Add",icon("plus"))))
                           )
                         ),
                         tabPanel(
                           nav_tip("Space-time lag","Summarizes spatial neighbors at previous times. The default uses ordered observed time levels; real interval mode subtracts the typed interval from the temporal value itself. Useful for prediction, because it uses past neighborhood context."),
                           value="st_lag",
                           div(class="st-nav",
                               div(style="color:#2f6f3e; font-size:11px; line-height:1.2; margin:4px 0 7px 0;",icon("circle-check"),span("Prediction-safe when the temporal column is correctly ordered and lagged values are available at prediction time.")),
                               radioButtons(ns("st_lag_basis"),tiphelp5("Lag basis","Observed time levels use the ordered temporal values already present in the data. Real interval subtracts the typed lag from the temporal value itself."),choices=c("Observed time levels"="observed","Real time interval"="interval"),selected="observed",inline=TRUE),
                               textInput(ns("st_lags"),tiphelp5("Temporal lags","Temporal lag steps or intervals separated by commas. Example: 1, 2. With observed levels, lag 1 means the previous observed time level. With real interval, lag 1 subtracts one time unit from the temporal value."),value="1"),
                               div(id=ns("st_interval_options"),
                                   numericInput(ns("st_interval_tolerance"),tiphelp5("Match tolerance","Tolerance used to match real interval targets. Units follow the temporal column: numeric units for numeric time, days for Date, seconds for POSIX date-time."),value=0,min=0,step=0.01)
                               ),
                               radioButtons(ns("st_mode"),tiphelp5("Neighbors","Use k nearest neighbors or all neighbors inside a distance radius."),choices=c("k nearest"="knn","radius"="radius"),selected="knn",inline=TRUE),
                               div(id=ns("st_k_box"),
                                   numericInput(ns("st_k"),tiphelp5("k","Number of nearest neighbors at the lagged time."),value=5,min=1,step=1)
                               ),
                               div(id=ns("st_radius_box"),
                                   numericInput(ns("st_radius"),tiphelp5("Radius","Distance radius in coordinate units."),value=1,min=0,step=.1)
                               ),
                               checkboxGroupInput(ns("st_stats"),tiphelp5("Summaries","Summaries calculated from lagged neighbor values."),choices=c("Mean"="mean","Median"="median","SD"="sd"),selected="mean"),
                               div(class="feature-actions save_changes",actionButton(ns("add_st_lag"),span("Add",icon("plus"))))
                           )
                         ),
                         tabPanel(
                           nav_tip("Distance","Creates geometry-only distance predictors from the coordinate attribute."),
                           value="distance",
                           div(class="st-nav",
                               checkboxGroupInput(ns("distance_types"),tiphelp5("Types","Distance-based predictors that do not use response values."),choices=c("Nearest-neighbor distance"="nearest","Distance to centroid"="centroid"),selected=c("nearest","centroid")),
                               div(class="feature-actions save_changes",actionButton(ns("add_distance"),span("Add",icon("plus"))))
                           )
                         )
                       )
                     )
                   )
            ),
            column(6,class="mp0",
                   box_caret(
                     ns("st-output"),
                     title="Output",
                     div(
                       radioButtons(
                         ns("output_mode"),
                         tiphelp5("Mode","Append keeps the current datalist and adds the derived variables. New datalist saves only the derived variables."),
                         choices=c("Append"="append","New datalist"="new"),
                         selected="append",
                         inline=TRUE
                       ),
                       textInput(ns("new_name"),tiphelp5("New name","Name used when saving the result as a new datalist."),value="spatiotemporal_features"),
                       div(class="feature-actions save_changes",
                           actionButton(ns("preview_features"),span("Preview",icon("eye"))),
                           actionButton(ns("run_features"),span("Create",icon("angles-right")))
                       ),
                       div(class="recipe-box",
                           div(class="feature-section-title","Recipe"),
                           uiOutput(ns("st_recipe")),
                           div(class="feature-actions",actionLink(ns("clear_recipe"),"clear"))
                       ),
                       uiOutput(ns("st_summary")),
                       div(style="overflow-y: scroll",uiOutput(ns("st_preview")))
                     )
                   )
            )
        )
      ))
}

tool2_tab11$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    observeEvent(input$exit_tool11,{
      vals$exit_tool11<-input$exit_tool11
    })

    data_x<-reactive(vals$saved_data[[input$data_x]])

    observeEvent(vals$saved_data,{
      updatePickerInput(session,'data_x',choices=names(vals$saved_data),selected=vals$cur_data)
    })

    observeEvent(input$data_x,{
      vals$cur_data<-input$data_x
      dat<-data_x()
      num_cols<-colnames(dat)[vapply(dat,is.numeric,logical(1))]
      updatePickerInput(session,'vars',choices=num_cols,selected=num_cols[1])
    })

    observe({
      shinyjs::toggle("st_interval_options",condition=identical(input$st_lag_basis,"interval"))
    })

    observe({
      shinyjs::toggle("spatial_k_box",condition=identical(input$spatial_mode,"knn"))
      shinyjs::toggle("spatial_radius_box",condition=identical(input$spatial_mode,"radius"))
      shinyjs::toggle("anom_k_box",condition=identical(input$anom_mode,"knn"))
      shinyjs::toggle("anom_radius_box",condition=identical(input$anom_mode,"radius"))
      shinyjs::toggle("st_k_box",condition=identical(input$st_mode,"knn"))
      shinyjs::toggle("st_radius_box",condition=identical(input$st_mode,"radius"))
    })

    output$time_col_ui<-renderUI({
      dat<-data_x()
      time_attr<-attr(dat,"time")
      if(is.null(time_attr)||!ncol(as.data.frame(time_attr))){
        return(NULL)
      }
      pickerInput_fromtop_live(
        session$ns("time_col"),
        tiphelp5("Temporal column","Optional temporal attribute used by same-time spatial summaries and space-time lag features."),
        choices=colnames(time_attr),
        selected=colnames(time_attr)[1]
      )
    })

    output$coords_status_ui<-renderUI({
      dat<-data_x()
      coords<-attr(dat,"coords")
      if(is.null(coords)||nrow(as.data.frame(coords))!=nrow(dat)||ncol(as.data.frame(coords))<2){
        return(div(em("Coords-Attribute is required for spatial features.",style="color: brown")))
      }
      div(em("Coords-Attribute detected."))
    })

    st_recipe<-reactiveVal(list())

    feature_vars<-reactive({
      dat<-data_x()
      vars<-input$vars
      if(length(vars)==0){
        vars<-colnames(dat)[vapply(dat,is.numeric,logical(1))]
      }
      vars
    })

    add_step<-function(type,label,settings){
      steps<-st_recipe()
      steps[[length(steps)+1]]<-list(type=type,label=label,vars=feature_vars(),prefix=input$prefix,settings=settings)
      st_recipe(steps)
    }

    parse_numeric_list<-function(x,default=NULL){
      if(is.null(x)||!nzchar(trimws(x))) return(default)
      vals<-suppressWarnings(as.numeric(trimws(unlist(strsplit(x,",")))))
      vals[!is.na(vals)]
    }

    feature_name<-function(prefix,var,suffix,existing){
      nm<-paste0(ifelse(nzchar(prefix),paste0(prefix,"_"),""),var,"_",suffix)
      make.unique(c(existing,nm))[length(existing)+1]
    }

    safe_stat<-function(x,fun){
      x<-x[!is.na(x)]
      if(!length(x)) return(NA_real_)
      val<-suppressWarnings(fun(x))
      if(!length(val)) return(NA_real_)
      val<-as.numeric(val[1])
      if(is.na(val)||is.nan(val)||is.infinite(val)) return(NA_real_)
      val
    }

    coords_matrix<-function(dat){
      coords<-attr(dat,"coords")
      if(is.null(coords)) return(NULL)
      coords<-as.data.frame(coords)
      if(nrow(coords)!=nrow(dat)||ncol(coords)<2) return(NULL)
      as.matrix(coords[,1:2,drop=FALSE])
    }

    time_vector<-function(dat){
      time_attr<-attr(dat,"time")
      time_col<-input$time_col
      if(is.null(time_attr)||length(time_col)!=1||!time_col%in%colnames(time_attr)) return(NULL)
      ids<-rownames(dat)
      if(!is.null(ids)&&all(ids%in%rownames(time_attr))) return(time_attr[ids,time_col,drop=TRUE])
      if(nrow(time_attr)!=nrow(dat)) return(NULL)
      time_attr[[time_col]]
    }

    time_numeric<-function(tt){
      if(inherits(tt,"Date")||inherits(tt,"POSIXt")||is.numeric(tt)||is.integer(tt)){
        return(as.numeric(tt))
      }
      tt_chr<-as.character(tt)
      non_missing<-!is.na(tt_chr)
      tt_num<-suppressWarnings(as.numeric(tt_chr))
      if(any(non_missing)&&all(!is.na(tt_num[non_missing]))){
        return(tt_num)
      }
      guess<-guess_time_settings(tt)
      if(!guess$type%in%c("date","datetime")){
        return(NULL)
      }
      tt_date<-convert_time_column(tt,guess$type,guess$format,guess$custom)
      if(any(non_missing)&&all(!is.na(tt_date[non_missing]))){
        return(as.numeric(tt_date))
      }
      NULL
    }

    dist_matrix<-function(coords){
      as.matrix(stats::dist(coords,upper=TRUE,diag=TRUE))
    }

    neighbor_index<-function(i,dmat,mode="knn",k=5,radius=1,candidates=NULL){
      idx<-seq_len(nrow(dmat))
      if(!is.null(candidates)) idx<-intersect(idx,candidates)
      idx<-setdiff(idx,i)
      if(!length(idx)) return(integer(0))
      dd<-dmat[i,idx]
      ok<-!is.na(dd)
      idx<-idx[ok]
      dd<-dd[ok]
      if(!length(idx)) return(integer(0))
      if(mode=="radius"){
        return(idx[dd<=radius])
      }
      idx[order(dd)][seq_len(min(k,length(idx)))]
    }

    summarize_neighbors<-function(x,idx,stat){
      vals_i<-x[idx]
      switch(stat,
             mean=safe_stat(vals_i,mean),
             median=safe_stat(vals_i,stats::median),
             sd=safe_stat(vals_i,stats::sd),
             min=safe_stat(vals_i,min),
             max=safe_stat(vals_i,max),
             safe_stat(vals_i,mean))
    }

    apply_spatial_summary<-function(dat,x,mode,k,radius,stats,same_time=FALSE){
      coords<-coords_matrix(dat)
      if(is.null(coords)) return(NULL)
      dmat<-dist_matrix(coords)
      tt<-if(isTRUE(same_time)) time_vector(dat) else NULL
      out<-setNames(vector("list",length(stats)),stats)
      for(stat in stats) out[[stat]]<-rep(NA_real_,nrow(dat))
      for(i in seq_len(nrow(dat))){
        candidates<-NULL
        if(!is.null(tt)) candidates<-which(tt==tt[i])
        idx<-neighbor_index(i,dmat,mode,k,radius,candidates)
        for(stat in stats) out[[stat]][i]<-summarize_neighbors(x,idx,stat)
      }
      out
    }

    apply_nearest<-function(dat,x,k){
      coords<-coords_matrix(dat)
      if(is.null(coords)) return(NULL)
      dmat<-dist_matrix(coords)
      val<-dist<-rep(NA_real_,nrow(dat))
      for(i in seq_len(nrow(dat))){
        idx<-neighbor_index(i,dmat,"knn",k,1)
        if(length(idx)){
          idx1<-idx[length(idx)]
          val[i]<-x[idx1]
          dist[i]<-dmat[i,idx1]
        }
      }
      list(value=val,distance=dist)
    }

    apply_distance<-function(dat,type){
      coords<-coords_matrix(dat)
      if(is.null(coords)) return(NULL)
      if(type=="centroid"){
        center<-colMeans(coords,na.rm=TRUE)
        return(sqrt(rowSums((coords-matrix(center,nrow(coords),2,byrow=TRUE))^2)))
      }
      dmat<-dist_matrix(coords)
      out<-rep(NA_real_,nrow(dat))
      for(i in seq_len(nrow(dat))){
        idx<-neighbor_index(i,dmat,"knn",1,1)
        if(length(idx)) out[i]<-dmat[i,idx[1]]
      }
      out
    }

    lag_label<-function(lag_i){
      lbl<-format(lag_i,trim=TRUE,scientific=FALSE)
      lbl<-gsub("[^A-Za-z0-9]+","p",lbl)
      if(!nzchar(lbl)) lbl<-"0"
      lbl
    }

    apply_st_lag<-function(dat,x,lags,mode,k,radius,stats,lag_basis="observed",tolerance=0){
      coords<-coords_matrix(dat)
      tt<-time_vector(dat)
      if(is.null(coords)||is.null(tt)) return(NULL)
      dmat<-dist_matrix(coords)
      interval_mode<-identical(lag_basis,"interval")
      if(interval_mode){
        tt_num<-time_numeric(tt)
        if(is.null(tt_num)) return(NULL)
        tolerance<-suppressWarnings(as.numeric(tolerance)[1])
        if(is.na(tolerance)||tolerance<0) tolerance<-0
      }else{
        time_levels<-sort(unique(tt))
      }
      out<-list()
      for(lag_i in lags){
        for(stat in stats){
          nm<-paste0(if(interval_mode) "interval" else "lag",lag_label(lag_i),"_",stat)
          out[[nm]]<-rep(NA_real_,nrow(dat))
        }
        for(i in seq_len(nrow(dat))){
          if(interval_mode){
            if(is.na(tt_num[i])) next
            target_time<-tt_num[i]-lag_i
            eps<-sqrt(.Machine$double.eps)*max(1,abs(target_time))
            candidates<-which(!is.na(tt_num)&abs(tt_num-target_time)<=max(tolerance,eps))
          }else{
            pos<-match(tt[i],time_levels)
            if(is.na(pos)||pos-lag_i<1) next
            candidates<-which(tt==time_levels[pos-lag_i])
          }
          idx<-neighbor_index(i,dmat,mode,k,radius,candidates)
          for(stat in stats){
            nm<-paste0(if(interval_mode) "interval" else "lag",lag_label(lag_i),"_",stat)
            out[[nm]][i]<-summarize_neighbors(x,idx,stat)
          }
        }
      }
      out
    }

    apply_st_recipe<-function(dat,steps,include_originals=TRUE){
      if(is.null(dat)) return(NULL)
      out<-data.frame(dat,check.names=FALSE)
      original_names<-colnames(out)
      if(!length(steps)) return(out)

      for(step in steps){
        vars<-intersect(step$vars,colnames(out))
        prefix<-step$prefix

        if(step$type=="nearest"){
          k<-max(1,as.integer(step$settings$k))
          for(var in vars){
            res<-apply_nearest(dat,as.numeric(out[[var]]),k)
            if(is.null(res)) next
            if(isTRUE(step$settings$value)){
              out[[feature_name(prefix,var,paste0("nearest",k),colnames(out))]]<-res$value
            }
            if(isTRUE(step$settings$distance)){
              out[[feature_name(prefix,var,paste0("nearest",k,"_dist"),colnames(out))]]<-res$distance
            }
          }
        }

        if(step$type=="spatial_lag"){
          stats<-step$settings$stats
          if(is.null(stats)||!length(stats)) stats<-"mean"
          for(var in vars){
            res<-apply_spatial_summary(dat,as.numeric(out[[var]]),step$settings$mode,step$settings$k,step$settings$radius,stats,step$settings$same_time)
            if(is.null(res)) next
            for(stat in names(res)){
              suffix<-paste0("spatial_",step$settings$mode,"_",stat)
              out[[feature_name(prefix,var,suffix,colnames(out))]]<-res[[stat]]
            }
          }
        }

        if(step$type=="spatial_anomaly"){
          types<-step$settings$types
          if(is.null(types)||!length(types)) types<-"mean"
          for(var in vars){
            x<-as.numeric(out[[var]])
            res<-apply_spatial_summary(dat,x,step$settings$mode,step$settings$k,step$settings$radius,types,FALSE)
            if(is.null(res)) next
            for(type_i in names(res)){
              suffix<-paste0("spatial_anom_",type_i)
              out[[feature_name(prefix,var,suffix,colnames(out))]]<-x-res[[type_i]]
            }
          }
        }

        if(step$type=="st_lag"){
          lags<-parse_numeric_list(step$settings$lags,1)
          if(identical(step$settings$lag_basis,"interval")){
            lags<-unique(lags[lags>0])
          }else{
            lags<-unique(as.integer(lags[lags>0]))
            lags<-lags[lags>0]
          }
          stats<-step$settings$stats
          if(is.null(stats)||!length(stats)) stats<-"mean"
          for(var in vars){
            res<-apply_st_lag(dat,as.numeric(out[[var]]),lags,step$settings$mode,step$settings$k,step$settings$radius,stats,step$settings$lag_basis,step$settings$tolerance)
            if(is.null(res)) next
            for(nm in names(res)){
              out[[feature_name(prefix,var,paste0("st_",nm),colnames(out))]]<-res[[nm]]
            }
          }
        }

        if(step$type=="distance"){
          types<-step$settings$types
          if(is.null(types)||!length(types)) types<-"nearest"
          for(type_i in types){
            res<-apply_distance(dat,type_i)
            if(is.null(res)) next
            out[[feature_name(prefix,"coords",paste0(type_i,"_dist"),colnames(out))]]<-res
          }
        }
      }

      generated_names<-setdiff(colnames(out),original_names)
      if(!isTRUE(include_originals)){
        out<-out[,generated_names,drop=FALSE]
      }
      attrs<-attributes(dat)
      attrs$names<-names(out)
      attrs$row.names<-attr(out,"row.names")
      attrs$class<-class(out)
      attrs$derived_feature_names<-generated_names
      attributes(out)<-attrs
      out
    }

    preview_data<-reactive({
      tryCatch(
        apply_st_recipe(data_x(),st_recipe(),include_originals=identical(input$output_mode,"append")),
        error=function(e)e
      )
    })

    prepare_result<-function(){
      dat<-preview_data()
      if(inherits(dat,"error")){
        showNotification(conditionMessage(dat),type="error",duration=8)
        return(NULL)
      }
      if(!length(st_recipe())){
        showNotification("Add at least one spatio-temporal feature step before creating the result.",type="warning",duration=6)
        return(NULL)
      }
      generated_names<-attr(dat,"derived_feature_names")
      if(is.null(generated_names)||!length(generated_names)){
        showNotification("No spatial feature was generated. Check coords, time, selected variables and recipe settings.",type="warning",duration=6)
        return(NULL)
      }
      attr(dat,"bag")<-if(nzchar(input$new_name)) input$new_name else paste0(input$data_x,"_spatiotemporal_features")
      attr(dat,"action")<-"datalist"
      attr(dat,"datalist_root")<-input$data_x
      attr(dat,"new_datalist")<-input$data_x
      dat
    }

    observeEvent(input$add_nearest,{
      if(isTRUE(input$nearest_value)){
        showNotification("Leakage warning: nearest-neighbor values should only be used if those neighbor values are available at prediction time. Distance features are safer.",type="warning",duration=8)
      }
      add_step("nearest","Nearest",list(k=input$nearest_k,value=isTRUE(input$nearest_value),distance=isTRUE(input$nearest_distance)))
    })
    observeEvent(input$add_spatial_lag,{
      if(isTRUE(input$spatial_same_time)){
        showNotification("Leakage warning: same-time spatial summaries can leak information in temporal prediction if neighbor values are not available at prediction time.",type="warning",duration=8)
      }else{
        showNotification("Check leakage: spatial summaries use neighbor values from the available dataset. Use only predictors that would be known at prediction time.",type="message",duration=7)
      }
      add_step("spatial_lag","Spatial context",list(mode=input$spatial_mode,k=input$spatial_k,radius=input$spatial_radius,stats=input$spatial_stats,same_time=isTRUE(input$spatial_same_time)))
    })
    observeEvent(input$add_spatial_anomaly,{
      showNotification("Leakage warning: local anomaly is usually diagnostic/contextual. Avoid using it as a predictor when computed from the response or unavailable same-time values.",type="warning",duration=8)
      add_step("spatial_anomaly","Local anomaly",list(mode=input$anom_mode,k=input$anom_k,radius=input$anom_radius,types=input$anom_types))
    })
    observeEvent(input$add_st_lag,{
      if(identical(input$st_lag_basis,"interval")&&is.null(time_numeric(time_vector(data_x())))){
        showNotification("Real interval lag requires a numeric, Date, POSIX date-time, or date-like Temporal-Attribute column.",type="warning",duration=8)
        return(NULL)
      }
      st_label<-if(identical(input$st_lag_basis,"interval")) "Space-time lag (real interval)" else "Space-time lag"
      add_step("st_lag",st_label,list(lags=input$st_lags,lag_basis=input$st_lag_basis,tolerance=input$st_interval_tolerance,mode=input$st_mode,k=input$st_k,radius=input$st_radius,stats=input$st_stats))
    })
    observeEvent(input$add_distance,{
      add_step("distance","Distance",list(types=input$distance_types))
    })
    observeEvent(input$clear_recipe,{ st_recipe(list()) })
    observeEvent(input$preview_features,{ preview_data() })
    observeEvent(input$run_features,{
      dat<-prepare_result()
      req(dat)
      vals$newdatalist<-dat
      vals$tosave<-dat
      vals$bagname<-attr(dat,"bag")
      module_save_changes$ui(session$ns("st_features_save"),vals)
    })
    module_save_changes$server("st_features_save",vals)

    output$st_recipe<-renderUI({
      steps<-st_recipe()
      if(!length(steps)) return(emgray("No spatio-temporal feature step added yet."))
      tags$ol(lapply(seq_along(steps),function(i){
        step<-steps[[i]]
        details<-paste(paste0("variables: ",length(step$vars)),paste0("prefix: ",ifelse(nzchar(step$prefix),step$prefix,"none")),sep="; ")
        tags$li(strong(step$label),tags$span(style="font-size: 11px; margin-left: 5px",details))
      }))
    })

    output$st_summary<-renderUI({
      preview<-preview_data()
      if(inherits(preview,"error")) return(div(strong("Preview error:"),conditionMessage(preview)))
      div(div(em("selected variables:"),length(feature_vars())),div(em("recipe steps:"),length(st_recipe())))
    })

    output$st_preview<-renderUI({
      dat<-data_x()
      req(dat)
      div(style="margin-top: 10px",strong("Preview"),div(style="max-height: 300px; overflow-y: auto; margin-top: 6px;",tableOutput(session$ns("st_preview_table"))))
    })

    output$st_preview_table<-renderTable({
      dat<-preview_data()
      if(inherits(dat,"error")) return(data.frame(Error=conditionMessage(dat)))
      head(dat,10)
    },rownames=TRUE)
  })
}

# SMOTE
tool2_tab12 <- list()
tool2_tab12$ui <- function(id) {
  ns <- NS(id)
  smote_help_label <- function(input_id, label) {
    span(
      label,
      tiphelp_icon(
        actionLink(
          ns(input_id),
          label = NULL,
          icon = icon("fas fa-question-circle"),
          style = "margin-left: 5px; font-size: 13px;"
        ),
        "Click for more details",
        "right"
      )
    )
  }

  div(
    #class = "tool2_tab9",
    div(
      class = "tool10 tool2_tab10",
      style = "overflow-y: scroll; height: 100vh",
      div(style = "position: fixed; right: 80vw;top: 50px ",
          actionButton(ns("exit_tool12"), label = NULL, icon = icon("times"), style = "padding: 0px; font-size: 15px; width: 20px; height: 20px;background: Brown; color: white; border: 0px;")),
      h4(
        strong("SMOTE"),
        tiphelp_icon(
          actionLink(
            ns("smote_title_help"),
            label = NULL,
            icon = icon("fas fa-question-circle"),
            style = "margin-left: 8px; ;"
          ),
          "Click for more details",
          "right"
        )
      ),

      column(
        7,class="mp0",
        div(
          class="model_setup",
          box_caret(ns('0'),inline=F,
                    title="Setup",
                    div(
                      div(class="inline_pickers",
                          radioButtons(
                            ns("problem_type"),
                            smote_help_label("task_help", "Task"),
                            choices = c("Classification" = "classification", "Regression" = "regression"),
                            selected = "classification",
                            inline = TRUE
                          )),
                      div(
                        style="display: flex;gap: 10px;align-items: flex-start",class="setup_box",
                        #div(tags$div("X",class="trailab")),
                        div(class="picker-flex picker-before-x",
                            div(
                              class="data_x",
                              pickerInput_fromtop_live(
                                ns("data_x"),
                                tiphelp5("Datalist X", "Datalist containing the predictor variables used to generate synthetic observations. Coords-Attribute and Temporal-Attribute are taken from this Datalist."),
                                choices = NULL
                              )
                            ),

                        ),
                        div(style="display: flex;gap: 10px;",
                            div(class="picker-flex",
                                uiOutput(ns("numeric_vars_ui")))
                        )
                      ),
                      div(
                        style="display: flex; gap: 10px; align-items: flex-start",class="setup_box",
                        id=ns("model_y_panel"),
                        div(
                          div(class="picker-flex picker-before-y",
                              pickerInput_fromtop(
                                ns("data_y"),
                                tiphelp5("Datalist Y", "Datalist containing the response variable. Only Datalists with all X observation IDs are shown."),
                                choices = NULL
                              ),

                          )
                        ),
                        tags$label("::",style="padding-top: 30px;"),
                        div(class="picker-flex",
                            uiOutput(ns("target_ui"))

                        )
                      ),
                      uiOutput(ns("metadata_options_ui"))
                    )
          )),
        box_caret(
          ns("smote_settings"),
          title = "SMOTE settings",
          color = "#c3cc74ff",
          div(style="display: flex",
              div(
                uiOutput(ns("smote_settings_ui")),
                checkboxInput(
                  ns("scale"),
                  tiphelp5("Scale variables internally", "Recommended when numeric variables use different units or scales."),
                  value = TRUE
                )
              ),
              div(

                actionButton(ns("preview_smote"), "Preview", style="height: 30px"),

                div(
                  class = "save_changes",
                  actionButton(ns("run_smote"), "Run SMOTE", icon = icon("angles-right"), style="height: 30px; margin-top: 10px")
                )
              )
          )
        )

      ),
      column(5,class="mp0",
             box_caret(
               ns("smote_preview"),
               title = "Preview output",
               color = "#81b37aff",
               div(
                 uiOutput(ns("smote_summary")),
                 div(style = "max-height: 260px; overflow-y: auto", tableOutput(ns("class_table"))),
                 div(style = "max-height: 220px; overflow-y: auto; margin-top: 10px", tableOutput(ns("smote_attr_table")))
               )
             )),



    )
  )
}
tool2_tab12$server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    smote_help_label <- function(input_id, label) {
      span(
        label,
        tiphelp_icon(
          actionLink(
            session$ns(input_id),
            label = NULL,
            icon = icon("fas fa-question-circle"),
            style = "margin-left: 5px; font-size: 13px;"
          ),
          "Click for more details",
          "right"
        )
      )
    }

    smote_help_modal <- function(title, ...) {
      showModal(
        modalDialog(
          title = title,
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "m",
          div(style = "line-height: 1.45; font-size: 13px;", ...)
        )
      )
    }

    observeEvent(input$exit_tool12, {
      vals$exit_tool12 <- input$exit_tool12
    })

    observeEvent(input$smote_title_help, {
      smote_help_modal(
        div(strong("SMOTE"), span(" synthetic observations", style = "font-weight: 400; color: #666;")),
        div(
          style = "border-left: 5px solid #81b37a; background: #f5faf3; padding: 12px 14px; margin-bottom: 12px;",
          h4("What this tool does", style = "margin-top: 0; color: #2f6f3e;"),
          p("SMOTE creates new synthetic rows by interpolating between similar observations. It is useful when important cases are rare, so a model might otherwise learn mostly from the most common cases."),
          p("The new rows are not copied observations. Each one is placed between an observed row and one of its nearest neighbors, using the selected numeric variables."),
          p(strong("Datalist X"), " supplies the predictor variables and, when available, the coordinates and temporal information. ", strong("Datalist Y"), " supplies the response variable. Y must contain all observation IDs present in X; extra rows in Y are ignored.")
        ),
        fluidRow(
          column(
            6,
            div(
              style = "background: #f7f7f7; border-top: 3px solid #05668D; padding: 10px 12px; min-height: 150px;",
              h4("Classification", style = "margin-top: 0; color: #05668D;"),
              p("Use this when the target is a category, such as presence/absence, habitat type, or class label."),
              p("The class must be stored in the Factor-Attribute. SMOTE then adds synthetic rows to under-represented classes.")
            )
          ),
          column(
            6,
            div(
              style = "background: #f7f7f7; border-top: 3px solid #2f6f3e; padding: 10px 12px; min-height: 150px;",
              h4("Regression", style = "margin-top: 0; color: #2f6f3e;"),
              p("Use this when the target is numeric, such as abundance, biomass, chlorophyll, temperature, or productivity."),
              p("SMOTER focuses on rare low and/or high response values, then creates synthetic rows inside those rare ranges.")
            )
          )
        ),
        div(
          style = "margin-top: 12px; padding: 10px 12px; background: #fff8e6; border-left: 5px solid #c3a53b;",
          h4("Important interpretation", style = "margin-top: 0; color: #7a5a00;"),
          p("Synthetic rows help the modelling step see rare situations more often. They should not be interpreted as new field samples or new satellite observations."),
          p("If coordinates or dates are interpolated, they are metadata positions for the synthetic rows. They make the Datalist usable by spatial or temporal tools, but they are still synthetic metadata.")
        ),
        div(
          style = "margin-top: 12px; padding: 10px 12px; background: #f7f7f7; border-left: 5px solid #777;",
          h4("References", style = "margin-top: 0; color: #444;"),
          tags$ul(
            tags$li(
              strong("Classic SMOTE: "),
              "Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). ",
              em("SMOTE: Synthetic Minority Over-sampling Technique. "),
              "Journal of Artificial Intelligence Research, 16, 321-357. https://doi.org/10.1613/JAIR.953"
            ),
            tags$li(
              strong("Borderline-SMOTE: "),
              "Han, H., Wang, W.-Y., & Mao, B.-H. (2005). ",
              em("Borderline-SMOTE: A New Over-Sampling Method in Imbalanced Data Sets Learning. "),
              "In Advances in Intelligent Computing, Lecture Notes in Computer Science, 3644, 878-887. https://doi.org/10.1007/11538059_91"
            ),
            tags$li(
              strong("SMOTE for regression / SMOTER: "),
              "Torgo, L., Ribeiro, R. P., Pfahringer, B., & Branco, P. (2013). ",
              em("SMOTE for Regression. "),
              "In Progress in Artificial Intelligence, Lecture Notes in Computer Science, 8154, 378-389. https://doi.org/10.1007/978-3-642-40669-0_33"
            )
          )
        )
      )
    })

    observeEvent(input$task_help, {
      smote_help_modal(
        "SMOTE task",
        p(strong("Classification"), " uses a categorical column from the Factor-Attribute as the class to balance. Synthetic observations are generated inside each class using Numeric-Attribute variables."),
        p(strong("Regression"), " uses SMOTER. Instead of balancing classes, it identifies rare low and/or high ranges of a numeric response and creates synthetic observations inside those rare regions."),
        p("Use classification when the modelling target is a factor. Use regression when the target is continuous and the important cases are under-represented in one or both tails of the response.")
      )
    })

    observeEvent(input$strategy_help, {
      smote_help_modal(
        "Oversampling strategy",
        p(strong("Balance to largest group"), " increases each selected class or rare region until it has the same size as the largest group."),
        p(strong("Percent per group"), " adds a percentage of synthetic observations relative to the current size of each selected group. With 100%, a group with 20 observations receives 20 synthetic observations."),
        p(strong("Ratio of largest group"), " targets a fraction of the largest group size. A ratio of 0.5 targets half of the largest group; 1 targets equality with the largest group.")
      )
    })

    observeEvent(input$mode_help, {
      smote_help_modal(
        "Classification mode",
        p(strong("Classic SMOTE"), " creates synthetic observations between a minority-class observation and one of its same-class nearest neighbors."),
        p(strong("Borderline-SMOTE 1"), " focuses generation on minority observations near class boundaries, where many neighbors belong to other classes."),
        p(strong("Borderline-SMOTE 2"), " also focuses on boundary observations, but can generate part of the synthetic points toward nearby majority observations. This is more aggressive and should be inspected carefully.")
      )
    })

    observeEvent(input$rare_help, {
      smote_help_modal(
        "Rare response range",
        p("SMOTER needs to know which part of the continuous response is under-represented or analytically important."),
        p(strong("High values"), " oversamples only the upper tail of the response. ", strong("Low values"), " oversamples only the lower tail. ", strong("Low and high values"), " oversamples both tails."),
        p("The rare quantile defines where those tails begin.")
      )
    })

    observeEvent(input$rare_q_help, {
      smote_help_modal(
        "Rare quantile",
        p("The rare quantile sets the fraction of observations treated as rare in each selected tail of the response."),
        p("For example, 0.10 with high values means the highest 10% of response values are treated as rare. With low and high values, the lowest 10% and highest 10% are both treated as rare regions."),
        p("Each selected rare region needs at least two complete observations because SMOTER interpolates between neighboring rare cases.")
      )
    })

    open_metadata_help <- function() {
      smote_help_modal(
        "Synthetic coordinates and dates",
        p("When enabled, Coords-Attribute and Temporal-Attribute values from Datalist X are interpolated between the same seed observation and neighbor used by SMOTE or SMOTER."),
        p("This keeps the output Datalist aligned for spatial or temporal tools, but these values should be interpreted as metadata positions for synthetic observations, not as newly observed sampling locations or dates."),
        p("When disabled, synthetic rows are kept in those attributes with NA values.")
      )
    }

    observeEvent(input$metadata_coords_help, {
      open_metadata_help()
    })

    observeEvent(input$metadata_time_help, {
      open_metadata_help()
    })

    data_x <- reactive({
      req(input$data_x)
      req(input$data_x %in% names(vals$saved_data))
      vals$saved_data[[input$data_x]]
    })

    compatible_y_choices <- reactive({
      req(input$data_x)
      req(input$data_x %in% names(vals$saved_data))
      x_ids <- rownames(vals$saved_data[[input$data_x]])
      names(vals$saved_data)[vapply(vals$saved_data, function(dat_y) {
        all(x_ids %in% rownames(dat_y))
      }, logical(1))]
    })

    observeEvent(vals$saved_data, {
      updatePickerInput(session, "data_x", choices = names(vals$saved_data), selected = vals$cur_data)
    })

    observe({
      choices <- compatible_y_choices()
      selected <- if (!is.null(input$data_y) && input$data_y %in% choices) input$data_y else if (input$data_x %in% choices) input$data_x else choices[1]
      updatePickerInput(session, "data_y", choices = choices, selected = selected)
    })

    data_y <- reactive({
      req(input$data_y)
      req(input$data_y %in% compatible_y_choices())
      vals$saved_data[[input$data_y]]
    })

    factors_y <- reactive({
      factors <- attr(data_y(), "factors")
      validate(need(!is.null(factors) && ncol(factors) > 0, "The selected response Datalist has no Factor-Attribute."))
      factors
    })

    numeric_vars <- reactive({
      dat <- data_x()
      colnames(dat)[vapply(dat, is.numeric, logical(1))]
    })

    numeric_y_vars <- reactive({
      dat <- data_y()
      colnames(dat)[vapply(dat, is.numeric, logical(1))]
    })

    copy_x_attrs <- function(out, x_data, rows) {
      coords <- attr(x_data, "coords")
      time <- attr(x_data, "time")
      if (!is.null(coords)) {
        attr(out, "coords") <- coords[rows, , drop = FALSE]
      }
      if (!is.null(time)) {
        attr(out, "time") <- time[rows, , drop = FALSE]
      }
      attr(out, "base_shape") <- attr(x_data, "base_shape")
      attr(out, "layer_shape") <- attr(x_data, "layer_shape")
      attr(out, "extra_shape") <- attr(x_data, "extra_shape")
      attr(out, "datalist_root") <- attr(x_data, "datalist_root") %||% attr(x_data, "datalist")
      out
    }

    working_datalist <- function(type, vars) {
      x_data <- data_x()
      y_data <- data_y()
      x_ids <- rownames(x_data)

      if (!all(x_ids %in% rownames(y_data))) {
        stop("Datalist Y must contain all observation IDs from Datalist X.")
      }

      if (identical(type, "regression")) {
        req(input$response_var)
        vars <- setdiff(vars, input$response_var)
        out <- x_data[x_ids, vars, drop = FALSE]
        out[[input$response_var]] <- y_data[x_ids, input$response_var, drop = TRUE]
        out <- out[, c(input$response_var, vars), drop = FALSE]
        factors <- attr(x_data, "factors")
        if (!is.null(factors)) {
          attr(out, "factors") <- factors[x_ids, , drop = FALSE]
        }
        return(copy_x_attrs(out, x_data, x_ids))
      }

      req(input$class_factor)
      factors_y <- attr(y_data, "factors")
      if (is.null(factors_y) || !input$class_factor %in% colnames(factors_y)) {
        stop("The selected class factor must exist in Datalist Y.")
      }
      if (!all(x_ids %in% rownames(factors_y))) {
        stop("The Factor-Attribute from Datalist Y must contain all observation IDs from Datalist X.")
      }

      out <- x_data[x_ids, vars, drop = FALSE]
      factors <- attr(x_data, "factors")
      if (is.null(factors)) {
        factors <- data.frame(row.names = x_ids)
      } else {
        factors <- factors[x_ids, , drop = FALSE]
      }
      factors[[input$class_factor]] <- factors_y[x_ids, input$class_factor, drop = TRUE]
      attr(out, "factors") <- factors
      copy_x_attrs(out, x_data, x_ids)
    }

    output$target_ui <- renderUI({
      if (identical(input$problem_type, "regression")) {
        vars <- numeric_y_vars()
        validate(need(length(vars) > 0, "The selected response Datalist has no numeric variables."))
        return(
          pickerInput_fromtop(
            session$ns("response_var"),
            tiphelp5("Response variable", "Numeric-Attribute variable whose rare low and/or high values will be oversampled with SMOTER."),
            choices = vars,
            selected = vars[1]
          )
        )
      }

      factors <- factors_y()
      choices <- colnames(factors)
      pickerInput_fromtop(
        session$ns("class_factor"),
        tiphelp5("Class factor", "Factor-Attribute column containing the classes to balance."),
        choices = choices,
        selected = choices[1]
      )
    })

    output$numeric_vars_ui <- renderUI({
      vars <- numeric_vars()
      validate(need(length(vars) > 0, "The selected Datalist has no numeric variables."))
      if (identical(input$problem_type, "regression")) {
        req(input$response_var)
        vars <- setdiff(vars, input$response_var)
        validate(need(length(vars) > 0, "SMOTER requires at least one numeric predictor besides the response."))
      }
      pickerInput_fromtop(
        session$ns("vars"),
        tiphelp5("Predictors", "Numeric-Attribute variables used as predictors to generate synthetic observations. Leave all selected to use the available Numeric-Attribute variables."),
        choices = vars,
        selected = vars,
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE)
      )
    })

    output$smote_settings_ui <- renderUI({
      common <- list(
        pickerInput(
          session$ns("strategy"),
          smote_help_label("strategy_help", "Strategy"),
          choices = c(
            "Balance to largest group" = "balance",
            "Percent per group" = "percent",
            "Ratio of largest group" = "ratio"
          ),
          selected = "balance",
          width = "250px"
        )
      )

      if (identical(input$problem_type, "regression")) {
        return(
          div(
            common,
            pickerInput(
              session$ns("rare"),
              smote_help_label("rare_help", "Rare range"),
              choices = c("High values" = "upper", "Low values" = "lower", "Low and high values" = "both"),
              selected = "upper",
              width = "250px"
            ),
            div(
              style = "display: flex; gap: 8px; flex-wrap: wrap",
              numericInput(session$ns("rare_q"), smote_help_label("rare_q_help", "Rare quantile"), value = 0.1, min = 0.01, max = 0.49, step = 0.01, width = "160px"),
              numericInput(session$ns("N"), tiphelp5("N (%)", "Oversampling percentage used by the percent strategy."), value = 100, min = 0, step = 10, width = "110px"),
              numericInput(session$ns("ratio"), tiphelp5("Ratio", "Target rare-region size as a fraction of the largest group."), value = 1, min = 0.01, step = 0.05, width = "110px"),
              numericInput(session$ns("seed"), tiphelp5("Seed", "Optional random seed for reproducible synthetic observations."), value = NA, step = 1, width = "110px"),
              numericInput(session$ns("k"), tiphelp5("k", "Number of rare-region neighbors used to generate synthetic observations."), value = 5, min = 1, step = 1, width = "110px"),
              numericInput(session$ns("p"), tiphelp5("p", "Minkowski distance order. Use 2 for Euclidean distance."), value = 2, min = 1, step = 1, width = "110px")
            )
          )
        )
      }

      div(
        common,
        pickerInput(
          session$ns("mode"),
          smote_help_label("mode_help", "Mode"),
          choices = c(
            "Classic SMOTE" = "none",
            "Borderline-SMOTE 1" = "bsmote1",
            "Borderline-SMOTE 2" = "bsmote2"
          ),
          selected = "none",
          width = "250px"
        ),
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap",
          numericInput(session$ns("N"), tiphelp5("N (%)", "Oversampling percentage used by the percent strategy."), value = 100, min = 0, step = 10, width = "110px"),
          numericInput(session$ns("ratio"), tiphelp5("Ratio", "Target class size as a fraction of the largest class."), value = 1, min = 0.01, step = 0.05, width = "110px"),
          numericInput(session$ns("seed"), tiphelp5("Seed", "Optional random seed for reproducible synthetic observations."), value = NA, step = 1, width = "110px"),
          numericInput(session$ns("k"), tiphelp5("k", "Number of same-class neighbors used to generate synthetic observations."), value = 5, min = 1, step = 1, width = "110px"),
          numericInput(session$ns("k_full"), tiphelp5("k full", "Number of full-dataset neighbors used to detect borderline observations."), value = 10, min = 1, step = 1, width = "110px"),
          numericInput(session$ns("p"), tiphelp5("p", "Minkowski distance order. Use 2 for Euclidean distance."), value = 2, min = 1, step = 1, width = "110px")
        )
      )
    })

    output$metadata_options_ui <- renderUI({
      dat <- data_x()
      coords <- attr(dat, "coords")
      time <- attr(dat, "time")
      has_coords <- !is.null(coords) && nrow(coords) > 0
      has_time <- !is.null(time) && nrow(time) > 0

      if (!has_coords && !has_time) {
        return(NULL)
      }

      div(
        style = "margin-top: 8px",
        if (has_coords) {
          checkboxInput(
            session$ns("synth_coords"),
            smote_help_label("metadata_coords_help", "Interpolate Coords-Attribute"),
            value = FALSE
          )
        },
        if (has_time) {
          checkboxInput(
            session$ns("synth_time"),
            smote_help_label("metadata_time_help", "Interpolate Temporal-Attribute"),
            value = FALSE
          )
        },
        emgray("Coordinate and temporal interpolation is optional because it creates metadata positions for synthetic observations.")
      )
    })

    smote_result <- reactiveVal()
    smote_error <- reactiveVal()

    run_smote <- function() {
      vars <- input$vars
      if (is.null(vars) || !length(vars)) {
        vars <- numeric_vars()
      }

      seed <- suppressWarnings(as.integer(input$seed))
      if (is.na(seed)) {
        seed <- NULL
      }

      task <- input$problem_type %||% "classification"
      new_name <- paste0(input$data_x, if (identical(task, "regression")) "_smoter" else "_smote")

      tryCatch(
        {
          if (identical(task, "regression")) {
            req(input$response_var)
            dat <- working_datalist("regression", vars)
            vars <- setdiff(vars, input$response_var)
            out <- imesc_smoter(
              data = dat,
              response_column = input$response_var,
              vars = vars,
              rare = input$rare,
              rare_q = input$rare_q,
              strategy = input$strategy,
              N = input$N,
              ratio = input$ratio,
              k = input$k,
              p = input$p,
              scale = input$scale,
              seed = seed,
              synth_coords = isTRUE(input$synth_coords),
              synth_time = isTRUE(input$synth_time),
              newname = new_name
            )
          } else {
            req(input$class_factor)
            dat <- working_datalist("classification", vars)
            out <- imesc_smote(
              data = dat,
              class_column = input$class_factor,
              vars = vars,
              strategy = input$strategy,
              N = input$N,
              ratio = input$ratio,
              k = input$k,
              k_full = input$k_full,
              mode = input$mode,
              p = input$p,
              scale = input$scale,
              seed = seed,
              synth_coords = isTRUE(input$synth_coords),
              synth_time = isTRUE(input$synth_time),
              newname = new_name
            )
          }
          attr(out, "datalist_root") <- input$data_x
          attr(out, "bag") <- new_name
          attr(out, "smote")$datalist_x <- input$data_x
          attr(out, "smote")$datalist_y <- input$data_y
          smote_error(NULL)
          smote_result(out)
          out
        },
        error = function(e) {
          smote_result(NULL)
          smote_error(conditionMessage(e))
          NULL
        }
      )
    }

    observeEvent(input$preview_smote, {
      run_smote()
    })

    observeEvent(input$run_smote, {
      dat <- run_smote()
      req(dat)
      vals$newdatalist <- dat
      vals$tosave <- dat
      vals$bagname <- attr(dat, "bag")
      module_save_changes$ui(session$ns("smote_save"), vals)
    })

    module_save_changes$server("smote_save", vals)

    output$smote_summary <- renderUI({
      if (!is.null(smote_error())) {
        return(div(class = "alert_warning", strong("SMOTE error: "), smote_error()))
      }

      dat <- smote_result()

      if (is.null(dat)) {
        return(emgray("Run Preview to inspect the class balance before saving."))
      }

      info <- attr(dat, "smote")

      if (identical(info$task, "regression")) {
        return(
          div(
            div(strong("Synthetic observations added: "), emgreen(info$added)),
            div(strong("Numeric predictors used: "), emgreen(length(info$variables))),
            div(strong("Response variable: "), emgreen(info$response_column)),
            div(strong("Rare range: "), emgreen(info$rare)),
            div(strong("Datalist X / Y: "), emgreen(paste(info$datalist_x, "/", info$datalist_y)))
          )
        )
      }

      div(
        div(strong("Synthetic observations added: "), emgreen(info$added)),
        div(strong("Numeric variables used: "), emgreen(length(info$variables))),
        div(strong("Class factor: "), emgreen(info$class_column)),
        div(strong("Datalist X / Y: "), emgreen(paste(info$datalist_x, "/", info$datalist_y)))
      )
    })

    output$class_table <- renderTable({
      dat <- smote_result()
      req(dat)
      info <- attr(dat, "smote")
      groups <- union(names(info$before), names(info$after))
      before <- info$before[groups]
      after <- info$after[groups]
      before[is.na(before)] <- 0
      after[is.na(after)] <- 0
      data.frame(
        Group = groups,
        Before = as.integer(before),
        After = as.integer(after),
        Added = as.integer(after) - as.integer(before),
        check.names = FALSE
      )
    })

    output$smote_attr_table <- renderTable({
      dat <- smote_result()
      req(dat)
      info <- attr(dat, "smote")
      has_coords <- !is.null(attr(dat, "coords"))
      has_time <- !is.null(attr(dat, "time"))

      data.frame(
        Attribute = c("Numeric-Attribute", "Factor-Attribute", "Coords-Attribute", "Temporal-Attribute"),
        Handling = c(
          if (identical(info$task, "regression")) paste("SMOTER generated", info$added, "synthetic rows from the selected numeric predictors and response.") else paste("SMOTE generated", info$added, "synthetic rows from the selected numeric variables."),
          if (identical(info$task, "regression")) "Existing factor columns are NA for synthetic rows; SMOTE_source and SMOTER_region mark original/synthetic rows and rare regions." else "The selected class factor was assigned to synthetic rows; other factor columns are NA and SMOTE_source marks original/synthetic rows.",
          if (has_coords && isTRUE(info$synth_coords)) "Synthetic coordinates were interpolated from the SMOTE seed and neighbor using Datalist X coordinates." else if (has_coords) "Synthetic coordinate rows were created with NA values. Coords-Attribute comes from Datalist X." else "No Coords-Attribute available in Datalist X.",
          if (has_time && isTRUE(info$synth_time)) "Synthetic temporal values were interpolated from the SMOTE seed and neighbor using Datalist X time." else if (has_time) "Synthetic temporal rows were created with NA values. Temporal-Attribute comes from Datalist X." else "No Temporal-Attribute available in Datalist X."
        ),
        check.names = FALSE
      )
    })
  })
}

# Run custom scripts
tool2_tab13<-list()
tool2_tab13$ui<-function(id){
  ns<-NS(id)
  shortcut_ns <- gsub("[^A-Za-z0-9]", "", ns("run_code"))
  div(
    tags$script(HTML(paste0("
      $(document).off('keydown.", shortcut_ns, "Run').on('keydown.", shortcut_ns, "Run', function(event) {
        if (event.ctrlKey && (event.key === 'r' || event.key === 'R')) {
          event.preventDefault();
          event.stopImmediatePropagation();
          var btn = document.getElementById('", ns("run_code"), "');
          if (btn) { btn.click(); }
        }
      });
    "))),
    tags$script(HTML(paste0("
      $(document).off('keydown.", shortcut_ns, "Clear').on('keydown.", shortcut_ns, "Clear', function(event) {
        if (event.ctrlKey && (event.key === 'l' || event.key === 'L')) {
          event.preventDefault();
          event.stopImmediatePropagation();
          var btn = document.getElementById('", ns("clear_script"), "');
          if (btn) { btn.click(); }
        }
      });
    "))),
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
tool2_tab13$server<-function(id,vals){
  moduleServer(id,function(input,output,session){

    r_code<-reactiveVal()
    code_env <- environment()

    if (exists("saved_data", envir = code_env, inherits = FALSE)) {
      rm(saved_data, envir = code_env)
    }

    makeActiveBinding(
      "saved_data",
      function(value) {
        if (missing(value)) {
          vals$saved_data
        } else {
          vals$saved_data <- value
          invisible(value)
        }
      },
      code_env
    )

    run_free_code <- function(code_text){

      if (is.null(code_text) || !nzchar(trimws(code_text))) {
        return(invisible(NULL))
      }

      expr <- try(parse(text = code_text), silent = TRUE)

      if (inherits(expr, "try-error")) {
        return(expr)
      }

      try(eval(expr, envir = code_env), silent = TRUE)
    }

    observeEvent(input$run_code,{
      t<-run_free_code(input$code)

      if(inherits(t,"try-error")){
        t<-gsub(".*input\\$code\\)\\) ","",as.character(t))
        t<-gsub(".*parse\\(text = code_text\\) : ","",t)
        t<-gsub(".*eval\\(expr, envir = code_env\\) : ","",t)
      }

      r_code(t)
    })
    observeEvent(input$clear_script,{
      r_code(NULL)
      updateTextAreaInput(session,'code',value="")
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

# Datalist manager
tool2_tab14 <- list()
tool2_tab14$ui <- function(id) {
  ns <- NS(id)

  div(
    div(strong("Datalist manager")),

    uiOutput(ns("teste")),

    uiOutput(ns("datalist_total_size")),

    div(
      class = "mp0",
      id = ns("tree_page"),
      style = "display: flex; overflow-y: auto; max-height: calc(100vh - 200px)",

      div(
        class = "mp0",
        shinyTree::shinyTree(
          ns("tree_gen"),
          checkbox = TRUE,
          animation = FALSE,
          contextmenu = FALSE,
          multiple = TRUE
        )
      ),

      div(
        class = "mp0",
        uiOutput(ns("print_tree_gen")),

        div(
          align = "right",
          hidden(
            actionButton(
              ns("remove"),
              icon("trash")
            )
          )
        )
      )
    )
  )
}
tool2_tab14$server <- function(id, vals) {

  moduleServer(id, function(input, output, session) {

    imesc_base_attrs <- c(
      "numeric",
      "factor",
      "coords",
      "time"
    )

    output$tree_gen <- shinyTree::renderTree({

      attrlist <- getTree_saved_data(
        vals,
        FALSE,
        imesc_attrs = imesc_base_attrs,
        imesc_models = c(
          "pwRDA",
          "som",
          "kmeans",
          as.character(available_models)
        )
      )

      attrlist
    })

    full_datalist <- reactive({

      result <- FALSE

      res <- sapply(input$tree_gen, function(x) {

        re <- sapply(x, function(xx) {
          length(attr(x, "stselected"))
        })

        sum(re) == length(x)
      })

      if (any(res)) {
        if (length(res) > 0) {
          result <- which(res)
        }
      }

      return(result)
    })

    any_numfac <- reactive({

      df <- df_selected()[1:2]

      numfac <- df$attr %in% c(
        "numeric",
        "factor",
        "factors"
      )

      res <- if (any(numfac)) {

        list(
          div(
            class = "alert_warning",

            div(
              strong(
                icon(
                  "triangle-exclamation",
                  style = "color: gold"
                ),
                "Warning:",
                "You have chosen to exclude",
                embrown(
                  paste(
                    df$attr[numfac],
                    collapse = "/"
                  )
                ),
                "attributes."
              )
            ),

            div(
              em("A DataList requires both numeric and factor attributes")
            ),

            div(
              em("Click 'Confirm' to delete the entire Datalist")
            )
          )
        )

      } else {

        NULL
      }

      res
    })

    observeEvent(input$remove, ignoreInit = TRUE, {

      if (!isFALSE(full_datalist())) {

        action <- div(
          any_numfac(),
          em(
            lapply(
              unique(df_selected()$datalist),
              div
            )
          )
        )

        left <- "Are you sure you want to delete the selected DataLists?"

      } else {

        df <- df_selected()[1:2]

        action <- div(
          any_numfac(),
          renderPrint(
            split(df, df$datalist)
          )
        )

        left <- div("Are you sure you want to delete the selected Attributes?")

        numfac <- df$attr %in% c(
          "numeric",
          "factor",
          "factors"
        )
      }

      confirm_modal(
        session$ns,
        action = "",
        left = left,
        div_right = FALSE,
        div1_post = action
      )
    })

    which_selected <- reactive({

      req(input$tree_gen)

      selall <- sapply(input$tree_gen, function(x) {
        unlist(
          sapply(x, function(xx) {
            attr(xx, "stselected")
          })
        )
      })

      if (!length(selall) > 0) {
        return(NULL)
      }

      selall <- do.call(rbind, selall)

      if (!length(selall) > 0) {
        return(NULL)
      }

      selall
    })

    observe({

      if (length(which_selected()) > 0) {
        shinyjs::show("remove")
      } else {
        shinyjs::hide("remove")
      }
    })

    all_selected <- reactive({

      selall <- which_selected()

      req(length(selall) > 0)

      div(
        style = "font-size: 11px",

        lapply(seq_len(nrow(selall)), function(i) {

          if (all(selall[i, ])) {

            rowname <- rownames(selall)[i]

            a <- "Datalist:"

            if (rowname == "Ensemble") {

              a <- "Ensembles"
              ob <- format(
                object.size(vals$saved_ensemble),
                "auto"
              )

            } else {

              ob <- format(
                object.size(vals$saved_data[[rowname]]),
                "auto"
              )
            }

            div(
              a,
              paste0(rowname, ":"),
              strong(emgreen(ob))
            )
          }
        })
      )
    })

    df_selected <- reactive({

      sel_table <- selected_Tree_attrs(input$tree_gen)

      req(!is.null(sel_table))

      res <- lapply(seq_len(nrow(sel_table)), function(i) {

        args <- as.list(sel_table[i, ])
        args$vals <- vals

        do.call(get_attr_imesc, args)
      })

      df <- data.frame(
        do.call(rbind, res)
      )

      df
    })

    observeEvent(input$confirm, ignoreInit = TRUE, {

      run_remove()

      removeModal()
    })

    run_remove <- reactive({

      df <- df_selected()

      full <- subset(
        df,
        attr %in% c(
          "numeric",
          "factor",
          "factors"
        )
      )

      fulldel <- unique(full$datalist)

      if (length(fulldel) > 0) {

        pic <- which(
          df$attr %in% c(
            "numeric",
            "factor",
            "factors"
          )
        )

        todel <- df$datalist[pic]

        for (i in seq_along(fulldel)) {
          vals$saved_data[[fulldel[i]]] <- NULL
        }

        df <- df[-which(df$datalist %in% todel), ]
      }

      if (nrow(df) > 0) {

        for (i in seq_len(nrow(df))) {

          if (df$datalist[i] == "Ensemble") {
            vals$saved_ensemble[[df$attr[i]]] <- NULL
          }

          if (length(vals$saved_ensemble) == 0) {
            vals$saved_ensemble <- NULL
          }
        }
      }

      pic <- which(df$datalist == "Ensemble")

      if (length(pic) > 0) {
        df <- df[-pic, ]
      }

      if (nrow(df) > 0) {

        dfmodels <- subset(
          df,
          attr %in% imesc_models
        )

        if (nrow(dfmodels) > 0) {

          for (i in seq_len(nrow(dfmodels))) {

            datalist <- dfmodels$datalist[i]
            attr_name <- dfmodels$attr[i]

            attr(vals$saved_data[[datalist]], attr_name)[[dfmodels$model_name[i]]] <- NULL
          }

          if (!length(attr(vals$saved_data[[datalist]], attr_name)) > 0) {
            attr(vals$saved_data[[datalist]], attr_name) <- NULL
          }

          df <- subset(
            df,
            !attr %in% imesc_models
          )
        }
      }

      if (nrow(df) > 0) {

        for (i in seq_len(nrow(df))) {

          datalist <- df$datalist[i]
          attr_name <- df$attr[i]

          if (attr_name == "temporal") {
            attr_name <- "time"
          }

          attr(vals$saved_data[[datalist]], attr_name) <- NULL
        }
      }
    })

    output$print_tree_gen <- renderUI({

      req(df_selected())
      req(length(df_selected()) > 0)

      div(
        div(
          class = "half-drop-inline small_table",
          style = "overflow:scroll; max-height: 300px",

          all_selected(),

          renderTable({
            df_selected()[c(1, 2, 4, 3, 5)]
          })
        )
      )
    })
  })
}

# Delete Datalists
tool2_tab15<-list()
tool2_tab15$ui<-function(id){
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
tool2_tab15$server<-function(id,vals){
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




tool3$ui <- function(id){

  tips <- list(
    "Remove rows with missing values",
    "Keep only rows matching IDs from another dataset",
    "Filter rows by selected factors.",
    "Select or remove rows manually.",
    "Remove rows with zero variance",
    "Filter rows by temporal attributes."
  )

  tips <- get_tips(tips)
  ns <- NS(id)

  div(
    id = "tool3",
    class = "tools_content",

    div(
      class = "nav-tools",

      tags$head(
        tags$script(
          HTML('$(function () { $("[data-toggle=\'tooltip\']").tooltip(); });')
        )
      ),

      navlistPanel(
        id = ns("tabs_tool3"),
        title = NULL,
        widths = c(5, 7),

        tabPanel(
          span("Individual row selection", tips[4]),
          value = "tab4",

          uiOutput(ns("update_tab4")),

          div(
            div(
              class = "picker_open",
              virtualPicker(ns("selecobs"))
            )
          ),

          uiOutput(ns("print_selecobs"))
        ),

        tabPanel(
          title = span("Remove NAs", tips[1]),
          value = "tab1",

          hidden(
            checkboxInput(
              ns("na.omit"),
              span("NA.omit"),
              value = FALSE
            )
          ),

          uiOutput(ns("print_na"))
        ),

        tabPanel(
          title = span("Remove Zero Variance", tips[5]),
          value = "tab5",

          checkboxInput(
            ns("zero_var"),
            span("Remove Zero Var"),
            value = FALSE
          ),

          uiOutput(ns("print_zero"))
        ),

        tabPanel(
          span("Match IDs with Datalist", tips[2]),
          value = "tab2",

          div(
            class = "half-drop",

            p(em()),

            uiOutput(ns("sync_ids")),
            uiOutput(ns("print_sync")),

            hidden(
              actionLink(
                ns("unsync_ids"),
                "Display Unmatched IDs"
              )
            ),

            uiOutput(ns("update_tab2"))
          )
        ),

        tabPanel(
          span("Filter by factors", tips[3]),
          value = "tab3",

          div(
            style = "height: 350px; background-color: #e3e3e3;",
            class = "tool3_tab3",

            tabsetPanel(
              NULL,

              tabPanel(
                "Tree",

                div(
                  div(
                    div(
                      style = "height: 250px; overflow-y:scroll;
                      -webkit-box-shadow: inset 0px 0px 5px #797979ff;",

                      div(
                        "Click on the Nodes ",

                        tipify_ui(
                          icon(
                            verify_fa = FALSE,
                            name = NULL,
                            class = "fas fa-question-circle"
                          ),
                          "Click on the nodes to expand and select the factor levels. Only available for factors with less than 100 levels"
                        ),

                        uiOutput(ns("validate_tab3")),

                        shinyTree::shinyTree(
                          ns("tree"),
                          checkbox = TRUE,
                          themeIcons = FALSE,
                          themeDots = TRUE
                        )
                      )
                    )
                  ),

                  uiOutput(ns("print_tree"))
                )
              ),

              tabPanel(
                "Subset",

                div(
                  class = "half-drop-inline",

                  tags$head(
                    tags$script(
                      "Shiny.addCustomMessageHandler(\"testmessage\",
                        function(message) {
                          var x = document.getElementsByClassName(\"filter-option pull-left\");
                          x[0].innerHTML = message;
                        }
                      );"
                    )
                  ),

                  div(
                    class = "tool_subset",

                    pickerInput(
                      ns("subset_factor"),
                      "Factor",
                      choices = NULL
                    ),

                    pickerInput(
                      ns("subset_level"),
                      "Levels",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        liveSearch = TRUE,
                        `actions-box` = TRUE,
                        `selected-text-format` = "count > 0",
                        `count-selected-text` = "{0}/{1} selected"
                      )
                    )
                  ),

                  uiOutput(ns("update_tab3_subset")),
                  uiOutput(ns("print_subset"))
                )
              )
            )
          )
        )

        # Importante:
        # A aba "Filter by time" NÃO fica aqui.
        # Ela será inserida/removida dinamicamente no server
        # com insertTab() e removeTab().
      ),

      div(
        style = "position: absolute; left: 10px; top: 200px; width: 200px",
        uiOutput(ns("zero_var_print"))
      )
    )
  )
}
# The tool3$ui function provides an interface for advanced row filtering and manipulation in Datalists.
# It includes options for removing rows with missing values or zero variance, matching IDs between Datalists,
# filtering rows based on factors (via tree-based or subset-based selection), and manual row selection.
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
# The tool3$server function implements the server-side logic for row filtering and manipulation.
# It processes user interactions, calculates rows to be removed based on various criteria, and updates the Datalist accordingly.
# The server ensures that the filtered data is consistently maintained and ready for downstream tools in the application.
tool3$server <- function(id, vals = NULL){

  moduleServer(id, function(input, output, session){

    vtool3 <- reactiveValues()
    row_match <- reactiveVal()
    factor_tree <- reactiveVal()
    rm_subset <- reactiveVal()

    time_tab_inserted <- reactiveVal(FALSE)

    data <- reactive({
      req(vals$pp_data)
      vals$pp_data
    })

    factors <- reactive({
      attr(data(), "factors")
    })

    time_attr <- reactive({
      attr(data(), "time")
    })

    choices_rows <- reactive({
      rownames(data())
    })

    # -------------------------------------------------------------------------
    # Dynamic Temporal Filter tab
    # -------------------------------------------------------------------------

    observe({

      time <- time_attr()

      has_time <- !is.null(time) &&
        is.data.frame(time) &&
        ncol(time) > 0

      if (isTRUE(has_time) && !isTRUE(time_tab_inserted())) {

        time_tip <- get_tips(
          list("Filter rows by temporal attributes.")
        )[[1]]

        insertTab(
          inputId = "tabs_tool3",
          target = "tab3",
          position = "after",
          select = FALSE,
          session = session,

          tab = tabPanel(
            span("Filter by time", time_tip),
            value = "tab6",

            uiOutput(session$ns("time_filter_ui")),
            uiOutput(session$ns("print_time_filter"))
          )
        )

        time_tab_inserted(TRUE)
      }

      if (!isTRUE(has_time) && isTRUE(time_tab_inserted())) {

        removeTab(
          inputId = "tabs_tool3",
          target = "tab6",
          session = session
        )

        vtool3$rm_time <- NULL
        time_tab_inserted(FALSE)
      }
    })

    output$time_filter_ui <- renderUI({

      time <- time_attr()

      req(!is.null(time))
      req(is.data.frame(time))
      req(ncol(time) > 0)

      div(
        class = "half-drop-inline",

        pickerInput(
          session$ns("time_column"),
          "Temporal column:",
          choices = colnames(time),
          selected = colnames(time)[1],
          width = "250px"
        ),

        uiOutput(session$ns("time_filter_controls"))
      )
    })

    selected_time_col <- reactive({

      time <- time_attr()

      req(!is.null(time))
      req(is.data.frame(time))
      req(ncol(time) > 0)
      req(input$time_column)
      req(input$time_column %in% colnames(time))

      time[[input$time_column]]
    })

    output$time_filter_controls <- renderUI({

      x <- selected_time_col()

      if (inherits(x, "Date")) {

        rng <- range(x, na.rm = TRUE)

        return(
          dateRangeInput(
            session$ns("time_date_range"),
            "Date range:",
            start = rng[1],
            end = rng[2],
            min = rng[1],
            max = rng[2]
          )
        )
      }

      if (inherits(x, c("POSIXct", "POSIXlt"))) {

        rng <- range(as.Date(x), na.rm = TRUE)

        return(
          dateRangeInput(
            session$ns("time_datetime_range"),
            "Date range:",
            start = rng[1],
            end = rng[2],
            min = rng[1],
            max = rng[2]
          )
        )
      }

      if (is.numeric(x) || is.integer(x)) {

        rng <- range(x, na.rm = TRUE)

        return(
          sliderInput(
            session$ns("time_numeric_range"),
            "Range:",
            min = rng[1],
            max = rng[2],
            value = rng
          )
        )
      }

      pickerInput(
        session$ns("time_levels"),
        "Values:",
        choices = sort(unique(as.character(x))),
        selected = sort(unique(as.character(x))),
        multiple = TRUE,
        options = list(
          liveSearch = TRUE,
          `actions-box` = TRUE,
          `selected-text-format` = "count > 0",
          `count-selected-text` = "{0}/{1} selected"
        )
      )
    })

    observeEvent(
      list(
        input$time_column,
        input$time_date_range,
        input$time_datetime_range,
        input$time_numeric_range,
        input$time_levels
      ),
      {

        time <- time_attr()

        if (is.null(time) || !is.data.frame(time) || ncol(time) == 0) {
          vtool3$rm_time <- NULL
          return()
        }

        req(input$time_column)
        req(input$time_column %in% colnames(time))

        x <- time[[input$time_column]]
        ids <- rownames(time)

        keep <- rep(TRUE, length(x))

        if (inherits(x, "Date")) {

          req(input$time_date_range)

          keep <- x >= as.Date(input$time_date_range[1]) &
            x <= as.Date(input$time_date_range[2])

        } else if (inherits(x, c("POSIXct", "POSIXlt"))) {

          req(input$time_datetime_range)

          x_date <- as.Date(x)

          keep <- x_date >= as.Date(input$time_datetime_range[1]) &
            x_date <= as.Date(input$time_datetime_range[2])

        } else if (is.numeric(x) || is.integer(x)) {

          req(input$time_numeric_range)

          keep <- x >= input$time_numeric_range[1] &
            x <= input$time_numeric_range[2]

        } else {

          req(input$time_levels)

          keep <- as.character(x) %in% input$time_levels
        }

        keep[is.na(keep)] <- FALSE

        vtool3$rm_time <- ids[!keep]
      },
      ignoreInit = FALSE
    )

    output$print_time_filter <- renderUI({

      time <- time_attr()

      if (is.null(time) || !is.data.frame(time) || ncol(time) == 0) {
        return(NULL)
      }

      div(
        renderPrint(
          c("Removed IDs" = length(vtool3$rm_time))
        )
      )
    })

    # -------------------------------------------------------------------------
    # Remove Zero Variance
    # -------------------------------------------------------------------------

    observeEvent(input$zero_var, {

      if (isTRUE(input$zero_var)) {

        ids_zv <- rownames(data())[
          apply(data(), 1, function(x) var(x, na.rm = TRUE)) == 0
        ]

      } else {

        ids_zv <- NULL
      }

      vtool3$rm_zv <- ids_zv
    })

    observe({

      shinyjs::toggle(
        "zero_var",
        condition = any(
          apply(data(), 1, function(x) var(x, na.rm = TRUE)) == 0
        )
      )
    })

    output$print_zero <- renderUI({

      div(
        if (!any(apply(data(), 1, function(x) var(x, na.rm = TRUE)) == 0)) {
          em(
            "No rows with zero variance were detected in the data.",
            style = "color: gray"
          )
        },

        if (isTRUE(input$zero_var)) {
          renderPrint({
            data.frame("Removed IDs" = vtool3$rm_zv)
          })
        }
      )
    })

    output$zero_var_print <- renderUI({

      req(
        any(
          apply(data(), 1, function(x) var(x, na.rm = TRUE)) == 0
        )
      )

      div(
        class = "alert_warning",
        icon("triangle-exclamation", style = "color: Dark yellow3"),
        "Warning: Some observations (rows) exhibit zero variance. You can remove them using the 'Remove Zero Variance Tool'."
      )
    })

    # -------------------------------------------------------------------------
    # Sync IDs
    # -------------------------------------------------------------------------

    output$sync_ids <- renderUI({

      selectInput(
        session$ns("sync_ids"),
        "Choose a Datalist for ID matching",
        names(vals$saved_data)
      )
    })

    observeEvent(input$sync_ids, {

      req(input$sync_ids)

      match <- rownames(data()) %in%
        rownames(vals$saved_data[[input$sync_ids]])

      row_match(match)
    })

    observeEvent(row_match(), {

      if (any(!row_match())) {

        shinyjs::show("unsync_ids")

      } else {

        shinyjs::hide("unsync_ids")
      }
    })

    observeEvent(input$unsync_ids, {

      showModal(
        modalDialog(
          easyClose = TRUE,
          print_unsync()
        )
      )
    })

    observeEvent(row_match(), {

      vtool3$rm_match <- rownames(data())[!row_match()]
    })

    print_unsync <- reactive({

      req(any(!row_match()))

      primary_datalist <- attr(data(), "datalist_root")
      secondary_datalist <- input$sync_ids

      div(
        span(
          em(primary_datalist, style = "color: SeaGreen"),
          "does not contain the following IDs from",
          em(secondary_datalist, style = "color: SeaGreen"),
          ":"
        ),

        div(
          style = "height: 200px; overflow-y:scroll",

          renderTable(
            data.frame(unmached_is = vtool3$rm_match)
          )
        )
      )
    })

    match_summary <- reactive({

      req(input$sync_ids)
      req(length(row_match()) > 0)

      primary_datalist <- attr(data(), "datalist_root")
      secondary_datalist <- input$sync_ids

      nd1 <- nrow(data())
      nd2 <- nrow(vals$saved_data[[input$sync_ids]])

      matches <- sum(row_match())
      nonmaches <- sum(!row_match())

      data.frame(
        Datalist = c(primary_datalist, secondary_datalist, "", ""),
        row_count = c(nd1, nd2, matches, nonmaches),
        row.names = c(
          "Primary",
          "Secondary",
          "Matched IDs",
          "Removed IDs"
        )
      )
    })

    output$print_sync <- renderUI({

      req(input$sync_ids)

      renderPrint(match_summary())
    })

    # -------------------------------------------------------------------------
    # Row choices
    # -------------------------------------------------------------------------

    observeEvent(choices_rows(), {

      shinyWidgets::updateVirtualSelect(
        "selecobs",
        choices = choices_rows(),
        selected = choices_rows()
      )
    })

    # -------------------------------------------------------------------------
    # Remove NAs
    # -------------------------------------------------------------------------

    observeEvent(input$na.omit, {

      if (isTRUE(input$na.omit)) {

        ids_rm <- names(
          which(
            apply(is.na(data()), 1, any)
          )
        )

      } else {

        ids_rm <- NULL
      }

      vtool3$rm_na <- ids_rm
    })

    output$print_na <- renderUI({

      div(
        if (!anyNA(data())) {
          em(
            "No missing values (NA) detected in the data.",
            style = "color: gray"
          )
        },

        renderPrint(
          c("Removed IDs" = length(vtool3$rm_na))
        )
      )
    })

    observe({

      if (anyNA(data())) {

        shinyjs::show("na.omit")

      } else {

        shinyjs::hide("na.omit")
      }
    })

    # -------------------------------------------------------------------------
    # Filter by factors
    # -------------------------------------------------------------------------

    observeEvent(factors(), {

      updatePickerInput(
        session,
        "subset_factor",
        choices = colnames(factors())
      )
    })

    valid_levels <- reactive({

      factor_levels <- get_faclevels(factors())

      which(
        unlist(
          lapply(
            factor_levels,
            function(x) length(x[[1]])
          )
        ) < 100
      )
    })

    output$validate_tab3 <- renderUI({

      req(length(valid_levels()) < ncol(factors()))

      invalid <- colnames(factors())[-valid_levels()]

      div(
        popify(
          actionLink(
            session$ns("dummy"),
            icon("triangle-exclamation"),
            style = "color: gold"
          ),
          NULL,
          HTML(
            paste0(
              p(
                "Selection of the following factors is disabled within the tree because they contain more than 100 levels"
              ),
              do.call(
                paste0,
                lapply(
                  invalid,
                  function(x) p(HTML(paste0(em(x))))
                )
              )
            )
          ),
          trigger = "click"
        )
      )
    })

    output$tree <- shinyTree::renderTree({

      req(length(valid_levels()) > 0)

      factors_valid <- factors()[valid_levels()]

      req(factors_valid)

      lev <- list()

      for (factor_name in colnames(factors_valid)) {

        lev_sub <- list()

        levels_factor <- unique(
          as.character(factors_valid[, factor_name])
        )

        for (level in levels_factor) {
          lev_sub[[level]] <- ""
        }

        lev[[factor_name]] <- lev_sub
        attr(lev[[factor_name]], "stselected") <- TRUE
      }

      lev
    })

    output$print_tree <- renderUI({

      renderPrint(
        c("Removed IDs" = length(vtool3$rm_tree))
      )
    })

    observeEvent(input$tree, {

      seltree <- shinyTree::get_selected(input$tree)

      if (!length(seltree) > 0) {

        vtool3$rm_tree <- rownames(factors())
        return()
      }

      selected <- get_selfactors(
        res = seltree,
        factors()
      )

      factor_ids <- rownames(factors())

      rm_ids <- factor_ids[
        !factor_ids %in% selected
      ]

      vtool3$rm_tree <- rm_ids
    })

    observeEvent(input$subset_factor, {

      req(input$subset_factor %in% colnames(factors()))

      choices <- levels(
        factors()[, input$subset_factor]
      )

      updatePickerInput(
        session,
        "subset_level",
        choices = choices,
        selected = choices
      )
    })

    observeEvent(input$subset_level, {

      req(input$subset_factor %in% colnames(factors()))

      choices <- levels(
        factors()[, input$subset_factor]
      )

      req(input$subset_level %in% choices)

      fac <- factors()[, input$subset_factor]

      rm_ids <- rownames(factors())[
        !fac %in% input$subset_level
      ]

      vtool3$rm_subset <- rm_ids
    })

    output$print_subset <- renderUI({

      renderPrint(
        removed_ids(vtool3$rm_subset)
      )
    })

    # -------------------------------------------------------------------------
    # Individual row selection
    # -------------------------------------------------------------------------

    observeEvent(input$selecobs, {

      ids <- rownames(data())

      res <- if (is.null(input$selecobs)) {

        ids

      } else {

        ids_rm <- ids[!ids %in% input$selecobs]
        ids_rm
      }

      vtool3$rm_row <- res
    })

    output$print_selecobs <- renderUI({

      div(
        renderPrint(
          c("Removed IDs" = length(vtool3$rm_row))
        )
      )
    })

    # -------------------------------------------------------------------------
    # Final result
    # -------------------------------------------------------------------------

    result_v3 <- reactive({

      list(
        rm_na = vtool3$rm_na,
        rm_match = vtool3$rm_match,
        rm_tree = vtool3$rm_tree,
        rm_subset = vtool3$rm_subset,
        rm_row = vtool3$rm_row,
        rm_zv = vtool3$rm_zv,
        rm_time = vtool3$rm_time
      )
    })

    v3_data <- reactive({

      data <- vals$pp_data

      if (!any(sapply(result_v3(), length) > 0)) {
        return(data)
      }

      ids <- unique(
        unlist(result_v3())
      )

      pic <- which(
        rownames(data) %in% ids
      )

      if (length(pic) > 0) {

        newdata <- data[-pic, , drop = FALSE]
        data <- data_migrate(data, newdata)
      }

      data
    })

    observeEvent(v3_data(), {

      vals$vtools$tool3 <- v3_data()
    })

    return(NULL)
  })
}

# The tool4$ui function provides an interface for column-based filtering and removal operations in a dataset.
# It allows users to perform tasks such as individual column selection, removal based on values,
# correlation-based filtering, and identification/removal of columns with zero or near-zero variance.
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
                       span(tipify_ui( icon(verify_fa=FALSE,name=NULL,class="fas fa-question-circle"),"Remove columns where the values are less than x-percent of their cumulative total","bottom"),
                            inline(checkboxInput(ns("rareabund"),'Abund<',F, width="80px")),
                            inline(hidden(div(id=ns("pc1"),inline(numericInput(ns('pct_abund'), NULL, value =0.1, width="100px")),'%'))))
                     ),
                     uiOutput(ns('print_rareabund')),
                     div(
                       span(tipify_ui( icon(verify_fa=FALSE,name=NULL,class="fas fa-question-circle"),"Remove variables occurring in less than  x-percent of the number of samples"),
                            inline(checkboxInput(ns("rarefreq"),"Freq<",F, width="80px")),
                            inline(hidden(div(id=ns("pc2"),inline(numericInput(ns('pct_freq'), NULL, value =0.1, width="100px")),"%")))
                       ),
                       uiOutput(ns('print_rarefreq'))
                     ),
                     div(
                       tipify_ui( icon(verify_fa=FALSE,name=NULL,class="fas fa-question-circle"),"Requires a counting data. Remove variables occurring only once"),
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
        ),
        tabPanel(span("Match Columns with Datalist"),value="tab7",
                 div(class="half-drop",
                     uiOutput(ns('sync_ids')),
                     uiOutput(ns("print_sync")),
                     hidden(actionLink(ns("unsync_ids"),"Display Unmatched Columns")),
                     uiOutput(ns("update_tab2")),
                 )),

      ),
      div(style="position: absolute; left: 10px; top: 200px; width: 200px",
          uiOutput(ns("zero_var_print"))
      )
  )
}
# The tool4$server function implements the backend logic for column-based filtering and removal.
# It processes user inputs, calculates columns to be removed based on specified criteria (e.g., value thresholds, correlation cutoffs),
# and updates the dataset accordingly. The server also ensures that user interactions dynamically reflect in the dataset,
# maintaining consistency for further data analysis tasks.
tool4$server<-function(id,vals=NULL){

  moduleServer(id,function(input,output,session){

    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })
    zero_var_cols<-reactive({
      apply(data(),2, function(x) var(x,na.rm=T))==0
    })

    observeEvent(input$zero_var,{
      if(isTRUE(input$zero_var)){
        ids_zv<-colnames(data())[zero_var_cols()]
      } else{
        ids_zv<-NULL
      }
      rm_zv(ids_zv)

    })

    observe({
      shinyjs::toggle('zero_var', condition = any(zero_var_cols()))

    })
    rm_zv<-reactiveVal()

    output$print_zero<-renderUI({

      div(
        if(!any(zero_var_cols())){
          em("No columns with zero variance were detected in the data.",style="color: gray")
        },
        if(isTRUE(input$zero_var))
          renderPrint({
            data.frame("Removed cols"=rm_zv())

          })
      )
    })

    output$zero_var_print<-renderUI({
      req(any(zero_var_cols()))


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
      nz<-do.call(caret::nearZeroVar,args_nzv())
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
        find_cor_result<-caret::findCorrelation(x,input$cor_cutoff, exact=T,names =T)
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
      down_corr$remove<-F
      df<-data.frame(rbind(up_corr,down_corr))
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
        rm_zv=rm_zv(),
        rm_col_match=rm_col_match()
      )
    })


    bag_name<-reactive({
      paste0(
        if(length(input$rareabund)>0) {
          if(isTRUE(input$rareabund)) {paste0("(-",paste0("Abund<",input$pct_abund,"%"),")")}
        },
        if(length(input$rarefreq)>0) {
          if(isTRUE(input$rarefreq)) {paste0("(-",paste0("Freq<",input$pct_freq,"%"),")")}
        },
        if(length(input$raresing)>0) {
          if(isTRUE(input$raresing)) {paste0("(-",input$raresing,")")}
        }

      )
    })

    ##


    output$sync_ids<-renderUI({
      choices<-names(vals$saved_data)
      pic<-sapply(vals$saved_data,function(x)
        any(   colnames(x)%in%colnames(vals$pp_data))
      )


      choices<-choices[pic]

      selectInput(session$ns("sync_ids"),"Choose a Datalist for Column matching",choices)
    })

    output$print_sync<-renderUI({
      req(input$sync_ids)

      renderPrint(match_summary())
    })

    observeEvent(col_match(),{
      if(any(!col_match())){
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



    print_unsync<-reactive({
      req(any(!col_match()))
      primary_datalist<-attr(data(),"datalist_root")
      secondary_datalist<-input$sync_ids

      div(
        span(
          em(primary_datalist,style="color: SeaGreen"),
          "does not contain the following Columns from",
          em(secondary_datalist,style="color: SeaGreen"), ":"
        ),
        div(style="height: 200px; overflow-y:scroll;",
            renderTable(
              data.frame(unmached_is=rm_col_match())
            )
        )

      )
    })
    match_summary<-reactive({
      req(input$sync_ids)
      req(length(col_match())>0)
      primary_datalist<-attr(data(),"datalist_root")
      secondary_datalist<-input$sync_ids
      nd1<-ncol(data())
      nd2<-ncol(vals$saved_data[[input$sync_ids]])
      matches<-sum(col_match())
      nonmaches<-sum(!col_match())

      data.frame(Datalist=c(primary_datalist,secondary_datalist,"",""),
                 col_count=c(nd1,nd2,matches,nonmaches),
                 col.names=c("Primary",
                             "Secondary",
                             "Matched Columns",
                             "Removed Columns")

      )

    })
    col_match<-reactiveVal()
    rm_col_match<-reactiveVal()

    observeEvent(col_match(),{
      rm_col_match(colnames(data())[!col_match()])
    })
    output$print_sync<-renderUI({
      req(input$sync_ids)

      div(style="overflow-x:scroll; width: 300px",
          renderTable(match_summary(), rownames = T))

    })

    observeEvent(input$sync_ids,{
      req(input$sync_ids)
      match<-colnames(data())%in%colnames(vals$saved_data[[input$sync_ids]])
      col_match(match)
    })
    ##

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

# The tool5$ui function provides a user interface for applying transformations and scaling to numeric attributes of a dataset.
# It includes options for various transformation methods (e.g., from 'vegan' and 'caret' packages),
# scaling and centering, and customizable transformation orders. The UI is designed for flexibility,
# allowing users to select specific columns and view summaries of pre- and post-transformation data.
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
      div(class="half-drop half-drop-inline picker150",
          div(style='display: flex',
              div(style="width: 55%",
                  pickerInput_fromtop_live(ns("transf"),
                                           label=span(strong("Transformation"),actionLink(ns("transf_help"),icon("fas fa-question-circle"))),choices=transf_value)
              ),
              virtualPicker_unique(ns("cols"),choices="All", label=NULL,multiple = T,allOptionsSelectedText="All columns",width='150px')
          ),

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
# The tool5$server function implements the logic for applying transformations and scaling to the dataset.
# It processes user inputs, applies the selected transformations and scaling in the desired order, and updates the dataset.
# The server also ensures that changes are dynamically reflected and provides feedback on the operations performed,
# such as displaying summaries and handling user interactions effectively.
tool5$server<-function(id,vals){

  moduleServer(id,function(input,output,session){
    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })


    observeEvent(data(),{
      shinyWidgets::updateVirtualSelect("cols",choices=colnames(data()),selected=colnames(data()))
    })


    observe({
      shinyjs::toggle("transf_out",condition=isTRUE(input$scale)|input$transf!="None")
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
      req(men)
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


    run_transf<-eventReactive(list(input$transf,input$scale,input$center,input$transf_ord,input$cols),{
      res<-list()
      req(input$cols)%in%colnames(data())
      args_list<-list(
        Transf=list(transf=input$transf,cols=input$cols,fun_name='transf_data'),
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
      try({

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
    })
    output$transf_order<-renderUI({
      div(
        div(
          div(style="display: flex;    padding-top: 0px;",
              div("First",style="width: 100px;"),
              div("Second",style="width: 100px"),
          ),
          class="tr_or",
          sortable::rank_list(
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
# tool6$ui: Provides a user interface for handling missing data imputation in a dataset.
# Users can choose the target attribute type (Numeric or Factor), select an imputation method
# (e.g., knn, bagImpute, medianImpute, pmm, rf, or cart), and adjust relevant parameters like the number of neighbors for knn.
# The UI also includes warnings for computationally intensive methods and tools for displaying imputation results.
tool6$ui<-function(id){
  ns<-NS(id)
  div(
    div(style="height: 430px; ;display: flex;",class="half-drop-inline",

        div(style="width: 50%;padding: 15px; overflow-y: auto; overflow-x: hidden;",class="half-drop",
            selectInput(ns("na_targ"),"Target:", c("Numeric-Attribute","Factor-Attribute")),
            selectInput(ns("na_method"), div("Method:",tipify_ui(actionLink(ns("na_help"),icon("fas fa-question-circle"), type="toggle"),"Click for details","right")),choices=c("knn","bagImpute","medianImpute","pmm","rf","cart")),
            uiOutput(ns('bag_warning')),
            hidden(numericInput(ns("na_knn"), span("K:",tiphelp("the number of nearest neighbors from the training set to use for imputation")),value=5)),
            checkboxInput(ns("na_by_group"),tiphelp5(
              "Impute by group",
              "Imputes according to a Factor-Attribute column, typically the one created by Data partition (levels training and test). This avoids data leakage: without it, values of the test observations help to fill the training data (and vice versa), which makes model evaluation optimistic. 'Learn from one level' fits the imputation only on the reference level (e.g. training) and applies it to all observations, as a fitted model is applied to new data. 'Each level separately' imputes every level using only its own observations."
            ),value=FALSE),
            div(id=ns("na_group_opts"),
                uiOutput(ns("na_group_dl_ui")),
                uiOutput(ns("na_group_var_ui")),
                radioButtons(ns("na_group_mode"),NULL,
                             choices=c("Learn from one level, apply to all"="reference","Each level separately"="separate"),
                             selected="reference"),
                uiOutput(ns("na_ref_level_ui"))
            ),
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

  )
}

# tool6$server: Implements the logic for missing data imputation using the selected method and parameters.
# It dynamically updates the dataset, handles user interactions (e.g., showing warnings, toggling input fields),
# and integrates with tools for outlier handling. The server ensures that the imputation results are
# appropriately reflected in the dataset and provides feedback on the imputation process.
tool6$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    bag_name<-reactive({paste0("(imp_",input$na_method,")")})


    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })

    # Imputation by group (e.g. partition column) to avoid leakage between levels
    observe({
      shinyjs::toggle("na_group_opts",condition=isTRUE(input$na_by_group))
    })
    # Datalists whose Factor-Attribute covers every observation being imputed
    group_datalists<-reactive({
      ids<-rownames(data())
      ok<-vapply(vals$saved_data,function(d){
        fac<-attr(d,"factors")
        !is.null(fac)&&ncol(fac)>0&&all(ids%in%rownames(fac))
      },logical(1))
      names(vals$saved_data)[ok]
    })
    output$na_group_dl_ui<-renderUI({
      choices<-group_datalists()
      req(length(choices))
      current<-attr(data(),"datalist")
      selected<-get_selected_from_choices(isolate(input$na_group_dl)%||%current,choices)
      pickerInput_fromtop(session$ns("na_group_dl"),tiphelp5("Factor from:","Datalist that holds the grouping factor. It must contain all observations of the Datalist being imputed; values are matched by observation ID."),choices=choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=TRUE))
    })
    # grouping factors aligned to the rows being imputed
    group_factors<-reactive({
      dl<-input$na_group_dl
      src<-if(!is.null(dl)&&dl%in%names(vals$saved_data)) vals$saved_data[[dl]] else data()
      fac<-attr(src,"factors")
      if(is.null(fac)||!ncol(fac)||!all(rownames(data())%in%rownames(fac))) return(NULL)
      fac[rownames(data()),,drop=FALSE]
    })
    group_choices<-reactive({
      fac<-group_factors()
      if(is.null(fac)||!ncol(fac)) return(character(0))
      # complete factors with a few levels (groups need several observations each)
      ok<-vapply(fac,function(x){
        n_lev<-length(unique(x))
        !anyNA(x)&&n_lev>1&&n_lev<=length(x)/2
      },logical(1))
      colnames(fac)[ok]
    })
    output$na_group_var_ui<-renderUI({
      choices<-group_choices()
      if(!length(choices)){
        return(div(em("No Factor-Attribute column without missing values. Create one with Data partition."),style="color: brown; font-size: 11px; white-space: normal"))
      }
      part<-choices[grepl("^Partition",choices)]
      selected<-get_selected_from_choices(isolate(input$na_group_var)%||%(if(length(part)) part[1] else NULL),choices)
      pickerInput_fromtop(session$ns("na_group_var"),"Group by:",choices=choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=TRUE))
    })
    output$na_ref_level_ui<-renderUI({
      req(identical(input$na_group_mode,"reference"))
      req(input$na_group_var%in%group_choices())
      levs<-levels(factor(group_factors()[[input$na_group_var]]))
      train<-levs[grepl("train",levs,ignore.case=TRUE)]
      selected<-if(length(train)) train[1] else levs[1]
      pickerInput_fromtop(session$ns("na_ref_level"),tiphelp5("Learn from:","Level used to fit the imputation (usually the training data)."),choices=levs,selected=selected)
    })
    observeEvent(list(input$na_by_group,input$na_group_dl,input$na_group_var,input$na_group_mode,input$na_ref_level),ignoreInit=TRUE,{
      shinyjs::addClass("run_na_btn","save_changes")
    })


    observeEvent(input$run_na,ignoreInit = T,{
      run_res<-try({



        shinyjs::removeClass("run_na_btn","save_changes")

        data=vals$pp_data
        na_method=input$na_method
        k=input$na_knn
        attr=input$na_targ



        group_args<-list()
        if(isTRUE(input$na_by_group)){
          validate(need(isTRUE(input$na_group_var%in%group_choices()),"Choose the Factor-Attribute column used to group the imputation."))
          group_args<-list(
            group=group_factors()[[input$na_group_var]],
            group_mode=input$na_group_mode,
            ref_level=input$na_ref_level,
            group_name=input$na_group_var
          )
        }
        withProgress(
          min=NA,
          max=NA,
          message="Imputing...",{
            newdata0<-newdata<-do.call(nadata,c(list(data(),na_method,k,attr=attr),group_args))
            attr(newdata,"bag")<-bag_name()
            newdata<-data_migrate(data(),newdata)
            if(input$na_targ=="Factor-Attribute"){
              data_o<-data()
              attr(data_o,"factors")<-newdata0
              newdata<-data_o
            }
            #result_v6
            attr(newdata,"group_info")<-attr(newdata0,"group_info")
            vals$r_imputed<-newdata
          })
        shinyjs::removeClass("run_na_btn","save_changes")
        done_modal()
        vals$done_impute<-T
        vals$r_impute<-NULL


      },silent=TRUE)
      if(inherits(run_res,"try-error")){
        showNotification(paste("Imputation was not done:",attr(run_res,"condition")$message),type="error",duration=10)
      }
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
        gi<-attr(vals$r_imputed,"group_info")
        div(
          div( emgreen(span(original_number_of_nas(),'values were imputed'))),
          if(!is.null(gi)) div(style="font-size: 11px",
                               if(identical(gi$mode,"reference")) paste0("Fitted on ",gi$group," = ",gi$ref_level,", applied to all observations")
                               else paste0("Each level of ",gi$group," imputed separately")),
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
    c(
      lapply(seq_along(titles),function(i){
        p(
          titles[[i]],
          p_descs[[i]]
        )
      }),
      list(p(
        strong("Impute by group:"),
        "uses a Factor-Attribute column, usually the one created by Data partition, to avoid data leakage between training and test observations. 'Learn from one level, apply to all' fits the imputation only on the reference level (e.g. training) and applies it to every observation; for pmm, rf and cart the reference level is imputed first on its own, and the other levels are then imputed with models fitted on it. 'Each level separately' imputes every level using only its own observations."
      ))
    )
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

tool7<-list()
# tool7$ui: Provides a user interface for creating data partitions, supporting balanced and random splits.
# Users can specify partition type, target variables, and parameters like split size and seed for reproducibility.
# Includes dynamic elements for selecting classification or regression models and adjusting inputs based on the chosen type.
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
                uiOutput(ns('split_y_groups'))
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
# tool7$server: Implements the logic for generating data partitions, using user-defined settings and methods.
# Handles balanced and random partitions, ensuring compatibility with classification or regression models.
# Dynamically validates input data and updates partitions in the dataset. Outputs partition summaries and integrates with downstream tools.
tool7$server<-function(id,vals=NULL){

  moduleServer(id,function(input,output,session){


    data<-reactive({
      req(vals$pp_data)
      vals$pp_data
    })




    observe({
      shinyjs::toggle('split_y_groups',condition=input$split_t=="Regression")
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
          '',
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







    output$split_y_groups<-renderUI({
      numericInput(
        session$ns("groups"),
        tags$label(span(tiphelp("For regression, this defines the number of quantile-based groups used during resampling. The sample is split into groups based on percentiles, and sampling is performed within these groups.",'left'),'Groups:'),style=""),
        value = 5,
        min = 2,
        step = 1
      )
    })

    output$split_y_out<-renderUI({
      req(input$part_type=="Balanced")
      req(data())
      req(input$split_t)
      if(input$split_t%in%"Classification"){
        groups_input<-NULL
        choices_factors<-colnames(attr(data(),"factors"))
      } else{
        choices_factors<-colnames(data())

      }

      # selected<-get_selected_from_choices(vals$cur_split_y,choices_factors)
      #selectInput(session$ns("split_y"),SelectedText="Columns selected", label=span(tiphelp("Select the target variable",'left'),'Y:'),choices=choices_factors,selected=vals$cur_split_y)

      div(
        virtualPicker_unique(session$ns("split_y"),tags$label(span(tiphelp("Select the target variable. Choose multiple variables for creating multiple partition columns",'left'),'Y:'),style=""),choices_factors,selected=choices_factors[1],multiple =T)

      )
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
        part_type=input$part_type,
        groups=input$groups
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
      try({

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

tool8<-list()
# tool8$ui: Provides the user interface for aggregating numeric data based on selected factors.
# Users can choose one or more factors for grouping and apply various aggregation functions (e.g., sum, mean, median).
# Includes dynamic input elements and displays summaries of the original and aggregated datasets.
tool8$ui <- function(id){

  ns <- NS(id)

  div(
    class = "class_tool8",

    div(
      style = "display: flex;",
      class = "half-drop-inline",

      div(
        style = "background-color: #f5f5f5; width: 50%; padding: 5px;",
        class = "half-drop",

        div(
          strong("Aggregate:"),
          tiphelp(
            "The process involves two stages. First, collate individual cases of the Numeric-Attribute together with a grouping variable (unselected factors). Second, perform which calculation you want on each group of cases (selected factors)."
          )
        ),

        div(
          class = "virtual-180",
          virtualPicker(
            ns("fac_descs"),
            "factor(s) selected"
          )
        ),

        selectInput(
          ns("spread_measures"),
          "Function:",
          choices = c("sum", "mean", "median", "var", "sd", "min", "max"),
          selected = "mean"
        ),

        div(
          id = ns("run_agg_btn"),
          align = "right",
          class = "save_changes",

          em(
            "Click to aggregate",
            icon("fas fa-hand-point-right")
          ),

          actionButton(
            ns("run_agg"),
            span(
              "RUN",
              img(src = agg_icon2, height = "14", width = "14")
            )
          )
        )
      ),

      div(
        style = "background: white; padding: 15px; width: 275px",
        uiOutput(ns("summ_agg"))
      )
    ),

    div(
      style = "background: white; padding: 15px;",
      class = "half-drop-inline",

      uiOutput(ns("war_agg")),
      uiOutput(ns("print_agg"))
    )
  )
}
# tool8$server: Implements the backend logic for aggregating numeric data.
# Handles user-defined grouping factors and aggregation functions to generate a summarized dataset.
# Coordinates are aggregated (if present) to prevent data loss. Updates the aggregated results and ensures seamless integration with the main dataset.
tool8$server <- function(id, vals = NULL){

  moduleServer(id, function(input, output, session){

    # -------------------------------------------------------------------------
    # Main reactives
    # -------------------------------------------------------------------------

    data <- reactive({
      req(vals$pp_data)
      vals$pp_data
    })

    factors <- reactive({
      fac <- attr(data(), "factors")
      req(fac)
      fac[rownames(data()), , drop = FALSE]
    })

    coords <- reactive({

      coord <- attr(data(), "coords")

      if (is.null(coord)) {
        return(NULL)
      }

      coord[rownames(data()), , drop = FALSE]
    })

    time_attr <- reactive({

      time <- attr(data(), "time")

      if (is.null(time)) {
        return(NULL)
      }

      time[rownames(data()), , drop = FALSE]
    })

    # -------------------------------------------------------------------------
    # UI updates
    # -------------------------------------------------------------------------

    observeEvent(factors(), {

      choices_factor <- colnames(factors())

      if (length(choices_factor) == 0) {
        return()
      }

      selected_factor <- if (length(choices_factor) > 1) {
        choices_factor[1:(length(choices_factor) - 1)]
      } else {
        choices_factor[1]
      }

      shinyWidgets::updateVirtualSelect(
        inputId = "fac_descs",
        choices = choices_factor,
        selected = selected_factor
      )
    })

    active_fac_descs <- reactiveVal(FALSE)
    active_fac_descs(TRUE)

    agg_args <- reactive({

      req(input$fac_descs)
      req(input$spread_measures)

      list(
        fac_descs = input$fac_descs,
        measure = input$spread_measures
      )
    })

    observeEvent(agg_args(), {
      shinyjs::addClass("run_agg_btn", "save_changes")
    })

    observeEvent(input$run_agg, {
      shinyjs::removeClass("run_agg_btn", "save_changes")
    })

    # -------------------------------------------------------------------------
    # Helper: aggregate Temporal-Attribute
    # -------------------------------------------------------------------------

    aggregate_time_attribute <- function(time, group_fac){

      if (is.null(time)) {
        return(NULL)
      }

      if (!is.data.frame(time)) {
        time <- data.frame(time)
      }

      if (ncol(time) == 0) {
        return(NULL)
      }

      time_agg_list <- list()

      for (time_col in colnames(time)) {

        x <- time[[time_col]]

        # Date / datetime: preserve temporal range
        if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {

          if (inherits(x, "POSIXlt")) {
            x <- as.POSIXct(x)
          }

          time_min <- aggregate(
            x,
            group_fac,
            min,
            na.rm = TRUE
          )

          time_max <- aggregate(
            x,
            group_fac,
            max,
            na.rm = TRUE
          )

          time_agg_list[[paste0(time_col, "_min")]] <- time_min[, ncol(time_min)]
          time_agg_list[[paste0(time_col, "_max")]] <- time_max[, ncol(time_max)]

        } else if (is.numeric(x) || is.integer(x)) {

          # Numeric temporal variables: preserve range
          time_min <- aggregate(
            x,
            group_fac,
            min,
            na.rm = TRUE
          )

          time_max <- aggregate(
            x,
            group_fac,
            max,
            na.rm = TRUE
          )

          time_agg_list[[paste0(time_col, "_min")]] <- time_min[, ncol(time_min)]
          time_agg_list[[paste0(time_col, "_max")]] <- time_max[, ncol(time_max)]

        } else {

          # Character/factor temporal descriptors: keep first non-NA unique value
          time_first <- aggregate(
            as.character(x),
            group_fac,
            function(z) {
              z <- unique(z[!is.na(z)])
              if (length(z) == 0) {
                return(NA_character_)
              }
              z[1]
            }
          )

          time_agg_list[[paste0(time_col, "_first")]] <- time_first[, ncol(time_first)]
        }
      }

      time_agg <- data.frame(
        time_agg_list,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      time_agg
    }

    # -------------------------------------------------------------------------
    # Aggregation
    # -------------------------------------------------------------------------

    r_agg <- reactiveVal()

    observeEvent(input$run_agg, ignoreInit = TRUE, {

      try({

        req(input$fac_descs)
        req(input$spread_measures)

        dat <- data()
        fac <- factors()
        coord <- coords()
        time <- time_attr()

        group_fac <- fac[, input$fac_descs, drop = FALSE]

        # ---------------------------------------------------------------------
        # Numeric-Attribute aggregation
        # ---------------------------------------------------------------------

        df <- data.frame(
          aggregate(
            dat,
            data.frame(group_fac),
            get(input$spread_measures),
            na.rm = TRUE
          )
        )

        dfnum <- df[, which(unlist(lapply(df, is.numeric))), drop = FALSE]
        dffac <- df[, which(!unlist(lapply(df, is.numeric))), drop = FALSE]

        labels <- make.unique(
          apply(
            dffac[, input$fac_descs, drop = FALSE],
            1,
            paste,
            collapse = "_"
          )
        )

        rownames(dfnum) <- labels
        rownames(dffac) <- labels

        # ---------------------------------------------------------------------
        # Coords-Attribute aggregation
        # ---------------------------------------------------------------------

        if (!is.null(coord)) {

          coord <- data.frame(
            aggregate(
              coord,
              group_fac[rownames(coord), , drop = FALSE],
              mean,
              na.rm = TRUE
            )
          )

          coord <- coord[, which(unlist(lapply(coord, is.numeric))), drop = FALSE]
          rownames(coord) <- labels
        }

        # ---------------------------------------------------------------------
        # Temporal-Attribute aggregation
        # ---------------------------------------------------------------------

        if (!is.null(time)) {

          time <- aggregate_time_attribute(
            time = time,
            group_fac = group_fac[rownames(time), , drop = FALSE]
          )

          if (!is.null(time)) {
            rownames(time) <- labels
          }
        }

        # ---------------------------------------------------------------------
        # Rebuild Datalist
        # ---------------------------------------------------------------------

        df <- data_migrate(
          dat,
          dfnum,
          "Aggregated_results"
        )

        attr(df, "factors") <- dffac
        attr(df, "coords") <- coord
        attr(df, "time") <- time

        r_agg(df)
      })
    })

    # -------------------------------------------------------------------------
    # Warnings
    # -------------------------------------------------------------------------

    output$war_agg <- renderUI({

      req(r_agg())

      has_coords <- !is.null(coords())
      has_time <- !is.null(time_attr())

      if (!has_coords && !has_time) {
        return(NULL)
      }

      div(
        if (has_coords) {
          div(
            em(
              icon("triangle-exclamation"),
              "The Datalist contained a Coords-Attribute, which was aggregated with the selected factors to prevent loss of coordinates."
            )
          )
        },

        if (has_time) {
          div(
            em(
              icon("triangle-exclamation"),
              "The Datalist contained a Temporal-Attribute, which was aggregated with the selected factors to preserve temporal information."
            )
          )
        }
      )
    })

    # -------------------------------------------------------------------------
    # Summary
    # -------------------------------------------------------------------------

    output$summ_agg <- renderUI({

      req(r_agg())

      nrow_agg <- nrow(r_agg())
      ncol_agg <- ncol(r_agg())

      div(
        div(strong("Original data:")),
        div(em("rows:"), nrow(data())),
        div(em("columns:"), ncol(data())),

        br(),

        div(strong("Aggregated data:")),
        div(em("rows:"), nrow_agg),
        div(em("columns:"), ncol_agg),

        br(),

        if (!is.null(attr(r_agg(), "coords"))) {
          div(em("Coords-Attribute preserved."))
        },

        if (!is.null(attr(r_agg(), "time"))) {
          div(em("Temporal-Attribute preserved."))
        },

        br(),

        emgreen("Save to make permanent")
      )
    })

    # -------------------------------------------------------------------------
    # Final result
    # -------------------------------------------------------------------------

    result <- reactive({
      list(
        r_agg = r_agg()
      )
    })

    result_v8 <- reactive({

      dat <- data()

      if (!is.null(r_agg())) {

        newdata <- r_agg()

        attr(newdata, "bag") <- paste0("_", input$spread_measures)

        dat <- newdata
      }

      dat
    })

    observeEvent(result_v8(), {

      vals$vtools$tool8 <- result_v8()
    })

    return(NULL)
  })
}

tool9<-list()
# tool9$ui: Provides the user interface for creating and managing color palettes.
# Users can select colors, add them to a temporary palette, and save them as reusable palettes.
# Includes options to restart the current palette, view a preview of the new palette, and manage created palettes.
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
# tool9$server: Implements the backend logic for managing color palettes.
# Handles user interactions such as adding colors, creating palettes, and updating the available palettes.
# Ensures seamless integration of new palettes with the broader application, including visual previews and management options.
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
      palette_name<-paste("palette",length(vals$newcolhabs))
      vals$once_cols<-NULL
      vals$newcolhabs[[palette_name]]<-colorRampPalette(vals$newpalette_colors,alpha=T)

    })
    observeEvent(vals$colors_img,{
      selected<-vals$colors_img$val[length(vals$colors_img$val)]
      updatePickerInput(
        session,'available_palette',
        choices = vals$colors_img$val,
        choicesOpt=list(content=vals$colors_img$img),
        selected=selected
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
      req(vals$newpalette_colors)
      renderPlot({
        par(mar=c(0,0,0,0),mai=c(0,0,0,0),mgp=c(0,0,0),xpd=T,ann=F, bg=NA, fg=NA)
        image(t(matrix(seq(0,1,length.out=100), nrow=1)), axes=F,col=  colorRampPalette(  vals$newpalette_colors,alpha=T)(100))
      }, height =30)})


  })
}

tool10<-list()
# tool10$ui: User interface for managing savepoints within the application.
# Provides functionality to create savepoints (download as .rds files), load existing savepoints to restore the application state, and manage active savepoints.
# Includes options for updating the savepoint on demand and showing active savepoint details.
tool10$ui<-function(id){
  ns<-NS(id)
  div(class="half-drop p20",
      div(
        div(class="cogs_in",
            div(style="padding: 5px;height: 100px",
                div(strong(icon("fas fa-thumbtack"),'Create a savepoint:',style="color: #05668D")),
                div(style="display: flex",
                    actionButton(ns("create_savepoint"), style = "font-size: 12px", label="Download"),
                    downloadButton(ns("download_savepoint"), style = "font-size: 12px;visibility: hidden;", label=NULL),
                    div(class="p20",emgray("Download the Savepoint as rds file. You can latter upload the created savepoint (panel bellow).")))
            )),
        div(style="background: #f7f7f7ff;
-webkit-box-shadow: inset 0px 0px 5px #797979ff;margin-top: 2px",
            div(
              div(style="padding: 5px;height: 130px",
                  div(
                    tipify_ui(strong(icon("upload"),"Load a savepoint:",style="color: #05668D"),"Restore the dashboard with a previously saved savepoint (.rds file)","right")),
                  div(id="tool_savepoint", fileInput(ns("load_savepoint"), label=NULL,accept =".rds",multiple =T))))),
        uiOutput(ns("validade_savepoint")),
        div(id=ns("current_savepoint"),
            style="background: #f7f7f7ff;
-webkit-box-shadow: inset 0px 0px 5px #797979ff;margin-top: 2px",
            div(
              style="padding: 5px;height: 130px",
              strong(style="color: #05668D",
                     icon('link'),
                     "Active Savepoint"),
              uiOutput(ns('cur_savepoint')),
              checkboxInput(ns('update_under_demand'),span('Include all inputs and update only on module demand'),width="400px")
            )
        )

      )


  )

}
# tool10$server: Backend logic for handling savepoint operations.
# Implements the creation of savepoints by saving the application state to an .rds file, restoring the state from a selected .rds file, and managing savepoint details.
# Handles user interactions for savepoint creation, loading, and toggling update behaviors, while ensuring data integrity during restoration or replacement.
tool10$server<-function (id,vals){
  moduleServer(id,function(input, output, session){

    ns<-session$ns

    observeEvent(input$update_under_demand,{
      if(isTRUE(input$update_under_demand)){
        vals$update_state<-vals$module_states
        vals$module_states<-NULL
      } else{
        vals$module_states<-vals$update_state
        vals$update_state<-NULL
      }
    })

    observeEvent(input$load_savepoint,{

      output$validade_savepoint<-renderUI({
        validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Error: the uploaded file is not an rds file"))
      })

    })
    savepoint_on<-reactiveVal(FALSE)
    observe({
      shinyjs::toggle('update_under_demand',condition=!is.null(vals$module_states)|!is.null(vals$update_state))
    })
    autoformat_bytes <- function(size_in_bytes) {
      if (size_in_bytes < 1024) {
        return(paste(size_in_bytes, "Bytes"))
      } else if (size_in_bytes < 1024^2) {
        return(paste(round(size_in_bytes / 1024, 2), "KB"))
      } else if (size_in_bytes < 1024^3) {
        return(paste(round(size_in_bytes / 1024^2, 2), "MB"))
      } else {
        return(paste(round(size_in_bytes / 1024^3, 2), "GB"))
      }
    }
    output$cur_savepoint<-renderUI({
      renderPrint({

        save<-input$load_savepoint
        save$size<-sapply(save$size,autoformat_bytes)
        save[,c("name","size")]

      })
    })
    observe({

      shinyjs::toggle('current_savepoint',condition=isTRUE(savepoint_on()))
    })

    output$download_savepoint <-{
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
              curs<-tosave[grepl("cur_",names(tosave))]

              tosave<-tosave[-which(names(vals)%in%c("saved_data","newcolhabs",'colors_img'))]
              tosave<-tosave[-which(unlist(lapply(tosave,function(x) object.size(x)))>2000)]

              tosave$saved_ensemble<-vals$saved_ensemble
              tosave$saved_maps<-vals$saved_maps
              tosave$saved_data<-vals$saved_data
              tosave$newcolhabs<-vals$newcolhabs
              tosave$colors_img<-vals$colors_img
              tosave$update_state[names(vals$input_state)]<-vals$input_state
              tosave$update_state[sapply(tosave$update_state,length)>20]<-NULL

              tosave$input_state<-NULL
              tosave$rid_plot<-NULL
              tosave$rda_plot <-NULL
              tosave$smw_dp<-NULL
              tosave$segrda_model<-NULL
              tosave$mds<-NULL
              tosave$corr_plot<-NULL
              tosave$plot_dp<-NULL
              tosave$seg_rda_plot<-NULL
              tosave$pbox_plot<-NULL
              tosave$dp_smw<-NULL
              tosave$splitBP<-NULL
              tosave$plot_correlation<-NULL
              tosave[names(curs)]<-curs


              saveRDS(tosave, file)
              vals$collectInputs<-0

              beepr::beep(10)

            }
          )

        }
      )}

    observeEvent(input$create_savepoint,{

      vals$collectInputs<-1

    })
    observeEvent(vals$collectInputs,{
      if(vals$collectInputs==2){


        shinyjs::click('download_savepoint')
      }
    })



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
      vals$collectInputs<-0
      vals$cur_tab<-"menu_intro"
      #updateTextInput(session, "tabs", value = 'menu_intro')
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
      savepoint_on(TRUE)
      updateCheckboxInput(session,'update_under_demand',value=F)
      vals$module_states<-NULL
      vals$update_state<-NULL
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
      #vals$colors_img[vals$colors_img$val%in%colors$colors_img$val,]<-colors$colors_img
      vals$newcolhabs[names(colors$newcolhabs)]<-colors$newcolhabs

      vals$module_states<-newvals$update_state
      newvals$update_state<-NULL
      vals$update_state<-NULL


      newvals$collectInputs<-0
      for (var in names(newvals) ) {
        vals[[var]]<-newvals[[var]]
      }




      delay(500,{
        #updateTextInput(session, "tabs", value = newvals$cur_tab)
        runjs("Shiny.setInputValue('last_btn', 'fade');")
      })

      beepr::beep(10)

      removeModal()
      ###vals$update_curtab<-"menu_upload"
      vals$restore_inputs<-TRUE
      vals$update_pp<-"tool_close"

    })


    output$savepoint_request<-renderUI({
      validate(need(length(grep(".rds",input$load_savepoint$datapath))==length(input$load_savepoint$datapath),"Requires rds file"))
      column(12,
             h5(strong('Are you sure? '),style="color: brown"),
             column(
               12,
               p(strong("Warning:", style="color: #0093fcff"),"The application will restart with the loaded savepoint and unsaved changes will be lost")

             ))

    })
  })
}

head_data<-list()
# head_data$ui: User interface for visualizing data summaries.
# Provides radio buttons to toggle between displaying the "head" (first rows), "full" dataset, or the structure ("str") of the data.
head_data$ui<-function(id,options=1:4){
  ns<-NS(id)
  div(
    radioGroupButtons(ns("showas"),NULL , c("head","full","str")[options],status="small_GroupBtn")
  )
}
# head_data$server: Backend logic for managing data visualization.
# Dynamically adjusts the displayed data based on user input ("head", "full", or "str").
# Renders the data as either a table, a structured summary, or an interactive datatable, depending on the user selection and configuration.
# Allows customization of height, width, and additional table properties like page length and display options.
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

# pp_data$ui: User interface for selecting and managing the active datalist.
# Provides a dropdown menu for choosing a datalist from the saved datasets.
# Includes a tooltip for better user guidance on the functionality.
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
# pp_data$server: Backend logic for handling datalist selection and data updates.
# Dynamically updates the current datalist (`vals$cur_data`) based on user selection.
# Ensures reactive management of the active dataset (`vals$pp_data`) and synchronizes it with user input.
# Handles potential errors gracefully when accessing invalid or unavailable datasets.
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
      req(input$data_upload)
      req(input$data_upload%in%names(vals$saved_data))
      vals$r_impute<-T
      data<-vals$saved_data[[input$data_upload]]
      req(data)
      attr(data,"datalist_root")<-input$data_upload
      data
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

#' @export
toolbar<-list()
#' @export
# toolbar$ui:
# Defines the main toolbar interface, which provides access to a series of data manipulation tools.
# Includes tabs for various preprocessing tasks such as filtering, transformations, imputation, and aggregation.
# Dynamically updates the displayed content based on user interaction with the toolbar options.
# Integrates icons and tooltips for better user navigation and understanding of available actions.
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
#' @export
# toolbar$server:
# Manages the logic for activating and controlling the selected tools from the toolbar.
# Handles user interactions with the toolbar tabs, ensuring the correct tool modules are loaded and displayed.
# Synchronizes the currently selected tool with the global `vals` reactive values, allowing for consistent state management across modules.
# Ensures visual feedback by toggling UI elements based on the active tool, such as highlighting unsaved changes and updating tool titles.
# Provides a seamless workflow for switching between tools and maintaining the context of the currently active dataset.
toolbar$server<-function(id, vals=NULL){
  moduleServer(id,function(input, output,session){

    shinyjs::onevent("mouseenter", "toolkit",{
      shinyjs::show(selector=".toolkit_items")

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

      shinyjs::toggle(selector=".track_changes_panel",condition=!input$radio_cogs%in%c("tool_close","tool2","tool9","tool10"))
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
    tool_titles<-c(
      tool2='',
      tool3='Filter observations',
      tool4='Filter variables',
      tool5='Transformations',
      tool6='Data imputation',
      tool7='Data partition',
      tool8='Aggregate',
      tool9='Create Palette',
      tool10='Savepoint'
    )
    title<-reactive({
      req(input$radio_cogs)
      tool_titles[[input$radio_cogs]]
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



    toolbar_servers<-list(
      tool2=tool2,
      tool3=tool3,
      tool4=tool4,
      tool5=tool5,
      tool6=tool6,
      tool7=tool7,
      tool8=tool8,
      tool9=tool9,
      tool10=tool10
    )
    for(tool_id in names(toolbar_servers)){
      local({
        tool_id_i<-tool_id
        tool_i<-toolbar_servers[[tool_id_i]]
        output[[paste0("out_",tool_id_i)]]<-renderUI({
          tool_i$server(tool_id_i,vals)
          NULL
        })
      })
    }







    observeEvent(vals$pp_data,{
      vals$r_imputed<-vals$pp_data
    })



    return(NULL)

  })
}


#' @export
pre_process<-list()
#' @export
# pre_process$ui:
# Builds the UI for the pre-processing module, providing a structured layout for data manipulation tools.
# Includes sections for displaying and managing the active dataset, tracking changes, and interacting with preprocessing tools.
# Dynamically integrates submodules like `pp_data` and `toolbar` to manage data operations.
# Implements user-friendly features such as tooltips, modal dialogs, and collapsible panels for enhanced navigation and usability.
pre_process$ui<-function(id){
  module_progress("Loading module: Pre-processing tools")
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
                  )
              )
            ),
            div(class="toolbar",
                id=ns("tool_pages"),
                div(class="dl_btn",
                    cogs_title="Create Datalit",
                    popify(bsButton(ns("create_datalist"), strong(icon("fas fa-plus")), style=""),"Data Input", paste(
                      "Create Datalists and upload your data and their attributes. All analyses available at ",strong('iMESc',style="font-family: 'Alata', sans-serif;")," will require a Datalist created by the user, either uploaded or using example data."


                    ))
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
# pre_process$server:
# Implements the server-side logic for the pre-processing module, coordinating interactions and state management.
# Tracks changes to the active dataset (`vals$pp_data`) and integrates tools for creating, modifying, and saving data lists.
# Manages the state and results of preprocessing tools, ensuring consistency across sessions.
# Provides mechanisms for saving changes (either creating new datasets or replacing existing ones) and confirms user actions with modals.
# Includes functionality for toggling visibility of elements, such as tracking panels and toolbars, to streamline user interactions.
# Ensures smooth integration with other modules like `pp_data` and `toolbar`, leveraging their respective server functions.
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
    observeEvent(vals$exit_tool10,{
      updateNavbarPage(session,"toolbar-radio_cogs", selected="tool_close")
      shinyjs::hide(selector=".fade_pp")
    })
    observeEvent(vals$exit_tool11,{
      updateNavbarPage(session,"toolbar-radio_cogs", selected="tool_close")
      shinyjs::hide(selector=".fade_pp")
    })
    observeEvent(vals$exit_tool12,{
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
