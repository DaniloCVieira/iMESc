# Spatial Tools > Autocorrelation
# Global Moran's I / Geary's C, Local Moran (LISA), Moran correlogram and semivariogram of the variable
# chosen in the Data setup of Spatial Tools (vals$data_map: one column, coordinates in attr "coords").
# Computations in inst/www/funs_autocorr.R.

sptools_autocorr<-list()

# spatial weights box (Global and LISA tabs; p = prefix of the input ids)
ac_weights_ui<-function(ns,p){
  box_caret(
    ns(paste0("box_",p,"_weights")),
    color="#c3cc74ff",
    title="Spatial weights",
    inline=F,
    div(
      pickerInput_fromtop(ns(paste0(p,"_wtype")),
                          tiphelp5("Neighbours:","How neighbours are defined: the k nearest observations, all observations within a distance band, or all observations weighted by the inverse of the distance (1/d^power). Distances are great-circle distances in km."),
                          choices=c("k nearest neighbours"="knn","Distance band"="band","Inverse distance"="idw")),
      numericInput(ns(paste0(p,"_k")),"k:",value=4,min=1,step=1),
      numericInput(ns(paste0(p,"_dmax")),tiphelp5("Max. distance (km):","Distance band: neighbours up to this distance (empty = the smallest band giving every observation a neighbour). Inverse distance: optional limit (empty = all observations)."),value=NA,min=0),
      numericInput(ns(paste0(p,"_power")),"Power:",value=1,min=0.1,step=0.5),
      checkboxInput(ns(paste0(p,"_rowstd")),tiphelp5("Row-standardise","Each row of the weights matrix sums to 1, so each observation is compared with the mean of its neighbours."),value=TRUE),
      uiOutput(ns(paste0(p,"_wsummary")))
    )
  )
}

sptools_autocorr$ui<-function(id){
  ns<-NS(id)
  alt_choices<-c("Two-sided"="two.sided","Positive (greater)"="greater","Negative (less)"="less")
  padj_choices<-c("None"="none","FDR (Benjamini-Hochberg)"="BH","Holm"="holm","Bonferroni"="bonferroni")
  div(
    style="background: white",
    # shown only when observations share their location (e.g. repeated sampling of a station)
    hidden(div(
      id=ns("ac_mean_box"),style="padding: 5px 10px 0px 10px",
      checkboxInput(ns("ac_mean"),tiphelp5("Mean of observations at the same location","Observations with identical coordinates (e.g. a station sampled in several seasons) are averaged, so each location enters the analyses once. When saving LISA clusters, each observation receives the cluster of its location."),value=FALSE),
      uiOutput(ns("ac_mean_info"))
    )),
    tabsetPanel(
      id=ns("ac_tab"),
      # ---- Global
      tabPanel(
        "Global Moran's I",value="global",
        column(
          4,class="mp0",
          ac_weights_ui(ns,"g"),
          box_caret(
            ns("box_g_opts"),
            color="#c3cc74ff",
            title="Test",
            inline=F,
            div(
              pickerInput_fromtop(ns("g_alt"),"Alternative:",choices=alt_choices),
              numericInput(ns("g_nsim"),tiphelp5("Permutations:","Number of random permutations of the values for the permutation p-value (0 = only the normal approximation)."),value=499,min=0,step=100),
              numericInput(ns("g_seed"),"Seed:",value=1,step=1),
              numericInput(ns("g_base_size"),"Base size:",value=12,min=6,step=1)
            )
          )
        ),
        column(
          8,class="mp0",
          box_caret(
            ns("box_g_res"),
            title="Global autocorrelation",
            button_title=actionLink(ns("down_g_table"),"Download",icon("fas fa-table")),
            div(
              uiOutput(ns("g_header")),
              tableOutput(ns("g_table")),
              emgray("Moran's I above its expectation (and Geary's C below 1) indicates positive spatial autocorrelation: nearby observations have similar values.")
            )
          ),
          box_caret(
            ns("box_g_plot"),
            title="Moran scatterplot",
            button_title=actionLink(ns("down_g_plot"),"Download",icon("download")),
            div(plotOutput(ns("g_plot"),height="380px"))
          ),
          box_caret(
            ns("box_g_all"),
            title="All numeric variables",
            button_title=actionLink(ns("down_g_all"),"Download",icon("fas fa-table")),
            div(
              actionButton(ns("g_run_all"),"Run for all variables",icon("play")),
              emgray("Global Moran's I and Geary's C for every numeric variable of the Datalist (same observations, weights and test options)."),
              div(style="overflow-x: auto",tableOutput(ns("g_all_table")))
            )
          )
        )
      ),
      # ---- Local
      tabPanel(
        "Local Moran (LISA)",value="local",
        column(
          4,class="mp0",
          ac_weights_ui(ns,"l"),
          box_caret(
            ns("box_l_opts"),
            color="#c3cc74ff",
            title="Test and map",
            inline=F,
            div(
              numericInput(ns("l_alpha"),"Significance level:",value=0.05,min=0.001,max=0.5,step=0.01),
              pickerInput_fromtop(ns("l_padj"),tiphelp5("p adjustment:","Correction for the many local tests (one per observation)."),choices=padj_choices,selected="none"),
              pickerInput_fromtop(ns("l_alt"),"Alternative:",choices=alt_choices),
              pickerInput_fromtop(ns("l_what"),"Map:",choices=c("Clusters"="cluster","Local Moran's I"="Ii")),
              numericInput(ns("l_point_size"),"Point size:",value=2.5,min=0.5,step=0.5),
              checkboxInput(ns("l_shapes"),"Show base and layer shapes",value=TRUE),
              numericInput(ns("l_base_size"),"Base size:",value=12,min=6,step=1)
            )
          ),
          box_caret(
            ns("box_l_save"),
            color="#c3cc74ff",
            title="Save clusters",
            inline=F,
            div(
              textInput(ns("l_colname"),"New Factor-Attribute column:",value=""),
              actionButton(ns("l_save"),"Save",icon("fas fa-save")),
              emgray("The LISA clusters are added to the Factor-Attribute of the Datalist (observations outside the filter get NA).")
            )
          )
        ),
        column(
          8,class="mp0",
          box_caret(
            ns("box_l_map"),
            title="LISA map",
            button_title=actionLink(ns("down_l_plot"),"Download",icon("download")),
            div(plotOutput(ns("l_plot"),height="420px"),uiOutput(ns("l_counts")))
          ),
          box_caret(
            ns("box_l_table"),
            title="Local statistics",
            button_title=actionLink(ns("down_l_table"),"Download",icon("fas fa-table")),
            div(style="max-height: 300px; overflow: auto",tableOutput(ns("l_table")))
          )
        )
      ),
      # ---- Correlogram
      tabPanel(
        "Correlogram",value="correlogram",
        column(
          4,class="mp0",
          box_caret(
            ns("box_c_opts"),
            color="#c3cc74ff",
            title="Distance classes",
            inline=F,
            div(
              numericInput(ns("c_classes"),"Number of classes:",value=10,min=2,step=1),
              numericInput(ns("c_dmax"),tiphelp5("Max. distance (km):","Classes up to this distance (empty = half of the largest distance between observations)."),value=NA,min=0),
              pickerInput_fromtop(ns("c_breaks"),"Classes:",choices=c("Equal width"="width","Equal number of pairs"="pairs")),
              pickerInput_fromtop(ns("c_padj"),"p adjustment:",choices=padj_choices,selected="holm"),
              numericInput(ns("c_alpha"),"Significance level:",value=0.05,min=0.001,max=0.5,step=0.01),
              numericInput(ns("c_base_size"),"Base size:",value=12,min=6,step=1)
            )
          )
        ),
        column(
          8,class="mp0",
          box_caret(
            ns("box_c_plot"),
            title="Moran correlogram",
            button_title=actionLink(ns("down_c_plot"),"Download",icon("download")),
            div(plotOutput(ns("c_plot"),height="380px"))
          ),
          box_caret(
            ns("box_c_table"),
            title="Correlogram table",
            button_title=actionLink(ns("down_c_table"),"Download",icon("fas fa-table")),
            div(style="overflow-x: auto",tableOutput(ns("c_table")))
          )
        )
      ),
      # ---- Semivariogram
      tabPanel(
        "Semivariogram",value="variogram",
        column(
          4,class="mp0",
          box_caret(
            ns("box_v_opts"),
            color="#c3cc74ff",
            title="Semivariogram",
            inline=F,
            div(
              numericInput(ns("v_cutoff"),tiphelp5("Cutoff (km):","Largest distance considered (empty = one third of the largest distance between observations)."),value=NA,min=0),
              numericInput(ns("v_lags"),"Number of lags:",value=15,min=2,step=1),
              pickerInput_fromtop(ns("v_model"),tiphelp5("Model:","Variogram model fitted with gstat::fit.variogram (with a nugget)."),
                                  choices=c("None","Spherical"="Sph","Exponential"="Exp","Gaussian"="Gau","Matern"="Mat")),
              checkboxInput(ns("v_sized"),"Point size by number of pairs",value=FALSE),
              numericInput(ns("v_base_size"),"Base size:",value=12,min=6,step=1)
            )
          )
        ),
        column(
          8,class="mp0",
          box_caret(
            ns("box_v_plot"),
            title="Semivariogram",
            button_title=actionLink(ns("down_v_plot"),"Download",icon("download")),
            div(plotOutput(ns("v_plot"),height="380px"),uiOutput(ns("v_warning")))
          ),
          box_caret(
            ns("box_v_table"),
            title="Semivariogram table",
            button_title=actionLink(ns("down_v_table"),"Download",icon("fas fa-table")),
            div(style="overflow-x: auto",uiOutput(ns("v_fit_table")),tableOutput(ns("v_table")))
          )
        )
      )
    )
  )
}

sptools_autocorr$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    for(b in c("box_g_weights","box_g_opts","box_g_res","box_g_plot","box_g_all",
               "box_l_weights","box_l_opts","box_l_save","box_l_map","box_l_table",
               "box_c_opts","box_c_plot","box_c_table","box_v_opts","box_v_plot","box_v_table")){
      box_caret_server(b)
    }

    # ---- data of the Data setup (one numeric variable, filtered rows, coordinates)
    ac_data<-reactive({
      d<-vals$data_map
      req(is.data.frame(d))
      validate(need(ncol(d)==1&&is.numeric(d[,1]),"Spatial autocorrelation requires a numeric variable: choose the Numeric-Attribute in the Data setup."))
      coords<-attr(d,"coords")
      validate(need(length(coords)>0,"The selected Datalist has no coordinates."))
      validate(need(all(rownames(d)%in%rownames(coords)),"The coordinates do not include all observations."))
      validate(need(nrow(d)>=4,"At least 4 observations are needed."))
      validate(need(nrow(d)<=5000,"Too many observations for the distance matrix (maximum 5000): use the filter of the Data setup."))
      d
    })
    # coordinates and location of each observation of the Data setup
    raw_coords<-reactive({
      d<-ac_data()
      attr(d,"coords")[rownames(d),,drop=FALSE]
    })
    loc_id<-reactive(ac_location_groups(raw_coords()))
    has_shared<-reactive(any(duplicated(loc_id())))
    observe(shinyjs::toggle("ac_mean_box",condition=isTRUE(has_shared())))
    use_mean<-reactive(isTRUE(input$ac_mean)&&isTRUE(has_shared()))
    output$ac_mean_info<-renderUI({
      req(has_shared())
      emgray(paste0(nrow(ac_data())," observations at ",length(unique(loc_id()))," locations",
                    if(use_mean()) ": the analyses use one mean value per location" else ""))
    })
    # values and coordinates used in the analyses (means per location when chosen)
    ac_x<-reactive({
      d<-ac_data()
      if(!use_mean()) return(d[,1])
      m<-ac_location_mean(d[,1,drop=FALSE],loc_id())
      stats::setNames(m[,1],rownames(m))
    })
    ac_coords<-reactive({
      co<-raw_coords()
      if(!use_mean()) return(co)
      co[unique(loc_id()),,drop=FALSE]
    })
    var_name<-reactive(colnames(ac_data())[1])
    datalist_name<-reactive({
      a<-attr(ac_data(),"args")
      if(!is.null(a$name)) a$name else attr(ac_data(),"datalist")
    })

    # distances: recomputed only when the observations or their coordinates change
    dist_cache<-new.env()
    dist_km<-reactive({
      co<-ac_coords()
      key<-paste(c(rownames(co),format(as.matrix(co))),collapse="|")
      if(!identical(dist_cache$key,key)){
        dist_cache$d<-ac_dist_km(co)
        dist_cache$key<-key
      }
      dist_cache$d
    })

    # ---- spatial weights (prefix g = Global, l = Local)
    weights_of<-function(p){
      reactive({
        type<-input[[paste0(p,"_wtype")]]
        req(type)
        d<-dist_km()
        tryCatch(
          ac_weights(d,type=type,k=input[[paste0(p,"_k")]]%||%4,dmax=input[[paste0(p,"_dmax")]]%||%NA,
                     power=input[[paste0(p,"_power")]]%||%1,row_std=isTRUE(input[[paste0(p,"_rowstd")]])),
          error=function(e) validate(need(FALSE,conditionMessage(e)))
        )
      })
    }
    W_g<-weights_of("g")
    W_l<-weights_of("l")
    for(p in c("g","l")){
      local({
        pp<-p
        observe({
          type<-input[[paste0(pp,"_wtype")]]
          req(type)
          shinyjs::toggle(paste0(pp,"_k"),condition=type=="knn")
          shinyjs::toggle(paste0(pp,"_dmax"),condition=type%in%c("band","idw"))
          shinyjs::toggle(paste0(pp,"_power"),condition=type=="idw")
        })
        output[[paste0(pp,"_wsummary")]]<-renderUI({
          W<-if(pp=="g") W_g() else W_l()
          s<-ac_neighbours_summary(W)
          d<-dist_km()
          n_same<-sum(rowSums(d==0)>1)
          div(style="font-size: 12px; color: gray",
              if(n_same>0) div(style="color: brown",paste0(n_same," observations share their location with another observation (e.g. repeated sampling of a station) and are neighbours of each other. Use the filter of the Data setup to analyse one sampling occasion.")),
              if(!is.null(attr(W,"dmax"))) div(paste0("Distance band: ",signif(attr(W,"dmax"),3)," km",
                                                    if(isTRUE(attr(W,"dmax_auto"))) " (automatic: the smallest band giving every observation a neighbour)" else "")),
              paste0(s$Observations," observations; neighbours per observation: ",s[["Min. neighbours"]],"-",s[["Max. neighbours"]],
                     " (mean ",s[["Mean neighbours"]],")"),
              if(s[["Without neighbours"]]>0) div(style="color: brown",paste(s[["Without neighbours"]],"observation(s) without neighbours")),
              if(s[["Min. neighbours"]]>=s$Observations-1&&!identical(attr(W,"type"),"idw"))
                div(style="color: brown","Every observation is a neighbour of all the others: the test is not informative. Reduce the distance or k."))
        })
      })
    }

    # whole-number columns (classes, pairs, n) without decimals; the others with 4 significant digits
    fmt_table<-function(df,digits=4){
      for(i in seq_along(df)){
        x<-df[[i]]
        if(!is.numeric(x)) next
        df[[i]]<-if(all(is.na(x)|(abs(x)<1e9&x==round(x)))) as.integer(round(x)) else signif(x,digits)
      }
      df
    }

    # ---- Global
    global_res<-reactive({
      req(input$g_alt)
      nsim<-max(0,round(input$g_nsim%||%0))
      withProgress(message="Global Moran's I",{
        tryCatch(ac_global(ac_x(),W_g(),nsim=nsim,alternative=input$g_alt,seed=input$g_seed),
                 error=function(e) validate(need(FALSE,conditionMessage(e))))
      })
    })
    output$g_header<-renderUI({
      r<-global_res()
      div(style="margin-bottom: 5px",strong(var_name()),
          emgray(paste0(" n = ",attr(r,"n"),"; ",attr(r,"nsim")," permutations; alternative: ",attr(r,"alternative"))))
    })
    output$g_table<-renderTable(fmt_table(global_res()),striped=TRUE,bordered=TRUE,spacing="xs",digits=4)
    g_plot<-reactive({
      ac_plot_scatter(ac_x(),W_g(),var_name=var_name(),base_size=input$g_base_size%||%12)
    })
    output$g_plot<-renderPlot(g_plot())

    all_res<-reactiveVal(NULL)
    # the table refers to a Datalist and filter: cleared when they change
    observeEvent(list(datalist_name(),rownames(ac_data()),use_mean()),ignoreInit=TRUE,{ all_res(NULL) })
    observeEvent(input$g_run_all,{
      name<-datalist_name()
      req(name%in%names(vals$saved_data))
      full<-vals$saved_data[[name]]
      rows<-rownames(ac_data())
      num<-full[rows,vapply(full,is.numeric,logical(1)),drop=FALSE]
      validate(need(ncol(num)>0,"No numeric variables."))
      if(use_mean()) num<-ac_location_mean(num,loc_id())
      W<-W_g()
      nsim<-max(0,round(input$g_nsim%||%0))
      res<-withProgress(message="Moran's I for all variables",min=0,max=ncol(num),{
        do.call(rbind,lapply(seq_along(num),function(j){
          incProgress(1)
          r<-tryCatch(ac_global(num[[j]],W,nsim=nsim,alternative=input$g_alt,seed=input$g_seed),error=function(e) NULL)
          if(is.null(r)) return(data.frame(Variable=colnames(num)[j],n=NA,"Moran's I"=NA,"I p (normal)"=NA,"I p (permutation)"=NA,
                                           "Geary's C"=NA,"C p (normal)"=NA,"C p (permutation)"=NA,check.names=FALSE))
          data.frame(Variable=colnames(num)[j],n=attr(r,"n"),
                     "Moran's I"=r$Observed[1],"I p (normal)"=r[["p (normal)"]][1],"I p (permutation)"=r[["p (permutation)"]][1],
                     "Geary's C"=r$Observed[2],"C p (normal)"=r[["p (normal)"]][2],"C p (permutation)"=r[["p (permutation)"]][2],
                     check.names=FALSE)
        }))
      })
      all_res(res)
    })
    output$g_all_table<-renderTable({
      req(all_res())
      fmt_table(all_res())
    },striped=TRUE,bordered=TRUE,spacing="xs",digits=4)

    # ---- Local
    local_res<-reactive({
      req(input$l_padj,input$l_alt)
      tryCatch(ac_local(ac_x(),W_l(),alpha=input$l_alpha%||%0.05,p_adjust=input$l_padj,alternative=input$l_alt),
               error=function(e) validate(need(FALSE,conditionMessage(e))))
    })
    l_plot<-reactive({
      req(input$l_what)
      d<-ac_data()
      shapes<-isTRUE(input$l_shapes)
      ac_plot_lisa(local_res(),ac_coords(),
                   base_shape=if(shapes) attr(d,"base_shape") else NULL,
                   layer_shape=if(shapes) attr(d,"layer_shape") else NULL,
                   base_size=input$l_base_size%||%12,point_size=input$l_point_size%||%2.5,what=input$l_what)
    })
    output$l_plot<-renderPlot(l_plot())
    output$l_counts<-renderUI({
      tb<-table(local_res()$cluster)
      emgray(paste(paste0(names(tb),": ",as.integer(tb)),collapse="; "))
    })
    l_table<-reactive({
      r<-local_res()
      data.frame(ID=rownames(r),Value=r$value,"Local I"=r$Ii,Expected=r$E,z=r$z,p=r$p,"p adjusted"=r$p_adj,Cluster=as.character(r$cluster),check.names=FALSE)
    })
    output$l_table<-renderTable(fmt_table(l_table()),striped=TRUE,bordered=TRUE,spacing="xs",digits=4)
    observeEvent(var_name(),{
      updateTextInput(session,"l_colname",value=paste0("LISA_",var_name()))
    })
    observeEvent(input$l_save,{
      name<-datalist_name()
      if(!isTRUE(name%in%names(vals$saved_data))){
        showNotification("The Datalist of the Data setup was not found.",type="error")
        return()
      }
      col<-trimws(input$l_colname%||%"")
      if(!nzchar(col)){
        showNotification("Enter a name for the new column.",type="warning")
        return()
      }
      r<-local_res()
      fac<-attr(vals$saved_data[[name]],"factors")
      if(!is.data.frame(fac)){
        showNotification("The Datalist has no Factor-Attribute.",type="error")
        return()
      }
      if(col%in%colnames(fac)) col<-make.unique(c(colnames(fac),col))[ncol(fac)+1]
      # with means per location, each observation receives the cluster of its location
      ids<-rownames(fac)
      if(use_mean()){
        lid<-loc_id()
        ids<-unname(lid[ids])
      }
      v<-as.character(r$cluster)[match(ids,rownames(r))]
      fac[[col]]<-factor(v,levels=levels(r$cluster))
      attr(vals$saved_data[[name]],"factors")<-fac
      showNotification(paste0("LISA clusters saved as '",col,"' in the Factor-Attribute of '",name,"'"),type="message")
    })

    # ---- Correlogram
    corr_res<-reactive({
      req(input$c_breaks,input$c_padj)
      withProgress(message="Moran correlogram",{
        tryCatch(ac_correlogram(ac_x(),dist_km(),n_classes=input$c_classes%||%10,dmax=input$c_dmax%||%NA,
                                breaks=input$c_breaks,p_adjust=input$c_padj,alpha=input$c_alpha%||%0.05),
                 error=function(e) validate(need(FALSE,conditionMessage(e))))
      })
    })
    c_plot<-reactive(ac_plot_correlogram(corr_res(),base_size=input$c_base_size%||%12))
    output$c_plot<-renderPlot(c_plot())
    c_table<-reactive({
      r<-corr_res()
      data.frame(Class=r$class,"From (km)"=r$from,"To (km)"=r$to,Pairs=r$pairs,"Moran's I"=r$I,Expected=r$Expected,
                 z=r$z,p=r$p,"p adjusted"=r$p_adj,Significant=ifelse(r$significant,"yes","no"),check.names=FALSE)
    })
    output$c_table<-renderTable(fmt_table(c_table()),striped=TRUE,bordered=TRUE,spacing="xs",digits=4)

    # ---- Semivariogram
    vario_res<-reactive({
      req(input$v_model)
      tryCatch(ac_variogram(ac_x(),ac_coords(),cutoff=input$v_cutoff%||%NA,n_lags=input$v_lags%||%15,model=input$v_model),
               error=function(e) validate(need(FALSE,conditionMessage(e))))
    })
    v_plot<-reactive(ac_plot_variogram(vario_res(),base_size=input$v_base_size%||%12,sized=isTRUE(input$v_sized)))
    output$v_plot<-renderPlot(v_plot())
    output$v_warning<-renderUI({
      v<-vario_res()
      fit<-attr(v,"fit")
      msgs<-list()
      if(isTRUE(attr(v,"coincident")>0)) msgs<-c(msgs,list(div(style="color: gray",paste0(attr(v,"coincident")," pairs of observations at the same place (distance 0, e.g. repeated sampling of a station): shown at distance 0 and left out of the model fit. Use the filter of the Data setup to analyse one sampling occasion."))))
      if(!identical(input$v_model,"None")&&is.null(fit)) msgs<-c(msgs,list(div(style="color: brown","The model could not be fitted: try another model, cutoff or number of lags.")))
      if(!is.null(attr(fit,"warning"))) msgs<-c(msgs,list(div(style="color: brown",attr(fit,"warning"))))
      if(!length(msgs)) return(NULL)
      do.call(div,msgs)
    })
    v_table<-reactive({
      v<-vario_res()
      data.frame(Lag=seq_len(nrow(v)),"Distance (km)"=v$dist,Semivariance=v$gamma,Pairs=v$np,check.names=FALSE)
    })
    output$v_table<-renderTable(fmt_table(v_table()),striped=TRUE,bordered=TRUE,spacing="xs",digits=4)
    output$v_fit_table<-renderUI({
      fit<-attr(vario_res(),"fit")
      req(fit)
      div(strong("Fitted model"),
          renderTable(fmt_table(data.frame(Component=as.character(fit$model),"Partial sill"=fit$psill,"Range (km)"=fit$range,check.names=FALSE)),
                      striped=TRUE,bordered=TRUE,spacing="xs",digits=4),
          strong("Empirical semivariogram"))
    })

    # ---- downloads
    down_plot<-function(plot_r,message,name){
      vals$hand_plot<-"generic_gg"
      generic<-imesc_plot_or_null(plot_r())
      module_ui_figs("downfigs")
      callModule(module_server_figs,"downfigs",vals=vals,generic=generic,message=message,
                 name_c=paste0(name,"_",var_name()),datalist_name=datalist_name())
    }
    down_table<-function(table_r,message,name){
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      callModule(module_server_downcenter,"downcenter",vals=vals,message=message,data=table_r(),
                 name=paste0(name,"_",var_name()))
    }
    observeEvent(input$down_g_plot,ignoreInit=TRUE,down_plot(g_plot,"Moran scatterplot","moran_scatterplot"))
    observeEvent(input$down_l_plot,ignoreInit=TRUE,down_plot(l_plot,"LISA map","lisa_map"))
    observeEvent(input$down_c_plot,ignoreInit=TRUE,down_plot(c_plot,"Moran correlogram","moran_correlogram"))
    observeEvent(input$down_v_plot,ignoreInit=TRUE,down_plot(v_plot,"Semivariogram","semivariogram"))
    observeEvent(input$down_g_table,ignoreInit=TRUE,down_table(function() data.frame(global_res(),check.names=FALSE),"Global autocorrelation","global_autocorrelation"))
    observeEvent(input$down_g_all,ignoreInit=TRUE,{
      if(is.null(all_res())){
        showNotification("Click 'Run for all variables' first.",type="warning")
        return()
      }
      down_table(all_res,"Global autocorrelation (all variables)","global_autocorrelation_all")
    })
    observeEvent(input$down_l_table,ignoreInit=TRUE,down_table(l_table,"Local Moran's I (LISA)","lisa"))
    observeEvent(input$down_c_table,ignoreInit=TRUE,down_table(c_table,"Moran correlogram","moran_correlogram"))
    observeEvent(input$down_v_table,ignoreInit=TRUE,down_table(v_table,"Semivariogram","semivariogram"))

    NULL
  })
}
