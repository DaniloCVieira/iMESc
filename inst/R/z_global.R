library(shiny)
options(shiny.autoload.r=FALSE)
#' @importFrom caret getModelInfo
#library(viridis)
#library(shinyBS)
#library(shinyjs)
#library(shinydashboard)

#library(shiny)
#library(shinybusy)
#rm(list=ls())
inicio<-Sys.time()


load_required_packages<-function(){
  packages_required<-c('ade4','aweSOM','base64enc','beepr','caret','class','cluster','colorRamps','colorspace','colourpicker','data.table','DBI','dendextend','doParallel',
'doSNOW','dplyr','DT','e1071','factoextra','foreach','gbm','gbRd','geodist','ggnewscale','ggplot2','ggpubr','ggraph','ggrepel','ggridges','ggthemes','golem','gplots','graphics','gstat','igraph','kernlab','klaR','kohonen','lattice','leaflet','leaflet.minicharts','MASS','methods','Metrics','parallel','pdp','plotly','pROC','processx','purrr','randomForestExplainer','raster','RColorBrewer','readxl','remotes','reshape','rintrojs','rstudioapi','scales','scatterpie','segRDA','sf','shiny','shinyBS','shinybusy',
'shinycssloaders','shinydashboard','shinydashboardPlus','shinyjqui','shinyjs','shinyTree','shinyWidgets','snow','sortable','sp','stats','stringr','subniche','tools','utils','vegan','viridisLite','wesanderson','writexl')

  # Loop para carregar cada pacote
  for (pkg in packages_required) {
    library(pkg, character.only = TRUE)
  }

}
#load_required_packages()



jscode_screen<-'var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            '

#last_update<-"2022-11-08"
if(getwd()=="D:/R3/imesc2024/app_new/inst"){
  last_update<-Sys.Date()
  saveRDS(last_update,"www/last_update.rds")
  qsave=T


 # rstudio<- rstudioapi::versionInfo()

}

last_update<-readRDS('www/last_update.rds')
#last_update<-format(Sys.Date(),"%d-%m-%Y")

TESTE<- reactive({
  mybooks_teste<-readRDS('vals.rds')
  for (var in names(mybooks_teste)) {    vals[[var]]<-mybooks_teste[[var]]  }
  updateTextInput(session, "tabs", value = mybooks_teste$cur_tab)

})







#deps<-tools::package_dependencies(packages = list.of.packages,recursive = TRUE)
#which(unlist(lapply(deps,function(x){ 'rgdal'%in%x})))

transf_df<-list(

  list(label="None", value="None", tooltip="No transformation"),
  list(label = "log2", value = "log2", tooltip = "logarithmic base 2 transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of the logarithm. Zeros are left as zeros. Higher bases give less weight to quantities and more to presences, and logbase = Inf gives the presence/absence scaling. Please note this is not log(x+1). Anderson et al. (2006) suggested this for their (strongly) modified Gower distance, but the standardization can be used independently of distance indices."),
  list(label = "log10", value = "log10", tooltip = " logarithmic base 10 transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of the logarithm. Zeros are left as zeros. Higher bases give less weight to quantities and more to presences, and logbase = Inf gives the presence/absence scaling. Please note this is not log(x+1). Anderson et al. (2006) suggested this for their (strongly) modified Gower distance, but the standardization can be used independently of distance indices."),
  list(label = "Total", value = "total", tooltip = "divide by the line (observation) total"),
  list(label = "Max", value = "max", tooltip = "divide by column (variable) maximum"),
  list(label = "Frequency", value = "frequency", tooltip = "divide by column (variable) total and multiply by the number of non-zero items, so that the average of non-zero entries is one"),
  list(label = "Range", value = "range", tooltip = "standardize column (variable) values into range 0 ... 1. If all values are constant, they will be transformed to 0"),
  list(label = "Presence/Absence", value = "pa", tooltip = "scale x to presence/absence scale (0/1)"),
  list(label = "Chi.square", value = "chi.square", tooltip = "divide by row sums and square root of column sums, and adjust for square root of matrix total"),
  list(label = "Hellinger", value = "hellinger", tooltip = "square root of method = total"),
  list(label = "Sqrt2", value = "sqrt2", tooltip = "square root"),
  list(label = "Sqrt4", value = "sqrt4", tooltip = "4th root"),
  list(label = "Log2(x+1)", value = "log2(x+1)", tooltip = " logarithmic base 2 transformation (x+1)"),
  list(label = "Log10(x+1)", value = "log10(x+1)", tooltip = "logarithmic base 10 transformation (x+1)"),
  list(label = "BoxCox", value = "BoxCox", tooltip = "Designed for non-negative responses. boxcox transforms nonnormally distributed data to a set of data that has approximately normal distribution. The Box-Cox transformation is a family of power transformations."),
  list(label = "YeoJohnson", value = "YeoJohnson", tooltip = "Similar to the Box-Cox model but can accommodate predictors with zero and/or negative values "),
  list(label = "expoTrans", value = "expoTrans", tooltip = "Exponential transformation")

)
mytips<-paste0(do.call(paste0,args=list("'",lapply(transf_df,function(x) x$tooltip), sep="'")),collapse=",")



source("www/funs_ordination_plot.R")
source('www/funs_texts.R')
source("www/funs_pre_treat.R")
source("www/funs_sominter.R")
source("www/funs_somplot.R")
source("www/funs_mapplot.R")
source("www/funs_rf_inter.R")
source("www/funs_ensemble.R")
source("www/funs_interp.R")
source("www/mytrain_som.R")
source("www/funs_spd.R")
source("www/funs_som2.R")
source("www/funs_task_mananger.R")
source("www/funs_desc.R")
source("www/funs_toolbar.R")
source("www/funs_mananger.R")
source("www/funs_SL.R")
#source("www/funs_keras.R")
#file.remove(paste0(getwd(), "/", 'bookmarks.rds'))
js<-'$(document).keyup(function(event) {
    if (event.keyCode == 13) {
        $("#data_confirm").click()'

source("www/fun_gg_rst.R")
source("www/funs_leaflet.R")

convertMenuItem<-function(mi, tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if (length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
    mi$attribs$class = NULL
  }
  mi
}


unsup_icon<-base64enc::dataURI(file = "www/unsup_icon.png", mime = "image/png")
sup_icon<-base64enc::dataURI(file = "www/sup_icon.png", mime = "image/png")
sup_icon2<-base64enc::dataURI(file = "www/sup_icon2.png", mime = "image/png")
imesc_icon<-base64enc::dataURI(file = "www/imesc_logo.png", mime = "image/png")
b64<-base64enc::dataURI(file = "www/logo.png", mime = "image/png")
split_icon<-base64enc::dataURI(file = "www/split_icon2.png", mime = "image/png")
split_icon_white<-base64enc::dataURI(file = "www/split_icon3.png", mime = "image/png")
smw_icon<-base64enc::dataURI(file = "www/smw_icon.png", mime = "image/png")

pw_icon<-base64enc::dataURI(file = "www/pwrda_icon.png", mime = "image/png")
na_icon<-base64enc::dataURI(file = "www/na_icon2.png", mime = "image/png")

tutor_icon<-base64enc::dataURI(file = "www/tutor_icon.png", mime = "image/png")

nb_icon<-base64enc::dataURI(file = "www/nb_icon.png", mime = "image/png")
nb_icon2<-base64enc::dataURI(file = "www/nb_icon2.png", mime = "image/png")

svm_icon<-base64enc::dataURI(file = "www/smv_icon.png", mime = "image/png")
svm_icon2<-base64enc::dataURI(file = "www/smv_icon2.png", mime = "image/png")

agg_icon2<-base64enc::dataURI(file = "www/agg_icon2.png", mime = "image/png")

knn_icon<-base64enc::dataURI(file = "www/knn_icon.png", mime = "image/png")
knn_icon2<-base64enc::dataURI(file = "www/knn_icon2.png", mime = "image/png")

comp_icon<-base64enc::dataURI(file = "www/comp_icon.png", mime = "image/png")
comp_icon2<-base64enc::dataURI(file = "www/comp_icon2.png", mime = "image/png")
sim_icon<-base64enc::dataURI(file = "www/sim_icon.png", mime = "image/png")

compare_icon<-base64enc::dataURI(file = "www/compare_icon.png", mime = "image/png")
ensemble_icon<-base64enc::dataURI(file = "www/ensemble_icon.png", mime = "image/png")
sgboost_icon<-base64enc::dataURI(file = "www/sgboost_icon.png", mime = "image/png")
sgboost_icon2<-base64enc::dataURI(file = "www/sgboost_icon2.png", mime = "image/png")


kmeans_icon<-base64enc::dataURI(file = "www/kmeans_icon.png", mime = "image/png")
kmeans_icon2<-base64enc::dataURI(file = "www/kmeans_icon2.png", mime = "image/png")

div_icon<-base64enc::dataURI(file = "www/div_icon.png", mime = "image/png")


icon_360_hor<-base64enc::dataURI(file = "www/icon_360_hor.png", mime = "image/png")


icon_360_ver<-base64enc::dataURI(file = "www/icon_360_ver.png", mime = "image/png")

rf_icon<-base64enc::dataURI(file = "www/rf_icon.png", mime = "image/png")

rf_icon2<-base64enc::dataURI(file = "www/rf_icon2.png", mime = "image/png")

xyf_icon<-base64enc::dataURI(file = "www/xyf_icon.png", mime = "image/png")



js_getid<-paste0("$(document).on('click', '.needed', function () {debugger;
           Shiny.onInputChange('", 'last_btn', "', this.id);
        });")





js_code_add_unsup<-'
   $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu a").removeClass("teste_go");
    });

    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(6) > a").addClass("teste_go");
    });
    '
js_code_add_sup<-'
   $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu a").removeClass("teste_go");
    });
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(7) > a").addClass("teste_go");
    });
    '

js_code_add_ens<-'
   $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu a").removeClass("teste_go");
    });
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(8) > a").addClass("teste_go");
    });
    '
js_code_remove<-'
     $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu a").removeClass("teste_go");
    });
    '

js_code_remove_all<-'
     $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(6) > a").removeClass("teste_go");
    });
   $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(7) > a").removeClass("teste_go");
    });
   $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(8) > a").removeClass("teste_go");
    });
    '


funs_dynamic_class<-list(
  'add_unsup'=js_code_add_unsup,
  'add_sup'=js_code_add_sup,
  'add_cc'=js_code_add_ens,
  "remove"=js_code_remove
)

is_ccmenu<-function(x){
  x%in%c("menu_compare","menu_ensemble")
}
is_unsupmenu<-function(x){
  x%in%c("menu_som","menu_hc",'menu_kmeans')
}
is_supmenu<-function(x){
  x%in%c("menu_rf","menu_nb",'menu_svm','menu_knn','menu_sgboost','menu_som2')
}
is_toolmenu<-function(x){
  x%in%c('menu_intro','menu_upload','menu_explore','menu_maps','menu_div')
}
which_menu<-function(x){
  req(x)
  if(is_toolmenu(x)){return("tool_menu")} else if(is_unsupmenu(x)){'unsup_expand'}else
    if(is_supmenu(x)){'sup_expand'}else  if(is_ccmenu(x)){'cc_expand'} else{"none"}
}
menu_is_in_expand<-function(menu, expand) {
  req(!is.null(expand))
  return(which_menu(menu) == expand)
}

current_expand_tab<-function(curr_menus,expanded,menu){

  res<-switch(expanded,
              "unsup_expand"=curr_menus$cur_unsup_menu,
              'sup_expand'=curr_menus$cur_sup_menu,
              "cc_expand"=curr_menus$cur_cc_menu
  )


  res
}


symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
df_symbol<-data.frame(
  val = c(16,15,17,18,8,1,5,3)
)
for(i in 1:length(symbols))
{
  symbol1<-base64enc::dataURI(file = paste0('www/pch',i,".png"), mime = "image/png")
  df_symbol$img[i]<- sprintf(paste0(img(src = symbol1, width = '10')))}


ns_dl_box<-NS("dlm_box")
ns_dl_name<-NS("dlm_name")
ns_dl_trash<-NS("dlm_trash")
ns_dl_out<-NS("dlm_out")
ns_attr_name<-NS("dlm_attr_name")
ns_attr_trash<-NS("dlm_attr_trash")
ns_attr_out<-NS("dlm_attr_out")
ns_model_name<-NS("dlm_model_name")
ns_model_out<-NS("dlm_model_out")
ns_model_trash<-NS("dlm_model_trash")
ns_feature_trash<-NS("dlm_feature_trash")
ns_attr_down<-NS("dlm_attr_down")
ns_feature_down<-NS("dlm_feat_down")
