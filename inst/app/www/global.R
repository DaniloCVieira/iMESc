
rm(list=ls())
inicio <- Sys.time()

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
if(getwd()=="D:/R3/imesc/imesc3"){
  last_update<-Sys.Date()
  saveRDS(last_update,"inst/app/www/last_update.rds")
  qsave=T


 # rstudio<- rstudioapi::versionInfo()

}

last_update<-readRDS('inst/app/www/last_update.rds')
#last_update<-format(Sys.Date(),"%d-%m-%Y")

TESTE<- reactive({
  mybooks_teste<-readRDS('vals.rds')
  for (var in names(mybooks_teste)) {    vals[[var]] <- mybooks_teste[[var]]  }
  updateTextInput(session, "tabs", value = mybooks_teste$cur_tab)

})







#deps <- tools::package_dependencies(packages = list.of.packages,recursive = TRUE)
#which(unlist(lapply(deps,function(x){ 'rgdal'%in%x})))

transf_df<-list(

  list(label="None", value="None", tooltip="No transformation"),
  list(label = "log2", value = "log2", tooltip = "logarithmic base 2 transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of the logarithm. Zeros are left as zeros. Higher bases give less weight to quantities and more to presences, and logbase = Inf gives the presence/absence scaling. Please note this is not log(x+1). Anderson et al. (2006) suggested this for their (strongly) modified Gower distance, but the standardization can be used independently of distance indices."),
  list(label = "log10", value = "log10", tooltip = " logarithmic base 10 transformation as suggested by Anderson et al. (2006): log_b (x) + 1 for x > 0, where b is the base of the logarithm. Zeros are left as zeros. Higher bases give less weight to quantities and more to presences, and logbase = Inf gives the presence/absence scaling. Please note this is not log(x+1). Anderson et al. (2006) suggested this for their (strongly) modified Gower distance, but the standardization can be used independently of distance indices."),
  list(label = "total", value = "total", tooltip = "divide by the line (observation) total"),
  list(label = "max", value = "max", tooltip = "divide by column (variable) maximum"),
  list(label = "frequency", value = "frequency", tooltip = "divide by column (variable) total and multiply by the number of non-zero items, so that the average of non-zero entries is one"),
  list(label = "range", value = "range", tooltip = "standardize column (variable) values into range 0 ... 1. If all values are constant, they will be transformed to 0"),
  list(label = "pa", value = "pa", tooltip = "scale x to presence/absence scale (0/1)"),
  list(label = "chi.square", value = "chi.square", tooltip = "divide by row sums and square root of column sums, and adjust for square root of matrix total"),
  list(label = "hellinger", value = "hellinger", tooltip = "square root of method = total"),
  list(label = "sqrt2", value = "sqrt2", tooltip = "square root"),
  list(label = "sqrt4", value = "sqrt4", tooltip = "4th root"),
  list(label = "log2(x+1)", value = "log2(x+1)", tooltip = " logarithmic base 2 transformation (x+1)"),
  list(label = "log10(x+1)", value = "log10(x+1)", tooltip = "logarithmic base 10 transformation (x+1)"),
  list(label = "BoxCox", value = "BoxCox", tooltip = "Designed for non-negative responses. boxcox transforms nonnormally distributed data to a set of data that has approximately normal distribution. The Box-Cox transformation is a family of power transformations."),
  list(label = "YeoJohnson", value = "YeoJohnson", tooltip = "Similar to the Box-Cox model but can accommodate predictors with zero and/or negative values "),
  list(label = "expoTrans", value = "expoTrans", tooltip = "Exponential transformation")

)
mytips<-paste0(do.call(paste0,args=list("'",lapply(transf_df,function(x) x$tooltip), sep="'")),collapse=",")


# create help data frame





source("inst/app/www/funs_ordination_plot.R")
source('inst/app/www/funs_texts.R')
#source("inst/app/www/module_createcolors.R")

#source("inst/app/www/module_nb.R")

#source("inst/app/www/module_rf.R")
#source("inst/app/www/module_svm.R")
#source("inst/app/www/module_knn.R")
source("inst/app/www/funs_pre_treat.R")
source("inst/app/www/funs_sominter.R")
source("inst/app/www/funs_somplot.R")
source("inst/app/www/funs_mapplot.R")
source("inst/app/www/funs_rf_inter.R")
#source("inst/app/www/BC.R")
source("inst/app/www/funs_interp.R")
source("inst/app/www/funs_3D.R")
source("inst/app/www/funs_3D_2.R")
source("inst/app/www/mytrain_som.R")
source("inst/app/www/funs_spd.R")
source("inst/app/www/funs_som2.R")
source("inst/app/www/funs_task_mananger.R")
source("inst/app/www/funs_desc.R")
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


unsup_icon <- base64enc::dataURI(file = "inst/app/www/unsup_icon.png", mime = "image/png")
sup_icon <- base64enc::dataURI(file = "inst/app/www/sup_icon.png", mime = "image/png")
imesc_icon <- base64enc::dataURI(file = "inst/app/www/imesc_logo.png", mime = "image/png")
b64 <- base64enc::dataURI(file = "inst/app/www/logo.png", mime = "image/png")
split_icon <- base64enc::dataURI(file = "inst/app/www/split_icon2.png", mime = "image/png")
split_icon_white <- base64enc::dataURI(file = "inst/app/www/split_icon3.png", mime = "image/png")
smw_icon <- base64enc::dataURI(file = "inst/app/www/smw_icon.png", mime = "image/png")

pw_icon <- base64enc::dataURI(file = "inst/app/www/pwrda_icon.png", mime = "image/png")
na_icon <- base64enc::dataURI(file = "inst/app/www/na_icon2.png", mime = "image/png")

tutor_icon <- base64enc::dataURI(file = "inst/app/www/tutor_icon.png", mime = "image/png")

nb_icon <- base64enc::dataURI(file = "inst/app/www/nb_icon.png", mime = "image/png")
nb_icon2 <- base64enc::dataURI(file = "inst/app/www/nb_icon2.png", mime = "image/png")

svm_icon <- base64enc::dataURI(file = "inst/app/www/smv_icon.png", mime = "image/png")
svm_icon2 <- base64enc::dataURI(file = "inst/app/www/smv_icon2.png", mime = "image/png")

agg_icon2 <- base64enc::dataURI(file = "inst/app/www/agg_icon2.png", mime = "image/png")

knn_icon <- base64enc::dataURI(file = "inst/app/www/knn_icon.png", mime = "image/png")
knn_icon2 <- base64enc::dataURI(file = "inst/app/www/knn_icon2.png", mime = "image/png")

comp_icon <- base64enc::dataURI(file = "inst/app/www/comp_icon.png", mime = "image/png")
comp_icon2 <- base64enc::dataURI(file = "inst/app/www/comp_icon2.png", mime = "image/png")
sim_icon <- base64enc::dataURI(file = "inst/app/www/sim_icon.png", mime = "image/png")

compare_icon <- base64enc::dataURI(file = "inst/app/www/compare_icon.png", mime = "image/png")
ensemble_icon <- base64enc::dataURI(file = "inst/app/www/ensemble_icon.png", mime = "image/png")
sgboost_icon <- base64enc::dataURI(file = "inst/app/www/sgboost_icon.png", mime = "image/png")
sgboost_icon2 <- base64enc::dataURI(file = "inst/app/www/sgboost_icon2.png", mime = "image/png")


kmeans_icon <- base64enc::dataURI(file = "inst/app/www/kmeans_icon.png", mime = "image/png")
kmeans_icon2 <- base64enc::dataURI(file = "inst/app/www/kmeans_icon2.png", mime = "image/png")

div_icon <- base64enc::dataURI(file = "inst/app/www/div_icon.png", mime = "image/png")


icon_360_hor <- base64enc::dataURI(file = "inst/app/www/icon_360_hor.png", mime = "image/png")


icon_360_ver <- base64enc::dataURI(file = "inst/app/www/icon_360_ver.png", mime = "image/png")

rf_icon <- base64enc::dataURI(file = "inst/app/www/rf_icon.png", mime = "image/png")


xyf_icon <- base64enc::dataURI(file = "inst/app/www/xyf_icon.png", mime = "image/png")



js_getid<-paste0("$(document).on('click', '.needed', function () {debugger;
           Shiny.onInputChange('", 'last_btn', "', this.id);
        });")





js_code_add_unsup <- '
   $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu a").removeClass("teste_go");
    });

    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(6) > a").addClass("teste_go");
    });
    '
js_code_add_sup <- '
   $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu a").removeClass("teste_go");
    });
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(7) > a").addClass("teste_go");
    });
    '

js_code_add_ens <- '
   $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu a").removeClass("teste_go");
    });
    $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu > li:nth-child(8) > a").addClass("teste_go");
    });
    '
js_code_remove <- '
     $(document).ready(function() {
      $("#sidebar-main > div.sidebar-menu a").removeClass("teste_go");
    });
    '

js_code_remove_all <- '
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
menu_is_in_expand <- function(menu, expand) {
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
