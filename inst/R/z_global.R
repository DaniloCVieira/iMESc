#' @import kohonen sf ggplot2 data.table shinyBS
#' @import viridis colorRamps
#' @importFrom shinyWidgets updatePickerInput pickerInput radioGroupButtons updateRadioGroupButtons dropdownButton tooltipOptions toggleDropdownButton switchInput
#' @importFrom shinyjs hide show hidden addClass removeClass toggle runjs toggleState extendShinyjs useShinyjs delay onclick
#' @importFrom raster crs mask rasterToPoints coordinates extent ratify raster rasterize `crs<-` values `values<-` `extent<-`


inicio<-Sys.time()
if(dir.exists("inst")){
  root='inst'
} else{
  root=""
}




version<-"1.0.4"
SL_models<-readRDS("inst/www/SL_models.rds")
sl_tips<-readRDS("inst/www/sl_tips.rds")
sl_formals<-readRDS("inst/www/sl_formals.rds")
sl_ctl_formals<-readRDS("inst/www/sl_ctl_formals.rds")
last_update<-readRDS('inst/www/last_update.rds')
#sl_tips<-sl_tips2

model_fits=SL_models$model_fits
model_grid=SL_models$model_grid
model_labels=SL_models$model_labels
model_labels3=SL_models$model_labels3
model_library=SL_models$model_library
model_parameters=SL_models$model_parameters
model_tags=SL_models$model_tags
model_varImp=SL_models$model_varImp
models=SL_models$models
train_functions=SL_models$train_functions
train_functions2=SL_models$train_functions2
available_models<-SL_models$models

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
if(getwd()=="C:/R4/iMESc-Dev/imesc"){
  print("saving last update...")
  last_update<-Sys.Date()
  saveRDS(last_update,"inst/www/last_update.rds")
  qsave=T


  # rstudio<- rstudioapi::versionInfo()

}


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


source("inst/www/check_version_desktop.R")
source("inst/www/funs_ordination_plot.R")
source('inst/www/funs_texts.R')
source("inst/www/funs_pre_treat.R")
source("inst/www/funs_sominter.R")
source("inst/www/funs_somplot.R")
source("inst/www/funs_mapplot.R")
source("inst/www/funs_rf_inter.R")
source("inst/www/funs_ensemble.R")
source("inst/www/funs_interp.R")
source("inst/www/mytrain_som.R")
source("inst/www/funs_spd.R")
source("inst/www/funs_som2.R")
source("inst/www/funs_task_mananger.R")
source("inst/www/funs_desc.R")
source("inst/www/funs_toolbar.R")
source("inst/www/funs_mananger.R")
source("inst/www/funs_SL.R")
source("inst/www/fun_gg_rst.R")
source("inst/www/funs_leaflet.R")
source("inst/www/sankey.R")

#source("inst/www/funs_keras.R")
#file.remove(paste0(getwd(), "/", 'bookmarks.rds'))
js<-'$(document).keyup(function(event) {
    if (event.keyCode == 13) {
        $("#data_confirm").click()'


unsup_icon<-base64enc::dataURI(file = "inst/www/unsup_icon.png", mime = "image/png")
sup_icon<-base64enc::dataURI(file = "inst/www/sup_icon.png", mime = "image/png")
sup_icon2<-base64enc::dataURI(file = "inst/www/sup_icon2.png", mime = "image/png")
imesc_name<-base64enc::dataURI(file = "inst/www/imesc_name.png", mime = "image/png")
#imesc_name<-base64enc::dataURI(file = "inst/www/imesc_name.png", mime = "image/png")


b64<-base64enc::dataURI(file = "inst/www/logo.png", mime = "image/png")
split_icon<-base64enc::dataURI(file = "inst/www/split_icon2.png", mime = "image/png")
split_icon_white<-base64enc::dataURI(file = "inst/www/split_icon3.png", mime = "image/png")
smw_icon<-base64enc::dataURI(file = "inst/www/smw_icon.png", mime = "image/png")

pw_icon<-base64enc::dataURI(file = "inst/www/pwrda_icon.png", mime = "image/png")
na_icon<-base64enc::dataURI(file = "inst/www/na_icon2.png", mime = "image/png")

tutor_icon<-base64enc::dataURI(file = "inst/www/tutor_icon.png", mime = "image/png")

nb_icon<-base64enc::dataURI(file = "inst/www/nb_icon.png", mime = "image/png")
nb_icon2<-base64enc::dataURI(file = "inst/www/nb_icon2.png", mime = "image/png")

svm_icon<-base64enc::dataURI(file = "inst/www/smv_icon.png", mime = "image/png")
svm_icon2<-base64enc::dataURI(file = "inst/www/smv_icon2.png", mime = "image/png")

agg_icon2<-base64enc::dataURI(file = "inst/www/agg_icon2.png", mime = "image/png")

knn_icon<-base64enc::dataURI(file = "inst/www/knn_icon.png", mime = "image/png")
knn_icon2<-base64enc::dataURI(file = "inst/www/knn_icon2.png", mime = "image/png")

comp_icon<-base64enc::dataURI(file = "inst/www/comp_icon.png", mime = "image/png")
comp_icon2<-base64enc::dataURI(file = "inst/www/comp_icon2.png", mime = "image/png")
sim_icon<-base64enc::dataURI(file = "inst/www/sim_icon.png", mime = "image/png")

compare_icon<-base64enc::dataURI(file = "inst/www/compare_icon.png", mime = "image/png")
ensemble_icon<-base64enc::dataURI(file = "inst/www/ensemble_icon.png", mime = "image/png")
sgboost_icon<-base64enc::dataURI(file = "inst/www/sgboost_icon.png", mime = "image/png")
sgboost_icon2<-base64enc::dataURI(file = "inst/www/sgboost_icon2.png", mime = "image/png")


kmeans_icon<-base64enc::dataURI(file = "inst/www/kmeans_icon.png", mime = "image/png")
kmeans_icon2<-base64enc::dataURI(file = "inst/www/kmeans_icon2.png", mime = "image/png")

div_icon<-base64enc::dataURI(file = "inst/www/div_icon.png", mime = "image/png")


icon_360_hor<-base64enc::dataURI(file = "inst/www/icon_360_hor.png", mime = "image/png")


icon_360_ver<-base64enc::dataURI(file = "inst/www/icon_360_ver.png", mime = "image/png")

rf_icon<-base64enc::dataURI(file = "inst/www/rf_icon.png", mime = "image/png")

rf_icon2<-base64enc::dataURI(file = "inst/www/rf_icon2.png", mime = "image/png")

xyf_icon<-base64enc::dataURI(file = "inst/www/xyf_icon.png", mime = "image/png")



js_getid<-paste0("$(document).on('click', '.needed', function () {debugger;
           Shiny.onInputChange('", 'last_btn', "', this.id);
        });")


symbols<-c("pch1","pch2","pch3","pch4",'pch5','pch6','pch7',"pch8")
df_symbol<-data.frame(
  val = c(16,15,17,18,8,1,5,3)
)
for(i in 1:length(symbols))
{
  symbol1<-base64enc::dataURI(file = paste0('inst/www/pch',i,".png"), mime = "image/png")
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



