# Published savepoints of older iMESc versions: their Datalists and models must still open.
# Folder: IMESC_SAVEPOINTS (environment variable) or ../imesc_savepoints-main next to the
# repository; the file is skipped when the folder is not found.
root<-Sys.getenv("IMESC_SAVEPOINTS",file.path("..","imesc_savepoints-main"))
files<-if(dir.exists(root)) list.files(root,pattern="[.]rds$",recursive=TRUE,full.names=TRUE) else character(0)
if(!length(files)){
  cat("  (skipped: savepoints folder not found:",root,")\n")
} else for(f in files){
  label<-basename(dirname(f))
  label<-paste0(substr(label,1,30),"/",basename(f))
  sp<-tryCatch(readRDS(f),error=function(e) e)
  if(!check(paste(label,": file read"),!inherits(sp,"error"))) next
  sd<-sp$saved_data
  # the same conversion as the app when a savepoint is loaded (module00, load_savepoint_yes)
  for(i in names(sd)) if(length(attr(sd[[i]],"rf"))) for(j in seq_along(attr(sd[[i]],"rf"))){
    x<-attr(sd[[i]],"rf")[[j]]
    if(!inherits(x,"list")) attr(sd[[i]],"rf")[[j]]<-list(x)
  }
  vals<-list(saved_data=sd)
  n_models<-0; bad<-character(0)
  for(d in names(sd)){
    dl<-sd[[d]]
    for(tp in imesc_models){
      old<-names(attr(dl,tp))
      if(!length(old)) next
      if(!identical(imesc_model_names(dl,tp,saved=FALSE),old)) bad<-c(bad,paste0(d,"/",tp,": names"))
      for(nm in old){
        n_models<-n_models+1
        m<-tryCatch(imesc_model_get(dl,tp,nm,unwrap=TRUE),error=function(e) e)
        if(inherits(m,"error")) { bad<-c(bad,paste0(d,"/",tp,"/",nm,": read")); next }
        info<-imesc_model_info(tp)
        if(isTRUE(info$wrapped)&&identical(class(m),"list")) bad<-c(bad,paste0(d,"/",tp,"/",nm,": not unwrapped"))
        if(identical(tp,"som")&&!identical(nm,info$unsaved)&&!inherits(m,"kohonen")&&length(m)) bad<-c(bad,paste0(d,"/",tp,"/",nm,": not a SOM"))
        g<-tryCatch(get_attr_imesc(d,tp,nm,vals=vals),error=function(e) e)
        if(inherits(g,"error")) bad<-c(bad,paste0(d,"/",tp,"/",nm,": get_attr_imesc ",conditionMessage(g)))
      }
      # renaming every model and back gives the same Datalist
      ren<-tryCatch(imesc_model_rename(imesc_model_rename(dl,tp,paste0(old,"__tmp")),tp,old),error=function(e) e)
      if(inherits(ren,"error")||!identical(ren,dl)) bad<-c(bad,paste0(d,"/",tp,": rename round trip"))
    }
    ov<-tryCatch(datalist_overview(dl,available_models=SL_models$models),error=function(e) e)
    if(inherits(ov,"error")) bad<-c(bad,paste0(d,": overview ",conditionMessage(ov)))
  }
  tr<-tryCatch(getTree_saved_data(vals,FALSE,imesc_attrs=imesc_attrs,imesc_models=imesc_models),error=function(e) e)
  if(inherits(tr,"error")) bad<-c(bad,paste0("Datalist manager tree: ",conditionMessage(tr)))
  check(paste0(label,": ",length(sd)," Datalists, ",n_models," models open",if(length(bad)) paste0(" | problems: ",paste(utils::head(bad,4),collapse="; ")) else ""),!length(bad))
}

# the SOMs and the (legacy) HC models open in their modules
for(f in files){
  sd<-readRDS(f)$saved_data
  label<-paste0(substr(basename(dirname(f)),1,30),"/",basename(f))
  with_som<-names(sd)[vapply(sd,function(d) any(vapply(attr(d,"som"),function(m) inherits(m,"kohonen"),logical(1))),logical(1))]
  with_hc<-names(sd)[vapply(sd,function(d) length(attr(d,"hc"))>0,logical(1))]
  if(length(with_som)){
    vals<-shiny::reactiveValues(saved_data=sd,cur_data=with_som[1],newcolhabs=list(turbo=viridis::turbo))
    ok<-TRUE; msg<-character(0)
    shiny::testServer(imesc_supersom$server,args=list(vals=vals),{
      for(d in with_som) for(nm in names(attr(sd[[d]],"som"))){
        if(!inherits(attr(sd[[d]],"som")[[nm]],"kohonen")) next
        session$setInputs(data_som=d,som_models=nm)
        r<-tryCatch(current_som_model(),error=function(e) e)
        if(!inherits(r,"kohonen")){ ok<<-FALSE; msg<<-c(msg,paste0(d,"/",nm)) }
      }
    })
    check(paste0(label,": SOMs open in the SOM module",if(length(msg)) paste0(" (failed: ",paste(msg,collapse=", "),")") else ""),ok)
  }
  if(length(with_hc)){
    vals<-shiny::reactiveValues(saved_data=sd,cur_data=with_hc[1],newcolhabs=list(turbo=viridis::turbo))
    res<-character(0); n_hc<-0
    shiny::testServer(hc_module$server,args=list(vals=vals),{
      for(d in with_hc){
        session$setInputs(data_hc=d,model_or_data="data")
        r<-tryCatch(hc_model_names_saved(),error=function(e) e)
        if(inherits(r,"error")) res<<-c(res,paste0(d,": ",conditionMessage(r)))
        if(!inherits(r,"error")) n_hc<<-n_hc+length(r)
      }
    })
    check(paste0(label,": HC models migrated and listed in the HC module (",n_hc,")",if(length(res)) paste0(" (",paste(res,collapse="; "),")") else ""),!length(res))
  }
}

# HC models of older versions stored on the SOMs: migrated and listed for the SOM codebook
for(f in files){
  sd<-readRDS(f)$saved_data
  label<-paste0(substr(basename(dirname(f)),1,30),"/",basename(f))
  pairs<-do.call(rbind,lapply(names(sd),function(d) do.call(rbind,lapply(names(attr(sd[[d]],"som")),function(s){
    m<-attr(sd[[d]],"som")[[s]]
    if(inherits(m,"kohonen")&&(!is.null(attr(m,"hc.object"))||length(attr(m,"hc")))) data.frame(d=d,s=s,stringsAsFactors=FALSE)
  }))))
  if(is.null(pairs)) next
  vals<-shiny::reactiveValues(saved_data=sd,cur_data=pairs$d[1],newcolhabs=list(turbo=viridis::turbo))
  res<-character(0)
  shiny::testServer(hc_module$server,args=list(vals=vals),{
    for(i in seq_len(nrow(pairs))){
      session$setInputs(data_hc=pairs$d[i],model_or_data="som codebook",som_model_name=pairs$s[i])
      r<-tryCatch(hc_model_names_saved(),error=function(e) e)
      if(inherits(r,"error")||!length(r)) res<<-c(res,paste0(pairs$d[i],"/",pairs$s[i]))
    }
  })
  check(paste0(label,": HC models of the SOMs (",nrow(pairs),") listed in the HC module",if(length(res)) paste0(" (missing: ",paste(res,collapse=", "),")") else ""),!length(res))
}
