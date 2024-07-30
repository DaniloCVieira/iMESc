
base_packages <- unique(c("base", "datasets", "graphics", "grDevices", "methods", "compiler","graphics","tools","rstudioapi","utils","grDevices",  "stats","datasets","NCmisc","methods","renv","base","stats", "utils", "grid", "parallel", "compiler", "splines","tools", "tcltk", "stats4",'Rcpp','plyr',"R6"))
is_base_package <- function(pkg) {
  pkg %in% unique(c("base", "datasets", "graphics", "grDevices", "methods", "compiler","graphics","tools","rstudioapi","utils","grDevices",  "stats","datasets","NCmisc","methods","renv","base","stats", "utils", "grid", "parallel", "compiler", "splines","tools", "tcltk", "stats4",'Rcpp','plyr',"R6"))
}

unload_all_packages <- function() {
  # Function to check if a package is base


  # Get the list of all currently loaded packages
  loaded_packages <- loadedNamespaces()

  # Identify non-base packages
  non_base_packages <- setdiff(loaded_packages, base_packages)

  # Helper function to unload a package
  unload_package <- function(pkg) {
    tryCatch({
      detach(paste0("package:", pkg), character.only = TRUE, unload = TRUE, force = TRUE)
    }, error = function(e) {
      message(paste("Could not unload package:", pkg))
    })
  }

  # Unload non-base packages in reverse order of dependency
  while (length(non_base_packages) > 0) {
    for (pkg in non_base_packages) {
      tryCatch({
        unloadNamespace(pkg)
        non_base_packages <- setdiff(non_base_packages, pkg)

      }, error = function(e) {

      })
    }
  }

  print_res<-loadedNamespaces()[!loadedNamespaces()%in%base_packages]
  if(length(print_res)>0){
    message("Remaining loaded packages:")
    print(print_res)
  } else{print("All unload")}
}


dps<-renv::dependencies("inst")
files<-dps[grepl("\\.R",dps$Source),]

files<-files[!files$Package%in%base_packages,]
files_funs<-files

list_fun_from_file<-function(file,pkg) {
  funs<-NCmisc::list.functions.in.file(file)


  fnames<-gsub("package:","",names(funs))
  fnames<-gsub("\\)","",gsub("c\\(","",gsub('"',"",fnames)))
  pic<-which(fnames==pkg)
  fnames<-fnames[pic]
  funs<-funs[pic]
  resdf<-lapply(seq_along(fnames),function(l){
    funs_name<-funs[[l]]
    res2<-data.frame(fun=funs_name)
    packs<-unlist(strsplit(fnames[l],","))
    do.call(rbind, lapply(packs,function(pac){
      res2$package<-pac
      res2
    }))
  })
  if(length(resdf)>0){
    resdf<-do.call(rbind,resdf)
    resdf$file<-file
    resdf
  }
}

all_packages_from_functions<-function(files_funs){


 pacotes<-unique(files_funs$Package)
  pack_results<-lapply(seq_along(pacotes),function(i) {
    pkg<- pacotes [i]


    to_read<-files_funs$Source[  files_funs$Package%in%pkg]

    try({
      print(paste0("Loading...",pkg))
      library(pkg,character.only = T)
    })
    to_read<-unique(to_read)

    result<-lapply(seq_along(to_read),function(x) {
      file<-to_read[x]
      cat("\f")
      message(paste0(i,"/",length(pacotes),"-",pkg))
      cat(paste0(x,"/",length(to_read)))
      list_fun_from_file(file,pkg)
    })
    try(unloadNamespace(pkg))

    result

  })
  pack_results
}
pack_results<-all_packages_from_functions(files_funs)

pack_results2<-do.call(rbind,unlist(pack_results,recursive=F))
pack_results2$file<-as.character(pack_results2$file)
pack_list<-split(pack_results2,pack_results2$package)
#pack_list<-do.call(rbind,pack_list)
#rownames(pack_list)<-NULL
x<-pack_list[[3]]

rspac<-lapply(pack_list,function(x){
  lapply(split(x,x$file),function(xx){})
  data.frame(Package=x$package[1],Functions=paste0(x$fun,collapse=", "),source=paste0(gsub(".R","",gsub("D:/R3/imesc2024/app_new/inst/R/|D:/R3/imesc2024/app_new/inst/www/","",unique(x$file))),collapse=", "))
})

rspac<-do.call(rbind,rspac)
rspac$version<-sapply(rspac$Package,function(x) as.character(utils::packageVersion(x)))
write.table(rspac,"rspac.csv",col.names=NA,sep=";")

newtable<-read.csv("rspac - Copia.csv",sep=";")
x<-split(newtable,newtable$Package)[[5]]
table_with_tasks<-do.call(rbind,lapply(split(newtable,newtable$Package),function(x){
  x$Functions<-paste0(unique(strsplit(x$Functions,", ")[[1]]),collapse=", ")
  x
}))
table_app<-rspac[!rspac$Package%in%table_with_tasks$Package,]

table_app<-do.call(rbind,lapply(split(table_app,table_app$Package),function(x){
  x$Functions<-paste0(unique(strsplit(x$Functions,", ")[[1]]),collapse=", ")
  x
}))

write.table(table_app,"table_app.csv",col.names=NA,sep=";")
write.table(table_with_tasks,"table_with_tasks.csv",col.names=NA,sep=";")
