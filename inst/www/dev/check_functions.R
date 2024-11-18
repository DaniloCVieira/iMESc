#package_version(mice)
#packageVersion('leaflet')

#dps<-renv::dependencies()


fun_files<-list.files("inst/www",pattern=".R", full.names = T)
fun_files<-c(fun_files,list.files("inst/R",pattern=".R", full.names = T))


check_function<-function(fun){
  pic_which<-unlist(lapply(fun_files,function(x){
    fiile<-readLines(x)
    sum(unlist(lapply(fiile,function(xx) grepl(fun,xx))))
  }))

  pic<-which(unlist(lapply(fun_files,function(x){
    fiile<-readLines(x)
    any(unlist(lapply(fiile,function(xx) grepl(fun,xx))))
  }))
  )
  list(
    file=fun_files[pic],
    n_calls=pic_which[pic]
  )

}
unique_funs<-unique(all_result$fun)
Get_pack_from_fun <- function(fun_name) {
  # Perform a help search for the function name
  search_result <- help.search(fun_name)

  # Extract the package names from the search result
  packages <- unique(search_result$matches[, "Package"])

  return(packages)
}
res<-lapply(seq_along(unique_funs)
  ,function(i){

    fun<-unique_funs[[i]]
    message(paste0(fun,"-",i,"/",length(unique_funs)))
    res<-check_function(fun)
    packs<-Get_pack_from_fun(fun)
    res$packs<-packs
    print(sum(res[[2]]))
    res
  })
Get_pack_from_fun("rsr")


# Example usage


'inst/R/module00_preprocess.R'




