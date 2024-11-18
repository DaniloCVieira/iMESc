all_packages_from_functions<-function(files_funs){
  library(NCmisc)

  all_packages<-lapply(files_funs, function (file){
    message(paste0(which(files_funs==file),"/",length(files_funs),"-",file))

    funs<-list.functions.in.file(file)

    fnames<-gsub("package:","",names(funs))
    fnames<-gsub("\\)","",gsub("c\\(","",gsub('"',"",fnames)))

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

  })

  packages_www<-all_packages
  packages_www<-packages_www[!sapply(sapply(packages_www,colnames),is.null)]


  packages_www_df<-do.call(rbind,packages_www)

  pack_df<-packages_www_df
  rownames(pack_df)<-NULL

  pack_df
}
dps<-renv::dependencies("inst")
packs<-unique(dps$Package)
sapply(packs,function(x){
  library(x,  character.only =T)
})


files_R<-list.files('inst/R',pattern = ".R",full.names = T,recursive = F)
files_www<-list.files('inst/www/',pattern = ".R",full.names = T,recursive = F)[-c(1:2)]
files_funs<-c(files_R,files_www)
all_result<-all_packages_from_functions(files_funs)
#all_result<-pack_df

saveRDS(all_result,"allfuns.rds")


packs1<-gsub(" ","",unique(all_result$package))
packs1<-unique(packs1)
pic1<-grep("character",packs1)
pic2<-grep(".GlobalEnv",packs1)
pic3<-which(packs1=="base")
pic4<-grep("grDev",packs1)
pic5<-grep("imesc",packs1)

pack_final<-packs1[-c(pic1,pic2,pic3,pic4, pic5)]

final_packs<-all_result[all_result$package%in%pack_final,]
fs<-split(final_packs,final_packs$package)
unique(final_packs$file)
x<-fs[[2]]
fsd<-lapply(fs,function(x){
  data.frame(package=x$package[1],version=packageVersion(x$package[1]),functions=paste0(unique(x$fun),collapse=", "))

})
final_packs[final_packs$package=="MLmetrics",]

fsdr<-do.call(rbind,fsd)
kernlab::ksvm()
write.table(fsdr,'fsdr.csv',sep=";",col.names=NA)








globals<-all_result[all_result$package==".GlobalEnv",]
once_funs<-table(globals$fun)[table(globals$fun)==1]
once_functions<-all_result[all_result$fun%in%names(once_funs),]
once_functions$fun[4]

which(all_result$fun=="check_model0")
check_model0
glo<-globals[order(globals$fun),]



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
names(res)<-unique_funs

res<-res[order(sapply(res,function(x) sum(x[[2]])))]

one_call<-res[sapply(res,function(x) sum(x[[2]])==1)]
grepl("module",names(one_call))
