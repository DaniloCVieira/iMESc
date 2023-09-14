
path_sys="D:/R3/imesc/"
folder_from='imesc3'
folder_to='beta'
library(fs)
### Remove all files from the target folder
app_folders<-c("doc", "inst", "man", "Meta", "R")
folder="inst"
#file.remove(list.files(paste0(path_sys,folder_to), recursive = T, full.names = T))
for (folder in app_folders){
  folder_tag<-paste0("/",folder)
  source_Directory<-paste0(path_sys,folder_from,folder_tag)
  list_Dirs <- list.dirs(source_Directory)
  tocreate<-gsub(folder_from,folder_to,list_Dirs)
  for( j in 1:length(tocreate)){

    todel<-tocreate[[j]]
    if(isTRUE(dir.exists(todel))){
      dir_delete(todel)
    }
  }
  }


for (folder in app_folders){

  folder_tag<-paste0("/",folder)
  source_Directory<-paste0(path_sys,folder_from,folder_tag)
  target_Directory<-paste0(path_sys,folder_to,folder_tag)




  list_Dirs <- list.dirs(source_Directory)
  tocreate<-gsub(folder_from,folder_to,list_Dirs)

  for (dir in tocreate) {
    dir.create(dir)}

  list_Files <- list.files(source_Directory, recursive = TRUE, full.names = TRUE)
  destination_files<-gsub(folder_from,folder_to,list_Files)
  for (i in 1:length(list_Files)) {
    file<-list_Files[i]
    destination_file <- destination_files[[i]]
    file.copy(file, destination_file, overwrite = TRUE)
  }

}

files_app<-c(
  "D:/R3/imesc/imesc3/LICENSE",
  "D:/R3/imesc/imesc3/DESCRIPTION",
  "D:/R3/imesc/imesc3/Dockerfile",
  "D:/R3/imesc/imesc3/NAMESPACE",
  "D:/R3/imesc/imesc3/packages.bib",
  "D:/R3/imesc/imesc3/README.md",
  "D:/R3/imesc/imesc3/.Rprofile",
  "D:/R3/imesc/imesc3/app.R",
  "D:/R3/imesc/imesc3/CITATION.cff"
)

for(i in 1:length(files_app)){
  to<-gsub("imesc3","beta",files_app[[i]])
  file.copy(files_app[[i]], to)
}



