

## 01 backup
path_sys="D:/R3/imesc/"
folder_from='beta'
folder_to='imesc_beta_backup/19'

### Remove all files from the target folder
app_folders<-c("doc", "inst", "man", "Meta", "R")
folder="inst"


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

files_app<-c(paste0(path_sys,folder_from,"/DESCRIPTION"),
             paste0(path_sys,folder_from,"/Dockerfile"),
             paste0(path_sys,folder_from,"/NAMESPACE"),
             paste0(path_sys,folder_from,"/packages.bib"),
             paste0(path_sys,folder_from,"/README.md"),
             paste0(path_sys,folder_from,"/.Rprofile"),
             paste0(path_sys,folder_from,"/app.R"),
             paste0(path_sys,folder_from,"/CITATION.cff"))

for(i in 1:length(files_app)){
  to<-gsub(folder_from,folder_to,files_app[[i]])
  file.copy(files_app[[i]], to)
}

