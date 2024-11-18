
Copy_new_files_imesc<-function(path_dest="D:/R3/imesc2024/imesc_beta2/"){
  folders=c("inst","www")
  j<-folders[1]
  for(j in folders){
    o_inst<-list.files(j,full.names = T,recursive = T)

    d_inst<-paste0(path_dest,o_inst)
    #file.remove(d_inst[41])


    new_files<-which(sapply(seq_along(o_inst),function(i){

      res<-tools::md5sum(o_inst[i]) != tools::md5sum(d_inst[i])
      if(is.na(as.numeric(res))){
        TRUE
      } else{
        res
      }
    }))


    if(!length(new_files)>0) {return(print(paste("folder",j,":","is already updated")))} else{
      file.copy(o_inst[new_files],d_inst[new_files],overwrite = T)
      print(paste("folder",j,":",length(new_files),"files copied"))
    }

  }
}


Copy_new_files_imesc()

