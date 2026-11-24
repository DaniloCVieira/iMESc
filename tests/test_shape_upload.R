# Base/Layer shape in Create Datalist: one .rds (as before) or the files of a shapefile selected
# together; shapefiles are returned in longitude/latitude (WGS84)
suppressPackageStartupMessages(library(sf))
base<-readRDS("inst/www/base_shape_araca.rds")
dir<-tempfile("shp_test_")
dir.create(dir)

# what a fileInput gives: original names and temporary paths with other names
as_upload<-function(paths){
  up<-file.path(tempfile("upload_"),seq_along(paths))
  dir.create(dirname(up[1]))
  up<-paste0(up,".",tools::file_ext(paths))
  file.copy(paths,up)
  data.frame(name=basename(paths),datapath=up,stringsAsFactors=FALSE)
}
shp_files<-function(stem) list.files(dir,pattern=paste0("^",stem,"[.]"),full.names=TRUE)

# .rds, as before (fileInput data.frame and the old character path)
rds<-file.path(dir,"base.rds")
saveRDS(base,rds)
r1<-imesc_read_shape_upload(as_upload(rds),"Base-Shape")
check("rds upload is read as before",inherits(r1,"sf")&&nrow(r1)==nrow(base))
check("rds as a character path is still read",inherits(imesc_read_shape_upload(rds,"Base-Shape"),"sf"))

# shapefile in longitude/latitude
invisible(capture.output(sf::st_write(base,file.path(dir,"araca.shp"),quiet=TRUE)))
up<-as_upload(shp_files("araca"))
s1<-imesc_read_shape_upload(up,"Base-Shape")
check("shapefile (shp, shx, dbf, prj) is read",inherits(s1,"sf")&&nrow(s1)==nrow(base))
check("shapefile keeps the geometry",isTRUE(all.equal(as.numeric(sf::st_bbox(s1)),as.numeric(sf::st_bbox(base)),tolerance=1e-6)))
check("uploaded files are left untouched (the shapefile can be read again)",all(file.exists(up$datapath))&&inherits(imesc_read_shape_upload(up,"Base-Shape"),"sf"))

# projected shapefile (UTM 23S) is converted to longitude/latitude
utm<-sf::st_transform(base,31983)
invisible(capture.output(sf::st_write(utm,file.path(dir,"araca_utm.shp"),quiet=TRUE)))
s2<-imesc_read_shape_upload(as_upload(shp_files("araca_utm")),"Layer-Shape")
check("projected shapefile is converted to longitude/latitude",isTRUE(sf::st_is_longlat(s2))&&
        isTRUE(all.equal(as.numeric(sf::st_bbox(s2)),as.numeric(sf::st_bbox(base)),tolerance=1e-4)))

# invalid selections stop with a message (validate)
msg_of<-function(expr) tryCatch({expr;""},error=function(e) conditionMessage(e))
check("two .rds files are refused",grepl("one .rds",msg_of(imesc_read_shape_upload(as_upload(c(rds,rds)),"Base-Shape"))))
check(".rds mixed with shapefile files is refused",grepl("do not mix",msg_of(imesc_read_shape_upload(as_upload(c(rds,shp_files("araca"))),"Base-Shape"))))
unlink(dir,recursive=TRUE)
