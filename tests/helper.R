# Helpers of the iMESc tests (base R only: no extra packages).
# check(): records one expectation; load_imesc(): sources the app code as app.R does;
# example data of the app (Araca) ready for the tests.

.tests<-new.env()
.tests$results<-data.frame(file=character(0),test=character(0),ok=logical(0),msg=character(0),stringsAsFactors=FALSE)
.tests$file<-""

# records an expectation; 'cond' may be an expression that errors (counted as a failure)
check<-function(desc,cond){
  res<-tryCatch(isTRUE(cond),error=function(e) structure(FALSE,msg=conditionMessage(e)))
  msg<-if(!is.null(attr(res,"msg"))) attr(res,"msg") else ""
  .tests$results<-rbind(.tests$results,data.frame(file=.tests$file,test=desc,ok=isTRUE(res),msg=msg,stringsAsFactors=FALSE))
  cat(if(isTRUE(res)) "  ok   " else "  FAIL ",desc,if(nzchar(msg)) paste0(" (",msg,")") else "","\n",sep="")
  invisible(isTRUE(res))
}

# packages and code of the app (inst/R modules and inst/www functions)
load_imesc<-function(root="."){
  pk<-c("shiny","shinyjs","shinyWidgets","colorRamps","data.table","shinyBS","colorspace","ggplot2","kohonen","caret")
  for(p in pk) suppressPackageStartupMessages(library(p,character.only=TRUE))
  code_files<-list.files(file.path(root,"inst/R"),pattern="[.]R$",full.names=TRUE)
  code_files<-code_files[!grepl("app_|run_app|install",code_files)]
  invisible(utils::capture.output(suppressMessages(for(f in code_files) try(source(f),silent=TRUE))))
  for(f in list.files(file.path(root,"inst/www"),pattern="^fun.*[.]R$",full.names=TRUE)) try(source(f),silent=TRUE)
  invisible(TRUE)
}

# example data of the app
read_example<-function(name,root="."){
  utils::read.csv(file.path(root,"inst/www",name),sep=";",row.names=1,check.names=FALSE,stringsAsFactors=TRUE)
}
araca_data<-function(root="."){
  fac<-read_example("factors_araca.csv",root)
  fac[]<-lapply(fac,factor)
  envi<-read_example("envi_araca.csv",root)
  envi<-envi[,vapply(envi,is.numeric,logical(1))]
  envi<-envi[stats::complete.cases(envi),]
  attr(envi,"factors")<-fac[rownames(envi),,drop=FALSE]
  nema<-read_example("nema_araca.csv",root)
  nema<-nema[,vapply(nema,is.numeric,logical(1))]
  nema[is.na(nema)]<-0
  hel<-sqrt(nema/rowSums(nema))
  hel<-hel[,colSums(hel)>0]
  attr(hel,"factors")<-fac[rownames(hel),,drop=FALSE]
  list(envi=envi,hel=hel,fac=fac)
}

# adjusted Rand index (agreement between two partitions)
ari<-function(a,b){
  t<-table(a,b); n<-sum(t); s<-function(x) sum(choose(x,2))
  ea<-s(rowSums(t))*s(colSums(t))/choose(n,2); den<-(s(rowSums(t))+s(colSums(t)))/2-ea
  if(den==0) return(NA_real_)
  (s(t)-ea)/den
}

# text of a rendered output (testServer)
out_text<-function(o) gsub("[[:space:]]+"," ",gsub("<[^>]+>"," ",o$html))
