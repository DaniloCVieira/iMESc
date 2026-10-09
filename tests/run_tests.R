# Runs the iMESc tests: every tests/test_*.R file (base R; no extra packages).
# From the root of the repository:
#   Rscript tests/run_tests.R              (all tests)
#   Rscript tests/run_tests.R dbscan       (files whose name contains 'dbscan')
# Exits with status 1 when a check fails, so it can be run before each commit.

args<-commandArgs(trailingOnly=TRUE)
if(!file.exists("tests/helper.R")) stop("Run from the root of the iMESc repository: Rscript tests/run_tests.R")
source("tests/helper.R")
cat("Loading iMESc code...\n")
load_imesc()
pdf(NULL)   # plots are built but not drawn on screen

files<-list.files("tests",pattern="^test_.*[.]R$",full.names=TRUE)
if(length(args)) files<-files[grepl(paste(args,collapse="|"),basename(files))]
t0<-Sys.time()
for(f in files){
  .tests$file<-basename(f)
  cat("\n==",basename(f),"\n")
  t1<-Sys.time()
  # each file runs in its own environment; an error stops that file only
  r<-tryCatch({ sys.source(f,envir=new.env(parent=globalenv())); NULL },error=function(e) e)
  if(inherits(r,"error")) check(paste0("file runs to the end (",conditionMessage(r),")"),FALSE)
  cat("   ",round(as.numeric(difftime(Sys.time(),t1,units="secs")),1),"s\n")
}
res<-.tests$results
cat("\n========================================\n")
cat(sum(res$ok),"passed,",sum(!res$ok),"failed in",length(files),"files (",round(as.numeric(difftime(Sys.time(),t0,units="secs")))," s )\n")
if(any(!res$ok)){
  cat("\nFailures:\n")
  for(i in which(!res$ok)) cat(" -",res$file[i],":",res$test[i],if(nzchar(res$msg[i])) paste0(" (",res$msg[i],")") else "","\n")
  if(file.exists("Rplots.pdf")) invisible(file.remove("Rplots.pdf"))
  quit(status=1)
}
if(file.exists("Rplots.pdf")) invisible(file.remove("Rplots.pdf"))
