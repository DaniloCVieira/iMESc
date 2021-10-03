
source("iMESc.R")
iMESc()
re
getcolhabs()

lapply(
  split(data,labels[,2:3]), function(x) apply(x,2,mean))

?plot.kohonen
kohonen:::plot.kohmapping


runGitHub('iMESc','DaniloCVieira', ref="main")
shiny::runGitHub('menvi','DaniloCVieira', ref="main")
