# Training animation of the SOM (recreate_som and plotnetwork_list2 in inst/www/funs_somplot.R):
# the frames must be the real states of the training, starting at the stored starting codebook
# and ending at the trained model, for one layer (online and batch) and for a superSOM
d<-araca_data()
x<-as.matrix(scale(d$envi))

# trains as the SOM module does: starting codebook drawn with the seed, seed set again before training
train_like_imesc<-function(layers,mode,seed=11,rlen=60,g=kohonen::somgrid(4,3,"hexagonal")){
  set.seed(seed)
  starters<-sample(seq_len(nrow(layers[[1]])),nrow(g$pts))
  init<-lapply(layers,function(l) l[starters,,drop=FALSE])
  set.seed(seed)
  m<-kohonen::supersom(layers,g,rlen=rlen,alpha=c(0.05,0.01),radius=c(2,0),mode=mode,init=init,
                       dist.fcts="euclidean",normalizeDataLayers=TRUE)
  m$mode<-mode
  m$seed<-seed
  m$init<-init
  m$normalizeDataLayers<-"TRUE"
  m
}

for(mode in c("online","batch")){
  m<-train_like_imesc(list(envi=x),mode)
  fr<-recreate_som(m,4,session=NULL)
  check(paste(mode,"- start frame plus 4 states"),length(fr)==5)
  check(paste(mode,"- iterations 0, 15, 30, 45, 60"),identical(vapply(fr,function(f) f$iter,numeric(1)),c(0,15,30,45,60)))
  check(paste(mode,"- first frame is the starting codebook"),isTRUE(all.equal(unname(fr[[1]]$codes[[1]]),unname(m$init[[1]]),check.attributes=FALSE)))
  check(paste(mode,"- last frame is the trained model"),isTRUE(attr(fr,"exact"))&&identical(fr[[5]]$unit.classif,setNames(m$unit.classif,rownames(x))))
  # real state at iteration 30: the same linear schedule stopped at 30 of 60
  set.seed(m$seed)
  half<-kohonen::supersom(list(envi=x),m$grid,rlen=30,alpha=c(0.05,0.03),radius=c(2,1),mode=mode,init=m$init,dist.fcts="euclidean")
  check(paste(mode,"- frame at iteration 30 is the state of the training at 30"),isTRUE(all.equal(unname(fr[[3]]$codes[[1]]),unname(half$codes[[1]]),tolerance=1e-8)))
  check(paste(mode,"- alpha and radius of the moment"),isTRUE(all.equal(c(fr[[3]]$alpha_now,fr[[3]]$radius_now),c(0.03,1))))
}

# superSOM (two layers, layer weights from the starting codebook)
hel<-as.matrix(d$hel[rownames(x),,drop=FALSE])
ms<-train_like_imesc(list(envi=x,hel=hel),"online")
frs<-recreate_som(ms,3,session=NULL)
check("superSOM - last frame is the trained model",isTRUE(attr(frs,"exact")))
check("superSOM - codes of both layers in every frame",all(vapply(frs,function(f) all(vapply(f$codes,nrow,numeric(1))==12),logical(1))))

# snap larger than rlen is limited to one frame per iteration
m5<-train_like_imesc(list(envi=x),"batch",rlen=5)
check("snap above rlen gives one frame per iteration",length(recreate_som(m5,50,session=NULL))==6)

# models without a starting codebook (older versions) give a message
old<-m5
old$init<-NULL
e<-tryCatch(recreate_som(old,3,session=NULL),error=function(e) conditionMessage(e))
check("model without starting codebook: clear message",is.character(e)&&grepl("older version",e))

# plot: frames and trained models of one and two layers; same axes in every frame
check("plot of a frame is a recorded plot",inherits(plotnetwork_list2(fr[[2]]),"recordedplot"))
check("plot of a trained superSOM is a recorded plot",inherits(plotnetwork_list2(ms,label=FALSE,show_points=FALSE),"recordedplot"))
p1<-som_pca_space(fr[[1]])
p5<-som_pca_space(fr[[5]])
check("observations at the same place in every frame",isTRUE(all.equal(p1$data,p5$data)))
check("one point per unit",nrow(p5$codes)==12)
