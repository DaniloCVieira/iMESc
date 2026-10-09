# Density-based clustering engine (inst/R/module09_dbscan.R)
set.seed(1)
blob<-function(n,mu,sd) matrix(rnorm(n*length(mu),rep(mu,each=n),sd),ncol=length(mu))

# DBSCAN against a brute-force reference (core points and their components)
X<-rbind(blob(60,c(0,0),.3),blob(60,c(3,3),.3),matrix(runif(10,-2,5),ncol=2))
pr<-dbs_prepare(X)
ref_dbscan<-function(X,eps,minPts){
  D<-as.matrix(dist(X)); core<-rowSums(D<=eps)>=minPts
  g<-igraph::graph_from_adjacency_matrix((D<=eps&outer(core,core))*1,mode="undirected",diag=FALSE)
  comp<-igraph::components(g)$membership
  cl<-ifelse(core,comp,NA)
  for(i in which(!core)){ nb<-which(D[i,]<=eps&core); if(length(nb)) cl[i]<-cl[nb[which.min(D[i,nb])]] }
  cl[is.na(cl)]<-0; cl
}
for(e in c(.2,.35,.5)) for(mp in c(4,8)){
  a<-dbs_dbscan(pr,e,mp)$cluster; b<-ref_dbscan(X,e,mp)
  # border points reachable from two clusters may join either one (order-dependent): the
  # clusters are compared on the core points
  core<-dbs_dbscan(pr,e,mp)$core
  check(paste0("DBSCAN = brute force (eps ",e,", minPts ",mp,")"),all(unname(a>0)==unname(b>0))&&(sum(core)<2||isTRUE(all.equal(ari(a[core],b[core]),1))))
}

# HDBSCAN recovers separated groups; its hierarchy is the single linkage of the mutual reachability
X<-rbind(blob(150,c(0,0),.5),blob(150,c(5,0),.5),blob(150,c(2.5,5),.5),matrix(runif(40,-2,7),ncol=2))
g<-c(rep(1:3,each=150),rep(0,20))
pr<-dbs_prepare(X)
mp<-dbs_suggest_minpts(pr$n,pr$p); mcs<-dbs_suggest_mcs(pr$n,mp)
h<-dbs_hdbscan(pr,mp,mcs)
check("HDBSCAN finds the 3 groups (ARI on inliers > 0.95)",length(unique(h$cluster[h$cluster>0]))==3&&ari(h$cluster[g>0],g[g>0])>0.95)
check("GLOSH ranks the outliers highest (AUC > 0.9)",{ r<-rank(h$glosh); n1<-sum(g==0); (sum(r[g==0])-n1*(n1+1)/2)/(n1*sum(g>0))>0.9 })
check("DBCV of a good partition > 0.5",dbs_dbcv(pr,h$cluster)>0.5)
hc<-dbs_hclust(h,as.character(seq_len(pr$n)))
M<-as.matrix(dist(X)); M<-pmax(M,outer(h$core_dist,h$core_dist,pmax)); diag(M)<-0
check("dendrogram = single linkage of the mutual reachability",isTRUE(all.equal(sort(hc$height),sort(stats::hclust(as.dist(M),"single")$height))))
check("leaf and selection epsilon run",{ dbs_hdbscan(pr,4,4,selection="leaf"); dbs_hdbscan(pr,mp,mcs,eps_sel=0.5); TRUE })

# Suggest eps: groups in blobs, a single group in a Gaussian cloud
s<-dbs_suggest_eps(pr,dbs_suggest_minpts(pr$n,pr$p,"dbscan"))
check("Suggest eps finds 3 groups (strong separation)",s$quality=="strong"&&length(unique(dbs_dbscan(pr,s$eps,dbs_suggest_minpts(pr$n,pr$p,"dbscan"))$cluster))>=3)
p1<-dbs_prepare(blob(400,c(0,0),1))
s1<-dbs_suggest_eps(p1,dbs_suggest_minpts(p1$n,p1$p,"dbscan"))
check("Suggest eps reports no structure in one Gaussian cloud",s1$quality=="none")

# principal axes and projection of new observations
d<-araca_data()
p0<-dbs_prepare(d$hel)
pa<-dbs_axes(p0)
check("PCA axes: projection of the training rows = their scores",max(abs(pa$project(d$hel)-pa$X))<1e-8)
pb<-dbs_axes(dbs_prepare(d$hel,"bray"))
check("PCoA axes: Gower's projection of the training rows = their scores",max(abs(pb$project(d$hel[1:20,])-pb$X[1:20,]))<1e-8)
check("broken-stick keeps 2 to 10 axes",pa$reduced$k>=2&&pa$reduced$k<=10)

# weighted points (SOM neurons with hits): no error and no zero eps
set.seed(1)
m<-kohonen::som(scale(as.matrix(d$envi)),grid=kohonen::somgrid(6,5,"hexagonal"))
hits<-tabulate(m$unit.classif,nrow(m$codes[[1]])); keep<-which(hits>0)
pw<-dbs_axes(dbs_prepare(m$codes[[1]][keep,]),NULL,rule="var50"); pw$w<-hits[keep]
check("weighted HDBSCAN with small minPts runs (heavy single neuron)",{ hw<-dbs_hdbscan(pw,4,4); length(hw$cluster)==length(keep) })
check("weighted Suggest eps is positive",dbs_suggest_eps(pw,4)$eps>0)
