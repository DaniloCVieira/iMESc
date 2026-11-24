# Spatial autocorrelation (Spatial Tools > Autocorrelation): Moran's I, Geary's C, LISA,
# correlogram and semivariogram, checked against direct formulas and permutations
set.seed(10)
n<-60
coords<-data.frame(lon=runif(n,-46,-45),lat=runif(n,-24,-23),row.names=paste0("p",1:n))
# a variable with a spatial trend (positive autocorrelation) and one without
x_trend<-coords$lon*10+coords$lat*5+rnorm(n,sd=0.5)
x_rand<-rnorm(n)

d<-ac_dist_km(coords)
check("distances in km: symmetric, zero diagonal, plausible values",
      isSymmetric(unname(d))&&all(diag(d)==0)&&max(d)>50&&max(d)<200)

W<-ac_weights(d,"knn",k=4)
check("k-nearest weights: 4 neighbours per row, row-standardised",all(rowSums(W>0)==4)&&isTRUE(all.equal(unname(rowSums(W)),rep(1,n))))
Wb<-ac_weights(d,"band",dmax=30,row_std=FALSE)
check("distance band: binary and symmetric",all(Wb%in%c(0,1))&&isSymmetric(unname(Wb)))
Wi<-ac_weights(d,"idw",power=2,row_std=FALSE)
check("inverse distance: 1/d^2 off the diagonal",isTRUE(all.equal(Wi[1,2],1/d[1,2]^2))&&all(diag(Wi)==0))

# Moran's I and Geary's C by their definitions
moran_def<-function(x,W){ z<-x-mean(x); n<-length(x); n/sum(W)*sum(W*outer(z,z))/sum(z^2) }
geary_def<-function(x,W){ n<-length(x); (n-1)*sum(W*outer(x,x,"-")^2)/(2*sum(W)*sum((x-mean(x))^2)) }
g<-ac_global(x_trend,W,nsim=199,seed=1)
check("Moran's I equals its definition",isTRUE(all.equal(g$Observed[1],moran_def(x_trend,W))))
check("Geary's C equals its definition",isTRUE(all.equal(g$Observed[2],geary_def(x_trend,W))))
check("trend: positive autocorrelation detected (I > E, C < 1, p < 0.01)",
      g$Observed[1]>g$Expected[1]&&g$Observed[2]<1&&all(g[["p (normal)"]]<0.01)&&all(g[["p (permutation)"]]<0.01))
g0<-ac_global(x_rand,W,nsim=199,seed=1)
check("random variable: no autocorrelation at 1% (normal and permutation)",all(g0[["p (normal)"]]>0.01)&&all(g0[["p (permutation)"]]>0.01))

# the analytic expectation and variance (randomisation) match those of many permutations
set.seed(2)
Isim<-replicate(3000,moran_def(sample(x_trend),Wb))
g_b<-ac_global(x_trend,Wb,nsim=0)
check("Moran's I: expectation -1/(n-1) matches the permutations",abs(mean(Isim)-g_b$Expected[1])<0.01)
check("Moran's I: randomisation variance matches the permutations (within 10%)",abs(stats::var(Isim)/g_b$Variance[1]-1)<0.1)

# the permutation shortcut (no n x n differences) gives the same statistics as the definitions
set.seed(3); xp<-sample(x_trend)
check("permutation shortcut: Moran's I and Geary's C of a permuted vector",
      isTRUE(all.equal(ac_global(xp,W,nsim=0)$Observed,c(moran_def(xp,W),geary_def(xp,W)))))

# missing values are left out
xna<-x_trend; xna[c(3,7)]<-NA
gna<-ac_global(xna,W,nsim=0)
check("NA values are left out (n = 58)",attr(gna,"n")==58&&is.finite(gna$Observed[1]))
check("constant variable stops with a message",grepl("constant",tryCatch(ac_global(rep(1,n),W,nsim=0),error=function(e) conditionMessage(e))))

# LISA: the local values add up to the global Moran's I (sum(Ii)/S0 = I)
loc<-ac_local(x_trend,W)
check("LISA: sum of local I / S0 equals the global Moran's I",isTRUE(all.equal(sum(loc$Ii)/sum(W),moran_def(x_trend,W))))
check("LISA: clusters are the five categories",all(loc$cluster%in%c("High-High","Low-Low","High-Low","Low-High","Not significant")))
check("LISA trend: High-High and Low-Low clusters found",all(c("High-High","Low-Low")%in%loc$cluster))
# the local expectation and variance match conditional permutations (value i fixed, the others permuted)
z<-x_trend-mean(x_trend); m2<-sum(z^2)/n
set.seed(4)
for(i in c(5,20)){
  Ii_sim<-replicate(4000,{ zz<-z; zz[-i]<-sample(z[-i]); zz[i]/m2*sum(W[i,]*zz) })
  check(paste0("LISA obs ",i,": expectation matches conditional permutations"),abs(mean(Ii_sim)-loc$E[i])<0.05*sd(Ii_sim))
  check(paste0("LISA obs ",i,": variance matches conditional permutations (within 10%)"),abs(stats::var(Ii_sim)/loc$Var[i]-1)<0.1)
}

# correlogram: positive at short distances for the trend, first class pairs counted
cg<-ac_correlogram(x_trend,d,n_classes=8)
check("correlogram: 8 classes up to half the maximum distance",nrow(cg)==8&&isTRUE(all.equal(max(cg$to),max(d)/2)))
check("correlogram: first class positive and significant",cg$I[1]>0&&cg$significant[1])
check("correlogram: pairs of the first class",cg$pairs[1]==sum(d[upper.tri(d)]>0&d[upper.tri(d)]<=cg$to[1]))
cgp<-ac_correlogram(x_trend,d,n_classes=5,breaks="pairs")
check("correlogram by number of pairs: similar counts",max(cgp$pairs)-min(cgp$pairs)<=0.05*sum(cgp$pairs)+2)

# semivariogram with gstat and a fitted model
v<-ac_variogram(x_trend,coords,n_lags=10,model="Sph")
check("variogram: lags up to the cutoff, semivariance increasing for the trend",nrow(v)>=5&&max(v$dist)<=attr(v,"cutoff")&&v$gamma[nrow(v)]>v$gamma[1])
check("variogram: spherical model fitted",inherits(attr(v,"fit"),"variogramModel")&&any(attr(v,"fit")$model=="Sph"))

# distance band without a distance: the smallest band giving every observation a neighbour
Wa<-ac_weights(d,"band")
check("automatic distance band: every observation has a neighbour",all(rowSums(Wa>0)>=1)&&isTRUE(attr(Wa,"dmax_auto")))

# repeated sampling at the same place (distance 0): the model is still fitted
co2<-rbind(coords,coords[1:10,])
rownames(co2)<-paste0("q",seq_len(nrow(co2)))
x2<-c(x_trend,x_trend[1:10]+rnorm(10,sd=0.5))
v2<-ac_variogram(x2,co2,n_lags=10,model="Exp")
check("variogram with coincident points: pairs counted and model fitted",attr(v2,"coincident")==10&&inherits(attr(v2,"fit"),"variogramModel"))

# mean per location: observations with identical coordinates averaged
lid<-ac_location_groups(co2)
check("location groups: the repeated points get the ID of the first one",all(lid[61:70]==rownames(co2)[1:10])&&length(unique(lid))==60)
m<-ac_location_mean(data.frame(x=x2,y=seq_along(x2),row.names=rownames(co2)),lid)
check("mean per location: 60 locations, means of the repeated points",nrow(m)==60&&isTRUE(all.equal(m$x[1],mean(x2[c(1,61)])))&&
        isTRUE(all.equal(m$y[10],mean(c(10,70))))&&m$x[11]==x2[11]&&sum(attr(m,"n_per_location"))==70)
xna2<-x2; xna2[c(1,61)]<-NA
check("mean per location: all values missing gives NA",is.na(ac_location_mean(data.frame(x=xna2,row.names=rownames(co2)),lid)$x[1]))

# plots are built
pdf(NULL)
check("plots: scatter, correlogram, variogram, LISA map",
      inherits(ac_plot_scatter(x_trend,W),"ggplot")&&inherits(ac_plot_correlogram(cg),"ggplot")&&
        inherits(ac_plot_variogram(v),"ggplot")&&inherits(ac_plot_lisa(loc,coords),"ggplot")&&
        !inherits(try(print(ac_plot_lisa(loc,coords)),silent=TRUE),"try-error"))
