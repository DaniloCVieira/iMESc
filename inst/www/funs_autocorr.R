# Spatial autocorrelation (Spatial Tools > Autocorrelation): base R plus geodist and gstat, already
# used by iMESc. Coordinates are longitude/latitude in decimal degrees; distances in km.

# great-circle distances (km) between the observations
ac_dist_km<-function(coords){
  xy<-data.frame(x=as.numeric(coords[,1]),y=as.numeric(coords[,2]))
  d<-geodist::geodist(xy,measure="haversine")/1000
  dimnames(d)<-list(rownames(coords),rownames(coords))
  d
}

# observations at the same place (identical coordinates, e.g. repeated sampling of a station):
# for each observation, the ID of the first observation at its location (the location ID)
ac_location_groups<-function(coords){
  key<-paste(format(as.numeric(coords[,1]),digits=15),format(as.numeric(coords[,2]),digits=15))
  rep_id<-rownames(coords)[match(key,key)]
  names(rep_id)<-rownames(coords)
  rep_id
}

# mean of each numeric column per location (rows named by the location IDs, in order of appearance)
ac_location_mean<-function(df,rep_id){
  ids<-unique(rep_id)
  g<-factor(rep_id[rownames(df)],levels=ids)
  out<-data.frame(lapply(df,function(v){
    m<-tapply(v,g,function(z) if(all(is.na(z))) NA_real_ else mean(z,na.rm=TRUE))
    as.numeric(m)
  }),row.names=ids,check.names=FALSE)
  attr(out,"n_per_location")<-as.integer(table(g))
  out
}

# spatial weights matrix from a distance matrix
# type: "knn" (k nearest neighbours), "band" (all neighbours up to dmax km) or "idw" (1/d^power,
# optionally only up to dmax km). Row-standardised by default (each row sums to 1).
ac_weights<-function(d,type=c("knn","band","idw"),k=4,dmax=NA,power=1,row_std=TRUE){
  type<-match.arg(type)
  n<-nrow(d)
  W<-matrix(0,n,n,dimnames=dimnames(d))
  if(type=="knn"){
    k<-max(1,min(round(k),n-1))
    for(i in seq_len(n)){
      o<-order(d[i,])
      o<-o[o!=i][seq_len(k)]
      W[i,o]<-1
    }
  } else if(type=="band"){
    auto<-!isTRUE(is.finite(dmax)&&dmax>0)
    if(auto){
      # smallest band in which every observation has at least one neighbour at a distance > 0
      # (observations at the same place are neighbours anyway)
      dd<-d
      dd[dd<=0]<-Inf
      dmax<-max(apply(dd,1,min))
      if(!is.finite(dmax)) stop("All observations are at the same place.")
    }
    W[d<=dmax]<-1
    diag(W)<-0
    attr(W,"dmax")<-dmax
    attr(W,"dmax_auto")<-auto
  } else{
    W<-1/d^power
    # coincident points (distance 0) are not weighted
    W[!is.finite(W)]<-0
    if(isTRUE(is.finite(dmax)&&dmax>0)) W[d>dmax]<-0
    diag(W)<-0
  }
  band<-attributes(W)[c("dmax","dmax_auto")]
  if(isTRUE(row_std)){
    rs<-rowSums(W)
    W[rs>0,]<-W[rs>0,,drop=FALSE]/rs[rs>0]
  }
  attr(W,"type")<-type
  attr(W,"dmax")<-band$dmax
  attr(W,"dmax_auto")<-band$dmax_auto
  W
}

# neighbour counts of a weights matrix (for the summary shown to the user)
ac_neighbours_summary<-function(W){
  nn<-rowSums(W>0)
  data.frame(Observations=nrow(W),"Min. neighbours"=min(nn),"Mean neighbours"=round(mean(nn),1),
             "Max. neighbours"=max(nn),"Without neighbours"=sum(nn==0),check.names=FALSE)
}

# Moran's I statistic
ac_moran_stat<-function(z,W,S0=sum(W)){
  length(z)/S0*sum(z*(W%*%z))/sum(z^2)
}

# Global Moran's I and Geary's C
# p-values: normal approximation (randomisation assumption) and permutations (nsim)
ac_global<-function(x,W,nsim=499,alternative=c("two.sided","greater","less"),seed=NULL){
  alternative<-match.arg(alternative)
  ok<-is.finite(x)
  if(!all(ok)){
    x<-x[ok]
    W<-W[ok,ok,drop=FALSE]
  }
  n<-length(x)
  if(n<4) stop("At least 4 observations with values are needed.")
  z<-x-mean(x)
  if(sum(z^2)==0) stop("The variable is constant (zero variance).")
  S0<-sum(W)
  if(S0==0) stop("No observation has neighbours with these spatial weights: increase k or the distance.")
  S1<-0.5*sum((W+t(W))^2)
  S2<-sum((rowSums(W)+colSums(W))^2)
  I<-ac_moran_stat(z,W,S0)
  EI<- -1/(n-1)
  b2<-n*sum(z^4)/sum(z^2)^2
  VI<-(n*((n^2-3*n+3)*S1-n*S2+3*S0^2)-b2*((n^2-n)*S1-2*n*S2+6*S0^2))/((n-1)*(n-2)*(n-3)*S0^2)-EI^2
  ZI<-(I-EI)/sqrt(VI)
  # Geary's C (E = 1; values below 1 indicate positive autocorrelation)
  # sum_ij w_ij (x_i - x_j)^2 = sum_i x_i^2 (rowsum_i + colsum_i) - 2 x'Wx (no n x n differences)
  rcs<-rowSums(W)+colSums(W)
  ssz<-sum(z^2)
  C<-(n-1)*(sum(x^2*rcs)-2*sum(x*(W%*%x)))/(2*S0*ssz)
  VC<-((2*S1+S2)*(n-1)-4*S0^2)/(2*(n+1)*S0^2)
  ZC<-(1-C)/sqrt(VC)
  p_norm<-function(zs){
    switch(alternative,
           two.sided=2*stats::pnorm(-abs(zs)),
           greater=stats::pnorm(zs,lower.tail=FALSE),
           less=stats::pnorm(zs))
  }
  p_perm_I<-p_perm_C<-NA
  if(isTRUE(nsim>0)){
    if(!is.null(seed)) set.seed(seed)
    # permutations keep the values (same mean and variance): only the cross products change
    perm<-replicate(nsim,sample(x))
    Wp<-W%*%perm
    xWx<-colSums(perm*Wp)
    mu<-mean(x)
    rs<-rowSums(W)
    # z'Wz with z = x - mu
    zWz<-xWx-mu*colSums(perm*rs)-mu*colSums(Wp)+mu^2*S0
    Iperm<-n/S0*zWz/ssz
    Cperm<-(n-1)*(colSums(perm^2*rcs)-2*xWx)/(2*S0*ssz)
    p_emp<-function(obs,sim,expected,larger_is_positive){
      dev<-if(larger_is_positive) sim-obs else obs-sim
      switch(alternative,
             two.sided=(sum(abs(sim-expected)>=abs(obs-expected))+1)/(nsim+1),
             greater=(sum(dev>=0)+1)/(nsim+1),
             less=(sum(dev<=0)+1)/(nsim+1))
    }
    p_perm_I<-p_emp(I,Iperm,EI,TRUE)
    # for Geary's C, positive autocorrelation is C below 1
    p_perm_C<-p_emp(C,Cperm,1,FALSE)
  }
  res<-data.frame(
    Statistic=c("Moran's I","Geary's C"),
    Observed=c(I,C),
    Expected=c(EI,1),
    Variance=c(VI,VC),
    z=c(ZI,ZC),
    "p (normal)"=c(p_norm(ZI),p_norm(ZC)),
    "p (permutation)"=c(p_perm_I,p_perm_C),
    check.names=FALSE
  )
  attr(res,"n")<-n
  attr(res,"nsim")<-nsim
  attr(res,"alternative")<-alternative
  res
}

# Local Moran's I (LISA; Anselin 1995), normal approximation under conditional randomisation
# clusters: High-High, Low-Low, High-Low, Low-High (significant at alpha) or Not significant
ac_local<-function(x,W,alpha=0.05,p_adjust="none",alternative=c("two.sided","greater","less")){
  alternative<-match.arg(alternative)
  n<-length(x)
  out<-data.frame(value=x,Ii=NA_real_,E=NA_real_,Var=NA_real_,z=NA_real_,p=NA_real_,p_adj=NA_real_,
                  cluster=factor(rep(NA,n),levels=c("High-High","Low-Low","High-Low","Low-High","Not significant")),
                  row.names=rownames(W))
  ok<-is.finite(x)
  xo<-x[ok]
  Wo<-W[ok,ok,drop=FALSE]
  m<-length(xo)
  if(m<4) stop("At least 4 observations with values are needed.")
  z<-xo-mean(xo)
  m2<-sum(z^2)/m
  if(m2==0) stop("The variable is constant (zero variance).")
  lag<-as.numeric(Wo%*%z)
  Ii<-z/m2*lag
  wi<-rowSums(Wo)
  wi2<-rowSums(Wo^2)
  # conditional randomisation: z_i fixed, the other m-1 values permuted over the neighbours
  # (the null distribution of conditional permutation tests, as in GeoDa and spdep >= 1.2)
  N<-m-1
  mu_o<- -z/N
  s2_o<-(sum(z^2)-z^2)/N-mu_o^2
  E<-(z/m2)*mu_o*wi
  V<-(z/m2)^2*s2_o*N/(N-1)*(wi2-wi^2/N)
  Z<-(Ii-E)/sqrt(V)
  Z[!is.finite(Z)]<-NA
  P<-switch(alternative,
            two.sided=2*stats::pnorm(-abs(Z)),
            greater=stats::pnorm(Z,lower.tail=FALSE),
            less=stats::pnorm(Z))
  # observations without neighbours have no local statistic
  P[wi==0]<-NA
  Padj<-stats::p.adjust(P,method=p_adjust)
  sig<-!is.na(Padj)&Padj<alpha
  cl<-ifelse(!sig,"Not significant",
             ifelse(z>0&lag>0,"High-High",ifelse(z<0&lag<0,"Low-Low",ifelse(z>0&lag<0,"High-Low","Low-High"))))
  out$Ii[ok]<-Ii; out$E[ok]<-E; out$Var[ok]<-V; out$z[ok]<-Z; out$p[ok]<-P; out$p_adj[ok]<-Padj
  out$cluster[ok]<-cl
  out$lag<-NA_real_
  out$lag[ok]<-lag
  attr(out,"alpha")<-alpha
  attr(out,"p_adjust")<-p_adjust
  out
}

# Moran correlogram: Moran's I for distance classes (binary weights within each class)
# breaks: "width" (classes of equal width) or "pairs" (classes with about the same number of pairs)
ac_correlogram<-function(x,d,n_classes=10,dmax=NA,breaks=c("width","pairs"),p_adjust="holm",alpha=0.05){
  breaks<-match.arg(breaks)
  ok<-is.finite(x)
  x<-x[ok]
  d<-d[ok,ok,drop=FALSE]
  n<-length(x)
  if(n<4) stop("At least 4 observations with values are needed.")
  z<-x-mean(x)
  if(sum(z^2)==0) stop("The variable is constant (zero variance).")
  dv<-d[upper.tri(d)]
  if(!isTRUE(is.finite(dmax)&&dmax>0)) dmax<-max(dv)/2
  n_classes<-max(1,round(n_classes))
  br<-if(breaks=="width"){
    seq(0,dmax,length.out=n_classes+1)
  } else{
    q<-stats::quantile(dv[dv<=dmax],probs=seq(0,1,length.out=n_classes+1),names=FALSE)
    q[1]<-0
    unique(q)
  }
  b2<-n*sum(z^4)/sum(z^2)^2
  EI<- -1/(n-1)
  res<-do.call(rbind,lapply(seq_len(length(br)-1),function(i){
    lo<-br[i]; hi<-br[i+1]
    W<-(d>lo&d<=hi)*1
    if(i==1) W[d==0]<-1
    diag(W)<-0
    S0<-sum(W)
    npairs<-S0/2
    if(S0==0){
      return(data.frame(class=i,from=lo,to=hi,mid=(lo+hi)/2,pairs=0,I=NA,Expected=EI,z=NA,p=NA))
    }
    S1<-0.5*sum((W+t(W))^2)
    S2<-sum((rowSums(W)+colSums(W))^2)
    I<-ac_moran_stat(z,W,S0)
    VI<-(n*((n^2-3*n+3)*S1-n*S2+3*S0^2)-b2*((n^2-n)*S1-2*n*S2+6*S0^2))/((n-1)*(n-2)*(n-3)*S0^2)-EI^2
    zs<-(I-EI)/sqrt(VI)
    data.frame(class=i,from=lo,to=hi,mid=(lo+hi)/2,pairs=npairs,I=I,Expected=EI,z=zs,p=2*stats::pnorm(-abs(zs)))
  }))
  res$p_adj<-stats::p.adjust(res$p,method=p_adjust)
  res$significant<-!is.na(res$p_adj)&res$p_adj<alpha
  attr(res,"n")<-n
  attr(res,"p_adjust")<-p_adjust
  attr(res,"alpha")<-alpha
  res
}

# empirical semivariogram (gstat, great-circle distances in km) and optional model fit
ac_variogram<-function(x,coords,cutoff=NA,n_lags=15,model="None"){
  ok<-is.finite(x)
  df<-data.frame(z=x[ok],lon=as.numeric(coords[ok,1]),lat=as.numeric(coords[ok,2]))
  if(nrow(df)<4) stop("At least 4 observations with values are needed.")
  if(stats::var(df$z)==0) stop("The variable is constant (zero variance).")
  pts<-sf::st_as_sf(df,coords=c("lon","lat"),crs=4326)
  d<-ac_dist_km(df[c("lon","lat")])
  if(!isTRUE(is.finite(cutoff)&&cutoff>0)) cutoff<-max(d)/3
  n_lags<-max(2,round(n_lags))
  v<-gstat::variogram(z~1,pts,cutoff=cutoff,width=cutoff/n_lags)
  if(!nrow(v)) stop("No pairs of observations within the cutoff distance: increase it.")
  fit<-NULL
  # pairs of observations at the same place (e.g. repeated sampling of a station)
  attr(v,"coincident")<-sum(d[upper.tri(d)]==0)
  if(!identical(model,"None")){
    v0<-stats::var(df$z)
    init<-gstat::vgm(psill=0.8*v0,model=model,range=cutoff/3,nugget=0.2*v0)
    # the fit uses the lags with distance > 0 (gstat's default weighting does not accept distance 0)
    vfit<-v[v$dist>0,]
    fit<-if(nrow(vfit)>=3) tryCatch(suppressWarnings(gstat::fit.variogram(vfit,init)),error=function(e) NULL) else NULL
    # negative parameters: no valid model (the line could not even be drawn)
    if(!is.null(fit)&&(any(fit$range<0)||any(fit$psill<0))) fit<-NULL
    if(!is.null(fit)&&isTRUE(attr(fit,"singular"))) attr(fit,"warning")<-"The fit may be unreliable (singular model)."
  }
  attr(v,"fit")<-fit
  attr(v,"cutoff")<-cutoff
  v
}

# ---- plots
ac_plot_scatter<-function(x,W,var_name="x",base_size=12,col="#2c7bb6"){
  ok<-is.finite(x)
  x<-x[ok]
  W<-W[ok,ok,drop=FALSE]
  zs<-(x-mean(x))/stats::sd(x)
  df<-data.frame(z=zs,lag=as.numeric(W%*%zs))
  I<-ac_moran_stat(zs,W)
  ggplot2::ggplot(df,ggplot2::aes(x=z,y=lag))+
    ggplot2::geom_hline(yintercept=0,linetype="dashed",colour="gray50")+
    ggplot2::geom_vline(xintercept=0,linetype="dashed",colour="gray50")+
    ggplot2::geom_point(colour=col,alpha=0.7)+
    ggplot2::geom_smooth(method="lm",formula=y~x,se=FALSE,colour="black",linewidth=0.7)+
    ggplot2::labs(title="Moran scatterplot",subtitle=paste0("Moran's I = ",signif(I,3)),
                  x=paste0(var_name," (standardised)"),y=paste0("Spatial lag of ",var_name))+
    ggplot2::theme_bw(base_size=base_size)
}

ac_plot_correlogram<-function(res,base_size=12,col="#2c7bb6"){
  res$Significance<-factor(ifelse(res$significant,"significant","not significant"),levels=c("significant","not significant"))
  ggplot2::ggplot(res,ggplot2::aes(x=mid,y=I))+
    ggplot2::geom_hline(yintercept=res$Expected[1],linetype="dashed",colour="gray50")+
    ggplot2::geom_line(colour=col,na.rm=TRUE)+
    ggplot2::geom_point(ggplot2::aes(shape=Significance),colour=col,fill=col,size=3,na.rm=TRUE)+
    ggplot2::scale_shape_manual(values=c("significant"=21,"not significant"=1),drop=FALSE,name=NULL)+
    ggplot2::labs(title="Moran correlogram",
                  subtitle=paste0("Filled: p < ",attr(res,"alpha"),if(attr(res,"p_adjust")!="none") paste0(" (",attr(res,"p_adjust")," adjusted)") else ""),
                  x="Distance class (km, midpoint)",y="Moran's I")+
    ggplot2::theme_bw(base_size=base_size)+
    ggplot2::theme(legend.position="bottom")
}

ac_plot_variogram<-function(v,base_size=12,col="#2c7bb6",sized=FALSE){
  fit<-attr(v,"fit")
  g<-ggplot2::ggplot(v,ggplot2::aes(x=dist,y=gamma))
  g<-if(isTRUE(sized)) g+ggplot2::geom_point(ggplot2::aes(size=np),colour=col)+ggplot2::scale_size(name="Pairs") else g+ggplot2::geom_point(colour=col,size=2.5)
  sub<-NULL
  if(!is.null(fit)){
    preds<-gstat::variogramLine(fit,maxdist=max(v$dist))
    g<-g+ggplot2::geom_line(data=preds,ggplot2::aes(x=dist,y=gamma),colour="black")
    m<-fit[fit$model!="Nug",][1,]
    nug<-sum(fit$psill[fit$model=="Nug"])
    sub<-paste0(as.character(m$model),": nugget = ",signif(nug,3),", partial sill = ",signif(m$psill,3),", range = ",signif(m$range,3)," km")
  }
  g+ggplot2::labs(title="Semivariogram",subtitle=sub,x="Distance (km)",y="Semivariance")+
    ggplot2::expand_limits(x=0,y=0)+
    ggplot2::theme_bw(base_size=base_size)
}

ac_cluster_colors<-c("High-High"="#d7191c","Low-Low"="#2c7bb6","High-Low"="#fdae61","Low-High"="#abd9e9","Not significant"="gray80")

ac_plot_lisa<-function(loc,coords,base_shape=NULL,layer_shape=NULL,base_size=12,point_size=2.5,what=c("cluster","Ii")){
  what<-match.arg(what)
  df<-data.frame(lon=as.numeric(coords[,1]),lat=as.numeric(coords[,2]),cluster=loc$cluster,Ii=loc$Ii)
  df<-df[!is.na(df$cluster),]
  g<-ggplot2::ggplot()
  if(!is.null(base_shape)) g<-g+ggplot2::geom_sf(data=sf::st_as_sf(base_shape),fill="gray95",colour="gray60")
  if(!is.null(layer_shape)) g<-g+ggplot2::geom_sf(data=sf::st_as_sf(layer_shape),fill="gray80",colour="gray60")
  if(what=="cluster"){
    g<-g+ggplot2::geom_point(data=df,ggplot2::aes(x=lon,y=lat,fill=cluster),shape=21,colour="gray30",size=point_size)+
      ggplot2::scale_fill_manual(values=ac_cluster_colors,drop=FALSE,name="LISA cluster")
  } else{
    g<-g+ggplot2::geom_point(data=df,ggplot2::aes(x=lon,y=lat,fill=Ii),shape=21,colour="gray30",size=point_size)+
      ggplot2::scale_fill_gradient2(low="#2c7bb6",mid="white",high="#d7191c",midpoint=0,name="Local I")
  }
  # the extent of the observations (the shapes may cover a larger area)
  rx<-range(df$lon); ry<-range(df$lat)
  px<-diff(rx)*0.05+1e-6; py<-diff(ry)*0.05+1e-6
  g<-if(!is.null(base_shape)||!is.null(layer_shape)) g+ggplot2::coord_sf(xlim=rx+c(-px,px),ylim=ry+c(-py,py),crs=4326,default_crs=4326,expand=FALSE) else g+ggplot2::coord_quickmap()
  g+ggplot2::labs(title=if(what=="cluster") "Local Moran's I (LISA) clusters" else "Local Moran's I",
                  subtitle=paste0("p < ",attr(loc,"alpha"),if(attr(loc,"p_adjust")!="none") paste0(" (",attr(loc,"p_adjust")," adjusted)") else ""),
                  x="Longitude",y="Latitude")+
    ggplot2::theme_bw(base_size=base_size)
}
