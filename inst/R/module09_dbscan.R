# Density-based clustering (Unsupervised Algorithms > Density-based clustering)
# DBSCAN (Ester et al. 1996) and HDBSCAN* (Campello et al. 2013, 2015) implemented without
# extra packages, for the Numeric-Attribute of a Datalist or the codebook of a SOM model;
# parameter guide (k-distance plot, Suggest eps by DBCV, sensitivity analyses, condensed tree), results
# with the plots of the HC/K-means modules and GLOSH outliers, clusters saved in the Factor-Attribute
# and prediction of new data (projected onto the principal axes when they are used).

# ---- Density-based clustering engine (DBSCAN and HDBSCAN*) --------------------------------
# Implemented without extra packages (RANN for fast Euclidean neighbours). Distances are
# computed row by row, so memory stays O(n) for data; a 'dist' object can also be supplied
# (e.g. distances between SOM neurons).

# distances from row i to all rows; X: numeric matrix or a full distance matrix (is_dist)
#' @export
dbs_dist_row<-function(X,i,metric="euclidean",is_dist=FALSE){
  if(isTRUE(is_dist)) return(X[i,])
  xi<-X[i,]
  switch(metric,
         manhattan=colSums(abs(t(X)-xi)),
         bray={
           num<-colSums(abs(t(X)-xi))
           den<-colSums(t(X)+xi)
           out<-num/den
           out[den==0]<-0
           out
         },
         jaccard={
           b<-X>0
           bi<-b[i,]
           inter<-colSums(t(b)&bi)
           uni<-colSums(t(b)|bi)
           out<-1-inter/uni
           out[uni==0]<-0
           out
         },
         sqrt(colSums((t(X)-xi)^2)))
}

# prepares the input: data matrix (optionally scaled) or a distance matrix
#' @export
dbs_prepare<-function(x,metric="euclidean",scale=FALSE){
  if(inherits(x,"dist")){
    m<-as.matrix(x)
    return(list(X=m,is_dist=TRUE,metric="precomputed",n=nrow(m),ids=rownames(m),p=NA))
  }
  X<-as.matrix(x)
  storage.mode(X)<-"double"
  if(anyNA(X)) stop("The data contain missing values; impute or remove them first.")
  if(isTRUE(scale)){
    keep<-apply(X,2,stats::sd)>0
    X<-X[,keep,drop=FALSE]
    X<-scale(X)
  }
  if(metric%in%c("bray")&&any(X<0)) stop("Bray-Curtis requires non-negative values (do not scale the data).")
  list(X=X,is_dist=FALSE,metric=metric,n=nrow(X),ids=rownames(X),p=ncol(X),scaled=isTRUE(scale))
}

# neighbours of every point within eps (indices and distances); reused for smaller eps
#' @export
dbs_neighbors<-function(prep,eps){
  lapply(seq_len(prep$n),function(i){
    d<-dbs_dist_row(prep$X,i,prep$metric,prep$is_dist)
    w<-which(d<=eps)
    list(i=w,d=d[w])
  })
}

# principal axes of the data (PCA for Euclidean, PCoA for the other distances): density
# methods work poorly with many variables because all distances become similar
# number of axes by the broken-stick model (axes whose share of the variance exceeds the
# share expected from randomly broken variance; Jackson 1993, Legendre & Legendre 2012)
#' @export
dbs_bstick<-function(ve){
  p<-length(ve)
  bs<-rev(cumsum(1/(p:1)))/p
  k<-which(ve<=bs)[1]-1
  if(is.na(k)) k<-p
  k
}

# PCA (Euclidean) or PCoA (other distances) of the prepared data, computed once; 'project'
# places new observations on the first k axes (PCoA: Gower's add-a-point formula)
#' @export
dbs_decomp<-function(prep0,max_k=10){
  X<-prep0$X
  center0<-attr(X,"scaled:center")
  scale0<-attr(X,"scaled:scale")
  if(identical(prep0$metric,"euclidean")){
    pc<-stats::prcomp(X)
    project<-function(newX,k){
      newX<-as.matrix(newX)
      if(!is.null(center0)) newX<-scale(newX,center=center0,scale=scale0)
      scale(newX,center=pc$center,scale=FALSE)%*%pc$rotation[,seq_len(k),drop=FALSE]
    }
    return(list(method="PCA",scores=pc$x[,seq_len(min(ncol(pc$x),max(max_k,20))),drop=FALSE],ve=pc$sdev^2/sum(pc$sdev^2),
                project=project,p0=ncol(X),metric0="euclidean"))
  }
  n<-prep0$n
  D<-dbs_dmat(prep0)
  co<-suppressWarnings(stats::cmdscale(stats::as.dist(D),k=min(n-1,max(max_k,20)),eig=TRUE))
  pos<-co$eig[co$eig>0]
  pts<-co$points
  eig<-co$eig[seq_len(ncol(pts))]
  # diagonal of the double-centred matrix: b_i = mean_j d2_ij - mean(d2) / 2
  D2<-D^2
  b<-rowMeans(D2)-mean(D2)/2
  rm(D,D2)
  metric<-prep0$metric
  project<-function(newX,k){
    newX<-as.matrix(newX)
    if(!is.null(center0)) newX<-scale(newX,center=center0,scale=scale0)
    out<-vapply(seq_len(nrow(newX)),function(i){
      d2<-dbs_dist_row(rbind(newX[i,],X),1,metric,FALSE)[-1]^2
      0.5*colSums(pts[,seq_len(k),drop=FALSE]*(b-d2))/eig[seq_len(k)]
    },numeric(k))
    matrix(out,ncol=k,byrow=TRUE)
  }
  list(method="PCoA",scores=pts,ve=pos/sum(pos),project=project,p0=ncol(X),metric0=metric)
}

# data on the first k principal axes; k from a rule ("var50": smallest number explaining 50%
# of the variance; "bstick": broken-stick) between 2 and max_k, or given
#' @export
dbs_axes<-function(prep0,k=NULL,rule="bstick",max_k=10,decomp=NULL){
  dc<-if(is.null(decomp)) dbs_decomp(prep0,max_k) else decomp
  ve<-dc$ve
  manual<-!(is.null(k)||is.na(k))
  if(!manual){
    k<-if(identical(rule,"bstick")) dbs_bstick(ve) else which(cumsum(ve)>=0.5)[1]
    k<-min(max_k,max(2,k))
  }
  k<-max(1,min(k,ncol(dc$scores)))
  Z<-dc$scores[,seq_len(k),drop=FALSE]
  colnames(Z)<-paste0(dc$method,seq_len(k))
  rownames(Z)<-rownames(prep0$X)
  pr<-dbs_prepare(Z,"euclidean",FALSE)
  pr$project<-local({ kk<-k; function(newX) dc$project(newX,kk) })
  pr$decomp<-dc
  pr$reduced<-list(method=dc$method,k=k,var=sum(ve[seq_len(k)]),axis_var=ve[seq_len(min(2,k))],p0=dc$p0,metric0=dc$metric0,
                   rule=if(manual) "manual" else rule)
  pr
}

# distance of each point to its k-th nearest neighbour (self excluded)
#' @export
dbs_knn_dist<-function(prep,k){
  n<-prep$n
  k<-max(1,min(k,n-1))
  if(!prep$is_dist&&identical(prep$metric,"euclidean")){
    nn<-RANN::nn2(prep$X,k=min(n,k+1))
    return(nn$nn.dists[,k+1])
  }
  vapply(seq_len(n),function(i){
    d<-dbs_dist_row(prep$X,i,prep$metric,prep$is_dist)
    sort(d,partial=k+1)[k+1]
  },numeric(1))
}

# core distance: distance at which the neighbourhood of a point (itself included) holds
# minPts points; with weights (prep$w, e.g. the hits of SOM neurons) the weights are summed
#' @export
dbs_core_dist<-function(prep,minPts){
  w<-prep$w
  if(is.null(w)) return(if(minPts>1) dbs_knn_dist(prep,minPts-1) else rep(0,prep$n))
  vapply(seq_len(prep$n),function(i){
    d<-dbs_dist_row(prep$X,i,prep$metric,prep$is_dist)
    o<-order(d)
    j<-which(cumsum(w[o])>=minPts)[1]
    if(is.na(j)) max(d) else d[o[j]]
  },numeric(1))
}

# percentage of noise (weighted by prep$w when present)
dbs_noise<-function(prep,cl){
  w<-prep$w
  if(is.null(w)) return(100*mean(cl==0))
  100*sum(w[cl==0])/sum(w)
}

# DBSCAN (Ester et al. 1996). minPts counts the point itself; border points join the
# cluster of the first core point that reaches them; 0 = noise. With weights (prep$w) a
# point is a core point when the weights within eps sum to minPts or more.
#' @export
dbs_dbscan<-function(prep,eps,minPts=5,nbd=NULL){
  n<-prep$n
  if(is.null(nbd)) nbd<-dbs_neighbors(prep,eps)
  nb<-lapply(nbd,function(z) z$i[z$d<=eps])
  core<-if(is.null(prep$w)) lengths(nb)>=minPts else vapply(nb,function(ix) sum(prep$w[ix]),numeric(1))>=minPts
  cl<-integer(n)
  id<-0L
  for(i in which(core)){
    if(cl[i]!=0L) next
    id<-id+1L
    cl[i]<-id
    queue<-nb[[i]]
    head<-1L
    while(head<=length(queue)){
      j<-queue[head]
      head<-head+1L
      if(cl[j]==0L){
        cl[j]<-id
        if(core[j]){
          add<-nb[[j]][cl[nb[[j]]]==0L]
          if(length(add)) queue<-c(queue,add)
        }
      }
    }
  }
  structure(list(cluster=cl,core=core,eps=eps,minPts=minPts,method="dbscan"),class="imesc_dbscan")
}

# ---- HDBSCAN* (Campello et al. 2013, 2015)
# minimum spanning tree of the mutual reachability graph (Prim, O(n^2) time, O(n) memory)
dbs_mrd_mst<-function(prep,core){
  n<-prep$n
  in_tree<-logical(n)
  best<-rep(Inf,n)
  from<-rep(NA_integer_,n)
  edges<-matrix(NA_real_,n-1,3)
  cur<-1L
  in_tree[cur]<-TRUE
  for(e in seq_len(n-1)){
    d<-dbs_dist_row(prep$X,cur,prep$metric,prep$is_dist)
    mrd<-pmax(d,core[cur],core)
    upd<-!in_tree&mrd<best
    best[upd]<-mrd[upd]
    from[upd]<-cur
    cand<-which(!in_tree)
    nxt<-cand[which.min(best[cand])]
    edges[e,]<-c(from[nxt],nxt,best[nxt])
    in_tree[nxt]<-TRUE
    cur<-nxt
  }
  edges
}

# single-linkage tree from the MST: internal node n+i joins a and b at height h; sizes are
# numbers of points or sums of weights
dbs_single_linkage<-function(edges,n,w=NULL){
  if(is.null(w)) w<-rep(1,n)
  edges<-edges[order(edges[,3]),,drop=FALSE]
  parent<-seq_len(2*n-1)
  find<-function(x){ while(parent[x]!=x){ parent[x]<<-parent[parent[x]]; x<-parent[x] }; x }
  left<-right<-integer(n-1)
  height<-numeric(n-1)
  size<-c(w,numeric(n-1))
  for(i in seq_len(n-1)){
    a<-find(edges[i,1])
    b<-find(edges[i,2])
    node<-n+i
    left[i]<-a
    right[i]<-b
    height[i]<-edges[i,3]
    size[node]<-size[a]+size[b]
    parent[a]<-node
    parent[b]<-node
  }
  list(left=left,right=right,height=height,size=size,n=n,w=w)
}

# leaves under a node of the single-linkage tree
dbs_leaves<-function(sl,node){
  n<-sl$n
  out<-integer(0)
  stack<-node
  while(length(stack)){
    v<-stack[length(stack)]
    stack<-stack[-length(stack)]
    if(v<=n) out<-c(out,v) else stack<-c(stack,sl$left[v-n],sl$right[v-n])
  }
  out
}

# condensed tree: rows parent, child, lambda, child_size (children <= n are points)
dbs_condense<-function(sl,min_cluster_size){
  n<-sl$n
  root<-2L*n-1L
  wl<-if(is.null(sl$w)) rep(1,n) else sl$w
  relabel<-integer(2*n-1)
  next_label<-n+1L
  relabel[root]<-next_label
  next_label<-next_label+1L
  rows<-list()
  add<-function(p,c,l,s) rows[[length(rows)+1]]<<-c(p,c,l,s)
  stack<-root
  while(length(stack)){
    node<-stack[length(stack)]
    stack<-stack[-length(stack)]
    if(node<=n) next
    i<-node-n
    a<-sl$left[i]
    b<-sl$right[i]
    lambda<-if(sl$height[i]>0) 1/sl$height[i] else Inf
    sa<-sl$size[a]
    sb<-sl$size[b]
    p<-relabel[node]
    # a cluster needs at least min_cluster_size points (or weights) and more than one point:
    # a single heavy point (e.g. a SOM neuron with many hits) falls out of its parent
    big_a<-sa>=min_cluster_size&&a>n
    big_b<-sb>=min_cluster_size&&b>n
    if(big_a&&big_b){
      relabel[a]<-next_label; next_label<-next_label+1L
      relabel[b]<-next_label; next_label<-next_label+1L
      add(p,relabel[a],lambda,sa)
      add(p,relabel[b],lambda,sb)
      stack<-c(stack,a,b)
    } else if(!big_a&&!big_b){
      for(leaf in dbs_leaves(sl,a)) add(p,leaf,lambda,wl[leaf])
      for(leaf in dbs_leaves(sl,b)) add(p,leaf,lambda,wl[leaf])
    } else if(!big_a){
      relabel[b]<-p
      for(leaf in dbs_leaves(sl,a)) add(p,leaf,lambda,wl[leaf])
      stack<-c(stack,b)
    } else{
      relabel[a]<-p
      for(leaf in dbs_leaves(sl,b)) add(p,leaf,lambda,wl[leaf])
      stack<-c(stack,a)
    }
  }
  ct<-as.data.frame(do.call(rbind,rows))
  colnames(ct)<-c("parent","child","lambda","size")
  ct
}

# stability of each cluster and selection: excess of mass ("eom") or the leaves of the
# condensed tree ("leaf"); eps_sel > 0 merges clusters born below that distance into their
# ancestor (cluster_selection_epsilon, Malzer & Baum 2020)
dbs_select<-function(ct,n,allow_single=FALSE,selection="eom",eps_sel=0){
  clusters<-sort(unique(ct$parent))
  # finite lambdas (duplicated points give infinite lambda)
  fin<-ct$lambda[is.finite(ct$lambda)]
  lam_max<-if(length(fin)) max(fin)*1.01 else 1
  ct$lambda[!is.finite(ct$lambda)]<-lam_max
  birth<-stats::setNames(rep(0,length(clusters)),clusters)
  cl_rows<-ct$child>n
  birth[as.character(ct$child[cl_rows])]<-ct$lambda[cl_rows]
  stab<-stats::setNames(numeric(length(clusters)),clusters)
  for(c in clusters){
    r<-ct$parent==c
    stab[as.character(c)]<-sum((ct$lambda[r]-birth[as.character(c)])*ct$size[r])
  }
  root<-min(clusters)
  selected<-stats::setNames(clusters!=root|isTRUE(allow_single),clusters)
  sub_stab<-stab
  children<-function(c) ct$child[ct$parent==c&ct$child>n]
  descendants<-function(c){ out<-integer(0); st<-children(c); while(length(st)){ v<-st[1]; st<-st[-1]; out<-c(out,v); st<-c(st,children(v)) }; out }
  for(c in rev(clusters)){
    ch<-children(c)
    if(!length(ch)) next
    s_ch<-sum(sub_stab[as.character(ch)])
    if(c==root&&!isTRUE(allow_single)){ next }
    if(s_ch>stab[as.character(c)]){
      selected[as.character(c)]<-FALSE
      sub_stab[as.character(c)]<-s_ch
    } else{
      selected[as.character(descendants(c))]<-FALSE
    }
  }
  if(!isTRUE(allow_single)) selected[as.character(root)]<-FALSE
  sel<-clusters[selected]
  if(identical(selection,"leaf")){
    leaves<-clusters[!clusters%in%ct$parent[cl_rows]]
    sel<-if(length(leaves)==1&&leaves==root) (if(isTRUE(allow_single)) root else integer(0)) else setdiff(leaves,root)
  }
  if(isTRUE(eps_sel>0)&&length(sel)){
    parent_of<-stats::setNames(ct$parent[cl_rows],ct$child[cl_rows])
    eps_birth<-function(c){ b<-birth[[as.character(c)]]; if(b>0) 1/b else Inf }
    out<-numeric(0)
    processed<-numeric(0)
    for(leaf in sel){
      if(leaf==root||eps_birth(leaf)>=eps_sel){ out<-c(out,leaf); next }
      if(leaf%in%processed) next
      node<-leaf
      repeat{
        p<-parent_of[[as.character(node)]]
        if(p==root){ if(isTRUE(allow_single)) node<-root; break }
        if(eps_birth(p)>eps_sel){ node<-p; break }
        node<-p
      }
      out<-c(out,node)
      processed<-c(processed,descendants(node))
    }
    sel<-unique(out)
  }
  list(ct=ct,stability=stab,selected=sel,birth=birth)
}

# GLOSH outlier scores (Campello et al. 2015): 1 - lambda(point) / largest lambda reached in
# the cluster the point falls out of (0 = in the densest part, close to 1 = outlier)
dbs_glosh<-function(ct,n){
  clusters<-sort(unique(ct$parent))
  dz<-stats::setNames(vapply(clusters,function(c) max(ct$lambda[ct$parent==c]),numeric(1)),clusters)
  cr<-ct[ct$child>n,,drop=FALSE]
  cr<-cr[order(-cr$child),,drop=FALSE]
  for(r in seq_len(nrow(cr))){
    c<-as.character(cr$child[r])
    p<-as.character(cr$parent[r])
    if(!is.na(dz[c])&&dz[c]>dz[p]) dz[p]<-dz[c]
  }
  pts<-ct[ct$child<=n,,drop=FALSE]
  lm<-dz[as.character(pts$parent)]
  out<-numeric(n)
  out[pts$child]<-ifelse(is.finite(lm)&lm>0&is.finite(pts$lambda),(lm-pts$lambda)/lm,0)
  pmax(0,pmin(1,out))
}

# HDBSCAN*: minPts (core distance, the point counted) and minimum cluster size; weights
# (prep$w) count in the core distances and in the cluster sizes
#' @export
dbs_hdbscan<-function(prep,minPts=5,min_cluster_size=minPts,allow_single=FALSE,tree=NULL,selection="eom",eps_sel=0){
  n<-prep$n
  if(n<3) stop("At least 3 observations are needed.")
  if(is.null(tree)){
    core<-dbs_core_dist(prep,minPts)
    edges<-dbs_mrd_mst(prep,core)
    tree<-list(sl=dbs_single_linkage(edges,n,prep$w),core=core,minPts=minPts)
  }
  mcs<-max(2,min(min_cluster_size,if(is.null(prep$w)) n else sum(prep$w)))
  ct<-dbs_condense(tree$sl,mcs)
  sel<-dbs_select(ct,n,allow_single,selection,eps_sel)
  ct<-sel$ct
  parent_of<-stats::setNames(ct$parent[ct$child>n],ct$child[ct$child>n])
  sel_set<-sel$selected
  # selected cluster containing a condensed-tree cluster (or NA)
  owner<-function(c){
    while(!is.na(c)){
      if(c%in%sel_set) return(c)
      c<-if(as.character(c)%in%names(parent_of)) parent_of[[as.character(c)]] else NA
    }
    NA
  }
  pts<-ct[ct$child<=n,,drop=FALSE]
  own<-vapply(pts$parent,owner,numeric(1))
  label<-match(own,sort(sel_set))
  cl<-integer(n)
  cl[pts$child]<-ifelse(is.na(label),0L,label)
  # membership probability: lambda of the point relative to the largest lambda of its cluster
  prob<-numeric(n)
  lam_pt<-numeric(n)
  lam_pt[pts$child]<-pts$lambda
  for(k in seq_along(sel_set)){
    idx<-which(cl==k)
    if(!length(idx)) next
    m<-max(lam_pt[idx])
    prob[idx]<-if(m>0) pmin(lam_pt[idx],m)/m else 1
  }
  st<-sel$stability[as.character(sort(sel_set))]
  structure(list(cluster=cl,prob=prob,glosh=dbs_glosh(ct,n),stability=stats::setNames(as.numeric(st),seq_along(st)),
                 core_dist=tree$core,minPts=minPts,min_cluster_size=mcs,condensed=ct,
                 selected=sort(sel_set),birth=sel$birth,tree=tree,selection=selection,eps_sel=eps_sel,method="hdbscan"),class="imesc_dbscan")
}

# ---- cluster validity
# distance matrix among the rows idx
dbs_dmat<-function(prep,idx=seq_len(prep$n)){
  if(isTRUE(prep$is_dist)) return(unname(prep$X[idx,idx,drop=FALSE]))
  X<-prep$X[idx,,drop=FALSE]
  if(prep$metric%in%c("euclidean","manhattan")) return(unname(as.matrix(stats::dist(X,method=prep$metric))))
  t(vapply(seq_along(idx),function(i) dbs_dist_row(X,i,prep$metric,FALSE),numeric(length(idx))))
}

# minimum spanning tree of a full distance matrix (Prim): rows from, to, weight
dbs_prim<-function(M){
  n<-nrow(M)
  if(n<2) return(matrix(numeric(0),0,3))
  in_tree<-logical(n)
  in_tree[1]<-TRUE
  best<-M[1,]
  from<-rep(1L,n)
  edges<-matrix(NA_real_,n-1,3)
  for(e in seq_len(n-1)){
    b<-best
    b[in_tree]<-Inf
    nxt<-which.min(b)
    edges[e,]<-c(from[nxt],nxt,b[nxt])
    in_tree[nxt]<-TRUE
    upd<-!in_tree&M[nxt,]<best
    best[upd]<-M[nxt,upd]
    from[upd]<-nxt
  }
  edges
}

# clustered points, evenly subsampled within each cluster when there are more than max_n
# (deterministic: the same result every time)
dbs_subsample<-function(cl,max_n){
  idx<-which(cl>0)
  if(length(idx)<=max_n) return(idx)
  sort(unlist(lapply(split(idx,cl[idx]),function(s){
    m<-max(min(length(s),3),round(max_n*length(s)/length(idx)))
    s[unique(round(seq(1,length(s),length.out=m)))]
  })))
}

# DBCV, Density-Based Clustering Validation (Moulavi et al. 2014): for each cluster, its
# density sparseness (largest edge of the minimum spanning tree of the mutual reachability
# distances among its internal points) against its density separation (smallest mutual
# reachability distance to another cluster). From -1 to 1; the noise lowers the index because
# each cluster counts with its share of all points (weights in prep$w).
#' @export
dbs_dbcv<-function(prep,cl,max_n=1500){
  ids<-sort(unique(cl[cl>0]))
  if(length(ids)<2) return(NA_real_)
  w<-if(is.null(prep$w)) rep(1,length(cl)) else prep$w
  idx<-dbs_subsample(cl,max_n)
  lab<-cl[idx]
  D<-dbs_dmat(prep,idx)
  dim<-prep$p
  if(is.null(dim)||is.na(dim)) dim<-2
  tiny<-max(D)*1e-10+1e-300
  # all-points core distance: (mean over the cluster of (1 / d)^dim)^(-1 / dim), in log scale
  core<-numeric(length(idx))
  mem<-split(seq_along(idx),lab)
  for(s in mem){
    m<-length(s)
    if(m<2) next
    L<- -dim*log(pmax(D[s,s,drop=FALSE],tiny))
    diag(L)<- -Inf
    mx<-apply(L,1,max)
    lse<-mx+log(rowSums(exp(L-mx)))
    core[s]<-exp(-(lse-log(m-1))/dim)
  }
  MRD<-pmax(D,outer(core,core,pmax))
  internal<-vector("list",length(mem))
  dsc<-numeric(length(mem))
  for(i in seq_along(mem)){
    s<-mem[[i]]
    if(length(s)<2){ internal[[i]]<-s; next }
    ed<-dbs_prim(MRD[s,s,drop=FALSE])
    deg<-tabulate(c(ed[,1],ed[,2]),length(s))
    int<-which(deg>1)
    if(!length(int)) int<-seq_along(s)
    ie<-ed[ed[,1]%in%int&ed[,2]%in%int,,drop=FALSE]
    dsc[i]<-if(nrow(ie)) max(ie[,3]) else max(ed[,3])
    internal[[i]]<-s[int]
  }
  sep<-vapply(seq_along(mem),function(i) min(vapply(seq_along(mem)[-i],function(j) min(MRD[internal[[i]],internal[[j]]]),numeric(1))),numeric(1))
  v<-(sep-dsc)/pmax(sep,dsc)
  v[!is.finite(v)]<-0
  wk<-vapply(as.numeric(names(mem)),function(k) sum(w[cl==k]),numeric(1))
  sum(wk*v)/sum(w)
}

# silhouette of the clustered points (noise excluded); NA when not computable
#' @export
dbs_silhouette<-function(prep,cl,max_n=3000){
  if(length(unique(cl[cl>0]))<2||sum(cl>0)<3) return(NA_real_)
  idx<-dbs_subsample(cl,max_n)
  s<-cluster::silhouette(cl[idx],stats::as.dist(dbs_dmat(prep,idx)))
  mean(s[,"sil_width"])
}

# ---- parameter help
# knee of a sorted curve (Kneedle on both axes normalised to [0,1])
#' @export
dbs_knee<-function(y){
  y<-sort(y)
  n<-length(y)
  if(n<3) return(list(index=n,value=y[n]))
  x<-(seq_len(n)-1)/(n-1)
  r<-diff(range(y))
  yn<-if(r>0) (y-min(y))/r else rep(0,n)
  dist<-x-yn
  i<-which.max(dist)
  list(index=i,value=y[i])
}

# DBSCAN for several eps values (neighbours computed once): clusters, noise, DBCV and the
# size of the smallest cluster (weighted by prep$w)
#' @export
dbs_eps_scan<-function(prep,eps_values,minPts,max_n=1500){
  nbd<-dbs_neighbors(prep,max(eps_values))
  w<-if(is.null(prep$w)) rep(1,prep$n) else prep$w
  do.call(rbind,lapply(eps_values,function(e){
    r<-dbs_dbscan(prep,e,minPts,nbd=nbd)
    cl<-r$cluster
    data.frame(eps=e,clusters=length(unique(cl[cl>0])),noise=dbs_noise(prep,cl),dbcv=dbs_dbcv(prep,cl,max_n),
               min_size=if(any(cl>0)) min(tapply(w[cl>0],cl[cl>0],sum)) else 0)
  }))
}

# suggested eps: DBSCAN over eps values between the 2% and 98% quantiles of the core
# distances; the eps with the highest DBCV among the solutions with 2 or more clusters, all
# of them with at least min_size points (by default the suggested min cluster size, as in
# HDBSCAN: without it, small eps values give many tiny clusters with a high DBCV, even in data
# without groups). The plateau is the range of neighbouring eps values with the same number of
# clusters. When no eps qualifies, the knee of the k-distance curve is returned and the data
# are reported as one group. Simulations (blobs, different densities, non-convex, unequal
# sizes, high-dimensional, community data, one Gaussian cloud) chose this rule.
#' @export
dbs_suggest_eps<-function(prep,minPts,kd=NULL,nq=25,max_n=1500,min_size=NULL){
  if(is.null(kd)) kd<-dbs_core_dist(prep,minPts)
  n_u<-if(is.null(prep$w)) prep$n else sum(prep$w)
  if(is.null(min_size)) min_size<-dbs_suggest_mcs(n_u,minPts)
  # core distances of 0 (a weighted point that alone reaches minPts, e.g. a SOM neuron with
  # many hits, or duplicates) say nothing about the radius
  kdp<-kd[kd>0]
  if(length(kdp)<3) kdp<-kd
  knee<-dbs_knee(kdp)$value
  e_v<-unique(signif(stats::quantile(kdp,seq(0.02,0.98,length.out=nq),names=FALSE),3))
  e_v<-e_v[e_v>0]
  if(!length(e_v)) return(list(eps=knee,knee=knee,scan=NULL,plateau=NULL,quality="none",stable=FALSE,note="knee of the k-distance plot."))
  scan<-dbs_eps_scan(prep,e_v,minPts,max_n)
  rk<-dbs_dbscan(prep,knee,minPts)
  k_knee<-length(unique(rk$cluster[rk$cluster>0]))
  knee_txt<-paste0("The knee (",signif(knee,3),") gives ",k_knee," cluster",if(k_knee!=1) "s" else "",".")
  ok<-which(scan$clusters>=2&!is.na(scan$dbcv)&scan$dbcv>0&scan$min_size>=min_size)
  if(!length(ok)){
    # the eps of a single group: the smallest tested eps that gives one cluster (the largest
    # tested eps when none does)
    one<-which(scan$clusters==1&seq_len(nrow(scan))>=which.max(scan$clusters))
    i1<-if(length(one)) min(one) else nrow(scan)
    e1<-scan$eps[i1]
    k1<-scan$clusters[i1]
    return(list(eps=e1,knee=knee,scan=scan,plateau=NULL,quality="none",stable=FALSE,min_size=min_size,
                note=paste0("no eps value separates 2 or more clusters of at least ",min_size," points by low density (DBCV > 0): the data seem to form a single group. eps set to ",signif(e1,3),
                            if(k1==1) ", which gives one cluster. " else paste0(" (the largest tested), which still gives ",k1," clusters, some with fewer than ",min_size," points. "),
                            "Check with HDBSCAN, a smaller minPts or other principal axes. ",knee_txt)))
  }
  b<-ok[which.max(scan$dbcv[ok])]
  lo<-hi<-b
  while(lo>1&&scan$clusters[lo-1]==scan$clusters[b]) lo<-lo-1
  while(hi<nrow(scan)&&scan$clusters[hi+1]==scan$clusters[b]) hi<-hi+1
  stable<-(hi-lo+1)>=3
  v<-scan$dbcv[b]
  quality<-if(v>=0.5) "strong" else if(v>=0.2) "moderate" else "weak"
  note<-paste0(scan$clusters[b]," clusters, ",round(scan$noise[b],1),"% noise, DBCV ",round(v,2)," (",quality," density separation; highest DBCV of ",nrow(scan)," eps values tested, clusters of at least ",min_size," points). ",
               if(stable) paste0("Stable: ",scan$clusters[b]," clusters from eps = ",scan$eps[lo]," to ",scan$eps[hi],". ") else "Unstable: the number of clusters changes with small changes of eps. ",
               knee_txt)
  list(eps=scan$eps[b],knee=knee,scan=scan,plateau=c(scan$eps[lo],scan$eps[hi]),quality=quality,stable=stable,min_size=min_size,note=note)
}

# suggested minPts: 2 x dimensions (Sander et al. 1998; Schubert et al. 2017), at least 4; for
# DBSCAN also at least ln(n), which smooths the density estimate of large data sets; at most
# about n/10
#' @export
dbs_suggest_minpts<-function(n,p,method="hdbscan"){
  if(is.na(p)) p<-2
  s<-max(4,2*p)
  if(identical(method,"dbscan")) s<-max(s,ceiling(log(n)))
  s<-min(s,max(3,floor(n/10)))
  as.integer(max(3,s))
}

# suggested minimum cluster size: sqrt(n), and at least minPts. In the simulations it found
# the right number of groups twice as often as min cluster size = minPts, which leaves small
# groups of outliers as clusters
#' @export
dbs_suggest_mcs<-function(n,minPts){
  as.integer(max(minPts,min(ceiling(sqrt(n)),max(minPts,floor(n/4)))))
}

# k-distance plot with the knee, the suggested eps and its plateau
#' @export
gg_dbs_kdist<-function(kd,k,eps=NULL,base_size=12,sugg=NULL,weighted=FALSE){
  y<-sort(kd)
  kn<-dbs_knee(y)
  df<-data.frame(index=seq_along(y),dist=y)
  p<-ggplot2::ggplot(df,ggplot2::aes(x=index,y=dist))
  cap<-"Red: knee of the curve. Dotted: current eps."
  if(!is.null(sugg$plateau)){
    p<-p+ggplot2::annotate("rect",xmin=-Inf,xmax=Inf,ymin=sugg$plateau[1],ymax=sugg$plateau[2],fill="#2f6f3e",alpha=0.12)+
      ggplot2::geom_hline(yintercept=sugg$eps,linetype=1,color="#2f6f3e",linewidth=0.6)+
      ggplot2::annotate("text",x=1,y=sugg$eps,label=paste0("suggested: ",signif(sugg$eps,3)),hjust=0,vjust=-0.6,color="#2f6f3e",size=base_size/3.2)
    cap<-"Red: knee of the curve. Green: suggested eps (highest DBCV) and the range with the same number of clusters. Dotted: current eps."
  }
  p<-p+ggplot2::geom_line(color="#05668D",linewidth=0.8)+
    ggplot2::geom_hline(yintercept=kn$value,linetype=2,color="#B2182B")+
    ggplot2::annotate("point",x=kn$index,y=kn$value,color="#B2182B",size=2.5)+
    ggplot2::annotate("text",x=kn$index,y=kn$value,label=paste0("knee: ",signif(kn$value,3)),hjust=1.1,vjust=-0.6,color="#B2182B",size=base_size/3.2)
  if(!is.null(eps)&&is.finite(eps)) p<-p+ggplot2::geom_hline(yintercept=eps,linetype=3,color="gray30")
  p+ggplot2::labs(x=if(weighted) "Neurons sorted by distance" else "Observations sorted by distance",
                  y=if(weighted) "Core distance (weighted by the hits)" else paste0("Distance to the ",k,"-th nearest neighbour"),
                  title=if(weighted) paste0("Core distances (minPts = ",k+1," observations)") else paste0("k-distance plot (k = minPts - 1 = ",k,")"),
                  caption=cap)+
    ggplot2::theme_bw(base_size=base_size)
}

# DBSCAN for a grid of eps x minPts: clusters, noise and DBCV
#' @export
dbs_grid<-function(prep,eps_values,minpts_values,progress=NULL){
  res<-list()
  k<-0
  nbd<-dbs_neighbors(prep,max(eps_values))
  for(mp in minpts_values) for(e in eps_values){
    k<-k+1
    if(is.function(progress)) progress(k/(length(eps_values)*length(minpts_values)))
    r<-dbs_dbscan(prep,e,mp,nbd=nbd)
    res[[k]]<-data.frame(eps=e,minPts=mp,clusters=length(unique(r$cluster[r$cluster>0])),
                         noise=dbs_noise(prep,r$cluster),dbcv=dbs_dbcv(prep,r$cluster,1000))
  }
  do.call(rbind,res)
}

# HDBSCAN for several minimum cluster sizes (the tree is computed once)
#' @export
dbs_hdbscan_sizes<-function(prep,minPts,sizes,allow_single=FALSE,selection="eom",eps_sel=0){
  base<-dbs_hdbscan(prep,minPts,min(sizes))
  do.call(rbind,lapply(sizes,function(s){
    r<-dbs_hdbscan(prep,minPts,s,allow_single=allow_single,tree=base$tree,selection=selection,eps_sel=eps_sel)
    data.frame(min_cluster_size=s,clusters=length(unique(r$cluster[r$cluster>0])),noise=dbs_noise(prep,r$cluster),
               dbcv=dbs_dbcv(prep,r$cluster,1000))
  }))
}

# number of principal axes: each number of axes with its suggested minPts (and, for DBSCAN,
# the suggested eps); clusters, noise and DBCV
#' @export
dbs_axes_scan<-function(prep0,decomp,ks,method="hdbscan",allow_single=FALSE,selection="eom",progress=NULL){
  do.call(rbind,lapply(seq_along(ks),function(i){
    if(is.function(progress)) progress(i/length(ks))
    pr<-dbs_axes(prep0,ks[i],decomp=decomp)
    mp<-dbs_suggest_minpts(pr$n,pr$p,method)
    eps<-NA_real_
    if(identical(method,"dbscan")){
      eps<-dbs_suggest_eps(pr,mp,nq=15,max_n=1000)$eps
      r<-dbs_dbscan(pr,eps,mp)
    } else{
      r<-dbs_hdbscan(pr,mp,dbs_suggest_mcs(pr$n,mp),allow_single=allow_single,selection=selection)
    }
    data.frame(axes=pr$reduced$k,variance=round(100*pr$reduced$var,1),minPts=mp,eps=eps,clusters=length(unique(r$cluster[r$cluster>0])),
               noise=dbs_noise(pr,r$cluster),dbcv=dbs_dbcv(pr,r$cluster,1000))
  }))
}

# new observations: DBSCAN -> cluster of the nearest core point within eps;
# HDBSCAN -> cluster of the nearest clustered point by mutual reachability, noise beyond
# the distance at which that cluster appeared in the hierarchy
#' @export
dbs_predict<-function(model,prep,newX){
  newX<-as.matrix(newX)
  if(is.function(prep$project)){
    newX<-prep$project(newX)
  } else if(isTRUE(prep$scaled)){
    newX<-scale(newX,center=attr(prep$X,"scaled:center"),scale=attr(prep$X,"scaled:scale"))
  }
  out<-integer(nrow(newX))
  for(i in seq_len(nrow(newX))){
    Xi<-rbind(newX[i,],prep$X)
    d<-dbs_dist_row(Xi,1,prep$metric,FALSE)[-1]
    if(identical(model$method,"dbscan")){
      cand<-which(model$core&d<=model$eps)
      out[i]<-if(length(cand)) model$cluster[cand[which.min(d[cand])]] else 0L
    } else{
      cl_pts<-which(model$cluster>0)
      if(!length(cl_pts)) next
      kn<-sort(d,partial=min(length(d),max(1,model$minPts-1)))[min(length(d),max(1,model$minPts-1))]
      mrd<-pmax(d[cl_pts],model$core_dist[cl_pts],kn)
      j<-cl_pts[which.min(mrd)]
      c<-model$cluster[j]
      b<-model$birth[as.character(model$selected[c])]
      limit<-if(is.finite(b)&&b>0) 1/b else Inf
      out[i]<-if(min(mrd)<=limit) c else 0L
    }
  }
  out
}

# ---- Results helpers
# cluster labels as a factor: 1, 2, ... and "Noise"
#' @export
dbs_factor<-function(cl){
  lv<-sort(unique(cl[cl>0]))
  f<-ifelse(cl==0,"Noise",as.character(cl))
  factor(f,levels=c(as.character(lv),if(any(cl==0)) "Noise"))
}

# noise points assigned to the cluster of their nearest clustered point
#' @export
dbs_assign_noise<-function(prep,cl){
  noise<-which(cl==0)
  ok<-which(cl>0)
  if(!length(noise)||!length(ok)) return(cl)
  for(i in noise){
    d<-dbs_dist_row(prep$X,i,prep$metric,prep$is_dist)[ok]
    cl[i]<-cl[ok[which.min(d)]]
  }
  cl
}

# clusters of all the neurons of a SOM: the neurons left out of the clustering (empty, no
# observations) take the cluster of the nearest clustered neuron (codebook distance), for
# the map only
#' @export
dbs_neuron_clusters<-function(prep,cl,m){
  nn<-if(is.null(prep$n_neurons)) length(cl) else prep$n_neurons
  keep<-if(is.null(prep$neurons)) seq_len(nn) else prep$neurons
  full<-integer(nn)
  full[keep]<-cl
  emp<-setdiff(seq_len(nn),keep)
  if(length(emp)&&any(cl>0)){
    D<-as.matrix(kohonen::object.distances(m,"codes"))
    lab<-keep[cl>0]
    full[emp]<-full[lab[apply(D[emp,lab,drop=FALSE],1,which.min)]]
  }
  full
}

#' @export
dbs_summary<-function(model,prep){
  cl<-model$cluster
  ids<-sort(unique(cl[cl>0]))
  tab<-data.frame(Cluster=c(as.character(ids),if(any(cl==0)) "Noise"),
                  n=c(vapply(ids,function(k) sum(cl==k),integer(1)),if(any(cl==0)) sum(cl==0)),stringsAsFactors=FALSE)
  w<-prep$w
  if(is.null(w)){
    tab$Percent<-round(100*tab$n/length(cl),1)
  } else{
    # SOM neurons weighted by their hits: sizes in observations
    colnames(tab)[2]<-"Neurons"
    tab$Observations<-c(vapply(ids,function(k) sum(w[cl==k]),numeric(1)),if(any(cl==0)) sum(w[cl==0]))
    tab$Percent<-round(100*tab$Observations/sum(w),1)
  }
  if(identical(model$method,"hdbscan")&&length(ids)){
    tab$Stability<-c(round(as.numeric(model$stability),3),if(any(cl==0)) NA)
    tab$Mean_membership<-c(vapply(ids,function(k) round(mean(model$prob[cl==k]),3),numeric(1)),if(any(cl==0)) NA)
    tab$Mean_GLOSH<-c(vapply(ids,function(k) round(mean(model$glosh[cl==k]),3),numeric(1)),if(any(cl==0)) round(mean(model$glosh[cl==0]),3))
  }
  tab
}

# 2-D view of the observations: PCA (Euclidean) or PCoA of the chosen distance
#' @export
dbs_ordination<-function(prep,max_n=3000){
  X<-prep$X
  if(!is.null(prep$reduced)&&ncol(X)>=2){
    out<-data.frame(Dim.1=X[,1],Dim.2=X[,2])
    attr(out,"labels")<-paste0(prep$reduced$method,1:2," (",round(100*prep$reduced$axis_var,1),"%)")
    return(out)
  }
  if(!prep$is_dist&&identical(prep$metric,"euclidean")){
    keep<-apply(X,2,stats::sd)>0
    X<-X[,keep,drop=FALSE]
    if(ncol(X)==1) return(data.frame(Dim.1=X[,1],Dim.2=0,row.names=rownames(X),check.names=FALSE))
    pc<-stats::prcomp(X)
    v<-round(100*summary(pc)$importance[2,1:2],1)
    out<-data.frame(pc$x[,1:2,drop=FALSE])
    colnames(out)<-c("Dim.1","Dim.2")
    attr(out,"labels")<-paste0(c("PC1","PC2")," (",v,"%)")
    return(out)
  }
  n<-prep$n
  validate(need(n<=max_n,paste0("The PCoA view of non-Euclidean distances is limited to ",max_n," observations.")))
  D<-if(prep$is_dist) prep$X else t(vapply(seq_len(n),function(i) dbs_dist_row(prep$X,i,prep$metric,FALSE),numeric(n)))
  co<-suppressWarnings(stats::cmdscale(stats::as.dist(D),k=2,eig=TRUE))
  ev<-co$eig[co$eig>0]
  v<-round(100*co$eig[1:2]/sum(ev),1)
  out<-data.frame(Dim.1=co$points[,1],Dim.2=co$points[,2])
  attr(out,"labels")<-paste0(c("PCoA1","PCoA2")," (",v,"%)")
  out
}

#' @export
gg_dbs_scatter<-function(ord,clusters,colors,hulls=TRUE,point_size=2,base_size=12,title="",outliers=NULL){
  df<-data.frame(ord,cluster=clusters)
  out_layer<-NULL
  if(any(outliers%in%TRUE)){
    # GLOSH outliers circled
    out_layer<-list(ggplot2::geom_point(data=df[outliers%in%TRUE,,drop=FALSE],mapping=ggplot2::aes(x=Dim.1,y=Dim.2,shape="GLOSH outlier"),
                                        color="black",size=point_size*2.2,stroke=0.7,inherit.aes=FALSE),
                    ggplot2::scale_shape_manual(values=c("GLOSH outlier"=1),name=NULL))
  }
  lab<-attr(ord,"labels")%||%c("Dim 1","Dim 2")
  p<-ggplot2::ggplot(df,ggplot2::aes(x=Dim.1,y=Dim.2,color=cluster,fill=cluster))
  if(isTRUE(hulls)){
    hl<-do.call(rbind,lapply(split(df[df$cluster!="Noise",,drop=FALSE],df$cluster[df$cluster!="Noise"],drop=TRUE),function(s){
      if(nrow(s)<3) return(NULL)
      s[grDevices::chull(s$Dim.1,s$Dim.2),,drop=FALSE]
    }))
    if(!is.null(hl)) p<-p+ggplot2::geom_polygon(data=hl,alpha=0.15,linewidth=0.3)
  }
  p+ggplot2::geom_point(data=df[df$cluster=="Noise",,drop=FALSE],shape=4,size=point_size*0.8)+
    ggplot2::geom_point(data=df[df$cluster!="Noise",,drop=FALSE],size=point_size,alpha=0.85)+
    out_layer+
    ggplot2::scale_color_manual(values=colors,name="Cluster",drop=FALSE)+
    ggplot2::scale_fill_manual(values=colors,name="Cluster",drop=FALSE)+
    ggplot2::labs(x=lab[1],y=lab[2],title=title)+ggplot2::theme_bw(base_size=base_size)
}

# palette with grey for the noise level
dbs_colors<-function(f,pal_fun){
  lv<-levels(f)
  k<-sum(lv!="Noise")
  cols<-if(k>0) pal_fun(k) else character(0)
  if("Noise"%in%lv) cols<-c(cols,"gray75")
  stats::setNames(cols,lv)
}

#' @export
gg_dbs_grid<-function(g,base_size=11){
  long<-rbind(data.frame(g[,c("eps","minPts")],measure="Clusters",value=g$clusters,label=g$clusters),
              data.frame(g[,c("eps","minPts")],measure="Noise (%)",value=g$noise,label=round(g$noise,1)),
              data.frame(g[,c("eps","minPts")],measure="DBCV",value=g$dbcv,label=round(g$dbcv,2)))
  long$measure<-factor(long$measure,levels=c("Clusters","Noise (%)","DBCV"))
  long$eps<-factor(signif(long$eps,3))
  long$minPts<-factor(long$minPts)
  ggplot2::ggplot(long,ggplot2::aes(x=eps,y=minPts,fill=value))+
    ggplot2::geom_tile(color="white")+
    ggplot2::geom_text(ggplot2::aes(label=label),size=base_size/3.3)+
    ggplot2::facet_wrap(~measure,scales="free")+
    ggplot2::scale_fill_gradient(low="#f7fbff",high="#6baed6",guide="none",na.value="gray90")+
    ggplot2::labs(x="eps",y="minPts",caption="Good choices: stable regions (the same number of clusters over neighbouring eps values) with high DBCV.")+
    ggplot2::theme_minimal(base_size=base_size)
}

# clusters, noise and DBCV along one parameter (min cluster size or number of axes)
#' @export
gg_dbs_curve<-function(s,x,xlab,caption,base_size=11){
  long<-rbind(data.frame(x=s[[x]],measure="Clusters",value=s$clusters),
              data.frame(x=s[[x]],measure="Noise (%)",value=s$noise),
              data.frame(x=s[[x]],measure="DBCV",value=s$dbcv))
  long$measure<-factor(long$measure,levels=c("Clusters","Noise (%)","DBCV"))
  ggplot2::ggplot(long[!is.na(long$value),,drop=FALSE],ggplot2::aes(x=x,y=value,group=measure))+
    ggplot2::geom_line(color="#05668D")+ggplot2::geom_point(color="#05668D",size=2)+
    ggplot2::facet_wrap(~measure,scales="free_y")+
    ggplot2::labs(x=xlab,y=NULL,caption=caption)+
    ggplot2::theme_bw(base_size=base_size)
}
#' @export
gg_dbs_sizes<-function(s,base_size=11){
  gg_dbs_curve(s,"min_cluster_size","Minimum cluster size","Plateaus (the same number of clusters over a range of sizes) with high DBCV indicate a stable solution.",base_size)
}
#' @export
gg_dbs_axes<-function(s,base_size=11){
  gg_dbs_curve(s,"axes","Number of principal axes","Each number of axes with its suggested minPts (and eps). DBCV values from different numbers of axes are only roughly comparable.",base_size)
}

# condensed tree of HDBSCAN (icicle plot): each cluster is a bar whose width is the number of
# its points (or weights) still in the cluster as lambda = 1 / distance increases; the selected
# clusters are coloured
#' @export
gg_dbs_ctree<-function(model,colors=NULL,base_size=12,title="",relabel=NULL){
  ct<-model$condensed
  n<-length(model$cluster)
  cr<-ct[ct$child>n,,drop=FALSE]
  clusters<-sort(unique(ct$parent))
  root<-min(clusters)
  size<-stats::setNames(numeric(length(clusters)),clusters)
  size[as.character(root)]<-sum(ct$size[ct$parent==root&ct$child<=n])+sum(cr$size[cr$parent==root])
  size[as.character(cr$child)]<-cr$size
  birth<-stats::setNames(rep(0,length(clusters)),clusters)
  birth[as.character(cr$child)]<-cr$lambda
  kids<-split(cr$child,cr$parent)
  xc<-stats::setNames(numeric(length(clusters)),clusters)
  place<-function(c,mid){
    xc[as.character(c)]<<-mid
    ch<-kids[[as.character(c)]]
    if(!length(ch)) return(invisible())
    tot<-sum(size[as.character(ch)])
    gap<-0.05*size[[as.character(c)]]
    cur<-mid-(tot+gap*(length(ch)-1))/2
    for(k in ch){
      s<-size[[as.character(k)]]
      place(k,cur+s/2)
      cur<-cur+s+gap
    }
  }
  place(root,0)
  sel<-model$selected
  lab<-stats::setNames(seq_along(sel),sel)
  # sorted clusters: new number of each cluster (relabel names: original numbers)
  if(!is.null(relabel)) lab[]<-as.integer(relabel[as.character(lab)])
  rects<-do.call(rbind,lapply(clusters,function(c){
    r<-ct[ct$parent==c,,drop=FALSE]
    end<-max(r$lambda)
    pts<-r[r$child<=n,,drop=FALSE]
    pts<-pts[order(pts$lambda),,drop=FALSE]
    brk<-sort(unique(c(birth[[as.character(c)]],pts$lambda[pts$lambda<end],end)))
    rem<-size[[as.character(c)]]-vapply(brk[-length(brk)],function(l) sum(pts$size[pts$lambda<=l&pts$lambda>birth[[as.character(c)]]]),numeric(1))
    if(length(brk)<2) return(NULL)
    data.frame(cluster=c,xmin=xc[[as.character(c)]]-rem/2,xmax=xc[[as.character(c)]]+rem/2,ymin=brk[-length(brk)],ymax=brk[-1],
               selected=if(c%in%sel) as.character(lab[[as.character(c)]]) else "not selected")
  }))
  rects<-rects[rects$xmax>rects$xmin,,drop=FALSE]
  links<-do.call(rbind,lapply(names(kids),function(p){
    ch<-kids[[p]]
    data.frame(x=min(xc[as.character(ch)]),xend=max(xc[as.character(ch)]),y=birth[[as.character(ch[1])]])
  }))
  lv<-c(as.character(seq_along(sel)),"not selected")
  cols<-c(if(length(sel)) (if(is.null(colors)) grDevices::hcl.colors(length(sel)) else unname(colors)[seq_along(sel)]),"gray80")
  rects$selected<-factor(rects$selected,levels=lv)
  labs_df<-do.call(rbind,lapply(sel,function(c) data.frame(x=xc[[as.character(c)]],y=birth[[as.character(c)]],label=lab[[as.character(c)]])))
  p<-ggplot2::ggplot()+
    ggplot2::geom_rect(data=rects,ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=selected),color=NA)
  if(!is.null(links)) p<-p+ggplot2::geom_segment(data=links,ggplot2::aes(x=x,xend=xend,y=y,yend=y),color="gray40",linewidth=0.4)
  if(!is.null(labs_df)) p<-p+ggplot2::geom_label(data=labs_df,ggplot2::aes(x=x,y=y,label=label),size=base_size/3.5,label.padding=ggplot2::unit(0.12,"lines"))
  p+ggplot2::scale_fill_manual(values=stats::setNames(cols,lv),name="Cluster",drop=FALSE)+
    ggplot2::scale_y_reverse()+
    ggplot2::labs(x=NULL,y="lambda = 1 / mutual reachability distance",title=title,
                  caption="Bar width: points still in the cluster. Long bars (large stability) are robust clusters; coloured: selected clusters.")+
    ggplot2::theme_bw(base_size=base_size)+
    ggplot2::theme(axis.text.x=ggplot2::element_blank(),axis.ticks.x=ggplot2::element_blank(),panel.grid.major.x=ggplot2::element_blank(),panel.grid.minor.x=ggplot2::element_blank())
}

# full HDBSCAN hierarchy as an 'hclust' object: single linkage of the mutual reachability
# distances (heights), leaves in the order of the tree
#' @export
dbs_hclust<-function(model,labels=NULL){
  sl<-model$tree$sl
  n<-sl$n
  merge<-cbind(ifelse(sl$left<=n,-sl$left,sl$left-n),ifelse(sl$right<=n,-sl$right,sl$right-n))
  ord<-integer(0)
  stack<-2L*n-1L
  while(length(stack)){
    v<-stack[length(stack)]
    stack<-stack[-length(stack)]
    if(v<=n) ord<-c(ord,v) else stack<-c(stack,sl$right[v-n],sl$left[v-n])
  }
  if(is.null(labels)) labels<-as.character(seq_len(n))
  structure(list(merge=merge,height=sl$height,order=ord,labels=labels,method="single (HDBSCAN mutual reachability)",
                 dist.method="mutual reachability"),class="hclust")
}

# dendrogram of the HDBSCAN hierarchy in the style of the HC module: each branch takes the
# colour of its cluster when all its leaves (noise aside) belong to that cluster; branches
# joining clusters are black and branches of noise only are grey; a label marks the top of
# each cluster. The clusters are not a horizontal cut (each one is selected at its own height),
# so the branches are coloured from the leaves instead of by cutree as in the HC module.
#' @export
gg_dbs_dendrogram<-function(model,ids,colors,base_size=12,title="",labels=NULL,lwd=0.6){
  n<-length(model$cluster)
  if(length(ids)!=n) ids<-as.character(seq_len(n))
  hc<-dbs_hclust(model,ids)
  if(is.null(labels)) labels<-n<=200
  cl<-model$cluster
  xleaf<-numeric(n)
  xleaf[hc$order]<-seq_len(n)
  H<-hc$height
  nodex<-numeric(n-1)
  nodecl<-integer(n-1)
  # cluster of a branch: the same cluster for all its leaves (noise ignored); 0 noise only; -1 mixed
  join<-function(a,b) if(a==b) a else if(a==-1||b==-1) -1L else if(a==0) b else if(b==0) a else -1L
  seg<-vector("list",n-1)
  for(i in seq_len(n-1)){
    ch<-hc$merge[i,]
    info<-lapply(ch,function(j) if(j<0) c(x=xleaf[-j],y=0,c=cl[-j]) else c(x=nodex[j],y=H[j],c=nodecl[j]))
    nodex[i]<-mean(c(info[[1]]["x"],info[[2]]["x"]))
    nodecl[i]<-join(as.integer(info[[1]]["c"]),as.integer(info[[2]]["c"]))
    seg[[i]]<-data.frame(x=c(info[[1]]["x"],info[[2]]["x"],info[[1]]["x"]),
                         y=c(info[[1]]["y"],info[[2]]["y"],H[i]),
                         xend=c(info[[1]]["x"],info[[2]]["x"],info[[2]]["x"]),
                         yend=c(H[i],H[i],H[i]),
                         c=c(info[[1]]["c"],info[[2]]["c"],nodecl[i]))
  }
  seg<-do.call(rbind,seg)
  k<-sort(unique(cl[cl>0]))
  lv<-c(as.character(k),"Noise","Between clusters")
  cols<-c(stats::setNames(unname(colors)[seq_along(k)],as.character(k)),Noise="gray70","Between clusters"="black")
  seg$group<-factor(ifelse(seg$c==-1,"Between clusters",ifelse(seg$c==0,"Noise",as.character(seg$c))),levels=lv)
  # label at the top of each cluster
  tops<-do.call(rbind,lapply(k,function(g){
    i<-which(nodecl==g)
    if(!length(i)) return(NULL)
    i<-i[which.max(H[i])]
    data.frame(x=nodex[i],y=H[i],label=as.character(g),group=factor(as.character(g),levels=lv))
  }))
  p<-ggplot2::ggplot()+
    ggplot2::geom_segment(data=seg,ggplot2::aes(x=x,y=y,xend=xend,yend=yend,color=group),linewidth=lwd,lineend="square")
  if(!is.null(tops)) p<-p+ggplot2::geom_label(data=tops,ggplot2::aes(x=x,y=y,label=label,color=group),size=base_size/3.2,fill="white",show.legend=FALSE)
  if(isTRUE(labels)){
    lab<-data.frame(x=seq_len(n),label=hc$labels[hc$order])
    p<-p+ggplot2::geom_text(data=lab,ggplot2::aes(x=x,y=-0.01*max(H),label=label),angle=90,hjust=1,size=base_size/12*1.6,color="gray30")+
      ggplot2::coord_cartesian(clip="off")
  }
  p+ggplot2::scale_color_manual(values=cols,name="Cluster",drop=TRUE)+
    ggplot2::labs(x=if(isTRUE(labels)) NULL else paste0(n," leaves"),y="Mutual reachability distance",title=if(nzchar(title)) title else "HDBSCAN hierarchy",
                  caption=paste0("Single linkage of the mutual reachability distances.","\n","Each cluster is selected at its own height (stability), not by a horizontal cut."))+
    ggplot2::theme_minimal(base_size=base_size)+
    ggplot2::theme(axis.text.x=ggplot2::element_blank(),panel.grid.major.x=ggplot2::element_blank(),panel.grid.minor.x=ggplot2::element_blank(),
                   plot.margin=ggplot2::margin(5.5,5.5,if(isTRUE(labels)) 30 else 5.5,5.5))
}

# ---- Module
#' @export
dbscan_module<-list()
#' @export
dbscan_module$ui<-function(id){
  module_progress("Loading module: Density-based clustering")
  ns<-NS(id)
  div(
    h4("Density-based clustering (DBSCAN / HDBSCAN)",
       tiphelp_icon(actionLink(ns("help"),label=NULL,icon=icon("fas fa-question-circle"),style="margin-left: 8px; font-size: 14px"),"Click for more details","right"),
       class="imesc_title"),
    box_caret(
      ns("box_setup"),title="Model setup",color="#374061ff",inline=F,
      div(
        # labels above the inputs, as in the Model Setup of the SOM module
        div(style="display: flex; column-gap: 20px; flex-flow: wrap; align-items: flex-start",
            pickerInput_fromtop(ns("data_db"),"~ Training Datalist:",choices=NULL,width="250px",options=shinyWidgets::pickerOptions(liveSearch=TRUE)),
            radioButtons(ns("target"),"Clustering target:",choices=c("Numeric-Attribute"="data"),width="170px"),
            div(id=ns("som_box"),style="display: flex; column-gap: 20px; align-items: flex-start",
                pickerInput_fromtop(ns("som_model"),"SOM model:",choices=NULL,width="180px"),
                div(style="padding-top: 25px",
                    checkboxInput(ns("som_weight"),tiphelp5("Weight neurons by hits","The SOM spreads its neurons over the data space, so the density of neurons does not follow the density of the observations. Each neuron counts with its number of observations (hits): minPts and the min cluster size are then numbers of observations, and the empty neurons (no observations) are left out of the clustering (on the map they take the cluster of the nearest clustered neuron): in simulations they bridged separate groups."),value=TRUE,width="auto"))),
            pickerInput_fromtop(ns("method"),"Method:",choices=c("HDBSCAN"="hdbscan","DBSCAN"="dbscan"),selected="hdbscan",width="150px"),
            div(id=ns("metric_box"),style="display: flex; column-gap: 20px; align-items: flex-start",
                pickerInput_fromtop(ns("metric"),tiphelp5("Distance:","Euclidean and Manhattan for continuous variables (scale them when they have different units); Bray-Curtis for abundances (non-negative); Jaccard for presence/absence (values > 0 are presences). With a SOM codebook, the distances between neurons of the SOM model are used."),
                                    choices=c("Euclidean"="euclidean","Manhattan"="manhattan","Bray-Curtis"="bray","Jaccard (presence/absence)"="jaccard"),width="200px"),
                div(style="padding-top: 25px",
                    checkboxInput(ns("scale"),tiphelp5("Scale","Standardise the variables (mean 0, SD 1) before computing distances. Recommended for Euclidean/Manhattan when the variables have different units."),value=FALSE,width="auto"))),
            # principal axes: of the data, or of the codebook vectors of a (single-layer) SOM
            div(style="display: flex; column-gap: 20px; align-items: flex-start",
                div(style="padding-top: 25px",
                    checkboxInput(ns("reduce"),tiphelp5("Use principal axes","With many variables all distances become similar (curse of dimensionality) and density-based clustering finds one large cluster or only noise. The clustering is then run on the first axes of a PCA (Euclidean distance) or of a PCoA (other distances); for a SOM codebook, on the first axes of a PCA of the codebook vectors (single-layer SOM). Checked by default when there are more than 10 variables."),value=TRUE,width="auto")),
                div(id=ns("axes_box"),style="display: flex; column-gap: 12px; align-items: flex-start",
                    pickerInput_fromtop(ns("axes_rule"),tiphelp5("Axes rule:","How many principal axes are kept (between 2 and 10). Broken-stick (default): the axes explaining more variance than expected if the variance were split at random (Jackson 1993; Legendre & Legendre 2012); in simulations with many noise variables it kept the axes with the groups, while the 50% rule often added noise axes. 50% of the variance: the smallest number of axes explaining half of the variance. Check the choice in Parameter guide > Sensitivity > Number of axes."),
                                        choices=c("Broken-stick"="bstick","50% of the variance"="var50"),selected="bstick",width="170px"),
                    numericInput(ns("n_axes"),tiphelp5("Axes:","Number of principal axes used. Empty: given by the Axes rule."),value=NA,min=1,step=1,width="90px")))
        ),
        uiOutput(ns("setup_info"))
      )
    ),
    tabsetPanel(
      id=ns("tabs"),
      tabPanel(
        "1. Parameters",value="tab1",
        column(4,class="mp0",
               box_caret(ns("box_params"),title="Parameters",color="#c3cc74ff",
                         div(class="dbs_params",
                           tags$style(HTML(".dbs_params .form-group{display: block !important; margin-bottom: 6px} .dbs_params .control-label{display: block} .dbs_params input[type=number]{max-width: 120px}")),
                           numericInput(ns("minPts"),tiphelp5("minPts","Number of points (the point itself included) that must lie within the neighbourhood of a point for it to be a core point. Larger values give smoother density estimates, fewer and larger clusters, and more noise."),value=5,min=2,step=1),
                           uiOutput(ns("minpts_hint")),
                           div(id=ns("dbscan_box"),
                               div(style="display: flex; gap: 8px; align-items: flex-end",
                                   numericInput(ns("eps"),tiphelp5("eps","Radius of the neighbourhood, in units of the chosen distance. Points closer than eps are neighbours. Suggest eps runs DBSCAN over a range of eps values (quantiles of the k-distances) and picks the one with the highest DBCV (density-based validity index) among the solutions with at least 2 clusters; it also reports the range of eps with the same number of clusters (stability). The knee of the k-distance plot is used when no eps separates 2 or more clusters."),value=0.5,min=0,step=0.05,width="130px"),
                                   actionButton(ns("suggest_eps"),"Suggest eps",style="height: 30px; margin-bottom: 6px")),
                               uiOutput(ns("eps_note"))),
                           div(id=ns("hdbscan_box"),
                               numericInput(ns("mcs"),tiphelp5("Min cluster size","Smallest group of points accepted as a cluster. Smaller groups are noise or part of larger clusters. Suggested: the square root of n (at least minPts); with min cluster size = minPts, small groups of outliers tend to become clusters. Check the Sensitivity curve."),value=5,min=2,step=1),
                               uiOutput(ns("mcs_hint")),
                               pickerInput_fromtop(ns("selection"),tiphelp5("Cluster selection:","Excess of mass (EOM, default): keeps the most stable clusters of the hierarchy, often a few large clusters. Leaf: keeps the clusters at the tips of the condensed tree, giving more and smaller homogeneous clusters; useful when EOM returns one large cluster."),
                                                   choices=c("Excess of mass (EOM)"="eom","Leaf"="leaf"),width="200px"),
                               numericInput(ns("eps_sel"),tiphelp5("Selection epsilon","Cluster selection epsilon (Malzer & Baum 2020): clusters that split below this distance are merged back into their parent, so small differences in density do not create many micro-clusters. 0: off. In units of the distance used (see the k-distance plot)."),value=0,min=0,step=0.05),
                               checkboxInput(ns("allow_single"),tiphelp5("Allow a single cluster","Allow the result to be one single cluster (by default HDBSCAN looks for at least two)."),value=FALSE)),
                           div(class="save_changes",id=ns("run_btn"),align="right",actionButton(ns("run"),"RUN >>"))
                         ))),
        column(8,class="mp0",
               box_caret(ns("box_guide"),title="Parameter guide",
                         button_title2=radioGroupButtons(ns("guide"),NULL,c("k-distance"="kdist","Sensitivity"="sens","Condensed tree"="ctree")),
                         div(uiOutput(ns("guide_ui")))))
      ),
      tabPanel(
        "2. Results",value="tab2",
        # views of the result (as the HC module): navigation tabs only; the plot box and the
        # common plot options (palette, base size, title) are shared by the views
        div(style="padding: 4px 0px 6px 0px",
            tabsetPanel(id=ns("plot_type"),selected="clusters",
                        tabPanel("Clusters",value="clusters"),
                        tabPanel("Condensed tree",value="ctree"),
                        tabPanel("Dendrogram",value="dendro"))),
        column(4,class="mp0",
               box_caret(ns("box_result"),title="Result",color="#c3cc74ff",
                         div(
                           # trained (unsaved) model and the models saved in the Datalist
                           div(style="display: flex; gap: 6px; align-items: flex-end; margin-bottom: 6px",
                               div(style="flex: 1 1 auto; min-width: 0",
                                   pickerInput_fromtop(ns("dbs_model"),tiphelp5("Model:","The model just trained (unsaved) or a model saved in the training Datalist. Saved models keep the setup, the parameters and the clusters; they can be renamed or deleted in Pre-processing tools > Rename models / Datalist manager."),choices=NULL,width="100%")),
                               div(id=ns("save_model_btn"),class="save_changes",
                                   actionButton(ns("save_model"),icon("fas fa-save"),title="Save the model in the Datalist",style="height: 30px; padding: 3px 10px")),
                               div(id=ns("delete_model_btn"),
                                   actionButton(ns("delete_model"),icon("fas fa-trash"),title="Delete the saved model",style="height: 30px; padding: 3px 10px"))),
                           uiOutput(ns("summary")),
                           checkboxInput(ns("assign_noise"),tiphelp5("Assign noise to the nearest cluster","Noise points (not dense enough to belong to any cluster) are given the cluster of their nearest clustered point. Only for the saved/plotted labels; keep them as Noise when outliers matter."),value=FALSE),
                           checkboxInput(ns("sort_clusters"),tiphelp5("Sort clusters","Renumber the clusters by the mean of a numeric variable of their observations (ascending: cluster 1 has the lowest mean), as in the HC module. Applies to the table, the plots and the saved clusters; noise is not renumbered."),value=FALSE),
                           div(id=ns("sort_box"),style="display: flex; gap: 8px",
                               div(style="flex: 1; min-width: 0",pickerInput_fromtop(ns("sort_datalist"),"Datalist:",choices=NULL,width="100%")),
                               div(style="flex: 1; min-width: 0",pickerInput_fromtop(ns("sort_var"),"Variable:",choices=NULL,width="100%",options=shinyWidgets::pickerOptions(liveSearch=TRUE)))),
                           div(id=ns("save_clusters_btn"),class="save_changes",actionButton(ns("save_clusters"),span(icon("fas fa-save")," Save clusters in the Factor-Attribute"))),
                           uiOutput(ns("clusters_saved_note"))
                         )),
               box_caret(ns("box_plotopts"),title="Plot options",color="#c3cc74ff",
                         div(
                           pickerInput_fromtop_live(ns("palette"),"Palette:",choices=NULL),
                           div(id=ns("data_opts"),
                               checkboxInput(ns("hulls"),"Convex hulls",value=TRUE),
                               numericInput(ns("pt_size"),"Point size:",value=2,min=0.1,step=0.2),
                               div(id=ns("glosh_box"),
                                   checkboxInput(ns("mark_glosh"),tiphelp5("Mark GLOSH outliers","GLOSH outlier score (Campello et al. 2015, from 0 to 1): how much a point lies outside the densest part of its cluster. The points with scores above the chosen quantile are circled."),value=FALSE),
                                   numericInput(ns("glosh_q"),"GLOSH quantile:",value=0.9,min=0.5,max=0.999,step=0.01))),
                           div(id=ns("som_opts"),
                               pickerInput_fromtop_live(ns("border"),"Border:",choices=NULL),
                               checkboxInput(ns("som_points"),"Observations",value=TRUE),
                               pickerInput_fromtop_live(ns("points_palette"),"Points palette:",choices=NULL),
                               pickerInput_fromtop(ns("points_factor"),"Points factor:",choices=NULL),
                               numericInput(ns("som_pt_size"),"Point size:",value=1,min=0.1,step=0.1)),
                           numericInput(ns("base_size"),"Base size:",value=12,min=6,step=1),
                           textInput(ns("title"),"Title:",value="")
                         ))),
        column(8,class="mp0",
               box_caret(ns("box_plot"),title="Plot",
                         button_title=actionLink(ns("down_plot"),"Download",icon("download")),
                         div(uiOutput(ns("plot_note")),plotOutput(ns("plot"),height="520px"))))
      ),
      tabPanel(
        "3. Predict",value="tab3",
        column(4,class="mp0",
               box_caret(ns("box_pred"),title="New data",color="#c3cc74ff",
                         div(
                           pickerInput_fromtop(ns("new_data"),tiphelp5("Datalist:","Datalists with all the variables used in the training."),choices=NULL),
                           div(class="save_changes",align="right",actionButton(ns("run_pred"),"Predict >>")),
                           uiOutput(ns("pred_info"))
                         ))),
        column(8,class="mp0",
               box_caret(ns("box_pred_res"),title="Predictions",
                         div(uiOutput(ns("pred_note")),tableOutput(ns("pred_table")))))
      )
    )
  )
}

#' @export
dbscan_module$server<-function(id,vals){
  moduleServer(id,function(input,output,session){
    ns<-session$ns
    for(b in c("box_setup","box_params","box_guide","box_result","box_plotopts","box_plot","box_pred","box_pred_res")) box_caret_server(b)
    first_or<-function(sel,choices) if(length(sel)==1&&sel%in%choices) sel else unname(choices[1])

    observeEvent(input$help,{
      showModal(modalDialog(
        title="Density-based clustering",easyClose=TRUE,footer=modalButton("Close"),size="l",
        div(style="line-height: 1.45; font-size: 13px;",
            p("Density-based methods find groups of points separated by regions of low density. They do not need the number of clusters, find clusters of any shape and leave isolated points as ",strong("noise"),"."),
            div(style="border-left: 5px solid #05668D; background: #fafafa; padding: 8px 12px; margin-bottom: 10px;",
                h4("DBSCAN",style="margin-top: 0; color: #05668D;"),
                p("A point is a ",strong("core point")," when at least ",strong("minPts")," points (itself included) lie within distance ",strong("eps"),". Core points closer than eps are joined into the same cluster; non-core points within eps of a core point are ",em("border")," points of its cluster; the others are noise."),
                p(strong("Choosing eps:")," the k-distance plot sorts the distance of every point to its k-th nearest neighbour (k = minPts - 1). Points inside clusters have small distances and noise has large ones. The knee of the curve is the classic choice, but it often joins groups that are not separated by a clear low-density gap. ",strong("Suggest eps")," runs DBSCAN for eps values along the curve and keeps the one with the highest ",strong("DBCV"),"; the green band shows the range of eps giving the same number of clusters (a wide band means a stable solution)."),
                p(strong("Limitation:")," one eps for the whole data: clusters of very different densities are hard to separate.")),
            div(style="border-left: 5px solid #2f6f3e; background: #fafafa; padding: 8px 12px; margin-bottom: 10px;",
                h4("HDBSCAN",style="margin-top: 0; color: #2f6f3e;"),
                p("Runs DBSCAN for all eps values at once and keeps the most stable clusters. The core distance of a point is the distance to its (minPts - 1)-th neighbour; the mutual reachability distance between a and b is max(core(a), core(b), d(a, b)). The minimum spanning tree of these distances gives a hierarchy of clusters; branches smaller than the ",strong("min cluster size")," are treated as points falling out of their parent cluster (the ",strong("condensed tree"),"). Each cluster gets a ",strong("stability")," (how long its points stay together as eps decreases)."),
                p(strong("Cluster selection:")," Excess of mass (EOM) keeps the set of clusters with the largest total stability; ",em("Leaf")," keeps the tips of the condensed tree (more, smaller clusters). The ",strong("selection epsilon")," (Malzer & Baum 2020) merges clusters that split below that distance."),
                p("The ",strong("membership")," (0-1) shows how strongly each point belongs to its cluster. The ",strong("GLOSH")," outlier score (Campello et al. 2015, 0-1) shows how far a point lies from the densest part of its cluster: high scores are outliers, also inside clusters.")),
            div(style="border-left: 5px solid #6a3d9a; background: #fafafa; padding: 8px 12px; margin-bottom: 10px;",
                h4("DBCV",style="margin-top: 0; color: #6a3d9a;"),
                p("Density-Based Clustering Validation (Moulavi et al. 2014), from -1 to 1. For each cluster it compares the largest gap inside the cluster (the longest edge of the minimum spanning tree of the mutual reachability distances among its points) with the smallest gap to the other clusters. Each cluster counts with its share of all the points, so noise lowers the index. Unlike the silhouette, it does not favour round clusters or solutions that drop most points as noise."),
                p("Rough guide: above 0.5 strong separation by density; 0.2-0.5 moderate; below 0.2 weak (the data may form one dense group).")),
            div(style="border-left: 5px solid #8a6d3b; background: #fafafa; padding: 8px 12px; margin-bottom: 10px;",
                h4("minPts and principal axes",style="margin-top: 0; color: #8a6d3b;"),
                p("Rule of thumb (Sander et al. 1998; Schubert et al. 2017): minPts = 2 x number of dimensions, at least 4 (for DBSCAN also at least ln n), and at most about n/10. Min cluster size: the square root of n (at least minPts). Suggest eps keeps only solutions whose clusters all have at least that size. These defaults were chosen with simulated data with known groups and outliers (blobs, different densities, non-convex shapes, unequal sizes, many noise variables, community data and data without groups)."),
                p("With many variables all distances become similar (curse of dimensionality). ",strong("Use principal axes")," runs the clustering on the first axes of a PCA (Euclidean) or a PCoA (other distances); the number of axes comes from the Axes rule (broken-stick model by default, or 50% of the variance) or is given. New observations are projected onto the same axes for prediction (PCoA: Gower's add-a-point formula). Sensitivity > Number of axes shows how the result depends on this choice.")),
            div(style="border-left: 5px solid #555555; background: #fafafa; padding: 8px 12px;",
                h4("SOM codebook",style="margin-top: 0; color: #555555;"),
                p("The neurons of the SOM are clustered and the observations take the cluster of their best-matching neuron. The SOM spreads its neurons over the data space, so the density of neurons is not the density of the data: with ",strong("Weight neurons by hits"),", each neuron counts with its number of observations (minPts and the min cluster size become numbers of observations).")))
      ))
    })

    # ---- setup
    observeEvent(vals$saved_data,{
      ch<-names(vals$saved_data)
      updatePickerInput(session,"data_db",choices=ch,selected=first_or(isolate(input$data_db)%||%vals$cur_data,ch))
    })
    data<-reactive({
      req(input$data_db%in%names(vals$saved_data))
      vals$saved_data[[input$data_db]]
    })
    observeEvent(data(),{
      soms<-names(attr(data(),"som"))
      ch<-c("Numeric-Attribute"="data",if(length(soms)) c("SOM codebook"="som"))
      updateRadioButtons(session,"target",choices=ch,selected=first_or(isolate(input$target),ch))
      updatePickerInput(session,"som_model",choices=soms,selected=first_or(isolate(input$som_model),soms))
      fac<-attr(data(),"factors")
      updatePickerInput(session,"points_factor",choices=c("None",colnames(fac)),selected=first_or(isolate(input$points_factor)%||%"None",c("None",colnames(fac))))
      ch_new<-names(vals$saved_data)[vapply(vals$saved_data,function(d) all(colnames(data())%in%colnames(d)),logical(1))]
      updatePickerInput(session,"new_data",choices=ch_new,selected=first_or(isolate(input$new_data),ch_new))
    })
    # a new training Datalist clears the model (saving clusters in the Factor-Attribute does not)
    observeEvent(input$data_db,ignoreInit=TRUE,{
      result(NULL)
      pred(NULL)
    })
    observeEvent(vals$newcolhabs,{
      updatePickerInput(session,"palette",choices=vals$colors_img$val,choicesOpt=list(content=vals$colors_img$img),selected=first_or(isolate(input$palette)%||%"turbo",vals$colors_img$val))
      updatePickerInput(session,"points_palette",choices=vals$colors_img$val,choicesOpt=list(content=vals$colors_img$img),selected=first_or(isolate(input$points_palette)%||%"black",vals$colors_img$val))
      updatePickerInput(session,"border",choices=vals$colors_img$val,choicesOpt=list(content=vals$colors_img$img),selected=first_or(isolate(input$border)%||%"white",vals$colors_img$val))
    })
    som_model<-reactive({
      req(identical(input$target,"som"))
      m<-attr(data(),"som")[[input$som_model]]
      req(inherits(m,"kohonen"))
      m
    })
    observe({
      shinyjs::toggle("som_box",condition=identical(input$target,"som"))
      shinyjs::toggle("metric_box",condition=!identical(input$target,"som"))
      shinyjs::toggle("axes_box",condition=isTRUE(input$reduce))
      shinyjs::toggle("dbscan_box",condition=identical(input$method,"dbscan"))
      shinyjs::toggle("hdbscan_box",condition=identical(input$method,"hdbscan"))
      # options of the Clusters view only, for the target of the model shown
      tg<-if(is.null(model())) input$target else model()$target
      clusters_view<-identical(input$plot_type%||%"clusters","clusters")
      shinyjs::toggle("som_opts",condition=clusters_view&&identical(tg,"som"))
      shinyjs::toggle("data_opts",condition=clusters_view&&!identical(tg,"som"))
      shinyjs::toggle("assign_noise",condition=!is.null(model()))
    })

    # data or SOM distances ready for the algorithms
    # Numeric-Attribute ready for the distances (before the principal axes)
    prep0<-reactive({
      d<-data()
      num<-d[,vapply(d,is.numeric,logical(1)),drop=FALSE]
      validate(need(ncol(num)>0,"The Datalist has no numeric variables."))
      validate(need(!anyNA(num),"The Numeric-Attribute has missing values: impute or remove them first (Pre-processing > Data imputation)."))
      tryCatch(dbs_prepare(num,input$metric%||%"euclidean",isTRUE(input$scale)),error=function(e) validate(need(FALSE,conditionMessage(e))))
    })
    # PCA / PCoA computed once (the number of axes does not recompute it)
    decomp<-reactive({
      tryCatch(dbs_decomp(prep0()),error=function(e) validate(need(FALSE,conditionMessage(e))))
    })
    use_axes<-reactive({
      !identical(input$target,"som")&&isTRUE(input$reduce)&&prep0()$p>2
    })
    prep<-reactive({
      if(identical(input$target,"som")){
        m<-som_model()
        codes<-do.call(cbind,m$codes)
        nn<-nrow(codes)
        hits<-tabulate(m$unit.classif,nn)
        weighted<-isTRUE(input$som_weight)
        # with the hits, the empty neurons are left out (they bridged separate groups in simulations)
        keep<-if(weighted&&sum(hits>0)>=3) which(hits>0) else seq_len(nn)
        if(isTRUE(input$reduce)&&length(m$codes)==1&&ncol(codes)>2){
          k<-suppressWarnings(as.integer(input$n_axes))
          pr<-tryCatch(dbs_axes(dbs_prepare(codes[keep,,drop=FALSE]),if(length(k)&&!is.na(k)) k else NULL,rule=input$axes_rule%||%"bstick"),
                       error=function(e) validate(need(FALSE,conditionMessage(e))))
        } else{
          D<-as.matrix(kohonen::object.distances(m,"codes"))[keep,keep,drop=FALSE]
          pr<-dbs_prepare(stats::as.dist(D))
          pr$p<-ncol(codes)
        }
        pr$ids<-paste0("neuron_",keep)
        pr$neurons<-keep
        pr$n_neurons<-nn
        if(weighted) pr$w<-hits[keep]
        return(pr)
      }
      pr0<-prep0()
      pr<-pr0
      if(use_axes()){
        k<-suppressWarnings(as.integer(input$n_axes))
        pr<-tryCatch(dbs_axes(pr0,if(length(k)&&!is.na(k)) k else NULL,rule=input$axes_rule%||%"bstick",decomp=decomp()),error=function(e) validate(need(FALSE,conditionMessage(e))))
      }
      pr$vars<-colnames(pr0$X)
      pr
    })
    # number of units for the minPts limits: observations (hits) when the neurons are weighted
    n_units<-reactive({
      pr<-prep()
      if(is.null(pr$w)) pr$n else sum(pr$w)
    })
    # principal axes by default with many variables
    observeEvent(data(),{
      p<-sum(vapply(data(),is.numeric,logical(1)))
      updateCheckboxInput(session,"reduce",value=p>10)
    })
    output$setup_info<-renderUI({
      pr<-tryCatch(prep(),error=function(e) NULL)
      if(is.null(pr)) return(NULL)
      warn<-NULL
      txt<-if(identical(input$target,"som")){
        n_emp<-(pr$n_neurons%||%pr$n)-pr$n
        paste0(pr$n," neurons of the SOM model",if(n_emp>0) paste0(" (",n_emp," empty neurons left out)") else "",", ",
               if(!is.null(pr$reduced)) paste0(pr$reduced$p0," variables -> ",pr$reduced$k," principal axes of the codebook (",round(100*pr$reduced$var),"% of the variance)") else paste0("codebook distances, ",pr$p," variables"),
               if(!is.null(pr$w)) paste0("; neurons weighted by their hits (",sum(pr$w)," observations)") else "")
      } else if(!is.null(pr$reduced)){
        rule<-switch(pr$reduced$rule,manual="number given",bstick="broken-stick rule","50% of the variance rule")
        paste0(pr$n," observations x ",pr$reduced$p0," variables -> ",pr$reduced$k," principal axes (",pr$reduced$method," of the ",pr$reduced$metric0," distance",if(isTRUE(input$scale)) " on scaled data" else "","; ",rule,"; ",round(100*pr$reduced$var),"% of the variance); Euclidean distance on the axes")
      } else{
        if(pr$p>20) warn<-paste0("With ",pr$p," variables the distances between observations become similar and density-based clustering tends to find one cluster or only noise: consider Use principal axes.")
        paste0(pr$n," observations x ",pr$p," variables, ",input$metric,if(isTRUE(input$scale)) " distance on scaled data" else " distance")
      }
      div(style="font-size: 11px; color: #555555; padding: 2px 10px",icon("circle-info")," ",txt,
          if(!is.null(warn)) div(style="color: #8a6d3b",icon("triangle-exclamation")," ",warn))
    })
    suggested_minpts<-reactive({
      dbs_suggest_minpts(n_units(),prep()$p,input$method%||%"hdbscan")
    })
    suggested_mcs<-reactive({
      dbs_suggest_mcs(n_units(),suggested_minpts())
    })
    observeEvent(suggested_minpts(),{
      updateNumericInput(session,"minPts",value=suggested_minpts())
    })
    observeEvent(suggested_mcs(),{
      updateNumericInput(session,"mcs",value=suggested_mcs())
    })
    output$minpts_hint<-renderUI({
      pr<-prep()
      div(style="font-size: 11px; color: #555555; margin-top: -4px; padding-bottom: 6px",
          em(paste0("Suggested: ",suggested_minpts()," (2 x ",pr$p," dimensions",if(identical(input$method,"dbscan")) ", at least ln n" else "",", at most n/10",if(!is.null(pr$w)) "; in observations" else "",")")))
    })
    output$mcs_hint<-renderUI({
      div(style="font-size: 11px; color: #555555; margin-top: -4px; padding-bottom: 6px",
          em(paste0("Suggested: ",suggested_mcs()," (square root of n, at least minPts)")))
    })
    minpts<-reactive({
      mp<-suppressWarnings(as.integer(input$minPts))
      validate(need(isTRUE(mp>=2),"minPts must be at least 2."))
      validate(need(mp<n_units(),"minPts must be smaller than the number of observations."))
      mp
    })

    # ---- parameter guide
    observeEvent(input$method,{
      ch<-if(identical(input$method,"hdbscan")) c("k-distance"="kdist","Sensitivity"="sens","Condensed tree"="ctree") else c("k-distance"="kdist","Sensitivity"="sens")
      sel<-input$guide%||%"kdist"
      if(!sel%in%ch) sel<-"kdist"
      updateRadioGroupButtons(session,"guide",choices=ch,selected=sel)
    })
    # core distances (k-distances; weighted by the hits for SOM neurons)
    kdist<-reactive({
      dbs_core_dist(prep(),minpts())
    })
    eps_res<-reactiveVal(NULL)
    observeEvent(list(prep(),input$minPts),{ eps_res(NULL) })
    observeEvent(input$suggest_eps,ignoreInit=TRUE,{
      res<-withProgress(message="Searching eps...",min=NA,max=NA,tryCatch(dbs_suggest_eps(prep(),minpts(),kdist()),error=function(e) conditionMessage(e)))
      if(is.character(res)){
        showNotification(paste("eps could not be suggested:",res),type="error")
        return()
      }
      updateNumericInput(session,"eps",value=signif(res$eps,3))
      eps_res(res)
      updateRadioGroupButtons(session,"guide",selected="kdist")
    })
    output$eps_note<-renderUI({
      res<-eps_res()
      req(res)
      col<-if(isTRUE(res$stable)&&res$quality%in%c("strong","moderate")) "#2f6f3e" else "#8a6d3b"
      div(style=paste0("font-size: 11px; color: ",col,"; padding-bottom: 6px"),em(paste0("eps = ",signif(res$eps,3),": ",res$note)))
    })
    sens<-reactiveVal(NULL)
    observeEvent(list(prep(),input$method),{ sens(NULL) })
    output$guide_ui<-renderUI({
      g<-input$guide%||%"kdist"
      if(identical(g,"ctree")&&identical(input$method,"hdbscan")){
        return(div(plotOutput(ns("ctree_guide"),height="420px"),
                   div(style="font-size: 11px; color: #555555",em("Condensed tree with the current minPts, min cluster size and selection (not yet trained: click RUN to use them). Increase the min cluster size to merge small branches, or use Leaf selection to split a large cluster."))))
      }
      if(!identical(g,"sens")){
        return(div(plotOutput(ns("kdist_plot"),height="400px"),
                   div(style="font-size: 11px; color: #555555",em(if(identical(input$method,"dbscan")) "The knee of the curve is a classic eps choice, but it often joins groups without a clear density gap. Suggest eps tests eps values along the curve and keeps the solution with the highest DBCV (density-based validity) with 2 or more clusters; the green band is the range of eps with the same number of clusters." else "For HDBSCAN the curve shows the core distances (distance to the (minPts - 1)-th neighbour): long flat parts indicate dense groups. See also the Condensed tree."))))
      }
      div(class="dbs_sens",
        tags$style(HTML(".dbs_sens .form-group{margin-bottom: 0px} .dbs_sens .radio-inline{padding-top: 0px; margin-top: 0px} .dbs_sens .bootstrap-select .filter-option{overflow: hidden; text-overflow: ellipsis; white-space: nowrap}")),
        # row 1: what varies and the run button; row 2: the combination to use
        div(style="display: flex; gap: 16px; align-items: center; flex-wrap: wrap; padding: 2px 0px 8px 0px",
            if(use_axes()) div(style="display: flex; align-items: center; gap: 8px",
                               tags$label("Vary:",style="margin: 0px"),
                               radioButtons(ns("sens_vary"),NULL,choices=c("Parameters"="par","Number of axes"="axes"),selected=isolate(input$sens_vary)%||%"par",inline=TRUE)),
            actionButton(ns("run_sens"),"Run sensitivity",icon=icon("play"),style="height: 30px; padding: 3px 12px")),
        uiOutput(ns("apply_ui")),
        uiOutput(ns("sens_note")),
        plotOutput(ns("sens_plot"),height="360px")
      )
    })
    output$kdist_plot<-renderPlot({
      pr<-prep()
      p<-gg_dbs_kdist(kdist(),minpts()-1,eps=if(identical(input$method,"dbscan")) input$eps else NULL,
                      sugg=if(identical(input$method,"dbscan")) eps_res() else NULL,weighted=!is.null(pr$w))
      suppressWarnings(suppressMessages(print(p)))
    })
    guide_tree<-reactive({
      req(identical(input$method,"hdbscan"))
      dbs_hdbscan(prep(),minpts(),max(2,input$mcs%||%minpts()),allow_single=isTRUE(input$allow_single),
                  selection=input$selection%||%"eom",eps_sel=max(0,input$eps_sel%||%0,na.rm=TRUE))
    })
    pal_fun<-reactive({
      f<-vals$newcolhabs[[input$palette%||%"turbo"]]
      if(is.function(f)) f else grDevices::hcl.colors
    })
    output$ctree_guide<-renderPlot({
      h<-guide_tree()
      k<-length(h$selected)
      p<-gg_dbs_ctree(h,colors=if(k) pal_fun()(k) else NULL,
                      title=paste0(k," cluster(s), ",round(dbs_noise(prep(),h$cluster),1),"% noise"))
      suppressWarnings(suppressMessages(print(p)))
    })
    observeEvent(input$run_sens,ignoreInit=TRUE,{
      pr<-prep()
      mp<-minpts()
      vary<-if(use_axes()) input$sens_vary%||%"par" else "par"
      res<-withProgress(message="Running the sensitivity analysis...",value=0,{
        tryCatch({
          if(identical(vary,"axes")){
            ks<-2:min(10,ncol(decomp()$scores))
            list(type="axes",method=input$method,current=pr$reduced$k,
                 tab=dbs_axes_scan(prep0(),decomp(),ks,input$method,isTRUE(input$allow_single),input$selection%||%"eom",progress=function(v) setProgress(v)))
          } else if(identical(input$method,"dbscan")){
            kd<-kdist()
            eps_v<-unique(signif(stats::quantile(kd,seq(0.05,0.95,length.out=10),names=FALSE),3))
            eps_v<-eps_v[eps_v>0]
            mp_v<-sort(unique(pmax(2,round(c(mp/2,mp,mp*1.5,mp*2)))))
            mp_v<-mp_v[mp_v<n_units()]
            list(type="dbscan",tab=dbs_grid(pr,eps_v,mp_v,progress=function(v) setProgress(v)))
          } else{
            nu<-n_units()
            sizes<-sort(unique(round(seq(max(2,mp),max(mp*2,min(nu/3,mp*12)),length.out=9))))
            list(type="hdbscan",tab=dbs_hdbscan_sizes(pr,mp,sizes,isTRUE(input$allow_single),input$selection%||%"eom",max(0,input$eps_sel%||%0,na.rm=TRUE)))
          }
        },error=function(e) conditionMessage(e))
      })
      if(is.character(res)){
        showNotification(paste("Sensitivity analysis failed:",res),type="error")
        return()
      }
      sens(res)
    })
    output$sens_note<-renderUI({
      s<-sens()
      if(is.null(s)){
        txt<-if(identical(input$sens_vary,"axes")&&use_axes()) "Runs the method for 2 to 10 principal axes, each with its suggested minPts (and, for DBSCAN, its suggested eps)." else if(identical(input$method,"dbscan")) "Runs DBSCAN for eps values along the k-distance curve and several minPts." else "Runs HDBSCAN for several minimum cluster sizes (minPts fixed)."
        return(div(style="font-size: 11px; color: #555555; padding: 6px 0px",em(txt)))
      }
      NULL
    })
    output$sens_plot<-renderPlot({
      s<-sens()
      req(s)
      p<-switch(s$type,dbscan=gg_dbs_grid(s$tab),axes=gg_dbs_axes(s$tab),gg_dbs_sizes(s$tab))
      suppressWarnings(suppressMessages(print(p)))
    })
    # pre-selected: highest DBCV with >= 2 clusters (number of axes: the current one, as DBCV
    # values from different numbers of axes are only roughly comparable)
    output$apply_ui<-renderUI({
      s<-sens()
      req(s)
      tab<-s$tab
      lab<-switch(s$type,
                  dbscan=paste0("eps = ",tab$eps,", minPts = ",tab$minPts),
                  axes=paste0(tab$axes," axes (",tab$variance,"%), minPts = ",tab$minPts,ifelse(is.na(tab$eps),"",paste0(", eps = ",tab$eps))),
                  paste0("min cluster size = ",tab$min_cluster_size))
      lab<-paste0(lab," | ",tab$clusters," clusters, ",round(tab$noise,1),"% noise",ifelse(is.na(tab$dbcv),"",paste0(", DBCV ",round(tab$dbcv,2))))
      ok<-which(tab$clusters>=2&!is.na(tab$dbcv))
      best<-if(identical(s$type,"axes")&&isTRUE(s$current%in%tab$axes)) which(tab$axes==s$current) else if(length(ok)) ok[which.max(tab$dbcv[ok])] else 1
      # the picker shrinks with long labels (ellipsis), so the Use button stays visible
      div(style="display: flex; gap: 8px; align-items: center; padding-bottom: 8px; max-width: 760px",
          tags$label("Combination:",style="margin: 0px; white-space: nowrap; flex-shrink: 0"),
          div(style="flex: 1 1 auto; min-width: 0",
              shinyWidgets::pickerInput(ns("sens_pick"),NULL,choices=stats::setNames(seq_len(nrow(tab)),lab),selected=best,width="100%",
                                        options=shinyWidgets::pickerOptions(container="body"))),
          actionButton(ns("sens_apply"),"Use",icon=icon("check"),style="height: 30px; padding: 3px 12px; white-space: nowrap; flex-shrink: 0"))
    })
    observeEvent(input$sens_apply,ignoreInit=TRUE,{
      s<-sens()
      req(s)
      i<-as.integer(input$sens_pick)
      if(identical(s$type,"dbscan")){
        updateNumericInput(session,"eps",value=s$tab$eps[i])
        updateNumericInput(session,"minPts",value=s$tab$minPts[i])
      } else if(identical(s$type,"axes")){
        updateNumericInput(session,"n_axes",value=s$tab$axes[i])
        if(!is.na(s$tab$eps[i])) updateNumericInput(session,"eps",value=s$tab$eps[i])
      } else{
        updateNumericInput(session,"mcs",value=s$tab$min_cluster_size[i])
      }
      showNotification("Parameters updated: click RUN to train the model.",type="message")
    })

    # ---- training
    result<-reactiveVal(NULL)
    observeEvent(list(input$minPts,input$eps,input$mcs,input$allow_single,input$selection,input$eps_sel,input$method,input$metric,input$scale,
                      input$reduce,input$axes_rule,input$n_axes,input$target,input$som_model,input$som_weight),{
      shinyjs::addClass("run_btn","save_changes")
    })
    observeEvent(input$run,ignoreInit=TRUE,{
      pr<-tryCatch(prep(),error=function(e) NULL)
      if(is.null(pr)){
        showNotification("Check the setup: the data could not be prepared.",type="error")
        return()
      }
      res<-withProgress(message=paste0("Running ",toupper(input$method),"..."),min=NA,max=NA,{
        tryCatch({
          if(identical(input$method,"dbscan")){
            validate(need(isTRUE(input$eps>0),"eps must be positive."))
            dbs_dbscan(pr,input$eps,minpts())
          } else{
            dbs_hdbscan(pr,minpts(),max(2,input$mcs),allow_single=isTRUE(input$allow_single),
                        selection=input$selection%||%"eom",eps_sel=max(0,input$eps_sel%||%0,na.rm=TRUE))
          }
        },error=function(e) conditionMessage(e))
      })
      if(is.character(res)){
        showNotification(paste("The model could not be trained:",res),type="error")
        return()
      }
      res$target<-input$target
      res$som_model<-if(identical(input$target,"som")) input$som_model else NULL
      res$datalist<-input$data_db
      res$prep<-pr
      res$dbcv<-tryCatch(dbs_dbcv(pr,res$cluster),error=function(e) NA_real_)
      res$silhouette<-tryCatch(dbs_silhouette(pr,res$cluster),error=function(e) NA_real_)
      result(res)
      shinyjs::removeClass("run_btn","save_changes")
      updateTabsetPanel(session,"tabs",selected="tab2")
    })

    # ---- models: the one just trained (unsaved) and the ones saved in the Datalist
    unsaved_label<-"New model (unsaved)"
    sel_model<-reactiveVal(NULL)
    saved_models<-reactive({
      req(input$data_db%in%names(vals$saved_data))
      imesc_models_of(vals$saved_data[[input$data_db]],"dbscan")
    })
    observeEvent(result(),ignoreNULL=FALSE,{
      if(!is.null(result())) sel_model(unsaved_label)
    })
    observe({
      ch<-c(if(!is.null(result())) unsaved_label,names(saved_models()))
      sel<-sel_model()
      if(is.null(sel)||!sel%in%ch) sel<-if(length(ch)) ch[1] else character(0)
      updatePickerInput(session,"dbs_model",choices=ch,selected=sel)
    })
    observeEvent(input$dbs_model,{ sel_model(input$dbs_model) })
    model<-reactive({
      s<-input$dbs_model
      if(is.null(s)||!nzchar(s)) return(NULL)
      if(identical(s,unsaved_label)) return(result())
      saved_models()[[s]]
    })
    # predictions belong to the model shown: cleared when another model is selected or a new one
    # is trained (not when the Datalist changes, e.g. when the predictions are saved in it)
    observeEvent(list(input$dbs_model,result()),ignoreNULL=FALSE,{ pred(NULL) })
    observe({
      unsaved<-identical(input$dbs_model,unsaved_label)&&!is.null(result())
      shinyjs::toggle("save_model_btn",condition=unsaved)
      shinyjs::toggle("delete_model_btn",condition=!is.null(model())&&!unsaved)
    })
    observeEvent(input$save_model,ignoreInit=TRUE,{
      r<-result()
      req(r)
      k<-length(unique(r$cluster[r$cluster>0]))
      name0<-paste0(toupper(r$method),if(identical(r$target,"som")) "_som" else "","_",k,"cl")
      nm<-imesc_model_unique_name(vals$saved_data[[r$datalist]],"dbscan",name0)
      showModal(modalDialog(
        title="Save model",easyClose=TRUE,
        div(p("Model saved in the Datalist ",strong(r$datalist),":"),
            textInput(ns("model_name"),NULL,value=nm,width="300px"),
            em("A model with the same name is replaced.")),
        footer=div(modalButton("Cancel"),actionButton(ns("confirm_save_model"),"Save"))
      ))
    })
    observeEvent(input$confirm_save_model,ignoreInit=TRUE,{
      r<-result()
      name<-trimws(input$model_name%||%"")
      req(r,nzchar(name),r$datalist%in%names(vals$saved_data))
      attr(r,"model_name")<-name
      r$saved<-format(Sys.time(),"%Y-%m-%d %H:%M")
      vals$saved_data[[r$datalist]]<-imesc_model_set(vals$saved_data[[r$datalist]],"dbscan",name,r)
      sel_model(name)
      result(NULL)
      removeModal()
      showNotification(paste0("Model '",name,"' saved in the Datalist ",r$datalist,"."),type="message")
    })
    observeEvent(input$delete_model,ignoreInit=TRUE,{
      req(input$dbs_model%in%names(saved_models()))
      showModal(modalDialog(
        title="Delete model",easyClose=TRUE,
        p("Delete the model ",strong(input$dbs_model)," from the Datalist ",strong(input$data_db),"?"),
        footer=div(modalButton("Cancel"),actionButton(ns("confirm_delete_model"),"Delete"))
      ))
    })
    observeEvent(input$confirm_delete_model,ignoreInit=TRUE,{
      nm<-input$dbs_model
      req(nm%in%names(saved_models()))
      vals$saved_data[[input$data_db]]<-imesc_model_delete(vals$saved_data[[input$data_db]],"dbscan",nm)
      sel_model(NULL)
      removeModal()
      showNotification(paste0("Model '",nm,"' deleted."),type="message")
    })

    # labels of the clustered units (neurons or observations), optionally without noise
    raw_clusters<-reactive({
      r<-model()
      validate(need(!is.null(r),"Train the model in 1. Parameters (RUN)."))
      cl<-r$cluster
      if(isTRUE(input$assign_noise)) cl<-dbs_assign_noise(r$prep,cl)
      cl
    })
    # ---- sort clusters by a numeric variable (as in the HC module)
    # observation IDs of the model and their (unsorted) clusters
    raw_obs_clusters<-reactive({
      r<-model()
      cl<-raw_clusters()
      if(identical(r$target,"som")){
        m<-attr(vals$saved_data[[r$datalist]],"som")[[r$som_model]]
        validate(need(inherits(m,"kohonen"),paste0("The SOM model ",r$som_model," of this model is no longer in the Datalist ",r$datalist,".")))
        o<-dbs_neuron_clusters(r$prep,cl,m)[m$unit.classif]
        names(o)<-names(m$unit.classif)%||%rownames(vals$saved_data[[r$datalist]])
        return(o)
      }
      stats::setNames(cl,r$prep$ids)
    })
    observe({ shinyjs::toggle("sort_box",condition=isTRUE(input$sort_clusters)) })
    observe({
      r<-model()
      req(r)
      ids<-names(raw_obs_clusters())
      ch<-names(vals$saved_data)[vapply(vals$saved_data,function(d) all(ids%in%rownames(d))&&any(vapply(d,is.numeric,logical(1))),logical(1))]
      updatePickerInput(session,"sort_datalist",choices=ch,selected=first_or(isolate(input$sort_datalist)%||%r$datalist,ch))
    })
    observeEvent(input$sort_datalist,{
      d<-vals$saved_data[[input$sort_datalist]]
      req(d)
      ch<-colnames(d)[vapply(d,is.numeric,logical(1))]
      updatePickerInput(session,"sort_var",choices=ch,selected=first_or(isolate(input$sort_var),ch))
    })
    # new number of each cluster (names: original numbers; noise stays 0), or NULL
    cl_map<-reactive({
      if(!isTRUE(input$sort_clusters)) return(NULL)
      o<-raw_obs_clusters()
      d<-vals$saved_data[[input$sort_datalist%||%""]]
      if(is.null(d)||!isTRUE(input$sort_var%in%colnames(d))||!all(names(o)%in%rownames(d))) return(NULL)
      v<-as.numeric(d[names(o),input$sort_var])
      k<-sort(unique(o[o>0]))
      if(length(k)<2) return(NULL)
      score<-vapply(k,function(g) mean(v[o==g],na.rm=TRUE),numeric(1))
      newk<-rank(score,ties.method="first",na.last=TRUE)
      stats::setNames(c(0L,as.integer(newk)),c("0",as.character(k)))
    })
    relabel_clusters<-function(cl){
      mp<-cl_map()
      if(is.null(mp)) return(cl)
      out<-as.integer(mp[as.character(cl)])
      out[is.na(out)]<-0L
      out
    }
    unit_clusters<-reactive({
      relabel_clusters(raw_clusters())
    })
    # labels of the observations of the Datalist
    obs_clusters<-reactive({
      r<-model()
      cl<-unit_clusters()
      if(identical(r$target,"som")){
        m<-attr(vals$saved_data[[r$datalist]],"som")[[r$som_model]]
        validate(need(inherits(m,"kohonen"),paste0("The SOM model ",r$som_model," of this model is no longer in the Datalist ",r$datalist,".")))
        f<-dbs_factor(dbs_neuron_clusters(r$prep,cl,m))[m$unit.classif]
        names(f)<-names(m$unit.classif)%||%rownames(vals$saved_data[[r$datalist]])
        return(f)
      }
      f<-dbs_factor(cl)
      names(f)<-r$prep$ids
      f
    })

    output$summary<-renderUI({
      r<-model()
      if(is.null(r)) return(div(style="font-size: 11px; color: #555555",em("Train the model in 1. Parameters (RUN).")))
      tab<-dbs_summary(r,r$prep)
      # sorted clusters: new numbers, in order
      mp<-cl_map()
      if(!is.null(mp)){
        cc<-tab$Cluster!="Noise"
        tab$Cluster[cc]<-as.character(mp[tab$Cluster[cc]])
        tab<-tab[order(cc==FALSE,suppressWarnings(as.integer(tab$Cluster))),,drop=FALSE]
        rownames(tab)<-NULL
      }
      k<-sum(tab$Cluster!="Noise")
      v<-r$dbcv
      q<-if(is.na(v)) "" else if(v>=0.5) " (strong)" else if(v>=0.2) " (moderate)" else if(v>=0) " (weak)" else " (no density-separated groups: the data may form a single group)"
      go<-glosh_out()
      div(
        if(!is.null(attr(r,"model_name"))) div(style="font-size: 11px; color: #555555; padding-bottom: 3px",icon("fas fa-save")," ",
                                                 em(paste0("Saved model '",attr(r,"model_name"),"' of ",r$datalist,if(!is.null(r$saved)) paste0(" (",r$saved,")") else ""))),
        div(style="font-size: 12px",
            strong(toupper(r$method)),": ",k," cluster(s), ",round(dbs_noise(r$prep,r$cluster),1),"% noise",
            if(!is.na(v)) span(", ",tiphelp5(paste0("DBCV ",round(v,3),q),"Density-Based Clustering Validation (-1 to 1): separation of the clusters by low-density regions, penalised by the noise. Above 0.5 strong, 0.2-0.5 moderate, below 0.2 weak.")) else "",
            if(!is.na(r$silhouette)) paste0(", silhouette (without noise) ",round(r$silhouette,3)) else "",
            if(identical(r$method,"dbscan")) paste0(" | eps = ",signif(r$eps,4),", minPts = ",r$minPts) else
              paste0(" | minPts = ",r$minPts,", min cluster size = ",r$min_cluster_size,if(identical(r$selection,"leaf")) ", leaf selection" else "",if(isTRUE(r$eps_sel>0)) paste0(", selection epsilon = ",r$eps_sel) else ""),
            if(!is.null(go)) div(em(paste0(sum(go)," GLOSH outliers (score above the ",input$glosh_q," quantile, ",round(stats::quantile(r$glosh,input$glosh_q,names=FALSE),3),")."))),
            if(identical(r$target,"som")) div(em("Neurons were clustered; the observations take the cluster of their best-matching neuron.",if(!is.null(r$prep$w)) " Neurons weighted by their hits: sizes and noise in observations." else ""))),
        div(style="overflow-x: auto; margin-top: 6px",renderTable(tab,striped=TRUE,bordered=TRUE,spacing="xs",na=""))
      )
    })
    # GLOSH outliers (HDBSCAN on the Numeric-Attribute)
    glosh_out<-reactive({
      r<-model()
      if(is.null(r)||!identical(r$method,"hdbscan")||!identical(r$target,"data")) return(NULL)
      q<-input$glosh_q
      if(!isTRUE(q>0&&q<1)) return(NULL)
      r$glosh>stats::quantile(r$glosh,q,names=FALSE)
    })
    observe({
      r<-model()
      is_h<-!is.null(r)&&identical(r$method,"hdbscan")
      # the hierarchy views exist for HDBSCAN only
      for(v in c("ctree","dendro")) if(is_h) showTab("plot_type",v) else hideTab("plot_type",v)
      if(!is_h&&!identical(isolate(input$plot_type),"clusters")) updateTabsetPanel(session,"plot_type",selected="clusters")
      shinyjs::toggle("glosh_box",condition=is_h&&identical(r$target,"data"))
    })

    # ---- plot (SOM map as in the HC / K-means modules, or PCA/PCoA of the observations)
    dbs_plot<-reactive({
      r<-model()
      req(r)
      cl<-unit_clusters()
      f<-dbs_factor(cl)
      req(input$palette%in%names(vals$newcolhabs))
      pal_fun<-vals$newcolhabs[[input$palette]]
      if(identical(r$method,"hdbscan")&&identical(input$plot_type,"ctree")){
        k<-length(r$selected)
        return(gg_dbs_ctree(r,colors=if(k) pal_fun(k) else NULL,base_size=input$base_size%||%12,title=input$title,relabel=cl_map()))
      }
      if(identical(r$method,"hdbscan")&&identical(input$plot_type,"dendro")){
        rr<-r
        rr$cluster<-cl
        return(gg_dbs_dendrogram(rr,r$prep$ids,dbs_colors(f,pal_fun),base_size=input$base_size%||%12,title=input$title))
      }
      if(identical(r$target,"som")){
        m<-attr(vals$saved_data[[r$datalist]],"som")[[r$som_model]]
        validate(need(inherits(m,"kohonen"),paste0("The SOM model ",r$som_model," of this model is no longer in the Datalist ",r$datalist,".")))
        req(m)
        f<-dbs_factor(dbs_neuron_clusters(r$prep,cl,m))
        hexs<-get_neurons(m,background_type="hc",property=NULL,hc=f)
        cols<-dbs_colors(f,pal_fun)
        newcol<-vals$newcolhabs
        newcol[[".dbs_bg"]]<-function(n) unname(cols)[seq_len(n)]
        pts<-NULL
        if(isTRUE(input$som_points)){
          pts<-rescale_copoints(hexs=hexs,copoints=getcopoints(m))
          fac<-attr(vals$saved_data[[r$datalist]],"factors")
          if(isTRUE(input$points_factor%in%colnames(fac))){
            pts$point<-fac[rownames(pts),input$points_factor]
            attr(pts,"namepoints")<-input$points_factor
          }
        }
        p<-bmu_plot_hc(m,hexs=hexs,points_tomap=pts,bp=NULL,points=isTRUE(input$som_points),points_size=input$som_pt_size%||%1,
                       points_palette=input$points_palette%||%"black",pch=16,bg_palette=".dbs_bg",newcolhabs=newcol,bgalpha=0,fill_neurons=TRUE,
                       border=input$border%||%"white",base_size=input$base_size%||%12,show_neucoords=FALSE,title=input$title,hc=f,
                       neuron_legend="Cluster",points_legend=if(isTRUE(input$points_factor%in%colnames(attr(vals$saved_data[[r$datalist]],"factors")))) input$points_factor else "Observations")
        return(p)
      }
      ord<-dbs_ordination(r$prep)
      gg_dbs_scatter(ord,f,dbs_colors(f,pal_fun),hulls=isTRUE(input$hulls),point_size=input$pt_size%||%2,base_size=input$base_size%||%12,title=input$title,
                     outliers=if(isTRUE(input$mark_glosh)) glosh_out() else NULL)
    })
    output$plot<-renderPlot({
      p<-dbs_plot()
      suppressWarnings(suppressMessages(print(p)))
    })
    output$plot_note<-renderUI({
      r<-model()
      req(r)
      if(identical(r$method,"hdbscan")&&input$plot_type%in%c("ctree","dendro")) return(NULL)
      if(identical(r$target,"som")) return(NULL)
      div(style="font-size: 11px; color: #555555; padding: 2px 5px",em(if(identical(r$prep$metric,"euclidean")) "Observations on the first two principal axes; noise as crosses." else "Observations on the first two axes of a PCoA of the chosen distance; noise as crosses."))
    })
    observeEvent(input$down_plot,ignoreInit=TRUE,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      callModule(module_server_figs,"downfigs",vals=vals,generic=dbs_plot(),message="Density-based clustering",name_c=paste0(model()$method,"_clusters"),datalist_name=model()$datalist)
    })

    # columns of the Factor-Attribute identical to a clustering (as cluster_already() of the HC
    # module): the save button stops highlighting and the column is named
    already_saved<-function(f,datalist){
      fac<-attr(vals$saved_data[[datalist]],"factors")
      if(is.null(fac)||!ncol(fac)||!all(rownames(fac)%in%names(f))) return(character(0))
      cur<-as.character(f[rownames(fac)])
      names(fac)[vapply(fac,function(x) identical(as.character(x),cur),logical(1))]
    }
    clusters_saved<-reactive({
      r<-model()
      req(r)
      tryCatch(already_saved(obs_clusters(),r$datalist),error=function(e) character(0))
    })
    observe({
      al<-tryCatch(clusters_saved(),error=function(e) character(0))
      if(length(al)) shinyjs::removeClass("save_clusters_btn","save_changes") else shinyjs::addClass("save_clusters_btn","save_changes")
    })
    output$clusters_saved_note<-renderUI({
      al<-clusters_saved()
      req(length(al))
      div(style="font-size: 11px; color: #555555; padding-top: 3px",em(paste0("The current clustering is saved in the Factor-Attribute as '",paste(al,collapse="; "),"'.")))
    })

    # ---- save the clusters as a Factor-Attribute
    observeEvent(input$save_clusters,ignoreInit=TRUE,{
      r<-model()
      if(is.null(r)){
        showNotification("Train the model first.",type="warning")
        return()
      }
      k<-length(unique(r$cluster[r$cluster>0]))
      name0<-paste0(toupper(r$method),if(identical(r$target,"som")) "_som" else "","_",k)
      fac<-colnames(attr(vals$saved_data[[r$datalist]],"factors"))
      nm<-make.unique(c(fac,name0),sep="_")
      showModal(modalDialog(
        title="Save clusters",easyClose=TRUE,
        div(p("New column of the Factor-Attribute of ",strong(r$datalist),":"),
            textInput(ns("factor_name"),NULL,value=nm[length(nm)],width="300px"),
            if(identical(r$target,"som")) em("Each observation receives the cluster of its best-matching neuron."),
            if(identical(r$method,"hdbscan")&&identical(r$target,"data"))
              checkboxInput(ns("save_glosh"),paste0("Also save the GLOSH outliers (score above the ",input$glosh_q," quantile) as a column '<name>_GLOSH' (Outlier / Inlier)"),value=FALSE)),
        footer=div(modalButton("Cancel"),actionButton(ns("confirm_save"),"Save"))
      ))
    })
    observeEvent(input$confirm_save,ignoreInit=TRUE,{
      r<-model()
      req(r,nzchar(input$factor_name%||%""))
      f<-obs_clusters()
      d<-vals$saved_data[[r$datalist]]
      fac<-attr(d,"factors")
      if(is.null(fac)) fac<-data.frame(row.names=rownames(d))
      fac[[input$factor_name]]<-factor(as.character(f[rownames(fac)]),levels=levels(f))
      go<-glosh_out()
      gname<-NULL
      if(isTRUE(input$save_glosh)&&!is.null(go)){
        gname<-paste0(input$factor_name,"_GLOSH")
        g<-stats::setNames(ifelse(go,"Outlier","Inlier"),r$prep$ids)
        fac[[gname]]<-factor(g[rownames(fac)],levels=c("Inlier","Outlier"))
      }
      attr(vals$saved_data[[r$datalist]],"factors")<-fac
      removeModal()
      showNotification(paste0("Clusters saved as '",input$factor_name,"'",if(!is.null(gname)) paste0(" and '",gname,"'") else ""," in the Factor-Attribute of ",r$datalist,"."),type="message")
    })

    # ---- predict new observations
    pred<-reactiveVal(NULL)
    observe({
      r<-model()
      shinyjs::toggle("run_pred",condition=!is.null(r)&&identical(r$target,"data"))
    })
    output$pred_info<-renderUI({
      r<-model()
      msg<-if(is.null(r)) "Train the model in 1. Parameters (RUN)." else if(!identical(r$target,"data")) "Prediction is available for models trained on the Numeric-Attribute (for a SOM codebook, use the SOM predictions)." else if(identical(r$method,"dbscan")) "A new observation gets the cluster of its nearest core point when it is within eps; otherwise it is noise." else "A new observation gets the cluster of its nearest clustered point (mutual reachability distance); it is noise when that distance is larger than the one at which the cluster appears in the hierarchy."
      div(style="font-size: 11px; color: #555555; padding-top: 6px",em(msg,if(!is.null(r$prep$reduced)) paste0(" New observations are first projected onto the ",r$prep$reduced$method," axes of the training data",if(identical(r$prep$reduced$method,"PCoA")) " (Gower's add-a-point formula)" else "",".") else ""))
    })
    observeEvent(input$run_pred,ignoreInit=TRUE,{
      r<-model()
      req(r,identical(r$target,"data"),input$new_data%in%names(vals$saved_data))
      nd<-vals$saved_data[[input$new_data]]
      vars<-r$prep$vars%||%colnames(r$prep$X)
      if(!all(vars%in%colnames(nd))){
        showNotification("The new Datalist does not have all the training variables.",type="error")
        return()
      }
      X<-nd[,vars,drop=FALSE]
      if(anyNA(X)){
        showNotification("The new data have missing values.",type="error")
        return()
      }
      cl<-withProgress(message="Predicting...",min=NA,max=NA,tryCatch(dbs_predict(r,r$prep,X),error=function(e) conditionMessage(e)))
      if(is.character(cl)){
        showNotification(paste("Prediction failed:",cl),type="error")
        return()
      }
      f<-dbs_factor(cl)
      names(f)<-rownames(nd)
      pred(list(f=f,datalist=input$new_data))
    })
    output$pred_table<-renderTable({
      p<-pred()
      req(p)
      tb<-table(p$f)
      data.frame(Cluster=names(tb),n=as.integer(tb),Percent=round(100*as.integer(tb)/length(p$f),1))
    },striped=TRUE,bordered=TRUE,spacing="xs")
    output$pred_note<-renderUI({
      p<-pred()
      if(is.null(p)) return(NULL)
      al<-already_saved(p$f,p$datalist)
      div(style="padding: 4px 5px",
          strong(paste0(length(p$f)," observations of ",p$datalist," classified.")),
          div(class=if(length(al)) "button_normal" else "save_changes",actionButton(ns("save_pred"),span(icon("fas fa-save")," Save as Factor-Attribute of ",p$datalist))),
          if(length(al)) div(style="font-size: 11px; color: #555555; padding-top: 3px",em(paste0("These predictions are saved in the Factor-Attribute as '",paste(al,collapse="; "),"'."))))
    })
    observeEvent(input$save_pred,ignoreInit=TRUE,{
      p<-pred()
      req(p)
      d<-vals$saved_data[[p$datalist]]
      fac<-attr(d,"factors")
      if(is.null(fac)) fac<-data.frame(row.names=rownames(d))
      nm<-make.unique(c(colnames(fac),paste0(toupper(model()$method),"_pred")),sep="_")
      nm<-nm[length(nm)]
      fac[[nm]]<-factor(as.character(p$f[rownames(fac)]),levels=levels(p$f))
      attr(vals$saved_data[[p$datalist]],"factors")<-fac
      showNotification(paste0("Predictions saved as '",nm,"' in the Factor-Attribute of ",p$datalist,"."),type="message")
    })
  })
}
