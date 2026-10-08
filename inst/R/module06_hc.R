

#' ## Overview of the Hierarchical Clustering (HC) Module
#' The Hierarchical Clustering (HC) module in the iMESc application enables data exploration
#' and visualization through various hierarchical clustering methods. This module supports both
#' divisive and agglomerative clustering techniques, allowing for comprehensive data analysis.

# Key Functionalities:
# 1. HC Setup:
#    - Define the clustering target: numeric attributes or SOM codebooks.
#    - Select clustering functions such as `hclust`, `agnes`, or `diana`.
#    - Configure the distance measure (e.g., Euclidean, Bray-Curtis, Jaccard) and linkage methods
#      (e.g., Ward, complete, single).
#
# 2. Dendrograms and Visualization:
#    - Generate dendrograms to represent clustering structures visually.
#    - Customize dendrogram aesthetics, including labels, titles, and scaling.
#    - Cut dendrograms to identify optimal cluster groups.
#
# 3. Scree Plots:
#    - Determine the optimal number of clusters using the Elbow method.
#    - Perform split moving window (SMW) analysis to detect significant discontinuities.
#
# 4. Integration with SOM:
#    - Utilize SOM codebooks for hierarchical clustering.
#    - Combine data layers and adjust weights for SOM-based clustering.
#    - Save and manage clustering results for future use or analysis.

# Workflow:
# - Data Preparation:
#   - Select the dataset and attributes for clustering.
#   - Preprocess data to ensure compatibility with distance metrics.
#
# - Model Configuration:
#   - Choose the clustering function and method.
#   - Set distance measures and parameters.
#
# - Analysis and Visualization:
#   - Generate dendrograms and explore hierarchical structures.
#   - Use scree plots to validate the number of clusters.
#   - Cut dendrograms to extract meaningful clusters.
#
# - Result Handling:
#   - Save clustering results to factors or new datasets.
#   - Visualize cluster assignments within SOM grids.

# Applications:
# - Exploratory data analysis in environmental sciences, bioinformatics, and more.
# - Visualizing relationships between variables and observations.
# - Integrating hierarchical clustering with SOM for detailed analysis.

# References:
# This module utilizes the `factoextra`, `kohonen`, and `vegan` R packages for hierarchical clustering
# and visualization.

.get_withinSS<-factoextra:::.get_withinSS
.get_withinSS <- factoextra:::.get_withinSS
.get_ave_sil_width <- factoextra:::.get_ave_sil_width


imesc_fviz_nbclust<-function (x, FUNcluster = NULL, method = c("silhouette", "wss",
                                                               "gap_stat"), diss = NULL, k.max = 10, nboot = 100, verbose = interactive(),
                              barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue",
                              print.summary = TRUE, ...)
{
  if (k.max < 2)
    stop("k.max must bet > = 2")
  method = match.arg(method)
  if (!inherits(x, c("data.frame", "matrix")) && !("Best.nc" %in%
                                                   names(x)))
    stop("x should be an object of class matrix/data.frame or ",
         "an object created by the function NbClust() [NbClust package].")
  if (inherits(x, "list") && "Best.nc" %in% names(x)) {
    best_nc <- x$Best.nc
    if (is.numeric(best_nc) && !is.matrix(best_nc)) {
      print(best_nc)}    else if (is.matrix(best_nc)) {

        .viz_NbClust(x, print.summary, barfill, barcolor)
      }
  }  else if (is.null(FUNcluster)) {

    stop("The argument FUNcluster is required. ", "Possible values are kmeans, pam, hcut, clara, ...")
  }  else if (!is.function(FUNcluster)) {
    stop("The argument FUNcluster should be a function. ",
         "Check if you're not overriding the specified function name somewhere.")
  }  else if (method %in% c("silhouette", "wss")) {
    if (is.data.frame(x))
      x <- as.matrix(x)
    if (is.null(diss))
      diss <- stats::dist(x)
    v <- rep(0, k.max)
    if (method == "silhouette") {
      for (i in 2:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_ave_sil_width(diss, clust$cluster)
      }
    }    else if (method == "wss") {
      i=2
      for (i in 2:k.max) {
        clust <- FUNcluster(x, i)
        v[i] <- .get_withinSS(diss, clust$cluster)
      }
    }

    df <- data.frame(clusters = as.factor(2:k.max), y = v[-1])
    ylab <- "Total Within Sum of Square"
    main_title <- "Optimal number of clusters"
    if (method == "silhouette") {
      ylab <- "Average silhouette width"
      main_title <- "Optimal number of clusters (method = \"silhouette\")"
    }
    p <- ggpubr::ggline(df, x = "clusters", y = "y", group = 1,
                        color = linecolor, ylab = ylab, xlab = "Number of clusters k",
                        main = main_title)
    if (method == "silhouette")
      p <- p + geom_vline(xintercept = which.max(v), linetype = 2,
                          color = linecolor)
    return(p)
  }  else if (method == "gap_stat") {
    extra_args <- list(...)
    if (!is.null(extra_args$maxSE)) {
      maxSE <- extra_args$maxSE
      extra_args$maxSE <- NULL
    }    else {
      maxSE <- list(method = "firstSEmax", SE.factor = 1)
    }
    gap_stat <- do.call(cluster::clusGap, c(list(x = x, FUNcluster = FUNcluster,
                                                 K.max = k.max, B = nboot, verbose = verbose), extra_args))
    p <- factoextra::fviz_gap_stat(gap_stat, linecolor = linecolor, maxSE = maxSE)
    return(p)
  }
}
textscreeplot<-function(...){
  div(tags$style(HTML("
   h2 {
      font-size: 20px;
      font-weight: bold;
   }
   h3 {
      font-size: 18px;
      font-weight: normal;
   }
   code {
      color: navy;
   }
")),
      strong("How many clusters (k) should be defined?"),
      br(),
      p("The scree plot is a graphical heuristic used to support the choice of the number of clusters. It involves:"),
      p(strong("a)"), "Mapping an internal clustering criterion against the number of clusters, and"),
      p(strong("b)"), "Looking for changes in the curve that indicate diminishing returns."),
      p("iMESc creates the Elbow plot via the", code("fviz_nbclust"), "function from the", code("factoextra"), "package."),
      p("Ideally, the cluster count should be such that adding another cluster no longer produces a substantial reduction in WSS. This is usually inspected around the transition from a steep decrease to a flatter curve."),
      p("Pinpointing the 'elbow' isn't always straightforward. To assist, users can apply", code("split moving window analysis"), "to the scree plot. This is an auxiliary suggestion, not an automatic decision rule."),
      p("iMESc can also calculate the", code("Gap statistic"), ". Gap compares the within-cluster dispersion observed in the data with the dispersion expected under an empirical reference distribution with no clear cluster structure. Larger Gap values indicate stronger separation relative to that reference. This method is usually more formal than visual elbow inspection, but it is computationally heavier and still depends on the chosen reference distribution, clustering algorithm, and distance representation."),
      p("In iMESc, SMW can be applied either to the WSS curve itself or to the marginal gain obtained when moving from k - 1 to k clusters. The marginal gain option is the default because it focuses on where additional clusters stop producing large improvements. The process is as follows:"),
      tags$ul(
        tags$li(strong("(a)"), "Position an even-sized window at the start of the dataset,"),
        tags$li(strong("(b)"), "Divide the window equally in two,"),
        tags$li(strong("(c)"), "Summarize the selected series within each half,"),
        tags$li(strong("(d)"), "Measure the dissimilarity between both halves,"),
        tags$li(strong("(e)"), "Slide the window one step along the series, and"),
        tags$li(strong("(f)"), "Repeat till the series concludes.")
      ),
      p("The window size choice impacts the SMW analysis outcomes and is mitigated by averaging dissimilarities from multiple even window sizes. These values help identify potential breakpoints. To provide an empirical reference, iMESc repeatedly resamples the selected series with replacement for each window size and recalculates the SMW dissimilarities. This reference is used as a practical threshold for highlighting unusually large local changes in the curve, not as a formal statistical test for the true number of clusters."),
      h3("SMW options"),
      tags$ul(
        tags$li(strong("SMW target:"), code("Marginal gain"), "applies SMW to the WSS reduction obtained when adding one more cluster and is recommended for detecting the elbow. ", code("WSS"), "applies SMW directly to the original WSS curve."),
        tags$li(strong("Threshold:"), code("Random quantile"), "flags a candidate breakpoint when the observed mean dissimilarity is greater than the selected randomization quantile. ", code("tol * SD"), "keeps the legacy rule, flagging a candidate breakpoint when mean dissimilarity is greater than tol times the resampled standard deviation."),
        tags$li(strong("confidence:"), "sets the randomization quantile used by", code("Random quantile"), ". For example, 0.95 uses the 95th percentile of the randomized dissimilarities."),
        tags$li(strong("tol:"), "sets the multiplier used by", code("tol * SD"), ". Larger values are more conservative and produce fewer red stars."),
        tags$li(strong("Window sizes:"), "defines the even-sized moving windows used by SMW. Results are averaged across the selected window sizes."),
        tags$li(strong("N randomizations:"), "defines how many resampled WSS series are generated for the random reference distribution.")
      ),
      p("By default, a position is flagged as a candidate breakpoint when the mean observed dissimilarity exceeds the selected randomization quantile. The legacy", code("tol * SD"), "rule is also available for compatibility. Red stars indicate positions exceeding the selected reference threshold. The dashed vertical line indicates the main suggested k: when red stars are present, iMESc chooses the flagged candidate with the highest dissimilarity; otherwise, it uses the highest dissimilarity peak within the stable window range. Edge positions are still displayed but are not used to place the dashed suggestion line. Highlighted points should be interpreted as candidate elbows, not as definitive statistical evidence for the true number of clusters."),
      h3("Gap statistic options"),
      tags$ul(
        tags$li(strong("Run Gap statistic:"), "calculates an additional k suggestion using a bootstrap reference distribution."),
        tags$li(strong("N bootstraps:"), "defines how many reference datasets are generated. Larger values are more stable but slower."),
        tags$li(strong("Gap rule:"), "defines how the suggested k is selected from the Gap curve. ", code("firstSEmax"), "is the common default: it chooses the smallest k within one standard error of a local/global maximum according to the selected rule.")
      )
  )
}
textgapplot<-function(...){
  div(tags$style(HTML("
   h2 {
      font-size: 20px;
      font-weight: bold;
   }
   h3 {
      font-size: 18px;
      font-weight: normal;
   }
   code {
      color: navy;
   }
")),
      strong("Gap statistic"),
      br(),
      p("The Gap statistic compares the within-cluster dispersion observed in the data with the dispersion expected under a reference distribution with no clear cluster structure. In practical terms, it asks whether increasing the number of clusters improves the partition more than would be expected by chance under that reference."),
      p("Larger Gap values indicate stronger evidence that the selected number of clusters captures structure beyond the reference distribution. The method is useful as an auxiliary suggestion, but it should still be interpreted together with the dendrogram, ecological meaning, cluster sizes, and other diagnostics."),
      h3("Options"),
      tags$ul(
        tags$li(strong("k:"), "maximum number of clusters tested by the Gap routine."),
        tags$li(strong("N bootstraps:"), "number of reference datasets generated by", code("cluster::clusGap"), ". Larger values give a more stable reference distribution but increase computation time."),
        tags$li(strong("Gap rule:"), "rule passed to", code("factoextra::fviz_gap_stat"), "through the", code("maxSE"), "argument to choose the suggested k from the Gap curve.")
      ),
      h3("Gap rule"),
      tags$ul(
        tags$li(strong("firstSEmax:"), "default option. It chooses the first k whose Gap value is within one standard error of a later maximum. This tends to favor simpler solutions when several k values are similarly supported."),
        tags$li(strong("Tibs2001SEmax:"), "rule proposed with the original Gap statistic. It selects the smallest k satisfying the one-standard-error criterion relative to the next k."),
        tags$li(strong("globalSEmax:"), "uses the global maximum of the Gap curve as the reference, then applies the one-standard-error criterion. It can be more conservative than simply taking the largest Gap value."),
        tags$li(strong("firstmax:"), "chooses the first local maximum of the Gap curve. This ignores the standard-error band and can be more sensitive to small local peaks."),
        tags$li(strong("globalmax:"), "chooses the k with the largest Gap value. This is simple and direct, but it may favor larger k values when the curve keeps increasing slightly.")
      ),
      p("For exploratory use, ", code("firstSEmax"), " is usually a good default because it balances fit and parsimony. If different rules suggest different k values, treat them as candidate solutions rather than as a single definitive answer.")
  )
}


# Plot/statistic helper functions and secondary analysis modules

segment_dd<-function (x) {
  x$segments
}
plotNode<-function (x1, x2, subtree, type, center, leaflab, dLeaf, nodePar,edgePar, horiz = FALSE){
  ddsegments <- NULL
  ddlabels <- list()
  wholetree <- subtree
  depth <- 0L
  llimit <- list()
  KK <- integer()
  kk <- integer()
  repeat {
    inner <- !is.leaf(subtree) && x1 != x2
    yTop <- attr(subtree, "height")
    bx <- plotNodeLimit(x1, x2, subtree, center)
    xTop <- bx$x
    depth <- depth + 1L
    llimit[[depth]] <- bx$limit
    hasP <- !is.null(nPar <- attr(subtree, "nodePar"))
    if (!hasP) {
      nPar <- nodePar}

    Xtract <- function(nam, L, default, indx) rep(if (nam %in%
                                                      names(L)) {L[[nam]] }else {default}, length.out = indx)[indx]
    asTxt <- function(x) if (is.character(x) || is.expression(x) ||is.null(x)) {x}  else {as.character(x)}
    i <- if (inner || hasP) {
      1} else {2}
    if (!is.null(nPar)) {
      pch <- Xtract("pch", nPar, default = 1L:2, i)
      cex <- Xtract("cex", nPar, default = c(1, 1), i)
      col <- Xtract("col", nPar, default = par("col"),
                    i)
      bg <- Xtract("bg", nPar, default = par("bg"), i)
      points(if (horiz)
        cbind(yTop, xTop)
        else cbind(xTop, yTop), pch = pch, bg = bg, col = col,
        cex = cex)
    }
    if (leaflab == "textlike")
      p.col <- Xtract("p.col", nPar, default = "white",
                      i)
    lab.col <- Xtract("lab.col", nPar, default = par("col"),
                      i)
    lab.cex <- Xtract("lab.cex", nPar, default = c(1, 1),
                      i)
    lab.font <- Xtract("lab.font", nPar, default = par("font"),
                       i)
    lab.xpd <- Xtract("xpd", nPar, default = c(TRUE, TRUE),
                      i)
    if (is.leaf(subtree)) {
      if (leaflab == "perpendicular") {
        if (horiz) {
          X <- yTop + dLeaf * lab.cex
          Y <- xTop
          srt <- 0
          adj <- c(0, 0.5)
        }        else {
          Y <- yTop - dLeaf * lab.cex
          X <- xTop
          srt <- 90
          adj <- 1
        }
        nodeText <- asTxt(attr(subtree, "label"))
        ddlabels$xy <- c(ddlabels$xy, X, 0)
        ddlabels$text <- c(ddlabels$text, nodeText)
      }
    }    else if (inner) {
      for (k in seq_along(subtree)) {
        child <- subtree[[k]]
        yBot <- attr(child, "height")
        if (getOption("verbose"))
          cat("ch.", k, "@ h=", yBot, "; ")
        if (is.null(yBot))
          yBot <- 0
        xBot <- if (center) {
          mean(bx$limit[k:(k + 1)])}else {bx$limit[k] + .midDend(child)}
        hasE <- !is.null(ePar <- attr(child, "edgePar"))
        if (!hasE)
          ePar <- edgePar
        i <- if (!is.leaf(child) || hasE) {
          1}  else {2}
        col <- Xtract("col", ePar, default = par("col"),
                      i)
        lty <- Xtract("lty", ePar, default = par("lty"),
                      i)
        lwd <- Xtract("lwd", ePar, default = par("lwd"),
                      i)
        if (type == "triangle") {
          ddsegments <- c(ddsegments, xTop, yTop, xBot,
                          yBot)
        }        else {
          ddsegments <- c(ddsegments, xTop, yTop, xBot,
                          yTop)
          ddsegments <- c(ddsegments, xBot, yTop, xBot,
                          yBot)
        }
        vln <- NULL
      }
    }
    if (inner && length(subtree)) {
      KK[depth] <- length(subtree)
      if (storage.mode(kk) != storage.mode(KK))
        storage.mode(kk) <- storage.mode(KK)
      kk[depth] <- 1L
      x1 <- bx$limit[1L]
      x2 <- bx$limit[2L]
      subtree <- subtree[[1L]]
    }    else {
      repeat {
        depth <- depth - 1L
        if (!depth || kk[depth] < KK[depth])
          break
      }
      if (!depth)
        break
      length(kk) <- depth
      kk[depth] <- k <- kk[depth] + 1L
      x1 <- llimit[[depth]][k]
      x2 <- llimit[[depth]][k + 1L]
      subtree <- wholetree[[kk]]
    }
  }
  list(segments = ddsegments, labels = ddlabels)
}

imesc_dendrogram_data<-function (x, type = c("rectangle", "triangle"), ...) {
  leaflab <- "perpendicular"
  center <- FALSE
  xlab <- ""
  ylab <- ""
  horiz <- FALSE
  xaxt <- "n"
  yaxt <- "s"
  nodePar <- NULL
  edgePar <- list()
  dLeaf <- NULL
  edge.root <- is.leaf(x) || !is.null(attr(x, "edgetext"))
  type <- match.arg(type)
  hgt <- attr(x, "height")
  if (edge.root && is.logical(edge.root)) {
    edge.root <- 0.0625 * if (is.leaf(x)) {
      1}    else {hgt}
  }
  mem.x <- .memberDend(x)
  yTop <- hgt + edge.root
  if (center) {
    x1 <- 0.5
    x2 <- mem.x + 0.5
  }  else {
    x1 <- 1
    x2 <- mem.x
  }
  xl. <- c(x1 - 1/2, x2 + 1/2)
  yl. <- c(0, yTop)
  if (edge.root) {
    if (!is.null(et <- attr(x, "edgetext"))) {
      my <- mean(hgt, yTop)
    }
  }
  ret <- plotNode(x1, x2, x, type = type, center = center,
                  leaflab = leaflab, dLeaf = dLeaf, nodePar = nodePar,                   edgePar = edgePar, horiz = FALSE)
  ret$segments <- as.data.frame(matrix(ret$segments, ncol = 4,
                                       byrow = TRUE, dimnames = list(NULL, c("x", "y", "xend",
                                                                             "yend"))))
  ret$labels <- cbind(as.data.frame(matrix(ret$labels$xy, ncol = 2,byrow = TRUE, dimnames = list(NULL, c("x", "y")))), data.frame(label = ret$labels$text))
  ret
}
.memberDend<-function (x){
  r <- attr(x, "x.member")
  if (is.null(r)) {
    r <- attr(x, "members")
    if (is.null(r)) {
      r <- 1L
    }
  }
  r
}
plotNodeLimit<-function (x1, x2, subtree, center){
  inner <- !is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    mTop <- .memberDend(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      m <- .memberDend(subtree[[k]])
      xx1 <- xx1 + (if (center) {
        (x2 - x1) * m/mTop
      }      else {
        m
      })
      limit[k] <- xx1
    }
    limit <- c(x1, limit)
  }  else {
    limit <- c(x1, x2)
  }
  mid <- attr(subtree, "midpoint")
  center <- center || (inner && !is.numeric(mid))
  x <- if (center) {
    mean(c(x1, x2))
  }  else {
    x1 + (if (inner) {
      mid
    }    else {
      0
    })
  }
  list(x = x, limit = limit)
}
.midDend<-function (x){
  if (is.null(mp <- attr(x, "midpoint")))
    0  else mp
}
add_ggtheme<-function(p,theme,base_size){
  p<-switch(theme,
            'theme_grey'={p+theme_grey(base_size)},
            'theme_bw'={p+theme_bw(base_size)},
            'theme_linedraw'={p+theme_linedraw(base_size)},
            'theme_light'={p+theme_light(base_size)},
            'theme_dark'={p+theme_dark(base_size)},
            'theme_minimal'={p+theme_minimal(base_size)},
            'theme_classic'={p+theme_classic(base_size)},
            'theme_void'={p+theme_void(base_size)})
  p
}
gg_hc_dendrogram<-function(hc.object, labels=NULL, lwd=1, line_color="black", main="", xlab="Observations", ylab="Height", base_size=0.8, theme='theme_minimal', angle_label=0){
  tree_data<-imesc_dendrogram_data(as.dendrogram(hc.object))
  segtree<-segment_dd(tree_data)
  label_data<-tree_data$labels

  if(isFALSE(labels)){
    label_data<-label_data[0,,drop=FALSE]
  } else if(!is.null(labels)){
    label_data$label<-labels[seq_len(nrow(label_data))]
  }

  label_offset<-0
  if(nrow(segtree)>0){
    label_offset<- -0.03*max(segtree$y,segtree$yend,na.rm=TRUE)
  }

  p<-ggplot()+
    geom_segment(
      data=segtree,
      lineend="square",
      aes(x=x,y=y,xend=xend,yend=yend),
      color=line_color,
      linewidth=lwd
    )+
    xlab(xlab)+ylab(ylab)+ggtitle(main)

  if(nrow(label_data)>0){
    p<-p+geom_text(
      data=label_data,
      aes(x=x,y=y,label=label),
      size=base_size*3,
      nudge_y=label_offset,
      angle=angle_label,
      hjust=ifelse(angle_label==0,0.5,1)
    )
  }

  p<-add_ggtheme(p,theme,base_size*12)+
    coord_cartesian(clip="off")+
    ggplot2::theme(plot.margin=ggplot2::margin(5.5,5.5,25,5.5))
  p
}
gg_dendrogram<-function(obs.clusters,hc.clusters,hc.object, palette=NULL, labels=NULL, lwd=2, main="", xlab="Observations", ylab="Height", base_size=12, theme='theme_grey',offset_labels=-.1,xlab_adj=20, legend=c("outside","inside"),base_color="black",angle_label=0,log=F){
  {
    tree_data <- imesc_dendrogram_data(as.dendrogram(hc.object))
    cols<-palette(nlevels(hc.clusters))
    segtree<-segment_dd(tree_data)

    tree_data$labels$group<-hc.clusters[tree_data$labels$label]

    ranges<-data.frame(do.call(rbind,lapply(split(tree_data$labels,tree_data$labels$group),function(x) range(x$x)))
    )

    levs<-levels(hc.clusters)

    group<-lapply(1:nrow(segtree),function(i){
      pic<-as.numeric(which(apply(ranges,1,function(x) between(segtree$xend[i],x[1],x[2]))))
      if(length(pic)==0){pic<-NA}
      pic
    })

    segtree$group<-do.call(c,group)
    segtree$group<-factor(segtree$group,levels=levs)
    #segtree[tree_data$labels$x,"group"]<-hc.clusters[tree_data$labels$label]


    num_clusters<-nlevels(hc.clusters)
    heights <- sort(hc.object$height, decreasing = TRUE)
    cut_height <- heights[num_clusters - 1]


    #  e<-sapply(1:nrow(segtree) ,function(i) segtree$y[i]!=segtree$yend[i])
    #segtree$group[segtree$y>=cut_height&e]<-NA
    segtree2<-segtree
    segtree2$group[segtree$yend>=cut_height]<-NA
  }

  {
    labels_dend<-do.call(rbind,lapply(split(segtree2,segtree2$group),function(x){
      data.frame(x=x$x[which.max(x$y)],y=max(x$y),group=x$group[1])
    }))

    labels_dend$y<-min(labels_dend$y)
    if(!is.null(labels)){tree_data$labels$label<-labels}
    segtree_head<-segtree_a<-segtree[segtree$y>=cut_height,]

    minv<-segtree_a[segtree_a$yend<cut_height,]
    rownames(labels_dend)<-labels_dend$group
    rownames(minv)<-minv$group
    minv[rownames(labels_dend),"y"]<-labels_dend$y
    maxv<-segtree_a[sapply(1:nrow(segtree_a),function(i){
      segtree_a$x[i]!=segtree_a$xend[i]
    }),]
    maxv2<-segtree_a[segtree_a$y>=cut_height,]
    maxv<-maxv[maxv$y<=cut_height,]
    segtree_a<-minv
    segtree_b<-segtree[!is.na(segtree$group),]
    segtree_b<-segtree_b
    segtree_a<-rbind(segtree_a,maxv)
    maxv2$yend[which(maxv2$yend<=cut_height)]<-labels_dend$y[1]
    maxv2<-maxv2[maxv2$y!=cut_height,]

    segtree_head<-maxv2
    p<-ggplot()
    # if(FALSE)
    p<-p+geom_segment(
      data=segtree_b,
      lineend="square",
      aes(x = x,
          y = y,
          xend = xend,
          yend = yend,
          color=group),
      linewidth=lwd
    )
    #if(FALSE)
    p<-p+
      geom_segment(
        data=segtree_head,
        lineend="square",
        aes(x = x,
            y = y,
            xend = xend,
            yend = yend),
        color=base_color,
        linewidth=lwd


      )
    #if(FALSE)
    p<-p+geom_segment(
      data=segtree_a,
      lineend="square",
      aes(x = x,
          y = y,
          xend = xend,
          yend = yend,
          color=group),

      linewidth=lwd)

    p<-p+scale_color_manual(values=cols,name="")


  }

  p<-p+geom_text(aes(x=x,y=y,label=label),data=tree_data$labels,size=base_size*.2,nudge_y=offset_labels,angle=angle_label)



  p<-p+geom_label(data=labels_dend,aes(x,y,label=group),color=cols,nudge_y=-0.1)

  p<-add_ggtheme(p,theme,base_size)+xlab(xlab)+ylab(ylab)+ggtitle(main)
  if(isTRUE(log)){
    p<-p+scale_y_continuous(transform="log")
  }
  p
}

measure_time<-function(f,args) {
  t1<-Sys.time()
  res<-do.call(f,args)
  t2<-Sys.time()
  print(t2-t1)
  res
}

textsilhouetteplot <- function(...){
  div(tags$style(HTML("
   h2 {
      font-size: 20px;
      font-weight: bold;
   }
   h3 {
      font-size: 18px;
      font-weight: normal;
   }
   code {
      color: navy;
   }
")),
      strong("Average silhouette width"),
      br(),
      p("The silhouette method evaluates how well each observation fits within its assigned cluster compared with the nearest alternative cluster."),
      p("Values close to 1 indicate observations that are well matched to their own cluster and poorly matched to neighboring clusters. Values close to 0 indicate observations near cluster boundaries. Negative values suggest possible misclassification or weak separation."),
      p("In iMESc, the suggested k is the number of clusters with the highest average silhouette width. This should be interpreted as an auxiliary diagnostic, not as an automatic decision rule."),
      h3("Options"),
      tags$ul(
        tags$li(strong("k:"), "maximum number of clusters tested by the silhouette routine."),
        tags$li(strong("Suggested k:"), "the k value with the largest average silhouette width.")
      ),
      p("Silhouette is most informative when clusters are compact and well separated. In environmental datasets with gradients, overlapping groups, or strong spatial/temporal structure, the suggested k should be interpreted together with the dendrogram, Gap statistic, WSS/Elbow, cluster sizes, and ecological meaning.")
  )
}


silhouette_module <- list()

silhouette_module$ui <- function(id){
  ns <- NS(id)
  tabPanel(
    "Silhouette",
    value = "silhouette",
    column(
      4, class = "mp0",
      box_caret(
        ns("box_sil_a"),
        title = "Options",
        color = "#c3cc74ff",
        div(
          div(style = "display: flex",
              numericInput(ns("sil_hc_k"), span("k", tipright("maximum number of clusters to be tested")), NULL),
              div(id = ns("run_sil_hc_btn"), style = "display: inline-block; vertical-align: top;", class = "save_changes",
                  actionButton(ns("run_sil_hc"), "Run Silhouette"))
          ),
          div(style = "margin-top: 8px;",
              actionLink(ns("silhelp"), "About silhouette", icon("question-circle")))
        )
      )
    ),
    column(
      8, class = "mp0", style = "position: absolute; right: 0px; padding-left: 6px",
      box_caret(ns("box_sil_b"),
                title = "Results",
                div(uiOutput(ns("sil_tab2_out")))
      )
    )
  )
}

silhouette_module$server <- function(id, vals, getdata_hc, getmodel_hc, model_or_data, som_model_name, disthc, hc_silhouetteplot_fun){
  moduleServer(id, function(input, output, session) {
    box_caret_server("box_sil_a")
    box_caret_server("box_sil_b")

    get_hc_silhouetteplot <- reactive({
      args <- list(
        data = getdata_hc(),
        model_or_data = model_or_data(),
        model_name = som_model_name(),
        disthc = disthc(),
        screeplot_hc_k = input$sil_hc_k,
        use_weights = FALSE,
        whatmap = vals$som_whatmap
      )

      re <- do.call(hc_silhouetteplot_fun, args)
      vals$sil_message <- attr(re, "logs")
      req(!isFALSE(re))

      vals$sil_results <- attr(re, "result")
      vals$sil_plot_hc <- re
    })

    output$sil_error <- renderUI({
      render_message(vals$sil_message)
    })

    output$sil_summary <- renderUI({
      req(vals$sil_results)
      k <- attr(vals$sil_results, "suggested_k")
      req(k)
      div(style = "margin-bottom: 8px; color: #374061ff;",
          strong("Suggested k by maximum average silhouette width: "),
          span(k))
    })

    output$sil_tab2_out <- renderUI({
      div(style = "margin-top: 20px;",
          div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
              strong("Average silhouette width"),
              if(is.null(vals$sil_plot_hc)){NULL}else{actionLink(session$ns("download_sil_plot"), "Download", icon("download"))}),
          uiOutput(session$ns("sil_error")),
          uiOutput(session$ns("sil_summary")),
          if(is.null(vals$sil_plot_hc)){
            div(style = "color: gray;", "Run Silhouette to generate the plot.")
          } else{
            plotOutput(session$ns("plot_hc_silhouetteplot"))
          })
    })

    output$plot_hc_silhouetteplot <- renderPlot({
      req(vals$sil_plot_hc)
      print(vals$sil_plot_hc)
    })

    observe({
      req(model_or_data())
      if(model_or_data() == "som codebook"){
        m <- getmodel_hc()
        data <- m$codes[[1]]
      } else{
        data <- getdata_hc()
      }
      updateNumericInput(session, "sil_hc_k", value = round(nrow(data) / 2))
    })

    observeEvent(input$run_sil_hc, {
      shinyjs::removeClass("run_sil_hc_btn", "save_changes")
      vals$sil_message <- NULL
      req(input$sil_hc_k >= 2)
      withProgress(min = 0, max = 1, message = "Running...", {
        get_hc_silhouetteplot()
      })
    })

    observeEvent(model_or_data(), {
      vals$sil_plot_hc <- NULL
      vals$sil_results <- NULL
    }, ignoreInit = TRUE)

    observeEvent(list(input$sil_hc_k, model_or_data(), som_model_name(), disthc(), vals$som_whatmap), ignoreInit = TRUE, {
      req(input$run_sil_hc)
      shinyjs::addCssClass("run_sil_hc_btn", "save_changes")
      vals$sil_plot_hc <- NULL
      vals$sil_results <- NULL
    })

    observe({
      has_data <- FALSE
      try({
        has_data <- !is.null(getdata_hc())
      }, silent = TRUE)
      shinyjs::toggle("run_sil_hc_btn", condition = has_data)
    })

    observeEvent(input$download_sil_plot, ignoreInit = TRUE, {
      vals$hand_plot <- "generic_gg"
      module_ui_figs("downfigs")
      generic <- vals$sil_plot_hc

      datalist_name <- attr(getdata_hc(), "datalist")
      mod_downcenter <- callModule(module_server_figs, "downfigs", vals = vals, generic = generic,
                                   message = "Average silhouette width", name_c = "silhouette", datalist_name = datalist_name)
    })

    observeEvent(input$silhelp, {
      showModal(
        modalDialog(
          textsilhouetteplot(),
          title = "Average silhouette width",
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l"
        )
      )
    })
  })
}

imesc_vat_order <- function(d){
  d <- as.matrix(d)
  n <- nrow(d)

  if(is.null(n) || n < 2){
    return(list(order = seq_len(n), joins = numeric(0)))
  }

  diag(d) <- 0
  d[!is.finite(d)] <- NA_real_

  max_pos <- arrayInd(which.max(replace(d, is.na(d), -Inf)), dim(d))[1, ]
  selected <- max_pos[1]
  remaining <- setdiff(seq_len(n), selected)
  ord <- integer(n)
  joins <- rep(NA_real_, n)
  ord[1] <- selected

  for(i in 2:n){
    sub_d <- d[selected, remaining, drop = FALSE]
    pos <- arrayInd(which.min(replace(sub_d, is.na(sub_d), Inf)), dim(sub_d))[1, ]
    next_id <- remaining[pos[2]]
    joins[i] <- sub_d[pos[1], pos[2]]
    ord[i] <- next_id
    selected <- c(selected, next_id)
    remaining <- setdiff(remaining, next_id)
  }

  list(order = ord, joins = joins)
}

hc_vatplot <- function(data,
                       model_or_data = "som codebook",
                       model_name = 1,
                       disthc,
                       threshold = 0.90,
                       palette = "magma",
                       show_profile = TRUE,
                       whatmap = NULL,
                       use_weights = FALSE){

  d_log_type <- p_log_type <- NULL
  d_log_message <- p_log_message <- NULL
  dist <- NULL

  if(model_or_data == "som codebook"){
    m <- attr(data, "som")[[model_name]]
    weights <- rep(1, length(whatmap))
    if(isTRUE(use_weights)){
      weights <- NULL
    }
    dist_log <- capture_log1(get_somdist_weighted)(m, weights = weights, whatmap)
    dist <- dist_log[[1]]
  } else{
    dist_log <- capture_log1(vegan::vegdist)(data, disthc)
    dist <- dist_log[[1]]
  }

  d_log_message <- sapply(dist_log$logs, function(x) x$message)
  if(length(d_log_message) == 0){
    d_log_message <- NULL
    d_log_type <- NULL
  } else{
    d_log_type <- sapply(dist_log$logs, function(x) x$type)
  }

  p_log <- capture_log1(function(dist){
    dmat <- as.matrix(dist)
    if(nrow(dmat) < 2){
      stop("VAT requires at least two observations.")
    }

    vat <- imesc_vat_order(dmat)
    d_ord <- dmat[vat$order, vat$order, drop = FALSE]
    max_d <- max(d_ord, na.rm = TRUE)
    if(is.finite(max_d) && max_d > 0){
      d_ord <- d_ord / max_d
    }

    n <- nrow(d_ord)
    vat_df <- expand.grid(x = seq_len(n), y = seq_len(n))
    vat_df$value <- as.vector(d_ord)

    joins <- vat$joins[-1]
    joins <- joins[is.finite(joins)]
    if(length(joins) > 0){
      jump_threshold <- as.numeric(stats::quantile(joins, probs = threshold, na.rm = TRUE, names = FALSE))
      jumps <- which(joins > jump_threshold) + 1
    } else{
      jump_threshold <- NA_real_
      jumps <- integer(0)
    }

    suggested_k <- max(1, length(jumps) + 1)
    if(suggested_k > n){
      suggested_k <- n
    }

    pal_cols <- grDevices::gray.colors(256, start = 0, end = 1)
    if(requireNamespace("viridisLite", quietly = TRUE)){
      pal_cols <- viridisLite::viridis(256, option = palette)
    }

    p <- ggplot2::ggplot(vat_df, ggplot2::aes(x = x, y = y, fill = value)) +
      ggplot2::geom_raster() +
      ggplot2::scale_y_reverse(expand = c(0, 0)) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_fill_gradientn(colors = pal_cols, name = "Dissimilarity") +
      ggplot2::coord_equal() +
      ggplot2::labs(
        title = "VAT reordered dissimilarity image",
        subtitle = paste0("Suggested k = ", suggested_k, " using jumps above the ", threshold, " quantile"),
        x = "VAT order",
        y = "VAT order"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )

    if(isTRUE(show_profile) && length(jumps) > 0){
      p <- p +
        ggplot2::geom_vline(xintercept = jumps - 0.5, color = "#e84a5f", linewidth = 0.45, alpha = 0.85) +
        ggplot2::geom_hline(yintercept = jumps - 0.5, color = "#e84a5f", linewidth = 0.45, alpha = 0.85)
    }

    attr(p, "result") <- data.frame(
      suggested_k = suggested_k,
      n_jumps = length(jumps),
      jump_threshold = jump_threshold,
      threshold_quantile = threshold,
      n = n
    )
    attr(p, "vat_order") <- vat$order
    attr(p, "vat_jumps") <- jumps
    p
  })(dist)

  p <- p_log[[1]]
  p_log_message <- sapply(p_log$logs, function(x) x$message)
  p_log_type <- sapply(p_log$logs, function(x) x$type)
  if(length(p_log_message) == 0){
    p_log_message <- NULL
    p_log_type <- NULL
  }

  if(is.null(p)){
    p <- FALSE
  }

  logs <- c(d_log_message, p_log_message)
  if(!is.null(logs)){
    attr(logs, "type") <- c(d_log_type, p_log_type)
  }
  attr(p, "logs") <- logs
  p
}

hc_silhouetteplot <- function(data,
                              model_or_data = "som codebook",
                              model_name = 1,
                              disthc,
                              screeplot_hc_k,
                              whatmap = NULL,
                              use_weights = FALSE){

  cmd_log_type <- d_log_type <- p_log_type <- NULL
  cmd_log_message <- d_log_message <- p_log_message <- NULL
  extra_args <- list()

  if(model_or_data == "som codebook"){
    m <- attr(data, "som")[[model_name]]

    weights <- rep(1, length(whatmap))
    if(isTRUE(use_weights)){
      weights <- NULL
    }

    dist <- get_somdist_weighted(m, weights = weights, whatmap)

    data_log <- capture_log1(cmdscale)(dist, k = dim(dist)[1] - 1)
    data <- data_log[[1]]

    cmd_log_message <- sapply(data_log$logs, function(x) x$message)
    if(length(cmd_log_message) == 0){
      cmd_log_message <- NULL
      cmd_log_type <- NULL
    } else{
      cmd_log_type <- sapply(data_log$logs, function(x) x$type)
    }

  } else{
    dist_log <- capture_log1(vegan::vegdist)(data, disthc)
    dist <- dist_log[[1]]

    d_log_message <- sapply(dist_log$logs, function(x) x$message)
    if(length(d_log_message) == 0){
      d_log_message <- NULL
      d_log_type <- NULL
    } else{
      d_log_type <- sapply(dist_log$logs, function(x) x$type)
    }
  }

  if(!is.null(data) && !is.null(dist)){
    p_log <- capture_log1(imesc_fviz_nbclust)(
      data,
      factoextra::hcut,
      method = "silhouette",
      k.max = screeplot_hc_k,
      diss = dist
    )
  } else{
    p_log <- list(result = NULL, logs = list(list(message = "Error", type = "error")))
  }

  p <- p_log[[1]]

  if(!is.null(p)){
    p <- p + theme_minimal() + ggtitle("Average Silhouette Method")
  }

  p_log_message <- sapply(p_log$logs, function(x) x$message)
  p_log_type <- sapply(p_log$logs, function(x) x$type)
  if(length(p_log_message) == 0){
    p_log_message <- NULL
    p_log_type <- NULL
  }

  if(length(p) > 0){
    p$data$clusters <- as.numeric(as.character(p$data$clusters))
    re <- p$data
    colnames(re) <- c("Clusters", "Average_silhouette_width")

    best_k <- re$Clusters[which.max(re$Average_silhouette_width)]
    attr(re, "suggested_k") <- best_k
    attr(p, "result") <- re
    attr(p, "suggested_k") <- best_k
  }

  if(is.null(p)){
    p <- FALSE
  }

  logs <- c(cmd_log_message, d_log_message, p_log_message)
  if(!is.null(logs)){
    attr(logs, "type") <- c(cmd_log_type, d_log_type, p_log_type)
  }

  attr(p, "logs") <- logs
  p
}



# Adjusted Rand Index without external packages
imesc_adjusted_rand_index <- function(x, y) {
  x <- as.factor(x)
  y <- as.factor(y)

  if (length(x) != length(y)) {
    stop("x and y must have the same length.")
  }
  if (length(x) < 2) {
    return(NA_real_)
  }

  tab <- table(x, y)
  n <- sum(tab)

  choose2 <- function(z) z * (z - 1) / 2

  sum_ij <- sum(choose2(tab))
  sum_i  <- sum(choose2(rowSums(tab)))
  sum_j  <- sum(choose2(colSums(tab)))
  total  <- choose2(n)

  if (total == 0) {
    return(NA_real_)
  }

  expected <- (sum_i * sum_j) / total
  max_ind  <- 0.5 * (sum_i + sum_j)
  denom    <- max_ind - expected

  if (isTRUE(all.equal(denom, 0))) {
    return(NA_real_)
  }

  (sum_ij - expected) / denom
}

# Internal helper: run hcut in the same spirit as current gap/scree routines
imesc_hcut_cluster <- function(x = NULL, k, hc_metric = NULL, diss = NULL,
                               hc_method = "ward.D2") {
  if (!is.null(diss)) {
    d <- diss
  } else {
    if (is.null(x)) {
      stop("Either x or diss must be provided.")
    }
    if (!is.null(hc_metric) && hc_metric %in% c("bray", "jaccard")) {
      d <- vegan::vegdist(x, method = hc_metric)
    } else if (!is.null(hc_metric)) {
      d <- stats::dist(x, method = hc_metric)
    } else {
      d <- stats::dist(x)
    }
  }

  hc <- stats::hclust(d, method = hc_method)
  stats::cutree(hc, k = k)
}

# Core stability calculation
imesc_hc_stability <- function(x = NULL, diss = NULL, k.max, nboot = 50,
                               subsample_fraction = 0.8, hc_metric = NULL,
                               hc_method = "ward.D2", seed = NULL) {
  if (!is.null(diss)) {
    diss_mat <- as.matrix(diss)
    n <- nrow(diss_mat)
    obs_names <- rownames(diss_mat)
    if (is.null(obs_names)) {
      obs_names <- paste0("obs_", seq_len(n))
      rownames(diss_mat) <- colnames(diss_mat) <- obs_names
    }
  } else {
    x <- as.matrix(x)
    n <- nrow(x)
    if (is.null(rownames(x))) {
      rownames(x) <- paste0("obs_", seq_len(n))
    }
    obs_names <- rownames(x)
  }

  if (k.max < 2) {
    stop("k.max must be >= 2.")
  }
  if (n < 4) {
    stop("At least four observations are required for stability analysis.")
  }
  if (subsample_fraction <= 0 || subsample_fraction >= 1) {
    stop("subsample_fraction must be > 0 and < 1.")
  }
  if (nboot < 1) {
    stop("nboot must be >= 1.")
  }

  k.max <- min(k.max, n - 1)
  n_sub <- max(3, floor(n * subsample_fraction))
  n_sub <- min(n_sub, n - 1)

  if (k.max >= n_sub) {
    k.max <- n_sub - 1
  }
  if (k.max < 2) {
    stop("The selected subsample size is too small for k >= 2.")
  }

  if(is.na(seed)){
    seed<-NULL
  }

  if (!is.null(seed) && length(seed) > 0 && !is.na(seed)) {
    set.seed(seed)
  }

  ks <- 2:k.max

  full_clusters <- lapply(ks, function(k) {
    if (!is.null(diss)) {
      imesc_hcut_cluster(k = k, diss = stats::as.dist(diss_mat), hc_method = hc_method)
    } else {
      imesc_hcut_cluster(x = x, k = k, hc_metric = hc_metric, hc_method = hc_method)
    }
  })
  names(full_clusters) <- as.character(ks)

  boot_rows <- list()
  row_id <- 1

  for (k in ks) {
    full_cl <- full_clusters[[as.character(k)]]

    for (b in seq_len(nboot)) {
      idx <- sample(seq_len(n), size = n_sub, replace = FALSE)
      ari <- NA_real_
      err <- NA_character_

      try_result <- try({
        if (!is.null(diss)) {
          d_sub <- stats::as.dist(diss_mat[idx, idx, drop = FALSE])
          sub_cl <- imesc_hcut_cluster(k = k, diss = d_sub, hc_method = hc_method)
        } else {
          x_sub <- x[idx, , drop = FALSE]
          sub_cl <- imesc_hcut_cluster(x = x_sub, k = k,
                                       hc_metric = hc_metric,
                                       hc_method = hc_method)
        }
        ari <- imesc_adjusted_rand_index(full_cl[idx], sub_cl)
      }, silent = TRUE)

      if (inherits(try_result, "try-error")) {
        err <- as.character(try_result)
      }

      boot_rows[[row_id]] <- data.frame(
        Clusters = k,
        Replicate = b,
        ARI = ari,
        Error = err,
        stringsAsFactors = FALSE
      )
      row_id <- row_id + 1
    }
  }

  boot_df <- do.call(rbind, boot_rows)

  agg <- stats::aggregate(ARI ~ Clusters, data = boot_df, FUN = function(z) {
    c(mean = mean(z, na.rm = TRUE), sd = stats::sd(z, na.rm = TRUE), n = sum(!is.na(z)))
  })

  summary_df <- data.frame(
    Clusters = agg$Clusters,
    Stability = agg$ARI[, "mean"],
    SD = agg$ARI[, "sd"],
    N = agg$ARI[, "n"],
    stringsAsFactors = FALSE
  )

  summary_df$SE <- summary_df$SD / sqrt(summary_df$N)

  if (nrow(summary_df) == 0 || all(is.na(summary_df$Stability))) {
    suggested_k <- NA_integer_
  } else {
    suggested_k <- summary_df$Clusters[which.max(summary_df$Stability)]
  }

  attr(summary_df, "boot") <- boot_df
  attr(summary_df, "suggested_k") <- suggested_k
  attr(summary_df, "subsample_size") <- n_sub
  attr(summary_df, "subsample_fraction") <- subsample_fraction
  attr(summary_df, "nboot") <- nboot
  attr(summary_df, "distance_based") <- !is.null(diss)

  summary_df
}

# Plot helper
imesc_plot_hc_stability <- function(stab_df) {
  if (is.null(stab_df) || isFALSE(stab_df) || !is.data.frame(stab_df) || nrow(stab_df) == 0) {
    return(FALSE)
  }

  suggested_k <- attr(stab_df, "suggested_k")
  if (is.null(suggested_k) || length(suggested_k) == 0) {
    suggested_k <- NA_integer_
  }

  p <- ggplot2::ggplot(stab_df, ggplot2::aes(x = Clusters, y = Stability)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = Stability - SE, ymax = Stability + SE),
      width = 0.12
    ) +
    ggplot2::scale_x_continuous(breaks = stab_df$Clusters) +
    ggplot2::labs(
      title = "Cluster stability analysis",
      subtitle = "Mean Adjusted Rand Index across subsamples",
      x = "Number of clusters k",
      y = "Mean stability (ARI)"
    ) +
    ggplot2::theme_minimal()

  if (length(suggested_k) == 1 && !is.na(suggested_k)) {
    p <- p + ggplot2::geom_vline(xintercept = suggested_k, linetype = 2)
  }

  p
}

# Main function passed into the module server
# Mirrors the current hc_gapplot/hc_screeplot style.
hc_stabilityplot <- function(data, model_or_data = "som codebook", model_name = 1,
                             disthc, screeplot_hc_k, nboot = 50,
                             subsample_fraction = 0.8, seed = NULL,
                             whatmap = NULL, use_weights = FALSE,
                             hc_method = "ward.D2") {
  cmd_log_type <- d_log_type <- p_log_type <- NULL
  cmd_log_message <- d_log_message <- p_log_message <- NULL
  dist <- NULL
  x_for_stability <- NULL

  if (model_or_data == "som codebook") {
    m <- attr(data, "som")[[model_name]]
    weights <- rep(1, length(whatmap))
    if (isTRUE(use_weights)) {
      weights <- NULL
    }

    dist <- get_somdist_weighted(m, weights = weights, whatmap)
    x_for_stability <- NULL
  } else {
    dist_log <- capture_log1(vegan::vegdist)(data, disthc)
    dist <- dist_log[[1]]

    d_log_message <- sapply(dist_log$logs, function(x) x$message)
    if (length(d_log_message) == 0) {
      d_log_message <- NULL
      d_log_type <- NULL
    } else {
      d_log_type <- sapply(dist_log$logs, function(x) x$type)
    }

    x_for_stability <- NULL
  }

  if (!is.null(dist) && !isFALSE(dist)) {
    stab_log <- capture_log1(imesc_hc_stability)(
      x = x_for_stability,
      diss = dist,
      k.max = screeplot_hc_k,
      nboot = nboot,
      subsample_fraction = subsample_fraction,
      hc_method = hc_method,
      seed = seed
    )

    stab_df <- stab_log[[1]]

    if (is.null(stab_df) || isFALSE(stab_df) || !is.data.frame(stab_df)) {
      p <- FALSE
    } else {
      p <- imesc_plot_hc_stability(stab_df)
    }

    p_log_message <- sapply(stab_log$logs, function(x) x$message)
    if (length(p_log_message) == 0) {
      p_log_message <- NULL
      p_log_type <- NULL
    } else {
      p_log_type <- sapply(stab_log$logs, function(x) x$type)
    }
  } else {
    stab_df <- NULL
    p <- FALSE
    p_log_message <- "Error: distance matrix could not be calculated."
    p_log_type <- "error"
  }

  logs <- c(cmd_log_message, d_log_message, p_log_message)
  if (!is.null(logs)) {
    attr(logs, "type") <- c(cmd_log_type, d_log_type, p_log_type)
  }

  if (!isFALSE(p)) {
    attr(p, "result") <- stab_df
    attr(p, "boot") <- attr(stab_df, "boot")
    attr(p, "suggested_k") <- attr(stab_df, "suggested_k")
  }
  attr(p, "logs") <- logs

  p
}



# Help text
textstabilityplot <- function(...) {
  div(tags$style(HTML("
   h2 {
      font-size: 20px;
      font-weight: bold;
   }
   h3 {
      font-size: 18px;
      font-weight: normal;
   }
   code {
      color: navy;
   }
")),
      strong("Stability analysis"),
      br(),
      p("Stability analysis evaluates whether a clustering solution is reproducible when the dataset is slightly perturbed. In iMESc, this is done by repeatedly taking random subsamples without replacement, re-estimating the clusters for each k, and comparing those clusters with the full-data clustering restricted to the same observations."),
      p("Agreement is measured with the", code("Adjusted Rand Index"), ". Values close to 1 indicate very similar partitions, values around 0 indicate agreement close to random expectation, and negative values can occur when agreement is worse than expected by chance."),
      p("The suggested k is the k with the highest mean stability. This should be interpreted as a candidate solution, not as an automatic decision rule."),
      h3("Options"),
      tags$ul(
        tags$li(strong("k:"), "maximum number of clusters tested."),
        tags$li(strong("N subsamples:"), "number of repeated subsamples used to estimate stability. Larger values are more stable but slower."),
        tags$li(strong("Subsample fraction:"), "fraction of observations retained in each subsample. Values around 0.7 to 0.9 are commonly useful for exploratory analysis."),
        tags$li(strong("Seed:"), "optional random seed for reproducibility. Leave empty to avoid setting a seed.")
      ),
      h3("Interpretation"),
      p("A stable k means that similar clusters are recovered when part of the data is removed. However, a stable solution is not necessarily the most ecologically meaningful one. Very coarse partitions can be stable simply because they are simple. Therefore, stability should be read together with Elbow/SMW, Silhouette, Gap statistic, dendrogram structure, cluster sizes, and ecological interpretation.")
  )
}


stability_module <- list()

stability_module$ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Stability",
    value = "stability",
    column(
      4, class = "mp0",
      box_caret(
        ns("box_stab_a"),
        title = "Options",
        color = "#c3cc74ff",
        div(
          div(style = "display: flex",
              numericInput(ns("stab_hc_k"), span("k", tipright("maximum number of clusters to be tested")), NULL),
              div(id = ns("run_stab_hc_btn"), style = "display: inline-block; vertical-align: top;", class = "save_changes",
                  actionButton(ns("run_stab_hc"), "Run Stability"))
          ),
          numericInput(ns("stab_hc_boot"), "N subsamples", 50, min = 1, step = 1),
          numericInput(ns("stab_hc_fraction"),
                       span("Subsample fraction", tiphelp("Fraction of observations retained in each random subsample.")),
                       0.8, min = 0.2, max = 0.95, step = 0.05),
          numericInput(ns("stab_hc_seed"),
                       span("Seed", tiphelp("Optional seed for reproducibility. Leave empty to avoid setting a seed.")),
                       123, step = 1),
          div(actionLink(ns("stabilityhelp"), "What is stability analysis?", icon("question-circle")))
        )
      )
    ),
    column(
      8, class = "mp0", style = "position: absolute; right: 0px; padding-left: 6px",
      box_caret(ns("box_stab_b"),
                title = "Results",
                div(uiOutput(ns("stab_tab2_out")))
      )
    )
  )
}

stability_module$server <- function(id, vals, getdata_hc, getmodel_hc,
                                    model_or_data, som_model_name, disthc,
                                    hc_stabilityplot_fun) {
  moduleServer(id, function(input, output, session) {
    box_caret_server("box_stab_a")
    box_caret_server("box_stab_b")

    get_hc_stabilityplot <- reactive({
      seed_value <- input$stab_hc_seed


      args <- list(
        data = getdata_hc(),
        model_or_data = model_or_data(),
        model_name = som_model_name(),
        disthc = disthc(),
        screeplot_hc_k = input$stab_hc_k,
        nboot = input$stab_hc_boot,
        subsample_fraction = input$stab_hc_fraction,
        seed = seed_value,
        use_weights = FALSE,
        whatmap = vals$som_whatmap
      )

      re <- do.call(hc_stabilityplot_fun, args)
      vals$stability_message <- attr(re, "logs")
      req(!isFALSE(re))

      vals$stability_results <- attr(re, "result")
      vals$stability_suggested_k <- attr(re, "suggested_k")
      vals$stability_plot_hc <- re
    })

    output$stability_error <- renderUI({
      render_message(vals$stability_message)
    })

    output$stab_summary <- renderUI({
      req(vals$stability_results)
      k <- vals$stability_suggested_k
      if (is.null(k) || is.na(k)) {
        return(div(style = "color: gray;", "No stable k suggestion was available."))
      }
      div(
        style = "margin-bottom: 8px;",
        strong("Suggested k: "), k,
        span(style = "color: gray;", " (highest mean ARI across subsamples)")
      )
    })

    output$stab_tab2_out <- renderUI({
      div(style = "margin-top: 20px;",
          div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
              strong("Stability analysis"),
              if (is.null(vals$stability_plot_hc)) {
                NULL
              } else {
                actionLink(session$ns("download_stability_plot"), "Download", icon("download"))
              }),
          uiOutput(session$ns("stability_error")),
          uiOutput(session$ns("stab_summary")),
          if (is.null(vals$stability_plot_hc)) {
            div(style = "color: gray;", "Run stability analysis to generate the plot.")
          } else {
            plotOutput(session$ns("plot_hc_stabilityplot"))
          })
    })

    output$plot_hc_stabilityplot <- renderPlot({
      req(vals$stability_plot_hc)
      print(vals$stability_plot_hc)
    })

    observe({
      req(model_or_data())
      if (model_or_data() == "som codebook") {
        m <- getmodel_hc()
        data <- m$codes[[1]]
      } else {
        data <- getdata_hc()
      }
      updateNumericInput(session, "stab_hc_k", value = max(2, round(nrow(data) / 2)))
    })

    observeEvent(input$run_stab_hc, {
      shinyjs::removeClass("run_stab_hc_btn", "save_changes")
      vals$stability_message <- NULL
      req(input$stab_hc_k >= 2)
      req(input$stab_hc_boot >= 1)
      req(input$stab_hc_fraction > 0)
      req(input$stab_hc_fraction < 1)

      withProgress(min = 0, max = 1, message = "Running...", {
        get_hc_stabilityplot()
      })
    })

    observeEvent(model_or_data(), {
      vals$stability_plot_hc <- NULL
      vals$stability_results <- NULL
      vals$stability_suggested_k <- NULL
    }, ignoreInit = TRUE)

    observeEvent(list(input$stab_hc_k, input$stab_hc_boot, input$stab_hc_fraction,
                      input$stab_hc_seed, model_or_data(), som_model_name(),
                      disthc(), vals$som_whatmap), ignoreInit = TRUE, {
                        req(input$run_stab_hc)
                        shinyjs::addCssClass("run_stab_hc_btn", "save_changes")
                        vals$stability_plot_hc <- NULL
                        vals$stability_results <- NULL
                        vals$stability_suggested_k <- NULL
                      })

    observe({
      has_data <- FALSE
      try({
        has_data <- !is.null(getdata_hc())
      }, silent = TRUE)
      shinyjs::toggle("run_stab_hc_btn", condition = has_data)
    })

    observeEvent(input$download_stability_plot, ignoreInit = TRUE, {
      vals$hand_plot <- "generic_gg"
      module_ui_figs("downfigs")
      generic <- vals$stability_plot_hc

      datalist_name <- attr(getdata_hc(), "datalist")
      mod_downcenter <- callModule(
        module_server_figs, "downfigs",
        vals = vals,
        generic = generic,
        message = "Stability analysis",
        name_c = "stability_analysis",
        datalist_name = datalist_name
      )
    })

    observeEvent(input$stabilityhelp, {
      showModal(
        modalDialog(
          textstabilityplot(),
          title = "Stability analysis",
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l"
        )
      )
    })
  })
}


# Internal helper: cluster a distance matrix using hierarchical clustering
imesc_hclust_from_dist <- function(d, k, method = "ward.D2") {
  if (!inherits(d, "dist")) {
    d <- stats::as.dist(d)
  }
  if (length(k) == 0 || is.na(k) || k < 2) {
    stop("k must be >= 2.")
  }
  hc <- stats::hclust(d, method = method)
  stats::cutree(hc, k = k)
}

# Core consensus clustering calculation
imesc_hc_consensus <- function(d, k.max, nboot = 50, subsample_fraction = 0.8,
                               pac_lower = 0.1, pac_upper = 0.9,
                               seed = NULL, hc_method = "ward.D2",
                               selection_rule = c("max_separation", "max_within", "min_pac")) {
  selection_rule <- match.arg(selection_rule)

  if (!inherits(d, "dist")) {
    d <- stats::as.dist(d)
  }

  D <- as.matrix(d)
  n <- nrow(D)

  if (is.null(rownames(D))) {
    rownames(D) <- paste0("obs_", seq_len(n))
    colnames(D) <- rownames(D)
  }

  if (k.max < 2) {
    stop("k.max must be >= 2.")
  }
  if (n < 4) {
    stop("At least four observations are required for consensus clustering.")
  }
  if (subsample_fraction <= 0 || subsample_fraction >= 1) {
    stop("subsample_fraction must be > 0 and < 1.")
  }
  if (nboot < 1) {
    stop("nboot must be >= 1.")
  }
  if (pac_lower < 0 || pac_upper > 1 || pac_lower >= pac_upper) {
    stop("PAC limits must satisfy 0 <= lower < upper <= 1.")
  }

  k.max <- min(k.max, n - 1)
  n_sub <- max(3, floor(n * subsample_fraction))
  n_sub <- min(n_sub, n - 1)

  if (k.max >= n_sub) {
    k.max <- n_sub - 1
  }
  if (k.max < 2) {
    stop("The selected subsample size is too small for k >= 2.")
  }
  if(is.na(seed)){
    seed<-NULL
  }
  if (!is.null(seed) && !is.na(seed)) {
    set.seed(seed)
  }

  ks <- 2:k.max
  summary_rows <- vector("list", length(ks))
  names(summary_rows) <- as.character(ks)
  consensus_mats <- vector("list", length(ks))
  names(consensus_mats) <- as.character(ks)

  full_clusters <- lapply(ks, function(k) {
    imesc_hclust_from_dist(d, k = k, method = hc_method)
  })
  names(full_clusters) <- as.character(ks)

  for (k in ks) {
    co_cluster <- matrix(0, n, n)
    co_sample  <- matrix(0, n, n)

    for (b in seq_len(nboot)) {
      idx <- sort(sample(seq_len(n), size = n_sub, replace = FALSE))
      d_sub <- stats::as.dist(D[idx, idx, drop = FALSE])
      cl_sub <- imesc_hclust_from_dist(d_sub, k = k, method = hc_method)

      same <- outer(cl_sub, cl_sub, FUN = "==") * 1
      co_cluster[idx, idx] <- co_cluster[idx, idx] + same
      co_sample[idx, idx]  <- co_sample[idx, idx] + 1
    }

    consensus <- co_cluster / co_sample
    consensus[co_sample == 0] <- NA_real_
    diag(consensus) <- 1
    rownames(consensus) <- rownames(D)
    colnames(consensus) <- rownames(D)
    consensus_mats[[as.character(k)]] <- consensus

    # PAC: proportion of pairwise consensus values in the ambiguous interval
    upper_tri <- upper.tri(consensus)
    cvals <- consensus[upper_tri]
    cvals <- cvals[!is.na(cvals)]

    pac <- if (length(cvals) == 0) {
      NA_real_
    } else {
      mean(cvals > pac_lower & cvals < pac_upper, na.rm = TRUE)
    }

    # Within/between consensus according to the full-data clustering
    full_cl <- full_clusters[[as.character(k)]]
    same_full <- outer(full_cl, full_cl, FUN = "==")
    same_vals <- consensus[upper.tri(consensus) & same_full]
    diff_vals <- consensus[upper.tri(consensus) & !same_full]

    mean_within <- mean(same_vals, na.rm = TRUE)
    mean_between <- mean(diff_vals, na.rm = TRUE)
    separation <- mean_within - mean_between

    summary_rows[[as.character(k)]] <- data.frame(
      Clusters = k,
      Mean_within_consensus = mean_within,
      Mean_between_consensus = mean_between,
      Consensus_separation = separation,
      PAC = pac,
      One_minus_PAC = 1 - pac,
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL

  suggested_k <- NA_integer_
  if (selection_rule == "max_separation") {
    if (!all(is.na(summary_df$Consensus_separation))) {
      suggested_k <- summary_df$Clusters[which.max(summary_df$Consensus_separation)]
    }
  } else if (selection_rule == "max_within") {
    if (!all(is.na(summary_df$Mean_within_consensus))) {
      suggested_k <- summary_df$Clusters[which.max(summary_df$Mean_within_consensus)]
    }
  } else if (selection_rule == "min_pac") {
    if (!all(is.na(summary_df$PAC))) {
      suggested_k <- summary_df$Clusters[which.min(summary_df$PAC)]
    }
  }

  attr(summary_df, "suggested_k") <- suggested_k
  attr(summary_df, "consensus_mats") <- consensus_mats
  attr(summary_df, "subsample_size") <- n_sub
  attr(summary_df, "subsample_fraction") <- subsample_fraction
  attr(summary_df, "nboot") <- nboot
  attr(summary_df, "pac_lower") <- pac_lower
  attr(summary_df, "pac_upper") <- pac_upper
  attr(summary_df, "selection_rule") <- selection_rule

  summary_df
}


# Plot helper
imesc_plot_hc_consensus <- function(cons_df) {
  suggested_k <- attr(cons_df, "suggested_k")
  rule <- attr(cons_df, "selection_rule")

  plot_df <- rbind(
    data.frame(Clusters = cons_df$Clusters, Metric = "Mean within consensus", Value = cons_df$Mean_within_consensus),
    data.frame(Clusters = cons_df$Clusters, Metric = "Consensus separation", Value = cons_df$Consensus_separation),
    data.frame(Clusters = cons_df$Clusters, Metric = "1 - PAC", Value = cons_df$One_minus_PAC)
  )

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Clusters, y = Value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_continuous(breaks = cons_df$Clusters) +
    ggplot2::labs(
      title = "Consensus clustering",
      subtitle = paste0("Candidate k selected by: ", rule),
      x = "Number of clusters k",
      y = "Diagnostic value"
    ) +
    ggplot2::theme_minimal()

  if (length(suggested_k) > 0 && !is.na(suggested_k)) {
    p <- p + ggplot2::geom_vline(xintercept = suggested_k, linetype = 2)
  }

  p
}


# Main function passed into the module server
hc_consensusplot <- function(data, model_or_data = "som codebook", model_name = 1,
                             disthc, screeplot_hc_k, nboot = 50,
                             subsample_fraction = 0.8,
                             pac_lower = 0.1, pac_upper = 0.9,
                             seed = NULL, whatmap = NULL,
                             use_weights = FALSE,
                             hc_method = "ward.D2",
                             selection_rule = c("max_separation", "max_within", "min_pac")) {
  selection_rule <- match.arg(selection_rule)

  cmd_log_type <- d_log_type <- p_log_type <- NULL
  cmd_log_message <- d_log_message <- p_log_message <- NULL
  dist <- NULL

  if (is.null(hc_method) || length(hc_method) == 0 || is.na(hc_method)) {
    hc_method <- "ward.D2"
  }

  if (model_or_data == "som codebook") {
    m <- attr(data, "som")[[model_name]]
    weights <- rep(1, length(whatmap))
    if (isTRUE(use_weights)) {
      weights <- NULL
    }

    dist_log <- capture_log1(get_somdist_weighted)(m, weights = weights, whatmap)
    dist <- dist_log[[1]]

    cmd_log_message <- sapply(dist_log$logs, function(x) x$message)
    if (length(cmd_log_message) == 0) {
      cmd_log_message <- NULL
      cmd_log_type <- NULL
    } else {
      cmd_log_type <- sapply(dist_log$logs, function(x) x$type)
    }
  } else {
    dist_log <- capture_log1(vegan::vegdist)(data, disthc)
    dist <- dist_log[[1]]

    d_log_message <- sapply(dist_log$logs, function(x) x$message)
    if (length(d_log_message) == 0) {
      d_log_message <- NULL
      d_log_type <- NULL
    } else {
      d_log_type <- sapply(dist_log$logs, function(x) x$type)
    }
  }

  if (!is.null(dist) && !isFALSE(dist)) {
    cons_log <- capture_log1(imesc_hc_consensus)(
      d = dist,
      k.max = screeplot_hc_k,
      nboot = nboot,
      subsample_fraction = subsample_fraction,
      pac_lower = pac_lower,
      pac_upper = pac_upper,
      seed = seed,
      hc_method = hc_method,
      selection_rule = selection_rule
    )

    cons_df <- cons_log[[1]]

    if (!is.null(cons_df) && !isFALSE(cons_df)) {
      p <- imesc_plot_hc_consensus(cons_df)
    } else {
      p <- FALSE
    }

    p_log_message <- sapply(cons_log$logs, function(x) x$message)
    if (length(p_log_message) == 0) {
      p_log_message <- NULL
      p_log_type <- NULL
    } else {
      p_log_type <- sapply(cons_log$logs, function(x) x$type)
    }
  } else {
    cons_df <- NULL
    p <- FALSE
    p_log_message <- "Consensus clustering could not be calculated."
    p_log_type <- "error"
  }

  logs <- c(cmd_log_message, d_log_message, p_log_message)
  if (!is.null(logs)) {
    attr(logs, "type") <- c(cmd_log_type, d_log_type, p_log_type)
  }

  if (!isFALSE(p)) {
    attr(p, "result") <- cons_df
    attr(p, "consensus_mats") <- attr(cons_df, "consensus_mats")
    attr(p, "suggested_k") <- attr(cons_df, "suggested_k")
  }
  attr(p, "logs") <- logs

  p
}


# Help text
textconsensusplot <- function(...) {
  div(tags$style(HTML("
   h2 {
      font-size: 20px;
      font-weight: bold;
   }
   h3 {
      font-size: 18px;
      font-weight: normal;
   }
   code {
      color: navy;
   }
")),
      strong("Consensus clustering"),
      br(),
      p("Consensus clustering evaluates how often pairs of observations are assigned to the same cluster across repeated subsamples of the data."),
      p("For each k, iMESc repeatedly samples a fraction of the observations, reclusters the subsample, and accumulates a consensus matrix. Values close to 1 indicate pairs that are almost always grouped together. Values close to 0 indicate pairs that are rarely grouped together."),
      h3("Diagnostics"),
      tags$ul(
        tags$li(strong("Mean within consensus:"), "average consensus among pairs that belong to the same cluster in the full-data solution. Higher values indicate more reproducible clusters."),
        tags$li(strong("Consensus separation:"), "mean within-cluster consensus minus mean between-cluster consensus. Higher values indicate clearer separation."),
        tags$li(strong("PAC:"), "proportion of ambiguous clustering. It is the proportion of consensus values between the selected lower and upper bounds. Lower PAC means fewer ambiguous pairwise assignments.")
      ),
      h3("Options"),
      tags$ul(
        tags$li(strong("k:"), "maximum number of clusters tested."),
        tags$li(strong("N subsamples:"), "number of repeated subsamples used to estimate the consensus matrix."),
        tags$li(strong("Subsample fraction:"), "fraction of observations retained in each subsample."),
        tags$li(strong("PAC lower / upper:"), "bounds used to define ambiguous consensus values."),
        tags$li(strong("Selection rule:"), "criterion used to place the dashed candidate-k line."),
        tags$li(strong("Seed:"), "optional random seed for reproducibility. Leave empty to avoid setting a seed.")
      ),
      h3("Interpretation"),
      p("Consensus clustering is useful for evaluating robustness, but it does not define the true number of clusters. Very small k values may look stable simply because they are coarse. Use this diagnostic together with Elbow/SMW, Silhouette, Gap statistic, Stability analysis, dendrogram structure, cluster sizes, and ecological interpretation.")
  )
}


# Shiny module
consensus_module <- list()

consensus_module$ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Consensus",
    value = "consensus",
    column(
      4, class = "mp0",
      box_caret(
        ns("box_cons_a"),
        title = "Options",
        color = "#c3cc74ff",
        div(
          div(style = "display: flex",
              numericInput(ns("cons_hc_k"), span("k", tipright("maximum number of clusters to be tested")), NULL),
              div(id = ns("run_cons_hc_btn"), style = "display: inline-block; vertical-align: top;", class = "save_changes",
                  actionButton(ns("run_cons_hc"), "Run Consensus"))
          ),
          numericInput(ns("cons_hc_boot"), "N subsamples", 50, min = 1, step = 1),
          numericInput(ns("cons_hc_fraction"),
                       span("Subsample fraction", tiphelp("Fraction of observations retained in each random subsample.")),
                       0.8, min = 0.2, max = 0.95, step = 0.05),
          numericInput(ns("cons_pac_lower"),
                       span("PAC lower", tiphelp("Lower bound used to define ambiguous consensus values.")),
                       0.1, min = 0, max = 0.95, step = 0.05),
          numericInput(ns("cons_pac_upper"),
                       span("PAC upper", tiphelp("Upper bound used to define ambiguous consensus values.")),
                       0.9, min = 0.05, max = 1, step = 0.05),
          selectInput(ns("cons_rule"),
                      span("Selection rule", tiphelp("Rule used to choose the candidate k shown by the dashed line.")),
                      choices = c("Max consensus separation" = "max_separation",
                                  "Max mean within consensus" = "max_within",
                                  "Min PAC" = "min_pac"),
                      selected = "max_separation"),
          numericInput(ns("cons_hc_seed"),
                       span("Seed", tiphelp("Optional seed for reproducibility. Leave empty to avoid setting a seed.")),
                       123, step = 1),
          div(actionLink(ns("consensushelp"), "What is consensus clustering?", icon("question-circle")))
        )
      )
    ),
    column(
      8, class = "mp0", style = "position: absolute; right: 0px; padding-left: 6px",
      box_caret(ns("box_cons_b"),
                title = "Results",
                div(uiOutput(ns("cons_tab2_out")))
      )
    )
  )
}

consensus_module$server <- function(id, vals, getdata_hc, getmodel_hc,
                                    model_or_data, som_model_name, disthc,
                                    hc_consensusplot_fun) {
  moduleServer(id, function(input, output, session) {
    box_caret_server("box_cons_a")
    box_caret_server("box_cons_b")

    get_hc_consensusplot <- reactive({
      seed_value <- input$cons_hc_seed
      if (length(seed_value) == 0 || is.na(seed_value)) {
        seed_value <- NULL
      }

      hc_method_value <- vals$method.hc0
      if (is.null(hc_method_value) || length(hc_method_value) == 0 || is.na(hc_method_value)) {
        hc_method_value <- "ward.D2"
      }

      args <- list(
        data = getdata_hc(),
        model_or_data = model_or_data(),
        model_name = som_model_name(),
        disthc = disthc(),
        screeplot_hc_k = input$cons_hc_k,
        nboot = input$cons_hc_boot,
        subsample_fraction = input$cons_hc_fraction,
        pac_lower = input$cons_pac_lower,
        pac_upper = input$cons_pac_upper,
        seed = seed_value,
        use_weights = FALSE,
        whatmap = vals$som_whatmap,
        hc_method = hc_method_value,
        selection_rule = input$cons_rule
      )

      re <- do.call(hc_consensusplot_fun, args)
      vals$consensus_message <- attr(re, "logs")
      req(!isFALSE(re))

      vals$consensus_results <- attr(re, "result")
      vals$consensus_plot_hc <- re
    })

    output$consensus_error <- renderUI({
      render_message(vals$consensus_message)
    })

    output$cons_tab2_out <- renderUI({
      div(style = "margin-top: 20px;",
          div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
              strong("Consensus clustering"),
              if (is.null(vals$consensus_plot_hc)) {
                NULL
              } else {
                actionLink(session$ns("download_consensus_plot"), "Download", icon("download"))
              }),
          uiOutput(session$ns("consensus_error")),
          if (is.null(vals$consensus_plot_hc)) {
            div(style = "color: gray;", "Run Consensus clustering to generate the plot.")
          } else {
            tagList(
              uiOutput(session$ns("consensus_suggested")),
              plotOutput(session$ns("plot_hc_consensusplot"))
            )
          })
    })

    output$consensus_suggested <- renderUI({
      req(vals$consensus_results)
      suggested_k <- attr(vals$consensus_results, "suggested_k")
      rule <- attr(vals$consensus_results, "selection_rule")
      if (length(suggested_k) == 0 || is.na(suggested_k)) {
        return(div(style = "color: gray;", "No candidate k could be suggested."))
      }
      div(style = "margin-bottom: 8px;",
          strong("Candidate k: "), suggested_k,
          span(style = "color: gray;", paste0("  | rule: ", rule)))
    })

    output$plot_hc_consensusplot <- renderPlot({
      req(vals$consensus_plot_hc)
      print(vals$consensus_plot_hc)
    })

    observe({
      req(model_or_data())
      if (model_or_data() == "som codebook") {
        m <- getmodel_hc()
        data <- m$codes[[1]]
      } else {
        data <- getdata_hc()
      }
      updateNumericInput(session, "cons_hc_k", value = max(2, round(nrow(data) / 2)))
    })

    observeEvent(input$run_cons_hc, {
      shinyjs::removeClass("run_cons_hc_btn", "save_changes")
      vals$consensus_message <- NULL
      req(input$cons_hc_k >= 2)
      req(input$cons_hc_boot >= 1)
      req(input$cons_hc_fraction > 0 && input$cons_hc_fraction < 1)
      req(input$cons_pac_lower >= 0 && input$cons_pac_upper <= 1)
      req(input$cons_pac_lower < input$cons_pac_upper)
      withProgress(min = 0, max = 1, message = "Running...", {
        get_hc_consensusplot()
      })
    })

    observeEvent(model_or_data(), {
      vals$consensus_plot_hc <- NULL
      vals$consensus_results <- NULL
    }, ignoreInit = TRUE)

    observeEvent(list(input$cons_hc_k, input$cons_hc_boot, input$cons_hc_fraction,
                      input$cons_pac_lower, input$cons_pac_upper, input$cons_rule,
                      input$cons_hc_seed, model_or_data(), som_model_name(),
                      disthc(), vals$som_whatmap, vals$method.hc0),
                 ignoreInit = TRUE, {
                   req(input$run_cons_hc)
                   shinyjs::addCssClass("run_cons_hc_btn", "save_changes")
                   vals$consensus_plot_hc <- NULL
                   vals$consensus_results <- NULL
                 })

    observe({
      has_data <- FALSE
      try({
        has_data <- !is.null(getdata_hc())
      }, silent = TRUE)
      shinyjs::toggle("run_cons_hc_btn", condition = has_data)
    })

    observeEvent(input$download_consensus_plot, ignoreInit = TRUE, {
      vals$hand_plot <- "generic_gg"
      module_ui_figs("downfigs")
      generic <- vals$consensus_plot_hc

      datalist_name <- attr(getdata_hc(), "datalist")
      mod_downcenter <- callModule(module_server_figs, "downfigs", vals = vals,
                                   generic = generic,
                                   message = "Consensus clustering",
                                   name_c = "consensus_clustering",
                                   datalist_name = datalist_name)
    })

    observeEvent(input$consensushelp, {
      showModal(
        modalDialog(
          textconsensusplot(),
          title = "Consensus clustering",
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l"
        )
      )
    })
  })
}






# Hierarchical clustering modules

elbow_smw_module<-list()
elbow_smw_module$ui<-function(id){
  ns<-NS(id)
  tabPanel(
    "Elbow / SMW",
    value="elbow_smw",
    column(
      4,class="mp0",
      box_caret(
        ns("box2_a"),
        title="Options",
        color="#c3cc74ff",
        div(
          div(style="display: flex",
              numericInput(ns("screeplot_hc_k"), span("k", tipright("maximum number of clusters to be tested")),NULL),
              div(id=ns('run_screeplot_hc_btn'),style="display: inline-block; vertical-align: top;", class="save_changes",actionButton(ns("run_screeplot_hc"), "Run screeplot"))
          ),
          div(style="margin-top: 20px; border-top: 1px solid gray",
              div(style="display: flex",
                  checkboxInput(ns("show_smw_hc"), value = F,
                                strong(
                                  "split moving window",
                                  tiphelp_icon(actionLink(ns('screeplothelp0'), icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),"Performs split moving window to detect significant discontinuities in the relationship between the number of clusters and WSS values. Click for more information.")
                                )),
                  div(id=ns('run_smw_hc_btn'),class="save_changes",actionButton(ns("run_smw_hc"),"RUN smw")),
                  inline(uiOutput(ns("smw_validate")))
              ),
              div(id=ns("hc_smw_control"),
                  selectInput(ns("smw_hc_target"),
                              span("SMW target",tiphelp("Series used by SMW. Marginal gain is the WSS reduction obtained when adding one more cluster.")),
                              choices=c("Marginal gain"="marginal_gain","WSS"="wss"),
                              selected="marginal_gain"),
                  selectInput(ns("smw_hc_threshold"),
                              span("Threshold",tiphelp("Rule used to mark potential breakpoints.")),
                              choices=c("Random quantile"="random_quantile","tol * SD"="tol_sd"),
                              selected="random_quantile"),
                  uiOutput(ns("smw_hc_w_out")),
                  numericInput(ns("smw_hc_rand"),"N randomizations",50),
                  numericInput(ns("smw_hc_conf"),
                               span("confidence",tiphelp("Randomization quantile used as the breakpoint threshold.")),
                               0.95,min=0.5,max=0.999,step=0.01),
                  numericInput(ns("smw_hc_tol"), span("tol", tiphelp("Adjusts sensitivity when identifying potential breakpoints. If the dissimilarity score (DS) exceeds -tol- times the standard deviation, a breakpoint is suggested.")), 1.5, step=0.1)
              )),
          div(actionLink(ns('down_results_screeplot'),"Download Results"))
        )
      )
    ),
    column(
      8,class="mp0",style="position: absolute; right: 0px; padding-left: 6px",
      box_caret(ns("box2_b"),
                title="Plot",
                div(
                  uiOutput(ns('smw_error')),
                  uiOutput(ns('smw_error2')),
                  uiOutput(ns("hc_tab2_out"))
                )
      )
    )
  )
}

elbow_smw_module$server<-function(id, vals, getdata_hc, getmodel_hc, model_or_data, som_model_name, disthc, hc_screeplot_fun){
  moduleServer(id,function(input, output, session) {
    box_caret_server("box2_a")
    box_caret_server("box2_b")

    getsmw_plot<-reactive({
      tol=input$smw_hc_tol
      threshold_method<-input$smw_hc_threshold
      p<-vals$scree_plot_hc0
      req(vals$screeplot_results)
      if(ncol(vals$screeplot_results)>2){
        smw<-vals$screeplot_results
        smw2<-get_screeplot_smw_sig(smw,tol,threshold_method=threshold_method)
        df<-get_ggdata_screesms(smw2,tol,threshold_method=threshold_method)
        p<-scree_smw_ggplot(df)
      }
      p
    })
    get_hc_screeplot<-reactive({
      args<-list(
        data=getdata_hc(),
        model_or_data=model_or_data(),
        model_name=som_model_name(),
        disthc=disthc(),
        screeplot_hc_k=input$screeplot_hc_k,
        use_weights=F,
        whatmap=vals$som_whatmap
      )

      re<-do.call(hc_screeplot_fun,args)
      vals$smw_message<-attr(re,"logs")
      req(!isFALSE(re[1]))

      result<-attr(re,"result")
      vals$screeplot_results0<-vals$screeplot_results<-result
      vals$scree_plot_hc<-vals$scree_plot_hc0<-re
    })
    output$smw_error2<-renderUI({
      render_message(vals$smw_message2)
    })
    output$smw_error<-renderUI({
      render_message(vals$smw_message)
    })
    output$smw_validate<-renderUI({
      req(isTRUE(input$show_smw_hc))
      req(length(input$smw_hc_w)>0)
      ws<-as.numeric(unlist(strsplit(input$smw_hc_w,",")))
      n_series<-length(vals$screeplot_results0$WSS)
      if(identical(input$smw_hc_target,"marginal_gain")){
        n_series<-n_series-1
      }
      max<-floor(n_series/2)
      validate(need(!any(is.na(ws)),"Running SMW is unavailable because window sizes must be numeric."))
      validate(need(!any(ws>max),
                    "Running SMW is unavailable because the maximum size should not exceed half of the selected SMW series."))
      validate(need(!any(ws%%2 == 1),"Running SMW is unavailable as all window sizes must be even."))
      validate(need(input$smw_hc_conf>0&input$smw_hc_conf<1,
                    "Running SMW is unavailable because confidence must be between 0 and 1."))
      NULL
    })
    output$smw_hc_w_out<-renderUI({
      req(vals$screeplot_results0$WSS)
      n_series<-length(vals$screeplot_results0$WSS)
      if(identical(input$smw_hc_target,"marginal_gain")){
        n_series<-n_series-1
      }
      max_w<-floor(n_series/2)
      ws<-if(max_w>=2){seq(2,max_w,by=2)} else{numeric(0)}
      tags$div(id="smw_hc_pool",
               textInput(
                 session$ns("smw_hc_w"),
                 span("Window sizes",tiphelp("comma-delimeted")),
                 value = paste0(ws,collapse=", ")
               )
      )
    })
    output$hc_tab2_out<-renderUI({
      req(!is.null(vals$screeplot_results))
      div(
        div(style="display: flex; justify-content: space-between; margin-bottom: 5px;",
            strong("Scree plot",
                   actionLink(session$ns('screeplothelp'), icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle"))),
            actionLink(session$ns("download_plot2"),"Download",icon("download"))
        ),
        div(plotOutput(session$ns('plot_hc_screeplot')))
      )
    })
    output$plot_hc_screeplot <-renderPlot({
      vals$scree_plot_hc<-getsmw_plot()
      print(vals$scree_plot_hc)
    })
    observe({
      req(model_or_data())
      if(model_or_data()=="som codebook"){
        m<-getmodel_hc()
        data<-m$codes[[1]]
      } else{
        data<-getdata_hc()
      }
      updateNumericInput(session,"screeplot_hc_k", value = round(nrow(data)/2))
    })
    observeEvent(input$run_screeplot_hc,ignoreInit = T,{
      shinyjs::removeClass('run_screeplot_hc_btn',"save_changes")
      get_hc_screeplot()
    })
    observeEvent(input$run_smw_hc,{
      shinyjs::removeClass("run_smw_hc_btn","save_changes")
      vals$run_smw_hc<-T
      vals$screeplot_results<-vals$screeplot_results0
      vals$scree_plot_hc<- vals$scree_plot_hc0

      req(input$smw_hc_rand>2)
      result<-vals$screeplot_results
      n.rand=input$smw_hc_rand
      ws<-as.numeric(unlist(strsplit(input$smw_hc_w,",")))
      smwlog<-capture_log1(smwhc)(result, n.rand, ws,
                                  target=input$smw_hc_target,
                                  conf=input$smw_hc_conf)

      smw<-smwlog[[1]]
      logs<-sapply(smwlog$logs,function(x) x$message)
      attr(logs,"type")<-sapply(smwlog$logs,function(x) x$type)
      if(length(logs)==0){
        logs<-NULL
      }
      vals$smw_message2<-logs
      req(smw)
      vals$screeplot_results<-smw
    })
    observeEvent(input$show_smw_hc,{
      vals$show_smw_hc<-input$show_smw_hc
    })
    observeEvent(input$smw_hc_rand,{
      vals$smw_hc_rand<-input$smw_hc_rand
    })
    observeEvent(input$smw_hc_tol,{
      vals$smw_hc_tol<-input$smw_hc_tol
    })
    observeEvent(input$smw_hc_target,{
      vals$smw_hc_target<-input$smw_hc_target
    })
    observeEvent(input$smw_hc_threshold,{
      vals$smw_hc_threshold<-input$smw_hc_threshold
    })
    observeEvent(input$smw_hc_conf,{
      vals$smw_hc_conf<-input$smw_hc_conf
    })
    observeEvent(model_or_data(),{
      vals$screeplot_results<-NULL
    },ignoreInit = TRUE)
    observe({
      shinyjs::toggle('run_smw_hc', condition =isTRUE(input$show_smw_hc) )
    })
    observe({
      req(isTRUE(input$show_smw_hc))
      req(length(input$smw_hc_w)>0)
      ws<-as.numeric(unlist(strsplit(input$smw_hc_w,",")))
      n_series<-length(vals$screeplot_results0$WSS)
      if(identical(input$smw_hc_target,"marginal_gain")){
        n_series<-n_series-1
      }
      max<-floor(n_series/2)
      shinyjs::toggle("run_smw_hc",condition=!any(is.na(ws))&!any(ws>max)&!any(ws%%2 == 1)&input$smw_hc_conf>0&input$smw_hc_conf<1)
    })
    observe({
      shinyjs::toggleClass('run_smw_hc_btn',class='save_changes',condition=!isTRUE(vals$run_smw_hc))
    })
    observe({
      shinyjs::toggle("hc_smw_control",condition=isTRUE(input$show_smw_hc))
    })
    observe({
      shinyjs::toggle("smw_hc_conf",condition=identical(input$smw_hc_threshold,"random_quantile"))
      shinyjs::toggle("smw_hc_tol",condition=identical(input$smw_hc_threshold,"tol_sd"))
    })
    observeEvent(list(input$screeplot_hc_k,model_or_data(),som_model_name(),disthc(),vals$som_whatmap),ignoreInit = T,{
      req(input$run_screeplot_hc)
      shinyjs::addCssClass('run_screeplot_hc_btn',"save_changes")
      vals$screeplot_results<-NULL
    })
    observeEvent(list(
      input$smw_hc_w,
      input$smw_hc_rand,
      input$smw_hc_target,
      input$smw_hc_conf
    ),ignoreInit = T,{
      vals$run_smw_hc<-F
    })
    observe(shinyjs::toggle("run_smw_hc_btn",condition = length(vals$screeplot_results0$WSS)>0))
    observeEvent(input$screeplothelp,{
      showModal(
        modalDialog(
          textscreeplot(),
          title="Scree plot",
          easyClose=TRUE,
          footer=modalButton("Close"),
          size="l"
        )
      )
    })

    observeEvent(input$screeplothelp0,{
      showModal(
        modalDialog(
          textscreeplot(),
          title="Scree plot",
          easyClose=TRUE,
          footer=modalButton("Close"),
          size="l"
        )
      )
    })
    observeEvent(input$download_plot2,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=getsmw_plot()

      datalist_name=attr(getdata_hc(),'datalist')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Scree plot", name_c="screeplot",datalist_name=datalist_name)
    })
    observeEvent(ignoreInit = T,input$down_results_screeplot,{
      vals$hand_down<-"screeplot"
      module_ui_downcenter("downcenter")
      mod_downcenter<-callModule(module_server_downcenter, "downcenter", vals=vals)
    })
  })
}

gap_module<-list()
gap_module$ui<-function(id){
  ns<-NS(id)
  tabPanel(
    "Gap statistic",
    value="gap_stat",
    column(
      4,class="mp0",
      box_caret(
        ns("box_gap_a"),
        title="Options",
        color="#c3cc74ff",
        div(
          div(style="display: flex",
              numericInput(ns("gap_hc_k"), span("k", tipright("maximum number of clusters to be tested")),NULL),
              div(id=ns('run_gap_hc_btn'),style="display: inline-block; vertical-align: top;", class="save_changes",actionButton(ns("run_gap_hc"),"Run Gap"))
          ),
          numericInput(ns("gap_hc_boot"),"N bootstraps",50,min=1,step=1),
          selectInput(ns("gap_hc_rule"),
                      span("Gap rule",
                           tiphelp(
                             tags$span(
                               tags$div("Rule used by maxSE to choose k from the Gap statistic."),
                               tags$li(tags$code("firstSEmax:"), "default option. It chooses the first k whose Gap value is within one standard error of a later maximum. This tends to favor simpler solutions when several k values are similarly supported."),
                               tags$li(tags$code("Tibs2001SEmax:"), "rule proposed with the original Gap statistic. It selects the smallest k satisfying the one-standard-error criterion relative to the next k."),
                               tags$li(tags$code("globalSEmax:"), "uses the global maximum of the Gap curve as the reference, then applies the one-standard-error criterion. It can be more conservative than simply taking the largest Gap value."),
                               tags$li(tags$code("firstmax:"), "chooses the first local maximum of the Gap curve. This ignores the standard-error band and can be more sensitive to small local peaks."),
                               tags$li(tags$code("globalmax:"), "chooses the k with the largest Gap value. This is simple and direct, but it may favor larger k values when the curve keeps increasing slightly.")
                             ),"right"

                           )),
                      choices=c("Tibs2001SEmax","firstSEmax","globalSEmax","firstmax","globalmax"),
                      selected="Tibs2001SEmax"),
          div(style = "margin-top: 8px;",
              actionLink(ns("gaphelp"), "About Gap statistic", icon("question-circle")))
        )
      )
    ),
    column(
      8,class="mp0",style="position: absolute; right: 0px; padding-left: 6px",
      box_caret(ns("box_gap_b"),
                title="Results",
                div(uiOutput(ns("gap_tab2_out")))
      )
    )
  )
}

gap_module$server<-function(id, vals, getdata_hc, getmodel_hc, model_or_data, som_model_name, disthc, hc_gapplot_fun){
  moduleServer(id,function(input, output, session) {
    box_caret_server("box_gap_a")
    box_caret_server("box_gap_b")

    get_hc_gapplot<-reactive({
      rule<-input$gap_hc_rule
      maxSE<-if(rule%in%c("firstmax","globalmax")){
        list(method=rule)
      } else{
        list(method=rule,SE.factor=1)
      }
      args<-list(
        data=getdata_hc(),
        model_or_data=model_or_data(),
        model_name=som_model_name(),
        disthc=disthc(),
        screeplot_hc_k=input$gap_hc_k,
        nboot=input$gap_hc_boot,
        maxSE=maxSE,
        use_weights=F,
        whatmap=vals$som_whatmap
      )

      re<-do.call(hc_gapplot_fun,args)
      vals$gap_message<-attr(re,"logs")
      req(!isFALSE(re))

      vals$gap_plot_hc<-re
    })
    output$gap_error<-renderUI({
      render_message(vals$gap_message)
    })
    output$gap_tab2_out<-renderUI({
      div(style="margin-top: 20px;",
          div(style="display: flex; justify-content: space-between; margin-bottom: 5px;",
              strong("Gap statistic"),
              if(is.null(vals$gap_plot_hc)){NULL}else{actionLink(session$ns("download_gap_plot"),"Download",icon("download"))}),
          uiOutput(session$ns("gap_error")),
          if(is.null(vals$gap_plot_hc)){
            div(style="color: gray;", "Run Gap statistic to generate the plot.")
          } else{
            plotOutput(session$ns("plot_hc_gapplot"))
          })
    })
    output$plot_hc_gapplot<-renderPlot({
      req(vals$gap_plot_hc)
      print(vals$gap_plot_hc)
    })
    observe({
      req(model_or_data())
      if(model_or_data()=="som codebook"){
        m<-getmodel_hc()
        data<-m$codes[[1]]
      } else{
        data<-getdata_hc()
      }
      updateNumericInput(session,"gap_hc_k", value = round(nrow(data)/2))
    })
    observeEvent(input$run_gap_hc,{
      shinyjs::removeClass("run_gap_hc_btn","save_changes")
      vals$gap_message<-NULL
      req(input$gap_hc_k>=2)
      req(input$gap_hc_boot>=1)
      withProgress(min=0,max=1,message="Running...",{
        get_hc_gapplot()
      })

    })
    observeEvent(model_or_data(),{
      vals$gap_plot_hc<-NULL
    },ignoreInit = TRUE)
    observeEvent(list(input$gap_hc_k,input$gap_hc_boot,input$gap_hc_rule,model_or_data(),som_model_name(),disthc(),vals$som_whatmap),ignoreInit = T,{
      req(input$run_gap_hc)
      shinyjs::addCssClass('run_gap_hc_btn',"save_changes")
      vals$gap_plot_hc<-NULL
    })
    observe({
      has_data<-FALSE
      try({
        has_data<-!is.null(getdata_hc())
      },silent=TRUE)
      shinyjs::toggle("run_gap_hc_btn",condition = has_data)
    })
    observeEvent(input$download_gap_plot,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=vals$gap_plot_hc

      datalist_name=attr(getdata_hc(),'datalist')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Gap statistic", name_c="gap_statistic",datalist_name=datalist_name)
    })
    observeEvent(input$gaphelp,{
      showModal(
        modalDialog(
          textgapplot(),
          title="Gap statistic",
          easyClose=TRUE,
          footer=modalButton("Close"),
          size="l"
        )
      )
    })
  })
}

vat_module<-list()
vat_module$ui<-function(id){
  ns<-NS(id)
  tabPanel(
    "VAT",
    value="vat",
    column(
      4,class="mp0",
      box_caret(
        ns("box_vat_a"),
        title="Options",
        color="#c3cc74ff",
        div(
          div(style="display: flex",
              numericInput(ns("vat_threshold"), span("Jump quantile", tipright("Quantile used to mark large jumps in the VAT ordering. Larger values are more conservative.")), 0.90, min=0.5, max=0.99, step=0.01),
              div(id=ns('run_vat_hc_btn'),style="display: inline-block; vertical-align: top;", class="save_changes",actionButton(ns("run_vat_hc"),"Run VAT"))
          ),
          pickerInput_fromtop(ns("vat_palette"), "Palette", c("magma","viridis","inferno","plasma","cividis"), selected="magma"),
          checkboxInput(ns("vat_show_profile"), "Show jump profile", value=TRUE),
          div(style = "margin-top: 8px;",
              actionLink(ns("vathelp"), "About VAT", icon("question-circle")))
        )
      )
    ),
    column(
      8,class="mp0",style="position: absolute; right: 0px; padding-left: 6px",
      box_caret(ns("box_vat_b"),
                title="Results",
                div(uiOutput(ns("vat_tab2_out")))
      )
    )
  )
}
vat_module$server<-function(id, vals, getdata_hc, getmodel_hc, model_or_data, som_model_name, disthc, hc_vatplot_fun){
  moduleServer(id,function(input, output, session) {
    box_caret_server("box_vat_a")
    box_caret_server("box_vat_b")

    get_hc_vatplot<-reactive({
      args<-list(
        data=getdata_hc(),
        model_or_data=model_or_data(),
        model_name=som_model_name(),
        disthc=disthc(),
        threshold=input$vat_threshold,
        palette=input$vat_palette,
        show_profile=input$vat_show_profile,
        use_weights=FALSE,
        whatmap=vals$som_whatmap
      )
      re<-do.call(hc_vatplot_fun,args)
      vals$vat_message<-attr(re,"logs")
      req(!isFALSE(re))
      vals$vat_results<-attr(re,"result")
      vals$vat_plot_hc<-re
    })
    output$vat_error<-renderUI({
      render_message(vals$vat_message)
    })
    output$vat_tab2_out<-renderUI({
      div(style="margin-top: 20px;",
          div(style="display: flex; justify-content: space-between; margin-bottom: 5px;",
              strong("VAT cluster tendency"),
              if(is.null(vals$vat_plot_hc)){NULL}else{actionLink(session$ns("download_vat_plot"),"Download",icon("download"))}),
          uiOutput(session$ns("vat_error")),
          if(is.null(vals$vat_plot_hc)){
            div(style="color: gray;", "Run VAT to generate the reordered dissimilarity image.")
          } else{
            tagList(
              uiOutput(session$ns("vat_summary")),
              plotOutput(session$ns("plot_hc_vat"))
            )
          })
    })
    output$vat_summary<-renderUI({
      req(vals$vat_results)
      div(style="margin-bottom: 8px;",
          strong("Suggested k: "), vals$vat_results$suggested_k[1],
          span(style="color: gray;", paste0(" (", vals$vat_results$n_jumps[1], " large jump(s) above threshold)")))
    })
    output$plot_hc_vat<-renderPlot({
      req(vals$vat_plot_hc)
      print(vals$vat_plot_hc)
    })
    observeEvent(input$run_vat_hc,{
      shinyjs::removeClass("run_vat_hc_btn","save_changes")
      vals$vat_message<-NULL
      req(input$vat_threshold>0&&input$vat_threshold<1)
      withProgress(min=0,max=1,message="Running VAT...",{
        get_hc_vatplot()
      })
    })
    observeEvent(model_or_data(),{
      vals$vat_plot_hc<-NULL
      vals$vat_results<-NULL
    },ignoreInit=TRUE)
    observeEvent(list(input$vat_threshold,input$vat_palette,input$vat_show_profile,model_or_data(),som_model_name(),disthc(),vals$som_whatmap),ignoreInit=TRUE,{
      req(input$run_vat_hc)
      shinyjs::addCssClass('run_vat_hc_btn',"save_changes")
      vals$vat_plot_hc<-NULL
      vals$vat_results<-NULL
    })
    observe({
      has_data<-FALSE
      try({has_data<-!is.null(getdata_hc())},silent=TRUE)
      shinyjs::toggle("run_vat_hc_btn",condition=has_data)
    })
    observeEvent(input$download_vat_plot,ignoreInit=TRUE,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      generic=vals$vat_plot_hc
      datalist_name=attr(getdata_hc(),'datalist')
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="VAT cluster tendency", name_c="vat_cluster_tendency",datalist_name=datalist_name)
    })
    observeEvent(input$vathelp,{
      showModal(modalDialog(
        title="VAT cluster tendency",
        easyClose=TRUE,
        footer=modalButton("Close"),
        size="l",
        div(
          p("VAT reorders the dissimilarity matrix so that compact groups tend to appear as dark blocks along the diagonal."),
          p("The image is exploratory. The suggested k shown here uses large jumps in the VAT ordering as candidate boundaries between blocks."),
          tags$ul(
            tags$li(strong("Jump quantile:"), "threshold used to flag unusually large VAT ordering jumps."),
            tags$li(strong("Suggested k:"), "one plus the number of jumps above that threshold.")
          )
        )
      ))
    })
  })
}
scree_plot_module<-list()
scree_plot_module$ui<-function(id){
  ns<-NS(id)
  tabPanel(
    "2. Scree Plot",
    value="tab2",
    tabsetPanel(
      id=ns("suggestion_tabs"),
      elbow_smw_module$ui(ns("elbow_smw")),
      gap_module$ui(ns("gap")),
      vat_module$ui(ns("vat")),
      silhouette_module$ui(ns("silhouette")),
      stability_module$ui(ns("stability")),
      consensus_module$ui(ns("consensus"))

    )
  )
}
scree_plot_module$server<-function(id, vals, getdata_hc, getmodel_hc, model_or_data, som_model_name, disthc, hc_screeplot_fun, hc_gapplot_fun, hc_vatplot_fun, hc_silhouetteplot_fun,hc_stabilityplot_fun,hc_consensusplot_fun){
  moduleServer(id,function(input, output, session) {
    elbow_smw_module$server(
      "elbow_smw",
      vals=vals,
      getdata_hc=getdata_hc,
      getmodel_hc=getmodel_hc,
      model_or_data=model_or_data,
      som_model_name=som_model_name,
      disthc=disthc,
      hc_screeplot_fun=hc_screeplot_fun
    )
    gap_module$server(
      "gap",
      vals=vals,
      getdata_hc=getdata_hc,
      getmodel_hc=getmodel_hc,
      model_or_data=model_or_data,
      som_model_name=som_model_name,
      disthc=disthc,
      hc_gapplot_fun=hc_gapplot_fun
    )
    vat_module$server(
      "vat",
      vals=vals,
      getdata_hc=getdata_hc,
      getmodel_hc=getmodel_hc,
      model_or_data=model_or_data,
      som_model_name=som_model_name,
      disthc=disthc,
      hc_vatplot_fun=hc_vatplot_fun
    )
    silhouette_module$server(
      "silhouette",
      vals = vals,
      getdata_hc = getdata_hc,
      getmodel_hc = getmodel_hc,
      model_or_data = model_or_data,
      som_model_name = som_model_name,
      disthc = disthc,
      hc_silhouetteplot_fun = hc_silhouetteplot_fun
    )


    stability_module$server(
      "stability",
      vals = vals,
      getdata_hc = getdata_hc,
      getmodel_hc = getmodel_hc,
      model_or_data = model_or_data,
      som_model_name = som_model_name,
      disthc = disthc,
      hc_stabilityplot_fun = hc_stabilityplot_fun
    )


    consensus_module$server(
      "consensus",
      vals = vals,
      getdata_hc = getdata_hc,
      getmodel_hc = getmodel_hc,
      model_or_data = model_or_data,
      som_model_name = som_model_name,
      disthc = disthc,
      hc_consensusplot_fun = hc_consensusplot_fun
    )
  })
}

hc_dendrogram_module<-list()
hc_dendrogram_module$ui<-function(ns){
  tabPanel(
    "1. Dendrogram",
    value="tab1",
    column(
      4,class="mp0",
      box_caret(ns("box1_a"),
                title="Options",
                color="#c3cc74ff",
                div(
                  textInput(ns("hc_title"), "Title", value = NULL),
                  textInput(ns("hc_xlab"), "x label", value = "Observations"),
                  textInput(ns("hc_ylab"), "y label", value = "Height"),
                  numericInput(ns("hc_lwd"), "Line width", value = 0.5, min = 0.1, step = 0.1),
                  colourpicker::colourInput(ns("hc_line_col"), "Line color", "black"),
                  numericInput(ns("hc_cex"), "Label size", value = 0.8, min = 0.1, step = 0.1),
                  pickerInput_fromtop(ns("hc_theme"), "Theme", c("theme_minimal","theme_grey","theme_linedraw","theme_light","theme_bw","theme_classic")),
                  numericInput(ns("hc_label_angle"), "Label angle", value = 0, step = 15),
                  checkboxInput(ns("hc_show_labels"), "Show labels", value = TRUE),
                  uiOutput(ns("labhc_out"))
                )
      )
    ),
    column(
      8,class="mp0",style="position: absolute; right: 0px; padding-left: 6px",
      box_caret(ns("box1_b"),
                title="Plot",
                button_title=actionLink(ns("download_plot1"),"Download",icon("download")),
                div(
                  div(id=ns("hcut_btn1"),class="save_changes",
                      actionButton(ns("run_hc1"),"RUN >>")
                  ),
                  uiOutput(ns("hcdata_plot"))
                )
      )
    )
  )
}
hc_dendrogram_module$server<-function(input, output, session, vals, getdata_hc, labhc, args_hc1, hc_model){
  ns<-session$ns

  box_caret_server('box1_a')
  box_caret_server('box1_b')

  hc_dendrogram_plot<-reactive({
    somC<-hc_model()
    vals$hc_messages<-attr(somC,"logs")
    hc<-somC$hc.object
    req(hc)

    labels<-NULL
    if(input$model_or_data!="som codebook"&&isTRUE(input$hc_show_labels)){
      labels<-as.character(labhc())
    }
    if(!isTRUE(input$hc_show_labels)){
      labels<-FALSE
    }

    gg_hc_dendrogram(
      hc.object=hc,
      labels=labels,
      lwd=input$hc_lwd,
      line_color=input$hc_line_col,
      main=input$hc_title,
      xlab=input$hc_xlab,
      ylab=input$hc_ylab,
      base_size=input$hc_cex,
      theme=input$hc_theme,
      angle_label=input$hc_label_angle
    )
  })

  observeEvent(args_hc1(),{
    vals$hc_messages<-NULL
    vals$hc_tab1_plot<-NULL
    shinyjs::addClass("hcut_btn1","save_changes")
  })

  observeEvent(input$run_hc1,ignoreInit = TRUE,{
    shinyjs::removeClass("hcut_btn1","save_changes")
    req(input$model_or_data)
    req(input$method.hc0)
    output$hcdata_plot<-renderUI({
      renderPlot({
        p<-hc_dendrogram_plot()
        vals$hc_tab1_plot<-p
        p
      })
    })
  })

  observeEvent(ignoreInit = TRUE,input$download_plot1,{
    vals$hand_plot<-"generic_gg"
    module_ui_figs("downfigs")
    req(vals$hc_tab1_plot)
    generic=vals$hc_tab1_plot
    datalist_name=attr(getdata_hc(),'datalist')
    mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="Dendrogram", name_c="dendrogram",datalist_name=datalist_name)
  })
  observe({
    shinyjs::toggle('labhc',condition=input$model_or_data=='data'&&isTRUE(input$hc_show_labels))
  })

  output$labhc_out<-renderUI({
    choices = c(colnames(attr(getdata_hc(),"factors")))
    pickerInput_fromtop(
      ns("labhc"),
      "Labels",
      choices=choices,selected=vals$labhc)
  })
}
hc_nclusters_module<-list()
hc_nclusters_module$ui<-function(ns){
  column(12,class="mp0",id=ns("Kcustom"),
         column(
           4,class="mp0",
           box_caret(
             ns("box_nclust"),
             title="Number of Clusters",
             color="#c3cc74ff",
             div(numericInput(ns("customKdata"),"Number of clusters: ",value = 3,step = 1),
                 uiOutput(ns("saveHC")),

                 div(style='border-bottom: 1px solid gray; border-top: 1px solid gray; padding-bottom: 5px',
                     div(
                       checkboxInput(ns("hc_sort"),span("Sort clusters",tiphelp("Sort clusters by a  variable")),value=F),
                       div(style="margin-left: 15px;",
                           pickerInput_fromtop(ns("hc_ord_datalist"),"Datalist:",choices=NULL,width="200px", options=shinyWidgets::pickerOptions(liveSearch =T)),

                           pickerInput_fromtop(ns("hc_ord_factor"),"Variable:",choices = NULL,selected=NULL,options=shinyWidgets::pickerOptions(liveSearch=T))
                       )

                     )
                 )
             ),


           )
         )
  )
}

hc_nclusters_module$server<-function(input, output, session, vals, getdata_for_hc, obs.clusters, getdata_hc, get_hc, hc_model_names_saved){
  ns<-session$ns
  phc<-reactiveVal()

  box_caret_server('box_nclust')

  cluster_already<-reactive({
    req(input$data_hc)
    datao<-vals$saved_data[[input$data_hc]]
    factors<-attr(datao,"factors")
    if(is.null(factors)||ncol(factors)==0){
      return(character(0))
    }
    current_clusters<-obs.clusters()
    if(!all(rownames(factors)%in%names(current_clusters))){
      return(character(0))
    }
    hc<-as.character(current_clusters[rownames(factors)])
    fac<-as.list(factors)
    fac<-lapply(fac,function(x) as.character(x))
    cluster_already<-which(sapply(fac, function(x) identical(x, hc)))
    names(cluster_already)
  })

  output$saveHC<-renderUI({

    clu_al<-cluster_already()
    if (length(cluster_already()) > 0) {
      class1<-"button_normal"
      class2<-"div"
      class3<-'divnull'
    } else {
      class1<-"save_changes"
      class2<-"divnull"
      class3<-'div'
    }

    div(style="display: flex",
        div(style="display: flex",
            div(class = class1,style="padding-right: 5px",
                actionButton(ns("tools_savehc"), icon("fas fa-save"),  type = "action", value = FALSE)), span(style = "font-size: 12px", icon("fas fa-hand-point-left"), "Save Clusters in Datalist ", strong("X"), class = class3)
        )
        ,
        div(style = "margin-bottom: 5px", class = class2,style="text-direction: normal",em(paste0("The current clustering is saved in the Factor-Attribute as '", paste0(clu_al, collapse = "; "), "'"))))
  })

  observeEvent(input$hc_ord_datalist,{
    vals$cur_hc_ord_datalist<-input$hc_ord_datalist
  })
  observe({
    shinyjs::toggle('hc_ord_factor',condition=isTRUE(input$hc_sort))

    shinyjs::toggle('hc_ord_datalist',condition=isTRUE(input$hc_sort))
  })
  observeEvent(getdata_for_hc(),{
    selected=vals$cur_hc_ord_datalist
    choices = c(names(vals$saved_data[getdata_for_hc()]))
    selected=get_selected_from_choices(selected,choices)
    if(is.null(selected)&&length(choices)>0) selected<-choices[1]
    updatePickerInput(session,'hc_ord_datalist',choices=choices,selected=selected)
  })
  observeEvent(input$hc_ord_datalist,{
    req(input$hc_ord_datalist)
    data<-vals$saved_data[[input$hc_ord_datalist]]
    choices<-colnames(data)[vapply(data,is.numeric,logical(1))]
    selected=vals$cur_hc_ord_factor
    selected<-get_selected_from_choices(selected,choices)
    if(is.null(selected)&&length(choices)>0) selected<-choices[1]
    updatePickerInput(session,'hc_ord_factor',choices=choices,selected=selected)

  })
  observeEvent(input$hc_ord_factor,{
    vals$cur_hc_ord_factor<-input$hc_ord_factor
  })

  observeEvent(hc_newlevels(),{
    phc(NULL)
  })

  hc_newlevels<-reactive({
    req(input$data_hc)
    req(input$data_hc%in%names(vals$saved_data))
    req(input$hc_ord_datalist)
    req(input$hc_ord_factor)
    req(input$hc_ord_datalist%in%names(vals$saved_data))

    data_o<-getdata_hc()
    data_ref<-vals$saved_data[[input$hc_ord_datalist]]
    validate(need(all(rownames(data_o)%in%rownames(data_ref)),"The IDs of the sorted data chosen do not match those of the training data."))
    data<-data_ref[rownames(data_o),,drop=F]
    hc<-get_hc()

    data<-data[rownames(data_o),,drop=F]
    req(input$hc_ord_factor%in%names(data))
    validate(need(is.numeric(data[[input$hc_ord_factor]]),"Cluster sorting requires a numeric variable."))
    validate(need(all(names(hc$somC)%in%rownames(data)),"The cluster observations are not all present in the selected sorting datalist."))
    fac<-data[names(hc$somC),input$hc_ord_factor,drop=TRUE]
    clusters<-hc$somC
    ord_value<-as.numeric(fac)
    validate(need(any(!is.na(ord_value)),"The selected sorting variable could not be converted to an ordering score."))
    cluster_score<-tapply(ord_value,as.character(clusters),function(x) mean(x,na.rm=TRUE))
    newlevels<-names(sort(cluster_score,na.last=TRUE))

    newlevels
  })

  hierarc_cluster<-reactive({

    req(input$model_or_data)
    req(input$method.hc0)
    req(length(input$hc_sort)>0)
    hc<-get_hc()
    vals$cutsom<-hc$som.hc
    vals$cutsom_samples<-hc$somC
    if(isFALSE(input$hc_sort)){
      vals$hc_newlevels<-NULL
      hc$som.hc<-factor(hc$som.hc)
      hc$somC<-factor(hc$somC)
      hc
    } else{
      req(input$hc_ord_datalist)
      req(input$hc_ord_factor)

      som.hc_names<-names(hc$som.hc)

      somC_names<-names(hc$somC)

      newlevels<-hc_newlevels()

      vals$hc_newlevels<-newlevels

      hc<-get_hc()

      hc$som.hc<-factor(hc$som.hc,levels=newlevels,labels=1:length(newlevels))

      hc$somC<-factor(hc$somC,levels=newlevels,labels=1:length(newlevels))

      hc$som.hc<-hc$som.hc[som.hc_names]

      hc$somC<-hc$somC[somC_names]

      vals$cutsom<-hc$som.hc

      vals$cutsom_samples<-hc$somC

      hc
    }


    hc

  })

  update_unsaved_hc_model<-function(){
    phc(hierarc_cluster())
    vals$hc_order_user_dirty<-FALSE
    vals$cur_hc_models<-"new HC (unsaved)"
    freezeReactiveValue(input,"hc_models")
    updatePickerInput(session,"hc_models",
                      choices=c("new HC (unsaved)",hc_model_names_saved()),
                      selected="new HC (unsaved)")
  }

  observeEvent(input$run_bmu,ignoreInit = TRUE,{
    update_unsaved_hc_model()
  })

  observeEvent(input$run_hc,ignoreInit = TRUE,{
    update_unsaved_hc_model()
  })

  reset_unsaved_hc_model<-function(){
    if(!is.null(phc())){
      phc(NULL)
    }
    vals$cur_hc_models<-NULL
    shinyjs::addClass("hcut_btn","save_changes")
    shinyjs::addClass("run_bmu_btn","save_changes")
    freezeReactiveValue(input,"hc_models")
    choices<-hc_model_names_saved()
    selected<-if(length(choices)>0){choices[1]} else{character(0)}
    updatePickerInput(session,"hc_models",choices=choices,selected=selected)
  }

  observeEvent(input$customKdata,{
    vals$saved_kcustom<-input$customKdata
  })

  observeEvent(input$customKdata,ignoreInit = TRUE,{
    reset_unsaved_hc_model()
  })

  observeEvent(list(input$hc_fun,input$method.hc0,input$disthc,input$model_or_data,input$som_model_name,vals$som_whatmap),{
    reset_unsaved_hc_model()
  }, ignoreInit=TRUE)

  observeEvent(list(input$hc_sort,input$hc_ord_datalist,input$hc_ord_factor),ignoreInit = TRUE, priority = 100,{
    shinyjs::addClass("hcut_btn","save_changes")
    shinyjs::addClass("run_bmu_btn","save_changes")

    # A change in the ordering inputs invalidates the current unsaved HC cut,
    # but it must not restore the ordering controls from a previously saved model.
    # This has to run even when phc() is already NULL: on the first time the
    # user checks hc_sort, the saved-model sync can still fire and write
    # hc_sort back to FALSE. Keeping this flag active through the next flush
    # prevents that first-click rollback.
    vals$suspend_hc_order_sync<-TRUE
    vals$hc_order_user_dirty<-TRUE
    session$onFlushed(function(){
      session$onFlushed(function(){
        vals$suspend_hc_order_sync<-FALSE
      }, once=TRUE)
    }, once=TRUE)

    if(!is.null(phc())){
      phc(NULL)
    }
    vals$cur_hc_models<-NULL
    freezeReactiveValue(input,"hc_models")
    choices<-hc_model_names_saved()
    selected<-if(length(choices)>0){choices[1]} else{character(0)}
    updatePickerInput(session,"hc_models",choices=choices,selected=selected)
  })

  observeEvent(obs.clusters(),{
    req(!isTRUE(vals$suspend_hc_order_sync))
    req(!isTRUE(vals$hc_order_user_dirty))
    res<-attr(obs.clusters(),"order")
    req(res)
    vals$cur_hc_ord_datalist<-res$hc_ord_datalist
    vals$cur_hc_ord_factor<-res$hc_ord_factor
    updateCheckboxInput(session,'hc_sort',value=res$hc_sort)
    updatePickerInput(session,'hc_ord_datalist',selected=res$hc_ord_datalist)
  })

  list(cluster_already=cluster_already, phc=phc)
}
cut_dendrogram_module<-list()
cut_dendrogram_module$ui<-function(ns){
  tabPanel(
    '3. Cut Dendrogram',
    value="tab3",
    column(
      4,class="mp0",style="margin-left: -1px; padding-right: 3px",
      div(style="overflow-y: auto;height: calc(100vh - 200px); padding-left: 1px",
          box_caret(
            ns("box3_a"),
            title="Options",
            color="#c3cc74ff",
            div(
              div(
                pickerInput_fromtop_live(inputId = ns("hcdata_palette"),label = "HC Palette:",NULL),
                pickerInput_fromtop(ns("hcut_labels"),"Factor",NULL),
                div(
                  pickerInput_fromtop(ns("hcut_theme"),"Theme:",c('theme_minimal','theme_grey','theme_linedraw','theme_light','theme_bw','theme_classic')),
                  numericInput(ns("hcut_cex"),"Size",value = 12,step = 1),
                  numericInput(ns("hcut_lwd"),"Line width",value = .5,step = .5),
                  textInput(ns("hcut_main"),"Title","Cluster Dendrogram"),
                  textInput(ns("hcut_ylab"),"y label","Height"),
                  textInput(ns("hcut_xlab"),"x label","Observations"),
                  numericInput(ns("hcut_xlab_angle"),"rotate labels",value = 0,step =15),
                  numericInput(ns("hcut_offset"),"offset",value = -.1,step = 0.05),
                  checkboxInput(ns("hcut_log"),"Log Scale:",F)
                )
              )
            )

          )
      )),

    column(
      8,class="mp0",style="position: absolute; right: 12px; padding-left: 15px",
      box_caret(ns("box3_b"),
                title="Plot",
                button_title=actionLink(ns("download_plot3"),"Download",icon("download")),
                div(
                  div(id=ns("hcut_btn"),class="save_changes",
                      actionButton(ns("run_hc"),"RUN >>")
                  ),
                  uiOutput(ns("hcut_plot"))
                )
      )
    ))
}
cut_dendrogram_module$server<-function(input, output, session, vals, getdata_hc, get_hc_record, get_hcut_labels, cur_som.hc.object, cur_som.obs.clusters, cur_som.hc.clusters){
  ns<-session$ns

  box_caret_server('box3_a')
  box_caret_server('box3_b')

  observeEvent(input$hcdata_palette,{
    vals$hcdata_palette<-input$hcdata_palette
  }, ignoreInit=TRUE)

  observeEvent(vals$newcolhabs,{
    updatePickerInput(session,'hcdata_palette',
                      choices = vals$colors_img$val,
                      choicesOpt=list(content=vals$colors_img$img),
                      selected=vals$hcdata_palette
    )
  })

  hcut_inputs<-reactive({
    list(
      input$hcdata_palette,
      input$customKdata,
      input$hcut_labels,
      input$hcut_lwd,
      input$hcut_cex,
      input$hcut_main,
      input$hcut_xlab,
      input$hcut_ylab,
      input$hcut_theme,
      input$hcut_offset,
      input$hcut_xlab_angle,
      input$hcut_log,
      input$hc_fun,
      input$method.hc0,
      input$disthc,
      input$som_model_name,
      input$model_or_data
    )
  })

  observe({
    req(input$data_hc)
    req(input$data_hc%in%names(vals$saved_data))
    cur0<-attr(get_hc_record(),"obs.clusters")
    req( as.character(input$customKdata)%in%names(cur0))
    cur<-cur0[[ as.character(input$customKdata)]]
    shinyjs::toggleClass("hcut_btn","save_changes",condition = is.null(cur))

  })

  hcut_argsplot<-reactive({
    hc.object<-cur_som.hc.object()
    hc.clusters<-cur_som.hc.clusters()
    obs.clusters<-cur_som.obs.clusters()
    args<-list(
      obs.clusters=obs.clusters,
      hc.object=hc.object,
      hc.clusters=hc.clusters,
      palette=vals$newcolhabs[[input$hcdata_palette]],
      labels=get_hcut_labels(),
      lwd=input$hcut_lwd,
      base_size=input$hcut_cex,
      main=input$hcut_main,
      xlab=input$hcut_xlab,
      ylab=input$hcut_ylab,
      theme=input$hcut_theme,
      offset_labels=input$hcut_offset,
      angle=input$hcut_xlab_angle,
      log=input$hcut_log
    )
    args
  })
  output$hcut_plot<-renderUI({
    renderPlot({
      args<-hcut_argsplot()

      p<-do.call(gg_dendrogram,args)
      p


    })
  })
  observeEvent(hcut_inputs(),{
    vals$hc_messages<-NULL
  })
  observeEvent(ignoreInit = T,input$download_plot3,{
    vals$hand_plot<-"generic_gg"
    module_ui_figs("downfigs")
    generic=do.call(gg_dendrogram,hcut_argsplot())
    name_c=paste0("Dendrogram_",input$customKdata,"groups")
    datalist_name=attr(getdata_hc(),'datalist')
    mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message=name_c, name_c=name_c,datalist_name=datalist_name)
  })
}
codebook_clusters_module<-list()
codebook_clusters_module$ui<-function(ns,
                                      cluster_label="HC",
                                      enable_prediction=TRUE,
                                      enable_model_download=TRUE,
                                      enable_codebook_create=TRUE,
                                      enable_cluster_importance=TRUE){
  vfm_choices<-if(isTRUE(enable_cluster_importance)){
    list("Highest"='var', "Chull"="cor","Cluster"="cor_hc")
  } else {
    list("Highest"='var', "Chull"="cor")
  }
  var_pie_choices<-if(isTRUE(enable_cluster_importance)){
    list("Top importance by cluster"='top_hc',"Top importance"="rsquared" ,"Relative importance"="top","Top weight"="top_w","Manual"="manual")
  } else {
    list("Top importance"="rsquared" ,"Relative importance"="top","Top weight"="top_w","Manual"="manual")
  }
  tabPanel('4. Codebook clusters',
           value="tab4",
           column(
             4,class="mp0",style="margin-left: -1px; padding-right: 3px",
             div(style="overflow-y: auto;height: calc(100vh - 200px); padding-left: 1px",
                 if(isTRUE(enable_prediction)) box_caret(
                   ns("box_4mapping"),
                   color="#c3cc74ff",
                   tip=tiphelp("Add predictions from new data to the trained SOM", "bottom"),
                   title=span(style="display: inline-block",
                              class="checktitle",
                              checkboxInput(ns("hcsom_newdata") ,label =strong(span("Predict")),F,width="80px")
                   ),
                   div(
                     uiOutput(ns("hc_save_tab4")),
                     uiOutput(ns("hcsom_newdata_mess")),
                     uiOutput(ns("out_hcsom_whatmap")))
                 ),
                 box_caret(
                   ns("box4_a"),
                   title="Neurons",
                   color="#c3cc74ff",
                   div(

                     checkboxInput(ns("fill_neurons"),"Fill",T),
                     pickerInput_fromtop_live(ns("bg_palette"),label ="Palette",NULL),

                     div(id=ns("neu_options"),


                         numericInput(ns("pcodes_bgalpha"),"Lightness",value = 0,min = 0,max = 1,step = .1),

                         colourpicker::colourInput(ns("pclus_border"),"Border:","white"),
                     ),
                     numericInput(ns("border_width"),"Border width",value = 0.5,step=0.1),
                     textInput(ns("neuron_legend_text"),"Legend text","Group")

                   )),
                 box_caret(
                   ns("box4_points"),
                   color="#c3cc74ff",
                   title=span(style="display: inline-block",
                              class="checktitle",
                              checkboxInput(ns("pclus_addpoints"),"Points",value=T,width="80px")
                   ),
                   div(id=ns("pclus_points_inputs"),
                       pickerInput_fromtop_live(inputId = ns("pclus_points_palette"),label ="Palette",choices =NULL),
                       pickerInput_fromtop(
                         inputId = ns("pclus_symbol"),
                         label = span("Point shape",tiphelp("Shape used for observations in the codebook cluster plot.")),
                         choices = NULL
                       ),
                       div(
                         id=ns("options_points_factor"),
                         pickerInput_fromtop(ns("pclus_points_factor"),"Factor",
                                             choices = NULL),
                         tags$div(id=ns("color_factor"),
                                  class="form-group shiny-input-container",
                                  tags$label(class = "control-label", " + Factor"),
                                  tags$div(class="dummy-input",
                                           "Choose a gradient palette for adding a factor",style="color: gray"
                                  )
                         ),

                         numericInput(ns("pclus_points_size"),"Size",value = 1,min = 0.1,max = 3,step = .1),
                         checkboxInput(ns("pclus_show_legend"),"Show legend",T),
                         textInput(ns("pclus_points_legend_text"),"Legend text","Observations"),

                       ))
                 ),
                 box_caret(
                   ns("box4_text"),
                   color="#c3cc74ff",
                   title=span(style="display: inline-block",
                              class="checktitle",
                              checkboxInput(ns("pclus_addtext"),"Labels",value=F,width="80px")
                   ),
                   div(id=ns('pclus_addtext_out'),

                       colourpicker::colourInput(ns("pclus_text_palette"),"Palette:","black"),
                       pickerInput_fromtop(ns("pclus_text_factor"),"Labels:",choices = NULL),
                       numericInput(ns("pclus_text_size"),"Size:",value = 1,min = 0.1,max = 3,step = .1),
                       checkboxInput(ns("text_repel"),"Repel Labels:",F),
                       numericInput(ns("max.overlaps"),"max.overlaps:",value = 10,min = 1,step = 1)
                   )),
                 box_caret(
                   ns("box4_vfm"),
                   color="#c3cc74ff",
                   button_title=tipify_ui(actionLink(ns("varfacmap"), icon("fas fa-question-circle")),"Click for details","right"),
                   title=span(style="display: inline-block",
                              class="checktitle",

                              checkboxInput(ns("varfacmap_action"),span("Variable factor map"),value =F,width="210px"),

                   ),
                   div(id=ns('varfac_out'),
                       # pickerInput_fromtop(ns("vfm_layer"),"Layer:",choices =NULL),
                       pickerInput_fromtop(ns("vfm_type"),"Show correlation:",choices =vfm_choices),

                       numericInput(ns("npic"), span(tiphelp("Number of variables to display"),"Number"), value = 10, min = 2),
                       numericInput(ns("pclus.cex.var"), "Var size", value = 1, min = 2),


                       colourpicker::colourInput(ns("p.clus.col.text"),"Var text color:","black"),
                       colourpicker::colourInput(ns("var_bg"),"Var background:","white"),

                       numericInput(ns("var_bg_transp"), "Var transparency", value = 0, min = 2),

                       div(actionLink(ns("create_dl_vfm"),span("Create Datalist",tiphelp("Create Datalist with variables from VFM")),icon("creative-commons-share")))
                   )
                 ),
                 box_caret(
                   ns("box_var_pie"),
                   color="#c3cc74ff",
                   button_title=tipify_ui(actionLink(ns("var_pie_help"), icon("fas fa-question-circle")),"Click for details","right"),
                   title=span(style="display: inline-block",
                              class="checktitle",

                              checkboxInput(ns("var_pie"),strong("Variable pies"),value =F,width="210px"),

                   ),

                   div(id=ns('var_pie_out'),
                       pickerInput_fromtop(ns("var_pie_type"),"Show:",choices =var_pie_choices),
                       pickerInput_fromtop(ns("var_pie_layer"),"Layer",NULL),
                       div(class="virtual-130",
                           virtualPicker(ns("var_pie_manual"),"variables selected")
                       ),

                       numericInput(ns("var_pie_n"), span(tipright("Number of variables to display"),"Number"), value = 10, min = 2),
                       pickerInput_fromtop_live(ns("var_pie_bg"),label = "Palette",choices = NULL),
                       numericInput(ns("var_pie_transp"), "Transparency", value = 0, min = 2))
                 ),


                 box_caret(
                   ns("box4_more"),
                   title = "General options",
                   color="#c3cc74ff",
                   div(
                     numericInput(ns("base_size"),"Base size",value = 12),
                     textInput(ns("hcs_title"), "Title: ", ""),
                     checkboxInput(ns("hcs_theme"),label = "show neuron coordinates",value = F),
                     if(isTRUE(enable_codebook_create)) div(actionLink(ns('create_codebook'),paste0("Create Datalist with the Codebook and ",cluster_label," class"))),
                     if(isTRUE(enable_model_download)) div(tipify_ui(downloadLink(ns('down_hc_model'),paste0("Download ",cluster_label," model"), style="button_active"),"Download file as .rds"))
                   )
                 )

             )),
           column(
             8,class="mp0",style="position: absolute;right: 0px",
             box_caret(
               ns("box4_b"),
               title="Plot",
               button_title=actionLink(ns("download_plot4"),"Download",icon("download")),
               div(
                 id=ns("hc_tab4_out"),
                 div(
                   div(id=ns("run_bmu_btn"),
                       actionButton(ns("run_bmu"),"RUN >>")
                   ),
                   div(
                     style="position: absolute;top: 25px; right: 0px",
                     if(isTRUE(enable_cluster_importance)) uiOutput(ns("importance_results")),
                     if(isTRUE(enable_cluster_importance)) uiOutput(ns("create_importance_results"))


                   )
                 ),
                 plotOutput(ns("BMU_PLOT")),


               )

             )
           )


  )
}
codebook_clusters_module$server<-function(input, output, session, vals, getdata_hc, getmodel_hc, getmodel_hc0, cluster_assignments, getsom_layers,
                                          model_object=NULL,
                                          model_name=reactive(NULL),
                                          cluster_count=reactive(NULL),
                                          is_codebook=reactive(TRUE),
                                          cluster_label="HC",
                                          enable_prediction=TRUE,
                                          enable_model_download=TRUE,
                                          enable_codebook_create=TRUE,
                                          enable_cluster_importance=TRUE){
  ns<-session$ns
  if(is.null(model_object)){
    model_object<-reactive(NULL)
  }

  if(isTRUE(enable_prediction)) box_caret_server('box_4mapping')
  box_caret_server('box4_a')
  box_caret_server('box4_points')
  box_caret_server('box4_text')
  box_caret_server('box4_vfm')
  box_caret_server('box_var_pie')
  box_caret_server('box4_more')
  box_caret_server('box4_b')

  observeEvent(df_symbol$val,{
    choices<-df_symbol$val
    selected<-get_selected_from_choices(vals$pclus_symbol,choices)
    if(is.null(selected)&&length(choices)>0){
      selected<-choices[1]
    }
    updatePickerInput(session,'pclus_symbol',choices = choices,
                      choicesOpt = list(content = df_symbol$img),
                      selected = selected)
  })

  observeEvent(input$pclus_symbol,{
    vals$pclus_symbol<-input$pclus_symbol
  },ignoreInit = TRUE)

  persist_plot_input<-function(input_id, vals_id=input_id){
    force(input_id)
    force(vals_id)
    observeEvent(input[[input_id]],{
      vals[[vals_id]]<-input[[input_id]]
    },ignoreInit = TRUE)
  }
  mapply(
    persist_plot_input,
    c(
      "bg_palette",
      "pclus_text_palette",
      "pclus_text_factor",
      "pclus_border",
      "vfm_type",
      "npic",
      "pclus.cex.var",
      "p.clus.col.text",
      "var_bg",
      "var_bg_transp",
      "pclus_points_palette",
      "insertx_pclus",
      "inserty_pclus",
      "ncol_pclus",
      "bgleg_pclus",
      "dot_label_clus",
      "varfacmap_action",
      "pclus_points_size",
      "pcodes_bgalpha",
      "pclus_newdata_addpoints",
      "pclus_newdata_points_palette",
      "pclus_newdata_points_factor",
      "pclus_newdata_symbol",
      "pclus_newdata_points_size",
      "pclus_newdata_addtext",
      "pclus_newdata_text_palette",
      "pclus_newdata_text_factor",
      "pclus_newdata_text_size"
    ),
    c(
      "pclussomplot_bg",
      "pclus_text_palette",
      "pclus_text_factor",
      "pclus_border",
      "vfm_type",
      "npic",
      "pclus.cex.var",
      "p.clus.col.text",
      "var_bg",
      "var_bg_transp.alpha",
      "pclus_points_palette",
      "insertx_pclus",
      "inserty_pclus",
      "ncol_pclus",
      "bgleg_pclus",
      "dot_label_clus",
      "pclus_varfacmap_action",
      "pclus_points_size",
      "pcodes_bgalpha",
      "pclus_newdata_addpoints",
      "pclus_newdata_points_palette",
      "pclus_newdata_points_factor",
      "pclus_newdata_symbol",
      "pclus_newdata_points_size",
      "pclus_newdata_addtext",
      "pclus_newdata_text_palette",
      "pclus_newdata_text_factor",
      "pclus_newdata_text_size"
    ),
    SIMPLIFY = FALSE
  )

  getgrad_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(2))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    grad<-names(res1[res1==F])
    pic<-which(vals$colors_img$val%in%grad)
    pic
  })
  getsolid_col<-reactive({
    res<-lapply(vals$newcolhabs, function(x) x(10))
    res1<-unlist(lapply(res, function(x) x[1]==x[2]))
    solid<-names(res1[res1==T])
    pic<-which(vals$colors_img$val%in%solid)
    pic
  })
  indicate_hc<-reactive({
    npic<-NULL
    indicate<-NULL
    if(isTRUE(input$varfacmap_action)){

      npic<- input$npic
      indicate<- input$vfm_type
    }
    iind=list(indicate=indicate,npic=npic)
    iind
  })
  bp_som<-reactive({

    iind=indicate_hc()
    m<-getmodel_hc()


    bp<-getbp_som2(m=m,indicate=iind$indicate,npic=iind$npic,hc=cluster_assignments())
    bp
  })

  get_network<-reactive({
    backtype=NULL
    property=NULL
    m<-getmodel_hc()
    hc<-cluster_assignments()
    hexs<-get_neurons(m,background_type="hc",property=NULL,hc=hc)
    hexs
  })
  get_copoints<-reactive({
    m<-getmodel_hc()
    copoints<-getcopoints(m)
    copoints
  })
  points_to_map<-reactive({
    rescale_copoints(hexs=get_network(),copoints=get_copoints())
  })
  hcsom_active_layers<-reactive({
    layers<-getsom_layers()
    active_layers<-sapply(layers,function(x) {
      if(isTRUE(input[[paste0("hcsom_layer",x)]])){
        input[[paste0("hcsom_newdata_layer",x)]]
      } else{NULL}

    })
    unlist(active_layers)
  })
  hcsom_whatmap<-reactive({
    layers<-getsom_layers()
    sapply(layers,function(x) {isTRUE(input[[paste0("hcsom_layer",x)]])})
  })
  predsupersom_hc<-reactive({
    layers<-getsom_layers()
    whatmap=layers[hcsom_whatmap()]

    m<-getmodel_hc0()
    if(length(m$data)==1){
      whatmap=NULL
    }

    newdatas<-vals$saved_data[hcsom_active_layers()]
    newdata_matrices<-lapply(newdatas,function(x) as.matrix(x))
    pic0<-names(which.min(sapply(newdata_matrices, nrow)))
    id_o<-rownames(newdata_matrices[[pic0]])
    newdata_matrices<-lapply(newdata_matrices,function(x){
      x[id_o,,drop=F]
    })
    if(length(m$data)>1){
      names(newdata_matrices)<-whatmap
    } else{
      newdata_matrices<-as.matrix(newdata_matrices[[1]])
    }
    pred<-predict(m,newdata_matrices,unit.predictions=m$codes,whatmap=whatmap)
    m2<-m
    m2$data<-pred$predictions
    m2$codes<-pred$unit.predictions
    bmus<-pred$unit.classif
    names(bmus)<-rownames(pred$predictions[[1]])
    m2$unit.classif<-bmus
    m2$whatmap<-pred$whatmap
    m<-m2
    m
  })
  points_tomap2<-reactive({
    m2<-predsupersom_hc()
    points_tomap2=rescale_copoints(hexs=get_network(),copoints=getcopoints(m2))
    points_tomap2
  })
  copoints_scaled<-reactive({
    points_tomap=points_to_map()
    data<-getdata_hc()
    factors<-attr(data,"factors")

    if(isTRUE(input$hcsom_newdata)){
      points_tomap2<-points_tomap2()
      points_tomap2$point<-"New data"
      points_tomap2$label<-rownames(points_tomap2)
      points_tomap$point<-"Training"
      dftemp<-rbind(points_tomap,points_tomap2)
      points_tomap<-dftemp
      attr(points_tomap,"namepoints")<-""
      return(points_tomap)
    }


    if(length(input$pclus_text_factor)>0){
      if(input$pclus_text_factor%in%colnames(factors)){
        text_factor= factors[rownames(data),input$pclus_text_factor, drop=F]
        points_tomap$label<-text_factor[rownames(points_tomap),]
      }
    }

    if(length(input$pclus_points_factor)>0){
      if(input$pclus_points_factor%in%colnames(factors)){
        points_factor= factors[rownames(data),input$pclus_points_factor, drop=F]
        points_tomap$point<-points_factor[rownames(points_tomap),]
        attr(points_tomap,"namepoints")<-input$pclus_points_factor
      }
    }

    points_tomap
  })

  argsplot_somplot<-reactive({


    req(input$pclus_points_palette)
    req(input$pcodes_bgalpha)


    indicate=indicate_hc()
    m<-getmodel_hc()
    som.hc<-cluster_assignments()


    tryco<-try(copoints_scaled(), silent = T)
    req(class(tryco)!='try-error')

    trybp<-try( bp_som(), silent = T)

    req(class(trybp)!='try-error')

    errors<-NULL
    args<-list(m=m,
               hexs=get_network(),
               points_tomap=copoints_scaled(),
               bp=bp_som(),
               points=input$pclus_addpoints,
               points_size=input$pclus_points_size,
               points_palette=input$pclus_points_palette,
               pch=if(is.null(input$pclus_symbol)||length(input$pclus_symbol)==0||is.na(as.numeric(input$pclus_symbol))){1}else{as.numeric(input$pclus_symbol)},
               text=input$pclus_addtext,
               text_size=input$pclus_text_size,
               text_palette=input$pclus_text_palette,
               bg_palette=input$bg_palette,
               newcolhabs=vals$newcolhabs,
               bgalpha=input$pcodes_bgalpha,
               border=input$pclus_border,
               indicate=indicate$indicate,
               cex.var=as.numeric(input$pclus.cex.var),
               col.text=input$p.clus.col.text,
               col.bg.var=input$var_bg,
               col.bg.var.alpha=1-input$var_bg_transp,
               show_error=errors,
               base_size=input$base_size,
               show_neucoords=input$hcs_theme,
               title=input$hcs_title,
               hc=som.hc,
               var_pie=input$var_pie,
               var_pie_type=input$var_pie_type,
               n_var_pie=input$var_pie_n,
               Y_palette=input$var_pie_bg,
               var_pie_transp=input$var_pie_transp,
               var_pie_layer=input$var_pie_layer,
               pie_variables=input$var_pie_manual,
               border_width=input$border_width,
               fill_neurons=input$fill_neurons,
               text_repel=input$text_repel,
               max.overlaps=input$max.overlaps,
               show_legend=input$pclus_show_legend,
               neuron_legend=input$neuron_legend_text,
               points_legend=input$pclus_points_legend_text
    )

    args

  })
  output$BMU_PLOT<-renderPlot({


    args<-argsplot_somplot()



    args$hc<-cluster_assignments()

    p<-do.call(bmu_plot_hc,args)
    hcplot4(p)
    p


  })
  observeEvent(argsplot_somplot(),{
    vals$hc_messages<-NULL
    shinyjs::addClass("run_bmu_btn","save_changes")
  })
  hcplot4<-reactiveVal()
  observeEvent(hcplot4(),{
    shinyjs::removeClass("run_bmu_btn","save_changes")
  })
  ##

  observeEvent(input$show_hcsom_fine,ignoreInit = T,{
    shinyjs::toggle("hcsom_fine")
  })
  output$importance_results<-renderUI({
    req(isTRUE(enable_cluster_importance))
    actionLink(ns("show_hc_imp"),"Importance results",icon("expand"))
  })
  output$create_importance_results<-renderUI({
    req(isTRUE(enable_cluster_importance))
    p<-hcplot4()
    imp_vars<-attr(p,"imp_vars")
    req(imp_vars)
    div(actionLink(ns("create_hc_imp"),span("Create Datalist",tiphelp("Create Datalist with selected variables for pies")),icon("creative-commons-share")))
  })
  observeEvent(input$create_hc_imp,{
    req(isTRUE(enable_cluster_importance))

    p<-hcplot4()

    imp_layer<-attr(p,"imp_layer")
    imp_vars<-attr(p,"imp_vars")

    req(imp_layer)
    req(imp_vars)
    req(imp_layer%in%names(vals$saved_data))
    imp_vars<-attr(p,"imp_vars")
    data_o<-vals$saved_data[[imp_layer]]
    req(data_o)
    req(imp_vars%in%colnames(data_o))
    data<-data_o[,imp_vars,drop=F]
    req(data)
    data<-data_migrate(data_o,data)

    bag<-paste0(imp_layer,"_som_top_vars")
    newnames<-make.unique(c(names(vals$saved_data),bag))
    bag<-newnames[length(newnames)]
    attr(data,"bag")<-bag
    vals$newdatalist<-data
    module_save_changes$ui(ns("som-imp-create"), vals)

  })
  module_save_changes$server("som-imp-create", vals)
  observeEvent(input$show_hc_imp,{
    req(isTRUE(enable_cluster_importance))
    data<-attr(hcplot4(),"imp_results")
    req(data)
    showModal(
      modalDialog(
        title="SOM Variable Importance Results",
        easyClose = T,
        div(class="half-drop-inline",
            div(actionLink(ns("download_hc_imp"),"Download",icon("download"))),

            fixed_dt(data,dom = 'lt',
                     pageLength=20,
                     lengthMenu = list(c(20, -1), c( "20","All")))
        )


      )
    )
  })
  observeEvent(input$download_hc_imp,{
    req(isTRUE(enable_cluster_importance))
    data<-data.frame(attr(hcplot4(),"imp_results"))
    req(data)
    vals$hand_down<-"generic"
    module_ui_downcenter("downcenter")
    name<-"som_imp_results"
    mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, message="Download Permutation Importance Results",data=data, name=name)

  })
  observeEvent(input$var_pie_layer,{
    m<-getmodel_hc()


    req(input$var_pie_layer%in%names(m$codes))
    order<-order(colMeans(abs( m$codes[[input$var_pie_layer]])),decreasing=T)
    choices<-colnames(m$codes[[input$var_pie_layer]])[order]


    shinyWidgets::updateVirtualSelect(
      "var_pie_manual",
      choices=choices,
      selected=choices[1:4]
    )
  })
  observe({
    shinyjs::toggle('var_pie_manual',condition=input$var_pie_type%in%"manual")
    shinyjs::toggle('var_pie_n',condition=!input$var_pie_type%in%"manual")
  })
  observeEvent(input$var_pie_help,{
    showModal(
      modalDialog(
        title = "Variable Pies in SOM codebook",
        easyClose = TRUE,
        div(
          p("The variables to display in the pie plots can be ranked using four methods:"),
          p("The pie plots represent variables from the trained codebook, where each value is calculated as the square root of the squared codebook weights. This allows for a straightforward comparison of variable contributions within each SOM unit."),
          tags$ul(
            tags$li(strong("Top importance by cluster"),
                    p(paste0("The groups based on the ",cluster_label," results assigned to neurons are used to split the codebook. For each group, the sum of the codebook weights for each variable is calculated, providing a measure of the absolute importance of each variable within each group. These importance scores are normalized by the total importance of each variable across all groups, resulting in a relative importance score for each variable within each group. The variables are ranked using relative importance scores for each group."))
            ),
            tags$li(strong("Top importance"),
                    p("Variables are ranked based on their ability to explain the clustering of the SOM. Importance is calculated by evaluating the coefficient of determination (R²) for each variable, representing the proportion of variance explained relative to the data associated with each classification unit in the SOM. Variables with the highest R² values are selected, highlighting those that contribute most to the SOM’s data structure.")
            ),
            tags$li(strong("Relative importance"),
                    p("The importance of each variable is determined based on the codebook weights for each neuron. The relative importance scores for each variable are calculated by normalizing the codebook weights by the sum of weights for each neuron. The top variables with the highest sum of relative importance scores across all neurons are identified and plotted using pies representing their weights in the codebook.")
            ),
            tags$li(strong("Top weight"),
                    p("The absolute importance of each variable is determined based on the sum of codebook weights for each variable across all neurons. The top variables with the highest absolute weights are identified and plotted using pies representing their weights in the codebook.")
            ),
            tags$li(strong("Manual"),
                    p("Select manually the variables to display.")
            )
          )
        )
      )
    )
  })
  observeEvent(getmodel_hc(),{
    updatePickerInput(session,'var_pie_layer',choices=names(getmodel_hc()$data))

  })
  observe({
    shinyjs::toggle('var_pie_layer',condition=length(getmodel_hc()$data)>1)
  })
  observeEvent(input$var_pie,{
    if(isTRUE(input$var_pie)){
      lapply(c('fill_neurons','pclus_addpoints','varfacmap_action'),function(x){
        updateCheckboxInput(session,x,value=F)
      })


    }
  })
  observe({
    shinyjs::toggle('neu_options',condition=isTRUE(input$fill_neurons))
  })
  observeEvent(input$fill_neurons,{
    if(isFALSE(input$fill_neurons)){
      updatePickerInput(session,"pclus_points_palette",
                        selected="black",
                        choices =  vals$colors_img$val[getsolid_col()],
                        choicesOpt = list(content =  vals$colors_img$img[getsolid_col()] ))
    } else{
      updatePickerInput(session,"pclus_points_palette",
                        choices =  vals$colors_img$val,
                        choicesOpt = list(content =  vals$colors_img$img ),selected=vals$pclus_points_palette)
    }
  })
  observe({
    req(is.null(vals$pclus_points_palette))

    vals$pclus_points_palette<-"black"

  })
  observe({
    shinyjs::toggle("options_points_factor",condition = isTRUE(input$fill_neurons))

  })
  observe({
    shinyjs::toggle("var_pie_out",condition=isTRUE(input$var_pie))
  })
  observe({
    updatePickerInput(session,'var_pie_bg',choices = vals$colors_img$val[getgrad_col()],selected="viridis",choicesOpt = list(content =  vals$colors_img$img[getgrad_col()]))

  })
  output$out_hcsom_whatmap<-renderUI({
    req(isTRUE(enable_prediction))
    req(input$hcsom_newdata)
    req(isTRUE(input$hcsom_newdata))
    req(isTRUE(is_codebook()))
    layers<-getsom_layers()
    div(style = "margin-left: 20px;",
        tags$style(HTML(
          ""
        )),
        strong("New Data:"),
        lapply(layers, function(x) {
          div(class = "map_control_style2", style = "color: #05668D",
              checkboxInput(ns(paste0("hcsom_layer", x)), div(
                style="display: flex; align-items:center;margin-top: -8px;height: 30px",
                x,uiOutput(ns(paste0("hcsom_piclayer", x)))
              ), TRUE),

          )
        })
    )
  })
  observeEvent(input$hcsom_newdata,{
    req(isTRUE(enable_prediction))
    if(isTRUE(input$hcsom_newdata)){
      cols<-vals$newcolhabs[[input$pclus_points_palette]](100)[1:2]
      if(cols[1]==cols[2])
        updatePickerInput(session,"pclus_points_palette",selected="turbo")
    }

  })
  output$hc_save_tab4<-renderUI({
    req(isTRUE(enable_prediction))
    req(hcplot4())
    req(isTRUE(input$hcsom_newdata))
    div(class = "save_changes",
        tipify_ui(shinyBS::bsButton(ns("savemapcode"), icon(verify_fa = FALSE, name = NULL, class = "fas fa-save"), style = "button_active", type = "action", value = FALSE),"Create Datalist with prediction results","right"), span(style = "font-size: 12px", icon(verify_fa = FALSE, name = NULL, class = "fas fa-hand-point-left"), "Create Datalist")
    )
  })
  output$down_hc_model<-downloadHandler(
    filename = function() {
      req(isTRUE(enable_model_download))
      paste0(cluster_label,"_", Sys.Date(),".rds")
    }, content = function(file) {
      req(isTRUE(enable_model_download))
      saveRDS(model_object(),file)
    })
  output$hcsom_newdata_mess<-  renderUI({
    req(isTRUE(enable_prediction))
    req(isTRUE(input$hcsom_newdata))
    span("select a gradient palette in 'Points' to differentiate between training and the new data", style="color: gray")

  })

  observeEvent(input$create_dl_vfm,{
    m<-getmodel_hc()
    vars<-rownames(bp_som())
    data_o<-vals$saved_data[[names(m$data)[1]]]
    data_n<-data.frame(do.call(rbind,m$data))

    data_n<-data_n[,vars,drop=F]
    data_n<-data_migrate(data_o,data_n)
    npic=input$npic

    type<-switch(input$vfm_type,
                 "var"='Highest',
                 "cor"="Chull",
                 "cor_hc"="Cluster")
    model_name_value<-model_name()
    if(is.null(model_name_value)||length(model_name_value)==0||is.na(model_name_value)||!nzchar(as.character(model_name_value))){
      model_name_value<-cluster_label
    }
    bag<-paste0(model_name_value,"_vfm",type,npic,"vars")

    attr(data_n,"bag")<-bag
    vals$newdatalist<-data_n
    module_save_changes$ui(ns("som-vfm-create"), vals)
  })
  module_save_changes$server("som-vfm-create", vals)
  observeEvent(ignoreInit = T,input$download_plot4,{
    vals$hand_plot<-"generic_gg"
    module_ui_figs("downfigs")
    generic=hcplot4()
    datalist_name=attr(getdata_hc(),'datalist')
    cluster_count_value<-cluster_count()
    if(is.null(cluster_count_value)||length(cluster_count_value)==0||is.na(cluster_count_value)){
      cluster_count_value<-""
    }
    name_c=paste0("Codebook",cluster_count_value,"groups")
    mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message=paste0("som codebook - ",cluster_label), name_c=name_c,datalist_name=datalist_name)
  })
  observeEvent(vals$newcolhabs,{
    updatePickerInput(session,'pclus_points_palette',
                      choices = vals$colors_img$val,
                      choicesOpt=list(content=vals$colors_img$img),
                      selected='black'
    )



  })
  observeEvent(vals$newcolhabs,{
    choices =  vals$colors_img$val[getgrad_col()]
    choicesOpt = list(content =  vals$colors_img$img[getgrad_col()] )
    updatePickerInput(session,'bg_palette',
                      choices=choices,
                      choicesOpt=choicesOpt
    )

  })
  observeEvent(input$pclus_points_palette,{
    cols<-vals$newcolhabs[[input$pclus_points_palette]](8)
    shinyjs::toggle('pclus_points_factor',condition=cols[1]!=cols[2])
    shinyjs::toggle('color_factor',condition=cols[1]==cols[2])
    if(cols[1]==cols[2]){
      updateTextInput(session,'pclus_points_legend_text',value="Observations")
    } else{
      updateTextInput(session,'pclus_points_legend_text',value=input$pclus_points_factor)
    }


  })

  observe({
    shinyjs::toggle("pclus_points_inputs",condition=isTRUE(input$pclus_addpoints))
  })
  observe({
    shinyjs::toggle("pclus_addtext_out", condition = isTRUE(input$pclus_addtext))
  })
  observe({
    shinyjs::toggle("varfac_out",condition = isTRUE(input$varfacmap_action))
  })
  observeEvent(input$varfacmap, {
    showModal(modalDialog(
      uiOutput(ns("textvarfacmap")),
      title = h4(strong("Variable factor map")),
      footer = modalButton("close"),
      size = "m",
      easyClose = TRUE
    ))
  })
  output$textvarfacmap<-renderUI({

    div(

      tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

      div(
        column(12,
               h4("Variable factor map"),
               p("The chart is very similar to the variable factor map obtained from the principal component analysis (PCA). It calculates the weighted correlation for each variable using the coordinates (x, y) of the neurons and their weights (number of instances). The codebooks vectors of the cells correspond to an estimation of the conditional averages, calculating their variance for each variable is equivalent to estimating the between-node variance of the variable, and hence their relevance."),
               p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("Chull correlations")," returns",code("npic")," variables with the highest correlation considering the convex hull, while also ensuring that the points are ordered by their proximity to codebook center")
        )

      )
    )

  })
  observe({
    shinyjs::toggle("hc_tab4_out",condition=isTRUE(is_codebook()))
  })
  observe({
    layers<-getsom_layers()
    m<-getmodel_hc0()
    lapply(layers,function(x){
      output[[paste0("hcsom_piclayer",x)]]<-renderUI({
        if( isTRUE(input[[paste0("hcsom_layer",x)]])){
          choices_temp<-names(which(sapply(vals$saved_data,function(xx){
            identical(sort(colnames(xx)),
                      sort(colnames(m$data[[x]])))
          })))
          div(class="label_none",style="max-width: 200px",
              pickerInput_fromtop(ns(paste0("hcsom_newdata_layer",x)), "", choices_temp))
        }
      })
    })
  })
  observeEvent(list(getdata_hc(),
                    input$hcsom_newdata),{

                      data<-getdata_hc()
                      choices<-colnames(attr(data,"factors"))
                      updatePickerInput(session,'pclus_text_factor',choices=choices,options=shinyWidgets::pickerOptions(liveSearch=T))
                      options<-NULL
                      if(isTRUE(input$hcsom_newdata)){
                        choices<-c("Training/New data","Training")
                        options=shinyWidgets::pickerOptions(liveSearch=T)
                      }

                      updatePickerInput(session,'pclus_points_factor',choices=choices,options=options)
                    })

  get_datalist_newmaps<-reactive({

    layers<-getsom_layers()
    news<-lapply(layers,function(x){
      if( isTRUE(input[[paste0("hcsom_layer",x)]])){ input[[paste0("hcsom_newdata_layer",x)]]} else{
        NULL
      }

    })
    unlist(news)


  })
  savemapcode<-reactive({
    news<-get_datalist_newmaps()
    numeric<-do.call(cbind,vals$saved_data[news])
    colnames(numeric)<-make.unique(unlist(sapply(vals$saved_data[news],colnames)))

    coords<-lapply(vals$saved_data[news],function(x) attr(x,"coords"))
    ids_coords<-unlist(lapply(coords,rownames))
    newcoords<-do.call(rbind,coords)
    newcoords$id<-ids_coords
    newcoords<-newcoords[!duplicated(newcoords$id),1:2]
    rownames(newcoords)<-unique(ids_coords)

    args<-argsplot_somplot()
    req(args)
    newdata<-args$points_tomap[  args$points_tomap$point=="New data",]
    new1<-newdata["hc"]
    rownames(new1)<-newdata$label
    new1<-new1[rownames(numeric),,drop=F]
    cluster_count_value<-cluster_count()
    if(is.null(cluster_count_value)||length(cluster_count_value)==0||is.na(cluster_count_value)){
      cluster_count_value<-""
    }
    colnames(new1)<-paste0(cluster_label,cluster_count_value)

    numeric<-data_migrate(getdata_hc(),numeric)

    attr(numeric,"coords")<-newcoords[rownames(numeric),]
    attr(numeric,"factors")<-new1[rownames(numeric),,drop=F]
    if(input$hand_save=="create"){
      vals$saved_data[[input$mc_newname]]<-numeric
    } else{
      vals$saved_data[[input$mc_over]]<-numeric
    }

  })

  list(
    argsplot_somplot=argsplot_somplot,
    savemapcode=savemapcode
  )
}

model_cluster_save_module<-list()
model_cluster_save_module$ui<-function(ns){
  NULL
}
model_cluster_save_module$server<-function(input, output, session, vals, getdata_hc, getmodel_hc, cur_som.hc.clusters, cur_som.obs.clusters, phc, cluster_already, hc_model_names_saved, hc_model_bagname, make_hc_record, set_hc_record, get_hc_models, set_hc_models, next_hc_model_name, bag_mp, savemapcode){
  ns<-session$ns
  ## saves
  output$data_create<-renderUI({
    req(length(vals$hand_save)>0)
    req(input$hand_save=="create")
    res<-switch (vals$hand_save,
                 "create_codebook"=textInput(ns("codebook_newname"), NULL,paste0(input$data_hc,"Codebook")),
                 "Save Clusters"= textInput(ns("hc_newname"), NULL,bag_hc()),
                 "Save HC model"= textInput(ns("hc_model_newname"), NULL,hc_model_bagname()),
                 "Create Datalist with new mapping"= textInput(ns("mc_newname"), NULL,bag_mp()),

    )
    res
  })
  output$databank_storage<-renderUI({
    div(

      div(p(strong("action:"),em("*",vals$hand_save,style="color: SeaGreen")), p(vals$hand_save2,style="color: gray")),
      div(vals$hand_save3),
      div(style='margin-top: 10px; margin-left: 10px',
          div(style='display: flex',
              div(
                radioButtons(ns("hand_save"),NULL,
                             choiceNames= list(div(style="display: flex;gap: 10px; height: 50px","create",uiOutput(ns("data_create"))),
                                               div(style="display: flex; gap: 10px;height: 50px","overwrite",uiOutput(ns("data_over")))),
                             choiceValues=list('create',"over"))
              )

          )

      ),
      div(vals$hand_save4)


    )
  })

  bag_hc<-reactive({
    datalist<-sommodel<-K<-""

    name0<-paste0('HC',input$customKdata)
    if(length(input$fixname)>0){
      if(isTRUE(input$fixname)){
        datalist<-paste0(input$data_hc,"_")
      }
    }
    if(length(input$fixmodel)>0){
      if(isTRUE(input$fixmodel)){
        sommodel<-paste0(input$som_model_name,"_")
      }
    }
    name0<-paste0(datalist,sommodel,name0)
    data<-attr(vals$saved_data[[input$data_hc]],"factors")
    name1<-make.unique(c(colnames(data),name0), sep="_")
    name1[ncol(data)+1]


  })
  hc_factor_bagname<-reactive({
    name0<-paste0("HC",input$customKdata)
    factors<-attr(vals$saved_data[[input$data_hc]],"factors")
    next_hc_model_name(name0,colnames(factors))
  })

  hand_save_modal<-reactive({

    tags$div(id="savemodal",
             modalDialog(
               shinycssloaders::withSpinner(type=8,color="SeaGreen",uiOutput(ns("databank_storage"))),
               title=span(icon(verify_fa = FALSE,name=NULL,class="fas fa-save"),'Save'),
               footer=column(12,class="needed",
                             fluidRow(shinyBS::bsButton(ns("cancel_save"),"Cancel"),
                                      inline(actionButton(ns("data_confirm"),strong("Confirm")))
                             )),
               #size="m",
               easyClose = T
             )
    )
  })




  output$data_over<-renderUI({
    req(input$hand_save=="over")
    res<-switch (vals$hand_save,
                 'Create Datalist with new mapping' = pickerInput(ns("mc_over"), NULL,choices=c(names(vals$saved_data)), options),
                 'create_codebook' = pickerInput(ns("codebook_over"), NULL,choices=c(names(vals$saved_data)),selected=input$data_upload),
                 'Save Clusters' = pickerInput(ns("hc_over"), NULL,choices=c(colnames(attr(getdata_hc(),"factors")))),
                 'Save HC model' = pickerInput(ns("hc_model_over"), NULL,choices=hc_model_names_saved()))
    res
  })
  save_hc_clusters_to_factor<-function(clusters,column_name){
    req(column_name)
    column_name<-trimws(column_name)
    req(nzchar(column_name))
    data_o<-vals$saved_data[[input$data_hc]]
    factors<-attr(data_o,"factors")
    if(is.null(factors)){
      factors<-data.frame(row.names=rownames(data_o))
    }
    column_name<-next_hc_model_name(column_name,colnames(factors))
    factors[names(clusters),column_name]<-clusters
    attr(vals$saved_data[[input$data_hc]],"factors")<-factors
    column_name
  }
  saveclusters<-reactive({
    vals$baghc0<-vals$baghc0+1
    temp<-cur_som.obs.clusters()
    if(input$hand_save=="create"){
      save_hc_clusters_to_factor(temp,input$hc_newname)
    } else{
      data_o<-vals$saved_data[[input$data_hc]]
      facold<-attr(data_o,"factors")[rownames(data_o),]
      facold[,input$hc_over]<-temp[rownames(data_o)]
      attr(vals$saved_data[[input$data_hc]],"factors")<-facold
    }

  })
  save_hc_model<-reactive({
    current<-phc()
    req(current)
    if(input$hand_save=="create"){
      req(input$hc_model_newname)
      model_name<-input$hc_model_newname
      existing<-NULL
    } else{
      req(input$hc_model_over)
      model_name<-input$hc_model_over
      existing<-if(model_name%in%hc_model_names_saved()){
        if(isTRUE(input$model_or_data=="som codebook")){
          som_model<-attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]]
          attr(som_model,"hc")[[model_name]]
        } else{
          attr(vals$saved_data[[input$data_hc]],"hc")[[model_name]]
        }
      } else{
        NULL
      }
    }
    record<-make_hc_record(current,existing=existing)
    set_hc_record(model_name,record)
    if(isTRUE(input$save_hc_factor)&&length(cluster_already())==0){
      req(input$save_hc_factor_name)
      save_hc_clusters_to_factor(current$somC,input$save_hc_factor_name)
    }
    saved_choices<-hc_model_names_saved()
    phc(NULL)
    updatePickerInput(session,"hc_models",
                      choices=saved_choices,
                      selected=model_name)
  })
  refresh_hc_models_input<-function(selected=NULL){
    choices<-c(if(!is.null(phc())){"new HC (unsaved)"}else{NULL},hc_model_names_saved())
    selected<-get_selected_from_choices(selected,choices)
    if(is.null(selected)&&length(choices)>0){
      selected<-choices[1]
    }
    vals$cur_hc_models<-selected
    updatePickerInput(session,"hc_models",choices=choices,selected=selected)
  }
  hc_model_edit_modal<-reactive({
    saved_models<-hc_model_names_saved()
    req(length(saved_models)>0)
    selected<-get_selected_from_choices(input$hc_models,saved_models)
    if(is.null(selected)){
      selected<-saved_models[1]
    }
    modalDialog(
      title=span(icon(verify_fa=FALSE,name=NULL,class="fas fa-edit"),"Edit HC models"),
      easyClose=TRUE,
      size="m",
      fluidRow(
        column(
          12,
          h4(strong("Rename")),
          div(style="display: flex;gap: 10px",
              pickerInput(ns("hc_model_rename_from"),"Model",choices=saved_models,selected=selected),
              textInput(ns("hc_model_rename_to"),"New name",value=selected)
          ),
          actionButton(ns("hc_model_rename_confirm"),strong("Rename"))
        ),
        column(
          12,
          tags$hr(),
          h4(strong("Delete")),
          checkboxGroupInput(ns("hc_model_delete_names"),"Models",choices=saved_models),
          actionButton(ns("hc_model_delete_confirm"),strong("Delete selected"))
        )
      ),
      footer=modalButton("Close")
    )
  })
  observeEvent(input$hc_model_rename_from,{
    req(input$hc_model_rename_from)
    updateTextInput(session,"hc_model_rename_to",value=input$hc_model_rename_from)
  },ignoreInit=TRUE)
  observeEvent(input$hc_model_rename_confirm,{
    req(input$hc_model_rename_from)
    req(input$hc_model_rename_to)
    old_name<-input$hc_model_rename_from
    new_name<-trimws(input$hc_model_rename_to)
    req(nzchar(new_name))
    hc_models<-get_hc_models()
    req(old_name%in%names(hc_models))
    if(!identical(old_name,new_name)){
      new_name<-next_hc_model_name(new_name,setdiff(names(hc_models),old_name))
      hc_models[[new_name]]<-hc_models[[old_name]]
      hc_models[[old_name]]<-NULL
      set_hc_models(hc_models)
    }
    removeModal()
    refresh_hc_models_input(new_name)
  },ignoreInit=TRUE)
  observeEvent(input$hc_model_delete_confirm,{
    delete_names<-input$hc_model_delete_names
    req(length(delete_names)>0)
    hc_models<-get_hc_models()
    delete_names<-intersect(delete_names,names(hc_models))
    req(length(delete_names)>0)
    hc_models[delete_names]<-NULL
    set_hc_models(hc_models)
    selected<-if(input$hc_models%in%delete_names){NULL}else{input$hc_models}
    removeModal()
    refresh_hc_models_input(selected)
  },ignoreInit=TRUE)

  savecodebook<-reactive({
    req(input$hand_save)
    data<-getdata_hc()
    m<-getmodel_hc()
    codes<-data.frame(do.call(cbind,m$codes))

    factors<-data.frame(cur_som.hc.clusters())

    rownames(factors)<-rownames(codes)<-paste0("unit_",1:nrow(codes))
    colnames(factors)<-paste0("Class",input$customKdata)

    attr(codes,"factors")<-factors
    temp<-codes
    temp<-data_migrate(data,temp,"new")
    attr(temp,"data.factor")<-NULL
    attr(temp,"factors")<-factors
    attr(temp,"datalist")<-NULL
    attr(temp,"filename")<-NULL
    attr(temp,"coords")<-NULL
    attr(temp,"base_shape")<-NULL
    attr(temp,"layer_shape")<-NULL
    attr(temp,"transf")<-NULL
    attr(temp,"nobs_ori")<-NULL
    if(input$hand_save=="create"){
      req(input$codebook_newname)
      vals$saved_data[[input$codebook_newname]]<-temp
    } else{
      req(input$codebook_over)
      vals$saved_data[[input$codebook_over]]<-temp
    }
    vals$new_facts<-NULL

  })
  save_switch<-reactive({
    switch(vals$hand_save,
           "Create Datalist with new mapping"= {savemapcode()},
           "create_codebook"=savecodebook(),
           "Save Clusters"= {saveclusters()},
           "Save HC model"= {save_hc_model()}
    )

  })


  observeEvent(input$data_hc,{
    vals$cur_data_hc<-input$data_hc
  })
  observeEvent(vals$saved_data,{
    selected=vals$cur_data_hc
    choices=names(vals$saved_data)
    selected=get_selected_from_choices(selected,choices)

    updatePickerInput(session,"data_hc",choices=choices, selected=selected)
  })
  observeEvent(names(attr(getdata_hc(), "som")),{
    choices= names(attr(getdata_hc(), "som"))
    selected=vals$cur_som_model_name

    selected<-get_selected_from_choices(selected,choices)
    if(is.null(selected)){
      selected<-choices[1]
    }

    updatePickerInput(session,'som_model_name',selected=selected,choices=choices)
  })
  observeEvent(input$model_or_data,{
    value=if(input$model_or_data=="som codebook"){
      paste0(input$data_hc,"(SOM codebook)")
    } else{ input$data_hc}

    updateTextInput(session,'hc_title',value=value)
    updateTextInput(session,'hc_xlab',value=if(input$model_or_data=="som codebook") {"SOM units"} else {"Observations"})
    updateTextInput(session,'hc_ylab',value="Height")
  })
  observeEvent(input$round_error,{
    vals$round_error<-input$round_error
  })
  observeEvent(input$hc_results,{
    vals$hc_results<-input$hc_results
  })
  observeEvent(ignoreInit = T,input$data_hc,{
    vals$show_mapcode_errors<-c("Within Sum of Squares","Dendrogram Height")
  })
  observeEvent(ignoreInit = T,input$help_hc_fun, {
    modal_help("hclust", intro=div(
      "iMESC implements ", tags$code("Hierarchical Clustering"), "analysis using the ", tags$code("hcut"), " function, which is part of the  ", tags$code("factoextra"), "package. The parameters that can be customized in iMESc are ",tags$code("hc_func"),"(clustering function),",  tags$code("k"), " (number of groups), ", tags$code("hc_method"), ", and ", tags$code("hc_metric"), " (distance measure for clustering numeric attributes). When clustering som codebook, iMESc uses the same distance metric used to train the SOM. The remaining parameters of the ", tags$code("hclust()"), " function are set to their default values. For more information regarding ",tags$code("hc_func"),"argument, refer to their documentation: ",
      actionLink("hclust_help", "hclust"),
      ",",  actionLink("hclust_diana", "Divisive Analysis Clustering(diana)"),
      ", and", actionLink("hclust_agnes", "Agglomerative Nesting (agnes)"),
      "."))
  })
  observeEvent(ignoreInit = T,input$diana_help,{
    modal_help("diana")
  })
  observeEvent(ignoreInit = T,input$agnes_help,{
    modal_help("agnes")
  })
  observeEvent(ignoreInit = T,input$hclust_help,{
    modal_help("hclust")
  })
  observeEvent(input$method.hc0,{
    vals$method.hc0<-input$method.hc0
  })
  observeEvent(input$hc_fun,{
    vals$hc_fun<-input$hc_fun
  })
  observeEvent(input$model_or_data,{
    vals$cur_model_or_data<-input$model_or_data
  })
  observeEvent(input$data_hc,{
    vals$cur_data=input$data_hc
  })
  observeEvent(input$hcsom_whatmap,{
    vals$hcsom_whatmap<-input$hcsom_whatmap
  })
  observeEvent(input$som_model_name,{
    vals$cur_som_model_name<-input$som_model_name
  })
  observeEvent(input$fixname,{
    vals$fixname<-input$fixname
  })
  observeEvent(input$fixmodel,{
    vals$fixmodel<-input$fixmodel
  })
  observeEvent(input$labhc,{
    vals$labhc<-input$labhc
  })
  observeEvent(input$hcut_labels,{
    vals$hcut_labels<-input$hcut_labels
  })
  observeEvent(ignoreInit = T,input$savemapcode,{
    if(input$savemapcode %% 2) {
      vals$hand_save<-"Create Datalist with new mapping"
      vals$hand_save3<-NULL
      vals$hand_save4<-NULL
      showModal(
        hand_save_modal()
      )
    }
  })
  observeEvent(ignoreInit = T,input$tools_savehc,{
    if(is.null(vals$fixname)){
      vals$fixname<-F
    }
    if(input$tools_savehc %% 2) {
      vals$hand_save<-"Save Clusters"
      vals$hand_save2<-p(
        div( style="color: gray",
             "Target:",em(input$data_hc),"->",em("Factor-Attribute")
        )
      )

      vals$hand_save3<-div(
        strong("Include name:"),
        inline(checkboxInput(ns("fixname"),"Datalist",vals$fixname, width="80px")),
        inline(checkboxInput(ns("fixmodel"),"Model",vals$fixmodel, width="80px")),
      )
      vals$hand_save4<-NULL
      showModal(
        hand_save_modal()
      )
    }
  })
  observeEvent(ignoreInit = T,input$tools_savehc_model,{
    req(phc())
    if(input$tools_savehc_model %% 2) {
      saved_cluster_cols<-cluster_already()
      save_factor_enabled<-length(saved_cluster_cols)==0
      vals$hand_save<-"Save HC model"
      vals$hand_save3<-'Save the current dendrogram and cluster cuts for later recovery'

      vals$hand_save2<-p(
        div(style="color: gray",
            "Target:",em(input$data_hc),"->",em(if(isTRUE(input$model_or_data=="som codebook")){"SOM-Attribute HC models"}else{"HC-Attribute"})
        )
      )
      vals$hand_save4<-div(


        div(style="display: flex; gap: 10px",
            div(checkboxInput(ns("save_hc_factor"),"Include clusters in Factor-Attribute",value=save_factor_enabled, width="120px")),
            div(textInput(ns("save_hc_factor_name"),"Column name",value=hc_factor_bagname()))
        ),


        if(!save_factor_enabled){
          div(
            style="color: gray; font-size: 12px;",
            em(paste0("Current clusters already exist in Factor-Attribute as: ",paste(saved_cluster_cols,collapse=", ")))
          )
        }
      )
      if(!save_factor_enabled){
        vals$hand_save4<-tagList(
          vals$hand_save4,
          tags$script(HTML(sprintf("setTimeout(function(){ $('#%s').prop('disabled', true); $('#%s').prop('disabled', true); }, 0);",ns("save_hc_factor"),ns("save_hc_factor_name"))))
        )
      }
      showModal(
        hand_save_modal()
      )
    }
  })
  observeEvent(ignoreInit = T,input$tools_edithc_model,{
    req(length(hc_model_names_saved())>0)
    showModal(hc_model_edit_modal())
  })
  observeEvent(ignoreInit = T,input$create_codebook,{
    if(input$create_codebook %% 2) {
      vals$hand_save<-"create_codebook"
      vals$hand_save2<-"Create Datalist with the Codebook and HC class"
      vals$hand_save3<-NULL
      vals$hand_save4<-NULL
      showModal(
        hand_save_modal()
      )
    }
  })
  observeEvent(ignoreInit = T,input$data_confirm,{
    vals$cur_hc_plot<-vals$hc_tab3_plot
    save_switch()
    removeModal()
  })
  observeEvent(ignoreInit = T,input$cancel_save,{
    removeModal()
  })
}

hc_module<-list()
#' @export
hc_module$ui<-function(id){
  module_progress("Loading module: Hierarchichal Clustering")
  ns<-NS(id)
  column(12,class="mp0",style="width: 100%",
         tags$style(HTML("
       input[type=checkbox], input[type=radio]{
         margin-top: 2px;
           color: red
         }

                         ")),

         h4("Hierarchical Clustering", class="imesc_title"),

         #actionLink(ns('save_bug'),"save_bug"),
         box_caret(
           ns("box_setup"),
           title="Model setup",
           color="#374061ff",
           inline=F,
           fluidRow(
             style="display: flex; flex-flow: row wrap;font-size: 12px",class="picker13",
             column(
               12,style="margin-bottom: 0px;",div(
                 style="gap: 10px;margin-bottom: 5px",class="som_grid",
                 pickerInput_fromtop_live(
                   ns("data_hc"),
                   strong("Datalist:"),choices = NULL
                 ),
                 radioButtons(ns("model_or_data"), strong("Clustering target:"), choiceValues = c("data", "som codebook"), choiceNames = c("Numeric-Attribute", "SOM-codebook"),width="130px"),
                 div(
                   pickerInput_fromtop_live(ns("som_model_name"), strong("Som model:"), choices=NULL, selected=NULL),
                   div(class="small_check",style="margin-top:3px",

                       checkboxInput(ns('show_hcsom_fine'),em("Select layers"))
                   ),
                   div(class="picker_fit inline_pickers",
                       style="background:white;padding: 5px;display:none",
                       id=ns("hcsom_fine"),
                       #checkboxInput(ns("use_weights"),span("Use weights",tipright("Use user weights from som model to calculate a weighted mean distance matrix")),width="250px"),
                       virtualPicker_unique(
                         ns("som_whatmap"),
                         strong("Whatmap",tipright("SOM Layers for clustering")), choices = NULL,search=F,multiple=T,allOptionsSelectedText="All layers",
                         alwaysShowSelectedOptionsCount=F
                       )
                   ),
                 ),


                 pickerInput_fromtop(ns("hc_fun"), strong("HC function:", actionLink(ns("help_hc_fun"), icon("fas fa-question-circle"))), choices = list("Hierarchical Clustering" = "hclust", "Agglomerative Nesting" = "agnes", "Divisive hierarchical clustering" = "diana")),
                 div(id=ns("disthc_id"),
                     pickerInput_fromtop(ns("disthc"), strong("Distance:"), choices = c('bray', "euclidean", 'jaccard'))
                 ),
                 pickerInput_fromtop(ns("method.hc0"),strong( "Method:"), choices = c("ward.D2", "ward.D", "single", "complete", "average", "mcquitty", "median", "centroid")),
                 div(
                   id=ns("hc_models_panel"),
                   style="display:none;",

                   div(style="display:flex; gap:6px; align-items:center",
                       div(
                         id=ns("hc_models_select_panel"),
                         pickerInput_fromtop_live(ns("hc_models"), strong("HC model:"), choices=NULL, selected=NULL)
                       ),
                       uiOutput(ns("saved_hc_print")),
                       div(
                         id=ns("tools_savehc_model_panel"),
                         style="display:none;margin-top: 20px",
                         tiphelp_icon(actionButton(ns("tools_savehc_model"), icon("fas fa-save"), type = "action", value = FALSE),"Save current HC model","right")
                       ),
                       div(
                         id=ns("tools_edithc_model_panel"),
                         style="display:none;margin-top: 20px",
                         tiphelp_icon(actionButton(ns("tools_edithc_model"), icon("fas fa-edit"), type = "action", value = FALSE),"Edit saved HC models","right")
                       )
                   )
                 )

               )
             ),
             column(12,style="margin-top: -5px;margin-bottom: 0px;",
                    uiOutput(ns("som_layers"))
             )

           )

         ),
         #actionLink(ns('save_bug'),"save_bug"),

         uiOutput(ns('hc_error')),
         tabsetPanel(id=ns("tabs_view"),title=NULL,
                     selected="tab1",
                     tabPanel("1. Dendrogram",value="tab1"),
                     tabPanel("2. Scree Plot",value="tab2"),
                     tabPanel("3. Cut Dendrogram",value="tab3")),

         tabsetPanel(
           id=ns("tabs"),
           type="hidden",
           #selected="tab4",

           header=hc_nclusters_module$ui(ns),
           hc_dendrogram_module$ui(ns),
           scree_plot_module$ui(ns("scree_plot")),
           cut_dendrogram_module$ui(ns),
           codebook_clusters_module$ui(ns),
           tabPanel(
             '5. Codebook screeplot',
             value='tab5',
             column(
               4,class="mp0",
               box_caret(
                 ns("box5_a"),
                 title="Options",

                 color="#c3cc74ff",
                 div(
                   numericInput(ns("mapcode_loop_K"), "K", 20),
                   checkboxGroupInput(ns("show_mapcode_errors"), 'Show error: ',
                                      choices = c("Within Sum of Squares", "Dendrogram Height"), selected=c("Within Sum of Squares", "Dendrogram Height")),
                   textInput(ns('code_screeplot_title'), "Title", ""),
                   pickerInput_fromtop(ns('code_screeplot_agg'), "Aggregate Errors", c("Mean", "Median", "Sum"))

                 )

               )
             ),

             column(
               8,class="mp0",style="position: absolute; right: 0px; padding-left: 6px",
               box_caret(ns("box5_b"),
                         title="Plot",
                         button_title=actionLink(ns("download_plot5"),"Download",icon("download")),
                         div(
                           actionButton(ns("mapcode_loop_go"), "Run loop"),
                           uiOutput(ns("plot5")))
               )
             )

           )
         )
  )
}
#' @export
hc_module$server<-function(id, vals){
  moduleServer(id,function(input, output, session) {
    ns<-session$ns
    ##
    legacy_hc_name<-function(prefix="Legacy HC"){
      parts<-c(prefix)
      if(length(input$hc_fun)>0) parts<-c(parts,input$hc_fun)
      if(length(input$method.hc0)>0) parts<-c(parts,input$method.hc0)
      if(length(input$disthc)>0&&isTRUE(input$model_or_data=="data")) parts<-c(parts,input$disthc)
      paste(parts,collapse=" - ")
    }
    next_hc_model_name<-function(base,choices){
      choices<-choices[!is.na(choices)]
      if(!base%in%choices){
        return(base)
      }
      i<-2
      repeat{
        candidate<-paste0(base," (",i,")")
        if(!candidate%in%choices){
          return(candidate)
        }
        i<-i+1
      }
    }
    hc_model_bagname<-reactive({
      k<-as.character(input$customKdata)
      k<-k[1]
      if(length(k)==0||!nzchar(k)||is.na(k)){
        k<-""
      }
      target<-if(isTRUE(input$model_or_data=="som codebook")){"SOM"} else{"numeric"}
      base<-paste0("HC",k,"_",target)
      next_hc_model_name(base,hc_model_names_saved())
    })
    make_hc_record<-function(hc,existing=NULL){
      req(hc)
      record<-if(is.null(existing)){list()} else{existing}
      if(is.null(attr(record,"hc.clusters"))){
        attr(record,"hc.clusters")<-list()
      }
      if(is.null(attr(record,"obs.clusters"))){
        attr(record,"obs.clusters")<-list()
      }
      k<-as.character(input$customKdata)
      attr(record,"hc.object")<-hc$hc.object
      attr(record,"hc.clusters")[[k]]<-hc$som.hc
      obs<-hc$somC
      attr(obs,"order")<-list(
        hc_sort=input$hc_sort,
        hc_ord_datalist=input$hc_ord_datalist,
        hc_ord_factor=input$hc_ord_factor
      )
      attr(record,"obs.clusters")[[k]]<-obs
      attr(record,"params")<-list(
        target=input$model_or_data,
        datalist=input$data_hc,
        som_model=input$som_model_name,
        hc_fun=input$hc_fun,
        hc_method=input$method.hc0,
        distance_metric=input$disthc,
        whatmap=vals$som_whatmap
      )
      record
    }
    migrate_hc_models<-function(data_name){
      if(!data_name%in%names(vals$saved_data)){
        return(invisible(NULL))
      }
      data<-vals$saved_data[[data_name]]
      hcs<-attr(data,"hc")
      if(!is.null(hcs)){
        old_names<-names(hcs)
        if("Numeric-hc"%in%old_names){
          old<-hcs[["Numeric-hc"]]
          if(!is.null(attr(old,"hc.object"))||!is.null(attr(old,"hc.clusters"))||!is.null(attr(old,"obs.clusters"))){
            new_name<-make.unique(c(setdiff(old_names,"Numeric-hc"),legacy_hc_name("Legacy HC Numeric")))[length(old_names)]
            hcs[[new_name]]<-old
            hcs[["Numeric-hc"]]<-NULL
            attr(vals$saved_data[[data_name]],"hc")<-hcs
          } else{
            hcs[["Numeric-hc"]]<-NULL
            attr(vals$saved_data[[data_name]],"hc")<-hcs
          }
        }
      }
      soms<-attr(vals$saved_data[[data_name]],"som")
      if(!is.null(soms)){
        for(som_name in names(soms)){
          som_model<-soms[[som_name]]
          old_has_hc<-!is.null(attr(som_model,"hc.object"))||!is.null(attr(som_model,"hc.clusters"))||!is.null(attr(som_model,"obs.clusters"))
          if(old_has_hc){
            hc_models<-attr(som_model,"hc")
            if(is.null(hc_models)){
              hc_models<-list()
            }
            new_name<-make.unique(c(names(hc_models),paste0("Legacy HC SOM - ",som_name)))[length(hc_models)+1]
            legacy<-list()
            attr(legacy,"hc.object")<-attr(som_model,"hc.object")
            attr(legacy,"hc.clusters")<-attr(som_model,"hc.clusters")
            attr(legacy,"obs.clusters")<-attr(som_model,"obs.clusters")
            hc_models[[new_name]]<-legacy
            attr(hc_models[[new_name]],"params")<-attr(som_model,"params")
            attr(som_model,"hc")<-hc_models
            attr(som_model,"hc.object")<-NULL
            attr(som_model,"hc.clusters")<-NULL
            attr(som_model,"obs.clusters")<-NULL
            soms[[som_name]]<-som_model
          }
        }
        attr(vals$saved_data[[data_name]],"som")<-soms
      }
      invisible(NULL)
    }
    hc_model_names_saved<-reactive({
      req(input$data_hc)
      migrate_hc_models(input$data_hc)
      data<-vals$saved_data[[input$data_hc]]
      if(isTRUE(input$model_or_data=="som codebook")){
        req(input$som_model_name)
        som_model<-attr(data,"som")[[input$som_model_name]]
        hc_models<-attr(som_model,"hc")
      } else{
        hc_models<-attr(data,"hc")
      }
      if(is.null(hc_models)){
        return(NULL)
      }
      if(length(hc_models)==0){
        return(NULL)
      }
      names(hc_models)[sapply(hc_models,function(x){
        !is.null(attr(x,"hc.object"))||!is.null(attr(x,"hc.clusters"))||!is.null(attr(x,"obs.clusters"))
      })]
    })
    current_hc_model_name<-reactive({
      choices<-c(if(!is.null(phc())){"new HC (unsaved)"}else{NULL},hc_model_names_saved())
      selected<-get_selected_from_choices(input$hc_models,choices)
      req(selected)
      selected
    })
    get_hc_record<-reactive({
      model_name<-current_hc_model_name()
      if(model_name=="new HC (unsaved)"){
        return(make_hc_record(phc()))
      }
      data<-vals$saved_data[[input$data_hc]]
      if(isTRUE(input$model_or_data=="som codebook")){
        som_model<-attr(data,"som")[[input$som_model_name]]
        record<-attr(som_model,"hc")[[model_name]]
      } else{
        record<-attr(data,"hc")[[model_name]]
      }
      req(record)
      record
    })
    get_hc_models<-function(){
      req(input$data_hc%in%names(vals$saved_data))
      if(isTRUE(input$model_or_data=="som codebook")){
        req(input$som_model_name)
        som_model<-attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]]
        hc_models<-attr(som_model,"hc")
      } else{
        hc_models<-attr(vals$saved_data[[input$data_hc]],"hc")
      }
      if(is.null(hc_models)){
        hc_models<-list()
      }
      hc_models
    }
    set_hc_models<-function(hc_models){
      req(input$data_hc%in%names(vals$saved_data))
      if(isTRUE(input$model_or_data=="som codebook")){
        req(input$som_model_name)
        som_model<-attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]]
        attr(som_model,"hc")<-hc_models
        attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]]<-som_model
      } else{
        attr(vals$saved_data[[input$data_hc]],"hc")<-hc_models
      }
    }
    set_hc_record<-function(model_name,record){
      hc_models<-get_hc_models()
      hc_models[[model_name]]<-record
      set_hc_models(hc_models)
      vals$cur_hc_models<-model_name
    }
    observeEvent(list(input$data_hc,input$model_or_data,input$som_model_name,vals$saved_data,phc()),{
      choices<-c(if(!is.null(phc())){"new HC (unsaved)"}else{NULL},hc_model_names_saved())
      selected<-get_selected_from_choices(vals$cur_hc_models,choices)
      if(is.null(selected)&&length(choices)>0){
        selected<-choices[1]
      }
      updatePickerInput(session,"hc_models",choices=choices,selected=selected)
    }, ignoreInit=FALSE)
    observeEvent(input$hc_models,{
      vals$cur_hc_models<-input$hc_models
    }, ignoreInit=TRUE)
    observe({
      saved_models<-hc_model_names_saved()
      has_unsaved<-!is.null(phc())
      has_models<-has_unsaved||length(saved_models)>0
      shinyjs::toggle("hc_models_panel",condition=has_models)
      shinyjs::toggle("hc_models_select_panel",condition=has_models)
      shinyjs::toggle("tools_savehc_model_panel",condition=has_unsaved)
      shinyjs::toggle("tools_edithc_model_panel",condition=length(saved_models)>0)
      shinyjs::toggleClass('tools_savehc_model_panel', class="save_changes",condition=has_unsaved&&identical(input$hc_models,"new HC (unsaved)"))

      shinyjs::toggleClass(
        "tools_savehc_model",
        class="save_changes",
        condition=has_unsaved&&identical(input$hc_models,"new HC (unsaved)")
      )
    })
    output$saved_hc_print<-renderUI({
      n<-length(hc_model_names_saved())
      req(n>0)
      div(class="saved_models",
          icon(verify_fa = FALSE,name=NULL,class="fas fa-hand-point-left"),"-",strong(n), "saved HC model(s)")
    })
    observeEvent(getdata_hc(),{

      if(is.null(attr(vals$saved_data[[input$data_hc]],"hc"))){
        attr(vals$saved_data[[input$data_hc]],"hc")<-list()
      }
    })
    cur_som.hc.object<-reactive({
      vals$cur_hc<-NULL
      cur<-attr(get_hc_record(),"hc.object")
      req(cur)
      cur
    })
    cur_som.obs.clusters<-reactive({
      req(input$data_hc)
      attrs_hc_obs<-attr(get_hc_record(),"obs.clusters")
      req(as.character(input$customKdata)%in%names(attrs_hc_obs))
      cur<-attrs_hc_obs[[ as.character(input$customKdata)]]
      req(cur)
      cur
    })
    cur_som.hc.clusters<-reactive({
      cur<-attr(get_hc_record(),"hc.clusters")[[ as.character(input$customKdata)]]
      req(cur)
      cur
    })

    cut_dendrogram_module$server(
      input=input,
      output=output,
      vals=vals,
      session=session,
      getdata_hc=getdata_hc,
      get_hc_record=get_hc_record,
      get_hcut_labels=get_hcut_labels,
      cur_som.hc.object=cur_som.hc.object,
      cur_som.obs.clusters=cur_som.obs.clusters,
      cur_som.hc.clusters=cur_som.hc.clusters
    )
    output$hc_error<-renderUI({
      req(vals$hc_messages)
      messages<-vals$hc_messages
      render_message(messages)
    })
    observeEvent(input$tabs_view,{
      vals$cur_hc_tab<-input$tabs_view
    })
    observeEvent(input$tabs_view, {
      updateTabsetPanel(session,'tabs',selected=input$tabs_view)

    })
    observeEvent(input$model_or_data,{
      if(input$model_or_data== "data"){
        removeTab("tabs_view","tab4")
        removeTab("tabs_view","tab5")
      } else{
        insertTab("tabs_view",tabPanel('4. Codebook clusters',value="tab4"),select=F)
        insertTab("tabs_view",tabPanel('5. Codebook screeplot',value='tab5')
        )
      }
    })
    box_caret_server('box_setup')
    box_caret_server('box5_a')
    box_caret_server('box5_b')

    bag_mp<-reactive({
      name0<-paste0('new_HC_',input$customKdata)
      if(length(input$fixname)>0){
        if(isTRUE(input$fixname)){
          name0<-paste0(input$data_hc,'_HC')
        }
      }
      name1<-make.unique(c(names(vals$saved_data),name0))
      name1[length(name1)]


    })
    choices_hc<-reactive({
      req(input$data_hc)
      a<-if (length(   names(vals$saved_data) > 0)) {
        "data"
      } else {
        NULL
      }

      b<-   if(length(attr(vals$saved_data[[input$data_hc]],"som"))>0){"som codebook"}else{NULL}
      res<-c(a, b)
      res
    })

    getdata_hc<-reactive({
      req(input$data_hc)
      req(input$data_hc%in%names(vals$saved_data))
      data=vals$saved_data[[input$data_hc]]
      validate(need(length(data)>0,"no data found"))

      data
    })
    get_hcut_labels<- reactive({
      if(input$model_or_data=="data"){
        req(input$hcut_labels)
        if(input$hcut_labels=="rownames"){
          rownames(getdata_hc())
        } else{
          as.factor(attr(getdata_hc(),"factors")[rownames(getdata_hc()), input$hcut_labels])
        }
      } else{NULL}


    })

    cutsom.reactive<-get_hc<-reactive({

      req(input$model_or_data)
      req(input$method.hc0)

      args<-list(data=getdata_hc(), k= input$customKdata,hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc,model_name=as.character(input$som_model_name),target=input$model_or_data,whatmap=vals$som_whatmap,use_weights=F)



      somC<-do.call(imesc_hclutering,args)
      vals$hc_messages<-attr(somC,"logs")

      somC

    })

    hc_screeplot<-function(data,model_or_data="som codebook",model_name=1,disthc,screeplot_hc_k,whatmap=NULL,use_weights=F){
      cmd_log_type<-d_log_type<-p_log_type<-NULL
      cmd_log_message<-d_log_message<-p_log_message<-NULL


      if(model_or_data=="som codebook"){
        m<- attr(data,"som")[[model_name]]
        weights<-rep(1,length(whatmap))
        if(isTRUE(use_weights)){
          weights<-NULL
        }
        dist=get_somdist_weighted(m,weights=weights,whatmap)


        data_log<-capture_log1(cmdscale)(dist, k=dim(dist)[1]-1)
        data<-data_log[[1]]
        cmd_log_message<-sapply(data_log$logs,function(x) x$message)
        if(length(cmd_log_message)==0){
          cmd_log_message<-NULL
          cmd_log_message<-NULL
        }
        cmd_log_type<-sapply(data_log$logs,function(x) x$type)
      } else{
        dist_log<-capture_log1(vegan::vegdist)(data,disthc)
        dist<-dist_log[[1]]
        d_log_message<-sapply(dist_log$logs,function(x) x$message)
        if(length(d_log_message)==0){
          d_log_message<-NULL
          d_log_message<-NULL
        }
        d_log_type<-sapply(dist_log$logs,function(x) x$type)
      }
      if(!is.null(dist)){
        p_log<-capture_log1(imesc_fviz_nbclust)(data, factoextra::hcut, method = "wss", k.max = screeplot_hc_k, diss=dist)




      } else{
        p_log<-list(result=NULL,logs=list(list(message="Error", type="error")))
      }
      x<-p_log$logs
      p<-p_log[[1]]

      if(!is.null(p)){
        p<-p+ theme_minimal() + ggtitle("the Elbow Method")
      }
      p_log_message<-sapply(p_log$logs,function(x) x$message)
      p_log_type<-sapply(p_log$logs,function(x) x$type)
      if(length(p_log_message)==0){
        p_log_message<-NULL
        p_log_type<-NULL
      }

      if(length(p)>0){
        p$data$clusters<-as.numeric(p$data$clusters)
        re<-p$data
        colnames(re)<-c("Clusters","WSS")
        attr(p,"result")<-re

      }
      if(is.null(p)){
        p<-FALSE
      }
      logs<-c(cmd_log_message,
              d_log_message,
              p_log_message)
      if(!is.null(logs)){
        attr(logs,"type")<-c(cmd_log_type,
                             d_log_type,
                             p_log_type)
      }

      attr(p,"logs")<-logs

      p
    }
    hc_gapplot<-function(data,model_or_data="som codebook",model_name=1,disthc,screeplot_hc_k,nboot=50,maxSE=list(method="firstSEmax",SE.factor=1),whatmap=NULL,use_weights=F){
      cmd_log_type<-d_log_type<-p_log_type<-NULL
      cmd_log_message<-d_log_message<-p_log_message<-NULL
      extra_args<-list()

      if(model_or_data=="som codebook"){
        m<- attr(data,"som")[[model_name]]
        weights<-rep(1,length(whatmap))
        if(isTRUE(use_weights)){
          weights<-NULL
        }
        dist=get_somdist_weighted(m,weights=weights,whatmap)

        data_log<-capture_log1(cmdscale)(dist, k=dim(dist)[1]-1)
        data<-data_log[[1]]
        cmd_log_message<-sapply(data_log$logs,function(x) x$message)
        if(length(cmd_log_message)==0){
          cmd_log_message<-NULL
          cmd_log_type<-NULL
        } else{
          cmd_log_type<-sapply(data_log$logs,function(x) x$type)
        }
      } else{
        dist_log<-capture_log1(vegan::vegdist)(data,disthc)
        dist<-dist_log[[1]]
        d_log_message<-sapply(dist_log$logs,function(x) x$message)
        if(length(d_log_message)==0){
          d_log_message<-NULL
          d_log_type<-NULL
        } else{
          d_log_type<-sapply(dist_log$logs,function(x) x$type)
        }
        extra_args$hc_metric<-disthc
      }
      if(!is.null(data)){
        gap_args<-list(
          x=data,
          FUNcluster=factoextra::hcut,
          method="gap_stat",
          k.max=screeplot_hc_k,
          nboot=nboot,
          verbose=FALSE,
          maxSE=maxSE
        )
        if(!is.null(extra_args$hc_metric)){
          gap_args$hc_metric<-extra_args$hc_metric
        }
        p_log<-do.call(capture_log1(imesc_fviz_nbclust),gap_args)
      } else{
        p_log<-list(result=NULL,logs=list(list(message="Error", type="error")))
      }
      p<-p_log[[1]]

      if(!is.null(p)){
        p<-p+theme_minimal()+ggtitle("Gap Statistic")
      }
      p_log_message<-sapply(p_log$logs,function(x) x$message)
      p_log_type<-sapply(p_log$logs,function(x) x$type)
      if(length(p_log_message)==0){
        p_log_message<-NULL
        p_log_type<-NULL
      }

      if(length(p)>0){
        attr(p,"result")<-p$data
      }
      if(is.null(p)){
        p<-FALSE
      }
      logs<-c(cmd_log_message,
              d_log_message,
              p_log_message)
      if(!is.null(logs)){
        attr(logs,"type")<-c(cmd_log_type,
                             d_log_type,
                             p_log_type)
      }

      attr(p,"logs")<-logs

      p
    }
    getdata_for_hc<-reactive({
      req(input$data_hc)
      datalist<-vals$saved_data
      data<-vals$saved_data[[input$data_hc]]
      req(length(data)>0)
      res0<-unlist(
        lapply(datalist, function (x){
          all(rownames(data)%in%rownames(x))
        })
      )
      names(res0[res0==T])
    })
    nclusters_server<-hc_nclusters_module$server(
      input=input,
      output=output,
      vals=vals,
      session=session,
      getdata_for_hc=getdata_for_hc,
      obs.clusters=cur_som.obs.clusters,
      getdata_hc=getdata_hc,
      get_hc=get_hc,
      hc_model_names_saved=hc_model_names_saved
    )
    phc<-nclusters_server$phc
    cluster_already<-nclusters_server$cluster_already
    choices_hc_names<-reactive({
      req(input$data_hc%in%names(vals$saved_data))
      a<-if (length(   names(vals$saved_data) > 0)) {
        "Numeric-Attribute"
      } else {
        NULL
      }

      b<-   if(length(attr(vals$saved_data[[input$data_hc]],"som"))>0){"SOM-codebook"}else{NULL}
      res<-c(a, b)
      res
    })
    getmodel_hc<-reactive({
      req(input$data_hc)
      req(input$som_model_name)
      req(input$model_or_data=="som codebook")
      data<-getdata_hc()
      m<-attr(data,"som")[[as.character(input$som_model_name)]]
      req(m)
      m
    })
    getmodel_hc0<-reactive({
      req(input$data_hc)
      req(input$som_model_name)
      data<-getdata_hc()
      m<-attr(data,"som")[[as.character(input$som_model_name)]]
      m
    })
    scree_plot_module$server(
      "scree_plot",
      vals=vals,
      getdata_hc=getdata_hc,
      getmodel_hc=getmodel_hc,
      model_or_data=reactive(input$model_or_data),
      som_model_name=reactive(input$som_model_name),
      disthc=reactive(input$disthc),
      hc_screeplot_fun=hc_screeplot,
      hc_gapplot_fun=hc_gapplot,
      hc_vatplot_fun=hc_vatplot,
      hc_silhouetteplot_fun = hc_silhouetteplot,
      hc_stabilityplot_fun = hc_stabilityplot,
      hc_consensusplot_fun = hc_consensusplot
    )
    getsom_layers<-reactive({
      m<-getmodel_hc0()
      layers<-names(m$data)
      layers

    })
    codebook_server<-codebook_clusters_module$server(
      input=input,
      output=output,
      session=session,
      vals=vals,
      getdata_hc=getdata_hc,
      getmodel_hc=getmodel_hc,
      getmodel_hc0=getmodel_hc0,
      cluster_assignments=cur_som.hc.clusters,
      getsom_layers=getsom_layers,
      model_object=phc,
      model_name=reactive(input$som_model_name),
      cluster_count=reactive(input$customKdata),
      is_codebook=reactive(input$model_or_data=="som codebook")
    )
    argsplot_somplot<-codebook_server$argsplot_somplot
    savemapcode<-codebook_server$savemapcode

    labhc<-reactive({
      req(input$labhc)
      as.character(attr(getdata_hc(),"factors")[rownames(getdata_hc()),as.character(input$labhc)])
    })

    model_cluster_save_module$server(
      input=input,
      output=output,
      session=session,
      vals=vals,
      getdata_hc=getdata_hc,
      getmodel_hc=getmodel_hc,
      cur_som.hc.clusters=cur_som.hc.clusters,
      cur_som.obs.clusters=cur_som.obs.clusters,
      phc=phc,
      cluster_already=cluster_already,
      hc_model_names_saved=hc_model_names_saved,
      hc_model_bagname=hc_model_bagname,
      make_hc_record=make_hc_record,
      set_hc_record=set_hc_record,
      get_hc_models=get_hc_models,
      set_hc_models=set_hc_models,
      next_hc_model_name=next_hc_model_name,
      bag_mp=bag_mp,
      savemapcode=savemapcode
    )

    hcplot5<-reactive({
      k.max<-input$mapcode_loop_K
      req(input$som_model_name)
      result<-attr(attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]],"codebook_screeplot")
      req(input$show_mapcode_errors%in%result$variable)
      df<-result[    result$variable%in%input$show_mapcode_errors,]
      p<-ggplot(df)+geom_line(aes(k,value))+facet_wrap(~variable,scales="free")+xlab("Number of Clusters")
      p
    })
    output$plot5<-renderUI({
      renderPlot(hcplot5())
    })

    observeEvent(input$download_plot5,ignoreInit = T,{
      vals$hand_plot<-"generic_gg"
      module_ui_figs("downfigs")
      datalist_name=attr(getdata_hc(),'datalist')
      generic=hcplot5()
      mod_downcenter<-callModule(module_server_figs,"downfigs", vals=vals,generic=generic,message="SOM- Scree plot", name_c="screeplot",datalist_name=datalist_name)
    })
    observe({
      shinyjs::toggle("Kcustom",condition=input$tabs%in%c("tab3","tab4"))
    })
    observe({
      shinyjs::toggle('disthc_id',condition=input$model_or_data=="data")

    })
    observe({
      shinyjs::toggle('som_model_name',condition=input$model_or_data=="som codebook")




    })
    observe({
      m<-getmodel_hc()
      shinyjs::toggle('som_whatmap',condition=input$model_or_data=="som codebook"&length(m$codes)>1)


    })
    observe({
      shinyjs::toggle('show_hcsom_fine',condition=!is.null(vals$som_whatmap)&input$model_or_data=="som codebook")
    })
    observe({
      if(is.null(vals$cur_whatmap)){
        m<-getmodel_hc()
        choices=names(m$codes)
        if(is.null(choices)||length(choices)==0){
          choices=seq_along(m$codes)
        }
        vals$cur_whatmap<-choices
      }
    })
    {

      observeEvent(getmodel_hc(),{
        m<-getmodel_hc()
        choices=names(m$codes)
        if(is.null(choices)||length(choices)==0){
          choices=seq_along(m$codes)
        }

        if(length(choices)==1){
          choices=1
        }
        selected<-vals$som_whatmap
        if(!is.null(selected)){
          selected<-selected[selected%in%choices]
        }
        if(is.null(selected)||length(selected)==0){
          selected=choices
        }
        vals$som_whatmap<-if(length(choices)==1){NULL}else{selected}
        #updateCheckboxInput(session,"show_hcsom_fine",value=F)
        shinyWidgets::updateVirtualSelect('som_whatmap',choices=choices,selected=selected)
      })




      observe({
        m<-getmodel_hc()
        choices=names(m$codes)
        if(is.null(choices)||length(choices)==0){
          choices=seq_along(m$codes)
        }

        if(length(choices)==1){
          vals$som_whatmap<-NULL
        } else{
          selected<-input$som_whatmap
          if(!is.null(selected)){
            selected<-selected[selected%in%choices]
          }
          if(is.null(selected)||length(selected)==0){
            selected<-choices
          }
          vals$som_whatmap<-selected
        }

      })

      args_hc1<-reactive({
        list(data=getdata_hc(), k= 2,hc_fun=input$hc_fun,hc_method=input$method.hc0,distance_metric=input$disthc,model_name=as.character(input$som_model_name),target=input$model_or_data,whatmap=vals$som_whatmap,use_weights=F)
      })

      hc_model<-reactive({
        args<-args_hc1()
        somC<-do.call(imesc_hclutering,args)
        somC
      })
      hc_dendrogram_module$server(
        input=input,
        output=output,
        session=session,
        vals=vals,
        getdata_hc=getdata_hc,
        labhc=labhc,
        args_hc1=args_hc1,
        hc_model=hc_model
      )

      observeEvent(ignoreInit = T,input$mapcode_loop_go,{


        somC<-hc_model()

        vals$mapcode_loop_res<-NULL

        m<-getmodel_hc()

        k.max<-input$mapcode_loop_K
        hc_fun=input$hc_fun;hc_method=input$method.hc0





        result<-screeplot_som(m,k.max,hc_fun,hc_method,whatmap=vals$som_whatmap,use_weights=F)



        dend_hei<-somC$hc.object$height
        result$dh<-rev(dend_hei)[2:k.max]
        colnames(result)<-c("k","Within Sum of Squares","Dendrogram Height")

        result<-reshape2::melt(result,"k")

        attr(result,"class_result")<-"som screeplot"
        attr(attr(vals$saved_data[[input$data_hc]],"som")[[input$som_model_name]],"codebook_screeplot")<-result



      })


      output$som_layers<-renderUI({
        m<-getmodel_hc()
        req(length(m$codes)>1)
        req(input$model_or_data=="som codebook")
        div(style="display: flex; justify-content: flex-start;;align-items: flex-end",
            div(strong("SOM layers:",style="white-space: nowrap;")),
            div(style="white-space: normal;max-width: 350px; padding-left: 2px",
                emgreen(
                  paste(vals$som_whatmap, collapse="; ")
                ))
        )
      })



    }
    observe({
      shinyjs::toggle("hc_side4",condition = input$model_or_data == "som codebook")
    })
    observe({
      req(input$model_or_data)
      if(input$model_or_data=="som codebook"){
        m<- getmodel_hc()
        data<-m$codes[[1]]
      } else{
        data<-  getdata_hc()
      }

      updateNumericInput(session,"screeplot_hc_k", value = round(nrow(data)/2))

    })
    observeEvent(getdata_hc(),{
      choiceValues<-choices_hc()
      choiceNames<-choices_hc_names()

      selected<-get_selected_from_choices(vals$cur_model_or_data,choiceValues)
      if(is.null(selected)&&length(choiceValues)>0){
        selected<-choiceValues[1]
      }
      vals$cur_model_or_data<-selected
      updateRadioButtons(session,"model_or_data",choiceNames =choiceNames,choiceValues =choiceValues,selected=selected )
    })
    observeEvent(getdata_hc(),{
      data<-getdata_hc()
      choices<-c(colnames(attr(data,"factors")))
      selected=vals$hcut_labels
      choices = c("rownames",choices)
      updatePickerInput(session,'hcut_labels',choices=choices,selected=selected,options=shinyWidgets::pickerOptions(liveSearch=T))
    })
    observe({
      req(vals$update_state)
      update_state<-vals$update_state
      ids<-names(update_state)
      update_on<-grepl(id,ids)
      names(update_on)<-ids
      to_loop<-names(which(update_on))
      withProgress(min=1,max=length(to_loop),message="Restoring",{
        for(i in to_loop) {
          idi<-gsub(paste0(id,"-"),"",i)
          incProgress(1)
          restored<-restoreInputs2(session, idi, update_state[[i]])
          if(isTRUE(restored)){
            vals$update_state[[i]]<-NULL
          }

        }
      })

    })

  })
}




















