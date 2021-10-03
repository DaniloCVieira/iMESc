getRF<-function(rfs,groups,somC, method="rf"){
  selection<-which(rfs$acutable$method==method&rfs$acutable$k==groups)
  RF=  rfs$trains[[selection]]
  col_vector=somC$colhabs
  sup="supervisor: Clusters from SOM"
  attr(RF,"supervisor")<-sup
  attr(RF,"col_vector")<-col_vector
  return(RF)
}


plot_accuclus<-function(rfs, sugg=NULL)
{
  res<-rfs$acutable


  {par(mar=c(5,5,5,5))

    plot(res, type="n", ylab="Accuracy", lwd=2, col="darkgreen", xlab="Number of Clusters", main="RF accuracies vs number of clusters", xlim=c(2,nrow(res)),axes=F)
    axis(1)
    axis(2,col.axis="darkgreen",col="darkgreen")
    if(!is.null(sugg))
    {
      ws<-attr(sugg,"ws")
      points<-attr(sugg,"smw")
      abline(v=sugg, col='red', lty=2)
      legend("topr",legend=c(paste("Suggested breakpoint:",sugg),
                             paste("window size:", ws)), bty="n", cex=.8)



      par(new = TRUE)
      plot(points[,1],points[,2], type="l", col="darkcyan", ann=F, axes=F, xlim=c(2,nrow(res)), lty=2)
      axis(4,col.axis="darkcyan",col="darkcyan", lty=2)

      mtext("diss smw",4, line=3, col="darkcyan", crt=90)



    }
    par(new = TRUE)
    plot(res,type="l",lwd=3, col="darkgreen", ann=F, axes=F, xlim=c(2,nrow(res)))

  }




}





machineRF<-function(m,data, k=5, type="data", method="ward.D2",dist="bray"){

  if(type=="som"){
    codes<-kohonen::getCodes(m)
    d=kohonen::object.distances(m,"codes")
  } else {
    #validate(need(anyNA(m)==F, "The selected data contains missing values. Please remove them in Upload>Transform."))
    codes<-m
    d=vegdist(m,dist)
  }


  clusters<-2:k
  treinamento<-list()
  init <- list()
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while.... ', value = 0,
               {
    for(clus in 1:length(clusters)) {
      if(type=="som"){
      somC<-cutsom(m,clusters[clus], method.hc=method)
      y= somC[[1]]} else  {
          y=cutree(cutdata(m, method.hc=method,dist=dist),clusters[clus])
        }
      base<- data.frame(na.omit(data[names(y),]))
      y<-y[rownames(base)]
      base$y<-as.factor(y)
      treinamento[[clus]] = randomForest::randomForest(y ~ ., data = base)
   incProgress(1/k)
    }

  }
  )

trains<-treinamento[which(unlist(lapply(treinamento, length))>0)]
  acutable<-data.frame(
    n_clusters=clusters,
    accuracy=unlist(lapply(trains, function (x) sum(diag(x$confusion/sum(x$confusion))) ))
   )
  rfs=list(m=m, trains=trains,acutable=acutable)

  return(rfs)
}


wrapRF<-function(data,supv=NULL, ntree=500, seed=NULL,trainControl.args = list(method = 'repeatedcv', number = 2,repeats=1),preProcess="",...)
{

  if(preProcess==""){ preProcess=NULL}

  suppressWarnings({
    controle_treinamento=do.call(trainControl,trainControl.args)
    if(is.null(seed==F)){set.seed(seed)}
    base<-na.omit(cbind(HAB=supv,data[rownames(supv),]))
    colnames(base)[1]<-"HAB"


    if( class(unlist(supv))=="factor")
    {

    lev<-names(table(base$HAB)>0)[which(table(base$HAB)>0)]
    base$HAB<-factor(base$HAB,levels=lev, labels = lev)

    }
    modelo = train(HAB ~ ., data = base, trControl = controle_treinamento, method ="rf", ntree=ntree, localImp = TRUE, preProcess=preProcess)
    return(modelo)
  })



}



getConfusion<-function(rf)
{
  ConfMat<-list()
  if(sum(names(rf)=="finalModel")==0)
  {
    ConfMat$table<-rf$confusion[,-c(ncol(rf$confusion))]
    ConfMat$text<-rownames( ConfMat$table)
    ConfMat$table<-ConfMat$table/sum(ConfMat$table)
  } else {  ConfMat<-confusionMatrix(rf)}
  CM<-CMerror<-ConfMat$table

  CM3<-as.data.frame(matrix(NA,nrow(CM),ncol(CM)))
  rownames(CM3)<-rownames(as.matrix(ConfMat$table))
  colnames(CM3)<-colnames(as.matrix(ConfMat$table))
  CM3[rownames(CM),colnames(CM)]<-CM
  diag(CMerror)<-0
  class.error<-rowSums(CMerror)/rowSums(CM)
  CM3[names(class.error),(ncol(CM3)+1)]<-data.frame(class.error=rowSums(CMerror)/rowSums(CM))[1]
  attr(CM3,"title")<- ConfMat$text

  return(CM3)
}



multipimp<-function(rf)
{


  if(class(rf)[1]=="randomForest"){
    forest=rf } else {forest=rf$finalModel}

  multi_imps = measure_importance(forest)
  indicadores<-as.character(multi_imps[order(multi_imps$mean_min_depth),][,"variable"])
  min_depth_frame <- min_depth_distribution(forest)
  res_multi<-list(min_depth_frame,multi_imps)
  return(res_multi)
}

prf<-function(res_multi, sigs=T, sig.value=0.05,size_plot=10,
              min_no_of_trees = 0,
              mean_sample = "top_trees",
              mean_scale = FALSE)
{
  multi_imps=res_multi[[2]]
  multi_imps$col<-"gray"
  min_depth_frame=res_multi[[1]]


  multi_imps<-multi_imps[order(multi_imps$mean_min_depth),]
  multi_imps[which(multi_imps$p_value<sig.value),"col"]<-"red"


  BoldImportance<-multi_imps[which(multi_imps$p_value<sig.value),]


  sig_vars<-which(min_depth_frame$variable%in%BoldImportance$variable)

  if(isTRUE(sigs)){min_depth_frame_sigs<-min_depth_frame[sig_vars,]
} else{    min_depth_frame_sigs<-min_depth_frame
  }

  if(isTRUE(sigs)){pick<-nrow(BoldImportance)} else{pick=sigs}


  min_depth_frame_sigs$minimal_depth

  MDD.plot<-plot_min_depth_distribution(min_depth_frame_sigs, k=pick,
                                        min_no_of_trees = min_no_of_trees,
                                        mean_sample = mean_sample,
                                        mean_scale = mean_scale)
  MDD.plot<-MDD.plot+theme_set(theme_grey(base_size = size_plot))


  MWI1.plot<-plot_multi_way_importance(multi_imps, no_of_labels = nrow(BoldImportance))
  attr(MDD.plot,"sigs")<-BoldImportance
  par(mfrow=c(1,2))
  return(MDD.plot)
  }


prf_multi<-function(res_multi, sigs=T, sig.value=0.05,x_measure = "mean_min_depth",
                    y_measure = "times_a_root",
                    size_measure = NULL,
                    min_no_of_trees = 0,
                    no_of_labels = 10)
{
  multi_imps=res_multi[[2]]
  multi_imps$col<-"gray"
  min_depth_frame=res_multi[[1]]


  multi_imps<-multi_imps[order(multi_imps$mean_min_depth),]
  multi_imps[which(multi_imps$p_value<sig.value),"col"]<-"red"


  BoldImportance<-multi_imps[which(multi_imps$p_value<sig.value),]


  sig_vars<-which(min_depth_frame$variable%in%BoldImportance$variable)

  if(isTRUE(sigs)){min_depth_frame_sigs<-min_depth_frame[sig_vars,]
  } else{    min_depth_frame_sigs<-min_depth_frame
  }

  if(isTRUE(sigs)){pick<-nrow(BoldImportance)} else{pick=sigs}
  MDD.plot<-plot_multi_way_importance(multi_imps, no_of_labels = nrow(BoldImportance),
                                      x_measure = x_measure,
                                      y_measure = y_measure,
                                      size_measure = size_measure,
                                      min_no_of_trees = min_no_of_trees)
  attr(MDD.plot,"sigs")<-BoldImportance
  par(mfrow=c(1,2))
  return(MDD.plot)
}




ptree<-function(rfs,
                teststat = c("quad", "max"),
                testtype = c("Bonferroni", "MonteCarlo",
                             "Univariate", "Teststatistic"),
                mincriterion = 0.95,
                minsplit = 20,
                minbucket = 7,
                nresample = 9999,
                maxsurrogate = 0,
                mtry = 0,
                savesplitstats = TRUE,
                maxdepth = 0,
                remove_weights = FALSE, palette="matlab.like2") {


  data=rfs[-1]
  prev=rfs[,1]

  if(is.factor(prev)==T)
  {
    nlevels<-nlevels(prev)


    colors=getcolhabs(palette,nlevels)

  } else {colors<-"gray"}


  datatree<-data.frame(prev=prev,data)

  treecontrol=ctree_control(teststat = teststat,
                            testtype = testtype,
                            mincriterion = mincriterion,
                            minsplit = minsplit,
                            minbucket = minbucket,
                            nresample = nresample,
                            maxsurrogate = maxsurrogate,
                            mtry = mtry,
                            savesplitstats = savesplitstats,
                            maxdepth = maxdepth,
                            remove_weights = remove_weights)


  model <- ctree(
    prev ~., data = datatree, controls=treecontrol)
  plot(model, tp_args = list(fill=colors))
  dectree<-recordPlot()

}


plotCM<-function(rf, palette)
{
  {

    my.data=round(getConfusion(rf),2)
    #my.data<-my.data[,-c(2:4)]
   # my.data<-my.data[-c(2:4),]
    rownames(my.data)<-paste("     ",rownames(my.data))

    subtitle<-paste("Accuracy:",sum(diag(as.matrix(my.data[,-ncol(my.data)]))),"%")
    title<-"Confusion Matrix"

    col_vector<-getcolhabs(palette,nrow(my.data))
    col_vector2<-c("white",col_vector)


    ggtab <- ggtexttable(my.data, theme = ttheme(
      "default",base_size = 15,
      rownames.style=
        rownames_style(
          color = "black",
          face = "bold",
          size = 15,
          fill = col_vector2,
          linewidth = 1,
          linecolor = "white",
          parse = FALSE, x = 0.5,hjust =0.7
        ),
      colnames.style=
        colnames_style(
          color = "black",
          face = "bold",
          size = 15,
          fill = c(col_vector2[-1],"white"),
          linewidth = 1,
          linecolor = "white",
          parse = FALSE
        )
    ))


    for(i in 2:(nrow(my.data)+1)){
      ggtab <-thead_add_border(
        ggtab,
        from.row = i,
        to.row = i,
        from.column = i,
        to.column = i,
        linetype = 1,
        linewidth = 3,
        linecolor = col_vector2[i]
      )
    }

    ggtab<-tab_add_title(
      ggtab, subtitle, just="left",padding = unit(1.5, "line"),
      size = 15,face="italic",
    )
    ggtab<-tab_add_title(
      ggtab, title, just="left",padding = unit(.5, "line"),
      size = 18,face="bold",
    )


    ggtab<-tab_add_footnote(ggtab,  gsub("Confusion Matrix","",attr(getConfusion(rf),'title')), face="italic")
    #ggtab$theme$plot.margin<-margin(0, -10, 0, 0,"cm")
  }
  #attributes(ggtab$theme$plot.margin)
  #ggtab$theme$panel.spacing  <-'7points'
  #ggtab$theme
  #ggtab$plot

  return(ggtab)
}



gettrain<-function (x, printCall = FALSE, details = FALSE, selectCol = FALSE,
          showSD = FALSE, ...)
{
  if (!is.null(x$modelInfo$label))
    cat(x$modelInfo$label, "\n\n")
  if (printCall)
    printCall(x$call)
  if (!is.null(x$trainingData)) {
    chDim <- dim(x$trainingData)
    chDim[2] <- chDim[2] - 1
    if (x$modelType == "Classification") {
      lev <- levels(x)
      if (is.character(lev))
        chDim <- c(chDim, length(lev))
    }
    else lev <- NULL
    chDim <- format(chDim)
    cat(chDim[1], " samples", sep = "")
    if (!is.null(x$control$indexFinal))
      cat(",", length(x$control$indexFinal), "used for final model\n")
    else cat("\n")
    cat(chDim[2], " predictor", ifelse(chDim[2] > 1,
                                       "s\n", "\n"), sep = "")
    if (is.character(lev)) {
      cat(chDim[3], "classes:", paste("'",
                                      lev, "'", sep = "", collapse = ", "),
          "\n")
    }
    cat("\n")
  }
  if (!is.null(x$preProc)) {
    pp_list(x$preProc$method)
  }  else {
    if (inherits(x, "train.recipe")) {
      step_names <- function(x) gsub("^step_", "",
                                     class(x)[1])
      steps_used <- unlist(lapply(x$recipe$steps, step_names))
      ppText <- paste("Recipe steps:", paste(steps_used,
                                             collapse = ", "))
      cat(truncateText(ppText), "\n")
    }
    else cat("No pre-processing\n")
  }
  if (!is.null(x$control$index)) {
    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)
    resampText <- caret:::resampName(x)
    cat("Resampling:", resampText, "\n")
    if (x$control$method != "none") {
      outLabel <- x$metric
      resampleN <- as.character(resampleN)
      if (numResamp > 5)
        resampleN <- c(resampleN[1:6], "...")
      cat("Summary of sample sizes:", paste(resampleN,
                                            collapse = ", "), "\n")
    }
  }
  if (!is.null(x$control$sampling)) {
    cat("Addtional sampling using ")
    cat(switch(x$control$sampling$name, down = "down-sampling",
               up = "up-sampling", smote = "SMOTE",
               rose = "ROSE", custom = "a custom function"))
    if (!is.null(x$preProc)) {
      if (x$control$sampling$first)
        cat(" prior to pre-processing")
      else cat(" after to pre-processing")
    }
    cat("\n\n")
  }
  if (x$control$method != "none") {
    tuneAcc <- x$results
    tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]
    cat("Resampling results")
    if (dim(tuneAcc)[1] > 1)
      cat(" across tuning parameters")
    if (showSD)
      cat(" (values below are 'mean (sd)')")
    cat(":\n\n")
    if (dim(tuneAcc)[1] > 1) {
      numParam <- length(x$bestTune)
      finalTune <- x$bestTune
      optValues <- paste(names(finalTune), "=", format(finalTune
                                                       ))
      optString <- paste0("The final ", ifelse(numParam >
                                                 1, "values", "value"), " used for the model ",
                          ifelse(numParam > 1, "were ", "was "),
                          caret:::stringFunc(optValues), ".")
      finalTune$Selected <- "*"
      if (any(names(tuneAcc) %in% "method"))
        names(tuneAcc)[names(tuneAcc) %in% "method"] <- ".method"
      if (any(names(finalTune) %in% "method"))
        names(finalTune)[names(finalTune) %in% "method"] <- ".method"
      tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)
      if (any(names(tuneAcc) %in% ".method"))
        names(tuneAcc)[names(tuneAcc) %in% ".method"] <- "method"
      tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""
    }  else optString <- ""
    sdCols <- grep("SD$", colnames(tuneAcc))
    if (showSD) {
      sdCheck <- unlist(lapply(tuneAcc[, sdCols, drop = FALSE],
                               function(u) all(is.na(u))))
      if (any(sdCheck)) {
        rmCols <- names(sdCheck)[sdCheck]
        tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% rmCols)]
      }
    }    else {
      if (length(sdCols) > 0)
        tuneAcc <- tuneAcc[, -sdCols, drop = FALSE]
    }
    params <- names(x$bestTune)
    if (!all(params == "parameter")) {
      numVals <- apply(tuneAcc[, params, drop = FALSE],
                       2, function(x) length(unique(x)))
      if (any(numVals < 2)) {
        constString <- NULL
        for (i in seq(along = numVals)) {
          if (numVals[i] == 1)
            constString <- c(constString, paste0("Tuning parameter '",
                                                 names(numVals)[i], "' was held constant at a value of ",
                                                 stringFunc(tuneAcc[1, names(numVals)[i]])))
        }
        discard <- names(numVals)[which(numVals == 1)]
        tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% discard),
                           drop = FALSE]
      }   else constString <- NULL
    }  else constString <- NULL
    tuneAcc <- tuneAcc[, !grepl("Apparent$|Optimism$",
                                names(tuneAcc)), drop = FALSE]
    colnames(tuneAcc)[colnames(tuneAcc) == ".B"] <- "Resamples"
    nms <- names(tuneAcc)[names(tuneAcc) %in% params]
    sort_args <- vector(mode = "list", length = length(nms))
    for (i in seq(along = nms)) {
      sort_args[[i]] <- tuneAcc[, nms[i]]
    }
    tune_ord <- do.call("order", sort_args)
    if (!is.null(tune_ord))
      tuneAcc <- NULL
    theDots <- list()
    theDots$x <- tuneAcc

    #rownames(printMat) <- rep("", dim(printMat)[1])
    if (showSD) {
      sdCols <- grep("SD$", colnames(printMat), value = TRUE)
      sd_dat <- NULL
      printMat <- NULL

    }

    cat("\n")
    if (!is.null(constString)) {
      cat(truncateText(paste(constString, collapse = "\n")))
      cat("\n")
    }

    if (is.null(x$update)) {
      met <- paste(x$metric, "was used to select the optimal model using")
      if (is.function(x$control$selectionFunction)) {
        met <- paste(met, " a custom selection rule.\n")
      }else {
        met <- paste(met, switch(x$control$selectionFunction,
                                 best = paste("the", ifelse(x$maximize,
                                                            "largest", "smallest"), "value.\n"),
                                 oneSE = " the one SE rule.\n", tolerance = " a tolerance rule.\n"))
      }
    }      else {
      met <- paste("The tuning", ifelse(ncol(x$bestTune) >
                                          1, "parameters", "parameter"),
                   "was set manually.\n")
    }
    cat(caret:::truncateText(met))

    cat(caret:::truncateText(optString))
    if (nzchar(optString))
      cat("\n")
  }

}

stringFunc <- function (x)  {
  if (!is.character(x)) x <- format(x)
  numElements <- length(x)
  out <- if (length(x) > 0) {
    switch(min(numElements, 3), x, paste(x, collapse = " and "), {
      x <- paste0(x, c(rep(",", numElements - 2), " and", ""))
      paste(x, collapse = " ")
    })
  } else ""
  out
}



#' Print Method for the train Class
#'
#' Print the results of a \code{\link{train}} object.
#'
#' The table of complexity parameters used, their resampled performance and a
#' flag for which rows are optimal.
#'
#' @param x an object of class \code{\link{train}}.
#' @param printCall a logical to print the call at the top of the output
#' @param details a logical to show print or summary methods for the final
#' model. In some cases (such as \code{gbm}, \code{knn}, \code{lvq}, naive
#' Bayes and bagged tree models), no information will be printed even if
#' \code{details = TRUE}
#' @param selectCol a logical whether to add a column with a star next to the
#' selected parameters
#' @param showSD a logical whether to show the standard deviation of the
#' resampling results within parentheses (e.g. "4.24 (0.493)")
#' @param \dots options passed to \code{\link[base]{format}}
#' @return A matrix with the complexity parameters and performance (invisibly).
#' @author Max Kuhn
#' @seealso \code{\link{train}}
#' @keywords print
#' @method print train
#' @export
#' @examples
#'
#' \dontrun{
#' data(iris)
#' TrainData <- iris[,1:4]
#' TrainClasses <- iris[,5]
#'
#' options(digits = 3)
#'
#' library(klaR)
#' rdaFit <- train(TrainData, TrainClasses, method = "rda",
#'                 control = trainControl(method = "cv"))
#' rdaFit
#' print(rdaFit, showSD = TRUE)
#' }
#'
#' @export print.train

"print.train" <-
  function(x,
           printCall = FALSE,
           details = FALSE,
           selectCol = FALSE,
           showSD = FALSE,
           ...) {

    if(!is.null(x$modelInfo$label)) cat(x$modelInfo$label, "\n\n")
    if(printCall) printCall(x$call)

    if(!is.null(x$trainingData)) {
      chDim <- dim(x$trainingData)
      chDim[2] <- chDim[2] - 1
      if(x$modelType == "Classification") {
        lev <- levels(x)
        if(is.character(lev)) chDim <- c(chDim, length(lev))
      } else lev <- NULL
      chDim <- format(chDim)
      cat(chDim[1], " samples", sep = "")
      if(!is.null(x$control$indexFinal))
        cat(",", length(x$control$indexFinal), "used for final model\n") else
          cat("\n")
      cat(chDim[2],
          " predictor", ifelse(chDim[2] > 1, "s\n", "\n"),
          sep = "")
      if(is.character(lev)){
        cat(chDim[3],
            "classes:",
            paste("'", lev, "'", sep = "", collapse = ", "),
            "\n")
      }
      cat("\n")
    }

    if(!is.null(x$preProc)){
      pp_list(x$preProc$method)
    } else {
      if(inherits(x, "train.recipe")) {
        step_names <- function(x) gsub("^step_", "", class(x)[1])
        steps_used <- unlist(lapply(x$recipe$steps, step_names))
        ppText <- paste("Recipe steps:", paste(steps_used, collapse = ", "))
        cat(truncateText(ppText), "\n")
      } else cat("No pre-processing\n")
    }

    if(!is.null(x$control$index)) {
      resampleN <- unlist(lapply(x$control$index, length))
      numResamp <- length(resampleN)

      resampText <- resampName(x)

      cat("Resampling:", resampText, "\n")
      if(x$control$method != "none") {
        outLabel <- x$metric

        resampleN <- as.character(resampleN)
        if(numResamp > 5) resampleN <- c(resampleN[1:6], "...")
        cat("Summary of sample sizes:", paste(resampleN, collapse = ", "), "\n")
      }
    }
    if(!is.null(x$control$sampling)) {
      cat("Addtional sampling using ")
      cat(switch(x$control$sampling$name,
                 down = "down-sampling",
                 up = "up-sampling",
                 smote = "SMOTE",
                 rose = "ROSE",
                 custom = "a custom function"))
      if(!is.null(x$preProc)) {
        if(x$control$sampling$first)
          cat(" prior to pre-processing") else
            cat(" after to pre-processing")
      }
      cat("\n\n")
    }

    if(x$control$method != "none") {

      tuneAcc <- x$results

      tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]

      cat("Resampling results")
      if(dim(tuneAcc)[1] > 1) cat(" across tuning parameters")
      if(showSD) cat(" (values below are 'mean (sd)')")
      cat(":\n\n")

      if(dim(tuneAcc)[1] > 1) {

        numParam <- length(x$bestTune)

        finalTune <- x$bestTune

        optValues <- paste(names(finalTune), "=", format(finalTune, ...))
        optString <- paste0("The final ",
                            ifelse(numParam > 1, "values", "value"),
                            " used for the model ",
                            ifelse(numParam > 1, "were ", "was "),
                            stringFunc(optValues),
                            ".")


        finalTune$Selected <- "*"

        ## See https://stat.ethz.ch/pipermail/r-help/2016-July/440230.html
        if(any(names(tuneAcc) %in% "method"))
          names(tuneAcc)[names(tuneAcc) %in% "method"] <- ".method"
        if(any(names(finalTune) %in% "method"))
          names(finalTune)[names(finalTune) %in% "method"] <- ".method"

        tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)

        if(any(names(tuneAcc) %in% ".method"))
          names(tuneAcc)[names(tuneAcc) %in% ".method"] <- "method"

        tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""

      } else optString <- ""

      sdCols <- grep("SD$", colnames(tuneAcc))
      if(showSD) {
        sdCheck <- unlist(lapply(tuneAcc[, sdCols, drop = FALSE],
                                 function(u) all(is.na(u))))
        if(any(sdCheck)) {
          rmCols <- names(sdCheck)[sdCheck]
          tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% rmCols)]
        }
      } else {
        if(length(sdCols) > 0) tuneAcc <- tuneAcc[, -sdCols, drop = FALSE]
      }

      params <- names(x$bestTune)

      if(!all(params == "parameter")){
        numVals <- apply(tuneAcc[, params, drop = FALSE], 2, function(x) length(unique(x)))
        if(any(numVals < 2)) {
          constString <- NULL
          for(i in seq(along = numVals)) {
            if(numVals[i] == 1)
              constString <- c(constString,
                               paste0("Tuning parameter '",
                                      names(numVals)[i],
                                      "' was held constant at a value of ",
                                      stringFunc(tuneAcc[1,names(numVals)[i]])))
          }
          discard <- names(numVals)[which(numVals == 1)]
          tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% discard), drop = FALSE]

        } else constString <- NULL
      } else constString <- NULL

      tuneAcc <- tuneAcc[,!grepl("Apparent$|Optimism$", names(tuneAcc)), drop = FALSE]
      colnames(tuneAcc)[colnames(tuneAcc) == ".B"] <- "Resamples"
      nms <- names(tuneAcc)[names(tuneAcc) %in% params]
      sort_args <- vector(mode = "list", length = length(nms))
      for(i in seq(along = nms)) {
        sort_args[[i]] <- tuneAcc[, nms[i]]
      }
      tune_ord <- do.call("order", sort_args)
      if(!is.null(tune_ord)) tuneAcc <- tuneAcc[tune_ord,,drop = FALSE]

      theDots <- list(...)
      theDots$x <- tuneAcc
      #       if(!(any(names(theDots) == "digits"))) theDots$digits <- min(3, getOption("digits"))
      printMat <- do.call("format.data.frame", theDots)
      printMat <- as.matrix(printMat)
      rownames(printMat) <- rep("", dim(printMat)[1])

      if(showSD){
        sdCols <- grep("SD$", colnames(printMat), value = TRUE)
        sd_dat <- printMat[, sdCols, drop = FALSE]
        printMat <- printMat[, !(colnames(printMat) %in% sdCols), drop = FALSE]
        for(col_name in sdCols) {
          not_sd <- gsub("SD$", "", col_name)
          if(any(colnames(printMat) == not_sd)) {
            printMat[, not_sd] <- paste0(printMat[, not_sd], " (",
                                         sd_dat[, col_name], ")")
          }
        }
      }
      if(!selectCol) printMat <- printMat[, colnames(printMat) != "Selected", drop = FALSE]

      print(printMat, quote = FALSE, print.gap = 2)

      cat("\n")

      if(!is.null(constString)){
        cat(truncateText(paste(constString, collapse = "\n")))
        cat("\n")
      }


      if(dim(tuneAcc)[1] > 1) {
        if(is.null(x$update)) {
          met <- paste(x$metric, "was used to select the optimal model using")
          if(is.function(x$control$selectionFunction)) {
            met <- paste(met, " a custom selection rule.\n")
          } else {

            met <- paste(met,
                         switch(x$control$selectionFunction,
                                best = paste(
                                  "the",
                                  ifelse(x$maximize, "largest", "smallest"),
                                  "value.\n"),
                                oneSE = " the one SE rule.\n",
                                tolerance = " a tolerance rule.\n"))
          }
        } else {
          met <- paste("The tuning", ifelse(ncol(x$bestTune) > 1, "parameters", "parameter"),
                       "was set manually.\n")

        }
        cat(truncateText(met))
      }

      cat(truncateText(optString))
      if(nzchar(optString)) cat("\n")
    } else printMat <- NULL

    if(details) {
      if(!(x$method %in% c("gbm", "treebag", "nb", "lvq", "knn"))) {
        cat("\n----------------------------------------------------------\n")
        cat("\nThe final model:\n\n")
        switch(x$method,
               lm =, nnet =, multinom =, pls =, earth =,
               lmStepAIC =,
               bagEarth =, bagFDA = print(summary(x$finalModel)),
               rpart =, ctree =, ctree2=, cforest =,
               glmboost =, gamboost =, blackboost =,
               ada =, randomForest =, pcaNNet =,
               svmradial =, svmpoly =,
               svmRadial =, svmPoly =,
               rvmRadial =, rvmPoly =,
               lssvmRadial =, lssvmPoly =,
               gaussprRadial =, gaussprPoly =,
               enet =, lasso =, LMT =, JRip =,
               lda =, rda =, pamr =, gpls =, J48 =,
               ppr = print(x$finalModel),
               fda =  {
                 print(x$finalModel)
                 cat("\n Summary of Terms\n\n")
                 print(x$finalModel$fit)

               })
      }
    }
    invisible(printMat)
  }


truncateText <- function(x){
  if(length(x) > 1) x <- paste(x, collapse = "")
  w <- options("width")$width
  if(nchar(x) <= w) return(x)

  cont <- TRUE
  out <- x
  while(cont){
    tmp <- out[length(out)]
    tmp2 <- substring(tmp, 1, w)

    spaceIndex <- gregexpr("[[:space:]]", tmp2)[[1]]
    stopIndex <- spaceIndex[length(spaceIndex) - 1] - 1
    tmp <- c(substring(tmp2, 1, stopIndex),
             substring(tmp, stopIndex + 1))
    out <- if(length(out) == 1) tmp else c(out[1:(length(x)-1)], tmp)
    if(all(nchar(out) <= w)) cont <- FALSE
  }

  paste(out, collapse = "\n")
}


pp_list <- function(x) {
  if(is.list(x)) {
    pp <- unlist(lapply(x, length))
    pp <- pp[pp > 0]
    if(length(pp) > 0) {
      names(pp) <- gsub("BoxCox", "Box-Cox transformation", names(pp))
      names(pp) <- gsub("YeoJohnson", "Yeo-Johnson transformation", names(pp))
      names(pp) <- gsub("expoTrans", "exponential transformation", names(pp))
      names(pp) <- gsub("scale", "scaled", names(pp))
      names(pp) <- gsub("center", "centered", names(pp))
      names(pp) <- gsub("pca", "principal component signal extraction", names(pp))
      names(pp) <- gsub("ica", "independent component signal extraction", names(pp))
      names(pp) <- gsub("spatialSign", "spatial sign transformation", names(pp))
      names(pp) <- gsub("knnImpute", "nearest neighbor imputation", names(pp))
      names(pp) <- gsub("bagImpute", "bagged tree imputation", names(pp))
      names(pp) <- gsub("medianImpute", "median imputation", names(pp))
      names(pp) <- gsub("range", "re-scaling to [0, 1]", names(pp))
    } else pp <- "None"
    ppText <- paste("Pre-processing:", paste0(names(pp),  " (", pp, ")", collapse = ", "))
    cat(truncateText(ppText), "\n")
  } else {
    pp <- x
    pp <- gsub("BoxCox", "Box-Cox transformation", pp)
    pp <- gsub("YeoJohnson", "Yeo-Johnson transformation", pp)
    pp <- gsub("expoTrans", "exponential transformation", pp)
    pp <- gsub("scale", "scaled", pp)
    pp <- gsub("center", "centered", pp)
    pp <- gsub("pca", "principal component signal extraction", pp)
    pp <- gsub("ica", "independent component signal extraction", pp)
    pp <- gsub("spatialSign", "spatial sign transformation", pp)
    pp <- gsub("knnImpute", "nearest neighbor imputation", pp)
    pp <- gsub("bagImpute", "bagged tree imputation", pp)
    pp <- gsub("medianImpute", "median imputation", pp)
    pp <- gsub("range", "re-scaling to [0, 1]", pp)

    if(length(pp) == 0) pp <- "None"

    ppText <- paste("Pre-processing:", paste(pp, collapse = ", "))
    cat(truncateText(ppText), "\n")
  }
  invisible(NULL)
}


resampName<-function (x, numbers = TRUE)
{
  if (!("control" %in% names(x)))
    return("")
  if (numbers) {
    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)
    out <- switch(tolower(x$control$method), none = "None",
                  apparent = "Apparent", custom = paste("Custom Resampling (",
                                                        numResamp, " reps)", sep = ""), timeslice = paste("Rolling Forecasting Origin Resampling (",
                                                                                                          x$control$horizon, " held-out with", ifelse(x$control$fixedWindow,
                                                                                                                                                      " a ", " no "), "fixed window)",
                                                                                                          sep = ""), oob = "Out of Bag Resampling",
                  boot = , optimism_boot = , boot_all = , boot632 = paste("Bootstrapped (",
                                                                          numResamp, " reps)", sep = ""), cv = paste("Cross-Validated (",
                                                                                                                     x$control$number, " fold)", sep = ""),
                  repeatedcv = paste("Cross-Validated (", x$control$number,
                                     " fold, repeated ", x$control$repeats,
                                     " times)", sep = ""), lgocv = paste("Repeated Train/Test Splits Estimated (",
                                                                         numResamp, " reps, ", round(x$control$p *
                                                                                                       100, 1), "%)", sep = ""), loocv = "Leave-One-Out Cross-Validation",
                  adaptive_boot = paste("Adaptively Bootstrapped (",
                                        numResamp, " reps)", sep = ""), adaptive_cv = paste("Adaptively Cross-Validated (",
                                                                                            x$control$number, " fold, repeated ", x$control$repeats,
                                                                                            " times)", sep = ""), adaptive_lgocv = paste("Adaptive Repeated Train/Test Splits Estimated (",
                                                                                                                                         numResamp, " reps, ", round(x$control$p,
                                                                                                                                                                     2), "%)", sep = ""))
  }
  else {
    out <- switch(tolower(x$control$method), none = "None",
                  apparent = "(Apparent)", custom = "Custom Resampling",
                  timeslice = "Rolling Forecasting Origin Resampling",
                  oob = "Out of Bag Resampling", boot = "(Bootstrap)",
                  optimism_boot = "(Optimism Bootstrap)", boot_all = "(Bootstrap All)",
                  boot632 = "(Bootstrap 632 Rule)", cv = "(Cross-Validation)",
                  repeatedcv = "(Repeated Cross-Validation)",
                  loocv = "Leave-One-Out Cross-Validation", lgocv = "(Repeated Train/Test Splits)")
  }
  out
}
