#' @export
predgbm<-function (modelFit, newdata, n.trees = 1)
{
  out <- predict(modelFit, newdata, type = "response", n.trees =n.trees, single.tree=T)
  out[is.nan(out)] <- NA
  out <- switch(modelFit$distribution$name, multinomial = {
    colnames(out[, , 1, drop = FALSE])[apply(out[, , 1, drop = FALSE],
                                             1, which.max)]
  }, bernoulli = , adaboost = , huberized = {
    ifelse(out >= 0.5, modelFit$obsLevels[1], modelFit$obsLevels[2])
  }, gaussian = , laplace = , tdist = , poisson = , quantile = {
    out
  })

  out
}
