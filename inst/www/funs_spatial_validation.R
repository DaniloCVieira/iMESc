

.check_classes<-function (clen, column, th = 15)
{
  if (clen > th) {
    warning(sprintf(paste("The are too many unique values in '%s'.",
                          "Use 'column' only for binary or categorical responses (ignore this if it is).\n"),
                    column))
  }
}
.fold_assign<-function (blocks, n, checkerboard = FALSE)
{
  old <- options("digits")
  options(digits = 22)
  on.exit(options(old))
  cent <- sf::st_centroid(blocks)
  xy <- as.data.frame(sf::st_coordinates(cent))
  xy$X <- as.factor(xy$X)
  xy$Y <- as.factor(xy$Y)
  ylev <- levels(xy$Y)
  ny <- length(ylev)
  len <- nrow(xy)
  xy$ids <- seq_len(len)
  xy <- xy[order(xy$Y), ]
  xy$z <- 0
  if (checkerboard) {
    for (i in rev(seq_len(ny))) {
      wyi <- which(xy$Y == ylev[i])
      nx <- length(wyi)
      if (i%%2) {
        xy$z[wyi] <- rep(1:2, length.out = nx)
      }      else {
        xy$z[wyi] <- rep(2:1, length.out = nx)
      }
    }
  }  else {
    xy$z <- rep(1:n, length.out = len)
  }
  blocks <- sf::st_sf(blocks)
  blocks$block_id <- 1:nrow(blocks)
  xy <- xy[order(xy$ids), ]
  blocks$folds <- xy$z
  return(blocks)
}
.check_column<-function (column, x)
{
  if (!is.null(column)) {
    if (!column %in% colnames(x)) {
      warning(sprintf("There is no column named '%s' in 'x'. Column is ignored!\n",
                      column))
      column <- NULL
    }
  }
  return(column)
}
.check_x<-function (x, name = "x")
{
  if (!methods::is(x, "sf")) {
    tryCatch({
      x <- sf::st_as_sf(x)
    }, error = function(cond) {
      message(sprintf("'%s' is not convertible to an sf object!",
                      name))
      message(sprintf("'%s' must be an sf or spatial* object.",
                      name))
    })
  }
  return(x)
}


.check_pkgs<-function (pkg)
{
  pkgna <- names(which(sapply(sapply(pkg, find.package, quiet = TRUE),
                              length) == 0))
  if (length(pkgna) > 0) {
    nm <- paste(pkgna, collapse = ", ")
    message("This function requires these packages: ", nm,
            "\nWould you like to install them now?\n1: yes\n2: no")
    user <- readline(prompt = paste0("Selection: "))
    if (tolower(user) %in% c("1", "yes", "y")) {
      utils::install.packages(pkgna)
    }    else {
      stop("Please install these packages for function to work: ",
           nm)
    }
  }
}

spatialBlock2<-function (speciesData, species = NULL, rasterLayer = NULL, theRange = NULL,
                         rows = NULL, cols = NULL, k = 5L, selection = "random", iteration = 100L,
                         blocks = NULL, foldsCol = NULL, numLimit = 0L, maskBySpecies = TRUE,
                         degMetre = 111325, border = NULL, showBlocks = TRUE, biomod2Format = TRUE,
                         xOffset = 0, yOffset = 0, extend = 0, seed = 42, progress = TRUE,
                         verbose = F) {
  message("This function is deprecated! Please use 'cv_spatial' instead.")
  speciesData <- .check_x(speciesData, name = "speciesData")
  .check_pkgs("sf")
  if (!is.null(species)) {
    if (!species %in% colnames(speciesData)) {
      warning(sprintf("There is no column named '%s' in 'speciesData'.\n",
                      species))
      species <- NULL
    }
  }
  if (selection == "predefined") {
    if (is.null(foldsCol) || is.null(blocks)) {
      stop("The 'blocks' and 'foldsCol' should be specified for 'predefined' selection")
    }
    if (!foldsCol %in% colnames(blocks)) {
      stop(sprintf("There is no column named '%s' in 'blocks'.\n",
                   foldsCol))
    }
    if (!is.numeric(blocks[, foldsCol, drop = TRUE])) {
      stop("The fold numbers in 'foldsCol' must be integer numbers.")
    }
  }
  if (!is.null(rasterLayer)) {
    rasterLayer <- .check_r(rasterLayer, name = "rasterLayer")
  }
  out <- cv_spatial2(x = speciesData, column = species, r = rasterLayer,
                     k = k, hexagon = FALSE, flat_top = FALSE, size = theRange,
                     rows_cols = c(rows, cols), selection = selection, iteration = iteration,
                     user_blocks = blocks, folds_column = foldsCol, deg_to_metre = degMetre,
                     biomod2 = biomod2Format, offset = c(xOffset, yOffset),
                     extend = extend, seed = seed, progress = progress, report = F,
                     plot = showBlocks)
  theList <- list(folds = out$folds_list, foldID = out$folds_ids,
                  biomodTable = out$biomod_table, k = k, blocks = out$blocks,
                  species = out$column, range = out$size, plots = NULL,
                  records = out$records)
  class(theList) <- c("SpatialBlock")
  return(theList)
}



cv_spatial2<-function (x, column = NULL, r = NULL, k = 5L, hexagon = TRUE,
                       flat_top = FALSE, size = NULL, rows_cols = c(10, 10), selection = "random",
                       iteration = 100L, user_blocks = NULL, folds_column = NULL,
                       deg_to_metre = 111325, biomod2 = TRUE, offset = c(0, 0),
                       extend = 0, seed = NULL, progress = TRUE, report = F,
                       plot = TRUE, ...) {
  if (plot)
    .check_pkgs(c("ggplot2"))
  selection <- match.arg(selection, choices = c("random", "systematic",
                                                "checkerboard", "predefined"))
  x <- .check_x(x)
  column <- .check_column(column, x)
  if (!is.null(user_blocks)) {
    user_blocks <- .check_x(user_blocks, name = "user_blocks")
    if (selection == "checkerboard") {
      warning("The checkerboard selection cannot be used with 'user_blocks`.\nThe random selection is used!")
      selection <- "random"
    }
  }
  if (selection == "predefined") {
    if (is.null(folds_column) || is.null(user_blocks)) {
      stop("The 'user_blocks' and 'folds_column' should be specified for 'predefined' selection")
    }
    if (!folds_column %in% colnames(user_blocks)) {
      stop(sprintf("There is no column named '%s' in 'user_blocks'.\n",
                   folds_column))
    }
    if (!is.numeric(user_blocks[, folds_column, drop = TRUE])) {
      stop("The fold numbers in 'folds_column' must be integer numbers.")
    }
  }
  if (!is.null(r)) {
    r <- .check_r(r)
    r <- r[[1]]
    .check_within(x, r)
  }
  if (hexagon && selection %in% c("checkerboard", "predefined")) {
    selection <- "random"
    message("Hexagon blocks can only be used with random or systematic selections!\nThe random selection is used.")
  }
  if (selection == "checkerboard")
    k <- 2
  tryCatch({
    extend <- abs(extend)
    extend <- min(5, extend)/100
  }, error = function(cond) {
    message("'extend' must be a numeric value between 0 and 5.")
  })
  tryCatch({
    iteration <- abs(as.integer(iteration))
    iteration <- max(1, iteration)
  }, error = function(cond) {
    message("'iteration' must be a natural number.")
  })
  if (selection != "random")
    progress <- FALSE
  if (iteration < 3)
    progress <- FALSE
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = iteration,
                                style = 3)
  }
  if (is.null(user_blocks)) {
    blocks <- .make_blocks2(x_obj = if (is.null(r)) {
      x}      else {r}, blocksize = size, blockcols = rows_cols[2], blockrows = rows_cols[1],
      hexagonal = hexagon, flat_top = flat_top, extend_perc = extend,
      degree = deg_to_metre, xy_offset = offset, checkerboard = ifelse(selection ==
                                                                         "checkerboard", TRUE, FALSE))
  }  else {
    blocks <- if (methods::is(user_blocks, "sfc"))
      sf::st_sf(user_blocks)
    else user_blocks
  }
  sub_blocks <- blocks[x, ]
  blocks_len <- nrow(sub_blocks)
  tryCatch({
    k <- abs(as.integer(k))
  }, error = function(cond) {
    message("'k' must be a natural number.")
  })
  if (k > blocks_len) {
    stop("'k' is bigger than the number of spatial blocks: ",
         blocks_len, ".\n")
  }  else if (k < 2) {
    stop("'k' must be a natural number equal or higher than 2.")
  }
  blocks_df <- as.data.frame(sf::st_intersects(sf::st_geometry(x),
                                               sf::st_geometry(sub_blocks)))
  names(blocks_df) <- c("records", "block_id")
  if (nrow(blocks_df) > nrow(x)) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    blocks_df <- blocks_df[sample(nrow(blocks_df)), ]
    blocks_df <- blocks_df[!duplicated(blocks_df$records),
    ]
  }  else if (nrow(blocks_df) < nrow(x) || anyNA(blocks_df)) {
    nonoverlap <- nrow(x) - nrow(blocks_df)
    warning("At least ", nonoverlap, " of the points are not within the defined spatial blocks!\n")
    message("Consider using the 'extend' parameter to ensure points are covered by blocks e.g. extend = 0.5.")
  }
  if (is.null(column)) {
    train_test_table <- data.frame(train = rep(0, k), test = 0)
  }  else {
    cl <- sort(unique(x[, column, drop = TRUE]))
    clen <- length(cl)
    .check_classes(clen, column)
    train_test_table <- as.data.frame(matrix(0, nrow = k,
                                             ncol = clen * 2))
    names(train_test_table) <- c(paste("train", cl, sep = "_"),
                                 paste("test", cl, sep = "_"))
  }
  biomod_table <- data.frame(RUN1 = rep(TRUE, nrow(blocks_df)))
  min_num <- 0
  max_sd <- Inf
  if (!is.null(seed)) {
    set.seed(seed)
  }
  for (i in seq_len(iteration)) {
    if (selection == "random") {
      blocks_df <- blocks_df[, c("records", "block_id")]
      fold_df <- data.frame(block_id = seq_len(blocks_len),
                            folds = 0)
      num <- floor(blocks_len/k)
      fold_df$folds[seq_len(num * k)] <- sample(rep(seq_len(k),
                                                    num), num * k)
      if (blocks_len%%k != 0) {
        rest <- blocks_len%%k
        unfold <- which(fold_df$folds == 0)
        fold_df$folds[unfold] <- sample(seq_len(k), rest,
                                        replace = FALSE)
      }
    }    else if (selection == "systematic") {
      if (hexagon) {
        sub_blocks <- .fold_assign(sf::st_geometry(sub_blocks),
                                   n = k)
      }      else {
        sub_blocks$block_id <- seq_len(blocks_len)
        sub_blocks$folds <- rep(1:k, length.out = blocks_len)
      }
      fold_df <- sf::st_drop_geometry(sub_blocks)
    }    else if (selection == "checkerboard") {
      sub_blocks$folds <- sub_blocks$id
      sub_blocks$block_id <- seq_len(blocks_len)
      fold_df <- sf::st_drop_geometry(sub_blocks)
    }    else if (selection == "predefined") {
      fold_df <- data.frame(block_id = seq_len(blocks_len),
                            folds = sub_blocks[, folds_column, drop = TRUE])
    }
    blocks_df <- merge(x = blocks_df, y = fold_df, by = "block_id",
                       all.x = TRUE)
    train_test_table[] <- 0
    fold_list <- list()
    fold_vect <- rep(NA, nrow(blocks_df))
    for (p in seq_len(k)) {
      train_set <- blocks_df$records[which(blocks_df$folds !=
                                             p)]
      test_set <- blocks_df$records[which(blocks_df$folds ==
                                            p)]
      fold_vect[test_set] <- p
      fold_list[[p]] <- assign(paste0("fold", p), list(train_set,
                                                       test_set))
      if (is.null(column)) {
        train_test_table$train[p] <- length(train_set)
        train_test_table$test[p] <- length(test_set)
      }      else {
        countrain <- table(x[train_set, column, drop = TRUE])
        countest <- table(x[test_set, column, drop = TRUE])
        train_test_table[p, which(cl %in% names(countrain))] <- countrain
        train_test_table[p, clen + which(cl %in% names(countest))] <- countest
      }
      if (biomod2) {
        colm <- paste0("RUN", p)
        biomod_table[, colm] <- FALSE
        biomod_table[train_set, colm] <- TRUE
      }
    }
    if (selection == "random") {
      if (min(train_test_table) >= min_num && stats::sd(unlist(train_test_table)) <
          max_sd) {
        train_test_table2 <- train_test_table
        min_num <- min(train_test_table2)
        max_sd <- stats::sd(unlist(train_test_table))
        blocks_df2 <- blocks_df
        fold_list2 <- fold_list
        fold_vect2 <- fold_vect
        biomod_table2 <- biomod_table
      }
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }    else {
      break
    }
  }
  if (selection == "random") {
    sub_blocks$block_id <- seq_len(nrow(sub_blocks))
    blocks_df_filter <- blocks_df2[, c("block_id", "folds")]
    blocks_df_filter <- blocks_df_filter[!duplicated(blocks_df_filter),
    ]
    sub_blocks <- merge(x = sub_blocks, y = blocks_df_filter,
                        by = "block_id", all.x = TRUE)
    blocks_df <- blocks_df2
    train_test_table <- train_test_table2
    fold_list <- fold_list2
    fold_vect <- fold_vect2
    biomod_table <- biomod_table2
  }
  if (report) {
    cat("\n")
    print(train_test_table)
  }
  if (any(train_test_table < 1)) {
    zerofolds <- which(apply(train_test_table, 1, function(x) any(x <
                                                                    1)))
    if (length(zerofolds) > 1) {
      warning("Folds ", paste(zerofolds, collapse = ", "),
              " have class(es) with zero records")
    }    else {
      warning("Fold ", zerofolds, " has class(es) with zero records")
    }
  }
  if (is.null(user_blocks)) {
    sub_blocks <- sub_blocks[stats::complete.cases(sub_blocks$folds),
    ]
  }
  final_objs <- list(folds_list = fold_list, folds_ids = fold_vect,
                     biomod_table = if (biomod2) as.matrix(biomod_table) else NULL,
                     k = k, size = size, column = column, blocks = sub_blocks,
                     records = train_test_table)
  class(final_objs) <- c("cv_spatial")

  return(final_objs)
}



.make_blocks2<-function (x_obj, blocksize = NULL, blockcols = NULL, blockrows = NULL,
                         hexagonal = FALSE, flat_top = FALSE, extend_perc = 0.005,
                         degree = 111325, xy_offset = c(0, 0), checkerboard = FALSE)
{
  if (all(sapply(list(blocksize, blockcols, blockrows), is.null))) {
    stop("Size or the number of rows/columns should be defined for making spatial blocks.")
  }
  mapext <- terra::ext(x_obj)[1:4]
  if (is.null(blocksize)) {
    xy_offset <- c(0, 0)
  }  else {
    if (hexagonal) {
      if (!xy_offset[1] %in% 0:1)
        xy_offset[1] <- 1 - xy_offset[1]
      if (!xy_offset[2] %in% 0:1)
        xy_offset[2] <- 1 - xy_offset[2]
    }
    tryCatch({
      xy_offset <- blocksize * (abs(xy_offset)%%1)
    }, error = function(cond) {
      message("Offsets should be numeric values between 0 and 1. For any higher values, only the decimal part is used.")
    })
    if (length(xy_offset) < 2)
      xy_offset[2] <- 0
    if (is.na(sf::st_crs(x_obj))) {
      if (all(mapext >= -180) && all(mapext <= 180)) {
        blocksize <- blocksize/degree
        warning("The input layer has no CRS defined. Based on the extent of the input map it is assumed to have an un-projected reference system.")
      }
    }    else {
      if (sf::st_is_longlat(x_obj)) {
        blocksize <- blocksize/degree
      }
    }
  }
  if (hexagonal) {
    xm <- as.numeric(sf::st_bbox(x_obj)[1])
    ym <- as.numeric(sf::st_bbox(x_obj)[2])
    xoff <- xm - xy_offset[1]
    yoff <- ym - xy_offset[2]
    hexsize <- ifelse(is.null(blocksize), diff(sf::st_bbox(x_obj)[c(1,
                                                                    3)])/blockrows, blocksize)
    tryCatch({
      fishnet_poly <- sf::st_make_grid(x_obj, cellsize = hexsize,
                                       offset = c(xoff, yoff), square = FALSE, what = "polygons",
                                       flat_topped = flat_top)
    }, error = function(cond) {
      message("Could not create spatial blocks! possibly because of using a very small block size.")
      message("Remember, size is in metres not the unit of the CRS.")
    })
    fishnet_poly <- sf::st_sf(fishnet_poly)
    sf::st_geometry(fishnet_poly) <- "geometry"
    fishnet_poly$id <- seq_len(nrow(fishnet_poly))
  }  else {
    ref_ext <- mapext
    xrange <- mapext["xmax"] - mapext["xmin"]
    yrange <- mapext["ymax"] - mapext["ymin"]
    mapext["xmin"] <- mapext["xmin"] - (xrange * extend_perc)
    mapext["xmax"] <- mapext["xmax"] + (xrange * extend_perc)
    mapext["ymin"] <- mapext["ymin"] - (yrange * extend_perc)
    mapext["ymax"] <- mapext["ymax"] + (yrange * extend_perc)
    if (!is.null(blocksize)) {
      xPix <- ceiling(xrange/blocksize)
      yPix <- ceiling(yrange/blocksize)
      xdif <- ((xPix * blocksize) - xrange)/2
      ydif <- ((yPix * blocksize) - yrange)/2
      mapext["xmin"] <- mapext["xmin"] - xdif + xy_offset[1]
      mapext["xmax"] <- mapext["xmax"] + xdif + xy_offset[1]
      mapext["ymin"] <- mapext["ymin"] - ydif + xy_offset[2]
      mapext["ymax"] <- mapext["ymax"] + ydif + xy_offset[2]
      if (mapext["xmin"] > ref_ext["xmin"]) {
        mapext["xmin"] <- mapext["xmin"] - blocksize
        xPix <- xPix + 1
      }
      if (mapext["ymin"] > ref_ext["ymin"]) {
        mapext["ymin"] <- mapext["ymin"] - blocksize
        yPix <- yPix + 1
      }
      blockcols <- xPix
      blockrows <- yPix
    }
    fishnet <- terra::rast(terra::ext(mapext), nrows = max(1,
                                                           blockrows, na.rm = TRUE), ncols = max(1, blockcols,
                                                                                                 na.rm = TRUE), crs = terra::crs(x_obj))
    terra::values(fishnet) <- seq_len(terra::ncell(fishnet))
    if (checkerboard) {
      net_rows <- seq_len(terra::nrow(fishnet))
      net_cols <- seq_len(terra::ncol(fishnet))
      net_ncol <- terra::ncol(fishnet)
      for (i in net_rows) {
        row_cells <- terra::cellFromRowCol(fishnet, row = i,
                                           col = net_cols)
        if (i%%2 == 0) {
          fishnet[row_cells] <- rep(1:2, length.out = net_ncol)
        }        else {
          fishnet[row_cells] <- rep(2:1, length.out = net_ncol)
        }
      }
    }
    fishnet_poly <- terra::as.polygons(fishnet, dissolve = FALSE)
    names(fishnet_poly) <- "id"
    fishnet_poly <- sf::st_as_sf(fishnet_poly)
  }
  return(fishnet_poly)
}








guess_size <- function(sf_points, method = "extent",p_test=0.2, kmax=200) {

  k_info <- choose_k(p_test, nrow(sf_points), kmax = kmax)
  k <- k_info$k

  bbox <- sf::st_bbox(sf_points)
  xrange <- bbox$xmax - bbox$xmin
  yrange <- bbox$ymax - bbox$ymin

  if (method == "extent") {
    # divide a menor dimensão pela raiz do k → blocos suficientes
    size <- min(xrange, yrange) / sqrt(k)
  } else if (method == "density") {
    dmat <- sf::st_distance(sf_points)
    mean_nn <- mean(apply(dmat, 1, function(x) sort(x)[2])) # vizinho mais próximo
    size <- mean_nn * 3  # por ex., 3x a distância média
  }

  return(round(size))
}




make_spatial_train_test <- function(sf_points,
                                    response = "y",   # coluna da variável resposta
                                    range = 2000,
                                    p_test = 0.3,
                                    selection = "random",
                                    seed = 123,
                                    verbose = FALSE,
                                    kmax = 100) {
  # Verificar se a coluna existe
  if (!response %in% names(sf_points)) {
    stop(sprintf("A coluna '%s' não existe em sf_points.", response))
  }

  # Determinar k automaticamente
  k_info <- choose_k(p_test, nrow(sf_points), kmax = kmax)
  k <- k_info$k

  # Controle do seed
  if (is.na(seed)) {
    seed <- NULL
  }

  # Rodar cv_spatial2, informando a coluna resposta
  folds <- cv_spatial2(x = sf_points,
                       column = response,
                       size = range,
                       k = k,
                       selection = selection,
                       plot = FALSE,
                       progress = FALSE,
                       seed = seed)

  # Selecionar 1 fold aleatório para ser o teste
  test_fold <- sample(unique(folds$folds_ids), 1)

  test_idx <- folds$folds_ids == test_fold
  train_idx <- !test_idx

  if (verbose) {
    cat("p_test desejado:", p_test,
        "| k escolhido:", k,
        "| prop. real esperada:", round(k_info$prop_real, 3),
        "| prop. obtida:", round(mean(test_idx), 3), "\n")
  }

  split <- ifelse(folds$folds_ids == test_fold, "test", "training")

  return(list(folds=folds,
              split=split))
}



check_leakage <- function(coords, split, k = 1, epsilon = NULL) {
  library(FNN)

  train <- coords[split == "Training", ]
  test  <- coords[split == "Test", ]

  nn <- get.knnx(train, test, k = k)
  dmin <- nn$nn.dist[,1]

  # se não definir epsilon → usa distância média ao vizinho mais próximo do treino
  if (is.null(epsilon)) {
    nn_train <- get.knn(train, k = 2)
    epsilon <- mean(nn_train$nn.dist[,2]) # média vizinho mais próximo
  }

  leakage_rate <- mean(dmin < epsilon)

  return(list(
    epsilon = epsilon,
    leakage_rate = leakage_rate,
    leakage_pct = round(100 * leakage_rate, 2),
    dmin_summary = summary(dmin)
  ))
}
choose_k <- function(p, n, kmax = 100) {
  ks <- 2:kmax
  # tamanho do fold em número inteiro de observações
  fold_sizes <- round(n / ks)
  props_real <- fold_sizes / n
  diffs <- abs(props_real - p)

  k_best <- ks[which.min(diffs)]

  list(k = k_best,
       fold_size = fold_sizes[which.min(diffs)],
       prop_real = props_real[which.min(diffs)],
       diff = min(diffs))
}


cv_spatial_autocor2<-function (r, x, column = NULL, num_sample = 5000L, deg_to_metre = 111325,
                               plot = TRUE, progress = TRUE, ...) {

  if (!missing(x)) {
    x <- .check_x(x)
  }
  if (!missing(x) && is.null(column)) {
    stop("When 'x' is provided, 'column' must also be provided. Otherwise, provide only 'r'.")
  }
  if (!missing(x) && !is.null(column)) {
    if (!all(column %in% colnames(x))) {
      wc <- which(!column %in% colnames(x))
      stop(sprintf("There is no column named '%s' in 'x'.\n",
                   column[wc]))
    }
  }
  if (!missing(r)) {
    r <- .check_r(r)
  }

  if (!missing(x)) {
    if (is.na(sf::st_crs(x))) {
      stop("The coordinate reference system of 'x' must be defined.")
    }
    nlayer <- length(column)
  }
  if (nlayer < 2)
    progress <- FALSE
  if (progress)
    pb <- utils::txtProgressBar(min = 0, max = nlayer, style = 3)
  vario_list <- lapply(seq_len(nlayer), .fit_variogram, rr = if (missing(x)) {
    r
  }    else {NULL}, xx = if (!missing(x)) {
    x
  }     else {NULL}, column = column, num_sample = num_sample, progress = progress,
  pb = if (progress) {pb}   else {NULL})
  vario_data <- data.frame(layers = seq_len(nlayer), range = 1,
                           sill = 1)
  for (v in seq_along(vario_list)) {
    vario_data$layers[v] <- if (missing(x)) {
      names(r)[v]}  else {column[v]}
    vario_data$range[v] <- vario_list[[v]]$var_model[2,
                                                     3]
    vario_data$sill[v] <- vario_list[[v]]$var_model[2, 2]
  }
  vario_data <- vario_data[order(vario_data$range), ]
  x_obj <- if (missing(x)) {
    sf::st_as_sf(samp_point)}  else {x}
  size <- the_range <- stats::median(vario_data$range)
  if (sf::st_is_longlat(x_obj)) {
    vario_data$range <- vario_data$range * 1000
    the_range <- the_range * 1000
    size <- the_range/deg_to_metre
  }
  vis_block <- sf::st_make_grid(x_obj, cellsize = round(size),
                                what = "polygons")
  vis_block <- sf::st_sf(vis_block[x_obj])
  vis_block$folds <- 1:nrow(vis_block)
  plot_data <- list(blocks = vis_block)
  class(plot_data) <- "cv_spatial"
  num_sample <- ifelse(missing(x), num_sample, nrow(x))



  final_list <- list(range = the_range, range_table = vario_data,
                     num_sample = num_sample, variograms = vario_list)
  class(final_list) <- c("cv_spatial_autocor")
  return(final_list)
}




evaluate_fold_classes <- function(sf_dat, resampling, column = "y") {

  response <- sf_dat[[column]]
  response <- as.factor(response)

  test_table <- table(resampling$folds_ids, response)

  total_by_class <- colSums(test_table)

  train_table <- sweep(
    x = test_table,
    MARGIN = 2,
    STATS = total_by_class,
    FUN = function(test, total) total - test
  )

  data.frame(
    fold = rownames(test_table),
    n_test = rowSums(test_table),
    n_train = rowSums(train_table),
    n_classes_test = rowSums(test_table > 0),
    n_classes_train = rowSums(train_table > 0),
    n_classes_absent_test = rowSums(test_table == 0),
    n_classes_absent_train = rowSums(train_table == 0),
    min_n_test_class = apply(test_table, 1, min),
    min_n_train_class = apply(train_table, 1, min),
    stringsAsFactors = FALSE
  )
}


.is_loo2<-function (x) {
  inherits(x, c("cv_buffer", "cv_nndm"))
}


.x_to_long2<-function (x, cv, num_plot = 1:10) {
  folds_list <- cv$folds_list
  tryCatch({
    num_plot <- abs(as.integer(num_plot))
    num_plot <- sort(num_plot)
  }, error = function(cond) {
    message("'num_plot' must be natural numbers.")
  })
  k <- length(folds_list)
  if (max(num_plot) > k) {
    num_plot <- num_plot[num_plot <= k]
  }
  if (.is_loo2(cv)) {
    len <- length(unique(unlist(cv$folds_list)))
  }  else {
    len <- length(unlist(folds_list[[1]]))
  }
  if (len != nrow(x)) {
    stop("Number of rows in 'x' does not match the folds in 'cv'!")
  }
  df <- data.frame(id = seq_len(len))
  for (i in num_plot) {
    df[, paste("Fold", i, sep = "")] <- NA
    test <- folds_list[[i]][[2]]
    train <- folds_list[[i]][[1]]
    df[test, paste("Fold", i, sep = "")] <- 0
    df[train, paste("Fold", i, sep = "")] <- 1
  }
  sf_colname <- attr(x, "sf_column")
  xf <- cbind(x, df)
  x_df <- as.data.frame(xf)
  fold_names <- paste("Fold", num_plot, sep = "")
  x_reshape <- stats::reshape(x_df, direction = "long", idvar = "id",
                              varying = fold_names, times = fold_names, v.names = "value",
                              timevar = "folds")
  x_long <- sf::st_as_sf(x_reshape, sf_column_name = sf_colname)
  x_long$value <- as.factor(x_long$value)
  levels(x_long$value) <- c("Test", "Train")
  return(x_long)
}

cv_plot2<-function (cv_list, x, r = NULL, nrow = NULL, ncol = NULL, num_plots = 1:10,max_pixels = 3e+05, remove_na = TRUE, raster_colors = gray.colors(10,alpha = 1), points_colors = c("#E69F00", "#56B4E9"), points_alpha = 0.7, label_size = 4, repeats=1)
{

  cv<-cv_list[[repeats]]

  if (!is.null(r)) {
    r <- r[[1]]
  }
  is_spatial <- inherits(cv, "cv_spatial")
  if (is_spatial) {
    blocks <- cv$blocks
    geom_poly <- ggplot2::geom_sf(data = sf::st_geometry(blocks),
                                  inherit.aes = FALSE, colour = "red", fill = "orangered4",
                                  alpha = 0.04, linewidth = 0.2)
  }
  if (!missing(x)) {
    x_long <- .x_to_long2(x, cv, num_plot = num_plots)
    if (.is_loo2(cv) && remove_na) {
      x_long <- x_long[which(stats::complete.cases(x_long$value)),  ]
    }
  }  else {
    if (!is_spatial)
      stop("'x' is required for plotting cv_cluster, cv_buffer and cv_nndm.")
  }
  geom_sftext <- if (label_size > 0) {
    ggplot2::geom_sf_text(ggplot2::aes(label = get("folds")),
                          size = label_size, fun.geometry = sf::st_centroid)
  }  else NULL
  if (missing(x)) {
    if (is_spatial) {
      p1 <- ggplot2::ggplot(data = blocks) + switch(!is.null(r),
                                                    geom_rast) + switch(!is.null(r), geom_rast_col) +
        ggplot2::geom_sf(colour = "red", fill = "orangered4",
                         alpha = 0.04, linewidth = 0.2) + switch(!is.null(geom_sftext),
                                                                 geom_sftext) + ggplot2::labs(x = "", y = "") +
        ggplot2::scale_x_continuous(guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::theme_minimal() + ggplot2::guides(fill = "none")
    }
  }  else {
    p1 <- ggplot2::ggplot(data = x_long) + switch(!is.null(r),
                                                  geom_rast) + switch(!is.null(r), geom_rast_col) +
      switch(is_spatial, geom_poly) + ggplot2::geom_sf(ggplot2::aes(col = get("value")),
                                                       alpha = points_alpha) + ggplot2::scale_color_manual(values = points_colors,
                                                                                                           na.value = "#BEBEBE03") + ggplot2::facet_wrap(~get("folds"),
                                                                                                                                                         nrow = nrow, ncol = ncol) + ggplot2::labs(x = "",
                                                                                                                                                                                                   y = "", col = "") + ggplot2::theme_bw() + ggplot2::guides(fill = "none")
  }
  return(p1)
}

plot_test_fold_heatmap <- function(summary_block,
                                   orientation = "auto",
                                   title = "Class distribution across test folds") {

  df <- as.data.frame(summary_block)

  test_cols <- grep("^test_", names(df), value = TRUE)

  if (length(test_cols) == 0) {
    stop("No columns starting with 'test_' were found.")
  }

  test_df <- df[test_cols]

  # Detect orientation
  if (orientation == "auto") {

    test_names <- gsub("^test_", "", test_cols)

    # If test column names are mostly numbers, assume columns are folds
    numeric_test_names <- suppressWarnings(!is.na(as.numeric(test_names)))

    if (all(numeric_test_names)) {
      orientation <- "classes_in_rows"
    } else {
      orientation <- "folds_in_rows"
    }
  }

  if (orientation == "classes_in_rows") {

    df_long <- data.frame(
      class = rep(seq_len(nrow(test_df)), times = length(test_cols)),
      fold = rep(test_cols, each = nrow(test_df)),
      n = as.numeric(unlist(test_df))
    )

    df_long$fold <- gsub("^test_", "Fold ", df_long$fold)
    df_long$class <- factor(df_long$class)

  } else if (orientation == "folds_in_rows") {

    df_long <- data.frame(
      fold = rep(seq_len(nrow(test_df)), times = length(test_cols)),
      class = rep(test_cols, each = nrow(test_df)),
      n = as.numeric(unlist(test_df))
    )

    df_long$fold <- paste("Fold", df_long$fold)
    df_long$class <- gsub("^test_", "", df_long$class)
    df_long$class <- factor(df_long$class, levels = unique(df_long$class))

  } else {
    stop("orientation must be 'auto', 'classes_in_rows', or 'folds_in_rows'.")
  }

  df_long$label <- as.character(df_long$n)
  df_long$text_color <- ifelse(df_long$n == 0, "red", "black")
  df_long$text_face <- ifelse(df_long$n == 0, "bold", "plain")

  ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = fold, y = class, fill = n)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = label,
        color = text_color,
        fontface = text_face
      ),
      size = 3.3
    ) +
    ggplot2::scale_color_identity() +
    viridis::scale_fill_viridis(
      option = "viridis",
      direction = -1,
      name = "Records"
    ) +
    ggplot2::labs(
      x = "Test fold",
      y = "Class / group",
      title = title
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}
blockcv_to_caret <- function(cvsp) {

  if (!is.null(cvsp$folds_list)) {

    if (is.list(cvsp$folds_list[[1]]) &&
        all(c("train", "test") %in% names(cvsp$folds_list[[1]]))) {

      index <- lapply(cvsp$folds_list, function(z) z$train)
      indexOut <- lapply(cvsp$folds_list, function(z) z$test)

    } else {

      index <- cvsp$folds_list

      if (is.null(cvsp$folds_ids)) {
        stop("Could not find 'folds_ids' to build indexOut.")
      }

      fold_ids <- sort(unique(cvsp$folds_ids))
      indexOut <- lapply(fold_ids, function(f) which(cvsp$folds_ids == f))
    }

  } else {

    if (is.null(cvsp$folds_ids)) {
      stop("The cv_spatial object must contain either 'folds_list' or 'folds_ids'.")
    }

    fold_ids <- sort(unique(cvsp$folds_ids))

    indexOut <- lapply(fold_ids, function(f) which(cvsp$folds_ids == f))
    index <- lapply(fold_ids, function(f) which(cvsp$folds_ids != f))
  }

  names(index) <- paste0("Fold", seq_along(index))
  names(indexOut) <- paste0("Fold", seq_along(indexOut))

  list(
    index = index,
    indexOut = indexOut
  )
}
repeated_blockcv_to_caret <- function(sf_dat,
                                      column = "y",
                                      k = 5,
                                      repeats = 3,
                                      size,
                                      selection = "random",
                                      iteration = 100,
                                      seed = 123) {

  index <- list()
  indexOut <- list()

  for (r in seq_len(repeats)) {

    set.seed(seed + r)

    cvsp <- cv_spatial2(
      x = sf_dat,
      column = column,
      k = k,
      size = size,
      selection = selection,
      iteration = iteration,
      progress = FALSE
    )

    folds <- blockcv_to_caret(cvsp)

    for (f in seq_len(k)) {

      nm <- paste0("Fold", f, ".Rep", r)

      index[[nm]] <- folds$index[[f]]
      indexOut[[nm]] <- folds$indexOut[[f]]
    }
  }

  list(
    index = index,
    indexOut = indexOut
  )
}
create_repeated_spatial_cv <- function(sf_dat,
                                       column = "y",
                                       best_cv,
                                       size,
                                       k = 5,
                                       repeats = 1,
                                       selection = "random",
                                       iteration = 100,
                                       hexagon = TRUE,
                                       progress = FALSE,
                                       seed = NULL) {
  if(!is.null(seed)){
    if(is.na(seed)){
      seed<-NULL
    }
  }


  cv_list <- vector("list", repeats)

  # First repetition: use the already evaluated/suggested scheme
  cv_list[[1]] <- best_cv

  if (repeats > 1) {

    for (r in 2:repeats) {

      if (!is.null(seed) && !is.na(seed)) {
        set.seed(seed + r - 1)
      }

      cv_list[[r]] <- cv_spatial2(
        x = sf_dat,
        column = column,
        k = k,
        size = size,
        selection = selection,
        iteration = iteration,
        hexagon = hexagon,
        progress = progress
      )
    }
  }

  names(cv_list) <- paste0("Rep", seq_len(repeats))

  return(cv_list)
}



cv_spatial3 <- function(x,column = "y",k = 5,repeats = 1,size,selection = "random",iteration = 100,hexagon=hexagon,seed = 123) {
  if(is.na(seed)){
    seed<-NULL
  }


  results<-list()

  for (r in seq_len(repeats)) {

    if(!is.null(seed))
    set.seed(seed + r)

    cvsp <- cv_spatial2(
      x = x,
      column = column,
      k = k,
      size = size,
      selection = selection,
      iteration = iteration,
      progress = FALSE,
      hexagon=hexagon
    )
    results[[r]]<-cvsp

  }
  names(results) <- paste0("Rep", seq_len(repeats))
  results
}


repeated_blockcv_to_caret_from_list <- function(cv_list) {

  index <- list()
  indexOut <- list()

  for (r in seq_along(cv_list)) {

    folds <- blockcv_to_caret(cv_list[[r]])

    for (f in seq_along(folds$index)) {

      nm <- paste0("Fold", f, ".Rep", r)

      train_idx <- folds$index[[f]]
      test_idx  <- folds$indexOut[[f]]

      if (is.list(train_idx)) {
        train_idx <- train_idx[[1]]
      }

      if (is.list(test_idx)) {
        if (length(test_idx) >= 2) {
          test_idx <- test_idx[[2]]
        } else {
          test_idx <- test_idx[[1]]
        }
      }

      index[[nm]] <- as.integer(train_idx)
      indexOut[[nm]] <- as.integer(test_idx)
    }
  }

  # Final validation
  valid_index <- all(vapply(index, is.integer, logical(1)))
  valid_indexOut <- all(vapply(indexOut, is.integer, logical(1)))

  if (!valid_index) {
    stop("'index' must be a list of integer vectors.")
  }

  if (!valid_indexOut) {
    stop("'indexOut' must be a list of integer vectors.")
  }

  list(
    index = index,
    indexOut = indexOut
  )
}

resampling_label <- function(m, numbers = TRUE, short_cvsp = FALSE) {

  if (!("control" %in% names(m))) {
    return("")
  }

  cvsp <- attr(m, "cvsp")

  if (!is.null(cvsp)) {

    k <- cvsp$k
    repeats <- cvsp$repeats
    size <- suppressWarnings(as.numeric(unlist(cvsp$size)[1]))
    selection <- cvsp$selection
    hexagon <- cvsp$hexagon

    if (is.null(repeats) || is.na(repeats)) {
      repeats <- 1
    }

    size_label <- if (is.finite(size)) {
      paste0(round(size, 2), " m")
    } else {
      "not available"
    }

    if (numbers) {

      if (repeats > 1) {

        if (short_cvsp) {
          return(
            paste0(
              "Repeated Spatial Block Cross-Validation (",
              k, " fold, repeated ", repeats, " times)"
            )
          )
        }

        return(
          paste0(
            "Repeated Spatial Block Cross-Validation (",
            k, " fold, repeated ", repeats, " times",
            "; block size = ", size_label,
            "; selection = ", selection,
            "; hexagonal blocks = ", hexagon,
            ")"
          )
        )

      } else {

        if (short_cvsp) {
          return(
            paste0(
              "Spatial Block Cross-Validation (",
              k, " fold)"
            )
          )
        }

        return(
          paste0(
            "Spatial Block Cross-Validation (",
            k, " fold",
            "; block size = ", size_label,
            "; selection = ", selection,
            "; hexagonal blocks = ", hexagon,
            ")"
          )
        )
      }

    } else {

      if (repeats > 1) {
        return("(Repeated Spatial Block Cross-Validation)")
      } else {
        return("(Spatial Block Cross-Validation)")
      }
    }
  }

  caret:::resampName(m, numbers = numbers)
}


print_train_imesc <- function(x,
                              printCall = FALSE,
                              details = FALSE,
                              selectCol = FALSE,
                              showSD = FALSE,
                              ...) {

  if (!inherits(x, "train")) {
    stop("'x' must be a caret train object.")
  }

  if (!is.null(x$modelInfo$label)) {
    cat(x$modelInfo$label, "\n\n")
  }

  if (printCall) {
    caret:::printCall(x$call)
  }

  if (!is.null(x$trainingData)) {

    chDim <- dim(x$trainingData)
    chDim[2] <- chDim[2] - 1

    if (x$modelType == "Classification") {
      lev <- levels(x)
      if (is.character(lev)) {
        chDim <- c(chDim, length(lev))
      }
    } else {
      lev <- NULL
    }

    chDim <- format(chDim)

    cat(chDim[1], " samples", sep = "")

    if (!is.null(x$control$indexFinal)) {
      cat(",", length(x$control$indexFinal), "used for final model\n")
    } else {
      cat("\n")
    }

    cat(
      chDim[2],
      " predictor",
      ifelse(chDim[2] > 1, "s\n", "\n"),
      sep = ""
    )

    if (is.character(lev)) {
      cat(
        chDim[3],
        "classes:",
        paste("'", lev, "'", sep = "", collapse = ", "),
        "\n"
      )
    }

    cat("\n")
  }

  if (!is.null(x$preProc)) {

    caret:::pp_list(x$preProc$method)

  } else {

    if (inherits(x, "train.recipe")) {

      step_names <- function(x) {
        gsub("^step_", "", class(x)[1])
      }

      steps_used <- unlist(lapply(x$recipe$steps, step_names))

      ppText <- paste(
        "Recipe steps:",
        paste(steps_used, collapse = ", ")
      )

      cat(caret:::truncateText(ppText), "\n")

    } else {
      cat("No pre-processing\n")
    }
  }

  if (!is.null(x$control$index)) {

    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)

    resampText <- resampling_label(x)

    cat("Resampling:", resampText, "\n")

    if (x$control$method != "none") {

      outLabel <- x$metric

      resampleN <- as.character(resampleN)

      if (numResamp > 5) {
        resampleN <- c(resampleN[1:6], "...")
      }

      cat(
        "Summary of sample sizes:",
        paste(resampleN, collapse = ", "),
        "\n"
      )
    }
  }

  if (!is.null(x$control$sampling)) {

    cat("Addtional sampling using ")

    cat(
      switch(
        x$control$sampling$name,
        down = "down-sampling",
        up = "up-sampling",
        smote = "SMOTE",
        rose = "ROSE",
        custom = "a custom function"
      )
    )

    if (!is.null(x$preProc)) {
      if (x$control$sampling$first) {
        cat(" prior to pre-processing")
      } else {
        cat(" after to pre-processing")
      }
    }

    cat("\n\n")
  }

  if (x$control$method != "none") {

    tuneAcc <- x$results
    tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter", drop = FALSE]

    cat("Resampling results")

    if (dim(tuneAcc)[1] > 1) {
      cat(" across tuning parameters")
    }

    if (showSD) {
      cat(" (values below are 'mean (sd)')")
    }

    cat(":\n\n")

    if (dim(tuneAcc)[1] > 1) {

      numParam <- length(x$bestTune)
      finalTune <- x$bestTune

      optValues <- paste(
        names(finalTune),
        "=",
        format(finalTune, ...)
      )

      optString <- paste0(
        "The final ",
        ifelse(numParam > 1, "values", "value"),
        " used for the model ",
        ifelse(numParam > 1, "were ", "was "),
        caret:::stringFunc(optValues),
        "."
      )

      finalTune$Selected <- "*"

      if (any(names(tuneAcc) %in% "method")) {
        names(tuneAcc)[names(tuneAcc) %in% "method"] <- ".method"
      }

      if (any(names(finalTune) %in% "method")) {
        names(finalTune)[names(finalTune) %in% "method"] <- ".method"
      }

      tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)

      if (any(names(tuneAcc) %in% ".method")) {
        names(tuneAcc)[names(tuneAcc) %in% ".method"] <- "method"
      }

      tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""

    } else {
      optString <- ""
    }

    sdCols <- grep("SD$", colnames(tuneAcc))

    if (showSD) {

      sdCheck <- unlist(
        lapply(
          tuneAcc[, sdCols, drop = FALSE],
          function(u) all(is.na(u))
        )
      )

      if (any(sdCheck)) {
        rmCols <- names(sdCheck)[sdCheck]
        tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% rmCols), drop = FALSE]
      }

    } else {

      if (length(sdCols) > 0) {
        tuneAcc <- tuneAcc[, -sdCols, drop = FALSE]
      }
    }

    params <- names(x$bestTune)

    if (!all(params == "parameter")) {

      numVals <- apply(
        tuneAcc[, params, drop = FALSE],
        2,
        function(x) length(unique(x))
      )

      if (any(numVals < 2)) {

        constString <- NULL

        for (i in seq(along.with = numVals)) {
          if (numVals[i] == 1) {
            constString <- c(
              constString,
              paste0(
                "Tuning parameter '",
                names(numVals)[i],
                "' was held constant at a value of ",
                caret:::stringFunc(tuneAcc[1, names(numVals)[i]])
              )
            )
          }
        }

        discard <- names(numVals)[which(numVals == 1)]
        tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% discard), drop = FALSE]

      } else {
        constString <- NULL
      }

    } else {
      constString <- NULL
    }

    tuneAcc <- tuneAcc[
      ,
      !grepl("Apparent$|Optimism$", names(tuneAcc)),
      drop = FALSE
    ]

    colnames(tuneAcc)[colnames(tuneAcc) == ".B"] <- "Resamples"

    nms <- names(tuneAcc)[names(tuneAcc) %in% params]

    sort_args <- vector(mode = "list", length = length(nms))

    for (i in seq(along.with = nms)) {
      sort_args[[i]] <- tuneAcc[, nms[i]]
    }

    tune_ord <- do.call("order", sort_args)

    if (!is.null(tune_ord)) {
      tuneAcc <- tuneAcc[tune_ord, , drop = FALSE]
    }

    theDots <- list(...)
    theDots$x <- tuneAcc

    printMat <- do.call("format.data.frame", theDots)
    printMat <- as.matrix(printMat)
    rownames(printMat) <- rep("", dim(printMat)[1])

    if (showSD) {

      sdCols <- grep("SD$", colnames(printMat), value = TRUE)
      sd_dat <- printMat[, sdCols, drop = FALSE]
      printMat <- printMat[
        ,
        !(colnames(printMat) %in% sdCols),
        drop = FALSE
      ]

      for (col_name in sdCols) {

        not_sd <- gsub("SD$", "", col_name)

        if (any(colnames(printMat) == not_sd)) {
          printMat[, not_sd] <- paste0(
            printMat[, not_sd],
            " (",
            sd_dat[, col_name],
            ")"
          )
        }
      }
    }

    if (!selectCol) {
      printMat <- printMat[
        ,
        colnames(printMat) != "Selected",
        drop = FALSE
      ]
    }

    print(printMat, quote = FALSE, print.gap = 2)
    cat("\n")

    if (!is.null(constString)) {
      cat(caret:::truncateText(paste(constString, collapse = "\n")))
      cat("\n")
    }

    if (dim(tuneAcc)[1] > 1) {

      if (is.null(x$update)) {

        met <- paste(
          x$metric,
          "was used to select the optimal model using"
        )

        if (is.function(x$control$selectionFunction)) {
          met <- paste(met, " a custom selection rule.\n")
        } else {
          met <- paste(
            met,
            switch(
              x$control$selectionFunction,
              best = paste(
                "the",
                ifelse(x$maximize, "largest", "smallest"),
                "value.\n"
              ),
              oneSE = " the one SE rule.\n",
              tolerance = " a tolerance rule.\n"
            )
          )
        }

      } else {

        met <- paste(
          "The tuning",
          ifelse(ncol(x$bestTune) > 1, "parameters", "parameter"),
          "was set manually.\n"
        )
      }

      cat(caret:::truncateText(met))
    }

    cat(caret:::truncateText(optString))

    if (nzchar(optString)) {
      cat("\n")
    }

  } else {

    printMat <- NULL
  }

  if (details) {

    if (!(x$method %in% c("gbm", "treebag", "nb", "lvq", "knn"))) {

      cat("\n----------------------------------------------------------\n")
      cat("\nThe final model:\n\n")

      switch(
        x$method,
        lm = ,
        nnet = ,
        multinom = ,
        pls = ,
        earth = ,
        lmStepAIC = ,
        bagEarth = ,
        bagFDA = print(summary(x$finalModel)),
        rpart = ,
        ctree = ,
        ctree2 = ,
        cforest = ,
        glmboost = ,
        gamboost = ,
        blackboost = ,
        ada = ,
        randomForest = ,
        pcaNNet = ,
        svmradial = ,
        svmpoly = ,
        svmRadial = ,
        svmPoly = ,
        rvmRadial = ,
        rvmPoly = ,
        lssvmRadial = ,
        lssvmPoly = ,
        gaussprRadial = ,
        gaussprPoly = ,
        enet = ,
        lasso = ,
        LMT = ,
        JRip = ,
        lda = ,
        rda = ,
        pamr = ,
        gpls = ,
        J48 = ,
        ppr = print(x$finalModel),
        fda = {
          print(x$finalModel)
          cat("\n Summary of Terms\n\n")
          print(x$finalModel$fit)
        }
      )
    }
  }

  invisible(printMat)
}

confusionMatrix_train_imesc <- function(data,
                                        norm = "overall",
                                        dnn = c("Prediction", "Reference"),
                                        short_cvsp=T,
                                        ...) {

  if (data$control$method %in% c("oob", "LOOCV", "none")) {
    stop(
      "cannot compute confusion matrices for leave-one-out, ",
      "out-of-bag resampling, or no resampling"
    )
  }

  if (inherits(data, "train")) {

    if (data$modelType == "Regression") {
      stop("confusion matrices are only valid for classification models")
    }

    lev <- levels(data)
    resampledCM <- caret:::train_resampledCM(data)

  } else {

    lev <- data$obsLevels

    if (inherits(data, "rfe")) {
      resampledCM <- caret:::rfe_resampledCM(data)
    }

    if (inherits(data, "sbf")) {
      resampledCM <- caret:::sbf_resampledCM(data)
    }
  }

  if (!is.null(data$control$index)) {

    resampleN <- unlist(lapply(data$control$index, length))
    numResamp <- length(resampleN)

    # Original caret:
    # resampText <- caret:::resampName(data)

    # iMESc spatial-aware label:
    resampText <- resampling_label(data, short_cvsp = T)

  } else {

    resampText <- ""
    numResamp <- 0
  }

  counts <- as.matrix(
    resampledCM[, grep("^\\.?cell", colnames(resampledCM))]
  )

  norm <- match.arg(norm, c("none", "overall", "average"))

  if (norm == "none") {
    counts <- matrix(
      apply(counts, 2, sum),
      nrow = length(lev)
    )
  } else {
    counts <- matrix(
      apply(counts, 2, mean),
      nrow = length(lev)
    )
  }

  if (norm == "overall") {
    counts <- counts / sum(counts) * 100
  }

  rownames(counts) <- colnames(counts) <- lev
  names(dimnames(counts)) <- dnn

  out <- list(
    table = as.table(counts),
    norm = norm,
    B = length(data$control$index),
    text = paste(resampText, "Confusion Matrix")
  )

  class(out) <- paste0("confusionMatrix.", class(data))

  out
}
