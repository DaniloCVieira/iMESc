# Spatio-temporal validation helpers for iMESc.
# These functions intentionally do not depend on the blockCV package.
# Spatial folds are created with cv_spatial2(), defined in funs_spatial_validation.R.

.stcv_require_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for spatial validation.")
  }
  invisible(TRUE)
}

.stcv_require_cv_spatial2 <- function() {
  if (!exists("cv_spatial2", mode = "function", inherits = TRUE)) {
    stop("cv_spatial2() was not found. Source inst/www/funs_spatial_validation.R before using these functions.")
  }
  invisible(TRUE)
}

.stcv_restore_attrs <- function(df, stcv_names, stcv_info, spatial_folds_info = NULL,
                                spatial_folds_object = NULL) {
  attr(df, "stcv_names") <- stcv_names
  attr(df, "stcv_info") <- stcv_info
  if (!is.null(spatial_folds_info)) {
    attr(df, "spatial_folds_info") <- spatial_folds_info
  }
  if (!is.null(spatial_folds_object)) {
    attr(df, "spatial_folds_object") <- spatial_folds_object
  }
  df
}

.stcv_get_space_names <- function(df, space_names = NULL) {
  stcv_names <- attr(df, "stcv_names")
  if (is.null(space_names)) {
    if (!is.null(stcv_names) && !is.null(stcv_names$lon) && !is.null(stcv_names$lat)) {
      space_names <- c(stcv_names$lon, stcv_names$lat)
    } else {
      candidates <- list(
        c("Lon", "Lat"),
        c("lon", "lat"),
        c("Longitude", "Latitude"),
        c("longitude", "latitude"),
        c("X", "Y"),
        c("x", "y")
      )
      for (cand in candidates) {
        if (all(cand %in% names(df))) {
          space_names <- cand
          break
        }
      }
      if (is.null(space_names)) {
        return(NULL)
      }
    }
  }
  if (length(space_names) != 2 || !all(space_names %in% names(df))) {
    return(NULL)
  }
  if (!is.numeric(df[[space_names[1]]]) || !is.numeric(df[[space_names[2]]])) {
    return(NULL)
  }
  space_names
}

.stcv_time_block_space_summary <- function(df, time_block_var = "fold_time",
                                           space_names = NULL) {
  space_names <- .stcv_get_space_names(df, space_names)
  if (is.null(space_names) || !time_block_var %in% names(df)) {
    return(NULL)
  }

  lon_var <- space_names[1]
  lat_var <- space_names[2]
  blocks <- sort(unique(as.character(df[[time_block_var]])))
  blocks <- blocks[!is.na(blocks)]

  out <- do.call(rbind, lapply(blocks, function(b) {
    idx <- which(as.character(df[[time_block_var]]) == b)
    coords <- unique(df[idx, c(lon_var, lat_var), drop = FALSE])
    data.frame(
      time_block = b,
      n_obs = length(idx),
      n_unique_coords = nrow(coords),
      lon_min = min(df[[lon_var]][idx], na.rm = TRUE),
      lon_max = max(df[[lon_var]][idx], na.rm = TRUE),
      lat_min = min(df[[lat_var]][idx], na.rm = TRUE),
      lat_max = max(df[[lat_var]][idx], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

.stcv_normalize_time_vector <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(x)
  }
  if (is.factor(x)) {
    return(as.integer(x))
  }
  if (is.character(x)) {
    x_num <- suppressWarnings(as.numeric(x))
    if (all(is.na(x) | !is.na(x_num))) {
      return(x_num)
    }
    x_date <- suppressWarnings(as.Date(x))
    if (all(is.na(x) | !is.na(x_date))) {
      return(x_date)
    }
    return(match(x, unique(x)))
  }
  x
}

prepare_stcv_data <- function(df, spattime_names = c("Lon", "Lat", "Tempo"),
                              response = NULL, sort_time = TRUE,
                              keep_original_order = TRUE) {
  df <- data.frame(df)

  if (length(spattime_names) != 3) {
    stop("spattime_names must have exactly 3 names: longitude, latitude, and time.")
  }
  if (!all(spattime_names %in% names(df))) {
    missing <- spattime_names[!spattime_names %in% names(df)]
    stop("Columns not found in df: ", paste(missing, collapse = ", "))
  }
  if (!is.null(response) && !response %in% names(df)) {
    stop("The response column was not found in df.")
  }

  lon_var <- spattime_names[1]
  lat_var <- spattime_names[2]
  time_var <- spattime_names[3]
  df[[time_var]] <- .stcv_normalize_time_vector(df[[time_var]])

  core_vars <- unique(c(spattime_names, response))
  core_vars <- core_vars[!is.na(core_vars) & nzchar(core_vars)]
  na_core <- colSums(is.na(df[, core_vars, drop = FALSE]))
  if (any(na_core > 0)) {
    warning(
      "Missing values in core columns: ",
      paste(names(na_core)[na_core > 0], na_core[na_core > 0], sep = "=", collapse = "; "),
      ". This may affect folds or splits."
    )
  }

  if (keep_original_order) {
    df$.rowid_original <- seq_len(nrow(df))
  }
  df$.rowid <- seq_len(nrow(df))

  if (sort_time) {
    ord <- order(df[[time_var]], df[[lon_var]], df[[lat_var]])
    df <- df[ord, , drop = FALSE]
    rownames(df) <- NULL
    df$.rowid <- seq_len(nrow(df))
  }

  attr(df, "stcv_names") <- list(
    lon = lon_var,
    lat = lat_var,
    time = time_var,
    response = response
  )
  attr(df, "stcv_info") <- list(
    n_rows = nrow(df),
    n_time = length(unique(df[[time_var]])),
    time_values = sort(unique(df[[time_var]])),
    sort_time = sort_time,
    keep_original_order = keep_original_order
  )

  df
}

choose_seed_blocks <- function(block_info, k_spat, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (!all(c("block_id", "cx", "cy") %in% names(block_info))) {
    stop("block_info must contain block_id, cx, and cy.")
  }
  if (nrow(block_info) < k_spat) {
    stop("Number of blocks is smaller than k_spat.")
  }

  coords <- as.matrix(block_info[, c("cx", "cy")])
  seed_idx <- integer(k_spat)
  seed_idx[1] <- sample(seq_len(nrow(block_info)), size = 1)
  if (k_spat >= 2) {
    for (j in 2:k_spat) {
      chosen <- seed_idx[1:(j - 1)]
      remaining <- setdiff(seq_len(nrow(block_info)), chosen)
      min_d <- vapply(remaining, function(i) {
        d <- sqrt((coords[i, 1] - coords[chosen, 1])^2 +
                    (coords[i, 2] - coords[chosen, 2])^2)
        min(d)
      }, numeric(1))
      seed_idx[j] <- remaining[which.max(min_d)]
    }
  }
  block_info$block_id[seed_idx]
}

make_square_blocks <- function(x, cellsize, eps = 1e-8) {
  if (!inherits(x, "sf")) {
    stop("x must be an sf object.")
  }
  if (!is.numeric(cellsize) || length(cellsize) != 1 || cellsize <= 0) {
    stop("cellsize must be a single number > 0.")
  }

  bb <- sf::st_bbox(x)
  bb_exp <- bb
  bb_exp["xmin"] <- bb["xmin"] - eps
  bb_exp["xmax"] <- bb["xmax"] + eps
  bb_exp["ymin"] <- bb["ymin"] - eps
  bb_exp["ymax"] <- bb["ymax"] + eps

  x_seq <- seq(bb_exp["xmin"], bb_exp["xmax"], by = cellsize)
  y_seq <- seq(bb_exp["ymin"], bb_exp["ymax"], by = cellsize)
  if (tail(x_seq, 1) < bb_exp["xmax"]) x_seq <- c(x_seq, bb_exp["xmax"])
  if (tail(y_seq, 1) < bb_exp["ymax"]) y_seq <- c(y_seq, bb_exp["ymax"])

  polys <- vector("list", length = (length(x_seq) - 1) * (length(y_seq) - 1))
  meta <- data.frame(block_id = integer(0), row_id = integer(0),
                     col_id = integer(0), stringsAsFactors = FALSE)
  id <- 1L
  for (r in seq_len(length(y_seq) - 1)) {
    for (c in seq_len(length(x_seq) - 1)) {
      coords <- matrix(
        c(x_seq[c], y_seq[r],
          x_seq[c + 1], y_seq[r],
          x_seq[c + 1], y_seq[r + 1],
          x_seq[c], y_seq[r + 1],
          x_seq[c], y_seq[r]),
        ncol = 2,
        byrow = TRUE
      )
      polys[[id]] <- sf::st_polygon(list(coords))
      meta <- rbind(meta, data.frame(block_id = id, row_id = r,
                                     col_id = c, stringsAsFactors = FALSE))
      id <- id + 1L
    }
  }

  grid_sf <- sf::st_sf(meta, geometry = sf::st_sfc(polys, crs = sf::st_crs(x)))
  grid_sf$grid_shape <- "square"
  grid_sf
}

make_hex_blocks <- function(x, cellsize, eps = 1e-8) {
  bb <- sf::st_bbox(x)
  bb_exp <- bb
  bb_exp["xmin"] <- bb["xmin"] - eps
  bb_exp["xmax"] <- bb["xmax"] + eps
  bb_exp["ymin"] <- bb["ymin"] - eps
  bb_exp["ymax"] <- bb["ymax"] + eps
  grid <- sf::st_make_grid(sf::st_as_sfc(bb_exp), cellsize = cellsize,
                           square = FALSE, what = "polygons")
  grid_sf <- sf::st_sf(block_id = seq_along(grid), geometry = grid)
  grid_sf$grid_shape <- "hexagon"
  grid_sf
}

make_spatial_blocks <- function(x, cellsize,
                                grid_shape = c("square", "hexagon"),
                                eps = 1e-8) {
  grid_shape <- match.arg(grid_shape)
  if (grid_shape == "square") {
    return(make_square_blocks(x = x, cellsize = cellsize, eps = eps))
  }
  make_hex_blocks(x = x, cellsize = cellsize, eps = eps)
}

assign_points_to_blocks <- function(points_sf, blocks_sf) {
  hits <- sf::st_intersects(points_sf, blocks_sf)
  block_id <- vapply(hits, function(idx) {
    if (length(idx) == 0) {
      return(NA_integer_)
    }
    blocks_sf$block_id[idx[1]]
  }, integer(1))
  as.integer(block_id)
}

get_block_centers <- function(blocks_sf) {
  if (!inherits(blocks_sf, "sf")) {
    stop("blocks_sf must be an sf object.")
  }
  if (!"block_id" %in% names(blocks_sf)) {
    stop("blocks_sf must contain block_id.")
  }
  cent <- sf::st_centroid(sf::st_geometry(blocks_sf))
  coords <- sf::st_coordinates(cent)
  out <- data.frame(block_id = blocks_sf$block_id, cx = coords[, 1],
                    cy = coords[, 2], stringsAsFactors = FALSE)
  if ("row_id" %in% names(blocks_sf)) out$row_id <- blocks_sf$row_id
  if ("col_id" %in% names(blocks_sf)) out$col_id <- blocks_sf$col_id
  if ("grid_shape" %in% names(blocks_sf)) out$grid_shape <- blocks_sf$grid_shape
  out
}

build_square_block_adjacency <- function(blocks_sf,
                                         contiguity = c("rook", "queen")) {
  contiguity <- match.arg(contiguity)
  req_cols <- c("block_id", "row_id", "col_id")
  if (!all(req_cols %in% names(blocks_sf))) {
    stop("Square blocks must contain block_id, row_id, and col_id.")
  }
  info <- sf::st_drop_geometry(blocks_sf)[, req_cols, drop = FALSE]
  info <- info[order(info$block_id), , drop = FALSE]
  adj_list <- vector("list", nrow(info))
  names(adj_list) <- as.character(info$block_id)
  for (i in seq_len(nrow(info))) {
    dr <- abs(info$row_id - info$row_id[i])
    dc <- abs(info$col_id - info$col_id[i])
    if (contiguity == "rook") {
      neigh <- which((dr + dc) == 1)
    } else {
      neigh <- which(pmax(dr, dc) == 1 & !(dr == 0 & dc == 0))
    }
    adj_list[[i]] <- info$block_id[neigh]
  }
  adj_list
}

build_hex_block_adjacency <- function(blocks_sf) {
  if (!"block_id" %in% names(blocks_sf)) {
    stop("Hexagon blocks must contain block_id.")
  }
  blocks_sf <- blocks_sf[order(blocks_sf$block_id), , drop = FALSE]
  nb <- sf::st_touches(blocks_sf)
  adj_list <- lapply(seq_along(nb), function(i) blocks_sf$block_id[nb[[i]]])
  names(adj_list) <- as.character(blocks_sf$block_id)
  adj_list
}

build_block_adjacency <- function(blocks_sf,
                                  grid_shape = c("square", "hexagon"),
                                  contiguity = c("rook", "queen")) {
  grid_shape <- match.arg(grid_shape)
  contiguity <- match.arg(contiguity)
  if (grid_shape == "square") {
    return(build_square_block_adjacency(blocks_sf, contiguity = contiguity))
  }
  build_hex_block_adjacency(blocks_sf)
}

grow_contiguous_folds <- function(block_info, adj_list, block_weights,
                                  k_spat, seed = NULL) {
  if (!all(c("block_id", "cx", "cy") %in% names(block_info))) {
    stop("block_info must contain block_id, cx, and cy.")
  }
  block_info <- data.frame(block_info)
  block_info <- block_info[order(block_info$block_id), , drop = FALSE]
  block_ids <- block_info$block_id
  if (length(block_ids) < k_spat) {
    stop("Number of blocks is smaller than k_spat.")
  }
  if (length(block_weights) != length(block_ids)) {
    stop("block_weights must have the same length as block_info.")
  }
  names(block_weights) <- as.character(block_ids)
  missing_adj <- setdiff(as.character(block_ids), names(adj_list))
  if (length(missing_adj) > 0) {
    stop("Missing blocks in adj_list: ", paste(missing_adj, collapse = ", "))
  }

  seed_blocks <- choose_seed_blocks(block_info[, c("block_id", "cx", "cy"),
                                               drop = FALSE],
                                    k_spat = k_spat, seed = seed)
  assignment <- setNames(rep(NA_integer_, length(block_ids)), as.character(block_ids))
  for (i in seq_len(k_spat)) {
    assignment[as.character(seed_blocks[i])] <- i
  }

  repeat {
    unassigned <- names(assignment)[is.na(assignment)]
    if (length(unassigned) == 0) break

    fold_sizes <- vapply(seq_len(k_spat), function(f) {
      ids_f <- names(assignment)[assignment == f]
      sum(block_weights[ids_f], na.rm = TRUE)
    }, numeric(1))
    assigned_this_round <- FALSE

    for (f in order(fold_sizes)) {
      ids_f <- names(assignment)[assignment == f]
      frontier <- unique(unlist(adj_list[ids_f], use.names = FALSE))
      frontier <- as.character(frontier[!is.na(frontier)])
      frontier <- frontier[frontier %in% unassigned]
      if (length(frontier) == 0) next

      fold_cells <- block_info[match(as.integer(ids_f), block_info$block_id), ,
                               drop = FALSE]
      cand_cells <- block_info[match(as.integer(frontier), block_info$block_id), ,
                               drop = FALSE]
      center_x <- mean(fold_cells$cx)
      center_y <- mean(fold_cells$cy)
      dist_to_center <- sqrt((cand_cells$cx - center_x)^2 +
                               (cand_cells$cy - center_y)^2)
      cand_weights <- block_weights[frontier]
      next_block <- frontier[order(dist_to_center, -cand_weights)[1]]
      assignment[next_block] <- f
      assigned_this_round <- TRUE
      break
    }

    if (!assigned_this_round) {
      next_block <- unassigned[1]
      neigh <- as.character(adj_list[[next_block]])
      neigh_folds <- assignment[neigh]
      neigh_folds <- neigh_folds[!is.na(neigh_folds)]
      if (length(neigh_folds) > 0) {
        assignment[next_block] <- neigh_folds[1]
      } else {
        fold_sizes <- vapply(seq_len(k_spat), function(f) {
          ids_f <- names(assignment)[assignment == f]
          sum(block_weights[ids_f], na.rm = TRUE)
        }, numeric(1))
        assignment[next_block] <- which.min(fold_sizes)
      }
    }
  }

  out <- data.frame(block_id = as.integer(names(assignment)),
                    fold_sp = as.integer(assignment),
                    stringsAsFactors = FALSE)
  out[order(out$block_id), , drop = FALSE]
}

make_contiguous_spatial_folds <- function(x, k_spat, cellsize,
                                          grid_shape = c("square", "hexagon"),
                                          contiguity = c("rook", "queen"),
                                          seed = NULL, plot = FALSE) {
  .stcv_require_sf()
  grid_shape <- match.arg(grid_shape)
  contiguity <- match.arg(contiguity)
  if (!inherits(x, "sf")) {
    stop("x must be an sf object.")
  }
  if (is.null(cellsize) || !is.numeric(cellsize) ||
      length(cellsize) != 1 || cellsize <= 0) {
    stop("cellsize must be supplied as a single number > 0.")
  }

  grid_sf <- make_spatial_blocks(x = x, cellsize = cellsize,
                                 grid_shape = grid_shape)
  point_block_id <- assign_points_to_blocks(points_sf = x, blocks_sf = grid_sf)
  if (any(is.na(point_block_id))) {
    stop("Some points were not assigned to a spatial block. Adjust cellsize.")
  }

  block_tab <- as.data.frame(table(point_block_id), stringsAsFactors = FALSE)
  names(block_tab) <- c("block_id", "n_points")
  block_tab$block_id <- as.integer(block_tab$block_id)
  block_tab$n_points <- as.integer(block_tab$n_points)
  occupied_blocks <- grid_sf[grid_sf$block_id %in% block_tab$block_id, ,
                             drop = FALSE]
  occupied_blocks <- merge(occupied_blocks, block_tab, by = "block_id",
                           sort = FALSE)
  if (nrow(occupied_blocks) < k_spat) {
    stop("Number of occupied spatial blocks is smaller than k_spat.")
  }

  block_info <- get_block_centers(occupied_blocks)
  block_info$n_points <- occupied_blocks$n_points
  adj_list <- build_block_adjacency(occupied_blocks, grid_shape = grid_shape,
                                    contiguity = contiguity)
  block_fold_df <- grow_contiguous_folds(
    block_info = block_info[, c("block_id", "cx", "cy"), drop = FALSE],
    adj_list = adj_list,
    block_weights = block_info$n_points,
    k_spat = k_spat,
    seed = seed
  )
  point_fold_df <- merge(
    data.frame(row_id = seq_len(nrow(x)), block_id = point_block_id),
    block_fold_df,
    by = "block_id",
    all.x = TRUE,
    sort = FALSE
  )
  point_fold_df <- point_fold_df[order(point_fold_df$row_id), , drop = FALSE]
  folds_ids <- point_fold_df$fold_sp

  if (plot) {
    blocks_plot <- merge(occupied_blocks, block_fold_df, by = "block_id",
                         sort = FALSE)
    plot(sf::st_geometry(blocks_plot), col = as.factor(blocks_plot$fold_sp),
         border = "grey40")
    plot(sf::st_geometry(x), pch = 16, cex = 0.5, add = TRUE)
  }

  list(
    folds_ids = folds_ids,
    point_block_id = point_block_id,
    block_fold_df = block_fold_df,
    occupied_blocks = occupied_blocks,
    adjacency = adj_list,
    meta = list(k_spat = k_spat, cellsize = cellsize,
                grid_shape = grid_shape,
                contiguity = if (grid_shape == "square") contiguity else NULL,
                seed = seed)
  )
}

make_spatial_folds <- function(df, space_names = NULL, k_spat = 5,
                               selection = c("random", "systematic", "checkerboard", "contiguous"),
                               var_name = NULL, n_bins = 6, extend = 0.5,
                               hexagon = FALSE, cellsize = NULL,
                               grid_shape = c("square", "hexagon"),
                               contiguity = c("rook", "queen"),
                               rows_cols = c(10, 10), iteration = 100L,
                               plot = FALSE, seed = NULL,
                               cellsize_units = c("meters", "native"),
                               spatial_crs = 4326,
                               spatial_work_crs = 3857,
                               ...) {
  .stcv_require_sf()

  stcv_names <- attr(df, "stcv_names")
  stcv_info <- attr(df, "stcv_info")
  df <- data.frame(df)
  selection <- match.arg(selection)
  grid_shape <- match.arg(grid_shape)
  contiguity <- match.arg(contiguity)
  cellsize_units <- match.arg(cellsize_units)

  if (!is.numeric(k_spat) || length(k_spat) != 1 || k_spat < 2) {
    stop("k_spat must be a single number >= 2.")
  }
  if (!is.numeric(n_bins) || length(n_bins) != 1 || n_bins < 2) {
    stop("n_bins must be a single number >= 2.")
  }

  if (is.null(space_names)) {
    if (is.null(stcv_names)) {
      stop("space_names was not supplied and df has no 'stcv_names' attribute.")
    }
    lon_var <- stcv_names$lon
    lat_var <- stcv_names$lat
  } else {
    if (length(space_names) != 2) {
      stop("space_names must have exactly 2 names: longitude and latitude.")
    }
    if (!all(space_names %in% names(df))) {
      missing <- space_names[!space_names %in% names(df)]
      stop("Spatial columns not found in df: ", paste(missing, collapse = ", "))
    }
    lon_var <- space_names[1]
    lat_var <- space_names[2]
  }

  if (!is.numeric(df[[lon_var]]) || !is.numeric(df[[lat_var]])) {
    stop("Spatial columns must be numeric.")
  }
  if (anyNA(df[[lon_var]]) || anyNA(df[[lat_var]])) {
    stop("Spatial columns contain NA. Handle missing coordinates before creating folds.")
  }

  column_to_use <- var_name
  temp_bin_name <- NULL
  if (!is.null(var_name)) {
    if (!var_name %in% names(df)) {
      stop("var_name was not found in df.")
    }
    if (is.numeric(df[[var_name]])) {
      temp_bin_name <- ".stcv_block_bin"
      df[[temp_bin_name]] <- cut(df[[var_name]], breaks = n_bins, include.lowest = TRUE)
      column_to_use <- temp_bin_name
    }
  }

  if (identical(cellsize_units, "meters")) {
    dfsf <- sf::st_as_sf(df, coords = c(lon_var, lat_var), crs = spatial_crs, remove = FALSE)
    if (!is.null(spatial_work_crs)) {
      dfsf <- sf::st_transform(dfsf, spatial_work_crs)
    }
  } else {
    dfsf <- sf::st_as_sf(df, coords = c(lon_var, lat_var), remove = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (selection == "contiguous") {
    sb <- make_contiguous_spatial_folds(
      x = dfsf,
      k_spat = as.integer(k_spat),
      cellsize = cellsize,
      grid_shape = grid_shape,
      contiguity = contiguity,
      seed = seed,
      plot = plot
    )
  } else {
    .stcv_require_cv_spatial2()
    sb <- cv_spatial2(
      x = dfsf,
      column = column_to_use,
      k = as.integer(k_spat),
      hexagon = hexagon,
      size = cellsize,
      rows_cols = rows_cols,
      selection = selection,
      iteration = iteration,
      extend = extend,
      seed = seed,
      plot = plot,
      ...
    )
  }

  if (is.null(sb$folds_ids)) {
    stop("The spatial fold engine did not return 'folds_ids'.")
  }
  if (length(sb$folds_ids) != nrow(df)) {
    stop("The length of spatial folds_ids differs from nrow(df).")
  }

  if (!is.null(temp_bin_name) && temp_bin_name %in% names(df)) {
    df[[temp_bin_name]] <- NULL
  }

  df$fold_sp <- factor(sb$folds_ids)

  spatial_folds_info <- list(
    lon = lon_var,
    lat = lat_var,
    k_spat = k_spat,
    selection = selection,
    var_name = var_name,
    n_bins = n_bins,
    extend = extend,
    hexagon = hexagon,
    cellsize = cellsize,
    cellsize_units = cellsize_units,
    spatial_crs = if (identical(cellsize_units, "meters")) spatial_crs else NULL,
    spatial_work_crs = if (identical(cellsize_units, "meters")) spatial_work_crs else NULL,
    grid_shape = if (selection == "contiguous") grid_shape else NULL,
    contiguity = if (selection == "contiguous" && grid_shape == "square") contiguity else NULL,
    rows_cols = rows_cols,
    iteration = iteration,
    seed = seed,
    engine = if (selection == "contiguous") "custom_contiguous" else "cv_spatial2"
  )

  .stcv_restore_attrs(
    df,
    stcv_names = stcv_names,
    stcv_info = stcv_info,
    spatial_folds_info = spatial_folds_info,
    spatial_folds_object = sb
  )
}

summarize_spatial_folds <- function(df, fold_var = "fold_sp") {
  sf_info <- attr(df, "spatial_folds_info")
  sf_obj <- attr(df, "spatial_folds_object")

  if (is.null(sf_info)) {
    stop("df has no 'spatial_folds_info' attribute.")
  }
  if (!fold_var %in% names(df)) {
    stop("fold_var was not found in df.")
  }

  cat("\n==============================\n")
  cat("Spatial folds summary\n")
  cat("==============================\n")
  cat("Engine   :", sf_info$engine, "\n")
  cat("Selection:", sf_info$selection, "\n")
  cat("k_spat   :", sf_info$k_spat, "\n")
  cat("\nObservations by fold:\n")
  print(table(df[[fold_var]], useNA = "ifany"))

  if (!is.null(sf_obj$blocks) && "folds" %in% names(sf_obj$blocks)) {
    cat("\nBlocks by fold:\n")
    print(table(sf_obj$blocks$folds, useNA = "ifany"))
  }
  if (!is.null(sf_obj$block_fold_df) && "fold_sp" %in% names(sf_obj$block_fold_df)) {
    cat("\nBlocks by fold:\n")
    print(table(sf_obj$block_fold_df$fold_sp, useNA = "ifany"))
  }

  invisible(NULL)
}

plot_spatial_folds <- function(df, fold_var = "fold_sp", time_var = NULL,
                               time_value = NULL, pch = 16, cex = 0.7,
                               main = "Spatial folds") {
  .stcv_require_sf()

  sf_info <- attr(df, "spatial_folds_info")
  if (is.null(sf_info)) {
    stop("df has no 'spatial_folds_info' attribute.")
  }
  if (!fold_var %in% names(df)) {
    stop("fold_var was not found in df.")
  }

  stcv_names <- attr(df, "stcv_names")
  if (is.null(time_var) && !is.null(stcv_names)) {
    time_var <- stcv_names$time
  }
  df_plot <- data.frame(df)
  if (!is.null(time_var) && !is.null(time_value)) {
    df_plot <- df_plot[df_plot[[time_var]] %in% time_value, , drop = FALSE]
  }

  pts_sf <- sf::st_as_sf(
    df_plot,
    coords = c(sf_info$lon, sf_info$lat),
    remove = FALSE
  )

  sf_obj <- attr(df, "spatial_folds_object")
  if (!is.null(sf_obj$occupied_blocks) && !is.null(sf_obj$block_fold_df)) {
    blocks_plot <- merge(sf_obj$occupied_blocks, sf_obj$block_fold_df,
                         by = "block_id", sort = FALSE)
    plot(sf::st_geometry(blocks_plot), col = as.factor(blocks_plot$fold_sp),
         border = "grey50", main = main)
    plot(sf::st_geometry(pts_sf), col = as.factor(df_plot[[fold_var]]), pch = pch,
         cex = cex, add = TRUE)
  } else if (!is.null(sf_obj$blocks)) {
    plot(sf::st_geometry(sf_obj$blocks), col = "grey90", border = "grey60", main = main)
    if ("folds" %in% names(sf_obj$blocks)) {
      plot(sf::st_geometry(sf_obj$blocks), col = as.factor(sf_obj$blocks$folds),
           border = "grey50", add = TRUE)
    }
    plot(sf::st_geometry(pts_sf), col = as.factor(df_plot[[fold_var]]), pch = pch,
         cex = cex, add = TRUE)
  } else {
    plot(sf::st_geometry(pts_sf), col = as.factor(df_plot[[fold_var]]), pch = pch,
         cex = cex, main = main)
  }

  invisible(pts_sf)
}

make_time_origins <- function(df, time_var = NULL, initial_window,
                              horizon = 1, step = 1,
                              window = c("expanding", "rolling"),
                              window_size = NULL) {
  stcv_names <- attr(df, "stcv_names")
  df <- data.frame(df)
  window <- match.arg(window)

  if (is.null(time_var)) {
    if (is.null(stcv_names) || is.null(stcv_names$time)) {
      stop("time_var was not supplied and df has no stcv_names$time attribute.")
    }
    time_var <- stcv_names$time
  }
  if (!time_var %in% names(df)) {
    stop("time_var was not found in df.")
  }

  if (!is.numeric(initial_window) || length(initial_window) != 1 || initial_window < 1) {
    stop("initial_window must be a single number >= 1.")
  }
  if (!is.numeric(horizon) || length(horizon) != 1 || horizon < 1) {
    stop("horizon must be a single number >= 1.")
  }
  if (!is.numeric(step) || length(step) != 1 || step < 1) {
    stop("step must be a single number >= 1.")
  }

  initial_window <- as.integer(initial_window)
  horizon <- as.integer(horizon)
  step <- as.integer(step)

  if (window == "rolling") {
    if (is.null(window_size)) {
      window_size <- initial_window
    }
    if (!is.numeric(window_size) || length(window_size) != 1 || window_size < 1) {
      stop("window_size must be a single number >= 1.")
    }
    window_size <- as.integer(window_size)
  }

  time_values <- sort(unique(df[[time_var]]))
  n_time <- length(time_values)
  if (n_time < (initial_window + horizon)) {
    stop("Not enough unique time levels for initial_window + horizon.")
  }

  origins <- list()
  split_id <- 1L
  train_end_pos_seq <- seq(from = initial_window, to = n_time - horizon, by = step)

  for (train_end_pos in train_end_pos_seq) {
    if (window == "expanding") {
      train_start_pos <- 1L
    } else {
      train_start_pos <- train_end_pos - window_size + 1L
      if (train_start_pos < 1L) {
        next
      }
    }

    train_times <- time_values[train_start_pos:train_end_pos]
    test_times <- time_values[(train_end_pos + 1L):(train_end_pos + horizon)]

    origins[[split_id]] <- list(
      split_time_id = split_id,
      window = window,
      train_start = min(train_times),
      train_end = max(train_times),
      test_start = min(test_times),
      test_end = max(test_times),
      origin_time = max(train_times),
      train_times = train_times,
      test_times = test_times,
      n_train_times = length(train_times),
      n_test_times = length(test_times)
    )
    split_id <- split_id + 1L
  }

  if (length(origins) == 0) {
    stop("No temporal origins could be created with the supplied parameters.")
  }

  origins_df <- do.call(rbind, lapply(origins, function(x) {
    data.frame(
      split_time_id = x$split_time_id,
      window = x$window,
      train_start = x$train_start,
      train_end = x$train_end,
      test_start = x$test_start,
      test_end = x$test_end,
      origin_time = x$origin_time,
      n_train_times = x$n_train_times,
      n_test_times = x$n_test_times
    )
  }))
  rownames(origins_df) <- NULL

  list(
    time_var = time_var,
    time_values = time_values,
    n_time = n_time,
    initial_window = initial_window,
    horizon = horizon,
    step = step,
    window = window,
    window_size = if (window == "rolling") window_size else NULL,
    origins = origins,
    origins_df = origins_df
  )
}

make_st_forecast_splits <- function(df, time_var = NULL, fold_sp_var = "fold_sp",
                                    time_origins,
                                    spatial_mode = c("forecast_new_space", "forecast_known_space"),
                                    drop_na_test = TRUE, response = NULL,
                                    verbose = TRUE) {
  stcv_names <- attr(df, "stcv_names")
  df <- data.frame(df)
  spatial_mode <- match.arg(spatial_mode)

  if (is.null(time_var)) {
    if (is.null(stcv_names) || is.null(stcv_names$time)) {
      stop("time_var was not supplied and df has no stcv_names$time attribute.")
    }
    time_var <- stcv_names$time
  }
  if (!time_var %in% names(df)) {
    stop("time_var was not found in df.")
  }
  if (!fold_sp_var %in% names(df)) {
    stop("fold_sp_var was not found in df.")
  }
  if (is.null(time_origins) || is.null(time_origins$origins)) {
    stop("time_origins must be the object returned by make_time_origins().")
  }

  if (all(is.na(df[[fold_sp_var]]))) {
    stop("fold_sp_var contains only NA.")
  }

  if (is.null(response) && !is.null(stcv_names) && !is.null(stcv_names$response)) {
    response <- stcv_names$response
  }
  if (!is.null(response) && !response %in% names(df)) {
    stop("The response column was not found in df.")
  }
  if (!".rowid" %in% names(df)) {
    df$.rowid <- seq_len(nrow(df))
  }

  sp_folds <- sort(unique(as.character(df[[fold_sp_var]])))
  sp_folds <- sp_folds[!is.na(sp_folds)]
  if (length(sp_folds) == 0) {
    stop("No valid spatial fold was found.")
  }

  splits <- list()
  split_id <- 1L
  for (i in seq_along(time_origins$origins)) {
    ori <- time_origins$origins[[i]]
    train_times <- ori$train_times
    test_times <- ori$test_times

    for (sp in sp_folds) {
      test_idx <- which(df[[time_var]] %in% test_times &
                          as.character(df[[fold_sp_var]]) == sp)

      if (spatial_mode == "forecast_known_space") {
        train_idx <- which(df[[time_var]] %in% train_times)
      } else {
        train_idx <- which(df[[time_var]] %in% train_times &
                             as.character(df[[fold_sp_var]]) != sp)
      }

      if (drop_na_test && !is.null(response)) {
        test_idx <- test_idx[!is.na(df[[response]][test_idx])]
      }
      if (!is.null(response)) {
        train_idx <- train_idx[!is.na(df[[response]][train_idx])]
      }

      splits[[split_id]] <- list(
        split_id = split_id,
        split_time_id = ori$split_time_id,
        spatial_test_fold = sp,
        spatial_mode = spatial_mode,
        window = ori$window,
        origin_time = ori$origin_time,
        train_start = ori$train_start,
        train_end = ori$train_end,
        test_start = ori$test_start,
        test_end = ori$test_end,
        train_times = train_times,
        test_times = test_times,
        train_idx = train_idx,
        test_idx = test_idx,
        train_rowid = df$.rowid[train_idx],
        test_rowid = df$.rowid[test_idx],
        train_n = length(train_idx),
        test_n = length(test_idx)
      )
      split_id <- split_id + 1L
    }
  }

  if (length(splits) == 0) {
    stop("No split was created.")
  }

  splits_df <- do.call(rbind, lapply(splits, function(x) {
    data.frame(
      split_id = x$split_id,
      split_time_id = x$split_time_id,
      spatial_test_fold = x$spatial_test_fold,
      spatial_mode = x$spatial_mode,
      window = x$window,
      origin_time = x$origin_time,
      train_start = x$train_start,
      train_end = x$train_end,
      test_start = x$test_start,
      test_end = x$test_end,
      train_n = x$train_n,
      test_n = x$test_n
    )
  }))
  rownames(splits_df) <- NULL

  if (verbose) {
    n_empty_test <- sum(splits_df$test_n == 0)
    n_empty_train <- sum(splits_df$train_n == 0)
    if (n_empty_test > 0) {
      warning(n_empty_test, " split(s) have test_n = 0.")
    }
    if (n_empty_train > 0) {
      warning(n_empty_train, " split(s) have train_n = 0.")
    }
  }

  list(
    data = df,
    time_var = time_var,
    fold_sp_var = fold_sp_var,
    response = response,
    spatial_mode = spatial_mode,
    spatial_folds = sp_folds,
    time_origins = time_origins,
    splits = splits,
    splits_df = splits_df,
    meta = list(
      n_splits = length(splits),
      n_spatial_folds = length(sp_folds),
      n_time_origins = length(time_origins$origins)
    )
  )
}

inspect_st_split <- function(cv_obj, split_id = 1, show_head = 10,
                             check_leakage = TRUE) {
  if (is.null(cv_obj$splits) || length(cv_obj$splits) == 0) {
    stop("cv_obj has no splits.")
  }
  split_id <- as.integer(split_id)
  if (length(split_id) != 1 || is.na(split_id) ||
      split_id < 1 || split_id > length(cv_obj$splits)) {
    stop("split_id is outside the available range.")
  }

  df <- cv_obj$data
  sp <- cv_obj$splits[[split_id]]
  time_var <- cv_obj$time_var
  fold_sp_var <- cv_obj$fold_sp_var
  response <- cv_obj$response
  train_df <- df[sp$train_idx, , drop = FALSE]
  test_df <- df[sp$test_idx, , drop = FALSE]

  cat("\n==============================\n")
  cat("Split ID:", sp$split_id, "\n")
  cat("==============================\n")
  if (!is.null(sp$validation_type)) {
    cat("validation_type  :", sp$validation_type, "\n")
  }
  if (!is.null(sp$split_time_id)) {
    cat("split_time_id    :", sp$split_time_id, "\n")
  }
  if (!is.null(sp$time_test_block)) {
    cat("time_test_block  :", sp$time_test_block, "\n")
  }
  if (!is.null(sp$spatial_test_fold)) {
    cat("spatial_test_fold:", sp$spatial_test_fold, "\n")
  }
  if (!is.null(sp$spatial_mode)) {
    cat("spatial_mode     :", sp$spatial_mode, "\n")
  }
  if (!is.null(sp$window)) {
    cat("window           :", sp$window, "\n")
  }
  if (!is.null(sp$origin_time)) {
    cat("origin_time      :", sp$origin_time, "\n")
  }
  cat("train_n          :", sp$train_n, "\n")
  cat("test_n           :", sp$test_n, "\n")

  cat("\nTrain times:\n")
  print(sp$train_times)
  cat("\nTest times:\n")
  print(sp$test_times)
  has_spatial_fold <- !is.null(fold_sp_var) && fold_sp_var %in% names(df)
  if (has_spatial_fold) {
    cat("\nSpatial folds in train:\n")
    print(sort(unique(as.character(train_df[[fold_sp_var]]))))
    cat("\nSpatial folds in test:\n")
    print(sort(unique(as.character(test_df[[fold_sp_var]]))))
  }

  leak_report <- NULL
  if (check_leakage) {
    same_rows <- intersect(sp$train_rowid, sp$test_rowid)
    same_times <- intersect(unique(train_df[[time_var]]), unique(test_df[[time_var]]))
    test_fold_in_train <- if (has_spatial_fold && !is.null(sp$spatial_test_fold)) {
      sp$spatial_test_fold %in% as.character(unique(train_df[[fold_sp_var]]))
    } else {
      NA
    }
    leak_report <- list(
      overlapping_rows = length(same_rows),
      overlapping_times = same_times,
      test_fold_present_in_train = test_fold_in_train
    )

    cat("\nLeakage check:\n")
    cat("- Overlapping rows:", length(same_rows), "\n")
    cat("- Overlapping times:\n")
    print(same_times)
    cat("- Test spatial fold appears in train?:", test_fold_in_train, "\n")
  }

  cat("\nTrain preview:\n")
  print(utils::head(train_df, show_head))
  cat("\nTest preview:\n")
  print(utils::head(test_df, show_head))

  if (!is.null(response) && response %in% names(df)) {
    cat("\nResponse summary in train:\n")
    print(summary(train_df[[response]]))
    cat("\nResponse summary in test:\n")
    print(summary(test_df[[response]]))
  }

  invisible(list(split = sp, train_data = train_df, test_data = test_df,
                 leakage = leak_report))
}

summarize_st_splits <- function(cv_obj) {
  if (is.null(cv_obj$splits_df)) {
    stop("cv_obj has no splits_df.")
  }

  cat("\n==============================\n")
  cat("Spatio-temporal splits summary\n")
  cat("==============================\n")
  cat("Total splits:", nrow(cv_obj$splits_df), "\n")
  if (!is.null(cv_obj$validation_type)) {
    cat("Validation type:", cv_obj$validation_type, "\n")
  }
  if (!is.null(cv_obj$meta$n_time_origins)) {
    cat("Temporal origins:", cv_obj$meta$n_time_origins, "\n")
  }
  if (!is.null(cv_obj$meta$n_time_blocks)) {
    cat("Temporal blocks:", cv_obj$meta$n_time_blocks, "\n")
  }
  if (!is.null(cv_obj$meta$n_spatial_folds)) {
    cat("Spatial folds:", cv_obj$meta$n_spatial_folds, "\n")
  }
  if (!is.null(cv_obj$spatial_mode)) {
    cat("Spatial mode:", cv_obj$spatial_mode, "\n")
  }
  if (!is.null(cv_obj$time_block_space_summary)) {
    cat("\nSpatial coverage by temporal block:\n")
    print(cv_obj$time_block_space_summary)
  }

  cat("\ntrain_n summary:\n")
  print(summary(cv_obj$splits_df$train_n))
  cat("\ntest_n summary:\n")
  print(summary(cv_obj$splits_df$test_n))

  empty_test <- cv_obj$splits_df[cv_obj$splits_df$test_n == 0, , drop = FALSE]
  empty_train <- cv_obj$splits_df[cv_obj$splits_df$train_n == 0, , drop = FALSE]
  if (nrow(empty_test) > 0) {
    cat("\nSplits with empty test:\n")
    print(empty_test)
  }
  if (nrow(empty_train) > 0) {
    cat("\nSplits with empty train:\n")
    print(empty_train)
  }

  invisible(cv_obj$splits_df)
}

make_time_blocks <- function(df, time_var = NULL, k_time = 5,
                             block_size = NULL, labels = NULL) {
  stcv_names <- attr(df, "stcv_names")
  df <- data.frame(df)

  if (is.null(time_var)) {
    if (is.null(stcv_names) || is.null(stcv_names$time)) {
      stop("time_var was not supplied and df has no stcv_names$time attribute.")
    }
    time_var <- stcv_names$time
  }
  if (!time_var %in% names(df)) {
    stop("time_var was not found in df.")
  }

  time_values <- sort(unique(df[[time_var]]))
  n_time <- length(time_values)
  if (n_time < 2) {
    stop("At least two observed time levels are required.")
  }

  if (!is.null(block_size)) {
    if (!is.numeric(block_size) || length(block_size) != 1 || block_size < 1) {
      stop("block_size must be a single number >= 1.")
    }
    block_size <- as.integer(block_size)
    k_time <- ceiling(n_time / block_size)
    block_ids_by_level <- ceiling(seq_along(time_values) / block_size)
    block_ids_by_level <- pmin(block_ids_by_level, k_time)
  } else {
    if (!is.numeric(k_time) || length(k_time) != 1 || k_time < 2) {
      stop("k_time must be a single number >= 2.")
    }
    k_time <- as.integer(k_time)
    k_time <- min(k_time, n_time)
    block_size <- NA_integer_
    block_ids_by_level <- as.integer(cut(seq_along(time_values), breaks = k_time,
                                         labels = FALSE, include.lowest = TRUE))
  }

  block_ids <- block_ids_by_level[match(df[[time_var]], time_values)]

  if (is.null(labels)) {
    labels <- paste0("T", sort(unique(block_ids_by_level)))
  }
  block_factor <- factor(block_ids, levels = sort(unique(block_ids_by_level)),
                         labels = labels[seq_along(sort(unique(block_ids_by_level)))])

  block_df <- do.call(rbind, lapply(sort(unique(block_ids_by_level)), function(b) {
    vals <- time_values[block_ids_by_level == b]
    data.frame(
      time_block = labels[match(b, sort(unique(block_ids_by_level)))],
      block_id = b,
      time_start = min(vals),
      time_end = max(vals),
      n_time_levels = length(vals)
    )
  }))
  rownames(block_df) <- NULL

  list(
    time_var = time_var,
    time_values = time_values,
    block_ids = block_factor,
    block_df = block_df,
    k_time = length(unique(block_ids_by_level)),
    block_size = block_size
  )
}

.stcv_split_labels <- function(splits) {
  keys <- vapply(splits, function(x) {
    if (is.null(x$time_test_block)) {
      paste0("split_", x$split_id)
    } else {
      as.character(x$time_test_block)
    }
  }, character(1))
  paste0("T", match(keys, unique(keys)))
}

.stcv_make_split_df <- function(splits) {
  split_labels <- .stcv_split_labels(splits)
  out <- do.call(rbind, lapply(splits, function(x) {
    i <- match(x$split_id, vapply(splits, `[[`, integer(1), "split_id"))
    data.frame(
      split_label = split_labels[i],
      split_id = x$split_id,
      validation_type = x$validation_type,
      time_test_block = if (is.null(x$time_test_block)) NA_character_ else x$time_test_block,
      spatial_test_fold = if (is.null(x$spatial_test_fold)) NA_character_ else x$spatial_test_fold,
      spatial_mode = if (is.null(x$spatial_mode)) NA_character_ else x$spatial_mode,
      train_start = if (is.null(x$train_start)) NA else x$train_start,
      train_end = if (is.null(x$train_end)) NA else x$train_end,
      test_start = if (is.null(x$test_start)) NA else x$test_start,
      test_end = if (is.null(x$test_end)) NA else x$test_end,
      train_n = x$train_n,
      test_n = x$test_n
    )
  }))
  rownames(out) <- NULL
  out
}

.stcv_make_split_membership <- function(df, splits) {
  split_labels <- .stcv_split_labels(splits)
  out <- do.call(rbind, lapply(seq_along(splits), function(i) {
    sp <- splits[[i]]
    role <- rep("Not used", nrow(df))
    role[sp$train_idx] <- "Train"
    role[sp$test_idx] <- "Test"
    data.frame(
      split_label = split_labels[i],
      split_id = sp$split_id,
      row_id = seq_len(nrow(df)),
      rowid_original = if (".rowid_original" %in% names(df)) {
        df$.rowid_original
      } else {
        NA_integer_
      },
      fold_time = as.character(df$fold_time),
      fold_sp = as.character(df$fold_sp),
      used = role,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out$used <- factor(out$used, levels = c("Train", "Test", "Not used"))
  out
}

.stcv_finalize <- function(df, splits, validation_type, time_var, fold_sp_var = NULL,
                           response = NULL, time_blocks = NULL,
                           time_block_space_summary = NULL,
                           spatial_mode = NULL, spatial_folds = NULL,
                           verbose = TRUE) {
  if (length(splits) == 0) {
    stop("No split was created.")
  }

  splits_df <- .stcv_make_split_df(splits)
  if (verbose) {
    n_empty_test <- sum(splits_df$test_n == 0)
    n_empty_train <- sum(splits_df$train_n == 0)
    if (n_empty_test > 0) {
      warning(n_empty_test, " split(s) have test_n = 0.")
    }
    if (n_empty_train > 0) {
      warning(n_empty_train, " split(s) have train_n = 0.")
    }
  }

  if (!"fold_time" %in% names(df)) {
    df$fold_time <- factor(rep("T1", nrow(df)))
  }
  if (!is.null(fold_sp_var) && fold_sp_var %in% names(df)) {
    fold_sp_chr <- as.character(df[[fold_sp_var]])
    fold_sp_chr[!is.na(fold_sp_chr) & !grepl("^S", fold_sp_chr)] <-
      paste0("S", fold_sp_chr[!is.na(fold_sp_chr) & !grepl("^S", fold_sp_chr)])
    df$fold_sp <- factor(fold_sp_chr, levels = sort(unique(fold_sp_chr[!is.na(fold_sp_chr)])))
    fold_sp_var <- "fold_sp"
  } else if (!"fold_sp" %in% names(df)) {
    df$fold_sp <- factor(rep("S1", nrow(df)))
    fold_sp_var <- "fold_sp"
  } else {
    fold_sp_chr <- as.character(df$fold_sp)
    fold_sp_chr[!is.na(fold_sp_chr) & !grepl("^S", fold_sp_chr)] <-
      paste0("S", fold_sp_chr[!is.na(fold_sp_chr) & !grepl("^S", fold_sp_chr)])
    df$fold_sp <- factor(fold_sp_chr, levels = sort(unique(fold_sp_chr[!is.na(fold_sp_chr)])))
    fold_sp_var <- "fold_sp"
  }
  if (is.null(spatial_folds)) {
    spatial_folds <- sort(unique(as.character(df$fold_sp)))
    spatial_folds <- spatial_folds[!is.na(spatial_folds)]
  } else {
    spatial_folds <- as.character(spatial_folds)
    spatial_folds[!is.na(spatial_folds) & !grepl("^S", spatial_folds)] <-
      paste0("S", spatial_folds[!is.na(spatial_folds) & !grepl("^S", spatial_folds)])
  }
  splits <- lapply(splits, function(sp) {
    if (!is.null(sp$spatial_test_fold)) {
      sp_fold <- as.character(sp$spatial_test_fold)
      if (!is.na(sp_fold) && !grepl("^S", sp_fold)) {
        sp_fold <- paste0("S", sp_fold)
      }
      sp$spatial_test_fold <- sp_fold
    }
    sp
  })
  splits_df <- .stcv_make_split_df(splits)
  split_membership <- .stcv_make_split_membership(df, splits)

  out <- list(
    data = df,
    validation_type = validation_type,
    time_var = time_var,
    fold_sp_var = fold_sp_var,
    response = response,
    spatial_mode = spatial_mode,
    spatial_folds = spatial_folds,
    time_blocks = time_blocks,
    time_block_space_summary = time_block_space_summary,
    splits = splits,
    splits_df = splits_df,
    split_membership = split_membership,
    meta = list(
      n_splits = length(splits),
      n_time_blocks = if (is.null(time_blocks)) length(unique(df$fold_time)) else time_blocks$k_time,
      n_spatial_folds = length(spatial_folds)
    )
  )
  class(out) <- c("stcv", "list")
  out
}

make_time_block_cv <- function(df, time_var = NULL, space_names = NULL, k_time = 5,
                               block_size = NULL, response = NULL,
                               drop_na_test = TRUE, verbose = TRUE) {
  stcv_names <- attr(df, "stcv_names")
  stcv_info <- attr(df, "stcv_info")
  df <- data.frame(df)

  if (is.null(time_var)) {
    if (is.null(stcv_names) || is.null(stcv_names$time)) {
      stop("time_var was not supplied and df has no stcv_names$time attribute.")
    }
    time_var <- stcv_names$time
  }
  if (is.null(response) && !is.null(stcv_names)) {
    response <- stcv_names$response
  }
  if (!".rowid" %in% names(df)) {
    df$.rowid <- seq_len(nrow(df))
  }

  tb <- make_time_blocks(df, time_var = time_var, k_time = k_time,
                         block_size = block_size)
  df$fold_time <- tb$block_ids
  df <- .stcv_restore_attrs(df, stcv_names = stcv_names, stcv_info = stcv_info)
  tb_space <- .stcv_time_block_space_summary(df, "fold_time", space_names)
  time_folds <- levels(tb$block_ids)

  splits <- vector("list", length(time_folds))
  split_id <- 1L
  for (tb_i in time_folds) {
    test_idx <- which(as.character(df$fold_time) == tb_i)
    train_idx <- which(as.character(df$fold_time) != tb_i)
    if (drop_na_test && !is.null(response)) {
      test_idx <- test_idx[!is.na(df[[response]][test_idx])]
    }
    if (!is.null(response)) {
      train_idx <- train_idx[!is.na(df[[response]][train_idx])]
    }

    test_times <- sort(unique(df[[time_var]][test_idx]))
    train_times <- sort(unique(df[[time_var]][train_idx]))
    splits[[split_id]] <- list(
      split_id = split_id,
      validation_type = "time_block_cv",
      time_test_block = tb_i,
      spatial_test_fold = NULL,
      spatial_mode = NULL,
      train_start = min(train_times),
      train_end = max(train_times),
      test_start = min(test_times),
      test_end = max(test_times),
      train_times = train_times,
      test_times = test_times,
      train_idx = train_idx,
      test_idx = test_idx,
      train_rowid = df$.rowid[train_idx],
      test_rowid = df$.rowid[test_idx],
      train_n = length(train_idx),
      test_n = length(test_idx)
    )
    split_id <- split_id + 1L
  }

  .stcv_finalize(df, splits, "time_block_cv", time_var = time_var,
                 response = response, time_blocks = tb,
                 time_block_space_summary = tb_space,
                 verbose = verbose)
}

make_st_contiguous_block_cv <- function(df, time_var = NULL, fold_sp_var = "fold_sp",
                                        space_names = NULL,
                                        k_time = 5, block_size = NULL,
                                        response = NULL, drop_na_test = TRUE,
                                        verbose = TRUE) {
  stcv_names <- attr(df, "stcv_names")
  stcv_info <- attr(df, "stcv_info")
  spatial_folds_info <- attr(df, "spatial_folds_info")
  spatial_folds_object <- attr(df, "spatial_folds_object")
  df <- data.frame(df)

  if (is.null(time_var)) {
    if (is.null(stcv_names) || is.null(stcv_names$time)) {
      stop("time_var was not supplied and df has no stcv_names$time attribute.")
    }
    time_var <- stcv_names$time
  }
  if (!fold_sp_var %in% names(df)) {
    stop("fold_sp_var was not found in df. Run make_spatial_folds() first.")
  }
  if (is.null(response) && !is.null(stcv_names)) {
    response <- stcv_names$response
  }
  if (!".rowid" %in% names(df)) {
    df$.rowid <- seq_len(nrow(df))
  }

  tb <- make_time_blocks(df, time_var = time_var, k_time = k_time,
                         block_size = block_size)
  df$fold_time <- tb$block_ids
  df <- .stcv_restore_attrs(
    df,
    stcv_names = stcv_names,
    stcv_info = stcv_info,
    spatial_folds_info = spatial_folds_info,
    spatial_folds_object = spatial_folds_object
  )
  tb_space <- .stcv_time_block_space_summary(df, "fold_time", space_names)
  time_folds <- levels(tb$block_ids)
  sp_folds <- sort(unique(as.character(df[[fold_sp_var]])))
  sp_folds <- sp_folds[!is.na(sp_folds)]

  splits <- list()
  split_id <- 1L
  for (tb_i in time_folds) {
    for (sp_i in sp_folds) {
      test_idx <- which(as.character(df$fold_time) == tb_i &
                          as.character(df[[fold_sp_var]]) == sp_i)
      train_idx <- setdiff(seq_len(nrow(df)), test_idx)

      if (drop_na_test && !is.null(response)) {
        test_idx <- test_idx[!is.na(df[[response]][test_idx])]
      }
      if (!is.null(response)) {
        train_idx <- train_idx[!is.na(df[[response]][train_idx])]
      }

      test_times <- sort(unique(df[[time_var]][test_idx]))
      train_times <- sort(unique(df[[time_var]][train_idx]))
      splits[[split_id]] <- list(
        split_id = split_id,
        validation_type = "spatiotemporal_contiguous_block_cv",
        time_test_block = tb_i,
        spatial_test_fold = sp_i,
        spatial_mode = "block_complement",
        train_start = if (length(train_times)) min(train_times) else NA,
        train_end = if (length(train_times)) max(train_times) else NA,
        test_start = if (length(test_times)) min(test_times) else NA,
        test_end = if (length(test_times)) max(test_times) else NA,
        train_times = train_times,
        test_times = test_times,
        train_idx = train_idx,
        test_idx = test_idx,
        train_rowid = df$.rowid[train_idx],
        test_rowid = df$.rowid[test_idx],
        train_n = length(train_idx),
        test_n = length(test_idx)
      )
      split_id <- split_id + 1L
    }
  }

  .stcv_finalize(df, splits, "spatiotemporal_contiguous_block_cv",
                 time_var = time_var, fold_sp_var = fold_sp_var,
                 response = response, time_blocks = tb,
                 time_block_space_summary = tb_space,
                 spatial_mode = "block_complement",
                 spatial_folds = sp_folds, verbose = verbose)
}

make_time_block_prequential <- function(df, time_var = NULL, space_names = NULL,
                                        k_time = NULL,
                                        block_size = NULL,
                                        initial_train_blocks = 1,
                                        horizon_blocks = 1, step_blocks = 1,
                                        window = c("expanding", "rolling"),
                                        rolling_train_blocks = NULL,
                                        response = NULL, drop_na_test = TRUE,
                                        verbose = TRUE) {
  stcv_names <- attr(df, "stcv_names")
  stcv_info <- attr(df, "stcv_info")
  df <- data.frame(df)
  window <- match.arg(window)

  if (is.null(time_var)) {
    if (is.null(stcv_names) || is.null(stcv_names$time)) {
      stop("time_var was not supplied and df has no stcv_names$time attribute.")
    }
    time_var <- stcv_names$time
  }
  if (is.null(response) && !is.null(stcv_names)) {
    response <- stcv_names$response
  }
  if (!".rowid" %in% names(df)) {
    df$.rowid <- seq_len(nrow(df))
  }

  if (is.null(k_time)) {
    if (is.null(block_size)) {
      stop("Supply k_time or block_size.")
    }
    k_time <- 999999L
  }
  tb <- make_time_blocks(df, time_var = time_var, k_time = k_time,
                         block_size = block_size)
  df$fold_time <- tb$block_ids
  df <- .stcv_restore_attrs(df, stcv_names = stcv_names, stcv_info = stcv_info)
  tb_space <- .stcv_time_block_space_summary(df, "fold_time", space_names)
  time_folds <- levels(tb$block_ids)
  n_blocks <- length(time_folds)
  if (n_blocks < initial_train_blocks + horizon_blocks) {
    stop("Not enough temporal blocks for initial_train_blocks + horizon_blocks.")
  }

  if (window == "rolling" && is.null(rolling_train_blocks)) {
    rolling_train_blocks <- initial_train_blocks
  }

  splits <- list()
  split_id <- 1L
  train_end_seq <- seq(from = initial_train_blocks,
                       to = n_blocks - horizon_blocks,
                       by = step_blocks)

  for (train_end in train_end_seq) {
    if (window == "expanding") {
      train_blocks <- time_folds[seq_len(train_end)]
    } else {
      train_start <- train_end - rolling_train_blocks + 1L
      if (train_start < 1L) {
        next
      }
      train_blocks <- time_folds[train_start:train_end]
    }
    test_blocks <- time_folds[(train_end + 1L):(train_end + horizon_blocks)]

    train_idx <- which(as.character(df$fold_time) %in% train_blocks)
    test_idx <- which(as.character(df$fold_time) %in% test_blocks)

    if (drop_na_test && !is.null(response)) {
      test_idx <- test_idx[!is.na(df[[response]][test_idx])]
    }
    if (!is.null(response)) {
      train_idx <- train_idx[!is.na(df[[response]][train_idx])]
    }

    train_times <- sort(unique(df[[time_var]][train_idx]))
    test_times <- sort(unique(df[[time_var]][test_idx]))
    splits[[split_id]] <- list(
      split_id = split_id,
      validation_type = "time_block_prequential",
      time_test_block = paste(test_blocks, collapse = ","),
      spatial_test_fold = NULL,
      spatial_mode = NULL,
      train_start = min(train_times),
      train_end = max(train_times),
      test_start = min(test_times),
      test_end = max(test_times),
      train_times = train_times,
      test_times = test_times,
      train_idx = train_idx,
      test_idx = test_idx,
      train_rowid = df$.rowid[train_idx],
      test_rowid = df$.rowid[test_idx],
      train_n = length(train_idx),
      test_n = length(test_idx)
    )
    split_id <- split_id + 1L
  }

  .stcv_finalize(df, splits, "time_block_prequential",
                 time_var = time_var, response = response,
                 time_blocks = tb,
                 time_block_space_summary = tb_space,
                 verbose = verbose)
}

make_st_contiguous_block_prequential <- function(df, time_var = NULL,
                                                 fold_sp_var = "fold_sp",
                                                 space_names = NULL,
                                                 k_time = NULL,
                                                 block_size = NULL,
                                                 initial_train_blocks = 1,
                                                 horizon_blocks = 1,
                                                 step_blocks = 1,
                                                 window = c("expanding", "rolling"),
                                                 rolling_train_blocks = NULL,
                                                 spatial_mode = c("forecast_known_space", "forecast_new_space"),
                                                 response = NULL,
                                                 drop_na_test = TRUE,
                                                 verbose = TRUE) {
  stcv_names <- attr(df, "stcv_names")
  stcv_info <- attr(df, "stcv_info")
  spatial_folds_info <- attr(df, "spatial_folds_info")
  spatial_folds_object <- attr(df, "spatial_folds_object")
  df <- data.frame(df)
  window <- match.arg(window)
  spatial_mode <- match.arg(spatial_mode)
  if (identical(spatial_mode, "forecast_new_space") && verbose) {
    warning(
      "spatial_mode = 'forecast_new_space' removes the spatial test fold from ",
      "past training blocks. This corresponds to the paper's spatial-region ",
      "removal variation, not the base PtBsC scheme. Use ",
      "spatial_mode = 'forecast_known_space' for PtBsC."
    )
  }

  if (is.null(time_var)) {
    if (is.null(stcv_names) || is.null(stcv_names$time)) {
      stop("time_var was not supplied and df has no stcv_names$time attribute.")
    }
    time_var <- stcv_names$time
  }
  if (!fold_sp_var %in% names(df)) {
    stop("fold_sp_var was not found in df. Run make_spatial_folds() first.")
  }
  if (is.null(response) && !is.null(stcv_names)) {
    response <- stcv_names$response
  }
  if (!".rowid" %in% names(df)) {
    df$.rowid <- seq_len(nrow(df))
  }

  if (is.null(k_time)) {
    if (is.null(block_size)) {
      stop("Supply k_time or block_size.")
    }
    k_time <- 999999L
  }
  tb <- make_time_blocks(df, time_var = time_var, k_time = k_time,
                         block_size = block_size)
  df$fold_time <- tb$block_ids
  df <- .stcv_restore_attrs(
    df,
    stcv_names = stcv_names,
    stcv_info = stcv_info,
    spatial_folds_info = spatial_folds_info,
    spatial_folds_object = spatial_folds_object
  )
  tb_space <- .stcv_time_block_space_summary(df, "fold_time", space_names)
  time_folds <- levels(tb$block_ids)
  n_blocks <- length(time_folds)
  if (n_blocks < initial_train_blocks + horizon_blocks) {
    stop("Not enough temporal blocks for initial_train_blocks + horizon_blocks.")
  }
  sp_folds <- sort(unique(as.character(df[[fold_sp_var]])))
  sp_folds <- sp_folds[!is.na(sp_folds)]

  if (window == "rolling" && is.null(rolling_train_blocks)) {
    rolling_train_blocks <- initial_train_blocks
  }

  splits <- list()
  split_id <- 1L
  train_end_seq <- seq(from = initial_train_blocks,
                       to = n_blocks - horizon_blocks,
                       by = step_blocks)

  for (train_end in train_end_seq) {
    if (window == "expanding") {
      train_blocks <- time_folds[seq_len(train_end)]
    } else {
      train_start <- train_end - rolling_train_blocks + 1L
      if (train_start < 1L) {
        next
      }
      train_blocks <- time_folds[train_start:train_end]
    }
    test_blocks <- time_folds[(train_end + 1L):(train_end + horizon_blocks)]

    for (sp_i in sp_folds) {
      test_idx <- which(as.character(df$fold_time) %in% test_blocks &
                          as.character(df[[fold_sp_var]]) == sp_i)

      if (spatial_mode == "forecast_known_space") {
        train_idx <- which(as.character(df$fold_time) %in% train_blocks)
      } else {
        train_idx <- which(as.character(df$fold_time) %in% train_blocks &
                             as.character(df[[fold_sp_var]]) != sp_i)
      }

      if (drop_na_test && !is.null(response)) {
        test_idx <- test_idx[!is.na(df[[response]][test_idx])]
      }
      if (!is.null(response)) {
        train_idx <- train_idx[!is.na(df[[response]][train_idx])]
      }

      train_times <- sort(unique(df[[time_var]][train_idx]))
      test_times <- sort(unique(df[[time_var]][test_idx]))
      splits[[split_id]] <- list(
        split_id = split_id,
        validation_type = "spatiotemporal_contiguous_block_prequential",
        time_test_block = paste(test_blocks, collapse = ","),
        spatial_test_fold = sp_i,
        spatial_mode = spatial_mode,
        train_start = if (length(train_times)) min(train_times) else NA,
        train_end = if (length(train_times)) max(train_times) else NA,
        test_start = if (length(test_times)) min(test_times) else NA,
        test_end = if (length(test_times)) max(test_times) else NA,
        train_times = train_times,
        test_times = test_times,
        train_idx = train_idx,
        test_idx = test_idx,
        train_rowid = df$.rowid[train_idx],
        test_rowid = df$.rowid[test_idx],
        train_n = length(train_idx),
        test_n = length(test_idx)
      )
      split_id <- split_id + 1L
    }
  }

  .stcv_finalize(df, splits, "spatiotemporal_contiguous_block_prequential",
                 time_var = time_var, fold_sp_var = fold_sp_var,
                 response = response, time_blocks = tb,
                 time_block_space_summary = tb_space,
                 spatial_mode = spatial_mode, spatial_folds = sp_folds,
                 verbose = verbose)
}

blockcv_to_caret <- function(cvsp) {
  if (!is.null(cvsp$folds_list)) {
    if (is.list(cvsp$folds_list[[1]]) &&
        all(c("train", "test") %in% names(cvsp$folds_list[[1]]))) {
      index <- lapply(cvsp$folds_list, function(z) z$train)
      indexOut <- lapply(cvsp$folds_list, function(z) z$test)
    } else {
      if (is.null(cvsp$folds_ids)) {
        stop("Could not find folds_ids to build indexOut.")
      }
      fold_ids <- sort(unique(cvsp$folds_ids))
      indexOut <- lapply(fold_ids, function(f) which(cvsp$folds_ids == f))
      index <- lapply(fold_ids, function(f) which(cvsp$folds_ids != f))
    }
  } else {
    if (is.null(cvsp$folds_ids)) {
      stop("The cv_spatial object must contain folds_list or folds_ids.")
    }
    fold_ids <- sort(unique(cvsp$folds_ids))
    indexOut <- lapply(fold_ids, function(f) which(cvsp$folds_ids == f))
    index <- lapply(fold_ids, function(f) which(cvsp$folds_ids != f))
  }

  names(index) <- paste0("Fold", seq_along(index))
  names(indexOut) <- names(index)
  list(index = index, indexOut = indexOut)
}

stcv_to_caret <- function(cv_obj, drop_empty = TRUE) {
  if (is.null(cv_obj$splits) || length(cv_obj$splits) == 0) {
    stop("cv_obj has no splits.")
  }

  splits <- cv_obj$splits
  if (drop_empty) {
    keep <- vapply(splits, function(x) length(x$train_idx) > 0 && length(x$test_idx) > 0,
                   logical(1))
    splits <- splits[keep]
  }
  if (length(splits) == 0) {
    stop("No non-empty split is available.")
  }

  data_out <- cv_obj$data
  row_index_var <- if (".rowid_original" %in% names(data_out)) ".rowid_original" else NULL
  index <- lapply(splits, function(sp) {
    if (is.null(row_index_var)) {
      sp$train_idx
    } else {
      data_out[[row_index_var]][sp$train_idx]
    }
  })
  indexOut <- lapply(splits, function(sp) {
    if (is.null(row_index_var)) {
      sp$test_idx
    } else {
      data_out[[row_index_var]][sp$test_idx]
    }
  })
  split_names <- vapply(splits, function(x) {
    time_part <- if (!is.null(x$split_time_id)) {
      paste0("T", x$split_time_id)
    } else if (!is.null(x$time_test_block)) {
      paste0("T", gsub("[^A-Za-z0-9]+", "_", x$time_test_block))
    } else {
      paste0("Split", x$split_id)
    }
    space_part <- if (!is.null(x$spatial_test_fold)) {
      sp_part <- as.character(x$spatial_test_fold)
      if (!grepl("^S", sp_part)) {
        sp_part <- paste0("S", sp_part)
      }
      paste0("_", sp_part)
    } else {
      ""
    }
    paste0(time_part, space_part)
  }, character(1))
  names(index) <- make.unique(split_names)
  names(indexOut) <- names(index)

  if (!"fold_time" %in% names(data_out)) {
    data_out$fold_time <- factor(rep("T1", nrow(data_out)))
  }
  if (!is.null(cv_obj$fold_sp_var) && cv_obj$fold_sp_var %in% names(data_out)) {
    data_out$fold_sp <- data_out[[cv_obj$fold_sp_var]]
  } else if (!"fold_sp" %in% names(data_out)) {
    data_out$fold_sp <- factor(rep("S1", nrow(data_out)))
  }

  split_labels <- .stcv_split_labels(splits)
  split_membership <- do.call(rbind, lapply(seq_along(splits), function(i) {
    sp <- splits[[i]]
    used <- rep("Not used", nrow(data_out))
    used[sp$train_idx] <- "Train"
    used[sp$test_idx] <- "Test"
    data.frame(
      split = names(index)[i],
      split_label = split_labels[i],
      split_id = sp$split_id,
      row_id = seq_len(nrow(data_out)),
      rowid_original = if (".rowid_original" %in% names(data_out)) {
        data_out$.rowid_original
      } else {
        NA_integer_
      },
      fold_time = as.character(data_out$fold_time),
      fold_sp = as.character(data_out$fold_sp),
      used = used,
      stringsAsFactors = FALSE
    )
  }))
  rownames(split_membership) <- NULL
  split_membership$used <- factor(split_membership$used,
                                  levels = c("Train", "Test", "Not used"))

  list(
    index = index,
    indexOut = indexOut,
    data = data_out,
    split_membership = split_membership,
    splits_df = cv_obj$splits_df,
    validation_type = cv_obj$validation_type,
    time_var = cv_obj$time_var,
    fold_sp_var = cv_obj$fold_sp_var,
    response = cv_obj$response
  )
}

make_st_validation <- function(df, spattime_names = c("Lon", "Lat", "Tempo"),
                               response = NULL,
                               validation_type = c(
                                 "time_block_cv",
                                 "spatiotemporal_contiguous_block_cv",
                                 "time_block_prequential",
                                 "spatiotemporal_contiguous_block_prequential",
                                 "forecast_origin"
                               ),
                               k_spat = 5,
                               spatial_selection = c("random", "systematic", "checkerboard", "contiguous"),
                               k_time = 5,
                               time_block_size = NULL,
                               initial_train_blocks = 1,
                               horizon_blocks = 1,
                               step_blocks = 1,
                               initial_window = NULL,
                               horizon = 1,
                               step = 1,
                               temporal_window = c("expanding", "rolling"),
                               rolling_train_blocks = NULL,
                               window_size = NULL,
                               spatial_mode = c("forecast_known_space", "forecast_new_space"),
                               cellsize = NULL,
                               cellsize_units = c("meters", "native"),
                               spatial_crs = 4326,
                               spatial_work_crs = 3857,
                               drop_na_test = TRUE,
                               verbose = TRUE,
                               ...) {
  validation_type <- match.arg(validation_type)
  spatial_selection <- match.arg(spatial_selection)
  temporal_window <- match.arg(temporal_window)
  spatial_mode <- match.arg(spatial_mode)
  cellsize_units <- match.arg(cellsize_units)

  dat <- prepare_stcv_data(
    df = df,
    spattime_names = spattime_names,
    response = response
  )
  space_names <- spattime_names[1:2]

  if (validation_type == "time_block_cv") {
    return(make_time_block_cv(
      df = dat,
      space_names = space_names,
      k_time = k_time,
      block_size = time_block_size,
      response = response,
      drop_na_test = drop_na_test,
      verbose = verbose
    ))
  }

  if (validation_type == "time_block_prequential") {
    return(make_time_block_prequential(
      df = dat,
      space_names = space_names,
      k_time = k_time,
      block_size = time_block_size,
      initial_train_blocks = initial_train_blocks,
      horizon_blocks = horizon_blocks,
      step_blocks = step_blocks,
      window = temporal_window,
      rolling_train_blocks = rolling_train_blocks,
      response = response,
      drop_na_test = drop_na_test,
      verbose = verbose
    ))
  }

  dat <- make_spatial_folds(
    df = dat,
    k_spat = k_spat,
    selection = spatial_selection,
    var_name = response,
    cellsize = cellsize,
    cellsize_units = cellsize_units,
    spatial_crs = spatial_crs,
    spatial_work_crs = spatial_work_crs,
    ...
  )

  if (validation_type == "spatiotemporal_contiguous_block_cv") {
    return(make_st_contiguous_block_cv(
      df = dat,
      space_names = space_names,
      k_time = k_time,
      block_size = time_block_size,
      response = response,
      drop_na_test = drop_na_test,
      verbose = verbose
    ))
  }

  if (validation_type == "spatiotemporal_contiguous_block_prequential") {
    return(make_st_contiguous_block_prequential(
      df = dat,
      space_names = space_names,
      k_time = k_time,
      block_size = time_block_size,
      initial_train_blocks = initial_train_blocks,
      horizon_blocks = horizon_blocks,
      step_blocks = step_blocks,
      window = temporal_window,
      rolling_train_blocks = rolling_train_blocks,
      spatial_mode = spatial_mode,
      response = response,
      drop_na_test = drop_na_test,
      verbose = verbose
    ))
  }

  if (is.null(initial_window)) {
    initial_window <- initial_train_blocks
  }

  origins <- make_time_origins(
    df = dat,
    initial_window = initial_window,
    horizon = horizon,
    step = step,
    window = temporal_window,
    window_size = window_size
  )

  make_st_forecast_splits(
    df = dat,
    time_origins = origins,
    spatial_mode = spatial_mode,
    response = response,
    drop_na_test = drop_na_test,
    verbose = verbose
  )
}


generate_grid_patch <- function(nx = 20, ny = 20,
                                xlim = c(-50, -41),
                                ylim = c(-30, -21),
                                centroids = NULL,
                                sd_patch = NULL,
                                noise_sd = 0.03,
                                phi = 0.6,
                                seed = 123) {

  set.seed(seed)

  if (is.null(centroids)) {
    centroids <- rbind(
      c(-48, -27.5),
      c(-48, -26.0),
      c(-48, -25.0),
      c(-48, -23.0),
      c(-47, -23.0),
      c(-46, -23.0),
      c(-43, -23.0),
      c(-43, -25.0),
      c(-43, -26.0),
      c(-43, -27.5),
      c(-45, -27.5)
    )
  }

  n_time <- nrow(centroids)
  grids <- vector("list", n_time)

  prev_npp <- NULL

  for (tt in seq_len(n_time)) {

    g <- generate_grid_mancha(
      nx = nx,
      ny = ny,
      xlim = xlim,
      ylim = ylim,
      centroid = centroids[tt, ],
      sd_patch = sd_patch,
      noise_sd = noise_sd,
      scale01 = FALSE,
      Tempo = tt
    )

    # memória temporal
    if (!is.null(prev_npp)) {
      g$NPP <- phi * prev_npp + (1 - phi) * g$NPP
    }

    prev_npp <- g$NPP
    grids[[tt]] <- g
  }

  grid <- do.call(rbind, grids)
  rownames(grid) <- NULL
  grid
}
generate_grid_mancha <- function(nx = 10, ny = 10,
                                 xlim = c(-50, -41),
                                 ylim = c(-30, -21),
                                 centroid = NULL,
                                 sd_patch = NULL,
                                 noise_sd = 0.05,
                                 scale01 = FALSE,
                                 Tempo = 1) {

  lon_seq <- seq(xlim[1], xlim[2], length.out = nx)
  lat_seq <- seq(ylim[1], ylim[2], length.out = ny)

  grid <- expand.grid(
    Lon = lon_seq,
    Lat = lat_seq
  )

  if (is.null(centroid)) {
    centroid <- c(mean(xlim), mean(ylim))
  }

  if (is.null(sd_patch)) {
    sd_patch <- min(diff(xlim), diff(ylim)) / 5
  }

  d2 <- (grid$Lon - centroid[1])^2 + (grid$Lat - centroid[2])^2

  grid$NPP <- exp(-d2 / (2 * sd_patch^2))
  grid$NPP <- grid$NPP + rnorm(nrow(grid), mean = 0, sd = noise_sd)

  if (scale01) {
    rng <- range(grid$NPP, na.rm = TRUE)
    grid$NPP <- (grid$NPP - rng[1]) / diff(rng)
  }

  grid$Tempo <- Tempo
  grid
}



plot_folds <- function(result, fold_sp = NULL, fold_time = NULL, split_id = NULL,
                       split_label = NULL,
                       col_train = "steelblue", col_test = "orange",
                       col_unused = "pink", title = NULL,
                       verbose = TRUE, opacity_unused = 0.35,
                       opacity_train = 0.95, opacity_test = 0.95,
                       marker_size = 4, file = NULL,
                       type = c("real", "simplified"),
                       simplified_n = 15) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("The 'plotly' package is required for plot_folds().")
  }
  type <- match.arg(type)
  res <- result$data
  if (is.null(res)) {
    stop("result$data was not found.")
  }
  if (!all(c("fold_sp", "fold_time") %in% names(res))) {
    stop("result$data must contain fold_sp and fold_time.")
  }
  if (is.null(result$splits) || length(result$splits) == 0) {
    stop("result$splits was not found.")
  }

  norm_sp <- function(x) {
    x <- as.character(x)
    x[!is.na(x) & !grepl("^S", x)] <- paste0("S", x[!is.na(x) & !grepl("^S", x)])
    x
  }
  norm_time <- function(x) as.character(x)

  res$fold_sp <- norm_sp(res$fold_sp)
  res$fold_time <- norm_time(res$fold_time)

  if (!is.null(fold_sp)) {
    fold_sp <- norm_sp(fold_sp)
    if (!any(res$fold_sp == fold_sp, na.rm = TRUE)) {
      stop("Spatial fold not found.")
    }
  }
  if (!is.null(fold_time)) {
    fold_time <- norm_time(fold_time)
    if (!any(res$fold_time == fold_time, na.rm = TRUE)) {
      stop("Time fold not found.")
    }
  }

  if (is.null(split_label) && !is.null(fold_time) &&
      !is.null(result$validation_type) && grepl("prequential", result$validation_type)) {
    split_label <- fold_time
  }

  if (is.null(split_id)) {
    is_prequential <- !is.null(result$validation_type) &&
      grepl("prequential", result$validation_type)
    if (is_prequential && !is.null(split_label)) {
      available_labels <- paste0("T", seq_along(unique(vapply(result$splits, function(sp) {
        if (is.null(sp$time_test_block)) paste0("split_", sp$split_id) else as.character(sp$time_test_block)
      }, character(1)))))
      if (!is.null(result$split_membership) &&
          "split_label" %in% names(result$split_membership)) {
        available_labels <- unique(as.character(result$split_membership$split_label))
        split_candidates <- unique(result$split_membership$split_id[
          as.character(result$split_membership$split_label) == split_label
        ])
        split_candidates <- split_candidates[!is.na(split_candidates)]
        has_split_sp <- any(vapply(result$splits, function(sp) {
          !is.null(sp$spatial_test_fold) && !is.na(sp$spatial_test_fold)
        }, logical(1)))
        if (!is.null(fold_sp) && has_split_sp && length(split_candidates) > 0) {
          split_candidates <- split_candidates[vapply(split_candidates, function(id) {
            sp_i <- result$splits[[match(id, vapply(result$splits, `[[`, integer(1), "split_id"))]]
            !is.null(sp_i$spatial_test_fold) && norm_sp(sp_i$spatial_test_fold) == fold_sp
          }, logical(1))]
        }
        if (length(split_candidates) > 0) {
          split_id <- split_candidates[1]
        }
      }
      if (is.null(split_id)) {
        stop(
          "No prequential temporal origin was found for split_label = '", split_label,
          "'. Available origins: ", paste(available_labels, collapse = ", "),
          call. = FALSE
        )
      }
    }
    if (is.null(split_id)) {
      matches <- seq_along(result$splits)
      if (!is.null(fold_time)) {
        matches <- matches[vapply(result$splits[matches], function(sp) {
          fold_time %in% strsplit(as.character(sp$time_test_block), ",", fixed = TRUE)[[1]]
        }, logical(1))]
      }
      has_split_sp <- any(vapply(result$splits, function(sp) {
        !is.null(sp$spatial_test_fold) && !is.na(sp$spatial_test_fold)
      }, logical(1)))
      if (!is.null(fold_sp) && has_split_sp) {
        matches <- matches[vapply(result$splits[matches], function(sp) {
          !is.null(sp$spatial_test_fold) && norm_sp(sp$spatial_test_fold) == fold_sp
        }, logical(1))]
      }
      if (length(matches) == 0) {
        stop("No split was found where the requested fold_sp/fold_time is used.")
      }
      split_id <- matches[1]
    }
  }

  sp <- result$splits[[split_id]]
  if (!is.null(result$split_membership) &&
      all(c("split_id", "row_id", "used") %in% names(result$split_membership))) {
    memb <- result$split_membership[result$split_membership$split_id == sp$split_id, , drop = FALSE]
    status <- rep("Not used", nrow(res))
    status[memb$row_id] <- as.character(memb$used)
  } else {
    status <- rep("Not used", nrow(res))
    status[sp$train_idx] <- "Train"
    status[sp$test_idx] <- "Test"
  }
  status <- factor(status, levels = c("Train", "Test", "Not used"))
  color_status <- c("Train" = col_train, "Test" = col_test, "Not used" = col_unused)
  res$.status <- status
  res$.color <- unname(color_status[as.character(status)])
  aux_by_marker <- ".coords_source" %in% names(res) &&
    any(as.character(res$.coords_source) == "auxiliary", na.rm = TRUE) &&
    !any(as.character(res$.coords_source) == "real", na.rm = TRUE)
  aux_by_shape <- FALSE
  if (!aux_by_marker && all(c("Lon", "Lat") %in% names(res))) {
    lon_num <- suppressWarnings(as.numeric(res$Lon))
    lat_num <- suppressWarnings(as.numeric(res$Lat))
    lon_unique <- sort(unique(lon_num[is.finite(lon_num)]))
    lat_unique <- unique(lat_num[is.finite(lat_num)])
    aux_by_shape <- length(lat_unique) == 1L &&
      length(lon_unique) == nrow(res) &&
      identical(lon_unique, as.numeric(seq_len(nrow(res))))
  }
  has_real_coords <- !(aux_by_marker || aux_by_shape)
  if (verbose) {
    message("plot_folds selected split_id = ", sp$split_id)
    print(table(res$fold_time, res$.status))
  }
  if (is.null(title)) {
    title <- paste0(
      if (!is.null(result$validation_type)) result$validation_type else "spatio-temporal validation",
      " | split ", split_id
    )
  }

  if (identical(type, "simplified")) {
    n_axis <- max(2L, as.integer(simplified_n))
    time_values <- sort(unique(as.character(res$fold_time)))
    space_values <- sort(unique(as.character(res$fold_sp)))
    space_values <- space_values[!is.na(space_values)]
    if (length(space_values) == 0) {
      space_values <- "S1"
    }
    tempo_real_values <- sort(unique(res$Tempo))
    if (length(tempo_real_values) != length(time_values)) {
      tempo_values <- seq(min(res$Tempo, na.rm = TRUE), max(res$Tempo, na.rm = TRUE),
                          length.out = length(time_values))
    } else {
      tempo_values <- tempo_real_values
    }
    if (has_real_coords) {
      lon_rng <- range(res$Lon, na.rm = TRUE)
      lat_rng <- range(res$Lat, na.rm = TRUE)
      if (!all(is.finite(lon_rng)) || diff(lon_rng) == 0) {
        lon_rng <- c(0, length(space_values))
      }
      if (!all(is.finite(lat_rng)) || diff(lat_rng) == 0) {
        lat_rng <- c(0, 1)
      }
      x_breaks <- seq(lon_rng[1], lon_rng[2], length.out = length(space_values) + 1L)
      y_seq <- seq(lat_rng[1], lat_rng[2], length.out = n_axis)
    } else {
      x_breaks <- seq(0.5, length(time_values) + 0.5, length.out = length(time_values) + 1L)
      y_seq <- seq(0, 1, length.out = n_axis)
      space_values <- "S1"
    }

    simplified <- do.call(rbind, lapply(seq_along(time_values), function(ti) {
      do.call(rbind, lapply(seq_along(space_values), function(si) {
        x_i <- if (has_real_coords) si else ti
        x_seq <- seq(x_breaks[x_i], x_breaks[x_i + 1L], length.out = n_axis)
        grid <- expand.grid(x = x_seq, y = y_seq)
        idx <- which(res$fold_time == time_values[ti] & res$fold_sp == space_values[si])
        status_i <- if (length(idx) == 0) {
          "Not used"
        } else {
          tab <- table(as.character(res$.status[idx]))
          names(tab)[which.max(tab)]
        }
        data.frame(
          Lon = grid$x,
          Lat = grid$y,
          Tempo = tempo_values[ti],
          fold_sp = space_values[si],
          fold_time = time_values[ti],
          .status = factor(status_i, levels = c("Train", "Test", "Not used"))
        )
      }))
    }))
    res <- simplified
  }

  hover_text <- paste0(
    "Lon: ", res$Lon,
    "<br>Lat: ", res$Lat,
    "<br>Tempo: ", res$Tempo,
    "<br>fold_sp: ", res$fold_sp,
    "<br>fold_time: ", res$fold_time,
    "<br>Status: ", res$.status
  )
  res$.hover <- hover_text
  plot_2d <- !has_real_coords
  if (plot_2d) {
    if (identical(type, "simplified")) {
      res$plot_x <- res$Lon
      res$plot_y <- res$Lat
      x_title <- "Temporal block"
      y_title <- ""
      res$.hover <- paste0(
        "Temporal block: ", res$fold_time,
        "<br>Tempo: ", res$Tempo,
        "<br>Status: ", res$.status
      )
    } else {
      res$plot_x <- res$Tempo
      res$plot_y <- if (".rowid_original" %in% names(res)) res$.rowid_original else seq_len(nrow(res))
      x_title <- "Tempo"
      y_title <- "Observation"
      res$.hover <- paste0(
        "Tempo: ", res$Tempo,
        "<br>Observation: ", res$plot_y,
        "<br>fold_time: ", res$fold_time,
        "<br>Status: ", res$.status
      )
    }
  }

  add_status_trace <- function(fig, status_name, col, alpha) {
    dat <- res[as.character(res$.status) == status_name, , drop = FALSE]
    if (plot_2d) {
      plotly::add_trace(
        fig,
        data = dat,
        x = ~plot_x,
        y = ~plot_y,
        type = "scatter",
        mode = "markers",
        name = status_name,
        marker = list(color = col, opacity = alpha, size = marker_size + 2),
        text = ~.hover,
        hoverinfo = "text",
        inherit = FALSE
      )
    } else {
      plotly::add_trace(
        fig,
        data = dat,
        x = ~Lon,
        y = ~Lat,
        z = ~Tempo,
        type = "scatter3d",
        mode = "markers",
        name = status_name,
        marker = list(color = col, opacity = alpha, size = marker_size),
        text = ~.hover,
        hoverinfo = "text",
        inherit = FALSE
      )
    }
  }

  fig <- plotly::plot_ly()
  fig <- add_status_trace(fig, "Not used", col_unused, opacity_unused)
  fig <- add_status_trace(fig, "Train", col_train, opacity_train)
  fig <- add_status_trace(fig, "Test", col_test, opacity_test)
  fig <- plotly::layout(fig, title = list(text = title))
  if (plot_2d) {
    fig <- plotly::layout(fig, xaxis = list(title = x_title), yaxis = list(title = y_title))
  }

  if (!is.null(file)) {
    if (!requireNamespace("tools", quietly = TRUE)) {
      stop("The 'tools' package is required to inspect file extensions.")
    }
    file <- normalizePath(file, winslash = "/", mustWork = FALSE)
    ext <- tolower(tools::file_ext(file))
    if (!identical(ext, "png")) {
      stop("file must have a .png extension.")
    }
    out_dir <- dirname(file)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    if (!requireNamespace("htmlwidgets", quietly = TRUE) ||
        !requireNamespace("webshot2", quietly = TRUE)) {
      stop(
        "Saving plot_folds() to PNG without Python requires the 'htmlwidgets' ",
        "and 'webshot2' packages.",
        call. = FALSE
      )
    }
    tmp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(fig, file = tmp_html, selfcontained = TRUE)
    webshot2::webshot(
      url = normalizePath(tmp_html, winslash = "/", mustWork = TRUE),
      file = file
    )
    saved <- TRUE
    invisible(saved)
  }

  fig
}

plot_spatiotemporal_scheme_preview <- function(
    validation_type = c("spatiotemporal_contiguous_block_cv",
                        "spatiotemporal_contiguous_block_prequential"),
    k_spat = 4,
    k_time = 5,
    initial_train_blocks = 2,
    horizon_blocks = 1,
    step_blocks = 1,
    temporal_window = c("expanding", "rolling"),
    rolling_train_blocks = NULL,
    spatial_mode = c("forecast_new_space", "forecast_known_space"),
    nx = 12,
    ny = 5,
    nz = 4,
    max_panels = 6,
    train_col = "#b8b6d9",
    test_col  = "#e66101",
    unused_col = "white",
    grid_col  = "#66666655",
    border_col = "black",
    gap = 2,
    show_legend = TRUE,
    title = NULL) {

  validation_type <- match.arg(validation_type)
  temporal_window <- match.arg(temporal_window)
  spatial_mode <- match.arg(spatial_mode)
  k_spat <- max(1L, as.integer(k_spat))
  k_time <- max(2L, as.integer(k_time))
  initial_train_blocks <- max(1L, as.integer(initial_train_blocks))
  horizon_blocks <- max(1L, as.integer(horizon_blocks))
  step_blocks <- max(1L, as.integer(step_blocks))
  nx <- max(k_time, as.integer(nx))
  ny <- max(k_spat, as.integer(ny))
  nz <- max(2L, as.integer(nz))
  max_panels <- max(1L, as.integer(max_panels))

  if (is.null(title)) {
    title <- if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      "Spatiotemporal block CV"
    } else {
      paste0("Prequential spatiotemporal CV: ", temporal_window)
    }
  }

  time_id <- ceiling(seq_len(nx) * k_time / nx)
  time_cells <- split(seq_len(nx), time_id)
  space_id <- ceiling(seq_len(ny) * k_spat / ny)
  space_cells <- split(seq_len(ny), space_id)

  make_status <- function(panel_i) {
    status <- matrix("Train", nrow = nx, ncol = ny)
    if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      pair_grid <- expand.grid(time = seq_len(k_time), space = seq_len(k_spat))
      pair <- pair_grid[panel_i, , drop = FALSE]
      status[time_cells[[pair$time]], space_cells[[pair$space]]] <- "Test"
      return(status)
    }

    train_end_seq <- seq(from = initial_train_blocks,
                         to = k_time - horizon_blocks,
                         by = step_blocks)
    if (length(train_end_seq) == 0) {
      return(matrix("Not used", nrow = nx, ncol = ny))
    }
    origin <- train_end_seq[panel_i]
    status[,] <- "Not used"
    if (identical(temporal_window, "rolling")) {
      if (is.null(rolling_train_blocks) || is.na(rolling_train_blocks)) {
        rolling_train_blocks <- initial_train_blocks
      }
      train_start <- max(1L, origin - rolling_train_blocks + 1L)
      train_blocks <- train_start:origin
    } else {
      train_blocks <- seq_len(origin)
    }
    test_blocks <- (origin + 1L):(origin + horizon_blocks)
    test_blocks <- test_blocks[test_blocks <= k_time]
    test_space <- ((panel_i - 1L) %% k_spat) + 1L

    status[unlist(time_cells[train_blocks]), ] <- "Train"
    if (identical(spatial_mode, "forecast_known_space")) {
      status[unlist(time_cells[test_blocks]), space_cells[[test_space]]] <- "Test"
    } else {
      status[unlist(time_cells[test_blocks]), space_cells[[test_space]]] <- "Test"
    }
    status
  }

  n_panels <- if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
    min(k_time * k_spat, max_panels)
  } else {
    n_origins <- floor((k_time - initial_train_blocks - horizon_blocks) / step_blocks) + 1L
    min(max(0L, n_origins), max_panels)
  }
  if (n_panels < 1L) {
    n_panels <- 1L
  }

  ax <- 0.45
  ay <- 0.28

  proj <- function(x, y, z, offset_x = 0, offset_y = 0) {
    list(x = x + ax * y + offset_x, y = z + ay * y + offset_y)
  }

  draw_poly <- function(x, y, z, col, offset_x = 0, offset_y = 0, border = grid_col) {
    p <- proj(x, y, z, offset_x, offset_y)
    polygon(p$x, p$y, col = col, border = border, lwd = 0.35)
  }

  draw_edge <- function(x, y, z, offset_x = 0, offset_y = 0) {
    p <- proj(x, y, z, offset_x, offset_y)
    lines(p$x, p$y, col = border_col, lwd = 1.1)
  }

  block_width <- nx + ax * ny
  block_height <- nz + ay * ny
  offsets_y <- rev(seq_len(n_panels) - 1) * (block_height + gap)
  xmax <- block_width
  ymax <- max(offsets_y) + block_height

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mar = c(1.2, 0.5, 2.6, 0.5), xpd = NA)
  plot(NA, NA,
       xlim = c(-0.5, xmax + 0.5),
       ylim = c(ifelse(show_legend, -1.4, -0.3), ymax + 0.7),
       asp = 1, axes = FALSE, xlab = "", ylab = "", frame.plot = FALSE)
  text(0, ymax + 0.45, title, adj = c(0, 0.5), font = 2, cex = 0.95)

  status_col <- c("Train" = train_col, "Test" = test_col, "Not used" = unused_col)

  for (f in seq_len(n_panels)) {
    offset_y <- offsets_y[f]
    status <- make_status(f)

    for (j in seq_len(ny)) {
      for (i in seq_len(nx)) {
        draw_poly(
          x = c(i - 1, i, i, i - 1),
          y = c(j - 1, j - 1, j, j),
          z = c(nz, nz, nz, nz),
          col = status_col[[status[i, j]]],
          offset_y = offset_y
        )
      }
    }

    for (j in seq_len(ny)) {
      for (l in seq_len(nz)) {
        col <- status_col[[status[nx, j]]]
        draw_poly(
          x = c(nx, nx, nx, nx),
          y = c(j - 1, j, j, j - 1),
          z = c(l - 1, l - 1, l, l),
          col = col,
          offset_y = offset_y
        )
      }
    }

    for (i in seq_len(nx)) {
      for (l in seq_len(nz)) {
        col <- status_col[[status[i, 1]]]
        draw_poly(
          x = c(i - 1, i, i, i - 1),
          y = c(0, 0, 0, 0),
          z = c(l - 1, l - 1, l, l),
          col = col,
          offset_y = offset_y
        )
      }
    }

    draw_edge(c(0, nx, nx, 0, 0), c(0, 0, 0, 0, 0), c(0, 0, nz, nz, 0), offset_y = offset_y)
    draw_edge(c(0, nx, nx, 0, 0), c(0, 0, ny, ny, 0), c(nz, nz, nz, nz, nz), offset_y = offset_y)
    draw_edge(c(nx, nx, nx, nx, nx), c(0, ny, ny, 0, 0), c(0, 0, nz, nz, 0), offset_y = offset_y)

    label <- if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      pair_grid <- expand.grid(time = seq_len(k_time), space = seq_len(k_spat))
      paste0("S", pair_grid$space[f], "-T", pair_grid$time[f])
    } else {
      paste0("O", f)
    }
    p <- proj(nx / 2, -0.15, -0.25, offset_y = offset_y)
    text(p$x, p$y, label, cex = 0.65)
  }

  if (show_legend) {
    legend("bottomleft",
           legend = c("Train", "Test", "Not used"),
           fill = c(train_col, test_col, unused_col),
           border = c(NA, NA, border_col),
           bty = "n", horiz = TRUE, x.intersp = 0.7, cex = 0.8)
  }
  invisible(TRUE)
}

plot_spatiotemporal_scheme_preview_ggplot <- function(
    validation_type = c("spatiotemporal_contiguous_block_cv",
                        "spatiotemporal_contiguous_block_prequential"),
    k_spat = 4,
    k_time = 5,
    initial_train_blocks = 2,
    horizon_blocks = 1,
    step_blocks = 1,
    temporal_window = c("expanding", "rolling"),
    rolling_train_blocks = NULL,
    spatial_mode = c("forecast_new_space", "forecast_known_space"),
    nx = 12,
    ny = 5,
    nz = 4,
    max_panels = 6,
    train_col = "#b8b6d9",
    test_col  = "#e66101",
    unused_col = "white",
    grid_col  = "gray20",
    border_col = "black",
    title = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plot_spatiotemporal_scheme_preview_ggplot().")
  }

  validation_type <- match.arg(validation_type)
  temporal_window <- match.arg(temporal_window)
  spatial_mode <- match.arg(spatial_mode)
  k_spat <- max(1L, as.integer(k_spat))
  k_time <- max(2L, as.integer(k_time))
  initial_train_blocks <- max(1L, as.integer(initial_train_blocks))
  horizon_blocks <- max(1L, as.integer(horizon_blocks))
  step_blocks <- max(1L, as.integer(step_blocks))
  nx <- max(k_time, as.integer(nx))
  ny <- max(k_spat, as.integer(ny))
  nz <- max(2L, as.integer(nz))
  max_panels <- max(1L, as.integer(max_panels))

  if (is.null(title)) {
    title <- if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      "Spatiotemporal block CV"
    } else {
      paste0("Prequential spatiotemporal CV: ", temporal_window)
    }
  }

  time_id <- ceiling(seq_len(nx) * k_time / nx)
  time_cells <- split(seq_len(nx), time_id)
  space_id <- ceiling(seq_len(ny) * k_spat / ny)
  space_cells <- split(seq_len(ny), space_id)
  train_end_seq <- seq(from = initial_train_blocks,
                       to = k_time - horizon_blocks,
                       by = step_blocks)
  n_origins <- length(train_end_seq)

  prequential_panel_info <- function(panel_i) {
    origin_i <- ((panel_i - 1L) %/% k_spat) + 1L
    space_i <- ((panel_i - 1L) %% k_spat) + 1L
    origin <- train_end_seq[origin_i]
    test_blocks <- (origin + 1L):(origin + horizon_blocks)
    test_blocks <- test_blocks[test_blocks <= k_time]
    list(
      origin_i = origin_i,
      space_i = space_i,
      origin = origin,
      test_blocks = test_blocks
    )
  }

  make_status <- function(panel_i) {
    status <- matrix("Train", nrow = nx, ncol = ny)
    if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      pair_grid <- expand.grid(time = seq_len(k_time), space = seq_len(k_spat))
      pair <- pair_grid[panel_i, , drop = FALSE]
      status[time_cells[[pair$time]], space_cells[[pair$space]]] <- "Test"
      return(status)
    }

    if (n_origins == 0) {
      return(matrix("Not used", nrow = nx, ncol = ny))
    }
    panel_info <- prequential_panel_info(panel_i)
    origin <- panel_info$origin
    status[,] <- "Not used"
    if (identical(temporal_window, "rolling")) {
      if (is.null(rolling_train_blocks) || is.na(rolling_train_blocks)) {
        rolling_train_blocks <- initial_train_blocks
      }
      train_start <- max(1L, origin - rolling_train_blocks + 1L)
      train_blocks <- train_start:origin
    } else {
      train_blocks <- seq_len(origin)
    }
    test_blocks <- panel_info$test_blocks
    test_space <- panel_info$space_i
    status[unlist(time_cells[train_blocks]), ] <- "Train"
    status[unlist(time_cells[test_blocks]), space_cells[[test_space]]] <- "Test"
    status
  }

  n_panels <- if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
    min(k_time * k_spat, max_panels)
  } else {
    min(max(0L, n_origins * k_spat), max_panels)
  }
  if (n_panels < 1L) {
    n_panels <- 1L
  }

  ax <- 0.45
  ay <- 0.28
  project <- function(x, y, z) {
    data.frame(px = x + ax * y, py = z + ay * y)
  }

  panel_label <- function(i) {
    if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      pair_grid <- expand.grid(time = seq_len(k_time), space = seq_len(k_spat))
      paste0("S", pair_grid$space[i], "-T", pair_grid$time[i])
    } else {
      info <- prequential_panel_info(i)
      if (length(info$test_blocks) == 0 || is.na(info$origin)) {
        paste0("O", info$origin_i, "-S", info$space_i)
      } else {
        paste0("T", info$origin_i, " | S", info$space_i, " | T", paste(info$test_blocks, collapse = "+"))
      }
    }
  }

  poly_rows <- list()
  edge_rows <- list()
  id <- 1L
  edge_id <- 1L
  for (panel_i in seq_len(n_panels)) {
    status <- make_status(panel_i)
    facet <- factor(panel_label(panel_i), levels = vapply(seq_len(n_panels), panel_label, character(1)))

    add_poly <- function(x, y, z, fill) {
      p <- project(x, y, z)
      p$panel <- facet
      p$status <- fill
      p$poly_id <- id
      id <<- id + 1L
      poly_rows[[length(poly_rows) + 1L]] <<- p
    }
    add_edge <- function(x, y, z) {
      p <- project(x, y, z)
      p$panel <- facet
      p$edge_id <- edge_id
      edge_id <<- edge_id + 1L
      edge_rows[[length(edge_rows) + 1L]] <<- p
    }

    for (j in seq_len(ny)) {
      for (i in seq_len(nx)) {
        add_poly(
          x = c(i - 1, i, i, i - 1),
          y = c(j - 1, j - 1, j, j),
          z = c(nz, nz, nz, nz),
          fill = status[i, j]
        )
      }
    }

    for (j in seq_len(ny)) {
      for (l in seq_len(nz)) {
        add_poly(
          x = c(nx, nx, nx, nx),
          y = c(j - 1, j, j, j - 1),
          z = c(l - 1, l - 1, l, l),
          fill = status[nx, j]
        )
      }
    }

    for (i in seq_len(nx)) {
      for (l in seq_len(nz)) {
        add_poly(
          x = c(i - 1, i, i, i - 1),
          y = c(0, 0, 0, 0),
          z = c(l - 1, l - 1, l, l),
          fill = status[i, 1]
        )
      }
    }

    add_edge(c(0, nx, nx, 0, 0), c(0, 0, 0, 0, 0), c(0, 0, nz, nz, 0))
    add_edge(c(0, nx, nx, 0, 0), c(0, 0, ny, ny, 0), c(nz, nz, nz, nz, nz))
    add_edge(c(nx, nx, nx, nx, nx), c(0, ny, ny, 0, 0), c(0, 0, nz, nz, 0))
  }

  poly_df <- do.call(rbind, poly_rows)
  edge_df <- do.call(rbind, edge_rows)
  status_levels <- if (identical(validation_type, "spatiotemporal_contiguous_block_prequential")) {
    c("Train", "Test", "Not used")
  } else {
    c("Train", "Test")
  }
  poly_df$status <- factor(poly_df$status, levels = status_levels)

  ggplot2::ggplot(poly_df, ggplot2::aes(.data$px, .data$py, group = .data$poly_id, fill = .data$status)) +
    ggplot2::geom_polygon(color = grid_col, linewidth = 0.15) +
    ggplot2::geom_path(data = edge_df, ggplot2::aes(.data$px, .data$py, group = .data$edge_id),
                       inherit.aes = FALSE, color = border_col, linewidth = 0.35) +
    ggplot2::facet_wrap(~panel, ncol = 1) +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::scale_fill_manual(
      values = c("Train" = train_col, "Test" = test_col, "Not used" = unused_col),
      breaks = status_levels,
      drop = FALSE
    ) +
    ggplot2::labs(title = title, fill = NULL, x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = 11),
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.text = ggplot2::element_text(face = "bold", size = 8),
      panel.spacing.y = grid::unit(4, "pt"),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )
}

plot_spatiotemporal_scheme_preview_plotly <- function(
    validation_type = c("spatiotemporal_contiguous_block_cv",
                        "spatiotemporal_contiguous_block_prequential"),
    k_spat = 4,
    k_time = 5,
    initial_train_blocks = 2,
    horizon_blocks = 1,
    step_blocks = 1,
    temporal_window = c("expanding", "rolling"),
    rolling_train_blocks = NULL,
    spatial_mode = c("forecast_new_space", "forecast_known_space"),
    nx = 12,
    ny = 8,
    nz = 6,
    panel_i = 1,
    train_col = "#b8b6d9",
    test_col  = "#e66101",
    unused_col = "#ffd6df",
    point_size = 3,
    point_opacity = 0.85,
    title = NULL) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("The 'plotly' package is required for plot_spatiotemporal_scheme_preview_plotly().")
  }

  validation_type <- match.arg(validation_type)
  temporal_window <- match.arg(temporal_window)
  spatial_mode <- match.arg(spatial_mode)
  k_spat <- max(1L, as.integer(k_spat))
  k_time <- max(2L, as.integer(k_time))
  initial_train_blocks <- max(1L, as.integer(initial_train_blocks))
  horizon_blocks <- max(1L, as.integer(horizon_blocks))
  step_blocks <- max(1L, as.integer(step_blocks))
  nx <- max(k_time, as.integer(nx))
  ny <- max(k_spat, as.integer(ny))
  nz <- max(2L, as.integer(nz))
  panel_i <- max(1L, as.integer(panel_i))

  time_id <- ceiling(seq_len(nx) * k_time / nx)
  time_cells <- split(seq_len(nx), time_id)
  space_id <- ceiling(seq_len(ny) * k_spat / ny)
  space_cells <- split(seq_len(ny), space_id)

  n_panels <- if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
    k_time * k_spat
  } else {
    n_origins <- floor((k_time - initial_train_blocks - horizon_blocks) / step_blocks) + 1L
    max(0L, n_origins)
  }
  if (n_panels < 1L) {
    n_panels <- 1L
  }
  panel_i <- min(panel_i, n_panels)

  panel_label <- function(i) {
    if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      pair_grid <- expand.grid(time = seq_len(k_time), space = seq_len(k_spat))
      paste0("S", pair_grid$space[i], "-T", pair_grid$time[i])
    } else {
      paste0("O", i)
    }
  }

  make_status <- function(i) {
    status <- matrix("Train", nrow = nx, ncol = ny)

    if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      pair_grid <- expand.grid(time = seq_len(k_time), space = seq_len(k_spat))
      pair <- pair_grid[i, , drop = FALSE]
      status[time_cells[[pair$time]], space_cells[[pair$space]]] <- "Test"
      return(status)
    }

    train_end_seq <- seq(
      from = initial_train_blocks,
      to = k_time - horizon_blocks,
      by = step_blocks
    )
    if (length(train_end_seq) == 0) {
      return(matrix("Not used", nrow = nx, ncol = ny))
    }

    origin <- train_end_seq[i]
    status[,] <- "Not used"
    if (identical(temporal_window, "rolling")) {
      if (is.null(rolling_train_blocks) || is.na(rolling_train_blocks)) {
        rolling_train_blocks <- initial_train_blocks
      }
      train_start <- max(1L, origin - rolling_train_blocks + 1L)
      train_blocks <- train_start:origin
    } else {
      train_blocks <- seq_len(origin)
    }

    test_blocks <- (origin + 1L):(origin + horizon_blocks)
    test_blocks <- test_blocks[test_blocks <= k_time]
    test_space <- ((i - 1L) %% k_spat) + 1L

    status[unlist(time_cells[train_blocks]), ] <- "Train"
    status[unlist(time_cells[test_blocks]), space_cells[[test_space]]] <- "Test"
    status
  }

  status <- make_status(panel_i)
  voxels <- expand.grid(
    x = seq_len(nx),
    y = seq_len(ny),
    z = seq_len(nz)
  )
  voxels$status <- status[cbind(voxels$x, voxels$y)]
  voxels$time_block <- paste0("T", time_id[voxels$x])
  voxels$spatial_fold <- paste0("S", space_id[voxels$y])
  voxels$hover <- paste0(
    "Status: ", voxels$status,
    "<br>Time block: ", voxels$time_block,
    "<br>Spatial fold: ", voxels$spatial_fold,
    "<br>x: ", voxels$x,
    "<br>y: ", voxels$y,
    "<br>z: ", voxels$z
  )
  voxels$status <- factor(voxels$status, levels = c("Not used", "Train", "Test"))

  if (is.null(title)) {
    title <- if (identical(validation_type, "spatiotemporal_contiguous_block_cv")) {
      paste0("Spatiotemporal block CV: ", panel_label(panel_i))
    } else {
      paste0("Prequential spatiotemporal CV: ", panel_label(panel_i), " (", temporal_window, ")")
    }
  }

  fig <- plotly::plot_ly()
  status_cols <- c("Not used" = unused_col, "Train" = train_col, "Test" = test_col)
  status_opacity <- c("Not used" = min(point_opacity, 0.45), "Train" = point_opacity, "Test" = 1)

  for (status_name in names(status_cols)) {
    dat <- voxels[voxels$status == status_name, , drop = FALSE]
    if (nrow(dat) == 0) {
      next
    }
    fig <- plotly::add_trace(
      fig,
      data = dat,
      x = ~x,
      y = ~y,
      z = ~z,
      type = "scatter3d",
      mode = "markers",
      name = status_name,
      text = ~hover,
      hoverinfo = "text",
      marker = list(
        color = status_cols[[status_name]],
        size = point_size,
        opacity = status_opacity[[status_name]]
      )
    )
  }

  edge <- data.frame(
    x = c(0.5, nx + 0.5, nx + 0.5, 0.5, 0.5, NA,
          0.5, nx + 0.5, nx + 0.5, 0.5, 0.5, NA,
          0.5, 0.5, NA, nx + 0.5, nx + 0.5, NA,
          nx + 0.5, nx + 0.5, NA, 0.5, 0.5),
    y = c(0.5, 0.5, ny + 0.5, ny + 0.5, 0.5, NA,
          0.5, 0.5, ny + 0.5, ny + 0.5, 0.5, NA,
          0.5, 0.5, NA, 0.5, 0.5, NA,
          ny + 0.5, ny + 0.5, NA, ny + 0.5, ny + 0.5),
    z = c(0.5, 0.5, 0.5, 0.5, 0.5, NA,
          nz + 0.5, nz + 0.5, nz + 0.5, nz + 0.5, nz + 0.5, NA,
          0.5, nz + 0.5, NA, 0.5, nz + 0.5, NA,
          0.5, nz + 0.5, NA, 0.5, nz + 0.5)
  )
  fig <- plotly::add_trace(
    fig,
    data = edge,
    x = ~x,
    y = ~y,
    z = ~z,
    type = "scatter3d",
    mode = "lines",
    showlegend = FALSE,
    hoverinfo = "skip",
    line = list(color = "#333333", width = 3)
  )

  fig <- plotly::layout(
    fig,
    title = list(text = title),
    scene = list(
      xaxis = list(title = "Time", tickmode = "array",
                   tickvals = vapply(time_cells, stats::median, numeric(1)),
                   ticktext = paste0("T", seq_len(k_time))),
      yaxis = list(title = "Space", tickmode = "array",
                   tickvals = vapply(space_cells, stats::median, numeric(1)),
                   ticktext = paste0("S", seq_len(k_spat))),
      zaxis = list(title = "Layer", showticklabels = FALSE),
      aspectmode = "manual",
      aspectratio = list(x = max(1, nx / max(ny, 1)), y = 1, z = max(0.4, nz / max(ny, 1)))
    ),
    legend = list(orientation = "h", x = 0, y = -0.05),
    margin = list(l = 0, r = 0, b = 0, t = 45)
  )

  fig
}

