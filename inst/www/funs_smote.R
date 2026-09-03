if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

smote_pairwise_dist <- function(A, B, p = 2) {
  D <- matrix(0, nrow(A), nrow(B))
  for (j in seq_len(ncol(A))) {
    D <- D + abs(outer(A[, j], B[, j], "-"))^p
  }
  D^(1 / p)
}

smote_knn_idx <- function(A, B, k, p = 2, self_exclude = FALSE) {
  D <- smote_pairwise_dist(A, B, p)
  if (self_exclude && nrow(A) == nrow(B)) {
    diag(D) <- Inf
  }
  t(apply(D, 1, function(r) order(r)[seq_len(min(k, length(r)))]))
}

smote_synth_attr_rows <- function(attr_data, synth_info, row_names, enabled = FALSE) {
  if (!nrow(synth_info)) {
    return(attr_data[FALSE, , drop = FALSE])
  }

  if (!isTRUE(enabled)) {
    synth <- attr_data[rep(1, nrow(synth_info)), , drop = FALSE]
    synth[] <- NA
    rownames(synth) <- row_names
    return(synth)
  }

  synth <- lapply(attr_data, function(x) {
    seed <- x[synth_info$seed_index]
    neighbor <- x[synth_info$neighbor_index]
    gap <- synth_info$gap

    if (is.numeric(x)) {
      return(seed + gap * (neighbor - seed))
    }

    if (is.integer(x)) {
      return(as.integer(round(seed + gap * (neighbor - seed))))
    }

    if (inherits(x, "Date")) {
      return(as.Date(round(as.numeric(seed) + gap * (as.numeric(neighbor) - as.numeric(seed))), origin = "1970-01-01"))
    }

    if (inherits(x, c("POSIXct", "POSIXlt"))) {
      tz <- attr(as.POSIXct(x), "tzone")
      if (is.null(tz) || !length(tz)) {
        tz <- "UTC"
      }
      return(as.POSIXct(as.numeric(seed) + gap * (as.numeric(neighbor) - as.numeric(seed)), origin = "1970-01-01", tz = tz[1]))
    }

    rep(NA, nrow(synth_info))
  })

  synth <- data.frame(synth, check.names = FALSE)
  colnames(synth) <- colnames(attr_data)
  rownames(synth) <- row_names
  synth
}

smote_extended <- function(X, y,
                           strategy = c("percent", "balance", "ratio"),
                           N = 100,
                           ratio = 1,
                           k = 5,
                           k_full = 10,
                           mode = c("none", "bsmote1", "bsmote2"),
                           p = 2,
                           scale = TRUE,
                           seed = NULL,
                           return_meta = TRUE) {
  if (!is.null(seed) && !is.na(seed)) {
    set.seed(seed)
  }

  strategy <- match.arg(strategy)
  mode <- match.arg(mode)

  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("X must be a numeric matrix or data.frame.")
  }

  X <- as.matrix(X)

  if (!all(vapply(seq_len(ncol(X)), function(j) is.numeric(X[, j]), logical(1)))) {
    stop("SMOTE requires numeric predictors.")
  }

  y <- factor(y)
  n <- nrow(X)

  if (n != length(y)) {
    stop("X and y have incompatible dimensions.")
  }

  mu <- colMeans(X)
  sdv <- apply(X, 2, sd)
  sdv[sdv == 0] <- 1
  Xs <- if (isTRUE(scale)) sweep(sweep(X, 2, mu, "-"), 2, sdv, "/") else X

  tab <- table(y)
  classes <- names(tab)
  n_max <- max(tab)
  target <- setNames(as.numeric(tab), classes)

  if (strategy == "balance") {
    target[] <- n_max
  }

  if (strategy == "ratio") {
    target[] <- ceiling(ratio * n_max)
  }

  if (strategy == "percent") {
    for (cl in classes) {
      target[[cl]] <- tab[[cl]] + floor((N / 100) * tab[[cl]])
    }
  }

  X_syn_all <- NULL
  y_syn_all <- character(0)
  synth_info_all <- NULL
  meta <- list()

  for (cl in classes) {
    idx <- which(y == cl)
    n_c <- length(idx)

    if (n_c < 2) {
      next
    }

    Xc <- Xs[idx, , drop = FALSE]
    k_min <- min(k, max(1, n_c - 1))
    nn_min <- smote_knn_idx(Xc, Xc, k = k_min, p = p, self_exclude = TRUE)

    if (mode != "none") {
      kf <- min(k_full, max(1, n - 1))
      nn_full <- smote_knn_idx(Xs[idx, , drop = FALSE], Xs, k = kf, p = p, self_exclude = FALSE)
      neigh_lab <- matrix(y[as.vector(nn_full)], nrow = nrow(nn_full))
      mprime <- rowSums(neigh_lab != cl)
      is_noise <- mprime == kf
      is_danger <- mprime >= ceiling(kf / 2) & mprime < kf
      is_safe <- mprime < ceiling(kf / 2)
      danger_ix_in_class <- which(is_danger)
      use_mode <- if (length(danger_ix_in_class) == 0) "none" else mode
    } else {
      use_mode <- "none"
      danger_ix_in_class <- seq_len(n_c)
      is_noise <- is_safe <- rep(FALSE, n_c)
      mprime <- rep(0L, n_c)
    }

    choose_seeds <- if (use_mode == "none") seq_len(n_c) else danger_ix_in_class
    Tn <- length(choose_seeds)
    n_per_seed <- integer(n_c)

    if (strategy == "percent") {
      if (use_mode == "none") {
        if (N < 100) {
          need <- floor((N / 100) * n_c)
          if (need > 0) {
            n_per_seed[sample.int(n_c, need)] <- 1
          }
        } else {
          N_int <- floor(N / 100)
          R_pct <- N - 100 * N_int
          n_per_seed[] <- N_int
          extra <- floor((R_pct / 100) * n_c)
          if (extra > 0) {
            add_idx <- sample.int(n_c, extra)
            n_per_seed[add_idx] <- n_per_seed[add_idx] + 1
          }
        }
      } else {
        if (N < 100) {
          need <- floor((N / 100) * Tn)
          if (need > 0) {
            n_per_seed[sample(choose_seeds, need)] <- 1
          }
        } else {
          N_int <- floor(N / 100)
          R_pct <- N - 100 * N_int
          n_per_seed[choose_seeds] <- N_int
          extra <- floor((R_pct / 100) * Tn)
          if (extra > 0) {
            add_idx <- sample(choose_seeds, extra)
            n_per_seed[add_idx] <- n_per_seed[add_idx] + 1
          }
        }
      }
    } else {
      need <- max(0, target[[cl]] - n_c)
      if (need > 0 && Tn > 0) {
        base <- floor(need / Tn)
        n_per_seed[choose_seeds] <- base
        rem <- need - base * Tn
        if (rem > 0) {
          give <- sample(choose_seeds, rem, replace = FALSE)
          n_per_seed[give] <- n_per_seed[give] + 1
        }
      }
    }

    m_total <- sum(n_per_seed)

    if (m_total == 0) {
      if (return_meta) {
        meta[[cl]] <- list(
          added = 0L,
          n_danger = if (mode == "none") NA_integer_ else length(danger_ix_in_class),
          n_safe = if (mode == "none") NA_integer_ else sum(is_safe),
          n_noise = if (mode == "none") NA_integer_ else sum(is_noise)
        )
      }
      next
    }

    synth <- matrix(NA_real_, nrow = m_total, ncol = ncol(Xs))
    ptr <- 1

    if (use_mode == "bsmote2") {
      idx_maj <- which(y != cl)
      Xmaj <- Xs[idx_maj, , drop = FALSE]
      dn <- smote_pairwise_dist(Xc, Xmaj, p)
      nearest_neg_idx <- idx_maj[max.col(-dn)]
    }

    for (i in seq_len(n_c)) {
      m <- n_per_seed[i]
      if (m <= 0) {
        next
      }

      seeds_i <- matrix(Xc[i, ], nrow = m, ncol = ncol(Xc), byrow = TRUE)

      if (use_mode == "bsmote2") {
        m_pos <- floor(m / 2)
        m_neg <- m - m_pos

        if (m_pos > 0) {
          nn_i <- nn_min[i, ]
          nn_chosen <- if (length(nn_i) == 1) rep(nn_i, m_pos) else sample(nn_i, m_pos, TRUE)
          neigh_pos <- Xc[nn_chosen, , drop = FALSE]
          gaps <- runif(m_pos)
          Spos <- seeds_i[seq_len(m_pos), ] + gaps * (neigh_pos - seeds_i[seq_len(m_pos), ])
          synth[ptr:(ptr + m_pos - 1), ] <- Spos
          synth_info_all <- rbind(
            synth_info_all,
            data.frame(
              class = cl,
              seed_index = idx[i],
              neighbor_index = idx[nn_chosen],
              gap = gaps,
              mode_part = "minority",
              stringsAsFactors = FALSE
            )
          )
          ptr <- ptr + m_pos
        }

        if (m_neg > 0) {
          neg_vec <- matrix(Xs[nearest_neg_idx[i], ], nrow = m_neg, ncol = ncol(Xs), byrow = TRUE)
          gapsn <- runif(m_neg, min = 0, max = 0.5)
          Sneg <- seeds_i[seq_len(m_neg), ] + gapsn * (neg_vec - seeds_i[seq_len(m_neg), ])
          synth[ptr:(ptr + m_neg - 1), ] <- Sneg
          synth_info_all <- rbind(
            synth_info_all,
            data.frame(
              class = cl,
              seed_index = idx[i],
              neighbor_index = nearest_neg_idx[i],
              gap = gapsn,
              mode_part = "majority",
              stringsAsFactors = FALSE
            )
          )
          ptr <- ptr + m_neg
        }
      } else {
        nn_i <- nn_min[i, ]
        nn_chosen <- if (length(nn_i) == 1) rep(nn_i, m) else sample(nn_i, m, TRUE)
        neigh_i <- Xc[nn_chosen, , drop = FALSE]
        gaps <- runif(m)
        S <- seeds_i + gaps * (neigh_i - seeds_i)
        synth[ptr:(ptr + m - 1), ] <- S
        synth_info_all <- rbind(
          synth_info_all,
          data.frame(
            class = cl,
            seed_index = idx[i],
            neighbor_index = idx[nn_chosen],
            gap = gaps,
            mode_part = "minority",
            stringsAsFactors = FALSE
          )
        )
        ptr <- ptr + m
      }
    }

    if (isTRUE(scale)) {
      synth <- sweep(sweep(synth, 2, sdv, "*"), 2, mu, "+")
    }

    X_syn_all <- rbind(X_syn_all, synth)
    y_syn_all <- c(y_syn_all, rep(cl, nrow(synth)))

    if (return_meta) {
      meta[[cl]] <- list(
        added = nrow(synth),
        n_danger = if (mode == "none") NA_integer_ else length(danger_ix_in_class),
        n_safe = if (mode == "none") NA_integer_ else sum(is_safe),
        n_noise = if (mode == "none") NA_integer_ else sum(is_noise),
        mprime = if (mode == "none") NULL else mprime,
        n_per_seed = n_per_seed
      )
    }
  }

  if (is.null(X_syn_all)) {
    return(list(X = as.data.frame(X), y = y, added = 0L, meta = meta, synth_info = data.frame()))
  }

  X_new <- rbind(X, X_syn_all)
  y_new <- factor(c(as.character(y), y_syn_all), levels = levels(y))

  list(X = as.data.frame(X_new), y = y_new, added = nrow(X_syn_all), meta = meta, synth_info = synth_info_all)
}

smoter_regression <- function(X, y,
                              rare = c("upper", "lower", "both"),
                              q = 0.1,
                              strategy = c("balance", "percent", "ratio"),
                              N = 100,
                              ratio = 1,
                              k = 5,
                              p = 2,
                              scale = TRUE,
                              seed = NULL,
                              return_meta = TRUE) {
  if (!is.null(seed) && !is.na(seed)) {
    set.seed(seed)
  }

  rare <- match.arg(rare)
  strategy <- match.arg(strategy)

  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("X must be a numeric matrix or data.frame.")
  }

  X <- as.matrix(X)
  y <- as.numeric(y)

  if (!all(vapply(seq_len(ncol(X)), function(j) is.numeric(X[, j]), logical(1)))) {
    stop("SMOTER requires numeric predictors.")
  }

  if (nrow(X) != length(y)) {
    stop("X and y have incompatible dimensions.")
  }

  if (!is.finite(q) || q <= 0 || q >= 0.5) {
    stop("Rare quantile must be greater than 0 and lower than 0.5.")
  }

  low_cut <- as.numeric(stats::quantile(y, probs = q, na.rm = TRUE, names = FALSE))
  high_cut <- as.numeric(stats::quantile(y, probs = 1 - q, na.rm = TRUE, names = FALSE))
  region <- rep("Regular", length(y))

  if (rare %in% c("lower", "both")) {
    region[y <= low_cut] <- "Rare low"
  }

  if (rare %in% c("upper", "both")) {
    region[y >= high_cut] <- "Rare high"
  }

  tab <- table(region)
  rare_groups <- setdiff(names(tab), "Regular")
  rare_groups <- rare_groups[tab[rare_groups] > 0]

  if (!length(rare_groups)) {
    stop("No rare observations were found for the selected response and quantile.")
  }

  if (any(tab[rare_groups] < 2)) {
    stop("Each selected rare region must contain at least two complete observations.")
  }

  mu <- colMeans(X)
  sdv <- apply(X, 2, sd)
  sdv[sdv == 0] <- 1
  Xs <- if (isTRUE(scale)) sweep(sweep(X, 2, mu, "-"), 2, sdv, "/") else X

  y_mu <- mean(y)
  y_sd <- stats::sd(y)
  if (!is.finite(y_sd) || y_sd == 0) {
    y_sd <- 1
  }
  ys <- if (isTRUE(scale)) (y - y_mu) / y_sd else y
  Zs <- cbind(Xs, .response = ys)

  target <- setNames(as.numeric(tab), names(tab))
  if (strategy == "balance") {
    target[rare_groups] <- max(as.numeric(tab))
  }
  if (strategy == "ratio") {
    target[rare_groups] <- ceiling(ratio * max(as.numeric(tab)))
  }
  if (strategy == "percent") {
    target[rare_groups] <- as.numeric(tab[rare_groups]) + floor((N / 100) * as.numeric(tab[rare_groups]))
  }

  X_syn_all <- NULL
  y_syn_all <- numeric(0)
  synth_info_all <- NULL
  meta <- list()

  for (grp in rare_groups) {
    idx <- which(region == grp)
    n_g <- length(idx)
    need <- max(0, target[[grp]] - n_g)

    if (need == 0) {
      if (return_meta) {
        meta[[grp]] <- list(added = 0L, cutoff = if (grp == "Rare low") low_cut else high_cut)
      }
      next
    }

    k_g <- min(k, max(1, n_g - 1))
    nn <- smote_knn_idx(Zs[idx, , drop = FALSE], Zs[idx, , drop = FALSE], k = k_g, p = p, self_exclude = TRUE)
    n_per_seed <- rep(floor(need / n_g), n_g)
    rem <- need - sum(n_per_seed)
    if (rem > 0) {
      add_idx <- sample.int(n_g, rem, replace = FALSE)
      n_per_seed[add_idx] <- n_per_seed[add_idx] + 1
    }

    synth_x <- matrix(NA_real_, nrow = need, ncol = ncol(X))
    synth_y <- numeric(need)
    ptr <- 1

    for (i in seq_len(n_g)) {
      m <- n_per_seed[i]
      if (m <= 0) {
        next
      }

      nn_i <- nn[i, ]
      nn_chosen <- if (length(nn_i) == 1) rep(nn_i, m) else sample(nn_i, m, TRUE)
      seed_x <- matrix(X[idx[i], ], nrow = m, ncol = ncol(X), byrow = TRUE)
      neigh_x <- X[idx[nn_chosen], , drop = FALSE]
      seed_y <- rep(y[idx[i]], m)
      neigh_y <- y[idx[nn_chosen]]
      gaps <- runif(m)

      rows <- ptr:(ptr + m - 1)
      synth_x[rows, ] <- seed_x + gaps * (neigh_x - seed_x)
      synth_y[rows] <- seed_y + gaps * (neigh_y - seed_y)
      synth_info_all <- rbind(
        synth_info_all,
        data.frame(
          region = grp,
          seed_index = idx[i],
          neighbor_index = idx[nn_chosen],
          gap = gaps,
          stringsAsFactors = FALSE
        )
      )
      ptr <- ptr + m
    }

    X_syn_all <- rbind(X_syn_all, synth_x)
    y_syn_all <- c(y_syn_all, synth_y)

    if (return_meta) {
      meta[[grp]] <- list(
        added = nrow(synth_x),
        cutoff = if (grp == "Rare low") low_cut else high_cut,
        n_per_seed = n_per_seed
      )
    }
  }

  if (is.null(X_syn_all)) {
    return(list(X = as.data.frame(X), y = y, added = 0L, before = tab, after = tab, meta = meta, synth_info = data.frame()))
  }

  X_new <- rbind(X, X_syn_all)
  y_new <- c(y, y_syn_all)
  region_new <- c(region, synth_info_all$region)

  list(
    X = as.data.frame(X_new),
    y = y_new,
    added = nrow(X_syn_all),
    before = tab,
    after = table(region_new),
    meta = meta,
    synth_info = synth_info_all,
    rare = rare,
    q = q,
    low_cut = low_cut,
    high_cut = high_cut
  )
}

imesc_smote <- function(data, class_column, vars = NULL,
                        strategy = c("balance", "percent", "ratio"),
                        N = 100,
                        ratio = 1,
                        k = 5,
                        k_full = 10,
                        mode = c("none", "bsmote1", "bsmote2"),
                        p = 2,
                        scale = TRUE,
                        seed = NULL,
                        synth_coords = FALSE,
                        synth_time = FALSE,
                        newname = "smote_datalist") {
  strategy <- match.arg(strategy)
  mode <- match.arg(mode)

  if (is.null(data) || !is.data.frame(data)) {
    stop("A valid Datalist is required.")
  }

  factors <- attr(data, "factors")

  if (is.null(factors) || !class_column %in% colnames(factors)) {
    stop("The selected class column must exist in the Factor-Attribute.")
  }

  if (is.null(vars) || !length(vars)) {
    vars <- colnames(data)[vapply(data, is.numeric, logical(1))]
  }

  vars <- vars[vars %in% colnames(data)]

  if (!length(vars)) {
    stop("Select at least one numeric variable for SMOTE.")
  }

  numeric_ok <- vapply(data[, vars, drop = FALSE], is.numeric, logical(1))

  if (!all(numeric_ok)) {
    stop("SMOTE can only use numeric variables.")
  }

  y <- factors[rownames(data), class_column, drop = TRUE]
  y <- factor(y)

  complete <- complete.cases(data[, vars, drop = FALSE]) & !is.na(y)

  if (sum(complete) < 3) {
    stop("SMOTE requires at least three complete observations.")
  }

  data_in <- data[complete, vars, drop = FALSE]
  y_in <- droplevels(y[complete])

  if (nlevels(y_in) < 2) {
    stop("SMOTE requires at least two classes in the selected factor.")
  }

  if (any(table(y_in) < 2)) {
    stop("Each class must contain at least two complete observations.")
  }

  result <- smote_extended(
    X = data_in,
    y = y_in,
    strategy = strategy,
    N = N,
    ratio = ratio,
    k = k,
    k_full = k_full,
    mode = mode,
    p = p,
    scale = scale,
    seed = seed,
    return_meta = TRUE
  )

  out <- result$X
  colnames(out) <- vars

  original_n <- nrow(data_in)
  added <- nrow(out) - original_n
  rownames(out) <- c(
    rownames(data_in),
    if (added > 0) paste0("SMOTE_", seq_len(added)) else character(0)
  )

  factors_in <- factors[rownames(data_in), , drop = FALSE]

  if (added > 0) {
    synth_factors <- factors_in[rep(1, added), , drop = FALSE]
    synth_factors[] <- NA
    rownames(synth_factors) <- rownames(out)[(original_n + 1):nrow(out)]
    synth_factors[[class_column]] <- result$y[(original_n + 1):length(result$y)]
    synth_factors$SMOTE_source <- factor("synthetic", levels = c("original", "synthetic"))
  } else {
    synth_factors <- factors_in[FALSE, , drop = FALSE]
    synth_factors$SMOTE_source <- factor(levels = c("original", "synthetic"))
  }

  factors_in$SMOTE_source <- factor("original", levels = c("original", "synthetic"))
  factors_out <- rbind(factors_in, synth_factors)
  factors_out[[class_column]] <- factor(factors_out[[class_column]], levels = levels(y_in))

  attr(out, "factors") <- factors_out

  coords <- attr(data, "coords")
  if (!is.null(coords)) {
    coords_in <- coords[rownames(data_in), , drop = FALSE]
    coords_out <- coords_in
    if (added > 0) {
      synth_coords <- smote_synth_attr_rows(
        attr_data = coords_in,
        synth_info = result$synth_info,
        row_names = rownames(out)[(original_n + 1):nrow(out)],
        enabled = isTRUE(synth_coords)
      )
      coords_out <- rbind(coords_in, synth_coords)
    }
    attr(out, "coords") <- coords_out
  }

  time <- attr(data, "time")
  if (!is.null(time)) {
    time_in <- time[rownames(data_in), , drop = FALSE]
    time_out <- time_in
    if (added > 0) {
      synth_time <- smote_synth_attr_rows(
        attr_data = time_in,
        synth_info = result$synth_info,
        row_names = rownames(out)[(original_n + 1):nrow(out)],
        enabled = isTRUE(synth_time)
      )
      time_out <- rbind(time_in, synth_time)
    }
    attr(out, "time") <- time_out
  }

  attr(out, "base_shape") <- attr(data, "base_shape")
  attr(out, "layer_shape") <- attr(data, "layer_shape")
  attr(out, "extra_shape") <- attr(data, "extra_shape")
  attr(out, "datalist_root") <- attr(data, "datalist_root") %||% attr(data, "datalist")
  attr(out, "datalist") <- newname
  attr(out, "new_datalist") <- newname
  attr(out, "action") <- "datalist"
  attr(out, "filename") <- newname
  attr(out, "nobs_ori") <- nrow(out)
  attr(out, "nvar_ori") <- ncol(out)
  attr(out, "smote") <- list(
    class_column = class_column,
    variables = vars,
    strategy = strategy,
    N = N,
    ratio = ratio,
    k = k,
    k_full = k_full,
    mode = mode,
    p = p,
    scale = scale,
    seed = seed,
    synth_coords = isTRUE(synth_coords),
    synth_time = isTRUE(synth_time),
    before = table(y_in),
    after = table(result$y),
    added = result$added,
    meta = result$meta
  )

  out
}

imesc_smoter <- function(data, response_column, vars = NULL,
                         rare = c("upper", "lower", "both"),
                         rare_q = 0.1,
                         strategy = c("balance", "percent", "ratio"),
                         N = 100,
                         ratio = 1,
                         k = 5,
                         p = 2,
                         scale = TRUE,
                         seed = NULL,
                         synth_coords = FALSE,
                         synth_time = FALSE,
                         newname = "smoter_datalist") {
  rare <- match.arg(rare)
  strategy <- match.arg(strategy)

  if (is.null(data) || !is.data.frame(data)) {
    stop("A valid Datalist is required.")
  }

  if (is.null(response_column) || !response_column %in% colnames(data)) {
    stop("The selected response must exist in the Numeric-Attribute.")
  }

  if (!is.numeric(data[[response_column]])) {
    stop("SMOTER requires a numeric response variable.")
  }

  if (is.null(vars) || !length(vars)) {
    vars <- setdiff(colnames(data)[vapply(data, is.numeric, logical(1))], response_column)
  }

  vars <- setdiff(vars[vars %in% colnames(data)], response_column)

  if (!length(vars)) {
    stop("Select at least one numeric predictor for SMOTER.")
  }

  numeric_ok <- vapply(data[, vars, drop = FALSE], is.numeric, logical(1))

  if (!all(numeric_ok)) {
    stop("SMOTER can only use numeric predictors.")
  }

  complete <- complete.cases(data[, c(response_column, vars), drop = FALSE])

  if (sum(complete) < 4) {
    stop("SMOTER requires at least four complete observations.")
  }

  data_in <- data[complete, vars, drop = FALSE]
  y_in <- data[complete, response_column, drop = TRUE]

  result <- smoter_regression(
    X = data_in,
    y = y_in,
    rare = rare,
    q = rare_q,
    strategy = strategy,
    N = N,
    ratio = ratio,
    k = k,
    p = p,
    scale = scale,
    seed = seed,
    return_meta = TRUE
  )

  out <- result$X
  colnames(out) <- vars
  out[[response_column]] <- result$y
  out <- out[, c(response_column, vars), drop = FALSE]

  original_n <- nrow(data_in)
  added <- nrow(out) - original_n
  rownames(out) <- c(
    rownames(data_in),
    if (added > 0) paste0("SMOTER_", seq_len(added)) else character(0)
  )

  factors <- attr(data, "factors")
  if (!is.null(factors)) {
    factors_in <- factors[rownames(data_in), , drop = FALSE]
  } else {
    factors_in <- data.frame(row.names = rownames(data_in))
  }

  if (added > 0) {
    synth_factors <- factors_in[rep(1, added), , drop = FALSE]
    if (ncol(synth_factors) > 0) {
      synth_factors[] <- NA
    }
    rownames(synth_factors) <- rownames(out)[(original_n + 1):nrow(out)]
    synth_factors$SMOTE_source <- factor("synthetic", levels = c("original", "synthetic"))
    synth_factors$SMOTER_region <- factor(result$synth_info$region, levels = c("Regular", "Rare low", "Rare high"))
  } else {
    synth_factors <- factors_in[FALSE, , drop = FALSE]
    synth_factors$SMOTE_source <- factor(levels = c("original", "synthetic"))
    synth_factors$SMOTER_region <- factor(levels = c("Regular", "Rare low", "Rare high"))
  }

  factors_in$SMOTE_source <- factor("original", levels = c("original", "synthetic"))
  factors_in$SMOTER_region <- factor("Regular", levels = c("Regular", "Rare low", "Rare high"))
  factors_out <- rbind(factors_in, synth_factors)
  attr(out, "factors") <- factors_out

  coords <- attr(data, "coords")
  if (!is.null(coords)) {
    coords_in <- coords[rownames(data_in), , drop = FALSE]
    coords_out <- coords_in
    if (added > 0) {
      synth_coords <- smote_synth_attr_rows(
        attr_data = coords_in,
        synth_info = result$synth_info,
        row_names = rownames(out)[(original_n + 1):nrow(out)],
        enabled = isTRUE(synth_coords)
      )
      coords_out <- rbind(coords_in, synth_coords)
    }
    attr(out, "coords") <- coords_out
  }

  time <- attr(data, "time")
  if (!is.null(time)) {
    time_in <- time[rownames(data_in), , drop = FALSE]
    time_out <- time_in
    if (added > 0) {
      synth_time <- smote_synth_attr_rows(
        attr_data = time_in,
        synth_info = result$synth_info,
        row_names = rownames(out)[(original_n + 1):nrow(out)],
        enabled = isTRUE(synth_time)
      )
      time_out <- rbind(time_in, synth_time)
    }
    attr(out, "time") <- time_out
  }

  attr(out, "base_shape") <- attr(data, "base_shape")
  attr(out, "layer_shape") <- attr(data, "layer_shape")
  attr(out, "extra_shape") <- attr(data, "extra_shape")
  attr(out, "datalist_root") <- attr(data, "datalist_root") %||% attr(data, "datalist")
  attr(out, "datalist") <- newname
  attr(out, "new_datalist") <- newname
  attr(out, "action") <- "datalist"
  attr(out, "filename") <- newname
  attr(out, "nobs_ori") <- nrow(out)
  attr(out, "nvar_ori") <- ncol(out)
  attr(out, "smote") <- list(
    task = "regression",
    response_column = response_column,
    variables = vars,
    rare = rare,
    rare_q = rare_q,
    low_cut = result$low_cut,
    high_cut = result$high_cut,
    strategy = strategy,
    N = N,
    ratio = ratio,
    k = k,
    p = p,
    scale = scale,
    seed = seed,
    synth_coords = isTRUE(synth_coords),
    synth_time = isTRUE(synth_time),
    before = result$before,
    after = result$after,
    added = result$added,
    meta = result$meta
  )

  out
}
