## from https://github.com/davidsjoberg/ggsankey/blob/main/R/sankey.R

utils::globalVariables(c(".", ".data", "x", "node", "next_node", "next_x", "..r"))
# importFrom(ggplot2, "%+replace%")
#' @importFrom ggplot2 %+replace%

# ** Support functions ----------
prepare_params <- function(...) {
  # Prepare aesthics for flow lines
  flow.aes <- list(...)
  removes <- names(flow.aes)
  removes <- stringr::str_extract_all(removes, "(?<=flow.).*")
  removes <- unlist(removes)
  removes2 <- names(flow.aes)
  removes2 <- stringr::str_subset(removes2, "node")
  removes2 <- unlist(removes2)
  flow.aes[c(removes, removes2)] <- NULL
  names(flow.aes) <- stringr::str_replace_all(names(flow.aes), "flow.", "")

  # Prepare aesthics for node boxes
  node.aes <- list(...)
  removes <- names(node.aes)
  removes <- stringr::str_extract_all(removes, "(?<=node.).*")
  removes <- unlist(removes)
  removes2 <- names(node.aes)
  removes2 <- stringr::str_subset(removes2, "flow")
  removes2 <- unlist(removes2)
  node.aes[c(removes, removes2)] <- NULL
  names(node.aes) <- stringr::str_replace_all(names(node.aes), "node.", "")

  return(list(flow.aes, node.aes))
}

find_default_space <- function(.df) {
  res <- .df
  res <- dplyr::group_by(res, .data$n_x)
  res <- dplyr::summarise(res, n_groups = dplyr::n_distinct(.data$node),
              freq = sum(.data$freq, na.rm = TRUE))
  res <- dplyr::mutate(res, v = .data$freq / .data$n_groups / 4)
  res <- dplyr::pull(res, .data$v)
  max(res)
}

sigmoid <- function(x_from, x_to, y_from, y_to, smooth = 5, n = 300) {
  x <- seq(-smooth, smooth, length = n)
  y <- exp(x) / (exp(x) + 1)
  out <- data.frame(x = (x + smooth) / (smooth * 2) * (x_to - x_from) + x_from,
                    y = y * (y_to - y_from) + y_from)
}


# ** make_long -----------------------------------------------------------------
#' @title make_long
#'
#' @description Prepares a 'wide' data frame into a format that `geom_sankey` or `geom_alluvial` understands. Useful to show flows between dimensions in dataset.
#'
#' @param .df a data frame
#' @param ... unquoted columnnames of df that you want to include in the plot.
#' @param value if each row have a weight this weight could be kept by providing column name of weight.
#'
#' @return a longer data frame
#'
#' @export
make_long <- function(.df, ..., value = NULL) {
  if("..r" %in% names(.df)) stop("The column name '..r' is not allowed")
  .vars <- dplyr::quos(...)

  if(!missing(value)) {
    value_var <- dplyr::enquo(value)
    out <- .df
    out <- dplyr::select(out, !!!.vars, value = !!value_var)
    out <- dplyr::mutate(out, ..r = dplyr::row_number())
    out <- tidyr::gather(out, x, node, -..r, -value)
    out <- dplyr::arrange(out, .data$..r)
    out <- dplyr::group_by(out, .data$..r)
    out <- dplyr::mutate(out, next_x = dplyr::lead(.data$x),
                    next_node = dplyr::lead(.data$node)
      )
    out <- dplyr::ungroup(out)
    out <- dplyr::select(out, -..r)
    out <- dplyr::relocate(out, value, .after = dplyr::last_col())
  } else {
    out <- .df
    out <- dplyr::select(out, !!!.vars)
    out <- dplyr::mutate(out, ..r = dplyr::row_number())
    out <- tidyr::gather(out, x, node, -..r)
    out <- dplyr::arrange(out, .data$..r)
    out <- dplyr::group_by(out, .data$..r)
    out <- dplyr::mutate(out, next_x = dplyr::lead(.data$x),
                    next_node = dplyr::lead(.data$node)
      )
    out <- dplyr::ungroup(out)
    out <- dplyr::select(out, -..r)
  }

  levels <- unique(out$x)

  dplyr::mutate(out, dplyr::across(c(x, next_x), ~factor(., levels = levels)))
}


#' @title sankey_themes
#' @name theme_sankey
#' @aliases theme_alluvial
#' @aliases theme_sankey_bump
#'
#' @description Minimal themes for sankey, alluvial and sankey bump plots
#'
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @export
theme_sankey <-
  function(base_size = 11,
           base_family = "",
           base_line_size = base_size / 22,
           base_rect_size = base_size / 22) {
    {
      ggplot2::theme_bw(
        base_size = base_size,
        base_family = base_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
      ) %+replace%
        ggplot2::theme(
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(colour = "black",
                                            linewidth = ggplot2::rel(1)),
          legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(
            fill = "white",
            colour = "transparent",
            size = ggplot2::rel(2)
          ),
          complete = TRUE,
          axis.line.y = ggplot2::element_blank(),
          axis.line.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }
  }

#' @rdname theme_sankey
#' @export
theme_alluvial <-
  function(base_size = 11,
           base_family = "",
           base_line_size = base_size / 22,
           base_rect_size = base_size / 22) {
    {
      ggplot2::theme_bw(
        base_size = base_size,
        base_family = base_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
      ) %+replace%
        ggplot2::theme(
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank(),
          legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(
            fill = "white",
            colour = "transparent",
            size = ggplot2::rel(2)
          ),
          complete = TRUE,
          axis.line.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }
  }

#' @rdname theme_sankey
#' @export
theme_sankey_bump <-
  function(base_size = 11,
           base_family = "",
           base_line_size = base_size / 22,
           base_rect_size = base_size / 22) {
    {
      ggplot2::theme_bw(
        base_size = base_size,
        base_family = base_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
      ) %+replace%
        ggplot2::theme(
          panel.border = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank(),
          legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(
            fill = "white",
            colour = "transparent",
            size = ggplot2::rel(2)
          ),
          complete = TRUE,
          axis.line.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line("gray90")
        )
    }
  }


# FLOW LAYER ---------
StatSankeyFlow <- ggplot2::ggproto("StatSankeyFlow", ggplot2::Stat,
                                   extra_params = c("n_grid", "na.rm", "type", "width", "space", "smooth"),

                                   setup_data = function(data, params) {
                                     purrr::map_dfr(unique(data$PANEL),
                                             ~{
                                               data <- dplyr::filter(data, PANEL == .x)

                                               data <- dplyr::mutate(data, dplyr::across(c(x, next_x), ~as.numeric(.), .names = ("n_{.col}")))

                                               if(!("value" %in% names(data))) {
                                                 flow_data <- data
                                                 flow_data <- dplyr::mutate(flow_data, group = 1)
                                                 flow_data <- dplyr::group_by(flow_data, n_x, node, n_next_x, next_node)
                                                 flow_data <- dplyr::summarise(flow_data, flow_freq = dplyr::n(), .groups = "keep")
                                                 flow_data <- dplyr::ungroup(flow_data)

                                                 data <- dplyr::mutate(data, group = 1)
                                                 data <- dplyr::select(data, -n_next_x, -next_node, -next_x)
                                                 data <- dplyr::group_by_all(data)
                                                 data <- dplyr::summarise(data, freq = dplyr::n(), .groups = "keep")
                                                 data <- dplyr::ungroup(data)
                                               } else {
                                                 flow_data <- data
                                                 flow_data <- dplyr::mutate(flow_data, group = 1)
                                                 flow_data <- dplyr::group_by(flow_data, n_x, node, n_next_x, next_node)
                                                 flow_data <- dplyr::summarise(flow_data, flow_freq = sum(value, na.rm = TRUE), .groups = "keep")
                                                 flow_data <- dplyr::ungroup(flow_data)

                                                 data <- dplyr::mutate(data, group = 1)
                                                 data <- dplyr::select(data, -n_next_x, -next_node, -next_x)
                                                 data <- dplyr::group_by_at(data, dplyr::vars(dplyr::everything(), -value))
                                                 data <- dplyr::summarise(data, freq = sum(value, na.rm = TRUE),, .groups = "keep")
                                                 data <- dplyr::ungroup(data)
                                               }

                                               if(is.null(params$space)) {
                                                 params$space <- find_default_space(data)
                                               }

                                               data <- dplyr::group_by(data, n_x)
                                               data <- dplyr::mutate(data, ymax = cumsum(freq) + (dplyr::row_number() - 1)*params$space,
                                                        ymin = ymax - freq)
                                               data <- dplyr::ungroup(data)

                                               if(params$type == "sankey") {
                                                 data <- dplyr::group_by(data, n_x)
                                                 data <- dplyr::mutate(data, ymin = ymin - max(ymax)/2,
                                                          ymax = ymax - max(ymax)/2)
                                                 data <- dplyr::ungroup(data)
                                               } else if (params$type == "alluvial"){
                                                 data <- data
                                               }

                                               data <- dplyr::mutate(data, xmin = n_x - params$width/2,
                                                        xmax = n_x + params$width/2)

                                               if("shift" %in% names(data)) {
                                                 data <- dplyr::mutate(data, dplyr::across(dplyr::contains("y"), ~ . + shift))
                                               }

                                               df <- data
                                               df <- dplyr::left_join(df, flow_data, by = c("n_x", "node"))



                                               flows <- df
                                               flows <- dplyr::left_join(flows, dplyr::distinct(dplyr::select(df, n_x, node, ymin_end = ymin, ymax_end = ymax, xmin_end = xmin, xmax_end = xmax)),
                                                           by = c("n_next_x" = "n_x", "next_node" = "node"))
                                               flows <- tidyr::drop_na(flows, n_x, node, next_node, n_next_x, ymax_end, ymin_end, xmax_end, xmin_end)
                                               flows <- dplyr::mutate(flows, r = dplyr::row_number())
                                               flows <- dplyr::arrange(flows, n_x, -r)
                                               flows <- dplyr::select(flows, -r)
                                               flows <- dplyr::group_by(flows, n_x, node)
                                               flows <- dplyr::mutate(flows, cum_flow_freq = cumsum(flow_freq) - flow_freq)
                                               flows <- dplyr::ungroup(flows)
                                               flows <- dplyr::group_by(flows, n_x, n_next_x, node, next_node)
                                               flows <- dplyr::mutate(flows, flow_start_ymax = ymax - cum_flow_freq,
                                                        flow_start_ymin = flow_start_ymax - flow_freq)

                                               flows <- dplyr::arrange(flows, n_x, n_next_x, next_node)
                                               flows <- dplyr::group_by(flows, n_next_x, next_node)
                                               flows <- dplyr::mutate(flows, cum_flow_freq_end = cumsum(flow_freq) - flow_freq)
                                               flows <- dplyr::mutate(flows, flow_end_ymax = ymax_end - cum_flow_freq_end,
                                                        flow_end_ymin = flow_end_ymax - flow_freq)
                                               flows <- dplyr::ungroup(flows)

                                               flows <- dplyr::select(flows, -n_x, -node, -freq, -ymax, -ymin, -xmin, -n_next_x, -next_node, -flow_freq, -ymin_end, -ymax_end, -xmax_end, -cum_flow_freq, -cum_flow_freq_end)
                                               flows <- dplyr::mutate(flows, group = dplyr::row_number())

                                               as.data.frame(dplyr::mutate(flows, smooth = params$smooth))
                                             })



                                   },

                                   compute_group = function(data, scales) {

                                     out1 <- sigmoid(data$xmax, data$xmin_end, data$flow_start_ymax, data$flow_end_ymax,
                                                     smooth = data$smooth)
                                     out2 <- sigmoid(data$xmin_end, data$xmax, data$flow_end_ymin, data$flow_start_ymin,
                                                     smooth = data$smooth)
                                     dplyr::bind_rows(out1, out2)
                                   }
)


# FLOW SANKEYBUMP LAYER ---------
StatSankeyBumpFlow <- ggplot2::ggproto("StatSankeyBumpFlow", ggplot2::Stat,
                                   extra_params = c("na.rm", "type", "space", "smooth"),

                                   setup_data = function(data, params) {

                                     purrr::map_dfr(unique(data$PANEL),
                                             ~{
                                               data <- dplyr::filter(data, PANEL == .x)

                                               data <- dplyr::mutate(data, nodes = paste(node, x))
                                               data <- dplyr::arrange(data, x, -value)
                                               data <- dplyr::mutate(data, bbb = dplyr::row_number())
                                               data <- dplyr::arrange(data, bbb)
                                               data <- dplyr::mutate(data, nodes = forcats::fct_reorder(nodes, value, mean))
                                               data <- dplyr::arrange(data, node, x)
                                               data <- dplyr::group_by(data, node)
                                               data <- dplyr::mutate(data, next_x = dplyr::lead(x),
                                                        node = nodes,
                                                        next_node = dplyr::lead(nodes))
                                               data <- dplyr::ungroup(data)
                                               data <- dplyr::arrange(data, x, node)

                                               data <- dplyr::mutate(data, dplyr::across(c(x, next_x), ~as.numeric(.), .names = ("n_{.col}")))

                                               if(!("value" %in% names(data))) {
                                                 flow_data <- data
                                                 flow_data <- dplyr::mutate(flow_data, group = 1)
                                                 flow_data <- dplyr::group_by(flow_data, n_x, node, n_next_x, next_node)
                                                 flow_data <- dplyr::summarise(flow_data, flow_freq = dplyr::n(), .groups = "keep")
                                                 flow_data <- dplyr::ungroup(flow_data)

                                                 data <- dplyr::mutate(data, group = 1)
                                                 data <- dplyr::select(data, -n_next_x, -next_node)
                                                 data <- dplyr::group_by_all(data)
                                                 data <- dplyr::summarise(data, freq = dplyr::n(), .groups = "keep")
                                                 data <- dplyr::ungroup(data)
                                               } else {
                                                 flow_data <- data
                                                 flow_data <- dplyr::mutate(flow_data, group = 1)
                                                 flow_data <- dplyr::group_by(flow_data, n_x, node, n_next_x, next_node)
                                                 flow_data <- dplyr::summarise(flow_data, flow_freq = sum(value, na.rm = TRUE), .groups = "keep")
                                                 flow_data <- dplyr::ungroup(flow_data)

                                                 data <- dplyr::mutate(data, group = 1)
                                                 data <- dplyr::select(data, -n_next_x, -next_node)
                                                 data <- dplyr::group_by_at(data, dplyr::vars(dplyr::everything(), -value))
                                                 data <- dplyr::summarise(data, freq = sum(value, na.rm = TRUE), .groups = "keep")
                                                 data <- dplyr::ungroup(data)
                                               }

                                               if(is.null(params$space)) {
                                                 params$space <- find_default_space(data)
                                               }

                                               data <- dplyr::group_by(data, n_x)
                                               data <- dplyr::arrange(data, node)
                                               data <- dplyr::mutate(data, ymax = cumsum(freq) + (dplyr::row_number() - 1)*params$space,
                                                        ymin = ymax - freq)
                                               data <- dplyr::ungroup(data)

                                               if(params$type == "sankey") {
                                                 data <- dplyr::group_by(data, n_x)
                                                 data <- dplyr::mutate(data, ymin = ymin - max(ymax)/2,
                                                          ymax = ymax - max(ymax)/2)
                                                 data <- dplyr::ungroup(data)
                                               } else if (params$type == "alluvial"){
                                                 data <- data
                                               }

                                               data <- dplyr::mutate(data, xmin = n_x,
                                                        xmax = n_x)

                                               df <- data
                                               df <- dplyr::left_join(df, flow_data, by = c("n_x", "node"))

                                               flows <- df
                                               flows <- dplyr::left_join(flows, dplyr::distinct(dplyr::select(df, n_x, node, ymin_end = ymin, ymax_end = ymax, xmin_end = xmin, xmax_end = xmax, flow_freq_end = flow_freq)),
                                                           by = c("n_next_x" = "n_x", "next_node" = "node"))
                                               flows <- tidyr::drop_na(flows, n_x, node, next_node, n_next_x, ymax_end, ymin_end, xmax_end, xmin_end)
                                               flows <- dplyr::mutate(flows, r = dplyr::row_number())
                                               flows <- dplyr::arrange(flows, n_x, -r)
                                               flows <- dplyr::select(flows, -r)
                                               flows <- dplyr::group_by(flows, n_x, node)
                                               flows <- dplyr::mutate(flows, cum_flow_freq = cumsum(flow_freq) - flow_freq)
                                               flows <- dplyr::ungroup(flows)
                                               flows <- dplyr::group_by(flows, n_x, n_next_x, node, next_node)
                                               flows <- dplyr::mutate(flows, flow_start_ymax = ymax - cum_flow_freq,
                                                        flow_start_ymin = flow_start_ymax - flow_freq)

                                               flows <- dplyr::arrange(flows, n_x, n_next_x, next_node)
                                               flows <- dplyr::group_by(flows, n_next_x, next_node)
                                               flows <- dplyr::mutate(flows, cum_flow_freq_end = cumsum(flow_freq_end) - flow_freq_end)
                                               flows <- dplyr::mutate(flows, flow_end_ymax = ymax_end - cum_flow_freq_end,
                                                        flow_end_ymin = flow_end_ymax - flow_freq_end)
                                               flows <- dplyr::ungroup(flows)

                                               flows <- dplyr::select(flows, -n_x, -node, -freq, -ymax, -ymin, -xmin, -n_next_x, -next_node, -flow_freq, -ymin_end, -ymax_end, -xmax_end, -cum_flow_freq, -cum_flow_freq_end)
                                               flows <- dplyr::mutate(flows, group = dplyr::row_number())

                                               res <- flows
                                               res <- dplyr::rowwise(res)
                                               res <- dplyr::mutate(res, ..groupqq = stringr::str_remove(nodes, as.character(x)))
                                               res <- dplyr::ungroup(res)
                                               res <- dplyr::group_by(res, ..groupqq)
                                               res <- dplyr::mutate(res, group = dplyr::cur_group_id())
                                               res <- dplyr::ungroup(res)
                                               res <- dplyr::select(res, -..groupqq)
                                               res <- dplyr::mutate(res, smooth = params$smooth)
                                               as.data.frame(res)
                                             })
                                   },

                                   compute_group = function(data, scales) {

                                     out1 <- purrr::map_dfr(1:nrow(data), ~{
                                       datat <- data
                                       datat <- dplyr::slice(datat, .x)
                                       sigmoid(datat$xmax, datat$xmin_end, datat$flow_start_ymax, datat$flow_end_ymax,
                                               smooth = datat$smooth)
                                     })
                                     out1 <- dplyr::arrange(out1, x)
                                     out2 <- purrr::map_dfr(1:nrow(data), ~{
                                       datat <- data
                                       datat <- dplyr::slice(datat, .x)
                                       sigmoid(datat$xmin_end, datat$xmax, datat$flow_end_ymin, datat$flow_start_ymin,
                                               smooth = datat$smooth)
                                     })
                                     out2 <- dplyr::arrange(out2, -x)

                                     dplyr::bind_rows(out1, out2)
                                   }
)

# TEXT LAYER -------
StatSankeyText <- ggplot2::ggproto("StatSankeyText", ggplot2::Stat,
                                   extra_params = c("n_grid", "na.rm", "type", "width", "space"),

                                   setup_data = function(data, params) {

                                     purrr::map_dfr(unique(data$PANEL),
                                             ~{
                                               data <- dplyr::filter(data, PANEL == .x)

                                               data <- dplyr::mutate(data, dplyr::across(c(x, next_x), ~as.numeric(.), .names = ("n_{.col}")))

                                               if(!("value" %in% names(data))) {
                                                 data <- dplyr::mutate(data, group = 1)
                                                 data <- dplyr::select(data, -n_next_x, -next_node, -next_x)
                                                 data <- dplyr::group_by_all(data)
                                                 data <- dplyr::summarise(data, freq = dplyr::n(), .groups = "keep")
                                                 data <- dplyr::ungroup(data)
                                               } else {
                                                 data <- dplyr::mutate(data, group = 1)
                                                 data <- dplyr::select(data, -n_next_x, -next_node)
                                                 data <- dplyr::group_by_at(data, dplyr::vars(dplyr::everything(), -value))
                                                 data <- dplyr::summarise(data, freq = sum(value, na.rm = TRUE), .groups = "keep")
                                                 data <- dplyr::ungroup(data)
                                               }

                                               if(is.null(params$space)) {
                                                 params$space <- find_default_space(data)
                                               }

                                               data <- dplyr::group_by(data, n_x)
                                               data <- dplyr::mutate(data, ymax = cumsum(freq) + (dplyr::row_number() - 1)*params$space,
                                                        ymin = ymax - freq)
                                               data <- dplyr::ungroup(data)

                                               if(params$type == "sankey") {
                                                 data <- dplyr::group_by(data, n_x)
                                                 data <- dplyr::mutate(data, ymin = ymin - max(ymax)/2,
                                                          ymax = ymax - max(ymax)/2)
                                                 data <- dplyr::ungroup(data)
                                               } else if (params$type == "alluvial"){
                                                 data <- data
                                               }

                                               data <- dplyr::mutate(data, xmin = n_x - params$width/2,
                                                        xmax = n_x + params$width/2)

                                               data <- dplyr::mutate(data, x = n_x,
                                                        y = ymin + (ymax - ymin)/2)

                                               if("shift" %in% names(data)) {
                                                 data <- dplyr::mutate(data, dplyr::across(dplyr::contains("y"), ~ . + shift))
                                               }


                                               return(as.data.frame(data))
                                               })
                                   },

                                   compute_group = function(data, scales) {
                                     data
                                   }
)


# NODE LAYER -------
StatSankeyNode <- ggplot2::ggproto("StatSankeyNode", ggplot2::Stat,
                                   extra_params = c("n_grid", "na.rm", "type", "width", "space", "smooth"),

                                   setup_data = function(data, params) {

                                     purrr::map_dfr(unique(data$PANEL),
                                             ~{

                                               data <- dplyr::filter(data, PANEL == .x)
                                               data <- dplyr::mutate(data, dplyr::across(c(x, next_x), ~as.numeric(.), .names = ("n_{.col}")))

                                               if(!("value" %in% names(data))) {
                                                 data <- dplyr::mutate(data, group = 1)
                                                 data <- dplyr::select(data, -n_next_x, -next_node, -next_x)
                                                 data <- dplyr::group_by_all(data)
                                                 data <- dplyr::summarise(data, freq = dplyr::n(), .groups = "keep")
                                                 data <- dplyr::ungroup(data)
                                               } else {
                                                 data <- dplyr::mutate(data, group = 1)
                                                 data <- dplyr::select(data, -n_next_x, -next_node, -next_x)
                                                 data <- dplyr::group_by_at(data, dplyr::vars(dplyr::everything(), -value))
                                                 data <- dplyr::summarise(data, freq = sum(value, na.rm = TRUE), .groups = "keep")
                                                 data <- dplyr::ungroup(data)
                                               }

                                               if(is.null(params$space)) {
                                                 params$space <- find_default_space(data)
                                               }

                                               data <- dplyr::group_by(data, n_x)
                                               data <- dplyr::mutate(data, ymax = cumsum(freq) + (dplyr::row_number() - 1)*params$space,
                                                        ymin = ymax - freq)
                                               data <- dplyr::ungroup(data)

                                               if(params$type == "sankey") {
                                                 data <- dplyr::group_by(data, n_x)
                                                 data <- dplyr::mutate(data, ymin = ymin - max(ymax)/2,
                                                          ymax = ymax - max(ymax)/2)
                                                 data <- dplyr::ungroup(data)
                                               } else if (params$type == "alluvial"){
                                                 data <- data
                                               }

                                               data <- dplyr::mutate(data, xmin = n_x - params$width/2,
                                                        xmax = n_x + params$width/2)

                                               if("shift" %in% names(data)) {
                                                 data <- dplyr::mutate(data, dplyr::across(dplyr::contains("y"), ~ . + shift))
                                               }

                                               return(as.data.frame(data))
                                             })

                                   },

                                   compute_group = function(data, scales) {
                                     data
                                   }
)


# geom_sankey -------
#' @title geom_sankey
#'
#' @description Creates a sankey plot which visualize flows between nodes. Each observation needs to have a `x` aesthetic as well as a `next_x` column which declares where that observation should flow.
#' Also each observation should have a `node` and a `next_node` aesthetic which provide information about which group in the y-direction. By default each row of the data frame is counted to calculate the size of flows. A manual flow value can be added with the `value` aesthetic.
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param space space between nodes in the y-direction
#' @param type either 'sankey' or 'alluvial'
#' @param width width of nodes
#' @param smooth how much smooth should the curve have? More means steeper curve.
#' @param inherit.aes should the geom inherit aesthetics
#' @param ... other arguments to be passed to the geom
#'
#' @section Aesthetics:
#' geom_sankey understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **a**
#' - **b**
#' - **angle**
#' - m1
#' - m2
#' - color
#' - fill
#' - size
#' - linetype
#' - alpha
#' - lineend
#'
#' @return ggplot layer
#'
#' @export
geom_sankey <- function(mapping = NULL,
                        data = NULL,
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        space = NULL,
                        type = "sankey",
                        width = .1,
                        smooth = 8,
                        inherit.aes = TRUE,
                        ...
) {
  params_list <- prepare_params(...)

  list(
    flow = ggplot2::layer(
      stat = StatSankeyFlow,
      data = data,
      mapping = mapping,
      geom = "polygon",
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(
        list(
          na.rm = na.rm,
          width = width,
          space = space,
          smooth = smooth,
          type = type,
          params_list[[1]]
        )
      )
    ),

    node = ggplot2::layer(
      stat = StatSankeyNode,
      data = data,
      mapping = mapping,
      geom = ggplot2::GeomRect,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(
        list(
          na.rm = na.rm,
          width = width,
          space = space,
          smooth = smooth,
          type = type,
          params_list[[2]]
        )
      )
    )
  )


}

#' @title geom_sankey_label
#' @name geom_sankey_label
#' @aliases geom_sankey_text
#'
#' @description Creates centered labels or text in nodes of your sankey plot. Needs to have the exact same aesthetics as the call to `geom_sankey` to work.
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param space space between nodes in the y-direction
#' @param type either 'sankey' or 'alluvial'
#' @param width width of nodes
#' @param type Either `sankey` which centers around the x axis or `alluvial` which starts at y = 0 and moves upward.
#' @param inherit.aes should the geom inherit aesthetics
#' @param ... other arguments to be passed to the geom
#'
#' @return ggplot layer
#'
#' @rdname geom_sankey_label
#' @export
geom_sankey_label <- function(mapping = NULL,
                              data = NULL,
                              position = "identity",
                              na.rm = FALSE,
                              show.legend = NA,
                              space = NULL,
                              type = "sankey",
                              width = .1,
                              inherit.aes = TRUE,
                              ...) {
  # Prepare aesthetics for label
  label.aes <- list(...)

  list(
    label = ggplot2::layer(
      stat = StatSankeyText,
      data = data,
      mapping = mapping,
      geom = "label",
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(
        list(
          na.rm = na.rm,
          width = width,
          space = space,
          type = type,
          label.aes
        )
      )
    )
  )
}

#' @rdname geom_sankey_label
#' @export
geom_sankey_text <- function(mapping = NULL,
                             data = NULL,
                             position = "identity",
                             na.rm = FALSE,
                             show.legend = NA,
                             space = NULL,
                             type = "sankey",
                             width = .1,
                             inherit.aes = TRUE,
                             ...) {
  # Prepare aesthics for label
  label.aes <- list(...)

  list(
    label = ggplot2::layer(
      stat = StatSankeyText,
      data = data,
      mapping = mapping,
      geom = "text",
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(
        list(
          na.rm = na.rm,
          width = width,
          space = space,
          type = type,
          label.aes
        )
      )
    )
  )


}

## GEOM_ALLUVIAL
#' @title geom_alluvial
#'
#' @description Creates an alluvial plot which visualize flows between nodes. Each observation needs to have a `x` aesthetic as well as a `next_x` column which declares where that observation should flow.
#' Also each observation should have a `node` and a `next_node` aesthetic which provide information about which group in the y-direction.
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param space space between nodes in the y-direction
#' @param width width of nodes
#' @param smooth how much smooth should the curve have? More means steeper curve.
#' @param inherit.aes should the geom inherit aesthetics
#' @param ... other arguments to be passed to the geo
#'
#' @return ggplot layer
#'
#' @export
geom_alluvial <- function(mapping = NULL,
                          data = NULL,
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          space = 0,
                          width = .1,
                          smooth = 8,
                          inherit.aes = TRUE,
                          ...)  {
  geom_sankey(
    mapping = mapping,
    data = data,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    space = space,
    width = width,
    smooth = smooth,
    type = "alluvial",
    inherit.aes = inherit.aes,
    ...
  )
}

#' @title geom_alluvial_label
#' @name geom_alluvial_label
#' @aliases geom_alluvial_text
#'
#' @description Creates centered labels or text in nodes of your alluvial plot. Needs to have the exact same aesthetics as the call to `geom_alluvial` to work.
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param space space between nodes in the y-direction
#' @param width width of nodes
#' @param inherit.aes should the geom inherit aesthetics
#' @param ... other arguments to be passed to the geo
#'
#' @details Other important arguments is; `space` which proves the space between nodes in the y-direction; `shift` which shifts nodes in the y-direction.
#'
#' @return ggplot layer
#'
#' @rdname geom_alluvial_label
#' @export
geom_alluvial_text <- function(mapping = NULL,
                               data = NULL,
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               space = 0,
                               width = .1,
                               inherit.aes = TRUE,
                               ...)  {
  geom_sankey_text(
    mapping = mapping,
    data = data,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    space = space,
    width = width,
    type = "alluvial",
    inherit.aes = inherit.aes,
    ...
  )
}

#' @rdname geom_alluvial_label
#' @export
geom_alluvial_label <- function(mapping = NULL,
                                data = NULL,
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                space = 0,
                                width = .1,
                                inherit.aes = TRUE,
                                ...)  {
  geom_sankey_label(
    mapping = mapping,
    data = data,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    space = space,
    width = width,
    type = "alluvial",
    inherit.aes = inherit.aes,
    ...
  )
}

# geom_sankeybump
#' @title geom_sankey_bump
#'
#' @description Creates an alluvial plot which visualize flows between nodes. Each observation needs to have a `x` aesthetic as well as a `next_x` column which declares where that observation should flow.
#' Also each observation should have a `node` and a `next_node` aesthetic which provide information about which group in the y-direction.
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param type either 'sankey' or 'alluvial'
#' @param smooth how much smooth should the curve have? More means steeper curve.
#' @param inherit.aes should the geom inherit aesthetics
#' @param ... other arguments to be passed to the geo
#'
#' @details Other important arguments is; `space` which proves the space between nodes in the y-direction; `shift` which shifts nodes in the y-direction.
#'
#' @return ggplot layer
#'
#' @export
geom_sankey_bump <- function(mapping = NULL,
                             data = NULL,
                             position = "identity",
                             na.rm = FALSE,
                             show.legend = NA,
                             smooth = 8,
                             type = "sankey",
                             inherit.aes = TRUE,
                             ...) {
  params_list <- prepare_params(...)

  list(
    flow = ggplot2::layer(
      stat = StatSankeyBumpFlow,
      data = data,
      mapping = mapping,
      geom = "polygon",
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = purrr::flatten(list(
        na.rm = na.rm,
        type = type,
        smooth = smooth,
        params_list[[1]]
      ))
    )
  )


}

