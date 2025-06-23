# functions written by humans, roxygen documentation made by chatGPT o3

# package imports ----------------------------------------------------
#' @import ggplot2
#' @importFrom scales breaks_pretty squish hue_pal
#' @importFrom grid unit

NULL


fct2num <- function(x) {
  if (is.factor(x)) {
    return(as.numeric(as.character(x)))
  }
  as.numeric(x)
}

# -----------------------------------------------------------------------------
#' Convert hexadecimal colour to RGB components
#'
#' @param hex A character vector of hexadecimal colour strings (e.g. "#FF0000").
#'
#' @return A data.frame with columns `R`, `G` and `B` (0–255).
#' @export
hex_to_rgb <- function(hex) {
  rgb <- grDevices::col2rgb(hex)
  data.frame(
    R = as.numeric(rgb[1, ]),
    G = as.numeric(rgb[2, ]),
    B = as.numeric(rgb[3, ]),
    row.names = NULL
  )
}

# -----------------------------------------------------------------------------
#' Convert RGB components to hexadecimal colour
#'
#' @param rgb A data.frame produced by [hex_to_rgb()] or with numeric columns
#'   `R`, `G`, `B` (0–255).
#'
#' @return A character vector of hexadecimal colour strings.
#' @export
rgb_to_hex <- function(rgb) {
  grDevices::rgb(rgb$R, rgb$G, rgb$B, maxColorValue = 255)
}

# -----------------------------------------------------------------------------
#' Linearly interpolate a colour palette
#'
#' @param original_colors Character vector of hexadecimal colours that constitute
#'   the original palette.
#' @param n Integer giving the total number of colours wanted (> 1).
#'
#' @return A character vector of length `n` with interpolated colours.
#' @examples
#' interpolate_colors(c("#000000", "#FFFFFF"), 5)
#' @export
interpolate_colors <- function(original_colors, n) {
  stopifnot(is.numeric(n), n > 1)
  n_original <- length(original_colors)
  interpolated_colors <- character(n)
  
  steps_between <- (n_original - 1) / (n - 1)
  
  for (i in seq_len(n)) {
    lower_index <- floor((i - 1) * steps_between) + 1
    upper_index <- ceiling((i - 1) * steps_between) + 1
    
    lower_index <- min(lower_index, n_original)
    upper_index <- min(upper_index, n_original)
    
    lower_color <- hex_to_rgb(original_colors[lower_index])
    upper_color <- hex_to_rgb(original_colors[upper_index])
    
    interp_ratio <- ((i - 1) * steps_between) - (lower_index - 1)
    
    new_color <- round((1 - interp_ratio) * lower_color + interp_ratio * upper_color)
    interpolated_colors[i] <- rgb_to_hex(new_color)
  }
  
  interpolated_colors
}

add_ggpoint <- function(data,ggobject=ggplot2::ggplot(),colors=c("#A2A2A2","#F1F1F1"),sizes=c(0.7,0.5),alphas=c(0.7,0.9)) {
  #check that data is df
  if (!is.data.frame(data)) {
    cat("Data must be a data frame.")
    data=as.data.frame(data)
  }
  # check if data has colnames
  if (is.null(colnames(data)) ) {
    colnames(data) <- paste0("V", seq_len(ncol(data)))
    warning("Data has no column names, assigning default names V1, V2, ...")
  }
  ## check if alphas, sizes and colors are of the same length
  if (length(colors) != length(sizes) || length(colors) != length(alphas)) {
    stop("Length of colors, sizes, and alphas must be the same.")
  }
  # check that data has at least two columns and that the first two are numeric
  if (ncol(data) < 2 || !is.numeric(data[[1]]) || !is.numeric(data[[2]])) {
    stop("Data must have at least two numeric columns for x and y coordinates.")
  }
  
  for (n in 1:length(colors) ) {
    ggobject=ggobject + geom_point(data=data, aes(x=.data[[colnames(data)[1]]] , y=.data[[colnames(data)[2]]]),
                                   size=sizes[n], alpha=alphas[n], color= colors[n] )
  }
  ggobject
}

# -----------------------------------------------------------------------------
#' Add colour-coded points to an existing ggplot ( atm intended functionality not implemented . works as described below)
#'
#' @param data A data.frame whose first two columns give x/y coordinates and
#'   whose third column provides either group labels (factor/character) or a
#'   numeric value to be mapped to a colour scale.
#' @param ggobject A ggplot object; defaults to a new one.
#' @param colorscheme Character vector of colours to be used for the scale.  If
#'   `NULL` a palette is generated automatically.
#' @param size Point size.
#' @param alpha Point alpha.
#' @param color_limits Optional limits for the numeric colour scale.
#' @param shape Point shape (default 16 — filled circle).
#' @param brks Approximate number of breaks for the colour bar.
#'
#' @return The augmented ggplot object.
#' @export
add_ggpoint_groups <- function(data,
                               ggobject      = ggplot2::ggplot(),
                               colorscheme  = NULL,
                               size          = 1,
                               alpha         = 1,
                               color_limits  = NULL,
                               shape         = 16,
                               brks          = 3) {
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("The 'scales' package is required but not installed.")
  }
  
  # check if data is df
  if (!is.data.frame(data)) {
    cat("Data must be a data frame.")
    data <- as.data.frame(data)
  }
  #check if data has colnames
  if (is.null(colnames(data))) {
    colnames(data) <- paste0("V", seq_len(ncol(data)))
    warning("Data has no column names, assigning default names V1, V2, ...")
  }
  # check that data has at least three columns
  if (ncol(data) < 3) {
    stop("Data must have at least three columns for x, y coordinates and group labels.")
  }
  # check that the first two columns are numeric
  if (!is.numeric(data[[1]]) || !is.numeric(data[[2]])) {
    stop("The first two columns of data must be numeric for x and y coordinates.")
  }
  
  if (is.character(data[[3]])) {
    data[[3]] <- as.factor(data[[3]])
  }
  
  
  if (is.null(colorscheme)) {
    colorscheme = c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2")
    if (is.factor(data[[3]])) {
      colorscheme <- interpolate_colors(colorscheme,n=length(levels(data[[3]]))) 
    } 
  }
  
  
  ggobject <- ggobject +
    ggplot2::geom_point(
      data   = data,
      mapping = ggplot2::aes(
        x=.data[[colnames(data)[1]]] , y=.data[[colnames(data)[2]]],
        colour=.data[[colnames(data)[3]]]
      ),
      alpha  = alpha,
      size   = size,
      shape  = shape
    )
  
  if (is.factor(data[[3]])) {
    ggobject <- ggobject +
      ggplot2::scale_colour_manual(values = colorscheme)
  } else {
    ggobject <- ggobject +
      ggplot2::scale_colour_gradientn(
        colours = colorscheme,
        limits  = color_limits,
        breaks  = scales::breaks_pretty(n = brks),
        oob     = scales::squish
      )
  }
  
  ggobject
}

# -----------------------------------------------------------------------------
#' Scatter plot of 2-D coordinates coloured by a third variable
#'
#' Convenience wrapper that layers a background of neutral points, colour-coded
#' points and optional highlighting of specific rows.
#'
#' @param coords Matrix/data.frame with two numeric columns giving x/y positions.
#' @param values Vector/factor/matrix or data.frame giving per-point values for colouring. in case of multiple columns - first is used
#' @param highlight_points Optional vector giving row names to highlight.
#' @param ggObj Base ggplot object (created with `ggplot()`).
#' @param size_mult Multiplier applied to all point sizes.
#' @param colors Base palette used when `values` is discrete.
#' @param col_highl Colour used for highlighted points.
#' @param highl_shp Shape used for highlighted points.
#' @param show_high_text Logical; add labels next to highlighted points.
#' @param symmQuant Optional numeric (0–1) quantile defining symmetric limits
#'   for a diverging palette when `values` is numeric. 
#' @param dimnamesXYZ Column names for the plot data (x, y, colour).
#'
#' @return A ggplot object.
#' @export
ggscatter_colored <- function(coords,
                              values,
                              highlight_points = NULL,
                              ggObj            = ggplot2::ggplot(),
                              size_mult        = 1,
                              colors           = NULL,
                              col_highl        = "#be2312",
                              highl_shp        = 4,
                              show_high_text   = TRUE,
                              symmQuant        = NULL,
                              dimnamesXYZ      = NULL) {
  if (is.null(colors)) {
    colors = c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2")
  }
  # check if coords is a matrix or data.frame
  if (!is.matrix(coords) && !is.data.frame(coords)) {
    stop("coords must be a matrix or data.frame with at least two columns.")
  }
  # check if coords has at least two columns
  if (ncol(coords) < 2) {
    stop("coords must have at least two columns for x and y coordinates.")
  }
  # check if coords has numeric columns
  if (!is.numeric(coords[[1]]) || !is.numeric(coords[[2]])) {
    stop("The first two columns of coords must be numeric for x and y coordinates.")
  }
  
  
  if (is.vector(values) || is.factor(values)  ) {
    nameZ="z"
  } else if (is.matrix(values) || is.data.frame(values)) {
    if (is.null(colnames(values)[1])) {
      nameZ="z"
    } else {
      nameZ=colnames(values)[1]
    } 
    values = values[,1] 
    
  } else  { stop("values must be a vector, factor, matrix or data.frame") }
  
  
  
  ### extract colnames from coords
  if (is.null(dimnamesXYZ)) {
    if (is.null(colnames(coords))) {
      dimnamesXY <- c("x", "y")
    } else {
      dimnamesXY <- colnames(coords)[1:2]
    }
    dimnamesXYZ <- c(dimnamesXY, nameZ)
  } else {
    if (length(dimnamesXYZ) < 3) {
      warning("dimnamesXYZ should have at least 3 elements, making names")
      dimnamesXYZ <- c("x", "y", nameZ)
    }
  }

  
  # Symmetric limits for diverging palette -----------------------------------
  lims <- NULL
  if (!is.null(symmQuant)) {
    if (!is.numeric(symmQuant) || symmQuant < 0 || symmQuant > 1) {
      stop("symmQuant must be numeric in [0, 1]")
    }
    
    qnts1 <- abs(stats::quantile(values, symmQuant, na.rm = TRUE))
    qnts2 <- abs(stats::quantile(values, 1 - symmQuant, na.rm = TRUE))
    maxQ  <- max(qnts1, qnts2)
    lims  <- c(-maxQ, maxQ)
  }
  
  # Data frame to plot --------------------------------------------------------
  to_plot <- data.frame(
    x       = coords[, 1],
    y       = coords[, 2],
    cluster = values,
    stringsAsFactors = FALSE
  )
  colnames(to_plot) <- dimnamesXYZ
  
  # Build the plot -----------------------------------------------------------
  colorscheme <- interpolate_colors(colors, n = length(unique(to_plot[[3]])))
  plot_ <- add_ggpoint(
    to_plot,
    ggObj,
    sizes  = c(2.2, 1.9) * size_mult,
    colors = c("#252525", "#fafafa"),
    alphas = c(1, 1)
  )
  
  plot_ <- add_ggpoint_groups(
    to_plot,
    plot_,
    colorscheme = colorscheme,
    size         = 2 * size_mult,
    color_limits = lims
  ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.title    = ggplot2::element_text(size = 8),
      legend.text     = ggplot2::element_text(size = 8),
      legend.key.size = grid::unit(0.45, "cm"),
      plot.title      = ggplot2::element_text(size = 12, face = "bold"),
      axis.title      = ggplot2::element_text(size = 11)
    )
  
  # Highlight points ---------------------------------------------------------
  if (!is.null(highlight_points)) {
    if (is.null(names(highlight_points))) {
      names(highlight_points) <- highlight_points
    }
    
    plot_ <- plot_ +
      ggplot2::geom_point(
        data   = to_plot[rownames(to_plot) %in% highlight_points, , drop = FALSE],
        mapping = ggplot2::aes(x = .data[[colnames(to_plot)[1] ]], y = .data[[colnames(to_plot)[2] ]] ),
        shape  = highl_shp,
        colour = col_highl,
        size   = 6 * size_mult,
        alpha  = 1
      )
    
    if (show_high_text) {
      to_pl_text <- to_plot[highlight_points, , drop = FALSE]
      
      plot_ <- plot_ +
        ggplot2::geom_text(
          data   = to_pl_text,
          mapping = ggplot2::aes(x = .data[[colnames(to_plot)[1] ]], y = .data[[colnames(to_plot)[2] ]], label = names(highlight_points)),
          vjust  = 0,
          hjust  = 0,
          size   = 10 * size_mult,
          colour = "#fefefe"
        ) +
        ggplot2::geom_text(
          data   = to_pl_text,
          mapping = ggplot2::aes(x = .data[[colnames(to_plot)[1] ]], y = .data[[colnames(to_plot)[2] ]] , label = names(highlight_points)),
          vjust  = 0.04,
          hjust  = 0.04,
          size   = 10 * size_mult,
          colour = "#212121"
        )
    }
    
    plot_ <- plot_ + ggplot2::theme_light()
  }
  
  plot_
}




#' Density estimation helpers using spatstat
#'
#' This script provides three helper functions for fast kernel density
#' estimation of 2‑D point patterns using the **spatstat** family of packages
#' (`ppp`, `density.ppp`, `owin`, `interp.im`).  

NULL

require(spatstat)
#' Expand a numeric vector by symmetric margins
#'
#' Computes a two‑element numeric vector giving the lower and upper bounds of
#' *x* expanded by *mult* × range(x).
#'
#' @param x     Numeric vector.
#' @param mult  Numeric scalar ≥ 0. Fraction of the data range to be added as a
#'              margin (default 0.1 = ±10 % of the range).
#'
#' @return Numeric vector of length 2: `c(min‑margin, max+margin)`.
#' @export
#'
#' @examples
#' get_plot_margins(1:10, mult = 0.2)
get_plot_margins <- function(x, mult = 0.1) {
  rng <- range(x)
  d   <- diff(rng)
  margin <- c(-d, d) * mult
  rng + margin
}

#' Fast kernel density estimate for a point pattern
#'
#' Thin wrapper around [spatstat.explore::density.ppp()] that accepts raw x/y
#' coordinates instead of a *ppp* object.
#'
#' @param umap   Two‑column numeric matrix / data frame with x/y coordinates.
#' @param window [`spatstat.geom::owin`] object describing the observation
#'               window.
#' @param adjust Bandwidth adjustment factor passed to
#'               [spatstat.explore::density.ppp()].
#' @param at     Grid type: "pixels" or "points" (default "pixels").
#' @param diggle Logical; if `TRUE` perform Diggle correction.
#' @param weights Optional numeric vector of per‑point weights.
#'
#' @return A [`spatstat::im`] object.
#' @export
#'
#' @examples

get_dens_ppp <- function(umap,
                         window,
                         adjust  = 1,
                         at      = "pixels",
                         diggle  = FALSE,
                         weights = NULL) {
  

  stopifnot(ncol(umap) == 2)
  
  pts <- spatstat.geom::ppp(x = umap[, 1],
                            y = umap[, 2],
                            window = window)
  
  spatstat.explore::density.ppp(pts,
                             adjust = adjust,
                             at     = at,
                             diggle = diggle,
                             weights = weights)
}

#' Convenience wrapper returning a full density image plus per‑point densities
#'
#' This helper chooses a suitable observation window (unless supplied),
#' computes the pixel density image, its long format `(x, y, density)`, and the
#' density values at the original data points.
#'
#' @param data    Two‑column numeric matrix / data frame with x/y coordinates.
#' @param window  Optional [`spatstat.geom::owin`] object.  If `NULL`
#'                the window is expanded by `margins` using [get_plot_margins()].
#' @param adjust  Bandwidth adjustment factor (default 0.25).
#' @param margins Numeric scalar; fraction of data range used as margin when
#'                `window = NULL`.
#' @param diggle  Logical; if `TRUE` perform Diggle correction.
#' @param weights Optional numeric vector of per‑point weights.
#'
#' @return A list containing:
#' \describe{
#'   \item{`im`}{Pixel density image (inherits from class **im**).}
#'   \item{`long_dens`}{`data.frame` with columns x, y, density.}
#'   \item{`original_points_dens`}{`data.frame` with the original coordinates
#'     and their density values.}
#' }
#' @export
#'
#' @examples

get_density2d_spatstat <- function(data,
                                       window  = NULL,
                                       adjust  = 0.25,
                                       margins = 0.1,
                                       diggle  = FALSE,
                                       weights = NULL) {
  
  stopifnot(ncol(data) == 2)
  
  if (is.null(window)) {
    window <- spatstat.geom::owin(get_plot_margins(data[, 1], margins),
                                  get_plot_margins(data[, 2], margins))
  }
  
  fit <- get_dens_ppp(data,
                      window  = window,
                      adjust  = adjust,
                      diggle  = diggle,
                      weights = weights)
  

  fit$long_dens <- cbind(expand.grid(fit$xcol, fit$yrow),
                         Density = as.vector(fit$v))
  
  orig_dens <- spatstat.geom::interp.im(fit,
                                        x = data[, 1],
                                        y = data[, 2])
  
  fit$original_points_dens <- cbind(data,
                                    Density = as.vector(orig_dens))
  
  fit
}




#' ggshape_heatmap – Circle-based Heat-map with Optional Clustering
#'
#' Draws a heat-map where each matrix element is represented by a glyph whose
#' **fill colour** encodes a numeric value and whose **size** encodes a second
#' numeric value.  Rows and/or columns can be hierarchically clustered before
#' plotting to reveal structure.
#'
#' @section Dependencies:
#'   * **ggplot2** – plotting backend
#'   * **colorspace** – for perceptual lightening/darkening of outline colours
#'
#' @param data `matrix` **(square)** \cr
#'   Numeric matrix providing the fill-colour values.  The matrix must be
#'   square (same number of rows and columns).
#'
#' @param data_sizes `matrix`, *optional* \cr
#'   Numeric matrix providing the glyph-size values.  Must be the same shape as
#'   `data`.  If `NULL` (default) the function uses `data` itself, so size is
#'   proportional to the magnitude of the colour values.
#'
#' @param shape_values `integer(1)` \cr
#'   Point shape code passed to **ggplot2**.  Values `21–25` are filled shapes
#'   with an outline and therefore the most useful (default `22`, a square).
#'
#' @param size_range `numeric(2)` \cr
#'   Minimum and maximum point size on the plot.
#'
#' @param colorscheme `character` or `function` \cr
#'   Either a vector of colours (hex or R-named) or a palette function (e.g.
#'   one returned by `colorRampPalette()`).  Determines the gradient used to
#'   fill the glyphs.
#'
#' @param value_label `character(1)` \cr
#'   Legend title for the colour scale.
#'
#' @param size_label `character(1)` \cr
#'   Legend title for the size scale.
#'
#' @param row_label `character(1)` \cr
#'   Y-axis label.
#'
#' @param column_label `character(1)` \cr
#'   X-axis label.
#'
#' @param title `character(1)` \cr
#'   Plot title.
#'
#' @param theme_choice `ggplot2` theme \cr
#'   A complete ggplot2 theme object to apply.  Default: `theme_minimal()`.
#'
#' @param legend.key.size, legend.text.size Numeric.  Passed to `theme()` to
#'   tweak legend key dimensions and title text size, respectively.
#'
#' @param cluster_by `character(1)` or `NULL` \cr
#'   Which matrix to use when computing distances for clustering:  
#'   * `'data'` – colour matrix (default)  
#'   * `'data_sizes'` – size matrix  
#'   * `'both'` – sum of the two distance matrices  
#'   * `NULL` – equivalent to `'data'`.
#'
#' @param cluster_rows, cluster_cols `logical(1)` \cr
#'   Should rows and/or columns be hierarchically clustered and reordered in
#'   the plot?
#'
#' @param na_rm `logical(1)` \cr
#'   If `TRUE`, removes `NA` values before plotting.
#'
#' @param text.angle.x, text.angle.y `numeric(1)` \cr
#'   Rotation angles (degrees) for axis tick labels.
#'
#' @param darken_outline_color_coeff `numeric(1)` \cr
#'   Coefficient for darkening (`>0`) or lightening (`<0`) the outline colour
#'   relative to the fill.  Use `0` for no change.  Values are interpreted by
#'   **colorspace**’s `darken()` / `lighten()` with `method = "relative"`.
#'
#' @param grid.pars `list` \cr
#'   Three optional components controlling panel grid lines:  
#'   * `grid.size`   – line width (set ≤0 to remove)  
#'   * `grid.color`  – colour  
#'   * `grid.linetype` – linetype string (`"solid"`, `"dashed"`, …).
#'
#' @return A **ggplot** object.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(1)
#'   n  <- 6
#'   cm <- matrix(runif(n*n, -1, 1), n)
#'   sm <- matrix(abs(rnorm(n*n, 5, 2)), n)
#'   rownames(cm) <- colnames(cm) <- paste0("Sample", 1:n)
#'   rownames(sm) <- colnames(sm) <- rownames(cm)
#'   ggshape_heatmap(cm, sm, cluster_rows = TRUE, cluster_cols = TRUE)
#' }
#'
#' @export
#' @import ggplot2
#' @importFrom colorspace darken lighten

ggshape_heatmap <- function(
    data,
    data_sizes  = NULL,
    shape_values = 22,
    size_range   = c(2, 8),
    colorscheme  = rev(c('#9C0824','#BF1316','#D42922','#E96251','#EBA49A','#B0B0B0','#838383','#5D5D5D', '#3B3B3B' ,'#1E1E1E')) ,
    value_label  = "Value",
    size_label   = "Size",
    row_label    = "Sample",
    column_label = "Measurement",
    title        = "ggCircle",
    theme_choice    = ggplot2::theme_minimal(),
    legend.key.size = 0.5,
    legend.text.size = 11,
    cluster_by   = "data",
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    na_rm     = FALSE,
    text.angle.x = 0,
    text.angle.y = 0,
    darken_outline_color_coeff = -1,
    grid.pars = list(grid.size = 0.15,
                     grid.color = "#dddddd",
                     grid.linetype = "solid")
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  if (is.null(data_sizes)) data_sizes <- data
  if (!is.matrix(data))       data       <- as.matrix(data)
  if (!is.matrix(data_sizes)) data_sizes <- as.matrix(data_sizes)
  
  stopifnot(identical(dim(data), dim(data_sizes)))
  stopifnot(nrow(data) == ncol(data))
  
  if (is.null(rownames(data))) {
    rn <- seq_len(nrow(data))
    rownames(data) <- rownames(data_sizes) <- rn
  }
  if (is.null(colnames(data))) {
    cn <- seq_len(ncol(data))
    colnames(data) <- colnames(data_sizes) <- cn
  }
  
  # --- clustering ---------------------------------------------------------
  get_order <- function(mat, by_row = TRUE)
    hclust(dist(if (by_row) mat else t(mat)), method = "complete")$order
  
  if (cluster_rows || cluster_cols) {
    chosen <- switch(cluster_by %||% "data",
                     data        = data,
                     data_sizes  = data_sizes,
                     both        = NULL,
                     stop("cluster_by must be 'data', 'data_sizes', 'both', or NULL"))
    
    if (cluster_rows) {
      ord <- if (is.null(chosen)) {
        d <- as.matrix(dist(data)) + as.matrix(dist(data_sizes))
        get_order(as.dist(d), TRUE)
      } else get_order(chosen, TRUE)
      data       <- data      [ord, , drop = FALSE]
      data_sizes <- data_sizes[ord, , drop = FALSE]
    }
    if (cluster_cols) {
      ord <- if (is.null(chosen)) {
        d <- as.matrix(dist(t(data))) + as.matrix(dist(t(data_sizes)))
        get_order(as.dist(d), TRUE)
      } else get_order(chosen, FALSE)
      data       <- data      [, ord, drop = FALSE]
      data_sizes <- data_sizes[, ord, drop = FALSE]
    }
  }
  
  # --- outline palette ----------------------------------------------------
  outline_colors <- if (darken_outline_color_coeff < 0)
    colorspace::lighten(colorscheme, abs(darken_outline_color_coeff),
                        space = "combined", method = "relative")
  else if (darken_outline_color_coeff > 0)
    colorspace::darken(colorscheme, darken_outline_color_coeff,
                       space = "combined", method = "relative")
  else colorscheme
  
  # --- long format (factor levels fixed) ----------------------------------
  row_lev <- rownames(data)
  col_lev <- colnames(data)
  
  df <- data.frame(
    Row    = factor(rep(row_lev,  times = ncol(data)), levels = row_lev),
    Column = factor(rep(col_lev, each  = nrow(data)), levels = col_lev),
    Value  = as.vector(data),
    Size   = as.vector(data_sizes),
    stringsAsFactors = FALSE
  )
  if (na_rm) df <- stats::na.omit(df)
  
  # --- plot ---------------------------------------------------------------
  p <- ggplot2::ggplot(df, aes(x = Column, y = Row)) +
    ggplot2::geom_point(aes(fill = Value, size = Size, colour = Value),
                        shape = shape_values) +
    ggplot2::scale_size(range = size_range, name = size_label) +
    ggplot2::scale_fill_gradientn(colours = colorscheme, name = value_label) +
    ggplot2::scale_colour_gradientn(colours = outline_colors, guide = "none") +
    theme_choice +
    ggplot2::labs(title = title, x = column_label, y = row_label)
  
  grid_size     <- grid.pars$grid.size     %||% 0.15
  grid_color    <- grid.pars$grid.color    %||% "#dddddd"
  grid_linetype <- grid.pars$grid.linetype %||% "solid"
  
  p + ggplot2::theme(
    panel.grid.major = if (grid_size > 0)
      ggplot2::element_line(size = grid_size, colour = grid_color,
                            linetype = grid_linetype)
    else ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(angle = text.angle.x, hjust = 1),
    axis.text.y      = ggplot2::element_text(angle = text.angle.y, hjust = 1),
    legend.key.size  = grid::unit(legend.key.size, "cm"),
    legend.title     = ggplot2::element_text(size = legend.text.size)
  )
}
