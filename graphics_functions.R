# functions written by humans, roxygen documentation made by chatGPT o3
library(ggplot2)
library(scales)
library(grid)
# package imports ----------------------------------------------------
#' @import ggplot2
#' @importFrom scales breaks_pretty squish hue_pal
#' @importFrom grid unit




source("https://raw.githubusercontent.com/YevhenAkimov/general_purpose_R/main/general_helpers.R")


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
                              dimnamesXYZ      = NULL,
                              gg_theme = NULL,
                              brks=5
                             ) {
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
    color_limits = lims,
    brks=brks
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
    
    plot_ <- plot_ 
  }
  if (is.null(gg_theme)) {
    return(plot_)
  } else {
    return(plot_+gg_theme())
    }
  
}


ggscatter_color_multi=function( coords, values, highlight_points=NULL, ggObj=ggplot(), 
                               titles=NULL,
                               size_mult=1,colors= c("#9e0142","#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#66c2a5","#3288bd","#5e4fa2"),col_highl="#be2312",
                               highl_shp=4, show_high_text=T, plots_per_row = NULL,
                               symmQuant=NULL, color_text="value", legend.text.angle=90,
                               legend.key.size=0.4, legend.text.size=7,legend.position="bottom",  gg_theme = NULL ){
  
  library(gridExtra)
  library(grid)
  
  if (!is.data.frame(values)) {
    if (is.matrix(values)) {
      values <- as.data.frame(values)
    } else {
      values <- cbind.data.frame(values = values)
    }
  }
  
  n = ncol(values)
  
  # Determine grid layout
  if (is.null(plots_per_row)) {
    plots_per_row = ceiling(sqrt(n))
  }
  
  plot_list = vector("list", length = n)
  
  if (is.null(titles)) {
    titles = colnames(values)
  }
  for (i in 1:n) {
    plot_list[[i]] = ggscatter_colored(coords=coords,
                                    values=values[,i],
                                    highlight_points=highlight_points,
                                    ggObj=ggObj,
                                    size_mult=size_mult,
                                    colors=colors,
                                    col_highl=col_highl,
                                    highl_shp=highl_shp,
                                    show_high_text=show_high_text,
                                    symmQuant=symmQuant,gg_theme=gg_theme)+
      
      ggtitle(titles[i]) +labs(color =color_text)+
      theme( legend.position = legend.position, 
             legend.title = element_text(size = legend.text.size), 
             legend.text = element_text(size = legend.text.size,
                                        angle = legend.text.angle),   
             legend.key.size = unit(legend.key.size, "cm"))
  }
  
  arranged_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = plots_per_row))
  
  return(arranged_plots)
  
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
  x <- as.numeric(x)                  # force an atomic numeric vector
  x <- x[is.finite(x)]                # drop NA/Inf that can upset range()
  if (!length(x)) return(c(0, 1))     # fallback if column is empty after filtering
  rng <-  base::range(x)                     # base::range
  d   <-  base::diff(rng)                    # base::diff expects numeric
  rng + c(-d, d) * mult
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


#' Scatter plot coloured by point density
#'
#' A thin wrapper around `ggscatter_colored()` that first estimates the
#' two-dimensional kernel density of `coords` using
#' `get_density2d_spatstat()` and then feeds the resulting per-point densities
#' (`original_points_dens`) back to `ggscatter_colored()` as the colouring
#' variable.
#'
#' @inheritParams ggscatter_colored
#' @param adjust,margins,diggle,weights Passed to `get_density2d_spatstat()` to
#'   control the kernel density estimate.
#'
#' @return A **ggplot2** object.
#' @export
ggscatter_density <- function(coords,
                                    highlight_points = NULL,
                                    ggObj            = ggplot2::ggplot(),
                                    size_mult        = 1,
                                    colors           = NULL,
                                    col_highl        = "#be2312",
                                    highl_shp        = 4,
                                    show_high_text   = TRUE,
                                    symmQuant        = NULL,
                                    dimnamesXYZ      = NULL,
                                    adjust           = 0.25,
                                    margins          = 0.1,
                                    diggle           = FALSE,
                                    weights          = NULL) {
  
  # --- ensure every coordinate column is numeric ------------------------
  coords <- dplyr::mutate(
    coords,
    dplyr::across(dplyr::everything(), as.numeric)
  )%>% as.data.frame()
  
  # --- estimate density -------------------------------------------------
  dens_res <- get_density2d_spatstat(
    data    = coords,
    adjust  = adjust,
    margins = margins,
    diggle  = diggle,
    weights = weights
  )
  
  # --- per-point densities ----------------------------------------------
  values <- dens_res$original_points_dens$Density
  
  # --- delegate to ggscatter_colored ------------------------------------
  ggscatter_colored(
    coords           = coords,
    values           = values,
    highlight_points = highlight_points,
    ggObj            = ggObj,
    size_mult        = size_mult,
    colors           = colors,
    col_highl        = col_highl,
    highl_shp        = highl_shp,
    show_high_text   = show_high_text,
    symmQuant        = symmQuant,
    dimnamesXYZ      = dimnamesXYZ
  )
}
#' Weighted 2D density contours over scatter coordinates
#'
#' Draws filled and line **kernel density estimate** (KDE) contours for one or
#' more phenotypes (weight columns) on top of optional points. Supports optional
#' centering and per-axis standard-deviation scaling, or log scaling, before KDE
#' estimation; the grid is then mapped back to the original coordinate space for
#' plotting. A legend is shown only when `weights` is supplied by the user.
#'
#' @param coords A data frame or matrix whose first two columns are numeric
#'   coordinates. The first two columns are used as `x`/`y` (and are internally
#'   renamed to `x`/`y`), while axis labels keep the original column names.
#' @param weights `NULL`, a numeric vector of length `nrow(coords)`, or a
#'   numeric data frame/matrix with `nrow(coords)` rows. If `NULL`, a single
#'   uniform phenotype is assumed and no legend is drawn. If a vector, it is
#'   treated as one phenotype; if a data frame/matrix, each column is a
#'   phenotype (weight) to contour.
#' @param scale Character or logical; one of `"none"`, `"sd"`, or `"log"`.
#'   Logical values are coerced to `"sd"` (for `TRUE`) or `"none"` (for `FALSE`).
#'   * `"sd"` scales `x` and `y` by their sample SDs (after optional centering)
#'   before KDE; the contour grid is inverse-transformed for plotting.  
#'   * `"log"` applies `log1p()` to `x`/`y` before KDE and inverse-transforms
#'   with `exp() - 1` for plotting (requires non-negative coordinates; see
#'   **Notes**).  
#'   * `"none"` uses raw coordinates (possibly after centering).
#' @param center Logical. If `TRUE`, subtracts the common mean of the pooled
#'   `c(x, y)` from both axes prior to any scaling. Ignored when `scale = "log"`.
#' @param ggObj A `ggplot2::ggplot()` object to add layers to. Defaults to an
#'   empty plot.
#' @param colors Character vector of base fill colors (one per phenotype). If
#'   fewer colors than phenotypes are provided, the palette is extended with
#'   `interpolate_colors()`; excess colors are truncated.
#' @param show_points Logical; add the raw points beneath the contours.
#' @param point_size Numeric point size for the scatter layer.
#' @param point_color Color for the scatter layer points.
#' @param adjust Numeric bandwidth adjustment passed to the KDE helper
#'   `get_density2d_spatstat()` (larger values produce smoother contours).
#' @param countour_color Color for the contour *lines* (note the parameter
#'   name spelling).
#' @param alpha_line Numeric alpha for the contour lines.
#' @param alpha_range Numeric length-2 vector giving the range used by
#'   `ggplot2::scale_alpha_discrete()` for the filled contour levels.
#' @param size Numeric line width for the contour lines.
#' @param gg_theme A theming function (e.g., `ggplot2::theme_light`) applied to
#'   the plot.
#' @param darken_value Numeric passed to `colorspace::darken()` to derive the
#'   filled contour color from `colors` for each phenotype.
#' @param contour_breaks Numeric vector of contour break values (on the
#'   min–max normalized density scale in `[0, 1]`) used for both filled and
#'   line contours.
#' @param ... Additional arguments forwarded to `get_density2d_spatstat()`.
#'
#' @details
#' - **Input checks:** `coords` must have at least two numeric columns. All
#'   columns of `weights` (if provided) must be numeric and have the same number
#'   of rows as `coords`.  
#' - **Transform → KDE → inverse transform:** The function optionally centers
#'   and/or scales/log-transforms coordinates **before** KDE; after estimating
#'   densities on a grid, grid coordinates are inverse-transformed back to the
#'   original scale so axes remain interpretable.  
#' - **Per-phenotype normalization:** For each phenotype, density values are
#'   min–max normalized to `[0, 1]` via `min_max_normalization()` so that
#'   `contour_breaks` and alpha mapping are comparable across phenotypes.  
#' - **Legend behavior:** A discrete "Phenotype" legend is drawn only when
#'   `weights` is user-supplied (vector or multi-column). With `weights = NULL`
#'   (uniform), the legend is suppressed.  
#' - **Color handling:** If `length(colors) < n_pheno`, the palette is extended
#'   using `interpolate_colors()`. Each phenotype’s filled contours use a
#'   darkened version of the corresponding color via `colorspace::darken()`.
#'
#' @note
#' - When `scale = "log"`, negative coordinates are not allowed and `center` is
#'   ignored (a warning is emitted).  
#' - When `scale = "sd"`, zero variance on any axis results in an error.  
#' - This function relies on helper utilities `get_density2d_spatstat()`,
#'   `interpolate_colors()`, and `min_max_normalization()` being available in
#'   your package namespace.
#'
#' @return A `ggplot` object with optional points and per-phenotype filled and
#'   line density contours.
#'
#' @examples
#' set.seed(1)
#' n <- 500
#' coords <- data.frame(X = rnorm(n, 0, 1), Y = rnorm(n, 0, 1))
#'
#' # Two phenotypes (weights as columns) with SD scaling and centering
#' w <- data.frame(A = runif(n), B = runif(n))
#' p <- ggdensity_contours(coords, weights = w, scale = "sd", center = TRUE,
#'                         contour_breaks = seq(0.2, 1, length.out = 6))
#' print(p)
#'
#' # Single (uniform) phenotype, no points, custom theme
#' ggdensity_contours(coords, show_points = FALSE, gg_theme = ggplot2::theme_minimal)
#'
#' @seealso ggplot2::geom_contour_filled, ggplot2::geom_contour,
#'   colorspace::darken, reshape2::melt
#' @importFrom stats sd setNames
#' @importFrom reshape2 melt
#' @importFrom colorspace darken
#' @export
ggdensity_contours <- function(coords,
                                    weights        = NULL,
                                    scale          = "none",   # "none", "sd", "log", or TRUE/FALSE
                                    center         = FALSE,
                                    ggObj          = ggplot2::ggplot(),
                                    colors         =  c('#5F4690','#1D6996','#0F8554','#EDAD08','#CC503E','#94346E','#1C1F33','#93A8AC','#A16928','#065256'),
                                    show_points    = TRUE,
                                    point_size     = 0.2,
                                    point_color    = "#a1a1a1",
                                    adjust         = 0.1,
                                    countour_color = "#ffffff",
                                    alpha_line     = 0.5,
                                    alpha_range    = c(0.1, 0.35),
                                    size           = 0.5,
                                    gg_theme       = ggplot2::theme_light,
                                    darken_value =0.2,
                                    contour_breaks = seq(0.25, 1, length.out = 5),
                                    ...) {
  
  ## Legend is shown only when user supplied non-NULL weights
  legend_required <- !is.null(weights)
  
  ## ------------------------------------------------------------------ ##
  ## 0. Normalise the `scale` flag ------------------------------------ ##
  ## ------------------------------------------------------------------ ##
  if (is.logical(scale))
    scale <- if (scale) "sd" else "none"
  scale <- match.arg(tolower(scale), c("none", "sd", "log"))
  
  if (scale == "log" && center) {
    warning("`center` is ignored when `scale = \"log\"`.")
    center <- FALSE
  }
  
  ## ------------------------------------------------------------------ ##
  ## 1. Basic coordinate checks --------------------------------------- ##
  ## ------------------------------------------------------------------ ##
  if (!is.data.frame(coords) && !is.matrix(coords))
    stop("`coords` must be a data frame or matrix.")
  coords <- as.data.frame(coords)
  
  if (ncol(coords) < 2L || !is.numeric(coords[[1]]) || !is.numeric(coords[[2]]))
    stop("First two columns of `coords` must be numeric x/y coordinates.")
  
  x_lab <- colnames(coords)[1]
  y_lab <- colnames(coords)[2]
  names(coords)[1:2] <- c("x", "y")      # standard names
  
  ## ------------------------------------------------------------------ ##
  ## 1a. Transform coordinates for KDE (single μ, axis-specific σ) ----- ##
  ## ------------------------------------------------------------------ ##
  x  <- coords$x
  y  <- coords$y
  
  mu     <- 0        # shared mean
  sd_x   <- 1
  sd_y   <- 1
  
  if (center) {
    mu <- mean(c(x, y))
    x  <- x - mu
    y  <- y - mu
  }
  
  if (scale == "sd") {
    sd_x <- stats::sd(x)
    sd_y <- stats::sd(y)
    if (sd_x == 0 || sd_y == 0)
      stop("Cannot scale by SD: zero variance detected.")
    x <- x / sd_x
    y <- y / sd_y
  } else if (scale == "log") {
    if (any(x < 0) || any(y < 0))
      stop("Negative coordinates cannot be log-transformed; use scale = \"sd\" or \"none\".")
    x <- log1p(x)
    y <- log1p(y)
  }
  
  coords_kde <- data.frame(x = x, y = y)
  
  ## ------------------------------------------------------------------ ##
  ## 2. Prepare weights ----------------------------------------------- ##
  ## ------------------------------------------------------------------ ##
  if (is.null(weights)) {
    weights <- data.frame(uniform = rep(1, nrow(coords)), check.names = FALSE)
  } else if (is.vector(weights)) {
    weights <- data.frame(phenotype = weights, check.names = FALSE)
  } else {
    weights <- as.data.frame(weights)
  }
  
  if (!all(vapply(weights, is.numeric, logical(1))))
    stop("All `weights` columns must be numeric.")
  if (nrow(weights) != nrow(coords))
    stop("`weights` must have the same number of rows as `coords`.")
  
  n_pheno <- ncol(weights)
  pheno_names <- colnames(weights)
  if (is.null(pheno_names))
    pheno_names <- paste0("phenotype_", seq_len(n_pheno))
  
  ## ------------------------------------------------------------------ ##
  ## 2a. Ensure enough colors for phenotypes --------------------------- ##
  ## ------------------------------------------------------------------ ##
  if (length(colors) < n_pheno) {
    warning(sprintf("`colors` length (%d) is less than number of phenotypes (%d); extending palette with interpolate_colors().",
                    length(colors), n_pheno))
    colors <- interpolate_colors(colors, n_pheno)
  } else if (length(colors) > n_pheno) {
    colors <- colors[seq_len(n_pheno)]
  }
  message("Number of phenotypes: ", n_pheno)
  message("Number of colors: ", length(colors))
  
  ## ------------------------------------------------------------------ ##
  ## 3. KDE + inverse transform of grid --------------------------------##
  ## ------------------------------------------------------------------ ##
  dens_list <- lapply(seq_len(n_pheno), function(i) {
    
    ## ---- 3a. Exclude zero-weighted points for this phenotype -------- ##
    w_i <- weights[[i]]
    idx <- w_i > 0 & !is.na(w_i)
    if (!any(idx))
      stop("All weights are 0 (or NA) for phenotype ", colnames(weights)[i], ".")
    
    coords_sub   <- coords_kde[idx, , drop = FALSE]
    weights_sub  <- w_i[idx]
    
    dens <- get_density2d_spatstat(coords_sub,
                                   weights = weights_sub,
                                   adjust  = adjust,
                                   ...)
    dens$v[dens$v < 0] <- 0
    df <- reshape2::melt(dens$v)
    
    ## ---- 3b. Map grid back to original coordinate space ------------- ##
    if (scale == "sd") {
      df$x <- dens$xcol[df$Var2] * sd_x + mu
      df$y <- dens$yrow[df$Var1] * sd_y + mu
    } else if (scale == "log") {
      df$x <- exp(dens$xcol[df$Var2]) - 1
      df$y <- exp(dens$yrow[df$Var1]) - 1
    } else {                     # "none"
      df$x <- dens$xcol[df$Var2] + mu   # mu is 0 unless centre-only
      df$y <- dens$yrow[df$Var1] + mu
    }
    
    df$value <- min_max_normalization(df$value, 0, 1)
    df
  })
  
  ## ------------------------------------------------------------------ ##
  ## 4. Base layers ---------------------------------------------------- ##
  ## ------------------------------------------------------------------ ##
  p_base <- ggObj
  if (show_points) {
    p_base <- p_base +
      ggplot2::geom_point(data = coords,
                          ggplot2::aes(x = x, y = y),
                          size = point_size,
                          color = point_color)
  }
  
  p_base <- p_base +
    ggplot2::labs(x = x_lab, y = y_lab) +
    gg_theme()
  
  ## ------------------------------------------------------------------ ##
  ## 5. Overlay phenotype contours ------------------------------------ ##
  ## ------------------------------------------------------------------ ##
  p_final <- Reduce(function(p, idx) {
    df_i   <- dens_list[[idx]]
    base_c <- colorspace::darken(colors[idx], darken_value,
                                 space = "combined",
                                 method = "relative")
    p +
      ggplot2::geom_contour_filled(
        data   = df_i,
        ggplot2::aes(x = x, y = y, z = value, alpha = ..level..),
        fill   = base_c,
        breaks = contour_breaks
      ) +
      ggplot2::geom_contour(
        data   = df_i,
        ggplot2::aes(x = x, y = y, z = value),
        color  = countour_color,
        alpha  = alpha_line,
        size   = size,
        breaks = contour_breaks
      )
  },
  x    = seq_len(n_pheno),
  init = p_base)
  
  ## Hide alpha legend, keep its range
  p_final <- p_final + ggplot2::scale_alpha_discrete(range = alpha_range, guide = "none")
  
  ## ------------------------------------------------------------------ ##
  ## 6. Phenotype legend (only when user supplied `weights`) ----------- ##
  ## ------------------------------------------------------------------ ##
  if (legend_required) {
    pal <- stats::setNames(colors[seq_len(n_pheno)], pheno_names)
    
    # Dummy, invisible points to drive discrete fill legend with squares
    legend_df <- data.frame(
      phenotype = factor(pheno_names, levels = pheno_names),
      x = min(coords$x, na.rm = TRUE),
      y = min(coords$y, na.rm = TRUE)
    )
    
    p_final <- p_final +
      ggplot2::geom_point(
        data = legend_df,
        ggplot2::aes(x = x, y = y, fill = phenotype),
        shape = 22, size = 4, alpha = 0,
        inherit.aes = FALSE, show.legend = TRUE
      ) +
      ggplot2::scale_fill_manual(
        name   = "Phenotype",
        values = pal,
        drop   = FALSE,
        guide  = ggplot2::guide_legend(
          override.aes = list(shape = 22, size = 5, alpha = 1, color = NA)
        )
      )
  } else {
    p_final <- p_final + ggplot2::guides(fill = "none")
  }
  
  p_final
}



#' Plot multiple density contour panels (one per weight column)
#'
#' @description
#' Wrapper around `ggdensity_contours()` that creates a separate plot for each
#' column in `weights` and arranges them in a grid.
#'
#' @param coords A data frame or matrix with at least two numeric columns (x/y).
#' @param weights A data frame/matrix (or vector) of weights; each column is
#'   treated as a separate phenotype to plot in its own panel.
#' @param ggObj A base `ggplot2::ggplot()` object to build on.
#' @param titles Optional character vector for panel titles; defaults to
#'   `colnames(weights)`. Recycled if necessary.
#' @param plots_per_row Integer; number of panels per row. Defaults to
#'   `ceiling(sqrt(ncol(weights)))`.
#' @param colors Character vector of base colors for panels. If fewer than the
#'   number of panels, it is extended with `interpolate_colors(colors, n)`.
#'   Each panel gets its own single color from this vector.
#' @param legend.position Legend position for each panel (passed via theme).
#' @param legend.key.size Legend key size (cm).
#' @param legend.text.size Legend text size.
#' @param legend.text.angle Legend text angle.
#' @param ... Additional arguments passed to `ggdensity_contours()`
#'   (e.g., `scale`, `center`, `show_points`, `adjust`, `contour_breaks`, etc.).
#'
#' @return A grob returned by `gridExtra::grid.arrange` containing all panels.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' n <- 400
#' coords <- data.frame(PC1 = rnorm(n), PC2 = rnorm(n))
#' weights <- data.frame(A = runif(n), B = runif(n), C = rexp(n))
#' ggdensity_contours_multi(coords, weights, plots_per_row = 2)
#' }
ggdensity_contours_multi <- function(coords,
                                     weights,
                                     ggObj = ggplot2::ggplot(),
                                     titles = NULL,
                                     plots_per_row = NULL,
                                     colors         =  c('#5F4690','#1D6996','#0F8554','#EDAD08','#CC503E','#94346E','#1C1F33','#93A8AC','#A16928','#065256'),
                                     legend.position = "bottom",
                                     legend.key.size = 0.4,
                                     legend.text.size = 7,
                                     legend.text.angle = 0,
                                     ...) {
  
  # Coerce weights to data.frame with columns
  if (!is.data.frame(weights)) {
    if (is.matrix(weights)) {
      weights <- as.data.frame(weights)
    } else {
      weights <- cbind.data.frame(values = weights)
    }
  }
  n <- ncol(weights)
  
  # Layout
  if (is.null(plots_per_row)) {
    plots_per_row <- ceiling(sqrt(n))
  }
  
  # Panel titles
  if (is.null(titles)) {
    titles <- colnames(weights)
    if (is.null(titles)) titles <- paste0("phenotype_", seq_len(n))
  }
  if (length(titles) != n) {
    warning("Length of `titles` (", length(titles),
            ") does not match number of columns in `weights` (", n, "). Titles will be recycled.")
    titles <- rep_len(titles, n)
  }
  
  # Ensure enough panel colors: extend using interpolate_colors() if needed
  if (length(colors) < n) {
    warning("`colors` length (", length(colors),
            ") is less than number of panels (", n, "); extending with interpolate_colors().")
    colors <- interpolate_colors(colors, n)
  } else if (length(colors) > n) {
    colors <- colors[seq_len(n)]
  }
  
  plot_list <- vector("list", length = n)
  
  # Build each panel by calling ggdensity_contours() with a single-column weight
  for (i in seq_len(n)) {
    w_i <- weights[, i, drop = FALSE]  # keep column name
    col_i <- colors[i]
    
    p_i <- ggdensity_contours(
      coords = coords,
      weights = w_i,
      ggObj = ggObj,
      colors = col_i,      # single color for this panel
      ...
    ) +
      ggplot2::ggtitle(titles[i]) +
      ggplot2::theme(
        legend.position = legend.position,
        legend.title = ggplot2::element_text(size = legend.text.size),
        legend.text  = ggplot2::element_text(size = legend.text.size,
                                             angle = legend.text.angle),
        legend.key.size = grid::unit(legend.key.size, "cm")
      )
    
    plot_list[[i]] <- p_i +coord_fixed()
  }
  
  arranged_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = plots_per_row))
  return(arranged_plots)
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
ggshape_heatmap=function(
    data,
    data_sizes  = NULL,
    shape_values = 22,
    size_range   = c(2, 8),
    stroke=0.2,
    colorscheme = rev(c('#9C0824','#BF1316','#D42922','#E96251','#EBA49A','#f0f0f0','#B0B0B0','#838383','#5D5D5D', '#3B3B3B' ,'#1E1E1E')), 
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
    symmQuant=NULL,
    grid.pars = list(grid.size = 0.15,
                     grid.color = "#dddddd",
                     grid.linetype = "solid")
) {
  
  if (!is.null(symmQuant)) {
    if (!is.numeric(symmQuant) || symmQuant < 0 || symmQuant > 1) {
      stop("symmQuant must be numeric in [0, 1]")
    }
    
    qnts1 <- abs(stats::quantile(data, symmQuant, na.rm = TRUE))
    qnts2 <- abs(stats::quantile(data, 1 - symmQuant, na.rm = TRUE))
    maxQ  <- max(qnts1, qnts2)
    data  <- MinMax(data, min = -maxQ, max = maxQ)
  }
  
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  if (is.null(data_sizes)) data_sizes <- data
  if (!is.matrix(data))       data       <- as.matrix(data)
  if (!is.matrix(data_sizes)) data_sizes <- as.matrix(data_sizes)
  
  stopifnot(identical(dim(data), dim(data_sizes)))
  
  
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
                        shape = shape_values,stroke=stroke) +
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
    axis.text.x      = ggplot2::element_text(angle = text.angle.x, hjust = 1,vjust=0),
    axis.text.y      = ggplot2::element_text(angle = text.angle.y, hjust = 1),
    legend.key.size  = grid::unit(legend.key.size, "cm"),
    legend.title     = ggplot2::element_text(size = legend.text.size)
  )
}

theme_umap=function() {
  theme_light() %+replace% 
    theme( panel.grid.major = element_blank(),
           plot.title = element_text(size = 16, hjust = 0, 
                                     vjust = 1 ),
           axis.ticks = element_blank(),
           legend.position = "none",
           panel.grid.minor = element_blank(),
           axis.text = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank())
} 
theme_umap_legend=function() {
  theme_umap() %+replace% 
    theme( 
      legend.position = "right")
  
} 

                
ggdensity_contours_diverge = function(coords,
                                      weights,
                                      ggObj = ggplot2::ggplot(),
                                      titles = NULL,
                                      plots_per_row = NULL,
                                      colors = c("#1f77b4", "#d62728"),  
                                      legend.labels = c("Positive", "Negative"),
                                      legend.position = "bottom",
                                      legend.key.size = 0.4,
                                      legend.text.size = 7,
                                      legend.text.angle = 0,
                                      ...) {
  
  # ---- Coerce weights to data.frame with columns (mirror ggdensity_contours_multi) ----
  if (!is.data.frame(weights)) {
    if (is.matrix(weights)) {
      weights <- as.data.frame(weights)
    } else {
      weights <- cbind.data.frame(values = weights)
    }
  }
  n <- ncol(weights)
  
  # ---- Layout ----
  if (is.null(plots_per_row)) {
    plots_per_row <- ceiling(sqrt(n))
  }
  
  # ---- Panel titles ----
  if (is.null(titles)) {
    titles <- colnames(weights)
    if (is.null(titles)) titles <- paste0("phenotype_", seq_len(n))
  }
  if (length(titles) != n) {
    warning("Length of `titles` (", length(titles),
            ") does not match number of columns in `weights` (", n, "). Titles will be recycled.")
    titles <- rep_len(titles, n)
  }
  
  # ---- Validate colors and labels ----
  if (length(colors) != 2L) {
    stop("`colors` must be length 2: first for +w_i, second for -w_i.")
  }
  if (length(legend.labels) != 2L) {
    stop("`legend.labels` must be length 2, e.g., c('Positive','Negative').")
  }
  
  plot_list <- vector("list", length = n)
  
  # ---- Build each panel by calling ggdensity_contours() with two densities: +w_i and -w_i ----
  for (i in seq_len(n)) {
    w_i <- weights[, i, drop = FALSE]  # keep as one-column data.frame
    
    # Two-column weights with consistent, customizable legend labels across panels
    w_div <- cbind.data.frame(
      setNames(w_i, legend.labels[1]),   # +w_i
      setNames(-w_i, legend.labels[2])   # -w_i
    )
    
    p_i <- ggdensity_contours(
      coords  = coords,
      weights = w_div,        # pass both +w_i and -w_i
      ggObj   = ggObj,
      colors  = colors,       # exactly two colors reused for every panel
      ...
    ) +
      ggplot2::ggtitle(titles[i]) +
      ggplot2::theme(
        legend.position = legend.position,
        legend.title    = ggplot2::element_text(size = legend.text.size),
        legend.text     = ggplot2::element_text(size = legend.text.size,
                                                angle = legend.text.angle),
        legend.key.size = grid::unit(legend.key.size, "cm")
      )
    
    plot_list[[i]] <- p_i + ggplot2::coord_fixed()
  }
  
  arranged_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = plots_per_row))
  return(arranged_plots)
}
