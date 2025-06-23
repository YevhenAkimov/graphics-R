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
    if (is.null(colnames(values)[3])) {
      nameZ="z"
    } else {
      nameZ=colnames(values)[3]
    } 
    values = values[,1] 
    
    } else  { stop("values must be a vector, factor, matrix or data.frame") }
    
    
  
  ### extract colnames from coords
  if (is.null(colnames(coords))) {
    if (is.null(dimnamesXYZ)) {
      dimnamesXY <- c("x", "y")
    } 
  } else if (is.null(dimnamesXYZ)) {
    dimnamesXY <- colnames(coords)[1:2]
  } 
  if (is.null(dimnamesXYZ)) {
    dimnamesXYZ <- c(dimnamesXY, nameZ)
  } else if (length(dimnamesXYZ) < 3) {
    warning("dimnamesXYZ should have at least 3 elements, making names")
    dimnamesXYZ <- c("x", "y", nameZ)
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
  colnames(to_plot) <- dimnamesXYZ[seq_along(colnames(to_plot))]
  
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
