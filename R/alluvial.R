#' Alluvial diagram
#' 
#' Drawing alluvial diagrams, also known as parallel set plots.
#' 
#' Alluvial diagrams have two principal uses: to track changing (relative) sizes
#' of data clusters over time, and to compare frequency data along several 
#' categorical variables (also use \code{mosaicplot} for two categorical 
#' variables). This package is still under development!
#' 
#' @param ... vectors or data frames to be combined into a single data frame 
#'   (must have compatible numbers of rows).
#' @param freq numeric vector of frequencies, having the same length as the 
#'   combined data frame.
#' @param col character vector of ribbon colors.
#' @param alpha numeric vector of ribbon transparencies; ignored if \code{NULL}.
#' @param border character vector of ribbon border colors.
#' @param hide logical vector indicating which ribbons to hide.
#' @param blocks logical vector, having length compatible with the number of 
#'   axes (factor variables), indicating which axes to represent as blocks 
#'   (rather than stripes); or a character string indicating a predefined 
#'   subset; options include "bookends".
#' @param layer numeric vector giving the order in which to draw the ribbons.
#' @param ranking character string indicating a predefined axis ranking
#'   function; options include "rightward", "leftward", and "zzout" (default).
#' @param gap.width numeric, relative widths of inter-category gaps.
#' @param xw numeric, the distance from the set axis to the control points of 
#'   each xspline.
#' @param cw numeric vector of axis widths.
#'   
#' @return Nothing.
#'   
#' @export
#' 
#' @example examples/alluvial.R

alluvial <- function(..., freq,
                     col = "gray", alpha = .5, border = 0,
                     hide = FALSE, blocks = TRUE,
                     layer, ranking = "zzout", ordering = NULL,
                     gap.width = .05, xw = .1, cw = .1) {
    
    # Dataset (combined alone, so that other inputs don't lengthen it)
    p <- data.frame(..., stringsAsFactors = FALSE)
    # Convert any character fields to factors
    is.ch <- sapply(p, is.character)
    p[is.ch] <- lapply(p[is.ch], as.factor)
    # If freq is missing, guess a frequency column; if none, all 1s
    if (missing(freq)) {
        # If a column of the exact name "freq" exists, use it
        gr <- grep("freq|count", tolower(names(p)))
        if (length(gr) == 1) {
            names(p)[gr] <- "freq"
        } else if (length(gr) > 1) {
            if ("freq" %in% names(p)) {
                warning("More than one guess for frequency; using 'freq'.")
            } else {
                warning("More than one guess for frequency; choosing first.")
                names(p)[gr[1]] <- "freq"
            }
        }
    } else {
        if ("freq" %in% names(p)) {
            warning("Replacing 'freq' field with input 'freq'.")
        }
        p$freq = freq
    }
    p$freq <- with(p, freq / sum(freq))
    wp <- which(names(p) != "freq")
    np <- length(p) - 1
    
    # Convert blocks to vector
    if (length(blocks) == 1) {
        blocks <- if (!is.na(as.logical(blocks))) {
            rep(blocks, np)
        } else if (blocks == "bookends") {
            c(TRUE, rep(FALSE, np - 2), TRUE)
        }
    }
    
    # Select ranking function
    rank.fun <- list(
        rightward = function(n, i) c(i, (1:n)[-i]),
        leftward = function(n, i) c(i, (n:1)[-(n+1-i)]),
        zzout = zzout
    )[[ranking]]
    # Check that 'ordering' is of proper form
    if (!is.null(ordering)) {
        if (is.matrix(ordering)) {
            ordering <- lapply(seq_len(ncol(ordering)), function(i) x[, i])
        }
        stopifnot(is.list(ordering))
        if (length(ordering) != np) {
            stop("Argument 'ordering' should have ",
                 np, " components, has ", length(ordering), ".")
        }
    }

    # List of data frames of vertical positions of stripe corners
    dd <- lapply(wp, function(i) {
        flowpoints(i = i, dat = p[, wp], freq = p$freq, wid = gap.width,
                   ord = ordering[[i]])
    })

    # Convert colors to hexcodes
    hex <- col2rgb(col, alpha = TRUE)
    if (!is.null(alpha)) hex["alpha", ] <- alpha * 255
    # Append graphical parameters to dataset
    p$col <- apply(hex, 2, function(x) {
        do.call(rgb, c(as.list(x), maxColorValue = 255))
    })
    p$border <- border
    p$hide = hide
    # Default layers, if not provided
    if (missing(layer)) layer <- 1:nrow(p)
    p$layer <- layer
    
    # Initiate plot
    op <- par(mar = c(2, 1, 1, 1))
    plot(NULL, type = "n",
         xlim = c(1-cw, np+cw), ylim = c(0, 1),
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", xlab = '', ylab = '',
         frame = FALSE)
    
    # Plot stripes (in order of layer)
    ind <- which(!p$hide)[rev(order(p[!p$hide, ]$layer))]
    for (i in ind) {
        # For every inter-axis interval
        for (j in 1:(np - 1)) {
            # Draw stripe
            xspline(c(j, j, j+xw, j+1-xw, j+1, j+1, j+1-xw, j+xw, j) +
                        rep(c(cw, -cw, cw), c(3, 4, 2)),
                    c(dd[[j]][i, c(1, 2, 2)],
                      rev(dd[[j+1]][i, c(1, 1, 2, 2)]),
                      dd[[j]][i, c(1, 1)]),
                    shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
                    open = FALSE,
                    col = p$col[i], border = p$border[i])
        }
    }
    
    # Category blocks with labels
    for (j in 1:np) {
        ax <- lapply(split(dd[[j]], p[,j]), range)
        if (blocks[j]) {
            for(k in seq_along(ax)) {
                rect(j-cw, ax[[k]][1], j+cw, ax[[k]][2])
            }
        } else {
            for (i in ind) {
                x <- j + c(-1, 1) * cw
                y <- t(dd[[j]][c(i, i), ])
                w <- xw * (x[2] - x[1])
                xspline(x = c(x[1], x[1], x[1] + w, x[2] - w,
                              x[2], x[2], x[2] - w, x[1] + w, x[1]),
                        y = c(y[c(1, 2, 2), 1],
                              y[c(2, 2, 1, 1), 2],
                              y[c(1, 1), 1]),
                        shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
                        open = FALSE, col = p$col[i], border = p$border[i])
            }
        }
        for (k in seq_along(ax)) {
            text(j, mean(ax[[k]]), labels = names(ax)[k])
        }
    }           
    
    # Axis labels
    axis(1, at = rep(c(-cw, cw), np) + rep(1:np, each = 2),
         line = 0.5, col = "white", col.ticks = "black", labels = FALSE)
    axis(1, at = 1:np, tick = FALSE, labels = names(p)[1:np])
    
    par(op)
}
