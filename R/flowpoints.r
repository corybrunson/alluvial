#' Compute endpoints of stripes (polygons or splines)
#' 
#' @param i numeric, an index between 1 and \code{ncol(dat)}.
#' @param dat data frame.
#' @param freq frequency vector.
#' @param wid numeric, gap between categories.
#' @param ord numeric vector giving the order in which to draw the stripes.

flowpoints <- function(i, dat, freq, wid, ord) {
    # Ordering dimension ids for lexicographic sorting
    a <- c(i, (1:ncol(dat))[-i])
    # Order of rows of d starting from i-th dimension
    if(is.null(ord)) {
        o <- do.call(order, dat[a])
    } else {
        d2 <- dat
        d2[1] <- ord
        o <- do.call(order, d2[a])
    }
    # Breakpoints on a dimension
    x <- c(0, cumsum(freq[o])) * (1-wid)
    # Stripe coordinates on a dimension
    x <- cbind(x[-length(x)], x[-1])
    # By how much stripes need to be shifted upwards (gap/max(gap))
    gap <- cumsum( c(0L, diff(as.numeric(dat[o, i])) != 0) )
    mx <- max(gap)
    if (mx == 0) mx <- 1
    # shifts
    gap <- gap / mx * wid
    # add gap-related shifts to stripe coordinates on dimension i
    (x + gap)[order(o), ]
}
