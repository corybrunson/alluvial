# Based on riverplot::curveseg

 
stripe <- function( x0, x1, y0, y1, width= 1, nsteps= 50, 
                    col= "#ffcc0066", grad= NULL, lty= 1, 
                    form= c( "sin", "line" ), alpha=1, ... ) {
  form <- match.arg( form, c( "sin", "line" ) )
  
  if( form == "sin" ) {
    xx  <- seq( -pi/2, pi/2, length.out= nsteps )
    yy <- y0 + ( y1 - y0 ) * ( sin( xx ) + 1 ) / 2
    xx <- seq( x0, x1, length.out= nsteps )
  }
  
  if( form == "line" ) {
    xx <- seq( x0, x1, length.out= nsteps )
    yy <- seq( y0, y1, length.out= nsteps )
  }
  
  if( is.null(grad) ) {
    stripe_simple(x=xx, y=yy, width=width, col=col, border=col, lty=lty, alpha=alpha, ...)
  } else {
    stripe_gradient(x=xx, y=yy, width=width, grad=grad, lty=lty, alpha=alpha, ...)
  }
}


stripe_simple <- function(x, y, width, col, border, lty, alpha, ... ) {
  w <- width
  polygon( x= c(x, rev(x)),
           y=c(y, rev(y) + w),
           col=col,
           border=border)
  lines(x, y, lty=lty)
  lines(x, y+w, lty=lty)
}


stripe_gradient <- function(x, y, width, grad, border, lty, alpha, ...) {
  w <- width
  nsteps <- length(x)
  grad <- riverplot::colorRampPaletteAlpha( grad )( nsteps )
  grad <- adjustcolor(grad, alpha.f=alpha)
  for( i in 1:(nsteps-1) ) {
    polygon( c( x[i], x[i+1], x[i+1], x[i] ),
             c( y[i], y[i+1], y[i+1] + w, y[i] + w ), col= grad[i], border=NA, ... )
    lines( c( x[i], x[i+1] ), c( y[i], y[i+1] ), lty= lty )
    lines( c( x[i], x[i+1] ), c( y[i] + w, y[i+1] + w ), lty= lty )
  }
}
