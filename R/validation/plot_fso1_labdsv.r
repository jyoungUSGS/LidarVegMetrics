plot.fso1 <- function (x, which = "all", xlab = x$var, ylab = "mu(x)", title = "", 
    r = TRUE, pch = 1, col.r = "red", cex.r=1, cex.pts = 1, col.pts = "blue",pch.pts,...) 
{
    if (class(x) != "fso") {
        stop("You must specify n object of class fso from fso()")
    }
    if (is.matrix(x$mu)) {
        if (which == "all") {
            for (i in 1:ncol(x$mu)) {
                if (is.na(x$r[i])) {
                  cat(paste("variable ", x$var[i], " has missing values \n"))
                }
                else {
                  plot(x$data[, i], x$mu[, i], xlab = xlab[i], cex = cex.pts, col = col.pts,pch = pch.pts,
                    ylab = ylab, main = title)
                  if (r) {
                    ax <- min(x$data[, i])
                    ay <- max(x$mu[, i])
                    text(ax, ay, paste("r = ", format(x$r[i], col = col.r, cex = cex.r,
                      digits = 3)), pos = 4)
                  }
                }
                if (i != ncol(x$mu)) 
                  readline("\nHit Return for Next Plot\n")
            }
        }
        else if (is.numeric(which)) {
            for (i in which) {
                plot(x$data[, i], x$mu[, i], xlab = xlab[i], cex = cex.pts, col = col.pts,pch = pch.pts,
                  ylab = ylab, main = title)
                if (r) {
                  ax <- min(x$data[, i])
                  ay <- max(x$mu[, i])
                  text(ax, ay, paste("r = ", format(x$r[i], digits = 3)), col = col.r, cex = cex.r,
                    pos = 4)
                }
                readline("\nHit Return for Next Plot\n")
            }
        }
        else {
            for (j in 1:ncol(x$mu)) {
                if (which == x$var[j]) 
                  plot(x$data[, j], x$mu[, j], xlab = xlab[i], cex = cex.pts, col = col.pts,pch = pch.pts,
                    ylab = ylab, main = title)
                if (r) {
                  ax <- min(x$data[, j])
                  ay <- max(x$mu[, j])
                  text(ax, ay, paste("r = ", format(x$r[j], digits = 3)), col = col.r, cex = cex.r,
                    pos = 4)
                }
            }
        }
    }
    else {
        plot(x$data, x$mu, xlab = xlab, ylab = ylab, main = title, cex = cex.pts, col = col.pts, pch = pch.pts)
        if (r) {
            ax <- min(x$data)
            ay <- max(x$mu)
            text(ax, ay, paste("r = ", format(x$r, digits = 3)), 
                pos = 4, col = col.r, cex = cex.r)
        }
    }
}
