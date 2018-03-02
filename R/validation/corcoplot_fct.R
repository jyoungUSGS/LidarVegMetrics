require(mvoutlier)
require(robustbase)

## functions: cor.plot, cor.plotlim, coplot.cex, panel.corplot, 
## classcor2pop.plot, robcor2pop.plot
 
cor.plot <-
function(x, y, quan=1/2, alpha=0.025, tcol1=4, tcol2=2, ecol1=4, ecol2=2, elty1=3, elty2=2, elwd1=1, 
elwd2=1, xlim1=TRUE, ylim1=TRUE,title.r = TRUE, ...) {

  #library(rrcov)  
  x <- as.matrix(cbind(x,y))
  x <- na.omit(x)

  covr <- covMcd(x, cor=TRUE, alpha=quan)
  cov.svd <- svd(cov(x), nv = 0)
  covr.svd <- svd(covr$cov, nv = 0)
  r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
  rr <- covr.svd[["u"]] %*% diag(sqrt(covr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
  if (xlim1 == TRUE) xlim1 <- range(x[,1]) else xlim1 <- c(min(c(x[, 1], tt[, 1],ttr[,1])), max(c(x[, 1], tt[, 1],ttr[,1])))
  #if (ylim1 == TRUE) ylim1 <- range(x[,2]) else ylim1 <- c(min(c(x[, 2], tt[, 2],ttr[,2])), max(c(x[, 2], tt[, 2],ttr[,2])))
  if (ylim1 == TRUE) ylim1 <- c(0,100) else ylim1 <- c(min(c(x[, 2], tt[, 2],ttr[,2])), max(c(x[, 2], tt[, 2],ttr[,2])))

  plot(x, xlim=xlim1, ylim=ylim1, ...)
  if (title.r == TRUE){
  title(main=list(paste("Class.cor =",round(cor(x)[1,2],2),"                            "), col=tcol1), cex.main=1.5, line = 1)
  title(main=list(paste("                                   Rob.cor =", round(covr$cor[1,2],2)),col=tcol2), cex.main=1.5, line=1)
}
  lines(tt[, 1], tt[, 2], type = "l",col=ecol1,lty=elty1, lwd=elwd1)
	
  lines(ttr[,1], ttr[,2], type = "l",col=ecol2, lty=elty2, lwd=elwd2)
	
  ret <- list(cor.cla = cor(x)[1,2], cor.rob=covr$cor[1,2], xlim = range(xlim1), ylim = range(ylim1))
  ret
}

cor.plotlim <-
function(x, y, quan=1/2, alpha=0.025, tcol1=4, tcol2=2, ecol1=4, ecol2=2, elty1=3, elty2=2, elwd1=1, elwd2=1, ymax = 8,...) {

  #library(rrcov)  
  x <- as.matrix(cbind(x,y))

  covr <- covMcd(x, cor=TRUE, alpha=quan)
  cov.svd <- svd(cov(x), nv = 0)
  covr.svd <- svd(covr$cov, nv = 0)
  r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
  rr <- covr.svd[["u"]] %*% diag(sqrt(covr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
  
plot(x, xlim=c(min(c(x[, 1], tt[, 1],ttr[,1])), max(c(x[, 1], tt[, 1],ttr[,1]))), ylim=c(min(c(x[, 2], tt[, 2],ttr[,2])), ymax), ...)
  title(main=list(paste("Class.cor =",round(cor(x)[1,2],2),"                                      "), col=tcol1), cex.main=1.5, line = 1)
  title(main=list(paste("                                      Rob.cor =", round(covr$cor[1,2],2)),col=tcol2), cex.main=1.5, line=1)

  lines(tt[, 1], tt[, 2], type = "l",col=ecol1,lty=elty1, lwd=elwd1)
	
  lines(ttr[,1], ttr[,2], type = "l",col=ecol2, lty=elty2, lwd=elwd2)
	
  ret <- list(cor.cla = cor(x)[1,2], cor.rob=covr$cor[1,2])
  ret
}


coplot.cex <- 
function (formula, data, given.values, panel = points, rows, 
    columns, show.given = TRUE, col = par("fg"), pch = par("pch"), 
    cola.fac = cola.fac, cola.num = cola.num, colb.fac = colb.fac, 
colb.num = colb.num, xlab = c(x.name, 
        paste("Given :", a.name)), ylab = c(y.name, paste("Given :", 
        b.name)), subscripts = FALSE, axlabels = function(f) abbreviate(levels(f)), 
    number = 6, overlap = 0.5, xlim, ylim, cex1=cex1, cex2=cex2,...) 
{
    deparen <- function(expr) {
        while (is.language(expr) && !is.name(expr) && deparse(expr[[1]])[1] == 
            "(") expr <- expr[[2]]
        expr
    }
    bad.formula <- function() stop("invalid conditioning formula")
    bad.lengths <- function() stop("incompatible variable lengths")
    formula <- deparen(formula)
    if (!inherits(formula, "formula")) 
        bad.formula()
    y <- deparen(formula[[2]])
    rhs <- deparen(formula[[3]])
    if (deparse(rhs[[1]])[1] != "|") 
        bad.formula()
    x <- deparen(rhs[[2]])
    rhs <- deparen(rhs[[3]])
    if (is.language(rhs) && !is.name(rhs) && (deparse(rhs[[1]])[1] == 
        "*" || deparse(rhs[[1]])[1] == "+")) {
        have.b <- TRUE
        a <- deparen(rhs[[2]])
        b <- deparen(rhs[[3]])
    }
    else {
        have.b <- FALSE
        a <- rhs
    }
    if (missing(data)) 
        data <- parent.frame()
    x.name <- deparse(x)
    x <- eval(x, data, parent.frame())
    nobs <- length(x)
    y.name <- deparse(y)
    y <- eval(y, data, parent.frame())
    if (length(y) != nobs) 
        bad.lengths()
    a.name <- deparse(a)
    a <- eval(a, data, parent.frame())
    if (length(a) != nobs) 
        bad.lengths()
    if (is.character(a)) 
        a <- as.factor(a)
    a.is.fac <- is.factor(a)
    if (have.b) {
        b.name <- deparse(b)
        b <- eval(b, data, parent.frame())
        if (length(b) != nobs) 
            bad.lengths()
        if (is.character(b)) 
            b <- as.factor(b)
        b.is.fac <- is.factor(b)
        missingrows <- which(is.na(x) | is.na(y) | is.na(a) | 
            is.na(b))
    }
    else {
        missingrows <- which(is.na(x) | is.na(y) | is.na(a))
        b <- NULL
        b.name <- ""
    }
    number <- as.integer(number)
    if (length(number) == 0 || any(number < 1)) 
        stop("'number' must be integer >= 1")
    if (any(overlap >= 1)) 
        stop("'overlap' must be < 1 (and typically >= 0).")
    bad.givens <- function() stop("invalid 'given.values'")
    if (missing(given.values)) {
        a.intervals <- if (a.is.fac) {
            i <- seq_along(a.levels <- levels(a))
            a <- as.numeric(a)
            cbind(i - 0.5, i + 0.5)
        }
        else co.intervals(unclass(a), number = number[1], overlap = overlap[1])
        b.intervals <- if (have.b) {
            if (b.is.fac) {
                i <- seq_along(b.levels <- levels(b))
                b <- as.numeric(b)
                cbind(i - 0.5, i + 0.5)
            }
            else {
                if (length(number) == 1) 
                  number <- rep.int(number, 2)
                if (length(overlap) == 1) 
                  overlap <- rep.int(overlap, 2)
                co.intervals(unclass(b), number = number[2], 
                  overlap = overlap[2])
            }
        }
    }
    else {
        if (!is.list(given.values)) 
            given.values <- list(given.values)
        if (length(given.values) != (if (have.b) 
            2
        else 1)) 
            bad.givens()
        a.intervals <- given.values[[1]]
        if (a.is.fac) {
            a.levels <- levels(a)
            if (is.character(a.intervals)) 
                a.intervals <- match(a.intervals, a.levels)
            a.intervals <- cbind(a.intervals - 0.5, a.intervals + 
                0.5)
            a <- as.numeric(a)
        }
        else if (is.numeric(a)) {
            if (!is.numeric(a.intervals)) 
                bad.givens()
            if (!is.matrix(a.intervals) || ncol(a.intervals) != 
                2) 
                a.intervals <- cbind(a.intervals - 0.5, a.intervals + 
                  0.5)
        }
        if (have.b) {
            b.intervals <- given.values[[2]]
            if (b.is.fac) {
                b.levels <- levels(b)
                if (is.character(b.intervals)) 
                  b.intervals <- match(b.intervals, b.levels)
                b.intervals <- cbind(b.intervals - 0.5, b.intervals + 
                  0.5)
                b <- as.numeric(b)
            }
            else if (is.numeric(b)) {
                if (!is.numeric(b.intervals)) 
                  bad.givens()
                if (!is.matrix(b.intervals) || ncol(b.intervals) != 
                  2) 
                  b.intervals <- cbind(b.intervals - 0.5, b.intervals + 
                    0.5)
            }
        }
    }
    if (any(is.na(a.intervals)) || (have.b && any(is.na(b.intervals)))) 
        bad.givens()
    if (have.b) {
        rows <- nrow(b.intervals)
        columns <- nrow(a.intervals)
        nplots <- rows * columns
        if (length(show.given) < 2) 
            show.given <- rep.int(show.given, 2)
    }
    else {
        nplots <- nrow(a.intervals)
        if (missing(rows)) {
            if (missing(columns)) {
                rows <- ceiling(round(sqrt(nplots)))
                columns <- ceiling(nplots/rows)
            }
            else rows <- ceiling(nplots/columns)
        }
        else if (missing(columns)) 
            columns <- ceiling(nplots/rows)
        if (rows * columns < nplots) 
            stop("rows * columns too small")
    }
    total.columns <- columns
    total.rows <- rows
    f.col <- f.row <- 1
    if (show.given[1]) {
        total.rows <- rows + 1
        f.row <- rows/total.rows
    }
    if (have.b && show.given[2]) {
        total.columns <- columns + 1
        f.col <- columns/total.columns
    }
    mar <- if (have.b) 
        rep.int(0, 4)
    else c(0.5, 0, 0.5, 0)
    oma <- c(5, 6, 5, 4)
    if (have.b) {
        oma[2] <- 5
        if (!b.is.fac) 
            oma[4] <- 5
    }
    if (a.is.fac && show.given[1]) 
        oma[3] <- oma[3] - 1
    opar <- par(mfrow = c(total.rows, total.columns), oma = oma, 
        mar = mar, xaxs = "r", yaxs = "r")
    on.exit(par(opar))
    plot.new()
    if (missing(xlim)) 
        xlim <- range(as.numeric(x), finite = TRUE)
    if (missing(ylim)) 
        ylim <- range(as.numeric(y), finite = TRUE)
    pch <- rep(pch, length.out = nobs)
    col <- rep(col, length.out = nobs)
    do.panel <- function(index, subscripts = FALSE, id) {
        Paxis <- function(side, x) {
            if (nlevels(x)) {
                lab <- axlabels(x)
                axis(side, labels = lab, at = seq(lab), xpd = NA)
            }
            else Axis(x, side = side, xpd = NA)
        }
        istart <- (total.rows - rows) + 1
        i <- total.rows - ((index - 1)%/%columns)
        j <- (index - 1)%%columns + 1
        par(mfg = c(i, j, total.rows, total.columns))
        plot.new()
        plot.window(xlim, ylim)
        if (any(is.na(id))) 
            id[is.na(id)] <- FALSE
        if (any(id)) {
            grid(lty = "solid")
            if (subscripts) 
                panel(x[id], y[id], subscripts = id, col = col[id], 
                  pch = pch[id], ...)
            else panel(x[id], y[id], col = col[id], pch = pch[id], 
                ...)
        }
        if ((i == total.rows) && (j%%2 == 0)) 
            Paxis(1, x)
        else if ((i == istart || index + columns > nplots) && 
            (j%%2 == 1)) 
            Paxis(3, x)
        if ((j == 1) && ((total.rows - i)%%2 == 0)) 
            Paxis(2, y)
        else if ((j == columns || index == nplots) && ((total.rows - 
            i)%%2 == 1)) 
            Paxis(4, y)
        box()
    }
    if (have.b) {
        count <- 1
        for (i in 1:rows) {
            for (j in 1:columns) {
                id <- ((a.intervals[j, 1] <= a) & (a <= a.intervals[j, 
                  2]) & (b.intervals[i, 1] <= b) & (b <= b.intervals[i, 
                  2]))
                do.panel(count, subscripts, id)
                count <- count + 1
            }
        }
    }
    else {
        for (i in 1:nplots) {
            id <- ((a.intervals[i, 1] <= a) & (a <= a.intervals[i, 
                2]))
            do.panel(i, subscripts, id)
        }
    }
    mtext(xlab[1], side = 1, at = 0.5 * f.col, outer = TRUE, 
        line = 3.5, xpd = NA, font = par("font.lab"), cex = cex1)
    mtext(ylab[1], side = 2, at = 0.5 * f.row, outer = TRUE, 
        line = 3.5, xpd = NA, font = par("font.lab"), cex = cex1)
    if (length(xlab) == 1) 
        xlab <- c(xlab, paste("Given :", a.name))
    if (show.given[1]) {
        par(fig = c(0, f.col, f.row, 1), mar = mar + c(3 + (!a.is.fac), 
            0, 0, 0), new = TRUE)
        plot.new()
        nint <- nrow(a.intervals)
        a.range <- range(a.intervals, finite = TRUE)
        plot.window(a.range + c(0.03, -0.03) * diff(a.range), 
            0.5 + c(0, nint))

if (a.is.fac) {

        rect(a.intervals[, 1], 1:nint - 0.3, a.intervals[, 2], 
            1:nint + 0.3, col = cola.fac)
}
            else {
rect(a.intervals[, 1], 1:nint - 0.3, a.intervals[, 2], 
            1:nint + 0.3, col = cola.num)
}
        if (a.is.fac) {
            text(apply(a.intervals, 1, mean), 1:nint, a.levels, cex = cex2)
        }
        else {
            Axis(a, side = 3, xpd = NA)
            axis(1, labels = FALSE)
        }
        box()
        mtext(xlab[2], 3, line = 3 - a.is.fac, at = mean(par("usr")[1:2]), 
            xpd = NA, font = par("font.lab"), cex = cex1)
    }
    else {
        mtext(xlab[2], 3, line = 3.25, outer = TRUE, at = 0.5 * 
            f.col, xpd = NA, font = par("font.lab"), cex = cex1)
    }
    if (have.b) {
        if (length(ylab) == 1) 
            ylab <- c(ylab, paste("Given :", b.name))
        if (show.given[2]) {
            par(fig = c(f.col, 1, 0, f.row), mar = mar + c(0, 
                3 + (!b.is.fac), 0, 0), new = TRUE)
            plot.new()
            nint <- nrow(b.intervals)
            b.range <- range(b.intervals, finite = TRUE)
            plot.window(0.5 + c(0, nint), b.range + c(0.03, -0.03) * 
                diff(b.range))

if (b.is.fac) {

        rect(1:nint - 0.3, b.intervals[, 1], 1:nint + 0.3, b.intervals[, 2], col = colb.fac) 
}
            else {
rect(1:nint - 0.3, b.intervals[, 1], 1:nint + 0.3, b.intervals[, 2], col = colb.num)
}

            if (b.is.fac) {
                text(1:nint, apply(b.intervals, 1, mean), b.levels, 
                  srt = 90, cex = cex2)
            }
            else {
                Axis(b, side = 4, xpd = NA)
                axis(2, labels = FALSE)
            }
            box()
            mtext(ylab[2], 4, line = 3 - b.is.fac, at = mean(par("usr")[3:4]), 
                xpd = NA, font = par("font.lab"), cex = cex1)
        }
        else {
            mtext(ylab[2], 4, line = 3.25, at = 0.5 * f.row, 
                outer = TRUE, xpd = NA, font = par("font.lab"), 
                cex = cex1)
        }
    }
    if (length(missingrows) > 0) {
        cat("\n", gettext("Missing rows"), ": ", missingrows, 
            "\n", sep = "")
        invisible(missingrows)
    }
}

panel.corplot <-
function(x, y, quan=1/2, alpha=0.025, tcol1=4, tcol2=2, ecol1=4, ecol2=2, elty1=2, elty2=1, elwd1=2, elwd2=2,...) {

  #library(rrcov)  
  x <- as.matrix(cbind(x,y))

  covr <- covMcd(x, cor=TRUE, alpha=quan)
  cov.svd <- svd(cov(x), nv = 0)
  covr.svd <- svd(covr$cov, nv = 0)
  r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
  rr <- covr.svd[["u"]] %*% diag(sqrt(covr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
  points(x, xlim=c(min(c(x[, 1], tt[, 1],ttr[,1])), max(c(x[, 1], tt[, 1],ttr[,1]))), ylim=c(min(c(x[, 2], tt[, 2],ttr[,2])), max(c(x[, 2], tt[, 2],ttr[,2]))), ...)
  title(main=list(paste("Class.cor =",round(cor(x)[1,2],2),"                            "), col=tcol1), cex.main=1.5, line = -1.5)
  title(main=list(paste("                                   Rob.cor =", round(covr$cor[1,2],2)),col=tcol2), cex.main=1.5, line=-1.5)

  lines(tt[, 1], tt[, 2], type = "l",col=ecol1,lty=elty1, lwd=elwd1)
	
  lines(ttr[,1], ttr[,2], type = "l",col=ecol2, lty=elty2, lwd=elwd2)
	
  ret <- list(cor.cla = cor(x)[1,2], cor.rob=covr$cor[1,2])
  ret
}

classcor2pop.plot <-
function(x1, y1,x2, y2, quan=1/2, alpha=0.025, tcol1=4, tcol2=2, ecol1=4, ecol2=2, elty1=3, elty2=2, elwd1=1, elwd2=1,...) {

  #library(rrcov)  
####deep
  x1 <- as.matrix(cbind(x1,y1))
  covr <- covMcd(x1, cor=TRUE, alpha=quan)
  dcov.svd <- svd(cov(x1), nv = 0)
  dcovr.svd <- svd(covr$cov, nv = 0)
  r <- dcov.svd[["u"]] %*% diag(sqrt(dcov.svd[["d"]]))
  rr <- dcovr.svd[["u"]] %*% diag(sqrt(dcovr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x1, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
lines(tt[, 1], tt[, 2], type = "l", col=ecol1, lty=elty1, lwd=elwd1)
  title(main=list(paste("Cor1 =", round(cor(x1)[1,2],2), "                                   "),col=tcol1), cex.main=1.5, line=1)

### shallow
  x2 <- as.matrix(cbind(x2,y2))
  covr <- covMcd(x2, cor=TRUE, alpha=quan)
  scov.svd <- svd(cov(x2), nv = 0)
  scovr.svd <- svd(covr$cov, nv = 0)
  r <- scov.svd[["u"]] %*% diag(sqrt(scov.svd[["d"]]))
  rr <- scovr.svd[["u"]] %*% diag(sqrt(scovr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x2, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
    lines(ttr[, 1], ttr[, 2], type = "l", col=ecol2, lty=elty2, lwd=elwd2)

title(main=list(paste("                                   Cor2 =", round(cor(x2)[1,2],2)),col=tcol2), cex.main=1.5, line=1)
}

robcor2pop.plot <-
function(x1, y1,x2, y2, quan=1/2, alpha=0.025, tcol1=4, tcol2=2, ecol1=4, ecol2=2, elty1=3, elty2=2, elwd1=1, elwd2=1,...) {

  #library(rrcov)  
####deep
  x1 <- as.matrix(cbind(x1,y1))
  covr <- covMcd(x1, cor=TRUE, alpha=quan)
  dcov.svd <- svd(cov(x1), nv = 0)
  dcovr.svd <- svd(covr$cov, nv = 0)
  r <- dcov.svd[["u"]] %*% diag(sqrt(dcov.svd[["d"]]))
  rr <- dcovr.svd[["u"]] %*% diag(sqrt(dcovr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x1, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
    lines(ttr[,1], ttr[,2], type = "l", col=ecol1,lty=elty1, lwd=elwd1)

  title(main=list(paste("Rob.C1 =", round(covr$cor[1,2],2), "                                   "),col=tcol1), cex.main=1.5, line=1)
### shallow
  x2 <- as.matrix(cbind(x2,y2))
  covr <- covMcd(x2, cor=TRUE, alpha=quan)
  scov.svd <- svd(cov(x2), nv = 0)
  scovr.svd <- svd(covr$cov, nv = 0)
  r <- scov.svd[["u"]] %*% diag(sqrt(scov.svd[["d"]]))
  rr <- scovr.svd[["u"]] %*% diag(sqrt(scovr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x2, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
    lines(ttr[,1], ttr[,2], type = "l", col=ecol2,lty=elty2, lwd=elwd2)

title(main=list(paste("                                   Rob.C2 =", round(covr$cor[1,2],2)),col=tcol2), cex.main=1.5, line=1)
}

panelwhite.corplot <-
function(x, y, quan=1/2, alpha=0.025, tcol1=2, tcol2=4,...) {
  
  require(stats)
  #library(rrcov)  
  x <- as.matrix(cbind(x,y))
  x <- na.omit(x)
  
  covr <- covMcd(x, cor=TRUE, alpha=quan)
  cov.svd <- svd(cov(x), nv = 0)
  covr.svd <- svd(covr$cov, nv = 0)
  r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
  rr <- covr.svd[["u"]] %*% diag(sqrt(covr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
  points(x, xlim=c(min(c(x[, 1], tt[, 1],ttr[,1])), max(c(x[, 1], tt[, 1],ttr[,1]))), ylim=c(min(c(x[, 2], tt[, 2],ttr[,2])), max(c(x[, 2], tt[, 2],ttr[,2]))), ...)

if (is.numeric(cor(x)[1,2])) title(main=list(paste("CC =",round(cor(x)[1,2],2),"                  "), col=tcol1), cex.main=1.5, line = -1.5) else title(main=list(paste("CC =",cor(x)[1,2],"                  "), col=tcol1), cex.main=1.5, line = -1.5)

if(is.numeric(covr$cor[1,2])) title(main=list(paste("                        RC =", round(covr$cor[1,2],2)),col=tcol2), cex.main=1.5, line=-1.5) else title(main="")

  ret <- list(cor.cla = cor(x)[1,2], cor.rob=covr$cor[1,2])
  ret
}

panelwhite75.corplot <-
function(x, y, quan=0.75, alpha=0.025, tcol1=2, tcol2=4,...) {

  #library(rrcov)  
  x <- as.matrix(cbind(x,y))

  covr <- covMcd(x, cor=TRUE, alpha=quan)
  cov.svd <- svd(cov(x), nv = 0)
  covr.svd <- svd(covr$cov, nv = 0)
  r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
  rr <- covr.svd[["u"]] %*% diag(sqrt(covr.svd[["d"]]))
  
  e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1-alpha,2)))
  tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x, 2, mean)
  ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
  points(x, xlim=c(min(c(x[, 1], tt[, 1],ttr[,1])), max(c(x[, 1], tt[, 1],ttr[,1]))), ylim=c(min(c(x[, 2], tt[, 2],ttr[,2])), max(c(x[, 2], tt[, 2],ttr[,2]))), ...)

if (is.numeric(cor(x)[1,2])) title(main=list(paste("CC =",round(cor(x)[1,2],2),"                  "), col=tcol1), cex.main=1.5, line = -1.5) else title(main=list(paste("CC =",cor(x)[1,2],"                  "), col=tcol1), cex.main=1.5, line = -1.5)

if(is.numeric(covr$cor[1,2])) title(main=list(paste("                        RC =", round(covr$cor[1,2],2)),col=tcol2), cex.main=1.5, line=-1.5) else title(main="")

  ret <- list(cor.cla = cor(x)[1,2], cor.rob=covr$cor[1,2])
  ret
}

