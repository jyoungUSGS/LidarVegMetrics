# http://ecology.msu.montana.edu/labdsv/R/labs/
# Lab 10

# ca()
# This particular version of CA is implemented in pure S, and does not call any C or FORTRAN code. 
# Accordingly it's a little slow, but can be used by anyone by simply cutting and pasting. 
# In addition, the algorithm is easily understood (not hidden in a compiled program). 
# The variable naming convention follows Legendre and Legendre (1998) in most parts.


ca <- function(x)
{
        p <- as.matrix(x/sum(x))
        rs <- as.vector(apply(p,1,sum))
        cs <- as.vector(apply(p,2,sum))
        cp <- rs %*% t(cs)
        Qbar <- as.matrix((p - cp) / sqrt(cp))
        Q.svd <- svd(Qbar)
        V <- diag(1/sqrt(cs)) %*% Q.svd$v
        Vhat <- diag(1/sqrt(rs)) %*% Q.svd$u
        F <- diag(1/rs) %*% p %*% V
        Fhat <- diag(1/cs) %*% t(p) %*% Vhat
        tmp <- list(V=V,Vhat=Vhat,F=F,Fhat=Fhat)
        class(tmp) <- "ca"
        return(tmp)
}

# plot.ca()


plot.ca <-
function(ca,x=1,y=2,scaling=1,title="",pltpch=1,spcpch=3,pltcol=1,spccol=2,pltcex=1, spccex=1,...)
{
        if (scaling == 1) {
                plot(ca$V[,x],ca$V[,y],type='n',asp=1,xlab=paste("CA",x),ylab=paste("CA",y),main=title)
                points(ca$F[,x],ca$F[,y],pch=pltpch,col=pltcol, cex=pltcex)
                points(ca$V[,x],ca$V[,y],pch=spcpch,col=spccol, cex=spccex)
        } else {
                plot(ca$Vhat[,x],ca$Vhat[,y],type='n',asp=1,xlab=paste("CA",x),ylab=paste("CA",y),main=title)
                points(ca$Vhat[,x],ca$Vhat[,y],pch=pltpch,col=pltcol,cex=pltcex )
                points(ca$Fhat[,x],ca$Fhat[,y],pch=spcpch,col=spccol, cex=spccex)
        }
}

# specid.ca()


specid.ca <- function(ca, ids=seq(1:nrow(ca$V)), x=1, y=2, scaling=1, all=FALSE,
col=2, cex=1)
{
        if(missing(ca)) {
                stop("You must specify a ca object from ca()")
        }
        if(scaling==1) { 
                if(all) {
                        text(ca$V[,x],ca$V[,y],ids,col=col, cex=cex)
                } else {
                        identify(ca$V[,x],ca$V[,y],ids,col=col, cex=cex)
                }
        } else {
                if(all) {
                        text(ca$Fhat[,x],ca$Fhat[,y],ids,col=col, cex=cex)
                } else {
                        identify(ca$Fhat[,x],ca$Fhat[,y],ids,col=col, cex=cex)
                }
        }
}

# plotid.ca()


plotid.ca <- function(ca, ids=seq(1:nrow(ca$F)),x = 1, y = 2, scaling=1, col = 1, cex=1)
{
        if(missing(ca)) {
                stop("You must specify a ca object from ca()")
        }
        if(scaling==1) {
                identify(ca$F[,x],ca$F[,y],ids, cex=cex)
        } else {
                identify(ca$Vhat[,x],ca$Vhat[,y],ids, cex=cex)
        }
}

# points.ca()


points.ca <- function(ca, logical, x = 1, y = 2, scaling=1, col = 3,  pch = "*", cex=1, ...)
{
        if(missing(ca)) {
                stop("You must specify a ca object from ca()")
        }
        if(missing(logical)) {
                stop("You must specify a logical subscript")
        }
        if(scaling==1) {
                points(ca$F[, x][logical], ca$F[, y][logical],col=col,pch=pch,cex=cex)
        } else {
                points(ca$Vhat[, x][logical], ca$Vhat[, y][logical],col=col,pch=pch,cex=cex)
        }
}

# surf.ca()


surf.ca <- function(ca, var, x=1, y=2, scaling=1, col=3, labcex=0.8,
family=gaussian, ...)
{
        if(missing(ca)) {
                stop("You must specify a ca object from ca()")
        }
        if(missing(var)) {
                stop("You must specify a variable to surface")
        }
        if (scaling==1) {
                x <- ca$F[,x]
                y <- ca$F[,y]
        } else {
                x <- ca$Vhat[,x]
                y <- ca$Vhat[,y]
        }
        if (is.logical(var)) {
                tmp <- gam(var~s(x)+s(y),family=binomial)
        } else {
                tmp <- gam(var~s(x)+s(y),family=family)
        }
        contour(interp(x,y,fitted(tmp)),add=T,col=col, labcex=labcex, ...)
        print(tmp)
        d2  <- (tmp$null.deviance-tmp$deviance)/tmp$null.deviance
        cat(paste("D^2 = ",formatC(d2,width=4),"\n"))
		return(tmp)
}

# An alternative ordisurf function that produces an D^2 statistic for comparison to other labdsv surf 
# functions.

ordisurf <- function (x, y, choices = c(1, 2), knots = 10, family = "gaussian", 
    col = "red", thinplate = TRUE, add = FALSE, display = "sites", 
    w = weights(x), main, nlevels = 10, levels, labcex = 0.6, 
    ...) 
{
    GRID = 25
    w <- eval(w)
    if (!is.null(w) && length(w) == 1) 
        w <- NULL
    if (!require(mgcv)) 
        stop("Requires package `mgcv'")
    X <- scores(x, choices = choices, display = display, ...)
    x1 <- X[, 1]
    x2 <- X[, 2]
    if (thinplate) 
        mod <- gam(y ~ s(x1, x2, k = knots), family = family, 
            weights = w)
    else mod <- gam(y ~ s(x1, k = knots) + s(x2, k = knots), 
            family = family, weights = w)
    d2 <- (mod$null.deviance - mod$deviance)/mod$null.deviance
    cat(paste("D^2 = ", formatC(d2, width = 4), "\n"))
    xn1 <- seq(min(x1), max(x1), len = GRID)
    xn2 <- seq(min(x2), max(x2), len = GRID)
    newd <- expand.grid(x1 = xn1, x2 = xn2)
    fit <- predict(mod, type = "response", newdata = as.data.frame(newd))
    poly <- chull(cbind(x1, x2))
    poly <- c(poly, poly[1])
    npol <- length(poly)
    np <- nrow(newd)
    inpoly <- numeric(np)
    inpoly <- .C("pnpoly", as.integer(npol), as.double(x1[poly]), 
        as.double(x2[poly]), as.integer(np), as.double(newd[, 
            1]), as.double(newd[, 2]), inpoly = as.integer(inpoly), 
        PACKAGE = "vegan")$inpoly
    is.na(fit) <- inpoly == 0
    if (!add) {
        plot(X, asp = 1, ...)
    }
    if (!missing(main) || (missing(main) && !add)) {
        if (missing(main)) 
            main <- deparse(substitute(y))
        title(main = main)
    }
    if (missing(levels)) 
        levels <- pretty(range(fit, finite = TRUE), nlevels)
    contour(xn1, xn2, matrix(fit, nrow = GRID), col = col, add = TRUE, 
        levels = levels, labcex = labcex, drawlabels = !is.null(labcex) && 
            labcex > 0)
    return(mod)
}
