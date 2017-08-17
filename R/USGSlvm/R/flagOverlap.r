flagOverlap <- function(x) {
    # WARNING this tool is under major development due to
    # inconsistent use of classification values.
    if (! "classFlagOverlap" %in% names(x)){
      x$classFlagOverlap <- 0
    }
    x$classFlagOverlap[x$Classification %in% c(12, 17,18,25)] <- 1
    x$Classification[x$Classification==12] <- 1
    x$Classification[x$Classification==17] <- 1
    x$Classification[x$Classification==18] <- 2
    x$Classification[x$Classification==25] <- 9
    return(x)
}
