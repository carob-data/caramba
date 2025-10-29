

bindr <- function(...) {
	d <- list(...)
	i <- sapply(d, is.null)
	if (all(i)) return(NULL)
	d <- d[!i]
	nms <- unique(unlist(lapply(d, names)))
	out <- lapply(d, 
			function(x) {
				x <- x[, colnames(x)!="", drop=FALSE]
					data.frame(c(x, 
						sapply(setdiff(nms, names(x)), function(y) NA)), check.names=FALSE)
				}
			)
				
	out$make.row.names <- FALSE
	do.call(rbind, out)
}
