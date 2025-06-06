
.onLoad <- function(libname, pkgname) {
	options(timeout = max(300, getOption("timeout")))
}


