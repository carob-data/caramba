

get_compiled <- function(group, path, overwrite=FALSE) {
	d <- file.path(path, "compiled")
	dir.create(d, FALSE, FALSE)
	fcsv <- file.path(d, paste0("carob_", group, c(".csv", "_meta.csv")))
	if (!overwrite) {
		if (all(file.exists(fcsv))) {
			return(fcsv)
		}
	}	
	url <- paste0("https://geodata.ucdavis.edu/carob/carob_", group, "-cc.zip")
	zf <- file.path(d, basename(url))
	test <- try(download.file(url, zf, mode="wb", quiet=TRUE), silent=TRUE)
	if (test == 0) {
		unzip(zf, exdir=d)
	} else {
		warning(paste(uri, "is not available"))
		NULL
	}
}


get_standardized <- function(uri, path, overwrite=FALSE) {
	uri <- ifelse (grepl(":|/", uri), yuri::simpleURI(uri), uri)

	exd <- file.path(path, "clean")
	dir.create(exd, FALSE, FALSE)
	
	if (!overwrite) {
		fcsv <- file.path(exd, paste0(uri, "-cc", c(".csv", "_meta.csv")))
		if (all(file.exists(fcsv))) {
			return(fcsv)
		}
	}
	
	url <- paste0("https://geodata.ucdavis.edu/carob/zip/", uri, ".zip")
	zf <- file.path(exd, basename(url))
	
	test <- try(download.file(url, zf, mode="wb", quiet=TRUE), silent=TRUE)
	if (test == 0) {
		unzip(zf, exdir=exd)
	} else {
		warning(paste(uri, "is not available"))
		NULL
	}
}


get_raw <- function(uri, path) {



}



carob_data <- function() {

}




read_carob <- function(uri) {

# todo: vectorize over uri

	d <- all_github_scripts()
	uri <- yuri::simpleURI(uri)
	i <- stats::na.omit(match(uri, d$dataset))
	if (length(i) == 0) {
		stop("this URI is not in Carob")
	}
	if (d$pending[i]) {
		stop("this dataset is pending and this function won't run it")	
	}
	# first try to read from server 
	
	# else:
	u <- "https://raw.githubusercontent.com/reagro/carob/refs/heads/master/scripts/"
	u <- paste0(u, d$group[i], "/", d$dataset[i], ".R")
	
	r <- readLines(u)
	carob_script <- NULL
	carob_script <- eval(parse(text=r)) 
	carob_script(NULL)
}

