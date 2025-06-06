

.caramba_environment <- new.env(parent=emptyenv())

repo <- function() {
	x <- httr::GET("https://api.github.com/repos/carob-data/carob/git/trees/master?recursive=true")
	z <- httr::content(x)
	d <- z$tree
	s <- sapply(d, length)
	d <- d[s==6]
	d <- lapply(d, unlist)
	d <- do.call(rbind, d)
	d <- d[grepl("^scripts", d[,"path"]), ] |> data.frame()
	d$dataset_id <- basename(d$path)
	d$pending <- grep("_pending", d$path)
	d
}


get_scripts <- function() {	
	if (is.null(.caramba_environment$scripts)) {
		.caramba_environment$scripts <- repo()
	} 
	.caramba_environment$scripts	
}


meta <- function() {
	u <- "https://geodata.ucdavis.edu/carob/carob_all_metadata.csv"
	f <- tempfile()
	download.file(u, f, quiet=TRUE)
	r <- read.csv(f)
}


get_meta <- function() {
	if (is.null(.caramba_environment$meta)) {
		.caramba_environment$meta <- meta()
	} 
	.caramba_environment$meta
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

	uri <- yuri::simpleURI(uri)

	d <- get_scripts()
	i <- stats::na.omit(match(uri, d$dataset_id))
	if (length(i) == 0) {
		stop("this URI is not in Carob")
	}
	if (d$pending[i]) {
		stop("this dataset is pending and this function won't run it")	
	}

	u <- paste0("https://raw.githubusercontent.com/carob-data/carob/refs/heads/master/", d$path[i])
	f <- tempfile()
	download.file(u, f, quiet=TRUE)
	r <- readLines(f)

	carob_script <- NULL
	carob_script <- eval(parse(text=r)) 
	carob_script(NULL)
}



get_dataset <- function(uri, path, overwrite=FALSE) {
	m <- get_meta()
# todo: vectorize over uri
	did <- yuri::simpleURI(uri)
	i <- stats::na.omit(match(did, m$dataset_id))[1]
	if (length(i) > 0) {
		if (grepl("CC", d$license[i])) {
			get_standardized(uri, path, overwrite) {	
		} else {
			get_raw(uri, path, overwrite)
		}
	} else {
		# to check if perhaps the script exists but is not in the published metadata yet
		get_raw(uri, path, overwrite)
	}
}




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



