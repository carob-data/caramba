

.caramba_environment <- new.env(parent=emptyenv())

repo <- function() {
	x <- httr::GET("https://api.github.com/repos/carob-data/carob/git/trees/main?recursive=true")
	z <- httr::content(x)
	d <- z$tree
	s <- sapply(d, length)
	d <- d[s==6]
	d <- lapply(d, unlist)
	d <- do.call(rbind, d)
	d <- d[grepl("^scripts", d[,"path"]), ] |> data.frame()
	d$dataset_id <- gsub(".R", "", basename(d$path), ignore.case=TRUE)
	d$pending <- grepl("_pending", d$path)
	d
}



get_scripts <- function() {	
	if (is.null(.caramba_environment$scripts)) {
		.caramba_environment$scripts <- repo()
	} 
	.caramba_environment$scripts	
}


get_metadata <- function() {
	if (is.null(.caramba_environment$meta)) {
		u <- "https://geodata.ucdavis.edu/carob/carob_all_metadata.csv"
		f <- tempfile()
		utils::download.file(u, f, quiet=TRUE)
		.caramba_environment$meta <- utils::read.csv(f)
	} 
	.caramba_environment$meta
}



get_standardized <- function(did, path, overwrite) {
	path <- file.path(path, "standardized")
	dir.create(path, FALSE, TRUE)
	
	if (!overwrite) {
		fcsv <- file.path(path, paste0(did, c(".csv", "_meta.csv")))
		if (all(file.exists(fcsv))) {
			return(fcsv)
		}
	}	
	url <- paste0("https://geodata.ucdavis.edu/carob/zip/", did, ".zip")
	zf <- file.path(path, basename(url))
	test <- try(utils::download.file(url, zf, mode="wb", quiet=TRUE), silent=TRUE)
	if (test == 0) {
		ff <- utils::unzip(zf, exdir=path)
	} else {
		NULL
	}
}

check_script_ongithub <- function(did) {
	d <- get_scripts()
	i <- stats::na.omit(match(did, d$dataset_id))
	if (length(i) == 0) {
		warning(paste("not available: ", did))
		return(NULL)
	}
	if (d$pending[i]) {
		warning(paste("ignored pending dataset: ", did))
		return(NULL)
	}
	
	dirname(gsub("scripts/", "", d$path[i]))
}


get_raw <- function(did, group, path, overwrite) {

	rfile <- paste0(did, ".R")
	if (!overwrite) {
		ff <- list.files(file.path(path, "data", "clean", group), pattern=did, full.names=TRUE)
		if (length(ff) > 1) {
			return(ff)
		}
	}
	
	u <- paste0("https://raw.githubusercontent.com/carob-data/carob/refs/heads/main/scripts/", group, "/", rfile)
	x <- httr::GET(u)
	if (x$status != 200) {
		warning(paste("could not access script: ", basename(u)))
		return(NULL)
	}
	r <- httr::content(x)	
	carob_script <- NULL
	carob_script <- eval(parse(text=r)) 
	message(paste("processing", basename(u))); utils::flush.console()
	if (carob_script(path)) {
		ff <- list.files(file.path(path, "data/clean"), full.names=TRUE, recursive=TRUE, pattern=did)
		rpath <- file.path(path, "scripts", rfile)
		dir.create(dirname(rpath), FALSE, TRUE)
		writeLines(r, rpath)
		ff
	} else {
		warning(paste("script failed: ", basename(u)))
		NULL
	}
}



get_path <- function(path) {
	if (is.null(path)) {
		path <- file.path(tempdir(), "carob")
	} else {
		stopifnot(file.exists(path))
	}
	path <- file.path(path)
	dir.create(path, FALSE, TRUE)
	path
}


read_set <- function(ff, did) {
	nms <- gsub(paste0(did, "|\\.csv$"), "", basename(ff))
	nms[nms==""] <- "wide"
	nms[nms=="_meta"] <- "metadata"
	nms <- gsub("_", "", nms)
	x <- lapply(ff, utils::read.csv)
	names(x) <- nms
	x
}


read_one_dataset <- function(m, did, path, read, overwrite, original){
	i <- stats::na.omit(match(tolower(did), tolower(m$dataset_id)))[1]
	if (length(i) > 0) {
		group <- m$group[i]
		if ((!original) && grepl("CC|ETALAB|not specified", m$license[i])) {
			out <- try(get_standardized(did, path, overwrite))
			if (inherits(out, "try-error")) {
				out <- get_raw(did, group, path, overwrite)
			}
		} else {
			out <- get_raw(did, group, path, overwrite)
		}
	} else {
		# to check if perhaps the script exists but is not in the published metadata yet
		group <- check_script_ongithub(did)
		if (!is.null(group)) {
			out <- get_raw(did, group, path, overwrite)
		}
	}
	out <- grep(".csv$", out, value=TRUE)
	if (read) {
		read_set(out, did)
	} else {
		out
	}
}


carob_dataset <- function(uri, path=NULL, read=TRUE, overwrite=FALSE, from_source=FALSE) {
	m <- caramba:::get_metadata()
	uri <- unique(uri)
	dids <- ifelse (grepl(":|/", uri), yuri::simpleURI(uri), uri)
	path <- caramba:::get_path(path)

	out <- lapply(dids, function(did) caramba:::read_one_dataset(m, did, path, read, overwrite, from_source))

	if (read) {
		wide <- lapply(out, \(x) x$wide)
		long <- lapply(out, \(x) x$long)
		meta <- lapply(out, \(x) x$metadata)
		out <- list()
		out$wide <- do.call(bindr, wide)
		out$long <- do.call(bindr, long)
		out$meta <- do.call(bindr, meta)
	} else {
		names(out) <- uri
	}
	out
}


read_col <- function(ff, group) {
	nms <- gsub(paste0("carob_", group, "|-cc|\\.csv$"), "", basename(ff))
	nms[nms==""] <- "wide"
	nms <- gsub("_", "", nms)
	x <- lapply(ff, utils::read.csv)
	names(x) <- nms
	x
}


carob_collection <- function(group, path=NULL, read=TRUE, overwrite=FALSE) {
	d <- file.path(get_path(path), "compiled")
	dir.create(d, FALSE, FALSE)
	if (!overwrite) {
		fcsv <- file.path(d, paste0("carob_", group, "-cc.csv"))
		if (file.exists(fcsv)) {
			ff <- list.files(pattern=paste0("^carob_", group, ".*\\.csv$"), d, full.names=TRUE)
			if (read) {
				return(read_col(ff))
			} 
			return(ff)
		}
	}	
	url <- paste0("https://geodata.ucdavis.edu/carob/carob_", group, "-cc.zip")
	zf <- file.path(d, basename(url))
	test <- try(utils::download.file(url, zf, mode="wb", quiet=TRUE), silent=TRUE)
	if (test == 0) {
		ff <- utils::unzip(zf, exdir=d)
		if (read) {
			read_col(ff, group)
		} else {
			ff
		}
	} else {
		warning(paste(group, "is not available"))
		NULL
	}
}



