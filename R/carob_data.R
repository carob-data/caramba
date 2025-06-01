
carob_data <- function() {

}




aggregated_data <- function(path, group, cc=FALSE) {

	if (cc) {
		f <- file.path(path, "data", "compiled", paste0("carob_", group, "-cc.csv"))	
	} else {
		f <- file.path(path, "data", "compiled", paste0("carob_", group, ".csv"))
	}
	if (!file.exists(f)) {
		stop("these data do not exist. First run 'make_carob'?")
	}
	data.frame(data.table::fread(f))	
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

