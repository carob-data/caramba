\name{carob_dataset}

\alias{carob_dataset}

\title{
Get the a Carob collection
}

\description{
Download a Carob collection. That is, a compilation of multiple standardized individual datasets. Note that only the datasets with a Creative Commons license of comparable are included in the collections.
}

\usage{
carob_dataset(uri, path=NULL, read=TRUE, overwrite=FALSE)
}
 
\arguments{
  \item{uri}{character. The URI of a datase (doi or hdl)}
  \item{path}{character. The path to download the data to}
  \item{read}{logical. If \code{TRUE} the downloaded files are read after downloading}
  \item{overwrite}{logical. If \code{TRUE} already downloaded files are overwritten}
}

\value{
character (filenames, if \code{read=FALSE}
list (if \code{read=TRUE})
}

\seealso{\code{\link{carob_collection}}}

\examples{
\dontrun{
ff <- carob_dataset("doi:10.7910/DVN/LTS278", read=FALSE)
basename(ff)

x <- carob_dataset("doi:10.7910/DVN/LTS278")
names(x)
t(x$metadata)
d <- x$wide
head(d[,1:7])
 
plot(sort(d$yield))
}
}
