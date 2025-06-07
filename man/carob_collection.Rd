\name{carob_collection}

\alias{carob_collection}

\title{
Get the a Carob collection
}

\description{
Download a Carob collection. That is, a compilation of multiple standardized individual datasets. Note that only the datasets with a Creative Commons license of comparable are included in the collections.
}

\usage{
carob_collection(group, path=NULL, read=TRUE, overwrite=FALSE)
}
 
\arguments{
  \item{group}{character. A group name such as "agronomy". See \url{https://carob-data.org/data}}
  \item{path}{character. The path to download the data to}
  \item{read}{logical. If \code{TRUE} the downloaded files are read after downloading}
  \item{overwrite}{logical. If \code{TRUE} already downloaded files are overwritten}
}

\value{
character (filenames, if \code{read=FALSE}
list (if \code{read=TRUE})
}

\seealso{\code{\link{carob_dataset}}}

\examples{
\dontrun{
x <- carob_collection("varieties")
names(x)
head(x$metadata[,c(1, 10:12, 15:16, 20:21)])
d <- x$wide
plot(sort(d$yield))
}
}
