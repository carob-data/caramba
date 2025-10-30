\name{carob_dataset}

\alias{carob_dataset}

\title{
Get one of more Carob datasets
}

\description{
Download a Carob dataset. That is, an individual dataset for which a Carob script exists. 

If the dataset has a Creative Commons or compariable license the processed data is downloaded. In other cases, the data is downloaded from the original source and processed locally.
}

\usage{
carob_dataset(uri, path=NULL, read=TRUE, overwrite=FALSE, from_source=FALSE)
}
 
\arguments{
  \item{uri}{character. The URI of a datase (doi or hdl)}
  \item{path}{character. The path to download the data to}
  \item{read}{logical. If \code{TRUE} the downloaded files are read after downloading}
  \item{overwrite}{logical. If \code{TRUE} already downloaded files are overwritten}
  \item{from_source}{logical. If \code{TRUE}, instead of downloading the processes dataset from Carob (if availalbe given the license), the original data source is downloaded and processed locally}
}

\value{
list with elements "wide", "meta" (and perhaps more, such as "long") with data.frames; or, if \code{read=FALSE}, a list of filenames (one element per dataset).
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
