% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{clean_species}
\alias{clean_species}
\title{Standardize species}
\usage{
clean_species(species)
}
\arguments{
\item{species}{vector of species names}
}
\value{
the vector of species names with standardized names, according to the
species names that have already been encountered in the past datasets.
}
\description{
Eliminate species duplicate names (things like \code{birdofprey} and \code{birdsofprey})
}
\examples{
species <- c("zebraplains", "zebraburchells", "duiker", 
             "duikercommon", "aardvark", "lionfemale")
clean_species(species)
}
