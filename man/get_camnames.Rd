% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{get_camnames}
\alias{get_camnames}
\title{Get camera names}
\usage{
get_camnames(cameras, locations, silence_warnings = FALSE, logger = NA)
}
\arguments{
\item{cameras}{a character vector of camera names}

\item{locations}{a character vector of locations (must be the same length as \code{cameras})}

\item{silence_warnings}{Should a warning be displayed when \code{locationID} is \code{NA}?}

\item{logger}{a \code{log4r} \code{logger} object if you want logging (can be created with \code{create_logger}),
else \code{NA}.}
}
\value{
A vector of camera names without the location prefix (if it was present)
}
\description{
Subsets the locations IDs from the camera names vector.
}
\examples{
get_camnames(c("APN_A01", "MAD_B01"), c("APN", "MAD"))
}
