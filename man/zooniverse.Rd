% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{zooniverse}
\alias{zooniverse}
\title{Zooniverse sample data}
\format{
A data frame with 100 rows and 24 variables:
\describe{
\item{\code{capture_id}}{character ID for the capture composed of season#site#roll#capture}
\item{\code{season}}{character Zooniverse season code}
\item{\code{site}}{character Camera ID}
\item{\code{roll}}{integer Roll (index for the camera service)}
\item{\code{capture}}{integer ID for the capture per camera/season/roll}
\item{\code{capture_date_local}}{character Date}
\item{\code{capture_time_local}}{character Time}
\item{\code{zooniverse_url_0}}{character Url for first photo (mock URL)}
\item{\code{zooniverse_url_1}}{character Url for second photo (mock URL)}
\item{\code{zooniverse_url_2}}{character Url for third photo (mock URL)}
\item{\code{subject_id}}{integer Internal Zooniverse ID for the capture event (mock ID)}
\item{\code{question__species}}{character Species}
\item{\code{question__count_max}}{character Maximum count from the volunteers}
\item{\code{question__count_median}}{character Median count from the volunteers}
\item{\code{question__count_min}}{character Minimum count from the volunteers}
\item{\code{question__standing}}{double Proportion of users who declared a standing behavior}
\item{\code{question__resting}}{double Proportion of users who declared a resting behavior}
\item{\code{question__moving}}{double Proportion of users who declared a moving behaviour}
\item{\code{question__eating}}{double Proportion of users who declared a eating behaviour}
\item{\code{question__interacting}}{double Proportion of users who declared an interacting behaviour}
\item{\code{question__young_present}}{double Proportion of users who declared young presents}
\item{\code{question__horns_visible}}{double Proportion of users who declared horns on the picture}
\item{\code{p_users_identified_this_species}}{double Proportion of users who identified the consensus species}
\item{\code{pielous_evenness_index}}{double Pielou evenness index}
}
}
\usage{
zooniverse
}
\description{
A dataset mimicking typical Zooniverse data (randomized rows)
}
\keyword{datasets}
