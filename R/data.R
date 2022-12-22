#' @title Standard column names
#' @description A dataframe listing the standard column names for the different Snapshot data formats.
#' @format A data frame with 39 rows and 4 variables:
#' \describe{
#'   \item{\code{zooniverse}}{character Expected columns names for Zooniverse data}
#'   \item{\code{traptagger}}{character Expected columns names for TrapTagger data}
#'   \item{\code{digikam}}{character Expected columns names for Digikam data}
#'   \item{\code{new}}{character Columns names for the output standardized data} 
#'}
"standard"

#' @title Zooniverse sample data
#' @description A dataset mimicking typical Zooniverse data (randomized rows)
#' @format A data frame with 100 rows and 24 variables:
#' \describe{
#'   \item{\code{capture_id}}{character ID for the capture composed of season#site#roll#capture}
#'   \item{\code{season}}{character Zooniverse season code}
#'   \item{\code{site}}{character Camera ID}
#'   \item{\code{roll}}{integer Roll (index for the camera service)}
#'   \item{\code{capture}}{integer ID for the capture per camera/season/roll}
#'   \item{\code{capture_date_local}}{character Date}
#'   \item{\code{capture_time_local}}{character Time}
#'   \item{\code{zooniverse_url_0}}{character Url for first photo (mock URL)}
#'   \item{\code{zooniverse_url_1}}{character Url for second photo (mock URL)}
#'   \item{\code{zooniverse_url_2}}{character Url for third photo (mock URL)}
#'   \item{\code{subject_id}}{integer Internal Zooniverse ID for the capture event (mock ID)}
#'   \item{\code{question__species}}{character Species}
#'   \item{\code{question__count_max}}{character Maximum count from the volunteers}
#'   \item{\code{question__count_median}}{character Median count from the volunteers}
#'   \item{\code{question__count_min}}{character Minimum count from the volunteers}
#'   \item{\code{question__standing}}{double Proportion of users who declared a standing behavior}
#'   \item{\code{question__resting}}{double Proportion of users who declared a resting behavior}
#'   \item{\code{question__moving}}{double Proportion of users who declared a moving behaviour}
#'   \item{\code{question__eating}}{double Proportion of users who declared a eating behaviour}
#'   \item{\code{question__interacting}}{double Proportion of users who declared an interacting behaviour}
#'   \item{\code{question__young_present}}{double Proportion of users who declared young presents}
#'   \item{\code{question__horns_visible}}{double Proportion of users who declared horns on the picture}
#'   \item{\code{p_users_identified_this_species}}{double Proportion of users who identified the consensus species}
#'   \item{\code{pielous_evenness_index}}{double Pielou evenness index}
#'}
"zooniverse"

#' @title Digikam sample data
#' @description A dataset mimicking typical Digikam data (randomized rows)
#' @format A data frame with 100 rows and 22 variables:
#' \describe{
#'   \item{\code{X}}{integer Row names (read by R when reading the csv file)}
#'   \item{\code{Station}}{character Camera}
#'   \item{\code{Species}}{character Species}
#'   \item{\code{DateTimeOriginal}}{character Date and time}
#'   \item{\code{Date}}{character Date}
#'   \item{\code{Time}}{character Time}
#'   \item{\code{delta.time.secs}}{integer Time elapsed since this species was last seen at this camera (seconds). Not relevant because rows were permuted.}
#'   \item{\code{delta.time.mins}}{double Time elapsed since this species was last seen at this camera (minutes). Not relevant because rows were permuted.}
#'   \item{\code{delta.time.hours}}{double Time elapsed since this species was last seen at this camera (hours). Not relevant because rows were permuted.}
#'   \item{\code{delta.time.days}}{double Time elapsed since this species was last seen at this camera (days). Not relevant because rows were permuted.}
#'   \item{\code{Directory}}{character Local directory where the original photo is}
#'   \item{\code{FileName}}{character Name of the original photo on the local storage}
#'   \item{\code{EXIF.Model}}{character Exif info}
#'   \item{\code{EXIF.Make}}{character Exif info}
#'   \item{\code{metadata_Species}}{character Species (other column)}
#'   \item{\code{metadata_Number}}{character Species count}
#'   \item{\code{metadata_Behaviour}}{character Tagged behaviors}
#'   \item{\code{metadata_Sex}}{character Tagged sex}
#'   \item{\code{n_images}}{integer Number of pictures associated to this event?}
#'   \item{\code{metadata_young_present}}{character Tagged youngs (Yes/No)}
#'   \item{\code{metadata_Numberofindividuals}}{logical Tagged number of individuals on the picture}
#'   \item{\code{HierarchicalSubject}}{character Summary column for all metadata_...} 
#'}
"digikam"

#' @title TrapTagger sample data
#' @description A dataset mimicking typical TrapTagger data (randomized rows)
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#'   \item{\code{Capture_ID}}{character ID for the capture composed of locationcapture#roll#Cam.Site}
#'   \item{\code{Cam.Site}}{character Camera}
#'   \item{\code{id}}{integer Picture ID}
#'   \item{\code{latitude}}{double Camera latitude (mock data)}
#'   \item{\code{longitude}}{double Camera longitude (mock data)}
#'   \item{\code{timestamp}}{character Date time}
#'   \item{\code{capture_labels}}{character Species seen on the picture}
#'   \item{\code{capture_sighting_count}}{integer Species count}
#'   \item{\code{capture_url}}{character Picture url (mock data)} 
#'}
"traptagger"