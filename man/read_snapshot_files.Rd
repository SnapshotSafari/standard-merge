% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files_handling.R
\name{read_snapshot_files}
\alias{read_snapshot_files}
\title{Read Snapshot files}
\usage{
read_snapshot_files(input, except, basepath, verbose = TRUE, logger = NA)
}
\arguments{
\item{input}{a character vector of valid paths: can be files and/or folders}

\item{except}{files to ignore (optional): all paths for (a part of) which match
the exact expression in \code{except} will be ignored.}

\item{basepath}{the part of the path that should be ignored when copying final
files (i.e. absolute path inside one's comupter that should not be copied in final file.)}

\item{verbose}{Should messages be displayed when reading a folder/file?}

\item{logger}{a \code{log4r} \code{logger} object if you want logging (can be created with \code{create_logger}),
else \code{NA}.}
}
\value{
A named list of dataframes. Each element of the list is a dataframe with
the contents of a file read from the files list given in input.
The names of the list are the file names from the root of input:
If the element from \code{input} is a file, it is a filename.
If the element from \code{input} is a folder, it is the relative path
from \code{basepath} to the file.
}
\description{
Reads files from a vector of files and/or folders
into a list of dataframes.
}
\details{
The files are assumed to be comma or semicolon-separated CSV.
If the filename is \verb{MAD_S2_full_report_0-50__agreement_corrected_fin.csv},
the function only reads the first 26 columns as this file has empty columns.
}
\examples{
\dontrun{
read_snapshot_files("path/to/datafolder/DHP", 
                    basepath = "path/to/datafolder", 
                    except = "DHP/DHP+OVE_same_file/*")
}
}
