# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-12
#
# Script Description: script to read datasets used in package.

standard <- read.csv("data-raw/standard.csv",
                     na.strings = "")
zooniverse <- read.csv("data-raw/zooniverse.csv")
traptagger <- read.csv("data-raw/traptagger.csv")
digikam <- read.csv("data-raw/digikam.csv")

# Save data
usethis::use_data(standard, 
                  zooniverse, traptagger, digikam,
                  overwrite = TRUE)
