# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-12
#
# Script Description: script to read standard column names.

standard <- read.csv("data-raw/standard.csv")

# Save data
usethis::use_data(standard, overwrite = TRUE)