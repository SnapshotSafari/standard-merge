library(testthat)

test_that("Test clean cameras", {
  # A vector with all combinations possible
  cam <- c("KHO_E_B03", "KHO_M_A02", "M_A01", "C02", "OA03")
  cloc <- c("KHO", "KHO", "APN", "MOK", "OVE")
  classifier <- c("traptagger", "traptagger", "traptagger", "digikam", "zooniverse")
  
  res <- clean_cameras(cam, cloc, classifier)
  expected <- c("EB03", "MA02", "M_A01", "C02", "A03")
  expect_equal(res, expected)
})

test_that("Get camera names", {
  res <- get_camnames(c("APN_A01", "MAD_B01", "EVF_A01"), 
                      locations = c("APN", "MAD", "MAD"))
  expect_equal(res, c("A01", "B01", "EVF_A01"))
  
  expect_warning(get_camnames("MAD_A01", NA),
                 "Some locations are NA, so camnames might be incorrect if they contain an underscore")
  
})

test_that("Clean location", {
  res <- clean_locations(c("OA01", "KHOGA01", "KHOGA01", "DA01"), 
                         c("DHP", "KGA", "APN", "DHP"))
  expect_equal(res, c("OVE", "KHO", "APN", "DHP"))
  
  res <- suppressWarnings(clean_locations(c(NA, "A01", NA),
                                          c("DHP", NA, NA)))
  expect_equal(res, c("DHP", NA, NA))
})

test_that("Add location prefix", {
  res <- add_location_prefix(c("APN_A01", "B01", "C01", "AS1_B01"), 
                             locations = c("APN", "MAD", "MAD", "MAD"))
  expect_equal(res, c("APN_A01", "MAD_B01", "MAD_C01", "MAD_AS1_B01"))
  
  expect_warning(add_location_prefix("A01", NA),
                 "Some locations are NA: camera codes have been modified accordingly to NA_cam.")
})
