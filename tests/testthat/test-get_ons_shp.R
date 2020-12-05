test_that("ONS Gen2 shapefile shapefile is loaded from url with class sf", {
  ons_shp <- get_ons_shp()
  testthat::expect_s3_class(object = ons_shp,
                             class="sf")
})
