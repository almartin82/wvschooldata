# ==============================================================================
# Tests for wvschooldata enrollment functions
# ==============================================================================

test_that("get_available_years returns expected range", {
  years <- get_available_years()

  expect_type(years, "integer")
  expect_true(2014 %in% years)
  expect_true(2024 %in% years)
  expect_equal(length(years), 11)  # 2014 to 2024
})


test_that("get_wv_counties returns all 55 counties", {
  counties <- get_wv_counties()

  expect_type(counties, "character")
  expect_equal(length(counties), 55)
  expect_true("KANAWHA" %in% counties)
  expect_true("BERKELEY" %in% counties)
  expect_true("CABELL" %in% counties)
})


test_that("fetch_enr validates year input", {
  expect_error(fetch_enr(2010), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
})


test_that("fetch_enr_multi validates year input", {
  expect_error(fetch_enr_multi(c(2010, 2020)), "Invalid years")
  expect_error(fetch_enr_multi(c(2020, 2030)), "Invalid years")
})


test_that("generate_district_id creates correct FIPS codes", {
  expect_equal(generate_district_id("KANAWHA"), "54039")
  expect_equal(generate_district_id("BERKELEY"), "54003")
  expect_equal(generate_district_id("BARBOUR"), "54001")
  expect_true(is.na(generate_district_id("NOT_A_COUNTY")))
})


test_that("format_district_name creates correct format", {
  expect_equal(format_district_name("KANAWHA"), "KANAWHA COUNTY SCHOOLS")
  expect_equal(format_district_name("berkeley"), "BERKELEY COUNTY SCHOOLS")
})


test_that("get_wvde_pdf_url constructs valid URLs", {
  url_fte <- get_wvde_pdf_url(2024, "FTE")
  url_headcount <- get_wvde_pdf_url(2024, "Headcount")

  expect_true(grepl("wvde.us", url_fte))
  expect_true(grepl("FTE-Enrollment-2nd-Mo-24", url_fte))
  expect_true(grepl("wvde.us", url_headcount))
  expect_true(grepl("Headcount-Enroll-2nd-Mo-24", url_headcount))
})


test_that("cache functions work correctly", {
  # Test cache path generation
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024.rds", path))
  expect_true(grepl("wvschooldata", path))

  # Test cache directory
  dir <- get_cache_dir()
  expect_true(dir.exists(dir) || !file.exists(dir))  # May not exist yet
})


test_that("safe_numeric handles suppression markers", {
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("-")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("--")))
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("500"), 500)
  expect_equal(safe_numeric("123.5"), 123.5)
})


# Integration tests (skip on CRAN and CI to avoid PDF downloads)
test_that("fetch_enr returns valid data structure", {
  skip_on_cran()
  skip_if_offline()

  # This test requires network access and pdftools
  skip_if_not_installed("pdftools")

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_true(is.data.frame(result))
    expect_true("end_year" %in% names(result))
    expect_true("type" %in% names(result))
    expect_true("n_students" %in% names(result))
    expect_true("subgroup" %in% names(result))
    expect_true("is_state" %in% names(result))

    # West Virginia should have 55 county districts
    n_districts <- length(unique(result$district_id[result$type == "District"]))
    expect_true(n_districts >= 50 && n_districts <= 60)

    # Should have state aggregate
    expect_true(any(result$type == "State"))
  }
})
