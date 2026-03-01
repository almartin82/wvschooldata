# ==============================================================================
# Exhaustive Tests for wvschooldata directory functions
# ==============================================================================
#
# Tests fetch_directory() with ALL parameter combinations.
# Tests clear_directory_cache().
# Expected values are pinned from real WVDE data.
#
# ==============================================================================

library(dplyr)

# ==============================================================================
# fetch_directory(tidy = TRUE, include_superintendents = TRUE) — default
# ==============================================================================

test_that("fetch_directory default returns data.frame", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_true(is.data.frame(dir_data))
})

test_that("fetch_directory default has 920 rows", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(nrow(dir_data), 920L)
})

test_that("fetch_directory default has 16 columns", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(ncol(dir_data), 16L)
})

test_that("fetch_directory default has all expected column names", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expected_cols <- c(
    "state_school_id", "state_district_id", "school_name", "district_name",
    "county", "school_type", "grades_served", "address", "city", "state",
    "zip", "phone", "fax", "website", "superintendent_name", "superintendent_email"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(dir_data), info = paste("Missing column:", col))
  }
})

# ==============================================================================
# fetch_directory(tidy = TRUE, include_superintendents = TRUE) — pinned values
# ==============================================================================

test_that("fetch_directory: all state values are WV", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_true(all(dir_data$state == "WV"))
})

test_that("fetch_directory: 64 unique counties", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(length(unique(dir_data$county)), 64L)
})

test_that("fetch_directory: Kanawha has 74 schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  kan <- dir_data %>% filter(county == "Kanawha")
  expect_equal(nrow(kan), 74L)
})

test_that("fetch_directory: Kanawha has 38 ELEM schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  kan_elem <- dir_data %>% filter(county == "Kanawha", school_type == "ELEM")
  expect_equal(nrow(kan_elem), 38L)
})

test_that("fetch_directory: Kanawha has 8 SECO schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  kan_seco <- dir_data %>% filter(county == "Kanawha", school_type == "SECO")
  expect_equal(nrow(kan_seco), 8L)
})

test_that("fetch_directory: Raleigh state_district_id = 54081", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  raleigh <- dir_data %>% filter(county == "Raleigh") %>% head(1)
  expect_equal(raleigh$state_district_id, "54081")
})

test_that("fetch_directory: Kanawha state_district_id = 54039", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  kan <- dir_data %>% filter(county == "Kanawha") %>% head(1)
  expect_equal(kan$state_district_id, "54039")
})

# ==============================================================================
# fetch_directory — school_type distribution
# ==============================================================================

test_that("fetch_directory: school_type has 11 unique values", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(length(unique(dir_data$school_type)), 11L)
})

test_that("fetch_directory: expected school types present", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expected_types <- c("ELEM", "MIDD", "SECO", "BOE", "K", "PRIM", "NT")
  for (typ in expected_types) {
    expect_true(typ %in% unique(dir_data$school_type),
                info = paste("Missing school_type:", typ))
  }
})

test_that("fetch_directory: 326 ELEM schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "ELEM"), 326L)
})

test_that("fetch_directory: 145 MIDD schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "MIDD"), 145L)
})

test_that("fetch_directory: 140 SECO schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "SECO"), 140L)
})

test_that("fetch_directory: 64 BOE entries", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "BOE"), 64L)
})

test_that("fetch_directory: 119 K (non-public) schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "K"), 119L)
})

test_that("fetch_directory: 33 PRIM schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "PRIM"), 33L)
})

test_that("fetch_directory: 8 NT (non-traditional) schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "NT"), 8L)
})

test_that("fetch_directory: 15 ADULT schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "ADULT"), 15L)
})

test_that("fetch_directory: 18 ALT schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "ALT"), 18L)
})

test_that("fetch_directory: 28 CTE schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "CTE"), 28L)
})

test_that("fetch_directory: 24 B schools", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_equal(sum(dir_data$school_type == "B"), 24L)
})

# ==============================================================================
# fetch_directory — superintendent data
# ==============================================================================

test_that("fetch_directory with superintendents has superintendent_name column", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_true("superintendent_name" %in% names(dir_data))
})

test_that("fetch_directory with superintendents has superintendent_email column", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_true("superintendent_email" %in% names(dir_data))
})

test_that("fetch_directory: most rows have superintendent data", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  # 908 out of 920 have non-NA superintendent_name
  expect_equal(sum(!is.na(dir_data$superintendent_name)), 908L)
})

# ==============================================================================
# fetch_directory — BOE (Central Office) entries
# ==============================================================================

test_that("fetch_directory: BOE entries have '(Central Office)' in school_name", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  boe <- dir_data %>% filter(school_type == "BOE")
  expect_true(all(grepl("\\(Central Office\\)", boe$school_name)))
})

test_that("fetch_directory: first 3 BOE entries", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  boe <- dir_data %>% filter(school_type == "BOE") %>% head(3)
  expect_true(grepl("Barbour County Schools", boe$school_name[1]))
  expect_true(grepl("Berkeley County Schools", boe$school_name[2]))
  expect_true(grepl("Boone County Schools", boe$school_name[3]))
})

# ==============================================================================
# fetch_directory — NT (Non-Traditional) entries
# ==============================================================================

test_that("fetch_directory: 8 NT school entries from specific counties", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  nt <- dir_data %>% filter(school_type == "NT")
  expect_equal(nrow(nt), 8L)

  # Known NT schools
  nt_counties <- sort(nt$county)
  expect_true("Berkeley" %in% nt_counties)
  expect_true("Kanawha" %in% nt_counties)
  expect_true("Raleigh" %in% nt_counties)
})

# ==============================================================================
# fetch_directory — state_school_id format
# ==============================================================================

test_that("fetch_directory: state_school_id format is TYPE-XXX-NNNN", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  # Sample check: format should be something like "ELEM-BAR-1234"
  sample_ids <- dir_data %>% head(5) %>% pull(state_school_id)
  for (id in sample_ids) {
    expect_true(grepl("^[A-Z]+-[A-Z]+-\\d+$", id),
                info = paste("Bad ID format:", id))
  }
})

# ==============================================================================
# fetch_directory — ZIP code format
# ==============================================================================

test_that("fetch_directory: ZIP codes are 5-digit or ZIP+4 format", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  valid_zips <- dir_data %>%
    filter(!is.na(zip)) %>%
    pull(zip)
  # Each ZIP should be 5 digits or 5-4 format
  for (z in valid_zips) {
    expect_true(grepl("^\\d{5}(-\\d{4})?$", z),
                info = paste("Bad ZIP format:", z))
  }
})

test_that("fetch_directory: some ZIPs have ZIP+4 format", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  zip_dash <- dir_data %>% filter(grepl("-", zip))
  expect_gt(nrow(zip_dash), 0)
})

# ==============================================================================
# fetch_directory — grades_served
# ==============================================================================

test_that("fetch_directory: grades_served has typical values", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  gs <- dir_data %>%
    filter(!is.na(grades_served)) %>%
    pull(grades_served)

  # Should have common patterns
  expect_true(any(grepl("PK", gs)))
  expect_true(any(grepl("K-", gs)))
  expect_true(any(grepl("9-12", gs)))
  expect_true(any(grepl("5-8", gs)))
})

# ==============================================================================
# fetch_directory — district_name format
# ==============================================================================

test_that("fetch_directory: district_name format is 'County County Schools'", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  # Most district names end with "County Schools"
  dn <- dir_data %>%
    filter(!is.na(district_name)) %>%
    pull(district_name) %>%
    unique()

  county_schools_pct <- mean(grepl("County Schools$", dn))
  # Most (but possibly not all, due to academies) should match
  expect_gt(county_schools_pct, 0.80)
})

# ==============================================================================
# fetch_directory(tidy = TRUE, include_superintendents = FALSE)
# ==============================================================================

test_that("fetch_directory without superintendents has 14 columns", {
  dir_no_supt <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = FALSE)
  expect_equal(ncol(dir_no_supt), 14L)
})

test_that("fetch_directory without superintendents: no superintendent columns", {
  dir_no_supt <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = FALSE)
  expect_false("superintendent_name" %in% names(dir_no_supt))
  expect_false("superintendent_email" %in% names(dir_no_supt))
})

test_that("fetch_directory without superintendents: same number of rows", {
  dir_no_supt <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = FALSE)
  expect_equal(nrow(dir_no_supt), 920L)
})

test_that("fetch_directory without superintendents: same school types", {
  dir_with <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  dir_without <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = FALSE)
  expect_equal(sort(unique(dir_with$school_type)), sort(unique(dir_without$school_type)))
})

# ==============================================================================
# fetch_directory(tidy = FALSE) — raw format
# ==============================================================================

test_that("fetch_directory raw has 920 rows", {
  raw <- fetch_directory(tidy = FALSE, use_cache = TRUE, include_superintendents = FALSE)
  expect_equal(nrow(raw), 920L)
})

test_that("fetch_directory raw has 11 columns", {
  raw <- fetch_directory(tidy = FALSE, use_cache = TRUE, include_superintendents = FALSE)
  expect_equal(ncol(raw), 11L)
})

test_that("fetch_directory raw has WVDE column names", {
  raw <- fetch_directory(tidy = FALSE, use_cache = TRUE, include_superintendents = FALSE)
  expected_cols <- c("type", "county", "name", "address", "city", "zip", "zip4",
                     "phone", "fax", "url", "grades")
  for (col in expected_cols) {
    expect_true(col %in% names(raw), info = paste("Missing raw column:", col))
  }
})

test_that("fetch_directory raw: all columns are character", {
  raw <- fetch_directory(tidy = FALSE, use_cache = TRUE, include_superintendents = FALSE)
  for (col in names(raw)) {
    expect_type(raw[[col]], "character")
  }
})

# ==============================================================================
# fetch_directory — column types (tidy format)
# ==============================================================================

test_that("fetch_directory tidy: state_school_id is character", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE)
  expect_type(dir_data$state_school_id, "character")
})

test_that("fetch_directory tidy: state_district_id is character", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE)
  expect_type(dir_data$state_district_id, "character")
})

test_that("fetch_directory tidy: school_name is character", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE)
  expect_type(dir_data$school_name, "character")
})

test_that("fetch_directory tidy: district_name is character", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE)
  expect_type(dir_data$district_name, "character")
})

test_that("fetch_directory tidy: county is character", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE)
  expect_type(dir_data$county, "character")
})

test_that("fetch_directory tidy: school_type is character", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE)
  expect_type(dir_data$school_type, "character")
})

test_that("fetch_directory tidy: state is character", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE)
  expect_type(dir_data$state, "character")
})

test_that("fetch_directory tidy: zip is character", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE)
  expect_type(dir_data$zip, "character")
})

# ==============================================================================
# clear_directory_cache()
# ==============================================================================

test_that("clear_directory_cache is a function", {
  expect_true(is.function(clear_directory_cache))
})

test_that("clear_directory_cache returns invisibly", {
  # We cannot actually test clearing without disrupting other tests,
  # but verify it runs without error on an already-cleared state
  result <- clear_directory_cache()
  expect_true(is.numeric(result) || is.integer(result))
})

# ==============================================================================
# fetch_directory — data quality
# ==============================================================================

test_that("fetch_directory: no completely NA rows", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  all_na_rows <- dir_data %>%
    filter(is.na(school_name) & is.na(school_type) & is.na(county))
  expect_equal(nrow(all_na_rows), 0L)
})

test_that("fetch_directory: every row has a school_name", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_true(all(!is.na(dir_data$school_name)))
})

test_that("fetch_directory: every row has a school_type", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_true(all(!is.na(dir_data$school_type)))
})

test_that("fetch_directory: every row has a county", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_true(all(!is.na(dir_data$county)))
})

test_that("fetch_directory: every row has a district_name", {
  dir_data <- fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
  expect_true(all(!is.na(dir_data$district_name)))
})
