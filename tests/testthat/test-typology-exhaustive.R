# ==============================================================================
# Exhaustive Typology Tests for wvschooldata
# ==============================================================================
#
# Tests data structure, column types, naming standards, edge cases,
# and utility/cache functions.
#
# ==============================================================================

library(dplyr)

# ==============================================================================
# Naming Standards — subgroup values
# ==============================================================================

test_that("tidy enrollment uses standard subgroup name: total_enrollment", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("total_enrollment" %in% unique(enr$subgroup))
})

test_that("tidy enrollment does not use non-standard subgroup names", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  non_standard <- c("total", "Total", "all_students", "ALL")
  for (ns in non_standard) {
    expect_false(ns %in% unique(enr$subgroup),
                 info = paste("Non-standard subgroup found:", ns))
  }
})

# ==============================================================================
# Naming Standards — grade levels
# ==============================================================================

test_that("grade levels are UPPERCASE", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grades <- unique(enr$grade_level)
  for (g in grades) {
    expect_equal(g, toupper(g), info = paste("Grade not uppercase:", g))
  }
})

test_that("grade level K is not KG or KF", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grades <- unique(enr$grade_level)
  expect_false("KG" %in% grades)
  expect_false("KF" %in% grades)
  expect_true("K" %in% grades)
})

test_that("grade levels 01-12 are zero-padded", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grades <- unique(enr$grade_level)
  # 01-09 should be present (zero-padded)
  for (g in sprintf("%02d", 1:9)) {
    expect_true(g %in% grades, info = paste("Missing zero-padded grade:", g))
  }
  # Non-zero-padded versions should NOT be present
  for (g in as.character(1:9)) {
    expect_false(g %in% grades,
                 info = paste("Non-zero-padded grade found:", g))
  }
})

test_that("TOTAL grade level exists", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("TOTAL" %in% unique(enr$grade_level))
})

test_that("PK grade level exists", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("PK" %in% unique(enr$grade_level))
})

# ==============================================================================
# Naming Standards — entity flags
# ==============================================================================

test_that("is_state flag exists and is logical", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("is_state" %in% names(enr))
  expect_type(enr$is_state, "logical")
})

test_that("is_district flag exists and is logical", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("is_district" %in% names(enr))
  expect_type(enr$is_district, "logical")
})

test_that("is_campus flag exists and is logical", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("is_campus" %in% names(enr))
  expect_type(enr$is_campus, "logical")
})

test_that("is_charter flag exists and is logical", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true("is_charter" %in% names(enr))
  expect_type(enr$is_charter, "logical")
})

# ==============================================================================
# Naming Standards — district naming convention
# ==============================================================================

test_that("district names follow 'COUNTY COUNTY SCHOOLS' format", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  districts <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(district_name) %>%
    unique()

  for (d in districts) {
    expect_true(grepl("COUNTY SCHOOLS$", d),
                info = paste("Bad district name format:", d))
  }
})

test_that("district names are ALL CAPS", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  districts <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(district_name) %>%
    unique()

  for (d in districts) {
    expect_equal(d, toupper(d), info = paste("District not uppercase:", d))
  }
})

# ==============================================================================
# District ID standards — FIPS-based
# ==============================================================================

test_that("district_id starts with 54 (WV state FIPS)", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_ids <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(district_id)

  for (id in dist_ids) {
    expect_true(grepl("^54", unname(id)),
                info = paste("District ID doesn't start with 54:", id))
  }
})

test_that("district_id is 5-character string", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_ids <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(district_id)

  for (id in dist_ids) {
    expect_equal(nchar(unname(id)), 5L,
                 info = paste("District ID not 5 chars:", id))
  }
})

test_that("55 unique district_ids", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_ids <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(district_id) %>%
    unname() %>%
    unique()
  expect_equal(length(dist_ids), 55L)
})

test_that("state rows have NA district_id", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_ids <- enr %>%
    filter(is_state) %>%
    pull(district_id) %>%
    unique()
  expect_true(all(is.na(state_ids)))
})

test_that("specific FIPS codes are correct", {
  expect_equal(generate_district_id("KANAWHA"), "54039")
  expect_equal(generate_district_id("BERKELEY"), "54003")
  expect_equal(generate_district_id("BARBOUR"), "54001")
  expect_equal(generate_district_id("WYOMING"), "54109")
  expect_equal(generate_district_id("WOOD"), "54107")
  expect_equal(generate_district_id("WIRT"), "54105")
  expect_equal(generate_district_id("CABELL"), "54011")
  expect_equal(generate_district_id("MONONGALIA"), "54061")
  expect_equal(generate_district_id("RALEIGH"), "54081")
  expect_equal(generate_district_id("PUTNAM"), "54079")
})

test_that("generate_district_id returns NA for invalid county", {
  expect_true(is.na(generate_district_id("NOT_A_COUNTY")))
  expect_true(is.na(generate_district_id("")))
  expect_true(is.na(generate_district_id("CHARLESTON")))
})

# ==============================================================================
# format_district_name
# ==============================================================================

test_that("format_district_name appends COUNTY SCHOOLS", {
  expect_equal(format_district_name("KANAWHA"), "KANAWHA COUNTY SCHOOLS")
  expect_equal(format_district_name("MCDOWELL"), "MCDOWELL COUNTY SCHOOLS")
})

test_that("format_district_name uppercases input", {
  expect_equal(format_district_name("berkeley"), "BERKELEY COUNTY SCHOOLS")
  expect_equal(format_district_name("Kanawha"), "KANAWHA COUNTY SCHOOLS")
})

# ==============================================================================
# get_wv_counties — internal utility
# ==============================================================================

test_that("get_wv_counties returns 55 counties", {
  counties <- get_wv_counties()
  expect_equal(length(counties), 55L)
})

test_that("get_wv_counties are all uppercase", {
  counties <- get_wv_counties()
  for (c_name in counties) {
    expect_equal(c_name, toupper(c_name))
  }
})

test_that("get_wv_counties includes known counties", {
  counties <- get_wv_counties()
  known <- c("KANAWHA", "BERKELEY", "CABELL", "WOOD", "MONONGALIA",
             "RALEIGH", "PUTNAM", "MCDOWELL", "WIRT", "BARBOUR")
  for (k in known) {
    expect_true(k %in% counties, info = paste("Missing county:", k))
  }
})

test_that("get_wv_counties are unique", {
  counties <- get_wv_counties()
  expect_equal(length(counties), length(unique(counties)))
})

test_that("get_wv_counties are in a consistent order", {
  counties <- get_wv_counties()
  # MCDOWELL comes before MARION in the source (custom ordering)
  # Verify the list is identical across calls (deterministic)
  expect_equal(counties, get_wv_counties())
})

# ==============================================================================
# safe_numeric — suppression handling
# ==============================================================================

test_that("safe_numeric handles asterisk suppression", {
  expect_true(is.na(safe_numeric("*")))
})

test_that("safe_numeric handles dash suppression", {
  expect_true(is.na(safe_numeric("-")))
})

test_that("safe_numeric handles <5 suppression", {
  expect_true(is.na(safe_numeric("<5")))
})

test_that("safe_numeric handles N/A", {
  expect_true(is.na(safe_numeric("N/A")))
})

test_that("safe_numeric handles double-dash", {
  expect_true(is.na(safe_numeric("--")))
})

test_that("safe_numeric handles dot suppression", {
  expect_true(is.na(safe_numeric(".")))
})

test_that("safe_numeric handles -1 suppression", {
  expect_true(is.na(safe_numeric("-1")))
})

test_that("safe_numeric handles -2 suppression", {
  expect_true(is.na(safe_numeric("-2")))
})

test_that("safe_numeric handles -9 suppression", {
  expect_true(is.na(safe_numeric("-9")))
})

test_that("safe_numeric handles PS suppression", {
  expect_true(is.na(safe_numeric("PS")))
})

test_that("safe_numeric handles empty string", {
  expect_true(is.na(safe_numeric("")))
})

test_that("safe_numeric handles NA string", {
  expect_true(is.na(safe_numeric("NA")))
})

test_that("safe_numeric converts comma-separated numbers", {
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("12,345"), 12345)
  expect_equal(safe_numeric("1,234,567"), 1234567)
})

test_that("safe_numeric converts plain integers", {
  expect_equal(safe_numeric("500"), 500)
  expect_equal(safe_numeric("0"), 0)
  expect_equal(safe_numeric("42"), 42)
})

test_that("safe_numeric converts decimals", {
  expect_equal(safe_numeric("123.5"), 123.5)
  expect_equal(safe_numeric("0.5"), 0.5)
  expect_equal(safe_numeric("99.99"), 99.99)
})

test_that("safe_numeric handles already-numeric input", {
  expect_equal(safe_numeric(42), 42)
  expect_equal(safe_numeric(3.14), 3.14)
})

test_that("safe_numeric handles whitespace", {
  expect_equal(safe_numeric("  500  "), 500)
})

test_that("safe_numeric handles vector input", {
  result <- safe_numeric(c("100", "200", "*", "300"))
  expect_equal(result[1], 100)
  expect_equal(result[2], 200)
  expect_true(is.na(result[3]))
  expect_equal(result[4], 300)
})

# ==============================================================================
# get_wvde_pdf_url — URL construction
# ==============================================================================

test_that("get_wvde_pdf_url returns string", {
  url <- get_wvde_pdf_url(2024, "FTE")
  expect_type(url, "character")
})

test_that("get_wvde_pdf_url 2024 FTE contains correct filename", {
  url <- get_wvde_pdf_url(2024, "FTE")
  expect_true(grepl("FTE-Enrollment-2nd-Mo-24", url))
})

test_that("get_wvde_pdf_url 2024 Headcount contains correct filename", {
  url <- get_wvde_pdf_url(2024, "Headcount")
  expect_true(grepl("Headcount-Enroll-2nd-Mo-24", url))
})

test_that("get_wvde_pdf_url 2023 FTE contains correct filename", {
  url <- get_wvde_pdf_url(2023, "FTE")
  expect_true(grepl("FTE-Enrollment-2nd-Mo-23", url))
})

test_that("get_wvde_pdf_url domain is wvde.us", {
  url <- get_wvde_pdf_url(2024, "FTE")
  expect_true(grepl("wvde\\.us", url))
})

test_that("get_wvde_pdf_url all available years return valid URLs", {
  years <- get_available_years()
  for (yr in years) {
    fte_url <- get_wvde_pdf_url(yr, "FTE")
    hc_url <- get_wvde_pdf_url(yr, "Headcount")
    expect_true(grepl("wvde\\.us", fte_url), info = paste("Bad FTE URL for", yr))
    expect_true(grepl("wvde\\.us", hc_url), info = paste("Bad HC URL for", yr))
    expect_true(grepl("\\.pdf$", fte_url), info = paste("FTE URL not PDF for", yr))
    expect_true(grepl("\\.pdf$", hc_url), info = paste("HC URL not PDF for", yr))
  }
})

# ==============================================================================
# Cache functions
# ==============================================================================

test_that("get_cache_dir returns a path string", {
  dir_path <- get_cache_dir()
  expect_type(dir_path, "character")
  expect_true(grepl("wvschooldata", dir_path))
})

test_that("get_cache_path returns correct filename pattern", {
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024\\.rds$", path))
})

test_that("get_cache_path for wide format", {
  path <- get_cache_path(2024, "wide")
  expect_true(grepl("enr_wide_2024\\.rds$", path))
})

test_that("get_cache_path for different years", {
  path_2023 <- get_cache_path(2023, "tidy")
  path_2024 <- get_cache_path(2024, "tidy")
  expect_true(grepl("2023", path_2023))
  expect_true(grepl("2024", path_2024))
  expect_false(path_2023 == path_2024)
})

test_that("cache_status returns a data.frame", {
  result <- cache_status()
  expect_true(is.data.frame(result))
})

test_that("clear_cache is a function", {
  expect_true(is.function(clear_cache))
})

test_that("clear_cache accepts year parameter", {
  # Just test that it doesn't error — don't actually clear
  # by calling with a year that probably has no cache
  result <- clear_cache(end_year = 9999)
  expect_true(is.numeric(result) || is.integer(result))
})

test_that("clear_cache accepts type parameter", {
  result <- clear_cache(type = "nonexistent_type")
  expect_true(is.numeric(result) || is.integer(result))
})

# ==============================================================================
# Aggregation logic: state total = sum of districts
# ==============================================================================

test_that("state total equals sum of district totals (2024)", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_total <- enr %>%
    filter(is_state, grade_level == "TOTAL") %>%
    pull(n_students)
  district_sum <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(n_students) %>%
    sum()
  expect_equal(state_total, district_sum)
})

test_that("state total equals sum of district totals (2023)", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  state_total <- enr %>%
    filter(is_state, grade_level == "TOTAL") %>%
    pull(n_students)
  district_sum <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(n_students) %>%
    sum()
  expect_equal(state_total, district_sum)
})

test_that("state grade-level totals equal district sums for each grade (2024)", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grades <- c("PK", "K", "01", "02", "03", "04", "05", "06",
              "07", "08", "09", "10", "11", "12")

  for (g in grades) {
    state_val <- enr %>%
      filter(is_state, grade_level == g) %>%
      pull(n_students)
    district_sum <- enr %>%
      filter(is_district, grade_level == g) %>%
      pull(n_students) %>%
      sum()
    expect_equal(state_val, district_sum,
                 tolerance = 0.01,
                 info = paste("Mismatch for grade", g))
  }
})

# ==============================================================================
# One observation per group per period
# ==============================================================================

test_that("no duplicate district-grade combinations for 2024", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dupes <- enr %>%
    filter(is_district) %>%
    count(district_id, grade_level) %>%
    filter(n > 1)
  expect_equal(nrow(dupes), 0L)
})

test_that("no duplicate district-grade combinations for 2023", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  dupes <- enr %>%
    filter(is_district) %>%
    count(district_id, grade_level) %>%
    filter(n > 1)
  expect_equal(nrow(dupes), 0L)
})

test_that("exactly one state row per grade level for 2024", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_dupes <- enr %>%
    filter(is_state) %>%
    count(grade_level) %>%
    filter(n > 1)
  expect_equal(nrow(state_dupes), 0L)
})

test_that("no duplicate district-year-grade in multi-year fetch", {
  multi <- fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE)
  dupes <- multi %>%
    filter(is_district) %>%
    count(end_year, district_id, grade_level) %>%
    filter(n > 1)
  expect_equal(nrow(dupes), 0L)
})

# ==============================================================================
# Wide format structure invariants
# ==============================================================================

test_that("wide format: end_year column is numeric", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(is.numeric(wide$end_year))
})

test_that("wide format: all end_year values are 2024", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(all(wide$end_year == 2024))
})

test_that("wide format: grade columns are numeric", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  grade_cols <- c("grade_pk", "grade_k",
                  paste0("grade_", sprintf("%02d", 1:12)),
                  "row_total")
  for (col in grade_cols) {
    if (col %in% names(wide)) {
      expect_true(is.numeric(wide[[col]]),
                  info = paste(col, "should be numeric"))
    }
  }
})

test_that("wide format: type column has State and District", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_setequal(unique(wide$type), c("State", "District"))
})

test_that("wide format: campus_id is all NA", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(all(is.na(wide$campus_id)))
})

test_that("wide format: campus_name is all NA", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(all(is.na(wide$campus_name)))
})

test_that("wide format: charter_flag is all NA", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(all(is.na(wide$charter_flag)))
})

# ==============================================================================
# Tidy format structure invariants
# ==============================================================================

test_that("tidy format: campus_id is all NA", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(is.na(enr$campus_id)))
})

test_that("tidy format: campus_name is all NA", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(is.na(enr$campus_name)))
})

test_that("tidy format: charter_flag is all NA", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(is.na(enr$charter_flag)))
})

test_that("tidy format: state rows have NA county", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_rows <- enr %>% filter(is_state)
  expect_true(all(is.na(state_rows$county)))
})

test_that("tidy format: state rows have NA district_name", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_rows <- enr %>% filter(is_state)
  expect_true(all(is.na(state_rows$district_name)))
})

test_that("tidy format: district rows have non-NA county", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_rows <- enr %>% filter(is_district)
  expect_true(all(!is.na(dist_rows$county)))
})

test_that("tidy format: district rows have non-NA district_name", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_rows <- enr %>% filter(is_district)
  expect_true(all(!is.na(dist_rows$district_name)))
})

# ==============================================================================
# Cross-year consistency
# ==============================================================================

test_that("2023 and 2024 have same number of districts", {
  enr_2023 <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  enr_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  n_2023 <- length(unique(enr_2023$district_id[enr_2023$is_district]))
  n_2024 <- length(unique(enr_2024$district_id[enr_2024$is_district]))
  expect_equal(n_2023, n_2024)
})

test_that("2023 and 2024 have same district IDs", {
  enr_2023 <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  enr_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  ids_2023 <- sort(unique(unname(enr_2023$district_id[enr_2023$is_district])))
  ids_2024 <- sort(unique(unname(enr_2024$district_id[enr_2024$is_district])))
  expect_equal(ids_2023, ids_2024)
})

test_that("2023 and 2024 have same grade levels", {
  enr_2023 <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  enr_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  grades_2023 <- sort(unique(enr_2023$grade_level))
  grades_2024 <- sort(unique(enr_2024$grade_level))
  expect_equal(grades_2023, grades_2024)
})

test_that("2023 and 2024 have same column names", {
  enr_2023 <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  enr_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(names(enr_2023), names(enr_2024))
})

# ==============================================================================
# Enrollment magnitude sanity checks
# ==============================================================================

test_that("WV state enrollment is between 200K and 300K", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  total <- enr %>% filter(is_state, grade_level == "TOTAL") %>% pull(n_students)
  expect_gt(total, 200000)
  expect_lt(total, 300000)
})

test_that("largest county has > 15000 students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  max_dist <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(n_students) %>%
    max()
  expect_gt(max_dist, 15000)
})

test_that("smallest county has > 0 students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  min_dist <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(n_students) %>%
    min()
  expect_gt(min_dist, 0)
})

test_that("smallest county has < 2000 students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  min_dist <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(n_students) %>%
    min()
  expect_lt(min_dist, 2000)
})

test_that("2023 state total is greater than 2024 (declining enrollment)", {
  enr_2023 <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  enr_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  total_2023 <- enr_2023 %>% filter(is_state, grade_level == "TOTAL") %>% pull(n_students)
  total_2024 <- enr_2024 %>% filter(is_state, grade_level == "TOTAL") %>% pull(n_students)
  expect_gt(total_2023, total_2024)
})

# ==============================================================================
# Edge cases
# ==============================================================================

test_that("fetch_enr_multi with single year returns same as fetch_enr", {
  single <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  multi <- fetch_enr_multi(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(nrow(single), nrow(multi))
  expect_equal(names(single), names(multi))
})

test_that("enr_grade_aggs only uses total_enrollment subgroup", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  expect_true(all(aggs$subgroup == "total_enrollment"))
})

test_that("enr_grade_aggs preserves boolean flags", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  expect_true("is_state" %in% names(aggs))
  expect_true("is_district" %in% names(aggs))
})

test_that("WV has exactly 55 counties in enrollment data", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  counties <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    pull(county) %>%
    unique()
  expect_equal(length(counties), 55L)
})

test_that("county names in enrollment match get_wv_counties()", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  enr_counties <- sort(unique(enr$county[enr$is_district]))
  wv_counties <- sort(get_wv_counties())
  expect_equal(enr_counties, wv_counties)
})
