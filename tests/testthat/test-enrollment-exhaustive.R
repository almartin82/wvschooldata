# ==============================================================================
# Exhaustive Tests for wvschooldata enrollment functions
# ==============================================================================
#
# Tests EVERY exported enrollment function with ALL parameter combinations.
# Expected values are pinned from real WVDE data (use_cache = TRUE).
#
# ==============================================================================

library(dplyr)

# ==============================================================================
# get_available_years()
# ==============================================================================

test_that("get_available_years returns integer vector", {
  years <- get_available_years()
  expect_type(years, "integer")
})

test_that("get_available_years returns exactly 2023 and 2024", {
  years <- get_available_years()
  expect_equal(years, c(2023L, 2024L))
})

test_that("get_available_years has length 2", {
  years <- get_available_years()
  expect_equal(length(years), 2L)
})

test_that("get_available_years min is 2023", {
  expect_equal(min(get_available_years()), 2023L)
})

test_that("get_available_years max is 2024", {

  expect_equal(max(get_available_years()), 2024L)
})

# ==============================================================================
# fetch_enr() — year validation
# ==============================================================================

test_that("fetch_enr rejects year below range", {
  expect_error(fetch_enr(2010, use_cache = TRUE), "end_year must be between")
})

test_that("fetch_enr rejects year above range", {
  expect_error(fetch_enr(2030, use_cache = TRUE), "end_year must be between")
})

test_that("fetch_enr rejects year 2022 (not available)", {
  expect_error(fetch_enr(2022, use_cache = TRUE), "end_year must be between")
})

test_that("fetch_enr rejects non-numeric year", {
  expect_error(fetch_enr("abc", use_cache = TRUE))
})

# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — structure and pinned values
# ==============================================================================

test_that("fetch_enr 2024 tidy returns data.frame", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(is.data.frame(enr))
})

test_that("fetch_enr 2024 tidy has 840 rows", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(nrow(enr), 840L)
})

test_that("fetch_enr 2024 tidy has 17 columns", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(ncol(enr), 17L)
})

test_that("fetch_enr 2024 tidy has all expected column names", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expected_cols <- c(
    "end_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "county", "charter_flag",
    "grade_level", "subgroup", "n_students", "pct",
    "aggregation_flag", "is_state", "is_district", "is_campus", "is_charter"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(enr), info = paste("Missing column:", col))
  }
})

test_that("fetch_enr 2024 tidy only has total_enrollment subgroup", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(unique(enr$subgroup), "total_enrollment")
})

test_that("fetch_enr 2024 tidy has 15 unique grade levels", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(length(unique(enr$grade_level)), 15L)
})

test_that("fetch_enr 2024 tidy grade levels are correct set", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expected_grades <- c(
    "01", "02", "03", "04", "05", "06", "07", "08",
    "09", "10", "11", "12", "K", "PK", "TOTAL"
  )
  expect_setequal(unique(enr$grade_level), expected_grades)
})

test_that("fetch_enr 2024 tidy has exactly 2 types: State and District", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_setequal(unique(enr$type), c("State", "District"))
})

test_that("fetch_enr 2024 tidy: 15 State rows", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(sum(enr$type == "State"), 15L)
})

test_that("fetch_enr 2024 tidy: 825 District rows", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(sum(enr$type == "District"), 825L)
})

test_that("fetch_enr 2024 tidy: 55 districts", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  n_districts <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    nrow()
  expect_equal(n_districts, 55L)
})

test_that("fetch_enr 2024 tidy: 56 rows per grade level", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grade_counts <- enr %>% count(grade_level)
  expect_true(all(grade_counts$n == 56L))
})

# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — pinned state totals
# ==============================================================================

test_that("fetch_enr 2024: state TOTAL enrollment = 242777", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_total <- enr %>%
    filter(is_state, grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(state_total, 242777)
})

test_that("fetch_enr 2024: state PK enrollment = 13090.01", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_pk <- enr %>%
    filter(is_state, grade_level == "PK") %>%
    pull(n_students)
  expect_equal(state_pk, 13090.01, tolerance = 0.01)
})

test_that("fetch_enr 2024: state K enrollment = 16473.84", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_k <- enr %>%
    filter(is_state, grade_level == "K") %>%
    pull(n_students)
  expect_equal(state_k, 16473.84, tolerance = 0.01)
})

test_that("fetch_enr 2024: state grade 01 enrollment = 16764.8", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(is_state, grade_level == "01") %>%
    pull(n_students)
  expect_equal(val, 16764.8, tolerance = 0.1)
})

test_that("fetch_enr 2024: state grade 09 enrollment = 20379.53", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(is_state, grade_level == "09") %>%
    pull(n_students)
  expect_equal(val, 20379.53, tolerance = 0.01)
})

test_that("fetch_enr 2024: state grade 12 enrollment = 16400.89", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(is_state, grade_level == "12") %>%
    pull(n_students)
  expect_equal(val, 16400.89, tolerance = 0.01)
})

# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — pinned district values
# ==============================================================================

test_that("fetch_enr 2024: Kanawha County total = 23437", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(district_name == "KANAWHA COUNTY SCHOOLS", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 23437)
})

test_that("fetch_enr 2024: Kanawha district_id = 54039", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(district_name == "KANAWHA COUNTY SCHOOLS", grade_level == "TOTAL") %>%
    pull(district_id)
  expect_equal(unname(val), "54039")
})

test_that("fetch_enr 2024: Berkeley County total = 19871", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(district_name == "BERKELEY COUNTY SCHOOLS", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 19871)
})

test_that("fetch_enr 2024: Berkeley district_id = 54003", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(district_name == "BERKELEY COUNTY SCHOOLS", grade_level == "TOTAL") %>%
    pull(district_id)
  expect_equal(unname(val), "54003")
})

test_that("fetch_enr 2024: Barbour County total = 2079", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "BARBOUR", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 2079)
})

test_that("fetch_enr 2024: Wirt County total = 899", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "WIRT", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 899)
})

test_that("fetch_enr 2024: Wirt district_id = 54105", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "WIRT", grade_level == "TOTAL") %>%
    pull(district_id)
  expect_equal(unname(val), "54105")
})

test_that("fetch_enr 2024: Cabell County total = 11436", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "CABELL", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 11436)
})

test_that("fetch_enr 2024: Monongalia County total = 11201", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "MONONGALIA", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 11201)
})

test_that("fetch_enr 2024: Wood County total = 11330", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "WOOD", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 11330)
})

test_that("fetch_enr 2024: Kanawha is the largest district", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  largest <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    arrange(desc(n_students)) %>%
    head(1)
  expect_equal(largest$county, "KANAWHA")
})

# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — pinned district grade-level values
# ==============================================================================

test_that("fetch_enr 2024: Barbour grade K = 161", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "BARBOUR", grade_level == "K") %>%
    pull(n_students)
  expect_equal(val, 161)
})

test_that("fetch_enr 2024: Barbour grade PK = 132.55", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "BARBOUR", grade_level == "PK") %>%
    pull(n_students)
  expect_equal(val, 132.55, tolerance = 0.01)
})

test_that("fetch_enr 2024: Barbour grade 01 = 165", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(county == "BARBOUR", grade_level == "01") %>%
    pull(n_students)
  expect_equal(val, 165)
})

test_that("fetch_enr 2024: Kanawha PK = 1337.04", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(district_name == "KANAWHA COUNTY SCHOOLS", grade_level == "PK") %>%
    pull(n_students)
  expect_equal(val, 1337.04, tolerance = 0.01)
})

# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — boolean flags
# ==============================================================================

test_that("fetch_enr 2024: is_state flag is logical", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(enr$is_state, "logical")
})

test_that("fetch_enr 2024: is_district flag is logical", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(enr$is_district, "logical")
})

test_that("fetch_enr 2024: is_campus flag is logical", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(enr$is_campus, "logical")
})

test_that("fetch_enr 2024: is_charter flag is logical", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(enr$is_charter, "logical")
})

test_that("fetch_enr 2024: no campus-level rows (all is_campus FALSE)", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(!enr$is_campus))
})

test_that("fetch_enr 2024: no charter rows (all is_charter FALSE)", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(!enr$is_charter))
})

test_that("fetch_enr 2024: is_state and is_district are mutually exclusive", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(!(enr$is_state & enr$is_district)))
})

test_that("fetch_enr 2024: every row is either state or district", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(enr$is_state | enr$is_district))
})

# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — aggregation_flag column
# ==============================================================================

test_that("fetch_enr 2024: aggregation_flag has expected values", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_setequal(unique(enr$aggregation_flag), c("state", "district"))
})

test_that("fetch_enr 2024: 15 state aggregation_flag rows", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(sum(enr$aggregation_flag == "state"), 15L)
})

test_that("fetch_enr 2024: 825 district aggregation_flag rows", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(sum(enr$aggregation_flag == "district"), 825L)
})

# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — pct column
# ==============================================================================

test_that("fetch_enr 2024: TOTAL grade pct is always 1", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  totals <- enr %>% filter(grade_level == "TOTAL")
  expect_true(all(totals$pct == 1.0))
})

test_that("fetch_enr 2024: non-TOTAL grade pct values are between 0 and 1", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  non_totals <- enr %>% filter(grade_level != "TOTAL")
  expect_true(all(non_totals$pct >= 0 & non_totals$pct <= 1, na.rm = TRUE))
})

test_that("fetch_enr 2024: Kanawha PK pct is correct", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(district_name == "KANAWHA COUNTY SCHOOLS", grade_level == "PK") %>%
    pull(pct)
  expect_equal(val, 0.05704826, tolerance = 1e-4)
})

# ==============================================================================
# fetch_enr(2024, tidy = FALSE) — wide format
# ==============================================================================

test_that("fetch_enr 2024 wide has 56 rows (1 state + 55 districts)", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_equal(nrow(wide), 56L)
})

test_that("fetch_enr 2024 wide has 23 columns", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_equal(ncol(wide), 23L)
})

test_that("fetch_enr 2024 wide has expected column names", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expected_cols <- c(
    "end_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "county", "charter_flag",
    "row_total",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(wide), info = paste("Missing column:", col))
  }
})

test_that("fetch_enr 2024 wide: state row_total = 242777", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state_row <- wide %>% filter(type == "State")
  expect_equal(state_row$row_total, 242777)
})

test_that("fetch_enr 2024 wide: state grade_pk = 13090.01", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state_row <- wide %>% filter(type == "State")
  expect_equal(state_row$grade_pk, 13090.01, tolerance = 0.01)
})

test_that("fetch_enr 2024 wide: state grade_k = 16473.84", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state_row <- wide %>% filter(type == "State")
  expect_equal(state_row$grade_k, 16473.84, tolerance = 0.01)
})

test_that("fetch_enr 2024 wide: state grade_01 = 16764.8", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state_row <- wide %>% filter(type == "State")
  expect_equal(state_row$grade_01, 16764.8, tolerance = 0.1)
})

test_that("fetch_enr 2024 wide: state grade_12 = 16400.89", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state_row <- wide %>% filter(type == "State")
  expect_equal(state_row$grade_12, 16400.89, tolerance = 0.01)
})

test_that("fetch_enr 2024 wide: Kanawha row_total = 23437", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  kanawha <- wide %>% filter(district_name == "KANAWHA COUNTY SCHOOLS")
  expect_equal(kanawha$row_total, 23437)
})

test_that("fetch_enr 2024 wide: Barbour row_total = 2079", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  barbour <- wide %>% filter(county == "BARBOUR")
  expect_equal(barbour$row_total, 2079)
})

test_that("fetch_enr 2024 wide: Brooke row_total = 2336", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  brooke <- wide %>% filter(county == "BROOKE")
  expect_equal(brooke$row_total, 2336)
})

test_that("fetch_enr 2024 wide: Boone row_total = 3100", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  boone <- wide %>% filter(county == "BOONE")
  expect_equal(boone$row_total, 3100)
})

# ==============================================================================
# fetch_enr(2024) — wide/tidy fidelity
# ==============================================================================

test_that("fetch_enr 2024: wide totals match tidy totals for all 55 districts", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_totals <- wide %>%
    filter(type == "District") %>%
    select(district_id, n_wide = row_total)
  tidy_totals <- tidy %>%
    filter(is_district, grade_level == "TOTAL") %>%
    select(district_id, n_tidy = n_students)

  comparison <- inner_join(wide_totals, tidy_totals, by = "district_id")
  expect_equal(nrow(comparison), 55L)
  expect_true(all(comparison$n_wide == comparison$n_tidy))
})

test_that("fetch_enr 2024: wide state total matches tidy state total", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_state <- wide %>% filter(type == "State") %>% pull(row_total)
  tidy_state <- tidy %>% filter(is_state, grade_level == "TOTAL") %>% pull(n_students)
  expect_equal(wide_state, tidy_state)
})

# ==============================================================================
# fetch_enr(2023, tidy = TRUE) — pinned values
# ==============================================================================

test_that("fetch_enr 2023 tidy has 840 rows", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  expect_equal(nrow(enr), 840L)
})

test_that("fetch_enr 2023: state TOTAL = 248801", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(is_state, grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 248801)
})

test_that("fetch_enr 2023: 55 districts", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  n_dist <- enr %>%
    filter(is_district, grade_level == "TOTAL") %>%
    nrow()
  expect_equal(n_dist, 55L)
})

test_that("fetch_enr 2023: Berkeley County total = 19855", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(district_name == "BERKELEY COUNTY SCHOOLS", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 19855)
})

test_that("fetch_enr 2023: Monongalia County total = 11307", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(district_name == "MONONGALIA COUNTY SCHOOLS", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(val, 11307)
})

test_that("fetch_enr 2023: state PK = 13220.55", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(is_state, grade_level == "PK") %>%
    pull(n_students)
  expect_equal(val, 13220.55, tolerance = 0.01)
})

test_that("fetch_enr 2023: state K = 17248.03", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(is_state, grade_level == "K") %>%
    pull(n_students)
  expect_equal(val, 17248.03, tolerance = 0.01)
})

test_that("fetch_enr 2023: state grade 09 = 21411.2", {
  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  val <- enr %>%
    filter(is_state, grade_level == "09") %>%
    pull(n_students)
  expect_equal(val, 21411.2, tolerance = 0.1)
})

# ==============================================================================
# fetch_enr_multi() — validation
# ==============================================================================

test_that("fetch_enr_multi rejects invalid years", {
  expect_error(fetch_enr_multi(c(2010, 2020), use_cache = TRUE), "Invalid years")
})

test_that("fetch_enr_multi rejects mixed valid/invalid years", {
  expect_error(fetch_enr_multi(c(2024, 2030), use_cache = TRUE), "Invalid years")
})

# ==============================================================================
# fetch_enr_multi(2023:2024) — structure and values
# ==============================================================================

test_that("fetch_enr_multi 2023:2024 returns 1680 rows", {
  multi <- fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(nrow(multi), 1680L)
})

test_that("fetch_enr_multi 2023:2024 has both years", {
  multi <- fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE)
  expect_setequal(unique(multi$end_year), c(2023, 2024))
})

test_that("fetch_enr_multi 2023:2024: 840 rows per year", {
  multi <- fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE)
  year_counts <- multi %>% count(end_year)
  expect_true(all(year_counts$n == 840))
})

test_that("fetch_enr_multi 2023:2024: state totals match individual fetches", {
  multi <- fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE)
  single_2023 <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  single_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  multi_2023_total <- multi %>%
    filter(end_year == 2023, is_state, grade_level == "TOTAL") %>%
    pull(n_students)
  multi_2024_total <- multi %>%
    filter(end_year == 2024, is_state, grade_level == "TOTAL") %>%
    pull(n_students)
  single_2023_total <- single_2023 %>%
    filter(is_state, grade_level == "TOTAL") %>%
    pull(n_students)
  single_2024_total <- single_2024 %>%
    filter(is_state, grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(multi_2023_total, single_2023_total)
  expect_equal(multi_2024_total, single_2024_total)
})

test_that("fetch_enr_multi single year works", {
  single <- fetch_enr_multi(2024, tidy = TRUE, use_cache = TRUE)
  expect_equal(nrow(single), 840L)
})

test_that("fetch_enr_multi wide format works", {
  multi_wide <- fetch_enr_multi(2023:2024, tidy = FALSE, use_cache = TRUE)
  expect_equal(nrow(multi_wide), 112L)  # 56 * 2
  expect_true("row_total" %in% names(multi_wide))
})

# ==============================================================================
# tidy_enr() — direct function tests
# ==============================================================================

test_that("tidy_enr transforms wide to long format", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  expect_true(is.data.frame(tidy))
  expect_equal(nrow(tidy), 840L)
})

test_that("tidy_enr output has required columns", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  expected <- c("end_year", "type", "grade_level", "subgroup", "n_students", "pct", "aggregation_flag")
  for (col in expected) {
    expect_true(col %in% names(tidy), info = paste("Missing:", col))
  }
})

test_that("tidy_enr does not have aggregation boolean flags", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  expect_false("is_state" %in% names(tidy))
})

test_that("tidy_enr preserves totals from wide format", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  state_wide <- wide %>% filter(type == "State") %>% pull(row_total)
  state_tidy <- tidy %>% filter(aggregation_flag == "state", grade_level == "TOTAL") %>% pull(n_students)
  expect_equal(state_wide, state_tidy)
})

# ==============================================================================
# id_enr_aggs() — direct function tests
# ==============================================================================

test_that("id_enr_aggs adds boolean flags", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  with_aggs <- id_enr_aggs(tidy)

  expect_true("is_state" %in% names(with_aggs))
  expect_true("is_district" %in% names(with_aggs))
  expect_true("is_campus" %in% names(with_aggs))
  expect_true("is_charter" %in% names(with_aggs))
})

test_that("id_enr_aggs: State type maps to is_state = TRUE", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  with_aggs <- id_enr_aggs(tidy)

  state_rows <- with_aggs %>% filter(type == "State")
  expect_true(all(state_rows$is_state))
  expect_true(all(!state_rows$is_district))
})

test_that("id_enr_aggs: District type maps to is_district = TRUE", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  with_aggs <- id_enr_aggs(tidy)

  district_rows <- with_aggs %>% filter(type == "District")
  expect_true(all(!district_rows$is_state))
  expect_true(all(district_rows$is_district))
})

test_that("id_enr_aggs: 15 state + 825 district rows", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- tidy_enr(wide)
  with_aggs <- id_enr_aggs(tidy)

  expect_equal(sum(with_aggs$is_state), 15L)
  expect_equal(sum(with_aggs$is_district), 825L)
})

# ==============================================================================
# enr_grade_aggs() — grade aggregation tests
# ==============================================================================

test_that("enr_grade_aggs returns K8, HS, K12", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  expect_setequal(unique(aggs$grade_level), c("K8", "HS", "K12"))
})

test_that("enr_grade_aggs has 168 rows", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  expect_equal(nrow(aggs), 168L)
})

test_that("enr_grade_aggs: state K8 = 154381.3", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  val <- aggs %>%
    filter(is_state, grade_level == "K8") %>%
    pull(n_students)
  expect_equal(val, 154381.3, tolerance = 0.1)
})

test_that("enr_grade_aggs: state HS = 74111.98", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  val <- aggs %>%
    filter(is_state, grade_level == "HS") %>%
    pull(n_students)
  expect_equal(val, 74111.98, tolerance = 0.01)
})

test_that("enr_grade_aggs: state K12 = 228493.3", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  val <- aggs %>%
    filter(is_state, grade_level == "K12") %>%
    pull(n_students)
  expect_equal(val, 228493.3, tolerance = 0.1)
})

test_that("enr_grade_aggs: K8 + HS = K12", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs %>% filter(is_state)
  k8 <- state_aggs %>% filter(grade_level == "K8") %>% pull(n_students)
  hs <- state_aggs %>% filter(grade_level == "HS") %>% pull(n_students)
  k12 <- state_aggs %>% filter(grade_level == "K12") %>% pull(n_students)
  expect_equal(k8 + hs, k12)
})

test_that("enr_grade_aggs: Kanawha K8 = 14565.29", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  val <- aggs %>%
    filter(district_name == "KANAWHA COUNTY SCHOOLS", grade_level == "K8") %>%
    pull(n_students)
  expect_equal(val, 14565.29, tolerance = 0.01)
})

test_that("enr_grade_aggs: Kanawha HS = 7317.37", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  val <- aggs %>%
    filter(district_name == "KANAWHA COUNTY SCHOOLS", grade_level == "HS") %>%
    pull(n_students)
  expect_equal(val, 7317.37, tolerance = 0.01)
})

test_that("enr_grade_aggs: Kanawha K12 = K8 + HS", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  kan <- aggs %>% filter(district_name == "KANAWHA COUNTY SCHOOLS")
  k8 <- kan %>% filter(grade_level == "K8") %>% pull(n_students)
  hs <- kan %>% filter(grade_level == "HS") %>% pull(n_students)
  k12 <- kan %>% filter(grade_level == "K12") %>% pull(n_students)
  expect_equal(k8 + hs, k12)
})

test_that("enr_grade_aggs: pct is NA_real_ for aggregates", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  expect_true(all(is.na(aggs$pct)))
})

test_that("enr_grade_aggs: 56 rows per grade level (1 state + 55 districts)", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  grade_counts <- aggs %>% count(grade_level)
  expect_true(all(grade_counts$n == 56L))
})

# ==============================================================================
# Data quality — no Inf, NaN, negative values
# ==============================================================================

test_that("fetch_enr 2024 tidy: no Inf values in n_students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.infinite(enr$n_students), na.rm = TRUE))
})

test_that("fetch_enr 2024 tidy: no NaN values in n_students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.nan(enr$n_students), na.rm = TRUE))
})

test_that("fetch_enr 2024 tidy: no negative n_students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(enr$n_students >= 0, na.rm = TRUE))
})

test_that("fetch_enr 2024 tidy: no NA in n_students", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.na(enr$n_students)))
})

test_that("fetch_enr 2024 tidy: no Inf in pct", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.infinite(enr$pct), na.rm = TRUE))
})

test_that("fetch_enr 2024 wide: no Inf in any numeric column", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  for (col in names(wide)[sapply(wide, is.numeric)]) {
    expect_false(any(is.infinite(wide[[col]]), na.rm = TRUE),
                 info = paste("Inf in", col))
  }
})

test_that("fetch_enr 2024 wide: no NaN in any numeric column", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  for (col in names(wide)[sapply(wide, is.numeric)]) {
    expect_false(any(is.nan(wide[[col]]), na.rm = TRUE),
                 info = paste("NaN in", col))
  }
})

test_that("fetch_enr 2024 wide: no negative row_total", {
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(all(wide$row_total >= 0, na.rm = TRUE))
})

# ==============================================================================
# Column types
# ==============================================================================

test_that("fetch_enr 2024 tidy: end_year is numeric", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(is.numeric(enr$end_year))
})

test_that("fetch_enr 2024 tidy: type is character", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(enr$type, "character")
})

test_that("fetch_enr 2024 tidy: district_id is character", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(enr$district_id, "character")
})

test_that("fetch_enr 2024 tidy: n_students is numeric", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(is.numeric(enr$n_students))
})

test_that("fetch_enr 2024 tidy: subgroup is character", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(enr$subgroup, "character")
})

test_that("fetch_enr 2024 tidy: grade_level is character", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(enr$grade_level, "character")
})

test_that("fetch_enr 2024 tidy: pct is numeric", {
  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(is.numeric(enr$pct))
})
