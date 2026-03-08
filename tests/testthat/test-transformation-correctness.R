# ==============================================================================
# Transformation Correctness Tests for wvschooldata
# ==============================================================================
#
# These tests verify that the data transformation pipeline produces correct,
# consistent output. Every expected value comes from real fetch_enr() output
# traced back to WVDE School Finance Data PDFs.
#
# Test categories:
#   1. Suppression handling (safe_numeric)
#   2. District IDs (FIPS-based)
#   3. Grade levels (standard codes)
#   4. Subgroups (naming standards)
#   5. Pivot fidelity (wide <-> tidy)
#   6. Percentages (pct column)
#   7. Aggregation (state = sum of districts)
#   8. Entity flags (is_state, is_district, is_campus, is_charter)
#   9. Per-year known values
#  10. Cross-year consistency
#
# ==============================================================================

library(testthat)

# Load tidy and wide data for both available years
tidy_2024 <- wvschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
wide_2024 <- wvschooldata::fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
tidy_2023 <- wvschooldata::fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
wide_2023 <- wvschooldata::fetch_enr(2023, tidy = FALSE, use_cache = TRUE)

# ==============================================================================
# 1. Suppression Handling
# ==============================================================================

test_that("safe_numeric converts suppression markers to NA", {
  sn <- wvschooldata:::safe_numeric

  # All standard suppression markers return NA

  expect_true(is.na(sn("*")))
  expect_true(is.na(sn(".")))
  expect_true(is.na(sn("-")))
  expect_true(is.na(sn("-1")))
  expect_true(is.na(sn("-2")))
  expect_true(is.na(sn("-9")))
  expect_true(is.na(sn("<5")))
  expect_true(is.na(sn("N/A")))
  expect_true(is.na(sn("NA")))
  expect_true(is.na(sn("--")))
  expect_true(is.na(sn("PS")))
  expect_true(is.na(sn("")))
})

test_that("safe_numeric converts valid numbers correctly", {
  sn <- wvschooldata:::safe_numeric

  expect_equal(sn("1,234"), 1234)
  expect_equal(sn("500"), 500)
  expect_equal(sn("123.5"), 123.5)
  expect_equal(sn("0"), 0)
})

test_that("safe_numeric passes through numeric input unchanged", {
  sn <- wvschooldata:::safe_numeric

  expect_equal(sn(42), 42)
  expect_equal(sn(3.14), 3.14)
})

test_that("no Inf or NaN in tidy enrollment data", {
  numeric_cols <- names(tidy_2024)[sapply(tidy_2024, is.numeric)]

  for (col in numeric_cols) {
    expect_false(
      any(is.infinite(tidy_2024[[col]]), na.rm = TRUE),
      info = paste("No Inf values in", col)
    )
    expect_false(
      any(is.nan(tidy_2024[[col]]), na.rm = TRUE),
      info = paste("No NaN values in", col)
    )
  }
})

test_that("no Inf or NaN in wide enrollment data", {
  numeric_cols <- names(wide_2024)[sapply(wide_2024, is.numeric)]

  for (col in numeric_cols) {
    expect_false(
      any(is.infinite(wide_2024[[col]]), na.rm = TRUE),
      info = paste("No Inf values in", col)
    )
    expect_false(
      any(is.nan(wide_2024[[col]]), na.rm = TRUE),
      info = paste("No NaN values in", col)
    )
  }
})

# ==============================================================================
# 2. District IDs (FIPS-based)
# ==============================================================================

test_that("district IDs use FIPS format 54XXX", {
  district_ids <- tidy_2024$district_id[tidy_2024$type == "District"]
  district_ids <- unique(district_ids)

  # All district IDs start with state FIPS "54"
  expect_true(all(grepl("^54", district_ids)))

  # All district IDs are 5 characters long
  expect_true(all(nchar(district_ids) == 5))
})

test_that("exactly 55 unique district IDs for county school districts", {
  district_ids <- unique(tidy_2024$district_id[tidy_2024$type == "District"])
  expect_equal(length(district_ids), 55)
})

test_that("known district IDs are correct FIPS codes", {
  expect_equal(wvschooldata:::generate_district_id("KANAWHA"), "54039")
  expect_equal(wvschooldata:::generate_district_id("BERKELEY"), "54003")
  expect_equal(wvschooldata:::generate_district_id("BARBOUR"), "54001")
  expect_equal(wvschooldata:::generate_district_id("CABELL"), "54011")
  expect_equal(wvschooldata:::generate_district_id("MONONGALIA"), "54061")
  expect_equal(wvschooldata:::generate_district_id("WYOMING"), "54109")
})

test_that("invalid county names return NA district ID", {
  expect_true(is.na(wvschooldata:::generate_district_id("NOT_A_COUNTY")))
  expect_true(is.na(wvschooldata:::generate_district_id("FAKECOUNTY")))
})

test_that("state-level rows have NA district_id", {
  state_rows <- tidy_2024[tidy_2024$type == "State", ]
  expect_true(all(is.na(state_rows$district_id)))
})

# ==============================================================================
# 3. Grade Levels (standard codes)
# ==============================================================================

test_that("tidy data has all expected grade levels", {
  grade_levels <- sort(unique(tidy_2024$grade_level))
  expected <- sort(c("PK", "K", "01", "02", "03", "04", "05", "06",
                     "07", "08", "09", "10", "11", "12", "TOTAL"))
  expect_equal(grade_levels, expected)
})

test_that("grade levels are uppercase standard codes", {
  grades <- unique(tidy_2024$grade_level)

  # No lowercase

  expect_true(all(grades == toupper(grades)))

  # Numeric grades are zero-padded
  numeric_grades <- grades[grepl("^\\d+$", grades)]
  expect_true(all(nchar(numeric_grades) == 2))
})

test_that("wide data has expected grade columns", {
  grade_cols <- grep("^grade_", names(wide_2024), value = TRUE)
  expected_cols <- c("grade_pk", "grade_k",
                     paste0("grade_", sprintf("%02d", 1:12)))
  expect_equal(sort(grade_cols), sort(expected_cols))
})

test_that("each district has exactly 15 rows in tidy (PK-12 + TOTAL)", {
  district_counts <- tidy_2024 |>
    dplyr::filter(type == "District") |>
    dplyr::count(district_id) |>
    dplyr::pull(n)

  # 15 grade levels per district: PK, K, 01-12, TOTAL
  expect_true(all(district_counts == 15))
})

test_that("state row has exactly 15 rows in tidy (PK-12 + TOTAL)", {
  state_count <- sum(tidy_2024$type == "State")
  expect_equal(state_count, 15)
})

# ==============================================================================
# 4. Subgroups (naming standards)
# ==============================================================================

test_that("only subgroup is total_enrollment", {
  subgroups <- unique(tidy_2024$subgroup)
  expect_equal(subgroups, "total_enrollment")
})

test_that("subgroup is consistent across years", {
  expect_equal(unique(tidy_2023$subgroup), "total_enrollment")
  expect_equal(unique(tidy_2024$subgroup), "total_enrollment")
})

# ==============================================================================
# 5. Pivot Fidelity (wide <-> tidy)
# ==============================================================================

test_that("tidy TOTAL matches wide row_total for every district", {
  tidy_totals <- tidy_2024 |>
    dplyr::filter(type == "District", grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::select(district_id, tidy_total = n_students)

  wide_totals <- wide_2024 |>
    dplyr::filter(type == "District") |>
    dplyr::select(district_id, wide_total = row_total)

  check <- dplyr::inner_join(tidy_totals, wide_totals, by = "district_id") |>
    dplyr::mutate(diff = abs(tidy_total - wide_total))

  expect_equal(nrow(check), 55)
  expect_true(all(check$diff == 0))
})

test_that("tidy grade-level n_students matches wide grade columns", {
  # Check Kanawha K
  kanawha_k_tidy <- tidy_2024 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS",
                  grade_level == "K") |>
    dplyr::pull(n_students)

  kanawha_k_wide <- wide_2024 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS") |>
    dplyr::pull(grade_k)

  expect_equal(kanawha_k_tidy, kanawha_k_wide)

  # Check Kanawha 09
  kanawha_09_tidy <- tidy_2024 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS",
                  grade_level == "09") |>
    dplyr::pull(n_students)

  kanawha_09_wide <- wide_2024 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS") |>
    dplyr::pull(grade_09)

  expect_equal(kanawha_09_tidy, kanawha_09_wide)
})

test_that("tidy state TOTAL matches wide state row_total", {
  tidy_state <- tidy_2024 |>
    dplyr::filter(type == "State", grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  wide_state <- wide_2024 |>
    dplyr::filter(type == "State") |>
    dplyr::pull(row_total)

  expect_equal(tidy_state, wide_state)
})

test_that("no duplicate district-grade combinations in tidy data", {
  dups <- tidy_2024 |>
    dplyr::filter(type == "District") |>
    dplyr::count(district_id, grade_level, subgroup) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dups), 0)
})

test_that("tidy row count = wide rows * 15 grade levels", {
  n_wide_rows <- nrow(wide_2024)
  n_tidy_rows <- nrow(tidy_2024)

  # Each wide row produces 15 tidy rows (PK, K, 01-12, TOTAL)
  # But some grade-level rows may be filtered out if n_students is NA
  expect_equal(n_tidy_rows, n_wide_rows * 15)
})

# ==============================================================================
# 6. Percentages (pct column)
# ==============================================================================

test_that("pct = n_students / row_total for each grade level", {
  # For district-level grade rows, pct should equal n_students / TOTAL
  kanawha_rows <- tidy_2024 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS",
                  subgroup == "total_enrollment")

  kanawha_total <- kanawha_rows |>
    dplyr::filter(grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  kanawha_grades <- kanawha_rows |>
    dplyr::filter(grade_level != "TOTAL")

  for (i in seq_len(nrow(kanawha_grades))) {
    expected_pct <- kanawha_grades$n_students[i] / kanawha_total
    expect_equal(
      kanawha_grades$pct[i], expected_pct,
      tolerance = 1e-10,
      info = paste("Pct check for grade", kanawha_grades$grade_level[i])
    )
  }
})

test_that("TOTAL row always has pct = 1.0", {
  total_rows <- tidy_2024 |>
    dplyr::filter(grade_level == "TOTAL", subgroup == "total_enrollment")

  expect_true(all(total_rows$pct == 1.0))
})

test_that("pct values are between 0 and 1 for non-TOTAL grades", {
  grade_rows <- tidy_2024 |>
    dplyr::filter(grade_level != "TOTAL")

  valid_pct <- grade_rows$pct[!is.na(grade_rows$pct)]
  expect_true(all(valid_pct >= 0 & valid_pct <= 1))
})

test_that("sum of grade pct values is close to 1 but may differ due to FTE", {

  # FTE can differ from headcount total, so sum of grade pct may not be exactly 1
  state_grades <- tidy_2024 |>
    dplyr::filter(type == "State", grade_level != "TOTAL",
                  subgroup == "total_enrollment")

  pct_sum <- sum(state_grades$pct, na.rm = TRUE)
  # Should be close to 1 but allow for FTE vs headcount discrepancy
  expect_true(pct_sum > 0.9 && pct_sum <= 1.0,
              info = paste("Grade pct sum:", pct_sum))
})

# ==============================================================================
# 7. Aggregation (state = sum of districts)
# ==============================================================================

test_that("state row_total equals sum of district row_totals (wide)", {
  state_total <- wide_2024 |>
    dplyr::filter(type == "State") |>
    dplyr::pull(row_total)

  district_sum <- wide_2024 |>
    dplyr::filter(type == "District") |>
    dplyr::pull(row_total) |>
    sum(na.rm = TRUE)

  expect_equal(state_total, district_sum)
})

test_that("state grade columns equal sum of district grade columns", {
  grade_cols <- grep("^grade_", names(wide_2024), value = TRUE)

  state_row <- wide_2024 |> dplyr::filter(type == "State")
  district_rows <- wide_2024 |> dplyr::filter(type == "District")

  for (col in grade_cols) {
    state_val <- state_row[[col]]
    dist_sum <- sum(district_rows[[col]], na.rm = TRUE)
    expect_equal(
      state_val, dist_sum,
      tolerance = 0.01,
      info = paste("State", col, "equals sum of districts")
    )
  }
})

test_that("state tidy TOTAL equals sum of district tidy TOTALs", {
  state_n <- tidy_2024 |>
    dplyr::filter(type == "State", grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  dist_sum <- tidy_2024 |>
    dplyr::filter(type == "District", grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students) |>
    sum(na.rm = TRUE)

  expect_equal(state_n, dist_sum)
})

test_that("enr_grade_aggs K8 equals sum of K through 08", {
  grade_aggs <- wvschooldata::enr_grade_aggs(tidy_2024)

  k8_state <- grade_aggs |>
    dplyr::filter(is_state, grade_level == "K8") |>
    dplyr::pull(n_students)

  manual_k8 <- tidy_2024 |>
    dplyr::filter(is_state, subgroup == "total_enrollment",
                  grade_level %in% c("K", "01", "02", "03", "04",
                                     "05", "06", "07", "08")) |>
    dplyr::pull(n_students) |>
    sum(na.rm = TRUE)

  expect_equal(k8_state, manual_k8, tolerance = 0.01)
})

test_that("enr_grade_aggs HS equals sum of 09 through 12", {
  grade_aggs <- wvschooldata::enr_grade_aggs(tidy_2024)

  hs_state <- grade_aggs |>
    dplyr::filter(is_state, grade_level == "HS") |>
    dplyr::pull(n_students)

  manual_hs <- tidy_2024 |>
    dplyr::filter(is_state, subgroup == "total_enrollment",
                  grade_level %in% c("09", "10", "11", "12")) |>
    dplyr::pull(n_students) |>
    sum(na.rm = TRUE)

  expect_equal(hs_state, manual_hs, tolerance = 0.01)
})

test_that("enr_grade_aggs K12 equals K8 + HS", {
  grade_aggs <- wvschooldata::enr_grade_aggs(tidy_2024)

  k12 <- grade_aggs |>
    dplyr::filter(is_state, grade_level == "K12") |>
    dplyr::pull(n_students)

  k8 <- grade_aggs |>
    dplyr::filter(is_state, grade_level == "K8") |>
    dplyr::pull(n_students)

  hs <- grade_aggs |>
    dplyr::filter(is_state, grade_level == "HS") |>
    dplyr::pull(n_students)

  expect_equal(k12, k8 + hs, tolerance = 0.01)
})

test_that("enr_grade_aggs returns K8, HS, K12 grade levels", {
  grade_aggs <- wvschooldata::enr_grade_aggs(tidy_2024)
  expect_equal(sort(unique(grade_aggs$grade_level)), c("HS", "K12", "K8"))
})

test_that("enr_grade_aggs has correct number of rows", {
  grade_aggs <- wvschooldata::enr_grade_aggs(tidy_2024)

  # 56 entities (1 state + 55 districts) x 3 grade aggs = 168
  expect_equal(nrow(grade_aggs), 168)
})

# ==============================================================================
# 8. Entity Flags
# ==============================================================================

test_that("is_state is TRUE only for State type rows", {
  expect_true(all(tidy_2024$is_state == (tidy_2024$type == "State")))
})

test_that("is_district is TRUE only for District type rows", {
  expect_true(all(tidy_2024$is_district == (tidy_2024$type == "District")))
})

test_that("is_campus is FALSE for all rows (no campus data from WVDE PDFs)", {
  expect_true(all(tidy_2024$is_campus == FALSE))
})

test_that("is_charter is FALSE for all rows (no charter data from WVDE PDFs)", {
  expect_true(all(tidy_2024$is_charter == FALSE))
})

test_that("is_state and is_district are mutually exclusive", {
  expect_true(all(!(tidy_2024$is_state & tidy_2024$is_district)))
})

test_that("every row is either state or district", {
  expect_true(all(tidy_2024$is_state | tidy_2024$is_district))
})

test_that("aggregation_flag matches entity flags", {
  expect_true(all(tidy_2024$aggregation_flag[tidy_2024$is_state] == "state"))
  expect_true(all(tidy_2024$aggregation_flag[tidy_2024$is_district] == "district"))
})

test_that("state rows have 15 grade-level records", {
  n_state <- sum(tidy_2024$is_state)
  expect_equal(n_state, 15)
})

test_that("district rows total 825 (55 districts x 15 grades)", {
  n_district <- sum(tidy_2024$is_district)
  expect_equal(n_district, 825)
})

test_that("total rows in tidy = 840 (56 entities x 15 grades)", {
  expect_equal(nrow(tidy_2024), 840)
})

test_that("campus_id and campus_name are NA for all rows", {
  expect_true(all(is.na(tidy_2024$campus_id)))
  expect_true(all(is.na(tidy_2024$campus_name)))
})

test_that("charter_flag is NA for all rows", {
  expect_true(all(is.na(tidy_2024$charter_flag)))
})

# ==============================================================================
# 9. Per-Year Known Values
# ==============================================================================

test_that("2024 state total enrollment is 241574", {
  state_2024 <- tidy_2024 |>
    dplyr::filter(is_state, grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(state_2024, 241574)
})

test_that("2023 state total enrollment is 248191", {
  state_2023 <- tidy_2023 |>
    dplyr::filter(is_state, grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(state_2023, 248191)
})

test_that("2024 Kanawha County total enrollment is 23219", {
  kanawha <- tidy_2024 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS",
                  grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(kanawha, 23219)
})

test_that("2024 Berkeley County total enrollment is 19785", {
  berkeley <- tidy_2024 |>
    dplyr::filter(district_name == "BERKELEY COUNTY SCHOOLS",
                  grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(berkeley, 19785)
})

test_that("2024 Calhoun County total enrollment is 821", {
  calhoun <- tidy_2024 |>
    dplyr::filter(district_name == "CALHOUN COUNTY SCHOOLS",
                  grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(calhoun, 821)
})

test_that("2024 Wirt County total enrollment is 894", {
  wirt <- tidy_2024 |>
    dplyr::filter(district_name == "WIRT COUNTY SCHOOLS",
                  grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(wirt, 894)
})

test_that("2024 Monongalia County total enrollment is 11159", {
  mono <- tidy_2024 |>
    dplyr::filter(district_name == "MONONGALIA COUNTY SCHOOLS",
                  grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(mono, 11159)
})

test_that("2024 Kanawha grade K is 1540", {
  kanawha_k <- tidy_2024 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS",
                  grade_level == "K",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(kanawha_k, 1540)
})

test_that("2024 Kanawha grade 09 is 2257", {
  kanawha_09 <- tidy_2024 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS",
                  grade_level == "09",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(kanawha_09, 2257)
})

test_that("2024 Berkeley grade K is 1369", {
  berk_k <- wide_2024 |>
    dplyr::filter(district_name == "BERKELEY COUNTY SCHOOLS") |>
    dplyr::pull(grade_k)

  expect_equal(berk_k, 1369)
})

test_that("2023 Kanawha County total enrollment is 23826", {
  kanawha_2023 <- tidy_2023 |>
    dplyr::filter(district_name == "KANAWHA COUNTY SCHOOLS",
                  grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(kanawha_2023, 23826)
})

test_that("2023 Berkeley County total enrollment is 19810", {
  berk_2023 <- tidy_2023 |>
    dplyr::filter(district_name == "BERKELEY COUNTY SCHOOLS",
                  grade_level == "TOTAL",
                  subgroup == "total_enrollment") |>
    dplyr::pull(n_students)

  expect_equal(berk_2023, 19810)
})

test_that("2024 state grade_pk is 13086", {
  state_pk <- wide_2024 |>
    dplyr::filter(type == "State") |>
    dplyr::pull(grade_pk)

  expect_equal(state_pk, 13086)
})

test_that("2024 state grade_k is 16473", {
  state_k <- wide_2024 |>
    dplyr::filter(type == "State") |>
    dplyr::pull(grade_k)

  expect_equal(state_k, 16473)
})

# ==============================================================================
# 10. Cross-Year Consistency
# ==============================================================================

test_that("both years have same column names in tidy format", {
  expect_equal(sort(names(tidy_2023)), sort(names(tidy_2024)))
})

test_that("both years have same column names in wide format", {
  expect_equal(sort(names(wide_2023)), sort(names(wide_2024)))
})

test_that("both years have 55 districts", {
  n_2023 <- length(unique(tidy_2023$district_id[tidy_2023$type == "District"]))
  n_2024 <- length(unique(tidy_2024$district_id[tidy_2024$type == "District"]))

  expect_equal(n_2023, 55)
  expect_equal(n_2024, 55)
})

test_that("same set of district IDs across years", {
  ids_2023 <- sort(unique(tidy_2023$district_id[tidy_2023$type == "District"]))
  ids_2024 <- sort(unique(tidy_2024$district_id[tidy_2024$type == "District"]))

  expect_equal(ids_2023, ids_2024)
})

test_that("same set of district names across years", {
  names_2023 <- sort(unique(tidy_2023$district_name[tidy_2023$type == "District"]))
  names_2024 <- sort(unique(tidy_2024$district_name[tidy_2024$type == "District"]))

  expect_equal(names_2023, names_2024)
})

test_that("end_year column is correct in each year", {
  expect_true(all(tidy_2023$end_year == 2023))
  expect_true(all(tidy_2024$end_year == 2024))
})

test_that("multi-year fetch combines correctly", {
  multi <- wvschooldata::fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE)

  expect_equal(sort(unique(multi$end_year)), c(2023L, 2024L))
  expect_equal(nrow(multi), nrow(tidy_2023) + nrow(tidy_2024))
})

test_that("state totals are plausible across years", {
  st_2023 <- tidy_2023 |>
    dplyr::filter(is_state, grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  st_2024 <- tidy_2024 |>
    dplyr::filter(is_state, grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  # WV enrollment is ~240-260k; year-to-year change should be < 10%
  pct_change <- abs(st_2024 - st_2023) / st_2023
  expect_true(pct_change < 0.10,
              info = paste("Year-to-year change:", round(pct_change * 100, 1), "%"))
})

test_that("all enrollment counts are non-negative", {
  expect_true(all(tidy_2024$n_students >= 0, na.rm = TRUE))
  expect_true(all(tidy_2023$n_students >= 0, na.rm = TRUE))
})

test_that("district names follow COUNTY COUNTY SCHOOLS pattern", {
  dist_names <- unique(tidy_2024$district_name[tidy_2024$type == "District"])
  expect_true(all(grepl("COUNTY SCHOOLS$", dist_names)))
})

test_that("format_district_name produces correct output", {
  expect_equal(wvschooldata:::format_district_name("KANAWHA"), "KANAWHA COUNTY SCHOOLS")
  expect_equal(wvschooldata:::format_district_name("berkeley"), "BERKELEY COUNTY SCHOOLS")
  expect_equal(wvschooldata:::format_district_name("McDowell"), "MCDOWELL COUNTY SCHOOLS")
})

test_that("wide format has correct number of rows per year", {
  # 1 state + 55 districts = 56 rows
  expect_equal(nrow(wide_2024), 56)
  expect_equal(nrow(wide_2023), 56)
})
