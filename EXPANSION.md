# Graduation Rate Data Expansion

## Research Date: 2026-01-04

## Current Package Status

- **R_CMD_check**: PASSING
- **python_test**: PASSING
- **pkgdown**: PASSING

The `wvschooldata` package currently supports enrollment data only (FTE and headcount from WVDE School Finance PDFs, years 2014-2025).

---

## Graduation Rate Data Source

### Primary Source: WV Balanced Scorecard Dashboard

The WV Balanced Scorecard provides publicly accessible CSV files with graduation rate data at state, district, and school levels with subgroup breakdowns.

**Dashboard URL**: https://wveis.k12.wv.us/essa/dashboard.html

### Data Files Enumerated

| File | URL Pattern | HTTP Status | Content |
|------|-------------|-------------|---------|
| High School Data (CSV) | `https://wveis.k12.wv.us/essa/data/{YEAR}/essa_data_hs.csv` | 200 | Graduation rates, school performance |
| All Schools Data (CSV) | `https://wveis.k12.wv.us/essa/data/{YEAR}/essa_data_a.csv` | 200 | Full ESSA accountability data |
| Elementary Data (CSV) | `https://wveis.k12.wv.us/essa/data/{YEAR}/essa_data_el.csv` | 200 | Elementary/middle school data |
| Per Pupil Data (CSV) | `https://wveis.k12.wv.us/essa/Data/{YEAR}/ESSA_PerPupil.csv` | 200 | Financial data per pupil |
| Full Dataset (Excel) | `https://wveis.k12.wv.us/essa/District_Sheets/WV_Balanced_Scorecard_Data_{YEAR}.xlsx` | 200 | Complete dataset in Excel |

### Years Available (Verified HTTP 200)

| Year | essa_data_hs.csv | Excel Download |
|------|------------------|----------------|
| 2018 | Available | Available |
| 2019 | Available | Available |
| 2020 | Available | Available |
| 2021 | Available | Available |
| 2022 | Available | Available |
| 2023 | Available | Available |
| 2024 | Available | Available |
| 2025 | Available | Available |

---

## Schema Analysis

### essa_data_hs.csv Column Structure

**2019-2025 Schema** (47 columns):
```
1. Accountable_Year
2. District_Code (3-digit, zero-padded: 002, 004, ... 106, 999=State)
3. District_Name
4. School_Code (3-digit: 501, 502, ... 999=District Record)
5. School_Name
6. School_Type (SECO for high school, MIDD, ELEM, NT, ALT)
7. Title_1_Indicator
8. Subgroup_Sort (integer 1-14)
9. Subgroup
10. Enrollment_October_Value
11. Enrollment_EOYFAY_Value
12. School_Count_Value
13. Teacher_FTE_Total_Value
14. Per_Pupil_Ratio_Value
15-20. Academic ELA/Math Performance (Color, Pct, Indicator)
21-24. Academic ELA/Math Progress (Color, Pct)
25-28. Attendance (Color, Pct, Rate)
29-30. Discipline (Color, Pct)
31-33. English Learners (Color, Pct, Indicator)
34-36. Gradrate_4_Year (Color, Pct, Indicator, Percent)
37-39. Gradrate_5_Year (Color, Pct, Percent)
40-43. On_Track and Post_Secondary (Color, Pct)
44-47. Create/Modified metadata
```

**2018 Schema** (48 columns - DIFFERENT):
- No `Subgroup_Sort` column (column 8)
- Has `Academic_ELA_Progress_Color_O` and `Academic_Math_Progress_Color_O` extra columns
- Graduation rate fields are at columns 37 (4-year) and 40 (5-year)

### Key Graduation Rate Columns

| Column | 2019+ Position | 2018 Position | Description |
|--------|---------------|---------------|-------------|
| `Gradrate_4_Year_Color` | 33 | 34 | Performance color (Blue/Green/Yellow/Red/Gray) |
| `Gradrate_4_Year_Color_Pct` | 34 | 35 | Percentile ranking |
| `Gradrate_4_Year_Indicator` | 35 | 36 | Target indicator |
| `Gradrate_4_Year_Percent` | 36 | 37 | Actual 4-year graduation rate |
| `Gradrate_5_Year_Color` | 37 | 38 | 5-year color rating |
| `Gradrate_5_Year_Color_Pct` | 38 | 39 | 5-year percentile |
| `Gradrate_5_Year_Percent` | 39 | 40 | Actual 5-year graduation rate |

### ID Format

- **District Code**: 3-digit zero-padded (002=Barbour, 004=Berkeley, ... 999=State)
- **School Code**: 3-digit (501, 502, ... 999=District aggregate)
- WV has 55 counties with 58 districts reporting graduation data
- 117 high schools with graduation data (2025)

### Subgroups (14 categories)

1. American Indian or Alaska Native
2. Asian
3. Black or African American
4. Hispanic or Latino
5. Multi-Racial
6. Pacific Islander
7. White
8. Economically Disadvantaged
9. Children With Disabilities
10. English Learners
11. Homeless
12. Military Connected
13. Foster Care
14. Totals

---

## Time Series Heuristics

### State-Level Graduation Rates (4-Year / 5-Year)

| Year | 4-Year Rate | 5-Year Rate | Notes |
|------|-------------|-------------|-------|
| 2018 | 89.40% | 90.37% | Baseline |
| 2019 | 90.16% | 90.00% | +0.76 pp |
| 2020 | 91.25% | 90.67% | +1.09 pp (COVID year) |
| 2021 | 92.07% | 91.67% | +0.82 pp |
| 2022 | 91.12% | 92.70% | -0.95 pp (dip) |
| 2023 | 91.17% | 92.65% | +0.05 pp |
| 2024 | 92.57% | 92.53% | +1.40 pp |
| 2025 | 92.58% | 93.41% | +0.01 pp (highest on record) |

### Validation Heuristics

1. **State 4-year rate**: Should be 89-93% range
2. **State 5-year rate**: Should be 90-94% range, typically >= 4-year
3. **Year-over-year change**: Max 1.5 percentage points typically
4. **District count**: Expect 55-58 districts with data
5. **School count (SECO)**: Expect 110-120 high schools
6. **Subgroup completeness**: 14 subgroups per entity

### Known Data Characteristics

- Empty/suppressed graduation rates for small subgroups (shown as blank)
- Gray color indicates "Cannot Be Reported" (small n-size)
- District Record (School_Code=999) provides district aggregates
- State Record (District_Code=999) provides state aggregates
- School_Type must be "SECO" for high school graduation data

---

## Implementation Notes

### Recommended Function Signature

```r
fetch_grad <- function(end_year, tidy = TRUE, use_cache = TRUE)
fetch_grad_multi <- function(end_years, tidy = TRUE, use_cache = TRUE)
```

### URL Construction

```r
get_grad_csv_url <- function(end_year) {
  paste0("https://wveis.k12.wv.us/essa/data/", end_year, "/essa_data_hs.csv")
}
```

### Schema Era Detection

```r
get_grad_rate_col <- function(end_year) {
  if (end_year == 2018) {
    list(rate_4yr = 37, rate_5yr = 40)
  } else {
    list(rate_4yr = 36, rate_5yr = 39)
  }
}
```

### Filtering for Graduation Data

```r
# High schools only
filter(School_Type == "SECO")

# Totals subgroup for main rates
filter(Subgroup == "Totals")

# District-level aggregates
filter(School_Code == "999" | School_Name == "District Record")

# State-level aggregates
filter(District_Code == "999" | District_Name == "State Record")
```

---

## Alternative Data Sources (Not Recommended)

### ZoomWV Dashboard
- URL: https://zoomwv.k12.wv.us/Dashboard/dashboard/2111
- Requires JavaScript, no direct API access
- Data is same as Balanced Scorecard CSVs

### NCES/Federal Sources
- **NOT RECOMMENDED per project policy**
- State DOE data preferred

---

## Next Steps (Not Implemented)

1. Create `R/fetch_graduation.R` with `fetch_grad()` function
2. Create `R/get_raw_graduation.R` for raw data download
3. Create `R/process_graduation.R` for data processing
4. Create `R/tidy_graduation.R` for tidy output
5. Add tests in `tests/testthat/test-graduation.R`
6. Add live pipeline tests in `tests/testthat/test-graduation-pipeline-live.R`
7. Update DESCRIPTION with any new dependencies
8. Update package documentation

---

## Research Notes

- Dashboard uses AlaSQL library to query CSVs client-side
- CSVs are UTF-8 with BOM (byte order mark) - may need handling
- Accountable Year in URL corresponds to school year end
- Data typically released in fall (Sep-Oct) for previous school year
