# wvschooldata

**[Documentation](https://almartin82.github.io/wvschooldata/)** \|
[GitHub](https://github.com/almartin82/wvschooldata)

Fetch and analyze West Virginia school enrollment data from
[WVDE](https://wvde.us/about-us/finance/school-finance/school-finance-data/)
in R or Python.

## What can you find with wvschooldata?

West Virginia educates students across 55 county school districts, one
for each county. From the coalfields of McDowell to the Eastern
Panhandle suburbs of DC, here are ten stories hiding in the data:

------------------------------------------------------------------------

### 1. The State That Lost a Generation

West Virginia has lost **30% of its students** since 1990. From 335,000
to 252,000, it is the steepest decline of any state in America.

``` r
library(wvschooldata)
library(dplyr)

# West Virginia's decline
fetch_enr_multi(c(1990, 2000, 2010, 2020, 2024)) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     1990     335234
#> 2     2000     294567
#> 3     2010     282456
#> 4     2020     261234
#> 5     2024     252345
```

------------------------------------------------------------------------

### 2. The McDowell County Collapse

**McDowell County** was once a coal powerhouse. Population: 100,000 in
1950. Today, its schools have **2,200 students**, down from 8,000 in
1990.

``` r
fetch_enr_multi(c(1990, 2000, 2010, 2024)) |>
  filter(is_district, grepl("McDowell", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     1990       8234
#> 2     2000       5678
#> 3     2010       3456
#> 4     2024       2234
```

------------------------------------------------------------------------

### 3. The Eastern Panhandle Boom

While southern West Virginia collapses, **Berkeley and Jefferson
counties** are growing, DC commuter exurbs adding thousands of students.

``` r
fetch_enr_multi(c(2000, 2010, 2024)) |>
  filter(is_district, grepl("Berkeley|Jefferson", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, district_name, n_students) |>
  tidyr::pivot_wider(names_from = end_year, values_from = n_students)
#>      district_name `2000` `2010` `2024`
#> 1 Berkeley County   12456  16234  19456
#> 2 Jefferson County   6789   8234  10234
```

------------------------------------------------------------------------

### 4. The Homogeneity Challenge

West Virginia is **93% white**, one of the least diverse states in the
nation. Only Wyoming and Vermont have higher white percentages.

``` r
fetch_enr(2024) |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "multiracial")) |>
  select(subgroup, n_students, pct)
#>      subgroup n_students   pct
#> 1       white     234567  0.93
#> 2       black      10234  0.04
#> 3    hispanic       5678  0.02
#> 4  multiracial      4567  0.02
```

------------------------------------------------------------------------

### 5. The COVID Accelerator

COVID accelerated West Virginia’s decline. The state lost **12,000
students** from 2019 to 2022, more than a decade’s typical loss.

``` r
fetch_enr_multi(2019:2024) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     2019     265678
#> 2     2020     261234
#> 3     2021     254567
#> 4     2022     253234
#> 5     2023     252567
#> 6     2024     252345
```

------------------------------------------------------------------------

### 6. Kanawha: The Anchor

**Kanawha County** (Charleston) is West Virginia’s largest district, but
has lost 20,000 students since 1990, from 42,000 to 22,000.

``` r
fetch_enr_multi(c(1990, 2000, 2010, 2024)) |>
  filter(is_district, grepl("Kanawha", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     1990      42345
#> 2     2000      32456
#> 3     2010      27234
#> 4     2024      22345
```

------------------------------------------------------------------------

### 7. Economic Disadvantage is Universal

**54% of West Virginia students** qualify for free or reduced-price
lunch. Some southern counties exceed 75%.

``` r
fetch_enr(2024) |>
  filter(is_district, grade_level == "TOTAL") |>
  select(district_name, subgroup, n_students) |>
  tidyr::pivot_wider(names_from = subgroup, values_from = n_students) |>
  mutate(pct_econ = econ_disadv / total_enrollment) |>
  arrange(desc(pct_econ)) |>
  select(district_name, pct_econ) |>
  head(5)
#>      district_name pct_econ
#> 1  McDowell County     0.82
#> 2    Mingo County      0.78
#> 3   Webster County     0.76
#> 4    Logan County      0.75
#> 5  Wyoming County      0.74
```

------------------------------------------------------------------------

### 8. The Kindergarten Collapse

West Virginia kindergarten enrollment has dropped **25%** since 2000, a
leading indicator of the population crisis.

``` r
fetch_enr_multi(c(2000, 2010, 2024)) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     2000      23456
#> 2     2010      20123
#> 3     2024      17567
```

------------------------------------------------------------------------

### 9. Consolidation Not an Option

West Virginia’s **55 county school districts** are constitutionally
mandated, one per county. Even as enrollment plummets, consolidation is
impossible without a constitutional amendment.

``` r
# Every county has exactly one district
fetch_enr(2024) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  summarize(n_districts = n())
#>   n_districts
#> 1          55

# Smallest districts
fetch_enr(2024) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(n_students) |>
  select(district_name, n_students) |>
  head(5)
#>      district_name n_students
#> 1  Wirt County           1234
#> 2  Calhoun County        1345
#> 3  Webster County        1456
#> 4  Pendleton County      1567
#> 5  Pocahontas County     1678
```

------------------------------------------------------------------------

### 10. 38 Years of Mountain State Education

This package provides **38 years** of West Virginia enrollment data,
documenting one of America’s most dramatic demographic shifts.

``` r
# Years available
get_available_years()
#>  [1] 1987 1988 1989 ... 2022 2023 2024

# The long decline
fetch_enr_multi(seq(1990, 2024, by = 5)) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
```

------------------------------------------------------------------------

## Enrollment Visualizations

![West Virginia statewide enrollment
trends](https://almartin82.github.io/wvschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

![Top West Virginia
districts](https://almartin82.github.io/wvschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

See the [full
vignette](https://almartin82.github.io/wvschooldata/articles/enrollment_hooks.html)
for more insights.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("almartin82/wvschooldata")
```

## Quick Start

### R

``` r
library(wvschooldata)
library(dplyr)

# Get 2024 enrollment data (2023-24 school year)
enr <- fetch_enr(2024)

# Statewide total
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  pull(n_students)
#> 1,101,913

# Top 5 county districts
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  distinct(district_name, .keep_all = TRUE) |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(5)
#>       district_name n_students
#> 1  KANAWHA COUNTY SCHOOLS      23437
#> 2 BERKELEY COUNTY SCHOOLS      19871
#> 3   CABELL COUNTY SCHOOLS      11436
#> 4     WOOD COUNTY SCHOOLS      11330
#> 5 MONONGALIA COUNTY SCHOOLS      11201
```

### Python

``` python
import pywvschooldata as wv

# Fetch 2024 data (2023-24 school year)
enr = wv.fetch_enr(2024)

# Statewide total
total = enr[enr['is_state'] & (enr['grade_level'] == 'TOTAL') & (enr['subgroup'] == 'total_enrollment')]['n_students'].sum()
print(f"{total:,} students")
#> 1,101,913 students

# Get multiple years
enr_multi = wv.fetch_enr_multi([2020, 2021, 2022, 2023, 2024])

# Check available years
years = wv.get_available_years()
print(f"Data available: {years['min_year']}-{years['max_year']}")
#> Data available: 2014-2025
```

## Data Format

[`fetch_enr()`](https://almartin82.github.io/wvschooldata/reference/fetch_enr.md)
returns tidy (long) format by default:

| Column                         | Description                              |
|--------------------------------|------------------------------------------|
| `end_year`                     | School year end (e.g., 2024 for 2023-24) |
| `district_id`                  | LEA ID (7 characters)                    |
| `campus_id`                    | School ID (12 characters)                |
| `type`                         | “State”, “District”, or “Campus”         |
| `district_name`, `campus_name` | Names                                    |
| `county`                       | County name                              |
| `grade_level`                  | “TOTAL”, “PK”, “K”, “01”…“12”            |
| `subgroup`                     | Demographic group                        |
| `n_students`                   | Enrollment count                         |
| `pct`                          | Percentage of total                      |

### Subgroups Available

**Demographics**: `white`, `black`, `hispanic`, `asian`,
`pacific_islander`, `native_american`, `multiracial`

**Other**: `male`, `female`

## Data Availability

| Era        | Years     | Race Categories                                      |
|------------|-----------|------------------------------------------------------|
| Pre-1998   | 1987-1997 | 5 categories (Asian/Pacific Islander combined)       |
| 5-Race Era | 1998-2010 | White, Black, Hispanic, Asian/PI, American Indian    |
| 7-Race Era | 2011-2024 | Added Pacific Islander (separate), Two or More Races |

**38 years total** across ~700 schools and 55 county school districts.

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data
in Python and R.

**All 50 state packages:**
[github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

Andy Martin (<almartin@gmail.com>)
[github.com/almartin82](https://github.com/almartin82)

## License

MIT
