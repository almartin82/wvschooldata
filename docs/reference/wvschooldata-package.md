# wvschooldata: Fetch and Process West Virginia School Data

Downloads and processes school enrollment data from the West Virginia
Department of Education (WVDE). Provides functions for fetching Fall
Membership (2nd month) enrollment data from WVDE School Finance Data
PDFs and transforming it into tidy format for analysis.

## Main functions

- [`fetch_enr`](https://almartin82.github.io/wvschooldata/reference/fetch_enr.md):

  Fetch enrollment data for a school year

- [`fetch_enr_multi`](https://almartin82.github.io/wvschooldata/reference/fetch_enr_multi.md):

  Fetch enrollment data for multiple years

- [`tidy_enr`](https://almartin82.github.io/wvschooldata/reference/tidy_enr.md):

  Transform wide data to tidy (long) format

- [`id_enr_aggs`](https://almartin82.github.io/wvschooldata/reference/id_enr_aggs.md):

  Add aggregation level flags

- [`enr_grade_aggs`](https://almartin82.github.io/wvschooldata/reference/enr_grade_aggs.md):

  Create grade-level aggregations

- [`get_available_years`](https://almartin82.github.io/wvschooldata/reference/get_available_years.md):

  Get list of available data years

## Cache functions

- [`cache_status`](https://almartin82.github.io/wvschooldata/reference/cache_status.md):

  View cached data files

- [`clear_cache`](https://almartin82.github.io/wvschooldata/reference/clear_cache.md):

  Remove cached data files

## ID System

West Virginia uses FIPS-based district IDs:

- State FIPS: 54 (West Virginia)

- District IDs: 5-digit codes (state FIPS + 3-digit county FIPS)

- Example: 54039 = Kanawha County Schools

West Virginia has 55 county school districts (one per county).

## Data Sources

Data is sourced from:

- WVDE School Finance Data:
  <https://wvde.us/about-us/finance/school-finance/school-finance-data/>

- FTE Enrollment by Grade PDFs (2nd month enrollment)

- Headcount Enrollment PDFs (2nd month enrollment)

## Data Availability

- Years: 2014-2025 (12 years)

- Aggregation levels: State, District (County)

- Grade levels: PK through 12

- Note: School-level and demographic data not available from PDFs

## Data Limitations

The WVDE School Finance Data PDFs provide:

- FTE (Full-Time Equivalent) enrollment by grade

- Headcount enrollment totals

- County-level aggregation only (no individual schools)

For school-level or demographic data, see ZoomWV:
<https://zoomwv.k12.wv.us/>

## See also

Useful links:

- <https://almartin82.github.io/wvschooldata>

- <https://github.com/almartin82/wvschooldata>

- Report bugs at <https://github.com/almartin82/wvschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
