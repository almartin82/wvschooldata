# Fetch West Virginia school directory data

Downloads and processes school directory data from the West Virginia
Department of Education. This includes all public and non-public schools
with contact information, addresses, and grade levels.

## Usage

``` r
fetch_directory(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE)
```

## Arguments

- tidy:

  If TRUE (default), returns data in a standardized format with
  consistent column names. If FALSE, returns raw column names from WVDE.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from WVDE.

- include_superintendents:

  If TRUE, merges superintendent information from the separate WVDE
  superintendent list. Default is TRUE.

## Value

A tibble with school directory data. Columns include:

- `state_school_id`: School identifier (type + county + name hash)

- `state_district_id`: County FIPS code (54XXX format)

- `school_name`: School name

- `district_name`: County school district name

- `school_type`: Type code (BOE, ELEM, MIDD, SECO, PRIM, K, NT)

- `grades_served`: Grade span (e.g., "K-5", "9-12")

- `address`: Street address

- `city`: City

- `state`: State (always "WV")

- `zip`: ZIP code (5-digit)

- `phone`: Phone number

- `fax`: Fax number

- `website`: School website URL

- `superintendent_name`: County superintendent name (if
  include_superintendents=TRUE)

- `superintendent_email`: County superintendent email (if
  include_superintendents=TRUE)

## Details

The directory data is downloaded as a CSV file from the WVEIS portal.
This data represents the current state of West Virginia schools and is
updated regularly by WVDE.

School types:

- BOE: Board of Education (county central office)

- ELEM: Elementary school

- MIDD: Middle school

- SECO: Secondary/High school

- PRIM: Primary school

- K: Non-public/private school

- NT: Non-traditional (virtual, etc.)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get school directory data
dir_data <- fetch_directory()

# Get raw format (original WVDE column names)
dir_raw <- fetch_directory(tidy = FALSE)

# Force fresh download (ignore cache)
dir_fresh <- fetch_directory(use_cache = FALSE)

# Get only public schools
library(dplyr)
public_schools <- dir_data |>
  filter(!school_type %in% c("K", "BOE"))

# Find all schools in a county
kanawha_schools <- dir_data |>
  filter(district_name == "Kanawha County Schools")
} # }
```
