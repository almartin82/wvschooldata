# Claude Code Instructions for wvschooldata

## Commit and PR Guidelines

- Do NOT include "Generated with Claude Code" in commit messages
- Do NOT include "Co-Authored-By: Claude" in commit messages
- Do NOT mention Claude or AI assistance in PR descriptions
- Keep commit messages clean and professional

## Project Context

This is an R package for fetching and processing West Virginia school enrollment data from the West Virginia Department of Education (WVDE).

### Key Files

- `R/fetch_enrollment.R` - Main `fetch_enr()` function
- `R/get_raw_enrollment.R` - Downloads and parses WVDE PDFs
- `R/process_enrollment.R` - Transforms raw data to standard schema
- `R/tidy_enrollment.R` - Converts to long/tidy format
- `R/utils.R` - Utility functions (year validation, county lists, etc.)
- `R/cache.R` - Local caching layer

### Data Sources

**CRITICAL: This package uses ONLY West Virginia DOE data sources.**

Data comes from WVDE School Finance Data PDFs:
- URL: https://wvde.us/about-us/finance/school-finance/school-finance-data/
- FTE Enrollment by Grade (2nd month): `FTE-Enrollment-2nd-Mo-YY.pdf`
- Headcount Enrollment (2nd month): `Headcount-Enroll-2nd-Mo-YY.pdf`

### Data Availability

- **Years**: 2014-2025 (12 years)
- **Aggregation**: State and District (County) level only
- **Grade levels**: PK through 12

### Data Limitations

The WVDE School Finance Data PDFs do NOT provide:
- School-level enrollment (only county aggregates)
- Demographic breakdowns (race, gender, etc.)
- Special education or ELL counts

For more detailed data, users would need to request from ZoomWV or use NCES data.

### URL Patterns

WVDE uses two URL patterns for PDFs:
1. Newer: `https://wvde.us/sites/default/files/YYYY/MM/filename.pdf`
2. Legacy: `https://wvde.us/wp-content/uploads/YYYY/MM/filename.pdf`

The `get_wvde_pdf_url()` function maintains a lookup table of known URLs.

### West Virginia Structure

- 55 counties = 55 county school districts
- District IDs use FIPS codes: `54` (state) + 3-digit county code
- Example: Kanawha County = `54039`

### DO NOT USE

- Urban Institute Education Data Portal API
- NCES Common Core of Data (CCD) API
- Any federal data sources

All enrollment data must come directly from WVDE.

### Related Package

This package follows patterns from other state schooldata packages in the same repository.
