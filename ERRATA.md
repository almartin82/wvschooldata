# West Virginia School Data - Known Issues

## 1. FTE PDF Parsing Failing

**Status**: Under investigation
**Discovered**: 2026-01-02
**Severity**: High

### Issue

The FTE (Full-Time Equivalent) enrollment PDFs from WVDE are not being parsed correctly. The `parse_fte_pdf()` function returns 0 rows for all years tested.

### Impact

- `fetch_enr()` returns empty data frames (0 rows)
- Only headcount data is being retrieved successfully
- Grade-level enrollment breakdowns are not available

### Workaround

None currently available. The PDF parsing logic needs to be updated to match the current PDF format from WVDE.

---

## 2. Missing Years: 2022 and 2025

**Status**: Data unavailable from source
**Discovered**: 2026-01-02

### Issue

The West Virginia Department of Education (WVDE) School Finance Data page does not have FTE enrollment PDF files available for:
- **2021-22 school year (2022)**: PDF was never published
- **2024-25 school year (2025)**: PDF not yet available (may be released later in the school year)

### Impact

- `get_available_years()` returns: 2013-2021, 2023-2024
- `fetch_enr(2022)` and `fetch_enr(2025)` will fail
- Multi-year analyses should exclude these years

### Workaround

Use the available years explicitly:
```r
# Instead of 2014:2025
enr <- fetch_enr_multi(c(2014:2021, 2023:2024))
```

### Data Source

WVDE School Finance Data: https://wvde.us/about-us/finance/school-finance/school-finance-data/

### Resolution Plan

1. Fix PDF parsing to match current WVDE PDF format
2. Monitor WVDE website for publication of missing years
3. Update `get_available_years()` when data becomes available
