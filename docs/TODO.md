# TODO: pkgdown Build Issues

## Issue: Network Connectivity to WVDE Website

**Date**: 2026-01-01

**Error**: pkgdown build fails when building the vignette
`enrollment_hooks.Rmd`

### Error Details

    Error in `download_fte_enrollment()`:
    ! Failed to download FTE enrollment PDF for year 2014

    The PDF may not be available yet or the URL may have changed.
    Check: https://wvde.us/about-us/finance/school-finance/school-finance-data/

### Root Cause

The WVDE website (wvde.us) is unreachable via HTTPS. While ping
succeeds, HTTPS connections time out after 30 seconds. This appears to
be a network/firewall issue preventing access to the PDF files hosted on
the WVDE site.

Example URL that times out:

    https://wvde.us/sites/default/files/2023/12/Headcount-Enroll-2nd-Mo-24.pdf

### Impact

- The vignette `enrollment_hooks.Rmd` requires live data from the WVDE
  website
- The `fetch_enr_multi(2014:2025)` call fails because it cannot download
  PDF files
- pkgdown cannot build the documentation site

### Possible Solutions

1.  **Wait for network connectivity** - If this is a transient issue,
    retry later
2.  **Pre-cache data** - Include cached enrollment data with the package
    so vignettes can run without network access
3.  **Use `eval=FALSE`** - Modify vignette to not evaluate code chunks
    that require live data (but this defeats the purpose of showing real
    results)
4.  **Add VPN/proxy** - If the issue is local network restrictions, use
    a different network path

### Verification Steps

Test network connectivity:

``` bash
curl -v -m 30 "https://wvde.us/sites/default/files/2023/12/Headcount-Enroll-2nd-Mo-24.pdf"
```

Test in R:

``` r
library(wvschooldata)
fetch_enr(2024)
```
