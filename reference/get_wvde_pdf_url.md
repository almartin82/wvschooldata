# Get URL for WVDE enrollment PDF

Constructs the URL for downloading enrollment PDFs from WVDE. WVDE uses
two URL patterns depending on the year.

## Usage

``` r
get_wvde_pdf_url(end_year, type = "FTE")
```

## Arguments

- end_year:

  School year end

- type:

  Either "FTE" or "Headcount"

## Value

Character URL
