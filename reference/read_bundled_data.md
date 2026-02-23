# Read bundled data from inst/extdata

Falls back to pre-packaged RDS files when network is unavailable.

## Usage

``` r
read_bundled_data(end_year, type)
```

## Arguments

- end_year:

  School year end

- type:

  Data type ("tidy" or "wide")

## Value

Data frame or NULL if no bundled data exists
