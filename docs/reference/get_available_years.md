# Get available years of West Virginia enrollment data

Returns a vector of years for which enrollment data is available from
the West Virginia Department of Education (WVDE).

## Usage

``` r
get_available_years()
```

## Value

Integer vector of available years (2013-2024)

## Details

Data comes from WVDE School Finance Data PDFs which are published
annually. Historical data availability depends on which years WVDE has
made available on their website.

Note: For 2013, only headcount enrollment data is available (no
grade-level FTE data). The FTE PDFs for 2010-2013 are no longer
available on the WVDE website.

## Examples

``` r
get_available_years()
#>  [1] 2013 2014 2015 2016 2017 2018 2019 2020 2021 2023 2024
```
