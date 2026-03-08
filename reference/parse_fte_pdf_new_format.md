# Parse FTE enrollment PDF in multi-page format

WVDE FTE PDFs split grade-level data across two pages. Two layouts
exist:

- **2014-2015, 2023+**: Page A has PK through 5th (7 columns), Page B
  has 6th through 12th + Total (8 columns). Page A does NOT have "Sixth"
  in header.

- **2016-2020**: Page A has PK through 6th (8 columns), Page B has 7th
  through 12th + Total (7 columns). Page A DOES have "Sixth" in header.

## Usage

``` r
parse_fte_pdf_new_format(text, end_year)
```

## Arguments

- text:

  Character vector of PDF text (one element per page)

- end_year:

  School year for context

## Value

Data frame with enrollment by county and grade
