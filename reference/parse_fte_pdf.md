# Parse FTE enrollment PDF

Parses the FTE Enrollment by Grade PDF from WVDE. These PDFs contain
county-level enrollment broken down by grade.

## Usage

``` r
parse_fte_pdf(pdf_path, end_year)
```

## Arguments

- pdf_path:

  Path to the PDF file

- end_year:

  School year for context

## Value

Data frame with enrollment by county and grade
