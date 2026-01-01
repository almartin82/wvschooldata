# Parse headcount enrollment PDF

Parses the Headcount Enrollment PDF from WVDE. These PDFs contain total
headcount by county, sorted by enrollment.

## Usage

``` r
parse_headcount_pdf(pdf_path, end_year)
```

## Arguments

- pdf_path:

  Path to the PDF file

- end_year:

  School year for context

## Value

Data frame with headcount by county
