# Parse FTE enrollment PDF in new 2024+ format

The 2024+ PDFs have a different structure:

- Page 3: Early Childhood, Kindergarten, First through Fifth (7 grade
  columns)

- Page 4: Sixth through Twelfth, Total (8 columns) Each district appears
  on both pages and data must be merged.

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
