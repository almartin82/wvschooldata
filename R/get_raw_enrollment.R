# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from the
# West Virginia Department of Education (WVDE).
#
# Data comes from WVDE School Finance Data PDFs:
# - FTE Enrollment by Grade (by county district)
# - Headcount Enrollment (by county district)
#
# PDFs are available at: https://wvde.us/about-us/finance/school-finance/school-finance-data/
#
# West Virginia has 55 county school districts (one per county).
# Enrollment data is collected as of October 1st (2nd month) each school year.
#
# ==============================================================================

#' Download raw enrollment data from WVDE
#'
#' Downloads county-level enrollment data from West Virginia Department of
#' Education School Finance Data PDFs.
#'
#' @param end_year School year end (e.g., 2024 for 2023-24 school year)
#' @return Data frame with enrollment data by county district
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year
  available_years <- get_available_years()
  if (!end_year %in% available_years) {
    stop(paste0(
      "end_year must be between ", min(available_years), " and ", max(available_years),
      ". Got: ", end_year
    ))
  }

  message(paste("Downloading West Virginia enrollment data for", end_year, "..."))

  # Years with FTE data available (2014+)
  # Note: FTE PDFs for 2010-2013 are no longer available on WVDE website
  fte_available_years <- 2014L:2026L

  # Download FTE enrollment PDF (has grade breakdowns) if available
  if (end_year %in% fte_available_years) {
    message("  Downloading FTE enrollment data...")
    fte_data <- download_fte_enrollment(end_year)
  } else {
    message("  FTE enrollment data not available for this year (headcount only)")
    fte_data <- create_empty_fte_df()
  }

  # Download headcount enrollment PDF (has total counts)
  message("  Downloading headcount enrollment data...")
  headcount_data <- download_headcount_enrollment(end_year)

  list(
    fte = fte_data,
    headcount = headcount_data
  )
}


#' Get URL for WVDE enrollment PDF
#'
#' Constructs the URL for downloading enrollment PDFs from WVDE.
#' WVDE uses two URL patterns depending on the year.
#'
#' @param end_year School year end
#' @param type Either "FTE" or "Headcount"
#' @return Character URL
#' @keywords internal
get_wvde_pdf_url <- function(end_year, type = "FTE") {

  yy <- sprintf("%02d", end_year %% 100)

  # WVDE uses different URL patterns by year
  # Pattern 1: https://wvde.us/sites/default/files/YYYY/MM/FTE-Enrollment-2nd-Mo-YY.pdf
  # Pattern 2: https://wvde.us/wp-content/uploads/YYYY/MM/FTE-Enrollment-2nd-Mo-YY.pdf

  # Build filename
  if (type == "FTE") {
    filename <- paste0("FTE-Enrollment-2nd-Mo-", yy, ".pdf")
  } else {
    filename <- paste0("Headcount-Enroll-2nd-Mo-", yy, ".pdf")
  }

  # Known URL mappings based on research

  # These are verified URLs from WVDE
  # Note: FTE PDFs for 2010-2013 are no longer available on WVDE website
  # Only 2013 headcount is available from pre-2014 years
  # URLs verified as of March 2026
  # WVDE migrated from wp-content/uploads to sites/default/files
  # Note: 2026 PDFs use spaces in filenames and YYYY-MM date path format
  url_map <- list(
    "FTE" = list(
      "26" = "https://wvde.us/sites/default/files/2025-12/FTE%20Enrollment%202nd%20Mo%2026.pdf",
      "25" = "https://wvde.us/sites/default/files/2024/12/FTE-Enrollment-2nd-Mo-25.pdf",
      "24" = "https://wvde.us/sites/default/files/2024/01/FTE-Enrollment-2nd-Mo-24.pdf",
      "23" = "https://wvde.us/sites/default/files/2023/12/FTE-Enrollment-2nd-Mo-23.pdf",
      "22" = "https://wvde.us/sites/default/files/2022/12/FTE-Enrollment-2nd-Mo-22.pdf",
      "21" = "https://wvde.us/sites/default/files/2022/01/FTE-Enrollment-2nd-Mo-21.pdf",
      "20" = "https://wvde.us/sites/default/files/2019/12/FTE-Enrollment-2nd-Mo-20.pdf",
      "19" = "https://wvde.us/sites/default/files/2019/08/FTE-Enrollment-2nd-Mo-19.pdf",
      "18" = "https://wvde.us/sites/default/files/2018/03/FTE-Enrollment-2nd-Mo-18.pdf",
      "17" = "https://wvde.us/sites/default/files/2017/10/FTE-Enrollment-2nd-Mo-17.pdf",
      "16" = "https://wvde.us/sites/default/files/2017/10/FTE-Enrollment-2nd-Mo-16.pdf",
      "15" = "https://wvde.us/sites/default/files/2017/10/FTE-Enrollment-2nd-Mo-15.pdf",
      "14" = "https://wvde.us/sites/default/files/2017/10/FTE-Enrollment-2nd-Mo-14.pdf"
      # Note: FTE PDFs for years 10-13 are no longer available on WVDE website
    ),
    "Headcount" = list(
      "26" = "https://wvde.us/sites/default/files/2025-12/Headcount%20Enroll%20-%202nd%20Mo%2026.pdf",
      "25" = "https://wvde.us/sites/default/files/2024/12/Headcount-Enroll-2nd-Mo-25.pdf",
      "24" = "https://wvde.us/sites/default/files/2024/01/Headcount-Enroll-2nd-Mo-24.pdf",
      "23" = "https://wvde.us/sites/default/files/2023/12/Headcount-Enroll-2nd-Mo-23.pdf",
      "22" = "https://wvde.us/sites/default/files/2022/12/Headcount-Enroll-2nd-Mo-22.pdf",
      "21" = "https://wvde.us/sites/default/files/2022/01/Headcount-Enroll-2nd-Mo-21.pdf",
      "20" = "https://wvde.us/sites/default/files/2019/12/Headcount-Enroll-2nd-Mo-20.pdf",
      "19" = "https://wvde.us/sites/default/files/2019/08/Headcount-Enroll-2nd-Mo-19.pdf",
      "18" = "https://wvde.us/sites/default/files/2018/03/Headcount-Enroll-2nd-Mo-18.pdf",
      "17" = "https://wvde.us/sites/default/files/2017/10/Headcount-Enroll-2nd-Mo-17.pdf",
      "16" = "https://wvde.us/sites/default/files/2017/10/Headcount-Enroll-2nd-Mo-16.pdf",
      "15" = "https://wvde.us/sites/default/files/2017/10/Headcount-Enroll-2nd-Mo-15.pdf",
      "14" = "https://wvde.us/sites/default/files/2017/10/Headcount-Enroll-2nd-Mo-14.pdf",
      "13" = "https://wvde.us/sites/default/files/2017/10/Headcount-Enroll-2nd-Mo-13.pdf"
      # Note: Headcount PDFs for years 10-12 are no longer available on WVDE website
    )
  )

  # Return known URL if available
  if (yy %in% names(url_map[[type]])) {
    return(url_map[[type]][[yy]])
  }

  # Fallback: try common URL patterns
  # Try sites/default/files first (newer pattern)
  url1 <- paste0("https://wvde.us/sites/default/files/", end_year - 1, "/12/", filename)
  url2 <- paste0("https://wvde.us/wp-content/uploads/", end_year - 1, "/12/", filename)

  # Return the primary guess - download function will try fallbacks
  url1
}


#' Download FTE enrollment PDF
#'
#' Downloads the FTE Enrollment by Grade PDF from WVDE and parses it.
#'
#' @param end_year School year end
#' @return Data frame with grade-level enrollment by county
#' @keywords internal
download_fte_enrollment <- function(end_year) {

  url <- get_wvde_pdf_url(end_year, "FTE")

  # Create temp file for download
  tname <- tempfile(pattern = paste0("wv_fte_", end_year, "_"), fileext = ".pdf")

  # Download with fallback attempts
  download_success <- FALSE
  urls_to_try <- c(url)

  # Add fallback URLs
  yy <- sprintf("%02d", end_year %% 100)
  filename <- paste0("FTE-Enrollment-2nd-Mo-", yy, ".pdf")
  urls_to_try <- c(
    urls_to_try,
    paste0("https://wvde.us/sites/default/files/", end_year, "/01/", filename),
    paste0("https://wvde.us/wp-content/uploads/", end_year, "/01/", filename),
    paste0("https://wvde.us/sites/default/files/", end_year - 1, "/12/", filename),
    paste0("https://wvde.us/wp-content/uploads/", end_year - 1, "/12/", filename)
  )
  urls_to_try <- unique(urls_to_try)

  for (try_url in urls_to_try) {
    tryCatch({
      response <- httr::GET(
        try_url,
        httr::write_disk(tname, overwrite = TRUE),
        httr::timeout(120),
        httr::user_agent("Mozilla/5.0 (compatible; wvschooldata R package)")
      )

      if (!httr::http_error(response)) {
        # Verify we got a PDF (check file size and magic bytes)
        file_info <- file.info(tname)
        if (file_info$size > 5000) {
          # Check for PDF magic bytes
          con <- file(tname, "rb")
          header <- readBin(con, what = "raw", n = 4)
          close(con)
          if (rawToChar(header) == "%PDF") {
            download_success <- TRUE
            break
          }
        }
      }
    }, error = function(e) {
      # Continue to next URL
    })
  }

  if (!download_success) {
    unlink(tname)
    stop(paste(
      "Failed to download FTE enrollment PDF for year", end_year,
      "\n\nThe PDF may not be available yet or the URL may have changed.",
      "\nCheck: https://wvde.us/about-us/finance/school-finance/school-finance-data/"
    ))
  }

  # Parse the PDF
  df <- parse_fte_pdf(tname, end_year)

  unlink(tname)

  df
}


#' Download headcount enrollment PDF
#'
#' Downloads the Headcount Enrollment PDF from WVDE and parses it.
#'
#' @param end_year School year end
#' @return Data frame with total enrollment by county
#' @keywords internal
download_headcount_enrollment <- function(end_year) {

  url <- get_wvde_pdf_url(end_year, "Headcount")

  # Create temp file for download
  tname <- tempfile(pattern = paste0("wv_headcount_", end_year, "_"), fileext = ".pdf")

  # Download with fallback attempts
  download_success <- FALSE
  urls_to_try <- c(url)

  # Add fallback URLs
  yy <- sprintf("%02d", end_year %% 100)
  filename <- paste0("Headcount-Enroll-2nd-Mo-", yy, ".pdf")
  urls_to_try <- c(
    urls_to_try,
    paste0("https://wvde.us/sites/default/files/", end_year, "/01/", filename),
    paste0("https://wvde.us/wp-content/uploads/", end_year, "/01/", filename),
    paste0("https://wvde.us/sites/default/files/", end_year - 1, "/12/", filename),
    paste0("https://wvde.us/wp-content/uploads/", end_year - 1, "/12/", filename)
  )
  urls_to_try <- unique(urls_to_try)

  for (try_url in urls_to_try) {
    tryCatch({
      response <- httr::GET(
        try_url,
        httr::write_disk(tname, overwrite = TRUE),
        httr::timeout(120),
        httr::user_agent("Mozilla/5.0 (compatible; wvschooldata R package)")
      )

      if (!httr::http_error(response)) {
        # Verify we got a PDF
        file_info <- file.info(tname)
        if (file_info$size > 5000) {
          con <- file(tname, "rb")
          header <- readBin(con, what = "raw", n = 4)
          close(con)
          if (rawToChar(header) == "%PDF") {
            download_success <- TRUE
            break
          }
        }
      }
    }, error = function(e) {
      # Continue to next URL
    })
  }

  if (!download_success) {
    unlink(tname)
    # Headcount is less critical - return empty with warning
    warning(paste(
      "Could not download headcount enrollment PDF for year", end_year,
      "- using FTE data only"
    ))
    return(data.frame())
  }

  # Parse the PDF
  df <- parse_headcount_pdf(tname, end_year)

  unlink(tname)

  df
}


#' Parse FTE enrollment PDF
#'
#' Parses the FTE Enrollment by Grade PDF from WVDE.
#' These PDFs contain county-level enrollment broken down by grade.
#'
#' @param pdf_path Path to the PDF file
#' @param end_year School year for context
#' @return Data frame with enrollment by county and grade
#' @keywords internal
parse_fte_pdf <- function(pdf_path, end_year) {

  # Check for pdftools package

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required to parse WVDE PDFs. Install with: install.packages('pdftools')")
  }

  # Read PDF text
  text <- pdftools::pdf_text(pdf_path)

  # All multi-page PDFs (2014+) have grade-level data split across pages.
  # Detect by looking for "Childhood" in the text (the "Early Childhood" header).
  # Note: in some PDFs "Early" and "Childhood" are on separate lines, so we

  # check for "Childhood" alone rather than requiring them adjacent.
  is_multipage_format <- any(grepl("Childhood", text, ignore.case = TRUE))

  if (is_multipage_format) {
    return(parse_fte_pdf_new_format(text, end_year))
  }

  # Legacy format parsing for older years
  all_data <- list()

  for (page_num in seq_along(text)) {
    page_text <- text[page_num]

    # Split into lines
    lines <- strsplit(page_text, "\n")[[1]]
    lines <- trimws(lines)

    # Find header line to understand column structure
    # Typical format: County | PK | K | 1 | 2 | ... | 12 | Total
    header_idx <- grep("^(COUNTY|County).*?(PK|K|KG).*?(Total|TOTAL)", lines, ignore.case = TRUE)

    if (length(header_idx) == 0) {
      # Try alternate header patterns
      header_idx <- grep("(Pre-?K|PK).*?(Grade|Gr\\.?)?\\s*1", lines, ignore.case = TRUE)
    }

    # Parse data lines (lines after header with county names and numbers)
    for (line in lines) {
      parsed <- parse_fte_line(line, end_year)
      if (!is.null(parsed)) {
        all_data <- c(all_data, list(parsed))
      }
    }
  }

  if (length(all_data) == 0) {
    warning("No data could be parsed from FTE PDF for year ", end_year)
    return(create_empty_fte_df())
  }

  # Combine all parsed data
  df <- dplyr::bind_rows(all_data)

  # Remove state total row if present
  df <- df[!grepl("^(STATE|TOTAL|State Total)", df$county_name, ignore.case = TRUE), ]

  df
}


#' Parse FTE enrollment PDF in multi-page format
#'
#' WVDE FTE PDFs split grade-level data across two pages. Two layouts exist:
#' - **2014-2015, 2023+**: Page A has PK through 5th (7 columns), Page B has
#'   6th through 12th + Total (8 columns). Page A does NOT have "Sixth" in header.
#' - **2016-2020**: Page A has PK through 6th (8 columns), Page B has 7th through
#'   12th + Total (7 columns). Page A DOES have "Sixth" in header.
#'
#' @param text Character vector of PDF text (one element per page)
#' @param end_year School year for context
#' @return Data frame with enrollment by county and grade
#' @keywords internal
parse_fte_pdf_new_format <- function(text, end_year) {

  # West Virginia counties (for validation)
  wv_counties <- get_wv_counties()

  # Detect layout by checking if any page has "Sixth" in the SAME page as
  # "Childhood"/"garten". If so, this is the 2016-2020 layout (8+7 columns).
  # If Sixth is on a separate page, this is the 2014-2015/2023+ layout (7+8).
  has_sixth_with_childhood <- FALSE
  for (page_text in text) {
    page_lines <- strsplit(page_text, "\n")[[1]]
    has_childhood <- any(grepl("Childhood", page_lines, ignore.case = TRUE))
    has_garten <- any(grepl("(garten|First|Second)", page_lines, ignore.case = TRUE))
    has_sixth <- any(grepl("Sixth", page_lines, ignore.case = TRUE))
    if (has_childhood && has_garten && has_sixth) {
      has_sixth_with_childhood <- TRUE
      break
    }
  }

  # Layout A (2014-2015, 2023+): first_page = 7 cols (PK-5th), second_page = 8 cols (6th-12th+Total)
  # Layout B (2016-2020): first_page = 8 cols (PK-6th), second_page = 7 cols (7th-12th+Total)
  is_layout_b <- has_sixth_with_childhood

  first_page_data <- list()
  second_page_data <- list()

  for (page_num in seq_along(text)) {
    page_text <- text[page_num]
    lines <- strsplit(page_text, "\n")[[1]]
    lines <- trimws(lines)

    # Identify page type
    has_childhood <- any(grepl("Childhood", lines, ignore.case = TRUE))
    has_garten <- any(grepl("(garten|First|Second)", lines, ignore.case = TRUE))
    has_seventh <- any(grepl("Seventh", lines, ignore.case = TRUE))

    is_first_page_type <- has_childhood && has_garten
    is_second_page_type <- has_seventh && !is_first_page_type

    if (!is_first_page_type && !is_second_page_type) next

    for (line in lines) {
      # Skip header/title lines
      if (grepl("^(WEST VIRGINIA|FULL-TIME|CERTIFIED|COUNTY BOARDS|\\d{4}-\\d{2}|District\\s+Early|District\\s+Sixth|County\\s+Early|County\\s+Sixth|County\\s+Childhood|^\\s*$)", line, ignore.case = TRUE)) {
        next
      }

      # Skip lines that are part of the header (grade labels)
      if (grepl("^(Early|Kinder|garten|Childhood)", line, ignore.case = TRUE)) next

      # Try to extract county name from beginning of line
      county_match <- regmatches(line, regexpr("^[A-Za-z]+", line))
      if (length(county_match) == 0) next

      county_name <- toupper(county_match)
      if (!county_name %in% wv_counties) next

      # Extract all numbers from the line
      nums <- extract_numbers_from_line(line)

      if (is_first_page_type) {
        if (is_layout_b && length(nums) >= 8) {
          # Layout B: PK, K, 1, 2, 3, 4, 5, 6 (8 values)
          first_page_data[[county_name]] <- nums[1:8]
        } else if (!is_layout_b && length(nums) >= 7) {
          # Layout A: PK, K, 1, 2, 3, 4, 5 (7 values)
          first_page_data[[county_name]] <- nums[1:7]
        }
      } else if (is_second_page_type) {
        if (is_layout_b && length(nums) >= 7) {
          # Layout B: 7, 8, 9, 10, 11, 12, Total (7 values)
          second_page_data[[county_name]] <- nums[1:7]
        } else if (!is_layout_b && length(nums) >= 8) {
          # Layout A: 6, 7, 8, 9, 10, 11, 12, Total (8 values)
          second_page_data[[county_name]] <- nums[1:8]
        }
      }
    }
  }

  if (length(first_page_data) == 0 && length(second_page_data) == 0) {
    warning("No data could be parsed from FTE PDF for year ", end_year)
    return(create_empty_fte_df())
  }

  # Merge data from both pages
  all_counties <- unique(c(names(first_page_data), names(second_page_data)))
  results <- list()

  for (county in all_counties) {
    fp <- first_page_data[[county]]
    sp <- second_page_data[[county]]

    if (is_layout_b) {
      # Layout B (2016-2020): first_page = PK-6th (8), second_page = 7th-12th+Total (7)
      if (is.null(fp)) fp <- rep(NA_real_, 8)
      if (is.null(sp)) sp <- rep(NA_real_, 7)

      results[[county]] <- data.frame(
        county_name = county,
        grade_pk = fp[1],      # Early Childhood
        grade_k = fp[2],       # Kindergarten
        grade_01 = fp[3],      # First
        grade_02 = fp[4],      # Second
        grade_03 = fp[5],      # Third
        grade_04 = fp[6],      # Fourth
        grade_05 = fp[7],      # Fifth
        grade_06 = fp[8],      # Sixth
        grade_07 = sp[1],      # Seventh
        grade_08 = sp[2],      # Eighth
        grade_09 = sp[3],      # Ninth
        grade_10 = sp[4],      # Tenth
        grade_11 = sp[5],      # Eleventh
        grade_12 = sp[6],      # Twelfth
        row_total = sp[7],     # Total
        stringsAsFactors = FALSE
      )
    } else {
      # Layout A (2014-2015, 2023+): first_page = PK-5th (7), second_page = 6th-12th+Total (8)
      if (is.null(fp)) fp <- rep(NA_real_, 7)
      if (is.null(sp)) sp <- rep(NA_real_, 8)

      results[[county]] <- data.frame(
        county_name = county,
        grade_pk = fp[1],      # Early Childhood
        grade_k = fp[2],       # Kindergarten
        grade_01 = fp[3],      # First
        grade_02 = fp[4],      # Second
        grade_03 = fp[5],      # Third
        grade_04 = fp[6],      # Fourth
        grade_05 = fp[7],      # Fifth
        grade_06 = sp[1],      # Sixth
        grade_07 = sp[2],      # Seventh
        grade_08 = sp[3],      # Eighth
        grade_09 = sp[4],      # Ninth
        grade_10 = sp[5],      # Tenth
        grade_11 = sp[6],      # Eleventh
        grade_12 = sp[7],      # Twelfth
        row_total = sp[8],     # Total
        stringsAsFactors = FALSE
      )
    }
  }

  df <- dplyr::bind_rows(results)

  # Validate totals - calculate sum and compare
  grade_cols <- c("grade_pk", "grade_k", paste0("grade_", sprintf("%02d", 1:12)))
  df$calc_total <- rowSums(df[, grade_cols], na.rm = TRUE)

  # If row_total is NA or 0, use calculated total
  df$row_total <- ifelse(is.na(df$row_total) | df$row_total == 0,
                         df$calc_total, df$row_total)
  df$calc_total <- NULL

  df
}


#' Extract numbers from a PDF line
#'
#' Handles various number formats including decimals and commas.
#'
#' @param line Character string from PDF
#' @return Numeric vector of extracted numbers
#' @keywords internal
extract_numbers_from_line <- function(line) {
  # Remove county name from beginning
  line_no_county <- sub("^[A-Za-z]+\\s+", "", line)

  # Find all number patterns (with optional commas and decimals)
  num_matches <- gregexpr("[0-9,]+\\.?[0-9]*", line_no_county)
  num_strings <- regmatches(line_no_county, num_matches)[[1]]

  # Convert to numeric
  nums <- sapply(num_strings, function(x) {
    x <- gsub(",", "", x)  # Remove commas
    as.numeric(x)
  }, USE.NAMES = FALSE)

  nums[!is.na(nums)]
}


#' Parse a single line from FTE enrollment PDF
#'
#' @param line Text line from PDF
#' @param end_year School year for context
#' @return Data frame row or NULL if line is not data
#' @keywords internal
parse_fte_line <- function(line, end_year) {

  # Skip obvious non-data lines
  skip_patterns <- c(
    "^\\s*$",                          # Empty lines
    "^COUNTY.*BOARDS",                 # Header
    "^FTE ENROLLMENT",                 # Title
    "^2nd MONTH",                      # Subtitle
    "^WEST VIRGINIA",                  # Header
    "^Page\\s+\\d",                    # Page numbers
    "^(PK|Pre-?K|K|KG)\\s",           # Column header row
    "^\\d{4}-\\d{2}",                  # School year header
    "^County\\s",                      # Column header
    "^\\s*PK\\s+K\\s+",               # Column header row
    "^\\*",                            # Footnotes
    "^Note:"                           # Notes
  )

  for (pattern in skip_patterns) {
    if (grepl(pattern, line, ignore.case = TRUE)) {
      return(NULL)
    }
  }

  # Try to extract county name and grade enrollment numbers
  # Typical format: BARBOUR    45.5   78.0   82.0   ...   Total

  # Split on multiple spaces (PDF columns are space-separated)
  parts <- strsplit(line, "\\s{2,}")[[1]]
  parts <- trimws(parts)
  parts <- parts[parts != ""]

  if (length(parts) < 5) return(NULL)

  # First part should be county name (all letters)
  county_name <- parts[1]

  # Validate it looks like a county name (all letters, possibly with spaces)
  if (!grepl("^[A-Za-z]+$", county_name)) {
    # Try extracting from the beginning if there's a multi-word county
    county_match <- regmatches(line, regexpr("^[A-Za-z][A-Za-z ]+(?=\\s{2,})", line, perl = TRUE))
    if (length(county_match) > 0) {
      county_name <- trimws(county_match)
      # Re-split after county name
      remainder <- sub(paste0("^", county_name, "\\s+"), "", line)
      parts <- strsplit(remainder, "\\s{2,}")[[1]]
      parts <- c(county_name, trimws(parts[parts != ""]))
    } else {
      return(NULL)
    }
  }

  # West Virginia counties (for validation)
  wv_counties <- c(
    "BARBOUR", "BERKELEY", "BOONE", "BRAXTON", "BROOKE",
    "CABELL", "CALHOUN", "CLAY", "DODDRIDGE", "FAYETTE",
    "GILMER", "GRANT", "GREENBRIER", "HAMPSHIRE", "HANCOCK",
    "HARDY", "HARRISON", "JACKSON", "JEFFERSON", "KANAWHA",
    "LEWIS", "LINCOLN", "LOGAN", "MCDOWELL", "MARION",
    "MARSHALL", "MASON", "MERCER", "MINERAL", "MINGO",
    "MONONGALIA", "MONROE", "MORGAN", "NICHOLAS", "OHIO",
    "PENDLETON", "PLEASANTS", "POCAHONTAS", "PRESTON", "PUTNAM",
    "RALEIGH", "RANDOLPH", "RITCHIE", "ROANE", "SUMMERS",
    "TAYLOR", "TUCKER", "TYLER", "UPSHUR", "WAYNE",
    "WEBSTER", "WETZEL", "WIRT", "WOOD", "WYOMING"
  )

  if (!toupper(county_name) %in% wv_counties) {
    return(NULL)
  }

  # Extract numbers from remaining parts
  num_parts <- parts[2:length(parts)]
  nums <- safe_numeric(num_parts)

  # Need at least PK through grade 12 + total = 15 values
  # But some PDFs may have different structures
  if (length(nums) < 10 || all(is.na(nums))) {
    return(NULL)
  }

  # Create data row
  # Expected columns: PK, K, 1-12, Total = 15 columns
  # Some years may have additional columns (e.g., Ungraded)
  result <- data.frame(
    county_name = toupper(county_name),
    grade_pk = if(length(nums) >= 1) nums[1] else NA_real_,
    grade_k = if(length(nums) >= 2) nums[2] else NA_real_,
    grade_01 = if(length(nums) >= 3) nums[3] else NA_real_,
    grade_02 = if(length(nums) >= 4) nums[4] else NA_real_,
    grade_03 = if(length(nums) >= 5) nums[5] else NA_real_,
    grade_04 = if(length(nums) >= 6) nums[6] else NA_real_,
    grade_05 = if(length(nums) >= 7) nums[7] else NA_real_,
    grade_06 = if(length(nums) >= 8) nums[8] else NA_real_,
    grade_07 = if(length(nums) >= 9) nums[9] else NA_real_,
    grade_08 = if(length(nums) >= 10) nums[10] else NA_real_,
    grade_09 = if(length(nums) >= 11) nums[11] else NA_real_,
    grade_10 = if(length(nums) >= 12) nums[12] else NA_real_,
    grade_11 = if(length(nums) >= 13) nums[13] else NA_real_,
    grade_12 = if(length(nums) >= 14) nums[14] else NA_real_,
    row_total = if(length(nums) >= 15) nums[15] else NA_real_,
    stringsAsFactors = FALSE
  )

  # If row_total is NA, calculate it
  if (is.na(result$row_total)) {
    grade_cols <- c("grade_pk", "grade_k", paste0("grade_", sprintf("%02d", 1:12)))
    result$row_total <- sum(result[, grade_cols], na.rm = TRUE)
  }

  result
}


#' Parse headcount enrollment PDF
#'
#' Parses the Headcount Enrollment PDF from WVDE.
#' These PDFs contain total headcount by county, sorted by enrollment.
#'
#' @param pdf_path Path to the PDF file
#' @param end_year School year for context
#' @return Data frame with headcount by county
#' @keywords internal
parse_headcount_pdf <- function(pdf_path, end_year) {

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required to parse WVDE PDFs. Install with: install.packages('pdftools')")
  }

  # Read PDF text
  text <- pdftools::pdf_text(pdf_path)

  # Parse all pages
  all_data <- list()

  for (page_text in text) {
    lines <- strsplit(page_text, "\n")[[1]]
    lines <- trimws(lines)

    for (line in lines) {
      parsed <- parse_headcount_line(line, end_year)
      if (!is.null(parsed)) {
        all_data <- c(all_data, list(parsed))
      }
    }
  }

  if (length(all_data) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(all_data)
}


#' Parse a single line from headcount enrollment PDF
#'
#' @param line Text line from PDF
#' @param end_year School year for context
#' @return Data frame row or NULL if line is not data
#' @keywords internal
parse_headcount_line <- function(line, end_year) {

  # Skip non-data lines
  skip_patterns <- c(
    "^\\s*$",
    "^SUMMARY",
    "^HEADCOUNT",
    "^WEST VIRGINIA",
    "^Page\\s+\\d",
    "^\\d{4}-\\d{2}",
    "^County",
    "^\\*",
    "^Note:",
    "^STATE TOTAL",
    "^TOTAL"
  )

  for (pattern in skip_patterns) {
    if (grepl(pattern, line, ignore.case = TRUE)) {
      return(NULL)
    }
  }

  # Headcount PDF format is typically: Rank | County | Enrollment
  # Or just: County | Enrollment

  parts <- strsplit(line, "\\s{2,}")[[1]]
  parts <- trimws(parts)
  parts <- parts[parts != ""]

  if (length(parts) < 2) return(NULL)

  # Try to identify county and enrollment
  county_name <- NULL
  enrollment <- NULL

  for (i in seq_along(parts)) {
    # Check if this part is a county name
    if (grepl("^[A-Za-z]+$", parts[i])) {
      county_name <- toupper(parts[i])
    }
    # Check if this is a number (enrollment)
    if (grepl("^[0-9,]+$", parts[i])) {
      enrollment <- safe_numeric(parts[i])
    }
  }

  if (is.null(county_name) || is.null(enrollment)) {
    return(NULL)
  }

  # Validate county name
  wv_counties <- c(
    "BARBOUR", "BERKELEY", "BOONE", "BRAXTON", "BROOKE",
    "CABELL", "CALHOUN", "CLAY", "DODDRIDGE", "FAYETTE",
    "GILMER", "GRANT", "GREENBRIER", "HAMPSHIRE", "HANCOCK",
    "HARDY", "HARRISON", "JACKSON", "JEFFERSON", "KANAWHA",
    "LEWIS", "LINCOLN", "LOGAN", "MCDOWELL", "MARION",
    "MARSHALL", "MASON", "MERCER", "MINERAL", "MINGO",
    "MONONGALIA", "MONROE", "MORGAN", "NICHOLAS", "OHIO",
    "PENDLETON", "PLEASANTS", "POCAHONTAS", "PRESTON", "PUTNAM",
    "RALEIGH", "RANDOLPH", "RITCHIE", "ROANE", "SUMMERS",
    "TAYLOR", "TUCKER", "TYLER", "UPSHUR", "WAYNE",
    "WEBSTER", "WETZEL", "WIRT", "WOOD", "WYOMING"
  )

  if (!county_name %in% wv_counties) {
    return(NULL)
  }

  data.frame(
    county_name = county_name,
    headcount = enrollment,
    stringsAsFactors = FALSE
  )
}


#' Create empty FTE data frame
#'
#' @return Empty data frame with FTE enrollment schema
#' @keywords internal
create_empty_fte_df <- function() {
  data.frame(
    county_name = character(),
    grade_pk = numeric(),
    grade_k = numeric(),
    grade_01 = numeric(),
    grade_02 = numeric(),
    grade_03 = numeric(),
    grade_04 = numeric(),
    grade_05 = numeric(),
    grade_06 = numeric(),
    grade_07 = numeric(),
    grade_08 = numeric(),
    grade_09 = numeric(),
    grade_10 = numeric(),
    grade_11 = numeric(),
    grade_12 = numeric(),
    row_total = numeric(),
    stringsAsFactors = FALSE
  )
}
