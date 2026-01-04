# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading school directory data from the
# West Virginia Department of Education (WVDE) website.
#
# Data sources:
# - School directory CSV: https://wveis.k12.wv.us/school-directory/download.php?dl
# - Superintendent list: https://static.k12.wv.us/school-directory/County-Superintendent-List.xlsx
#
# ==============================================================================

#' Fetch West Virginia school directory data
#'
#' Downloads and processes school directory data from the West Virginia
#' Department of Education. This includes all public and non-public schools
#' with contact information, addresses, and grade levels.
#'
#' @param tidy If TRUE (default), returns data in a standardized format with
#'   consistent column names. If FALSE, returns raw column names from WVDE.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from WVDE.
#' @param include_superintendents If TRUE, merges superintendent information
#'   from the separate WVDE superintendent list. Default is TRUE.
#' @return A tibble with school directory data. Columns include:
#'   \itemize{
#'     \item \code{state_school_id}: School identifier (type + county + name hash)
#'     \item \code{state_district_id}: County FIPS code (54XXX format)
#'     \item \code{school_name}: School name
#'     \item \code{district_name}: County school district name
#'     \item \code{school_type}: Type code (BOE, ELEM, MIDD, SECO, PRIM, K, NT)
#'     \item \code{grades_served}: Grade span (e.g., "K-5", "9-12")
#'     \item \code{address}: Street address
#'     \item \code{city}: City
#'     \item \code{state}: State (always "WV")
#'     \item \code{zip}: ZIP code (5-digit)
#'     \item \code{phone}: Phone number
#'     \item \code{fax}: Fax number
#'     \item \code{website}: School website URL
#'     \item \code{superintendent_name}: County superintendent name (if include_superintendents=TRUE)
#'     \item \code{superintendent_email}: County superintendent email (if include_superintendents=TRUE)
#'   }
#' @details
#' The directory data is downloaded as a CSV file from the WVEIS portal.
#' This data represents the current state of West Virginia schools and is
#' updated regularly by WVDE.
#'
#' School types:
#' \itemize{
#'   \item BOE: Board of Education (county central office)
#'   \item ELEM: Elementary school
#'   \item MIDD: Middle school
#'   \item SECO: Secondary/High school
#'   \item PRIM: Primary school
#'   \item K: Non-public/private school
#'   \item NT: Non-traditional (virtual, etc.)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get school directory data
#' dir_data <- fetch_directory()
#'
#' # Get raw format (original WVDE column names)
#' dir_raw <- fetch_directory(tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' dir_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Get only public schools
#' library(dplyr)
#' public_schools <- dir_data |>
#'   filter(!school_type %in% c("K", "BOE"))
#'
#' # Find all schools in a county
#' kanawha_schools <- dir_data |>
#'   filter(district_name == "Kanawha County Schools")
#' }
fetch_directory <- function(tidy = TRUE, use_cache = TRUE, include_superintendents = TRUE) {

  # Determine cache type based on parameters
  cache_type <- if (tidy) {
    if (include_superintendents) "directory_tidy_supt" else "directory_tidy"
  } else {
    "directory_raw"
  }

  # Check cache first
  if (use_cache && cache_exists_directory(cache_type)) {
    message("Using cached school directory data")
    return(read_cache_directory(cache_type))
  }

  # Get raw data from WVDE
  raw <- get_raw_directory()

  # Process to standard schema
  if (tidy) {
    result <- process_directory(raw)

    # Merge superintendent data if requested
    if (include_superintendents) {
      supt_data <- get_superintendent_data()
      if (!is.null(supt_data)) {
        result <- merge_superintendent_data(result, supt_data)
      }
    }
  } else {
    result <- raw
  }

  # Cache the result
  if (use_cache) {
    write_cache_directory(result, cache_type)
  }

  result
}


#' Get raw school directory data from WVDE
#'
#' Downloads the raw school directory CSV file from the West Virginia
#' Department of Education WVEIS portal.
#'
#' @return Raw data frame as downloaded from WVDE
#' @keywords internal
get_raw_directory <- function() {

  url <- "https://wveis.k12.wv.us/school-directory/download.php?dl"

  message("Downloading school directory data from WVDE...")

  # Download CSV file
  # Use longer timeout and retry logic for WVEIS server
  # Note: WVEIS server can be slow/unresponsive; use curl for better timeout control
  old_timeout <- getOption("timeout")
  options(timeout = 120)

  response <- tryCatch({
    httr::GET(
      url,
      httr::config(connecttimeout = 60, timeout = 120),
      httr::user_agent("wvschooldata R package")
    )
  }, error = function(e) {
    message("First attempt failed, retrying...")
    Sys.sleep(3)
    httr::GET(
      url,
      httr::config(connecttimeout = 60, timeout = 120),
      httr::user_agent("wvschooldata R package")
    )
  }, finally = {
    options(timeout = old_timeout)
  })

  if (httr::http_error(response)) {
    stop(paste("Failed to download school directory data from WVDE:",
               httr::status_code(response)))
  }

  # Parse CSV content
  content <- httr::content(response, as = "text", encoding = "UTF-8")

  # Read CSV - file uses quotes as text delimiters

  df <- readr::read_csv(
    content,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  message(paste("Loaded", nrow(df), "records"))

  dplyr::as_tibble(df)
}


#' Get superintendent data from WVDE
#'
#' Downloads and parses the county superintendent list Excel file
#' from the WVDE website.
#'
#' @return Data frame with superintendent info by county, or NULL if unavailable
#' @keywords internal
get_superintendent_data <- function() {

  url <- "https://static.k12.wv.us/school-directory/County-Superintendent-List.xlsx"

  # Download to temp file
  tname <- tempfile(pattern = "wv_supt", tmpdir = tempdir(), fileext = ".xlsx")

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(120)
    )

    if (httr::http_error(response)) {
      message("Note: Could not download superintendent list (HTTP ",
              httr::status_code(response), ")")
      return(NULL)
    }

    # Check file size
    if (file.info(tname)$size < 1000) {
      message("Note: Superintendent file too small, may be error page")
      return(NULL)
    }

    # Read Excel file - first row is header
    if (!requireNamespace("readxl", quietly = TRUE)) {
      message("Note: Package 'readxl' required for superintendent data")
      return(NULL)
    }

    raw <- readxl::read_excel(
      tname,
      col_types = "text",
      .name_repair = "unique"
    )

    # The Excel file has a complex header - first row contains column names
    # Columns are: County, Dial, Phone, FAX, Email, Title, Superintendent, Address, City, Zip
    # The actual header is in row 1

    # Find header row
    if (nrow(raw) < 2) {
      message("Note: Superintendent file appears empty")
      return(NULL)
    }

    # Check if first row looks like header
    first_row <- as.character(raw[1, ])
    if (any(grepl("county|superintendent|email", tolower(first_row), ignore.case = TRUE))) {
      # Use first row as header
      colnames(raw) <- make.names(first_row, unique = TRUE)
      raw <- raw[-1, ]
    }

    # Standardize column names
    names(raw) <- tolower(gsub("[^a-zA-Z0-9]", "_", names(raw)))
    names(raw) <- gsub("_+", "_", names(raw))
    names(raw) <- gsub("^_|_$", "", names(raw))

    # Find relevant columns
    county_col <- names(raw)[grep("county", names(raw), ignore.case = TRUE)[1]]
    supt_col <- names(raw)[grep("superintendent", names(raw), ignore.case = TRUE)[1]]
    email_col <- names(raw)[grep("e_mail|email", names(raw), ignore.case = TRUE)[1]]
    title_col <- names(raw)[grep("title", names(raw), ignore.case = TRUE)[1]]

    if (is.na(county_col) || is.na(supt_col)) {
      message("Note: Could not identify superintendent columns")
      return(NULL)
    }

    # Build result
    result <- dplyr::tibble(
      county = trimws(raw[[county_col]]),
      superintendent_name = if (!is.na(supt_col)) {
        # Combine title and name if title exists
        if (!is.na(title_col)) {
          paste(trimws(raw[[title_col]]), trimws(raw[[supt_col]]))
        } else {
          trimws(raw[[supt_col]])
        }
      } else {
        NA_character_
      },
      superintendent_email = if (!is.na(email_col)) trimws(raw[[email_col]]) else NA_character_
    )

    # Clean up name - remove extra spaces
    result$superintendent_name <- gsub("\\s+", " ", result$superintendent_name)
    result$superintendent_name <- trimws(result$superintendent_name)

    # Remove empty rows
    result <- result |>
      dplyr::filter(!is.na(county) & county != "")

    message(paste("Loaded", nrow(result), "superintendent records"))

    result

  }, error = function(e) {
    message(paste("Note: Could not load superintendent data:", e$message))
    return(NULL)
  })
}


#' Process raw school directory data to standard schema
#'
#' Takes raw school directory data from WVDE and standardizes column names,
#' types, and adds derived columns.
#'
#' @param raw_data Raw data frame from get_raw_directory()
#' @return Processed data frame with standard schema
#' @keywords internal
process_directory <- function(raw_data) {

  # Expected columns from WVDE CSV:
  # type, county, name, address, city, zip, zip4, phone, fax, url, grades

  result <- dplyr::tibble(
    # School type code
    school_type = trimws(raw_data$type),

    # County/district info
    county = trimws(raw_data$county),
    district_name = paste(
      stringr::str_to_title(trimws(raw_data$county)),
      "County Schools"
    ),

    # Generate district ID using FIPS code
    state_district_id = purrr::map_chr(
      toupper(trimws(raw_data$county)),
      generate_district_id
    ),

    # School name
    school_name = trimws(raw_data$name),

    # Generate school ID (type + county abbreviation + hash of name)
    state_school_id = paste0(
      trimws(raw_data$type), "-",
      substr(toupper(trimws(raw_data$county)), 1, 3), "-",
      sprintf("%04d", as.integer(abs(sapply(raw_data$name, function(x) {
        sum(utf8ToInt(as.character(x))) %% 10000
      }))))
    ),

    # Grades served
    grades_served = trimws(raw_data$grades),

    # Address fields
    address = trimws(raw_data$address),
    city = trimws(raw_data$city),
    state = "WV",

    # ZIP code - combine zip and zip4 if zip4 is not "0000"
    zip = dplyr::case_when(
      is.na(raw_data$zip4) | raw_data$zip4 == "" | raw_data$zip4 == "0000" ~
        trimws(raw_data$zip),
      TRUE ~ paste0(trimws(raw_data$zip), "-", trimws(raw_data$zip4))
    ),

    # Contact info
    phone = trimws(raw_data$phone),
    fax = trimws(raw_data$fax),
    website = trimws(raw_data$url)
  )

  # Clean up empty strings to NA
  result <- result |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ dplyr::if_else(.x == "", NA_character_, .x)
      )
    )

  # Clean up district name for BOE rows (they are the district, not a school)
  result <- result |>
    dplyr::mutate(
      # For BOE entries, school_name is the district name
      school_name = dplyr::if_else(
        school_type == "BOE",
        paste(district_name, "(Central Office)"),
        school_name
      )
    )

  # Reorder columns for consistency
  result |>
    dplyr::select(
      state_school_id,
      state_district_id,
      school_name,
      district_name,
      county,
      school_type,
      grades_served,
      address,
      city,
      state,
      zip,
      phone,
      fax,
      website,
      dplyr::everything()
    )
}


#' Merge superintendent data with school directory
#'
#' Joins superintendent information to the school directory by county.
#'
#' @param directory_data Processed directory data
#' @param supt_data Superintendent data from get_superintendent_data()
#' @return Directory data with superintendent columns added
#' @keywords internal
merge_superintendent_data <- function(directory_data, supt_data) {

  # Standardize county names for join
  supt_data <- supt_data |>
    dplyr::mutate(
      county_join = toupper(trimws(county))
    )

  directory_data <- directory_data |>
    dplyr::mutate(
      county_join = toupper(trimws(county))
    )

  # Left join to add superintendent info
  result <- directory_data |>
    dplyr::left_join(
      supt_data |> dplyr::select(county_join, superintendent_name, superintendent_email),
      by = "county_join"
    ) |>
    dplyr::select(-county_join)

  result
}


# ==============================================================================
# Directory-specific cache functions
# ==============================================================================

#' Build cache file path for directory data
#'
#' @param cache_type Type of cache ("directory_tidy", "directory_raw", etc.)
#' @return File path string
#' @keywords internal
build_cache_path_directory <- function(cache_type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(cache_type, ".rds"))
}


#' Check if cached directory data exists
#'
#' @param cache_type Type of cache
#' @param max_age Maximum age in days (default 30). Set to Inf to ignore age.
#' @return Logical indicating if valid cache exists
#' @keywords internal
cache_exists_directory <- function(cache_type, max_age = 30) {
  cache_path <- build_cache_path_directory(cache_type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param cache_type Type of cache
#' @return Cached data frame
#' @keywords internal
read_cache_directory <- function(cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param data Data frame to cache
#' @param cache_type Type of cache
#' @return Invisibly returns the cache path
#' @keywords internal
write_cache_directory <- function(data, cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  cache_dir <- dirname(cache_path)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  saveRDS(data, cache_path)
  invisible(cache_path)
}


#' Clear school directory cache
#'
#' Removes cached school directory data files.
#'
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear cached directory data
#' clear_directory_cache()
#' }
clear_directory_cache <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist")
    return(invisible(0))
  }

  files <- list.files(cache_dir, pattern = "^directory_", full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}
