# ==============================================================================
# Utility Functions
# ==============================================================================
#
# This file contains utility functions for the wvschooldata package.
#
# ==============================================================================

#' @importFrom rlang .data
NULL


#' Get available years of West Virginia enrollment data
#'
#' Returns a vector of years for which enrollment data is available
#' from the West Virginia Department of Education (WVDE).
#'
#' Data comes from WVDE School Finance Data PDFs which are published
#' annually. Historical data availability depends on which years
#' WVDE has made available on their website.
#'
#' Note: For 2013, only headcount enrollment data is available (no grade-level
#' FTE data). The FTE PDFs for 2010-2013 are no longer available on the
#' WVDE website.
#'
#' @return Integer vector of available years (2013-2024)
#' @export
#' @examples
#' get_available_years()
get_available_years <- function() {
  # WVDE School Finance Data PDFs availability:
  # - 2013-2021, 2023-2024: Available
  # - 2022: PDF not published by WVDE
  # - 2025: PDF not yet available (check back later in school year)
  # - 2010-2012: No longer available on WVDE website
  #
  # Based on research of https://wvde.us/about-us/finance/school-finance/school-finance-data/
  #
  # Years represent the end of school year:
  # - 2024 = 2023-24 school year
  # - 2023 = 2022-23 school year
  # etc.
  c(2013L:2021L, 2023L:2024L)
}


#' Get list of West Virginia counties
#'
#' Returns a vector of all 55 West Virginia county names.
#' West Virginia has 55 counties, each with one county school district.
#'
#' @return Character vector of county names (uppercase)
#' @keywords internal
get_wv_counties <- function() {
  c(
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
}


#' Convert to numeric, handling suppression markers
#'
#' Handles various markers for suppressed data and formatting issues.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  if (is.numeric(x)) return(x)

  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  x[x %in% c("*", ".", "-", "-1", "-2", "-9", "<5", "N/A", "NA", "", "PS", "--")] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Format county name as district name
#'
#' Converts county name to standard district name format.
#' West Virginia districts are named "COUNTY COUNTY SCHOOLS".
#'
#' @param county_name County name (e.g., "KANAWHA")
#' @return District name (e.g., "KANAWHA COUNTY SCHOOLS")
#' @keywords internal
format_district_name <- function(county_name) {
  paste(toupper(county_name), "COUNTY SCHOOLS")
}


#' Generate district ID from county name
#'
#' Creates a standardized district ID based on county name.
#' Uses a simple two-letter abbreviation scheme.
#'
#' @param county_name County name
#' @return Character district ID
#' @keywords internal
generate_district_id <- function(county_name) {
  # Use FIPS-based ID: West Virginia = 54, then county codes
  # County FIPS codes are 001-109 (odd numbers only for WV)

  county_fips <- c(
    "BARBOUR" = "001", "BERKELEY" = "003", "BOONE" = "005",
    "BRAXTON" = "007", "BROOKE" = "009", "CABELL" = "011",
    "CALHOUN" = "013", "CLAY" = "015", "DODDRIDGE" = "017",
    "FAYETTE" = "019", "GILMER" = "021", "GRANT" = "023",
    "GREENBRIER" = "025", "HAMPSHIRE" = "027", "HANCOCK" = "029",
    "HARDY" = "031", "HARRISON" = "033", "JACKSON" = "035",
    "JEFFERSON" = "037", "KANAWHA" = "039", "LEWIS" = "041",
    "LINCOLN" = "043", "LOGAN" = "045", "MCDOWELL" = "047",
    "MARION" = "049", "MARSHALL" = "051", "MASON" = "053",
    "MERCER" = "055", "MINERAL" = "057", "MINGO" = "059",
    "MONONGALIA" = "061", "MONROE" = "063", "MORGAN" = "065",
    "NICHOLAS" = "067", "OHIO" = "069", "PENDLETON" = "071",
    "PLEASANTS" = "073", "POCAHONTAS" = "075", "PRESTON" = "077",
    "PUTNAM" = "079", "RALEIGH" = "081", "RANDOLPH" = "083",
    "RITCHIE" = "085", "ROANE" = "087", "SUMMERS" = "089",
    "TAYLOR" = "091", "TUCKER" = "093", "TYLER" = "095",
    "UPSHUR" = "097", "WAYNE" = "099", "WEBSTER" = "101",
    "WETZEL" = "103", "WIRT" = "105", "WOOD" = "107",
    "WYOMING" = "109"
  )

  county_upper <- toupper(county_name)
  if (county_upper %in% names(county_fips)) {
    paste0("54", county_fips[county_upper])
  } else {
    NA_character_
  }
}
