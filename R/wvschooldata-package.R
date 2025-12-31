#' wvschooldata: Fetch and Process West Virginia School Data
#'
#' Downloads and processes school enrollment data from the West Virginia
#' Department of Education (WVDE). Provides functions for fetching Fall
#' Membership (2nd month) enrollment data from WVDE School Finance Data PDFs
#' and transforming it into tidy format for analysis.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{fetch_enr}}}{Fetch enrollment data for a school year}
#'   \item{\code{\link{fetch_enr_multi}}}{Fetch enrollment data for multiple years}
#'   \item{\code{\link{tidy_enr}}}{Transform wide data to tidy (long) format}
#'   \item{\code{\link{id_enr_aggs}}}{Add aggregation level flags}
#'   \item{\code{\link{enr_grade_aggs}}}{Create grade-level aggregations}
#'   \item{\code{\link{get_available_years}}}{Get list of available data years}
#' }
#'
#' @section Cache functions:
#' \describe{
#'   \item{\code{\link{cache_status}}}{View cached data files}
#'   \item{\code{\link{clear_cache}}}{Remove cached data files}
#' }
#'
#' @section ID System:
#' West Virginia uses FIPS-based district IDs:
#' \itemize{
#'   \item State FIPS: 54 (West Virginia)
#'   \item District IDs: 5-digit codes (state FIPS + 3-digit county FIPS)
#'   \item Example: 54039 = Kanawha County Schools
#' }
#'
#' West Virginia has 55 county school districts (one per county).
#'
#' @section Data Sources:
#' Data is sourced from:
#' \itemize{
#'   \item WVDE School Finance Data: \url{https://wvde.us/about-us/finance/school-finance/school-finance-data/}
#'   \item FTE Enrollment by Grade PDFs (2nd month enrollment)
#'   \item Headcount Enrollment PDFs (2nd month enrollment)
#' }
#'
#' @section Data Availability:
#' \itemize{
#'   \item Years: 2014-2025 (12 years)
#'   \item Aggregation levels: State, District (County)
#'   \item Grade levels: PK through 12
#'   \item Note: School-level and demographic data not available from PDFs
#' }
#'
#' @section Data Limitations:
#' The WVDE School Finance Data PDFs provide:
#' \itemize{
#'   \item FTE (Full-Time Equivalent) enrollment by grade
#'   \item Headcount enrollment totals
#'   \item County-level aggregation only (no individual schools)
#' }
#'
#' For school-level or demographic data, see ZoomWV: \url{https://zoomwv.k12.wv.us/}
#'
#' @docType package
#' @name wvschooldata-package
#' @aliases wvschooldata
#' @keywords internal
"_PACKAGE"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
