## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5,
  eval = FALSE
)

## ----load-packages------------------------------------------------------------
# library(wvschooldata)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# 
# theme_set(theme_minimal(base_size = 14))

## ----statewide-data-----------------------------------------------------------
# enr <- fetch_enr_multi(2014:2025)
# 
# state_totals <- enr |>
#   filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   select(end_year, n_students) |>
#   mutate(change = n_students - lag(n_students),
#          pct_change = round(change / lag(n_students) * 100, 2))
# 
# state_totals

## ----statewide-chart----------------------------------------------------------
# ggplot(state_totals, aes(x = end_year, y = n_students)) +
#   geom_line(linewidth = 1.2, color = "#002855") +
#   geom_point(size = 3, color = "#002855") +
#   scale_y_continuous(labels = scales::comma,
#                      limits = c(0, NA)) +
#   labs(
#     title = "West Virginia Public School Enrollment (2014-2025)",
#     subtitle = "The Mountain State has seen consistent enrollment decline",
#     x = "School Year (ending)",
#     y = "Total Enrollment"
#   )

## ----top-districts-data-------------------------------------------------------
# enr_2025 <- fetch_enr(2025)
# 
# top_10 <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   arrange(desc(n_students)) |>
#   head(10) |>
#   select(district_name, county, n_students)
# 
# top_10

## ----top-districts-chart------------------------------------------------------
# top_10 |>
#   mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
#   ggplot(aes(x = n_students, y = district_name)) +
#   geom_col(fill = "#002855") +
#   scale_x_continuous(labels = scales::comma) +
#   labs(
#     title = "West Virginia's 10 Largest School Districts (2025)",
#     subtitle = "Kanawha County leads, but no district exceeds 30,000 students",
#     x = "Total Enrollment",
#     y = NULL
#   )

## ----demographics-data--------------------------------------------------------
# # Since WV data lacks demographic breakdowns, we'll analyze district size distribution
# size_distribution <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   mutate(size_category = case_when(
#     n_students < 1000 ~ "Under 1,000",
#     n_students < 2500 ~ "1,000-2,499",
#     n_students < 5000 ~ "2,500-4,999",
#     n_students < 10000 ~ "5,000-9,999",
#     TRUE ~ "10,000+"
#   )) |>
#   mutate(size_category = factor(size_category,
#                                  levels = c("Under 1,000", "1,000-2,499",
#                                            "2,500-4,999", "5,000-9,999", "10,000+"))) |>
#   group_by(size_category) |>
#   summarize(
#     n_districts = n(),
#     total_students = sum(n_students, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# size_distribution

## ----demographics-chart-------------------------------------------------------
# size_distribution |>
#   ggplot(aes(x = size_category, y = n_districts, fill = size_category)) +
#   geom_col(show.legend = FALSE) +
#   geom_text(aes(label = n_districts), vjust = -0.5) +
#   scale_fill_brewer(palette = "Blues") +
#   labs(
#     title = "West Virginia Districts by Size (2025)",
#     subtitle = "Most counties have small student populations",
#     x = "District Size (students)",
#     y = "Number of Districts"
#   )

## ----regional-data------------------------------------------------------------
# # Eastern Panhandle counties (DC suburbs)
# panhandle <- c("BERKELEY", "JEFFERSON", "MORGAN")
# 
# regional_comparison <- enr |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   mutate(region = case_when(
#     county %in% panhandle ~ "Eastern Panhandle",
#     TRUE ~ "Rest of State"
#   )) |>
#   group_by(end_year, region) |>
#   summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop")
# 
# regional_comparison |>
#   pivot_wider(names_from = region, values_from = n_students)

## ----regional-chart-----------------------------------------------------------
# regional_indexed <- regional_comparison |>
#   group_by(region) |>
#   mutate(index = n_students / first(n_students) * 100)
# 
# ggplot(regional_indexed, aes(x = end_year, y = index, color = region)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2) +
#   geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5) +
#   scale_color_manual(values = c("Eastern Panhandle" = "#4CAF50",
#                                  "Rest of State" = "#F44336")) +
#   labs(
#     title = "Enrollment Trends: Eastern Panhandle vs. Rest of State",
#     subtitle = "Indexed to 2014 = 100",
#     x = "School Year",
#     y = "Enrollment Index",
#     color = NULL
#   ) +
#   theme(legend.position = "bottom")

## ----growth-data--------------------------------------------------------------
# # Coal counties
# coal_counties <- c("MCDOWELL", "WYOMING", "MINGO", "LOGAN", "BOONE")
# 
# growth_analysis <- enr |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
#          end_year %in% c(2014, 2025)) |>
#   group_by(district_name, county) |>
#   filter(n() == 2) |>
#   summarize(
#     y2014 = n_students[end_year == 2014],
#     y2025 = n_students[end_year == 2025],
#     change = y2025 - y2014,
#     pct_change = round((y2025 / y2014 - 1) * 100, 1),
#     .groups = "drop"
#   ) |>
#   arrange(pct_change)
# 
# # Show the most declining districts
# declining_10 <- head(growth_analysis, 10)
# declining_10

## ----growth-chart-------------------------------------------------------------
# declining_10 |>
#   mutate(county = forcats::fct_reorder(county, pct_change)) |>
#   ggplot(aes(x = pct_change, y = county)) +
#   geom_col(fill = "#B71C1C") +
#   geom_text(aes(label = paste0(pct_change, "%")), hjust = 1.1, color = "white") +
#   labs(
#     title = "Fastest-Declining West Virginia Counties (2014-2025)",
#     subtitle = "Coal country leads population loss",
#     x = "Percent Change",
#     y = NULL
#   )

## ----mcdowell-----------------------------------------------------------------
# mcdowell_trend <- enr |>
#   filter(is_district, county == "MCDOWELL",
#          subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   select(end_year, n_students) |>
#   mutate(pct_of_2014 = round(n_students / first(n_students) * 100, 1))
# 
# mcdowell_trend

## ----grade-level--------------------------------------------------------------
# grade_trends <- enr |>
#   filter(is_state, subgroup == "total_enrollment",
#          grade_level %in% c("K", "05", "09", "12")) |>
#   select(end_year, grade_level, n_students) |>
#   pivot_wider(names_from = grade_level, values_from = n_students)
# 
# grade_trends

## ----growing------------------------------------------------------------------
# growing <- growth_analysis |>
#   filter(pct_change >= 0) |>
#   arrange(desc(pct_change))
# 
# growing

## ----kindergarten-------------------------------------------------------------
# k_trend <- enr |>
#   filter(is_state, subgroup == "total_enrollment",
#          grade_level == "K") |>
#   select(end_year, n_students) |>
#   mutate(change = n_students - lag(n_students))
# 
# k_trend

## ----smallest-----------------------------------------------------------------
# smallest <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   arrange(n_students) |>
#   head(10) |>
#   select(district_name, county, n_students)
# 
# smallest

