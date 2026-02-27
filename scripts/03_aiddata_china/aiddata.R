# =============================================================================
# AidData CLG-Global 1.0: Western Balkans Exploration
# =============================================================================

library(readxl)
library(data.table)
library(dplyr)
library(tidyr)


# =============================================================================
# 1. LOAD DATA
# =============================================================================

path <- "C:/Users/ivana/Downloads/AidDatas_CLG_Global_Dataset_v1.0.xlsx"

excel_sheets(path)  # inspect sheet names

global <- as.data.table(read_excel(path, sheet = 5))


# =============================================================================
# 2. FILTER TO WESTERN BALKANS
# =============================================================================

balkan_countries <- c(
  "Serbia",
  "Albania",
  "Bosnia and Herzegovina",
  "Montenegro",
  "North Macedonia"
)

balkan <- global[Country_of_Activity %in% balkan_countries]
serbia <- global[Country_of_Activity == "Serbia"]


# =============================================================================
# 3. STATUS SUMMARIES
# =============================================================================

# --- 3a. Status by country and year (long format) ---
status_summary <- balkan %>%
  group_by(Country_of_Activity, Commitment_Year, Status) %>%
  summarise(n_projects = n(), .groups = "drop") %>%
  arrange(Country_of_Activity, Commitment_Year, Status)

print(status_summary)

# --- 3b. Status by country and year (wide format — easier to scan) ---
status_wide <- balkan %>%
  group_by(Country_of_Activity, Commitment_Year, Status) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Status, values_from = n, values_fill = 0) %>%
  arrange(Country_of_Activity, Commitment_Year)

print(status_wide)

# --- 3c. Status by country only (collapsing years — quickest overview) ---
status_by_country <- balkan %>%
  group_by(Country_of_Activity, Status) %>%
  summarise(n_projects = n(), .groups = "drop") %>%
  pivot_wider(names_from = Status, values_from = n_projects, values_fill = 0)

print(status_by_country)

# --- 3d. Recommended_for_Aggregates filter check ---
# Shows how many records would be dropped by AidData's recommended filter
balkan %>%
  group_by(Country_of_Activity, Recommended_for_Aggregates) %>%
  summarise(n = n(), .groups = "drop")


# =============================================================================
# Chinese Project Dates: Exploration and Visualization
# =============================================================================


# =============================================================================
# 1. SETUP
# =============================================================================

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

# Countries of interest
balkans <- c("Serbia", "Albania", "Bosnia and Herzegovina",
             "Kosovo", "North Macedonia", "Montenegro")

# Parse dates once, reuse throughout
balkan_dates <- balkan %>%
  filter(Country_of_Activity %in% balkans) %>%
  mutate(
    commit_date  = mdy(Commitment_Date),
    impl_date    = mdy(Actual_Implementation_Start_Date),
    complet_date = mdy(Actual_Completion_Date),
    commit_year  = year(commit_date),
    commit_mon   = month(commit_date, label = TRUE),
    commit_month = floor_date(commit_date, "month")
  )


# =============================================================================
# 2. COVERAGE CHECK
# =============================================================================

balkan_dates %>%
  summarise(
    n                  = n(),
    pct_commit_date    = round(mean(!is.na(commit_date))  * 100, 1),
    pct_impl_planned   = round(mean(!is.na(mdy(Planned_Implementation_Start_Date))) * 100, 1),
    pct_impl_actual    = round(mean(!is.na(impl_date))    * 100, 1),
    pct_complet_actual = round(mean(!is.na(complet_date)) * 100, 1)
  )

# Missing implementation date by project status
balkan_dates %>%
  mutate(has_impl_date = !is.na(impl_date)) %>%
  count(Status, has_impl_date) %>%
  pivot_wider(names_from  = has_impl_date,
              values_from = n,
              values_fill = 0) %>%
  rename(missing = `FALSE`, has_date = `TRUE`)


# =============================================================================
# 3. VISUALIZATIONS
# =============================================================================

# --- 3a. Commitments by month, all countries combined ---
balkan_dates %>%
  filter(!is.na(commit_month)) %>%
  count(commit_month) %>%
  ggplot(aes(x = commit_month, y = n)) +
  geom_col(fill = "gray30") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  labs(x = NULL, y = "Number of projects",
       title = "Chinese project commitments by month (all countries)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --- 3b. Commitments by year, faceted by country ---
balkan_dates %>%
  filter(!is.na(commit_year)) %>%
  count(Country_of_Activity, commit_year) %>%
  ggplot(aes(x = commit_year, y = n)) +
  geom_col(fill = "gray30") +
  facet_wrap(~Country_of_Activity, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(1998, 2024, by = 4)) +
  theme_minimal() +
  labs(x = NULL, y = "Number of projects",
       title = "Chinese project commitments by year and country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --- 3c. Heatmap: month x year, faceted by country (2015 onwards) ---
# Best for showing monthly granularity clearly
balkan_dates %>%
  filter(!is.na(commit_date), commit_year >= 2015) %>%
  count(Country_of_Activity, commit_year, commit_mon) %>%
  ggplot(aes(x = commit_mon, y = factor(commit_year), fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~Country_of_Activity, ncol = 2) +
  scale_fill_gradient(low = "gray90", high = "gray10") +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "N projects",
       title = "Chinese project commitments by month and year (2015+)") +
  theme(strip.text = element_text(face = "bold", hjust = 0))


# --- 3d. Bar chart by month, faceted by country (2015 onwards) ---
# Alternative to heatmap if you prefer bars
balkan_dates %>%
  filter(!is.na(commit_month), commit_month >= "2015-01-01") %>%
  count(Country_of_Activity, commit_month) %>%
  ggplot(aes(x = commit_month, y = n)) +
  geom_col(fill = "gray30", width = 20) +
  facet_wrap(~Country_of_Activity, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  labs(x = NULL, y = "N projects",
       title = "Chinese project commitments by month (2015+)") +
  theme(strip.text       = element_text(face = "bold", hjust = 0),
        panel.spacing    = unit(0.5, "lines"),
        axis.text.x      = element_text(angle = 45, hjust = 1))