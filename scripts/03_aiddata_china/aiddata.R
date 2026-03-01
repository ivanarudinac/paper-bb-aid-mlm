# =============================================================================
# AidData 
# =============================================================================
# Datasets:
#   1. AidData's Global Chinese Development Finance Dataset, v3.0      [Excel]
#   2. AidData's Geospatial Global Chinese Development Finance, v3.0   [GeoPackage]
#   3. China's Global Loans and Grants Dataset, v1.0                   [Excel]
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)
library(openxlsx)
library(purrr)
library(scales)


# =============================================================================
# 1. PATHS & CONSTANTS
# =============================================================================

data_dir <- "C:/Users/ivana/Documents/GitHub/paper-bb-aid-mlm/data/raw/aid data"
fig_dir  <- "C:/Users/ivana/Documents/GitHub/paper-bb-aid-mlm/outputs/figures/descriptives"
out_dir  <- "C:/Users/ivana/Documents/GitHub/paper-bb-aid-mlm/outputs/tables/descriptives"
int_dir  <- "C:/Users/ivana/Documents/GitHub/paper-bb-aid-mlm/data/intermediate"

path_gcdf <- file.path(data_dir, "AidDatasGlobalChineseDevelopmentFinanceDataset_v3.0.xlsx")
path_geo  <- file.path(data_dir, "all_combined_global.gpkg")
path_clg  <- file.path(data_dir, "AidDatas_CLG_Global_Dataset_v1.0.xlsx")

balkan_countries <- c(
  "Albania",
  "Bosnia and Herzegovina",
  "Montenegro",
  "North Macedonia",
  "Serbia"
)


# =============================================================================
# 2. LOAD DATASETS
# =============================================================================

# --- 2a. Global Chinese Development Finance Dataset, v3.0 (Excel) ---
excel_sheets(path_gcdf)
gcdf_global <- as.data.table(read_excel(path_gcdf, sheet = "GCDF_3.0"))
gcdf_balkan <- gcdf_global[Recipient %in% balkan_countries]

# --- 2b. Geospatial Global Chinese Development Finance Dataset, v3.0 (GeoPackage) ---
geo_global <- st_read(path_geo)
geo_balkan <- geo_global[geo_global$Recipient %in% balkan_countries, ]

# --- 2c. China's Global Loans and Grants Dataset, v1.0 (Excel) ---
excel_sheets(path_clg)
clg_global <- as.data.table(read_excel(path_clg, sheet = "CLG-Global 1.0_Records"))
clg_balkan <- clg_global[Country_of_Activity %in% balkan_countries]


# =============================================================================
# 3. DATASET OVERVIEW & COVERAGE
# =============================================================================

# --- 3a. Dimensions ---
overview <- tibble(
  dataset  = c("GCDF v3.0", "Geo GCDF v3.0", "CLG v1.0"),
  n_global = c(nrow(gcdf_global), nrow(geo_global), nrow(clg_global)),
  n_balkan = c(nrow(gcdf_balkan), nrow(geo_balkan), nrow(clg_balkan)),
  n_cols   = c(ncol(gcdf_global), ncol(geo_global), ncol(clg_global))
)
print(overview)

# --- 3b. Column names: shared vs. unique ---
cols <- list(
  gcdf = names(gcdf_global),
  geo  = names(geo_global),
  clg  = names(clg_global)
)

intersect(cols$gcdf, cols$clg)    # shared between GCDF and CLG
setdiff(cols$gcdf,   cols$clg)    # in GCDF only
setdiff(cols$clg,    cols$gcdf)   # in CLG only

# --- 3c. Project counts per country per dataset ---
bind_rows(
  gcdf_balkan %>% count(Recipient,           name = "n") %>% mutate(dataset = "GCDF"),
  as.data.frame(geo_balkan) %>% count(Recipient, name = "n") %>% mutate(dataset = "Geo"),
  clg_balkan  %>% count(Country_of_Activity, name = "n") %>% mutate(dataset = "CLG") %>%
    rename(Recipient = Country_of_Activity)
) %>%
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
  arrange(Recipient)

# --- 3d. Key variable completeness (% non-missing) ---
check_coverage <- function(df, vars, label) {
  df %>%
    summarise(across(any_of(vars), ~ round(mean(!is.na(.)) * 100, 1))) %>%
    mutate(dataset = label) %>%
    relocate(dataset)
}

shared_vars <- c("Commitment_Year", "Amount_Constant_USD_2021", "Status",
                 "Sector", "Flow_Type", "Recipient")

bind_rows(
  check_coverage(gcdf_balkan, shared_vars, "GCDF"),
  check_coverage(clg_balkan,  shared_vars, "CLG")
)

# --- 3e. CLG status summaries ---
status_summary <- clg_balkan %>%
  group_by(Country_of_Activity, Commitment_Year, Status) %>%
  summarise(n_projects = n(), .groups = "drop") %>%
  arrange(Country_of_Activity, Commitment_Year, Status)
print(status_summary)

status_wide <- clg_balkan %>%
  group_by(Country_of_Activity, Commitment_Year, Status) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Status, values_from = n, values_fill = 0) %>%
  arrange(Country_of_Activity, Commitment_Year)
print(status_wide)

status_by_country <- clg_balkan %>%
  group_by(Country_of_Activity, Status) %>%
  summarise(n_projects = n(), .groups = "drop") %>%
  pivot_wider(names_from = Status, values_from = n_projects, values_fill = 0)
print(status_by_country)

clg_balkan %>%
  group_by(Country_of_Activity, Recommended_for_Aggregates) %>%
  summarise(n = n(), .groups = "drop")


# =============================================================================
# 4. MISSING VALUES — per variable, per country
# =============================================================================

missing_by_country <- function(df, country_col) {
  countries_list <- sort(unique(df[[country_col]]))
  result <- map_dfc(countries_list, function(ctry) {
    sub         <- df[df[[country_col]] == ctry, ]
    pct_missing <- sapply(sub, function(x) round(mean(is.na(x)) * 100, 1))
    tibble(!!ctry := pct_missing)
  })
  bind_cols(tibble(Variable = names(df)), result)
}

gcdf_missing <- missing_by_country(as.data.frame(gcdf_balkan),   "Recipient")
clg_missing  <- missing_by_country(as.data.frame(clg_balkan),    "Country_of_Activity")
geo_missing  <- missing_by_country(st_drop_geometry(geo_balkan), "Recipient")


# =============================================================================
# 5. EXPORT MISSING VALUES TO EXCEL
# =============================================================================

wb <- createWorkbook()

header_style <- createStyle(
  fontName = "Arial", fontSize = 10, fontColour = "white",
  fgFill   = "#2F4F6F", textDecoration = "bold",
  halign   = "center", wrapText = TRUE
)
high_miss_style <- createStyle(fontColour = "#CC0000", textDecoration = "bold")

format_sheet <- function(wb, sheet_name, df) {
  addStyle(wb, sheet_name, header_style,
           rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  for (col_idx in 2:ncol(df)) {
    high_rows <- which(df[[col_idx]] > 50) + 1
    if (length(high_rows) > 0)
      addStyle(wb, sheet_name, high_miss_style,
               rows = high_rows, cols = col_idx, gridExpand = TRUE)
  }
  setColWidths(wb, sheet_name, cols = 1,          widths = 35)
  setColWidths(wb, sheet_name, cols = 2:ncol(df), widths = 18)
  freezePane(wb,  sheet_name, firstRow = TRUE, firstCol = TRUE)
}

addWorksheet(wb, "GCDF v3.0");     writeData(wb, "GCDF v3.0",     gcdf_missing); format_sheet(wb, "GCDF v3.0",     gcdf_missing)
addWorksheet(wb, "CLG v1.0");      writeData(wb, "CLG v1.0",      clg_missing);  format_sheet(wb, "CLG v1.0",      clg_missing)
addWorksheet(wb, "Geo GCDF v3.0"); writeData(wb, "Geo GCDF v3.0", geo_missing);  format_sheet(wb, "Geo GCDF v3.0", geo_missing)

saveWorkbook(wb, file.path(out_dir, "AidData overview.xlsx"), overwrite = TRUE)
message("Saved: ", file.path(out_dir, "AidData overview.xlsx"))


# =============================================================================
# 6. DATASET SUBSETS
# =============================================================================

# --- 6a. CLG projects not in GCDF ---
clg_only <- anti_join(clg_balkan, gcdf_balkan,
                      by = c("AidData_Record_ID" = "AidData Record ID"))

# --- 6b. Check ID column names before joining GCDF <-> Geo ---
grep("AidData|ID|id", names(gcdf_balkan), value = TRUE)
grep("AidData|ID|id", names(geo_balkan),  value = TRUE)

# --- 6c. GCDF projects NOT in Geo dataset ---
gcdf_not_in_geo <- anti_join(as.data.frame(gcdf_balkan), st_drop_geometry(geo_balkan),
                             by = c("AidData Record ID" = "id"))

gcdf_not_in_geo %>%
  count(Recipient, `Commitment Year`) %>%
  pivot_wider(names_from = Recipient, values_from = n, values_fill = 0) %>%
  arrange(`Commitment Year`)

gcdf_not_in_geo %>% count(Status,        sort = TRUE)
gcdf_not_in_geo %>% count(`Flow Type`,   sort = TRUE)
gcdf_not_in_geo %>% count(`Flow Class`,  sort = TRUE)
gcdf_not_in_geo %>% count(`Sector Name`, sort = TRUE)

# --- 6d. GCDF projects IN Geo dataset ---
gcdf_in_geo <- semi_join(as.data.frame(gcdf_balkan), st_drop_geometry(geo_balkan),
                         by = c("AidData Record ID" = "id"))


# =============================================================================
# 7. PARSE DATES (geo_balkan)
# =============================================================================

geo_balkan_dates <- geo_balkan %>%
  st_drop_geometry() %>%
  mutate(
    commit_date = ymd(Commitment.Date..MM.DD.YYYY.),
    start_date  = ymd(Actual.Implementation.Start.Date..MM.DD.YYYY.),
    end_date    = ymd(Actual.Completion.Date..MM.DD.YYYY.),
    commit_year = year(commit_date)
  )

countries <- sort(unique(geo_balkan_dates$Recipient))


# =============================================================================
# 8. ACADEMIC THEME & PALETTES
# =============================================================================

theme_academic <- function() {
  theme_classic(base_family = "serif", base_size = 11) +
    theme(
      axis.text        = element_text(size = 9, colour = "black"),
      axis.title       = element_text(size = 10),
      axis.line        = element_line(colour = "black", linewidth = 0.4),
      axis.ticks       = element_line(colour = "black", linewidth = 0.4),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 9),
      legend.key.size  = unit(0.4, "cm"),
      strip.text       = element_text(size = 9, face = "bold", hjust = 0),
      strip.background = element_blank(),
      plot.title       = element_blank(),
      plot.margin      = margin(5, 10, 5, 5)
    )
}

country_colors <- c(
  "Albania"                = "#E69F00",
  "Bosnia and Herzegovina" = "#56B4E9",
  "Montenegro"             = "#009E73",
  "North Macedonia"        = "#CC79A7",
  "Serbia"                 = "#D55E00"
)

stage_colors <- c(
  "Committed" = "black",
  "Started"   = "#666666",
  "Completed" = "#BBBBBB"
)

stage_fills <- c(
  "Commitment"           = "grey10",
  "Implementation start" = "grey50",
  "Completion"           = "grey80"
)


# =============================================================================
# 9. VISUALIZATIONS
# =============================================================================

# --- 9a. Gantt: implementation bars colored by country ---
plot_gantt <- geo_balkan_dates %>%
  filter(!is.na(start_date), !is.na(end_date)) %>%
  arrange(Recipient, start_date) %>%
  mutate(project_label = factor(id, levels = rev(id))) %>%
  ggplot(aes(y = project_label, xmin = start_date, xmax = end_date, color = Recipient)) +
  geom_linerange(linewidth = 1.2) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = 0.01)) +
  scale_color_manual(values = country_colors) +
  labs(x = NULL, y = NULL) +
  theme_academic() +
  theme(axis.text.y = element_text(size = 5))

# --- 9b. Gantt: milestones per project (commitment -> start -> completion) ---
geo_balkan_dates %>%
  filter(!is.na(commit_date)) %>%
  arrange(Recipient, commit_date) %>%
  mutate(project_id = factor(row_number())) %>%
  pivot_longer(cols = c(commit_date, start_date, end_date),
               names_to = "milestone", values_to = "date") %>%
  filter(!is.na(date)) %>%
  mutate(milestone = recode(milestone,
                            commit_date = "Commitment",
                            start_date  = "Implementation start",
                            end_date    = "Completion")) %>%
  ggplot(aes(x = date, y = project_id, color = Recipient, shape = milestone)) +
  geom_line(aes(group = project_id), color = "grey70", linewidth = 0.3) +
  geom_point(size = 1.8) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_shape_manual(values = c("Commitment" = 16, "Implementation start" = 17, "Completion" = 15)) +
  scale_color_manual(values = country_colors) +
  labs(x = NULL, y = NULL, shape = NULL, color = NULL) +
  theme_academic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3))

# --- 9c. Gantt: implementation bars + commitment X, faceted by country ---
geo_balkan_dates %>%
  filter(!is.na(start_date), !is.na(end_date)) %>%
  arrange(Recipient, start_date) %>%
  group_by(Recipient) %>%
  mutate(project_id = factor(row_number(), levels = rev(row_number()))) %>%
  ungroup() %>%
  ggplot(aes(y = project_id, xmin = start_date, xmax = end_date)) +
  geom_linerange(linewidth = 1.2, color = "grey30") +
  geom_point(aes(x = commit_date), shape = 4, size = 1.5, color = "black") +
  facet_wrap(~Recipient, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL, y = NULL,
       caption = "Bars: implementation period. x marks commitment date.") +
  theme_academic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3))

# --- 9d. Commitments by year, colored by country ---
plot_commits <- geo_balkan_dates %>%
  filter(!is.na(Commitment.Year)) %>%
  count(Recipient, Commitment.Year) %>%
  ggplot(aes(x = Commitment.Year, y = n, fill = Recipient)) +
  geom_col(position = "dodge", width = 0.7, colour = NA) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2), expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = country_colors) +
  labs(x = NULL, y = "Number of projects") +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 9e. Project lifecycle: committed / started / completed by year ---
plot_lifecycle <- bind_rows(
  geo_balkan_dates %>% filter(!is.na(commit_date)) %>% count(year = year(commit_date)) %>% mutate(stage = "Committed"),
  geo_balkan_dates %>% filter(!is.na(start_date))  %>% count(year = year(start_date))  %>% mutate(stage = "Started"),
  geo_balkan_dates %>% filter(!is.na(end_date))    %>% count(year = year(end_date))    %>% mutate(stage = "Completed")
) %>%
  mutate(stage = factor(stage, levels = c("Committed", "Started", "Completed"))) %>%
  ggplot(aes(x = year, y = n, color = stage, linetype = stage, shape = stage)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 2), expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_color_manual(values    = stage_colors) +
  scale_linetype_manual(values = c("Committed" = "solid", "Started" = "dashed", "Completed" = "dotted")) +
  scale_shape_manual(values    = c("Committed" = 16, "Started" = 17, "Completed" = 15)) +
  labs(x = NULL, y = "Number of projects") +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 9f. Lifecycle faceted by country ---
plot_lifecycle_facet <- bind_rows(
  geo_balkan_dates %>% filter(!is.na(commit_date)) %>% count(Recipient, year = year(commit_date)) %>% mutate(stage = "Committed"),
  geo_balkan_dates %>% filter(!is.na(start_date))  %>% count(Recipient, year = year(start_date))  %>% mutate(stage = "Started"),
  geo_balkan_dates %>% filter(!is.na(end_date))    %>% count(Recipient, year = year(end_date))    %>% mutate(stage = "Completed")
) %>%
  mutate(stage = factor(stage, levels = c("Committed", "Started", "Completed"))) %>%
  ggplot(aes(x = year, y = n, color = stage, linetype = stage, shape = stage)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  facet_wrap(~Recipient, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4), expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_color_manual(values    = stage_colors) +
  scale_linetype_manual(values = c("Committed" = "solid", "Started" = "dashed", "Completed" = "dotted")) +
  scale_shape_manual(values    = c("Committed" = 16, "Started" = 17, "Completed" = 15)) +
  labs(x = NULL, y = "Number of projects") +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 9g. Monthly timelines per country: bar chart ---
plots_bar <- map(countries, function(ctry) {
  df <- geo_balkan_dates %>% filter(Recipient == ctry)
  bind_rows(
    df %>% filter(!is.na(commit_date)) %>% mutate(month = floor_date(commit_date, "month")) %>% count(month) %>% mutate(stage = "Commitment"),
    df %>% filter(!is.na(start_date))  %>% mutate(month = floor_date(start_date,  "month")) %>% count(month) %>% mutate(stage = "Implementation start"),
    df %>% filter(!is.na(end_date))    %>% mutate(month = floor_date(end_date,    "month")) %>% count(month) %>% mutate(stage = "Completion")
  ) %>%
    mutate(stage = factor(stage, levels = c("Commitment", "Implementation start", "Completion"))) %>%
    ggplot(aes(x = month, y = n, fill = stage)) +
    geom_col(position = "dodge", width = 25, colour = NA) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", expand = expansion(mult = 0.02)) +
    scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks(n = 4), expand = expansion(mult = c(0, 0.1))) +
    scale_fill_manual(values = stage_fills) +
    labs(x = NULL, y = "Number of projects", fill = NULL, subtitle = ctry) +
    theme_academic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
          plot.subtitle = element_text(face = "bold", size = 11))
})
names(plots_bar) <- countries

# --- 9h. Monthly timelines per country: line chart (zeros filled for missing months) ---
plots_line <- map(countries, function(ctry) {
  df <- geo_balkan_dates %>% filter(Recipient == ctry)
  bind_rows(
    df %>% filter(!is.na(commit_date)) %>% mutate(month = floor_date(commit_date, "month")) %>% count(month) %>% mutate(stage = "Commitment"),
    df %>% filter(!is.na(start_date))  %>% mutate(month = floor_date(start_date,  "month")) %>% count(month) %>% mutate(stage = "Implementation start"),
    df %>% filter(!is.na(end_date))    %>% mutate(month = floor_date(end_date,    "month")) %>% count(month) %>% mutate(stage = "Completion")
  ) %>%
    mutate(stage = factor(stage, levels = c("Commitment", "Implementation start", "Completion"))) %>%
    complete(month = seq(min(month), max(month), by = "month"), stage, fill = list(n = 0)) %>%
    ggplot(aes(x = month, y = n, color = stage, linetype = stage, shape = stage)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.8) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", expand = expansion(mult = 0.02)) +
    scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks(n = 4), expand = expansion(mult = c(0, 0.1))) +
    scale_color_manual(values    = stage_colors) +
    scale_linetype_manual(values = c("Commitment" = "solid", "Implementation start" = "dashed", "Completion" = "dotted")) +
    scale_shape_manual(values    = c("Commitment" = 16, "Implementation start" = 17, "Completion" = 15)) +
    labs(x = NULL, y = "Number of projects", color = NULL, linetype = NULL, shape = NULL, subtitle = ctry) +
    theme_academic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
          plot.subtitle = element_text(face = "bold", size = 11))
})
names(plots_line) <- countries


# =============================================================================
# 10. PRINT & EXPORT FIGURES
# =============================================================================

print(plot_gantt)
print(plot_commits)
print(plot_lifecycle)
print(plot_lifecycle_facet)
walk(countries, ~ print(plots_bar[[.x]]))
walk(countries, ~ print(plots_line[[.x]]))

ggsave(file.path(fig_dir, "fig_gantt.pdf"),           plot_gantt,           width = 7, height = 9, device = cairo_pdf)
ggsave(file.path(fig_dir, "fig_commitments.pdf"),     plot_commits,         width = 7, height = 4, device = cairo_pdf)
ggsave(file.path(fig_dir, "fig_lifecycle.pdf"),       plot_lifecycle,       width = 7, height = 4, device = cairo_pdf)
ggsave(file.path(fig_dir, "fig_lifecycle_facet.pdf"), plot_lifecycle_facet, width = 7, height = 6, device = cairo_pdf)

walk(countries, function(ctry) {
  slug <- gsub(" ", "_", tolower(ctry))
  ggsave(file.path(fig_dir, paste0("fig_timeline_bar_",  slug, ".pdf")),
         plot = plots_bar[[ctry]],  width = 8, height = 4, device = cairo_pdf)
  ggsave(file.path(fig_dir, paste0("fig_timeline_line_", slug, ".pdf")),
         plot = plots_line[[ctry]], width = 8, height = 4, device = cairo_pdf)
})


# =============================================================================
# 11. SAVE INTERMEDIATE DATA
# =============================================================================

st_write(geo_balkan,
         file.path(int_dir, "geo_balkan.gpkg"),
         delete_if_exists = TRUE)
