# =============================================================================
# GODAD Project-Level Dataset: Cleaning and Preparation (REVISED)
# Description: Prepares project-level data for analysis. Simplified to include
#              only variables needed for revised models:
#              - Treatment coding (pipeline/active/completed) for Chinese projects
#              - Donor controls (WB, EU location counts)
#              - China sector dummies for sector match mechanism
#              - Commitment amounts for project size robustness check
#              - Lags for prior exposure (H4)
# =============================================================================


# =============================================================================
# 1. SETUP
# =============================================================================

library(data.table)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(stringi)
library(readxl)
library(purrr)
library(stringr)
library(janitor)
library(tibble)
library(writexl)
library(cairo)
library(ggrepel)


# =============================================================================
# 2. LOAD AND FILTER GODAD DATA
# =============================================================================

data_dir <- "C:/data"

godad <- fread(file.path(data_dir, "GODAD_projectlevel.csv"))

# Filter: Western Balkans, 2000 onwards
godad_wb <- godad %>%
  filter(
    name_0 %in% c("Serbia", "Albania", "Bosnia and Herzegovina",
                  "Kosovo", "North Macedonia", "Montenegro"),
    startyear >= 2000
  )

# Exclude US and India; create donor categories
godad_wb <- godad_wb %>%
  filter(!donor %in% c("India", "United States")) %>%
  mutate(donor_category = case_when(
    donor == "World Bank" ~ "World Bank",
    donor == "China"      ~ "China",
    TRUE                  ~ "European Donors"
  ))


# =============================================================================
# 3. LOAD AND PREPARE SHAPEFILES
# =============================================================================

# ADM0 (country level)
adm0_files     <- list.files(data_dir, pattern = "gadm41_.+_0\\.json$", full.names = TRUE)
combined_shape <- do.call(rbind, lapply(adm0_files, st_read))

# ADM1 (sub-national level)
adm1_files       <- list.files(data_dir, pattern = "gadm41_.+\\.gpkg$", full.names = TRUE)
adm1_shapes_list <- lapply(adm1_files, function(file) st_read(file, layer = "ADM_ADM_1"))
combined_adm1_shape <- do.call(rbind, adm1_shapes_list)

# Align CRS and clean names
combined_adm1_shape <- st_transform(combined_adm1_shape, crs = st_crs(combined_shape)) %>%
  mutate(ADM1_NAME_CLEAN = stri_trans_general(NAME_1, "Latin-ASCII"))


# =============================================================================
# 4. MAP ADM1 REGIONS TO SURVEY CLUSTERS
# =============================================================================

lookup_table <- read_excel(file.path(data_dir, "mapping.xlsx"), sheet = 1)

combined_adm1_shape <- combined_adm1_shape %>%
  left_join(lookup_table, by = "ADM1_NAME_CLEAN")

# Dissolve ADM1 polygons into 37 survey cluster regions
merged_regions_shape <- combined_adm1_shape %>%
  group_by(region) %>%
  summarize(geom = st_union(geom)) %>%
  ungroup()


# =============================================================================
# 5. VISUALISE SURVEY CLUSTERS
# =============================================================================

region_centroids <- merged_regions_shape %>%
  st_centroid() %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

survey_map <- ggplot() +
  geom_sf(data = merged_regions_shape, fill = "white", color = "grey60", linewidth = 0.25) +
  geom_sf(data = combined_shape, fill = NA, color = "black", linewidth = 0.7) +
  geom_text_repel(
    data = region_centroids,
    aes(x = lon, y = lat, label = region),
    size = 2.2, family = "serif", color = "black",
    box.padding = 0.2, point.size = NA,
    segment.color = "grey50", segment.size = 0.3,
    segment.linetype = "dashed",
    min.segment.length = 0.4, max.overlaps = 25
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 11) +
  theme(plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin      = margin(10, 10, 10, 10))

ggsave(file.path(data_dir, "survey_map.pdf"), survey_map,
       width = 7.5, height = 5.5, units = "in", device = cairo_pdf)


# =============================================================================
# 6. VISUALISE PROJECT LOCATIONS
# =============================================================================

godad_wb_sf <- godad_wb %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

project_points_2000_2021 <- godad_wb_sf %>%
  filter(startyear >= 2000, startyear <= 2021) %>%
  mutate(donor_group = factor(donor_category,
                              levels = c("China", "World Bank", "European Donors")))

plot_project_map_bw <- function(data) {
  ggplot() +
    geom_sf(data = merged_regions_shape, fill = "white", color = "grey70", linewidth = 0.2) +
    geom_sf(data = combined_shape, fill = NA, color = "black", linewidth = 0.7) +
    geom_sf(data = filter(data, donor_group == "European Donors"),
            aes(shape = donor_group), color = "grey65", size = 1.6, alpha = 0.8) +
    geom_sf(data = filter(data, donor_group == "World Bank"),
            aes(shape = donor_group), color = "grey30", size = 1.6, alpha = 0.8) +
    geom_sf(data = filter(data, donor_group == "China"),
            aes(shape = donor_group), color = "black",  size = 1.8, alpha = 0.9) +
    scale_shape_manual(
      name   = "Donor",
      values = c("China" = 16, "World Bank" = 17, "European Donors" = 15)
    ) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border     = element_rect(fill = NA, color = "black", linewidth = 0.5),
      legend.position  = "right",
      legend.title     = element_text(size = 9, face = "bold", family = "serif"),
      legend.text      = element_text(size = 8.5, family = "serif"),
      legend.key.size  = unit(0.4, "cm"),
      plot.margin      = margin(10, 10, 10, 10)
    )
}

map_bw <- plot_project_map_bw(project_points_2000_2021)
ggsave(file.path(data_dir, "map_bw_2000_2021.pdf"), map_bw,
       width = 7.5, height = 5.5, units = "in", device = cairo_pdf)


# =============================================================================
# 7. BUILD PANEL STRUCTURE (37 regions x 2000-2021)
# =============================================================================

region_names <- unique(merged_regions_shape$region)
years        <- 2000:2021

panel_df <- expand_grid(region = region_names, year = years)

region_country <- combined_adm1_shape %>%
  st_drop_geometry() %>%
  select(region, COUNTRY) %>%
  distinct()

panel_df <- panel_df %>%
  left_join(region_country, by = "region") %>%
  rename(country = COUNTRY)


# =============================================================================
# 8. SPATIAL JOIN: ASSIGN PROJECTS TO REGIONS
# =============================================================================

godad_sf <- godad_wb %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(merged_regions_shape))

godad_with_region <- st_join(godad_sf, merged_regions_shape, join = st_within)


# =============================================================================
# 9. CHINESE PROJECT TREATMENT CODING (pipeline / active / completed)
#
# Logic:
#   pipeline  = startyear == survey_year (newly committed, not yet active)
#   active    = startyear < survey_year AND closingyear >= survey_year (ongoing)
#   completed = closingyear < survey_year (finished before survey year)
#
# Missing closingyear:
#   - Implementation / Pipeline:Commitment => treat as active through 2021
#   - Completion with missing closingyear  => excluded from main analysis (n=14)
#     (run as robustness check with median-imputed closingyear)
# =============================================================================

china_treatment <- godad_with_region %>%
  st_drop_geometry() %>%
  filter(donor_category == "China", !is.na(startyear), !is.na(region)) %>%
  distinct(project_location_id, .keep_all = TRUE) %>%
  mutate(
    closingyear_adj = case_when(
      is.na(closingyear) & status %in% c("Implementation",
                                         "Pipeline: Commitment") ~ 2021L,
      is.na(closingyear) & status == "Completion"                 ~ NA_integer_,
      TRUE                                                        ~ as.integer(closingyear)
    )
  ) %>%
  # drop 14 Completion locations with missing closingyear
  filter(!(status == "Completion" & is.na(closingyear_adj)))

china_treatment_panel <- china_treatment %>%
  crossing(survey_year = 2016:2021) %>%
  mutate(
    treatment = case_when(
      startyear == survey_year &
        (is.na(closingyear_adj) | closingyear_adj >= survey_year) ~ "pipeline",
      startyear < survey_year &
        (is.na(closingyear_adj) | closingyear_adj >= survey_year) ~ "active",
      closingyear_adj < survey_year                               ~ "completed",
      TRUE                                                        ~ NA_character_
    )
  ) %>%
  filter(!is.na(treatment))

china_treatment_counts <- china_treatment_panel %>%
  count(region, year = survey_year, treatment) %>%
  pivot_wider(names_from  = treatment,
              values_from = n,
              values_fill = 0,
              names_prefix = "chn_loc_") %>%
  mutate(
    chn_loc_pipeline_dummy  = as.integer(chn_loc_pipeline  > 0),
    chn_loc_active_dummy    = as.integer(chn_loc_active    > 0),
    chn_loc_completed_dummy = as.integer(chn_loc_completed > 0),
    # combined current exposure (pipeline OR active) for comparability with prior models
    chn_loc_current         = chn_loc_pipeline + chn_loc_active,
    chn_loc_current_dummy   = as.integer(chn_loc_current   > 0)
  )

panel_df <- panel_df %>%
  left_join(china_treatment_counts, by = c("region", "year")) %>%
  mutate(across(starts_with("chn_loc_"), ~ replace_na(.x, 0)))


# =============================================================================
# 10. DONOR CONTROLS: WORLD BANK AND EUROPEAN DONOR LOCATION COUNTS
# =============================================================================

location_counts_by_donor <- godad_with_region %>%
  st_drop_geometry() %>%
  filter(!is.na(startyear)) %>%
  distinct(region, year = startyear, donor_category, project_location_id) %>%
  count(region, year, donor_category, name = "location_count")

location_counts_wide <- location_counts_by_donor %>%
  pivot_wider(names_from  = donor_category,
              values_from = location_count,
              names_prefix = "location_count_",
              values_fill  = 0) %>%
  rename(
    location_count_wb  = `location_count_World Bank`,
    location_count_eu  = `location_count_European Donors`
  ) %>%
  # drop China column here - China exposure handled by treatment coding above
  select(region, year, location_count_wb, location_count_eu)

panel_df <- panel_df %>%
  left_join(location_counts_wide, by = c("region", "year")) %>%
  mutate(across(c(location_count_wb, location_count_eu), ~ replace_na(.x, 0)))


# =============================================================================
# 11. PROJECT SIZE: COMMITMENT AMOUNTS FOR CHINA
#     (used in robustness check for project heterogeneity)
# =============================================================================

godad_dedup <- godad_with_region %>%
  st_drop_geometry() %>%
  filter(!is.na(startyear), !is.na(region), !is.na(comm_loc_evensplit),
         donor_category == "China") %>%
  distinct(project_location_id, .keep_all = TRUE)

comm_chn <- godad_dedup %>%
  group_by(region, year = startyear) %>%
  summarise(comm_chn = sum(comm_loc_evensplit, na.rm = TRUE), .groups = "drop")

panel_df <- panel_df %>%
  left_join(comm_chn, by = c("region", "year")) %>%
  mutate(comm_chn = coalesce(comm_chn, 0))


# =============================================================================
# 12. CHINA SECTOR DUMMIES (for sector match mechanism, H3)
# =============================================================================

sector_dummies_china <- godad_with_region %>%
  st_drop_geometry() %>%
  filter(donor_category == "China", !is.na(region),
         !is.na(startyear), !is.na(sector_name)) %>%
  distinct(region, year = startyear, sector_name) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from  = sector_name,
              values_from = value,
              values_fill = 0,
              names_prefix = "dummy_chn_sector_")

panel_df <- panel_df %>%
  left_join(sector_dummies_china, by = c("region", "year")) %>%
  mutate(across(starts_with("dummy_chn_sector_"), ~ replace_na(.x, 0)))

# Clean variable names
panel_df <- panel_df %>%
  rename_with(~ janitor::make_clean_names(.), starts_with("dummy_chn_sector_"))


# =============================================================================
# 13. TIME LAGS AND DERIVED VARIABLES
# =============================================================================

dt <- as.data.table(panel_df)
dt[, year   := as.integer(as.character(year))]
dt[, region := as.character(region)]
setorder(dt, region, year)

# Lags t1-t5 for treatment variables
dt[, paste0("chn_loc_pipeline_t",  1:5) :=
     lapply(1:5, function(k) shift(chn_loc_pipeline,  k)), by = region]
dt[, paste0("chn_loc_active_t",    1:5) :=
     lapply(1:5, function(k) shift(chn_loc_active,    k)), by = region]
dt[, paste0("chn_loc_completed_t", 1:5) :=
     lapply(1:5, function(k) shift(chn_loc_completed, k)), by = region]

# Lags for WB location counts (donor control)
dt[, paste0("location_count_wb_t", 1:5) :=
     lapply(1:5, function(k) shift(location_count_wb, k)), by = region]

# Pre-2016 cumulative Chinese exposure (for H4 prior exposure measure)
dt[, sum_loc_chn_2000_2015 := sum(
  chn_loc_current[year >= 2000 & year <= 2015], na.rm = TRUE
), by = region]

dt[year >= 2016 & year <= 2021,
   location_count_chn_pre2016 := sum_loc_chn_2000_2015]

dt[, location_count_chn_pre2016_dummy :=
     as.integer(sum_loc_chn_2000_2015 > 0)]

# Regions with pre-2016 Chinese exposure (descriptive check)
regions_with_pre2016 <- unique(
  dt[location_count_chn_pre2016_dummy == 1, .(region, country)]
)
print(regions_with_pre2016)


# =============================================================================
# 14. SAVE DATASETS
# =============================================================================

saveRDS(dt,                              file.path(data_dir, "godad_panel_full.rds"))
saveRDS(dt[year >= 2016 & year <= 2021], file.path(data_dir, "godad_panel_2016_2021.rds"))


# =============================================================================
# 15. DESCRIPTIVE CHECKS
# =============================================================================

# Treatment distribution overall
dt[year >= 2016 & year <= 2021] %>%
  as_tibble() %>%
  summarise(
    n_pipeline  = sum(chn_loc_pipeline  > 0),
    n_active    = sum(chn_loc_active    > 0),
    n_completed = sum(chn_loc_completed > 0),
    n_none      = sum(chn_loc_current   == 0 & chn_loc_completed == 0)
  )

# Treatment distribution by country-year
dt[year >= 2016 & year <= 2021] %>%
  as_tibble() %>%
  group_by(country, year) %>%
  summarise(
    pipeline  = sum(chn_loc_pipeline),
    active    = sum(chn_loc_active),
    completed = sum(chn_loc_completed),
    .groups   = "drop"
  ) %>%
  print(n = Inf)

# Region-year counts per treatment category
# (effective sample size for model identification)
dt[year >= 2016 & year <= 2021] %>%
  as_tibble() %>%
  summarise(
    region_years_pipeline  = sum(chn_loc_pipeline_dummy  > 0),
    region_years_active    = sum(chn_loc_active_dummy    > 0),
    region_years_completed = sum(chn_loc_completed_dummy > 0)
  )


dt[year >= 2016 & year <= 2021] %>%
  as_tibble() %>%
  filter(chn_loc_pipeline_dummy > 0 | chn_loc_active_dummy > 0) %>%
  select(region, country, year, 
         chn_loc_pipeline, chn_loc_active, chn_loc_completed) %>%
  arrange(country, region, year) %>%
  print(n = Inf)



