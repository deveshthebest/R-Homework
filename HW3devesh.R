# Homework 3 


# install.packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(patchwork)   # only used to show plot pairs side by side

vehicles <- ggplot2::mpg        
gems     <- ggplot2::diamonds   

dir.create("data/raw",   recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs",    recursive = TRUE, showWarnings = FALSE)


# PROBLEM 1


# Part A
vehicles_A <- vehicles %>% filter(displ <= 5)
nrow(vehicles_A)                     # 198 of 234 cars
vehicles_A %>% count(drv)            # 4 = 84, f = 105, r = 9

pA <- ggplot(vehicles_A, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  facet_wrap(~ drv) +
  labs(x = "Engine displacement (litres)",
       y = "Highway fuel economy (mpg)") +
  theme_bw()
pA
ggsave("outputs/p1_partA.png", pA, width = 9, height = 4.5, dpi = 150)


# Part B 
pB_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  theme_bw()

pB_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion within drive type") +
  theme_bw()

pB <- pB_count + pB_prop
pB
ggsave("outputs/p1_partB.png", pB, width = 10, height = 4.5, dpi = 150)

# Sample sizes behind each bar (shows the heights in the right plot hide n):
vehicles %>% count(drv, name = "n_cars")
#   4 = 103, f = 106, r = 25


#  Part C
gems_small <- gems %>% filter(carat <= 3)

n_total    <- nrow(gems)
n_retained <- nrow(gems_small)
n_excluded <- n_total - n_retained
tibble(total = n_total, retained = n_retained, excluded = n_excluded,
       rule = "kept carat <= 3")

x_scale <- list(
  coord_cartesian(xlim = c(0, 3)),
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)),
  labs(x = "Carat", y = "Count")
)

p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right",
                 fill = "grey30") +
  x_scale +
  ggtitle("Bin width = 0.01 carat") +
  theme_bw()

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right",
                 fill = "grey30", color = "white") +
  x_scale +
  ggtitle("Bin width = 0.5 carat") +
  theme_bw()

p_fine
p_coarse
pC <- p_fine / p_coarse +
  plot_annotation(caption = sprintf(
    "Diamonds with carat <= 3: %d retained, %d excluded (of %d).",
    n_retained, n_excluded, n_total))
pC
ggsave("outputs/p1_partC.png", pC, width = 9, height = 7, dpi = 150)

gems %>%
  summarise(n_0.99 = sum(near(carat, 0.99)),
            n_1.00 = sum(near(carat, 1.00)))



# PROBLEMS 2 & 3

lib_url  <- "https://data.cityofchicago.org/api/views/x8fc-8rcq/rows.csv?accessType=DOWNLOAD"
ca_url   <- "https://data.cityofchicago.org/api/geospatial/igwz-8jzy?method=export&format=GeoJSON"
lib_raw_path <- "data/raw/chicago_libraries_raw.csv"
ca_raw_path  <- "data/raw/chicago_community_areas_raw.geojson"

if (!file.exists(lib_raw_path)) download.file(lib_url, lib_raw_path, mode = "wb")
if (!file.exists(ca_raw_path))  download.file(ca_url,  ca_raw_path,  mode = "wb")
download_date <- format(file.info(lib_raw_path)$mtime, "%Y-%m-%d")

data_sources <- tibble(
  dataset    = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
                 "Boundaries - Community Areas"),
  publisher  = "City of Chicago Data Portal",
  url        = c("https://data.cityofchicago.org/d/x8fc-8rcq",
                 "https://data.cityofchicago.org/d/igwz-8jzy"),
  download_url = c(lib_url, ca_url),
  format     = c("CSV (LOCATION field holds lat/long)", "GeoJSON"),
  local_file = c(lib_raw_path, ca_raw_path),
  downloaded = download_date,
  license    = "City of Chicago Data Portal Terms of Use",
  used_in    = c("Problems 2 and 3", "Problem 3 (outline in Problem 2 maps)")
)
write_csv(data_sources, "data_sources.csv")

crs_m <- 26916

# PROBLEM 2 


lib_raw <- read_csv(lib_raw_path, show_col_types = FALSE)
names(lib_raw) <- tolower(trimws(names(lib_raw)))   # e.g. "NAME " -> "name"
n_raw <- nrow(lib_raw)

coord_mat <- regmatches(
  lib_raw$location,
  regexec("\\(\\s*(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)\\s*\\)", lib_raw$location)
)
lib <- lib_raw %>%
  mutate(
    lat = suppressWarnings(as.numeric(sapply(coord_mat, `[`, 2))),
    lon = suppressWarnings(as.numeric(sapply(coord_mat, `[`, 3))),
    facility_name = trimws(name)
  )

lib <- lib %>%
  mutate(coord_problem = case_when(
    is.na(location) | location == ""            ~ "missing location",
    is.na(lat) | is.na(lon)                      ~ "unparseable location",
    lat < 41.6 | lat > 42.1 | lon < -88 | lon > -87.5 ~ "outside Chicago bbox",
    lat == 0 | lon == 0                          ~ "zero coordinate",
    TRUE                                         ~ NA_character_
  ))

coord_report <- lib %>% count(coord_problem, name = "n")
coord_report

dup_coords <- lib %>% filter(is.na(coord_problem)) %>%
  group_by(lat, lon) %>% filter(n() > 1) %>% ungroup() %>%
  select(facility_name, address, lat, lon)
dup_names  <- lib %>% group_by(facility_name) %>% filter(n() > 1) %>% ungroup() %>%
  select(facility_name, address)
n_exact_dup_rows <- sum(duplicated(lib_raw))
cat("Rows sharing coordinates:", nrow(dup_coords),
    "| rows sharing a name:", nrow(dup_names),
    "| fully identical rows:", n_exact_dup_rows, "\n")

removed <- lib %>% filter(!is.na(coord_problem)) %>%
  transmute(facility_name, address, reason = coord_problem)
write_csv(removed, "outputs/p2_removed_records.csv")

lib_clean <- lib %>%
  filter(is.na(coord_problem)) %>%
  mutate(site_id = sprintf("LIB%03d", row_number())) %>%
  select(site_id, facility_name, address, zip, lat, lon)

cleaning_report <- tibble(
  step  = c("raw records", "removed (bad/missing coords)", "retained"),
  n     = c(n_raw, nrow(removed), nrow(lib_clean))
)
cleaning_report
write_csv(cleaning_report, "outputs/p2_cleaning_report.csv")
write_csv(lib_clean, "data/clean/libraries_clean.csv")

stopifnot(nrow(lib_clean) >= 20, !anyDuplicated(lib_clean$site_id))

sites_m <- st_as_sf(lib_clean, coords = c("lon", "lat"), crs = 4326,
                    remove = FALSE) %>%
  st_transform(crs_m)
st_crs(sites_m)$input                  # "EPSG:26916"
st_crs(sites_m)$units                  # "m"

ca_raw <- st_read(ca_raw_path, quiet = TRUE)

src_caption <- paste0("Source: City of Chicago Data Portal (CPL locations;\n",
                      "Community Areas), downloaded ", download_date,
                      ". CRS: EPSG:26916 (m).")
theme_set(theme_bw() + theme(plot.caption = element_text(hjust = 0, size = 8),
                             plot.caption.position = "plot"))

#  Maps
p_sites <- ggplot() +
  geom_sf(data = st_transform(ca_raw, crs_m), aes(linetype = "Community area"),
          fill = "grey97", color = "grey60", linewidth = 0.3) +
  geom_sf(data = sites_m, aes(color = "Library branch"), size = 1.8) +
  scale_color_manual(name = NULL, values = c("Library branch" = "#1f4e9c")) +
  scale_linetype_manual(name = NULL, values = c("Community area" = "solid")) +
  labs(title = sprintf("Chicago Public Library locations (n = %d)", nrow(sites_m)),
       caption = src_caption) +
  theme_get()
p_sites
ggsave("outputs/p2_sites.png", p_sites, width = 7, height = 8, dpi = 150)


buf_500  <- sites_m %>% st_buffer(500)  %>% st_union() %>% st_make_valid()
buf_1000 <- sites_m %>% st_buffer(1000) %>% st_union() %>% st_make_valid()

buffer_summary <- tibble(
  radius_m       = c(500, 1000),
  union_area_km2 = as.numeric(c(st_area(buf_500), st_area(buf_1000))) / 1e6
)
buffer_summary
stopifnot(buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1])
write_csv(buffer_summary, "outputs/buffer_summary.csv")

buffers_sf <- st_sf(radius_m = c(500, 1000), geometry = c(buf_500, buf_1000))
st_write(sites_m,    "data/clean/sites_m.gpkg",   delete_dsn = TRUE, quiet = TRUE)
st_write(buffers_sf, "data/clean/buffers.gpkg",   delete_dsn = TRUE, quiet = TRUE)

p_buffers <- ggplot() +
  geom_sf(data = st_transform(ca_raw, crs_m), fill = "grey97",
          color = "grey60", linewidth = 0.3) +
  geom_sf(data = st_sf(geometry = buf_500), aes(fill = "Within 500 m of a library"),
          color = NA, alpha = 0.5) +
  geom_sf(data = sites_m, aes(color = "Library branch"), size = 1.2) +
  scale_fill_manual(name = NULL, values = c("Within 500 m of a library" = "#f28e2b")) +
  scale_color_manual(name = NULL, values = c("Library branch" = "#1f4e9c")) +
  labs(title = "Area within 500 m of at least one Chicago library",
       subtitle = sprintf("Combined 500 m buffer = %.1f km² (overlaps counted once)",
                          buffer_summary$union_area_km2[1]),
       caption = src_caption) +
  theme_get()
p_buffers
ggsave("outputs/p2_buffers.png", p_buffers, width = 7, height = 8, dpi = 150)




# PROBLEM 3

names(ca_raw) <- tolower(names(ca_raw))
id_col <- intersect(c("area_numbe", "area_number", "area_num_1", "area_num"),
                    names(ca_raw))[1]

areas <- ca_raw %>%
  transmute(district_id   = as.integer(as.character(.data[[id_col]])),
            district_name = tools::toTitleCase(tolower(trimws(community)))) %>%
  st_transform(crs_m)

boundary_checks <- tibble(
  n_polygons     = nrow(areas),
  missing_ids    = sum(is.na(areas$district_id)),
  duplicate_ids  = sum(duplicated(areas$district_id)),
  empty_geoms    = sum(st_is_empty(areas)),
  invalid_geoms  = sum(!st_is_valid(areas))
)
boundary_checks
areas <- st_make_valid(areas)            
stopifnot(!anyDuplicated(areas$district_id), all(st_is_valid(areas)),
          nrow(areas) >= 5, st_crs(areas) == st_crs(sites_m))

areas <- areas %>% mutate(area_km2 = as.numeric(st_area(.)) / 1e6)
stopifnot(all(areas$area_km2 > 0))

hits <- st_intersects(sites_m, areas)   
site_match_audit <- tibble(
  site_id       = sites_m$site_id,
  facility_name = sites_m$facility_name,
  n_matches     = lengths(hits)
)
write_csv(site_match_audit, "site_match_audit.csv")

site_match_audit %>%
  mutate(match_type = case_when(n_matches == 0 ~ "zero",
                                n_matches == 1 ~ "one",
                                TRUE           ~ "multiple")) %>%
  count(match_type)
not_exactly_one <- site_match_audit %>% filter(n_matches != 1)
not_exactly_one                          

assigned <- sites_m %>%
  mutate(district_id = sapply(hits, function(h)
           if (length(h) == 1) areas$district_id[h] else NA_integer_)) %>%
  filter(!is.na(district_id))
n_assigned   <- nrow(assigned)
n_unassigned <- nrow(sites_m) - n_assigned

# Summary table 
district_counts <- areas %>%
  st_drop_geometry() %>%
  left_join(assigned %>% st_drop_geometry() %>% count(district_id, name = "n_sites"),
            by = "district_id") %>%
  mutate(n_sites       = replace_na(n_sites, 0L),
         sites_per_km2 = n_sites / area_km2) %>%
  select(district_id, district_name, n_sites, area_km2, sites_per_km2) %>%
  arrange(desc(n_sites), desc(sites_per_km2)) %>%
  as_tibble()

stopifnot(sum(district_counts$n_sites) == n_assigned)
print(district_counts, n = Inf)
write_csv(district_counts, "outputs/district_counts.csv")

district_counts %>% slice_max(n_sites, n = 5, with_ties = FALSE)
district_counts %>% slice_max(sites_per_km2, n = 5, with_ties = FALSE)
cat("Areas with zero libraries:", sum(district_counts$n_sites == 0), "\n")

# Maps 
map_df <- areas %>% left_join(district_counts %>% select(-district_name, -area_km2),
                              by = "district_id")
bb <- st_bbox(areas)
same_extent <- coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")],
                        expand = FALSE)
p3_caption <- paste0(src_caption, "\n", n_assigned, " sites assigned; ",
                     n_unassigned, " left unassigned (matched 0 or >1 areas).\n",
                     "0 = lightest colour; dark grey = missing (none expected).")

p_count <- ggplot(map_df) +
  geom_sf(aes(fill = n_sites), color = "white", linewidth = 0.2) +
  scale_fill_distiller(name = "Libraries\n(count)", palette = "Blues",
                       direction = 1, limits = c(0, NA),
                       breaks = function(l) unique(c(0, scales::breaks_pretty()(l))),
                       na.value = "grey50") +
  same_extent +
  labs(title = "Libraries per community area (count)", caption = p3_caption) +
  theme_get()

p_density <- ggplot(map_df) +
  geom_sf(aes(fill = sites_per_km2), color = "white", linewidth = 0.2) +
  scale_fill_distiller(name = "Libraries\nper km²", palette = "Oranges",
                       direction = 1, limits = c(0, NA),
                       breaks = function(l) unique(c(0, scales::breaks_pretty()(l))),
                       na.value = "grey50") +
  same_extent +
  labs(title = "Libraries per km² by community area", caption = p3_caption) +
  theme_get()

p_count
p_density
ggsave("outputs/p3_count.png",   p_count,   width = 7, height = 8, dpi = 150)
ggsave("outputs/p3_density.png", p_density, width = 7, height = 8, dpi = 150)

