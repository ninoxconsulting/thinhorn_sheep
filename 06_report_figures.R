## 05: Summary plots for the report

library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidyterra)
library(rnaturalearth)
library(gganimate)
library(magick)
library(sf)
library(lubridate)
library(terra)
library(fs)


# read in the summary data 
clean_dir <- fs::path("01_clean_data")
out_dir <- fs::path("02_draft_outputs/01_lamb_figures")


# read in graphics background
dem <- rast(path("02_draft_outputs","02_lamb_kde", "lambing_kde_dem.tif"))
slope_r  <- terrain(dem, v = "slope",  unit = "radians")
aspect_r <- terrain(dem, v = "aspect", unit = "radians")
hillshade <- shade(slope_r, aspect_r, angle = 45, direction = 315)  # sun altitude/azimuth

# get boundaries
bg = ne_countries(scale = "medium", continent = 'north america', returnclass = "sf")
bg <- bg |> select(admin, continent)

# use .gpkg as csv drops time stamp
allpts <- st_read(fs::path("01_clean_data", "location_steps_all_20260721_TEST.gpkg"))

# add X and Y columns 
allpts <- cbind(allpts, st_coordinates(allpts))

# generate a key to be used in plots below
id_key <- allpts |> 
  select(tag_idn, sex,Age_annuli, sheep_class) |>
  st_drop_geometry() |> 
  unique()  


age_lookup <- allpts |>
  select(tag_idn, year_pst, Age_annuli) |>
  mutate(tag_idn = as.character(tag_idn)) |> 
  st_drop_geometry() |> unique() |> 
  group_by(tag_idn) |> 
  mutate(cap_year= min(year_pst)) |> 
  rowwise() |> 
  mutate(Age_annuli_est = Age_annuli + (year_pst - cap_year)) 




sheep_data <- allpts |> 
  st_drop_geometry() |>
  mutate(date_time_pst1 = format(as.POSIXct(date_time_pst), "%Y-%m-%d %H:%M:%S")) |> 
  mutate(date_time_pst = ymd_hms(date_time_pst1)) |> 
  mutate(date_pst = as_date(date_time_pst)) |> 
  select(-date_time_pst1) |> 
  select(tag_idn, sex, Age_annuli, sheep_class, year_pst, date_pst) 

sheep_data <- sheep_data %>%
  mutate(date_pst = as.Date(date_pst))

# quick sanity check -- should print "Date"
#print(class(sheep_data$date_pst))

# ------------------------------------------------------------
# 1. One row per sheep per day it has at least one GPS fix
# ------------------------------------------------------------
sheep_days <- sheep_data %>%
  distinct(tag_idn, date_pst) %>%
  arrange(tag_idn, date_pst) |> 
  left_join(id_key, by = "tag_idn")   # FIX: explicit `by=` (was implicit)

# Common date range, used to align both panels' x-axes exactly
date_range <- range(sheep_days$date_pst )

# ------------------------------------------------------------
# 2. Per-sheep active date range + number of active days
#    (for the Gantt-style timeline)
# ------------------------------------------------------------
sheep_range <- sheep_days |> 
  group_by(tag_idn, sex, sheep_class) |> 
  summarise(
    start_date = min(date_pst),
    end_date   = max(date_pst),
    n_days     = n_distinct(date_pst),
    .groups = "drop"
  ) %>%
  arrange(sheep_class, start_date) %>%
  mutate(tag_idn = factor(tag_idn, levels = tag_idn)) 

# ------------------------------------------------------------
# 3. Daily count of active sheep (for the summary bar chart)
# ------------------------------------------------------------
daily_active <- sheep_days %>%
  group_by(date_pst, sex) %>%
  summarise(n_active = n_distinct(tag_idn), .groups = "drop")


# # ------------------------------------------------------------
# # 4. Panel A -- per-sheep activity timeline
# # ------------------------------------------------------------
# p_timeline <- ggplot(sheep_range) +
#   geom_segment(aes(x = start_date, xend = end_date, y = tag_idn, yend = tag_idn,
#                    colour = factor(sex)),
#                linewidth = 2.5, lineend = "round") +
#   scale_colour_manual(name = "Sex", values = c("female" = "firebrick", "male" = "steelblue")) +
#   scale_x_date(limits = date_range, breaks = scales::breaks_pretty(n = 20), date_labels = "%b %Y") +
#   labs(x = NULL, y = "Sheep (Tag ID)", title = "Sheep GPS Activity Timeline") +
#   #facet_wrap(~, scales = "free_y") +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_text(size = 6),
#     axis.text.x = element_text(size = 8)
#   )
# p_timeline




# ------------------------------------------------------------
# 4. Panel A -- per-sheep activity timeline
# ------------------------------------------------------------
# Compute y-positions (as plotted, integer factor codes) for each sheep,
# then find the boundaries where sheep_class changes -- used to draw
# break lines between groups -- and each group's midpoint, used to
# place a class label alongside the plot.
sheep_range <- sheep_range %>%
  mutate(y_pos = as.integer(tag_idn))

break_lines <- sheep_range %>%
  arrange(y_pos) %>%
  mutate(prev_class = lag(sheep_class)) %>%
  filter(sheep_class != prev_class) %>%
  pull(y_pos) %>%
  { . - 0.5 }

class_labels <- sheep_range %>%
  group_by(sheep_class) %>%
  summarise(y_mid = mean(y_pos), .groups = "drop")

p_timeline <- ggplot(sheep_range) +
  geom_segment(aes(x = start_date, xend = end_date, y = tag_idn, yend = tag_idn,
                   colour = factor(sex)),
               linewidth = 2.5, lineend = "round") +
  # break lines between sheep_class groups, in place of faceting --
  # avoids the lopsided panel sizes free_y faceting produced when
  # class sizes are very uneven
  geom_hline(yintercept = break_lines, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  # class label alongside each group, since there's no facet strip
  # to show it anymore
  geom_text(data = class_labels, aes(x = date_range[2], y = y_mid, label = sheep_class),
            hjust = -0.05, fontface = "bold", size = 3, inherit.aes = FALSE) +
  scale_colour_manual(name = "Sex", values = c("female" = "firebrick", "male" = "steelblue")) +
  scale_x_date(limits = date_range, breaks = scales::breaks_pretty(n = 10), date_labels = "%b %Y") +
  coord_cartesian(clip = "off") +   # lets the class labels draw just outside the panel
  labs(x = NULL, y = "Sheep (Tag ID)", title = "Sheep GPS Activity Timeline") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    plot.margin = margin(5.5, 60, 5.5, 5.5)   # extra right margin for class labels
  )
p_timeline

# write out 
ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","sheep_duration_class_summary.png"), width = 9, height = 12, dpi = 300)

############################################################################



# ============================================================
# Spatial map: all sheep GPS points over hillshade,
# with a north arrow, scale bar, and a BC-wide inset map
# ------------------------------------------------------------
# Assumes `allpts` (sf points, already loaded) and `hillshade`
# (SpatRaster, already computed) exist in the session -- both are
# built earlier in the activity-summary script. If running this
# standalone, re-run those steps first (DEM load -> terrain() ->
# shade()) before this script.
# ============================================================

library(ggplot2)
library(sf)
library(terra)
library(tidyterra)
library(ggspatial)   # annotation_north_arrow(), annotation_scale()
library(patchwork)   # inset_element()
library(dplyr)
library(bcmaps)
library(Polychrome)
# ------------------------------------------------------------
# 1. Main map: all GPS points over hillshade
# ------------------------------------------------------------
# read in the location data # use .gpkg as csv drops time stamp
allpts <- st_read(fs::path("01_clean_data", "location_steps_all_20260721_TEST.gpkg"))
allpts1 <- allpts |> 
  select(tag_idn, date_pst.y,Age_annuli, sheep_class, sex) |> 
  group_by(tag_idn, date_pst.y) |> 
  slice(1)

n_sheep <- length(unique(allpts1$tag_idn))  
id_colours <- as.vector(createPalette(
  n_sheep,
  seedcolors = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00"),
  range = c(30, 90)   # perceptual lightness range to draw from
))

pts_bbox <- st_bbox(allpts1)
pad <- 3000  # metres of padding around the points, adjust to taste

main_map <- ggplot() +
  geom_spatraster(data = hillshade, show.legend = FALSE, alpha = 0.4) +
  scale_fill_gradient(low = "grey10", high = "grey95", na.value = NA) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = allpts1, aes(colour = factor(tag_idn)), size = 1.2, alpha = 0.5) +
  scale_colour_manual(name = "Sheep ID", values = id_colours) +
  #scale_colour_viridis_d(name = "Sheep ID") +
  #scale_colour_manual(name = "Sex", values = c("female" = "firebrick", "male" = "steelblue")) +
  coord_sf(
    xlim = c(pts_bbox["xmin"] - pad, pts_bbox["xmax"] + pad),
    ylim = c(pts_bbox["ymin"] - pad, pts_bbox["ymax"] + pad),
    expand = FALSE
  ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering(),
    height = unit(1.2, "cm"), width = unit(1.2, "cm")
  ) +
  annotation_scale(location = "br", width_hint = 0.25) +
  labs(title = "Sheep GPS Locations") +
  theme_minimal() +
  theme(
    axis.title  = element_blank(),
    panel.grid  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    legend.position = "none"
  )

# ------------------------------------------------------------
# 2. Inset map: all of BC, with a red box marking the main map's extent
# ------------------------------------------------------------
bc <- bcmaps::bc_bound()  # sf polygon, BC Albers (EPSG:3005)

extent_box <- st_as_sfc(st_bbox(
  c(xmin = unname(pts_bbox["xmin"] - pad),
    xmax = unname(pts_bbox["xmax"] + pad),
    ymin = unname(pts_bbox["ymin"] - pad),
    ymax = unname(pts_bbox["ymax"] + pad)),
  crs = st_crs(allpts)
))

inset_map <- ggplot() +
  geom_sf(data = bc, fill = "grey90", colour = "grey40", linewidth = 0.3) +
  geom_sf(data = extent_box, fill = NA, colour = "red", linewidth = 0.9) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5))

# ------------------------------------------------------------
# 3. Combine: inset placed in the map's top-right corner
# ------------------------------------------------------------
# Adjust left/bottom/right/top (0-1, fraction of the full plot) to
# reposition or resize the inset.
sheep_point_map <- main_map +
  inset_element(inset_map, left = 0.78, bottom = 0.08, right = 0.99, top = 0.30)
sheep_point_map

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","sheep_points_hillshade_map.png"), 
       sheep_point_map, width = 10, height = 9, dpi = 300)








###############################################################################
## PLot the UD per year per individual 

#################################################################################

#allpts <- st_read(fs::path("01_clean_data", "location_steps_all_20260721_TEST.gpkg"))
all_poly<- st_read(fs::path("02_draft_outputs", "sheep_yr_polygons_bbmm30.gpkg"))
all_poly <- all_poly |> filter(th ==50)
all_poly <- left_join(all_poly, id_key, by = c("id" = "tag_idn"))

pts_bbox <- st_bbox(all_poly)
pad <- 3000  # metres of padding around the points, adjust to taste

## Ewes 
ewes <- all_poly |> filter(sex == "female")

main_map <- ggplot() +
  geom_spatraster(data = hillshade, show.legend = FALSE, alpha = 0.4) +
  scale_fill_gradient(low = "grey10", high = "grey95", na.value = NA) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = ewes, aes(colour = factor(year)), size = 1.2, alpha = 0.5) +
  facet_wrap(~id)+
  scale_colour_viridis_d(name = "Year") +
  #scale_colour_manual(name = "Sex", values = c("female" = "firebrick", "male" = "steelblue")) +
  coord_sf(
    xlim = c(pts_bbox["xmin"] - pad, pts_bbox["xmax"] + pad),
    ylim = c(pts_bbox["ymin"] - pad, pts_bbox["ymax"] + pad),
    expand = FALSE
  ) +
  labs(title = "Ewes Home Range per year") +
  theme_minimal() +
  theme(
    axis.title  = element_blank(),
    panel.grid  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank()#,
    #legend.position = "none"
  )
main_map

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","ewes_UD_yr_hillshade_map.png"), 
       main_map, width = 10, height = 10, dpi = 300)

## Males
males <- all_poly |> filter(sex == "male")

main_map <- ggplot() +
  geom_spatraster(data = hillshade, show.legend = FALSE, alpha = 0.4) +
  scale_fill_gradient(low = "grey10", high = "grey95", na.value = NA) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = males, aes(colour = factor(year)), size = 1.2, alpha = 0.5) +
  facet_wrap(~id)+
  scale_colour_viridis_d(name = "Year") +
  #scale_colour_manual(name = "Sex", values = c("female" = "firebrick", "male" = "steelblue")) +
  coord_sf(
    xlim = c(pts_bbox["xmin"] - pad, pts_bbox["xmax"] + pad),
    ylim = c(pts_bbox["ymin"] - pad, pts_bbox["ymax"] + pad),
    expand = FALSE
  ) +
   labs(title = "Rams Home Range per year") +
  theme_minimal() +
  theme(
    axis.title  = element_blank(),
    panel.grid  = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank()#,
    #legend.position = "none"
  )

main_map

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","rams_UD_yr_hillshade_map.png"), 
       main_map, width = 10, height = 10, dpi = 300)




#################################################
# Part 3: Generate a summary plot ##############

# ============================================================================
# Monthly summary of home range area across individuals
# Input: all_poly (sf) with columns id, area, th, month, year, geometry
# ============================================================================

library(sf)
library(dplyr)
library(ggplot2)

# by yr: 
#st_write(all_poly, path("02_draft_outputs", "sheep_yr_polygons_bbmm30.gpkg"))

# by yr and month 
all_poly <- st_read(path("02_draft_outputs", "sheep_month_yr_polygons_bbmm30.gpkg"))
st_crs(all_poly)= 3005


# ---- 1. TIDY UP TYPES --------------------------------------------------------
poly_df <- all_poly |>
  st_drop_geometry() |>                                  # numeric summary/plots don't need geometry
  mutate(
    id    = as.character(id),
    th    = factor(th, levels = c(50, 75, 95)),
    year  = as.character(year),
    month = factor(month.abb[month], levels = month.abb)  # 3 -> "Mar", ordered Jan-Dec
  )

poly_df <- left_join(poly_df, id_key, by = c("id" = "tag_idn"))


# ---- 2. SUMMARY TABLE: area by month x th (pooled across id and year) ------
month_summary <- poly_df |>
  group_by(month, th) |>
  summarise(
    n      = n(),
    mean   = mean(area, na.rm = TRUE),
    median = median(area, na.rm = TRUE),
    sd     = sd(area, na.rm = TRUE),
    se     = sd / sqrt(n),
    .groups = "drop"
  )

month_summary

# same summary but keeping year separate, in case you want to compare years
month_year_summary <- poly_df |>
  group_by(year, month, th) |>
  summarise(
    n      = n(),
    mean   = mean(area, na.rm = TRUE),
    median = median(area, na.rm = TRUE),
    sd     = sd(area, na.rm = TRUE),
    se     = sd / sqrt(n),
    .groups = "drop"
  )

month_year_summary



poly_df_95 <- poly_df |> filter(th == 95) 


# ============================================================================
# 3. PLOTS
# ============================================================================

# ---- 3a. boxplot: every id's area by month, one panel per isopleth ---------
p1 <- ggplot(poly_df, aes(x = month, y = area)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  #geom_jitter(aes(colour = id), width = 0.15, alpha = 0.4, size = 1.5) +
  geom_jitter(colour = "darkblue", width = 0.15, alpha = 0.2, size = 1.5) +
  facet_wrap(~sex, labeller = label_both) +
  labs(x = NULL, y = "area (ha)") +
  theme(legend.position = "none")   # drop if you want the id colour legend
p1


ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","UD_bbmm_by_month.png"), 
       p1, width = 10, height = 7, dpi = 300)






##########################################################################
## compare the annual, summer and winter areas
## summary of areas for each of the polygons generated above
# generate a table with the area of each polygon for each sex and age class for each of the time periods (year, winter, summer)

alyr <- st_read(path("02_draft_outputs", "sheep_yr_polygons_bbmm30.gpkg")) |> mutate(type = "annual")
sum <- st_read(path("02_draft_outputs", "sheep_summer_yr_polygons_bbmm30.gpkg")) |> mutate(type = "summer") |> rename("year" = year_pst)
win <-  st_read(path("02_draft_outputs", "sheep_winter_yr_polygons_bbmm30.gpkg"))|> mutate(type = "winter")|> rename("year" = winter_year)

uds <- rbind(alyr, sum, win) |> 
  left_join(id_key, by = c("id" = "tag_idn")) |> 
  st_drop_geometry() 

# update the age so each year the age is updated based on the year of capture and the year of the polygon.ueat
age_lookup1 <- age_lookup |> 
  select( -Age_annuli, -cap_year) |> #cap_year, -sex) |> 
  rename("year"= year_pst)

uds <- left_join(uds, age_lookup1, by = c("id" = "tag_idn", "year" = "year"))

poly_df_95 <- uds |> filter(th == 95) 

## Tables  
uds_sum <- poly_df_95 |>
  group_by( type, sex) |>
  summarise(count = n(),
            median_area = median(area),
            mean_area = mean(area),
            min_area = min(area),
            max_area = max(area))
# 
# 
# uds_sum <- uds |> 
#   filter(th == 95) |>
#   group_by( type, sex, sheep_class) |> 
#   summarise(count = n(), 
#             median_area = median(area),
#             mean_area = mean(area),
#             min_area = min(area),
#             max_area = max(area))
# 



# ---- 2. SUMMARY TABLE: area by month x th (pooled across id and year) ------
type_summary <- uds |>
  group_by(type, sex, th, Age_annuli_est, sheep_class) |>
  summarise(
    n      = n(),
    mean   = mean(area, na.rm = TRUE),
    median = median(area, na.rm = TRUE),
    sd     = sd(area, na.rm = TRUE),
    se     = sd / sqrt(n),
    .groups = "drop"
  )

type_summary

poly_df_95 <- uds |> filter(th == 95) 


# ============================================================================
# 3. PLOTS with estimated age 
# ============================================================================

# ---- 3a. boxplot: every id's area by month, one panel per isopleth ---------
p1 <- ggplot(uds, aes(x = type, y = area)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(colour = sex), width = 0.15, alpha = 0.4, size = 1.5) +
  #geom_jitter(colour = "darkblue", width = 0.15, alpha = 0.2, size = 1.5) +
  facet_wrap(~Age_annuli_est, labeller = label_both) +
  #labs(x = type, y = "area (ha)") +
  theme()#legend.position = "none")   # drop if you want the id colour legend

p1

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","annual_sum_win_area_sheep_age_est.png"), 
       p1, width = 10, height = 9, dpi = 300)



#########################################

## how to determine the groups 

## Females - based on winter grouping 

overlap_tbl <- read.csv(fs::path("02_draft_outputs", "sheep_winter_yr_overlap_pc.csv")) |> 
  select(-X.1, -X)

id_key1 <- id_key |> rename("id_1" = tag_idn) |> select(id_1, sex) |> 
  mutate(id_1 = as.integer(id_1))

overlap_tbl<- left_join(overlap_tbl, id_key1)

# select 95% for winter 

ot <- overlap_tbl |> 
  filter(th == 95) |>
  filter(pct_overlap_1 >65)

write.csv(ot, fs::path("02_draft_outputs", "06_report_summary_figures","sheep_winter_groups.csv"), row.names = FALSE)


## Rut season - which males are hanging out with which female groups based on rut grouping 

overlap_tbl <- read.csv(fs::path("02_draft_outputs", "sheep_rut_yr_overlap_pc.csv")) |> 
  select( -X)

id_key1 <- id_key |> rename("id_1" = tag_idn) |> select(id_1, sex) |> 
  mutate(id_1 = as.integer(id_1))

overlap_tbl<- left_join(overlap_tbl, id_key1)

# select 95% for winter 

ot <- overlap_tbl |> 
  filter(th == 95) |>
  filter(pct_overlap_1 >65)

write.csv(ot, fs::path("02_draft_outputs", "06_report_summary_figures","sheep_rut_groups.csv"), row.names = FALSE)



# 
# # read in the location data 
# 
# allpts <- read.csv(fs::path("01_clean_data", "location_steps_all_raw.csv")) 
# 
# # filter to ewes and for only breeding period based on julian dates 
# # calculate the julian date for May 1st and June 30th
# Julianday <- function(x) {
#   as.numeric(format(x, "%j"))
# }
# 
# jstart <- Julianday(ymd("2024-05-01"))
# jend <- Julianday(ymd("2024-06-10"))
# #120 - 183
# 
# # get list of ewes
# ewes <- allpts |> 
#   select(sheep_class, tag_idn) |> 
#   filter(sheep_class == "ewe") |> 
#   pull(tag_idn)
# 
# pts <- allpts |> 
#   filter(Julianday >= jstart & Julianday <= jend) |> 
#   select("tag_idn", "Latitude", "Longitude","date_time_pst", "year_pst", "speed_ave","mcp_area_1d","mcp_area_3d" ) |> 
#   mutate(date_time_pst = ymd_hms(date_time_pst)) |> 
#   mutate(date_pst = as_date(date_time_pst)) |> 
#   filter(tag_idn %in% ewes)
# 
# 
# ##############################################################################
# ## Plot 1: combined spatial map + speed profile animation 
# ###############################################################################
# 
# # select only 2024 data 
# pts <- pts %>%
#   filter(year_pst ==2024)
# 
# #unique(pts$tag_idn)
# 
# # select only 2024 data 
# ptsi <- pts %>% filter(tag_idn %in% c( 55670) )
# #ptsi <- ptsi[1:30,]
# 
# 
# # # compute daily average positions and speeds
# df_ave = ptsi %>%
#   mutate(date=as.Date(date_pst)) %>%
#   group_by(tag_idn,date) %>%
#   summarise(
#     lat = mean(Latitude, na.rm = TRUE),
#     lon = mean(Longitude, na.rm = TRUE),
#     spd = mean(speed_ave, na.rm=TRUE)
#   )
# 
# # create 'ideal' data with all combinations of data
# ideal = expand_grid(
#   id = unique(df_ave$tag_idn),
#   date = seq.Date(from = min(df_ave$date), to = max(df_ave$date), by = 1)
# )
# # create complete dataset
# df_all = left_join(ideal,df_ave)
# 
# # 2. Generate clean GPS dataset 
# set.seed(42)
# 
# n_frames <- length(df_all$tag_idn)
# 
# gps_data <- data.frame(
#   time  = df_all$date, #1:n_frames,
#   long  = df_all$lon,
#   lat   = df_all$lat,
#   speed = df_all$spd
# )
# 
# 
# # generate an aoi per tag_id
# gps_data_sf <- st_as_sf(gps_data, coords = c("long", "lat"), crs = 4326)
# bbox <- st_bbox(gps_data_sf)
# bbox <- st_buffer(st_as_sfc(bbox), dist = 0.1) # Add a buffer to ensure we capture all points)
# 
# # clip the dem for processing speed 
# cded_clip <- terra::crop(cded, bbox)
# contour_lines <- terra::as.contour(cded_clip, levels = seq(100, 2000, by = 100))
# conl <- st_as_sf(st_as_sf(contour_lines))
# 
# #terra::plot(contour_lines)
# 
# map_plot = ggplot()+
#   #geom_sf(data = bg)+
#   tidyterra::geom_spatraster(data = cded_clip, alpha = 0.5, show.legend = FALSE) +
#   tidyterra::scale_fill_terrain_c(direction = -1) +
#   #scale_fill_grey() +
#   geom_sf(data = conl, color = "grey", size = 0.5) +
#   coord_sf(xlim = range(gps_data$long, na.rm = TRUE), 
#            ylim = range(gps_data$lat, na.rm = TRUE), 
#            expand = FALSE) +
#   ggnewscale::new_scale_fill() +
#   # lines and points
#   geom_path(data = gps_data,
#             aes(x=long,y=lat,color=speed),
#             alpha = 0.3) +
#   geom_point(data = gps_data,
#              aes(x=long,y=lat,fill=speed),
#              alpha = 0.7, shape=21, size = 2) +
#   # formatting
#   # ggnewscale::new_scale_fill() +
#   scale_fill_viridis_c(option = "inferno")+
#   scale_color_viridis_c(option = "inferno")+
#   scale_size_continuous(range = c(0.1,10))+
#   labs(x=NULL, y=NULL, 
#        fill = 'Speed (m/s)', 
#        color = 'Speed (m/s)',
#        title = 'Date: {frame_along}')+
#   #labs(title = 'Year: {time}', x = 'GDP per capita', y = 'life expectancy') +
#   #theme_dark()+
#   #theme(panel.grid = element_blank())+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         rect = element_blank())+
#   transition_reveal(time)
# 
# map_plot
# 
# 
# 
# 
# 
# 
# 
# ##############################################################################
# ## Plot 2: All ewes movement over lambing period - 2024
# ###############################################################################
# 
# 
# pts <- allpts |> 
#   filter(Julianday >= jstart & Julianday <= jend) |> 
#   select("tag_idn", "Latitude", "Longitude","date_time_pst", "year_pst", "speed_ave","mcp_area_1d","mcp_area_3d" ) |> 
#   mutate(date_time_pst = ymd_hms(date_time_pst)) |> 
#   mutate(date_pst = as_date(date_time_pst)) |> 
#   filter(tag_idn %in% ewes)
# 
# # select only 2024 data 
# pts <- pts %>%
#   filter(year_pst ==2024)
# 
# unique(pts$tag_idn)
# 
# # select only 2024 data 
# ptsi <- pts #%>% filter(tag_idn %in% c( 55670) )
# #ptsi <- ptsi[1:30,]
# 
# 
# # # compute daily average positions and speeds
# df_ave = ptsi %>%
#   mutate(date=as.Date(date_pst)) %>%
#   group_by(tag_idn,date) %>%
#   summarise(
#     lat = mean(Latitude, na.rm = TRUE),
#     lon = mean(Longitude, na.rm = TRUE),
#     spd = mean(speed_ave, na.rm=TRUE)
#   )
# 
# # create 'ideal' data with all combinations of data
# ideal = expand_grid(
#   id = unique(df_ave$tag_idn),
#   date = seq.Date(from = min(df_ave$date), to = max(df_ave$date), by = 1)
# )
# # create complete dataset
# df_all = left_join(ideal,df_ave)
# 
# 
# 
# # 2. Generate clean GPS dataset 
# set.seed(42)
# 
# n_frames <- length(df_all$tag_idn)
# 
# gps_data <- data.frame(
#   time  = df_all$date, #1:n_frames,
#   long  = df_all$lon,
#   lat   = df_all$lat,
#   id = as.character(df_all$tag_idn)
# )
# 
# # 3. Create the Map Animation (Plot 1)
# # Note: Swap out theme_minimal for a real bounding map layer if desired
# # map_plot <- ggplot(gps_data, aes(x = long, y = lat)) +
# #   geom_path(color = "darkgreen", linewidth = 1) +
# #   geom_point(color = "red", size = 4) +
# #   theme_light() +
# #   labs(title = "GPS Locations", x = "Longitude", y = "Latitude") +
# #   transition_reveal(time)
# 
# 
# # generate an aoi per tag_id
# gps_data_sf <- st_as_sf(gps_data, coords = c("long", "lat"), crs = 4326)
# bbox <- st_bbox(gps_data_sf)
# bbox <- st_buffer(st_as_sfc(bbox), dist = 0.1) # Add a buffer to ensure we capture all points)
# 
# # clip the dem for processing speed 
# cded_clip <- terra::crop(cded, bbox)
# contour_lines <- terra::as.contour(cded_clip, levels = seq(100, 2000, by = 100))
# conl <- st_as_sf(st_as_sf(contour_lines))
# 
# #terra::plot(contour_lines)
# 
# map_plot = ggplot()+
#   #geom_sf(data = bg)+
#   tidyterra::geom_spatraster(data = cded_clip, alpha = 0.5, show.legend = FALSE) +
#   tidyterra::scale_fill_terrain_c(direction = -1) +
#   #scale_fill_viridis_c(name = "elevation") +
#   #geom_sf(data = conl, color = "grey", size = 0.5) +
#   coord_sf(xlim = range(gps_data$long, na.rm = TRUE), 
#            ylim = range(gps_data$lat, na.rm = TRUE), 
#            expand = FALSE) +
#   # lines and points
#   ggnewscale::new_scale_fill() +
#   geom_path(data = gps_data,
#             aes(x=long,y=lat,#color=id,
#                 group = id),
#             alpha = 0.3) +
#   # formatting
#   geom_point(data = gps_data,
#              aes(x=long,y=lat,fill=id, group = id),
#              alpha = 0.7, shape=21, size = 2) +
#   # scale_fill_viridis_d(option = "inferno")+
#   scale_color_viridis_d(option = "inferno")+
#   scale_size_continuous(range = c(0.1,10))+
#   labs(x=NULL, y=NULL, 
#        fill = 'IDs', 
#        #color = 'Speed (m/s)',
#        title = 'Date: {frame_along}')+
#   #theme(panel.grid = element_blank())+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         rect = element_blank())+
#   transition_reveal(time)
# 
# map_plot
# 
# 
# # 5. Render frames as image lists in memory
# #message("Rendering map frames...")
# #map_anim <- animate(map_plot, nframes = n_frames, fps = 2, width = 400, height = 400, renderer = magick_renderer())
# 
# final_output_path <- file.path(out_dir, "all_ewes_2024_all.gif")
# anim_save(final_output_path, animation = map_plot)
# 
# ## 7. Compile stitched frames and save directly to your drive
# #final_gif <- image_animate(map_anim, fps = 2)
# #final_output_path <- file.path(out_dir, "all_ewes_2024.gif")
# #
# #image_write(final_gif, path = final_output_path)
# #message("Success! File saved to: ", final_output_path)
# 
# 
# 
# 
# ##############################################################################
# ## Plot 3: All ewes movement over lambing period - 2025
# ###############################################################################
# 
# pts <- allpts |> 
#   filter(Julianday >= jstart & Julianday <= jend) |> 
#   select("tag_idn", "Latitude", "Longitude","date_time_pst", "year_pst", "speed_ave","mcp_area_1d","mcp_area_3d" ) |> 
#   mutate(date_time_pst = ymd_hms(date_time_pst)) |> 
#   mutate(date_pst = as_date(date_time_pst)) |> 
#   filter(tag_idn %in% ewes)
# 
# # select only 2024 data 
# pts <- pts %>%
#   filter(year_pst ==2025)
# 
# unique(pts$tag_idn)
# 
# ptsi <- pts 
# 
# # # compute daily average positions and speeds
# df_ave = ptsi %>%
#   mutate(date=as.Date(date_pst)) %>%
#   group_by(tag_idn,date) %>%
#   summarise(
#     lat = mean(Latitude, na.rm = TRUE),
#     lon = mean(Longitude, na.rm = TRUE),
#     spd = mean(speed_ave, na.rm=TRUE)
#   )
# 
# # create 'ideal' data with all combinations of data
# ideal = expand_grid(
#   id = unique(df_ave$tag_idn),
#   date = seq.Date(from = min(df_ave$date), to = max(df_ave$date), by = 1)
# )
# # create complete dataset
# df_all = left_join(ideal,df_ave)
# 
# # 2. Generate clean GPS dataset 
# set.seed(42)
# 
# n_frames <- length(df_all$tag_idn)
# 
# gps_data <- data.frame(
#   time  = df_all$date, #1:n_frames,
#   long  = df_all$lon,
#   lat   = df_all$lat,
#   id = as.character(df_all$tag_idn)
# )
# 
# # generate an aoi per tag_id
# gps_data_sf <- st_as_sf(gps_data, coords = c("long", "lat"), crs = 4326)
# bbox <- st_bbox(gps_data_sf)
# bbox <- st_buffer(st_as_sfc(bbox), dist = 0.1) # Add a buffer to ensure we capture all points)
# 
# # clip the dem for processing speed 
# cded_clip <- terra::crop(cded, bbox)
# contour_lines <- terra::as.contour(cded_clip, levels = seq(100, 2000, by = 100))
# conl <- st_as_sf(st_as_sf(contour_lines))
# 
# 
# map_plot25 = ggplot()+
#   #geom_sf(data = bg)+
#   tidyterra::geom_spatraster(data = cded_clip, alpha = 0.5, show.legend = FALSE) +
#   scale_fill_viridis_c(name = "elevation") +
#   #geom_sf(data = conl, color = "grey", size = 0.5) +
#   coord_sf(xlim = range(gps_data$long, na.rm = TRUE), 
#            ylim = range(gps_data$lat, na.rm = TRUE), 
#            expand = FALSE) +
#   # lines and points
#   geom_path(data = gps_data,
#             aes(x=long,y=lat,#color=id,
#                 group = id),
#             alpha = 0.3) +
#   # formatting
#   ggnewscale::new_scale_fill() +
#   geom_point(data = gps_data,
#              aes(x=long,y=lat,fill=id, group = id),
#              alpha = 0.7, shape=21, size = 2) +
#   # scale_fill_viridis_d(option = "inferno")+
#   scale_color_viridis_d(option = "inferno")+
#   scale_size_continuous(range = c(0.1,10))+
#   labs(x=NULL, y=NULL, 
#        fill = 'Tag ID', 
#        #color = 'Speed (m/s)',
#        title = 'Date: {frame_along}')+
#   #theme(panel.grid = element_blank())+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         rect = element_blank())+
#   transition_reveal(time)
# 
# map_plot25
# 
# 
# 
# # 5. Render frames as image lists in memory
# message("Rendering map frames...")
# #map_anim <- animate(map_plot, nframes = n_frames, fps = 2, width = 400, height = 400, renderer = magick_renderer())
# 
# final_output_path <- file.path(out_dir, "all_ewes_2025_all.gif")
# 
# anim_save(final_output_path, animation = map_plot25)
# 
