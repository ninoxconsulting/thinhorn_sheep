
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

out_dir <- fs::path("02_draft_outputs/01_lamb_figures")

# get basemap data version 1: 
bg = ne_countries(scale = "medium", continent = 'north america', returnclass = "sf")
bg <- bg |> select(admin, continent)

# get elevation  data - virtual raster
aoi = st_read(fs::path("00_raw_data", "aoi.gpkg"))
cded_raw <- bcmaps::cded(aoi)
cded <- terra::rast(cded_raw) 

cded_3005 <- terra::project(cded, "EPSG:3005")
names(cded) <- "file80e410842c5"


# read in the location data 

allpts <- read.csv(fs::path("01_clean_data", "location_steps_all_raw.csv")) 

# filter to ewes and for only breeding period based on julian dates 
# calculate the julian date for May 1st and June 30th
Julianday <- function(x) {
  as.numeric(format(x, "%j"))
}

jstart <- Julianday(ymd("2024-05-01"))
jend <- Julianday(ymd("2024-06-10"))
#120 - 183

# get list of ewes
ewes <- allpts |> 
  select(sheep_class, tag_idn) |> 
  filter(sheep_class == "ewe") |> 
  pull(tag_idn)

pts <- allpts |> 
  filter(Julianday >= jstart & Julianday <= jend) |> 
  select("tag_idn", "Latitude", "Longitude","date_time_pst", "year_pst", "speed_ave","mcp_area_1d","mcp_area_3d" ) |> 
  mutate(date_time_pst = ymd_hms(date_time_pst)) |> 
  mutate(date_pst = as_date(date_time_pst)) |> 
  filter(tag_idn %in% ewes)
  

##############################################################################
## Plot 1: combined spatial map + speed profile animation 
###############################################################################

# select only 2024 data 
pts <- pts %>%
  filter(year_pst ==2024)

#unique(pts$tag_idn)

# select only 2024 data 
ptsi <- pts %>% filter(tag_idn %in% c( 55670) )
#ptsi <- ptsi[1:30,]


# # compute daily average positions and speeds
df_ave = ptsi %>%
  mutate(date=as.Date(date_pst)) %>%
  group_by(tag_idn,date) %>%
  summarise(
    lat = mean(Latitude, na.rm = TRUE),
    lon = mean(Longitude, na.rm = TRUE),
    spd = mean(speed_ave, na.rm=TRUE)
  )
 
 # create 'ideal' data with all combinations of data
 ideal = expand_grid(
   id = unique(df_ave$tag_idn),
   date = seq.Date(from = min(df_ave$date), to = max(df_ave$date), by = 1)
 )
 # create complete dataset
df_all = left_join(ideal,df_ave)
 
# 2. Generate clean GPS dataset 
set.seed(42)

n_frames <- length(df_all$tag_idn)

gps_data <- data.frame(
  time  = df_all$date, #1:n_frames,
  long  = df_all$lon,
  lat   = df_all$lat,
  speed = df_all$spd
)


# generate an aoi per tag_id
gps_data_sf <- st_as_sf(gps_data, coords = c("long", "lat"), crs = 4326)
bbox <- st_bbox(gps_data_sf)
bbox <- st_buffer(st_as_sfc(bbox), dist = 0.1) # Add a buffer to ensure we capture all points)

# clip the dem for processing speed 
cded_clip <- terra::crop(cded, bbox)
contour_lines <- terra::as.contour(cded_clip, levels = seq(100, 2000, by = 100))
conl <- st_as_sf(st_as_sf(contour_lines))

#terra::plot(contour_lines)

map_plot = ggplot()+
     #geom_sf(data = bg)+
     tidyterra::geom_spatraster(data = cded_clip, alpha = 0.5, show.legend = FALSE) +
     tidyterra::scale_fill_terrain_c(direction = -1) +
     #scale_fill_grey() +
     geom_sf(data = conl, color = "grey", size = 0.5) +
     coord_sf(xlim = range(gps_data$long, na.rm = TRUE), 
              ylim = range(gps_data$lat, na.rm = TRUE), 
              expand = FALSE) +
     ggnewscale::new_scale_fill() +
     # lines and points
     geom_path(data = gps_data,
               aes(x=long,y=lat,color=speed),
               alpha = 0.3) +
     geom_point(data = gps_data,
                aes(x=long,y=lat,fill=speed),
                alpha = 0.7, shape=21, size = 2) +
     # formatting
    # ggnewscale::new_scale_fill() +
     scale_fill_viridis_c(option = "inferno")+
     scale_color_viridis_c(option = "inferno")+
     scale_size_continuous(range = c(0.1,10))+
    labs(x=NULL, y=NULL, 
          fill = 'Speed (m/s)', 
          color = 'Speed (m/s)',
         title = 'Date: {frame_along}')+
  #labs(title = 'Year: {time}', x = 'GDP per capita', y = 'life expectancy') +
    #theme_dark()+
    #theme(panel.grid = element_blank())+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())+
  transition_reveal(time)

map_plot

# 4. Create the Speed Animation (Plot 2)
speed_plot <- ggplot(gps_data, aes(x = time, y = speed)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  theme_light() +
  labs(title = "Speed Profile", x = "Time (seconds)", y = "Speed (km/h)") +
  transition_reveal(time)

#speed_plot

# 5. Render frames as image lists in memory
# Both MUST use identical nframes and dimensions for clean alignment
message("Rendering map frames...")
map_anim <- animate(map_plot, nframes = n_frames, fps = 2, width = 400, height = 400, renderer = magick_renderer())

message("Rendering speed frames...")
speed_anim <- animate(speed_plot, nframes = n_frames, fps = 2, width = 400, height = 400, renderer = magick_renderer())

# 6. Side-by-Side Stitching using Magick
message("Stitching animations side-by-side...")
combined_frames <- image_append(c(map_anim[1], speed_anim[1])) # Initialize structure

for(i in 2:n_frames) {
  combined_frames <- c(combined_frames, image_append(c(map_anim[i], speed_anim[i])))
}

# 7. Compile stitched frames and save directly to your drive
final_gif <- image_animate(combined_frames, fps = 2)
final_output_path <- file.path(out_dir, "synchronized_gps_speed.gif")

image_write(final_gif, path = final_output_path)
message("Success! File saved to: ", final_output_path)









##############################################################################
## Plot 2: All ewes movement over lambing period - 2024
###############################################################################


pts <- allpts |> 
  filter(Julianday >= jstart & Julianday <= jend) |> 
  select("tag_idn", "Latitude", "Longitude","date_time_pst", "year_pst", "speed_ave","mcp_area_1d","mcp_area_3d" ) |> 
  mutate(date_time_pst = ymd_hms(date_time_pst)) |> 
  mutate(date_pst = as_date(date_time_pst)) |> 
  filter(tag_idn %in% ewes)

# select only 2024 data 
pts <- pts %>%
  filter(year_pst ==2024)

unique(pts$tag_idn)

# select only 2024 data 
ptsi <- pts #%>% filter(tag_idn %in% c( 55670) )
#ptsi <- ptsi[1:30,]


# # compute daily average positions and speeds
df_ave = ptsi %>%
  mutate(date=as.Date(date_pst)) %>%
  group_by(tag_idn,date) %>%
  summarise(
    lat = mean(Latitude, na.rm = TRUE),
    lon = mean(Longitude, na.rm = TRUE),
    spd = mean(speed_ave, na.rm=TRUE)
  )

# create 'ideal' data with all combinations of data
ideal = expand_grid(
  id = unique(df_ave$tag_idn),
  date = seq.Date(from = min(df_ave$date), to = max(df_ave$date), by = 1)
)
# create complete dataset
df_all = left_join(ideal,df_ave)



# 2. Generate clean GPS dataset 
set.seed(42)

n_frames <- length(df_all$tag_idn)

gps_data <- data.frame(
  time  = df_all$date, #1:n_frames,
  long  = df_all$lon,
  lat   = df_all$lat,
  id = as.character(df_all$tag_idn)
)

# 3. Create the Map Animation (Plot 1)
# Note: Swap out theme_minimal for a real bounding map layer if desired
# map_plot <- ggplot(gps_data, aes(x = long, y = lat)) +
#   geom_path(color = "darkgreen", linewidth = 1) +
#   geom_point(color = "red", size = 4) +
#   theme_light() +
#   labs(title = "GPS Locations", x = "Longitude", y = "Latitude") +
#   transition_reveal(time)


# generate an aoi per tag_id
gps_data_sf <- st_as_sf(gps_data, coords = c("long", "lat"), crs = 4326)
bbox <- st_bbox(gps_data_sf)
bbox <- st_buffer(st_as_sfc(bbox), dist = 0.1) # Add a buffer to ensure we capture all points)

# clip the dem for processing speed 
cded_clip <- terra::crop(cded, bbox)
contour_lines <- terra::as.contour(cded_clip, levels = seq(100, 2000, by = 100))
conl <- st_as_sf(st_as_sf(contour_lines))

#terra::plot(contour_lines)

map_plot = ggplot()+
  #geom_sf(data = bg)+
  tidyterra::geom_spatraster(data = cded_clip, alpha = 0.5, show.legend = FALSE) +
  tidyterra::scale_fill_terrain_c(direction = -1) +
  #scale_fill_viridis_c(name = "elevation") +
  #geom_sf(data = conl, color = "grey", size = 0.5) +
  coord_sf(xlim = range(gps_data$long, na.rm = TRUE), 
           ylim = range(gps_data$lat, na.rm = TRUE), 
           expand = FALSE) +
  # lines and points
  ggnewscale::new_scale_fill() +
  geom_path(data = gps_data,
            aes(x=long,y=lat,#color=id,
                group = id),
            alpha = 0.3) +
  # formatting
  geom_point(data = gps_data,
             aes(x=long,y=lat,fill=id, group = id),
             alpha = 0.7, shape=21, size = 2) +
 # scale_fill_viridis_d(option = "inferno")+
  scale_color_viridis_d(option = "inferno")+
  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL, 
       fill = 'IDs', 
       #color = 'Speed (m/s)',
       title = 'Date: {frame_along}')+
 #theme(panel.grid = element_blank())+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())+
 transition_reveal(time)

map_plot


# 5. Render frames as image lists in memory
#message("Rendering map frames...")
#map_anim <- animate(map_plot, nframes = n_frames, fps = 2, width = 400, height = 400, renderer = magick_renderer())

final_output_path <- file.path(out_dir, "all_ewes_2024_all.gif")
anim_save(final_output_path, animation = map_plot)

## 7. Compile stitched frames and save directly to your drive
#final_gif <- image_animate(map_anim, fps = 2)
#final_output_path <- file.path(out_dir, "all_ewes_2024.gif")
#
#image_write(final_gif, path = final_output_path)
#message("Success! File saved to: ", final_output_path)




##############################################################################
## Plot 3: All ewes movement over lambing period - 2025
###############################################################################

pts <- allpts |> 
  filter(Julianday >= jstart & Julianday <= jend) |> 
  select("tag_idn", "Latitude", "Longitude","date_time_pst", "year_pst", "speed_ave","mcp_area_1d","mcp_area_3d" ) |> 
  mutate(date_time_pst = ymd_hms(date_time_pst)) |> 
  mutate(date_pst = as_date(date_time_pst)) |> 
  filter(tag_idn %in% ewes)

# select only 2024 data 
pts <- pts %>%
  filter(year_pst ==2025)

unique(pts$tag_idn)

ptsi <- pts 

# # compute daily average positions and speeds
df_ave = ptsi %>%
  mutate(date=as.Date(date_pst)) %>%
  group_by(tag_idn,date) %>%
  summarise(
    lat = mean(Latitude, na.rm = TRUE),
    lon = mean(Longitude, na.rm = TRUE),
    spd = mean(speed_ave, na.rm=TRUE)
  )

# create 'ideal' data with all combinations of data
ideal = expand_grid(
  id = unique(df_ave$tag_idn),
  date = seq.Date(from = min(df_ave$date), to = max(df_ave$date), by = 1)
)
# create complete dataset
df_all = left_join(ideal,df_ave)

# 2. Generate clean GPS dataset 
set.seed(42)

n_frames <- length(df_all$tag_idn)

gps_data <- data.frame(
  time  = df_all$date, #1:n_frames,
  long  = df_all$lon,
  lat   = df_all$lat,
  id = as.character(df_all$tag_idn)
)

# generate an aoi per tag_id
gps_data_sf <- st_as_sf(gps_data, coords = c("long", "lat"), crs = 4326)
bbox <- st_bbox(gps_data_sf)
bbox <- st_buffer(st_as_sfc(bbox), dist = 0.1) # Add a buffer to ensure we capture all points)

# clip the dem for processing speed 
cded_clip <- terra::crop(cded, bbox)
contour_lines <- terra::as.contour(cded_clip, levels = seq(100, 2000, by = 100))
conl <- st_as_sf(st_as_sf(contour_lines))


map_plot25 = ggplot()+
  #geom_sf(data = bg)+
  tidyterra::geom_spatraster(data = cded_clip, alpha = 0.5, show.legend = FALSE) +
  scale_fill_viridis_c(name = "elevation") +
  #geom_sf(data = conl, color = "grey", size = 0.5) +
  coord_sf(xlim = range(gps_data$long, na.rm = TRUE), 
           ylim = range(gps_data$lat, na.rm = TRUE), 
           expand = FALSE) +
  # lines and points
  geom_path(data = gps_data,
            aes(x=long,y=lat,#color=id,
                group = id),
            alpha = 0.3) +
  # formatting
  ggnewscale::new_scale_fill() +
  geom_point(data = gps_data,
             aes(x=long,y=lat,fill=id, group = id),
             alpha = 0.7, shape=21, size = 2) +
  # scale_fill_viridis_d(option = "inferno")+
  scale_color_viridis_d(option = "inferno")+
  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL, 
       fill = 'Tag ID', 
       #color = 'Speed (m/s)',
       title = 'Date: {frame_along}')+
  #theme(panel.grid = element_blank())+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())+
  transition_reveal(time)

map_plot25



# 5. Render frames as image lists in memory
message("Rendering map frames...")
#map_anim <- animate(map_plot, nframes = n_frames, fps = 2, width = 400, height = 400, renderer = magick_renderer())

final_output_path <- file.path(out_dir, "all_ewes_2025_all.gif")

anim_save(final_output_path, animation = map_plot25)

 

###########################################################################
# Plot 4: Side-by-Side Synchronized Map for 2024 and 2025 yrs 
###########################################################################
# 
# # Render frames as image lists in memory
# message("Rendering map frames...")
# map_anim24 <- animate(map_plot, nframes = n_frames, fps = 2, width = 400, height = 400, renderer = magick_renderer())
# 
# message("Rendering speed frames...")
# map_anim25 <- animate(map_plot25, nframes = n_frames, fps = 2, width = 400, height = 400, renderer = magick_renderer())
# 
# # 6. Side-by-Side Stitching using Magick
# message("Stitching animations side-by-side...")
# combined_frames <- image_append(c(map_anim24[1], map_anim25[1])) # Initialize structure
# 
# for(i in 2:n_frames) {
#   combined_frames <- c(combined_frames, image_append(c(map_anim24[i], map_anim25[i])))
# }
# 
# # 7. Compile stitched frames and save directly to your drive
# final_gif <- image_animate(combined_frames, fps = 2)
# final_output_path <- file.path(out_dir, "breeding_2024_25_plots.gif")
# image_write(final_gif, path = final_output_path)
# 










# # compute daily average positions and speeds
# df_ave = allpts %>%
#   mutate(date=as.Date(date_pst.x)) %>%
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
# 
# # create complete dataset
# df_all = left_join(ideal,df_ave)
# 
# 
# p = ggplot()+
#   geom_sf(data = bg)+
#   coord_sf(xlim = c(-130, -128), #range(df_all$lon, na.rm = TRUE), 
#            ylim = c(57.4, 58), #range(df_all$lat, na.rm = TRUE), 
#            expand = FALSE) +
#   # lines and points
#   geom_path(data = df_all,
#             aes(x=lon,y=lat,group=id,color=spd),
#             alpha = 0.3) +
#   geom_point(data = df_all,
#              aes(x=lon,y=lat,group=id,fill=spd),
#              alpha = 0.7, shape=21, size = 2) +
#   
#   # formatting
#   scale_fill_viridis_c(option = "inferno")+
#   scale_color_viridis_c(option = "inferno")+
#   scale_size_continuous(range = c(0.1,10))+
#   labs(x=NULL, y=NULL, 
#        fill = 'Speed (m/s)', 
#        color = 'Speed (m/s)')+
#   theme_dark()+
#   theme(panel.grid = element_blank())
# p
# 
# 
# # animate
# anim = p + 
#   transition_reveal(along = date)+
#   ease_aes('linear')+
#   ggtitle("Date: {frame_along}")
# 
# animate(anim, nframes = 365, fps = 10)

































# 1. Generate clean mock GPS data
set.seed(42)
gps_data <- data.frame(
  time  = 1:20,
  long  = cumsum(runif(20, -0.01, 0.01)) - 73.98,
  lat   = cumsum(runif(20, -0.01, 0.01)) + 40.75,
  speed = runif(20, 5, 25)
)

# 2. Reshape data into long format for faceting
# This binds X and Y coordinates under a single column structure
animated_data <-  ggplot(gps_data, aes(long, lat, color = id)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "GPS Movement", x = "Longitude", y = "Latitude") +
  #transition_reveal(time) # Key for animation
  #rename(Map_X = long, Map_Y = lat, Speed_X = time, Speed_Y = speed) %>%
  #pivot_longer(
  #  cols = everything(),
  #  names_to = c("plot_type", ".value"),
  #  names_pattern = "(.*)_(.*)"
  #) %>%
  # Match the true time variable back to the Map rows so transition works
  mutate(time_step = rep(1:20, each = 2)) 

# 3. Build the synchronized single plot
sync_plot <- ggplot(animated_data, aes(x = X, y = Y)) +
  # Draw paths and points dynamically
  geom_path(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  # Split into Map and Speed panels with independent axes
  facet_wrap(~plot_type, scales = "free", ncol = 1) +
  theme_minimal() +
  labs(
    title = "GPS Tracking & Speed Metric",
    subtitle = "Frame time step: {frame_along}",
    x = "Longitude / Time",
    y = "Latitude / Speed (km/h)"
  ) +
  # Single transition rule binds both panels to the exact same clock
  transition_reveal(time_step)

# 4. Render and explicitly save to a specific output path
output_file_path <- fs::path(out_dir, "gps_movement_test.gif") # Edit this path

animate(
  sync_plot, 
  nframes = 40, 
  fps = 5, 
  width = 600, 
  height = 800,
  renderer = gifski_renderer(output_file_path)
)



library(ggplot2)
library(gganimate)
library(dplyr)
library(magick)

# 1. Configure custom output directories
output_dir <- out_dir # Edit this path
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. Generate clean mock GPS data
set.seed(42)
n_frames <- 30
gps_data <- data.frame(
  time  = 1:n_frames,
  long  = cumsum(runif(n_frames, -0.005, 0.005)) - 73.98,
  lat   = cumsum(runif(n_frames, -0.005, 0.005)) + 40.75,
  speed = runif(n_frames, 5, 25)
)

# 3. Create the Map Animation (Plot 1)
# Note: Swap out theme_minimal for a real bounding map layer if desired
map_plot <- ggplot(gps_data, aes(x = long, y = lat)) +
  geom_path(color = "darkgreen", linewidth = 1) +
  geom_point(color = "red", size = 4) +
  theme_light() +
  labs(title = "GPS Vehicle Track", x = "Longitude", y = "Latitude") +
  transition_reveal(time)

# 4. Create the Speed Animation (Plot 2)
speed_plot <- ggplot(gps_data, aes(x = time, y = speed)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  theme_light() +
  labs(title = "Speed Profile", x = "Time (seconds)", y = "Speed (km/h)") +
  transition_reveal(time)

# 5. Render frames as image lists in memory
# Both MUST use identical nframes and dimensions for clean alignment
message("Rendering map frames...")
map_anim <- animate(map_plot, nframes = n_frames, fps = 5, width = 400, height = 400, renderer = magick_renderer())

message("Rendering speed frames...")
speed_anim <- animate(speed_plot, nframes = n_frames, fps = 5, width = 400, height = 400, renderer = magick_renderer())

# 6. Side-by-Side Stitching using Magick
message("Stitching animations side-by-side...")
combined_frames <- image_append(c(map_anim[1], speed_anim[1])) # Initialize structure

for(i in 2:n_frames) {
  combined_frames <- c(combined_frames, image_append(c(map_anim[i], speed_anim[i])))
}

# 7. Compile stitched frames and save directly to your drive
final_gif <- image_animate(combined_frames, fps = 5)
final_output_path <- file.path(output_dir, "synchronized_gps_speed.gif")

image_write(final_gif, path = final_output_path)
message("Success! File saved to: ", final_output_path)
