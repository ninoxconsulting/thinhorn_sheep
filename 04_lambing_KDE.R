#04_KDE_lambing

# Summary 
# this script uses the mlambing points generates in 03_lambing_events.R to 
# generate KDE polygons for each ewe for each year at 50, 75, 90% UD. 
# generates lambing plot summaries. 

library(dplyr)
library(sf)
library(fs)
library(readxl)
library(lubridate)
library(hms)
library(ggplot2)
library(tidyverse)
#library(geosphere)
library(sp)
library(adehabitatLT)
library(adehabitatHR)


# read in the summary data 

clean_dir <- fs::path("01_clean_data")
out_dir <- fs::path("02_draft_outputs/01_lamb_figures_20260721")

# get a list of all the ewes 
ewes <- list.files (out_dir, pattern = ".gpkg")
#ewes <- ewes[1:3]

#####################################################
## Part 2: generate the polygons per lambing period per year 

# loop through the combinations ofyear and id 

all_ewes <- purrr::map(ewes, function(x){
  
  #x <- ewes[1]
  pts <- st_read(fs::path(out_dir, x))
  
  all_pts <- cbind(pts, st_coordinates(pts)) |> 
    select(tag_idn, date_time_pst, X, Y, year_pst)

  all_pts <- all_pts |> st_drop_geometry()
  
  sheep_yrs <- unique(all_pts$year_pst) 
    
 
    # loop through the combinations of month and year and id 
    all_poly <- purrr::map(sheep_yrs, function(xx){
      
      print(xx)
      #xx = sheep_yrs[1]
      
      dbi <- all_pts |> 
        filter(year_pst == xx)
      
      if(nrow(dbi) >= 5) {
        
        #  kde: h reference parameter
        dbisf <- st_as_sf(dbi, coords = c("X", "Y"), crs = 3005)
        
        dbisp <- dbisf |> 
          select(tag_idn) |> 
          as("Spatial")
        
        # # define the parameters (h, kern, grid, extent) 
        kde_href  <- kernelUD(dbisp, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)
        
        # add a try statement to skip to next line if error is produced in vers95
        
        ver95_sf <- tryCatch({
          ver95 <- getverticeshr(kde_href,95) # get vertices for home range
          st_as_sf(ver95) |> 
            mutate(th = 95)        # convert to sf object 
        }, error = function(e) {
          return(NULL) # return NULL if error occurs
        })
        
        ver75_sf <- tryCatch({
          ver75 <- getverticeshr(kde_href,75)
          st_as_sf(ver75 )|> 
            mutate(th = 75)
        }, error = function(e) {
          return(NULL) # return NULL if error occurs
        })
        
        ver50_sf <- tryCatch({
          ver50 <- getverticeshr(kde_href,50)
          st_as_sf(ver50) |> 
            mutate(th = 50)
        }, error = function(e) {
          return(NULL) # return NULL if error occurs
        })
        
        # if it is not null the bind 
        if(!is.null(ver95_sf) & !is.null(ver75_sf) & !is.null(ver50_sf)) {
          allvers <- bind_rows( ver95_sf, ver75_sf , ver50_sf)
          #allvers$month_pst = unique(dbi$month_pst)
          allvers$year = unique(dbi$year_pst)
          
          return(allvers)
        }
      } else {
        return(NULL)
      }
      
    }) |> bind_rows()
  
  all_poly
  
})|> bind_rows()
    

st_write(all_ewes, path("02_draft_outputs","02_lamb_kde", "lambing_kde_yr_polygons.gpkg"))
    
    
#############################################################
## Rerun this using a BBMM home range estimate (as used by Grace Enns paper )
#############################################################


# loop through the combinations ofyear and id 

all_ewes <- purrr::map(ewes, function(x){
  
  #x <- ewes[1]
  pts <- st_read(fs::path(out_dir, x))
  #crs(pts)
  
  all_pts <- cbind(pts, st_coordinates(pts)) |> 
    select(tag_idn, date_time_pst, X, Y, year_pst)
  
  all_pts <- all_pts |> st_drop_geometry()
  
  sheep_yrs <- unique(all_pts$year_pst) 
  
  # loop through the combinations of month and year and id 
  all_poly <- purrr::map(sheep_yrs, function(xx){
    
    print(xx)
   # xx = sheep_yrs[1]
    
    dbi <- all_pts |> 
      filter(year_pst == xx)
    
    if(nrow(dbi) >= 5) {
      
      # # #  kde: bbmm model 
      #  dbisf <- st_as_sf(dbi, coords = c("X", "Y"), crs = 3005)
      # # 
      #  dbisp <- dbisf |> 
      #    select(tag_idn) |> 
      #    as("Spatial")
      # 
      # 
      # 1. build a trajector# 1. build a trajector# 1. build a trajectory object
      tr <- as.ltraj(xy = dbi[, c("X", "Y")], date = dbi$date_time_pst, id = dbi$tag_idn)
      
      # 2. estimate sig1 (Brownian motion variance) given a fixed GPS error (sig2)
      sig2 <- 30   # location error in metres -- adjust to your GPS spec
      lik  <- liker(tr, sig2 = sig2, rangesig1 = c(0.1, 500))
      sig1 <- lik[[1]]$sig1
      
      # 3. run the Brownian Bridge Movement Model
      bbmm <- kernelbb(tr, sig1 = sig1, sig2 = sig2, grid = 100)
      
      # 4. view it / pull out a home-range contour
      #image(bbmm)                                # UD raster
      #hr95 <- getverticeshr(bbmm, percent = 95)  # 95% isopleth as a polygon
      #plot(hr95)
      
      # add a try statement to skip to next line if error is produced in vers95
      
      ver95_sf <- tryCatch({
        ver95 <- getverticeshr(bbmm,95) # get vertices for home range
        st_as_sf(ver95) |> 
          mutate(th = 95)        # convert to sf object 
      }, error = function(e) {
        return(NULL) # return NULL if error occurs
      })
      
      ver75_sf <- tryCatch({
        ver75 <- getverticeshr(bbmm,75)
        st_as_sf(ver75 )|> 
          mutate(th = 75)
      }, error = function(e) {
        return(NULL) # return NULL if error occurs
      })
      
      ver50_sf <- tryCatch({
        ver50 <- getverticeshr(bbmm,50)
        st_as_sf(ver50) |> 
          mutate(th = 50)
      }, error = function(e) {
        return(NULL) # return NULL if error occurs
      })
      
      # if it is not null the bind 
      if(!is.null(ver95_sf) & !is.null(ver75_sf) & !is.null(ver50_sf)) {
        allvers <- bind_rows( ver95_sf, ver75_sf , ver50_sf)
        allvers$id = unique(dbi$tag_idn)
        allvers$year = unique(dbi$year_pst)
        
        return(allvers)
      }
    } else {
      return(NULL)
    }
    
  }) |> bind_rows()
  
  all_poly
  
})|> bind_rows()

st_crs(all_ewes) <- 3005
st_write(all_ewes, path("02_draft_outputs","02_lamb_kde", "lambing_bbmm_yr_30m_polygons.gpkg"))#, append = F)
#st_write(all_ewes, path("02_draft_outputs","02_lamb_kde", "lambing_bbmm_yr_polygons.gpkg"))#, append = F)



################################################################################

# Compare the href and bbmm kde versions - testing for lambing period 

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1. READ + TAG METHOD ---------------------------------------------------
href_path <- path("02_draft_outputs","02_lamb_kde", "lambing_kde_yr_polygons.gpkg")
bbmm_path <- path("02_draft_outputs","02_lamb_kde", "lambing_bbmm_yr_polygons.gpkg")  

href_sf <- st_read(href_path, quiet = TRUE) |> mutate(method = "href")
bbmm_sf <- st_read(bbmm_path, quiet = TRUE) |> mutate(method = "bbmm") |> 
  mutate(id = as.character(id))

hr_sf <- bind_rows(href_sf, bbmm_sf) |>
  mutate(
    th     = factor(th, levels = c(50, 75, 95)),
    id     = as.character(id),
    year   = as.character(year),
    method = factor(method, levels = c("href", "bbmm"))
  )

hr_df <- hr_sf |> st_drop_geometry()   # non-spatial version for tables/stats


# ============================================================================
# 2. NUMBERS
# ============================================================================

# ---- 2a. side-by-side table: one row per id x year x th, href & bbmm area ---
hr_wide <- hr_df |>
  select(id, year, th, method, area) |>
  pivot_wider(names_from = method, values_from = area) |>
  mutate(
    diff_ha  = bbmm - href,
    pct_diff = 100 * (bbmm - href) / href     # + means BBMM larger than href
  ) |>
  arrange(id, year, th)

hr_wide

# ---- 2b. summary stats per isopleth level -----------------------------------
summary_by_th <- hr_wide |>
  group_by(th) |>
  summarise(
    n           = n(),
    mean_href   = mean(href, na.rm = TRUE),
    mean_bbmm   = mean(bbmm, na.rm = TRUE),
    mean_pct_diff = mean(pct_diff, na.rm = TRUE),
    median_pct_diff = median(pct_diff, na.rm = TRUE),
    rmse        = sqrt(mean(diff_ha^2, na.rm = TRUE)),
    mae         = mean(abs(diff_ha), na.rm = TRUE),
    pearson_r   = cor(href, bbmm, method = "pearson", use = "complete.obs"),
    spearman_r  = cor(href, bbmm, method = "spearman", use = "complete.obs"),
    .groups = "drop"
  )

summary_by_th

# ---- 2c. paired test per th: does BBMM systematically differ from href? -----
# Wilcoxon signed-rank (paired), robust to the skew typical of home-range areas
wilcox_by_th <- hr_wide |>
  group_by(th) |>
  summarise(
    p_value = wilcox.test(bbmm, href, paired = TRUE)$p.value,
    .groups = "drop"
  )

wilcox_by_th


# ============================================================================
# 3. PLOTS
# ============================================================================

# ---- 3a. href vs bbmm scatter, 1:1 line, one panel per isopleth -------------
ggplot(hr_wide, aes(x = href, y = bbmm, colour = year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~th, scales = "free", labeller = label_both) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "href area", y = "BBMM area",
       title = "BBMM vs href home range area (log-log, dashed = 1:1)")

# ---- 3b. area distributions by method, one panel per isopleth --------------
ggplot(hr_df, aes(x = method, y = area, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  facet_wrap(~th, scales = "free_y", labeller = label_both) +
  scale_y_log10() +
  labs(y = "area (log scale)", x = NULL,
       title = "Home range area by method and isopleth level") +
  theme(legend.position = "none")

# ---- 3c. percent difference (BBMM vs href) by isopleth ----------------------
ggplot(hr_wide, aes(x = th, y = pct_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(aes(colour = year), width = 0.15, alpha = 0.7) +
  labs(x = "isopleth (%)", y = "% difference (BBMM - href) / href",
       title = "How much BBMM diverges from href, by isopleth level")

# ---- 3d. area trend across years, per id, coloured by method ---------------
ggplot(hr_df, aes(x = year, y = area, colour = method, group = interaction(id, method))) +
  geom_line(alpha = 0.5) +
  geom_point(size = 1.5) +
  facet_wrap(~th, scales = "free_y", labeller = label_both) +
  labs(x = "year", y = "area", title = "Area by year, per animal, href vs BBMM")


# generate a plot per id 


# get a list of all the ewes 
ewes <- list.files (out_dir, pattern = ".gpkg")

# loop through the combinations ofyear and id 

all_ewes <- purrr::map(ewes, function(x){
  
  x <- ewes[1]
  pts <- st_read(fs::path(out_dir, x))
  
  all_pts <- cbind(pts, st_coordinates(pts)) |> 
    select(tag_idn, date_time_pst, X, Y, year_pst)
  
  xid <- unique(all_pts$tag_idn)
 
  sheep_yrs <- unique(all_pts$year_pst) 
  
  allyrs <- purrr::map(sheep_yrs, function(xx){
    
    print(xx)
    xx = sheep_yrs[1]
    
    dbi <- all_pts |> 
      filter(year_pst == xx)
    
    # # ---- 3e. spatial overlay for ONE id/year -- eyeball how the shapes differ ---
    # focus_id   <- unique(hr_sf$id)[1]     # <-- change to inspect a specific animal
    # focus_year <- unique(hr_sf$year)[1]   # <-- change to inspect a specific year
    
    hr_sf |>
      filter(id == xid, year == xx) |>
      ggplot() +
      geom_sf(aes(colour = method, linetype = th), fill = NA, linewidth = 0.9) +
      labs(colour = "Method", linetype = "Isopleth (%)",
           title = paste("href vs BBMM contours -- id", xid, "year", xx))

    
    ggplot() +
      geom_point(data = dbi, aes(x = X, y = Y),
                 colour = "grey30", size = 0.8, alpha = 0.5) +
      geom_sf(data = hr_sf |> filter(id == xid, year == xx),
              aes(colour = method, linetype = th), fill = NA, linewidth = 0.9) +
      coord_sf(datum = st_crs(hr_sf)) +
      labs(colour = "Method", linetype = "Isopleth (%)",
           title = paste("href vs BBMM contours -- id", focus_id, "year", focus_year),
           subtitle = paste(nrow(dbi), "GPS fixes shown"))















# ============================================================
# Map KDE home range isopleths for all ewes, by year
# ------------------------------------------------------------
# Expected input: an sf object (e.g. `kde_sf`) with columns:
#   id    - ewe tag ID
#   area  - polygon area
#   th    - KDE isopleth/contour level (e.g. 50, 75, 95)
#   year  - year of the estimate
#   geom  - MULTIPOLYGON geometry
# ============================================================
library(ggplot2)
library(sf)
library(terra)
library(tidyterra)   # geom_spatraster() -- lets ggplot2 plot SpatRaster objects
library(dplyr)
library(elevatr) 

kde_sf <- st_read(path("02_draft_outputs","02_lamb_kde", "lambing_kde_yr_polygons.gpkg"))

# read in the dem if needed 
dem_crop <- rast(path("02_draft_outputs","02_lamb_kde", "lambing_kde_dem.tif"))

if(file.exists(dem_crop)) {
  dem_crop <- rast(path("02_draft_outputs","02_lamb_kde", "lambing_kde_dem.tif"))
} else {
  
# ------------------------------------------------------------
# 1. Download a DEM covering your study area
# ------------------------------------------------------------
# elevatr pulls from AWS Terrain Tiles (global) or Open Topography.
# It accepts an sf object directly and works out the extent itself.
#
# z = zoom level, 1 (coarse) to 14 (finest). Roughly:
#   z = 10 -> ~150 m/pixel   z = 12 -> ~40 m/pixel   z = 14 -> ~10 m/pixel
# Higher z = much larger download/slower -- 12 is a good default for a
# home-range-sized study area.
buffer_m <- 2000  # extra margin around the KDE extent, in metres
aoi_sf <- st_read(path("00_raw_data", "aoi.gpkg")) |> 
  st_bbox(kde_sf) |> st_as_sfc() |> st_buffer(buffer_m) |> st_as_sf()

dem_raw <- get_elev_raster(aoi_sf, z = 12, clip = "bbox")
dem <- rast(dem_raw)

if (crs(dem) != st_crs(kde_sf)$wkt) {
  dem <- project(dem, st_crs(kde_sf)$wkt)
}

# ------------------------------------------------------------
# 2. Crop the DEM to the buffered KDE extent
# ------------------------------------------------------------
# (elevatr's clip="bbox" already limits the download to roughly this area,
# but tiles can extend slightly beyond it -- crop tightens it up exactly)
kde_extent <- vect(aoi_sf)
dem_crop <- crop(dem, kde_extent)

writeRaster(dem_crop, path("02_draft_outputs","02_lamb_kde", "lambing_kde_dem.tif"), overwrite = TRUE)

}


# ------------------------------------------------------------
# 3. Compute hillshade (slope + aspect -> shaded relief)
# ------------------------------------------------------------
slope_r  <- terrain(dem_crop, v = "slope",  unit = "radians")
aspect_r <- terrain(dem_crop, v = "aspect", unit = "radians")
hillshade <- shade(slope_r, aspect_r, angle = 45, direction = 315)  # sun altitude/azimuth


# ------------------------------------------------------------
# 4. Order th so the core (50%) contour draws on top
# ------------------------------------------------------------
kde_sf <- kde_sf %>%
  mutate(th = factor(th, levels = c(95, 75, 50)))

kde_sf_50 <- kde_sf %>%
  filter(th == 50)


# ------------------------------------------------------------
# 5. Zoom extent -- tied to the KDE polygons themselves, not the
#    (larger, buffered) hillshade raster extent
# ------------------------------------------------------------
bbox_50 <- st_bbox(kde_sf_50)
pad_x <- (bbox_50["xmax"] - bbox_50["xmin"]) * 0.05  # 5% padding so lines aren't clipped at the edge
pad_y <- (bbox_50["ymax"] - bbox_50["ymin"]) * 0.05

zoom_xlim <- c(bbox_50["xmin"] - pad_x, bbox_50["xmax"] + pad_x)
zoom_ylim <- c(bbox_50["ymin"] - pad_y, bbox_50["ymax"] + pad_y)

# ------------------------------------------------------------
# OPTION A: small multiples (one row per ewe, one column per year)
# with the same hillshade behind every panel
# ------------------------------------------------------------

kde_sf_50 <- kde_sf_50 |> 
  filter(id %in% c("55670", "55672"))

# ------------------------------------------------------------
# OPTION A: small multiples (one row per ewe, one column per year)
# with the same (subtle) hillshade behind every panel
# ------------------------------------------------------------
# With many ewes (e.g. 17 x 3 years = 51 panels), one grid gets
# very crowded. Wrap the plotting code in a function and split the
# ewe IDs into two roughly equal groups -- e.g. 9 ewes / 8 ewes --
# so each figure stays a manageable, readable size.


make_kde_grid <- function(kde_data, plot_title) {
  ggplot() +
    geom_spatraster(data = hillshade, show.legend = FALSE, alpha = 0.5) +
    scale_fill_gradient(low = "grey45", high = "grey92", na.value = NA) +
    ggnewscale::new_scale_fill() +
    geom_sf(data = kde_data, fill = "#1f5c8a", colour = "grey20", linewidth = 0.2, alpha = 0.6) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
    coord_sf(xlim = zoom_xlim, ylim = zoom_ylim, expand = FALSE) +
    facet_grid(id ~ year) +
    labs(title = plot_title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text     = element_blank(),
      axis.ticks    = element_blank(),
      strip.text.y  = element_text(angle = 0, size = 8),
      strip.text.x  = element_text(size = 9, face = "bold"),
      panel.spacing = unit(0.3, "lines"),
      panel.grid    = element_blank()
    )
}

# Split ewe IDs into two roughly equal groups (adjust split point as needed --
# e.g. group by age class or capture site instead of a simple half-split,
# if that groups more naturally for your dataset)
ewe_ids   <- sort(unique(kde_sf_50$id))
half      <- ceiling(length(ewe_ids) / 2)
ids_fig1  <- ewe_ids[1:half]
ids_fig2  <- ewe_ids[(half + 1):length(ewe_ids)]

kde_map_grid_1 <- make_kde_grid(
  kde_sf_50 %>% filter(id %in% ids_fig1),
  paste0("Ewe Core Lambing Kernel Density Home Ranges (", ids_fig1[1], "\u2013", ids_fig1[length(ids_fig1)], ")")
)
kde_map_grid_2 <- make_kde_grid(
  kde_sf_50 %>% filter(id %in% ids_fig2),
  paste0("Ewe Core (50%) Kernel Density Home Ranges (", ids_fig2[1], "\u2013", ids_fig2[length(ids_fig2)], ")")
)

kde_map_grid_1
kde_map_grid_2


# ------------------------------------------------------------
# OPTION B: single overlay map per year, all ewes together,
# over the same subtle hillshade
# ------------------------------------------------------------
kde_map_overlay <- ggplot() +
  geom_spatraster(data = hillshade, show.legend = FALSE, alpha = 0.5) +
  scale_fill_gradient(low = "grey45", high = "grey92", na.value = NA) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = kde_sf_50, aes(fill = factor(id)), colour = NA, alpha = 0.55) +
  scale_fill_viridis_d(name = "Ewe ID") +
  coord_sf(xlim = zoom_xlim, ylim = zoom_ylim, expand = FALSE) +
  facet_wrap(~ year, nrow = 3) +
  labs(title = "Ewe Core Lambing Kernel Density Home Ranges by Year",
       subtitle = "May 1 - July 31st") +
  theme_minimal() +
  theme( axis.text     = element_blank(),
         axis.ticks    = element_blank(),
         panel.grid = element_blank())

kde_map_overlay


# ------------------------------------------------------------
# OPTION C: facet by EWE ONLY, with years overlaid as colour
# within each panel -- avoids the large id x year crossed grid
# from Option A. Panel count drops from (n_ewes x n_years) down
# to just n_ewes, and it directly shows how each ewe's core range
# shifts year to year, which is often the real question of interest.
#
# scales = "free" lets each ewe's panel zoom to its own extent
# (their ranges may sit in quite different specific locations),
# rather than sharing one extent across all ewes.
# ------------------------------------------------------------
kde_map_by_id <- ggplot() +
  geom_spatraster(data = hillshade, show.legend = FALSE, alpha = 0.5) +
  scale_fill_gradient(low = "grey45", high = "grey92", na.value = NA) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = kde_sf_50, aes(fill = factor(year), colour = factor(year)),
          alpha = 0.35, linewidth = 0.4) +
  scale_fill_viridis_d(name = "Year", option = "plasma") +
  scale_colour_viridis_d(name = "Year", option = "plasma") +
  facet_wrap(~ id, scales = "free", ncol = 5) +
  labs(title = "Ewe Core (50%) Kernel Density Home Ranges",
       subtitle = "Years overlaid within each ewe's panel") +
  theme_minimal() +
  theme(
    axis.text  = element_text(size = 5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8, face = "bold"),
    panel.grid = element_blank()
  )

kde_map_by_id

# If you have many ewes and even this gets crowded, split it across
# multiple pages instead of one giant grid (install.packages("ggforce")):
# library(ggforce)
# kde_map_by_id +
#   facet_wrap_paginate(~ id, scales = "free", ncol = 4, nrow = 4, page = 1)
# # increase `page` to see subsequent pages; check n_pages() first:
# n_pages(kde_map_by_id + facet_wrap_paginate(~ id, ncol = 4, nrow = 4))


# ------------------------------------------------------------
# Optional: save either map
# ------------------------------------------------------------
# ggsave("kde_home_ranges_grid_1.png", kde_map_grid_1, width = 6, height = 12, dpi = 300)
# ggsave("kde_home_ranges_grid_2.png", kde_map_grid_2, width = 6, height = 12, dpi = 300)
# ggsave("kde_home_ranges_overlay.png", kde_map_overlay, width = 11, height = 6, dpi = 300)
# ggsave("kde_home_ranges_by_id.png", kde_map_by_id, width = 13, height = 10, dpi = 300)