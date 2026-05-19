# read in the formatted data and estimate step length and distance

library(dplyr)
library(sf)
library(fs)
library(readxl)
library(lubridate)
library(hms)
library(ggplot2)
library(tidyverse)
library(geosphere)
library(sp)
library(adehabitatLT)


# read in the summary data 

clean_dir <- fs::path("01_clean_data")
out_dir <- fs::path("02_draft_outputs/01_lamb_figures")

allpts <- read.csv(fs::path("01_clean_data", "location_steps_all_raw.csv")) 


# extract date, x, y, tagid. 
pts <- allpts |> 
  select(date_time_pst, X, Y, tag_idn) |> 
  st_as_sf(coords = c("X", "Y"), crs = 3005)


# get elevation  data - virtual raster
aoi = st_read(fs::path("00_raw_data", "aoi.gpkg"))
cded_raw <- bcmaps::cded(aoi)
cded <- terra::rast(cded_raw) 

cded_3005 <- terra::project(cded, "EPSG:3005")
names(cded_3005) <- "dem"

# generate slope / roughness / aspect
slope <- terra::terrain(cded_3005, v = "slope", unit = "degrees")
aspect <- terra::terrain(cded_3005, v = "aspect", unit = "degrees")
roughness <- terra::terrain(cded_3005, v = "roughness")
tri <-  terra::terrain(cded_3005, v = "TRI")

rastss <- c(cded_3005, slope, aspect, roughness, tri)


pts <- terra::extract(rastss, pts) 




# remove unwated cols 
pts <- allpts |> 
  select(-X.1, -CollarSerialNumber , -Hdop,-NumSats, -FixTime, -Year , -Hour, -Minute, -X2D.3D, 
         -Date, -Time.Zone, -tag_id.x, -tag_id.y, -capture_date, -time,-date_pst.x,
         -Recorder, -Capture_GPS_Zone_NAD_83., -Northing, -Westing,-x,-y,
         -Animal_WLH, -Eartag, -End_Date, -Comments, - date_time,
         -lat_prior, -long_prior  , -time_prior , -cos_turn, -gps_spike     
  ) |> 
  mutate(date_time_pst = ymd_hms(date_time_pst)) |> 
  mutate(date_pst = as_date(date_pst.y)) 





