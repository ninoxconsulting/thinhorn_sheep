#04_Kernal_density_estimates 

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
out_dir <- fs::path("02_draft_outputs/01_lamb_figures")

allpts <- read.csv(fs::path("01_clean_data", "location_steps_all.csv")) 

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

pts_sf <- pts |> 
  st_as_sf(coords = c("X", "Y"), crs = 3005)

st_write(pts_sf, path("01_clean_data", "location_steps_all.gpkg"))




#####################################################
## Part 2: generate the polygons per month  and year

all_pts <- pts |> 
  mutate(month_pst = month(date_time_pst)) |> 
  select(tag_idn, date_time_pst, X, Y, month_pst, year_pst)

# convert to sp object
all_pts_sp <- all_pts|> 
  st_as_sf(coords = c("X", "Y"), crs = 3005) |>
  as("Spatial") 

all_pts <- all_pts |> st_drop_geometry()

taglsm <- all_pts |> select(tag_idn, month_pst, year_pst) |> unique()

taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag_idn)))

#taglsm <- taglsm[1:3]

# loop through the combinations of month and year and id 
all_poly <- purrr::map(taglsm$id, function(x){
  
  print(x)
  #x = taglsm$id[376]  
  taglsmi <- taglsm[x,]
  
  dbi <- all_pts |> 
    filter(tag_idn == taglsmi$tag_idn) |> 
    filter(month_pst == taglsmi$month_pst) |> 
    filter(year_pst == taglsmi$year_pst)
  
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
    allvers$month_pst = unique(dbi$month_pst)
    allvers$year = unique(dbi$year_pst)
    
    return(allvers)
  }
  } else {
    return(NULL)
  }
  
}) |> bind_rows()

st_write(all_poly, path("02_draft_outputs", "sheep_month_yr_polygons.gpkg"))


### generate per season 

# breeding season:  May 1 - June 15th (as per Enns et al + refernces) - see other scripts 
# 




# 
# 
# 
# #####################################################
# ## Part 2: generate the polygons over breeding period for each ewe 
# 
# all_pts <- pts |> 
#   mutate(month_pst = month(date_time_pst)) |> 
#   select(tag_idn, date_time_pst, X, Y, month_pst, year_pst)
# 
# # convert to sp object
# all_pts_sp <- all_pts|> 
#   st_as_sf(coords = c("X", "Y"), crs = 3005) |>
#   as("Spatial") 
# 
# all_pts <- all_pts |> st_drop_geometry()
# 
# taglsm <- all_pts |> select(tag_idn, month_pst, year_pst) |> unique()
# 
# taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag_idn)))
# 
# #taglsm <- taglsm[1:3]
# 
# # loop through the combinations of month and year and id 
# all_poly <- purrr::map(taglsm$id, function(x){
#   
#   print(x)
#   #x = taglsm$id[376]  
#   taglsmi <- taglsm[x,]
#   
#   dbi <- all_pts |> 
#     filter(tag_idn == taglsmi$tag_idn) |> 
#     filter(month_pst == taglsmi$month_pst) |> 
#     filter(year_pst == taglsmi$year_pst)
#   
#   if(nrow(dbi) >= 5) {
#     
#     #  kde: h reference parameter
#     dbisf <- st_as_sf(dbi, coords = c("X", "Y"), crs = 3005)
#     
#     dbisp <- dbisf |> 
#       select(tag_idn) |> 
#       as("Spatial")
#     
#     # # define the parameters (h, kern, grid, extent) 
#     kde_href  <- kernelUD(dbisp, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)
#     
#     # add a try statement to skip to next line if error is produced in vers95
#     
#     ver95_sf <- tryCatch({
#       ver95 <- getverticeshr(kde_href,95) # get vertices for home range
#       st_as_sf(ver95) |> 
#         mutate(th = 95)        # convert to sf object 
#     }, error = function(e) {
#       return(NULL) # return NULL if error occurs
#     })
#     
#     ver75_sf <- tryCatch({
#       ver75 <- getverticeshr(kde_href,75)
#       st_as_sf(ver75 )|> 
#         mutate(th = 75)
#     }, error = function(e) {
#       return(NULL) # return NULL if error occurs
#     })
#     
#     ver50_sf <- tryCatch({
#       ver50 <- getverticeshr(kde_href,50)
#       st_as_sf(ver50) |> 
#         mutate(th = 50)
#     }, error = function(e) {
#       return(NULL) # return NULL if error occurs
#     })
#     
#     # if it is not null the bind 
#     if(!is.null(ver95_sf) & !is.null(ver75_sf) & !is.null(ver50_sf)) {
#       allvers <- bind_rows( ver95_sf, ver75_sf , ver50_sf)
#       allvers$month_pst = unique(dbi$month_pst)
#       allvers$year = unique(dbi$year_pst)
#       
#       return(allvers)
#     }
#   } else {
#     return(NULL)
#   }
#   
# }) |> bind_rows()
# 
# st_write(all_poly, path("02_draft_outputs", "sheep_month_yr_polygons.gpkg"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## Summarise the dataset by tag id and no of months 
# ## how many birds have more than 1 year and more than one month 
# 
# 
# summary_db <- all_polys |> 
#   st_drop_geometry() |> 
#   group_by(id) |> 
#   summarise(n_months = n_distinct(month),
#             n_years = n_distinct(year)) 
# 
# 
# ## note we can filter this down to 50% percentage 
# 
# all_poly_50 <- all_polys |> filter(th == 50)
# 
# # ggplot 
# 
# global_href <- ggplot(data = Americas) +
#   geom_sf(color = "grey") +
#   #geom_sf(data = dbsf,  size = 1, alpha = 0.2,colour = "blue") +
#   geom_sf(data = all_pts,  size = 1, alpha = 0.2,colour = "lightblue") +
#   #geom_sf(data = all_poly_50, linewidth = 0.5, alpha = 0.4, fill = "red")+
#   geom_sf(data = all_poly_50,  alpha = 0.4,fill = "red")+
#   xlab("Longitude") + ylab("Latitude") +
#   #coord_sf(xlim = c(-75, -74.35), ylim = c(38.8, 39.4), expand = FALSE)+
#   coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE)+
#   theme_bw()+
#   facet_wrap(~month)+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())
# 
# global_href