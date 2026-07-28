#04_Kernal_density_estimates 
# this script uses the adehabitatHR package to generate kernal density estimates for kDEs 
# based on 1) each month and year for each individual sheep, 2) lambing period (see other script)
# 3) rutting period 

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

# use .gpkg as csv drops time stamp
allpts <- st_read(fs::path("01_clean_data", "location_steps_all_20260721_TEST.gpkg"))

# add X and Y columns 
allpts <- cbind(allpts, st_coordinates(allpts))

# generate a key to be used in plots below
id_key <- allpts |> 
  select(tag_idn, sex, Age_annuli, sheep_class) |>
  st_drop_geometry() |> 
  unique() 


############################################################################
## Part 1: generate the polygons year

all_pts <- allpts |> 
  select(tag_idn, date_time_pst, X, Y, year_pst) |> 
  st_drop_geometry()

taglsm <- all_pts |> select(tag_idn, year_pst) |> unique()

taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag_idn)))
taglsmids <- unique(taglsm$id)

# loop through the combinations of month and year and id 
all_poly <- purrr::map(taglsmids, function(x){
  
  print(x)
  #x = taglsm$id[376]  
  taglsmi <- taglsm |> filter(id == x)
  
  dbi <- all_pts |> 
    filter(tag_idn == taglsmi$tag_idn) |> 
    filter(year_pst == taglsmi$year_pst)
  
  if(nrow(dbi) >= 5) {
    
    # 1. build a trajector# 1. build a trajector# 1. build a trajectory object
    tr <- as.ltraj(xy = dbi[, c("X", "Y")], date = dbi$date_time_pst, id = dbi$tag_idn)
    
    # 2. estimate sig1 (Brownian motion variance) given a fixed GPS error (sig2)
    sig2 <- 30   # location error in metres -- adjust to your GPS spec
    lik  <- liker(tr, sig2 = sig2, rangesig1 = c(0.1, 500),plotit = F)
    sig1 <- lik[[1]]$sig1
    
    # 3. run the Brownian Bridge Movement Model
    bbmm <- kernelbb(tr, sig1 = sig1, sig2 = sig2, grid = 100)
    
    # output vericies for 95%, 75% and 50% home range polygon 
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

st_crs(all_poly) <- 3005
st_write(all_poly, path("02_draft_outputs", "sheep_yr_polygons_bbmm30.gpkg"))






############################################################################
## Part 2: generate the polygons per month  and year

all_pts <- allpts |> 
  mutate(month_pst = month(date_time_pst)) |> 
  select(tag_idn, date_time_pst, X, Y, month_pst, year_pst) |> 
  st_drop_geometry()


taglsm <- all_pts |> select(tag_idn, month_pst, year_pst) |> unique()

taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag_idn)))
taglsmids <- unique(taglsm$id)

# loop through the combinations of month and year and id 
all_poly <- purrr::map(taglsmids, function(x){
  
  print(x)
  #x = taglsm$id[376]  
  taglsmi <- taglsm |> filter(id == x)
  
  dbi <- all_pts |> 
    filter(tag_idn == taglsmi$tag_idn) |> 
    filter(month_pst == taglsmi$month_pst) |> 
    filter(year_pst == taglsmi$year_pst)
  
  if(nrow(dbi) >= 5) {
  
    # 1. build a trajector# 1. build a trajector# 1. build a trajectory object
    tr <- as.ltraj(xy = dbi[, c("X", "Y")], date = dbi$date_time_pst, id = dbi$tag_idn)
    
    # 2. estimate sig1 (Brownian motion variance) given a fixed GPS error (sig2)
    sig2 <- 30   # location error in metres -- adjust to your GPS spec
    lik  <- liker(tr, sig2 = sig2, rangesig1 = c(0.1, 500),plotit = F)
    sig1 <- lik[[1]]$sig1
    
    # 3. run the Brownian Bridge Movement Model
    bbmm <- kernelbb(tr, sig1 = sig1, sig2 = sig2, grid = 100)
    
    # output vericies for 95%, 75% and 50% home range polygon 
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
      allvers$month = unique(dbi$month_pst)
      allvers$year = unique(dbi$year_pst)
      
      return(allvers)
    }
  } else {
    return(NULL)
  }

}) |> bind_rows()


st_crs(all_poly) <- 3005
st_write(all_poly, path("02_draft_outputs", "sheep_month_yr_polygons_bbmm30.gpkg"))





############################################################################
## Part 3: Winter season ## 
## winter defined as Dec 1 to March 31st (winter year starts in the previous year)

all_pts <- allpts |> 
  mutate(month_pst = month(date_time_pst)) |> 
  select(tag_idn, date_time_pst, X, Y, month_pst, year_pst) |> 
  mutate(winter_year = ifelse(month_pst %in% c(12), year_pst + 1, year_pst)) |>
  filter(month_pst %in% c(12, 1, 2,3))|> 
  st_drop_geometry()


taglsm <- all_pts |> select(tag_idn, winter_year) |> unique()

taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag_idn)))
taglsmids <- unique(taglsm$id)

# loop through the combinations of month and year and id 
all_poly <- purrr::map(taglsmids, function(x){
  
  print(x)
  #x = taglsm$id[50]  
  taglsmi <- taglsm |> filter(id == x)
  
  dbi <- all_pts |> 
    filter(tag_idn == taglsmi$tag_idn) |> 
    filter(winter_year == taglsmi$winter_year)
  
  if(nrow(dbi) >= 5) {
    
    # 1. build a trajector# 1. build a trajector# 1. build a trajectory object
    tr <- as.ltraj(xy = dbi[, c("X", "Y")], date = dbi$date_time_pst, id = dbi$tag_idn)
    
    # 2. estimate sig1 (Brownian motion variance) given a fixed GPS error (sig2)
    sig2 <- 30   # location error in metres -- adjust to your GPS spec
    lik  <- liker(tr, sig2 = sig2, rangesig1 = c(0.1, 500),plotit = F)
    sig1 <- lik[[1]]$sig1
    
    # 3. run the Brownian Bridge Movement Model
    bbmm <- kernelbb(tr, sig1 = sig1, sig2 = sig2, grid = 100)
    
    # output vericies for 95%, 75% and 50% home range polygon 
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
      #allvers$month = unique(dbi$month_pst)
      allvers$winter_year = unique(dbi$winter_year)
      
      return(allvers)
    }
  } else {
    return(NULL)
  }
  
}) |> bind_rows()


st_crs(all_poly) <- 3005
st_write(all_poly, path("02_draft_outputs", "sheep_winter_yr_polygons_bbmm30.gpkg"))



############################################################################
## Part 4: Summer season ## 
## summer date range - July/August? Based on timing dates for females in Enns paper  


all_pts <- allpts |> 
  mutate(month_pst = month(date_time_pst)) |> 
  select(tag_idn, date_time_pst, X, Y, month_pst, year_pst) |> 
  #mutate(winter_year = ifelse(month_pst %in% c(12), year_pst + 1, year_pst)) |>
  filter(month_pst %in% c(7,8))|> 
  st_drop_geometry()


taglsm <- all_pts |> select(tag_idn, year_pst) |> unique()

taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag_idn)))
taglsmids <- unique(taglsm$id)

# loop through the combinations of month and year and id 
all_poly <- purrr::map(taglsmids, function(x){
  
  print(x)
  #x = taglsm$id[50]  
  taglsmi <- taglsm |> filter(id == x)
  
  dbi <- all_pts |> 
    filter(tag_idn == taglsmi$tag_idn) |> 
    filter(year_pst == taglsmi$year_pst)
  
  if(nrow(dbi) >= 5) {
    
    # 1. build a trajector# 1. build a trajector# 1. build a trajectory object
    tr <- as.ltraj(xy = dbi[, c("X", "Y")], date = dbi$date_time_pst, id = dbi$tag_idn)
    
    # 2. estimate sig1 (Brownian motion variance) given a fixed GPS error (sig2)
    sig2 <- 30   # location error in metres -- adjust to your GPS spec
    lik  <- liker(tr, sig2 = sig2, rangesig1 = c(0.1, 500),plotit = F)
    sig1 <- lik[[1]]$sig1
    
    # 3. run the Brownian Bridge Movement Model
    bbmm <- kernelbb(tr, sig1 = sig1, sig2 = sig2, grid = 100)
    
    # output vericies for 95%, 75% and 50% home range polygon 
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
      #allvers$month = unique(dbi$month_pst)
      allvers$year_pst = unique(dbi$year_pst)
      
      return(allvers)
    }
  } else {
    return(NULL)
  }
  
}) |> bind_rows()


st_crs(all_poly) <- 3005
st_write(all_poly, path("02_draft_outputs", "sheep_summer_yr_polygons_bbmm30.gpkg"))



############################################################################
## Part 4: Rut season 
## Oct / november

all_pts <- allpts |> 
  mutate(month_pst = month(date_time_pst)) |> 
  select(tag_idn, date_time_pst, X, Y, month_pst, year_pst) |> 
  #mutate(winter_year = ifelse(month_pst %in% c(12), year_pst + 1, year_pst)) |>
  filter(month_pst %in% c(10,11))|> 
  st_drop_geometry()


taglsm <- all_pts |> select(tag_idn, year_pst) |> unique()

taglsm <- taglsm |> mutate(id = seq(1:length(taglsm$tag_idn)))
taglsmids <- unique(taglsm$id)

# loop through the combinations of month and year and id 
all_poly <- purrr::map(taglsmids, function(x){
  
  print(x)
  #x = taglsm$id[50]  
  taglsmi <- taglsm |> filter(id == x)
  
  dbi <- all_pts |> 
    filter(tag_idn == taglsmi$tag_idn) |> 
    filter(year_pst == taglsmi$year_pst)
  
  if(nrow(dbi) >= 5) {
    
    # 1. build a trajector# 1. build a trajector# 1. build a trajectory object
    tr <- as.ltraj(xy = dbi[, c("X", "Y")], date = dbi$date_time_pst, id = dbi$tag_idn)
    
    # 2. estimate sig1 (Brownian motion variance) given a fixed GPS error (sig2)
    sig2 <- 30   # location error in metres -- adjust to your GPS spec
    lik  <- liker(tr, sig2 = sig2, rangesig1 = c(0.1, 500),plotit = F)
    sig1 <- lik[[1]]$sig1
    
    # 3. run the Brownian Bridge Movement Model
    bbmm <- kernelbb(tr, sig1 = sig1, sig2 = sig2, grid = 100)
    
    # output vericies for 95%, 75% and 50% home range polygon 
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
      #allvers$month = unique(dbi$month_pst)
      allvers$year_pst = unique(dbi$year_pst)
      
      return(allvers)
    }
  } else {
    return(NULL)
  }
  
}) |> bind_rows()


st_crs(all_poly) <- 3005
st_write(all_poly, path("02_draft_outputs", "sheep_rut_yr_polygons_bbmm30.gpkg"))



