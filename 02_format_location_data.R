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
library(mcp)
library(adehabitatHR)

# read in the summary data 

data_dir <- fs::path("00_raw_data")

# read in ref data
ref <- read.csv(fs::path("01_clean_data", "reference_edit.csv")) |> select(-X)
ref <- ref |> 
  mutate(tag_idn = gsub("-", "", tag_id))

# read in location data 
#loc_dir <- fs::path(data_dir, "20260325")
loc_dir <- fs::path(data_dir, "20260624")

list.files(loc_dir)
#file_i <- fs::path(loc_dir, "Cumulative_D_13197_202623181141.txt")
file_i <- fs::path(loc_dir,"Cumulative_D_13197_202653101756.txt")

tfile <- read.table(file_i, header = T, sep = ",")|> 
  mutate(tag_id = as.character(CollarSerialNumber )) |>
  mutate(Date = ymd(Date)) 


# 1) update tag ids for re-collared individuals 
# need to split out the tag_ids to reflect the -1 and -2 versions 

unique(tfile$tag_id)
# Duplicate tags                    
# 55669-1
# 55669-2 # 2025-04-03
# #
# 55688-1	2024-03-04
# 55688-2	2025-04-02
# 
# 55691-1	2024-03-02
# 55691-2	2025-04-03
# 
# 55706-1	2024-03-02
# 55706-2	2025-04-02
# 
# 55680-1	2024-03-05
# 55680-2	2024-03-27
# 
# 55686-1	2024-03-04
# 55686-2	2024-03-10

# need to update the tag_id for these records based on the date threshold

tfile <- tfile |> 
  mutate(tag_idn = case_when(
    tag_id == 55669 & Date < ymd("2024-06-01") ~ "556691",
    tag_id == 55669 & Date >= ymd("2025-04-03") ~ "556692",
    tag_id == 55685 & Date < ymd("2024-08-15") ~ "556851",
    tag_id == 55685 & Date >= ymd("2025-04-03") ~ "556852",
    tag_id == 55688 & Date < ymd("2024-09-05") ~ "556881",
    tag_id == 55688 & Date >= ymd("2025-04-02") ~ "556882",
    tag_id == 55691 & Date < ymd("2024-09-05") ~ "556911",
    tag_id == 55691 & Date >= ymd("2025-04-03") ~ "556912",
    tag_id == 55706 & Date < ymd("2024-08-31") ~ "557061",
    tag_id == 55706 & Date >= ymd("2025-04-02") ~ "557062",
    tag_id == 55680 & Date < ymd("2024-03-27") ~ "556801",
    tag_id == 55680 & Date >= ymd("2024-03-27") ~ "556802",
    tag_id == 55686 & Date < ymd("2024-03-10") ~ "556861",
    tag_id == 55686 & Date >= ymd("2024-03-10") ~ "556862",
    TRUE ~ as.character(tag_id)
  ))

ref <- ref |> select(-"tag_id")

# join the ref data to location data 
tfileo <- left_join(tfile, ref, by = c("tag_idn" = "tag_idn")) 

# convert to 3005 to enable accurate estimation of steps
tfileo <- st_as_sf(tfileo, coords = c("Longitude", "Latitude"), crs = 4326) |> 
  st_transform(crs = 3005) |> 
  st_coordinates() |> 
  as.data.frame() |> 
  bind_cols(tfileo) |> 
  rename(X = X, Y = Y)

#######################################################
# 1) remove records before or around deployment
# filter any rows that are before the tag deployment date (based on ref data) and or within 24 hours of deployment

tt <- tfileo |> 
  group_by(tag_idn) |> 
  filter(Date > min(capture_date)) 

## check the outputs 
aa <- tt |> 
  group_by(tag_idn) |> 
  summarise(capture_date = min(capture_date),
            min_date = min(Date)) |> 
  rowwise() |> 
  # calculate differnce between capture and min_date
  mutate( diff_days = as.numeric(difftime(min_date, capture_date, units = "days"))) #|> 
#  filter(diff_days > 1)


# check if any are after the end-date 
tt <- tt |> 
  group_by(tag_idn) |> 
  mutate(check = ifelse(!is.na(End_Date) & Date > End_Date, 1, 0))
  
aa <- tt |> filter(check == 1)
#unique(aa$tag_idn)
aa <- aa |> 
  group_by(tag_idn) |> 
  summarise(end_date = max(End_Date),
            max_date = max(Date)) 

aa <- aa |> 
  group_by(tag_idn) |> 
  rowwise() |> 
  # calculate differnce between end data and max_date
  mutate( diff_days = as.numeric(difftime(max_date, end_date, units ="days")))  

# drop the records post end-date
tt <- tt |> 
  filter(check !=1)


################################################################
# 2) remove any record with 2d locations #unique(tt$X2D.3D)

ttt <- tt |> 
  dplyr::filter(X2D.3D != 2) 
         

## 3) convert to Pacific Standard time (PST)
tfile_sub <- ttt |> 
  mutate(date_time = ymd_hm(paste(Date, Hour, Minute))) |> 
  mutate(date_time_pst = with_tz(date_time, tzone = "America/Vancouver")) |> 
  mutate(date_pst = as_date(date_time_pst)) |> 
  mutate(year_pst = year(date_pst))



############################################################################
## 3: Calculate the step length per animal using lat and long estimates 
# Calculate individual movement rates for all individuals. 
# group by tag_idn and add the lag lat and long and time

tfile_subLL <- tfile_sub |> 
  group_by(tag_idn) |> 
  arrange(date_time_pst) |>
  mutate(lat_prior = lag(Latitude, 1L),
         long_prior = lag(Longitude, 1L),
         time_prior = lag(date_time_pst)) |> 
  rowwise() |> 
  dplyr::mutate(dist_m = distGeo(c(long_prior,lat_prior), c(Longitude, Latitude)),
                bearing = bearing(c(long_prior,lat_prior), c(Longitude, Latitude)),
                time_diff_hours = as.numeric(difftime(date_time_pst, time_prior, units = "hours")),
                speed_ave = dist_m/time_diff_hours) |> 
  ungroup()

## remove any locations where speed is > 5.5km/h ? (5000m/h) # what would be a reasonable speed? 
#sort(unique(tfile_subLL$speed_ave), decreasing = TRUE)
#hist(tfile_subLL$speed_ave, breaks = 100)     

tfile_subLL <- tfile_subLL |> 
 filter(speed_ave < 5500)


# rerun the distance and speed calcs after removing errors and check for more outliers 

tfile_subLL <- tfile_subLL |> 
  group_by(tag_idn) |> 
  arrange(date_time_pst) |>
  mutate(lat_prior = lag(Latitude, 1L),
         long_prior = lag(Longitude, 1L),
         time_prior = lag(date_time_pst)) |> 
  rowwise() |> 
  dplyr::mutate(dist_m = distGeo(c(long_prior,lat_prior), c(Longitude, Latitude)),
                bearing = bearing(c(long_prior,lat_prior), c(Longitude, Latitude)),
                time_diff_hours = as.numeric(difftime(date_time_pst, time_prior, units = "hours")),
                speed_ave = dist_m/time_diff_hours) |> 
  ungroup()

# check outputs 
#sort(unique(tfile_subLL$speed_ave), decreasing = TRUE)



##################################################################################
#4) removed fixes if their incoming and outgoing speeds exceeded 2 km/h (0.99 percentile of the dataset), 
#and the cosine of the turning angle was less than −0.97.ie almost complete reversal in direction.

# calculate the turning angle from bearing and previous bearing 
# filter where the incoming speed is 2km/h and outgoing speed is 2km/h and the cosine of the turning angle is less than -0.97

tfile_subLL <- tfile_subLL |> 
  group_by(tag_idn) |> 
  arrange(date_time_pst) |>
  mutate(bearing_prior = lag(bearing, 1L)) |> 
  rowwise() |> 
  dplyr::mutate(turning_angle = ((bearing - bearing_prior + 180) %% 360) - 180) |> 
  ungroup()


traj_df <- tfile_subLL %>%
  arrange(tag_idn, date_time_pst) %>%
  group_by(tag_idn) %>%
  mutate(
    speed_in = lag(speed_ave),
    speed_out = speed_ave,
    cos_turn = cos(turning_angle),
    gps_spike =
      speed_in  > 2000 &
      speed_out > 2000 &
      cos_turn < -0.97
  ) %>%
  ungroup()

#ttsf <- st_as_sf(traj_df, coords = c("Longitude", "Latitude"), crs = 4326)
#write_sf(ttsf, fs::path("01_clean_data", "location_pointsTESST.gpkg"))

traj_df <- traj_df |> 
  filter(gps_spike == FALSE)


# rerun the distance and speed calcs after removing errors and check for more outliers 
tfile_subLL <- traj_df |> 
  group_by(tag_idn) |> 
  arrange(date_time_pst) |>
  mutate(lat_prior = lag(Latitude, 1L),
         long_prior = lag(Longitude, 1L),
         time_prior = lag(date_time_pst)) |> 
  rowwise() |> 
  dplyr::mutate(dist_m = distGeo(c(long_prior,lat_prior), c(Longitude, Latitude)),
                bearing = bearing(c(long_prior,lat_prior), c(Longitude, Latitude)),
                time_diff_hours = as.numeric(difftime(date_time_pst, time_prior, units = "hours")),
                speed_ave = dist_m/time_diff_hours) |> 
  ungroup()




#################################################################################
### 5: Calculate the step length per animal using adehabitat movements 
# this is a repeat as above to use more metrics from adehabitat package 

ts <- tfile_subLL |> 
  select(tag_idn, Y, X, date_time_pst, date_pst) 

sf_pts <- st_as_sf(
  ts,
  coords = c("X" , "Y"),
  crs = 3005,
  remove = FALSE
)


# get unique tags 
uts <- unique(sf_pts$tag_idn)#[20:22]

# cycle through all sheep and calculate the movement metrics using adehabitat package

adehab_metrics <- purrr::map(uts, function(x){
 
   #x <- uts[2]
  
   sf_ptsu <- sf_pts |> 
    filter(tag_idn == x) |> 
    arrange(date_time_pst) |> 
    ungroup() |> 
    unique()
   
  # 1: calculate step and movement metrics
   
  ltraj <- as.ltraj(
    xy = as.data.frame(sf_ptsu[,c("X", "Y")]),
    date = sf_ptsu$date_time_pst,
    id = sf_ptsu$tag_idn
  )
  
  # 2: add residence time # 1000m 
  #https://rdrr.io/cran/adehabitatLT/man/residenceTime.html
  
  rr <- residenceTime(ltraj, 100, 2,
                       units = c("hours"))
  
  
  # 3 generate minimum convex polygon for 1 day, 2 days , 3 days (median and mean)
  if(length(unique(sf_ptsu$date_pst))>3){
    
  # get date range for three day window 
  dates <- seq(
    min(sf_ptsu$date_pst),
    max(sf_ptsu$date_pst) - 2,
    by = "1 day"
  )
  
  dm <- dates[2:length(dates)-2]
  
  # for each date 
  mcp_area <- purrr::map(dm, function(d){
    
     #print(d)
   # d <- dates[2]
    
    # get date before and after
    start_date <- d - days(1)
    end_date <- d + days(1)
    
    # Define your date range as an interval
    sf_ptsu_window <- sf_ptsu %>%
      filter(date_pst >= start_date &
               date_pst <= end_date)
    
    # Require minimum points
    if(nrow(sf_ptsu_window) < 5) return(NULL)
    # Convert sf -> sp
    sp_ptsu_w <- as(sf_ptsu_window, "Spatial")
    
    # minimum convex polygon
    mcpu <- mcp(sp_ptsu_w[,1],percent=95, unin = c("m"))
  
    outline <- c(x, as.character(d), round(mcpu$area,2))
    #plot(mcpu)
    #plot(sp_ptsu_w, col=as.data.frame(sp_ptsu)[,1], add=TRUE)
    outline 
      
    }) 
  
  # drop NULLs 
  mcp_area <- mcp_area[!sapply(mcp_area, is.null)]
  #bind_rows(mcp_area)
  df <- do.call("rbind", mcp_area)
  df <- as.data.frame(df)
  if(nrow(df)>0){
  colnames(df) <- c("tag_idn", "date_pst", "mcp_area_3d")
  } else {
    df <- data.frame(tag_idn = character(), date_pst = character(), mcp_area_3d = character())
  }
 
  }else{
    df <- data.frame(tag_idn = character(), date_pst = character(), mcp_area_3d = character())
  }
  ########################################################
  # mcp for one day 
  
  # get date range 
  dates <- seq(
    min(sf_ptsu$date_pst),
    max(sf_ptsu$date_pst),
    by = "1 day"
  )
  
  dm <- dates 
  
  # for each date 
  mcp_area_1d <- purrr::map(dm, function(d){
    
    # print(d)
    #d <- dates[2]
    
    # Define your date range as an interval
    sf_ptsu_window <- sf_ptsu %>%
      filter(date_pst == d)
    
    # Require minimum points
    if(nrow(sf_ptsu_window) < 5) return(NULL)
    # Convert sf -> sp
    sp_ptsu_w <- as(sf_ptsu_window, "Spatial")
    
    # minimum convex polygon
    mcpu <- mcp(sp_ptsu_w[,1],percent=95, unin = c("m"))
    
    outline <- c(x, as.character(d), round(mcpu$area,2))
    
    #plot(mcpu)
    #plot(sp_ptsu_w, col=as.data.frame(sp_ptsu)[,1], add=TRUE)
    outline 
    
  }) 
  
  # drop NULLs 
  mcp_area_1d <- mcp_area_1d[!sapply(mcp_area_1d, is.null)]
  #bind_rows(mcp_area)
  df1d <- do.call("rbind", mcp_area_1d)
  df1d <- as.data.frame( df1d)
  colnames(df1d) <- c("tag_idn", "date_pst", "mcp_area_1d")
  
  avemcp <- left_join( df1d,df, by = c("tag_idn", "date_pst"))
  
  
  ## join everything together 
  tr <- ld(ltraj)
  tr$date_time_pst <- tr$date
  tr$date_pst = as_date(tr$date_time_pst)
  
  #tr$data_pst <- ymd(tr$date_time_pst)
  rdf <- as.data.frame(rr[[1]])
  rdf$date_time_pst <- rdf$Date
  
  outt <- left_join(tr,rdf )
  avemcp <- avemcp |> 
    mutate(date_pst = as_date(date_pst))
  
  outtt <- left_join(outt, avemcp,by = c("date_pst"))
  
  out <- outtt |> 
    select(-date, -id, -burst, -pkey, -Date)
  
  out
})


adehab_metrics_all <- adehab_metrics |> 
  bind_rows()

all = left_join( tfile_subLL, adehab_metrics_all, by = c("tag_idn","date_time_pst")) 




### 4) potential to drop more records based on HPOD values (although not great outcomes: )
#traj_df <- traj_df %>%
# mutate(date_time_pst = format(date_time_pst, "%Y-%m-%d %H:%M:%S"))


## filter down some columns 
out <- all |> 
  select(-CollarSerialNumber, -Hour, -Minute, -X2D.3D, #-tag_id.x, -tag_id.y, 
         -Date, -Time.Zone, -time,-date_pst.x,
         -Northing, -Westing, -Animal_WLH, -Eartag,-Comments, -date_time, 
         -Recorder, -Capture_GPS_Zone_NAD_83., -lat_prior, -long_prior  , -time_prior , 
         -cos_turn, -gps_spike, -check, -x, -y )


out_sf <- st_as_sf(out, coords = c("X", "Y"), crs = 3005)

# write this out 
write.csv(out, fs::path("01_clean_data", "location_steps_all_20260703.csv")) # filtered down cols
write.csv(all, fs::path("01_clean_data", "location_steps_all_raw_20260703.csv")) # all columns 

# write out subset cols as sf object 
st_write(out_sf, fs::path("01_clean_data", "location_steps_all_20260703.gpkg"), append = FALSE)








