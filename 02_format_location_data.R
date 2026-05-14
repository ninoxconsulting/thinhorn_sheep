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

# read in the summary data 

data_dir <- fs::path("00_raw_data")

# read in ref data
ref <- read.csv(fs::path("01_clean_data", "reference_edit.csv")) |> select(-X)
ref <- ref |> 
  mutate(tag_idn = gsub("-", "", tag_id))

# read in location data 
loc_dir <- fs::path(data_dir, "20260325")

#list.files(loc_dir)
file_i <- fs::path(loc_dir, "Cumulative_D_13197_202623181141.txt")

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
    tag_id == 55669 & Date < ymd("2025-04-03") ~ "556691",
    tag_id == 55669 & Date >= ymd("2025-04-03") ~ "556692",
    tag_id == 55688 & Date < ymd("2025-04-02") ~ "556881",
    tag_id == 55688 & Date >= ymd("2025-04-02") ~ "556882",
    tag_id == 55691 & Date < ymd("2025-04-03") ~ "556911",
    tag_id == 55691 & Date >= ymd("2025-04-03") ~ "556912",
    tag_id == 55706 & Date < ymd("2025-04-02") ~ "557061",
    tag_id == 55706 & Date >= ymd("2025-04-02") ~ "557062",
    tag_id == 55680 & Date < ymd("2024-03-27") ~ "556801",
    tag_id == 55680 & Date >= ymd("2024-03-27") ~ "556802",
    tag_id == 55686 & Date < ymd("2024-03-10") ~ "556861",
    tag_id == 55686 & Date >= ymd("2024-03-10") ~ "556862",
    TRUE ~ as.character(tag_id)
  ))



# join the ref data to location data 
tfileo <- left_join(tfile, ref, by = c("tag_idn" = "tag_idn")) 


# 1) remove records before or around deployment
# filter any rows that are before the tag deployment date (based on ref data) and or within 24 hours of deployment

tt <- tfileo |> 
  group_by(tag_idn) |> 
  filter(Date > min(capture_date)) 

# check the dates 
# tt |> group_by(tag_idn) |> 
#  summarise(capture_date = min(capture_date), min_date = min(Date), max_date = max(Date), duration = max_date - min_date)


# 2) remove any record with 2d locations 
#unique(tt$X2D.3D)
ttt <- tt |> 
  dplyr::filter(X2D.3D != 2) 
         
#ttt     
   
#ttsf <- st_as_sf(tt, coords = c("Longitude", "Latitude"), crs = 4326)
#write_sf(ttsf, fs::path("01_clean_data", "location_pointsTESST.gpkg"))






## Step 1: Calculate the step length per animal 
# Calculate individual movement rates for all individuals. 
# For ewes, estimate parturition timing and duration based on changes in 
# movement rate, proximity to herd, 
# and within expected lambing season (1 May to June 30th). 

# convert to Pacific Standard time (PST)
#Q for Bill - do you want in PST or Summer time
tfile_sub <- ttt |> 
 # select(Latitude, Longitude, tag_idn, Date, Hour, Minute) |>
  mutate(date_time = ymd_hm(paste(Date, Hour, Minute))) |> 
  mutate(date_time_pst = with_tz(date_time, tzone = "America/Vancouver")) 
  
# group by tag_idn and add the lag lat and long and time

tfile_sub <- tfile_sub |> 
  group_by(tag_idn) |> 
  arrange(date_time_pst) |>
  mutate(lat_prior = lag(Latitude, 1L),
         long_prior = lag(Longitude, 1L),
         time_prior = lag(date_time_pst)) |> 
  rowwise() |> 
  dplyr::mutate(#gcd_m = distHaversine(c(long_prior,lat_prior), c(Longitude, Latitude)),
                dist_m = distGeo(c(long_prior,lat_prior), c(Longitude, Latitude)),
                bearing = bearing(c(long_prior,lat_prior), c(Longitude, Latitude)),
                time_diff_hours = as.numeric(difftime(date_time_pst, time_prior, units = "hours")),
                speed_ave = dist_m/time_diff_hours) |> 
  ungroup()


# remove any locations where speed is > 6km/h ? (5000m/h) # what would be a reasonable speed? 
sort(unique(tfile_sub$speed_ave), decreasing = TRUE)
     
# could refine this with window analysis as per Bjørneraas 2010 

tfile_sub <- tfile_sub |> 
  filter(speed_ave < 5000)


#3) Third, we removed fixes if their incoming and outgoing speeds exceeded 2 km/h (0.99 percentile of the dataset), 
#and the cosine of the turning angle was less than −0.97.ie almost complete reversal in direction.

# calculate the turning angle from bearing and previous bearing 
# filter where the incoming speed is 2km/h and outgoing speed is 2km/h and the cosine of the turning angle is less than -0.97

tfile_sub <- tfile_sub |> 
  group_by(tag_idn) |> 
  arrange(date_time_pst) |>
  mutate(bearing_prior = lag(bearing, 1L)) |> 
  rowwise() |> 
  dplyr::mutate(turning_angle = ((bearing - bearing_prior + 180) %% 360) - 180) |> 
  ungroup()


traj_df <- tfile_sub %>%
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

traj_df <- traj_df |> 
  filter(gps_spike == FALSE)


## 4) potential to drop more records based on HPOD values (although not great outcomes: )


traj_df

traj_df <- traj_df %>%
  mutate(date_time_pst = format(date_time_pst, "%Y-%m-%d %H:%M:%S"))

## maybe recalculate the speed etc? 




# write this out and manually add the End data shown below 
write.csv(traj_df, fs::path("01_clean_data", "location_steps_test.csv"))


# 
# # check durations
# tfile_dur <- ttt |> 
#   group_by(tag_idn, sex, sheep_class, Age_annuli ) |> 
#   summarise(min_date = min(Date), max_date = max(Date), duration = max_date - min_date)
#   
# # by sheep class
# dur_plot_class <- ggplot(tfile_dur, aes(y=factor(tag_idn), color = sex)) +  
#   geom_segment(aes(x=min_date, xend=max_date, y=factor(tag_idn), yend=factor(tag_idn)), size=1)+  
#   xlab("Date") + ylab("Tag") +
#   scale_color_viridis_d(begin = 0.2, end = 0.8)+
#   facet_wrap(~ sheep_class, scales = "free_y") #+
# 
# 
# dur_plot  
# 
# # by age annuli
# dur_plot_age <- ggplot(tfile_dur, aes(y=factor(tag_idn), color = sex)) +  
#   geom_segment(aes(x=min_date, xend=max_date, y=factor(tag_idn), yend=factor(tag_idn)), size=1)+  
#   xlab("Date") + ylab("Tag") +
#   scale_color_viridis_d(begin = 0.2, end = 0.8)+
#   facet_wrap(~ Age_annuli, scales = "free_y") #+
# 
# dur_plot_age  


# dur_hist <- ggplot(dur, aes(x= duration))+  
#   geom_histogram() + #fill="white", position="dodge") +  
#   scale_color_viridis_d()+
#   #facet_wrap(~year)+  
#   xlab("duration (days)")   
# 
# dur_hist  


## generate a summary of how many tags - duration and type. 









