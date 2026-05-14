## 01_load data script 

# in preparation of this script users should firstly download data from ATS site 
# https://www.atsidaq.net/home/home.aspx. Note these collars are still transmitting
# at the time of data analysis : March 2026

library(dplyr)
library(sf)
library(fs)
library(readxl)
library(lubridate)
library(hms)
library(ggplot2)

# read in the summary data 

data_dir <- fs::path("00_raw_data")

#list.files(data_dir)

ref <- read_xlsx(fs::path(data_dir, "Spatsizi Sheep Collar Data-for range project.xlsx"),
                 skip = 1, 
                 .name_repair = "universal",
                 col_types = c("date", "date", "text", "numeric", "numeric", "numeric", 
                               "guess", "guess", "guess","guess", "guess","guess","date"),
                 trim_ws = TRUE) 
names(ref) = c("Capture_Date", "Time", "Recorder" ,"Capture_GPS_Zone_NAD_83." , "Northing", "Westing",
              "Animal_WLH", "Eartag", "tag_id" , "Age_annuli", "sheep_class", "sex", "End_Date")                                                       

ref <- ref |> 
  mutate(time = as_hms(ymd_hms(Time))) |> 
  mutate(capture_date = ymd(Capture_Date)) |> 
  select(-Time, -Capture_Date) |> 
  select(tag_id, capture_date, time, "Recorder" ,"Capture_GPS_Zone_NAD_83." , "Northing", "Westing",
         "Animal_WLH", "Eartag", "Age_annuli", "sheep_class", "sex", "End_Date")


# write this out and manually add the End data shown below 
write.csv(ref, fs::path("01_clean_data", "reference.csv") )




############################################
## Summary of tags 

## manually edited this file

ref <- read.csv(fs::path("01_clean_data", "reference_edit.csv"))
ref <- ref |> 
  mutate(tag_idn = gsub("-", "", tag_id))
  
  
## summary of tags 

# by sex 
sex_sum <- ref |> 
  group_by(sex) |> 
  count()

sex_sum


# by age class and sex
sex_age_sum <- ref |> 
  group_by(sex, sheep_class) |> 
  count()

sex_age_sum


# by age annualli and sex
sex_aage_sum <- ref |> 
  group_by(sex, Age_annuli) |> 
  count()

sex_aage_sum



# duration of the tags 
dur <- ref |> 
  mutate(end_date_estimate = case_when(
    is.na(End_Date) ~ "2026-04-07",
    .default = End_Date
  )) |> 
  mutate(end_date_estimate = ymd(end_date_estimate)) |> 
  mutate(capture_date = ymd(capture_date)) |> 
  mutate(duration = end_date_estimate - capture_date)



dur_plot <- ggplot(dur, aes(y=factor(tag_idn), color = sex)) +  
  geom_segment(aes(x=capture_date, xend=end_date_estimate, y=factor(tag_idn), yend=factor(tag_idn)), size=1)+  
  xlab("Date") + ylab("Tag") +
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  facet_wrap(~ sheep_class) #+
# xlim("2022-01-01", "2026-07-04")


dur_plot  

dur_hist <- ggplot(dur, aes(x= duration))+  
  geom_histogram() + #fill="white", position="dodge") +  
  scale_color_viridis_d()+
  #facet_wrap(~year)+  
  xlab("duration (days)")   

dur_hist  



## Lambing dates 3rd May - June 14 (enns et al. 2024)
# 
# out <- out %>%
#   group_by(tag.id) |> 
#   mutate(diff = difftime(date_time, lag(date_time),  units = c("hours")), 
#          diff = as.numeric(diff)) 
