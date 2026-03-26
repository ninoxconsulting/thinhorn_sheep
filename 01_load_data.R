## 01_load data script 

# in preparation of this script users should firstly download data from ATS site 
# https://www.atsidaq.net/home/home.aspx. Note these collars are still transmitting
# at the time of data analysis : March 2026

library(dplyr)
library(sf)
library(fs)
library(readxl)
library(lubridate)


# read in the summary data 

data_dir <- fs::path("00_raw_data")


#list.files(data_dir)
library(hms)

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






# read in the dead tags #####################################
# check if this matches the counts for all transmission download 

tag_dir <-  fs::path(data_dir, "Dead Sheep Data")          
dtags <- list.files(tag_dir, pattern = "txt", full.names = TRUE)

# read in each file and join togehter 
dead_tags <- purrr::map_dfr(dtags, function(x){
  #x <- dtags[1]
  tfile <- read.table(fs::path(x), header = T, sep = ",") |> 
    mutate(tag_id = as.character(CollarSerialNumber )) |>
    mutate(Date = ymd(Date)) 

}) |> bind_rows()
  
  
# summary 
dtag_sum <- dead_tags |> 
  group_by(tag_id) |> 
  count()

# confirmed that this matches the tags in the total transmission data so will use this source 

# get last date for each of these tags to update ref 




# read in the location dates 
# update this to latest folder 
latest_dir <-  fs::path(data_dir, "20260325")          


list.files(latest_dir)       

loc <- read.table(fs::path(latest_dir, "Cumulative_D_13197_202623181141.txt"), header = T, sep = ",")



# summary 
tag_sum <- loc |> 
  group_by(CollarSerialNumber) |> 
  count() |> 
  mutate(tag_id = as.character(CollarSerialNumber)) 

tag_date <- loc |> 
  group_by(CollarSerialNumber) |> 
  slice_max(Date) |> 
  select(CollarSerialNumber, Date ) |> 
  unique()



check <- left_join(tag_sum, dtag_sum)



  