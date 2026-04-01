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
last_date <- dead_tags |> 
  select(Date, tag_id) |> 
  group_by(tag_id) |> 
  summarise(max(Date))


# plot the frequency to check for breaks 
ggplot(dead_tags, aes(x = Date, y = Temperature)) +
  geom_line() +
  facet_wrap(~tag_id)+
  #scale_x_date(date_labels = "%b %Y")
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y")




#### 55680 #######
s1 <- dead_tags |> 
  dplyr::filter(tag_id == 55680)

# plot the frequency to check for breaks 
ggplot(s1, aes(x = Date, y = Temperature)) +
  geom_line() +
  #scale_x_date(date_labels = "%b %Y")
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y")


## checked this record to find the break
s1_dates <- s1 |> 
  select(Date, tag_id) |> 
  group_by(Date,tag_id) |> 
  count()

# 
ggplot(s1_dates, aes(x = Date, y = n)) +
  geom_line() +
  #scale_x_date(date_labels = "%b %Y")
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y") 

## questions 
# for 55680-1 and 55680-2 both tagged within short time frame, 
# tag freq slow down around March 27 and March 28 (potential there is UTC time?)
# then tag swapped? and then redeployed? 
# Q: was 55680-1 a dud? 


#### 55686 #######
s2 <- dead_tags |> 
  dplyr::filter(tag_id == 55686)

# plot the frequency to check for breaks 
ggplot(s2, aes(x = Date, y = Temperature)) +
  geom_line() +
  #scale_x_date(date_labels = "%b %Y")
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y")


## checked this record to find the break
s2_dates <- s2 |> 
  select(Date, tag_id) |> 
  group_by(Date,tag_id) |> 
  count()

# 
ggplot(s2_dates, aes(x = Date, y = n)) +
  geom_line() +
  #scale_x_date(date_labels = "%b %Y")
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y") 


## seems something funcky with this tag as the frequency of 
# counts seemed to drop at the end before tag died. 







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



  