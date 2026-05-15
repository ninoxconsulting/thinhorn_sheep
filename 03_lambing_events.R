## identify ewes and potential partuition dates and locations
## Lambing dates May 1 - June 15th (as per Enns et al + refernces)
# For ewes, estimate parturition timing and duration based on changes in 
# movement rate, proximity to herd, 
# and within expected lambing season (1 May to June 30th). 
library(dplyr)
library(sf)
library(fs)
library(readxl)
library(lubridate)
library(hms)
library(ggplot2)

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



#### Catergories ######

# get list of ewes
ewes <- pts |> 
  filter(sheep_class == "ewe") |> 
  mutate(tag_idn = as.numeric(tag_idn)) |> 
  select(-sex, -sheep_class) 


#### Breeding period - May 1st to 30 June any year #########################

Julianday <- function(x) {
  as.numeric(format(x, "%j"))
}

# calculate the julian date for May 1st and June 30th
jstart <- Julianday(ymd("2024-05-01"))
jend <- Julianday(ymd("2024-06-10"))
#jstart <- Julianday(ymd("2023-05-01"))
#end <- Julianday(ymd("2023-06-30"))


#120 - 183

be <- ewes |> 
  filter(Julianday >= jstart & Julianday <= jend) |> 
  group_by(tag_idn, date_pst) |>
  mutate(speed_ave_day = mean(speed_ave, na.rm = TRUE),
         speed_median_day = median(speed_ave, na.rm = TRUE)) |> 
  ungroup()


# no unique females and age class (n = 23)
beu <- unique(be$tag_idn)

# some summary of ewes
#ewe_sum <- be |> 
#  group_by(tag_idn) |>
#  summarise(count = n())

for(ii in beu){
  
  #ii <-beu[7]
  
  print(ii)
  
  ewi <- be |> 
    filter(tag_idn == ii) |> 
    arrange(date_time_pst)
  
  age_annuali_capture <- unique(ewi$Age_annuli)
  age_annuali_capture
  
  ewi_years = unique(ewi$year_pst)
  
  # #daily average
  # ewi_day <- ewi |> 
  #   dplyr::select(tag_idn, year_pst ,Julianday,date_pst, speed_ave_day,speed_median_day) |> 
  #   unique()
  # 
  # sheep_move <- ggplot(ewi_day, aes(x = Julianday, y = speed_ave_day, colour = year_pst)) +
  #   geom_line() +
  #   #geom_point()+
  #   facet_wrap(~year_pst)#+
  #   #ylim(0,1500)
  # 
  # sheep_move
  # 
  # sheep_move <- ggplot(ewi_day, aes(x = Julianday, y = speed_median_day, colour = year_pst)) +
  #   geom_line() +
  #   #geom_point()+
  #   facet_wrap(~year_pst)#+
  # #ylim(0,1500)
  # 
  # sheep_move
  # 
  
  purrr::map(ewi_years, function(x){
    print(x)
    x = 2024
    
    ewi_year <- ewi |> 
      filter(year_pst == x)
    
    sheep_move_year <- ggplot(ewi_year, aes(x = date_time_pst , y = speed_ave)) +
      geom_line(colour= "darkgrey") +
      geom_point(size = 0.2,colour= "darkgrey")+
      labs(title = paste0("Tag ID: ", ii, " Age annuali at capture: ", age_annuali_capture," Year: ", x),
           x = "Date", y = "movement rate (m/h)")+ 
      scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d")+
      ylim(0,1600)
    
    print(sheep_move_year)
    ggsave(filename = fs::path(out_dir, paste0("ewe_",ii, "_", x, ".png")), plot = sheep_move_year, width = 11, height = 6)
    
  })
 
  
}





# todo: 
# generate a kernal density estimate based on moving average of 10 days 
# estimate area and then add to plot as background line? 
# calculate a three day average movement event (See de mars, 2013)













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



# plot the frequency to check for breaks 
ggplot(dead_tags, aes(x = Date, y = Activity )) +
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




