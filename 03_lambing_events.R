## identify ewes and potential partition dates and locations
## Lambing dates May 1 - June 15th (as per Enns et al + refernces)
# For ewes, estimate parturition timing and duration based on changes in 
# movement rate, proximity to herd, 
# and within expected lambing season (1 May to June 30th). 

#install.packages("nseq")
#install.packages("GGally")
#install.packages("corrplot")

library(corrplot)
library(GGally)
library(nseq)
library(dplyr)
library(sf)
library(fs)
library(readxl)
library(lubridate)
library(hms)
library(ggplot2)


# read in the summary data 

clean_dir <- fs::path("01_clean_data")
out_dir <- fs::path("02_draft_outputs/01_lamb_figures_20260706")

#allpts <- read.csv(fs::path("01_clean_data", "location_steps_all_raw_20260703.csv"))  # issues with date field and csv
allpts <- st_read(fs::path("01_clean_data", "location_steps_all_20260703.gpkg")) |> 
  st_drop_geometry()

# remove unwated cols 
pts <- allpts |> 
  select( -Hdop,-NumSats, -FixTime, -Year,
          -End_Date) |> 
  mutate(date_time_pst1 = format(as.POSIXct(date_time_pst), "%Y-%m-%d %H:%M:%S")) |> 
  mutate(date_time_pst = ymd_hms(date_time_pst1)) |> 
  mutate(date_pst = as_date(date_time_pst)) |> 
  select(-date_time_pst1)


#### Sub set to Ewes 
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
jend <- Julianday(ymd("2024-06-30"))
#jstart <- Julianday(ymd("2025-05-01"))
#end <- Julianday(ymd("2025-06-30"))


#120 - 183

be <- ewes |> 
  filter(Julianday >= jstart & Julianday <= jend) |> 
  group_by(tag_idn, date_pst) |>
  mutate(speed_ave_day = mean(speed_ave, na.rm = TRUE),
         speed_median_day = median(speed_ave, na.rm = TRUE)) |> 
  ungroup()


# no unique females and age class (n = 23) - for usable females 21 had data over the lambing time window, 
beu <- unique(be$tag_idn)


# for each ewe and each year plot the step length and mcp over three years. 
for(ii in beu){
  
# ii <-beu[21]
  
  print(ii)
  
  ewi <- be |> 
    filter(tag_idn == ii) |> 
    arrange(date_time_pst)
  
  # write out gps data to review 
  out_sf <- st_as_sf(ewi, coords = c("Longitude", "Latitude"), crs = 4326)
  out_sf <- st_transform(out_sf, crs = 3005)
  
  # write out subset cols as sf object 
  st_write(out_sf, fs::path(out_dir, paste0("location_lamb_", ii, ".gpkg")), append = FALSE)
  
#}
  
  # GET INFO FOR HEADING
  age_annuali_capture <- unique(ewi$Age_annuli)
  ewi_years = unique(ewi$year_pst)
  date_collared = unique(ewi$capture_date)
  preg_2024 <- unique(ewi$pregnant_2024)
  preg_2025 <- unique(ewi$pregnant_2025)
  capture_preg_status = ifelse(preg_2024 %in% c("", "na"), preg_2025, preg_2024)

  # LOOP THROUGH EACH YEAR FOR EACH EWE
  purrr::map(ewi_years, function(x){
    
    print(x)
    #x = 2024
    #x <- ewi_years[1]
    
    ewi_year <- ewi |> 
      filter(year_pst == x) |> 
      select(tag_idn, date_time_pst, speed_ave, mcp_area_3d) 
   
    median_step_length = median(ewi_year$speed_ave, na.rm = TRUE)
    mean_step_length = mean(ewi_year$speed_ave, na.rm = TRUE)
    
    
    # all column to highlight where speed is < 50% of median for >36 hours
    ewi_year <- ewi_year |> 
      mutate(low_speed_median = ifelse(speed_ave < (0.5*median_step_length), 1, 0)) |> 
      mutate(low_speed_mean = ifelse(speed_ave < (0.5*mean_step_length), 1, 0))
    
  
    # How many sequences have at least 36 consecutive observations with value equal or greater than mean or median step length
      median_length = trle_cond(x = c(ewi_year$low_speed_median), a_op = "gte", a = 36, b_op = "gte", b = 1)
      mean_length = trle_cond(x = c(ewi_year$low_speed_mean), a_op = "gte", a = 36, b_op = "gte", b = 1)
       
    # conversion for scales from movement rate to mcp area
    mcp_area_3d = 0.01
  
    sheep_move_year <- ggplot(ewi_year, aes(x = date_time_pst)) +
      geom_hline(yintercept=median_step_length, linetype="dashed", color = "red")+
      geom_line(aes(y =speed_ave), colour= "darkgrey") +
      ##geom_point(aes(y =speed_ave), size = 0.2,colour= "darkgrey")+
      geom_line(aes(y = as.numeric(mcp_area_3d)), size = 0.2,colour= "blue", linetype = "longdash")+
      scale_y_continuous("movement rate (m/h)", sec.axis = sec_axis(~.*mcp_area_3d, name = "mcp three day ave")) +
      
      labs(title = paste0("Tag ID:", ii,  ",  Capture: ", date_collared,",  Capture Age annuali: ", age_annuali_capture, ", Preg on capture: " ,capture_preg_status, ", Plot Year: ", x),
           x = "Date", y = "movement rate (m/h)")+ 
      scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d")+
      ## add top left annotation for number of consecutive days below 50% of median and mean step length
      ##geom_text("text", x = median_length )
      #annotate(geom = 'text', label = paste0("50% below median > 36hrs =", median_length), x = min(ewi_year$date_time_pst), y = 1600, hjust = 0, vjust = 1)+
      annotate(geom = 'text', label = paste0("50% below mean > 36hrs =", mean_length), x = min(ewi_year$date_time_pst), y = 1600, hjust = 0, vjust = 1)+ 
      #ylim(0,1800)
      coord_cartesian(ylim = c(0,1600))
    
    
    sheep_move_year
    
    print(sheep_move_year)
    ggsave(filename = fs::path(out_dir, paste0("ewe_",ii, "_", x, "v2.png")), plot = sheep_move_year, width = 11, height = 6)
    
  })
 
  
}

### TO DO: EWES
# review the plots to determine potential lambing events. 
# double check these on the map 
# double check with mcp areas 

# compare multiple tags to see which tags are a family groups
#	Plot multiple years on same axis 

## FOR ALL: 
# generate KDE for entire range per year and also for season (lambing or rutting)









#######################################################################
# ## Look at summary of the metrics to use - currrently still testing
###################################################################### 
#
# head(be)
# # activity
# #RT.100 = resistence time
# #mcp_area_3d = 3 day moving average of mcp area
# #mcp_area_1d = 1 day moving average of mcp area
# 
# beudf <- be |> 
#   select(X, Y, tag_idn, date_time_pst,year_pst, speed_ave, mcp_area_3d, 
#          RT.100, mcp_area_1d, Activity, date_pst, speed_ave_day,speed_median_day )
# 
# # for each ewe and each year plot the steplength and mcp over three years. 
# for(ii in beu){
#   
#   ii <-beu[7]
#   
#   print(ii)
#   
#   ewi <- beudf |> 
#     filter(tag_idn == ii) |> 
#     arrange(date_time_pst) |> 
#     filter(year_pst == 2024)
#   
#   age_annuali_capture <- unique(ewi$Age_annuli)
#   age_annuali_capture
#   
#   # correlation plots 
#   
#   ls_vars <- c("mcp_area_3d", "mcp_area_1d", "speed_ave", "RT.100", "Activity")  
#   corr_matrix <- stats::cor(ewi[,ls_vars], use = "pairwise.complete.obs")
#   corrplot::corrplot.mixed(cor(ewi[,ls_vars], use = "pairwise.complete.obs"), lower.col = "black")
#   pairs(ewi[,ls_vars], main = "Basic Correlation Matrix")
#   ggpairs(ewi[,ls_vars], title = "Pairwise Scatter Plots with Correlation Coefficients")
#   
#   
#   # compare some specific 
#   # conversion for scales from movement rate to mcp area
#   RT100_scale = 0.001 
#   range(ewi$RT.100, na.rm = TRUE)
#   range(ewi$speed_ave, na.rm = TRUE)
#   
#   sheep_move_year <- ggplot(ewi, aes(x = date_time_pst)) +
#     geom_line(aes(y =speed_ave), colour= "darkgrey") +
#     geom_point(aes(y =speed_ave), size = 0.2,colour= "darkgrey")+
#     geom_line(aes(y =RT.100*0.01), size = 0.2,colour= "blue", linetype = "longdash")+
#     #scale_y_continuous("movement rate (m/h)", sec.axis = sec_axis(~.*RT100_scale, name = "RT100")) +
#     labs(title = paste0("Tag ID: ", ii, " Age annuali at capture: ", age_annuali_capture," Year: ", x),
#          x = "Date", y = "movement rate (m/h)")+ 
#     scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") #+
#    # annotate(geom = 'text', label = paste0("50% below median > 36hrs =", median_length), x = min(ewi_year$date_time_pst), y = max(ewi_year$speed_ave), hjust = 0, vjust = 1)+
#    # annotate(geom = 'text', label = paste0("50% below mean > 36hrs =", mean_length), x = min(ewi_year$date_time_pst), y = Inf-1, hjust = 0, vjust = 1)
#   #ylim(0,1600)
#   
#   
#   sheep_move_year
#   144499 *0.001 
#   
# }
# 





###############################################################################################

### Estimate the kde for breeding period (May 1st to June 30th) for each ewe and plot against movement rate

kdes <- purrr::map(beu, function(ii) {
  #ii <- beu[7]

  print(ii)

  ewi <- be |>
    filter(tag_idn == ii) |>
    arrange(date_time_pst)

  age_annuali_capture <- unique(ewi$Age_annuli)
  age_annuali_capture

  ewi_years <- unique(ewi$year_pst)


  kde_yr <- purrr::map(ewi_years, function(x) {
    print(x)
    # x = 2024
    # x <- ewi_years[1]

    ewi_year <- ewi |>
      filter(year_pst == x)

    if (nrow(ewi_year) >= 5) {
      #  kde: h reference parameter
      dbisf <- st_as_sf(ewi_year, coords = c("X", "Y"), crs = 3005)

      dbisp <- dbisf |>
        select(tag_idn) |>
        as("Spatial")

      # # define the parameters (h, kern, grid, extent)
      kde_href <- kernelUD(dbisp, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)

      # add a try statement to skip to next line if error is produced in vers95

      ver95_sf <- tryCatch(
        {
          ver95 <- getverticeshr(kde_href, 95) # get vertices for home range
          st_as_sf(ver95) |>
            mutate(th = 95) # convert to sf object
        },
        error = function(e) {
          return(NULL) # return NULL if error occurs
        }
      )

      ver75_sf <- tryCatch(
        {
          ver75 <- getverticeshr(kde_href, 75)
          st_as_sf(ver75) |>
            mutate(th = 75)
        },
        error = function(e) {
          return(NULL) # return NULL if error occurs
        }
      )

      ver50_sf <- tryCatch(
        {
          ver50 <- getverticeshr(kde_href, 50)
          st_as_sf(ver50) |>
            mutate(th = 50)
        },
        error = function(e) {
          return(NULL) # return NULL if error occurs
        }
      )

      # if it is not null the bind
      if (!is.null(ver95_sf) & !is.null(ver75_sf) & !is.null(ver50_sf)) {
        allvers <- bind_rows(ver95_sf, ver75_sf, ver50_sf)
        allvers$year <- x
        allvers$tag_idn <- ii

        return(allvers)
      }
    } else {
      return(NULL)
    }

  }) |> bind_rows()

  kde_yr
}) |> bind_rows()



st_write(kdes, path("02_draft_outputs", "ewes_lambing_yr_polygons.gpkg"))







####################################################################
##### plot the kde for one ewe and year to check

# Kernal density estimate (lambing period)
kde <- st_read(path("02_draft_outputs", "ewes_lambing_yr_polygons.gpkg"))

# select 50% threshold kde per lambing season for all years
kde50 <- kde |> 
  filter(th == 50) 
st_write(kde50, path("02_draft_outputs", "ewes_lambing_yr_50th_poly.gpkg"))


# select 50% threshold kde for each month for all years
all_poly <- st_read(path("02_draft_outputs", "sheep_month_yr_polygons.gpkg")) |> 
  filter(th == 50) |> 
  filter(id %in% unique(kde$tag_idn))
st_write(all_poly, path("02_draft_outputs", "ewes_kde_month_50th_poly.gpkg"), append = FALSE)


# points 
be_sub <- be |> 
  select(tag_idn, date_time_pst, X, Y, Latitude,Longitude, tag_idn,mcp_area_3d  ) |>
  st_as_sf(coords = c("X", "Y"), crs = 3005)
st_write(be_sub, path("02_draft_outputs", "ewes_lambing_yr_pts.gpkg"))


head(kde)

filter(kde, tag_idn == 55670, year == 2024) |> 
  ggplot() +
  geom_sf(aes(fill = as.factor(th))) +
  scale_fill_manual(values = c("red", "orange", "yellow")) +
  labs(title = "KDE Polygons for Tag ID: 55680 in 2024",
       fill = "KDE Threshold") +
  theme_minimal()


# filter the smallest region (50% threshold)

kde50 <- kde |> 
  filter(th == 50) |> 
  st_transform(crs = 3005) 


# get elevation  data - virtual raster
aoi = st_read(fs::path("00_raw_data", "aoi.gpkg"))
cded_raw <- bcmaps::cded(aoi)
cded <- terra::rast(cded_raw) 
cded_3005 <- terra::project(cded, "EPSG:3005")



kde50_plot1 <- ggplot() +
  geom_sf(data = kde50, aes(fill = as.factor(year)), alpha = 0.8) +
  scale_fill_manual(values = c("red", "orange", "yellow")) +
  #geom_polygon(data = kde50, aes(x = long, y = lat, group = group), fill = "blue", alpha = 0.2) +
  #ggnewscale::new_scale_fill() +
  #tidyterra::geom_spatraster(data = cded_3005, alpha = 0.5, show.legend = FALSE) +
  #tidyterra::scale_fill_terrain_c(direction = -1) +
  labs(title = "KDE Polygons",
       fill = "Year") +
  facet_wrap(~tag_idn ) +
 # geom_point(data = be, aes(x = X, y = Y), size = 0.2, colour = "blue") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())



kde50_plot1

# for each ewe and year plot the movement rate and the kde polygons to check for overlap and potential parturition events.















# todo: 
# generate a kernal density estimate based on moving average of 10 days 
# estimate area and then add to plot as background line? 
# calculate a three day average movement event (See de mars, 2013)





# 
# # read in the dead tags #####################################
# # check if this matches the counts for all transmission download 
# 
# tag_dir <-  fs::path(data_dir, "Dead Sheep Data")          
# dtags <- list.files(tag_dir, pattern = "txt", full.names = TRUE)
# 
# # read in each file and join togehter 
# dead_tags <- purrr::map_dfr(dtags, function(x){
#   #x <- dtags[1]
#   tfile <- read.table(fs::path(x), header = T, sep = ",") |> 
#     mutate(tag_id = as.character(CollarSerialNumber )) |>
#     mutate(Date = ymd(Date)) 
#   
# }) |> bind_rows()
# 
# # summary 
# dtag_sum <- dead_tags |> 
#   group_by(tag_id) |> 
#   count()
# 
# # confirmed that this matches the tags in the total transmission data so will use this source 
# 
# # get last date for each of these tags to update ref 
# last_date <- dead_tags |> 
#   select(Date, tag_id) |> 
#   group_by(tag_id) |> 
#   summarise(max(Date))
# 
# 
# # plot the frequency to check for breaks 
# ggplot(dead_tags, aes(x = Date, y = Temperature)) +
#   geom_line() +
#   facet_wrap(~tag_id)+
#   #scale_x_date(date_labels = "%b %Y")
#   scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y")
# 
# 
# 
# # plot the frequency to check for breaks 
# ggplot(dead_tags, aes(x = Date, y = Activity )) +
#   geom_line() +
#   facet_wrap(~tag_id)+
#   #scale_x_date(date_labels = "%b %Y")
#   scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y")
# 
# 
# #### 55680 #######
# s1 <- dead_tags |> 
#   dplyr::filter(tag_id == 55680)
# 
# # plot the frequency to check for breaks 
# ggplot(s1, aes(x = Date, y = Temperature)) +
#   geom_line() +
#   #scale_x_date(date_labels = "%b %Y")
#   scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y")
# 
# 
# ## checked this record to find the break
# s1_dates <- s1 |> 
#   select(Date, tag_id) |> 
#   group_by(Date,tag_id) |> 
#   count()
# 
# # 
# ggplot(s1_dates, aes(x = Date, y = n)) +
#   geom_line() +
#   #scale_x_date(date_labels = "%b %Y")
#   scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y") 
# 
# ## questions 
# # for 55680-1 and 55680-2 both tagged within short time frame, 
# # tag freq slow down around March 27 and March 28 (potential there is UTC time?)
# # then tag swapped? and then redeployed? 
# # Q: was 55680-1 a dud? 
# 
# 
# #### 55686 #######
# s2 <- dead_tags |> 
#   dplyr::filter(tag_id == 55686)
# 
# # plot the frequency to check for breaks 
# ggplot(s2, aes(x = Date, y = Temperature)) +
#   geom_line() +
#   #scale_x_date(date_labels = "%b %Y")
#   scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y")
# 
# 
# ## checked this record to find the break
# s2_dates <- s2 |> 
#   select(Date, tag_id) |> 
#   group_by(Date,tag_id) |> 
#   count()
# 
# # 
# ggplot(s2_dates, aes(x = Date, y = n)) +
#   geom_line() +
#   #scale_x_date(date_labels = "%b %Y")
#   scale_x_date(date_breaks = "3 month", date_labels = "%b %d %Y") 
# 
# 
# ## seems something funcky with this tag as the frequency of 
# # counts seemed to drop at the end before tag died. 
# 
# 


