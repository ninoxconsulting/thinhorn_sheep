## identify ewes and potential partition dates and locations
## Lambing dates May 1 - June 15th (as per Enns et al + refernces)
# For ewes, estimate parturition timing and duration based on changes in 
# movement rate step length , proximity to herd, 
# and within expected lambing season (1 May to June 30th). 

# calaulate step lengths
# identify and plot potential lambing based on step length figure 
# output a basic figure for shiny app. 


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
#out_dir <- fs::path("02_draft_outputs/01_lamb_figures_20260706")
#out_dir <- fs::path("02_draft_outputs/01_lamb_figures_20260721")
out_dir <- fs::path("02_draft_outputs/01_lamb_figures_20260731")

# use .gpkg as csv drops time stamp
allpts <- st_read(fs::path("01_clean_data", "location_steps_all_20260731.gpkg")) |> 
  st_drop_geometry()

# 
# # # # export data for App
# exportcsv <- allpts |> select(tag_idn, date_pst.y, date_time_pst, Latitude, Longitude, Age_annuli, sheep_class, sex ) |>
#   mutate(date_time_pst1 = format(as.POSIXct(date_time_pst), "%Y-%m-%d %H:%M:%S")) |>
#   mutate(date_time_pst = ymd_hms(date_time_pst1)) |>
#   rename("id"= tag_idn,
#          "lat" = Latitude,
#          "lon" = Longitude,
#          "datetime"=  date_time_pst,
#          "date" = date_pst.y) |>
#   select(id, lat, lon, datetime, date_time_pst1, date, Age_annuli, sheep_class, sex) #|>
#   #filter(!is.na(date))
# 
# st_write(exportcsv, path("01_clean_data", "sheep_data.csv"))

# remove unwanted cols 
pts <- allpts |> 
  select( -Hdop,-NumSats, -FixTime, -Year,
          -End_Date) |> 
  mutate(date_time_pst1 = format(as.POSIXct(date_time_pst), "%Y-%m-%d %H:%M:%S")) |> 
  mutate(date_time_pst = ymd_hms(date_time_pst1)) |> 
  mutate(date_pst = as_date(date_time_pst)) |> 
  select(-date_time_pst1)

# FIX: capture the timezone once, from data that actually exists at this
# point in the script -- used later when building highlight_ranges instead
# of the undefined ewi_jyear.
tz_use <- tz(pts$date_time_pst)

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


# calculate the julian date for May 1st and July 01st as template for all years 
jstart <- Julianday(ymd("2024-05-01"))
jend <- Julianday(ymd("2024-07-01"))
#jstart <- Julianday(ymd("2025-05-01"))
#end <- Julianday(ymd("2025-06-30"))

#120 - 212

be <- ewes |> 
  mutate(Julianday = Julianday(date_time_pst)) |> 
  filter(Julianday >= jstart & Julianday <= jend) |> 
  group_by(tag_idn, date_pst) |>
  mutate(speed_ave_day = mean(speed_ave, na.rm = TRUE),
         speed_median_day = median(speed_ave, na.rm = TRUE)) |> 
  ungroup()


# no unique females and age class (n = 23) - for usable females 21 had data over the lambing time window, 
beu <- unique(be$tag_idn)

# how many females and how many female year combinations 
ewe_success <- be |> 
  group_by(tag_idn, year_pst) |> 
  count() |> 
  ungroup() |> 
  group_by(tag_idn) |> 
  count() |> 
  ungroup()

sum(ewe_success$n)



# add the potential lambing events based on prior review 

# ------------------------------------------------------------
# 1. Lookup table of grey-box date ranges: one row per ewi x year
#    Replace this with your actual date ranges (e.g. read from CSV)
# ------------------------------------------------------------
ref_year <- 2000

# FIX: column renamed year_pst -> year, so it matches the faceting variable
# used by facet_grid(year ~ .) later on -- otherwise ggplot doesn't know
# which facet panel each box belongs to and draws it on every panel.
highlight_ranges <- tibble::tribble(
  ~tag_idn,    ~year, ~box_start,     ~box_end,    ~confidence,
  "55670",  2025,  "2025-05-27",   "2025-05-30",  "likely",
  "55670",  2026,  "2026-05-12",   "2026-05-16",  "likely",
  
  "55672",  2024,  "2024-05-21",   "2024-05-25",  "likely",
  "55672",  2025,  "2025-05-20",   "2025-05-23",  "likely",
  "55672",  2026,  "2026-05-19",   "2026-05-22",  "possible", 
  
  "55673",  2024,  "2024-05-17",   "2024-05-25",   "likely",
  "55673",  2025,  "2025-05-17",   "2025-05-25",   "likely",
  "55673",  2026,  "2026-05-21",   "2026-05-24",  "likely",

  "55674",  2025,  "2025-06-06",   "2025-06-09",  "possible",
  "55674",  2026,  "2026-06-09",   "2026-06-12",  "likely",

  "55676",  2024,  "2024-06-01",   "2025-06-05", "possible",
  "55676",  2025,  "2025-06-12",   "2025-06-16", "possible",
 # "55676",  2026,  "2026-06-03",   "2026-06-05", # might have died earlier than 12th
  
  "55678",  2024,  "2024-05-15",   "2025-05-19", "likely",
  "55678",  2025,  "2025-05-15",   "2025-05-19", "likely",
 # "55678",  2026,  "2026-06-13",   "2025-06-16",
  
 
  "55679",  2024,  "2024-05-20",   "2024-05-21","likely",
  "55679",  2025,  "2025-05-25",   "2025-05-28","possible",
  "55679",  2026,  "2026-05-13",   "2026-05-16","likely",
 
  "55681",  2024,  "2024-05-17",   "2024-05-18","likely",
  "55681",  2025,  "2025-06-06",   "2025-06-08","likely",

 "55684",  2024,  "2024-05-18",   "2024-05-20","likely",
 "55684",  2025,  "2025-05-31",   "2025-06-03","likely",
 "55684",  2026,  "2026-05-24",   "2026-05-29","likely",

 #2024
 "55690",  2025,  "2025-05-10",   "2025-05-12","likely",

"55692",  2024,  "2024-05-14",   "2024-05-18", "possible",
 "55692",  2025,  "2025-06-05",   "2025-06-10","likely",
 

 "55694",  2024,  "2024-05-11",   "2024-05-13","likely",
 #"55694",  2025,  "2025-06-05",   "2025-06-09",
 "55694",  2026,  "2026-05-11",   "2026-05-14","likely",
 
 "55698",  2024,  "2024-05-21",   "2024-05-24","likely",
 "55698",  2025,  "2025-05-10",   "2025-05-15","likely",
 #"55698",  2026,  "2026-05-21",   "2026-05-24",
 
 "55699",  2024,  "2024-05-16",   "2024-05-22","likely",
 "55699",  2025,  "2025-05-24",   "2025-05-28","likely",
 "55699",  2026,  "2026-05-21",   "2026-05-22", "possible",

 "55700",  2024,  "2024-05-20",   "2024-05-23","likely",
 "55700",  2025,  "2025-05-29",   "2025-06-03","likely",

 "55701",  2025,  "2025-05-09",   "2025-05-13","likely",

 "55702",  2024,  "2024-05-22",   "2024-05-27","likely",
 "55702",  2025,  "2025-05-20",   "2025-05-25","likely",
 "55702",  2026,  "2026-05-22",   "2026-05-28", "likely",
 
"55707",  2024,  "2024-06-14",   "2024-06-16", "possible",
 "55707",  2025,  "2025-06-15",   "2025-06-17","likely",
 "55707",  2026,  "2026-05-21",   "2026-05-22", "possible",

 "556802",  2024,  "2024-05-13",   "2024-05-20","likely",
 "556802",  2025,  "2025-05-16",   "2025-05-24","likely",

 "556882",  2025,  "2025-05-11",   "2025-05-15","likely",
 "556882",  2026,  "2026-05-18",   "2026-05-22","likely"
 
) %>%
  mutate(
    tag_idn   = as.numeric(tag_idn),  # FIX: match type with be$tag_idn (numeric)
    # FIX: tz_use (defined above from real data) replaces the undefined
    # ewi_jyear$plot_date reference, which crashed the script immediately.
    box_start = update(as.POSIXct(box_start, tz = tz_use), year = ref_year),
    box_end   = update(as.POSIXct(box_end,   tz = tz_use), year = ref_year)
  )



# for each ewe and each year plot the step length and mcp over three years. 
for(ii in beu){
  
  #ii <-beu[2]
  
  print(ii)
  
  ewi <- be |> 
    filter(tag_idn == ii) |> 
    arrange(date_time_pst)
  
  highlight_ii <- highlight_ranges |>  filter(tag_idn == ii) |> 
    mutate(fill = case_when(
      confidence == "likely" ~ "darkorange",
      confidence == "possible" ~ "burlywood"
    ))
 
  # write out gps data to review 
  out_sf <- st_as_sf(ewi, coords = c("Longitude", "Latitude"), crs = 4326)
  out_sf <- st_transform(out_sf, crs = 3005)
  
  # write out subset cols as sf object 
  st_write(out_sf, fs::path(out_dir, paste0("location_lamb_", ii, ".gpkg")), append = FALSE)
  
  # GET INFO FOR HEADING
  age_annuali_capture <- unique(ewi$Age_annuli)
  ewi_years = unique(ewi$year_pst)
  date_collared = unique(ewi$capture_date)
  preg_2024 <- unique(ewi$pregnant_2024)
  preg_2025 <- unique(ewi$pregnant_2025)
  capture_preg_status = ifelse(preg_2024 %in% c("", "na"), preg_2025, preg_2024)
  
  # ------------------------------------------------------------
  # PLOT 1: one plot PER YEAR
  # ------------------------------------------------------------
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
      geom_line(aes(y = as.numeric(mcp_area_3d)), size = 0.2,colour= "blue", linetype = "longdash")+
      scale_y_continuous("movement rate (m/h)", sec.axis = sec_axis(~.*mcp_area_3d, name = "mcp three day ave")) +
      labs(title = paste0("Tag ID:", ii,  ",  Capture: ", date_collared,",  Capture Age annuali: ", age_annuali_capture, ", Preg on capture: " ,capture_preg_status, ", Plot Year: ", x),
           x = "Date", y = "movement rate (m/h)")+ 
      scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d")+
      annotate(geom = 'text', label = paste0("50% below mean > 36hrs =", mean_length), x = min(ewi_year$date_time_pst), y = 1600, hjust = 0, vjust = 1)+ 
      coord_cartesian(ylim = c(0,1600))
    
    print(sheep_move_year)
    ggsave(filename = fs::path(out_dir, paste0("ewe_",ii, "_", x, ".png")), plot = sheep_move_year, width = 12, height = 6)
    
  })
  
  
  # ------------------------------------------------------------
  # PLOT 2: one COMMON plot, all years overlaid as facet rows on a
  # shared (reference-year) date axis
  # ------------------------------------------------------------
  ewi_jyear <- ewi %>%
    mutate(
      year      = year(date_time_pst),
      plot_date = update(date_time_pst, year = ref_year)   # FIX: use the same ref_year (2000) as highlight_ranges
    )
  
  sheep_move_julian <- ggplot(ewi_jyear, aes(x = plot_date)) +
    geom_rect(data = highlight_ii,
              aes(xmin = box_start, xmax = box_end, ymin = -Inf, ymax = Inf, fill = fill),
              inherit.aes = FALSE,  alpha = 0.3) +
    geom_line(aes(y = speed_ave), colour = "darkgrey") +
    labs(title = paste0("Tag ID:", ii, ",  Capture: ", date_collared,
                        ",  Capture Age annuali: ", age_annuali_capture,
                        ", Preg on capture: ", capture_preg_status),
         x = "Date", y = "movement rate (m/h)")+
    scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
    scale_fill_identity(name = "Confidence",
                        breaks = c("darkorange", "burlywood"),
                        labels = c("likely", "possible"),
                        guide = "legend") +
    coord_cartesian(
      xlim = c(min(ewi_jyear$plot_date),
               as.POSIXct(paste0(ref_year, "-07-01 23:59:59"), tz = tz_use)),
      ylim = c(0, 1600))+
    facet_grid(year ~ .) +
    theme(strip.text = element_text(face = "bold", hjust = 0))
  
  print(sheep_move_julian)
  
  ggsave(filename = fs::path(out_dir, paste0("ewe_", ii, "_all_years_common.png")),
         plot = sheep_move_julian, width = 12, height = 8)
  
}


#####################################################
## Summary plots 
############################################

#
# ------------------------------------------------------------
# Lambing date drift: difference in estimated first lambing date
# between successive years, per ewe
# ------------------------------------------------------------

lamb_dates <- highlight_ranges |>
  mutate(
    lamb_date = box_start,          # assumption: box_start = estimated first lambing date
    # lamb_date = box_start + (box_end - box_start)/2,  # alt: use midpoint instead
    doy = yday(lamb_date)
  ) |>
  arrange(tag_idn, year)

# ------------------------------------------------------------
# Plot: diverging bars, direction = fill colour, confidence = alpha
# ------------------------------------------------------------
lamb_diff <- lamb_dates |>
  group_by(tag_idn) |>
  mutate(
    prev_year       = lag(year),
    doy_prev        = lag(doy),
    confidence_prev = lag(confidence),
    day_diff        = doy - doy_prev,
    year_gap        = year - prev_year,
    year_pair       = paste0(prev_year, "\u2192", year)
  ) |>
  ungroup() |>
  filter(!is.na(day_diff), year_gap == 1) |>
  mutate(
    direction = ifelse(day_diff >= 0, "Later", "Earlier"),
    tag_idn_f = factor(tag_idn),
    # overall confidence for the comparison: "possible" if either year is possible
    pair_confidence = ifelse(confidence == "possible" | confidence_prev == "possible",
                             "possible", "likely")
  )

# ------------------------------------------------------------
# Plot: diverging bars, direction = fill colour, confidence = alpha
# ------------------------------------------------------------
p_lamb_diverge <- ggplot(lamb_diff, aes(x = reorder(tag_idn_f, day_diff), y = day_diff,
                                        fill = direction, alpha = pair_confidence)) +
  geom_col(width = 0.7, colour = "grey30", linewidth = 0.2) +
  geom_hline(yintercept = 0, colour = "grey30") +
  geom_text(aes(label = day_diff,
                vjust = ifelse(day_diff >= 0, -0.4, 1.2)),
            alpha = 1, size = 3) +
  scale_fill_manual(values = c("Later" = "firebrick", "Earlier" = "steelblue")) +
  scale_alpha_manual(values = c("likely" = 1, "possible" = 0.4)) +
  labs(title = "Change in estimated first lambing date between successive years",
       subtitle = "Positive = later than previous year, negative = earlier | Faded bars = at least one year's estimate is 'possible'",
       x = "Ewe (Tag ID)", y = "Difference in lambing date (days)",
       fill = "Direction", alpha = "Confidence") +
  facet_wrap(~ year_pair, scales = "free_x", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold"))

print(p_lamb_diverge)
ggsave(fs::path(out_dir, "lambing_date_diff_diverging_bars_confidence.png"), p_lamb_diverge,
       width = 10, height = 8)



# ------------------------------------------------------------
# Plot 2: change in lambing date (days) between successive years
# ------------------------------------------------------------
#p_lamb_diff <- ggplot(lamb_diff, aes(x = year_pair, y = day_diff)) +
#  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
#  geom_boxplot(outlier.shape = NA, alpha = 0.3, colour = "grey40") +
#  geom_jitter(aes(colour = factor(tag_idn), shape = year_gap > 1), width = 0.15, size = 2.5) +
#  labs(title = "Change in estimated first lambing date between successive years",
#       subtitle = "Positive = later than previous year, negative = earlier. Triangle = non-consecutive years (gap > 1)",
#       x = "Year pair", y = "Difference in lambing date (days)",
#       colour = "Tag ID", shape = "Non-consecutive years") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#print(p_lamb_diff)
#ggsave(fs::path(out_dir, "lambing_date_diff_successive_years.png"), p_lamb_diff, width = 10, height = 6)


# ------------------------------------------------------------
# Simple point plot: Julian day (x) vs Tag ID (y), shaded by confidence

point_data <- highlight_ranges |>
  mutate(
    doy_mid   = yday(box_start), #+ (yday(box_end) - yday(box_start)) / 2,  # midpoint of window
    tag_idn_f = factor(tag_idn),
    year_f    = factor(year)
  )

p_lamb_points <- ggplot(point_data, aes(x = doy_mid, y = tag_idn_f)) +
  geom_point(aes(colour = year_f, alpha = confidence), size = 3) +
  scale_alpha_manual(values = c("likely" = 1, "possible" = 0.35)) +
  scale_colour_viridis_d(option = "D", begin = 0.2, end = 0.99) +
  scale_x_continuous(
    breaks = yday(seq(ymd("2000-05-01"), ymd("2000-06-30"), by = "1 week")),
    labels = format(seq(ymd("2000-05-01"), ymd("2000-06-30"), by = "1 week"), "%b %d"),
    minor_breaks = NULL,
    limits = c(yday(ymd("2000-05-01")), yday(ymd("2000-06-30")))
  ) +
  labs(#title = "Estimated lambing date by ewe and year",
       x = "Date (Julian day)", y = "Ewe (Tag ID)",
       colour = "Year", alpha = "Confidence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

print(p_lamb_points)

ggsave(fs::path(out_dir, "lambing_date_points_by_ewe.png"), p_lamb_points,
       width = 11, height = 8)


######################################################################
# summary of range of mini and maximum lambing dates for each ewe across years


point_data <- highlight_ranges |>
  mutate(
    doy_mid   = yday(box_start), #+ (yday(box_end) - yday(box_start)) / 2,  # midpoint of window
    tag_idn_f = factor(tag_idn),
    year_f    = factor(year)
  )

# how many options for successful breeding 
sum(ewe_success$n)
type_lamb <- point_data |> 
  group_by( confidence) |> 
  count()

# estimate the min, max and median julien date for all confidence types and for only likely events
# convert to date
sum_lamb_all <- point_data |> 
  group_by(year) |>
  summarise(min_lamb = min(doy_mid),
            max_lamb = max(doy_mid)) |> 
  ungroup() |> 
  mutate(range_lamb = max_lamb - min_lamb)

# convert to dates
sum_lamb_all <- sum_lamb_all |> 
  mutate(min_lamb_date = as.Date(min_lamb, origin = as.Date("2024-01-01")),
         max_lamb_date = as.Date(max_lamb, origin = as.Date("2024-01-01")))
  
my_date <- as.Date(my_julian,    # Convert Julian day to date
                   origin = as.Date("2024-01-01"))

# makes no different if only using likley vs all events.  
# sum_lamb_likley  <- point_data |> 
#   filter(confidence == "likely") |>
#   summarise(min_lamb = min(doy_mid),
#             max_lamb = max(doy_mid)) |> 
#   ungroup() |> 
#   mutate(range_lamb = max_lamb - min_lamb)
# 
# sum_lamb_likley




###############################################################################################
## Change to BBMM 
### Estimate the kde for breeding period (May 1st to June 30th) for each ewe and plot against movement rate

be <-  st_as_sf(be, coords = c("Longitude", "Latitude"), crs = 4326)
be <- st_transform(be, crs = 3005)
be <- cbind(be, st_coordinates(be)) |> 
  st_drop_geometry()

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
    #x <- ewi_years[1]

    ewi_year <- ewi |>
      filter(year_pst == x)

    if (nrow(ewi_year) >= 5) {
       
      dbisf <- ewi_year
      
      # 1. build a trajector# 1. build a trajector# 1. build a trajectory object
      tr <- as.ltraj(xy = dbisf[, c("X", "Y")], date = dbisf$date_time_pst, id = dbisf$tag_idn)
      
      # 2. estimate sig1 (Brownian motion variance) given a fixed GPS error (sig2)
      sig2 <- 30   # location error in metres -- adjust to your GPS spec
      lik  <- liker(tr, sig2 = sig2, rangesig1 = c(0.1, 500),plotit = F)
      sig1 <- lik[[1]]$sig1
      
      # 3. run the Brownian Bridge Movement Model
      kde_href <- kernelbb(tr, sig1 = sig1, sig2 = sig2, grid = 100)
      
      #dbisp <- dbisf |>
      #  select(tag_idn) |>
      #  as("Spatial")

      # # define the parameters (h, kern, grid, extent)
      #kde_href <- kernelUD(dbisp, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)

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

st_crs(kdes) <- 3005
st_write(kdes, path("02_draft_outputs", "ewes_lambing_yr_polygons_20260801bb.gpkg"))



##############################################################################


library(patchwork)
library(cowplot)  

year_colors <- c("2024" = "#E69F00", "2025" = "#56B4E9", "2026" = "#009E73")

focus_th_fidelity <- 95 #fidelity_summary$th[1]   # <-- change to inspect a different isopleth

poly_annual <- kdes |> 
  mutate(year = as.factor(year))

ids_fidelity <- poly_annual |>
  filter(th == focus_th_fidelity) |>
  st_drop_geometry() |>
  count(tag_idn, name = "n_years") |>
  filter(n_years >= 2) |>       # drop ids with only one year -- nothing to compare
  pull(tag_idn) |>
  sort()

# real panel's legend off.
legend_ref <- tibble(year = factor(names(year_colors), levels = names(year_colors)), x = 1, y = 1)

legend_ref_plot <- ggplot(legend_ref, aes(x, y, colour = year, fill = year)) +
  geom_point(size = 4, shape = 21) +
  scale_colour_manual(values = year_colors, name = "Year") +
  scale_fill_manual(values = year_colors, name = "Year") +
  theme_void() +
  theme(legend.position = "bottom")

shared_legend <- get_legend(legend_ref_plot)

id_plots <- map(ids_fidelity, function(this_id) {
  poly_annual |>
    filter(tag_idn == this_id, th == focus_th_fidelity) |>
    mutate(year = factor(as.character(year), levels = names(year_colors))) |>
    ggplot() +
    geom_sf(aes(colour = year, fill = year), alpha = 0.25, linewidth = 0.6) +
    scale_colour_manual(values = year_colors, drop = FALSE, guide = "none") +
    scale_fill_manual(values = year_colors, drop = FALSE, guide = "none") +
    labs(title = this_id) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "grey85", colour = NA),  # <-- adjust shade here
      panel.grid        = element_blank(),
      axis.title        = element_blank(),
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      plot.title        = element_text(size = 9, hjust = 0.5)
    )
})

panel_grid <- wrap_plots(id_plots) ##+
#  plot_annotation(
#    title = "Annual lambing homerange 95% (May 1st -June 31) across years"
#    )

all_yr_overlap<-wrap_elements(panel_grid) / wrap_elements(shared_legend) +
  plot_layout(heights = c(20, 1))   # legend gets a thin strip along the bottom

all_yr_overlap

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", "UD_lambing_95_annual_map.png"), 
       all_yr_overlap, width = 10, height = 7.5, dpi = 300)




# 
# ####################################################################
# ##### plot the kde for one ewe and year to check
# 
# # Kernal density estimate (lambing period)
# kde <- st_read(path("02_draft_outputs", "ewes_lambing_yr_polygons_20260801.gpkg"))
# 
# # select 50% threshold kde per lambing season for all years
# kde50 <- kde |> 
#   filter(th == 50) 
# st_write(kde50, path("02_draft_outputs", "ewes_lambing_yr_50th_poly.gpkg"))
# 
# 
# # select 50% threshold kde for each month for all years
# all_poly <- st_read(path("02_draft_outputs", "sheep_month_yr_polygons.gpkg")) |> 
#   filter(th == 50) |> 
#   filter(id %in% unique(kde$tag_idn))
# st_write(all_poly, path("02_draft_outputs", "ewes_kde_month_50th_poly.gpkg"), append = FALSE)
# 
# 
# # points 
# be_sub <- be |> 
#   select(tag_idn, date_time_pst, X, Y, Latitude,Longitude, tag_idn,mcp_area_3d  ) |>
#   st_as_sf(coords = c("X", "Y"), crs = 3005)
# st_write(be_sub, path("02_draft_outputs", "ewes_lambing_yr_pts.gpkg"))
# 
# 
# head(kde)
# 
# filter(kde, tag_idn == 55670, year == 2024) |> 
#   ggplot() +
#   geom_sf(aes(fill = as.factor(th))) +
#   scale_fill_manual(values = c("red", "orange", "yellow")) +
#   labs(title = "KDE Polygons for Tag ID: 55680 in 2024",
#        fill = "KDE Threshold") +
#   theme_minimal()
# 
# 
# # filter the smallest region (50% threshold)
# 
# kde50 <- kde |> 
#   filter(th == 50) |> 
#   st_transform(crs = 3005) 
# 
# 
# # get elevation  data - virtual raster
# aoi = st_read(fs::path("00_raw_data", "aoi.gpkg"))
# cded_raw <- bcmaps::cded(aoi)
# cded <- terra::rast(cded_raw) 
# cded_3005 <- terra::project(cded, "EPSG:3005")
# 
# 
# 
# kde50_plot1 <- ggplot() +
#   geom_sf(data = kde50, aes(fill = as.factor(year)), alpha = 0.8) +
#   scale_fill_manual(values = c("red", "orange", "yellow")) +
#   #geom_polygon(data = kde50, aes(x = long, y = lat, group = group), fill = "blue", alpha = 0.2) +
#   #ggnewscale::new_scale_fill() +
#   #tidyterra::geom_spatraster(data = cded_3005, alpha = 0.5, show.legend = FALSE) +
#   #tidyterra::scale_fill_terrain_c(direction = -1) +
#   labs(title = "KDE Polygons",
#        fill = "Year") +
#   facet_wrap(~tag_idn ) +
#  # geom_point(data = be, aes(x = X, y = Y), size = 0.2, colour = "blue") +
#   theme_minimal()+
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         rect = element_blank())
# 
# 
# 
# kde50_plot1
# 
# # for each ewe and year plot the movement rate and the kde polygons to check for overlap and potential parturition events.
#