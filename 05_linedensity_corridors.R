#05_point to point line density 

library("rnaturalearth")
library("rnaturalearthdata")
library(lubridate)
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)


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
## Part 1: build lines between points per individual, keep month, year and id

all_pts <- allpts |> 
  select(tag_idn, date_time_pst, X, Y, year_pst) |> 
  #st_drop_geometry() |> 
  group_by(tag_idn) |>
  arrange(date_time_pst) |>
  ungroup() |>
  mutate(id_order = seq(1, length(allpts$tag_idn), 1))

all_pts <- all_pts |> tibble::rowid_to_column("idr")
proj <- sf::st_crs(all_pts)

## convert GPSPoints to a table for manipulation
GPSPoints <- cbind(all_pts, sf::st_coordinates(all_pts))
GPSPoints <- GPSPoints |> sf::st_drop_geometry()

# iterate through transect id
tag_id <- unique(GPSPoints$tag_idn)
#tag_id <- tag_id[1:200]

all_lines <- purrr::map(tag_id, function(x) {
   
  #x <- tag_id[1] # testing line
  print(x)
   #x = 242570
  
  GPSPoints_transect <- GPSPoints |>
    dplyr::filter(tag_idn == x)
  
  ## Define the Line Start and End Coordinates and Add XY coordinates as
  
  GPSPoints_transect |>
    dplyr::mutate(
      Xend = dplyr::lead(.data$X),
      Yend = dplyr::lead(.data$Y),
      DTend = dplyr::lead(.data$date_time_pst )
    ) |>
    dplyr::filter(!is.na(.data$Yend)) |>
    dplyr::rowwise(.data$id_order) |>
    dplyr::mutate(geometry = sf::st_sfc(
      sf::st_linestring(
        x = matrix(c(.data$X, .data$Xend, .data$Y, .data$Yend), ncol = 2)
      )
    )) |>
    sf::st_sf(crs = proj)
}) |> dplyr::bind_rows()

# check 1:100
unique(all_lines$tag_idn)

# all_lines <- sf::st_make_valid(all_lines)
al<- all_lines |>
  mutate(date_time_from = date_time_pst ) |>
  mutate(date_time_to = DTend) |>
  select(tag_idn,date_time_from, year_pst, date_time_to)


#sf::st_write(al, fs::path("02_draft_outputs", "lines_sheep.gpkg"), driver = "GPKG", append = FALSE)
## Note this one above was edited in QGIS to highlight the errors = 1. 


