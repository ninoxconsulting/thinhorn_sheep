## use KDE to determine the group and overlaps 

# identify which individual are overlapping and in the same group. 
# based on 1) entire year, 2) winter, 3) summer 4) Rut season ? 

library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot) 

# ============================================================================
# Pairwise home range overlap between individuals
# For each  year compute how much each pair of animals'
# polygons overlap (as a % of each animal's own range).
# ============================================================================


##############################################################
# 1. All year (full year)
###############################################################

all_poly <- st_read(fs::path("02_draft_outputs", "sheep_yr_polygons_bbmm30.gpkg"))

poly_clean <- all_poly |>
  group_by(id, th, year) |>
  summarise(geometry = st_union(geom), .groups = "drop") |>
  st_make_valid()

poly_annual_all <- poly_clean

# ---- 1. PAIRWISE OVERLAP WITHIN ONE STRATUM ---------------------------------
overlap_one_group <- function(sub_sf) {
  ids <- unique(sub_sf$id)
  if (length(ids) < 2) return(tibble())
  
  pairs <- combn(ids, 2, simplify = FALSE)
  
  map_dfr(pairs, function(p) {
    poly_i <- sub_sf |> filter(id == p[1])
    poly_j <- sub_sf |> filter(id == p[2])
    
    area_i <- as.numeric(st_area(poly_i)) / 10000   # hectares
    area_j <- as.numeric(st_area(poly_j)) / 10000
    
    inter <- suppressWarnings(st_intersection(poly_i, poly_j))
    if (nrow(inter) == 0) {
      area_inter <- 0
    } else {
      inter_poly <- suppressWarnings(st_collection_extract(inter, "POLYGON"))
      area_inter <- if (nrow(inter_poly) > 0) sum(as.numeric(st_area(inter_poly))) / 10000 else 0
    }
    
    union_area <- area_i + area_j - area_inter
    
    tibble(
      id_1 = p[1],
      id_2 = p[2],
      area_1_ha       = area_i,
      area_2_ha       = area_j,
      overlap_area_ha = area_inter,
      pct_overlap_1   = 100 * area_inter / area_i,   # % of id_1's range overlapped by id_2
      pct_overlap_2   = 100 * area_inter / area_j,   # % of id_2's range overlapped by id_1
      jaccard         = if (union_area > 0) area_inter / union_area else 0  # symmetric index
    )
  })
}

# ---- 2. RUN ACROSS EVERY year x th STRATUM --------------------------
overlap_tbl <- poly_clean |>
  group_by(year, th) |>
  group_split() |>
  map_dfr(function(grp) {
    res <- overlap_one_group(grp)
    if (nrow(res) > 0) {
      res <- res |> mutate(year = grp$year[1], th = grp$th[1])
    }
    res
  }) |>
  relocate( year, th)

overlap_tbl

# write out overlap summary
write.csv(overlap_tbl, path("02_draft_outputs", "sheep_all_yr_overlap_pc.csv"))


# # output map heatmap output 
# 
# focus_year  <- 2025 # change as needed
# focus_th    <- 95
# 
# heat_wide <- overlap_tbl |>
#   filter(year == focus_year, th == focus_th) |>
#   select(id_1, id_2, pct_overlap_1, pct_overlap_2)
# 
# # make it symmetric for a clean heatmap: one row per directional pair
# heat_df <- bind_rows(
#   heat_wide |> transmute(id = id_1, id_other = id_2, pct = pct_overlap_1),
#   heat_wide |> transmute(id = id_2, id_other = id_1, pct = pct_overlap_2)
# )
# 
# fullyr_ol <- ggplot(heat_df, aes(x = id_other, y = id, fill = pct)) +
#   geom_tile() +
#   geom_text(aes(label = round(pct)), size = 3) +
#   scale_fill_viridis_c(name = "% of\nrow id's\nrange", limits = c(1, 100)) +
#   labs(x = NULL, y = NULL,
#        title = paste("Pairwise annual home range overlap --",
#                      "year", focus_year, "th", focus_th),
#        subtitle = "Cell = % of row animal's range that overlaps column animal's range") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# fullyr_ol
# ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_annual_overlap_matrix_", focus_year, "_th", focus_th,".png")), 
#        fullyr_ol, width = 10, height = 10, dpi = 300)
# 



##############################################################
# 2. Winter Overlap 
##############################################################
# note year is winter year ie Dec from previous yr 

all_poly <- st_read(fs::path("02_draft_outputs", "sheep_winter_yr_polygons_bbmm30.gpkg"))
all_poly <- rename(all_poly, "year" = winter_year)

poly_clean <- all_poly |>
  group_by(id, th, year) |>
  summarise(geometry = st_union(geom), .groups = "drop") |>
  st_make_valid()

poly_winter <- poly_clean

# ---- 1. PAIRWISE OVERLAP WITHIN ONE STRATUM ---------------------------------
overlap_one_group <- function(sub_sf) {
  ids <- unique(sub_sf$id)
  if (length(ids) < 2) return(tibble())
  
  pairs <- combn(ids, 2, simplify = FALSE)
  
  map_dfr(pairs, function(p) {
    poly_i <- sub_sf |> filter(id == p[1])
    poly_j <- sub_sf |> filter(id == p[2])
    
    area_i <- as.numeric(st_area(poly_i)) / 10000   # hectares
    area_j <- as.numeric(st_area(poly_j)) / 10000
    
    inter <- suppressWarnings(st_intersection(poly_i, poly_j))
    if (nrow(inter) == 0) {
      area_inter <- 0
    } else {
      inter_poly <- suppressWarnings(st_collection_extract(inter, "POLYGON"))
      area_inter <- if (nrow(inter_poly) > 0) sum(as.numeric(st_area(inter_poly))) / 10000 else 0
    }
    
    union_area <- area_i + area_j - area_inter
    
    tibble(
      id_1 = p[1],
      id_2 = p[2],
      area_1_ha       = area_i,
      area_2_ha       = area_j,
      overlap_area_ha = area_inter,
      pct_overlap_1   = 100 * area_inter / area_i,   # % of id_1's range overlapped by id_2
      pct_overlap_2   = 100 * area_inter / area_j,   # % of id_2's range overlapped by id_1
      jaccard         = if (union_area > 0) area_inter / union_area else 0  # symmetric index
    )
  })
}

# ---- 2. RUN ACROSS EVERY  year x th STRATUM --------------------------
overlap_tbl <- poly_clean |>
  group_by( year, th) |>
  group_split() |>
  map_dfr(function(grp) {
    res <- overlap_one_group(grp)
    if (nrow(res) > 0) {
      res <- res |> mutate(year = grp$year[1], th = grp$th[1])
    }
    res
  }) |>
  relocate( year, th)

overlap_tbl

# write out overlap summary
write.csv(overlap_tbl, path("02_draft_outputs", "sheep_winter_yr_overlap_pc.csv"))

# # ============================================================================
# # 4. HEATMAP for one stratum -- visualize the pairwise overlap matrix
# # ============================================================================
# #overlap_tbl <- read.csv(fs::path("02_draft_outputs", "sheep_winter_yr_overlap_pc.csv")) |> 
# #  select(-X.1, -X)
# 
# head(overlap_tbl)
# focus_year  <- 2024
# focus_th    <- 95
# 
# heat_wide <- overlap_tbl |>
#   filter(year == focus_year, th == focus_th) |>
#   select(id_1, id_2, pct_overlap_1, pct_overlap_2)
# 
# # make it symmetric for a clean heatmap: one row per directional pair
# heat_df <- bind_rows(
#   heat_wide |> transmute(id = id_1, id_other = id_2, pct = pct_overlap_1),
#   heat_wide |> transmute(id = id_2, id_other = id_1, pct = pct_overlap_2)
# )
# 
# winter_ol <- ggplot(heat_df, aes(x = id_other, y = id, fill = pct)) +
#   geom_tile() +
#   geom_text(aes(label = round(pct)), size = 3) +
#   scale_fill_viridis_c(name = "% of\nrow id's\nrange", limits = c(1, 100)) +
#   labs(x = NULL, y = NULL,
#        title = paste("Pairwise winter home range overlap --",
#                      "year", focus_year, "th", focus_th),
#        subtitle = "Cell = % of row animal's range that overlaps column animal's range") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_winter_overlap_matrix_", focus_year, "_th", focus_th,".png")), 
#        winter_ol, width = 10, height = 10, dpi = 300)
# 



##############################################################
# 3. Summer Overlap 
##############################################################

all_poly <- st_read(fs::path("02_draft_outputs", "sheep_summer_yr_polygons_bbmm30.gpkg"))
all_poly <- rename(all_poly, "year" = year_pst)

poly_clean <- all_poly |>
  group_by(id, th, year) |>
  summarise(geometry = st_union(geom), .groups = "drop") |>
  st_make_valid()

poly_summer <- poly_clean

# ---- 1. PAIRWISE OVERLAP WITHIN ONE STRATUM ---------------------------------
overlap_one_group <- function(sub_sf) {
  ids <- unique(sub_sf$id)
  if (length(ids) < 2) return(tibble())
  
  pairs <- combn(ids, 2, simplify = FALSE)
  
  map_dfr(pairs, function(p) {
    poly_i <- sub_sf |> filter(id == p[1])
    poly_j <- sub_sf |> filter(id == p[2])
    
    area_i <- as.numeric(st_area(poly_i)) / 10000   # hectares
    area_j <- as.numeric(st_area(poly_j)) / 10000
    
    inter <- suppressWarnings(st_intersection(poly_i, poly_j))
    if (nrow(inter) == 0) {
      area_inter <- 0
    } else {
      inter_poly <- suppressWarnings(st_collection_extract(inter, "POLYGON"))
      area_inter <- if (nrow(inter_poly) > 0) sum(as.numeric(st_area(inter_poly))) / 10000 else 0
    }
    
    union_area <- area_i + area_j - area_inter
    
    tibble(
      id_1 = p[1],
      id_2 = p[2],
      area_1_ha       = area_i,
      area_2_ha       = area_j,
      overlap_area_ha = area_inter,
      pct_overlap_1   = 100 * area_inter / area_i,   # % of id_1's range overlapped by id_2
      pct_overlap_2   = 100 * area_inter / area_j,   # % of id_2's range overlapped by id_1
      jaccard         = if (union_area > 0) area_inter / union_area else 0  # symmetric index
    )
  })
}

# ---- 2. RUN ACROSS EVERY year x th STRATUM --------------------------
overlap_tbl <- poly_clean |>
  group_by( year, th) |>
  group_split() |>
  map_dfr(function(grp) {
    res <- overlap_one_group(grp)
    if (nrow(res) > 0) {
      res <- res |> mutate(year = grp$year[1], th = grp$th[1])
    }
    res
  }) |>
  relocate( year, th)

overlap_tbl

# write out overlap summary
write.csv(overlap_tbl, path("02_draft_outputs", "sheep_summer_yr_overlap_pc.csv"))


# # ============================================================================
# # 4. HEATMAP for one stratum -- visualize the pairwise overlap matrix
# focus_year  <- 2025
# focus_th    <- 95
# 
# heat_wide <- overlap_tbl |>
#   filter(year == focus_year, th == focus_th) |>
#   select(id_1, id_2, pct_overlap_1, pct_overlap_2)
# 
# # make it symmetric for a clean heatmap: one row per directional pair
# heat_df <- bind_rows(
#   heat_wide |> transmute(id = id_1, id_other = id_2, pct = pct_overlap_1),
#   heat_wide |> transmute(id = id_2, id_other = id_1, pct = pct_overlap_2)
# )
# 
# summer_ol <- ggplot(heat_df, aes(x = id_other, y = id, fill = pct)) +
#   geom_tile() +
#   geom_text(aes(label = round(pct)), size = 3) +
#   scale_fill_viridis_c(name = "% of\nrow id's\nrange", limits = c(1, 100)) +
#   labs(x = NULL, y = NULL,
#        title = paste("Pairwise summer home range overlap --",
#                      "year", focus_year, "th", focus_th),
#        subtitle = "Cell = % of row animal's range that overlaps column animal's range") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# summer_ol
# 
# ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_summer_overlap_matrix_", focus_year, "_th", focus_th,".png")), 
#        summer_ol, width = 10, height = 10, dpi = 300)






##############################################################
# 4. Rut Season Overlap Oct/ Nov 
##############################################################

all_poly <- st_read(fs::path("02_draft_outputs", "sheep_rut_yr_polygons_bbmm30.gpkg"))
all_poly <- rename(all_poly, "year" = year_pst)

poly_clean <- all_poly |>
  group_by(id, th, year) |>
  summarise(geometry = st_union(geom), .groups = "drop") |>
  st_make_valid()

poly_rut <- poly_clean

# ---- 1. PAIRWISE OVERLAP WITHIN ONE STRATUM ---------------------------------
overlap_one_group <- function(sub_sf) {
  ids <- unique(sub_sf$id)
  if (length(ids) < 2) return(tibble())
  
  pairs <- combn(ids, 2, simplify = FALSE)
  
  map_dfr(pairs, function(p) {
    poly_i <- sub_sf |> filter(id == p[1])
    poly_j <- sub_sf |> filter(id == p[2])
    
    area_i <- as.numeric(st_area(poly_i)) / 10000   # hectares
    area_j <- as.numeric(st_area(poly_j)) / 10000
    
    inter <- suppressWarnings(st_intersection(poly_i, poly_j))
    if (nrow(inter) == 0) {
      area_inter <- 0
    } else {
      inter_poly <- suppressWarnings(st_collection_extract(inter, "POLYGON"))
      area_inter <- if (nrow(inter_poly) > 0) sum(as.numeric(st_area(inter_poly))) / 10000 else 0
    }
    
    union_area <- area_i + area_j - area_inter
    
    tibble(
      id_1 = p[1],
      id_2 = p[2],
      area_1_ha       = area_i,
      area_2_ha       = area_j,
      overlap_area_ha = area_inter,
      pct_overlap_1   = 100 * area_inter / area_i,   # % of id_1's range overlapped by id_2
      pct_overlap_2   = 100 * area_inter / area_j,   # % of id_2's range overlapped by id_1
      jaccard         = if (union_area > 0) area_inter / union_area else 0  # symmetric index
    )
  })
}

# ---- 2. RUN ACROSS EVERY month x year x th STRATUM --------------------------
overlap_tbl <- poly_clean |>
  group_by( year, th) |>
  group_split() |>
  map_dfr(function(grp) {
    res <- overlap_one_group(grp)
    if (nrow(res) > 0) {
      res <- res |> mutate(year = grp$year[1], th = grp$th[1])
    }
    res
  }) |>
  relocate( year, th)

overlap_tbl

# write out overlap summary
write.csv(overlap_tbl, path("02_draft_outputs", "sheep_rut_yr_overlap_pc.csv"))


# # ============================================================================
# # 4. HEATMAP for one stratum -- visualize the pairwise overlap matrix
# # ============================================================================
# focus_year  <- 2024
# focus_th    <- 95
# 
# heat_wide <- overlap_tbl |>
#   filter(year == focus_year, th == focus_th) |>
#   select(id_1, id_2, pct_overlap_1, pct_overlap_2)
# 
# # make it symmetric for a clean heatmap: one row per directional pair
# heat_df <- bind_rows(
#   heat_wide |> transmute(id = id_1, id_other = id_2, pct = pct_overlap_1),
#   heat_wide |> transmute(id = id_2, id_other = id_1, pct = pct_overlap_2)
# )
# 
# rut_ol <- ggplot(heat_df, aes(x = id_other, y = id, fill = pct)) +
#   geom_tile() +
#   geom_text(aes(label = round(pct)), size = 3) +
#   scale_fill_viridis_c(name = "% of\nrow id's\nrange", limits = c(1, 100)) +
#   labs(x = NULL, y = NULL,
#        title = paste("Pairwise summer home range overlap --",
#                      "year", focus_year, "th", focus_th),
#        subtitle = "Cell = % of row animal's range that overlaps column animal's range") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# rut_ol
# 
# ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_rut_overlap_matrix_", focus_year, "_th", focus_th,".png")), 
#        rut_ol, width = 10, height = 10, dpi = 300)
# 
# 




##############################################################
# 5. Lambing period - ewes only May 1 to June 31st  
##############################################################

all_poly <- st_read(fs::path("02_draft_outputs", "ewes_lambing_yr_polygons_20260801bb.gpkg")) |> 
  select(-id)
all_poly <- rename(all_poly, "id" = tag_idn)

poly_clean <- all_poly |>
  group_by(id, th, year) |>
  summarise(geometry = st_union(geom), .groups = "drop") |>
  st_make_valid()

poly_lamb <- poly_clean

# ---- 1. PAIRWISE OVERLAP WITHIN ONE STRATUM ---------------------------------
overlap_one_group <- function(sub_sf) {
  ids <- unique(sub_sf$id)
  if (length(ids) < 2) return(tibble())
  
  pairs <- combn(ids, 2, simplify = FALSE)
  
  map_dfr(pairs, function(p) {
    poly_i <- sub_sf |> filter(id == p[1])
    poly_j <- sub_sf |> filter(id == p[2])
    
    area_i <- as.numeric(st_area(poly_i)) / 10000   # hectares
    area_j <- as.numeric(st_area(poly_j)) / 10000
    
    inter <- suppressWarnings(st_intersection(poly_i, poly_j))
    if (nrow(inter) == 0) {
      area_inter <- 0
    } else {
      inter_poly <- suppressWarnings(st_collection_extract(inter, "POLYGON"))
      area_inter <- if (nrow(inter_poly) > 0) sum(as.numeric(st_area(inter_poly))) / 10000 else 0
    }
    
    union_area <- area_i + area_j - area_inter
    
    tibble(
      id_1 = p[1],
      id_2 = p[2],
      area_1_ha       = area_i,
      area_2_ha       = area_j,
      overlap_area_ha = area_inter,
      pct_overlap_1   = 100 * area_inter / area_i,   # % of id_1's range overlapped by id_2
      pct_overlap_2   = 100 * area_inter / area_j,   # % of id_2's range overlapped by id_1
      jaccard         = if (union_area > 0) area_inter / union_area else 0  # symmetric index
    )
  })
}

# ---- 2. RUN ACROSS EVERY month x year x th STRATUM --------------------------
overlap_tbl <- poly_clean |>
  group_by( year, th) |>
  group_split() |>
  map_dfr(function(grp) {
    res <- overlap_one_group(grp)
    if (nrow(res) > 0) {
      res <- res |> mutate(year = grp$year[1], th = grp$th[1])
    }
    res
  }) |>
  relocate( year, th)

overlap_tbl

# write out overlap summary
write.csv(overlap_tbl, path("02_draft_outputs", "ewe_lambing_yr_overlap_pc.csv"))

# # ============================================================================
# # 4. HEATMAP for one stratum -- visualize the pairwise overlap matrix
# # ============================================================================
# focus_year  <- 2024
# focus_th    <- 95
# 
# heat_wide <- overlap_tbl |>
#   filter(year == focus_year, th == focus_th) |>
#   select(id_1, id_2, pct_overlap_1, pct_overlap_2)
# 
# # make it symmetric for a clean heatmap: one row per directional pair
# heat_df <- bind_rows(
#   heat_wide |> transmute(id = id_1, id_other = id_2, pct = pct_overlap_1),
#   heat_wide |> transmute(id = id_2, id_other = id_1, pct = pct_overlap_2)
# )
# 
# rut_ol <- ggplot(heat_df, aes(x = id_other, y = id, fill = pct)) +
#   geom_tile() +
#   geom_text(aes(label = round(pct)), size = 3) +
#   scale_fill_viridis_c(name = "% of\nrow id's\nrange", limits = c(1, 100)) +
#   labs(x = NULL, y = NULL,
#        title = paste("Pairwise summer home range overlap --",
#                      "year", focus_year, "th", focus_th),
#        subtitle = "Cell = % of row animal's range that overlaps column animal's range") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# rut_ol
# 
# ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_rut_overlap_matrix_", focus_year, "_th", focus_th,".png")), 
#        rut_ol, width = 10, height = 10, dpi = 300)
# 
# 







# ============================================================================
# 5. SITE FIDELITY: overlap for ONE individual across multiple years
# For each id x th, build one polygon per year (union of that year's months)
# then compare years pairwise -- how much does an individual's range repeat
# from one year to the next?
# ============================================================================

# generate a function to calculate the summary per group 

summary_overlap <- function(poly_clean, out_name) {
  # ---- 5a. one polygon per id x th x year (union across months) --------------
  poly_annual <- poly_clean |>
    mutate(id = as.character(id)) |>
    group_by(id, th, year) |>
    summarise(geometry = st_union(geometry), .groups = "drop") |>
    st_make_valid()

  # ---- 5b. pairwise overlap between years, within each id x th ----------------
  overlap_one_id_years <- function(sub_sf) {
    years <- unique(sub_sf$year)
    if (length(years) < 2) {
      return(tibble())
    }

    pairs <- combn(years, 2, simplify = FALSE)

    map_dfr(pairs, function(p) {
      poly_1 <- sub_sf |> filter(year == p[1])
      poly_2 <- sub_sf |> filter(year == p[2])

      area_1 <- as.numeric(st_area(poly_1)) / 10000
      area_2 <- as.numeric(st_area(poly_2)) / 10000

      inter <- suppressWarnings(st_intersection(poly_1, poly_2))
      if (nrow(inter) == 0) {
        area_inter <- 0
      } else {
        inter_poly <- suppressWarnings(st_collection_extract(inter, "POLYGON"))
        area_inter <- if (nrow(inter_poly) > 0) sum(as.numeric(st_area(inter_poly))) / 10000 else 0
      }

      union_area <- area_1 + area_2 - area_inter

      tibble(
        year_1          = p[1],
        year_2          = p[2],
        area_1_ha       = area_1,
        area_2_ha       = area_2,
        overlap_area_ha = area_inter,
        pct_overlap_1   = 100 * area_inter / area_1, # % of year_1's range re-used in year_2
        pct_overlap_2   = 100 * area_inter / area_2, # % of year_2's range that was used in year_1
        jaccard         = if (union_area > 0) area_inter / union_area else 0
      )
    })
  }

  year_overlap_tbl <- poly_annual |>
    group_by(id, th) |>
    group_split() |>
    map_dfr(function(grp) {
      res <- overlap_one_id_years(grp)
      if (nrow(res) > 0) {
        res <- res |> mutate(id = grp$id[1], th = grp$th[1])
      }
      res
    }) |>
    relocate(id, th)

  year_overlap_tbl

  # write out overlap summary
  write.csv(year_overlap_tbl, path("02_draft_outputs", out_name)) # "sheep_byid_all_yr_overlap_pc.csv"))
}


# cycle through each of the four seasons. 

allyrs <- summary_overlap(poly_annual_all, "sheep_byid_all_yr_overlap_pc.csv")
summer <- summary_overlap(poly_summer, "sheep_byid_summer_yr_overlap_pc")
winter <- summary_overlap(poly_winter, "sheep_byid_winter_yr_overlap_pc.csv")
lamb <- summary_overlap(poly_lamb, "sheep_byid_lamb_yr_overlap_pc.csv")
rut <- summary_overlap(poly_rut, "sheep_byid_rut_yr_overlap_pc.csv")










###########################################################
## Review the overlap  - TABLES 
##########################################################

## review the summer overlap in ewes 
summer <- read.csv( path("02_draft_outputs","sheep_byid_summer_yr_overlap_pc")) |> select(-X)
year_overlap_tbl <- summer

# # ---- 5c. site fidelity score per individual (mean overlap across all year
# #          pairs available for that animal, per th) --------------------------
fidelity_summary <- year_overlap_tbl |>
   group_by(id, th) |>
   summarise(
     n_year_pairs     = n(),
     mean_jaccard     = mean(jaccard),
     mean_pct_overlap = mean(c(pct_overlap_1, pct_overlap_2)),
     .groups = "drop"
   ) |>
   arrange(th, desc(mean_jaccard))
 
fidelity_summary
# Get the ID nnumber of the sheep with largest overlap 
#top <- fidelity_summary |> 
#  filter(th ==95) |> 
#  arrange(mean_pct_overlap)
 
 

id_key1 <- id_key |> rename("id"= "tag_idn") |>  
  select(id, sex)

fidelity_summary_95 <- fidelity_summary |>
  filter(th == 95) |> 
  mutate(tag_idn = as.character(id))

fidelity_summary_95<- left_join(fidelity_summary_95, id_key)

fid_table <- fidelity_summary_95 |>
  group_by(sex) |> 
  summarise(n = n(),
            min_pct = min(mean_pct_overlap),
            max_pct = max(mean_pct_overlap),
            mean_pct_overlap = mean(mean_pct_overlap))
fid_table


###################################################################################
# Table for all years 

summer <- read.csv( path("02_draft_outputs","sheep_byid_all_yr_overlap_pc.csv")) |> select(-X)
year_overlap_tbl <- summer

# # ---- 5c. site fidelity score per individual (mean overlap across all year
# #          pairs available for that animal, per th) --------------------------
fidelity_summary <- year_overlap_tbl |>
  group_by(id, th) |>
  summarise(
    n_year_pairs     = n(),
    mean_jaccard     = mean(jaccard),
    mean_pct_overlap = mean(c(pct_overlap_1, pct_overlap_2)),
    .groups = "drop"
  ) |>
  arrange(th, desc(mean_jaccard))

#fidelity_summary

id_key1 <- id_key |> rename("id"= "tag_idn") |>  
  select(id, sex)

fidelity_summary_95 <- fidelity_summary |>
  filter(th == 95) |> 
  mutate(tag_idn = as.character(id))

fidelity_summary_95<- left_join(fidelity_summary_95, id_key)

fid_table <- fidelity_summary_95 |>
  group_by(sex) |> 
  summarise(n = n(),
            min_pct = min(mean_pct_overlap),
            max_pct = max(mean_pct_overlap),
            mean_pct_overlap = mean(mean_pct_overlap))
fid_table

####################################################
## winter values: 

summer <- read.csv( path("02_draft_outputs","sheep_byid_winter_yr_overlap_pc.csv")) |> select(-X)
year_overlap_tbl <- summer

# # ---- 5c. site fidelity score per individual (mean overlap across all year
# #          pairs available for that animal, per th) --------------------------
fidelity_summary <- year_overlap_tbl |>
  group_by(id, th) |>
  summarise(
    n_year_pairs     = n(),
    mean_jaccard     = mean(jaccard),
    mean_pct_overlap = mean(c(pct_overlap_1, pct_overlap_2)),
    .groups = "drop"
  ) |>
  arrange(th, desc(mean_jaccard))

#fidelity_summary

id_key1 <- id_key |> rename("id"= "tag_idn") |>  
  select(id, sex)

fidelity_summary_95 <- fidelity_summary |>
  filter(th == 95) |> 
  mutate(tag_idn = as.character(id))

fidelity_summary_95 <- left_join(fidelity_summary_95, id_key)

fidelity_summary_95<- fidelity_summary_95 |> 
  filter(mean_pct_overlap !=0)

fid_table <- fidelity_summary_95 |>
  group_by(sex) |> 
  summarise(n = n(),
            min_pct = min(mean_pct_overlap),
            max_pct = max(mean_pct_overlap),
            mean_pct_overlap = mean(mean_pct_overlap))
fid_table


##################################################################################################
## LAMBING  area overlaps 
# what is the propotion of lambing period in overlap 

lamb <- read.csv( path("02_draft_outputs","sheep_byid_lamb_yr_overlap_pc.csv")) |> select(-X)
year_overlap_tbl <- lamb

fidelity_summary <- year_overlap_tbl |>
  group_by(id, th) |>
  summarise(
    n_year_pairs     = n(),
    mean_jaccard     = mean(jaccard),
    mean_pct_overlap = mean(c(pct_overlap_1, pct_overlap_2)),
    .groups = "drop"
  ) |>
  arrange(th, desc(mean_jaccard))

id_key1 <- id_key |> rename("id"= "tag_idn") |>  
  select(id, sex)

fidelity_summary_95 <- fidelity_summary |>
  filter(th == 95) |> 
  mutate(id = as.character(id))

fidelity_summary_95<- left_join(fidelity_summary_95, id_key1)

fid_table <- fidelity_summary_95 |>
  group_by(sex) |> 
  summarise(n = n(),
            min_pct = min(mean_pct_overlap),
            max_pct = max(mean_pct_overlap),
            mean_pct_overlap = mean(mean_pct_overlap))
fid_table

############################################################################


## overlap plots # 1) 
## ENTIRE RANGE PER INDIVIDUAL 95% ## SEE SCRIPT 06_report_figures 
## 1) ewes 
## 2) males 


## overlap plots # 2) 
## Summer home range at 95% 
## 1) ewes 

## lambing plot  plots # 3) - already generated 
## Summer home range at 95% 
## 1) ewes 


## overlap plot 4) 
## Rut range at 95% 
## 1) Males only (grouped by class?)


######################################################################

## overlap plots # 2) 
## Summer home range at 95% 
## 1) ewes 

poly_annual <- poly_summer 
poly_annual <- poly_annual |>
  mutate(tag_idn = as.character(id))

poly_annual<- left_join(poly_annual, id_key)

year_colors <- c("2024" = "#E69F00", "2025" = "#56B4E9", "2026" = "#009E73")

focus_th_fidelity <- 95 #fidelity_summary$th[1]   # <-- change to inspect a different isopleth


## EWES 
poly_annual <- poly_annual |> 
  mutate(year = as.factor(year)) |> 
  filter(sex =="female")

ids_fidelity <- poly_annual |>
  filter(th == focus_th_fidelity) |>
  st_drop_geometry() |>
  count(id, name = "n_years") |>
  filter(n_years >= 2) |>       # drop ids with only one year -- nothing to compare
  pull(id) |>
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
    filter(id == this_id, th == focus_th_fidelity) |>
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

panel_grid <- wrap_plots(id_plots) +
  plot_annotation(
    title = paste("Summer home range (95%) for females"),
   # subtitle = "More overlapping shapes across years = higher site fidelity (each panel independently zoomed)"
  )

all_yr_overlap<-wrap_elements(panel_grid) / wrap_elements(shared_legend) +
  plot_layout(heights = c(20, 1))   # legend gets a thin strip along the bottom

all_yr_overlap

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_overlap_female_summer_map_th", focus_th,".png")), 
       all_yr_overlap, width = 8, height = 8, dpi = 300)



################################################################################
## Plot Male Rams overlap
################################################################################

poly_annual <- poly_rut |>
  mutate(tag_idn = as.character(id))

poly_annual<- left_join(poly_annual, id_key)

year_colors <- c("2024" = "#E69F00", "2025" = "#56B4E9", "2026" = "#009E73")

focus_th_fidelity <- 95 #fidelity_summary$th[1]   # <-- change to inspect a different isopleth


poly_annual <- poly_annual |> 
  mutate(year = as.factor(year)) |> 
  filter(sex =="male") |> 
  rowwise() |> 
  mutate(id = paste0(id, " (",Age_annuli,")"))

ids_fidelity <- poly_annual |>
  filter(th == focus_th_fidelity) |>
  st_drop_geometry() |>
  count(id, name = "n_years") |>
  filter(n_years >= 2) |>       # drop ids with only one year -- nothing to compare
  pull(id) |>
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
    filter(id == this_id, th == focus_th_fidelity) |>
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

panel_grid <- wrap_plots(id_plots) +
  plot_annotation(
    title = paste("Rut home range (95%) for males"),
    # subtitle = "More overlapping shapes across years = higher site fidelity (each panel independently zoomed)"
  )

all_yr_overlap<-wrap_elements(panel_grid) / wrap_elements(shared_legend) +
  plot_layout(heights = c(20, 1))   # legend gets a thin strip along the bottom

all_yr_overlap

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_overlap_males_Rut_map_th", focus_th,".png")), 
       all_yr_overlap, width = 8, height = 8, dpi = 300)

