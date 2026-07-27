## use KDE to determine the group and overlaps 

# identify which individual are overlapping and in the same group. 
# based on 1) entire year, 2) winter, 3) summer 4) Rut season ? 

library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# ============================================================================
# Pairwise home range overlap between individuals
# For each  year compute how much each pair of animals'
# polygons overlap (as a % of each animal's own range).
# ============================================================================


all_poly <- st_read(fs::path("02_draft_outputs", "sheep_yr_polygons_bbmm30.gpkg"))

#all_poly <- all_poly |>
#  filter(th ==50)

##############################################################
# 1. All year (full year)
###############################################################

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

# ---- 2. RUN ACROSS EVERY month x year x th STRATUM --------------------------
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


# output map heatmap output 

focus_year  <- 2025 # change as needed
focus_th    <- 95

heat_wide <- overlap_tbl |>
  filter(year == focus_year, th == focus_th) |>
  select(id_1, id_2, pct_overlap_1, pct_overlap_2)

# make it symmetric for a clean heatmap: one row per directional pair
heat_df <- bind_rows(
  heat_wide |> transmute(id = id_1, id_other = id_2, pct = pct_overlap_1),
  heat_wide |> transmute(id = id_2, id_other = id_1, pct = pct_overlap_2)
)

fullyr_ol <- ggplot(heat_df, aes(x = id_other, y = id, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = round(pct)), size = 3) +
  scale_fill_viridis_c(name = "% of\nrow id's\nrange", limits = c(1, 100)) +
  labs(x = NULL, y = NULL,
       title = paste("Pairwise annual home range overlap --",
                     "year", focus_year, "th", focus_th),
       subtitle = "Cell = % of row animal's range that overlaps column animal's range") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


fullyr_ol
ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_annual_overlap_matrix_", focus_year, "_th", focus_th,".png")), 
       fullyr_ol, width = 10, height = 10, dpi = 300)




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
write.csv(overlap_tbl, path("02_draft_outputs", "sheep_summer_yr_overlap_pc.csv"))


# ============================================================================
# 4. HEATMAP for one stratum -- visualize the pairwise overlap matrix
# ============================================================================
focus_year  <- 2025
focus_th    <- 95

heat_wide <- overlap_tbl |>
  filter(year == focus_year, th == focus_th) |>
  select(id_1, id_2, pct_overlap_1, pct_overlap_2)

# make it symmetric for a clean heatmap: one row per directional pair
heat_df <- bind_rows(
  heat_wide |> transmute(id = id_1, id_other = id_2, pct = pct_overlap_1),
  heat_wide |> transmute(id = id_2, id_other = id_1, pct = pct_overlap_2)
)

winter_ol <- ggplot(heat_df, aes(x = id_other, y = id, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = round(pct)), size = 3) +
  scale_fill_viridis_c(name = "% of\nrow id's\nrange", limits = c(1, 100)) +
  labs(x = NULL, y = NULL,
       title = paste("Pairwise winter home range overlap --",
                     "year", focus_year, "th", focus_th),
       subtitle = "Cell = % of row animal's range that overlaps column animal's range") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_winter_overlap_matrix_", focus_year, "_th", focus_th,".png")), 
       winter_ol, width = 10, height = 10, dpi = 300)


##############################################################
# 3. Summer Overlap 
##############################################################
# note year is winter year ie Dec from previous yr 

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
write.csv(overlap_tbl, path("02_draft_outputs", "sheep_summer_yr_overlap_pc.csv"))


# ============================================================================
# 4. HEATMAP for one stratum -- visualize the pairwise overlap matrix
# ============================================================================
focus_year  <- 2025
focus_th    <- 95

heat_wide <- overlap_tbl |>
  filter(year == focus_year, th == focus_th) |>
  select(id_1, id_2, pct_overlap_1, pct_overlap_2)

# make it symmetric for a clean heatmap: one row per directional pair
heat_df <- bind_rows(
  heat_wide |> transmute(id = id_1, id_other = id_2, pct = pct_overlap_1),
  heat_wide |> transmute(id = id_2, id_other = id_1, pct = pct_overlap_2)
)

summer_ol <- ggplot(heat_df, aes(x = id_other, y = id, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = round(pct)), size = 3) +
  scale_fill_viridis_c(name = "% of\nrow id's\nrange", limits = c(1, 100)) +
  labs(x = NULL, y = NULL,
       title = paste("Pairwise summer home range overlap --",
                     "year", focus_year, "th", focus_th),
       subtitle = "Cell = % of row animal's range that overlaps column animal's range") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summer_ol

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_summer_overlap_matrix_", focus_year, "_th", focus_th,".png")), 
       summer_ol, width = 10, height = 10, dpi = 300)









# ============================================================================
# 5. SITE FIDELITY: overlap for ONE individual across multiple years
# For each id x th, build one polygon per year (union of that year's months)
# then compare years pairwise -- how much does an individual's range repeat
# from one year to the next?
# ============================================================================
poly_annual_all # all years 
poly_summer     # summers 
poly_winter     # winter 

poly_clean <- poly_annual_all


# ---- 5a. one polygon per id x th x year (union across months) --------------
poly_annual <- poly_clean |>
  mutate(id = as.character(id)) |>
  group_by(id, th, year) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid()

# ---- 5b. pairwise overlap between years, within each id x th ----------------
overlap_one_id_years <- function(sub_sf) {
  years <- unique(sub_sf$year)
  if (length(years) < 2) return(tibble())
  
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
      pct_overlap_1   = 100 * area_inter / area_1,  # % of year_1's range re-used in year_2
      pct_overlap_2   = 100 * area_inter / area_2,  # % of year_2's range that was used in year_1
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
write.csv(year_overlap_tbl, path("02_draft_outputs", "sheep_byid_all_yr_overlap_pc.csv"))

# ---- 5c. site fidelity score per individual (mean overlap across all year
#          pairs available for that animal, per th) --------------------------
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
# animals near the top of this table (high mean_jaccard) keep coming back to
# roughly the same area year after year; animals near the bottom are ranging
# into substantially different areas from one year to the next.

# # ---- 5d. rank individuals by site fidelity, one panel per isopleth ---------
# ggplot(fidelity_summary, aes(x = reorder(id, mean_jaccard), y = mean_jaccard)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   facet_wrap(~th, labeller = label_both) +
#   labs(x = NULL, y = "Mean year-to-year overlap (Jaccard)",
#        title = "Site fidelity: how consistently each individual reuses the same area across years")

# ---- 5e. map ONE individual's range across all years, overlaid -------------
focus_id_fidelity <- fidelity_summary$id[1]   # <-- change to inspect a specific animal
focus_th_fidelity  <- 95# fidelity_summary$th[1]

poly_annual |>
  filter(id == focus_id_fidelity, th == focus_th_fidelity) |>
  ggplot() +
  geom_sf(aes(colour = year, fill = as.factor(year)), alpha = 0.25, linewidth = 0.8) +
  labs(title = paste("Annual range across years -- id", focus_id_fidelity, "th", focus_th_fidelity),
       subtitle = "More overlapping shapes across years = higher site fidelity")



# ---- 5e. map ALL individuals' ranges across years, faceted by id -----------
library(patchwork)
library(cowplot)  

year_colors <- c("2024" = "#E69F00", "2025" = "#56B4E9", "2026" = "#009E73")

focus_th_fidelity <- 95 #fidelity_summary$th[1]   # <-- change to inspect a different isopleth

poly_annual <- poly_annual |> 
  mutate(year = as.factor(year))

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
    title = paste("Annual range across years, all individuals -- th", focus_th_fidelity),
    subtitle = "More overlapping shapes across years = higher site fidelity (each panel independently zoomed)"
  )

all_yr_overlap<-wrap_elements(panel_grid) / wrap_elements(shared_legend) +
  plot_layout(heights = c(20, 1))   # legend gets a thin strip along the bottom

all_yr_overlap

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures", paste0("UD_overlap_annual_map_th", focus_th,".png")), 
       all_yr_overlap, width = 10, height = 10, dpi = 300)









