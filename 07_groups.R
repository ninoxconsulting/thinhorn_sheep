# ============================================================
# Schematic network diagram: groups of sheep based on
# home range overlap (pairwise KDE overlap data)
# ------------------------------------------------------------

library(dplyr)
library(igraph)
library(ggraph)
library(patchwork)
library(Polychrome) 

# read in the summary data 
clean_dir <- fs::path("01_clean_data")
out_dir <- fs::path("02_draft_outputs/01_lamb_figures")

# use .gpkg as csv drops time stamp
allpts <- st_read(fs::path("01_clean_data", "location_steps_all_20260804.gpkg"))

# generate a key to be used in plots below
id_key <- allpts |> 
  select(tag_idn, sex,Age_annuli, sheep_class) |>
  st_drop_geometry() |> 
  unique() 

id_key <- id_key |> 
  mutate(herd = case_when(
    tag_idn %in% c("55692", "55698", "556912", "58856","556692" ) ~ "blue",
    tag_idn %in% c("55702", "55707","55708") ~ "red",
    tag_idn %in% c("55683", "55687","55690", "55704", "55693", "55701", "55670","55674","55676") ~ "red",
    tag_idn %in% c("55684","55681", "55699","55695","55679", '55694', '55677', '55678', '55705', '55700') ~ "green",
    tag_idn %in% c("55689", "55672","556802", "55673", "55675", "55697", "55671", "55703", "557062","556882", "58857") ~ "green", #"55701", "55670","55674","55676") ~ "red",
    TRUE ~ "Other"
  ))


#allyrs <- summary_overlap(poly_annual_all, "sheep_byid_all_yr_overlap_pc.csv")
#summer <- summary_overlap(poly_summer, "sheep_byid_summer_yr_overlap_pc")
#winter <- summary_overlap(poly_winter, "sheep_byid_winter_yr_overlap_pc.csv")
#lamb <- summary_overlap(poly_lamb, "sheep_byid_lamb_yr_overlap_pc.csv")

overlap_data_all <- read.csv(fs::path("02_draft_outputs", "sheep_rut_yr_overlap_pc.csv")) |> 
  select( -X) |> 
  mutate(id_1 = as.character(id_1),
    id_2 = as.character(id_2))

# greenherd <- id_key |> 
#   filter(herd == "green") |> 
#   pull(tag_idn) |> 
#   as.character()

overlap_data <- overlap_data_all %>%
  filter(year >= 2019) %>%
  mutate(id_1 = as.character(id_1),
         id_2 = as.character(id_2))# |> 
 # filter(id_1 %in% greenherd | id_2 %in% greenherd)
  

sex_age_lookup <- id_key %>%
  distinct(tag_idn, sex, Age_annuli) %>%
  mutate(tag_idn = as.character(tag_idn))

th_level <- 95

all_ids <- overlap_data %>%
  filter(th == th_level) %>%
  { sort(unique(c(.$id_1, .$id_2))) }

id_colours <- as.vector(createPalette(
  length(all_ids),
  seedcolors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
  range = c(20, 80)
))
names(id_colours) <- all_ids

# ------------------------------------------------------------
# 3. Build one network plot per year
# ------------------------------------------------------------
make_network_plot <- function(yr) {
  
  data_yr <- overlap_data %>% filter(year == yr, th == th_level)
  
  nodes_yr <- data.frame(name = sort(unique(c(data_yr$id_1, data_yr$id_2)))) %>%
    left_join(sex_age_lookup, by = c("name" = "tag_idn"))
  
  edges_yr <- data_yr %>%
    filter(jaccard > 0) %>%
    select(id_1, id_2, jaccard, pct_overlap_1, pct_overlap_2, overlap_area_ha)
  
  missing_ids <- setdiff(unique(c(edges_yr$id_1, edges_yr$id_2)), nodes_yr$name)
  if (length(missing_ids) > 0) {
    stop("Year ", yr, ": edge IDs missing from vertex list: ",
         paste(missing_ids, collapse = ", "))
  }
  
  g <- graph_from_data_frame(edges_yr, directed = FALSE, vertices = nodes_yr)
  
  if (ecount(g) > 0) {
    comm <- cluster_louvain(g, weights = E(g)$jaccard)
    V(g)$group <- factor(membership(comm))
  } else {
    V(g)$group <- factor(1)
  }
  
  ggraph(g, layout = "fr") +   # force-directed layout: connected nodes pull together
    geom_edge_link(aes(width = jaccard, alpha = jaccard), colour = "grey40") +
    scale_edge_width(range = c(0.3, 3), guide = "none") +
    scale_edge_alpha(range = c(0.25, 0.9), guide = "none") +
    geom_node_point(aes(fill = name, shape = sex, size = Age_annuli), colour = "black", stroke = 0.6) +
    #geom_node_text(aes(label = name), size = 3, repel = TRUE, max.overlaps = 20) +
    geom_node_label(aes(label = name), size = 2.8,
                    repel = TRUE, max.overlaps = 30,
                    box.padding = 0.6, point.padding = 0.4,
                    label.padding = unit(0.12, "lines"), label.size = 0.3,
                    fill = alpha("white", 0.8), label.colour = "black") +
    scale_fill_manual(values = id_colours, guide = "none") +
    scale_shape_manual(name = "sex", values = c("female" = 21, "male" = 24), na.value = 23) +
    scale_size_continuous(name = "Age class", range = c(5, 11)) +
    labs(title = paste0(yr, " (", th_level, "% isopleth)")) +
    theme_void() +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 11),
      legend.position = "bottom",
      # box around each panel so the boundary between years is clear
      # (patchwork::wrap_plots() doesn't add this automatically the
      # way facet_wrap()'s panel.border would)
      panel.border     = element_rect(colour = "grey", fill = NA, linewidth = 0.8),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(6, 6, 6, 6)
    )
}

years <- sort(unique(overlap_data$year[overlap_data$th == th_level]))
network_plots <- lapply(years, make_network_plot)

# ------------------------------------------------------------
# 4. Combine into one figure, one panel per year
# ------------------------------------------------------------
overlap_network <- wrap_plots(network_plots, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#overlap_network <- overlap_network +
#  plot_annotation(
#    title = "Sheep Rut Home Range Overlap Groups ",
#    subtitle = paste0("Edges = ", th_level, "% rut homerang overlap (width/opacity = Jaccard index)  |  Node colour = individual sheep ")
#  )

overlap_network

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","rut_95_overlap_network.png"), overlap_network, width = 11, height = 9, dpi = 300)
ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","rut_50_overlap_network.png"), overlap_network, width = 11, height = 9, dpi = 300)



###############################################################################
###############################################################################




# repeat the plots for annual range with 50th? 

#allyrs <- summary_overlap(poly_annual_all, "sheep_byid_all_yr_overlap_pc.csv")
#summer <- summary_overlap(poly_summer, "sheep_byid_summer_yr_overlap_pc")
#winter <- summary_overlap(poly_winter, "sheep_byid_winter_yr_overlap_pc.csv")
#lamb <- summary_overlap(poly_lamb, "sheep_byid_lamb_yr_overlap_pc.csv")

overlap_data <- read.csv(fs::path("02_draft_outputs", "sheep_byid_all_yr_overlap_pc.csv")) |> 
  select( -X) |> 
  mutate(id_1 = as.character(id_1),
         id_2 = as.character(id_2))

sex_age_lookup <- id_key %>%
  distinct(tag_idn, sex, Age_annuli) %>%
  mutate(tag_idn = as.character(tag_idn))

th_level <- 50

all_ids <- overlap_data %>%
  filter(th == th_level) %>%
  { sort(unique(c(.$id_1, .$id_2))) }

id_colours <- as.vector(createPalette(
  length(all_ids),
  seedcolors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
  range = c(20, 80)
))
names(id_colours) <- all_ids

# ------------------------------------------------------------
# 3. Build one network plot per year
# ------------------------------------------------------------
make_network_plot <- function(yr) {
  
  data_yr <- overlap_data %>% filter(year == yr, th == th_level)
  
  nodes_yr <- data.frame(name = sort(unique(c(data_yr$id_1, data_yr$id_2)))) %>%
    left_join(sex_age_lookup, by = c("name" = "tag_idn"))
  
  edges_yr <- data_yr %>%
    filter(jaccard > 0) %>%
    select(id_1, id_2, jaccard, pct_overlap_1, pct_overlap_2, overlap_area_ha)
  
  missing_ids <- setdiff(unique(c(edges_yr$id_1, edges_yr$id_2)), nodes_yr$name)
  if (length(missing_ids) > 0) {
    stop("Year ", yr, ": edge IDs missing from vertex list: ",
         paste(missing_ids, collapse = ", "))
  }
  
  g <- graph_from_data_frame(edges_yr, directed = FALSE, vertices = nodes_yr)
  
  if (ecount(g) > 0) {
    comm <- cluster_louvain(g, weights = E(g)$jaccard)
    V(g)$group <- factor(membership(comm))
  } else {
    V(g)$group <- factor(1)
  }
  
  ggraph(g, layout = "fr") +   # force-directed layout: connected nodes pull together
    geom_edge_link(aes(width = jaccard, alpha = jaccard), colour = "grey40") +
    scale_edge_width(range = c(0.3, 3), guide = "none") +
    scale_edge_alpha(range = c(0.25, 0.9), guide = "none") +
    geom_node_point(aes(fill = name, shape = sex, size = Age_annuli), colour = "black", stroke = 0.6) +
    #geom_node_text(aes(label = name), size = 3, repel = TRUE, max.overlaps = 20) +
    geom_node_label(aes(label = name), size = 2.8,
                    repel = TRUE, max.overlaps = 30,
                    box.padding = 0.6, point.padding = 0.4,
                    label.padding = unit(0.12, "lines"), label.size = 0.3,
                    fill = alpha("white", 0.8), label.colour = "black") +
    scale_fill_manual(values = id_colours, guide = "none") +
    scale_shape_manual(name = "sex", values = c("female" = 21, "male" = 24), na.value = 23) +
    scale_size_continuous(name = "Age class", range = c(5, 11)) +
    labs(title = paste0(yr, " (", th_level, "% isopleth)")) +
    theme_void() +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 11),
      legend.position = "bottom",
      panel.border     = element_rect(colour = "grey", fill = NA, linewidth = 0.8),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(6, 6, 6, 6)
    )
}

years <- sort(unique(overlap_data$year[overlap_data$th == th_level]))
network_plots <- lapply(years, make_network_plot)


overlap_network <- wrap_plots(network_plots, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


overlap_network

ggsave(fs::path("02_draft_outputs", "06_report_summary_figures","annual_50_overlap_network.png"), overlap_network, width = 11, height = 9, dpi = 300)




