#### code to make some plots ####

library(tidyr)
library(stringi)
library(glue)
library(ggplot2)

# some helper functions
source("code/helper_functions.R")

# read in the capacity
capacity <- png::readPNG("data/data_parameters/kernels32.png")[,,1]

max_capacity <- 5L

capacity <- round(capacity * max_capacity, digits = 1)

# simulation type
sim_type <- c("obligate", "facultative", "foragers", "random")

# get the replicates
replicate <- stringr::str_pad(seq_len(2), pad = "0", width = 3)

# growth rates
regrowth <- c(0.001, 0.01, 0.03, 0.05)

# glue data folder names together
data <- crossing(
  sim_type,
  replicate,
  regrowth
)

# get folders
data$folder_path <- glue_data(.x = data,
                              'data/sim_{sim_type}_rep_{replicate}_\\
                                      gro_{regrowth}')

# get data
data$data_summary <- lapply(data$folder_path,
                            get_sim_summary, capacity_matrix = capacity)

# unnest the data
data <- unnest(data,
               cols = "data_summary")

# relevel the layer names for better plotting
data$layer <- forcats::fct_relevel(data$layer,
                                   c("klepts", "foragers", "items",
                                     "klepts_intake", "foragers_intake",
                                     "pc_intake_klepts", 
                                     "pc_intake_forager"))
# relevel the simulation (strategy) type
data$sim_type <- forcats::fct_relevel(data$sim_type, 
                                      "obligate", "facultative", "foragers", "random")

# regroup data
data$layer_type <- dplyr::case_when(
  stringi::stri_detect(data$layer, fixed = "pc_intake") ~ "per_capita_intake",
  stringi::stri_detect(data$layer, fixed = "item") ~ "items",
  stringi::stri_detect(data$layer, fixed = "intake") ~ "intake",
  TRUE ~ "strategy count"
)

# choose layer colours
layer_cols <- tibble(
  layer = c("klepts", "foragers", "items",
            "klepts_intake", "foragers_intake",
            "pc_intake_klepts", 
            "pc_intake_forager"),
  colour = c("indianred", "royalblue", "forestgreen",
             "indianred1", "royalblue1",
             "indianred2", "royalblue2")
)

# merge to data
data_plot <- dplyr::left_join(data,
                              layer_cols)

# split the data by growth rate
data_plot <- split(data_plot, data$layer_type)

#### overall figure ####

plot_list <- lapply(data_plot, function(df) {
  df <- dplyr::filter(df,
                      cap %in% seq(0, 5, 0.2))
  ggplot(df)+
    
    geom_hline(yintercept = 0, col = "grey", lwd = 0.2)+
    geom_vline(xintercept = 0, col = "grey", lwd = 0.2)+
    geom_errorbar(aes(cap,
                    ymin = mean_val - sd_val,
                    ymax = mean_val + sd_val,
                    group = interaction(layer, replicate, regrowth)),
                alpha = 0.5,
                show.legend = F,
                position = position_dodge(width = 0.2),
                col = df$colour)+
    geom_line(aes(cap, mean_val,
                   group = interaction(layer, replicate, regrowth)),
               position = position_dodge(width = 0.2),
               col = df$colour,
              lwd = 0.2)+
    geom_point(aes(cap, mean_val,
                   shape = layer,
                  group = interaction(layer, replicate, regrowth)),
               position = position_dodge(width = 0.2),
               fill = df$colour, 
               colour = "white",
               show.legend = F)+
    facet_grid(regrowth ~ sim_type, as.table = F,
               scales = "free_y",
               labeller = label_both)+
    scale_shape_manual(values = c(21, 24))+
    coord_cartesian(ylim = c(0, NA))+
    theme_test()+
    theme(legend.position = "top",
          axis.text.y = element_text(size = 6),
          axis.title.y = element_blank())+
    labs(x = "grid cell quality",
         y = "value",
         colour = "value",
         title = sprintf("%s", 
                         unique(df$layer_type)))
})

plot_dist <- patchwork::wrap_plots(plot_list[c("strategy count",
                                               "intake", 
                                   "items", "per_capita_intake")])

ggsave(plot_dist,
       filename = "figures/fig_agent_item_distribution.png",
       dpi = 300, height = 10, width = 12)
