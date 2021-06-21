#' ---
#' output: html_document
#' editor_options:
#'   chunk_output_type: console
#' ---
#'
#' load libs
#'
## -----------------------------------------------------------------------------
library(data.table)

library(ggplot2)
library(patchwork)
library(colorspace)

#'
#' ## get data
#'
## -----------------------------------------------------------------------------
# activity data
data_activity <- fread("data_sim/results/data_strategy_gen.csv")
data_activity <- data_activity[
  sim_type == "foragers" &
    regrowth == 0.01 &
    variable != "stealing"
]

#'
#' ## activity budget plot
#'
## -----------------------------------------------------------------------------
fig_activity <-
  ggplot(data_activity[gen <= 50, ]) +
  
  geom_path(
    aes(
      gen, value,
      colour = variable,
      group = interaction(variable, replicate)
    )
  ) +
  scale_colour_manual(
    values = c(
      foraging = "dodgerblue4",
      handling = "forestgreen"
    ),
    labels = c(
      foraging = "Foraging", 
      handling = "Handling"
      ),
    breaks = c("foraging", "handling")
  ) +
  scale_y_continuous(
    # labels = scales::percent,
    breaks = seq(0, 1, 0.25)
  ) +
  scale_x_log10(
    breaks = c(1, 3, 10, 30, 50)
  ) +
  coord_cartesian(
    xlim = c(1, 50),
    ylim = c(0, 1),
    expand = F
  ) +
  theme_classic(base_size = 8) +
  theme(legend.position = "top",
        legend.key.height = unit(3, units = "mm"),
        legend.key.width = unit(2, units = "mm")
  ) +
  labs(
    x = "Generation",
    y = "Proportion of time",
    colour = NULL
  )+
  guides(colour = guide_legend(nrow = 2, ncol = 2, byrow = T))

#'
#' ## figure intake
#'
## -----------------------------------------------------------------------------
fig_intake <-
  ggplot(
    unique(data_activity[gen <= 50, ],
      by = c("gen", "replicate", "pop_fitness")
    )
  ) +
  
  geom_path(
    aes(gen, pop_fitness,
      group = replicate
    ),
  ) +
  scale_x_log10(
    breaks = c(1, 3, 10, 30, 50)
  ) +
  coord_cartesian(
    xlim = c(1, 50),
    ylim = c(0, 50),
    expand = F
  ) +
  theme_classic(base_size = 8) +
  labs(
    x = "Generation",
    y = "Population intake"
  )

#'
#' ## correlation with quality
#'
## -----------------------------------------------------------------------------
# raw correlation data
data_quality <- fread("data_sim/results/data_quality_matching_rule.csv")
data_quality <- data_quality[sim_type == "foragers" & regrowth == 0.01, ]

#'
## -----------------------------------------------------------------------------
fig_matching_quality <-
  ggplot() +
  geom_hline(
    yintercept = 0,
    col = "red"
  ) +
  geom_point(
    data = data_quality[gen < 50, ],
    aes(
      x = gen,
      y = cf
    ),
    colour = "dodgerblue4",
    shape = 1,
    stroke = 0.5,
    show.legend = F
  ) +
  theme_classic(base_size = 8) +
  coord_cartesian(
    ylim = c(-0.5, 0.5),
    xlim = c(0, 50),
    expand = F
  ) +
  xlim(0, 50) +
  labs(
    x = "Generation",
    y = "Corr. # indivs. ~ cell quality"
  )

#'
#' prepare landscape at 0 and 25
#'
## -----------------------------------------------------------------------------
# get landscape data
data_land <- fread("data_sim/results/data_landscape_item_count_1_50.csv")
data_land <- data_land[sim_type == "foragers" & regrowth == 0.01, ]

# read agent data
data_agent <- fread("data_sim/results/data_agent_count_1_50.csv")[
  sim_type == "foragers" & regrowth == 0.01,
]

#'
#' plot landscape foragers model
#'
## -----------------------------------------------------------------------------
fig_land_foragers <-
  ggplot(data_land) +
  geom_tile(aes(x, y, fill = items)) +
  geom_point(
    data = data_agent[agents > 0, ],
    aes(x, y, colour = agents),
    shape = 4,
    size = 0.5,
    stroke = 1,
    alpha = 0.8
  ) +
  facet_grid(~gen,
    labeller = label_both
  ) +
  scale_fill_continuous_sequential(
    palette = "Blues2",
    begin = 0.1,
    limits = c(1, NA),
    na.value = "white",
    name = "# Prey",
    guide = guide_legend(order = 1)
  )+
  scale_colour_continuous_sequential(
    palette = "Reds",
    begin = 0.2,
    na.value = "darkred",
    limits = c(1, 5),
    name = "# Consumers",
    guide = guide_legend(order = 2)
  )+
  coord_equal(expand = F) +
  kleptomoveMS::theme_custom(landscape = T, base_size = 6) +
  theme(legend.position = "bottom")
  

#'
#' ## Figure 1 Foragers model
#'
#' wrap figures together
#'
## -----------------------------------------------------------------------------
figure_1_forager_model <-
  wrap_plots(
    fig_land_foragers,
    fig_activity, fig_intake,
    fig_matching_quality,
    design = "AAAAAA\nBBCCDD"
  ) +
    plot_annotation(
      tag_levels = "A"
    ) &
    theme(plot.tag = element_text(
      face = "bold",
      size = 12
    ))

#'
#' save figure
#'
## -----------------------------------------------------------------------------
ggsave(figure_1_forager_model,
  filename = "figures/fig_01.png",
  height = 120, width = 150, units = "mm"
)

