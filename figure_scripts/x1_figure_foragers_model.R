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
    ),
    position = position_jitter()
  ) +
  scale_colour_manual(
    values = c(
      foraging = "darkblue",
      handling = "seagreen"
    ),
    labels = c(foraging = "Searching", handling = "Handling"),
    breaks = c("foraging", "handling")
  ) +
  scale_y_continuous(
    # labels = scales::percent,
    breaks = seq(0, 1, 0.25),
    labels = c("0", "0.25", "0.5", "0.75", "1")
  ) +
  scale_x_continuous(
    breaks = c(1, seq(10, 50, 10))
  ) +
  coord_cartesian(
    xlim = c(1, 50),
    ylim = c(0, 1),
    expand = F
  ) +
  theme_classic(base_size = 8) +
  theme(legend.position = "top") +
  labs(
    x = "Generation",
    y = "Prop. population time",
    colour = NULL
  )

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
    position = position_jitter()
  ) +
  scale_x_continuous(
    breaks = c(1, seq(10, 50, 10))
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
    shape = 1,
    colour = "steelblue",
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
    y = "Corr. indivs. ~ cell quality"
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
    shape = 19,
    alpha = 0.6
  ) +
  facet_grid(~gen,
    labeller = label_both
  ) +
  scale_colour_viridis_c(
    option = "C", begin = 0.2,
    direction = 1,
    name = "# Consumers"
  )+
  scale_fill_viridis_c(
    option = "G",
    limits = c(1, NA),
    na.value = "white",
    direction = -1
  ) +
  coord_equal(expand = F) +
  kleptomoveMS::theme_custom(landscape = T, base_size = 6) +
  theme(legend.position = "bottom") +
  labs(fill = "# Items", size = "# Indiv.")

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
