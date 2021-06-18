#' ---
#' output: html_document
#' editor_options:
#'   chunk_output_type: console
#' ---
#'
## -----------------------------------------------------------------------------
library(data.table)
# plotting
library(ggplot2)
library(patchwork)
library(colorspace)

#'
## -----------------------------------------------------------------------------
# read in data
data <- fread("data_sim/results/data_strategy_gen.csv")

# remove random data
data = data[sim_type != "random",]

# rearrange
data$sim_type <- factor(data$sim_type,
  levels = c("foragers", "obligate", "facultative")
)

# remove excess growth
data <- data[regrowth <= 0.1, ]

#'
#' ### Fitness in relation to regrowth and scenario
#'
## -----------------------------------------------------------------------------
# get last 10 generations
data_equi <- data[gen > max(gen) - 10 & regrowth <= 0.05, ]

# look at handling and fitness
cols_we_want <- setdiff(
  colnames(data_equi),
  c("variable", "value")
)
data_fitness <- data_equi[, ..cols_we_want]

# get unique
data_fitness <- unique(data_fitness)

# boxplot handling time
data_fitness_summary <-
  data_fitness[, unlist(lapply(.SD, function(x) {
    list(
      median = median(x),
      sd = sd(x)
    )
  }), recursive = F),
  .SDcols = c("pop_fitness"),
  by = c("sim_type", "regrowth")
  ]

# make a manual colorscale
manual_colours = colorspace::qualitative_hcl(n = 3, palette = "Harmonic")
names(manual_colours) = levels(data_fitness_summary$sim_type)

# boxplot fitness
fig_strategy_fitness <-
  ggplot() +
  geom_errorbar(
    data = data_fitness_summary,
    aes(regrowth,
      ymin = pop_fitness.median -
        pop_fitness.sd,
      ymax = pop_fitness.median +
        pop_fitness.sd,
      group = sim_type,
      colour = sim_type
    )
  ) +
  geom_line(
    data = data_fitness_summary,
    aes((regrowth),
      y = pop_fitness.median,
      col = sim_type,
      group = sim_type
    ),
    size = 1
  ) +
  geom_point(
    data = data_fitness_summary,
    aes((regrowth),
      y = pop_fitness.median,
      fill = sim_type,
      group = sim_type
    ),
    shape = 21,
    size = 1
  ) +
  scale_fill_manual(
    values = manual_colours,
    labels = c(
      foragers = "Scenario 1",
      obligate = "Scenario 2",
      facultative = "Scenario 3"
    )
  ) +
  scale_colour_manual(
    values = manual_colours,
    labels = c(
      foragers = "Scenario 1",
      obligate = "Scenario 2",
      facultative = "Scenario 3"
    )
  ) +
  scale_x_continuous(breaks = c(
    0.001,
    0.01,
    0.025,
    0.05
  )) +
  scale_y_continuous(
    breaks = seq(0, 60, 20)
  ) +
  coord_cartesian(
    ylim = c(0, 60),
    xlim = c(0, 0.051),
    expand = F
  ) +
  kleptomoveMS::theme_custom(grid = F, base_size = 8) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Regrowth rate",
    y = "Population intake",
    colour = "Model",
    fill = "Model",
    shape = "Model"
  )

#'
#' ### Foraging and stealing across regrowth and scenario
#'
## -----------------------------------------------------------------------------
# get the stealing and foraging over regrowth rates
data_strategy_summary <- data_equi[variable %in% c(
  "stealing",
  "foraging"
) &
  regrowth <= 0.05, ]

# set sim type as factor
data_strategy_summary$sim_type = factor(data_strategy_summary$sim_type)

# remove stealing in foragers
data_strategy_summary = data_strategy_summary[
  !(variable == "stealing" & sim_type == "foragers"),
]

# summarise proportion per sim type and growth rate
data_strategy_summary <-
  data_strategy_summary[, unlist(lapply(.SD, function(x) {
    list(
      median = median(x),
      sd = sd(x)
    )
  }), recursive = F),
  .SDcols = c("value"),
  by = c("sim_type", "regrowth", "variable")
  ]

# split by variable
data_strategy_summary <- split(data_strategy_summary,
  by = "variable"
)

#'
## -----------------------------------------------------------------------------

subfigures_strategy_growth <- Map(function(df, name) {
  yaxis_name <- sprintf("Prop. %s", stringr::str_to_sentence(name))
  # plot figure
  ggplot(df) +
    geom_errorbar(
      aes(regrowth,
        ymin = value.median - value.sd,
        ymax = value.median + value.sd,
        colour = sim_type
      ),
      show.legend = F
    ) +
    geom_path(
      aes(regrowth, value.median,
        group = interaction(sim_type, variable),
        colour = sim_type
      ),
      size = 1,
      show.legend = F
    ) +
    geom_point(
      aes(regrowth, value.median,
        group = interaction(sim_type, variable),
        fill = sim_type
      ),
      show.legend = F,
      size = 1, 
      shape = 21) +
    scale_fill_manual(
      values = manual_colours,
      labels = c(
        foragers = "Scenario 1",
        obligate = "Scenario 2",
        facultative = "Scenario 3"
      )
    ) +
    scale_colour_manual(
      values = manual_colours,
      labels = c(
        foragers = "Scenario 1",
        obligate = "Scenario 2",
        facultative = "Scenario 3"
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.25)
    ) +
    scale_x_continuous(breaks = c(
      0.001,
      0.01,
      0.025,
      0.05
    )) +
    coord_cartesian(
      ylim = c(0, 1),
      expand = F
    ) +
    kleptomoveMS::theme_custom(base_size = 8) +
    theme(legend.position = "bottom") +
    labs(
      x = "Regrowth rate",
      y = yaxis_name,
      colour = "Model",
      fill = "Model",
      shape = "Model"
    )
}, data_strategy_summary, names(data_strategy_summary))

#'
#' ## Make Figure model comparison
#'
## -----------------------------------------------------------------------------
# point plots
point_plots <- append(
  subfigures_strategy_growth,
  list(fig_strategy_fitness)
)

#'
## -----------------------------------------------------------------------------
# wrap all figures
figure_06 <-
  wrap_plots(point_plots) +
    plot_annotation(
      tag_levels = "A"
    ) +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      plot.tag = element_text(
        face = "bold",
        size = 12
      )
    )

ggsave(
  figure_06,
  filename = "figures/fig_06.png",
  height = 75, width = 160, units = "mm"
)

#'
