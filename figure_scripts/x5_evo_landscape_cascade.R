#' ---
#' output: html_document
#' editor_options:
#'   chunk_output_type: console
#' ---
#'
#' # Landscape effects
#'
## -----------------------------------------------------------------------------
# to handle data and plot
library(data.table)
library(glue)

library(ggplot2)
library(colorspace)
library(patchwork)

#'
#' ## Read node weight evolution
#'
## -----------------------------------------------------------------------------
# read back in
data <- fread("data_sim/results/data_early_0_100_weight_evolution.csv")
# data[, folder_path := stringr::str_replace(folder_path, "data", "data_sim")]

# select growth rates of 0.001, 0.01, 0.1, 0.25
focus_r <- c(0.01)
data <- data[regrowth %in% focus_r, ]

# handle weight value ranges and use UPPER bound
data[, weight_num :=
  stringi::stri_extract_last(weight_value,
    regex = "[-0-9]+\\.\\d{2}"
  )]
# assign numeric
data[, weight_num := as.numeric(weight_num)]

# remove random sim
data <- data[sim_type != "random", ]

#'
#' ## Prepare summary data
#'
#' Handler preference
#'
## -----------------------------------------------------------------------------
# get positive weights for handlers (weight 3)
wt_handler <- data[weight_id == 3 &
  weight_num > 0 &
  regrowth == 0.01 &
  sim_type != "random", ]

# sum proportions
wt_handler <- wt_handler[, list(pref_handlers = sum(weight_prop)),
  by = c("sim_type", "replicate", "regrowth", "gen")
]

#'
#' ### Merge weight strategy
#'
## -----------------------------------------------------------------------------
data_wt <- rbindlist(list(klept_bias, handler_strategy))

#'
#' ## Read p clueless
#'
## -----------------------------------------------------------------------------
# focus r
focus_r <- c(0.01)

# read clueless data
data <- fread("data_sim/results/data_p_clueless.csv")
data[, path := NULL]
data <- data[regrowth %in% focus_r, ]
data[, sim_type := ifelse(sim_type == "forager", "foragers", sim_type)]

# remove v1
data[, V1 := NULL]

# remove random
data <- data[sim_type != "random"]

# 1 - p_clueless
data$p_clueless <- 1 - data$p_clueless

# join with data_wt
data_strat <- merge(data, wt_handler)

# melt
data <- copy(data_strat)

data <- melt(data,
  id.vars = c(
    "gen", "replicate",
    "regrowth", "sim_type"
  )
)

# set factor order
data$sim_type <- factor(data$sim_type,
  levels = c("foragers", "obligate", "facultative")
)

# remove variables
data <- data[variable %in%
  c("p_clueless", "pref_handlers"), ]

# split data
data <- split(data, by = "sim_type")

#'
## -----------------------------------------------------------------------------
# this green
this_green <- "forestgreen"

# make subplots
subplots <- lapply(data, function(df) {
  if (unique(df$sim_type) == "foragers") {
    x_lim <- c(0, 100)
    df <- df[gen <= 100, ]
    df[variable == "klept_strategy", "value"] <- NA
  } else {
    x_lim <- c(0, 50)
    df <- df[gen <= 50, ]
  }

  ggplot(df) +
    geom_vline(
      xintercept = c(1, 10, 50),
      colour = "grey",
      lty = 2,
      size = 0.3
    )+
    geom_path(
      aes(gen, value,
        col = variable,
        group = interaction(replicate, variable)
      )
    ) +
    scale_colour_manual(
      values = c(
        p_clueless = this_green,
        pref_handlers = "lightslateblue"
      ),
      labels = c(
        p_clueless = "Higher prey density\nin neighbourhood",
        pref_handlers = "Tendency to move\ntowards handlers"
      ),
      breaks = c(
        "p_clueless", "pref_handlers"
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.25),
      labels = c("0", "0.25", "0.5", "0.75", "1")
    ) +
    coord_cartesian(
      xlim = x_lim,
      ylim = c(0, 1),
      expand = F
    ) +
    kleptomoveMS::theme_custom(grid = F, base_size = 8) +
    theme(legend.position = "top") +
    labs(
      x = "Generation",
      y = "Proportion",
      colour = NULL
    ) +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE))
})

# arrange order
subplots = subplots[c("foragers", "obligate", "facultative")]

#'
#' ## Show landscape for each sim type
#'
#' Here we show a landscape with and without clues at 1, 10, and 40th generation for $r_{max}$ = 0.01.
#'
#' ### Items landscape
#'
## -----------------------------------------------------------------------------
# list folders
paths <- list.dirs("data_sim/for_landscape/", recursive = F)

# select gens
gens_for_figure <- stringr::str_pad(c(1, 10, 50), width = 5, pad = "0")

# get paths for files
landscape_data <- CJ(
  gen = gens_for_figure,
  path = paths
)

# add sim_type
landscape_data[, sim_type := rep(c("facultative", "foragers", "obligate"), 3)]

# get paths
landscape_data$image <- glue_data(landscape_data, "{path}/{gen}.png")

# get data
landscape_data$data <- lapply(landscape_data$image,
  kleptomoveMS::read_landscape,
  layer = 4,
  crop_dim = 60,
  type = "items"
)

# convert gen to numeric
landscape_data[, gen := as.numeric(gen)]

# unlst
landscape_items <- landscape_data[, unlist(data, recursive = F),
  by = c("sim_type", "gen")
]

#'
#' ### Gradient landscape
#'
## -----------------------------------------------------------------------------
# get data
landscape_data$data <- lapply(landscape_data$image,
  kleptomoveMS::read_landscape,
  layer = 4,
  crop_dim = 60,
  type = "gradient"
)
# unlst
landscape_gradient <- landscape_data[, unlist(data, recursive = F),
  by = c("sim_type", "gen")
]

#'
#' ### Merge landscapes
#'
## -----------------------------------------------------------------------------
# landscape overall
landscape <- (landscape_gradient)

# melt
landscape <- melt(landscape, id.vars = c("sim_type", "gen", "x", "y"))

# split by sim_type
landscape <- split(landscape, by = c("sim_type", "variable"))

#'
#'
#' ### Plot landscape
#'
## -----------------------------------------------------------------------------
subplot_land <- lapply(landscape, function(df) {
  ggplot(df) +
    geom_tile(
      aes(x, y, 
          fill = value >= 0.7),
      show.legend = T
    ) +
    facet_grid(~gen, labeller = label_both) +
    scale_fill_manual(
      values = c(
        "TRUE" = this_green,
        "FALSE" = "white"
      ),
      labels = c(
        "TRUE" = "Higher prey density\nin neighbourhood",
        "FALSE" = "Neighbourhood has\nsame prey density"
      ),
      breaks = c("TRUE", "FALSE"),
      name = NULL
    ) +
    coord_equal(expand = F) +
    kleptomoveMS::theme_custom(landscape = T, base_size = 8) +
    theme(
      legend.position = "top",
      legend.key = element_rect(
        fill = NA,
        colour = "grey"
      ),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
})

# arrange order
sim_types <- c("foragers", "obligate", "facultative")

# figure names
fignames <- c(glue("{sim_types}.gradient"))
# arrange plots in order
subplot_land <- subplot_land[fignames]

#'
#' ## Figure 5
#'
## -----------------------------------------------------------------------------
# make figure 5
# wrap cues per gen plots
plots_cues = wrap_plots(subplots, ncol = 1) +
  plot_layout(guides= "collect", tag_level = "new") &
  theme(legend.position = "top")

# wrap landscape plots
plots_land = wrap_plots(subplot_land, ncol = 1) +
  plot_layout(guides= "collect", tag_level = "new") &
  theme(legend.position = "top")

# make figure 5
figure_5 = wrap_plots(plots_land, plots_cues, 
                      design = "AAAABB") &
  plot_annotation(tag_levels = c("A", "1")) &
  theme(
    plot.tag = element_text(
      face = "bold",
      size = 8
    )
  )

ggsave(
  figure_5,
  height = 150,
  width = 180,
  units = "mm",
  filename = "figures/fig_05.png"
)

#'
