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
# process scenario 2 (obligate) separately as for figure 3
wt_handler <- data[weight_id == 3 &
  weight_num > 0 &
  regrowth == 0.01 &
  !sim_type %in% c("random", "obligate"), ]

# sum proportions
wt_handler <- wt_handler[, list(pref_handlers = sum(weight_prop)),
  by = c("sim_type", "replicate", "regrowth", "gen")
]

# set strategy descriptor
wt_handler[, strategy := "consumer"]

# HANDLE SCENARIO 2
wt_handler_s2 = fread("data_sim/results/data_syndrome.csv")
wt_handler_s2 = wt_handler_s2[regrowth == 0.01, list(N = sum(N)),
                              by = c("sim_type", "replicate", "regrowth", 
                                     "klept_bias", "handler_pref", "gen")
                              ]
wt_handler_s2[, c("klept_bias", "handler_pref") := list(
  factor(ifelse(klept_bias, "forager", "klept")),
  factor(ifelse(handler_pref, "prefers handlers", "avoids handlers"),
         levels = c("prefers handlers", "avoids handlers")
  )
)]
wt_handler_s2[, prop_per_strat := N / sum(N),
              by = c("gen", "klept_bias", "regrowth", "replicate")
              ]
# subset data
wt_handler_s2 = wt_handler_s2[handler_pref == "prefers handlers",]
wt_handler_s2[, c("handler_pref", "N") := NULL]
setnames(wt_handler_s2, old = c("klept_bias", "prop_per_strat"),
         new = c("strategy", "pref_handlers"))

#' ### Merge data
wt_handler = rbindlist(list(wt_handler, wt_handler_s2), use.names = TRUE)
wt_handler$gen = as.numeric(wt_handler$gen)

# make factor
wt_handler$strategy = factor(wt_handler$strategy)

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

# set factor order
data$sim_type <- factor(data$sim_type,
  levels = c("foragers", "obligate", "facultative")
)

# split data
data <- split(data, by = "sim_type")
# set order
data = data[c("foragers", "obligate", "facultative")]

# split preference data
wt_handler = split(wt_handler, by = "sim_type")
wt_handler = wt_handler[c("foragers", "obligate", "facultative")]

#'
## -----------------------------------------------------------------------------
# this green
this_green <- "forestgreen"

# make subplots
subplots <- Map(function(df_wt, df_land) {
  if (unique(df_wt$sim_type) == "foragers") {
    x_lim <- c(1, 100)
    df_wt <- df_wt[gen <= 100, ]
    # df[variable == "klept_strategy", "value"] <- NA
  } else {
    x_lim <- c(1, 50)
    df_wt <- df_wt[gen <= 50, ]
  }

  ggplot() +
    geom_vline(
      xintercept = c(1, 10, 50),
      colour = "grey",
      lty = 2,
      size = 0.3
    )+
    geom_path(
      data = df_land,
      aes(
        gen, p_clueless,
        colour = "land",
        group = replicate
      )
    )+
    geom_path(
      data = df_wt,
      aes(gen, pref_handlers,
        col = strategy,
        group = interaction(replicate, strategy)
      )
    ) +
    scale_colour_manual(
      values = c(
        land = this_green,
        consumer = "dodgerblue4",
        klept = "indianred",
        forager = "dodgerblue4"
      ),
      labels = c(
        land = "Higher prey density\nin neighbourhood",
        consumer = "Tendency to move\ntowards handlers",
        klept = "Klept. tendency\nto move\ntowards handlers",
        forager = "Forager tendency\nto move\ntowards handlers"
      ),
      breaks = c(
        "land", "consumer", "forager", "klept"
      )
    ) +
    scale_y_continuous(
      limits = c(0, 1.05),
      breaks = seq(0, 1, 0.25)
    ) +
    coord_cartesian(
      xlim = x_lim,
      ylim = c(0.2, 1.05),
      expand = F
    ) +
    kleptomoveMS::theme_custom(grid = F, base_size = 8) +
    theme(
      legend.position = "right"
      ) +
    labs(
      x = "Generation",
      y = "Proportion",
      colour = NULL,
      shape = NULL
    ) +
    guides(colour = guide_legend(nrow = 3, ncol = 1))
}, wt_handler, data)

# arrange order
subplots = subplots[c("foragers", "obligate", "facultative")]
wrap_plots(subplots, ncol = 1) &
  theme(legend.position = "right")
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
landscape <- split(landscape, by = c("sim_type"))

landscape = landscape[c("foragers", "obligate", "facultative")]

#'
#'
#' ### Plot landscape
#'
## -----------------------------------------------------------------------------
subplot_land <- Map(function(df, n) {
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
      legend.key.height = unit(1, units = "mm"),
      title = element_text(face = "bold"),
      axis.text = element_blank(),
      axis.title = element_blank()
    )+
    labs(
      title = glue("Scenario {n}")
    )+
    guides(colour = guide_legend(nrow = 2, ncol = 1))
}, landscape, seq(3))

#'
#' ## Figure 5
#'
## -----------------------------------------------------------------------------
# make figure 5
# wrap cues per gen plots
plots_cues = wrap_plots(subplots, ncol = 1) +
  plot_layout(tag_level = "new") &
  theme(legend.position = "right")

# wrap landscape plots
plots_land =
  wrap_plots(subplot_land, ncol = 1) +
  plot_layout(guides= "collect", tag_level = "new") &
  theme(
    legend.key.width = unit(3, units = "mm"),
    legend.position = "top"
  )

# make figure 5
figure_5 =
  wrap_plots(plots_land, plots_cues, 
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
