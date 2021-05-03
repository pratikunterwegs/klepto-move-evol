#' ---
#' output: html_document
#' editor_options:
#'   chunk_output_type: console
#' ---
#'
#' # Behavioural syndrome
#'
## -----------------------------------------------------------------------------
# to handle data and plot
library(data.table)
library(glue)

library(ggplot2)
library(colorspace)
library(patchwork)

#'
#' ## show node weight
#'
## -----------------------------------------------------------------------------
# get raw data
data_nodes <- fread("data_sim/results/data_early_0_100_weight_evolution.csv")
# subset
data_nodes <- data_nodes[sim_type == "obligate" &
  regrowth == 0.01 &
  weight_id %in% c(3, 5), ]
# get numeric lower bounds
data_nodes[, weight_num := as.numeric(
  stringi::stri_extract_last(weight_value,
    regex = "[-0-9]+\\.\\d{2}"
  )
)]

#'
## -----------------------------------------------------------------------------
fig3a <-
  ggplot(data_nodes[weight_prop > 0.001 &
    weight_id == 5 &
    gen %% 3 == 0, ]) +
  geom_tile(
    aes(
      x = gen,
      y = (weight_num),
      fill = weight_prop
    )
  ) +
  geom_hline(
    yintercept = 0,
    col = "grey20",
    lty = 3,
    size = 0.3
  ) +
  scale_fill_continuous_sequential(
    palette = "Light Grays"
  ) +
  scale_y_continuous(
    breaks = c(-1.75, 1.75),
    labels = c("Klept.", "Forager")
  ) +
  coord_cartesian(
    expand = F
  ) +
  kleptomoveMS::theme_custom(base_size = 8) +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    strip.text = element_text(face = "italic", hjust = 0),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    legend.key.height = unit(1, units = "mm")
  ) +
  facet_grid(~replicate, labeller = label_both) +
  labs(
    x = "Generation", y = "Behavioural strategy",
    fill = "Prop. indivs."
  )

#'
#' ## Prepare strategy-wise weight data WIP
#'
## -----------------------------------------------------------------------------
data_strategy_weight <- fread("data_sim/results/data_syndrome_by_strategy.csv")
data_strategy_weight <- split(data_strategy_weight, by = "klept_bias")

#'
#'
## -----------------------------------------------------------------------------
subplots_syndrome <- Map(function(df, val) {
  colours <- sequential_hcl(n = 10, palette = "Reds 3", rev = T)
  fill_lab <- "Prop. klepts."
  if (val == "1") {
    fill_lab <- "Prop. foragers"
    colours <- sequential_hcl(n = 10, palette = "Blues 3", rev = T)
  }

  ggplot(df[prop_handler_pref > 0.01, ]) +
    geom_tile(
      aes(
        x = gen, y = handler_pref,
        fill = prop_handler_pref
      )
    ) +
    geom_hline(
      yintercept = 0,
      col = "grey20",
      lty = 2,
      size = 0.3
    ) +
    scale_fill_gradientn(
      colours = colours,
      labels = scales::percent
    ) +
    scale_y_continuous(
      breaks = c(-1, 0, 1),
      labels = c("Avoid", "Neutral", "Prefer")
    ) +
    coord_cartesian(
      ylim = c(-1, 1),
      expand = F
    ) +
    kleptomoveMS::theme_custom(base_size = 8) +
    theme(
      legend.key.height = unit(1, units = "mm"),
      legend.position = "top"
    ) +
    facet_wrap(~replicate,
      labeller = label_both,
      scales = "free"
    ) +
    labs(
      x = "Generation",
      y = "Handler response",
      fill = fill_lab
    )
}, data_strategy_weight, names(data_strategy_weight))

# wrap subplots by strat
fig3b <- wrap_plots(
  subplots_syndrome,
  ncol = 1
)

#'
## -----------------------------------------------------------------------------
# get data
data <- fread("data_sim/results/data_syndrome.csv")

# recode variables
data[, c(
  "klept_bias", "handler_pref",
  "item_pref", "nh_pref"
) := list(
  factor(ifelse(klept_bias, "forager", "klept")),
  factor(ifelse(handler_pref, "prefers handlers", "avoids handlers"),
    levels = c("prefers handlers", "avoids handlers")
  ),
  factor(ifelse(item_pref, "prefers items", "avoids items")),
  factor(ifelse(nh_pref, "prefers nh", "avoids nh"))
)]

#'
## -----------------------------------------------------------------------------
# counter by klept and handler
data <- data[, list(N = sum(N)),
  by = c("sim_type", "replicate", "regrowth", "klept_bias", "handler_pref", "gen")
]

# get proportion
data[, prop_per_strat := N / sum(N),
  by = c("gen", "klept_bias", "regrowth", "replicate")
]

#'
## -----------------------------------------------------------------------------
fig3c <-
  ggplot(data[regrowth == 0.01 &
    handler_pref == "prefers handlers", ]) +
  geom_hline(
    yintercept = 0.5,
    col = "grey20",
    lty = 2,
    size = 0.3
  ) +
  geom_path(
    aes(
      x = gen,
      y = prop_per_strat,
      col = interaction(klept_bias, handler_pref),
      # size = handler_pref,
      group = interaction(sim_type, klept_bias, handler_pref, replicate)
    )
  ) +
  scale_colour_manual(
    values = c(
      "forager.prefers handlers" = "royalblue",
      "forager.avoids handlers" = "lightblue",
      "klept.prefers handlers" = "darkred",
      "klept.avoids handlers" = "pink"
    ),
    name = NULL,
    labels = c(
      "forager.prefers handlers" = "Forager, prefers handlers",
      "forager.avoids handlers" = "Forager, avoids handlers",
      "klept.prefers handlers" = "Klept., prefers handlers",
      "klept.avoids handlers" = "Klept., avoids handlers"
    )
  ) +
  scale_y_continuous(
    breaks = c(0.5, 0.75, 1)
  ) +
  coord_cartesian(
    ylim = c(0.33, 1),
    xlim = c(1, 50),
    expand = F
  ) +
  labs(
    x = "Generation",
    y = "Prop. handler preference"
  ) +
  scale_x_log10() +
  annotation_logticks(
    sides = "b",
    colour = "grey",
    size = 0.2
  ) +
  kleptomoveMS::theme_custom(grid = F, base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(1, units = "mm")
  ) +
  guides(colour = guide_legend(nrow = 4, byrow = TRUE))

#'
## -----------------------------------------------------------------------------
fig_3 <-
  wrap_plots(
    fig3a, fig3b, fig3c,
    design = "AAA\nBBC\nBBC\nBBC\nBB#"
  ) +
    plot_annotation(
      tag_levels = "A"
    ) &
    theme(
      plot.tag = element_text(
        face = "bold",
        size = 12
      )
    )

#'
#'
## -----------------------------------------------------------------------------
ggsave(fig_3,
  filename = "figures/fig_03.png",
  width = 125,
  height = 150, units = "mm"
)

#'
