# Gallup Analysis: make the manuscript-style Figure 1 specification curve
#
# The top panel orders all multiverse coefficients from smallest to largest.
# The lower panel shows which modeling choices produced each coefficient.

# Run with the working directory set to the repository folder.
project_dir <- getwd()

results_file <- file.path(project_dir, "results", "derived", "gallup_multiverse_results.csv")
output_dir <- file.path(project_dir, "results", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Prepare the coefficient curve and mark statistically significant estimates.
curve_data <- read.csv(results_file, stringsAsFactors = FALSE) %>%
  filter(!is.na(coef)) %>%
  mutate(
    significance = ifelse(
      pvalue < 0.05 & coef > 0,
      "Positive (p<.05)",
      "Not significant"
    )
  ) %>%
  arrange(coef) %>%
  mutate(specification_order = row_number())

median_coefficient <- median(curve_data$coef)
significant_count <- sum(curve_data$significance == "Positive (p<.05)")

# Top panel: coefficient estimates and 95% confidence intervals.
coefficient_panel <- ggplot(
  curve_data,
  aes(x = specification_order, y = coef, color = significance)
) +
  geom_linerange(
    aes(ymin = ci_lower, ymax = ci_upper),
    linewidth = 0.25,
    alpha = 0.45
  ) +
  geom_point(size = 1.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.45) +
  geom_hline(
    yintercept = median_coefficient,
    color = "gray60",
    linetype = "dashed",
    linewidth = 0.45
  ) +
  annotate(
    "text",
    x = max(curve_data$specification_order) * 0.98,
    y = median_coefficient + 0.45,
    label = paste0("Median = ", round(median_coefficient, 2)),
    hjust = 1,
    color = "gray60",
    size = 3.4
  ) +
  scale_color_manual(
    values = c(
      "Not significant" = "#999999",
      "Positive (p<.05)" = "#377EB8"
    ),
    breaks = c("Not significant", "Positive (p<.05)"),
    name = "Significance"
  ) +
  labs(
    x = NULL,
    y = "Coefficient Estimate\n(percentage points)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Bottom panel: show which modeling choices produced each coefficient.
specification_ticks <- curve_data %>%
  select(
    specification_order,
    significance,
    ownership_var,
    fixed_effects,
    include_lag,
    first_year_interaction,
    objective_controls,
    event_controls,
    political_controls
  ) %>%
  mutate(
    ownership_var = recode(
      ownership_var,
      owner_is_president = "President only",
      owner_has_unified = "Unified govt"
    ),
    fixed_effects = recode(
      fixed_effects,
      year_only = "Year FE only",
      topic = "Year + Topic FE",
      topic_trend = "Year + Topic + Trends"
    ),
    include_lag = recode(
      as.character(include_lag),
      `0` = "No lag",
      `1` = "With lag"
    ),
    first_year_interaction = recode(
      as.character(first_year_interaction),
      `0` = "No interaction",
      `1` = "First year int."
    ),
    objective_controls = recode(
      objective_controls,
      none = "None",
      all = "All",
      strong_only = "Strong only"
    ),
    event_controls = recode(
      as.character(event_controls),
      `0` = "None",
      `1` = "With events"
    ),
    political_controls = recode(
      political_controls,
      none = "None",
      both = "With political"
    )
  ) %>%
  pivot_longer(
    cols = c(
      ownership_var,
      fixed_effects,
      include_lag,
      first_year_interaction,
      objective_controls,
      event_controls,
      political_controls
    ),
    names_to = "dimension",
    values_to = "choice"
  ) %>%
  mutate(
    dimension = factor(
      dimension,
      levels = c(
        "ownership_var",
        "fixed_effects",
        "include_lag",
        "first_year_interaction",
        "objective_controls",
        "event_controls",
        "political_controls"
      ),
      labels = c(
        "Ownership",
        "Fixed Effects",
        "Lag DV",
        "First Year Int.",
        "Objective Controls",
        "Event Controls",
        "Political Controls"
      )
    )
  )

tick_panel <- ggplot(
  specification_ticks,
  aes(x = specification_order, y = choice, color = significance)
) +
  geom_point(size = 0.5, shape = 15) +
  scale_color_manual(
    values = c(
      "Not significant" = "#999999",
      "Positive (p<.05)" = "#377EB8"
    ),
    guide = "none"
  ) +
  facet_grid(
    rows = vars(dimension),
    scales = "free_y",
    space = "free_y"
  ) +
  labs(
    x = "Specifications (ordered by coefficient estimate)",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", angle = 0, hjust = 0),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.grid = element_blank(),
    panel.spacing = grid::unit(0.2, "lines")
  )

figure_1 <- coefficient_panel / tick_panel +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(
    title = "Figure 1. Specification Curve: Issue Ownership Effects on Public Perceptions",
    subtitle = paste0(
      "N = ", nrow(curve_data),
      " specifications | Median coefficient = ",
      round(median_coefficient, 2),
      " pp | ", significant_count,
      " significant at p<.05 (",
      round(100 * significant_count / nrow(curve_data), 1),
      "%)"
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5)
    )
  )

ggsave(
  file.path(output_dir, "figure_1_gallup_specification_curve.png"),
  figure_1,
  width = 8.5,
  height = 11,
  dpi = 600
)

ggsave(
  file.path(output_dir, "figure_1_gallup_specification_curve.pdf"),
  figure_1,
  width = 8.5,
  height = 11
)

cat("Valid specifications:", nrow(curve_data), "\n")
cat("Median coefficient:", round(median_coefficient, 3), "\n")
cat("Positive and significant:", significant_count, "\n")
