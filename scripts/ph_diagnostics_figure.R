# ph_diagnostics_figure.R — what the PH remediation did, and what it cost.
#
# The diagnostics artifacts carry four models and two very different kinds of
# number. Drawing them together makes one thing visible that a table hides: the
# time-varying fit has NO post-remediation PH p, not because it failed but
# because survival::cox.zph is undefined for models containing tt() terms. An
# absent bar for a reason is different from an absent bar for a failure, and the
# figure says which.
#
# Usage: Rscript scripts/ph_diagnostics_figure.R

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(ggplot2); library(tidyr)
})

cmp <- read_csv(here("output", "cox_ph_model_comparison.csv"),
                show_col_types = FALSE, progress = FALSE)
ph  <- read_csv(here("output", "cox_ph_assumption.csv"),
                show_col_types = FALSE, progress = FALSE)
alpha <- 0.05

label <- c(original = "Original\n(as fitted)",
           production = "Production\n(each violator remediated)",
           violators_dropped = "Violators dropped\n(diagnostic only)",
           time_varying = "Time-varying\n(sensitivity)")

ph_p <- c(original = ph$p_value[1],
          production = ph$remediated_global_p[1],
          violators_dropped = ph$violators_dropped_global_p[1],
          time_varying = NA_real_)

d <- cmp |>
  mutate(model_label = factor(unname(label[model]), levels = unname(label)),
         ph_global_p = unname(ph_p[model]),
         is_production = model == "production")

pal <- c(`TRUE` = "#1B3A4B", `FALSE` = "#B8654F")

both <- bind_rows(
  d |> transmute(model_label, panel = "Global Schoenfeld p", value = ph_global_p, is_production),
  d |> transmute(model_label, panel = "Delta AIC vs original", value = delta_aic_vs_original, is_production))
both$panel <- factor(both$panel, levels = c("Global Schoenfeld p", "Delta AIC vs original"))

pp <- ggplot(both, aes(model_label, value, fill = is_production)) +
  geom_col(width = .66, na.rm = TRUE) +
  geom_hline(data = data.frame(panel = factor("Global Schoenfeld p", levels = levels(both$panel)),
                               y = alpha),
             aes(yintercept = y), linetype = "22", colour = "#8A5A44") +
  # Labels sit OUTSIDE the bar in whichever direction it points; a -617 label
  # drawn with a fixed vjust lands inside the bar and disappears.
  geom_text(aes(label = ifelse(is.na(value), "",
                               ifelse(panel == "Global Schoenfeld p",
                                      sprintf("%.3f", value), sprintf("%+.1f", value))),
                vjust = ifelse(!is.na(value) & value < 0, 1.5, -0.5)),
            size = 3, colour = "#33393D", na.rm = TRUE) +
  # An absent bar needs to say why it is absent, on the plot and not only in
  # the subtitle, or it reads as a model that failed.
  geom_text(data = both[is.na(both$value), ],
            aes(y = 0, label = "not defined:\ncox.zph rejects tt() terms"),
            vjust = -0.25, size = 2.8, colour = "#7A8287", lineheight = 1.05) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = pal, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.14, 0.20))) +
  labs(title = "Proportional-hazards remediation, and what it cost",
       subtitle = paste0("Violators: ", gsub("\\|", " and ", ph$violating_terms[1]),
                         ". The time-varying fit has no Schoenfeld p because cox.zph\n",
                         "is undefined for models containing tt() terms — absent for a reason, not a failure."),
       x = NULL, y = NULL,
       caption = "Source: scripts/ph_diagnostics_figure.R") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "#4A5459", lineheight = 1.15),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave(here("output", "figures", "ph_remediation.png"), pp,
       width = 9, height = 7, dpi = 300, bg = "white")
message("wrote output/figures/ph_remediation.png")
