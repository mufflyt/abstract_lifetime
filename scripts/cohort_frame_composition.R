# cohort_frame_composition.R — the cohort is two sampling frames, not one.
#
# Ten congresses are front-truncated and two are complete censuses of their oral
# block, and the two groups report publication rates differing by a factor of
# more than two. That is the single most consequential fact about the cohort and
# it was established only in conversation, so this makes it a reproducible
# artifact and a figure.
#
# The evidence for "complete" is not an assumption. R/01d_tag_session_type.R
# reads real <h3 class="section-title"> headings, and in 2022 and 2023 the
# capture ran PAST the end of the oral block into Video, which is what proves
# the oral block was exhausted. In 2012-2021 every captured record carries an
# Oral heading and no Video row exists, so ingestion stopped inside the block.
#
# Usage: Rscript scripts/cohort_frame_composition.R

suppressPackageStartupMessages({
  library(here); library(readr); library(dplyr); library(ggplot2); library(tidyr)
})

parsed <- read_csv(here("data", "processed", "abstracts_parsed.csv"),
                   show_col_types = FALSE, progress = FALSE)
fad <- read_csv(here("output", "final_analytical_dataset.csv"),
                show_col_types = FALSE, progress = FALSE)
cov <- read_csv(here("output", "cohort_coverage_by_year.csv"),
                show_col_types = FALSE, progress = FALSE)

# A congress is a census of its oral block only if the capture reached Video.
frames <- parsed |>
  group_by(congress_year) |>
  summarise(captured = n(),
            n_oral  = sum(session_type == "Oral",  na.rm = TRUE),
            n_video = sum(session_type == "Video", na.rm = TRUE),
            .groups = "drop") |>
  mutate(frame = if_else(n_video > 0, "Census of orals", "Front-truncated"))

rates <- fad |>
  filter(!is.na(final_published)) |>
  group_by(congress_year) |>
  summarise(evaluated = n(), published = sum(final_published),
            rate = 100 * mean(final_published), .groups = "drop")

tbl <- frames |>
  left_join(rates, by = "congress_year") |>
  left_join(cov |> select(congress_year, supplement_items, coverage_pct),
            by = "congress_year") |>
  arrange(congress_year)

write_csv(tbl, here("output", "cohort_frame_composition.csv"))

pooled <- tbl |>
  group_by(frame) |>
  summarise(congresses = n(), evaluated = sum(evaluated),
            published = sum(published),
            rate = round(100 * sum(published) / sum(evaluated), 1), .groups = "drop")
print(as.data.frame(pooled))

# --- figure -----------------------------------------------------------------
# Two panels sharing the x axis: what fraction of each programme was captured,
# and what rate that capture reported. Colour carries the frame, because the
# point is that the two groups are not comparable rather than that they differ.
long <- tbl |>
  transmute(congress_year, frame,
            `Share of the supplement captured (%)` = coverage_pct,
            `Reported publication rate (%)` = round(rate, 1)) |>
  pivot_longer(-c(congress_year, frame), names_to = "panel", values_to = "value")

pal <- c("Front-truncated" = "#B8654F", "Census of orals" = "#1B3A4B")

p <- ggplot(long, aes(factor(congress_year), value, fill = frame)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.1f", value)), vjust = -0.45, size = 2.7,
            colour = "#33393D") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = pal, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "The cohort is two sampling frames, not one",
    subtitle = sprintf(
      paste("Ten congresses were captured front-first and stopped inside the oral block (%.1f%%).",
            "\nTwo ran past the block's end into Video and are complete censuses of it (%.1f%%)."),
      pooled$rate[pooled$frame == "Front-truncated"],
      pooled$rate[pooled$frame == "Census of orals"]),
    x = "Congress year", y = NULL,
    caption = "Source: scripts/cohort_frame_composition.R") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "#4A5459", lineheight = 1.15),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave(here("output", "figures", "cohort_frame_composition.png"), p,
       width = 9, height = 7, dpi = 300, bg = "white")
message("wrote output/cohort_frame_composition.csv and ",
        "output/figures/cohort_frame_composition.png")
