###############################################################################
# 95_lifestyle_overlap.R — lifestyle group overlap table + Venn diagram (SI).
# Requires: target_data (from 30_stat_vars.R), output (results dir).
# Writes:   lifestyle_overlap.csv/.png, lifestyle_overlap_venn.png
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

.ov_data <- target_data |>
  filter(!is.na(no_car), !is.na(no_flying), !is.na(no_meat))

.ov_n <- nrow(.ov_data)

# --- 8-cell overlap table ----------------------------------------------------
lifestyle_overlap <- .ov_data |>
  count(no_car, no_flying, no_meat, name = "N") |>
  arrange(desc(no_car), desc(no_flying), desc(no_meat)) |>
  mutate(
    share = round(N / .ov_n, 4),
    # TRE disclosure rule of thumb; export officer makes the final call
    small_cell_flag = N < 5
  )

# Marginals and pairwise overlaps (convenience for SI text)
.mar <- function(expr) sum(expr)
lifestyle_overlap_summary <- tibble(
  group = c("car_free", "flight_free", "meat_free",
            "car_free & flight_free", "car_free & meat_free",
            "flight_free & meat_free", "all three", "none"),
  N = c(.mar(.ov_data$no_car), .mar(.ov_data$no_flying), .mar(.ov_data$no_meat),
        .mar(.ov_data$no_car & .ov_data$no_flying),
        .mar(.ov_data$no_car & .ov_data$no_meat),
        .mar(.ov_data$no_flying & .ov_data$no_meat),
        .mar(.ov_data$no_car & .ov_data$no_flying & .ov_data$no_meat),
        .mar(!.ov_data$no_car & !.ov_data$no_flying & !.ov_data$no_meat))
) |>
  mutate(share = round(N / .ov_n, 4))

write.csv(lifestyle_overlap, file.path(output, "lifestyle_overlap.csv"),
          row.names = FALSE)
write.csv(lifestyle_overlap_summary,
          file.path(output, "lifestyle_overlap_summary.csv"), row.names = FALSE)

# Lifestyle x major-city crosstab (verifies/replaces the SI S11.2 shares)
if ("major_city" %in% names(.ov_data)) {
  lifestyle_major_city <- tibble(
    group = c("car_free", "car_owner", "flight_free", "flyer",
              "meat_free", "meat_eater", "all"),
    N = c(sum(.ov_data$no_car), sum(!.ov_data$no_car),
          sum(.ov_data$no_flying), sum(!.ov_data$no_flying),
          sum(.ov_data$no_meat), sum(!.ov_data$no_meat), nrow(.ov_data)),
    share_major_city = round(c(
      mean(.ov_data$major_city[.ov_data$no_car]),
      mean(.ov_data$major_city[!.ov_data$no_car]),
      mean(.ov_data$major_city[.ov_data$no_flying]),
      mean(.ov_data$major_city[!.ov_data$no_flying]),
      mean(.ov_data$major_city[.ov_data$no_meat]),
      mean(.ov_data$major_city[!.ov_data$no_meat]),
      mean(.ov_data$major_city)), 4)
  )
  write.csv(lifestyle_major_city,
            file.path(output, "lifestyle_major_city.csv"), row.names = FALSE)
  cat("  Saved: lifestyle_major_city.csv\n")
}

# --- Venn diagram (three fixed circles, no extra packages) -------------------
.circle <- function(cx, cy, r, n = 200) {
  t <- seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}

# Region counts (Venn cells)
.cnt <- function(car, fly, meat)
  sum(.ov_data$no_car == car & .ov_data$no_flying == fly & .ov_data$no_meat == meat)
v100 <- .cnt(TRUE, FALSE, FALSE);  v010 <- .cnt(FALSE, TRUE, FALSE)
v001 <- .cnt(FALSE, FALSE, TRUE);  v110 <- .cnt(TRUE, TRUE, FALSE)
v101 <- .cnt(TRUE, FALSE, TRUE);   v011 <- .cnt(FALSE, TRUE, TRUE)
v111 <- .cnt(TRUE, TRUE, TRUE);    v000 <- .cnt(FALSE, FALSE, FALSE)

.r <- 1.25
c_car  <- .circle(-0.72,  0.42, .r)
c_fly  <- .circle( 0.72,  0.42, .r)
c_meat <- .circle( 0.00, -0.82, .r)

.fmt <- function(n) sprintf("%d\n(%.0f%%)", n, 100 * n / .ov_n)

venn_labels <- data.frame(
  x = c(-1.25,  1.25,  0.00,  0.00, -0.80,  0.80,  0.00),
  y = c( 0.75,  0.75, -1.35,  0.85, -0.45, -0.45, -0.10),
  lab = c(.fmt(v100), .fmt(v010), .fmt(v001), .fmt(v110),
          .fmt(v101), .fmt(v011), .fmt(v111))
)

venn_plot <- ggplot() +
  geom_polygon(data = c_car,  aes(x, y), fill = "#4477AA", alpha = 0.30) +
  geom_polygon(data = c_fly,  aes(x, y), fill = "#EE6677", alpha = 0.30) +
  geom_polygon(data = c_meat, aes(x, y), fill = "#228833", alpha = 0.30) +
  geom_path(data = c_car,  aes(x, y), colour = "#4477AA", linewidth = 0.4) +
  geom_path(data = c_fly,  aes(x, y), colour = "#EE6677", linewidth = 0.4) +
  geom_path(data = c_meat, aes(x, y), colour = "#228833", linewidth = 0.4) +
  geom_text(data = venn_labels, aes(x, y, label = lab), size = 3.4, lineheight = 0.9) +
  annotate("text", x = -1.55, y =  1.75, label = sprintf("Car-free (n = %d)",    v100 + v110 + v101 + v111),
           colour = "#4477AA", fontface = "bold", size = 4, hjust = 0.5) +
  annotate("text", x =  1.55, y =  1.75, label = sprintf("Flight-free (n = %d)", v010 + v110 + v011 + v111),
           colour = "#EE6677", fontface = "bold", size = 4, hjust = 0.5) +
  annotate("text", x =  0.00, y = -2.30, label = sprintf("Meat-free (n = %d)",   v001 + v101 + v011 + v111),
           colour = "#228833", fontface = "bold", size = 4, hjust = 0.5) +
  annotate("text", x =  2.05, y = -1.95,
           label = sprintf("None of the three: %d (%.0f%%)", v000, 100 * v000 / .ov_n),
           size = 3.4, hjust = 1) +
  coord_equal(xlim = c(-2.3, 2.3), ylim = c(-2.5, 2.0)) +
  theme_void() +
  ggtitle(sprintf("Overlap of low-carbon lifestyle groups (N = %d)", .ov_n)) +
  theme(plot.title = element_text(hjust = 0.5, size = 12))

ggsave(file.path(output, "lifestyle_overlap_venn.png"), venn_plot,
       width = 7, height = 6.2, dpi = 300, bg = "white")

if (exists("render_data_frame_to_png")) {
  render_data_frame_to_png(lifestyle_overlap,
                           file.path(output, "lifestyle_overlap"),
                           "Lifestyle group overlap (8 cells)")
  render_data_frame_to_png(lifestyle_overlap_summary,
                           file.path(output, "lifestyle_overlap_summary"),
                           "Lifestyle group overlap summary")
}

cat(sprintf("  Overlap: car&fly %d | car&meat %d | fly&meat %d | all three %d | none %d\n",
            v110 + v111, v101 + v111, v011 + v111, v111, v000))
cat("  Saved: lifestyle_overlap.csv, lifestyle_overlap_summary.csv, lifestyle_overlap_venn.png\n")
