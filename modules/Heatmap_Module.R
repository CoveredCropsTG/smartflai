if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 is required but not installed.")
}
library(ggplot2)

# Helper code (to build the heat maps)
make_heatmap <- function(df, x_levels, y_levels, title, fill_label, cols, brks) {
  ggplot(df, aes(
    x = factor(col, levels = x_levels),
    y = factor(row, levels = y_levels),
    fill = count
  )) +
    geom_tile(color = "black") +
    geom_text(aes(label = ifelse(count > 0, count, "")), size = 4) +
    scale_fill_gradientn(
      colors = cols,
      values  = scales::rescale(brks),
      limits  = c(0, max(df$count, na.rm = TRUE))
    ) +
    theme_minimal() +
    labs(title = title, fill = fill_label) +
    theme(
      axis.title      = element_blank(),
      axis.text       = element_text(size = 12),
      legend.position = "right"
    )
}