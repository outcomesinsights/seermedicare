utils::globalVariables(c("values", ".", "label"))

#' Create Publications Bar Charts (i.e., Publications by Tumor Site, Author, or Journal)
#'
#' @param x String representing data to plot on x axis
#' @param publications data.table of publications data from scrape_all_data
#' @param years Data years to include in analysis
#' @param display Number of bars to show on plot
#' @param x_label String for x-axis label
#'
#' @return ggplot2 bar plot
#' @import data.table ggplot2 magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- scrape_all_data()
#' bar_plot("sites", seer_pub_data$publications)
#' bar_plot("sites", seer_pub_data$publications, years = c(2008:2015), display = 15)
#' }
bar_plot <- function(x, publications, years = publications$year, display = NULL, x_label = x) {
  # get appropriate dataset, then subset by years and display parameter
  data <- get_data(x, dataset = publications[year %in% years])
  if(!is.null(display)) data <- data[values %in% levels(values)[1:display]]

  # Pre-compute counts for reliable rendering with ggsave/officer
  year_range <- range(data$year)
  counts <- data[, .(count = .N), by = values]
  
  # Truncate very long labels (>60 chars) to avoid rendering issues with png()
  truncate_label <- function(x, max_len = 60) {
    ifelse(nchar(x) > max_len, paste0(substr(x, 1, max_len - 3), "..."), x)
  }
  counts[, label := truncate_label(as.character(values))]
  
  # Set factor levels in order matching the original data levels
  orig_order <- unique(truncate_label(levels(data$values)))
  counts[, label := factor(label, levels = orig_order[orig_order %in% label])]
  
  # create bar plot with pre-computed counts using truncated labels
  bp <- ggplot(counts, aes(x = label, y = count)) + 
    geom_bar(stat = "identity", fill = seer_palette()[1])
  .format_plot(bp, x_label, title = sprintf("Publications by %s: %s-%s%s", x_label, year_range[1], year_range[2], ifelse(is.null(display), "", sprintf(" (top %d)", display))))
}

#' Format ggplot2 Plots With Minimalist Theme, Title, and Labels
#'
#' @param plot ggplot2 plot to format
#' @param x_label String for x-axis label
#' @param y_label String for y-axis label
#' @param title String for plot title
#'
#' @return Formatted ggplot2 plot with labels
#' @import ggplot2
#' @export
.format_plot <- function(plot, x_label, y_label = "Publications", title = "") {
  y_vals <- unlist(lapply(ggplot_build(plot)$data, function(x){x[["y"]]}))
  y_vals <- y_vals[is.finite(y_vals)]
  top <- if (length(y_vals) > 0) ceiling(max(y_vals) / 50) * 50 else 50
  if (top <= 0) top <- 50  # Ensure valid positive value
  
  # Apply theme and y-axis scale with subtle gridlines at major breaks only
  plot <- plot + .theme_seermedicare() + 
    scale_y_continuous(
      breaks = seq(0, top, top / 5),  # Fewer breaks for cleaner look
      limits = c(0, top * 1.05),
      expand = c(0, 0)
    )

  # add plot title and labels
  plot + ggtitle(title) + xlab(tools::toTitleCase(x_label)) + ylab(y_label)
}

#' Create Publications by Year Plots
#'
#' @param publications data.table of publications data from scrape_all_data
#' @param by String representing variable to use to cut data - will use top 4 most frequent in publications data
#' @param by_label Label to use in chart title
#'
#' @return ggplot2 line plot
#' @import data.table ggplot2 magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- scrape_all_data()
#' line_plot(seer_pub_data$publications)
#' line_plot(seer_pub_data$publications, by = "sites", by_label = "tumor site")
#' }
line_plot <- function(publications, by = NULL, by_label = by) {
  if(is.null(by)) {
    lp <- ggplot(publications, aes(x = factor(year), group = 1)) + geom_line(color = seer_palette()[1], linewidth = 1, stat = "count")
  } else {
    plot_data <- get_data(by, publications)[values %in% levels(values)[1:4]]
    lp <- ggplot(plot_data, aes(x = factor(year), group = values, color = values, linetype = values)) + geom_line(linewidth = 1, stat = "count") +
      scale_color_manual(values = seer_palette()) + scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"))
  }
  .format_plot(lp, x_label = "year", title = sprintf("Publications by year%s", ifelse(is.null(by), "", sprintf(" and top 4 %ss", by_label))))
}

#' Create Publications and Incidence/Prevalence/Deaths by Year Plot
#'
#' @param x Data column to plot - should be "incidence", "prevalence", or "deaths"
#' @param seer_pub_data Object of class seermedicare from scrape_all_data
#'
#' @return Combination ggplot2 plot containing bar plot for parameter and line plot for total publications
#' @import data.table ggplot2 magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' seer_pub_data <- scrape_all_data()
#' seer_plot("incidence", seer_pub_data)
#' seer_plot("prevalence", seer_pub_data)
#' seer_plot("deaths", seer_pub_data)
#' }
seer_plot <- function(x, seer_pub_data) {

  sites <- seer_pub_data$site_summary
  metadata <- seer_pub_data$metadata

  # validate parameter
  .arg_error(x, sites)

  # create dataset to use in plot
  data <- data.table(melt(sites, id.vars = "site", value.name = "values"), key = "variable") %>% .["publications", ("label") := "Total Publications"]
  data[x, `:=`(values = values / get_scale(values), label = .seer_label(x, sites, metadata))]

  # create plot using aes() with .data pronoun (aes_string is deprecated)
  sp <- ggplot(data, aes(x = .data[["site"]], y = .data[["values"]], color = .data[["label"]], fill = .data[["label"]])) +
          geom_bar(data = data[x], position = "dodge", stat = "identity") +
          geom_line(aes(group = .data[["label"]]), data = data["publications"], linetype = "dashed") +
          geom_point(aes(group = .data[["label"]]), data = data["publications"], size = 3, shape = 22) +
          scale_fill_manual(values = seer_palette()[1:2]) + scale_color_manual(values = seer_palette()[1:2])
  .format_plot(sp, x_label = "Tumor Site", y_label = "Frequency", title = sprintf("Publications relative to %s", x))
}

#' ggplot2 Theme for SEER-Medicare Plots
#'
#' @return ggplot2 theme object
#' @import ggplot2
#' @export
.theme_seermedicare <- function() {
  theme_classic() + theme(
    # Use clean sans-serif font for better readability
    text = element_text(family = "sans"),
    # Larger, bold title for bigger figures
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16, margin = margin(b = 15)),
    # Larger axis text for readability
    axis.text = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(angle = 40, hjust = 1),
    axis.title = element_text(face = "bold", size = 13, vjust = 1.25),
    # Remove gridlines for cleaner look
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    # Clean axis styling
    axis.ticks = element_blank(),
    axis.line = element_line(color = "gray70", linewidth = 0.5),
    # Legend styling - position at bottom to avoid overlap
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.2, "lines"),
    # Add margins for breathing room
    plot.margin = margin(t = 20, r = 20, b = 10, l = 10)
  )
}
