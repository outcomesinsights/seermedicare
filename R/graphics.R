utils::globalVariables(c("values", "."))

#' Create Publications Bar Charts (i.e., Publications by Tumor Site, Author, or Journal)
#'
#' @param x String representing data to plot on x axis
#' @param years Data years to include in analysis
#' @param display Number of bars to show on plot
#' @param x_label String for x-axis label
#'
#' @return ggplot2 bar plot
#' @import data.table ggplot2 magrittr
#' @export
#'
#' @examples \dontrun{
#' bar_plot("sites")
#' bar_plot("sites", years = c(2008:2015), display = 15)
#' }
bar_plot <- function(x, years = publications$year, display = NULL, x_label = x) {
  # get appropriate dataset, then subset by years and display parameter
  data <- get_data(x, dataset = publications[year %in% years])
  if(!is.null(display)) data <- data[values %in% levels(values)[1:display]]

  # create bar plot
  bp <- ggplot(data, aes_string("values")) + geom_bar(fill = palette()[1])
  .format_plot(bp, x_label, title = sprintf("Publications by %s: %s-%s%s", x_label, min(data$year), max(data$year), ifelse(is.null(display), "", sprintf(" (top %d)", display))))
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
#'
#' @examples \dontrun{}
.format_plot <- function(plot, x_label, y_label = "Publications", title = "") {
  top <- plyr::round_any(max(unlist(lapply(ggplot_build(plot)$data, function(x){x[["y"]]})), na.rm = T), 50, f = ceiling)
  plot <- plot + .theme_seermedicare() + scale_y_continuous(breaks = seq(0, top, top / 10), limits = c(0, top + 5), expand = c(0, 0))
  
  # add plot title and labels
  plot + ggtitle(sprintf("%s\u000A", title)) + xlab(tools::toTitleCase(x_label)) + ylab(y_label)
}

#' Create Publications by Year Plots
#'
#' @param by String representing variable to use to cut data - will use top 4 most frequent in publications data
#' @param by_label Label to use in chart title
#'
#' @return ggplot2 line plot
#' @import data.table ggplot2 magrittr
#' @export
#'
#' @examples \dontrun{
#' line_plot()
#' line_plot(by = "sites", by_label = "tumor site")
#' }
line_plot <- function(by = NULL, by_label = by) {
  if(is.null(by)) { 
    lp <- ggplot(publications, aes(factor(year), group = 1)) + geom_line(color = palette()[1], size = 1, stat = "count")
  } else { 
    lp <- ggplot(get_data(by)[values %in% levels(values)[1:4]], aes(factor(year), group = values, color = values, linetype = values)) + geom_line(size = 1, stat = "count") +
      scale_color_manual(values = palette()) + scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted"))
  }
  .format_plot(lp, x_label = "year", title = sprintf("Publications by year%s", ifelse(is.null(by), "", sprintf(" and top 4 %ss", by_label))))
}

#' Create Publications and Incidence/Prevalence/Deaths by Year Plot
#'
#' @param x Data column to plot - should be "incidence", "prevalence", or "deaths"
#'
#' @return Combination ggplot2 plot containing bar plot for parameter and line plot for total publications
#' @import data.table ggplot2 magrittr
#' @export
#'
#' @examples \dontrun{
#' seer_plot("incidence")
#' seer_plot("prevalence")
#' seer_plot("deaths")
#' }
seer_plot <- function(x) {
  # validate parameter
  .arg_error(x, sites)
  
  # create dataset to use in plot
  data <- data.table(melt(sites, id.vars = "site", value.name = "values"), key = "variable") %>% .["publications", ("label") := "Total Publications"]
  data[x, `:=`(values = values / get_scale(values), label = .seer_label(x))]

  # create plot
  sp <- ggplot(data, aes_string("site", "values", color = "label", fill = "label")) + geom_bar(data = data[x], position = "dodge", stat = "identity") +
    geom_line(aes_string(group = "label"), data["publications"], linetype = "dashed") + geom_point(aes_string(group = "label"), data["publications"], size = 3, shape = 22) +
    scale_fill_manual(values = palette()[1:2]) + scale_color_manual(values = palette()[1:2])
  .format_plot(sp, x_label = "Tumor Site", y_label = "Frequency", title = sprintf("Publications relative to %s", x))
}

#' ggplot2 Theme
#'
#' @return ggplot2 Theme
#' @import ggplot2
#' @export
#'
#' @examples \dontrun{}
.theme_seermedicare <- function() {
  theme_classic() + theme(text = element_text(family = "serif"), plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
                          axis.text = element_text(size = 10), axis.text.x = element_text(angle = 40, hjust = 1),
                          axis.title = element_text(face = "bold", vjust = 1.25), panel.grid.major.y = element_line(linetype = "dashed"),
                          axis.ticks = element_blank(), legend.title = element_blank(),legend.position = c(.5, .82))
}
