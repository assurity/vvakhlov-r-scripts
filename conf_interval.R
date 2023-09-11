# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(forcats)
library(extrafont)
library(gridExtra)
library(ggpubr)

source("graphs.R")

# Read the input CSV file, only use 14 and 15 column.
csv <- read.csv("C:\\Users\\vakhlovv\\jmeter-assessment\\TDC_20230831-155006_0_converted_copy.csv", sep=",", header=TRUE)
#data <- read.csv("C:\\Users\\vakhlovv\\jmeter-assessment\\TDC_20230831-155006_0_converted_copy.csv", sep=",", header=TRUE)[, c(14, 15)]

data <- csv %>% select(timeStamp, label, Latency)
colnames(data) <- c("timeStamp", "group","response")

data$timeStamp <- as.POSIXct(data$timeStamp, format = "%Y/%m/%d %H:%M:%OS")

print(names(data))

#column1_index <- 1
#column2_index <- 2

# Using temporary variables
#temp_column1 <- data[, 1]
#data[, 1] <- data[, 2]
#data[, 2] <- temp_column1
#data$group <- as.factor(data$group)

x_axis_name <- names(data)[2]
y_axis_name <- names(data)[1]
title <- sprintf("Confidence interval for %s", names(data)[1])

#update theme for visualization
theme_set (theme_light())
theme_update (
  text = element_text(family = "Courier New"),
  axis.text = element_text(size = 9),
  plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
  axis.title.y = element_text(margin = margin(r = 8, unit = "pt")),
  axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
  legend.position = "none"
)


# Filter and generate separate graphs for "*.js" and "*.css" groups
graph_group_one <- generate_graph_conf(data %>% filter(grepl("Rainfall", group)), "Response", "Group", "Confidence interval for resources")
graph_group_two <- generate_graph_conf(data %>% filter(!grepl("Rainfall", group)), "Response", "Group", "Confidence interval for pages")
graph_group_three <- generate_graph_percentile(data %>% filter(!grepl("jquery", group)), "Response", "Group", "Percentile for pages")
graph_group_four <- generate_graph_scatter(data %>% filter(grepl("Flow", group)), "Response", "Group", "Confidence interval for resources")

print(graph_group_one)
print(graph_group_two)
print(graph_group_three)
print(graph_group_four)


arranged_graphsScatter <- do.call(grid.arrange, c(graph_group_four, ncol = 2))


# Arrange the graphs using grid.arrange, this won't align graphs.
# Left if needed in the future.

#arranged_graphs <- grid.arrange(
#  graph_js$graph1, graph_js$graph2, graph_other$graph1, graph_other$graph2,
#  widths = c(2, 1, 1),
#  layout_matrix = rbind(c(1, 1, 2),
#                        c(3, 3, 4))
#)


# The code below arranges graphs by pair and aligns them.

arranged_graphs1 <- ggarrange(
  graph_group_one$graph1, graph_group_two$graph1,
  ncol = 1, nrow = 2, align = "v")

arranged_graphs2 <- ggarrange(
  graph_group_one$graph2, graph_group_two$graph2,
  ncol = 1, nrow = 2, align = "v")

arranged_graphs_all <- grid.arrange(
  arranged_graphs1, arranged_graphs2,
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 1, 2))
)

# Display the arranged graphs

#print(arranged_graphs)
print(arranged_graphs_all)
print(arranged_graphsScatter)

