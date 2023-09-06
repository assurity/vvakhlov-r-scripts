# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)
library(forcats)
library(extrafont)
library(gridExtra)
library(ggpubr)

# Read the input CSV file
# data <- read.csv("C:\\Users\\vakhlovv\\jmeter-assessment\\test_csv.csv", sep=";", header=TRUE)
# data <- read.csv("C:\\Users\\vakhlovv\\jmeter-assessment\\results_2023-08-30-14-29-02.946.csv", sep=",", nrows=33, header=FALSE)
data <- read.csv("C:\\Users\\vakhlovv\\jmeter-assessment\\TDC_20230831-155006_0_converted_copy.csv", sep=",", header=TRUE)[, c(14, 15)]
colnames(data) <- c("group","response")

print(names(data))

column1_index <- 1
column2_index <- 2

# Using temporary variables
#temp_column1 <- data[, 1]
#data[, 1] <- data[, 2]
#data[, 2] <- temp_column1
#data$group <- as.factor(data$group)

x_axis_name <- names(data)[2]
y_axis_name <- names(data)[1]
title <- sprintf("Confidence interval for %s", names(data)[1])

# Define custom color palette for each group
group_colors <- plasma(length(unique(group_stats$group)))
print(group_colors)

#update theme
theme_set (theme_light())
theme_update (
  text = element_text(family = "Courier New"),
  axis.text = element_text(size = 9),
  plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
  axis.title.y = element_text(margin = margin(r = 8, unit = "pt")),
  axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
  legend.position = "none"
)

generate_graph <- function(data, x_axis_name, y_axis_name, title) {
  
# Calculate mean and confidence intervals for each group
group_stats <- data %>%
  group_by(group) %>%
  summarise(
    mean = mean(response),
    lower_ci_90 = mean - qt(0.90, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    upper_ci_90 = mean + qt(0.90, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    lower_ci_95 = mean - qt(0.95, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    upper_ci_95 = mean + qt(0.95, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    lower_ci_99 = mean - qt(0.99, df = n() - 1) * (sd(response) / sqrt(n()-1)),
    upper_ci_99 = mean + qt(0.99, df = n() - 1) * (sd(response) / sqrt(n()-1)),
#    lower_ci_999 = mean - qt(0.999, df = n() - 1) * (sd(response) / sqrt(n()-1)),
#    upper_ci_999 = mean + qt(0.999, df = n() - 1) * (sd(response) / sqrt(n()-1)),
#    percentile_95 = quantile(response, 0.95)
  ) %>%
  arrange(desc(mean))

print (group_stats)

# Find absolute maximum from group_stats
# If you want to zoom in the values, then set
limit_x_left <- 0.85*min(unlist(group_stats[, sapply(group_stats, is.numeric)]))
#limit_x_left = 0
#limit_x_right = 30000
limit_x_right <- 1.15*max(unlist(group_stats[, sapply(group_stats, is.numeric)]))
#print (paste(limit_x_left," ", limit_x_right))

#converting group in factor variable
group_stats$group <- factor(group_stats$group, levels = group_stats$group[order(group_stats$mean, decreasing = FALSE)])

# Generate the graph
graph1 <- ggplot(group_stats, aes(y = group, x = mean, color = group)) +
  geom_errorbarh(aes(xmin = lower_ci_90, xmax = upper_ci_90), height = 0.0, linewidth = 2.5, alpha = 0.25) +
  geom_errorbarh(aes(xmin = lower_ci_95, xmax = upper_ci_95), height = 0.0, linewidth = 2.5, alpha = 0.25) +
  geom_errorbarh(aes(xmin = lower_ci_99, xmax = upper_ci_99), height = 0.0, linewidth = 2.5, alpha = 0.25) +
  geom_point(size = 3) +
  labs(x = x_axis_name, y = y_axis_name, title = title, color = "Legend") +
  xlim(limit_x_left, limit_x_right) +
  scale_color_manual(values = group_colors)

# Create the density plot using facets
graph2 <- ggplot(data, aes(x = response)) +
  #geom_histogram(aes(y=..density..), binwidth = 10000, colour="brown", fill="lightblue", alpha = 0.2) +
  geom_density(aes(color=group), stat="density", size=0.81) +
  labs(x = "Response", y = "Density", title = "Density", color = group_colors) +
  scale_color_manual(values = group_colors)
    #geom_vline(data=group_stats, aes(xintercept=mean),  colour="red",
    #linetype="dashed", linewidth=0.7, alpha = 0.5) +
    #geom_vline(data=group_stats, aes(xintercept=percentile_95), colour="blue",
    #linetype="dashed", linewidth=0.7, alpha = 0.5) +
    #geom_text(data = group_stats, aes(x = mean, y = 0.001, label = paste0("Mean: ", round(mean, 0), "\n")), vjust = -1.5, color = "red") +
    #geom_text(data = group_stats, aes(x = percentile_95, y = 0.002, label = paste0(round(percentile_95, 1))), angle = 90, vjust = -0.5, color = "blue", size = 3.5, family="Courier New")

#return(graph)
return(list(graph1 = graph1, graph2 = graph2))
}

# Filter and generate separate graphs for "*.js" and "*.css" groups
graph_group_one <- generate_graph(data %>% filter(grepl("jquery", group)), "Response", "Group", "Confidence interval for resources")
graph_group_two <- generate_graph(data %>% filter(!grepl("jquery", group)), "Response", "Group", "Confidence interval for pages")

print(graph_group_one)
print(graph_group_two)

# Arrange the graphs using grid.arrange
arranged_graphs <- grid.arrange(
  graph_js$graph1, graph_js$graph2, graph_other$graph1, graph_other$graph2,
  widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 1, 2),
                        c(3, 3, 4))
)

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