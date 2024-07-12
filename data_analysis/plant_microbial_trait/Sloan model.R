# Load necessary libraries
library(MicEco)
library(ggplot2)
library(plyr)
library(ggimage)

comm <- read.csv('microb_comm.csv', header = TRUE, row.names = 1)
comm[is.na(comm)] <- 0

# Fit the neutral model
res <- neutral.fit(comm)
m <- res[[1]][1]
r2 <- res[[1]][3]
data <- res[[2]]

# Plot predicted occurrence frequency and confidence intervals
p1 <- ggplot() +
   geom_line(data = data, aes(x = log(p), y = freq.pred), size = 0.9) +
   geom_line(data = data, aes(x = log(p), y = Lower), size = 0.9, linetype = 2) +
   geom_line(data = data, aes(x = log(p), y = Upper), size = 0.9, linetype = 2) +
   xlab("Mean relative abundance(log10)") + ylab("Occurrence frequency")

# Categorize points into three groups
data <- mutate(data, group = case_when(
   freq < Lower ~ "#47aee3",  # Below lower bound
   freq > Upper ~ "#F46875",  # Above upper bound
   TRUE ~ "lightgray"  # Within bounds
))

 
mycols <- c("#47aee3", "#F46875", "lightgray")

# Plot points with color categorization
p2 <- p1 + 
   geom_point(data = data, aes(x = log(p), y = freq, color = group), size = 0.6) +
   scale_colour_manual(values = mycols) +
   annotate("text", x = -16, y = 0.95, label = paste("m = ", round(m, 3)), size = 3) +
   annotate("text", x = -16, y = 1, label = paste("R2 = ", round(r2, 3)), size = 3)

# Adjust plot theme
plot_theme <- theme(
   panel.background = element_blank(),
   panel.grid = element_blank(),
   axis.line = element_line(size = 0.3, colour = "black"),
   axis.ticks = element_line(color = "black"),
   axis.text = element_text(color = "black", size = 9),
   legend.position = "none",
   legend.background = element_blank(),
   legend.key = element_blank(),
   legend.text = element_text(size = 10),
   text = element_text(family = "sans", size = 10)
)

p3 <- p2 + plot_theme

# Calculate proportions for each group
group_counts <- table(data$group)
df <- data.frame(
   type = c('Neutral', 'Above', 'Below'),
   nums = c(group_counts["lightgray"], group_counts["#F46875"], group_counts["#47aee3"])
)
df$label <- paste(df$type, round(df$nums / sum(df$nums) * 100, 1), '%')
 
p4 <- ggplot(data = df, aes(x = "", y = nums, fill = type)) +
   geom_bar(stat = 'identity', width = 1) +
   coord_polar(theta = 'y') +
   geom_text(aes(label = label), position = position_stack(vjust = 0.6), size = 3) +
   scale_fill_manual(name = '', values = c("#F46875", "#47aee3", "lightgray")) +
   theme_void() +
   theme(legend.position = "right", legend.text = element_text(size = 6))

# Combine plots
g <- p3 + geom_subview(subview = p4, x = -6, y = 0.3, w = 7, h = 7)
g
