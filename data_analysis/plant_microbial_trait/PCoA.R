
library(vegan)
library(ggplot2)

data <- read.csv("trait_com.csv", row.names = 1)

data_bc <- vegdist(data, method = "bray")

# Perform PCoA analysis
pcoa <- pcoa(data_bc)

groups <- read.csv("group.csv", header = FALSE, colClasses = "character")
groups <- as.list(groups)

# Prepare plot data
plotdata <- data.frame(
   sample = rownames(pcoa$vectors),
   PCo1 = pcoa$vectors[, 1],
   PCo2 = pcoa$vectors[, 2],
   group = groups$V2
)

# Calculate explained variance
PCo1_var <- floor(pcoa$values$Relative_eig[1] * 100)
PCo2_var <- floor(pcoa$values$Relative_eig[2] * 100)

# Perform PERMANOVA
otu.adonis <- adonis(data_bc ~ V2, data = groups, permutations = 999)
R2 <- round(otu.adonis$aov.tab$R2[[1]], 3)
p <- otu.adonis$aov.tab$`Pr(>F)`[[1]]

# Basic plotting
ggplot(plotdata, aes(PCo1, PCo2)) +
   geom_point(aes(colour = "black", shape = group, fill = group), size = 3) +
   scale_shape_manual(values = 21:25) +
   scale_fill_manual(values = c("#FF3300", "#842820", "#AE1F24", "#E04110", "#EF7E49", 
                                "#E18D37", "#0A8544", "#43B363", "#5FB96C", "#4682B4", "#86C5ED")) +
   labs(title = "Functional traits") + 
   xlab(paste("PCo1 (", PCo1_var, "%)", sep = "")) + 
   ylab(paste("PCo2 (", PCo2_var, "%)", sep = "")) +
   theme(text = element_text(size = 5)) +
   geom_vline(xintercept = 0, linetype = "dotted") +
   geom_hline(yintercept = 0, linetype = "dotted") +
   theme(
      panel.background = element_rect(fill = 'white', colour = 'black'),
      panel.grid = element_blank(),
      axis.title = element_text(color = 'black', size = 8),
      axis.ticks.length = unit(0.4, "lines"), 
      axis.ticks = element_line(color = 'black'),
      axis.line = element_line(colour = "black"),
      axis.title.x = element_text(colour = 'black', size = 11),
      axis.title.y = element_text(colour = 'black', size = 11),
      axis.text = element_text(colour = 'black', size = 8),
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key = element_blank(),
      legend.background = element_rect(colour = "black"),
      legend.key.height = unit(0.6, "cm"),
      plot.title = element_text(size = 8, colour = "black", hjust = 0.5, face = "bold")
   ) +
   stat_ellipse(aes(fill = group), geom = "polygon", level = 0.7, alpha = 0.3) +
   geom_text(aes(x = 0.1, y = -0.05, label = paste("PERMANOVA:\n R2 = ", R2, "\n p = ", p)), 
             size = 3, hjust = 0)
