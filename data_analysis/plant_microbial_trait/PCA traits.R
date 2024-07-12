library(PCAtest)
library(doBy)
library(ggplot2)
library(ggrepel)

data <- read_excel("plant_traits.xlsx")

data1 <- data[, 2:13]

# Test the significance of principal components
result <- PCAtest(data1, 100, 100, 0.05, varcorr=FALSE, counter=FALSE, plot=TRUE)

# Perform PCA with scaling and centering
data_scaled <- scale(data1)
res <- prcomp(data_scaled, center=TRUE, retx=TRUE, scale.=TRUE)

# Extract principal components and merge with group data
pca_scores <- as.data.frame(res$x)
merged_data <- cbind(pca_scores, group=data$group)
colnames(merged_data)[ncol(merged_data)] <- "group"

pc1 <- merged_data[, 1]
pc2 <- merged_data[, 2]
group <- data$group

# Prepare data for plotting
plotdata <- data.frame(pc1=pc1, pc2=pc2, group=group)

se <- function(x) sd(x) / sqrt(length(x))
conf_95 <- function(x) t.test(x, conf.level=0.95)$conf.int[2]
summary_data <- summaryBy(pc1 + pc2 ~ group, data=plotdata, FUN=c(mean, sd, se, conf_95))

explained_variance <- summary(res)$importance

loadings <- as.data.frame(res$rotation) * 7

# Plot PCA
p <- ggplot() +
   geom_hline(yintercept=0, linetype=2, size=0.5, color="gray") +
   geom_vline(xintercept=0, linetype=2, size=0.5, color="gray") +
   geom_errorbarh(data=summary_data, aes(x=pc1.mean, y=pc2.mean, xmin=pc1.mean - pc1.se, xmax=pc1.mean + pc1.se), height=0.15) +
   geom_errorbar(data=summary_data, aes(x=pc1.mean, y=pc2.mean, ymin=pc2.mean - pc2.se, ymax=pc2.mean + pc2.se), width=0.15) +
   geom_point(data=summary_data, aes(x=pc1.mean, y=pc2.mean, color=group, shape=group, fill=group), size=3) +
   scale_shape_manual(values=c(21, 22, 23, 24, 25, 21, 22, 23, 24, 25, 21)) +
   scale_fill_manual(values=c("red", "#842820", "#AE1F24", "#E04110", "#EF7E49", "#E18D37", "#0A8544", "#43B363", "#5FB96C", "#4682B4", "#86C5ED")) +
   geom_segment(data=loadings, aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(angle=30, length=unit(0.35, "cm")), linetype=1, size=0.5, color="red") +
   geom_text_repel(data=loadings, aes(PC1 + PC1 * 0.15, PC2 + PC2 * 0.1, label=row.names(loadings))) +
   labs(x=paste("PC1 (", format(100 * explained_variance[2, 1], digits=4), "%)", sep=""),
        y=paste("PC2 (", format(100 * explained_variance[2, 2], digits=4), "%)", sep="")) +
   theme_bw()


