#1. Cluster Analysis
library(cluster)

# data from relfreq.R (type) or proportion.R (all_proportion)
#after adding metadata: Filename; Author; Title; Year as first 4 rows
data_cluster <- type #all_proportion
rownames(data_cluster) <- unlist(data_cluster$Filename)

#cluster
d <- dist(data_cluster[,5:16], method = "manhattan") # no Copulative
fit <- hclust(d, method="ward.D")
dend <- as.dendrogram(fit)
#olor values
library(dendextend)
library(colorspace)
color_unique_labels <- function(dend, ...) {
  n_unique_labels <- length(unique(sub('(.*)(_.*)','\\1', labels(dend), perl=T)))
  colors <- colorspace::diverging_hcl(n_unique_labels, "Berlin")
  labels_number <- as.numeric(factor(sub('(.*)(_.*)','\\1', labels(dend), perl=T)))
  labels_colors(dend) <- colors[labels_number]
  dend
}
dend2 <- color_unique_labels(dend)
labels(dend2) <- sub('(.*_)(.*)','\\2', labels(dend), perl=T)
par(cex=0.7, mar=c(5, 8, 4, 1))
plot(dend2) 
title(main = "Manhattan Distance Based on Rel Freq", adj=0.5, cex.lab = 0.7)
title(sub = "33 Novels", adj=1.0, line=-15)

# Calculating ARI
library(mclust )
adjustedRandIndex(cutree(fit, 12), data_cluster[,2])
library(fossil)
rand.index(cutree(fit, 12), as.numeric(as.factor(data_cluster$Athor)))

#2. PCA
pca <- prcomp(data_cluster[,5:16], scale=T)
library(ggfortify)
library(ggplot2)
data_cluster$Athor <- as.factor(data_cluster$Athor)
autoplot(pca, data = data_cluster, colour = "Athor", shape=F, label.size = 3, loadings = TRUE,  loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("PCA Rel. Freq", subtitle = "33 Novels")+
  theme(plot.title = element_text(size=12)) +
  theme(plot.subtitle = element_text(hjust=1.0))+
  theme(legend.position = "none")



