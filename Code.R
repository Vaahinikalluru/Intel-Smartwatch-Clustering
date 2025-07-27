library(readxl)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
#install.packages("factoextra", dependencies = TRUE)

# Load data
df <- read_excel("C://Users//Vaahini//Downloads//SmartWatch Data File (3).xlsx")

# Select features and scale
features <- df %>% select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)
scaled_features <- scale(features)

# Elbow method (K = 4)
png("C://Users//Vaahini//Downloads//elbow_method.png", width = 800, height = 600)
fviz_nbclust(scaled_features, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "red", linewidth = 1.2) +
  ggtitle("Elbow Method for K = 4") +
  theme_minimal(base_size = 14)
dev.off()


# K-Means clustering (K = 4)
set.seed(123)
km_res <- kmeans(scaled_features, centers = 4, nstart = 25)
df$Segment <- as.factor(km_res$cluster)

# Silhouette plot
sil <- silhouette(km_res$cluster, dist(scaled_features))
png("C://Users//Vaahini//Downloads//silhouette_plot_k4.png", width = 800, height = 600)
fviz_silhouette(sil) +
  ggtitle("Silhouette Plot (K = 4)") +
  theme_minimal(base_size = 14)
dev.off()

# Cluster visualization
png("C://Users//Vaahini//Downloads//kmeans_clustering.png", width = 800, height = 600)
fviz_cluster(km_res, data = scaled_features, geom = "point", ellipse.type = "convex",
             palette = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00")) +
  ggtitle("K-Means Clustering (K = 4)") +
  theme_minimal(base_size = 14)
dev.off()

# Hierarchical clustering
dist_mat <- dist(scaled_features)
hc <- hclust(dist_mat, method = "ward.D2")
png("C://Users//Vaahini//Downloads//dendrogram_4_clusters.png", width = 800, height = 600)
plot(hc, main = "Dendrogram (K = 4)", xlab = "", ylab = "Height", cex.main = 1.5)
rect.hclust(hc, k = 4, border = c("red", "blue", "green", "purple"))
dev.off()

df$Hierarchical_Segment <- cutree(hc, k = 4)

# Segment summary
summary <- df %>%
  group_by(Segment) %>%
  summarise(across(c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, Age, Income), 
                   mean, na.rm = TRUE))
print(summary)
