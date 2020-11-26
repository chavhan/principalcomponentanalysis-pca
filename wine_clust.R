## Obtaining cluster with PCA concept on wine dataset 
dim(wine)
wine1 <- wine[,2:14]
View(wine1)
library(psych)
pairs.panels(wine1)
pc <- princomp(wine1,cor = T,scores = T,covmat = NULL)
pc
pairs.panels(pc)
summary(pc)
loadings(pc)
str(pc)
plot(pc)
dim(pc$scores)
pc_data <- pc$scores[,1:3]
head(pc_data)
pairs.panels(pc_data)

## Hierarchical clustering on principle data 
pc_d <- dist(pc_data,method = 'euclidean')
pc_fit <- hclust(pc_d,method = 'complete')
plot(pc_fit)
pc_k <- kselection(pc_data,parallel = T)
pc_k
fviz_nbclust(pc_data,hcut,method = 'silhouette')+labs(subtitle = 'Elbow Curve')
pc_group <- cutree(pc_fit,k=3)
pc_group <- as.matrix(pc_group)
pc_data1 <- cbind(pc_data,pc_group)
View(pc_data1)
str(pc_data1)
names(pc_data1)
pc_data1 <- as.data.frame(pc_data1)
names(pc_data1)[names(pc_data1) == 'V4'] <- 'group'
View(aggregate(pc_data1,by=list(pc_group),FUN = 'mean'))

## Kmeans on PC data 
pc_km <- kmeans(pc_data,3)
library(animation)
kmeans.ani(pc_data,3)
summary(pc_km)
View(aggregate(pc_data,by=list(pc_km$cluster),FUN='mean'))

## Hierarchical clustering on normal data 

str(wine1)
normalized_wine <- scale(wine1)
d <- dist(normalized_wine,method = 'euclidean')
fit <- hclust(d,method = 'complete')
plot(fit,hang = -1)
k <- kselection(normalized_wine,parallel = T)
k
fviz_nbclust(normalized_wine,kmeans,method = 'silhouette')