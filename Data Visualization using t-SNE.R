library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)

train <- read.csv("train_otto.csv")

## Plot Mean of Feature by 9 different Classes

## calculating mean of each feature by class
mean_feat_target<-aggregate(train[c(2:94)],list(train$target),FUN=mean)

## Gather columns into key-value pairs, key is feauture, b/c we want plot each feature.
mean_feat_target <- gather(mean_feat_target,feature,mean,2:94)

## Rename the column 1 to be "Class"
names(mean_feat_target)[1]<-"Class"

## Make the plot
pl <- ggplot(mean_feat_target, aes(x=Class,y=mean))+geom_bar(stat="identity",aes(fill=Class)) + facet_wrap( ~ feature,ncol=10, scales="free_y")

## Save as pdf
ggsave("rplot.pdf", pl, dpi = 100, width = 19.2, height = 10, units="in")

## The t-SNE t-distributed stochastic neighbor embedding algorithm is a machine learning algorithm for dimensionality reduction. 
## It is used for embedding high-dimensional data into a space of two, which can then be visualized in a scatter plot. 
features <- train[,c(-1,-95)]
tsne <- Rtsne(as.matrix(features), check_duplicates = FALSE, pca = TRUE, 
              perplexity=30, theta=0.5, dims=2)

embedding <- as.data.frame(tsne$Y)
embedding$Class <- as.factor(sub("Class_", "", train[,95]))

p <- ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  geom_point(size=1.25) +
  guides(colour = guide_legend(override.aes = list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE 2D Embedding of Products Data") +
  theme_light(base_size=20) +
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks       = element_blank(),
        axis.line        = element_blank(),
        panel.border     = element_blank())

ggsave("tsne.png", p, width=8, height=6, units="in")
