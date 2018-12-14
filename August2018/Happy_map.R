library(tidyverse)
library(data.table)
library(mixtools)
library(ggrepel)
library(Rtsne)

fiv <- fread("world-happiness-report/2015.csv")
six <- fread("world-happiness-report/2016.csv")
sev <- fread("world-happiness-report/2017.csv")
newcolnames <- c("Country", "Region", "Happy_Rank", "Happy_Score",
                     "Economy", "Family", "Health", "Freedom", "Trust", "Generosity", "Dystopia_Residual")
newcolnames2 <- c("Country", "Happy_Rank", "Happy_Score",
                  "Economy", "Family", "Health", "Freedom", "Generosity", "Trust", "Dystopia_Residual")
fifth <- fiv
sixth <- six %>%
  select(-c(5, 6))
seventh <- sev %>%
  select(-c(4, 5))
names(fifth) <- newcolnames
names(sixth) <- newcolnames
names(seventh) <- newcolnames2
country_region1 <- fifth %>%
  select(Country, Region)
country_region2 <- sixth %>%
  select(Country, Region)
cr_union <- union(country_region1, country_region2)
tempSeven <- left_join(seventh, cr_union)
tempSeven[is.na(tempSeven$Region),] # to find which country has different name like Hong Kong S.A.R., China
seventh2 <- tempSeven %>%
  select(1, 11, 2:7, 9, 8, 10) # change order of columns
happy_data <- bind_rows(fifth, sixth, seventh2) # bind three dfs at once
nrow(happy_data)

happy_data <- happy_data[,-c("Happy_Rank", "Happy_Score")]
country_scores <- happy_data %>%
  group_by(Country, Region) %>%
  summarise_all(mean)


country_name <- country_scores$Country
country_scores$Region <- as.factor(country_scores$Region)
country_region <- country_scores$Region
country_score <- as.data.frame(country_scores) %>%
  select(-Country, -Region)

# Gaussian Mixture Model
gmm <- mvnormalmixEM(country_score, k = 5, epsilon = 1e-04)
gmmcluster <- apply(gmm$posterior, 1, function(row) which.max(row))

kms <- kmeans(country_score, 5)
kmscluster <- kms$cluster

country.d <- dist(country_score)
fit <- cmdscale(country.d, eig=TRUE, k=2, add=F)
x <- fit$points[,1]
y <- fit$points[,2]

ggplot(bind_cols(x=x,y=y,name=country_name, clust=as.factor(gmmcluster)), aes(x, y)) +
  geom_text_repel(aes(label = name, color = clust), fontface = "bold") + 
  stat_ellipse(aes(x=x, y=y, fill = clust), alpha=.1,type='t',geom="polygon") +
  ggtitle("Gaussian Mixture Cluster by happy_index") + xlab("") + ylab("")

ggplot(bind_cols(x=x,y=y,name=country_name, clust=as.factor(kmscluster)), aes(x, y)) +
  geom_text_repel(aes(label = name, color = clust), fontface = "bold") + 
  stat_ellipse(aes(x=x, y=y, fill = clust), alpha=.1,type='norm', geom="polygon") +
  ggtitle("K-means by happy_index") + xlab("") + ylab("")

tsne_model = Rtsne(as.matrix(country_score), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)

ggplot(data.frame(x=tsne_model$Y[,1], y=tsne_model$Y[,2], clust=as.factor(gmmcluster), name=country_name), aes(x, y)) +
  geom_text_repel(aes(label = name, color = clust), fontface = "bold") + 
  stat_ellipse(aes(x=x, y=y, fill = clust), alpha=.1,type='t', geom="polygon") +
  ggtitle("Gaussian Mixture Cluster by happy_index visualized using t-sne") + xlab("") + ylab("")
