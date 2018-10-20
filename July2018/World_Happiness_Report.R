library(tidyverse)
library(data.table)
library(earth)
library(plotly)
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
fisix <- bind_rows(fifth, sixth)
happy_data <- bind_rows(fisix, seventh2) # bind three dfs at once
happy_data2 <- happy_data %>%
  arrange(desc(Happy_Score))
nrow(happy_data)

corHeat <- function(mat){
  as.data.frame(mat) %>%
    rownames_to_column("Var1") %>%
    gather(column, Happy_Score, -Var1) %>%
    ggplot(aes(ordered(Var1, levels = unique(Var1)), ordered(column,levels = rev(unique(column))))) + 
    geom_tile(aes(fill = Happy_Score),colour = "white") + 
    scale_fill_gradient2(low = "deepskyblue3", mid = "white", high = "violetred3")+
    theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=0.7))
}

cor_top <- function(nrows, mat){ # top n countries' correlation
  willusedata <- mat[1:nrows,]
  c_will <- cor(willusedata[,4:ncol(willusedata)])
  c_will#[,1]
}
cor_bottom <- function(nrows, mat){ # bottom n countries' correlation
  willusedata <- mat[(nrow(mat)-nrows):nrow(mat),]
  c_will <- cor(willusedata[,4:ncol(willusedata)])
  c_will
}

corHeat(cor_top(nrow(happy_data2), happy_data2)) # correlation heatmap of whole countries

top30 <- happy_data2 %>%
  filter(Happy_Rank <= 30)
corHeat(cor(top30[4:ncol(top30)])) # countries in rank 30

no_top30 <- happy_data2 %>%
  filter(Happy_Rank > 30)
corHeat(cor(no_top30[4:ncol(no_top30)])) # countries out rank 30

corHeat(cor_top(200, happy_data2))

corHeat(cor_bottom(200, happy_data2))

ggplot(happy_data, aes(Region, Happy_Score)) + geom_boxplot(aes(color = Region)) + # boxplot by region
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=0.7))

korea <- happy_data[happy_data$Country == "South Korea",]



# Only about economy
happyConomy <- earth(Happy_Score~Economy, 
                     data = happy_data[,4:ncol(happy_data)], ncross = 3, nfold = 10, pmethod = "cv")
plotmo(happyConomy, do.par = 2)
points(korea$Economy, korea$Happy_Score, cex = 3, pch = 19, col = "red")


# Only about family
happyFam <- earth(Happy_Score~Family, 
                     data = happy_data[,4:ncol(happy_data)], ncross = 3, nfold = 10, pmethod = "cv")
plotmo(happyFam, do.par = 2)
points(korea$Family, korea$Happy_Score, cex = 3, pch = 19, col = "red")


# Only about health
happyHealth <- earth(Happy_Score~Health, 
                     data = happy_data[,4:ncol(happy_data)], ncross = 3, nfold = 10, pmethod = "cv")
plotmo(happyHealth, do.par = 2)
points(korea$Health, korea$Happy_Score, cex = 3, pch = 19, col = "red")

# Only about free
happyFree <- earth(Happy_Score~Freedom, 
                     data = happy_data[,4:ncol(happy_data)], ncross = 3, nfold = 10, pmethod = "cv")
plotmo(happyFree, do.par = 2)
points(korea$Freedom, korea$Happy_Score, cex = 3, pch = 19, col = "red")

# Only about trust
happyTrust <- earth(Happy_Score~Trust, 
                     data = happy_data[,4:ncol(happy_data)], ncross = 3, nfold = 10, pmethod = "cv")
plotmo(happyTrust, do.par = 2)
points(korea$Trust, korea$Happy_Score, cex = 3, pch = 19, col = "red")

# Only about gene <== will not use this, because it doesn't show any relationship by scatterplot
happyGene <- earth(Happy_Score~Generosity, 
                     data = happy_data[,4:ncol(happy_data)], ncross = 3, nfold = 10, pmethod = "cv")
plotmo(happyGene, do.par = 2)
points(korea$Generosity, korea$Happy_Score, cex = 3, pch = 19, col = "red")

# Only about dystopia
happyDist <- earth(Happy_Score~Dystopia_Residual, 
                     data = happy_data[,4:ncol(happy_data)], ncross = 3, nfold = 10, pmethod = "cv")
plotmo(happyDist, do.par = 2)
points(korea$Dystopia_Residual, korea$Happy_Score, cex = 3, pch = 19, col = "red")

# Final
happiness <- earth(Happy_Score~Economy+Family+Health+Freedom+Trust,
                   data = fisix[,4:ncol(fisix)], ncross = 3, nfold = 10, pmethod = "cv")
plotmo(happiness)
plot(happiness)
summary(happiness)
evimp(happiness)
hpnss2017 <- predict(happiness, newdata = seventh2)
pred2017 <- bind_cols(pred = hpnss2017, real = seventh2$Happy_Score, Country = seventh2$Country,
                      Region = seventh2$Region)
p <- ggplot(data = pred2017, aes(x=real, y=pred, label=Country)) + geom_point(mapping = aes(color=Region)) + 
  geom_smooth(se=TRUE, color = "Gray")
ggplotly(p)
