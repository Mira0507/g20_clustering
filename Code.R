library(tidyverse)
library(ggplot2)
library(cluster)
library(dendextend)
library(stringr)
library(randomForest)

h <- head    
s <- summary
g <- glimpse        

# df: a master data frame
df <- read_csv("Gender_StatsData.csv")
g20 <- c('AUS', 'CAN', 'USA', 'SAU', 'IND', 'RUS', 'ZAF', 'TUR', 'ARG',
         'BRA', 'MEX', 'FRA', 'DEU', 'ITA', 'GBR', 'IDN', 'JPN', 'KOR', 'CHN') 

# primary data cleaning
names(df) <- c("Country_Name", "Country_Code",
                "Indicator_Name", "Indicator_Code",
                as.character(1960:2020))

df1 <- df %>%
        select(-"2020") %>%
        gather(key = Year, 
               value = Value, 
               -c("Country_Name", "Country_Code",
                  "Indicator_Name", "Indicator_Code"))

indicators_gdp <- df1$Indicator_Name[str_detect(df1$Indicator_Name, "GDP")]
my_indicator <- c("GDP per capita (Current US$)",
                       "GDP (current US$)")

df2 <- df1 %>%
        filter(Indicator_Name %in% my_indicator,
               Country_Code %in% g20,
               Year == "2017")
df3 <- spread(df2, Indicator_Name, Value)
names(df3) <- c("Country_Name", "Country_Code", "Indicator_Code",
                "Year", "Total_GDP", "GDP_per_capita")

df4 <- data.frame(Country = unique(df3$Country_Name),
                  Total_GDP = na.omit(df3$Total_GDP),
                  GDP_per_capita = na.omit(df3$GDP_per_capita)) %>%
        
        # recaling total GDP and GDP per capita
        mutate(rescaled_total_GDP = (Total_GDP - mean(Total_GDP)) / sd(Total_GDP),
               rescaled_GDP_per_capita = (GDP_per_capita - mean(GDP_per_capita)) / sd(GDP_per_capita))

df5 <- df4[ , 4:5]
df6 <- df5
rownames(df6) <- df4$Country



# Elbow analysis
tot_withinss <- map_dbl(1:10, function(k) {
        model <- kmeans(x = df5, centers = k)
        model$tot.withinss})
elbow_df <- data.frame(k = 1:10, tot = tot_withinss)

# Silhouette analysis
sil_width <- map_dbl(2:10, function(k) {
        model <- pam(x = df5, k = k)
        model$silinfo$avg.width})
sil_df <- data.frame(k = 2:10,
                     sil_width = sil_width)

# Hierchical clustering: df7
dist_gdp <- dist(df6, method = "euclidean")
hc_gdp <- hclust(dist_gdp, method = "average")
df7 <- df6 %>%
        mutate(hcluster_k3 = cutree(hc_gdp, k = 3),
               hcluster_k4 = cutree(hc_gdp, k = 4))

# kmeans clustering: df7
df7$kmcluster_k3 <- kmeans(df6, centers = 3, nstart = 20)$cluster
df7$kmcluster_k4 <- kmeans(df6, centers = 4, nstart = 20)$cluster

# inner-join: df8
df8 <- df7 %>% 
        inner_join(df4, 
                   by = c("rescaled_total_GDP", 
                          "rescaled_GDP_per_capita")) %>%
        mutate(hcluster_k3 = factor(hcluster_k3),
               hcluster_k4 = factor(hcluster_k4),
               kmcluster_k3 = factor(kmcluster_k3),
               kmcluster_k4 = factor(kmcluster_k4))

# data cleaning for cluster table of kmeans clustering
df9 <- data.frame(Country = df8$Country, 
                  kmeans_k3 = df8$kmcluster_k3, 
                  kmeans_k4 = df8$kmcluster_k4)
                               
# dendrogram 
dend_gdp <- as.dendrogram(hc_gdp)
dend_colored_k3 <- color_branches(dend_gdp, k = 3)
dend_colored_k4 <- color_branches(dend_gdp, k = 4)

dendrogram_k3 <- plot(dend_colored_k3, 
                      main = "Hierarchical Clustering with k = 3",
                      ylab = "Distance")
dendrogram_k4 <- plot(dend_colored_k4, 
                      main = "Hierarchical Clustering with k = 4",
                      ylab = "Distance")
# analysis plots 
elbow_plot <- 
        ggplot(elbow_df, aes(x = k, y = tot)) + 
        geom_line(size = 1.5, color = "red") + 
        scale_x_continuous(breaks = 1:10) +
        ggtitle("Elbow Plot When k = 1 to 10") +
        ylab("Total Width Cluster Sum of Squares") + 
        theme_classic() + 
        theme(axis.text = element_text(size = 10))

sil_plot <-
        ggplot(sil_df, aes(x = k, y = sil_width)) +
        geom_line(size = 1.5, color = "blue") + 
        scale_x_continuous(breaks = 2:10) +
        ggtitle("Silhouette Plot When k = 2 to 10") +
        ylab("Silhouette Width") + 
        theme_classic() +
        theme(axis.text = element_text(size = 10))

# plotting function 
clust_fn <- function(df, tit, cluster) {
        ggplot(df, aes(x = rescaled_total_GDP, 
                       y = rescaled_GDP_per_capita,
                       color = cluster)) +
                geom_point(size = 3, alpha = 0.6) +
                theme_classic() +
                ggtitle(tit) + 
                xlab("Rescaled Total GDP (US dollars)") + 
                ylab("Rescaled GDP per Capita (US Collars)")
}

# hierachical clustering plots 
hc_k3_plot <- clust_fn(df8, 
                       "Hierarchical Clustering with k = 3", 
                       df8$hcluster_k3)
hc_k4_plot <- clust_fn(df8, 
                       "Hierarchical Clustering with k = 4", 
                       df8$hcluster_k4)

# k-means clustering plots
kmc_k3_plot <- clust_fn(df8,
                        "K-means Clustering with k = 3",
                        df8$kmcluster_k3)
kmc_k4_plot <- clust_fn(df8, 
                        "K-means Clustering with k = 4", 
                        df8$kmcluster_k4)

# grid 
library(gridExtra)
analysis_plot_grid <- grid.arrange(elbow_plot,
                                   sil_plot,
                                   nrow = 1)
clustering_plot_grid <- grid.arrange(hc_k3_plot,
                                     hc_k4_plot,
                                     kmc_k3_plot,
                                     kmc_k4_plot,
                                     nrow = 2)

# kmeans cluster table 
library(formattable)

km_table <- formattable(df9)
#be0b50
#1a930a
#1795cb