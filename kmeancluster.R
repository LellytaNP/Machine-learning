library(readr) #membaca data
library(dplyr) #meproses data
library(DT)
#clustering
#asean_demografi
asean_data <- read.csv('E:\\datasetbelajar\\asean_demografi.csv')
datatable(asean_data,caption = 'indikator demografi negara asean 2015')
data_standardized <- round(scale(asean_data[,2:5]),4)
datatable(asean_data,caption = 'indikator demografi negara ASEAN')
jumlah_klaster <- c(1:9)
within_ss <-c()
?kmeans
for( i in jumlah_klaster){
  within_ss <- c(within_ss,
                 kmeans(x=data_standardized,centers = i, nstart = 25)$tot.withinss)
}
within_ss

plot(x=jumlah_klaster, y=within_ss,type = 'b',
     xlab = 'number of cluster',
     ylab = ' total within sum of square',
     main='eblow plot')

abline(v=4, col= 'red')

set.seed(24)
help('kmeans')
kmeans_clustering <-
  kmeans(x=data_standardized, centers = 4, nstart = 25)
kmeans_clustering$centers
kmeans_clustering$size
library(magrittr)
library(dplyr)
asean_data %>%
  mutate(klaster = kmeans_clustering$cluster) %>%
  group_by(klaster) %>%
  summarise(Mean_TFR = mean(TFR), Mean_AKB = mean(AKB),
            Mean_AKI = mean(AKI), Mean_AHH = mean(AHH))

asean_data %>%
  mutate(klaster = kmeans_clustering$cluster) %>%
  select(Negara, klaster) %>%
  arrange(klaster)
