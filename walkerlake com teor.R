library(readxl)
library (ggplot2)
t <- read_excel("C:/Users/artur/Desktop/TCC - Walklaker/Dados Walkelaker.xlsx", 
    na = "-99")
t <- t[,- (4:5)]

library(factoextra)
library(kohonen)
head(t)
sommap <- som(scale(t), grid = somgrid(9, 9, "hexagonal"),rlen = 1000)
plot(sommap, type="changes")
plot(sommap, type="count")
plot(sommap, type="dist.neighbours")
plot(sommap, type="codes")
fviz_nbclust(sommap$codes[[1]], kmeans, method = "wss", k.max = 50)
clust <- kmeans(sommap$codes[[1]],12)

plot(sommap, type = "codes", bgcol = rainbow(50)[clust$cluster], main = "Cluster Map")
add.cluster.boundaries(sommap, clust$cluster)
newData <- data.frame(t,cluster = clust$cluster[sommap$unit.classif])
newData$cluster <- factor(newData$cluster)
grupos <- as.factor(sommap$unit.classif)
ggplot(newData, aes(x = x ,y = y, color = cluster)) + geom_point() + theme_classic() 
+ scale_color_brewer(palette= "Paired") 

c<-data.frame(newData$cluster)

contagem <- as.data.frame(table(newData$cluster))
j <- data.frame(v1=1:470)
for(i in 1:12){
  j$v1[c$newData.cluster==i]<-1/(12*contagem[i,2])
}
data2 <- data.frame(t,j)
sum(data2$teor*(data2$v1))
mean(data2$teor)

table(j)

o <- read_excel("C:/Users/artur/OneDrive/TCC/Orange/Data_SOM organizado.xlsx", sheet = "Parametrizacao")
