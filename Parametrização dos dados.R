library(readxl)

#normalização dos dados#
Data <- read_excel("C:/Users/artur/OneDrive/TCC/Orange/Data_SOM organizado.xlsx", sheet = "Parametrizacao")
x<- Data$x  #a variavel será chamada de x (do tipo numérica), o BD q vou utilzar no R é o Data, ao colocar $ irá aparecer a coluna 
y<- Data$y
teor <-Data$teor
x<- (x-min(x))/(max(x) - min(x))
y<- (y-min(y))/(max(y)- min(y))
teor<- (teor-min(teor))/(max(teor)- min(teor))
dadosagrupados <- data.frame(x,y,teor,Data$peso) #juntando os dados, cria uma var nova, utiliza a função data.frame e junta todos

library(randomForest)
set.seed(100)
train <- sample(nrow(dadosagrupados), 0.7*nrow(dadosagrupados), replace = FALSE)
TrainSet <- dadosagrupados[train,]
ValidSet <- dadosagrupados[-train,]
summary(TrainSet)
summary(ValidSet)

model1 <- randomForest(Data.peso~ ., data = TrainSet, importance = TRUE, ntree= 100, mtry=2)
model1
predTrain <- data.frame(predict(model1, ValidSet))
m <- predict(model1, dadosagrupados)
t <- dadosagrupados [,-4]
t$pesos <-(m)
t<-data.frame (t)

library(ggplot2)
ggplot(predTrain,aes(x=predict.model1..ValidSet., y=ValidSet$Data.peso))+geom_point()


var<-((ValidSet$Data.peso) - (predTrain$predict.model1..ValidSet.))
mean(var)
h<- data.frame(var)
ggplot(h,aes(x=var))+geom_histogram()

res <- cor.test(ValidSet$Data.peso, predTrain$predict.model1..ValidSet., 
                method = "pearson")
res

res2 <- cor.test(ValidSet$Data.peso, predTrain$predict.model1..ValidSet., method="kendall")
res2

res3 <-cor.test(ValidSet$Data.peso, predTrain$predict.model1..ValidSet.,  method = "spearman")
res3

(res$estimate^2)
(res2$estimate^2)
(res3$estimate^2)

library(factoextra)
library(kohonen)
head(t)
sommap <- som(scale(t), grid = somgrid(9, 9, "hexagonal"),rlen = 1000)
plot(sommap, type="changes")
plot(sommap, type="count")
plot(sommap, type="dist.neighbours")
plot(sommap, type="codes")
fviz_nbclust(sommap$codes[[1]], kmeans, method = "wss", k.max = 50)
clust <- kmeans(sommap$codes[[1]],8)

plot(sommap, type = "codes", bgcol = rainbow(10)[clust$cluster], main = "Cluster Map")
add.cluster.boundaries(sommap, clust$cluster)
newData <- data.frame(Data,cluster = clust$cluster[sommap$unit.classif])
newData$cluster <- factor(newData$cluster)
grupos <- as.factor(sommap$unit.classif)
ggplot(newData, aes(x = x ,y = y, color = cluster)) + geom_point() + theme_classic() + scale_color_brewer(palette="Dark2") 

inverse <- 1/(newData$teor)

library(ggvoronoi)
ggplot(newData) + 
  geom_voronoi(aes(x,y, fill = cluster)) +
  geom_point(aes(x = x ,y = y, color = teor, size = peso)) + 
  stat_voronoi(aes(x,y),geom="path") +    
  theme_classic() + 
  scale_fill_brewer(palette="Dark2") 



library(plotly)
graf <- ggplot(newData) + 
  geom_voronoi(aes(x,y, fill = cluster)) +
  geom_point(aes(x = x ,y = y, color = teor, size = peso)) + 
  stat_voronoi(aes(x,y),geom="path") +    
  theme_classic() + 
  scale_fill_brewer(palette="Dark2")  
ggplotly(graf)


mean (newData$teor*newData$peso)

##############################################################Dados Walklaker #############################
DadosCarvao <- read_excel("C:/Users/artur/Desktop/TCC - Walklaker/Dados Walkelaker.xlsx", 
    na = "-99")
DadosCarvao <- DadosCarvao[,-4]

x<- DadosCarvao$x
y<- DadosCarvao$y
teor <- DadosCarvao$teor
x<- (x-min(x))/(max(x) - min(x))
y<- (y-min(y))/(max(y)- min(y))
teor<- (teor-min(teor))/(max(teor)- min(teor))
carvao <- data.frame(x,y,teor, DadosCarvao$peso)
n <- predict(model1, carvao)

mean(DadosCarvao$teor)
res4 <- cor.test(DadosCarvao$peso, n, 
                method = "pearson")
res4
ggplot(DadosCarvao) + geom_point(aes (x=x, y=y))
