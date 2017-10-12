library(ggplot2)
library(caret)
source("DataCreation.R")
N <- 200
D <- 2
K <- 4

dataTrain <- createData()
X1 <- as.matrix(dataTrain[,1:2])
Y1 <- matrix(0, N*K, K)

for (i in 1:(N*K))
 {
    Y1[i, dataTrain[["label"]][i]] <- 1
 }

N <- 100
dataTest <- createData( N = N )
X2 <- as.matrix(dataTest[,1:2])
Y2 <- matrix(0, N*K, K)

for (i in 1:(N*K))
 {
    Y2[i, dataTest[["label"]][i]] <- 1
 }

source("FF_NN_Train.R")

 nnet.model <- train_nnet(X1,Y1,learning_rate = 0.3, reg = 0.0001, nodes = c(10,10), epochs = 500)
 
 predicted_class <- nnetPred2(X2, nnet.model,hidden_layers=2)
 print(paste('training accuracy:',mean(predicted_class == (dataTest[,3]))))
 
 #plot the resulting classifier
 hs <- 0.01
 x_min <- min(X1[,1])-0.2; x_max <- max(X1[,1])+0.2
 y_min <- min(X1[,2])-0.2; y_max <- max(X1[,2])+0.2
 
 grid <- as.matrix(expand.grid(seq(x_min, x_max, by = hs), seq(y_min, y_max, by =hs)))
 Z <- nnetPred2(grid, nnet.model)
 
 plot <- ggplot()+ geom_tile(aes(x = grid[,1],y = grid[,2],fill=as.character(Z)), alpha = 0.3, show.legend = F)+
   geom_point(data = dataTrain, aes(x=x, y=y, color = as.character(label)), size = 2) +
   xlab('X') + ylab('Y') + ggtitle('Neural Network Decision Boundary') +
   scale_color_discrete(name = 'Label') + coord_fixed(ratio = 0.6) +
   theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         axis.text=element_blank(),legend.position = 'none')
 
 print(plot)