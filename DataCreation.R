createData <- function(N = 200, D = 2, K = 4)
{
#N <- 200 # number of points per class
#D <- 2 # dimensionality
#K <- 4 # number of classes

   X <- data.frame() # data matrix (each row = single example)
   y <- data.frame() # class labels
  
  set.seed(308) # For repeatability of results
  
  for (j in (1:K)){
    r <- seq(0.05,1,length.out = N) # radius
    t <- seq((j-1)*4.7,j*4.7, length.out = N) + rnorm(N, sd = 0.3) # theta
    Xtemp <- data.frame(x =r*sin(t) , y = r*cos(t)) 
    ytemp <- data.frame(matrix(j, N, 1))
    X <- rbind(X, Xtemp)
    y <- rbind(y, ytemp)
  }
  
  data <- cbind(X,y)
  colnames(data) <- c(colnames(X), 'label')
  
  return(data)
}