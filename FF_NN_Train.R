train_nnet <- function(X, Y, learning_rate = 0.5, reg = 0.001, nodes = c(10), epochs = 7000)
{
  #X,Y represents training data, X are inputs (matrix, #Samples by #featsTypes), Y is output (matrix, #Samples by #Classes )
  #Learning Rate - defines the rate at which the weights change during training
  #reg - For regularization
  #nodes - number of nodes for each hidden layer and output layer( vector of size hidden_layers + 1)
  #hidden_layers - number of hidden layers to implement in NN
  #epochs - Number of training iterations
  
  in_nodes <- ncol(X) #Number of nodes in input layer
  classes <- ncol(Y) #number of classes for classification
  nsamples <- nrow(X)
  hidden_layers = length(nodes)
  
  #Define output layer
  W_out <- 0.01 * matrix(rnorm(nodes[length(nodes)]*classes), nrow = nodes[length(nodes)])
  b_out <- matrix(0, nrow = 1, ncol = classes)
  
  nodes <- c(nodes, c(in_nodes,classes))
  Weights <- list()
  Baises <- list()
  activations <- list()
  layer_outputs <- list()
  
  BPWeights <- list()
  BPBiases <- list()
  
  BPropErrors <- list()
  
  
  errors <- list()
  deltas <- list()
  
  #output of 'Input Layer'
  #layer_outputs[[1]] <- X  
  
  if (hidden_layers > 0 )
  {
    #DataFrame for weight matricies and backprop vectors for all hidden layers
    
    
    for (i in 1:hidden_layers)
    {
      name <- paste(c("W",i), collapse= "")
      if (i == 1)
      {
        
        num_nodes <- nodes[i]
        num_inputs <- in_nodes
        
        w_temp <- 0.01 * matrix(rnorm(num_inputs*num_nodes), nrow = num_inputs)
        b_temp <- matrix(0, nrow = 1, ncol = nodes)
        
        Weights[[i]] <- w_temp
        Baises[[i]] <- b_temp
        
        print("First Layer")
      }
      else
      {
        print("Layer XX")
        num_nodes <- nodes[i]
        num_inputs <- nodes[i-1]
        
        w_temp <- 0.01 * matrix(rnorm(num_inputs*num_nodes), nrow = num_inputs)
        b_temp <- matrix(0, nrow = 1, ncol = nodes)
        
        Weights[[i]] <- w_temp
        Baises[[i]] <- b_temp
        #print(colnames(Weights))
        #colnames(Weights) <- cbind(colnames(Weights), name)
        #colnames(Baises) <- cbind(colnames(Baises), name)
      }
    } #End Hidden layer gen forloop
  } #End Hidden Layer Gen if
  
  
  for (i in 0:epochs)
  {
    
    #j <- 0
    for (j in 1:hidden_layers)
    {
      #ReLU Activation function
      
      #print(matrix(rep(Baises[,c(i+1)],nsamples), nrow = nsamples, byrow = T))
      if( j == 1)
      {
        activations[[j]] <- pmax(0, X%*% Weights[[j]] + matrix(rep(Baises[[j]],nsamples), nrow = nsamples, byrow = T)) 
        layer_outputs[[j]] <- matrix(activations[[j]], nrow = nsamples)
      }
      else
      {
        activations[[j]] <- pmax(0, layer_outputs[[j-1]] %*% Weights[[j]] + matrix(rep(Baises[[j]],nsamples), nrow = nsamples, byrow = T)) 
        layer_outputs[[j]] <- matrix(activations[[j]], nrow = nsamples)
      }
     

    }#end layer calcs loop
    
    #compute output layer
    
    # calc output layer ==>  scores
    scores <- layer_outputs[[hidden_layers]]%*%W_out + matrix(rep(b_out,nsamples), nrow = nsamples, byrow = T)
    
    # compute and normalize class probabilities
    exp_scores <- exp(scores)
    probs <- exp_scores / rowSums(exp_scores)
    #print(probs[1:10,])
    
    correct_logprobs <- -log(probs)
    data_loss <- sum(correct_logprobs*Y)/nsamples
    #reg_loss <- 0.5*reg*sum(W_out*W_out) + 0.5*reg*sum(Weights[[hidden_layers]]*Weights[[hidden_layers]])
    loss <- data_loss
    
    
    if (i%%500 == 0 | i == epochs){
      print(paste("iteration", i,': loss', loss))}
    
    error <- probs-Y
    error_norm <- error/nsamples
    
    #print( error_norm[1,] )
    #print(error)
    
    
    dw_out <- t(layer_outputs[[hidden_layers]]) %*% error_norm
    dw_out <- dw_out + W_out*reg
    db_out <- colSums(error_norm)
    
    

    deltas[[hidden_layers+1]] <- error_norm
    Weights[[hidden_layers+1]] <- W_out

    for (j in hidden_layers:1)
    {
      
      if (j==1)
      {
        #print("First Layer")
        
        deltas[[j]] <- deltas[[j+1]]%*%t(Weights[[j+1]])
        deltas[[j]][layer_outputs[[j]] <= 0] <- 0
        
        BPWeights[[j]] <- t(X)%*%deltas[[j]]
        BPBiases[[j]] <- colSums(deltas[[j]])
        BPWeights[[j]] <- BPWeights[[j]] + reg*Weights[[j]]

        
      }
      else
      {
        

        deltas[[j]] <- deltas[[j+1]]%*%t(Weights[[j+1]])
        deltas[[j]][layer_outputs[[j]] <= 0] <- 0
        
        BPWeights[[j]] <- t(layer_outputs[[j]])%*%deltas[[j]]
        BPBiases[[j]] <- colSums(deltas[[j]])
        BPWeights[[j]] <- BPWeights[[j]] + reg*Weights[[j]]
        
      }
      
    }
    #print(Weights)
    W_out <- W_out-learning_rate*dw_out
    b_out <- b_out-learning_rate*db_out

    for (j in  1:hidden_layers)
    {
      
      Weights[[j]] <- Weights[[j]]-learning_rate*BPWeights[[j]]
      Baises[[j]] <- Baises[[j]]-learning_rate*BPBiases[[j]]
      
      
    }
    
    
    
  }#End training epoch loop
  
  
  
  return(list(Weights,Baises,W_out,b_out))
}#End Function
  