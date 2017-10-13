# Neural_Networks_R

Simple R implementations of Machine Learning Techniques Using R.
Focusing on Neural Networks. 

From Scratch implementations - Not using predefined libraries, to enhance understanding of how these techniques work.


Current Implementations:

 1. Feed Forward Neural Network (FF_NN_Train.R )
      - Using ReLU Activation function
      - Softmax Loss Function
      - Variable hidden layers and nodes (Currently does not support 0 hidden layers, being fixed)
      - L2 regularization implemented
      - Variable Iterations (epochs) (No threshold setting for Loss function to end training interation)
      - Re-implementation of https://www.kaggle.com/russwill/build-your-own-neural-network-in-r ( this implementation uses hard coded hidden layer)
      - Pre-requistes for running the Code
         - R + RStudio (or any IDE supporting R)
         - qqplot, caret ( both available through CRAN ) for plotting outputs, decision boundaries
         - No libraries needed for running the training/prediction algorithms using the implemented Neural Net
      
      - Simply Run TestScript.R to use the neural network, it has already been set up to generate a spiral train and test data set of 4 classes, all that needs to be changed is the parameters of the neural network : learning rate, nodes in hidden layers (minimum 1 hidden layer), regularization factor and epochs (training iterations)
      
