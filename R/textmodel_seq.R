#' Neural Network with Two Layers
#' 
#' This function is a wrapper for a two layered neural network written using the Keras Package. It takes a 
#' @param x The data that will be used as training and test data.
#' @param y The outcome that will be used as outcomes to be predicted by the NN model.
#' @param Seed The seed used in the model. Defaults to 17
#' @param Weighting The type of feature weighting used in the document feature matrix. I.e., count and tfidf.
#' @param Epochs The number of epochs used in the NN model.
#' @param Units The number of network nodes used in the first layer of the sequential model
#' @param Batch The number of batches estimated
#' @param Dropout A floating variable bound between 0 and 1. It determines the rate at which units are dropped for the linear tranformation of the inputs.
#' @param ValSplit The validation split of the data used in the training of the LSTM model
#' @param Metric Metric used to train algorithm
#' @param Loss Metric used to train algorithm
#' @param Optimizer Optimizer used to fit model to training data
#' @importFrom magrittr %>%
#' @importFrom caret confusionMatrix
#' @keywords neural networks
#' @export

textmodel_seq <- function(x, y, Seed = 17, 
                        Epochs = 3, Units = 512, Batch = 32, Dropout = .2, Valsplit = .1,
                        Metric = "categorical_accuracy",Loss = "categorical_crossentropy", Optimizer = "adam", 
                        Verbose = TRUE){
  
  set.seed(Seed)
  
  v <- ifelse(Verbose, 1, 0)
  
  if(dim(x)[1] != length(y)) {
    cat("The length of x and y are not the same.")
    break
  }
  y2 <- as.numeric(as.factor(y))
    
  classes <- length(unique(y2)) + 1
    
  y2 <- to_categorical(y2, num_classes = classes)
#  test_y <- to_categorical(as.numeric(con_test_y), num_classes = classes)
  
  model <- keras_model_sequential() 
  model %>%
    layer_dense(units = Units, input_shape = dim(x)[2]) %>% 
    layer_activation(activation = 'relu') %>% 
    layer_dropout(rate = Dropout) %>% 
    layer_dense(units = classes) %>% 
    layer_activation(activation = 'softmax')
  
  model %>% compile(
    loss = Loss,
    optimizer = Optimizer,
    metrics = Metric
  )
  
  history <- model %>% fit(
    x, y2,
    batch_size = Batch,
    epochs = Epochs,
    verbose = v,
    validation_split = Valsplit
  )
  return(model)
}
