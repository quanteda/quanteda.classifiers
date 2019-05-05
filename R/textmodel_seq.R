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
  
  na_ind <- which(is.na(y2))
  
  if(length(na_ind) > 0) {
    cat(length(na_ind),"observations with the value 'NA' were removed.")
    y2 <- y2[-na_ind]
    x <- x[-na_ind]
    }
  
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

#' Prediction from a fitted textmodel_seq object
#' 
#' \code{predict.textmodel_seq()} implements class predictions from a fitted
#' SEQ model.
#' @param object a fitted linear SEQ textmodel 
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make newdata's feature set conformant to the model terms
#' @param ... not used
#' @return \code{predict.textmodel_seq} returns either a vector of class
#'   predictions for each row of \code{newdata} (when \code{type = "class"}), or
#'   a document-by-class matrix of class probabilities (when \code{type =
#'   "probability"}).
#' @seealso \code{\link{textmodel_seq}}
#' @keywords textmodel internal
#' @importFrom SparseM as.matrix.csr
#' @export
predict.textmodel_seq <- function(object, newdata = NULL, 
                                  type = c("class", "probability"), 
                                  force = TRUE,
                                  ...) {
  quanteda:::unused_dots(...)
  
  type <- match.arg(type)
  
  if (!is.null(newdata)) {
    data <- as.dfm(newdata)
  } else {
    cat("New data required to make prediction.")
    break
  }
  
  
  if (type == "class") {
    pred_y <- predict_classes(object = object, 
                              x = data)
    pred_y <- as.character(pred_y)
    names(pred_y) <- docnames(data)
  } else if (type == "probability") {
    pred_y <- predict_proba(object = object, 
                              x = data)
    #pred_y <- pred_y$probabilities
    rownames(pred_y) <- docnames(data)
  }
  
  pred_y
}
