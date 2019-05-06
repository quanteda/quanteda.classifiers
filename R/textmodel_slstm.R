#' stacked LSTM neural network model for text
#'
#' This function is a wrapper for a stacked Long Short-Term Memory (LSTM) neural
#' network with two layers, implemented in the \pkg{keras} package.
#' @inheritParams textmodel_svm
#' @param units The number of network nodes used in the first layer of the
#'   sequential model
#' @param dropout A floating variable bound between 0 and 1. It determines the
#'   rate at which units are dropped for the linear transformation of the
#'   inputs.
#' @param optimizer optimizer used to fit model to training data, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param loss objective loss function, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param metrics metric used to train algorithm, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param ... additional options passed to
#'   \code{\link[keras]{fit.keras.engine.training.Model}}
#' @keywords textmodel
#' @importFrom keras keras_model_sequential to_categorical
#' @importFrom keras layer_dense layer_activation layer_dropout compile fit
#' @export
#' @examples 
#' # need examples here
textmodel_slstm <- function(x, y, units = 512, dropout = .2, 
                          optimizer = "adam",
                          loss = "categorical_crossentropy", 
                          metrics = "categorical_accuracy", 
                          ...) {
    
    if (dim(x)[1] != length(y)) {
        stop("The length of x and y are not the same.")
    }
    y2 <- as.numeric(as.factor(y))
    
    na_ind <- which(is.na(y2))
    
    if (length(na_ind) > 0) {
        cat(length(na_ind), "observations with the value 'NA' were removed.")
        y2 <- y2[-na_ind]
        x <- x[-na_ind]
    }
    
    classes <- length(unique(y2)) + 1
    
    y2 <- to_categorical(y2, num_classes = classes)
    #  test_y <- to_categorical(as.numeric(con_test_y), num_classes = classes)
    
    model <- keras_model_sequential()
    model %>%
        layer_dense(units = units, input_shape = dim(x)[2]) %>%
        layer_activation(activation = "relu") %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = classes) %>%
        layer_activation(activation = "softmax")
    
    compile(model, loss = loss, optimizer = optimizer, metrics = metrics)
    history <- fit(x, y2, ...)
    return(model)
}

#' Prediction from a fitted textmodel_slstm object
#'
#' \code{predict.textmodel_slstm()} implements class predictions from a fitted
#' sequential neural network model.
#' @param object a fitted \link{textmodel_slstm} model
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make \code{newdata}'s feature set conformant to the model terms
#' @param ... not used
#' @return \code{predict.textmodel_slstm} returns either a vector of class
#'   predictions for each row of \code{newdata} (when \code{type = "class"}), or
#'   a document-by-class matrix of class probabilities (when \code{type =
#'   "probability"}).
#' @seealso \code{\link{textmodel_slstm}}
#' @keywords textmodel internal
#' @importFrom keras predict_classes predict_proba 
#' @export
predict.textmodel_slstm <- function(object, newdata = NULL,
                                  type = c("class", "probability"),
                                  force = TRUE,
                                  ...) {
    quanteda:::unused_dots(...)
    
    type <- match.arg(type)
    
    if (!is.null(newdata)) {
        data <- as.dfm(newdata)
    } else {
        stop("New data required to make prediction.")
    }
    
    if (type == "class") {
        pred_y <- predict_classes(object = object, x = data)
        pred_y <- as.character(pred_y)
        names(pred_y) <- docnames(data)
    } else if (type == "probability") {
        pred_y <- predict_proba(object = object, x = data)
        #pred_y <- pred_y$probabilities
        rownames(pred_y) <- docnames(data)
    }
    
    pred_y
}
