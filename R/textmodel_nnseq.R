#' sequential neural network model for text
#'
#' This function is a wrapper for a sequential neural network model with a single hidden layer
#' network with two layers, implemented in the \pkg{keras} package.
#' @param epochs number of iterations the model is run to fit weights to training data
#' @param units The number of network nodes used in the first layer of the
#'   sequential model
#' @param batch
#' @param dropout A floating variable bound between 0 and 1. It determines the
#'   rate at which units are dropped for the linear transformation of the
#'   inputs.
#' @param valsplit for each epoch, the training data is split into training and 
#'   validation data at a ratio determined by this parameter
#' @param optimizer optimizer used to fit model to training data, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param loss objective loss function, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param metrics metric used to train algorithm, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param verbose if set to true, output for each epoch will be provided
#' @param ... additional options passed to
#'   \code{\link[keras]{fit.keras.engine.training.Model}}
#' @keywords textmodel
#' @importFrom keras keras_model_sequential to_categorical
#' @importFrom keras layer_dense layer_activation layer_dropout compile fit
#' @export
#' @examples 
#' # need examples here
textmodel_nnseq <- function(x, y, seed = 17, 
                          epochs = 3, units = 512, batch = 32, dropout = .2, valsplit = .1,
                          metrics = "categorical_accuracy", loss = "categorical_crossentropy", optimizer = "adam", 
                          verbose = TRUE, 
                          ...) {
    set.seed(seed)
    x <- as.dfm(x)
    if (!sum(x)) stop(quanteda:::message_error("dfm_empty"))
    call <- match.call()
    
    # exclude NA in training labels
    x_train <- suppressWarnings(
        dfm_trim(x[!is.na(y), ], min_termfreq = .0000000001)
    )
    y_train <- y[!is.na(y)]
    
    # remove zero-variance features
    constant_features <- which(apply(x_train, 2, stats::var) == 0)
    if (length(constant_features)) x_train <- x_train[, -constant_features]
    
    # creating dummy matrix for multinomial classification 
    y_train <- as.numeric(as.factor(y_train)) - 1
    
    classes <- length(unique(y_train))
    
    y_train <- to_categorical(y_train, num_classes = classes)
    
    # define seqential neural network model
    model <- keras_model_sequential()
    model %>%
        layer_dense(units = units, input_shape = dim(x_train)[2]) %>%
        layer_activation(activation = "relu") %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = classes) %>%
        layer_activation(activation = "softmax")
    
    # compile model with optimization and lost metrics
    compile(model, loss = loss, optimizer = optimizer, metrics = metrics)
    
    # fit model to training data
    history <- fit(model, 
                   x_train, y_train,
                   batch_size = batch,
                   epochs = epochs,
                   verbose = as.numeric(verbose),
                   validation_split = valsplit
    )
    
    result <- list(
        x = x, y = y,
        seqfitted = model,
        call = call,
        weights = featnames(x_train)
    )
    class(result) <- c("textmodel_nnseq", "textmodel", "list")
    return(result)
}


#' Prediction from a fitted textmodel_nnseq object
#'
#' \code{predict.textmodel_nnseq()} implements class predictions from a fitted
#' sequential neural network model.
#' @param object a fitted \link{textmodel_nnseq} model
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make \code{newdata}'s feature set conformant to the model terms
#' @param ... not used
#' @return \code{predict.textmodel_nnseq} returns either a vector of class
#'   predictions for each row of \code{newdata} (when \code{type = "class"}), or
#'   a document-by-class matrix of class probabilities (when \code{type =
#'   "probability"}).
#' @seealso \code{\link{textmodel_nnseq}}
#' @keywords textmodel internal
#' @importFrom keras predict_classes predict_proba
#' @export
predict.textmodel_nnseq <- function(object, newdata = NULL,
                                  type = c("class", "probability"),
                                  force = TRUE,
                                  ...) {
    quanteda:::unused_dots(...)
    
    type <- match.arg(type)
    
    if (!is.null(newdata)) {
        data <- as.dfm(newdata)
    } else {
        data <- as.dfm(object$x)
    }
    
    # the seq_along is because this will have an added term "bias" at end if bias > 0
    model_featnames <- object$weights
    #if (object$bias > 0) model_featnames <- model_featnames[-length(model_featnames)]
    
    data <- if (is.null(newdata)) {
        suppressWarnings(quanteda:::force_conformance(data, model_featnames, force))
    } else {
        quanteda:::force_conformance(data, model_featnames, force)
    }
    
    if (type == "class") {
        pred_y <- predict_classes(object$seqfitted, # Was unable to convert as.matrix.csr to python object for predict_classes function
                                  x = data)
        pred_y <- as.character(factor(pred_y, 
                                      levels = 1:length(names(table(object$y))), 
                                      labels = names(table(object$y))))
        names(pred_y) <- docnames(data)
    } else if (type == "probability") {
        pred_y <- predict_proba(object$seqfitted, # Was unable to convert as.matrix.csr to python object for predict_classes function
                                  x = data)
        colnames(pred_y) <- names(table(object$y))
        names(pred_y) <- docnames(data)
    }
    
    pred_y
}

#' @export
#' @method print textmodel_nnseq
print.textmodel_nnseq <- function(x, ...) {
    layer_names <- gsub(pattern = "_\\d*", "", lapply(model$seqfitted$layers, function(x) x$name))
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        format(length(na.omit(x$y)), big.mark = ","), " training documents; ",
        format(length(x$weights), big.mark = ","), " fitted features",
        ".\n",
        "Structure: ", paste(layer_names, collapse = " -> "), "\n",
        sep = "")
}

#' summary method for textmodel_svm objects
#' @param object output from \code{\link{textmodel_svm}}
#' @param n how many coefficients to print before truncating
#' @param ... additional arguments not used
#' @keywords textmodel internal
#' @method summary textmodel_svm
#' @export
summary.textmodel_nnseq <- function(object, ...) {
    layer_names <- gsub(pattern = "_\\d*", "", lapply(model$seqfitted$layers, function(x) x$name))
    
    result <- list(
        "call" = object$call,
        "model structure" = paste(layer_names, collapse = " -> ")
    )
    as.summary.textmodel(result)
}

#' @export
#' @method print predict.textmodel_nnseq
print.predict.textmodel_nnseq <- function(x, ...) {
    print(unclass(x))
}
