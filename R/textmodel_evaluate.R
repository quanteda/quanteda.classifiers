#' Model evaluation function
#'
#' Designed to streamline the parameter tuning and evaluation process. Users
#' chose a function to evaluate and include parameter values as a list. If
#' multiple parameter values are provided, the function will perform a grid
#' search by estimating a separate model for every combination of parameters.
#'
#' @param x the \link{dfm} or \link{tokens} object on which the model will be
#'   fit.  Does not need to contain only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in \code{train}.  (These will be converted to factors if not already
#'   factors.)
#' @param model the name of the machine learning function that will be evaluated
#' @param fun the name of the function that will be used to evaluate the machine
#'   learning model. Can take the values "accuracy", "precision", "recall", or
#'   "f1_score"
#' @param k number of folds
#' @param parameters model hyperparameters
#' @param seed a seed that can allow for replication of k training data splits.
#'   If seed is not provided a seed is chosen based on the current time.
#' @param time a logical parameter that determines whether output will include
#'   training time (in seconds) of model
#' @param by_class estimates a separate value of provided evaluation function
#'   for every class of the true vector
#' @importFrom stats predict
#' @examples
#' # evaluate immigration classification performance
#' \dontrun{
#' dfmat <- dfm(data_corpus_manifestosentsUK)
#' codes <- docvars(data_corpus_manifestosentsUK, "crowd_immigration_label")
#' evaluation <- textmodel_evaluate(dfmat, codes, k = 3,
#'                                  model = "textmodel_mlp", fun = "f1_score",
#'                                  parameters = list(epochs = c(3, 4)))
#' head(evaluation)
#' aggregate(evaluation, by = list(evaluation$cost), FUN = "mean")
#' }
#' @export
#'
textmodel_evaluate <- function(x, y,
                               model,
                               fun = "f1_score",
                               k = 5,
                               parameters = list(),
                               seed = as.numeric(Sys.time()),
                               time = TRUE,
                               by_class = FALSE) {
    UseMethod("textmodel_evaluate")
}

#' @export
textmodel_evaluate.dfm <- function(x, y, model, fun = "f1_score", k = 5,
                                   parameters = list(),
                                   seed = as.numeric(Sys.time()),
                                   time = TRUE, by_class = FALSE) {
    stopifnot(is.dfm(x))
    if ("accuracy" %in% fun & by_class) {
        cat("No class oriented accuracy score defined. Calculating average accuracy accross all classes.\n")
    }
    if(is.tokens2sequences(x)) x <- x$matrix
    total_start <- Sys.time()
    set.seed(seed)
    y <- as.factor(y)
    folds <- cut(seq(1, length(y)), breaks = k, labels = FALSE)
    folds <- sample(folds, length(folds), replace = FALSE)
    output <- list()
    params_df <- expand.grid(parameters, stringsAsFactors = FALSE)
    param_len <- ifelse(length(parameters) != 0, nrow(params_df), 1)
    w <- 1
    for (t in 1:param_len) {
        # drop = FALSE ensures that params_df remains a data.frame even if
        # there is only a single input parameter
        param_list <- as.list(params_df[t, , drop = FALSE])
        for (i in 1:k) {
            test_set <- which(folds == i)
            x_train <- x[-test_set, ]
            y_train <- y[-test_set]
            x_test <- x[test_set, ]
            y_test <- y[test_set]
            start <- Sys.time()
            mod <- do.call(what = model, args = c(list(x = x_train, y = y_train), param_list))
            time <- round(as.numeric(difftime(Sys.time(), start, units = "secs")), 2) # Outputs time in seconds
            names(time) <- "time"
            y_pred <- predict(mod, x_test)
            met <- lapply(fun, function(x) do.call(what = x, args = list(y_pred, y_test, by_class))) # Accepts any evaluation function that takes predicted and test vectors as inputs
            #met <- as.list(met)
            if ( is.null(names(met))) names(met) <- fun
            if (length(parameters) != 0) {
                output[[w]] <- data.frame(k = i, met, param_list, as.list(time), seed)
            } else {
                output[[w]] <- data.frame(k = i, met, as.list(time), seed)
            }
            if (by_class) {
                output[[w]]$class <- rownames(output[[w]])
            }
            w <- w + 1
        }
    }
    output <- do.call(rbind, output)
    rownames(output) <- NULL
    total_time <- round(as.numeric(difftime(Sys.time(), total_start, units = "secs")), 2) # Outputs time in seconds
    attr(output, "model") <- model
    attr(output, "fun") <- fun
    attr(output, "k") <- k
    attr(output, "parameters") <- parameters
    attr(output, "nparameters") <- param_len
    attr(output, "total_time") <- total_time
    class(output) <- c("textmodel_evaluate", "data.frame")
    return(output)
}

#' @export
textmodel_evaluate.tokens <- function(x, y, model, fun = "f1_score", k = 5,
                                      parameters = list(),
                                      seed = as.numeric(Sys.time()),
                                      time = TRUE, by_class = FALSE) {
    stopifnot(is.tokens(x))
    if ("accuracy" %in% fun & by_class) {
        cat("No class oriented accuracy score defined. Calculating average accuracy accross all classes.\n")
    }
    total_start <- Sys.time()
    set.seed(seed)
    y <- as.factor(y)
    folds <- cut(seq(1, length(y)), breaks = k, labels = FALSE)
    folds <- sample(folds, length(folds), replace = FALSE)
    output <- list()
    params_df <- expand.grid(parameters, stringsAsFactors = FALSE)
    param_len <- ifelse(length(parameters) != 0, nrow(params_df), 1)
    w <- 1
    for (t in 1:param_len) {
        param_list <- as.list(params_df[t, , drop = FALSE]) # drop = FALSE ensures that params_df remains a data.frame even if there is only a single input parameter
        for (i in 1:k) {
            test_set <- which(folds == i)
            x_train <- x[-test_set]
            y_train <- y[-test_set]
            x_test <- x[test_set]
            y_test <- y[test_set]
            start <- Sys.time()
            mod <- do.call(what = model, args = c(list(x = x_train, y = y_train), param_list))
            time <- round(as.numeric(difftime(Sys.time(), start, units = "secs")), 2) # Outputs time in seconds
            names(time) <- "time"
            y_pred <- predict(mod, x_test)
            met <- lapply(fun, function(x) do.call(what = x, args = list(y_pred, y_test, by_class))) # Accepts any evaluation function that takes predicted and test vectors as inputs
            #met <- as.list(met)
            if (is.null(names(met))) names(met) <- fun
            if (length(parameters) != 0) {
                output[[w]] <- data.frame(k = i, met, param_list, as.list(time), seed)
            } else {
                output[[w]] <- data.frame(k = i, met, as.list(time), seed)
            }
            if (by_class) {
                output[[w]]$class <- rownames(output[[w]])
            }
            w <- w + 1
        }
    }
    output <- do.call(rbind, output)
    rownames(output) <- NULL
    total_time <- round(as.numeric(difftime(Sys.time(), total_start, units = "secs")), 2) # Outputs time in seconds
    attr(output, "model") <- model
    attr(output, "fun") <- fun
    attr(output, "k") <- k
    attr(output, "parameters") <- parameters
    attr(output, "nparameters") <- param_len
    attr(output, "total_time") <- total_time
    class(output) <- c("textmodel_evaluate", "data.frame")
    return(output)
}

#' @seealso [textmodel_evaluate()]
#' @importFrom utils head
#' @method head textmodel_evaluate
#' @export
#'
head.textmodel_evaluate <- function(x, n = 5, ...) {
    return(head(as.data.frame(x), n))
}

#' @seealso [textmodel_evaluate()]
#' @method print textmodel_evaluate
#' @export
print.textmodel_evaluate <- function(x, ...) {
    # output
    cat("Evaluation of", attr(x, "model"), "using the", attr(x, "fun"), "function.",
        "\n")
    #return(head(x, 4))
}
