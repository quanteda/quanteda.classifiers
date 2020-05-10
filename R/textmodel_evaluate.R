#' Model evaluation function
#' 
#' Designed to streamline the parameter tuning and evaluation process. 
#' Users chose a function to evaluate and include parameter values as a list.
#' If multiple parameter values are provided, the function will perform a grid search by estimating a separate
#' model for every combination of parameters.
#' 
#' @param x the \link{dfm} or \link{tokens} object on which the model will be fit.  Does not need to
#'   contain only the training documents.
#' @param y vector of training labels associated with each document identified 
#'   in \code{train}.  (These will be converted to factors if not already 
#'   factors.)
#' @param model the name of the machine learning function that will be evaluated
#' @param fun the name of the function that will be used to evaluate the machine learning model. 
#' For example, accuracy, precision, recall, or f1_score
#' @param k number of folds
#' @param parameters model hyperparameters
#' @param seed a seed that can allow for replication of k training data splits. 
#' If seed is not provided a seed is chosen based on the current time.
#' @param time a logical parameter that determines whether output will include training
#' time (in seconds) of model
#' @importFrom stats predict
#' @examples
#' # evaluate immigration classification performance
#' \dontrun{
#' dfmat <- dfm(data_corpus_manifestosentsUK)
#' codes <- docvars(data_corpus_manifestosentsUK, "crowd_immigration_label")
#' evaluation <- textmodel_evaluate(dfmat, codes, k = 3, model = "textmodel_mlp", fun = "f1_score",
#'   parameters = list(epochs = c(3, 4)))
#' 
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
                               time = TRUE) {
    UseMethod("textmodel_evaluate")
}

#' @export
textmodel_evaluate.dfm <- function(x, y, model, fun = "f1_score", k = 5, parameters = list(), seed = as.numeric(Sys.time()), time = TRUE) {
    total_start <- Sys.time()
    set.seed(seed)
    y <- as.factor(y)
    folds <- cut(seq(1,length(y)),breaks=k, labels=FALSE)
    folds <- sample(folds, length(folds), replace = FALSE)
    output <- list()
    params_df <- expand.grid(parameters, stringsAsFactors = FALSE)
    param_len <- ifelse(length(parameters) != 0, nrow(params_df), 1)
    w <- 1
    for(t in 1:param_len) {
        param_list <- as.list(params_df[t, , drop = FALSE]) # drop = FALSE ensures that params_df remains a data.frame even if there is only a single input parameter
        for(i in 1:k){
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
            met <- do.call(what = fun, args = list(y_pred, y_test)) # Accepts any evaluation function that takes predicted and test vectors as inputs
            met <- as.list(met)
            if(is.null(names(met))) {names(met) <- fun}
            if(length(parameters) != 0){
                output[[w]] <- data.frame(k = i, met, param_list, as.list(time), seed)
            } else {
                output[[w]] <- data.frame(k = i, met, as.list(time), seed)
            }
            w <- w + 1
        }
    }
    output <- do.call(rbind, output)
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

#' F1-score
#'
#' `f1_score()` Implements a function that calculates the harmonic mean of precision and recall of predicted labels and a vector of true values. 
#' Its values range from 0 to 1, where 0 would be a harmonic mean in which one or both of precision or recall are 0 and 1 where both parameters are 1.
#' @param pred vector of predicted labels derived from some model, such as \link{textmodel_mlp}, 
#' which is being subjected to evaluation
#' @param true a vector of known labels that are used to evaluate model performance
#' @seealso [textmodel_evaluate()]
#' @export

f1_score <- function(pred, true){
    true <- as.factor(true)
    pred <- factor(pred, levels = levels(true))
    CMat <- table(pred, true)
    if(length(levels(true)) == 2) {
        r <- CMat[1, 1] / sum(CMat[, 1])
        p <- CMat[1, 1] / sum(CMat[1, ])
        f1 <- ifelse(r + p != 0, 2 * (r * p) / (r + p), 0)
    } else{ 
        diag_vec <- 1:length(diag(CMat))
        r <- p <- f1 <-  vector(length = length(diag_vec))
        for(k in diag_vec) {
            r[k] <- ifelse(sum(CMat[,k]) != 0, diag(CMat)[k] / sum(CMat[, k]), 0)
            p[k] <- ifelse(sum(CMat[k, ]) != 0, diag(CMat)[k] / sum(CMat[k, ]), 0)
            f1[k] <- ifelse(r[k] + p[k] != 0, 2 * (r[k] * p[k]) / (r[k] + p[k]), 0)
        }
        f1 <- mean(f1)
    }
    out <- f1
    names(out) <- c("f1_score")
    return(out)
}

#' Precision
#'
#' `precision()` Implements a function that calculates the precision, also known as the positive predictive value, of labels predicted by a machine learning model and a vector of true values. 
#' It is calculated by calculating the ratio of true predicted positives to total predicted positives. Its values range from 0 to 1, where 0 would indicate that no predicted positives were true positives
#' and 1 would indicate that all predicted positives were true positives.
#' @param pred vector of predicted labels derived from some model, such as \link{textmodel_mlp}, 
#' which is being subjected to evaluation
#' @param true a vector of known labels that are used to evaluate model performance
#' @seealso [textmodel_evaluate()]
#' @export

precision <- function(pred, true){
    true <- as.factor(true)
    pred <- factor(pred, levels = levels(true))
    CMat <- table(pred, true)
    if(length(levels(true)) == 2) {
        p <- CMat[1, 1] / sum(CMat[1, ])
    } else {
        diag_vec <- 1:length(diag(CMat))
        p <-  vector(length = length(diag_vec))
        for(k in diag_vec) {
            p[k] <- ifelse(sum(CMat[k, ]) != 0, diag(CMat)[k] / sum(CMat[k, ]), 0)
        }
        p <- mean(p)
    }
    out <- p
    names(out) <- c("precision")
    return(out)
}

#' Recall
#'
#' `recall()` Implements a function that calculates the recall, also known as the sensitivity, of labels predicted by a machine learning model and a vector of true values. 
#' It is calculated by calculating the ratio of true predicted positives to total true positives. Its values range from 0 to 1, where 0 would indicate that no true positives 
#' were predicted by the model while 1 would indicate that all true positives in the data were identified.
#' @param pred vector of predicted labels derived from some model, such as \link{textmodel_mlp}, 
#' which is being subjected to evaluation
#' @param true a vector of known labels that are used to evaluate model performance
#' @seealso [textmodel_evaluate()]
#' @export

recall <- function(pred, true){
    true <- as.factor(true)
    pred <- factor(pred, levels = levels(true))
    CMat <- table(pred, true)
    if(length(levels(true)) == 2) {
        r <- CMat[1, 1] / sum(CMat[, 1])
    } else {
        diag_vec <- 1:length(diag(CMat))
        r <- vector(length = length(diag_vec))
        for(k in diag_vec) {
            r[k] <- ifelse(sum(CMat[,k]) != 0, diag(CMat)[k] / sum(CMat[, k]), 0)
        }
        r <- mean(r)
    }
    out <- r
    names(out) <- c("recall")
    return(out)
}

#' Accuracy
#'
#' `accuracy()` Implements a function that calculates the proportion of positive and negative values that have been correctly identified by a machine learning model and a vector of true values. 
#' It is calculated by calculating the quotient of the total correctly identified labels and the total number of labels. Its values range from 0 to 1, where 0 would indicate that no predicted labels were correct
#' and 1 would indicate that all predicted labels were correct.
#' @param pred vector of predicted labels derived from some model, such as \link{textmodel_mlp}, 
#' which is being subjected to evaluation
#' @param true a vector of known labels that are used to evaluate model performance
#' @seealso [textmodel_evaluate()]
#' @export

accuracy <- function(pred, true){
    true <- as.factor(true)
    pred <- factor(pred, levels = levels(true))
    CMat <- table(pred, true)
    out <- sum(diag(CMat)) / sum(CMat)
    names(out) <- c("accuracy")
    return(out)
}