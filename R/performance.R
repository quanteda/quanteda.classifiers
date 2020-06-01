# overall performance ----------

#' Performance statistics for prediction
#' 
#' @description Functions for computing performance statistics used for model
#'   evaluation.
#' 
#' @description `performance()` computes all of the following, which are also
#'   available via specific functions:
#'   
#' Given a 2 x 2 table with notation
#' 
#' \tabular{rcc}{ \tab Truth \tab \cr Predicted \tab Positive \tab
#' Negative \cr Positive \tab _A_ \tab _B_ \cr Negative \tab _C_ \tab _D_ \cr }
#' 
#' The metrics computed here are:
#' \itemize{
#'   \item{precision: }{\eqn{A / (A + B)}}
#'   \item{recall: }{\eqn{A / (A + C)}}
#'   \item{_F1_: }{\eqn{2 / (recall^{-1} + precision^{-1})}}
#'   \item{accuracy: }{\eqn{(A + D) / (A + B + C + D)}, or correctly predicted / all}
#'   \item{balanced_accuracy: }{mean(recall) for all categories}
#'  }
#' @param data a table of predicted by truth, or vector of predicted labels
#' @param truth vector of "true" labels, or if a table, `2` to indicate that the 
#'   "true" values are in columns, or `1` if in rows.
#' @param by_class logical; if `TRUE`, estimate performance score separately for
#'   each class, otherwise average across classes
#' @param ... not used
#' @return named list consisting of the selected measure(s), where each element
#'   is a scalar if `by_class = FALSE`, or a vector named by class if `by_class
#'   = TRUE`.
#' @references
#' Powers, D. (2007). "Evaluation: From Precision, Recall and F Factor to ROC,
#' Informedness, Markedness and Correlation." _Technical Report SIE-07-001_,
#' Flinders University.
#' @examples
#' ## Data in Table 2 of Powers (2007)
#' 
#' lvs <- c("Relevant", "Irrelevant")
#' tbl_2_1_pred <- factor(rep(lvs, times = c(42, 58)), levels = lvs)
#' tbl_2_1_truth <- factor(c(rep(lvs, times = c(30, 12)),
#'                           rep(lvs, times = c(30, 28))),               
#'                         levels = lvs)
#'                         
#' performance(tbl_2_1_pred, tbl_2_1_truth)
#' performance(tbl_2_1_pred, tbl_2_1_truth, by_class = FALSE)
#' performance(table(tbl_2_1_pred, tbl_2_1_truth), by_class = TRUE)
#' 
#' @export
performance <- function(data, truth, by_class = TRUE, ...) {
    UseMethod("performance")
}

#' @export
performance.default <- function(data, truth, by_class = TRUE, ...) {
    performance(build_table(data, truth), by_class = by_class)
}

#' @export
performance.table <- function(data, truth = 2, by_class = TRUE, ...) {
    data <- check_table(data, truth)
    result <- as.list(c(precision(data, by_class = by_class),
                        recall(data, by_class = by_class),
                        accuracy(data),
                        balanced_accuracy(data)))
    result <- c(result, f1_score(result))
    result[c("precision", "recall", "f1", "accuracy", "balanced_accuracy")]
}


# precision, recall, f1 ----------

#' @rdname performance
#' @export
#' @examples
#' precision(tbl_2_1_pred, tbl_2_1_truth)
#' 
precision <- function(data, truth, by_class = TRUE, ...) {
    UseMethod("precision")
}

#' @export
precision.default <- function(data, truth, by_class = TRUE, ...) {
    precision(build_table(data, truth), by_class = by_class)
}    

#' @export
precision.table <- function(data, truth = 2, by_class = TRUE, ...) {
    data <- check_table(data, truth)
    prec <- sapply(seq_along(diag(data)), 
                   function(x) diag(data)[x] / sum(data[x, ]))
    prec <- list(precision = prec)
    if (by_class) prec else sapply(prec, mean)
}


#' @rdname performance
#' @export
#' @examples
#' recall(tbl_2_1_pred, tbl_2_1_truth)
#' 
recall <- function(data, truth, by_class = TRUE, ...) {
    UseMethod("recall")
}

#' @export
recall.default <- function(data, truth, by_class = TRUE, ...) {
    recall(build_table(data, truth), by_class = by_class)
}   

#' @export
recall.table <- function(data, truth = 2, by_class = TRUE, ...) {
    data <- check_table(data, truth)
    prec <- sapply(seq_along(diag(data)), 
                   function(x) diag(data)[x] / sum(data[, x]))
    prec <- list(recall = prec)
    if (by_class) prec else sapply(prec, mean)
}
 
#' @rdname performance
#' @export
#' @examples
#' f1_score(tbl_2_1_pred, tbl_2_1_truth)
#' 
f1_score <- function(data, truth, by_class = TRUE, ...) {
    UseMethod("f1_score")
}

#' @export
f1_score.default <- function(data, truth, by_class = TRUE, ...) {
    f1_score(build_table(data, truth), by_class = by_class)
}

#' @export
f1_score.table <- function(data, truth = 2, by_class = TRUE, ...) {
    data <- check_table(data, truth)
    pr <- list(precision = precision(data, by_class = by_class),
               recall = recall(data, by_class = by_class))
    f1_score(pr)
}

#' @export
f1_score.list <- function(data, ...) {
    if (!all(c("precision", "recall") %in% names(data)))
        stop("list must contain both precision and recall")
    result <- list(f1 = apply(data.frame(data[c("precision", "recall")]), 1, 
                              function(y) 2 / sum(y^(-1))))
    if (length(result[[1]]) == 1) result[[1]] <- unname(result[[1]])
    result
}

# accuracy ----------

#' @rdname performance
#' @export
#' @examples
#' accuracy(tbl_2_1_pred, tbl_2_1_truth)
#' 
accuracy <- function(data, truth, ...) {
    UseMethod("accuracy")
}

#' @export
accuracy.default <- function(data, truth, ...) {
    accuracy(build_table(data, truth))
}

#' @export
accuracy.table <- function(data, truth = 2, ...) {
    data <- check_table(data, truth)
    list(accuracy = sum(diag(data)) / sum(data))
}

#' @rdname performance
#' @export
#' @examples
#' balanced_accuracy(tbl_2_1_pred, tbl_2_1_truth)
#' 
balanced_accuracy <- function(data, ...) {
    UseMethod("balanced_accuracy")
}

#' @export
balanced_accuracy.default <- function(data, truth, by_class = TRUE, ...) {
    balanced_accuracy(build_table(data, truth))
}

#' @export
balanced_accuracy.table <- function(data, truth = 2, ...) {
    data <- check_table(data, truth)
    rec <- recall(data, by_class = TRUE)
    balanced_accuracy(rec)
}

#' @export
balanced_accuracy.list <- function(data, ...) {
    if (! "recall" %in% names(data))
        stop("list must include recall")
    if (length(data[["recall"]]) < 2)
        stop("recall must be computed by class")
    list(balanced_accuracy = mean(unlist(data["recall"])))
}

# utility functions -------------

check_table <- function(data, truth) {
    if (!truth %in% c(1, 2))
        stop("truth must be 2 for columns or 1 for rows")
    if (!identical(rownames(data), colnames(data)))
        stop("predicted and truth values must have the same order and names")
    if (truth == 1) data <- t(data)
    data
}

build_table <- function(data, truth) {
    truth <- as.factor(truth)
    data <- factor(data, levels = levels(truth))
    table(data, truth)
}
