#' Cross-validate a fitted textmodel
#'
#' Cross-validate a fitted textmodel using _k_-fold cross-validation.
#' @param x a fitted textmodel
#' @param k number of folds
#' @inheritParams performance
#' @param verbose logical; if `TRUE`, output results to the console
#' @export
#' @examples
#' library("quanteda")
#' library("quanteda.textmodels")
#' dfmat <- tokens(data_corpus_moviereviews) |>
#'   dfm()
#' tmod <- textmodel_nb(dfmat, y = data_corpus_moviereviews$sentiment)
#' crossval(tmod, k = 5, by_class = TRUE)
#' crossval(tmod, k = 5, by_class = FALSE)
#' crossval(tmod, k = 5, by_class = FALSE, verbose = TRUE)
crossval <- function(x, k = 5, by_class = FALSE, verbose = FALSE) {
    UseMethod("crossval")
}

#' @importFrom groupdata2 fold
#' @importFrom quanteda dfm dfm_subset
#' @import quanteda.textmodels
#' @export
crossval.textmodel <- function(x, k = 5, by_class = FALSE, verbose = FALSE) {
    # create folds vector - many ways to do this, I chose something available
    folds <- fold(data.frame(doc_id = docnames(x)), k = k)[[".folds"]]

    # result list (could be a df, I'm old-fashioned though)
    results <- list()

    # loop across folds and refit model, add to results list
    for (i in seq_len(k)) {
        this_mod <- do.call(class(x)[1],
                            args = list(x = dfm_subset(x$x, folds != i),
                                        y = x$y[folds != i]))
        this_pred <- predict(this_mod, newdata = dfm_subset(x$x, folds == i),
                             type = "class")
        results <- c(results,
                     structure(list(c(performance(this_pred, x$y[folds == i]),
                                      list(obs = split(seq_len(ndoc(x)), folds)[[i]]))),
                               names = paste0("fold_", i)))
    }

    summ <- summarize_results(results)

    # this may not be the "correct" way to do it - here it averages across
    # class-specific averages.  Should we average across classes first within
    # folds and then average across folds?
    if (!by_class)
        summ <- apply(summ, 2, mean)

    if (verbose) {
        cat("Cross-validation:\n\nMean results for k =", k, "folds:\n\n")
        print(summ)
        invisible(summ)
    } else {
        summ
    }
}

# old-skool function to aggregate across a 3-D array
summarize_results <- function(x) {
    # remove the "obs"
    x <- lapply(x, function(y) y[-which(names(y) == "obs")])

    # make into a 3D array
    x_df <- lapply(x, data.frame)
    x_array <- array(unlist(x_df), dim <- c(dim(x_df[[1]]), length(x_df)),
                     dimnames = c(dimnames(x_df[[1]]), list(names(x_df))))

    apply(x_array, c(1, 2), mean)
}
