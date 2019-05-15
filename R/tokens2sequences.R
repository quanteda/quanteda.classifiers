#' convert tokens to sequences for keras word2vec input
#'
#' More description here, with links to appropriate \pkg{keras} functions.
#' @param x 
#'
#' @return explain return format
#' @export
#'
#' @examples
#' # add one
tokens2sequences <- function(x) {
    UseMethod("tokens2sequences")
}

tokens2sequences.tokens <- function(x) {
    x <- unclass(x)
    attributes(x) <- NULL
    x_freqs <- table(unlist(unclass(x)))
    x_freqs_ordered <- temp[order(temp, decreasing = TRUE)]
    lapply(x, function(y) as.integer(names(x_freqs_ordered))[y])
}
