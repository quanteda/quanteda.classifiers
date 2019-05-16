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

tokens2sequences.tokens <- function(x, maxsenlen = 40) {
    x <- unclass(x)
    attributes(x) <- NULL
    x_freqs <- table(unlist(unclass(x)))
    x_freqs_ordered <- x_freqs[order(x_freqs, decreasing = TRUE)]
    x <- lapply(x, function(y) as.integer(names(x_freqs_ordered))[y])
    do.call("rbind", lapply(x, function(y) if(length(y) >= maxsenlen) {y[1:maxsenlen]} else {c(rep(0,times = maxsenlen - length(y)), y)}))
}
