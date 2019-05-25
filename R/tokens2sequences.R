#' convert tokens to sequences for quanteda.classifiers lstm model input
#'
#' This function converts a \pkg{quanteda} \link[quanteda]{tokens} object into a \link{tokens2sequence} object. 
#' 
#' 
#' @param x tokens object
#' @param maxsenlen the maximum sentence length kept in output matrix
#' @param keepn the maximum number of features to keep
#'
#' @return \code{tokens2sequences} The output matrix has a 
#' number of rows which represent each tokenized sentence input into the function
#' and a number of columns determined by \code{maxsenlen}. The matrix contains a numeric 
#' code for every unique token kept (determined by \code{keepn})
#' and they are arranged in the same sequence indicated by the original tokens object. 
#' @export
#'
#' @examples
#' \dontrun{
#' corpcoded <- corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
#' corpuncoded <- data_corpus_manifestosentsUK %>%
#'     corpus_subset(is.na(crowd_immigration_label) & year > 1980) %>%
#'     corpus_sample(size = ndoc(corpcoded))
#' corp <- corpcoded + corpuncoded
#' 
#' corptok <- tokens(texts(corp))
#' print(corp)
#' seqs <- tokens2sequences(corptok, maxsenlen = 200)
#' print(seqs)
#' }
#' 
tokens2sequences <- function(x, maxsenlen = 40, keepn = NULL) {
    UseMethod("tokens2sequences")
}

tokens2sequences.tokens <- function(x, maxsenlen = 40, keepn = NULL) {
    tfeq <- sort(table(unlist(x)), decreasing = T)
    doc_nam <- docnames(x)
    x <- unclass(x)
    features <- attr(x, "types")
    data <- data.frame(features = features, label1 = 1:length(features), freq = as.integer(tfeq[features]), stringsAsFactors = F)
    attributes(x) <- NULL
    data <- data[order(data$freq, decreasing = T), ]
    if (!is.null(keepn)) {
        data$label <- NA
        data$label[1:keepn] <- 1:keepn

    } else {
        data$label <- 1:nrow(data)
    }
    data <- data[order(data$label1, decreasing = F), ]
    x <- lapply(x, function(y) as.integer(na.omit(data$label[y])))
    mat <- do.call("rbind", lapply(x, function(y) if(length(y) >= maxsenlen) {y[1:maxsenlen]} else {c(rep(0,times = maxsenlen - length(y)), y)}))
    rownames(mat) <- doc_nam
    colnames(mat) <- as.character(1:maxsenlen)
    data <- data[!is.na(data$label), ]
    data <- data[order(data$label, decreasing = F), c("features", "label", "freq")]
    rownames(data) <- NULL
    output <- list(matrix = mat, nfeatures = nrow(data), features = data)
    class(output) <- c("tokens2sequences", "list")
    return(output)
}


#' @seealso \code{\link{tokens2sequences}}
#' @export
#' @method print tokens2sequences
print.tokens2sequences <- function(x, ...) {
    # calculate % sparse
    zeros <- sum(colSums(x$matrix == 0))
    tot <- nrow(x$matrix) * ncol(x$matrix)
    sparse_pct <- round(zeros/tot * 100, 1)
    
    # determine max number of features to print
    max_n <- ifelse(ncol(x$matrix) > 10, 10, ncol(x$matrix))
    
    # output
    cat("Ordered feature matrix of: ", format(nrow(x$matrix), big.mark = ","), " documents, ", format(x$nfeatures, big.mark = ","), " features ", "(", sparse_pct, "% sparse).\n", sep = "")
    cat(nrow(x$matrix), " x ", ncol(x$matrix), " Matrix of class \"tokens2sequences\" \n", sep = "")
    head(x$matrix[, 1:max_n], 4)
}

#' Converts the feature names of one text2sequences object to that of another
#'
#' 
#' @param x tokens2sequence object that will be forced to conform
#' @param y tokens2sequence object whose feature names will be used to change token labels for \code{x}
#' @seealso \code{\link{tokens2sequences}}
#' @export
#' @method t2s_conform tokens2sequences
#' @examples 
#' \dontrun{
#' corpcoded <- corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
#' corpuncoded <- data_corpus_manifestosentsUK %>%
#'     corpus_subset(is.na(crowd_immigration_label) & year > 1980) %>%
#'     corpus_sample(size = ndoc(corpcoded))
#' 
#' tokx <- tokens(texts(corpuncoded))
#' toky <- tokens(texts(corpcoded))
#' 
#' seqx <- tokens2sequences(tokx, maxsenlen = 50, keepn = 5000)
#' seqy <- tokens2sequences(toky, maxsenlen = 50, keepn = 5000)
#' 
#' seqxy <- t2s_conform(seqx, seqy)
#' 
#' print(seqxy)
#' }
#' 

t2s_conform <- function(x, y) {
    UseMethod("t2s_conform")
}

t2s_conform.tokens2sequences <- function(x, y) {
    
    joint_feat <- merge(x$features, y$features[, -3], by = "features", all.x = T)
    joint_feat <- joint_feat[order(joint_feat$label.x, decreasing = F), ]
    mat <- apply(x$matrix, 1, function(x) as.integer(na.omit(joint_feat$label.y[x])))
    mat <- do.call("rbind", lapply(mat, function(y) if(length(y) >= ncol(x$matrix)) {y[1:ncol(x$matrix)]} else {c(rep(0,times = ncol(x$matrix) - length(y)), y)}))
    rownames(mat) <- rownames(x$matrix)
    colnames(mat) <- colnames(x$matrix)
    joint_feat <- joint_feat[, c("features", "label.y", "freq")]
    names(joint_feat)[2] <- "label"
    joint_feat <- joint_feat[order(joint_feat$label, decreasing = F), ]
    joint_feat <- joint_feat[!is.na(joint_feat$label), ]
    rownames(joint_feat) <- NULL
    
    output <- list(matrix = mat, nterms = nrow(joint_feat), features = joint_feat)
    class(output) <- c("tokens2sequences", "list")
    return(output)
}
