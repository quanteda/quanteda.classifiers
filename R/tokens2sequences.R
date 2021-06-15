#' \[Experimental\] Convert quanteda tokens to keras sequences
#'
#' This function converts a \pkg{quanteda} [quanteda::tokens()] object
#' into a tokens sequence object as expected by some functions in the
#' \pkg{keras} package.
#' @param x [quanteda::tokens()] object
#' @param maxsenlen the maximum sentence length kept in output matrix
#' @param keepn the maximum number of features to keep
#' @param tolower converts text to the lower case
#' @param keep_beginning Boolean indicating whether tokens kept are 
#' those at the beginning of the text sequence or the end
#' @return [tokens2sequences()] The output matrix has a number of rows
#'   which represent each tokenized sentence input into the function and a
#'   number of columns determined by `maxsenlen`. The matrix contains a
#'   numeric code for every unique token kept (determined by `keepn`) and
#'   they are arranged in the same sequence indicated by the original
#'   [quanteda::tokens()] object.
#' @seealso [is.tokens2sequences()], [tokens2sequences_conform()]
#' @export
#' @examples
#' corp <- corpus_subset(data_corpus_inaugural, Year <= 1793)
#' corptok <- tokens(corp)
#' print(corp)
#' seqs <- tokens2sequences(corptok, maxsenlen = 200)
#' print(seqs)
tokens2sequences <- function(x, maxsenlen = 100, keepn = NULL, tolower = TRUE, keep_beginning = FALSE) {
    UseMethod("tokens2sequences")
}

#' @export
tokens2sequences.tokens <- function(x, maxsenlen = 100, keepn = NULL, tolower = TRUE, keep_beginning = FALSE) {
    stopifnot(is.tokens(x))
    if(tolower) x <- lapply(x, function(y) tolower(y)) %>% 
            tokens()
    tfeq <- sort(table(unlist(x)), decreasing = T) # Creates a table of tokens and their frequencies sorted from most common to least
    doc_nam <- docnames(x) # Store docnames from tokens object for future use
    x <- unclass(x) # Convert tokens to integer IDs
    features <- attr(x, "types") # Store feature names
    data <- data.frame(features = features, # Create a dataframe that maps each token to its id and frequency
                       label1 = 1:length(features),
                       freq1 = as.integer(tfeq[features]),
                       stringsAsFactors = FALSE)
    attributes(x) <- NULL
    out <- remove_features(x = x, data = data, maxsenlen = maxsenlen, keepn = keepn, keep_beginning = keep_beginning)
    data <- out$data
    keep_tokens <- data$label1[-which.max(data$label)]
    x <- lapply(x, function(y) y[y %in% keep_tokens])
    if(keep_beginning) {
        x <- lapply(x, function(y) if(length(y) > maxsenlen) y[1:maxsenlen] else y)
        } else {
            x <- lapply(x, function(y) if(length(y) > maxsenlen) y[(length(y) - maxsenlen + 1):length(y)] else y)
            }
    words <- data.frame(table(unlist(x)))
    names(words) <- c("label1", "freq")
    data <- merge(data, words, by = "label1", all.x = TRUE)

    x <- lapply(x, function(y) as.integer(na.omit(out$mapping[y]))) # Assign new, frequency-based IDs to word sequence list
    mat <- do.call("rbind", lapply(x, function(y) {
        if (length(y) < maxsenlen) {y = c(rep(0L, times = maxsenlen - length(y)), y)} # Adds zeros to ensure an even number of rows across word sequences and binds into a single data frame
        return(y)
    }
    ))
    rownames(mat) <- doc_nam # Adds docname to each row of the matrix
    colnames(mat) <- as.character(1:maxsenlen) # Adds a numeric label to each column
    dropped_tokens <- 1 - sum(data$freq, na.rm = TRUE) / sum(tfeq, na.rm = T)
    dropped_types <- 1 -  nrow(data) / nrow(tfeq)
    #data <- data[!is.na(data$label), ] # Removes words that were not assigned numeric ids from the dictionary
    data <- data[order(data$label, decreasing = FALSE),
                 c("features", "label", "freq")] # selects feature names, ids, and frequency for dictionary and orders by frequency-based ID
    rownames(data) <- NULL # Resets rownames of dictionary
    output <- list(matrix = mat, nfeatures = nrow(data), features = data, dropped_types = dropped_types, dropped_tokens = dropped_tokens)
    class(output) <- "tokens2sequences"
    return(output)
}

#' @export
tokens2sequences.character <- function(x, maxsenlen = 100, keepn = NULL, tolower = TRUE, keep_beginning = FALSE) {
    stopifnot(is.character(x))
    if(tolower) x <- tolower(x)
    x <- tokens(x)
    tfeq <- sort(table(unlist(x)), decreasing = T) # Creates a table of tokens and their frequencies sorted from most common to least
    doc_nam <- docnames(x) # Store docnames from tokens object for future use
    x <- unclass(x) # Convert tokens to integer IDs
    features <- attr(x, "types") # Store feature names
    data <- data.frame(features = features, # Create a dataframe that maps each token to its id and frequency
                       label1 = 1:length(features),
                       freq1 = as.integer(tfeq[features]),
                       stringsAsFactors = FALSE)
    attributes(x) <- NULL
    out <- remove_features(x, data, maxsenlen, keepn, keep_beginning = keep_beginning)
    data <- out$data
    keep_tokens <- data$label1[-which.max(data$label)]
    x <- lapply(x, function(y) y[y %in% keep_tokens])
    if(keep_beginning) {
        x <- lapply(x, function(y) if(length(y) > maxsenlen) y[1:maxsenlen] else y)
        } else {
            x <- lapply(x, function(y) if(length(y) > maxsenlen) y[(length(y) - maxsenlen + 1):length(y)] else y)
            }
    words <- data.frame(table(unlist(x)))
    names(words) <- c("label1", "freq")
    data <- merge(data, words, by = "label1", all.x = TRUE)

    x <- lapply(x, function(y) as.integer(na.omit(out$mapping[y]))) # Assign new, frequency-based IDs to word sequence list
    mat <- do.call("rbind", lapply(x, function(y) {
        if (length(y) < maxsenlen) {y = c(rep(0L, times = maxsenlen - length(y)), y)} # Adds zeros to ensure an even number of rows across word sequences and binds into a single data frame
        return(y)
    }
    ))
    rownames(mat) <- doc_nam # Adds docname to each row of the matrix
    colnames(mat) <- as.character(1:maxsenlen) # Adds a numeric label to each column
    dropped_tokens <- 1 - sum(data$freq, na.rm = TRUE) / sum(tfeq, na.rm = T)
    dropped_types <- 1 -  nrow(data) / nrow(tfeq)
    data <- data[order(data$label, decreasing = FALSE),
                 c("features", "label", "freq")] # selects feature names, ids, and frequency for dictionary and orders by frequency-based ID
    rownames(data) <- NULL # Resets rownames of dictionary
    output <- list(matrix = mat, nfeatures = nrow(data), features = data, dropped_types = dropped_types, dropped_tokens = dropped_tokens)
    class(output) <- "tokens2sequences"
    return(output)
}

#' @export
tokens2sequences.tokens2sequences <- function(x, maxsenlen = 100, keepn = NULL, tolower = TRUE, keep_beginning = FALSE) {
    stopifnot(is.tokens2sequences(x))
    doc_nam <- rownames(x$matrix) # Store docnames from tokens object for future use
    data <- x$features
    names(data)[names(data) %in% c("label", "freq")] <- c('label1', "freq1")
    x <- x$matrix
    colnames(x) <- NULL
    x <- lapply(1:nrow(x), function(y) {
        j <- x[y, ]
        return(j[j != 0])
    })
    tfeq <- sort(table(unlist(x)), decreasing = T) # Creates a table of tokens and their frequencies sorted from most common to least
    out <- remove_features(x, data, maxsenlen, keepn, keep_beginning)
    data <- out$data
    keep_tokens <- data$label1[-which.max(data$label)]
    
    x <- lapply(x, function(y) y[y %in% keep_tokens])
    if(keep_beginning) {
        x <- lapply(x, function(y) if(length(y) > maxsenlen) y[1:maxsenlen] else y)
        } else {
            x <- lapply(x, function(y) if(length(y) > maxsenlen) y[(length(y) - maxsenlen + 1):length(y)] else y)
            }
    words <- data.frame(table(unlist(x)))
    names(words) <- c("label1", "freq")
    data <- merge(data, words, by = "label1", all.x = TRUE)
    
    x <- lapply(x, function(y) as.integer(na.omit(out$mapping[y]))) # Assign new, frequency-based IDs to word sequence list
    mat <- do.call("rbind", lapply(x, function(y) {
        if (length(y) < maxsenlen) {y = c(rep(0L, times = maxsenlen - length(y)), y)} # Adds zeros to ensure an even number of rows across word sequences and binds into a single data frame
        return(y)
    }
    ))
    rownames(mat) <- doc_nam # Adds docname to each row of the matrix
    colnames(mat) <- as.character(1:maxsenlen) # Adds a numeric label to each column
    dropped_tokens <- 1 - sum(data$freq, na.rm = TRUE) / sum(tfeq, na.rm = T)
    dropped_types <- 1 -  nrow(data) / nrow(tfeq)
    data <- data[order(data$label, decreasing = FALSE),
                 c("features", "label", "freq")] # selects feature names, ids, and frequency for dictionary and orders by frequency-based ID
    rownames(data) <- NULL # Resets rownames of dictionary
    output <- list(matrix = mat, nfeatures = nrow(data), features = data, dropped_types = dropped_types, dropped_tokens = dropped_tokens)
    class(output) <- "tokens2sequences"
    return(output)
}

#' @seealso [tokens2sequences()]
#' @export
#' @importFrom utils head
#' @method print tokens2sequences
print.tokens2sequences <- function(x, removed = FALSE, ...) {
    # calculate % sparse
    zeros <- sum(colSums(x$matrix == 0))
    tot <- nrow(x$matrix) * ncol(x$matrix)
    sparse_pct <- round(zeros / tot * 100, 1)

    # determine max number of features to print
    max_n <- ifelse(ncol(x$matrix) > 10, 10, ncol(x$matrix))

    # output
    cat("Ordered feature matrix of: ", format(nrow(x$matrix), big.mark = ","),
        " documents, ", format(x$nfeatures, big.mark = ","), " features ",
        "(", sparse_pct, "% sparse).\n\n", sep = "")
    #cat(nrow(x$matrix), " x ", ncol(x$matrix),
    #    " Matrix of class \"tokens2sequences\" \n\n", sep = "")
    if(removed) cat("Current parameter settings resulted in the removal of", round(100*x$dropped_types, 1), "percent of types\nand", round(100*x$dropped_tokens, 1), "percent of tokens.\n\n")
    print(head(x$matrix[, 1:max_n], 4))
}

#' Match the feature names of one tokens2sequences object to another
#'
#' Converts the feature names of one tokens2sequences object to those of
#' another.  Useful in aligning training and test sets.
#' @param x [tokens2sequences()] object that will be forced to conform
#' @param y [tokens2sequences()] object whose feature names will be
#'   used to change token labels for `x`
#' @seealso [tokens2sequences()]
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' corpcoded <- corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
#' corpuncoded <- data_corpus_manifestosentsUK %>%
#'     corpus_subset(is.na(crowd_immigration_label) & year > 1980) %>%
#'     corpus_sample(size = ndoc(corpcoded))
#'
#' tokx <- tokens(corpuncoded)
#' toky <- tokens(corpcoded)
#'
#' seqx <- tokens2sequences(tokx, maxsenlen = 50, keepn = 5000)
#' seqy <- tokens2sequences(toky, maxsenlen = 50, keepn = 5000)
#' tokens2sequences_conform(seqx, seqy)
#' }
tokens2sequences_conform <- function(x, y) {
    UseMethod("tokens2sequences_conform")
}

#' @export
#' @importFrom stats na.omit
tokens2sequences_conform.tokens2sequences <- function(x, y) {
    if("tokens2sequences" %in% class(y)){
        joint_feat <- merge(x$features, y$features[, -3], by = "features",
                        all.x = TRUE)
        joint_feat <- joint_feat[order(joint_feat$label.x, decreasing = FALSE), ]      
    } else if("textmodel_cnnlstmemb" %in% class(y)){
        joint_feat <- merge(x$features, y$features[, -3], by = "features",
                        all.x = TRUE)
        joint_feat <- joint_feat[order(joint_feat$label.x, decreasing = FALSE), ]       
    } else {
        stop("Object y should be either a tokens2sequences or a textmodel_cnnlstmemb object\n")
    }
    x <- x$matrix
    mat <- lapply(1:nrow(x), function(y) {
        j <- x[y, ]
        return(j[j != 0])
    })
    mat <- lapply(mat, function(y) as.integer(na.omit(joint_feat$label.y[y])))
    mat <- do.call("rbind", lapply(mat, function(y)
        if (length(y) >= ncol(x))
            y[1:ncol(x)]
        else
            c(rep(0L, times = ncol(x) - length(y)), y)
    ))
    rownames(mat) <- rownames(x)
    colnames(mat) <- colnames(x)
    joint_feat <- joint_feat[, c("features", "label.y", "freq")]
    names(joint_feat)[2] <- "label"
    joint_feat <- joint_feat[order(joint_feat$label, decreasing = FALSE), ]
    joint_feat <- joint_feat[!is.na(joint_feat$label), ]
    joint_feat$freq[is.na(joint_feat$freq)] <- 0
    rownames(joint_feat) <- NULL

    output <-
        list(matrix = mat, nfeatures = nrow(joint_feat), features = joint_feat)
    class(output) <- c("tokens2sequences")
    return(output)
}

#' Check to see if function is a tokens2sequences type
#'
#'
#' @param x Object that will be checked to see if it is of the type [tokens2sequences()]
#' @seealso [tokens2sequences()]
#' @keywords internal
is.tokens2sequences <- function(x) {
    "tokens2sequences" %in% class(x)
}

#' Function that applies feature reduction to data set
#' @param x List of token IDs
#' @param data Data frame that maps features to their IDs
#' @param maxsenlen the maximum sentence length kept in output matrix
#' @param keepn the maximum number of features to keep
#' @param keep_beginning Boolean indicating whether tokens kept are 
#' those at the beginning of the text sequence or the end
remove_features <- function(x, data, maxsenlen, keepn, keep_beginning){
    data <- data[order(data$freq1, decreasing = TRUE), ] # Reorders feature dictionary by frequency
    data$label <- NA
    if (!is.null(keepn)) {
        if (keepn > nrow(data)) keepn <- nrow(data) # Makes sure that we are not attempting to keep more features than exist
        if(keep_beginning) {
            x1 <- lapply(x, function(y) if(length(y) > maxsenlen) y[1:maxsenlen] else y)
            x2 <- lapply(x, function(y) if(length(y) > maxsenlen) y[(maxsenlen + 1):length(y)] else y)            
        } else {
            x1 <- lapply(x, function(y) if(length(y) > maxsenlen) y[(length(y) - maxsenlen + 1):length(y)] else y)
            x2 <- lapply(x, function(y) if(length(y) > maxsenlen) y[1:(length(y) - maxsenlen)] else y)
        }
        x1_unique = unique(unlist(x1))
        x2_unique = unique(unlist(x2))
        x2_x1 = setdiff(x2_unique, x1_unique)
        data$main_list <- as.integer(!(data$label1 %in% x2_x1))
        data <- data[order(data$main_list, data$freq1, decreasing = TRUE), ]
        data$label[1:keepn] <- 1:keepn # Subsets tokens to include only the n most common
        mapping <- data[order(data$label1, decreasing = FALSE), ]$label
        data <- data[1:keepn, ]
        
    } else {
        data$label <- 1:nrow(data)
        mapping <- data[order(data$label1, decreasing = FALSE), ]$label
    }
    return(list(data = data, mapping = mapping))
}

#' Subset a tokens2sequences object using index numbers or a Boolean vector
#' @param x [tokens2sequences()] object that will be subsetted
#' @param indexes [tokens2sequences()] range of indexes or logical values indicating rows of tokens2sequences object that will be kept
#' @export
tokens2sequences_subset <- function(x, indexes) {
    stopifnot(is.tokens2sequences(x))
    stopifnot(class(indexes) %in% c("integer", "logical"))
    if(is.logical(indexes)) indexes <- which(indexes)
    stopifnot(length(setdiff(indexes, 1:nrow(x$matrix))) == 0)
    doc_nam <- rownames(x$matrix) # Store docnames from tokens object for future use
    data <- x$features
    maxsenlen <- ncol(x$matrix)
    data$freq <- NULL
    x <- x$matrix[indexes, ]
    doc_nam <- doc_nam[indexes]
    x <- lapply(1:nrow(x), function(y) {
        j <- x[y, ]
        return(j[j != 0])
    })
    mat <- do.call("rbind", lapply(x, function(y) {
        if (length(y) < maxsenlen) {y = c(rep(0L, times = maxsenlen - length(y)), y)} # Adds zeros to ensure an even number of rows across word sequences and binds into a single data frame
        return(y)
    }
    ))
    rownames(mat) <- doc_nam # Adds docname to each row of the matrix
    colnames(mat) <- as.character(1:maxsenlen) # Adds a numeric label to each column
    words <- data.frame(table(unlist(x)))
    names(words) <- c("label", "freq")
    data <- merge(data, words, by = "label", all.x = TRUE)
    data <- data[order(data$label, decreasing = FALSE),
                 c("features", "label", "freq")] # selects feature names, ids, and frequency for dictionary and orders by frequency-based ID
    rownames(data) <- NULL # Resets rownames of dictionary
    output <- list(matrix = mat, nfeatures = nrow(data), features = data)
    class(output) <- "tokens2sequences"
    return(output)
}


#' Convert a matrix and label dictionary to a [tokens2sequences()] object
#' @param mat A matrix containing numeric representations of tokens of dimensions n x k, where n is the document level and k represents the maximum 
#' sequence length
#' @param dict A data frame with one numeric column representing 
#' @export
tokens2sequences_convert <- function(mat, dict) {
    UseMethod("tokens2sequences_convert")
}

#' @export
tokens2sequences_convert.matrix <- function(mat, dict) {
    stopifnot(is.matrix(mat))
    stopifnot(is.data.frame(dict))
    stopifnot(ncol(dict) == 2)
    stopifnot("character" %in% sapply(dict, class))
    stopifnot("numeric" %in% sapply(dict, class) | "integer" %in% sapply(dict, class))
    doc_nam <- rownames(mat) # Store docnames from tokens object for future use
    if(is.null(doc_nam)) doc_nam <- 1:nrow(mat)
    # Extracting feature and label columns
    features <- dict[,sapply(dict, class) == "character"]
    labels <- dict[,sapply(dict, class) %in% c("numeric", "integer")]
    
    stopifnot(sum(!(unique(c(matrix)) %in% labels)) <= 1)
    tfeq <- sort(table(c(mat)), decreasing = T)
    data <- data.frame(features = features, # Create a dataframe that maps each token to its id and frequency
                       label = labels,
                       freq = as.integer(tfeq[as.character(labels)]),
                       stringsAsFactors = FALSE)
    data <- data[order(data$label, decreasing = FALSE),
                 c("features", "label", "freq")]
    rownames(mat) <- doc_nam # Adds docname to each row of the matrix
    colnames(mat) <- as.character(1:ncol(mat)) # Adds a numeric label to each column
    output <- list(matrix = mat, nfeatures = nrow(data), features = data)
    class(output) <- "tokens2sequences"
    return(output)
}

#' Returns docnames from an tokens2sequences or textmodel_cnnlstmemb object
#' @param x output from [tokens2sequences()]
#' @param ... additional arguments not used
#' @keywords textmodel
#' @method docnames tokens2sequences
#' @importFrom quanteda docnames
#' @export
docnames.tokens2sequences <- function(x, ...) {
    return(rownames(x$matrix))
}

#' @export
docnames.textmodel_cnnlstmemb <- function(x, ...) {
    return(rownames(x$x$matrix))
}