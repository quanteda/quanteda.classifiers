context("test tokens2sequences")

test_that("tokens2sequences works", {
    skip_on_cran()
    
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    text <- c("Chinese Beijing Chinese",
              "Chinese Chinese Shanghai",
              "Chinese Macao",
              "Tokyo Japan",
              "Chinese Chinese Chinese Tokyo Japan")
    text_tokens <- tokens(text)
    seq <- tokens2sequences(text_tokens, maxsenlen = 10, keepn = 5)
    
    expect_equal(dim(seq$matrix), c(5, 10))
    expect_equal(seq$nfeatures, 5)
})

test_that("tokens2sequences_conform works", {
    corpcoded <- corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
    corpuncoded <- data_corpus_manifestosentsUK %>%
        corpus_subset(is.na(crowd_immigration_label) & year > 1980) %>%
        corpus_sample(size = ndoc(corpcoded))
    
    tokx <- tokens(corpuncoded)
    toky <- tokens(corpcoded)
    
    seqx <- tokens2sequences(tokx, maxsenlen = 50, keepn = 5000)
    seqy <- tokens2sequences(toky, maxsenlen = 50, keepn = 5000)
    
    seqxy <- tokens2sequences_conform(seqx, seqy)
    expect_equal(dim(seqxy$matrix), c(7322, 50))
    expect_equal(ncol(seqxy$features), 3)
    print(seqxy)
})
