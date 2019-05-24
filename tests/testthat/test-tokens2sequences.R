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
    expect_equal(seq$nterms, 3)
})
