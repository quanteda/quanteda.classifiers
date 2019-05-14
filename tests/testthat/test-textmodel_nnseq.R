context("test textmodel_nnseq")

test_that("the nnseq model works", {
    skip_on_cran()

    ## Example from 13.1 of _An Introduction to Information Retrieval_
    text <- c("Chinese Beijing Chinese",
              "Chinese Chinese Shanghai",
              "Chinese Macao",
              "Tokyo Japan",
              "Chinese Chinese Chinese Tokyo Japan")
    text <- rep(text, times = 25)
    train <- c("Y", "Y", "Y", "N", NA)
    train <- rep(train, times = 25)
    test <- c("Y", "Y", "Y", "N", "Y")
    test <- rep(test, times = 25)
    
    corp <- corpus(text, docvars = data.frame(train = factor(train)))
    dfmat <- dfm(corp, tolower = FALSE)
    
    tmod <- textmodel_nnseq(dfmat, y = docvars(dfmat, "train"), epochs = 3, verbose = FALSE)
    
    expect_output(
        print(tmod),
        "Call:"
    )
    
    expect_equal(names(summary(tmod)), c("call", "model structure"))
    expect_identical(
        as.character(predict(tmod, type = "class")),
        test
    )
    set.seed(10)
    pred_out <- predict(tmod, type = "probability")
    pred_max <- apply(pred_out, 1, function(x) colnames(pred_out)[which.max(x)])
    names(test) <- paste0("text", 1:length(test))
    expect_equal(
        pred_max,
        test
    )
})

test_that("multiclass prediction works", {
    skip_on_cran()
    
    dfmat <- dfm(data_corpus_irishbudget2010) %>%
        dfm_tfidf()
    y <- docvars(data_corpus_irishbudget2010, "party")
    y[5] <- NA
    tmod2 <- textmodel_nnseq(dfmat, y = y)
    expect_equal(
        names(predict(tmod2, newdata = dfmat[5, ], type = "class")),
        "Cowen, Brian (FF)"
    )

    probmat <- predict(tmod2, type = "probability")
    expect_equal(dim(probmat), c(14, 5))
    expect_equal(rownames(probmat), docnames(dfmat))
    expect_equal(colnames(probmat), tmod2$classnames)
    expect_equal(unname(rowSums(probmat)), rep(1, nrow(probmat)), tol = .000001)
})
