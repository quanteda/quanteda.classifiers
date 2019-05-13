context("test textmodel_nnseq")

test_that("the nnseq model works", {
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
    
    corp <- corpus(text, 
                   docvars = data.frame(train = factor(train)))
    
    dfmat <- dfm(corp, tolower = FALSE)
    
    tmod <- textmodel_nnseq(dfmat, y = docvars(dfmat, "train"), epochs = 3, verbose = F)
    
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

#test_that("multiclass prediction works", {
#    dfmat <- dfm(data_corpus_dailnoconf1991)
#    outcome <- rep(docvars(dfmat, "party"), 2)
#    dfmat <- rbind(dfmat, dfmat)
#    levels(outcome) <- c("OTH","FF","FG","OTH","OTH","OTH")
#    outcome[1:3] <- NA
#    tmod2 <- textmodel_nnseq(dfmat, seed = 10,
#                           y = outcome, epochs = 5,
#                           verbose = FALSE, units = 150)
#    expect_equal(
#        predict(tmod2, dfmat[1:3,]),
#        c("Haughey_FF_Taois.txt" = "FF", 
#          "Spring_Lab_Leader.txt" = "OTH",
#          "deRossa_DL_Leader.txt" = "OTH")
#    )
#})



