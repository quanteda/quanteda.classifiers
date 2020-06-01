context("test evaluation functions")

lvs <- c("Relevant", "Irrelevant")
pred <- factor(rep(lvs, times = c(42, 58)), levels = lvs)
true <- factor(c(rep(lvs, times = c(30, 12)),
                 rep(lvs, times = c(30, 28))), levels = lvs)
tab <- table(pred, true)

test_that("performance works by_class = TRUE", {
    perf <- performance(tab, by_class = TRUE)
    expect_equal(perf,
                 list(precision = c(Relevant = 0.714, 
                                    Irrelevant = 0.483),
                      recall = c(Relevant = 0.5, Irrelevant = 0.7), 
                      f1 = c(Relevant = 1.647, 
                             Irrelevant = 1.69), 
                      accuracy = 0.58, 
                      balanced_accuracy = 0.6),
                 tol = .001
    )
})

test_that("performance works by_class = FALSE", {
    perf <- performance(tab, by_class = FALSE)
    expect_equal(perf,
                 list(precision = 0.599, recall = 0.6, f1 = 1.669, 
                      accuracy = 0.58, balanced_accuracy = 0.6),
                 tol = .001
    )
})

test_that("exceptions work", {
    perf <- performance(tab, by_class = TRUE)
    expect_error(
        f1_score(perf[c("precision", "accuracy")]),
        "list must contain both precision and recall"
    )
    expect_error(
        balanced_accuracy(perf[c("precision", "accuracy")]),
        "list must include recall"
    )
    
    expect_error(
        balanced_accuracy(performance(tab, by_class = FALSE)),
        "recall must be computed by class"
    )
    
    expect_error(
        quanteda.classifiers:::check_table(tab, 3),
        "truth must be 2 for columns or 1 for rows"
    )
    tab2 <- tab
    colnames(tab2)[2] <- "dummy" 
    expect_error(
        quanteda.classifiers:::check_table(tab2, 2),
        "predicted and truth values must have the same order and names"
    )
})
