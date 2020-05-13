context("test textmodel_evaluate")

test_that("textmodel_evaluate works", {
    skip_on_cran()
    
    set.seed(100)
    corp <- corpus_sample(data_corpus_EPcoaldebate, size = 1000, by = "crowd_subsidy_label")
    dfmat <- dfm(corp) %>% 
        dfm_trim(min_termfreq = 10)
    labels <- docvars(dfmat, "crowd_subsidy_label")
    model_eval <- textmodel_evaluate(x = dfmat, y = labels, model = "textmodel_mlp", fun = "f1_score", k = 3, seed = 5)
        
    # Check ouptuts for consistency
    expect_equal(dim(model_eval), c(3, 4))
    expect_equal(names(model_eval), c("k", "f1_score", "time", "seed"))
    expect_equal(max(model_eval$k), 3)
    
    
    model_eval2 <- textmodel_evaluate(x = dfmat, y = labels, model = "textmodel_mlp", fun = "f1_score", k = 2, parameters = list(epochs = c(3, 4)), seed = 5)
    
    # Check ouptuts for consistency
    expect_equal(dim(model_eval2), c(4, 5))
    expect_equal(names(model_eval2), c("k", "f1_score", "epochs", "time", "seed"))
    expect_equal(max(model_eval2$k), 2)
    
    # Check by_class
    model_eval3 <- textmodel_evaluate(x = dfmat, y = labels, model = "textmodel_mlp", fun = "recall", k = 2, seed = 5, by_class = TRUE)
    labels_names <- gsub(" |-", ".", levels(labels))
    expect_equal(sum(labels_names %in% names(model_eval3)), 3)
    })
