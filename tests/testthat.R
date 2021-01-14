library("testthat")
library("quanteda.classifiers")

library("quanteda")
library("quanteda.textmodels")

# library("keras")
# use_session_with_seed(42)
# see https://github.com/rstudio/keras/issues/890#issuecomment-539044011
# tensorflow::tf$random$set_seed(42)

test_check("quanteda.classifiers")
