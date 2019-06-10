
# quanteda.classifiers: Text classification textmodel extensions for quanteda

[![CRAN
Version](https://www.r-pkg.org/badges/version/quanteda.classifiers)](https://CRAN.R-project.org/package=quanteda.classifiers)
[![Travis build
status](https://travis-ci.org/quanteda/quanteda.classifiers.svg?branch=master)](https://travis-ci.org/quanteda/quanteda.classifiers)
[![Build
status](https://ci.appveyor.com/api/projects/status/l80oet8swj2q6h4y/branch/master?svg=true)](https://ci.appveyor.com/project/kbenoit/quanteda-svm/branch/master)
[![Coverage
status](https://codecov.io/gh/quanteda/quanteda.classifiers/branch/master/graph/badge.svg)](https://codecov.io/github/quanteda/quanteda.classifiers?branch=master)

## Installation

``` r
# devtools package required to install quanteda from Github 
devtools::install_github("quanteda/quanteda.classifiers") 
```

## Available classifiers

| Classifier                                                          | Command                  |
| ------------------------------------------------------------------- | ------------------------ |
| Support Vector Machine (SVM)                                        | `textmodel_svm()`        |
| Sequential neural network                                           | `textmodel_nnseq()`      |
| Convolutional neural network + LSTM model fitted to word embeddings | `textmodel_cnnlstmemb()` |

## Available human-annotated corpora

| Corpus                                                                                    | Name                           |
| ----------------------------------------------------------------------------------------- | ------------------------------ |
| Sentence-level corpus of UK party manifestos 1945–2017, partially annotated               | `data_corpus_manifestosentsUK` |
| Crowd-labelled sentence corpus from a 2010 EP debate on coal subsidies (in six languages) | `data_corpus_EPcoaldebate`     |

``` r
library("quanteda.classifiers")

performance <- function(mytable, verbose = TRUE) {
  truePositives <- mytable[1, 1]
  trueNegatives <- sum(diag(mytable)[-1])
  falsePositives <- sum(mytable[1, ]) - truePositives
  falseNegatives <- sum(mytable[, 1]) - truePositives
  precision <- truePositives / (truePositives + falsePositives)
  recall <- truePositives / (truePositives + falseNegatives)
  accuracy <- sum(diag(mytable)) / sum(mytable)
  tnr <- trueNegatives / (trueNegatives + falsePositives)
  balanced_accuracy <- sum(c(precision, tnr), na.rm = TRUE) / 2
  if (verbose) {
    print(mytable)
    cat(
      "\n    precision =", round(precision, 2),
      "\n       recall =", round(recall, 2),
      "\n     accuracy =", round(accuracy, 2),
      "\n    bal. acc. =", round(balanced_accuracy, 2),
      "\n"
    )
  }
  invisible(c(precision, recall))
}

# define training texts and the "true" govt/opp status
y <- ifelse(docvars(data_corpus_dailnoconf1991, "name") == "Haughey", "Govt", NA)
y <- ifelse(docvars(data_corpus_dailnoconf1991, "name") %in% c("Spring", "deRossa"), "Opp", y)
truth <- ifelse(docvars(data_corpus_dailnoconf1991, "party") %in% c("FF", "PD"), "Govt", "Opp")

# no weighting: poor
dfm(data_corpus_dailnoconf1991) %>%
  textmodel_svm(y) %>%
  predict() %>%
  table(truth) %>%
  performance()
##       truth
## .      Govt Opp
##   Govt    6   0
##   Opp    19  33
## 
##     precision = 1 
##        recall = 0.24 
##      accuracy = 0.67 
##     bal. acc. = 1

# proportions: poor, predicts everyone to be opposition
dfm(data_corpus_dailnoconf1991) %>%
  dfm_weight(scheme = "prop") %>%
  textmodel_svm(y) %>%
  predict() %>%
  table(truth) %>%
  performance()
##       truth
## .      Govt Opp
##   Govt    0   0
##   Opp    25  33
## 
##     precision = NaN 
##        recall = 0 
##      accuracy = 0.57 
##     bal. acc. = 0.5

# scaled - results in a fully dense dfm, and poor performance
dfm(data_corpus_dailnoconf1991) %>%
  scale() %>%
  as.dfm() %>%
  textmodel_svm(y) %>%
  predict() %>%
  table(truth) %>%
  performance()
##       truth
## .      Govt Opp
##   Govt   24  25
##   Opp     1   8
## 
##     precision = 0.49 
##        recall = 0.96 
##      accuracy = 0.55 
##     bal. acc. = 0.37

# tf-idf: better
dfm(data_corpus_dailnoconf1991) %>%
  dfm_tfidf() %>%
  textmodel_svm(y) %>%
  predict() %>%
  table(truth) %>%
  performance()
##       truth
## .      Govt Opp
##   Govt   16   3
##   Opp     9  30
## 
##     precision = 0.84 
##        recall = 0.64 
##      accuracy = 0.79 
##     bal. acc. = 0.88

# tf-idf: best with document frequency weights
dfm(data_corpus_dailnoconf1991) %>%
  dfm_tfidf() %>%
  textmodel_svm(y, weight = "docfreq") %>%
  predict() %>%
  table(truth) %>%
  performance()
##       truth
## .      Govt Opp
##   Govt   15   2
##   Opp    10  31
## 
##     precision = 0.88 
##        recall = 0.6 
##      accuracy = 0.79 
##     bal. acc. = 0.91
```

## How to cite

Benoit, Kenneth, Patrick Chester, and Stefan Müller (2019).
quanteda.classifiers: Models for supervised text classification. R
package version 0.1. URL: <http://github.com/quanteda/quanteda.svm>.

For a BibTeX entry, use the output from citation(package =
“quanteda.classifiers”).

## Issues

  - Please file an issue (with a bug, wish list, etc.) [via
    GitHub](https://github.com/quanteda/quanteda.classifiers/issues).
