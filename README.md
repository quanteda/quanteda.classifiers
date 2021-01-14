
# quanteda.classifiers: Text classification textmodel extensions for quanteda

[![CRAN
Version](https://www.r-pkg.org/badges/version/quanteda.classifiers)](https://CRAN.R-project.org/package=quanteda.classifiers)
[![R build
status](https://github.com/quanteda/quanteda.classifiers/workflows/R-CMD-check/badge.svg)](https://github.com/quanteda/quanteda.classifiers/actions)
[![Coverage
status](https://codecov.io/gh/quanteda/quanteda.classifiers/branch/master/graph/badge.svg)](https://codecov.io/github/quanteda/quanteda.classifiers?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Installation

This package requires the development version of **quanteda**, which
will soon be released as v2.

``` r
devtools::install_github("quanteda/quanteda") 
```

To install this package, use the following, which also installs what the
R **keras** package needs in order to run.

``` r
# devtools package required to install quanteda from Github 
devtools::install_github("quanteda/quanteda.classifiers") 

keras::install_keras(method = "conda")
```

## Available classifiers

This package contains two experimental methods that are built on top of
the **keras** package. (The SVM models have been moved to
[**quanteda.textmodels**](https://github.com/quanteda/quanteda.textmodels).)

| Classifier                                                          | Command                  |
|---------------------------------------------------------------------|--------------------------|
| Multilevel perceptron network                                       | `textmodel_mlp()`        |
| Convolutional neural network + LSTM model fitted to word embeddings | `textmodel_cnnlstmemb()` |

## Available human-annotated corpora

| Corpus                                                                                                                       | Name                           |
|------------------------------------------------------------------------------------------------------------------------------|--------------------------------|
| Sentence-level corpus of UK party manifestos 1945–2019, partially annotated                                                  | `data_corpus_manifestosentsUK` |
| Large Movie Review Dataset of 50,000 annotated highly polar movie reviews for training and testing, from Maas et. al. (2011) | `data_corpus_LMRD`             |

## Demonstration

See this (very preliminary!) [performance
comparison](https://htmlpreview.github.io/?https://github.com/quanteda/quanteda.classifiers/blob/master/tests/misc/test-LMRD.nb.html).

## How to cite

Benoit, Kenneth, Patrick Chester, and Stefan Müller (2019).
quanteda.classifiers: Models for supervised text classification. R
package version 0.2. URL: <http://github.com/quanteda/quanteda.svm>.

For a BibTeX entry, use the output from citation(package =
“quanteda.classifiers”).

## Issues

-   Please file an issue (with a bug, wish list, etc.) [via
    GitHub](https://github.com/quanteda/quanteda.classifiers/issues).
