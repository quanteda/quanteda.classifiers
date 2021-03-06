% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textmodel_mlp.R
\name{textmodel_mlp}
\alias{textmodel_mlp}
\title{Multilayer perceptron network (MLP) model for text classification}
\usage{
textmodel_mlp(
  x,
  y,
  units = 512,
  dropout = 0.2,
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = "categorical_accuracy",
  ...
)
}
\arguments{
\item{x}{the \link{dfm} on which the model will be fit.  Does not need to
contain only the training documents.}

\item{y}{vector of training labels associated with each document identified
in \code{train}.  (These will be converted to factors if not already
factors.)}

\item{units}{The number of network nodes used in the first layer of the
sequential model}

\item{dropout}{A floating variable bound between 0 and 1. It determines the
rate at which units are dropped for the linear transformation of the
inputs.}

\item{optimizer}{optimizer used to fit model to training data, see
\code{\link[keras:compile.keras.engine.training.Model]{keras::compile.keras.engine.training.Model()}}}

\item{loss}{objective loss function, see
\code{\link[keras:compile.keras.engine.training.Model]{keras::compile.keras.engine.training.Model()}}}

\item{metrics}{metric used to train algorithm, see
\code{\link[keras:compile.keras.engine.training.Model]{keras::compile.keras.engine.training.Model()}}}

\item{...}{additional options passed to
\code{\link[keras:fit.keras.engine.training.Model]{keras::fit.keras.engine.training.Model()}}}
}
\description{
This function is a wrapper for a multilayer perceptron network model with a
single hidden layer network with two layers, implemented in the \pkg{keras}
package.
}
\examples{
\dontrun{
# create a dataset with evenly balanced coded and uncoded immigration sentences
corpcoded <- corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
corpuncoded <- data_corpus_manifestosentsUK \%>\%
    corpus_subset(is.na(crowd_immigration_label) & year > 1980) \%>\%
    corpus_sample(size = ndoc(corpcoded))
corp <- corpcoded + corpuncoded

# form a tf-idf-weighted dfm
dfmat <- dfm(corp) \%>\%
    dfm_tfidf()

set.seed(1000)
tmod <- textmodel_mlp(dfmat, y = docvars(dfmat, "crowd_immigration_label"),
                        epochs = 5, verbose = 1)
pred <- predict(tmod, newdata = dfm_subset(dfmat, is.na(crowd_immigration_label)))
table(pred)
tail(texts(corpuncoded)[pred == "Immigration"], 10)
}
}
\seealso{
\code{\link[=save.textmodel_mlp]{save.textmodel_mlp()}}, \code{\link[=load.textmodel_mlp]{load.textmodel_mlp()}}
}
\keyword{textmodel}
