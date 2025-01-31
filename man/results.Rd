% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hhcartr_export_results.R
\name{results}
\alias{results}
\title{results - Create generic S3method to display results via results.hhcartr. Needs export entry
in the NAMESPACE file.}
\usage{
results(x, ...)
}
\arguments{
\item{x}{Unused parameter.}

\item{...}{Unused parameter.}
}
\value{
Prints relevant information about accuracy from model training. Object returned
           from call to results exposes the accuracy() method, which can then be used to return
           accuracy information for each fold/trial of the training process. Also exposed is
           the margin() method, this returns the margin for each tree in each fold.
}
\description{
This function creates a generic S3method results which is used to call results.hhcartr when
an object of type hhcartr passed to the results function, i.e. an object that is returned
from the fit() function. Parameters and return are the same for the results.hhcartr function.
}
\examples{
# source: /man/examples/results.R

# Basic usage of results().

# Note: we need to have a model to modify first.

# load our data.
X <- iris[,1:4]
y <- iris[,5]

# instantiate our model.
clf = HHDecisionTree(n_folds=1,
                     n_trees=1,
                     pruning=FALSE,
                     min_node_impurity=0.0)

# describe what dataset our model is using.
setDataDescription("IRIS Dataset")

# train our model.
model_output <- clf$fit(X, y)

# create our results() object.
res <- results(model_output)

# The results object 'res' exposes the following methods
# that we can use to extract the results of interest
# (depending upon model options used):

# res$accuracy, res$margin, res$mni_data,
# res$ccp_subtree_data, res$ccp_phase_data, res$ccp_predictions.


}
