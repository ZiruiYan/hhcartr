% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grow_tree.R
\name{grow_tree_}
\alias{grow_tree_}
\title{grow_tree_}
\usage{
grow_tree_(
  node,
  X,
  y,
  n_min,
  min_node_impurity,
  useIdentity,
  classify = TRUE,
  n_features,
  n_classes,
  max_features,
  depth = 0
)
}
\arguments{
\item{node}{The current node to be tested for a split.}

\item{X}{The feature variables of the current node being processed.}

\item{y}{The target variable for the feature variables.}

\item{n_min}{The n_min parameter is used to terminate node splitting when a minimum number
of samples at that node has been reached. The default value is 2.}

\item{min_node_impurity}{Splitting a node will stop when the impurity of a node is less than
min_node_impurity. The node impurity is calculated using the hyperplane Gini index.
The default value is 0.2.}

\item{useIdentity}{The useIdentity parameter when set TRUE will result in hhcartr using the
original training data to find the optimal splits rather than using the reflected data.
The default value is FALSE.}

\item{classify}{The classify parameter when set TRUE will result in hhcartr performing a
classification, when set FALSE will perform a regression.}

\item{n_features}{The number of feature variables.}

\item{n_classes}{The number of classes in the target variable.}

\item{max_features}{The maximum number of features to consider in the current split.}

\item{depth}{This parameter is not used as yet.}
}
\value{
Returns the latest node of type NNode.
}
\description{
This internal function is used to grow the decision tree, it is called recursively until
the stopping criteria are met.
}
\details{
The following parameters are supported:
}
