% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hhcartr_export_displayTree.R
\name{navigate_hash}
\alias{navigate_hash}
\title{navigate_hash generates DOT statements for all trees in current model.}
\usage{
navigate_hash(hobj, numtree = NA, dataset_description = "Unknown")
}
\arguments{
\item{hobj}{List of all tree objects.}

\item{numtree}{The number of the tree to display.}

\item{dataset_description}{A brief description of the dataset being used.}
}
\value{
nothing.
}
\description{
This internal function generates DOT statements for the selected tree generated by the
current model, grViz is then called to display the resultant graph.
}
