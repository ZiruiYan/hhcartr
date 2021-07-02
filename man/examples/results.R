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


