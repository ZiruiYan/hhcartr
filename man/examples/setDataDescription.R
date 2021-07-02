# source: /man/examples/setDataDescription.R

# Basic usage of setDataDescription().

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
vv <- clf$fit(X, y)
