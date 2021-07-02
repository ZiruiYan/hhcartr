# source: /man/examples/print.R

# Basic usage of print().

# Note: we need to have a model first.

# load our data.
X <- iris[,1:4]
y <- iris[,5]

# instantiate our model.
clf = HHDecisionTree(n_folds=10,
                     n_trees=1,
                     pruning=FALSE,
                     min_node_impurity=0.0)

# describe what dataset our model is using.
setDataDescription("IRIS Dataset")

# train our model.
model_output <- clf$fit(X, y)

# print fold accuracy distribution from our training data.
print(model_output)
