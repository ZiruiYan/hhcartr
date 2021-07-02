# source: /man/examples/displayTree.R

# basic usage of displayTree(n)

# Note: need to have something to display first.
X <- iris[,1:4]
y <- iris[,5]
clf = HHDecisionTree(n_folds=1,
                     n_trees=1,
                     pruning=FALSE,
                     min_node_impurity=0.0)
# train our model.
vv <- clf$fit(X, y)
# display the resulting tree.
displayTree(1)

