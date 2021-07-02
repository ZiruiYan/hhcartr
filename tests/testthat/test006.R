# source: test006.R

# Test parameter parsing of HHDecisionTree function response="regressor"
# Parsing of the following parameters will be tested:

# Only the following parameters will be validated as they are used in HHDecisionTree.
# @param response
# @param n_min
# @param n_trees
# @param n_folds
# @param testSize

# The following parameters will not be tested as they are not used by HHDecisionTree,
# they are only used by HHRandomForest which is not part of this package yet.
#-@param sample_size
#-@param sampleWithReplacement
#-@param max_features
#-@param parallelize
#-@param number_cpus

# The following parameter will not be tested as it package functionality is not dependent
# upon its value. Used for display purposes only.
# @param dataDescription


context("Test parameter parsing of HHDecisionTree response=regressor function.")

test_that("Test parsing of the n_min parameter.", {
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds=1,
                                        n_trees=1,
                                        n_min = 0),
               "Assertion on 'n_min' failed: ")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds=1,
                                        n_trees=1,
                                        n_min = TRUE),
               "Assertion on 'n_min' failed: Must be of type 'single integerish value', not 'logical'.")
  expect_error(HHDecisionTree(response="regressor",
                                       n_folds=1,
                                        n_trees=1,
                                        n_min = 0.2),
               "Assertion on 'n_min' failed: Must be of type 'single integerish value', not 'double'.")
})

test_that("Test parsing of the n_folds parameter.", {
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds = 0,
                                        n_trees = 1,
                                        n_min   = 2),
               "Assertion on 'n_folds' failed: ")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds = FALSE,
                                        n_trees = 1,
                                        n_min   = 2),
               "Assertion on 'n_folds' failed: Must be of type 'single integerish value', not 'logical'.")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds = 6.5,
                                        n_trees = 1,
                                        n_min   = 2),
               "Assertion on 'n_folds' failed: Must be of type 'single integerish value', not 'double'.")
})

test_that("Test parsing of the n_trees parameter.", {
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds = 1,
                                        n_trees = 0,
                                        n_min   = 2),
               "Assertion on 'n_trees' failed: ")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds = 1,
                                        n_trees = 10,
                                        n_min   = 2),
               "Assertion on 'n_trees' failed: ")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds = 1,
                                        n_trees = TRUE,
                                        n_min   = 2),
               "Assertion on 'n_trees' failed: Must be of type 'single integerish value', not 'logical'.")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds = 1,
                                        n_trees = 5.4,
                                        n_min   = 2),
               "Assertion on 'n_trees' failed: Must be of type 'single integerish value', not 'double'.")
})

test_that("Test parsing of the testSize parameter.", {
  tt87 <- HHDecisionTree(response="regressor",
                         n_folds  = 1,
                         n_trees  = 1,
                         testSize = 0)
  expect_type(tt87, "list")

  expect_error(HHDecisionTree(response="regressor",
                                        n_folds  = 1,
                                        n_trees  = 1,
                                        testSize = -0.04),
               "Assertion on 'testSize' failed: ")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds  = 1,
                                        n_trees  = 1,
                                        testSize = TRUE),
               "Assertion on 'testSize' failed: Must be of type 'number', not 'logical'.")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds  = 1,
                                        n_trees  = 1,
                                        testSize = 0.8),
               "Assertion on 'testSize' failed: Element 1 is not <= 0.75.")
  expect_error(HHDecisionTree(response="regressor",
                                        n_folds  = 1,
                                        n_trees  = 1,
                                        testSize = 10),
               "Assertion on 'testSize' failed: Element 1 is not <= 0.75.")
})
