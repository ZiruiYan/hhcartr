context("Test parameter parsing of the HHDecisionTree mni.control function.")

test_that("Test parsing of the control parameter - mni_n_folds.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              min_node_impurity = 'auto',
                              control = mni.control(mni_n_folds=0.5)),
  "Assertion on 'mni_parms$mni_n_folds' failed: Must be of type 'single integerish value', not 'double'.", fixed=TRUE)
})

test_that("Test parsing of the control parameter - mni_n_trees.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              min_node_impurity = 'auto',
                              control = mni.control(mni_n_trees=0.5)),
               "Assertion on 'mni_parms$mni_n_trees' failed: Must be of type 'single integerish value', not 'double'.", fixed=TRUE)
})

test_that("Test parsing of the control parameter - mni_trials.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              min_node_impurity = 'auto',
                              control = mni.control(mni_trials=0.5)),
               "Assertion on 'mni_parms$mni_trials' failed: Must be of type 'single integerish value', not 'double'.", fixed=TRUE)
})

test_that("Test parsing of the control parameter - mni_size.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              min_node_impurity = 'auto',
                              control = mni.control(mni_size=2.0)),
               "Assertion on 'mni_parms$mni_size' failed: Element 1 is not <= 0.1.", fixed=TRUE)
})

#test_that("Test parsing of the control parameter - mni_end.", {
#  expect_error(HHDecisionTree(n_folds=1,
#                              n_trees=1,
#                              min_node_impurity = 'auto',
#                              control = mni.control(mni_end=2.0)),
#               "Assertion on 'mni_parms$mni_end' failed: Element 1 is not <= 1.", fixed=TRUE)
#})

test_that("Test parsing of the control parameter - mni_numvals.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              min_node_impurity = 'auto',
                              control = mni.control(mni_numvals = 2000.0)),
               "Assertion on 'mni_parms$mni_numvals' failed: Element 1 is not <= 1000.", fixed=TRUE)
})







