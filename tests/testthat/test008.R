# prune_control     = prune.control(prune_type = "all",
#                                   prune_stochastic_max_nodes = 12,
#                                   prune_stochastic_samples = 1000)

context("Test parameter parsing of the HHDecisionTree prune.control function.")

test_that("Test parsing of the control parameter - prune_type = 'all'.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              prune_control = prune.control(prune_type = "dog")),
               "Assertion on 'prune_parms$prune_type' failed: Must be element of set {'ccp','all'}, but is 'dog'.", fixed=TRUE)
})

test_that("Test parsing of the control parameter - prune_stochastic_max_nodes =.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              prune_control = prune.control(prune_stochastic_max_nodes = 3)),
               "Assertion on 'prune_parms$prune_stochastic_max_nodes%%2' failed. FALSE.", fixed=TRUE)
})

test_that("Test parsing of the control parameter - prune_stochastic_max_nodes =.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              prune_control = prune.control(prune_stochastic_max_nodes = 1)),
               "Assertion on 'prune_parms$prune_stochastic_max_nodes' failed: Element 1 is not >= 2.", fixed=TRUE)
})

test_that("Test parsing of the control parameter - prune_stochastic_max_nodes =.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              prune_control = prune.control(prune_stochastic_max_nodes = 100)),
               "Assertion on 'prune_parms$prune_stochastic_max_nodes' failed: Element 1 is not <= 24.", fixed=TRUE)
})

test_that("Test parsing of the control parameter - prune_stochastic_samples =.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              prune_control = prune.control(prune_stochastic_samples = 20000)),
               "Assertion on 'prune_parms$prune_stochastic_samples' failed: Element 1 is not <= 10000.", fixed=TRUE)
})

test_that("Test parsing of the control parameter - prune_stochastic_samples =.", {
  expect_error(HHDecisionTree(n_folds=1,
                              n_trees=1,
                              prune_control = prune.control(prune_stochastic_samples = 0)),
               "Assertion on 'prune_parms$prune_stochastic_samples' failed: Element 1 is not >= 1.", fixed=TRUE)
})









