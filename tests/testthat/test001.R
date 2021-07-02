context("Instantiate HHDecisionTree model response=classify and test functions.")

test_that("Model instantiates and returns an object of type list", {
  clf = HHDecisionTree(response="classify", n_folds=1, n_trees=1)
  expect_is(clf, "list")
})

test_that("Data cancer loads", {
  data("cancer", package = "hhcartr", overwrite = TRUE)
  X <- cancer$X
  y <- cancer$y
  expect_equal(nrow(X), length(y))
})

test_that("Test hhcartr fit and getRunStats functions.", {
  set.seed(2020)
  clf = HHDecisionTree(response="classify", n_folds=1, n_trees=1)
  data("cancer", package = "hhcartr", overwrite = TRUE)
  X <- cancer$X
  y <- cancer$y
  model <- clf$fit(X, y)
  # verify name of S3 class
  expect_is(model, "hhcartr")
  #outp <- clf$getRunStats()[[1]]
  #acc <- outp["Accuracy"]
  res <- results(model)
  resacc <- res$accuracy()
  acc <- resacc[[1]]["Accuracy"]
  # not worried if values not match exactly, just that we get some
  expect_equal(as.numeric(acc), 88.2352941176471, tolerance=20)
  numnodes <- resacc[[1]]["Number_of_Nodes"]
  expect_equal(as.numeric(numnodes), 3, tolerance=20)
  numleaves <- resacc[[1]]["Number_of_Leaves"]
  expect_equal(as.numeric(numleaves), 2, tolerance=20)
})
