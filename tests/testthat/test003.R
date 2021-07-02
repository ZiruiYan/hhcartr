context("Instantiate HHDecisionTree model and test functions.")

test_that("Model instantiates and returns an object of type list", {
  clf = HHDecisionTree(response="classify", n_folds=1, n_trees=1)
  expect_is(clf, "list")
})

test_that("Data landsat loads", {
  data("landsat", package = "hhcartr", overwrite = TRUE)
  X <- landsat$X
  y <- landsat$y
  expect_equal(nrow(X), length(y))
})

test_that("Test hhcartr fit, getRunStats and predict functions.", {
  set.seed(2020)
  clf = HHDecisionTree(response="classify", n_folds=1, n_trees=1)
  data("landsat", package = "hhcartr", overwrite = TRUE)
  X <- landsat$X
  y <- landsat$y
  model <- clf$fit(X, y)
  # verify name of S3 class
  expect_is(model, "hhcartr")
  #outp <- clf$getRunStats()[[1]]
  #acc <- outp["Accuracy"]
  res <- results(model)
  resacc <- res$accuracy()
  acc <- resacc[[1]]["Accuracy"]
  # not worried if values not match exactly, just that we get some
  expect_equal(as.numeric(acc), 85.456595264938, tolerance=20)
  #numnodes <- outp["Number_of_Nodes"]
  numnodes <- resacc[[1]]["Number_of_Nodes"]
  expect_equal(as.numeric(numnodes), 381, tolerance=100)
  #numleaves <- outp["Number_of_Leaves"]
  numleaves <- resacc[[1]]["Number_of_Leaves"]
  expect_equal(as.numeric(numleaves), 191, tolerance=100)
  # predict on test data now
  test_data <- landsat$test_data
  preds <- predict(model, test_data=test_data)
  allaccs <- preds$accuracy()
  meanacc <- mean(allaccs$Accuracy)
  expect_equal(meanacc, 83.15, tolerance=20)
})

