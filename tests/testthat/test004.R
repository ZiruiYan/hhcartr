context("Verify all hhcartr datasets can be loaded and used.")

test_that("Test hhcartr datasets can be loaded and used.", {
  # datasets to be tested
  # str_data_sets : dataset, external test set, nrows X, ncols X, nsamples y, nrows test, cols test
  str_data_sets <- list("landsat",   TRUE,  4435, 36,  4435,  2000, 37,
                       # "shuttle",   TRUE, 43500,  9, 43500, 14500, 10,
                        "pendigits", TRUE,  7494, 16,  7494,  3498, 17,
                        "segment",   TRUE,   210, 19,   210,  2100, 20,
                        "vehicle",  FALSE,   752, 18,   752,     0,  0,
                        "balance",  FALSE,   625,  4,   625,     0,  0,
                        "housing",  FALSE,   506, 13,   506,     0,  0,
                        "cancer",   FALSE,   683,  9,   683,     0,  0,
                        "bupa",     FALSE,   345,  6,   345,     0,  0,
                        "glass",    FALSE,   214,  9,   214,     0,  0,
                        "pima",     FALSE,   768,  8,   768,     0,  0,
                        "wine",     FALSE,   178, 13,   178,     0,  0,
                        "survival", FALSE,   306,  3,   306,     0,  0,
                        "heart",    FALSE,   303, 13,   303,     0,  0)
                        #"hhcartr_letters")

  for(dsin in seq(1, length(str_data_sets), 7)){
    tmp <- eval(parse(text=str_data_sets[dsin]))
    # get feature variable values
    X <- tmp$X
    # verify correct dimensions for feature variables
    expect_equal(as.numeric(dim(X)[1]), as.numeric(str_data_sets[dsin+2]))
    expect_equal(as.numeric(dim(X)[2]), as.numeric(str_data_sets[dsin+3]))
    # get target variable values
    y <- tmp$y
    # verify correct dimensions for target variable
    expect_equal(as.numeric(length(y)), as.numeric(str_data_sets[dsin+4]))
    # external test dataset present?
    if(as.logical(str_data_sets[dsin+1])){
      # load external test dataset
      test_data <- tmp$test_data
      # verify correct dimensions for test datasets
      expect_equal(as.numeric(dim(test_data)[1]), as.numeric(str_data_sets[dsin+5]))
      expect_equal(as.numeric(dim(test_data)[2]), as.numeric(str_data_sets[dsin+6]))
    }
  }
})

