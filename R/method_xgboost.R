

method_xgboost <- function() {

  total.results$shotrate.a <- total.results$shotrate.h <- NULL


  xgboost::xgboost

  data(agaricus.train, package='xgboost')
  data(agaricus.test, package='xgboost')
  train <- agaricus.train
  test <- agaricus.test

  #original.results <- total.results
  #original.results -> total.results
  mylab <- total.results$res %>% as.factor %>% as.integer %>% `-`(1)
  total.results$res <- NULL

  my.dat <- list(data = mytrain, label = mylab)
  xgb <- xgboost::xgboost(data = sparse_matrix, label = mylab, max_depth = 2, eta = 1, nthread = 2, nrounds = 200, objective = "multi:softprob", num_class = 3)

  total.results[1, ]

  y_pred <- predict(xgb, sparse_matrix[1:2, ])

  sparse_matrix <- Matrix::sparse.model.matrix(res ~ .-1, data = original.results)






  # Split into train and test??
  new.results <- original.results
  new.results$res %<>% as.factor %>% as.integer %>% `-`(1)

  split.data <- new.results$xg.h %>% caTools::sample.split(SplitRatio = 0.70)
  train.data <- new.results %>% subset(split.data %>% `==`(TRUE))
  test.data <- new.results %>% subset(split.data %>% `==`(FALSE))

  mylab <- train.data$res

  # creat matrix + train
  sparse.matrix <- Matrix::sparse.model.matrix(res ~ .-1, data = train.data)
  xgb <- xgboost::xgboost(data = sparse.matrix, label = mylab, max_depth = 2, eta = 1, nthread = 2, nrounds = 50000, objective = "multi:softprob", num_class = 3)

  testRes <- test.data$res
  test.data$res <- NULL

  totCor <- 0
  for (i in 1:(testRes %>% length)) {
    y_pred <- predict(xgb, test.data[1, ] %>% as.matrix)
    mypred <- y_pred %>% which.max %>% `-`(1)
    if (testRes[i] == mypred) totCor %<>% `+`(1)
  }

  totCor/(testRes %>% length)





  # Conver everything to a factor
  #orig.scaled <- scaled.results
  #orig.scaled -> scaled.results
  boundLen <- 4
  bounds <- seq(0, 1, length.out = boundLen)

  for (i in 1:(scaled.results %>% nrow)) {
    for (j in 1:12) {
      scaled.results[i, j] %<>% findInterval(bounds)
    }
  }

  # Split into train and test??
  new.results <- scaled.results
  new.results$res %<>% as.factor %>% as.integer %>% `-`(1)

  split.data <- new.results$xg.h %>% caTools::sample.split(SplitRatio = 0.70)
  train.data <- new.results %>% subset(split.data %>% `==`(TRUE))
  test.data <- new.results %>% subset(split.data %>% `==`(FALSE))

  mylab <- train.data$res

  testRes <- test.data$res
  #test.data$res <- NULL

  # creat matrix + train
  sparse.matrix <- Matrix::sparse.model.matrix(res ~ .-1, data = train.data)


  # Create a sparse matrix

  c_sparse <- function(my.data) {
    myDimNames <- indI <- indJ <- c()
    boundLen <- 4
    allNames <- my.data %>%
      colnames %>%
      `[`(1:12)
    startLen <- 0

    for (k in 1:(my.data %>% ncol %>% `-`(1))) {
      for (j in 1:(my.data %>% nrow)) {
        indJ %<>% c(startLen + my.data[j, k])
        indI %<>% c(j)
      }
      myDimNames %<>% c(allNames[k] %>% paste0('=', c(1:boundLen)))
      startLen %<>% `+`(boundLen)
    }


    Matrix::sparseMatrix(
      i = indI,
      j = indJ,
      x = 1,
      dimnames = list(NULL, myDimNames)
    )


    return(
      Matrix::sparseMatrix(
        i = indI,
        j = indJ,
        x = 1,
        dimnames = list(NULL, myDimNames)
      )
    )
  }



  sparse.matrix <- train.data %>% c_sparse()

  numClasses <- 3
  xgb <- xgboost::xgboost(
    data = sparse.matrix,
    label = mylab,
    max_depth = 10,
    eta = 0.2,
    gamma = 4,
    nthread = 2,
    nrounds = 5000,
    objective = "multi:softmax",
    num_class = numClasses,
    verbose = 1
  )

  test.sparse <- test.data %>% c_sparse()
  totCor <- 0
  mypreds <- predict(xgb, test.sparse)
  for (i in 1:(testRes %>% length)) {
    if (testRes[i] == mypreds[i]) totCor %<>% `+`(1)
  }



  mypres %>% match(testRes)
  totCor/(testRes %>% length)
}
