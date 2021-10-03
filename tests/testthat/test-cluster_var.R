require(testthat)

## random number generator
RNGkind("L'Ecuyer-CMRG")

test_that("cluster_vars: expect error", {
  expect_error(cluster_vars(x = NULL, d = NULL, method = "average", block = NULL,
                           use = "everything"),
               "Please specify x or d.")

  expect_error(cluster_vars(x = matrix(1:4, ncol = 2), d = matrix(1:4, ncol = 2),
                           method = "average", block = NULL, use = "everything"),
               "Please specify only x or d.")

  # expect an error if x has no column names
  expect_error(cluster_vars(x = matrix(1:4, ncol = 2), d = NULL, method = "average",
                           block = NULL, use = "everything"),
               "The matrices (or matrix) which are stored in x are required to have column names. If there is no natural naming convention, then one can set them to some integer, say, 1 to p.",
               fixed = TRUE)

  tt <- matrix(c(1:3, NA), ncol = 2)
  colnames(tt) <- c("c1", "c2")
  expect_error(cluster_vars(x = tt, d = NULL, method = "average",
                           block = NULL, use = "everything"),
               "The matrix x is required to have no missing values.",
               fixed = TRUE)

  tt1 <- tt2 <- matrix(c(1:4), ncol = 2)
  colnames(tt1) <- c("c1", "c2")
  colnames(tt2) <- c("c1", "c5")
  temp <- attr(suppressWarnings(cluster_vars(x = list(tt1, tt2), d = NULL,
                                            method = "average",
                                            block = NULL,
                                            use = "everything")$res.tree),
               "errorMsgs")
  names(temp) <- NULL
  expect_equal(temp,
               "There are NA's in the calculated dissimilarity matrix / distance matrix. The variables cannot be clustered. This might be due to multiple data sets which do not contain all the same variables.",
               fixed = TRUE)

  expect_error(cluster_vars(x = NULL, d = matrix(1:4, ncol = 2), method = "average",
                           block = data.frame(c("c1", "c2"), c(1, 1)),
                           use = "everything"),
               "The argument block can only be specified in connection with the input x.")

  expect_error(cluster_vars(x = matrix(1:4, ncol = 2), d = NULL, method = "average",
                           block = data.frame(c("c1", "c2"), c(1, 1)),
                           use = "everything"),
               "The matrices (or matrix) which are stored in x are required to have column names. If there is no natural naming convention, then one can set them to some integer, say, 1 to p.",
               fixed  = TRUE)

  tt <- matrix(1:4, ncol = 2)
  colnames(tt) <- c("c1", "c2")
  expect_error(cluster_vars(x = tt, d = NULL, method = "average",
                           block = data.frame(c("c1", "c2"), c(1, 1),
                                              stringsAsFactors = TRUE),
                           use = "everything"),
               "The first column of block (column names of x) is required to be of type character.",
               fixed = TRUE)

  expect_error(cluster_vars(x = tt, d = NULL, method = "average",
                           block = data.frame(c("c1", "c2"), c(1, 1),
                                              stringsAsFactors = FALSE),
                           use = "everything"),
               "The second column of the input block is required to encode at least two blocks.")

  # error because the distance matrix does not have labels
  expect_error(cluster_vars(d = dist(matrix(rnorm(100), ncol = 10))),
               "The distance matrix d is required to have labels. The labels should correspond to the column names of the data set or data sets stored in x. For example, the function dist uses the row names to set the labels.")

  expect_error(cluster_vars(d = matrix(rnorm(80), ncol = 8)),
               "The matrix d is required to have the same number of columns and rows.")

  d <- as.matrix(dist(matrix(rnorm(100), ncol = 10)))
  rownames(d) <- colnames(d) <- NULL
  expect_error(cluster_vars(d = d),
               "The matrix d is required to have column and row names.")

  d <- matrix(rnorm(100), ncol = 10)
  rownames(d) <- colnames(d) <- letters[1:10]
  expect_error(cluster_vars(d = d),
               "The matrix d is required to be symmetric.")

  # NA's in block argument
  set.seed(888)
  tt <- matrix(rnorm(300), ncol = 30)
  colnames(tt) <- paste0("rs", 1:30)
  block <- data.frame(paste0("rs", 1:30), rep(c(1, 2), each = 15),
                      stringsAsFactors = FALSE)
  block[29:30, 2] <- NA
  expect_error(cluster_vars(x = tt, d = NULL, block = block),
               "There are missing values in the input block.")

  # The argument block has less variables / column names than the data sets
  require("MASS")
  p <- 20
  n <- 80
  sim.geno1 <- mvrnorm(n = n, mu = rep(0, p), Sigma = toeplitz(0.8^(seq(0, p - 1))) )
  colnames(sim.geno1) <- paste0("rsid", 1:p)
  sim.geno2 <- mvrnorm(n = n, mu = rep(0, p), Sigma = toeplitz(0.8^(seq(0, p - 1))) )
  colnames(sim.geno2) <- paste0("rsid", 1:p)

  expect_error(cluster_vars(x = list(sim.geno1, sim.geno2),
                           block = data.frame(paste0("rsid", 1:10),
                                              rep(1:2, each = 5),
                                              stringsAsFactors = FALSE)),
               "There are column name of x which have no corresponding values in the first column of block (column names of x).",
               fixed = TRUE)

  # The column names of x or each element of x (list containing data sets)
  # are required to have unique column names.
  expect_error(cluster_vars(x = cbind(sim.geno1, sim.geno1)),
               "Each of the matrices (or matrix) which are stored in x are required to have unique column names.",
               fixed = TRUE)

  expect_error(cluster_vars(x = list(cbind(sim.geno1, sim.geno1), sim.geno1)),
               "Each of the matrices (or matrix) which are stored in x are required to have unique column names.",
               fixed = TRUE)
})


test_that("cluster_vars: check return object", {
  tt <- matrix(1:4, ncol = 2)
  colnames(tt) <- c("c1", "c2")
  res_simple <- cluster_vars(x = tt, d = NULL, method = "average", block = NULL,
                            use = "everything")
  expect_is(res_simple, "hierD")
  expect_type(res_simple, "list")
  expect_is(res_simple$res.tree[[1]], "dendrogram")

  set.seed(888)
  tt <- matrix(rnorm(300), ncol = 30)
  colnames(tt) <- paste0("rs", 1:30)
  res <- cluster_vars(x = tt, d = NULL, method = "average",
                     block = data.frame(paste0("rs", 1:30), rep(c(1, 2), each = 15),
                                        stringsAsFactors = FALSE),
                     use = "everything")
  expect_equal(res$block, data.frame(paste0("rs", 1:30), rep(c(1, 2), each = 15),
                                     stringsAsFactors = FALSE))
  expect_equal(length(res$res.tree), 2)
  expect_true(all(labels(res$res.tree[[1]]) %in% paste0("rs", 1:15))) # labels res1 - rs15
  expect_true(all(labels(res$res.tree[[2]]) %in% paste0("rs", 16:30))) # labels rs16 - rs30


  sim.geno <- matrix(rnorm(100), ncol = 10)
  rownames(sim.geno) <- letters[1:10]
  res_d <- cluster_vars(d = dist(sim.geno))
  expect_is(res_d$res.tree[[1]], "dendrogram")
  expect_true(all(labels(res_d$res.tree[[1]]) %in% letters[1:10]))
})

test_that("cluster_vars: check return object for multiple data sets", {
  set.seed(938)
  tt1 <- matrix(rnorm(40), ncol = 2)
  tt2 <- matrix(rnorm(40), ncol = 2)
  tt3 <- matrix(rnorm(40), ncol = 2)
  colnames(tt1) <- c("c1", "c2")
  colnames(tt2) <- c("c1", "c5")
  colnames(tt3) <- c("c2", "c5")

  # expect error because the argument use = "everything"
  res <- suppressWarnings(cluster_vars(x = list(tt1, tt2, tt3), d = NULL,
                                      method = "average",
                                      block = NULL,
                                      use = "everything"))
  tmp <- attr(res$res.tree, "errorMsgs")
  names(tmp) <- NULL
  expect_equal(tmp,
               "There are NA's in the calculated dissimilarity matrix / distance matrix. The variables cannot be clustered. This might be due to multiple data sets which do not contain all the same variables.",
               fixed = TRUE)

  # use = "pairwise.complete.obs" (default)
  res <- cluster_vars(x = list(tt1, tt2, tt3), d = NULL,
                     method = "average",
                     block = NULL)
  # plot(res$res.tree[[1]])
  expect_identical(labels(res$res.tree[[1]]), c("c5", "c1", "c2"))
})

test_that("cluster_vars: check if the functions runs in parallel", {
  # Run this test only locally. (Not suitable for Windows)
  skip_on_bioc()
  skip_on_os("windows")

  set.seed(888)
  tt <- matrix(rnorm(300), ncol = 30)
  colnames(tt) <- paste0("rs", 1:30)
  set.seed(999)
  res_p <- cluster_vars(x = tt, d = NULL, method = "average",
                     block = data.frame(paste0("rs", 1:30), rep(c(1, 2), each = 15),
                                        stringsAsFactors = FALSE),
                     use = "everything", sort.parallel = TRUE,
                     parallel = "multicore", ncpus = 2)
  expect_is(res_p, "hierD")
  expect_type(res_p, "list")
  expect_is(res_p$res.tree[[1]], "dendrogram")
  expect_is(res_p$res.tree[[2]], "dendrogram")
  expect_equal(length(res_p), 2)
  expect_true(all(labels(res_p$res.tree[[1]]) %in% paste0("rs", 1:15))) # labels res1 - rs15
  expect_true(all(labels(res_p$res.tree[[2]]) %in% paste0("rs", 16:30))) # labels rs16 - rs30
})






