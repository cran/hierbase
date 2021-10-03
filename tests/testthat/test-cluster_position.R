require("testthat")

## random number generator
RNGkind("L'Ecuyer-CMRG")

test_that("cluster_positions: check input", {
  expect_error(cluster_positions(position = NULL),
               "The input position is required to be a data.frame or a list of data.frames.")

  position <- cbind(paste0("Var", 1:500), as.character(seq(from = 1, to = 1000, by = 2)))
  expect_error(cluster_positions(position = position),
               "The input position is required to be a data.frame or a list of data.frames.",
               fixed = TRUE)

  position <- data.frame("var.names" = paste0("Var", 1:500),
                         "position" = rep(seq(from = 1, to = 500, by = 2), each = 2),
                         stringsAsFactors = FALSE)
  expect_error(cluster_positions(position = position),
               "The second column of the input position is required to encode unique positions of the corresponding variable / columns in x.")

  position <- data.frame("var.names" = paste0("Var", c(1, 1, 2:499)),
                         "position" = seq(from = 1, to = 1000, by = 2),
                         stringsAsFactors = FALSE)
  expect_error(cluster_positions(position = position),
               "The values in the first column of position (column names of x) are not unique.",
               fixed = TRUE)

  position <- data.frame("var.names" = paste0("Var", 1:500),
                         "position" = seq(from = 1, to = 1000, by = 2),
                         "non.sense" = runif(500),
                         stringsAsFactors = FALSE)
  expect_error(cluster_positions(position = position),
               "The input position or its list elements are required to have two columns.")

  position <- data.frame("var.names" = paste0("Var", 1:500),
                         "position" = seq(from = 1, to = 1000, by = 2),
                         stringsAsFactors = TRUE)
  expect_error(cluster_positions(position = position),
               "The first column of position or of its list elements (column names of x) are required to be of type character.",
               fixed = TRUE)

  position <- data.frame("var.names" = paste0("Var", 1:500),
                         "position" = as.character(seq(from = 1, to = 1000, by = 2)),
                         stringsAsFactors = FALSE)
  expect_error(cluster_positions(position = position),
               "The second column of position or of its list elements (the positions of the corresponding variables / columns in x) are required to be a numeric vector.",
               fixed = TRUE)

  position1 <- data.frame("var.names" = paste0("Var", 1:500),
                         "position" = seq(from = 1, to = 1000, by = 2),
                         stringsAsFactors = FALSE)
  position2 <- data.frame("var.names" = c("VAR.999", paste0("Var", 2:500)),
                          "position" = seq(from = 1, to = 1000, by = 2),
                          stringsAsFactors = FALSE)
  expect_error(cluster_positions(position = list(position1, position2)),
               "The second column of the input position is required to encode unique positions of the corresponding variable / columns in x. For multiple data sets, the combined version without duplicated rows (same variable name and position) is considered.",
               fixed = TRUE)

  position1 <- data.frame("var.names" = paste0("Var", 1:500),
                          "position" = seq(from = 1, to = 1000, by = 2),
                          stringsAsFactors = FALSE)
  position2 <- data.frame("var.names" = paste0("Var", 1:500),
                          "position" = c(seq(from = 1, to = 996, by = 2), 87, 12000),
                          stringsAsFactors = FALSE)
  expect_error(cluster_positions(position = list(position1, position2)),
               "The values in the first column of position (column names of x) are not unique. For multiple data sets, the combined version without duplicated rows (same variable name and position) is considered.",
               fixed = TRUE)

  # NA's in position argument
  position.org <- data.frame("var.names" = paste0("Var", 1:500),
                             "position" = seq(from = 1, to = 1000, by = 2),
                             stringsAsFactors = FALSE)
  position <- position.org
  position[29:30, 2] <- NA
  expect_error(cluster_positions(position = position),
               "There are missing values in the input position.")

  position <- position.org
  position[29:30, 1] <- NA
  expect_error(cluster_positions(position = position),
               "There are missing values in the input position.")

  position <- position.org
  position[30, 2] <- NA
  expect_error(cluster_positions(position = position),
               "There are missing values in the input position.")

  position <- position.org
  position[30, 1] <- NA
  expect_error(cluster_positions(position = position),
               "There are missing values in the input position.")

  # NA's in block argument
  position <- data.frame("var.names" = paste0("Var", 1:500),
                         "position" = seq(from = 1, to = 1000, by = 2),
                         stringsAsFactors = FALSE)
  block <- data.frame(paste0("Var", 1:500), rep(c(1, 2), each = 250),
                      stringsAsFactors = FALSE)
  block[29:30, 2] <- NA
  expect_error(cluster_positions(position = position, block = block),
               "There are missing values in the input block.")
})

test_that("cluster_positions: check output", {
  set.seed(884)

  position <- data.frame("var.names" = paste0("Var", 1:500),
                         "position" = seq(from = 1, to = 1000, by = 2),
                         stringsAsFactors = FALSE)
  position <- position[sample(x = 1:500, size = 500), ]
  d <- cluster_positions(position = position)$res.tree[[1]]
  expect_equal(labels(d), paste0("Var", 1:500))

  position <- data.frame("var.names" = paste0("Var", 1:500),
                         "position" = seq(from = 1000, to = 1, by = -2),
                         stringsAsFactors = FALSE)
  position <- position[sample(x = 1:500, size = 500), ]
  d <- cluster_positions(position = position)$res.tree[[1]]
  expect_equal(labels(d), paste0("Var", 500:1))

  # plot(d <- cluster_positions(position = position)$res.tree[[1]])

  ind <- sample(1:1000, 1000)
  position <- data.frame("var.names" = paste0("SNP.", 1:1000),
                         "position" = ind,
                         stringsAsFactors = FALSE)
  d <- cluster_positions(position = position)$res.tree[[1]]
  expect_equal(labels(d), paste0("SNP.", 1:1000)[order(ind)])

  # Check that it works with decimal values and possible negative values as
  # positions.
  ind <- sample(rnorm(1000), 1000)
  position <- data.frame("var.names" = paste0("SNP.", 1:1000),
                         "position" = ind,
                         stringsAsFactors = FALSE)
  d <- cluster_positions(position = position)$res.tree[[1]]
  expect_equal(labels(d), paste0("SNP.", 1:1000)[order(ind)])

  position <- data.frame("var.names" = c("SNP.1", "SNP.3", "SNP.2"),
                         "position" = c(4, 6.66, 7.75),
                         stringsAsFactors = FALSE)
  d <- cluster_positions(position = position)$res.tree[[1]]
  expect_equal(labels(d), c("SNP.1", "SNP.3", "SNP.2"))
  # plot(d)

  position <- data.frame("var.names" = c("SNP.1", "SNP.3", "SNP.2", "SNP.4", "SNP.5"),
                         "position" = c(4, 6.66, 7.75, 1, 2.74),
                         stringsAsFactors = FALSE)
  d <- cluster_positions(position = position)$res.tree[[1]]
  expect_equal(labels(d), c("SNP.4", "SNP.5", "SNP.1", "SNP.3", "SNP.2"))
  # plot(d)

  position1 <- data.frame("var.names" = paste0("Var", 1:500),
                          "position" = seq(from = 1, to = 1000, by = 2),
                          stringsAsFactors = FALSE)
  position2 <- data.frame("var.names" = paste0("Var", 5:505),
                          "position" = seq(from = 9, to = 1010, by = 2),
                          stringsAsFactors = FALSE)
  d <- cluster_positions(position = list(position1, position2))$res.tree[[1]]
  expect_equal(labels(d), paste0("Var", 1:505))

  position1 <- data.frame("var.names" = paste0("Var", 1:500),
                          "position" = seq(from = 1, to = 1000, by = 2),
                          stringsAsFactors = FALSE)
  position2 <- data.frame("var.names" = paste0("Var", 501:1000),
                          "position" = seq(from = 8001, to = 8500, by = 1),
                          stringsAsFactors = FALSE)
  d <- cluster_positions(position = list(position1, position2))$res.tree[[1]]
  expect_equal(labels(d), paste0("Var", 1:1000))

  position1 <- data.frame("var.names" = c("SNP.1", "SNP.3", "SNP.2", "SNP.4", "SNP.5"),
                          "position" = c(4, 6.66, 7.75, 1, 2.74),
                          stringsAsFactors = FALSE)
  position2 <- data.frame("var.names" = c("SNP.1", "SNP.20", "SNP.14", "SNP.19", "SNP.77"),
                          "position" = c(4, 74, 1.1, 6, 4.5),
                          stringsAsFactors = FALSE)
  d <- cluster_positions(position = list(position1, position2))$res.tree[[1]]
  expect_equal(labels(d), c("SNP.4", "SNP.14", "SNP.5", "SNP.1", "SNP.77",
                            "SNP.19", "SNP.3", "SNP.2", "SNP.20"))
  # plot(d)
})


