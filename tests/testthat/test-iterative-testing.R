# tests if we apply our mc correction, then we get exactly the p-values back

# check MD.correction => maybe print them or return them as p-values MD.factor * 1e-10 or os

# use F-test: compare some easy examples clculated by hand

# since not hier.adj. has to be made.... exact p-values can be compared....


require("testthat")

## random number generator
RNGkind("L'Ecuyer-CMRG")

# dummy test function
# The function returns the p-vlaue which we specify in arg.all.fix
test.func.1 <- function(x, y, clvar, colnames.cluster,
                        arg.all, arg.all.fix, mod.large,
                        mod.small) {

  # browser()
  # We want to be able to choose which cluster is significant
  check.sig <- lapply(arg.all.fix$sig,
                      function(x, colnames.cluster) {
                        all(colnames.cluster %in% x) & (length(colnames.cluster) == length(x))},
                      colnames.cluster = colnames.cluster)

  pval <- if(sum(do.call(c, check.sig)) > 0) { # ==1 would be enough if arg.all.fix$sig does not contain duplicates
    arg.all.fix$pval
  } else {
    0.5
  }

  return(list("pval" = pval, "mod.small" = NULL))
}

test.func.1.simple <- function(x, y, clvar, colnames.cluster,
                        arg.all, arg.all.fix, mod.large,
                        mod.small) {
  pval <- 1e-3

  return(list("pval" = pval, "mod.small" = NULL))
}

# # low-dimensional partial F-test
# test.func.2 <- function(x, y, clvar, colnames.cluster,
#                         arg.all, arg.all.fix, mod.large,
#                         mod.small) {
#
#   ### calculate smaller model ###
#   # generate design matrices
#   setdiff.cluster <- setdiff(colnames(x), colnames.cluster)
#
#   # data.large <- cbind(clvar, x)
#   data.small <- cbind(clvar, x[, setdiff.cluster]) # This results in a matrix although it might only have one column :-)
#   if (ncol(data.small) == 0) {data.small <- rep(1, length(y))}
#
#   # calculate smaller model
#   mod.small <- lm(y ~ data.small) #, model = FALSE, qr = FALSE)
#
#   ### compare the models ###
#   # partial F test
#   pval <- anova(mod.small,
#                 mod.large, # stats::lm(y ~ data.large),
#                 test = "F")$P[2]
#
#   return(list("pval" = pval, "mod.small" = NULL))
# }




test_that("iterative_testing: check multiple testing adjustment and aggregation", {

  ## dummy test function and one data set
  set.seed(3)
  x <- matrix(rnorm(1000), ncol = 10)
  colnames(x) <- paste0("Var", 1:10)
  y <- x %*% c(2, 1.8, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  clvar <- NULL

  dendr <- cluster_vars(x = x)
  # plot(dendr$res.tree$value)

  pval.res <- pval <- 1e-3
  # names(pval.res) <- "pval"

  # oder of testing respectively the order of the heights is indicated in comments
  cluster_test <- list(c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6",
                         "Var7", "Var8", "Var9", "Var10"), # 0
                       c("Var1", "Var2", "Var6"), # 1
                       c("Var3", "Var4", "Var5", "Var7", "Var8", "Var9", "Var10"), # 1
                       c("Var8", "Var10"), # 2
                       c("Var3", "Var4", "Var5", "Var7", "Var9"), # 2
                       c("Var2", "Var6"), # 3
                       "Var1", # 3
                       c("Var3", "Var9"), # 4
                       c("Var4", "Var5", "Var7"), # 4
                       "Var2", # 5
                       "Var6", # 5
                       c("Var4", "Var5"), # 6
                       "Var7", # 6
                       "Var3", # 7
                       "Var9", # 7
                       "Var8", # 8
                       "Var10", # 8
                       "Var4", # 9
                       "Var5") # 9
  # global null only
  res <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                      clvar = list(clvar),
                                      name.block = NULL,
                                      dendr = dendr$res.tree$value,
                                      res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                        "pval" = 1e-3),
                                                       "mod.small" = list(NULL),
                                                       "MD.factor" = 1),
                                      test.func = test.func.1,
                                      arg.all = list(NULL),
                                      arg.all.fix = list(pval = pval,
                                                         sig = cluster_test[1]),
                                      alpha = 0.05,
                                      agg.method = "Tippett",
                                      hier.adj = FALSE,
                                      mt.adj = "SBH",
                                      mod.large = list(list(NULL)),
                                      verbose = FALSE,
                                      stouffer.weights = 1)
  res.compare <- do.call(rbind, res$signif.clusters)

  expect_identical(res.compare[, "pval"], list(pval = pval.res))
  expect_identical(res.compare[, "colnames.cluster"], list(colnames.cluster = cluster_test[[1]]))

  ## check mc.ajd = "SBH" ##
  # We define what the output should look like and then check if that is correct.
  # Especially, if the multiple testing adjustment is correct.

  # {1, 2, 6}, {8, 10}, {3, 4, 5, 7, 9}
  pval.res <- pval <- 1e-3
  names(pval.res) <- "pval"

  res <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                      clvar = list(clvar),
                                      name.block = NULL,
                                      dendr = dendr$res.tree$value,
                                      res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                        "pval" = 1e-3),
                                                       "mod.small" = list(NULL),
                                                       "MD.factor" = 1),
                                      test.func = test.func.1,
                                      arg.all = list(NULL),
                                      arg.all.fix = list(pval = pval,
                                                         sig = cluster_test[1:5]),
                                      alpha = 0.05,
                                      agg.method = "Tippett",
                                      hier.adj = FALSE,
                                      mt.adj = "SBH",
                                      mod.large = list(list(NULL)),
                                      verbose = FALSE,
                                      stouffer.weights = 1)
  res.compare <- do.call(rbind, res$signif.clusters)

  expect_equal(res.compare[, "pval"], list(pval.res / 0.3,
                                           pval.res / (0.7 / 7 * 5),
                                           pval.res / (0.7 / 7 * 2)))
  check.names <- apply(cbind(res.compare[, "colnames.cluster"], cluster_test[c(2, 5, 4)]), 1,
        function(x) all(x[[1]] %in% x[[2]]) & all(x[[2]] %in% x[[1]]))
  expect_true(all(check.names))

  # {1}, {4}, {5}
  pval.res <- pval <- 1e-3
  names(pval.res) <- "pval"

  res <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                      clvar = list(clvar),
                                      name.block = NULL,
                                      dendr = dendr$res.tree$value,
                                      res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                        "pval" = 1e-3),
                                                       "mod.small" = list(NULL),
                                                       "MD.factor" = 1),
                                      test.func = test.func.1,
                                      arg.all = list(NULL),
                                      arg.all.fix = list(pval = pval,
                                                         sig = cluster_test[c(1, 2, 3, 5, 7,
                                                                              9, 12, 18, 19)]),
                                      alpha = 0.05,
                                      agg.method = "Tippett",
                                      hier.adj = FALSE,
                                      mt.adj = "SBH",
                                      mod.large = list(list(NULL)),
                                      verbose = FALSE,
                                      stouffer.weights = 1)
  res.compare <- do.call(rbind, res$signif.clusters)

  expect_equal(res.compare[, "pval"], list(pval.res / (1 / 8), # L_{poss. sig} = 8 and no MD found
                                           pval.res / ((1 - 1/8) * 1 / 2), # L_{poss. sig} = 2 and one MD found
                                           pval.res / ((1 - 1/8) * 1 / 2))) # L_{poss. sig} = 2 and one MD found
  check.names <- apply(cbind(res.compare[, "colnames.cluster"], cluster_test[c(7, 18, 19)]), 1,
                       function(x) all(x[[1]] %in% x[[2]]) & all(x[[2]] %in% x[[1]]))
  expect_true(all(check.names))

  # {2, 6}, {3, 9}, {4, 5}
  pval.res <- pval <- 1e-3
  names(pval.res) <- "pval"

  res <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                      clvar = list(clvar),
                                      name.block = NULL,
                                      dendr = dendr$res.tree$value,
                                      res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                        "pval" = 1e-3),
                                                       "mod.small" = list(NULL),
                                                       "MD.factor" = 1),
                                      test.func = test.func.1,
                                      arg.all = list(NULL),
                                      arg.all.fix = list(pval = pval,
                                                         sig = cluster_test[c(1, 2, 3, 5, 6, 8,
                                                                              9, 12)]),
                                      alpha = 0.05,
                                      agg.method = "Tippett",
                                      hier.adj = FALSE,
                                      mt.adj = "SBH",
                                      mod.large = list(list(NULL)),
                                      verbose = FALSE,
                                      stouffer.weights = 1)
  res.compare <- do.call(rbind, res$signif.clusters)

  expect_equal(res.compare[, "pval"], list(pval.res / (2 / 8), # L_{poss. sig} = 8 and no MD found
                                           pval.res / (2 / 7), # L_{poss. sig} = 7 and no MD found
                                           pval.res / ((1 - 2/8) * 2 / 5))) # L_{poss. sig} = 5 and one MD found
  check.names <- apply(cbind(res.compare[, "colnames.cluster"], cluster_test[c(6, 8, 12)]), 1,
                       function(x) all(x[[1]] %in% x[[2]]) & all(x[[2]] %in% x[[1]]))
  expect_true(all(check.names))

  # {1}, {4}, {5}
  pval.res <- pval <- 1e-3
  names(pval.res) <- "pval"

  res <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                      clvar = list(clvar),
                                      name.block = NULL,
                                      dendr = dendr$res.tree$value,
                                      res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                        "pval" = 1e-3),
                                                       "mod.small" = list(NULL),
                                                       "MD.factor" = 1),
                                      test.func = test.func.1,
                                      arg.all = list(NULL),
                                      arg.all.fix = list(pval = pval,
                                                         sig = cluster_test[c(1, 2, 3, 5, 7,
                                                                              9, 12, 18, 19)]),
                                      alpha = 0.05,
                                      agg.method = "Tippett",
                                      hier.adj = FALSE,
                                      mt.adj = "SBH",
                                      mod.large = list(list(NULL)),
                                      verbose = FALSE,
                                      stouffer.weights = 1)
  res.compare <- do.call(rbind, res$signif.clusters)

  expect_equal(res.compare[, "pval"], list(pval.res / (1 / 8), # L_{poss. sig} = 8 and no MD found
                                           pval.res / ((1 - 1/8) * 1 / 2), # L_{poss. sig} = 2 and one MD found
                                           pval.res / ((1 - 1/8) * 1 / 2))) # L_{poss. sig} = 2 and one MD found
  check.names <- apply(cbind(res.compare[, "colnames.cluster"], cluster_test[c(7, 18, 19)]), 1,
                       function(x) all(x[[1]] %in% x[[2]]) & all(x[[2]] %in% x[[1]]))
  expect_true(all(check.names))

  # {8}
  pval.res <- pval <- 1e-3
  names(pval.res) <- "pval"

  res <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                      clvar = list(clvar),
                                      name.block = NULL,
                                      dendr = dendr$res.tree$value,
                                      res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                        "pval" = 1e-3),
                                                       "mod.small" = list(NULL),
                                                       "MD.factor" = 1),
                                      test.func = test.func.1,
                                      arg.all = list(NULL),
                                      arg.all.fix = list(pval = pval,
                                                         sig = cluster_test[c(1, 3, 4, 16)]),
                                      alpha = 0.05,
                                      agg.method = "Tippett",
                                      hier.adj = FALSE,
                                      mt.adj = "SBH",
                                      mod.large = list(list(NULL)),
                                      verbose = FALSE,
                                      stouffer.weights = 1)
  res.compare <- do.call(rbind, res$signif.clusters)

  expect_equal(res.compare[, "pval"], list(pval = pval.res / (1 / 2))) # L_{poss. sig} = 2 and no MD found
  check.names <- apply(cbind(res.compare[, "colnames.cluster"], cluster_test[c(16)]), 1,
                       function(x) all(x[[1]] %in% x[[2]]) & all(x[[2]] %in% x[[1]]))
  expect_true(all(check.names))

  # ## Commented on November 3rd
  # ## dummy test function and three data set
  # ## run hierarchy with block
  # set.seed(3)
  # x1 <- matrix(rnorm(5000), ncol = 50)
  # x2 <- matrix(rnorm(5000), ncol = 50)
  # x3 <- matrix(rnorm(5000), ncol = 50)
  # colnames(x1) <- colnames(x2) <- colnames(x3) <- paste0("Var", 1:50)
  # y1 <- x1 %*% c(2, 1.8, 0, 0, 0, 0.25, 0, 0, 0, 0,
  #                1.2, rep(0, 38), 0.1) + rnorm(100)
  # y2 <- x2 %*% c(0.1, 0.1, 0, 0, 0, 0.25, 0, 0, 0, 0,
  #                0.87, rep(0, 38), 0.2) + rnorm(100)
  # y3 <- x3 %*% c(-0.05, -0.7, 0, 0, 0, 0.25, 0, 0, 0, 0,
  #                0.14, rep(0, 38), 0.05) + rnorm(100)
  # clvar <- NULL
  # 
  # # dummy function which returns NULL
  # compMOD_changing <- function(...) NULL
  # 
  # pval <- 1e-3
  # 
  # # Block
  # block <- data.frame("var.names" = paste0("Var", 1:50),
  #                     "blocks" = c(rep(1, 10), rep(2, 32), rep(3, 8)),
  #                     stringsAsFactors = FALSE)
  # 
  # dendr <- cluster_vars(x = list(x1, x2, x3), block = block)
  # 
  # cluster_test <- list(c(paste0("Var", 1:10), paste0("Var", 11: 42), paste0("Var", 43:50)),
  #                      paste0("Var", 1:10),
  #                      paste0("Var", 11: 42),
  #                      paste0("Var", 43:50),
  #                      # block 1
  #                      c("Var8", "Var3", "Var9"),
  #                      c("Var1", "Var6", "Var5", "Var10", "Var2","Var4", "Var7"),
  #                      # block 2
  #                      # not used
  #                      # block 3
  #                      c("Var46", "Var44", "Var49"),
  #                      c("Var43", "Var47", "Var45", "Var48", "Var50"))
  # 
  # # first / top cluster of every block
  # res <- run_hierarchy(x = list(x1, x2, x3), y = list(y1, y2, y3), clvar = clvar,
  #                      dendr = dendr, test.func = test.func.1, # dummy test funct.
  #                      compMOD.same = compMOD_changing, # it only returns NULL
  #                      compMOD.changing = compMOD_changing,
  #                      arg.all.fix = list(pval = pval,
  #                                         sig = cluster_test[c(1:4)]),
  #                      hier.adj = FALSE,
  #                      mt.adj = "SBH")
  # 
  # fc.tippett <- function(x) 1 - (1 - min(x))^(3)
  # 
  # expect_identical(res$p.value, c(fc.tippett(pval / (32 / 50)),
  #                                 fc.tippett(pval / (10 / 50)),
  #                                 fc.tippett(pval / (8 / 50))))
  # expect_identical(res$block, as.character(c(2, 1, 3)))
  # 
  # # first / top cluster of frist block only
  # res <- run_hierarchy(x = list(x1, x2, x3), y = list(y1, y2, y3), clvar = clvar,
  #                      dendr = dendr, test.func = test.func.1, # dummy test funct.
  #                      compMOD.same = compMOD_changing, # it only returns NULL
  #                      compMOD.changing = compMOD_changing,
  #                      arg.all.fix = list(pval = pval,
  #                                         sig = cluster_test[c(1:2)]),
  #                      hier.adj = FALSE,
  #                      mt.adj = "SBH")
  # 
  # expect_identical(res$p.value, c(fc.tippett(pval/ (10 / 50)), NA, NA))
  # expect_identical(res$block, as.character(c(1, 2, 3)))
  # 
  # # one cluster on the second level of the first block only (3rd level in entire tree)
  # res <- run_hierarchy(x = list(x1, x2, x3), y = list(y1, y2, y3), clvar = clvar,
  #                      dendr = dendr, test.func = test.func.1, # dummy test funct.
  #                      compMOD.same = compMOD_changing, # it only returns NULL
  #                      compMOD.changing = compMOD_changing,
  #                      arg.all.fix = list(pval = pval,
  #                                         sig = cluster_test[c(1:2, 6)]),
  #                      hier.adj = FALSE,
  #                      mt.adj = "SBH")
  # 
  # expect_identical(res$p.value, c(fc.tippett(pval/ (7 / 10)), NA, NA))
  # expect_identical(res$block, as.character(c(1, 2, 3)))
  # 
  # # two cluster on the second level of the first and thrid block only (3rd level in entire tree)
  # res <- run_hierarchy(x = list(x1, x2, x3), y = list(y1, y2, y3), clvar = clvar,
  #                      dendr = dendr, test.func = test.func.1, # dummy test funct.
  #                      compMOD.same = compMOD_changing, # it only returns NULL
  #                      compMOD.changing = compMOD_changing,
  #                      arg.all.fix = list(pval = pval,
  #                                         sig = cluster_test[c(1:2, 4, 6, 8)]),
  #                      hier.adj = FALSE,
  #                      mt.adj = "SBH")
  # 
  # # devided by ... / 18 because the second block could not be shown to be significant and
  # # therefore its part is inherited
  # expect_identical(res$p.value, c(fc.tippett(pval/ (10 / 18 * 7 / 10)), fc.tippett(pval/ (8 / 18 * 5 / 8)), NA))
  # expect_identical(res$block, as.character(c(1, 3, 2)))
  # ## End: Commented on November 3rd

})



test_that("iterative_testing: compare multiple testing adjustment", {
  
  ## dummy test function and one data set
  set.seed(3)
  x <- matrix(rnorm(1000), ncol = 10)
  colnames(x) <- paste0("Var", 1:10)
  y <- x %*% c(2, 1.8, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  clvar <- NULL
  
  dendr <- cluster_vars(x = x)
  # plot(dendr$res.tree$value)
  
  pval.res <- pval <- 1e-3
  # names(pval.res) <- "pval"
  
  # oder of testing respectively the order of the heights is indicated in comments
  cluster_test <- list(c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6",
                         "Var7", "Var8", "Var9", "Var10"), # 0
                       c("Var1", "Var2", "Var6"), # 1
                       c("Var3", "Var4", "Var5", "Var7", "Var8", "Var9", "Var10"), # 1
                       c("Var8", "Var10"), # 2
                       c("Var3", "Var4", "Var5", "Var7", "Var9"), # 2
                       c("Var2", "Var6"), # 3
                       "Var1", # 3
                       c("Var3", "Var9"), # 4
                       c("Var4", "Var5", "Var7"), # 4
                       "Var2", # 5
                       "Var6", # 5
                       c("Var4", "Var5"), # 6
                       "Var7", # 6
                       "Var3", # 7
                       "Var9", # 7
                       "Var8", # 8
                       "Var10", # 8
                       "Var4", # 9
                       "Var5") # 9
  # global null only
  res1 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                      clvar = list(clvar),
                                      name.block = NULL,
                                      dendr = dendr$res.tree$value,
                                      res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                        "pval" = 1e-3),
                                                       "mod.small" = list(NULL),
                                                       "MD.factor" = 1),
                                      test.func = test.func.1,
                                      arg.all = list(NULL),
                                      arg.all.fix = list(pval = pval,
                                                         sig = cluster_test[1]),
                                      alpha = 0.05,
                                      agg.method = "Tippett",
                                      hier.adj = FALSE,
                                      mt.adj = "SBH",
                                      mod.large = list(list(NULL)),
                                      verbose = FALSE,
                                      stouffer.weights = 1)
  res.compare1 <- do.call(rbind, res1$signif.clusters)
  
  res3 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[1]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "dpBF",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare3 <- do.call(rbind, res3$signif.clusters)
  
  expect_identical(res.compare1[, "pval"], res.compare3[, "pval"])

  # {1, 2, 6}, {8, 10}, {3, 4, 5, 7, 9}
  pval <- 1e-3

  res1 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[1:5]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "SBH",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare1 <- do.call(rbind, res1$signif.clusters)

  res3 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[1:5]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "dpBF",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare3 <- do.call(rbind, res3$signif.clusters)
  
  res.first <- vapply(seq_len(length(res.compare3[, "pval"])), function(ind, x1, x2) x1[[ind]] <= x2[[ind]], 
                      FUN.VALUE = TRUE, x1 = res.compare1[, "pval"], 
                      x2 = res.compare3[, "pval"])
  expect_true(all(res.first))

  # {1}, {4}, {5}
  pval <- 1e-3
  
  res1 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[c(1, 2, 3, 5, 7,
                                                                               9, 12, 18, 19)]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "SBH",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare1 <- do.call(rbind, res1$signif.clusters)

  res3 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[c(1, 2, 3, 5, 7,
                                                                               9, 12, 18, 19)]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "dpBF",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare3 <- do.call(rbind, res3$signif.clusters)
  
  res.first <- vapply(seq_len(length(res.compare3[, "pval"])), function(ind, x1, x2) x1[[ind]] <= x2[[ind]], 
                      FUN.VALUE = TRUE, x1 = res.compare1[, "pval"], 
                      x2 = res.compare3[, "pval"])
  expect_true(all(res.first))

  # {2, 6}, {3, 9}, {4, 5}
  pval <- 1e-3
  
  res1 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[c(1, 2, 3, 5, 6, 8,
                                                                               9, 12)]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "SBH",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare1 <- do.call(rbind, res1$signif.clusters)
  
  res3 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[c(1, 2, 3, 5, 6, 8,
                                                                               9, 12)]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "dpBF",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare3 <- do.call(rbind, res3$signif.clusters)
  
  res.first <- vapply(seq_len(length(res.compare3[, "pval"])), function(ind, x1, x2) x1[[ind]] <= x2[[ind]], 
                      FUN.VALUE = TRUE, x1 = res.compare1[, "pval"], 
                      x2 = res.compare3[, "pval"])
  expect_true(all(res.first))

  # {1}, {4}, {5}
  pval <- 1e-3
  
  res1 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[c(1, 2, 3, 5, 7,
                                                                               9, 12, 18, 19)]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "SBH",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare1 <- do.call(rbind, res1$signif.clusters)
  
  res3 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[c(1, 2, 3, 5, 7,
                                                                               9, 12, 18, 19)]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "dpBF",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare3 <- do.call(rbind, res3$signif.clusters)
  
  res.first <- vapply(seq_len(length(res.compare3[, "pval"])), function(ind, x1, x2) x1[[ind]] <= x2[[ind]], 
                      FUN.VALUE = TRUE, x1 = res.compare1[, "pval"], 
                      x2 = res.compare3[, "pval"])
  expect_true(all(res.first))

  # {8}
  
  pval <- 1e-3
  
  res1 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[c(1, 3, 4, 16)]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "SBH",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare1 <- do.call(rbind, res1$signif.clusters)
  
  res3 <- hierbase:::iterative_testing(x = list(x), y = list(y),
                                       clvar = list(clvar),
                                       name.block = NULL,
                                       dendr = dendr$res.tree$value,
                                       res.block = list("cluster" = list("colnames.cluster" = paste0("Var", 1:10),
                                                                         "pval" = 1e-3),
                                                        "mod.small" = list(NULL),
                                                        "MD.factor" = 1),
                                       test.func = test.func.1,
                                       arg.all = list(NULL),
                                       arg.all.fix = list(pval = pval,
                                                          sig = cluster_test[c(1, 3, 4, 16)]),
                                       alpha = 0.05,
                                       agg.method = "Tippett",
                                       hier.adj = FALSE,
                                       mt.adj = "dpBF",
                                       mod.large = list(list(NULL)),
                                       verbose = FALSE,
                                       stouffer.weights = 1)
  res.compare3 <- do.call(rbind, res3$signif.clusters)
  
  res.first <- vapply(seq_len(length(res.compare3[, "pval"])), function(ind, x1, x2) x1[[ind]] <= x2[[ind]], 
                      FUN.VALUE = TRUE, x1 = res.compare1[, "pval"], 
                      x2 = res.compare3[, "pval"])
  expect_true(all(res.first))

})












# ## test from bottom or test starting from some level
#
# lapply(subclust, function(x)  attr(x, "height"))
#
#
#
# # run all test from hierinf
#
#
#
#
#
#
#
#
#
# # using iris data simply for reproducible example
# data(iris)
# d <- data.frame(scale(iris[,1:4]))
# hc <- hclust(dist(d))
# plot(hc)
#
# unnest <- function(x) { # from Vlo's answer
#   if(is.null(names(x))) x
#   else c(list(all=unname(unlist(x))), do.call(c, lapply(x, unnest)))
# }
#
# cuts <- hc$height + 1e-9
#
# min_size <- 10
# smallest <- 0
# i <- 0
#
# while(smallest < min_size & i <= length(cuts)){
#   h_i <- cuts[i <- i+1]
#   if(i > length(cuts)){
#     warning("Couldn't find a cluster big enough.")
#   }
#   else  smallest <-
#       Reduce(min,
#              lapply(X = unnest(cut(as.dendrogram(hc), h=h_i)$lower),
#                     FUN = attr, which = "members") ) # from lukeA's comment
# }
# h_i # returns desired output: [1] 3.79211
