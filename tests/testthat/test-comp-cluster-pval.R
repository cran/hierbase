require("testthat")

## random number generator
RNGkind("L'Ecuyer-CMRG")

# dummy test function
# The function returns the p-vlaue which we specify in arg.all.fix
test.func.1 <- function(x, y, clvar, colnames.cluster,
                        arg.all, arg.all.fix, mod.large,
                        mod.small) {

  pval <- arg.all.fix$pval

  return(list("pval" = pval, "mod.small" = NULL))
}

# low-dimensional partial F-test
test.func.2 <- function(x, y, clvar, colnames.cluster,
                      arg.all, arg.all.fix, mod.large,
                      mod.small) {

  ### calculate smaller model ###
  # generate design matrices
  setdiff.cluster <- setdiff(colnames(x), colnames.cluster)

  # data.large <- cbind(clvar, x)
  data.small <- cbind(clvar, x[, setdiff.cluster]) # This results in a matrix although it might only have one column :-)
  if (ncol(data.small) == 0) {data.small <- rep(1, length(y))}

  # calculate smaller model
  mod.small <- lm(y ~ data.small) #, model = FALSE, qr = FALSE)

  ### compare the models ###
  # partial F test
  pval <- anova(mod.small,
                mod.large, # stats::lm(y ~ data.large),
                test = "F")$P[2]

  return(list("pval" = pval, "mod.small" = NULL))
}




test_that("comp_cluster_pval: check multiple testing adjustment and aggregation", {

  ## dummy test function and one data set
  x <- matrix(rnorm(1000), ncol = 10)
  colnames(x) <- paste0("Var", 1:10)
  y <- x %*% c(2, 1.8, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  clvar <- NULL

  pval.res <- pval <- 1e-3
  names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x), y = list(y),
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = NULL,
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett",
                                            hier.adj = FALSE,
                                            mt.adj = "SBH",
                                            MD.factor = 1,
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = 1)$cluster$pval,
               pval.res)

  pval.res <- pval <- 1e-20 # changed
  names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x), y = list(y),
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = NULL,
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett",
                                            hier.adj = FALSE,
                                            mt.adj = "SBH",
                                            MD.factor = 1,
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = 1)$cluster$pval,
               pval.res)

  pval <- 1e-3
  pval.res <- pval.min <- 0.03
  # names(pval.res) <- names(pval.min) <- "pval"
  # the function max removes the names
  expect_equal(hierbase:::comp_cluster_pval(x = list(x), y = list(y),
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = NULL,
                                            minimal.pval = pval.min, # changed
                                            agg.method = "Tippett",
                                            hier.adj = TRUE, # changed
                                            mt.adj = "SBH",
                                            MD.factor = 1,
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = 1)$cluster$pval,
               pval.res)

  pval <- 1e-3
  pval.res <- pval / 0.35
  names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x), y = list(y),
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = NULL,
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett",
                                            hier.adj = FALSE,
                                            mt.adj = "SBH", # changed
                                            MD.factor = 0.35, # changed
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = 1)$cluster$pval,
               pval.res)

  pval <- 1e-3
  pval.res <- pval
  names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x), y = list(y),
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = NULL,
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett",
                                            hier.adj = FALSE,
                                            mt.adj = "none", # changed
                                            MD.factor = 0.35, # changed
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = 1)$cluster$pval,
               pval.res)
  
  
  pval <- 1e-3
  pval.res <- pval * 10 / 4
  names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x), y = list(y),
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = c("abb", "aab", "aaa", "bbb"),
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett",
                                            hier.adj = FALSE,
                                            mt.adj = "dpBF", # changed
                                            MD.factor = 1, 
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = 1)$cluster$pval,
               pval.res)

  pval <- 1e-3
  pval.res <- pval / 0.35
  names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x), y = list(y),
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = c("abb", "aab", "aaa", "bbb"),
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett",
                                            hier.adj = FALSE,
                                            mt.adj = "SBH", # changed
                                            MD.factor = 0.35, # changed
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = 1)$cluster$colnames.cluster,
               c("abb", "aab", "aaa", "bbb"))
  
  ## dummy test function and three data set
  set.seed(3)
  x1 <- matrix(rnorm(1000), ncol = 10)
  x2 <- matrix(rnorm(1000), ncol = 10)
  x3 <- matrix(rnorm(1000), ncol = 10)
  colnames(x1) <- colnames(x2) <- colnames(x3) <- paste0("Var", 1:10)
  y1 <- x1 %*% c(2, 1.8, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  y2 <- x2 %*% c(0.1, 0.1, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  y3 <- x3 %*% c(-0.05, -0.7, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  clvar <- NULL

  pval <- 1e-3
  pval.res <- 1 - (1 - min(pval))^(3)
  # names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x1, x2, x3), # changed
                                            y = list(y1, y2, y3), # changed
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = NULL,
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett", # changed
                                            hier.adj = FALSE,
                                            mt.adj = "none", # (changed)
                                            MD.factor = 0.35,
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = c(0.25, 0.25, 0.5) # changed
                                            )$cluster$pval,
               pval.res)

  pval <- 1e-3
  pval.res <- pnorm(sum(c(0.25, 0.25, 0.5) * qnorm(c(pval, pval, pval))))
  # names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x1, x2, x3), # changed
                                            y = list(y1, y2, y3), # changed
                                            clvar = list(clvar),
                                            test.func = test.func.1,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = NULL,
                                            minimal.pval = 0.03,
                                            agg.method = "Stouffer", # changed
                                            hier.adj = FALSE,
                                            mt.adj = "none",
                                            MD.factor = 0.35,
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = c(0.25, 0.25, 0.5) # changed
                                            )$cluster$pval,
  pval.res)

  # dummy test function
  # The function returns the p-vlaue which we specify in arg.all.fix
  test.func.1.a <- function(x, y, clvar, colnames.cluster,
                          arg.all, arg.all.fix, mod.large,
                          mod.small) {

    pval <- x[1, 1]

    return(list("pval" = pval, "mod.small" = NULL))
  }

  x1[1, 1] <- 1e-3
  x2[1, 1] <- 1e-10
  x3[1, 1] <- 1e-1
  pval.res <- pnorm(sum(c(0.25, 0.25, 0.5) * qnorm(c(1e-3, 1e-10, 1e-1))))
  # names(pval.res) <- "pval"
  expect_equal(hierbase:::comp_cluster_pval(x = list(x1, x2, x3), # changed
                                            y = list(y1, y2, y3), # changed
                                            clvar = list(clvar),
                                            test.func = test.func.1.a,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(pval = pval),
                                            colnames.cluster = NULL,
                                            minimal.pval = 0.03,
                                            agg.method = "Stouffer", # changed
                                            hier.adj = FALSE,
                                            mt.adj = "none",
                                            MD.factor = 0.35,
                                            mod.large = list(NULL),
                                            mod.small = list(NULL),
                                            stouffer.weights = c(0.25, 0.25, 0.5) # changed
                                            )$cluster$pval,
  pval.res)

  ## low-dimensional F-test
  set.seed(3)
  x1 <- matrix(rnorm(1000), ncol = 10)
  x2 <- matrix(rnorm(1000), ncol = 10)
  x3 <- matrix(rnorm(1000), ncol = 10)
  colnames(x1) <- colnames(x2) <- colnames(x3) <- paste0("Var", 1:10)
  y1 <- x1 %*% c(2, 1.8, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  y2 <- x2 %*% c(0.1, 0.1, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  y3 <- x3 %*% c(-0.05, -0.7, 0, 0, 0, 0.25, 0, 0, 0, 0) + rnorm(100)
  clvar <- NULL

  # test a group of five variables
  y <- y1
  mod.large1 <- lm(y ~ x1)
  pval1 <- anova(lm(y1 ~ x1), lm(y1 ~ x1[, 1:5]), test = "F")$P[2]
  y <- y2
  mod.large2 <- lm(y ~ x2)
  pval2 <- anova(lm(y2 ~ x2), lm(y2 ~ x2[, 1:5]), test = "F")$P[2]
  y <- y3
  mod.large3 <- lm(y ~ x3)
  pval3 <- anova(lm(y3 ~ x3), lm(y3 ~ x3[, 1:5]), test = "F")$P[2]

  pval.res <- 1 - (1 - min(c(pval1, pval2, pval3)))^(3)
  expect_equal(hierbase:::comp_cluster_pval(x = list(x1, x2, x3), # changed
                                            y = list(y1, y2, y3), # changed
                                            clvar = list(clvar),
                                            test.func = test.func.2,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(NULL),
                                            colnames.cluster = paste0("Var", 6:10),
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett", # changed
                                            hier.adj = FALSE,
                                            mt.adj = "none",
                                            MD.factor = 0.35,
                                            mod.large = list(mod.large1, mod.large2, mod.large3), # changed
                                            mod.small = list(NULL),
                                            stouffer.weights = c(0.25, 0.25, 0.5))$cluster$pval,
  pval.res)

  # test the global null hypothesis
  y <- y1
  mod.large1 <- lm(y ~ x1)
  pval1 <- anova(lm(y1 ~ x1), lm(y1 ~ 1), test = "F")$P[2]
  y <- y2
  mod.large2 <- lm(y ~ x2)
  pval2 <- anova(lm(y2 ~ x2), lm(y2 ~ 1), test = "F")$P[2]
  y <- y3
  mod.large3 <- lm(y ~ x3)
  pval3 <- anova(lm(y3 ~ x3), lm(y3 ~ 1), test = "F")$P[2]

  pval.res <- 1 - (1 - min(c(pval1, pval2, pval3)))^(3)
  expect_equal(hierbase:::comp_cluster_pval(x = list(x1, x2, x3), # changed
                                            y = list(y1, y2, y3), # changed
                                            clvar = list(clvar),
                                            test.func = test.func.2,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(NULL),
                                            colnames.cluster = paste0("Var", 1:10),
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett", # changed
                                            hier.adj = FALSE,
                                            mt.adj = "none",
                                            MD.factor = 0.35,
                                            mod.large = list(mod.large1, mod.large2, mod.large3), # changed
                                            mod.small = list(NULL),
                                            stouffer.weights = c(0.25, 0.25, 0.5))$cluster$pval,
               pval.res)

  # test a single variable
  y <- y1
  mod.large1 <- lm(y ~ x1)
  pval1 <- anova(lm(y1 ~ x1), lm(y1 ~ x1[, 2:10]), test = "F")$P[2]
  y <- y2
  mod.large2 <- lm(y ~ x2)
  pval2 <- anova(lm(y2 ~ x2), lm(y2 ~ x2[, 2:10]), test = "F")$P[2]
  y <- y3
  mod.large3 <- lm(y ~ x3)
  pval3 <- anova(lm(y3 ~ x3), lm(y3 ~ x3[, 2:10]), test = "F")$P[2]

  pval.res <- max(1 - (1 - min(c(pval1, pval2, pval3)))^(3), .Machine$double.neg.eps)
  expect_identical(hierbase:::comp_cluster_pval(x = list(x1, x2, x3), # changed
                                            y = list(y1, y2, y3), # changed
                                            clvar = list(clvar),
                                            test.func = test.func.2,
                                            arg.all = list(NULL),
                                            arg.all.fix = list(NULL),
                                            colnames.cluster = paste0("Var", 1),
                                            minimal.pval = 0.03,
                                            agg.method = "Tippett", # changed
                                            hier.adj = FALSE,
                                            mt.adj = "none",
                                            MD.factor = 0.35,
                                            mod.large = list(mod.large1, mod.large2, mod.large3), # changed
                                            mod.small = list(NULL),
                                            stouffer.weights = c(0.25, 0.25, 0.5))$cluster$pval,
               pval.res)

})




# test_that("test compute_cluster_pval: for multisample splitting", {
#   ## simulate index
#   p <- 10
#   B <- 50
#
#   ## simulate data
#   require(MASS)
#
#   r1 <- gen_one(n = 800, p = p, seed1 = 9229, ind.a = c(7, 3), seed3 = 8,
#                 num_clvar = 3, coef_clvar = c(0.5, 0.25, 1.25))
#   r2 <- gen_one(n = 200, p = p, seed1 = 929, ind.a = c(7, 3), seed3 = 99,
#                 num_clvar = 3, coef_clvar = c(0.5, 0.25, 1.25))
#   r3 <- gen_one(n = 350, p = p, seed1 = 99, ind.a = c(7, 3), seed3 = 100,
#                 num_clvar = 3, coef_clvar = c(0.5, 0.25, 1.25))
#   r4 <- gen_one(n = 50, p = p, seed1 = 9, ind.a = c(7, 3), seed3 = 1111,
#                 num_clvar = 3, coef_clvar = c(0.5, 0.25, 1.25))
#
#   x <- list(r1$x, r2$x, r3$x, r4$x)
#   y <- list(r1$y, r2$y, r3$y, r4$y)
#   clvar <- list(r1$clvar, r2$clvar, r3$clvar, r4$clvar) # with co-variables
#
#   # Block
#   block <- data.frame("var.names" = paste0("rsid", 1:10),
#                       "blocks" = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
#                       stringsAsFactors = FALSE)
#
#   # cluster the data
#   dendr <- cluster_vars(x = x, block = block)
#   # plot(dendr$res.tree[[1]])
#   # plot(dendr$res.tree[[2]])
#
#   # multisplit
#   set.seed(3)
#   res.multisplit <- hierinf::multisplit(x = x, y = y, clvar = clvar, family = "gaussian",
#                                         B = B)
#
#   # test hierarchy: Tippett
#   arg.all <- res.multisplit
#   arg.all.fix <- list(family = "gaussian")
#
#   ####################################################
#   identical(res.multisplit, arg.all)
#
#   ## cluster c("rsid6", "rsid7", rsid8")
#   mod.large.tmp <- hierbase:::compMOD_large(compMOD_same = compMOD_one_data, x = x, y = y,
#                                             clvar = clvar, arg.all = arg.all,
#                                             arg.all.fix = arg.all.fix)
#   # compMOD_one_data(x = x, y = y, clvar = clvar,
#   #                                 arg.all = arg.all,
#   #                                 arg.all.fix = arg.all.fix)
#   mod.small.tmp <- hierbase:::compMOD_small(compMOD_changing = compMOD_one_data_S, x = x, y = y,
#                                             clvar = clvar, arg.all = arg.all,
#                                             arg.all.fix = arg.all.fix)
#
#   stouffer_weights <- sqrt(c(800, 200, 350, 50) / sum(c(800, 200, 350, 50)))
#   res.oneC <- hierbase:::comp_cluster_pval(x = x, y = y, clvar = clvar,
#                                            test.func = comp_one_data,
#                                            arg.all = arg.all,
#                                            arg.all.fix = arg.all.fix,
#                                            colnames.cluster = c("rsid6", "rsid7", "rsid8"),
#                                            minimal.pval = 1, agg.method = "Stouffer",
#                                            hier.adj = FALSE, mt.adj = "none", MD.factor = 1,
#                                            mod.large = mod.large.tmp, mod.small = mod.small.tmp,
#                                            stouffer.weights = stouffer_weights)
#
#   res.oneC$cluster
#
#   r1.oneC <- comp_one_data(x = x[[1]], y = y[[1]], clvar = clvar[[1]],
#                            arg.all = arg.all[[1]],
#                            arg.all.fix = arg.all.fix,
#                            colnames.cluster = c("rsid6", "rsid7", "rsid8"),
#                            mod.large = mod.large.tmp[[1]],
#                            mod.small = mod.small.tmp[[1]])
#   r2.oneC <- comp_one_data(x = x[[2]], y = y[[2]], clvar = clvar[[2]],
#                            arg.all = arg.all[[2]],
#                            arg.all.fix = arg.all.fix,
#                            colnames.cluster = c("rsid6", "rsid7", "rsid8"),
#                            mod.large = mod.large.tmp[[2]],
#                            mod.small = mod.small.tmp[[2]])
#   r3.oneC <- comp_one_data(x = x[[3]], y = y[[3]], clvar = clvar[[3]],
#                            arg.all = arg.all[[3]],
#                            arg.all.fix = arg.all.fix,
#                            colnames.cluster = c("rsid6", "rsid7", "rsid8"),
#                            mod.large = mod.large.tmp[[3]],
#                            mod.small = mod.small.tmp[[3]])
#   r4.oneC <- comp_one_data(x = x[[4]], y = y[[4]], clvar = clvar[[4]],
#                            arg.all = arg.all[[4]],
#                            arg.all.fix = arg.all.fix,
#                            colnames.cluster = c("rsid6", "rsid7", "rsid8"),
#                            mod.large = mod.large.tmp[[4]],
#                            mod.small = mod.small.tmp[[4]])
#
#   pval.oneC <- c(r1.oneC$pval, r2.oneC$pval, r3.oneC$pval, r4.oneC$pval)
#   pnorm(sum(stouffer_weights * qnorm(pval.oneC)))
#
#   res1.oneC <- check_test_hierarchy(x = x[[1]], y = y[[1]], clvar = clvar[[1]],
#                                     res.multisplit = res.multisplit[1],
#                                     B = B, cluster_test = list(c("rsid6", "rsid7", "rsid8")))
#
#   res2.oneC <- check_test_hierarchy(x = x[[2]], y = y[[2]], clvar = clvar[[2]],
#                                     res.multisplit = res.multisplit[2],
#                                     B = B, cluster_test = list(c("rsid6", "rsid7", "rsid8")))
#
#   res3.oneC <- check_test_hierarchy(x = x[[3]], y = y[[3]], clvar = clvar[[3]],
#                                     res.multisplit = res.multisplit[3],
#                                     B = B, cluster_test = list(c("rsid6", "rsid7", "rsid8")))
#
#   res4.oneC <- check_test_hierarchy(x = x[[4]], y = y[[4]], clvar = clvar[[4]],
#                                     res.multisplit = res.multisplit[4],
#                                     B = B, cluster_test = list(c("rsid6", "rsid7", "rsid8")))
#
#   pval.hand.oneC <- c(res1.oneC, res2.oneC, res3.oneC, res4.oneC)
#   pnorm(sum(stouffer_weights * qnorm(pval.hand.oneC)))
#
#   ####################################################
#   ## Cluster c("rsid7", rsid8")
#
#   res.oneC2 <- hierbase:::comp_cluster_pval(x = x, y = y, clvar = clvar,
#                                             test.func = comp_one_data,
#                                             arg.all = arg.all,
#                                             arg.all.fix = arg.all.fix,
#                                             colnames.cluster = c("rsid7", "rsid8"),
#                                             minimal.pval = 1, agg.method = "Stouffer",
#                                             hier.adj = FALSE, mt.adj = "none", MD.factor = 1,
#                                             mod.large = mod.large.tmp, mod.small = mod.small.tmp,
#                                             stouffer.weights = stouffer_weights)
#
#   res.oneC2$cluster
#
#   r1.oneC2 <- comp_one_data(x = x[[1]], y = y[[1]], clvar = clvar[[1]],
#                             arg.all = arg.all[[1]],
#                             arg.all.fix = arg.all.fix,
#                             colnames.cluster = c("rsid7", "rsid8"),
#                             mod.large = mod.large.tmp[[1]],
#                             mod.small = mod.small.tmp[[1]])
#   r2.oneC2 <- comp_one_data(x = x[[2]], y = y[[2]], clvar = clvar[[2]],
#                             arg.all = arg.all[[2]],
#                             arg.all.fix = arg.all.fix,
#                             colnames.cluster = c("rsid7", "rsid8"),
#                             mod.large = mod.large.tmp[[2]],
#                             mod.small = mod.small.tmp[[2]])
#   r3.oneC2 <- comp_one_data(x = x[[3]], y = y[[3]], clvar = clvar[[3]],
#                             arg.all = arg.all[[3]],
#                             arg.all.fix = arg.all.fix,
#                             colnames.cluster = c("rsid7", "rsid8"),
#                             mod.large = mod.large.tmp[[3]],
#                             mod.small = mod.small.tmp[[3]])
#   r4.oneC2 <- comp_one_data(x = x[[4]], y = y[[4]], clvar = clvar[[4]],
#                             arg.all = arg.all[[4]],
#                             arg.all.fix = arg.all.fix,
#                             colnames.cluster = c("rsid7", "rsid8"),
#                             mod.large = mod.large.tmp[[4]],
#                             mod.small = mod.small.tmp[[4]])
#
#   pval.oneC2 <- c(r1.oneC2$pval, r2.oneC2$pval, r3.oneC2$pval, r4.oneC2$pval)
#   pnorm(sum(stouffer_weights * qnorm(pval.oneC2)))
#
#   res1.oneC2 <- check_test_hierarchy(x = x[[1]], y = y[[1]], clvar = clvar[[1]],
#                                      res.multisplit = res.multisplit[1],
#                                      B = B, cluster_test = list(c("rsid7", "rsid8")))
#
#   res2.oneC2 <- check_test_hierarchy(x = x[[2]], y = y[[2]], clvar = clvar[[2]],
#                                      res.multisplit = res.multisplit[2],
#                                      B = B, cluster_test = list(c("rsid7", "rsid8")))
#
#   res3.oneC2 <- check_test_hierarchy(x = x[[3]], y = y[[3]], clvar = clvar[[3]],
#                                      res.multisplit = res.multisplit[3],
#                                      B = B, cluster_test = list(c("rsid7", "rsid8")))
#
#   res4.oneC2 <- check_test_hierarchy(x = x[[4]], y = y[[4]], clvar = clvar[[4]],
#                                      res.multisplit = res.multisplit[4],
#                                      B = B, cluster_test = list(c("rsid7", "rsid8")))
#
#   pval.hand.oneC2 <- c(res1.oneC2, res2.oneC2, res3.oneC2, res4.oneC2)
#   pnorm(sum(stouffer_weights * qnorm(pval.hand.oneC2)))
#
#
# })

