# require("testthat")
#
# ## random number generator
# ##### Change RNGversion in the future. Change Description such that #####
# ##### Depends: R (>= 3.6.0)                                         #####
# suppressWarnings(RNGversion("3.5.0"))
# RNGkind("L'Ecuyer-CMRG")
#
# test_that("test_only_hierarchy: check input", {
#   expect_error(test_only_hierarchy(x = NULL, y = NULL, dendr = NULL,
#                                    res.multisplit = NULL,
#                                    family = NULL),
#                "The response y is required to be a vector or a list of vectors if multiple data sets are present.")
#
#   expect_error(test_only_hierarchy(x = NULL, y = matrix(1:4, ncol = 2),
#                                    dendr = NULL, res.multisplit = NULL,
#                                    family = NULL),
#                "The elements of the list y are required to be numeric vectors or matrices with only one column. In the case of only one data set, it is enough that y is a numeric vector or matrix with only one column but it can as well be a list with one element.")
#
#   expect_error(test_only_hierarchy(x = NULL, y = list(matrix(1:4, ncol = 2),
#                                                       matrix(1:4, ncol = 2)),
#                                    dendr = NULL, res.multisplit = NULL,
#                                    family = NULL),
#                "The elements of the list y are required to be numeric vectors or matrices with only one column.")
#
#   expect_error(test_only_hierarchy(x = NULL, y = 1:2, dendr = NULL,
#                                    res.multisplit = NULL, family = NULL),
#                "The input x is required to be a matrix or a list of matrices if multiple data sets are present.")
#
#   expect_error(test_only_hierarchy(x = matrix(1:4, ncol = 2), y = 1:2,
#                                    dendr = NULL, res.multisplit = NULL,
#                                    family = NULL),
#                "The matrix x is required to have column names. If there is no natural naming convention, then one can set them to some integer, say, 1 to p.",
#                fixed = TRUE)
#
#   tt <- matrix(1:4, ncol = 2)
#   colnames(tt) <- c("a", "b")
#   expect_error(test_only_hierarchy(x = tt, y = 1:2, dendr = NULL,
#                                    res.multisplit = NULL, family = NULL),
#                "The input res.multisplit is required to be a list.")
#
#   set.seed(88)
#   x <- matrix(rnorm(100), ncol = 2)
#   colnames(x) <- c("col1", "col2")
#   y <- 1:50
#   res.multisplit <- multisplit(x = x, y = y)
#   expect_error(test_only_hierarchy(x = x, y = y, dendr = NULL,
#                                    res.multisplit = res.multisplit,
#                                    family = NULL),
#                "The input dendr is required to be a list of dendrograms.")
#
#   # Please note that family = NULL results in taking the default value.
#   dendr <- cluster_vars(x = x)
#   # expect_error(test_only_hierarchy(x = x, y = y, dendr = dendr,
#   #                                              res.multisplit = res.multisplit,
#   #                                              family = "alsdkk"),
#   #              "'arg' should be one of \"gaussian\", \"binomial\"")
#
#   expected_result_1 <- data.frame(block = NA, p.value = NA,
#                                   significant.cluster = NA)
#   expected_result_1$significant.cluster <- list(NA)
#   attr(expected_result_1, "class") <- c("data.frame")
#   expected_result <- list(res.multisplit = res.multisplit,
#                           res.hierarchy = expected_result_1)
#   attr(expected_result, "class") <- c("hierT", "list")
#   expect_equal(test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                    res.multisplit = res.multisplit,
#                                    family = "gaussian"),
#                expected_result)
#
#   # The column names of x or each element of x (list containing data sets)
#   # are required to have unique column names.
#   # (The tree and the output of the function multisplit are only fitted on x
#   # and y. This is irrelevant for this test.)
#   expect_error(test_only_hierarchy(x = cbind(x, x), y = y,
#                                    dendr = dendr,
#                                    res.multisplit = res.multisplit,
#                                    family = "gaussian"),
#                "The matrix x is required to have unique column names.",
#                fixed = TRUE)
#
#   expect_error(test_only_hierarchy(x = list(cbind(x, x), x),
#                                    y = list(y, y),
#                                    dendr = dendr,
#                                    res.multisplit = res.multisplit,
#                                    family = "gaussian"),
#                "Each of the matrices which are stored in x are required to have unique column names.",
#                fixed = TRUE)
#
#   # If the trees (argument block is supplied) were fit on less variables,
#   # then we try to fit the model.
#   require("MASS")
#   p <- 20
#   n <- 80
#   B <- 50
#   sim.geno1 <- mvrnorm(n = n, mu = rep(0, p),
#                        Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno1) <- paste0("rsid", 1:p)
#   sim.geno2 <- mvrnorm(n = n, mu = rep(0, p),
#                        Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno2) <- paste0("rsid", 1:p)
#
#   dendr <- cluster_vars(x = list(sim.geno1[, paste0("rsid", 1:10)],
#                                 sim.geno2[, paste0("rsid", 1:10)]),
#                        block = data.frame(paste0("rsid", 1:10),
#                                           rep(1:2, each = 5),
#                                           stringsAsFactors = FALSE))
#   # plot(dendr$res.tree[[1]])
#   # plot(dendr$res.tree[[2]])
#
#   set.seed(144)
#   data.dim <- c(80, 20)
#   ind.active <- sample(1:data.dim[2], 2)
#   beta <- rep(0, data.dim[2])
#   beta[ind.active] <- 2
#   y1 <- sim.geno1 %*% beta + rnorm(data.dim[1])
#   y2 <- sim.geno2 %*% beta + rnorm(data.dim[1])
#
#   res.multisplit <- multisplit(x = list(sim.geno1, sim.geno2), y = list(y1, y2),
#                                family = "gaussian", B = B)
#
#   expect_error(test_only_hierarchy(x = list(sim.geno1, sim.geno2),
#                                    y = list(y1, y2), dendr = dendr,
#                                    res.multisplit = res.multisplit,
#                                    family = "gaussian"),
#                "There are column name of x which have no corresponding values in the first column of block (column names of x).",
#                fixed = TRUE)
# })
#
# #### Check output with one data set ####
# # This function is used to calculate the p-value of a given split for the
# # binomial case.
# MEL2 <- function(x, y, maxit, delta = 0.01, epsilon = 1e-6) {
#   # mean of y but we bound it awy from 0 and 1. See Equation (10) on page 8.
#   pi.hat <- max(delta, min(1 - delta, mean(y)))
#   # The two parameters delta.0 and delta.1 are constrained such that the average
#   # of the pseudo-observation is equal to pi.hat. See Equation (9) on page 8.
#   delta.0 <- (pi.hat * delta) / (1 + delta)
#   delta.1 <- (1 + pi.hat * delta) / (1 + delta)
#   # Pseudo-observations. See Equation (3) on page 4.
#   y.tilde <- delta.0 * (1 - y) + delta.1 * y
#   pseudo.y <- cbind(y.tilde, 1 - y.tilde)
#
#   # Suppress warning that "non-integer counts in a binomial glm!" because the
#   # function glm expects in the first column to be the number of successes and
#   # the second column to be the number of failures
#   suppressWarnings(res <- glm(pseudo.y ~ x, family = "binomial",
#                               control = glm.control(epsilon = epsilon,
#                                                     maxit = maxit),
#                               model = FALSE, y = FALSE))
#
#   return(res)
# }
#
# # This function calculates the p-value for each of the notes in the tree.
# check_test_hierarchy <- function(x, y, clvar, res.multisplit, B, cluster_test,
#                                  binomial = FALSE){
#   CT_colnames <- lapply(X = cluster_test, function(x) paste(x, collapse = "_"))
#   res <- matrix(NA, ncol = length(cluster_test), nrow = B)
#   colnames(res) <- CT_colnames
#   RES <- rep(NA, length(cluster_test))
#   names(RES) <- CT_colnames
#   for (i in cluster_test) { # for each cluster
#     for (b in 1:B) { # for each split (multi-sample splitting)
#       # selected coefficients
#       sel.coef <- res.multisplit[[1]]$sel.coef[b, ][!is.na(res.multisplit[[1]]$sel.coef[b, ])]
#       # other half of the samples
#       ind <- res.multisplit[[1]]$out.sample[b, ]
#       # combined data set (clvar plus x)
#       clvar_x <- cbind(clvar, x[, sel.coef, drop = FALSE])[ind, , drop = FALSE]
#
#       # intersection and set difference of selected coefficients and the given cluster
#       intersect_i <- intersect(sel.coef, i)
#       setdiff_i <- setdiff(sel.coef, i)
#
#       # columnnames for the model \hat{S}^{(b)} \setminus C
#       sel_i_clvar <- c(colnames(clvar), setdiff_i)
#
#       # browser()
#       # design matrix of the reduced model: variables of \hat{S}^{(b)} \setminus C & clvar
#       clvar_x_reduced <- clvar_x[, sel_i_clvar, drop =  FALSE]
#       if (ncol(clvar_x_reduced) == 0) {
#         clvar_x_reduced <- rep(1, length(y[ind]))
#       }
#
#       res[b, paste(i, collapse = "_")] <-
#         if (length(intersect_i) == 0) { # Equation (2) on page 333 of Mandozzi and Buehlmann (2016)
#           1
#         } else {
#           if (binomial) {
#             # min(1, ... * |\hat{S}^{(b)}| / |\hat{S}^{(b)} \setminus C|)  =>  Equation (2) & (3)
#             # on page 333 of Mandozzi and Buehlmann (2016)
#             min(1, anova(
#               # full model: variables of \hat{S}^{(b)} & clvar
#               MEL2(y = y[ind], x = clvar_x, maxit = 100),
#               # reduced model: variables of \hat{S}^{(b)} \setminus C & clvar
#               MEL2(y = y[ind], x = clvar_x_reduced, maxit = 100),
#               test = "Chisq")$"Pr(>Chi)"[2] *
#                 length(sel.coef) / length(intersect_i)
#             )
#           } else {
#             # min(1, ... * |\hat{S}^{(b)}| / |\hat{S}^{(b)} \setminus C|)  =>  Equation (2) & (3)
#             # on page 333 of Mandozzi and Buehlmann (2016)
#             min(1, anova(
#               # full model: variables of \hat{S}^{(b)} & clvar
#               lm(y[ind] ~ clvar_x),
#               # reduced model: variables of \hat{S}^{(b)} \setminus C & clvar
#               lm(y[ind] ~ clvar_x_reduced),
#               test = "F")$P[2] *
#                 length(sel.coef) / length(intersect_i)
#             )
#           }
#         }
#     }
#     # Equation (4) on page 333 of Mandozzi and Buehlmann (2016)
#     RES[paste(i, collapse = "_")] <- adj_pval(res[,  paste(i, collapse = "_")], B = B)
#   }
#
#   # hierarchical adjustment has to be done by hand (Equation below Equation (4))
#   return(RES)
# }
#
# adj_pval <- function(pvals, B) {
#   # define the sequence of gamma values
#   gamma_min <- 0.05
#   gamma_step <- 0.01
#   gamma_seq <- seq(gamma_min, 1, gamma_step)
#
#   # compute the empirical quantile vector
#   gamma_step <- vector("numeric", length = length(gamma_seq))
#   for (g in 1:length(gamma_seq)) {
#     gamma_step[g] <- min(1, quantile(pvals / gamma_seq[g], gamma_seq[g],
#                                      na.rm = TRUE))
#   }
#
#   # compute the adjusted p value
#   # Equation 4 on page 333 in Mandozzi and Buehlmann (2016)
#   return(min(1, (1 - log(gamma_min)) * min(gamma_step)))
# }
#
# ### Example I ###
# test_that("test_only_hierarchy: check output (Example I)", {
#   ## simulate index
#   n <- 800
#   p <- 5
#   B <- 50
#
#   ## simulate data
#   require(MASS)
#   set.seed(9229)
#   sim.geno <- mvrnorm(n = n, mu = rep(0, p),
#                       Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno) <- paste0("rsid", 1:p)
#
#   set.seed(144)
#   data.dim <- dim(sim.geno)
#
#   ind.active <- c(4, 1) # sample(1:data.dim[2], 2)
#   beta <- rep(0, data.dim[2])
#   beta[ind.active] <- 2
#   y <- sim.geno %*% beta + rnorm(data.dim[1])
#
#   # cluster the data
#   dendr <- cluster_vars(x = sim.geno)
#   # plot(dendr$res.tree[[1]])
#
#   # multisplit
#   set.seed(2)
#   res.multisplit <- multisplit(x = sim.geno, y = y, family = "gaussian", B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = sim.geno, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = sim.geno, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", agg.method = "Stouffer")
#
#   ## Test
#   # This list encodes the tree structure
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        c("rsid3", "rsid4", "rsid5"),
#                        c("rsid3", "rsid4"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   pvals_to_be <- check_test_hierarchy(x = sim.geno, y = y, clvar = NULL,
#                                       res.multisplit = res.multisplit,
#                                       B = B, cluster_test = cluster_test)
#
#
#
#   # Tippett
#   compare_with <- pvals_to_be
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-120)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-88)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   compare_with <- pvals_to_be
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-120)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-88)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
# ### Example II ###
# test_that("test_only_hierarchy: check output (Example II)", {
#   ## simulate index
#   n <- 800
#   p <- 5
#   B <- 5
#
#   ## simulate data
#   require(MASS)
#   set.seed(9229)
#   sim.geno <- mvrnorm(n = n, mu = rep(0, p),
#                       Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno) <- paste0("rsid", 1:p)
#   sim.clvar <- matrix(rnorm(n * 3), ncol = 3)
#   colnames(sim.clvar) <- paste0("clvar", 1:3)
#
#   set.seed(144)
#   data.dim <- dim(sim.geno) # first entry corresponds to rows and second to columns
#
#   ind.active <- c(4, 1) # sample(1:data.dim[2], 2)
#   beta <- rep(0, data.dim[2])
#   beta[ind.active] <- 2
#   y <- sim.geno %*% beta + sim.clvar %*% c(0.25, 0.5, 1) + rnorm(data.dim[1])
#
#   # cluster the data
#   dendr <- cluster_vars(x = sim.geno)
#   # plot(dendr$res.tree[[1]])
#
#   # multisplit
#   set.seed(555)
#   res.multisplit <- multisplit(x = sim.geno, y = y, clvar = sim.clvar,
#                                family = "gaussian", B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = sim.geno, y = y, clvar = sim.clvar,
#                                dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = sim.geno, y = y, clvar = sim.clvar,
#                                dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", agg.method = "Stouffer")
#
#   ## test
#   # This list encodes the tree structure
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        c("rsid3", "rsid4", "rsid5"),
#                        c("rsid3", "rsid4"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   pvals_to_be <- check_test_hierarchy(x = sim.geno, y = y, clvar = sim.clvar,
#                                       res.multisplit = res.multisplit,
#                                       B = B, cluster_test = cluster_test)
#
#   # Tippett
#   compare_with <- pvals_to_be
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-115)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-85)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   compare_with <- pvals_to_be
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-115)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-85)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
# #### Check output with multiple data sets ####
# # Function for generating the data
# require(MASS)
#
# gen_one <- function(n, p, seed1, ind.a, seed3, num_clvar = NULL,
#                     coef_clvar = NULL) {
#   set.seed(seed1)
#   x <- mvrnorm(n = n, mu = rep(0, p), Sigma = toeplitz(0.8^(seq(0, p - 1))) )
#   colnames(x) <- paste0("rsid", 1:p)
#   if (!is.null(num_clvar)) {
#     clvar <- matrix(rnorm(n * 3), ncol = 3)
#     colnames(clvar) <- paste0("clvar", 1:3)
#   } else {
#     clvar <- NULL
#   }
#
#   data.dim <- dim(x) # first entry corresponds to rows and second to columns
#   ind.active <- ind.a # sample(1:data.dim[2], 2)
#   beta <- rep(0, data.dim[2])
#   beta[ind.active] <- 2
#
#   set.seed(seed3)
#   if (!is.null(num_clvar)) {
#     y <- x %*% beta + rnorm(data.dim[1]) + clvar %*% coef_clvar
#   } else {
#     y <- x %*% beta + rnorm(data.dim[1])
#   }
#
#
#   return(list(x = x, y = y, clvar = clvar))
# }
#
# ### Example III ###
# test_that("test_only_hierarchy: check output (Example III multiple data sets)", {
#   skip_on_bioc()
#
#   ## simulate index
#   n <- 800
#   p <- 5
#   B <- 50
#
#   ## simulate data
#   r1 <- gen_one(n = n, p = p, seed1 = 9229, ind.a = c(4, 1), seed3 = 8)
#   r2 <- gen_one(n = n, p = p, seed1 = 929, ind.a = c(4, 1), seed3 = 99)
#   r3 <- gen_one(n = n, p = p, seed1 = 99, ind.a = c(4, 1), seed3 = 100)
#   r4 <- gen_one(n = n, p = p, seed1 = 9, ind.a = c(4, 1), seed3 = 1111)
#
#   x <- list(r1$x, r2$x, r3$x, r4$x)
#   y <- list(r1$y, r2$y, r3$y, r4$y)
#   # clvar <- list(r1$clvar, r2$clvar, r3$clvar, r4$clvar)
#
#   # cluster the data
#   dendr <- cluster_vars(x = x)
#   # plot(dendr$res.tree[[1]])
#
#   # multisplit
#   set.seed(744)
#   res.multisplit <- multisplit(x = x, y = y, family = "gaussian", B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian",
#                                agg.method = "Stouffer")
#
#   ## Test
#   # This list encodes the tree structure
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2", "rsid3"),
#                        c("rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   res1 <- check_test_hierarchy(x = x[[1]], y = y[[1]], clvar = NULL,
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test)
#
#   res2 <- check_test_hierarchy(x = x[[2]], y = y[[2]], clvar = NULL,
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test)
#
#   res3 <- check_test_hierarchy(x = x[[3]], y = y[[3]], clvar = NULL,
#                                res.multisplit = res.multisplit[3],
#                                B = B, cluster_test = cluster_test)
#
#   res4 <- check_test_hierarchy(x = x[[4]], y = y[[4]], clvar = NULL,
#                                res.multisplit = res.multisplit[4],
#                                B = B, cluster_test = cluster_test)
#
#   pvals_to_be <- rbind(res1, res2, res3, res4)
#
#
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 4)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid4", "rsid1")])
#   expected_result$significant.cluster <- list(c("rsid4"), c("rsid1"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-145)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   # c(0.5, 0.5, 0.5, 0.5)
#   stouffer_weights <- sqrt(c(800, 800, 800, 800) / sum(c(800, 800, 800, 800)))
#
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y, stouffer_weights) {
#                           pnorm(sum(stouffer_weights * qnorm(x)))
#                         },
#                         len_y = 4, stouffer_weights = stouffer_weights)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid4", "rsid1")])
#   expected_result$significant.cluster <- list(c("rsid4"), c("rsid1"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-200)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
# ### Example IV unbalanced data sets ###
# test_that("test_only_hierarchy: check output (Example IV multiple data sets)", {
#   skip_on_bioc()
#
#   ## simulate index
#   p <- 5
#   B <- 50
#
#   ## simulate data
#   require(MASS)
#
#   r1 <- gen_one(n = 800, p = p, seed1 = 9229, ind.a = c(4, 1), seed3 = 8)
#   r2 <- gen_one(n = 200, p = p, seed1 = 929, ind.a = c(4, 1), seed3 = 99)
#   r3 <- gen_one(n = 350, p = p, seed1 = 99, ind.a = c(4, 1), seed3 = 100)
#   r4 <- gen_one(n = 50, p = p, seed1 = 9, ind.a = c(4, 1), seed3 = 1111)
#
#   x <- list(r1$x, r2$x, r3$x, r4$x)
#   y <- list(r1$y, r2$y, r3$y, r4$y)
#   # clvar <- list(r1$clvar, r2$clvar, r3$clvar, r4$clvar)
#
#   # cluster the data
#   dendr <- cluster_vars(x = x)
#   # plot(dendr$res.tree[[1]])
#
#   # multisplit
#   set.seed(6)
#   res.multisplit <- multisplit(x = x, y = y, family = "gaussian", B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian",
#                                agg.method = "Stouffer")
#   ## Test
#   # This list encodes the tree structure
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2", "rsid3"),
#                        c("rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   res1 <- check_test_hierarchy(x = x[[1]], y = y[[1]], clvar = NULL,
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test)
#
#   res2 <- check_test_hierarchy(x = x[[2]], y = y[[2]], clvar = NULL,
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test)
#
#   res3 <- check_test_hierarchy(x = x[[3]], y = y[[3]], clvar = NULL,
#                                res.multisplit = res.multisplit[3],
#                                B = B, cluster_test = cluster_test)
#
#   res4 <- check_test_hierarchy(x = x[[4]], y = y[[4]], clvar = NULL,
#                                res.multisplit = res.multisplit[4],
#                                B = B, cluster_test = cluster_test)
#
#   pvals_to_be <- rbind(res1, res2, res3, res4)
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 4)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-132)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-135)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   stouffer_weights <- sqrt(c(800, 200, 350, 50) / sum(c(800, 200, 350, 50)))
#
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y, stouffer_weights) {
#                           pnorm(sum(stouffer_weights * qnorm(x)))
#                         },
#                         len_y = 4, stouffer_weights = stouffer_weights)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-220)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-190)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
# ### Example V with co-variables ###
# ###### The result of this test will change under RNGversion("3.6.0"). #####
# ###### TODO Adjust example calcualted by hand when switching to       #####
# ###### RNGversion("3.6.0").                                           #####
# test_that("test_only_hierarchy: check output (Example V multiple data sets)", {
#   skip_on_bioc()
#
#   ## simulate index
#   p <- 5
#   B <- 50
#
#   ## simulate data
#   require(MASS)
#
#   r1 <- gen_one(n = 800, p = p, seed1 = 9229, ind.a = c(4, 1), seed3 = 8,
#                 num_clvar = 3, coef_clvar = c(0.5, 0.25, 1.25))
#   r2 <- gen_one(n = 200, p = p, seed1 = 929, ind.a = c(4, 1), seed3 = 99,
#                 num_clvar = 3, coef_clvar = c(0.5, 0.25, 1.25))
#   r3 <- gen_one(n = 350, p = p, seed1 = 99, ind.a = c(4, 1), seed3 = 100,
#                 num_clvar = 3, coef_clvar = c(0.5, 0.25, 1.25))
#   r4 <- gen_one(n = 50, p = p, seed1 = 9, ind.a = c(4, 1), seed3 = 1111,
#                 num_clvar = 3, coef_clvar = c(0.5, 0.25, 1.25))
#
#   x <- list(r1$x, r2$x, r3$x, r4$x)
#   y <- list(r1$y, r2$y, r3$y, r4$y)
#   clvar <- list(r1$clvar, r2$clvar, r3$clvar, r4$clvar) # with co-variables
#
#   # cluster the data
#   dendr <- cluster_vars(x = x)
#   # plot(dendr$res.tree[[1]])
#
#   # multisplit
#   set.seed(3)
#   res.multisplit <- multisplit(x = x, y = y, clvar = clvar, family = "gaussian",
#                                B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = x, y = y, clvar = clvar,
#                                dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = x, y = y, clvar = clvar,
#                                dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian",
#                                agg.method = "Stouffer")
#
#   ## Test
#   # This list encodes the tree structure
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        c("rsid3", "rsid4", "rsid5"),
#                        c("rsid4", "rsid5"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   res1 <- check_test_hierarchy(x = x[[1]], y = y[[1]], clvar = clvar[[1]],
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test)
#
#   res2 <- check_test_hierarchy(x = x[[2]], y = y[[2]], clvar = clvar[[2]],
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test)
#
#   res3 <- check_test_hierarchy(x = x[[3]], y = y[[3]], clvar = clvar[[3]],
#                                res.multisplit = res.multisplit[3],
#                                B = B, cluster_test = cluster_test)
#
#   res4 <- check_test_hierarchy(x = x[[4]], y = y[[4]], clvar = clvar[[4]],
#                                res.multisplit = res.multisplit[4],
#                                B = B, cluster_test = cluster_test)
#
#   pvals_to_be <- rbind(res1, res2, res3, res4)
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 4)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-134)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-138)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   stouffer_weights <- sqrt(c(800, 200, 350, 50) / sum(c(800, 200, 350, 50)))
#
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y, stouffer_weights) {
#                           pnorm(sum(stouffer_weights * qnorm(x)))
#                         },
#                         len_y = 4, stouffer_weights = stouffer_weights)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-220)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-180)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#
#   ### Example VI, same data as in Example V but without clvar ###
#   # test hierarchy: no clvar but the response was created incl. clvar
#   # test_only_hierarchy: check output (Example VI multiple data sets)
#
#   # multisplit
#   set.seed(4)
#   res.multisplit <- multisplit(x = x, y = y, family = "gaussian",
#                                B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", global.test = TRUE)
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", global.test = TRUE,
#                                agg.method = "Stouffer")
#
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        c("rsid3", "rsid4", "rsid5"),
#                        c("rsid4", "rsid5"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   res1 <- check_test_hierarchy(x = x[[1]], y = y[[1]], clvar = NULL,
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test)
#
#   res2 <- check_test_hierarchy(x = x[[2]], y = y[[2]], clvar = NULL,
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test)
#
#   res3 <- check_test_hierarchy(x = x[[3]], y = y[[3]], clvar = NULL,
#                                res.multisplit = res.multisplit[3],
#                                B = B, cluster_test = cluster_test)
#
#   res4 <- check_test_hierarchy(x = x[[4]], y = y[[4]], clvar = NULL,
#                                res.multisplit = res.multisplit[4],
#                                B = B, cluster_test = cluster_test)
#
#   pvals_to_be <- rbind(res1, res2, res3, res4)
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 4)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-70)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-80)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y, stouffer_weights) {
#                           pnorm(sum(stouffer_weights * qnorm(x)))
#                         },
#                         len_y = 4, stouffer_weights = stouffer_weights)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid3_rsid4_rsid5", "rsid1")])
#   expected_result$significant.cluster <- list(c("rsid3", "rsid4", "rsid5"), c("rsid1"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-120)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-100)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
# ### Example VII: similar as Example V but with a block ###
# test_that("test_only_hierarchy: check output (Example V multiple data sets)", {
#   skip_on_bioc()
#
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
#   res.multisplit <- multisplit(x = x, y = y, clvar = clvar, family = "gaussian",
#                                B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = x, y = y, clvar = clvar, dendr = dendr,
#                                res.multisplit = res.multisplit, family = "gaussian")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = x, y = y, clvar = clvar, dendr = dendr,
#                                res.multisplit = res.multisplit, family = "gaussian",
#                                agg.method = "Stouffer")
#
#   ## Test
#   # This list encodes the tree structure
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5", "rsid6",
#                          "rsid7", "rsid8", "rsid9", "rsid10"),
#                        c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid6", "rsid7", "rsid8", "rsid9", "rsid10"),
#                        c("rsid1", "rsid2", "rsid3"),
#                        c("rsid4", "rsid5"),
#                        c("rsid2", "rsid3"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5",
#                        c("rsid6", "rsid7", "rsid8"),
#                        c("rsid9", "rsid10"),
#                        c("rsid7", "rsid8"),
#                        "rsid6",
#                        "rsid7",
#                        "rsid8",
#                        "rsid9",
#                        "rsid10")
#
#   res1 <- check_test_hierarchy(x = x[[1]], y = y[[1]], clvar = clvar[[1]],
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test)
#
#   res2 <- check_test_hierarchy(x = x[[2]], y = y[[2]], clvar = clvar[[2]],
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test)
#
#   res3 <- check_test_hierarchy(x = x[[3]], y = y[[3]], clvar = clvar[[3]],
#                                res.multisplit = res.multisplit[3],
#                                B = B, cluster_test = cluster_test)
#
#   res4 <- check_test_hierarchy(x = x[[4]], y = y[[4]], clvar = clvar[[4]],
#                                res.multisplit = res.multisplit[4],
#                                B = B, cluster_test = cluster_test)
#
#   pvals_to_be <- rbind(res1, res2, res3, res4)
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 4)
#
#   expected_result <- data.frame(block = c("1", "2"),
#                                 p.value = compare_with[c("rsid3", "rsid7")],
#                                 stringsAsFactors = FALSE)
#   expected_result$significant.cluster <- list(c("rsid3"), c("rsid7"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_identical(res.T$res.hierarchy$block, expected_result$block)
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-90)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-100)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Tippett with global = FALSE
#   res.T <- test_only_hierarchy(x = x, y = y, clvar = clvar, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", global.test = FALSE)
#
#   expect_identical(res.T$res.hierarchy$block, expected_result$block)
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-90)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-100)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   stouffer_weights <- sqrt(c(800, 200, 350, 50) / sum(c(800, 200, 350, 50)))
#
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y, stouffer_weights) {
#                           pnorm(sum(stouffer_weights * qnorm(x)))
#                         },
#                         len_y = 4, stouffer_weights = stouffer_weights)
#
#   expected_result <- data.frame(block = c("1", "2"),
#                                 p.value = compare_with[c("rsid3", "rsid7")],
#                                 stringsAsFactors = FALSE)
#   expected_result$significant.cluster <- list(c("rsid3"), c("rsid7"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_identical(res.S$res.hierarchy$block, expected_result$block)
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-125)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-140)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer with global = FALSE
#   res.S <- test_only_hierarchy(x = x, y = y, clvar = clvar, dendr = dendr,
#                                res.multisplit = res.multisplit, family = "gaussian",
#                                agg.method = "Stouffer", global.test = FALSE)
#
#   expect_identical(res.S$res.hierarchy$block, expected_result$block)
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-125)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-140)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
# })
#
# #### Perform testing on a tree which is build on less variables ####
# test_that("test_only_hierarchy: check output (Example with smaller tree)", {
#   n <- 800
#   p <- 5
#   B <- 50
#
#   ## simulate data
#   require(MASS)
#   set.seed(9229)
#   sim.geno <- mvrnorm(n = n, mu = rep(0, p),
#                       Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno) <- paste0("rsid", 1:p)
#
#   set.seed(144)
#   data.dim <- dim(sim.geno)
#
#   ind.active <- c(4, 1) # sample(1:data.dim[2], 2)
#   beta <- rep(0, data.dim[2])
#   beta[ind.active] <- 2
#   y <- sim.geno %*% beta + rnorm(data.dim[1])
#
#   # cluster the data
#   dendr <- cluster_vars(x = sim.geno[, c(1, 2, 5)])
#   # plot(dendr$res.tree[[1]])
#
#   # multisplit
#   set.seed(2)
#   res.multisplit <- multisplit(x = sim.geno, y = y, family = "gaussian", B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = sim.geno, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = sim.geno, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian",
#                                agg.method = "Stouffer")
#
#   cluster_test <- list(c("rsid1", "rsid2", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid5")
#
#   pvals_to_be <- check_test_hierarchy(x = sim.geno, y = y, clvar = NULL,
#                                       res.multisplit = res.multisplit,
#                                       B = B, cluster_test = cluster_test)
#
#   # Tippett
#   compare_with <- pvals_to_be
#
#   expected_result <- data.frame(block = c(NA),
#                                 p.value = compare_with[c("rsid1")])
#   expected_result$significant.cluster <- list(c("rsid1"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-120)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # test_only_hierarchy: check output (Example with smaller tree; no global)
#   # no global test
#   res.T <- test_only_hierarchy(x = sim.geno, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", global.test = FALSE)
#
#   expect_equal(res.T$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-120)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   compare_with <- pvals_to_be
#
#   expected_result <- data.frame(block = c(NA),
#                                 p.value = compare_with[c("rsid1")])
#   expected_result$significant.cluster <- list(c("rsid1"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-120)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # test_only_hierarchy: check output (Example with smaller tree; no global)
#   # no global test
#   res.S <- test_only_hierarchy(x = sim.geno, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", global.test = FALSE,
#                                agg.method = "Stouffer")
#
#   expect_equal(res.S$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-120)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#
# })
#
# #### Perform testing with three data sets not measuring all the same variables ####
# test_that("test_only_hierarchy: check return object for multiple data sets not measuring the same variables", {
#   B <- 50
#   set.seed(938)
#   tt1 <- matrix(rnorm(40), ncol = 2)
#   tt2 <- matrix(rnorm(40), ncol = 2)
#   tt3 <- matrix(rnorm(40), ncol = 2)
#   colnames(tt1) <- c("c1", "c2")
#   colnames(tt2) <- c("c1", "c5")
#   colnames(tt3) <- c("c2", "c5")
#
#   set.seed(144)
#   ind.active <- 2 # sample(1:3, 1)
#   beta <- rep(0, 3)
#   beta[ind.active] <- 2
#   y1 <- tt1 %*% beta[c(1, 2)] + rnorm(20)
#   y2 <- tt2 %*% beta[c(1, 3)] + rnorm(20)
#   y3 <- tt3 %*% beta[c(2, 3)] + rnorm(20)
#
#   # use = "pairwise.complete.obs" (default)
#   res_d <- cluster_vars(x = list(tt1, tt2, tt3), d = NULL,
#                        method = "average",
#                        block = NULL)
#   # plot(res_d$res.tree[[1]])
#
#   # multisplit
#   set.seed(2)
#   res.multisplit <- multisplit(x = list(tt1, tt2, tt3), y = list(y1, y2, y3),
#                                family = "gaussian", B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = list(tt1, tt2, tt3), y = list(y1, y2, y3),
#                                dendr = res_d, res.multisplit = res.multisplit,
#                                family = "gaussian")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = list(tt1, tt2, tt3), y = list(y1, y2, y3),
#                                dendr = res_d, res.multisplit = res.multisplit,
#                                family = "gaussian", agg.method = "Stouffer")
#
#   cluster_test <- list(c("c1", "c2", "c5"),
#                        c("c1", "c2"),
#                        "c1",
#                        "c2",
#                        "c5")
#
#   res1 <- check_test_hierarchy(x = tt1, y = y1, clvar = NULL,
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test)
#
#   res2 <- check_test_hierarchy(x = tt2, y = y2, clvar = NULL,
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test)
#
#   res3 <- check_test_hierarchy(x = tt3, y = y3, clvar = NULL,
#                                res.multisplit = res.multisplit[3],
#                                B = B, cluster_test = cluster_test)
#
#   pvals_to_be <- rbind(res1, res2, res3)
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 3)
#
#   # expected_result <- data.frame(block = c(NA),
#   #                               p.value = NA)
#   # expected_result$significant.cluster <- list(NA)
#   # rownames(expected_result) <- NULL
#
#   # For RNGversion("3.6.0"), we need to apply the rule that the p-value can
#   # only increase by going from top to bottom through the tree.
#   expected_result <- data.frame(block = c(NA),
#                                 p.value = max(compare_with[c("c2")],
#                                               compare_with[c("c1_c2")],
#                                               compare_with[c("c1_c2_c5")]))
#   expected_result$significant.cluster <- list(c("c2"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-15)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   stouffer_weights <- sqrt(c(20, 20, 20) / sum(c(20, 20, 20)))
#
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y, stouffer_weights) {
#                           pnorm(sum(stouffer_weights * qnorm(x)))
#                         },
#                         len_y = 3, stouffer_weights = stouffer_weights)
#
#   expected_result <- data.frame(block = c(NA),
#                                 p.value = NA)
#   expected_result$significant.cluster <- list(NA)
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-120)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
#
# #### TODO We would have to apply the hierarchical constraint to the result
# #### calculated by hand in order to receive the exact same results.
#
# #### Perform testing with a data set that contains colinear variables ####
# test_that("test_only_hierarchy: check return object for data set containing colinear variables", {
#   skip_on_bioc()
#
#   B <- 50
#   n <- 200
#   p <- 500
#   set.seed(3)
#   x <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))
#   colnames(x) <- paste0("Var", 1:p)
#   beta <- rep(0, p)
#   beta[c(5, 20, 46)] <- 1
#   y <- x %*% beta + rnorm(n)
#
#   x <- cbind(x, x) # duplicate all the variables (so now 1000 variables)
#   colnames(x)[501:1000] <- paste0("D2_", colnames(x)[501:1000])
#
#   dendr1 <- cluster_vars(x = x)
#
#   set.seed(68)
#   res.multisplit1 <- multisplit(x = x, y = y, B = B)
#
#   # # both variables are included
#   # for (i in seq_len(nrow(res.multisplit1[[1]]$sel.coef))) {
#   #   print(i)
#   #   print(c("Var5", "D2_Var5")   %in% res.multisplit1[[1]]$sel.coef[i, ])
#   #   print(c("Var20", "D2_Var20") %in% res.multisplit1[[1]]$sel.coef[i, ])
#   #   print(c("Var46", "D2_Var46") %in% res.multisplit1[[1]]$sel.coef[i, ])
#   # }
#
#   # library(glmnet)
#   # fit.lasso <- glmnet(x = x, y = y)
#
#   # # unequal zero
#   # for (i in seq(0.01225, 1.11600, by = 0.005)) {
#   #   print(i)
#   #   coef.lasso <- coef(fit.lasso, s = i)
#   #   print(c("Var5", "D2_Var5")   %in% rownames(coef.lasso)[as.vector(coef.lasso) != 0])
#   #   print(c("Var20", "D2_Var20") %in% rownames(coef.lasso)[as.vector(coef.lasso) != 0])
#   #   print(c("Var46", "D2_Var46") %in% rownames(coef.lasso)[as.vector(coef.lasso) != 0])
#   # }
#
#   # # larer than 0.01 (some fixed constant)
#   # for (i in seq(0.01225, 1.11600, by = 0.005)) {
#   #   print(i)
#   #   coef.lasso <- coef(fit.lasso, s = i)
#   #   print(c("Var5", "D2_Var5")   %in% rownames(coef.lasso)[as.vector(coef.lasso) > 0.01])
#   #   print(c("Var20", "D2_Var20") %in% rownames(coef.lasso)[as.vector(coef.lasso) > 0.01])
#   #   print(c("Var46", "D2_Var46") %in% rownames(coef.lasso)[as.vector(coef.lasso) > 0.01])
#   # }
#
#   # plot(fit.lasso)
#   # sum(coef(fit.lasso, s = 0.01225) > 0.5)
#   # rownames(coef.lasso)[as.vector(coef(fit.lasso, s = 0.01225) > 0.01)]
#
#   # We need to apply the rule that the p-value can only increase by going
#   # from top to bottom through the tree. This is NOT done here!!!!
#   suppressWarnings(sign.clusters1 <- test_only_hierarchy(x = x, y = y, dendr = dendr1,
#                                                          res.multisplit = res.multisplit1,
#                                                          family = "gaussian"))
#   # attr(sign.clusters1$res.hierarchy, "warningMsgs")
#
#   cluster_test <- list(c("Var46", "D2_Var46"),
#                        c("Var5", "D2_Var5"),
#                        c("Var20", "D2_Var20"))
#                        # # and the single variables
#                        # c("Var46"), c("D2_Var46"),
#                        # c("Var5"), c("D2_Var5"),
#                        # c("Var20"), c("D2_Var20"))
#
#   expected_result <- check_test_hierarchy(x = x, y = y, clvar = NULL,
#                                           res.multisplit = res.multisplit1[1],
#                                           B = B, cluster_test = cluster_test)
#
#   expect_true(all(sign.clusters1$res.hierarchy$p.value >= expected_result))
#   expect_equal(sign.clusters1$res.hierarchy$significant.cluster,
#                cluster_test)
#
# })
#
# #### Perform testing with binary response ####
# test_that("test_only_hierarchy: check return object for a data set with binary response", {
#   n <- 800
#   p <- 5
#   B <- 50
#
#   ## simulate data
#   require(MASS)
#   set.seed(9229)
#   sim.geno <- mvrnorm(n = n, mu = rep(0, p),
#                       Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno) <- paste0("rsid", 1:p)
#
#   set.seed(144)
#   data.dim <- dim(sim.geno)
#
#   ind.active <- c(4, 1) # sample(1:data.dim[2], 2)
#   beta <- rep(0, data.dim[2])
#   beta[ind.active] <- 2
#
#   eta <- sim.geno %*% beta
#   pr <- 1 / (1 + exp(-eta))
#   y <- rbinom(n, 1, prob = pr)
#
#   # use = "pairwise.complete.obs" (default)
#   res_d <- cluster_vars(x = sim.geno, d = NULL,
#                        method = "average",
#                        block = NULL)
#   # plot(res_d$res.tree[[1]])
#
#   # multisplit
#   set.seed(2)
#   res.multisplit <- multisplit(x = sim.geno, y = y,
#                                family = "binomial", B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = sim.geno, y = y,
#                                dendr = res_d, res.multisplit = res.multisplit,
#                                family = "binomial")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = sim.geno, y = y,
#                                dendr = res_d, res.multisplit = res.multisplit,
#                                family = "binomial", agg.method = "Stouffer")
#
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2", "rsid3"),
#                        c("rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   pvals_to_be <- check_test_hierarchy(x = sim.geno, y = y, clvar = NULL,
#                                       res.multisplit = res.multisplit,
#                                       B = B, cluster_test = cluster_test,
#                                       binomial = TRUE)
#
#   # Tippett
#   compare_with <- pvals_to_be
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-25)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-20)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   compare_with <- pvals_to_be
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-25)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-20)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
# #### Perform testing two data sets with binary response ####
# test_that("test_only_hierarchy: check return object for two data sets with binary response", {
#   n <- 800
#   p <- 5
#   B <- 50
#
#   ## simulate data
#   require(MASS)
#   set.seed(9229)
#   sim.geno1 <- mvrnorm(n = n, mu = rep(0, p),
#                        Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno1) <- paste0("rsid", 1:p)
#
#   sim.geno2 <- mvrnorm(n = n, mu = rep(0, p),
#                        Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno2) <- paste0("rsid", 1:p)
#
#   set.seed(144)
#   data.dim <- dim(sim.geno1)
#
#   ind.active <- c(4, 1) # sample(1:data.dim[2], 2)
#   beta <- rep(0, data.dim[2])
#   beta[ind.active] <- 2
#
#   eta1 <- sim.geno1 %*% beta
#   pr1 <- 1 / (1 + exp(-eta1))
#   y1 <- rbinom(n, 1, prob = pr1)
#
#   eta2 <- sim.geno2 %*% beta
#   pr2 <- 1 / (1 + exp(-eta2))
#   y2 <- rbinom(n, 1, prob = pr2)
#
#   # use = "pairwise.complete.obs" (default)
#   res_d <- cluster_vars(x = list(sim.geno1, sim.geno2), d = NULL,
#                        method = "average",
#                        block = NULL)
#   # plot(res_d$res.tree[[1]])
#
#   # multisplit
#   set.seed(2)
#   res.multisplit <- multisplit(x = list(sim.geno1, sim.geno2), y = list(y1, y2),
#                                family = "binomial", B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = list(sim.geno1, sim.geno2), y = list(y1, y2),
#                                dendr = res_d, res.multisplit = res.multisplit,
#                                family = "binomial")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = list(sim.geno1, sim.geno2), y = list(y1, y2),
#                                dendr = res_d, res.multisplit = res.multisplit,
#                                family = "binomial", agg.method = "Stouffer")
#
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        c("rsid3", "rsid4", "rsid5"),
#                        c("rsid3", "rsid4"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   res1 <- check_test_hierarchy(x = sim.geno1, y = y1, clvar = NULL,
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test,
#                                binomial = TRUE)
#
#   res2 <- check_test_hierarchy(x = sim.geno2, y = y2, clvar = NULL,
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test,
#                                binomial = TRUE)
#
#   pvals_to_be <- rbind(res1, res2)
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 2)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-40)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-30)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   stouffer_weights <- sqrt(c(800, 800) / sum(c(800, 800)))
#
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y, stouffer_weights) {
#                           pnorm(sum(stouffer_weights * qnorm(x)))
#                         },
#                         len_y = 2, stouffer_weights = stouffer_weights)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-40)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-30)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
#
# #### Perform testing three data sets with binary response and unequal sample size ####
# test_that("test_only_hierarchy: check return object for three data sets with binary response", {
#   skip_on_bioc()
#
#   n1 <- 800
#   n2 <- 200
#   n3 <- 150
#   p <- 5
#   B <- 50
#
#   ## simulate data
#   require(MASS)
#   set.seed(9229)
#   sim.geno1 <- mvrnorm(n = n1, mu = rep(0, p),
#                        Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno1) <- paste0("rsid", 1:p)
#
#   sim.geno2 <- mvrnorm(n = n2, mu = rep(0, p),
#                        Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno2) <- paste0("rsid", 1:p)
#
#   sim.geno3 <- mvrnorm(n = n3, mu = rep(0, p),
#                        Sigma = toeplitz(0.8^(seq(0, p - 1))))
#   colnames(sim.geno3) <- paste0("rsid", 1:p)
#
#   set.seed(144)
#   data.dim <- dim(sim.geno1)
#
#   ind.active <- c(4, 1) # sample(1:data.dim[2], 2)
#   beta <- rep(0, data.dim[2])
#   beta[ind.active] <- 2
#
#   eta1 <- sim.geno1 %*% beta
#   pr1 <- 1 / (1 + exp(-eta1))
#   y1 <- rbinom(n1, 1, prob = pr1)
#
#   eta2 <- sim.geno2 %*% beta
#   pr2 <- 1 / (1 + exp(-eta2))
#   y2 <- rbinom(n2, 1, prob = pr2)
#
#   eta3 <- sim.geno3 %*% beta
#   pr3 <- 1 / (1 + exp(-eta3))
#   y3 <- rbinom(n3, 1, prob = pr3)
#
#   # use = "pairwise.complete.obs" (default)
#   res_d <- cluster_vars(x = list(sim.geno1, sim.geno2, sim.geno3), d = NULL,
#                        method = "average",
#                        block = NULL)
#   # plot(res_d$res.tree[[1]])
#
#   # multisplit
#   set.seed(2)
#   res.multisplit <- multisplit(x = list(sim.geno1, sim.geno2, sim.geno3),
#                                y = list(y1, y2, y3), family = "binomial",
#                                B = B)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = list(sim.geno1, sim.geno2, sim.geno3),
#                                y = list(y1, y2, y3), dendr = res_d,
#                                res.multisplit = res.multisplit,
#                                family = "binomial")
#
#   # test hierarchy: Stouffer
#   res.S <- test_only_hierarchy(x = list(sim.geno1, sim.geno2, sim.geno3),
#                                y = list(y1, y2, y3), dendr = res_d,
#                                res.multisplit = res.multisplit,
#                                family = "binomial", agg.method = "Stouffer")
#
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        c("rsid3", "rsid4", "rsid5"),
#                        c("rsid3", "rsid4"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   res1 <- check_test_hierarchy(x = sim.geno1, y = y1, clvar = NULL,
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test,
#                                binomial = TRUE)
#
#   res2 <- check_test_hierarchy(x = sim.geno2, y = y2, clvar = NULL,
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test,
#                                binomial = TRUE)
#
#   res3 <- check_test_hierarchy(x = sim.geno3, y = y3, clvar = NULL,
#                                res.multisplit = res.multisplit[3],
#                                B = B, cluster_test = cluster_test,
#                                binomial = TRUE)
#
#   pvals_to_be <- rbind(res1, res2, res3)
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 3)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-40)
#   expect_equal(res.T$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-30)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#   # Stouffer
#   stouffer_weights <- sqrt(c(n1, n2, n3) / sum(c(n1, n2, n3)))
#
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y, stouffer_weights) {
#                           pnorm(sum(stouffer_weights * qnorm(x)))
#                         },
#                         len_y = 2, stouffer_weights = stouffer_weights)
#
#   expected_result <- data.frame(block = c(NA, NA),
#                                 p.value = compare_with[c("rsid1", "rsid4")])
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.S$res.hierarchy$p.value[1], expected_result$p.value[1],
#                tol = 1e-40)
#   expect_equal(res.S$res.hierarchy$p.value[2], expected_result$p.value[2],
#                tol = 1e-30)
#   expect_equal(res.S$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
# })
#
# #### Test parallel computations for multicore and sown ####
# test_that("Parallel computations (in test_only_hierarchy): Test parallel computations for multicore and sown", {
#   skip_on_bioc()
#
#   ## simulate index
#   n <- 800
#   p <- 10
#   B <- 50
#
#   ## simulate data
#   r1 <- gen_one(n = n, p = p, seed1 = 9229, ind.a = c(4, 1), seed3 = 8)
#   r2 <- gen_one(n = n, p = p, seed1 = 929, ind.a = c(4, 1), seed3 = 99)
#   r3 <- gen_one(n = n, p = p, seed1 = 99, ind.a = c(4, 1), seed3 = 100)
#   r4 <- gen_one(n = n, p = p, seed1 = 9, ind.a = c(4, 1), seed3 = 1111)
#
#   x <- list(r1$x, r2$x, r3$x, r4$x)
#   y <- list(r1$y, r2$y, r3$y, r4$y)
#   # clvar <- list(r1$clvar, r2$clvar, r3$clvar, r4$clvar)
#
#   # Block
#   block <- data.frame("var.names" = paste0("rsid", 1:10),
#                       "blocks" = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
#                       stringsAsFactors = FALSE)
#
#   ### parallel = "multicore"
#
#   # cluster the data
#   dendr <- cluster_vars(x = x, block = block, parallel = "multicore", ncpus = 2)
#   # plot(dendr$res.tree[[1]])
#
#   # multisplit
#   set.seed(744)
#   res.multisplit <- multisplit(x = x, y = y, family = "gaussian", B = B,
#                                parallel = "multicore", ncpus = 2)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", parallel = "multicore",
#                                ncpus = 2)
#
#
#   ## Test
#   # This list encodes the tree structure
#   cluster_test <- list(c("rsid1", "rsid2", "rsid3", "rsid4", "rsid5"),
#                        c("rsid1", "rsid2", "rsid3"),
#                        c("rsid4", "rsid5"),
#                        c("rsid1", "rsid2"),
#                        "rsid1",
#                        "rsid2",
#                        "rsid3",
#                        "rsid4",
#                        "rsid5")
#
#   res1 <- check_test_hierarchy(x = x[[1]], y = y[[1]], clvar = NULL,
#                                res.multisplit = res.multisplit[1],
#                                B = B, cluster_test = cluster_test)
#
#   res2 <- check_test_hierarchy(x = x[[2]], y = y[[2]], clvar = NULL,
#                                res.multisplit = res.multisplit[2],
#                                B = B, cluster_test = cluster_test)
#
#   res3 <- check_test_hierarchy(x = x[[3]], y = y[[3]], clvar = NULL,
#                                res.multisplit = res.multisplit[3],
#                                B = B, cluster_test = cluster_test)
#
#   res4 <- check_test_hierarchy(x = x[[4]], y = y[[4]], clvar = NULL,
#                                res.multisplit = res.multisplit[4],
#                                B = B, cluster_test = cluster_test)
#
#   pvals_to_be <- rbind(res1, res2, res3, res4)
#
#
#
#   # Tippett
#   compare_with <- apply(X = pvals_to_be, MARGIN = 2,
#                         FUN = function(x, len_y) {
#                           max(1 - (1 - min(x))^(len_y), .Machine$double.neg.eps)
#                         },
#                         len_y = 4)
#
#   expected_result <- data.frame(block = c(1, 1, 2),
#                                 p.value = c(compare_with[c("rsid1", "rsid4")], NA))
#   expected_result$significant.cluster <- list(c("rsid1"), c("rsid4"), c(NA))
#   rownames(expected_result) <- NULL
#   attr(expected_result, "class") <- c("data.frame")
#
#   expect_equal(res.T$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-40)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#
#   ### parallel = "snow"
#
#   # cluster the data
#   dendr <- cluster_vars(x = x, block = block, parallel = "snow", ncpus = 2)
#   # plot(dendr$res.tree[[1]])
#
#   # multisplit
#   set.seed(744)
#   res.multisplit <- multisplit(x = x, y = y, family = "gaussian", B = B,
#                                parallel = "snow", ncpus = 2)
#
#   # test hierarchy: Tippett
#   res.T <- test_only_hierarchy(x = x, y = y, dendr = dendr,
#                                res.multisplit = res.multisplit,
#                                family = "gaussian", parallel = "snow",
#                                ncpus = 2)
#
#   ## Test
#   expect_equal(res.T$res.hierarchy$p.value, expected_result$p.value,
#                tol = 1e-40)
#   expect_equal(res.T$res.hierarchy$significant.cluster,
#                expected_result$significant.cluster)
#
#
# })
