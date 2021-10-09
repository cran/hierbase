#' Hierarchical Testing
#'
#' Hierarchical testing for a given build-in test function.
#'
#' @param x a matrix or list of matrices for multiple data sets. The matrix or
#' matrices have to be of type numeric and are required to have column names
#' / variable names. The rows and the columns represent the observations and
#' the variables, respectively.
#' @param y a vector, a matrix with one column, or list of the aforementioned
#' objects for multiple data sets. The vector, vectors, matrix, or matrices
#' have to be of type numeric. 
#' @param dendr the output of one of the functions
#' \code{\link{cluster_vars}} or \code{\link{cluster_positions}}.
#' @param test a character string naming a 'build-in' group test function. 
#' See the 'Details' section.
#' @param clvar a matrix or list of matrices of control variables.
#' @param alpha the significant level at which the FWER is controlled.
#' @param global.test a logical value indicating whether the global test should
#' be performed.
#' @param hier.adj a logical value indicating whether the p-values can only 
#' increase when going down some given branch. Strong FWER control holds as 
#' well if the argument is set to FALSE which is the default option. 
#' @param mt.adj type of multiple testing correction to be used; 
#' either \code{"SBH"} (Sparse Branch Hierarchical multiple adjustment), 
#' \code{"dpBF"} (depth-wise Bonferroni multiple adjustment), or \code{"none"}
#' (no adjusment). See the 'Details' section.
#' @param agg.method a character string naming an aggregation method which
#' aggregates the p-values over the different data sets for a given cluster;
#' either \code{"Tippett"} (Tippett's rule) or \code{"Stouffer"}
#' (Stouffer's rule). This argument is only relevant if multiple data sets
#' are specified in the function call.
#' @param verbose a logical value indicating whether the progress of the computation
#' should be printed in the console.
#' @param sort.parallel a logical indicating whether the blocks should be sorted with respect to
#' the size of the block. This can reduce the run time for parallel computation.
#' @param parallel type of parallel computation to be used. See the 'Details' section.
#' @param ncpus number of processes to be run in parallel.
#' @param cl an optional \strong{parallel} or \strong{snow} cluster used if
#' \code{parallel = "snow"}. If not supplied, a cluster on the local machine is created.
#'
#' @details Hierarchical testing is performed by going top down through the hierarchical
#' tree. Testing in some branch only continues if at least one child of a given cluster 
#' is significant. The function \code{\link{advance_hierarchy}} requires the output
#' of one of the functions \code{\link{cluster_vars}} or
#' \code{\link{cluster_positions}} as an input (argument \code{dendr}).
#' 
#' The user can choose one of the 'build-in' group test functions that is applied 
#' to every group which is tested. The default is \code{test = "QF"} (inference for 
#' quadratic functionals in linear regression; see function \code{\link[SIHR]{QF}} in 
#' the R package \pkg{SIHR}). Alternatively, there are \code{"hdi"} (de-biased Lasso for 
#' linear regression; see function \code{\link[hdi]{lasso.proj}} in the R package 
#' \pkg{hdi}), \code{"hdi.logistic"} (de-biased Lasso for logistic regression; see function 
#' \code{\link[hdi]{lasso.proj}} in the R package \pkg{hdi}), \code{"F"} (classical partial
#' F-Test for low-dimensional data; see function \code{\link[stats]{anova}}), and 
#' \code{"logistic"} (likelihood ratio test for logistic regression for low-dimensional
#' data; see function \code{\link[stats]{anova}}). 
#' 
#' If one of the 'build-in' group test functions \code{"QF"}, \code{"hdi"}, or \code{"hdi.logistic"} 
#' is applied and control variables are specified using the argument \code{clvar}, then 
#' those variables are included in the model, there is an L1-penalty in the Lasso imposed on them, 
#' and obviously those covariates are not tested in the hierarchical procedure. 
#'
#' The user can specify which hierarchical multiple testing adjustment for the 
#' hierarchical procedure is applied. The default is \code{mt.adj = "SBH"} 
#' (Sparse Branch Hierarchical multiple adjustment), which is an improvement 
#' over so-called depth-wise Bonferroni with respect to power. 
#' Alternatively, the user can choose \code{"dpBF"} (depth-wise Bonferroni multiple 
#' adjustment) or \code{"none"} (no adjustment). The hierarchical multiple testing
#' adjustments \code{"SBH"} and \code{"dpBF"} guarantee strong family-wise error 
#' control if the group test, which is applied for testing a given group, 
#' controls the type I error. 
#'
#' If the argument \code{block} was supplied for the building
#' of the hierarchical tree (i.e. in the function call of either
#' \code{\link{cluster_vars}} or
#' \code{\link{cluster_positions}}), i.e. the second level of the
#' hierarchical tree was given, the hierarchical testing step can be run in
#' parallel across the different blocks by specifying the arguments
#' \code{parallel} and \code{ncpus}. There is an optional argument \code{cl} if
#' \code{parallel = "snow"}. There are three possibilities to set the
#' argument \code{parallel}: \code{parallel = "no"} for serial evaluation
#' (default), \code{parallel = "multicore"} for parallel evaluation
#' using forking, and \code{parallel = "snow"} for parallel evaluation
#' using a parallel socket cluster. It is recommended to select
#' \code{\link{RNGkind}("L'Ecuyer-CMRG")} and set a seed to ensure that
#' the parallel computing of the package \code{hierbase} is reproducible.
#' This way each processor gets a different substream of the pseudo random
#' number generator stream which makes the results reproducible if the arguments
#' (as \code{sort.parallel} and \code{ncpus}) remain unchanged. See the vignette
#' or the reference for more details.
#'
#' Note that if Tippett's aggregation method is applied for multiple data
#' sets, then very small p-values are set to machine precision. This is
#' due to rounding in floating point arithmetic.
#'
#' @return The returned value is an object of class \code{"hierBase"}, consisting 
#' a data.frame with the result of the hierarchical testing.
#'
#' The data.frame has the following columns:
#' \item{block}{\code{NA} or the name of the block if the significant cluster
#' is a subcluster of the block or is the block itself.}
#' \item{p.value}{The p-value of the significant cluster.}
#' \item{significant.cluster}{The column names of the members of the significant
#' cluster.}
#'
#' There is a \code{print} method for this class; see
#' \code{\link{print.hierBase}}.
#'
#' @seealso \code{\link{cluster_vars}}, \code{\link{cluster_positions}}, 
#' and \code{\link{run_hierarchy}}.
#'
#' @examples
#' ## Low-dimensonal example
#' n <- 100
#' p <- 50
#' library(MASS)
#' set.seed(3)
#' x <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))
#' colnames(x) <- paste0("Var", 1:p)
#' beta <- rep(0, p)
#' beta[c(5, 20, 46)] <- 1
#' y <- x %*% beta + rnorm(n)
#'
#' dendr1 <- cluster_vars(x = x)
#' set.seed(76)
#' sign.clusters1 <- advance_hierarchy(x = x, y = y, dendr = dendr1,
#'                                     test = "F")
#' 
#' ## High-dimensional example
#' if (FALSE) {
#'   n <- 50
#'   p <- 80
#'   library(MASS)
#'   set.seed(3)
#'   x <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))
#'   colnames(x) <- paste0("Var", 1:p)
#'   beta <- rep(0, p)
#'   beta[c(5, 20, 46)] <- 1
#'   y <- x %*% beta + rnorm(n)
#'  
#'   dendr1 <- cluster_vars(x = x)
#'   set.seed(76)
#'   sign.clusters1 <- advance_hierarchy(x = x, y = y, dendr = dendr1,
#'                                     test = "QF")
#'
#'   ## With block
#'   # I.e. second level of the hierarchical tree is specified by 
#'   # the user. This would allow to run the code in parallel; see the 'Details'
#'   # section.
#'   # The column names of the data frame block are optional.
#' 
#'   block <- data.frame("var.name" = paste0("Var", 1:p),
#'                       "block" = rep(c(1, 2), each = p/2))
#'   dendr2 <- cluster_vars(x = x, block = block)
#'   set.seed(76)
#'   sign.clusters2 <- advance_hierarchy(x = x, y = y, dendr = dendr2,
#'                                       test = "QF")
#'  
#'   # Access part of the return object or result
#'   sign.clusters2[, "block"]
#'   sign.clusters2[, "p.value"]
#'   # Column names or variable names of the significant cluster in the first row.
#'   sign.clusters2[[1, "significant.cluster"]]
#' }
#'
#' @references Renaux, C., Bühlmann, P. (2021), Efficient Multiple Testing 
#' Adjustment for Hierarchical Inference. <arXiv:2104.15028>
#'
#' @name advance_hierarchy
#' @export

advance_hierarchy <- function(x, y, dendr, 
                              test = c("QF", "hdi", "hdi.logistic", "F", "logistic"), 
                              clvar = NULL,
                              
                              alpha = 0.05, global.test = TRUE,
                              
                              hier.adj = FALSE,
                              mt.adj = c("SBH", "dpBF", "none"),
                              agg.method = c("Tippett", "Stouffer"),
                              
                              verbose = FALSE, sort.parallel = TRUE,
                              parallel = c("no", "multicore", "snow"),
                              ncpus = 1L, cl = NULL) {
  
  test <- match.arg(test)
  mt.adj <- match.arg(mt.adj)
  agg.method <- match.arg(agg.method)
  parallel <- match.arg(parallel)
  
  # Check dimensions if low-dimensional test is applied
  # For simplicity, we only check x and omit checking clvar. 
  if (test %in% c("F", "logistic")) {
    if(is.matrix(x)){ # if x is a matrix
      if (nrow(x) <= ncol(x)) {
        stop("You chose a test function for low-dimensional data but you have more variables than observations in your data.")
      }
    }
    if (is.list(x)) { # if x is a list of matrices
      len_x <- length(x)
      for (i in seq_len(len_x)) {
        if (nrow(x[[i]]) <= ncol(x[[i]])) {
          stop("You chose a test function for low-dimensional data but you have more variables than observations in your data.")
        }
      }
    }
  }
  
  # The test functions can be found in the R script test-functions.R
  if (test == "F") {
    test.func.tmp <- test.func.F
    arg.all.tmp <- list(NULL)
    arg.all.fix.tmp <- NULL
    compMOD.same.tmp <- compMOD.same.F
    compMOD.changing.tmp <- compMOD.NULL
  } else if (test == "logistic") {
    test.func.tmp <- test.func.LRT
    arg.all.tmp <- list(NULL)
    arg.all.fix.tmp <- NULL
    compMOD.same.tmp <- compMOD.same.LRT
    compMOD.changing.tmp <- compMOD.NULL
  } else if (test == "hdi") {
    test.func.tmp <- test.func.hdi
    arg.all.tmp <- list(NULL)
    arg.all.fix.tmp <- list(standardize = TRUE, # default
                            family = "gaussian")
    compMOD.same.tmp <- compMOD.same.hdi
    compMOD.changing.tmp <- compMOD.NULL
  } else if (test == "hdi.logistic") {
    test.func.tmp <- test.func.hdi
    arg.all.tmp <- list(NULL)
    arg.all.fix.tmp <- list(standardize = TRUE, # default
                            family = "binomial")
    compMOD.same.tmp <- compMOD.same.hdi
    compMOD.changing.tmp <- compMOD.NULL
  }
  else if (test == "QF") {
    p <- if (is.null(clvar)) {ncol(x)} else {ncol(x) + ncol(clvar)}
    test.func.tmp <- test.func.QF
    arg.all.tmp <- list(NULL)
    arg.all.fix.tmp <- list(Cov.weight = TRUE, # default
                            A = NULL, 
                            # previous version of SIHR: diag(1, p), 
                            # previous version of SIHR: intercept = TRUE, 
                            tau.vec = 1, 
                            lambda = NULL, # use glmnet CV with lambda.min
                            # lambda = "CV", # use glmnet CV with lambda.1se
                            mu = NULL, step = NULL, 
                            resol = 1.5, maxiter = 6)
    compMOD.same.tmp <- compMOD.same.QF
    compMOD.changing.tmp <- compMOD.NULL
  }
  
  
    run_hierarchy(x = x, y = y, dendr = dendr, 
                  test.func = test.func.tmp, # see above
                  clvar = clvar,
                  arg.all = arg.all.tmp,  # see above
                  arg.all.fix = arg.all.fix.tmp,  # see above
                  compMOD.same = compMOD.same.tmp,  # see above
                  compMOD.changing = compMOD.changing.tmp, # see above
                  
                  # block argument....
                  alpha = alpha, global.test = global.test,
                  hier.adj = hier.adj,
                  mt.adj = mt.adj,
                  agg.method = agg.method,
                  verbose = verbose, sort.parallel = sort.parallel,
                  parallel = parallel,
                  ncpus = ncpus, cl = cl)

  
} # {advance_hierarchy}

