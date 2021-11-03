#' Hierarchical Testing
#'
#' Hierarchical testing for a given user-specified test function.
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
#' @param test.func a test function for groups of variables. See the 'Details' 
#' and 'Examples' sections.
#' @param clvar a matrix or list of matrices of control variables.
#' @param arg.all a list with arguments which is passed on to 
#' each call of the functions inputted to the arguments \code{test.func},
#' \code{compMOD.same}, and \code{compMOD.changing}. See the 'Details' section.
#' @param arg.all.fix a vector or list with arguments which is passed on to 
#' each call of the functions inputted to the arguments \code{test.func},
#' \code{compMOD.same}, and \code{compMOD.changing}. See the 'Details' section.
#' @param compMOD.same a function which is called once at the beginning of 
#' the hierarchical testing procedure and its output is passed on to each
#' call of \code{test.func}. See the 'Details' section.
#' @param compMOD.changing a function which is called once at the beginning of 
#' the hierarchical testing procedure and its purpose is to serve as a 
#' cache or memory if some output does not have to be recalculated for 
#' multiple group tests but the cache can be updated while testing. 
#' See the 'Details' section.
#' @param alpha the significant level at which the FWER is controlled.
#' @param global.test a logical value indicating whether the global test should
#' be performed.
#' @param hier.adj a logical value indicating whether the p-values can only 
#' increase when going down some given branch. Strong FWER control holds as 
#' well if the argument is set to FALSE which is the default option. 
#' @param mt.adj type of multiple testing correction to be used; 
#' either \code{"dpBF"} (depth-wise Bonferroni multiple adjustment) or \code{"none"}
#' (no adjustment). See the 'Details' section.
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
#' @details Hierarchical testing is performed by going top down through the 
#' hierarchical tree. Testing in some branch only continues if at least one 
#' child of a given cluster is significant. The function \code{\link{run_hierarchy}} 
#' requires the output of one of the functions \code{\link{cluster_vars}} or
#' \code{\link{cluster_positions}} as an input (argument \code{dendr}).
#'
#' The user can specify which hierarchical multiple testing adjustment for the 
#' hierarchical procedure is applied. The default is \code{"dpBF"} (depth-wise 
#' Bonferroni multiple adjustment). Alternatively, the user can choose \code{"none"} 
#' (no adjustment). The hierarchical multiple testing adjustment \code{"dpBF"} 
#' guarantees strong family-wise error control if the group test, which is applied 
#' for testing a given group, controls the type I error. 
#' 
#' The group test function has to be specified by the user; see argument 
#' \code{test.func}. It is required to have the following 
#' arguments: \code{x, y, clvar, colnames.cluster, arg.all, 
#' arg.all.fix, mod.large, mod.small}. Although it is fine if not all of them 
#' are used inside the function. Most of the arguments are described in the 
#' list of arguments above for the function \code{run_hierarchy} since they 
#' are as well specified by the user. The argument \code{colnames.cluster} contains 
#' the column names of the group of variables which should be tested. The arguments 
#' \code{mod.large} and \code{mod.small} are used as a cache or memory since 
#' one can often reuse, say, one initial lasso fit for all the subsequent 
#' group tests. The argument \code{mod.large} corresponds to some result of 
#' some initial calculation which is reused later. The argument \code{mod.small} 
#' can be updated continuously through the hierarchical sequential testing 
#' procedure. The test function is required to output or return a list with 
#' two elements, i.e. the p-value ("pval") calculated for the given group and 
#' some object ("mod.small"), which serves as a cache or memory and is passed on 
#' to the next function calls in the same branch; see example below. The latter 
#' can just be \code{NULL} if this feature is not used. Compare as well with the 
#' above arguments \code{compMOD.same} and \code{compMOD.changing} for the function 
#' \code{run_hierarchy}, and see the example below. 
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
#' and \code{\link{advance_hierarchy}}.
#'
#' @examples
#' # low-dimensional example with user specified test function
#' n <- 200
#' p <- 100
#' library(MASS)
#' set.seed(3)
#' x <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))
#' colnames(x) <- paste0("Var", 1:p)
#' beta <- rep(0, p)
#' beta[c(5, 20, 46)] <- 1
#' y <- x %*% beta + rnorm(n)
#'
#' dendr1 <- cluster_vars(x = x)
#' 
#' # Define own test function: partial F-test
#' # Many of the arguments of the function below might not be 
#' # used but the function is required to have those arguments.
#' my.test <- function(x, y, clvar, colnames.cluster, arg.all, 
#'                     arg.all.fix, mod.large, mod.small) {
#'                       
#'                        # generate design matrices
#'                        data.large <- cbind(clvar, x)
#'                        setdiff.cluster <- setdiff(colnames(x), colnames.cluster)
#'                        data.small <- cbind(clvar, x[, setdiff.cluster]) 
#'                        # Replace data.small if set of indices setdiff.cluster is empty.
#'                        if (ncol(data.small) == 0) {data.small <- rep(1, length(y))}
#'                        
#'                        # compare the models using a partial F test
#'                        pval <- anova(lm(y ~ data.small),
#'                                      lm(y ~ data.large),
#'                                      test = "F")$P[2]
#'                        
#'                        return(list("pval" = pval, "mod.small" = NULL))
#'                     }
#' 
#' # run hierarchical testing
#' set.seed(76)
#' sign.clusters1 <- run_hierarchy(x = x, y = y, dendr = dendr1,
#'                                 test.func = my.test)
#'
#' ## With block
#' # I.e. second level of the hierarchical tree is specified by 
#' # the user. This would allow to run the code in parallel; see the 'Details'
#' # section.
#' # The column names of the data frame block are optional.
#' block <- data.frame("var.name" = paste0("Var", 1:p),
#'                     "block" = rep(c(1, 2), each = p/2))
#' dendr2 <- cluster_vars(x = x, block = block)
#' set.seed(76)
#' sign.clusters2 <- run_hierarchy(x = x, y = y, dendr = dendr2,
#'                                 test.func = my.test)
#'
#' # Access part of the return object or result
#' sign.clusters2[, "block"]
#' sign.clusters2[, "p.value"]
#' # Column names or variable names of the significant cluster in the first row.
#' sign.clusters2[[1, "significant.cluster"]]
#' 
#' @references 
#' Meinshausen, N. (2008). Hierarchical testing of variable importance. 
#' Biometrika, 95(2), 265-278.
#' Renaux, C., Buzdugan, L., Kalisch, M., and Bühlmann, P. (2020). Hierarchical inference for 
#' genome-wide association studies: a view on methodology with software. 
#' Computational Statistics, 35(1), 1-40.
#'
#' @name run_hierarchy
#' @export

run_hierarchy <- function(x, y, dendr, test.func,
                          clvar = NULL,
                          arg.all = list(NULL), arg.all.fix = NULL,
                          compMOD.same = function(...) NULL, # dummy place holder / default
                          compMOD.changing = function(...) NULL,
                          # block argument....
                          alpha = 0.05, global.test = TRUE,

                          hier.adj = FALSE,
                          mt.adj = c("dpBF", "none"), # "SBH"
                          agg.method = c("Tippett", "Stouffer"),

                          verbose = FALSE, sort.parallel = TRUE,
                          parallel = c("no", "multicore", "snow"),
                          ncpus = 1L, cl = NULL) {

  block <- dendr$block
  dendr <- dendr$res.tree
  # family <- match.arg(family)
  mt.adj <- match.arg(mt.adj)
  agg.method <- match.arg(agg.method)
  parallel <- match.arg(parallel)
  do.parallel <- (parallel != "no" && ncpus > 1L)

  if (do.parallel && parallel == "multicore" && .Platform$OS.type == "windows") {
    stop("The argument parallel = 'multicore' is not available for windows. Use parallel = 'snow' for parallel computing or parallel = 'no' for serial execution of the code.")
  }

  # browser()
  res <- check_input_testing(x = x, y = y, clvar = clvar,
                             check_testing_arguments = TRUE,
                             dendr = dendr, block = block, alpha = alpha,
                             global.test = global.test,
                             agg.method = agg.method, verbose = verbose)
  
  x <- res$x
  y <- res$y
  clvar <- res$clvar
  unique.colnames.x <- res$unique_colnames_x
  rm(list = c("res"))

  len.y <- length(y)
  if (verbose & len.y > 1) {
    message(paste("Jointly analyzing ", len.y, " phenotypes..."))
  }

  # Defining the weights for aggregating the p-values using Stouffer's method
  stouffer.weights <- vapply(X = x, FUN = function(x) {nrow(x)}, FUN.VALUE = 1)
  stouffer.weights <- sqrt(stouffer.weights / sum(stouffer.weights))

  # Calculate the model output which stays the same over the entire tree
  mod.large <- compMOD_large(compMOD_same = compMOD.same, x = x, y = y, clvar = clvar,
                             arg.all = arg.all, arg.all.fix = arg.all.fix)

  # Create skeleton of the object which is updated during testing. Works like a cache
  mod.small <- compMOD_small(compMOD_changing = compMOD.changing, x = x, y = y, clvar = clvar,
                             arg.all = arg.all, arg.all.fix = arg.all.fix)

  # The variable minimal.pval is used for the hierarchical adjustment.
  # The p-value of a subcluster has to be as least as large as the p-value of
  # its parent.
  minimal.pval <- 0

  # This variable is used to stop testing if the global null hypothesis or all
  # the null hypotheses on the block level could not be rejected.
  continue.testing <- TRUE

  # This variable is used in order to store the warnings on the block level.
  warnings.to.return <- NULL

  ### testing the global null hypothesis ###
  if (global.test) {
    if (verbose) {
      message("Testing the global null hypothesis..")
    }
    # calculate the global p-value
    res.global <- tryCatch_W_E(comp_cluster_pval(x = x, y = y, clvar = clvar,
                                                 test.func = test.func,
                                                 arg.all = arg.all,
                                                 arg.all.fix = arg.all.fix,
                                                 colnames.cluster = unique.colnames.x,
                                                 minimal.pval = minimal.pval,
                                                 hier.adj = hier.adj,
                                                 mt.adj = mt.adj,
                                                 # Set MD.factor to 1. The MD.factor is only used if mt.adj = "MD"
                                                 MD.factor = 1,
                                                 agg.method = agg.method,
                                                 mod.large = mod.large,
                                                 mod.small = mod.small,
                                                 stouffer.weights = stouffer.weights),
                               ret.obj = list("cluster" = list("colnames.cluster" = NULL,
                                                               "pval" = NULL),
                                              "mod.small" = mod.small,
                                              "MD.factor" = NULL))

    # If some warning occurred, then continue testing but report the warning
    # messages as an attribute of the return object.
    warnings.to.return <- res.global$warning

    # If an error occurred during the computation of the global hypothesis,
    # then output all the error messages and stop running.
    if (!is.null(error.msg <- res.global$error)) {
      stop(print_error_warning(error.msg = error.msg, warning.msg = warnings.to.return,
                               location.msg = "testing the global hypothesis."))
    }

    # check if the global p-value is significant
    if (res.global$value$cluster$pval > alpha) {
      # the global p-value is larger than alpha
      if (verbose) {
        message("The global null hypothesis cannot be rejected.")
      }
      continue.testing <- FALSE
      signif.clusters <- list(list(value = list(name.block = NA,
                                                signif.clusters = list(
                                                  list(pval = NULL,
                                                       colnames.cluster = NULL))),
                                   error = NULL,
                                   warning = NULL))
    } else {
      # the global p-value is smaller than alpha => continue testing
      if (verbose) {
        message("The global null hypothesis was rejected.")
      }
      minimal.pval <- res.global$value$cluster$pval
      mod.small <- res.global$value$mod.small
    }
  }

  ### testing the blocks given by the argument block ###
  if (!is.null(block) & continue.testing) {
    if (verbose) {
      message("Testing the blocks...")
      # TODO find some better message: maybe subsets
      # testing .... number of blocks and their subsets
      # Testing the top clusters defined by the input block.
    }
    # test the blocks

    # The function split or divides the data x into blocks defined by f and stores
    # it in a list.
    colnames.per.block <- split(x = block[, 1], f = block[, 2])
    tbl.blocks <- sort(table(block[, 2]), decreasing = TRUE)

    if (sort.parallel) {
      # Sort the blocks such that we test the large blocks first. This is
      # faster if we have less nodes / cpu's compared to the number of blocks.
      name.blocks <- names(tbl.blocks)
      colnames.per.block <- colnames.per.block[name.blocks]
      tbl.blocks <- tbl.blocks[name.blocks]
    } else {
      name.blocks <- unique(block[, 2])
    }

    # Define MD.factor.blocks
    MD.factor.blocks <- if (mt.adj == "SBH") {
      tbl.blocks / sum(tbl.blocks)
    } else {
      # the values in the table could be replaced by any numeric except
      # an object of length 0 like NULL
      tbl.blocks
    }

    # The concept of how to elegantly parallelize a function call (and save
    # all warning and error messages) is taken from the package boot
    # respectively lme4. Both are nearly identical in that respect.
    # See the source code of the package boot: R/bootfuns.q in the function
    # boot().
    # See the source code of the package lme4: R/bootMer.R in the function
    # bootMer().

    # Using a closure, the function below can access all the variables of the
    # environment in which it was created. This makes parallel computation
    # leaner or simpler, i.e. there are less arguments or we do not have to
    # export objects to the workers in the PSOCKcluster case
    comp_per_blocks <- local({
      x
      y
      clvar
      colnames.per.block
      test.func
      arg.all
      arg.all.fix
      minimal.pval
      hier.adj
      mt.adj
      MD.factor.blocks
      agg.method
      mod.large
      mod.small
      stouffer.weights
      function(name.block) {
        tryCatch_W_E(comp_cluster_pval(x = x, y = y, clvar = clvar,
                                       test.func = test.func,
                                       arg.all = arg.all,
                                       arg.all.fix = arg.all.fix,
                                       colnames.cluster = colnames.per.block[[name.block]],
                                       minimal.pval = minimal.pval,
                                       hier.adj = hier.adj,
                                       mt.adj = mt.adj,
                                       MD.factor = MD.factor.blocks[name.block],
                                       agg.method = agg.method,
                                       mod.large = mod.large,
                                       mod.small = mod.small,
                                       stouffer.weights = stouffer.weights),
                     ret.obj = list("cluster" = list("colnames.cluster" = NULL,
                                                     "pval" = NULL),
                                    "mod.small" = mod.small,
                                    "MD.factor" = NULL))
      }})

    res.blocks <- if (do.parallel) {
      if (parallel == "multicore") {
        parallel::mclapply(name.blocks, comp_per_blocks, mc.cores = ncpus)
      } else if (parallel == "snow") {
        if (is.null(cl)) {
          cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
          # export the namespace of hierbase in order for the use the functions
          # of the package hierbase on the workers
          parallel::clusterExport(cl, varlist = getNamespaceExports("hierbase"))
          if(RNGkind()[1L] == "L'Ecuyer-CMRG")
            parallel::clusterSetRNGStream(cl)
          res <- parallel::parLapply(cl, name.blocks, comp_per_blocks)
          parallel::stopCluster(cl)
          cl <- NULL # overwrite object which is responsible for the connection
          res
        } else parallel::parLapply(cl, name.blocks, comp_per_blocks)
      }
    } else lapply(name.blocks, comp_per_blocks)

    res.blocks <- do.call(cbind, res.blocks)

    # If some warning occurred, then continue testing but report the warning
    # messages as an attribute of the return object.
    warning.msg <- do.call(c, res.blocks["warning", ])
    warnings.to.return <- c(warnings.to.return, warning.msg)

    # If an error occurred during the computation per block, then output all the
    # error messages and stop running.
    if (!is.null(error.msg <- do.call(c, res.blocks["error", ]))) {
      stop(print_error_warning(error.msg = error.msg, warning.msg = warning.msg,
                               location.msg = "testing each block."))
    }

    # Check if any p-value of the blocks is significant.
    if (all(do.call(c, do.call(cbind, do.call(cbind, res.blocks["value", ])["cluster", ])["pval", ]) > alpha)) { ### TODO
      # All p-values of the blocks are larger than alpha
      if (verbose) {
        message("None of the null hypotheses for each block could be rejected.")
        message("Testing stops.")
      }
      if (global.test) {
        continue.testing <- FALSE
        signif.clusters <- list(list(value = list(name.block = NA,
                                                  signif.clusters =
                                                    list(res.global$value$cluster)),
                                     error = NULL,
                                     warning = NULL)) # See warnings.to.return
      } else {
        continue.testing <- FALSE
        signif.clusters <- list(list(value = list(name.block = NA,
                                                  signif.clusters = list(
                                                    list(pval = NULL,
                                                         colnames.cluster = NULL))),
                                     error = NULL,
                                     warning = NULL)) # See warnings.to.return
      }
    } else {
      # the p-value of the subset SNP_index is smaller than alpha => continue
      # testing
      if (verbose) {
        message("The null hypothesis of at least one block was rejected.")
        message("Testing the hierarchy of the corresponding significant blocks...")
        # TODO check the wording of the messages
      }

      # browser()
      # TODO check inherit level to others!!!!!

      # calculate new MD.factor (multiple testing adjustment: inheritance rule)
      if (mt.adj == "SBH") {
        # check which clusters are significant
        ind.signif <- vapply(res.blocks["value", ],
                             FUN = function(x) {ifelse(x$cluster$pval <= alpha, TRUE, FALSE)},
                             FUN.VALUE = TRUE)

        # inherit or calculate new MD.factor
        MD.factor.blocks[!ind.signif] <- 0
        MD.factor.new <- MD.factor.blocks / sum(MD.factor.blocks)

        # replace MD.factor in res.blocks with new values
        res.blocks["value", ] <- lapply(seq_len(length(res.blocks["value", ])),
                                        function(x, res, MD.factor.new) {
                                          res[[x]]$MD.factor <- MD.factor.new[x]
                                          return(res[[x]])
                                        }, MD.factor.new = MD.factor.new,
                                        res = res.blocks["value", ])
      }
    }
  }

  ### prepare the input for the iterative testing for two special cases ###
  # Prepare the inputs for the function call of iterative_testing if the user
  # did not specify the argument block or the user did not specify the argument
  # block PLUS did set global.test to FALSE.
  if (is.null(block) & continue.testing) {
    # The function mapply cannot deal with arguments to vectorize over where
    # some arguments have strictly positive length and other arguments like
    # block have length 0. We use that list(NULL) has length 1 because it is
    # a list containing one element.
    name.blocks <- block

    # The second condition is to ensure that we test the top level of the tree
    # if the top level of the tree is not the same as the full data set /
    # global null. This makes it possible to use the package for each block,
    # say, chromosome separately as it is possible with the package hierGWAS.
    if (global.test) {
      if (length(setdiff(res.global$value$colnames.cluster, labels(dendr[[1]]))) == 0) { ### TODO
        # top level of tree = global null
        test.top.level <- FALSE
      } else {
        # top level of tree != global null
        test.top.level <- TRUE
      }
    } else {
      # global.test = FALSE
      test.top.level <- TRUE
    }

    if (test.top.level) {
      # Top level of tree is tested
      #
      # There are two cases:
      # 1) global.test = FALSE: Test the top cluster of the tree in order to
      # initialize the iterative testing procedure. The result has to be
      # stored in a list with one element.
      # 2) global.test = TRUE: The top level of the tree is not the same as
      # the full data set / global null. Test first the top cluster of the
      # tree before continuing.
      res.blocks <- list(tryCatch_W_E(comp_cluster_pval(x = x, y = y,
                                                        clvar = clvar,
                                                        test.func = test.func,
                                                        arg.all = arg.all,
                                                        arg.all.fix = arg.all.fix,
                                                        colnames.cluster = labels(dendr[[1]]),
                                                        minimal.pval = minimal.pval,
                                                        hier.adj = hier.adj,
                                                        mt.adj = mt.adj,
                                                        # Set MD.factor to 1. The MD.factor is only used if mt.adj = "MD"
                                                        MD.factor = 1,
                                                        agg.method = agg.method,
                                                        mod.large = mod.large,
                                                        mod.small = mod.small,
                                                        stouffer.weights = stouffer.weights),
                                      ret.obj = list("cluster" = list("colnames.cluster" = NULL,
                                                                      "pval" = NULL),
                                                     "mod.small" = mod.small,
                                                     "MD.factor" = NULL)))

      res.blocks <- do.call(cbind, res.blocks)

      # If some warning occurred, then continue testing but report the warning
      # messages as an attribute of the return object.
      warning.msg <- do.call(c, res.blocks["warning", ])
      warnings.to.return <- c(warnings.to.return, warning.msg)

      # If an error occurred during the computation per block, then output all the
      # error messages and stop running.
      if (!is.null(error.msg <- do.call(c, res.blocks["error", ]))) {
        stop(print_error_warning(error.msg = error.msg, warning.msg = warning.msg,
                                 location.msg = "testing the top level of the tree."))
      }

      # If global.test = TRUE and the top cluster of the tree is not
      # significant, then return the res.global. (If global.test = FALSE, then
      # the function iterative_testing takes care of that.)
      # We could omit the all() below because it's just one value but that
      # does not hurt.
      if (global.test & all(do.call(c, do.call(cbind, do.call(cbind, res.blocks["value", ])["cluster", ])["pval", ]) > alpha)) { ### TODO
        # All p-values of the blocks are larger than alpha
        if (verbose) {
          message("The null hypotheses of the top level of the tree could not be rejected.")
          message("Testing stops.")
        }
        continue.testing <- FALSE
        signif.clusters <- list(list(value = list(name.block = NA,
                                                  signif.clusters =
                                                    list(res.global$value$cluster)), ### TODO
                                     error = NULL,
                                     warning = NULL)) # See warnings.to.return
      }
    } else {
      # top level of tree does not have to be tested
        res.blocks <- list(res.global)

        res.blocks <- do.call(cbind, res.blocks)
    }
  }

  ### testing the hierarchy defined by the tree (for all significant blocks) ###
  if (continue.testing) {

    if (!is.null(block)) {
      # Sort the list of dendrograms.
      # It is needed if, say, sort.parallel is set to FALSE for the building of
      # the hierarchical tree but is set to TRUE for the function call of
      # test_hierarchy.
      dendr <- dendr[name.blocks]

      # We sort res.group and other objects first such that non-signif. blocks
      # are called or executed only at the end, i.e. this can help to balance
      # better the load between the different cores.
      if(sort.parallel) {
        # browser()

        ind.signif <- vapply(res.blocks["value", ],
                             FUN = function(x) {ifelse(x$cluster$pval <= alpha, TRUE, FALSE)},
                             FUN.VALUE = TRUE)
        ind.order <- order(ind.signif, decreasing = TRUE)
        dendr <- dendr[ind.order]
        name.blocks <- name.blocks[ind.order]
        res.blocks <- res.blocks[, ind.order, drop = FALSE]
      }
    }



    if(verbose) {
      message("Note that a messages is printed only every 20th iteration of the testing procedure.")
    }

    # The concept of how to elegantly parallelize a function call (and save
    # all warning and error messages) is taken from the package boot
    # respectively lme4. Both are nearly identical in that respect.
    # See the source code of the package boot: R/bootfuns.q in the function
    # boot().
    # See the source code of the package lme4: R/bootMer.R in the function
    # bootMer().

    # Using a closure, the function below can access all the variables of the
    # environment in which it was created. This makes parallel computation
    # leaner or simpler, i.e. there are less arguments or we do not have to
    # export objects to the workers in the PSOCKcluster case
    cluster_the_blocks <- local({
      x
      y
      clvar
      test.func
      arg.all
      arg.all.fix
      alpha
      verbose
      dendr
      name.blocks
      res.blocks
      hier.adj
      mt.adj
      agg.method
      mod.large
      # mod.small
      stouffer.weights
      function(i) {
        tryCatch_W_E(iterative_testing(x = x, y = y, clvar = clvar,
                                       test.func = test.func,
                                       arg.all = arg.all,
                                       arg.all.fix = arg.all.fix,
                                       dendr = dendr[[i]],
                                       name.block = name.blocks[i],
                                       # res.blocks contains the MD.factor
                                       res.block = res.blocks[["value", i]],
                                       alpha = alpha,
                                       verbose = verbose,
                                       hier.adj = hier.adj,
                                       mt.adj = mt.adj,
                                       agg.method = agg.method,
                                       mod.large = mod.large,
                                       # mod.small = mod.small,
                                       stouffer.weights = stouffer.weights),
                     ret.obj = list(name.block = name.blocks[i],
                                    signif.clusters = list(list(colnames.cluster = NULL,
                                                                pval = NULL))))
      }})


    # The sorting is done above during the testing of the block level.
    ind <- seq_len(dim(res.blocks)[2])

    signif.clusters <- if (do.parallel) {
      if (parallel == "multicore") {
        parallel::mclapply(ind, cluster_the_blocks, mc.cores = ncpus)
      } else if (parallel == "snow") {
        if (is.null(cl)) {
          cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
          # export the namespace of hierbase in order for the use the functions
          # of the package hierbase on the workers
          parallel::clusterExport(cl, varlist = getNamespaceExports("hierbase"))
          if(RNGkind()[1L] == "L'Ecuyer-CMRG")
            parallel::clusterSetRNGStream(cl)
          res <- parallel::parLapply(cl, ind, cluster_the_blocks)
          parallel::stopCluster(cl)
          res
        } else parallel::parLapply(cl, ind, cluster_the_blocks)
      }
    } else lapply(ind, cluster_the_blocks)
  }

  # browser()
  signif.clusters <- do.call(cbind, signif.clusters)
  sig.cl.compact <- lapply(X = signif.clusters["value", ], FUN = prepare_output)
  sig.cl.compact <- do.call(rbind, sig.cl.compact)
  colnames(sig.cl.compact) <- c("block", "p.value", "significant.cluster")
  rownames(sig.cl.compact) <- NULL

  resT <- sig.cl.compact
  # do.call() returns NULL if there occurred no errors.
  attr(resT,"errorMsgs") <- do.call(c, signif.clusters["error", ])
  # Add warning messages from block levels
  attr(resT, "warningMsgs") <- c(warnings.to.return,
                                 do.call(c, signif.clusters["warning", ]))

  if (!is.null(attr(resT, "errorMsgs"))) {
    warning("There occurred some errors while testing the hierarchy. See attribute 'errorMsgs' of the corresponding list element of the return object for more details.")
  }

  if (!is.null(attr(resT, "warningMsgs"))) {
    warning("There occurred some warnings while testing the hierarchy. See attribute 'warningMsgs' of the corresponding list element of the return object for more details.")
  }

  resT <- structure(resT, class = c("hierBase", "data.frame"))

  return(resT)
} # {run_hierarchy}

# Prepare the output
#
# This function changes the format of the output.
prepare_output <- function(signif.clusters) {
  name.block <-
    if (is.null(signif.clusters$name.block)) {
      NA
    } else {
      signif.clusters$name.block
    }
  len.sig.cl <- length(signif.clusters$signif.clusters)
  res.out <- cbind(name.block, data.frame(matrix(NA, nrow = len.sig.cl, ncol = 2)),
                   stringsAsFactors = FALSE)
  for (i in seq_len(len.sig.cl)) {
    res.out[i, 2:3] <-
      if (!is.null(signif.clusters$signif.clusters[[i]]$colnames.cluster)) {
        # We use a list instead of a vector in order for the p-value not to be
        # converted to a character.
        list(signif.clusters$signif.clusters[[i]]$pval,
             list(signif.clusters$signif.clusters[[i]]$colnames.cluster))
      } else {
        list(NA, list(NA)) # list(NA, NA) would work as well.
      }
  }
  return(res.out)
} # {prepare_output}

# Print error and warning messages
#
# This function prints error and warning messages.
print_error_warning <- function(error.msg, warning.msg, location.msg) {

  w.msg <- if (!is.null(warning.msg)) {
    paste("All the warning messages are printed below:", "\n",
          paste(warning.msg,collapse = "\n"))
  } else {
    NULL
  }

  stop.msg <- paste(paste("There occurred errors while", location.msg),
                    "All the error messages are printed below:",
                    paste(error.msg, collapse = "\n"),
                    "\n", w.msg, sep = "\n")
  return(stop.msg)
} # {print_error_warning}
