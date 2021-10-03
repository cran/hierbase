#' Build a Hierarchical Tree based on Hierarchical Clustering
#'
#' Build a hierarchical tree based on hierarchical clustering of the variables.
#'
#' @param x a matrix or list of matrices for multiple data sets. The matrix or
#' matrices have to be of type numeric and are required to have column names
#' / variable names. The rows and the columns represent the observations and
#' the variables, respectively. Either the argument \code{x} or \code{d} has
#' to be specified.
#' @param d a dissimilarity matrix. This can be either a symmetric matrix of
#' type numeric with column and row names or an object of class
#' \code{\link{dist}} with labels. Either the argument \code{x} or \code{d} has
#' to be specified.
#' @param block a data frame or matrix specifying the second level of the
#' hierarchical tree. The first column is required to contain the
#' variable names and to be of type character. The second column is required to
#' contain the group assignment and to be a vector of type character or numeric.
#' If not supplied, the second level is built based on the
#' data.
#' @param method the agglomeration method to be used for the hierarchical
#' clustering. See \code{\link{hclust}} for details.
#' @param use the method to be used for computing covariances in the presence
#' of missing values. This is important for multiple data sets which do not measure
#' exactly the same variables. If data is specified using the argument \code{x}, the
#' dissimilarity matrix for the hierarchical clustering is calculated using
#' correlation. See the 'Details' section and \code{\link{cor}} for all the options.
#' @param sort.parallel a logical indicating whether the blocks should be sorted with respect to
#' the size of the block. This can reduce the run time for parallel computation.
#' @param parallel type of parallel computation to be used. See the 'Details' section.
#' @param ncpus number of processes to be run in parallel.
#' @param cl an optional \strong{parallel} or \strong{snow} cluster used if
#' \code{parallel = "snow"}. If not supplied, a cluster on the local machine is created.
#'
#' @details
#' The hierarchical tree is built by hierarchical clustering of the variables.
#' Either the data (using the argument \code{x}) or a dissimilarity matrix
#' (using the argument \code{d}) can be specified.
#'
#' If one or multiple data sets are defined using the argument \code{x},
#' the dissimilarity matrix is calculated by one minus squared empirical
#' correlation. In the case of multiple data sets, a single hierarchical
#' tree is jointly estimated using hierarchical clustering. The argument
#' \code{use} is important because missing values are introduced if the
#' data sets do not measure exactly the same variables. The argument
#' \code{use} determines how the empirical correlation is calculated.
#'
#' Alternatively, it is possible to specify a user-defined dissimilarity
#' matrix using the argument \code{d}.
#'
#' If the argument \code{x} and \code{block} are supplied, i.e. the
#' \code{block} defines the second level of the
#' hierarchical tree, the function can be run in parallel across
#' the different blocks by specifying the arguments \code{parallel} and
#' \code{ncpus}. There is an optional argument \code{cl} if
#' \code{parallel = "snow"}. There are three possibilities to set the
#' argument \code{parallel}: \code{parallel = "no"} for serial evaluation
#' (default), \code{parallel = "multicore"} for parallel evaluation
#' using forking, and \code{parallel = "snow"} for parallel evaluation
#' using a parallel socket cluster. It is recommended to select
#' \code{\link{RNGkind}("L'Ecuyer-CMRG")} and set a seed to ensure that
#' the parallel computing of the package \code{hierinf} is reproducible.
#' This way each processor gets a different substream of the pseudo random
#' number generator stream which makes the results reproducible if the arguments
#' (as \code{sort.parallel} and \code{ncpus}) remain unchanged. See the vignette
#' or the reference for more details.
#'
#' @return The returned value is an object of class \code{"hierD"},
#' consisting of two elements, the argument \code{"block"} and the
#' hierarchical tree \code{"res.tree"}.
#'
#' The element \code{"block"} defines the second level of the hierarchical
#' tree if supplied.
#'
#' The element \code{"res.tree"} contains a \code{\link{dendrogram}}
#' for each of the blocks defined in the argument \code{block}.
#' If the argument \code{block} is \code{NULL} (i.e. not supplied),
#' the element contains only one \code{\link{dendrogram}}.
#'
#' @seealso \code{\link{cluster_positions}}, \code{\link{advance_hierarchy}}, 
#' and \code{\link{run_hierarchy}}.
#'
#' @examples
#' library(MASS)
#' x <- mvrnorm(50, mu = rep(0, 100), Sigma = diag(100))
#' colnames(x) <- paste0("Var", 1:100)
#' dendr1 <- cluster_vars(x = x)
#'
#' # The column names of the data frame block are optional.
#' block <- data.frame("var.name" = paste0("Var", 1:100),
#'                     "block" = rep(c(1, 2), each = 50))
#' dendr2 <- cluster_vars(x = x, block = block)
#'
#' # The matrix x is first transposed because the function dist calculates
#' # distances between the rows.
#' d <- dist(t(x))
#' dendr3 <- cluster_vars(d = d, method = "single")
#'
#' @references Renaux, C., Bühlmann, P. (2021), Efficient Multiple Testing 
#' Adjustment for Hierarchical Inference. <arXiv:2104.15028>
#'
#' @name cluster_vars
#' @export

cluster_vars <- function(x = NULL, d = NULL, block = NULL, method = "average",
                        use = "pairwise.complete.obs", sort.parallel =  TRUE,
                        parallel = c("no", "multicore", "snow"), ncpus = 1L,
                        cl = NULL) {

  parallel <- match.arg(parallel)
  do.parallel <- (parallel != "no" && ncpus > 1L)

  if (do.parallel && parallel == "multicore" && .Platform$OS.type == "windows") {
    stop("The argument parallel = 'multicore' is not available for windows. Use parallel = 'snow' for parallel computing or parallel = 'no' for serial execution of the code.")
  }

  ## check input
  check_input_cl(x = x, d = d, method = method, block = block, use = use)

  ## case if x is specified
  if (!is.null(x)) {
    if (is.matrix(x)) {
      # if x is matrix, then save x as a list with one element
      x.all <- x
    }

    if (is.list(x)) {
      len.x <- length(x) # this corresponds to the number of data sets

      # TODO find more efficient way => e.g. plyr::rbind.fill.matrix => very
      # similar to the code below.

      # get all the column names and dimensions
      dim.x <- unlist(lapply(x, nrow))
      colnames.x <- lapply(x, colnames)

      # create one big matrix containing all the elements of the list x
      unique.colnames.x <- unique(x = unlist(colnames.x))
      x.all <- matrix(NA, nrow = sum(dim.x), ncol = length(unique.colnames.x))
      colnames(x.all) <- unique.colnames.x
      cumsum.dim <- cumsum(c(0, dim.x))
      for (i in seq_len(len.x)) {
         x.all[(cumsum.dim[i] + 1):(cumsum.dim[i + 1]), colnames.x[[i]]] <- x[[i]]
      }

    }
  }


  resultDendr <-
    if (!is.null(x) & !is.null(block)) {
      ## Cluster the variables: given x and block ##

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
        x.all
        block
        method
        use
        function(givenBlock) {
          tryCatch_W_E(cluster_one_block(x = x.all, block = block, d = NULL,
                                         method = method, use = use,
                                         givenBlock = givenBlock),
                       ret.obj = NA)
        }})

      if (sort.parallel) {
        # Sort the blocks such that we cluster the large blocks first. This is
        # faster if we have less nodes / cpu's compared to the number of blocks.
        unique.blocks <- names(sort(table(block[, 2]), decreasing = TRUE))
      } else {
        unique.blocks <- unique(block[, 2])
      }

      if (do.parallel) {
        if (parallel == "multicore") {
          parallel::mclapply(unique.blocks, cluster_the_blocks,
                             mc.cores = ncpus)
        } else if (parallel == "snow") {
          if (is.null(cl)) {
            cl <- parallel::makePSOCKcluster(rep("localhost", ncpus))
            # export the namespace of hierinf in order for the use the functions
            # of the package hierinf on the workers
            parallel::clusterExport(cl, varlist = getNamespaceExports("hierinf"))
            if(RNGkind()[1L] == "L'Ecuyer-CMRG")
              parallel::clusterSetRNGStream(cl)
            res <- parallel::parLapply(cl, unique.blocks, cluster_the_blocks)
            parallel::stopCluster(cl)
            res
          } else parallel::parLapply(cl, unique.blocks, cluster_the_blocks)
        }
      } else lapply(unique.blocks, cluster_the_blocks)

    } else if (!is.null(x) & is.null(block)) {
      ## Cluster the SNPs: given x and no blocks ##
      list(tryCatch_W_E(cluster_one_block(x = x.all, block = NULL, d = NULL,
                                          method = method, use = use,
                                          givenBlock = NULL),
                        ret.obj = NA))
    } else if (!is.null(d)) {
      ## Cluster the SNPs: given d ##
      list(tryCatch_W_E(cluster_one_block(x = NULL, block = NULL, d = d,
                                          method = method, use = use,
                                          givenBlock = NULL),
                        ret.obj = NA))
    }

  # Assign names to each of the dendrograms, i.e. the list elements
  # The condition “!is.null(x) &“ is maybe not needed, but it doesn't hurt.
  if (!is.null(x) & !is.null(block)) {
    names(resultDendr) <- unique.blocks
      # if (sort.parallel) {
      #   names(sort(table(block[, 2]), decreasing = TRUE))
      # } else {
      #   unique(block[, 2])
      # }
  }

  # prepare the return object
  resultDendr <- do.call(cbind, resultDendr)
  resD <- resultDendr["value", ]
  # do.call() returns NULL if there occurred no errors.
  attr(resD,"errorMsgs") <- do.call(c, resultDendr["error", ])
  attr(resD, "warningMsgs") <- do.call(c, resultDendr["warning", ])

  if (!is.null(attr(resD, "errorMsgs"))) {
    warning("There occurred some errors while clustering. See attribute 'errorMsgs' of the corresponding list element of the return object for more details.")
  }

  if (!is.null(attr(resD, "warningMsgs"))) {
    warning("There occurred some warnings while clustering. See attribute 'warningMsgs' of the corresponding list element of the return object for more details.")
  }

  resDD <- list("block" = block, "res.tree" = resD)
  resDD <- structure(resDD, class = c("hierD", "list"))
  return(resDD)
} # {cluster_vars}

# Builds a hierarchical tree using hierarchical clustering
#
# Builds a hierarchical tree using hierarchical clustering for a given
# dissimilarity matrix \code{d}, for a given \code{x}, or for a given block
# \code{givenBlock} (and x). An object of class dendrogramm is returned.
cluster_one_block <- function(x, d, method, block, use, givenBlock) {
  if (!is.null(x)) {
    if (!is.null(block)) {
      # case if a block is given
      indX <- which(colnames(x) %in% block[block[, 2] == givenBlock, 1])
      # The argument of %in% are coerced to a common type.
    }else{
      # case if no blocks are present
      indX <- seq_len(ncol(x))
    }
    # compute the distance matrix, if it was not provided
    d <- 1 - abs(stats::cor(x = x[, indX], use = use))^2
  }

  ## cluster the SNPs
  dist.matrix <- stats::as.dist(m = d)

  if (!all(!is.na(dist.matrix))) {
    stop("There are NA's in the calculated dissimilarity matrix / distance matrix. The variables cannot be clustered. This might be due to multiple data sets which do not contain all the same variables.")
    # TODO maybe give some candidates pairs!!!
  }

  x.hclust <- stats::hclust(d = dist.matrix, method = method)
  x.dendr <- stats::as.dendrogram(object = x.hclust)

  return(x.dendr)
} # {cluster_one_block}

