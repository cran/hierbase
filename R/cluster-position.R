#' Build Hierarchical Tree based on Position
#'
#' Build a hierarchical tree based on the position of the variables.
#'
#' @param position a data frame with two columns specifying the variable names
#' and the corresponding position or a list of data frames for multiple data
#' sets. The first column is required to contain the
#' variable names and to be of type character. The second column is required to
#' contain the position and to be of type numeric.
#' @param block a data frame or matrix specifying the second level of the
#' hierarchical tree. The first column is required to contain the
#' variable names and to be of type character. The second column is required to
#' contain the group assignment and to be a vector of type character or numeric.
#' If not supplied, the second level is built based on the
#' data.
#' @param sort.parallel a logical indicating whether the blocks should be sorted with respect to
#' the size of the block. This can reduce the run time for parallel computation.
#' @param parallel type of parallel computation to be used. See the 'Details' section.
#' @param ncpus number of processes to be run in parallel.
#' @param cl an optional \strong{parallel} or \strong{snow} cluster used if
#' \code{parallel = "snow"}. If not supplied, a cluster on the local machine is created.
#'
#' @details
#' The hierarchical tree is built based on recursive binary partitioning of
#' consecutive variables w.r.t. their position. The partitioning consists of
#' splitting a given node / cluster into two children of about equal size based
#' on the positions of the variables. If a node contains an odd number of
#' variables, then the variable in the middle w.r.t. position is assigned to
#' the cluster containing the closest neighbouring variable.
#' Hence, clusters at a given depth of the binary hierarchical tree contain
#' about the same number of variables.
#'
#' If the argument \code{block} is supplied, i.e. the second level of the
#' hierarchical tree is given, the function can be run in parallel across
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
#' @seealso \code{\link{cluster_vars}}, \code{\link{advance_hierarchy}}, 
#' and \code{\link{run_hierarchy}}.
#'
#' @examples
#' # The column names of the data frames position and block are optional.
#' position <- data.frame("var.name" = paste0("Var", 1:500),
#'                        "position" = seq(from = 1, to = 1000, by = 2))
#' dendr1 <- cluster_positions(position = position)
#'
#' block <- data.frame("var.name" = paste0("Var", 1:500),
#'                     "block" = rep(c(1, 2), each = 250),
#'                     stringsAsFactors = FALSE)
#' dendr2 <- cluster_positions(position = position, block = block)
#'
#'
#' @references 
#' Meinshausen, N. (2008). Hierarchical testing of variable importance. 
#' Biometrika, 95(2), 265-278.
#' Renaux, C., Buzdugan, L., Kalisch, M., and Bühlmann, P. (2020). Hierarchical inference for 
#' genome-wide association studies: a view on methodology with software. 
#' Computational Statistics, 35(1), 1-40.
#'
#' @name cluster_positions
#' @export

cluster_positions <- function(position, block = NULL,sort.parallel =  TRUE,
                             parallel = c("no", "multicore", "snow"),
                             ncpus = 1L, cl = NULL) {

  parallel <- match.arg(parallel)
  do.parallel <- (parallel != "no" && ncpus > 1L)

  if (do.parallel && parallel == "multicore" && .Platform$OS.type == "windows") {
    stop("The argument parallel = 'multicore' is not available for windows. Use parallel = 'snow' for parallel computing or parallel = 'no' for serial execution of the code.")
  }

  if (!is.list(position)) { # A data.frame is as well a list.
    stop("The input position is required to be a data.frame or a list of data.frames.")
  }
  if (is.list(position) & !is.data.frame(position)) {
    position <- do.call(rbind, position)
    position <- unique(position) # Alternatively position[!duplicated(position), ]
  }

  ## check input
  check_input_pos(position = position, block = block)

  resultDendr <-
    if (!is.null(block)) {
      ## build the hierarchical tree: given position and block ##

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
        position
        block
        function(givenBlock) {
          tryCatch_W_E(cluster_position_one_block(position = position, block = block,
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

    } else if (is.null(block)) {
      ## Cluster the SNPs: given x and no blocks ##
      list(tryCatch_W_E(cluster_position_one_block(position = position, block = NULL,
                                          givenBlock = NULL),
                        ret.obj = NA))
    }

  # Assign names to each of the dendrograms, i.e. the list elements
  # The condition “!is.null(x) &“ is maybe not needed, but it doesn't hurt.
  if (!is.null(block)) {
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
} # {cluster_var}

# Builds a hierarchical tree based on the position of the variables
#
# Builds a hierarchical tree based on the position of the variables for
# given \code{position} or for a given block \code{givenBlock} (and
# \code{position}). An object of class dendrogramm is returned.
cluster_position_one_block <- function(position, block, givenBlock) {

  if (!is.null(block)) {
    # case if a block is given
    position <- position[which(position[, 1] %in% block[block[, 2] == givenBlock, 1]), ]
    # The argument of %in% are coerced to a common type.
  }

  # sort position
  position <- position[order(position[, 2]), ]
  # position[, 3] <- 1:nrow(position)

  # If there are negative positions, then add a constant to make all of them
  # positive. This does not change the order of the variables and the distance
  # between the variables.
  if (min(position[, 2]) <= 0) {
    position[, 2] <- position[, 2] - min(position[, 2]) + 1
  }

  ## calculate the dendrogram
  x.hclust <- calculate_hclust_obj(pos.x = position)
  x.dendr <- stats::as.dendrogram(object = x.hclust)

  return(x.dendr)
} # {cluster_position_one_block}

# Builds a hierarchical tree by grouping variables with consecutive positions
#
# Builds a hierarchical tree by grouping variables with consecutive positions.
# An object of class \code{hclust} is returned. The element height of the
# returned object is chosen by us because there is no definition of a height
# for a given split if this method is used. This means that the heights have
# no deeper meaning. They are anyway not used by the hierarchical testing.
calculate_hclust_obj <- function(pos.x) {
  # matrix
  n <- nrow(pos.x)

  # negative numbers are leaves
  # positive are merged clusters (defined by row number in merge)
  merge <- matrix(NA, ncol = 2, nrow = (n - 1))

  cluster.queue <- vector("list", n - 1)
  # list element: number given to that cluster, first variable, last variable
  cluster.queue[[n - 1]] <- c(n - 1, 1, n)
  # current cluster for the matrix merge of an object of class hclust
  curr.cluster <- n - 2

  for (i in (n - 1):1) {
    cluster <- cluster.queue[[i]][2:3]
    stopifnot(cluster.queue[[i]][1] == i)

    # calculate split, i.e. two new clusters
    res.split <- split_cluster(cluster = cluster, pos.x = pos.x)
    # calculate the line of the merge matrix, update current cluster, and update
    # the cluster queue
    res.merge <- get_merge_line(res.split = res.split,
                                curr.cluster = curr.cluster)
    # update values
    merge[i, ] <- res.merge$line.merge
    curr.cluster <- res.merge$curr.cluster
    if (!is.null(res.merge$queue1)) {
      stopifnot(is.null(cluster.queue[[ res.merge$queue1[1] ]]))
      cluster.queue[[ res.merge$queue1[1] ]] <- res.merge$queue1
    }
    if (!is.null(res.merge$queue2)) {
      stopifnot(is.null(cluster.queue[[ res.merge$queue2[1] ]]))
      cluster.queue[[ res.merge$queue2[1] ]] <- res.merge$queue2
    }
  }

  # create cluster of class hclust
  ret <- list()
  ret$merge <- merge
  ret$height <- seq(from = 0, to = 1, length.out = n - 1) # define merge heights
  ret$order <- seq_len(n)     # order of leaves
  ret$labels <- pos.x[, 1]    # labels of leaves
  class(ret) <- "hclust"      # define class

  return(ret)
} # {calculate_hclust_obj}

# Splits a cluster in two children
#
# The functions splits a cluster in two children given the first and the
# last index of the ordered variables (w.r.t. their position). This means
# that the argument \code{cluster} is a vector with two values in 1 to p
# where p is the number of variables. The first element is always smaller
# than the second element of \code{cluster}.
split_cluster <- function(cluster, pos.x) {

  if ((n.cluster <- (cluster[2] - cluster[1] + 1)) %% 2 == 0) {
    # even number of elements in that cluster
    new.clusters <- list(c(cluster[1], cluster[1] + (n.cluster / 2 - 1)),
                         c(cluster[1] + (n.cluster / 2 - 1) + 1, cluster[2]))
  } else {
    # odd number of elements in that cluster
    # check where to split: compare distance of the three variables
    # in the middel (w.r.t. to the position) of the cluster
    floor.mid.cluster <- cluster[1] + (floor(n.cluster / 2) - 1)
    dist.left <- pos.x[floor.mid.cluster + 1, 2] - pos.x[floor.mid.cluster, 2]
    dist.right <- pos.x[floor.mid.cluster + 2, 2] - pos.x[floor.mid.cluster + 1, 2]
    if (dist.left <= dist.right) {
      new.clusters <- list(c(cluster[1], floor.mid.cluster + 1),
                           c(floor.mid.cluster + 2, cluster[2]))
    } else {
      new.clusters <- list(c(cluster[1], floor.mid.cluster),
                           c(floor.mid.cluster + 1, cluster[2]))
    }
  }

    return(new.clusters)
} # {split_cluster}


# Calculates a line for the matrix \code{merge}
#
# Calculates a line for the matrix \code{merge} which is encodes the
# hierarchical tree. The matrix is used to define the object of class
# \code{hclust} in the function \coe{calculate_hclust_obj}.
#
# @return a list with the following elements
#    \item \code{line.merge} line for the matrix merge
#    \item \code{curr.cluster} current cluster
#    \item \code{queue1} cluster to be added to the queue
#    \item \code{queue2} cluster to be added to the queue
get_merge_line <- function(res.split, curr.cluster) {
  c1 <- res.split[[1]] # cluster 1
  c2 <- res.split[[2]] # cluster 2

  n1 <- c1[2] - c1[1] + 1 # number of variables in cluster / group 1
  n2 <- c2[2] - c2[1] + 1

  if (n1 > 1 & n2 > 1) {
    line.merge <- c(curr.cluster, curr.cluster - 1)
    queue1 <- c(curr.cluster, c1)
    queue2 <- c(curr.cluster - 1, c2)
    curr.cluster <- curr.cluster - 2
  } else if (n1 > 1 & n2 == 1) {
    line.merge <- c(curr.cluster, -c2[1])
    queue1 <- c(curr.cluster, c1)
    queue2 <- NULL
    curr.cluster <- curr.cluster - 1
  } else if (n1 == 1 & n2 > 1) {
    line.merge <- c(-c1[1], curr.cluster)
    queue1 <- NULL
    queue2 <- c(curr.cluster, c2)
    curr.cluster <- curr.cluster - 1
  } else if (n1 == 1 & n2 == 1) {
    line.merge <- c(-c1[1], -c2[1])
    # curr.cluster unchanged
    queue1 <- NULL
    queue2 <- NULL
  }

  return(list("line.merge" = line.merge, "curr.cluster" = curr.cluster,
              "queue1" = queue1, "queue2" = queue2))
} # {get_merge_line}

