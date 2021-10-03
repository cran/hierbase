# Iterative testing
#
# The function goes top down through a given hierarchical tree and returns
# the significant clusters or single variables. A cluster is returned as
# a significant cluster if the cluster is significant and both children
# are not significant (if the cluster isn't a leaf). The list
# \code{cluster.queue} keeps track of the cluster to be tested.
iterative_testing <- function(x, y, clvar, dendr, name.block,
                              res.block, test.func, arg.all,
                              arg.all.fix, alpha, hier.adj,
                              mt.adj, verbose, agg.method,
                              mod.large, stouffer.weights) {

  # browser()

  ### check if the current cluster is significant ###
  if (res.block$cluster$pval > alpha){
    return(list(name.block = name.block,
                signif.clusters = list(list(colnames.cluster = NULL,
                                            pval = NULL))))
  }

  # Initialize the cluster.queue
  # This is a list with one element which contains a list with elements.
  cluster.queue <- list(list(dendr = dendr,
                             colnames.cluster = res.block$cluster$colnames.cluster,
                             pval = res.block$cluster$pval,
                             mod.small = res.block$mod.small,
                             MD.factor = res.block$MD.factor,
                             height = attr(dendr, "height")))
  # Note that res.block$pval is the same as minimal.pval for the children of
  # that cluster.

  # Define variables which are updated throughout the iterative testing
  # if multiple testing adjustment global inheritance, within this tree is chosen
  if (mt.adj == "SBH") {
    MD.factor.global <- res.block$MD.factor
    L.poss.signif <- length(res.block$cluster$colnames.cluster)
  }

  # maybe remove this line
  stopifnot(length(setdiff(labels(dendr), res.block$cluster$colnames.cluster)) == 0)

  # the significant clusters are stored in this following list
  signif.clusters <- vector("list", 0)

  # variable for counting iteration used for prinnting
  if (verbose) {
    iter <- 0
  }

  # the whole functions is a big loop over the list cluster.queue
  while (length(cluster.queue) > 0) {
    ### get the current cluster ###

    # Reorder the list cluster.queue such that the highest cluster in the queue is
    # tested first!
    # (Side-remark: cluster.queue contains clusters of size 2 or larger. In the 
    # code below, we test the two children of the given cluster.)
    ind <- order(do.call(c, do.call(cbind, cluster.queue)["height", ]), decreasing = TRUE)
    cluster.queue <- cluster.queue[ind]

    # extract some values of the first entry of the list
    clust <- cluster.queue[[1]]$dendr
    size.cluster <- length(cluster.queue[[1]]$colnames.cluster)
    clust.height <- cluster.queue[[1]]$height

    if (verbose) {
      # print only every 20th iteration
      iter <- iter + 1
      if (iter %% 20 == 1) {
        if (!is.null(name.block)) {
          message(paste0("For block ", name.block, ", testing a cluster at height ", clust.height, "."))
        } else if (is.null(name.block)) {
          message(paste0("Testing a cluster at height ", clust.height, "."))
        }
      }
    }


    # get the subclusters of the current cluster
    subclust <- cut(clust, h = clust.height)$lower
    fun_labels <- function(x, subclust) {
      labels(subclust[[x]])
    }
    colnames.subcluster <- lapply(seq_along(subclust), fun_labels,
                                  subclust = subclust)

    # Calculate new MD.factor for children
    MD.factor <- if (mt.adj == "SBH") {
      # multiple testing adjustment: global inheritance, within this tree
      n.colnames.subcluster <- vapply(X = colnames.subcluster, FUN = length,
                                      FUN.VALUE = 1L)
      n.colnames.subcluster / L.poss.signif * MD.factor.global
    } else {
      1 # could be any numeric except an object of length 0 like NULL
    }

    ### check the children of this cluster ###
    res.children <- mapply(FUN = comp_cluster_pval,
                           colnames.cluster = colnames.subcluster,
                           MD.factor = MD.factor,
                           MoreArgs = list(x = x, y = y,
                                           clvar = clvar,
                                           test.func =test.func,
                                           arg.all = arg.all,
                                           arg.all.fix = arg.all.fix,
                                           minimal.pval = cluster.queue[[1]]$pval,
                                           agg.method = agg.method,
                                           hier.adj = hier.adj,
                                           mt.adj = mt.adj,
                                           mod.large = mod.large,
                                           mod.small = cluster.queue[[1]]$mod.small,
                                           stouffer.weights = stouffer.weights),
                           SIMPLIFY = FALSE)

    # check which clusters are significant
    ind.signif <- vapply(res.children,
                         FUN = function(x) {ifelse(x$cluster$pval <= alpha, TRUE, FALSE)},
                         FUN.VALUE = TRUE)

    if (sum(ind.signif) == 0){
      # None of the children is significant
      # put the significant group in the list of results
      signif.clusters <- c(signif.clusters,
                           list(list(pval = cluster.queue[[1]]$pval,
                                     colnames.cluster = cluster.queue[[1]]$colnames.cluster)))

      if (mt.adj == "SBH") {
        L.poss.signif.subtract <- length(cluster.queue[[1]]$colnames.cluster)
        MD.factor.global.subtract <- cluster.queue[[1]]$MD.factor
      }
    } else {
      # Add the significant children to the cluster.queue or add significant
      # leaves to the signif.clusters

      # Pass on MD.factor such that it is stored in the cluster.queue
      # We make use of this for mt.adj == "SBH", such that we
      # subtract the correct quanity from MD.factor.global.
      MD.factor.new <- MD.factor

      # prepare update of cluster.queue
      update.signif.clusters <- mapply(FUN = check_significant,
                                       res.child = res.children,
                                       subcluster = subclust,
                                       MD.factor.new = MD.factor.new,
                                       MoreArgs = list(alpha = alpha,
                                                       mt.adj = mt.adj),
                                       SIMPLIFY = FALSE)

      update.signif.clusters <- do.call(cbind, update.signif.clusters)

      # add only the list elements which are non-empty
      signif.tmp <- update.signif.clusters["signif.clusters.tmp", ]
      signif.clusters <- c(signif.clusters, signif.tmp[lengths(signif.tmp) > 0L])

      queue.tmp <- update.signif.clusters["cluster.queue.tmp", ]
      cluster.queue <- c(cluster.queue, queue.tmp[lengths(queue.tmp) > 0L])

      if (mt.adj == "SBH") {
        L.poss.signif.subtract <- sum(do.call(c, update.signif.clusters["L.poss.signif.subtract", ]))
        MD.factor.global.subtract <- sum(do.call(c, update.signif.clusters["MD.factor.global.subtract", ]))
      }
    }

    # Update L.poss.signif and MD.factor.global if applicable
    if (mt.adj == "SBH") {
      # L.poss.signif is updated because some clusters (i.e. all or some
      # children) could not be shown to be significant or a MD was found
      L.poss.signif <- L.poss.signif - L.poss.signif.subtract

      # MD.factor is updated because a minimal detection was found
      # Two cases:
      # 1) all children could not be shown to be signif. => parent is MD
      # 2) all or some children are signif. & are leaves => signif. leaves are MDs
      MD.factor.global <- MD.factor.global - MD.factor.global.subtract
    }

    # remove current cluster from cluster.queue
    cluster.queue <- cluster.queue[-1]
  }

  if (verbose & !is.null(name.block)) {
    message(paste0("There have been found ", length(signif.clusters), " significant clusters for block ", name.block, ". Stop testing block ", name.block, "."))
  } else if (verbose & is.null(name.block)) {
    message(paste0("There have been found ", length(signif.clusters), " significant clusters. Stop testing."))
  }

  # return the list of significant clusters
  return(list(name.block = name.block,
              signif.clusters = signif.clusters))
} # {iterative_testing}

# Check if a cluster (it's a child of some cluster) is significant
#
# Check if a cluster (it's a child of some cluster) is significant. It
# is significant, then it has to be added to the list \code{cluster.queue}
# inside the function \code{iterative_testing}.
check_significant <- function(res.child, alpha, subcluster, mt.adj,
                              MD.factor.new = 1) {
  cluster.queue.tmp <-
    if(res.child$cluster$pval > alpha) {
      # child could not be shown to be significant
      NULL
    } else if(length(res.child$cluster$colnames.cluster) == 1) {
      # sinificant leave found
      NULL
    } else {
      # significant child of size larger or equal to two found
      list(dendr = subcluster,
           colnames.cluster = res.child$cluster$colnames.cluster,
           pval = res.child$cluster$pval,
           mod.small = res.child$mod.small,
           MD.factor = MD.factor.new,
           height = attr(subcluster, "height"))
    }

  signif.clusters.tmp <-
    if(res.child$cluster$pval > alpha) {
      # child could not be shown to be significant
      NULL
    } else if(length(res.child$cluster$colnames.cluster) == 1) {
      # sinificant leave found
      list(pval = res.child$cluster$pval,
           colnames.cluster = res.child$cluster$colnames.cluster)
    } else {
      # significant child of size larger or equal to two found
      NULL
    }

  if (mt.adj != "SBH") {
    L.poss.signif.subtract <- NULL
    MD.factor.global.subtract <- NULL
  } else {
    # calcualte  L.poss.signif.subtract and MD.factor.global.subtract for
    # mt.adj == "SBH"
    L.poss.signif.subtract <-
      if(res.child$cluster$pval > alpha) {
        # child could not be shown to be significant
        length(res.child$cluster$colnames.cluster)
      } else if(length(res.child$cluster$colnames.cluster) == 1) {
        # sinificant leave found
        1
      } else {
        # significant child of size larger or equal to two found
        0
      }

    MD.factor.global.subtract <-
      if(res.child$cluster$pval > alpha) {
        # child could not be shown to be significant
        0
      } else if(length(res.child$cluster$colnames.cluster) == 1) {
        # sinificant leave found
        MD.factor.new # for mt.adj == "SBH", this is identical to MD.factor
      } else {
        # significant child of size larger or equal to two found
        0
      }
  }

  return(list(cluster.queue.tmp = cluster.queue.tmp,
              signif.clusters.tmp = signif.clusters.tmp,
              L.poss.signif.subtract = L.poss.signif.subtract,
              MD.factor.global.subtract = MD.factor.global.subtract))
} # {check_significant}

