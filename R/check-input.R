# check input of the function cluster_var
check_input_cl <- function(x, d, method, block, use) {
  ## check general assumptions
  if (!is.null(x) & !is.null(d)) {
    stop("Please specify only x or d.")
  }
  if (is.null(x) & is.null(d)) {
    stop("Please specify x or d.")
  }

  ## case if x is specified
  if (!is.null(x)) {
    check_x_block(x = x, block = block)
  }

  ## case if d is specified
  if (!is.null(d)) {
    if (!is.null(block)) {
      stop("The argument block can only be specified in connection with the input x.")
    }
    if (!is.numeric(d)) {
      stop("The input d must be numeric.")
    }
    if (any(is.na(d))) {
      stop("The input d is required to have no missing values.")
    }
    # The function is(d, "dist") checks whether the object d is of class dist.
    if(methods::is(d, "dist")) {
      if (is.null(labels(d))) {
        stop("The distance matrix d is required to have labels. The labels should correspond to the column names of the data set or data sets stored in x. For example, the function dist uses the row names to set the labels.")
      }
    }
    if(is.matrix(d)){
      if(nrow(d) != ncol(d)) {
        stop("The matrix d is required to have the same number of columns and rows.")
      }
      if(is.null(rownames(d)) | is.null(colnames(d))) {
        stop("The matrix d is required to have column and row names.")
      }
      if(!isSymmetric(d)) {
        stop("The matrix d is required to be symmetric.")
      }
    }
    # The function is(d, "dist") checks whether the object d is of class dist.
    if(!is.matrix(d) & !methods::is(d, "dist")) {
     stop("The argument d is required to be either a matrix or an object of class dist.")
    }
  }

  return(TRUE)
} # {check_input_cl}

# check input of the function cluster_position
check_input_pos <- function(position, block) {
  ## check argument position
  if (ncol(position) != 2) {
    stop("The input position or its list elements are required to have two columns.")
  }
  # Should we really require this!!!  = > for split in test_hierarchy to work
  # => yes
  if (!is.character(position[, 1])) {
    stop("The first column of position or of its list elements (column names of x) are required to be of type character.")
  }
  if (any(is.na(position))) {
    stop("There are missing values in the input position.")
  }
  if (length(unique(position[, 1])) < nrow(position)) {
    stop("The values in the first column of position (column names of x) are not unique. For multiple data sets, the combined version without duplicated rows (same variable name and position) is considered.")
  }
  if (!is.numeric(position[, 2])) {
    stop("The second column of position or of its list elements (the positions of the corresponding variables / columns in x) are required to be a numeric vector.")
  }
  if (length(unique(position[, 2])) < nrow(position)) {
    stop("The second column of the input position is required to encode unique positions of the corresponding variable / columns in x. For multiple data sets, the combined version without duplicated rows (same variable name and position) is considered.")
  }

  ## check argument block
  if (!is.null(block)) {
    check_block(block = block)
  }

  return(TRUE)
} # {check_input_pos}

# check input of the function multisplit, test_hierarchy, or
# test_hierarchy_given_multisplit
check_input_testing <- function(x, y, clvar,
                                # arguments for the function
                                # test_hierarchy_given_multisplit
                                check_testing_arguments,
                                dendr, block, alpha, global.test,
                                agg.method, verbose) {
  ## check x, y, and clvar
  res <- check_x_y_clvar(x = x, y = y, clvar = clvar)

  if (check_testing_arguments) {
    ## check block
    unique_colnames_x <- check_x_block(x = x, block = block)

    ## check dendr
    if (!is.list(dendr)) {
      stop("The input dendr is required to be a list of dendrograms.")
    }
    no_blocks <- length(unique(block[, 2]))
    if (length(dendr) != no_blocks & !is.null(block)) {
      stop("The number of blocks to be tested is different from the number of dendrograms in the list dendr.")
    }
    if (is.null(block) & length(dendr) != 1){
      stop("If the argument block is NULL or not specified, then the list dendr is required to have one element. This means that there is one tree for the hierarchical testing.")
    }

    ## check alpha
    if (!is.numeric(alpha) | length(alpha) != 1 | alpha < 0 | alpha > 1) {
      stop("The argument alpha is required to be a numeric value between 0 and 1.")
    }

    ## check global.test, agg.method, and verbose
    if (!is.logical(global.test) | length(global.test) != 1) {
      stop("The argument global.test is required to be of type logical and to be of length 1.")
    }
    if (!(agg.method %in% c("Tippett", "Stouffer")) | !is.character(agg.method) | length(agg.method) != 1) {
      stop("The argument agg.method is required to be of type character and to be of length 1.")
    }
    if (!is.logical(verbose) | length(verbose) != 1) {
      stop("The argument verbose is required to be of type logical and to be of length 1.")
    }

  } else {
    unique_colnames_x <- NULL
  }

  return(list(x = res$x, y = res$y, clvar = res$clvar, unique_colnames_x = unique_colnames_x))
} # {check_input_hierarchy}


check_x_block <- function(x, block) {
  ## check x
  if ((!is.list(x) & !is.matrix(x)) | is.data.frame(x)) {
    stop("The input x is required to be a matrix or a list of matrices if multiple data sets are present.")
  }
  if (is.matrix(x)) {
    if (!is.numeric(x)) {
      stop("The matrix x is required to be of type numeric or integer.")
    }
    if (any(is.na(x))) {
      stop("The matrix x is required to have no missing values.")
    }
    x <- list(x)
  }
  if (is.list(x)) {
    len_x <- length(x) # this corresponds to the number of data sets
    colnames_x <- vector(mode = "character", length = 0)
    for (i in seq_len(len_x)) {
      if (!is.numeric(x[[i]]) | !is.matrix(x[[i]])) {
        stop("The elements of the list x are required to be matrices of type numeric.")
      }
      if (any(is.na(x[[i]]))) {
        stop("The matrices (or matrix) which are stored in x are required to have no missing values.")
      }
      if (is.null(colnames(x[[i]]))) {
        stop("The matrices (or matrix) which are stored in x are required to have column names. If there is no natural naming convention, then one can set them to some integer, say, 1 to p.")
      }
      if (length(unique(colnames(x[[i]]))) != ncol(x[[i]])) {
        stop("Each of the matrices (or matrix) which are stored in x are required to have unique column names.")
      }
      colnames_x <- c(colnames_x, colnames(x[[i]]))
    }
    unique_colnames_x <- unique(x = colnames_x)
  }

  ## check block
  if (!is.null(block)) {
    check_block(block = block)
    if (sum(!(block[, 1] %in% unique_colnames_x)) > 0) {
      stop("There are values in the first column of block (column names of x) which are not a column name of x.")
    }
    if (sum(!(unique_colnames_x %in% block[, 1])) > 0) {
      stop("There are column name of x which have no corresponding values in the first column of block (column names of x).")
    }
  }

  return(unique_colnames_x)
} # {check_x_block}


check_block <- function(block) {
  ## check block
  if (!is.matrix(block) & !is.data.frame(block)) {
    stop("The input block is required to be a data.frame or a matrix.")
  }
  if (ncol(block) != 2) {
    stop("The input block is required to have two columns.")
  }
  # Should we really require this!!!  = > for split in test_hierarchy to work
  # => yes
  if (!is.character(block[, 1])) {
    stop("The first column of block (column names of x) is required to be of type character.")
  }
  if (length(unique(block[, 1])) < nrow(block)) {
    stop("The values in the first column of block (column names of x) are not unique.")
  }
  if (!is.numeric(block[, 2]) & !is.character(block[, 2]) # & !is.factor(block[, 2])
      ) {
    stop("The second column of block (the assigned blocks) is required to be a numeric vector or a character vector.")
  }
  if (length(unique(block[, 2])) < 2) {
    stop("The second column of the input block is required to encode at least two blocks.")
  }
  if (any(is.na(block))) {
    stop("There are missing values in the input block.")
  }
  # TODO come up with a number below or remove this because we do not specify
  # anything for d or only x (without any blocks)
  if (any(table(block[, 2]) < 5)) {
    stop("There are less than 5 columns of x for at least one block which is encoded by the second column of block.")
  }

  return(TRUE)
} # {check_block}

check_x_y_clvar <- function(x, y, clvar) {
  ## check y
  if ((!is.list(y) & !is.numeric(y)) | is.data.frame(y)) {
    stop("The response y is required to be a vector or a list of vectors if multiple data sets are present.")
  }
  if (is.list(y)) {
    len_y <- length(y)
    for (i in seq_len(len_y)) {
      if (!is.numeric(y[[i]])) {
        stop("The elements of the list y are required to be numeric vectors.")
      }
      if (is.matrix(y[[i]]) & ifelse(is.null(ncol(y[[i]])), FALSE, ncol(y[[i]]) > 1)) {
        stop("The elements of the list y are required to be numeric vectors or matrices with only one column.")
      }
      if (any(is.na(y[[i]]))) {
        stop("The elements of the list y are required to have no missing values.")
      }
    }
  }
  if (is.numeric(y)) {
    if (is.matrix(y) &  ifelse(is.null(ncol(y)), FALSE, ncol(y) > 1)) {
      stop("The elements of the list y are required to be numeric vectors or matrices with only one column. In the case of only one data set, it is enough that y is a numeric vector or matrix with only one column but it can as well be a list with one element.")
    }
    if (any(is.na(y))) {
      stop("The argument y is required to have no missing values.")
    }
    # if y is numeric, then save y as a list with one element
    y <- list(y)
  }

  # number of responses or number of different data sets
  len_y <- length(y) # y is a list

  ## check x
  if ((!is.list(x) & !is.matrix(x)) | is.data.frame(x)) {
    stop("The input x is required to be a matrix or a list of matrices if multiple data sets are present.")
  }
  if (is.list(x)) {
    len_x <- length(x)
    if (len_y != len_x) {
      stop("The number of responses to be tested is different from the number of matrices which are stored in x.")
    }

    for (i in seq_len(len_x)) {
      if (!is.matrix(x[[i]]) | !is.numeric(x[[i]])) {
        stop("The elements of the list x are required to be matrices of type numeric.")
      }
      if (nrow(x[[i]]) != length(y[[i]])) {
        stop("The length of the response and the corresponding number of rows / observations of the data matrix are different.")
      }
      if (is.null(colnames(x[[i]]))) {
        stop("The matrices which are stored in x are required to have column names. If there is no natural naming convention, then one can set them to some integer, say, 1 to p.")
      }
      if (length(unique(colnames(x[[i]]))) != ncol(x[[i]])) {
        stop("Each of the matrices which are stored in x are required to have unique column names.")
      }
    }
  }
  if (is.matrix(x)) {
    if (len_y != 1) {
      stop("The number of responses to be tested is different from the number of matrices which are stored in x.")
    }
    if (is.null(colnames(x))) {
      stop("The matrix x is required to have column names. If there is no natural naming convention, then one can set them to some integer, say, 1 to p.")
    }
    if (length(unique(colnames(x))) != ncol(x)) {
      stop("The matrix x is required to have unique column names.")
    }
    # if x is matrix, then save x as a list with one element
    x <- list(x)
    }

  ## check clvar
  if ((!is.list(clvar) & !is.matrix(clvar) & !is.null(clvar)) | is.data.frame(clvar)) {
    stop("The input clvar is required to be a matrix or a list of matrices if multiple data sets are present.")
  }
  if (is.list(clvar)) {
    len_clvar <- length(clvar)
    if (len_y != len_clvar) {
      stop("The number of responses to be tested is different from the number of clvar data matrices.")
    }
    for (i in seq_len(len_clvar)) {
      if (!is.matrix(clvar[[i]]) & !is.null(clvar[[i]])) {
        stop("The elements of the list clvar are required to be matrices or NULL if no control covariates are present for a given data set.")
      }
      if (!is.null(clvar[[i]])) {
        if (nrow(clvar[[i]]) != length(y[[i]])) {
          stop("The length of the response and the corresponding number of rows of the control covariates data matrix are not the same.")
        }
        if (any(is.na(clvar[[i]]))) {
          stop("The elements of the list clvar are required to have no missing values.")
        }
      }
    }
  }
  if (is.matrix(clvar)) {
    if (len_y != 1) {
      stop("The number of responses to be tested is different from the number of control covariates data matrices.")
    }
    if (any(is.na(clvar))) {
      stop("The argument clvar is required to have no missing values.")
    }
    # if clvar is a matrix, then save clvar as a list with one element
    clvar <- list(clvar)
  }
  if (is.null(clvar)) {
    clvar <- rep(list(NULL),  len_y)
  }
  return(list(x = x, y = y, clvar = clvar))
} # {check_x_y_clvar}
