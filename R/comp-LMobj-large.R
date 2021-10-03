# Compute model output for the large design matrix
#
# Compute model output for the large design matrix, i.e. based all the
# selected variables per split and per data set
#
# @return list of lm/glm objects for the large design matrix
compMOD_large <- function(compMOD_same, x, y, clvar, arg.all, arg.all.fix) {

  MODobj_data <- mapply(compMOD_same, x = x, y = y, clvar = clvar,
                        arg.all = arg.all,
                        MoreArgs = list(arg.all.fix), # fixed arguments
                        SIMPLIFY = FALSE)

  return(MODobj_data)
} # {compMOD_large}






# Create skeleton of model output for the small design matrix
#
# Create skeleton of model output for the small design matrix,
# i.e. we fill it with NULL:
#
# @return list of NULL for the small design matrix
compMOD_small <- function(compMOD_changing, x, y, clvar, arg.all, arg.all.fix) {

  MODobj_data <- mapply(compMOD_changing, x = x, y = y, clvar = clvar,
                        arg.all = arg.all,
                        MoreArgs = list(arg.all.fix), # fixed arguments
                        SIMPLIFY = FALSE)

  return(MODobj_data)
} # {compMOD_large_S}

