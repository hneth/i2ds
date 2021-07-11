## trans_arrays.R | i2ds
## hn | uni.kn | 2021 07 11
## ---------------------------

# Functions for transforming/manipulating arrays. 

# add_dimnames: Add default names to array dimensions: ------ 

#' Add dimension names (to arrays). 
#' 
#' \code{add_dimnames} adds (or re-assigns) dimension names to arrays 
#' (or data frames or atomic vectors, but not other lists). 
#' 
#' When \code{x} is an atomic vector, 
#' \code{add_dimnames} returns a named vector. 
#' 
#' @return Input \code{x} with dimension names. 
#' 
#' @param x An array to which dimension names are to be added.
#' 
#' @param prefix A vector of prefixes (as character, 
#' length must match the number of dimensions of \code{x}). 
#' Default: \code{prefix = c("r", "c", "t")} (for row, column, and table).
#' 
#' @param sep Separator (as character) between prefix and numeric indices. 
#' Default: \code{sep = ""}. 
#' 
#' @examples 
#' a <- array(1:24, dim = c(3, 4, 2))
#' add_dimnames(a)
#' add_dimnames(a, prefix = c("row", "col", "table"), sep = "_")
#' 
#' # More dimensions:
#' b <- array(1:2^5, dim = rep(2, 5))
#' add_dimnames(b)
#' 
#' # For data frames:
#' c <- data.frame(let = letters[1:4],
#'                 num = 5:8)
#' is.list(c)
#' is.data.frame(c)
#' add_dimnames(c, prefix = c("r", "c"))
#' add_dimnames(c)
#' 
#' # For atomic vectors:
#' d <- 1:4
#' add_dimnames(d, prefix = "nr")  # adding named elements
#' add_dimnames(d, prefix = letters[1:4], sep = "_")
#' 
#' # For scalars:
#' add_dimnames(NA)
#' 
#' # For lists (that are NOT data frames):
#' l <- list(1:2, letters[1:3])
#' add_dimnames(l)  # return NA
#' 
#' @family array functions
#' 
#' @export 

add_dimnames <- function(x, prefix = c("r", "c", "t"), sep = ""){
  
  dims <- dim(x)
  
  if ( (is.list(x)) & (!is.data.frame(x)) ){ # (A) x is a list, but NO data frame: 
    
    message("add_dimnames: x is a list, but must be an array, a data frame, or a vector.")
    
    return(NA)
    
  } else if ( (is.null(dims)) & (is.atomic(x)) ){ # (B) x is an atomic vector:
    
    elm_nr <- length(x)  # nr of elements
    
    if ( (length(prefix) != 1) & (length(prefix) != elm_nr) ){
      
      message("add_dimnames: Length of prefix must match x. Using 'n':")
      prefix <- "n"
      
    }
    
    # Create dim_names (as a vector): 
    dim_names <- paste(prefix, 1:elm_nr, sep = sep) 
    
    # Assign dim_names to x:
    names(x) <- dim_names
    
  } else { # (C) x is a data frame or an array:
    
    dim_nr <- length(dims)  # nr of dimensions
    
    if (length(prefix) != dim_nr){
      
      message("add_dimnames: Length of prefix must match x dimensions. Using letters:")
      prefix <- letters[1:dim_nr]
      
    }
    
    # Create dim_names (as a list): 
    dim_names <- vector("list", dim_nr)  # an empty list
    
    for (i in 1:dim_nr){ # fill list:
      
      dim_names[[i]] <- paste(prefix[i], 1:dims[i], sep = sep)
      
    }
    
    # Assign dim_names to x:
    dimnames(x) <- dim_names
    
  } # else.
  
  return(x)
  
} # add_dimnames(). 

## Check:
# a <- array(1:24, dim = c(3, 4, 2))
# add_dimnames(a)
# add_dimnames(a, prefix = c("row", "col", "table"), sep = "_")
# 
# # More dimensions:
# b <- array(1:2^5, dim = rep(2, 5))
# add_dimnames(b)
# 
# # For data frames:
# c <- data.frame(let = letters[1:4],
#                 num = 5:8)
# is.list(c)
# is.data.frame(c)
# add_dimnames(c, prefix = c("r", "c"))
# add_dimnames(c)
# 
# # For atomic vectors:
# d <- 1:4
# add_dimnames(d, prefix = "nr")  # adding named elements
# add_dimnames(d, prefix = letters[1:4], sep = "_")
# 
# # For scalars:
# add_dimnames(NA)
# 
# # For lists (that are NOT data frames):
# l <- list(1:2, letters[1:3])
# add_dimnames(l)  # return NA


# flatten_array: Turn a 3D array into a 2D data frame: ------ 

#' Flatten a 3D array into a 2D data frame. 
#' 
#' \code{flatten_array} turns a 3-dimensional array 
#' into a 2-dimensional data frame.
#' 
#' \code{flatten_array} assumes that \code{x} is a 3-dimensional array 
#' with dimension names (and calls \code{\link{add_dimnames}} instead). 
#' 
#' Internally, \code{flatten_array} uses \code{apply} to apply 
#' the \code{\link{c}} function to a specified \code{margin} of \code{x}. 
#' It aims to reconstruct the names of the collapsed variables 
#' from the initial letters of the dimension names. 
#' 
#' @return A data frame. 
#' 
#' @param x A 3-dimensional array. 
#' 
#' @param margin The margin across which \code{\link{c}} is to be applied. 
#' Default: \code{margin = 2} (i.e., columns). 
#' 
#' @param varsAsFactors Boolean: Should reconstructed variables be factors? 
#' Default: \code{varsAsFactors = FALSE} (i.e., as character variables). 
#' 
#' @importFrom tidyr expand_grid 
#' 
#' @examples  
#' a1 <- array(data = 1:8, dim = c(2, 2, 2), 
#'             dimnames = list(c("r1", "r2"), c("c1", "c2"), c("t1", "t2")))
#' 
#' flatten_array(a1)  # using default (margin = 2) 
#' 
#' # Returning name variables as factors:
#' a1f <- flatten_array(a1, varsAsFactors = TRUE)
#' is.factor(a1f$r)
#' is.factor(a1f$t)
#' 
#' a2 <- array(data = 1:60, dim = c(5, 4, 3))  # no dimnames 
#' flatten_array(a2)  # default names added
#' flatten_array(a2, margin = 1)   
#' flatten_array(a2, margin = 3)
#' 
#' a3 <- array(data = 1:2^4, dim = c(2, 2, 2, 2))  # 4-dimensions
#' flatten_array(a3)
#' 
#' @family array functions
#' 
#' @seealso
#' \code{\link{add_dimnames}} for adding dimension names to arrays.  
#' 
#' @export

flatten_array <- function(x, margin = 2, varsAsFactors = FALSE){
  
  # Check input:
  if ( (!is.array(x)) | (length(dim(x)) != 3) ){
    
    message("flatten_array: x must be a 3-dimensional array.")
    return(NA)
    
  }
  
  if ( (!is.numeric(margin)) | (margin < 1) | (margin > 3) ){
    
    message("flatten_array: margin must be a number from 1 to 3.")
    return(NA)
    
  }
  
  # Adding dimnames (if none present): 
  if (is.null(dimnames(x))){
    
    message("flatten_array: Using add_dimnames() to add dimnames to x.")
    x <- add_dimnames(x)
    
  }
  
  # Main: 
  # Flatten array:
  flat <- apply(x, MARGIN = margin, FUN = c)
  
  # Reconstruct the names of row variables:
  vars <- tidyr::expand_grid(expand.grid(dimnames(x)[c(-margin)], stringsAsFactors = varsAsFactors))
  
  # Add row names to the flat table:
  df <- data.frame(vars, flat)
  
  # Adjust names (of 2 initial columns/variables): 
  # if (varsAsFactors){  # get var names from factor levels:
  #   n_1 <- substr(levels(df$Var1)[1], 1, 1)
  #   n_2 <- substr(levels(df$Var2)[1], 1, 1)
  # } else { # get var names from dimnames of array x: 
  n_1 <- substr(dimnames(x)[-margin][[1]][1], 1, 1)  # 1st letter of dim/var 1
  n_2 <- substr(dimnames(x)[-margin][[2]][1], 1, 1)  # 1st letter of dim/var 2
  # }
  
  names(df)[1:2] <- c(n_1, n_2)
  
  return(df)
  
} # flatten_array(). 

## Check: 
# a1 <- array(data = 1:8, dim = c(2, 2, 2), 
#             dimnames = list(c("r1", "r2"), c("c1", "c2"), c("t1", "t2")))
# 
# flatten_array(a1)  # using default (margin = 2) 
# 
# # Returning name variables as factors:
# a1f <- flatten_array(a1, varsAsFactors = TRUE)
# is.factor(a1f$r)
# is.factor(a1f$t)
# 
# a2 <- array(data = 1:60, dim = c(5, 4, 3))  # no dimnames 
# flatten_array(a2)  # default names added
# flatten_array(a2, margin = 1)   
# flatten_array(a2, margin = 3)
# 
# a3 <- array(data = 1:2^4, dim = c(2, 2, 2, 2))  # 4-dimensions
# flatten_array(a3)



## ToDo: ------
## - ...

## eof. ----------
