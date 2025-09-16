## util.R | i2ds
## hn | uni.kn | 2023 12 23

# General utility functions: ------ 


## swap_xy_dims: Swap 2 dimensions x and y of an array/table OR elements of a vector/list/columns of a data.frame: ------

# Goal: A generalized version of t() that works for multiple object types.

swap_xy_dims <- function(obj, x = 1, y = 2){
  
  # Inputs: Ensure that x and y are integer values: 
  if ( (x %% 1 != 0) || (y %% 1 != 0) ) {
    
    message("swap_xy_dims: x and y must be integer values.")
    x <- as.integer(x)
    y <- as.integer(y)
    
  }
  
  t_obj <- NA  # initialize
  
  
  if (is.array(obj)) { # Case 1: obj is an array/matrix/table: ---- 
    
    # print("1: An array, matrix, or table: Transpose dimensions")  # 4debugging
    
    n_dim <- length(dim(obj))  
    
    if ( (x <= n_dim) && (y <= n_dim) && (x != y) ){
      
      ix    <- 1:n_dim
      ix[x] <- y
      ix[y] <- x
      
      t_obj <- aperm(obj, perm = ix)  # permute dimensions x and y
      
    } else { # no change:
      
      message("swap_xy_dims: no change.")
      t_obj <- obj
      
    }
    
    
  } else if ( is.atomic(obj) | is.list(obj) ) { # Case 2: obj is atomic vector/list/df: ---- 
    
    # print("2: An atomic vector, list, or data frame: Swap elements")  # 4debugging
    
    n <- length(obj)
    
    if ( (x <= n) && (y <= n) && (x != y) ){
      
      ix    <- 1:n
      ix[x] <- y
      ix[y] <- x
      
      t_obj <- obj[ix]  # switch elements x and y
      
      
    } else { # Trivial case 0: no change:
      
      message("swap_xy_dims: no change.")      
      t_obj <- obj      
      
    }
    
    
  } else { # any other obj: 
    
    message("swap_xy_dims: obj is not an array/table or linear vector/list.")
    
  }
  
  
  return(t_obj)
  
} # swap_xy_dims().

# # Check:
# (v  <- 1:4)
# (m <- matrix(1:6, nrow = 3))
# (l  <- list(a = 1:3, b = letters[1:2], c = 11:14))
# (df <- data.frame(v1 = v, v2 = letters[1:4], v3 = 11:14))
# (ar <- array(1:24, dim = c(4, 3, 2)))
# (tb <- UCBAdmissions)
# 
# swap_xy_dims(v)
# swap_xy_dims(v, 1, 4)
# swap_xy_dims(v, 2.2, pi)  # as integers
# swap_xy_dims(v, 1, 9)     # no change
# 
# swap_xy_dims(l)
# 
# swap_xy_dims(l, 2, pi)
# swap_xy_dims(l, 1, 9)     # no change
# 
# swap_xy_dims(df) # df is list
# 
# swap_xy_dims(m)  # same as t(m)
# 
# swap_xy_dims(ar)
# swap_xy_dims(ar, 2, 3)
# 
# swap_xy_dims(tb)
# swap_xy_dims(tb, 2, 3)
# swap_xy_dims(tb, 1, pi)
# 
# swap_xy_dims(factor(v)) # factor



## get_arg: Get raw argument(s) of a function (from inside a function) ------

get_arg <- function(x){
  
  base::substitute(x)
  
  # rlang::enexpr(x)
  
} # get_arg().

# # Check:
# get_arg(x = "abc")
# get_arg(x = list("abc", 123, TRUE))
# get_arg(x = list(a, b, c))
# as.character(get_arg(x = list(a, b, c)))[-1]



## get_name: Get an object's name (from inside a function): ------ 

# vec <- 1:10
# deparse(substitute(vec))

get_name <- function(x){
  
  nm <- NA
  
  nm <- deparse(substitute(x))
  
  return(nm)
  
} # get_name(). 

# # Check:
# (v  <- 1:4)
# (m <- matrix(1:6, nrow = 3))
# (l  <- list(a = 1:3, b = letters[1:2], c = 11:14))
# (df <- data.frame(v1 = v, v2 = letters[1:4], v3 = 11:14))
# (ar <- array(1:24, dim = c(4, 3, 2)))
# (tb <- UCBAdmissions)
# 
# get_name(v)
# get_name(m)
# get_name(df)
# get_name(ar)
# get_name(tb)








## ToDo: ------

# 1. Verify that some object contains only freq counts (i.e., integers >= 0).
#    (see is_freq() in riskyr and i2ds).


## eof. ----------
