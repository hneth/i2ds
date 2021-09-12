## util.R | i2ds
## hn | uni.kn | 2021 09 12

# General utility functions: ------ 

## swap_xy: Swap the first 2 dimensions of an array/elements of a vector/column of a data.frame: ------

# A generalized version of t() that works for multiple object types.

swap_xy <- function(obj){
  
  t_obj <- NA  # initialize
  
  if (is.array(obj)) { # 1: obj is an array/matrix/table: ---- 
    
    # Swap the first 2 dimensions of an array (i.e., X and Y):
    n_dim <- length(dim(obj))  
    
    if (n_dim > 1){
      
      if (n_dim > 2){ ix_rest <- 3:n_dim } else { ix_rest <- NULL }
      
      t_obj <- aperm(obj, perm = c(2, 1, ix_rest))  # swap dimensions 1 and 2
      
    } else {
      
      t_obj <- obj
      
    }
    
  } else if (is.vector(obj)) { # 2: obj is vector or list: ---- 
    
    # Swap the first 2 elements (i.e., X and Y):
    x_len <- length(obj)  
    
    if (x_len > 1){
      
      if (x_len > 2){ ix_rest <- 3:x_len } else { ix_rest <- NULL }
      
      t_obj <- obj[c(2, 1, ix_rest)]  # swap elements 1 and 2
      
    } else {
      
      t_obj <- obj      
      
    }
    
  } else if (is.data.frame(obj)) { # 3: obj is a data.frame: ---- 
    
    # Swap the first 2 columns:
    n_col <- ncol(obj)
    
    if (n_col > 1){
      
      if (n_col > 2){ ix_rest <- 3:n_col } else { ix_rest <- NULL }
      
      t_obj <- obj[ , c(2, 1, ix_rest)]  # swap columns 1 and 2
      
    } else {
      
      t_obj <- obj      
      
    }
    
  }
  
  return(t_obj)
  
} # swap_xy().

# # Check: 
# (v  <- 1:4)
# (m <- matrix(1:6, nrow = 3))
# (l  <- list(a = 1:3, b = letters[1:4]))
# (ar <- array(1:8, dim = c(4,2)))
# (df <- data.frame(v1 = v, v2 = LETTERS[1:4], v3 = 11:14))
#   
# swap_xy(v)
# swap_xy(m)  # same as t(m)
# swap_xy(l)
# swap_xy(ar)
# swap_xy(df)



## get_name: A function to get an object's name (inside a function): ------ 

# vec <- 1:10
# deparse(substitute(vec))

get_name <- function(x){
  
  nm <- NA
  
  nm <- deparse(substitute(x))
  
  return(nm)
  
} # get_name

# # Check:
# get_name(group)
# (ls <- list(group = group, sex = sex))
# (df <- data.frame(group = group, sex = sex))
# get_name(ls)
# get_name(df)




## ToDo: ------

## eof. ----------
