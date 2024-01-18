## array_fun.R | i2ds
## hn | uni.kn | 2023 05 24

# Functions for creating and manipulating (transforming: reducing and reshaping) arrays, matrices, and tables: ------ 

# Notes on arrays and tables: 

# 1. An R matrix is (the special case of) a 2-dimensional array:
# (m2 <- matrix(1:9, nrow = 3))
# is.array(m2)

# 2. A 1-dimensional array is NOT a vector:
# m1 <- matrix(1:9)
# is.matrix(m1)
# as.vector(m1)

# 3. R objects of type "table" are specific cases of arrays (contingency tables with freq. counts).


# ## Background: Computing margins and proportions of matrices/tables
# (m <- matrix(1:4, 2))
# aperm(m)
# 
# margin.table(m)
# margin.table(m, 1)
# margin.table(m, 2)
# 
# addmargins(m)
# addmargins(m, margin = c(1, 2), FUN = c(min, max))
# 
# prop.table(m)     # cell percentages
# prop.table(m, 1)  # row percentages
# prop.table(m, 2)  # col percentages


## flip_mx: Arrange order of dimensions and/or reverse order within dimension(s): ------ 

flip_mx <- function(x, perm = NULL, dim_swap = NA){
  
  cur_x <- x
  
  # (1) Swap the order of dimensions in dim_swap:
  
  # +++ here now +++ : See flip() in MML package!
  
  for (i in 1:length(dim_swap)){
    
    cur_dim <- dim_swap[i]
    
    dim_vector <- dimnames(cur_x)[cur_dim]
    dim_levels <- dimnames(cur_x)[[cur_dim]]
    
    factor(dim_vector, levels = rev(dim_levels))
    
  }
  
  # (2) Permute dimensions:
  cur_x <- aperm(a = cur_x, perm = perm)  # pass perm to aperm() 
  
  # Output: 
  return(cur_x)
  
} # flip_mx(). 

# # Check:
# all.equal(UCBAdmissions, UCBAdmissions[1:2, 1:2, 1:6])
# 
# flip_mx(UCBAdmissions)
# flip_mx(UCBAdmissions)


## prop_mx: A version of prop.table() with a special case of margin = 3 for 2x2 matrices: ------

# (Based on the trans() function of the MLM package.) 

prop_mx <- function(x, margin = NULL){
  
  if (!is.null(margin) && (length(margin) == 1) && (margin == 3)){ # A: special case: margin = 3: 
    
    if ( (length(dim(x) == 2)) && (unique(dim(x)) == 2) ){ # x is 2x2 matrix:
      
      # print("compute diagonal proportions")
      
      # 4 essential frequency values:
      acbd <- as.vector(x)  # values in by-COL direction!
      
      a <- acbd[1]
      c <- acbd[2]  # x[2, 1]
      b <- acbd[3]  # x[1, 2]
      d <- acbd[4]
      
      sum_ad <- (a + d)
      sum_bc <- (b + c)
      
      out <- x  # re-initialize out
      
      # (a) Change all 4 cell values:
      out[1, 1] <- a/sum_ad
      out[1, 2] <- b/sum_bc
      out[2, 1] <- c/sum_bc
      out[2, 2] <- d/sum_ad
      
      return(out)
      
    } else { # not a 2x2 matrix:
      
      return("prop_mx: margin = 3 is currently only defined for 2x2 matrices.")
      
      # Note: Could be defined for n-dimensional _binary_ grids.
      
    }
    
  } else {  # B: default case: pass to prop.table()
    
    return(base::prop.table(x = x, margin = margin))
    
  }
  
} # prop_mx().

# # Check:
# m1 <- matrix(1:4)
# m2 <- matrix(1:4, nrow = 2)
# 
# prop_mx(m1)
# prop_mx(m1, margin = 3)
# 
# prop_mx(m2)
# prop_mx(m2, margin = 1)
# prop_mx(m2, margin = 2)
# prop_mx(m2, margin = 3)


## add_dimnames: Add default names to array dimensions: ------ 

#' Add dimension names (to arrays). 
#' 
#' \code{add_dimnames} adds (or re-assigns) dimension names to arrays. 
#' 
#' Although \code{add_dimnames} is intended for arrays, 
#' it also works for atomic vectors (yielding a named vector) 
#' and data frames (yielding new row and column names) 
#' but not other lists. 
#' 
#' As R objects of class \code{\link{matrix}}, \code{\link{table}}, 
#' and \code{\link{xtabs}} are also arrays \code{add_dimnames} works for them as well. 
#' 
#' See \code{\link{provideDimnames}} (from \strong{base} R) 
#' for a related function. 
#' 
#' @return Input \code{x} with dimension names. 
#' 
#' @param x An array to which dimension names are to be added.
#' 
#' @param dnames A character vector of dimension names. 
#' If \code{length(dnames == 1)}, a numeric digit is added to 
#' signal the dimension number. 
#' If \code{length(dnames)} exceeds the number of dimensions, 
#' it is truncated. 
#' If the number of dimensions exceeds \code{length(dnames)}, 
#' \code{dnames} are created from \code{LETTERS}. 
#' Default: \code{dnames = c("row", "col", "tab")}.   
#'        
#' @param prefix A character vector of prefixes (whose  
#' length must match the number of dimensions of \code{x}). 
#' Default: \code{prefix = c("r", "c", "t")} (for row, column, and table).
#' 
#' @param sep A separator (as character) between prefix and numeric indices. 
#' Default: \code{sep = ""}. 
#' 
#' @examples 
#' a <- array(1:24, dim = c(3, 4, 2))
#' add_dimnames(a)
#' add_dimnames(a, dnames = "dim", sep = "_")
#' add_dimnames(a, dnames = c("X", "Y", "Layer", "XYZ"), prefix = c("x", "y", "l"), sep = "_")
#' 
#' # More dimensions:
#' b <- array(1:2^5, dim = rep(2, 5))
#' add_dimnames(b)
#' add_dimnames(b, dnames = "D")
#' add_dimnames(b, dnames = "dim", prefix = c("r", "c", "l", "m", "n"), sep = "_")
#' 
#' # For a table:
#' add_dimnames(UCBAdmissions)
#' 
#' # For data frames:
#' c <- data.frame(let = letters[1:4], num = 5:8)
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
#' @seealso
#' \code{\link{provideDimnames}} is a more basic version. 
#' 
#' @export 

add_dimnames <- function(x, dnames = c("row", "col", "tab"), prefix = c("r", "c", "t"), sep = ""){
  
  dims <- dim(x)
  
  if ( (is.list(x)) & (!is.data.frame(x)) ){ # (A) x is a list, but NO data frame: 
    
    message("add_dimnames: x is a list, but must be an array, a vector, or a data.frame.")
    
    return(NA)
    
  } else if ( (is.null(dims)) & (is.atomic(x)) ){ # (B) x is an atomic vector:
    
    elm_nr <- length(x)  # nr of elements
    
    if ( (length(prefix) != 1) & (length(prefix) != elm_nr) ){
      
      message("add_dimnames: Length of prefix must match x. Using 'n':")
      prefix <- "n"
      
    }
    
    # Create name_vec: 
    name_vec <- paste(prefix, 1:elm_nr, sep = sep) 
    
    # Assign name_vec to x (as a vector):
    names(x) <- name_vec
    
  } else { # (C) x is a data frame or an array:
    
    dim_nr <- length(dims)  # nr of dimensions
    
    
    # Check/fix dnames length: 
    if (length(dnames) == 1){ # atomic dnames:
      
      dnames <- paste(dnames, 1:dim_nr, sep = sep)  # add dim number to dnames
      
    } else if (length(dnames) > dim_nr){  # too many dnames for dim_nr: 
      
      dnames <- dnames[1:dim_nr]  # truncate dnames
      
    } else if (length(dnames) != dim_nr){ # dim_nr exceeds dnames length: 
      
      message("add_dimnames: Length of dnames must match x dimensions. Using LETTERS:")
      dnames <- LETTERS[1:dim_nr]
      
    }
    
    # Check/fix prefix length: 
    if (length(prefix) > dim_nr){
      
      prefix <- prefix[1:dim_nr]  # truncate prefix
      
    } else if (length(prefix) != dim_nr){ # dim_nr exceeds prefix length: 
      
      message("add_dimnames: Length of prefix must match x dimensions. Using letters:")
      prefix <- letters[1:dim_nr]
      
    }
    
    # Create name_list (as list): 
    name_list <- vector("list", dim_nr)  # an empty list
    
    for (i in 1:dim_nr){ # list elements:
      
      name_list[[i]] <- paste(prefix[i], 1:dims[i], sep = sep)
      
    }
    
    # Add dnames:
    names(name_list) <- dnames 
    
    # Assign name_list to x:
    dimnames(x) <- name_list
    
  } # else.
  
  return(x)
  
} # add_dimnames(). 

## Check:
# a <- array(1:24, dim = c(4, 3, 2))
# a <- add_dimnames(a)
# add_dimnames(a, prefix = c("row", "col", "tab"), sep = "_")
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



## flatten_array: Turn a 3D array into a 2D data frame: ------ 

#' Flatten a 3D array into a 2D data frame. 
#' 
#' \code{flatten_array} turns a 3-dimensional array   
#' into a 2-dimensional data frame (in wide format).
#' 
#' \code{flatten_array} assumes that \code{x} is a 3-dimensional 
#' \code{\link{array}} with dimension names 
#' (and calls \code{\link{add_dimnames}} if they are absent). 
#' 
#' The \code{margin} argument specifies an array dimension 
#' to be passed to \code{apply(x, MARGIN = margin, FUN = c)}. 
#' 
#' \code{flatten_array} returns \code{NA} for non-arrays and  
#' for arrays with more than 3 dimensions. 
#' 
#' As R objects of class \code{\link{table}} are also arrays 
#' (contingency tables of frequency counts, in numeric mode) 
#' \code{flatten_array} works for them as well. 
#' 
#' To flatten numeric arrays or objects of class \code{\link{table}}  
#' with more then 3 dimensions, use \code{\link{margin.table}} (from \strong{base} R) 
#' to create a 3-dimensional aggregate first. 
#' 
#' Internally, \code{flatten_array} uses \code{\link{apply}} to apply 
#' the \code{\link{c}} function to a specified \code{margin} of \code{x}. 
#' It aims to reconstruct the names of the collapsed variables 
#' from the initial letters of the dimension names. 
#' 
#' See \code{\link{ftable}} (from the \strong{stats} package) 
#' for a more general function (in combination with \code{\link{aperm}})  
#' and \code{\link{margin.table}} (from \strong{base} R) 
#' and \code{\link{addmargins}} (from \strong{stats}) for aggregating 
#' over table dimensions.
#' 
#' @return A data frame. 
#' 
#' @param x A 3-dimensional array. 
#' 
#' @param margin The margin across which \code{\link{c}} is to be applied. 
#' Default: \code{margin = 2} (i.e., columns). 
#' 
#' @param varsAsFactors Boolean: Should reconstructed variables be factors? 
#' Default: \code{varsAsFactors = FALSE} (i.e., character variables). 
#' 
#' @importFrom tidyr expand_grid 
#' 
#' @examples
#' a1 <- array(data = LETTERS[1:8], dim = c(2, 2, 2), 
#'             dimnames = list(c("r1", "r2"), c("c1", "c2"), c("t1", "t2")))
#' flatten_array(a1)  # using default (margin = 2) 
#' 
#' # Using names of dimnames:
#' names(dimnames(a1)) <- c("row", "col", "tab")
#' flatten_array(a1)
#' flatten_array(a1, margin = 3)  
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
#' flatten_array(margin.table(a3, margin = 1:3))
#' 
#' # For table:
#' # UCBAdmissions data (3-dimensions): 
#' flatten_array(UCBAdmissions) # margin = 2
#' flatten_array(UCBAdmissions, margin = 1)
#' flatten_array(UCBAdmissions, margin = 3)
#' 
#' # Titanic data (4-dimensions):
#' T3d <- margin.table(Titanic, margin = c(2, 3, 4))  # aggregate 3d-array
#' flatten_array(T3d, margin = 3)  # compare to ftable(T3d)
#' 
#' @family array functions
#' 
#' @seealso
#' \code{\link{ftable}} for flattening multi-dimensional arrays; 
#' \code{\link{margin.table}} for aggregating across array dimensions; 
#' \code{\link{addmargins}} for expanding factor levels on margins; 
#' \code{\link{aperm}} for permuting array dimensions; 
#' \code{\link{add_dimnames}} for adding dimension names to arrays.  
#' 
#' @export

flatten_array <- function(x, margin = 2, varsAsFactors = FALSE){
  
  # Check inputs:
  if ( (!is.array(x)) | (length(dim(x)) != 3) ){
    
    message("flatten_array: x must be a 3-dimensional array.")
    return(NA)
    
  }
  
  # Get numeric margin:
  if (is.character(margin)){
    margin <- which(names(dimnames(x)) == margin)
  } else {
    margin <- as.numeric(margin)  # use number
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
  
  # Store dimension names:
  dim_names_x <- names(dimnames(x))
  
  # Main: 
  # Flatten array:
  flat <- apply(x, MARGIN = margin, FUN = c)
  
  # Reconstruct the names of row variables:
  vars <- tidyr::expand_grid(expand.grid(dimnames(x)[c(-margin)], stringsAsFactors = varsAsFactors))
  
  # Add row names to the flat table:
  df <- data.frame(vars, flat)
  
  # Adjust names (of 2 initial columns/variables): 
  
  if (is.null(dim_names_x)) { # dimensions are not named:
    
    # if (varsAsFactors){  # get var names from factor levels:
    # 
    #   n_1 <- substr(levels(df$Var1)[1], 1, 1)
    #   n_2 <- substr(levels(df$Var2)[1], 1, 1)
    #   
    # } else { # create var names from name initials of array x: 
    
    n_1 <- substr(dimnames(x)[-margin][[1]][1], 1, 1)  # 1st letter of dim/var 1
    n_2 <- substr(dimnames(x)[-margin][[2]][1], 1, 1)  # 1st letter of dim/var 2
    
    # }
    
  } else {  # use existing dimension names:
    
    n_1 <- dim_names_x[-margin][1]  # 1st of the remaining names
    n_2 <- dim_names_x[-margin][2]  # 2nd of the remaining names
    
  }
  
  # Name 2 initial variables:
  names(df)[1:2] <- c(n_1, n_2)
  
  return(df)
  
} # flatten_array(). 

## Check: 
# flatten_array(UCBAdmissions)
# flatten_array(UCBAdmissions, margin = 1)
# flatten_array(UCBAdmissions, margin = 3)
# 
# a1 <- array(data = 1:8, dim = c(2, 2, 2), 
#             dimnames = list(c("r1", "r2"), c("c1", "c2"), c("t1", "t2")))
# 
# flatten_array(a1)  # using default (margin = 2) 
# 
# # Using names of dimnames:
# a1 <- array(data = 1:8, dim = c(2, 2, 2), 
#             dimnames = list(c("r1", "r2"), c("c1", "c2"), c("t1", "t2")))
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

## Compare with margin.table() and ftable(): 

# dim(Titanic)
# dimnames(Titanic)
# (T2 <- margin.table(Titanic, margin = c(2, 3, 4)))  # aggregate over dimension 1 (Class)
# 
# flatten_array(T2, margin = 3)
# 
# # Compare: 
# ftable(T2)
# data.frame(ftable(T2)) # same as:
# data.frame(T2)



## expand_freq_table: Turn a contingency table into a data frame of raw cases: ------ 

#' Expand a contingency table (as array/table or data frame) to raw cases (as data frame). 
#' 
#' \code{expand_freq_table} turns a contingency table 
#' (given as an array/table or as a data frame with a frequency count variable 
#' \code{freq_var} that denotes the number of cases for each factor combination) 
#' into a data frame of raw cases. 
#' 
#' \code{expand_freq_table} assumes that \code{x} is a \code{\link{table}} 
#' or a \code{\link{data.frame}} with a frequency count variable \code{freq_var}. 
#' 
#' If \code{x} is a \code{\link{table}}, \code{expand_freq_table} first uses 
#' \code{\link{as.data.frame}} (with \code{responseName = freq_var}) 
#' to turn \code{x} into a contingency table (as a data frame with 
#' a frequency variable, named \code{"Freq"} by default). 
#' 
#' The function allows turning a contingency table 
#' (i.e., a table that cross-classifies frequency counts) ---  
#' in the form of a \code{\link{table}} or a \code{\link{data.frame}} 
#' with a frequency count variable \code{freq_var} --- 
#' into a corresponding \code{\link{data.frame}} of raw cases. 
#' The number of cases or observations (rows) in the resulting data frame 
#' corresponds to \code{sum(x)} (for tables) or 
#' \code{sum(x$freq_var)} (for data frames). 
#' 
#' @return A data frame (of raw cases). 
#' 
#' @param x An contingency table (as \code{\link{table}} or \code{\link{data.frame}}). 
#' 
#' @param freq_var The name of the frequency count variable in 
#' a data frame \code{x} (aka. \code{responseName} in \code{\link{as.data.frame}}). 
#' Default: \code{freq_var = "Freq"},  
#' based on default of \code{as.data.frame(x, responseName = "Freq")} for arrays/tables. 
#' 
#' @param row_name_repair Boolean: Should rows be repaired (i.e., enumerated)? 
#' Default: \code{row_name_repair = TRUE}. 
#' 
#' @examples
#' # (a) from raw data (vectors):
#' ans <- sample(c("yes", "no", "maybe"), 100, replace = TRUE)
#' eat <- sample(c("fish", "meat", "veggie"), 100, replace = TRUE)
#' df_1 <- data.frame(ans, eat)  # data frame from vectors
#' df_2 <- expand_freq_table(data.frame(table(ans, eat))) # table > contingency table > df
#' all.equal(table(df_1), table(df_2))
#' 
#' # (b) from a table (3D table > contingency table > data frame > 3D array):
#' df <- expand_freq_table(UCBAdmissions)  # array/table > contingency table > df
#' tb <- table(df)                         # df > array/table 
#' all.equal(UCBAdmissions, tb)
#' 
#' # Trivial case:
#' expand_freq_table(data.frame(x = "a", Freq = 2))
#' 
#' # Full circle (4D array > contingency table > data frame > 4D array): 
#' df <- expand_freq_table(Titanic)
#' tb <- table(df)
#' all.equal(Titanic, tb)
#' 
#' @family array functions
#' 
#' @seealso
#' \code{\link{table}} and \code{\link{xtabs}} for turning data frames into contingency tables;  
#' \code{\link{as.data.frame}} for turning an array/table into a contingency table (as df). 
#' 
#' @export

expand_freq_table <- function(x, freq_var = "Freq", row_name_repair = TRUE){
  
  # 0. Initialize:
  df_out   <- NA
  freqs    <- NA
  ix_freqs <- NA
  
  # 1. Turn a table (a numeric array/contingency table with frequency counts) into a contingency table (as df): 
  if (is.table(x)){
    
    x <- as.data.frame(x, responseName = freq_var)  # Note: x is a contingency table (table > df)
    # message("expand_freq_table: Converted x from a table to a data frame.")
    # print(head(x))  # 4debugging
    
  }
  
  # 2. Process df: 
  if (!is.data.frame(x)){
    
    message("expand_freq_table: x must be a contingency table (as data frame or table).")
    return(NA)
    
  } else { # x is df: 
    
    # 3. Check frequency count variable:
    
    freqs <- x[[freq_var]]  # freq of cases per combination (as vector)
    # print(table(freqs))  # 4debugging
    
    if ( (!is.numeric(freqs)) | (any(freqs < 0)) | (any(is.na(as.integer(freqs)))) | (any((freqs %% 1) > 0)) ){
      
      message("expand_freq_table: freq_var must be an existing numeric variable and only contain non-negative integers.")
      return(NA)
      
    } else { # Get index of frequency count variable: 
      
      if (is.character(freq_var)){      
        
        ix_freqs <- which(names(x) == freq_var)
        
      } else if ( (is.numeric(freq_var)) & (!is.na(as.integer(freq_var))) ) {
        
        ix_freqs <- freq_var 
        
      } else {
        
        message("expand_freq_table: freq_var is not a name or numeric index of df.")
        return(NA)
        
      }
      # print(ix_freqs)  # 4debugging 
    }
    
    # 4. Remove freq count variable from df:
    df_rest <- x[ , -ix_freqs]
    # print(df_rest)  # 4debugging 
    
    # 5. Trivial case: If df_rest is only a vector: 
    if (!is.data.frame(df_rest)){
      
      df_rest <- data.frame(df_rest)  # make data frame
      # print(df_rest)  # 4debugging 
      
    }
    
    # Main part: ---- 
    
    # 6. Index of how often each row of df_rest is to be repeated:
    ix_repeats <- rep(1:nrow(df_rest), freqs)
    # print(ix_repeats)  # 4debugging 
    
    # 7. Use ix_repeats to generate new data frame:
    df_out <- df_rest[ix_repeats, ]
    
    # 8. Trivial case: If df_out is only a vector: 
    if (!is.data.frame(df_out)){
      
      df_out <- data.frame(df_out)  # make data frame
      names(df_out) <- "v1"
      # print(df_out)  # 4debugging 
      
    }
    
    # 9. Repair rownames:
    if (row_name_repair){
      
      row.names(df_out) <- NULL # NULL sets row names to "automatic" values 
      # of seq_len(nrow(x)) (i.e., 1:nrow(df_out))
      
    }
    
  } # if (df) else. 
  
  # 10. Output: 
  return(df_out) 
  
} # expand_freq_table(). 

# # Check:
# # (a) from raw data (vectors):
# ans <- sample(c("yes", "no", "maybe"), 100, replace = TRUE)
# eat <- sample(c("fish", "meat", "veggie"), 100, replace = TRUE)
# df_1 <- data.frame(ans, eat)  # data frame from vectors
# df_2 <- expand_freq_table(data.frame(table(ans, eat))) # from table > contingency table > data frame
# all.equal(table(df_1), table(df_2))
# 
# # (b) from array/table:
# df <- expand_freq_table(UCBAdmissions) # array/table > contingency table > df
# tb <- table(df)                        # df > array/table 
# all.equal(UCBAdmissions, tb)
# 
# # Full circle (4D array > contingency table > data frame > 4D array): 
# df <- expand_freq_table(Titanic)
# tb <- table(df)
# all.equal(Titanic, tb)
# 
# # From 2x2 table:
# (mx <- margin.table(Titanic, c(4, 2)))
# all.equal(mx, table(expand_freq_table(mx)))
# 
# # Trivial case:
# expand_freq_table(data.frame(x = "a", Freq = 2))
# 
# # Note: Works for non-normal data frames (with positive freq count variables): 
# count <- round(runif(100, min = 0, max = 3), 0)  # count must be non-negative integers
# df <- data.frame(ans, count)
# head(df)
# ed <- expand_freq_table(df, freq_var = "count")
# ed <- expand_freq_table(df, freq_var = 2)
# dim(ed)
# head(ed)



## subtable_names: Utility function to get the names of a table: -----

# subtable_names returns the names of a table tbl that are specified in a list dim_list.
# 
# Crucially, for each element of dim_list, the list can specify levels as names OR numeric indices. 
#
# Constraints: 
# - The table tbl is assumed to be named
# - The length of dim_list is assumed to match the length of dimnames(tbl)
#
# Note: 
# - Dimensions in tbl are considered in the order provided.
# - The names of additional dimensions (without names or numbers in dim_list) are fully included.
# - Additional/extra dim_list arguments are truncated.
#
# Currently NOT USED or exported. 

subtable_names <- function(tbl, dim_list){
  
  # Initialize:
  org_name_list <- dimnames(tbl)  # original list of dimnames
  new_name_list <- org_name_list  # initialize a new list (to be reduced)
  n_dim <- length(dim_list)       # N of desired dimensions
  
  # Verify correspondence of list lengths:
  if (n_dim < length(org_name_list)){  # Notify user: 
    message("subtable_names: dim_list is shorter than dimnames(tbl). Using dimnames(tbl) in turn:")
  }
  
  if (n_dim > length(org_name_list)){  # Notify user: 
    message("subtable_names: dim_list is longer than dimnames(tbl). Truncating to same length:")
    n_dim <- length(org_name_list)
  }
  
  # Main: 
  for (i in 1:n_dim){
    
    # org_dim_name <- names(org_name_list[i])
    # cur_dim_name <- names(dim_list[i])
    
    # if ( (is.null(cur_dim_name)) | (cur_dim_name == org_dim_name) ){
    
    cur_lev_vec <- dim_list[[i]]  # extract current levels (element of dim_list)
    
    if (is.numeric(cur_lev_vec)){  # provided a numeric index
      
      cur_lev_names <- org_name_list[[i]]  # all names of current levels
      
      sub_cur_lev_names <- cur_lev_names[cur_lev_vec]  # desired subset
      # print(sub_cur_lev_names)  # 4debugging
      
    } else { # cur_lev_vec is NOT numeric: use names as provided
      
      sub_cur_lev_names <- cur_lev_vec  # copy desired subset
      
    }
    
    new_name_list[[i]] <- sub_cur_lev_names  # Reduce cur level names to desired subset
    # print(new_name_list)[[i]]  # 4debugging  
    
    # } # if (cur_dim_name).
    
  } # loop end. 
  
  # Output:
  return(new_name_list) 
  
} # subtable_names(). 

## Check: 
# # A purely numeric index as dim_list:
# subtable_names(Titanic, dim_list = list(c(1, 3), 2, 2, 2))
# 
# # A mix of names and numeric index:
# subtable_names(Titanic, dim_list = list(c(1, 3), "Female", 2, "Yes"))
# 
# # A Warning: If dim_list has fewer/more elements than dimensions:
# subtable_names(Titanic, dim_list = list(c(1, 3), "Female", 1))
# subtable_names(Titanic, dim_list = list(c(1, 3), "Female", 1, 2, 99))
# # Note:
# # - Dimensions are considered in the order provided.
# # - The names of additional dimensions (without names or numbers in dim_list) are fully included.
# # - Additional/extra dim_list arguments are truncated.



## subtable: Extract a subtable (or subset) from a table: ----- 

# Source: Core is based on the subtable() function by 
#         Norman Matloff (2011), The Art of R Programming (pp. 131--134), 
# but using sublist_tbl() function to include/exclude dimensions/levels.

#' Extract a subtable (or subset) from a table. 
#' 
#' \code{subtable} yields a subtable of a table \code{tbl}
#' by filtering or extracting a subset of table's 
#' dimensions and levels. 
#' 
#' \code{subtable} provides a filter/slice function for tables, by specifying 
#' a positive subset (i.e., dimensions and levels to include) as \code{in_list} or  
#' a negative subset (i.e., dimensions and levels to exclude) as \code{out_list} 
#' (both as lists that specify dimension and level names, in tag and value format). 
#' 
#' \code{subtable} assumes that \code{dimnames(tbl)} exist, 
#' but is flexible in allowing \code{in_list} and \code{out_list} 
#' to use both names or numeric indices for subsetting table levels.
#' 
#' As a key constraint, the subset of dimensions specified by 
#' \code{in_list} and \code{out_list} must still contain all dimensions 
#' (as excluding an entire dimension would leave no elements of a table) 
#' and yield a list with elements in the same order as \code{dimnames(tbl)} 
#' (to allow using \code{do.call} on \code{what = "[", args}  
#' to index/subset the array of tbl).
#' 
#' @return A table. 
#' 
#' @param tbl An original table to be reduced (as \code{\link{table}}). 
#' 
#' @param in_list A list specifying the subset of \code{tbl} to keep/include. 
#' Each list element is named after a dimension of \code{tbl}, 
#' and the value of that component is a vector of the names or a numeric index 
#' of the desired levels. 
#' Default: \code{in_list = dimnames(tbl)} (i.e., everything in \code{tbl}).
#' 
#' @param out_list A list specifying the subset of \code{tbl} to drop/exclude. 
#' Each list element is named after a dimension of \code{tbl}, 
#' and the value of that component is a vector of the names or a numeric index 
#' of the desired levels. 
#' Default: \code{out_list = NULL} (i.e., nothing). 
#' 
#' @param quiet Boolean: Hide feedback messages?
#' Default: \code{quiet = FALSE} (i.e., show messages). 
#' 
#' @examples 
#' t <- datasets::Titanic
#' 
#' # Trivial case:
#' subtable(t, in_list = dimnames(t))  # same as t
#' 
#' # (a) Using level names:
#' subtable(t, in_list = list(Class = c("1st", "2nd", "3rd"),
#'                            Sex = "Female", Age = "Adult",
#'                            Survived = c("Yes")))
#' 
#' # Alternatives ways of obtaining same result:
#' subtable(Titanic, out_list = list(Class = c("Crew"),
#'                                   Sex = "Male", Age = "Child",
#'                                   Survived = c("No")))
#' 
#' subtable(Titanic, in_list = list(Class = c("1st", "2nd", "3rd"),
#'                                  Sex = "Female", Age = c("Adult", "Child"),
#'                                  Survived = c("Yes", "No")),
#'          out_list = list(Age = "Child", Survived = c("No")))
#' 
#' # (b) Using dim names and level numbers:
#' subtable(t, in_list = list(Class = 1:3, Sex = 2, Age = 2, Survived = 2))
#' 
#' # (c) Using only level numbers (note messages):
#' subtable(t, in_list = list(1:3, 2, 2, 2))
#' 
#' # (d) Using a mix of level names and numbers (note messages): 
#' subtable(t, in_list = list(1:3, "Female", 2, "Yes"))
#' 
#' # (e) Note: Different length of sub_dims than dimnamesI(tbl) yield ERRORs:
#' # subtable(t, in_list = list(1:3, "Female", 2, 2, 99))
#' 
#' # Any tbl dimensions (names/levels) not changed by in_list/out_list are included in full:
#' subtable(t, in_list = list(1:3, "Female", Survived = 2))  # both Age levels included
#'  
#' @source Based on the \code{subtable} function by Norman Matloff, 
#' The Art of R Programming (2011, pp. 131-134). 
#' 
#' @family array functions
#' 
#' @seealso
#' \code{\link{ctable}} for creating a contingency table; 
#' \code{\link{sublist}} for extracting subsets of a list; 
#' \code{\link{table}} and \code{\link{array}} for the underlying data structures. 
#' 
#' @export

subtable <- function(tbl, 
                     # sub_dims = dimnames(tbl), # s1: Use sublist_names() function
                     in_list = dimnames(tbl), out_list = NULL, quiet = FALSE  # s3: Use sublist_tbl() function
) 
{
  
  # 0. Preparation:
  tbl_names <- dimnames(tbl)
  
  if (is.null(tbl_names)){
    message("subtable: tbl has no dimnames.")
  }
  
  # sub_dims <- sublist_names(name_list = tbl_names, sub_list = sub_dims)  # s1: Use sublist_names() function
  # sub_dims <- sublist(ls = tbl_names, in_list = in_list, out_list = out_list, quiet = quiet)    # s2: Use sublist() function
  sub_dims <- sublist_tbl(ls = tbl_names, in_list = in_list, out_list = out_list, quiet = quiet)  # s3: Use sublist_tbl() function  
  # print(sub_dims)  # 4debugging
  
  # Check sub_dims:
  if (!all.equal(names(sub_dims), names(tbl_names))){
    message("subtable: Specified dimensions must contain all tbl names in the order of dimnames(tbl).")
  }
  
  # 1. Deconstruction: 
  # Get the array of cell counts in tbl:
  t_array <- unclass(tbl)
  
  # 2. Preparation:
  # Strategy: We get the sub_array of cell counts corresponding to sub_dims 
  #           by calling do.call() on the "[" function; 
  #           but build up a list of its arguments first: 
  
  dc_args <- list(t_array)  # 1st element of list is t_array
  # message(dc_args)  # 4debugging: List of count values
  
  n_dims <- length(sub_dims)  # number of desired dimensions
  
  if (n_dims < length(tbl_names)) {
    message("subtable: Fewer dimensions specified than in tbl.")
  } else if (n_dims > length(tbl_names)) {
    message("subtable: More dimensions specified than in tbl.")
  }
  
  for (i in 1:n_dims) {
    dc_args[[i + 1]] <- sub_dims[[i]]
  }
  
  # message(dc_args)  # 4debugging: List of count values + desired dimension levels
  
  # 3. Main:   
  sub_array <- do.call(what = "[", args = dc_args)
  
  # 4. Reconstruction: 
  # Build the new table, consisting of the sub_array, 
  # the number of levels in each dimension, the dimnames() value, 
  # plus the "table" class attribute: 
  
  dims  <- lapply(sub_dims, length)
  sub_tbl <- array(sub_array, dim = dims, dimnames = sub_dims) 
  class(sub_tbl) <- "table"
  
  # 5. Output: 
  return(sub_tbl)
  
} # subtable(). 

# ## Check:
# # (A) Use with a dummy table (from an array):
# dims <- 4:2
# v <- sample(1:100, size = prod(dims), replace = FALSE)
# a <- array(v, dims)
# a <- add_dimnames(a)
# (t <- as.table(a))
# 
# # standard case:
# subtable(t, in_list = list(row = c("r1", "r2"),
#                            col = c("c1", "c2"),
#                            tab = c("t2")))
# 
# # works with numeric indexing of level names:
# subtable(t, in_list = list(row = 1:2,
#                            col = 1:2,
#                            tab = 2))
# 
# # works without dimension names (but note messages):
# subtable(t, in_list = list(1:2, 1:2, 2))
# 
# # using a mix of names and numeric indices (note messages):
# subtable(t, in_list = list(1:2, c("c1", "c2"), 2))
# 
# 
# # (B) Use an existing table (with dimnames): Titanic
# 
# # (a) Use level names:
# subtable(Titanic, in_list = list(Class = c("1st", "2nd", "3rd"),
#                                  Sex = "Female", Age = "Adult",
#                                  Survived = c("Yes")))
# 
# # Alternatives ways of obtaining same result:
# subtable(Titanic, out_list = list(Class = c("Crew"),
#                                   Sex = "Male", Age = "Child",
#                                   Survived = c("No")))
# 
# subtable(Titanic, in_list = list(Class = c("1st", "2nd", "3rd"),
#                                  Sex = "Female", Age = c("Adult", "Child"),
#                                  Survived = c("Yes", "No")),
#          out_list = list(Age = "Child", Survived = c("No")))
# 
# # (b) Use dim names and level numbers:
# subtable(Titanic, in_list = list(Class = 1:3, Sex = 2, Age = 2, Survived = 2))
# 
# # (c) Use level numbers only (note messages):
# subtable(Titanic,  in_list = list(1:3, 2, 2, 2))
# subtable(Titanic, out_list = list(4, 1, 1, 1))
# 
# # (d) Use a mix of level names and numbers (note messages):
# subtable(Titanic,  in_list = list(1:3, "Female", 2, "Yes"))
# subtable(Titanic, out_list = list(4, "Male", 1, "No"))
# subtable(Titanic, out_list = list(4, "Male", Survived = "No"))
# 
# # Key constraint: 
# # sub_dims must contain ALL dimensions/names/tags of tbl_names in the SAME order: 
# subtable(Titanic, in_list = list(Class = 1:3, Sex = "Female", Age = 2, Survived = "Yes"))  # works
# subtable(Titanic, in_list = list(Age = 2, Sex = "Female", Class = 1:3, Survived = "Yes"))  # works: different order of dimnames
# subtable(Titanic, in_list = list(Class = 1:3, Age = 2, Survived = "Yes"))  # works: not all dimensions in sublist
# 
# # (e) Note: Using fewer elements in in_list than dimnamesI(tbl):
# subtable(Titanic, in_list = list(Age = 2, Survived = "Yes"))
# subtable(Titanic,
#          in_list = list(Class = 1:3, Sex = "Female", Age = 2),
#          out_list = list(Survived = 1))
# 
# # Note NA/errors for:
# subtable(Titanic, out_list = list("Female", "Male")) 
# subtable(Titanic, in_list = list(1:3, "Female", 2, 2, 99))  # excess of dimensions (with no tag)

# ToDo: 
#
# - The filter() function from dplyr only includes cases that are specified.
#   Consider changing subtable() to only include specified dimensions 
#   (when length(in_list) < length(dimnames(tbl))).
#
# - Create a negative version that filters out/excludes specified dimensions and levels, 
#   rather than including all and only specified dimensions and levels.


## ctable: Create a contingency table from description (as tb or df, analogous to array): ------ 

# A function to define a multi-dimensional array/contingency table 
# from a description of frequency counts and dimensions 
# (e.g., a case of Simpson's paradox). 

# ## Example: 
# # Data illustrating Simpson's paradox (The book of why, Pearl & McKenzie, p. 201):
# 
# # Vectors of factors/dimensions and levels:
# group <- c("control", "treatment")
# sex <- c("female", "male")
# outcome <- c("heart attack", "no attack") 
# 
# # Frequency values:
# # group:   control:       treatment:
# v <- c(1, 12, 19, 28,   3, 8, 37, 12) # Note: by-column order (per subtable)
# 
# # As array:
# (a <- array(v, 
#             dim = c(2, 2, 2), 
#             dimnames = list(sex = sex, outcome = outcome, group = group) # Note: Reversed order of dimensions (from inner to outer)!
# ))
# (tb <- as.table(a))
# 
# # Contingency table as df: 
# data.frame(tb)
# # data.frame(a)  # would NOT work!
# (tb_df <- data.frame(ftable(a)))
# 
# # Back to array/table:
# (tb_2 <- table(expand_freq_table(tb_df)))
# all.equal(tb, tb_2)

# Tasks: 
# 1. Reverse order of name columns in tb_df
# 2. Write a function that takes v, dim, and dimnames 
#    and returns either tb or tb_df in a robust fashion. 

# # ad 1. Reverse order of name columns in tb_df:
# (n_names <- length(list(sex = sex, outcome = outcome, group = group)))
# (non_names <- tb_df[-(1:n_names)])
# (ix_rest <- -(1:n_names))
# (tb_df_2 <- cbind(tb_df[n_names:1], tb_df[ix_rest]))

# ad 2. Write a function that takes v, dim, and dimnames 
#       and returns either tb or tb_df in a robust fashion:

## ctable() function:

# Note: 
# ctable() currently works by passing arguments to array()  
# and then converting array() into a table via as.table()
# Thus, the order of data, dim, and dimnames corresponds to array() defaults, 
# implying that data is entered in by-col order and dimensions from y/left, x/top, to group/table. 

# Desired data frames are created from array/table [via data.frame(tb)], 
# rather than building the df from scratch via 
# expand.grid(outcome, sex, group, stringsAsFactors = TRUE)  
# Note reversed order of vecs!

#' Create a contingency table from data and description (as a table). 
#' 
#' \code{ctable} yields a contingency table (as a \code{table}) 
#' in analogy to creating an array by \code{\link{array}}. 
#' 
#' \code{ctable} internally passes its arguments to \code{\link{array}} 
#' before coercing the resulting \code{array} into a \code{table} or \code{data.frame}. 
#' 
#' The order of elements in its arguments \code{data}, \code{dim}, 
#' and \code{dimnames} must correspond to the defaults of \code{\link{array}}. 
#' For instance, \code{data} are entered in a by-column fashion 
#' and the dimensions in \code{dimnames} from left/y, top/x, to group/table/z, etc. 
#' 
#' @return A table. 
#' 
#' @param data The data to be used (required; 
#' frequency counts, as a \code{\link{vector}} of positive integers). 
#' 
#' @param dim The dimensions to be used 
#' (as a numeric \code{\link{vector}}). 
#' Default: \code{dim = length(data)}. 
#' 
#' @param dimnames The dimension names to be used 
#' (as a \code{\link{list}}). 
#' Default: \code{dimnames = NULL}, using 
#' \code{\link{add_dimnames}} per default.  
#' 
#' @param by_row (Boolean): 
#' Are \code{data} to be read in a by-row fashion? 
#' Default: \code{by_row = FALSE}. 
#' 
#' @param as_df (Boolean): 
#' Return result as a \code{\link{data.frame}}?
#' Default: \code{as_df = FALSE}. 
#' 
#' @param quiet Boolean: Hide feedback messages?
#' Default: \code{quiet = FALSE} (i.e., show messages). 
#' 
#' @examples
#' # Prepare dimnames (as lists): 
#' l_24 <- list(rows = c("A", "B"), cols = letters[1:4])
#' l432 <- list(Ys = letters[1:4], Xs = LETTERS[1:3], group = c("I", "II"))
#' 
#' # Standard use:
#' ctable(1:8,  dim = c(2,4 ), dimnames = l_24)
#' ctable(1:24, dim = c(4, 3, 2), dimnames = l432)
#' 
#' # by row:
#' ctable(1:8,  dim = c(2, 4), dimnames = l_24, by_row = TRUE)
#' ctable(1:24, dim = c(4, 3, 2), dimnames = l432, by_row = TRUE)
#' 
#' # Return as df:
#' ctable(1:8, dim = c(2, 4), dimnames = l_24, as_df = TRUE)
#' ctable(1:24, dim = c(4, 3, 2), dimnames = l432, as_df = TRUE)
#' 
#' # No dimnames:
#' ctable(1:8, dim = c(2, 4))
#' ctable(1:8, dim = c(2, 2, 2))
#' 
#' # Non-integers:
#' ctable(1:9/3, dim = c(3, 3), quiet = FALSE)
#' 
#' @family array functions
#' 
#' @seealso
#' \code{\link{subtable}} for extracting a subset of a table;  
#' \code{\link{sublist}} for extracting subsets of a list;  
#' \code{\link{table}} and \code{\link{array}} for the underlying data structures. 
#' 
#' @export

ctable <- function(data, 
                   dim = length(data), dimnames = NULL, 
                   by_row = FALSE, as_df = FALSE, quiet = FALSE){
  
  # Inputs:
  # Verify that data contains only frequency counts (integers >= 0).
  # a) only numeric values
  # b) only integers >= 0
  if (!quiet){
    
    if (!is.numeric(data) | any(data %% 1 != 0) | any(data < 0)) {
      
      message("ctable: Contingency data must only contain frequency counts (i.e., non-negative integers).")
      
    }
    
    if (length(data) != prod(dim)){ # conflict between data and dim: 
      
      message("ctable: Length of data does not correspond to dim. Using array() defaults...")
      
    }
    
  } # if (!quiet). 
  
  
  # Main: Pass to array():
  ar <- array(data = data, # Note: by-col order of data (as in array()) 
              dim = dim, 
              dimnames = dimnames # Note: by-col order of dimensions (from left/Y, inner/X, outer/table)!
  )
  
  
  if (by_row){  # swap dimensions 1 and 2 of array: 
    
    ar <- swap_xy_dims(ar, x = 1, y = 2)  # use utility function (in util.R)
    
  }
  
  # Check/add names:
  if (is.null(dimnames)){ # no dimnames provided:
    
    if (!quiet){
      message("ctable: No list of dimnames provided. Adding default dimnames:")
    } # if (!quiet).
    
    ar <- add_dimnames(ar, sep = "_")  # use utility function (above)
    
  }
  
  # Coerce to "table" (to flag impossible values):
  tb <- as.table(ar, stringsAsFactors = TRUE)  # Note: Use factors to preserve level orders  
  
  if (as_df){ # convert to df:
    
    df <- data.frame(tb)
    
    # if (!by_row){ # Reverse the order of named variables/columns:
    
    n_names <- length(dimnames(tb))
    ix_rest <- -(1:n_names)
    df <- cbind(df[n_names:1], df[ix_rest])  # reverse order of initial n_names columns
    
    # }
    
    return(df) 
    
  } else { # default: 
    
    return(tb)
    
  }
  
} # ctable(). 

# # Check:
# 
# # (0) Row/column/group names (as lists): 
# (l_24 <- list(rows = c("A", "B"), cols = letters[1:4]))
# (l432 <- list(Ys = letters[1:4], Xs = LETTERS[1:3], group = c("I", "II")))
# 
# # (1) Abstract cases:
# ctable(1:8,  dim = c(2,4), dimnames = l_24)
# ctable(1:24, dim = c(4,3,2), dimnames = l432)
# 
# # by row:
# ctable(1:8,  dim = c(2,4), dimnames = l_24, by_row = TRUE)
# ctable(1:24, dim = c(4,3,2), dimnames = l432, by_row = TRUE)
# 
# # Return as df:
# ctable(1:8, dim = c(2,4), dimnames = l_24, as_df = TRUE)
# ctable(1:24, dim = c(4,3,2), dimnames = l432, as_df = TRUE)
# 
# # No dimnames:
# ctable(1:8, dim = c(2,4))
# ctable(1:8, dim = c(2,2,2))
# 
# # Entering data in by-row direction:
# ctable(1:12, dim = c(3,4), dimnames = list(X = paste0("x_", 1:3),
#                                            Y = paste0("y_", 1:4)), by_row = TRUE)
# ctable(1:24, dim = c(3,4,2), dimnames = list(X = paste0("x_", 1:3),
#                                              Y = paste0("y_", 1:4),
#                                              Z = paste0("z_", 1:2)), by_row = TRUE)
# 
# # Conflicting data and dim (see array()):
# ctable(1:8, dim = c(2, 2))  # truncating data
# ctable(1:8, dim = c(2, 5))  # recycling data
# ctable(1:8, dim = c(2, 3), as_df = TRUE)  # truncated df
# 
# # Note impossible values:
# ctable(c(1:8), dim = c(2, 4), by_row = FALSE)   # ok
# ctable(c(0:7), dim = c(2, 4), by_row = FALSE)   # ok
# ctable(c(-1:6), dim = c(2, 4), by_row = FALSE)      # negative
# ctable(c(1:8 + .1), dim = c(2, 4), by_row = FALSE)  # non-integer
# 
# 
# # # (2) Example use cases: --------  
# 
# # A. Mammography problem (2D, 2x2): ------
# xdim <- c("cancer", "no cancer")
# ydim <- c("postive test", "negative test")
# dims <- list(dec = ydim, cond = xdim)
# freq <- c(8, 2, 95, 895)
# 
# # contingency table:
# (ct <- ctable(data = freq, dim = c(2, 2), dimnames = dims))
# 
# # Check:
# is.table(ct)
# is.matrix(ct)
# 
# # Note: 
# is.factor(as.data.frame(ct)$dec)
# as.data.frame(ct)$dec  # Dimensions are factors
# tibble::as_tibble(ct)  # Dimensions are character variables!
# 
# # Full circle:
# as_df <- data.frame(ct)
# cases <- i2ds::expand_freq_table(as_df)
# as_tb <- table(cases)
# all.equal(as_tb, ct)


# # B. Simpson's paradox (Example 1: The book of why, Pearl & McKenzie, p. 201): ------ 
# 
# # Frequency values:
# # group:    control:         treatment:
# freq_v <- c(1, 12, 19, 28,   3, 8, 37, 12) # Note: by-column order (per subtable)
# 
# # Vectors of factors/dimensions and levels:
# group   <- c("control", "treatment")
# sex     <- c("female", "male")
# outcome <- c("heart attack", "no heart attack")
# 
# # as list in order: y/left, x/top, group/tab, as in array():
# dnl <- list(sex = sex, outcome = outcome, group = group)
# 
# ctable(freq_v, c(2, 2, 2), dimnames = dnl)
# ctable(freq_v, c(2, 2, 2), dimnames = dnl, as_df = TRUE)
# 
# # Using default dimnames:
# ctable(freq_v, c(2, 2, 2))
# ctable(freq_v, c(2, 2, 2), as_df = TRUE)
# ctable(freq_v, c(2, 4))
# 
# ## Data directions:
# freq_v2 <- 1:16
# matrix(freq_v2, nrow = 4, byrow = FALSE) # by-col (default)
# matrix(freq_v2, nrow = 4, byrow = TRUE)  # by-row
# 
# array(freq_v2, c(4, 4))     # by-col (default)
# array(freq_v2, c(4, 2, 2))  # by-col


# # C. Simpson's paradox (Example 2: Causality, Pearl, 2009, p. 174ff.): ------
# 
# # +++ here now +++
# 
# # Load pkgs:
# library(i2ds)      # ctable(), expand_freq_table(), subtable()
# library(MLM)       # frame(), trans(), focus()
# library(magrittr)  # pipe %>%
# 
# # (A) Data: ----
# 
# # Frequency values:
# # group:  male (-F):          female (F):
# freq_v3 <- c(18, 12, 7, 3,    2, 8, 9, 21) # Note: by-column order (per subtable)
# 
# # Vectors of factors/dimensions and levels:
# sex    <- c("male (-F)",    "female (F)")
# cause  <- c("drug (C)",     "no drug (-C)")
# effect <- c("recovery (E)", "sick (-E)")
# 
# # as list in order: y/left, x/top, group/tab, as in array():
# dnl <- list(effect = effect, cause = cause, sex = sex)
# 
# # Create 3 informationally equivalent data structures:
# (S_tb <- ctable(freq_v3, c(2, 2, 2), dimnames = dnl))  # 3D table
# (S_ct <- ctable(freq_v3, c(2, 2, 2), dimnames = dnl, as_df = TRUE))  # contingency table (df)
# (S_df <- expand_freq_table(S_ct))  # df of raw cases
# 
# # (B) Illustrating the paradox: ----
# 
# # Collapsing over one dimension: Framing a 2x2 matrix:
# (mx_A <- frame(S_tb, x = "cause", y = "effect"))  # from 3D-table
# mx_A <- frame(S_df, x = "cause", y = "effect")    # from df of raw cases
# 
# # Collapse for 2 other aggregate matrices (using pipe):
# (mx_B <- S_tb %>% frame(x = "cause", y = "sex"))
# (mx_C <- S_tb %>% frame(x = "effect", y = "sex"))
# 
# # Selecting a slice/subset of a 3D table:
# (tb_A_m <- subtable(S_tb,  in_list = list(sex = "male (-F)")))
# (tb_A_f <- subtable(S_tb, out_list = list(sex = "male (-F)")))
# 
# # as 2x2 matrix:
# (mx_A_m <- frame(tb_A_m, x = "cause", y = "effect"))
# (mx_A_f <- frame(tb_A_f, x = "cause", y = "effect"))
# 
# # Conditionalizing matrices by row/column/diagonal: Transforming
# mx_A %>% trans(margin = 2)  # by column
# mx_B %>% trans(margin = 2)  # by column
# mx_C %>% trans(margin = 1)  # by row!
# 
# # Conditionalizing mx of subtable:
# mx_A_m %>% trans(margin = 2)  # by column
# mx_A_f %>% trans(margin = 2)  # by column
# 
# # Focus on measure(s): Derive difference in by-column contingency/dPc:
# focus(mx_A_m, measures = "dPc")
# focus(mx_A_f, measures = "dPc")
# # vs. aggregate matrix:
# focus(mx_A, measures = "dPc")
# 
# # Same measure(s) for alternative aggregates:
# focus(mx_B, measures = "dPc")
# focus(mx_C, measures = "dPc")


# # D. Shades of skepticism (3x3): ----
# t <- "TRUE"
# q <- "?"
# f <- "FALSE"
# belief   <- c(t, q, f)
# truth    <- c(t, q, f)
# evidence <- c(t, q, f)
# dnl <- list( belief = belief, evidence = evidence, truth = truth)
# v <- 1:27
# 
# cube <- ctable(v, c(3,3,3), dnl, as_df = FALSE)
# head(cube)
# 
# data.frame(subtable(cube, in_list = list(belief = "?")))
# data.frame(subtable(cube, in_list = list(truth = "TRUE", belief = "?")))

# +++ here now +++ 


# ## Using ctable() with X, Y, Z: ---- 
# 
# # 2D:
# dims <- c(4, 3)
# dnl <- list(X = paste0("x_", 1:4),
#             Y = paste0("y_", 1:3))
# (t2 <- ctable(data = 1:prod(dims), dim = dims, dimnames = dnl))
# (t2r <- ctable(data = 1:prod(dims), dim = dims, dimnames = dnl, by_row = TRUE))
# 
# # 3D:
# dims <- c(4, 3, 2)
# dnl <- list(X = paste0("x_", 1:4),
#             Y = paste0("y_", 1:3),
#             Z = paste0("z_", 1:2))
# 
# (t3 <- ctable(data = 1:prod(dims), dim = dims, dimnames = dnl))
# (t3r <- ctable(data = 1:prod(dims), dim = dims, dimnames = dnl, by_row = TRUE))
# 
# # 4D:
# dims <- c(5, 4, 3, 2)
# dnl <- list(X = paste0("x_", 1:5),
#             Y = paste0("y_", 1:4),
#             M = paste0("m_", 1:3),
#             O = paste0("o_", 1:2))
# 
# (t4 <- ctable(data = 1:prod(dims), dim = dims, dimnames = dnl))
# (t4r <- ctable(data = 1:prod(dims), dim = dims, dimnames = dnl, by_row = TRUE))


# # Unused snippets: 
# v_list <- list(group = group, sex = sex, outcome = outcome)
# names(v_list)
# rev(v_list)
# 
# # As df of all combinations:
# expand.grid(outcome, sex, group, stringsAsFactors = TRUE)  # Note reversed order of vecs!



## Key data structures: Contingency table (as a data frame, with a freq_var): ------ 

# Note: A contingency table (as df) can easily be created from and transformed into an array/table:

# a <- array(1:prod(5:3), dim = 5:3)
# a <- i2ds::add_dimnames(a)
# a <- as.table(a)  # turn array into table (note: any "table" is still an "array")
# # is.array(a)
# # is.table(a)
# a
# 
# tb_org <- a # Titanic # UCBAdmissions ##  Data (as table)
# tb_org
# # str(tb_org)
# 
# df_con <- as.data.frame(ftable(tb_org))       # contingency table (as df)
# df_con
# 
# # Note the inverse of as.data.frame(table):
# tb_xtb <- xtabs(Freq ~ ., data = df_con)
# tb_xtb
# # str(tb_xtb)
# all.equal(tb_xtb, tb_org)  # Note: xtabs are NOT of type table!
# 
# df_raw <- i2ds::expand_freq_table(df_con)  # raw cases (as df)
# df_raw
# 
# tb_new <- table(df_raw)
# tb_new
# 
# all.equal(tb_new, tb_org)

# # Full circle (table > contingency (df) > cases (df) > table):
# tb_org <- UCBAdmissions # Titanic  # from table
# (ct_df <- as.data.frame(tb_org))   # contingency frame
# (ct_raw <- i2ds::expand_freq_table(ct_df))  # raw cases
# (tb_new <- table(ct_raw))          # to table
# all.equal(tb_new, tb_org)

# # Half circle (table > contingency (df) > table):
# (tb_org <- UCBAdmissions) # Titanic  # from table
# (ct_df <- as.data.frame(tb_org))   # contingency frame
# (tb_xt <- xtabs(Freq ~ ., ct_df))  # xtabs  
# all.equal(tb_xt, tb_org)  # Note: Attribute differences!
# tb_tb <- ctable(tb_xt, dim = dim(tb_org), dimnames = dimnames(tb_org))  # xtabs as table!
# all.equal(tb_tb, tb_org)  
# # +++ here now +++ 

## ToDo: ------

# ctable() function to define a multi-dimensional array/contingency table.
#          (e.g., from a description of a case of Simpson's paradox)

# Work out relations between 
# A: Data structures:
#    n-dimensional array/table <--> contingency table (as df) <--> table of raw cases (as df)
# and 
# B: Functions:
# - ctable() defines an n-dimensional table from description
# - expand_freq_table()
# - data.frame(), as.data.frame() 
# - array(), table(), xtabs() 
# - stats::ftable()


## eof. ----------
