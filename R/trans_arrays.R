## trans_arrays.R | i2ds
## hn | uni.kn | 2021 07 29
## -------------------------

# Functions for transforming/manipulating arrays/tables. 

## add_dimnames: Add default names to array dimensions: ------ 

#' Add dimension names (to arrays). 
#' 
#' \code{add_dimnames} adds (or re-assigns) dimension names to arrays 
#' (or data frames or atomic vectors, but not other lists). 
#' 
#' When \code{x} is an atomic vector, 
#' \code{add_dimnames} returns a named vector. 
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
#' add_dimnames(a, dnames = c("X", "Y", "Z", "XYZ"), prefix = c("x", "y", "z"), sep = "_")
#' 
#' # More dimensions:
#' b <- array(1:2^5, dim = rep(2, 5))
#' add_dimnames(b)
#' add_dimnames(b, dnames = "D")
#' add_dimnames(b, dnames = "dim", prefix = c("r", "c", "u", "v", "w"), sep = "_")
#' 
#' # For data frames:
#' c <- data.frame(let = letters[1:4], num = 5:8)
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
#' @seealso
#' \code{\link{provideDimnames}} is a more basic version. 
#' 
#' @export 

add_dimnames <- function(x, dnames = c("row", "col", "tab"), prefix = c("r", "c", "t"), sep = ""){
  
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
    
    # Create name_vec: 
    name_vec <- paste(prefix, 1:elm_nr, sep = sep) 
    
    # Assign name_vec to x (as a vector):
    names(x) <- name_vec
    
  } else { # (C) x is a data frame or an array:
    
    dim_nr <- length(dims)  # nr of dimensions
    
    # Check dnames length: 
    if (length(dnames) == 1){ # atomic dnames:
      
      dnames <- paste(dnames, 1:dim_nr, sep = sep)  # add dim number to dnames
      
    } else if (length(dnames) > dim_nr){  # too many dnames for dim_nr: 
      
      dnames <- dnames[1:dim_nr]  # truncate dnames
      
    } else if (length(dnames) != dim_nr){ # dim_nr exceeds dnames: 
      
      message("add_dimnames: Length of dnames must match x dimensions. Using LETTERS:")
      dnames <- LETTERS[1:dim_nr]
      
    }
    
    # Check prefix length: 
    if (length(prefix) != dim_nr){
      
      message("add_dimnames: Length of prefix must match x dimensions. Using letters:")
      prefix <- letters[1:dim_nr]
      
    }
    
    # Create name_list (as a list): 
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
#' into a 2-dimensional data frame.
#' 
#' \code{flatten_array} assumes that \code{x} is a 3-dimensional array 
#' with dimension names (and calls \code{\link{add_dimnames}} if not).
#' 
#' \code{flatten_array} returns \code{NA} for non-arrays and  
#' for arrays with more than 3 dimensions. 
#' 
#' Internally, \code{flatten_array} uses \code{apply} to apply 
#' the \code{\link{c}} function to a specified \code{margin} of \code{x}. 
#' It aims to reconstruct the names of the collapsed variables 
#' from the initial letters of the dimension names. 
#' 
#' See \code{\link{ftable}} (from the \strong{stats} package) 
#' for a more general function (in combination with \code{\link{aperm}})  
#' and \code{\link{margin.table}} (from \strong{base} R) 
#' and \code{\link{addmargins}} (from \strong{stats}) for aggregating 
#' over array/table dimensions.
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
#' # UCBAdmissions data: 
#' flatten_array(UCBAdmissions)  # margin = 2
#' flatten_array(UCBAdmissions, margin = 1)
#' flatten_array(UCBAdmissions, margin = 3)
#' 
#' # Dummy data: 
#' a1 <- array(data = 1:8, dim = c(2, 2, 2), 
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


## expand_data_frame: Turn a contingency table into a data frame of raw cases: ------ 

#' Expand an array/contingency table into a data frame. 
#' 
#' \code{expand_data_frame} turns an array/table or contingency table 
#' (with a frequency count variable denoting the number of corresponding cases) 
#' into a data frame of raw cases.
#' 
#' \code{expand_data_frame} assumes that \code{x} is an array/table 
#' or a data frame with a frequency count variable \code{freq_var}. 
#' 
#' If \code{x} is an array/table, \code{expand_data_frame} uses 
#' \code{\link{data.frame}} to turn \code{x} into a contingency table 
#' (with a frequency variable named \code{"Freq"} by default). 
#' 
#' The function allows turning data stored in an array/table 
#' or a contingency table (with a frequency count variable \code{freq_var}) 
#' into a corresponding data frame of raw cases. 
#' The number of cases (rows) in the resulting data frame 
#' corresponds to \code{sum(x$freq_var)}. 
#' 
#' @return A data frame. 
#' 
#' @param x An array/table or contingency table (as data frame). 
#' 
#' @param freq_var The name of the frequency count variable in 
#' a data frame \code{x}. 
#' Default: \code{freq_var = "Freq"} (i.e., as in \code{data.frame(x)} for arrays/tables). 
#' 
#' @param fix_row_names Boolean: Should rows be named consecutively? 
#' Default: \code{fix_row_names = TRUE}. 
#' 
#' @examples
#' # (a) from raw data (vectors):
#' ans <- sample(c("yes", "no", "maybe"), 100, replace = TRUE)
#' eat <- sample(c("fish", "meat", "veggie"), 100, replace = TRUE)
#' df_1 <- data.frame(ans, eat)  # data frame from vectors
#' df_2 <- expand_data_frame(data.frame(table(ans, eat))) # from table > contingency table > data frame
#' all.equal(table(df_1), table(df_2))
#' 
#' # (b) from array/table:
#' df <- expand_data_frame(UCBAdmissions)  # array/table > contingency table > df
#' tb <- table(df)                         # df > array/table 
#' all.equal(UCBAdmissions, tb)
#' 
#' # Trivial case:
#' expand_data_frame(data.frame(x = "a", Freq = 2))
#' 
#' # Full circle (4D array > contingency table > data frame > 4D array): 
#' df <- expand_data_frame(Titanic)
#' tb <- table(df)
#' all.equal(Titanic, tb)
#' 
#' @family array functions
#' 
#' @seealso
#' \code{\link{table}} for the inverse function;  
#' \code{\link{data.frame}} for turning an array/table into a contingency table. 
#' 
#' @export

expand_data_frame <- function(x, freq_var = "Freq", fix_row_names = TRUE){
  
  # 0. Initialize:
  df_out   <- NA
  freqs    <- NA
  ix_freqs <- NA
  
  # 1. Turn array/table into a contingency table (as df): 
  if (is.table(x)){
    
    x <- data.frame(x)  # Note: x is a contingency table
    # message("expand_data_frame: Converted x from table to data frame.")
    
  }
  
  # 2. Process df: 
  if (!is.data.frame(x)){
    
    message("expand_data_frame: x must be a data frame (or table).")
    return(NA)
    
  } else { # x is df: 
    
    # 3. Check frequency count variable:
    
    freqs <- x[[freq_var]]  # freq of cases per combination (as vector)
    # print(table(freqs))  # 4debugging
    
    if ( (!is.numeric(freqs)) | (any(freqs < 0)) | (any(is.na(as.integer(freqs)))) | (any((freqs %% 1) > 0)) ){
      
      message("expand_data_frame: freq_var must be an existing numeric variable and only contain non-negative integers.")
      return(NA)
      
    } else { # Get index of frequency count variable: 
      
      if (is.character(freq_var)){      
        
        ix_freqs <- which(names(x) == freq_var)
        
      } else if ( (is.numeric(freq_var)) & (!is.na(as.integer(freq_var))) ) {
        
        ix_freqs <- freq_var 
        
      } else {
        
        message("expand_data_frame: freq_var is not a name or numeric index of df.")
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
    
    # 9. Fix rownames:
    if (fix_row_names){
      row.names(df_out) <- 1:nrow(df_out)
    }
    
  } # if (df) else. 
  
  # 10. Output: 
  return(df_out) 
  
} # expand_data_frame(). 

# # Check:
# # (a) from raw data (vectors):
# ans <- sample(c("yes", "no", "maybe"), 100, replace = TRUE)
# eat <- sample(c("fish", "meat", "veggie"), 100, replace = TRUE)
# df_1 <- data.frame(ans, eat)  # data frame from vectors
# df_2 <- expand_data_frame(data.frame(table(ans, eat))) # from table > contingency table > data frame
# all.equal(table(df_1), table(df_2))
# 
# # (b) from array/table:
# df <- expand_data_frame(UCBAdmissions) # array/table > contingency table > df
# tb <- table(df)                        # df > array/table 
# all.equal(UCBAdmissions, tb)
# 
# # Full circle (4D array > contingency table > data frame > 4D array): 
# df <- expand_data_frame(Titanic)
# tb <- table(df)
# all.equal(Titanic, tb)
# 
# # Trivial case:
# expand_data_frame(data.frame(x = "a", Freq = 2))
# 
# # Note: Works for non-normal data frames (with positive freq count variables): 
# count <- round(runif(100, min = 0, max = 3), 0)  # count must be non-negative integers
# df <- data.frame(ans, count)
# head(df)
# ed <- expand_data_frame(df, freq_var = "count")
# ed <- expand_data_frame(df, freq_var = 2)
# dim(ed)
# head(ed)


## ToDo: ------

## - ...

## eof. ----------
