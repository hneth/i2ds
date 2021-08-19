## trans_lists.R | i2ds
## hn | uni.kn | 2021 08 19

# Functions for transforming/manipulating lists: ------ 
#
# Note that objects of type "table" are specific cases of arrays (contingency tables with freq. counts).


# (A) Analogs of is.element/match/which for list elements: 

# Goal: Get all elements of a list that contain some element x.
#       i.e., functions analog to match/which, but for lists.


## is_list_element: Which list elements contain some x (as a logical vector): ------ 

# Note: If x has multiple elements (as a vector), 
#       is_list_element() returns a matrix, 
#       with each row being a logical vector of element matches for each item of x.

is_list_element <- function(x, list){
  
  sapply(list, function(elem) is.element(x, elem))
  
} # is_list_element(). 

# # Check:
# # (a) vector:
# (v <- c("A", c("A", "B"), c("A", "B", "C"), c("Z")))
# x  <- c("B")
# x3 <- c("B", "M", "Z")
# is.element(x, v)
# is.element(x3, v)  # vector of 1st match positions for each element of x2
# 
# # # # For list:
# (l <- list("A", c("A", "B"), c("A", "B", "C"), c("Z")))
# # is.element(x, l)  # Note: fails for lists!
# 
# is_list_element(x, l)
# is_list_element(x3, l)  # returns a matrix, with each row corresponding to an element of x


## match_list: Get the positions of first matches of x in (elements of) list: ------ 

# match returns a vector of the positions of (first) matches of (elements of) x in (elements of) list

# Assuming that 
# - x is a vector of targets 
# - list is a list of elements 

# Note: If x has multiple elements: Return a vector of first matches for each element of x.

match_list <- function(x, list, nomatch = 0L){
  
  # All element matches (as matrix, if x has multiple elements):
  match_mx <- is_list_element(x, list)
  
  if (is.matrix(match_mx)){
    
    # match TRUE values of each row of match_mx: 
    apply(match_mx, MARGIN = 1, FUN = match, x = TRUE, nomatch = nomatch)
    
  } else { # match_mx is a logical vector:
    
    # match TRUE values of logical vector:
    match(x = TRUE, table = match_mx, nomatch = 0L)
    
  }
  
} # match_list(). 

# # Check:
# # (a) vector:
# (v <- c("A", c("A", "B"), c("A", "B", "C"), c("Z")))
# x  <- c("B")
# x3 <- c("B", "M", "Z")
# match(x, v)
# match(x3, v, nomatch = -1)  # vector of 1st match positions for each element of x3
# 
# # (b) list:
# (l <- list("A", c("A", "B"), c("A", "B", "C"), c("Z")))
# match(x, l)  # Note: fails for lists
# match_list(x, l)
# match_list("C", l)
# match_list(x3, l, nomatch = -1)

# Use case (for sub_list() function below): 
# When looking for specific targets in list elements:
# - In which list element(s) does each target appear?
# - Are they uniquely appearing in ONE list element?
# Example: 
# l2 <- list("A", c("A", "B"), c("L", "M", "N"), c("Z"))
# unique(match_list(c("A", "B"), l2))  # 2 targets appear in multiple list elements
# unique(match_list(c("N", "L"), l2))  # 2 targets appear in SAME list element


## which_list: Get position of ALL matches of (elements of) x in (elements of) list: ------ 

# Assuming that 
# - x is a vector of targets 
# - list is a list of elements 

# Note: If x has multiple elements, which_list returns a LIST 
#       of all element positions in list that contain each element of x.
# However, which_list() currently does NOT allow providing an expression to verify (as which() does). 

which_list <- function(x, list){
  
  which(is_list_element(x, list))
  
  # Get all element matches (as matrix, each row corresponding to an element of x):
  match_mx <- is_list_element(x, list)
  
  if (is.matrix(match_mx)){  
    
    # apply which to each row of match_mx: 
    apply(match_mx, MARGIN = 1, FUN = which)
    
  } else { # match_mx is a logical vector:
    
    # which values of logical vector are TRUE:
    which(match_mx == TRUE)
    
  }
  
}

# # Check:
# # (a) vector:
# (v <- c("A", c("A", "B"), c("A", "B", "C"), c("Z")))
# x  <- c("B")
# x3 <- c("B", "M", "Z")
# match(x, v)
# match(x3, v, nomatch = -1)  # vector of 1st match positions for each element of x3
# 
# # (b) list:
# (l <- list("A", c("A", "B"), c("A", "B", "C"), c("Z")))
# which(x == l)  # Note: fails for lists
# which_list(x, l)
# which_list("C", l)
# which_list(x3, l)


## sub_list_names: Utility function to get the names of a name_list specified in dim_list: -----

# A simpler variant of sub_table_names that does not require a table tbl, 
# but only uses 2 lists as arguments:  
# - name_list is the list of original names (that is to be reduced) 
# - dim_list is a list that specifies a subset of name_list (by name or numeric indices)
#
# Currently used, but not exported. 

sub_list_names <- function(name_list, dim_list){
  
  # Initialize:
  # name_list <- dimnames(tbl)  # original list of dimnames
  new_name_list <- name_list    # initialize a new list (to be reduced)
  n_dim <- length(dim_list)     # N of desired dimensions
  
  # Verify correspondence of list lengths:
  if (n_dim < length(name_list)){  # Notify user: 
    message("sub_list_names: dim_list is shorter than name_list. Using elements of name_list in turn:")
  }
  
  if (n_dim > length(name_list)){  # Notify user: 
    message("sub_list_names: dim_list is longer than name_list. Truncating to same length:")
    n_dim <- length(name_list)
  }
  
  # Main: 
  for (i in 1:n_dim){  # Consider 1:n_dim elements of name_list(!) in turn:
    
    # org_dim_name <- names(name_list[i])
    # cur_dim_name <- names(dim_list[i])
    
    # if ( (is.null(cur_dim_name)) | (cur_dim_name == org_dim_name) ){
    
    cur_lev_vec <- dim_list[[i]]  # extract current levels (element on dim_list)
    
    if (is.numeric(cur_lev_vec)){  # provided a numeric index:
      
      cur_lev_names <- name_list[[i]]  # all names of current levels
      
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
  
} # sub_list_names(). 

# # Check:
# org_list <- dimnames(Titanic)
# 
# # A purely numeric index as dim_list:
# sub_list_names(org_list, dim_list = list(c(1, 3), 2, 2, 2))
# 
# # A mix of names and numeric index:
# sub_list_names(org_list, dim_list = list(c(1, 3), "Female", 2, "Yes"))
# 
# # A Warning: If dim_list has fewer OR more elements than dimensions:
# sub_list_names(org_list, dim_list = list(c(1, 3), "Female", 1))
# sub_list_names(org_list, dim_list = list(c(1, 3), "Female", 1, 2, 99))
# 
# # Note some features:
# # - Dimensions are considered in the order provided in dim_list.
# # - The names of additional dimensions (without names or numbers in dim_list) are fully included.
# # - Additional/extra dim_list arguments are truncated. 


## sub_list_2: Variant of sub_list_names() with in_list and out_list: ------ 

# A more specific and powerful version, 
# - using 2 lists of targets (in_list vs. out_list) and 
# - allowing for multiple mentions of dimensions in each (considering each element in target list in turn). 

sub_list_2 <- function(org_list, in_list){
  
  # Initialize:
  # org_list <- dimnames(tbl)  # original list of dimnames
  org_dim_names <- names(org_list)  # names of org_list (as a vector)
  new_name_list <- org_list    # initialize a new list (to be reduced)
  n_in <- length(in_list)      # N of desired dimensions
  
  # Verify correspondence of list lengths:
  if (n_in < length(org_list)){  # Notify user: 
    message("sub_list_2: in_list is shorter than org_list. Using elements of in_list in turn:")
  }
  
  if (n_in > length(org_list)){  # Notify user: 
    message("sub_list_2: in_list is longer than org_list. Using elements of in_list in turn:")
    # n_in <- length(org_list)
  }
  
  # Main: 
  for (i in 1:n_in){ # Consider each element of in_list:
    
    cur_dim_name <- names(in_list[i])  # name of desired dimension
    print(cur_dim_name)  # 4debugging
    
    cur_lev_vec  <- in_list[[i]]  # desired levels (as names or numeric)
    
    # Get corresponding index in original names (org_dim_names): 
    if (is.character(cur_dim_name) & (nchar(cur_dim_name) > 0)){
      
      org_name_idx <- which(org_dim_names == cur_dim_name)
      print(paste0("cur_dim_name = ", cur_dim_name, " is element ", org_name_idx, " of org_list."))  # 4debugging
      
    } else { # no dim name provided: 
      
      # +++ here now +++:
      
      if (is.character(cur_lev_vec)){
        
        element_matches <- match_list(x = cur_lev_vec, list = org_list)  # using utility function (above)
        
        if (length(unique(element_matches)) == 1){  # names in cur_lev_vec match 1 unique element in org_list:
          
          org_name_idx <- unique(element_matches)
          print(paste0("Levels of element ", i, " correspond to element ", org_name_idx, " of org_list."))  # 4debugging
          
        } else { # no unique element identified:
          
          message("sub_list_2: Specified levels do not correspond to a unique element of org_list.")
          return(NA)
          
        }
        
      } else {
        
        message(paste0("Element ", i, " of in_list is unnamed and numeric. Using element ", i, " of org_list:"))
        org_name_idx <- i
        
      }
    }
    
    # Levels:
    org_lev_names <- org_list[[org_name_idx]]  # all names of original levels
    
    if (is.numeric(cur_lev_vec)){  # provided a numeric index:
      
      sub_org_lev_names <- org_lev_names[cur_lev_vec]  # desired subset
      # print(sub_org_lev_names)  # 4debugging
      
    } else { # cur_lev_vec is NOT numeric: use names as provided
      
      # sub_org_lev_names <- cur_lev_vec  # Option 1: Copy desired subset of levels
      sub_org_lev_names <- intersect(org_lev_names, cur_lev_vec)  # Option 2: Drop non-existing levels!
      
    }
    
    # Reduce levels of (org_name_idx-th element) new_name_list to desired subset: 
    new_name_list[[org_name_idx]] <- sub_org_lev_names  
    # print(new_name_list)[[org_name_idx]]  # 4debugging  
    
    # } # if (cur_dim_name).
    
  } # loop end. 
  
  # Output:
  return(new_name_list) 
  
} # sub_list_2(). 

# # Check:
# (t_list <- dimnames(Titanic))
# names(t_list)
# # as.data.frame(t_list)
# 
# sub_list_2(t_list, in_list = list(Age = "Adult", Class = c("3rd", "1st", "stuff")))
# 
# sub_list_2(t_list, in_list = list("Adult", Class = c("3rd", "1st", "stuff")))

# +++ here now +++ 

# A purely numeric index as dim_list (without dim names):
# sub_list_2(org_list, in_list = list(c(1, 3), 2, 2, 2))

# A mix of names and numeric index:
# sub_list_names(org_list, dim_list = list(c(1, 3), "Female", 2, "Yes"))

# A Warning: If dim_list has fewer OR more elements than dimensions:
# sub_list_names(org_list, dim_list = list(c(1, 3), "Female", 1))
# sub_list_names(org_list, dim_list = list(c(1, 3), "Female", 1, 2, 99))
# 
# # Note some features:
# # - Dimensions are considered in the order provided in dim_list.
# # - The names of additional dimensions (without names or numbers in dim_list) are fully included.
# # - Additional/extra dim_list arguments are truncated. 





## ToDo: ------

# - match for lists: in which sublist is an element (e.g., name)?
# - sublist for lists: include and exclude elements of lists in org_list
#                      to use in a subtable() function.


## eof. ----------
