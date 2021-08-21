## trans_lists.R | i2ds
## hn | uni.kn | 2021 08 21

# Functions for transforming/manipulating lists: ------ 
#
# Note that objects of type "table" are specific cases of arrays (contingency tables with freq. counts).


# (A) Analogs of is.element/match/which for list elements: 

# Goal: Get all elements of a list that contain some element x.
#       i.e., functions analog to match/which, but for lists.


## is_list_element: Which list elements contain some x (as a logical vector): ------ 

# Goal: Verify if a list has some x as an element in EACH of its sub-lists.

# Returns a logical vector of length(list). 

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
  
  if (is.matrix(match_mx)){ # x had multiple elements:
    
    # match TRUE values of each ROW of match_mx: 
    apply(match_mx, MARGIN = 1, FUN = match, x = TRUE, nomatch = nomatch)
    
  } else { # match_mx is a logical vector:
    
    # match TRUE values of the logical vector:
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


## is_list_tag: Get the position of first list name matching a tag t: ------ 

# - get the list element position that matches a tag t

# Notes: 
# - Using match() to return position of first match!
# - Approximate name/tag matching not supported.

is_list_tag <- function(t, list, nomatch = 0L){
  
  tags <- names(list)
  
  if (is.null(tags)){ message("The list contains no names/tags.") }
  
  match(x = t, table = tags, nomatch = nomatch)
  
} # is_list_tag(). 

# # Check:
# (l <- list(one = "A", two = c("A", "B"), three = "C", c("L", "M"), "Y", six = "Z"))
# names(l)
# (n <- list("A", c("A", "B"), "C", c("L", "M"), "y", "Z"))
# names(n)
# 
# is_list_tag("two", l)
# is_list_tag("", l)  # works for (1st) un-named element
# is_list_tag("else", l, nomatch = -1)
# is_list_tag("any", n) # Note message that list contains no names/tags.
# is_list_tag("any", n, nomatch = -99)
# 
# # multiple targets in t:
# is_list_tag(c("two", "", "three"), l)  # works by yielding 1st matches (as vector)
# is_list_tag(c("t", "th", "three"), l)  # NO approximate matching!


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


## list_element_ix: Find list position for tag AND/OR values (levels): ------ 

# Goal: A utility function that finds the relevant list position matching 
# - a tag label (a list name/tag) AND/OR 
# - level values (i.e., one or more sub-list items).
# 
# Note: 
# - Multiple matches are possible when x = tag contains multiple elements! 

list_element_ix <- function(list, tag = NA, values = NA){
  
  # Determine relevant dimension/position of list element (that corresponds to current tag AND/OR value):
  
  # Initialize 3 indices:
  tag_ix    <- NA  # ix corresponding to tag
  values_ix <- NA  # ix corresponding to values 
  list_ix   <- NA  # to output
  
  org_names <- names(list)
  
  # 1. Use tag:
  if (!all(is.na(tag))){
    
    if (all(is.character(tag)) & all((nchar(tag) > 0)) ){ # (A) A tag was provided:
      
      # Get corresponding index in vector of original names (org_names):   
      # tag_ix <- which(org_names == tag)
      tag_ix <- match(x = tag, table = org_names, nomatch = NA)
      
      # Note: Multiple matches are possible when x = tag contains multiple elements!
      
      # # Detect 2 cases: 
      # if (is.na(tag_ix)){
      #  
      #  message(paste0("list_element_ix: tag = ", tag, " is no name/tag in list."))  # 4debugging
      #  # return(NA)
      #  
      # } # else {
      # # print(paste0("list_element_ix: tag = ", tag, " corresponds to element ", tag_ix, " of list."))  # 4debugging  
      # # }
      
    } # if (is.character(tag)).
    
  } # if (!is.na(tag)).
  
  # 2. Use values:
  if (!all(is.na(values))){
    
    if (is.character(values)){ # (B) Match character values:
      
      element_matches <- match_list(x = values, list = list, nomatch = 0L)  # using utility function (above)
      unique_matches  <- unique(element_matches)
      # print(unique_matches)  # 4debugging
      
      if ( !all(unique_matches == 0) & (length(unique_matches) == 1) ){  # names in values match a unique element in org_list:
        
        values_ix <- unique_matches
        # print(paste0("list_element_ix: Values correspond to element ", values_ix, " of list: name/tag = ", org_names[values_ix]))  # 4debugging
        
      } else { # no unique matching element identified:
        
        message(paste0("list_element_ix: values = ", paste(values, collapse = ", "), " correspond to no unique element of list."))
        # return(NA)
        
      }
      
    } else { # (C) Numeric levels provided: 
      
      print(paste0("list_element_ix: Not matching numeric values = ", paste(values, collapse = ", "), " to list levels (character)."))
      # return(NA)
      
    } # if (is.character(values)).
    
  } # if (!all(is.na(values))).
  
  
  # 3. Compare and use indices: 
  if (!all(is.na(tag_ix)) & is.na(values_ix)){ # only tag_ix:
    
    list_ix <- tag_ix      # use tag_ix
    
  } else if (all(is.na(tag_ix)) & !is.na(values_ix)){ # only values_ix: 
    
    list_ix <- values_ix   # use values_ix
    
  } else if (!all(is.na(tag_ix)) & !is.na(values_ix)){ # both exist:
    
    if (tag_ix == values_ix){ # verify correspondence:
      
      list_ix <- tag_ix    # use either one
      
    } else { # note conflict: 
      
      message(paste0("list_element_ix: Conflict between tag_ix = ", tag_ix, " and values_ix = ", values_ix))
      
    }
    
  }
  
  # 4. Output:
  return(list_ix)
  
} # list_element_ix().

# # Check:
# (t_list <- dimnames(Titanic))  # A test list
# names(t_list)                  # names/tags of t_list
# 
# # (a) only tag(s):
# list_element_ix(t_list, tag = "Class")
# list_element_ix(t_list, tag = "Age")
# list_element_ix(t_list, tag = "XXX")
# # Note: 
# list_element_ix(t_list, tag = c("Age", "Class"))         # Note: Multiple matches possible!
# list_element_ix(t_list, tag = c("Age", "XXX", "Class"))  # Note: Multiple matches possible!
# 
# # (b) only values:
# list_element_ix(t_list, values = "Adult")
# list_element_ix(t_list, values = "3rd")
# list_element_ix(t_list, values = "99th")
# list_element_ix(t_list, values = c("3rd", "Adult"))
# 
# # (c) tag and values:
# list_element_ix(t_list, tag = "Class", values = "3rd")    # correspondence
# list_element_ix(t_list, tag = "Class", values = "Adult")  # lack of correspondence
# list_element_ix(t_list, tag = "Age", values = "99th")     # tag wins conflict
# list_element_ix(t_list, tag = "XXX", values = "3rd")      # values win conflict
# # Note:
# list_element_ix(t_list, tag = c("Age", "Class"), values = c("3rd", "Adult"))  # tags win conflict!
# 
# # (d) numeric values only:
# list_element_ix(t_list, values = 1:4)

# # Numeric list:
# (n_list <- list(A = 1:3, B = 3:7, C = 7:9))
# list_element_ix(n_list, tag = "B", values = 2:3)
# list_element_ix(n_list, values = 4:6)


## sublist_in: Variant of sub_list_names() taking 2 arguments (in_list and out_list): ------ 

# Extract a subset of elements (dimensions OR levels) from an org_list:
# - include only elements (dimensions OR levels) specified in in_list
# - exclude any elements (dimension OR levels) specified in out_list

# A more specific and powerful version of sub_list() (above), 
# - using 2 lists of targets (in_list vs. out_list) and 
# - allowing for multiple mentions of dimensions in each (considering each element in target list in turn). 

# Note: Function assumes that org_list and in_list denote  
#       element names/tags and level/dim names of type "character" 
#       (as returned by dimnames(tbl)). 

sublist_in <- function(org_list, in_list = org_list){
  
  # Early returns:
  if (!is.list(org_list)){ message("sublist_in: org_list is not a list."); return(NA) }
  if (!is.list(in_list)) { message("sublist_in: in_list is not a list.");  return(NA) }
  if (identical(in_list, org_list)) { return(org_list) }
  
  # Goal: Extract only the name/tags/values of in_list from org_list: 
  
  # List names: 
  # org_list <- dimnames(tbl)   # Use case: org_list are dimnames of a table tbl 
  org_names <- names(org_list)  # names of org_list (as a vector)
  # if (is.null(org_names)){ print("org_list contains NO names/tags.") }  # 4debugging 
  
  in_names <- names(in_list)    # names of in_list (as a vector)
  # if (is.null(in_names)){ print("in_list contains NO names/tags.") }  # 4debugging 
  
  # Initialize: 
  n_in <- length(in_list)           # N of desired elements/dimensions
  # new_list <- org_list            # (s1) keep elements not mentioned in in_list
  # new_list <- vector("list", n_in)# (s2a) pre-allocate an empty list of length n_in
  new_list <- vector("list", 0)     # (s2b) pre-allocate an empty list of length 0
  
  for (i in 1:n_in){ # loop through in_list:
    
    # Determine 2 features of desired element:
    
    # (a) cur_tag: current name/tag (as character): 
    # cur_tag <- names(in_list[i])  # (a1) tag/name of desired list dimension
    cur_tag <- in_names[i]          # (a2) tag/name of desired list dimension    
    
    if (is.null(cur_tag)){ # fix NULL cases:
      # print("NOTE: in_list contains NO names.")  # 4debugging 
      cur_tag <- NA
    }
    # print(cur_tag)  # 4debugging
    
    # (b) cur_lev_vec: values of current level (as character/numeric vector) 
    cur_lev_vec <- in_list[[i]]  # (b) desired levels (as vector of names or numeric)
    
    # Determine relevant dimension of org_list (corresponding to current element of in_list):
    org_list_ix <- NA  # initialize
    org_list_ix <- list_element_ix(org_list, tag = cur_tag, values = cur_lev_vec)  # assign using utility function (above)
    
    if ( all(is.na(org_list_ix)) ){ # org_list_ix could not be assigned:
      
      message(paste0("sublist_in: Element ", i, " of in_list is unnamed/untagged and numeric. Using element ", i, " of org_list:"))
      org_list_ix <- i  # Heuristic: Assume same position i in org_list
      
    }
    
    # Determine original levels (at org_list_ix) and new levels (as subset/intersection):
    org_levels <- org_list[[org_list_ix]]  # original levels of element org_list_ix
    
    if (is.numeric(cur_lev_vec)){  # provided a numeric index:
      
      new_levels <- org_levels[cur_lev_vec]  # desired subset (by numeric indexing)
      # print(new_levels)  # 4debugging
      # NOTE: Non-existing index values yield and keep NA values. 
      new_levels <- new_levels[!is.na(new_levels)]  # remove NA values
      # NOTE: Non-existing levels are dropped!
      
    } else { # cur_lev_vec is NOT numeric: use provided names: 
      
      # new_levels <- cur_lev_vec  # 1: Copy desired subset of levels
      # new_levels <- intersect(org_levels, cur_lev_vec)  # 2a: Prioritize org_levels and drop any non-existing levels!
      new_levels <- intersect(cur_lev_vec, org_levels)  # 2b: Prioritize org_levels and drop any non-existing levels!      
      # NOTE: Non-existing levels are dropped!
      
    }
    
    # Populate new_list by new_levels:
    
    # (s1) Reduce the levels of (org_list_ix-th element) new_list to desired subset: 
    # new_list[[org_list_ix]] <- new_levels  
    # print(new_list)[[org_list_ix]]  # 4debugging  
    
    # (s2) Add elements to new_list:
    new_list[[i]] <- new_levels  # ADD list element  
    names(new_list)[i] <- org_names[org_list_ix]  # Assign corresponding name/tag of org_list to list element
    
    # } # if (cur_tag).
    
  } # loop end. 
  
  
  # Output:
  return(new_list) 
  
} # sublist_in(). 

# # Check:
# (t_list <- dimnames(Titanic))  # A test list (with dimension and level names)
# names(t_list)  # names of t_list
# 
# # Trivial case:
# sublist_in(t_list)  # returns original t_list
# 
# ## (A) Working for:
# # # in_list provides dimnames (dropping non-existent levels):
# sublist_in(t_list, in_list = list(Age = "Adult", Class = c("3rd", "1st", "stuff")))
# 
# # in_list provides a mix of names/tags and no names/tags (dropping non-existent levels):
# sublist_in(t_list, in_list = list("Adult", Class = c("3rd", "1st", "stuff")))
# sublist_in(t_list, in_list = list("Adult", Class = c(3, 1, 99)))
# 
# # in_list provides NO names: Note: If levels are identified, names of org_list are used:
# sublist_in(t_list, in_list = list("Adult", c("3rd", "1st")))
# 
# # in_list provides names/tags and numeric levels:
# sublist_in(t_list, in_list = list(Age = 2, Class = c(3, 1)))
# 
# # in_list provides a mix of numeric levels and named levels:
# sublist_in(t_list, in_list = list("Adult", Class = c(3, 1)))
# sublist_in(t_list, in_list = list(c(3, 1), "Adult"))
# sublist_in(t_list, in_list = list(c(3, 1), "Female", 2, "Yes"))
# 
# # in_list provides no names/tags and only numeric levels:
# sublist_in(t_list, in_list = list(c(1, 3), 2, 2, 2))
# sublist_in(t_list, in_list = list(c(1, 3), 2))
# sublist_in(t_list, in_list = list(c(3, 1), NA, 2))  # Note missing level
# sublist_in(t_list, in_list = list(c(3, 1), 99, 2))  # Note missing level
# 
# ## (B) Returning partial results/NA for:
# 
# # NA if no names/tags provided and a level cannot be identified:
# sublist_in(t_list, in_list = list("Adult", c("3rd", "1st", "stuff")))  # Class levels not recognized
# 
# # NA for providing a non-existing tags/names (also: no approximate/partial matching):
# sublist_in(t_list, in_list = list(Age = "Adult", Clas = c("3rd", "1st")))
# 
# # NA for providing no dimname and a non-existent level combination:
# sublist_in(t_list, in_list = list("Adult", c("3rd", "1st", "stuff")))
# 
# # NA for providing existing dim-name/tag as a level:
# sublist_in(t_list, in_list = list("Adult", "Class"))
# 
# 
# ## Handling NA inputs:
# sublist_in(t_list, in_list = NA)
# sublist_in(t_list, in_list = c(NA, NA))
# sublist_in(t_list, in_list = list(NA, NA))


# ToDo: 
# - Simplify function (e.g., by making more conservative/strict)
# - Analog version: out_list uses all, but excludes all mentioned dimensions/levels


## ToDo: ------

# - match for lists: in which sublist is an element (e.g., name).

# - sublist() for lists: with 2 arguments to include and exclude elements of lists in org_list
#                        to use in a subtable() function.

# - write a function that removes empty list elements (i.e., elements with no values/vector)

## eof. ----------
