## trans_lists.R | i2ds
## hn | uni.kn | 2021 08 28

# Functions for transforming/manipulating lists: ------ 
#
# Note that objects of type "table" are specific cases of arrays (contingency tables with freq. counts).


## is_empty_list: Check if some x is an empty list (i.e., x is both a list and empty): ------

is_empty_list <- function(x){
  
  is.list(x) & (length(x) == 0)
  
} # is_empty_list().

# # Check:
# is_empty_list(list())
# is_empty_list("")
# is_empty_list(NA)
# is_empty_list(NULL)


# (A) Analogs of is.element/match/which for list elements: 

# Goal: Get all elements of a list that contain some element x.
#       i.e., functions analog to match/which, but for lists.

## drop_empty_list_elements: Removes empty list elements of ls (i.e., elements with NULL values/empty vector/empty list): ------ 

drop_empty_list_elements <- function(ls){
  
  if (!is.list(ls)){
    
    message("drop_empty_list_elements: ls is no list.")
    return(NA)
    
  } else {
    
    e_len <- sapply(ls, FUN = length)
    
    ls[e_len == 0] <- NULL
    
    return(ls)
    
  }
  
} # drop_empty_list_elements(). 

# # Check:
# ls <- list(a = 1, b = "", x = NULL, c = NA, y = list(), z = vector())
# 
# drop_empty_list_elements(ls)
# drop_empty_list_elements(vector())
# drop_empty_list_elements(list())



## is_list_element: Which list elements of ls contain some x (as a logical vector): ------ 

# Goal: Check/verify if a list ls has some x as an element for EACH of its sub-lists.

# Returns a logical vector of length(ls). 

# Note: If x has multiple elements (as a vector), 
#       is_list_element() returns a matrix, 
#       with each row being a logical vector of element matches for each item of x.

is_list_element <- function(x, ls){
  
  sapply(ls, function(elem) is.element(x, elem))
  
} # is_list_element(). 

# # Check:
# # (a) vector:
# (v <- c("A", c("A", "B"), c("A", "B", "C"), c("Z")))
# x  <- c("B")
# x3 <- c("B", "M", "Z")
# is.element(x, v)
# is.element(x3, v)  # vector of 1st match positions for each element of x2
# 
# # (b) list:
# (l <- list("A", c("A", "B"), c("A", "B", "C"), c("Z")))
# # is.element(x, l)  # Note: fails for lists!
# 
# is_list_element(x, l)
# is_list_element(x3, l)  # returns a matrix, with each row corresponding to an element of x
# 
# # numeric list: 
# (nl <- list(1:3, 3:7, 7:9))
# is_list_element(7, nl)
# 
# # mixed list:
# (ml <- list(letters[1:3], 3:7, letters[7:3], 7:9))
# is_list_element(7, ml)
# is_list_element("c", ml)


## match_list: Get the positions of first matches of x in (elements of) list: ------ 

# match returns a vector of the positions of (first) matches of (elements of) x in (elements of) a list ls. 

# Assuming that 
# - x is a vector of targets 
# - list is a list of elements 

# Note: If x has multiple elements: Return a vector of first matches for each element of x.

match_list <- function(x, ls, nomatch = 0L){
  
  # All element matches (as matrix, if x has multiple elements):
  match_mx <- is_list_element(x, ls)
  
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
# 
# # numeric list:
# (nl <- list(1:3, 3:7, 7:9))
# match_list(7, nl)
# match_list("X", nl)
# 
# # mixed list:
# (ml <- list(letters[1:3], 3:7, letters[7:3], 7:9))
# match_list(7, ml)
# match_list("f", ml)
# match_list("X", ml)

# Use case (for sub_list() function below): 
# When looking for specific targets in list elements:
# - In which list element(s) does each target appear?
# - Are they uniquely appearing in ONE list element?
# Example: 
# l2 <- list("A", c("A", "B"), c("L", "M", "N"), c("Z"))
# unique(match_list(c("A", "B"), l2))  # 2 targets appear in multiple list elements
# unique(match_list(c("N", "L"), l2))  # 2 targets appear in SAME list element


## which_list: Get position of ALL matches of (elements of) x in (elements of) list ls: ------ 

# Assuming that 
# - x is a vector of targets 
# - list is a list of elements 

# Note: If x has multiple elements, which_list returns a LIST 
#       of all element positions in list that contain each element of x.
# However, which_list() currently does NOT allow providing an expression to verify (as which() does). 

which_list <- function(x, ls){
  
  which(is_list_element(x, ls))
  
  # Get all element matches (as matrix, each row corresponding to an element of x):
  match_mx <- is_list_element(x, list)
  
  if (is.matrix(match_mx)){  
    
    # apply which to each row of match_mx: 
    apply(match_mx, MARGIN = 1, FUN = which)
    
  } else { # match_mx is a logical vector:
    
    # which values of logical vector are TRUE:
    which(match_mx == TRUE)
    
  }
  
} # which_list(). 

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
# 
# # numeric list:
# (nl <- list(1:3, 3:7, 7:9))
# which_list(7, nl)
# which_list("X", nl)
# which_list(c(2, 5), nl)  # multiple targets: unique matches
# which_list(c(2, 7), nl)  # multiple targets: multiple matches
# 
# # mixed list:
# (ml <- list(letters[1:3], 3:7, letters[7:3], 7:9))
# which_list(7, ml)
# which_list("c", ml)
# which_list("X", ml)



## match_list_tag: Get the position of first list name in a list ls matching a tag: ------ 

# Goal: Get the list element position of list that matches a name/tag tag. 

# Notes: 
# - Using match() to return position of first match!
# - Approximate name/tag matching not supported.

match_list_tag <- function(tag, ls, nomatch = 0L){
  
  l_tags <- names(ls)
  
  if (is.null(l_tags)){ message("The list ls contains no names/tags.") }
  
  match(x = tag, table = l_tags, nomatch = nomatch)
  
} # match_list_tag(). 

# # Check:
# (l <- list(one = "A", two = c("A", "B"), three = "C", c("L", "M"), "Y", six = "Z"))
# names(l)
# (n <- list("A", c("A", "B"), "C", c("L", "M"), "y", "Z"))
# names(n)
# 
# match_list_tag("two", l)
# match_list_tag("", l)  # works for (1st) un-named element
# match_list_tag("else", l, nomatch = -1)
# match_list_tag("any", n)  # Note: Returns nomatch value + message.
# match_list_tag("any", n, nomatch = -99)
# 
# # multiple targets in t:
# match_list_tag(c("two", "", "three"), l)  # works by yielding 1st matches (as vector)
# match_list_tag(c("t", "th", "three"), l)  # NO approximate matching!
# 
# # numeric list:
# (nl <- list(a = 1:3, b = 3:7, c = 7:9))
# match_list_tag("c", nl)
# match_list_tag("X", nl)
# 
# # mixed list, some tags:
# (ml <- list(a1 = letters[1:3], n1 = 3:7, letters[7:3], 7:9))
# match_list_tag("n1", ml)
# match_list_tag(c("n1", "n9"), ml)
# match_list_tag("XX", ml)



## sublist_names: Utility function to get the names of a name_list specified in sub_list: -----

# A simpler variant of sub_table_names that does not require a table tbl, 
# and uses 2 lists as arguments:  
# - name_list is the list of original names (that is to be reduced) 
# - sub_list is a list that specifies a subset of name_list (by name or numeric indices)
#
# Currently used, but not exported. 

sublist_names <- function(name_list, sub_list){
  
  # Initialize:
  # name_list <- dimnames(tbl)  # original list of dimnames
  new_name_list <- name_list    # initialize a new list (to be reduced)
  n_sub <- length(sub_list)     # N of desired dimensions
  
  # Verify correspondence of list lengths:
  if (n_sub < length(name_list)){  # Notify user: 
    message("sublist_names: sub_list is shorter than name_list. Using elements of name_list in turn:")
  }
  
  if (n_sub > length(name_list)){  # Notify user: 
    message("sublist_names: sub_list is longer than name_list. Truncating to same length:")
    n_sub <- length(name_list)
  }
  
  # Main: 
  for (i in 1:n_sub){  # Consider each elements of sub_list(!) in turn:
    
    # org_dim_name <- names(name_list[i])
    # cur_dim_name <- names(sub_list[i])
    
    # if ( (is.null(cur_dim_name)) | (cur_dim_name == org_dim_name) ){
    
    cur_lev_vec <- sub_list[[i]]  # extract current levels (element on sub_list)
    
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
  
} # sublist_names(). 

# # Check:
# ls <- dimnames(Titanic)
# 
# # A purely numeric index as sub_list:
# sublist_names(ls, sub_list = list(2, 2, 2, Class = 1))  # Note: tags are ignored!
# sublist_names(ls, sub_list = list(c(1, 3), 2, 2, 2))
# 
# # A mix of names and numeric index:
# sublist_names(ls, sub_list = list(c(1, 3), "Female", 2, "Yes"))
# 
# # A Warning: If sub_list has fewer OR more elements than dimensions:
# sublist_names(ls, sub_list = list(c(1, 3), "Female", 1))
# sublist_names(ls, sub_list = list(c(1, 3), "Female", 1, 2, 99))
# 
# # Note some features:
# # - Any names/tags are ignored (i.e., assuming that sub_list elements are in SAME order as name_list).
# #   Dimensions of name_list are considered in the order provided in sub_list.
# # - The names of additional dimensions (without names or numbers in sub_list) are included in full.
# # - Additional/extra sub_list arguments are truncated. 


## list_element_ix: Find a list position for tag AND/OR values (levels): ------ 

# Goal: A utility function that finds the relevant position in a list ls matching 
# - a tag label (a list name/tag, as character) AND/OR 
# - level values (i.e., one or more sub-list items, as vector).
# 
# Note: 
# - Multiple matches are possible when x = tag contains multiple elements! 

list_element_ix <- function(ls, tag = NULL, values = NULL, quiet = FALSE){
  
  # Determine relevant dimension/position of a list's ls element (that corresponds to current name/tag AND/OR value):
  
  # Inputs: ---- 
  if (!is.list(ls)){ message("list_element_ix: list ls is not a list."); return(NA) }
  
  if (is.null(tag) & is.null(values)){
    message("list_element_ix: Neither tag nor value specified.")
    return(NA)
  }
  
  # Initialize 3 indices:
  tag_ix    <- NA  # sub-ix corresponding to tag
  values_ix <- NA  # sub-ix corresponding to values 
  ls_ix <- NA      # (output)
  
  org_names <- names(ls)
  
  # 1. Find tag: ---- 
  # if (!all(is.na(tag))){
  if (!is.null(tag)){
    
    if (all(is.character(tag)) & all((nchar(tag) > 0)) ){ # (A) A tag was provided:
      
      # Get corresponding index in vector of original names (org_names):   
      tag_ix <- match(x = tag, table = org_names, nomatch = NA)
      # Note: Multiple matches are possible when x = tag contains multiple elements!
      
      if (any(is.na(tag_ix))){
        
        if (!quiet) { message("list_element_ix: Some tag not found in list.") }
        
      }
      
    } else {
      
      if (!quiet) { message("list_element_ix: tag must contain non-empty characters.") }
      
    } # if (is.character(tag)).
    
  } # if (!is.null(tag)).
  
  
  # 2. Find values: ---- 
  if (!is.null(values)){
    
    if (!is.vector(values, mode = "any") | is.list(values)){
      
      if (!quiet) { message("list_element_ix: values must be a vector and no list.") }
      
    } else {
      
      # Count matches of values in each element of list ls: 
      n_elements <- length(ls)
      n_values   <- length(values)
      n_matches  <- rep(NA, n_elements)  # count matches per element
      
      for (i in 1:n_elements){  # for each element of list ls:
        
        n_matches[i] <- sum(values %in% ls[[i]])  # total number of matches
        
      }
      
      # Have ALL values been matched in the same element?
      allmatch_ix <- which(n_matches == n_values)  # positions with correspondence 
      # print(allmatch_ix)  # 4debugging 
      
      if (length(allmatch_ix) == 0){
        
        if (!quiet) { message(paste0("list_element_ix: No element of ls contains all given values.")) }
        
      } else if (length(allmatch_ix) == 1){
        
        # print(paste0("list_element_ix: Exactly 1 element of ls contains all values = ", allmatch_ix, ".")) # 4debugging 
        values_ix <- allmatch_ix
        
      } else { # (length(allmatch_ix) > 1): 
        
        if (!quiet) { message(paste0("list_element_ix: Multiple elements of ls contain all values: ", 
                                     paste(allmatch_ix, collapse = ", "), ".")) }  
        
      }
      
    }
    
  } # if (!is.null(values)).
  
  # 3. Compare and use indices: ----  
  if (!all(is.na(tag_ix)) & is.na(values_ix)){ # only tag_ix:
    
    ls_ix <- tag_ix      # use tag_ix
    
  } else if (all(is.na(tag_ix)) & !is.na(values_ix)){ # only values_ix: 
    
    ls_ix <- values_ix   # use values_ix
    
  } else if (!all(is.na(tag_ix)) & !is.na(values_ix)){ # both exist:
    
    if (tag_ix == values_ix){ # verify correspondence:
      
      ls_ix <- tag_ix    # use either one
      
    } else { # note conflict: 
      
      if (!quiet) { message(paste0("list_element_ix: Conflict between tag_ix = ", tag_ix, " and values_ix = ", values_ix)) }
      
    }
    
  }
  
  # 4. Output:
  return(ls_ix)
  
} # list_element_ix().

# # Check:
# ## (A) Character/names/tags and levels:
# (t_nm <- dimnames(Titanic))  # A test list
# names(t_nm)                  # names/tags of t_nm
# 
# # (a) only tag(s):
# list_element_ix(t_nm, tag = "Class")
# list_element_ix(t_nm, tag = "Age")
# list_element_ix(t_nm, tag = "XXX")
# # Note:
# list_element_ix(t_nm, tag = c("Age", "Class"))         # Note: Multiple matches possible!
# list_element_ix(t_nm, tag = c("Age", "XXX", "Class"))  # Note: Multiple matches possible!
# 
# # (b) only values:
# list_element_ix(t_nm, values = "Adult")
# list_element_ix(t_nm, values = "3rd")
# list_element_ix(t_nm, values = "99th")             # non-existent level
# list_element_ix(t_nm, values = c("3rd", "Adult"))  # non-existent combination
# 
# # (c) tag and values:
# list_element_ix(t_nm, tag = "Class", values = "3rd")    # correspondence
# list_element_ix(t_nm, tag = "Class", values = "Adult")  # lack of correspondence
# list_element_ix(t_nm, tag = "Age", values = "99th")     # tag wins conflict
# list_element_ix(t_nm, tag = "XXX", values = "3rd")      # values win conflict
# # Note:
# list_element_ix(t_nm, tag = c("Age", "Class"), values = c("3rd", "Adult"))  # tags win conflict!
#
# # (d) Numeric values only:
# list_element_ix(t_nm, values = 1:4)  # not matched in current list
#
# # Note: Repeated entries:
# list_element_ix(t_nm, tag = c("Age", "Age")) 
# list_element_ix(t_nm, values = c("2nd", "1st", "2nd"))  # Note: Elements not count twice. 
#
# # Trivial cases:
# list_element_ix(t_nm)
# list_element_ix(t_nm, tag = 3)
# list_element_ix(t_nm, values = list(a = 1:3))
# 
# 
# ## (B) Numeric list levels:
# (nl <- list(A = 1:3, B = 3:5, C = 2:4))
# list_element_ix(nl, tag = "B")
# list_element_ix(nl, tag = "B", values = c(5, 3))
# list_element_ix(nl, values = 5)
# list_element_ix(nl, values = 5:3)
# 
# # works with message:
# list_element_ix(nl, tag = c("C", "X", "A"))
# list_element_ix(nl, tag = "X", values = 5)
# list_element_ix(nl, tag = "B", values = c(4, 3))
# 
# # fails with message:
# list_element_ix(nl, tag = c("X"))
# list_element_ix(nl, tag = "B", values = c(4, 2))
# list_element_ix(nl, values = c(3, 99))
# list_element_ix(nl, values = c(4, 3))
# list_element_ix(nl, values = 4)
# list_element_ix(nl, values = 2:3)
# 
# # list with NA values:
# (na <- list(a = c(1, 3), b = c(3, NA, 5), c = c(6, NA, 7, NA)))  
# 
# list_element_ix(na, values = c(3, 1))
# list_element_ix(na, values = c(5, 3))
# list_element_ix(na, values = c(NA, 5))
# list_element_ix(na, values = c(NA, 3))
# list_element_ix(na, values = c(NA, 1))
# list_element_ix(na, values = NA)
# list_element_ix(na, values = c(NA, NA))  # Note: Elements not count twice. 
#
# ## (C) Mixed list levels, some tags:
# (ml <- list(a1 = letters[1:3], n1 = 3:7, a2 = letters[7:3], 7:9, tf = c(TRUE, FALSE, TRUE)))
# list_element_ix(ml, tag = "a2", values = 4:5)
# list_element_ix(ml, tag = "XX", values = 4:5)
# list_element_ix(ml, tag = c("a1", "XX", "a2"))
# list_element_ix(ml, tag = "XX", values = 7:6)
# list_element_ix(ml, tag = "XX", values = 5)
# list_element_ix(ml, tag = "XX", values = 7)
# list_element_ix(ml, values = c(TRUE, FALSE, FALSE))



## sublist: Extract a sublist (or subset) of a list ls, using 2 arguments (in_list and out_list): ------ 

# A more general and powerful version of sublist_names() (above): 

# Extract a subset of elements (dimensions OR levels) from a list ls:
# - include only elements (dimensions OR levels) specified in in_list
# - exclude any elements (dimension OR levels) specified in out_list 
# - allowing for multiple mentions of dimensions in each (considering each element in target list in turn). 

# Note: sublist() originally assumed that list ls and in_list denote element names/tags and level/dim names 
#       (e.g., as returned by dimnames(tbl)), but now also allows for other data modes/types. 


#' Extract a sublist (or subset) of a list. 
#' 
#' \code{sublist} yields a sublist of a list \code{ls}
#' by filtering or extracting a subset of the list's 
#' dimensions and levels. 
#' 
#' \code{sublist} provides a filter/slice function for lists, by specifying 
#' a positive subset \code{in_list} and a negative subset \code{out_list} 
#' (both as lists, in tag and value format). 
#' 
#' @return A list. 
#' 
#' @param ls The original list (as \code{\link{list}}). 
#' 
#' @param in_list A list specifying the tag and value(s) of \code{ls} to keep/include. 
#' Default: \code{in_list = ls} (i.e., everything). 
#' 
#' @param out_list A list specifying the tag and value(s) of \code{ls} to drop/exclude. 
#' Default: \code{out_list = NULL} (i.e., nothing). 
#' 
#' @param quiet Boolean: Hide feedback messages?
#' Default: \code{quiet = FALSE} (i.e., show messages). 
#' 
#' @examples 
#' (ls <- list(n = 1:4, l = letters[1:4]))
#' 
#' # Trivial cases:
#' sublist(ls, in_list = ls)
#' sublist(ls, out_list = ls)
#' 
#' # (a) in_list only:
#' sublist(ls,  in_list = list(l = letters[c(2, 4)], n = c(2, 4)))
#' sublist(ls,  in_list = list(n = 4))
#' 
#' # NA/empty cases:
#' sublist(ls,  in_list = NA)      # returns NA
#' sublist(ls,  in_list = list())  # returns an empty list
#' 
#' # Note:
#' sublist(ls,  in_list = list(l = 4, n = 4))      # matching list elements and levels
#' sublist(ls,  in_list = list(l = 99, n = 99))    # non-existent levels: nothing in
#' sublist(ls,  in_list = list(ll = 99, nn = 99))  # heuristics match list elements
#' 
#' # (b) out_list only:
#' sublist(ls, out_list = list(l = letters[c(2, 4)], n = c(2, 4)))
#' sublist(ls, out_list = list(n = 3, l = "c"))
#' 
#' # NA/empty cases:
#' sublist(ls, out_list = NA)      # returns NA
#' sublist(ls, out_list = list())  # returns original list
#' 
#' # Note:
#' sublist(ls, out_list = list(l = 4, n = 4))      # matching list elements and levels
#' sublist(ls, out_list = list(l = 99, n = 99))    # non-existent levels: nothing out
#' sublist(ls, out_list = list(ll = 99, nn = 99))  # heuristics match list elements
#' 
#' # (c) in_list AND out_list:
#' sublist(ls, in_list = list(n = 3:4, l = c("c", "a")), out_list = list(l = "c"))
#' sublist(ls, in_list = list(n = 3:4, l = c("c", "a")), out_list = list(n = 4, l = "c"))
#' 
#' # removing everything:
#' sublist(ls, in_list = list(n = 4:3), out_list = list(n = 3:4))
#' sublist(ls, in_list = list(l = c("c", "a")), out_list = list(l = c("a", "c")))
#' sublist(ls, in_list = list(n = 3:4, l = c("a", "c")),
#'         out_list = list(n = 4:3, l = c("c", "a")))
#' 
#' # Note: Tags can be used repeatedly and empty list elements are dropped:
#' sublist(ls, out_list = list(l = c("c", "b"), n = 2:3))
#' sublist(ls, out_list = list(l = c("c", "b"), n = 2:3, l = c("a", "d")))
#' sublist(ls, out_list = list(l = c("c", "b"), n = 2:3, l = c("a", "d"), n = 1:4))
#' 
#' @family list functions
#' 
#' @seealso
#' \code{\link{subtable}} for extracting subsets of a table. 
#' 
#' @export

sublist <- function(ls, in_list = ls, out_list = NULL, quiet = FALSE){
  
  # 0. Inputs: ---- 
  if (!is.list(ls)){ message("sublist: ls is not a list."); return(NA) }
  
  if (!is.list(in_list)) { message("sublist: in_list is not a list.");  return(NA) }
  
  if (is_empty_list(in_list)){ return(vector("list", 0)) }  # return an empty list
  
  if (is.null(out_list)) {
    
    if (identical(in_list, ls)) { return(ls) }
    
  } else { # out_list has been specified:
    
    if (!is.list(out_list)) { message("sublist: out_list is not a list.");  return(NA) }
    
  }
  
  
  # 1. Use in_list: ---- 
  if ( (is.null(in_list)) | (identical(in_list, ls)) ){
    
    sub_list <- ls  # Case_A: Transfer ls to sub_list (to use out_list below)
    
  } else { # interpret in_list (to fill elements/levels of an initially empty sub_list):
    
    # Initialize: 
    sub_list <- vector("list", 0)  #  Case_B: Pre-allocate an empty list of length 0 to sub_list. 
    
    CT_list <- ls  # current target list (transfer current main list)
    CT_names <- names(CT_list)  # names of current target list (as a vector)
    # if (is.null(CT_names)){ print("CT_list contains NO names/tags.") }  # 4debugging 
    
    n_in <- length(in_list)       # N of desired elements/dimensions    
    in_names <- names(in_list)    # names of in_list (as a vector)
    # if (is.null(in_names)){ print("in_list contains NO names/tags.") }  # 4debugging 
    
    for (i in 1:n_in){ # loop through in_list:
      
      # Determine 2 features of desired/target element: name/tag and vector of levels/values
      
      # (a) cur_in_tag: current name/tag (as character): 
      cur_in_tag <- in_names[i]                     # tag/name of desired list dimension    
      if (is.null(cur_in_tag)){ cur_in_tag <- NA }  # fix NULL cases
      # print(cur_in_tag)  # 4debugging
      
      # (b) cur_in_vec: levels/values of current element of in_list (as a vector) 
      cur_in_vec <- in_list[[i]]  # (b) desired levels (as vector)
      
      # Determine relevant ix/position of CT_list (corresponding to current elements of in_list):
      CT_ix <- NA  # initialize
      CT_ix <- list_element_ix(CT_list, tag = cur_in_tag, values = cur_in_vec, quiet = quiet)  # use utility function (above)
      
      if ( all(is.na(CT_ix)) ){ # CT_ix could not be assigned:
        
        message(paste0("sublist: Element ", i, " of in_list not located in current list. Using element ", i, " of current list."))
        CT_ix <- i  # Heuristic: Assume SAME position i in CT_list (never quiet)!
        
      }
      
      # Determine current target levels (at CT_ix) and new levels (as subset/intersection):
      CT_vec <- CT_list[[CT_ix]]  # vector of original levels/element CT_ix of CT_list
      
      if ( (!is.numeric(CT_vec)) & (is.numeric(cur_in_vec)) ) { # provided a numeric index to non-numeric vector of CT_vec:
        
        if (!quiet) { 
          message(paste0("sublist: Using element ", i, " of in_list as numeric subset of element ", CT_ix, " of current list."))
        }
        
        new_vec <- CT_vec[cur_in_vec]  # desired subset (by numeric indexing)
        # NOTE: Non-existing index values yield and keep NA values.
        
        new_vec <- new_vec[!is.na(new_vec)]  # remove NA values
        # NOTE: Non-existing levels are dropped!
        
      } else { # cur_in_vec is NOT numeric or BOTH are numeric: use levels/names provided: 
        
        new_vec <- intersect(cur_in_vec, CT_vec)  # prioritize cur_in_vec and drop any non-existing levels!      
        # NOTE: Non-existing levels are dropped!
        
      }
      
      # Populate i-th element of sub_list by new_vec:
      sub_list[[i]] <- new_vec  # ADD list element  
      names(sub_list)[i] <- CT_names[CT_ix]  # Assign corresponding name/tag of CT_list to list element
      
    } # loop in_list end. 
    
  } # if (is.null(in_list)) etc.
  
  
  # Intermediate feedback:
  # print(paste0("sublist: sub_list (after processing in_list): "))  
  # print(sub_list) # 4debugging 
  
  
  # 2. Use out_list: ---- 
  if ( is.null(out_list) | is_empty_list(out_list) ) {
    
    return(sub_list)
    
  } else if (identical(out_list, sub_list)) {
    
    # print(paste0("sublist: Nothing remains, as out_list corresponds to sub_list."))  # 4debugging 
    return(vector("list", 0))  # return an empty list
    
  } else { # interpret out_list (to remove elements or levels from CT_list): 
    
    # print(paste0("sublist: Interpret a given out_list by removing from sub_list:"))  # 4debugging 
    
    # Initialize: 
    CT_list <- sub_list  # current target list (transfer current main list)
    CT_names <- names(CT_list)  # names of current target list (as a vector)
    # if (is.null(CT_names)){ print("CT_list contains NO names/tags.") }  # 4debugging 
    
    n_out <- length(out_list)           # N of desired elements/dimensions
    out_names <- names(out_list)  # names of out_list (as a vector)  
    # if (is.null(out_names)){ print("out_list contains NO names/tags.") }  # 4debugging 
    
    for (i in 1:n_out){ # loop through out_list:
      
      # Determine 2 features of desired/target element: name/tag and vector of levels/values
      
      # (a) cur_out_tag: current name/tag (as character): 
      cur_out_tag <- out_names[i]                     # tag/name of desired list dimension    
      if (is.null(cur_out_tag)){ cur_out_tag <- NA }  # fix NULL cases
      # print(cur_out_tag)  # 4debugging
      
      # (b) cur_out_vec: levels/values of current element of out_list (as a vector) 
      cur_out_vec <- out_list[[i]]  # desired/target levels (as vector)
      
      # Determine relevant ix/position of CT_list (corresponding to current elements of out_list):
      CT_ix <- NA  # initialize
      CT_ix <- list_element_ix(CT_list, tag = cur_out_tag, values = cur_out_vec, quiet = quiet)  # use utility function (above)
      
      if ( all(is.na(CT_ix)) ){ # CT_ix could not be assigned:
        
        message(paste0("sublist: Element ", i, " of out_list not located in current list. Using element ", i, " of current list."))
        CT_ix <- i  # Heuristic: Assume SAME position i in CT_list (never quiet)!
        
      }
      
      # Determine current target levels (at CT_ix) and new levels (as subset/setdiff):
      CT_vec <- CT_list[[CT_ix]]  # vector of levels/element CT_ix of CT_list
      
      if ( (!is.numeric(CT_vec)) & (is.numeric(cur_out_vec)) ) { # provided a numeric index to non-numeric vector of CT_vec:
        
        if (!quiet) { 
          message(paste0("sublist: Using element ", i, " of out_list as numeric subset of element ", CT_ix, " of current list."))
        }
        
        new_vec <- CT_vec[-cur_out_vec]  # reduced subset (by numeric indexing)
        # NOTE: Non-existing index values yield and keep NA values.
        
        new_vec <- new_vec[!is.na(new_vec)]  # remove NA values
        # NOTE: Non-existing levels are dropped!
        
      } else { # cur_out_vec is NOT numeric or BOTH are numeric: use levels/names provided: 
        
        new_vec <- setdiff(CT_vec, cur_out_vec)  # prioritize CT_vec and drop any non-existing levels!
        # NOTE: Non-existing levels are dropped!
        
      }
      
      # print(new_vec)  # 4debugging
      
      # CHANGE CT_ix-th element of CT_list to new_vec:
      if ( !all(is.na(new_vec)) ) { # there is something to change:
        
        CT_list[[CT_ix]] <- new_vec
        # print(sub_list)[[CT_ix]]  # 4debugging  
        
      } else { # remove i-th list element:
        
        CT_list[[CT_ix]] <- NULL  # Note: This implies that CT_list can shrink!
        # print(paste0("Removed CT_ix = ", CT_ix, ". element of CT_list"))  # 4debugging
        
      }
      
    } # loop out_list end. 
    
    sub_list <- CT_list # transfer CT_list back to sub_list: 
    
  } # if (is.null(out_list)) etc.  
  
  
  # 3. Output: ---- 
  
  # Remove empty list elements: 
  sub_list <- drop_empty_list_elements(sub_list)  # use utility function (above)
  
  return(sub_list)
  
} # sublist(). 

# ## Check:
# ## (A) Examples: 
# (ls <- list(n = 1:4, l = letters[1:4]))
# 
# # Trivial cases:
# sublist(ls, in_list = ls)
# sublist(ls, out_list = ls)
# 
# # Examples:
# # (a) in_list only:
# sublist(ls,  in_list = list(l = letters[c(2, 4)], n = c(2, 4)))
# sublist(ls,  in_list = list(n = 4))
# 
# # NA/empty cases:
# sublist(ls,  in_list = NA)      # returns NA
# sublist(ls,  in_list = list())  # returns an empty list
# 
# # Note:
# sublist(ls,  in_list = list(l = 4, n = 4))      # matching list elements and levels
# sublist(ls,  in_list = list(l = 99, n = 99))    # non-existent levels: nothing in
# sublist(ls,  in_list = list(ll = 99, nn = 99))  # heuristics match list elements
# 
# # (b) out_list only:
# sublist(ls, out_list = list(l = letters[c(2, 4)], n = c(2, 4)))
# sublist(ls, out_list = list(n = 4))
# 
# # NA/empty cases:
# sublist(ls, out_list = NA)      # returns NA
# sublist(ls, out_list = list())  # returns original list
# 
# # Note:
# sublist(ls, out_list = list(l = 4, n = 4))      # matching list elements and levels
# sublist(ls, out_list = list(l = 99, n = 99))    # non-existent levels: nothing out
# sublist(ls, out_list = list(ll = 99, nn = 99))  # heuristics match list elements
# 
# # (c) in_list AND out_list:
# sublist(ls, in_list = list(n = 3:4, l = c("c", "a")), out_list = list(l = "c"))
# sublist(ls, in_list = list(n = 3:4, l = c("c", "a")), out_list = list(n = 4, l = "c"))
# 
# # removing everything:
# sublist(ls, in_list = list(n = 4:3), out_list = list(n = 3:4))
# sublist(ls, in_list = list(l = c("c", "a")), out_list = list(l = c("a", "c")))
# 
# sublist(ls, in_list = list(n = 3:4, l = c("a", "c")),
#         out_list = list(n = 4:3, l = c("c", "a")))
# 
# # Note: Tags can be used repeatedly and empty list elements are dropped:
# sublist(ls, out_list = list(l = c("c", "b"), n = 2:3))
# sublist(ls, out_list = list(l = c("c", "b"), n = 2:3, l = c("a", "d")))
# sublist(ls, out_list = list(l = c("c", "b"), n = 2:3, l = c("a", "d"), n = 1:4))
# 
# 
# ## (B) Character elements (e.g., from dimnames(tbl)):
# (t_nm <- dimnames(Titanic))  # A test list (with dimension and level names)
# # names(t_nm)  # names of t_nm
# #
# # # Trivial case:
# sublist(t_nm)  # returns original list
# sublist(t_nm,  in_list = list())  # empty list
# sublist(t_nm, out_list = t_nm)    # empty list
# 
# ## (a) Working for:
# # # in_list provides dimnames (dropping non-existent levels):
# sublist(t_nm, in_list = list(Age = "Adult", Class = c("3rd", "1st", "stuff")))
# sublist(t_nm, in_list = list(Age = "Adult", XXX = c("3rd", "1st")))
# sublist(t_nm, in_list = list(Age = 2, Class = c(3, 99, 1)), quiet = FALSE)   
# sublist(t_nm, in_list = list(Age = 2, Class = c(3, 99, 1)), quiet = TRUE)  # suppress some messages
# 
# # in_list provides a mix of names/tags and no names/tags (dropping non-existent levels):
# sublist(t_nm, in_list = list("Adult", Class = c("3rd", "1st", "stuff")))
# sublist(t_nm, in_list = list("Adult", Class = c(3, 1, 99)))
# sublist(t_nm, in_list = list("Adult", Class = c(3, 1, 99)), quiet = TRUE)  # suppress some messages
# 
# # in_list provides NO names: Note: If levels are identified, names of ls are used:
# sublist(t_nm, in_list = list("Adult", c("3rd", "1st")))
# 
# # in_list provides names/tags and numeric levels:
# sublist(t_nm, in_list = list(Age = 2, Class = c(3, 1)))
# sublist(t_nm, in_list = list(Age = 2, Class = c(3, 1)), quiet = TRUE)  # suppress some messages
# 
# # in_list provides a mix of numeric levels and named levels:
# sublist(t_nm, in_list = list("Adult", Class = c(3, 1)))
# sublist(t_nm, in_list = list(c(3, 1), "Adult"))
# sublist(t_nm, in_list = list(c(3, 1), "Female", 2, "Yes"))
# 
# # in_list provides no names/tags and only numeric levels:
# sublist(t_nm, in_list = list(c(1, 3), 2, 2, 2))
# sublist(t_nm, in_list = list(c(1, 3), 2))
# sublist(t_nm, in_list = list(c(3, 1), NA, 2))  # Note missing level
# sublist(t_nm, in_list = list(c(3, 1), 99, 2))  # Note missing level
# sublist(t_nm, in_list = list(c(3, 1), 99, 2), quiet = TRUE)  # some messages NOT suppressed 
# 
# ## (b) Using in_list and out_list:
# sublist(t_nm, out_list = list(Survived = "No", Age = "Child", Class = c("Crew", "3rd")))
# sublist(t_nm, out_list = list(Survived = "No", Age = "Child", Class = 3:4))  # using numeric index on Class
# 
# sublist(t_nm, in_list = list("Adult", Class = c(3, 1)), out_list = list(Class = "1st"))
# sublist(t_nm, in_list = list("Adult", Class = c(3, 1)), out_list = list(Class = 1)) # Note: 1-st element of in_list Class dropped!
# 
# ## (c) Returning partial results/NA for:
# # NA if no names/tags provided and a level cannot be identified:
# sublist(t_nm, in_list = list("Adult", c("3rd", "1st", "stuff")))  # Class levels not recognized
# 
# # NA for providing no dimname and a non-existent level combination:
# sublist(t_nm, in_list = list("Adult", c("3rd", "1st", "stuff")))
# 
# # NA for providing existing dim-name/tag as a level:
# sublist(t_nm, in_list = list("Adult", "Class"))
# sublist(t_nm, in_list = list("Adult", "Class"), quiet = TRUE)  # some messages NOT suppressed 
# 
# ## (d) Handling NA inputs:
# sublist(NA)
# sublist(t_nm, in_list = NA)
# sublist(t_nm, in_list = c(NA, NA))
# sublist(t_nm, in_list = list(NA, NA), quiet = TRUE)  # some messages NOT suppressed 
# 
# 
# ## (C) Mixed list levels, some tags:
# (ml <- list(a1 = LETTERS[1:9], n1 = 3:7, a2 = letters[7:3], 7:9, tf = c(TRUE, FALSE, TRUE)))
# 
# sublist(ml, in_list = list(n1 = c(6, 4), a2 = letters[5:6], tf = FALSE))
# sublist(ml, in_list = list(n1 = c(6, 4)))  # numeric values NOT used for numeric subsetting!
# sublist(ml, in_list = list(a1 = 2, tf = 2)) # numeric values used for numeric subsetting!
# 
# # Note: Tags can be used repeatedly and empty list elements are dropped:
# sublist(ml, out_list = list(tf = TRUE, n1 = 7:5))
# sublist(ml, out_list = list(tf = TRUE, n1 = 7:5, tf = FALSE, n1 = 3:4))
# 
# # Note: Providing numeric index to non-numeric list elements can yield
# #       a conflict (if numeric elements found in another element) + heuristic => wrong selection:
# sublist(ml, in_list = list(a2 = 5:6))           # yields ERRONEOUS result, but
# sublist(ml, in_list = list(a2 = letters[5:6]))  # yields correct result.


# ToDo: 
# - Simplify function (e.g., by making more conservative/strict)
# - Analog version: out_list uses all, but excludes all mentioned dimensions/levels


## sublist_tbl: A more contrained sublist function for subtable(), using 2 arguments (in_list and out_list): ------ 

# A more specific, constrained and robust version of sublist() (above)
# to be used in the subtable() function: 
# 
# Constraints:
# - Lists returned keep ALL elements (name/tags) of original ls 
# - in the same order as the original ls.  
# - Any non-mentioned elements of ls are left intact/kept as is/unchanged! 
# - Any elements without levels remaining are set to NA (but not removed by setting them to NULL). 
# 
# => The resulting sub_list is more similar to ls (same names/tags and element order).

sublist_tbl <- function(ls, in_list = ls, out_list = NULL, quiet = FALSE){
  
  # 0. Inputs: ---- 
  if (!is.list(ls)){ message("sublist_tbl: ls is not a list."); return(NA) }
  
  if (!is.list(in_list)) { message("sublist_tbl: in_list is not a list.");  return(NA) }
  
  if (is_empty_list(in_list)){ return(vector("list", 0)) }  # return an empty list
  
  if (is.null(out_list)) {
    
    if (identical(in_list, ls)) { return(ls) }
    
  } else { # out_list has been specified:
    
    if (!is.list(out_list)) { message("sublist_tbl: out_list is not a list.");  return(NA) }
    
  }
  
  
  # 1. Use in_list: ---- 
  if ( (is.null(in_list)) | (identical(in_list, ls)) ){
    
    sub_list <- ls  # Case_A: Transfer ls to sub_list (to use out_list below)
    
  } else { # interpret in_list elements and change sub_list accordingly:
    
    # Initialize: 
    sub_list <- ls  # initialize sub_list to ls
    
    CT_list <- ls  # current target list (transfer current main list)
    CT_names <- names(CT_list)  # names of current target list (as a vector)
    # if (is.null(CT_names)){ print("CT_list contains NO names/tags.") }  # 4debugging 
    
    n_in <- length(in_list)       # N of desired elements/dimensions    
    in_names <- names(in_list)    # names of in_list (as a vector)
    # if (is.null(in_names)){ print("in_list contains NO names/tags.") }  # 4debugging 
    
    for (i in 1:n_in){ # loop through in_list:
      
      # Determine 2 features of desired/target element: name/tag and vector of levels/values
      
      # (a) cur_in_tag: current name/tag (as character): 
      cur_in_tag <- in_names[i]                     # tag/name of desired list dimension    
      if (is.null(cur_in_tag)){ cur_in_tag <- NA }  # fix NULL cases
      # print(cur_in_tag)  # 4debugging
      
      # (b) cur_in_vec: levels/values of current element of in_list (as a vector) 
      cur_in_vec <- in_list[[i]]  # (b) desired levels (as vector)
      
      # Determine relevant ix/position of CT_list (corresponding to current elements of in_list):
      CT_ix <- NA  # initialize
      CT_ix <- list_element_ix(CT_list, tag = cur_in_tag, values = cur_in_vec, quiet = quiet)  # use utility function (above)
      
      if ( all(is.na(CT_ix)) ){ # CT_ix could not be assigned:
        
        message(paste0("sublist_tbl: Element ", i, " of in_list not located in current list. Using element ", i, " of current list."))
        CT_ix <- i  # Heuristic: Assume SAME position i in CT_list (never quiet)!
        
      }
      
      # Determine current target levels (at CT_ix) and new levels (as subset/intersection):
      CT_vec <- CT_list[[CT_ix]]  # vector of original levels/element CT_ix of CT_list
      
      if ( (!is.numeric(CT_vec)) & (is.numeric(cur_in_vec)) ) { # provided a numeric index to non-numeric vector of CT_vec:
        
        if (!quiet) { 
          message(paste0("sublist_tbl: Using element ", i, " of in_list as numeric subset of element ", CT_ix, " of current list."))
        }
        
        new_vec <- CT_vec[cur_in_vec]  # desired subset (by numeric indexing)
        # NOTE: Non-existing index values yield and keep NA values.
        
        new_vec <- new_vec[!is.na(new_vec)]  # remove NA values
        # NOTE: Non-existing levels are dropped!
        
      } else { # cur_in_vec is NOT numeric or BOTH are numeric: use levels/names provided: 
        
        new_vec <- intersect(cur_in_vec, CT_vec)  # prioritize cur_in_vec and drop any non-existing levels!      
        # NOTE: Non-existing levels are dropped!
        
      }
      
      # Populate CT_ix-th element of sub_list by new_vec:
      sub_list[[CT_ix]] <- new_vec  # ADD list element  
      names(sub_list)[CT_ix] <- CT_names[CT_ix]  # Assign corresponding name/tag of CT_list to list element
      
    } # loop in_list end. 
    
  } # if (is.null(in_list)) etc.
  
  
  # # Intermediate feedback:
  # print(paste0("sublist_tbl: sub_list (after processing in_list): "))  
  # print(sub_list) # 4debugging 
  
  
  # 2. Use out_list: ---- 
  if ( is.null(out_list) | is_empty_list(out_list) ) {
    
    return(sub_list)
    
  } else if (identical(out_list, sub_list)) {
    
    # print(paste0("sublist: Nothing remains, as out_list corresponds to sub_list."))  # 4debugging 
    return(vector("list", 0))  # return an empty list
    
  } else { # interpret out_list (to remove elements or levels from CT_list): 
    
    # print(paste0("sublist: Interpret a given out_list by removing from sub_list:"))  # 4debugging 
    
    # Initialize: 
    CT_list <- sub_list  # current target list (transfer current main list)
    CT_names <- names(CT_list)  # names of current target list (as a vector)
    # if (is.null(CT_names)){ print("CT_list contains NO names/tags.") }  # 4debugging 
    
    n_out <- length(out_list)     # N of desired elements/dimensions
    out_names <- names(out_list)  # names of out_list (as a vector)  
    # if (is.null(out_names)){ print("out_list contains NO names/tags.") }  # 4debugging 
    
    for (i in 1:n_out){ # loop through out_list:
      
      # Determine 2 features of desired/target element: name/tag and vector of levels/values
      
      # (a) cur_out_tag: current name/tag (as character): 
      cur_out_tag <- out_names[i]                     # tag/name of desired list dimension    
      if (is.null(cur_out_tag)){ cur_out_tag <- NA }  # fix NULL cases
      # print(cur_out_tag)  # 4debugging
      
      # (b) cur_out_vec: levels/values of current element of out_list (as a vector) 
      cur_out_vec <- out_list[[i]]  # desired/target levels (as vector)
      
      # Determine relevant ix/position of CT_list (corresponding to current elements of out_list):
      CT_ix <- NA  # initialize
      CT_ix <- list_element_ix(CT_list, tag = cur_out_tag, values = cur_out_vec, quiet = quiet)  # use utility function (above)
      
      if ( all(is.na(CT_ix)) ){ # CT_ix could not be assigned:
        
        message(paste0("sublist_tbl: Element ", i, " of out_list not located in current list. Using element ", i, " of current list."))
        CT_ix <- i  # Heuristic: Assume SAME position i in CT_list (never quiet)!
        
      }
      
      # Determine current target levels (at CT_ix) and new levels (as subset/setdiff):
      CT_vec <- CT_list[[CT_ix]]  # vector of levels/element CT_ix of CT_list
      
      if ( (!is.numeric(CT_vec)) & (is.numeric(cur_out_vec)) ) { # provided a numeric index to non-numeric vector of CT_vec:
        
        if (!quiet) { 
          message(paste0("sublist_tbl: Using element ", i, " of out_list as numeric subset of element ", CT_ix, " of current list."))
        }
        
        new_vec <- CT_vec[-cur_out_vec]  # reduced subset (by numeric indexing)
        # NOTE: Non-existing index values yield and keep NA values.
        
        new_vec <- new_vec[!is.na(new_vec)]  # remove NA values
        # NOTE: Non-existing levels are dropped!
        
      } else { # cur_out_vec is NOT numeric or BOTH are numeric: use levels/names provided: 
        
        new_vec <- setdiff(CT_vec, cur_out_vec)  # prioritize CT_vec and drop any non-existing levels!
        # NOTE: Non-existing levels are dropped!
        
      }
      
      # print(new_vec)  # 4debugging
      
      # CHANGE CT_ix-th element of CT_list to new_vec:
      if ( !all(is.na(new_vec)) ) { # there is something to change:
        
        CT_list[[CT_ix]] <- new_vec
        # print(CT_list)[[CT_ix]]  # 4debugging  
        
      } else { # new_vec contains only NA:
        
        CT_list[[CT_ix]] <- NA  # Note: NULL would imply that CT_list can shrink!
        # print(paste0("Element CT_ix = ", CT_ix, " of CT_list set to NA."))  # 4debugging
        
        # if (!quiet) { 
        message(paste0("sublist_tbl: Element ", CT_ix, " of current list is NA."))
        # }
        
      }
      
    } # loop out_list end. 
    
    sub_list <- CT_list  # transfer CT_list back to sub_list: 
    
  } # if (is.null(out_list)) etc.  
  
  
  # 3. Output: ---- 
  
  # # Remove empty list elements: 
  # sub_list <- drop_empty_list_elements(sub_list)  # use utility function (above)
  
  return(sub_list)
  
} # sublist_tbl(). 

# ## Check:
# ## (A) Examples:
# (ls <- list(n = 1:4, l = letters[1:4]))
# 
# # Trivial cases:
# sublist_tbl(ls, in_list = ls)
# sublist_tbl(ls, out_list = ls)
# 
# # Examples:
# # (a) in_list only:
# sublist_tbl(ls,  in_list = list(l = letters[4]))  # Note: non-mentioned element(s) are retained!
# sublist_tbl(ls,  in_list = list(l = letters[c(4, 2)], n = c(4, 2)))  # level order reversed
# sublist_tbl(ls,  in_list = list(l = c("d", "a"), n = 3:2))  # element order remains constant.
# sublist_tbl(ls,  in_list = list(l = c("d", "a"), n = 3:2, l = "d", n = 2))  # using elements repeatedly.
# 
# # NA/empty cases:
# sublist_tbl(ls,  in_list = NA)      # returns NA
# sublist_tbl(ls,  in_list = list())  # returns an empty list
# 
# # Note:
# sublist_tbl(ls,  in_list = list(l = 4, n = 4))      # matching list elements and levels
# sublist_tbl(ls,  in_list = list(l = 99, n = 99))    # non-existent levels: nothing in
# sublist_tbl(ls,  in_list = list(ll = 99, nn = 99))  # heuristics match list elements
# # 
# # # (b) out_list only:
# sublist_tbl(ls, out_list = list(l = letters[c(3, 1)], n = c(1, 3)))
# sublist_tbl(ls, out_list = list(l = "c", n = 3, n = 1, l = "a"))  # multiple mentions
# 
# # NA/empty cases:
# sublist_tbl(ls, out_list = NA)      # returns NA
# sublist_tbl(ls, out_list = list())  # returns original list
# 
# # Note 1: When using numeric indices, order may matter:
# sublist_tbl(ls, out_list = list(l = "d", n = 4))  # works, BUT: 
# sublist_tbl(ls, out_list = list(l = 4, n = 4))    # 4-th dim of l not removed, due to conflict!
# sublist_tbl(ls, out_list = list(n = 4, l = 4))    # 4-th dim of l removed, due to NO more conflict!
# 
# # Note 2: Heuristics for matching un-identified levels may yield unexpected results:
# sublist_tbl(ls, out_list = list(l = 99, n = 99))    # non-existent levels: nothing out
# sublist_tbl(ls, out_list = list(ll = 99, nn = 99))  # heuristics match list elements (but no removal of non-existent levels)
# sublist_tbl(ls, out_list = list(ll = 3, nn = 3))    # heuristics match list elements (and removal of non-existent levels)
# 
# # (c) in_list AND out_list:
# sublist_tbl(ls, in_list = list(n = 3:4, l = c("c", "a")), out_list = list(l = "c"))
# sublist_tbl(ls, in_list = list(n = 3:4, l = c("c", "a")), out_list = list(n = 4, l = "c"))
# 
# # removing everything yields NA (but does not remove list elements): 
# sublist_tbl(ls, in_list = list(n = 3:4), out_list = list(n = 4:3), quiet = FALSE)
# sublist_tbl(ls, in_list = list(l = c("c", "a")), out_list = list(l = c("a", "c")))
# sublist_tbl(ls, in_list = list(n = 3:4, l = c("a", "c")),
#           out_list = list(n = 4:3, l = c("c", "a")))
# 
# # Note: Tags can be used repeatedly and empty levels yield NA (but do not drop list elements):
# sublist_tbl(ls, out_list = list(l = c("c", "b"), n = 2:3))
# sublist_tbl(ls, out_list = list(l = c("c", "b"), n = 2:3, l = c("a", "d")))
# sublist_tbl(ls, out_list = list(l = c("c", "b"), n = 2:3, l = c("a", "d"), n = 1:4))
# 
#
# ## (B) Character elements (e.g., from dimnames(tbl)):
# (t_nm <- dimnames(Titanic))  # A test list (with dimension and level names)
# # names(t_nm)  # names of t_nm
# #
# # # Trivial case:
# sublist_tbl(t_nm)  # returns original list
# sublist_tbl(t_nm,  in_list = list())  # empty list
# sublist_tbl(t_nm, out_list = t_nm)    # empty list
# 
# ## (a) Working for:
# # # in_list provides dimnames (ignoring non-existent tags and levels):
# sublist_tbl(t_nm, in_list = list(Age = "Adult", Class = c("3rd", "1st", "stuff")))
# sublist_tbl(t_nm, in_list = list(Age = "Adult", XXX = c("3rd", "1st")))
# 
# sublist_tbl(t_nm, in_list = list(Age = 2, Class = c(3, 99, 1)), quiet = FALSE)
# sublist_tbl(t_nm, in_list = list(Age = 2, Class = c(3, 99, 1)), quiet = TRUE)  # suppress some messages
# 
# # in_list provides NO names: Note: If levels are identified, names of ls are used:
# sublist_tbl(t_nm, in_list = list("Adult", c("3rd", "1st")))
# 
# # in_list provides names/tags and numeric levels:
# sublist_tbl(t_nm, in_list = list(Age = 2, Class = c(3, 1)))
# sublist_tbl(t_nm, in_list = list(Age = 2, Class = c(3, 1)), quiet = TRUE)  # suppress some messages
# 
# # in_list provides a mix of numeric levels and named levels:
# sublist_tbl(t_nm, in_list = list("Adult", Class = c(3, 1)))
# sublist_tbl(t_nm, in_list = list(c(3, 1), "Adult"))
# sublist_tbl(t_nm, in_list = list(c(3, 1), "Female", 2, "Yes"))  # Note: Heuristics and messages!
# 
# # in_list provides no names/tags and only numeric levels:
# sublist_tbl(t_nm, in_list = list(c(1, 3), 2, 2, 2))
# sublist_tbl(t_nm, in_list = list(c(1, 3), 2))
# sublist_tbl(t_nm, in_list = list(c(3, 1), NA, 2))  # Note missing level
# sublist_tbl(t_nm, in_list = list(c(3, 1), 99, 2))  # Note empty level
# sublist_tbl(t_nm, in_list = list(c(3, 1), 99, 2), quiet = TRUE)  # some messages NOT suppressed
# 
# ## (b) Using in_list and out_list:
# sublist_tbl(t_nm, out_list = list(Survived = "No", Age = "Child", Class = c("Crew", "3rd")))
# sublist_tbl(t_nm, out_list = list(Survived = "No", Age = "Child", Class = 3:4))  # using numeric index on Class
# 
# sublist_tbl(t_nm, in_list = list("Adult", Class = c(3, 1)), out_list = list(Class = "1st"))
# sublist_tbl(t_nm, in_list = list("Adult", Class = c(3, 1)), out_list = list(Class = 1)) # Note: 1-st element of in_list Class dropped!
# 
# ## (c) Returning partial results/NA for:
# # NA if no names/tags provided and a level cannot be identified:
# sublist_tbl(t_nm, in_list = list("Adult", c("3rd", "1st", "stuff")))  # Class levels not recognized
# 
# # NA for providing no dimname and a non-existent level combination:
# sublist_tbl(t_nm, in_list = list("Adult", c("3rd", "1st", "stuff")))
# 
# # NA for providing existing dim-name/tag as a level:
# sublist_tbl(t_nm, in_list = list("Adult", "Class"), quiet = TRUE)  # some messages NOT suppressed
# 
# ## (d) Handling NA inputs:
# sublist_tbl(NA)
# sublist_tbl(t_nm, in_list = NA)
# sublist_tbl(t_nm, in_list = c(NA, NA))
# sublist_tbl(t_nm, in_list = list(NA, NA), quiet = TRUE)  # some messages NOT suppressed
#
# 
# ## (C) Mixed list levels, some tags:
# (ml <- list(a1 = LETTERS[1:9], n1 = 3:7, a2 = letters[7:3], 7:9, tf = c(TRUE, FALSE, TRUE)))
# 
# sublist_tbl(ml, in_list = list(n1 = c(6, 4), a2 = letters[5:6], tf = FALSE))  # non-mentioned elements included in full
# sublist_tbl(ml, in_list = list(n1 = c(6, 4)))    # numeric values NOT used for numeric subsetting
# sublist_tbl(ml, in_list = list(a1 = 2, tf = 2))  # numeric values used for numeric subsetting
# 
# # Note: Tags can be used repeatedly and empty list elements are NOT dropped:
# sublist_tbl(ml, out_list = list(tf = TRUE, n1 = 7:5))
# sublist_tbl(ml, out_list = list(tf = TRUE, n1 = 7:5, tf = FALSE, n1 = 3:4))  # $n1 becomes NA
# 
# # Note: Providing numeric index to non-numeric list elements can yield
# #       a conflict (if numeric elements found in another element) + heuristic => wrong selection:
# sublist_tbl(ml, in_list = list(a2 = 5:6))           # yields ERRONEOUS result (matching $a1 + numeric indexing), but
# sublist_tbl(ml, in_list = list(a2 = letters[5:6]))  # yields correct result.


# ToDo: 
# - Simplify sublist functions (e.g., by making more conservative/strict)

## ToDo: ------

# - match for lists: in which sublist is an element (e.g., name).

# - sublist() for lists: with 2 arguments to include and exclude elements of lists in ls
#                        to use in a subtable() function.


## eof. ----------
