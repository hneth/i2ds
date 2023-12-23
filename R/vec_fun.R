## vec_fun.R  | i2ds
## hn | uni.kn | 2023 12 23

# Functions for transforming/manipulating vectors: ------ 
#
# Note that objects of type "table" are specific cases of arrays (contingency tables with freq. counts).



# sort_in_order: Sort a vector x into the order given by the elements of order: ------ 

sort_in_order <- function(x, order){
  
  if (! all(x %in% order)){
    
    message("Some elements of x are not in order.")
    
  }
  
  x[order(match(x, order))]
  
} # sort_in_order().

# # Check:
# sort_in_order(c(2, 1, 4, 3, 4, 3, 2, 1), order = c(1, 2, 3, 4))
# sort_in_order(c(2, 1, 4, 3, 4, 3, 2, 1), order = c(4, 3))
# sort_in_order(c(2, 1, 4, 3, 4, 3, 2, 1), order = c(3, 5, 8))
# sort_in_order(c("C", "A", "X", "B", "Y", "A", "Z"), order = LETTERS)
# sort_in_order(c("C", "A", "X", "B", "Y", "A", "Z"), order = rev(LETTERS))


## Done: ------ 

# - etc.

## ToDo: ------

# - etc.

## eof. ----------
