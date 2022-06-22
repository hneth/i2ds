## string_fun.R  | i2ds
## hn | uni.kn | 2022 06 22

# Functions for transforming/manipulating character strings: ------ 

# Turn a vector into a string: ----

vec2str <- function(s) {
  
  paste(s, collapse = "")
  
} # vec2str(). 


# Turn a string into a vector (of 1-symbol elements): ---- 

str2vec <- function(s){
  
  unlist(strsplit(s, split = ""))  # assumes ONLY 1-symbol elements/digits
  
} # str2vec(). 


## ToDo: ------

# - etc.

## eof. ----------
