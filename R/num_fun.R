## num_fun.R  | i2ds
## hn | uni.kn | 2022 06 23

# Functions for manipulating/transforming numbers or numeric symbols: ------ 


# base2dec: Convert a base N numeric string into a decimal number: ------ 

#' Convert a base N numeric string (to a decimal numeric string). 
#' 
#' \code{base2dec} converts a sequence of numeric symbols 
#' from base N notation to decimal (i.e., base 10) notation. 
#' 
#' @return A decimal number. 
#' 
#' @param seq A (required) sequence or string of numeric symbols 
#' (i.e., digits 0-9, as defined in specified base).
#' 
#' @param base The base of the symbols in \code{seq}. 
#' Default: \code{base = 2} (binary).   
#'        
#' @examples 
#' # (a) string inputs:
#' base2dec("11")
#' base2dec("11", base = 5)
#' base2dec("1010")
#' base2dec("0101")
#' # (b) numeric vectors as inputs:
#' base2dec(c(1, 0, 1, 0))
#' base2dec(c(1, 0, 1, 0), base = 3)
#' 
#' @family numeric functions
#' 
#' @seealso
#' \code{\link{as.roman}} converts integers into Roman numerals. 
#' 
#' @export 

base2dec <- function(seq, base = 2){
  
  # Initialize: 
  dec_nr  <- 0   
  len_seq <- length(seq)
  
  # Prepare:
  if ((len_seq == 1) && (is.character(seq))) { 
    
    # convert string into vector (of 1-digit numeric elements):
    vec <- str2vec(seq)
    seq <- as.numeric(vec)
    len_seq <- length(seq)
    
  } # if. 
  
  # Main:   
  rev_seq <- rev(seq)
  
  for (i in 1:len_seq){ # compute polynomial: 
    
    cur_i  <- rev_seq[i]
    dec_nr <- dec_nr + (cur_i * base^(i-1))
    
  } # for.
  
  return(dec_nr)
  
} # base2dec(). 

# ## Check:
# # (a) string inputs:
# base2dec("0")
# base2dec("1")
# base2dec("1010")  # seq as string
# # (b) numeric vectors as inputs:
# base2dec(c(1, 0, 1, 0))
# base2dec(c(1, 0, 1, 0), base = 3)


## ToDo: ------

# - dec2base conversion function (complement to base2dec)

## eof. ----------
