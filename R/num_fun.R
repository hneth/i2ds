## num_fun.R  | i2ds
## hn | uni.kn | 2022 06 24

# Functions for manipulating/transforming numbers or numeric symbols: ------ 


# base2dec: Convert a base N numeric string into a decimal number: ------ 

#' Convert a string of numeric digits from some base into a decimal number. 
#' 
#' \code{base2dec} converts a sequence of numeric symbols 
#' from base N notation to decimal (i.e., base 10) notation. 
#' 
#' \code{base2dec} is the complement of \code{\link{dec2base}}. 
#' 
#' @return A decimal number. 
#' 
#' @param x A (required) sequence or string of numeric symbols 
#' (i.e., digits 0-9, as defined in the specified base).
#' 
#' @param base The base of the symbols in \code{seq}. 
#' Default: \code{base = 2} (binary).   
#'        
#' @examples 
#' # (a) single string input:
#' base2dec("11")
#' base2dec("11", base = 5)
#' base2dec("1010")
#' base2dec("0101")
#' 
#' # (b) numeric vectors as inputs:
#' base2dec(c(1, 0, 1, 0))
#' base2dec(c(1, 0, 1, 0), base = 3)
#' 
#' # (c) character vector as inputs:
#' base2dec(c("1", "0", "1", "0"))
#' base2dec(c("1", "0", "1", "0"), base = 3)
#' 
#' # (d) multi-digit vectors:
#' base2dec(c(1, 1), base = 10)
#' base2dec(c(1, 1), base = 3)
#' base2dec(c(2, 3), base = 3)  # Note message.
#' 
#' # Note: 
#' base2dec(dec2base(0120, base = 3), base = 3)
#' dec2base(base2dec(0210, base = 3), base = 3)
#' 
#' @family numeric functions
#' 
#' @seealso
#' \code{\link{dec2base}} converts decimal numbers into another base;  
#' \code{\link{as.roman}} converts integers into Roman numerals. 
#' 
#' @export 

base2dec <- function(x, base = 2){
  
  # Process inputs:
  seq  <- as.character(x)
  base <- as.numeric(base)
  
  # Initialize: 
  out_val <- 0  # output value (in decimal notation) 
  len_seq <- length(seq)  
  
  # Catch some special cases:
  if (any(is.na(seq)) | is.na(base)) { return(NA) }
  if ((len_seq == 1) && (seq == "0")){ return(0)  }  
  if ((base < 2) | (base > 10) | (base %% 1 != 0)) { 
    message("base2dec: base must be an integer in 2:10.")
    return(NA)
  }
  
  # Prepare:
  if ((len_seq == 1) && (nchar(seq) > 1)) { # seq is a multi-digit string:
    
    # Convert a string into a numeric vector (of 1-digit numeric elements):
    vec <- str2vec(seq)
    seq <- as.numeric(vec)
    
    
  } else { # convert character sequence into numeric values:
    
    seq <- as.numeric(seq)
    
  } # if.
  
  # print(seq)  # 4debugging
  len_seq <- length(seq) 
  
  # Ensure that seq only contains integers <= base:
  if (any(seq >= base)){
    message("base2dec: All digits in x must be < base!")
  }
  
  # Main:
  rev_seq <- rev(seq)
  
  for (i in 1:len_seq){ # loop to compute polynomial: 
    
    cur_i  <- rev_seq[i]
    # print(paste0("cur_i = ", cur_i))  # 4debugging
    
    out_val <- out_val + (cur_i * base^(i - 1))
    
  } # for.
  
  return(out_val)
  
} # base2dec(). 

# ## Check:
# # (a) string inputs:
# base2dec("0")
# base2dec("1")
# base2dec("1010")  # seq as string
# # (b) numeric vectors as inputs:
# base2dec(c(1, 0, 1, 0))
# base2dec(c(1, 0, 1, 0), base = 3)


# - dec2base conversion function (as complement to base2dec): ------

#' Convert a decimal number into a string of numeric digits in some base. 
#' 
#' \code{dec2base} converts a decimal (i.e., base 10) number into 
#' a sequence of numeric symbols (digits) in some other base. 
#' 
#' \code{dec2base} is the complement of \code{\link{base2dec}}. 
#' 
#' @return A number (in base notation).
#' 
#' @param x A (required) integer in decimal notation 
#' (i.e., containing only digits 0-9).
#' 
#' @param base The base of the symbols in \code{seq}. 
#' Default: \code{base = 2} (binary).   
#'        
#' @examples 
#' # (a) single numeric input:
#' dec2base(3)
#' dec2base(8)
#' dec2base(8, base = 3)
#' dec2base(8, base = 7)
#' 
#' dec2base(110, base = 2)
#' dec2base(110, base = 10)
#' 
#' # (b) single string input:
#' dec2base("7", base = 2)
#' dec2base("8", base = 3)
#' 
#' # Note: 
#' base2dec(dec2base(01230, base = 4), base = 4)
#' dec2base(base2dec(03210, base = 4), base = 4)
#' 
#' @family numeric functions
#' 
#' @seealso
#' \code{\link{base2dec}} converts numbers from some base into decimal numbers;  
#' \code{\link{as.roman}} converts integers into Roman numerals. 
#' 
#' @export 

dec2base <- function(x, base = 2){
  
  # Version 1: ----
  # - calculate n_digits 
  # - use for loop
  # 
  # 
  # # Process inputs: 
  # dec  <- as.numeric(x)     # numeric value (in decimal notation) 
  # base <- as.numeric(base)
  #
  # # Catch some special cases:
  # if (is.na(dec) | is.na(base)) { return(NA) }
  # if (dec == 0){ return(0) }  
  # if ( any(base < 2) | any(base > 10) | any(base %% 1 != 0) ) { 
  #   message("dec2base: base must be an integer in 2:10.")
  #   return(NA)
  # }
  # 
  # # Initialize: 
  # out <- NULL
  # dec_left <- dec 
  # 
  # # Prepare:
  # n_digits <- floor(log(dec)/log(base) + 1)
  # # print(paste("n_digits =", n_digits))  # 4debugging
  # 
  # # Main: 
  # for (i in n_digits:1){
  #   
  #   cur_digit <- dec_left %/% base^(i - 1)
  #   
  #   dec_left <- dec_left - (cur_digit * base^(i - 1))
  #   
  #   out <- paste0(out, cur_digit)
  #   
  # } # for.
  
  # Version 2: ---- 
  # - without computing n_digits
  # - while loop
  
  if (is.na(x)) { 
    
    out <- NA 
    
  } else {
    
    # Process inputs: 
    val_left  <- as.numeric(x)  # numeric value left (in decimal notation) 
    base <- as.numeric(base)
    
    # Prepare: 
    position <- 0     # position/order (0 is rightmost/unit/base^0)
    next_units <- 88  # number of units in next higher order
    out <- NULL       # initialize output
    
    # Main: 
    # while (val_left > 0){
    while (next_units > 0){
      
      # print(paste0("position = ", position, ": val_left = ", val_left))  # 4debugging
      
      # nr_next_pos  <- val_left %/% base^(position + 1)
      # sum_next_pos <- nr_next_pos * base^(position + 1)
      
      # cur_digit    <- val_left - sum_next_pos
      # cur_digit <- dec_remain %% base^(position + 1)
      # cur_digit   <- val_left - sum_next_pos
      # print(paste("cur_digit =", cur_digit))  # 4debugging
      
      next_units <- val_left %/% base^(position + 1)  # dividor on NEXT position (higher order)
      # print(paste0("- next_units = ", next_units))  # 4debugging
      
      next_rem <- val_left %%  base^(position + 1)  # remainder on NEXT position (higher order)
      # print(paste0("- next_rem = ", next_rem))  # 4debugging
      
      if (next_rem > 0){  
        
        cur_left <- val_left - (next_units * base^(position + 1))
        
        cur_div <- cur_left %/% base^(position)  # current dividor
        # print(paste0("- cur_div = ", cur_div))  # 4debugging
        
        # cur_rem <- val_left %%  base^(position)  # current remainder
        # print(paste0("- cur_rem = ", cur_rem))  # 4debugging    
        
        cur_digit <- cur_div
        
      } else { 
        
        cur_digit <- 0
        
      }
      
      # print(paste0("- cur_digit = ", cur_digit))  # 4debugging    
      
      # # choose:
      # if ((next_units > 0)) {
      #   cur_digit <- cur_rem
      # } else {
      #   cur_digit <- cur_div  
      # }
      
      # collect output:     
      out <- paste0(cur_digit, out)
      
      # update value and position counter:
      val_left <- val_left - (cur_digit * base^(position))
      position <- position + 1 
      
    } # while. 
    
    out <- as.numeric(out)
    
  }
  
  return(out)
  
} # dec2base(). 

# ## Check:
# dec2base(0)
# dec2base(1)
# dec2base(2)
# dec2base(7)
# dec2base(8)
# dec2base(8, base = 3)
# dec2base(8, base = 7)
# dec2base(8, base = 10)
# base2dec(2222, base = 3)
# 
# # Note:
# base2dec(dec2base(0120, base = 3), base = 3)
# dec2base(base2dec(0210, base = 3), base = 3)

# # Special cases:
# dec2base(0)
# dec2base(NA)


## ToDo: ------

# - Create recursive versions of base2dec() and dec2base().
# - Create vectorized versions of base2dec() and dec2base().

## eof. ----------
