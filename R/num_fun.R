## num_fun.R  | i2ds
## hn | uni.kn | 2022 06 22

# Functions for transforming/manipulating numbers: ------ 

# base2dec: Convert a base N numeric string into a decimal number: ---- 

base2dec <- function(seq, base = 2){
  
  dec_nr  <- 0   
  len_seq <- length(seq)
  
  if ((len_seq == 1) && (is.character(seq))) { 
    
    # convert string into vector (of 1-digit numeric elements):
    vec <- str2vec(seq)
    seq <- as.numeric(vec)
    len_seq <- length(seq)
    
  } # if. 
  
  rev_seq <- rev(seq)
  
  for (i in 1:len_seq){
    
    cur_i  <- rev_seq[i]
    dec_nr <- dec_nr + (cur_i * base^(i-1))
    
  } # for.
  
  return(dec_nr)
  
} # base2dec(). 

# # Check:
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
