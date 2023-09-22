## df_fun.R | i2ds
## hn | uni.kn | 2023 09 22

# Functions for working with data frames (df): ------ 


# lookup: Look-up / filtering operations within a df: ----

# Goal: Use indexing / matching functions to filter df OR 
#       look up the corresponding values of some target variable v_2 given the values of some input variable v_1.

# Context: indexing / subsetting / matching vectors / df variables.


lookup <- function(x, df, v_1 = 1, v_2 = 1){
  
  # identify vectors in df (for numeric OR name inputs):
  df_v_1 <- df[[v_1]]  # input  / origin
  df_v_2 <- df[[v_2]]  # output / target
  
  # # (A) FIRST match of x in input:
  # match_pos <- match(x = x, table = df_v_1)  # yields numeric index
  # 
  # # index output:
  # df_v_2[match_pos]
  
  # (B) Allowing for multiple matches:
  # match_log <- df_v_1 %in% x  # yields logical index, is identical to:
  match_log <- match(x = df_v_1, table = x, nomatch = 0) > 0  # yields logical index
  
  # (C) Allowing for partial matches:
  # match_log <- pmatch(x = df_v_1, table = x, nomatch = 0, duplicates.ok = TRUE) > 0  # does not work
  # match_log <- charmatch(x = df_v_1, table = x, nomatch = 0) > 0  # does not work
  
  # index output:
  df_v_2[match_log]
  
} # lookup(). 


# # Check:
# # (A) 1: specific exemplars:
# lookup(x = c("cardiff_1", "not there", "princeton_1"), unicol_data, v_1 = "pal", v_2 = "inst")
# lookup(x = c("cardiff_1", "not there", "princeton_1"), unicol_data, v_1 = "pal", v_2 = "url")
# 
# # (A) 2: entire columns:
# lookup(x = unicol_data$pal, unicol_data, v_1 = "pal", v_2 = "inst")
# lookup(x = unicol_data$pal, unicol_data, v_1 = "pal", v_2 = "url")
# 
# # (B) from 1 input to MANY outputs:
# lookup(x = c("University of Waterloo", "no where", "McGill University"), unicol_data, v_1 = "inst", v_2 = "pal")
# lookup(x = c("University of Waterloo", "no where", "McGill University"), unicol_data, v_1 = "inst", v_2 = "url")
# 
# # (C) Note that partial matches DO NOT work:
# lookup(x = c("Waterloo", "nowhere", "McGill"), unicol_data, v_1 = "inst", v_2 = "url")
# 
# # Towards partial matches:
# pmatch(c("Waterloo", "not there", "McGill"), unicol_data$inst)
# charmatch(c("Waterloo", "not there", "McGill"), unicol_data$inst)
# grepl(c("Waterloo", "not there", "McGill"), unicol_data$inst)  # ToDo: Turn x into regex pattern. 
 
# # +++ here now +++ 


## ToDo: ------

# - i2ds course: Explicate relation(s) between match() and which() (in logical/numeric indexing).

# - Create a filter function for df (that works with multiple conditions/variables)


## eof. ----------
