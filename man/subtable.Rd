% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/array_fun.R
\name{subtable}
\alias{subtable}
\title{Extract a subtable (or subset) from a table.}
\source{
Based on the \code{subtable} function by Norman Matloff, 
The Art of R Programming (2011, pp. 131-134).
}
\usage{
subtable(tbl, in_list = dimnames(tbl), out_list = NULL, quiet = FALSE)
}
\arguments{
\item{tbl}{An original table to be reduced (as \code{\link{table}}).}

\item{in_list}{A list specifying the subset of \code{tbl} to keep/include. 
Each list element is named after a dimension of \code{tbl}, 
and the value of that component is a vector of the names or a numeric index 
of the desired levels. 
Default: \code{in_list = dimnames(tbl)} (i.e., everything in \code{tbl}).}

\item{out_list}{A list specifying the subset of \code{tbl} to drop/exclude. 
Each list element is named after a dimension of \code{tbl}, 
and the value of that component is a vector of the names or a numeric index 
of the desired levels. 
Default: \code{out_list = NULL} (i.e., nothing).}

\item{quiet}{Boolean: Hide feedback messages?
Default: \code{quiet = FALSE} (i.e., show messages).}
}
\value{
A table.
}
\description{
\code{subtable} yields a subtable of a table \code{tbl}
by filtering or extracting a subset of table's 
dimensions and levels.
}
\details{
\code{subtable} provides a filter/slice function for tables, by specifying 
a positive subset (i.e., dimensions and levels to include) as \code{in_list} or  
a negative subset (i.e., dimensions and levels to exclude) as \code{out_list} 
(both as lists that specify dimension and level names, in tag and value format). 

\code{subtable} assumes that \code{dimnames(tbl)} exist, 
but is flexible in allowing \code{in_list} and \code{out_list} 
to use both names or numeric indices for subsetting table levels.

As a key constraint, the subset of dimensions specified by 
\code{in_list} and \code{out_list} must still contain all dimensions 
(as excluding an entire dimension would leave no elements of a table) 
and yield a list with elements in the same order as \code{dimnames(tbl)} 
(to allow using \code{do.call} on \code{what = "[", args}  
to index/subset the array of tbl).
}
\examples{
t <- datasets::Titanic

# Trivial case:
subtable(t, in_list = dimnames(t))  # same as t

# (a) Using level names:
subtable(t, in_list = list(Class = c("1st", "2nd", "3rd"),
                           Sex = "Female", Age = "Adult",
                           Survived = c("Yes")))

# Alternatives ways of obtaining same result:
subtable(Titanic, out_list = list(Class = c("Crew"),
                                  Sex = "Male", Age = "Child",
                                  Survived = c("No")))

subtable(Titanic, in_list = list(Class = c("1st", "2nd", "3rd"),
                                 Sex = "Female", Age = c("Adult", "Child"),
                                 Survived = c("Yes", "No")),
         out_list = list(Age = "Child", Survived = c("No")))

# (b) Using dim names and level numbers:
subtable(t, in_list = list(Class = 1:3, Sex = 2, Age = 2, Survived = 2))

# (c) Using only level numbers (note messages):
subtable(t, in_list = list(1:3, 2, 2, 2))

# (d) Using a mix of level names and numbers (note messages): 
subtable(t, in_list = list(1:3, "Female", 2, "Yes"))

# (e) Note: Different length of sub_dims than dimnamesI(tbl) yield ERRORs:
# subtable(t, in_list = list(1:3, "Female", 2, 2, 99))

# Any tbl dimensions (names/levels) not changed by in_list/out_list are included in full:
subtable(t, in_list = list(1:3, "Female", Survived = 2))  # both Age levels included
 
}
\seealso{
\code{\link{ctable}} for creating a contingency table; 
\code{\link{sublist}} for extracting subsets of a list; 
\code{\link{table}} and \code{\link{array}} for the underlying data structures.

Other array functions: 
\code{\link{add_dimnames}()},
\code{\link{ctable}()},
\code{\link{expand_freq_table}()},
\code{\link{flatten_array}()}
}
\concept{array functions}
