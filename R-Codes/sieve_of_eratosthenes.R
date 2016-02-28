#------------------------------------------------------------------------------
# Description: Generation of prime numbers up to limit n by the sieve of 
# Eratosthenes. 
#------------------------------------------------------------------------------
# Usage: -
#------------------------------------------------------------------------------
# Inputs: Limit n
#------------------------------------------------------------------------------
# Output: Prime numbers up to n
#------------------------------------------------------------------------------
# Author: Josephine Kraft, 2016-02-28
#------------------------------------------------------------------------------

eratosthenes = function(n) {
  
  x = c(2:n) # list of numbers from 2 to n
  p = 2            
  r = c()    # results vector

  while (p*p < n) {
    r = c(r,x[1])                # first element of n is always prime
    x = x[-(which(x %% p ==0))]  # elements with remainder 0 are multiples of p
    p = x[1]                     # first element of n will always be prime
  }
   return(c(r,x))   # all remaining elements in n are prime
}


# Results:

eratosthenes(100)
eratosthenes(1000)





