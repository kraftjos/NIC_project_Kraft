#------------------------------------------------------------------------------
# Description: Generation of prime numbers up to limit n by the sieve of 
# Atkin and Bernstein. 
#------------------------------------------------------------------------------
# Usage: -
#------------------------------------------------------------------------------
# Inputs: Limit n
#------------------------------------------------------------------------------
# Output: Prime numbers up to n
#------------------------------------------------------------------------------
# Author: Josephine Kraft, 2016-02-28
#------------------------------------------------------------------------------


atkin = function(n){
  
  s = c(1,7,11,13,17,19,23,29,31,37,41,43,47,49,53,59)
  
  L = 16*ceiling(n/60) 
  to_test = matrix(numeric(L), ncol=ceiling(n/60))
  
  for (i in 1:ceiling(n/60))
  {
    for (j in 1:16)
    {
      to_test[j,i] = 60*(i-1)+s[j]
    }
  }
  to_test = as.vector(to_test)
  to_test = to_test[to_test<=n]   # numbers to check for primality
  is_prime = as.logical(numeric(length(to_test)))   # initially marked as FALSE for all numbers

  rest = to_test %% 60                # vector of modulo-60 remainders
  rest1 = c(1,13,17,29,37,41,49,53)   # remainders group 1
  rest2 = c(7,19,31,43)               # remainders group 2
  rest3 = c(11,23,47,59)              # remainders group 3
  
  for (i in 1:length(to_test))
  {
    if (is.element(rest[i],rest1))    # check: remainder in group 1
    {
      fun = function(y) sqrt((to_test[i]-y^2)/4)  # quadratic form as function of y
      x = fun(1:floor(sqrt(to_test[i])))          # calculate corresponding x values
      k = x[x > 0] == round(x[x > 0])
      t = sum(k)                                  # numbers of integer x-values > 0
      
      if(t > 0 && !(round(t/2) == t/2)) is_prime[i] = TRUE
    }
    
    if (is.element(rest[i],rest2))    # check: remainder in group 2
    {
      fun = function(y) sqrt((to_test[i]-y^2)/3)
      x = fun(1:floor(sqrt(to_test[i])))
      k = x[x > 0] == round(x[x > 0])
      t = sum(k)
      
      if(t > 0 && !(round(t/2) == t/2)) is_prime[i] = TRUE
    }
    
    if (is.element(rest[i],rest3))    # check: remainder in group 3
    {
      fun = function(y) sqrt((to_test[i]+y^2)/3)
      x = fun(1:floor(sqrt(to_test[i])))
      k = x[x > 0 & x > 1:floor(sqrt(to_test[i]))] == round(x[x > 0 & x > 1:floor(sqrt(to_test[i]))])
      t = sum(k)
      
      if(t > 0 && !(round(t/2) == t/2)) is_prime[i] = TRUE
    }
  }
  primes = to_test[is_prime]
  primes_loop = to_test[is_prime]
  
  i=1
  while (primes_loop[i]^2 <= n)
  {
    primes2 = primes/primes[i]^2
    primes = primes[primes2 != round(primes2)]    # remove all non-squarefree numbers
    i=i+1
  }
  primes = c(2,3,5,primes)
  return(primes)
}



