#######################################
######  Sieve of Eratosthenes  ########
#######################################


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

# Tests:
library(gmp)
isprime(eratosthenes(1000))

length(eratosthenes(1000))
length(isprime(1:1000)[isprime(1:1000)==2])

# Results for 100 million:

start = Sys.time()
eratosthenes(10^8)
print(Sys.time() - start)



