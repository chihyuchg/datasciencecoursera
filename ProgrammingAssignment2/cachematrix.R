## Put comments here that give an overall description of what your
## functions do


#The function creates a list that contains 4 member functions: #set, get, set_inv and get_inv. The operator <<- are normally #only used in the function, and cause a search to made through #parent environments for an existing definition of the variable #being assigned. If such a variable is found and otsbinding is #not locked, then its value is refined, otherwise assignment #takes place in the global environment.

makeCacheMatrix <- function(x = matrix()) {

      x_inv <- NULL #The result of inversion is stored in x_inv.
      #The set function uses this to set a matrix to object created by makeCacheMatrix function.
      
      set <- function(y) {
      	x <<- y
	  	x_inv <<- NULL #Initialize xinv to null
      }

      get <- function() x #Return the input matrix
      
      set_inv <- function(inv) x_inv <<- inv #Set the inversed matrix
      get_inv <- function() x_inv #Return the inversed matrix
      
      #Return a list contained the four functions           
      list(set = set, get = get,
	       set_inv = set_inv,
	       get_inv = get_inv)
  }

cacheSolve <- function(x, ...) {
      m <- x$get_inv() #Get the inversed matrix from object x

	# Check if the inversion result is existing
      if(!is.null(m)) { 	  
      	message("getting cached data")
	  	return(m) #Return inversed result
      }
      data <- x$get()
      m <- solve(data)
      x$set_inv(m) #Set it to the object
      m #Return the solved result
  }
