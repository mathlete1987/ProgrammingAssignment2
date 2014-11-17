## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL									##initially assigning NULL to the inverse
	set_matrix <- function(y){
		x<<- y										##the argument y is saved as x in the higher environment
		inverse <<- NULL
	}
	get_matrix <- function() x 
	set_inverse <- function(solvedinverse) inverse <<- solvedinverse 	##the argument solve which will be calculated to be the value of the inverse is saved as "inverse" in the higher environment
	get_inverse <- function() inverse					##returns the inverse
	list(set_matrix = set_matrix, get_matrix = get_matrix, 
		set_inverse = set_inverse, 
		get_inverse = get_inverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		cacheSolve <- function(x,...) {						##the function takes x as the argument which is defined as the matrix in the first makeCacheMatrix function
	inverse <- x$get_inverse()						##this performs the get_inverse function which just returns the inverse from that was set to the higher environment in the set_inverse function...why do you need the x in front of getinverse? 
	if(!is.null(inverse)){							##if inverse is not null...
		message("getting cached data") 				
		return(inverse) 							##...then return the saved value for inverse from the higher environment
	}
	data <- x$get_matrix()							##get_matrix just returns the matrix x. 
	inverse <- solve(data,...)						##solve() will compute the inverse. Why can't you just do: inverse <- solve(x,...)??? 
	x$set_inverse(inverse) 							##this passes the inverse that we calculated as an argument through the set_inverse function that we defined (it will replace the argument "solvedinverse")
	inverse
}
##is this an if-else though??? I get the first part that returns the inverse if it's in the cache, but it seems like REGARDLESS we're still going through the second half of the function (everything after the if-statement)....????
	
##when you're done, fork the class respository on Github and then clone it to your desktop. It will ask where you want to save the file...the default is under thispc>documents>Github.  Then just go to that folder, open up the text file named cachemean and add it your code. 
}
