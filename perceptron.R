x1 <- runif(30,-1,1)
x2 <- runif(30,-1,1)
x <- cbind(x1,x2)

calculate_distance <- function(x,w,b){
  sum(x*w)+b
}

linear_classifier <- function(x,w,b){
  distance <- apply(x, 1,calculate_distance,w,b)
  return(ifelse((distance<0),-1,1))
}

# calculate_distance calculates the distance of each point
# linear_classifier gives the cateory to which te point belongs

#function to calculate 2nd norm
second_norm = function(x) {
  sqrt(sum(x * x))
}
#perceptron training algorithm
perceptron = function(x, y, learning_rate=1) {
  w = vector(length = ncol(x)) # initialize w
  b = 0 # Initialize b
  k = 0 # count iterations
  #constant with value greater than distance of furthest point
  R = max(apply(x, 1, second_norm)) 
  incorrect = TRUE # flag to identify classifier
  #initialize plot
  plot(x,cex=0.2)
  #loop till correct classifier is not found
  while (incorrect ) {
    incorrect =FALSE 
    #classify with current weights
    yc <- linear_classifier(x,w,b)
    #Loop over each point in the input x
    for (i in 1:nrow(x)) {
      #update weights if point not classified correctly
      if (y[i] != yc[i]) {
        w <- w + learning_rate * y[i]*x[i,]
        b <- b + learning_rate * y[i]*R^2
        k <- k+1
        #currect classifier's components
        # update plot after ever 5 iterations
        if(k%%5 == 0){
          intercept <- - b / w[[2]]
          slope <- - w[[1]] / w[[2]]
          #plot the classifier hyper plane
          abline(intercept,slope,col="red")
          #wait for user input
          cat ("Iteration # ",k,"\n")
          cat ("Press [enter] to continue")
          line <- readline()
        }
        incorrect =TRUE
      }
    }
  }
  s = second_norm(w)
  #scale the classifier with unit vector
  return(list(w=w/s,b=b/s,updates=k))
}

#train the perceptron
p <- perceptron(x,1)
