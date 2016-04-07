# Name: Tess Steplyk
# Email: steplyk@wisc.edu

# We'll grade your homework by running
#   source("hw2.R")
# in a directory containing a "groceries.csv" grocery price list file
# (which will be different from the one you used), giving user input,
# checking your output, and reading your code.
#
# To make it possible partly to automate grading, your output strings
# should exactly match the specified output strings.

rm(list = ls()) # remove all variables
layout(1)    # set graphical device to default state
par(pty="m") # set graphical device to default state


# ----------------------------------------------------------------------
# Part 1: Write the floor.log2() function described below.
#
# Description: floor.log2(n) computes the largest integer exponent, e,
# such that 2^e <= n. That is, it computes floor(log(n, base=2)).
#
# Usage: floor.log2(n)
# Parameter: n, a number (must be >= 1)
# Value: floor(log(n, base=2)
# Examples:
#   floor.log2(1) is 0
#   floor.log2(2) is 1
#   floor.log2(3) is 1
#   floor.log2(4) is 2
#   floor.log2(5) is 2
#   floor.log2(6) is 2
#   floor.log2(7) is 2
#   floor.log2(8) is 3
# (Use a loop to accumulate a product. You may not use "^" or log(),
# etc. Use stopifnot() to check the argument according to the "must"
# statement above.)

floor.log2 = function(n) {
  stopifnot(n>=1)
  count=0
  if(n<2)
    return(0)
  while(n%/%2 != 0) {
    n = n/2
    count = count+1
  }
  return(count)
}
# Write code to confirm that floor.log2(n) matches floor(log(n, base=2))
# on integer inputs from 1 to 1000.

for(i in 1:1000)
  stopifnot(isTRUE(all.equal(floor.log2(i),floor(log2(i)))))

# ----------------------------------------------------------------------
# Part 2: Run a Monte Carlo simulation to estimate pi.

# ----------------------------------------------------------------------
# Description: magnitude returns the distance a of point (x, y) from
#   the origin (or a vector of such distances if x and y are vectors).
# Usage: magnitude(x, y)
# Arguments:
#   x: x-coordinate of one point (or x-coordinates of several points)
#   y: y-coordinate of one point (or y-coordinates of several points)
#      (must have same length as x)
# Value: distance(s) of (x, y) from the origin.
# Hint: don't forget to check that x and y have the same length.

magnitude = function(x, y) {
  stopifnot(isTRUE(all.equal(length(x), length(y))))
  return(sqrt(x^2 + y^2))
}

# Here are some test cases, first passing just one (x, y) point.
stopifnot(isTRUE(all.equal(magnitude(3,  4), 5)))
stopifnot(isTRUE(all.equal(magnitude(3, -4), 5)))
stopifnot(isTRUE(all.equal(magnitude(0, 0), 0)))
stopifnot(isTRUE(all.equal(magnitude(-1, 1), sqrt(2))))
# Now test passing vectors of points (x, y). (Note that R's arithmetic
# operators act on vectors, so a loop isn't necessary to handle this
# test case.)
test.x   = c(3,  3, 0,      -1,  5)
test.y   = c(4, -4, 0,       1, 12)
test.mag = c(5,  5, 0, sqrt(2), 13)
stopifnot(isTRUE(all.equal(magnitude(test.x, test.y), test.mag)))

# Description: monte.carlo.pi estimates pi by simulation.
# Usage: monte.carlo.pi(n, draw=FALSE)
# Arguments:
#   n: number of random points to use
#   draw: logical, whether or not to draw a graph of the simulation
#     consisting of the n points and the unit circle.
# Details: The simulation proceeds by getting n random points {(x, y)}
#   in the square defined by -1 < x < 1 and -1 < y < 1, and then
#   counting how many of those points are within a radius 1 of (0, 0).

#   We can use
#     (area of circle)/(area of square) = pi*r^2  / (2r)^2
#                                       = pi(1^2) / (2*1)^2, since r=1
#                                       = pi/4
#   so
#     pi = 4*(area of circle)/(area of square)
#        =~ 4*(#points in circle) / (#points in square)
# Value: an estimate of pi.
# Hints:
#   - ?runif tells how to get random coordinates in the range (-1, 1)
#   - You can see a picture like the required drawing at the top of
#     http://en.wikipedia.org/wiki/Monte_Carlo_Integration. (Don't
#     worry if you don't understand the article, which isn't easy.)
#   - Using R's base graphics, you can plot the points via plot(x, y).
#   - One way to draw the circle is to get a sequence of angles (in
#     radians) from 0 to 2*pi, and then use x coordinate cos(angle)
#     and y coordinate sin(angle).
monte.carlo.pi = function(n, draw=FALSE) {  
  circlep <- 0
  squarep <- 0
  x <- runif(n,-1,1)
  y <- runif(n,-1,1)
  for (i in 1:n)
  {
    #if ((x[i])^2 + (y[i])^2 <=1)
    if (magnitude(x[i],y[i])<=1)
    {
      circlep<-circlep+1
      squarep<-squarep+1
    } 
    else{
      squarep<-squarep+1
    }
  }
  if (draw==TRUE) {
    plot.new()
    frame()
    plot(x, y, asp=1, xlim=c(-1,1), ylim=c(-1,1))
    rect(-1,-1, 1, 1)
    theta <- seq(0, 2*pi, length.out=100)
    lines(cos(theta),sin(theta))
  }
  return(as.numeric(4*circlep / squarep))
}
cat(sep="", "monte.carlo.pi(1000)=", monte.carlo.pi(1000), "\n") # get estimate
monte.carlo.pi(1000, TRUE) # make graph
Sys.sleep(2) # sleep 2 seconds to let user see graph

# Finally, make a graph to show the distribution of pi estimates for
# n=100 and n=1000. Use replicate() to get 500 pi estimates using
# n=100 each time. Then use replicate() to get 500 pi estimates using
# n=1000 each time. Draw a graph comparing these two distributions.
# (You can use a reasonable graph of your choice.)

x=replicate(100, monte.carlo.pi(500))
y=replicate(1000, monte.carlo.pi(500))
plot(density(x))
lines(density(y))


Sys.sleep(2) # sleep 2 seconds to let user see graph

# ----------------------------------------------------------------------
# Part 3: Manage a grocery list.
#
# Write a function, how.many, that takes two arguments, item
# (character string) and n.max (numeric). Display the prompt,
# "How many item?", and then require the user to enter desired number
# of item, an integer between 0 and n.max. (Assume the user will enter
# a nonnegative integer.) If user enters a number larger than n.max,
# display the message, "  ERROR: too many for the budget" and start
# again. Return the user's number.
#
# e.g. the call
#   how.many("apple", 4)
# prints "How many apple?" and returns a number between 0 and 4.

how.many <- function(item, n.max) {
  x <- cat("How many", item,"?", sep=" ") 
  x <- as.numeric(readline(x))
  if(item>n.max) {
    print("ERROR: too many for the budget")
    x <- cat("How many", item,"?", sep=" ") 
    x <- as.numeric(readline(x))
  }
  return(x)
}

# Write a function, grocery.list, that takes two arguments, file (a
# character string file name) and budget (numeric). From file, read a
# grocery price list containing items and prices, like this:
#   spinach,2.00
#   rice,3.00
#   toilet paper,4.00
#   bread,2.40
#   milk,3.10
#   apple,0.40
#
# Display the price list. Loop through it asking how many of each item
# should be purchased. (Use your how.many() function.)
#
# budget is the maximum amount that can be spent. Do not accept a
# users's number of an item if it causes the budget to be exceeded. Do
# not display an item at all if it's price is higher than the
# remaining budget.
#
# Return a data frame consisting of three columns, "item", "price",
# and "quantity", and those rows with nonzero quantities.

grocery.list = function(file, budget) {
  curr.budget <- budget
  count <- 0
  purfinal <- 0
  purchased <- data.frame(count= numeric(0), item= character(0), price = numeric(0),quantity=numeric(0))
  groceries <- read.csv(file, header = FALSE)
  colnames(groceries)<-c("item","price")
  matrix = as.matrix(groceries)
  colnames(matrix) <- NULL
  m.dim=dim(matrix)
  eric = as.numeric(matrix[i,2])
  for (i in 1:m.dim[1]){  
    if ((curr.budget - eric) > 0) {
      number <- how.many(matrix[i,1], curr.budget/eric)
      curr.budget <- curr.budget - (n.max * eric)
      while (number>0){
        count <- count + 1
        newrow <- data.frame(item=matrix[i,1], price=eric, quantity=n.max)
        purchased<-rbind(purchased, newrow) }  
    } 
  }
  colnames(purchased) <- c("item","price","quantity" ) 
  print(purchased)
  purmatrix <- as.matrix(purchased)
  colnames(purmatrix) <- NULL
  rownames(purmatrix) <- NULL
  purdim <- dim(purmatrix)
  for (i in 1:purdim[1])
    purfinal <- purfinal + ((as.numeric(purmatrix[,2])) * (as.numeric(purmatrix[,3])))
  return(purfinal)  
}
 

# Finally, write a few lines of code to call your grocery.list() on a
# "groceries.csv" file with a budget of $10. Print the returned
# shopping list (data frame) along with the total bill, in a line of
# the form, "Your bill is $n", where n is a number.

groc <- grocery.list(file="groceries.csv", budget=10)
bill <- cat("Your bill is $", groc, sep =" ")
print(bill)

# e.g. Here's a sample session:
#   > source("hw2.R")
#             item price
#   1      spinach   2.0
#   2         rice   3.0
#   3 toilet paper   4.0
#   4        bread   2.4
#   5         milk   3.1
#   6        apple   0.4
#   How many spinach?
#   1: 2
#   How many rice?
#   1: 1
#   How many bread?
#   1: 1
#   How many apple?
#   1: 2
#     ERROR: too many for the budget
#   How many apple?
#   1: 1
#        item price quantity
#   1 spinach   2.0        2
#   2    rice   3.0        1
#   3   bread   2.4        1
#   4   apple   0.4        1
#   Your bill is $9.8
#   >

Sys.sleep(2) # sleep 2 seconds to let user see grocery list output

# ----------------------------------------------------------------------
# Part 4: Practice with apply() family of functions.
#
# I've provided three functions in "hw4freeCode.R". Read through the
# file quickly, and then use its functions below. R will read them
# into the current session here:

source("hw2freeCode.R") # Don't change this line.

# The file "scores.csv" contains simulated scores data for a
# statistics course. Some of its column names are abbreviations:
#   - "Poss" means "Possible" (total points possible)
#   - "Pct" means "Percentage"
#   - "Pctile" means "Percentile"

# Read the file into a data frame.

score = read.csv("scores.csv")

# Add three columns to the "scores" data frame:
#   - Pct.gr ("gr" abbreviates "grade"), the letter grade given by the
#     PercentageGrade() function on the Pct column. Hint: use sapply()
#   - Pctile.gr, the letter grade given by the PercentileGrade()
#     function on the Pctile column.
#   - Grade, the letter grade given by the CourseGrade() function on
#     your first two new columns. Hint: use mapply(). By default, it
#     will return a named character (string) vector. You can get rid
#     of the names with one of
#     - USE.NAMES=FALSE as a mapply() argument
#     - names(x) = NULL on the returned vector
#     - as.character() on the returned vector
# Display the data frame.

newscore <- cbind(score, Pct.gr = c(sapply(score$Pct, PercentageGrade)), Pctile.gr = c(sapply(score$Pctile, PercentileGrade)))
Grade <- (mapply(CourseGrade, newscore$Pct.gr, newscore$Pctile.gr, USE.NAMES = FALSE))
newscore <- cbind(newscore, Grade)
newscore

# Display the average E1 score, rounded to one place after the decimal
# point, for each group of students with the same course grade. (There
# should be an average E1 score for the A students, an average for the
# AB students, etc.) Hint: use tapply().

avgE <- tapply(newscore$E1, newscore$Pct.gr, mean)
round(avgE, digits = 1)

# Display the average score (across all students) for each quiz
# (column). That is, display four quiz averages. Hint: use apply().

quiz <- data.frame(Q1 = newscore$Q1, Q2 = newscore$Q2, Q3 = newscore$Q3, Q4 = newscore$Q4)
apply(quiz, 2,mean)

# Display the average quiz score (across Q1 through Q4) for each
# student (row). That is, display 25 student averages. Hint: use
# apply().

studquiz <- apply(newscore[,1:4], 1, mean)
studquiz
