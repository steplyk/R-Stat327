# Please write the names and email addresses ("...@wisc.edu") of your
# group members:
#
# Name: ...; email: ...@wisc.edu
# Name: ...; email: ...@wisc.edu
# Name: ...; email: ...@wisc.edu
# 
# At the end of class, one person from each group should submit a
# completed copy of this file. (Please don't submit multiple copies.)
# Here are some guidelines:
#   - This is a learning exercise designed for students without prior
#     programming experience.
#   - This is not an exam.
#   - The primary goal is to learn about writing conditional expressions,
#     loops, and functions, and about how to call apply() functions.
#   - Completing the task is only a secondary goal.
#   - You will receive full credit if your email address is on a file
#     that is turned in by your group, provided the file indicates
#     reasonable effort in solving the tasks.

rm(list=ls())

# First, here are a few exercises on the apply() family of functions.
#
# Run "?airquality" (in the Console, not in this script) to remind
# yourself of the "airquality" data frame that we've used before.
#
# 1. Use one call to an apply() function to find, for each of the
#    first four variables of airquality, the standard deviation of
#    that column, rounded to one place after the decimal point.

# ...


# 2. Use one call to an apply() function to find, for each of the
#    first four variables of airquality, an interval covering the
#    central 95% of the values of that variable, rounded to one digit
#    after the decimal point. (Note that this is not a confidence
#    interval.)

# ...



# 3. Use one call to an apply() function to find, for each month, the
#    average temperature for that month, rounded to the nearest degree.

# ...



# Second, here's an exercise on conditional expressions and loops.
#
# 4. Write the function is.prime(), below.
#
# Description: is.prime() figures out whether a number is prime
# Usage: is.prime(n)
# Parameters:
#   n: an integer (must have 0 <= n)
# Value: TRUE or FALSE, whether or not n is prime
is.prime = function(n) {
  # ...

}
# test cases
stopifnot(!is.prime(0))
stopifnot(!is.prime(1))
stopifnot( is.prime(2))
stopifnot( is.prime(3))
stopifnot(!is.prime(4))
stopifnot( is.prime(5))
for (i in 1:100) {
  if (is.prime(i)) {
    cat(sep="", i, " ")
  }
}
cat("\n")

# Third, here's a statistical problem requiring a combination of skills.
#
# 5. Write the function moving.average(), below.
# 
# Description: moving.average() computes moving averages over a vector.
# Usage: moving.average(x, n=2)
# Parameters:
#   x: a numeric vector of data values
#   n: length of moving average window (must have 1 <= n <= length(x))
# Value: a vector v such that
#   - for i in 1:(n-1), v[i] is 0 (because no moving average exists)
#   - for i in n:length(x), v[i] is the average of the n values x[i]
#     and the n-1 values that precede it.

moving.average = function(x, n=2) {
  # ...
}
# test cases
stopifnot(isTRUE(all.equal(moving.average(x=c(1, 2, 3),    n=1), c(1, 2,   3))))
stopifnot(isTRUE(all.equal(moving.average(x=c(1, 2, 3),    n=2), c(0, 1.5, 2.5))))
stopifnot(isTRUE(all.equal(moving.average(x=c(1, 2, 3),    n=3), c(0, 0,   2))))
stopifnot(isTRUE(all.equal(moving.average(x=c(1, 2, 3, 4), n=2), c(0, 1.5, 2.5, 3.5))))


# 6. Make a graph of the daily temperatures in the built-in data frame
#    "airquality" with day number (since May 1) on the x-axis and
#    temperature on the y-axis. Smooth it with 1-day, 7-day, and
#    30-day moving averages. (Well, a 1-day moving average isn't
#    smoothed at all; it's a degenerate case of smoothing.)

# ...
