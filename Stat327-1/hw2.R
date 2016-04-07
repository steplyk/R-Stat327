# You should replace all occurrences of "..." with your
# code. If you'd like to replace a single "..." with more
# than one line of code (using your own variables), you may
# certainly do that. Please don't remove all my comments, as they
# help orient us when we're reading your code.
#
# My solution to this homework uses information from the first two
# handouts and the Vector portion of the third handout.
#
# We'll grade your homework by running this ".R" file via
#   source("hw2.R")
# (in a directory containing the data files) and reading your code.

# Name: Tess Steplyk
# Email: steplyk@wisc.edu

rm(list = ls()) # Remove all objects defined in workspace.

# Download WI_donations and NY_donations from the class website. (Do
# this outside of R. They are donations from Wisconsin and New York to
# the 2012 presidential candidates. I got them from files at
# www.fec.gov/disclosurep/PDownload.do, a Federal Election Commission
# website.)
#
# Read these two files into two vectors, one for each state.

x<-scan("NY_tiny.txt")
y<-scan("WI_tiny.txt")

x<-scan("NY_donations.txt")
y<-scan("WI_donations.txt")

# Find the sum of donations from WI and the sum of donations from NY.

WI.sum = sum(x,na.rm = FALSE) # ... set this variable correctly
NY.sum = sum(y,na.rm = FALSE) # ... set this variable correctly
cat(sep="", "WI sum=", WI.sum, "\n")
cat(sep="", "NY sum=", NY.sum, "\n")

# Find the sum of small donations from each state. I've set small=100
# (below). Let's say "small donation" are those such that
#   -100 <= donation <= 100.
# (Note that there are negative donations, which are refunds.)

small = 100
WI.sum.small = sum(y[(y <= small) & (y >= -small)],na.rm=TRUE) # ... set this variable correctly
NY.sum.small = sum(x[(x <= small) & (x >= -small)],na.rm=TRUE) # ... set this variable correctly
cat(sep="", "WI.sum.small=", WI.sum.small, "\n")
cat(sep="", "NY.sum.small=", NY.sum.small, "\n")

# Find the sum of big donations from each state. Let's say "big
# donations" are those such that
#   donation < -100 or donation > 100.

WI.sum.big = sum(y[(y > 100 )| (y < -100)] ,na.rm=TRUE) # ... set this variable correctly
NY.sum.big = sum(x[(x > 100 )| (x < -100)] ,na.rm=TRUE) # ... set this variable correctly
cat(sep="", "WI.sum.big=", WI.sum.big, "\n")
cat(sep="", "NY.sum.big=", NY.sum.big, "\n")

# Find the ratio of the sum of small donations to total donations from
# each state.
WI.small.ratio = WI.sum.small/WI.sum # ... set this variable correctly
NY.small.ratio = NY.sum.small/NY.sum # ... set this variable correctly
cat(sep="", "WI.small.ratio=", WI.small.ratio, "\n")
cat(sep="", "NY.small.ratio=", NY.small.ratio, "\n")

# Find the mean positive donation from each state. (Do not include
# zero or negative donations.)
NY.positive=x[x>0]
WI.positive=y[y>0]
WI.mean.pos = mean(WI.positive) # ... set this variable correctly
NY.mean.pos = mean(NY.positive) # ... set this variable correctly
cat(sep="", "WI.mean.pos=", WI.mean.pos, "\n")
cat(sep="", "NY.mean.pos=", NY.mean.pos, "\n")

# Find the median positive donation from each state. (Do not include
# zero or negative donations.)
WI.median.pos =median(WI.positive)  # ... set this variable correctly
NY.median.pos =median(NY.positive) # ... set this variable correctly
cat(sep="", "WI.median.pos=", WI.median.pos, "\n")
cat(sep="", "NY.median.pos=", NY.median.pos, "\n")

# Find the largest and second-largest donation from each state. Write
# code to produce formatted output exactly like this (except that the
# numbers should be correct, not 0):
#
# WI.largest=0, WI.second.largest=0
# NY.largest=0, NY.second.largest=0

WI.sort = sort(y)
WI.sort.length = length(y)
WI.large.don = WI.sort[WI.sort.length]
WI.sec.large = WI.sort[WI.sort.length-1]
cat(sep="", "WI.largest=",WI.large.don, ", ","WI.second.largest=",WI.sec.large,"\n")

NY.sort = sort(x)
NY.sort.length = length(y)
NY.large.don = NY.sort[NY.sort.length]
NY.sec.large = NY.sort[NY.sort.length-1]
cat(sep="", "NY.largest=",NY.large.don, ", ","NY.second.largest=",NY.sec.large,"\n")

# Note that your code should work on the current data set, and also on
# a new data set. For example, if I ask for a sum, don't use "17",
# even if the sum is 17, because 17 will probably be wrong for a new
# data set. Instead, use sum(...), since this second solution will be
# correct even for a new data set.
#
# I recommend checking that your code works on the files WI_tiny and
# NY_tiny, which are fake data files for which you can check your
# computations by hand. Get your code working on them, and then switch
# to the real data files specified above. Your submitted code should
# use the real files, "WI_donations" and "NY_donations".
