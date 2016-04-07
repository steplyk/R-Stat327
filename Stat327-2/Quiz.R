setwd("~/Documents/Stat327/")
v = readLines(con="steplykQ1.txt")


#[begin abc]
v1=grep(pattern="^[A-C]",x=v)
v1



#[end other than a vowel]

v2=grep(pattern="^[a-zA-Z]+[^aeiou] ",x=v)
v2




# [How many people repeated a score in consecutive quizzes?]

grep(pattern = "(\\d)\\1{1,}", x = v)




#[Convert v to comma-separated values by putting commas]

ww = sub(pattern = "(\\d)(\\d)(\\d)(\\d)", replacement = "\\1,\\2,\\3,\\4", x = v)
w = gsub(" ", ",", ww)

wl = writeLines(w)
csv = read.csv("Sample.csv", header = TRUE, col.names = c("NAME", "SCORES1", "SCORES2",
                                                          "SCORES3","SCORES4","WORD"))
s = sum(csv$SCORES1) + sum(csv$SCORES2) + sum(csv$SCORES3) + sum(csv$SCORES4)
s




#[How many lines have two consecutive vowels in WORD?]

z = sapply(v, function(x) gsub(".*[0-9]\\s", "", x))
length(unlist(sapply(z, function(f) grep("[aeiou]{2}", f))))
r'\b[^aeoui]*'

yy = sub(r'\b[aeiou][^aeiou]*',r'\b[^aeiou][aeiou]*',v)
length(unlist(sapply(yy, function(y) grep("[aeiou]{2}", y))))



#
yy = sub("\\b([�aeiou])([^�aeious])",�"\\2\\1","abmm�")




# [Replace, in each WORD, the first occurrence of a vowel-consonant]

rep=sub("\\b([aeiou](^aeious))","\\2\\1","abmm")
buf <- sapply(v, function(f) gsub(".*[0-9]\\s", "", f))
reversed=sub("([aeiou])([^aeiou])","\\2\\1",buf)
length(unlist(sapply(reversed, function(rev) grep("[aeiou]{2}", rev))))
