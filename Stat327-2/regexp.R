# This is just the code stripped out of 5pattern.pdf.

a = c("Brown,Joe    123456789 jbrown@wisc.edu      1000",
      "Roukos,Sally 456789123 sroukos@wisc.edu     5000",
      "Chen,Jean    789123456 chen@wisc.edu       24000",
      "Juniper,Jack 345678912 jjuniper@wisc.edu  300000"
  )
grep(pattern = "j", x = a)
grep(pattern = "j", x = a, ignore.case=TRUE, value = TRUE)

sub(pattern = "e", replacement = "E", x = a)
gsub(pattern = "e", replacement = "_E_", x = a)

gsub(pattern = "[aeiou]", replacement = "", x = a) # strip vowels
gsub(pattern = "[^aeiou]", replacement = "", x = a) # strip non-vowels

grep(pattern = "^r", x = a)
grep(pattern = "^r", ignore.case = TRUE, x = a)

grep(pattern = "e\\>", x = a)

grep(pattern = "\\d{4}$", x = a) # 4 digits, end-of-line
grep(pattern = " \\d{4}$", x = a) # space, 4 digits, end-of-line
grep(pattern = " \\d{4,5}$", x = a) # space, 4 or 5 digits, end-of-line

sub(pattern="\\d{1, }" , replacement="X", x=a)
sub(pattern="\\d{1, }?", replacement="X", x=a)

link = "blah blah blah ... <a href=http://www.google.com>Google</a> blah ..."
sub(pattern=".*<a href=(.*)>.*" , replacement="\\1", x=link) # match too much
sub(pattern=".*<a href=(.*?)>.*" , replacement="\\1", x=link) # one fix
sub(pattern=".*<a href=([^>]*)>.*", replacement="\\1", x=link) # another fix
# rewrite "last,first ID email ..." to ".csv": "first,last,user,ID"
b = sub(pattern = "(\\w+),(\\w+) +(\\d+) (\\w+).*", replacement = "\\2,\\1,\\4,\\3", x=a)
# textConnection() allows a character string vector to be treated as a file
d = read.csv(file=textConnection(b), header=FALSE, col.names=c("first","last","user","ID"))

grep(pattern = "Joe|Jack", x = a)
grep(pattern = "J(o|a)", x = a)

?regex

strsplit(x=a, split=",")
strsplit(x=a, split=" +")
strsplit(x=a, split=",|( +)")
