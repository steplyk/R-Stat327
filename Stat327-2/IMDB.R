rm(list=ls())
require("XML")
imdb="http://www.imdb.com/chart/top?sort=ir, desc"
imdb.page=readLines(imdb)
g = grep(pattern = "/title/tt", x = imdb.page) 
imdb.lines=imdb.page[g]
tt <- htmlParse('http://www.imdb.com/chart/top?sort=ir,desc');
#xml_val=xpathSApply(tt, "//td[@class='titleColumn']//a", xmlValue)
#xml_attr=xpathSApply(tt, "//td[@class='titleColumn']//a", xmlAttrs)
movies.urls=xpathSApply(tt, "//td[@class='titleColumn']//a", xmlAttrs)[1, ]
producer.list=list()

for (movie.url in movies.urls[1:2]) {
  print(movie.url)
  tyt=sub('.*/title/(.*)/.*', '\\1', movie.url)
  imdb.url = paste(sep="", "http://www.imdb.com/title/", tyt,"/fullcredits")
  print(imdb.url)
  movie.tables = readHTMLTable(imdb.url)
  movie.producers=as.character(movie.tables[[4]]$V1)
  producer.list[[length(producer.list)+1]] <- movie.producers
  named.list=unlist(producer.list)
}

tabled.named=table(named.list)
head(sort(tabled.named,decreasing=TRUE),5)
tail(sort(tabled.named),5)
