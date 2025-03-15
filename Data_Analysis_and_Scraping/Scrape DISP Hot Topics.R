#install.packages("httr")
#install.packages("XML")
library(httr)
library(XML)

html <- htmlParse(GET("https://disp.cc/m/"))

ht_title <- xpathSApply(html, "//div[@class='ht_title']", xmlValue)
ht_desc <- xpathSApply(html, "//div[@class='ht_desc']", xmlValue)
#print(ht_title)
#print(ht_desc)

sep <- rep("---", times = length(ht_desc))
data <- cbind(ht_title,ht_desc, sep)
data

file.path <- "111700014.txt"
write.table(
  data,
  file =file.path,
  append = F,
  quote = F,
  sep = "\n",
  row.names = F,
  col.names = F,
)


   