setwd("~/Google\ Drive/GitHub/Project\ 2\ test/")
# head(work.data$INSTURL)
n = nrow(work.data)

count = 0
for (i in 1:n) {
  if (str_sub(work.data$INSTURL[i],-1,-1) == "/") {
    work.data$INSTURL[i] = str_sub(work.data$INSTURL[i],1,-2)
    count = count + 1
    print(count)
  }
}

save(work.data, file = "data/WorkData.Rdata")

