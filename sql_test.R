install.packages("RSQLite")
install.packages("dplyr")

library(RSQLite)

setwd("/db")
my_db1 <- src_sqlite("pitchRx.sqlite3", create = FALSE)

print(src_tbls(my_db1))
print(tbl(my_db1, "pitch"))
head(tbl(my_db1, "pitch"),3)
