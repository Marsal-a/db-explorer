library(DBI)
library(connections)
# Create an ephemeral in-memory RSQLite database
# con <- dbConnect(RSQLite::SQLite(), ":memory:")
con_sqlite <- connection_open(RSQLite::SQLite(),":memory:")

dbWriteTable(con_sqlite, "mtcars", mtcars)
dbWriteTable(con_sqlite, "CO2", data.frame(CO2))
dbWriteTable(con_sqlite, "billboard", tidyr::billboard)

# dbListTables(con)

