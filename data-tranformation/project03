# install.packages("RPostgreSQL")
library(RPostgreSQL)

con <- dbConnect(
  PostgreSQL(),
  host = "arjuna.db.elephantsql.com",
  dbname = "qblhbprh",
  port = 5432,
  user = "qblhbprh",
  password = "WWq3R1NUNrG25ThsOt8WDvaym6IT3gtR"
)

dbListTables(con)

students <- data.frame(
  id = 1:5,
  student_name = c("Lisa", "Jisoo", "Rose", "Jenny", "Milli")
)

dbWriteTable(con, "students", students)

dbListTables(con)

### get data from students
df <- dbGetQuery(con, "SELECT * FROM students")

### CLOSE connection
dbDisconnect(con)
