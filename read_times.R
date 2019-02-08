
system.time(loan <- read.csv("loan.csv"))  ##
system.time(loan <- readr::read_csv("loan.csv"))
system.time(loan <- data.table::fread("loan.csv",data.table=FALSE))
