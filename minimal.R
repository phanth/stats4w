library("RSQLite")
dataframe = "lending-club-loan-data/database.sqlite"
loan.sqlite = dbConnect(SQLite(),dbname=dataframe)
tmp = dbGetQuery(loan.sqlite,'select title from loan')
## omit NA values
title <- na.omit(tmp$title)
utitle <- sort(unique(trimws(tolower(tmp$title))))
length(utitle)

v <- seq(100,1500,by=100)
tt <- sapply(v,
             function(n) {
                 cat(n,"\n")
                 system.time(adist(utitle[1:n],
                 ignore.case = TRUE,costs=c(i=1,d=1,s=2)))[["elapsed"]]
             })
par(las=1)
plot(v,tt,type="b",xlab="size",ylab="time",log="xy")
coef(lm(log(tt)~log(v)))
## time goes approximately as n^2 (no big surprise ...)

ngrp <- 100
nv <- length(utitle)
svec <- rep(1:ngrp,each=ceiling(nv/ngrp))
svec <- svec[1:nv]
spl_title <- split(utitle, svec)
sum(lengths(spl_title))
mean(lengths(spl_title)) ## about 500 obs per group

x <- spl_title[[1]]
a <- adist(x,ignore.case = TRUE,costs=c(i=1,d=1,s=2))
rownames(a) <- x
hc <- hclust(as.dist(a))

cc <- cutree(hc,k=200)
library(dplyr)
df <- tibble(word=names(cc),clust=cc)
df2 <- df %>%
    add_count(clust) %>%
    arrange(desc(n),clust)
View(df2)

## hmm.  a huge fraction of the first chunk (which are alphabetically
## first) are car loans.  Do these need to be split out separately
brands <- c("jeep","dodge","ford","mitsubishi","bmw","chevrolet",
            "mustang","volskwagen","vette","pontiac","buick",
            "nissan","audi","camry","toyota","highlander")
## OK, only 49 values ...
year_str <- "((19|20)[0-9]{2})"
## year before or after brand ...
car_str <- paste0(year_str,
                 "? ?(",
                 paste(brands,collapse="|"),
                 ") ?",
                 year_str,"?")
sum(grepl(car_str,utitle)) ## 223 values

grep("(19|20)[0-9]{2}",utitle,value=TRUE)
## maybe even DELETE year values? 
## that would make a lot of values like "2003 refinance", "refinance 2010"
##  be more comparable



