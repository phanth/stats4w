## https://www.dcs.bbk.ac.uk/~ROGER/corpora.html
## https://www.dcs.bbk.ac.uk/~ROGER/aspell.dat

## https://stackoverflow.com/questions/21511801/text-clustering-with-levenshtein-distances
aspell <- download.file("https://www.dcs.bbk.ac.uk/~ROGER/aspell.dat",
                        dest="aspell.dat")
aa0 <- scan("aspell.dat", what=character(1))
aa <- grep("^[^$]",aa0,value=TRUE)  ## extract only *incorrect* spellings
sum(grepl("^[$]",aa0)) ## there are 450 'true' words
d  <- adist(aa)
rownames(d) <- aa
hc <- hclust(as.dist(d))
plot(hc)
## cut into *lots* of categories because there aren't a huge
## number of misspellings per word
rect.hclust(hc,k=450)
cc <- cutree(hc,k=450)
library(dplyr)
df <- tibble(word=names(cc),clust=cc)
df2 <- df %>%
    add_count(clust) %>%
    arrange(desc(n),clust)
View(df2)
## seems to do pretty well: "faver" and "faxe" are lumped together ...
## so are "alchohol" and "alchoholic" and "rsx" and "ast" ...

