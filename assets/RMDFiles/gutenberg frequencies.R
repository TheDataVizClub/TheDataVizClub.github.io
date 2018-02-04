---
title: "Untitled"
author: "Taha Monfared"
date: "February 4, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(gutenbergr)
library(dplyr)
library(tidytext)
library(stringr)
#to get an Excel file of all the works in Project Gutenberg
write.csv(x = gutenberg_works(), file = "works.csv")

#want Treasure Island
gutenberg_metadata %>% filter(title == "Treasure Island") #120

#want Wuthering Heights
gutenberg_metadata %>% filter(title == "Wuthering Heights") #768

#Want Adventures of Sherlock Holmes
gutenberg_works(str_detect(title, "Adventures of Sherlock Holmes")) #1661

#want Taxidermy w/o a teacher
gutenberg_works(str_detect(title, "Taxidermy without")) #51439

#want Great expectations
gutenberg_metadata %>% filter(title == "Great Expectations") #1400

#want Adventures of Huck Finn
gutenberg_works(str_detect(title, "Huckleberry Finn")) #76

#download books
ti <- gutenberg_download(120)
wh <- gutenberg_download(768)
sh <- gutenberg_download(1661)
tx <- gutenberg_download(51439)
gh <- gutenberg_download(1400)
hf <- gutenberg_download(76)

#word counts

tidy_ti <- ti %>% unnest_tokens(word, text) %>% anti_join(stop_words)
ti.count <- tidy_ti %>% count(word, sort = TRUE)

word.count <- function(s){
  name <- paste0("tidy_",s)
  tib <- s %>% unnest_tokens(word, text) %>% anti_join(stop_words)
  name.count <- paste0(s,".count")
  name.count <- tib %>% count(word, sort = TRUE)
  return(nc = name.count[1:10,])
}

w1 <- word.count(ti)
w2 <- word.count(wh)
w3 <- word.count(sh)
w4 <- word.count(tx)
w5 <- word.count(gh)
w6 <- word.count(hf)

library(ggplot2)
library(gridExtra)

countplot2 <- function(w, t, tzeva){
  ggplot(data = w, aes(x = reorder(word,n), y = n)) + geom_col(fill = tzeva) + 
    labs(x = "10 Most Frequent Words", y = "Count", title = t) + coord_flip()
}

p1 <- countplot2(w1, "Treasure Island", "royalblue2")
p2 <- countplot2(w2, "Wuthering Heights", "slategray")
p3 <- countplot2(w3, "The Adventures of Sherlock Holmes", "seagreen")
p4 <- countplot2(w4, "Taxidermy without a Teacher", "chocolate3")
p5 <- countplot2(w5, "Great Expectations", "magenta4")
p6 <- countplot2(w6, "The Adventures of Huckleberry Finn", "firebrick1")

grid.arrange(p1,p2,p3,p5,p4,p6, ncol = 2)

```

