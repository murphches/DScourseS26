library(tidyverse)
library(jsonlite)
library(dplyr)
library(sparklyr)

sc <- spark_connect(master = "local")

df1 <-as_tibble(iris)
df <- copy_to(sc, df1)
#This is where my code stopped working. It kept saying that sc wasn't an object.
#My df1 got loaded in, but I couldn't get sc to work. I tried to load sparklyr again, 
#but it didn't work. I also tried to restart R and load sparklyr again.

df %>% select(Sepal_Length,Species) %>% head%>% print

df %>% filter(Sepal_Length>5.5) %>% head %>% print