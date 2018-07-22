library(dplyr)
library(ggplot2)
#pnorm(63000, 51500, 4000, lower.tail = FALSE)
#pnorm(45000, 51500, 4000)
#pnorm(55000, 51500, 4000) - pnorm(45000, 51500, 4000)
setwd("/media/anton/Data/R")
movie_body_counts<-read.csv('filmdeathcounts.csv')#, dec = ',')

head(movie_body_counts)
str(movie_body_counts)

movie_body_counts$body_per_min<-movie_body_counts$Body_Count/movie_body_counts$Length_Minutes
ggplot(movie_body_counts, aes(x=Body_Count))+
  geom_histogram(bins=20, color="grey",fill="lightblue")

movie_body_counts %>%
  top_n(n = 10, Body_Count) %>%
  arrange(desc(Body_Count))

movie_body_counts %>%
  top_n(n = 10, body_per_min) %>%
  arrange(desc(body_per_min))

ggplot(movie_body_counts, aes(x=IMDB_Rating))+
  geom_histogram(bins=10, color="grey", fill="lightblue")

imdb_mean<-mean(movie_body_counts$IMDB_Rating)
imdb_sd<-sd(movie_body_counts$IMDB_Rating)

set.seed(900)
imdb_simulation <- rnorm(n = nrow(movie_body_counts), mean = imdb_mean, sd = imdb_sd)

movie_body_counts$imdb_simulation <- imdb_simulation

ggplot(movie_body_counts, aes(x=imdb_simulation)) +
  geom_histogram(bins=10, color="grey", fill="lightblue")

ggplot(movie_body_counts, aes(sample=imdb_simulation))+
  stat_qq()

ggplot(movie_body_counts, aes(sample=IMDB_Rating))+
  stat_qq()

pnorm(8.0, imdb_mean, imdb_sd) - pnorm(4.0, imdb_mean, imdb_sd)

lm(formula = movie_body_counts$Body_Count ~ movie_body_counts$IMDB_Rating)
cor(movie_body_counts$Body_Count, movie_body_counts$IMDB_Rating)

pnorm(2.575, lower.tail = FALSE)
