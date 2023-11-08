install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(corrplot)
tinytex::install_tinytex()
install.packages('plyr', repos = "http://cran.us.r-project.org")

#Loading my dataset
setwd('C:/Users/there/OneDrive/Desktop/Phase1')
songs <- read.csv("spotify_songs.csv", header =TRUE)
View(songs)

#preprocessing : Removing rows with Missing data 
which(is.na(songs))
sum(is.na(songs))
na.omit(songs)

#Removing rows with repetitive entries 
clean_songs <-songs[!duplicated(songs[c("track_name", "track_artist")]), ]
View(clean_songs)

#see types of the attributes
str(clean_songs)

#Visualization and exploration Techniques

#visualizing the count of songs in each level of popularity 
ggplot(clean_songs, aes(x = track_popularity)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") 

#Danceability vs. popularity 
ggplot(data = clean_songs) +
  geom_point(mapping = aes(x = danceability, y = track_popularity, color=playlist_genre)) +
  geom_smooth(mapping = aes(x = danceability, y = track_popularity))

ggplot(data = clean_songs, mapping = aes(x = danceability, y = track_popularity, fill=playlist_genre)) +
  geom_boxplot()

#Energy vs popularity 
ggplot(data = clean_songs) +
  geom_point(mapping = aes(x = energy, y = track_popularity, color=playlist_genre)) +
  geom_smooth(mapping = aes(x = energy, y = track_popularity))


#Valence vs popularity 
ggplot(data = clean_songs) +
  geom_point(mapping = aes(x = valence, y = track_popularity, color=playlist_genre)) +
  geom_smooth(mapping = aes(x = valence, y = track_popularity))

ggplot(data = clean_songs) +
  geom_point(mapping = aes(x = valence, y = danceability)) +
  geom_smooth(mapping = aes(x = valence, y = danceability))

#Tempo popularity 
ggplot(data = clean_songs) +
  geom_point(mapping = aes(x = tempo, y = track_popularity, color=playlist_genre)) +
  geom_smooth(mapping = aes(x = tempo, y = track_popularity))

#Correlations

correlation_matrix <- cor(clean_songs[c("track_popularity", "danceability", "energy", "valence", "tempo")])
corrplot(correlation_matrix, method = "color")

cor.test(clean_songs$danceability, clean_songs$track_popularity)
cor.test(clean_songs$energy, clean_songs$track_popularity)
cor.test(clean_songs$valence, clean_songs$track_popularity)

#Multiple Linear Regression Model 
mlr_model <- lm (track_popularity ~ danceability + energy + valence, data= clean_songs)
summary(mlr_model)



