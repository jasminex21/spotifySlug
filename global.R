### spotifySlug ###

library(spotifyr)
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(shinyjs)
library(RColorBrewer)
library(wordcloud2)
library(shinycustomloader)
library(httr)
library(plotly)
library(lubridate)

# Setting up Spotify client ID and client secret
auth = function(id, secret) {
  Sys.setenv(SPOTIFY_CLIENT_ID = id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
  access_token = get_spotify_access_token()
  options(httr_oauth_cache = T)
  print("Successful authentication!")
}
