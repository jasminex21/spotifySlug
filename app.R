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

# Setting up Spotify client ID and client secret
auth = function(id, secret) {
  Sys.setenv(SPOTIFY_CLIENT_ID = id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
  access_token = get_spotify_access_token()
  print("Successful authentication!")
}

ui = navbarPage(title = span(div(img(src = "spotifySlugLogo.jpg", 
                                     height = 30,
                                     width = 30), 
                                 strong("Spotify Slug")), style = "color: #1DB954"),
                windowTitle = "Spotify Slug",
                inverse = T,
                useShinyjs(),
                includeCSS("www/styles.css"),
                # Panel 1 - Introduction and Authentication)
                tabPanel(title = span(strong("Welcome!"), style = "color: #1DB954"),
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(h4(strong("First steps: Authentication")), 
                                          p("To look at your personal Spotify data, you will need to log in to ", a(href = "https://developer.spotify.com/", "Spotify for Developers,"), 'go to "Dashboard," and click "Create an App." This will prompt you to give your app a name and a description. Once your app is set up, you will be able to access a "Client ID" and "Client Secret" code - this is what Spotify Slug needs for your authentication.'), 
                                          hr(),
                                          textInput("id", 
                                                    label = "Enter your Client ID"), 
                                          textInput("secret", 
                                                    label = "Enter your Client Secret"), 
                                          actionButton("authButton", 
                                                       label = "Authenticate!"), 
                                          br(), 
                                          br(),
                                          strong(tags$span(style = "color: #579559"), 
                                                 textOutput("validate_message")),
                                          br()),
                             mainPanel(h3(strong("Welcome to Spotify Slug!")), 
                                       p("This is a web application that analyses patterns in a user's Spotify listening data by accessing the publicly-available ", a(href = "https://developer.spotify.com/documentation/web-api/", "Spotify API."), " Authenticate your account and start exploring!"),
                                       p("Created by Spotify user ", a(href = "https://open.spotify.com/user/bananaslug-10?si=a04c3557683c45bf", "bananaslug-10.")),
                                       div(img(src = "spotifySlugLogo.jpg", 
                                               height = 500, 
                                               width = 500), 
                                           style = "text-align: center;"))
                           )
                         )), 
                
                # Panel 2 - Top Artists and associated stats
                tabPanel(title = span(strong("Your Top Artists"), style = "color: #1DB954"), 
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(h3(strong("Who are your favorite artists?")), 
                                          p("Select a time frame over which to view your top 50 artists. Please note that the time frames are ", em("approximate"), " and that data may take time to update."), 
                                          hr(), 
                                          selectInput("artistsTime", 
                                                      label = "Select a time frame", 
                                                      choices = c("last 4 weeks" = "short_term", 
                                                                  "last 3 months" = "medium_term", 
                                                                  "all time" = "long_term"), 
                                                      selected = "short_term"),
                                          hr(), 
                                          h4(strong("Your Top Genres")), 
                                          helpText("Hover over any genre in the wordcloud for details!"),
                                          wordcloud2Output("genresCloud")),
                             mainPanel(h2(strong("Your Top Artists")), 
                                       br(),
                                       withLoader(dataTableOutput("artistsTable"), 
                                                  type = "html", 
                                                  loader = "pacman"), 
                                       br())
                           )
                         )), 
                # Panel 3 - Top Tracks
                tabPanel(title = span(strong("Your Top Tracks"), style = "color: #1DB954"), 
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(h3(strong("What are your favorite tracks?")),
                                          p("Select a time frame upon which to view your top 50 tracks. Again, time frames are ", em("approximate, "), "and your data may not update immediately."),
                                          p('Based on your selected time frame, you can explore the "Tracks for You" tab to look at some tracks that the Spotify API thinks you will also like.'),
                                          hr(), 
                                          selectInput("tracksTime", 
                                                      label = "Select a time frame", 
                                                      choices = c("last 4 weeks" = "short_term",
                                                                  "last 3 months" = "medium_term", 
                                                                  "all time" = "long_term"), 
                                                      selected = "short_term"), 
                                          hr(), 
                                          h4(strong("A look at the most-represented artists")), 
                                          plotOutput("pieTable"), 
                                          hr(), 
                                          h4(strong("When were your favorite tracks released?")), 
                                          plotOutput("trackYears"),
                                          hr(),
                                          h4(strong("How mainstream are your favorite tracks?")),
                                          htmlOutput("trackAnnotate"),
                                          br(),
                                          plotOutput("trackPops")),
                             mainPanel(tabsetPanel(type = "tabs", 
                                                   tabPanel(h2(strong("Your Top Tracks")), 
                                                            br(), 
                                                            withLoader(dataTableOutput("tracksTable"), 
                                                                       type = "html", 
                                                                       loader = "pacman")), 
                                                   tabPanel(h2(strong("Tracks for You")), 
                                                            br(), 
                                                            withLoader(dataTableOutput("trackRecsTable"), 
                                                                       type = "html", 
                                                                       loader = "pacman"))), 
                                       br(), 
                                       hr(), 
                                       h4(strong("A look at the audio features of your top tracks")), 
                                       fluidRow(
                                         column(4,
                                                p(strong("Valence")),
                                                plotOutput("valencePlot")), 
                                         column(4, 
                                                p(strong("Energy")), 
                                                plotOutput("energyPlot")), 
                                         column(4, 
                                                p(strong("Danceability")), 
                                                plotOutput("dancePlot"))
                                       ))
                           )
                         ))
)

server = function(input, output) {
  # Panel 1 - Authentication
  observe({
    if (input$id == "" | input$secret == "" | is.null(input$id) | is.null(input$secret)) {
      shinyjs::disable("authButton")
    }
    else {
      shinyjs::enable("authButton")
    }
  })
  validate = eventReactive(input$authButton, {
    req(input$id)
    req(input$secret)
    auth(input$id, input$secret)
  })
  output$validate_message = renderText({ 
    validate()
  })
  # Panel 2 - Top Artists
  topArtists = reactive({
    # obtaining top artists and placing them into a tibble
    artists = get_my_top_artists_or_tracks(type = "artists", 
                                           limit = 50, 
                                           time_range = input$artistsTime) %>%
      as_tibble() %>%
      select(name, images, genres, popularity) %>%
      rename(Artist = 1, 
             Image = 2, 
             Genres = 3, 
             Popularity = 4) %>%
      # keeping only the images with a size of 160
      unnest(cols = c(Image))
    artists = artists[artists$height == 160,] %>%
      select(-c(height, width))
    artists$Popularity = as.numeric(artists$Popularity)
    artists$Artist = paste0("<strong>", artists$Artist, "</strong>")
    artists$url = paste0('<img src="', artists$url, '"/img height=110 width = 110>')
    artists
  })
  output$artistsTable = renderDataTable({
    topArtists()[1:3] %>% 
      datatable(escape = F)
  })
  cloudTable = reactive({
    genres = topArtists()$Genres %>% flatten() %>% unlist()
    as.data.frame(table(genres)) %>% arrange(desc(Freq))
  })
  output$genresCloud = renderWordcloud2({
    wordcloud2(data = cloudTable(), 
               fontFamily = "Helvetica",
               fontWeight = "bold",
               shape = 'circle', 
               ellipticity = 0.70, 
               color = rep_len(c("#0D340B", "#325630", "#567855", "#7A9A7A", "#9EBC9F"), 
                               nrow(cloudTable())), 
               size = 0.85)
  })
  # Panel 3 - Top Tracks
  topTracks = reactive({
    tracks = get_my_top_artists_or_tracks(type = "tracks", 
                                          limit = 50, 
                                          time_range = input$tracksTime) %>% 
      as_tibble() %>%
      select(name, album.images, artists, album.name, album.release_date, popularity, id) %>%
      unnest(cols = c(album.images))
    tracks = tracks[tracks$height == 64,] %>%
      select(-c(height, width)) 
    tracks$url = paste0('<img src="', tracks$url, '"/img>')
    tracks$name = paste0("<strong>", tracks$name, "</strong>")
    for (i in 1:nrow(tracks)) {
      tracks$artists[i] = tracks$artists[[i]][3][1,]
    }
    tracks$artists = unlist(tracks$artists)
    tracks = tracks %>%
      rename(Track = 1, 
             `Album Art` = 2, 
             Artist = 3, 
             Album = 4, 
             `Album Release Date` = 5, 
             Popularity = 6)
    tracks
  })
  output$tracksTable = renderDataTable({
    topTracks() %>%
      select(-c(id)) %>%
      datatable(escape = F)
  })
  output$pieTable = renderPlot({
    topArts = topTracks() %>% 
      group_by(Artist) %>%
      summarise(Count = n(), 
                Proportion = n()/nrow(topTracks())) %>%
      mutate(Percentage = Proportion * 100) %>%
      arrange(desc(Proportion))
    belowFive = sum(topArts$Percentage[topArts$Percentage < 5])
    pie(x = c(topArts$Percentage[topArts$Percentage >= 5], belowFive), 
        labels = c(topArts$Artist[topArts$Percentage >= 5], "Other"), 
        col = brewer.pal(n = 9, name = "Greens"))
  })
  output$trackYears = renderPlot({
    ggplot(topTracks()) + 
      geom_bar(aes(x = substr(`Album Release Date`, 1, 4)), 
               color = "black", 
               fill = "#567855") + 
      labs(x = "Release Year",
           y = "Number of Tracks") + 
      coord_flip() + 
      theme_classic()
  })
  output$trackAnnotate = renderUI ({
    popMean = mean(topTracks()$Popularity, na.rm = T)
    if (popMean > 50) {
      statement = " they are relatively well-known."
    }
    else {
      statement = " they are relatively obscure."
    }
    HTML(paste0("Your top tracks have an average popularity rating of ", 
                strong(popMean), " - ", statement))
  })
  output$trackPops = renderPlot({
    ggplot(topTracks()) + 
      geom_histogram(aes(x = Popularity), 
                     bins = 20, 
                     color = "black", 
                     fill = "#96C896") + 
      xlim(c(0, 100)) + 
      theme_classic() + 
      labs(x = "Popularity", 
           y = "Count") + 
      theme_classic()
    
  })
  getFeatures = function() {
    get_track_audio_features(ids = topTracks()$id)
  }
  output$valencePlot = renderPlot({
    ggplot(getFeatures()) +
      geom_density(aes(x = valence), 
                   color = "black", 
                   fill = "#0D340B",  
                   size = 1.3) + 
      xlim(c(0, 1)) + 
      labs(x = "Valence", 
           y = "Count") + 
      theme_minimal()
  })
  output$energyPlot = renderPlot({
    ggplot(getFeatures()) +
      geom_density(aes(x = energy), 
                   color = "black", 
                   fill = "#325630",
                   size = 1.3) + 
      xlim(c(0, 1)) + 
      labs(x = "Energy", 
           y = "Count") + 
      theme_minimal()
  })
  output$dancePlot = renderPlot({
    ggplot(getFeatures()) +
      geom_density(aes(x = danceability), 
                   color = "black", 
                   fill = "#96C896", 
                   size = 1.3) + 
      xlim(c(0, 1)) + 
      labs(x = "Danceability", 
           y = "Count") + 
      theme_minimal()
  })
  trackRecs = reactive({
    tRecs = get_recommendations_all(topTracks()$id) %>%
      as_tibble() %>%
      select(name, album.images, artists, album.name, album.release_date, popularity, id) %>%
      unnest(cols = c(album.images))
    tRecs = tRecs[tRecs$height == 64,] %>%
      select(-c(height, width)) 
    tRecs$url = paste0('<img src="', tRecs$url, '"/img>')
    tRecs$name = paste0("<strong>", tRecs$name, "</strong>")
    for (i in 1:nrow(tRecs)) {
      tRecs$artists[i] = tRecs$artists[[i]][3][1,]
    }
    tRecs$artists = unlist(tRecs$artists)
    tRecs = tRecs %>%
      rename(Track = 1, 
             `Album Art` = 2, 
             Artist = 3, 
             Album = 4, 
             `Album Release Date` = 5, 
             Popularity = 6)
    tRecs
  })
  output$trackRecsTable = renderDataTable({
    trackRecs() %>%
      select(-c(id)) %>%
      datatable(escape = F)
  })
  
}

shinyApp(server = server, ui = ui)
