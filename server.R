server = function(input, output, session) {
  # Panel 1 - Authentication
  observe({
    if (input$authID == "" | input$secret == "" | is.null(input$authID) | is.null(input$secret)) {
      shinyjs::disable("authButton")
    }
    else {
      shinyjs::enable("authButton")
    }
  })
  validate = eventReactive(input$authButton, {
    req(input$authID)
    req(input$secret)
    auth(input$authID, input$secret)
  })
  loadMessage = eventReactive(input$authButton, {
    "Authenticating...please wait :)"
  })
  output$loadingMessage = renderText({
    loadMessage()
  })
  output$validate_message = renderText({ 
    validate()
  })
  # Panel 2 - Top Artists
  topArtists = eventReactive(input$generateArts, {
    shinyjs::hide("defaultTextArts")
    # obtaining top artists and placing them into a tibble
    artists = get_my_top_artists_or_tracks(type = "artists", 
                                           limit = 50, 
                                           time_range = input$artistsTime) %>%
      as_tibble() %>%
      select(name, images, genres, popularity, id) %>%
      rename(Artist = 1, 
             Image = 2, 
             Genres = 3, 
             Popularity = 4, 
             ID = 5) %>%
      # keeping only the images with a size of 160
      unnest(cols = c(Image))
    artists = artists[artists$height == 160,] %>%
      select(-c(height, width))
    artists$Popularity = as.numeric(artists$Popularity)
    artists$Artist = paste0("<strong>", artists$Artist, "</strong>")
    artists$url = paste0('<img src="', artists$url, '"/img height=110 width = 110>')
    # shinyjs::reset("exploreArt")
    # tops = str_remove_all(topArtists()$Artist, "<.*?>") # removing HTML tags
    # updateSelectInput(session, 
    #                   "exploreArt", 
    #                   choices = tops, # removing HTML tags 
    #                   selected = tops[1])
    artists
  })
  output$defaultTextArts = renderUI({
    HTML(paste0("</br>" ,strong('Click the "Load" button to view your listening data!')))
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
    output$topArtistsUI = renderUI({
      tops = str_remove_all(topArtists()$Artist, "<.*?>") # removing HTML tags
      selectInput("exploreArt",
                  label = "Select your top artist",
                  choices = tops,
                  selected = tops[1])
  })
  recArtists = eventReactive(input$recArtsButton, {
    # req(input$artistsTime)
    index = match(input$exploreArt, str_remove_all(topArtists()$Artist, "<.*?>"))
    selectedID = topArtists()$ID[index]
    anotherDf = get_related_artists(selectedID) %>%
      select(name, images, genres, popularity) %>%
      rename(`Recommended Artist` = 1, 
             Image = 2, 
             Genres = 3, 
             Popularity = 4) %>%
      # keeping only the images with a size of 160
      unnest(cols = c(Image))
    anotherDf = anotherDf[anotherDf$height == 160,] %>%
      select(-c(height, width))
    anotherDf$`Recommended Artist` = paste0("<strong>", anotherDf$`Recommended Artist`, "</strong>")
    anotherDf$url = paste0('<img src="', anotherDf$url, '"/img height=80 width = 80>')
    anotherDf
  })
  printTopArt = eventReactive(input$recArtsButton, {
    HTML(paste0("<h3><strong>Artists similar to ", input$exploreArt, "</strong></h3>"))
  })
  output$selectedTopArt = renderUI({
    printTopArt()
  })
  output$recArtsTable = renderDataTable({
    recArtists() %>%
      datatable(escape = F)
  })
  # Panel 3 - Top Tracks
  topTracks = eventReactive(input$generateTracks, {
    shinyjs::hide("defaultTextTracks")
    tracks = get_my_top_artists_or_tracks(type = "tracks", 
                                          limit = 50, 
                                          time_range = input$tracksTime) %>% 
      as_tibble() %>%
      select(name, album.images, artists, album.name, album.release_date, popularity, id) %>%
      unnest(cols = c(album.images))
    tracks = tracks[tracks$height == 64,] %>%
      select(-c(height, width)) 
    tracks$url = paste0('<img src="', tracks$url, '"/img height=80 width = 80>')
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
  output$defaultTextTracks = renderUI({
    HTML(paste0("</br>" ,strong('Click the "Load" button to view your listening data!')))
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
  output$yearsAnnotate = renderUI({
    yearsData = as.numeric(substr(topTracks()$`Album Release Date`, 1, 4))
    yearsTab = table(yearsData)
    favYear = names(yearsTab)[which.max(yearsTab)]
    if (favYear <= 2010) {
      yearStatement = "your age is showing..."
    }
    else if (favYear <= 2019) {
      yearsStatement = "a transformative year of music."
    }
    else {
      yearsStatement = "your taste is pretty up to date!"
    }
    HTML(paste0("The largest proportion of your favorite tracks are from ", 
                strong(favYear), " - ", yearsStatement))
  })
  output$yearsMinMax = renderUI({
    allDates = as_date(topTracks()$`Album Release Date`)
    minYear = min(allDates, na.rm = T)
    minTrackDf = topTracks() %>% 
      filter(`Album Release Date` == minYear)
    minTrack = minTrackDf$Track[1]
    minPhoto = minTrackDf$`Album Art`[1]
    maxYear = max(allDates, na.rm = T)
    maxTrackDf = topTracks() %>%
      filter(`Album Release Date` == maxYear)
    maxTrack = maxTrackDf$Track[1]
    maxPhoto = maxTrackDf$`Album Art`[1]
    daysDiff = gsub( "d.*$", "", days(maxYear - minYear))
    HTML(paste0("Your oldest favorite track is <strong>", minTrack, "</strong>, released on ", strong(minYear), "</br></br><center>", minPhoto, "</center></br></br>", 
                "Your most recent favorite track is ", maxTrack, ", released on ", strong(maxYear), "</br></br><center>", maxPhoto, "</center></br>Your taste spans ", strong(daysDiff), " days!"))
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
  output$popMinMax = renderUI({
    allPops = as.numeric(topTracks()$Popularity)
    minPop = min(allPops, na.rm = T)
    maxPop = max(allPops, na.rm = T)
    minPopTrack = topTracks()$Track[topTracks()$Popularity == minPop][1]
    minPopPhoto = topTracks()$`Album Art`[topTracks()$Popularity == minPop][1]
    maxPopTrack = topTracks()$Track[topTracks()$Popularity == maxPop][1]
    maxPopPhoto = topTracks()$`Album Art`[topTracks()$Popularity == maxPop][1]
    HTML(paste0("Your most obscure track is ", minPopTrack, ", with a popularity rating of ", strong(minPop), "</br></br><center>", minPopPhoto, "</center></br></br>",
                "Your most mainstream track is ", maxPopTrack, ", with a popularity rating of ", strong(maxPop), "</br></br><center>", maxPopPhoto, "</center>"))
  })
  output$favoriteAlbum = renderUI({
    uniqueCount = length(unique(topTracks()$Album))
    albTab = table(topTracks()$Album)
    favAlbum = names(albTab)[which.max(albTab)]
    favAlbImage = topTracks()$`Album Art`[topTracks()$Album == favAlbum][1]
    filteredDf = topTracks() %>%
      filter(Album != favAlbum)
    anotherTab = table(filteredDf$Album)
    secondFavAlbum = names(anotherTab)[which.max(anotherTab)]
    secFavAlbImage = filteredDf$`Album Art`[filteredDf$Album == secondFavAlbum][1]
    HTML(paste0("A total of ", strong(uniqueCount), " different albums are represented across your top tracks. ", "</br></br>Of these, your favorite album is ", strong(favAlbum), ", which appears ", 
                strong(max(albTab)), " times", "</br></br><center>", favAlbImage, "</center></br></br>Your second favorite album is ", strong(secondFavAlbum), ", which appears ", strong(max(anotherTab)), " times", "</br></br><center>", secFavAlbImage, "</center>"))
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
