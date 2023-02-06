# spotifySlug

#### Meet Todd...this is Todd, the logo I created for Spotify Slug :)

<img src="https://user-images.githubusercontent.com/109494334/216837146-5f8c224d-9bfc-4586-9560-bb1f1b744841.png" width="150" height="150">

***

### How to run Spotify Slug

You can use Spotify Slug by running the below code snippet in R Studio, or via [this](https://jasminex21.shinyapps.io/spotifySlug/) link.
```
library(shiny)
runGitHub("spotifySlug", "jasminex21")
```

### Inspiration
I, like many fellow Spotify users, really enjoy looking at my own listening habits and statistics - I find it interesting to see how often I change my tastes. However, I got tired of the same old sites that I used to do this task, so I decided to harness the Spotify API and make my own web application...welcome to Spotify Slug, created by Spotify user bananaslug-10 with lots of love and hard work.

### What it does
Spotify Slug is a dynamic web application that aims to curate an interactive and visual experience for users who hope to gain insight into their listening habits. Users can view their top artists and top tracks over three different time frames, and explore the audio features, popularity, and release years of their favorite tracks, and a distribution of the genres represented in their favorite artists. Additionally, a tab is provided to recommend users to new tracks based on their most-played tracks.

### How I built it
Spotify Slug was built entirely using R and R Shiny, as well as several other packages that facilitate graphing and enhance visuals (ggplot2, wordcloud2, tidyverse, DT). The R package spotifyr was used to access the Spotify API.

![image](https://user-images.githubusercontent.com/109494334/216837036-67ca9f76-deb1-4c07-a7b0-81e59c52ba6b.png)
![image](https://user-images.githubusercontent.com/109494334/216837051-b049953d-9a22-4434-a030-3d58b22c570b.png)
![image](https://user-images.githubusercontent.com/109494334/216837058-890136e5-12dd-4715-b483-3e826a40d1b5.png)
![image](https://user-images.githubusercontent.com/109494334/216837063-14a844b5-47c0-44a0-99b2-0412bea21d82.png)
