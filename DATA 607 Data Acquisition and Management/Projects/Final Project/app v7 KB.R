# TO DO: [Determine whether we can just call the app from the RMD file]

# In the next iteration:
# Prevent duplication between the first, second, and third titles users select
# If possible, add logic to randomize drop down list
# Add a reset button (fairly complex to control state with reactive per multiple commenters)
# Add hyperlinks based on title and artist to search engines


library(shiny)
library(shinythemes)
library(dplyr)
library(RNeo4j)



# Source data for user inputs currently set to s3, requires RMD code to run first
MSD_options <- s3 %>% 
  select(title, release, artist_name)



# Shiny app UI code block
ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Music Recommender"),
  br(),
  strong("Kavya Beheraj & Jeremy O'Brien"),
  br(), 
  sidebarLayout(
    sidebarPanel(
      strong("Please give us your name, select three songs, and rate them on a scale of 1-10"),
      br(), br(), 
                 
      # Get user name
        textInput(inputId = "user_name",
                  label = "Your name"
                  ),
                 
      # Choose first song title
      selectizeInput(inputId = "title1", 
                     label = "First song",
                     choices = c("", as.character(MSD_options$title)),
                     options = list(
                       placeholder = "Start typing here",
                       create = F, 
                       maxOptions = 5,
                       maxItems = 1,
                       multiple = F,
                       closeAfterSelect = T,
                       allowEmptyOption = F
                       )
                     ),

      # Rate first song
      sliderInput(inputId = "rating1", 
                  label = "", 
                  min = 0, 
                  max = 10, 
                  value = 5,
                  ticks = F,
                  pre = NULL
                  ),

      # Choose second title
      selectizeInput(inputId = "title2", 
                     label = "Second song",
                     choices = c("", as.character(MSD_options$title)),
                     options = list(
                       placeholder = "Start typing here",
                       create = F, 
                       maxOptions = 5,
                       maxItems = 1,
                       multiple = F,
                       closeAfterSelect = T,
                       allowEmptyOption = F
                       )
                     ),

      # Rate second song
      sliderInput(inputId = "rating2", 
                  label = "", 
                  min = 0, 
                  max = 10, 
                  value = 5,
                  ticks = F,
                  pre = NULL
                  ),
                 
      # Choose third title
      selectizeInput(inputId = "title3", 
                     label = "Third song",
                     choices = c("", as.character(MSD_options$title)),
                     options = list(
                       placeholder = "Start typing here",
                       create = F, 
                       maxOptions = 5,
                       maxItems = 1,
                       multiple = F,
                       closeAfterSelect = T,
                       allowEmptyOption = F
                       )
                     ),
                 
      # Rate third song
      sliderInput(inputId = "rating3",
                  label = "", 
                  min = 0, 
                  max = 10, 
                  value = 5, 
                  ticks = F,
                  pre = NULL
                  ),
      
          # Submit choices
      actionButton(inputId = "go", label = "Here we go!")
    ),
    
    mainPanel(
      #strong("Here are the songs you rated:"),
      #br(), br(),
      tableOutput("choices"),
      #br(), br(),
      #strong("Based on your ratings, here are some songs you might also like:"),
      #br(), br(),
      tableOutput("recommendations")
    )
  )
)



# Shiny server code block
server <- function(input, output, session) {
  
  # On button push, collect user inputs, including user name, three song titles, and ratings
  user_choices <- eventReactive(input$go, {
    choices <- data.frame(
               User = as.character(input$user_name),
               Title = c(input$title1, input$title2, input$title3),
               Rating = as.integer(c(input$rating1, input$rating2, input$rating3)),
               stringsAsFactors = F)
  })
  
  
  query <- {Title}
  
  cypher(graph, query)
  
# Query to create new nodes and relationships based on user choices (with placeholders):
#q1 <- "#CREATE p = (User { id : %s })-[:RATED {
#    Rating : [ {
#      rating : %d
#    }, {
#      rating : %d
#    }, {
#      rating : %d
#    }]
#  }]-> (s:Song {
#    Song : [ {
#      title : %s,
#      song_id : %s
#    }, {
#      title : %s,
#      song_id : %s
#    }, {
#      title : %s,
#      song_id : %s
#    }]
#  }) "
#
# sprintf fills in placeholders starting with a "%" in order
#
#  p <- cypher(graph, sprintf(q1, 
#                             output$User,
#                             output$Rating[1],
#                             output$Rating[2],
#                             output$Rating[3],
#                             output$Title[1]))
    
  # TO DO: [Integrate the neo4j code and return the recommended data frame]     
  user_reco <- eventReactive(input$go, {
  
    # Cypher code to build nodes based on user inputs (Nicole calls wrapped in brackets)
    # {title1} {rating1}
    # {title2} {rating2}
    # {title3} {rating3}
    # {user_name} is also available if we'd like to designate the node with it plus digits (or something)

    # Nicole's query code below, for reference: https://nicolewhite.github.io/2014/06/30/create-shiny-app-neo4j-graphene.html
    # query = "
    # MATCH (p:Place)-[:IN_CATEGORY]->(c:Category),
    # (p)-[:AT_GATE]->(g:Gate),
    # (g)-[:IN_TERMINAL]->(t:Terminal)
    # WHERE c.name IN {categories} AND t.name = {terminal}
    # WITH c, p, g, t, ABS(g.gate - {gate}) AS dist
    # ORDER BY dist
    # RETURN p.name AS Name, c.name AS Category, g.gate AS Gate, t.name AS Terminal
    # "
    
    # Cypher code to find similarity based on user inputs 
    # q2 <- 
    #  "MATCH (p1:User)-[x:RATED]->(s:Song)<-[y:RATED]-(p2:User)
    # WITH SUM(x.rating * y.rating) AS xyDotProduct,
    # SQRT(REDUCE(xDot = 0.0, a IN COLLECT(x.rating) | xDot + a^2)) AS xLength,
    # SQRT(REDUCE(yDot = 0.0, b IN COLLECT(y.rating) | yDot + b^2)) AS yLength,
    # p1, p2
    # MERGE (p1)-[s:SIMILARITY]-(p2)
    # SET s.similarity = xyDotProduct / (xLength * yLength)"
    # cypher(graph, q2)
    
    # Cypher code to produce top-N recommendations based on similarity
    # q5 <- "
    # MATCH (b:User)-[r:RATED]->(m:Song), (b)-[s:SIMILARITY]-(a:User {id:'be0a4b64e9689c46e94b5a9a9c7910ee61aeb16f'})
    # WHERE NOT((a)-[:RATED]->(m))
    # WITH m, s.similarity AS similarity, r.rating AS rating
    # ORDER BY m.title, similarity DESC
    # WITH m.song_id AS song_id, COLLECT(rating)[0..3] AS ratings
    # WITH song_id, REDUCE(s = 0, i IN ratings | s + i)*1.0 / LENGTH(ratings) AS reco
    # ORDER BY reco DESC
    # RETURN song_id AS song_id, reco AS recommendation
    # "
    # f <- cypher(graph, q5)
    # g <- inner_join(grouped_song, f, by="song_id") %>%
    #  arrange(desc(recommendation)) %>% 
    #  distinct()
    
    # Filler call - replace var as needed
    g %>% 
      arrange(desc(recommendation)) %>% 
      head(n = 5) %>% 
      select(Title = title, Artist = artist_name)
  })

  # Prepare table output based on user inputs to render in main panel
  output$choices <- renderTable({
    user_choices()
    }, caption = "Here are the songs you rated:",
    caption.placement = getOption("xtable.caption.placement", "top")
    )

  # Prepare table output based on recommender outputs to render in main panel
  output$recommendations <- renderTable({
    user_reco()
    }, caption = "Based on your ratings, here are some songs you might also like:",
    caption.placement = getOption("xtable.caption.placement", "top")
    )
}



# Reference links:

# http://shiny.rstudio.com/gallery/
# https://shiny.rstudio.com/reference/shiny/1.0.2/textInput.html
# https://shiny.rstudio.com/reference/shiny/1.0.1/selectInput.html
# https://shiny.rstudio.com/articles/selectize.html
# https://github.com/selectize/selectize.js/blob/master/docs/usage.md
# https://shiny.rstudio.com/reference/shiny/1.0.5/renderTable.html
# http://shiny.rstudio.com/reference/shiny/1.0.2/observeEvent.html

# https://rdrr.io/cran/RNeo4j/man/cypher.html
# https://stackoverflow.com/questions/31123283/pass-shiny-ui-text-input-into-rmongodb-query
# https://stackoverflow.com/questions/41504111/modularizing-shiny-r-app-code?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# https://nicolewhite.github.io/2014/06/30/create-shiny-app-neo4j-graphene.html
# https://nicolewhite.github.io/2014/06/30/create-shiny-app-neo4j-graphene.html



# The following code must be last line in the file

shinyApp(ui = ui, server = server)