# Shiny App 
# Section 1. First install and activate all your required packages. 

library(shiny)
library(bslib)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

library(visNetwork)

# Section 2. Design the site in the UI section (US = User Interface). This is where we define how everything looks and 
# how people can use the app. 

ui <-fluidPage(
  
  titlePanel("Does proximity play a role in how freshman make their friends?"),
  
  
    
    card(
      card_header("For my project, I am measuring how connected the people who live in my floor section, B2S are. 
Welcome to my Shiny App! Toggle between the different thresholds of my network to see the differences! This compares 
the entire network to just a subset of the nodes with highest degree centrality. To see the degree centrality of all 
the nodes in a clearer way, scroll down to the histogram, which compares each node to each other. To see a couple of 
other measures about this network, click on the different options right below this intro! You can see the eigenvector 
measure, how central a node is in the network based off the degree centrality of the nodes it's connected to. You can 
also see the assortativity of this network by gender! That will tell you whether or not gender impacts the strength of
ties of the B2S residents, if men are more likely to be more connected with other men, and women with other women. 
"),
      
      "To collect my data for my project on measuring how connected my floor is, Battell 2nd floor south (B2S), 
      I conducted a survey that asked the residents who they were, and how often they interacted with each one of
      the other residents. I sent out this survey via email and our floor GroupMe and sent out multiple reminders 
      over the course of a week to get responses. I asked them their names (which then got translated into number 
      ids to ensure anonymity), their gender identities, potential major, STEM/humanities/undecided, and then 
      listed each other resident with the options of having interacted with them 0-1 times, 2-3, 4-6, 7-9, or 10+ times
      in the last week. I then translated those number of interactions to a weight from 1-5, 1 being 0-1 interactions,
      2 being 2-3 interactions and so forth. "),
    card(
      card_header("Choose what measure you want to see!"),
      selectInput("select", 
                  "select an option", 
                  choices = list("Assortativity" = "-0.02631579", 
                                 "Degree Centrality" = "B",
                  "Eigenvector" = "C"),
                  selected =1), 
      textOutput("ourVariable")
      ), 
    
    
    card(card_header("Here's a network!"),
         selectInput("size",
                     "Choose a measure", 
                     choices = list("Normal Degree Centrality" = "degree", 
                                    "Thresholded Degree Centrality" = "threshold"), 
                     selected = 1), 
         plotOutput("example_network"), height = "400px"),
    
    card(
      card_header("Histogram"),
      plotOutput("histogram")
    
    
    ),
)


# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  # CARD 1 
  
  output$ourVariable <- renderText({
    paste("This measure = ", input$select)
  })
  
# let's create a simple example network with 10 nodes and calulate the degree centrality

  # CARD 2 
  
network <- reactive({
  
  b2s_nodes <- read.csv("b2snodes(Sheet1).csv")
  b2s_edges <- read.csv("b2sedges_1(Sheet1).csv")
  
  b2s_edges <- b2s_edges[-c(64, 715), ]
  
  b2s_net <- tbl_graph(nodes = b2s_nodes, 
                       edges= b2s_edges,
                       directed = FALSE) 
  
  b2s_net <- b2s_net |> activate(nodes) |>
    mutate(degree = centrality_degree(weights = weight),
           betweeness_row = centrality_betweenness(normalized = FALSE),
           betweeness_norm = centrality_betweenness(normalized = TRUE),
           closeness = centrality_closeness(),
           threshold = centrality_degree(weight > 3)) #help
  b2s_net
  

  
  b2s_net <- b2s_net |> activate(nodes) |> mutate(degree = centrality_degree(weights = weight), 
                                                  #how connected the connections are to one another (local density)
                                                  eigenvector = centrality_eigen(weights = weight))
  b2s_net

  
  
})
dataframe <- reactive({
  b2s_nodes <- read.csv("b2snodes(Sheet1).csv")
  b2s_edges <- read.csv("b2sedges_1(Sheet1).csv")
  
  b2s_edges <- b2s_edges[-c(64, 715), ]
  
  b2s_net <- tbl_graph(nodes = b2s_nodes, 
                       edges= b2s_edges,
                       directed = FALSE) 
  
  b2s_df <- b2s_net |> activate(nodes) |> as_tibble()
  b2s_net |> activate(edges) |> as_tibble()
  b2s_df
  ggplot(b2s_df, aes(x= reorder(Label, degree), y=degree)) + 
    geom_col(fill = "purple") + 
    labs(x = "ID", y = "Degree", title = "Degree centrality of B2S Residents")
  b2s_net <- tbl_graph(nodes = b2s_nodes, 
                       edges= b2s_edges,
                       directed = FALSE) 
  
  b2s_net <- b2s_net |> activate(nodes) |>
    mutate(degree = centrality_degree(weights = weight),
           betweeness_row = centrality_betweenness(normalized = FALSE),
           betweeness_norm = centrality_betweenness(normalized = TRUE),
           closeness = centrality_closeness(),
           threshold = centrality_degree(weight > 3)) #help
  
  
  b2s_df <- b2s_net |> activate(nodes) |> as_tibble()
  b2s_net
  b2s_df
})


# now let's get it visualized and reactive to our choice from above! 

output$example_network <- renderPlot({
  b2s_net <- network() 
  
  p<- ggraph(b2s_net, layout= "auto")+
    geom_edge_link(color = "darkgrey")+
    geom_node_point(aes(size = .data[[input$size]]), color = "pink")+
    geom_node_text(aes(label = Label), color = "purple")
  
  p
  

})

# CARD 3 
output$histogram <- renderPlot({
  b2s_df <- dataframe()
  ggplot(b2s_df, aes(x= reorder(Label, degree), y=degree)) + 
    geom_col(fill = "lightblue") + 
    labs(x = "ID", y = "Degree", title = "Degree centrality of B2S Residents")
})



}

# Run the application 
shinyApp(ui = ui, server = server)



