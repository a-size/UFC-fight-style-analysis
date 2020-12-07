library(shiny)
library(tidyverse)
library(rvest)
library(rstanarm)
library(ggridges)
library(gtsummary)
library(broom.mixed)
library(gt)


master <- read_csv("data/ufc-master.csv") %>%
  select(1:4, 10:13) %>%
  mutate(differenceOdds = abs(R_odds - B_odds)) 

poundforpound <- read_csv("data/poundforpound_foundation.csv") 

win_loss <- read_csv("data/foundation.csv") %>%
  mutate(num_foundation = Wrestling + BJJ + Boxing + 
           Kickboxing + Muay_Thai + Judo +
           Taekwondo + Karate + Capoeira + Kajukenbo +
           Hapkido + Sanda + ARB) %>%
  mutate(win_loss = Wins/(Wins + Losses))

foundations <- read_csv("data/foundation.csv")
foundations_left_fighter <- foundations_right_fighter <- foundations 
colnames(foundations_left_fighter)[2:20] <- paste0(colnames(foundations_left_fighter)[2:20], "_left_fighter")



fight_data <- sherdog %>%
  left_join(foundations_left_fighter, by = c("f1name" = "Name_left_fighter"))
fight_data[complete.cases(fight_data[ , 21:39]),]
fight_data[rowSums(is.na(fight_data[ , 21:39])) == 0, ]


model_1 <-  stan_glm(data = win_loss, 
                     win_loss ~ Wrestling + BJJ + 
                       Boxing + Kickboxing + 
                       Muay_Thai + Judo,
                     seed = 9,
                     refresh = 0)

print(model_1, digits = 5)

model_2 <-  stan_glm(data = win_loss, 
                     Ranking ~ Wrestling + BJJ + 
                       Boxing + Kickboxing + 
                       Muay_Thai + Judo,
                     seed = 9,
                     refresh = 0)

print(model_2, digits = 5) 

ui <- navbarPage(
    "Analysis of the UFC",
    tabPanel("Interactive Element & Rankings",
             fluidPage(
                 titlePanel("Talent Distribution in each weight class"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "weight_class",
                             "Choose Weight Class",
                              choices = unique(master$weight_class)
                         )),
                     mainPanel(plotOutput("ufcOddsPlot"), 
                               plotOutput("poundforpoundPlot"), 
                               plotOutput("strawweightPlot"), 
                               plotOutput("flyweightPlot"),
                               plotOutput("bantamweightPlot"), 
                               plotOutput("featherweightPlot"), 
                               plotOutput("lightweightPlot"), 
                               plotOutput("welterweightPlot"), 
                               plotOutput("middleweightPlot"),
                               plotOutput("lightHeavyweightPlot"), 
                               plotOutput("heavyweightPlot")))
             )),
    tabPanel("Win-Loss percentages for major styles",
             fluidPage(
               titlePanel("Different Fighting Styles"),

                 mainPanel( 
                           plotOutput("boxingPlot"), 
                           plotOutput("bjjPlot"), 
                           plotOutput("mtPlot"), 
                           plotOutput("wrestlingPlot"), 
                           plotOutput("stylePlot")))),
    tabPanel("Models: Which has the biggest impact on winning?",
             fluidPage(
               titlePanel("Foundational Styles"),
               
                 mainPanel(
                   h3(textOutput("text")),
                   plotOutput("Model1"), 
                   tableOutput("table1"),

                   
                   
                   
                   
                   plotOutput("Model2"), 
                   tableOutput("table2")))),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I am a casual fan of MMA and follow the UFC. I am also 
             interested in combat sports more generally, and I train in 
             Brazilian Jiu-Jitsu. I thought it 
             would be interesting to explore the records of fighters' with 
             different foundational styles, to answer the question - is there
             really a style that can beat out all the others?"),
             h3("About Me"),
             p("My name is Amaya Sizer and I study Social Studies. 
             You can reach me at asizer@college.harvard.edu.
               You can access my GitHub Respository here: 
               https://github.com/a-size/UFC-fight-style-analysis"))) 

server <- function(input, output) {
 
  output$ufcOddsPlot <- renderPlot({
    
    output$ufcOddsPlot <- renderPlot({
      
      master %>%
        filter(weight_class == input$weight_class) %>%
        ggplot(mapping = aes(x = differenceOdds)) + 
        geom_density()+ 
        labs(title = "Absolute value of difference in Odds by Weight 
           Division & Gender", 
             x = "Abs Value of Diff in Odds",
             y = "Prob") +  
        theme_bw()
      
    }) 
    
    
  })
  
  output$poundforpoundPlot <- renderPlot({
    
    poundforpound %>%
      arrange(desc(Ranking)) %>%
      mutate(Name = factor(Name, levels = Name)) %>%
      ggplot(aes(x = X, y = Y, color = Division)) + 
      geom_segment(inherit.aes = FALSE, 
                   aes(x = 0, xend = Ranking, 
                       y = Name, yend = Name)) + 
      geom_point(aes(x = Ranking, y = Name)) +
      scale_x_continuous(breaks = seq(0, 15, 1)) +
      theme_bw() + 
      labs(title = "Pound for Pound Rankings", 
           x = "Ranking", 
           y = "Fighter") 
    
    
  })

output$strawweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Strawweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(-Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Strawweight Rankings", 
         subtitle = "Upper weight limit of 115 lb",
         x = "Ranking", 
         y = "Fighter") 
  
  
  
})
  
output$flyweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Flyweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(-Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Flyweight Rankings", 
         subtitle = "Upper weight limit of 125 lb",
         x = "Ranking", 
         y = "Fighter") 
  
  
})
  
output$bantamweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Bantamweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(-Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Bantamweight Rankings", 
         subtitle = "Upper weight limit of 135 lb",
         x = "Ranking", 
         y = "Fighter") 
  
  
  
})

output$featherweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Featherweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(-Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Featherweight Rankings", 
         subtitle = "Upper weight limit of 145 lb",
         x = "Ranking", 
         y = "Fighter") 
  
  
})


output$lightweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Lightweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(-Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Lightweight Rankings", 
         subtitle = "Upper weight limit of 155 lb",
         x = "Ranking", 
         y = "Fighter") 
  
  
})

output$welterweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Welterweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(-Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Welterweight Rankings", 
         subtitle = "Upper weight limit of 170 lb",
         x = "Ranking", 
         y = "Fighter") 
  
  
  
})


output$middleweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Middleweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(-Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Middleweight Rankings", 
         subtitle = "Upper weight limit of 185 lb",
         x = "Ranking", 
         y = "Fighter") 
  
  
  
})


output$lightHeavyweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Light Heavyweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(-Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Light Heavyweight Rankings", 
         subtitle = "Upper weight limit of 205 lb",
         x = "Ranking", 
         y = "Fighter") 
  
})

output$heavyweightPlot <- renderPlot({
  
  foundations %>%
    filter(Division == "Heavyweight") %>%
    arrange(desc(Ranking)) %>%
    mutate(Name = factor(Name, levels = Name)) %>%
    ggplot(aes(x = X, 
               y = reorder(Ranking), 
               color = Gender)) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = Ranking, 
                     y = Name, yend = Name)) + 
    geom_point(aes(x = Ranking, y = Name)) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    theme_bw() + 
    labs(title = "Heavyweight Rankings", 
         subtitle = "Upper weight limit of 265 lb",
         x = "Ranking", 
         y = "Fighter") 
  
  
  
})

  output$stylePlot  <- renderPlot({
    
    
    win_loss %>%
      ggplot() + 
      geom_col(fill = "#E08B00", aes(x = num_foundation, 
                                     y = win_loss)) + 
      theme_bw() + 
      labs(title = "Does having more foundational styles give
       more of an advantage?",
           subtitle = "Fighters with two foundational styles
       tend to have highest win/ loss percentages", 
           x = "Number of Foundational Styles", 
           y = "Win/ Loss Percentage")
    
    
  })
  
  
  output$Model1 <- renderPlot({
    
    model_1 %>%
      as_tibble() %>%
      select(-c(`(Intercept)`, sigma)) %>%
      pivot_longer(cols = everything(), 
                   values_to = "Coefficients", 
                   names_to = "Variable") %>%
      ggplot(aes(x = Coefficients, y = Variable, fill = stat(x))) +
      geom_density_ridges_gradient(alpha = 0.5, scale = 3, 
                                   rel_min_height = 0.01, 
                                   gradient_lwd = 1.) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
      scale_fill_viridis_c(name = "Win-loss pct", 
                           option = "C") +
      labs(
        title = 'Difference in Win-loss percentage',
        subtitle = 'Wrestling makes a significant 
    difference in win-loss percentage') +
      theme_ridges(font_size = 13, grid = TRUE) + 
      theme(axis.title.y = element_blank())
    
  })
  
  output$wrestlingPlot <- renderPlot({
    fight_data %>%
      group_by(f1name) %>%
      filter(Wrestling_left_fighter == 1) %>%
      mutate(pct = Wins_left_fighter/
               (Wins_left_fighter + Losses_left_fighter)) %>%
      summarize(pct = mean(pct), 
                Ranking = mean(Ranking)
                ,.groups = "drop") %>%
      arrange(desc(pct)) %>%
      ggplot(aes(x = X, 
                 y = reorder(pct))) + 
      geom_segment(inherit.aes = FALSE, 
                   aes(x = 0, xend = pct, 
                       y = f1name, yend = f1name)) + 
      geom_point(aes(x = pct, y = f1name)) +
      theme_bw() + 
      labs(title = "Win-Loss Percentage for Wrestlers", 
           x = "Percentage", 
           y = "Fighter") + 
      xlim(0.7, 1)
    
    
  })
    
  
output$boxingPlot <- renderPlot({

  fight_data %>%
    group_by(f1name) %>%
    filter(Boxing_left_fighter == 1) %>%
    mutate(pct = Wins_left_fighter/
             (Wins_left_fighter + Losses_left_fighter)) %>%
    summarize(pct = mean(pct), 
              Ranking = mean(Ranking)
              ,.groups = "drop") %>%
    arrange(desc(pct)) %>%
    ggplot(aes(x = X, 
               y = reorder(pct))) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = pct, 
                     y = f1name, yend = f1name)) + 
    geom_point(aes(x = pct, y = f1name)) +
    theme_bw() + 
    labs(title = "Win-Loss Percentage for Boxers", 
         x = "Percentage", 
         y = "Fighter") + 
    xlim(0.55, 0.95)
  
  
})
  
output$bjjPlot <- renderPlot({
  
  fight_data %>%
    group_by(f1name) %>%
    filter(BJJ_left_fighter == 1) %>%
    mutate(pct = Wins_left_fighter/
             (Wins_left_fighter + Losses_left_fighter)) %>%
    summarize(pct = mean(pct), 
              Ranking = mean(Ranking)
              ,.groups = "drop") %>%
    arrange(desc(pct)) %>%
    ggplot(aes(x = X, 
               y = reorder(pct))) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = pct, 
                     y = f1name, yend = f1name)) + 
    geom_point(aes(x = pct, y = f1name)) +
    theme_bw() + 
    labs(title = "Win-Loss Percentage for BJJ", 
         x = "Percentage", 
         y = "Fighter") + 
    xlim(0.6, 1)
  
  
})

output$mtPlot <- renderPlot({
  
  fight_data %>%
    group_by(f1name) %>%
    filter(Muay_Thai_left_fighter == 1) %>%
    mutate(pct = Wins_left_fighter/
             (Wins_left_fighter + Losses_left_fighter)) %>%
    summarize(pct = mean(pct), 
              Ranking = mean(Ranking)
              ,.groups = "drop") %>%
    arrange(desc(pct)) %>%
    ggplot(aes(x = X, 
               y = reorder(pct))) + 
    geom_segment(inherit.aes = FALSE, 
                 aes(x = 0, xend = pct, 
                     y = f1name, yend = f1name)) + 
    geom_point(aes(x = pct, y = f1name)) +
    theme_bw() + 
    labs(title = "Win-Loss Percentage for Muay Thai", 
         x = "Percentage", 
         y = "Fighter") + 
    xlim(0.65, 0.9) 
  
  
})

  output$Model2 <- renderPlot({
    
    model_2 %>%
      as_tibble() %>%
      select(-c(`(Intercept)`, sigma)) %>%
      pivot_longer(cols = everything(), 
                   values_to = "Coefficients", 
                   names_to = "Variable") %>%
      ggplot(aes(x = Coefficients, y = Variable, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3,
                                   rel_min_height = 0.01, 
                                   gradient_lwd = 2) +
      
      
      scale_fill_viridis_c(name = "Ranking", 
                           option = "D") +
      labs(
        title = 'Distribution of the difference in
    ranking based for each style',
        subtitle = 'Here, a lower mean is desirable. 
        The lower the mean, the higher the rank.'
      ) +
      theme_ridges(font_size = 13, grid = TRUE) + 
      theme(axis.title.y = element_blank())
    
    
  })
  
  
output$table1 <- renderTable({
  
  tbl_regression(model_1, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Win-Loss", 
               subtitle = "The Effect of Style on Win-Loss pct") %>%
    tab_source_note(md("Sources: Wikipedia, ESPN"))
  
  
  
  
})  
  
  
output$table2 <- renderTable({

  tbl_regression(model_2, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Ranking", 
               subtitle = "The Effect of Style on Ranking") %>%
    tab_source_note(md("Sources: Wikipedia, ESPN"))

})

}

shinyApp(ui = ui, server = server)


