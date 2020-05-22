# Must call r script app.R for deployment.
 rm(list = ls())

# setup libraries.
#install.packages("DT")
library(shiny)
library(magrittr)
library(regplot)
library(shinythemes)
library(plotly)
library(reshape2)
library(rms)
library(car)
library(covid19us)
library(stringr)
library(rio)

two_dp <- function(x){
  formatC(x, digits = 2, format = "f")
}

one_dp <- function(x){
  formatC(x, digits = 1, format = "f")
}

zero_dp <- function(x){
  formatC(x, digits = 0, format = "f")
}



load('data/for_shiny.rdata')

## Update cumulative deaths from webpage

Death_by_state <- rio::import("https://covidtracking.com/api/v1/states/current.csv")

Death_by_state <- Death_by_state[, c("death", "positive", "state")] 

CD <- merge(CD, Death_by_state, by = "state", all.x = TRUE)

CD$Deaths_per_million <-  CD$death/CD$Population

CD$Cases_per_million <-  CD$positive/CD$Population

#CD$ICU_cases_per_million <-  CD$in_icu_cumulative/CD$Population

#CD$Hospitalized_cases_per_million <- CD$hospitalized_cumulative/CD$Population


##----  plot regression surface.----
three_d_plot_with_regression_surface <- function(df = CD, reg_model = "linear",
                                                 covars = c("Lockdown", "weighted_density"), extra_info = "geographic_area",
                                                 outcome = "Deaths_per_million", adjusted_or_crude = "crude",
                                                 dimension = c(10, 10), opacity_surface = 0.75, get_model = FALSE,
                                                 NY = "exclude NY", Miss = "exclude Mississipi"){
  if (NY == "exclude NY"){
    df <- df[df$StateName != "New York",]
  }
  
  if (Miss == "exclude Mississipi"){
    df <- df[df$StateName != "Miss.",]
  }
  
  
  
  
  run_model <- function(df1 = df, reg_model1 = reg_model, covars1 = covars, outcome1 = outcome, adjust = adjusted_or_crude,
                        NY1 = NY){ 
  
  if (reg_model1 == "linear outcome" & adjust == "crude (Lockdown \u2192 deaths or cases per million)"){
    
    model3 <- lm(as.formula( paste0(outcome1, "~", covars1[2])), data = df1, 
                 na.action = na.exclude)
  }
    if (reg_model1 == "linear outcome" & adjust == "crude (Density \u2192 deaths or cases per million)"){
      
      model3 <- lm(as.formula( paste0(outcome1, "~", covars1[1])), data = df1, 
                   na.action = na.exclude)
    }
    
     if (reg_model1 == "linear outcome" & adjust =="adjusted"){
    #browser()
    model3 <- lm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2])), data = df1, 
                 na.action = na.exclude)
     }
    
    if (reg_model1 == "linear outcome with squared terms" & adjust == "crude (Lockdown \u2192 deaths or cases per million)"){
      
      model3 <- lm(as.formula( paste0(outcome1, "~", covars1[2])), data = df1, 
                   na.action = na.exclude)
    }
    if (reg_model1 == "linear outcome with squared terms" & adjust == "crude (Density \u2192 deaths or cases per million)"){
      
      model3 <- lm(as.formula( paste0(outcome1, "~ I(", covars1[1], "^2)")), data = df1, 
                   na.action = na.exclude)
    }
    
    if (reg_model1 == "linear outcome with squared terms" & adjust =="adjusted"){
      #browser()
      model3 <- lm(as.formula(paste0(outcome1, "~ I(", covars1[1],"^2) + ", covars1[2])), data = df1, 
                   na.action = na.exclude)
    }
    
    if (reg_model1 == "log-linear outcome" & adjust =="adjusted"){
      #browser()
      model3 <- lm(as.formula(paste0("log(",outcome1, ") ~ ", covars1[1], " + ", covars1[2])), data = df1, 
                   na.action = na.exclude)
    }
    
    if (reg_model1 == "log-linear outcome" & adjust =="crude (Lockdown \u2192 deaths or cases per million)"){
      #browser()
      model3 <- lm(as.formula(paste0("log(",outcome1, ") ~ ", covars1[2])), data = df1, 
                   na.action = na.exclude)
    }
    
    if (reg_model1 == "log-linear outcome" & adjust =="crude (Density \u2192 deaths or cases per million)"){
      #browser()
      model3 <- lm(as.formula(paste0("log(",outcome1, ") ~ ", covars1[1])), data = df1, 
                   na.action = na.exclude)
    }
    
    if (reg_model1 == "log-linear outcome with quadratic terms" & adjust == "crude (Lockdown \u2192 deaths or cases per million)"){
      
      model3 <- lm(as.formula( paste0("log(", outcome1, ") ~", covars1[2])), data = df1, 
                   na.action = na.exclude)
    }
    if (reg_model1 == "log-linear outcome with quadratic terms" & adjust == "crude (Density \u2192 deaths or cases per million)"){
      
      model3 <- lm(as.formula( paste0("log(", outcome1, ") ~ I(", covars1[1], "^2) + ", covars1[1])), data = df1, 
                   na.action = na.exclude)
    }
    
    if (reg_model1 == "log-linear outcome with quadratic terms" & adjust =="adjusted"){
      #browser()
      model3 <- lm(as.formula(paste0("log(", outcome1, ") ~ I(", covars1[1],"^2) + ", covars1[1], " + ", covars1[2])), data = df1, 
                   na.action = na.exclude)
    }
    
    
    
    
    
    if (reg_model1 == "linear outcome with quadratic terms" & adjust == "crude (Lockdown \u2192 deaths or cases per million)")
    {
      model3 <- lm(as.formula(paste0(outcome1, "~", covars1[2],' +', 
                                      covars1[2])), 
                   data = df1, na.action = na.exclude)
    }
    if (reg_model1 == "linear outcome with quadratic terms" & adjust == "crude (Density \u2192 deaths or cases per million)"){
      model3 <- lm(as.formula(paste0(outcome1, "~", covars1[1],' +', 
                                     'I(', covars1[1], '^ 2)')), 
                   data = df1, na.action = na.exclude)
    }  
    
  if (reg_model1 == "linear outcome with quadratic terms" & adjust == "adjusted"){
    model3 <- lm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2],' +', 
                'I(', covars1[1], '^ 2)', '+',  covars1[2])), 
                data = df1, na.action = na.exclude)
  }
    
    if (reg_model1 == "Poisson with linear terms" & adjust == "adjusted"){
    model3 <- glm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2])), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
    
    if (reg_model1 == "Poisson with linear terms" & adjust == "crude (Lockdown \u2192 deaths or cases per million)"){
    model3 <- glm(as.formula(paste0(outcome1, "~",  covars1[2])), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
    
    if (reg_model1 == "Poisson with linear terms" & adjust == "crude (Density \u2192 deaths or cases per million)"){
    model3 <- glm(as.formula(paste0(outcome1, "~",  covars1[1])), 
                family = "poisson",
                data = df1, na.action = na.exclude)
  }
    
    
    if (reg_model1 == "Poisson with quadratic terms" & adjust == "crude (Density \u2192 deaths or cases per million)"){
    model3 <- glm(as.formula(paste0(outcome1, "~", covars1[1],' +', 
                                     'I(', covars1[1], '^ 2)')), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
     if (reg_model1 == "Poisson with quadratic terms" & adjust == "adjusted"){
    model3 <- glm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2],' +', 
                'I(', covars1[1], '^ 2)', '+',  covars1[2])), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
    
    if (reg_model1 == "Poisson with quadratic terms" & adjust == "crude (Lockdown \u2192 Deaths per million)"){
    model3 <- glm(as.formula(paste0(outcome1, "~", covars1[2],' +', 
                                     'I(', covars1[2], '^ 2)')), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
    
 
    
    
    return(model3)
  }
 # browser()
  
  model <- run_model()
  
  if(get_model == TRUE){
    return(model)
  }
  
  
  # model %>% print
  stopifnot(df %>%  is.data.frame)
  ## Calculate various variables required for plotting.
  
  df$predicted_outcome <- predict(model, newdata = df) %>% as.numeric
  
  #df %>% str %>% print
  df <- na.exclude(df)
  
  df$predicted_minus_observed_outcome <- df$predicted_outcome - df[, outcome] 
  
  df$predicted_minus_observed_outcome %>% str
  
  df$opacity <- (df[, outcome] - df$predicted_outcome )^2/ max((df[, outcome] - df$predicted_outcome )^2 %>% abs)
  
  df$predicted_outcome %>% zero_dp
  
  df %<>% data.frame
  
  outcome_title <- str_replace_all(outcome, "_", " ")
  #print("df is now:")
  #str(df) %>% print
  
  n_points <- 25
  graph_reso_first_covar <- (max(df[, covars[1], drop = FALSE]) - min(df[, covars[1], drop = FALSE]))/n_points
  #print("graph_reso_mileage is:")
  #print(graph_reso_mileage)
  
  graph_reso_second_covar <- (max(df[, covars[2], drop = FALSE]) - min(df[, covars[2], drop = FALSE]))/n_points
  #print("graph_reso_year is:")
  #print(graph_reso_year)
  #Setup Axis
  axis_x <- seq(min(df[, covars[1], drop = FALSE]), max(df[, covars[1], drop = FALSE]), 
                by = graph_reso_first_covar)
  axis_y <- seq(min(df[, covars[2], drop = FALSE]), max(df[, covars[2], drop = FALSE]), 
                by = graph_reso_second_covar)
  model_surface <- expand.grid(x = axis_x, y = axis_y, KEEP.OUT.ATTRS = F)
  names(model_surface) <- covars
  
  if(reg_model == "linear outcome" | reg_model == "linear outcome with quadratic terms" | 
     reg_model == "linear outcome with squared terms" ){
  model_surface$outcome <- predict.lm(model, newdata = model_surface)
  } 
  
  if(reg_model == "log-linear outcome"|reg_model ==  "log-linear outcome with quadratic terms"){
    model_surface$outcome <- predict.lm(model, newdata = model_surface)
  } 
  
  
  if (reg_model == "Poisson with linear terms" | reg_model == "Poisson with quadratic terms"){
    model_surface$outcome <- predict.glm(model, newdata = model_surface, type = "response")
  }
  
  
  
  #print("model surface outcome is:")
  #print(str(model_surface$outcome))
  
  model_surface <- acast(model_surface, as.formula(paste0(covars[2], "~", covars[1])),
                         value.var = "outcome") #y ~ x
  
  
  if(reg_model == "log-linear outcome" | reg_model == "log-linear outcome with quadratic terms"){
    model_surface <- exp(model_surface)
    surf <- cbind(axis_x, axis_y, model_surface) %>% data.frame
  } else {
    surf <- cbind(axis_x, axis_y, model_surface) %>% data.frame
  }
  
 
  
  # Create lists for axis properties
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "black")
  
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "#ff9999")
  
  xaxis <- list(
    titlefont = f1,
    tickfont = f2,
    showgrid = TRUE,
    title = covars[1]
                )
  
  yaxis <- list(
    titlefont = f1,
    tickfont = f2,
    showgrid = TRUE,
    title =  covars[2]
                )
  
  
  zaxis <- list(
    titlefont = f1,
    tickfont = f2,
    showgrid = TRUE,
    title = outcome
                )
  
  
  scene = list(
    xaxis = xaxis,
    yaxis = yaxis,
    zaxis = zaxis
  )
  
 # names(df) <- 
  
  #- Plot 
  plot_3d <- plot_ly( data = df, 
                     x = ~df[, covars[1]], 
                     y = ~df[, covars[2]],
                     z = ~df[, outcome],
                     type = "scatter3d",
                     name = "Lockdown",
                     marker = list(size = 5, 
                                   color = df$opacity, 
                                   line = list(
                                     color = 'black',
                                     width = 1)),
                     color = 'rgba(255, 182, 193, .9)',
                     width = (0.6*as.numeric(dimension[1])), 
                     height = 0.7*as.numeric(dimension[2]),
                     hoverinfo = 'text',
                     hoverlabel = list(bgcolor = "black", font = list(color = "white")),
                     text = ~paste('Density: ', df[, covars[1]] %>% zero_dp, 
                                   '<br>Lockdown: ', df[, covars[2]] , 
                                   '<br>', paste0("Covid-19 ", outcome_title, ":"), df[, outcome] %>% zero_dp,
                                   '<br>State: ', df[, extra_info] )) %>% 
  layout(title = "", scene = list(
    xaxis = list(title = "Density (people per square mile)"),
    yaxis = list(title = "State under lockdown"),
    zaxis = list(title = paste0("Covid-19 ", outcome_title)))) %>% suppressWarnings
  
  plot_3d <- add_trace(p = plot_3d,
                      data = surf,
                      z = ~model_surface,
                      x = ~axis_x,
                      y = ~axis_y,
                      opacity = opacity_surface,
                      type = "surface",
                      name = "regression plane",
                      showlegend = F,
                      hoverinfo = 'all',
                      text = ~paste('Density:', df[, covars[1]] %>% two_dp, 
                                    '<br>Lockdown:', df[, covars[2]], 
                                    '<br>', paste0("Covid-19 ", outcome_title, ": "), df[, outcome]
                                    )
                      ) %>% suppressWarnings
  plot_3d
  }





#---- Define UI for app  ----
ui <- fluidPage(theme = shinytheme("cyborg"), ## provides theme for shiny app...
                # this provides dimensions for plotly output.
                
      #--- This styles validation error messages... --- 
      
                tags$head(
                  tags$style(HTML("
                                  .shiny-output-error-validation {
                                  color: green;
                                  }
                                  "))),
                
      tags$head(
        tags$style(HTML("
                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                        
                        h1 {
                        font-family: 'Lobster', cursive;
                        font-weight: 500;
                        line-height: 1.1;
                        color: #33ccff;
                        }
                        
                        "))),
      
      tags$head(
        tags$style(HTML("
                        @import url('https://fonts.googleapis.com/css?family=Frank+Ruhl+Libre');
                        
                        body {
                        font-family: 'Frank Ruhl Libre', serif;
                        font-weight:  50;
                        line-height: 1.1;
                        color: #FFCBB8;
                        }
                        
                        "))),
                
                tags$head(tags$script('
                        var dimension = [0, 0];
                                      $(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                      });
                                      $(window).resize(function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                      });
                                      ')),
  tabsetPanel(
  tabPanel("Learn about regression", 
  # App title ----
  headerPanel("Is lockdown worth it?"),
  sidebarLayout(
    sidebarPanel(
      selectInput('reg_model', 'Regression Model', c("linear outcome",
                                                     "linear outcome with squared terms",
                                                     "linear outcome with quadratic terms","log-linear outcome",
                                                     "log-linear outcome with quadratic terms",
                                                      "Poisson with linear terms", "Poisson with quadratic terms"), 
                                                      "log-linear outcome"),
      selectInput('adjust', 'Crude or adjusted model?', c("crude (Lockdown \u2192 deaths or cases per million)",
                                                          "crude (Density \u2192 deaths or cases per million)", "adjusted"), "adjusted"),
      selectInput('outcome', 'Select model outcome', c("Deaths_per_million", "Cases_per_million")),
      selectInput('exclude_NY', 'Exclude New York?', c("include NY", "exclude NY")),
      selectInput('exclude_Miss', 'Exclude Mississipi?', c("include Mississipi", "exclude Mississipi")),
      selectInput('density', 'Use raw or weighted density', c("weighted density", "raw density"))),
  # Sidebar layout with input and output definitions ----
  # 3d plot tab----
  mainPanel("This plot illustrates the relationship between population density (raw or weighted; people per square mile), 
  lockdown (1 = lockdown; 0 = social distancing without lockdown)
  and cumulative deaths (or cases) in US states with Covid-19 (up to date). The data was kindly provided by
            Wilfred Reilly after reading his article on ",
  tags$a(href="https://www.spiked-online.com/2020/04/22/there-is-no-empirical-evidence-for-these-lockdowns/", "spiked-online."),
  "App by Simon Thornley, Epidemiologist, Auckland, New Zealand.",
          br(),
          br(),
  "The near horizontal plane in the direction of the 'lockdown' variable indicates little difference in per capita
  numbers of deaths or cases, comparing States without a lockdown to those have such a policy. In contrast, there is a strong relationship between per capita deaths or cases and population 
  density of the State.",
           br(),
          br(),
           plotlyOutput("three_d_plot", width = "100%", height = "100%"))
  
)),
  tabPanel("Diagnostic Plots",
           mainPanel(br(),
                    br(),
                    "Here, we have an added variable plot, in which the partial relationship between
                    the response and a regressor is adjusted for all other regressors.",
                    br(),
                    br(),
                    plotOutput("addedVariablePlot")
                  
                     )),
tabPanel("Regression nomogram", 
         headerPanel("Visualise a regression model as a nomogram"),
         mainPanel("This is a nomogram of the regression model, from Roger Marshall's ", tags$i("regplot"), " package.",
                   "\n The independent variables (Strategy: 1 = lockdown or 0 = social distancing and population density) 
                   contribute points that correspond to the outcome (deaths per million from Covid-19, by state). 
                   Density plots indicate the distribution of each independent variable. 
                                       Asterisks indicate statistical significance.",
                   br(),
                   br(),
                   plotOutput("regplot_logistic", width = "100%", height = "800px")))


))






# Define server logic ----
server <- function(input, output) {
  
 
   
#--- Render 3d plot ----
  
  output$three_d_plot <- renderPlotly({
    
    three_d_plot_with_regression_surface(df = CD, reg_model = input$reg_model,
                                         covars = if (input$density == "weighted density"){
                                            c("weighted_density", "Strategy")
                                         } else {
                                            c("Density", "Strategy")
                                         }, 
                                         outcome = input$outcome, adjusted_or_crude = input$adjust,
                                         dimension = input$dimension, NY = input$exclude_NY,
                                         Miss = input$exclude_Miss)
  })
    
  output$addedVariablePlot <- renderPlot({
      
      model <-  three_d_plot_with_regression_surface(df = CD, reg_model = input$reg_model,
                                                     covars = if (input$density == "weighted density"){
                                                        c("weighted_density", "Strategy")
                                                     } else {
                                                        c("Density", "Strategy")
                                                     }, 
                                                     outcome = input$outcome, adjusted_or_crude = input$adjust,
                                                     dimension = input$dimension, get_model = TRUE, NY = input$exclude_NY,
                                                     Miss = input$exclude_Miss)
      
      avPlots(model)
      
    })
 
  output$regplot_logistic <- renderPlot({
    model <- three_d_plot_with_regression_surface(df = CD, reg_model = input$reg_model,
                                                  covars = if (input$density == "weighted density"){
                                                     c("weighted_density", "Strategy")
                                                  } else {
                                                     c("Density", "Strategy")
                                                  }, 
                                                  outcome = input$outcome, adjusted_or_crude = input$adjust,
                                                  dimension = input$dimension, get_model = TRUE, NY = input$exclude_NY,
                                                  Miss = input$exclude_Miss)
    regplot(model, clickable = FALSE)
    
  })
  



}

shinyApp(ui = ui, server = server)



