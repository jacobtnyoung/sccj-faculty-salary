# --------------------------------------------------------------- #
# --------------------------------------------------------------- #


# ----
# set up the libraries needed

library( shinydashboard ) # for rendering the dashboard
library( shiny )          # for running shiny
library( sna )            # for working with the network
library( network )        # for working with the network
library( tidyverse )      # for handling data
library( here )           # for local directory
library( dplyr )
library( tidyr )
library( ggplot2 )
library( openxlsx )
library( ggrepel )
library( DT )
library( plotly )

# ----
# load the data file
dat <- read.xlsx( 
  here( "sccj-faculty-salary-data.xlsx" ) 
)


# ----
# This section preps the data

# create year labels
label_years <- min( unique( dat$year ) ): max( unique( dat$year ) )



# --------------------------------------------------------------- #

# ----
# USER INTERFACE

ui <- fluidPage(
  titlePanel("ASU SCCJ Faculty Salaries (2018-2024)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # select rank
      checkboxGroupInput(
        inputId = "rank",
        label = "Rank",
        choices = list( "Assistant" = "Assistant", "Associate" = "Associate", "Full" = "Professor" ),
        selected = "Professor" 
        ),
      
      # select sex
      checkboxGroupInput( 
        inputId = "sex" ,
        label = "Sex",
        choices = list( "Male" = "M", "Female" = "F" ),
        selected = "F" 
        ),
      
      # add some spacing
      br(),  # spacing
      tags$hr(),  # subtle divider
      h4("Other options"),
      
      # exclude those with joint appointments
      checkboxInput(
        inputId = "exclude_joint",
        label = "Exclude faculty with joint appointments",
        value = FALSE
      ),
      
      # exclude cases that have administrative appointments
      checkboxInput(
        inputId = "exclude_admin",
        label = "Exclude director/assoc. dean appointments",
        value = FALSE
      )
      
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Overview",
                 br(),
                 plotlyOutput("salary_plot", height = "600px"),
        ),
        tabPanel("About",
                 br(),
                 p(
                   "Data source: The ",
                   a(
                     "State Press",
                     href = "https://www.statepress.com/article/2017/04/spinvestigative-salary-database",
                     target = "_blank"
                   ),
                   " maintains a database of salaries for all Arizona government employees. ",
                   "The data are available ",
                   a(
                     "here",
                     href = "https://www.statepress.com/article/2017/04/spinvestigative-salary-database",
                     target = "_blank"
                   ),
                   ". I have compiled the data for CCJ faculty and those data are used in the dashboard. ",
                   "For some cases there are no values reported. This is because the individual was not yet employed at ASU ",
                   "or they were not in the database (for reasons unknown to me)."
                 ),
                 p(
                   "Please report any needed corrections to the ",
                   a(
                     "Issues",
                     href = "https://github.com/jacobtnyoung/sccj-faculty-salary/issues/new",
                     target = "_blank"
                   ),
                   " page. Thanks!"
                 )
      )
    )
  )
)
)


# --------------------------------------------------------------- #
# --------------------------------------------------------------- #

# ----
# SERVER

server <- function(input, output, session) {
  
  # reactive filtered dataset (defensive: handles NULL)
  filtered_dat <- reactive({
    req(dat)
    
    dat %>%
      # no-op mutate placeholder in case you need coercion, e.g. year = as.integer(year)
      mutate() %>%
      filter(
        
        # filter for rank
        (is.null(input$rank) | rank %in% input$rank),
        
        # filter for sex
        (is.null(input$sex)  | sex %in% input$sex),
        
        # filter for joint appointment
        (!isTRUE(input$exclude_joint) | joint_appointment == 0),
        
        # filter for admin appointment
        (!isTRUE(input$exclude_admin) | admin_appointment == 0),
        
      )
  })
  
  # renderPlot using the reactive filtered_dat()
  output$salary_plot <- renderPlotly({
    
    df <- filtered_dat()
    req(nrow(df) > 0)
    
    # compute label_years fallback if needed
    ly <- if (exists("label_years") && length(label_years) > 0) {
      label_years
    } else {
      sort(unique(df$year))
    }
    
    # prepare tooltip text (this is what shows on hover)
    df <- df %>%
      mutate(
        tooltip_text = paste0(
          "Name: ", ifelse(is.na(l.name), "<unknown>", l.name),
          "<br>Year: ", year,
          "<br>Salary: $", scales::comma(rate)
        )
      )
    
    p <- ggplot(
      df,
      aes(x = year, y = rate, group = ID, text = tooltip_text, color = rank)
    ) +
      geom_line() +
      geom_point(aes(text = tooltip_text)) +
      scale_color_brewer(palette = "Dark2", name = "Rank") +
      scale_y_continuous(labels = scales::dollar) +
      scale_x_continuous(
        breaks = ly,
        labels = as.character(ly)
      ) +
      ggtitle(
        paste0(
          "ASU CCJ Faculty Salaries (",
          head(ly, 1), " to ", tail(ly, 1), ")"
        )
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.1, y = -0.2)
      )
  })
  
}
# --------------------------------------------------------------- #

# ----
# run the application

shinyApp( ui = ui, server = server )

# --------------------------------------------------------------- #
# --------------------------------------------------------------- #