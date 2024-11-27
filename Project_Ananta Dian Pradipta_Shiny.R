setwd("E:/Work/S3/NJIT/1. 2024 Fall/DS 636 Data Analytics with R Programming/Project")
# --------------------------------
# Step 1: Set Up the R Environment
# --------------------------------
# install.packages("rvest")
# install.packages("httr")
# install.packages("xml2")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("plotly")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("shiny")

library(rvest)
library(httr)
library(xml2)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(plotly)
library(wordcloud)
library(RColorBrewer)
library(shiny)


default_yearstart = 2017
default_yearend = 2024

fetch_articles <- function(years) {
  withProgress(message = 'Initializing Environment', min = 0, max = length(years) + 1, {
    # --------------------------------
    # Step 2: Scraping Article Data
    # --------------------------------
    
    # Initialize a data frame to store all articles across years
    article.all <- data.frame(
      Year = numeric(),
      Month = factor(),
      Title = character(),
      Authors = character(),
      Correspondence_Author = character(),
      Email = character(),
      Publish_Date = character(),
      Abstract = character(),
      Keywords = character(),
      Article_Link = character(),
      stringsAsFactors = FALSE
    )
    
    # Loop through each year and extract articles
    i <- 1
    for (year in years) {
      setProgress(i, message = paste("Fetching Data from Source Website: ", year))
      # Define the URL for the specific year
      base_url <- paste0("https://jps.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&year=", year)
      # Read the first page to determine total articles
      first_page <- tryCatch(read_html(base_url), error = function(e) NULL)
      if (is.null(first_page)) next  # Skip the year if the page cannot be loaded
      
      # Extract total articles from the `data-total` attribute
      total_articles <- first_page %>%
        html_node(".c-page-layout__main div[data-test='search-content']") %>%
        html_attr("data-total") %>%
        as.numeric()
      
      if (is.na(total_articles)) next  # Skip if `data-total` is not found
      
      # Calculate the number of pages
      articles_per_page <- 50
      total_pages <- ceiling(total_articles / articles_per_page)
      
      # Loop through each page
      for (page in 1:total_pages) {
        url <- paste0(base_url, "&page=", page)
        # Read the page
        page_content <- tryCatch(read_html(url), error = function(e) NULL)
        if (is.null(page_content)) break  # Stop if the page cannot be loaded
        
        # Extract article links
        article_links <- page_content %>%
          html_nodes(".c-listing__title a") %>%
          html_attr("href") %>%
          paste0("https://jps.biomedcentral.com", .)
        
        # Loop through each article link and scrape details
        for (link in article_links) {
          # Read the article page
          article_page <- read_html(link)
          
          # Extract the required fields
          title <- article_page %>%
            html_node("h1.c-article-title") %>%
            html_text(trim = TRUE)
          
          authors <- article_page %>%
            html_nodes(".c-article-author-affiliation__authors-list") %>%
            html_text(trim = TRUE) %>%
            paste(collapse = ", ")
          
          correspondence_author <- article_page %>%
            html_nodes("#corresponding-author-list a") %>%
            html_text(trim = TRUE) %>%
            paste(collapse = ", ")
          
          email <- article_page %>%
            html_nodes("#corresponding-author-list a") %>%
            html_attr("href") %>%
            sub("^mailto:", "", .) %>% 
            paste(collapse = ", ")
          
          publish_date <- article_page %>%
            html_node(".c-article-identifiers__item time") %>%
            html_text(trim = TRUE)
          
          abstract <- article_page %>%
            html_node(".c-article-section__content") %>%
            html_text(trim = TRUE)
          
          keywords <- article_page %>%
            html_nodes(".c-article-subject-list__subject") %>%
            html_text(trim = TRUE) %>%
            paste(collapse = ", ")
          
          # Append details to the data frame
          article.all <- rbind(article.all, data.frame(
            Year = year,
            Title = title,
            Authors = authors,
            Correspondence_Author = correspondence_author,
            Email = email,
            Publish_Date = publish_date,
            Abstract = abstract,
            Keywords = keywords,
            Article_Link = link,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      i <- i+1
    }
    
    # --------------------------------
    # Step 3: Data Cleaning and Preprocessing
    # --------------------------------
    setProgress(length(years) + 1, message = "Data Cleaning and Preprocessing")
    articles <- article.all
    
    # Trim whitespace from all fields
    articles <- articles %>%
      mutate(across(everything(), ~ str_trim(.)))
    
    #------
    # NA or Empty
    #------
    # Check columns with NA or an empty string
    col_empty <- colSums(is.na(articles) | articles == "")
    print(col_empty)
    
    # Shows rows with empty column
    rows_empty <- articles %>%
      filter(if_any(everything(), ~ is.na(.) | . == ""))
    rows_empty
    
    # Remove rows where 'Authors' is NA or empty string
    articles <- articles %>% 
      filter(!is.na(Authors), Authors != "")
    
    
    #------
    # Duplicated Values
    #------
    # Check duplicated rows
    duplicate_rows <- articles[duplicated(articles), ]
    duplicate_rows
    
    # Check rows with same Title
    duplicate_rows_title <- articles[duplicated(articles[, c("Title")]), ]
    duplicate_rows_title
    
    # For each Article, Check Duplicated Authors and Keywords (comma separated)
    duplicate_check <- articles %>%
      separate_rows(Authors, sep = ",\\s*") %>%
      group_by(Title) %>%
      summarize(Duplicate_Authors = any(duplicated(Authors))) %>%
      left_join(
        articles %>%
          separate_rows(Keywords, sep = ",\\s*") %>%
          group_by(Title) %>%
          summarize(Duplicate_Keywords = any(duplicated(Keywords))),
        by = "Title"
      ) %>%
      left_join(
        articles %>%
          separate_rows(Correspondence_Author, sep = ",\\s*") %>%
          group_by(Title) %>%
          summarize(Duplicate_Correspondence_Author = any(duplicated(Correspondence_Author))),
        by = "Title"
      )
    
    duplicate_check = duplicate_check %>% 
      filter(duplicate_check$Duplicate_Authors == TRUE | duplicate_check$Duplicate_Keywords == TRUE | duplicate_check$Duplicate_Correspondence_Author == TRUE)
    
    # Remove Duplicate Authors, Keywords, and Correspondence Author for each Article
    articles <- articles %>%
      rowwise() %>%  # Ensure row-by-row processing
      mutate(
        Authors = paste(unique(strsplit(Authors, ",\\s*")[[1]]), collapse = ", "),  # Remove duplicates
        Keywords = paste(unique(strsplit(Keywords, ",\\s*")[[1]]), collapse = ", "),  # Remove duplicates
        Correspondence_Author = paste(unique(strsplit(Correspondence_Author, ",\\s*")[[1]]), collapse = ", ")  # Remove duplicates
      ) %>%
      ungroup()
    
    
    # Convert publish date as Date, Year as integer, and Month as Factor
    articles <- articles %>%
      mutate(
        Publish_Date = as.Date(Publish_Date, format = "%d %B %Y"),
        Year = as.integer(Year),
        Month = factor(format(Publish_Date, "%B"), levels = month.name, ordered = TRUE)
      )
    
    # --------------------------------
    # Export to CSV
    # --------------------------------
    write.csv(articles, "DS636_Project_Ananta Dian Pradipta.csv", row.names = TRUE)
    
    return(articles)
  })
}

# UI (User Interface)
ui <- fluidPage(
  titlePanel("DS636 Project: Extracting and Analyzing Journal Article Data"),
  p(HTML("<b>Source: <a href='https://jps.biomedcentral.com/articles' target='_blank'>The journal of Physiological Sciences</b></a>")),
  br(),
  sidebarLayout(
    sidebarPanel(
      
      br(),
      sliderInput("yearRange", "Select Year Range:",
                  min = 2015, max = 2024,
                  value = c(default_yearstart, default_yearend),
                  sep = ""),
      br(),
      br(),
      radioButtons("chart", "Select Chart Layout:",
                   choices = list("Articles Published by Year" = "chart1",
                                  "Top Keywords" = "chart2",
                                  "Top Contributing Authors" = "chart3",
                                  "Monthly Trend Publication" = "chart4", 
                                  "EDA" = "eda"), 
                   selected = "chart1"),
      width = 3
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.chart == 'chart1'",
        plotlyOutput("chart1")
      ),
      conditionalPanel(
        condition = "input.chart == 'chart2'",
        fluidRow(
          column(4,
                 plotOutput("wordcloud")
                 
          ),
          column(8,
                 plotlyOutput("chart2")
          )
        )
      ),
      conditionalPanel(
        condition = "input.chart == 'chart3'",
        plotlyOutput("chart3")
      ),
      conditionalPanel(
        condition = "input.chart == 'chart4'",
        plotlyOutput("chart4")
      ),
      conditionalPanel(
        condition = "input.chart == 'eda'",
        h4("Exploratory Data Analysis (EDA)"),
        verbatimTextOutput("summary"),
        verbatimTextOutput("str")
      ),
      br(),
      br(),
      tableOutput("dataset"),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {
  articles <- reactive({
    req(input$yearRange)
    fetch_articles(input$yearRange[1]:input$yearRange[2])
  })
  
  # --------------------------------
  # Step 4: Data Analysis and Visualization
  # --------------------------------
  # --------------------------------
  # Report 1: Articles Published by Year
  # --------------------------------
  output$chart1 <- renderPlotly({
    req(articles())
    stacked_data <- articles() %>%
      group_by(Year, Month) %>%
      summarize(Article_Count = n(), .groups = "drop") %>%
      arrange(Month)
    
    # Calculate total articles per year
    yearly_totals <- stacked_data %>%
      group_by(Year) %>%
      summarize(Total_Articles = sum(Article_Count), .groups = "drop")
    
    # Create the stacked bar chart
    bar_chart <- plot_ly(
      data = stacked_data,
      x = ~Year,
      y = ~Article_Count,
      color = ~Month, 
      type = 'bar',
      name = ~Month
    )
    
    # Add the line chart for yearly totals
    combined_chart <- bar_chart %>%
      add_trace(
        data = yearly_totals,
        x = ~Year,
        y = ~Total_Articles,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'blue', width = 2),
        marker = list(color = 'blue'),
        name = 'Yearly Total',
        inherit = FALSE
      ) %>%
      layout(
        title = paste("Articles Published by Year (", input$yearRange[1], "-", input$yearRange[2], "), with Monthly Segments", sep = ""),
        barmode = 'stack',
        xaxis = list(
          title = "Year",
          tickmode = "array",
          tickvals = unique(stacked_data$Year),
          tickformat = "d"
        ),
        yaxis = list(title = "Number of Articles Published"),
        legend = list(title = list(text = "Legend"))
      )
    
    combined_chart
  })
  
  # --------------------------------
  # Report 2: Top Keywords
  # --------------------------------
  output$chart2 <- renderPlotly({
    req(articles())
    keywords_split <- articles() %>%
      separate_rows(Keywords, sep = ",") %>%
      group_by(Keywords) %>%
      summarize(Frequency = n()) %>%
      arrange(desc(Frequency)) %>%
      filter(Keywords != "")
    
    # Top 10 keywords
    top_keywords <- keywords_split[1:10, ]
    
    plot_ly(top_keywords, x = ~reorder(Keywords, Frequency, decreasing=TRUE), y = ~Frequency, type = 'bar', marker = list(color = 'green')) %>%
      layout(
        title = paste("Top 10 Keywords (", input$yearRange[1], "-", input$yearRange[2],")", sep = ""),
        xaxis = list(title = "Keywords"), 
        yaxis = list(title = "Frequency"))
  })
  
  # --------------------------------
  # Report 3: Top Contributing Authors
  # --------------------------------
  output$chart3 <- renderPlotly({
    req(articles())
    authors_split <- articles() %>%
      separate_rows(Authors, sep = ",") %>%
      group_by(Authors) %>%
      summarize(Frequency = n()) %>%
      arrange(desc(Frequency)) %>%
      filter(Authors != "Unknown")
    
    # Top 10 authors
    top_authors <- authors_split[1:10, ]
    
    plot_ly(top_authors, x = ~Frequency, y = ~reorder(Authors, Frequency),type = 'bar', orientation = 'h',marker = list(color = 'purple')) %>%
      layout(
        title = paste("Top 10 Contributing Authors (", input$yearRange[1], "-", input$yearRange[2], ")", sep = ""),
        xaxis = list(title = "Number of Contributions"),
        yaxis = list(title = "Authors"))
  })
  
  # --------------------------------
  # Report 4: Monthly Trend Publication
  # --------------------------------
  output$chart4 <- renderPlotly({
    req(articles())
    all_articles <- articles()
    
    # Group by Year-Month and calculate total publications
    articles_by_month <- all_articles %>%
      group_by(Month) %>%
      summarize(Total_Publications = n()) %>%
      arrange(Month)
    
    plot_ly(articles_by_month, x = ~Month, y = ~Total_Publications, type = 'scatter', mode = 'lines+markers',line = list(color = 'blue')) %>%
      layout(
        title = paste("Publication Trends by Month Across Years (", input$yearRange[1], "-", input$yearRange[2], ")", sep = ""),
        xaxis = list(title = "Month", tickformat = "%Y-%m", tickangle = 45),
        yaxis = list(title = "Total Publications")
      )
  })
  
  # --------------------------------
  # Wordcloud
  # --------------------------------
  output$wordcloud <- renderPlot({
    req(articles())
    keyword_frequencies <- articles() %>%
      separate_rows(Keywords, sep = ",") %>%  # Split keywords into individual rows
      filter(Keywords != "") %>%  # Exclude unspecified keywords
      count(Keywords, sort = TRUE)  # Count frequencies and sort
    
    keyword_frequencies <- keyword_frequencies %>%
      mutate(Keywords = ifelse(nchar(Keywords) > 13, substr(Keywords, 1, 10), Keywords))
    
    # Create the word cloud
    # set.seed(123)  # For reproducibility
    wordcloud(
      words = keyword_frequencies$Keywords,  # Words to display
      freq = keyword_frequencies$n,          # Frequencies of the words
      scale = c(1.5, 0.5),
      min.freq = 2,                          # Minimum frequency of words to include
      max.words = 100,                       # Maximum number of words to display
      random.order = FALSE,                  # Arrange words by frequency
      colors = brewer.pal(8, "Dark2")        # Color palette
    )
  })
  
  # --------------------------------
  # EDA
  # --------------------------------
  # Data Summary
  output$summary <- renderPrint({
    req(articles())
    summary(articles())
  })
  
  # Data Str
  output$str <- renderPrint({
    req(articles())
    str(articles())
  })
  
  # --------------------------------
  # Data Table
  # --------------------------------
  output$dataset <- renderTable({
    req(articles())
    articles() %>%
      mutate(
        Title = paste0('<a href="', Article_Link, '" target="_blank">', Title, '</a>'),
        Publish_Date = format(Publish_Date, "%d-%b-%Y")
      ) %>%
      select(Year, Title, Authors, Correspondence_Author, Email, Publish_Date)
  }, sanitize.text.function = identity, striped = TRUE, bordered = TRUE, hover = TRUE)
}

# --------------------------------
# Run the app
# --------------------------------
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
