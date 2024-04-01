# Load required libraries

library(plotly)
library(tidyverse)

# Read the dataset from the URL
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/public-picks.csv")

# Prepare data for plotting
data_long <- pivot_longer(data, -c(YEAR, TEAMNO, TEAM), names_to = "Round", values_to = "Percentage")
data_long$Percentage <- as.numeric(sub("%", "", data_long$Percentage))  # Convert percentage to numeric

# Create a sunburst chart
fig <- plot_ly(data_long, ids = ~paste(YEAR, TEAMNO, sep = "_"), 
               labels = ~TEAM, parents = ~YEAR, values = ~Percentage, type = 'sunburst',
               branchvalues = "total") %>% 
  layout(title = list(text = "<b>Progression of NCAA basketball teams across different rounds</b>", 
                      font = list(size = 18, color = "black", family = "Arial")))

caption <- "Source: 2024 Tidy Tuesday week 13 | Graphic: Simisani Ndaba"
fig <- fig %>%
  layout(annotations = list(text = caption, 
                            font = list(size = 14, family = "Arial"), 
                            x = 0.5, y = -0.1, 
                            showarrow = FALSE, xref = "paper", yref = "paper"))


# Show the plot
fig

ggsave("2024TidyTuesdayweek13.png", width = 10, height = 10)



