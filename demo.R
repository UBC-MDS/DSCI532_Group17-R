library(dash)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(tidyverse)
data <- read_csv('data/processed/processed_data.csv')

test <- function(data, by='Height', ascending=FALSE, show_n=10) {
    df_nation <- data %>% 
        group_by(Nationality) %>%
        summarise({{by}} := mean({{by}}) %>%
        {if (ascending) arrange(., !!as.name(by)) 
            else arrange(., desc(!!as.name(by)))} %>%
        head(show_n) 
    
    nation_chart <- ggplot(df_nation, 
                           aes(x = reorder(Nationality, -!!as.name(by)), 
                               y = !!as.name(by))) +
        geom_bar(stat = 'identity') +
        labs(x = "Nationality", y = by) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    # return(df_nation)
    return(nation_chart)
}
    
test(data)
