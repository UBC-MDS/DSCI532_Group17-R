# authors: Yuanzhe Marco Ma, Sicheng Sun, Guanshu Tao, Yuan Xiong
# date: 2021-01-30
library(dash)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(tidyverse)

# make initial table, land-on page
make_table <- function(data) {
    table <- data %>% select(Name, Nationality, Age, "Value(\u20AC)", Overall) %>% 
        arrange('Overall', ascending=FALSE) %>%
        head(15)
    
    table <- cbind(table, 'Ranking' = 1:nrow(table))
    
    return(table)
}

# make initial charts, land-on page
plot_altair <- function(data, by=Overall, by_str='Overall', ascending=FALSE, show_n=10) {
    df_nation <- data %>% 
        group_by(Nationality) %>%
        summarise({{by}} := mean({{by}})) %>%
        arrange({{by}}, ascending=FALSE) %>%
        head(show_n) 

    nation_chart <- ggplot(df_nation, aes(x = reorder(Nationality, -{{by}}), y = {{by}})) +
        geom_bar(stat = 'identity') + 
        labs(x = "Nationality", y = by_str) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
    df_club <- data %>% 
        group_by(Club) %>%
        summarise({{by}} := mean({{by}})) %>%
        arrange({{by}}, ascending=FALSE) %>%
        head(show_n) 
    
    club_chart <- ggplot(df_club, aes(x = reorder(Club, -{{by}}), y = {{by}})) +
        geom_bar(stat = 'identity') + 
        labs(x = "Club", y = by_str) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


    # chart_nationality <- df %>% 
    #     group_by(Nationality) %>% 
    #     summarise({{by}} := mean({{by}})) %>% 
    #     arrange(desc({{by})) %>% 
    #     top_n(10) %>% 
    #     ggplot(aes(x = reorder(Nationality, -{{by}}), y = {{by}})) +
    #     geom_bar(stat = 'identity') +
    #     labs(x = 'Nationality', y = by_str) +
    #     theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.5))  



    # chart_nationality <- df %>% 
    #     group_by(Club) %>% 
    #     summarise({{by}} := mean({{by}})) %>% 
    #     arrange(desc({{by})) %>% 
    #     top_n(10) %>% 
    #     ggplot(aes(x = reorder(Club, -{{by}}), y = {{by}})) +
    #     geom_bar(stat = 'identity') +
    #     labs(x = 'Club', y = by_str) +
    #     theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.5))   


    return(list(nation_chart, club_chart))
}


# Updates table from given parameters
#
# df : dataframe, processed dataset
# by : str, column to sort by
# order : bool, determines ascending or not
# cols : list(str), columns to include in table
# filter_natn: str, column to filter Nationality on
# filter_club: str, column to filter Club on
#
# return : dataframe, top ten rows of the sorted dataset
