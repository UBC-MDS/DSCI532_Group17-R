library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dashTable)
library(ggplot2)
library(plotly)
library(purrr)
library(tidyverse)
source("data_manager.R")


data <- read_csv('data/processed/processed_data.csv')
colnames(data)[8] <- "Value(\u20AC)"
colnames(data)[9] <- "Wage(\u20AC)"
table <- make_table(data)
charts <- plot_altair(data)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      htmlBr(),
      dbcRow(
        list(
          dbcCol(
            list(
              htmlDiv(
                  id='placebolder-left',
                  style=list('height' = '20vh')
                ),
              htmlDiv('Rank By:'),
              dccDropdown(
                id='rankby-widget',
                value='Overall',
                options=(data %>% colnames) %>% map(function(col) list(label = col, value = col))
                ),
              htmlBr(),
              htmlDiv('Order:'),
              dccDropdown(
                id='order-widget',
                value='True',
                options=list(list(label = 'Descending', value =  'False'),
                             list(label = 'Ascending', value = 'True'))
                ),
              htmlBr(),
              htmlDiv('Continent:'),
              dccDropdown(
                id='filter-cont-widget',
                value='',
                options=map(unique(data$Continent), function(val) list(label = val, value = val))
                ),
              htmlBr(),
              htmlDiv('Club:'),
              dccDropdown(
                id='filter-club-widget',
                value='',
                options=map(unique(data$Club[!is.na(data$Club)]), function(val) list(label = val, value = val))
              )), md=3),
          dbcCol(
            list(
              htmlH1('FIFA STAR BOARD', style=list(width= '50vh', height= '10vh')),
              htmlH4('Select Attributes:'),
              dccDropdown(
                id='attribute-widget',
                value=list('Name', 'Nationality', 'Age', 'Value(\u20AC)', 'Overall'),
                options=(data %>% colnames) %>% map(function(col) list(label = col, value = col)),
                multi=TRUE
                ),
              dashDataTable(
                id = "table",
                columns = lapply(colnames(table), 
                                 function(colName){
                                   list(
                                     id = colName,
                                     name = colName
                                   )
                                 }),
                data = df_to_list(table)
                )
              )),
         dbcCol(
           list(
             htmlDiv(
               id='placebolder-right',
               style=list('height' = '10vh')
               ),
             htmlDiv('Top 10 by Club and by Nationality'),
             dccGraph(
               id='charts-1',
               figure = ggplotly(charts[[1]]),
               style=list('border-width'= '0', 'width' = '150%', 'height' = '350px')
               ),
             dccGraph(
               id='charts-2',
               figure = ggplotly(charts[[2]]),
               style=list('border-width'= '0', 'width' = '150%', 'height' = '350px')
             )
           ), md=3)
          )))))
    
# updates table from all 5 dropdowns
#app$callback(
#   output(id = 'table', property = 'figure'),
#   list(input('rankby-widget', 'value'),
#        input('order-widget', 'value'),
#        input('attribute-widget', 'value'),
#        input('filter-cont-widget', 'value'),
#        input('filter-club-widget', 'value')),
#   function(by, order, cols, filter_cont, filter_club){
#     update_table(data, by, order == "True", cols, filter_cont, filter_club)
#     
#   }
# )




# updates charts with Rank-by selection 
# updates only when selected col is numeric
app$callback(
   output(id = 'charts', property = 'figure'),
   list(input('rankby-widget', 'value')),
   function(by){
     if (is_numeric(data$by) == FALSE){
       return(charts)
     } else{
       plot_altair(data, by=by)
     }
     
   }
 )


app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050))
