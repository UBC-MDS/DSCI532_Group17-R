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
data$Height <- as.integer(substring(data$Height, 3, 5)) + 
  12*as.integer(substring(data$Height, 1, 1))

table <- make_table(data) %>% 
          select('Ranking', 'Name', everything())
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
                options=((data %>% colnames)[2:11]) %>% map(function(col) list(label = col, value = col)),
                style=list('width'= '80%')
                ),
              htmlBr(),
              htmlBr(),
              htmlDiv('Order:'),
              dccDropdown(
                id='order-widget',
                value='False',
                options=list(list(label = 'Descending', value =  'False'),
                             list(label = 'Ascending', value = 'True')),
                style=list('width'= '80%')
                ),
              htmlBr(),
              htmlBr(),
              htmlDiv('Continent:'),
              dccDropdown(
                id='filter-cont-widget',
                value='',
                options=map(unique(data$Continent), function(val) list(label = val, value = val)),
                style=list('width'= '80%')
                ),
              htmlBr(),
              htmlBr(),
              htmlDiv('Club:'),
              dccDropdown(
                id='filter-club-widget',
                value='',
                options=map(unique(data$Club[!is.na(data$Club)]), function(val) list(label = val, value = val)),
                style=list('width'= '80%')
              )), md=3),
          dbcCol(
            list(
              htmlH1('FIFA STAR BOARD', style=list(textAlign = 'center', color = '#0000A0', width= '50vh', height= '9vh')),
              htmlH4('Select Attributes:'),
              dccDropdown(
                id='attribute-widget',
                value=list('Name', 'Nationality', 'Age', 'Value(\u20AC)', 'Overall'),
                options=((data %>% colnames)[2:11]) %>% map(function(col) list(label = col, value = col)),
                multi=TRUE,
                style=list(height= '10%')
                ),
              dashDataTable(
                style_table = list(
                  height = '100'
                ),
                id = "table",
                columns = lapply(colnames(table), 
                                 function(colName){
                                   list(
                                     id = colName,
                                     name = colName
                                   )
                                 }),
                data = df_to_list(table)
                # style_data_conditional = list(list('if' = list(column_id = 'Ranking'),width = '10px'),
                #                               list( 'if' = list(column_id = 'Name'),width = '20px'),
                #                               list( 'if' = list(column_id = 'Nationality'), width = '20px'),
                #                               list( 'if' = list(column_id = 'Value(\u20AC)'), width = '20px'),
                #                               list('if' = list(column_id = 'Overall'), width = '20px' ),
                #                               list('if' = list(column_id = 'Age'),width = '20px'),
                #                               list('if' = list(column_id = 'Potential'),width = '20px'),
                #                               list('if' = list(column_id = 'Overall'),  width = '20px'),
                #                               list( 'if' = list(column_id = 'Club'),  width = '20px'),
                #                               list('if' = list(column_id = 'Weight(lbs)'),width = '20px'),
                #                               list('if' = list(column_id = 'Value'),width = '20px')
                #                               )
                )
              )
            ),
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
               style=list('border-width'= '0', 'width' = '120%', 'height' = '350px')
               ),
             dccGraph(
               id='charts-2',
               figure = ggplotly(charts[[2]]),
               style=list('border-width'= '0', 'width' = '120%', 'height' = '350px')
             )
           ), md=3)
          )
         )
        )
      )
    )
    
# updates table from all 5 dropdowns
app$callback(
  list(output(id = 'table', property = 'columns'),
       output(id = 'table', property = 'data')),
  list(input('rankby-widget', 'value'),
       input('order-widget', 'value'),
       input('attribute-widget', 'value'),
       input('filter-cont-widget', 'value'),
       input('filter-club-widget', 'value')),
  function(by, order, cols, filter_cont, filter_club){
    new_table <- update_table(data, by, order == 'True', cols, filter_cont, filter_club)
    
    # returns
    list(lapply(colnames(new_table), 
                function(colName){
                  list(
                    id = colName,
                    name = colName
                  )
                }), 
         df_to_list(new_table))
  }
)


# updates charts with Rank-by selection
# updates only when selected col is numeric
app$callback(
  list(output(id = 'charts-1', property = 'figure'),
       output(id = 'charts-2', property = 'figure')),
  list(input('rankby-widget', 'value')),
  function(by){
    if (is_numeric(data[[by]]) == FALSE){
      print(data[by])
      print('not numeric')
      list(ggplotly(charts[[1]]), ggplotly(charts[[2]]))
    } else{
      print('numeric')
      print(data[by])
      charts_temp <- plot_altair(data, by=by)
      list(ggplotly(charts_temp[[1]]), ggplotly(charts_temp[[2]]))
    }
  }
)

app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050))
