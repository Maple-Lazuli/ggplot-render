## functions.R ##
## ----------- ##
#* @get /totalHoursWorked
#* @serializer unboxedJSON
totalHoursWorked <- function() {
  library(listviewer)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  data <- read.csv("/home/ada/Desktop/slots.scv")
  #Count Days Worked
  
  df <- data %>% mutate(startDtg = as.Date(start_dtg, format="%b %d %Y"), stopDtg = as.Date(stop_dtg, format="%b %d %Y"),) %>%
    mutate(user_id = as.character(user_id)) %>% count(user_id)
  
  data_output <- ggplot(df, aes(x = user_id, y = n, fill = user_id)) + geom_col()
  out <- ggplotly(data_output)
  #data_output <- "hello world"
  list( plotly_json(out,FALSE))
}


#* @get /cummulativeHoursWorkedOverTime
#* @serializer unboxedJSON
cummulativeHoursWorkedOverTime <- function() {
  
  
  library(listviewer)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  data <- read.csv("/home/ada/Desktop/slots.scv")
  #Count Days Worked
  ##cum Hours Over Time
  df <- data %>% mutate(startDtg = as.POSIXct(start_dtg, format="%b %d %Y %H:%M:%S"), stopDtg = as.POSIXct(stop_dtg, format="%b %d %Y %H:%M:%S"),) %>%
    mutate(user_id = as.character(user_id)) %>% mutate (hoursWorked = as.numeric(stopDtg - startDtg))  %>% group_by(user_id) %>% mutate(cum = cumsum(hoursWorked))
  data_output <-  ggplot(df, aes(x = startDtg, y = cum, color = user_id)) + geom_line()
  out <- ggplotly(data_output)
  list( plotly_json(out,FALSE))
}
#* @get /dataBasePull
#* @serializer unboxedJSON
dataBasePull <- function() {
  
  library(listviewer)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(tidyjson)
  library(httr)
  
  data <- (content(GET("http://localhost:5501/v1/slot"), "parse"))
  df <- data %>% spread_all %>% mutate(user_id = as.character(user_id))%>% count(user_id) %>% group_by(user_id)
  data_output <- ggplot(df, aes(x =user_id, y = n, fill = user_id)) + geom_col()
  out <- ggplotly(data_output)
  list( plotly_json(out,FALSE))
}

#* @get /dataBasePull2
#* @serializer unboxedJSON
dataBasePull2 <- function() {
  
  library(listviewer)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(tidyjson)
  library(httr)
  
  data <- (content(GET("http://localhost:5501/v1/slot"), "parse"))
  df <- data %>% spread_all %>% mutate(startDtg = as.POSIXct(start_dtg, "UTC", "%Y-%m-%dT%H:%M:%S"), stopDtg = as.POSIXct(stop_dtg, "UTC", "%Y-%m-%dT%H:%M:%S"),) %>%
    mutate(user_id = as.character(user_id)) %>% mutate (hoursWorked = as.numeric(stopDtg - startDtg))  %>% group_by(user_id) %>% mutate(cum = cumsum(hoursWorked))
  
  
  data_output <-  ggplot(df, aes(x = startDtg, y = cum, color = user_id)) + geom_line()
  out <- ggplotly(data_output)
  list( plotly_json(out,FALSE))
}

#* @get /dataBasePull3
#* @serializer unboxedJSON
dataBasePull3 <- function() {
  
  library(listviewer)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(tidyjson)
  library(httr)
  library(gapminder)
  
  by_year_continent <- gapminder %>% group_by(year, continent) %>% summarize(medianGdpPercap = median(gdpPercap))
  
  data_output <-  ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color=continent)) + geom_line() + expand_limits(y = 0) 
  out <- ggplotly(data_output)
  list( plotly_json(out,FALSE))
}


#* @get /dataBasePull4
#* @serializer unboxedJSON
dataBasePull4 <- function() {
  
  library(listviewer)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(tidyjson)
  library(httr)
  library(gapminder)
  
  gapminder_1952 <- gapminder %>%
    filter(year == 1952)
  
  # Create a boxplot comparing gdpPercap among continents
  
  data_output <- ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
    geom_boxplot() +
    scale_y_log10() + labs(title = "Comparing GDP per capita across continents")

  out <- ggplotly(data_output)
  list( plotly_json(out,FALSE))
}


