p <- economics %>%
  tidyr::gather(variable, value, -date) %>%
  transform(id = as.integer(factor(variable))) %>%
  plot_ly(x = ~date, y = ~value, color = ~variable, colors = "Dark2",
          yaxis = ~paste0("y", id)) %>%
  add_lines() %>%
  subplot(nrows = 5, shareX = TRUE)

p

economics

economics %>% 
  tidyr::gather(variable, value, -date)

economics %>% 
  tidyr::gather(colnam, valofthatcol, -date)

z <- economics %>% 
  tidyr::gather(colnam, valofthatcol, -date) %>% 
  transform(id = as.integer(factor(colnam))) %>% 
  plot_ly(x = ~date, y = ~valofthatcol, color = ~colnam, colors = "Dark2",
          yaxis = ~paste0("y", id)) %>% 
  add_lines() %>% 
  subplot(nrows = 5, shareX = TRUE)
z

economics <- economics
