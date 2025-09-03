library(plotly)

s <- s %>% mutate(Year = year(Date),  Month = month(Date, label = TRUE))

eight1 <- s %>% filter(Year < 1985)
eight2 <- s %>% filter(1985 <= Year, Year < 1990)
nine1 <- s %>% filter(1990 <= Year, Year < 1995)
nine2 <- s %>% filter(1995 <= Year, Year < 2000)
thou1 <- s %>% filter(2000 <= Year, Year < 2005)
thou2 <- s %>% filter(2005 <= Year, Year < 2010)
ten1 <- s %>% filter(2010 <= Year, Year < 2015)
ten2 <- s %>% filter(2015 <= Year, Year < 2020)
twenties <- s %>% filter(2020 <= Year, Year <= 2023)

e1_count <- eight1 %>% group_by(Month) %>% summarize(sum(Count))%>% mutate(Group = 9)
e2_count <- eight2 %>% group_by(Month) %>% summarize(sum(Count)) %>% mutate(Group = 8)
n1_count <- nine1 %>% group_by(Month) %>% summarize(sum(Count)) %>% mutate(Group = 7)
n2_count <- nine2 %>% group_by(Month) %>% summarize(sum(Count)) %>% mutate(Group = 6)
thou1_count <- thou1 %>% group_by(Month) %>% summarize(sum(Count)) %>% mutate(Group = 5)
thou2_count <- thou2 %>% group_by(Month) %>% summarize(sum(Count)) %>%  mutate(Group = 4)
tens1_count <- ten1 %>% group_by(Month) %>% summarize(sum(Count)) %>% mutate(Group = 3)
tens2_count <- ten2 %>% group_by(Month) %>% summarize(sum(Count)) %>% mutate(Group = 2)
twenties_count <- twenties %>% group_by(Month) %>% summarize(sum(Count)) %>% mutate(Group = 1)

combined_decades <- bind_rows(e1_count, e2_count, n1_count, n2_count, thou1_count, thou2_count, tens1_count, tens2_count, twenties_count)

Plot5 <- plot_ly(data = e1_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', line = list(width = 2), name = "1980 - 1984") %>%
  add_trace(data = e2_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', name = "1985 - 1989") %>%
  add_trace(data = n1_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', name = "1990 - 1994") %>%
  add_trace(data = n2_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', name = "1995 - 1999") %>%
  add_trace(data = thou1_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', name = "2000 - 2004") %>%
  add_trace(data = thou2_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', name = "2005 - 2009") %>%
  add_trace(data = tens1_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', name = "2010 - 2014") %>%
  add_trace(data = tens2_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', name = "2015 - 2019") %>%
  add_trace(data = twenties_count, x = ~Month, y = ~`sum(Count)`, z = ~Group, type = 'scatter3d', mode = 'lines', name = "2020 - 2023")


Plot5<- Plot5 %>% layout(title = "Count of Disasters by Month by Year", 
                         scene = list(xaxis = list(title = 'Month'),
                                      yaxis = list(title = 'Count'),
                                      zaxis = list(title = "Group of Years")),
       legend = list(title = "Grouped by 5 Year Periods"))
