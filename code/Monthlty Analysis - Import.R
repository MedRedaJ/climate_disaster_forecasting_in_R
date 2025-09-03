library(fpp3)

## Initial Loading and Transformations

data <- read.csv("StartEndDisasterCSV.csv")

t <- tibble(data)

t <- t %>% mutate(Start = as.Date(Start), End = as.Date(End), Cost = as.numeric(Cost)) %>% filter(year(Start) <= 2023)

t <- t %>% group_by(Name, Disaster, Cost, Count)

t <- t %>% summarize(Date = list(seq(floor_date(min(Start),unit = "month"), 
                            floor_date(max(End), unit = "month"),
                            by = "month"))) 

t <- t %>% ungroup()

t <- t %>% mutate(Cost = Cost / lengths(Date))

t <- t %>% tidyr::unnest(cols = Date)

s <- t

t <- t %>% mutate(Date = yearmonth(Date))

t <- t %>% arrange(Date)

ts <- t %>% mutate(Key = row_number())

## Month Only Data

m <- t %>% mutate(Date = month(Date, label = TRUE), Key = row_number())

## Parameters

MonthlyCount <- m %>% group_by(Date) %>% summarize(sum(Count))

DisasterCount <- m %>% group_by(Disaster) %>% summarize(sum(Count))

CombinedCount <- m %>% group_by(Date, Disaster) %>% summarize(sum(Count))

CombinedCount <- rename(CombinedCount, Count = `sum(Count)`)

MonthlyCost <- m %>% group_by(Date) %>% summarize(sum(Cost))

MonthlyCost <- rename(MonthlyCost, Cost = `sum(Cost)`)

## Creating a Time Series

ts_count <- ts %>% group_by(Date) %>% summarize(Count = sum(Count))

ts_count <- ts_count %>% as_tsibble(index = Date)

ts_count <- ts_count %>% tsibble:: fill_gaps()
ts_count[is.na(ts_count)] <- 0

ts_cost <- ts %>% group_by(Date) %>% summarize(Cost = sum(Cost))

ts_cost <- ts_cost %>% as_tsibble(index = Date)

ts_cost <- ts_cost %>% tsibble:: fill_gaps()
ts_cost[is.na(ts_cost)] <- 0

ts_corr <- ts %>% group_by(Date, Disaster) %>% summarize(Cost = sum(Cost))
ts_corr <- ts_corr %>% ungroup() %>% mutate(key = row_number())

ts_corr <- ts_corr %>% as_tsibble(index = Date, key = key)

## ts_corr$Disaster <- factor(ts_corr$Disaster, levels = c(levels(ts_corr$Disaster)))

