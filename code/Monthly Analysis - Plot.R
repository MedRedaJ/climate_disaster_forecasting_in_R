## Summary Statistics - Count

autoplot(ts_count) + labs(title = "Monthly Counts")

acf(ts_count, main = "Monthly Count ACF")

boxplot(ts_count$Count, main = "Monthly Count Boxplot")

## Summary Statistics - Cost

autoplot(ts_cost) + labs(title = "Monthly Costs")

acf(ts_cost, main = "Monthly Cost ACF")

boxplot(ts_cost$Cost, main = "Monthly Cost Boxplot")

## Decomposition for Count

dcmp_count <- ts_count %>% model(classical_decomposition(Count, type = "multiplicative"))

stl_count <- ts_count %>% model(STL(Count))

components(dcmp_count) %>% autoplot()

components(dcmp_count) %>% gg_season(seasonal) + labs(title = "Count by Month")

components(stl_count) %>% autoplot()

autoplot(ts_count) + autolayer(components(stl_count), trend + season_year, colour = "red")

autoplot(ts_count) + autolayer(components(dcmp_count), trend + seasonal, colour = "red")

## Decomposition for Cost

dcmp_cost <- ts_cost %>% model(classical_decomposition(Cost, type = "additive"))

components(dcmp_cost) %>% autoplot()

stl_cost <- ts_cost %>% model(STL(Cost))

components(stl_cost) %>% autoplot()

gg_tsdisplay(ts_cost)

## Plot 1: Count by Month, disaster

Plot1 <- CombinedCount %>% ggplot(aes(x = Date, y = Count, fill = Disaster)) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Bar Chart: Count of Disasters by Month")

## Plot 2: Count by month, disaster, and cost line

Plot2 <- MonthlyCost %>% ggplot(aes(x = Date, y = Cost, group = 1)) + 
  geom_line() + 
  labs(title = "Cost by Month")

## Plot3: combination of the two

coeff <- 7500 ## calculated by doing the max of both data sets % by each other

Plot3 <- ggplot(NULL, aes(x = Date)) + 
  geom_bar(data = CombinedCount, aes(y = Count, fill = Disaster), position = "stack", stat = "identity") +
  geom_line(data = MonthlyCost, aes(y = Cost / coeff, group = 1, linetype = "Monthly Cost"), linewidth = 1.5) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~.*coeff, name = "Cost ($)")
  ) + labs(title = "Count and Cost of Disasters per Month")

## Plot4: Counts by event per month

Plot4 <- ggplot(CombinedCount, aes(x = Date, y = Count, group = Disaster, colour = Disaster)) + geom_line() + labs(title = "Line Chart: Count of Disasters by Month")

## Correlation

costdata <- t %>% mutate(Name = NULL, Count = NULL, key = row_number())
costdata <- costdata %>% as_tsibble(key = key)
