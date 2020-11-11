#Importing, naming, cleaning datasets

library(tidyverse)
BCS <- read_csv("Data/BeeCensusState.csv")
BCS
BSS <- read_csv("Data/BeeSurveyState.csv")
head(BSS)
BSS <- BSS %>%
  select(Year, Period, State, State.ANSI, Data.Item, Value)
BSS
BSSw <- BSS %>%
  pivot_wider(names_from = "Data Item", values_from = "Value")
BSSw
write_csv(BSSw, "Data/BeeStateSurveyClean.csv")
BLC <- read_csv("Data/BeeLossClean.csv")
head(BLC)
BLC <- read_csv("Data/BeeLossClean2.csv")
head(BLC)

#Figure out how to make "Year" a date object 
BLC <- BLC %>%
  as.Date(BLC, Year, format = "%Y", trim_ws = TRUE)

#BLC$Year <- parse_date(BLC$Year, col_date(format = "%Y"))

#Add regions and divisions to BLC
#Data is from census.gov 
st_reg <- read_csv("Data/regions.csv")
st_reg
BLC_reg <- BLC %>%
  full_join(st_reg, by = "State") 

head(BLC_reg)

south <- filter(BLC_reg, Region == "South")

northeast <- BLC_reg %>%
  filter(Region == "Northeast")

midwest <- BLC_reg %>%
  filter(Region == "Midwest")

west <- BLC_reg %>%
  filter(Region == "West")

ggplot(data = south) + 
  geom_smooth(mapping = aes(x = Year, y = PercentTotalWinterLoss, se = FALSE)) +
  facet_wrap(~State)



#By Division

#South Atlantic 
southatlantic <- BLC_reg %>%
  filter(Division == "South Atlantic")

ggplot(data = southatlantic) + 
  geom_point(mapping = aes(x = Year, y = PercentTotalWinterLoss, se = FALSE)) +
  facet_wrap(~State)
                
c <- ggplot(data = southatlantic, mapping = aes(Year,PercentTotalWinterLoss)) 

#Figure out how to add color
#Figure out best graphical display 

c + geom_col(
  mapping = NULL,
  data = NULL,
  position = "stack",
  width = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
  ) + facet_wrap(~State
  ) +  scale_fill_brewer(palette = "Blues") 




#Average each division per year (average of all of the states)
#Plot on one graph

div_avg <- BLC_reg %>%
  group_by(Division, Year) %>%
  summarize(average = mean(PercentTotalWinterLoss), Year = max(Year))

div_avg

#This plot shows a general trend of Percent Total Winter Loss decreasing between all Divisons between 2007 and 2011. Percent Total Winter Loss Tends to increase across all regions post 2012.
ggplot(data = div_avg) + 
  geom_smooth(mapping = aes(x = Year, y = average, color = Division), se = FALSE)

ggplot(data = div_avg) + 
  geom_smooth(mapping = aes(x = Year, y = average), se = FALSE) +
  facet_wrap(~Division)

#Lindsay: Pull yearly averages 

#Trying to plot derivative of Percent Total Winter Loss 
#Derivative will show us between which years PTWL changed the most 
data <- d[order(d$X), ]
data$derivative = c(diff(d$fitted_values) / diff(d$X), NA)

#Ask Prof Wall about this if we want to include it
dd <- div_avg %>%
  summarize(deriv = c(NA, diff(average)/diff(Year)), Division = Division, Year = Year)

ggplot(data = dd) + 
  geom_smooth(mapping = aes(x = Year, y = deriv), se = FALSE) +
  facet_wrap(~Division)

  



  

