##Load libraries
library(ggplot2)
library(gapminder)
library(dplyr)
##filter data out for kuwait
gapminder_filtered <- gapminder %>%
filter(country != 'Kuwait')

##First plot type
ggplot(gapminder_filtered, aes(x=lifeExp, y=gdpPercap, color = continent, size=pop/100000))+
  geom_point() + 
  facet_wrap(~year) + 
  scale_y_continuous(trans = "sqrt") + 
  theme_bw() + 
  labs("Continent", size="Pop(100k)")

##second plot preperation
#technical format, pipe symbol %>% avoids issues group_by(gapminder_filtered, country, year, .add = FALSE)
gapminder_continent <- gapminder_filtered %>% group_by(continent, year) %>% 
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop), 
            pop = sum(as.numeric(pop))) %>%
  left_join(gapminder %>%
              group_by(continent, year) %>%
              summarize(lifeExp = mean(lifeExp)),
            by = c("continent", "year"))

#Plot 2
ggplot(gapminder, aes(x = year, y = gdpPercap, color = continent)) +
  geom_line(aes(group = country), size = 0.5) +  # Line for each country over time
  geom_line(data = gapminder_continent, 
            aes(x = year, y = gdpPercapweighted, group = continent), 
            color = "black", size = 1) +
  geom_point(data = gapminder_continent, 
             aes(x = year, y = gdpPercapweighted, group = continent, size = pop/1000000), 
             color = "black") +
  facet_wrap(~continent) +  # Facet by continent
  theme_bw() +
  labs(x = "Year", 
       y = "GDP per Capita", 
       color = "Continent", 
       title = "GDP per Capita Over Time with Continent Averages",
       size="Pop(100k)")

