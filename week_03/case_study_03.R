install.packages("gapminder") # install dataset "gapminder"

library(ggplot2) # load ggplot
library(gapminder) # load gapminder dataset
library(dplyr) #load the dplyr to use summarize

gapminder
str(gapminder) # check the structure of the gapminder

# remove the "Kuwait" from the dataset
NoK_gapminder <- gapminder %>%
  filter(country != "Kuwait")

# plot a scatter point of life expetation and gdp with theme_bw, and other co
ggplot(NoK_gapminder, aes(x = lifeExp, y = gdpPercap, color = continent, size = pop/100000)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(trans = "sqrt") +
  facet_wrap(~year,nrow=1) +
  labs(title = "Relationship between GDP and life expetation") + 
  xlab("Life Expection (Year)") +
  ylab("GDP (million $)")

gapminder_weighted <- NoK_gapminder %>%
  group_by(continent, year) %>%
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop), pop = sum(as.numeric(pop)))

ggplot() +
  geom_line(data = NoK_gapminder, aes(x = year, y = gdpPercap, color = continent, group = country)) +
  geom_point(data = NoK_gapminder, aes(x = year, y = gdpPercap, color = continent, group = country)) +
  geom_line(data = gapminder_weighted, aes(x = year, y = gdpPercapweighted))+
  geom_point(data = gapminder_weighted, aes(x = year, y = gdpPercapweighted, size = pop/100000)) +
  facet_wrap(~continent,nrow=1) +
  theme_bw() +
  labs()
