install.packages("gapminder")

library(ggplot2)
library(gapminder)
library(dplyr)

gapminder
str(gapminder)

NoK_gapminder <- gapminder %>%
  filter(country != "Kuwait")

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
