library(gapminder)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(forcats)

#scatter plot
gapminder_2007 <- gapminder %>%
  filter(year == 2007)
ggplot(gapminder_2007,aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))+
  geom_point()+scale_x_log10()+theme_minimal()+
  labs(x = "GDP per capita",
       y = "Life expectancy",
       title = "Life expectancy increases as GDP per capita increases",
       caption = "Data source: gapminder")

plot <- ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()+geom_smooth(method="lm")+
  labs(x = "GDP per capita",
       y = "Life expectancy",
       title = "Life expectancy increases as GDP per capita increases",
       caption = "Data source: gapminder")
ggMarginal(plot, type = "histogram", fill="transparent")
ggMarginal(plot, type = "boxplot", fill="transparent")

#histogram
gapminder_gdp2007 <- gapminder %>%
  filter(year == 2007, continent == "Americas") %>%
  mutate(country = fct_reorder(country,gdpPercap,last))
ggplot(gapminder_gdp2007, aes(x=country, y = gdpPercap))+
  geom_col(fill="skyblue", color="black")+
  labs(x = "Country",
       y = "GDP per capita",
       title = "GDP per capita in North America and South America, 2007",
       caption = "Data source: gapminder")+
  coord_flip()+theme_minimal()

#line plot
gapminder_pop <- gapminder %>%
  filter(country %in% c("United States","China"))
ggplot(gapminder_pop,aes(x = year, y = pop, color = country))+
  geom_line(lwd = 0.8)+theme_light()+
  labs(x = "Year",
       y = "Population",
       title = "Population in China and United States, 1953-2007",
       caption = "Data source: gapminder")

#facet plot
gapminder_gdp <- gapminder %>%
  group_by(year, continent) %>%
  summarize(avg_gdp = mean(gdpPercap))
ggplot(gapminder_gdp,aes(x = year, y = avg_gdp, color = continent))+
  geom_line(lwd = 0.8)+theme_light()+facet_wrap(~continent)+
  labs(x = "Year",
       y = "Average GDP per capita",
       title = "Average GDP per capita change in different continent",
       caption = "Data source: gapminder")+
  scale_x_continuous(breaks=c(1955,1970,1985,2000))

#path plot
gapminder_lifeexp <- gapminder %>%
  filter(year %in% c(1957,2007), continent == "Europe") %>%
  arrange(year) %>%
  mutate(country = fct_reorder(country,lifeExp,last))
ggplot(gapminder_lifeexp) +geom_path(aes(x = lifeExp, y = country),
            arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  geom_text(
    aes(x = lifeExp,
        y = country,
        label = round(lifeExp, 1),
        hjust = ifelse(year == 2007,-0.2,1.2)),
    size =3,
    family = "Bookman",
    color = "gray25")+
  scale_x_continuous(limits=c(45, 85))+
  labs(
    x = "Life expectancy",
    y = "Country",
    title = "People live longer in 2007 compared to 1957",
    subtitle = "Life expectancy in European countries",
    caption = "Data source: gapminder"
  )

#density plot
gapminder_1992 <- gapminder %>%
  filter(year == 1992)
ggplot(gapminder_1992, aes(lifeExp))+theme_classic()+
  geom_density(aes(fill=factor(continent)), alpha=0.8) + 
  labs(
    x="Life expectancy",
    title="Life expectancy group by continent, 1992", 
    caption="Data source: gapminder",
    fill="Continent")

#slope chart
gapminder_lifeexp2 <- gapminder %>%
  filter(year %in% c(1977,1987,1997,2007),
         country %in% c("Canada", "United States","Mexico","Haiti","El Salvador",
                        "Guatemala","Jamaica")) %>%
  mutate(lifeExp = round(lifeExp))
ylabs <- subset(gapminder_lifeexp2, year==head(year,1))$country
yvals <- subset(gapminder_lifeexp2, year==head(year,1))$lifeExp
ggplot(gapminder_lifeexp2, aes(x=as.factor(year),y=lifeExp)) +
  geom_line(aes(group=country),colour="grey80") +
  geom_point(colour="white",size=8) +
  geom_text(aes(label=lifeExp), size=3, color = "black") +
  scale_y_continuous(name="", breaks=yvals, labels=ylabs)+
  theme_classic()+
  labs(title="Life Expectancy of some North America countries change from 1977 to 2007") + 
  theme(axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))