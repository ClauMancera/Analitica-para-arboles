library(gapminder) # esta libreria trae una tabla con datos mundiales(GDP, POPULATION)
library(dplyr)


# PIPES

gapminder %>% # este signo es para hacer pipes, lo que hace es filtar, ordenar, coambiar etc la tabla con comando que se llaman verbs
  filter(year==1957)
gapminder %>%
  filter(country== "Austria")
gapminder %>%
  filter(country== "China")
gapminder %>%
  arrange(pop)
gapminder %>%
  arrange(desc(pop)) # este verb sortea los datos de forma ascendente por default

gapminder %>%  #se pueden hacer pipes anidados, es decir varios comandos en la misma tabla
  filter(year==1957) %>%
  arrange(desc(pop))

gapminder %>%
  mutate(pop= pop/1000000) # este verb remplaza la columna con lo que tu le asignes

gapminder %>%
  mutate(gdp= gdpPercap*pop) # no solo reemplaza tambien puede agregar una nueva columna si usas un nombre nuevo

gapminder %>%
  mutate(lifeExp= lifeExp*12) 

gapminder %>%
  mutate(lifeExpMonths= lifeExp*12)

lifeorder <- gapminder %>%  # puedes asignar un pipe a una variable
  filter(year==2007) %>%
  mutate(lifeExp= lifeExp*12) %>%
  arrange(desc(lifeExp))


# GRAFICAS EN GGPLOT2

library(ggplot2)

gapminder_2007 <- gapminder %>% 
  filter(year == 2007)
ggplot(gapminder_2007, aes(x= gdpPercap, y = lifeExp, color= continent, size= pop)) +
  geom_point() + scale_x_log10() + facet_wrap(~ continent)

# con + vas agregando detalles de formatio de la grafica 
#facet_wrap hace un subplot (grafico) independiente para cada uno
#adentro de aes asignas las variables x y y tambien con color y pop puedes poner el tama?o de los puntos basadas en una variable

#Ejercicio:
#Scatter plot comparing GDP/Capita and LifeExp with color representing continent
#and size representing population faceted by year

ggplot(gapminder, aes(x= gdpPercap, y = lifeExp, color= continent, size= pop)) +
  geom_point() + scale_x_log10() + facet_wrap(~ year)


#SUMMARIZED

gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp =mean(lifeExp), totalPop =sum(as.numeric(pop)))

# Summarize to find the median life expectancy
gapminder %>%
  summarize(medianLifeExp =median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp =median(lifeExp))


# Filter for 1957 then summarize the median life xpectancy and the maximum GDP per capita
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp =median(lifeExp), maxGDP =max(gdpPercap))


#GROUP BY
gapminder %>%
  group_by(year, continent) %>%
  summarize(meanLifeExp =mean(lifeExp), totalPop =sum(as.numeric(pop)))



#Ejercicios
# Find median life expectancy and maximum GDP per capita in each year

gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp =median(lifeExp), maxGDP =max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent in 1957

gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(medianLifeExp =median(lifeExp), maxGDP =max(gdpPercap))

#HISTOGRAMA

ggplot(gapminder_2007, aes(x= lifeExp)) +
  geom_histogram(binwidth = 2)


# by continent
by_continent <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp))
ggplot(by_continent, aes(x = continent, y = meanLifeExp, fill = continent)) +
  geom_col()


#


ggplot(gapminder, aes(x= gdpPercap, y = lifeExp, color= continent, size= pop)) +
  geom_point() + scale_x_log10()