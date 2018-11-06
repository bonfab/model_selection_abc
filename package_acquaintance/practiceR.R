library(gapminder)
data("gapminder")

summary(gapminder)

hist(gapminder$lifeExp)
attach(gapminder)

hist(log(pop))

boxplot(lifeExp ~ continent)

plot(lifeExp ~ log(gdpPercap))

library(dplyr)

df1 <- gapminder %>%
  select(country, lifeExp) %>%
  filter(country == "South Africa" |
           country == "Ireland")


t.test(data = df1, lifeExp ~ country)

library(ggplot2)

gapminder %>%
  filter(gdpPercap < 50000) %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, col=continent, size= pop)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm) +
  facet_wrap(~ continent)


summary(lm(lifeExp ~ gdpPercap + pop))

dnorm(0)
pnorm(0)

pbinom(4, 9, 0.5)

summary(lm(lifeExp ~ gdpPercap))


makeKid <- function(good = TRUE, grade = 1){
  
  kid <- list(
              goodBehaviour = good,
              schoolGrade = grade
  )
  
  class(kid) <- append(class(kid), "kid")
  
  return(kid)
}

newGrade <- function(x, y){
  UseMethod("newGrade", x)
}

newGrade.kid <- function(x, y){
  x$schoolGrade = y
  return(x)
}

tom <- makeKid(FALSE, 4)

print(tom)

tom <- newGrade(tom, 2)

print(tom)




