# Machining Learning
# Aula 4
# Prof. Neylson Crepalde
# Lucas Cesar Fernandes Ferreira
# -------------------------------- #

# Importando um banco de dados embutido no R
data('mtcars')
head(mtcars)

# Vamos tentar explicar o consumo (mpg) a partir do hp e do tipo am
reg1 <- lm(mpg ~ hp + am, data = mtcars)
summary(reg1)

# Regressão com termo interativo
reg2 <- lm(mpg ~ hp * am, data = mtcars)
summary(reg2)

# Usando o ISLR
install.packages('ISLR')
library(ISLR)
library(readr)

adv <- read_csv('http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv')
head(adv)

adv <- select(adv, -X1) # ou adv = adv %>% select(-X1)

reg3 <- lm(sales ~ radio + newspaper, data = adv)
summary(reg3)

reg4 <- lm(sales ~ radio * newspaper, data = adv)
summary(reg4)

# Regressão polinomial
# Vamos investigar a relção entre mpg e hp
plot(mtcars$hp, mtcars$mpg)

# Testando o modelo mpg por hp
reg5 <- lm(mpg ~ hp, data = mtcars)
summary(reg5)

t = lm(mpg ~ hp + I(hp^2), data = mtcars)
summary(t)

install.packages('ggplot2')
library(ggplot2)

ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y~x+I(x^2))
plot(t)





