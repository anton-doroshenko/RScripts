anscombe
str(anscombe)
summary(anscombe)
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

summary(lm(formula = anscombe$y1 ~ anscombe$x1))
summary(lm(formula = anscombe$y2 ~ anscombe$x2))

lm(formula = anscombe$y3 ~ anscombe$x3)
lm(formula = anscombe$y4 ~ anscombe$x4)

lm1 <- lm(data = anscombe, y1 ~ x1)
lm1$fitted.values
anscombe$y1 - lm1$fitted.values
lm1$residuals

library(ggplot2)

anscombe$residuals_lm1 <- lm1$residuals
ggplot(anscombe, aes(x = residuals_lm1)) + geom_dotplot(fill = "orange")

lm2 <- lm(data = anscombe, y2 ~ x2)
lm3 <- lm(data = anscombe, y3 ~ x3)
lm4 <- lm(data = anscombe, y4 ~ x4)

anscombe$residuals_lm2 <- lm2$residuals
anscombe$residuals_lm3 <- lm3$residuals
anscombe$residuals_lm4 <- lm4$residuals

ggplot(anscombe, aes(x = residuals_lm2)) + geom_dotplot(fill = "orange")
ggplot(anscombe, aes(x = residuals_lm3)) + geom_dotplot(fill = "orange")
ggplot(anscombe, aes(x = residuals_lm4)) + geom_dotplot(fill = "orange")

qqnorm(lm1$residuals, col="orange", pch = 20)
qqline(lm1$residuals, col = "blue")

qqnorm(lm2$residuals, col="orange", pch = 20)
qqline(lm2$residuals, col = "blue")

qqnorm(lm3$residuals, col = "orange", pch = 20)
qqline(lm3$residuals, col = "blue")

qqnorm(lm4$residuals, col = "orange", pch = 20)
qqline(lm4$residuals, col = "blue")

anscombe$fitted_lm1 <- lm1$fitted.values
anscombe$fitted_lm2 <- lm2$fitted.values
anscombe$fitted_lm3 <- lm3$fitted.values
anscombe$fitted_lm4 <- lm4$fitted.values

ggplot(data = anscombe, aes(x = fitted_lm1, y = residuals_lm1)) + geom_point(col = "orange")
ggplot(data = anscombe, aes(x = fitted_lm2, y = residuals_lm2)) + geom_point(col = "orange")
ggplot(data = anscombe, aes(x = fitted_lm3, y = residuals_lm3)) + geom_point(col = "orange")
ggplot(data = anscombe, aes(x = fitted_lm4, y = residuals_lm4)) + geom_point(col = "orange")

str(diamonds)
ggplot(diamonds, aes(x = carat, y = price)) + geom_point(col = "lightblue")
cor(diamonds$carat, diamonds$price)
cor(diamonds$price, diamonds$carat)

ggplot(diamonds, aes(x = carat, y = price, col = cut)) + geom_point()
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(col = "lightblue") +
  facet_wrap(~cut)+ geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~cut)

library(dplyr)

lin.diamond.ideal <- lm(data = diamonds%>%filter(cut =="Ideal"), price ~ carat)

lin.diamond.fair <- lm(data = diamonds%>%filter(cut == "Fair"), price ~ carat)

lin.diamond.ideal
lin.diamond.fair
