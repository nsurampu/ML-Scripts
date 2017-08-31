library(ggplot2)
library(gridExtra)
library(GGally) 
library(scales) #required for trans_new()
library(memisc) #required for mtable()
library(MASS) #required for memisc
library(lattice) #required for memisc

cube_root_trans = function() trans_new('cuberoot', 
                                       transform = function(x) x ^ (1/3),
                                       inverse = function(x) x ^ 3)

p1 <- ggplot(aes(x = carat, y = price), data = diamonds)+
    geom_jitter(alpha = 1/2, size = 3/4, aes(color = diamonds$color))+
    geom_smooth(method = 'lm')+
    scale_x_continuous(trans = cube_root_trans(), limits = c(0.2, 3), breaks = seq(0, 4, 0.1))+
    scale_y_continuous(trans = cube_root_trans(), limits = c(3, 16000), breaks = seq(0, 15000, 3000))+
    ggtitle("log10(price) vs cuberoot(carat)")

m1 <- lm(I(log10(price)) ~ I(carat ^ (1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + cut)
m4 <- update(m3, ~ . + color)
m5 <- update(m4, ~ . + clarity)

mtable(m1, m2, m3, m4, m5)

newDiamonds = data.frame(carat = 1.00, color = 'I', cut = 'Fair', clarity = 'VVS1')
predict(m5, newdata = newDiamonds, interval = 'prediction', level = 0.95) #model prediction

dat = data.frame(m4$model, m4$residuals)

with(dat, sd(m4.residuals))

with(subset(dat, carat > .9 & carat < 1.1), sd(m4.residuals))

dat$resid <- as.numeric(dat$m4.residuals)

p2 <- ggplot(aes(y = resid, x = round(carat, 2)), data = dat) +
  geom_line(stat = "summary", fun.y = sd)

grid.arrange(p1, p2, nrow = 2)

predict_values <- predict(m1, diamonds)

summary(predict_values)