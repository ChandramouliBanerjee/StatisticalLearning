set.seed(1)
library("ggplot2")

#Generating Random Data for x and epsilon
x = rnorm(100, 0 , 1)
eps0 = rnorm(100, 0 , 0.25)

#Creating Data Frame (because it is cool)
thirteen = data.frame(x, eps0)

#Generating y variable
thirteen$y = -1 + 0.5*x + eps0

#Scatterplot
p1 = ggplot(data = thirteen, aes(x = thirteen$x, y = thirteen$y, color)) + geom_point(shape = 1, color = "darkgreen") + labs(list(x =  "X", y = "Y")) + theme_bw()

#Regression 1
model1 = lm(thirteen$y~thirteen$x)
summary(model1)

#Regression Line
p2 = p1 + geom_smooth(method = lm)

#Regression 2: Polynomial
model2 = lm(thirteen$y ~ poly(thirteen$x, 2, raw = TRUE))
summary(model2)

#Plotting
p3 = p1 + geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "red")

#Decreasing Noise
thirteen$eps1 =  rnorm(100, 0 , 0.05)
thirteen$y1 = -1 + 0.5*thirteen$x + thirteen$eps1
model3 = lm(thirteen$y1~thirteen$x)
summary(model3)
p4 = ggplot(data = thirteen, aes(x = thirteen$x, y = thirteen$y1, color)) + geom_point(shape = 1, color = "darkorange3") + labs(list(x =  "X", y = "Y1")) + theme_bw() + geom_smooth(method = lm)

#Increasing Noise
thirteen$eps2 =  rnorm(100, 0 , 1.5)
thirteen$y2 = -1 + 0.5*thirteen$x + thirteen$eps2
model4 = lm(thirteen$y2~thirteen$x)
summary(model4)
p5 = ggplot(data = thirteen, aes(x = thirteen$x, y = thirteen$y2, color)) + geom_point(shape = 1, color = "darkorchid2") + labs(list(x =  "X", y = "Y2")) + theme_bw() + geom_smooth(method = lm)