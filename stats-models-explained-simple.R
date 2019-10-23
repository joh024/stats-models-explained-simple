## Open png output
png("stats-models-explained-simple.png", width = 1100, height = 1000, pointsize = 24)
opar = par(mfrow = c(2, 2))

## Plot function
do_plot = function(main = "MAIN", pch = 1){
  pch = rep(pch, length = 2)
  with(dat, {
    plot(0:11, 0:11, type = "n", main = main, xlab = "x", ylab = "y")
    points(x[group == "A"], y[group == "A"], pch = pch[1])
    points(x[group == "B"], y[group == "B"], pch = pch[2])
  })
}

## Generate Data
set.seed(20191023)
dat = data.frame(
  group = rep(c("A", "B"), each = 10),
  x = c(1:10, 1:10),
  y = c(
    0 + 1:10 * 1 + rnorm(10),
    3 + 1:10 * 0.5 + rnorm(10, 0, 0.5)
  )
)
do_plot("1. We have some data")

## Simple Model
fit1 = lm(y ~ x, data = dat)
do_plot("2. Use a model to understand it")
abline(coef = fit1$coefficients, lwd = 2)

## Better Model
fit2 = lm(y ~ x * group, data = dat)
do_plot("3. Use features to do better", pch = 1:2)
abline(coef = fit2$coefficients[1:2], col = "#CC0000", lwd = 2)
abline(coef = fit2$coefficients[1:2] + fit2$coefficients[3:4], col = "#0000CC", lwd = 2)

## Overfit Model
fit3 = lm(y ~ poly(x, 9) * group, data = dat)
do_plot("4. But don't go too crazy", pch = 1:2)
pred_x = seq(0, 11, length = 101)
lines(pred_x, predict(fit3, data.frame(x = pred_x, group = "A")), col = "#CC0000", lwd = 2)
lines(pred_x, predict(fit3, data.frame(x = pred_x, group = "B")), col = "#0000CC", lwd = 2)

## Close output
par(opar)
dev.off()
