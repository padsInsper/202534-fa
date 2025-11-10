###### SCRIPT AULA 2 - VOLATILIDADE ####################
# Financial Analytics - 2024
# Paloma Vaissman Uribe
########################################################

## Usando o pacote yfR para pegar ações

library(yfR)
data = yf_get(
  c('PETR4.SA', 'VALE3.SA', '^BVSP'),
  first_date = '2007-01-01',
  last_date = Sys.Date(),
  bench_ticker = "^BVSP",
  type_return = "log",
  freq_data = "daily",
  do_complete_data = TRUE
)


head(data)

## gráficos dos preços usando ggplot2

library(ggplot2)
data <- data |>
  dplyr::filter(volume != 0)
sub <- data |>
  subset(ticker != '^BVSP')
ibov <- data |>
  subset(ticker == '^BVSP')

p1 <- ggplot(sub, aes(x = ref_date, y = price_close, group = ticker)) +
  geom_line(aes(color = ticker)) +
  geom_point(aes(color = ticker)) +
  theme(legend.position = "top") +
  xlab('Data') +
  ylab('Reais')
p2 <- ggplot(ibov, aes(x = ref_date, y = price_close)) +
  geom_line() +
  geom_point() +
  xlab('Data') +
  ylab('Pontos - IBOVESPA')

library(patchwork)
p1 / p2

## gráficos dos log-retornos usando ggplot2

p <- ggplot(
  data,
  aes(
    x = ref_date,
    y = ret_closing_prices,
    group = ticker
  )
) +
  geom_line(aes(color = ticker)) +
  geom_point(aes(color = ticker))
p


## gráficos dos log-retornos ao quadrado usando ggplot2

library(xts)
library(lubridate)
library(dplyr)
selection <- data |>
  dplyr::select(ref_date, ticker, ret_closing_prices) |>
  mutate(ret2 = ret_closing_prices^2)
p <- ggplot(selection, aes(x = ref_date, y = ret2, group = ticker)) +
  geom_line(aes(color = ticker)) +
  geom_point(aes(color = ticker)) +
  theme(legend.position = "top") +
  xlab('Data') +
  ylab('Retornos ao quadrado')
p

# Retorno da PETROBRAS usando xts

petro <- data %>%
  subset(ticker == 'PETR4.SA') %>%
  select(ref_date, ret_closing_prices)
ret <- xts(petro[, -1], order.by = ymd(petro$ref_date))[-1, ]
ret2 = ret**2
plot(ret2)


## Autocorrelação dos retornos

par(mfrow = c(1, 2))
acf(ret, 60, na.action = na.pass)
acf(ret2, 60, na.action = na.pass)

## Normalidade? Gráficos qqplot e histograma

par(mfrow = c(1, 2))
h <- hist(ret, breaks = 20, col = "red", xlab = "", main = "Histogram")
xfit <- seq(min(ret), max(ret), length = 40)
yfit <- dnorm(xfit, mean = mean(ret), sd = sd(ret))
yfit <- yfit * diff(h$mids[1:2]) * length(ret)
lines(xfit, yfit, col = "blue", lwd = 2)

qqnorm(ret, pch = 1, frame = FALSE)
qqline(ret, col = "steelblue", lwd = 2)

## Normalidade: teste Shapiro-Wilk
shapiro.test(as.vector(ret))

## Teste para verificar efeitos ARCH: duas opções

# 1. Ljung-Box sobre os retornos quadráticos
Box.test(ret, type = "Ljung-Box")
Box.test(ret2, type = "Ljung-Box")

# 2. Teste LM
library(FinTS)
ArchTest(ret)

##################### ARCH(12) ###########################

## Modelo ARCH(12) com erros normais
#rugarch

library(fGarch)
fit.arch = garchFit(~ garch(12, 0), data = ret, trace = F)
fit.arch

## gráfico dos resíduos: verificar que é ruído branco
resi = residuals(fit.arch, standardize = T)
par(mfrow = c(1, 2))
ts.plot(resi)
acf(resi)

## teste Ljung Box para verificar que os resíduos são RB
Box.test(resi, lag = 12, type = "Ljung")

## teste Ljung Box para verificar que os resíduos ao quadrado são RB
Box.test(resi^2, lag = 12, type = "Ljung")

## ARCH com erros t-Student
fit.arch2 = garchFit(~ garch(12, 0), data = ret, trace = F, cond.dist = "std")

## Comparando os dois em termos de critérios de informação
fit.arch@fit$ics
fit.arch2@fit$ics

## gráficos das volatilidades

par(mfrow = c(2, 1))
plot(ret)
sigma <- xts(cbind(fit.arch@sigma.t, fit.arch2@sigma.t), order.by = index(ret))
colnames(sigma) <- c('erros normais', 'erros t')
plot(sigma, auto.legend = T, legend.loc = "top", main = '')


##################### GARCH(1,1) ###########################

## Modelo GARCH(1,1) com erros normais

library(fGarch)
fit.garch = garchFit(~ garch(1, 1), data = ret, trace = F, include.mean = TRUE)
fit.garch@fit$matcoef

## Modelo GARCH(1,1) com erros t-Student

fit.garch2 = garchFit(
  ~ garch(1, 1),
  data = ret,
  cond.dist = 'std',
  trace = F,
  include.mean = TRUE
)
fit.garch2@fit$matcoef

## Comparando os dois em termos de critérios de informação
fit.garch@fit$ics
fit.garch2@fit$ics # melhor modelo!!!

## gráficos das volatilidades

par(mfrow = c(2, 1))
plot(ret)
sigma <- xts(
  cbind(fit.garch@sigma.t, fit.garch2@sigma.t),
  order.by = index(ret)
)
colnames(sigma) <- c('erros normais', 'erros t')
plot(sigma, auto.legend = T, legend.loc = "top", main = '')

########### PACOTE RUGARCH ##############################################
# Gerando um backtest do modelo escolhido

library(rugarch)
spec <- ugarchspec(
  mean.model = list(armaOrder = c(0, 0)),
  variance.model = list(garchOrder = c(1, 1)),
  distribution.model = "std"
)
garchroll <- ugarchroll(
  spec,
  data = ret,
  n.start = 1000,
  refit.window = "expanding",
  refit.every = 100
)

preds <- as.data.frame(garchroll)

# Prediction error for the mean
e <- preds$Realized - preds$Mu

# Prediction error for the variance
d <- e^2 - preds$Sigma^2

# Mean of prediction error
mean(d^2)

##################### SV-AR(1) ###########################

# Modelo SV-AR(1)

library(stochvol)
draws = svsample(ret, draws = 30000)
stdevs = t(apply(exp(draws$latent[[1]] / 2), 2, quantile, c(0.025, 0.5, 0.975)))

# gráficos das volatilidades

par(mfrow = c(2, 1))
ylim = range(min(stdevs), max(stdevs))
plot(ret, ylab = "retornos")
ts.plot(stdevs, col = 1:3)

# GRÁFICO COMPARATIVO
par(mfrow = c(1, 1))
ts.plot(fit.garch@sigma.t, col = 4, ylab = 'volatilidade', xlab = '')
lines(fit.garch2@sigma.t, col = 2)
lines(stdevs[, 2], col = 3)
legend(
  "top",
  col = c(2, 4, 3),
  lty = 1,
  legend = c('erros normais', 'erros t-Student', 'SV')
)
