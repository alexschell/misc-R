sir_model_forecast = function(i_0 = 10, r_0 = 0, pop = 1e7,
                              par_r0 = 2.5, par_gamma = 1/14,
                              N = 300, period_0 = 1,
                              sigma = 0.3) {
  period = period_0:N
  susceptible = infectious = recovered = infections = recoveries = rep(NA_real_, length(period))
  par_beta = par_r0 * par_gamma
  infectious[1] = i_0
  recovered[1] = r_0
  susceptible[1] = pop - i_0 - r_0
  
  for (t in 2:length(period)) {
    infections[t] = 
      susceptible[t-1] * infectious[t-1] / pop * par_beta * exp(rnorm(1, 0, sigma) - (sigma^2)/2)
    recoveries[t] = 
      infectious[t-1] * par_gamma * exp(rnorm(1, 0, sigma) - (sigma^2)/2)
    infectious[t] = infectious[t-1] + infections[t] - recoveries[t]
    susceptible[t] = susceptible[t-1] - infections[t]
    recovered[t] = recovered[t-1] + recoveries[t]
  }
  
  data.frame(period, susceptible, infectious, recovered, infections, recoveries)
}

sir_model_estimate = function(susceptible, infectious, recovered, infections, recoveries) {
  N = length(susceptible)
  pop = susceptible[1] + infectious[1] + recovered[1]
  
  beta_hat = sum(infections[-1]) / sum(susceptible[-N] * infectious[-N] / pop)
  gamma_hat = sum(recoveries[-1]) / sum(infectious[-N])
  
  list(beta_hat, gamma_hat)
}

set.seed(393)

N = 300
r0 = 2.5
gamma = 1/14
pop = 1e6
df_truth = sir_model_forecast(pop = pop, par_r0 = r0, par_gamma = gamma, N = N)

forecast_ls = list()
for (i in 10:N) {
  
  estimates =
    with(
      df_truth[1:(i-1),],
      sir_model_estimate(susceptible, infectious, recovered, infections, recoveries)
    )

  forecast = 
    with(
      df_truth[i-1,],
      sir_model_forecast(i_0 = infectious, r_0 = recovered, pop = pop,
                         par_r0 = estimates[[1]]/estimates[[2]], par_gamma = estimates[[2]],
                         N = N, period_0 = i-1, sigma = 0)
    )
  forecast$period_start = i
  forecast$r0_estimate = estimates[[1]]/estimates[[2]]
  forecast$gamma_estimate = estimates[[2]]
  
  forecast_ls[[length(forecast_ls) + 1]] = forecast
    
}

library(dplyr)
library(ggplot2)
library(gridExtra)

df_forecasts = bind_rows(forecast_ls)
df_truth$period_start = 0L

tmp = filter(df_forecasts, period_start %in% c(10, 20, 40, 60))

p1 = 
  ggplot(df_truth,
         aes(x=period, y=(infectious+recovered)/pop*100)) +
  geom_line(size=1.5, alpha=0.4) +
  geom_line(data=tmp, aes(col=factor(period_start))) +
  labs(col="Forecast start", x="Period", y="Infectious + Recovered (%)") +
  ggtitle("Infectious + Recovered (% of Population)") +
  theme_bw()

p2 = 
  ggplot(df_truth,
         aes(x=period, y=infections)) +
  geom_line(size=1.5, alpha=0.4) +
  geom_line(data=tmp, aes(col=factor(period_start))) +
  labs(col="Forecast start", x="Period", y="Infections") +
  ggtitle("New Infections (Count)") +
  theme_bw()
  
grid.arrange(p1, p2, ncol=1)
  

