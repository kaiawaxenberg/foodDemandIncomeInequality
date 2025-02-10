library(ggplot2)
library(dplyr)
library(tidyr)

############# Plot parameter distributions from monte carlo ##############

#load parameter distribution data
betaParams=read.csv("Data/inputs/ssp_params.csv")%>% filter(dist=="beta")

#Functions to generate a beta distribution with rescaling
estimate_beta_params <- function(x05, xmode, x95, xmin, xmax) {
  # Rescale inputs to [0, 1]
  x05_scaled <- (x05 - xmin) / (xmax - xmin)
  xmode_scaled <- (xmode - xmin) / (xmax - xmin)
  x95_scaled <- (x95 - xmin) / (xmax - xmin)
  
  # Approximation for mean and variance
  mean <- xmode_scaled
  variance <- ((x95_scaled - x05_scaled) / 4)^2
  
  # Estimate beta shape parameters
  alpha <- ((1 - mean) / variance - 1 / mean) * mean^2
  beta <- alpha * (1 / mean - 1)
  
  if (alpha <= 0 || beta <= 0 || is.nan(alpha) || is.nan(beta)) {
    stop("Invalid beta parameters calculated. Check your inputs.")
  }
  
  list(alpha = alpha, beta = beta)
}
generate_beta_distribution <- function(dist, xmin, xmax, x05, xmode, x95, n = 1000) {
  params <- estimate_beta_params(x05, xmode, x95, xmin, xmax)
# Generate x values in [0, 1] for beta distribution
  x_scaled <- seq(0, 1, length.out = n)
  density <- dbeta(x_scaled, shape1 = params$alpha, shape2 = params$beta)
  
  # Rescale x values back to [xmin, xmax]
  x_rescaled <- x_scaled * (xmax - xmin) + xmin
  
  data.frame(x = x_rescaled, density = density)
}

# Generate distribution data for each parameter with beta distributions
plot_data <- betaParams %>% rowwise() %>%
  mutate(
    dist_data = list(
      generate_beta_distribution(dist, xmin, xmax, x05, xmode, x95)
    )
  ) %>% unnest(dist_data) %>% ungroup()

#Plot parameter distributions by SSP
ggplot(plot_data, aes(x = x, y = density, color = Ensemble)) +
  geom_line() +
  scale_color_brewer(palette="Set1")+
  facet_wrap(~ pname, ncol=3, scales = "free_x") +
  labs(
    x = "Parameter Value",
    y = "Density",
    color = "Scenario"
  ) +
  theme_minimal()

