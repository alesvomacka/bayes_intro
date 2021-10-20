# Packages ----------------------------------------------------------------

library(tidyverse)
library(rstanarm)
library(broom.mixed)
library(ggeffects)
library(bayesplot)

# Data --------------------------------------------------------------------

evals <- read.csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Beauty/data/beauty.csv")

options(mc.cores = parallel::detectCores())

# Variable description ----------------------------------------------------

# eval: evaluation rating, scale 1-5 (5 is best).
# beauty: beauty rating of teachers, z scores.
# female: indicator variable whether teacher is a woman.
# age: age of the teacher.
# minority: indicator variable whether teacher belongs to an ethnic minority.
# lower: indicator varibale whether teachers lectures undergrad or grad course.
# course_id: ID of the cource tought.

# Research questions ------------------------------------------------------

# Is there a relationship between evaluations and beauty?
# Does the relationship differs between men and women?

# Data preparation --------------------------------------------------------

# center numeric variables, especially predictors

evals$age_c    <- evals$age - mean(evals$age)
evals$beauty_c <- evals$beauty - mean(evals$beauty)

# Setting priors ----------------------------------------------------------

# intercept: N(3.5, sd = 0.5)
# beauty: N(0.05, sd = 0.01)
# female: N(-0.5, sd = 1)
# age: N(0, sd = 0.1)
# residuals: Expon(1)

# Sampling from priors ----------------------------------------------------

set.seed(1234)
model_prior_only <- stan_glm(eval ~ beauty_c + female + age_c,
                             prior_intercept = normal(location = 3.5, scale = 0.5),
                             prior = normal(location = c(0.2, -0.5, 0), scale = c(0.1, 1, 0.1)),
                             prior_aux = rstanarm::exponential(rate = 1),
                             prior_PD = TRUE,
                             data = evals)

mcmc_hist(model_prior_only) # distributions of our priors

# Export posterior samples
coeffs_prior <- as_tibble(model_prior_only)
coeffs_prior <- slice_sample(coeffs_prior, n = 500)
names(coeffs_prior)[1] <- "Intercept"

# plot posterior samples
ggplot(evals, aes(x = beauty, y = eval)) +
  geom_blank() +
  geom_abline(aes(intercept = Intercept, slope = beauty), alpha = 0.1,
              data = coeffs_prior) +
  scale_y_continuous(limits = c(1,5))

# Are we ok with priors or do we try others?

# Model with informative priors -------------------------------------------

model_informative <- stan_glm(eval ~ beauty_c + female + age_c,
                              prior_intercept = normal(location = 3.5, scale = 0.5),
                              prior = normal(location = c(0.2, -0.5, 0), scale = c(0.1, 0.1, 0.1)),
                              prior_aux = rstanarm::exponential(rate = 1),
                              data = evals)

# MCMC diagnostics
mcmc_trace(model_informative) # did the chains converged
rhat(model_informative) # Quick check if model converged (shoud be close to 1)
neff_ratio(model_informative) # were there enough simulations? The ratio should be at least 0.1, the higher the better
mcmc_parcoord(model_informative) # There should be no divergent transitions (red lines)
mcmc_pairs(model_informative)

# Model diagnstics --------------------------------------------------------

# Check linearity and residual distributions

launch_shinystan(model_informative) #best for quick check
plot(loo(model_informative)) # influential observations

# Interpreting results ----------------------------------------------------

summary(model_informative) # regression coefficients
posterior_interval(model_informative, prob = 0.95) # credibility intervals
mean(bayes_R2(model_informative)) # R squared, adjusted!
loo(model_informative) # Equivalent of AIC

marginals_informative <- ggpredict(model_informative) # compute marginals effects
plot(marginals_informative$beauty_c, add.data = TRUE) # plot marginal effect for beauty

# Model with interaction --------------------------------------------------

model_interaction <- stan_glm(eval ~ beauty_c + female + beauty_c:female + age_c,
                              prior_intercept = normal(location = 3.5, scale = 0.5),
                              prior = normal(location = c(0.2, -0.5, 0, 0), scale = c(0.1, 1, 1, 0.1)),
                              prior_aux = rstanarm::exponential(rate = 1),
                              data = evals)

posterior_interval(model_interaction, probs = 0.95)
mean(bayes_R2(model_interaction)) 

loo_compare(loo(model_informative), loo(model_interaction)) # compare models based on LOO.
