library(tidyverse)
theme_set(theme_void() +
            theme(plot.background = element_rect(color = "black", fill = "#F5EDDE"),
                  plot.title = element_text(size = 7, margin = margin(l = 3, r = 3, t = 3, b = 3))))


# discrete ----------------------------------------------------------------
gbernoulli <- tibble(y = 0:10) |> 
  mutate(p = dbinom(y, 1, 0.3)) |> 
  ggplot() + geom_col(aes(y, p), fill = "#BE830E") +
  labs(title = "Y ~ Bernoulli(0.3)", x = "Y", y = "Probability") +
  scale_y_continuous(expand = c(0, 0))

gbinom <- gbernoulli %+% (tibble(y = 0:10) |> 
                            mutate(p = dbinom(y, 10, 0.7))) +
  labs(title = "Binomial")

gnbinom <- gbernoulli %+% (tibble(y = 0:10) |> 
                            mutate(p = dnbinom(y, 10, 0.7))) +
  labs(title = "Negative Binomial")

gpoisson <- gbernoulli %+% (tibble(y = 0:10) |> 
                              mutate(p = dpois(y, 2))) +
  labs(title = "Poisson") 

ghyper <- gbernoulli %+% (tibble(y = 0:10) |> 
                              mutate(p = dhyper(y, 8, 5, 5))) +
  labs(title = "Hypergeometric") 

ggeom <- gbernoulli %+% (tibble(y = 0:10) |> 
                            mutate(p = dgeom(y, 0.5))) +
  labs(title = "Geometric") 

gbetabinom <- gbernoulli %+% (tibble(y = 0:10) |> 
                           mutate(p = VGAM::dbetabinom(y, 5, 0.5))) +
  labs(title = "Beta-Binomial") 

# continuous --------------------------------------------------------------
gnormal <- gbernoulli %+% (tibble(y = seq(-4, 4, length.out = 1000)) |> 
  mutate(p = dnorm(y, 0, 1))) +
  labs(title = "Normal/Gaussian") 
ginvg <- gbernoulli %+% (tibble(y = seq(0, 7, length.out = 1000)) |> 
                             mutate(p = statmod::dinvgauss(y))) +
  labs(title = "Inverse Gaussian") 

glnormal <- gbernoulli %+% (tibble(y = seq(0, 7, length.out = 1000)) |> 
                             mutate(p = dlnorm(y, 0, 1))) +
  labs(title = "Log Normal") 

gbeta <- gbernoulli %+% (tibble(y = seq(0, 1, length.out = 1000)) |> 
                             mutate(p = dbeta(y, 2, 3))) +
  labs(title = "Beta") 

gchi <- gbernoulli %+% (tibble(y = seq(0, 10, length.out = 1000)) |> 
                           mutate(p = dchisq(y, 2))) +
  labs(title = "Chi-squared") 

gt <- gbernoulli %+% (tibble(y = seq(-4, 4, length.out = 1000)) |> 
                           mutate(p = dt(y, 3))) +
  labs(title = "t") 

gf <- gbernoulli %+% (tibble(y = seq(0, 20, length.out = 1000)) |> 
                        mutate(p = dt(y, 3, 5))) +
  labs(title = "F")

gunif <- gbernoulli %+% (tibble(y = seq(0, 1, length.out = 1000)) |> 
                            mutate(p = dunif(y, 0, 1))) +
  labs(title = "Uniform") 


ggamma <- gbernoulli %+% (tibble(y = seq(0, 10, length.out = 1000)) |> 
                             mutate(p = dgamma(y, 2))) +
  labs(title = "Gamma") 

gexp <- gbernoulli %+% (tibble(y = seq(0, 10, length.out = 1000)) |> 
                            mutate(p = dexp(y, 1/2))) +
  labs(title = "Exponential") 

gcauchy <- gbernoulli %+% (tibble(y = seq(-4, 4, length.out = 1000)) |> 
                          mutate(p = dcauchy(y))) +
  labs(title = "Cauchy") 

gskewnormal <- gbernoulli %+% (tibble(y = seq(-4, 8, length.out = 1000)) |> 
                             mutate(p = sn::dsn(y, 1, 2))) +
  labs(title = "Skew-Normal") 

ggsave(filename = "images/dist-skewnormal.png", gskewnormal, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-binomial.png", gbinom, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-neg-binomial.png", gnbinom, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-normal.png", gnormal, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-lnormal.png", glnormal, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-beta.png", gbeta, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-chi.png", gchi, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-poisson.png", gpoisson, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-gamma.png", ggamma, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-hyper.png", ghyper, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-unif.png", gunif, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-t.png", gt, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-f.png", gf, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-invg.png", ginvg, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-exp.png", gexp, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-geom.png", ggeom, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-betabinom.png", gbetabinom, width = 0.9, height = 0.9, units = "in")
ggsave(filename = "images/dist-cauchy.png", gcauchy, width = 0.9, height = 0.9, units = "in")

       