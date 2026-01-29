library(boot)
set.seed(1)

Default <- read.csv("Insert your file path")
Default$default <- as.factor(Default$default)

# 2a Defines boot.fn
boot.fn <- function(data, index) {
  coef(
    glm(default ~ income + balance,
        data = data,
        family = binomial,
        subset = index)
  )[c("income", "balance")]
}

# 2b Bootstrap with 1000 resamples
set.seed(1)
boot.out <- boot::boot(Default, boot.fn, R = 1000)

boot.out

# Print Bootstrap standard errors for income and balance
apply(boot.out$t, 2, sd)
