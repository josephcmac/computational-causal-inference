library(tidyverse)

# Generate synthetic dataset
f <- function(U, A0) {
    beta0 <- -0.5 # Intercept could be adjusted based on domain knowledge
    betaU <- 0.8  # Effect of unmeasurable common cause
    betaA0 <- -0.25 # Effect of treatment at baseline
    X <- exp(beta0 + betaU*U + betaA0*A0)
    prob <- X / (1 + X)
    return(prob)
}
g <- function(Z1) {
    alpha0 <- -0.3 # Intercept, adjust based on expected treatment uptake
    alphaZ1 <- 0.6 # Effect of elevated HIV viral load at follow-up
    X <- exp(alpha0 + alphaZ1*Z1)
    prob <- X / (1 + X)
    return(prob)
}
h <- function(A0, Z1, A1, U) {
    gamma0 <- 500 # Intercept representing baseline CD4 count level
    gammaA0 <- -30 # Effect of baseline treatment
    gammaZ1 <- -50 # Effect of elevated HIV viral load at follow-up
    gammaA1 <- 40 # Effect of treatment during follow-up
    gammaU <- -20 # Effect of unmeasurable common cause
    # Assuming linear relationship for simplicity; real-world scenarios might require more complex models
    Y <- max(0, gamma0 + gammaA0*A0 + gammaZ1*Z1 + gammaA1*A1 + gammaU*U)
    return(Y)
}
# n : number of participants
synthetic_data <- function(seed, n, f, g, h) {
    set.seed(seed) # For reproducibility
    #Z0 <- rep(1, n) # Elevated HIV viral load at baseline (constant by design)
    U <- runif(n, 0, 1) # Unmeasurable common cause
    A0 <- sample(0:1, n, replace = TRUE) # Treatment at baseline
    Z1 <- sapply(1:n, function(i) { # Elevated HIV viral load at follow-up
        p <- f(U[i], A0[i])
        return(sample(0:1, 1, prob=c(p, 1-p)))
    })
    A1 <- sapply(1:n, function(i) { # Treatment during follow-up
        p <- g(Z1[i])
        return(sample(0:1, 1, prob=c(p, 1-p)))
    })
    # Generating CD4 count outcome with some made-up relationship for illustration
    Y <- sapply(1:n, function(i) {
        return( h(A0[i], Z1[i], A1[i], U[i]) )
    }) 
    return (data.frame(A0, Z1, A1, Y))
}
df <- synthetic_data(seed = 123, n = 500000, f, g, h)


head(df)

table1 <- df %>% 
    group_by(A0, Z1, A1) %>% 
    summarise(Y = mean(Y), N = n(), .groups = "drop")

print(table1)

model <- lm(Y ~ A1 + A0 + Z1, data=df)
df$always <- predict(model, df %>% mutate(A0 = 1, A1 = 1))
df$never <- predict(model, df %>% mutate(A0 = 0, A1 = 0))

head(df)

cat("Average Causal Effect: ", mean(df$always - df$never), "\n")

