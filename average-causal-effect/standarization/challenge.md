# Coding Challenge: Estimating the Average Causal Effect of HIV Treatment on CD4 Count using G-Computation

## Background
This coding challenge is inspired by the example from Naimi-Cole-Kennedy's paper [1], although we use synthetic data instead of clinical data. The synthetic data satisfies the causal diagram of Figure 1 in [1].

You are given a dataset from a hypothetical observational cohort study on the effect of HIV treatment on CD4 count. The dataset includes information on treatment at baseline (A0), treatment during follow-up (A1), elevated HIV viral load at baseline and follow-up (Z0 and Z1), and the CD4 count outcome. By design, Z0 = 1, i.e. all subjects in the cohort have a high viral load at the start of the study. Your task is to estimate the average causal effect of the treatment on CD4 count (Y) using the g-computation algorithm.

## Dataset Structure
- **A0**: Treatment at baseline (1 = treated, 0 = otherwise)

- **Z1**: Elevated HIV viral load at follow-up (>200 copies/ml = 1, otherwise = 0)

- **A1**: Treatment during follow-up (1 = treated, 0 = otherwise)

- **Y**: Outcome measured at the end of follow-up in units of cells/mm³

The CD4 count (Y) outcome is summarized over participants at each level of treatments and covariate, with the number of participants provided in the dataset.

## Task:
1. **Synthetic Data Generation**: Use the R script below to generate the synthetic dataset.

```r
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
```

2. **Implement G-Computation**: Using the synthetic data, implement the g-computation algorithm to estimate the average causal effect of always being treated (A0 = 1, A1 = 1) versus never being treated (A0 = 0, A1 = 0) on CD4 count (Y).

## Requirements
- R programming skills
- Understanding of g-computation and causal inference

## Submission
Submit an R script that performs the data preparation, implements the g-computation algorithm, and outputs the estimated average causal effect along with a brief interpretation of the results.

## Evaluation Criteria
- Correctness of the implemented g-computation algorithm
- Accuracy of the estimated average causal effect
- Clarity and efficiency of the code
- Quality of the interpretation of the results

## References
- [1] Naimi AI, Cole SR, Kennedy EH. An introduction to g methods. Int J Epidemiol. 2017 Apr 1;46(2):756-762. doi: 10.1093/ije/dyw323. PMID: 28039382; PMCID: PMC6074945. https://academic.oup.com/ije/article/46/2/756/2760169

