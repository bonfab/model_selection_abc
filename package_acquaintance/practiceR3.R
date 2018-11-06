
m_observed <- 16

success_A <- 6
success_B <- 10

diff_success <- success_B - success_A

sampling_size <- 10000

prior <- rbeta(sampling_size, 2, 10)

model <- function(theta){
  return(rbinom(sampling_size, m_observed, theta))
}


sim <- model(prior)

stratA <- prior[sim == success_A]
stratB <- prior[sim == success_B]


hist(stratA)
hist(stratB)

print(mean(stratB) / (mean(stratA) + mean(stratB)))
  
expected_revenue_A <- (sample(stratA, 10000, replace = TRUE) * 1000 - 30)

expected_revenue_B <- (sample(stratB , 10000, replace = TRUE)  * 1000 - 300)

hist(expected_revenue_A - expected_revenue_B)


print(sum(expected_revenue_A /10000))
print(sum(expected_revenue_B / 10000))


prior2 <- rbeta(sampling_size, 2, 10)

sim2 <- model(prior)  - model(prior2)

A <- prior2[sim2 == diff_success]
B <- prior[sim2 == diff_success]

diff <- B - A

hist(diff)

print(length(diff[diff > 0])/length(diff))

