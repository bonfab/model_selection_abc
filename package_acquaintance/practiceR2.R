
m_observed <- 16
observed_success <- 6


n_draws <- 1000000

prior <- rbeta(n_draws, 2, 10)

#hist(prior)


generative_model <- rbinom

sime_data <- rep(NA, n_draws)

for(i in 1:n_draws){
  sime_data[i] <- generative_model(1, m_observed, prior[i])
}


posterior <- prior[sime_data == observed_success]

post_results <- sime_data[sime_data == observed_success]

print(length(post_results))

hist(posterior)
length(posterior)

median(posterior)
quantile(posterior, c(0.025, 0.975))

telemarketing <- rbinom(length(post_results), m_observed, 0.2)

print(telemarketing)
print(length(telemarketing))
print(length(post_results))

against_telemarketing <- post_results - telemarketing

print(length(against_telemarketing))
print(length(posterior))

print(against_telemarketing)

count <- 0
for(num in against_telemarketing){
  if(num > 0){
    count <- count +1
  }
}
print(count)

hist(against_telemarketing)

hist(against_telemarketing[against_telemarketing > 0])

length(against_telemarketing[against_telemarketing > 0])

length(against_telemarketing)

print(length(against_telemarketing[against_telemarketing > 0])/length(against_telemarketing))

sum(posterior > 0.2) / length(posterior)



signups <- rbinom(n = length(posterior), size = 100, prob = posterior)

hist(signups)

quantile(signups, c(0.025, 0.975))

