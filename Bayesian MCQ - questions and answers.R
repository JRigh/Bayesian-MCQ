#---------------------------------------------------------------------------------
# Bayesian Multiple Choice Questionnaire (MCQ) - An Giang university (12.0.7.2022)
#---------------------------------------------------------------------------------

# Question 2:

# Which statement (only one) is correct ?

# A: The Normal distribution is a discrete distribution 
# B: The Normal distribution is a symmetric distribution  
# C: The Poisson distribution is a continuous distribution 
# D: The Gamma distribution is a discrete distribution 

# correct answer is B


# Question 3:

# According to a certain maritime organization, the distribution of the length of 
# a specific fish species at maturity in the Mekong delta has mean mu= 83 cm. 
# and standard deviation sigma = 7 cm. 
# The random variable Y denotes the length of these fishes.


# Suppose we sample 25 individuals. What is the probability that the sample 
# average is above 86 cm. if Y was Normally distributed (hint: no need for CLT) ?

# A: 25.09%
# B: 33.41%
# C: 6.54%
# D: 60%

# correct answer is B

# (86-83)/7 = 0.42857 quantile
# We have to find: P(Z> 0.42857) so the R command is

pnorm(q=86, mean=83, sd=7, lower.tail = FALSE)
# [1] 0.3341


# Question 4:

# Suppose that we record the number of a specific bacteria present in 10 water 
# samples taken in the Mekong delta so that we have the following data:
  
# x_i = 2,2,5,2,5,9,8,3,0,8

# Assuming a Poisson likelihood for the data and using the Jeffreys' prior, 
# what is what is the posterior distribution of the model parameter and the 95 % 
# credible intreval for the model parameter ?

# correct answer is A

# We should recognize the functional form of a Gamma(44.5, 10), then
qgamma(c(0.025, 0.975), sum(c(2,2,5,2,5,9,8,3,0,8))+ 0.5, 10)
# [1] 3.239668 5.849454

# rounded to 4 decimal places
round(qgamma(c(0.025, 0.975), sum(c(2,2,5,2,5,9,8,3,0,8))+ 0.5, 10), 4)
# [1] 3.2397 5.8495

# correct answer is A
