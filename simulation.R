# code translated from Helge Klapper; https://github.com/helgeklapper/Organizational-Decision-Making

library(ggplot2)

# Parameters

t_high <- 10
t_low <- 0
q_high <- 5
q_low <- -5
e_low <- 0
e_mid <- 5
e_high <- 10
E <- 100000
K <- seq(0, 5, length.out = 11)
number_Ks <- length(K)

# Functions

project <- function(q_low, q_high, t_low, t_high) {
  value_p <- runif(1, q_low, q_high)
  type_p <- runif(1, t_low, t_high)
  return(c(value_p, type_p))
}

perceive_quality <- function(value_p, type_p, type_ind) {
  knowledge_distance <- abs(type_p - type_ind)
  noise <- rnorm(1, 0, knowledge_distance)
  perceive <- value_p + noise
  return(perceive)
}

choose_individual <- function(value_p, per_e_mid) {
  if (per_e_mid > 0) {
    return(value_p)
  } else {
    return(0)
  }
}

choose_delegate <- function(value_p, per_e_low, per_e_mid, per_e_high, type_p, e_low, e_mid, e_high) {
  decision <- 0
  if (type_p < ((e_mid + e_low)/2)) {
    if (per_e_low > 0) {
      decision <- 1
    }
  } else if (type_p > ((e_mid + e_high)/2)) {
    if (per_e_high > 0) {
      decision <- 1
    }
  } else {
    if (per_e_mid > 0) {
      decision <- 1
    }
  }
  if (decision == 1) {
    return(value_p)
  } else {
    return(0)
  }
}

choose_voting <- function(value_p, per_e_low, per_e_mid, per_e_high) {
  vote <- (per_e_low > 0) + (per_e_mid > 0) + (per_e_high > 0)
  if (vote >= 2) {
    return(value_p)
  } else {
    return(0)
  }
}

choose_average <- function(value_p, per_e_low, per_e_mid, per_e_high) {
  per_vector <- c(per_e_low, per_e_mid, per_e_high)
  decision <- mean(per_vector)
  if (decision > 0) {
    return(value_p)
  } else {
    return(0)
  }
}

# Simulation

performance_matrix <- matrix(0, nrow = number_Ks, ncol = 4)
value_type_matrix <- matrix(0, nrow = E, ncol = 2)

k_counter <- 1
for (k in K) {
  cat('At k: ', k, '\n')
  e_high <- 5 + k
  e_low <- 5 - k
  temp_matrix <- matrix(0, nrow = E, ncol = 4)
  for (e in 1:E) {
    project_result <- project(q_low, q_high, t_low, t_high)
    value_p <- project_result[1]
    type_p <- project_result[2]
    value_type_matrix[e, ] <- c(value_p, type_p)
    per_e_low <- perceive_quality(value_p, type_p, e_low)
    per_e_mid <- perceive_quality(value_p, type_p, e_mid)
    per_e_high <- perceive_quality(value_p, type_p, e_high)
    temp_matrix[e, 1] <- choose_individual(value_p, per_e_mid)
    temp_matrix[e, 2] <- choose_delegate(value_p, per_e_low, per_e_mid, per_e_high, type_p, e_low, e_mid, e_high)
    temp_matrix[e, 3] <- choose_voting(value_p, per_e_low, per_e_mid, per_e_high)
    temp_matrix[e, 4] <- choose_average(value_p, per_e_low, per_e_mid, per_e_high)
  }
  performance_matrix[k_counter, ] <- colMeans(temp_matrix)
  k_counter <- k_counter + 1
}

# Plotting

df <- data.frame(K = rep(K, each = 4),
                 Performance = as.vector(t(performance_matrix)),
                 Method = rep(c("Individual", "Delegation", "Voting", "Averaging"), times = number_Ks))

ggplot(df, aes(x = K, y = Performance, color = Method)) +
  geom_line() +
  geom_point() +
  labs(x = "Knowledge breadth", y = "Performance") +
  theme_minimal() +
  theme(legend.title = element_blank())
