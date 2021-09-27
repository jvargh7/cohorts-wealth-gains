# Creating contrast matrices: BMI YA ---------
contrast_matrix_bmi_ya = matrix(c(
  rep(c(0),each=14),
  rep(c(0),each=14),
  rep(c(0),each=14),
  rep(c(0),each=14),
  rep(c(0),each=14),
  rep(c(0),each=14)
),
nrow=6,
byrow=TRUE
)
contrast_matrix_bmi_ya[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_bmi_ya[2,c(2,12)] <- 1 # wealth_child x male
contrast_matrix_bmi_ya[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_bmi_ya[4,c(4,13)] <- 1 # cwealth_teen x male
contrast_matrix_bmi_ya[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_bmi_ya[6,c(5,14)] <- 1 # cwealth_ya x male

# Creating contrast matrices: BMI EA ---------
contrast_matrix_bmi_ea = matrix(c(
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18)
),
nrow=8,
byrow=TRUE
)
contrast_matrix_bmi_ea[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_bmi_ea[2,c(2,15)] <- 1 # wealth_child x male
contrast_matrix_bmi_ea[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_bmi_ea[4,c(4,16)] <- 1 # cwealth_teen x male
contrast_matrix_bmi_ea[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_bmi_ea[6,c(5,17)] <- 1 # cwealth_ya x male
contrast_matrix_bmi_ea[7,c(6)] <- 1 # cwealth_ea x female
contrast_matrix_bmi_ea[8,c(6,18)] <- 1 # cwealth_ea x male

# Creating contrast matrices: SRQ EA ---------
contrast_matrix_srq_ea = matrix(c(
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18)
),
nrow=8,
byrow=TRUE
)
contrast_matrix_srq_ea[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_srq_ea[2,c(2,15)] <- 1 # wealth_child x male
contrast_matrix_srq_ea[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_srq_ea[4,c(4,16)] <- 1 # cwealth_teen x male
contrast_matrix_srq_ea[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_srq_ea[6,c(5,17)] <- 1 # cwealth_ya x male
contrast_matrix_srq_ea[7,c(6)] <- 1 # cwealth_ea x female
contrast_matrix_srq_ea[8,c(6,18)] <- 1 # cwealth_ea x male

# Creating contrast matrices: STRESS YA ---------
contrast_matrix_stress_ya = matrix(c(
  rep(c(0),each=14),
  rep(c(0),each=14),
  rep(c(0),each=14),
  rep(c(0),each=14),
  rep(c(0),each=14),
  rep(c(0),each=14)
),
nrow=6,
byrow=TRUE
)
contrast_matrix_stress_ya[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_stress_ya[2,c(2,12)] <- 1 # wealth_child x male
contrast_matrix_stress_ya[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_stress_ya[4,c(4,13)] <- 1 # cwealth_teen x male
contrast_matrix_stress_ya[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_stress_ya[6,c(5,14)] <- 1 # cwealth_ya x male

# Creating contrast matrices: RAVENS EA ---------
contrast_matrix_ravens_ea = matrix(c(
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18)
),
nrow=8,
byrow=TRUE
)
contrast_matrix_ravens_ea[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_ravens_ea[2,c(2,15)] <- 1 # wealth_child x male
contrast_matrix_ravens_ea[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_ravens_ea[4,c(4,16)] <- 1 # cwealth_teen x male
contrast_matrix_ravens_ea[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_ravens_ea[6,c(5,17)] <- 1 # cwealth_ya x male
contrast_matrix_ravens_ea[7,c(6)] <- 1 # cwealth_ea x female
contrast_matrix_ravens_ea[8,c(6,18)] <- 1 # cwealth_ea x male


# Creating contrast matrices: HAPPINESS EA ---------
contrast_matrix_happiness_ea = matrix(c(
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18),
  rep(c(0),each=18)
),
nrow=8,
byrow=TRUE
)
contrast_matrix_happiness_ea[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_happiness_ea[2,c(2,15)] <- 1 # wealth_child x male
contrast_matrix_happiness_ea[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_happiness_ea[4,c(4,16)] <- 1 # cwealth_teen x male
contrast_matrix_happiness_ea[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_happiness_ea[6,c(5,17)] <- 1 # cwealth_ya x male
contrast_matrix_happiness_ea[7,c(6)] <- 1 # cwealth_ea x female
contrast_matrix_happiness_ea[8,c(6,18)] <- 1 # cwealth_ea x male