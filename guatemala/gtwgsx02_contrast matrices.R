# Creating contrast matrices: BMI YA ---------
contrast_matrix_bmi_ya = matrix(c(
  rep(c(0),each=17),
  rep(c(0),each=17),
  rep(c(0),each=17),
  rep(c(0),each=17),
  rep(c(0),each=17),
  rep(c(0),each=17)
),
nrow=6,
byrow=TRUE
)
contrast_matrix_bmi_ya[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_bmi_ya[2,c(2,14)] <- 1 # wealth_child x male
contrast_matrix_bmi_ya[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_bmi_ya[4,c(4,15)] <- 1 # cwealth_teen x male
contrast_matrix_bmi_ya[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_bmi_ya[6,c(5,16)] <- 1 # cwealth_ya x male


# Creating contrast matrices: BMI EA ---------
contrast_matrix_bmi_ea = matrix(c(
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20)
),
nrow=8,
byrow=TRUE
)
contrast_matrix_bmi_ea[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_bmi_ea[2,c(2,16)] <- 1 # wealth_child x male
contrast_matrix_bmi_ea[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_bmi_ea[4,c(4,17)] <- 1 # cwealth_teen x male
contrast_matrix_bmi_ea[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_bmi_ea[6,c(5,18)] <- 1 # cwealth_ya x male
contrast_matrix_bmi_ea[7,c(6)] <- 1 # cwealth_ea x female
contrast_matrix_bmi_ea[8,c(6,19)] <- 1 # cwealth_ea x male

# Creating contrast matrices: IQ ---------
contrast_matrix_ravens_ea = matrix(c(
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20),
  rep(c(0),each=20)
),
nrow=8,
byrow=TRUE
)
contrast_matrix_ravens_ea[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_ravens_ea[2,c(2,16)] <- 1 # wealth_child x male
contrast_matrix_ravens_ea[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_ravens_ea[4,c(4,17)] <- 1 # cwealth_teen x male
contrast_matrix_ravens_ea[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_ravens_ea[6,c(5,18)] <- 1 # cwealth_ya x male
contrast_matrix_ravens_ea[7,c(6)] <- 1 # cwealth_ea x female
contrast_matrix_ravens_ea[8,c(6,19)] <- 1 # cwealth_ea x male