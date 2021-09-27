# Creating contrast matrices: BMI ---------
contrast_matrix_bmi = matrix(c(
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
contrast_matrix_bmi[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_bmi[2,c(2,15)] <- 1 # wealth_child x male
contrast_matrix_bmi[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_bmi[4,c(4,16)] <- 1 # cwealth_teen x male
contrast_matrix_bmi[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_bmi[6,c(5,17)] <- 1 # cwealth_ya x male


# Creating contrast matrices: SRQ ---------
contrast_matrix_srq = matrix(c(
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
contrast_matrix_srq[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_srq[2,c(2,15)] <- 1 # wealth_child x male
contrast_matrix_srq[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_srq[4,c(4,16)] <- 1 # cwealth_teen x male
contrast_matrix_srq[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_srq[6,c(5,17)] <- 1 # cwealth_ya x male

# Creating contrast matrices: IQ ---------
contrast_matrix_iq = matrix(c(
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
contrast_matrix_iq[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_iq[2,c(2,12)] <- 1 # wealth_child x male
contrast_matrix_iq[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_iq[4,c(4,13)] <- 1 # cwealth_teen x male
contrast_matrix_iq[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_iq[6,c(5,14)] <- 1 # cwealth_ya x male

# Creating contrast matrices: WELLBEING ---------
contrast_matrix_wellbeing = matrix(c(
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
contrast_matrix_wellbeing[1,c(2)] <- 1 # wealth_child x female
contrast_matrix_wellbeing[2,c(2,15)] <- 1 # wealth_child x male
contrast_matrix_wellbeing[3,c(4)] <- 1 # cwealth_teen x female
contrast_matrix_wellbeing[4,c(4,16)] <- 1 # cwealth_teen x male
contrast_matrix_wellbeing[5,c(5)] <- 1 # cwealth_ya x female
contrast_matrix_wellbeing[6,c(5,17)] <- 1 # cwealth_ya x male