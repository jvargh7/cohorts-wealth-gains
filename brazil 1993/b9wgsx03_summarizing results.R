source(paste0(path_incap_repo,"/rally/clean_glm_result.R"))
source(paste0(path_replication_repo,"/package/clean_mi_conditionalregression.R"))


source(paste0(path_gains_repo,"/brazil 1993/b9wgsx01_fitting models.R"))
source(paste0(path_gains_repo,"/brazil 1993/b9wgsx02_contrast matrices.R"))

(sex_stratified = bind_rows(clean_mi_contrasts(models_bmi_ya,link = "lm",model_matrix = contrast_matrix_bmi,vcov_type = "naive") %>% 
            mutate(Outcome = "BMI YA (kg/m2)"),
          clean_mi_contrasts(models_srq_ya,link = "lm",model_matrix = contrast_matrix_srq,vcov_type = "naive") %>% 
            mutate(Outcome = "STRESS (z-scores) YA"),
          clean_mi_contrasts(models_iq_ya,link = "lm",model_matrix = contrast_matrix_iq,vcov_type = "naive") %>% 
            mutate(Outcome = "IQ (z-scores) YA"),
          clean_mi_contrasts(models_wellbeing_ya,link = "lm",model_matrix = contrast_matrix_wellbeing,vcov_type = "naive") %>% 
            mutate(Outcome = "WELLBEING (z-scores) YA")
) %>%
  mutate(iv = case_when(iv == "Contrast 1" ~ "Wealth in childhood x Female",
                        iv == "Contrast 2" ~ "Wealth in childhood x Male",
                        iv == "Contrast 3" ~ "Conditional wealth 6-17y x Female",
                        iv == "Contrast 4" ~ "Conditional wealth 6-17y x Male",
                        iv == "Contrast 5" ~ "Conditional wealth 18-26y x Female",
                        iv == "Contrast 6" ~ "Conditional wealth 18-26y x Male",
                        TRUE ~ NA_character_
                        ))) %>% 
  dplyr::select(Outcome,iv,Coefficient) %>% 
  arrange(iv) %>% 
  pivot_wider(names_from=Outcome,values_from=Coefficient) %>%
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/brazil 1993/sex stratified_conditionals.csv"),row.names = FALSE)


# Coefficient Plot -----------

  sex_stratified %>% 
  dplyr::select(iv,Outcome,theta_D,lci,uci) %>% 
  ggplot(data=.,aes(x=theta_D,xmin=lci,xmax=uci,y=iv)) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_grid(~Outcome,scales="free_x") +
  theme_bw() +
  geom_vline(aes(xintercept=0),col="red",linetype=2) +
  ylab("") + xlab("Estimate (95% CI)")

  # https://stats.stackexchange.com/questions/418267/pooling-f-values-in-multiple-regression-in-a-multiply-imputed-database

  