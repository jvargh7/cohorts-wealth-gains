source(paste0(path_incap_repo,"/rally/clean_glm_result.R"))
source(paste0(path_replication_repo,"/package/clean_mi_conditionalregression.R"))


source(paste0(path_gains_repo,"/south africa/sawgsx01_fitting models.R"))
source(paste0(path_gains_repo,"/south africa/sawgsx02_contrast matrices.R"))



(sex_stratified <- bind_rows(clean_mi_contrasts(models_bmi_ya,link = "lm",model_matrix = contrast_matrix_bmi_ya,vcov_type = "naive") %>% 
            mutate(Outcome = "BMI YA"),
            clean_mi_contrasts(models_srq_ea,link = "lm",model_matrix = contrast_matrix_srq_ea,vcov_type = "naive") %>% 
              mutate(Outcome = "SRQ20 z-scores EA"),
            clean_mi_contrasts(models_ravens_ea,link = "lm",model_matrix = contrast_matrix_ravens_ea,vcov_type = "naive") %>% 
              mutate(Outcome = "IQ z-scores EA"),
            clean_mi_contrasts(models_happiness_ea,link = "lm",model_matrix = contrast_matrix_happiness_ea,vcov_type = "naive") %>% 
              mutate(Outcome = "WELLBEING z-scores EA")
)%>%
    mutate(iv = case_when(iv == "Contrast 1" ~ "Wealth in childhood x Female",
                          iv == "Contrast 2" ~ "Wealth in childhood x Male",
                          iv == "Contrast 3" ~ "Conditional wealth 6-17y x Female",
                          iv == "Contrast 4" ~ "Conditional wealth 6-17y x Male",
                          iv == "Contrast 5" ~ "Conditional wealth 18-26y x Female",
                          iv == "Contrast 6" ~ "Conditional wealth 18-26y x Male",
                          iv == "Contrast 7" ~ "Conditional wealth 27-36y x Female",
                          iv == "Contrast 8" ~ "Conditional wealth 27-36y x Male",
                          TRUE ~ NA_character_
    ))) %>% 
  dplyr::select(Outcome,iv,Coefficient) %>% 
  arrange(iv) %>% 
  pivot_wider(names_from=Outcome,values_from=Coefficient) %>%
  write.csv(.,paste0(path_dissertation,"/aim 2/working/cohorts gains/south africa/sex stratified_conditionals.csv"),row.names = FALSE)


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

