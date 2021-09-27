display_results_ph <- function(results_df,type="summary"){
  
  results_summary <- results_df %>% 
    mutate(iv = case_when(
      
      iv == "chbirtho" ~ "Birth order (numeric)",
      
      # Conditional wealth 
      iv == "wealth_child" ~ "Wealth in childhood",
      iv == "cwealth_teen" ~ "Conditional wealth 6-17y",
      iv == "cwealth_ya" ~ "Conditional wealth 18-26y",
      iv == "cwealth_ea" ~ "Conditional wealth 27-36y",
      

      iv == "chsexfemale" ~ "Sex = Female",
      iv %in% c("male","male1") ~ "Sex = Male",
      iv %in% c("children","children1") ~ "Children = Yes",
      iv %in% c("children_ya","children_ya1") ~ "Children = Yes YA",
      iv %in% c("children_ea","children_ea1") ~ "Children = Yes EA",
      iv == "moscho" ~ "Maternal schooling",
      iv == "moage" ~ "Maternal age",
      iv == "moage" ~ "Maternal age",
      iv == "moht" ~ "Maternal height",
      
      iv %in% c("scale(adeduyr)","scale(eduyr)") ~ "Attained schooling (relative z-scores)",
      iv %in% c("phadeduyr2018","adeduyr","eduyr") ~ "Attained schooling",
      iv %in% c("formal","formal1") ~ "Formal employment",
      iv %in% c("formal_ya","formal_ya1") ~ "Formal employment YA",
      iv %in% c("formal_ea","formal_ea1") ~ "Formal employment EA",
      iv %in% c("married_ea","married_ea1") ~ "Married EA",
      

      iv %in% c("cstratum1983","rural1983") ~ "Rural in 1983",
      iv %in% c("cstratum2009","rural2009") ~ "Rural in 2009",
      iv %in% c("rural_teen","rural_teen1") ~ "Rural TEEN",
      iv %in% c("rural_ya","rural_ya1") ~ "Rural YA",
      iv %in% c("cstratum2018","rural2018","rural20181","rural1","rural","rural_ea","rural_ea1") ~ "Rural EA",
      
      TRUE ~ iv
    )
    )  
  if(type == "summary"){
    results_summary <- results_summary %>% 
      pivot_wider(names_from="Outcome",values_from = "Coefficient")
    
  }
  
  return(results_summary)
  
}