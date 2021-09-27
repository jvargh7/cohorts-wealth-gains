display_results_b9 <- function(results_df,type="summary"){
  
  results_summary <- results_df %>% 
    mutate(iv = case_when(
      
      iv == "chbirtho" ~ "Birth order (numeric)",
      
      # Conditional wealth 
      iv == "wealth_child" ~ "Wealth in childhood",
      iv == "cwealth_teen" ~ "Conditional wealth 6-17y",
      iv == "cwealth_ya" ~ "Conditional wealth 18-26y",
      iv == "cwealth_ea" ~ "Conditional wealth 27-36y",
      
      # Conditional wealth x sex
      iv %in% c("wealth_child:male1","wealth_child:male") ~ "Wealth in childhood x Male",
      iv %in% c("cwealth_teen:male1","cwealth_teen:male") ~ "Conditional wealth 6-17y x Male",
      iv %in% c("cwealth_ya:male1","cwealth_ya:male") ~ "Conditional wealth 18-26y x Male",
      iv %in% c("cwealth_ea:male1","cwealth_ea:male") ~ "Conditional wealth 27-36y x Male",
      
      
      iv == "chsexfemale" ~ "Sex = Female",
      iv %in% c("children","children1","children_ya1","children_ea1") ~ "Children = Yes",
      iv %in% c("children_2011","children_20111") ~ "Children in 2011 = Yes",
      iv %in% c("children_2015","children_20151") ~ "Children in 2015 = Yes",
      iv == "nchildren" ~ "Number of children",
      iv %in% c("male","male1") ~ "Sex = Male",
      
      iv == "moscho" ~ "Maternal schooling",
      iv == "moage" ~ "Maternal age",
      iv == "moht" ~ "Maternal height",
      iv %in% c("momwhite","momwhite1") ~ "Mother is white",
      
      iv %in% c("scale(adeduyr)","scale(eduyr)") ~ "Attained schooling (relative z-scores)",
      iv %in% c("b9adeduyr2015","adeduyr","eduyr","eduyr_ya","eduyr_ea") ~ "Attained schooling",
      iv %in% c("formal_ya","formal_ya1") ~ "Formal employment YA",
      iv %in% c("formal_ea","formal_ea1") ~ "Formal employment EA",
      iv %in% c("white","white1") ~ "White skin color",
      iv %in% c("married_ya","married_ya1") ~ "Married YA",
      
      TRUE ~ NA_character_
    )
    )   
  if(type == "summary"){
    results_summary <- results_summary %>% 
      pivot_wider(names_from="Outcome",values_from = "Coefficient")
    
  }
  
  return(results_summary)
  
}