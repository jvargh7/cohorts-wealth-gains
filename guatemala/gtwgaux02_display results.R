display_results_gtml <- function(results_df,type = "summary"){
  
  results_summary <- results_df %>% 
    mutate(iv = case_when(iv == "gtatole" ~ "Atole (Period = NONE)",
                          iv == "byear" ~ "Birth year - 1970",
                          iv == "chbirtho" ~ "Birth order (numeric)",
                          iv == "full" ~ "Period = Full (Fresco)",
                          iv %in% c("gtatole:full","atole1000") ~ "Atole x Full",
                          
                          iv == "wealth_child" ~ "Wealth in childhood",
                          iv == "cwealth_teen" ~ "Conditional wealth 6-17y",
                          iv == "cwealth_ya" ~ "Conditional wealth 18-26y",
                          iv == "cwealth_ea" ~ "Conditional wealth 27-36y",

                          
                          iv == "chsexfemale" ~ "Sex = Female",
                          iv %in% c("male","male1") ~ "Sex = Male",
                          iv %in% c("children","children1") ~ "Children = Yes",
                          iv %in% c("childrenya","childrenya1") ~ "Children = Yes YA",
                          iv %in% c("childrenea","childrenea1") ~ "Children = Yes EA",
                          
                          iv == "moscho_sib" ~ "Maternal schooling",
                          iv == "moage" ~ "Maternal age",
                          iv == "moht_sib" ~ "Maternal height",
                          
                          iv %in% c("scale(adeduyr)","scale(gtadeduyr1618)") ~ "Attained schooling (relative z-scores)",
                          iv %in% c("gtadeduyr1618","adeduyr","eduyr") ~ "Attained schooling",
                          iv %in% c("formal_ya","formal_ya1","formal98991") ~ "Formal employment YA",
                          iv %in% c("formal_ea","formal_ea1","formal041") ~ "Formal employment EA",
                          
                          iv %in% c("rural_ea","rural_ea1","rural041") ~ "Rural in 2002-04",
                          iv %in% c("rural_ya","rural_ya1","rural961") ~ "Rural in 1996",

                          TRUE ~ NA_character_
    )
    ) 
  if(type == "summary"){
    results_summary <- results_summary %>% 
      pivot_wider(names_from="Outcome",values_from = "Coefficient")
    
  }
  
  return(results_summary)
  
}