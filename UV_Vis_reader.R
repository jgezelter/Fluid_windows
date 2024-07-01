library(tidyverse)
library(janitor)

read_uv_vis <- function(file){
  data <- read_csv(file, 
                   col_names = T, 
                   skip = 1, 
                   show_col_types = F) %>% 
    clean_names()
  
  path <- file
  
  headers <- read_csv(file, col_names = T, show_col_types = F) %>% slice(c(1))
  
  cols <- ncol(data)
  rows <- nrow(data)
  out <- data.frame()
  
  for(i in 1:(cols / 2)){
    sample <- names(headers)[[(2 * i) - 1]]
    
    data_cur <- data[,((2 * i) - 1):((2 * i))] 
    data_cur <- data_cur %>% 
      mutate(measurement_type = str_extract(names(data_cur)[2], ".+(?=[:digit:])")) %>% 
      rename_with(.fn = function(x){case_when(str_detect(x, "wavelength_nm") ~ "wavelength_nm", 
                                              str_detect(x, "measurement_type") ~ "measurement_type",
                                              .default = "value")}) %>% 
      mutate(sample = sample, 
             id = basename(path), 
             date = basename(dirname(path))) %>% 
      separate(id, into = c("machine", "attachment", "mode"), sep = "_") %>% 
      separate(sample, 
               into = c("solvent", "dye", "concentration", "measurement_num"), 
               sep = "_", 
               remove = F) %>% 
      mutate(mode = str_extract(mode, ".+(?=\\.csv)"), 
             concentration_value = as.numeric(str_extract(concentration, "[^[:alpha:]]+")), 
             concentration_type = str_extract(concentration, "[:alpha:]+"))
    if(!str_detect(sample, "Baseline")){
      out <- out %>% rbind(data_cur)
    }
    
  }
  
  out_help <- out %>% 
    filter(dye == "")
  
  out_help_3 <- out %>% 
    filter(dye != "")
  
  out_help_2 <- unique(out_help_3$dye)
  
  out2 <- data.frame()
  
  for(i in 1:length(out_help_2)){
    out_help_4 <- out_help %>% 
      mutate(dye = out_help_2[i])
    
    out2 <- out2 %>% rbind(out_help_4)
  }
  
  out2 <- out2 %>% 
    mutate(dye = as.character(dye)) %>%  
    bind_rows(out_help_3)
  
  conc_orderer <- out2 %>% 
    count(concentration_value, concentration_type) %>% 
    mutate(conc_order = str_c(concentration_value, concentration_type))
  
  out2 <- out2 %>% 
    mutate(concentration = factor(concentration, 
                                  levels = conc_orderer$conc_order))
  
  return(out2)
}