read_uv_vis_is_fluid <- function(path){
  data <- read_csv(path, 
                   col_names = T, 
                   skip = 1,
                   show_col_types = F, 
                   name_repair = "unique_quiet") %>% 
    clean_names()
  
  
  headers <- read_csv(path, 
                      col_names = T, 
                      show_col_types = F, 
                      name_repair = "unique_quiet") %>% 
    slice(c(1))
  
  cols <- ncol(data)
  rows <- nrow(data)
  out <- data.frame()
  
  for(i in 1:(cols / 2)){
    sample <- names(headers)[[(2 * i) - 1]]
    
    data_cur <- data[,((2 * i) - 1):((2 * i))] 
    data_cur <- data_cur %>% 
      rename_with(.fn = function(x){case_when(str_detect(x, "wavelength_nm") ~ "wavelength_nm", 
                                              .default = "value")}) %>% 
      mutate(sample_id = sample, 
             scan_info = str_extract(basename(path), ".+(?=\\.)"),
             date = basename(dirname(path))) %>% 
      separate(scan_info, into = c("machine", "attachment", "mode"), sep = "_") %>% 
      mutate(mode = case_when(mode == "C" ~ str_c("C-", str_extract(sample_id, "(?<=-)[:digit:]+")), 
                              .default = mode), 
             sample_id = case_when(str_detect(sample_id, "-") ~ str_extract(sample_id, ".+(?=-)"), 
                                   .default = sample_id)) %>% 
      filter(!str_detect(wavelength_nm, "[^[:digit:]]")) %>% 
      mutate(wavelength_nm = as.numeric(wavelength_nm), 
             value = as.numeric(value))
    
    if(!str_detect(sample, "Baseline")){
      out <- out %>% rbind(data_cur)
    }
    

    
  }
  
  
  return(out)
}
