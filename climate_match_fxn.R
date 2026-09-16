###make climate match function
#make sure all time periods needed are included here

climate_match <- function(prov_clim, plant_clim, mcmt_TL, shm_TL, plant_period){
  
  m=prov_clim %>% select(provenance_name)
  
  if("8GCMs_ensemble_ssp245_2011-2040.gcm" %in% plant_period == TRUE){
    mcmt_MS_1140_245 <- (abs(prov_clim$Normal_1961_1990_MCMT - plant_clim$`8GCMs_ensemble_ssp245_2011-2040_MCMT`)/mcmt_TL)^2
    shm_MS_1140_245 <- (abs(prov_clim$Normal_1961_1990_SHM - plant_clim$`8GCMs_ensemble_ssp245_2011-2040_SHM`)/shm_TL)^2
    
    m_1140_245 <- data.frame(m_1140_245 = (-1*(sqrt(mcmt_MS_1140_245+shm_MS_1140_245)-1))*100) %>% 
      bind_cols(prov_clim) %>% 
      bind_cols(delta_mcmt_1140_245 =  plant_clim$`8GCMs_ensemble_ssp245_2011-2040_MCMT` - prov_clim$Normal_1961_1990_MCMT) %>% 
      bind_cols(delta_shm_1140_245 = plant_clim$`8GCMs_ensemble_ssp245_2011-2040_SHM` - prov_clim$Normal_1961_1990_SHM)
    
    m = left_join(m, m_1140_245)
  }
  
  if("Normal_1991_2020.nrm" %in% plant_period == TRUE){
    mcmt_MS_9120 <- (abs(prov_clim$Normal_1961_1990_MCMT - plant_clim$Normal_1991_2020_MCMT)/mcmt_TL)^2
    shm_MS_9120 <- (abs(prov_clim$Normal_1961_1990_SHM - plant_clim$Normal_1991_2020_SHM)/shm_TL)^2
    
    m_9120 <- data.frame(m_9120 = (-1*(sqrt(mcmt_MS_9120+shm_MS_9120)-1))*100) %>% 
      bind_cols(prov_clim) %>% 
      bind_cols(delta_mcmt_9120 =  plant_clim$Normal_1991_2020_MCMT - prov_clim$Normal_1961_1990_MCMT) %>% 
      bind_cols(delta_shm_9120 = plant_clim$Normal_1991_2020_SHM - prov_clim$Normal_1961_1990_SHM)
    
    m = left_join(m, m_9120)
  }
  
  if("Normal_1981_2010.nrm" %in% plant_period == TRUE){
    mcmt_MS_8110 <- (abs(prov_clim$Normal_1961_1990_MCMT - plant_clim$Normal_1981_2010_MCMT)/mcmt_TL)^2
    shm_MS_8110 <- (abs(prov_clim$Normal_1961_1990_SHM - plant_clim$Normal_1981_2010_SHM)/shm_TL)^2
    
    m_8110 <- data.frame( m_8110 = (-1*(sqrt(mcmt_MS_8110+shm_MS_8110)-1))*100) %>% 
      bind_cols(prov_clim) %>% 
      bind_cols(delta_mcmt_8110 =  plant_clim$Normal_1981_2010_MCMT - prov_clim$Normal_1961_1990_MCMT) %>% 
      bind_cols(delta_shm_8110 = plant_clim$Normal_1981_2010_SHM - prov_clim$Normal_1961_1990_SHM)
    
    m=left_join(m, m_8110)
  }
  
  
  if("Normal_1971_2000.nrm" %in% plant_period == TRUE){
    mcmt_MS_7100 <- (abs(prov_clim$Normal_1961_1990_MCMT - plant_clim$Normal_1971_2000_MCMT)/mcmt_TL)^2
    shm_MS_7100 <- (abs(prov_clim$Normal_1961_1990_SHM - plant_clim$Normal_1971_2000_SHM)/shm_TL)^2
    
    m_7100 <- data.frame( m_7100 = (-1*(sqrt(mcmt_MS_7100+shm_MS_7100)-1))*100) %>% 
      bind_cols(prov_clim) %>% 
      bind_cols(delta_mcmt_7100 =  plant_clim$Normal_1971_2000_MCMT - prov_clim$Normal_1961_1990_MCMT) %>% 
      bind_cols(delta_shm_7100 = plant_clim$Normal_1971_2000_SHM - prov_clim$Normal_1961_1990_SHM)
    
    m=left_join(m, m_7100)
  }
  
  
  if("Normal_1961_1990.nrm" %in% plant_period == TRUE){
    mcmt_MS_6190 <- (abs(prov_clim$Normal_1961_1990_MCMT - plant_clim$Normal_1961_1990_MCMT)/mcmt_TL)^2
    shm_MS_6190 <- (abs(prov_clim$Normal_1961_1990_SHM - plant_clim$Normal_1961_1990_SHM)/shm_TL)^2
    
    m_6190 <- data.frame( m_6190 = (-1*(sqrt(mcmt_MS_6190+shm_MS_6190)-1))*100) %>% 
      bind_cols(prov_clim) %>% 
      bind_cols(delta_mcmt_6190 =  plant_clim$Normal_1961_1990_MCMT - prov_clim$Normal_1961_1990_MCMT) %>% 
      bind_cols(delta_shm_6190 = plant_clim$Normal_1961_1990_SHM - prov_clim$Normal_1961_1990_SHM)
    
    m=left_join(m, m_6190)
  }
  
  return(m)}