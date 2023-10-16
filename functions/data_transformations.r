# Data transformations

transform_metadata_to_df <- 
  function(.){ # function  transforming the data from veivesnet to dataframe
    (.)[[1]] %>% 
      map_dfr(~as_tibble(.)) %>% 
      mutate(latestData = map_chr(latestData,1,.default="")) %>% 
      mutate(latestData = lubridate::as_datetime(latestData,tz="UTC")) %>% 
      unnest_wider(location) %>% 
      unnest_wider(latLon)
  }



to_iso8601 <- 
  function(date_time,offset_days){
    (date_time + days(offset_days)) %>%  
      iso8601(.) %>% 
      glue::glue(.,"Z")
  }


transform_volumes <- 
  function(data){
    data$trafficData$volume %>% 
      map_dfr(~as_tibble(.)) %>% 
      unnest_wider(edges) %>% 
      unnest_wider(node) %>% 
      unnest_wider(total) %>% 
      unnest_wider(volumeNumbers) %>% 
      mutate(
        from=lubridate::as_datetime(from,tz="Europe/Berlin"),
        to=lubridate::as_datetime(to,tz="Europe/Berlin")
      )
  }
