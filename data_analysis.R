source("utils.R")

# Reading the input files
ships_data <- fread("data/ships.csv")
countries_codes <- fread("data/country_codes.csv")

# Joining the data to countries dataset to ontain the flags
ships_data <- left_join(
  ships_data,
  countries_codes,
  by = c("FLAG" = "Code")
) 

# Transforming the data ----
# 1. Transform all names to snake case
# 2. Giving meaningful title names to variables
ships_data <- ships_data %>% 
  transmute(
    ship_id = SHIP_ID,
    lat = LAT,
    lon = LON,
    speed = SPEED,
    course = COURSE,
    heading = HEADING,
    country = Name,
    destination = str_to_title(DESTINATION),
    flag = tolower(FLAG),
    ship_length = LENGTH,
    ship_name = str_to_title(SHIPNAME),
    ship_type_id = SHIPTYPE,
    ship_type = str_to_title(ship_type),
    ship_width = WIDTH,
    dead_weight = DWT,
    observation_time = DATETIME,
    observation_date = date,
    port_assigned = str_to_title(port),
    port_reported = str_to_title(PORT),
    week_number = week_nb,
    ship_type,
    is_parked
  )

# 3. Include an image url
ships_data <- ships_data %>% 
  mutate(
    image_url = case_when(
      ship_type == "Cargo" ~ "images/Cargo.svg",
      ship_type == "Tanker" ~ "images/Tanker.svg",
      ship_type == "Unspecified" ~ "images/Unspecified.svg",
      ship_type == "Tug" ~ "images/Tug.svg",
      ship_type == "Fishing" ~ "images/Fishing.svg",
      ship_type == "Passenger" ~ "/images/Passenger.svg",
      ship_type == "Pleasure" ~ "images/Pleasure.svg",
      ship_type == "Navigation" ~ "images/Navigation.svg",
      ship_type == "High Special" ~ "images/High Special.svg"
  )
  )

# 4. Update port names for uniformity
ships_data <- ships_data %>% 
  mutate(port_assigned = case_when(
    port_assigned == "Gdaå„Sk" ~ "Gdansk",
    port_assigned == "St. Petersburg" ~ "St_petersburg",
    TRUE ~ port_assigned
  )
         )

# Performing the calculations on the data ----
# Obtain the next consecutive observations
ships_data <- ships_data %>%
  group_by(ship_id) %>% 
  arrange(ship_id, observation_time) %>%
  mutate(
    lon_end = lead(lon),
    lat_end = lead(lat),
    port_assigned_end = lead(port_assigned),
    port_reported_end = lead(port_reported),
    join_helper_column = row_number()
  ) 

# Obtaining distance between two locations
ships_data <- ships_data %>% 
  mutate(distance = ifelse(is.na(lat_end), NA, distHaversine(cbind(lon, lat),
                                                             cbind(lon_end, lat_end)
                                                             )
                           )
         )

# Maximum distance observation per ship
max_dist_observation <- ships_data %>% 
  group_by (ship_id) %>% 
  arrange(desc(distance), desc(observation_time)) %>% 
  slice(1) %>% 
  ungroup()

# Merging the data points ----
# First data point (begin observation)
begin_observation <- inner_join(
  ships_data,
  max_dist_observation %>% 
    select(ship_id, distance),
  by = c("ship_id" = "ship_id",
         "distance" = "distance")
  ) %>%
  group_by(ship_id) %>% 
  arrange(desc(distance)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(data_point = "start")

# Last data point (end observation)
end_observation <- inner_join(
  ships_data,
  begin_observation %>% 
    mutate(join_helper_column = join_helper_column + 1) %>% 
    select(ship_id, join_helper_column),
  by = c("ship_id", "join_helper_column")
) %>%
  group_by(ship_id) %>% 
  arrange(desc(distance)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(data_point = "end")

# Merging the data points
analyzed_data <- rbind(end_observation,
                       begin_observation) %>%
  arrange(ship_id, data_point) # To allow capturing of distance


# Port statistics----
# 1. Summarizing the data
ship_data_statistics <- ships_data %>% 
  group_by(ship_id) %>% 
  summarize(observations = n(),
            distance_travelled = sum(distance, na.rm = TRUE),
            dead_weight = mean(dead_weight)  
  ) 

# 2. Average vessel speed
average_speed <- ships_data %>% 
  filter(is_parked %in% 0) %>% 
  group_by(ship_id) %>% 
  summarize(max_speed = max(speed, na.rm = TRUE),
            min_speed = min(speed, na.rm = TRUE),
            mean_speed = mean(speed, na.rm = TRUE)
  )

# 3. Is the ship parked
park_status <- ships_data %>% 
  arrange(ship_id, desc(join_helper_column)) %>% 
  group_by(ship_id) %>% 
  slice_max(1) %>%
  select(ship_id, is_parked)

# 4. Daily observations
daily_observations <- ships_data %>% 
  filter(is_parked == 0) %>% 
  group_by(ship_id, 
           observation_time) %>% 
  summarise(speed = mean(speed))

# 5.Last observation
final_observations <- ships_data %>% 
  group_by(ship_id) %>% 
  arrange(desc(observation_time)) %>% 
  slice_max(1) %>% ungroup()