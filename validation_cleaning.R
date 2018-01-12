# @author Scott Dobbins
# @version 0.9.9.7
# @date 2018-01-12 00:30


#WW2_basic_fixes <- copy(WW2_bombs)#***
### the big city filtering question thing
debug_message("verifying data by row")
# turn off indexing to prevent cache invalidation errors in WW2_bombs$Target_City's level table
previous_auto_index_option <- getOption('datatable.auto.index')
previous_use_index_option <- getOption('datatable.auto.index')
options(datatable.auto.index = FALSE)
options(datatable.use.index = FALSE)


### discover and fix partially-named cities
debug_message("fixing partially-named cities")

city_levels <- sort(fix_spaces(levels(WW2_bombs$Target_City)))
city_levels_length <- length(city_levels)

partial_string_levels <- list()

ignored_similar_cities <- character(0L)
changed_similar_cities <- character(0L)

for (city_index in seq_along(city_levels)[c(-1L, -city_levels_length)]) {
  city <- city_levels[city_index]
  updated_city_levels <- levels(WW2_bombs$Target_City)
  if (city %c% updated_city_levels) {
    next_city_index <- city_index + 1L
    next_city <- city_levels[next_city_index]
    similar_cities <- character(0L)
    while (next_city_index <= city_levels_length && 
           next_city %c% updated_city_levels && 
           adist(city, next_city, fixed = TRUE, partial = TRUE) == 0L) {
      similar_cities <- append(similar_cities, next_city)
      next_city_index <- next_city_index + 1L
      next_city <- city_levels[next_city_index]
    }
    if (!is_empty(similar_cities)) {
      # in case list is necessary#***
      similar_cities_list <- list(similar_cities)
      re_name(similar_cities_list, city)
      partial_string_levels <- append(partial_string_levels, similar_cities_list)
      
      # keep only those names that have lat-long pairs closeby
      possible_spellings <- c(city, similar_cities)
      mean_lat <- WW2_bombs[Target_City == city, mean(Target_Latitude, na.rm = TRUE)]
      mean_long <- WW2_bombs[Target_City == city, mean(Target_Longitude, na.rm = TRUE)]
      relevant_data <- WW2_bombs[Target_City %in% possible_spellings & 
                                   close_to(Target_Latitude, mean_lat, coord_buffer) & 
                                   close_to(Target_Longitude, mean_long, coord_buffer), 
                                 .(Target_City, Target_Country, Target_Latitude, Target_Longitude), 
                                 nomatch = 0L]
      possible_spellings <- relevant_data[, as.character(Target_City)]
      if (length(possible_spellings) > 1L) {
        changed_similar_cities <- append(changed_similar_cities, city)
        
        # if extra words (if exists space), then do target_rules on city names and see if the results match target_names; if so, delete that word
        corrected_spellings <- toupper(possible_spellings) %>% gsubs(changes = target_rules)
        has_target <- corrected_spellings %like% word(any_of(target_names))
        if (any(has_target)) {
          for (item in which(has_target)) {#****nested fors
            corrected_spellings[item] <- corrected_spellings[item] %>% 
              grem(pattern = word(any_of(target_names))) %>% 
              fix_spaces %>% 
              proper_noun_phrase
            WW2_bombs[["Target_City"]] %>% 
              replace_level(from = possible_spellings[item], to = corrected_spellings[item])
            relevant_data[["Target_City"]] %>% 
              replace_level(from = possible_spellings[item], to = corrected_spellings[item])
            possible_spellings[item] <- corrected_spellings[item]
          }
          possible_spellings <- unique(possible_spellings)
        }
        
        # and then prefer the modal name over other names
        if (length(possible_spellings) > 1L) {
          preferred_spelling <- mode_factor(relevant_data[["Target_City"]])
          other_spellings <- possible_spellings %[!=]% preferred_spelling
          WW2_bombs[["Target_City"]] %>% 
            replace_levels(from = other_spellings, to = preferred_spelling)
        }
      } else {
        ignored_similar_cities <- append(ignored_similar_cities, city)
      }
    } else {
      ignored_similar_cities <- append(ignored_similar_cities, city)
    }
  }
}
#WW2_similar_name_fixes <- copy(WW2_bombs)#***good through here


### find and fix latitude and longitude errors
debug_message("fixing lat-long problems")

WW2_loc <- unique(WW2_bombs[, .(Target_City, Target_Country, Target_Latitude, Target_Longitude)])
WW2_loc_by_coord <- WW2_loc[Target_City %!in% c("", known_duplicate_city_names) & 
                              !is.na(Target_Latitude) & 
                              !is.na(Target_Longitude), 
                            .(Target_City, 
                              Target_Country, 
                              'GRPN' = .N), 
                            keyby = .(Target_Latitude, Target_Longitude)]
WW2_locs_to_investigate <- WW2_loc_by_coord[GRPN > 1L, ]
WW2_unique_locs_to_investigate <- unique(WW2_locs_to_investigate[, .(Target_Latitude, Target_Longitude)])

# fix city and country by loc
for (loc_index in seq_len(nrow(WW2_unique_locs_to_investigate))) {
  lat <- WW2_unique_locs_to_investigate[loc_index, Target_Latitude]
  long <- WW2_unique_locs_to_investigate[loc_index, Target_Longitude]
  local_data <- WW2_bombs[near(Target_Latitude, lat, coord_buffer_tight) & #*** maybe reduce coord_buffer_tight to prevent dropping actual local cities
                            near(Target_Longitude, long, coord_buffer_tight), #*** also, do we even really need to edit cities or countries like this?
                          .(Target_City, Target_Country)]
  
  all_possible_cities <- levels(local_data[["Target_City"]][, drop = TRUE])#*** combine this and "" elimination step
  if (length(all_possible_cities) > 1L) {
    possible_cities <- all_possible_cities %[!=]% ""
    if (length(possible_cities) > 1L) {
      local_certain_cities <- local_data[Target_City != "", Target_City][, drop = TRUE]
      best_city <- mode_factor(local_certain_cities)
    } else {
      best_city <- possible_cities
    }
    WW2_bombs[Target_City != best_city & 
                near(Target_Latitude, lat, coord_buffer_tight) & 
                near(Target_Longitude, long, coord_buffer_tight), 
              Target_City := best_city]
  }
  
  all_possible_countries <- levels(local_data[["Target_Country"]][, drop = TRUE])#*** combine this and "" elimination step
  if (length(all_possible_countries) > 1L) {
    possible_countries <- all_possible_countries %[!=]% ""
    if (length(possible_countries) > 1L) {
      local_certain_countries <- local_data[Target_Country != "", Target_Country][, drop = TRUE]
      best_country <- mode_factor(local_certain_countries)
    } else {
      best_country <- possible_countries
    }
    WW2_bombs[Target_Country != best_country & 
                near(Target_Latitude, lat, coord_buffer_tight) & 
                near(Target_Longitude, long, coord_buffer_tight), 
              Target_Country := best_country]
  }
}
WW2_bombs %>% 
  drop_missing_levels_by_col(cols = c("Target_City", "Target_Country"))
#WW2_city_country_fixes <- copy(WW2_bombs)#***probably good through here


WW2_loc <- unique(WW2_bombs[, .(Target_City, Target_Country, Target_Latitude, Target_Longitude)])
WW2_loc_by_city <- WW2_loc[Target_City %!in% c("", known_duplicate_city_names) & 
                             !is.na(Target_Latitude) & 
                             !is.na(Target_Longitude), 
                           .(Target_Country, 
                             Target_Latitude, 
                             Target_Longitude, 
                             'GRPN' = .N, 
                             'lat_sd' = sd(Target_Latitude) %NA% 0, 
                             'long_sd' = sd(Target_Longitude) %NA% 0), 
                           keyby = Target_City]
#WW2_loc_by_city[, loc_sd := sqrt(lat_sd ** 2 + long_sd ** 2), by = Target_City]

# first lat errors (for lat corrections)
lat_error <- character(0L)
cities_to_investigate <- WW2_loc_by_city[lat_sd > lat_long_error_threshold_tight, 
                                         as.character(unique(Target_City))]
for (city in cities_to_investigate) {
  lats <- WW2_loc_by_city[Target_City == city & !is.na(Target_Latitude), unique(Target_Latitude)]
  lat_dists <- dist(lats)
  if (any(is_int(10*lat_dists, eps = lat_long_error_threshold_tight) & lat_dists != 0)) {
    lat_error <- append(lat_error, city)
    index <- which(is_int(10*lat_dists, eps = lat_long_error_threshold_tight) & lat_dists != 0)
    for (ind in index) {#****nested fors
      indices <- dist_indices(ind, length(lats))
      lats_in_question <- lats[c(indices[['i']], indices[['j']])]
      all_lats <- WW2_bombs[Target_City == city & Target_Latitude %in% lats_in_question, Target_Latitude]
      lat_sets <- mode_and_others_num(all_lats)
      other_lats <- lat_sets[['others']]
      mode_lat <- lat_sets[['mode']]
      WW2_bombs[Target_City == city & Target_Latitude %in% other_lats, 
                Target_Latitude := mode_lat]
    }
  }
}

# then long errors (for long corrections)
long_error <- character(0L)
cities_to_investigate <- WW2_loc_by_city[long_sd > lat_long_error_threshold_tight, 
                                         as.character(unique(Target_City))]
for (city in cities_to_investigate) {
  longs <- WW2_loc_by_city[Target_City == city & !is.na(Target_Longitude), unique(Target_Longitude)]
  long_dists <- dist(longs)
  if (any(is_int(10*long_dists, eps = lat_long_error_threshold_tight) & long_dists != 0)) {
    long_error <- append(long_error, city)
    index <- which(is_int(10*long_dists, eps = lat_long_error_threshold_tight) & long_dists != 0)
    for (ind in index) {#****nested fors
      indices <- dist_indices(ind, length(longs))
      longs_in_question <- longs[c(indices[['i']], indices[['j']])]
      all_longs <- WW2_bombs[Target_City == city & Target_Longitude %in% longs_in_question, Target_Longitude]
      long_sets <- mode_and_others_num(all_longs)
      other_longs <- long_sets[['others']]
      mode_long <- long_sets[['mode']]
      WW2_bombs[Target_City == city & Target_Longitude %in% other_longs, 
                Target_Longitude := mode_long]
    }
  }
}
#WW2_lat_long_fixes <- copy(WW2_bombs) #***


# then loc errors (for city corrections)
WW2_loc <- unique(WW2_bombs[, .(Target_City, Target_Country, Target_Latitude, Target_Longitude)])
WW2_loc_by_city <- WW2_loc[Target_City %!in% c("", known_duplicate_city_names) & 
                             !is.na(Target_Latitude) & 
                             !is.na(Target_Longitude), 
                           .(Target_Country, 
                             Target_Latitude, 
                             Target_Longitude, 
                             'GRPN' = .N, 
                             'lat_sd' = sd(Target_Latitude) %NA% 0, 
                             'long_sd' = sd(Target_Longitude) %NA% 0), 
                           keyby = Target_City]
WW2_loc_by_city[, loc_sd := sqrt(lat_sd ** 2 + long_sd ** 2), by = Target_City]

lat_long_error <- character(0L)
city_error <- character(0L)
unsure_error <- character(0L)
cities_to_investigate <- WW2_loc_by_city[loc_sd > lat_long_error_threshold | 
                                           (near(loc_sd, 0) & GRPN > 1L), 
                                         as.character(unique(Target_City))]
for (city in cities_to_investigate) {
  local_data <- unique(WW2_loc_by_city[Target_City == city, .(Target_Latitude, Target_Longitude)])
  lats <- local_data[, Target_Latitude]
  longs <- local_data[, Target_Longitude]
  lat_dists <- dist(lats)
  long_dists <- dist(longs)
  # see if any lats or longs exactly match other cities
  has_city_error <- FALSE
  for (i in seq_along(lats)) {#****nested fors
    lat <- lats[[i]]
    long <- longs[[i]]
    city_matches <- WW2_loc_by_city[Target_City != city & 
                                      Target_Latitude == lat & 
                                      Target_Longitude == long, 
                                    as.character(Target_City), 
                                    nomatch = 0L]
    country_matches <- WW2_loc_by_city[Target_Country != "" & 
                                         Target_Latitude == lat & 
                                         Target_Longitude == long, 
                                       as.character(Target_Country), 
                                       nomatch = 0L]
    if (!is_empty(city_matches) || !is_empty(country_matches)) {
      # probably a data entry error on the city #*** still need to solve this
      has_city_error <- TRUE
      if (!is_empty(city_matches)) {
        if (length(city_matches) == 1L) {
          best_city <- city_matches
        } else {
          local_cities <- WW2_bombs[Target_City %in% city_matches & 
                                      Target_Latitude == lat & 
                                      Target_Longitude == long, 
                                    Target_City][, drop = TRUE]
          best_city <- mode_factor(local_cities)
        }
        WW2_bombs[Target_City == city & 
                    Target_Latitude == lat & 
                    Target_Longitude == long, 
                  Target_City := best_city]
      }
      if (!is_empty(country_matches)) {
        if (length(country_matches) == 1L) {
          best_country <- country_matches
        } else {
          local_countries <- WW2_bombs[Target_Country %in% country_matches & 
                                         Target_Latitude == lat & 
                                         Target_Longitude == long, 
                                       Target_Country][, drop = TRUE]
          best_country <- mode_factor(local_countries)
        }
        WW2_bombs[Target_City == city & 
                    Target_Country != best_country & 
                    Target_Latitude == lat & 
                    Target_Longitude == long, 
                  Target_Country := best_country]
      }
    }
  }
  if (has_city_error) {
    city_error <- append(city_error, city)
  } else {
    unsure_error <- append(unsure_error, city)#*** not sure what to do here--these are just vaguely defined cities
  }
}
#WW2_city_loc_fixes <- copy(WW2_bombs) #****


### fix countries for cities with small variance in lat-longs
WW2_loc <- WW2_bombs[Target_City %!in% c("", known_duplicate_city_names) & 
                       !is.na(Target_Latitude) & 
                       !is.na(Target_Longitude), 
                     .(Target_City, Target_Country, Target_Latitude, Target_Longitude)]
cities_to_investigate <- unique(WW2_loc[, .(Target_City, Target_Country)])[, 
                                                                           .(Target_Country, 'GRPN' = .N), 
                                                                           keyby = Target_City][GRPN > 1L, 
                                                                                                as.character(unique(Target_City))]
sub_selection <- WW2_loc[Target_City %in% cities_to_investigate, ]
cities_to_investigate <- sub_selection[, .('sd' = sqrt((sd(Target_Latitude) %NA% 0) ** 2 + (sd(Target_Longitude) %NA% 0)) ** 2), 
                                       by = Target_City][sd < lat_long_error_threshold, 
                                                         as.character(Target_City)]
#print(cities_to_investigate)
for (city in cities_to_investigate) {
  best_country <- mode_factor(WW2_bombs[Target_City == city, Target_Country][, drop = TRUE])
  WW2_bombs[Target_City == city, Target_Country := best_country]
}
#WW2_country_loc_fixes <- copy(WW2_bombs) #****


# fix mistaken countries
debug_message("fixing country mistakes")

WW2_loc <- unique(WW2_bombs[, .(Target_Country, Target_Latitude, Target_Longitude)])
WW2_loc_by_coord <- WW2_loc[Target_Country != "" & 
                              !is.na(Target_Latitude) & 
                              !is.na(Target_Longitude), 
                            .(Target_Country), 
                            keyby = .(Target_Latitude, Target_Longitude)]

country_levels <- levels(WW2_bombs[["Target_Country"]])
recorded_countries <- unique(WW2_country_locs[["Target_Country"]])

mistaken_countries <- list()

for (i in seq_len(nrow(WW2_loc_by_coord))) {
  lat <- WW2_loc_by_coord[i, Target_Latitude]
  long <- WW2_loc_by_coord[i, Target_Longitude]
  country <- WW2_loc_by_coord[i, as.character(Target_Country)]
  
  dists <- distance_2D(numbers_x = WW2_country_locs[["mean_lat"]], 
                       numbers_y = WW2_country_locs[["mean_long"]], 
                       ref = c(lat, long)) / WW2_country_locs[["size"]]
  temp_table <- cbind(WW2_country_locs, data.table('dists' = dists))
  predicted_country <- temp_table[which.min(dists), Target_Country]# [["Target_Country"]][which.min(dists)]
  
  if (country != predicted_country) {
    country_ranking <- ranking(temp_table, 
                               sort_col = "dists", 
                               ref_col = "Target_Country", 
                               ref = country)
    rank_2_country <- which_has_ranking(temp_table, 
                                        sort_col = "dists", 
                                        ref_col = "Target_Country", 
                                        rank = 2L)
    
    predicted_country_dist <- temp_table[Target_Country == predicted_country, min(dists)]
    if (country %c% recorded_countries) {
      country_dist <- temp_table[Target_Country == country, min(dists)]
    } else {
      country_dist <- NA_real_
    }
    dist_ratio <- predicted_country_dist / country_dist
    
    # country is not in top 4 or predicted_country is way closer
    if (is.na(country_ranking) || country_ranking > 4L || dist_ratio < 0.5) {
      # change country immediately
      WW2_bombs[Target_Country == country & 
                  near(Target_Latitude, lat, lat_long_error_threshold_tight) & 
                  near(Target_Longitude, long, lat_long_error_threshold_tight), 
                Target_Country := predicted_country]
      mistaken_countries <- append(mistaken_countries, list(c(i, country, best_country)))
      # if country is ranked 2 or dists are too similar, ignore; otherwise, proceed below
    } else if (country_ranking > 2L && dist_ratio < 0.8) {
      # maybe change country based on other local finds as in below
      current_buffer <- coord_buffer
      country_matches <- WW2_bombs[!near(Target_Latitude, lat, coord_buffer_tight) & 
                                     !near(Target_Longitude, long, coord_buffer_tight) & 
                                     Target_Country != "" & 
                                     close_to(Target_Latitude, lat, current_buffer) & 
                                     close_to(Target_Longitude, long, current_buffer), 
                                   Target_Country, 
                                   nomatch = 0L][, drop = TRUE]
      while (length(country_matches) == 0L & current_buffer <= 4) {
        current_buffer <- current_buffer * 2
        country_matches <- WW2_bombs[!near(Target_Latitude, lat, coord_buffer_tight) & 
                                       !near(Target_Longitude, long, coord_buffer_tight) & 
                                       Target_Country != "" & 
                                       close_to(Target_Latitude, lat, current_buffer) & 
                                       close_to(Target_Longitude, long, current_buffer), 
                                     Target_Country, 
                                     nomatch = 0L][, drop = TRUE]
      }
      if (current_buffer <= 4 && !is_empty(country_matches)) {
        best_country <- mode_factor(country_matches)
        if (country != best_country) {
          mistaken_countries <- append(mistaken_countries, list(c(i, country, best_country)))
          WW2_bombs[Target_Country == country & 
                      near(Target_Latitude, lat, lat_long_error_threshold_tight) & 
                      near(Target_Longitude, long, lat_long_error_threshold_tight), 
                    Target_Country := best_country]
        }
      } else {
        # don't change it? #***
      }
    }
  }
}
WW2_bombs[["Target_Country"]] %>% 
  drop_levels(drop = country_levels %d% recorded_countries)
#WW2_mistaken_countries_fix <- copy(WW2_bombs) #***


### fill in missing countries
debug_message("filling in missing countries")

# part 1: by city (if close)
WW2_loc <- unique(WW2_bombs[Target_City != "" & 
                              !is.na(Target_Latitude) & 
                              !is.na(Target_Longitude), 
                            .(Target_City, Target_Country, Target_Latitude, Target_Longitude)])
cities_with_multiple_countries <- unique(WW2_loc[, .(Target_City, Target_Country)])[, .('GRPN' = .N), keyby = Target_City][GRPN > 1L, as.character(Target_City)]
cities_with_blank_country <- unique(WW2_loc[, .(Target_City, Target_Country)])[Target_Country == "", as.character(Target_City)]
cities_with_blank_and_non_black_countries <- cities_with_multiple_countries %i% cities_with_blank_country
WW2_country_by_city <- WW2_loc[, .(Target_Country, 
                                   'GRPN' = .N, 
                                   'lat_sd' = sd(Target_Latitude) %NA% 0, 
                                   'long_sd' = sd(Target_Longitude) %NA% 0), 
                               keyby = Target_City]
WW2_country_by_city[, loc_sd := sqrt(lat_sd ** 2 + long_sd ** 2), by = Target_City]

lat_long_error <- character(0L)
city_error <- character(0L)
unsure_error <- character(0L)
cities_to_investigate <- WW2_loc_by_city[loc_sd > lat_long_error_threshold | 
                                           (near(loc_sd, 0) & GRPN > 1L), 
                                         as.character(unique(Target_City))]
for (city in cities_to_investigate) {
  
}

# part 2: by location

WW2_blank_countries_by_coord <- unique(WW2_bombs[Target_Country == "" & 
                                                   !is.na(Target_Latitude) & 
                                                   !is.na(Target_Longitude), 
                                                 .(Target_Latitude, Target_Longitude)])

blank_countries <- list()

for (i in seq_len(nrow(WW2_blank_countries_by_coord))) {
  lat <- WW2_blank_countries_by_coord[i, Target_Latitude]
  long <- WW2_blank_countries_by_coord[i, Target_Longitude]
  dists <- distance_2D(numbers_x = WW2_country_locs[["mean_lat"]], numbers_y = WW2_country_locs[["mean_long"]], ref = c(lat, long)) / WW2_country_locs[["size"]]
  predicted_countries <- top_n_ranks(cbind(WW2_country_locs, data.table('dists' = dists)), "dists", "Target_Country", 3L)
  current_buffer <- coord_buffer
  country_matches <- WW2_bombs[!near(Target_Latitude, lat, coord_buffer_tight) & 
                                 !near(Target_Longitude, long, coord_buffer_tight) & 
                                 Target_Country != "" & 
                                 close_to(Target_Latitude, lat, current_buffer) & 
                                 close_to(Target_Longitude, long, current_buffer), 
                               Target_Country, 
                               nomatch = 0L][, drop = TRUE]
  while (length(country_matches) == 0L & current_buffer <= 4) {
    current_buffer <- current_buffer * 2
    country_matches <- WW2_bombs[!near(Target_Latitude, lat, coord_buffer_tight) & 
                                   !near(Target_Longitude, long, coord_buffer_tight) & 
                                   Target_Country != "" & 
                                   close_to(Target_Latitude, lat, current_buffer) & 
                                   close_to(Target_Longitude, long, current_buffer), 
                                 Target_Country, 
                                 nomatch = 0L][, drop = TRUE]
  }
  if (current_buffer > 4 || is_empty(country_matches) || best_country %!c% predicted_countries) {
    # pick #1 ranked predicted_country as best_country
    best_country <- WW2_country_locs[["Target_Country"]][which.min(dists)]
  } else {
    # pick local mode as best_country
    best_country <- mode_factor(country_matches)
  }
  blank_countries <- append(blank_countries, list(c(i, best_country)))
  WW2_bombs[Target_Country == "" & 
              near(Target_Latitude, lat, lat_long_error_threshold_tight) & 
              near(Target_Longitude, long, lat_long_error_threshold_tight), 
            Target_Country := best_country]
}
#WW2_empty_country_fix <- copy(WW2_bombs) #***


### fix cities

# fix spelling differences and OCR errors in (similarly-named) cities
debug_message("fixing similarly-spelled cities")

string_similarity_threshold <- 2L

countries <- levels(WW2_bombs$Target_Country)

for (country in countries) {#*** perhaps rewrite to look for nearby cities first, and then correct spellings if necessary
  country_data <- WW2_bombs[Target_Country == country, .(Target_City, Target_Latitude, Target_Longitude)]
  cities <- country_data[, as.character(unique(Target_City))] %[!=]% ""
  
  if (length(cities) > 1L) {
    city_string_similarity <- stringdistmatrix(cities, nthread = cores)
    are_cities_similar <- city_string_similarity <= string_similarity_threshold
    
    similar_city_list <- list()
    total_length <- length(cities)
    for (i in seq(total_length - 1L)) {#****nested fors
      city <- cities[i]
      col_indices <- dist_column_indices(i, total_length)
      similar_cities <- cities[-seq(from = 1L, to = i)][are_cities_similar[col_indices]]
      
      if (!is_empty(similar_cities)) {
        new_list <- list()
        new_list[[cities[i]]] <- similar_cities
        similar_city_list <- append(similar_city_list, new_list)
        
        # do stuff here instead
        mean_lat <- country_data[Target_City == city, mean(Target_Latitude, na.rm = TRUE)]
        mean_long <- country_data[Target_City == city, mean(Target_Longitude, na.rm = TRUE)]
        relevant_data <- country_data[Target_City %in% similar_cities & 
                                        close_to(Target_Latitude, mean_lat, coord_buffer) & 
                                        close_to(Target_Longitude, mean_long, coord_buffer), 
                                      nomatch = 0L]
        if (relevant_data[Target_City != city, .N] > 0L) {
          changed_similar_cities <- append(changed_similar_cities, city)
          city_sets <- mode_and_others_factor(relevant_data[["Target_City"]][, drop = TRUE])
          mode_city <- city_sets[['mode']]
          other_cities <- city_sets[['others']]
          exclusive <- WW2_bombs[Target_City %in% other_cities & 
                                   !close_to(Target_Latitude, mean_lat, coord_buffer) & 
                                   !close_to(Target_Longitude, mean_long, coord_buffer), .N == 0L]
          if (exclusive) {
            WW2_bombs[["Target_City"]] %>% 
              replace_levels(from = other_cities, to = mode_city)
          } else {
            WW2_bombs[Target_City %in% other_cities & 
                        close_to(Target_Latitude, mean_lat, coord_buffer) & 
                        close_to(Target_Longitude, mean_long, coord_buffer), 
                      Target_City := mode_city]
          }
        } else {
          ignored_similar_cities <- append(ignored_similar_cities, city)
        }
      }
    }
  }
}
#WW2_city_names_fix <- copy(WW2_bombs) #***


# correct mistaken cities #*** wait--this is redundant with fixing cities (and, unrelatedly, countries) by loc earlier, right?
# debug_message("correcting city name mistakes")
# 
# for (country in countries) {
#   country_data <- WW2_bombs[Target_Country == country, .(Target_City, Target_Latitude, Target_Longitude)]
#   cities <- country_data[, as.character(unique(Target_City))] %[!=]% ""
#   
# }


# fill in missing cities (if possible)
debug_message("filling in missing cities")

WW2_blank_cities_by_coord <- unique(WW2_bombs[Target_City == "" & 
                                                !is.na(Target_Latitude) & 
                                                !is.na(Target_Longitude), 
                                              .(Target_Country, Target_Latitude, Target_Longitude)])

blank_cities <- list()

for (i in seq_len(nrow(WW2_blank_cities_by_coord))) {
  country <- WW2_blank_cities_by_coord[i, Target_Country]
  lat <- WW2_blank_cities_by_coord[i, Target_Latitude]
  long <- WW2_blank_cities_by_coord[i, Target_Longitude]
  if (country == "") {
    city_matches <- WW2_bombs[!near(Target_Latitude, lat, coord_buffer_tight) & 
                                !near(Target_Longitude, long, coord_buffer_tight) & 
                                Target_City != "" & 
                                close_to(Target_Latitude, lat, coord_buffer) & 
                                close_to(Target_Longitude, long, coord_buffer), 
                              Target_City, 
                              nomatch = 0L][, drop = TRUE]
  } else {
    city_matches <- WW2_bombs[!near(Target_Latitude, lat, coord_buffer_tight) & 
                                !near(Target_Longitude, long, coord_buffer_tight) & 
                                Target_City != "" & 
                                Target_Country == country & 
                                close_to(Target_Latitude, lat, coord_buffer) & 
                                close_to(Target_Longitude, long, coord_buffer), 
                              Target_City, 
                              nomatch = 0L][, drop = TRUE]
  }
  if (!is_empty(city_matches)) {
    # update city to best_city
    best_city <- mode_factor(city_matches)
    best_country <- WW2_bombs[Target_City == best_city & 
                                near(Target_Latitude, lat, coord_buffer) & 
                                near(Target_Longitude, long, coord_buffer), 
                              as.character(unique(Target_Country))]
    # if (is_nonscalar(best_country)) {
    #   print(best_city)
    #   print(best_country)
    # }
    blank_cities <- append(blank_cities, list(c(i, best_city)))
    WW2_bombs[Target_City == "" & 
                near(Target_Latitude, lat, lat_long_error_threshold_tight) & 
                near(Target_Longitude, long, lat_long_error_threshold_tight), 
              `:=`(Target_City = best_city, 
                   Target_Country = best_country)]
  }
}


### fill in missing lats and longs with mean
WW2_loc <- unique(WW2_bombs[Target_City %!in% c("", known_duplicate_city_names), 
                            .(Target_City, Target_Country, Target_Latitude, Target_Longitude)])
blank_loc_cities <- WW2_loc[is.na(Target_Longitude) | is.na(Target_Latitude), 
                            as.character(Target_City)]
non_blank_loc_cities <- WW2_loc[!is.na(Target_Longitude) & !is.na(Target_Latitude), 
                                as.character(Target_City)]
cities_to_handle <- blank_loc_cities %i% non_blank_loc_cities

for (city in cities_to_handle) {
  local_data <- WW2_bombs[Target_City == city, .(Target_Latitude, Target_Longitude)]
  mean_lat <- local_data[, round(mean(Target_Latitude, na.rm = TRUE), digits = 1L)]
  mean_long <- local_data[, round(mean(Target_Longitude, na.rm = TRUE), digits = 1L)]
  WW2_bombs[Target_City == city & 
              is.na(Target_Latitude), 
            Target_Latitude := mean_lat]
  WW2_bombs[Target_City == city & 
              is.na(Target_Longitude), 
            Target_Longitude := mean_long]
}



### reset data.table index for WW2_bombs$Target_City
options(datatable.auto.index = previous_auto_index_option)
WW2_bombs[1, Target_City := WW2_bombs[1, Target_City]]
WW2_bombs[1, Target_Country := WW2_bombs[1, Target_Country]]
options(datatable.use.index = previous_use_index_option)

### end of experimentation
