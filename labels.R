# @author Scott Dobbins
# @version 0.9.9.3
# @date 2017-11-07 19:30


### Overview Tab ------------------------------------------------------------

# war tags
WW1 <- "WW1"
WW2 <- "WW2"
Korea <- "Korea"
Vietnam <- "Vietnam"
war_tags <- c(WW1, WW2, Korea, Vietnam)

# war labels
WW1_label = "World War I (1914-1918)"
WW2_label = "World War II (1939-1945)"
Korea_label = "Korean War (1950-1953)"
Vietnam_label = "Vietnam War (1955-1975)"
war_labels <- c(WW1_label, WW2_label, Korea_label, Vietnam_label)

# war data tags
war_data_tags <- c("WW1", "WW2", "Korea1", "Korea2", "Vietnam")


### Mission Dates -----------------------------------------------------------

war_first_missions <- c(WW1_first_mission, WW2_first_mission, Korea_first_mission, Vietnam_first_mission)
war_last_missions <- c(WW1_last_mission, WW2_last_mission, Korea_last_mission, Vietnam_last_mission)


### Data Tab ----------------------------------------------------------------

# colors
war_color <- c(WW1_color, WW2_color, Korea_color, Vietnam_color)
war_background <- c(WW1_background, WW2_background, Korea_background, Vietnam_background)

# columns
WW1_datatable_columns <-     c("Mission_Date", "Unit_Country", "Target_Country", "Target_City", "Target_Type", "Aircraft_Type", "Aircraft_Attacking_Num", "Weapon_Type", "Weapon_Expended_Num", "Weapon_Weight_Pounds")
WW2_datatable_columns <-     c("Mission_Date", "Unit_Country", "Target_Country", "Target_City", "Target_Type", "Aircraft_Type", "Aircraft_Attacking_Num", "Weapon_Type", "Weapon_Expended_Num", "Weapon_Weight_Pounds")
Korea_datatable_columns <-   c("Mission_Date", "Unit_Country",                   "Target_City", "Target_Type", "Aircraft_Type", "Aircraft_Attacking_Num", "Weapon_Type", "Weapon_Expended_Num", "Weapon_Weight_Pounds")
Vietnam_datatable_columns <- c("Mission_Date", "Unit_Country", "Target_Country",                "Target_Type", "Aircraft_Type", "Aircraft_Attacking_Num", "Weapon_Type", "Weapon_Expended_Num", "Weapon_Weight_Pounds")
war_datatable_columns <- list(WW1_datatable_columns, WW2_datatable_columns, Korea_datatable_columns, Vietnam_datatable_columns)

# column names
WW1_datatable_colnames <-     c("Date", "Airforce", "Target Country", "Target City", "Target", "Aircraft", "# of Aircraft", "Weapon", "# of Weapons", "Explosives (lbs)")
WW2_datatable_colnames <-     c("Date", "Airforce", "Target Country", "Target City", "Target", "Aircraft", "# of Aircraft", "Weapon", "# of Weapons", "Explosives (lbs)")
Korea_datatable_colnames <-   c("Date", "Airforce",                   "Target City", "Target", "Aircraft", "# of Aircraft", "Weapon", "# of Weapons", "Explosives (lbs)")
Vietnam_datatable_colnames <- c("Date", "Airforce", "Target Country",                "Target", "Aircraft", "# of Aircraft", "Weapon", "# of Weapons", "Explosives (lbs)")
war_datatable_colnames <- list(WW1_datatable_colnames, WW2_datatable_colnames, Korea_datatable_colnames, Vietnam_datatable_colnames)


### WW1 ---------------------------------------------------------------------

WW1_categorical = list("Operation Supported" = "Operation", 
                       "Military Regiment" = "Unit_Service", 
                       "Country of Origin" = "Unit_Country", 
                       "Target Country" = "Target_Country", 
                       "Target City" = "Target_City", 
                       "Target Type" = "Target_Category", 
                       "Aircraft Model" = "Aircraft_Type", 
                       "Bomb Type" = "Weapon_Type", 
                       "Takeoff Time of Day" = "Takeoff_Day_Period", 
                       "Takeoff Base" = "Takeoff_Base", 
                       "Visibility" = "Target_Visibility", 
                       "Year" = "Year", 
                       "Month" = "Month_name")
WW1_categorical_choices = names(WW1_categorical)

WW1_continuous = list("Number of Attacking Aircraft" = "Aircraft_Attacking_Num", 
                      "Altitude at Bomb Drop" = "Bomb_Altitude_Feet", 
                      "Number of Bombs Dropped" = "Weapon_Expended_Num", 
                      "Weight of Bombs Dropped" = "Weapon_Weight_Pounds", 
                      "Bombload (weight of bombs per plane)" = "Aircraft_Bombload_Pounds", 
                      "Number of Aircraft Lost/Destroyed" = "Casualties_Friendly")
WW1_continuous_choices = names(WW1_continuous)

WW1_all_choices <- c(WW1_categorical_choices, WW1_continuous_choices)


### WW2 ---------------------------------------------------------------------

WW2_categorical = list("Theater of Operations" = "Mission_Theater", 
                       "Military Regiment" = "Unit_Service", 
                       "Country of Origin" = "Unit_Country", 
                       "Target Country" = "Target_Country", 
                       "Target City" = "Target_City", 
                       "Target Type" = "Target_Category", 
                       "Target Industry" = "Target_Industry", 
                       "Aircraft Model" = "Aircraft_Type", 
                       "Target Priority" = "Target_Priority", 
                       "Bomb Type" = "Weapon_Type", 
                       "Bomb Class" = "Weapon_Class", 
                       "Takeoff Country" = "Takeoff_Country", 
                       "Takeoff Base" = "Takeoff_Base", 
                       "Sighting Method" = "Sighting_Method", 
                       "Year" = "Year", 
                       "Month" = "Month_name")
WW2_categorical_choices = names(WW2_categorical)

WW2_continuous = list("Number of Attacking Aircraft" = "Aircraft_Attacking_Num", 
                      "Altitude at Bomb Drop" = "Bomb_Altitude_Feet", 
                      "Number of Bombs Dropped" = "Weapon_Expended_Num", 
                      "Weight of Bombs Dropped" = "Weapon_Weight_Pounds", 
                      "Number of Aircraft Lost/Destroyed" = "Aircraft_Lost_Num", 
                      "Number of Aircraft Damaged" = "Aircraft_Damaged_Num")
WW2_continuous_choices = names(WW2_continuous)

WW2_all_choices <- c(WW2_categorical_choices, WW2_continuous_choices)


### Korea -------------------------------------------------------------------

Korea_categorical = list("Military Division" = "Unit_Squadron", 
                         "Mission Type" = "Mission_Type", 
                         "Target City" = "Target_City", 
                         "Target Type" = "Target_Category", 
                         "Aircraft Model" = "Aircraft_Type", 
                         "Bomb Type" = "Weapon_Type", 
                         "Sighting Method" = "Bomb_Sighting_Method", 
                         "Nose Fuze" = "Nose_Fuze", 
                         "Tail Fuze" = "Tail_Fuze", 
                         "Year" = "Year", 
                         "Month" = "Month_name")
Korea_categorical_choices = names(Korea_categorical)

Korea_continuous = list("Number of Attacking Aircraft" = "Aircraft_Attacking_Num", 
                        "Altitude at Bomb Drop" = "Bomb_Altitude_Feet", 
                        "Number of Bombs Dropped" = "Weapon_Num", 
                        "Weight of Bombs Dropped" = "Weapon_Weight_Pounds", 
                        "Bombload (weight of bombs per plane)" = "Aircraft_Bombload_Calculated_Pounds", 
                        "Number of Aircraft Lost" = "Aircraft_Lost_Num", 
                        "Number of Aircraft Aborted" = "Aircraft_Aborted_Num")
Korea_continuous_choices = names(Korea_continuous)

Korea_all_choices <- c(Korea_categorical_choices, Korea_continuous_choices)


### Vietnam -----------------------------------------------------------------

Vietnam_categorical = list("Operation Supported" = "Operation", 
                           "Military Regiment" = "Unit_Service", 
                           "Country of Origin" = "Unit_Country", 
                           "Target Country" = "Target_Country", 
                           "Target Type" = "Target_Category", 
                           "Aircraft Model" = "Aircraft_Type", 
                           "Bomb Type" = "Weapon_Type", 
                           "Bomb Class" = "Weapon_Class", 
                           "Takeoff City" = "Takeoff_Location", 
                           "Takeoff Time of Day" = "Mission_Day_Period", 
                           "Target Control" = "Target_Control", 
                           "Target Visibility" = "Target_Visibility", 
                           "Year" = "Year", 
                           "Month" = "Month_name")
Vietnam_categorical_choices = names(Vietnam_categorical)

Vietnam_continuous = list("Number of Attacking Aircraft" = "Aircraft_Attacking_Num", 
                          "Altitude at Bomb Drop" = "Bomb_Altitude", 
                          "Number of Bombs Dropped" = "Weapon_Expended_Num", 
                          "Number of Bombs Jettisoned" = "Weapon_Jettisoned_Num", 
                          "Number of Bombs Returned" = "Weapon_Returned_Num", 
                          "Flight Hours" = "Flight_Hours")
Vietnam_continuous_choices = names(Vietnam_continuous)

Vietnam_all_choices <- c(Vietnam_categorical_choices, Vietnam_continuous_choices)


### Binnings ----------------------------------------------------------------

# histogram sliders
war_init_bins <- c(WW1_init_bins, WW2_init_bins, Korea_init_bins, Vietnam_init_bins)
war_min_bins <- c(WW1_min_bins, WW2_min_bins, Korea_min_bins, Vietnam_min_bins)
war_max_bins <- c(WW1_max_bins, WW2_max_bins, Korea_max_bins, Vietnam_max_bins)


### Videos ------------------------------------------------------------------

WW1_video <- "dwrIf_5gEEM"
WW2_video <- "PIYVwqHM488"
Korea_video <- "u9Avsd3O3Bs"
Vietnam_video <- "HvDz4MrYXNc"
war_videos <- c(WW1_video, WW2_video, Korea_video, Vietnam_video)

WW1_video_description <- "footage of World War One biplanes"
WW2_video_description <- "a World War Two era training video on how to avoid flak"
Korea_video_description <- "a short film on the role of Tactical Air Forces in the Korean War"
Vietnam_video_description <- "a pre-Vietnam War training video on dogfighting tactics"
war_video_descriptions <- c(WW1_video_description, WW2_video_description, Korea_video_description, Vietnam_video_description)
war_video_description_phrases <- paste0("Select ", war_tags, " in the sidebar to see ", war_video_descriptions)

pilot_text_id <- paste0("pilot_text_", war_tags)
pilot_text_box_id <- paste0("pilot_text_box_", war_tags)
pilot_video_id <- paste0("pilot_video_", war_tags)
pilot_video_box_id <- paste0("pilot_video_box_", war_tags)


### Maps --------------------------------------------------------------------

WW1_maps <- c("https://infographics.economist.com/2014/1914-19Swiper/img/20140802_1914.png", 
              "http://www.personal.psu.edu/users/u/x/uxa100/World%20History%202/WW1%20hostilities%20map.jpg")
WW2_maps <- c("https://www.maps.com/media/catalog/product/cache/1/thumbnail/2500x/17f82f742ffe127f42dca9de82fb58b1/5/1/510002_wwii_europe_im_z.jpg", 
              "http://www.emersonkent.com/images/wwii_asia.jpg")
Korea_maps <- c("https://i.pinimg.com/originals/19/37/31/1937310ff5c1f3b3e6d7b9b2fac808da.jpg", 
                "https://i.pinimg.com/originals/64/f6/50/64f65082c44aea18e1af296c9f3ce345.jpg")
Vietnam_maps <- c("https://i.pinimg.com/originals/8d/10/45/8d1045cf572f4bd715c6032f373d6568.jpg", 
                  "http://www.socialstudies.com/itemimages/large/NYS3129.jpg")
war_maps <- list(WW1_maps, WW2_maps, Korea_maps, Vietnam_maps)

WW1_maps_description <- "World War One"
WW2_maps_description <- "World War Two Theaters of Operations"
Korea_maps_description <- "Korean War"
Vietnam_maps_description <- "Vietnam War"
war_maps_descriptions <- c(WW1_maps_description, WW2_maps_description, Korea_maps_description, Vietnam_maps_description)
war_maps_description_phrases <- paste0("Select ", war_tags, " in the sidebar to see ", war_maps_descriptions)

commander_text_id <- paste0("commander_text_", war_tags)
commander_text_box_id <- paste0("commander_text_box_", war_tags)
commander_maps_ids <- list()
commander_maps_box_id <- paste0("commander_maps_box_", war_tags)
commander_maps_ids <- list(WW1     = paste0("commander_map_", WW1, c("_1", "_2")), 
                           WW2     = paste0("commander_map_", WW2, c("_1", "_2")), 
                           Korea   = paste0("commander_map_", Korea, c("_1", "_2")), 
                           Vietnam = paste0("commander_map_", Vietnam, c("_1", "_2")))


### Lookup Tables -----------------------------------------------------------

war_categorical <- list(WW1_categorical, WW2_categorical, Korea_categorical, Vietnam_categorical)
war_categorical_choices <- list(WW1_categorical_choices, WW2_categorical_choices, Korea_categorical_choices, Vietnam_categorical_choices)
war_continuous <- list(WW1_continuous, WW2_continuous, Korea_continuous, Vietnam_continuous)
war_continuous_choices <- list(WW1_continuous_choices, WW2_continuous_choices, Korea_continuous_choices, Vietnam_continuous_choices)
war_all_choices <- list(WW1_all_choices, WW2_all_choices, Korea_all_choices, Vietnam_all_choices)


### Graphs ------------------------------------------------------------------

WW1_histogram_title <- "World War One Histogram"
WW2_histogram_title <- "World War Two Histogram"
Korea_histogram_title <- "Korean War Histogram"
Vietnam_histogram_title <- "Vietnam War Histogram"
war_histogram_title <- c(WW1_histogram_title, WW2_histogram_title, Korea_histogram_title, Vietnam_histogram_title)

WW1_sandbox_title <- "World War One Sandbox"
WW2_sandbox_title <- "World War Two Sandbox"
Korea_sandbox_title <- "Korean War Sandbox"
Vietnam_sandbox_title <- "Vietnam War Sandbox"
war_sandbox_title <- c(WW1_sandbox_title, WW2_sandbox_title, Korea_sandbox_title, Vietnam_sandbox_title)

war_hist_ids <- paste0("hist_", war_tags)
war_sandbox_ids <- paste0("sandbox_", war_tags)

war_sandbox_group_ids <- paste0(war_tags, "_sandbox_group")
war_sandbox_ind_ids <- paste0(war_tags, "_sandbox_ind")
war_sandbox_dep_ids <- paste0(war_tags, "_sandbox_dep")
war_hist_slider_ids <- paste0(war_tags, "_hist_slider")
war_transformation_hor_ids <- paste0(war_tags, "_transformation_hor")
war_transformation_ver_ids <- paste0(war_tags, "_transformation_ver")


### Map Layers --------------------------------------------------------------

WW1_overview <- "WW1_overview"
WW2_overview <- "WW2_overview"
Korea_overview <- "Korea_overview"
Vietnam_overview <- "Vietnam_overview"
war_overview <- c(WW1_overview, WW2_overview, Korea_overview, Vietnam_overview)

WW1_civilian <- "WW1_civilian"
WW2_civilian <- "WW2_civilian"
Korea_civilian <- "Korea_civilian"
Vietnam_civilian <- "Vietnam_civilian"
war_civilian <- c(WW1_civilian, WW2_civilian, Korea_civilian, Vietnam_civilian)


### Set Names ---------------------------------------------------------------

walk(list(war_labels, 
          war_color, 
          war_background, 
          war_first_missions, 
          war_last_missions, 
          war_datatable_columns, 
          war_datatable_colnames, 
          war_init_bins, 
          war_min_bins, 
          war_max_bins, 
          war_videos, 
          war_video_descriptions, 
          war_video_description_phrases, 
          war_maps, 
          war_maps_descriptions, 
          war_maps_description_phrases, 
          pilot_text_id, 
          pilot_text_box_id, 
          pilot_video_id, 
          pilot_video_box_id, 
          commander_text_id, 
          commander_text_box_id, 
          commander_maps_box_id, 
          war_categorical, 
          war_categorical_choices, 
          war_continuous, 
          war_continuous_choices, 
          war_all_choices, 
          war_histogram_title, 
          war_sandbox_title, 
          war_hist_ids, 
          war_sandbox_ids, 
          war_sandbox_group_ids, 
          war_sandbox_ind_ids, 
          war_sandbox_dep_ids, 
          war_hist_slider_ids, 
          war_transformation_hor_ids, 
          war_transformation_ver_ids, 
          war_overview, 
          war_civilian), 
     ~re_name(., war_tags))


### Dropdowns ---------------------------------------------------------------

dropdowns <- list(regions   = "Target_Country", 
                  targets   = "Target_Category", 
                  countries = "Unit_Country", 
                  aircrafts = "Aircraft_Type", 
                  weapons   = "Weapon_Type")
dropdown_tags <- names(dropdowns)
