# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


### Setup -------------------------------------------------------------------

bomb_data <- list(WW1_bombs, 
                  WW2_bombs, 
                  Korea_bombs1, 
                  Korea_bombs2, 
                  Vietnam_bombs)
setattr(bomb_data, "names", war_data_tags)


### Fix Dates ---------------------------------------------------------------

debug_message("cleaning date columns")

WW1_bombs[,     c("Year", "Month", "Day") := tstrsplit(Mission_Date, "-", fixed = TRUE, keep = 1:3)]
WW2_bombs[,     c("Month", "Day", "Year") := tstrsplit(Mission_Date, "/", fixed = TRUE, keep = 1:3)]
Korea_bombs1[,  c("Month", "Day", "Year") := tstrsplit(Mission_Date, "/", fixed = TRUE, keep = 1:3)]
Korea_bombs2[,  c("Month", "Day", "Year") := tstrsplit(Mission_Date, "/", fixed = TRUE, keep = 1:3)]
Vietnam_bombs[, c("Year", "Month", "Day") := tstrsplit(Mission_Date, "-", fixed = TRUE, keep = 1:3)]

Vietnam_bombs[is.na(Month), 
              `:=`(Day = substr(Year, 7, 8),
                   Month = substr(Year, 5, 6), 
                   Year = substr(Year, 1, 4))]

Vietnam_bombs[is.na(Mission_Date), 
              `:=`(Mission_Date = paste(Year, Month, Day, sep = "-"))]

cols <- c("Year", "Month", "Day")
walk(bomb_data, 
     function(dt) dt[, (cols) := lapply(.SD, ordered %.% as.integer), .SDcols = cols])

Korea_bombs1[["Year"]] %>% 
  format_levels(function(YY) paste0("19", YY))
Korea_bombs2[["Year"]] %>% 
  format_levels(function(YY) paste0("19", YY))

walk(bomb_data, 
     function(dt) dt[, Month_name := Month][["Month_name"]] %>% 
       format_levels(month_num_to_name))

WW1_bombs[,     Mission_Date := ymd(Mission_Date)]
WW2_bombs[,     Mission_Date := mdy(Mission_Date)]
Korea_bombs1[,  Mission_Date := mdy(Mission_Date) - years(100)]
Korea_bombs2[,  Mission_Date := mdy(Mission_Date) - years(100)]
Vietnam_bombs[, Mission_Date := ymd(Mission_Date)]


### Set Keys ----------------------------------------------------------------

debug_message("setting keys")

walk(bomb_data, 
     ~setkeyv(., "Mission_Date"))


### WW1 Edits ---------------------------------------------------------------

debug_message("cleaning WW1")

# temporary type fixes
cols <- c("ID", 
          "Mission_Num", 
          "Aircraft_Attacking_Num", 
          "Weapon_Expended_Num", 
          "Num_Aircraft_Lost", 
          "Bomb_Altitude_Feet")
WW1_bombs[, (cols) := lapply(.SD, as.integer), .SDcols = cols]

# general fixes, numerics
cols <- c("Aircraft_Bombload_Pounds", 
          "Weapon_Weight_Pounds")
WW1_bombs[, (cols) := lapply(.SD, round_to_int), .SDcols = cols]

# specific fixes, numerics
WW1_bombs[Aircraft_Attacking_Num == 0L | Aircraft_Attacking_Num >= 99L, 
          `:=`(Aircraft_Attacking_Num = NA_integer_)]

WW1_bombs[Weapon_Expended_Num == 0L, 
          `:=`(Weapon_Expended_Num = NA_integer_)]

WW1_bombs[Weapon_Weight_Pounds == 0L, 
          `:=`(Weapon_Weight_Pounds = NA_integer_)]

WW1_bombs[Bomb_Altitude_Feet == 0L, 
          `:=`(Bomb_Altitude_Feet = NA_integer_)]
WW1_bombs[Bomb_Altitude_Feet > WW1_altitude_max_feet, 
          `:=`(Bomb_Altitude_Feet = WW1_altitude_max_feet)]

WW1_bombs[Target_Latitude %!between% c(-90, 90) | Target_Latitude == 0, 
          `:=`(Target_Latitude = NA_real_)]
WW1_bombs[Target_Longitude %!between% c(-180, 180) | Target_Longitude == 0, 
          `:=`(Target_Longitude = NA_real_)]

# general fixes, times
WW1_bombs[["Takeoff_Time"]] %>% 
  format_levels(substr, 12, 16)

# standard formatting
WW1_bombs %>% keep(is.factor) %>% discard(is.ordered) %>% 
  format_levels_by_col(remove_bad_formatting)

# editing string levels
WW1_bombs[["Unit_Country"]] %>% 
  format_levels(proper_noun_phrase_vectorized)

WW1_bombs[["Unit_Squadron"]] %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c("GROUP"    = "GRP", 
                                    "SQUADRON" = "SQDN", 
                                                 "FRENCH ")) %>% 
  format_levels(proper_noun_phrase_vectorized)

WW1_bombs[["Unit_Service"]] %>% 
  format_levels(proper_noun_phrase_vectorized)

WW1_bombs[, Unit_Title := Unit_Service][["Unit_Title"]] %>% 
  rename_levels(changes = c("US Army"             = "Army", 
                            "US Navy"             = "Navy", 
                            "US Army Air Service" = "USAAS", 
                            "French GAR"          = "GAR", 
                            "UK Royal Air Force"  = "RAF"))
WW1_bombs[, Unit_Title := factor(trimws(paste(Unit_Title, Unit_Squadron)))]

WW1_bombs[["Weapon_Type"]] %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c(" KG" = " KILO")) %>% 
  rename_similar_levels(changes = weapon_rules) %>% 
  format_levels(weapon_phrase_vectorized)

WW1_bombs[["Target_City"]] %>% 
  drop_levels(drop = "OTHER") %>% 
  format_levels(remove_parentheticals) %>% 
  rename_similar_levels(changes = target_city_rules) %>% 
  rename_similar_levels(changes = and_preceding(word(any_of(English_prepositions_for_place_names_upper)))) %>% 
  format_levels(proper_noun_phrase_vectorized)

WW1_bombs[["Operation"]] %>% 
  drop_levels(drop = "WW I") %>% 
  format_levels(proper_noun_phrase_vectorized)

WW1_bombs[["Target_Type"]] %>% 
  format_levels(remove_parentheticals) %>% 
  rename_similar_levels(changes = target_rules) %>% 
  format_levels(tolower) %>% 
  format_levels(fix_invalid_words, 
                dictionary = dictionary_20k_plus, 
                exclude = c(measurement_units, "unidentified", "unknown"))

WW1_bombs[["Route_Details"]] %>% 
  drop_levels(drop = "NONE")

WW1_bombs %>% select("Takeoff_Day_Period", 
                     "Target_Weather") %>% 
  format_levels_by_col(tolower)

WW1_bombs[["Target_Country"]] %>% 
  format_levels(proper_noun_phrase_vectorized)

WW1_bombs %>% select("Route_Details", 
                     "Takeoff_Base") %>% 
  format_levels_by_col(proper_noun_phrase_vectorized)

WW1_bombs[["Aircraft_Type"]] %>% 
  format_levels(format_aircraft_types %,% proper_noun_phrase_aircraft_vectorized)

# fill out missing countries by service
WW1_bombs %>% 
  fill_matching_values(code_col = "Unit_Service", value_col = "Unit_Country", drop.missing.levels = TRUE)

# fill out missing countries by city
WW1_bombs %>% 
  fill_matching_values(code_col = "Target_City", value_col = "Target_Country", drop.missing.levels = TRUE)

# reduced columns
WW1_bombs[, Target_Category := Target_Type][["Target_Category"]] %>% 
  reduce_levels(rules = target_categorizations)
WW1_bombs[, Target_Category := ordered_empty_at_end(Target_Category, "other")]

WW1_bombs[, Target_Visibility := Target_Weather][["Target_Visibility"]] %>% 
  reduce_levels(rules = visibility_categorizations, other = "fair")
WW1_bombs[, Target_Visibility := ordered(Target_Visibility, levels = c("poor", "fair", "good"))]


### WW2 Edits ---------------------------------------------------------------

debug_message("cleaning WW2")

# temporary type fixes
cols <- c("Target_Country_Code", 
          "Target_City_Code", 
          "Target_Industry_Code", 
          "Bomb_Altitude_Feet", 
          "Weapon_Weight_Pounds", 
          "Aircraft_Attacking_Num", 
          "Aircraft_Lost_Num", 
          "Aircraft_Damaged_Num", 
          "Aircraft_Dropping_Num", 
          "Aircraft_Spares_Num", 
          "Aircraft_Fail_WX_Num", 
          "Aircraft_Fail_Mech_Num", 
          "Aircraft_Fail_Misc_Num")
WW2_bombs[, (cols) := lapply(.SD, as.integer), .SDcols = cols]

# quick fixes
WW2_bombs %>% 
  drop_levels_by_col(drop = "0", 
                     cols = c("Weapon_Expl_Type", 
                              "Weapon_Incd_Type", 
                              "Weapon_Frag_Type"))

# new columns
WW2_bombs[, Weapon_Expl_Unit_Weight := grem(Weapon_Expl_Type, pattern = "\\D[ -~]*")]
WW2_bombs[, Weapon_Expl_Unit_Weight := as.integer(if_else(Weapon_Expl_Unit_Weight %like% "\\d+", 
                                                          Weapon_Expl_Unit_Weight, 
                                                          NA_character_))]

WW2_bombs[, Weapon_Incd_Unit_Weight := grem(Weapon_Incd_Type, pattern = "\\D[ -~]*")]
WW2_bombs[, Weapon_Incd_Unit_Weight := as.integer(if_else(Weapon_Incd_Unit_Weight %like% "\\d+", 
                                                          Weapon_Incd_Unit_Weight, 
                                                          NA_character_))]

WW2_bombs[, Weapon_Frag_Unit_Weight := grem(Weapon_Frag_Type, pattern = "\\D[ -~]*")]
WW2_bombs[, Weapon_Frag_Unit_Weight := as.integer(if_else(Weapon_Frag_Unit_Weight %like% "\\d+", 
                                                          Weapon_Frag_Unit_Weight, 
                                                          NA_character_))]

# column error fixes
WW2_bombs[grepl(pattern = ":", Unit_Squadron), 
          `:=`(Bomb_Time = Unit_Squadron, 
               Unit_Squadron = "")]
drop_similar_levels(WW2_bombs[["Unit_Squadron"]], drop = ":", exact = TRUE)

bad_city_levels <- unique(WW2_bombs[Target_City_Code >= 90000L, Target_City])
WW2_bombs[Target_City_Code >= 90000L, 
          `:=`(Target_City_Code = NA_integer_, 
               Target_City = "")]
drop_levels(WW2_bombs[["Target_City"]], drop = bad_city_levels)

WW2_bombs[Sighting_Method_Code == "PFF", 
          `:=`(Sighting_Method_Code = "", 
               Sighting_Method = "PFF")]
WW2_bombs[Sighting_Method_Code == "VISUAL", 
          `:=`(Sighting_Method_Code = "1", 
               Sighting_Method = "VISUAL")]
WW2_bombs[, Sighting_Method_Code := as.integer(Sighting_Method_Code)]


### non-integer numbers of weapons

WW2_bombs[!is_int(Weapon_Expl_Num) & 
            Weapon_Expl_Type %exactlylike% "325 LB", 
          `:=`(Weapon_Expl_Num = round(Weapon_Expl_Tons * 1840 / 230), 
               Weapon_Expl_Pounds = round(Weapon_Expl_Tons * 1840), 
               Weapon_Expl_Tons = Weapon_Expl_Tons * 1840 / 2000, 
               Weapon_Expl_Unit_Weight = 230L)]

WW2_bombs[!is_int(Weapon_Expl_Num) & 
            Weapon_Expl_Type %exactlylike% "500 LB", 
          `:=`(Weapon_Expl_Num = round(Weapon_Expl_Tons * 1950 / 325), 
               Weapon_Expl_Pounds = round(Weapon_Expl_Tons * 1950), 
               Weapon_Expl_Tons = Weapon_Expl_Tons * 1950 / 2000, 
               Weapon_Expl_Unit_Weight = 325L)]

WW2_bombs[!is_int(Weapon_Expl_Num) & 
            Weapon_Expl_Type %exactlylike% "1000 LB", 
          `:=`(Weapon_Expl_Num = round(Weapon_Expl_Tons * 1950 / 650), 
               Weapon_Expl_Pounds = round(Weapon_Expl_Tons * 1950), 
               Weapon_Expl_Tons = Weapon_Expl_Tons * 1950 / 2000, 
               Weapon_Expl_Unit_Weight = 650L)]

WW2_bombs[!is_int(Weapon_Expl_Num) & 
            Weapon_Expl_Type %exactlylike% "4000 LB", 
          `:=`(Weapon_Expl_Num = round(Weapon_Expl_Tons * 2000 / 3400), 
               Weapon_Expl_Pounds = round(Weapon_Expl_Tons * 2000 / 3400) * 3250, 
               Weapon_Expl_Tons = round(Weapon_Expl_Tons * 2000 / 3400) * 3250 / 2000, 
               Weapon_Expl_Unit_Weight = 3250L)]

WW2_bombs[!is_int(Weapon_Frag_Num) & 
            Weapon_Frag_Type %exactlylike% "23 LB", 
          `:=`(Weapon_Frag_Num = round(Weapon_Frag_Tons * 1840 / 23), 
               Weapon_Frag_Pounds = round(Weapon_Frag_Tons * 1840), 
               Weapon_Frag_Tons = Weapon_Frag_Tons * 1840 / 2000)]

WW2_bombs[!is_int(Weapon_Frag_Num) & 
            Weapon_Frag_Type %exactlylike% "90 LB", 
          `:=`(Weapon_Frag_Num = round(Weapon_Frag_Tons * 2160 / 90), 
               Weapon_Frag_Pounds = round(Weapon_Frag_Tons * 2160), 
               Weapon_Frag_Tons = Weapon_Frag_Tons * 2160 / 2000)]

WW2_bombs[!is_int(Weapon_Frag_Num) & 
            Weapon_Frag_Type %exactlylike% "96 LB", 
          `:=`(Weapon_Frag_Num = round(Weapon_Frag_Tons * 1920 / 96), 
               Weapon_Frag_Pounds = round(Weapon_Frag_Tons * 1920), 
               Weapon_Frag_Tons = Weapon_Frag_Tons * 1920 / 2000)]

WW2_bombs[!is_int(Weapon_Frag_Num) & 
            Weapon_Frag_Type %exactlylike% "120 LB", 
          `:=`(Weapon_Frag_Num = round(Weapon_Frag_Tons * 1920 / 120), 
               Weapon_Frag_Pounds = round(Weapon_Frag_Tons * 1920), 
               Weapon_Frag_Tons = Weapon_Frag_Tons * 1920 / 2000)]

WW2_bombs[!is_int(Weapon_Frag_Num) & 
            Weapon_Frag_Type %exactlylike% "260 LB", 
          `:=`(Weapon_Frag_Num = round(Weapon_Frag_Tons * 2080 / 260), 
               Weapon_Frag_Pounds = round(Weapon_Frag_Tons * 2080), 
               Weapon_Frag_Tons = Weapon_Frag_Tons * 2080 / 2000)]

WW2_bombs[!is_int(Weapon_Frag_Num) & 
            Weapon_Frag_Type %exactlylike% "360 LB", 
          `:=`(Weapon_Frag_Num = round(Weapon_Frag_Tons * 2160 / 360), 
               Weapon_Frag_Pounds = round(Weapon_Frag_Tons * 2160), 
               Weapon_Frag_Tons = Weapon_Frag_Tons * 2160 / 2000)]

WW2_bombs[!is_int(Weapon_Frag_Num) & 
            Weapon_Frag_Type %exactlylike% "540 LB", 
          `:=`(Weapon_Frag_Num = round(Weapon_Frag_Tons * 2160 / 540), 
               Weapon_Frag_Pounds = round(Weapon_Frag_Tons * 2160), 
               Weapon_Frag_Tons = Weapon_Frag_Tons * 2160 / 2000)]

cols <- c("Weapon_Expl_Num", 
          "Weapon_Incd_Num", 
          "Weapon_Frag_Num")
WW2_bombs[, (cols) := lapply(.SD, round_to_int), .SDcols = cols]


### altitude fixes

# if Num is na and there's a disagreement, move feet into num
WW2_bombs[!near(Bomb_Altitude * 100, Bomb_Altitude_Feet) & is.na(Weapon_Expl_Num), 
          `:=`(Weapon_Expl_Num = Bomb_Altitude_Feet, 
               Bomb_Altitude_Feet = NA_integer_)]

# if feet is 0, then set to NA
WW2_bombs[Bomb_Altitude_Feet == 0, 
          `:=`(Bomb_Altitude_Feet = NA_integer_)]

# if altitude is 0 or 1, then set to NA
WW2_bombs[Bomb_Altitude == 0 | Bomb_Altitude == 1, 
          `:=`(Bomb_Altitude = NA_real_)]

# get altitude right
WW2_bombs[Bomb_Altitude > 0 & 
            Bomb_Altitude < 1, 
          `:=`(Bomb_Altitude = Bomb_Altitude * 100)]
WW2_bombs[Bomb_Altitude >= 1000, 
          `:=`(Bomb_Altitude = Bomb_Altitude / 100)]
WW2_bombs[Bomb_Altitude >= 350, 
          `:=`(Bomb_Altitude = Bomb_Altitude / 10)]

# fill in feet based on altitude
WW2_bombs[!is.na(Bomb_Altitude) & 
            is.na(Bomb_Altitude_Feet), 
          `:=`(Bomb_Altitude_Feet = round_to_int(Bomb_Altitude * 100))]

# fill in altitude based on feet
WW2_bombs[is.na(Bomb_Altitude) & 
            !is.na(Bomb_Altitude_Feet), 
          `:=`(Bomb_Altitude = Bomb_Altitude_Feet / 100)]

# if there's a disagreement, trust altitude
WW2_bombs[!near(Bomb_Altitude * 100, Bomb_Altitude_Feet), 
          `:=`(Bomb_Altitude_Feet = round_to_int(Bomb_Altitude * 100))]


# specific fixes, numerics
WW2_bombs[Aircraft_Attacking_Num == 0L | Aircraft_Attacking_Num >= 99L, 
          `:=`(Aircraft_Attacking_Num = NA_integer_)]
WW2_bombs[!near(Aircraft_Airborne_Num, as.integer(Aircraft_Airborne_Num)), 
          `:=`(Aircraft_Airborne_Num = 2 * Aircraft_Airborne_Num)]
WW2_bombs[Aircraft_Airborne_Num == 0, 
          `:=`(Aircraft_Airborne_Num = NA_real_)]
WW2_bombs[, Aircraft_Airborne_Num := as.integer(Aircraft_Airborne_Num)] # gets forced to double somehow

WW2_bombs[Target_Industry_Code >= 100, 
          `:=`(Target_Industry_Code = as.integer(Target_Industry_Code / 10))]

WW2_bombs[, Target_Priority_Code := grem(Target_Priority_Code, pattern = "[A-Z]*")]
WW2_bombs[, Target_Priority_Code := as.integer(if_else(Target_Priority_Code == "", 
                                                       NA_character_, 
                                                       Target_Priority_Code))] # gets forced to character somehow

WW2_bombs[Target_Latitude %!between% c(-90, 90) | Target_Latitude == 0, 
          `:=`(Target_Latitude = NA_real_)]
WW2_bombs[Target_Longitude %!between% c(-180, 180) | Target_Longitude == 0, 
          `:=`(Target_Latitude = NA_real_)]
WW2_bombs[Target_Longitude %between% c(0, 20) & 
            Target_Latitude < 20 & Target_Country == "TUNISIA", 
          `:=`(Target_Latitude = Target_Latitude + 30)]
WW2_bombs[Target_Longitude %between% c(0, 20) & 
            Target_Latitude < 20 & Target_Country == "FRENCH WEST AFRICA", 
          `:=`(Target_Latitude = Target_Latitude + 10)]
WW2_bombs[Target_Longitude %between% c(0, 20) & 
            Target_Latitude < 20 & Target_Country == "GERMANY", 
          `:=`(Target_Latitude = 10*(Target_Latitude %% 10 - Target_Latitude %% 1) + 
                 0.1*(Target_Latitude - (Target_Latitude %% 10)) + 
                 (Target_Latitude %% 1))]

### aircraft attacking
WW2_bombs[is.na(Aircraft_Attacking_Num) & 
            !is_NA_or_0L(Aircraft_Dropping_Num), 
          `:=`(Aircraft_Attacking_Num = Aircraft_Dropping_Num)]
WW2_bombs[is.na(Aircraft_Attacking_Num) & 
            is_NA_or_0L(Aircraft_Dropping_Num) & 
            !is_NA_or_0L(Aircraft_Airborne_Num), 
          `:=`(Aircraft_Attacking_Num = Aircraft_Airborne_Num)]
WW2_bombs[is.na(Aircraft_Attacking_Num) & 
            is_NA_or_0L(Aircraft_Dropping_Num) & 
            is_NA_or_0L(Aircraft_Airborne_Num) & 
            !is_NA_or_0L(Aircraft_Lost_Num), 
          `:=`(Aircraft_Attacking_Num = Aircraft_Lost_Num, 
               Aircraft_Lost_Num = NA_integer_)]

# general fixes, numerics
cols <- c("Weapon_Expl_Num", 
          "Weapon_Incd_Num", 
          "Weapon_Frag_Num", 
          "Weapon_Expl_Pounds", 
          "Weapon_Incd_Pounds", 
          "Weapon_Frag_Pounds")
WW2_bombs[, (cols) := lapply(.SD, as.integer), .SDcols = cols]

WW2_bombs[Bomb_Altitude_Feet == 0L, 
          `:=`(Bomb_Altitude_Feet = NA_integer_)]
WW2_bombs[Bomb_Altitude_Feet > WW2_altitude_max_feet, 
          `:=`(Bomb_Altitude_Feet = WW2_altitude_max_feet)]

WW2_bombs[Weapon_Incd_Type == "10 LB INCENDIARY", 
          `:=`(Weapon_Incd_Tons = Weapon_Incd_Tons * 2.5, 
               Weapon_Incd_Pounds = round_to_int(Weapon_Incd_Pounds * 2.5))]

WW2_bombs[Weapon_Incd_Type == "100 LB WP (WHITE PHOSPHROUS)" & 
            is.na(Weapon_Incd_Tons), 
          `:=`(Weapon_Incd_Tons = 1)]

WW2_bombs[Weapon_Expl_Unit_Weight %exactlylike% " KG", 
          `:=`(Weapon_Expl_Unit_Weight = round_to_int(Weapon_Expl_Unit_Weight * 2.2))]
WW2_bombs[Weapon_Incd_Unit_Weight %exactlylike% " KG", 
          `:=`(Weapon_Incd_Unit_Weight = round_to_int(Weapon_Incd_Unit_Weight * 2.2))]
WW2_bombs[Weapon_Frag_Unit_Weight %exactlylike% " KG", 
          `:=`(Weapon_Frag_Unit_Weight = round_to_int(Weapon_Frag_Unit_Weight * 2.2))]

# general fixes, times
WW2_bombs[["Bomb_Time"]] %>% 
  format_similar_levels(pairings = list("[AaPp][Mm]" = ampm_to_24_hour)) %>% 
  format_levels(format_military_times)

# standard formatting
WW2_bombs %>% keep(is.factor) %>% discard(is.ordered) %>% 
  format_levels_by_col(remove_bad_formatting)

# string column error fixes
WW2_original_targets <- copy(WW2_bombs[, .(Target_City, Target_Type)])#***
offending_targets <- target_names %which!in% c("UNIDENTIFIED", "UNKNOWN, BAY, HILL, POINT")
WW2_bombs[Target_City %contain% word(offending_targets) & 
            Target_Type == "", 
          `:=`(Target_Type = Target_City, 
               Target_City = Target_City %>% remove_after(word(any_of(offending_targets)) %>% with_preceding, greedy = TRUE, inclusive = TRUE))]
drop_similar_levels(WW2_bombs[["Target_City"]], drop = word(offending_targets))

# editing string levels
WW2_bombs[["Mission_Theater"]] %>% 
  format_levels(proper_noun_phrase_vectorized)

WW2_bombs[["Unit_Service"]] %>% 
  rename_levels(changes = c("RAAF" = "RAAF/NEI"))

WW2_bombs[["Unit_Squadron"]] %>% 
  drop_levels(drop = c("0.458333333", 
                       "? FAA", 
                       "0 S")) %>% 
  format_levels(remove_parentheticals) %>% 
  drop_similar_levels(drop = "ASSISTED BY ", exact = TRUE) %>% 
  rename_similar_levels(changes = " +DET.*") %>% 
  rename_similar_levels(changes = c(" SQUADRON" = " (SQ?)\\b")) %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c("SQUADRON"                       = "SQDN", 
                                    "INDIA AIR TASK FORCE"           = "IATF", 
                                    "CHINA AIR TASK FORCE"           = "CATF", 
                                    "SERVICE FLYING TRAINING SCHOOL" = "SFTS", 
                                    "FIGHTER GROUP"                  = "FG", 
                                    "FIGHTER SQUADRON"               = "FS", 
                                    "BOMBARDMENT GROUP"              = "BG", 
                                    "BOMBARDMENT SQUADRON"           = "BS", 
                                                                       " RNZAF", 
                                                                       "RAAF ", 
                                                                       "RNAS", 
                                                                       "\\?")) %>% 
  format_levels(proper_noun_phrase_vectorized)

WW2_bombs[, Unit_Title := Unit_Service][["Unit_Title"]] %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c(" US Tactical Air Command"     = " TAC", 
                                    " US Air Force"                = " AF", 
                                    " Royal Australian Air Force"  = " RAAF", 
                                    " Royal Air Force"             = " RAF", 
                                    " Royal New Zealand Air Force" = " RNZAF", 
                                    " South African Air Force"     = " SAAF"))
WW2_bombs[, Unit_Title := factor(trimws(paste(Unit_Title, Unit_Squadron)))]

WW2_bombs[["Unit_Country"]] %>% 
  rename_levels(changes = c("UK" = "GREAT BRITAIN")) %>% 
  format_levels(proper_noun_phrase_vectorized)

WW2_bombs[["Aircraft_Type"]] %>% 
  rename_levels(changes = c("A-31 Vengeance" = "VENGEANCE (A31)", 
                            "A-31 Vengeance" = "VENGEANCE(A-31)")) %>% 
  format_levels(format_aircraft_types %,% proper_noun_phrase_aircraft_vectorized)

WW2_bombs[["Weapon_Expl_Type"]] %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c("0 LB GP" = "0 GP")) %>% 
  rename_levels(changes = c("TORPEDO"         = "TORPEDOES", 
                            "TORPEDO"         = "TORPEDOES MISC", 
                            "40 LB EXPLOSIVE" = "UNK CODE 20 110 LB EXPLOSIVE", 
                            "250 LB BAP"      = "250 BAP", 
                            "500 LB/250 LB"   = "250 LB/500 LB")) %>% 
  format_levels(fix_parentheses) %>% 
  rename_similar_levels(changes = weapon_rules) %>% 
  format_levels(weapon_phrase_vectorized)

WW2_bombs[["Weapon_Incd_Type"]] %>% 
  drop_levels(drop = "X") %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c("TANK INCENDIARY" = "TANK AS INCENDIARY", 
                                    "PHOSPHORUS"      = "PHOSPHROUS")) %>% 
  format_levels(fix_parentheses) %>% 
  rename_similar_levels(changes = weapon_rules) %>% 
  format_levels(weapon_phrase_vectorized)

WW2_bombs[["Weapon_Frag_Type"]] %>% 
  rename_levels(changes = c("138 LB FRAG (6X23 CLUSTERS)" = "23 LB FRAG CLUSTERS (6 X23 PER CLUSTER)")) %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c("PARA FRAG" = "PARAFRAG")) %>% 
  drop_levels(drop = "UNK CODE 15") %>% 
  format_levels(fix_parentheses) %>% 
  rename_similar_levels(changes = weapon_rules) %>% 
  format_levels(weapon_phrase_vectorized)

WW2_bombs[["Target_City"]] %>% 
  drop_levels(drop = c("UNKNOWN", "UNIDENTIFIED")) %>% 
  rename_similar_levels(changes = c("LONGSTOP" = "\\b(LONG STOP)\\b")) %>% 
  drop_levels_formula(expr = (grem(., pattern = "[ NSEW]+") %like% "^[\\d.]+$")) %>% 
  format_levels(remove_parentheticals %,% remove_square_brackets) %>% 
  rename_similar_levels(changes = target_city_rules) %>% 
  rename_similar_levels(changes = and_preceding(word(any_of(English_prepositions_for_place_names_upper)))) %>% 
  rename_similar_levels(changes = "\\b ?[A-Z]$") %>% 
  drop_levels(drop = c("BATTLE", "CONVOY")) %>% #*** this may be useful information if Target_Type is undefined
  format_levels(proper_noun_phrase_vectorized) %>% 
  rename_levels(changes = c("Beaumont sur Oise"    = "Beaum", 
                            "Beaumont le Roger"    = "Beaumdnt le Roger", 
                            "Czelldomolk"          = "Czelldomdlk", 
                            "Foret de Isle Adam"   = "Foret de Isle Ada", 
                            "Krefeld"              = "Frefeld", 
                            "Freital"              = "Freita", 
                            "Merzenich"            = "Merzenicg", 
                            "Pont St Esprit"       = "Pdnt St Esprit", 
                            "Sandersleben"         = "Sangersleben", 
                            "Sottevast"            = "Soltevast", 
                            "St Pierre de la Fage" = "St Pierre de la Fag", 
                            "Wittlich"             = "Wiltlich"))

WW2_bombs[["Target_Type"]] %>% 
  rename_similar_levels(changes = target_rules) %>% 
  format_levels(tolower)
WW2_half_formatted_targets <- WW2_bombs[["Target_Type"]]#***
WW2_bombs[["Target_Type"]] %>% 
  format_levels(fix_invalid_words, 
                dictionary = dictionary_20k_plus, 
                exclude = c(measurement_units, "unidentified", "unknown"))

WW2_bombs[["Target_Country"]] %>% 
  drop_similar_levels(drop = "UNKNOWN", exact = TRUE) %>% 
  rename_similar_levels(changes = " MINING", exact = TRUE) %>% 
  rename_levels(changes = c("Papua New Guinea"           = "PAPUA NEW GUINEA, MANUS ISLAND", 
                            "India"                      = "ANDAMAN ISLANDS", 
                            "Papua New Guinea"           = "BISMARK ARCHIPELAGO", 
                            "Papua New Guinea"           = "BOUGAINVILLE", 
                            "Indonesia"                  = "BALI", 
                            "Micronesia"                 = "CAROLINE ISLANDS", 
                            "Indonesia"                  = "CELEBES ISLANDS", 
                            "Papua New Guinea"           = "CORAL SEA AREA", 
                            "France"                     = "CORSICA", 
                            "Greece"                     = "CRETE", 
                            "Ethiopia"                   = "ETHIOPIA/ABSINNYA", 
                            "Formosa and Ryukyu Islands" = "FORMOSA", 
                            "Vietnam"                    = "FRENCH INDO CHINA", 
                            "Libya"                      = "FRENCH WEST AFRICA", 
                            "Micronesia"                 = "GILBERT ISLANDS", 
                            "Netherlands"                = "HOLLAND OR NETHERLANDS", 
                            "India"                      = "INDIAN OCEAN", 
                            "Korea"                      = "KOREA OR CHOSEN", 
                            "Japan"                      = "KURILE ISLANDS", 
                            "Malay States"               = "MAYLAYSIA", 
                            "China"                      = "MANCHURIA", 
                            "Japan"                      = "MARCUS ISLANDS", 
                            "Micronesia"                 = "MARSHALL ISLANDS", 
                            "Indonesia"                  = "NETHERLANDS EAST INDIES", 
                            "Papua New Guinea"           = "NEW GUINEA", 
                            "Papua New Guinea"           = "NEW IRELAND", 
                            "Micronesia"                 = "PALAU ISLANDS", 
                            "Italy"                      = "PANTELLARIA", 
                            "Philippines"                = "PHILIPPINE ISLANDS", 
                            "Italy"                      = "SARDINIA", 
                            "Italy"                      = "SICILY", 
                            "Indonesia"                  = "SUMATRA", 
                            "Thailand"                   = "THAILAND OR SIAM", 
                            "Indonesia"                  = "TIMOR", 
                            "Japan"                      = "VOLCANO AND BONIN ISLANDS", 
                            "Micronesia"                 = "WAKE ISLAND")) %>% 
  format_levels(proper_noun_phrase_vectorized)

# WW2_bombs[Target_City == "Acqualagna", Target_Country := "Germany"]
# WW2_bombs[Target_City == "Berat", Target_Country := "Albania"]
# WW2_bombs[Target_City == "Berlin", Target_Country := "Germany"]
# WW2_bombs[Target_City == "Faisi Island", Target_Country := "Solomon Islands"]
# WW2_bombs[Target_City == "Fuiloro", Target_Country := "Indonesia"]
# WW2_bombs[Target_City == "Magur", Target_Country := "Micronesia"]
# WW2_bombs[Target_City == "Maizuru", Target_Country := "Japan"]
# WW2_bombs[Target_City == "Okayama", Target_Country := "Japan"]
# 
# WW2_bombs[Target_City == "Brod" & Target_Country == "Romania", Target_City := "Brad"]

WW2_bombs[["Target_Industry"]] %>% 
  drop_levels(drop = "UNIDENTIFIED TARGETS") %>% 
  rename_similar_levels(changes = c("AIRCRAFT"      = "A/C", 
                                    "MANUFACTURING" = "MFG.", 
                                    "RAILROAD "     = "R.?R.? ", 
                                    "V-WEAPON"      = "V - WEAPON", 
                                                      ":? \\([A-Z 0-9]*\\)", 
                                                      ",[A-Z, 0-9-]*", 
                                                      " - [A-Z 0-9]*")) %>% 
  format_levels(tolower)

WW2_bombs[["Takeoff_Country"]] %>% 
  rename_levels(changes = c("PHILIPPINES" = "PHILLIPINES")) %>% 
  format_levels(proper_noun_phrase_vectorized)

WW2_bombs[["Takeoff_Base"]] %>% 
  format_levels(proper_noun_phrase_vectorized)

WW2_bombs[["Sighting_Method"]] %>% 
  drop_levels(drop = c("NOT INDICATED")) %>% 
  rename_levels(changes = c("FFF" = "F.F.F.")) %>% 
  format_levels(tolower)

WW2_bombs[["Target_Priority"]] %>% 
  format_levels(tolower)

# fill out matching codes and values
value_cols = c("Target_Country", 
               "Target_City", 
               "Target_Industry", 
               "Target_Priority", 
               "Sighting_Method")
code_cols = c("Target_Country_Code", 
              "Target_City_Code", 
              "Target_Industry_Code", 
              "Target_Priority_Code", 
              "Sighting_Method_Code")
WW2_bombs[, (code_cols) := lapply(.SD, factor, exclude = NULL), .SDcols = code_cols]

WW2_bombs %>% 
  fill_matching_values_by_col(code_cols = code_cols, value_cols = value_cols, backfill = TRUE, drop.codes = TRUE, drop.missing.levels = TRUE, NA_code = NA_character_)

WW2_bombs[, (code_cols) := lapply(.SD, as.integer), .SDcols = code_cols]

# fill out missing countries by service
WW2_bombs %>% 
  fill_matching_values(code_col = "Unit_Service", value_col = "Unit_Country", drop.missing.levels = TRUE)

# fill out missing countries by city
WW2_bombs %>%
  fill_matching_values(code_col = "Target_City", value_col = "Target_Country", drop.missing.levels = TRUE)


### the big city filtering question thing

WW2_copy <- copy(WW2_bombs)

# turn off indexing to prevent cache invalidation errors in WW2_bombs$Target_City's level table
previous_auto_index_option <- getOption('datatable.auto.index')
previous_use_index_option <- getOption('datatable.auto.index')
options(datatable.auto.index = FALSE)
options(datatable.use.index = FALSE)

### latitude and longitude errors

debug_message("finding cities with lat-long problems")
WW2_loc <- unique(WW2_bombs[, .(Target_City, Target_Country, Target_Latitude, Target_Longitude)])
WW2_loc_by_coord <- WW2_loc[!is.na(Target_Latitude) & 
                              !is.na(Target_Longitude), 
                            .(Target_City, 
                              Target_Country, 
                              'GRPN' = .N), 
                            keyby = .(Target_Latitude, Target_Longitude)]
WW2_loc_by_city <- WW2_loc[Target_City != "", 
                           .(Target_Country, 
                             Target_Latitude, 
                             Target_Longitude, 
                             'GRPN' = .N, 
                             'loc_sd' = sqrt(sd(Target_Latitude, na.rm = TRUE) ** 2 + sd(Target_Longitude, na.rm = TRUE) ** 2) %NA% 0), 
                           keyby = Target_City]
WW2_cities_to_investigate <- WW2_loc_by_city[loc_sd > 0.5, as.character(unique(Target_City))]

lat_long_error <- character(0L)
city_error <- character(0L)
unsure_error <- character(0L)
for (city in WW2_cities_to_investigate) {
  lats <- WW2_loc_by_city[Target_City == city & !is.na(Target_Latitude), Target_Latitude]
  longs <- WW2_loc_by_city[Target_City == city & !is.na(Target_Longitude), Target_Longitude]
  lat_dists <- dist(lats)
  long_dists <- dist(longs)
  if (any(is_int(lat_dists) & lat_dists != 0) || any(is_int(long_dists) & long_dists != 0)) {
    # probably just a data entry/OCR mistake on the lat or long
    lat_long_error <- append(lat_long_error, city)
    if (any(is_int(lat_dists) & lat_dists != 0)) {
      index <- which(is_int(lat_dists) & lat_dists != 0)
      for (ind in index) {
        indices <- dist_indices(ind, length(lats))
        lats_in_question <- lats[c(indices[['i']], indices[['j']])]
        all_lats <- WW2_bombs[Target_City == city, Target_Latitude]
        lat_sets <- mode_and_others_num(all_lats)
        other_lats <- lat_sets[['others']]
        mode_lat <- lat_sets[['mode']]
        WW2_bombs[Target_City == city & Target_Latitude %in% other_lats, Target_Latitude := mode_lat]
      }
    }
    if (any(is_int(long_dists) & long_dists != 0)) {
      index <- which(is_int(long_dists) & long_dists != 0)
      for (ind in index) {
        indices <- dist_indices(ind, length(longs))
        longs_in_question <- longs[c(indices[['i']], indices[['j']])]
        all_longs <- WW2_bombs[Target_City == city, Target_Longitude]
        long_sets <- mode_and_others_num(all_longs)
        other_longs <- long_sets[['others']]
        mode_long <- long_sets[['mode']]
        WW2_bombs[Target_City == city & Target_Longitude %in% other_longs, Target_Longitude := mode_long]
      }
    }
  } else {
    # see if any lats or longs exactly match other cities
    lat_test <- lapply(WW2_loc_by_city$Target_Latitude, `==`, lats)
    long_test <- lapply(WW2_loc_by_city$Target_Longitude, `==`, longs)
    test <- lat_test %map_and% long_test
    city_matches <- WW2_loc_by_city[Target_City != city & sapply(test, any), as.character(Target_City), nomatch = 0L]
    if (!is_empty(city_matches)) {
      # probably a data entry error on the city#*** still need to solve this
      city_error <- append(city_error, city)
    } else {
      unsure_error <- append(unsure_error, city)#*** not sure what to do here
    }
  }
}

### similarly named cities

city_levels <- levels(WW2_bombs$Target_City)
city_levels_copy <- copy(city_levels)
city_levels <- sort(fix_spaces(city_levels))

partial_string_levels <- list()

ignored_similar_cities <- character(0L)
changed_similar_cities <- character(0L)

for (city_index in seq_along(city_levels)[c(-1L, -length(city_levels))]) {
  city <- city_levels[city_index]
  if (city %c% levels(WW2_bombs$Target_City)) {
    next_city_index <- city_index + 1L
    next_city <- city_levels[next_city_index]
    similar_cities <- character(0L)
    while (next_city_index <= length(city_levels) && 
           city %c% levels(WW2_bombs$Target_City) && 
           adist(city, next_city, fixed = TRUE, partial = TRUE) == 0L) {
      similar_cities <- append(similar_cities, next_city)
      next_city_index <- next_city_index + 1L
      next_city <- city_levels[next_city_index]
    }
    if (!is_empty(similar_cities)) {
      # in case list is necessary
      similar_cities_list <- list(similar_cities)
      re_name(similar_cities_list, city)
      partial_string_levels <- append(partial_string_levels, similar_cities_list)
      # just go straight to edits
      
      # keep only those names that have lat-long pairs closeby
      
      possible_spellings <- c(city, similar_cities)
      mean_lat <- WW2_bombs[Target_City == city, mean(Target_Latitude, na.rm = TRUE)]
      mean_long <- WW2_bombs[Target_City == city, mean(Target_Longitude, na.rm = TRUE)]
      relevant_data <- WW2_bombs[Target_City %in% possible_spellings & 
                                   close_to(Target_Latitude, mean_lat, coord_buffer) & 
                                   close_to(Target_Longitude, mean_long, coord_buffer), 
                                 .(Target_City, Target_Country, Target_Latitude, Target_Longitude), 
                                 nomatch = 0L]
      if (relevant_data[Target_City != city, .N] > 0L) {
        changed_similar_cities <- append(changed_similar_cities, city)
        
        # if extra words (if exists space), then do target_rules on city names and see if the results match target_names; if so, delete that word
        
        corrected_spellings <- toupper(possible_spellings) %>% gsubs(changes = target_rules)
        has_target <- corrected_spellings %like% word(any_of(target_names))
        
        if (any(has_target)) {
          for (item in which(has_target)) {
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
        }
        
        # otherwise prefer longer names
        
        possible_spellings <- unique(possible_spellings)
        
        if (length(possible_spellings) > 1L) {
          spelling_lengths <- nchar(possible_spellings)
          if (length(spelling_lengths %[==]% which.max(spelling_lengths)) > 1L) {
            which_preferred <- mode_factor(relevant_data[["Target_City"]])
          } else {
            which_preferred <- which.max(spelling_lengths)
          }
          preferred_spelling <- possible_spellings[which_preferred]
          other_spellings <- possible_spellings[-which_preferred]
          WW2_bombs[["Target_City"]] %>% 
            replace_levels(from = other_spellings, to = preferred_spelling)
        }
      } else {
        ignored_similar_cities <- append(ignored_similar_cities, city)
      }
    }
  } else {
    ignored_similar_cities <- append(ignored_similar_cities, city)
  }
}



debug_message("calculating string distance")
library(stringdist)

string_similarity_threshold <- 1L

city_string_similarity <- stringdistmatrix(levels(WW2_bombs$Target_City), nthread = cores)
are_cities_similar <- city_string_similarity <= string_similarity_threshold

debug_message("finding similarly spelled cities")
similar_city_list <- list()
#city_pairs <- data.table('one' = character(sum(curious2)), 'two' = character(sum(curious2)))
total_length <- nlevels(WW2_bombs$Target_City)
city_names <- levels(WW2_bombs$Target_City)
for (i in seq(total_length - 1L)) {
  similar_cities <- character(0L)
  for (j in seq((i + 1L), total_length)) {
    if (are_cities_similar[dist_index(i, j, total_length)]) {
      similar_cities <- append(similar_cities, city_names[j])
    }
  }
  if (!is_empty(similar_cities)) {
    new_list <- list()
    new_list[[city_names[i]]] <- similar_cities
    similar_city_list <- append(similar_city_list, new_list)
  }
}
# now we have a list of cities that are similar in name to each city

similar_city_names <- names(similar_city_list)
for (city in similar_city_names[1:10]) {
  possible_spellings <- c(city, similar_city_list[[city]])
  mean_lat <- WW2_bombs[Target_City == city, mean(Target_Latitude, na.rm = TRUE)]
  mean_long <- WW2_bombs[Target_City == city, mean(Target_Longitude, na.rm = TRUE)]
  relevant_data <- WW2_bombs[Target_City %in% possible_spellings & 
                               close_to(Target_Latitude, mean_lat, coord_buffer) & 
                               close_to(Target_Longitude, mean_long, coord_buffer), 
                             .(Target_City, Target_Country, Target_Latitude, Target_Longitude), 
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

# reset data.table index for WW2_bombs$Target_City
options(datatable.auto.index = previous_auto_index_option)
WW2_bombs[1, Target_City := WW2_bombs[1, Target_City]]
options(datatable.use.index = previous_use_index_option)

### end of experimentation


# long weapons types and numbers cleaning script
source('WW2_weapon_cleaning.R')

# reduced columns
WW2_bombs[, Target_Category := Target_Type][["Target_Category"]] %>% 
  reduce_levels(rules = target_categorizations)
WW2_bombs[, Target_Category := ordered_empty_at_end(Target_Category, "other")]


### Korea 1 Edits -------------------------------------------------------------

debug_message("cleaning Korea1")

# temporary type fixes
cols <- c("Aircraft_Dispatched_Num", 
          "Aircraft_Attacking_Num", 
          "Aircraft_Aborted_Num", 
          "Aircraft_Lost_Enemy_Air_Num", 
          "Aircraft_Lost_Enemy_Ground_Num", 
          "Aircraft_Lost_Enemy_Unknown_Num", 
          "Aircraft_Lost_Other_Num", 
          "Aircraft_Damaged_Num", 
          "KIA", 
          "WIA", 
          "MIA", 
          "Enemy_Aircraft_Destroyed_Confirmed", 
          "Enemy_Aircraft_Destroyed_Probable", 
          "Rocket_Num", 
          "Bullet_Rounds")
Korea_bombs1[, (cols) := lapply(.SD, as.integer), .SDcols = cols]

# new columns
Korea_bombs1[, Weapon_Weight_Pounds := round_to_int(Weapon_Weight_Tons * 2000)]

# specific fixes, numerics
Korea_bombs1[Aircraft_Attacking_Num == 0L | Aircraft_Attacking_Num >= 99L, 
             `:=`(Aircraft_Attacking_Num = NA_integer_)]

# standard formatting
Korea_bombs1 %>% keep(is.factor) %>% discard(is.ordered) %>% 
  format_levels_by_col(remove_bad_formatting)

# editing string levels
Korea_bombs1[["Unit_ID"]] %>% 
  rename_levels(changes = c("GAF" = "G AF")) %>% 
  drop_levels(drop = c("NONE", "N0NE", "NONE0", "NONE6", "NQNE"))

Korea_bombs1[["Unit_Squadron"]] %>% 
  drop_levels(drop = "HQ")

Korea_bombs1[["Aircraft_Type"]] %>% 
  rename_levels(changes = c("C-54"  = "O54", 
                            "L-05"  = "LO5", 
                            "RB-29" = "R829", 
                            "T-06"  = "TO6", 
                            "T-06"  = "TQ6")) %>% 
  format_levels(format_aircraft_types)

Korea_bombs1[, Unit_Country := "USA"]
Korea_bombs1[Unit_Group %exactlylike% "Hellenic", 
             `:=`(Unit_Country = "Greece")]
Korea_bombs1[Unit_Group %exactlylike% "South African", 
             `:=`(Unit_Country = "South Africa")]
Korea_bombs1[Unit_Group %exactlylike% "Australian", 
             `:=`(Unit_Country = "Australia")]
Korea_bombs1[, Unit_Country := factor(Unit_Country)]

Korea_bombs1[, Unit_Title := factor(if_else(Unit_Country == "USA", 
                                            paste0("US ", Unit_Squadron), 
                                            as.character(Unit_Squadron)))]

Korea_bombs1[, Target_Country := factor("Korea")]
Korea_bombs1[, Target_Category := factor("other")]

Korea_bombs1[, Weapon_Type := factor(if_else(!is.na(Rocket_Num), 
                                             "bombs", 
                                     if_else(!is.na(Bullet_Rounds), 
                                             "rounds", 
                                             "")))]


### Korea 2 Edits -----------------------------------------------------------

debug_message("cleaning Korea2")

# temporary type fixes
cols <- c("Aircraft_Attacking_Num", 
          "Sortie_Duplicates", 
          "Aircraft_Aborted_Num", 
          "Aircraft_Bombload_Calculated_Pounds")
Korea_bombs2[, (cols) := lapply(.SD, as.integer), .SDcols = cols]

# column error fixes
Korea_bombs2[Bomb_Altitude_Feet_Range != "" & !(Bomb_Altitude_Feet_Range %like% "\\d+ ?- ?\\d*"), 
             `:=`(Bomb_Damage_Assessment = Bomb_Altitude_Feet_Range, 
                  Bomb_Altitude_Feet_Range = "")]
Korea_bombs2[Bomb_Altitude_Feet_Range == "" & 
               Bomb_Damage_Assessment %like% "^\\d+$", 
             `:=`(Bomb_Altitude_Feet_Range = Bomb_Damage_Assessment, 
                  Bomb_Damage_Assessment = "")]
drop_similar_levels(Korea_bombs2[["Bomb_Damage_Assessment"]], drop = "^\\d+$")

Korea_bombs2[Aircraft_Total_Weight %like% "\\d+ - \\d+", 
             `:=`(Aircraft_Total_Weight = "")]

Korea_bombs2[Target_City %like% "ission|ccomplish|erform|btain|eturn|lew|B[CO][APR]|photo|reconnaissance|weather|due to|effective", 
             `:=`(Bomb_Damage_Assessment = Target_City, 
                  Target_City = "")]
drop_similar_levels(Korea_bombs2[["Target_City"]], drop = "ission|ccomplish|erform|btain|eturn|lew|B[CO][APR]|photo|reconnaissance|weather|due to|effective")

formatted_target_names <- capitalize_phrase_vectorized(target_names)
Korea_bombs2[Target_City %contain% word(formatted_target_names) & 
               Target_Type == "", 
             `:=`(Target_Type = Target_City, 
                  Target_City = grem(Target_City, word(any_of(formatted_target_names))) %>% fix_spaces)]
drop_similar_levels(Korea_bombs2[["Target_City"]], drop = word(any_of(formatted_target_names)))

Korea_bombs2[Target_Type %like% "ccomplished|erformed|eturned|Un[a-z]*own", 
             `:=`(Bomb_Damage_Assessment = Target_Type, 
                  Target_Type = "")]
drop_similar_levels(Korea_bombs2[["Target_Type"]], drop = "ccomplished|erformed|eturned|Un[a-z]*own")

# new columns
Korea_bombs2[, Unit_Country := factor("USA")]
Korea_bombs2[, Target_Country := factor("Korea")]

cols <- c("Bomb_Altitude_Feet_Low", 
          "Bomb_Altitude_Feet_High")
Korea_bombs2[, (cols) := tstrsplit(Bomb_Altitude_Feet_Range, " ?- ?", keep = 1:2)]
Korea_bombs2[, (cols) := lapply(.SD, as.integer), .SDcols = cols]

Korea_bombs2[, Weapon_Unit_Weight := grem(Weapon_Type, pattern = "\\D[ -~]*")]
Korea_bombs2[, Weapon_Unit_Weight := as.integer(if_else(Weapon_Unit_Weight %like% "\\d+", 
                                                        Weapon_Unit_Weight, 
                                                        NA_character_))]
Korea_bombs2[, Weapon_Weight_Pounds := Aircraft_Attacking_Num * Aircraft_Bombload_Calculated_Pounds]

# general fixes, numerics
Korea_bombs2[, Target_Latitude  := grem(Target_Latitude, pattern = "[^0-9.]*")]
Korea_bombs2[, Target_Latitude  := as.numeric(if_else(Target_Latitude == "", 
                                                      NA_character_, 
                                                      Target_Latitude))]
Korea_bombs2[, Target_Longitude := grem(Target_Longitude, pattern = "[^0-9.]*")]
Korea_bombs2[, Target_Longitude := as.numeric(if_else(Target_Longitude == "", 
                                                      NA_character_, 
                                                      Target_Longitude))]

# specific fixes, numerics
Korea_bombs2[Aircraft_Attacking_Num == 0L | Aircraft_Attacking_Num >= 99L, 
             `:=`(Aircraft_Attacking_Num = NA_integer_)]

Korea_bombs2[Bomb_Altitude_Feet_High == 0L, 
             `:=`(Bomb_Altitude_Feet_High = NA_integer_)]
Korea_bombs2[Bomb_Altitude_Feet_High > Korea_altitude_max_feet & 
               Bomb_Altitude_Feet_High / 10 < Bomb_Altitude_Feet_Low, 
             `:=`(Bomb_Altitude_Feet_High = as.integer(Bomb_Altitude_Feet_High / 10))]
Korea_bombs2[Bomb_Altitude_Feet_High > Korea_altitude_max_feet, 
             `:=`(Bomb_Altitude_Feet_High = Korea_altitude_max_feet)]

Korea_bombs2[, Row_Number := as.integer(grem(Row_Number, pattern = "\r\r", exact = TRUE))] # gets forced to character somehow
Korea_bombs2[Weapon_Expended_Num == "" | 
               Weapon_Expended_Num == "Unknown", 
             `:=`(Weapon_Expended_Num = NA_character_)]
Korea_bombs2[, Weapon_Expended_Num := as.integer(Weapon_Expended_Num)] # gets forced to character somehow
Korea_bombs2[Weapon_Expended_Num == 0L, 
             `:=`(Weapon_Expended_Num = NA_integer_)]
Korea_bombs2[, Mission_Number := as.integer(grem(Mission_Number, pattern = "[ ?L]"))] # gets forced to character somehow
Korea_bombs2[Aircraft_Lost_Num == "", 
             `:=`(Aircraft_Lost_Num = NA_character_)]
Korea_bombs2[!(Aircraft_Lost_Num == "1" | Aircraft_Lost_Num == "2" | Aircraft_Lost_Num == "3"), 
             `:=`(Aircraft_Lost_Num = NA_character_, 
                  Bomb_Damage_Assessment = Aircraft_Lost_Num)]
Korea_bombs2[, Aircraft_Lost_Num := as.integer(Aircraft_Lost_Num)] # cleaning data type due to bad data

Korea_bombs2[Target_Latitude %!between% c(-90, 90) | Target_Latitude == 0, 
             `:=`(Target_Latitude = NA_real_)]
Korea_bombs2[Target_Longitude %!between% c(-180, 180) | Target_Longitude == 0, 
             `:=`(Target_Longitude = NA_real_)]
Korea_bombs2[Target_Latitude > 44, 
             `:=`(Target_Latitude = NA_real_, 
                  Target_Longitude = NA_real_)]

# type fixes
Korea_bombs2[, Bomb_Damage_Assessment := factor(Bomb_Damage_Assessment)]

# standard formatting
Korea_bombs2 %>% keep(is.factor) %>% discard(is.ordered) %>% 
  format_levels_by_col(remove_bad_formatting)

# editing string levels
Korea_bombs2[["Target_City"]] %>% 
  format_levels(remove_parentheticals %,% remove_square_brackets %,% toupper) %>% 
  rename_similar_levels(changes = c("PENINSULA" = "PENNINSULA"), exact = TRUE) %>% 
  rename_similar_levels(changes = target_city_rules) %>% 
  rename_similar_levels(changes = and_preceding(word(any_of(English_prepositions_for_place_names_upper)))) %>% 
  format_levels(proper_noun_phrase_vectorized)

Korea_bombs2[["Weapon_Type"]] %>% 
  rename_similar_levels(changes = c(" LB GP" = " GP\\b")) %>% 
  drop_levels(drop = "Unknown")

Korea_bombs2[["Target_Type"]] %>% 
  format_levels(toupper) %>% 
  rename_similar_levels(changes = target_rules) %>% 
  format_levels(tolower) %>% 
  format_levels(fix_invalid_words, 
                dictionary = dictionary_20k_plus, 
                exclude = c(measurement_units, "unidentified", "unknown"))

Korea_bombs2[["Unit_Squadron"]] %>% 
  rename_similar_levels(changes = c("Reconnaissance" = "Recon", 
                                    " Squadron"      = " Sq\\b"))
Korea_bombs2[, Unit_Title := factor(trimws(paste0("US ", Unit_Squadron)))]

Korea_bombs2[["Aircraft_Type"]] %>% 
  rename_levels(changes = c("RB-45" = "RB 45")) %>% 
  format_levels(proper_noun_phrase_aircraft_vectorized)

Korea_bombs2[["Mission_Type"]] %>% 
  rename_similar_levels(changes = c("Interdiction" = "Interd[a-z]*n", 
                                    "evaluation"   = "eva[a-z]*ion"))

Korea_bombs2[["Nose_Fuze"]] %>% 
  rename_levels(changes = c("Instantaneous" = "instantaneous")) %>% 
  drop_levels(drop = "Unknown to poor results.")

Korea_bombs2[["Bomb_Sighting_Method"]] %>% 
  format_levels(tolower)

# reduced columns
Korea_bombs2[, Target_Category := Target_Type][["Target_Category"]] %>% 
  reduce_levels(rules = target_categorizations)
Korea_bombs2[, Target_Category := ordered_empty_at_end(Target_Category, "other")]


### Vietnam Edits -----------------------------------------------------------

debug_message("cleaning Vietnam")

# temporary type fixes
cols <- c("ID", 
          "Weapon_Expended_Num", 
          "Weapon_Unit_Weight", 
          "Aircraft_Attacking_Num", 
          "Weapon_Jettisoned_Num", 
          "Weapon_Returned_Num", 
          "Weapon_Weight_Loaded")
Vietnam_bombs[, (cols) := lapply(.SD, as.integer), .SDcols = cols]

# column error fixes
Vietnam_bombs[, Unit_Squadron := factor(Unit_Squadron)] # gets read as character somehow

# specific fixes, numerics
Vietnam_bombs[Aircraft_Attacking_Num == 0L | Aircraft_Attacking_Num >= 99L, 
             `:=`(Aircraft_Attacking_Num = NA_integer_)]
Vietnam_bombs[Weapon_Expended_Num == 0L, 
              `:=`(Weapon_Expended_Num = NA_integer_)]
Vietnam_bombs[Weapon_Unit_Weight == 0L, 
              `:=`(Weapon_Unit_Weight = NA_integer_)]
Vietnam_bombs[Weapon_Jettisoned_Num == -1L, 
              `:=`(Weapon_Jettisoned_Num = NA_integer_)]
Vietnam_bombs[Weapon_Returned_Num == -1L, 
              `:=`(Weapon_Returned_Num = NA_integer_)]
Vietnam_bombs[Weapon_Weight_Loaded < 1L, 
              `:=`(Weapon_Weight_Loaded = NA_integer_)]
Vietnam_bombs[is_NA_or_0L(Weapon_Expended_Num) & 
                is_NA_or_0L(Weapon_Jettisoned_Num) & 
                is_NA_or_0L(Weapon_Returned_Num), 
              `:=`(Weapon_Expended_Num = NA_integer_, 
                   Weapon_Jettisoned_Num = NA_integer_, 
                   Weapon_Returned_Num = NA_integer_)]

Vietnam_bombs[Target_Latitude %!between% c(-90, 90) | Target_Latitude == 0, 
              `:=`(Target_Latitude = NA_real_)]
Vietnam_bombs[Target_Longitude %!between% c(-180, 180) | Target_Longitude == 0, 
              `:=`(Target_Longitude = NA_real_)]
Vietnam_bombs[Target_Latitude < 0.02, 
              Target_Latitude := Target_Latitude * 1000]
Vietnam_bombs[Target_Latitude < 1, 
              Target_Latitude := Target_Latitude * 10 + 10]
Vietnam_bombs[Target_Latitude < 8.5, 
              Target_Latitude := Target_Latitude + 10]
Vietnam_bombs[Target_Longitude %!between% c(100,110), 
              Target_Longitude := (Target_Longitude %% 10) + 100]

# new columns
Vietnam_bombs[, Weapon_Weight_Pounds := Weapon_Expended_Num * Weapon_Unit_Weight]
Vietnam_bombs[, Bomb_Altitude_Feet := as.integer(Bomb_Altitude * 1000)] # assumed factor of 1000 here (in ft)--otherwise makes no sense

# more specific fixes, numerics
Vietnam_bombs[Bomb_Altitude_Feet == 0L, 
              `:=`(Bomb_Altitude_Feet = NA_integer_)]
Vietnam_bombs[Bomb_Altitude_Feet > Vietnam_altitude_max_feet, 
              `:=`(Bomb_Altitude_Feet = round_to_int(Bomb_Altitude_Feet / 10))]

# general fixes, times
cols <- c("Bomb_Time_Start", 
          "Bomb_Time_Finish")
Vietnam_bombs %>% select(cols) %>% 
  rename_similar_levels_by_col(exact = TRUE, 
                               changes = ".0") %>% 
  drop_levels_by_col(drop = "0") %>% 
  format_levels_by_col(format_military_times)

# standard formatting
Vietnam_bombs %>% keep(is.factor) %>% discard(is.ordered) %>% 
  format_levels_by_col(remove_bad_formatting)

# editing string levels
Vietnam_bombs[["Aircraft_Type"]] %>% 
  rename_similar_levels(changes = c("\\1-\\2" = "([A-Za-z]+)[ ./]?(\\d+[A-Za-z]*)( ?/.*)?")) %>% 
  drop_levels(drop = "NOT CODED") %>% 
  format_levels(format_aircraft_types)

Vietnam_bombs[["Target_Type"]] %>% 
  rename_similar_levels(changes = c("/"      = "\\\\", 
                                               "/ANY", 
                                    "TROOPS" = "TROOPS? UNK.*")) %>% 
  rename_similar_levels(changes = target_rules) %>% 
  drop_levels(drop = c("NO TARGET ACQUIRED")) %>% 
  format_levels(tolower) %>% 
  format_levels(fix_invalid_words, 
                dictionary = dictionary_20k_plus, 
                exclude = c(measurement_units, "unidentified", "unknown"))

Vietnam_bombs[["Weapon_Type"]] %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c(" (100 rounds each)" = " (HNDRDS)")) %>% 
  drop_levels(drop = c("UNK", "UNKNOWN")) %>% 
  format_levels(fix_parentheses) %>% 
  rename_similar_levels(changes = weapon_rules) %>% 
  format_levels(weapon_phrase_vectorized)

Vietnam_bombs[["Callsign"]] %>% 
  rename_similar_levels(changes = c("O\\1"     = "0([A-Z])", 
                                    "\\1O\\2"  = "([A-Z])0([A-Z ])", 
                                    "\\1OO\\2" = "([A-Z])00([A-Z])", 
                                    "\\1 \\2"  = "([A-Z]{1,6})0(\\d)", 
                                    "ICON"     = "1CON", 
                                    "ISSUE"    = "1SSUE"))

Vietnam_bombs[["Mission_Function"]] %>% 
  rename_similar_levels(exact = TRUE, 
                        changes = c("AIRCRAFT"       = "A/C", 
                                    "COMBAT"         = "CMBT", 
                                    "RECONNAISSANCE" = "RECCE")) %>% 
  format_levels(tolower)

Vietnam_bombs[["Operation"]] %>% 
  rename_similar_levels(changes = c(Vietnam_operation_rules, 
                                    Vietnam_operation_rules2)) %>% 
  format_levels(capitalize_phrase_vectorized)

Vietnam_bombs[["Unit_Country"]] %>% 
  rename_levels(changes = c("South Korea"   = "KOREA (SOUTH)", 
                            "USA"           = "UNITED STATES OF AMERICA", 
                            "South Vietnam" = "VIETNAM (SOUTH)")) %>% 
  format_levels(proper_noun_phrase_vectorized)

Vietnam_bombs[["Target_Country"]] %>% 
  rename_levels(changes = c("PHILIPPINES"         = "PHILLIPINES", 
                            "WEST PACIFIC WATERS" = "WESTPAC WATERS")) %>% 
  drop_levels(drop = "UNKNOWN") %>% 
  format_levels(proper_noun_phrase_vectorized)

Vietnam_bombs[["Mission_Day_Period"]] %>% 
  keep_levels(keep = c("D", "N", "M", "E")) %>% 
  format_levels(format_day_periods)

Vietnam_bombs[["Unit_Service"]] %>% 
  drop_levels(drop = "OTHER")

Vietnam_bombs[["Unit_Squadron"]] %>% 
  rename_similar_levels(changes = c("\\1 Fighter Squadron" = "(\\d+) ?FS$", 
                                    "\\1 Liaison Squadron" = "(\\d+) ?LS$", 
                                    "\\1 Squadron" = "(\\d+) ?S$", 
                                    "\\1 Squadron" = "(\\d+) ?SQ$", 
                                    "\\1 Tactical Reconnaissance Squadron" = "(\\d+) ?TRS$", 
                                    "\\1 School Squadron" = "(\\d+) ?SCHS$", 
                                    "\\1 Special Operations Squadron" = "(\\d+) ?SOS$", 
                                    "\\1 Special Operations Squadron" = "(\\d+) ?S0S$", 
                                    "\\1 Special Operations Support Squadron" = "(\\d+) ?SOSS$", 
                                    "\\1 Air Force" = "(\\d+) ?AF$", 
                                    "\\1 Base Support Team" = "(\\d+) ?BST$", 
                                    "\\1 Tactical Fighter Squadron" = "(\\d+) ?TFS$", 
                                    "\\1 Tactical Fighter Squadron" = "(\\d+) ?TF>$", 
                                    "\\1 Mission Area Group" = "(\\d+) ?MAG$", 
                                    "\\1 Mission Area Group" = "(\\d+) ?MAG1$", 
                                    "\\1 Wild Weasel Squadron" = "(\\d+) ?WWS$", 
                                    "\\1 Tactical Air Squadron" = "(\\d+) ?TAS$", 
                                    "\\1 Tactical Air Squadron" = "TAS1$", 
                                    "\\1 Tactical Air Support Squadron" = "(\\d+) ?TASS$", 
                                    "\\1 Heavy Squadron" = "(\\d+) ?HS$", 
                                    "\\1 Air Reserve Forces" = "(\\d+) ?ARF$", 
                                    "\\1 Bombardment Wing" = "(\\d+) ?BW$", 
                                    "\\1 Strategic Wing" = "(\\d+) ?SW$", 
                                    "\\1 Test & Evaluation Squadron" = "(\\d+) ?TES$", 
                                    "\\1 Special Air Mission" = "(\\d+) ?SAM$", 
                                    "\\1 Tactical Fighter Wing" = "(\\d+) ?TFW$", 
                                    "\\1 Test & Evaluation Wing" = "(\\d+) ?TEW$", 
                                    "\\1 Organizational Maintenance Squadron" = "(\\d+) ?OMS$", 
                                    "\\1 Tactical Airlift Wing" = "(\\d+) ?TAW$", 
                                    "\\1 Aerial Bombardment Wing" = "(\\d+) ?ABW$", 
                                    "\\1 Aerial Rescue and Recovery Squadron" = "(\\d+) ?ARRS$", 
                                    "\\1 Aerial Rescue and Recovery Squadron" = "(\\d+) ?AARS$", 
                                    "\\1 Air Reserve Squadron" = "(\\d+) ?ARS$", 
                                    "\\1 Tactical Squadron" = "(\\d+) ?TS$", 
                                    "\\1 Tactical Electronic Warfare Squadron" = "(\\d+) ?TEWS$", 
                                    "\\1 Tactical Reconnaissance Wing" = "(\\d+) ?TRW$", 
                                    "\\1 Operational Flight Squadron" = "(\\d+) ?OFS$", 
                                    "\\1 Reconnaissance Squadron" = "(\\d+) ?RS$", 
                                    "\\1 Special Operations Wing" = "(\\d+) ?SOW$", 
                                    "\\1 Weather Squadron" = "(\\d+) ?WS$", 
                                    "\\1 Weather Wing" = "(\\d+) ?WW$", 
                                    "\\1 Communications Squadron" = "(\\d+) ?CS$", 
                                    "\\1 Attack Squadron" = "(\\d+) ?ATK$", 
                                    "\\1 Carrier Task Group" = "(\\d+) ?CTG$", 
                                    "\\1 Carrier Task Force" = "(\\d+) ?CTF$", 
                                    "\\1 Airborne Command & Control Squadron" = "(\\d+) ?ACCS$", 
                                    "\\1 Airlift Squadron" = "(\\d+) ?AS$", 
                                    "\\1 Group Task Force Squadron" = "(\\d+) ?GTFS$")) %>% 
  drop_similar_levels(drop = c("^.{1,7}$"))

Vietnam_bombs[, Unit_Title := Unit_Service][["Unit_Title"]] %>% 
  rename_levels(changes = c("Korean Air Force"           = "KAF", 
                            "Royal Australian Air Force" = "RAAF", 
                            "Royal Laotian Air Force"    = "RLAF", 
                            "US Army"                    = "USA", 
                            "US Air Force"               = "USAF", 
                            "US Marine Corps"            = "USMC", 
                            "US Navy"                    = "USN", 
                            "Vietnamese Air Force"       = "VNAF"))
Vietnam_bombs[, Unit_Title := factor(trimws(paste(Unit_Title, Unit_Squadron)))]

Vietnam_bombs[["Mission_Function_Code"]] %>% 
  drop_similar_levels(drop = "\\d[A-Z]")

Vietnam_bombs[["Target_CloudCover"]] %>% 
  drop_levels(drop = "NOT OBS") %>% 
  format_levels(tolower)

Vietnam_bombs[["Target_Control"]] %>% 
  drop_levels(drop = "UNKNOWN") %>% 
  rename_similar_levels(changes = c("TARGET"   = "TGT", 
                                    "GROUND"   = "GRND", 
                                    "CENTER"   = "CTR", 
                                    "CONTROL"  = "CNTL", 
                                    "AIRBORNE" = "ABN", 
                                    "FORWARD"  = "FWD")) %>% 
  format_levels(tolower)

Vietnam_bombs[["Weapon_Class"]] %>% 
  format_levels(tolower)

Vietnam_bombs[["Takeoff_Location"]] %>% 
  format_levels(proper_noun_phrase_vectorized)

# type fixes
Vietnam_bombs[, Mission_Function_Code := as.integer(if_else(Mission_Function_Code == "", 
                                                            NA_character_, 
                                                            as.character(Mission_Function_Code)))]
Vietnam_bombs[, Mission_Function_Code := factor(Mission_Function_Code, exclude = NULL)]

# fill out matching codes and values
Vietnam_bombs %>% 
  fill_matching_values_(Mission_Function_Code, Mission_Function, backfill = TRUE, drop.codes = TRUE, drop.missing.levels = TRUE, NA_code = NA_character_)

Vietnam_bombs[, Mission_Function_Code := as.integer(Mission_Function_Code)]

# fill out missing countries by service
Vietnam_bombs %>% 
  fill_matching_values(code_col = "Unit_Service", value_col = "Unit_Country", drop.missing.levels = TRUE)

# reduced columns
Vietnam_bombs[, Target_Category := Target_Type][["Target_Category"]] %>% 
  reduce_levels(rules = target_categorizations)
Vietnam_bombs[, Target_Category := ordered_empty_at_end(Target_Category, "other")]

Vietnam_bombs[, Target_Visibility := Target_CloudCover][["Target_Visibility"]] %>% 
  reduce_levels(rules = visibility_categorizations, other = "fair")
Vietnam_bombs[, Target_Visibility := ordered(Target_Visibility, levels = c("poor", "fair", "good"))]
