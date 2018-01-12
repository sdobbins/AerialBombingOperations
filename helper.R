# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


### Imports -----------------------------------------------------------------

# import pluralizer
if (!for_publication) {
  source('~/Developer/Github/acid/acid.R')
} else {
  source('acid/acid.R')
}


### Local Values ------------------------------------------------------------

WW1_countries <- c("UK", "USA")
WW1_service <- c("GAR", "RAF", "USAAS")
WW2_countries <- c("UK", "USA")
WW2_service <- c("AF", "RAF", "RAAF", "RNZAF", "SAAF", "TAC")
WW2_theaters <- c("CBI", "ETO", "MTO", "PTO")
Korea_countries <- c("USA")
Korea_service <- c()
Vietnam_countries <- c("USA")
Vietnam_service <- c("KAF", "RAAF", "RLAF", "USA", "USAF", "USMC", "USN", "VNAF")
countries <- unique(c(WW1_countries, WW2_countries, Korea_countries, Vietnam_countries))
services <- unique(c(WW1_service, WW2_service, Korea_service, Vietnam_service))

upper_case_abbreviations <- c("AAA", "HQ", "RR", "USS")
upper_case_abbreviations_lower <- tolower(upper_case_abbreviations)
upper_case_set <- unique(c(cardinal_directions, countries, services, WW2_theaters, roman_numerals, upper_case_abbreviations))
upper_case_set_lower <- tolower(upper_case_set)

WW1_aircraft_letters <- c()
WW2_aircraft_letters <- c("SBD", "TBF")
Korea_aircraft_letters <- c()
Vietnam_aircraft_letters <- c()
aircraft_letters <- unique(c(WW1_aircraft_letters, WW2_aircraft_letters, Korea_aircraft_letters, Vietnam_aircraft_letters, roman_numerals))

English_preposition_exceptions <- c("in", "to")# Italy and Burma
English_prepositions_for_place_names <- English_prepositions %d% English_preposition_exceptions
English_prepositions_for_place_names_upper <- toupper(English_prepositions_for_place_names)

military_invariate_plurals <- c("anti-aircraft", "aircraft", "ammunition", "ammo", "personnel")

weapon_set_upper <- c("A", "ADSID", "DC", "DP", "FADSID", "FS", "GO", "GP", "HANDSID", "HD", "HE", "HELOSID", "HVAR", "IRID", "LD", "MAGID", "MP", "PH", "PHS", "PWP", "SAP", "THR", "WP", roman_numerals)#***
weapon_set_lower <- tolower(weapon_set_upper)


### Named Vectors -----------------------------------------------------------

### basic

months <- c("January" = 1L, 
            "February" = 2L, 
            "March" = 3L, 
            "April" = 4L, 
            "May" = 5L, 
            "June" = 6L, 
            "July" = 7L, 
            "August" = 8L, 
            "September" = 9L, 
            "October" = 10L, 
            "November" = 11L, 
            "December" = 12L)


### categorizations

visibility_categorizations <- c("good" = ending_with(any_of(c("good", "clear", "excellent", "undercast"))), 
                                "poor" = ending_with(any_of(c("poor", "clouds", "overcast"))), 
                                "fair" = ending_with(any_of(c("fair", "layers", "scattered"))), 
                                "poor" = ending_with(any_of(c("very"))))

target_categorizations <- c("town"           = ending_with(any_of(c("town", "business", "city", "house", "huts", "market", "monastery", "town", "urban", "village"))), 
                            "factory"        = ending_with(any_of(c("factory", "assembly", "blast", "construction", "engine", "furnace", "gasoline", "hydroelectric", "manufacturing", "plant", "power", "refinery", "works"))), 
                            "airfield"       = ending_with(any_of(c("airfield", "airdrome", "runway"))), 
                            "aircraft"       = ending_with(any_of(c("aircraft", "airframes", "mig", "seaplanes"))), 
                            "headquarters"   = ending_with(any_of(c("communications", "compound", "facility", "government", "headquarters", "management"))), 
                            "defenses"       = ending_with(any_of(c("defenses", "anti-aircraft", "battery", "defensive", "emplacement", "gun", "installation", "pillbox", "tower"))), 
                            "base"           = ending_with(any_of(c("base", "aresenal", "barracks", "buildings", "camp"))), 
                            "harbor"         = ending_with(any_of(c("harbor", "barges", "boats", "coastal", "dock", "ferry", "jetty", "shipyard", "vessels", "waterfront", "wharf"))), 
                            "munitions"      = ending_with(any_of(c("munitions", "ammunition", "explosives", "ordinances"))), 
                            "infrastructure" = ending_with(any_of(c("bridge", "canal", "river crossing", "tunnel"))), 
                            "rail"           = ending_with(any_of(c("rail", "junction", "marshalling", "railroad", "railway", "station", "trains", "yard"))), 
                            "road"           = ending_with(any_of(c("road", "highway", "locomotives", "transportation", "trucks", "vehicles"))), 
                            "supplies"       = ending_with(any_of(c("supplies", "depot", "dump", "equipment", "rations", "shipping", "storage", "warehouse"))), 
                            "troops"         = ending_with(any_of(c("troops", "artillery", "concentration", "enemy", "japanese", "moving target", "personnel", "support"))), 
                            "area"           = ending_with(any_of(c("area", "hill", "location", "place", "point", "position", "tactical", "target"))))


### rules

target_city_rules <- c(            "\\?", 
                                   "\\b[\\d.-]+\\b", 
                                   " ?\\d+.*", 
                                   "(( -)|( ?/)|( ?&)|( AND)|(- )|(: )|(; )|,|#).*", 
                       "\\1O\\2" = "([A-Z])0([A-Z]|\\b)", 
                       "ISLAND"  = "\\b(I ?SL?)\\b", 
                       "MOUNT"   = "\\b(MT)\\b", 
                       "POINT"   = "\\b(PT)\\b", 
                       "RIVER"   = "\\b(RIV)\\b", 
                                   "\\b(\\d+ ?)?((KM|MI) ?)?\\b(N|NORTH|NE|NORTHEAST|E|EAST|SE|S|SOUTH|SW|SOUTHWEST|W|WEST|NW|NORTHWEST|L|LEFT|R|RIGHT)\\.?( |$)(OF )?", 
                                   " ?(AIRFIELD|AREA|BANK|BOMB|BRIDGE|CHANNEL|COAST|DOCK|HARBOR|HOLLOW|JUNCTION|MINING|OIL REFINERY|PLANTATION|RAILROAD|RAILWAY|RANGE|REGION|REGULATOR|ROAD|RR|STATION|TOWN|VILLAGE|WATERWAY)S?\\b", 
                                   #"\\b(OFF|ON|BETWEEN|NEAR) ", 
                                   #"( OF)\\b", 
                                   "[^A-Z .'-]")

target_rules <- c(                            "/?ETC", 
                                              "[#?.:()]", 
                                              "\\bNO\\b.*", 
                                              "\\d+ | \\d+", 
                  " "                       = " +-? *", 
                  " , "                     = " ?, ?", 
                  " / "                     = " ?/ ?", 
                  " AND "                   = " ?[+&] ?", 
                  "ACKNOWLEDGED"            = "\\b(ACK)\\b", 
                  "ADMINISTRATIVE"          = "\\b(ADM?(IN[A-Z]*)?)\\b", 
                  "AIR"                     = "\\b(IAR|ARI)\\b", 
                  "AIR-GROUND"              = "\\b(AIRGROUND)\\b", 
                  "AIRCRAFT"                = "\\b((AC|A C)(RFT)?|AIR CRAFT|ARICRAFT)\\b", 
                  "AIRCRAFT FACTORY"        = "\\b(A FACTORY|A ?F)\\b", 
                  "AIRDROME"                = "\\b((AERO?D?|SERODR|AIRODR|AIRDR|ARIDR|IARDR|AIDR|AIRDR|AIRHO|AIR DR)[A-Z]*|DROMES?)\\b", 
                  "AIRFIELD"                = "\\b(AIR FIELDS?|AIRFLD|AIRFIEL|AIRFIELDS)\\b", 
                  "AIRFRAMES"               = "\\b(AIR ?FRAMES?)\\b", 
                  "AIRPORT"                 = "\\b(AIR[A-Z]*P[A-Z]*TS?)\\b", 
                  "AMMUNITION"              = "\\b(AMMO|AMMUN[A-Z]*)\\b", 
                  "ANTENNA"                 = "\\b(ANT)\\b", 
                  "ANTI-AIRCRAFT"           = "\\b(A ?A|ANTI ?-? ?AIR ?CRAFT|ANTIARICRAFT)\\b", 
                  "ANTI-AIRCRAFT ARTILLERY" = "\\b(AAA)\\b", 
                  "AREA"                    = "\\b(ARES?|ABEAS?|APEAS?|AREAS)\\b", 
                  "ARMORED VEHICLE"         = "\\b(A ?V)\\b", 
                  "ARSENAL"                 = "\\b(AESENAL|ARSENALS)\\b", 
                  "ARTILLERY"               = "\\b(ARTILLER)\\b", 
                  "ASSEMBLY"                = "\\b(ASSBLY)\\b", 
                  "BALL BEARINGS"           = "\\b(BB)\\b", 
                  "BARGES"                  = "\\b(BRAGES?|BARGE)\\b", 
                  "BARRACKS"                = "\\b(BR?KS?|BARRAC.*)\\b", 
                  "BASE"                    = "\\b(BASFS?|BASES)\\b", 
                  "BATTERY"                 = "\\b(BTY|BRTY|BTRY)\\b", 
                  "BEACH"                   = "\\b([BH]EACH(ES)?)\\b", 
                  "BEARINGS"                = "\\b(BEARING)\\b", 
                  "BIVOUAC"                 = "\\b(BIVOVAC|RIVOUAC)\\b", 
                  "BLAST"                   = "\\b(BLST)\\b", 
                  "BOATS"                   = "\\b(BOAT)\\b", 
                  "BRIDGE"                  = "\\b(BR|BR?I?D?GE?S?|BRID ?E?S?|8RIDGES?|GRIDGES?|BOIDGES?|BIRDGES?|BRIDGES)\\b", 
                  "BUILDINGS"               = "\\b(BLDG?S?|BUILDS?|BUILOINGS?|SUILDINGS?|BUILDI[A-Z]*)\\b", 
                  "BUNKER"                  = "\\b(BNKR)\\b", 
                  "BUSINESS"                = "\\b(BUSIHESS)\\b", 
                  "CALIBER"                 = "\\b(CAL)\\b", 
                  "CAMP"                    = "\\b(CAMPS)\\b", 
                  "CANAL"                   = "\\b(CANAI|CANALS)\\b", 
                  "CAVALRY"                 = "\\b(CVLRY?|CAVALARY)\\b", 
                  "CEMETERY"                = "\\b(CEMETARY)\\b", 
                  "CENTER"                  = "\\b(CENTRE?|CENTERS)\\b", 
                  "CHEMICAL"                = "\\b(CHEM)\\b", 
                  "CITY"                    = "\\b(CTY|CIIY|CITIES)\\b", 
                  "CIVILIAN"                = "\\b(CIV)\\b", 
                  "COASTAL"                 = "\\b(COAST|COASTAL[A-Z]*)\\b", 
                  "COMMERCIAL"              = "\\b(COM)\\b", 
                  "COMMAND"                 = "\\b(COMM|COMD)\\b", 
                  "COMMAND CONTROL CENTER"  = "\\b(CCC)\\b", 
                  "COMMAND POST"            = "\\b(CP)\\b", 
                  "COMMUNICATIONS"          = "\\b(COMMUNICATION)\\b", 
                  "COMPANY"                 = "\\b(CO|COMPAN[A-Z]*)\\b", 
                  "COMPONENTS"              = "\\b(COMPONENET|COMPONENT)\\b", 
                  "COMPOUND"                = "\\b(COMPOS?|CONPOUNDS?|CPMPOUNDS?|COMPOU[A-Z]*)\\b", 
                  "CONCENTRATION"           = "\\b(CONCT?S?|CONCENT[A-Z]*|CONSTRATIONS?|CONTRATIONS?|CQNCENTRATIONS?)\\b", 
                  "CONSTRUCTION"            = "\\b(CONST)\\b", 
                  "CONTAINER"               = "\\b(CONT)\\b", 
                  "COORDINATES"             = "\\b(COORDS?|COORDINATE)\\b", 
                  "COORDINATION"            = "\\b(COORDINATI[A-Z]*)\\b", 
                  "CROSSROADS"              = "\\b(XROADS?|CROSSROAD)\\b", 
                  "CRUISERS"                = "\\b(CRUISER)\\b", 
                  "DEFENSES"                = "\\b(DEFENCE?S|DEFENSE)\\b", 
                  "DEFENSIVE"               = "\\b(DEF)\\b", 
                  "DEPOT"                   = "\\b(DEPO|DEPOTS)\\b", 
                  "DESTROYERS"              = "\\b(DESTR?OYERS?)\\b", 
                  "DISPERSAL"               = "\\b(DISPRSL)\\b", 
                  "DISTRICT"                = "\\b(DISTRICT?S?)\\b", 
                  "DIVISION"                = "\\b(DIV)\\b", 
                  "DOCK"                    = "\\b(DDCKS?|DCXKS?|BOCKS?|DOCKS)\\b", 
                  "DOCKYARD"                = "\\b(DOCKYARS?|DOCKYARDS)\\b", 
                  "DRYDOCK"                 = "\\b(DR[IY]DOCKS?)\\b", 
                  "DUMP"                    = "\\b(DIMPS?|DOOPS?|DUMPS)\\b", 
                  "ELECTRIC"                = "\\b(ELCT|ELECT?)\\b", 
                  "EMPLACEMENT"             = "\\b(EMP|EMPL|EMPLACE[A-Z]*|IMPLACE[A-Z]*)\\b", 
                  "ENEMY"                   = "\\b(EN|ENEMIES)\\b", 
                  "ENGINE"                  = "\\b(ENG)\\b", 
                  "ENTRENCHMENT"            = "\\b(ENTRENCH[A-Z]*)\\b", 
                  "EQUIPMENT"               = "\\b(EQUIPT?)\\b", 
                  "EXPLOSIVES"              = "\\b(EXPLO[A-Z]*)\\b", 
                  "FACILITY"                = "\\b(FACILIT[A-Z]*)\\b", 
                  "FACTORY"                 = "\\b(FAC?T?|FCTY|FACTO|FACTOR[A-Z]+)\\b", 
                  "FERRY"                   = "\\b(FERRIES)\\b", 
                  "FLIGHT"                  = "\\b(FLT)\\b", 
                  "FURNACE"                 = "\\b(FRNCS?|FURNACES)\\b", 
                  "GASOLINE"                = "\\b(GAS)\\b", 
                  "GOVERNMENT"              = "\\b(GOVT|GOVERMENT)\\b", 
                  "GUN"                     = "\\b(GUM)\\b", 
                  "GUN POSITION"            = "\\b(G P|G POSITION)\\b", 
                  "HANGAR"                  = "\\b(HANG[AE]?R?S?)\\b", 
                  "HARBOR"                  = "\\b(HARBDR|HARBOURS?|HARBORS)\\b", 
                  "HEADQUARTERS"            = "\\b(HDOS?|HD?QR?S?|H ?Q ?S?|HEADQUATERS?|HEADQUARTER)\\b", 
                  "HEAVY"                   = "\\b(HVY)\\b", 
                  "HIDEOUT"                 = "\\b(HDGS?|HIDINGS?|HIDEOUTS)\\b", 
                  "HIGHWAY"                 = "\\b(HWY?S?|HTGHWAYS?|HIWAYS?|HIGHWAYS)\\b", 
                  "HILL"                    = "\\b(HILLSIDE|HILLS)\\b", 
                  "HOUSES"                  = "\\b(HOUSES)\\b", 
                  "HUTS"                    = "\\b(HUYS?|HUT)\\b", 
                  "HYDROELECTRIC"           = "\\b(HYDRIELECTRIC|HYDRO ELECTRIC)\\b", 
                  "INDUSTRIAL"              = "\\b(IND|INDUS[^Y]*)\\b", 
                  "INSTALLATION"            = "\\b(INST[A-Z]*|INSAT[A-Z]*|ISTAL[A-Z]*|IOSTAL[A-Z]*)\\b", 
                  "INTERSECTION"            = "\\b(INTERSEC[A-Z]*)\\b", 
                  "JAPANESE"                = "\\b(JAPS?|JAPANSE)\\b", 
                  "JETTY"                   = "\\b(JETTIES)\\b", 
                  "JUNCTION"                = "\\b(JTNS?|JCTS?|JUNC|JUNCT[A-Z]*|JNCTNS?|JCTIONS?)\\b", 
                  "LAUNCHER"                = "\\b(LNCHR)\\b", 
                  "LIGHT"                   = "\\b(LGT)\\b", 
                  "LOCATION"                = "\\b(LOCS?)\\b", 
                  "LOCOMOTIVES"             = "\\b(LOCOS?|LOCOMOTIVE)\\b", 
                  "LOOKOUT"                 = "\\b(LKOUT)\\b", 
                  "MANAGEMENT"              = "\\b(MGMT)\\b", 
                  "MANUFACTURING"           = "\\b(MFG|MANU|MANUFACTORING)\\b", 
                  "MARKET"                  = "\\b(MKT)\\b", 
                  "MARSHALLING"             = "\\b(MANSHALLING|MARSHALLIHG|MARSHALLIN ?G)\\b", 
                  "MARSHALLING YARD"        = "\\b(M[/ ]+Y(ARD)?)\\b", 
                  "MILITARY"                = "\\b(MIL)\\b", 
                  "MISCELLANEOUS"           = "\\b(MISCEL[A-Z]*)\\b", 
                  "MONASTERY"               = "\\b(MON[AE]ST[A-Z]*)\\b", 
                  "MOTOR VEHICLES"          = "\\b(MV)\\b", 
                  "MOVING TARGET"           = "\\b(M ?TS?)\\b", 
                  "MUNITIONS"               = "\\b(MUNITION)\\b", 
                  "OFFICE"                  = "\\b(OFFICES)\\b", 
                  "OFFICERS"                = "\\b(OFFIC|OFFICIERS?)\\b", 
                  "ORDINANCES"              = "\\b(ORD|ORDI?NANCE)\\b", 
                  "PARK/STOP"               = "\\b(PRK[ /]ST)\\b", 
                  "PARK"                    = "\\b(PRK)\\b", 
                  "PERSONNEL"               = "\\b(PER?S?ONN?EL|PERSONN[A-Z]*)\\b", 
                  "PETROL"                  = "\\b(POL)\\b", 
                  "PILLBOXES"               = "\\b(PILL BOX[A-Z]*)\\b", 
                  "PLACE"                   = "\\b(PL)\\b", 
                  "PLANT"                   = "\\b(PLN?TS?|PLANTS)\\b", 
                  "PLANTATION"              = "\\b(PLANTAT[A-Z]*)\\b", 
                  "POINT"                   = "\\b(PT|POINTS)\\b", 
                  "PONTOON"                 = "\\b(PANTOONS?|PONTOONS)\\b", 
                  "POPULATION"              = "\\b(POP[A-Z]*N)\\b", 
                  "POSITION"                = "\\b(POS|P0SI[A-Z]*|PDSI[A-Z]*|POSI[A-Z]*|POI?STIONS?)\\b", 
                  "\\1 \\2"                 = "\\b(POWER)([A-Z]+)\\b", 
                  "POWER"                   = "\\b(PWR)\\b", 
                  "RAIL JUNCTION"           = "\\b(RAIL J)\\b", 
                  "RAILROAD"                = "\\b(R ?RS?|[A-Z]AILROADS?|RAIL ROADS?|ROALROADS?|RAILROADS)\\b", 
                  "RAILROAD BRIDGE"         = "\\b(RRB|RRBRIDGE)\\b", 
                  "RAILWAY"                 = "\\b(RLWY|RAILWAYS)\\b", 
                  "RAILYARD"                = "\\b(RAILYARDS)\\b", 
                  "RATIONS"                 = "\\b(RATION)\\b", 
                  "RECONNAISSANCE"          = "\\b(RECON)\\b", 
                  "REFINERY"                = "\\b(REF|R[EI]FINER[A-Z]*)\\b", 
                  "REINFORCEMENTS"          = "\\b(REINFORC[A-Z]*)\\b", 
                  "REPORTED"                = "\\b(RPTD)\\b", 
                  "RESIDENTIAL"             = "\\b(RESIDENTI[A-Z]*)\\b", 
                  "REVETMENT"               = "\\b(REVETMENTS)\\b", 
                  "RIVER CROSSING"          = "\\b(RIV[A-Z]* CR[A-Z]*|RIV[A-Z]* CROSS NG)\\b", 
                  "ROAD"                    = "\\b(RD)\\b", 
                  "ROCKET"                  = "\\b(RCKT)\\b", 
                  "RUNWAY"                  = "\\b(RWY|RWAY|RUNWA|RUNWAYS)\\b", 
                  "SCHOONER"                = "\\b(SCHOOONER)\\b", 
                  "SEAPLANES"               = "\\b(SEAPIANES?|SEA PLANES?|SEPLANES?|SEAPLANE)\\b", 
                  "SHIPPING"                = "\\b(SHII?PP[A-Z]*)\\b", 
                  "SHIPYARD"                = "\\b(SHIPVARDS?|SHIP ?YARDS?)\\b", 
                  "SIDING"                  = "\\b(SIDINQS?|SIDINGS)\\b", 
                  "SMELTING"                = "\\b(SMELTER)\\b", 
                  "STATION"                 = "\\b(STAS?|STNS?|STATI?O?N?S?)\\b", 
                  "STORAGE"                 = "\\b(STGE|STORA?|STOEAGE|STORAG)\\b", 
                  "SUBMARINE"               = "\\b(SUB)\b", 
                  "SULFURIC"                = "\\b(SULPHURIC)\\b", 
                  "SUPPLIES"                = "\\b(SUPPL?Y?S?|BUPPLIES|SIPPLY|SOPPLIES)\\b", 
                  "SUPPORT"                 = "\\b(SUP[A-Z]*ORT|SUPPOER)\\b", 
                  "SUSPECTED"               = "\\b(SUSP)\\b", 
                  "SYNTHETIC"               = "\\b(SYN)\\b", 
                  "TACTICAL"                = "\\b(TA?CT)\\b", 
                  "TANKS"                   = "\\b(TNKS?|TANK)\\b", 
                  "TARGET"                  = "\\b(TGTS?|TARGETS)\\b", 
                  "TRENCHES"                = "\\b(TRNCHS?|TRANCH(ES)?)\\b", 
                  "TOWER"                   = "\\b(TOWENS)\\b", 
                  "TOWN"                    = "\\b(TDWNS?|TOWMS?|TOWNS)\\b", 
                  "TRAILER"                 = "\\b(TRLR)\\b", 
                  "TRANSSHIPMENT POINT"     = "\\b(TRANS POINT)\\b", 
                  "TRANSSHIPMENT"           = "\\b(TRANSHIPMENT)\\b", 
                  "TRANSPORTS"              = "\\b(TRANS|TRANSPOS?|TRANSPONTS?|TRASNPORTS?|TRANSPORT)\\b", 
                  "TRANSPORTATION"          = "\\b(TRANSPORTA[A-Z]*)\\b", 
                  "TROOPS"                  = "\\b(IROOPS?|TOOOO|TROOP)\\b", 
                  "TRUCKS"                  = "\\b(TRK|TRUCK)\\b", 
                  "TUNNEL"                  = "\\b(TUNNELS)\\b", 
                  "UNDERGROWTH"             = "\\b(UNDENGROWTH)\\b", 
                  "UNIDENTIFIED"            = "\\b(UNI?D|UNI?DENT)\\b", 
                  "UNKNOWN"                 = "\\b(UNK)\\b", 
                  "URBAN"                   = "\\b(URSAN)\\b", 
                  "UTILITIES"               = "\\b(UTILITES)\\b", 
                  "VEHICLES"                = "\\b(VEHICS?|VEHICLE)\\b", 
                  "VESSELS"                 = "\\b(VESSEL)\\b", 
                  "VILLAGE"                 = "\\b(VILIAGES?|VTLLAGES?|VILLAGES)\\b", 
                  "WAREHOUSE"               = "\\b(WARE[A-Z]? HO[A-Z]*|DAREHO[A-Z]*|WHARES?|WHAREHO[A-Z]*|WAREHO[A-Z]*)\\b", 
                  "WATERFRONT"              = "\\b(WATER FRONT|BATERFRONT)\\b", 
                  "WATERWAY"                = "\\b(WATERSAYS?|WATERWAYS)\\b", 
                  "WHARF"                   = "\\b(WHARVES|WHARFS)\\b", 
                  "WORKS"                   = "\\b(WR?KS?)\\b", 
                  "WORKSHOP"                = "\\b(WORKSHO[A-Z]*)\\b", 
                  "YARD"                    = "\\b(YDS?|YARUS?|YAROS?|YARDES?|YARDS)\\b", 
                  ", "                      = " , ", 
                  "/"                       = " / ")

weapon_rules <- c("ANTI-MATERIAL"  = "\\b(ANTI-MTL?|AN MTL?)\\b", 
                  "ANTI-PERSONNEL" = "\\b(AN PR/MT|ANTI-PER|ANTIPERS)\\b", 
                  "AUXILIARY"      = "\\b(AUX)\\b", 
                  "CLUSTERS"       = "\\b(CLS?|CLUSTRS?)\\b", 
                  "DELIVERY"       = "\\b(DELIV)\\b", 
                  "DEMOLITION"     = "\\b(DEMOL)\\b", 
                  "INCENDIARY"     = "\\b(INCEN)\\b", 
                  "LASER-GUIDED"   = "\\b(LASER G)\\b", 
                  "MISCELLANEOUS"  = "\\b(MISC)\\b", 
                  "ROCKET"         = "\\b(RKT)\\b")

Vietnam_operation_rules <- c(                    " ?[%&].*", 
                                                 "NOT APPLICABLE|UNK", 
                                                 "\\b(- )", 
                                                 " ?(UT.*|QR.*|NRB.*)", 
                             "\\2"             = "\\b(HJ|XT)([A-Z]+)", 
                             " \\1"            = "[/&-](\\d)", 
                             "\\1 \\2"         = "([A-Z])(\\d+)", 
                             "HAWK"            = "AHWK|KAWK|HAWL|HAWKK", 
                             "ROLLING THUNDER" = "ROLLING THUND?", 
                             "SUN"             = "SVN", 
                             "AEROSOL"         = "\\bAER[LOST]*[A-Z]*", 
                             "ALASH"           = "\\bALA?SH[A-Z]*", 
                             "AMTRACK"         = "\\bA?MTRACK[A-Z]*", 
                             "ARGON"           = "\\b[AS]?RG[NOB]*[A-Z]*", 
                             "BABBIT"          = "\\b(BABB|BABBUT)[A-Z]*", 
                             "BAFFLE"          = "\\bJ?BAFFLE[A-Z]*", 
                             "BERSERK"         = "\\b(RERSERK|BSRAERK|J?BE[FR]S[ERK]+[A-Z]*)", 
                             "BLABBER"         = "\\bBLABBER[A-Z]*", 
                             "BLADE"           = "\\bBLAD[A-Z]*", 
                             "BLUE TREE"       = "\\bBLUE ?T[RE]EE[A-Z]*", 
                             "BOBCAT"          = "\\bBOBCAT[A-Z]*", 
                             "BRAVO"           = "\\bBRAV[A-Z]*", 
                             "BUCKSHOT"        = "\\bBUC[KH]SHOT[A-Z]*", 
                             "BULLWHIP"        = "\\bB?ULLWH[A-Z]*", 
                             "CHARLIE"         = "\\bCHARLIE[A-Z]*", 
                             "COBRA"           = "\\bC?OBRA[A-Z]*", 
                             "COMBAT SKYSPOT"  = "\\bCOMBAT ?[SKYPORT]+[A-Z]*", 
                             "COMBO"           = "\\bJ?C?OMBO[A-Z]*", 
                             "COMFORT"         = "\\bC?OMFORT[A-Z]*", 
                             "DAD"             = "\\bDAD[A-Z]*", 
                             "DAMSEL"          = "\\bDA[MN][SEL]+[A-Z]*", 
                             "DEVIL"           = "\\bDEVIL[A-Z]*", 
                             "DOWNY"           = "\\bDOWNE?Y[A-Z]*", 
                             "ECHO"            = "\\bECHO[A-Z]*", 
                             "ELECTRA"         = "\\bJ?E?LECTRA", 
                             "GINKO"           = "\\b[JX]G[I1]?NKO[A-Z]*", 
                             "GOLF"            = "\\bGOLF[A-Z]*", 
                             "HAGGLE"          = "\\b[HA]+GGLE[A-Z]*", 
                             "HEMP"            = "\\bHEMP[A-Z]*", 
                             "HILLSBRO"        = "\\bHILLSB[A-Z]*", 
                             "HIPSTER"         = "\\bH?[UI]PST[A-Z]*", 
                             "HOTEL"           = "\\bHOTEL[A-Z]*", 
                             "ICON"            = "\\bI[OC]+N[A-Z]*", 
                             "INDIA"           = "\\bINDIA[A-Z]*", 
                             "JALOPY"          = "\\b[HJ]ALOPY[A-Z]*", 
                             "JIM"             = "\\bJ[IU]M[A-Z]*", 
                             "JULIET"          = "\\bJULIE[A-Z]*", 
                             "JUNK"            = "\\bJ?[4J]UNK[A-Z]*", 
                             "KING COBRA"      = "\\bK?ING COBRA[A-Z]*", 
                             "KINGDOM"         = "\\b(K?I[NM]G[DL]|KINDOM)[A-Z]*", 
                             "LAOS VIETNAM"    = "\\b[AJ]?L[A-Z0-9/]*[ 0]*[VI][A-Z0-9]*[TN][A-Z]*", 
                             "LAOS VIETNAM"    = "\\bA[LAOUS]* VI[RTNAM]*[A-Z]*", 
                             "LAOS"            = "\\bLA[OUS]+[A-Z]*", 
                             "LASH"            = "\\bLAS[A-Z]*", 
                             "LEOTARD"         = "\\bL[EO/]+TARD[A-Z]*", 
                             "LIMA"            = "\\bLIMA[A-Z]*", 
                             "MIKE"            = "\\bMIKE[A-Z]*", 
                             "NAPALM"          = "\\b[J]?NAP[ALM]+[A-Z]*", 
                             "NOVEMBER"        = "\\b(NOV[EMBR]+[A-Z]*|N\\/VEMBER[A-Z]*)", 
                             "OSCAR"           = "\\bOSCAR[A-Z]*", 
                             "OXFORD"          = "\\bOXFO[ERD]+[A-Z]*", 
                             "PANAMA"          = "\\bPA?NAMA[A-Z]*", 
                             "PAPA"            = "\\bPAPA[A-Z]*", 
                             "PAVEN"           = "\\bPAVEN[A-Z]*", 
                             "PETRO"           = "\\bPE[TY]RO[A-Z]*", 
                             "PETTICOAT"       = "\\bP?ETTI[A-Z]*T[A-Z]*", 
                             "POLKA DOT"       = "\\bPOLKADOT[A-Z]*", 
                             "PULLOVER"        = "\\bULLOVER[A-Z]*", 
                             "QUEBEC"          = "\\bQUEBIC[A-Z]*", 
                             "RAMROD"          = "\\bR?AMROD[A-Z]*", 
                             "RED CROWN"       = "\\bRED ?CR[OWN]+[A-Z]*", 
                             "REDNECK"         = "\\bREDN[A-Z]*K[A-Z]*", 
                             "ROMEO"           = "\\bRO[MEO]+[A-Z]*", 
                             "REUBEN"          = "\\bR[EU]+BEN[A-Z]*", 
                             "SABRE"           = "\\bS[AZ]B[RE]*[A-Z]*", 
                             "SHADOW"          = "\\b[SW]HADOW[A-Z]*", 
                             "SHELLAC"         = "\\bS?[HN]ELLAC[A-Z]*", 
                             "SIERRA"          = "\\bS[IERA]{4,}[A-Z]*", 
                             "SINFUL"          = "\\bSIN[DF]UL[A-Z]*", 
                             "SLOVAK"          = "\\bSLOVA[CK]+[A-Z]*", 
                             "SNAIL"           = "\\bSNAIL?[A-Z]*", 
                             "SOUTH VIETNAM"   = "\\bS(VSTH)? VIETNAM[A-Z]*", 
                             "STORMY"          = "\\b[GFJ]*STORM[ Y]+[A-Z]*", 
                             "SUN DOG"         = "\\bSUN DO[BG][A-Z]*", 
                             "THERMAL"         = "\\bT?HERMAL[A-Z]*", 
                             "TIGER CUB"       = "\\bT?IGER CUB[A-Z]*", 
                             "TRICYCLE"        = "\\bT?R[IU]C[UY]CLE[A-Z]*", 
                             "TRUMP"           = "\\bTRUM?P[A-Z]*", 
                             "UNIFORM"         = "\\bUNI[FORM]+[A-Z]*", 
                             "VACCUUM"         = "\\bJ?VAC[CUM]+[A-Z]*", 
                             "VERMIN"          = "\\bVERMIN[A-Z]*", 
                             "VICE SQUAD"      = "\\bVICE ?SQ[UAD]+[A-Z]*", 
                             "VIETNAM"         = "\\bA?VIETNAM[A-Z]*", 
                             "YANKEE"          = "\\bYANKEE[A-Z]*", 
                             "YELLOW JACKET"   = "\\bYELLOW JACK[A-Z]*", 
                             "YOUNG TIGER"     = "\\b(Y?OUNG|YOUMG|YONUG|YPUNG) T[IU]GER[A-Z]*", 
                             "YOYO"            = "\\bJ?(Y-Y-|YOYO|YOUO|UOUO|UOYO|OOYO)[A-Z]*", 
                             "WATERBOY"        = "\\bWA?TERB[A-Z]*", 
                             "WHISKEY"         = "\\bWHIS[KEY]+[A-Z]*", 
                             "ZULU"            = "\\bZULU[A-Z]*", 
                                                 " +$")

Vietnam_operation_rules2 <- c(        " ?- ?.*", 
                                      " ?\\d.*", 
                                      " ?ZERO|ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|SIXTY|([IVX]+\\b)", 
                              "\\3" = "((ARC|FINE|SUN|[LR][IU]GHT) )?([A-Z]* ?[A-Z]*)( (DUPT|SUPR|SUPT))?", 
                                      "SSY", 
                                      " +$", 
                                      " .{1,2}$", 
                                      "^.{1,2}$")


### Dictionary Supplementation ----------------------------------------------

other_target_names <- c("BOMBED", "DIVE", "DIVE BOMBED", "MISSION", "SHORE", "TORPEDO", "WRECK")
target_names <- c(names(target_rules) %whichlike% "^[A-Z]", other_target_names)
target_names <- toupper(sort(get_singular_and_plural(tolower(target_names))))

specific_terms <- tolower(target_names)
other_specific_terms <- read_list_of_words('data/other specific terms.txt')
other_specific_terms <- get_singular_and_plural(other_specific_terms, data_in = 'singular')

#dictionary_10k_plus <- c(dictionary_10k, specific_terms, other_specific_terms)
dictionary_20k_plus <- c(dictionary_20k, specific_terms, other_specific_terms)


### Capitalization Supplementation ------------------------------------------

proper_aircraft_noun <- function(word) {
  if (word %in% aircraft_letters) {
    return (word)
  } else {
    return (capitalize(word))
  }
}

proper_aircraft_nouns <- function(words) {
  return (if_else(words %in% aircraft_letters | 
                    (regexpr(pattern = "-|\\d", words) > 0L & regexpr(pattern = "[A-Za-z]{6,}", words) == -1L), 
                  words, 
                  capitalize_phrases(words)))
}

proper_noun_phrase_aircraft <- function(line) {
  return (paste(proper_aircraft_nouns(strsplit(line, split = " ", fixed = TRUE)[[1]]), collapse = " "))
}

proper_aircraft_noun_phrases <- function(lines) {
  lines_mod <- if_else(lines == "", "`", lines)
  lines_mod <- strsplit(lines_mod, split = " ", fixed = TRUE)
  lines_mod <- split(proper_aircraft_nouns(unlist(lines_mod, use.names = FALSE)), rep(1:length(lines), lengths(lines_mod)))
  lines_reduced <- map_chr(lines_mod, paste0, collapse = " ")
  return (if_else(lines == "", "", lines_reduced))
}

weapon <- function(word) {
  if (grepl("[-\\d]", word) || word %in% weapon_set_upper) {
    return (word)
  } else if (word %in% measurement_units_upper) {
    return (tolower(word))
  } else {
    return (capitalize(word))
  }
}

weapons <- function(words) {
  return (if_else(grepl("[0-9-]", words) | words %in% weapon_set_upper, 
                  words, 
                  if_else(words %in% measurement_units_upper, 
                          tolower(words), 
                          capitalize_phrases(words))))
}

weapons2 <- function(words) {
  not_weapon <- !(grepl("[0-9-]", words) | words %in% weapon_set_upper)
  is_measuring_unit <- words %in% measurement_units_upper
  words[not_weapon & is_measuring_unit] <- tolower(words[not_weapon & is_measuring_unit])
  words[not_weapon & !is_measuring_unit] <- capitalize_phrases(words[not_weapon & !is_measuring_unit])
  return (words)
}

weapon_phrase <- function(line) {
  return (paste(weapons(strsplit(line, split = " ", fixed = TRUE)[[1]]), collapse = " "))
}

weapon_phrases <- function(lines) {
  lines_mod <- if_else(lines == "", "`", lines)
  lines_mod <- strsplit(lines_mod, split = " ", fixed = TRUE)
  lines_mod <- split(weapons(unlist(lines_mod, use.names = FALSE)), rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- map_chr(lines_mod, paste0, collapse = " ")
  return (if_else(lines == "", "", lines_mod))
}


### Tooltip Helper Functions ------------------------------------------------

date_string <- function(month_names, day_strings, year_strings) {
  return (paste0("On ", month_names, " ", day_strings, ", ", year_strings, ","))
}

date_time_string <- function(date_strings, time_strings, empty = "") {
  return (if_else(time_strings == empty, 
                  date_strings, 
                  paste0(date_strings, " at ", time_strings, " hours,")))
}

date_period_time_string <- function(date_strings, period_strings, time_strings, empty = "") {
  return (if_else(time_strings == empty, 
                  if_else(period_strings == empty, 
                          date_strings, 
                          paste0(date_strings, " during the ", period_strings, ",")), 
                  paste0(date_strings, " at ", time_strings, " hours,")))
}

bomb_string <- function(weight, bomb, empty = "") {
  if (is.na(weight)) {
    if (bomb == empty) {
      return ("some bombs on")
    } else {
      return (paste0("some ", bomb, " on"))
    }
  } else {
    if (bomb == empty) {
      return (paste0(commas_string(weight), " pounds of bombs on"))
    } else {
      return (paste0(commas_string(weight), " pounds of ", bomb, " on"))
    }
  }
}

bomb_strings <- function(weights, bombs, empty = "") {
  results <- rep("", length(weights))
  NA_weights <- is.na(weights)
  results[NA_weights] <- "some "
  results[!NA_weights] <- paste0(commas_strings(weights[!NA_weights]), " pounds of ")
  empty_bombs <- bombs == empty
  results[empty_bombs] <- paste0(results[empty_bombs], "bombs on")
  results[!empty_bombs] <- paste0(results[!empty_bombs], bombs[!empty_bombs], " on")
  return (results)
}

aircraft_numtype_string <- function(num, type, empty = "") {
  if (is.na(num)) {
    if (type == empty) {
      return ("some aircraft")
    } else {
      return (paste0("some ", type, "s"))
    }
  } else if (num == 1) {
    if (type == empty) {
      return ("1 aircraft")
    } else {
      return (paste0("1 ", type))
    }
  } else {
    if (type == empty) {
      return (paste0(as.character(num), " aircraft"))
    } else {
      return (paste0(as.character(num), " ", type, "s"))
    }
  }
}

aircraft_numtype_strings <- function(nums, types, empty = "") {
  results <- rep("", length(nums))
  
  NA_nums <- is.na(nums)
  single_nums <- !NA_nums & nums == 1L
  multiple_nums <- !NA_nums & nums > 1L
  results[NA_nums] <- "some"
  results[single_nums] <- "1"
  results[multiple_nums] <- as.character(nums[multiple_nums])
  
  non_empty_types <- types != empty
  non_empty_types_strings <- as.character(types[non_empty_types])
  results[non_empty_types] <- paste0(results[non_empty_types], ' <a href="http://en.wikipedia.org/wiki/', non_empty_types_strings, '">', non_empty_types_strings, "</a>")
  
  results <- paste0(results, " aircraft")
  
  # plural_nums <- empty_nums | multiple_nums
  # results[plural_nums] <- paste0(results[plural_nums], "s")
  return (results)
}

aircraft_string <- function(numtype, division, empty = "") {
  if (division == empty) {
    return (paste0(numtype, " dropped"))
  } else {
    return (paste0(numtype, " of the ", division, " dropped"))
  }
}

aircraft_strings <- function(numtypes, divisions, empty = "") {
  return (if_else(divisions == empty, 
                  paste0(numtypes, " dropped"), 
                  paste0(numtypes, " of the ", divisions, " dropped")))
}

target_type_string <- function(type, empty = "") {
  if (type == empty) {
    return ("a target")
  } else {
    return (fix_article(type, military_invariate_plurals))
  }
}

target_type_strings <- function(types, empty = "") {
  if (is.factor(types)) {
    return (if_else(types == empty, 
                    "a target", 
                    fix_articles(as.character(types), military_invariate_plurals)))
  } else {
    return (if_else(types == empty, 
                    "a target", 
                    fix_articles(types, military_invariate_plurals)))
  }
}

target_area_string <- function(area, empty = "") {
  if (area == empty) {
    return ("in this area")
  } else {
    return (paste0("in ", area))
  }
}

target_area_strings <- function(areas, empty = "") {
  results <- rep("in ", length(areas))
  empty_areas <- areas == empty
  results[empty_areas] <- paste0(results[empty_areas], "this area")
  results[!empty_areas] <- paste0(results[!empty_areas], '<a href="https://en.wikipedia.org/wiki/', areas[!empty_areas], '">', areas[!empty_areas], "</a>")
  return (results)
}

target_location_string <- function(city, country, empty = "") {
  if (city == empty) {
    if (country == empty) {
      return ("in this area")
    } else {
      return (paste0("in this area of ", country))
    }
  } else {
    if (country == empty) {
      return (paste0("in ", city))
    } else {
      return (paste0("in ", city, ", ", country))
    }
  }
}

target_location_strings <- function(cities, countries, empty = "") {
  results <- rep("in ", length(cities))
  
  empty_cities <- cities == empty
  empty_countries <- countries == empty
  results[empty_cities] <- paste0(results[empty_cities], "this area")
  only_country_non_empty <- empty_cities & !empty_countries
  results[only_country_non_empty] <- paste0(results[only_country_non_empty], " of ", countries[only_country_non_empty])
  
  non_link_cities <- cities %exactlylike% " "
  link_cities <- !non_link_cities & !empty_cities
  results[non_link_cities] <- paste0(results[non_link_cities], cities[non_link_cities])
  results[link_cities] <- paste0(results[link_cities], '<a href="https://en.wikipedia.org/wiki/', cities[link_cities], '">', cities[link_cities], "</a>")
  
  both_non_empty <- !empty_cities & !empty_countries
  results[both_non_empty] <- paste0(results[both_non_empty], ", ", countries[both_non_empty])
  return (results)
}


### Formatting --------------------------------------------------------------

format_aircraft_types <- function(types) {
  return (gsub(pattern = "([A-Za-z]+)[ ./]?(\\d+[A-Za-z]*)(.*)", replacement = "\\1-\\2", types))
}

format_military_times <- function(digits) {
  return (gsubs(digits, changes = c("\\1:\\2"  = "^(\\d{1,2}):(\\d{2}):(\\d{2})", 
                                    "\\1:\\2"  = "^(\\d{1,2})(\\d{2})", 
                                    "\\1:\\20" = "^(\\d)([03])$", 
                                    "\\1:00"   = "^(1[0-9]|2[0-3])$", 
                                    "\\1:\\20" = "^(\\d)(\\d)$", 
                                    "\\1:00"   = "^(\\d)$")))
}

format_military_times_orig <- function(digits) {
  return (gsub(pattern = "^(\\d)$", replacement = "\\1:00", 
          gsub(pattern = "^(\\d)(\\d)$", replacement = "\\1:\\20", 
          gsub(pattern = "^(1[0-9]|2[0-3])$", replacement = "\\1:00", 
          gsub(pattern = "^(\\d)([03])$", replacement = "\\1:\\20", 
          gsub(pattern = "^(\\d{1,2})(\\d{2})", replacement = "\\1:\\2", 
          gsub(pattern = "^(\\d{1,2}):(\\d{2}):(\\d{2})", replacement = "\\1:\\2", digits)))))))
}

ampm_to_24_hour <- function(times) {
  return (gsubs(times, changes = c(           " ?[Aa][Mm]", 
                                   "13:\\1" = "^0?1:(\\d{2}) ?[Pp][Mm]", 
                                   "14:\\1" = "^0?2:(\\d{2}) ?[Pp][Mm]", 
                                   "15:\\1" = "^0?3:(\\d{2}) ?[Pp][Mm]", 
                                   "16:\\1" = "^0?4:(\\d{2}) ?[Pp][Mm]", 
                                   "17:\\1" = "^0?5:(\\d{2}) ?[Pp][Mm]", 
                                   "18:\\1" = "^0?6:(\\d{2}) ?[Pp][Mm]", 
                                   "19:\\1" = "^0?7:(\\d{2}) ?[Pp][Mm]", 
                                   "20:\\1" = "^0?8:(\\d{2}) ?[Pp][Mm]", 
                                   "21:\\1" = "^0?9:(\\d{2}) ?[Pp][Mm]", 
                                   "22:\\1" = "^10:(\\d{2}) ?[Pp][Mm]", 
                                   "23:\\1" = "^11:(\\d{2}) ?[Pp][Mm]", 
                                   "00:\\1" = "^12:(\\d{2}) ?[Pp][Mm]")))
}

ampm_to_24_hour_orig <- function(times) {
  return (gsub(pattern = "^12:(\\d{2}) ?[Pp][Mm]",  replacement = "00:\\1", 
          gsub(pattern = "^11:(\\d{2}) ?[Pp][Mm]",  replacement = "23:\\1", 
          gsub(pattern = "^10:(\\d{2}) ?[Pp][Mm]",  replacement = "22:\\1", 
          gsub(pattern = "^0?9:(\\d{2}) ?[Pp][Mm]", replacement = "21:\\1", 
          gsub(pattern = "^0?8:(\\d{2}) ?[Pp][Mm]", replacement = "20:\\1", 
          gsub(pattern = "^0?7:(\\d{2}) ?[Pp][Mm]", replacement = "19:\\1", 
          gsub(pattern = "^0?6:(\\d{2}) ?[Pp][Mm]", replacement = "18:\\1", 
          gsub(pattern = "^0?5:(\\d{2}) ?[Pp][Mm]", replacement = "17:\\1", 
          gsub(pattern = "^0?4:(\\d{2}) ?[Pp][Mm]", replacement = "16:\\1", 
          gsub(pattern = "^0?3:(\\d{2}) ?[Pp][Mm]", replacement = "15:\\1", 
          gsub(pattern = "^0?2:(\\d{2}) ?[Pp][Mm]", replacement = "14:\\1", 
          gsub(pattern = "^0?1:(\\d{2}) ?[Pp][Mm]", replacement = "13:\\1", 
          gsub(pattern = " ?[Aa][Mm]", replacement = "", times))))))))))))))
}

month_num_to_name <- function(month_strings) {
  month_ints <- as.integer(month_strings)
  results <- rep("", length(month_strings))
  month_names <- names(months)
  for (month_name in month_names) {
    results[month_ints == months[[month_name]]] <- month_name
  }
  return (results)
}


### Bomb Damage Estimator ---------------------------------------------------

damage_radius <- function(bomb_weights) {
  return (sqrt(pmax((if_else(is.na(bomb_weights), 100L, bomb_weights)), 100L) * 2 / pi))
}
