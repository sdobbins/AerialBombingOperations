# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


### Constants ---------------------------------------------------------------

vowels <- c("a", "e", "i", "o", "u")
vowels_y <- c(vowels, "y")

consonants_y <- letters %d% vowels
consonants <- consonants_y %d% "y"

articles <- c("el", "l", "la", "le", "les", "the")
conjunctions <- c("and", "or")
negations <- c("no", "not")

English_prepositions <- c("above", "around", "at", "away", "behind", "between", "close", "far", "from", "in", "inside", "into", "near", "of", "off", "on", "outside", "to", "top", "toward", "towards", "under", "underneath")
English_prepositions_upper <- toupper(English_prepositions)

foreign_prepositions <- list()
foreign_prepositions[["Germany"]] <- c("ab", "am", "an", "ans", "anstatt", "auf", "aufs", "aus", "ausser", "ausserhalb", "bei", "beim", "bis", "diesseits", "durch", "durchs", "entlang", "fur", "furs", "gegen", "gegenuber", "hinter", "innerhalb", "jenseits", "mit", "nach", "neben", "im", "in", "ins", "ohne", "seit", "statt", "trotz", "uber", "ubers", "um", "ums", "umweit", "unter", "unters", "vom", "von", "vor", "vorm", "wegen", "wehrend", "zu", "zum", "zur", "zwischen")
foreign_prepositions[["France"]] <- c("a", "apres", "au", "aupres", "aux", "avant", "avec", "chez", "contre", "d", "dans", "de", "depuis", "derriere", "des", "devant", "du", "en", "entre", "envers", "environment", "hors", "jusqu", "jusque", "malgre", "moins", "moyennant", "par", "parmi", "pendant", "plus", "pour", "sans", "sauf", "selon", "sous", "sur", "vers", "versus", "via")
foreign_prepositions[["Italy"]] <- c("a", "accanto", "ad", "al", "all", "attraverso", "come", "con", "contro", "da", "dentro", "di", "dietro", "dopo", "durante", "eccetto", "fino", "fuori", "in", "intorno", "giu", "la", "ma", "nonostante", "ogni", "opposto", "per", "piu", "poi", "poiche", "prima", "prossimo", "quando", "riguardo", "senza", "sopra", "sotto", "su", "tra", "tramite", "verso", "vicino")
prepositions <- c(English_prepositions, foreign_prepositions)

stop_words <- unique(c(articles, conjunctions, negations, English_prepositions))

measurement_units <- c("km", "m", "cm", "mm", "mi", "in", "ft", "nm", "kg", "lb")
measurement_units_upper <- toupper(measurement_units)

ordinal_markers <- c("nd", "rd", "th")#"st" (also an abbreviation for station)
roman_numerals <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII")
cardinal_directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

lower_case_set <- c(stop_words, measurement_units, ordinal_markers)
lower_case_set_upper <- toupper(lower_case_set)

num_languages_supported <- 8L
languages_supported <- c("Latin", "Greek", "French", "Italian", "Hebrew", "Slavic", "Japanese", "Maori")

English_invariant_words <- c("bison", "buffalo", "cannon", "carp", "cod", "deer", "fish", "moose", "pike", "salmon", "sheep", "shrimp", "squid", "swine", "trout")
English_invariant_plurals <- c("series", "species")
English_invariant_plurals_string <- "[Ss](er|pec)ies$"
English_plural_like_singulars <- c("lens", "news", "yes")
English_plural_like_singulars_string <- "\\b([Ll]en|[Nn]ew|[Yy]e)s$"
English_ie_singulars_string <- "\\b[LlPp]ies$"
English_se_singulars <- c("byes", "incenses", "senses", "tenses")
English_ves_plurals <- c("calves", "elves", "knives", "leaves", "lives", "selves")
English_us_plurals <- c("bayous", "caribous", "emus", "gnus", "menus", "tiramisus", "tutus")
Latin_es_plurals_string <- "([Aa]x|[Cc]ris|[Gg]enes|[Nnemes|[Tt]est)es$"
Latin_is_singulars_string <- "([Aa]x|[Cc]ris|[Gg]enes|[Nnemes|[Tt]est)is$"

words_with_plain_plurals <- c("canto", "hereto", "kimono", "photo", "piano", "portico", "pro", "quarto", "zero")
Latin_us_to_i_singulars <- c("alumnus", "cactus", "focus", "fungus", "succubus", "syllabus", "terminus", "uterus")
Latin_us_to_i_plurals <- c("alumni", "cacti", "foci", "fungi", "succubi", "syllabi", "termini", "uteri")
Latin_us_to_a_plurals <- c("addenda", "auditoria", "collisea", "media", "memoranda", "millennia", "ova", "referenda", "spectra", "stadia", "strata")
Latin_a_to_ae_singulars <- c("alga", "alumna", "antenna", "fauna", "fistula", "flora", "formula", "fovea", "hernia", "larva", "trachea")
Japanese_words_in_English <- c("bento", "katana", "kimono", "ninja", "otaku", "samurai", "sushi", "tsunami")
Maori_words_in_English <- c("kakapo", "kiwi", "waka")
#*** need to make first character ambiguous in case


### Helpers ----------------------------------------------------------------

is_na_or_empty <- function(strings) {
  return (is.na(strings) | strings == "")
}


### Capitalizers -----------------------------------------------------------

### Normal capitalization

capitalize <- function(words) {
  return (paste0(toupper(substring(words, 1L, 1L)), tolower(substring(words, 2L))))
}

capitalize_first_letters <- function(words) {
  return (paste0(toupper(substring(words, 1L, 1L)), substring(words, 2L)))
}

capitalize_phrase <- function(line) {
  return (paste(capitalize(strsplit(line, split = " ", fixed = TRUE)[[1]]), collapse = " "))
}

capitalize_phrase_vectorized <- function(lines) {
  lines_mod <- if_else(lines == "", "`", tolower(lines))
  lines_mod <- strsplit(lines_mod, split = " ", fixed = TRUE)
  lines_mod <- split(proper_noun_vectorized(unlist(lines_mod, use.names = FALSE)), rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- map_chr(lines_mod, paste0, collapse = " ")
  return (if_else(lines == "", "", lines_mod))
}

### Proper noun capitalization

### Proper Noun Functions ---------------------------------------------------

proper_noun <- function(word) {
  if (word %in% upper_case_set_lower) {
    return (toupper(word))
  } else if (word %in% lower_case_set) {
    return (word)
  } else {
    return (capitalize(word))
  }
}

proper_noun_vectorized <- function(words) {
  upper_case_words <- words %in% upper_case_set_lower
  capitalize_words <- !(upper_case_words | words %in% lower_case_set)
  words[upper_case_words] <- toupper(words[upper_case_words])
  words[capitalize_words] <- capitalize(words[capitalize_words])
  return (words)
}

proper_noun_phrase <- function(line) {
  line <- provide_buffers_around(tolower(line), chars = "[-/(]")
  line <- paste0(proper_noun_vectorized(strsplit(line, split = " ", fixed = TRUE)[[1]]), collapse = " ")
  return (remove_buffers_around(line, chars = "[-/(]"))
}

proper_noun_phrase_vectorized <- function(lines) {
  lines_mod <- if_else(lines == "", "`", provide_buffers_around(tolower(lines), chars = "[-/(]"))
  lines_mod <- strsplit(lines_mod, split = " ", fixed = TRUE)
  lines_mod <- split(proper_noun_vectorized(unlist(lines_mod, use.names = FALSE)), rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- map_chr(lines_mod, paste0, collapse = " ")
  return (if_else(lines == "", "", remove_buffers_around(lines_mod, chars = "[-/(]")))
}


### Numbers as strings ------------------------------------------------------

### Comma adders

add_commas <- function(number) {
  if (is.finite(number)) {
    abs_number <- abs(number)
    if (abs_number > 1) {
      num_groups <- log(abs_number, base = 1000)
    } else {
      num_groups <- 0
    }
    
    if (num_groups < 1) {
      return (as.character(number))
    } else {
      num_rounds <- floor(num_groups)
      output_string <- ""
      
      for (round in 1:num_rounds) {
        this_group_int <- abs_number %% 1000
        if(this_group_int < 10) {
          output_string <- paste0(",00", as.character(this_group_int), output_string)
        } else if(this_group_int < 100) {
          output_string <- paste0(",0", as.character(this_group_int), output_string)
        } else {
          output_string <- paste0(",", as.character(this_group_int), output_string)
        }
        abs_number <- abs_number %/% 1000
      }
      
      if (number < 0) {
        return (paste0("-", as.character(abs_number), output_string))
      } else {
        return (paste0(as.character(abs_number), output_string))
      }
    }
  } else {
    return (NA_character_)
  }
}

# note: this assumes non-negative finite integers as inputs
add_commas_vectorized <- function(numbers) {
  numbers_strings <- as.character(numbers)
  
  nums_digits <- if_else(numbers < 10, 1, ceiling(log10(numbers)))
  max_digits <- max(nums_digits, na.rm = TRUE)
  num_rounds <- ceiling(max_digits / 3) - 1L
  
  head_lengths <- 3L - (-nums_digits %% 3L)
  tail_positions <- head_lengths + 1L
  results <- substr(numbers_strings, 1L, head_lengths)
  
  for (round in seq(num_rounds)) {
    needs_more <- nums_digits > (3L * round)
    results[needs_more] <- paste0(results[needs_more], ",", substr(numbers_strings[needs_more], tail_positions[needs_more] + (3L * (round - 1L)), tail_positions[needs_more] + (3L * round) - 1L))
  }
  return (results)
}


### Modifications -----------------------------------------------------------

### Articles

fix_articles <- function(string, invariate_plurals) {
  l <- nchar(string)
  if (substr(string, l, l) == "s" | substr(string, 1, 2) == "a " | string %in% invariate_plurals) {
    return (string)
  } else if (substr(string, 1, 1) %in% vowels) {
    return (paste("an", string))
  } else {
    return (paste("a", string))
  }
}

fix_articles_vectorized <- function(strings, invariate_plurals) {
  lengths <- nchar(strings)
  needs_article <- !(substr(strings, lengths, lengths) == "s" | 
    substr(strings, 1, 2) == "a " | 
    strings %in% invariate_plurals)
  starts_with_vowel <- substr(strings, 1, 1) %in% vowels
  strings[needs_article & starts_with_vowel] <- paste("an", strings[needs_article & starts_with_vowel])
  strings[needs_article & !starts_with_vowel] <- paste("a", strings[needs_article & !starts_with_vowel])
  return (strings)
}


### Singulars and Plurals

handle_language_arguments <- function(...) {
  arguments_line <- remove_quotes(deparse(substitute(...)))
  if (arguments_line == "NULL") {
    return (character(0L))
  } else {
    arguments <- strsplit(arguments_line, "[ ,]+")[[1]] %[!=]% ""
    negations <- tolower(arguments) == "no"
    if (any(negations)) {
      which_negations <- which(negations)
      which_after_negations <- which_negations %[<=]% length(arguments)
      arguments[which_after_negations] <- paste0("-", capitalize(arguments[which_after_negations]))
      arguments <- arguments[-which_negations]
    }
    return (capitalize_first_letters(arguments))
  }
}

pluralize <- function(words, ...) {
  arguments <- handle_language_arguments(...)
  return (pluralize_(words, arguments))
}

# requires lower or proper noun case to work
pluralize_ <- function(words, ...) {
  # languages to turn on
  other_languages <- unlist(list(...), use.names = FALSE)
  if (is_empty(other_languages) || (!is_empty(other_languages) && other_languages[[1]] == "All")) {
    use_language <- rep(TRUE, 8L)
  } else {
    use_language <- rep(FALSE, 8L)
  }
  re_name(use_language, languages_supported)
  for (item in other_languages) {
    if (item %contain% languages_supported) {
      if (item %like% "^-") {
        use_language[[grem(item, "-", exact = TRUE)]] <- FALSE
      } else {
        use_language[[item]] <- TRUE
      }
    }# else ignore
  }
  
  # invariants
  is_invariant <- words %like% ending_with(any_of(English_invariant_words)) | 
    words %like% "ies$" | 
    words %like% "nese$"
  
  # Anglo-Saxon oddities
  is_person <- words %like% "[Pp]erson$"
  is_child <- words %like% "[Cc]hild$"
  is_brother <- words %like% "[Bb]rother$"
  is_man <- words %like% "[Mm]an$"
  is_oo <- words %like% "([Tt]ooth|[Ff]oot|\\b[Gg]oose)$"
  is_ouse <- words %like% "[LlMm]ouse$"
  is_ox <- words %like% "\\b[Oo]x$"
  is_die <- words %like% "\\b[Dd]ie$"
  
  rule_not_applied <- reduce_nor(is_invariant, 
                                 is_person, 
                                 is_child, 
                                 is_brother, 
                                 is_man, 
                                 is_oo, 
                                 is_ouse, 
                                 is_ox, 
                                 is_die)
  
  # Japanese
  if (use_language[["Japanese"]]) {
    is_japanese_invariant <- words %like% ending_with(any_of(Japanese_words_in_English)) & rule_not_applied
    rule_not_applied <- rule_not_applied & !is_japanese_invariant
  }
  
  # Maori
  if (use_language[["Maori"]]) {
    is_maori_invariant <- words %like% ending_with(any_of(Maori_words_in_English)) & rule_not_applied
    rule_not_applied <- rule_not_applied & !is_maori_invariant
  }
  
  # Hebrew
  if (use_language[["Hebrew"]]) {
    need_im <- words %like% "([Cc]herub|[Kk]ibbutz|[Ss]eraph)$" & rule_not_applied
    need_ot <- words %like% "[Mm]atzah$" & rule_not_applied
    
    hebrew_rule_applies <- reduce_or(need_im, 
                                     need_ot)
    rule_not_applied <- rule_not_applied & !hebrew_rule_applies
  }
  
  # Slavic
  if (use_language[["Slavic"]]) {
    need_a_slavic <- words %like% "[Kk]niazhestvo$" & rule_not_applied
    need_i_slavic <- words %like% "([Kk]obzar|[Oo]blast)$" & rule_not_applied
    
    slavic_rule_applies <- reduce_or(need_a_slavic, 
                                     need_i_slavic)
    rule_not_applied <- rule_not_applied & !slavic_rule_applies
  }
  
  # Greek
  if (use_language[["Greek"]]) {
    need_ta <- words %like% "ma$" & rule_not_applied
    need_a_greek <- words %like% "(hedr|mat|men|ri)on$" & rule_not_applied
    need_eis <- words %like% "[Pp]olis$" & rule_not_applied
    
    greek_rule_applies <- reduce_or(need_ta, 
                                    need_a_greek, 
                                    need_eis)
    rule_not_applied <- rule_not_applied & !greek_rule_applies
  }
  
  # Italian
  if (use_language[["Italian"]]) {
    need_i_italian <- words %like% "[Cc]ello$" & rule_not_applied
    rule_not_applied <- rule_not_applied & !need_i_italian
  }
  
  # French
  if (use_language[["French"]]) {
    need_x <- words %like% "eau$" & rule_not_applied
    rule_not_applied <- rule_not_applied & !need_x
  }
  
  # Latin
  if (use_language[["Latin"]]) {
    need_a <- words %like% "um$" & rule_not_applied
    need_e <- words %like% ending_with(any_of(Latin_a_to_ae_singulars)) & rule_not_applied
    need_i <- (words %like% "ius$" | words %like% ending_with(any_of(Latin_us_to_i_singulars))) & rule_not_applied
    need_era <- words %like% "([Gg]en|[Vv]isc)us$" & rule_not_applied
    need_ora <- words %like% "[Cc]orpus" & rule_not_applied
    need_ices <- words %like% "(d|t|tr)[ei]x$" & rule_not_applied
    need_es_latin <- words %like% "is$" & rule_not_applied
    
    latin_rule_applies <- reduce_or(need_a, 
                                    need_e, 
                                    need_i, 
                                    need_era, 
                                    need_ora, 
                                    need_ices, 
                                    need_es_latin)
    rule_not_applied <- rule_not_applied & !latin_rule_applies
  }
  
  # English leftovers
  need_ses <- (words %like% "[aeio]s$" | words %like% "[Bb]us$") & rule_not_applied
  need_es <- (words %like% "(x|ch|sh|ss|us|[^aeiouy]o)$" & !need_ses & words %!like% ending_with(any_of(words_with_plain_plurals))) & rule_not_applied
  need_ies <- words %like% "[^aeiou]y$" & rule_not_applied
  need_ves <- words %like% "[ailr]fe?$" & rule_not_applied
  
  english_rule_applies <- reduce_or(need_es, 
                                    need_ies, 
                                    need_ses, 
                                    need_ves)
  rule_not_applied <- rule_not_applied & !english_rule_applies
  
  # catch-all generic English plural
  need_s <- rule_not_applied
  
  # fix Anglo-Saxon oddities
  words[is_person] <- gsub(words[is_person], pattern = "rson$", replacement = "ople")
  words[is_child] <- paste0(words[is_child], "ren")
  words[is_brother] <- gsub(words[is_brother], pattern = "other$", replacement = "ethren")
  words[is_man] <- gsub(words[is_man], pattern = "an$", replacement = "en")
  words[is_oo] <- gsub(words[is_oo], pattern = "oo([a-z]{1,2})$", replacement = "ee\\1")
  words[is_ouse] <- gsub(words[is_ouse], pattern = "ouse$", replacement = "ice")
  words[is_ox] <- paste0(words[is_ox], "en")
  words[is_die] <- gsub(words[is_die], pattern = "e$", replacement = "ce")
  
  # fix French
  if (use_language[["French"]]) {
    words[need_x] <- paste0(words[need_x], "x")
  }
  
  # fix Greek
  if (use_language[["Greek"]]) {
    words[need_ta] <- paste0(words[need_ta], "ta")
    words[need_a_greek] <- gsub(words[need_a_greek], pattern = "on$", replacement = "a")
    words[need_eis] <- gsub(words[need_eis], pattern = "is$", replacement = "eis")
  }
  
  # fix Hebrew
  if (use_language[["Hebrew"]]) {
    words[need_im] <- paste0(words[need_im], "im")
    words[need_ot] <- gsub(words[need_ot], pattern = "[aeiouh]*$", replacement = "ot")
  }
  
  # fix Italian
  if (use_language[["Italian"]]) {
    words[need_i_italian] <- gsub(words[need_i_italian], pattern = "o$", replacement = "i")
  }
  
  # fix Latin
  if (use_language[["Latin"]]) {
    words[need_a] <- paste0(grem(words[need_a], "um$"), "a")
    words[need_e] <- paste0(words[need_e], "e")
    words[need_i] <- gsub(words[need_i], pattern = "us$", replacement = "i")
    words[need_era] <- gsub(words[need_era], pattern = "us$", replacement = "era")
    words[need_ora] <- gsub(words[need_ora], pattern = "us$", replacement = "ora")
    words[need_es_latin] <- gsub(words[need_es_latin], pattern = "is$", replacement = "es")
    words[need_ices] <- gsub(words[need_ices], pattern = "[ei]x$", replacement = "ices")
  }
  
  # fix Slavic
  if (use_language[["Slavic"]]) {
    words[need_a_slavic] <- gsub(words[need_a_slavic], pattern = "o$", replacement = "a")
    words[need_i_slavic] <- paste0(words[need_i_slavic], "i")
  }
  
  # fix English leftovers
  words[need_es] <- paste0(words[need_es], "es")
  words[need_ses] <- paste0(words[need_ses], "ses")
  words[need_ies] <- gsub(words[need_ies], pattern = "y$", replacement = "ies")
  words[need_ves] <- gsub(words[need_ves], pattern ="fe?$", replacement = "ves")
  
  # catch-all
  words[need_s] <- paste0(words[need_s], "s")
  
  return (words)
}

singularize <- function(words) {
  # invariants
  is_invariant <- words %like% English_invariant_plurals_string | 
    words %like% ending_with(any_of(Japanese_words_in_English)) | 
    words %like% ending_with(any_of(Maori_words_in_English)) | 
    words %like% "nese$"
  
  # Anglo-Saxon oddities
  is_person <- words %like% "[Pp]eople$"
  remove_last3 <- words %like% "[Cc]hildren$"
  is_brother <- words %like% "[Bb]rethren$"
  is_man <- words %like% "[Mm]en$"
  is_oo <- words %like% "([Tt]eeth|[Ff]eet|[Gg]eese)$"
  is_ouse <- words %like% "[LlMm]ice$"
  is_ox <- words %like% "\\b[Oo]xen$"
  is_die <- words %like% "[Dd]ice$"
  
  rule_not_found <- reduce_nor(is_invariant, 
                               
                               is_person, 
                               remove_last3, 
                               is_brother, 
                               is_man, 
                               is_oo, 
                               is_ouse, 
                               is_ox, 
                               is_die)
  
  # foreign language rules
  remove_last <- (words %like% "([Kk]obzari|[Oo]blasti)$" | words %like% "eaux$" | words %like% "ae$") & rule_not_found
  need_o <- (words %like% "[Kk]niazhestva$" | words %like% "[Cc]elli$") & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(remove_last, 
                                                need_o)
  
  remove_last2 <- words %like% "im$" & rule_not_found
  need_ah <- words %like% "ot$" & rule_not_found
  need_on <- words %like% "(hedr|mat|men|ri)a$" & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(remove_last2, 
                                                need_ah, 
                                                need_on)
  
  need_ma <- words %like% "mata$" & rule_not_found
  need_us <- words %like% "ii$" & rule_not_found
  need_us_special <- words %like% "ra$" & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(need_ma, 
                                                need_us, 
                                                need_us_special)
  
  need_um <- words %like% "a$" & rule_not_found
  rule_not_found <- rule_not_found & !need_um
  
  need_is_latin <- words %like% Latin_es_plurals_string & rule_not_found
  rule_not_found <- rule_not_found & !need_is_latin
  
  need_ex <- words %like% "(d|t)ices$" & rule_not_found
  need_ix <- words %like% "trices$" & rule_not_found
  need_is_greek <- words %like% "[Pp]oleis$" & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(need_ex, 
                                                need_ix, 
                                                need_is_greek)
  
  remove_last <- remove_last | ((words %like% English_ie_singulars_string | 
                                   words %like% "[aeiouy][^aeioux]es$" | 
                                   words %like% "[^aeiouy][cgl]es$" | 
                                   words %like% "ues$" | 
                                   words %like% ending_with(any_of(English_se_singulars))) & 
                                  rule_not_found)
  rule_not_found <- rule_not_found & !remove_last
  
  need_f <- words %like% ending_with(any_of(English_ves_plurals)) & rule_not_found
  need_y <- words %like% "ies$" & rule_not_found
  rule_not_found <- rule_not_found & reduce_nor(need_f, 
                                                need_y)
  
  remove_last3 <- remove_last3 | (words %like% "[Bb]usses$" & rule_not_found)
  rule_not_found <- rule_not_found & !remove_last3
  
  remove_last2 <- remove_last2 | (words %like% "es$" & rule_not_found)
  rule_not_found <- rule_not_found & !remove_last2
  
  remove_last <- remove_last | (words %like% "s$" & rule_not_found)
  
  # fix English rules
  words[is_person] <- gsub(words[is_person], pattern = "ople$", replacement = "rson")
  words[is_brother] <- gsub(words[is_brother], pattern = "ethren$", replacement = "other")
  words[is_man] <- gsub(words[is_man], pattern = "en$", replacement = "an")
  words[is_oo] <- gsub(words[is_oo], pattern = "ee([a-z]{1,2})$", replacement = "oo\\1")
  words[is_ouse] <- gsub(words[is_ouse], pattern = "ice$", replacement = "ouse")
  words[is_ox] <- gsub(words[is_ox], pattern = "en$", replacement = "")
  words[is_die] <- gsub(words[is_die], pattern = "ice$", replacement = "ie")
  words[need_f] <- gsub(words[need_f], pattern = "ves$", replacement = "f")
  words[need_y] <- gsub(words[need_y], pattern = "ies$", replacement = "y")
  
  # fix foreign rules
  words[need_o] <- gsub(words[need_o], pattern = "[a-z]$", replacement = "o")
  words[need_ah] <- gsub(words[need_ah], pattern = "ot$", replacement = "ah")
  words[need_ma] <- gsub(words[need_ma], pattern = "ta$", replacement = "ma")
  words[need_on] <- gsub(words[need_on], pattern = "a$", replacement = "on")
  words[need_us] <- gsub(words[need_us], pattern = "i$", replacement = "us")
  words[need_us_special] <- gsub(words[need_us_special], pattern = "[a-z]ra$", replacement = "us")
  words[need_um] <- gsub(words[need_um], pattern = "a$", replacement = "um")
  words[need_ex] <- gsub(words[need_ex], pattern = "ices$", replacement = "ex")
  words[need_ix] <- gsub(words[need_ix], pattern = "ices$", replacement = "ix")
  words[need_is_greek] <- gsub(words[need_is_greek], pattern = "eis$", replacement = "is")
  words[need_is_latin] <- gsub(words[need_is_latin], pattern = "es$", replacement = "is")
  
  # fix generic rules
  words[remove_last3] <- gsub(words[remove_last3], pattern = "[a-z]{3}$", replacement = "")
  words[remove_last2] <- gsub(words[remove_last2], pattern = "[a-z]{2}$", replacement = "")
  words[remove_last] <- gsub(words[remove_last], pattern = "[a-z]$", replacement = "")
  
  return (words)
}

is_singular <- function(words) {
  return (!is_plural(words))
}

is_plural <- function(words) {
  is_singular_with_s <- words %like% English_invariant_plurals_string | 
    words %like% English_plural_like_singulars_string | 
    words %like% "[^e]iu?s$" | 
    words %like% ending_with(any_of(Latin_us_to_i_singulars)) | 
    words %like% "([Cc]orp|[Gg]en|[Vv]isc)us$" | 
    words %like% Latin_is_singulars_string | 
    words %like% "ss$" | 
    (words %like% "us$" & 
       words %!like% ending_with(any_of(English_us_plurals)) & 
       words %!like% "eaus$")
  
  is_plural_without_s <- words %like% English_invariant_plurals_string | 
    words %like% "[Pp]eople$" | 
    words %like% "[Cc]hildren$" | 
    words %like% "[Bb]rethren$" | 
    words %like% "[Mm]en$" | 
    words %like% "([Tt]eeth|[Ff]eet|[Gg]eese)$" | 
    words %like% "[LlMm]ice$" | 
    words %like% "\\b[Oo]xen$" | 
    words %like% "[Dd]ice$" | 
    words %like% "([Kk]obzari|[Oo]blasti)$" | 
    words %like% "eaux$" | 
    words %like% "ae$" | 
    words %like% "[Kk]niazhestva$" | 
    words %like% "[Cc]elli$" | 
    words %like% "([Cc]herub|[Kk]ibbutz|[Ss]eraph)im$" | 
    words %like% "[Mm]atzot$" | 
    words %like% "(hedr|mat|men|ri)a$" | 
    words %like% "ii$" | 
    words %like% "([Gg]en|[Vv]isc)era$" | 
    words %like% "[Cc]orpora$" | 
    words %like% ending_with(any_of(Latin_us_to_i_plurals)) | 
    words %like% ending_with(any_of(Latin_us_to_a_plurals))
  
  is_indeterminate <- words %like% ending_with(any_of(English_invariant_words)) | 
    words %like% ending_with(any_of(Japanese_words_in_English)) | 
    words %like% ending_with(any_of(Maori_words_in_English)) | 
    words %like% "nese$"
  
  is_plural <- !is_indeterminate & 
    (is_plural_without_s | (words %like% "s$" & !is_singular_with_s))
  
  results <- logical(length(words))
  results[is_indeterminate] <- NA
  results[is_plural] <- TRUE
  
  return (results)
}

make_plural <- function(words, ...) {
  arguments <- handle_language_arguments(...)
  are_singular <- is_singular(words)
  of_indeterminate_number <- is.na(are_singular)
  if (any(of_indeterminate_number) & !any(c("All", "Japanese", "Maori") %in% arguments)) {
    words[of_indeterminate_number] <- pluralize_(words[of_indeterminate_number], arguments)
    words[are_singular & !of_indeterminate_number] <- pluralize_(words[are_singular & !of_indeterminate_number], arguments)
  } else {
    words[are_singular] <- pluralize_(words[are_singular], arguments)
  }
  return (words)
}

make_singular <- function(words) {
  are_plural <- is_plural(words)
  of_indeterminate_number <- is.na(are_plural)
  words[are_plural & !of_indeterminate_number] <- singularize(words[are_plural & !of_indeterminate_number])
  return (words)
}


### Dictionary Functions ----------------------------------------------------

#dictionary_10k <- read.csv('words/10k most common English words.txt', header = FALSE, stringsAsFactors = FALSE)[["V1"]]
dictionary_20k <- read.csv('words/20k most common English words.txt', header = FALSE, stringsAsFactors = FALSE)[["V1"]]

is_valid_word_vectorized <- function(words, dictionary) {
  return (words %in% dictionary)
}

is_valid_word_phrase_vectorized <- function(lines, dictionary) {
  lines_mod <- if_else(lines == "", "`", lines)
  lines_mod <- strsplit(lines_mod, split = "[, ()/-]")
  lines_mod <- split(is_valid_word_vectorized(unlist(lines_mod, use.names = FALSE), dictionary), rep(1:length(lines), lengths(lines_mod)))
  lines_reduced <- map_lgl(lines_mod, all)
  return (if_else(lines == "", TRUE, lines_reduced))
}

get_invalid_words <- function(lines, dictionary) {
  lines_mod <- if_else(lines == "", "`", lines)
  lines_mod <- strsplit(lines_mod, split = "[, ()/-]")
  combined_words <- unlist(lines_mod, use.names = FALSE)
  combined_invalid_words <- if_else(is_valid_word_vectorized(combined_words, dictionary), "", combined_words)
  lines_mod <- split(combined_invalid_words, rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- map_chr(lines_mod, paste0, collapse = " ")
  return (if_else(lines == "", "", lines_mod))
}

filter_out_invalid_words <- function(lines, dictionary, exclude = character(0)) {
  lines_mod <- if_else(lines == "", "`", lines)
  lines_mod <- gsub(pattern = "([,()/-])", replacement = " \\1 ", lines_mod)
  lines_mod <- strsplit(lines_mod, split = " ", fixed = TRUE)
  combined_words <- unlist(lines_mod, use.names = FALSE)
  is_valid_word <- is_valid_word_vectorized(combined_words, dictionary) | combined_words %like% "[,()/-]"
  is_valid_word <- is_valid_word & combined_words %!in% exclude
  combined_valid_words <- if_else(is_valid_word, combined_words, "")
  lines_mod <- split(combined_valid_words, rep(1:length(lines), lengths(lines_mod)))
  lines_mod <- trimws(gsubs(changes = c("\\1" = " ([,()/-]) ", 
                                        "-?\\b[a-z]\\b-?", 
                                        " "   = " +"), 
                            map_chr(lines_mod, paste0, collapse = " ")))
  return (if_else(lines == "", "", lines_mod))
}

fix_invalid_words <- function(lines, dictionary, exclude = character(0)) {#*** fix num_lines <- length(lines) and other similar stuff here
  lines_mod <- if_else(lines == "", "`", lines) %>% 
    remove_duplicates(duplicate = ",", exact = TRUE) %>% # for safety
    provide_buffers_around("[,()/-]") %>% # to conserve punctuation (will re-delete spaces at end)
    gsub(pattern = "&", replacement = " and ", fixed = TRUE) %>% # for safety; make all &s ands
    fix_spaces
  
  num_lines <- length(lines)
  
  split_result <- strsplit(lines_mod, split = " ", fixed = TRUE)
  split_lengths <- lengths(split_result)
  combined_words <- unlist(split_result, use.names = FALSE)
  is_valid_word <- is_valid_word_vectorized(combined_words, dictionary) | combined_words %like% "[,()/-]" | combined_words == ""
  is_valid_word <- is_valid_word & combined_words %!in% exclude
  is_invalid_word <- !is_valid_word
  is_and <- combined_words == "and"
  is_comma <- combined_words == ","
  split_sizes <- rep(1:num_lines, split_lengths)
  
  is_valid_word_column <- split(is_valid_word, split_sizes)
  is_invalid_word_column <- split(is_invalid_word, split_sizes)
  is_and_column <- split(is_and, split_sizes)
  is_comma_column <- split(is_comma, split_sizes)
  
  is_previous_comma <- is_comma_column %map_and% lead(is_invalid_word_column, 1L, fill = FALSE)
  is_twice_previous_comma <- is_comma_column %map_and% lead(is_invalid_word_column, 2L, fill = FALSE)
  is_previous_and <- is_and_column %map_and% lead(is_invalid_word_column, 1L, fill = FALSE)
  is_comma_before_previous_and <- is_twice_previous_comma %map_and% lead(is_previous_and, 1L, fill = FALSE)
  
  is_subsequent_and <- is_and_column %map_and% lag(is_invalid_word_column, 1L, fill = FALSE)
  is_subsequent_comma <- is_comma_column %map_and% lag(is_invalid_word_column, 1L, fill = FALSE)
  is_subsequent_bridge <- is_subsequent_and %map_or% is_subsequent_comma
  
  is_previous_bridge_word <- is_previous_comma %map_or% is_previous_and
  is_previous_removed <- lag(is_previous_bridge_word, 1L, fill = FALSE)
  is_previous_not_removed <- map_not(is_previous_removed)
  is_subsequent_bridge_and_previous_not_removed <- is_subsequent_bridge %map_and% lag(is_previous_not_removed, 1L, fill = FALSE)
  
  is_removable <- reduce_or_l(list(is_invalid_word_column, is_previous_bridge_word, is_comma_before_previous_and, is_subsequent_bridge_and_previous_not_removed))
  is_keepable <- map_not(is_removable)
  
  is_remaining_before_comma_and <- is_keepable %map_and% lead(is_comma_before_previous_and, 1L, fill = FALSE)
  is_and_needing_saving <- lag(is_remaining_before_comma_and, 2L, fill = FALSE)
  which_is_and_twice_after_remaining_before_comma_and <- map_which(is_and_needing_saving)
  which_is_comma <- map_which(is_comma_column)
  which_is_remaining_before_comma_and <- map_which(is_remaining_before_comma_and)
  which_is_remaining_after_comma_yet_before_comma_and <- map2(which_is_comma, which_is_remaining_before_comma_and, ~quiet(closest_to, .x, .y, but_less_than = TRUE, sorted = TRUE)) %map_plus% 1L
  
  reinsertion_necessary <- !map_lgl(which_is_remaining_after_comma_yet_before_comma_and, is_empty)
  
  is_keepable <- is_keepable %map_or% is_and_needing_saving
  is_removable <- map_not(is_keepable)
  
  is_keepable_comma <- is_keepable %map_and% is_comma_column
  which_is_keepable_comma <- map_which(is_keepable_comma)
  are_commas_unneeded <- lengths(which_is_keepable_comma) < 2L
  is_removable_comma <- is_comma_column %map_and% are_commas_unneeded
  
  is_removable <- is_removable %map_or% is_removable_comma
  
  is_removable_vector <- unlist(is_removable, use.names = FALSE)
  combined_words[is_removable_vector] <- ""
  
  filtered_lines <- split(combined_words, split_sizes)
  
  filtered_lines <- pmap(list(filtered_lines, 
                              reinsertion_necessary, 
                              which_is_and_twice_after_remaining_before_comma_and, 
                              which_is_remaining_after_comma_yet_before_comma_and), 
                         ~(if.else(..2, remove_and_reinsert(..1, from = ..3, at = ..4), ..1)))
  
  filtered_lines_reduced <- map_chr(filtered_lines, paste0, collapse = " ") %>% 
    remove_buffers_around("[()/-]") %>% 
    format_commas %>% 
    remove_single_letters(with_punctuation = "[()/-]") %>% 
    remove_hanging_punctuation("[()/-]") %>% 
    grems(patterns = c(ending_with(as_many_of(word(any_of(stop_words)))), 
                       beginning_with(word(any_of(English_prepositions))))) %>% 
    remove_duplicates(c(",", "/")) %>% 
    fix_spaces
  
  return (if_else(lines == "", "", filtered_lines_reduced))
}
