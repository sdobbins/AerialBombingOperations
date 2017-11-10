# @author Scott Dobbins
# @version 0.9.9.5
# @date 2017-11-10 16:00


### Package Functions -------------------------------------------------------

is_package_installed <- function(package_name) {
  return (package_name %in% rownames(installed.packages()))
}

# could replace with package:R.utils::isPackageLoaded
is_package_loaded <- function(package_name) {
  return (paste0("package:", package_name) %in% search())
}


### Piping ------------------------------------------------------------------

if (is_package_installed("pipeR")) {
  # overwrite the magrittr pipe (%>%) with the better pipeR pipe (normally %>>%)
  `%>%` <- pipeR::`%>>%`
}


### Debugging Functions -----------------------------------------------------

message_if <- function(condition, string) {
  if (condition) message(string)
}

# duplicate of message_if() that responds to global debug_mode_on flag
debug_message <- function(string) {
  if (debug_mode_on) message(string)
}

debug_message_p <- function(...) {
  if (debug_mode_on) message(paste(...))
}

debug_message_p0 <- function(...) {
  if (debug_mode_on) message(paste0(...))
}

with_debug_message <- function(func_call) {
  debug_message(deparse(substitute(func_call)))
  eval(func_call)
}


### Unit Testing Functions --------------------------------------------------

cardinality <- function(column, na.rm = TRUE) {
  if (is.null(column)) {
    warning("cardinality(NULL) = NA")
    return (NA)
  }
  if (is.factor(column)) {
    return (nlevels(column))
  } else {
    if (na.rm) {
      return (length(unique(drop_NA(column))))
    } else {
      return (length(unique(column)))
    }
  } 
}

diversity <- function(column, calibrated = TRUE, na.rm = NULL) {
  if (is.null(column)) {
    warning("diversity(NULL) = NA")
    return (NA)
  }
  if (is.null(na.rm)) {
    na.rm = TRUE
    give_warnings <- TRUE
  } else {
    give_warnings <- FALSE
  }
  num_cats <- cardinality(column, na.rm = na.rm)
  if (num_cats <= 1L) {
    return (0)
  } else {
    if (!is.factor(column)) {
      column <- as.factor(column)
    }
    tab_counts <- tabulate_factor(column)
    if (sum(tab_counts == 0L) > 0L) {
      if (na.rm) {
        tab_counts <- tab_counts %[!=]% 0
        num_cats <- length(tab_counts)
        if (give_warnings) {
          warning("Skipped levels not present in data")
        }
      } else {
        warning("Some levels are not present in the data, so diversity is 0")
        return (0)
      }
    }
    num_total <- sum(tab_counts, na.rm = TRUE)
    result <- 1
    for (i in seq_len(num_cats)) {
      result <- result * tab_counts[[i]] / num_total * num_cats
    }
    if (calibrated) {
      return (result ^ (1/num_cats))
    } else {
      return (result)
    }
  }
}


### Predicates --------------------------------------------------------------

is_scalar <- function(thing) {
  return (length(thing) == 1L)
}

is_plural <- function(thing) {
  return (length(thing) > 1L)
}

is_NULL <- function(thing) {
  return (sapply(thing, is.null))
}


### Slicers -----------------------------------------------------------------

keep_NA <- function(thing) {
  return (thing[is.na(thing)])
}

drop_NA <- function(thing) {
  return (thing[!is.na(thing)])
}

drop_NULL <- function(thing) {
  return (thing[!is_NULL(thing)])
}

keep_true <- function(thing) {
  return (thing[thing])
}

keep_false <- function(thing) {
  return (thing[!thing])
}

slice_from <- function(thing, from) {
  return (seq(from, length(thing)))
}

first <- function(thing) {
  return (thing[[1]])
}

last <- function(thing) {
  return (thing[[length(thing)]])
}

nth <- function(thing, n) {
  return (thing[[n]])
}

fix_subset <- function(vec, condition, func) {
  vec[condition] <- func(vec[condition])
  return (vec)
}


### Subsetters --------------------------------------------------------------

limited_subset <- function(dt, cols, limit) {
  results <- list()
  for (i in seq_along(cols)) {
    if (cardinality(dt[[cols[[i]]]]) <= limit) {
      results <- append(results, cols[i])
    }
  }
  return (results)
}


### Breakers ----------------------------------------------------------------

#*** unfinished but functional
generate_reasonable_cuts <- function(data_range, min_breaks, max_breaks = min_breaks, data_median = NULL, low_break = NULL, high_break = NULL, data_scale = "normal", fit_minimum = FALSE, fit_maximum = FALSE, sigfigs = 15L) {
  range_low <- data_range[[1]]
  range_high <- data_range[[2]]
  if (data_scale == "normal") {
    total_range <- range_high - range_low
    if (near(total_range, 0)) {
      # issue
    } else {
      range_breaks_min <- total_range / max_breaks
      range_breaks_max <- total_range / min_breaks
      
      # divide by factor of 10 to get close to but greater than 1
      factor_of_10_min <- 10 ** floor(log10(range_breaks_min))
      # see whether closest (in greater direction) to 1, 2, or 5
      if (range_breaks_min / factor_of_10_min < 2) {
        nearest_break_low <- factor_of_10_min * 2
      } else if (range_breaks_min / factor_of_10_min < 5) {
        nearest_break_low <- factor_of_10_min * 5
      } else {
        nearest_break_low <- factor_of_10_min * 10
      }
      
      factor_of_10_max <- 10 ** floor(log10(range_breaks_max))
      if (range_breaks_max / factor_of_10_max < 2) {
        nearest_break_high <- factor_of_10_max
      } else if (range_breaks_max / factor_of_10_max < 5) {
        nearest_break_high <- factor_of_10_max * 2
      } else {
        nearest_break_high <- factor_of_10_max * 5
      }
      
      if (nearest_break_low == nearest_break_high) {
        range_break <- nearest_break_low
      } else {
        nearest_range_break_middle <- sqrt(nearest_break_low * nearest_break_high)#*** perhaps just redo using range_breaks_min and range_breaks_max
        factor_of_10_middle <- floor(log10(nearest_range_break_middle))
        if (nearest_range_break_middle / factor_of_10_middle < sqrt(2)) {
          range_break <- factor_of_10_middle
        } else if (nearest_range_break_middle / factor_of_10_middle < sqrt(2*5)) {
          range_break <- factor_of_10_middle * 2
        } else if (nearest_range_break_middle / factor_of_10_middle < sqrt(5*10)) {
          range_break <- factor_of_10_middle * 5
        } else {
          range_break <- factor_of_10_middle * 10
        }
      }
      
      break_low <- floor(range_low / range_break) * range_break
      break_high <- ceiling(range_high / range_break) * range_break
      
      breaks <- break_low
      current_break <- break_low
      while (current_break < break_high) {
        current_break <- current_break + range_break
        breaks <- c(breaks, current_break)
      }
      
      return (breaks)
      
      # if (fit_minimum) {
      #   
      # } else {
      #   
      # }
    }
  } else if (data_scale %like% "log") {
    if (near(range_low, 0)) {
      # issue
    } else {
      total_range <- range_high / range_low
      if (data_scale == "log-10") {
        range_factors <- log10(total_range)
        
        power_low <- range_factors / max_breaks
        if (power_low < 0.5) {
          root_of_10_high <- floor(1 / power_low)
          factor_low <- 10 ** (1/root_of_10_high)
        } else if (power_low < 2) {
          if (power_low < 2/3) {
            factor_low <- 10 ** (2/3)
          } else if (power_low < 1) {
            factor_low <- 10
          } else if (power_low < 4/3) {
            factor_low <- 10 ** (4/3)
          } else if (power_low < 3/2) {
            factor_low <- 10 ** (3/2)
          } else {
            factor_low <- 10 ** 2
          }
        } else {
          factor_low <- 10 ** ceiling(power_low)
        }
        
        power_high <- range_factors / min_breaks
        if (power_high < 0.5) {
          root_of_10_low <- ceiling(1 / power_high)
          factor_high <- 10 ** (1/root_of_10_low)
        } else if (power_high < 2) {
          if (power_high < 2/3) {
            factor_high <- 10 ** (1/2)
          } else if (power_high < 1) {
            factor_high <- 10 ** (2/3)
          } else if (power_high < 4/3) {
            factor_high <- 10
          } else if (power_high < 3/2) {
            factor_high <- 10 ** (4/3)
          } else {
            factor_high <- 10 ** (3/2)
          }
        } else {
          factor_high <- 10 ** floor(power_high)
        }
        
        if (factor_low == factor_high) {
          nearest_factor <- factor_low
        } else {
          nearest_factor_middle <- sqrt(factor_low * factor_high)#*** perhaps just redo using range_breaks_min and range_breaks_max
          log10_nearest_factor_middle <- log10(nearest_factor_middle)
          
          if (log10_nearest_factor_middle < 0.5) {
            nearest_factor <- 10 ** (1 / round(1/log10_nearest_factor_middle))
          } else if (log10_nearest_factor_middle < 2) {
            if (log10_nearest_factor_middle - sqrt(0.5*log10_nearest_factor_middle) < 2/3 - sqrt(log10_nearest_factor_middle*2/3)) {
              nearest_factor <- 10 ** (1/2)
            } else if (log10_nearest_factor_middle - sqrt(2/3*log10_nearest_factor_middle) < 1 - sqrt(log10_nearest_factor_middle*1)) {
              nearest_factor <- 10 ** (2/3)
            } else if (log10_nearest_factor_middle - sqrt(1*log10_nearest_factor_middle) < 4/3 - sqrt(log10_nearest_factor_middle*4/3)) {
              nearest_factor <- 10 ** (1)
            } else if (log10_nearest_factor_middle - sqrt(4/3*log10_nearest_factor_middle) < 3/2 - sqrt(log10_nearest_factor_middle*3/2)) {
              nearest_factor <- 10 ** (4/3)
            } else if (log10_nearest_factor_middle - sqrt(3/2*log10_nearest_factor_middle) < 2 - sqrt(log10_nearest_factor_middle*2)) {
              nearest_factor <- 10 ** (3/2)
            } else {
              nearest_factor <- 10 ** (2)
            }
          } else {
            nearest_factor <- 10 ** round(log10_nearest_factor_middle)
          }
        }
        
        break_low_factors <- floor(log10(range_low) / log10(nearest_factor))
        break_low <- nearest_factor ** break_low_factors
        break_high_factors <- ceiling(log10(range_high) / log10(nearest_factor))
        break_high <- nearest_factor ** break_high_factors
        
        breaks <- signif(break_low, sigfigs)
        current_break <- break_low
        num_factors <- 0L
        while (current_break < break_high) {
          num_factors <- num_factors + 1L
          current_break <- break_low * (nearest_factor ** num_factors)
          breaks <- c(breaks, signif(current_break, sigfigs))
        }
        
        return (breaks)
        
        # if (fit_minimum) {
        #   
        # } else {
        #   
        # }
        
      } else if (data_scale == "log-2") {
        range_factors <- log2(total_range)
      }
    }
  } 
}

cut_data_column <- function(dt, col, breaks_min, breaks_max = breaks_min, trans = "normal") {
  if (trans == "Logarithm") {
    breaks <- generate_reasonable_cuts(data_range = range(dt[[col]], na.rm = TRUE), 
                                       min_breaks = breaks_min, 
                                       max_breaks = breaks_max, 
                                       data_scale = "log-10")
  } else {
    breaks <- generate_reasonable_cuts(data_range = range(dt[[col]], na.rm = TRUE), 
                                       min_breaks = breaks_min, 
                                       max_breaks = breaks_max, 
                                       data_scale = "normal")
  }
  set(dt, j = col, value = cut(dt[["col"]], breaks = breaks, include.lowest = TRUE, ordered_result = TRUE))
}


### Control Flow ------------------------------------------------------------

if.else <- function(condition, true_value, false_value) {
  if (condition) {
    return (true_value)
  } else {
    return (false_value)
  }
}



### Vector Functions --------------------------------------------------------

recycle_arguments <- function(arg, len) {
  arg_len <- length(arg)
  if (arg_len < len) {
    message_if(len %% arg_len != 0L, "arguments were recycled unevenly")
    return (rep_len(arg, len))
  } else if (arg_len > len) {
    message("you supplied more arguments than the final length you requested (length(arg) > len), so clipping has occurred")
    return (arg[1:len])
  } else {
    return (arg)
  }
}


### Numeric Functions -------------------------------------------------------

bounded <- function(number, lower, upper) {
  if (number > upper) {
    return (upper)
  } else if (number < lower) {
    return (lower)
  } else {
    return (number)
  }
}

if (is_package_installed("dplyr")) {
  # best version of this function using the faster dplyr::if_else
  bounded_vectorized <- function(numbers, lowers, uppers) {
    return (dplyr::if_else(numbers > uppers, uppers, dplyr::if_else(numbers < lowers, lowers, numbers)))
  }
} else {
  # if dplyr isn't loaded, then this function is still supported with base::ifelse
  bounded_vectorized <- function(numbers, lowers, uppers) {
    return (ifelse(numbers > uppers, uppers, ifelse(numbers < lowers, lowers, numbers)))
  }
}

is_int <- function(double) {
  return (near(double, as.integer(double)))
}

round_to_int <- function(numbers, digits = 0L) {
  return (as.integer(round(numbers, digits)))
}

is_NA_or_0L <- function(ints) {
  if (!is.numeric(ints)) {
    stop("This method is for integers; you supplied some non-numeric type")
  }
  if (is.double(ints)) {
    warning("This method is for integers; you supplied doubles")
  }
  return (is.na(ints) | ints == 0L)
}

is_NA_or_0 <- function(dbls, tol = NULL) {
  if (!is.numeric(dbls)) {
    stop("This method is for doubles; you supplied some non-numeric type")
  }
  if (is.integer(dbls)) {
    warning("This method is for doubles; you supplied integers")
  }
  if (is.null(tol)) {
    return (is.na(dbls) | dbls == 0)
  } else {
    return (is.na(dbls) | near(dbls, 0, tol = tol))
  }
}


### Character Functions -----------------------------------------------------

is_na_or_empty <- function(strings) {
  return (is.na(strings) | strings == "")
}


### Regex -------------------------------------------------------------------

regex_metacharacters <- c("$", "(", ")", "*", "+", ".", "?", "[", "\\", "]", "^", "{", "|", "}")
regex_metacharacters_escaped <- paste0("\\", regex_metacharacters)
regex_metacharacters_set <- "[]|{}$()*+.?\\[^]"
regex_metacharacters_set_captured <- paste0("(", regex_metacharacters_set, ")")

rem <- function(x, pattern, ...) {
  return (sub(pattern = pattern, replacement = "", x = x, ...))
}

grem <- function(x, pattern, ...) {
  return (gsub(pattern = pattern, replacement = "", x = x, ...))
}

fix_spaces <- function(strings) {
  return (gsub(pattern = "  +", replacement = " ", strings, fixed = TRUE))
}

word <- function(strings) {
  return (paste0("\\b", strings, "\\b"))
}

capturing_group <- function(strings) {
  return (paste0("(", strings, ")"))
}

selection_group <- function(strings, not = FALSE) {
  if (not) {
    invert <- "^"
  } else {
    invert <- ""
  }
  return (paste0("[", invert, strings, "]"))
}

literal <- function(strings) {
  return (gsub(pattern = regex_metacharacters_set_captured, "\\\\\\1", strings))
}

with_preceding <- function(strings, marks = " ", mandatory = FALSE) {
  needs_grouping <- nchar(marks) > 1L
  marks[needs_grouping] <- capturing_group(marks[needs_grouping])
  if (mandatory) {
    return (paste0(marks, capturing_group(strings)))
  } else {
    return (paste0(marks, "?", capturing_group(strings)))
  }
}

with_following <- function(strings, marks = " ", mandatory = FALSE) {
  needs_grouping <- nchar(marks) > 1L
  marks[needs_grouping] <- capturing_group(marks[needs_grouping])
  if (mandatory) {
    return (paste0(capturing_group(strings), marks))
  } else {
    return (paste0(capturing_group(strings), marks, "?"))
  }
}

any_of <- function(strings) {
  return (capturing_group(paste0(strings, collapse = "|")))
}

and_preceding <- function(strings, precedings = " ", succeedings = " ", exact = FALSE, greedy = FALSE, stoppers = "") {
  if (exact) {
    strings <- literal(strings)
  }
  greed <- if_else(greedy, "", "?")
  return (paste0("([^", precedings, stoppers, "]*", precedings, ")*", greed, strings, succeedings))
}

remove_before <- function(strings, points, exact = FALSE, greedy = FALSE, inclusive = FALSE) {
  if (greedy) {
    positions <- map_int(gregexpr(points, strings, fixed = exact), last)
  } else {
    positions <- as.integer(regexpr(points, strings, fixed = exact))
  }
  slicer <- positions != -1L
  if (is.factor(strings)) {
    strings <- as.character(strings)
  }
  if (inclusive) {
    strings[slicer] <- sub(points, "", substr(strings[slicer], start = positions, stop = nchar(strings)), fixed = exact)
  } else {
    strings[slicer] <- substr(strings[slicer], start = positions[slicer], stop = nchar(srings))
  }
  return (strings)
}

and_rest_of <- function(strings, precedings = " ", exact = FALSE, greedy = TRUE, stoppers = "") {
  if (exact) {
    strings <- literal(strings)
  }
  if (length(stoppers) == 1) {
    stoppers <- rep(stoppers, length(strings))
  }
  if (length(greedy) == 1) {
    greedy <- rep(greedy, length(strings))
  }
  chars <- if_else(stoppers == "", "[ -~]*", paste0("[^", stoppers, "]*"))
  greed <- if_else(greedy, "", paste0("(?!", chars, strings, ")"))
  return (paste0(precedings, strings, greed, chars))
}

remove_after <- function(strings, points, exact = FALSE, greedy = FALSE, inclusive = FALSE) {
  if (greedy) {
    matches <- regexpr(points, strings, fixed = exact)
    match_lengths <- attr(matches, "match.length")
    positions <- as.integer(matches)
  } else {
    matches <- gregexpr(points, strings, fixed = exact)
    match_lengths <- map_int(matches, ~last(. %@% "match.length"))
    positions <- map_int(matches, last)
  }
  slicer <- positions != -1L
  if (is.factor(strings)) {
    strings <- as.character(strings)
  }
  if (inclusive) {
    strings[slicer] <- substr(strings[slicer], start = 1L, stop = positions[slicer] - 1L)
  } else {
    strings[slicer] <- substr(strings[slicer], start = 1L, stop = positions[slicer] + match_lengths[slicer])
  }
  return (strings)
}

and_between <- function(starts, ends, preceding_starts = " ", precedings = " ", succeedings = " ", exact = FALSE, greedy = FALSE, stoppers = "") {
  if (exact) {
    starts <- literal(starts)
    ends <- literal(ends)
  }
  greed <- if_else(greedy, "", "?")
  preceding_starts_chars <- grem(preceding_starts, "[]\\[]")
  stop_sets <- paste0("[", preceding_starts_chars, precedings, "]")
  not_stop_sets <- paste0("[^", preceding_starts_chars, precedings, "]")
  look_aheads <- if_else(greedy, "", paste0("(?!", starts, ")"))
  return (paste0(preceding_starts, starts, "(", not_stop_sets, "*", stop_sets, look_aheads, ")*", greed, ends, succeedings))
}

#and_between_containing <- function()


### Attribute Functions -----------------------------------------------------

get_names <- function(vec) {
  return (names(vec) %||% rep("", length(vec)))
}


### Factor Functions --------------------------------------------------------

tabulate_factor <- function(fact) {
  return (tabulate(fact, nbins = nlevels(fact)))
}

mode <- function(nums) {
  return (which.max(tabulate(nums)))
}

mode_and_others <- function(nums) {
  all_nums <- unique(nums)
  tabs <- tabulate(nums)
  present_nums <- all_nums[tabs > 0L]
  mode_num <- all_nums[which.max(tabs)]
  non_mode_nums <- present_nums %d% mode_num
  return (list("mode" = mode_num, "others" = non_mode_nums))
}

mode_factor <- function(fact) {
  return (levels(fact)[which.max(tabulate_factor(fact))])
}

mode_and_others_factor <- function(fact) {
  all_levels <- levels(fact)
  tabs <- tabulate_factor(fact)
  present_levels <- all_levels[tabs > 0L]
  mode_level <- all_levels[which.max(tabs)]
  non_mode_levels <- present_levels %d% mode_level
  return (list("mode" = mode_level, "others" = non_mode_levels))
}

level_proportions <- function(column, na.rm = FALSE) {
  assert_that(is.factor(column), 
              msg = "You supplied a non-factor vector/column to a factor-only method")
  tab_counts <- tabulate_factor(column)
  if (na.rm && (sum(tab_counts == 0L) > 0L)) {
    tab_counts <- tab_counts %[!=]% 0
  }
  tab_total <- sum(tab_counts)
  return (tab_counts / tab_total)
}

missing_levels <- function(fact) {
  return (levels(fact)[tabulate_factor(fact) == 0L])
}

level_index <- function(fact, lev) {
  if (is.na(lev)) {
    return (which(is.na(levels(fact))))
  } else {
    return (which(levels(fact) == lev))
  }
}

# duplicate of level_index() with better name and default argument for finding NAs
NA_index <- function(fact, NA_level = NA_character_) {
  if (is.na(NA_level)) {
    return (which(is.na(levels(fact))))
  } else {
    return (which(levels(fact) == NA_level))
  }
}

if (is_package_installed("data.table")) {
  # best versions of the factor functions set levels (and names) by reference using data.table::setattr
  re_name <- function(vec, new_names) {
    data.table::setattr(vec, "names", new_names)
  }
  
  re_level <- function(vec, new_levels, NA_level = NA_character_) {
      data.table::setattr(vec, "levels", fix_NA_levels(new_levels, NA_level = NA_level))
  }
} else {
  re_name <- function(vec, new_names) {
    names(vec) <- new_names
  }
  
  re_level <- function(vec, new_levels) {
    levels(vec) <- new_levels
  }
}

fix_NA_levels <- function(levs, NA_level = NA_character_) {
  if (!is.na(NA_level) && anyNA(levs)) {
    warning("NA levels set to the given (character) NA_level")
    levs[is.na(levs)] <- NA_level
  }
  return (levs)
}

format_levels <- function(fact, func, ...) {
  re_level(fact, func(levels(fact), ...))
}

format_similar_levels <- function(fact, pairings, exact = FALSE, ...) {
  new_levels <- levels(fact)
  pairings_names <- get_names(pairings)
  if (exact) {
    for (i in seq_along(pairings)) {
      new_levels[new_levels %exactlylike% pairings_names[[i]]] <- (pairings[[i]])(new_levels %whichexactlylike% pairings_names[[i]], ...)
    }
  } else {
    for (i in seq_along(pairings)) {
      new_levels[new_levels %like% pairings_names[[i]]] <- (pairings[[i]])(new_levels %whichlike% pairings_names[[i]], ...)
    }
  }
  re_level(fact, new_levels)
}

replace_level <- function(fact, from, to) {
  assert_that(length(from) == 1L && length(to) == 1L, 
              msg = "either (or both) 'from' or 'to' are of length > 1L (you may have intended to use replace_levels, not replace_level")
  new_levels <- levels(fact)
  new_levels[new_levels == from] <- to
  re_level(fact, new_levels)
}

replace_levels <- function(fact, from, to) {
  assert_that(length(from) == length(to) || length(to) == 1L, 
              msg = "Lengths of 'from' and 'to' don't match")
  new_levels <- levels(fact)
  if (length(to) == 1L) {
    for (i in seq_along(from)) {
      new_levels[new_levels == from[[i]]] <- to
    }
  } else {
    for (i in seq_along(from)) {
      new_levels[new_levels == from[[i]]] <- to[[i]]
    }
  }
  re_level(fact, new_levels)
}

rename_levels <- function(fact, changes) {
  new_levels <- levels(fact)
  re_name(new_levels, new_levels)
  changes <- changes %whichin% new_levels
  if (!is_empty(changes)) {
    change_empty_level <- "" %c% changes
    if (change_empty_level) {
      empty_change <- get_names(changes %[==]% "")
      changes <- changes %[!=]% ""
    }
    changes_names <- get_names(changes)
    for (i in seq_along(changes)) {
      new_levels[[changes[[i]]]] <- changes_names[[i]]
    }
    if (change_empty_level) {
      new_levels[new_levels == ""] <- empty_change
    }
    re_level(fact, unname(new_levels))
  } else {
    invisible(fact)
  }
}

rename_similar_levels <- function(fact, changes, exact = FALSE) {
  new_levels <- levels(fact)
  changes_names <- get_names(changes)
  for (i in seq_along(changes)) {
    new_levels <- gsub(pattern = changes[[i]], replacement = changes_names[[i]], new_levels, fixed = exact)
  }
  re_level(fact, new_levels)
}

add_levels <- function(fact, add) {
  new_levels <- levels(fact)
  add <- add %which!in% new_levels
  if (!is_empty(add)) {
    re_level(fact, c(new_levels, add))
  } else {
    invisible(fact)
  }
}

drop_levels <- function(fact, drop, to = "") {
  new_levels <- levels(fact)
  drop <- drop %whichin% new_levels
  if (!is_empty(drop)) {
    re_name(new_levels, new_levels)
    drop_empty_level <- "" %c% drop
    if (drop_empty_level) {
      drop <- drop %[!=]% ""
    }
    for (i in seq_along(drop)) {
      new_levels[[drop[[i]]]] <- to
    }
    if (drop_empty_level) {
      new_levels[new_levels == ""] <- to
    }
    re_level(fact, unname(new_levels))
  } else {
    invisible(fact)
  }
}

drop_similar_levels <- function(fact, drop, to = "", exact = FALSE) {
  new_levels <- levels(fact)
  for (i in seq_along(drop)) {
    new_levels[grepl(pattern = drop[[i]], fixed = exact, new_levels)] <- to
  }
  re_level(fact, new_levels)
}

drop_missing_levels <- function(fact, to = "") {
  drop_levels(fact, drop = missing_levels(fact), to)
}

drop_levels_formula <- function(fact, expr, to = "") {
  drop_levels <- eval(parse(text = paste0("levels(fact) %[]% ", deparse(substitute(expr)))))
  drop_levels(fact, drop = drop_levels, to)
}

keep_levels <- function(fact, keep, to = "") {
  new_levels <- levels(fact)
  new_levels[new_levels %!in% keep] <- to
  re_level(fact, new_levels)
}

keep_similar_levels <- function(fact, keep, to = "", exact = FALSE) {
  levels_to_drop <- levels(fact)
  for (i in seq_along(keep)) {
    levels_to_drop[grepl(pattern = keep[[i]], fixed = exact, levels_to_drop)] <- to
  }
  levels_to_drop <- levels_to_drop[levels_to_drop != to]
  drop_levels(fact, drop = levels_to_drop, to = to)
}

reduce_levels <- function(fact, rules, other = "other", exact = FALSE) {
  replacements <- get_names(rules)
  patterns <- unname(rules)
  old_levels <- levels(fact)
  new_levels <- rep(other, length(old_levels))
  for (i in seq_along(rules)) {
    slicer <- grepl(pattern = patterns[[i]], fixed = exact, old_levels)
    new_levels[slicer] <- replacements[[i]]
    old_levels[slicer] <- ""
  }
  re_level(fact, new_levels)
}

otherize_levels_rank <- function(fact, cutoff, other = "other", otherize_empty_levels = TRUE, include_ties = TRUE) {
  fact_levels <- levels(fact)
  if (otherize_empty_levels) {
    otherize_empty_levels <- "" %c% fact_levels
  }
  lookup_table <- data.table(levels = fact_levels, count = tabulate_factor(fact))
  print(lookup_table)
  if (other %c% fact_levels) {
    cutoff <- cutoff + 1L
  }
  if (otherize_empty_levels) {
    lookup_table <- lookup_table[levels != ""]
  }
  setkey(lookup_table, count)
  if (include_ties) {
    count_at_cutoff <- lookup_table[.N-cutoff, count]
    dropped_levels <- lookup_table[count < count_at_cutoff, levels]
  } else {
    dropped_levels <- lookup_table[1:(.N-cutoff), levels]
  }
  if (otherize_empty_levels) {
    dropped_levels <- append(dropped_levels, "")
  }
  drop_levels(fact, drop = dropped_levels, to = other)
}

otherize_levels_prop <- function(fact, cutoff, other = "other", otherize_empty_levels = TRUE) {
  fact_levels <- levels(fact)
  if (otherize_empty_levels) {
    otherize_empty_levels <- "" %c% fact_levels
  }
  lookup_table <- data.table(levels = fact_levels, prop = level_proportions(fact))
  if (otherize_empty_levels) {
    lookup_table <- lookup_table[levels != ""]
  }
  dropped_levels <- lookup_table[prop < cutoff, levels]
  if (otherize_empty_levels) {
    dropped_levels <- append(dropped_levels, "")
  }
  drop_levels(fact, drop = dropped_levels, to = other)
}

otherize_levels_count <- function(fact, cutoff, other = "other", otherize_empty_levels = TRUE) {
  fact_levels <- levels(fact)
  if (otherize_empty_levels) {
    otherize_empty_levels <- "" %c% fact_levels
  }
  lookup_table <- data.table(levels = fact_levels, count = tabulate_factor(fact))
  if (otherize_empty_levels) {
    lookup_table <- lookup_table[levels != ""]
  }
  dropped_levels <- lookup_table[count < cutoff, levels]
  if (otherize_empty_levels) {
    dropped_levels <- append(dropped_levels, "")
  }
  drop_levels(fact, drop = dropped_levels, to = other)
}

otherize_groups_count <- function(dt, cutoff, group_cols = colnames(dt), cols_to_otherize = colnames(dt), other = "other") {
  count_table <- dt[, .N, by = group_cols]
  for_removal <- count_table[N < cutoff, group_cols, with = FALSE]
  otherize_groups_base(dt, for_removal, group_cols, cols_to_otherize, other)
}

otherize_groups_prop <- function(dt, cutoff, group_cols = colnames(dt), cols_to_otherize = colnames(dt), other = "other") {
  dt_nrows <- nrow(dt)
  prop_table <- dt[, .(prop = .N / dt_nrows), by = group_cols]
  for_removal <- prop_table[prop < cutoff, group_cols, with = FALSE]
  otherize_groups_base(dt, for_removal, group_cols, cols_to_otherize, other)
}

otherize_groups_rank <- function(dt, cutoff, group_cols = colnames(dt), cols_to_otherize = colnames(dt), other = "other", include_ties = TRUE) {
  count_table <- dt[, .N, by = group_cols]
  setkey(count_table, N)
  if (include_ties) {
    N_at_cutoff <- count_table[.N-cutoff, N]
    for_removal <- count_table[N < N_at_cutoff, group_cols, with = FALSE]
  } else {
    for_removal <- count_table[1:(.N-cutoff), group_cols, with = FALSE]
  }
  otherize_groups_base(dt, for_removal, group_cols, cols_to_otherize, other)
}

otherize_groups_base <- function(dt, for_removal, group_cols, cols_to_otherize, other) {
  otherize_ncols <- length(cols_to_otherize)
  other <- recycle_arguments(other, otherize_ncols)
  removal_nrows <- nrow(for_removal)
  for (c in seq_len(removal_nrows)) {
    rows <- dt[for_removal[c], on = group_cols, which = TRUE]
    for (d in seq_len(otherize_ncols)) {
      set(dt, i = rows, j = cols_to_otherize[[d]], value = other[[d]])
    }
  }
}

NA_of_same_type <- function(thing, factor_as_blank = TRUE) {
  return (switch(class(thing), 
                 "integer"   = NA_integer_, 
                 "numeric"   = NA_real_, 
                 "factor"    = if.else(factor_as_blank, "", factor(NA)), 
                 "character" = NA_character_, 
                 "logical"   = NA))
}

NA_of_same_type_by_col <- function(thing, cols = colnames(thing), factor_as_blank = TRUE) {
  NAs <- list()
  for (col in cols) {
    NAs <- append(NAs, NA_of_same_type(thing[[col]], factor_as_blank = factor_as_blank))
  }
  return (NAs)
}

if (is_package_loaded("data.table")) {
  # currently requires both codes and values to be factors before calling the function
  # need to ensure NAs are handled properly
  # need to also ensure that different types of missing/incomplete values are handled (what if "" and NA are both present?)
  fill_matching_values <- function(data, code_col, value_col, drop.codes = FALSE, backfill = FALSE, drop.values = FALSE, NA_code = NA_of_same_type(data[[code_col]]), NA_value = NA_of_same_type(data[[value_col]]), assume.exclusive = FALSE) {
    NA_index_value <- NA_index(data[[value_col]], NA_value)
    if (is_empty(NA_index_value)) {
      code_to_values <- eval(parse(text = paste0("data[, .('codes' = ", code_col, ", 'values' = ", value_col, ")]")))
    } else {
      code_to_values <- eval(parse(text = paste0("data[as.integer(", value_col, ") != ", as.character(NA_index_value), "L, .('codes' = ", code_col, ", 'values' = ", value_col, ")]")))
    }
    NA_index_code <- NA_index(code_to_values[["codes"]], NA_code)
    if (is_empty(NA_index_code)) {
      code_to_values <- unique(code_to_values)
    } else {
      code_to_values <- unique(code_to_values[as.integer(codes) != NA_index_code, ])
    }
    code_to_values <- code_to_values[, .(values, 'GRPN' = .N), keyby = codes]
    indeterminate_codes <- code_to_values[GRPN > 1L, as.character(unique(codes))]
    if (assume.exclusive) {
      for (a_code in indeterminate_codes) {#*** more possibly empty NA_index_value instances below
        present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", code_col, " == a_code & as.integer(", value_col, ") != ", as.character(NA_index_value), "L, ", value_col, "]"))))
        replace_levels(code_to_values[["values"]], from = present_levels[["others"]], to = present_levels[["mode"]])
      }
    } else {
      for (a_code in indeterminate_codes) {
        present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", code_col, " == a_code & as.integer(", value_col, ") != ", as.character(NA_index_value), "L, ", value_col, "]"))))
        set(code_to_values, i = which(code_to_values[["codes"]] == a_code), j = "values", present_levels[["mode"]])
      }
    }#*** some of below may also need to be under !assume.exclusive
    code_to_values <- unique(code_to_values[, .(codes, values)])
    verified_codes <- code_to_values[["codes"]]
    
    NA_index_code <- NA_index(data[[code_col]], NA_code)
    if (is_empty(NA_index_code)) {
      all_codes <- levels(data[[code_col]])
    } else {
      if (is.na(NA_code)) {
        all_codes <- unique(data[[code_col]] %[]% (as.integer(.) != NA_index_code))
      } else {
        all_codes <- levels(data[[code_col]]) %[!=]% ""
      }
    }
    unverified_codes <- all_codes %d% verified_codes
    
    if (is_empty(unverified_codes)) {
      code_to_values_for_join <- code_to_values
    } else {
      if (drop.codes) {
        drop_levels(data[[code_col]], drop = unverified_codes, to = NA_code)
        code_to_values_for_join <- rbindlist(list(code_to_values, data.table(codes = NA_code, values = NA_value)))
      } else {
        code_to_values_for_join <- rbindlist(list(code_to_values, data.table(codes = unverified_codes, values = NA_value)))
      }
    }
    
    joined_data <- eval(parse(text = paste0("code_to_values_for_join[data, .(values), on = c('codes' = '", code_col, "')]")))
    NA_index_code <- NA_index(data[[code_col]], NA_code)
    if (!is_empty(NA_index_code)) {
      slicer <- as.integer(data[[code_col]]) != NA_index_code
      eval(parse(text = paste0("data[slicer, ", value_col, " := joined_data[['values']][slicer]]")))#*** maybe rewrite using set()
    } else {
      eval(parse(text = paste0("data[, ", value_col, " := joined_data[['values']]")))#*** maybe rewrite using set()
    }
    
    NA_index_code <- NA_index(data[[code_col]], NA_code)
    if (is_empty(NA_index_code)) {
      value_to_codes <- eval(parse(text = paste0("data[, .('values' = ", value_col, ", 'codes' = ", code_col, ")]")))
    } else {
      value_to_codes <- eval(parse(text = paste0("data[as.integer(", code_col, ") != ", as.character(NA_index_code), "L, .('values' = ", value_col, ", 'codes' = ", code_col, ")]")))
    }
    NA_index_value <- NA_index(value_to_codes[["values"]], NA_value)
    if (is_empty(NA_index_value)) {
      value_to_codes <- unique(value_to_codes)
    } else {
      value_to_codes <- unique(value_to_codes[as.integer(values) != NA_index_value, ])
    }
    value_to_codes <- value_to_codes[, .(codes, 'GRPN' = .N), keyby = values]
    indeterminate_values <- value_to_codes[GRPN > 1L, as.character(unique(values))]
    if (assume.exclusive) {
      for (a_value in indeterminate_values) {#*** more possibly empty NA_index_value instances below
        present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", value_col, " == a_value & as.integer(", code_col, ") != ", as.character(NA_index_code), "L, ", code_col, "]"))))
        replace_levels(value_to_codes[["codes"]], from = present_levels[["others"]], to = present_levels[["mode"]])
      }
    } else {
      for (a_value in indeterminate_values) {
        present_levels <- mode_and_others_factor(eval(parse(text = paste0("data[", value_col, " == a_value & as.integer(", code_col, ") != ", as.character(NA_index_code), "L, ", code_col, "]"))))
        set(value_to_codes, i = which(value_to_codes[["values"]] == a_value), j = "codes", present_levels[["mode"]])
      }
    }
    value_to_codes <- unique(value_to_codes[, .(values, codes)])
    verified_values <- value_to_codes[["values"]]
    
    NA_index_value <- NA_index(data[[value_col]], NA_value)
    if (is_empty(NA_index_value)) {
      all_values <- levels(data[[value_col]])
    } else {
      if (is.na(NA_value)) {
        all_values <- unique(data[[value_col]] %[]% (as.integer(.) != NA_index_value))
      } else {
        all_values <- levels(data[[value_col]]) %[!=]% ""
      }
    }
    unverified_values <- all_values %d% verified_values
    
    if (is_empty(unverified_values)) {
      value_to_codes_for_join <- value_to_codes
    } else {
      if (drop.values) {
        drop_levels(data[[value_col]], drop = unverified_values, to = NA_value)
        value_to_codes_for_join <- rbindlist(list(value_to_codes, data.table(values = NA_value, codes = NA_code)))
      } else {
        value_to_codes_for_join <- rbindlist(list(value_to_codes, data.table(values = unverified_values, codes = NA_code)))
      }
    }
    
    if (backfill) {
      joined_data <- eval(parse(text = paste0("value_to_codes_for_join[data, .(codes), on = c('values' = '", value_col, "')]")))
      NA_index_value <- NA_index(data[[value_col]], NA_value)
      if (!is_empty(NA_index_value)) {
        slicer <- as.integer(data[[value_col]]) != NA_index_value
        eval(parse(text = paste0("data[slicer, ", code_col, " := joined_data[['codes']][slicer]]")))#*** maybe rewrite using set()
      } else {
        eval(parse(text = paste0("data[, ", code_col, " := joined_data[['codes']]]")))#*** maybe rewrite using set()
      }
    }
    invisible(data)
  }
  
  fill_matching_values_ <- function(data, code_col, value_col, ...) {
    code_col <- deparse(substitute(code_col))
    value_col <- deparse(substitute(value_col))
    fill_matching_values(data, code_col, value_col, ...)
  }
  
  correct_mistakes <- function() {
    # do fill_matching_values()-like stuff but correct based on other columns (useful for me here as I'll be correcting City and Country based on latitude and longitude)
    # get code_to_values (here observation 1 and observation 2 or obseration and corollary)
    # n-dimensional (easy case 2-dimensional like I have) corresponding values that can be used (distance deviation from median-wise) to determine whether two corollaries of an observation are different and should be different or not *and/or* whether two observations are labeled differently but should be labeled the same
  }
}

by_col <- function(data, col_func, cols = colnames(keep(data, is.factor)), ...) {
  for (col in cols) {
    col_func(data[[col]], ...)
  }
  invisible(data)
}

fix_NAs_by_col <- function(data, NA_level = "<NA>", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    if (anyNA(data[[col]])) {
      data[[col]] %>% add_levels(NA_level)
      set(data, i = which(is.na(data[[col]])), j = col, NA_level)
    }
  }
  invisible(data)
}

format_levels_by_col <- function(data, func, cols = colnames(keep(data, is.factor)), ...) {
  for (col in cols) {
    format_levels(data[[col]], func, ...)
  }
  invisible(data)
}

format_similar_levels_by_col <- function(data, pairings, exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    format_similar_levels(data[[col]], pairings)
  }
  invisible(data)
}

replace_level_by_col <- function(data, from, to, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    replace_level(data[[col]], from, to)
  }
  invisible(data)
}

replace_levels_by_col <- function(data, from, to, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    replace_levels(data[[col]], from, to)
  }
  invisible(data)
}

rename_levels_by_col <- function(data, changes, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    rename_levels(data[[col]], changes)
  }
  invisible(data)
}

rename_similar_levels_by_col <- function(data, changes, exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    rename_similar_levels(data[[col]], changes, exact)
  }
  invisible(data)
}

add_levels_by_col <- function(data, add, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    add_levels(data[[col]], add)
  }
  invisible(data)
}

drop_levels_by_col <- function(data, drop, to = "", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    drop_levels(data[[col]], drop, to)
  }
  invisible(data)
}

drop_similar_levels_by_col <- function(data, drop, to = "", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    drop_similar_levels(data[[col]], drop, to, exact)
  }
  invisible(data)
}

drop_missing_levels_by_col <- function(data, to = "", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    drop_missing_levels(data[[col]], to)
  }
  invisible(data)
}

drop_levels_formula_by_col <- function(data, expr, to = "", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    drop_levels_formula(data[[col]], expr, to)
  }
  invisible(data)
}

keep_levels_by_col <- function(data, keep, to = "", cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    keep_levels(data[[col]], keep, to)
  }
  invisible(data)
}

keep_similar_levels_by_col <- function(data, keep, to = "", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    keep_similar_levels(data[[col]], keep, to, exact)
  }
  invisible(data)
}

reduce_levels_by_col <- function(data, rules, other = "other", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    reduce_levels(data[[col]], rules, other, exact)
  }
  invisible(data)
}

otherize_levels_rank_by_col <- function(data, rank, other = "other", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    otherize_levels_rank(data[[col]], rank, other, exact)
  }
  invisible(data)
}
  
otherize_levels_prop_by_col <- function(data, cutoff, other = "other", exact = FALSE, cols = colnames(keep(data, is.factor))) {
  for (col in cols) {
    otherize_levels_prop(data[[col]], cutoff, other, exact)
  }
  invisible(data)
}

fill_matching_values_by_col <- function(data, code_cols, value_cols, drop.codes = FALSE, backfill = FALSE, drop.values = FALSE, NA_code = NA_of_same_type_by_col(select(data, code_cols)), NA_value = NA_of_same_type_by_col(select(data, value_cols)), assume.exclusive = FALSE) {
  assert_that(length(value_cols) == length(code_cols), 
              msg = "There is a different number of value and code columns")
  num_cols <- length(value_cols)
  drop.codes       <- recycle_arguments(drop.codes,       num_cols)
  backfill         <- recycle_arguments(backfill,         num_cols)
  drop.values      <- recycle_arguments(drop.values,      num_cols)
  NA_value         <- recycle_arguments(NA_value,         num_cols)
  NA_code          <- recycle_arguments(NA_code,          num_cols)
  assume.exclusive <- recycle_arguments(assume.exclusive, num_cols)
  for (i in seq_along(value_cols)) {
    fill_matching_values(data, code_col = code_cols[[i]], value_col = value_cols[[i]], drop.codes = drop.codes[[i]], backfill = backfill[[i]], drop.values = drop.values[[i]], NA_value = NA_value[[i]], NA_code = NA_code[[i]], assume.exclusive = assume.exclusive[[i]])
  }
  invisible(data)
}


### Functional Operators ----------------------------------------------------

compose <- function(f, g) {
  function(...) f(g(...))
}

compose_rev <- function(f, g) {
  function(...) g(f(...))
}


### Infix Functions ---------------------------------------------------------

### convenience

# safe defaults
`%||%` <- function(a, b) if (!is.null(a)) a else b
`%NA%` <- function(a, b) if (!is.na(a)) a else b
`%OR%` <- function(a, b) if (!is.null(a) && !is_empty(a) && !is.na(a)) a else b

# slicing
`%[==]%` <- function(a, b) a[a == b]
`%[!=]%` <- function(a, b) a[a != b]
`%[>]%`  <- function(a, b) a[a > b]
`%[>=]%` <- function(a, b) a[a >= b]
`%[<]%`  <- function(a, b) a[a < b]
`%[<=]%` <- function(a, b) a[a <= b]

# complicated slices
`%[]%`   <- function(a, b) {
  a[eval(parse(text = paste0(deparse(substitute(a)), " %>% ", deparse(substitute(b)))), env = parent.frame())]
}
`%[!]%`  <- function(a, b) {
  a[!eval(parse(text = paste0(deparse(substitute(a)), " %>% ", deparse(substitute(b)))), env = parent.frame())]
}

### logical operators
`%><%` <- function(a, b) xor(a, b)

# bitwise logical operators
`%&%` <- function(a, b) bitwAnd(a, b)
`%|%` <- function(a, b) bitwOr(a, b)
`%^%` <- function(a, b) bitwXor(a, b)

# bitshifts
`%<<%` <- function(a, n) bitwShiftL(a, n)
`%>>%` <- function(a, n) bitwShiftR(a, n)

### set operations

# set operations, standard
`%u%` <- function(a, b) base::union(a, b)
`%i%` <- function(a, b) base::intersect(a, b)
`%e%` <- function(a, b) base::setequal(a, b)
`%d%` <- function(a, b) base::setdiff(a, b)
`%dd%` <- function(a, b) base::setdiff(base::union(a, b), base::intersect(a, b))

# set operations, length one
`%c%` <- function(a, b) {
  if (length(a) > 1L) debug_message("%c%: condition (LHS) length greater than 1")
  return (is.element(a[1], b))
}

# set operations, negations
`%!c%` <- function(a, b) {
  if (length(a) > 1L) debug_message("%!c%: condition (LHS) length greater than 1")
  return (!is.element(a[1], b))
}
`%!in%` <- function(a, b) {
  return (!(a %in% b))
}

# data.table add-ons
if (!(is_package_loaded("data.table") && exists("like") && is.function(like))) {
  if (!is_package_installed("data.table")) {
    between <- function(a, b1, b2) (a >= b1) & (a <= b2)
    `%between%` <- function(a, b) between(a, b[1], b[2])
    like <- function(a, b) grepl(pattern = b, a)
    `%like%` <- function(a, b) like(a, b)
  } else {
    between <- data.table::between
    `%between%` <- data.table::`%between%`
    like <- data.table::like
    `%like%` <- data.table::`%like%`
  }
}
`%!between%` <- function(a, b) !between(a, b[1], b[2])
`%!in%` <- function(a, b) !is.element(a, b)
`%!like%` <- function(a, b) !like(a, b)
`%exactlylike%` <- function(a, b) grepl(pattern = b, fixed = TRUE, a)
`%!exactlylike%` <- function(a, b) !grepl(pattern = b, fixed = TRUE, a)

`%whichbetween%` <- function(a, b) a[a %between% b]
`%which!between%` <- function(a, b) a[a %!between% b]
`%whichin%` <- function(a, b) a[a %in% b]
`%which!in%` <- function(a, b) a[a %!in% b]
`%whichlike%` <- function(a, b) a[a %like% b]
`%which!like%` <- function(a, b) a[a %!like% b]
`%whichexactlylike%` <- function(a, b) a[a %exactlylike% b]
`%which!exactlylike%` <- function(a, b) a[a %!exactlylike% b]

`%contain%` <- function(a, b) {
  results <- rep(FALSE, length(a))
  for (p in b) {
    results[a %like% p] <- TRUE
  }
  return (results)
}
`%!contain%` <- function(a, b) {
  results <- rep(TRUE, length(a))
  for (p in b) {
    results[a %like% p] <- FALSE
  }
  return (results)
}
`%exactlycontain%` <- function(a, b) {
  results <- rep(FALSE, length(a))
  for (p in b) {
    results[a %exactlylike% p] <- TRUE
  }
  return (results)
}
`%!exactlycontain%` <- function(a, b) {
  results <- rep(TRUE, length(a))
  for (p in b) {
    results[a %exactlylike% p] <- FALSE
  }
  return (results)
}
`%whichcontain%` <- function(a, b) a[a %contain% b]
`%which!contain%` <- function(a, b) a[a %!contain% b]
`%whichexactlycontain%` <- function(a, b) a[a %exactlycontain% b]
`%which!exactlycontain%` <- function(a, b) a[a %!exactlycontain% b]

### functional operators
`%.%` <- compose
`%,%` <- compose_rev
