# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


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


### Dist Functions ---------------------------------------------------------

dist_index <- function(i, j, l) {
  return ((i / 2) * (2 * l - i - 1L) - (l - j))
}

dist_indices <- function(ind, l) {
  i <- round_to_int(ceiling((2 * ind + 1) / (2 * l - 1)))
  j <- round_to_int(ind + l - ((i * l) - (i * (i + 1L) / 2L)))
  return (c("i" = i, "j" = j))
}


### Attribute Functions -----------------------------------------------------

get_names <- function(vec) {
  return (names(vec) %||% rep("", length(vec)))
}


### Function Operators ------------------------------------------------------

### behavior

if (is_package_installed("purrr")) {
  quiet <- function(func, ...) {
    return (((quietly(func))(...))[['result']])
  }
}


### defaults

null_default_provider <- function(func, default, ...) {
  return (func(...) %||% default)
}

NA_default_provider <- function(func, default, ...) {
  return (func(...) %NA% default)
}

full_default_provider <- function(func, default, ...) {
  return (func(...) %OR% default)
}


### composition

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

# other map functions
map_which <- function(a) map(a, which)

map_func <- function(f) {
  force(f)
  function(a, b) map2(a, b, f)
}
`%map_plus%`   <- map_func(`+`)
`%map_minus%`  <- map_func(`-`)
`%map_times%`  <- map_func(`*`)
`%map_divide%` <- map_func(`/`)


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
