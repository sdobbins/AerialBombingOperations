# @author Scott Dobbins
# @version 0.9.9.6
# @date 2017-11-19 01:00


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


### Fillers -----------------------------------------------------------------

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

fill_of_same_type <- function(thing, factor_as_blank = TRUE) {
  return (switch(class(thing), 
                 "integer"   = 0L, 
                 "numeric"   = 0, 
                 "factor"    = if.else(factor_as_blank, "", factor("")), 
                 "character" = "", 
                 "logical"   = FALSE))
}


### Helpers -----------------------------------------------------------------

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

is_position_in_vector <- function(vec, pos) {
  if (is_empty(vec)) {
    message("No index position is within an empty vector. Returning FALSE.")
    return (FALSE)
  } else {
    return (pos %between% c(1L, length(vec)))
  }
}


### Reorganizers ------------------------------------------------------------

remove_and_reinsert <- function(things, from, at, fill = NA_of_same_type(things)) {
  assert_that(all(is_position_in_vector(things, from)), 
              msg = "You attempted to extract from a position outside the vector.")
  if (from[1] == at) {
    message("You reinserted (a) vector element(s) back into the same spot.")
    return (things)
  } else {
    end_of_things <- length(things)
    if (length(from) + at - 1L > end_of_things) {
      if (is.na(fill)) {
        message("You reinserted at least one element outside the previous range of the original vector. NA(s) generated.")
      } else {
        message("You reinserted at least one element outside the previous range of the original vector. The gap was filled with your choice of fill.")
      }
    }
    rest_of_things <- things[-from]
    if (at == 1L) {
      return (c(things[from], rest_of_things))
    } else {
      first_positions <- 1L:(at-1L)
      return (c(rest_of_things[first_positions], things[from], rest_of_things[-first_positions]))
    }
  }
}

cycle <- function(vec, step = 1L) {
  assert_that(!is.na(step), 
              msg = "Can't cycle vector by NA amount of steps.")
  vec_length <- length(vec)
  if (step > vec_length) {
    message("Cycled vector by more steps than the vector has in its length, so vector was wrapped around.")
    step <- step %% vec_length
  }
  first_portion <- step:vec_length
  return (c(vec[first_portion], vec[-first_portion]))
}

if (is_package_loaded("data.table")) {
  lead <- function(vec, step = 1L, fill = NULL) {
    if (is.null(fill)) {
      return (shift(vec, n = step, fill = fill_of_same_type(vec), type = 'lead'))
    } else {
      return (shift(vec, n = step, fill = fill, type = 'lead'))
    }
  }
  
  lag <- function(vec, step = 1L, fill = NULL) {
    if (is.null(fill)) {
      return (shift(vec, n = step, fill = fill_of_same_type(vec), type = 'lag'))
    } else {
      return (shift(vec, n = step, fill = fill, type = 'lag'))
    }
  }
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


### Statistics --------------------------------------------------------------

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
