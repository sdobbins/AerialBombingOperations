# @author Scott Dobbins
# @version 0.9.8.1
# @date 2017-08-15 21:00


### Version History ###

0.1
This version mapped out semi-cleaned longitudinal and latitudinal target data
  from the United States Airforce in WW2 onto one color map with map labels.
Major changes: implemented basic data cleaning and basic plotting


0.2
This version mapped out semi-cleaned longitudinal and latitudinal target data
  from the United States Airforce in 4 major conflicts during the 20th century
  onto a selection of maps with map label options.
Major changes: added other datasets; added other maps and map options


0.3
This version maps out semi-cleaned longitudinal and latitudinal target data
  from the United States Airforce in 4 major conflicts during the 20th century
  onto a selection of maps with map label options and clickable, detailed tooltips.
Major changes: tooltips

0.3.1	Separated scripts into Shiny 3-file structure (with README.rm and code_graveyard.txt)
	Fixed leaflet tile attributions (sort of) and removed htmlEscapes
	Added app_id and app_code for HERE map usage
	Removed redundant initial drawings of map and labels
0.3.2	Made code much more readable with line breaks
	Edited formatting of popup labels

0.4
This version maps out all semi-cleaned longitudinal and latitudinal target data
  from the United States Airforce in 4 major conflicts during the 20th century
  onto a selection of maps with map label options and clickable, detailed tooltips.
Major changes: big data clean; incorporation of all Vietnam and Korea data

0.5
This version maps out all semi-cleaned longitudinal and latitudinal target data
  from the United States Airforce in 4 major conflicts during the 20th century
  onto a selection of maps with map label options and clickable, well-formatted, 
  detailed tooltips using speed advantages brought to it by data table functions.
Major changes: help cleaning and formatting with helper.R; speed-optimized with data table

0.6
This version maps out all semi-cleaned longitudinal and latitudinal target data
  from the United States Airforce in 4 major conflicts during the 20th century
  onto a selection of maps with map label options and clickable, well-formatted, 
  detailed tooltips using speed advantages brought to it by data table functions
  and cached outputs (while still enabling full refreshes of the displayed data).
Major changes: further organized code with cleaner.R; development-friendly caching

0.6.1	Additionally cleared up how data is input and processed
	Fixed Korean war dates
	Fixed how data is saved and read

0.7
This version maps out all semi-cleaned longitudinal and latitudinal target data
  from the United States Airforce in 4 major conflicts during the 20th century
  onto a selection of maps with map label options and clickable, well-formatted, 
  detailed tooltips using speed advantages brought to it by data table functions
  and cached outputs (while still enabling full refreshes of the displayed data).
Major changes: Shiny dashboard

0.8
This version maps out all semi-cleaned longitudinal and latitudinal target data
  from the United States Airforce in 4 major conflicts during the 20th century
  onto a selection of maps with map label options and clickable, well-formatted, 
  detailed tooltips using speed advantages brought to it by data table functions
  and cached outputs (while still enabling full refreshes of the displayed data).
  It also plots a bin-editable histogram of missions by date for each conflict.
Major changes: full sidebar; tabs for each war

0.8.1	Added stat boxes in the overview tab
	Made date restrictions apply to all data accessed in all tabs
	Made sampling occur after date restriction changes
	Opacity now updates with sample size as well

0.9
Version for class lecture
Major changes: Sandbox added; labeling added
Notes: sidebar and other parts reduced to what works

0.9.1	Fixed civilian tab
	Moved "which wars?" selectizeInput to the sidebar
	Made "which wars?" update civilian tab
	Increased speed of sandbox and removed one typo-bug

0.9.2	Added explanatory text box to overview tab for spacing
	Put comma delimiters in infoBoxes
	Increased maximum sample size
	Filtered out (0,0) GPS locations

0.9.3	Properly formatted tooltips for all conflicts
	Included all sandboxes for all conflicts

0.9.4	Functionalized code with vectorized helper functions
	Broke code up into more representative scripts

0.9.5	Implemented selectize inputs so that they filter and get updated
	Used data.table syntax to improve filtering and fix bugs in graphs

0.9.6	Redesigned cleaner.R to edit strings as factors for vast speed improvement
	Introduced parallel processing for a few steps

0.9.7	Wrote script to further clean WW2 weapon types, numbers, and total weights
	Implemented DataTable display

0.9.8	Improved style of code
	Did unit testing
	Created utils.R for debugging purposes
	Split README into README and NEWS

### Future version plans:

0.9.9	<finalize graphs and other tabs>

1.0	<misc bug fixes, data polishing, and tooltip improvement>
Complete version


### Future change notepad:

#*#* see if you can use Map and Reduce in code; basically, get rid of some for loops

#*#*#* read through R Graphics Cookbook to fix ggplot2/ggvis graphs and such

#*#*#*#* some WW2 bombs are encoded by weight of explosive, and others by gross weight

#*#*#*#*#* speed up fix_missing_values code

#*#*#*#*#*#*#* generate minimal reproducible example whereby the processor clean_data walking refactor_and_order step alters levels (by adding names) in bombs_data tables even though the two aren't (shouldn't be) linked

Data Processing:
# also do some num * per-unit-weight = total-weight calculation verification for Korea 2 and others
# perhaps update WW2 (and potentially other wars') weapons types that include clusters to just be counted as singlets but with a mention that they were originally in clusters
# change Weapon_Unit_Weight in Vietnam to something like Weapon_Unit_Total_Weight and then create new column Weapon_Unit_Weight that reads from Weapon_Type and then fills in if possible from Weapon_Unit_Total_Weight if the read value is blank

Sandboxes/Graphics:
# can make histogram semi-transparent with alpha and still do fill-based grouping in geom_histogram with position = 'identity'; maybe depending on number of groups, pick alpha and switch between geom_polyfreq and geom_histogram; I bet fill-based grouping would also work in geom_polyfreq
# show mean with diamond (shape 23)
# geom_violin(trim = FALSE)
# fix formatting of title, axes, background, and legend in sandbox
# make histograms able to plot density of num_missions (this is just a normal histogram), num_aircraft, num_bombs, and weight_bombs
# fix categories with an overwhelming number of choices by allowing the graphing of top or bottom (or evenly spaced sorted sample of) <x> choices
# turn label text sideways above a certain number of labels
# maybe reorder bars in plots in ascending order
# maybe use coord_flip() to help category names not overlap

Programming Style:
# further functionalize a lot of repeated code
# move all label changes into labs() function
# fix the fact that the app saves locally when it should save externally
# maybe further functionalize cleaner.R cleaning sections for each war

Programming Quality:
# merge together Korea1 and Korea2 after cleaning
# compare timing with and without JIT compiler
# figure out a way to suppress or fix all those warnings (and also random data.table updates and package information)
# make sure filters returning NAs don't mess things up--check if nomatch = 0 would be better
# see if different map_*() functions can improve speed
# see if broom can fix issues with the vectorized strsplit helper functions I made
# maybe change local absolute paths to relative paths--use basename/dirname to help with filepaths stuff / use file.path to make filepaths platform independent (path.expand may help as well)
# maybe fix proper_noun_phrase_vectorized by filtering down amount of data that needs to be processed at each step: first check if the given line is empthy, then check if the line is only one word long (doesn't contain any of the split characters) and then check for each character before each step (or keep large vectors of logicals about whether each symbol is contained in each row and use those)

Small Improvements:
# times with hms or ITime
# Change heatmap parameters so it's not as odd-looking, especially changing so much between zoom levels: change blur and radius (and maybe even max) parameters?
# convert Expl, Incd, and Frag combined into a single matrix column (use df$col <- matrix() for this
# also add in new columns and/or rephrase descriptions of old columns (for sandbox)
# maybe actually convert the times into time objects

Significant Improvements:
# Edit civilian map to have not just number of bombing missions but also number of bombs and weight of bombs to get a better sense of danger/intensity
# Better ggplots, maybe a self-playing gif of bombs droppedover time for each conflict
# Could also allow users to link (using html) to relevant Wikipedia articles on certain aspects of the conflicts (airframes, campaigns, etc.)
# maybe put background of flags with low alpha over territories they controlled
# also include vague categories like "incendiary", "fragmentary", and "high explosive" (as in WW2) as possible selections in weapons drop down (depending on what wars and countries are selected); provide fragmentary, incendiary, and high explosive columns in non-WW2 databases as well
# maybe also add a kinetic weapon column for WW2 and others
# rewrite type-specific if_else in Rcpp so that it only calculates the values it needs (or just write your own if.else in R that automatically reshuffles the code to do everything <- `true`, everything[places where not TRUE] <- `false`)

Misc/Other Projects:
# broom for list-columns of models etc
# Make efficient for use on shinyapps.io through SQLite
# Maybe match (or fail to match) the data with the historical record.

Presentation/Writing:
# eventually, Rmarkdown stuff

