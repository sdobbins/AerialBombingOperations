# @author Scott Dobbins
# @version 0.9.9.5
# @date 2017-11-10 16:00


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
	Introduced parallel processing for a few steps (later removed)

0.9.7	Wrote script to further clean WW2 weapon types, numbers, and total weights
	Implemented DataTable display

0.9.8	Improved style of code
	Included unit testing
	Created utils.R for debugging purposes
	Split README into README and NEWS

0.9.9	Added animation
	Added cinema and maps
	Improved sandbox plots
	Added Wikipedia links to and reformatted tooltips
	Functionalized more regex code
	Further reduced redundant code
	Fixed more data cleaning errors

### Future version plans:

1.0	<misc bug fixes, data polishing>
Complete version
