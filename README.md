# North Coast Continuous Dissolved Oxygen Visualizer

This app is to help visualize dissolved oxygen data around 14 sampling locations in Oregon's North Coast region over the span of about a decade.


## Usability


This app is best used with some instruction from the creators. Its usability could be improved with the introduction of text prompts and tool tips.

This is especially true of some of the Plotly functionality on the "Display Continuous Data" tab. In a pilot in another section of the department, a video introduction was used with great success to introduce users to the functionality of the app, including plots. This video was then embedded in the front tab of the app, which helped the users who chose to view it considerably.


## Data sources

Right now, the app brings in data from three locations: AWQMS, LASAR, and Excel, that was preprocessed and collected in a file "ShinyAllData.Rdata." The LASAR data should now reside entirely in the AWQMS database, so this could be revised by querying AWQMS in the future. Entries that were nearly duplicate were typically left in to improve transparency.

Two sites originally present in the Excel data were excluded because they have not been assigned LASAR/MLocIDs and this is was important for coding the sites in the app. These sites may receive codes in the future.


## Data prep
The data is cleaned and merged before being brought into the app using the "FinalDataPrep.R" script. This script brings in several sources of data. "ShinyAllData" is the continuous dissolved oxygen data that also includes monitoring station id (formerly LASAR id, now MLocID) and typically temperature, pH and conductivity data. 

The "FinalDataPrep" script merges the DO data with some other data sources. Those include "IRDatabase_CriteriaTables.csv", which is a list of spawn codes, spawn date range, and spawn start and spawn end dates. It also includes "DO_crit.csv" which lists DO codes and the DO cutoff level for the sample time interval (30 day, 7 day, etc). Because we had relatively short samples, we only used "crit_Instant", the same cutoff as grab samples. This file would need to be revised for longer term continuous monitoring projects. "TEP_StationsInfo.csv" was created from a GIS file and contains the station information.

The prep script gives a code to each DO observation based on the DO level, the crit_instant DO limit for that site, and whether the sample time falls within the spawning time. This piece of code has not been subject to review and should be formally tested before publication and release.

"FinalDataPrep" removes some columns that might be useful to some users or managers and should be reviewed. Both it and the app renames columns to make thing friendlier to the viewer - however, if the user needs to export data and merge it back with other sources, it might be better to retain the original column names from AWQMS or the GIS layers if those are consistent.



## Next Steps


Here are some of the next steps scheduled for the app:
+ Add a site summary table and/or map to pages with plots that display multiple sites at once, for easy reference 
+ Modify plotly tool options (upper right corner) to improve UX
+ Fix all column names
+ Improved integration of units in interactively generated plots
+ Update Estuary/Freshwater components, check to make sure these categorizations are accurate, and propogate those categories through the app usefully
+ Address discrepancies in DO Saturation in the data, as well as create visualizations for DO Sat or incorporate it along with its state regulatory threshholds
+ Improve tooltips and hoverovers, both inside plotly and in other areas
+ Create a glossary
+ Create "select all" and "deselect all" options for certain plots
+ Make data selectable and downloadable out of more plots (Plotly click javascript feature)
+ Allow more nested data exploration/pop-up plots with Plotly
+ Have potential users try out the app and integrate suggested features
+ Integrate land use maps, ODS spawning critera, coastal estuary layers, DMA layers, or other onto leaflet map
+ Improve summary plots and tables - help user quickly understand trends within sites and across sites
+ Add criteria/limits for temperature
+ Add summaries for conductivity
+ Add more in depth summaries for each site
+ Improve colors for clarity, consistency, and usability
+ Create screen-cap videos if intended to be used by users with no introduction
+ Create in-app User Guide

## Known bugs and issues

+ The plotly selection-to-plot option on the "Display Continuous Data" tab recently stopped working in some circumstances. It seemed as though it might be because of a plotly update, but I wasn't able to track it down in time. It seems that if there are only points that meet criteria, the "Count" plot to the right may not generate properly.

This could potentially be addressed through a revision of plotly and/or a revision of the process that takes the plotly selection and creates the new plot with the data.