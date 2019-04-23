# North Coast Continuous Dissolved Oxygen Visualizer

This app is to help visualize dissolved oxygen data around 14 sampling locations in Oregon's North Coast region over the span of about a decade.


## Usability
=======

This app is best used with some instruction from the creators. Its usability could be improved with the introduction of text prompts and tool tips.

This is especially true of some of the Plotly functionality on the "Display Continuous Data" tab. In a pilot in another section of the department, a video introduction was used with great success to introduce users to the functionality of the app, including plots. This video was then embedded in the front tab of the app, which helped the users who chose to view it considerably.


## Data prep
The data is cleaned before being loaded in the "FinalDataPrep.R" script. Right now, the app brings in data from three locations: AWQMS, LASAR, and Excel, that was preprocessed and collected in a file "ShinyAllData.Rdata." The LASAR data should now reside entirely in the AWQMS database, so this could be revised by querying AWQMS in the future. Entries that were nearly duplicate were typically left in to improve transparency.

"FinalDataPrep.R" brings in data from several locations. "ShinyAllData" is the continuous dissolved oxygen data that also includes monitoring station id (formerly LASAR id, now MLocID) and typically temperature, pH and conductivity data. 

The "FinalDataPrep" script merges the DO data with some other data. This include "IRDatabase_CriteriaTables.csv", which is a list of spawn codes, spawn date range, and spawn start and spawn end dates. It also includes "DO_crit.csv" which lists DO codes and the DO cutoff level for the time interval you are looking at (30 day, 7 day, etc). Because we had relatively short samples, we only used "crit_Instant", the same cutoff as grab samples. This app would need to be revised for longer term continuous monitoring projects. "TEP_StationsInfo.csv" was created from a GIS file and contains the station information.

"FinalDataPrep" removes some columns that might be useful to some users or managers and should be reviewed. Both it and the app renames columns to make thing friendlier to the viewer - however, if the user needs to export data and merge it back with other sources, it might be better to retain the original column names from AWQMS or the GIS layers if those are consistent.



## Next Steps

=======
The data is cleaned before being loaded in the "FinalDataPrep.R" script. Right now, the app brings in data from three locations: AWQMS, LASAR, and Excel, that was preprocessed and collected in a file "ShinyAllData.Rdata." The LASAR data should now reside entirely in AWQMS, so this could be revised in the future. Entries that were nearly duplicate were typically left in to improve transparency.

"FinalDataPrep.R" brings in data from "ShinyAllData" and as well as 


Here are some of the next steps scheduled for the app:
+ Fixing all column names
+ Updating Estuary/Freshwater components, checking to make sure these categorizations are accurate, and propogating those categories through the app usefully
+ Addressing discrepancies in DO Saturation in the data, as well as creating visualizations for DO Sat or incorporating it along with its state regulatory threshholds
