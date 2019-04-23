# North Coast Continuous Dissolved Oxygen Visualizer

This app is to help visualize dissolved oxygen data around 14 sampling locations in Oregon's North Coast region over the span of about a decade.

This app is best used with some instruction from the creators. Its usability could be improved with the introduction of text prompts and tool tips.

This is especially true of some of the Plotly functionality on the "Display Continuous Data" tab. In a pilot in another section of the department, a video introduction was used with great success to introduce users to the functionality of the app, including plots. This video was then embedded in the front tab of the app, which helped the users who chose to view it considerably.

The data is cleaned before being loaded in the "FinalDataPrep.R" script. Right now, the app brings in data from three locations: AWQMS, LASAR, and Excel, that was preprocessed and collected in a file "ShinyAllData.Rdata." The LASAR data should now reside entirely in AWQMS, so this could be revised in the future. Entries that were nearly duplicate were typically left in to improve transparency.

"FinalDataPrep.R" brings in data from "ShinyAllData" and as well as 




Here are some of the next steps scheduled for the app:
*Fixing all column names
