Background
SyndromicEpiAlert was developed in response to the need for rapid analysis of data from the NSSP ESSENCE system. Its primary aim is to identify potential visit clusters or trends that are statistically anomalous. The initial version of SyndromicEpiAlert was tailored for quick data product acquisition, necessitating some proficiency in R and regular manual data input by an epidemiologist.

This iteration, however, minimizes the need for extensive R knowledge. It introduces a Graphical User Interface (GUI) and expands the range and type of data products available.
Table of Contents
1.	Data Source and Caveats
2.	Prerequisite Steps
3.	Primary Use Guide
4.	Script Description
5.	Methods
6.	Contact
Data Source and Caveats
SyndromicEpiAlert v2 uses both Emergency Department and Climate data (Temperature and PM2.5 readings) from NSSP ESSENCE. As such, all required data caveats and restrictions are relevant to this data product, and being aware of NSSP downtimes is highly advised. 
Prerequisite Steps
1.	Obtain an NSSP ESSENCE account through completing a DSA with the WA-DOH RHINO Team and have an up to date username and password
2.	Download R (min version 4.2), preferably on C drive. 
3.	Download RStudio and also have it installed locally. 
Primary Use Guide
1.	Copy this directory and files over to your local drive, and note what the file location becomes. 
2.	Double click the Test_Shiny_UIPage.RProj file, and it should open up a new window of RStudio. Clear environment using Session > Clear Workspace.
3.	Open STEP_0_PACKAGES_TO_INSTALL.R . Next, select all lines and click run. This may take a few moments, as packages for all operations will need to be installed. 
4.	Open app.R . You should be greeted by a large script in the main window. In the upper right hand corner, there should be “Run App”. Left click that icon. If there is a warning message about missing packages, click “Yes” to allow for needed installs. 
5.	A Graphical User Interface (GUI) should appear called ESSENCE Extractor. Here, you will need to complete a few tasks. 
a.	Enter your username and password exactly as they are from ESSENCE. 
b.	Select a folder path (preferably where you have the scripts currently operating from). 
c.	Syndromes. Each selection will generate a core report, whose visits will all be captured by that particular definition.
d.	Combo-Examination. If desired, click on Combo-Examination, this will cause several new fields to appear. 
i.	Syndromes 1 and 2 are possible selections from the above list, whose data will then be combined and examined, finding visit records where both syndromes have been assigned to a visit.
ii.	Paired Syndrome Name is whatever you want to call the syndrome combination. While spaces, - and + are all allowed, it’s generally advised to keep to alphanumeric characters as much as possible, and somewhat short. For example, “All Drug v2” and “Persons Experiencing Homelessness dd v1” could be combined as “PEH + AD”.
e.	Historical Data. If set to TRUE, a second dropdown will give you the option to enter Years of Lookback.
i.	Years of Lookback will be set automatically to 1 year but can be adjusted. This will create a separate query for each syndrome for each year back. For example, if you select 2 syndromes with a 2 year lookback, 6 total reports will be created. 2 for this year, 2 for the previous year, and 2 for the year before that. All reports will be created iteratively and shouldn’t be affected by memory constraints except under extreme circumstances. 
f.	Start Date/ End Date are both calendar entry fields. It is advised to not go more than 3 years back, nor within 2 weeks of present date for either field. Illogical dates will likely kick back an error from an API call. 
g.	Climate Data defaults to “No”, but if selected, will allow for weather data to be displayed, as well as a secondary option to include PM 2.5 Measures. Both will create small timeseries graphs of daily measurements of temperature and PM2.5 in the final reports. 
h.	Age and Gender Graphs, Age Only Graphs, Gender Only Graphs all will determine how age and sex demographics will be displayed, with the demographics being presented in combination or alone for all graphs that use them. You can select any combination of these measures. 
i.	Age Group Selection can be set to PED (pediatic), PIT (Point In Time Count) or the Standard age breaks. This will affect the above graphs in terms of patient age cut off and display. 
j.	Race/Ethnicity Graphs determine if R/E graphs will be displayed, using standard census groups (8 race groups + Hispanic/Non-Hispanic).
k.	Maps will provide maps that are count based; Avoidance of this field is advised if memory is an issue, as is true for its child selection, Spatial Analysis, which does in depth demographic and geographic analysis using 10k trends. 
l.	Hospital Groupings will provide a graph showing where care has been sought by patients who are captured with the syndrome query for dates specified. 
m.	Time of Day Analysis provides a 2-hour * 7 Day heatmap, showing visit density by time block for records captured. 
n.	Time Series Count Graphs provides for a standard count-by-day graph. 
o.	Per 10,000 Visits Graphs display graphs and maps for all previous demographic/feature selections against all visits during that same time that also match the same demographics group(s). 
p.	Natural Language Processing Analysis provides exploratory analysis of free text fields within the patient records: Discharge Diagnosis, Chief Complaint, Admit Reason, and ICD Category. Topic Modeling as well as Social Network Analysis is performed on cleaned data (explanations will be provided in Methods). This is a somewhat computationally intensive task, that increases in burden relative to length of time between start and end date. 
6.	Click Submit . Depending on number of requests and features selected, completion may take several minutes. If you wish to see if progress is going well, return to the R Studio window (without closing the Shiny Application), and view the console. You should see scripts being run and a count-up from 0 to 22, denoting which scripts are processing normally. 
7.	Once Complete, check your folder for an additional folder, which should follow pattern “Start Date – End Date – Syndrome Name”, this should hold 2 additional folders. Report will have a generated HTML report and Data should have precleaned underlying data for use or analysis if desired. If there are 1 or less rows in the main returned data frame from the ESSENCE API, a Null Report will be generated and no data will be provided. 
8.	Move subfolders as desired. 
NOTE: Selecting too many features may generate a pandoc error, which will say that the document or memory is too small. Try deselecting or splitting reports into a few different feature sets and retrying the script.
