# FOCB_WQ
Analysis of Friends of Casco Bay water quality  data from Casco Bay, Maine.

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />
    
Friends of Casco Bay has been monitoring water quality in Casco Bay for nearly
thirty years. in 2016, they began collecting continuous data from a monitoring
location on Cousins Island.  This GitHub repository contains code reviewing,
analyzing and generating graphics based on FOCB monitoring data.

This archive focuses on a handful of conventional water quality parameters,
including temperature, salinity, pH, dissolved oxygen, Secchi depth and 
chlorophyll a.

Related data from FOCB on nutrients and ocean acidification were analyzed in
other data archives.

# Statement of Purpose
CBEP is committed to the ideal of open science.  Our State of the Bay data
archives ensure the science underlying the 2020 State of the Bay report is
documented and reproducible by others. The purpose of these archives is to
release raw data and data analysis code whenever possible to allow others to
review, critique, learn from, and build upon CBEP science.

# Archive Structure
 CBEP 2020 State of the Bay data analysis repositories are divided into from two
 to four sub-folders.  All archives contain an "Original_Data" folder and at
 least one other folder, usually "Graphics" or "Analysis". Other folders, often
 a "Derived_Data" folder are included if necessary.
 
 This archive contains the following sub-folders:

- **Original Data**.  Original data, with a "DATA_SOURCES.md" or "READ ME.txt" file
that documents data sources.  
    **DATA IN THIS FOLDER IS AS ORIGINALLY PROVIDED OR ACCESSED.** 

- **Derived Data**.  Data derived from the original raw data.  Includes
documentation of data reorganization steps, either in the form of files (R
notebooks, Excel files, etc.) that embody data transformations, or via another
README.txt file.

- **Analysis**.  Contains one or more R Notebooks proceeding through the data
analysis steps.

- **Graphics**.  Contains R Notebooks stepping through development of related
graphics, and also raw copies of resulting graphics, usually in \*.png and
\*.pdf formats.  These graphics may differ from graphics as they appear in final
State of the Bay graphical layouts.
  

# Summary of Data Sources
Friends of Casco Bay has been monitoring water quality in Casco Bay for nearly
thirty years.  For many years, their monitoring was conducted by volunteers, at
dozens of locations around the Bay.  In recent years, monitoring has shifted 
more and more towards use of automated sensors and data collected by
professional staff.  

## Continuous Data 
In 2016, FOCB began collecting continuous data from a monitoring
location on Cousins Island.

Their "Cage of Science" monitoring apparatus consists of a YSI data sonde
(currently an "EXO" model), collecting data on primary water quality
parameters (including pH), and C-Sense pCO2 sensor, measuring pCO2.  Both are
housed in a modified lobster trap, which facilitates deployment and retrieval.

FOCB staff visit the site regularly, and swap out sensors and equipment as
needed to ensure a  near-continuous year-round data record.

Additional information on their continuous water quality monitoring program is
available on the FOCB web site.

## Discrete Data
FOCB continues to collect water quality data from many monitoring 
locations around the Bay.  Most data is now collected by professional staff 
roughly every three weeks during the warmer months of the year.  Data consists 
of surface water quality samples at all locations, and vertical profile data on 
selected parameters at a subset of sites approached by boat.

Older volunteer-based data is available from additional locations, but without
recent data at many location, we could use that historical data neither to
depict current conditions, nor evaluate recent trends, so data from sites
no longer monitored was not evaluated here.

Recent changes in monitoring practices complicate data analysis, as data is not 
uniformly available from all sites at all times of year in all years.  The 
result is a need to evaluate and analyze data with sensitivity to possible
bias introduced by differential sampling histories.  We analyzed most data 
using multiple modeling strategies, to evaluate impact of sampling history 
on our qualitative conclusions.
