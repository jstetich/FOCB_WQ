GIS Analysis
 
#  Source Data
## Monitoring Locations
The shapefile 'monitoring_locations' was derived from the Excel spreadsheet 
"FOCB Monitoring Sites.xlsx" in the Original_Data folder.  This file does not 
include geographic information for the "CMS3" station, which FOCB began using 
in 2020.  We added the 'Short_Name' attribute by hand in ArcGIS. It mirrors 
data in the Excel File "FOCB Monitoring Sites SHORT NAMES.xlsx" in the 
Derived_Data folder.

#  Near Impervious Cover Estimates
Impervious cover estimates (calculated only for station locations) were
based on Maine IF&W one meter pixel impervious cover data, which is based
largely on data from 2007.  CBEP has a version of this impervious cover data for
the Casco Bay watershed towns in our GIS data archives. Analysis followed the
following steps. 

1. Town by town IC data in a Data Catalog were assembled into a large `tif` 
   file using the "Mosaic Raster Catalog" item from the context menu from the
   ArcGIS table of contents.

2. We created a convex hull enclosing all of the FOCB Station locations
   using the "minimum bounding geometry" tool.  We buffered that polygon by 2000 
   meters.  The resulting polygon omitted several portions of the Bay likely to
   be important for other analyses, so we created an edited version by hand that
   included other portions of the Bay likely to be of interest (chiefly around
   several islands and the lower New Meadows).  While this area was added by 
   hand, we tried to ensure that it included all islands and an area 2000 m 
   landward of likely monitoring locations.  That polygon was saved as the 
   shapefile "cb_poly_buf".

3. We have previously used used "Extract by Mask" to extract a smaller version
of the 2007 impervious cover data ("imperv2007.tif") for just our buffered sample
region.  For some reason, the tool failed, as did "Clip (Data Management)". We
made a tiff copy of the  data using the context menu item
   
   Data -> Export Data
   
   and saved it as 'imperv_copy' inside the personal geodatabase, 'imperve.mdb'.
   
   We were able to clip that copy of the data layer with the "Extract by Mask" 
   tool, and saved the results in teh geodatabase as 'imperv_clp'.

4. We used "Aggregate" to reduce the resolution of the impervious cover raster
   to a 5 meter resolution, summing the total impervious cover within the
   5m x 5 m area, generating a raster with values from zero to 25. This
   speeds later processing, an a negligible reduction in precision.

5. We used "Focal Statistics" to generate rasters that show the cumulative area
   of impervious cover (in meters) within 100 m, 500 m, and 1000 m. 
   The 1000m version was very slow to calculate.

6. We clipped the `cnty24p` data layer, to the mask polygon, merged all 
   polygons to a single multipolygon and added a dummy attribute with a value 
   of one.  We converted that to a raster.  "Polygon to Raster (Conversion)"),
   using a 5 meter pixel, as a TIFF and copied that into the 'imperv.mdb' 
   geodatabase. Every pixel has a value of one, but an area of 25 so we need to 
   account for that later.

7. We used "Focal Statistics" to generate rasters that show the cumulative sum
   (NOT area) of the land cover raster within 100 m, 500 m, and 1000 m.
   (to get true area, we still need to multiply values by 25).  These rasters are 
   too large to include in the GitHub repository, so were omitted.

8. We extracted the values of the three rasters produced in step 5 and three
   rasters produced in step 7 at each Station location. We used  'Extract 
   Multi Values to Points'. (variable names are imperv_[radius] and 
   land[_radius] respectively).  
   
   We replaced any null values (-9999) with zeros, to account for points that
   lie more than the specified distance from land or impervious cover (using the
   field calculator).

9. We calculated (two versions of) percent cover with the Field Calculator.   
   *   We divided the total impervious cover within the specified distance by the 
       area of the circle sampled under step (5) ($\pi \cdot r^2$).  
   *   We divided the total impervious cover within the specified distance by the 
       estimated land area within each circle, for a percent impervious per unit
       land. (Land area is 25 times the extracted value from the raster).  
   *   Variable names are pct_[radius] and pct_l_[radius], respectively for percent
       based on total area and land area.  

10.  Impervious cover data was exported in a text file 
     "focb_monitoring_imperv.csv".

