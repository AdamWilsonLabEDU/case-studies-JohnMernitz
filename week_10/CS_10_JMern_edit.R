library(terra)
library(rasterVis)
library(ggmap)
library(tidyverse)
library(knitr)
library(sf)
# New Packages
library(ncdf4) # to import data from netcdf format


dir.create("data",showWarnings = F) 
##create a folder to hold the data
## 
lulc_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"
## 
## # download them
download.file(lulc_url,destfile="data/MCD12Q1.051_aid0001.nc", mode="wb")
download.file(lst_url,destfile="data/MOD11A2.006_aid0001.nc", mode="wb")

#' 
#' 
#' You should also edit your .gitignore file (in your tasks repository folder) to include `*data*` on one line. This will prevent git from adding these files.  
#' 
#' 
#' ## Load data into R
## ----warning=F, message=F, results="hide"-------------------------------------
lulc=rast("data/MCD12Q1.051_aid0001.nc",subds="Land_Cover_Type_1")
lst=rast("data/MOD11A2.006_aid0001.nc",subds="LST_Day_1km")


plot(lulc)

#'  
#' 
#' We'll just pick one year to work with to keep this simple:
## ----warning=F----------------------------------------------------------------
lulc=lulc[[13]]
plot(lulc)

#' 
#' ### Process landcover data
#' 
#' Assign land cover clases from [MODIS website](https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mcd12q1)
#' 
## -----------------------------------------------------------------------------
  Land_Cover_Type_1 = c(
    Water = 0, 
    `Evergreen Needleleaf forest` = 1, 
    `Evergreen Broadleaf forest` = 2,
    `Deciduous Needleleaf forest` = 3, 
    `Deciduous Broadleaf forest` = 4,
    `Mixed forest` = 5, 
    `Closed shrublands` = 6,
    `Open shrublands` = 7,
    `Woody savannas` = 8, 
    Savannas = 9,
    Grasslands = 10,
    `Permanent wetlands` = 11, 
    Croplands = 12,
    `Urban & built-up` = 13,
    `Cropland/Natural vegetation mosaic` = 14, 
    `Snow & ice` = 15,
    `Barren/Sparsely vegetated` = 16, 
    Unclassified = 254,
    NoDataFill = 255)

lcd=data.frame(
  ID=Land_Cover_Type_1,
  landcover=names(Land_Cover_Type_1),
  col=c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", 
        "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", 
        "#808080", "#000000", "#000000"),
  stringsAsFactors = F)
# colors from https://lpdaac.usgs.gov/about/news_archive/modisterra_land_cover_types_yearly_l3_global_005deg_cmg_mod12c1
kable(head(lcd))

#' 
#' Convert LULC raster into a 'factor' (categorical) raster.  This requires building the Raster Attribute Table (RAT).  Unfortunately, this is a bit of manual process as follows.
## -----------------------------------------------------------------------------
# convert to raster (easy)
lulc=as.factor(lulc)

# update the RAT with a left join
levels(lulc)=left_join(levels(lulc)[[1]],lcd)[-1,]
activeCat(lulc)=1

gplot(lulc)+
  geom_raster(aes(fill=as.factor(value)))+
  scale_fill_manual(values=setNames(lcd$col,lcd$ID),
                    labels=lcd$landcover,
                    breaks=lcd$ID,
                    name="Landcover Type")+
  coord_equal()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1,byrow=TRUE))

#' 

#' 
#' # Land Surface Temperature
plot(lst[[1:12]])

#' ## Convert LST to Degrees C 
#' You can convert LST from Degrees Kelvin (K) to Celcius (C) with `scoff()`.
## -----------------------------------------------------------------------------
scoff(lst)=cbind(0.02,-273.15)
plot(lst[[1:10]])

#' <div class="well">
#' 
#' # MODLAND Quality control
#' 
#' See a detailed explaination [here](https://lpdaac.usgs.gov/sites/default/files/public/modis/docs/MODIS_LP_QA_Tutorial-1b.pdf).  Some code below from [Steven Mosher's blog](https://stevemosher.wordpress.com/2012/12/05/modis-qc-bits/).
#' 
#' Expand this to learn more about MODIS quality control.  This is optional for this class, but important if you want to work with this kind of data 'for real'.
#' 
#' [MOD11A2 QC Layer table](https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod11a2)
#' 

#' 
#' ### LST QC data
#' 
#' QC data are encoded in 8-bit 'words' to compress information.
#' 

#' 

#' #### MODIS QC data are _Big Endian_
#' 
#' Format          Digits              value     sum
#' ----            ----                ----      ----
#' Little Endian   1 0 0 0 0 0 1 0     65        2^0 + 2^6
#' Big Endian      0 1 0 0 0 0 0 1     65        2^6 + 2^0
#' 
#' 
#' Reverse the digits with `rev()` and compare with QC table above.
#' 

#' QC for value `65`:
#' 
#' * LST produced, other quality, recommend examination of more detailed QA
#' * good data quality of L1B in 7 TIR bands
#' * average emissivity error <= 0.01
#' * Average LST error <= 2K
#' 
#' ### Filter the the lst data using the QC data
#' 

#' 
#' ### Select which QC Levels to keep

#' 
#' ### How many observations will be dropped?
#' 

#' 
#' Do you want to update the values you are keeping?
#' 
#' ### Filter the LST Data keeping only `keepvals`
#' 
#' These steps take a couple minutes.  
#' 
#' Make logical flag to use for mask

#' 
#' Plot the mask

#' 
#' 
#' Mask the lst data using the QC data and overwrite the original data.

#' 
#' </div>
#' </div>
#' 
#' 
#' ## Part 1: Extract timeseries for a point
#' 
#' Extract LST values for a single point and plot them.
#' 
#' 
#' 
lw= data.frame(x= -78.791547,y=43.007211) %>% st_as_sf(coords=c("x","y"),crs=4326)
#defined new point
#' 2. Transform the point to the projection of the raster using 
st_transform(lw, st_crs(lst))  
#' You'll need to get the projection of the raster with 
#'st_crs(lst)
#' 3. Extract the LST data for that location with: `
env_points <- terra::extract(
  lst,
  lw,
  buffer = 1000,
  fun = 'mean',
  na.rm = TRUE)

tlst <- time(lst)

tlst_df <- data.frame(time = tlst)

bound_points <- bind_cols(tlst_df, lst = t(env_points[-1]))

ggplot(bound_points, aes(x = time, y = lst)) +
  geom_point() +
  geom_smooth(span = 0.05)


#'  You may want to transpose them with `t()` to convert it from a wide matrix to long vector. You may also need to drop the first column with `[-1]` becauase the first column is the ID.
#' 4. Extract the dates for each layer with `time(lst)`
#' 5. Combine the dates and transposed raster values into a data.frame.  You could use `data.frame()`, `cbind.data.frame()` or `bind_cols()` to do this. The goal is to make a single dataframe with the dates and lst values in columns.
#' 5. Plot it with `ggplot()` including points for the raw data and a smooth version as a line.  You will probably want to adjust both `span` and `n` in `geom_smooth`.
#'

#' 
#' 
#' # Part 2: Summarize weekly data to monthly climatologies
#' 
#' Now we will use a function called `stackApply()` to calculate monthly mean land surface temperature.
#' 
#' Hints:
#' 
#' 1. Use `tapp()` to summarize the mean value per month (using `index='month'`) and save the results as `lst_month`.

lst_month <- tapp(lst, index = 'month', fun = mean, na.rm = TRUE) 

#' 2. Set the names of the layers to months with `

names(lst_month)= month.name[as.numeric(str_replace(names(lst_month),"m_",""))]

#' This will convert `m_01` to `January`, etc.
#' 3. Plot the map for each month with `gplot()` in the RasterVis Package.

gplot(lst_month) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = 'blue', high = 'red') +
  coord_equal()
#' 4. Calculate the monthly mean for the entire image with `
global(lst_month,mean,na.rm=TRUE)


#' ## Part 3: Summarize Land Surface Temperature by Land Cover
#' 
#' Make a plot and table to contrast Land Surface Temperature in _Urban & built-up_ and _Deciduous Broadleaf forest_ areas. 
#' 
#' 
#' 1. Resample `lulc` to `lst` grid using `resample()` with `method=near` to create a new object called lulc2.

lulc2 <- resample(lulc, lst, method = "near")

#' 2. Extract the values from `lst_month` and `lulc2` into a data.frame as follows:
#'    ``` 
lcds1=cbind.data.frame(
    values(lst_month),
    ID=values(lulc2[[1]]))%>%
    na.omit()
#'    ```
#' 3. Gather the data into a 'tidy' format 
##using either pipe in or save as object and give that obj
##Land_Cover_Type_1_13 <- Land_Cover_Type_1[14]

lcds2 <- lcds1 %>% gather(key='month',value='value',-Land_Cover_Type_1_13)

#' 4. Use `mutate()` to convert ID to numeric (e.g. `ID=as.numeric(Land_Cover_Type_1_13)` and month to an _ordered_ factor with `month=factor(month,levels=month.name,ordered=T)`.
#' 
lcds3 <- lcds2 %>% mutate(ID = as.numeric(Land_Cover_Type_1_13), month = factor(month, levels=month.name, ordered = T))
                 
#' 
#' 5. do a left join with the `lcd` table you created at the beginning.
lcds3 <- lcds3 %>%
  left_join(lcd, by = "ID")

#head(lcds1_long)
#' 6. Use `filter()` to keep only `landcover%in%c("Urban & built-up","Deciduous Broadleaf forest")`
lcds1_filtered <- lcds3 %>%
  filter(landcover %in% c("Urban & built-up", "Deciduous Broadleaf forest"))

#' 7. Develop a ggplot to illustrate the monthly variability in LST between the two land cover types.  The exact form of plot is up to you.  Experiment with different geometries, etc.
#' 

lcds1_filtered %>% 
  filter(landcover%in%c("Urban & built-up","Deciduous Broadleaf forest")) %>% 
  ggplot(aes(y=value,x=month))+
  facet_wrap(~landcover)+
  geom_point(alpha=.5,position="jitter")+
  geom_smooth()+
  geom_violin(alpha=.5,col="red",scale = "width",position="dodge")+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ylab("Monthly Mean Land Surface Temperature (C)")+
  xlab("Month")+
  ggtitle("Land Surface Temperature in Urban and Forest areas in Buffalo, NY")


#' 
#' 
#' 
#' 
#' If you have extra time, try to reproduce the table in this box.
#' 
#' 
#' This is a more complicated table which involves using the `zonal` function to aggregate, followed by `gather`ing, `spread`ing, and creative `paste`ing to combine text fields.
#' 
