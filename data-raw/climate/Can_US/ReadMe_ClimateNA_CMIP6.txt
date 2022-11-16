Coordinate Systems
------------------

X,Y: 
Projection: Lambert_Azimuthal_Equal_Area
False_Easting: 0.0
False_Northing: 0.0
Central_Meridian: -100.0
Latitude_Of_Origin: 45.0
Linear Unit: Meter 
Geographic Coordinate System: GCS_WGS_1984
Datum: D_WGS_1984

ELEV:
Elevation in meters above sea level Source: GMTED2010 data at 15 arc-second resolution. Danielson, J.J., and Gesch, D.B., 2011, Global multi-resolution terrain elevation data 2010 (GMTED2010): USGS Open-File Report 2011–1073. https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-multi-resolution-terrain-elevation

Climate Variables (see also http://climatena.ca and https://bit.ly/2RU6kOH (pdf manual) for more information)
-----------------

Monthly variables:
Tmin: minimum temperature for a given month (°C)
Tmax: maximum temperature for a given month (°C)
Tave: mean temperature for a given month (°C)
Ppt:  total precipitation for a given month (mm)

Bioclimatic variables:
MAT:  mean annual temperature (°C)
MWMT: mean temperature of the warmest month (°C)
MCMT: mean temperature of the coldest month (°C)
TD:   difference between MCMT and MWMT, as a measure of continentality (°C)
MAP:  mean annual precipitation (mm)
MSP:  mean summer (May to Sep) precipitation (mm)
AHM:  annual heat moisture index, calculated as (MAT+10)/(MAP/1000)
SHM:  summer heat moisture index, calculated as MWMT/(MSP/1000)
DD_0: degree-days below 0°C (chilling degree days)
DD5: degree-days above 5°C (growing degree days)
DD_18: degree-days below 18°C
DD18: degree-days above 18°C
NFFD: the number of frost-free days
FFP: frost-free period
bFFP: the julian date on which the frost-free period begins
eFFP: the julian date on which the frost-free period ends
PAS:  precipitation as snow (mm)
EMT:  extreme minimum temperature over 30 years
EXT: extreme maximum temperature over 30 years
Eref: Hargreave's reference evaporation
CMD:  Hargreave's climatic moisture index
MAR: mean annual solar radiation (MJ m-2 d-1) (excludes areas south of US and some high-latitude areas)
RH: mean annual relative humidity (%)
CMI: Hogg’s climate moisture index (mm)
DD1040: (10<DD<40) degree-days above 10°C and below 40°C
Tave_wt: winter (December to February) mean temperature (°C)
Tave_sp: spring (March to May) mean temperature (°C)
Tave_sm: summer (June to August) mean temperature (°C)
Tave_at: autumn (September to November) mean temperature (°C)
PPT_wt:  winter (December to February) precipitation (mm)
PPT_sp:  spring (March to May) precipitation (mm)
PPT_sm:  summer (June to August) precipitation (mm)
PPT_at:  autumn (September to November) precipitation (mm)

Known issues:
1) Some discontinuity in precipitation values occurs along the US/Canada border due to edge-matching issues between the PRISM data for the two nations.
2) Mean annual solar radiation (MAR) data are provisional and are slated to be revised in an upcoming release of the ClimateNA software.

Data citation:

AdaptWest Project. 2021. Gridded current and future climate data for North America at 1km resolution, interpolated using the ClimateNA v7.01 software (T. Wang et al., 2021). Available at adaptwest.databasin.org.

Reference:

Wang, T., A. Hamann, D. Spittlehouse, C. Carroll. 2016. Locally Downscaled and Spatially Customizable Climate Data for Historical and Future Periods for North America. PLoS One 11(6): e0156720.


