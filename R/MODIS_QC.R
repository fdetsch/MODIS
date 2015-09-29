# This file contains part of the information in the QC layers since it is a relatively boring job any contribution is welcome!
# the required information can be found under https://lpdaac.usgs.gov/products/modis_products_table -> Layers
# Status: Experimental!

########### M.D09

MOD09A1_QC <- data.frame(
        LongName=c("MODLAND_QA", "band 1 data quality four bit range", "band 2 data quality four bit range", "band 3 data quality four bit range", "band 4 data quality four bit range", "band 5 data quality four bit range", "band 6 data quality four bit range", "band 7 data quality four bit range", "atmospheric correction performed", "adjacency correction performed"), 
        bitShift=c(0,2,6,10,14,18,22,26,30,31),
        bitMask=c(3,15,15,15,15,15,15,15,1,1)
    )
    
MYD09A1_QC <- data.frame(
        LongName=c("MODLAND_QA", "band 1 data quality four bit range", "band 2 data quality four bit range", "band 3 data quality four bit range", "band 4 data quality four bit range", "band 5 data quality four bit range", "band 6 data quality four bit range", "band 7 data quality four bit range", "atmospheric correction performed", "adjacency correction performed"), 
        bitShift=c(0,2,6,10,14,18,22,26,30,31),
        bitMask=c(3,15,15,15,15,15,15,15,1,1)
    )

MOD09GQ_QC <- data.frame(
        LongName=c("MODLAND_QA", "cloud state", "band 1 data quality four bit range", "band 2 data quality four bit range",
        "atmospheric correction performed", "adjacency correction performed", "spare (unused)"), 
        bitShift=c(0,2,4,8,12,13,14),
        bitMask=c(3,3,15,15,1,1,3)
    )

MYD09GQ_QC <- data.frame(
        LongName=c("MODLAND_QA", "cloud state", "band 1 data quality four bit range", "band 2 data quality four bit range",
        "atmospheric correction performed", "adjacency correction performed", "spare (unused)"), 
        bitShift=c(0,2,4,8,12,13,14),
        bitMask=c(3,3,15,15,1,1,3)
    )

MOD09GA_QC <- data.frame(
        LongName=c("cloud state", "cloud shadow", "land/water flag", "aerosol quantity", "cirrus detected",
        "internal cloud algorithm flag", "internal fire algorithm flag", "MOD35 snow/ice flag",
        "Pixel is adjacent to cloud", "BRDF correction performed", "internal snow mask"), 
        bitShift=c(0,2,3,6,8,10,11,12,13,14,15),
        bitMask=c(3,1,7,3,3,1,1,1,1,1,1)
    )

MYD09GA_QC <- data.frame(
        LongName=c("cloud state", "cloud shadow", "land/water flag", "aerosol quantity", "cirrus detected",
        "internal cloud algorithm flag", "internal fire algorithm flag", "MOD35 snow/ice flag",
        "Pixel is adjacent to cloud", "BRDF correction performed", "internal snow mask"), 
        bitShift=c(0,2,3,6,8,10,11,12,13,14,15),
        bitMask=c(3,1,7,3,3,1,1,1,1,1,1)
    )

MOD09Q1_QC <- data.frame(
        LongName=c("MODLAND_QA", "cloud state", "band 1 data quality four bit range", "band 2 data quality four bit range",
        "atmospheric correction performed", "adjacency correction performed", "different orbit from 500 m", "spare (unused)"), 
        bitShift=c(0,2,4,8,12,13,14,15),
        bitMask=c(3,3,15,15,1,1,1,1)
    )

MYD09Q1_QC <- data.frame(
        LongName=c("MODLAND_QA", "cloud state", "band 1 data quality four bit range", "band 2 data quality four bit range",
        "atmospheric correction performed", "adjacency correction performed", "different orbit from 500 m", "spare (unused)"), 
        bitShift=c(0,2,4,8,12,13,14,15),
        bitMask=c(3,3,15,15,1,1,1,1)
    )

########### M.D11


MOD11A1_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MYD11A1_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MOD11A2_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MYD11A2_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MOD11B1_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MYD11B1_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MOD11C1_QC <- data.frame(
        LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
        bitShift=c(0,2,4,6),
        bitMask=c(3,3,3,3)
    )

MYD11C1_QC <- data.frame(
        LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
        bitShift=c(0,2,4,6),
        bitMask=c(3,3,3,3)
    )

MOD11C2_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MYD11C2_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MOD11C3_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

MYD11C3_QC <- data.frame(
  LongName=c("Mandatory QA flag", "Data quality flag", "Emissivity error flag", "LST error flag"), 
  bitShift=c(0,2,4,6),
  bitMask=c(3,3,3,3)
)

############ MCD12

MCD12C1_QC <- data.frame(
  LongName=c("Mandatory QA", "Quarters since updated", "Land/Water"), 
  bitShift=c(0,2,4),
  bitMask=c(3,3,15)
)

############ M.D13

MOD13Q1_QC <- data.frame(
        LongName=c("MODLAND_QA", "VI usefulness", "Aerosol quantity", "Adjacent cloud detected",
        "Atmosphere BRDF correction performed", "Mixed Clouds", "Land/Water Flag", "Possible snow/ice", "Possible shadow"), 
        bitShift=c(0,2,6,8,9,10,11,14,15),
        bitMask=c(3,15,3,1,1,1,7,1,1)
    )

MYD13Q1_QC <- data.frame(
        LongName=c("MODLAND_QA", "VI usefulness", "Aerosol quantity", "Adjacent cloud detected",
        "Atmosphere BRDF correction performed", "Mixed Clouds", "Land/Water Flag", "Possible snow/ice", "Possible shadow"), 
        bitShift=c(0,2,6,8,9,10,11,14,15),
        bitMask=c(3,15,3,1,1,1,7,1,1)
    )

MYD13C1_QC <- data.frame(
        LongName=c("MODLAND_QA", "VI usefulness", "Aerosol quantity", "Adjacent cloud detected",
        "Atmosphere BRDF correction performed", "Mixed Clouds", "Land/Water Flag", "Geospatial quality"), 
        bitShift=c(0,2,6,8,9,10,11,14),
        bitMask=c(3,15,3,1,1,1,7,3)
    )

MOD13C1_QC <- data.frame(
        LongName=c("MODLAND_QA", "VI usefulness", "Aerosol quantity", "Adjacent cloud detected",
        "Atmosphere BRDF correction performed", "Mixed Clouds", "Land/Water Flag", "Geospatial quality"), 
        bitShift=c(0,2,6,8,9,10,11,14),
        bitMask=c(3,15,3,1,1,1,7,3)
    )

MYD13A2_QC <- data.frame(
        LongName=c("MODLAND_QA", "VI usefulness", "Aerosol quantity", "Adjacent cloud detected",
        "Atmosphere BRDF correction performed", "Mixed Clouds", "Land/Water Flag", "Possible snow/ice", "Possible shadow"), 
        bitShift=c(0,2,6,8,9,10,11,14,15),
        bitMask=c(3,15,3,1,1,1,7,1,1)
    )

MOD13A2_QC <- data.frame(
        LongName=c("MODLAND_QA", "VI usefulness", "Aerosol quantity", "Adjacent cloud detected",
        "Atmosphere BRDF correction performed", "Mixed Clouds", "Land/Water Flag", "Possible snow/ice", "Possible shadow"), 
        bitShift=c(0,2,6,8,9,10,11,14,15),
        bitMask=c(3,15,3,1,1,1,7,1,1)
    )


MYD13A3_QC <- data.frame(
        LongName=c("MODLAND_QA", "VI usefulness", "Aerosol quantity", "Adjacent cloud detected",
        "Atmosphere BRDF correction performed", "Mixed Clouds", "Land/Water Flag", "Possible snow/ice", "Possible shadow"), 
        bitShift=c(0,2,6,8,9,10,11,14,15),
        bitMask=c(3,15,3,1,1,1,7,1,1)
    )

MOD13A3_QC <- data.frame(
        LongName=c("MODLAND_QA", "VI usefulness", "Aerosol quantity", "Adjacent cloud detected",
        "Atmosphere BRDF correction performed", "Mixed Clouds", "Land/Water Flag", "Possible snow/ice", "Possible shadow"), 
        bitShift=c(0,2,6,8,9,10,11,14,15),
        bitMask=c(3,15,3,1,1,1,7,1,1)
    )

#### MCD15

MCD15A2_QC <- data.frame(
  LongName=c("MODLAND_QC bits", "Sensor", "DeadDetector", "CloudState (inherited from Aggregate_QC bits {0,1} cloud state)",
             "SCF_QC (five level confidence score)"), 
  bitShift=c(0,1,2,3,5),
  bitMask=c(1,1,1,3,7)
  )

#### M.D17

MOD17A2_QC <- data.frame(
  LongName=c("MODLAND_QC bits", "Sensor", "DeadDetector", "CloudState (inherited from Aggregate_QC bits {0,1} cloud state)",
             "SCF_QC (five level confidence score)"),
  bitShift=c(0,1,2,3,5),
  bitMask=c(1,1,1,3,7)
  )

MYD17A2_QC <- data.frame(
    LongName=c("MODLAND_QC bits", "Sensor", "DeadDetector", "CloudState (inherited from Aggregate_QC bits {0,1} cloud state)",
               "SCF_QC (five level confidence score)"),
    bitShift=c(0,1,2,3,5),
    bitMask=c(1,1,1,3,7)
    )
