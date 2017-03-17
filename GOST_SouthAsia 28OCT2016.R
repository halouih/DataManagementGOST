#install.packages(c("csvread", "csvy", "stringr", "tidyr", "dplyr", "readxl", "car"))
library(stringr)
library(tidyr)
library(dplyr)
library(readxl)
library(csvread)
library(car)
options(stringsAsFactors = FALSE)

# Arbitrary Decisions in Coding -------------------------------------------
#1. When there is no start/end date in the original data, the date of the data is the 'startDate'.
#2. The resolution of the rasters is listed as spatialRes not pixelRes
#3. All "creative commons" keywords are converted into Open data Access.
#4. The output data always drops admin boundaries, because we will use the GAUL boundaries only
#5. Files listed as "Spatial Agent" are "termsofUse = Esri Credits"
#6. Some values are recoded as "?confirm X" because they are not final, need to confirm with Malar
#7. GAFSP data is public (*), but the geospatial is created through ESRI, and is only available for download through ESRI. Therefore listed as Esri Credits.
# (*) this was later edited to make data = Official Use Only (comment date: 10FEB2017)
#8. Search by index or UUID (I changed the serial reference later in the program)jk
#9. The output of this program excludes: a. admin layer shapefiles, b. data without a 'body' description
#10, GISAT Data License is pending: The ESA basemaps are public, but the GISAT analytical products could be private. Refer to response from Christoph Aubrecht.
#11. Land Info Data is Official Use (not restricted) because it is listed as Sharing = Y in the original document from Rafa
#12. India Water Portal is tables only, without a link to ESRI. So it is listed so far as a timeseries, until we find if it has geospatial layer
#13. Record 266 (India: Ministry of Drinking Water and Sanitation) is dropped until we get full information on it from GOST
#14. For all emails that are blank, I added IN THE CSV rjimenezalcaide@worldbank.org as the contact email. I did not do it here because the function kept giving an error.
# Load data ---------------------------------------------------------------


#set working directory
# setwd("C:/Users/wb385939/Documents/Metadata Review/input")
southasia <- read.csv("./input/southasia.csv", header = TRUE, sep = ",")



# Clean data ------------------------------------------------------------
southasia <- select(southasia, -(X:X.14))
colnames(southasia)[names(southasia) == 'Name'] <- "filename"
colnames(southasia)[names(southasia) == 'Descriptive.Title'] <- "title"
colnames(southasia)[names(southasia) == 'Description'] <- "body"
names(southasia)[names(southasia) == 'License'] <- 'license'
names(southasia)[names(southasia) == 'Level'] <- 'granularity'
names(southasia)[names(southasia) == 'Link.URL'] <- 'resource'
names(southasia)[names(southasia) == 'Data.type'] <- 'fileurlDocumentType'
names(southasia)[names(southasia) == 'Known.Caveats.to.use'] <- 'dataNotes'
names(southasia)[names(southasia) == 'Projection'] <- 'mapProject'
names(southasia)[names(southasia) == 'Coverage'] <- 'xxgeographicalCoverage'
names(southasia)[names(southasia) == 'country'] <- 'geographicalCoverage'
names(southasia)[names(southasia) == 'Date.of.publication'] <- 'releaseDate'
names(southasia)[names(southasia) == 'Keywords'] <- 'searchTags'
names(southasia)[names(southasia) == 'Resolution'] <- 'spatialRes'
names(southasia)[names(southasia) == 'Sharable..externally.'] <- 'sharing'
names(southasia)[names(southasia) == 'Author'] <- 'publisher'

southasia <- mutate(southasia, dataType = "geospatial")
southasia <- mutate(southasia, language = "English")
southasia <- mutate(southasia, termsofUse = license)
southasia <- mutate(southasia, copyright = "")
southasia <- mutate(southasia, dataClassification = "")
southasia <- mutate(southasia, apiFormat = "")
southasia <- mutate(southasia, mapview = resource)
southasia <- mutate(southasia, source = "")
colnames(southasia)
#Now all column headers are renamed.




# Recode Data Types -------------------------------------------------------


southasia$fileurlDocumentType <- tolower(southasia$fileurlDocumentType)
southasia$fileurlDocumentType <- str_replace(southasia$fileurlDocumentType, pattern = '.*shapefile.*', replacement = 'vector')
southasia$fileurlDocumentType <- str_replace(southasia$fileurlDocumentType, pattern = 'api', replacement = 'Query tool')
southasia$fileurlDocumentType <- str_replace(southasia$fileurlDocumentType, pattern = '.*xls.*', replacement = 'xls')
southasia$fileurlDocumentType <- str_replace(southasia$fileurlDocumentType, pattern = '.*csv.*', replacement = 'csv')
southasia$fileurlDocumentType <- str_replace(southasia$fileurlDocumentType, pattern = '.*aster.*', replacement = 'raster')
southasia$fileurlDocumentType <- str_replace(southasia$fileurlDocumentType, pattern = '.*network.*', replacement = 'vector')
southasia$fileurlDocumentType <- str_replace(southasia$fileurlDocumentType, pattern = '.*gdb.*', replacement = 'vector')
unique(southasia$fileurlDocumentType)
#Now all document types have been recoded


# Recode Data Granularity -------------------------------------------------


unique(southasia$granularity) #Check the list
southasia$granularity <- str_replace(southasia$granularity, pattern = '.*City.*', replacement = 'City or metropolitan')
southasia$granularity <- str_replace(southasia$granularity, pattern = '.*State.*', replacement = 'Sub-national')
southasia$granularity <- str_replace(southasia$granularity, pattern = '.*District.*', replacement = 'Sub-national')
southasia$granularity <- str_replace(southasia$granularity, pattern = '.*Country.*', replacement = 'National')

unique(southasia$granularity) #This is to Confirm Results
#Now all data granularity is recoded


# Recode termsofUse ----------------------------------------------

#Root Variable is the 'license':
#First: Recode termsofUse FROM license
unique(southasia$license)
unique(southasia$termsofUse)
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*Creative Commons.*', replacement = 'Open Data Access')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*Digital Globe.*', replacement = 'Licensed Data Files')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*digitalglobe.*', replacement = 'Licensed Data Files')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*DigitalGlobe.*', replacement = 'Licensed Data Files')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*incorporated.*', replacement = 'Licensed Data Files')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*get permission.*', replacement = 'Licensed Data Files')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*Licensed Data Files\nproduct in any way derived there from are restricted.  Unauthorized\nuse and/or dissemination is prohibited.*', replacement = 'Licensed Data Files')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*EULA.*', replacement = 'Confidential')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*esri.*', replacement = 'Esri Credits')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*ESRI.*', replacement = 'Esri Credits')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*Esri.*', replacement = 'Esri Credits')
southasia$termsofUse <- str_replace(southasia$termsofUse, pattern = '.*esri.*', replacement = 'Esri Credits')

unique(southasia$license)
unique(southasia$termsofUse)

#Second: Recode termsofUse FROM sharing rules:
#If viewable through spatial agent, therefore termsofUse = Esri Credits
southasia$termsofUse  <- with(southasia, ifelse(X=='Spatial Agent', 'Esri Credits', termsofUse))
southasia$termsofUse  <- with(southasia, ifelse(sharing=='N', 'Restricted Data', termsofUse))

#termsofUse is now all fixed.
unique(southasia$termsofUse)

# Recode dataClassification -----------------------------------------------

unique(southasia$license)
unique(southasia$termsofUse)
unique(southasia$dataClassification)

#Third: Recode dataClassification from termsofUse
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='Open Data Access', 'Public', dataClassification))
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='Direct Access', '?Confirm Terms', dataClassification))
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='Public Use', 'Public', dataClassification))
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='Licensed Data Files', 'Official Use Only', dataClassification))
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='Data from External Repositories', 'Confirm Public, Confidential or Strictly Confidential', dataClassification))
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='No Access', '?Confirm Confidential or Strictly Confidential', dataClassification))
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='Enclave', '?Confirm Confidential or Strictly Confidential', dataClassification))
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='Restricted Data', 'Confidential', dataClassification))
southasia$dataClassification  <- with(southasia, ifelse(termsofUse=='Esri Credits', 'Official Use Only', dataClassification))


unique(southasia$license)
unique(southasia$termsofUse)
unique(southasia$dataClassification)
#Now all termsofUse have been matched to dataClassification and license descriptions

# Date Dates --------------------------------------------------------------

#Calculate start and end dates

southasia <- separate(southasia, col = 'Date.of.data', 
                     into = c('startDate', 'endDate'),
                     sep = '-|,') #Multiple Separators

#Quality Assurance: Test if releaseDate < startDate #HELP:
#convert from character format into date format, use package lubridate



# Extract Operational Datasets --------------------------------------------


southasia <- southasia[!southasia$Sector == "ADMIN", ]

#Extract Dataset: Records that are NOT: A. LINK = services.arcgis (KEEP THIS FOR NOW BECAUSE THEY LOOK LIKE THEY ARE NOT DUPLICATES), and B. Sector = Admin




# Reorder Columns -----------------------------------------------------------------
front_col <- southasia[,c(1,33,3,4,19,34,7,18,27,29,28,6,35,25)]

#all_col <- seq_along(colnames(southasia))
#front_col <- southasia[,c(1,33,3,4,19,34,7,18,27,29,28,6,35,25)]
#back_col <- setdiff(all_col, front_col)
#southasia <- southasia[,c(front_col, back_col)]

#southasia <- cbind(front_col, back_col) #not sure if this is correct


#test <- southasia[,c(1,33,3,4,25,19,34,7,30,18,27,29,28,6,35:ncol(southasia))] #This reorders a subset into a new sheet
#licenses <- southasia[,c(1,3,6,18,19,23,24,27,4)]


#mid output to analyze license
#write.csv(fixlicense, file = "southasiaLICENSES.csv")

unique(southasia$publisher)
unique(southasia$resource)

# Single Row Edits --------------------------------------------------------

southasia$index <- row.names(southasia)

#Fixing License
southasia[southasia$index==286, "license"] = "LAND INFO is a DigitalGlobe Certified Reseller and an AIRBUS DEFENCE & SPACE Image Partner"
southasia[southasia$index%in%317:326, "license"] = "LAND INFO is a DigitalGlobe Certified Reseller and an AIRBUS DEFENCE & SPACE Image Partner"
southasia[southasia$index==286, "fileurlResourceType"] = "Landing page"
southasia[southasia$index%in%317:326, "fileurlResourceType"] = "Landing page"
southasia[southasia$index==286, "fileurlDocumentType"] = "raster"
southasia[southasia$index%in%317:326, "fileurlDocumentType"] = "raster"
southasia[southasia$index==286, "copyright"] = "Digital Globe. For full text please refer to: http://www.digitalglobe.infra.atrust.com/pages/website-terms-of-use"
southasia[southasia$index%in%317:326, "copyright"] = "Digital Globe. For full text please refer to: http://www.digitalglobe.infra.atrust.com/pages/website-terms-of-use"

southasia[southasia$index==63, "license"] = "Open GeoSpatial Consortium (OGC) Standards"
southasia[southasia$index==67, "license"] = "Open GeoSpatial Consortium (OGC) Standards"
southasia[southasia$index==72, "license"] = "Open GeoSpatial Consortium (OGC) Standards"
southasia[southasia$index==82, "license"] = "Open GeoSpatial Consortium (OGC) Standards"
southasia[southasia$index==89, "license"] = "Open GeoSpatial Consortium (OGC) Standards"
  
southasia[southasia$index==63, "dataClassification"] = "Public"
southasia[southasia$index==67, "dataClassification"] = "Public"
southasia[southasia$index==72, "dataClassification"] = "Public"
southasia[southasia$index==82, "dataClassification"] = "Public"
southasia[southasia$index==89, "dataClassification"] = "Public"

southasia[southasia$index==63, "termsofUse"] = "Open Data Access"
southasia[southasia$index==67, "termsofUse"] = "Open Data Access"
southasia[southasia$index==72, "termsofUse"] = "Open Data Access"
southasia[southasia$index==82, "termsofUse"] = "Open Data Access"
southasia[southasia$index==89, "termsofUse"] = "Open Data Access"

southasia[southasia$index==63, "publisher"] = "GeoDASH of the Ministry of Posts, Telecommunications and Information Technology, Government of Bangladesh (GOB)."
southasia[southasia$index==67, "publisher"] = "GeoDASH of the Ministry of Posts, Telecommunications and Information Technology, Government of Bangladesh (GOB)."
southasia[southasia$index==72, "publisher"] = "GeoDASH of the Ministry of Posts, Telecommunications and Information Technology, Government of Bangladesh (GOB)."
southasia[southasia$index==82, "publisher"] = "GeoDASH of the Ministry of Posts, Telecommunications and Information Technology, Government of Bangladesh (GOB)."
southasia[southasia$index==89, "publisher"] = "GeoDASH of the Ministry of Posts, Telecommunications and Information Technology, Government of Bangladesh (GOB)."



southasia[southasia$index==315, "termsofUse"] = "Open Data Access"
southasia[southasia$index==315, "dataClassification"] = "Public"
southasia[southasia$index==315, "license"] = "Open GeoSpatial Consortium (OGC) Standards"
southasia[southasia$index==315, "fileurlResourceType"] = "Query Tool"

#This is geoDash
southasia[southasia$index==63, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index==67, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index==72, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index==82, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index==89, "fileurlResourceType"] = "Bulkdownload Dataset file"

southasia[southasia$index==63, "fileurlDocumentType"] = ""
southasia[southasia$index==67, "fileurlDocumentType"] = "vector"
southasia[southasia$index==72, "fileurlDocumentType"] = ""
southasia[southasia$index==82, "fileurlDocumentType"] = ""
southasia[southasia$index==89, "fileurlDocumentType"] = "vector"

#Fixing Publisher Name
southasia[southasia$index==49, "publisher"] = "UNEP-SWERA Global Environment Facility"
southasia[southasia$index==316, "publisher"] = "UNEP-SWERA Global Environment Facility"
southasia[southasia$index==49, "termsofUse"] = "Open Data Access"
southasia[southasia$index==316, "termsofUse"] = "Open Data Access"
southasia[southasia$index==49, "dataClassification"] = "Public"
southasia[southasia$index==316, "dataClassification"] = "Public"


southasia[southasia$index==449, "publisher"] = "Digital Globe"
southasia[southasia$index==449, "fileurlResourceType"] = "Landing page"
southasia[southasia$index==449, "fileurlDocumentType"] = "vector"
southasia[southasia$index==449, "copyright"] = "Digital Globe. For full text please refer to: http://www.digitalglobe.infra.atrust.com/pages/website-terms-of-use"

southasia[southasia$index==388, "publisher"] = "Technical Univesity of Denmark - Department of Wind Energy (DTU Wind)"
southasia[southasia$index==389, "publisher"] = "Technical Univesity of Denmark - Department of Wind Energy (DTU Wind)"
southasia[southasia$index==388, "termsofUse"] = "Open Data Access"
southasia[southasia$index==389, "termsofUse"] = "Open Data Access"
southasia[southasia$index==388, "dataClassification"] = "Public"
southasia[southasia$index==389, "dataClassification"] = "Public"
southasia[southasia$index==388, "fileurlResourceType"] = "Landing page"
southasia[southasia$index==389, "fileurlResourceType"] = "Landing page"
southasia[southasia$index==388, "fileurlDocumentType"] = "raster"
southasia[southasia$index==389, "fileurlDocumentType"] = "vector"



southasia[southasia$index==266, "publisher"] = "Ministry of Drinking Water and Sanitation, Government of India"
southasia[southasia$index==266, "resource"] = "http://sbm.gov.in/sbm_new/"
southasia[southasia$index==266, "termsofUse"] = "NA"
southasia[southasia$index==266, "dataClassification"] = "NA"
#Drop this 

southasia[southasia$index==275, "license"] = "Available at no charge for any user pursuant to an agreement between METI and NASA. For more information: http://asterweb.jpl.nasa.gov/GDEM.ASP"
southasia[southasia$index==275, "termsofUse"] = "Open Data Access"
southasia[southasia$index==275, "dataClassification"] = "Public"
southasia[southasia$index==275, "fileurlResourceType"] = "Landing Page"

southasia[southasia$index==426, "license"] = "Available at no charge for any user pursuant to an agreement between METI and NASA. For more information: http://asterweb.jpl.nasa.gov/GDEM.ASP"
southasia[southasia$index==426, "termsofUse"] = "Open Data Access"
southasia[southasia$index==426, "dataClassification"] = "Public"
southasia[southasia$index==426, "fileurlResourceType"] = "Landing Page"

#India Biodiversity Portal
southasia[southasia$index==267, "publisher"] = "India Biodiversity Portal"
southasia[southasia$index==267, "termsofUse"] = "Open Data Access"
southasia[southasia$index==267, "dataClassification"] = "Public"
southasia[southasia$index==267, "fileurlResourceType"] = "Query Tool"
southasia[southasia$index==267, "fileurlDocumentType"] = "Other"
southasia[southasia$index==267, "title"] = "India Biodiversity Portal"
southasia[southasia$index==267, "body"] = " India Biodiversity Portal (IBP) is repository of information on biodiversity in India. The portal provides geospatial data on biodiversity by the following themes: Biogeography, Abiotic, Demography, Species, Administrative Units, Land Use Land Cover, Conservation, Threats.As well as for the following geographies: India (national), Uttaranchal, Nilgiri Biosphere Reserve, Papagni, Andhra Pradesh, Western Ghats, BR Hills, Karnataka, Vembanad, Kerala, Satkoshia, Orissa, North East Area, Agar, Madhya Pradesh, Mandla, Madhya Pradesh, Pench, Madhya Pradesh, Bandipur, Karnataka, Kanakapura."
southasia[southasia$index==267, "license"] = "The data is available for subscribers. Particular datasets on the portal may have varying terms of use. Please consult the respective data listing on India Biodiversity Portal."


#BHUVAN
southasia$publisher <- str_replace(southasia$publisher, pattern = '.*BHUVAN.*', replacement = 'BHUVAN, India Geo-Platform of the Indian Space Research Organisation (ISRI)')
southasia$termsofUse  <- with(southasia, ifelse(publisher=='BHUVAN, India Geo-Platform of the Indian Space Research Organisation (ISRI)', 'Open Data Access', termsofUse))
southasia$dataClassification  <- with(southasia, ifelse(publisher=='BHUVAN, India Geo-Platform of the Indian Space Research Organisation (ISRI)', 'Public', dataClassification))
southasia$fileurlResourceType  <- with(southasia, ifelse(publisher=='BHUVAN, India Geo-Platform of the Indian Space Research Organisation (ISRI)', 'Landing page', fileurlResourceType))
southasia$fileurlDocumentType  <- with(southasia, ifelse(publisher=='BHUVAN, India Geo-Platform of the Indian Space Research Organisation (ISRI)', 'Other', fileurlDocumentType))

#riskinfo DMC Sri Lanka
southasia$publisher <- str_replace(southasia$publisher, pattern = '.*riskinfo.*', replacement = 'Disaster Management Center (DMC) of Srilanka, and the Global Facility for Disaster Reduction and Recovery (GFDRR) of the World Bank Group')
southasia[southasia$index%in%345:363, "termsofUse"] = "Open Data Access"
southasia[southasia$index%in%345:363, "dataClassification"] = "Public"
southasia[southasia$index%in%345:363, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index%in%345:363, "fileurlDocumentType"] = "vector"


southasia[southasia$index%in%384:387, "publisher"] = "National Renewable Energy Laboratory (NREL)"
southasia[southasia$index%in%384:387, "termsofUse"] = "Open Data Access"
southasia[southasia$index%in%384:387, "dataClassification"] = "Public"
southasia[southasia$index%in%384:387, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index%in%384:387, "copyright"] = "U.S. Department of Energy (DOE)/NREL/ALLIANCE"
southasia[southasia$index%in%384:387, "license"] = "This GIS data was developed by the National Renewable Energy Laboratory (NREL), which is operated by the Alliance for Sustainable Energy, LLC for the U.S. Department of Energy (DOE). The user is granted the right, without any fee or cost, to use, copy, modify, alter, enhance and distribute this data for any purpose whatsoever, provided that this entire notice appears in all copies of the data. Further, the user of this data agrees to credit NREL in any publications or software that incorporate or use the data. Please refer to the full license text at http://www.nrel.gov/disclaimer.html"




southasia[southasia$index==99, "publisher"] = "National Renewable Energy Laboratory (NREL)"
southasia[southasia$index==99, "termsofUse"] = "Open Data Access"
southasia[southasia$index==99, "dataClassification"] = "Public"
southasia[southasia$index==99, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index==99, "copyright"] = "U.S. Department of Energy (DOE)/NREL/ALLIANCE"
southasia[southasia$index==99, "license"] = "This GIS data was developed by the National Renewable Energy Laboratory (NREL), which is operated by the Alliance for Sustainable Energy, LLC for the U.S. Department of Energy (DOE). The user is granted the right, without any fee or cost, to use, copy, modify, alter, enhance and distribute this data for any purpose whatsoever, provided that this entire notice appears in all copies of the data. Further, the user of this data agrees to credit NREL in any publications or software that incorporate or use the data. Please refer to the full license text at http://www.nrel.gov/disclaimer.html"


#IWMI
southasia$publisher <- str_replace(southasia$publisher, pattern = '.*IWMI.*', replacement = 'International Water Management Institute (IWMI)')
southasia$termsofUse  <- with(southasia, ifelse(publisher=='International Water Management Institute (IWMI)', 'Open Data Access', termsofUse))
southasia$dataClassification  <- with(southasia, ifelse(publisher=='International Water Management Institute (IWMI)', 'Public', dataClassification))
southasia$fileurlResourceType  <- with(southasia, ifelse(publisher=='International Water Management Institute (IWMI)', 'Bulkdownload Dataset file', fileurlResourceType))
southasia$fileurlDocumentType  <- with(southasia, ifelse(publisher=='International Water Management Institute (IWMI)', 'vector',fileurlDocumentType))

#India WRIS
southasia$termsofUse  <- with(southasia, ifelse(publisher=='India WRIS', 'Open Data Access', termsofUse))
southasia$dataClassification  <- with(southasia, ifelse(publisher=='India WRIS', 'Public', dataClassification))
southasia$fileurlResourceType  <- with(southasia, ifelse(publisher=='India WRIS', 'Bulkdownload Dataset file', fileurlResourceType))
southasia$fileurlDocumentType  <- with(southasia, ifelse(publisher=='India WRIS', 'Other', fileurlDocumentType))


southasia[southasia$index==277, "publisher"] = "Global Vegetation Monitoring (GVM) unit of the Joint Research Centre (JRC)"
southasia[southasia$index==277, "resource"] = "http://forobs.jrc.ec.europa.eu/products/glc2000/products.php"
southasia[southasia$index==277, "termsofUse"] = "Open Data Access"
southasia[southasia$index==277, "dataClassification"] = "Public"
southasia[southasia$index==277, "fileurlResourceType"] = "Landing page"
southasia[southasia$index==277, "fileurlDocumentType"] = "raster"





#Fixing resource
southasia[southasia$index==233, "resource"] = "http://www.indiawaterportal.org"
southasia[southasia$index==233, "publisher"] = "India Water Portal"
southasia[southasia$index==233, "termsofUse"] = "Open Data Access"
southasia[southasia$index==233, "dataClassification"] = "Public"
southasia[southasia$index==233, "dataType"] = "timeseries"
#ROW 233 India Portal is tables only, without a link to ESRI. So it is listed so far as a timeseries, until we find if it has geospatial layer

#GAFSP
southasia$resource  <- with(southasia, ifelse(publisher=='http://www.gafspfund.org/gafspmapglobal', 'http://www.gafspfund.org/gafspmapglobal', resource))
southasia$publisher <- str_replace(southasia$publisher, pattern = '.*gafspfund.*', replacement = 'Global Agriculture and Food Security Program (GAFSP) of the World Bank Group')
southasia$publisher <- str_replace(southasia$publisher, pattern = '.*GAFSP.*', replacement = 'Global Agriculture and Food Security Program (GAFSP) of the World Bank Group')
southasia$fileurlResourceType  <- with(southasia, ifelse(resource=='http://www.gafspfund.org/gafspmapglobal', 'Query Tool', fileurlResourceType))
southasia$fileurlDocumentType  <- with(southasia, ifelse(resource=='http://www.gafspfund.org/gafspmapglobal', 'vector', fileurlDocumentType))

#GISAT

southasia[southasia$index%in%29:33, "contactemail"] = "tomas.soukup@gisat.cz"
southasia[southasia$index%in%29:33, "source"] = "Private Source"
southasia[southasia$index%in%29:33, "dataClassification"] = "Official Use Only"
southasia[southasia$index%in%29:33, "termsofUse"] = "Licensed Data Files"

southasia$publisher <- str_replace(southasia$publisher, pattern = '.*GISAT.*', replacement = 'GISAT, European Space Agency (ESA) and The World Bank Group (WBG)')
southasia$resource  <- with(southasia, ifelse(publisher=='GISAT, European Space Agency (ESA) and The World Bank Group (WBG)', 'http://maps.elie.ucl.ac.be/CCI/viewer/index.php', resource))
southasia$termsofUse  <- with(southasia, ifelse(publisher=='GISAT, European Space Agency (ESA) and The World Bank Group (WBG)', 'Official Use Only', termsofUse))
southasia$dataClassification  <- with(southasia, ifelse(publisher=='GISAT, European Space Agency (ESA) and The World Bank Group (WBG)', 'Licensed Data Files', dataClassification))
southasia$fileurlResourceType  <- with(southasia, ifelse(publisher=='GISAT, European Space Agency (ESA) and The World Bank Group (WBG)', 'Query Tool', fileurlResourceType))
southasia$fileurlDocumentType  <- with(southasia, ifelse(publisher=='GISAT, European Space Agency (ESA) and The World Bank Group (WBG)', 'raster', fileurlDocumentType))
southasia$contactemail  <- with(southasia, ifelse(publisher=='GISAT, European Space Agency (ESA) and The World Bank Group (WBG)', 'tomas.soukup@gisat.cz', contactemail))
southasia$license  <- with(southasia, ifelse(publisher=='GISAT, European Space Agency (ESA) and The World Bank Group (WBG)', 'The present products are made available to the public by ESA and the consortium. You may use one or several CCI-LC products land cover map for educational and/or scientific purposes, without any fee on the condition that you credit the ESA Climate Change Initiative and in particular its Land Cover project as the source of the CCI-LC database:

                                             Copyright notice: © ESA Climate Change Initiative - Land Cover project 2014-2017
                                             
                                             Should you write any scientific publication on the results of research activities that use one or several CCI-LC products as input, you shall acknowledge the ESA CCI Land Cover project in the text of the publication and provide the project with an electronic copy of the publication (contact@esa-landcover-cci.org).
                                             
                                             If you wish to use one or several CCI-LC products in advertising or in any commercial promotion, you shall acknowledge the ESA CCI Land Cover project and you must submit the layout to the project for approval beforehand (contact@esa-landcover-cci.org).', license))
southasia$copyright  <- with(southasia, ifelse(publisher=='GISAT, European Space Agency (ESA) and The World Bank Group (WBG)', '© ESA Climate Change Initiative - Land Cover project 2014-2017', copyright))


#ESMAP Files
southasia[southasia$index%in%34:36, "apiFormat"] = "WMS"
southasia[southasia$index%in%34:36, "termsofUse"] = "Open Data Access"
southasia[southasia$index%in%34:36, "dataClassification"] = "Public"
southasia[southasia$index%in%34:36, "source"] = "ESMAP"
southasia[southasia$index%in%34:36, "publisher"] = "Energy Sector Management Assistance Program (ESMAP) of the World Bank Group"
southasia[southasia$index%in%34:36, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index%in%34:36, "fileurlDocumentType"] = "Other"

southasia[southasia$index%in%368:373, "apiFormat"] = "WMS"
southasia[southasia$index%in%368:373, "dataClassification"] = "Public"
southasia[southasia$index%in%368:373, "termsofUse"] = "Open Data Access"
southasia[southasia$index%in%368:373, "source"] = "ESMAP"
southasia[southasia$index%in%368:373, "publisher"] = "Energy Sector Management Assistance Program (ESMAP) of the World Bank Group"
southasia[southasia$index%in%368:373, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index%in%368:373, "fileurlDocumentType"] = "Other"

southasia[southasia$index%in%462:466, "apiFormat"] = "WMS"
southasia[southasia$index%in%462:466, "dataClassification"] = "Public"
southasia[southasia$index%in%462:466, "termsofUse"] = "Open Data Access"
southasia[southasia$index%in%462:466, "source"] = "ESMAP"
southasia[southasia$index%in%462:466, "publisher"] = "Energy Sector Management Assistance Program (ESMAP) of the World Bank Group"
southasia[southasia$index%in%462:466, "fileurlResourceType"] = "Bulkdownload Dataset file"
southasia[southasia$index%in%462:466, "fileurlDocumentType"] = "Other"





southasia[southasia$index%in%56:60, "source"] = "Private Source"

#World Resource Institute: 
southasia[southasia$index%in%183:184, "fileurlResourceType"] = 'Landing Page'



#Fixing mapview
#southasia$mapview <- str_replace(southasia$mapview, pattern = '.*landinfo.*', 'NA')
#southasia[southasia$index%in%183:184, "mapview"] = 'NA'
#southasia[southasia$index%in%384:387, "mapview"] = 'NA'
#southasia[southasia$index%in%194:199, "mapview"] = 'NA'
#southasia[southasia$index%in%200:216, "mapview"] = 'NA'


#southasia$mapview <- str_replace(southasia$mapview, pattern = '.*featureserver.*', 'NA') #Put the "!" somewhere

#southasia[southasia$index==286, "mapview"] = southasia$resource
#southasia[southasia$index%in%317:326, "mapview"] = southasia$resource


# Fix Dates and Description "body" -------------------------------------------------------


southasia[southasia$index==408, "body"] = "Population: Total population is based on the de facto definition of population, which counts all residents regardless of legal status or citizenship, except for refugees not permanently settled in the country of asylum, who are generally considered part of the population of their country of origin. "

southasia[southasia$index%in%34:36, "startDate"] = "2001"
southasia[southasia$index%in%34:36, "endDate"] = "2010"
southasia[southasia$index%in%34:36, "releaseDate"] = "2010"

southasia[southasia$UUID == 1060, "startDate"] = ""
southasia[southasia$UUID == 1060, "endDate"] = "2011"
southasia[southasia$UUID == 1060, "releaseDate"] = "2011"
southasia[southasia$UUID == 1060, "body"] = "Malnutrition (Proportion of underweight children under 5 years) (2011): Prevalence of severely underweight children is the percentage of children under age 5 whose weight for age is more than 3 standard deviations below the median for the international reference population ages 0-59 months.Data Source: Measure DHS. “Bangladesh Demographic and Health Survey 2011. Preliminary Report."

southasia[southasia$UUID == 1061, "startDate"] = "2010"
southasia[southasia$UUID == 1061, "endDate"] = "2011"
southasia[southasia$UUID == 1061, "releaseDate"] = "2011"

southasia[southasia$UUID == 1062, "startDate"] = "1979"
southasia[southasia$UUID == 1062, "endDate"] = "2015"
southasia[southasia$UUID == 1062, "releaseDate"] = ""

southasia[southasia$UUID == 1063, "startDate"] = ""
southasia[southasia$UUID == 1063, "endDate"] = ""
southasia[southasia$UUID == 1063, "releaseDate"] = ""
southasia[southasia$UUID == 1063, "body"] = "This is the WFP node showing flood prone areas."

southasia[southasia$UUID == 1065, "startDate"] = ""
southasia[southasia$UUID == 1065, "endDate"] = "2011"
southasia[southasia$UUID == 1065, "releaseDate"] = "2011"
southasia[southasia$UUID == 1065, "body"] = "Total Population (2011): Total population is based on the de facto definition of population, which counts all residents regardless of legal status or citizenship, except for refugees not permanently settled in the country of asylum, who are generally considered part of the population of their country of origin. Data Source: Bangladesh Bureau of Statistics. “Population and Housing Census 2011. Preliminary Results."

southasia[southasia$UUID == 1066, "startDate"] = ""
southasia[southasia$UUID == 1066, "endDate"] = "2005"
southasia[southasia$UUID == 1066, "releaseDate"] = "2005"

southasia[southasia$UUID == 1067, "startDate"] = ""
southasia[southasia$UUID == 1067, "endDate"] = ""
southasia[southasia$UUID == 1067, "releaseDate"] = ""

southasia[southasia$UUID == 1068, "startDate"] = "2010"
southasia[southasia$UUID == 1068, "endDate"] = "2011"
southasia[southasia$UUID == 1068, "releaseDate"] = "2011"

southasia[southasia$UUID == 1069, "startDate"] = "2010"
southasia[southasia$UUID == 1069, "endDate"] = "2011"
southasia[southasia$UUID == 1069, "releaseDate"] = "2011"

southasia[southasia$UUID == 1074, "startDate"] = "2009"
southasia[southasia$UUID == 1074, "endDate"] = "2010"
southasia[southasia$UUID == 1074, "releaseDate"] = "2010"
southasia[southasia$UUID == 1074, "body"] = "Irrigated Area: Total irrigated area in hectares."



southasia[southasia$UUID == 1075, "startDate"] = "2009"
southasia[southasia$UUID == 1075, "endDate"] = "2010"
southasia[southasia$UUID == 1075, "releaseDate"] = "2010"

southasia[southasia$UUID == 1079, "startDate"] = ""
southasia[southasia$UUID == 1079, "endDate"] = "2011"
southasia[southasia$UUID == 1079, "releaseDate"] = "2011"
southasia[southasia$UUID == 1079, "body"] = "Irrigated Area: Total irrigated area in hectares. Data Source: Bangladesh Bureau of Statistics. 2010 Yearbook of Agricultural Statistics of Bangladesh."


southasia[southasia$UUID == 1080, "startDate"] = "2010"
southasia[southasia$UUID == 1080, "endDate"] = "2011"
southasia[southasia$UUID == 1080, "releaseDate"] = "2011"

southasia[southasia$UUID == 1081, "startDate"] = ""
southasia[southasia$UUID == 1081, "endDate"] = "2001"
southasia[southasia$UUID == 1081, "releaseDate"] = "2001"

southasia[southasia$UUID == 1082, "startDate"] = "2010"
southasia[southasia$UUID == 1082, "endDate"] = "2011"
southasia[southasia$UUID == 1082, "releaseDate"] = "2011"

southasia[southasia$UUID == 1084, "startDate"] = ""
southasia[southasia$UUID == 1084, "endDate"] = "2010"
southasia[southasia$UUID == 1084, "releaseDate"] = "2010"
southasia[southasia$UUID == 1084, "body"] = "Poverty Incidence (Proportion of population below the poverty line) (2010): Proportion of the population living on less than US$1.25 a day, measured at 2005 international prices, adjusted for purchasing power parity (PPP).
"

southasia[southasia$UUID == 1085, "startDate"] = ""
southasia[southasia$UUID == 1085, "endDate"] = "2007"
southasia[southasia$UUID == 1085, "releaseDate"] = "2007"

southasia[southasia$UUID == 1086, "startDate"] = ""
southasia[southasia$UUID == 1086, "endDate"] = "2011"
southasia[southasia$UUID == 1086, "releaseDate"] = "2011"
southasia[southasia$UUID == 1086, "body"] = "Population Density (2011): Population divided by land area in square kilometers. Data Source: Bangladesh Bureau of Statistics. “Population and Housing Census 2011. Preliminary Results."



southasia[southasia$UUID == 1087, "startDate"] = "2009"
southasia[southasia$UUID == 1087, "endDate"] = "2010"
southasia[southasia$UUID == 1087, "releaseDate"] = "2010"

southasia[southasia$UUID == 1090, "startDate"] = "2010"
southasia[southasia$UUID == 1090, "endDate"] = "2011"
southasia[southasia$UUID == 1090, "releaseDate"] = "2011"

southasia[southasia$UUID == 1092, "startDate"] = "2010"
southasia[southasia$UUID == 1092, "endDate"] = "2011"
southasia[southasia$UUID == 1092, "releaseDate"] = "2011"

southasia[southasia$UUID == 1099, "startDate"] = ""
southasia[southasia$UUID == 1099, "endDate"] = "2005"
southasia[southasia$UUID == 1099, "releaseDate"] = "2005"
southasia[southasia$UUID == 1099, "body"] = "Bhutan Population (2005) is based on individual enumeration - i.e. Every person in the country is to be enumerated and recorded separately
irrespective of nationality. Data Source: National Statistics Bureau - Buthan 2005 Population and Housing Census http://www.nsb.gov.bt/publication/files/pub7ps7846bs.pdf"

southasia[southasia$UUID == 1100, "startDate"] = ""
southasia[southasia$UUID == 1100, "endDate"] = "2012"
southasia[southasia$UUID == 1100, "releaseDate"] = "2012"

southasia[southasia$UUID == 1101, "startDate"] = ""
southasia[southasia$UUID == 1101, "endDate"] = "2010"
southasia[southasia$UUID == 1101, "releaseDate"] = "2010"

southasia[southasia$UUID == 1102, "startDate"] = ""
southasia[southasia$UUID == 1102, "endDate"] = "2005"
southasia[southasia$UUID == 1102, "releaseDate"] = "2005"

southasia[southasia$UUID == 1210, "startDate"] = ""
southasia[southasia$UUID == 1210, "endDate"] = "2001"
southasia[southasia$UUID == 1210, "releaseDate"] = "2001"

southasia[southasia$UUID == 1276, "startDate"] = ""
southasia[southasia$UUID == 1276, "endDate"] = "2000"
southasia[southasia$UUID == 1276, "releaseDate"] = "2000"


southasia[southasia$UUID == 1367, "startDate"] = "1999"
southasia[southasia$UUID == 1367, "endDate"] = "2013"
southasia[southasia$UUID == 1367, "releaseDate"] = "2013"

southasia[southasia$UUID == 1368, "startDate"] = "2005"
southasia[southasia$UUID == 1368, "endDate"] = "2015"
southasia[southasia$UUID == 1368, "releaseDate"] = "2015"

southasia[southasia$UUID == 1369, "startDate"] = "2005"
southasia[southasia$UUID == 1369, "endDate"] = "2015"
southasia[southasia$UUID == 1369, "releaseDate"] = "2015"

southasia[southasia$UUID == 1370, "startDate"] = "2005"
southasia[southasia$UUID == 1370, "endDate"] = "2015"
southasia[southasia$UUID == 1370, "releaseDate"] = "2015"

southasia[southasia$UUID == 1371, "startDate"] = "2005"
southasia[southasia$UUID == 1371, "endDate"] = "2015"
southasia[southasia$UUID == 1371, "releaseDate"] = "2015"


southasia[southasia$UUID == 1372, "startDate"] = "1999"
southasia[southasia$UUID == 1372, "endDate"] = "2013"
southasia[southasia$UUID == 1372, "releaseDate"] = "2013"


southasia[southasia$UUID == 1396, "startDate"] = ""
southasia[southasia$UUID == 1396, "endDate"] = "2007"
southasia[southasia$UUID == 1396, "releaseDate"] = "2007"

southasia[southasia$UUID == 1399, "startDate"] = "2012"
southasia[southasia$UUID == 1399, "endDate"] = "2013"
southasia[southasia$UUID == 1399, "releaseDate"] = "2013"

southasia[southasia$UUID == 1407, "startDate"] = ""
southasia[southasia$UUID == 1407, "endDate"] = "2010"
southasia[southasia$UUID == 1407, "releaseDate"] = "2010"

southasia[southasia$UUID == 1417, "startDate"] = "2012"
southasia[southasia$UUID == 1417, "endDate"] = "2013"
southasia[southasia$UUID == 1417, "releaseDate"] = "2013"


# Filter Output: Data with External Links -----------------------------------------------------------


#Scenario A: This sequence was when GISAT resources were blank but we still needed them
#df1 <- southasia[!(southasia$resource == ""), ]
#df2 <- southasia[(southasia$publisher == "GISAT, European Space Agency (ESA) and The World Bank Group (WBG)"), ]
#output <- rbind(df1, df2) 

#Scenario B: When GISAT have the resources filled, we only need to drop the blank resources:
output <- southasia[!(southasia$resource == ""), ]


# Name Convention ---------------------------------------------------------

list(output$title)
edits <- read.csv("./input/fixnames.csv", header = TRUE, sep = ",")
names(edits)[names(edits) == 'title'] <- 'dummy'


#vlookup UUID to match name
southasia <- left_join(southasia, edits, by = 'UUID')

#Replace values of 'title' if dummy is NOT blank
southasia$title[!is.na(southasia$dummy)] <- southasia$dummy[!is.na(southasia$dummy)]
#southasia$title[southasia$dummy == 'test'] <- southasia$dummy[!is.na(southasia$dummy)] #use this when you want to replace values of column with a string if another column is blank


#Correct Emails:
southasia$contactemail  <- with(southasia, ifelse(contactemail=='NA', 'rjimenezalcaide@worldbank.org', contactemail))


# Dropped Records ---------------------------------------------------------
#Delete the following records because they don't have good "body" description
southasia <- southasia[ ! southasia$UUID %in% c("1061", "1069", "1071", "1072", "1075", "1077", "1080", "1089", "1232", "1265","1344" ), ]

#Rewrite output to include corrected titles
output <- southasia[!(southasia$resource == ""), ]

output <- select(output, -c(filename, Sector, index, Multiple.Layers, xxgeographicalCoverage, mapview, API..Logon.Credential, Path.Location, sharing, X, dummy)) 

colnames(output)
unique(output$termsofUse)
unique(output$dataClassification)
unique(output$license)
unique(output$copyright)
unique(output$resource)
unique(output$publisher)

#Now output is ready with the correct column names


#Split Data Frame into Resources and Datasets
colnames(southasia)
#resources <- southasia[,c("UUID", "title", "resource", "fileurlResourceType", "fileurlDocumentType", "dataClassification")]
#datasets <- southasia
#write.csv(datasets, file = "geodata_datasets_10112016.csv")
#write.csv(resources, file = "geodata_resources_10112016.csv")


write.csv(output, file = "southasiaOUTPUT1.csv")

#write.csv(southasia, file = "southasiaOUTPUT.csv")


