###	README ###

Included files:

pointID_spatialData_towerPlots.csv - Spatial coordinates for pointIDs that are used as a reference location when mapping apparent individuals.

vst_apparentIndiv2015_goughSites.csv - Table linking tagIDs with structural data measured by NEON technicians in the field. 

vst_mappingTagging2015_goughSites.csv - Table linking tagIDs of mapped individuals with taxonID, stemDistance (relative to a pointID), and stemAzimuth (relative to a pointID). Not all individuals that are measured in the 'apparentIndividual' table are mapped. See the protocol for mapping guidelines.

NEON.DOC.000987vG_vstProtocol.docx - The NEON Vegetation Structure protocol, describing how data in each field are collected.

DPS_TermList_20170120.csv - The full list of terms used in all of NEON's data products. Definitions for terms used in the two data files listed above can be found here.

Note: There is a known issue with mapping and tagging data from HARV in "vst_mappingTagging2015_goughSites.csv", where some of the pointIDs recorded do not have a corresponding match to pointIDs in "pointID_spatialData_towerPlots.csv". Unfortunately, prior to introducing front-end data entry controls, field technicians mistakenly chose pointIDs to map apparent individuals that did not have high-resolution GPS data associated with them. We are working to correct these errors, and in the meantime, it will be more difficult to calculate the location of mapped individuals when incorrect pointIDs were used by field technicians.