# IMPACT_MFS_toolkit
## Generic tools for IMPACT Market Functionality Score (MFS)

This project contains smoe basic tools for calculating a Market Functionality Score (MFS) using JMMI data.
The script is built using IMPACT Initiatives HQ guidance on the MFS and contains the weights from the guidance (expect for the Resilience component, which has some modifications). The code is commented to explain these.
This script was built by the SSD mission with a few functions from the AFG mission.
The project contains the following folders:

### documentation
This contains:
- the IMPACT Initiatives HQ guidance on the MFS,
- the indicator menu and scores suggested by HQ and implemented in the script.

### input
This is where you put the cleaned data.

### output
This is where the output with the MFS scores will be.

### maps
This is a side project which will require modification for each mission.
It contains the following:
- script for creating maps of the MFS and each MFS pillar,
- input - this contains a .csv with the points of all market locations in the JMMI tool and a .gpkg with the spatial layers for the admin boudaries (adm0 - adm2) for the country. The SSD layers are provided as an example.
- output - the folder for the output .png files.

Original script: Sirimon Thomas - sirimon.thomas@impact-initiatives.org
