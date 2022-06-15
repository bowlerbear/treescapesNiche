# Treescapes Niche
Analysis of BRC data to assess forest niches and associations with long-term trends

### Numbered scripts
- 01 assess the number of records for each taxa and region
- 02 process land cover maps to get % forest cover in available years
- 03 process occurrence data so that they are in a format for further models
- 04 scripts to assess the association between species occupancy and forest cover - run on HPC
- 05 scripts to do 03 but in an occupancy model framework - not used at the moment, testing out a new package - run on HPC
- 06 script to process the outputs of script 04 across all taxa, i.e., outputs obtained after HPC run
- 07 script to explore the use of cluster analysis to group species with similar niches for the multi-species indicators
- 08 script to explore relationships between continuous estimates of forest preference and species trends
- 10 script to look in more detail at carabid results
- 11 script to look in more detail at moth results


### Folders
- plots 
- inla (quick script to start a possible spatio-temporal model)

### Analysis scripts not on github - instead on the CEH shared datalab
- script to produce the time-series indicators using the results of Charlie Outhwaites occu-det models. These occu-det models are stored on the CEH object store, accessible from datalabs
- a script to summarise the long-term trends of each species - also using the outputs Charlie' models

### Ignored folders
- inputs - files containing the processed raw data ready for analysis
- outputs - ignore for the moment - they are many and large

