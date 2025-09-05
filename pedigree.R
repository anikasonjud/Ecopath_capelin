

# Pedigree
# The criteria for the data pedigrees (or data quality grade). B = biomass, P/B = production/biomass ratio, Q/B = consumption/biomass ratio, DC = diet
# composition, and C = fishery catch or subsistence harvest. This table recreated from Aydin et al. (2007).
# Data pedigree and corresponding data characteristics B, P/B, Q/B, DC, and C

# 0.1 Assessment data is established and substantial, from more than one independent method (from which the best method is selected) with resolution on multiplespatial scales.
# 2
# 2 Data is a direct estimate but with limited coverage/corroboration, or established regional estimate is available while subregional resolution is poor.
#0.8
# 3 Data is proxy, proxy may have known but consistent bias.
#0.7
# 4 Direct estimate or proxy with high variation/limited confidence or incomplete coverage.
#0.6
# B and C                                      |      P/B, Q/B, and DC
# 5 Estimate requires inclusion                |   # 5 Estimation based on same species but in
# of highly uncertain scaling                  |   # "historical" time period, or a general
# factors or extrapolation                     |   # model specific to the area.

# 0.5

# 6 Historical and/or single                   |  # 6 For P/B and Q/B, general life history 
# study only, not overlapping                  |  # proxies or other Ecopath model. For DC, 
# in area or time.                             |  # same species in adjacent region or similar
# 0.4                                             |  # species in the same region.


# 7 Requires selection between                 |  # 7 General literature review from a wide
# multiple incomplete sources                  |  # range of species, or outside the region. For
# with wide range.                             |  # DC, from other Ecopath model.
#0.3


# 8 Estimated by Ecopath                       | # 8 Functional group represents multiple
#0.8
#                                              | # species with diverse life history traits. For
#                                               | # P/B and Q/B, estimated by Ecopath.


# CEP
REco.params$pedigree[Group == 'CEP', Biomass := 0.8] #estimated by ecopath
REco.params$pedigree[Group == 'CEP', PB := 0.6] #from other ecopath model
REco.params$pedigree[Group == 'CEP', QB := 0.6] #from other ecopath model
REco.params$pedigree[Group == 'CEP', Diet := 0.8] # 8 - from literature
REco.params$pedigree[Group == 'CEP', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'CEP', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'CEP', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'CEP', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'CEP', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'CEP', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'CEP', PELAGIC := 0.1] #from other ecopath model




# FBP
REco.params$pedigree[Group == 'FBP', Biomass := 0.8] #estimated by ecopath
REco.params$pedigree[Group == 'FBP', PB := 0.6] #from other ecopath model
REco.params$pedigree[Group == 'FBP', QB := 0.6] #from other ecopath model
REco.params$pedigree[Group == 'FBP', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'FBP', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FBP', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FBP', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FBP', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FBP', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FBP', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FBP', PELAGIC := 0.1] #from other ecopath model

# FSD

REco.params$pedigree[Group == 'FSD', Biomass := 0.8] #estimated by ecopath
REco.params$pedigree[Group == 'FSD', Diet := 0.3]# Data is a direct estimate but with limited coverage/corroboration, or established regional estimate is available while subregional resolution is poor.
REco.params$pedigree[Group == 'FSD', PB := 0.2] #Data is a direct estimate but with limited coverage/corroboration, or established regional estimate is available while subregional resolution is poor.
REco.params$pedigree[Group == 'FSD', QB := 0.2] #Data is a direct estimate but with limited coverage/corroboration, or established regional estimate is available while subregional resolution is poor.
REco.params$pedigree[Group == 'FSD', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FSD', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSD', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSD', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FSD', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FSD', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSD', PELAGIC := 0.1] #from other ecopath model

#FDF
REco.params$pedigree[Group == 'FDF', Biomass := 0.8] #estimated by ecopath
REco.params$pedigree[Group == 'FDF', Diet := 0.2]# Data is a direct estimate but with limited coverage/corroboration, or established regional estimate is available while subregional resolution is poor.
REco.params$pedigree[Group == 'FDF', PB := 0.6] # 6 For P/B and Q/B, general life history roxies or other Ecopath model 
REco.params$pedigree[Group == 'FDF', QB := 0.6] # 6 For P/B and Q/B, general life history roxies or other Ecopath model 
REco.params$pedigree[Group == 'FDF', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FDF', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FDF', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FDF', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FDF', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FDF', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FDF', PELAGIC := 0.1] #from other ecopath model


# PWN

REco.params$pedigree[Group == 'PWN', Biomass := 0.8] #estimated by ecopath
REco.params$pedigree[Group == 'PWN', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'PWN', PB := 0.6] # 6 For P/B and Q/B, general life history roxies or other Ecopath model 
REco.params$pedigree[Group == 'PWN', QB := 0.8] # 8 estimated by ecopath
REco.params$pedigree[Group == 'PWN', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'PWN', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'PWN', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'PWN', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'PWN', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'PWN', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'PWN', PELAGIC := 0.1] #from other ecopath model


# FFF
REco.params$pedigree[Group == 'FFF', Biomass := 0.8] #estimated by ecopath
REco.params$pedigree[Group == 'FFF', Diet := 0.8 ] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FFF', PB := 0.8] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FFF', QB := 0.8] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FFF', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FFF', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FFF', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FFF', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FFF', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FFF', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FFF', PELAGIC := 0.1] #from other ecopath model

#SSR
REco.params$pedigree[Group == 'SSR', Biomass := 0.8] # 7 reqires selection between multiple incomplete sources with wide range 
REco.params$pedigree[Group == 'SSR', Diet := 0.3 ] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'SSR', PB := 0.6] # 6 general life history proxies
REco.params$pedigree[Group == 'SSR', QB := 0.6] # 6 general life history proxies
REco.params$pedigree[Group == 'SSR', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'SSR', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSR', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSR', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'SSR', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'SSR', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSR', PELAGIC := 0.1] #from other ecopath model

# SSD
REco.params$pedigree[Group == 'SSD', Biomass :=0.8] # 7 reqires selection between multiple incomplete sources with wide range 
REco.params$pedigree[Group == 'SSD', Diet := 0.3] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'SSD', PB := 0.6] # 6 general life history proxies
REco.params$pedigree[Group == 'SSD', QB := 0.6] # 6 general life history proxies
REco.params$pedigree[Group == 'SSD', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'SSD', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSD', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSD', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'SSD', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'SSD', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSD', PELAGIC := 0.1] #from other ecopath model

#SSH
REco.params$pedigree[Group == 'SSH', Biomass :=0.8] # 7 reqires selection between multiple incomplete sources with wide range 
REco.params$pedigree[Group == 'SSH', Diet := 0.3] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'SSH', PB := 0.6] # 6 general life history proxies
REco.params$pedigree[Group == 'SSH', QB := 0.6] # 6 general life history proxies
REco.params$pedigree[Group == 'SSH', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'SSH', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSH', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSH', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'SSH', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'SSH', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SSH', PELAGIC := 0.1] #from other ecopath model



REco.params$pedigree[Group == 'Phytoplankton', Biomass := 0.8] #Estimates requirres inclusion of highly uncertain scaling factors or extrapolations
REco.params$pedigree[Group == 'Phytoplankton', Diet := 0.8] #Estimates requirres inclusion of highly uncertain scaling factors or extrapolations
REco.params$pedigree[Group == 'Phytoplankton', PB := 0.8] #Estimates requirres inclusion of highly uncertain scaling factors or extrapolations
REco.params$pedigree[Group == 'Phytoplankton', QB := 0.8] #Estimates requirres inclusion of highly uncertain scaling factors or extrapolations
REco.params$pedigree[Group == 'Phytoplankton', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'Phytoplankton', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'Phytoplankton', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'Phytoplankton', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'Phytoplankton', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'Phytoplankton', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'Phytoplankton', PELAGIC := 0.1] #from other ecopath model

# FCD juv
REco.params$pedigree[Group == 'FCD.juv', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FCD.juv', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FCD.juv', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FCD.juv', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FCD.juv', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FCD.juv', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCD.juv', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCD.juv', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FCD.juv', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FCD.juv', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCD.juv', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'FCD.adult', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FCD.adult', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FCD.adult', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FCD.adult', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FCD.adult', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FCD.adult', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCD.adult', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCD.adult', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FCD.adult', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FCD.adult', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCD.adult', PELAGIC := 0.1] #from other ecopath model



REco.params$pedigree[Group == 'FHA.juv', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FHA.juv', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FHA.juv', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FHA.juv', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FHA.juv', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FHA.juv', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHA.juv', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHA.juv', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FHA.juv', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FHA.juv', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHA.juv', PELAGIC := 0.1] #from other ecopath model



REco.params$pedigree[Group == 'FHA.adult', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FHA.adult', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FHA.adult', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FHA.adult', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FHA.adult', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FHA.adult', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHA.adult', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHA.adult', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FHA.adult', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FHA.adult', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHA.adult', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'FSA.juv', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FSA.juv', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FSA.juv', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FSA.juv', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FSA.juv', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FSA.juv', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSA.juv', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSA.juv', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FSA.juv', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FSA.juv', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSA.juv', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'FSA.adult', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FSA.adult', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FSA.adult', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FSA.adult', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FSA.adult', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FSA.adult', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSA.adult', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSA.adult', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FSA.adult', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FSA.adult', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FSA.adult', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'FGH', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FGH', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FGH', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FGH', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FGH', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FGH', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FGH', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FGH', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FGH', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FGH', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FGH', PELAGIC := 0.1] #from other ecopath model




REco.params$pedigree[Group == 'FRF', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FRF', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FRF', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FRF', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FRF', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FRF', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FRF', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FRF', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FRF', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FRF', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FRF', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'FHE', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FHE', Diet := 0.1] # 2 dATA IS A DIRECT ESTIMATE BUT WOTH LIMITED COVERAGE/CORROBORATION, OR ESTABLISHES REGIONAL ESTIMATE IS AVAILABLE WHILE SUBREGIONAL RESOLUTION IS POOR
REco.params$pedigree[Group == 'FHE', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FHE', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FHE', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FHE', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHE', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHE', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FHE', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FHE', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FHE', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'FCA', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FCA', Diet := 0.8 ] # from literaturel
REco.params$pedigree[Group == 'FCA', PB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FCA', QB := 0.1] # 6 general life history proxies
REco.params$pedigree[Group == 'FCA', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FCA', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCA', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCA', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FCA', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FCA', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FCA', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'FOC', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FOC', PB := 0.8] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FOC', QB := 0.8] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FDC', Diet := 0.1 ] #from other ecopath model
REco.params$pedigree[Group == 'FOC', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FOC', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FOC', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FOC', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FOC', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FOC', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FOC', PELAGIC := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FOC', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'FDC', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FDC', PB := 0.8] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FDC', QB := 0.8] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FDC', Diet := 0.1 ] #from other ecopath model
REco.params$pedigree[Group == 'FDC', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FDC', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FDC', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FDC', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FDC', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FDC', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FDC', PELAGIC := 0.1] #from other ecopath model




REco.params$pedigree[Group == 'FMI', Biomass := 0.1] # 0.1 biomass from assessment
REco.params$pedigree[Group == 'FMI', PB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FMI', QB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FMI', Diet := 0.1 ] #
REco.params$pedigree[Group == 'FMI', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FMI', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FMI', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FMI', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FMI', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FMI', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FMI', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'PIN', Biomass := 0.5] # 4 Direct estimate or proxy with high variation/limited confidence or incomplete coverage
REco.params$pedigree[Group == 'PIN', PB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'PIN', QB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'PIN', Diet := 0.7 ] #estimation based on same species but in historical time period
REco.params$pedigree[Group == 'PIN', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'PIN', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'PIN', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'PIN', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'PIN', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'PIN', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'PIN', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'WMW', Biomass := 0.5] # 4 Direct estimate or proxy with high variation/limited confidence or incomplete coverage
REco.params$pedigree[Group == 'WMW', PB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'WMW', QB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'WMW', Diet := 0.7 ] #estimation based on same species but in historical time period
REco.params$pedigree[Group == 'WMW', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'WMW', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WMW', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WMW', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'WMW', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'WMW', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WMW', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'WHB', Biomass := 0.5] # 4 Direct estimate or proxy with high variation/limited confidence or incomplete coverage
REco.params$pedigree[Group == 'WHB', PB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'WHB', QB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'WHB', Diet := 0.7 ] # 5 estimation based on same species but in historical time period
REco.params$pedigree[Group == 'WHB', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'WHB', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WHB', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WHB', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'WHB', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'WHB', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WHB', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'WTO', Biomass := 0.5] # 4 Direct estimate or proxy with high variation/limited confidence or incomplete coverage
REco.params$pedigree[Group == 'WTO', PB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'WTO', QB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'WTO', Diet := 0.7 ] #estimation based on same species but in historical time period
REco.params$pedigree[Group == 'WTO', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'WTO', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WTO', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WTO', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'WTO', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'WTO', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WTO', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'WHT', Biomass :=0.5] # 4 Direct estimate or proxy with high variation/limited confidence or incomplete coverage
REco.params$pedigree[Group == 'WHT', PB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'WHT', QB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'WHT', Diet := 0.7 ] #estimation based on same species but in historical time period
REco.params$pedigree[Group == 'WHT', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'WHT', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WHT', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WHT', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'WHT', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'WHT', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'WHT', PELAGIC := 0.1] #from other ecopath model





REco.params$pedigree[Group == 'SB', Biomass := 0.5] # 4 Direct estimate or proxy with high variation/limited confidence or incomplete coverage
REco.params$pedigree[Group == 'SB', PB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'SB', QB := 0.1] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'SB', Diet := 0.7 ] # estimation based on same species but in historical time period
REco.params$pedigree[Group == 'SB', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'SB', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SB', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SB', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'SB', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'SB', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'SB', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'FKR', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'FKR', Biomass := 0.8 ] #estimated by ecopath
REco.params$pedigree[Group == 'FKR', PB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FKR', QB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FKR', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FKR', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FKR', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FKR', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FKR', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FKR', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FKR', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'FEP', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'FEP', Biomass := 0.8 ] #estimated by ecopath
REco.params$pedigree[Group == 'FEP', PB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FEP', QB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FEP', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FEP', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FEP', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FEP', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FEP', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FEP', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FEP', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'FIN', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'FIN', Biomass := 0.8 ] #estimated by ecopath
REco.params$pedigree[Group == 'FIN', PB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FIN', QB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FIN', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FIN', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FIN', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FIN', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FIN', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FIN', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FIN', PELAGIC := 0.1] #from other ecopath model



REco.params$pedigree[Group == 'FLC', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'FLC', Biomass := 0.8 ] #estimated by ecopath
REco.params$pedigree[Group == 'FLC', PB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FLC', QB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'FLC', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FLC', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FLC', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FLC', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'FLC', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'FLC', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'FLC', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'ZL', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'ZL', Biomass := 0.8 ] #estimated by ecopath
REco.params$pedigree[Group == 'ZL', PB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'ZL', QB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'ZL', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'ZL', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZL', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZL', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'ZL', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'ZL', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZL', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'ZG', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'ZG', Biomass := 0.8 ] #estimated by ecopath
REco.params$pedigree[Group == 'ZG', PB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'ZG', QB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'ZG', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'ZG', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZG', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZG', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'ZG', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'ZG', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZG', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'ZS', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'ZS', Biomass := 0.8 ] #estimated by ecopath
REco.params$pedigree[Group == 'ZS', PB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'ZS', QB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'ZS', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'ZS', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZS', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZS', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'ZS', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'ZS', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'ZS', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'LOB', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'LOB', Biomass := 0.5 ] #direct estimate or proxy with high variation/ limited confidence or incomplete coverage
REco.params$pedigree[Group == 'LOB', PB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'LOB', QB := 0.6] # 8 Functional group represents multiple groups with diverse life history traits

REco.params$pedigree[Group == 'LOB', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'LOB', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'LOB', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'LOB', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'LOB', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'LOB', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'LOB', PELAGIC := 0.1] #from other ecopath model

REco.params$pedigree[Group == 'Detritus', Diet := 0.8 ] #from literature
REco.params$pedigree[Group == 'Detritus', Biomass := 0.8 ] #estimated by ecopath
REco.params$pedigree[Group == 'Detritus', PB := 0.8] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'Detritus', QB := 0.8] # 8 Functional group represents multiple groups with diverse life history traits
REco.params$pedigree[Group == 'Detritus', TRAWLS := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'Detritus', LONGLINE := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'Detritus', SEINERS := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'Detritus', GILLNETS := 0.1] # 8 - from literature
REco.params$pedigree[Group == 'Detritus', HARPOON := 0.1] #estimated by ecopath
REco.params$pedigree[Group == 'Detritus', OTHER := 0.1] #from other ecopath model
REco.params$pedigree[Group == 'Detritus', PELAGIC := 0.1] #from other ecopath model


REco.params$pedigree[Group == 'TRAWLS', TRAWLS := 0.1]
REco.params$pedigree[Group == 'TRAWLS', PELAGIC := 0.1]
REco.params$pedigree[Group == 'TRAWLS', LONGLINE := 0.1]
REco.params$pedigree[Group == 'TRAWLS', GILLNETS := 0.1]
REco.params$pedigree[Group == 'TRAWLS', OTHER := 0.1]
REco.params$pedigree[Group == 'TRAWLS', SEINERS := 0.1]
REco.params$pedigree[Group == 'TRAWLS', HARPOON := 0.1]

REco.params$pedigree[Group == 'PELAGIC', TRAWLS := 0.1]
REco.params$pedigree[Group == 'PELAGIC', PELAGIC := 0.1]
REco.params$pedigree[Group == 'PELAGIC', LONGLINE := 0.1]
REco.params$pedigree[Group == 'PELAGIC', GILLNETS := 0.1]
REco.params$pedigree[Group == 'PELAGIC', OTHER := 0.1]
REco.params$pedigree[Group == 'PELAGIC', SEINERS := 0.1]
REco.params$pedigree[Group == 'PELAGIC', HARPOON := 0.1]

REco.params$pedigree[Group == 'LONGLINE', TRAWLS := 0.1]
REco.params$pedigree[Group == 'LONGLINE', PELAGIC := 0.1]
REco.params$pedigree[Group == 'LONGLINE', LONGLINE := 0.1]
REco.params$pedigree[Group == 'LONGLINE', GILLNETS := 0.1]
REco.params$pedigree[Group == 'LONGLINE', OTHER := 0.1]
REco.params$pedigree[Group == 'LONGLINE', SEINERS := 0.1]
REco.params$pedigree[Group == 'LONGLINE', HARPOON := 0.1]

REco.params$pedigree[Group == 'GILLNETS', TRAWLS := 0.1]
REco.params$pedigree[Group == 'GILLNETS', PELAGIC := 0.1]
REco.params$pedigree[Group == 'GILLNETS', LONGLINE := 0.1]
REco.params$pedigree[Group == 'GILLNETS', GILLNETS := 0.1]
REco.params$pedigree[Group == 'GILLNETS', OTHER := 0.1]
REco.params$pedigree[Group == 'GILLNETS', SEINERS := 0.1]
REco.params$pedigree[Group == 'GILLNETS', HARPOON := 0.1]

REco.params$pedigree[Group == 'OTHER', TRAWLS := 0.1]
REco.params$pedigree[Group == 'OTHER', PELAGIC := 0.1]
REco.params$pedigree[Group == 'OTHER', LONGLINE := 0.1]
REco.params$pedigree[Group == 'OTHER', GILLNETS := 0.1]
REco.params$pedigree[Group == 'OTHER', OTHER := 0.1]
REco.params$pedigree[Group == 'OTHER', SEINERS := 0.1]
REco.params$pedigree[Group == 'OTHER', HARPOON := 0.1]

REco.params$pedigree[Group == 'SEINERS', TRAWLS := 0.1]
REco.params$pedigree[Group == 'SEINERS', PELAGIC := 0.1]
REco.params$pedigree[Group == 'SEINERS', LONGLINE := 0.1]
REco.params$pedigree[Group == 'SEINERS', GILLNETS := 0.1]
REco.params$pedigree[Group == 'SEINERS', OTHER := 0.1]
REco.params$pedigree[Group == 'SEINERS', SEINERS := 0.1]
REco.params$pedigree[Group == 'SEINERS', HARPOON := 0.1]

REco.params$pedigree[Group == 'HARPOON', TRAWLS := 0.1]
REco.params$pedigree[Group == 'HARPOON', PELAGIC := 0.1]
REco.params$pedigree[Group == 'HARPOON', LONGLINE := 0.1]
REco.params$pedigree[Group == 'HARPOON', GILLNETS := 0.1]
REco.params$pedigree[Group == 'HARPOON', OTHER := 0.1]
REco.params$pedigree[Group == 'HARPOON', SEINERS := 0.1]
REco.params$pedigree[Group == 'HARPOON', HARPOON := 0.1]


REco.params$pedigree[Group == 'TRAWLS', TRAWLS := 0.1] # DoF
REco.params$pedigree[Group == 'PELAGIC', PELAGIC := 0.1] # DoF
REco.params$pedigree[Group == 'LONGLINE', LONGLINE := 0.1] #DoF
REco.params$pedigree[Group == 'GILLNETS', GILLNETS := 0.1] #DoF
REco.params$pedigree[Group == 'OTHER', OTHER := 0.1] # DoF
REco.params$pedigree[Group == 'SEINERS', SEINERS := 0.1] # DoF
REco.params$pedigree[Group == 'HARPOON', HARPOON := 0.1] # DoF


REco.params$pedigree[Group == 'TRAWLS', Biomass := 0.1] # DoF
REco.params$pedigree[Group == 'TRAWLS', Diet := 0.1] # DoF
REco.params$pedigree[Group == 'TRAWLS', PB := 0.1] #DoF
REco.params$pedigree[Group == 'TRAWLS', QB := 0.1] #DoF

REco.params$pedigree[Group == 'LONGLINE', Biomass := 0.1] # DoF
REco.params$pedigree[Group == 'LONGLINE', Diet := 0.1] # DoF
REco.params$pedigree[Group == 'LONGLINE', PB := 0.1] # DoF
REco.params$pedigree[Group == 'LONGLINE', QB := 0.1] # DoF

REco.params$pedigree[Group == 'SEINERS', Biomass := 0.1] # DoF
REco.params$pedigree[Group == 'SEINERS', Diet := 0.1] # DoF
REco.params$pedigree[Group == 'SEINERS', PB := 0.1] # DoF
REco.params$pedigree[Group == 'SEINERS', QB := 0.1] # DoF

REco.params$pedigree[Group == 'HARPOON', Biomass := 0.1] # DoF
REco.params$pedigree[Group == 'HARPOON', Diet := 0.1] # DoF
REco.params$pedigree[Group == 'HARPOON', PB := 0.1] #DoF
REco.params$pedigree[Group == 'HARPOON', QB := 0.1] # DoF

REco.params$pedigree[Group == 'GILLNETS', Biomass := 0.1] # DoF
REco.params$pedigree[Group == 'GILLNETS', Diet := 0.1] #DoF
REco.params$pedigree[Group == 'GILLNETS', PB := 0.1] # DoF
REco.params$pedigree[Group == 'GILLNETS', QB := 0.1] # DoF

REco.params$pedigree[Group == 'OTHER', Biomass := 0.1] # DoF
REco.params$pedigree[Group == 'OTHER', Diet := 0.1] # DoF
REco.params$pedigree[Group == 'OTHER', PB := 0.1] # DoF
REco.params$pedigree[Group == 'OTHER', QB := 0.1] # DoF

REco.params$pedigree[Group == 'PELAGIC', Biomass := 0.1] # DoF
REco.params$pedigree[Group == 'PELAGIC', Diet := 0.1] # DoF
REco.params$pedigree[Group == 'PELAGIC', PB := 0.1] # DoF
REco.params$pedigree[Group == 'PELAGIC', QB := 0.1] # DoF