
# The Importance of Blue and Green Landscape Connectivity for Biodiversity in Urban Ponds
- The model:
  - Calculating dissimilarity among pond communities to use as the multivariate response
  - Determining multivariate predictors summarizing the local environment
  - Identifying spatial vectors to use as structural connectivity predictors
  - Optimizing resistance surfaces and using them as functional connectivity predictors
- Community differentiation attributable purely to the local environment versus structural connectivity versus functional connectivity
- Species diversity and connectivity:
  - Classifying ponds based on local environmental characteristics and comparing differences in community composition and blue and green connectivity among pond classes
  - Determining the relationship between connectivity and the biological diversity across pond communities, and how this relationship may vary based on pond class



To run the R scripts:

source("https://raw.githubusercontent.com/chazhyseni/pond_conn/master/R/Input.R");
source("https://raw.githubusercontent.com/chazhyseni/pond_conn/master/R/LandCover_250mRadius.R");
source("https://raw.githubusercontent.com/chazhyseni/pond_conn/master/R/Connectivity.R");
source("https://raw.githubusercontent.com/chazhyseni/pond_conn/master/R/dbRDA.R");
source("https://raw.githubusercontent.com/chazhyseni/pond_conn/master/R/Diversity_Differentiation.R");
source("https://raw.githubusercontent.com/chazhyseni/pond_conn/master/R/Connectivity_Biodiversity.R");
source("https://raw.githubusercontent.com/chazhyseni/pond_conn/master/R/Maps.R")



To run Circuitscape:

cd ~
git clone https://github.com/chazhyseni/pond_conn
cd pond_conn


Then, in Julia:

using Pkg
Pkg.add("Circuitscape")
compute("Circuitscape/Blue/Input/Blue_CS_parameters.ini")
compute("Circuitscape/BlueGreen/Input/BlueGreen_CS_parameters.ini")
compute("Circuitscape/Green/Input/Green_CS_parameters.ini")