# Tidal modulation of moonlight
This was a project to analyse how tides affect moonlight visibility in the intertidal region. 

It includes scripts used to produce some figures (Figure 1a and Figures S1-S4) of the manuscript ”How light at night sets the circalunar clock in the marine midge *Clunio marinus*”. [https://doi.org/10.1177/07487304241286936](https://doi.org/10.1177/07487304241286936)


![Heatmap of light intensity detected at night with a
submerged radiometer in the intertidal region of Dinard](02_visuals/Figure1a_nm_400_500_600_2cycles_NIGHT.png)

## Usage

To get the data and produce the figures, follow these steps:

1. Clone the GitHub repository:

   ```bash
   git clone https://github.com/CMPeralta/tidal_modulation_of_moonlight.git
   cd tidal_modulation_of_moonlight
   ```

2. Create and activate the conda environment with the versions used to run all code: 

   ```bash
   conda env create -f unified_env.yml
   conda activate moon_py3d
   ```

3. Open R and install geslaR via CRAN (takes a while to install):

   ```r
   install.packages("geslaR", dependencies = TRUE, repos = "https://cloud.r-project.org")
   ```

4. Run the pipeline using Snakemake:

   ```bash
   snakemake -c 1
   ```