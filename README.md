# Tidal modulation of moonlight
This was a project to analyse how tides affect moonlight visibility in the intertidal region. 

It includes scripts used to produce Figure 1a and Figures S1-S3 of the manuscript ”How light at night sets the circalunar clock in the marine midge *Clunio marinus*”. [https://doi.org/10.1177/07487304241286936](https://doi.org/10.1177/07487304241286936)

![Heatmap of light intensity detected at night with a
submerged radiometer in the intertidal region of Dinard](02_visuals/Figure1a_nm_400_500_600_2cycles_NIGHT.png)

**Figure 1. Moonlight intensity in the intertidal zone is modulated by the tides.** (a) Heatmap of light intensity detected at night with a submerged radiometer in the intertidal region of Dinard. Days of two consecutive lunar months from new moon to new moon (3 November 2013 and 2 January 2014) are shown on the x-axis and time of day on the y axis. Timing of low and high tides is shown as blue and red triangles, respectively. The maximum daily moonlight duration detected was of 4 h 30 min in days close to full moon.


*Note: The script for Figure S4 recreates the plot using tidal data fetched via the R package **geslaR**. In the manuscript, tide times and water levels were manually extracted from **maree.info**.  As a result, FigS4 is just a reproducible alternative.*

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
   R -q -e 'install.packages("geslaR", repos="https://cloud.r-project.org")'
   ```

4. Run the pipeline using Snakemake:

   ```bash
   snakemake -c 1
   ```