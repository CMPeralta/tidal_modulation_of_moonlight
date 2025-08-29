rule targets: 
    input: 
        "01_data/249803",
        "01_data/dinard_light_data_formatted.csv",
        "02_visuals/FigureS2a_Dinard_raw_data_spectrum_all.pdf",
        "02_visuals/FigureS1_map_locations_insets.pdf",
        "02_visuals/FigureS2bc_daylight_and_moonlight_averaged_spectraFM.pdf",
        "02_visuals/FigureS3_Dinard_summed_waves_over_time.pdf",
        "01_data/tidal_data_GESLA_STmalo.csv",
        "02_visuals/FigureS4_selected_wavelenghts_daylight_tides.pdf",
        "02_visuals/Figure1a_nm_400_500_600_2cycles_NIGHT.pdf",
        

rule get_field_light_data: 
    input: 
        script = "00_code/00_get_field_light_data.sh"
    output:
        "01_data/249803"
    params: 
        file = "249803"
    shell: 
        """
        {input.script} {params.file}
        """

rule prep_light_data: 
    input: 
        script = "00_code/01_prep_light_data.R",
        data = "01_data/249803"
    output:
        "01_data/dinard_light_data_formatted.csv"
    params: 
        file = "249803"
    shell: 
        """
        {input.script} {params.file}
        """

rule create_3d_plot: 
    input: 
        bash_script = "00_code/04_create_plots_Dinard_spectrum.sh",
        r_script = "00_code/02_subset_light_data_windows.R",
        python_script = "00_code/03_dinard_light_6_plots.py",
        data = "01_data/dinard_light_data_formatted.csv"
    output: 
        "02_visuals/FigureS2a_Dinard_raw_data_spectrum_all.pdf"
    shell: 
        """
        {input.bash_script}
        """

rule create_map:
    input: 
        rscript = "00_code/07_make_map_locations.R"
    output: 
        "02_visuals/FigureS1_map_locations_insets.pdf"
  

rule create_plots_dinard_wave: 
    input: 
        bash_script = "00_code/10_create_R_plots_Dinard.sh",
        r_script1 = "00_code/05_dinard_averaged_light_spectra.R",
        r_script2 = "00_code/06_plot_summed_wavelengths_over_time.R",
        data = "01_data/dinard_light_data_formatted.csv"
    output: 
        "02_visuals/FigureS2bc_daylight_and_moonlight_averaged_spectraFM.pdf",
        "02_visuals/FigureS3_Dinard_summed_waves_over_time.pdf",
    shell: 
        """
        {input.bash_script}
        """     
       
rule get_GELSA_tidal_data: 
    input: 
        r_script = "00_code/11_get_tidal_data_GESLA.R"
    output: 
        "01_data/tidal_data_GESLA_STmalo.csv"
    shell: 
        """
        {input.r_script}
        """ 

rule create_plot_dinard_day_tides: 
    input: 
        r_script = "00_code/09_plot_selected_wavelenghts_over_daytime.R",
        data_light = "01_data/dinard_light_data_formatted.csv",
        data_tides = "01_data/tidal_data_GESLA_STmalo.csv"
    output: 
        "02_visuals/FigureS4_selected_wavelenghts_daylight_tides.pdf"
    shell: 
        """
        {input.r_script}
        """ 
        
rule create_plots_dinard_night_tides: 
    input: 
        r_script = "00_code/08_plot_dinard_moonlight.R",
        data_light = "01_data/dinard_light_data_formatted.csv",
        data_tides = "10_pre_processed_data/tidal_data_water_level_ERA5_X_dinard_357.9712_Y_48.64014.csv"
    output: 
        "02_visuals/Figure1a_nm_400_500_600_2cycles_NIGHT.pdf",
    shell: 
        """
        {input.r_script}
        """ 