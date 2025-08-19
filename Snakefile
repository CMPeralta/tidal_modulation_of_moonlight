rule targets: 
    input: 
        "01_data/249803",
        "01_data/dinard_light_data_formatted.csv",
        "02_visuals/FigureS2a_Dinard_raw_data_spectrum_all.pdf",
        "02_visuals/FigureS2bc_daylight_and_moonlight_averaged_spectraFM.pdf",
        "02_visuals/Figure1a_nm_400_500_600_2cycles_NIGHT.pdf"

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
        script = "00_code/01_prep_light_data.R"
    output:
        "01_data/dinard_light_data_formatted.csv"
    shell: 
        """
        {input.script}
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

rule create_plots_dinard: 
    input: 
        bash_script = "00_code/10_create_R_plots_Dinard.sh",
        r_script1 = "00_code/05_dinard_averaged_light_spectra.R",
        r_script2 = "00_code/08_plot_dinard_moonlight.R",
        data = "01_data/dinard_light_data_formatted.csv"
    output: 
        "02_visuals/FigureS2bc_daylight_and_moonlight_averaged_spectraFM.pdf",
        "02_visuals/Figure1a_nm_400_500_600_2cycles_NIGHT.pdf"
    shell: 
        """
        {input.bash_script}
        """        