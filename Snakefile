rule targets: 
    input: 
        "01_data/249803",
        "01_data/dinard_light_data_formatted.csv"


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