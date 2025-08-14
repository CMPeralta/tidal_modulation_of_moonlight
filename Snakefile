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