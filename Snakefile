rule build_db:
    input:
        multiext("fetch_data/usa_00081", ".xml", ".dat.gz"),
    output:
        "pums.duckdb",
    script:
        "prep_scripts/build_db.R"


rule analysis:
    input:
        "pums.duckdb",
    output:
        "input_data/income_tbls.rds",
        "input_data/income_hists.rds",
    script:
        "prep_scripts/calc_wage_gap.R"


rule eda:
    input:
        "input_data/income_tbls.rds",
        "input_data/income_hists.rds",
        "_utils/misc_functions.R",
    output:
        "analysis/wage_gap_eda.md",
        "output_data/wage_gap_tbls.rds",
        "output_data/wage_gap_tbls.json",
    shell:
        "quarto render analysis/wage_gap_eda.qmd"


rule readme:
    input:
        "README.qmd",
    output:
        "README.md",
    shell:
        "quarto render README.qmd"


rule all:
    default_target: True
    input:
        "README.md",
        rules.eda.output


rule list_rules:
    run:
        rule_names = [f"* {rule}" for rule in list(rules._rules.keys())]
        bullets = "\n".join(rule_names)
        print(f"Rules:\n{bullets}")
