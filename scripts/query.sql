-- query for genes of interest

SELECT 
    g.gene_accession_id,
    g.gene_symbol,
    a.analysis_id,
    a.pvalue,
    m.mouse_strain,
    m.mouse_life_stage,
    p.parameter_id,
    p.parameter_name,
    p.parameter_description_name AS parameter_description,
    pr.procedure_name,
    pr.isMandatory,
    pr.description AS procedure_description,
    -- aggregate all linked diseases into a single string
    GROUP_CONCAT(DISTINCT d.disease_name SEPARATOR ', ') AS linked_diseases,
    -- aggregate all parameter groups into a single string
    GROUP_CONCAT(DISTINCT pg.group_name SEPARATOR ', ') AS parameter_groups_list
FROM analysis a
JOIN gene g 
    ON a.gene_accession_id = g.gene_accession_id
LEFT JOIN gene_disease_join gdj
    ON g.gene_accession_id = gdj.gene_accession_id
LEFT JOIN disease d
    ON gdj.doid = d.doid
JOIN parameter p
    ON a.parameter_id = p.parameter_id
LEFT JOIN parameter_group_join pgj
    ON p.parameter_id = pgj.parameter_id
LEFT JOIN parameter_groups pg
    ON pgj.parameter_group_id = pg.parameter_group_id
LEFT JOIN procedure_parameter pp
    ON p.impcParameterOrigID = pp.impcParameterOrigID
LEFT JOIN procedures pr
    ON pp.procedure_id = pr.procedure_id
JOIN mouse m
    ON a.mouse_id = m.mouse_id
WHERE g.gene_symbol IN ('Arr3', 'Mon1a', 'Myo1a', 'Prss21')
-- group all unique fields to collapse the aggregated fields
GROUP BY
    g.gene_accession_id, g.gene_symbol, a.analysis_id, a.pvalue,
    m.mouse_strain, m.mouse_life_stage, p.parameter_id, p.parameter_name,
    p.parameter_description_name, pr.procedure_name, pr.isMandatory, pr.description
ORDER BY 
    g.gene_accession_id, p.parameter_id, pr.procedure_name;