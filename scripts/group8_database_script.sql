
create database if not exists impc_db;
use impc_db;

set foreign_key_checks = 0;

-- staging tables to load raw csv data before normalisation

drop table if exists staging_table;
create table staging_table(
gene_accession_id varchar(20),
gene_symbol varchar(20),
mouse_strain varchar(20),
mouse_life_stage varchar(20),
parameter_id varchar(25),
pvalue float,
parameter_name varchar(255),
analysis_id varchar(20)
);

drop table if exists disease_staging_table;
create table disease_staging_table(
doid varchar(25),
disease_name varchar(255),
omim_ids text,
gene_accession_id varchar(20)
);

drop table if exists parameters_staging_table;
create table parameters_staging_table(
impcParameterOrigId int,
name varchar(255),
description text,
parameterId varchar(25)
);

drop table if exists procedures_staging_table;
create table procedures_staging_table(
procedure_name varchar(255) not null,
description text,
isMandatory varchar(5),
impcParameterOrigId int
);

drop table if exists parameter_groups_staging;
create table parameter_groups_staging (
group_name varchar(50),
parameter_name varchar(255)
);

-- normalised tables

drop table if exists gene;
create table gene(
gene_accession_id varchar(20) not null primary key,
gene_symbol varchar(20) not null
);

drop table if exists disease;
create table disease(
doid varchar(25) not null primary key,
disease_name varchar(255) not null,
omim_ids varchar(1000)
);

-- handles many to many gene and disease relationships
drop table if exists gene_disease_join;
create table gene_disease_join(
gene_accession_id varchar(20) not null,
doid varchar(25) not null,
primary key(gene_accession_id, doid),
foreign key(gene_accession_id) references gene(gene_accession_id),
foreign key(doid) references disease(doid)
);

drop table if exists mouse;
create table mouse(
mouse_id INT primary key auto_increment,
mouse_strain varchar(20),
mouse_life_stage varchar(20)
);

drop table if exists parameter_groups;
create table parameter_groups(
parameter_group_id int primary key auto_increment,
group_name varchar(50) not null
); 

drop table if exists parameter;
create table parameter(
parameter_id varchar(25) not null primary key,
parameter_name varchar(255) not null, 
parameter_description text,
parameter_description_name varchar(255), 
impcParameterOrigID INT unique null
);

drop table if exists parameter_group_join;
create table parameter_group_join(
parameter_id varchar(25) not null,
parameter_group_id int not null,
primary key (parameter_id, parameter_group_id),
foreign key (parameter_id) references parameter(parameter_id),
foreign key (parameter_group_id) references parameter_groups(parameter_group_id)
);

drop table if exists procedures;
create table procedures(
procedure_id int primary key auto_increment,
procedure_name varchar(255) not null,
isMandatory boolean,
description text
);


-- handles many to many procedure and parameter relationships
drop table if exists procedure_parameter;
create table procedure_parameter(
procedure_id int not null,
impcParameterOrigID int not null,
primary key (procedure_id, impcParameterOrigID),
foreign key (procedure_id) references procedures(procedure_id),
foreign key (impcParameterOrigID) references parameter(impcParameterOrigID)
);

drop table if exists analysis;
create table analysis(
analysis_id varchar(20) not null primary key,
gene_accession_id varchar(20),
mouse_id int,
parameter_id varchar(25),
pvalue float,
foreign key (gene_accession_id) references gene(gene_accession_id),
foreign key (mouse_id) references mouse(mouse_id),
foreign key (parameter_id) references parameter(parameter_id)
);

-- insert data into staging tables using manual import in DBeaver
-- data insertion from staging tables into normalised tables

-- insert gene information into gene table
insert into gene (gene_accession_id, gene_symbol)
select distinct gene_accession_id, gene_symbol
from staging_table st;

-- insert disease information into disease table
insert into disease (doid, disease_name, omim_ids)
select distinct doid, disease_name, omim_ids
from disease_staging_table dst;

-- insert information into gene-disease joining table
insert into gene_disease_join (gene_accession_id, doid)
select dst.gene_accession_id, dst.doid
from disease_staging_table dst
join gene g on g.gene_accession_id = dst.gene_accession_id;

-- insert information into mouse table
insert into mouse (mouse_strain, mouse_life_stage)
select distinct mouse_strain, mouse_life_stage
from staging_table st;

-- insert grouping information into parameter groups table
insert into parameter_groups (group_name)
select distinct group_name
from parameter_groups_staging pgs;

-- insert parameter information into parameter table
insert into parameter (parameter_id, parameter_name, parameter_description_name, impcParameterOrigID, parameter_description)
select distinct
    st.parameter_id,
    st.parameter_name,
    pst.name,
    pst.impcParameterOrigId,
    pst.description
from staging_table st
LEFT JOIN parameters_staging_table pst
    ON st.parameter_id = pst.parameterId;

-- insert information into parameter-group joining table
insert into parameter_group_join (parameter_id, parameter_group_id)
select distinct
    st.parameter_id,
    pg.parameter_group_id
from staging_table st
join parameter_groups_staging pgs
    on st.parameter_name = pgs.parameter_name
join parameter_groups pg
    on pgs.group_name = pg.group_name;

-- insert procedure information into procedure table
insert into procedures (procedure_name, isMandatory, description)
select distinct procedure_name,
	case upper(isMandatory)
	when 'TRUE' then 1
	when 'FALSE' then 0
	else null
	end,
	description
from procedures_staging_table pst;

-- insert information into procedure parameter linking table
insert into procedure_parameter (procedure_id, impcParameterOrigID)
select distinct
    p.procedure_id,
    pst.impcParameterOrigId
from procedures_staging_table pst
join procedures p on p.procedure_name = pst.procedure_name
INNER JOIN parameter pr
    ON pr.impcParameterOrigID = pst.impcParameterOrigId;


-- insert information into analysis table
insert into analysis (analysis_id, gene_accession_id, mouse_id, parameter_id, pvalue)
select st.analysis_id,
       st.gene_accession_id,
       m.mouse_id,
       st.parameter_id,
       st.pvalue
from staging_table st
join gene g on g.gene_accession_id = st.gene_accession_id
join parameter p on p.parameter_id = st.parameter_id
join mouse m on m.mouse_strain = st.mouse_strain
and m.mouse_life_stage = st.mouse_life_stage;

-- clean up staging tables
drop table if exists staging_table;
drop table if exists disease_staging_table;
drop table if exists parameters_staging_table;
drop table if exists procedures_staging_table;
drop table if exists parameter_groups_staging;

set foreign_key_checks = 1;

-- query to find related information for genes of interest
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
