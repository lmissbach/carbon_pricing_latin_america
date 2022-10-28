# Supplementary Data to Cash transfers in the context of carbon pricing reforms in Latin America and the Caribbean 

This repository contains supplementary data on the publication "Cash transfers in the context of carbon pricing reforms in Latin America and the Caribbean".

## Data

Please note that due to confidentiality restrictions we have no permission to share raw data. Access is permitted upon reasonable request from the authors and conditional on permission by the respective statistical authorities. Raw data for each country-specific household budget survey can nevertheless be accessed via:
- Argentina: https://www.indec.gob.ar/indec/web/Nivel4-Tema-4-45-151
- Barbados: https://publications.iadb.org/en/barbados-survey-living-conditions-2016
- Bolivia: https://www.ine.gob.bo/index.php/estadisticas-sociales/vivienda-y-servicios-basicos/encuestas-de-hogares-vivienda/
- Brazil: https://www.ibge.gov.br/en/statistics/social/population/25610-pof-2017-2018-pof-en.html?=&t=downloads including https://github.com/paulobistenealexandrino/Iniciacao-Cientifica/blob/f7a52cb8efb025590ef2a8d27fb0f1b2d79ef3fd/Leitura%20dos%20Microdados%20-%20R.R
- Chile: https://www.ine.cl/estadisticas/sociales/ingresos-y-gastos/encuesta-de-presupuestos-familiares
- Colombia: https://www.dane.gov.co/index.php/estadisticas-por-tema/pobreza-y-condiciones-de-vida/encuesta-nacional-de-presupuestos-de-los-hogares-enph
- Costa Rica: https://inec.cr/estadisticas-fuentes/encuestas/encuesta-nacional-ingresos-gastos-los-hogares
- Dominican Republic: https://archivo.one.gob.do/encuestas/enigh
- Ecuador: https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/index.xhtml
- El Salvador: http://www.digestyc.gob.sv/index.php/temas/des/ehpm.html
- Guatemala: https://www.proyectoencovi.com/
- Mexico: https://www.inegi.org.mx/rnm/index.php/catalog/685
- Nicaragua: https://www.inide.gob.ni/Home/enmv
- Paraguay: https://www.ine.gov.py/microdatos/microdatos.php
- Peru: https://www.datosabiertos.gob.pe/dataset/encuesta-nacional-de-hogares-enaho-2019-instituto-nacional-de-estad%C3%ADstica-e-inform%C3%A1tica-inei
- Uruguay: https://www.ine.gub.uy/web/guest/encuesta-de-gastos-e-ingresos-de-los-hogares-2016

## Supplementary data and matching tables

Supplementary data and matching tables are accessible via a stable online repository at Zenodo:
This repository includes matching tables for each country, computed sectoral carbon_intensities from GTAP and supplementary information, such as exchange rates and consumer prices indices used for this analysis.

# Code:

- Cleaning_X contains code for each country-specific household data. Outputs are fed into CPI_1_Cleaning_Transformation_Model_LCA.R
- CPI_1_Cleaning_Transformion_Model_LCA.R includes code to homogenize household data and compute the carbon pricing incidence of households.
- CPI_2_Graphical_Output_LCA.R includes code to produce all figures shown in the main body of the text and the supplementary information appendix.
- CPI_3_Econometric_Analysis_LCA.R includes code to produce all tables and statistical analysis, mostly shown in the supplementary information appendix.

## Contact
- Leonard Missbach, missbach@mcc-berlin.net
