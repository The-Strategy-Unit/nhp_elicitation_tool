Research into alcohol related mortality and admissions suggests there are a number of admissions that are attributable to excess alcohol consumption which can ultimately lead to an emergency hospital admission or even death.

Such admissions could be avoided by adopting a range of interventions including public health strategies such as raising public awareness against the harms of excessive alcohol consumption or minimum pricing policies or patient specific interventions such as primary care support and increasing addiction treatment services.

Alcohol attributable condition codes are sourced from the 2020 PHE document [Alcohol-attributable fractions for England: An update][1]

[1]: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/958648/RELATI_1-1.pdf

These admissions have been grouped into 3 categories:

wholly attributable to alcohol consumption (100%)
partially attributable to alcohol consumption (Chronic conditions)
partially attributable to alcohol consumption (Acute conditions)

Whilst most activity mitigation strategies identify all spells based on the specified SQL coding for relevant conditions in this case the tool selects a proportion of spells based on the alcohol attributable fraction (AAF) for that condition. As an example the AAF for cancer of the oesophagus for males aged between 35 and 44 is 52% therefore the model randomly selects 52% of spells meeting these criteria. As such the data represents an estimate of the number of admissions that are caused by alcohol consumption. The AAFs are also sourced from the above referenced document. For those conditions that are wholly attributable the tool selects all admissions with the specified diagnoses as their AAF is 100%