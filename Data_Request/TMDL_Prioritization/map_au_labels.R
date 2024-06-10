library(readr)
au_parameters <- read_csv("~/Documents/GitHub/Data_Request/TMDL_Prioritization/UTAHDWQ_parameters_11464.csv")
au_parameters=au_parameters[au_parameters$PARAMETER_ATTAINMENT=="Not meeting criteria",]

au_parameters1= au_parameters%>%
  select(-ASSESSMENT_UNIT_NAME,-PARAMETER_STATUS_NAME,-ATTAINMENT_CODE_NAME,-PARAMETER_ATTAINMENT,-USE_NAME)%>%
  rename(ASSESS_ID=ASSESSMENT_UNIT_ID)%>%
  unique(.)%>%
  mutate(PARAMETER_CODE_NAME= tools::toTitleCase(tolower(PARAMETER_CODE_NAME)),
        PARAMETER_CODE_NAME=gsub(",","",PARAMETER_CODE_NAME),
        PARAMETER_CODE_NAME=ifelse(PARAMETER_CODE_NAME=="Benthic Macroinvertebrates Bioassessments","Macroinvertebrates"
         ,ifelse(PARAMETER_CODE_NAME=="Escherichia Coli (E. Coli)","E. coli",
                 ifelse(PARAMETER_CODE_NAME=="Total Dissolved Solids (Tds)","TDS",
                        ifelse(PARAMETER_CODE_NAME=="Nitrate/Nitrite (Nitrite + Nitrate as n)","Nitrate as N", PARAMETER_CODE_NAME)))))


au_parameters2=au_parameters1%>%
  group_by(ASSESS_ID,EPA_PARAM_IR_CATEGORY_ID)%>%
  mutate(Params=paste(PARAMETER_CODE_NAME,collapse = ", "),
         EPA_PARAM_IR_CATEGORY_ID=ifelse(EPA_PARAM_IR_CATEGORY_ID=="4A","Cat_4A","Cat_5"))%>%
  select(-PARAMETER_CODE_NAME) %>%
  unique(.)%>%
  ungroup()%>%
  pivot_wider(id_cols = ASSESS_ID,names_from = EPA_PARAM_IR_CATEGORY_ID, values_from = Params,
    values_fn = list(Params = toString))  


au_poly1=wqTools::au_poly

au_poly_label=merge(au_poly1,au_parameters2,all.x=TRUE)



setwd("/Users/alanochoa/Documents/GitHub/Data_Request/TMDL_Prioritization")

save(au_poly_label,file="rps_map_label.Rdata")

