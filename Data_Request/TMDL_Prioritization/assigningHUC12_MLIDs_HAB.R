#Load HAB data
hab_taxa1=read.csv("/Users/alanochoa/Documents/GitHub/Data_Request/TMDL_Prioritization/2023 Resolved HAB_Data_Compilation - Taxonomy_2002-22.csv")
hab_no_coordinate1=hab_taxa1[hab_taxa1$X_Long%in%c("#REF!","#N/A",""),]

hab_taxa_huc12=hab_taxa1[!hab_taxa1$X_Long%in%c("#REF!","#N/A",""),]
hab_taxa_huc12=hab_taxa_huc12%>%
  rename(LatitudeMeasure=Y_Lat,LongitudeMeasure=X_Long)
hab_taxa_huc12=assignPolys(hab_taxa_huc12,huc12_poly)

hab_taxa_huc12=hab_taxa_huc12%>%
  rename(Y_Lat=LatitudeMeasure,X_Long=LongitudeMeasure)


habs_list=list(hab_taxa_huc12,hab_no_coordinate1)
write.xlsx(habs_list,"HAB_Taxa_HUC12s.xlsx")

rm(hab_taxa,hab_taxa1,hab_taxa_huc12,hab_no_coordinate,hab_no_coordinate1)