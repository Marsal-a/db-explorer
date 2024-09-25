library(arrow,lib.loc = "~/R/x86_64-pc-linux-gnu-library/4.1_20240806_115852/")
library(dbplyr)
library(dplyr)
packageVersion("arrow")

nz <- connectNzSDSE()



DCAPPI_CONDA <- tbl(nz, in_schema(sql("DTL_APPI."), "DCAPPI_CONDA"))
DWAPPI_TRF2_PEC <- tbl(nz, in_schema(sql("DTL_APPI."), "DWAPPI_TRF2_PEC"))
DPAPPI_ELEMENT_STRUCTURE_202202 <- tbl(nz, in_schema(sql("DTL_APPI."), "DPAPPI_ELEMENT_STRUCTURE_202202"))
P_AFFECTATIONMUTATION_202208 <- tbl(nz, in_schema(sql("DTL_GENESIS."), "P_AFFECTATIONMUTATION_202208"))
P_CHANGEMENTCATEGORIEPENA_2021 <- tbl(nz, in_schema(sql("DTL_GENESIS."), "P_CHANGEMENTCATEGORIEPENA_2021"))
P_CPUPARTICIPANT_2021 <- tbl(nz, in_schema(sql("DTL_GENESIS."), "P_CPUPARTICIPANT_2021"))

list_table=list("DCAPPI_CONDA"=DCAPPI_CONDA,
             "DWAPPI_TRF2_PEC"=DWAPPI_TRF2_PEC,
             "DPAPPI_ELEMENT_STRUCTURE_202202"=DPAPPI_ELEMENT_STRUCTURE_202202,
             "P_AFFECTATIONMUTATION_202208"=P_AFFECTATIONMUTATION_202208,
             "P_CHANGEMENTCATEGORIEPENA_2021"=P_CHANGEMENTCATEGORIEPENA_2021,
             "P_CPUPARTICIPANT_2021"=P_CPUPARTICIPANT_2021)

purrr::map(names(list_table),function(x){
  col = list_table[[x]] %>% collect() 
  col %>% write_parquet(
    sink = paste0("~/R_Commun/Adam/parquet_data/",x,".parquet")
  )
})

DCAPPI_CONDAc <- DCAPPI_CONDA %>% arrange(NATAFF_APPI) %>% collect()
DCAPPI_CONDAc %>% write_dataset(path="~/R_Commun/Adam/parquet_data/DCAPPI_CONDAc",
                                partitioning = "NATAFF_APPI",
                                format = "parquet")

