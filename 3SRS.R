# Create sampling frame

format_sampling_frame_3srs<-function(sframe,strata=NULL,col_psu,col_ssu,col_pop){
  sframe$id_sampl<-paste0("id_",rownames(sframe))
  if(is.null(strata)){
    sframe$strata_id <- "all"
  } else {
    sframe$strata_id <- sframe[[strata]]
  }
    sframe$psu_id <- sframe[[col_psu]]
    sframe$ssu_id <- sframe[[col_ssu]]

  sframe$pop_numbers <- sframe[[col_pop]]

  sumdist<-sframe %>% dplyr::group_by(strata_id) %>%  dplyr::summarise(SumDist = sum(pop_numbers,na.rm=T))
  sframe<-merge(sframe,sumdist,by="strata_id")
  proba<-as.numeric(sframe$pop_numbers)/as.numeric(sframe$SumDist)
  sframe<-cbind(sframe,proba)
  sframe<-sframe[!is.na(sframe$proba),]
  sframe<- sframe |> dplyr::select(id_sampl,strata_id,psu_id,ssu_id,pop_numbers)
  return(sframe)
}




# Calculate the sample size required for a given population proportion
#
# Parameters:
#   N: The population size
#   cl: The desired level of confidence (between 0 and 1)
#   p: The estimated population proportion (between 0 and 1)
#   E: The desired margin of error
#   rho1: The intra correlation coefficient at the PSU level
#   rho2: The intra correlation coefficient at the SSU level
#   m1: The mean number of SSU per PSU
#   m2: The mean number of final units per SSU
# Returns:
#   The sample size required to achieve the desired level of confidence and margin of error

Ssize_with_deff<-function (
  N,
  cl=0.95,
  p=0.5,
  E=0.05,
  rho1=0,
  rho2=0,
  m1=NULL,
  m2=NULL
) {
  n0 <- (qchisq(cl,1)*N*p*(1-p)) / (E^2*(N-1)+qchisq(cl,1)*p*(1-p))
  n1 <- n0*(1+(m1-1)*rho1+((m1*m2-1)*rho2))
  return(n1)
}

# Calculate the sample size required for a given population proportion
# sframe: A dataframe containing the sampling frame data.

create_targets<-function(sframe,strata_id, pop_numbers, buffer,
                          cl=0.95,
                          p=0.5,
                          E=0.05,
                          rho1=0,
                          rho2=0,
                          m1=0,
                          m2=0){
  sframe |> 
    dplyr::group_by({{strata_id}}) |> 
    dplyr::summarise(
      population = sum({{pop_numbers}},na.rm=T)
    ) |> 
    dplyr::mutate(
      target = 
        ceiling(Ssize_with_deff(population,cl=cl,p=p,E=E,rho1=rho1,rho2=rho2,m1=m1,m2=m2))
       |> as.numeric(),
      target.with.buffer = 
        as.numeric(ceiling(target * (1+buffer)))
    ) |> 
    dplyr::rename(strata_id={{strata_id}})
}

# Perform 3 stages random sampling
msrs_3stages <- function(sframe,dist,sampling_target,m1,m2
){
  Sys.sleep(0.25)
  dbr<-sframe[as.character(sframe$strata_id)==dist,]
  dbr<-dbr[dbr$pop_numbers>=cls,]
  out<-sample(as.character(dbr$id_sampl),ceiling(as.numeric(target*(1+buf))/cls),prob=dbr$proba,replace=TRUE)
}