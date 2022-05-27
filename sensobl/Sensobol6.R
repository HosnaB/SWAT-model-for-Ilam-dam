library(SWATplusR)
library(hydroGOF)
library(tidyverse)
library(lubridate)
library(sensobol)

pathIlam <- "./Ilam2/Scenarios/Default/TxtInOut/"
q_obs_sarjoy <- read.csv("./Qsarjoy_final.csv", header = TRUE)
q_obs_sarjoy$date <- as.Date(q_obs_sarjoy$date, format = "%Y-%m-%d")
##################### Global sensitivity by SOBOL
start_date <- ymd("2000-01-01")
end_date <- ymd("2017-01-01")
q_obs_sarjoy <- q_obs_sarjoy %>% filter(date >= start_date & date < end_date)
q_obs <- q_obs_sarjoy %>% filter(date <= ymd("2015-01-01") | date >= ymd("2015-09-01"))

par_bound <- tibble("CN2.mgt | change = pctchg" = c(-64, 11),
                    "ESCO.hru | change = absval" = c(0, 1),
                    "EPCO.hru | change = absval" = c(0, 1),
                    "CANMX.hru | change = absval" = c(2.8, 4.8),
                    "ALPHA_BF.gw | change = absval" = c(0, 1),
                    "OV_N.hru | change = absval" = c(2.8, 4.8),
                    "SURLAG.bsn | change = absval" = c(0, 10),
                    "SOL_K.sol | change = pctchg" = c(-50, 50),
                    "SOL_AWC.sol | change = pctchg" = c(-50, 50),
                    "SOL_Z.sol | change = pctchg" = c(-50, 50),
                    "SOL_BD.sol | change = pctchg" = c(-50, 50),
                    "SLSOIL.hru | change = absval" = c(0, 150),
                    "SLSUBBSN.hru | change = absval" = c(9.46, 91.46),
                    "CH_K2.rte | change = abschg" = c(0, 2.5),
                    "GW_DELAY.gw | change = absval" = c(0, 100),
                    "REVAPMN.gw | change = absval" = c(0, 500),
                    "GW_REVAP.gw | change = absval" = c(0.02, 0.2),
                    "FFCB.bsn | change = absval" = c(0, 1),
                    "GWQMN.gw | change = absval" = c(0, 1000),
                    "RCHRG_DP.gw | change = absval" = c(0, 1),
                    "MSK_CO1.bsn | change = absval" = c(0, 10),
                    "MSK_CO2.bsn | change = absval" = c(0, 10),
                    "DDRAIN.mgt | change = absval" = c(0, 200),
                    "SOL_ALB.sol | change = pctchg" = c(-3, 20),
                    "HRU_SLP.hru | change = pctchg" = c(0, 20),
                    "SOL_CBN.sol | change = pctchg" = c(-3, 20))

par_names <- names(par_bound)

swat_sobol <- function(par) {
  names(par) <- par_names
  q_sim <- run_swat2012(project_path = pathIlam,
                        output = define_output(file = "rch",
                                               variable = "FLOW_OUT",
                                               unit = 31),
                        parameter = par,
                        start_date = "1997-01-01",
                        end_date = "2016-12-31",
                        years_skip = 3,
                        n_thread = 96)
  q_cal <- q_sim$simulation$FLOW_OUT %>% filter(date <= ymd("2015-01-01") | date >= ymd("2015-09-01"))
  nse_q <- map_dbl(select(q_cal, -date), ~ NSE(.x, q_obs$Flow_obs))
  return(nse_q)
}

mat <- read.csv("./data_part6.csv")

y <- swat_sobol(mat[2:27])

write.csv(y, file = "./result_part6.csv")
