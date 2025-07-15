# Function to simulate data for Anova chapter

#SD in vaccine acceptance in vaccine.sav is 1.65633
simulate_data=function(Autonomy_avg=1,AutonomyHigh_avg=Autonomy_avg,AutonomyLow_avg=Autonomy_avg,
                       Control_avg=1,ControlHigh_avg=Control_avg,ControlLow_avg=Control_avg,
                       Neutral_avg=1,NeutralHigh_avg=Neutral_avg,NeutralLow_avg=Neutral_avg, 
                       Autonomy_sd=1.63920,AutonomyHigh_sd=Autonomy_sd,AutonomyLow_sd=Autonomy_sd,
                       Neutral_sd=1.47389,NeutralHigh_sd=Neutral_sd,NeutralLow_sd=Neutral_sd,
                       Control_sd=1.63033,ControlHigh_sd=Control_sd,ControlLow_sd=Control_sd,
                       seed = 123,N_per_lang_cond=50){
  set.seed(seed)
  
  d=tibble(
    lang_cond = rep(c("Neutral", "Autonomy", "Control"), each = N_per_lang_cond),
    health_literacy=rep(rep(c('high','low'),each= N_per_lang_cond/2), times = 3)
  ) %>% 
    mutate(
      vacc_acceptance = case_when(
        lang_cond == "Neutral" & health_literacy=='high'  ~ rnorm(n(), mean = NeutralHigh_avg, sd = NeutralHigh_sd),
        lang_cond == "Neutral" & health_literacy=='low'  ~ rnorm(n(), mean = NeutralLow_avg, sd = NeutralLow_sd),
        lang_cond == "Autonomy" & health_literacy=='high' ~ rnorm(n(), mean = AutonomyHigh_avg, sd = AutonomyHigh_sd),
        lang_cond == "Autonomy" & health_literacy=='low'~ rnorm(n(), mean = AutonomyLow_avg, sd = AutonomyLow_sd),
        lang_cond == "Control" &  health_literacy=='high' ~ rnorm(n(), mean = ControlHigh_avg, sd = ControlHigh_sd),
        lang_cond == "Control" & health_literacy=='low' ~ rnorm(n(), mean = ControlLow_avg, sd = ControlLow_sd)
      ),
      # keep values within the stated 1–10 bounds
      vacc_acceptance = pmin(pmax(vacc_acceptance, 1), 10)
    )
  return(d)
}
