#' Process data
#'
#' @param expo exposure variable
#' @param med mediator variable
#' @param tvar time-varying variable
#' @param lag number of lag
#' @param outc outcome variable
#' @param time time variable
#' @param norev non-reversible variable (among expo,med,tvar)
#' @param data input data
#' @param basec time-fixed variables
#' @param tvar_to_med time-varying varible to mediator
#' @param cont_exp continuous exposure
#' @param cont_exp_std standardize the continuous exposure
#' @param sp_list splines list
#' @param sp_type splines type
#' @param sp_df spines degree of freedom
#'
#' @importFrom purrr keep
#' @importFrom dplyr mutate group_by select starts_with
#' @importFrom stringr str_starts
#' @importFrom rlang := sym
#'
#' @return
#' a data frame with lag of exposure, mediator, time-varying variable
#' @export
process_data <- function(basec, expo, med, tvar, lag, outc, time,
                         norev = NULL, tvar_to_med = FALSE,
                         cont_exp = FALSE, cont_exp_std = FALSE,
                         sp_list = NULL,
                         sp_type = NULL,
                         sp_df = NULL, data) {

  ## detect which variables are non-reversible
  if(length(which(expo %in% norev)) != 0){
    norev_expo <- paste0("A",which(expo %in% norev))
  } else{
    norev_expo <- NULL
  }

  if(length(which(med %in% norev)) != 0){
    norev_med <- paste0("M",which(med %in% norev))
  } else {
    norev_med <- NULL
  }

  if(length(which(tvar %in% norev)) != 0){
    norev_tvar <- paste0("L",which(tvar %in% norev))
  } else {
    norev_tvar <- NULL
  }

  norev_var <- c(norev_expo,norev_med,norev_tvar)


  sp_var <- NULL

  ## detect which variables in splines list
  ### time-fixed var
  if(length(which(sp_list %in% basec)) != 0){
    sp_fix <- paste0("v",which(basec %in% sp_list))
  } else{
    sp_fix <- NULL
  }

  sp_var[which(sp_list %in% basec)] <- sp_fix

  ## exposure var
  if(length(which(sp_list %in% expo)) != 0){
    sp_expo <- paste0("A",which(expo %in% sp_list))
  } else{
    sp_expo <- NULL
  }

  sp_var[which(sp_list %in% expo)] <- sp_expo

  ## mediator var
  if(length(which(sp_list %in% med)) != 0){
    sp_med <- paste0("M",which(med %in% sp_list))
  } else {
    sp_med <- NULL
  }

  sp_var[which(sp_list %in% med)] <- sp_med

  ## time-varying var
  if(length(which(sp_list %in% tvar)) != 0){
    sp_tvar <- paste0("L",which(tvar %in% sp_list))
  } else {
    sp_tvar <- NULL
  }

  sp_var[which(sp_list %in% tvar)] <- sp_tvar

  ## time var
  if(length(which(sp_list %in% time)) != 0){
    sp_time <- paste0("j")
  } else{
    sp_time <- NULL
  }

  sp_var[which(sp_list %in% time)] <- sp_time

  ## continous exposure
  if(cont_exp == F & cont_exp_std == T){
    stop("standardize only applicable when continuous exposure was TRUE, default is FALSE")
  } else if (cont_exp == F & cont_exp_std == F){
    ## binary exposure
    expo_mean <- 0
  } else if (cont_exp == T & cont_exp_std == F){
    ## continuous exposure without standardize
    expo_mean <- mean(data[,expo])
  } else if (cont_exp == T & cont_exp_std == T){
    ## continuous exposure with standardize
    expo_mean <- 0
    data[,expo] <- as.numeric(scale(data[,expo]))
  }


  ## column names of time-fixed variables
  name_v <- paste0("v",1:length(basec))

  out <- data.frame(id = data$id) |>
    mutate(data[,basec]) |>
    magrittr::set_colnames(c("id",name_v))

  ## column names of exposure variables
  name_e <- paste0("A",1:length(expo))

  ## column names of mediator variables
  name_me <- paste0("M",1:length(med))

  ## column names of time-varying variables
  name_tvar <- paste0("L",1:length(tvar))


  for (i in 1:lag){

    ## column name of lag effect on exposure variable
    co_ex <- paste0(name_e,"l",i)
    co_me <- paste0(name_me,"l",i)
    co_tvar <- paste0(name_tvar,"l",i)

    for (k in 1:length(name_e)){

      out[,name_e[k]] <- data[,expo[k]]
      name_ep <- {{co_ex}}[k]
      out <- out |>
        group_by(id) |>
        mutate(
          {{name_ep}} := lag(!!sym(name_e[k]),n=i,default = data[1,expo[k]])
        )

    }

    for (k in 1:length(name_me)){

      out[,name_me[k]] <- data[,med[k]]
      name_med <- {{co_me}}[k]
      out <- out |>
        group_by(id) |>
        mutate(
          {{name_med}} := lag(!!sym(name_me[k]),n=i,default = data[1,med[k]])
        )

    }

    for (k in 1:length(name_tvar)){

      out[,name_tvar[k]] <- data[,tvar[k]]
      name_tv <- {{co_tvar}}[k]
      out <- out |>
        group_by(id) |>
        mutate(
          {{name_tv}} := lag(!!sym(name_tvar[k]),n=i,default = data[1,tvar[k]])
        )

    }


  }

  ## outcome variable
  out$Y <- data[,outc]

  ## time variable
  out$j <- data[,time]


  kq <- list()
  kq$df <- out |> data.frame()
  kq$norev_var <- norev_var
  kq$am <-  expo_mean

  ## column name

  eps <- kq$df |> select(starts_with("A")) |> colnames()
  tf <- kq$df |> select(starts_with("v")) |> colnames()
  tva <- kq$df |> select(starts_with("L")) |> colnames()
  mediator <- kq$df |> select(starts_with("M")) |> colnames()
  outcome <- kq$df |> select(starts_with("Y")) |> colnames()
  timee <- kq$df |> select(starts_with("j")) |> colnames()


  ### exposure variable
  if (any(eps %in% sp_var)){
    ep_sp <- sp_type[which(sp_var %in% eps)]
    ep_df <- sp_df[which(sp_var %in% eps)]
    eps <- paste0("splines::",ep_sp,"(",eps,",df=",ep_df,")")
  }

  ### time-fixed variables
  if (any(tf %in% sp_var)){
    tf_sp <- sp_type[which(sp_var %in% tf)]
    tf_df <- sp_df[which(sp_var %in% tf)]
    tf_v <- keep(tf, ~ any(str_starts(.x, sp_var)))
    tf <- paste0("splines::",tf_sp,"(",tf_v,",df=",tf_df,")")
  }

  ### mediator variables
  if (any(mediator %in% sp_var)){
    mediator_sp <- sp_type[which(sp_var %in% mediator)]
    mediator_df <- sp_df[which(sp_var %in% mediator)]
    mediator_v <- keep(tva, ~ any(str_starts(.x, sp_var)))
    mediator <- paste0("splines::",mediator_sp,"(",mediator_v,",df=",mediator_df,")")
  }

  ### time-varying variables
  if (any(tva %in% sp_var)){
    tva_sp <- sp_type[which(sp_var %in% tva)]
    tva_df <- sp_df[which(sp_var %in% tva)]
    tva_v <- keep(tva, ~ any(str_starts(.x, sp_var)))
    tva <- paste0("splines::",tva_sp,"(",tva_v,",df=",tva_df,")")
  }

  ### time variables
  if (any(timee %in% sp_var)){
    timee_sp <- sp_type[which(sp_var %in% timee)]
    timee_df <- sp_df[which(sp_var %in% timee)]
    timee <- paste0("splines::",timee_sp,"(j,df=",timee_df,")")
  } else {
    timee <- paste0("j")
  }


  ## formula for M(t)
  l_tm1 <- tva[!tva %in% name_tvar]
  m_tm1 <- mediator[!mediator %in% name_me]

  if (tvar_to_med == FALSE){
    formula_mt <- list()

    for (i in 1:length(med)){
      formula_mt[i] <- paste(name_me[i],"~",paste(c(eps,m_tm1,l_tm1,tf,timee),collapse = " + "))
    }

    kq$fm <- formula_mt

    ## formula for L(t)
    formular_lt <- list()

    for (i in 1:length(tvar)){
      formular_lt[i] <- paste(name_tvar[i],"~",paste(c(eps,mediator,l_tm1,tf,timee),collapse = " + "))
    }

    kq$fl <- formular_lt

    ## formula for Y(t)
    formular_y <- paste(outcome,"~",paste(c(eps,mediator,tva,tf,timee),collapse = " + "))
    kq$fy <- formular_y

  } else {
    formula_mt <- list()

    for (i in 1:length(med)){
      formula_mt[i] <- paste(name_me[i],"~",paste(c(eps,m_tm1,name_tvar,l_tm1,tf,timee),collapse = " + "))
    }

    kq$fm <- formula_mt

    ## formula for L(t)
    formular_lt <- list()

    for (i in 1:length(tvar)){
      formular_lt[i] <- paste(name_tvar[i],"~",paste(c(eps,m_tm1,l_tm1,tf,timee),collapse = " + "))
    }

    kq$fl <- formular_lt

    ## formula for Y(t)
    formular_y <- paste(outcome,"~",paste(c(eps,tva,mediator,tf,timee),collapse = " + "))
    kq$fy <- formular_y
  }

  return(kq)
}

