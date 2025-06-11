#' Process data
#'
#' @param fix time-fixed variable
#' @param expo exposure variable
#' @param med mediator variable
#' @param tvar time-varying variable
#' @param lag number of lag
#' @param outc outcome variable
#' @param time time variable
#' @param norev non-reversible variable (among expo,med,tvar)
#' @param LM L before M assumption
#' @param data input data
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate group_by select starts_with
#' @importFrom rlang := sym
#'
#' @return
#' a data frame with lag of exposure, mediator, time-varying variable
#' @export
process_data <- function(fix,expo,med,tvar,lag,outc,time,norev = NULL,LM = FALSE,data){

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

  ## column names of time-fixed variables
  name_v <- paste0("v",1:length(fix))

  out <- data.frame(id = data$id) %>%
    mutate(data[,fix]) %>%
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
      out <- out %>%
        group_by(id) %>%
        mutate(
          {{name_ep}} := lag(!!sym(name_e[k]),n=i,default = data[1,expo[k]])
        )

    }

    for (k in 1:length(name_me)){

      out[,name_me[k]] <- data[,med[k]]
      name_med <- {{co_me}}[k]
      out <- out %>%
        group_by(id) %>%
        mutate(
          {{name_med}} := lag(!!sym(name_me[k]),n=i,default = data[1,med[k]])
        )

    }

    for (k in 1:length(name_tvar)){

      out[,name_tvar[k]] <- data[,tvar[k]]
      name_tv <- {{co_tvar}}[k]
      out <- out %>%
        group_by(id) %>%
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
  kq$df <- out %>% data.frame()
  kq$norev_var <- norev_var

  ## column name

  eps <- kq$df %>% select(starts_with("A")) %>% colnames()
  tf <- kq$df %>% select(starts_with("v")) %>% colnames()
  tva <- kq$df %>% select(starts_with("L")) %>% colnames()
  mediator <- kq$df %>% select(starts_with("M")) %>% colnames()
  outcome <- kq$df %>% select(starts_with("Y")) %>% colnames()
  timee <- kq$df %>% select(starts_with("j")) %>% colnames()


  ## formula for M(t)
  l_tm1 <- tva[!tva %in% name_tvar]
  m_tm1 <- mediator[!mediator %in% name_me]

  if (LM == FALSE){
    formula_mt <- list()

    for (i in 1:length(med)){
      formula_mt[i] <- paste(name_me[i],"~",paste(c(eps,m_tm1,l_tm1,tf),collapse = " + "))
    }

    kq$fm <- formula_mt

    ## formula for L(t)
    formular_lt <- list()

    for (i in 1:length(tvar)){
      formular_lt[i] <- paste(name_tvar[i],"~",paste(c(eps,mediator,l_tm1,tf),collapse = " + "))
    }

    kq$fl <- formular_lt

    ## formula for Y(t)
    formular_y <- paste(outcome,"~",paste(c(eps,mediator,tva,tf),collapse = " + "))
    kq$fy <- formular_y

  } else {
    formula_mt <- list()

    for (i in 1:length(med)){
      formula_mt[i] <- paste(name_me[i],"~",paste(c(eps,m_tm1,name_tvar,l_tm1,tf),collapse = " + "))
    }

    kq$fm <- formula_mt

    ## formula for L(t)
    formular_lt <- list()

    for (i in 1:length(tvar)){
      formular_lt[i] <- paste(name_tvar[i],"~",paste(c(eps,m_tm1,l_tm1,tf),collapse = " + "))
    }

    kq$fl <- formular_lt

    ## formula for Y(t)
    formular_y <- paste(outcome,"~",paste(c(eps,tva,mediator,tf),collapse = " + "))
    kq$fy <- formular_y
  }

  return(kq)
}
