connexion_modal <- function(failed = FALSE,title='Login for PostgreSQL (Production)') {
  modalDialog(
    tags$h2(title),
    textInput('modal_username', 'Username',value = system("whoami", intern = TRUE)),
    passwordInput('modal_pw', 'Password'),
    if (failed)
      div(tags$b("Login incorrect", style = "color: red;")),
    footer=tagList(
      actionButton('modal_submit_login', 'Submit'),
      modalButton('cancel')
    )
  )
}
