pg_connexion_modal <- function(failed = FALSE) {
  modalDialog(
    tags$h2('Login for PostgreSQL (Production)'),
    textInput('username_pg', 'Username',value = system("whoami", intern = TRUE)),
    passwordInput('password_pg', 'Password'),
    if (failed)
      div(tags$b("Login incorrect", style = "color: red;")),
    footer=tagList(
      actionButton('submit_pg_login', 'Submit'),
      modalButton('cancel')
    )
  )
}
