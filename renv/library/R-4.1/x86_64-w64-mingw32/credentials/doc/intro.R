## ---- echo=FALSE--------------------------------------------------------------
has_git <- credentials:::has_git_cmd()
delete_git_config_on_exit <- !file.exists('~/.gitconfig')
credentials:::set_default_cred_helper()

library <- function(package){
  withCallingHandlers(base::library(credentials), 
                      packageStartupMessage = function(e) {
                        cat(e$message)
                        invokeRestart("muffleMessage")
                      })
}  

## -----------------------------------------------------------------------------
library(credentials)

## ---- eval=has_git------------------------------------------------------------
credential_helper_get()

## ---- echo=FALSE, eval=has_git------------------------------------------------
# This hack may not work on MacOS server where cred helper is osxkeychain 
# which always requires user interaction. Hence error=TRUE in the next block.
example <- list(protocol = "https", host = "example.com",
  username = "jeroen", password = "supersecret")
credential_approve(example)

## ----error=TRUE, eval=has_git-------------------------------------------------
library(credentials)
git_credential_ask('https://example.com')

## ---- echo=FALSE, eval=has_git------------------------------------------------
credential_reject(list(protocol = "https", host = "example.com"))

## ---- echo = FALSE------------------------------------------------------------
ssh_key_info <- function(){
  try({
    out <- credentials:::ssh_key_info(auto_keygen = FALSE)
    out$pubkey = paste(substring(out$pubkey, 1, 80), "...")
    out
  })
}

## -----------------------------------------------------------------------------
ssh_key_info()

## ----echo=FALSE---------------------------------------------------------------
if(isTRUE(delete_git_config_on_exit))
  unlink("~/.gitconfig")

