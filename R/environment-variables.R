#' How to Access Sensitive Information Non-Interactively with Environment Vars
#'
#' When writing code that might be shared, it's bad practice to hard-code
#' sensitive information in a script (or package, obviously). For data that
#' doesn't need to be shared, but doesn't necessarily need encryption,
#' environment variables provide a way to store and retrieve information.
#'
#' \code{\link[base:EnvVar]{Environment variables}} are just variables that are
#' available using the \code{\link[base:Sys.getenv]{Sys.getenv()}} function.
#' They're not specific to R; your Windows software uses a number of environment
#' variables to function. They're particularly useful for storing information
#' that you need to access from multiple places, or information that you don't
#' want to leave your computer, like API keys. You can also store usernames and
#' passwords in this file; it's better than entering them directly, but note
#' that they \strong{will not be encrypted}. A better solution for usernames,
#' passwords, and other things that should be encrypted is the
#' \href{https://github.com/r-lib/keyring}{keyring} package; however, the
#' coviData package does not currently follows this convention.
#'
#' To use environment variables, follow these steps:
#'
#' \enumerate{
#'
#'   \item Go to the Windows search bar and type "environment variable". You
#'   should see an option to "Edit environment variables for your account". Open
#'   it. \emph{(Note that the option "Edit the system environment variables"
#'   requires admin rights; you can't use it.)}
#'   \item You should see a popup box with two panes; "User variables for
#'   {your_username}" on top and "System variables" on bottom.
#'   \item Click "New..." under the "User variables..." section.
#'   \item This will open an input box with fields "Variable name" and
#'   "Variable value". These fields correspond to the variable name and value in
#'   R. Type the name and value of your variable in each field, and click "OK".
#'   \item The variable should now appear in the "User variables..." pane. You
#'   can click "OK" to exit the window.
#'   \item To see a new variable in R, you need to restart the sesson. Save your
#'   work, then go to Session -> Restart R in the top left corner of R Studio.
#'   Alternately, you can close R Studio and re-open it.
#'   \item Type `Sys.getenv("your_variable")` into the console and press
#'   `Enter`. R should return your variable value!
#' }
#'
#' \code{coviData} currently uses environment variables for REDcap API tokens,
#' but may expand to ESSENCE and NBS tokens as well in the future.
#'
#' @md
#'
#' @name env_vars
NULL
