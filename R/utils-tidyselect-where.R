# `where()` is not an exported function but is used in tidy selection
# Suppress "no visible binding for global variable" when using `where()`
if (getRversion() >= "2.15.1") utils::globalVariables("where")
