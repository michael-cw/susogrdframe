## Suppress R CMD check NOTEs for data.table NSE column references and
## function-scope variables that R's static analysis cannot resolve.
##
## * layer, TOTPOP, stratum_numeric -- data.table column references used with
##   the := walrus operator or in [...] filter expressions in app_server.R.
## * maxLong -- local variable captured as a default argument in long2UTM()
##   inside project_to_utm(). The static analyser flags the standalone
##   long2UTM() definition that reuses it as a default outside its original
##   scope.
utils::globalVariables(c("layer", "maxLong", "stratum_numeric", "TOTPOP"))
