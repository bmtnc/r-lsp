# the reporter's likely shape: the function loads its own input, mutates, returns;
# its callers are a targets pipeline / script / exported API, so never followed
build_panel <- function() {
  load_panel() %>% dplyr::mutate(self_loaded_created = value * 10)
}
use_panel <- function(panel) {
  panel %>% dplyr::filter(self_loaded_created > 0)
}
