#' @export
vc.r <- function(expr) {
  mock_network_functions(
    eval.parent(substitute(expr))
  )
}

network_function_registry <- local({
  registry <- list()
  list(
    empty = function() { length(registry) == 0 },
    init = function() {
      registry$`httr:::perform` <<- duplicate(httr:::perform)
    },
    get = function(key) {
      registry[[key]]
    }
  )
})

mock_network_functions <- function(expr) {
  setup_network_mocks()
  eval.parent(substitute({
    testthatsomemore::package_stub("httr", "perform", mock_network_function("httr:::perform"),
      expr
  )}))  
}

setup_network_mocks <- function() {
  if (network_function_registry$empty()) {
    network_function_registry$init()
  }
}

mock_network_function <- function(fn) {
  # TODO: (RK) Support non-standard evaluation, argument preservation,
  # environment preservation, etc.?
  eval(bquote(
    function(...) {
      network_request(.(fn), list(...))
    }
  ))
}


network_request <- local({
  # TODO: (RK) Replace with package currently being tested.
  network_requests <- NULL

  function(fn, args) {
    if (is.null(network_requests)) {
      network_requests <<- director::registry$new(
        file.path(system.file(package = "vcr"), file.path("tests", "net_data"))
      )
    }

    key <- digest::digest(list(fn, args))
    result <- network_requests$get(key)
    if (is.null(result)) {
      result <- do.call(network_function_registry$get(fn), args)
      network_requests$set(key, result)
    }
    network_requests$get(key)
  }
})

