
#options(shiny.fullstacktrace = TRUE, shiny.error = write_error_logs)
# adapt for your purposes
write_error_logs <- function() {
  error <- recover_error()

  # cat error message to logfile
  logfile <- paste0(Sys.time(), '.shinylog')
  cat(error$message, '\n', file = logfile)

  # format the stack and append to logfile
  stack <- error$stack %>%
    dplyr::select(-category) %>%
    dplyr::mutate(num = paste0(num, ':')) %>%
    tidyr::unite(col = res, sep = ' ') %>%
    dplyr::pull(res)

  stack <- paste0(' ', stack, '\n')
  cat(stack, file = logfile, append = TRUE, sep = '')
}

# adapted from utils::recover
recover_error <- function ()  {

  # get calls
  calls <- sys.calls()
  from <- 0L

  # get frame previous to last stop() call
  n <- length(calls)
  for (i in rev(seq_len(n))) {

    calli <- calls[[i]]
    fname <- calli[[1L]]
    if ( "stop(e)" %in% deparse(calli)) {
      from <- i - 1
      break
    }
  }

  frame <- sys.frame(from)

  # write to logfile
  getError(frame$e)
}

# adapted from shiny::printError
getError <- function (cond,
                      full = get_devmode_option("shiny.fullstacktrace", FALSE),
                      offset = getOption("shiny.stacktraceoffset", TRUE)) {

  error_msg <- sprintf(
    "Error in %s: %s",
    shiny:::getCallNames(list(conditionCall(cond))),
    conditionMessage(cond)
  )

  should_drop <- !full
  should_strip <- !full
  should_prune <- !full

  stackTraceCalls <- c(
    attr(cond, "deep.stack.trace", exact = TRUE),
    list(attr(cond, "stack.trace", exact = TRUE))
  )

  stackTraceParents <- lapply(stackTraceCalls, attr, which = "parents", exact = TRUE)
  stackTraceCallNames <- lapply(stackTraceCalls, shiny:::getCallNames)
  stackTraceCalls <- lapply(stackTraceCalls, shiny:::offsetSrcrefs, offset = offset)

  # Use dropTrivialFrames logic to remove trailing bits (.handleSimpleError, h)
  if (should_drop) {
    # toKeep is a list of logical vectors, of which elements (stack frames) to keep
    toKeep <- lapply(stackTraceCallNames, shiny:::dropTrivialFrames)
    # We apply the list of logical vector indices to each data structure
    stackTraceCalls <- mapply(stackTraceCalls, FUN = `[`, toKeep, SIMPLIFY = FALSE)
    stackTraceCallNames <- mapply(stackTraceCallNames, FUN = `[`, toKeep, SIMPLIFY = FALSE)
    stackTraceParents <- mapply(stackTraceParents, FUN = `[`, toKeep, SIMPLIFY = FALSE)
  }

  delayedAssign("all_true", {
    # List of logical vectors that are all TRUE, the same shape as
    # stackTraceCallNames. Delay the evaluation so we don't create it unless
    # we need it, but if we need it twice then we don't pay to create it twice.
    lapply(stackTraceCallNames, function(st) {
      rep_len(TRUE, length(st))
    })
  })

  # stripStackTraces and lapply(stackTraceParents, pruneStackTrace) return lists
  # of logical vectors. Use mapply(FUN = `&`) to boolean-and each pair of the
  # logical vectors.
  toShow <- mapply(
    if (should_strip) shiny:::stripStackTraces(stackTraceCallNames) else all_true,
    if (should_prune) lapply(stackTraceParents, shiny:::pruneStackTrace) else all_true,
    FUN = `&`,
    SIMPLIFY = FALSE
  )

  dfs <- mapply(seq_along(stackTraceCalls), rev(stackTraceCalls), rev(stackTraceCallNames), rev(toShow), FUN = function(i, calls, nms, index) {
    data.frame(
      num = rev(which(index)),
      call = rev(nms[index]),
      loc = rev(shiny:::getLocs(calls[index])),
      category = rev(shiny:::getCallCategories(calls[index])),
      stringsAsFactors = FALSE
    )
  }, SIMPLIFY = FALSE)

  res <- list(
    message = error_msg,
    stack = dfs[[1]]
  )

  return(res)
}
