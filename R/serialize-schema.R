# Structural validation precedes native constructors. The installed schemas are
# paired with a generated native contract, so validation never fetches a URL. Frame-dependent semantics remain in the existing design/frame gates.

#' @noRd
check_document_versions <- function(payload, path = "$", call = caller_env()) {
  if (!is.list(payload) || is_null(names(payload))) {
    abort_samplyr("Design file must contain a JSON object at {path}.",
                  class = "samplyr_error_design_file_malformed", call = call)
  }
  format <- payload$format
  formats <- c("samplyr/design", "samplyr/frame-stack", "samplyr/shared-sample")
  if (!is_scalar_string(format) || !format %in% formats) {
    abort_samplyr("This is not a samplyr design file at {path}: unknown format.",
                  class = "samplyr_error_design_file_malformed", call = call)
  }
  expected <- switch(format, "samplyr/design" = design_format_version,
                    "samplyr/frame-stack" = frame_stack_format_version,
                    "samplyr/shared-sample" = shared_sample_format_version)
  version <- payload$format_version
  if (!is.numeric(version) || length(version) != 1 || is.na(version) ||
      version != expected) {
    abort_samplyr("The format version at {path}.format_version is not supported.",
                  class = "samplyr_error_design_file_unsupported", call = call)
  }
  if (length(payload$required_extensions) > 0) {
    abort_samplyr(c(
      "Unsupported required extensions at {path}.required_extensions.",
      "i" = "This reader implements no executable extensions. Metadata in
             tools or annotations cannot change the sampling design."
    ), class = "samplyr_error_design_file_unsupported", call = call)
  }
  if (identical(format, "samplyr/design")) {
    if (!is.list(payload$design) || !is.list(payload$design$stages)) {
      abort_samplyr("Design file has no {path}.design.stages entry.",
                    class = "samplyr_error_design_file_malformed", call = call)
    }
    execution <- payload$execution
    if (is.list(execution) && !is_null(execution$panel_assignment)) {
      prepare_panel_record(execution$panel_assignment, "A design file", call = call)
    }
    schema <- payload$schema
    vocabulary <- if (is.list(schema)) schema$method_vocabulary else NULL
    if (is.list(vocabulary)) {
      v <- vocabulary$version
      if (!identical(vocabulary$id, method_vocabulary_id) ||
          !is.numeric(v) || length(v) != 1 || is.na(v) ||
          v != method_vocabulary_version) {
        abort_samplyr("Unsupported sampling method vocabulary at {path}.schema.method_vocabulary.",
                      class = "samplyr_error_design_file_unsupported", call = call)
      }
    }
  } else if (identical(format, "samplyr/frame-stack") && is.list(payload$components)) {
    for (i in seq_along(payload$components)) {
      check_document_versions(payload$components[[i]],
                              paste0(path, ".components[", i - 1L, "]"), call)
    }
  } else if (identical(format, "samplyr/shared-sample") && is.list(payload$source)) {
    check_document_versions(payload$source, paste0(path, ".source"), call)
    transformation <- payload$transformation
    if (is.list(transformation) &&
        (!identical(transformation$algorithm, weight_share_record_algorithm) ||
         !identical(as.character(transformation$version), "1"))) {
      abort_samplyr("Unsupported transformation algorithm or version at {path}.transformation.",
                    class = "samplyr_error_weight_share_record_unsupported", call = call)
    }
  }
  invisible(NULL)
}

#' @noRd
check_json_duplicate_keys <- function(x, path = "$", call = caller_env(),
                                      context = "document") {
  if (!is.list(x)) return(invisible(NULL))
  nms <- names(x)
  if (!is_null(nms) && anyDuplicated(nms)) {
    key <- nms[anyDuplicated(nms)]
    abort_samplyr("Duplicate JSON key at {path}: {.val {key}}.",
                  class = "samplyr_error_design_file_malformed", call = call)
  }
  for (i in seq_along(x)) {
    key <- if (is_null(nms)) "" else nms[i]
    # Digest tables have their own validator. Restrict the exemption to
    # document execution receipts, including nested component/source designs.
    # Duplicate frame_digest fields in the receipt itself still fail above.
    if (context == "execution" && key == "frame_digest") next
    child_context <- "other"
    if (context == "document") {
      if (identical(x$format, "samplyr/design") && key == "execution") {
        child_context <- "execution"
      } else if (identical(x$format, "samplyr/frame-stack") && key == "components") {
        child_context <- "components"
      } else if (identical(x$format, "samplyr/shared-sample") && key == "source") {
        child_context <- "document"
      }
    } else if (context == "components" && is_null(nms)) {
      child_context <- "document"
    }
    child <- if (is_null(nms)) paste0(path, "[", i - 1L, "]") else paste0(path, ".", nms[i])
    check_json_duplicate_keys(x[[i]], child, call, context = child_context)
  }
  invisible(NULL)
}

#' @noRd
validate_design_document <- function(payload, call = caller_env()) {
  check_json_duplicate_keys(payload, call = call)
  check_document_versions(payload, call = call)
  errors <- serialization_contract_errors(payload)
  if (length(errors)) {
    field <- vapply(errors, `[[`, character(1), "path")
    details <- vapply(errors, function(e) paste0("$", e$path, ": ", e$message),
                      character(1))
    details <- utils::head(unique(details), 5L)
    classes <- "samplyr_error_design_file_malformed"
    if (any(grepl("/draw/(n|frac)(/|$)", field))) {
      classes <- c(classes, "samplyr_error_alloc_invalid_input_type")
    }
    if (any(grepl("/execution/environment/rng(/|$)", field))) {
      classes <- c(classes, "samplyr_error_replay_rng")
    }
    abort_samplyr(c(
      "Design file contains invalid or contradictory properties.",
      "x" = paste(details, collapse = "\n")
    ), class = classes, call = call)
  }
  invisible(payload)
}

# A restored wrapper has no frame/link data to embed. Re-encode its declarative
# arguments and its current source designs, preserving namespaced annotations.
#' @noRd
restored_document_payload <- function(x, frame, frame_label, fn_name,
                                      call = caller_env()) {
  payload <- attr(x, "wrapper_document")
  if (is_shared_sample_design(x)) {
    spec <- attr(x, "transformation")
    payload$transformation <- encode_weight_share_call(list(
      algorithm = weight_share_record_algorithm, version = 1L,
      call = spec, source_integrity = spec$source_integrity,
      result_integrity = spec$result_integrity
    ))
    source <- structure(x, class = setdiff(class(x), "shared_sample_design"))
    attr(source, "wrapper_document") <- NULL
    attr(source, "transformation") <- NULL
    payload$source <- design_payload(source, frame, frame_label, fn_name, call)
  } else {
    frames <- frame_stack_component_frames(frame, names(x), fn_name, call = call)
    payload$key <- attr(x, "key")
    payload$overlaps <- encode_overlap_spec(attr(x, "overlaps"))
    membership <- attr(x, "membership")
    payload$components <- lapply(names(x), function(name) {
      c(list(name = name, membership = unname(membership[[name]])),
        design_payload(x[[name]], frames[[name]], name, fn_name, call))
    })
  }
  payload
}
