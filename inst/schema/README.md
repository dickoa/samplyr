# Serialization schemas and method vocabulary

This directory ships with samplyr. The format remains experimental and native
to samplyr. Structural validation is available outside R, but statistical
conformance across implementations has not yet been established.

## Documents

The three self-contained JSON Schemas use Draft 7 and contain no remote
references. They can be used offline with any compatible validator.

| Schema | Supported format versions |
|---|---|
| `design.schema.json` | `samplyr/design` 3 |
| `frame-stack.schema.json` | `samplyr/frame-stack` 1 |
| `shared-sample.schema.json` | `samplyr/shared-sample` 1 |

In R, locate a schema with
`system.file("schema", "design.schema.json", package = "samplyr")`.
`read_design()` parses JSON with `jsonlite` and validates it in native R before
creating design objects.
`write_design()` and `design_json()` validate their output before returning or
writing it. Runtime checks use `runtime-contract.json`, generated from the same
source as the published schemas. They never load a document-supplied schema or
fetch a URL. Duplicate object keys outside optional frame digest contents are
rejected before contract validation.
Neither `jsonvalidate` nor V8 is a package dependency, even optionally.

Schemas check document shape, field names, types, required fields and built-in
method descriptors. Native validation additionally checks relationships such
as unique component names and consistency between common and tool descriptors.
Frame-dependent checks remain in `validate_frame()` and `execute()`. A valid
JSON document is not evidence that an arbitrary frame can execute its design.

The optional `execution.frame_digest` is diagnostic data with its own native
version and validator. It is not an execution instruction and is intentionally
outside the document schema's structural checks and duplicate-key traversal,
including in nested component and source documents. This avoids a second walk
through potentially large per-unit tables. Duplicate `frame_digest` fields in
the enclosing execution object are still rejected. Its existing behavior remains:
unsupported digest versions are errors, while malformed optional diagnostics
are dropped with a warning. This exception does not apply to selection rules,
frame bindings, panel assignment, RNG settings or transformation arguments.

Legacy null encodings in optional fields can be `null`, an empty array or an
empty object. The writer emits `null` or omits absent fields. Nonempty values
must satisfy the declared type.

## Metadata and extensions

Executable objects reject unknown fields. For example, `draw.minn_n` is an
error, not an ignored spelling of `min_n`. Column names inside data tables and
maps are data and are not subject to the executable-field allowlist.

Optional top-level `annotations` and `tools` objects contain named namespaces:

```json
{
  "annotations": {
    "example.org": {"review_status": "approved"}
  },
  "tools": {
    "other_sampler": {"version": "1.0"}
  }
}
```

Each namespace is an object. Unknown namespaces are descriptive only and are
preserved when reading and writing all three formats, including restored
collection and shared-sample documents. `tools.samplyr` has a known schema
because native method and replay metadata are interpreted by samplyr.

An extension that affects execution must be listed in the top-level
`required_extensions` array of the affected document. No executable extensions
are implemented in this revision, so every nonempty requirement list is
refused, including in nested component or source documents. The schemas reserve
this field but allow only an empty list. There is no API for registering an
extension or silently substituting another method.

A future revision that implements executable extensions must also raise the
relevant format version. Earlier readers that ignore an unfamiliar requirement
field must not execute a document with new semantics. Descriptive metadata
alone does not require a format-version change.

## Method vocabulary

`sampling-methods-v2.json` is the maintained source of built-in descriptors.
The R dictionary reads it directly, and the schema generator derives method
constraints from it.

Documents declare vocabulary version 2, and readers refuse any other version.
Design format versions 1 and 2 and vocabulary version 1 were never released,
so there is no compatibility path for them.

| Field | Meaning |
|---|---|
| `id` | Canonical built-in method identifier |
| `family`, `algorithm` | Method family and algorithm identity |
| `replacement` | Without, with, or minimum replacement |
| `sample_size` | Fixed or random realized count |
| `probabilities` | Equal, unequal, or equal-or-unequal chances |
| `probability_quantity` | `inclusion_probability` or `expected_hits` |
| `probability_quality` | `exact`, `approximate`, or `unknown` |

For approximate methods, the quantity field identifies what the targets
approximate. It does not turn those targets into exact inclusion probabilities.
Quality describes the first-order quantity under the method contract, not
variance accuracy or confidence-interval coverage. SPS and Pareto are
approximate. WR and Chromy descriptors use expected hits. Custom methods carry
their author's declaration, which is not an independent statistical proof.

Built-in fields must agree with the registry. The quantity and quality fields
are required even without `tools.samplyr`. Contradictory common and native
quality declarations are refused. DDI entries are descriptive crosswalks,
not substitute algorithm definitions.

## Maintaining the schemas

The schemas and `runtime-contract.json` are generated, not edited by hand.
The generator and an independent parity check live in the samplyr source
repository and are not part of the installed package. Edit
`sampling-methods-v2.json` for current method descriptors and the generator
for structural rules, then regenerate and commit the outputs with their
sources. The native checker implements only this contract's fixed set of
operations. Generation refuses an unsupported schema keyword, so adding a new
constraint requires an explicit runtime implementation. It is not a general
JSON Schema validator and accepts no user-supplied schemas.

The parity check compares the native checker with a Draft 7 validator over
valid documents, field removals, type substitutions and contradictory method
descriptors. Python is needed only for regeneration and that check, not to
install, load, test or use samplyr in R. The package tests cover a frozen
version 3 document, malformed documents, metadata preservation and
vocabulary consistency.
