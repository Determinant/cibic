TODO
====

- More detailed checking in regex for:

  - char constant (done)
  - string constant (done)

- Fix:

  - check global definition (if type is complete) when semantic analysis finishes
  - local function declaration is not in a local scope (external linkage issue) (will not be fixed)
  - incomplete type issues
    - function **definition** requires complete return type (function declaration does not) (done)
    - array requires **complete** elem type (done)
    - struct or union requires **complete** fields (done)
    - pointer may **allow incomplete** type (done)
  - calculate type memory footprint at proper time
  - function to 'pointer to function' conversion (according the std 6.3.2/4) (done)
  - vague var table management (done)

- Not Implemented:

  - complex type name (to be in agreement with complex decl) (done)
  - initializer checking (done)
  - typedef support (via adding mid-rule actions to bision to inform flex) (done)
