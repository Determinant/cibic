TODO
====

- More detailed checking in regex for:

  - char constant (done)
  - string constant (done)

- Fix:

  - check global definition (if type is complete) when semantic analysis finishes
  - local function declaration is not in a local scope (external linkage issue)
  - incomplete type issues
    - function **definition** requires complete return type (function declaration does not)
    - array requires **complete** elem type
    - struct or union requires **complete** fields ( ``struct A;`` vs. ``struct A a;`` ?)
    - pointer may **allow incomplete** type
  - calculate type memory footprint when complete type is required
  - function to 'pointer to function' conversion (according the std 6.3.2/4)

- Not Implemented:

  - complex type name (to be in agreement with complex decl) (almost done)
  - initializer checking
  - typedef support (via adding mid-rule actions to bision to inform flex)
