# Changelog for futhask

## Unreleased changes
Change in the name of the Monad from `FT` to `Fut`. This is a breaking change, but in principle a simple search and replace should fix it. The motivation for the change is both that it is more descriptive, and the introduction of the transformer. `FutT` simply looks better then `FTT`, especially in names using CamelCase. The original name `FT` was inspired by `ST`, since they both use rigid type variables for similar purposes.