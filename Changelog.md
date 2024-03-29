# 0.2.3.0 -- 2023-05-11

* Support GHC 9.2 and 9.4
* Support `text-2.0`

# 0.2.2.0 -- 2022-03-21

* Support GHC 9.0

# 0.2.1.0 -- 2020-06-18

* Support GHC 8.10

# 0.2.0.0 -- 2019-11-29

* Support GHC 8.8
* Improve FastDownward.Exec to support predefining evaluators
* Added FastDownward.Exec.bjqlp, which is a good starting point for configuring
  Fast Downward.
* Add FastDownward.requiresAxioms, to see if a Test requires a search engine
  with axiom support.
* Rewrite the `Effect` type to be considerably faster. The new implementation
  uses continuations to minimally compute the set of all concrete effects.
* Enforce `writeVar v >=> readVar v === return`

# 0.1.1.0 -- 2019-01-09

* Support `containers-0.6`.
* Added `instance MonadFail Effect`
* Bumped `base` upper bound to `< 4.13`
* Bumped `list-t` lower bound to `>= 1.0.2` (this is necessary for internal
  reasons related to generalised newtype deriving)

# 0.1.0.1 -- 2019-01-02

* Removed a stray `putStrLn`.

# 0.1.0.0

* Initial release!
