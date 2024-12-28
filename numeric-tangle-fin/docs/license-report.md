**NB**: This captures the licenses associated with a particular set of dependency versions. If your own build solves differently, it’s possible that the licenses may have changed, or even that the set of dependencies itself is different. Please make sure you run [`cabal-plan license-report`](https://hackage.haskell.org/package/cabal-plan) on your own components rather than assuming this is authoritative.

# Dependency License Report

Bold-faced **`package-name`**s denote standard libraries bundled with `ghc-9.10.1`.

## Direct dependencies of `numeric-tangle-fin:lib:numeric-tangle-fin`

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |
| --- | --- | --- | --- | --- |
| **`base`** | [`4.20.0.0`](http://hackage.haskell.org/package/base-4.20.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/base-4.20.0.0/src/LICENSE) | Core data structures and operations | *(core library)* |
| `fin` | [`0.3.2`](http://hackage.haskell.org/package/fin-0.3.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/fin-0.3.2/src/LICENSE) | Nat and Fin: peano naturals and finite numbers |  |
| `no-recursion` | [`0.3.0.0`](http://hackage.haskell.org/package/no-recursion-0.3.0.0) | [`(AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR AGPL-3.0-only OR LicenseRef-commercial)`](http://hackage.haskell.org/package/no-recursion-0.3.0.0/src/LICENSE) | A GHC plugin to remove support for recursion | `numeric-tangle` |
| `numeric-tangle` | [`0.0.1.0`](http://hackage.haskell.org/package/numeric-tangle-0.0.1.0) |  *MISSING* | *MISSING* |  |

## Indirect transitive dependencies

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |
| --- | --- | --- | --- | --- |
| `QuickCheck` | [`2.15.0.1`](http://hackage.haskell.org/package/QuickCheck-2.15.0.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/QuickCheck-2.15.0.1/src/LICENSE) | Automatic testing of Haskell programs | `fin` |
| **`array`** | [`0.5.7.0`](http://hackage.haskell.org/package/array-0.5.7.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/array-0.5.7.0/src/LICENSE) | Mutable and immutable arrays | `binary`, `containers`, `deepseq`, `ghc`, `ghci`, `stm`, `text` |
| **`binary`** | [`0.8.9.2`](http://hackage.haskell.org/package/binary-0.8.9.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/binary-0.8.9.2/src/LICENSE) | Binary serialisation for Haskell values using lazy ByteStrings | `ghc`, `ghc-boot`, `ghci`, `text` |
| `boring` | [`0.2.2`](http://hackage.haskell.org/package/boring-0.2.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/boring-0.2.2/src/LICENSE) | Boring and Absurd types | `dec`, `fin` |
| **`bytestring`** | [`0.12.1.0`](http://hackage.haskell.org/package/bytestring-0.12.1.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/bytestring-0.12.1.0/src/LICENSE) | Fast, compact, strict and lazy byte strings with a list interface | `binary`, `filepath`, `ghc`, `ghc-boot`, `ghci`, `hashable`, `os-string`, `random`, `text`, `unix` |
| **`containers`** | [`0.7`](http://hackage.haskell.org/package/containers-0.7) | [`BSD-3-Clause`](http://hackage.haskell.org/package/containers-0.7/src/LICENSE) | Assorted concrete container types | `QuickCheck`, `binary`, `ghc`, `ghc-boot`, `ghc-heap`, `ghci`, `hashable`, `hpc`, `universe-base` |
| `dec` | [`0.0.6`](http://hackage.haskell.org/package/dec-0.0.6) | [`BSD-3-Clause`](http://hackage.haskell.org/package/dec-0.0.6/src/LICENSE) | Decidable propositions. | `fin` |
| **`deepseq`** | [`1.5.0.0`](http://hackage.haskell.org/package/deepseq-1.5.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/deepseq-1.5.0.0/src/LICENSE) | Deep evaluation of data structures | `QuickCheck`, `bytestring`, `containers`, `filepath`, `fin`, `ghc`, `ghc-boot`, `ghci`, `hashable`, `hpc`, `os-string`, `pretty`, `process`, `random`, `some`, `splitmix`, `tagged`, `text`, `time` |
| **`directory`** | [`1.3.8.3`](http://hackage.haskell.org/package/directory-1.3.8.3) | [`BSD-3-Clause`](http://hackage.haskell.org/package/directory-1.3.8.3/src/LICENSE) | Platform-agnostic library for filesystem operations | `ghc`, `ghc-boot`, `hpc`, `process` |
| **`exceptions`** | [`0.10.7`](http://hackage.haskell.org/package/exceptions-0.10.7) | [`BSD-3-Clause`](http://hackage.haskell.org/package/exceptions-0.10.7/src/LICENSE) | Extensible optionally-pure exceptions | `filepath`, `ghc`, `os-string`, `semaphore-compat` |
| **`filepath`** | [`1.5.2.0`](http://hackage.haskell.org/package/filepath-1.5.2.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/filepath-1.5.2.0/src/LICENSE) | Library for manipulating FilePaths in a cross platform way. | `directory`, `ghc`, `ghc-boot`, `ghci`, `hashable`, `hpc`, `process`, `unix` |
| **`ghc`** | [`9.10.1`](http://hackage.haskell.org/package/ghc-9.10.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-9.10.1/src/LICENSE) | The GHC API | `no-recursion` |
| **`ghc-bignum`** | [`1.3`](http://hackage.haskell.org/package/ghc-bignum-1.3) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-bignum-1.3/src/LICENSE) | GHC BigNum library | `ghc-internal`, `hashable` |
| **`ghc-boot`** | [`9.10.1`](http://hackage.haskell.org/package/ghc-boot-9.10.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-boot-9.10.1/src/LICENSE) | Shared functionality between GHC and its boot libraries | `ghc`, `ghci` |
| **`ghc-boot-th`** | [`9.10.1`](http://hackage.haskell.org/package/ghc-boot-th-9.10.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-boot-th-9.10.1/src/LICENSE) | Shared functionality between GHC and the @template-haskell@ library | `ghc-boot`, `template-haskell` |
| **`ghc-heap`** | [`9.10.1`](http://hackage.haskell.org/package/ghc-heap-9.10.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-heap-9.10.1/src/LICENSE) | Functions for walking GHC's heap | `ghc`, `ghci` |
| **`ghc-internal`** | [`9.1001.0`](http://hackage.haskell.org/package/ghc-internal-9.1001.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-internal-9.1001.0/src/LICENSE) | Basic libraries | `base`, `ghc-heap` |
| **`ghc-platform`** | [`0.1.0.0`](http://hackage.haskell.org/package/ghc-platform-0.1.0.0) |  *MISSING* | *MISSING* | `ghc-boot` |
| **`ghc-prim`** | [`0.11.0`](http://hackage.haskell.org/package/ghc-prim-0.11.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-prim-0.11.0/src/LICENSE) | GHC primitives | *(core library)* |
| **`ghci`** | [`9.10.1`](http://hackage.haskell.org/package/ghci-9.10.1) |  *MISSING* | *MISSING* | `ghc` |
| `hashable` | [`1.5.0.0`](http://hackage.haskell.org/package/hashable-1.5.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/hashable-1.5.0.0/src/LICENSE) | A class for types that can be converted to a hash value | `fin` |
| **`hpc`** | [`0.7.0.1`](http://hackage.haskell.org/package/hpc-0.7.0.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/hpc-0.7.0.1/src/LICENSE) | Code Coverage Library for Haskell | `ghc` |
| **`mtl`** | [`2.3.1`](http://hackage.haskell.org/package/mtl-2.3.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/mtl-2.3.1/src/LICENSE) | Monad classes for transformers, using functional dependencies | `exceptions`, `random` |
| **`os-string`** | [`2.0.2`](http://hackage.haskell.org/package/os-string-2.0.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/os-string-2.0.2/src/LICENSE) | Library for manipulating Operating system strings. | `directory`, `filepath`, `hashable`, `unix` |
| **`pretty`** | [`1.1.3.6`](http://hackage.haskell.org/package/pretty-1.1.3.6) | [`BSD-3-Clause`](http://hackage.haskell.org/package/pretty-1.1.3.6/src/LICENSE) | Pretty-printing library | `template-haskell` |
| **`process`** | [`1.6.19.0`](http://hackage.haskell.org/package/process-1.6.19.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/process-1.6.19.0/src/LICENSE) | Process libraries | `ghc` |
| `random` | [`1.3.1`](http://hackage.haskell.org/package/random-1.3.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/random-1.3.1/src/LICENSE) | Pseudo-random number generation | `QuickCheck` |
| **`semaphore-compat`** | [`1.0.0`](http://hackage.haskell.org/package/semaphore-compat-1.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/semaphore-compat-1.0.0) | Cross-platform abstraction for system semaphores | `ghc` |
| `some` | [`1.0.6`](http://hackage.haskell.org/package/some-1.0.6) | [`BSD-3-Clause`](http://hackage.haskell.org/package/some-1.0.6/src/LICENSE) | Existential type: Some | `fin` |
| `splitmix` | [`0.1.3.1`](http://hackage.haskell.org/package/splitmix-0.1.3.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/splitmix-0.1.3.1/src/LICENSE) | Fast Splittable PRNG | `QuickCheck`, `random` |
| **`stm`** | [`2.5.3.1`](http://hackage.haskell.org/package/stm-2.5.3.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/stm-2.5.3.1/src/LICENSE) | Software Transactional Memory | `exceptions`, `ghc` |
| `tagged` | [`0.8.9`](http://hackage.haskell.org/package/tagged-0.8.9) | [`BSD-3-Clause`](http://hackage.haskell.org/package/tagged-0.8.9/src/LICENSE) | Haskell 98 phantom types to avoid unsafely passing dummy arguments | `boring`, `universe-base` |
| **`template-haskell`** | [`2.22.0.0`](http://hackage.haskell.org/package/template-haskell-2.22.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/template-haskell-2.22.0.0/src/LICENSE) | Support library for Template Haskell | `QuickCheck`, `bytestring`, `containers`, `exceptions`, `filepath`, `ghc`, `ghci`, `os-string`, `tagged`, `text` |
| **`text`** | [`2.1.1`](http://hackage.haskell.org/package/text-2.1.1) | [`BSD-2-Clause`](http://hackage.haskell.org/package/text-2.1.1/src/LICENSE) | An efficient packed Unicode text type. | `hashable` |
| **`time`** | [`1.12.2`](http://hackage.haskell.org/package/time-1.12.2) | [`BSD-2-Clause`](http://hackage.haskell.org/package/time-1.12.2/src/LICENSE) | A time library | `directory`, `ghc`, `hpc`, `unix` |
| **`transformers`** | [`0.6.1.1`](http://hackage.haskell.org/package/transformers-0.6.1.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/transformers-0.6.1.1/src/LICENSE) | Concrete functor and monad transformers | `QuickCheck`, `boring`, `exceptions`, `ghc`, `ghci`, `mtl`, `random`, `tagged`, `universe-base` |
| `universe-base` | [`1.1.4`](http://hackage.haskell.org/package/universe-base-1.1.4) | [`BSD-3-Clause`](http://hackage.haskell.org/package/universe-base-1.1.4/src/LICENSE) | A class for finite and recursively enumerable types. | `fin` |
| **`unix`** | [`2.8.5.1`](http://hackage.haskell.org/package/unix-2.8.5.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/unix-2.8.5.1/src/LICENSE) | POSIX functionality | `directory`, `ghc`, `ghc-boot`, `ghci`, `process`, `semaphore-compat` |

