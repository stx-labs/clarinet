## [0.28.2](https://github.com/hirosystems/clarity-repl/compare/v0.28.1...v0.28.2) (2022-05-26)

### Bug Fixes

- ability to interpret by passing an AST
  ([77164c8](https://github.com/hirosystems/clarity-repl/commit/77164c833478ae056f352925752bce7353cbdce1))

## [0.28.1](https://github.com/hirosystems/clarity-repl/compare/v0.28.0...v0.28.1) (2022-05-26)

### Bug Fixes

- any line of code that can not be tested
  ([7acff7b](https://github.com/hirosystems/clarity-repl/commit/7acff7b730ca6d399f8a436d4a13852882938d3f))
- fix crash on contract-call to invalid func
  ([de981a2](https://github.com/hirosystems/clarity-repl/commit/de981a27f8ab12823b3312d775dcd2584b4fc11b)),
  closes [#170](https://github.com/hirosystems/clarity-repl/issues/170)
- harden `ASTVisitor` with default args
  ([c0fcf7a](https://github.com/hirosystems/clarity-repl/commit/c0fcf7a0db2d0b9ae14707316895732d03fe2870)),
  closes [#173](https://github.com/hirosystems/clarity-repl/issues/173)
- remove panic from ASTVisitor
  ([315d169](https://github.com/hirosystems/clarity-repl/commit/315d169b29117486d52fc9f76d91b18ddef88532))

# [0.28.0](https://github.com/hirosystems/clarity-repl/compare/v0.27.0...v0.28.0) (2022-05-19)

### Bug Fixes

- address feedbacks
  ([da81a11](https://github.com/hirosystems/clarity-repl/commit/da81a1118016b0453d214fe71cd3f4f65ce7ae3b))
- check the maximum length of a contract name
  ([24cb554](https://github.com/hirosystems/clarity-repl/commit/24cb55437290883cae55089790473c541e4bb883)),
  closes [#145](https://github.com/hirosystems/clarity-repl/issues/145)
- **dap:** return a map's type for its value
  ([cc6a3a9](https://github.com/hirosystems/clarity-repl/commit/cc6a3a9494af5bbf19de8be87f443e671e8d0585))
- **dap:** track the stack frames on every expression
  ([a19fc65](https://github.com/hirosystems/clarity-repl/commit/a19fc6565e2479e8b51eb26c32aa003d730ba1cb))
- disable dap for wasm builds
  ([a60cdd4](https://github.com/hirosystems/clarity-repl/commit/a60cdd4fc428129b5201f8ed0378bacfe4e28063))
- harden `ASTVisitor` to handle invalid ASTs
  ([4da54ff](https://github.com/hirosystems/clarity-repl/commit/4da54ff19eba895bcbef05e54b1350d176d506aa)),
  closes [hirosystems/clarinet#334](https://github.com/stx-labs/clarinet/issues/334)
- make dep detector robust to invalid ASTs
  ([e179647](https://github.com/hirosystems/clarity-repl/commit/e179647d36f6102e31853eacc409ccc9818d8518))
- make TestCoverageReport attributes public
  ([41f98d7](https://github.com/hirosystems/clarity-repl/commit/41f98d7f9037fdd19f94ee9e223e9a3e61a4305b))
- resolve problem comparing paths in Windows
  ([e010141](https://github.com/hirosystems/clarity-repl/commit/e010141218f92c78ad820a6418c31ea30ef75965))
- update block limits to 2.05 numbers
  ([f66a0d7](https://github.com/hirosystems/clarity-repl/commit/f66a0d769fbf6429a51ec585751953ce06676237)),
  closes [#114](https://github.com/hirosystems/clarity-repl/issues/114)
- update tests for eval_hooks changes
  ([7b5c059](https://github.com/hirosystems/clarity-repl/commit/7b5c059ee0a676994f0ed7ff3695f862aea423aa))
- wasm build
  ([dea995d](https://github.com/hirosystems/clarity-repl/commit/dea995d734446f41ead737638fcc32c22d4d6503))

### Features

- add `::trace` command
  ([389eaef](https://github.com/hirosystems/clarity-repl/commit/389eaef67460d93c3c71beae0de5e1c09741c6e4))
- add emitted events to execution trace
  ([9f2a913](https://github.com/hirosystems/clarity-repl/commit/9f2a913100c41393710df383717fa9ad518ca9ea))
- add reload command
  ([e2f2e53](https://github.com/hirosystems/clarity-repl/commit/e2f2e53b52886b021ec295cc0499129dd219be47))
- better simulated block time
  ([9f5f0f8](https://github.com/hirosystems/clarity-repl/commit/9f5f0f808e695a3927174ca59ae5b235486cab0e)),
  closes [hirosystems/clarinet#310](https://github.com/stx-labs/clarinet/issues/310)
- **dap:** add support for watching variables
  ([7e750ea](https://github.com/hirosystems/clarity-repl/commit/7e750ea5006edbf2b84f925b0fa62f973ec5b7d4))
- **dap:** added `complete` method to `EvalHook`
  ([51ee196](https://github.com/hirosystems/clarity-repl/commit/51ee19699aeebf1b495e0c8f679b7f7f650febd0))
- **dap:** begin support for DAP debugger
  ([f133bee](https://github.com/hirosystems/clarity-repl/commit/f133bee333fef7e02c38c2f6f39d2de5ffebb0b4))
- **dap:** implement evaluate request
  ([9ccd742](https://github.com/hirosystems/clarity-repl/commit/9ccd742898ee7f727c23cdbeb3c423f70d89344c))
- **dap:** implement scopes and variables
  ([40bfde5](https://github.com/hirosystems/clarity-repl/commit/40bfde585f658c58c94684e245c762c1b929af0d))
- **dap:** stack traces and pause
  ([03611da](https://github.com/hirosystems/clarity-repl/commit/03611da2320add5120e7b534b8296d66d66e64a6))
- display digest
  ([69a48af](https://github.com/hirosystems/clarity-repl/commit/69a48af33dc69091b1f508a3d5514df89a6691d1))
- **reload:** do not clear `executed`
  ([86dac32](https://github.com/hirosystems/clarity-repl/commit/86dac32b62088c01e08b084fcb85c4467f6f14de))
- **reload:** have error reading a file being a catastrophic error
  ([60afeb8](https://github.com/hirosystems/clarity-repl/commit/60afeb87f1f3d0a515ba653fe2c58ee80dfe1012))
- **reload:** report error when file in path can not be reloaded
  ([9827021](https://github.com/hirosystems/clarity-repl/commit/9827021b62da84af09aed21f2699baa6ceb6e9b4))
- **reload:** use `std::mem::take` instead of `clone` and `clear`
  ([456239a](https://github.com/hirosystems/clarity-repl/commit/456239a27e8261c865650420c222c69facb07acf))
- return unresolved dependencies from detector
  ([5a9d122](https://github.com/hirosystems/clarity-repl/commit/5a9d122cb25281b45e8ebc42561dbb4c33abb9fc))
- track a dependency must be pre-deployed
  ([fe076d8](https://github.com/hirosystems/clarity-repl/commit/fe076d8622f9d130cea23a3e3281d2fd07b7a33a)),
  closes [#119](https://github.com/hirosystems/clarity-repl/issues/119)
- various adjustments
  ([0fc2cb3](https://github.com/hirosystems/clarity-repl/commit/0fc2cb3f2ec5b9b35ca28030992a102d7afe9986))

# [0.27.0](https://github.com/hirosystems/clarity-repl/compare/v0.26.0...v0.27.0) (2022-05-11)

### Bug Fixes

- check the maximum length of a contract name
  ([0a2d110](https://github.com/hirosystems/clarity-repl/commit/0a2d11090086150ad54ebc421e7c6283a5f7606d)),
  closes [#145](https://github.com/hirosystems/clarity-repl/issues/145)
- **dap:** return a map's type for its value
  ([6772bf8](https://github.com/hirosystems/clarity-repl/commit/6772bf8247ee088f18a9042e827caa6c2017ea71))
- **dap:** track the stack frames on every expression
  ([7be30f9](https://github.com/hirosystems/clarity-repl/commit/7be30f983ff17ea10b31cb828665849d274aba18))
- disable dap for wasm builds
  ([ef27199](https://github.com/hirosystems/clarity-repl/commit/ef2719997f071f36f6a6d4870fc0a683d5aac282))
- harden `ASTVisitor` to handle invalid ASTs
  ([89304d9](https://github.com/hirosystems/clarity-repl/commit/89304d9d50e7d63590bdd891eb2056e13bcd3154)),
  closes [hirosystems/clarinet#334](https://github.com/stx-labs/clarinet/issues/334)
- update block limits to 2.05 numbers
  ([a75fe34](https://github.com/hirosystems/clarity-repl/commit/a75fe34cade34047979e7ccda5c10b657536ab5c)),
  closes [#114](https://github.com/hirosystems/clarity-repl/issues/114)
- update tests for eval_hooks changes
  ([c2e1f66](https://github.com/hirosystems/clarity-repl/commit/c2e1f66615380e36eb28c1e7b36d60635ef4e212))

### Features

- add reload command
  ([0772c82](https://github.com/hirosystems/clarity-repl/commit/0772c82fe7d754d68777e832dd5c7af51ce3a041))
- better simulated block time
  ([bca289e](https://github.com/hirosystems/clarity-repl/commit/bca289e41fa317ef62dfd7d9cd0535541d5094f5)),
  closes [hirosystems/clarinet#310](https://github.com/stx-labs/clarinet/issues/310)
- **dap:** add support for watching variables
  ([884e46e](https://github.com/hirosystems/clarity-repl/commit/884e46eda30fe9455663c8e5314e69e96a879d6a))
- **dap:** added `complete` method to `EvalHook`
  ([7de2090](https://github.com/hirosystems/clarity-repl/commit/7de2090cf566c4dcfb6d5d806372d3b88f5a328c))
- **dap:** begin support for DAP debugger
  ([53f1f34](https://github.com/hirosystems/clarity-repl/commit/53f1f34a530630bfc00ec2032c2a7bdb3c7e9660))
- **dap:** implement evaluate request
  ([bfc3f46](https://github.com/hirosystems/clarity-repl/commit/bfc3f460ca80555266197f4da7dca806b8b3c8fa))
- **dap:** implement scopes and variables
  ([b034d4c](https://github.com/hirosystems/clarity-repl/commit/b034d4c9180b71c6fa55dde12c3008432686fbde))
- **dap:** stack traces and pause
  ([145f581](https://github.com/hirosystems/clarity-repl/commit/145f581d87147b125e9febb904b9b80ae9bb9728))
- **reload:** do not clear `executed`
  ([bf8d614](https://github.com/hirosystems/clarity-repl/commit/bf8d614315adf38edca115a3e2c75a878a614e73))
- **reload:** have error reading a file being a catastrophic error
  ([2828e25](https://github.com/hirosystems/clarity-repl/commit/2828e25111562e35b894be35609c4cfe743cb765))
- **reload:** report error when file in path can not be reloaded
  ([a8a3e2a](https://github.com/hirosystems/clarity-repl/commit/a8a3e2a5e166aa74580fcadb63c5cac74fc6249a))
- **reload:** use `std::mem::take` instead of `clone` and `clear`
  ([b58c85c](https://github.com/hirosystems/clarity-repl/commit/b58c85c7c6eb4d6481e52008d92b0845e7f05cac))
- return unresolved dependencies from detector
  ([c650c91](https://github.com/hirosystems/clarity-repl/commit/c650c91cf8d7f67570cfc2d4fc1007c7ac6a803b))
- track a dependency must be pre-deployed
  ([98c902f](https://github.com/hirosystems/clarity-repl/commit/98c902f6868e754ea883637f101080fbf3dfc5db)),
  closes [#119](https://github.com/hirosystems/clarity-repl/issues/119)

# [0.26.0](https://github.com/hirosystems/clarity-repl/compare/v0.25.0...v0.26.0) (2022-04-20)

### Bug Fixes

- finish AST dependency detector
  ([3f09ce5](https://github.com/hirosystems/clarity-repl/commit/3f09ce54bf0657a6a30160034a521f81a7f5b81a))
- hint for patches
  ([34766f5](https://github.com/hirosystems/clarity-repl/commit/34766f5e1d9f778b5373d791f8c08e0a17c23cc5))

### Features

- add AST-based dependency checker
  ([237691f](https://github.com/hirosystems/clarity-repl/commit/237691fef279c425085f5c6c90125a8d50e0d3ea))

# [0.25.0](https://github.com/hirosystems/clarity-repl/compare/v0.24.0...v0.25.0) (2022-04-04)

### Bug Fixes

- add to-int to intellisense
  ([bb3dabd](https://github.com/hirosystems/clarity-repl/commit/bb3dabd8f0066986fad311ee67db7af196bfbc4a)),
  closes [#124](https://github.com/hirosystems/clarity-repl/issues/124)
- **debugger:** add watchpoint help, `help w`
  ([aa07f95](https://github.com/hirosystems/clarity-repl/commit/aa07f9595b908e08c1ebff6d1a16356afaa8d0e4))
- **debugger:** delete all breakpoints with `b del`
  ([1fec069](https://github.com/hirosystems/clarity-repl/commit/1fec069785da774a444aa91c862ecc797648ef02))
- **debugger:** handle breaking in debug command
  ([d3eaac6](https://github.com/hirosystems/clarity-repl/commit/d3eaac63ee32d0d07aad33ec5ea9315122c9f535))
- **debugger:** improve handling of `finish`
  ([00440a6](https://github.com/hirosystems/clarity-repl/commit/00440a695a2a0c36b40274e92aaeb01be490b658))
- enable debug only with cli feature
  ([2797fab](https://github.com/hirosystems/clarity-repl/commit/2797fab5eda4cf88bee2077970e539cd60ca0ef7))
- fix intellisense for `append`
  ([e1b3641](https://github.com/hirosystems/clarity-repl/commit/e1b36411cb14195709ab98294ec6ea0c7c7cebc4)),
  closes [#123](https://github.com/hirosystems/clarity-repl/issues/123)
- improve intellisense for define-\*
  ([b0d25d5](https://github.com/hirosystems/clarity-repl/commit/b0d25d57c153d479087bdff72e2ed26ff2f5400a)),
  closes [#65](https://github.com/hirosystems/clarity-repl/issues/65)
- update tests after debugger changes
  ([1a3bf9b](https://github.com/hirosystems/clarity-repl/commit/1a3bf9b654f395b1993c22d285f56f7d2a365487))

### Features

- **debugger:** add breakpoint management
  ([84cb948](https://github.com/hirosystems/clarity-repl/commit/84cb948d7a4534dcba01ca3333fa1a653b39cf48))
- **debugger:** add print command and print source
  ([2d3e32c](https://github.com/hirosystems/clarity-repl/commit/2d3e32c8d3ec200e174cd0eec4d411d0d33424e5))
- **debugger:** add watchpoints
  ([cd3ced9](https://github.com/hirosystems/clarity-repl/commit/cd3ced9e83df69c7ee20075d632ac5b19339ecc3))
- **debugger:** avoid repeated breakpoints
  ([24f90ec](https://github.com/hirosystems/clarity-repl/commit/24f90ec53254163b4f61fcb17decc740dfff54a5))
- **debugger:** implement source breakpoints
  ([4cc26ac](https://github.com/hirosystems/clarity-repl/commit/4cc26ac4492d9ca6974ca732b1f0ec4d47803bd0))
- **debugger:** implement source breakpoints
  ([cf5cd8d](https://github.com/hirosystems/clarity-repl/commit/cf5cd8d29f39b1e58bed4bcdcd1e798cf0f07594))
- **debugger:** print any expression with 'print'
  ([51ff32f](https://github.com/hirosystems/clarity-repl/commit/51ff32f471c30dcc925d34fd8099a62bb74093a5))
- implement a step-debugger in the REPL
  ([d776538](https://github.com/hirosystems/clarity-repl/commit/d77653867195c52bcd290bd3d8d2bcb85a3c14f2))
- record executed commands in the session
  ([f3a865a](https://github.com/hirosystems/clarity-repl/commit/f3a865ab59526db82f1bd188b6af450edb007230))

# [0.24.0](https://github.com/hirosystems/clarity-repl/compare/v0.23.1...v0.24.0) (2022-03-30)

### Bug Fixes

- **debugger:** handle breaking in debug command
  ([58d7695](https://github.com/hirosystems/clarity-repl/commit/58d7695e0ba590086220fb3ad6eaccf6c3b75290))
- enable debug only with cli feature
  ([594c59a](https://github.com/hirosystems/clarity-repl/commit/594c59a943b21def7002c3d555ba4ce5e00d05c5))
- update tests after debugger changes
  ([d760a7a](https://github.com/hirosystems/clarity-repl/commit/d760a7a1370755f03977614bb7993d0cf07b5a69))

### Features

- **debugger:** add breakpoint management
  ([354386a](https://github.com/hirosystems/clarity-repl/commit/354386a979c4a6883ba7a86494812d5eecbf99db))
- **debugger:** add print command and print source
  ([332ca5c](https://github.com/hirosystems/clarity-repl/commit/332ca5c2be397686dc86666fa853aa846ae6b90d))
- **debugger:** add watchpoints
  ([cac728c](https://github.com/hirosystems/clarity-repl/commit/cac728c5e3cd1ef61d31d1874460285352a457ca))
- **debugger:** avoid repeated breakpoints
  ([e4dc660](https://github.com/hirosystems/clarity-repl/commit/e4dc66088a1c6e0874875f45a18607d5afe98e42))
- **debugger:** implement source breakpoints
  ([d427609](https://github.com/hirosystems/clarity-repl/commit/d4276091c9e5629ec486ce68bcb2434148da4573))
- **debugger:** implement source breakpoints
  ([5a9331e](https://github.com/hirosystems/clarity-repl/commit/5a9331ea659071dc63e280b1d184f8c46bca4858))
- **debugger:** print any expression with 'print'
  ([d9875ce](https://github.com/hirosystems/clarity-repl/commit/d9875ce6d36dbf8a1a8b9f8a6a985f8d13d59968))
- implement a step-debugger in the REPL
  ([14ac56e](https://github.com/hirosystems/clarity-repl/commit/14ac56eb08ed2039e1bcd8edb64662fd5582d3e2))
- record executed commands in the session
  ([3af06aa](https://github.com/hirosystems/clarity-repl/commit/3af06aa86b251d52d29a4fde09a3aff061a40b1b))

## [0.23.1](https://github.com/hirosystems/clarity-repl/compare/v0.23.0...v0.23.1) (2022-03-08)

### Bug Fixes

- add missing traversal of cond in if expr
  ([49c0688](https://github.com/hirosystems/clarity-repl/commit/49c068849e555c2ce2bd7a0e3c5080cb7c5d5196))
- check for whitespace between exprs in list
  ([5cf0f06](https://github.com/hirosystems/clarity-repl/commit/5cf0f061adc2950c8217979fbf998fa68be564e1)),
  closes [#110](https://github.com/hirosystems/clarity-repl/issues/110)
- consider as-contract in check-checker
  ([00bd603](https://github.com/hirosystems/clarity-repl/commit/00bd60373deb3d480d576a4e4e88c9035e436af0))
- detect dependency through principal literal
  ([e6dfe4b](https://github.com/hirosystems/clarity-repl/commit/e6dfe4b287276bbbfb4183bf6402fe5570920e0e))
- improve error in type checker
  ([e6e7267](https://github.com/hirosystems/clarity-repl/commit/e6e72679d893128c624dd500a61b191007e81c1d))

# [0.23.0](https://github.com/hirosystems/clarity-repl/compare/v0.22.2...v0.23.0) (2022-02-23)

### Bug Fixes

- report an error for CRLF line-endings
  ([5a4ccf0](https://github.com/hirosystems/clarity-repl/commit/5a4ccf083e3965569749d39b4ccd9345b93cdf22)),
  closes [#98](https://github.com/hirosystems/clarity-repl/issues/98)

### Features

- add note about CRLF -> LF mode
  ([5c1d2b6](https://github.com/hirosystems/clarity-repl/commit/5c1d2b6498b7fb0f6527cfd2c67b8d76e9775507))

## [0.22.2](https://github.com/hirosystems/clarity-repl/compare/v0.22.1...v0.22.2) (2022-02-18)

### Bug Fixes

- rustls was not properly enabled (openssl c lib was being used)
  ([4f6b7b5](https://github.com/hirosystems/clarity-repl/commit/4f6b7b5284abb0a37b0338d78e0853bfc1459d17))

## [0.22.1](https://github.com/hirosystems/clarity-repl/compare/v0.22.0...v0.22.1) (2022-02-12)

### Bug Fixes

- append output from initial contracts
  ([7dc1a8e](https://github.com/hirosystems/clarity-repl/commit/7dc1a8ee076227ca23e78b3e83db8d71f1033f36))

# [0.22.0](https://github.com/hirosystems/clarity-repl/compare/v0.21.0...v0.22.0) (2022-02-09)

### Bug Fixes

- add checks for argument counts to map-\* funcs
  ([1a1cadb](https://github.com/hirosystems/clarity-repl/commit/1a1cadb876f281b732801455334167a17cd84ac7)),
  closes
  [stacks-network/stacks-blockchain#3018](https://github.com/stacks-network/stacks-blockchain/issues/3018)
  [hirosystems/clarinet#228](https://github.com/stx-labs/clarinet/issues/228)
- allow symbols in identifiers
  ([15acc61](https://github.com/hirosystems/clarity-repl/commit/15acc61d4bd9e31235608de08514f2900eab7578))
- crash when an error is reported at EOF
  ([af6894a](https://github.com/hirosystems/clarity-repl/commit/af6894a2934973298df2bd16500bcbb4c53d4512))
- disabling requirements on wasm builds
  ([9176e2b](https://github.com/hirosystems/clarity-repl/commit/9176e2b61b79e1b21e70dcb7fce2699938866495))
- fix bug in comment handling
  ([6dd45de](https://github.com/hirosystems/clarity-repl/commit/6dd45dea7224e8e690b5f49da8835f207294de1a))
- fix crash on error with 0 column
  ([0ee66b9](https://github.com/hirosystems/clarity-repl/commit/0ee66b900410800dddd4edb861f15e0a673f798e))
- fix error when handling an invalid symbol
  ([70cfa1a](https://github.com/hirosystems/clarity-repl/commit/70cfa1ae63016500761ca540cf88b31fd9e044dd))
- fix handling of filtered params
  ([4d6d222](https://github.com/hirosystems/clarity-repl/commit/4d6d2227a2e15ae22f0858c19d7be770e603f846))
- fix handling of negative integer literals
  ([edb4d14](https://github.com/hirosystems/clarity-repl/commit/edb4d145f388131e6c62cabb48c6ac7148611c89))
- fix lexer error with empty comment
  ([ae896b5](https://github.com/hirosystems/clarity-repl/commit/ae896b5006f2fabdb8fba4895bf8a5c0da611cab))
- improve handling of invalid trait reference
  ([5aa363a](https://github.com/hirosystems/clarity-repl/commit/5aa363a8b2f5beaf872c9401fc348d9c5482b60b))
- improved handling of unterminated strings
  ([5035a2f](https://github.com/hirosystems/clarity-repl/commit/5035a2ff5db95b2abcd5d8f27a69ed24e63629b2))
- return more errors
  ([a44e35d](https://github.com/hirosystems/clarity-repl/commit/a44e35d67d1274899601e4b62cb01bc9486586c6))
- returns all the diagnostics
  ([dc992a3](https://github.com/hirosystems/clarity-repl/commit/dc992a3eba4c59586c8ba538365532bfdf21f51d))

### Features

- ability to lazy load contracts
  ([bc50b26](https://github.com/hirosystems/clarity-repl/commit/bc50b268bd61cb32710d4dd4418f21e1ac624d1c))
- add ability to save contracts
  ([f43abb5](https://github.com/hirosystems/clarity-repl/commit/f43abb585e10db298f882c8f9667dafd365513ae))
- add disk cache for contracts
  ([a036fda](https://github.com/hirosystems/clarity-repl/commit/a036fda0780fb0ca96635910f424d8ec28a7cc7a))
- add option to select parser version
  ([c731e56](https://github.com/hirosystems/clarity-repl/commit/c731e5675e06690d978c3f9a6629f25dba05f6a9))
- checker support of trusted sender/caller
  ([70191a4](https://github.com/hirosystems/clarity-repl/commit/70191a4fbda4aaf45f53f26a9c5ea6558c0ed565)),
  closes [#62](https://github.com/hirosystems/clarity-repl/issues/62)
- cleanup configuration of repl and analysis
  ([ce389c1](https://github.com/hirosystems/clarity-repl/commit/ce389c1ba94935dec34b54cf650188b2a06c3569))
- improve check-checker handling of rollbacks
  ([cc0c3e2](https://github.com/hirosystems/clarity-repl/commit/cc0c3e2bbc59c85ad4cf9b141d9e071a12af08c9)),
  closes [#81](https://github.com/hirosystems/clarity-repl/issues/81)
- improved parser
  ([e7ae7b8](https://github.com/hirosystems/clarity-repl/commit/e7ae7b813542a9be512c87fbd37f9b16d8009198)),
  closes [#74](https://github.com/hirosystems/clarity-repl/issues/74)

# [0.21.0](https://github.com/hirosystems/clarity-repl/compare/v0.20.1...v0.21.0) (2022-01-13)

### Bug Fixes

- fix ast visitor traversal of contract-of expr
  ([d553e50](https://github.com/hirosystems/clarity-repl/commit/d553e50d3ffdac6b4994015450058a3a29e872ed)),
  closes [#77](https://github.com/hirosystems/clarity-repl/issues/77)
- resolve CI failure for forks
  ([8152e4b](https://github.com/hirosystems/clarity-repl/commit/8152e4b086faef02ac21f23b8af5d65c93345166))

### Features

- add 'filter' annotation
  ([4cebe6b](https://github.com/hirosystems/clarity-repl/commit/4cebe6bcc58c928ef62a3d3faad6d15802f215db)),
  closes [#72](https://github.com/hirosystems/clarity-repl/issues/72)

## [0.20.1](https://github.com/hirosystems/clarity-repl/compare/v0.20.0...v0.20.1) (2022-01-06)

### Bug Fixes

- remove println events
  ([4879ee4](https://github.com/hirosystems/clarity-repl/commit/4879ee426655b43f04b12492b41543d5ad486fb9))

# [0.20.0](https://github.com/hirosystems/clarity-repl/compare/v0.19.0...v0.20.0) (2022-01-05)

### Bug Fixes

- properly update block id lookup table when advancing the chain tip
  ([d457df5](https://github.com/hirosystems/clarity-repl/commit/d457df5270b04356bbc382c0d2fb2baa929c5308))
- snippet use in LSP
  ([f4dccdf](https://github.com/hirosystems/clarity-repl/commit/f4dccdfc1820108ec23f321ac404151720af21df))

### Features

- **check-checker:** allow private function filter
  ([6036d69](https://github.com/hirosystems/clarity-repl/commit/6036d6997dc9ffd38d98a5fddf85626213b1682d))

# [0.19.0](https://github.com/hirosystems/clarity-repl/compare/v0.18.0...v0.19.0) (2021-12-21)

### Bug Fixes

- chain tip logic and vrf seed generation
  ([1863e00](https://github.com/hirosystems/clarity-repl/commit/1863e00ec0c0391610f2cf1635f048a82f40052e))
- correctly utilize current_chain_tip
  ([b134d39](https://github.com/hirosystems/clarity-repl/commit/b134d39fc56e7ddd1a8152d25ec2a6f700f13de2))
- panic if block doesn't exist
  ([2aedd35](https://github.com/hirosystems/clarity-repl/commit/2aedd352069488452349d6b2246936c14c2661ea))
- use lookup table to make datastore more efficient
  ([ad1cfae](https://github.com/hirosystems/clarity-repl/commit/ad1cfaee29aa7d811c83f9db6b9c3defe3eb0cb1))

### Features

- start making Datastore block aware
  ([ca1e097](https://github.com/hirosystems/clarity-repl/commit/ca1e09733fddff3a07d9619ee4d165a2c29a7fa6))
- use hash for block id
  ([2ab9ed6](https://github.com/hirosystems/clarity-repl/commit/2ab9ed603d320bd86db9fbec15b187e48d5be1b7))

# [0.18.0](https://github.com/hirosystems/clarity-repl/compare/v0.17.0...v0.18.0) (2021-12-17)

### Bug Fixes

- fix bug in handling of map-insert/set
  ([7b47da1](https://github.com/hirosystems/clarity-repl/commit/7b47da1efcaf80f17f5dcb2a0dbf9557fa078d5c))
- fix unit tests after 351ad77
  ([af6a3f4](https://github.com/hirosystems/clarity-repl/commit/af6a3f464d2dbf920b8d15062405f3143f51998c))
- handle private functions in check-checker
  ([b73ad7b](https://github.com/hirosystems/clarity-repl/commit/b73ad7b03fff169436fb7c794bf6bed713d067f6))
- order taint info diagnostics
  ([e4c4211](https://github.com/hirosystems/clarity-repl/commit/e4c42113d9ffe22b9c3a3b4bc1ad77c1413bdca4))
- proposal for extra logs
  ([e72bc97](https://github.com/hirosystems/clarity-repl/commit/e72bc976356eacd48121ac66f0f435c4a1753631))
- set costs_version
  ([54bd48c](https://github.com/hirosystems/clarity-repl/commit/54bd48c77520b2408ca53bdc003a37ec25807856))
- **taint:** fix bug in taint propagation
  ([4a5579e](https://github.com/hirosystems/clarity-repl/commit/4a5579efe1072ba4282b04b38dc320893ec3d2c1))
- use contract name in diagnostic output
  ([45b9993](https://github.com/hirosystems/clarity-repl/commit/45b9993efbcf2484ec5f63cac9e84656f030a4c9))

### Features

- add `analysis` field to settings
  ([ef0d186](https://github.com/hirosystems/clarity-repl/commit/ef0d186cb4ec716e8a576ff964cf7711b185bba1))
- add support for annotations
  ([4b10465](https://github.com/hirosystems/clarity-repl/commit/4b104651a9d9768e03bb767865a1ff2f2dee3489))
- **analysis:** add taint checker pass
  ([f03f20a](https://github.com/hirosystems/clarity-repl/commit/f03f20a7d74e928e3b6c1a3df40991b98f4ca503)),
  closes [#33](https://github.com/hirosystems/clarity-repl/issues/33)
- **analysis:** improve diagnostics
  ([2eea11a](https://github.com/hirosystems/clarity-repl/commit/2eea11a7a3855aba23977923acc51ee1ad57c0e1))
- check argument count to user-defined funcs
  ([ceff88a](https://github.com/hirosystems/clarity-repl/commit/ceff88ac58f379e78b10e33947504de14b6d8805)),
  closes [#47](https://github.com/hirosystems/clarity-repl/issues/47)
- check for unchecked trait in contract-call?
  ([fec4149](https://github.com/hirosystems/clarity-repl/commit/fec4149e4317f7a9ea4da0fb4da925c7659f5793))
- invoke binary with clarity code
  ([264931e](https://github.com/hirosystems/clarity-repl/commit/264931e143ab45fcbf81faa7c6890dfe36c39088))
- remove warnings for txns on sender's assets
  ([2922e5c](https://github.com/hirosystems/clarity-repl/commit/2922e5c6dda668b1710a660666d02563a2bb0851))
- report warning for tainted return value
  ([137c806](https://github.com/hirosystems/clarity-repl/commit/137c806b3107e278d19d0425af6b45f4f62a4e56))
- update costs with final values
  ([b36196a](https://github.com/hirosystems/clarity-repl/commit/b36196aa55fd34c2705ee21364b79949590ba969))
- update default costs
  ([00e3328](https://github.com/hirosystems/clarity-repl/commit/00e332820441b851e8c60da34184e83bbe25daf5))

# [0.17.0](https://github.com/hirosystems/clarity-repl/compare/v0.16.0...v0.17.0) (2021-11-17)

### Bug Fixes

- ignore RUSTSEC-2021-0124
  ([65a494a](https://github.com/hirosystems/clarity-repl/commit/65a494ad2e761a729653b127882034cec9f465ff))

### Features

- add encode/decode commands
  ([cfea2e8](https://github.com/hirosystems/clarity-repl/commit/cfea2e8fa3e330dfd610a2516d2cc1918ccf6361)),
  closes [#7](https://github.com/hirosystems/clarity-repl/issues/7)
