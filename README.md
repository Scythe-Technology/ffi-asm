# ffi-asm

An alternative to libffi. Generates instructions to do similar capabilities as libffi.

Currently designed to work on Linux, macOS and Windows. More platforms could be considered.

Project Status: `alpha`

### Calling Conventions
- [x] cdecl (c)
- [ ] stdcall (windows only)
- [ ] fastcall

### Architecture Backends
- 32-bit
  - [ ] x86
  - [ ] aarch32
  - [ ] risc-v32
- 64-bit
  - [x] x86_64
  - [ ] aarch64
  - [ ] risc-v64