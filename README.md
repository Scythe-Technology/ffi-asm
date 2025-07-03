# ffi-asm

An alternative to libffi. Generates instructions to do similar capabilities as libffi.

Project Status: `alpha`

### Calling Conventions
- [x] cdecl (c)
- [ ] stdcall (windows only)
- [ ] fastcall

### Supported Platforms
| OS | Architecture |
|----|--------------|
| Linux | x86_64 |
| macOS | x86_64 |
| Windows | x86_64 |

### Example
```zig
const std = @import("std");
const ffi = @import("ffi-asm");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const add = struct {
        fn inner(a: i8, b: i8) callconv(.c) i8 {
            return a + b;
        }
    }.inner;

    const mem = try ffi.generateAsmCall(allocator, &.{ ffi.type_i8, ffi.type_i8 }, ffi.type_i8);
    const dynm = ffi.ExecutableMemory{
        .allocator = allocator,
        .mem = mem,
    };
    defer dynm.deinit();

    try dynm.executable();

    const ffi_fn: *const ffi.CallFn = @ptrCast(dynm.mem);

    var a: i8 = 5;
    var b: i8 = 7;
    var res: i8 = 0;

    ffi_fn(@ptrCast(&add), &.{ &a, &b }, &res);

    print("Result: {}\n", .{res});
}