# LUA ULTIMATE INT

Make it possible to calculate large number (bignum) in Lua, Pure Lua number library!\
**POWERED BY INT DOT LUA**

> [!NOTE]
> To install this module with Luarocks:
>
> ```bash
> luarocks install uint
> ```

Example:

```lua
-- require a module
local int = require("uint")

-- build a new int object
local x, y = int.new("20", "10")

print(x ^ y) -- output: 10240000000000
```

## feature

- **Calculate**
  - addition `+`
  - subtraction `-`
  - multiplication `*`
  - division `\, \\`
  - modulo `%`
  - power `^`
- **Equation**
  - equal
  - less than
  - more than
- **Function**
  - exp
  - fact
  - sqrt
  - *and more...*

## interested?

[**readmore**](https://github.com/SupTan85/int.lua)
