#import std/macros, std/strutils, std/tables
import std/strutils

# This code was borrowed from: 
# https://github.com/treeform/shady
# and this is licensed as follows
# 
# The MIT License (MIT)
# 
# Copyright (c) 2021 Andre von Houck
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

proc `addIndent`*(res: var string, level: int) =
  ## Add indent (only if its needed).
  var
    idx = res.len - 1
    spaces = 0
  while res[idx] == ' ':
    dec idx
    inc spaces
  if spaces == 0 and res[idx] != '\n':
    res.add '\n'
  let level = level - spaces div 2
  for i in 0 ..< level:
    res.add "  "

proc `addSmart`*(res: var string, c: char, others = {'}'}) =
  ## Ads a char but first checks if its already here.
  var idx = res.len - 1
  while res[idx] in Whitespace:
    dec idx
  if res[idx] != c and res[idx] notin others:
    res.add c
