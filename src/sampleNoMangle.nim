include extraMacros
import nimToPipelineC

type
  Vec3I32 = object
    v: array[3, int32]

proc v3add(
  left: ptr Vec3I32,
  right: ptr Vec3I32,
): Vec3I32 {.cnomangle.} =
  #for i in 0 ..< result.v.len():
  var i: int = 0
  while i < result.v.len():
    result.v[i] = left[].v[i] + right[].v[i]
    i = i + 1
    #i += 1

#proc do_print(
#  arg: ptr char
#) {.cextern,cnomangle.} =
#  discard
#
#proc main(
#  argc: int,
#  argv: ptr seq[char],
#): int {.cnomangle.} = 
#  for i in 0 ..< argc:
#    do_print(addr(argv[i]))
#
#proc outerMain(
#  argc: int,
#  argv: ptr seq[char],
#): int =
#  result = main(argc=argc, argv=argv)
template `&`*(
  obj: untyped
): untyped =
  addr(obj)
#macro `->`*(
#  obj: untyped,
#  mbr: untyped,
#): untyped = 
#  #(obj[]).mbr
#  result = quote do:
#    (`obj`).`mbr`

proc outerMain(): Vec3I32 {.cnomangle.}=
  let left: Vec3I32 = Vec3I32(v: [1, 2, 3])
  let right: Vec3I32 = Vec3I32(v: [5, 6, 7])
  #result = (&(left)).vec3i32_add(addr(right))
  let pLeft: ptr Vec3I32 = addr(left)
  let pRight: ptr Vec3I32 = addr(right)
  #result = addr(left)->vec3i32_add(addr(right))
  #result = addr(left)->vec3i32_add(addr(right))
  #result = pLeft.v3add(pRight)
  return pLeft.v3add(pRight)

proc outerOuterMain(): Vec3I32 =
  result = outerMain()

echo toPipelineC(outerOuterMain, regularC=true)
