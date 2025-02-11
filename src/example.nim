include extraMacros
import nimToPipelineC
type
  Vec3[T] = object
    x*: T
    y*, z*: T
    #z*: T
proc plus[T](
  left: Vec3[T],
  right: Vec3[T]
): Vec3[T] =
  result = Vec3[T](
    x: left.x + right.x,
    y: left.y + right.y,
    z: left.z + right.z,
  )

template `+`*(
  left: Vec3,
  right: Vec3,
): Vec3 =
  left.plus(right)

proc doVec3IAdd(
  a: Vec3[int],
  b: Vec3[int],
): Vec3[int] =
  result = a + b

proc myMain(): Vec3[int] =
  let a: Vec3[int] = Vec3[int](x: 1, y: 2, z: 3)
  let b: Vec3[int] = Vec3[int](x: 7, y: 9, z: 2)
  result = doVec3IAdd(a=a, b=b)

proc myOuterMain(): Vec3[int] =
  result = myMain()
echo toPipelineC(myOuterMain)
