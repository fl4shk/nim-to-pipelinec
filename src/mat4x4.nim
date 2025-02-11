include extraMacros
import nimToPipelineC

type
  Mat4x4*[T] = object
    m*: array[4, array[4, T]]

proc `plus`*[T](
  left: Mat4x4[T],
  right: Mat4x4[T],
): Mat4x4[T] =
  for j in 0 .. result.m.len():
    for i in 0 .. result.m[j].len():
      result.m[j][i] = left.m[j][i] + right.m[j][i]

template `+`(
  left: Mat4x4,
  right: Mat4x4,
): Mat4x4 =
  result = left.plus(right)

proc `star`*[T](
  left: Mat4x4[T],
  right: Mat4x4[T],
): Mat4x4[T] =
  discard
