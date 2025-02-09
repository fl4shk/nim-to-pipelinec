import std/macros

macro `mkBinop`*(
  LeftT: typedesc,
  RightT: typedesc,
  opStr: untyped,
  funcStr: untyped,
): untyped = 
  result = nnkStmtList.newTree(
    nnkTemplateDef.newTree(
      nnkAccQuoted.newTree(
        newIdentNode($opStr)
      ),
      newEmptyNode(),
      newEmptyNode(),
      nnkFormalParams.newTree(
        newIdentNode("untyped"),
        nnkIdentDefs.newTree(
          newIdentNode("left"),
          newIdentNode($LeftT),
          newEmptyNode()
        ),
        nnkIdentDefs.newTree(
          newIdentNode("right"),
          newIdentNode($RightT),
          newEmptyNode()
        )
      ),
      newEmptyNode(),
      newEmptyNode(),
      nnkStmtList.newTree(
        nnkCommand.newTree(
          nnkDotExpr.newTree(
            newIdentNode("left"),
            newIdentNode($funcStr)
          ),
          newIdentNode("right")
        )
      )
    )
  )
