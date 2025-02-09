import std/macros
import std/strutils
import std/tables#, src/fusion/matching
import Borrowed

{.experimental: "caseStmtMacros".}

##macro cdecl*[T](
##  name: untyped,
##): untyped = 
##  result = newStmtList()
#
#  #result = newStmtList()
#  #for i in 0..<10:
#  #  let name = ident("myProc" & $i)
#  #  let content = newLit("I am procedure number #" & $i)
#
#  #  result.add quote do:
#  #    proc `name`() =
#  #      echo `content`
#
#template cif*(
#  expr: untyped,
#  stmtList: untyped
#): untyped =
#  let ret {.nodecl.} = expr
#  {.emit:"if (".}
#  {.emit:"ret".}
#  {.emit:") {\n".}
#  stmtList
#  {.emit:"}\n".}
#
##macro cif*(
##  expr: untyped,
##  stmtList: untyped,
##): untyped =
##  stmtList.expectKind nnkStmtList
##  let retName = ident("ret")
##  result = quote do:
##    {.emit:"if (".}
##    #cexpr(expr=`expr`)
##    #let temp {.nodecl.}: typeof(`expr`) = `expr`
##    #discard temp
##    #let `ret` = `expr`
##    let `retName` {.importc,nodecl.} = `expr`
##    #cexpr(expr=`expr`)
##    {.emit:") {\n".}
##    cstmtlist(stmtList=`stmtList`)
##    {.emit:"}\n".}
#
#macro celif*(
#  expr: untyped,
#  stmtList: untyped,
#): untyped =
#  result = quote do:
#    {.emit:"else if (".}
#    cexpr(expr=`expr`)
#    {.emit:") {\n".}
#    cstmtlist(stmtList=`stmtList`)
#    {.emit:"}\n".}
#
#macro celse*(
#  stmtList: untyped
#): untyped = 
#  result = quote do:
#    {.emit:"else {".}
#    cstmtlist(stmtList=`stmtList`)
#    {.emit:"}\n".}
#
#macro cstmtlist*(
#  stmtList: untyped
#): untyped =
#  stmtList.expectKind nnkStmtList
#
##macro cexpr*(
##  expr: untyped,
##): untyped =
##  result = quote do:
##    {.emit:"" & $`expr`.}
#template cexpr*(
#  expr: untyped
#): bool =
#  let temp {.nodecl.}: typeof(expr) = expr
#  temp
#
#
##macro `eq`*(
##  left: untyped,
##  right: untyped
##): untyped =
##  discard
#
##cif(3):
##  let a = 3
##  let b = 8

type
  Convert = object
    funcTbl: Table[string, string]
    typedefTbl: Table[string, string]
    res: string
    useResult: bool

macro fail(): untyped =
  result = quote do:
    assert(false, repr(n) & " disallowed (maybe just for now?)")
  
proc have(
  n: NimNode,
  kinds: seq[NimNodeKind],
  start: int=0
): bool =
  result = true
  for i in start ..< kinds.len:
    if n[i].kind != kinds[i]:
      result = false
      return


proc toCodeIfStmt(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0
) =
  for n in nodes:
    case n.kind:
    of nnkEmpty:
      discard
    of nnkElifBranch:
      discard
    of nnkElse:
      discard
    else:
      fail()

proc toCodeExpr(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
  isLhs: static bool,
) =
  var n = nodes
  #echo n.kind
  case n.kind:
  of nnkIdent, nnkSym:
    self.res.add n.strVal
  of nnkInfix:
    if n[0].repr in ["+=", "-=", "*=", "/="]:
      #self.toCodeExpr(n[1], level + 1, isLhs)
      #self.res.add " "
      #self
      # TODO: maybe support this?
      fail()
    else:
      self.res.add "("
      self.toCodeExpr(n[1], level, isLhs)
      self.res.add " "
      if n[0].repr in ["mod"] and n[1].getType().repr != "int":
        self.res.add("fmod")
      else:
        self.res.add(n[0].strVal)
      self.res.add " "
      self.toCodeExpr(n[2], level, isLhs)
      self.res.add ")"
  of nnkDotExpr:
    self.toCodeExpr(n[0], level, isLhs)
    self.res.add "."
    self.toCodeExpr(n[1], level, isLhs)
  of nnkBracketExpr:
    if n[0].kind == nnkBracketExpr:
      discard
    else:
      self.toCodeExpr(n[0], level, isLhs)
      self.res.add "["
      self.toCodeExpr(n[1], level, isLhs)
      self.res.add "]"
  #of nnkResult:
  #  discard
  else:
    when not isLhs:
      case n.kind:
      of nnkIntLit:
        self.res.add("((int) " & $n.intVal & ")")
      of nnkInt8Lit:
        self.res.add("((int8_t)" & $n.intVal & ")")
      of nnkInt16Lit:
        self.res.add("((int16_t)" & $n.intVal & ")")
      of nnkInt32Lit:
        self.res.add("((int32_t)" & $n.intVal & ")")
      of nnkInt64Lit:
        self.res.add("((int64_t)" & $n.intVal & ")")
      of nnkFloatLit, nnkFloat32Lit:
        self.res.add("((float)" & $n.floatVal & ")")
      #of nnkFloat32Lit:
      #  discard
      #of nnkFloat64Lit:
      #  discard
      of nnkObjConstr:
        echo repr(n)
        let typeName = n[0].strVal
        self.res.add "(("
        self.res.add typeName
        self.res.add "){"
        for i in 1 ..< n.len:
          #if n[i].len == 2:
          self.res.add "."
          self.res.add n[i][0].strVal
          self.res.add "="
          self.toCodeExpr(n[i][1], level, isLhs)
          #else:
          #  self.toCodeExpr(n[i][0], level, isLhs)
          if i + 1 < n.len:
            self.res.add ", "
        self.res.add "})"
      of nnkCall:
        discard
      #of nnkCommand:
      #  
      else:
        fail()
  discard

proc toCodeAsgn(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
) =
  addIndent(self.res, level)
  let n = nodes
  case nodes.len:
    of 2:
      self.toCodeExpr(
        nodes=n[0],
        #res=res,
        level=level,
        isLhs=true,
      )
      self.res.add " = "
      self.toCodeExpr(
        nodes=n[1],
        #res=res,
        level=level,
        isLhs=false,
      )
    else:
      fail()
  self.res.addSmart(';')
      
  #for n in nodes:
  #  case n.kind:
  #  else:
  #    assert false
proc toCodeCall(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0
) =
  addIndent(self.res, level)

proc typeDef(
  self: var Convert,
  topLevelNode: NimNode,
): string =
  var typeName = ""
  #assert topLevelNode.kind in {nnkTypeDef}

  result = typeName

proc toCodeTypeSection(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0,
) =
  addIndent(self.res, level)
  for n in nodes:
    case n.kind:
    of nnkTypeDef:
      discard
    else:
      fail()
  discard

proc toCodeVarSection(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0,
) =
  addIndent(self.res, level)
  for n in nodes:
    case n.kind:
    of nnkIdentDefs:
      case n.len:
      of 3:
        if have(n, @[nnkIdent, nnkIdent, nnkEmpty]):
          discard
        elif have(n, @[nnkIdent, nnkEmpty]):
          discard
        else:
          fail()
        #of (nnkIdent, nnkIdent, nnkEmpty):
        #  discard
        #else:
        #  discard
      else:
        #echo "disallowed for now"
        #echo n
        fail()
      #for n in n:
      #  case n.kind:
      #  of nnkIdent:
      #    discard
      #  of nnkEmpty:
      #    discard
      #  of nnkObjConstr:
      #    discard
      #  else:
      #    echo n
      #    fail()
    else:
      #echo n
      fail()

proc toCodeStmts(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int
) = 
  addIndent(self.res, level)
  #echo repr(nodes)
  for n in nodes:
    case n.kind:
    of nnkEmpty:
      discard
    of nnkSym:
      discard
    of nnkIfStmt:
      self.toCodeIfStmt(n, level + 1)
      #discard
    of nnkTypeSection:
      self.toCodeTypeSection(n, level + 1)
    of nnkVarSection:
      self.toCodeVarSection(n, level + 1)
    #of nnkV
    of nnkAsgn:
      self.toCodeAsgn(n, level + 1)
    of nnkCall:
      self.toCodeCall(n, level + 1)
    #of nnkElifBranch:
    #  discard
    #of nnkElse:
    #  discard
    else:
      #echo n
      fail()
      

#proc toCodeTopLevel(
#  self: var Convert,
#  topLevelNode: NimNode,
#  #res: var string,
#  level = 0
#) = 
#  assert topLevelNode.kind == nnkProcDef
#  for n in topLevelNode:
#    case n.kind:
#    of nnkEmpty:
#      discard
#    of nnkSym:
#      discard
#    of nnkTypeSection:
#      self.toCodeTypeSection(n, level + 1)
#    #of nnkFormalParams:
#    #  for param in n:
#    else:
#      self.toCodeStmts(n, level + 1)
#      discard


const ignoreFuncs = [
  "echo", "print", "debugEcho", "len", "$"
]

proc hasResult(
  self: var Convert,
  node: NimNode,
): bool =
  if node.kind == nnkSym and node.strVal == "result":
    return true
  for c in node.children:
    if self.hasResult(c):
      return true
  return false

proc procDef(
  self: var Convert,
  topLevelNode: NimNode,
): string =
  var procName = ""
  var paramsStr = ""
  var returnType = "void"

  assert topLevelNode.kind in {nnkProcDef, nnkFuncDef}
  for n in topLevelNode:
    case n.kind:
    of nnkEmpty:
      discard
    of nnkPragma:
      # TODO: come back to this later
      discard
    of nnkSym:
      procName = $n
    of nnkFormalParams:
      if n[0].kind != nnkEmpty:
        returnType = n[0].strVal
      for paramDef in n[1 .. ^1]:
        # the paramDef is like `x, y, z: float`
        if paramDef.kind != nnkEmpty:
          for param in paramDef[0 ..< ^2]:
          #for idx in 0 .. paramDef[0 ..< ^2].len:
            #let param = paramDef[idx]
            # Process each `x`, `y`, `z` in a loop
            paramsStr.add "  "
            let paramName = param.repr()
            let paramType = param.getTypeInst()
            #echo paramName & ": " & $paramType
            #dumpAstNode(n)
            #dumpTree(n)
            if paramType.kind == nnkVarTy:
              fail()
            elif paramType.kind == nnkBracketExpr:
              fail()
            #else:
            #  paramsStr.add ""
            paramsStr.add paramType.strVal
            paramsStr.add " "
            paramsStr.add paramName
            paramsStr.add ",\n"
          # remove the final ",\n" since C doesn't support that
      paramsStr.setLen(paramsStr.len - 2)
      paramsStr.add "\n"
    else:
      self.res.setLen(0)
      self.res.add "\n"
      self.res.add returnType & " " & procName & "(\n"
      if paramsStr.len == 0:
        self.res.add "  void"
      else:
        self.res.add paramsStr
      self.res.add ") {\n"
      self.useResult = self.hasResult(n)
      if self.useResult:
        self.res.addIndent(1)
        self.res.add returnType
        self.res.add " result;"
      self.toCodeStmts(n, 0)
      if self.useResult:
        if "return result" notin self.res[^20..^1]:
          self.res.addIndent(1)
          self.res.add "return result;\n"
      self.res.add "}"

  #echo paramsStr
  echo self.res
  result = self.res


proc findFuncs(
  self: var Convert,
  topLevelNode: NimNode,
  #funcTbl: var Table[string, string],
  #typedefTbl: var Table[string, string],
) =
  for n in topLevelNode:
    if n.kind == nnkCall:
      #echo n
      #dumpTree n
      if repr(n[0]) == "[]":
        continue
      let procName = repr n[0]
      if (
        (
          procName in ignoreFuncs
        ) or (
          procName in self.funcTbl
        )
      ):
        continue
      let impl = n[0].getImpl()
      #echo repr(impl)
      self.findFuncs(impl)
      self.funcTbl[procName] = self.procDef(impl)

    #if (
    #  n.kind == nnkVarSection
    #):
    #  #dumpAstGen(n)
    #  #echo "testificate"
    #  if repr(n[0]) == "[]":
    #    continue
    #  for n in n:
    #    case n.kind:
    #    of nnkIdentDefs:
    #      case n.len:
    #      of 2, 3:
    #        if have(n, @[nnkIdent, nnkIdent]):
    #          discard
    #        elif have(n, @[nnkIdent, nnkEmpty]):
    #          # TODO: support this
    #        #  discard
    #        #else:
    #          fail()
    #      else:
    #        fail()
    #    else:
    #      fail()

    #if (
    #  n.kind == nnkTypeSection
    #):
    #  #dumpTree n
    #  #echo n
    #  if repr(n[0]) == "[]":
    #    #echo "continuing..."
    #    continue
    #  #echo repr n[0]
    #  let typedefName = repr n[0]
    #  if typedefName in self.typedefTbl:
    #    continue
    #  echo typedefName
    #  #let myType = n[0].getTypeInst()
    #  #echo myType
    #  #let impl = n[0].getImpl()
    #  #findFuncsTypedefs(impl, funcTbl, typedefTbl)
    #  #typedefTbl[typedefName] = typeDef(impl)

    self.findFuncs(n)


proc toPipelineCInner*(
  s: NimNode,
): string =
  var code: string
  #code.add "asdf"
  var n = getImpl(s)
  #dumpAstGen(n)

  #var funcTbl: Table[string, string]
  #var typedefTbl: Table[string, string]
  #var convert: Convert = Convert(
  #  #funcTbl=funcTbl,
  #  #typedefTbl=typedefTbl,
  #  #res=code,
  #)
  var convert: Convert
  convert.findFuncs(n)
  #echo convert.funcTbl
  #echo convert.typedefTbl
  #var globals: Table[string, string]

  ##for k, v in globals:
  ##  code.add(v)
  ##  code.add "\n"

  #for k, v in typedefTbl:
  #  code.add(v)
  #  code.add "\n"
  #code.add "\n"

  #for k, v in funcTbl:
  #  code.add v
  #  code.add "\n"
  #code.add "\n"

  ##toCodeTopLevel(topLevelNode=n, res=code, level=0)

  return code

macro toPipelineC*(
  s: typed,
): string =
  newLit(toPipelineCInner(s))
