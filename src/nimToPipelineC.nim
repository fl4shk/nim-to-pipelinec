import std/macros
import std/strutils
import std/tables#, src/fusion/matching
import borrowed

#{.experimental: "caseStmtMacros".}

template `cstatic`*() {.pragma.}
template `cconst`*() {.pragma.}
template `cextern`*() {.pragma.}
template `cnodecl`*() {.pragma.}
template `cnomangle`*() {.pragma.}
template `craw`*(
  key: string
) {.pragma.}

template `cmainmhz`*(
  mhz: string
) {.pragma.}

#macro `cimport`*() =
#  {.pragma: imported, importc, cdecl, raises: [], gcsafe.}

type
  FuncTblElem = object
    #doMangle: bool
    defn: string

type
  Convert = object
    #procRenameTbl: seq[Table[string, string]]
    #objRenameTbl: seq[Table[string, string]]
    #renameLevel: int = 0
    globalTbl: Table[string, string]
    globalSeq: seq[string]
    funcTbl: Table[string, FuncTblElem]
    funcSeq: seq[string]
    typedefTbl: Table[string, string]
    typedefSeq: seq[string]
    res: string
    useResult: bool
    hadArray: bool
    procName: string
    regularC: bool
    cppConstRefInp: bool
    noMangleTbl: Table[string, bool]
    noMangleProtoTbl: Table[string, string]

macro fail(): untyped =
  result = quote do:
    assert(
      false,
      (
        $n.kind & " " & repr(n) & " disallowed (maybe just for now?): "
      ) & (
        n.treeRepr
      )
    )
macro errFail(
  msg: string
): untyped =
  result = quote do:
    assert(
      false,
      (
        $n.kind & " " & repr(n) & "\n"
      ) & (
      #  n.treeRepr & "\n"
      #) 
      #& (
        `msg`
      ),
    )
#macro fail(
#  n: untyped
#): untyped =
#  result = quote do:
#    fail()

proc have(
  n: NimNode,
  kinds: seq[NimNodeKind],
  start: int=0
): bool =
  result = true
  for i in 0 ..< kinds.len:
    if n[i + start].kind != kinds[i]:
      result = false

proc toCodeExprInner(
  self: var Convert,
  nodes: NimNode,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false,
  isSingle: bool=true,
  typeImpl: NimNode=nil,
  inHaveArray: bool=false,
  arrayPass: int=(-1),
  typeName: string="",
  isVarDecl: bool,
): string

proc toCodeExpr(
  self: var Convert,
  nodes: NimNode,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false,
  isVarDecl: bool,
)
proc toCodeStmts(
  self: var Convert,
  nodes: NimNode,
  level: int
)

proc toCodeIfStmt(
  self: var Convert,
  nodes: NimNode,
  level = 0
) =
  var first: bool = true
  for n in nodes:
    case n.kind:
    of nnkEmpty:
      discard
    of nnkElifBranch:
      addIndent(self.res, level)
      if not first:
        self.res.add "else "
      else:
        first = false
      self.res.add "if ("
      self.toCodeExpr(n[0], level, false, isVarDecl=false)
      self.res.add ") {\n"

      addIndent(self.res, level)
      self.toCodeStmts(n[1], level)
      addIndent(self.res, level)
      self.res.add "}\n"
    of nnkElse:
      addIndent(self.res, level)
      self.res.add "else {\n"
      self.toCodeStmts(n[0], level)
      addIndent(self.res, level)
      self.res.add "}\n"
    else:
      fail()
proc toCodeSwitchStmt(
  self: var Convert,
  nodes: NimNode,
  level = 0
) =
  if self.regularC:
    self.res.addIndent(level)
    self.res.add "switch ("
    self.toCodeExpr(
      nodes=nodes[0],
      level=level,
      isLhs=false,
      isTypeInst=false,
      isVarDecl=false,
    )
    self.res.add ") {\n"
    for n in nodes[1 .. ^1]:
      self.res.addIndent(level)
      case n.kind:
      of nnkOfBranch:
        self.res.add "case "
        self.toCodeExpr(
          nodes=n[0],
          level=level,
          isLhs=false,
          isTypeInst=false,
          isVarDecl=false,
        )
        self.res.add ":\n"
        if n.len() > 2:
          for ofN in n[1 ..< ^1]:
            self.res.addIndent(level)
            self.res.add "case "
            self.toCodeExpr(
              nodes=n[0],
              level=level,
              isLhs=false,
              isTypeInst=false,
              isVarDecl=false,
            )
            self.res.add ":\n"
      of nnkElse:
        self.res.add "default:\n"
      else:
        echo n.treeRepr()
        fail()
      self.toCodeStmts(
        nodes=n[^1],
        level=level #+ 1
      )
      self.res.addIndent(level + 1)
      self.res.add "break;\n"
    self.res.addIndent(level)
    self.res.add "}\n"
  else: # if not self.regularC:
    let myLhs = self.toCodeExprInner(
      nodes=nodes[0],
      level=level,
      isLhs=false,
      isTypeInst=false,
      isVarDecl=false,
    )
    var first: bool = true
    for n in nodes[1 .. ^1]:
      self.res.addIndent(level)
      case n.kind:
      of nnkOfBranch:
        if first:
          first = false
        else:
          self.res.add "else "
        self.res.add "if ("
        for i in 0 ..< n.len() - 1:
          self.res.addIndent(level + 1)
          if n.len() > 2:
            if i > 0:
              self.res.add "|| "
            self.res.add "("
          else:
            self.res.addIndent(level)
          self.res.add myLhs
          self.res.add " == "
          self.toCodeExpr(
            nodes=n[i],
            level=level,
            islhs=false,
            isTypeInst=false,
            isVarDecl=false,
          )
          if n.len() > 2:
            self.res.add ")\n"
        self.res.addIndent(level)
        self.res.add ") {\n"
        self.toCodeStmts(
          nodes=n[^1],
          level=level,
        )
        self.res.addIndent(level)
        self.res.add "}\n"
      of nnkElse:
        if nodes.len() > 2:
          self.res.add "else {\n"
          self.toCodeStmts(
            nodes=n[^1],
            level=level,
          )
          self.res.addIndent(level)
          self.res.add "}\n"
        else:
          self.res.add "{\n"
          self.toCodeStmts(
            nodes=n[^1],
            level=level,
          )
          self.res.addIndent(level)
          self.res.add "}\n"
      else:
        echo n.treeRepr()
        fail()
proc toCodeWhileStmt(
  self: var Convert,
  nodes: NimNode,
  level = 0
) =
  let n = nodes
  self.res.addIndent(level)
  self.res.add "while ("
  self.toCodeExpr(
    nodes=n[0],
    level=level,
    isLhs=false,
    isTypeInst=false,
    isVarDecl=false,
  )
  self.res.add ") {\n"
  for innerN in n[1 .. ^1]:
    self.toCodeStmts(
      nodes=innerN,
      level=level,
    )
  self.res.addIndent(level)
  self.res.add "}\n"
  
proc toCodeForStmt(
  self: var Convert,
  nodes: NimNode,
  level = 0
) = 
  let n = nodes
  if have(n, @[nnkSym, nnkInfix]):
    let mySym = n[0].repr()
    addIndent(self.res, level)
    addIndent(self.res, level)
    addIndent(self.res, level)
    self.res.add "for ("
    self.res.add "int32_t "
    self.res.add mySym
    self.res.add "="
    var valid: bool = true
    if valid:
      valid = (
        have(n, @[nnkInfix], 1)
      )
    if valid:
      valid = (
        n[1][0].kind == nnkSym
      )
    var isLte: bool = false
    if valid:
      isLte = (
        n[1][0].strVal == ".."
      )
      valid = (
        (
          isLte
        ) or (
          n[1][0].strVal == "..<"
        )
      )
    if valid:
      self.toCodeExpr(
        nodes=n[1][1],
        level=level,
        isLhs=false,
        isTypeInst=false,
        isVarDecl=false,
      )
      self.res.add "; "
      self.res.add mySym
      if not isLte:
        self.res.add "<"
      else:
        self.res.add "<="
      self.toCodeExpr(
        nodes=n[1][2],
        level=level,
        isLhs=false,
        isTypeInst=false,
        isVarDecl=false,
      )
      self.res.add "; "
      self.res.add mySym
      self.res.add "+=1) {\n"
      for innerN in n[2 .. ^1]:
        self.toCodeStmts(
          nodes=innerN,
          level=level
        )
      self.res.addIndent(level)
      self.res.add "}"
    else:
      fail()
  else:
    fail()

proc typeRenameInner(
  s: string
): (bool, string) =
  case s:
  of "char":
    result = (true, "char")
  of "bool":
    result = (true, "bool")
  of "int":
    result = (true, "int32_t")
  of "uint":
    result = (true, "uint32_t")
  of "int8":
    result = (true, "int8_t")
  of "uint8":
    result = (true, "uint8_t")
  of "int16":
    result = (true, "int16_t")
  of "uint16":
    result = (true, "uint16_t")
  of "int32":
    result = (true, "int32_t")
  of "uint32":
    result = (true, "uint32_t")
  of "int64":
    result = (true, "int64_t")
  of "uint64":
    result = (true, "uint64_t")
  of "float32", "float":
    result = (true, "float")
  else:
    result = (false, s)
proc typeRename(
  s: string
): string =
  typeRenameInner(s)[1]


proc funcRenameIter(
  self: var Convert,
  paramType: NimNode,
  procName: var string,
  first: var bool,
  isVarDecl: bool,
): string =
  result = self.toCodeExprInner(
    nodes=paramType,
    level=0,
    isLhs=false,
    isTypeInst=true,
    isSingle=true,
    isVarDecl=isVarDecl,
  )
  if paramType.getTypeImpl().kind != nnkEnumTy:
    procName.add "_c"
  procName.add self.toCodeExprInner(
    nodes=paramType,
    level=0,
    isLhs=false,
    isTypeInst=true,
    isSingle=true,
    isVarDecl=false,
  )


proc toCodeExprInner(
  self: var Convert,
  nodes: NimNode,
  level: int,
  isLhs: static bool,
  isTypeInst: bool,
  isSingle: bool,
  typeImpl: NimNode=nil,
  inHaveArray: bool=false,
  arrayPass: int=(-1),
  typeName: string="",
  isVarDecl: bool,
): string =
  var n = nodes
  proc getHaveArray(
    someN: NimNode
  ): bool =
    var haveArray: bool = (
      have(someN, @[nnkSym, nnkInfix])
    )
    if haveArray:
      haveArray = (
        someN[0].strVal == "array"
      )
    if haveArray:
      haveArray = (
        someN[1].len == 3
      )
    if haveArray:
      haveArray = (
        have(someN[1], @[nnkIdent, nnkIntLit, nnkIntLit])
      )
    if haveArray:
      haveArray = (
        someN[1][0].strVal == ".."
      )
    result = haveArray
  self.hadArray = false
  if isTypeInst:
    if typeName.len == 0:
      let tempTypeImpl = n.getTypeImpl()
      let tempTypeInst = n.getTypeInst()
      if tempTypeImpl.kind == nnkObjectTy:
        let myTypeName = self.toCodeExprInner(
          nodes=tempTypeInst,
          level=level,
          isLhs=isLhs,
          isTypeInst=isTypeInst,
          isSingle=false,
          typeImpl=nil,
          inHaveArray=false,
          arrayPass=arrayPass,
          typeName="make-it-longer-than-zero-chars",
          isVarDecl=isVarDecl,
        )
        discard self.toCodeExprInner(
          nodes=tempTypeImpl,
          level=level,
          isLhs=isLhs,
          isTypeInst=isTypeInst,
          isSingle=false,
          typeImpl=nil,
          inHaveArray=false,
          arrayPass=arrayPass,
          typeName=myTypeName,
          isVarDecl=isVarDecl,
        )
    proc innerHandleIdentSym(
      someN: NimNode,
      someIsSingle: bool,
      someTypeImpl: NimNode=nil,
    ): string =
      var tempStr: (bool, string) = typeRenameInner(someN.strVal)
      if someN.kind == nnkIdent:
        if someTypeImpl != nil:
          discard
      let haveEnumTy = (
        someN.getTypeImpl().kind != nnkEnumTy
      )
      if (
        (
          haveEnumTy
        ) or (
          (
            someIsSingle
          ) and (
            (
              tempStr[0]
            ) or (
              tempStr[1].find("_") == -1
            )
          )
        )
      ):
        result.add tempStr[1]
      else:
        result.add tempStr[1].replace("_", "_a")
        result.add "_c"

    case n.kind:
    of nnkIdent, nnkSym:
      result.add innerHandleIdentSym(
        someN=n,
        someIsSingle=isSingle,
        someTypeImpl=typeImpl,
      )
    of nnkBracketExpr:
      let tempHadArray = getHaveArray(someN=n)
      self.hadArray = tempHadArray
      if tempHadArray:
        if inHaveArray:
          fail()
        proc handleArray(
          self: var Convert,
          nodes: NimNode,
          ret: var seq[string],
          level: int,
        ) =
          let n = nodes
          proc doLastIter(
            self: var Convert,
            nodes: NimNode,
            ret: var seq[string],
            level: int,
          ) =
            if arrayPass == 0:
              discard
            ret.add self.toCodeExprInner(
              nodes=nodes,
              level=level,
              isLhs=isLhs,
              isTypeInst=isTypeInst,
              isSingle=isSingle,
              typeImpl=nil, # we already have the implementation I think?
              inHaveArray=true,
              arrayPass=arrayPass,
              isVarDecl=isVarDecl,
            )

          case n.kind:
          of nnkBracketExpr:
            if getHaveArray(someN=n):
              if arrayPass == 0:
                discard
              ret.add $(n[1][2].intVal - n[1][1].intVal + 1)
              self.handleArray(
                nodes=n[2],
                ret=ret,
                level=level,
              )
            else:
              if arrayPass == 0:
                discard
              self.doLastIter(
                nodes=n,
                ret=ret,
                level=level,
              )
          else:
            if arrayPass == 0:
              discard
            self.doLastIter(
              nodes=n,
              ret=ret,
              level=level,
            )

        var mySeq: seq[string]
        self.handleArray(
          nodes=n,
          ret=mySeq,
          level=level
        )
        if mySeq.len > 1:
          if arrayPass == 0:
            result.add mySeq[^1]
            self.hadArray = true
          elif arrayPass == 1:
            for i in 0 ..< mySeq.len - 1:
              result.add "["
              result.add mySeq[i]
              result.add "]"
          else:
            echo "#----"
            echo n.treeRepr
            echo "----"
            fail()
        else:
          fail()
      else: # if not haveArray
        case n[0].kind:
        of nnkIdent, nnkSym:
          if typeImpl != nil:
            result.add innerHandleIdentSym(
              someN=n[0],
              someIsSingle=false,
              someTypeImpl=typeImpl[0],
            )
          else:
            result.add innerHandleIdentSym(
              someN=n[0],
              someIsSingle=false,
              someTypeImpl=nil
            )
        else:
          result.add self.toCodeExprInner(
            nodes=n[0],
            level=level,
            isLhs=isLhs,
            isTypeInst=true,
            isSingle=false,
            typeImpl=typeImpl,
            isVarDecl=isVarDecl,
          )
        result.add "_b"
        for i in 1 ..< n.len():
          case n[i].kind:
          of nnkIdent, nnkSym:
            if typeImpl != nil:
              result.add innerHandleIdentSym(
                someN=n[i],
                someIsSingle=false,
                someTypeImpl=typeImpl[i],
              )
            else:
              result.add innerHandleIdentSym(
                someN=n[i],
                someIsSingle=false,
                someTypeImpl=nil
              )
          else:
            result.add self.toCodeExprInner(
              nodes=n[i],
              level=level,
              isLhs=isLhs,
              isTypeInst=true,
              isSingle=false,
              typeImpl=typeImpl,
              isVarDecl=isVarDecl,
            )
        result.add "_d"
    of nnkUIntLit, nnkUInt8Lit, nnkUInt16Lit, nnkUInt32Lit, nnkUInt64Lit:
      #result.add("((int)" & $n.intVal & ")")
      # (old concept for the transpiler, couldn't get it to work!):
      # only allow unsigned integer literals
      result.add($n.intVal)
    of nnkIntLit, nnkInt8Lit, nnkInt16Lit, nnkInt32Lit, nnkInt64Lit:
      let tempIntVal = n.intVal
      if tempIntVal < 0:
        fail()
      else:
        result.add($tempIntVal)
    of nnkInfix:
      var valid: bool = true
      if valid:
        valid = have(n, @[nnkIdent, nnkIntLit, nnkIntLit])
      if valid:
        valid = n[0].repr() == ".."

      if valid:
        result.add $(n[2].intVal - n[1].intVal + 1)
      else:
        fail()
    of nnkCall:
      let innerTypeImpl = n.getTypeImpl()
      let innerTypeInst = n.getTypeInst()
      if n.len >= 3:
        if n[0].kind == nnkOpenSymChoice:
          result.add self.toCodeExprInner(
            nodes=innerTypeInst,
            level=level,
            isLhs=isLhs,
            isTypeInst=isTypeInst,
            isSingle=isSingle,
            typeImpl=innerTypeImpl,
            arrayPass=arrayPass,
            isVarDecl=isVarDecl,
          )
        else:
          fail()
      else:
        fail()
    of nnkObjectTy:
      var toAdd: string
      toAdd.add "typedef struct "
      toAdd.add typeName
      toAdd.add " {\n"
      for memberDef in n[2]:
        self.hadArray = false
        toAdd.addIndent(1)
        var tempToAdd: string
        let haveEnum = (memberDef[1].getTypeImpl().kind == nnkEnumTy)
        if not haveEnum:
          tempToAdd = self.toCodeExprInner(
            memberDef[1].getTypeInst(),
            level, isLhs=false, isSingle=true, isTypeInst=true,
            arrayPass=0,
            isVarDecl=isVarDecl,
          )
        else:
          tempToAdd = self.toCodeExprInner(
            memberDef[1].getTypeImpl(),
            level, isLhs=false, isSingle=true, isTypeInst=true,
            arrayPass=0, typeName=memberDef[1].getTypeInst().repr,
            isVarDecl=isVarDecl,
          )
          
        toAdd.add tempToAdd
        let temp = self.hadArray
        toAdd.add " "
        tempToAdd = self.toCodeExprInner(
          memberDef[0], level, isLhs=true,
          isVarDecl=isVarDecl,
        )
        toAdd.add tempToAdd
        if temp:
          if not haveEnum:
            toAdd.add self.toCodeExprInner(
              memberDef[1].getTypeInst(),
              level, isLhs=false, isSingle=true, isTypeInst=true,
              arrayPass=1,
              isVarDecl=isVarDecl,
            )
          else:
            toAdd.add self.toCodeExprInner(
              memberDef[1].getTypeImpl(),
              level, isLhs=false, isSingle=true, isTypeInst=true,
              arrayPass=1, typeName=memberDef[1].getTypeInst().repr,
              isVarDecl=isVarDecl,
            )

        toAdd.add ";\n"
      toAdd.add "} "
      toAdd.add typeName
      toAdd.add ";\n"
      if typeName notin self.typedefTbl:
        self.typedefSeq.add toAdd
        self.typedefTbl[typeName] = ""
    of nnkEnumTy:
      var toAdd: string
      toAdd.add "typedef enum "
      toAdd.add typeName
      toAdd.add " {\n"
      for item in n.children:
        case item.kind:
        of nnkEmpty:
          discard
        of nnkSym:
          toAdd.addIndent(1)
          toAdd.add item.repr
          toAdd.add ",\n"
        else:
          let n = item
          fail()
      toAdd.add "} "
      toAdd.add typeName
      toAdd.add ";\n"
      if typeName notin self.typedefTbl:
        self.typedefSeq.add toAdd
        self.typedefTbl[typeName] = ""
      result = typeName
    of nnkEmpty:
      discard
    of nnkPtrTy:
      if self.regularC:
        result.add self.toCodeExprInner(
          nodes=nodes[0],
          level=level,
          isLhs=isLhs,
          isTypeInst=isTypeInst,
          isSingle=isSingle,
          typeImpl=typeImpl,
          inHaveArray=inHaveArray,
          arrayPass=arrayPass,
          typeName=typeName,
          isVarDecl=isVarDecl,
        )
        if isVarDecl:
          result.add "*"
        else:
          result.add "_p"
      else:
        echo "typeinst nnkPtrTy:"
        echo n.treeRepr
        errFail(
          "error: regular C mode not enabled"
        )
    else:
      echo "typeinst nnkOther:"
      echo n.treeRepr
      fail()

    return result

  case n.kind:
  of nnkIdent, nnkSym:
    result.add n.strVal
  of nnkInfix:
    if n[0].repr in ["+=", "-=", "*=", "/="]:
      #self.toCodeExprInner(n[1], level + 1, isLhs)
      #result.add " "
      #self
      # TODO: maybe support this?
      fail()
    else:
      result.add "("
      result.add self.toCodeExprInner(
        n[1], level, isLhs,
        isVarDecl=isVarDecl,
      )
      result.add " "
      if n[0].repr in ["mod"]:
        if n[1].getType().repr != "int":
          result.add("fmod")
        else:
          result.add "%"
      elif n[0].repr == "div":
        result.add "/"
      elif n[0].repr == "shl":
        result.add "<<"
      elif n[0].repr == "shr":
        result.add ">>"
      elif n[0].repr == "and":
        if n[1].getType().repr != "bool":
          result.add "&"
        else:
          result.add "&&"
      elif n[0].repr == "or":
        if n[1].getType().repr != "bool":
          result.add "|"
        else:
          result.add "||"
      elif n[0].repr == "xor":
        result.add "^"
      else:
        result.add(n[0].strVal)
      result.add " "
      result.add self.toCodeExprInner(
        n[2], level, isLhs,
        isVarDecl=isVarDecl,
      )
      result.add ")"
  of nnkAddr:
    if self.regularC:
      result.add "(&"
      result.add self.toCodeExprInner(
        n[0], level, isLhs, isVarDecl=isVarDecl
      )
      result.add ")"
    else:
      errFail(
        "error: regular C mode not enabled"
      )
  of nnkDerefExpr:
    if self.regularC:
      result.add "(*"
      result.add self.toCodeExprInner(
        n[0], level, isLhs,
        isVarDecl=isVarDecl,
      )
      result.add ")"
    else:
      errFail(
        "error: regular C mode not enabled"
      )
  of nnkDotExpr:
    result.add "("
    if n[0].kind == nnkDerefExpr:
      result.add self.toCodeExprInner(
        n[0][0], level, isLhs,
        isVarDecl=isVarDecl,
      )
      result.add "->"
    else:
      result.add self.toCodeExprInner(
        n[0], level, isLhs,
        isVarDecl=isVarDecl,
      )
      result.add "."
    result.add self.toCodeExprInner(
      n[1], level, isLhs,
      isVarDecl=isVarDecl,
    )
    result.add ")"
  of nnkBracketExpr:
    #when not isTypeInst:
    result.add self.toCodeExprInner(
      n[0], level, isLhs,
      isVarDecl=isVarDecl,
    )
    result.add "["
    result.add self.toCodeExprInner(
      n[1], level, false,
      isVarDecl=isVarDecl,
    )
    result.add "]"
  of nnkBracket:
    result.add "{"
    for i in 0 ..< n.len():
      result.add self.toCodeExprInner(
        n[i], level, isLhs,
        isVarDecl=isVarDecl,
      )
      if i + 1 < n.len():
        result.add ", "
    result.add "}"
  of nnkChckRange:
    # skip check range and treat it as a hidden cast instead
    var typeStr = typeRename(n.getType.repr)
    result.add "(("
    result.add typeStr
    result.add ")"
    result.add self.toCodeExprInner(
      n[0], level, false,
      isVarDecl=isVarDecl,
    )
    result.add ")"
  of nnkHiddenStdConv:
    var typeStr = typeRename(n.getType.repr)
    if typeStr.startsWith("range["):
      result.add self.toCodeExprInner(
        n[1], level, false,
        isVarDecl=isVarDecl,
      )
    else:
      for j in 1 .. n.len-1:
        result.add "(("
        result.add typeStr
        result.add ")"
        result.add self.toCodeExprInner(
          n[1], level, false,
          isVarDecl=isVarDecl,
        )
        result.add ")"

  else:
    when not isLhs:
      case n.kind:
      of nnkConv:
        result.add "(("
        result.add self.toCodeExprInner(
          nodes=n[0].getTypeInst(),
          level=level,
          isLhs=false,
          isTypeInst=true,
          isVarDecl=isVarDecl,
        )
        result.add ")"
        result.add self.toCodeExprInner(
          nodes=n[1],
          level=level,
          isLhs=false,
          isTypeInst=false,
          isVarDecl=isVarDecl,
        )
        result.add ")"
      of nnkIntLit:
        #result.add("((int)" & $n.intVal & ")")
        result.add($n.intVal)
      of nnkUIntLit:
        result.add("((uint32_t)" & $n.intVal & ")")
      of nnkInt8Lit:
        result.add("((int8_t)" & $n.intVal & ")")
      of nnkUInt8Lit:
        result.add("((uint8_t)" & $n.intVal & ")")
      of nnkInt16Lit:
        result.add("((int16_t)" & $n.intVal & ")")
      of nnkUInt16Lit:
        result.add("((uint16_t)" & $n.intVal & ")")
      of nnkInt32Lit:
        result.add("((int32_t)" & $n.intVal & ")")
      of nnkUInt32Lit:
        result.add("((uint32_t)" & $n.intVal & ")")
      of nnkInt64Lit:
        result.add("((int64_t)" & $n.intVal & ")")
      of nnkUInt64Lit:
        result.add("((uint64_t)" & $n.intVal & ")")
      of nnkFloatLit, nnkFloat32Lit:
        result.add("((float)" & $n.floatVal & ")")
      #of nnkFloat32Lit:
      #  discard
      #of nnkFloat64Lit:
      #  discard
      of nnkObjConstr:
        var typeName: string
        typeName = self.toCodeExprInner(
          n[0], level, isLhs, true, true,
          isVarDecl=isVarDecl,
        )
        result.add "{"
        for i in 1 ..< n.len:
          var paramType: NimNode
          case n[i].kind:
          of nnkExprColonExpr:
            paramType = n[i][1].getTypeInst()
          else:
            paramType = n[i].getTypeInst()

          result.add self.toCodeExprInner(
            n[i][1], level, isLhs,
            isVarDecl=isVarDecl,
          )
          if i + 1 < n.len:
            result.add ", "
        result.add "}"
      of nnkCall:
        var procName: string = n[0].strVal #& "_f"
        var origProcName: string = procName
        let procTypeInst = n[0].getTypeInst()
        var first: bool = true
        var myHaveArrLen: bool = true
        if myHaveArrLen:
          myHaveArrLen = have(n, @[nnkSym])
        if myHaveArrLen:
          myHaveArrLen = (
            n[0].repr() == "len"
          )
        if myHaveArrLen:
          myHaveArrLen = (
            have(procTypeInst, @[nnkFormalParams])
          )
        if myHaveArrLen:
          myHaveArrLen = (
            have(procTypeInst[0], @[nnkSym, nnkIdentDefs])
          )
        if myHaveArrLen:
          myHaveArrLen = (
            procTypeInst[0][1].len == 3
          )
        if myHaveArrLen:
          myHaveArrLen = (
            getHaveArray(procTypeInst[0][1][1])
          )
          
        if myHaveArrLen:
          result = $(
            (
              procTypeInst[0][1][1][1][2].intVal
            ) - (
              procTypeInst[0][1][1][1][1].intVal
            ) + (
              1
            )
          )
        else:
          for i in 1 ..< n.len:
            var paramType: NimNode
            case n[i].kind:
            of nnkExprEqExpr:
              paramType = n[i][1].getTypeInst()
            else:
              paramType = n[i].getTypeInst()

            if paramType.kind == nnkVarTy:
              let n = paramType
              fail()
            else:
              discard self.funcRenameIter(
                paramType=paramType,
                procName=procName,
                first=first,
                isVarDecl=false,
              )
          var returnType: string
          returnType = self.toCodeExprInner(
            nodes=n[0].getTypeInst()[0][0],
            level=level,
            isLhs=false,
            isTypeInst=true,
            isVarDecl=isVarDecl,
          )
          if not self.noMangleTbl[origProcName]:
            procName = procName & "_g" & returnType
          else:
            procName = origProcName
          result.add procName
          result.add "("
          for i in 1 ..< n.len:
            case n[i].kind:
            of nnkExprEqExpr:
              result.add self.toCodeExprInner(
                n[i][1], level, isLhs,
                isVarDecl=isVarDecl,
              )
            else:
              result.add self.toCodeExprInner(
                n[i], level, isLhs,
                isVarDecl=isVarDecl,
              )
            if i + 1 < n.len:
              result.add ", "
          result.add ")"
      of nnkStmtListExpr:
        if (
          (
            have(n, @[nnkEmpty])
          ) and (
            n.len() == 2
          )
        ):
          result.add self.toCodeExprInner(
            n[1], level, false, isVarDecl=false
          )
        else:
          echo repr(n)
          echo n.treeRepr
          fail()
      else:
        echo repr(n)
        echo n.treeRepr
        fail()
    else:
      fail()

proc toCodeExpr(
  self: var Convert,
  nodes: NimNode,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false,
  isVarDecl: bool,
) =
  self.res.add self.toCodeExprInner(
    nodes, level, isLhs, isTypeInst,
    isVarDecl=isVarDecl,
  )
  discard

proc toCodeAsgn(
  self: var Convert,
  nodes: NimNode,
  level: int,
) =
  addIndent(self.res, level)
  let n = nodes
  case nodes.len:
    of 2:
      self.toCodeExpr(
        nodes=n[0],
        level=level,
        isLhs=true,
        isVarDecl=false,
      )
      self.res.add " = "
      self.toCodeExpr(
        nodes=n[1],
        level=level,
        isLhs=false,
        isVarDecl=false,
      )
    else:
      fail()
  #self.res.addSmart(';')
  self.res.add ";"
      
proc toCodeTypeSection(
  self: var Convert,
  nodes: NimNode,
  level = 0,
) =
  discard

proc toCodeVarSection(
  self: var Convert,
  nodes: NimNode,
  level = 0,
) =
  addIndent(self.res, level)
  let n = nodes
  case n.kind:
  of nnkIdentDefs:
    case n.len:
    of 3:
      let tempCond = (
        (
          have(n, @[nnkSym]) 
        ) or (
          have(n, @[nnkPragmaExpr])
        )
      )
      if tempCond:
        var tempToAdd: string
        if have(n, @[nnkPragmaExpr]):
          if have(n[0], @[nnkPragma], 1):
            proc myInnerFunc(
              self: var Convert,
              someN: NimNode,
            ) =
              case someN.repr():
              of "cstatic":
                self.res.add "static "
              of "cconst":
                self.res.add "const "
              else:
                let n = someN
                fail()
            if n[0][1][0].kind == nnkCall:
              self.myInnerFunc(
                someN=n[0][1][0][0]
              )
            else:
              self.myInnerFunc(
                someN=n[0][1][0]
              )
        self.hadArray = false
        var tempN: NimNode
        if not have(n, @[nnkEmpty], 1):
          tempN = n[1]
        else:
          tempN = n[2]
        tempToAdd = self.toCodeExprInner(
          tempN.getTypeInst(),
          level, isLhs=false, isTypeInst=true, arrayPass=0,
          isVarDecl=true,
        )
        self.res.add tempToAdd
        let temp = self.hadArray
        self.res.add " "
        var mySym: NimNode
        if have(n, @[nnkSym]):
          mySym = n[0]
        elif have(n, @[nnkPragmaExpr]):
          mySym = n[0][0]
        else:
          fail()
        tempToAdd = self.toCodeExprInner(
          mySym, level, isLhs=true, isVarDecl=true,
        )
        self.res.add tempToAdd
        if temp:
          self.res.add self.toCodeExprInner(
            tempN.getTypeInst(),
            level, isLhs=false, isTypeInst=true, arrayPass=1,
            isVarDecl=true,
          )
        if not have(n, @[nnkEmpty], 2):
          self.res.add " = "
          self.toCodeExpr(n[2], level, isLhs=false, isVarDecl=true)
        self.res.add ";\n"
      else:
        fail()
    else:
      #echo "disallowed for now"
      #echo n
      fail()
  else:
    fail()

type
  PragmaFlagKind = enum
    pfkStatic,
    pfkExtern,
    pfkNoDecl,
    pfkNoMangle,
    pfkLim,
proc toCodePragmaStmtInner(
  self: var Convert,
  nodes: NimNode,
  level: int,
): (array[pfkLim, bool], string) =
  let n = nodes
  result[0] = [false, false, false, false]
  for item in n.children:
    case item.kind:
    of nnkIdent, nnkSym:
      case item.repr():
      of "cstatic":
        result[1].add "static "
        result[0][pfkStatic] = true
      of "cconst":
        result[1].add "const "
      of "cextern":
        result[0][pfkExtern] = true
      of "cnodecl":
        result[0][pfkNoDecl] = true
      of "cnomangle":
        result[0][pfkNoMangle] = true
      else:
        let n = item
        fail()
    of nnkExprColonExpr:
      case item[0].repr():
      of "craw":
        result[1].add item[1].strVal
      of "cmainmhz":
        result[1].add "#pragma MAIN_MHZ "
        result[1].add self.procName
        result[1].add " "
        result[1].add item[1].strVal
        result[1].add "\n"
      of "importc":
        discard
      else:
        let n = item
        fail()
    of nnkCall:
      let tempResult = self.toCodePragmaStmtInner(
        nodes=item,
        level=level,
      )
      for idx in 0 ..< tempResult[0].len():
        let myPfk = PragmaFlagKind(idx)
        if tempResult[0][myPfk]:
          result[0][myPfk] = true
      result[1].add tempResult[1]
    else:
      fail()

proc toCodePragmaStmt(
  self: var Convert,
  nodes: NimNode,
  level: int,
) =
  self.res.add self.toCodePragmaStmtInner(
    nodes=nodes, level=level,
  )[1]
proc toCodeStmts(
  self: var Convert,
  nodes: NimNode,
  level: int
) = 
  addIndent(self.res, level)
  proc innerFunc(
    self: var Convert,
    n: NimNode,
    level: int
  ) =
    case n.kind:
    of nnkEmpty:
      discard
    of nnkSym:
      discard
    of nnkPragma:
      self.toCodePragmaStmt(nodes=n, level=level + 1)
    of nnkIfStmt:
      self.toCodeIfStmt(n, level + 1)
    of nnkCaseStmt:
      self.toCodeSwitchStmt(n, level + 1)
    of nnkWhileStmt:
      self.toCodeWhileStmt(n, level + 1)
    of nnkForStmt:
      self.toCodeForStmt(n, level + 1)
    of nnkTypeSection:
      self.toCodeTypeSection(n[0], level + 1)
    of nnkVarSection, nnkLetSection:
      self.toCodeVarSection(n[0], level + 1)
    of nnkAsgn:
      self.toCodeAsgn(n, level + 1)
    of nnkCall:
      self.toCodeExpr(n, level + 1, false, isVarDecl=false)
      self.res.add ";"
    of nnkBracket:
      discard
    of nnkCommentStmt:
      discard
    of nnkDiscardStmt:
      discard
    of nnkReturnStmt:
      self.toCodeStmts(n[0], level)
    else:
      fail()

  case nodes.kind:
    of nnkStmtList:
      for n in nodes:
        self.innerFunc(n, level)
    of nnkCommentStmt:
      discard
    else:
      self.res.addIndent level
      self.innerFunc(nodes, level)
      self.res.addSmart ';'
      self.res.add "\n"
      

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
  procNode: NimNode,
  typeImpl: NimNode,
  pass: int,
): (string, string, bool) =
  var procName = ""
  var origProcName = ""
  var paramsStr = ""
  var returnType = "void"

  assert procNode.kind in {
    nnkProcDef, nnkFuncDef
  }
  assert typeImpl.kind in {
    nnkProcTy
  }
  var myProcPragmaSeq: seq[NimNode]
  var pragmaFlagArr: array[pfkLim, bool]
  var haveGenerics: bool = false

  var foundElse: bool = false
  for n in procNode:
    case n.kind:
    of nnkEmpty:
      discard
    of nnkPragma:
      #echo "procDef() nnkPragma:"
      #echo n.treeRepr()
      # TODO: come back to this later
      #myProcPragmaStr.add self.toCodePragmaStmtInner(
      #  nodes=n, level=0, procName=#isProc=true
      #)
      pragmaFlagArr = self.toCodePragmaStmtInner(
        nodes=n, level=0, #procName=isProc=true
      )[0]
      if (
        (
          pragmaFlagArr[pfkExtern]
        ) and (
          pragmaFlagArr[pfkStatic]
        )
      ):
        defer:
          errFail(
            (
              "error: can't have both \"cextern\" and \"cstatic\" "
            ) & (
              "for a `proc`/`func`: "
            ) & (
              origProcName
            )
          )
      myProcPragmaSeq.add n
      if (
        (
          (
            pragmaFlagArr[pfkExtern]
          ) or (
            pragmaFlagArr[pfkNoMangle]
          )
        ) and (
          haveGenerics
        )
      ):
        defer:
          errFail(
            (
              "error: can't have generics with "
            ) & (
              "\"cextern\" and/or \"cnomangle\": "
            ) & (
              origProcName
            )
          )
    of nnkSym:
      procName = $n
      origProcName = procName
    of nnkAccQuoted:
      if n.len == 1:
        procName = $n[0]
        origProcName = procName
      else:
        fail()
    of nnkBracket:
      if (
        (
          n[0].kind == nnkEmpty
        ) and (
          n[1].kind == nnkGenericParams
        )
      ):
        if pragmaFlagArr[pfkExtern] or pragmaFlagArr[pfkNoMangle]:
          defer:
            errFail(
              (
                "error: can't have generics with "
              ) & (
                "\"cextern\" and/or \"cnomangle\": "
              ) & (
                origProcName
              )
            )
        let n = n[1]
        haveGenerics = true
        if n[0].kind == nnkIdentDefs:
          discard
        else:
          fail()
      else:
        fail()
    of nnkFormalParams:
      if pass == 1:
        if n[0].kind != nnkEmpty:
          returnType = self.toCodeExprInner(
            nodes=typeImpl[0][0],
            level=0,
            isLhs=false,
            isTypeInst=true,
            isVarDecl=true,
          )
        var idx = 1
        for paramDef in n[1 .. ^1]:
          # the paramDef is like `x, y, z: float`
          #echo "dbg: nnkFormalParams:"
          #echo n.treeRepr
          var first: bool = true
          if paramDef.kind != nnkEmpty:
            #echo "paramDef start"
            ##echo paramDef[0]
            #echo "paramDef end"
            for param in paramDef[0 ..< ^2]:
              # Process each `x`, `y`, `z` in a loop
              paramsStr.add "  "
              proc findParamName(
                paramRepr: string
              ): string =
                var myLast = paramRepr.find(' ')
                #last = if paramRepr.find(' ') < 0
                result = paramRepr.substr(
                  first=0,
                  last=if myLast < 0: paramRepr.high else: myLast - 1
                )
              let paramName = findParamName(param.repr()) #param.repr()
              #echo "dbg: paramName: " & paramName
              #echo "dbg: param: start: " & $param.len()
              #for paramElem in param:
              #  echo paramElem.treeRepr()
              #echo "dbg: param: end"
              #echo "----"
              #dumpAstNode(n)
              #dumpTree(n)
              case typeImpl[0][idx].kind:
              of nnkIdentDefs:
                #echo "dbg: nnkIdentDefs:"
                #echo typeImpl[0][idx].treeRepr
                paramsStr.add(
                  self.funcRenameIter(
                    paramType=(
                      typeImpl[0][idx][1]
                    ),
                    procName=procName,
                    first=first,
                    isVarDecl=true,
                  )
                )
                if self.cppConstRefInp:
                  paramsStr.add " IN"
              else:
                discard
              paramsStr.add " "
              paramsStr.add paramName
              paramsStr.add ",\n"
              idx += 1
      if paramsStr.len > 0:
        # remove the final ",\n" since C doesn't support that
        paramsStr.setLen(paramsStr.len - 2)
        paramsStr.add "\n"
    else:
      if pass == 0:
        if origProcName notin self.noMangleTbl:
          self.noMangleTbl[origProcName] = pragmaFlagArr[pfkNoMangle]
          self.noMangleProtoTbl[origProcName] = ""
      else: # if pass == 1:
        foundElse = true
        self.res.setLen(0)
        self.res.add "\n"
        let tempProcName = procName & "_g" & returnType 
        if self.noMangleTbl[origProcName]:
          let proto = self.noMangleProtoTbl[origProcName]
          if proto.len() == 0:
            self.noMangleProtoTbl[origProcName] = tempProcName
          elif proto != tempProcName:
            defer:
              errFail(
                (
                  "error: can't have overloading with \"cnomangle\": "
                ) & (
                  tempProcName
                )
              )

        if not pragmaFlagArr[pfkNoMangle]:
          procName = tempProcName
        else:
          procName = origProcName
        #procName = procName & "_" & returnType
        result[0] = procName

        if pragmaFlagArr[pfkExtern]:
          self.res.add "extern "
        self.res.add returnType & " " & procName & "("
        if paramsStr.len == 0:
          self.res.add "void"
        else:
          self.res.add "\n"
          self.res.add paramsStr
        self.res.add ")"
        if not pragmaFlagArr[pfkExtern]:
          self.res.add " {\n"
          self.useResult = self.hasResult(n)
          if self.useResult:
            self.res.addIndent(1)
            self.res.add returnType
            self.res.add " result;"
          self.procName = procName
          self.toCodeStmts(n, 0)
          if self.useResult:
            if "return result" notin self.res[^20..^1]:
              self.res.addIndent(1)
              self.res.add "return result;\n"
            else:
              self.res.add "\n"
          else:
            self.res.add "\n"
          self.res.add "}"
        else:
          self.res.add ";"

  if not pragmaFlagArr[pfkNoDecl]:
    for procPragma in myProcPragmaSeq:
      result[1].add self.toCodePragmaStmtInner(
        nodes=procPragma, level=0,
      )[1]

    result[1].add self.res

  result[2] = foundElse


proc findTopLevel(
  self: var Convert,
  topLevelNode: NimNode,
  pass: int,
) =
  proc innerFunc(
    self: var Convert,
    n: NimNode,
    pass: int,
  ) = 
    if n.kind != nnkEmpty:
      discard
    if n.kind == nnkCall:
      #dumpTree n
      if n[0].repr() == "[]":
        return
      var procName = n[0].repr()
      if (
        (
          procName in ignoreFuncs
        )
      ):
        return

      case n[0].kind:
      of nnkIdent:
        fail() # can't have 
      of nnkSym:
        let impl = n[0].getImpl()
        let innerTypeImpl = n[0].getTypeImpl()

        var tempImpl: NimNode
        if impl.kind != nnkTemplateDef:
          tempImpl = impl
        else:
          return

        var myProcDef = self.procDef(tempImpl, innerTypeImpl, pass=pass)

        self.findTopLevel(
          impl,
          pass=pass,
        )

        if pass == 1:
          if (
            (
              myProcDef[0] in ignoreFuncs
            ) or (
              myProcDef[0] in self.funcTbl
            ) or (
              not myProcDef[2]
            )
          ):
            return
          self.funcSeq.add myProcDef[1]
          self.funcTbl[myProcDef[0]] = FuncTblElem(
            defn: self.funcSeq[^1],
          )
      else:
        discard
      for innerN in n[1 .. ^1]:
        self.findTopLevel(
          innerN,
          pass=pass
        )

    self.findTopLevel(
      n,
      pass=pass
    )

  for n in topLevelNode:
    self.innerFunc(
      n=n,
      pass=pass
    )

proc toPipelineCInner*(
  s: NimNode,
  regularC: NimNode,
  cppConstRefInp: NimNode,
): string =
  var code: string
  var n = getImpl(s)
  var convert: Convert
  convert.regularC = ($regularC == "true")
  convert.cppConstRefInp = ($cppConstRefInp == "true")
  for pass in 0 .. 1:
    convert.findTopLevel(n, pass=pass)
  code.add "#ifndef __PIPELINEC__\n"
  code.add "#include <stdint.h>\n"
  code.add "#include <stdbool.h>\n"
  code.add "#else\n"
  code.add "#include \"intN_t.h\"\n"
  code.add "#include \"uintN_t.h\"\n"
  code.add "#include \"float_e_m_t.h\"\n"
  code.add "#include \"bool.h\"\n"
  code.add "#endif\n"
  if convert.cppConstRefInp:
    code.add "#ifdef __cplusplus\n" 
    code.add "#define IN const &\n"
    code.add "#else\n"
    code.add "#define IN\n"
    code.add "#endif\n"
  #code.add "#define uint8_t_c uint8_t\n"
  #code.add "#define int8_t_c int8_t\n"
  #code.add "#define uint16_t_c uint16_t\n"
  #code.add "#define int16_t_c int16_t\n"
  #code.add "#define uint32_t_c uint32_t\n"
  #code.add "#define int32_t_c int32_t\n"
  #code.add "#define uint64_t_c uint64_t\n"
  #code.add "#define int64_t_c int64_t\n"
  #code.add "#define float_c float\n"
  for v in convert.typedefSeq:
    #echo "convert test"
    #code.add convert.typedefSeq[i]
    code.add v
    code.add "\n"
    #i -= 1
  code.add "\n"

  for v in convert.funcSeq:
    #echo "this function being output:"
    #echo v
    code.add v
    code.add "\n"
  code.add "\n"

  ##toCodeTopLevel(topLevelNode=n, res=code, level=0)
  #code = convert.res

  #result = quote do:
  #  `code`
  code

macro toPipelineC*(
  s: typed,
  regularC: typed,
  cppConstRefInp: typed,
): untyped =
  #echo s.treeRepr
  newLit(toPipelineCInner(
    s,
    regularC=regularC,
    cppConstRefInp=cppConstRefInp,
  ))
  #echo s.treeRepr
  #result = quote do:
  #result = quote do:
  #  ""
  #echo s.getImpl
  #toPipelineCInner(getImpl(s))
