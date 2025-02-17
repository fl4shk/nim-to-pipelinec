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
  #res: var string,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false,
  isSingle: bool=true,
  typeImpl: NimNode=nil,
  inHaveArray: bool=false,
  #stickyInHaveArray: bool=false
  #varName: ref string=nil,
  arrayPass: int=(-1),
  #hadArray: ref bool=nil,
  typeName: string="",
  isVarDecl: bool,
): string

#proc nextProcRenameTbl(
#  self: var Convert,
#  n: NimNode,
#) =
#  #self.procRenameTbl.clear()
#  echo "nextProcRenameTbl():"
#  echo n.treeRepr
#  if n.kind == nnkProcTy:
#    if n[0].kind == nnkFormalParams:
#      for paramDef in n[0]:
#        echo paramDef.treeRepr
#        if paramDef.kind == nnkBracketExpr:
#          #echo paramDef.
#          #echo paramDef.treeRepr
#          discard
#        #elif paramDef.kind == nnkIdentDefs:
#        #  discard
#        #elif paramDef.kind == 
#        else:
#          #let n = paramDef
#          #fail()
#    else:
#      #n[0].
#      fail()
#  #else:
#  #  fail()

  

proc toCodeExpr(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
  isLhs: static bool,
  isTypeInst: bool=false,
  isVarDecl: bool,
)
proc toCodeStmts(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int
)

proc toCodeIfStmt(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0
) =
  var first: bool = true
  #echo nodes.treeRepr()
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
      #if n[0].kind != nnkStmtListExpr:
      self.toCodeExpr(n[0], level, false, isVarDecl=false)
      #elif (
      #  (
      #    have(n[0], @[nnkEmpty])
      #  ) and (
      #    n.len() == 2
      #  )
      #):
      #  self.toCodeExpr(n[0][1], level, false, isVarDecl=false)
      #else:
      #  #let n = n
      #  fail()
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
  #let n = nodes
  #echo "switch:"
  #echo nodes.treeRepr()
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
        #self.toCodeStmts(
        #  nodes=n[^1],
        #  level=level + 1
        #)
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
      #echo "for loop:", nodes[1 .. ^1].len()
      #echo n.treeRepr()
      #echo "--------"
      self.res.addIndent(level)
      case n.kind:
      of nnkOfBranch:
        if first:
          first = false
        else:
          self.res.add "else "
        self.res.add "if ("
        #for innerN in n[0 ..< ^1]:
        for i in 0 ..< n.len() - 1:
          #if i > 0:
          self.res.addIndent(level + 1)
          if n.len() > 2:
            #self.res.add "\n"
            if i > 0:
              self.res.add "|| "
            self.res.add "("
          else:
            self.res.addIndent(level)
          #self.res.add "("
          self.res.add myLhs
          self.res.add " == "
          self.toCodeExpr(
            nodes=n[i],
            level=level,
            islhs=false,
            isTypeInst=false,
            isVarDecl=false,
          )
          #if i > 0: #
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
        #if nodes[1 .. ^1].len() > 1:
        if nodes.len() > 2:
          self.res.add "else {\n"
          self.toCodeStmts(
            nodes=n[^1],
            level=level,
          )
          self.res.addIndent(level)
          self.res.add "}\n"
        else:
          #echo "test: ", nodes.len()
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
    #self.res.add "{\n"
    addIndent(self.res, level)
    #self.res.add "int32_t "
    #self.res.add mySym
    #self.res.add ";\n"
    addIndent(self.res, level)
    self.res.add "for ("
    self.res.add "int32_t "
    self.res.add mySym
    self.res.add "="
    #echo "#----"
    #echo "test 2:"
    #echo n[1].treeRepr
    #echo "----"
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
      #self.toCodeExpr(nodes=n[1][0]
      #self.toCodeExpr(nodes=n[1], level=level, isLhs=false, isTypeInst=false)

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
          level=level #+ 1
        )
      self.res.addIndent(level)
      #self.res.add "}\n"
      #self.res.addIndent(level)
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
  #origProcName: string,
  procName: var string,
  first: var bool,
  isVarDecl: bool,
): string =
  #if paramType.kind == nnkVarTy:
  #  #echo "test"
  #  fail()
  ##elif paramType.kind == nnkBracketExpr:
  ##  #fail()
  #else:
  #paramsStr.add ""
  #paramsStr.add paramType.strVal
  #echo "#----"
  #echo "funcRenameIter: ", procName, " ", first
  #echo paramType.treeRepr
  #echo "----"
  result = self.toCodeExprInner(
    nodes=paramType,
    level=0,
    isLhs=false,
    isTypeInst=true,
    isSingle=true,
    #typeImpl=paramType,
    isVarDecl=isVarDecl,
  )
  #if first:
  #  first = false
  #  procName.add "_f"
  #  #procName.add "_"
  #else:
  #  #discard
  #echo "funcRenameIter:"
  #echo paramType.kind
  #echo ""
  #echo paramType.getTypeImpl().kind
  #echo ""
  #echo "funcRenameIter:"
  #echo "origProcName: ", origProcName,
  #if not self.noMangleTbl[origProcName]:
  #if not self.noMangleTbl[procName]:
  if paramType.getTypeImpl().kind != nnkEnumTy:
    procName.add "_c"
    #procName.add "_"
  #procName.add result
  procName.add self.toCodeExprInner(
    nodes=paramType,
    level=0,
    isLhs=false,
    isTypeInst=true,
    isSingle=true,
    #typeImpl=paramType,
    isVarDecl=false,
  )
  #echo "funcRenameIter: " & procName
  #paramsStr.add tempParamsStr
  #result = tempParamsStr


proc toCodeExprInner(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int,
  isLhs: static bool,
  isTypeInst: bool,
  isSingle: bool,
  typeImpl: NimNode=nil,
  inHaveArray: bool=false,
  #stickyInHaveArray: bool=false
  #varName: ref string=nil,
  arrayPass: int=(-1),
  #hadArray: ref bool=nil,
  typeName: string="",
  isVarDecl: bool,
): string =
  var n = nodes
  #echo n.kind
  proc getHaveArray(
    someN: NimNode
  ): bool =
    #echo "getHaveArray: begin"
    var haveArray: bool = (
      have(someN, @[nnkSym, nnkInfix])
    )
    if haveArray:
      #echo "getHaveArray: first"
      haveArray = (
        someN[0].strVal == "array"
      )
    if haveArray:
      #echo "getHaveArray: second"
      haveArray = (
        someN[1].len == 3
      )
    if haveArray:
      #echo "getHaveArray: third"
      haveArray = (
        have(someN[1], @[nnkIdent, nnkIntLit, nnkIntLit])
      )
    if haveArray:
      #echo "getHaveArray: fourth"
      haveArray = (
        someN[1][0].strVal == ".."
      )
    result = haveArray
  self.hadArray = false
  if isTypeInst:
    #echo n.getTypeInst().treeRepr
    #if typeImpl != nil:
    #  if typeImpl.kind == nnkObjectTy:
    if typeName.len == 0:
      #echo n.treeRepr()
      let tempTypeImpl = n.getTypeImpl()
      let tempTypeInst = n.getTypeInst()
      #echo "tempTypeImpl:"
      #echo tempTypeImpl.treeRepr
      #echo ""
      #echo "tempTypeInst:"
      #echo tempTypeInst.treeRepr
      #echo "--------"
      if tempTypeImpl.kind == nnkObjectTy:
        #echo "found nnkObjectTy"
        #echo tempTypeInst.treeRepr
        #echo ""
        #echo n.treeRepr
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
        #echo "myTypeName: ", myTypeName
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
          #echo "have nnkIdent with typeImpl:"
          #echo tempStr[1]
          #echo someTypeImpl.treeRepr
          #tempStr[1] = someType
          discard
        #echo someN.getType()
        #echo someN.astGenRepr()
        #echo someN.getImpl()
        #echo someN.getTypeInst()
      #result.add someN.strVal

      #let mainMangleCond = (
      #  (
      #    someN.getTypeImpl().kind != nnkEnumTy
      #  ) and (
      #    not someIsSingle
      #  ) and (
      #    not tempStr[0]
      #  )
      #)
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
        #result.add "_b"
        result.add tempStr[1].replace("_", "_a")
        result.add "_c"

      #if mangleCond:
      #  discard
      #else:
      #  discard

      #if (
      #  (
      #    someN.getTypeImpl().kind != nnkEnumTy
      #  ) and (
      #    not someIsSingle
      #  )
      #):
      #  if not tempStr[0]:
      #    result.add tempStr[1].replace("_", "_a")
      #    #result.add tempStr[1]#.replace("_", "_a")
      #  else:
      #    result.add tempStr[1]
      #  #if not isSingle:
      #  result.add "_c"
      #else:
      #  result.add tempStr[1]
      #  #result.add "_"

    #echo "isTypeInst: " & repr(n)
    case n.kind:
    of nnkIdent, nnkSym:
      #echo "test:"
      #echo n.treeRepr
      result.add innerHandleIdentSym(
        someN=n,
        someIsSingle=isSingle,
        someTypeImpl=typeImpl,
      )
    #of nnkSym:
    #  result.add n.strVal
    of nnkBracketExpr:
      #echo "typeinst nnkBracketExpr: ", arrayPass
      #echo n.treeRepr
      #let innerTypeImpl = n.getTypeImpl()
      #echo innerTypeImpl.treeRepr
      #let innerTypeInst = n.getTypeInst()
      #echo innerTypeInst.treeRepr
      #if innerTypeInst.kind == nnkObjectTy:
      #  discard
      let tempHadArray = getHaveArray(someN=n)
      #echo "tempHadArray 0: ", tempHadArray
      self.hadArray = tempHadArray
      #if hadArray != nil:
      #  #echo "hadArray:" & $hadArray[]
      #  hadArray[] = tempHadArray
      #  echo "tempHadArray: ", tempHadArray
      if tempHadArray:
        if inHaveArray:
          fail()
        #echo "getHaveArray(someN=n) == true:"
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
              #echo "doLastIter():"
              #echo nodes.treeRepr
              #echo "--------"
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
                #echo "nnkBracketExpr getHaveArray:"
                discard
              ret.add $(n[1][2].intVal - n[1][1].intVal + 1)
              self.handleArray(
                nodes=n[2],
                ret=ret,
                level=level,
              )
            else:
              if arrayPass == 0:
                #echo "nnkBracketExpr not getHaveArray:"
                #echo n.treeRepr
                #echo "--------"
                discard
              self.doLastIter(
                nodes=n,
                ret=ret,
                level=level,
              )
          else:
            if arrayPass == 0:
              #echo "not nnkBracketExpr getHaveArray:"
              #echo n.treeRepr
              #echo "--------"
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
            #echo "arrayPass == 0:"
            #echo mySeq
            result.add mySeq[^1]
            #result.add " "
            #result.add self.toCodeExprInner(
            #  nodes=n[0],
            #  level=level,
            #  isLhs=false,
            #  isTypeInst=false,
            #  inHaveArray=true,
            #)
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
        #echo "not getHaveArray(someN=n):"
        #echo n.treeRepr()
        case n[0].kind:
        of nnkIdent, nnkSym:
          if typeImpl != nil:
            #echo "n[0].kind == nnkIdent, nnkSym"
            #echo typeImpl[0].treeRepr
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
              #echo "n[i].kind == nnkIdent, nnkSym"
              #echo typeImpl[i].treeRepr
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
          #result.add "_c"
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
      #echo "#--------"
      #echo "typeinst nnkCall: "
      #echo n.treeRepr
      let innerTypeImpl = n.getTypeImpl()
      let innerTypeInst = n.getTypeInst()
      #echo ""
      #echo innerTypeImpl.treeRepr
      #echo ""
      #echo innerTypeInst.treeRepr
      #echo "--------"
      if n.len >= 3:
        if n[0].kind == nnkOpenSymChoice:
          #echo "n[0].kind == nnkOpenSymChoice: first:"
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
          #result.add "_b"
          #var idx = 2
          #if idx < innerTypeImpl[2 .. ^1].len():
          #  for param in innerTypeImpl[2 .. ^1]:
          #    echo param.treeRepr
          #    result.add self.toCodeExprInner(
          #      param, level, isLhs, isTypeInst, isSingle,
          #      innerTypeImpl[idx]
          #    )
          #    result.add "_d"
          #    idx += 1
        else:
          #echo n.repr
          #echo n.treeRepr
          fail()
      else:
        #echo "typeinst nnkCall other len: "
        #echo n.repr
        #echo n.treeRepr
        fail()
    of nnkObjectTy:
      #echo "#----"
      #echo "typeinst have nnkObjectTy:"
      #echo "typeName: ", typeName
      #echo n.treeRepr
      #echo ""
      #echo n.getTypeInst().treeRepr
      #echo ""
      #echo n.getTypeImpl().treeRepr
      #echo "----"
      #discard
      #fail()
      var toAdd: string
      toAdd.add "typedef struct "
      toAdd.add typeName
      toAdd.add " {\n"
      for memberDef in n[2]:
        self.hadArray = false
        toAdd.addIndent(1)
        var tempToAdd: string
        #echo "#----"
        #echo "memberDef[1].getTypeImpl():"
        #echo memberDef[1].getTypeImpl().treeRepr
        #echo ""
        #echo memberDef[1].getTypeInst().treeRepr
        #echo ""
        #echo memberDef[1].getType().treeRepr
        #echo ""
        #echo memberDef.treeRepr
        #echo "----"
        let haveEnum = (memberDef[1].getTypeImpl().kind == nnkEnumTy)
        if not haveEnum:
          tempToAdd = self.toCodeExprInner(
            memberDef[1].getTypeInst(),
            level, isLhs=false, isSingle=true, isTypeInst=true,
            arrayPass=0,
            #hadArray=hadArray
            isVarDecl=isVarDecl,
          )
        else:
          tempToAdd = self.toCodeExprInner(
            memberDef[1].getTypeImpl(),
            level, isLhs=false, isSingle=true, isTypeInst=true,
            arrayPass=0, typeName=memberDef[1].getTypeInst().repr,
            #hadArray=hadArray
            isVarDecl=isVarDecl,
          )
          
        #echo "tempToAdd: "
        #echo tempToAdd
        toAdd.add tempToAdd
        let temp = self.hadArray
        toAdd.add " "
        tempToAdd = self.toCodeExprInner(
          memberDef[0], level, isLhs=true,
          isVarDecl=isVarDecl,
        )
        toAdd.add tempToAdd
        if temp:
          #echo "past self.hadArray:"
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
        #echo "notin test:"
        #echo toAdd
        self.typedefSeq.add toAdd
        self.typedefTbl[typeName] = ""
      #echo result
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
      #echo "typeinst nnkPtrTy:"
      #echo n.treeRepr
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
        #fail()
    else:
      echo "typeinst nnkOther:"
      echo n.treeRepr
      fail()

    return result

  case n.kind:
  of nnkIdent, nnkSym:
    #echo "is this \"t\"?"
    #echo n.strVal
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
        #if n[1].getType().repr != "bool":
        #  result.add "|"
        #else:
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
    #echo "non-typeinst nnkAddr:"
    #echo n.treeRepr
    #echo ""
    ##echo "n.getImpl():"
    ##echo n.getImpl().treeRepr
    ##echo ""
    #echo "n.getType():"
    #echo n.getType().treeRepr
    #echo ""
    #echo "n.getTypeImpl():"
    #echo n.getTypeImpl().treeRepr
    #echo ""
    #echo "n.getTypeInst():"
    #echo n.getTypeInst().treeRepr
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
    #echo "non-typeinst nnkDerefExpr:"
    #echo n.treeRepr
    #echo ""
    ##echo "n.getImpl():"
    ##echo n.getImpl().treeRepr
    ##echo ""
    #echo "n.getType():"
    #echo n.getType().treeRepr
    #echo ""
    #echo "n.getTypeImpl():"
    #echo n.getTypeImpl().treeRepr
    #echo ""
    #echo "n.getTypeInst():"
    #echo n.getTypeInst().treeRepr
    ##fail()
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
    #echo "non-typeinst nnkDotExpr:"
    #echo n.treeRepr
    #echo ""
    ##echo "n.getImpl():"
    ##echo n.getImpl().treeRepr
    ##echo ""
    #echo "n.getType():"
    #echo n.getType().treeRepr
    #echo ""
    #echo "n.getTypeImpl():"
    #echo n.getTypeImpl().treeRepr
    #echo ""
    #echo "n.getTypeInst():"
    #echo n.getTypeInst().treeRepr
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
    #if n[0].kind == nnkBracketExpr:
    #  discard
    #else:
    #echo "test: " & $n.len
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
  #of nnkResult:
  #  discard
  of nnkChckRange:
    # skip check range and treat it as a hidden cast instead
    var typeStr = typeRename(n.getType.repr)
    #echo("typeStr 0: ", typeStr)
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
    #echo("typeStr 1: ", typeStr)
    #if typeStr == "float" and n[1].kind == nnkIntLit:
    #  discard
    #elif typeStr == "float" and n[1].kind == nnkFloatLit:
    #  discard
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
        #if have(n, @[nnkSym]):
          result.add "(("
          #result.add n[0].repr()
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
        #else:
        #  fail()
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
        #echo repr(n)
        ##echo n.strVal
        #echo n[0].kind
        var typeName: string
        #case n[0].kind:
        #of nnkBracketExpr:
        typeName = self.toCodeExprInner(
          n[0], level, isLhs, true, true,
          isVarDecl=isVarDecl,
        )
        #else:
        #  typeName = n[0].strVal

        #result.add "(("
        #result.add typeName
        #result.add "){"
        result.add "{"
        for i in 1 ..< n.len:
          #if n[i].len == 2:
          #result.add "."
          #result.add n[i][0].strVal
          #result.add "="
          #echo "#--------"
          #echo "i: ", i
          #echo n[i].treeRepr
          #echo ""
          #echo n[i][0].getTypeInst().treeRepr
          #echo ""
          #echo n[i][0].getTypeImpl().treeRepr
          #echo ""
          #discard self.toCodeExprInner(
          #  n[i][0].getTypeInst(), level, false, true, true
          #)
          var paramType: NimNode
          #echo "n[i].kind: " & $n[i].kind & " " & $i
          case n[i].kind:
          of nnkExprColonExpr:
            #echo n[i][1].getTypeInst()
            #result.add self.toCodeExprInner(n[i][1], level, isLhs)
            paramType = n[i][1].getTypeInst()
          else:
            #echo n[i].getTypeInst()
            #result.add self.toCodeExprInner(n[i], level, isLhs)
            paramType = n[i].getTypeInst()
          #echo ""
          #echo paramType.treeRepr
          #echo ""
          #echo "--------"

          result.add self.toCodeExprInner(
            n[i][1], level, isLhs,
            isVarDecl=isVarDecl,
          )
          #else:
          #  self.toCodeExprInner(n[i][0], level, isLhs)
          if i + 1 < n.len:
            result.add ", "
        result.add "}"
      of nnkCall:
        var procName: string = n[0].strVal #& "_f"
        var origProcName: string = procName
        #echo "expr nnkCall:"
        #echo "procName: ", procName
        #echo "origProcName: ", origProcName
        #echo "expr nnkCall: ", procName
        #echo n.treeRepr
        #echo n[0].getTypeInst().treeRepr
        let procTypeInst = n[0].getTypeInst()
        #echo procTypeInst.treeRepr
        var first: bool = true
        #if 
        #echo "pre for loop"
        #proc have
        var myHaveArrLen: bool = true
        if myHaveArrLen:
          myHaveArrLen = have(n, @[nnkSym])
        if myHaveArrLen:
          myHaveArrLen = (
            #procTypeInst[0]
            n[0].repr() == "len"
          )
        if myHaveArrLen:
          myHaveArrLen = (
            have(procTypeInst, @[nnkFormalParams])
          )
          #echo "this: ", myHaveArrLen
        if myHaveArrLen:
          myHaveArrLen = (
            have(procTypeInst[0], @[nnkSym, nnkIdentDefs])
          )
          #echo "this 2: ", myHaveArrLen
        if myHaveArrLen:
          myHaveArrLen = (
            procTypeInst[0][1].len == 3
          )
          #echo "this 3: ", myHaveArrLen, " ", procTypeInst[0][1].len
          #echo procTypeInst[0][1].treeRepr
          #echo "----"
        if myHaveArrLen:
          #echo "#----"
          #echo "procTypeInst[0][1][1]:"
          #echo procTypeInst[0][1][1].treeRepr
          #echo "----"
          myHaveArrLen = (
            getHaveArray(procTypeInst[0][1][1])
          )
          #echo "this 4: ", myHaveArrLen
          
        if myHaveArrLen:
          #echo "myHaveArrLen == true"
          result = $(
            (
              #procTypeInst[0][1][1][1][1][2].intVal
              procTypeInst[0][1][1][1][2].intVal
            ) - (
              #procTypeInst[0][1][1][1][1][1].intVal
              procTypeInst[0][1][1][1][1].intVal
            ) + (
              1
            )
          )
        else:
          for i in 1 ..< n.len:
            #--------
            #--------
            #echo "tempificate:"
            #echo n[i].treeRepr
            var paramType: NimNode
            #echo "n[i].kind: " & $n[i].kind & " " & $i
            case n[i].kind:
            of nnkExprEqExpr:
              #echo n[i][1].getTypeInst()
              #result.add self.toCodeExprInner(n[i][1], level, isLhs)
              paramType = n[i][1].getTypeInst()
            else:
              #echo n[i].getTypeInst()
              #result.add self.toCodeExprInner(n[i], level, isLhs)
              paramType = n[i].getTypeInst()

            if paramType.kind == nnkVarTy:
              let n = paramType
              fail()
            else:
              #echo "doing rename: "
              discard self.funcRenameIter(
                paramType=paramType,
                #origProcName=origProcName,
                procName=procName,
                first=first,
                isVarDecl=false,
              )
              #echo paramType.treeRepr
          #echo "post for loop"

          #echo "#--------"
          #echo "expr nnkCall: " & procName
          #echo n.treeRepr
          #echo ""
          #echo n.getTypeImpl().treeRepr
          #echo ""
          #echo n.getTypeInst().treeRepr
          #echo "--------"
          var returnType: string
          #returnType = self.toCodeExprInner(
          #  nodes=n[0].getTypeInst(),
          #)
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
          #procName = procName & "_" & returnType
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
      #of nnkCommand:
      #  
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
      #echo repr(n)
      #echo n.kind
      fail()

proc toCodeExpr(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
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
        isVarDecl=false,
      )
      self.res.add " = "
      self.toCodeExpr(
        nodes=n[1],
        #res=res,
        level=level,
        isLhs=false,
        isVarDecl=false,
      )
    else:
      fail()
  #self.res.addSmart(';')
  self.res.add ";"
      
  #for n in nodes:
  #  case n.kind:
  #  else:
  #    assert false
#proc toCodeCall(
#  self: var Convert,
#  nodes: NimNode,
#  #res: var string,
#  level = 0
#) =
#  addIndent(self.res, level)

#proc typeDef(
#  self: var Convert,
#  topLevelNode: NimNode,
#  level: int,
#): string =
#  #var typeName = ""
#
#  #assert topLevelNode.kind in {nnkTypeDef}
#  let n = topLevelNode
#  result.add "typedef struct "
#  result.add self.toCodeExprInner(n[0], level, true)
#  result.add " {\n"
#  if have(n, @[nnkEmpty, nnkObjectTy], 1):
#    discard
#  else:
#    fail()
#  result.add "} "
#  result.add self.toCodeExprInner(n[0], level, true)
#  result.add ";\n"
#
#  #result = typeName

proc toCodeTypeSection(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0,
) =
  discard
  #echo nodes.kind
  #addIndent(self.res, level)
  #let n = nodes
  ##for n in nodes:
  #case n.kind:
  #of nnkTypeDef:
  #  # `n[0]` should be the `nnkIdent` or `nnkSym`
  #  echo "in toCodeTypeSection:"
  #  echo n.len
  #else:
  #  fail()

#proc toCodeTypeInst(
#  self: var Convert,
#  nodes: NimNode,
#  level = 0,
#): string = 
#  self.toCodeExprInner(nodes, level, false, true)

proc toCodeVarSection(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level = 0,
) =
  addIndent(self.res, level)
  #echo repr(nodes) & " " & repr(nodes.kind)
  let n = nodes
  #echo "#----"
  #echo "toCodeVarSection(): start:"
  #echo n.repr
  #echo n.treeRepr
  #echo "----"
  case n.kind:
  of nnkIdentDefs:
    case n.len:
    of 3:
      #echo n[0].kind
      #echo n[1].kind
      #echo "nnkIdentDefs:"
      ##echo n.getTypeImpl().treeRepr
      ##echo ""
      ##echo n.getTypeInst().treeRepr
      #echo n.treeRepr
      #echo ""
      #echo n[1].getTypeInst().treeRepr
      #echo ""
      #echo n[1].getTypeImpl().treeRepr
      #echo "--------"
      let tempCond = (
        (
          have(n, @[nnkSym]) 
        ) or (
          have(n, @[nnkPragmaExpr])
        )
      )
      if (
        (
          tempCond
        )
        #and (
        #  not have(n, @[nnkEmpty], 1)
        #)
      ):
        #if n[1].strVal != "array":
        #if (
        #  (
        #    n[1].kind != nnkBracketExpr
        #  ) and (
        #    not (
        #      (
        #        n[1].kind == nnkCall
        #      ) and (
        #        n[1][0].repr == "[]"
        #      )
        #    )
        #  )
        #):
        #echo "#----"
        #echo "not an array?"
        #echo n.repr
        #echo "in nnkVarSection:"
        #echo n.treeRepr
        #echo n[0].treeRepr
        #echo "----"

        #var hadArray: ref bool = new bool
        #hadArray[] = false
        var tempToAdd: string
        if have(n, @[nnkPragmaExpr]):
          echo "nnkPragmaExpr: n[0]"
          echo n[0].treeRepr
          echo "----"
          if have(n[0], @[nnkPragma], 1):
            echo "nnkPragmaExpr: n[0][1]"
            echo n[0][1].treeRepr
            echo "----"
            proc myInnerFunc(
              self: var Convert,
              someN: NimNode,
            ) =
              echo "myInnerFunc(): "
              echo someN.treeRepr
              #if have(someN, @[nnkSym]):
              case someN.repr():
              of "cstatic":
                self.res.add "static "
              of "cconst":
                self.res.add "const "
              else:
                let n = someN
                fail()
            #if have(n[0], @[nnkCall]):
            #  self.myInnerFunc(
            #    someN=n[0][0]
            #  )
            #else:
            if n[0][1][0].kind == nnkCall:
              echo "found nnkCall"
              self.myInnerFunc(
                someN=n[0][1][0][0]
              )
            else:
              self.myInnerFunc(
                someN=n[0][1][0]
              )
        self.hadArray = false
        #echo "test:"
        #echo n[1].getType().treeRepr
        #echo ""
        #echo n[1].getTypeImpl().treeRepr
        #echo ""
        #echo n[1].getTypeInst().treeRepr
        var tempN: NimNode
        if not have(n, @[nnkEmpty], 1):
          tempN = n[1]
        else:
          tempN = n[2]
        tempToAdd = self.toCodeExprInner(
          tempN.getTypeInst(),
          level, isLhs=false, isTypeInst=true, arrayPass=0,
          #hadArray=hadArray
          isVarDecl=true,
        )
        #echo "tempToAdd: " & tempToAdd
        self.res.add tempToAdd
        #echo "not eek: ", self.hadArray
        #echo "not eek: "
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
        #echo tempToAdd
        self.res.add tempToAdd
        #echo "post n[0]"
        if temp:
          #echo "past self.hadArray:"
          #echo n.treeRepr
          #echo n[1].treeRepr
          #echo "----"
          #echo tempN.treeRepr
          #echo tempN[1].treeRepr
          #echo "----"
          self.res.add self.toCodeExprInner(
            tempN.getTypeInst(),
            level, isLhs=false, isTypeInst=true, arrayPass=1,
            isVarDecl=true,
          )
        if not have(n, @[nnkEmpty], 2):
          self.res.add " = "
          self.toCodeExpr(n[2], level, isLhs=false, isVarDecl=true)
        self.res.add ";\n"
      #elif have(n, @[nnkSym, nnkEmpty]):
      #  if have(n, @[nnkSym], 2):
      #  else:
      #    fail()
      else:
        #echo "my-fail-notes:"
        #echo n.treeRepr
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
  #procName: string,
): (array[pfkLim, bool], string) =
  let n = nodes
  result[0] = [false, false, false, false]
  #proc innerFunc(
  #  self: var Convert,
  #  nodes: NimNode,
  #  level: int,
  #): string =
  #  let n = nodes
  #  #if (
  #  #  (
  #  #    n.kind == nnkIdent
  #  #  ) or (
  #  #    n.kind == nnkSym
  #  #  )
  #  #):
  #  case n.repr():
  #  of "cstatic":
  #    result.add "static"
  #  of "craw":
  #    #if n[0].repr() == "craw":
  #    if (
  #      (
  #        n.kind != nnkIdent 
  #      ) and (
  #        n.kind != nnkSym
  #      )
  #    ):
  #      result.add n[1].strVal
  #  of "cmainmhz":
  #    if (
  #      (
  #        n[0].kind != nnkIdent 
  #      ) and (
  #        n[0].kind != nnkSym
  #      )
  #    ):
  #      result.add "#pragma MAIN_MHZ "
  #      result.add self.procName
  #      result.add " "
  #      result.add n[1].strVal
  #  of "importc":
  #    discard
  #  else:
  #    fail()
  for item in n.children:
    #echo "item:"
    #echo item.treeRepr
    case item.kind:
    of nnkIdent, nnkSym:
      #result.add self.innerFunc(nodes=item, level=level)
      case item.repr():
      of "cstatic":
        result[1].add "static "
        result[0][pfkStatic] = true
      of "cconst":
        result[1].add "const "
      of "cextern":
        #echo "testificate"
        result[0][pfkExtern] = true
      of "cnodecl":
        result[0][pfkNoDecl] = true
      of "cnomangle":
        #result.add "nomangle "
        result[0][pfkNoMangle] = true
      else:
        let n = item
        fail()
    of nnkExprColonExpr:
      #result.add self.innerFunc(nodes=item[0], level=level)
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
      #echo tempResult[0].len()
      for idx in 0 ..< tempResult[0].len():
        let myPfk = PragmaFlagKind(idx)
        if tempResult[0][myPfk]:
          result[0][myPfk] = true
      result[1].add tempResult[1]
    else:
      fail()
  #echo "test: "
  #echo result
  #if result[0][pfkStatic]:
  #  echo "found \"cstatic\""
  #if result[0][pfkExtern]:
  #  echo "found \"cextern\""
  #if result[0][pfkNoMangle]:
  #  echo "found \"cnomangle\""

  #self.noMangleSeq.add result[0]

proc toCodePragmaStmt(
  self: var Convert,
  nodes: NimNode,
  level: int,
  #procName: string,
) =
  self.res.add self.toCodePragmaStmtInner(
    nodes=nodes, level=level, #procName=procName,
  )[1]
proc toCodeStmts(
  self: var Convert,
  nodes: NimNode,
  #res: var string,
  level: int
) = 
  addIndent(self.res, level)
  #echo "toCodeStmts:"
  #echo repr(nodes)
  ##echo repr(nodes.kind)
  #echo nodes.treeRepr
  proc innerFunc(
    self: var Convert,
    n: NimNode,
    level: int
  ) =
    #echo repr(n.kind)
    #echo n.len
    case n.kind:
    of nnkEmpty:
      discard
    of nnkSym:
      discard
    of nnkPragma:
      self.toCodePragmaStmt(nodes=n, level=level + 1)
    of nnkIfStmt:
      self.toCodeIfStmt(n, level + 1)
      #discard
    of nnkCaseStmt:
      #echo n.treeRepr()
      self.toCodeSwitchStmt(n, level + 1)
    of nnkWhileStmt:
      self.toCodeWhileStmt(n, level + 1)
    of nnkForStmt:
      #echo "doing toCodeForStmt():"
      self.toCodeForStmt(n, level + 1)
    #of nnkForStmt:
    #  self.toCodeForStmt(n, level + 1)
    of nnkTypeSection:
      self.toCodeTypeSection(n[0], level + 1)
    of nnkVarSection, nnkLetSection:
      self.toCodeVarSection(n[0], level + 1)
    #of nnkV
    of nnkAsgn:
      self.toCodeAsgn(n, level + 1)
    of nnkCall:
      #self.toCodeCall(n, level + 1)
      self.toCodeExpr(n, level + 1, false, isVarDecl=false)
      self.res.add ";"
    of nnkBracket:
      #echo repr(n)
      #n.dumpAstGen
      #dumpLisp(n)
      #echo n.treeRepr
      discard
    of nnkCommentStmt:
      #addIndent(self.res, level)
      #self.res.add "// "
      #self.res.add n.strVal
      #self.res.add "\n"
      discard
    of nnkDiscardStmt:
      discard
    of nnkReturnStmt:
      #echo n.treeRepr()
      #self.res.add "return "
      #self.toCodeExpr(n[0], level, false, isVarDecl=false)
      #self.res.add ";"
      #discard
      self.toCodeStmts(n[0], level)
      #self.res.add ";"
    else:
      #echo n
      #echo repr(n)
      #echo n.kind
      fail()

  #echo repr(nodes)
  #echo nodes.kind
  #echo repr(nodes.kind)
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
  #parentNode: NimNode,
  procNode: NimNode,
  typeImpl: NimNode,
  pass: int,
): (string, string, bool) =
  var procName = ""
  var origProcName = ""
  var paramsStr = ""
  var returnType = "void"
  #result[0] = false
  #result[0] = ""
  #echo topLevelNode.treeRepr
  #echo "procDef(): very start:"
  #echo procNode.treeRepr

  assert procNode.kind in {
    nnkProcDef, nnkFuncDef
  }
  assert typeImpl.kind in {
    nnkProcTy
  }
  #echo "#----"
  #echo "procDef(): start:"
  #echo procNode.kind
  #echo procNode.treeRepr
  #echo ""
  #echo typeImpl.kind
  #echo typeImpl.treeRepr
  #echo "----"
  #var myProcPragmaStr: string
  var myProcPragmaSeq: seq[NimNode]
  var pragmaFlagArr: array[pfkLim, bool]
  #var haveExtern: bool = false
  #var haveNoMangle: bool = false
  var haveGenerics: bool = false

  var foundElse: bool = false
  for n in procNode:
    #echo "for n in procNode:"
    #echo n.kind
    #echo n.treeRepr
    #echo "----"
    #echo "for loop begin:"
    #echo procName
    #echo "--------"
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
      #if pragmaFlagArr[pfkExtern]:
      #  echo "setting haveExtern to true"
      #  haveExtern = true
      #if pragmaFlagArr[pfkNoMangle]:
      #  echo "setting haveNoMangle to true"
      #  haveNoMangle = true
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
      #if (
      #  not (
      #    (
      #      pragmaFlagArr[pfkExtern]
      #    ) or (
      #      pragmaFlagArr[pfkNoMangle]
      #    )
      #  )
      #):
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
        #echo "error: can't have generics with \"cnomangle\""
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
        #echo "nnkBracket:"
        #echo n.treeRepr
        #echo "haveExtern: ", haveExtern
        #echo "haveNoMangle: ", haveNoMangle
        if pragmaFlagArr[pfkExtern] or pragmaFlagArr[pfkNoMangle]:
          #echo "error: can't have generics with \"cnomangle\""
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
          #for i in 0 ..< n[0].len:
          ##for paramDef in n[0]:
          #  let paramDef = n[0][i]
          #  echo $i
          #  echo paramDef.treeRepr
          #  if (
          #    (
          #      paramDef.kind != nnkEmpty
          #    ) and (
          #      i != 0
          #    )
          #  ):
          #    echo "temp: asdf"
          #    fail()
          #  else:
          #    discard
          discard
        else:
          #echo "temp 1: asdf"
          fail()
      else:
        fail()
    of nnkFormalParams:
      #echo "nnkFormalParams: ", pass, " ", procName
      #if pass == 0:
      if pass == 1:
      #else:
        if n[0].kind != nnkEmpty:
          #returnType = n[0].strVal
          #echo "returnType:"
          #echo n.treeRepr
          #echo ""
          #echo n[0].treeRepr
          #echo n[0].getTypeImpl().treeRepr
          #echo n[0].getTypeInst().treeRepr
          #echo typeImpl
          returnType = self.toCodeExprInner(
            nodes=typeImpl[0][0],
            level=0,
            isLhs=false,
            isTypeInst=true,
            isVarDecl=true,
          )
          #echo returnType
          #echo "--------"
        #echo n.treeRepr
        var idx = 1
        for paramDef in n[1 .. ^1]:
          # the paramDef is like `x, y, z: float`
          #echo "paramDef:"
          #echo paramDef.treeRepr
          var first: bool = true
          if paramDef.kind != nnkEmpty:
            for param in paramDef[0 ..< ^2]:
            #for otherIdx in 0 ..< paramDef[0 ..< ^2].len:
              #let param = paramDef[otherIdx]
              #echo "param stuff:"
              #echo param.treeRepr
              #echo "the length: ", paramDef[0 ..< ^2].len()
              # Process each `x`, `y`, `z` in a loop
              paramsStr.add "  "
              let paramName = param.repr()
              #echo "here's the paramName: ", paramName
              #let paramType = param.getTypeInst()
              #let paramTypeImpl = param.getTypeImpl()
              #echo "#--------"
              #echo "testificate:"
              #echo typeImpl.treeRepr
              #echo ""
              #echo "----#----"
              #echo param.treeRepr
              #echo ""
              #echo paramType.treeRepr
              #echo ""
              #echo paramTypeImpl.treeRepr
              #echo "--------"
              #echo paramName & ": " & $paramType
              #dumpAstNode(n)
              #dumpTree(n)
              #if paramType.kind == nnkVarTy:
              #  #echo "test"
              #  #fail()
              #  #let n = paramType
              #  fail()
              ##elif paramType.kind == nnkBracketExpr:
              ##  #fail()
              #else:
              #paramsStr.add ""
              #paramsStr.add paramType.strVal
              #let tempParamsStr = self.toCodeExprInner(
              #  nodes=paramType,
              #  level=0,
              #  isLhs=false,
              #  isTypeInst=true,
              #)
              #if first:
              #  first = false
              #  procName.add "_f"
              #else:
              #  procName.add "_c"
              #procName.add tempParamsStr
              #paramsStr.add tempParamsStr
              #echo "procName: " & procName
              #echo "paramsStr.add: "
              #echo typeImpl[0][idx].treeRepr
              #echo ""
              #echo typeImpl[0][idx].treeRepr
              #echo "note:"
              #echo typeImpl[idx].treeRepr
              #echo "idx, otherIdx: " & $idx & " " & $otherIdx
              #echo typeImpl[0][idx].treeRepr
              #echo "kind: ", typeImpl[0][idx].kind
              case typeImpl[0][idx].kind:
              of nnkIdentDefs:
                #echo "temp: "
                ##echo typeImpl[0][0][idx][1].treeRepr
                ##echo typeImpl[0][idx].treeRepr
                ##echo typeImpl[0][idx][1].treeRepr
                #echo typeImpl[0][idx].treeRepr()
                #echo "temp end:"
                paramsStr.add(
                  self.funcRenameIter(
                    paramType=(
                      #paramType
                      typeImpl[0][idx][1]
                    ),
                    #origProcName=origProcName,
                    procName=procName,
                    first=first,
                    isVarDecl=true,
                  )
                )
                if self.cppConstRefInp:
                  paramsStr.add " IN"
              else:
                discard
              #echo "idx: ", $idx
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
          #echo "test: ", origProcName
          self.noMangleTbl[origProcName] = pragmaFlagArr[pfkNoMangle]
          #if origProcName notin self.noMangleProtoTbl:
          #  self.noMangleProtoTbl[origProcName] = (
          #    paramsStr
          #  )
          self.noMangleProtoTbl[origProcName] = ""
        #elif self.noMangleTbl[origProcName]:
        #  errFail("error: can't have overloading with \"cnomangle\"")
      else: # if pass == 1:
        #echo "ending? " & procName
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
        #echo "setting result[0]"
        #echo procName
        #echo "--------"

        if pragmaFlagArr[pfkExtern]:
          self.res.add "extern "
        self.res.add returnType & " " & procName & "("
        #if result[0].len > 0:
        #  procName.add "_f" & result[0]
        #self.res.add "("
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
    #echo "--------"
    #echo "for loop end:"
    #echo procName
    #echo "--------"

  #echo paramsStr
  #echo self.res
  #echo "test: "
  #result[0] = procName
  #if myProcPragmaStr.len() > 0:
  #  result[1] = myProcPragmaStr
  #  result[1].add " "
  if not pragmaFlagArr[pfkNoDecl]:
    for procPragma in myProcPragmaSeq:
      #echo "procPragma:"
      #echo procPragma.treeRepr
      result[1].add self.toCodePragmaStmtInner(
        nodes=procPragma, level=0, #procName=#isProc=true
      )[1]

    result[1].add self.res

  result[2] = foundElse
  #echo result


proc findTopLevel(
  self: var Convert,
  topLevelNode: NimNode,
  #typeImpl: NimNode,
  pass: int,
) =
  #if topLevelNode.kind != nnkEmpty:
  #  echo "#----"
  #  echo "findTopLevel(): topLevelNode.kind outer:"
  #  echo topLevelNode.repr
  #  echo topLevelNode.treeRepr
  #  echo "----"
  #  discard
  proc innerFunc(
    self: var Convert,
    n: NimNode,
    pass: int,
  ) = 
    if n.kind != nnkEmpty:
      #echo "#----"
      #echo "findTopLevel(): n.kind outer:"
      #echo n.repr
      #echo n.treeRepr
      #echo "----"
      discard
    if n.kind == nnkCall:
      #echo "#----"
      #echo "n.kind == nnkCall:"
      #echo n.treeRepr()
      #echo "----"
      #echo n
      #dumpTree n
      if n[0].repr() == "[]":
        #echo "n[0].repr() == \"[]\""
        #echo n[0].treeRepr()
        #echo "----"
        #continue
        return
      var procName = n[0].repr()
      if (
        (
          procName in ignoreFuncs
        )
        #or (
        #  procName in self.funcTbl
        #)
      ):
        #echo "continuing"
        #echo "procName in ignoreFuncs"
        #continue
        return

      case n[0].kind:
      of nnkIdent:
        #echo "findTopLevel(): nnkIdent"
        #echo repr(n)
        #echo n[0]
        #echo n.treeRepr
        #echo "test in findTopLevel()"
        fail() # can't have 
      of nnkSym:
        #echo "#--------"
        #echo "findTopLevel(): nnkSym"
        #echo n[0].kind
        #echo n[0].symKind()
        let impl = n[0].getImpl()
        let innerTypeImpl = n[0].getTypeImpl()
        #let innerTypeInst = n[0].getTypeInst()
        #let innerType = n[0].getType()
        #echo repr(impl)
        #echo "impl.treeRepr:"
        #echo impl.treeRepr
        #echo ""
        #echo "innerTypeImpl.treeRepr:"
        #echo innerTypeImpl.treeRepr
        #echo ""
        #echo n[0].repr()
        #echo "innerTypeInst.treeRepr:"
        #echo innerTypeInst.treeRepr
        #echo ""
        #echo "innerType.treeRepr:"
        #echo innerType.treeRepr
        #echo "--------"
        #self.nextProcRenameTbl(typeImpl)

        #self.noMangleTbl.add false
        var tempImpl: NimNode
        if impl.kind != nnkTemplateDef:
          tempImpl = impl
        else:
          ###tempImpl = innerTypeImpl
          ###tempImpl = n.getTypeInst()
          ##tempImpl = n
          #echo "other impl:"
          ##echo tempImpl.treeRepr
          #echo n[0].treeRepr
          #echo impl.treeRepr
          ##continue
          return

        var myProcDef = self.procDef(tempImpl, innerTypeImpl, pass=pass)
        #if myProcDef[0] == "slash":
        #  echo "slash pre:"
        #  echo "pass:", pass
        #  echo n[0].treeRepr
        #  echo impl.treeRepr

        #let innerProcName = myProcDef[0]

        self.findTopLevel(
          impl,#, innerTypeImpl
          pass=pass,
        )

        #if myProcDef[0] == "slash":
        #  echo "slash post:"
        #  echo "pass:", pass
        #  echo n[0].treeRepr
        #  echo impl.treeRepr
        #myProcDef = self.procDef(tempImpl, innerTypeImpl, pass=pass)

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
            #echo "continuing 2:"
            ##echo "first cond: ", innerProcName in ignoreFuncs
            ##echo "second cond: ", innerProcName in self.funcTbl
            #echo procName
            #echo myProcDef[0]
            #echo myProcDef[1]
            #echo myProcDef[2]
            #echo "..."
            ##var cond: bool = true
            ##if cond:
            ##  cond = myProcDef[0] in self.funcTbl
            ##if cond:
            ##  cond = self.funcTbl[myProcDef[0]].doMangle
            ##if cond:
            ##  #cond =
            ##  discard

            ##if cond:
            ##  fail()
            ##else:
            ##continue
            return
          #echo "adding this function:"
          #echo procName
          #echo myProcDef[0]
          #echo myProcDef[1]
          #echo "..."
          self.funcSeq.add myProcDef[1]
          self.funcTbl[myProcDef[0]] = FuncTblElem(
            #doMangle: not self.noMangleTbl[^1],
            defn: self.funcSeq[^1],
          )
          #if self.procRenameTbl.len > 0:
          #  discard self.procRenameTbl.pop()
      else:
        #echo "test in findTopLevel() 1"
        #echo repr(n[0])
        discard
      for innerN in n[1 .. ^1]:
        self.findTopLevel(
          innerN,
          pass=pass
        )

    self.findTopLevel(
      n,#, typeImpl
      pass=pass
    )

  #let n = topLevelNode
  #if topLevelNode.kind != nnkCall:
  for n in topLevelNode:
    #echo "for n in topLevelNode:"
    self.innerFunc(
      n=n,
      pass=pass
    )
  #else:
  #  self.innerFunc(
  #    n=topLevelNode,
  #    pass=pass
  #  )


proc toPipelineCInner*(
  s: NimNode,
  regularC: NimNode,
  cppConstRefInp: NimNode,
): string =
  var code: string
  #code.add "asdf"
  #echo s.treeRepr
  #echo $s.kind
  var n = getImpl(s)
  #echo "#--------"
  #echo "toPipelineCInner():"
  #echo n.treeRepr
  #echo "--------"
  ##var n = s
  #var n = s.getImplTransformed()
  #echo s.treeRepr
  #var n = s.bindSym()
  #var n = s
  #echo n.treeRepr
  #echo n.treeRepr

  ##var funcTbl: Table[string, string]
  ##var typedefTbl: Table[string, string]
  ##var convert: Convert = Convert(
  ##  #funcTbl=funcTbl,
  ##  #typedefTbl=typedefTbl,
  ##  #res=code,
  ##)
  #echo $regularC
  var convert: Convert
  convert.regularC = ($regularC == "true")
  convert.cppConstRefInp = ($cppConstRefInp == "true")
  for pass in 0 .. 1:
    convert.findTopLevel(n, pass=pass)
  #echo convert.funcTbl
  #echo convert.typedefTbl
  #var globals: Table[string, string]

  ##for k, v in globals:
  ##  code.add(v)
  ##  code.add "\n"

  #for v in convert.typedefSeq[^1 .. 0]:
  #  code.add(v)
  #  code.add "\n"
  #var i = convert.typedefSeq.len() - 1
  #while i >= 0:
  #for i in convert.typedefSeq.len() - 1 .. 0:
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
