//package com.mistlang
//
//sealed trait Ast
//
//sealed trait Expr extends Ast
//
//sealed trait BlockExpr extends Expr
//
//sealed trait SimpleExpr extends BlockExpr
//
//sealed trait Literal extends SimpleExpr
//case class NumberLiteral(value: Long) extends Literal
//case class FloatLiteral(value: Double) extends Literal
//case class StringLiteral(value: String) extends Literal
//case class BoolLiteral(value: Boolean) extends Literal
//
//case class Ref(name: Ident) extends SimpleExpr
//case class Call(func: SimpleExpr, args: List[SimpleExpr]) extends SimpleExpr
//
//class TypeArg(name: Ident, tpe: Option[TypeExpr])
//
//sealed trait TypeExpr
//
//case class TypeRef(name: Ident) extends TypeExpr
//case class TypeCall(t: TypeExpr, args: List[TypeArg])
//
//case class Assignment(name: Ident, tpe: Option[TypeExpr], value: SimpleExpr, isVar: Boolean) extends BlockExpr
//
//case class File(pkg: Package, imports: List[Import], objects: List[TopLevelObject]) extends Ast
//
//case class Package(pkg: String) extends Ast
//
//enum ImportSpec {
//  case STAR
//  case CustomName(name: Ident)
//}
//
//case class Import(path: String, spec: Option[ImportSpec] = None) extends Ast
//
//sealed trait TopLevelObject extends Ast
//sealed trait TraitObject extends Ast
//
//
//case class Ident(s: String)
//
//case class Arg(name: Ident, tpe: TypeExpr)
//case class Struct(name: Ident, typeArgs: Option[List[TypeArg]], args: List[Arg]) extends TopLevelObject
//case class Method(name: Ident, receiver: Ident, args: List[Arg], typeArgs: List[Arg], body: Expr) extends TopLevelObject
//case class Function(name: Ident, typeArgs: List[TypeArg], receiver: Option[Ident], args: List[Arg], body: Expr,
//                    returnType: TypeExpr) extends TopLevelObject with TraitObject
//
//case class FunctionSpec(name: Ident, args: List[Arg], typeArgs: List[Arg]) extends TraitObject
//case class Trait(name: Ident, objects: List[TraitObject]) extends TopLevelObject
