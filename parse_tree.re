open Sexplib.Std;

type main('a) = {
  program: block('a),
  subprograms: list(sub('a)),
}

and sub('a) = {
  sub_subprogram: sub_desc('a),
  sub_loc: Location.t,
}

and sub_desc('a) =
  | Subroutine(subroutine('a))
  | Function(func('a))

and subroutine('a) = {
  sub_name: string,
  sub_args: list(string),
  sub_decls: list(decl('a)),
}

and func('a) = {
  func_name: string,
  func_args: list(string),
  func_decls: list(decl('a)),
}

and block('a) = {
  vardecls: list(vardecl('a)),
  decls: list(decl('a)),
}

and vardecl('a) = {
  vardecl_desc: vardecl_desc('a),
  vardecl_loc: Location.t,
}

and vardecl_desc('a) = {
  var: string,
  init: option(expr('a)),
  typ,
  kind: list(kind('a)),
}

and typ = {
  typ_desc,
  typ_loc: Location.t,
}

and typ_desc =
  | Tinteger
  | Treal
  | Tcomplex
  | Tlogical
  | Tdouble

and kind('a) = {
  kind_desc: kind_desc('a),
  kind_loc: Location.t,
}

and kind_desc('a) =
  | Pointer
  | Dimension(list(dim_param('a)))
  | Allocatable
  | Parameter

and dim_param('a) = {
  dim_param_desc: dim_param_desc('a),
  dim_param_loc: Location.t,
}

and dim_param_desc('a) =
  | Default
  | Colon(option(expr('a)), option(expr('a)), option(expr('a)))
  | Exp(expr('a))

and decl('a) = {
  decl_desc: decl_desc('a),
  decl_loc: Location.t,
}

and decl_desc('a) =
  | While(expr('a), list(decl('a)))
  | If(expr('a), list(decl('a)), list(decl('a)))
  | Assign(string, expr('a))
  | Assign_a(string, list(dim_param('a)), expr('a))
  | Do(string, expr('a), expr('a), option(expr('a)), list(decl('a)))
  | Select(select('a))
  | Call(string, list(expr('a)))
  | Return(expr('a))
  | Stop
  | Label(int, decl('a))
  | Goto(int)

and expr('a) = {
  expr_desc: expr_desc('a),
  expr_loc: Location.t,
  expr_typ: 'a,
}

and expr_desc('a) =
  | Const(const)
  | Funcall(string, list(dim_param('a)))
  | Ident(string)
  | Access(string, list(dim_param('a)))
  | Array(list(expr('a)))
  | Rev(expr('a))
  | Plus(expr('a), expr('a))
  | Minus(expr('a), expr('a))
  | Mul(expr('a), expr('a))
  | Div(expr('a), expr('a))
  | Eq(expr('a), expr('a))
  | Neq(expr('a), expr('a))
  | Leq(expr('a), expr('a))
  | Less(expr('a), expr('a))
  | Not(expr('a))
  | And(expr('a), expr('a))
  | Or(expr('a), expr('a))
  | Eqv(expr('a), expr('a))
  | Neqv(expr('a), expr('a))

and const = {
  const_desc,
  const_loc: Location.t,
}

and const_desc =
  | Cint(int)
  | Creal(string)
  | Cbool(bool)

and case('a) = {
  case_option: list(case_option('a)), /* [] means DEFAULT */
  case_decls: list(decl('a)),
  case_loc: Location.t,
}

and case_option('a) =
  | Range(range('a))
  | Scala(expr('a))

and range('a) = {
  range_left: option(expr('a)),
  range_right: option(expr('a)),
  range_loc: Location.t,
}

[@deriving sexp]
and select('a) = {
  select_expr: expr('a),
  select_cases: list(case('a)),
  select_loc: Location.t,
};
