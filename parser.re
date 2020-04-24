module MenhirBasics = {
  exception Error;

  type token =
    | WHILE
    | TRUE
    | TO
    | THEN
    | S_RPAREN
    | SUBROUTINE
    | STOP
    | SELECT
    | RPAREN
    | RETURN
    | REAL
    | RBRACE
    | PROGRAM
    | PRECISION
    | POINTER
    | PLUS
    | PARAMETER
    | OR
    | NOT
    | NEQV
    | NEQ
    | MUL
    | MINUS
    | LPAREN_S
    | LPAREN
    | LOGICAL
    | LESS
    | LEQ
    | LBRACE
    | INTEGER
    | INT(int)
    | IF
    | IDENT(string)
    | GREATER
    | GOTO
    | GO
    | GEQ
    | FUNCTION
    | FLOAT(string)
    | FALSE
    | EQV
    | EQEQ
    | EQ
    | EOF
    | END
    | ELSE
    | DOUBLE
    | DO
    | DIV
    | DIMENSION
    | DEFAULT
    | CONTAINS
    | COMPLEX
    | COMMENT
    | COMMA
    | COLON
    | COLCOL
    | CASE
    | CALL
    | BR
    | AND
    | ALLOCATABLE;
};

include MenhirBasics;

let _eRR = MenhirBasics.Error;

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf => token,
  _menhir_lexbuf: Lexing.lexbuf,
  _menhir_token: token,
  mutable _menhir_error: bool,
}

and _menhir_state =
  | MenhirState267
  | MenhirState265
  | MenhirState263
  | MenhirState262
  | MenhirState258
  | MenhirState256
  | MenhirState255
  | MenhirState252
  | MenhirState251
  | MenhirState249
  | MenhirState245
  | MenhirState244
  | MenhirState240
  | MenhirState238
  | MenhirState237
  | MenhirState233
  | MenhirState232
  | MenhirState230
  | MenhirState223
  | MenhirState220
  | MenhirState219
  | MenhirState217
  | MenhirState213
  | MenhirState210
  | MenhirState208
  | MenhirState205
  | MenhirState204
  | MenhirState203
  | MenhirState202
  | MenhirState201
  | MenhirState200
  | MenhirState199
  | MenhirState196
  | MenhirState193
  | MenhirState189
  | MenhirState187
  | MenhirState184
  | MenhirState183
  | MenhirState182
  | MenhirState181
  | MenhirState177
  | MenhirState173
  | MenhirState170
  | MenhirState169
  | MenhirState167
  | MenhirState166
  | MenhirState163
  | MenhirState161
  | MenhirState160
  | MenhirState159
  | MenhirState158
  | MenhirState157
  | MenhirState155
  | MenhirState153
  | MenhirState152
  | MenhirState151
  | MenhirState150
  | MenhirState147
  | MenhirState144
  | MenhirState142
  | MenhirState140
  | MenhirState139
  | MenhirState138
  | MenhirState137
  | MenhirState133
  | MenhirState132
  | MenhirState131
  | MenhirState128
  | MenhirState126
  | MenhirState123
  | MenhirState122
  | MenhirState119
  | MenhirState117
  | MenhirState115
  | MenhirState111
  | MenhirState108
  | MenhirState107
  | MenhirState104
  | MenhirState102
  | MenhirState98
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState87
  | MenhirState86
  | MenhirState82
  | MenhirState80
  | MenhirState79
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState27
  | MenhirState26
  | MenhirState23
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState5
  | MenhirState3
  | MenhirState2;
open Parse_tree;
open Location;

let mkblock = (var, decl) => {vardecls: var, decls: decl};

let mktyp = (~loc, typ_desc) => {typ_desc, typ_loc: loc};

let mkkind = (~loc, kind) => {kind_desc: kind, kind_loc: loc};

let mkvar_decl = (~kind, typ, pairs) =>
  List.map(
    ((var, dim, init, loc)) => {
      let kind =
        switch (dim) {
        | None => kind
        | Some(a) => [mkkind(~loc, Dimension(a)), ...kind]
        };
      {
        vardecl_desc: {
          var,
          init,
          kind,
          typ,
        },
        vardecl_loc: loc,
      };
    },
    pairs,
  );

let mkexp = (~loc, exp) => {expr_loc: loc, expr_desc: exp, expr_typ: ()};

let mkconst = (~loc, con) => {const_desc: con, const_loc: loc};

let mkdim_param = (~loc, param) => {
  dim_param_desc: param,
  dim_param_loc: loc,
};

let mkdecl = (~loc, dec) => {decl_desc: dec, decl_loc: loc};

let mkrange = (~loc, range_left, range_right) => {
  range_left,
  range_right,
  range_loc: loc,
};

let mkcase = (~loc, case_option, case_decls) => {
  case_option,
  case_decls,
  case_loc: loc,
};

let mkselect = (~loc, select_expr, select_cases) => {
  select_expr,
  select_cases,
  select_loc: loc,
};

let mksub = (~loc, sub) => {sub_subprogram: sub, sub_loc: loc};

let mksubroutine = (~loc, ident, args, decls) => {
  sub_name: ident,
  sub_args: args,
  sub_decls: decls,
};

let mkfunc = (~loc, ident, args, decls) => {
  func_name: ident,
  func_args: args,
  func_decls: decls,
};

let rec _menhir_goto_main:
  (_menhir_env, 'ttv_tail, Parse_tree.main(unit)) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _v) => {
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv1045 = Obj.magic(_menhir_stack);
    let _v: Parse_tree.main(unit) = _v;
    (
      {
        let _menhir_env: _menhir_env = _menhir_env;
        let _menhir_stack: 'freshtv1043 = Obj.magic(_menhir_stack);
        let (_1: Parse_tree.main(unit)): Parse_tree.main(unit) = _v;
        (Obj.magic(_1): 'freshtv1044);
      }: 'freshtv1046
    );
  }:
    (_menhir_env, 'ttv_tail, Parse_tree.main(unit)) => 'ttv_return
)

and _menhir_goto_seq_case_opt:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_case_opt) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState142 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv1037, _menhir_state),
        _menhir_state,
        'tv_seq_case_opt,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv1033, _menhir_state),
              _menhir_state,
              'tv_seq_case_opt,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | BR =>
                  _menhir_run3(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState150,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState150,
                  );
                };
              }: 'freshtv1034
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv1035, _menhir_state),
              _menhir_state,
              'tv_seq_case_opt,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv1036
            );
          };
        }: 'freshtv1038
      );
    | MenhirState230 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv1041, _menhir_state, 'tv_case_opt),
        _menhir_state,
        'tv_seq_case_opt,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv1039, _menhir_state, 'tv_case_opt),
            _menhir_state,
            'tv_seq_case_opt,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_case_opt),
                _,
                _3: 'tv_seq_case_opt,
              ) = _menhir_stack;
              let _2 = ();
              let _v: 'tv_seq_case_opt = ([_1, ..._3]: 'tv_seq_case_opt);

              _menhir_goto_seq_case_opt(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv1040
          );
        }: 'freshtv1042
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_case_opt) => 'ttv_return
)

and _menhir_goto_eof:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_eof) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) =>
    switch (_menhir_s) {
    | MenhirState119 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv1023, _menhir_state) =
        Obj.magic(_menhir_stack);
      let _menhir_s: _menhir_state = _menhir_s;
      let _v: 'tv_eof = _v;
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv1021, _menhir_state) =
            Obj.magic(_menhir_stack);
          let _: _menhir_state = _menhir_s;
          let (_2: 'tv_eof): 'tv_eof = _v;
          (
            {
              let (_menhir_stack, _menhir_s) = _menhir_stack;
              let _1 = ();
              let _v: 'tv_eof = ((): 'tv_eof);

              _menhir_goto_eof(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv1022
          );
        }: 'freshtv1024
      );
    | MenhirState117 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv1027, string), _menhir_state, 'tv_br),
          _menhir_state,
          'tv_top_block,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      let _menhir_s: _menhir_state = _menhir_s;
      let _v: 'tv_eof = _v;
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (('freshtv1025, string), _menhir_state, 'tv_br),
              _menhir_state,
              'tv_top_block,
            ),
            _menhir_state,
            'tv_ident_or_blank,
          ) =
            Obj.magic(_menhir_stack);
          let _: _menhir_state = _menhir_s;
          let (_8: 'tv_eof): 'tv_eof = _v;
          (
            {
              let (
                (
                  ((_menhir_stack, _2: string), _, _3: 'tv_br),
                  _,
                  _4: 'tv_top_block,
                ),
                _,
                _7: 'tv_ident_or_blank,
              ) = _menhir_stack;
              let _6 = ();
              let _5 = ();
              let _1 = ();
              let _v: Parse_tree.main(unit) = (
                {program: _4, subprograms: []}: Parse_tree.main(unit)
              );

              _menhir_goto_main(_menhir_env, _menhir_stack, _v);
            }: 'freshtv1026
          );
        }: 'freshtv1028
      );
    | MenhirState263 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv1031, string), _menhir_state, 'tv_br),
              _menhir_state,
              'tv_top_block,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_subprogram,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      let _menhir_s: _menhir_state = _menhir_s;
      let _v: 'tv_eof = _v;
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (('freshtv1029, string), _menhir_state, 'tv_br),
                  _menhir_state,
                  'tv_top_block,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_subprogram,
            ),
            _menhir_state,
            'tv_ident_or_blank,
          ) =
            Obj.magic(_menhir_stack);
          let _: _menhir_state = _menhir_s;
          let (_11: 'tv_eof): 'tv_eof = _v;
          (
            {
              let (
                (
                  (
                    (
                      ((_menhir_stack, _2: string), _, _3: 'tv_br),
                      _,
                      _4: 'tv_top_block,
                    ),
                    _,
                    _6: 'tv_br,
                  ),
                  _,
                  _7: 'tv_seq_subprogram,
                ),
                _,
                _10: 'tv_ident_or_blank,
              ) = _menhir_stack;
              let _9 = ();
              let _8 = ();
              let _5 = ();
              let _1 = ();
              let _v: Parse_tree.main(unit) = (
                {program: _4, subprograms: _7}: Parse_tree.main(unit)
              );

              _menhir_goto_main(_menhir_env, _menhir_stack, _v);
            }: 'freshtv1030
          );
        }: 'freshtv1032
      );
    | _ => _menhir_fail()
    }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_eof) => 'ttv_return
)

and _menhir_goto_comp:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_comp) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv1019 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    let _v: 'tv_comp = _v;
    (
      {
        let _menhir_env: _menhir_env = _menhir_env;
        let _menhir_stack: 'freshtv1017 = Obj.magic(_menhir_stack);
        let _menhir_s: _menhir_state = _menhir_s;
        let (_1: 'tv_comp): 'tv_comp = _v;
        (
          {

            let _v: 'tv_exp = (mkexp(~loc=mkloc(), _1): 'tv_exp);

            _menhir_goto_exp(_menhir_env, _menhir_stack, _menhir_s, _v);
          }: 'freshtv1018
        );
      }: 'freshtv1020
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_comp) => 'ttv_return
)

and _menhir_goto_logical:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_logical) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv1015 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    let _v: 'tv_logical = _v;
    (
      {
        let _menhir_env: _menhir_env = _menhir_env;
        let _menhir_stack: 'freshtv1013 = Obj.magic(_menhir_stack);
        let _menhir_s: _menhir_state = _menhir_s;
        let (_1: 'tv_logical): 'tv_logical = _v;
        (
          {

            let _v: 'tv_exp = (mkexp(~loc=mkloc(), _1): 'tv_exp);

            _menhir_goto_exp(_menhir_env, _menhir_stack, _menhir_s, _v);
          }: 'freshtv1014
        );
      }: 'freshtv1016
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_logical) => 'ttv_return
)

and _menhir_goto_arith:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_arith) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv1011 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    let _v: 'tv_arith = _v;
    (
      {
        let _menhir_env: _menhir_env = _menhir_env;
        let _menhir_stack: 'freshtv1009 = Obj.magic(_menhir_stack);
        let _menhir_s: _menhir_state = _menhir_s;
        let (_1: 'tv_arith): 'tv_arith = _v;
        (
          {

            let _v: 'tv_exp = (mkexp(~loc=mkloc(), _1): 'tv_exp);

            _menhir_goto_exp(_menhir_env, _menhir_stack, _menhir_s, _v);
          }: 'freshtv1010
        );
      }: 'freshtv1012
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_arith) => 'ttv_return
)

and _menhir_run31:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState31)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState31, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState31, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState31, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState31)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState31)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState31)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState31)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState31)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState31)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState31,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run33:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState33)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState33, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState33, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState33, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState33)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState33)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState33)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState33)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState33)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState33)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState33,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run37:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState37)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState37, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState37, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState37, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState37)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState37)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState37)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState37)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState37)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState37)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState37,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run35:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState35)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState35, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState35, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState35, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState35)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState35)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState35)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState35)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState35)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState35)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState35,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run41:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState41)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState41, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState41, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState41, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState41)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState41)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState41)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState41)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState41)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState41)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState41,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run58:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState58)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState58, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState58, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState58, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState58)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState58)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState58)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState58)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState58)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState58)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState58,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run43:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState43)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState43, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState43, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState43, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState43)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState43)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState43)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState43)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState43)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState43)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState43,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run52:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState52)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState52, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState52, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState52, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState52)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState52)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState52)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState52)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState52)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState52)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState52,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run54:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState54)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState54, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState54, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState54, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState54)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState54)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState54)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState54)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState54)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState54)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState54,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run56:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState56)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState56, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState56, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState56, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState56)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState56)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState56)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState56)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState56)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState56)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState56,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run45:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState45)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState45, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState45, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState45, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState45)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState45)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState45)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState45)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState45)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState45)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState45,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run39:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState39)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState39, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState39, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState39, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState39)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState39)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState39)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState39)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState39)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState39)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState39,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run60:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState60)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState60, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState60, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState60, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState60)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState60)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState60)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState60)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState60)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState60)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState60,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_run50:
  (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
  'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState50)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState50, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState50, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState50, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState50)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState50)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState50)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState50)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState50)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState50)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState50,
      );
    };
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, 'tv_exp), _menhir_state) =>
    'ttv_return
)

and _menhir_goto_case_opt:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_case_opt) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: ('freshtv1007, _menhir_state, 'tv_case_opt) =
      Obj.magic(_menhir_stack);
    (
      {
        assert(!_menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token;
        switch (_tok) {
        | COMMA =>
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv1001, _menhir_state, 'tv_case_opt) =
            Obj.magic(_menhir_stack);
          (
            {
              let _menhir_env = _menhir_discard(_menhir_env);
              let _tok = _menhir_env._menhir_token;
              switch (_tok) {
              | COLON =>
                _menhir_run144(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState230,
                )
              | FALSE =>
                _menhir_run25(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState230,
                )
              | FLOAT(_v) =>
                _menhir_run24(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState230,
                  _v,
                )
              | IDENT(_v) =>
                _menhir_run143(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState230,
                  _v,
                )
              | INT(_v) =>
                _menhir_run21(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState230,
                  _v,
                )
              | TRUE =>
                _menhir_run15(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState230,
                )
              | _ =>
                assert(!_menhir_env._menhir_error);
                _menhir_env._menhir_error = true;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState230,
                );
              };
            }: 'freshtv1002
          );
        | RPAREN =>
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv1003, _menhir_state, 'tv_case_opt) =
            Obj.magic(_menhir_stack);
          (
            {
              let (_menhir_stack, _menhir_s, _1: 'tv_case_opt) = _menhir_stack;
              let _v: 'tv_seq_case_opt = ([_1]: 'tv_seq_case_opt);

              _menhir_goto_seq_case_opt(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv1004
          );
        | _ =>
          assert(!_menhir_env._menhir_error);
          _menhir_env._menhir_error = true;
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv1005, _menhir_state, 'tv_case_opt) =
            Obj.magic(_menhir_stack);
          (
            {
              let (_menhir_stack, _menhir_s, _) = _menhir_stack;
              _menhir_errorcase(
                _menhir_env,
                Obj.magic(_menhir_stack),
                _menhir_s,
              );
            }: 'freshtv1006
          );
        };
      }: 'freshtv1008
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_case_opt) => 'ttv_return
)

and _menhir_goto_range:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_range) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv999 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    let _v: 'tv_range = _v;
    (
      {
        let _menhir_env: _menhir_env = _menhir_env;
        let _menhir_stack: 'freshtv997 = Obj.magic(_menhir_stack);
        let _menhir_s: _menhir_state = _menhir_s;
        let (_1: 'tv_range): 'tv_range = _v;
        (
          {

            let _v: 'tv_case_opt = (Range(_1): 'tv_case_opt);

            _menhir_goto_case_opt(_menhir_env, _menhir_stack, _menhir_s, _v);
          }: 'freshtv998
        );
      }: 'freshtv1000
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_range) => 'ttv_return
)

and _menhir_run118: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv995 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    (
      {
        let _1 = ();
        let _v: 'tv_eof = ((): 'tv_eof);

        _menhir_goto_eof(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv996
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run119: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | BR =>
      _menhir_run119(_menhir_env, Obj.magic(_menhir_stack), MenhirState119)
    | EOF =>
      _menhir_run118(_menhir_env, Obj.magic(_menhir_stack), MenhirState119)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState119,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_goto_seq_decl_assign:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_decl_assign) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState12 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv987, _menhir_state, 'tv_typ),
        _menhir_state,
        'tv_seq_decl_assign,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState104,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState104,
            );
          };
        }: 'freshtv988
      );
    | MenhirState107 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv989, _menhir_state, 'tv_typ), _menhir_state, 'tv_opt_kind),
        _menhir_state,
        'tv_seq_decl_assign,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState108,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState108,
            );
          };
        }: 'freshtv990
      );
    | MenhirState111 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv993, _menhir_state, 'tv_decl_assign),
        _menhir_state,
        'tv_seq_decl_assign,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv991, _menhir_state, 'tv_decl_assign),
            _menhir_state,
            'tv_seq_decl_assign,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_decl_assign),
                _,
                _3: 'tv_seq_decl_assign,
              ) = _menhir_stack;
              let _2 = ();
              let _v: 'tv_seq_decl_assign = (
                [_1, ..._3]: 'tv_seq_decl_assign
              );

              _menhir_goto_seq_decl_assign(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv992
          );
        }: 'freshtv994
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_decl_assign) => 'ttv_return
)

and _menhir_goto_exp:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_exp) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState27 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv841, _menhir_state), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            )
          | COMMA
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv839, _menhir_state), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (((_menhir_stack, _menhir_s), _), _, _3: 'tv_exp) = _menhir_stack;
                let _2 = ();
                let _1 = ();
                let _v: 'tv_adecl = (
                  mkdim_param(
                    ~loc=mkloc(),
                    [@implicit_arity] Colon(None, None, Some(_3)),
                  ): 'tv_adecl
                );

                _menhir_goto_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv840
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState30,
            );
          };
        }: 'freshtv842
      );
    | MenhirState31 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv845, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            )
          | BR
          | COLON
          | COMMA
          | MINUS
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv843, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_arith = (
                  [@implicit_arity] Plus(_1, _3): 'tv_arith
                );

                _menhir_goto_arith(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv844
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState32,
            );
          };
        }: 'freshtv846
      );
    | MenhirState33 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv849, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState34,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState34,
            )
          | AND
          | BR
          | COLON
          | COMMA
          | DIV
          | EQV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | NEQV
          | OR
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv847, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_logical = (
                  [@implicit_arity] Or(_1, _3): 'tv_logical
                );

                _menhir_goto_logical(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv848
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState34,
            );
          };
        }: 'freshtv850
      );
    | MenhirState35 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv853, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            )
          | BR
          | COLON
          | COMMA
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv851, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_comp = ([@implicit_arity] Neq(_1, _3): 'tv_comp);

                _menhir_goto_comp(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv852
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState36,
            );
          };
        }: 'freshtv854
      );
    | MenhirState37 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv857, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState38,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState38,
            )
          | AND
          | BR
          | COLON
          | COMMA
          | DIV
          | EQV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | NEQV
          | OR
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv855, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_logical = (
                  [@implicit_arity] Neqv(_1, _3): 'tv_logical
                );

                _menhir_goto_logical(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv856
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState38,
            );
          };
        }: 'freshtv858
      );
    | MenhirState39 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv861, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            )
          | BR
          | COLON
          | COMMA
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv859, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_comp = ([@implicit_arity] Eq(_1, _3): 'tv_comp);

                _menhir_goto_comp(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv860
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState40,
            );
          };
        }: 'freshtv862
      );
    | MenhirState41 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv865, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            )
          | BR
          | COLON
          | COMMA
          | DIV
          | MINUS
          | MUL
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv863, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_arith = (
                  [@implicit_arity] Mul(_1, _3): 'tv_arith
                );

                _menhir_goto_arith(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv864
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState42,
            );
          };
        }: 'freshtv866
      );
    | MenhirState43 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv869, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState44,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState44,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState44,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState44,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState44,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState44,
            )
          | BR
          | COLON
          | COMMA
          | DIV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv867, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_comp = ([@implicit_arity] Less(_1, _3): 'tv_comp);

                _menhir_goto_comp(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv868
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState44,
            );
          };
        }: 'freshtv870
      );
    | MenhirState45 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv873, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState46,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState46,
            )
          | AND
          | BR
          | COLON
          | COMMA
          | DIV
          | EQV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | NEQV
          | OR
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv871, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_logical = (
                  [@implicit_arity] Eqv(_1, _3): 'tv_logical
                );

                _menhir_goto_logical(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv872
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState46,
            );
          };
        }: 'freshtv874
      );
    | MenhirState50 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv877, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState51,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState51,
            )
          | AND
          | BR
          | COLON
          | COMMA
          | DIV
          | EQV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | NEQV
          | OR
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv875, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_logical = (
                  [@implicit_arity] And(_1, _3): 'tv_logical
                );

                _menhir_goto_logical(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv876
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState51,
            );
          };
        }: 'freshtv878
      );
    | MenhirState52 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv881, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState53,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState53,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState53,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState53,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState53,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState53,
            )
          | BR
          | COLON
          | COMMA
          | DIV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv879, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_comp = ([@implicit_arity] Leq(_1, _3): 'tv_comp);

                _menhir_goto_comp(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv880
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState53,
            );
          };
        }: 'freshtv882
      );
    | MenhirState54 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv885, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState55,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState55,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState55,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState55,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState55,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState55,
            )
          | BR
          | COLON
          | COMMA
          | DIV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv883, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_comp = ([@implicit_arity] Less(_3, _1): 'tv_comp);

                _menhir_goto_comp(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv884
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState55,
            );
          };
        }: 'freshtv886
      );
    | MenhirState56 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv889, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState57,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState57,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState57,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState57,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState57,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState57,
            )
          | BR
          | COLON
          | COMMA
          | DIV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv887, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_comp = ([@implicit_arity] Leq(_3, _1): 'tv_comp);

                _menhir_goto_comp(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv888
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState57,
            );
          };
        }: 'freshtv890
      );
    | MenhirState58 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv893, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            )
          | BR
          | COLON
          | COMMA
          | MINUS
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv891, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_arith = (
                  [@implicit_arity] Minus(_1, _3): 'tv_arith
                );

                _menhir_goto_arith(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv892
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState59,
            );
          };
        }: 'freshtv894
      );
    | MenhirState60 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv897, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            )
          | BR
          | COLON
          | COMMA
          | DIV
          | MINUS
          | MUL
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv895, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_arith = (
                  [@implicit_arity] Div(_1, _3): 'tv_arith
                );

                _menhir_goto_arith(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv896
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState61,
            );
          };
        }: 'freshtv898
      );
    | MenhirState26 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv903, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | COLON =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv899, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState62;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState63,
                  );
                };
              }: 'freshtv900
            );
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            )
          | COMMA
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv901, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s), _, _2: 'tv_exp) = _menhir_stack;
                let _1 = ();
                let _v: 'tv_adecl = (
                  mkdim_param(
                    ~loc=mkloc(),
                    [@implicit_arity] Colon(None, Some(_2), None),
                  ): 'tv_adecl
                );

                _menhir_goto_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv902
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState62,
            );
          };
        }: 'freshtv904
      );
    | MenhirState63 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv907, _menhir_state), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            )
          | COMMA
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (('freshtv905, _menhir_state), _menhir_state, 'tv_exp),
                _menhir_state,
              ),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  (((_menhir_stack, _menhir_s), _, _2: 'tv_exp), _),
                  _,
                  _4: 'tv_exp,
                ) = _menhir_stack;
                let _3 = ();
                let _1 = ();
                let _v: 'tv_adecl = (
                  mkdim_param(
                    ~loc=mkloc(),
                    [@implicit_arity] Colon(None, Some(_2), Some(_4)),
                  ): 'tv_adecl
                );

                _menhir_goto_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv906
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState64,
            );
          };
        }: 'freshtv908
      );
    | MenhirState65 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv911, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            )
          | COMMA
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv909, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s), _, _2: 'tv_exp) = _menhir_stack;
                let _1 = ();
                let _v: 'tv_adecl = (
                  mkdim_param(
                    ~loc=mkloc(),
                    [@implicit_arity] Colon(None, None, Some(_2)),
                  ): 'tv_adecl
                );

                _menhir_goto_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv910
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState66,
            );
          };
        }: 'freshtv912
      );
    | MenhirState163
    | MenhirState98
    | MenhirState14
    | MenhirState75
    | MenhirState23 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv917, _menhir_state, 'tv_exp) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | COLON =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv913, _menhir_state, 'tv_exp) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState69;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState70,
                  );
                };
              }: 'freshtv914
            );
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            )
          | COMMA
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv915, _menhir_state, 'tv_exp) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _1: 'tv_exp) = _menhir_stack;
                let _v: 'tv_adecl = (
                  mkdim_param(~loc=mkloc(), Exp(_1)): 'tv_adecl
                );

                _menhir_goto_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv916
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState69,
            );
          };
        }: 'freshtv918
      );
    | MenhirState70 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv923, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | COLON =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv919, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState71;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState72,
                  );
                };
              }: 'freshtv920
            );
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            )
          | COMMA
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv921, _menhir_state, 'tv_exp), _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                  _,
                  _3: 'tv_exp,
                ) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_adecl = (
                  mkdim_param(
                    ~loc=mkloc(),
                    [@implicit_arity] Colon(Some(_1), Some(_3), None),
                  ): 'tv_adecl
                );

                _menhir_goto_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv922
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState71,
            );
          };
        }: 'freshtv924
      );
    | MenhirState72 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv927, _menhir_state, 'tv_exp), _menhir_state),
            _menhir_state,
            'tv_exp,
          ),
          _menhir_state,
        ),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            )
          | COMMA
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv925, _menhir_state, 'tv_exp), _menhir_state),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
              ),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  (
                    (
                      ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                      _,
                      _3: 'tv_exp,
                    ),
                    _,
                  ),
                  _,
                  _5: 'tv_exp,
                ) = _menhir_stack;
                let _4 = ();
                let _2 = ();
                let _v: 'tv_adecl = (
                  mkdim_param(
                    ~loc=mkloc(),
                    [@implicit_arity] Colon(Some(_1), Some(_3), Some(_5)),
                  ): 'tv_adecl
                );

                _menhir_goto_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv926
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState73,
            );
          };
        }: 'freshtv928
      );
    | MenhirState187
    | MenhirState18
    | MenhirState80
    | MenhirState20 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv933, _menhir_state, 'tv_exp) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | COMMA =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv929, _menhir_state, 'tv_exp) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState79;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  )
                | RBRACE
                | RPAREN
                | S_RPAREN =>
                  _menhir_reduce91(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState80,
                  );
                };
              }: 'freshtv930
            );
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            )
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv931, _menhir_state, 'tv_exp) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _1: 'tv_exp) = _menhir_stack;
                let _v: 'tv_seq_exp = ([_1]: 'tv_seq_exp);

                _menhir_goto_seq_exp(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv932
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState79,
            );
          };
        }: 'freshtv934
      );
    | MenhirState19 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv939, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            )
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv937, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState82;
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _menhir_env: _menhir_env = _menhir_env;
                let _menhir_stack: (
                  ('freshtv935, _menhir_state),
                  _menhir_state,
                  'tv_exp,
                ) =
                  Obj.magic(_menhir_stack);
                let _: _menhir_state = _menhir_s;
                (
                  {
                    let ((_menhir_stack, _menhir_s), _, _2: 'tv_exp) = _menhir_stack;
                    let _3 = ();
                    let _1 = ();
                    let _v: 'tv_exp = (_2: 'tv_exp);

                    _menhir_goto_exp(
                      _menhir_env,
                      _menhir_stack,
                      _menhir_s,
                      _v,
                    );
                  }: 'freshtv936
                );
              }: 'freshtv938
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState82,
            );
          };
        }: 'freshtv940
      );
    | MenhirState17 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv943, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            )
          | BR
          | COLON
          | COMMA
          | MINUS
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv941, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s), _, _2: 'tv_exp) = _menhir_stack;
                let _1 = ();
                let _v: 'tv_exp = (mkexp(~loc=mkloc(), Rev(_2)): 'tv_exp);

                _menhir_goto_exp(_menhir_env, _menhir_stack, _menhir_s, _v);
              }: 'freshtv942
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState86,
            );
          };
        }: 'freshtv944
      );
    | MenhirState16 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv947, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState87,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState87,
            )
          | AND
          | BR
          | COLON
          | COMMA
          | DIV
          | EQV
          | GEQ
          | GREATER
          | LEQ
          | LESS
          | MINUS
          | MUL
          | NEQV
          | OR
          | PLUS
          | RBRACE
          | RPAREN
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv945, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s), _, _2: 'tv_exp) = _menhir_stack;
                let _1 = ();
                let _v: 'tv_logical = (Not(_2): 'tv_logical);

                _menhir_goto_logical(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv946
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState87,
            );
          };
        }: 'freshtv948
      );
    | MenhirState90 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv951, _menhir_state, string), _menhir_state, 'tv_seq_adecl),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            )
          | BR
          | COMMA =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                ('freshtv949, _menhir_state, string),
                _menhir_state,
                'tv_seq_adecl,
              ),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (
                  (
                    (_menhir_stack, _menhir_s, _1: string),
                    _,
                    _3: 'tv_seq_adecl,
                  ),
                  _,
                  _6: 'tv_exp,
                ) = _menhir_stack;
                let _5 = ();
                let _4 = ();
                let _2 = ();
                let _v: 'tv_decl_assign = (
                  (_1, Some(_3), Some(_6), mkloc()): 'tv_decl_assign
                );

                _menhir_goto_decl_assign(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv950
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState91,
            );
          };
        }: 'freshtv952
      );
    | MenhirState92 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv955, _menhir_state, string),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            )
          | BR
          | COMMA =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv953, _menhir_state, string),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s, _1: string), _, _3: 'tv_exp) = _menhir_stack;
                let _2 = ();
                let _v: 'tv_decl_assign = (
                  (_1, None, Some(_3), mkloc()): 'tv_decl_assign
                );

                _menhir_goto_decl_assign(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv954
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState93,
            );
          };
        }: 'freshtv956
      );
    | MenhirState137 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv959, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            )
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv957, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState138;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | BR =>
                  _menhir_run3(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState139,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState139,
                  );
                };
              }: 'freshtv958
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState138,
            );
          };
        }: 'freshtv960
      );
    | MenhirState152 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv961, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState153,
            );
          };
        }: 'freshtv962
      );
    | MenhirState157 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv967, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            )
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv965, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState158;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | CALL =>
                  _menhir_run185(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState159,
                  )
                | GO =>
                  _menhir_run175(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState159,
                  )
                | GOTO =>
                  _menhir_run172(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState159,
                  )
                | IDENT(_v) =>
                  _menhir_run162(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState159,
                    _v,
                  )
                | RETURN =>
                  _menhir_run152(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState159,
                  )
                | STOP =>
                  _menhir_run133(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState159,
                  )
                | THEN =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (('freshtv963, _menhir_state), _menhir_state, 'tv_exp),
                    _menhir_state,
                  ) =
                    Obj.magic(_menhir_stack);
                  let _menhir_s: _menhir_state = MenhirState159;
                  (
                    {
                      let _menhir_stack = (_menhir_stack, _menhir_s);
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | BR =>
                        _menhir_run3(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState160,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState160,
                        );
                      };
                    }: 'freshtv964
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState159,
                  );
                };
              }: 'freshtv966
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState158,
            );
          };
        }: 'freshtv968
      );
    | MenhirState166 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv969, _menhir_state, string), _menhir_state, 'tv_seq_adecl),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState167,
            );
          };
        }: 'freshtv970
      );
    | MenhirState169 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv971, _menhir_state, string),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState170,
            );
          };
        }: 'freshtv972
      );
    | MenhirState181 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv975, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            )
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv973, _menhir_state),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState182;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | BR =>
                  _menhir_run3(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState183,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState183,
                  );
                };
              }: 'freshtv974
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState182,
            );
          };
        }: 'freshtv976
      );
    | MenhirState199 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv979, _menhir_state), string),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | COMMA =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv977, _menhir_state), string),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState200;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState201,
                  );
                };
              }: 'freshtv978
            );
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState200,
            );
          };
        }: 'freshtv980
      );
    | MenhirState201 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          ((('freshtv983, _menhir_state), string), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | COMMA =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv981, _menhir_state), string),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
              ),
              _menhir_state,
              'tv_exp,
            ) =
              Obj.magic(_menhir_stack);
            let _menhir_s: _menhir_state = MenhirState202;
            (
              {
                let _menhir_stack = (_menhir_stack, _menhir_s);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState203,
                  );
                };
              }: 'freshtv982
            );
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState202,
            );
          };
        }: 'freshtv984
      );
    | MenhirState203 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv985, _menhir_state), string),
                _menhir_state,
                'tv_exp,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_exp,
          ),
          _menhir_state,
        ),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | AND =>
            _menhir_run50(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | DIV =>
            _menhir_run60(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | EQEQ =>
            _menhir_run39(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | EQV =>
            _menhir_run45(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | GEQ =>
            _menhir_run56(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | GREATER =>
            _menhir_run54(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | LEQ =>
            _menhir_run52(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | LESS =>
            _menhir_run43(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | MINUS =>
            _menhir_run58(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | MUL =>
            _menhir_run41(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | NEQ =>
            _menhir_run35(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | NEQV =>
            _menhir_run37(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | OR =>
            _menhir_run33(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | PLUS =>
            _menhir_run31(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState204,
            );
          };
        }: 'freshtv986
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_exp) => 'ttv_return
)

and _menhir_goto_simple_exp:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_simple_exp) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState203
    | MenhirState201
    | MenhirState199
    | MenhirState187
    | MenhirState181
    | MenhirState169
    | MenhirState166
    | MenhirState163
    | MenhirState157
    | MenhirState152
    | MenhirState137
    | MenhirState98
    | MenhirState92
    | MenhirState90
    | MenhirState14
    | MenhirState16
    | MenhirState17
    | MenhirState18
    | MenhirState19
    | MenhirState80
    | MenhirState20
    | MenhirState75
    | MenhirState72
    | MenhirState70
    | MenhirState23
    | MenhirState65
    | MenhirState63
    | MenhirState26
    | MenhirState60
    | MenhirState58
    | MenhirState56
    | MenhirState54
    | MenhirState52
    | MenhirState50
    | MenhirState45
    | MenhirState43
    | MenhirState41
    | MenhirState39
    | MenhirState37
    | MenhirState35
    | MenhirState33
    | MenhirState31
    | MenhirState27 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv819, _menhir_state, 'tv_simple_exp) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv817, _menhir_state, 'tv_simple_exp) =
            Obj.magic(_menhir_stack);
          (
            {
              let (_menhir_stack, _menhir_s, _1: 'tv_simple_exp) = _menhir_stack;
              let _v: 'tv_exp = (_1: 'tv_exp);

              _menhir_goto_exp(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv818
          );
        }: 'freshtv820
      );
    | MenhirState144 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv823, _menhir_state),
        _menhir_state,
        'tv_simple_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv821, _menhir_state),
            _menhir_state,
            'tv_simple_exp,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let ((_menhir_stack, _menhir_s), _, _2: 'tv_simple_exp) = _menhir_stack;
              let _1 = ();
              let _v: 'tv_range = (
                mkrange(~loc=mkloc(), None, Some(_2)): 'tv_range
              );

              _menhir_goto_range(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv822
          );
        }: 'freshtv824
      );
    | MenhirState230
    | MenhirState142 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv833, _menhir_state, 'tv_simple_exp) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | COLON =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv827, _menhir_state, 'tv_simple_exp) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState147,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState147,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run143(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState147,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState147,
                    _v,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState147,
                  )
                | COMMA
                | RPAREN =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    'freshtv825,
                    _menhir_state,
                    'tv_simple_exp,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _1: 'tv_simple_exp) = _menhir_stack;
                      let _2 = ();
                      let _v: 'tv_range = (
                        mkrange(~loc=mkloc(), Some(_1), None): 'tv_range
                      );

                      _menhir_goto_range(
                        _menhir_env,
                        _menhir_stack,
                        _menhir_s,
                        _v,
                      );
                    }: 'freshtv826
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState147,
                  );
                };
              }: 'freshtv828
            );
          | COMMA
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv829, _menhir_state, 'tv_simple_exp) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _1: 'tv_simple_exp) = _menhir_stack;
                let _v: 'tv_case_opt = (Scala(_1): 'tv_case_opt);

                _menhir_goto_case_opt(
                  _menhir_env,
                  _menhir_stack,
                  _menhir_s,
                  _v,
                );
              }: 'freshtv830
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv831, _menhir_state, 'tv_simple_exp) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv832
            );
          };
        }: 'freshtv834
      );
    | MenhirState147 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv837, _menhir_state, 'tv_simple_exp),
        _menhir_state,
        'tv_simple_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv835, _menhir_state, 'tv_simple_exp),
            _menhir_state,
            'tv_simple_exp,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_simple_exp),
                _,
                _3: 'tv_simple_exp,
              ) = _menhir_stack;
              let _2 = ();
              let _v: 'tv_range = (
                mkrange(~loc=mkloc(), Some(_1), Some(_3)): 'tv_range
              );

              _menhir_goto_range(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv836
          );
        }: 'freshtv838
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_simple_exp) => 'ttv_return
)

and _menhir_goto_ident_or_blank:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_ident_or_blank) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState115 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv807, string), _menhir_state, 'tv_br),
          _menhir_state,
          'tv_top_block,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run119(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState117,
            )
          | EOF =>
            _menhir_run118(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState117,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState117,
            );
          };
        }: 'freshtv808
      );
    | MenhirState237 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv809, _menhir_state), _menhir_state, 'tv_exp),
              _menhir_state,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_case,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState238,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState238,
            );
          };
        }: 'freshtv810
      );
    | MenhirState244 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv811, _menhir_state), string),
              _menhir_state,
              'tv_seq_ident,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState245,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState245,
            );
          };
        }: 'freshtv812
      );
    | MenhirState255 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv813, _menhir_state), string),
              _menhir_state,
              'tv_seq_ident,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState256,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState256,
            );
          };
        }: 'freshtv814
      );
    | MenhirState262 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv815, string), _menhir_state, 'tv_br),
              _menhir_state,
              'tv_top_block,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_subprogram,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run119(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState263,
            )
          | EOF =>
            _menhir_run118(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState263,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState263,
            );
          };
        }: 'freshtv816
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_ident_or_blank) => 'ttv_return
)

and _menhir_goto_opt_kind:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_opt_kind) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState102 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv799, _menhir_state), 'tv_kind),
        _menhir_state,
        'tv_opt_kind,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (('freshtv797, _menhir_state), 'tv_kind),
            _menhir_state,
            'tv_opt_kind,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                ((_menhir_stack, _menhir_s), _2: 'tv_kind),
                _,
                _3: 'tv_opt_kind,
              ) = _menhir_stack;
              let _1 = ();
              let _v: 'tv_opt_kind = ([_2, ..._3]: 'tv_opt_kind);

              _menhir_goto_opt_kind(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv798
          );
        }: 'freshtv800
      );
    | MenhirState12 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv805, _menhir_state, 'tv_typ),
        _menhir_state,
        'tv_opt_kind,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | COLCOL =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv801, _menhir_state, 'tv_typ),
              _menhir_state,
              'tv_opt_kind,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | IDENT(_v) =>
                  _menhir_run13(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState107,
                    _v,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState107,
                  );
                };
              }: 'freshtv802
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv803, _menhir_state, 'tv_typ),
              _menhir_state,
              'tv_opt_kind,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv804
            );
          };
        }: 'freshtv806
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_opt_kind) => 'ttv_return
)

and _menhir_goto_decl_assign:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_decl_assign) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: ('freshtv795, _menhir_state, 'tv_decl_assign) =
      Obj.magic(_menhir_stack);
    (
      {
        assert(!_menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token;
        switch (_tok) {
        | COMMA =>
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv789, _menhir_state, 'tv_decl_assign) =
            Obj.magic(_menhir_stack);
          (
            {
              let _menhir_env = _menhir_discard(_menhir_env);
              let _tok = _menhir_env._menhir_token;
              switch (_tok) {
              | IDENT(_v) =>
                _menhir_run13(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState111,
                  _v,
                )
              | _ =>
                assert(!_menhir_env._menhir_error);
                _menhir_env._menhir_error = true;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState111,
                );
              };
            }: 'freshtv790
          );
        | BR =>
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv791, _menhir_state, 'tv_decl_assign) =
            Obj.magic(_menhir_stack);
          (
            {
              let (_menhir_stack, _menhir_s, _1: 'tv_decl_assign) = _menhir_stack;
              let _v: 'tv_seq_decl_assign = ([_1]: 'tv_seq_decl_assign);

              _menhir_goto_seq_decl_assign(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv792
          );
        | _ =>
          assert(!_menhir_env._menhir_error);
          _menhir_env._menhir_error = true;
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv793, _menhir_state, 'tv_decl_assign) =
            Obj.magic(_menhir_stack);
          (
            {
              let (_menhir_stack, _menhir_s, _) = _menhir_stack;
              _menhir_errorcase(
                _menhir_env,
                Obj.magic(_menhir_stack),
                _menhir_s,
              );
            }: 'freshtv794
          );
        };
      }: 'freshtv796
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_decl_assign) => 'ttv_return
)

and _menhir_goto_kind: (_menhir_env, 'ttv_tail, 'tv_kind) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _v) => {
    let _menhir_stack = (_menhir_stack, _v);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: (('freshtv787, _menhir_state), 'tv_kind) =
      Obj.magic(_menhir_stack);
    (
      {
        assert(!_menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token;
        switch (_tok) {
        | COMMA =>
          _menhir_run94(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState102,
          )
        | COLCOL =>
          _menhir_reduce75(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState102,
          )
        | _ =>
          assert(!_menhir_env._menhir_error);
          _menhir_env._menhir_error = true;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState102,
          );
        };
      }: 'freshtv788
    );
  }:
    (_menhir_env, 'ttv_tail, 'tv_kind) => 'ttv_return
)

and _menhir_goto_case:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_case) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: ('freshtv785, _menhir_state, 'tv_case) =
      Obj.magic(_menhir_stack);
    (
      {
        assert(!_menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token;
        switch (_tok) {
        | CASE =>
          _menhir_run141(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState240,
          )
        | END =>
          _menhir_reduce83(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState240,
          )
        | _ =>
          assert(!_menhir_env._menhir_error);
          _menhir_env._menhir_error = true;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState240,
          );
        };
      }: 'freshtv786
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_case) => 'ttv_return
)

and _menhir_goto_seq_adecl:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_adecl) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState23 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv749, _menhir_state, string),
        _menhir_state,
        'tv_seq_adecl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv745, _menhir_state, string),
              _menhir_state,
              'tv_seq_adecl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _menhir_env: _menhir_env = _menhir_env;
                let _menhir_stack: (
                  ('freshtv743, _menhir_state, string),
                  _menhir_state,
                  'tv_seq_adecl,
                ) =
                  Obj.magic(_menhir_stack);
                (
                  {
                    let (
                      (_menhir_stack, _menhir_s, _1: string),
                      _,
                      _3: 'tv_seq_adecl,
                    ) = _menhir_stack;
                    let _4 = ();
                    let _2 = ();
                    let _v: 'tv_exp = (
                      mkexp(~loc=mkloc(), [@implicit_arity] Funcall(_1, _3)): 'tv_exp
                    );

                    _menhir_goto_exp(
                      _menhir_env,
                      _menhir_stack,
                      _menhir_s,
                      _v,
                    );
                  }: 'freshtv744
                );
              }: 'freshtv746
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv747, _menhir_state, string),
              _menhir_state,
              'tv_seq_adecl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv748
            );
          };
        }: 'freshtv750
      );
    | MenhirState75 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv753, _menhir_state, 'tv_adecl),
        _menhir_state,
        'tv_seq_adecl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv751, _menhir_state, 'tv_adecl),
            _menhir_state,
            'tv_seq_adecl,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_adecl),
                _,
                _3: 'tv_seq_adecl,
              ) = _menhir_stack;
              let _2 = ();
              let _v: 'tv_seq_adecl = ([_1, ..._3]: 'tv_seq_adecl);

              _menhir_goto_seq_adecl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv752
          );
        }: 'freshtv754
      );
    | MenhirState14 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv765, _menhir_state, string),
        _menhir_state,
        'tv_seq_adecl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv761, _menhir_state, string),
              _menhir_state,
              'tv_seq_adecl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | EQ =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    ('freshtv755, _menhir_state, string),
                    _menhir_state,
                    'tv_seq_adecl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | FALSE =>
                        _menhir_run25(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                        )
                      | FLOAT(_v) =>
                        _menhir_run24(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                          _v,
                        )
                      | IDENT(_v) =>
                        _menhir_run22(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                          _v,
                        )
                      | INT(_v) =>
                        _menhir_run21(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                          _v,
                        )
                      | LBRACE =>
                        _menhir_run20(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                        )
                      | LPAREN =>
                        _menhir_run19(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                        )
                      | LPAREN_S =>
                        _menhir_run18(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                        )
                      | MINUS =>
                        _menhir_run17(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                        )
                      | NOT =>
                        _menhir_run16(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                        )
                      | TRUE =>
                        _menhir_run15(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState90,
                        );
                      };
                    }: 'freshtv756
                  );
                | BR
                | COMMA =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    ('freshtv757, _menhir_state, string),
                    _menhir_state,
                    'tv_seq_adecl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (
                        (_menhir_stack, _menhir_s, _1: string),
                        _,
                        _3: 'tv_seq_adecl,
                      ) = _menhir_stack;
                      let _4 = ();
                      let _2 = ();
                      let _v: 'tv_decl_assign = (
                        (_1, Some(_3), None, mkloc()): 'tv_decl_assign
                      );

                      _menhir_goto_decl_assign(
                        _menhir_env,
                        _menhir_stack,
                        _menhir_s,
                        _v,
                      );
                    }: 'freshtv758
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    ('freshtv759, _menhir_state, string),
                    _menhir_state,
                    'tv_seq_adecl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv760
                  );
                };
              }: 'freshtv762
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv763, _menhir_state, string),
              _menhir_state,
              'tv_seq_adecl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv764
            );
          };
        }: 'freshtv766
      );
    | MenhirState98 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv773, _menhir_state, 'tv_seq_adecl) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv769, _menhir_state, 'tv_seq_adecl) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _menhir_env: _menhir_env = _menhir_env;
                let _menhir_stack: ('freshtv767, _menhir_state, 'tv_seq_adecl) =
                  Obj.magic(_menhir_stack);
                (
                  {
                    let (_menhir_stack, _, _3: 'tv_seq_adecl) = _menhir_stack;
                    let _4 = ();
                    let _2 = ();
                    let _1 = ();
                    let _v: 'tv_kind = (
                      mkkind(~loc=mkloc(), Dimension(_3)): 'tv_kind
                    );

                    _menhir_goto_kind(_menhir_env, _menhir_stack, _v);
                  }: 'freshtv768
                );
              }: 'freshtv770
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv771, _menhir_state, 'tv_seq_adecl) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv772
            );
          };
        }: 'freshtv774
      );
    | MenhirState163 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv783, _menhir_state, string),
        _menhir_state,
        'tv_seq_adecl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv779, _menhir_state, string),
              _menhir_state,
              'tv_seq_adecl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | EQ =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    ('freshtv775, _menhir_state, string),
                    _menhir_state,
                    'tv_seq_adecl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | FALSE =>
                        _menhir_run25(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                        )
                      | FLOAT(_v) =>
                        _menhir_run24(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                          _v,
                        )
                      | IDENT(_v) =>
                        _menhir_run22(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                          _v,
                        )
                      | INT(_v) =>
                        _menhir_run21(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                          _v,
                        )
                      | LBRACE =>
                        _menhir_run20(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                        )
                      | LPAREN =>
                        _menhir_run19(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                        )
                      | LPAREN_S =>
                        _menhir_run18(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                        )
                      | MINUS =>
                        _menhir_run17(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                        )
                      | NOT =>
                        _menhir_run16(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                        )
                      | TRUE =>
                        _menhir_run15(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState166,
                        );
                      };
                    }: 'freshtv776
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    ('freshtv777, _menhir_state, string),
                    _menhir_state,
                    'tv_seq_adecl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv778
                  );
                };
              }: 'freshtv780
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv781, _menhir_state, string),
              _menhir_state,
              'tv_seq_adecl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv782
            );
          };
        }: 'freshtv784
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_adecl) => 'ttv_return
)

and _menhir_goto_adecl:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_adecl) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: ('freshtv741, _menhir_state, 'tv_adecl) =
      Obj.magic(_menhir_stack);
    (
      {
        assert(!_menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token;
        switch (_tok) {
        | COMMA =>
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv735, _menhir_state, 'tv_adecl) =
            Obj.magic(_menhir_stack);
          (
            {
              let _menhir_env = _menhir_discard(_menhir_env);
              let _tok = _menhir_env._menhir_token;
              switch (_tok) {
              | COLCOL =>
                _menhir_run65(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | COLON =>
                _menhir_run26(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | FALSE =>
                _menhir_run25(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | FLOAT(_v) =>
                _menhir_run24(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                  _v,
                )
              | IDENT(_v) =>
                _menhir_run22(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                  _v,
                )
              | INT(_v) =>
                _menhir_run21(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                  _v,
                )
              | LBRACE =>
                _menhir_run20(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | LPAREN =>
                _menhir_run19(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | LPAREN_S =>
                _menhir_run18(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | MINUS =>
                _menhir_run17(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | NOT =>
                _menhir_run16(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | TRUE =>
                _menhir_run15(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | RPAREN =>
                _menhir_reduce80(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                )
              | _ =>
                assert(!_menhir_env._menhir_error);
                _menhir_env._menhir_error = true;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  MenhirState75,
                );
              };
            }: 'freshtv736
          );
        | RPAREN =>
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv737, _menhir_state, 'tv_adecl) =
            Obj.magic(_menhir_stack);
          (
            {
              let (_menhir_stack, _menhir_s, _1: 'tv_adecl) = _menhir_stack;
              let _v: 'tv_seq_adecl = ([_1]: 'tv_seq_adecl);

              _menhir_goto_seq_adecl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv738
          );
        | _ =>
          assert(!_menhir_env._menhir_error);
          _menhir_env._menhir_error = true;
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv739, _menhir_state, 'tv_adecl) =
            Obj.magic(_menhir_stack);
          (
            {
              let (_menhir_stack, _menhir_s, _) = _menhir_stack;
              _menhir_errorcase(
                _menhir_env,
                Obj.magic(_menhir_stack),
                _menhir_s,
              );
            }: 'freshtv740
          );
        };
      }: 'freshtv742
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_adecl) => 'ttv_return
)

and _menhir_goto_seq_exp:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_exp) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState20 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv715, _menhir_state),
        _menhir_state,
        'tv_seq_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RBRACE =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv711, _menhir_state),
              _menhir_state,
              'tv_seq_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _menhir_env: _menhir_env = _menhir_env;
                let _menhir_stack: (
                  ('freshtv709, _menhir_state),
                  _menhir_state,
                  'tv_seq_exp,
                ) =
                  Obj.magic(_menhir_stack);
                (
                  {
                    let ((_menhir_stack, _menhir_s), _, _2: 'tv_seq_exp) = _menhir_stack;
                    let _3 = ();
                    let _1 = ();
                    let _v: 'tv_exp = (
                      mkexp(~loc=mkloc(), Array(_2)): 'tv_exp
                    );

                    _menhir_goto_exp(
                      _menhir_env,
                      _menhir_stack,
                      _menhir_s,
                      _v,
                    );
                  }: 'freshtv710
                );
              }: 'freshtv712
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv713, _menhir_state),
              _menhir_state,
              'tv_seq_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv714
            );
          };
        }: 'freshtv716
      );
    | MenhirState80 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv719, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_seq_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (('freshtv717, _menhir_state, 'tv_exp), _menhir_state),
            _menhir_state,
            'tv_seq_exp,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                ((_menhir_stack, _menhir_s, _1: 'tv_exp), _),
                _,
                _3: 'tv_seq_exp,
              ) = _menhir_stack;
              let _2 = ();
              let _v: 'tv_seq_exp = ([_1, ..._3]: 'tv_seq_exp);

              _menhir_goto_seq_exp(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv718
          );
        }: 'freshtv720
      );
    | MenhirState18 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv727, _menhir_state),
        _menhir_state,
        'tv_seq_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | S_RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv723, _menhir_state),
              _menhir_state,
              'tv_seq_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _menhir_env: _menhir_env = _menhir_env;
                let _menhir_stack: (
                  ('freshtv721, _menhir_state),
                  _menhir_state,
                  'tv_seq_exp,
                ) =
                  Obj.magic(_menhir_stack);
                (
                  {
                    let ((_menhir_stack, _menhir_s), _, _2: 'tv_seq_exp) = _menhir_stack;
                    let _3 = ();
                    let _1 = ();
                    let _v: 'tv_exp = (
                      mkexp(~loc=mkloc(), Array(_2)): 'tv_exp
                    );

                    _menhir_goto_exp(
                      _menhir_env,
                      _menhir_stack,
                      _menhir_s,
                      _v,
                    );
                  }: 'freshtv722
                );
              }: 'freshtv724
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              ('freshtv725, _menhir_state),
              _menhir_state,
              'tv_seq_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv726
            );
          };
        }: 'freshtv728
      );
    | MenhirState187 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv733, _menhir_state), string),
        _menhir_state,
        'tv_seq_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv729, _menhir_state), string),
              _menhir_state,
              'tv_seq_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | BR =>
                  _menhir_run3(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState189,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState189,
                  );
                };
              }: 'freshtv730
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv731, _menhir_state), string),
              _menhir_state,
              'tv_seq_exp,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv732
            );
          };
        }: 'freshtv734
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_exp) => 'ttv_return
)

and _menhir_reduce102:
  (_menhir_env, ('ttv_tail, _menhir_state, string)) => 'ttv_return = (
  (_menhir_env, _menhir_stack) => {
    let (_menhir_stack, _menhir_s, _1: string) = _menhir_stack;
    let _v: 'tv_simple_exp = (
      mkexp(~loc=mkloc(), Ident(_1)): 'tv_simple_exp
    );

    _menhir_goto_simple_exp(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, ('ttv_tail, _menhir_state, string)) => 'ttv_return
)

and _menhir_goto_const:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_const) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv707 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    let _v: 'tv_const = _v;
    (
      {
        let _menhir_env: _menhir_env = _menhir_env;
        let _menhir_stack: 'freshtv705 = Obj.magic(_menhir_stack);
        let _menhir_s: _menhir_state = _menhir_s;
        let (_1: 'tv_const): 'tv_const = _v;
        (
          {

            let _v: 'tv_simple_exp = (
              mkexp(~loc=mkloc(), Const(_1)): 'tv_simple_exp
            );

            _menhir_goto_simple_exp(
              _menhir_env,
              _menhir_stack,
              _menhir_s,
              _v,
            );
          }: 'freshtv706
        );
      }: 'freshtv708
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_const) => 'ttv_return
)

and _menhir_reduce55: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_ident_or_blank = ("empty": 'tv_ident_or_blank);

    _menhir_goto_ident_or_blank(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run116:
  (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv703 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    let (_1: string): string = _v;
    (
      {

        let _v: 'tv_ident_or_blank = (_1: 'tv_ident_or_blank);

        _menhir_goto_ident_or_blank(
          _menhir_env,
          _menhir_stack,
          _menhir_s,
          _v,
        );
      }: 'freshtv704
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return
)

and _menhir_goto_seq_ident:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_ident) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState128 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv689, _menhir_state, string),
        _menhir_state,
        'tv_seq_ident,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv687, _menhir_state, string),
            _menhir_state,
            'tv_seq_ident,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: string),
                _,
                _3: 'tv_seq_ident,
              ) = _menhir_stack;
              let _2 = ();
              let _v: 'tv_seq_ident = ([_1, ..._3]: 'tv_seq_ident);

              _menhir_goto_seq_ident(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv688
          );
        }: 'freshtv690
      );
    | MenhirState126 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv695, _menhir_state), string),
        _menhir_state,
        'tv_seq_ident,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv691, _menhir_state), string),
              _menhir_state,
              'tv_seq_ident,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | BR =>
                  _menhir_run3(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState131,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState131,
                  );
                };
              }: 'freshtv692
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv693, _menhir_state), string),
              _menhir_state,
              'tv_seq_ident,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv694
            );
          };
        }: 'freshtv696
      );
    | MenhirState249 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv701, _menhir_state), string),
        _menhir_state,
        'tv_seq_ident,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | RPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv697, _menhir_state), string),
              _menhir_state,
              'tv_seq_ident,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | BR =>
                  _menhir_run3(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState251,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState251,
                  );
                };
              }: 'freshtv698
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (('freshtv699, _menhir_state), string),
              _menhir_state,
              'tv_seq_ident,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv700
            );
          };
        }: 'freshtv702
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_ident) => 'ttv_return
)

and _menhir_reduce75: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_opt_kind = ([]: 'tv_opt_kind);

    _menhir_goto_opt_kind(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run13:
  (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | EQ =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv679, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | FALSE =>
            _menhir_run25(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
            )
          | FLOAT(_v) =>
            _menhir_run24(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
              _v,
            )
          | IDENT(_v) =>
            _menhir_run22(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
              _v,
            )
          | INT(_v) =>
            _menhir_run21(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
              _v,
            )
          | LBRACE =>
            _menhir_run20(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
            )
          | LPAREN =>
            _menhir_run19(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
            )
          | LPAREN_S =>
            _menhir_run18(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
            )
          | MINUS =>
            _menhir_run17(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
            )
          | NOT =>
            _menhir_run16(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
            )
          | TRUE =>
            _menhir_run15(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState92,
            );
          };
        }: 'freshtv680
      );
    | LPAREN =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv681, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | COLCOL =>
            _menhir_run65(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | COLON =>
            _menhir_run26(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | FALSE =>
            _menhir_run25(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | FLOAT(_v) =>
            _menhir_run24(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
              _v,
            )
          | IDENT(_v) =>
            _menhir_run22(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
              _v,
            )
          | INT(_v) =>
            _menhir_run21(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
              _v,
            )
          | LBRACE =>
            _menhir_run20(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | LPAREN =>
            _menhir_run19(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | LPAREN_S =>
            _menhir_run18(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | MINUS =>
            _menhir_run17(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | NOT =>
            _menhir_run16(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | TRUE =>
            _menhir_run15(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | RPAREN =>
            _menhir_reduce80(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState14,
            );
          };
        }: 'freshtv682
      );
    | BR
    | COMMA =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv683, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _1: string) = _menhir_stack;
          let _v: 'tv_decl_assign = (
            (_1, None, None, mkloc()): 'tv_decl_assign
          );

          _menhir_goto_decl_assign(_menhir_env, _menhir_stack, _menhir_s, _v);
        }: 'freshtv684
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv685, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv686
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return
)

and _menhir_run94: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | ALLOCATABLE =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: 'freshtv661 = Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: 'freshtv659 = Obj.magic(_menhir_stack);
          (
            {
              let _1 = ();
              let _v: 'tv_kind = (
                mkkind(~loc=mkloc(), Allocatable): 'tv_kind
              );

              _menhir_goto_kind(_menhir_env, _menhir_stack, _v);
            }: 'freshtv660
          );
        }: 'freshtv662
      );
    | DIMENSION =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: 'freshtv667 = Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | LPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: 'freshtv663 = Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | COLCOL =>
                  _menhir_run65(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | COLON =>
                  _menhir_run26(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | RPAREN =>
                  _menhir_reduce80(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState98,
                  );
                };
              }: 'freshtv664
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: 'freshtv665 = Obj.magic(_menhir_stack);
            (raise(_eRR): 'freshtv666);
          };
        }: 'freshtv668
      );
    | PARAMETER =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: 'freshtv671 = Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: 'freshtv669 = Obj.magic(_menhir_stack);
          (
            {
              let _1 = ();
              let _v: 'tv_kind = (mkkind(~loc=mkloc(), Parameter): 'tv_kind);

              _menhir_goto_kind(_menhir_env, _menhir_stack, _v);
            }: 'freshtv670
          );
        }: 'freshtv672
      );
    | POINTER =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: 'freshtv675 = Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: 'freshtv673 = Obj.magic(_menhir_stack);
          (
            {
              let _1 = ();
              let _v: 'tv_kind = (mkkind(~loc=mkloc(), Pointer): 'tv_kind);

              _menhir_goto_kind(_menhir_env, _menhir_stack, _v);
            }: 'freshtv674
          );
        }: 'freshtv676
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv677, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv678
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_goto_seq_case:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_case) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState140 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv653, _menhir_state), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_case,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv649, _menhir_state), _menhir_state, 'tv_exp),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_case,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | SELECT =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          ('freshtv645, _menhir_state),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_case,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | IDENT(_v) =>
                        _menhir_run116(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState237,
                          _v,
                        )
                      | BR =>
                        _menhir_reduce55(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState237,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState237,
                        );
                      };
                    }: 'freshtv646
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          ('freshtv647, _menhir_state),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_case,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv648
                  );
                };
              }: 'freshtv650
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv651, _menhir_state), _menhir_state, 'tv_exp),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_case,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv652
            );
          };
        }: 'freshtv654
      );
    | MenhirState240 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv657, _menhir_state, 'tv_case),
        _menhir_state,
        'tv_seq_case,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv655, _menhir_state, 'tv_case),
            _menhir_state,
            'tv_seq_case,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_case),
                _,
                _2: 'tv_seq_case,
              ) = _menhir_stack;
              let _v: 'tv_seq_case = ([_1, ..._2]: 'tv_seq_case);

              _menhir_goto_seq_case(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv656
          );
        }: 'freshtv658
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_case) => 'ttv_return
)

and _menhir_run143:
  (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env = _menhir_discard(_menhir_env);
    _menhir_reduce102(_menhir_env, Obj.magic(_menhir_stack));
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return
)

and _menhir_run144: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState144)
    | FLOAT(_v) =>
      _menhir_run24(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState144,
        _v,
      )
    | IDENT(_v) =>
      _menhir_run143(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState144,
        _v,
      )
    | INT(_v) =>
      _menhir_run21(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState144,
        _v,
      )
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState144)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState144,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_goto_seq_decl:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_decl) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState184 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv551, _menhir_state), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv547, _menhir_state), _menhir_state, 'tv_exp),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | DO =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          ('freshtv543, _menhir_state),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | BR =>
                        _menhir_run3(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState193,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState193,
                        );
                      };
                    }: 'freshtv544
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          ('freshtv545, _menhir_state),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv546
                  );
                };
              }: 'freshtv548
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv549, _menhir_state), _menhir_state, 'tv_exp),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv550
            );
          };
        }: 'freshtv552
      );
    | MenhirState196 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv555, _menhir_state, 'tv_decl),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv553, _menhir_state, 'tv_decl),
            _menhir_state,
            'tv_seq_decl,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_decl),
                _,
                _2: 'tv_seq_decl,
              ) = _menhir_stack;
              let _v: 'tv_seq_decl = ([_1, ..._2]: 'tv_seq_decl);

              _menhir_goto_seq_decl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv554
          );
        }: 'freshtv556
      );
    | MenhirState205 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (
                    (('freshtv565, _menhir_state), string),
                    _menhir_state,
                    'tv_exp,
                  ),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_exp,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_exp,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (
                      (
                        (
                          (('freshtv561, _menhir_state), string),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_exp,
                    ),
                    _menhir_state,
                  ),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | DO =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          (
                            (
                              (
                                (('freshtv557, _menhir_state), string),
                                _menhir_state,
                                'tv_exp,
                              ),
                              _menhir_state,
                            ),
                            _menhir_state,
                            'tv_exp,
                          ),
                          _menhir_state,
                        ),
                        _menhir_state,
                        'tv_exp,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | BR =>
                        _menhir_run3(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState208,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState208,
                        );
                      };
                    }: 'freshtv558
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          (
                            (
                              (
                                (('freshtv559, _menhir_state), string),
                                _menhir_state,
                                'tv_exp,
                              ),
                              _menhir_state,
                            ),
                            _menhir_state,
                            'tv_exp,
                          ),
                          _menhir_state,
                        ),
                        _menhir_state,
                        'tv_exp,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv560
                  );
                };
              }: 'freshtv562
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (
                      (
                        (
                          (('freshtv563, _menhir_state), string),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_exp,
                    ),
                    _menhir_state,
                  ),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv564
            );
          };
        }: 'freshtv566
      );
    | MenhirState210 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv575, _menhir_state), string),
                _menhir_state,
                'tv_exp,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_exp,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (
                      (('freshtv571, _menhir_state), string),
                      _menhir_state,
                      'tv_exp,
                    ),
                    _menhir_state,
                  ),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | DO =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          (
                            (('freshtv567, _menhir_state), string),
                            _menhir_state,
                            'tv_exp,
                          ),
                          _menhir_state,
                        ),
                        _menhir_state,
                        'tv_exp,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | BR =>
                        _menhir_run3(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState213,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState213,
                        );
                      };
                    }: 'freshtv568
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          (
                            (('freshtv569, _menhir_state), string),
                            _menhir_state,
                            'tv_exp,
                          ),
                          _menhir_state,
                        ),
                        _menhir_state,
                        'tv_exp,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv570
                  );
                };
              }: 'freshtv572
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (
                      (('freshtv573, _menhir_state), string),
                      _menhir_state,
                      'tv_exp,
                    ),
                    _menhir_state,
                  ),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv574
            );
          };
        }: 'freshtv576
      );
    | MenhirState161 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv587, _menhir_state), _menhir_state, 'tv_exp),
              _menhir_state,
            ),
            _menhir_state,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | ELSE =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (('freshtv577, _menhir_state), _menhir_state, 'tv_exp),
                    _menhir_state,
                  ),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | BR =>
                  _menhir_run3(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState219,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState219,
                  );
                };
              }: 'freshtv578
            );
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (('freshtv583, _menhir_state), _menhir_state, 'tv_exp),
                    _menhir_state,
                  ),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | IF =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          (
                            ('freshtv579, _menhir_state),
                            _menhir_state,
                            'tv_exp,
                          ),
                          _menhir_state,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | BR =>
                        _menhir_run3(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState217,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState217,
                        );
                      };
                    }: 'freshtv580
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          (
                            ('freshtv581, _menhir_state),
                            _menhir_state,
                            'tv_exp,
                          ),
                          _menhir_state,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv582
                  );
                };
              }: 'freshtv584
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (('freshtv585, _menhir_state), _menhir_state, 'tv_exp),
                    _menhir_state,
                  ),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv586
            );
          };
        }: 'freshtv588
      );
    | MenhirState220 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (('freshtv597, _menhir_state), _menhir_state, 'tv_exp),
                  _menhir_state,
                ),
                _menhir_state,
              ),
              _menhir_state,
              'tv_br,
            ),
            _menhir_state,
            'tv_seq_decl,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (
                      (
                        (
                          ('freshtv593, _menhir_state),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                    ),
                    _menhir_state,
                    'tv_br,
                  ),
                  _menhir_state,
                  'tv_seq_decl,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | IF =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          (
                            (
                              (
                                ('freshtv589, _menhir_state),
                                _menhir_state,
                                'tv_exp,
                              ),
                              _menhir_state,
                            ),
                            _menhir_state,
                          ),
                          _menhir_state,
                          'tv_br,
                        ),
                        _menhir_state,
                        'tv_seq_decl,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | BR =>
                        _menhir_run3(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState223,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState223,
                        );
                      };
                    }: 'freshtv590
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (
                          (
                            (
                              (
                                ('freshtv591, _menhir_state),
                                _menhir_state,
                                'tv_exp,
                              ),
                              _menhir_state,
                            ),
                            _menhir_state,
                          ),
                          _menhir_state,
                          'tv_br,
                        ),
                        _menhir_state,
                        'tv_seq_decl,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv592
                  );
                };
              }: 'freshtv594
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (
                    (
                      (
                        (
                          ('freshtv595, _menhir_state),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                    ),
                    _menhir_state,
                    'tv_br,
                  ),
                  _menhir_state,
                  'tv_seq_decl,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv596
            );
          };
        }: 'freshtv598
      );
    | MenhirState151 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv601, _menhir_state), _menhir_state, 'tv_seq_case_opt),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (('freshtv599, _menhir_state), _menhir_state, 'tv_seq_case_opt),
              _menhir_state,
              'tv_br,
            ),
            _menhir_state,
            'tv_seq_decl,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  ((_menhir_stack, _menhir_s), _, _3: 'tv_seq_case_opt),
                  _,
                  _5: 'tv_br,
                ),
                _,
                _6: 'tv_seq_decl,
              ) = _menhir_stack;
              let _4 = ();
              let _2 = ();
              let _1 = ();
              let _v: 'tv_case = (mkcase(~loc=mkloc(), _3, _6): 'tv_case);

              _menhir_goto_case(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv600
          );
        }: 'freshtv602
      );
    | MenhirState233 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv605, _menhir_state), _menhir_state, 'tv_br),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (('freshtv603, _menhir_state), _menhir_state, 'tv_br),
            _menhir_state,
            'tv_seq_decl,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                ((_menhir_stack, _menhir_s), _, _3: 'tv_br),
                _,
                _4: 'tv_seq_decl,
              ) = _menhir_stack;
              let _2 = ();
              let _1 = ();
              let _v: 'tv_case = (mkcase(~loc=mkloc(), [], _4): 'tv_case);

              _menhir_goto_case(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv604
          );
        }: 'freshtv606
      );
    | MenhirState132 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv615, _menhir_state), string),
            _menhir_state,
            'tv_seq_ident,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv611, _menhir_state), string),
                  _menhir_state,
                  'tv_seq_ident,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | SUBROUTINE =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (('freshtv607, _menhir_state), string),
                        _menhir_state,
                        'tv_seq_ident,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | IDENT(_v) =>
                        _menhir_run116(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState244,
                          _v,
                        )
                      | BR =>
                        _menhir_reduce55(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState244,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState244,
                        );
                      };
                    }: 'freshtv608
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (('freshtv609, _menhir_state), string),
                        _menhir_state,
                        'tv_seq_ident,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv610
                  );
                };
              }: 'freshtv612
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv613, _menhir_state), string),
                  _menhir_state,
                  'tv_seq_ident,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv614
            );
          };
        }: 'freshtv616
      );
    | MenhirState252 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv625, _menhir_state), string),
            _menhir_state,
            'tv_seq_ident,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv621, _menhir_state), string),
                  _menhir_state,
                  'tv_seq_ident,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FUNCTION =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (('freshtv617, _menhir_state), string),
                        _menhir_state,
                        'tv_seq_ident,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | IDENT(_v) =>
                        _menhir_run116(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState255,
                          _v,
                        )
                      | BR =>
                        _menhir_reduce55(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState255,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState255,
                        );
                      };
                    }: 'freshtv618
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (('freshtv619, _menhir_state), string),
                        _menhir_state,
                        'tv_seq_ident,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_decl,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv620
                  );
                };
              }: 'freshtv622
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv623, _menhir_state), string),
                  _menhir_state,
                  'tv_seq_ident,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv624
            );
          };
        }: 'freshtv626
      );
    | MenhirState265 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv643, _menhir_state, 'tv_seq_var),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv641, _menhir_state, 'tv_seq_var),
            _menhir_state,
            'tv_seq_decl,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_seq_var),
                _,
                _2: 'tv_seq_decl,
              ) = _menhir_stack;
              let _v: 'tv_top_block = (mkblock(_1, _2): 'tv_top_block);

              let _menhir_env: _menhir_env = _menhir_env;
              let _menhir_stack: 'freshtv639 = _menhir_stack;
              let _menhir_s: _menhir_state = _menhir_s;
              let _v: 'tv_top_block = _v;
              (
                {
                  let _menhir_stack = (_menhir_stack, _menhir_s, _v);
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (('freshtv637, string), _menhir_state, 'tv_br),
                    _menhir_state,
                    'tv_top_block,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      assert(!_menhir_env._menhir_error);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | CONTAINS =>
                        let _menhir_env: _menhir_env = _menhir_env;
                        let _menhir_stack: (
                          (('freshtv627, string), _menhir_state, 'tv_br),
                          _menhir_state,
                          'tv_top_block,
                        ) =
                          Obj.magic(_menhir_stack);
                        (
                          {
                            let _menhir_env = _menhir_discard(_menhir_env);
                            let _tok = _menhir_env._menhir_token;
                            switch (_tok) {
                            | BR =>
                              _menhir_run3(
                                _menhir_env,
                                Obj.magic(_menhir_stack),
                                MenhirState122,
                              )
                            | _ =>
                              assert(!_menhir_env._menhir_error);
                              _menhir_env._menhir_error = true;
                              _menhir_errorcase(
                                _menhir_env,
                                Obj.magic(_menhir_stack),
                                MenhirState122,
                              );
                            };
                          }: 'freshtv628
                        );
                      | END =>
                        let _menhir_env: _menhir_env = _menhir_env;
                        let _menhir_stack: (
                          (('freshtv633, string), _menhir_state, 'tv_br),
                          _menhir_state,
                          'tv_top_block,
                        ) =
                          Obj.magic(_menhir_stack);
                        (
                          {
                            let _menhir_env = _menhir_discard(_menhir_env);
                            let _tok = _menhir_env._menhir_token;
                            switch (_tok) {
                            | PROGRAM =>
                              let _menhir_env: _menhir_env = _menhir_env;
                              let _menhir_stack: (
                                (
                                  ('freshtv629, string),
                                  _menhir_state,
                                  'tv_br,
                                ),
                                _menhir_state,
                                'tv_top_block,
                              ) =
                                Obj.magic(_menhir_stack);
                              (
                                {
                                  let _menhir_env =
                                    _menhir_discard(_menhir_env);
                                  let _tok = _menhir_env._menhir_token;
                                  switch (_tok) {
                                  | IDENT(_v) =>
                                    _menhir_run116(
                                      _menhir_env,
                                      Obj.magic(_menhir_stack),
                                      MenhirState115,
                                      _v,
                                    )
                                  | BR
                                  | EOF =>
                                    _menhir_reduce55(
                                      _menhir_env,
                                      Obj.magic(_menhir_stack),
                                      MenhirState115,
                                    )
                                  | _ =>
                                    assert(!_menhir_env._menhir_error);
                                    _menhir_env._menhir_error = true;
                                    _menhir_errorcase(
                                      _menhir_env,
                                      Obj.magic(_menhir_stack),
                                      MenhirState115,
                                    );
                                  };
                                }: 'freshtv630
                              );
                            | _ =>
                              assert(!_menhir_env._menhir_error);
                              _menhir_env._menhir_error = true;
                              let _menhir_env: _menhir_env = _menhir_env;
                              let _menhir_stack: (
                                (
                                  ('freshtv631, string),
                                  _menhir_state,
                                  'tv_br,
                                ),
                                _menhir_state,
                                'tv_top_block,
                              ) =
                                Obj.magic(_menhir_stack);
                              (
                                {
                                  let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                                  _menhir_errorcase(
                                    _menhir_env,
                                    Obj.magic(_menhir_stack),
                                    _menhir_s,
                                  );
                                }: 'freshtv632
                              );
                            };
                          }: 'freshtv634
                        );
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        let _menhir_env: _menhir_env = _menhir_env;
                        let _menhir_stack: (
                          (('freshtv635, string), _menhir_state, 'tv_br),
                          _menhir_state,
                          'tv_top_block,
                        ) =
                          Obj.magic(_menhir_stack);
                        (
                          {
                            let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                            _menhir_errorcase(
                              _menhir_env,
                              Obj.magic(_menhir_stack),
                              _menhir_s,
                            );
                          }: 'freshtv636
                        );
                      };
                    }: 'freshtv638
                  );
                }: 'freshtv640
              );
            }: 'freshtv642
          );
        }: 'freshtv644
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_decl) => 'ttv_return
)

and _menhir_reduce80: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_seq_adecl = ([]: 'tv_seq_adecl);

    _menhir_goto_seq_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run26: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | COLON =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv539, _menhir_state) =
        Obj.magic(_menhir_stack);
      let _menhir_s: _menhir_state = MenhirState26;
      (
        {
          let _menhir_stack = (_menhir_stack, _menhir_s);
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | FALSE =>
            _menhir_run25(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
            )
          | FLOAT(_v) =>
            _menhir_run24(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
              _v,
            )
          | IDENT(_v) =>
            _menhir_run22(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
              _v,
            )
          | INT(_v) =>
            _menhir_run21(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
              _v,
            )
          | LBRACE =>
            _menhir_run20(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
            )
          | LPAREN =>
            _menhir_run19(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
            )
          | LPAREN_S =>
            _menhir_run18(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
            )
          | MINUS =>
            _menhir_run17(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
            )
          | NOT =>
            _menhir_run16(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
            )
          | TRUE =>
            _menhir_run15(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState27,
            );
          };
        }: 'freshtv540
      );
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState26)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState26, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState26, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState26, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState26)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState26)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState26)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState26)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState26)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState26)
    | COMMA
    | RPAREN =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv541, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          let _1 = ();
          let _v: 'tv_adecl = (mkdim_param(~loc=mkloc(), Default): 'tv_adecl);

          _menhir_goto_adecl(_menhir_env, _menhir_stack, _menhir_s, _v);
        }: 'freshtv542
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState26,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run65: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState65)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState65, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState65, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState65, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState65)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState65)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState65)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState65)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState65)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState65)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState65,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_reduce91: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_seq_exp = ([]: 'tv_seq_exp);

    _menhir_goto_seq_exp(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run15: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv537 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    (
      {
        let _1 = ();
        let _v: 'tv_const = (mkconst(~loc=mkloc(), Cbool(true)): 'tv_const);

        _menhir_goto_const(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv538
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run16: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState16)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState16, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState16, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState16, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState16)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState16)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState16)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState16)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState16)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState16)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState16,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run17: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState17)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState17, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState17, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState17, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState17)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState17)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState17)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState17)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState17)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState17)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState17,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run18: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState18)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState18, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState18, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState18, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState18)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState18)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState18)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState18)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState18)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState18)
    | S_RPAREN =>
      _menhir_reduce91(_menhir_env, Obj.magic(_menhir_stack), MenhirState18)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState18,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run19: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState19)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState19, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState19, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState19, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState19)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState19)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState19)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState19)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState19)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState19)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState19,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run20: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState20)
    | FLOAT(_v) =>
      _menhir_run24(_menhir_env, Obj.magic(_menhir_stack), MenhirState20, _v)
    | IDENT(_v) =>
      _menhir_run22(_menhir_env, Obj.magic(_menhir_stack), MenhirState20, _v)
    | INT(_v) =>
      _menhir_run21(_menhir_env, Obj.magic(_menhir_stack), MenhirState20, _v)
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState20)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState20)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState20)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState20)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState20)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState20)
    | RBRACE =>
      _menhir_reduce91(_menhir_env, Obj.magic(_menhir_stack), MenhirState20)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState20,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run21: (_menhir_env, 'ttv_tail, _menhir_state, int) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv535 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    let (_1: int): int = _v;
    (
      {

        let _v: 'tv_const = (mkconst(~loc=mkloc(), Cint(_1)): 'tv_const);

        _menhir_goto_const(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv536
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, int) => 'ttv_return
)

and _menhir_run22:
  (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | LPAREN =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv531, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | COLCOL =>
            _menhir_run65(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | COLON =>
            _menhir_run26(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | FALSE =>
            _menhir_run25(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | FLOAT(_v) =>
            _menhir_run24(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
              _v,
            )
          | IDENT(_v) =>
            _menhir_run22(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
              _v,
            )
          | INT(_v) =>
            _menhir_run21(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
              _v,
            )
          | LBRACE =>
            _menhir_run20(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | LPAREN =>
            _menhir_run19(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | LPAREN_S =>
            _menhir_run18(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | MINUS =>
            _menhir_run17(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | NOT =>
            _menhir_run16(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | TRUE =>
            _menhir_run15(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | RPAREN =>
            _menhir_reduce80(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState23,
            );
          };
        }: 'freshtv532
      );
    | AND
    | BR
    | COLON
    | COMMA
    | DIV
    | EQEQ
    | EQV
    | GEQ
    | GREATER
    | LEQ
    | LESS
    | MINUS
    | MUL
    | NEQ
    | NEQV
    | OR
    | PLUS
    | RBRACE
    | RPAREN
    | S_RPAREN => _menhir_reduce102(_menhir_env, Obj.magic(_menhir_stack))
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv533, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv534
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return
)

and _menhir_run24:
  (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv529 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    let (_1: string): string = _v;
    (
      {

        let _v: 'tv_const = (mkconst(~loc=mkloc(), Creal(_1)): 'tv_const);

        _menhir_goto_const(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv530
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return
)

and _menhir_run25: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv527 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    (
      {
        let _1 = ();
        let _v: 'tv_const = (mkconst(~loc=mkloc(), Cbool(false)): 'tv_const);

        _menhir_goto_const(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv528
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_goto_seq_subprogram:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_subprogram) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState258 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv515, _menhir_state, 'tv_subprogram),
        _menhir_state,
        'tv_seq_subprogram,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv513, _menhir_state, 'tv_subprogram),
            _menhir_state,
            'tv_seq_subprogram,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_subprogram),
                _,
                _2: 'tv_seq_subprogram,
              ) = _menhir_stack;
              let _v: 'tv_seq_subprogram = ([_1, ..._2]: 'tv_seq_subprogram);

              _menhir_goto_seq_subprogram(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv514
          );
        }: 'freshtv516
      );
    | MenhirState123 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv525, string), _menhir_state, 'tv_br),
            _menhir_state,
            'tv_top_block,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_subprogram,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | END =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv521, string), _menhir_state, 'tv_br),
                  _menhir_state,
                  'tv_top_block,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_subprogram,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | PROGRAM =>
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (('freshtv517, string), _menhir_state, 'tv_br),
                        _menhir_state,
                        'tv_top_block,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_subprogram,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let _menhir_env = _menhir_discard(_menhir_env);
                      let _tok = _menhir_env._menhir_token;
                      switch (_tok) {
                      | IDENT(_v) =>
                        _menhir_run116(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState262,
                          _v,
                        )
                      | BR
                      | EOF =>
                        _menhir_reduce55(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState262,
                        )
                      | _ =>
                        assert(!_menhir_env._menhir_error);
                        _menhir_env._menhir_error = true;
                        _menhir_errorcase(
                          _menhir_env,
                          Obj.magic(_menhir_stack),
                          MenhirState262,
                        );
                      };
                    }: 'freshtv518
                  );
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  let _menhir_env: _menhir_env = _menhir_env;
                  let _menhir_stack: (
                    (
                      (
                        (('freshtv519, string), _menhir_state, 'tv_br),
                        _menhir_state,
                        'tv_top_block,
                      ),
                      _menhir_state,
                      'tv_br,
                    ),
                    _menhir_state,
                    'tv_seq_subprogram,
                  ) =
                    Obj.magic(_menhir_stack);
                  (
                    {
                      let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                      _menhir_errorcase(
                        _menhir_env,
                        Obj.magic(_menhir_stack),
                        _menhir_s,
                      );
                    }: 'freshtv520
                  );
                };
              }: 'freshtv522
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (
              (
                (
                  (('freshtv523, string), _menhir_state, 'tv_br),
                  _menhir_state,
                  'tv_top_block,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_subprogram,
            ) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s, _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv524
            );
          };
        }: 'freshtv526
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_subprogram) => 'ttv_return
)

and _menhir_reduce94: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_seq_ident = ([]: 'tv_seq_ident);

    _menhir_goto_seq_ident(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run127:
  (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | COMMA =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv507, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | IDENT(_v) =>
            _menhir_run127(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState128,
              _v,
            )
          | RPAREN =>
            _menhir_reduce94(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState128,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState128,
            );
          };
        }: 'freshtv508
      );
    | RPAREN =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv509, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _1: string) = _menhir_stack;
          let _v: 'tv_seq_ident = ([_1]: 'tv_seq_ident);

          _menhir_goto_seq_ident(_menhir_env, _menhir_stack, _menhir_s, _v);
        }: 'freshtv510
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv511, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv512
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return
)

and _menhir_goto_seq_var:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_var) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState5 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv501, _menhir_state, 'tv_seq_var) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | CONTAINS
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState265,
            );
          };
        }: 'freshtv502
      );
    | MenhirState267 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv505, _menhir_state, 'tv_decl_var),
        _menhir_state,
        'tv_seq_var,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv503, _menhir_state, 'tv_decl_var),
            _menhir_state,
            'tv_seq_var,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (_menhir_stack, _menhir_s, _1: 'tv_decl_var),
                _,
                _2: 'tv_seq_var,
              ) = _menhir_stack;
              let _v: 'tv_seq_var = (_1 @ _2: 'tv_seq_var);

              _menhir_goto_seq_var(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv504
          );
        }: 'freshtv506
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_seq_var) => 'ttv_return
)

and _menhir_goto_typ:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_typ) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: ('freshtv499, _menhir_state, 'tv_typ) =
      Obj.magic(_menhir_stack);
    (
      {
        assert(!_menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token;
        switch (_tok) {
        | COMMA =>
          _menhir_run94(_menhir_env, Obj.magic(_menhir_stack), MenhirState12)
        | IDENT(_v) =>
          _menhir_run13(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState12,
            _v,
          )
        | COLCOL =>
          _menhir_reduce75(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState12,
          )
        | _ =>
          assert(!_menhir_env._menhir_error);
          _menhir_env._menhir_error = true;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState12,
          );
        };
      }: 'freshtv500
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_typ) => 'ttv_return
)

and _menhir_fail: unit => 'a = (
  () => {
    Printf.fprintf(
      stderr,
      "Internal failure -- please contact the parser generator's developers.\n%!",
    );
    assert(false);
  }:
    unit => 'a
)

and _menhir_goto_subprogram:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_subprogram) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: ('freshtv497, _menhir_state, 'tv_subprogram) =
      Obj.magic(_menhir_stack);
    (
      {
        assert(!_menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token;
        switch (_tok) {
        | FUNCTION =>
          _menhir_run247(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState258,
          )
        | SUBROUTINE =>
          _menhir_run124(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState258,
          )
        | END =>
          _menhir_reduce97(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState258,
          )
        | _ =>
          assert(!_menhir_env._menhir_error);
          _menhir_env._menhir_error = true;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState258,
          );
        };
      }: 'freshtv498
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_subprogram) => 'ttv_return
)

and _menhir_goto_decl:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_decl) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState265
    | MenhirState252
    | MenhirState132
    | MenhirState233
    | MenhirState151
    | MenhirState161
    | MenhirState220
    | MenhirState210
    | MenhirState205
    | MenhirState196
    | MenhirState184 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv491, _menhir_state, 'tv_decl) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | CASE
          | CONTAINS
          | ELSE
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState196,
            );
          };
        }: 'freshtv492
      );
    | MenhirState155 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv495, _menhir_state, int),
        _menhir_state,
        'tv_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv493, _menhir_state, int),
            _menhir_state,
            'tv_decl,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let ((_menhir_stack, _menhir_s, _1: int), _, _2: 'tv_decl) = _menhir_stack;
              let _v: 'tv_decl = (
                mkdecl(~loc=mkloc(), [@implicit_arity] Label(_1, _2)): 'tv_decl
              );

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv494
          );
        }: 'freshtv496
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_decl) => 'ttv_return
)

and _menhir_reduce83: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_seq_case = ([]: 'tv_seq_case);

    _menhir_goto_seq_case(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run141: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | DEFAULT =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv485, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState232,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState232,
            );
          };
        }: 'freshtv486
      );
    | LPAREN =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv487, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | COLON =>
            _menhir_run144(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState142,
            )
          | FALSE =>
            _menhir_run25(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState142,
            )
          | FLOAT(_v) =>
            _menhir_run24(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState142,
              _v,
            )
          | IDENT(_v) =>
            _menhir_run143(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState142,
              _v,
            )
          | INT(_v) =>
            _menhir_run21(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState142,
              _v,
            )
          | TRUE =>
            _menhir_run15(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState142,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState142,
            );
          };
        }: 'freshtv488
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv489, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv490
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_goto_one_line_decl:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_one_line_decl) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) =>
    switch (_menhir_s) {
    | MenhirState265
    | MenhirState252
    | MenhirState132
    | MenhirState233
    | MenhirState151
    | MenhirState155
    | MenhirState161
    | MenhirState220
    | MenhirState210
    | MenhirState205
    | MenhirState196
    | MenhirState184 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: 'freshtv479 = Obj.magic(_menhir_stack);
      let _menhir_s: _menhir_state = _menhir_s;
      let _v: 'tv_one_line_decl = _v;
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: 'freshtv477 = Obj.magic(_menhir_stack);
          let _menhir_s: _menhir_state = _menhir_s;
          let (_1: 'tv_one_line_decl): 'tv_one_line_decl = _v;
          (
            {

              let _v: 'tv_decl = (_1: 'tv_decl);

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv478
          );
        }: 'freshtv480
      );
    | MenhirState159 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv483, _menhir_state), _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      let _menhir_s: _menhir_state = _menhir_s;
      let _v: 'tv_one_line_decl = _v;
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (('freshtv481, _menhir_state), _menhir_state, 'tv_exp),
            _menhir_state,
          ) =
            Obj.magic(_menhir_stack);
          let _: _menhir_state = _menhir_s;
          let (_5: 'tv_one_line_decl): 'tv_one_line_decl = _v;
          (
            {
              let (((_menhir_stack, _menhir_s), _, _3: 'tv_exp), _) = _menhir_stack;
              let _4 = ();
              let _2 = ();
              let _1 = ();
              let _v: 'tv_decl = (
                mkdecl(~loc=mkloc(), [@implicit_arity] If(_3, [_5], [])): 'tv_decl
              );

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv482
          );
        }: 'freshtv484
      );
    | _ => _menhir_fail()
    }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_one_line_decl) => 'ttv_return
)

and _menhir_reduce87: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_seq_decl = ([]: 'tv_seq_decl);

    _menhir_goto_seq_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run133: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | BR =>
      _menhir_run3(_menhir_env, Obj.magic(_menhir_stack), MenhirState133)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState133,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run135: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | CASE =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv473, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | LPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv469, _menhir_state) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState137,
                  );
                };
              }: 'freshtv470
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv471, _menhir_state) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv472
            );
          };
        }: 'freshtv474
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv475, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv476
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run152: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | FALSE =>
      _menhir_run25(_menhir_env, Obj.magic(_menhir_stack), MenhirState152)
    | FLOAT(_v) =>
      _menhir_run24(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState152,
        _v,
      )
    | IDENT(_v) =>
      _menhir_run22(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState152,
        _v,
      )
    | INT(_v) =>
      _menhir_run21(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState152,
        _v,
      )
    | LBRACE =>
      _menhir_run20(_menhir_env, Obj.magic(_menhir_stack), MenhirState152)
    | LPAREN =>
      _menhir_run19(_menhir_env, Obj.magic(_menhir_stack), MenhirState152)
    | LPAREN_S =>
      _menhir_run18(_menhir_env, Obj.magic(_menhir_stack), MenhirState152)
    | MINUS =>
      _menhir_run17(_menhir_env, Obj.magic(_menhir_stack), MenhirState152)
    | NOT =>
      _menhir_run16(_menhir_env, Obj.magic(_menhir_stack), MenhirState152)
    | TRUE =>
      _menhir_run15(_menhir_env, Obj.magic(_menhir_stack), MenhirState152)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState152,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run155: (_menhir_env, 'ttv_tail, _menhir_state, int) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | CALL =>
      _menhir_run185(_menhir_env, Obj.magic(_menhir_stack), MenhirState155)
    | DO =>
      _menhir_run179(_menhir_env, Obj.magic(_menhir_stack), MenhirState155)
    | GO =>
      _menhir_run175(_menhir_env, Obj.magic(_menhir_stack), MenhirState155)
    | GOTO =>
      _menhir_run172(_menhir_env, Obj.magic(_menhir_stack), MenhirState155)
    | IDENT(_v) =>
      _menhir_run162(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState155,
        _v,
      )
    | IF =>
      _menhir_run156(_menhir_env, Obj.magic(_menhir_stack), MenhirState155)
    | INT(_v) =>
      _menhir_run155(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState155,
        _v,
      )
    | RETURN =>
      _menhir_run152(_menhir_env, Obj.magic(_menhir_stack), MenhirState155)
    | SELECT =>
      _menhir_run135(_menhir_env, Obj.magic(_menhir_stack), MenhirState155)
    | STOP =>
      _menhir_run133(_menhir_env, Obj.magic(_menhir_stack), MenhirState155)
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(
        _menhir_env,
        Obj.magic(_menhir_stack),
        MenhirState155,
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, int) => 'ttv_return
)

and _menhir_run156: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | LPAREN =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv465, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | FALSE =>
            _menhir_run25(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
            )
          | FLOAT(_v) =>
            _menhir_run24(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
              _v,
            )
          | IDENT(_v) =>
            _menhir_run22(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
              _v,
            )
          | INT(_v) =>
            _menhir_run21(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
              _v,
            )
          | LBRACE =>
            _menhir_run20(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
            )
          | LPAREN =>
            _menhir_run19(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
            )
          | LPAREN_S =>
            _menhir_run18(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
            )
          | MINUS =>
            _menhir_run17(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
            )
          | NOT =>
            _menhir_run16(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
            )
          | TRUE =>
            _menhir_run15(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState157,
            );
          };
        }: 'freshtv466
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv467, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv468
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run162:
  (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | EQ =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv459, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | FALSE =>
            _menhir_run25(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
            )
          | FLOAT(_v) =>
            _menhir_run24(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
              _v,
            )
          | IDENT(_v) =>
            _menhir_run22(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
              _v,
            )
          | INT(_v) =>
            _menhir_run21(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
              _v,
            )
          | LBRACE =>
            _menhir_run20(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
            )
          | LPAREN =>
            _menhir_run19(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
            )
          | LPAREN_S =>
            _menhir_run18(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
            )
          | MINUS =>
            _menhir_run17(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
            )
          | NOT =>
            _menhir_run16(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
            )
          | TRUE =>
            _menhir_run15(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState169,
            );
          };
        }: 'freshtv460
      );
    | LPAREN =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv461, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | COLCOL =>
            _menhir_run65(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | COLON =>
            _menhir_run26(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | FALSE =>
            _menhir_run25(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | FLOAT(_v) =>
            _menhir_run24(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
              _v,
            )
          | IDENT(_v) =>
            _menhir_run22(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
              _v,
            )
          | INT(_v) =>
            _menhir_run21(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
              _v,
            )
          | LBRACE =>
            _menhir_run20(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | LPAREN =>
            _menhir_run19(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | LPAREN_S =>
            _menhir_run18(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | MINUS =>
            _menhir_run17(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | NOT =>
            _menhir_run16(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | TRUE =>
            _menhir_run15(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | RPAREN =>
            _menhir_reduce80(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState163,
            );
          };
        }: 'freshtv462
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv463, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv464
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, string) => 'ttv_return
)

and _menhir_run172: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | INT(_v) =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv455, _menhir_state) =
        Obj.magic(_menhir_stack);
      let _v: int = _v;
      (
        {
          let _menhir_stack = (_menhir_stack, _v);
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | BR =>
            _menhir_run3(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState173,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState173,
            );
          };
        }: 'freshtv456
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv457, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv458
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run175: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | TO =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv451, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | INT(_v) =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv447, _menhir_state) =
              Obj.magic(_menhir_stack);
            let _v: int = _v;
            (
              {
                let _menhir_stack = (_menhir_stack, _v);
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | BR =>
                  _menhir_run3(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState177,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState177,
                  );
                };
              }: 'freshtv448
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv449, _menhir_state) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv450
            );
          };
        }: 'freshtv452
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv453, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv454
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run179: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | IDENT(_v) =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv437, _menhir_state) =
        Obj.magic(_menhir_stack);
      let _v: string = _v;
      (
        {
          let _menhir_stack = (_menhir_stack, _v);
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | EQ =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (('freshtv433, _menhir_state), string) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState199,
                  );
                };
              }: 'freshtv434
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (('freshtv435, _menhir_state), string) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv436
            );
          };
        }: 'freshtv438
      );
    | WHILE =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv443, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | LPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv439, _menhir_state) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState181,
                  );
                };
              }: 'freshtv440
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: ('freshtv441, _menhir_state) =
              Obj.magic(_menhir_stack);
            (
              {
                let (_menhir_stack, _menhir_s) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv442
            );
          };
        }: 'freshtv444
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv445, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv446
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run185: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | IDENT(_v) =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv429, _menhir_state) =
        Obj.magic(_menhir_stack);
      let _v: string = _v;
      (
        {
          let _menhir_stack = (_menhir_stack, _v);
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | LPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (('freshtv425, _menhir_state), string) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | FALSE =>
                  _menhir_run25(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  )
                | FLOAT(_v) =>
                  _menhir_run24(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                    _v,
                  )
                | IDENT(_v) =>
                  _menhir_run22(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                    _v,
                  )
                | INT(_v) =>
                  _menhir_run21(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                    _v,
                  )
                | LBRACE =>
                  _menhir_run20(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  )
                | LPAREN =>
                  _menhir_run19(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  )
                | LPAREN_S =>
                  _menhir_run18(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  )
                | MINUS =>
                  _menhir_run17(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  )
                | NOT =>
                  _menhir_run16(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  )
                | TRUE =>
                  _menhir_run15(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  )
                | RPAREN =>
                  _menhir_reduce91(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState187,
                  );
                };
              }: 'freshtv426
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (('freshtv427, _menhir_state), string) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv428
            );
          };
        }: 'freshtv430
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv431, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv432
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_reduce97: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_seq_subprogram = ([]: 'tv_seq_subprogram);

    _menhir_goto_seq_subprogram(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run124: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | IDENT(_v) =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv421, _menhir_state) =
        Obj.magic(_menhir_stack);
      let _v: string = _v;
      (
        {
          let _menhir_stack = (_menhir_stack, _v);
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | LPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (('freshtv417, _menhir_state), string) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | IDENT(_v) =>
                  _menhir_run127(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState126,
                    _v,
                  )
                | RPAREN =>
                  _menhir_reduce94(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState126,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState126,
                  );
                };
              }: 'freshtv418
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (('freshtv419, _menhir_state), string) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv420
            );
          };
        }: 'freshtv422
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv423, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv424
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run247: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | IDENT(_v) =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv413, _menhir_state) =
        Obj.magic(_menhir_stack);
      let _v: string = _v;
      (
        {
          let _menhir_stack = (_menhir_stack, _v);
          let _menhir_env = _menhir_discard(_menhir_env);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | LPAREN =>
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (('freshtv409, _menhir_state), string) =
              Obj.magic(_menhir_stack);
            (
              {
                let _menhir_env = _menhir_discard(_menhir_env);
                let _tok = _menhir_env._menhir_token;
                switch (_tok) {
                | IDENT(_v) =>
                  _menhir_run127(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState249,
                    _v,
                  )
                | RPAREN =>
                  _menhir_reduce94(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState249,
                  )
                | _ =>
                  assert(!_menhir_env._menhir_error);
                  _menhir_env._menhir_error = true;
                  _menhir_errorcase(
                    _menhir_env,
                    Obj.magic(_menhir_stack),
                    MenhirState249,
                  );
                };
              }: 'freshtv410
            );
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            let _menhir_env: _menhir_env = _menhir_env;
            let _menhir_stack: (('freshtv411, _menhir_state), string) =
              Obj.magic(_menhir_stack);
            (
              {
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
                _menhir_errorcase(
                  _menhir_env,
                  Obj.magic(_menhir_stack),
                  _menhir_s,
                );
              }: 'freshtv412
            );
          };
        }: 'freshtv414
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv415, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv416
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_goto_decl_var:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_decl_var) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: ('freshtv407, _menhir_state, 'tv_decl_var) =
      Obj.magic(_menhir_stack);
    (
      {
        assert(!_menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token;
        switch (_tok) {
        | COMPLEX =>
          _menhir_run11(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState267,
          )
        | DOUBLE =>
          _menhir_run9(_menhir_env, Obj.magic(_menhir_stack), MenhirState267)
        | INTEGER =>
          _menhir_run8(_menhir_env, Obj.magic(_menhir_stack), MenhirState267)
        | LOGICAL =>
          _menhir_run7(_menhir_env, Obj.magic(_menhir_stack), MenhirState267)
        | REAL =>
          _menhir_run6(_menhir_env, Obj.magic(_menhir_stack), MenhirState267)
        | CALL
        | CONTAINS
        | DO
        | END
        | GO
        | GOTO
        | IDENT(_)
        | IF
        | INT(_)
        | RETURN
        | SELECT
        | STOP =>
          _menhir_reduce99(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState267,
          )
        | _ =>
          assert(!_menhir_env._menhir_error);
          _menhir_env._menhir_error = true;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            MenhirState267,
          );
        };
      }: 'freshtv408
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_decl_var) => 'ttv_return
)

and _menhir_reduce99: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {

    let _v: 'tv_seq_var = ([]: 'tv_seq_var);

    _menhir_goto_seq_var(_menhir_env, _menhir_stack, _menhir_s, _v);
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run6: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv405 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    (
      {
        let _1 = ();
        let _v: 'tv_typ = (mktyp(~loc=mkloc(), Treal): 'tv_typ);

        _menhir_goto_typ(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv406
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run7: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv403 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    (
      {
        let _1 = ();
        let _v: 'tv_typ = (mktyp(~loc=mkloc(), Tlogical): 'tv_typ);

        _menhir_goto_typ(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv404
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run8: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv401 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    (
      {
        let _1 = ();
        let _v: 'tv_typ = (mktyp(~loc=mkloc(), Tinteger): 'tv_typ);

        _menhir_goto_typ(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv402
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run9: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | PRECISION =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv397, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env = _menhir_discard(_menhir_env);
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: ('freshtv395, _menhir_state) =
            Obj.magic(_menhir_stack);
          (
            {
              let (_menhir_stack, _menhir_s) = _menhir_stack;
              let _2 = ();
              let _1 = ();
              let _v: 'tv_typ = (mktyp(~loc=mkloc(), Tdouble): 'tv_typ);

              _menhir_goto_typ(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv396
          );
        }: 'freshtv398
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv399, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv400
      );
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run11: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_env = _menhir_discard(_menhir_env);
    let _menhir_env: _menhir_env = _menhir_env;
    let _menhir_stack: 'freshtv393 = Obj.magic(_menhir_stack);
    let _menhir_s: _menhir_state = _menhir_s;
    (
      {
        let _1 = ();
        let _v: 'tv_typ = (mktyp(~loc=mkloc(), Tcomplex): 'tv_typ);

        _menhir_goto_typ(_menhir_env, _menhir_stack, _menhir_s, _v);
      }: 'freshtv394
    );
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_goto_br:
  (_menhir_env, 'ttv_tail, _menhir_state, 'tv_br) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s, _v) => {
    let _menhir_stack = (_menhir_stack, _menhir_s, _v);
    switch (_menhir_s) {
    | MenhirState3 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv299, _menhir_state),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv297, _menhir_state),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let ((_menhir_stack, _menhir_s), _, _2: 'tv_br) = _menhir_stack;
              let _1 = ();
              let _v: 'tv_br = ((): 'tv_br);

              _menhir_goto_br(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv298
          );
        }: 'freshtv300
      );
    | MenhirState2 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv301, string), _menhir_state, 'tv_br) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | COMPLEX =>
            _menhir_run11(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState5,
            )
          | DOUBLE =>
            _menhir_run9(_menhir_env, Obj.magic(_menhir_stack), MenhirState5)
          | INTEGER =>
            _menhir_run8(_menhir_env, Obj.magic(_menhir_stack), MenhirState5)
          | LOGICAL =>
            _menhir_run7(_menhir_env, Obj.magic(_menhir_stack), MenhirState5)
          | REAL =>
            _menhir_run6(_menhir_env, Obj.magic(_menhir_stack), MenhirState5)
          | CALL
          | CONTAINS
          | DO
          | END
          | GO
          | GOTO
          | IDENT(_)
          | IF
          | INT(_)
          | RETURN
          | SELECT
          | STOP =>
            _menhir_reduce99(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState5,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState5,
            );
          };
        }: 'freshtv302
      );
    | MenhirState104 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          ('freshtv305, _menhir_state, 'tv_typ),
          _menhir_state,
          'tv_seq_decl_assign,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              ('freshtv303, _menhir_state, 'tv_typ),
              _menhir_state,
              'tv_seq_decl_assign,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (_menhir_stack, _menhir_s, _1: 'tv_typ),
                  _,
                  _2: 'tv_seq_decl_assign,
                ),
                _,
                _3: 'tv_br,
              ) = _menhir_stack;
              let _v: 'tv_decl_var = (
                mkvar_decl(_1, _2, ~kind=[]): 'tv_decl_var
              );

              _menhir_goto_decl_var(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv304
          );
        }: 'freshtv306
      );
    | MenhirState108 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            ('freshtv309, _menhir_state, 'tv_typ),
            _menhir_state,
            'tv_opt_kind,
          ),
          _menhir_state,
          'tv_seq_decl_assign,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                ('freshtv307, _menhir_state, 'tv_typ),
                _menhir_state,
                'tv_opt_kind,
              ),
              _menhir_state,
              'tv_seq_decl_assign,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (_menhir_stack, _menhir_s, _1: 'tv_typ),
                    _,
                    _2: 'tv_opt_kind,
                  ),
                  _,
                  _4: 'tv_seq_decl_assign,
                ),
                _,
                _5: 'tv_br,
              ) = _menhir_stack;
              let _3 = ();
              let _v: 'tv_decl_var = (
                mkvar_decl(_1, _4, ~kind=_2): 'tv_decl_var
              );

              _menhir_goto_decl_var(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv308
          );
        }: 'freshtv310
      );
    | MenhirState122 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv311, string), _menhir_state, 'tv_br),
          _menhir_state,
          'tv_top_block,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | FUNCTION =>
            _menhir_run247(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState123,
            )
          | SUBROUTINE =>
            _menhir_run124(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState123,
            )
          | END =>
            _menhir_reduce97(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState123,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState123,
            );
          };
        }: 'freshtv312
      );
    | MenhirState131 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv313, _menhir_state), string),
          _menhir_state,
          'tv_seq_ident,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState132,
            );
          };
        }: 'freshtv314
      );
    | MenhirState133 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv317, _menhir_state),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            ('freshtv315, _menhir_state),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let ((_menhir_stack, _menhir_s), _, _2: 'tv_br) = _menhir_stack;
              let _1 = ();
              let _v: 'tv_one_line_decl = (
                mkdecl(~loc=mkloc(), Stop): 'tv_one_line_decl
              );

              _menhir_goto_one_line_decl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv316
          );
        }: 'freshtv318
      );
    | MenhirState139 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv319, _menhir_state), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CASE =>
            _menhir_run141(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState140,
            )
          | END =>
            _menhir_reduce83(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState140,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState140,
            );
          };
        }: 'freshtv320
      );
    | MenhirState150 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv321, _menhir_state), _menhir_state, 'tv_seq_case_opt),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | CASE
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState151,
            );
          };
        }: 'freshtv322
      );
    | MenhirState153 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv325, _menhir_state), _menhir_state, 'tv_exp),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (('freshtv323, _menhir_state), _menhir_state, 'tv_exp),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                ((_menhir_stack, _menhir_s), _, _2: 'tv_exp),
                _,
                _3: 'tv_br,
              ) = _menhir_stack;
              let _1 = ();
              let _v: 'tv_one_line_decl = (
                mkdecl(~loc=mkloc(), Return(_2)): 'tv_one_line_decl
              );

              _menhir_goto_one_line_decl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv324
          );
        }: 'freshtv326
      );
    | MenhirState160 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv327, _menhir_state), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | ELSE
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState161,
            );
          };
        }: 'freshtv328
      );
    | MenhirState167 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            ('freshtv331, _menhir_state, string),
            _menhir_state,
            'tv_seq_adecl,
          ),
          _menhir_state,
          'tv_exp,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                ('freshtv329, _menhir_state, string),
                _menhir_state,
                'tv_seq_adecl,
              ),
              _menhir_state,
              'tv_exp,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (_menhir_stack, _menhir_s, _1: string),
                    _,
                    _3: 'tv_seq_adecl,
                  ),
                  _,
                  _6: 'tv_exp,
                ),
                _,
                _7: 'tv_br,
              ) = _menhir_stack;
              let _5 = ();
              let _4 = ();
              let _2 = ();
              let _v: 'tv_one_line_decl = (
                mkdecl(~loc=mkloc(), [@implicit_arity] Assign_a(_1, _3, _6)): 'tv_one_line_decl
              );

              _menhir_goto_one_line_decl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv330
          );
        }: 'freshtv332
      );
    | MenhirState170 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv335, _menhir_state, string), _menhir_state, 'tv_exp),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (('freshtv333, _menhir_state, string), _menhir_state, 'tv_exp),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                ((_menhir_stack, _menhir_s, _1: string), _, _3: 'tv_exp),
                _,
                _4: 'tv_br,
              ) = _menhir_stack;
              let _2 = ();
              let _v: 'tv_one_line_decl = (
                mkdecl(~loc=mkloc(), [@implicit_arity] Assign(_1, _3)): 'tv_one_line_decl
              );

              _menhir_goto_one_line_decl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv334
          );
        }: 'freshtv336
      );
    | MenhirState173 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv339, _menhir_state), int),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (('freshtv337, _menhir_state), int),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (((_menhir_stack, _menhir_s), _2: int), _, _3: 'tv_br) = _menhir_stack;
              let _1 = ();
              let _v: 'tv_one_line_decl = (
                mkdecl(~loc=mkloc(), Goto(_2)): 'tv_one_line_decl
              );

              _menhir_goto_one_line_decl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv338
          );
        }: 'freshtv340
      );
    | MenhirState177 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv343, _menhir_state), int),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (('freshtv341, _menhir_state), int),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (((_menhir_stack, _menhir_s), _3: int), _, _4: 'tv_br) = _menhir_stack;
              let _2 = ();
              let _1 = ();
              let _v: 'tv_one_line_decl = (
                mkdecl(~loc=mkloc(), Goto(_3)): 'tv_one_line_decl
              );

              _menhir_goto_one_line_decl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv342
          );
        }: 'freshtv344
      );
    | MenhirState183 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv345, _menhir_state), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState184,
            );
          };
        }: 'freshtv346
      );
    | MenhirState189 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv349, _menhir_state), string),
          _menhir_state,
          'tv_seq_exp,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (('freshtv347, _menhir_state), string),
              _menhir_state,
              'tv_seq_exp,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  ((_menhir_stack, _menhir_s), _2: string),
                  _,
                  _4: 'tv_seq_exp,
                ),
                _,
                _6: 'tv_br,
              ) = _menhir_stack;
              let _5 = ();
              let _3 = ();
              let _1 = ();
              let _v: 'tv_one_line_decl = (
                mkdecl(~loc=mkloc(), [@implicit_arity] Call(_2, _4)): 'tv_one_line_decl
              );

              _menhir_goto_one_line_decl(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv348
          );
        }: 'freshtv350
      );
    | MenhirState193 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv353, _menhir_state), _menhir_state, 'tv_exp),
              _menhir_state,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (('freshtv351, _menhir_state), _menhir_state, 'tv_exp),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (((_menhir_stack, _menhir_s), _, _4: 'tv_exp), _),
                    _,
                    _6: 'tv_br,
                  ),
                  _,
                  _7: 'tv_seq_decl,
                ),
                _,
                _10: 'tv_br,
              ) = _menhir_stack;
              let _9 = ();
              let _8 = ();
              let _5 = ();
              let _3 = ();
              let _2 = ();
              let _1 = ();
              let _v: 'tv_decl = (
                mkdecl(~loc=mkloc(), [@implicit_arity] While(_4, _7)): 'tv_decl
              );

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv352
          );
        }: 'freshtv354
      );
    | MenhirState204 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (('freshtv355, _menhir_state), string),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
              ),
              _menhir_state,
              'tv_exp,
            ),
            _menhir_state,
          ),
          _menhir_state,
          'tv_exp,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState205,
            );
          };
        }: 'freshtv356
      );
    | MenhirState208 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (
                    (
                      (('freshtv359, _menhir_state), string),
                      _menhir_state,
                      'tv_exp,
                    ),
                    _menhir_state,
                  ),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
              ),
              _menhir_state,
              'tv_exp,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (
                    (
                      (
                        (
                          (('freshtv357, _menhir_state), string),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                      'tv_exp,
                    ),
                    _menhir_state,
                  ),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (
                      (
                        (
                          (
                            (
                              ((_menhir_stack, _menhir_s), _2: string),
                              _,
                              _4: 'tv_exp,
                            ),
                            _,
                          ),
                          _,
                          _6: 'tv_exp,
                        ),
                        _,
                      ),
                      _,
                      _8: 'tv_exp,
                    ),
                    _,
                    _9: 'tv_br,
                  ),
                  _,
                  _10: 'tv_seq_decl,
                ),
                _,
                _13: 'tv_br,
              ) = _menhir_stack;
              let _12 = ();
              let _11 = ();
              let _7 = ();
              let _5 = ();
              let _3 = ();
              let _1 = ();
              let _v: 'tv_decl = (
                mkdecl(
                  ~loc=mkloc(),
                  [@implicit_arity] Do(_2, _4, _6, Some(_8), _10),
                ): 'tv_decl
              );

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv358
          );
        }: 'freshtv360
      );
    | MenhirState202 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            ((('freshtv361, _menhir_state), string), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
          'tv_exp,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState210,
            );
          };
        }: 'freshtv362
      );
    | MenhirState213 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (('freshtv365, _menhir_state), string),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
              ),
              _menhir_state,
              'tv_exp,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (
                    (
                      (('freshtv363, _menhir_state), string),
                      _menhir_state,
                      'tv_exp,
                    ),
                    _menhir_state,
                  ),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (
                      (
                        (
                          ((_menhir_stack, _menhir_s), _2: string),
                          _,
                          _4: 'tv_exp,
                        ),
                        _,
                      ),
                      _,
                      _6: 'tv_exp,
                    ),
                    _,
                    _7: 'tv_br,
                  ),
                  _,
                  _8: 'tv_seq_decl,
                ),
                _,
                _11: 'tv_br,
              ) = _menhir_stack;
              let _10 = ();
              let _9 = ();
              let _5 = ();
              let _3 = ();
              let _1 = ();
              let _v: 'tv_decl = (
                mkdecl(
                  ~loc=mkloc(),
                  [@implicit_arity] Do(_2, _4, _6, None, _8),
                ): 'tv_decl
              );

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv364
          );
        }: 'freshtv366
      );
    | MenhirState217 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv369, _menhir_state), _menhir_state, 'tv_exp),
                _menhir_state,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (
                    (('freshtv367, _menhir_state), _menhir_state, 'tv_exp),
                    _menhir_state,
                  ),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    ((((_menhir_stack, _menhir_s), _, _3: 'tv_exp), _), _),
                    _,
                    _6: 'tv_br,
                  ),
                  _,
                  _7: 'tv_seq_decl,
                ),
                _,
                _10: 'tv_br,
              ) = _menhir_stack;
              let _9 = ();
              let _8 = ();
              let _5 = ();
              let _4 = ();
              let _2 = ();
              let _1 = ();
              let _v: 'tv_decl = (
                mkdecl(~loc=mkloc(), [@implicit_arity] If(_3, _7, [])): 'tv_decl
              );

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv368
          );
        }: 'freshtv370
      );
    | MenhirState219 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv371, _menhir_state), _menhir_state, 'tv_exp),
                _menhir_state,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState220,
            );
          };
        }: 'freshtv372
      );
    | MenhirState223 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (
                    (('freshtv375, _menhir_state), _menhir_state, 'tv_exp),
                    _menhir_state,
                  ),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (
                    (
                      (
                        (
                          ('freshtv373, _menhir_state),
                          _menhir_state,
                          'tv_exp,
                        ),
                        _menhir_state,
                      ),
                      _menhir_state,
                    ),
                    _menhir_state,
                    'tv_br,
                  ),
                  _menhir_state,
                  'tv_seq_decl,
                ),
                _menhir_state,
                'tv_br,
              ),
              _menhir_state,
              'tv_seq_decl,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (
                      (
                        (
                          (((_menhir_stack, _menhir_s), _, _3: 'tv_exp), _),
                          _,
                        ),
                        _,
                        _6: 'tv_br,
                      ),
                      _,
                      _7: 'tv_seq_decl,
                    ),
                    _,
                    _9: 'tv_br,
                  ),
                  _,
                  _10: 'tv_seq_decl,
                ),
                _,
                _13: 'tv_br,
              ) = _menhir_stack;
              let _12 = ();
              let _11 = ();
              let _8 = ();
              let _5 = ();
              let _4 = ();
              let _2 = ();
              let _1 = ();
              let _v: 'tv_decl = (
                mkdecl(~loc=mkloc(), [@implicit_arity] If(_3, _7, _10)): 'tv_decl
              );

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv374
          );
        }: 'freshtv376
      );
    | MenhirState232 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv377, _menhir_state),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | CASE
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState233,
            );
          };
        }: 'freshtv378
      );
    | MenhirState238 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv381, _menhir_state), _menhir_state, 'tv_exp),
                _menhir_state,
              ),
              _menhir_state,
              'tv_br,
            ),
            _menhir_state,
            'tv_seq_case,
          ),
          _menhir_state,
          'tv_ident_or_blank,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (
                    (('freshtv379, _menhir_state), _menhir_state, 'tv_exp),
                    _menhir_state,
                  ),
                  _menhir_state,
                  'tv_br,
                ),
                _menhir_state,
                'tv_seq_case,
              ),
              _menhir_state,
              'tv_ident_or_blank,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (
                      (((_menhir_stack, _menhir_s), _, _4: 'tv_exp), _),
                      _,
                      _6: 'tv_br,
                    ),
                    _,
                    _7: 'tv_seq_case,
                  ),
                  _,
                  _10: 'tv_ident_or_blank,
                ),
                _,
                _11: 'tv_br,
              ) = _menhir_stack;
              let _9 = ();
              let _8 = ();
              let _5 = ();
              let _3 = ();
              let _2 = ();
              let _1 = ();
              let _v: 'tv_decl = (
                mkdecl(~loc=mkloc(), Select(mkselect(~loc=mkloc(), _4, _7))): 'tv_decl
              );

              _menhir_goto_decl(_menhir_env, _menhir_stack, _menhir_s, _v);
            }: 'freshtv380
          );
        }: 'freshtv382
      );
    | MenhirState245 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv385, _menhir_state), string),
                _menhir_state,
                'tv_seq_ident,
              ),
              _menhir_state,
              'tv_br,
            ),
            _menhir_state,
            'tv_seq_decl,
          ),
          _menhir_state,
          'tv_ident_or_blank,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (
                    (('freshtv383, _menhir_state), string),
                    _menhir_state,
                    'tv_seq_ident,
                  ),
                  _menhir_state,
                  'tv_br,
                ),
                _menhir_state,
                'tv_seq_decl,
              ),
              _menhir_state,
              'tv_ident_or_blank,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (
                      (
                        ((_menhir_stack, _menhir_s), _2: string),
                        _,
                        _4: 'tv_seq_ident,
                      ),
                      _,
                      _6: 'tv_br,
                    ),
                    _,
                    _7: 'tv_seq_decl,
                  ),
                  _,
                  _10: 'tv_ident_or_blank,
                ),
                _,
                _11: 'tv_br,
              ) = _menhir_stack;
              let _9 = ();
              let _8 = ();
              let _5 = ();
              let _3 = ();
              let _1 = ();
              let _v: 'tv_subprogram = (
                mksub(
                  ~loc=mkloc(),
                  Subroutine(mksubroutine(~loc=mkloc(), _2, _4, _7)),
                ): 'tv_subprogram
              );

              _menhir_goto_subprogram(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv384
          );
        }: 'freshtv386
      );
    | MenhirState251 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv387, _menhir_state), string),
          _menhir_state,
          'tv_seq_ident,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          assert(!_menhir_env._menhir_error);
          let _tok = _menhir_env._menhir_token;
          switch (_tok) {
          | CALL =>
            _menhir_run185(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | DO =>
            _menhir_run179(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | GO =>
            _menhir_run175(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | GOTO =>
            _menhir_run172(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | IDENT(_v) =>
            _menhir_run162(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
              _v,
            )
          | IF =>
            _menhir_run156(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | INT(_v) =>
            _menhir_run155(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
              _v,
            )
          | RETURN =>
            _menhir_run152(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | SELECT =>
            _menhir_run135(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | STOP =>
            _menhir_run133(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | END =>
            _menhir_reduce87(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            )
          | _ =>
            assert(!_menhir_env._menhir_error);
            _menhir_env._menhir_error = true;
            _menhir_errorcase(
              _menhir_env,
              Obj.magic(_menhir_stack),
              MenhirState252,
            );
          };
        }: 'freshtv388
      );
    | MenhirState256 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv391, _menhir_state), string),
                _menhir_state,
                'tv_seq_ident,
              ),
              _menhir_state,
              'tv_br,
            ),
            _menhir_state,
            'tv_seq_decl,
          ),
          _menhir_state,
          'tv_ident_or_blank,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let _menhir_env: _menhir_env = _menhir_env;
          let _menhir_stack: (
            (
              (
                (
                  (
                    (('freshtv389, _menhir_state), string),
                    _menhir_state,
                    'tv_seq_ident,
                  ),
                  _menhir_state,
                  'tv_br,
                ),
                _menhir_state,
                'tv_seq_decl,
              ),
              _menhir_state,
              'tv_ident_or_blank,
            ),
            _menhir_state,
            'tv_br,
          ) =
            Obj.magic(_menhir_stack);
          (
            {
              let (
                (
                  (
                    (
                      (
                        ((_menhir_stack, _menhir_s), _2: string),
                        _,
                        _4: 'tv_seq_ident,
                      ),
                      _,
                      _6: 'tv_br,
                    ),
                    _,
                    _7: 'tv_seq_decl,
                  ),
                  _,
                  _10: 'tv_ident_or_blank,
                ),
                _,
                _11: 'tv_br,
              ) = _menhir_stack;
              let _9 = ();
              let _8 = ();
              let _5 = ();
              let _3 = ();
              let _1 = ();
              let _v: 'tv_subprogram = (
                mksub(
                  ~loc=mkloc(),
                  Function(mkfunc(~loc=mkloc(), _2, _4, _7)),
                ): 'tv_subprogram
              );

              _menhir_goto_subprogram(
                _menhir_env,
                _menhir_stack,
                _menhir_s,
                _v,
              );
            }: 'freshtv390
          );
        }: 'freshtv392
      );
    | _ => _menhir_fail()
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state, 'tv_br) => 'ttv_return
)

and _menhir_errorcase: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) =>
    switch (_menhir_s) {
    | MenhirState267 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv13, _menhir_state, 'tv_decl_var) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv14
      );
    | MenhirState265 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv15, _menhir_state, 'tv_seq_var) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv16
      );
    | MenhirState263 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv17, string), _menhir_state, 'tv_br),
              _menhir_state,
              'tv_top_block,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_subprogram,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv18
      );
    | MenhirState262 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv19, string), _menhir_state, 'tv_br),
            _menhir_state,
            'tv_top_block,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_subprogram,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv20
      );
    | MenhirState258 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv21, _menhir_state, 'tv_subprogram) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv22
      );
    | MenhirState256 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv23, _menhir_state), string),
              _menhir_state,
              'tv_seq_ident,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv24
      );
    | MenhirState255 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv25, _menhir_state), string),
            _menhir_state,
            'tv_seq_ident,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv26
      );
    | MenhirState252 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv27, _menhir_state), string),
          _menhir_state,
          'tv_seq_ident,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv28
      );
    | MenhirState251 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv29, _menhir_state), string),
        _menhir_state,
        'tv_seq_ident,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv30
      );
    | MenhirState249 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv31, _menhir_state), string) =
        Obj.magic(_menhir_stack);
      (
        {
          let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv32
      );
    | MenhirState245 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv33, _menhir_state), string),
              _menhir_state,
              'tv_seq_ident,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv34
      );
    | MenhirState244 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv35, _menhir_state), string),
            _menhir_state,
            'tv_seq_ident,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv36
      );
    | MenhirState240 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv37, _menhir_state, 'tv_case) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv38
      );
    | MenhirState238 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv39, _menhir_state), _menhir_state, 'tv_exp),
              _menhir_state,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_case,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv40
      );
    | MenhirState237 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv41, _menhir_state), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_case,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv42
      );
    | MenhirState233 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv43, _menhir_state), _menhir_state, 'tv_br) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv44
      );
    | MenhirState232 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv45, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv46
      );
    | MenhirState230 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv47, _menhir_state, 'tv_case_opt) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv48
      );
    | MenhirState223 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (('freshtv49, _menhir_state), _menhir_state, 'tv_exp),
                  _menhir_state,
                ),
                _menhir_state,
              ),
              _menhir_state,
              'tv_br,
            ),
            _menhir_state,
            'tv_seq_decl,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv50
      );
    | MenhirState220 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv51, _menhir_state), _menhir_state, 'tv_exp),
                _menhir_state,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_br,
          ),
          _menhir_state,
          'tv_seq_decl,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv52
      );
    | MenhirState219 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv53, _menhir_state), _menhir_state, 'tv_exp),
              _menhir_state,
            ),
            _menhir_state,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv54
      );
    | MenhirState217 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (('freshtv55, _menhir_state), _menhir_state, 'tv_exp),
              _menhir_state,
            ),
            _menhir_state,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv56
      );
    | MenhirState213 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv57, _menhir_state), string),
                _menhir_state,
                'tv_exp,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_exp,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv58
      );
    | MenhirState210 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            ((('freshtv59, _menhir_state), string), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
          'tv_exp,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv60
      );
    | MenhirState208 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (
                    (('freshtv61, _menhir_state), string),
                    _menhir_state,
                    'tv_exp,
                  ),
                  _menhir_state,
                ),
                _menhir_state,
                'tv_exp,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_exp,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv62
      );
    | MenhirState205 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (
                  (('freshtv63, _menhir_state), string),
                  _menhir_state,
                  'tv_exp,
                ),
                _menhir_state,
              ),
              _menhir_state,
              'tv_exp,
            ),
            _menhir_state,
          ),
          _menhir_state,
          'tv_exp,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv64
      );
    | MenhirState204 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (
              (
                (('freshtv65, _menhir_state), string),
                _menhir_state,
                'tv_exp,
              ),
              _menhir_state,
            ),
            _menhir_state,
            'tv_exp,
          ),
          _menhir_state,
        ),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv66
      );
    | MenhirState203 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            ((('freshtv67, _menhir_state), string), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
          'tv_exp,
        ),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv68
      );
    | MenhirState202 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          ((('freshtv69, _menhir_state), string), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv70
      );
    | MenhirState201 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ((('freshtv71, _menhir_state), string), _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv72
      );
    | MenhirState200 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv73, _menhir_state), string),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv74
      );
    | MenhirState199 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv75, _menhir_state), string) =
        Obj.magic(_menhir_stack);
      (
        {
          let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv76
      );
    | MenhirState196 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv77, _menhir_state, 'tv_decl) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv78
      );
    | MenhirState193 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv79, _menhir_state), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
          'tv_br,
        ),
        _menhir_state,
        'tv_seq_decl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv80
      );
    | MenhirState189 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv81, _menhir_state), string),
        _menhir_state,
        'tv_seq_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv82
      );
    | MenhirState187 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv83, _menhir_state), string) =
        Obj.magic(_menhir_stack);
      (
        {
          let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv84
      );
    | MenhirState184 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv85, _menhir_state), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv86
      );
    | MenhirState183 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv87, _menhir_state), _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv88
      );
    | MenhirState182 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv89, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv90
      );
    | MenhirState181 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv91, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv92
      );
    | MenhirState177 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv93, _menhir_state), int) =
        Obj.magic(_menhir_stack);
      (
        {
          let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv94
      );
    | MenhirState173 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv95, _menhir_state), int) =
        Obj.magic(_menhir_stack);
      (
        {
          let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv96
      );
    | MenhirState170 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv97, _menhir_state, string),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv98
      );
    | MenhirState169 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv99, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv100
      );
    | MenhirState167 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv101, _menhir_state, string), _menhir_state, 'tv_seq_adecl),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv102
      );
    | MenhirState166 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv103, _menhir_state, string),
        _menhir_state,
        'tv_seq_adecl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv104
      );
    | MenhirState163 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv105, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv106
      );
    | MenhirState161 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv107, _menhir_state), _menhir_state, 'tv_exp),
            _menhir_state,
          ),
          _menhir_state,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv108
      );
    | MenhirState160 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv109, _menhir_state), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv110
      );
    | MenhirState159 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv111, _menhir_state), _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv112
      );
    | MenhirState158 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv113, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv114
      );
    | MenhirState157 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv115, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv116
      );
    | MenhirState155 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv117, _menhir_state, int) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv118
      );
    | MenhirState153 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv119, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv120
      );
    | MenhirState152 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv121, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv122
      );
    | MenhirState151 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv123, _menhir_state), _menhir_state, 'tv_seq_case_opt),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv124
      );
    | MenhirState150 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv125, _menhir_state),
        _menhir_state,
        'tv_seq_case_opt,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv126
      );
    | MenhirState147 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv127, _menhir_state, 'tv_simple_exp) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv128
      );
    | MenhirState144 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv129, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv130
      );
    | MenhirState142 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv131, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv132
      );
    | MenhirState140 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv133, _menhir_state), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv134
      );
    | MenhirState139 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv135, _menhir_state), _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv136
      );
    | MenhirState138 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv137, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv138
      );
    | MenhirState137 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv139, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv140
      );
    | MenhirState133 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv141, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv142
      );
    | MenhirState132 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv143, _menhir_state), string),
          _menhir_state,
          'tv_seq_ident,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv144
      );
    | MenhirState131 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv145, _menhir_state), string),
        _menhir_state,
        'tv_seq_ident,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv146
      );
    | MenhirState128 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv147, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv148
      );
    | MenhirState126 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv149, _menhir_state), string) =
        Obj.magic(_menhir_stack);
      (
        {
          let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv150
      );
    | MenhirState123 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv151, string), _menhir_state, 'tv_br),
          _menhir_state,
          'tv_top_block,
        ),
        _menhir_state,
        'tv_br,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv152
      );
    | MenhirState122 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv153, string), _menhir_state, 'tv_br),
        _menhir_state,
        'tv_top_block,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv154
      );
    | MenhirState119 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv155, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv156
      );
    | MenhirState117 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv157, string), _menhir_state, 'tv_br),
          _menhir_state,
          'tv_top_block,
        ),
        _menhir_state,
        'tv_ident_or_blank,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv158
      );
    | MenhirState115 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv159, string), _menhir_state, 'tv_br),
        _menhir_state,
        'tv_top_block,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv160
      );
    | MenhirState111 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv161, _menhir_state, 'tv_decl_assign) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv162
      );
    | MenhirState108 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv163, _menhir_state, 'tv_typ), _menhir_state, 'tv_opt_kind),
        _menhir_state,
        'tv_seq_decl_assign,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv164
      );
    | MenhirState107 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv165, _menhir_state, 'tv_typ),
        _menhir_state,
        'tv_opt_kind,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv166
      );
    | MenhirState104 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv167, _menhir_state, 'tv_typ),
        _menhir_state,
        'tv_seq_decl_assign,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv168
      );
    | MenhirState102 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv169, _menhir_state), 'tv_kind) =
        Obj.magic(_menhir_stack);
      (
        {
          let ((_menhir_stack, _menhir_s), _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv170
      );
    | MenhirState98 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: 'freshtv171 = Obj.magic(_menhir_stack);
      (raise(_eRR): 'freshtv172);
    | MenhirState93 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv173, _menhir_state, string),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv174
      );
    | MenhirState92 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv175, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv176
      );
    | MenhirState91 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv177, _menhir_state, string), _menhir_state, 'tv_seq_adecl),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv178
      );
    | MenhirState90 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv179, _menhir_state, string),
        _menhir_state,
        'tv_seq_adecl,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv180
      );
    | MenhirState87 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv181, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv182
      );
    | MenhirState86 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv183, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv184
      );
    | MenhirState82 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv185, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv186
      );
    | MenhirState80 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv187, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv188
      );
    | MenhirState79 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv189, _menhir_state, 'tv_exp) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv190
      );
    | MenhirState75 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv191, _menhir_state, 'tv_adecl) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv192
      );
    | MenhirState73 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (
            (('freshtv193, _menhir_state, 'tv_exp), _menhir_state),
            _menhir_state,
            'tv_exp,
          ),
          _menhir_state,
        ),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv194
      );
    | MenhirState72 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv195, _menhir_state, 'tv_exp), _menhir_state),
          _menhir_state,
          'tv_exp,
        ),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv196
      );
    | MenhirState71 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv197, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv198
      );
    | MenhirState70 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv199, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv200
      );
    | MenhirState69 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv201, _menhir_state, 'tv_exp) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv202
      );
    | MenhirState66 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv203, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv204
      );
    | MenhirState65 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv205, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv206
      );
    | MenhirState64 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (
          (('freshtv207, _menhir_state), _menhir_state, 'tv_exp),
          _menhir_state,
        ),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv208
      );
    | MenhirState63 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv209, _menhir_state), _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv210
      );
    | MenhirState62 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv211, _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv212
      );
    | MenhirState61 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv213, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv214
      );
    | MenhirState60 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv215, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv216
      );
    | MenhirState59 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv217, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv218
      );
    | MenhirState58 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv219, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv220
      );
    | MenhirState57 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv221, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv222
      );
    | MenhirState56 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv223, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv224
      );
    | MenhirState55 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv225, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv226
      );
    | MenhirState54 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv227, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv228
      );
    | MenhirState53 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv229, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv230
      );
    | MenhirState52 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv231, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv232
      );
    | MenhirState51 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv233, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv234
      );
    | MenhirState50 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv235, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv236
      );
    | MenhirState46 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv237, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv238
      );
    | MenhirState45 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv239, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv240
      );
    | MenhirState44 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv241, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv242
      );
    | MenhirState43 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv243, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv244
      );
    | MenhirState42 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv245, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv246
      );
    | MenhirState41 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv247, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv248
      );
    | MenhirState40 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv249, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv250
      );
    | MenhirState39 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv251, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv252
      );
    | MenhirState38 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv253, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv254
      );
    | MenhirState37 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv255, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv256
      );
    | MenhirState36 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv257, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv258
      );
    | MenhirState35 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv259, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv260
      );
    | MenhirState34 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv261, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv262
      );
    | MenhirState33 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv263, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv264
      );
    | MenhirState32 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv265, _menhir_state, 'tv_exp), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv266
      );
    | MenhirState31 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        ('freshtv267, _menhir_state, 'tv_exp),
        _menhir_state,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv268
      );
    | MenhirState30 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (
        (('freshtv269, _menhir_state), _menhir_state),
        _menhir_state,
        'tv_exp,
      ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv270
      );
    | MenhirState27 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv271, _menhir_state), _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv272
      );
    | MenhirState26 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv273, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv274
      );
    | MenhirState23 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv275, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv276
      );
    | MenhirState20 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv277, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv278
      );
    | MenhirState19 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv279, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv280
      );
    | MenhirState18 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv281, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv282
      );
    | MenhirState17 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv283, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv284
      );
    | MenhirState16 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv285, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv286
      );
    | MenhirState14 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv287, _menhir_state, string) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv288
      );
    | MenhirState12 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv289, _menhir_state, 'tv_typ) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv290
      );
    | MenhirState5 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: (('freshtv291, string), _menhir_state, 'tv_br) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s, _) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv292
      );
    | MenhirState3 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv293, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          _menhir_errorcase(
            _menhir_env,
            Obj.magic(_menhir_stack),
            _menhir_s,
          );
        }: 'freshtv294
      );
    | MenhirState2 =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv295, string) = Obj.magic(_menhir_stack);
      (raise(_eRR): 'freshtv296);
    }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_run3: (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return = (
  (_menhir_env, _menhir_stack, _menhir_s) => {
    let _menhir_stack = (_menhir_stack, _menhir_s);
    let _menhir_env = _menhir_discard(_menhir_env);
    let _tok = _menhir_env._menhir_token;
    switch (_tok) {
    | BR => _menhir_run3(_menhir_env, Obj.magic(_menhir_stack), MenhirState3)
    | CALL
    | CASE
    | COMPLEX
    | CONTAINS
    | DO
    | DOUBLE
    | ELSE
    | END
    | FUNCTION
    | GO
    | GOTO
    | IDENT(_)
    | IF
    | INT(_)
    | INTEGER
    | LOGICAL
    | REAL
    | RETURN
    | SELECT
    | STOP
    | SUBROUTINE =>
      let _menhir_env: _menhir_env = _menhir_env;
      let _menhir_stack: ('freshtv11, _menhir_state) =
        Obj.magic(_menhir_stack);
      (
        {
          let (_menhir_stack, _menhir_s) = _menhir_stack;
          let _1 = ();
          let _v: 'tv_br = ((): 'tv_br);

          _menhir_goto_br(_menhir_env, _menhir_stack, _menhir_s, _v);
        }: 'freshtv12
      );
    | _ =>
      assert(!_menhir_env._menhir_error);
      _menhir_env._menhir_error = true;
      _menhir_errorcase(_menhir_env, Obj.magic(_menhir_stack), MenhirState3);
    };
  }:
    (_menhir_env, 'ttv_tail, _menhir_state) => 'ttv_return
)

and _menhir_discard: _menhir_env => _menhir_env = (
  _menhir_env => {
    let lexer = _menhir_env._menhir_lexer;
    let lexbuf = _menhir_env._menhir_lexbuf;
    let _tok = lexer(lexbuf);
    {
      _menhir_lexer: lexer,
      _menhir_lexbuf: lexbuf,
      _menhir_token: _tok,
      _menhir_error: false,
    };
  }:
    _menhir_env => _menhir_env
)

and main: (Lexing.lexbuf => token, Lexing.lexbuf) => Parse_tree.main(unit) = (
  (lexer, lexbuf) => {
    let _menhir_env = {
      let lexer: Lexing.lexbuf => token = lexer;
      let lexbuf: Lexing.lexbuf = lexbuf;
      (
        {
          let _tok = Obj.magic();
          {
            _menhir_lexer: lexer,
            _menhir_lexbuf: lexbuf,
            _menhir_token: _tok,
            _menhir_error: false,
          };
        }: _menhir_env
      );
    };

    Obj.magic(
      {
        let _menhir_env: _menhir_env = _menhir_env;
        let _menhir_stack: 'freshtv9 = (
          (),
          _menhir_env._menhir_lexbuf.Lexing.lex_curr_p,
        );
        (
          {
            let _menhir_env = _menhir_discard(_menhir_env);
            let _tok = _menhir_env._menhir_token;
            switch (_tok) {
            | PROGRAM =>
              let _menhir_env: _menhir_env = _menhir_env;
              let _menhir_stack: 'freshtv5 = Obj.magic(_menhir_stack);
              (
                {
                  let _menhir_env = _menhir_discard(_menhir_env);
                  let _tok = _menhir_env._menhir_token;
                  switch (_tok) {
                  | IDENT(_v) =>
                    let _menhir_env: _menhir_env = _menhir_env;
                    let _menhir_stack: 'freshtv1 = Obj.magic(_menhir_stack);
                    let _v: string = _v;
                    (
                      {
                        let _menhir_stack = (_menhir_stack, _v);
                        let _menhir_env = _menhir_discard(_menhir_env);
                        let _tok = _menhir_env._menhir_token;
                        switch (_tok) {
                        | BR =>
                          _menhir_run3(
                            _menhir_env,
                            Obj.magic(_menhir_stack),
                            MenhirState2,
                          )
                        | _ =>
                          assert(!_menhir_env._menhir_error);
                          _menhir_env._menhir_error = true;
                          _menhir_errorcase(
                            _menhir_env,
                            Obj.magic(_menhir_stack),
                            MenhirState2,
                          );
                        };
                      }: 'freshtv2
                    );
                  | _ =>
                    assert(!_menhir_env._menhir_error);
                    _menhir_env._menhir_error = true;
                    let _menhir_env: _menhir_env = _menhir_env;
                    let _menhir_stack: 'freshtv3 = Obj.magic(_menhir_stack);
                    (raise(_eRR): 'freshtv4);
                  };
                }: 'freshtv6
              );
            | _ =>
              assert(!_menhir_env._menhir_error);
              _menhir_env._menhir_error = true;
              let _menhir_env: _menhir_env = _menhir_env;
              let _menhir_stack: 'freshtv7 = Obj.magic(_menhir_stack);
              (raise(_eRR): 'freshtv8);
            };
          }: 'freshtv10
        );
      },
    );
  }:
    (Lexing.lexbuf => token, Lexing.lexbuf) => Parse_tree.main(unit)
);
