Program {
    structs: [],
    functions: [
        Function {
            span: Span {
                start: Position {
                    line: 1,
                    column: 1,
                    absolute: 0
                },
                end: Position {
                    line: 6,
                    column: 1,
                    absolute: 58
                }
            },
            name: Symbol(
                0
            ),
            params: [],
            returns: Nil,
            body: Expr(
                TypedExpression {
                    expr: Assign(
                        Symbol(
                            1
                        ),
                        TypedExpression {
                            expr: Literal(
                                Number(
                                    Number {
                                        value: 10,
                                        ty: None
                                    }
                                )
                            ),
                            ty: Var(
                                TypeVar(
                                    1
                                )
                            )
                        }
                    ),
                    ty: Var(
                        TypeVar(
                            1
                        )
                    )
                }
            ),
            linkage: Normal
        }
    ]
}