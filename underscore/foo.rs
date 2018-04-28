../Program {
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
                    line: 5,
                    column: 1,
                    absolute: 37
                }
            },
            name: Symbol(
                0
            ),
            generic: true,
            params: [],
            returns: Nil,
            body: Block(
                [
                    While(
                        TypedExpression {
                            expr: Binary(
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
                                            0
                                        )
                                    )
                                },
                                LT,
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
                            ty: App(
                                Bool,
                                []
                            )
                        },
                        Expr(
                            TypedExpression {
                                expr: Literal(
                                    Nil
                                ),
                                ty: Nil
                            }
                        )
                    )
                ]
            ),
            linkage: Normal
        }
    ]
}.