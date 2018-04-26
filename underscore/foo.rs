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
                    line: 3,
                    column: 1,
                    absolute: 30
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
                    Expr(
                        TypedExpression {
                            expr: Binary(
                                TypedExpression {
                                    expr: Literal(
                                        True(
                                            true
                                        )
                                    ),
                                    ty: App(
                                        Bool,
                                        []
                                    )
                                },
                                Or,
                                TypedExpression {
                                    expr: Literal(
                                        True(
                                            true
                                        )
                                    ),
                                    ty: App(
                                        Bool,
                                        []
                                    )
                                }
                            ),
                            ty: App(
                                Bool,
                                []
                            )
                        }
                    )
                ]
            ),
            linkage: Normal
        }
    ]
}.