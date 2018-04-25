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
                    line: 7,
                    column: 1,
                    absolute: 59
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
                        }
                    ),
                    Expr(
                        TypedExpression {
                            expr: Binary(
                                TypedExpression {
                                    expr: Literal(
                                        Number(
                                            Number {
                                                value: 10,
                                                ty: Some(
                                                    (
                                                        Unsigned,
                                                        Bit8
                                                    )
                                                )
                                            }
                                        )
                                    ),
                                    ty: App(
                                        Int(
                                            Unsigned,
                                            Bit8
                                        ),
                                        []
                                    )
                                },
                                Plus,
                                TypedExpression {
                                    expr: Literal(
                                        Number(
                                            Number {
                                                value: 10,
                                                ty: Some(
                                                    (
                                                        Unsigned,
                                                        Bit8
                                                    )
                                                )
                                            }
                                        )
                                    ),
                                    ty: App(
                                        Int(
                                            Unsigned,
                                            Bit8
                                        ),
                                        []
                                    )
                                }
                            ),
                            ty: App(
                                Int(
                                    Unsigned,
                                    Bit8
                                ),
                                []
                            )
                        }
                    ),
                    Expr(
                        TypedExpression {
                            expr: Unary(
                                Bang,
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
                    ),
                    Expr(
                        TypedExpression {
                            expr: Unary(
                                Bang,
                                TypedExpression {
                                    expr: Literal(
                                        False(
                                            false
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