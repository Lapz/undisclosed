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
                    absolute: 68
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
                    Block(
                        [
                            Let {
                                ident: Symbol(
                                    1
                                ),
                                ty: Var(
                                    TypeVar(
                                        0
                                    )
                                ),
                                expr: Some(
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
                                )
                            },
                            While(
                                TypedExpression {
                                    expr: Binary(
                                        TypedExpression {
                                            expr: Var(
                                                Symbol(
                                                    1
                                                ),
                                                Var(
                                                    TypeVar(
                                                        0
                                                    )
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
                                                    3
                                                )
                                            )
                                        }
                                    ),
                                    ty: App(
                                        Bool,
                                        []
                                    )
                                },
                                Block(
                                    [
                                        Block(
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
                                                                1
                                                            )
                                                        )
                                                    }
                                                )
                                            ]
                                        ),
                                        Expr(
                                            TypedExpression {
                                                expr: Assign(
                                                    Symbol(
                                                        1
                                                    ),
                                                    TypedExpression {
                                                        expr: Binary(
                                                            TypedExpression {
                                                                expr: Var(
                                                                    Symbol(
                                                                        1
                                                                    ),
                                                                    Var(
                                                                        TypeVar(
                                                                            0
                                                                        )
                                                                    )
                                                                ),
                                                                ty: Var(
                                                                    TypeVar(
                                                                        0
                                                                    )
                                                                )
                                                            },
                                                            Plus,
                                                            TypedExpression {
                                                                expr: Literal(
                                                                    Number(
                                                                        Number {
                                                                            value: 1,
                                                                            ty: None
                                                                        }
                                                                    )
                                                                ),
                                                                ty: Var(
                                                                    TypeVar(
                                                                        2
                                                                    )
                                                                )
                                                            }
                                                        ),
                                                        ty: Var(
                                                            TypeVar(
                                                                0
                                                            )
                                                        )
                                                    }
                                                ),
                                                ty: Var(
                                                    TypeVar(
                                                        0
                                                    )
                                                )
                                            }
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                ]
            ),
            linkage: Normal
        }
    ]
}.