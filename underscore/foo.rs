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
                    absolute: 21
                }
            },
            name: Symbol(
                0
            ),
            generic: true,
            params: [
                FunctionParam {
                    name: Symbol(
                        1
                    ),
                    ty: Array(
                        App(
                            Int(
                                Unsigned,
                                Bit32
                            ),
                            []
                        ),
                        3
                    )
                }
            ],
            returns: Nil,
            body: Expr(
                TypedExpression {
                    expr: Literal(
                        Nil
                    ),
                    ty: Nil
                }
            ),
            linkage: Normal
        },
        Function {
            span: Span {
                start: Position {
                    line: 5,
                    column: 1,
                    absolute: 24
                },
                end: Position {
                    line: 8,
                    column: 1,
                    absolute: 64
                }
            },
            name: Symbol(
                2
            ),
            generic: true,
            params: [],
            returns: Nil,
            body: Block(
                [
                    Let {
                        ident: Symbol(
                            1
                        ),
                        ty: Array(
                            Var(
                                TypeVar(
                                    0
                                )
                            ),
                            3
                        ),
                        expr: Some(
                            TypedExpression {
                                expr: Array(
                                    [
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
                                                    0
                                                )
                                            )
                                        },
                                        TypedExpression {
                                            expr: Literal(
                                                Number(
                                                    Number {
                                                        value: 2,
                                                        ty: None
                                                    }
                                                )
                                            ),
                                            ty: Var(
                                                TypeVar(
                                                    1
                                                )
                                            )
                                        },
                                        TypedExpression {
                                            expr: Literal(
                                                Number(
                                                    Number {
                                                        value: 3,
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
                                    ]
                                ),
                                ty: Array(
                                    Var(
                                        TypeVar(
                                            0
                                        )
                                    ),
                                    3
                                )
                            }
                        )
                    },
                    Expr(
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
                        }
                    )
                ]
            ),
            linkage: Normal
        }
    ]
}.