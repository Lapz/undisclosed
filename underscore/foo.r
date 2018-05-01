../Program {
    structs: [
        Struct {
            name: Symbol(
                0
            ),
            type_params: [],
            fields: [
                Field {
                    name: Symbol(
                        1
                    ),
                    ty: App(
                        Int(
                            Signed,
                            Bit32
                        ),
                        []
                    )
                }
            ]
        }
    ],
    functions: [
        Function {
            span: Span {
                start: Position {
                    line: 5,
                    column: 1,
                    absolute: 29
                },
                end: Position {
                    line: 7,
                    column: 1,
                    absolute: 50
                }
            },
            name: Symbol(
                2
            ),
            generic: true,
            params: [
                FunctionParam {
                    name: Symbol(
                        3
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
                    line: 9,
                    column: 1,
                    absolute: 53
                },
                end: Position {
                    line: 13,
                    column: 1,
                    absolute: 109
                }
            },
            name: Symbol(
                4
            ),
            generic: true,
            params: [],
            returns: Nil,
            body: Block(
                [
                    Let {
                        ident: Symbol(
                            3
                        ),
                        ty: Struct(
                            Symbol(
                                0
                            ),
                            [
                                Field {
                                    name: Symbol(
                                        1
                                    ),
                                    ty: Var(
                                        TypeVar(
                                            0
                                        )
                                    )
                                }
                            ],
                            Unique(
                                0
                            )
                        ),
                        expr: Some(
                            TypedExpression {
                                expr: StructLit(
                                    Symbol(
                                        0
                                    ),
                                    [
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
                                    ]
                                ),
                                ty: Struct(
                                    Symbol(
                                        0
                                    ),
                                    [
                                        Field {
                                            name: Symbol(
                                                1
                                            ),
                                            ty: Var(
                                                TypeVar(
                                                    0
                                                )
                                            )
                                        }
                                    ],
                                    Unique(
                                        0
                                    )
                                )
                            }
                        )
                    },
                    Expr(
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
                    )
                ]
            ),
            linkage: Normal
        }
    ]
}.