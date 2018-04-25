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
                    line: 8,
                    column: 1,
                    absolute: 78
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
                    ty: App(
                        Int(
                            Signed,
                            Bit32
                        ),
                        []
                    )
                }
            ],
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
                    ),
                    Expr(
                        TypedExpression {
                            expr: Call(
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
                                                1
                                            )
                                        )
                                    }
                                ]
                            ),
                            ty: Nil
                        }
                    )
                ]
            ),
            linkage: Normal
        },
        Function {
            span: Span {
                start: Position {
                    line: 11,
                    column: 1,
                    absolute: 82
                },
                end: Position {
                    line: 14,
                    column: 1,
                    absolute: 118
                }
            },
            name: Symbol(
                2
            ),
            generic: true,
            params: [
                FunctionParam {
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
                },
                FunctionParam {
                    name: Symbol(
                        3
                    ),
                    ty: App(
                        Int(
                            Signed,
                            Bit32
                        ),
                        []
                    )
                }
            ],
            returns: Nil,
            body: Block(
                [
                    Expr(
                        TypedExpression {
                            expr: Call(
                                Symbol(
                                    2
                                ),
                                [
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
                                                        2
                                                    )
                                                )
                                            },
                                            Minus,
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
                                                        3
                                                    )
                                                )
                                            }
                                        ),
                                        ty: Var(
                                            TypeVar(
                                                2
                                            )
                                        )
                                    },
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
                                                4
                                            )
                                        )
                                    }
                                ]
                            ),
                            ty: Nil
                        }
                    )
                ]
            ),
            linkage: Normal
        }
    ]
}.