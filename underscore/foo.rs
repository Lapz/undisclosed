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
                    line: 3,
                    column: 1,
                    absolute: 28
                }
            },
            name: Symbol(
                0
            ),
            params: [],
            returns: Nil,
            body: Let {
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
            linkage: Normal
        }
    ]
}