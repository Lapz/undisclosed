Program {
    structs: [],
    functions: [
        Spanned {
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
            value: Function {
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
                name: Spanned {
                    span: Span {
                        start: Position {
                            line: 1,
                            column: 4,
                            absolute: 3
                        },
                        end: Position {
                            line: 1,
                            column: 8,
                            absolute: 7
                        }
                    },
                    value: ItemName {
                        name: Spanned {
                            span: Span {
                                start: Position {
                                    line: 1,
                                    column: 4,
                                    absolute: 3
                                },
                                end: Position {
                                    line: 1,
                                    column: 8,
                                    absolute: 7
                                }
                            },
                            value: Symbol(
                                0
                            )
                        },
                        type_params: []
                    }
                },
                params: Spanned {
                    span: Span {
                        start: Position {
                            line: 1,
                            column: 8,
                            absolute: 7
                        },
                        end: Position {
                            line: 1,
                            column: 9,
                            absolute: 8
                        }
                    },
                    value: []
                },
                returns: None,
                body: Spanned {
                    span: Span {
                        start: Position {
                            line: 1,
                            column: 11,
                            absolute: 10
                        },
                        end: Position {
                            line: 6,
                            column: 1,
                            absolute: 58
                        }
                    },
                    value: Block(
                        [
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 2,
                                        column: 5,
                                        absolute: 16
                                    },
                                    end: Position {
                                        line: 2,
                                        column: 15,
                                        absolute: 26
                                    }
                                },
                                value: Let {
                                    escapes: false,
                                    ident: Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 2,
                                                column: 9,
                                                absolute: 20
                                            },
                                            end: Position {
                                                line: 2,
                                                column: 10,
                                                absolute: 21
                                            }
                                        },
                                        value: Symbol(
                                            1
                                        )
                                    },
                                    ty: None,
                                    expr: Some(
                                        Spanned {
                                            span: Span {
                                                start: Position {
                                                    line: 2,
                                                    column: 13,
                                                    absolute: 24
                                                },
                                                end: Position {
                                                    line: 2,
                                                    column: 15,
                                                    absolute: 26
                                                }
                                            },
                                            value: Literal(
                                                Number(
                                                    Number {
                                                        value: 10,
                                                        ty: None
                                                    }
                                                )
                                            )
                                        }
                                    )
                                }
                            },
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 3,
                                        column: 5,
                                        absolute: 34
                                    },
                                    end: Position {
                                        line: 5,
                                        column: 5,
                                        absolute: 56
                                    }
                                },
                                value: Block(
                                    [
                                        Spanned {
                                            span: Span {
                                                start: Position {
                                                    line: 4,
                                                    column: 15,
                                                    absolute: 50
                                                },
                                                end: Position {
                                                    line: 4,
                                                    column: 15,
                                                    absolute: 50
                                                }
                                            },
                                            value: Expr(
                                                Spanned {
                                                    span: Span {
                                                        start: Position {
                                                            line: 4,
                                                            column: 9,
                                                            absolute: 44
                                                        },
                                                        end: Position {
                                                            line: 4,
                                                            column: 15,
                                                            absolute: 50
                                                        }
                                                    },
                                                    value: Assign {
                                                        name: Spanned {
                                                            span: Span {
                                                                start: Position {
                                                                    line: 4,
                                                                    column: 9,
                                                                    absolute: 44
                                                                },
                                                                end: Position {
                                                                    line: 4,
                                                                    column: 10,
                                                                    absolute: 45
                                                                }
                                                            },
                                                            value: Simple(
                                                                Spanned {
                                                                    span: Span {
                                                                        start: Position {
                                                                            line: 4,
                                                                            column: 9,
                                                                            absolute: 44
                                                                        },
                                                                        end: Position {
                                                                            line: 4,
                                                                            column: 10,
                                                                            absolute: 45
                                                                        }
                                                                    },
                                                                    value: Symbol(
                                                                        1
                                                                    )
                                                                }
                                                            )
                                                        },
                                                        value: Spanned {
                                                            span: Span {
                                                                start: Position {
                                                                    line: 4,
                                                                    column: 13,
                                                                    absolute: 48
                                                                },
                                                                end: Position {
                                                                    line: 4,
                                                                    column: 15,
                                                                    absolute: 50
                                                                }
                                                            },
                                                            value: Literal(
                                                                Number(
                                                                    Number {
                                                                        value: 10,
                                                                        ty: None
                                                                    }
                                                                )
                                                            )
                                                        }
                                                    }
                                                }
                                            )
                                        }
                                    ]
                                )
                            }
                        ]
                    )
                },
                linkage: Normal
            }
        }
    ],
    type_alias: []
}