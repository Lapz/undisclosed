../Program {
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
                    line: 5,
                    column: 1,
                    absolute: 53
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
                        line: 5,
                        column: 1,
                        absolute: 53
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
                            line: 5,
                            column: 1,
                            absolute: 53
                        }
                    },
                    value: Block(
                        [
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 2,
                                        column: 4,
                                        absolute: 15
                                    },
                                    end: Position {
                                        line: 2,
                                        column: 24,
                                        absolute: 35
                                    }
                                },
                                value: Let {
                                    escapes: false,
                                    ident: Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 2,
                                                column: 8,
                                                absolute: 19
                                            },
                                            end: Position {
                                                line: 2,
                                                column: 9,
                                                absolute: 20
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
                                                    column: 12,
                                                    absolute: 23
                                                },
                                                end: Position {
                                                    line: 2,
                                                    column: 23,
                                                    absolute: 34
                                                }
                                            },
                                            value: Array {
                                                items: [
                                                    Spanned {
                                                        span: Span {
                                                            start: Position {
                                                                line: 2,
                                                                column: 13,
                                                                absolute: 24
                                                            },
                                                            end: Position {
                                                                line: 2,
                                                                column: 14,
                                                                absolute: 25
                                                            }
                                                        },
                                                        value: Literal(
                                                            Number(
                                                                Number {
                                                                    value: 1,
                                                                    ty: None
                                                                }
                                                            )
                                                        )
                                                    },
                                                    Spanned {
                                                        span: Span {
                                                            start: Position {
                                                                line: 2,
                                                                column: 15,
                                                                absolute: 26
                                                            },
                                                            end: Position {
                                                                line: 2,
                                                                column: 16,
                                                                absolute: 27
                                                            }
                                                        },
                                                        value: Literal(
                                                            Number(
                                                                Number {
                                                                    value: 2,
                                                                    ty: None
                                                                }
                                                            )
                                                        )
                                                    },
                                                    Spanned {
                                                        span: Span {
                                                            start: Position {
                                                                line: 2,
                                                                column: 17,
                                                                absolute: 28
                                                            },
                                                            end: Position {
                                                                line: 2,
                                                                column: 21,
                                                                absolute: 32
                                                            }
                                                        },
                                                        value: Array {
                                                            items: [
                                                                Spanned {
                                                                    span: Span {
                                                                        start: Position {
                                                                            line: 2,
                                                                            column: 18,
                                                                            absolute: 29
                                                                        },
                                                                        end: Position {
                                                                            line: 2,
                                                                            column: 19,
                                                                            absolute: 30
                                                                        }
                                                                    },
                                                                    value: Literal(
                                                                        Number(
                                                                            Number {
                                                                                value: 3,
                                                                                ty: None
                                                                            }
                                                                        )
                                                                    )
                                                                },
                                                                Spanned {
                                                                    span: Span {
                                                                        start: Position {
                                                                            line: 2,
                                                                            column: 20,
                                                                            absolute: 31
                                                                        },
                                                                        end: Position {
                                                                            line: 2,
                                                                            column: 21,
                                                                            absolute: 32
                                                                        }
                                                                    },
                                                                    value: Literal(
                                                                        Number(
                                                                            Number {
                                                                                value: 4,
                                                                                ty: None
                                                                            }
                                                                        )
                                                                    )
                                                                }
                                                            ]
                                                        }
                                                    }
                                                ]
                                            }
                                        }
                                    )
                                }
                            },
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 4,
                                        column: 4,
                                        absolute: 41
                                    },
                                    end: Position {
                                        line: 4,
                                        column: 14,
                                        absolute: 51
                                    }
                                },
                                value: Return(
                                    Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 4,
                                                column: 11,
                                                absolute: 48
                                            },
                                            end: Position {
                                                line: 4,
                                                column: 14,
                                                absolute: 51
                                            }
                                        },
                                        value: Literal(
                                            Nil
                                        )
                                    }
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