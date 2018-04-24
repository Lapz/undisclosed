../Program {
    structs: [
        Spanned {
            span: Span {
                start: Position {
                    line: 1,
                    column: 1,
                    absolute: 0
                },
                end: Position {
                    line: 4,
                    column: 1,
                    absolute: 46
                }
            },
            value: Struct {
                span: Span {
                    start: Position {
                        line: 1,
                        column: 1,
                        absolute: 0
                    },
                    end: Position {
                        line: 1,
                        column: 7,
                        absolute: 6
                    }
                },
                name: Spanned {
                    span: Span {
                        start: Position {
                            line: 1,
                            column: 8,
                            absolute: 7
                        },
                        end: Position {
                            line: 1,
                            column: 14,
                            absolute: 13
                        }
                    },
                    value: ItemName {
                        name: Spanned {
                            span: Span {
                                start: Position {
                                    line: 1,
                                    column: 8,
                                    absolute: 7
                                },
                                end: Position {
                                    line: 1,
                                    column: 12,
                                    absolute: 11
                                }
                            },
                            value: Symbol(
                                0
                            )
                        },
                        type_params: [
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 1,
                                        column: 13,
                                        absolute: 12
                                    },
                                    end: Position {
                                        line: 1,
                                        column: 14,
                                        absolute: 13
                                    }
                                },
                                value: Symbol(
                                    1
                                )
                            }
                        ]
                    }
                },
                fields: Spanned {
                    span: Span {
                        start: Position {
                            line: 1,
                            column: 16,
                            absolute: 15
                        },
                        end: Position {
                            line: 4,
                            column: 1,
                            absolute: 46
                        }
                    },
                    value: [
                        Spanned {
                            span: Span {
                                start: Position {
                                    line: 2,
                                    column: 5,
                                    absolute: 21
                                },
                                end: Position {
                                    line: 2,
                                    column: 11,
                                    absolute: 27
                                }
                            },
                            value: Field {
                                name: Spanned {
                                    span: Span {
                                        start: Position {
                                            line: 2,
                                            column: 5,
                                            absolute: 21
                                        },
                                        end: Position {
                                            line: 2,
                                            column: 9,
                                            absolute: 25
                                        }
                                    },
                                    value: Symbol(
                                        2
                                    )
                                },
                                ty: Spanned {
                                    span: Span {
                                        start: Position {
                                            line: 2,
                                            column: 10,
                                            absolute: 26
                                        },
                                        end: Position {
                                            line: 2,
                                            column: 11,
                                            absolute: 27
                                        }
                                    },
                                    value: Simple(
                                        Spanned {
                                            span: Span {
                                                start: Position {
                                                    line: 2,
                                                    column: 10,
                                                    absolute: 26
                                                },
                                                end: Position {
                                                    line: 2,
                                                    column: 11,
                                                    absolute: 27
                                                }
                                            },
                                            value: Symbol(
                                                1
                                            )
                                        }
                                    )
                                }
                            }
                        },
                        Spanned {
                            span: Span {
                                start: Position {
                                    line: 3,
                                    column: 5,
                                    absolute: 33
                                },
                                end: Position {
                                    line: 3,
                                    column: 16,
                                    absolute: 44
                                }
                            },
                            value: Field {
                                name: Spanned {
                                    span: Span {
                                        start: Position {
                                            line: 3,
                                            column: 5,
                                            absolute: 33
                                        },
                                        end: Position {
                                            line: 3,
                                            column: 9,
                                            absolute: 37
                                        }
                                    },
                                    value: Symbol(
                                        3
                                    )
                                },
                                ty: Spanned {
                                    span: Span {
                                        start: Position {
                                            line: 3,
                                            column: 10,
                                            absolute: 38
                                        },
                                        end: Position {
                                            line: 3,
                                            column: 16,
                                            absolute: 44
                                        }
                                    },
                                    value: Poly(
                                        Spanned {
                                            span: Span {
                                                start: Position {
                                                    line: 3,
                                                    column: 10,
                                                    absolute: 38
                                                },
                                                end: Position {
                                                    line: 3,
                                                    column: 14,
                                                    absolute: 42
                                                }
                                            },
                                            value: Symbol(
                                                0
                                            )
                                        },
                                        [
                                            Spanned {
                                                span: Span {
                                                    start: Position {
                                                        line: 3,
                                                        column: 15,
                                                        absolute: 43
                                                    },
                                                    end: Position {
                                                        line: 3,
                                                        column: 16,
                                                        absolute: 44
                                                    }
                                                },
                                                value: Simple(
                                                    Spanned {
                                                        span: Span {
                                                            start: Position {
                                                                line: 3,
                                                                column: 15,
                                                                absolute: 43
                                                            },
                                                            end: Position {
                                                                line: 3,
                                                                column: 16,
                                                                absolute: 44
                                                            }
                                                        },
                                                        value: Symbol(
                                                            1
                                                        )
                                                    }
                                                )
                                            }
                                        ]
                                    )
                                }
                            }
                        }
                    ]
                }
            }
        }
    ],
    functions: [
        Spanned {
            span: Span {
                start: Position {
                    line: 7,
                    column: 1,
                    absolute: 50
                },
                end: Position {
                    line: 14,
                    column: 1,
                    absolute: 150
                }
            },
            value: Function {
                span: Span {
                    start: Position {
                        line: 7,
                        column: 1,
                        absolute: 50
                    },
                    end: Position {
                        line: 14,
                        column: 1,
                        absolute: 150
                    }
                },
                name: Spanned {
                    span: Span {
                        start: Position {
                            line: 7,
                            column: 4,
                            absolute: 53
                        },
                        end: Position {
                            line: 7,
                            column: 8,
                            absolute: 57
                        }
                    },
                    value: ItemName {
                        name: Spanned {
                            span: Span {
                                start: Position {
                                    line: 7,
                                    column: 4,
                                    absolute: 53
                                },
                                end: Position {
                                    line: 7,
                                    column: 8,
                                    absolute: 57
                                }
                            },
                            value: Symbol(
                                4
                            )
                        },
                        type_params: []
                    }
                },
                params: Spanned {
                    span: Span {
                        start: Position {
                            line: 7,
                            column: 8,
                            absolute: 57
                        },
                        end: Position {
                            line: 7,
                            column: 9,
                            absolute: 58
                        }
                    },
                    value: []
                },
                returns: None,
                body: Spanned {
                    span: Span {
                        start: Position {
                            line: 7,
                            column: 11,
                            absolute: 60
                        },
                        end: Position {
                            line: 14,
                            column: 1,
                            absolute: 150
                        }
                    },
                    value: Block(
                        [
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 8,
                                        column: 5,
                                        absolute: 66
                                    },
                                    end: Position {
                                        line: 11,
                                        column: 6,
                                        absolute: 130
                                    }
                                },
                                value: Let {
                                    escapes: false,
                                    ident: Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 8,
                                                column: 9,
                                                absolute: 70
                                            },
                                            end: Position {
                                                line: 8,
                                                column: 13,
                                                absolute: 74
                                            }
                                        },
                                        value: Symbol(
                                            5
                                        )
                                    },
                                    ty: None,
                                    expr: Some(
                                        Spanned {
                                            span: Span {
                                                start: Position {
                                                    line: 8,
                                                    column: 16,
                                                    absolute: 77
                                                },
                                                end: Position {
                                                    line: 8,
                                                    column: 26,
                                                    absolute: 87
                                                }
                                            },
                                            value: StructLit(
                                                Spanned {
                                                    span: Span {
                                                        start: Position {
                                                            line: 8,
                                                            column: 16,
                                                            absolute: 77
                                                        },
                                                        end: Position {
                                                            line: 8,
                                                            column: 26,
                                                            absolute: 87
                                                        }
                                                    },
                                                    value: Instantiation {
                                                        ident: Spanned {
                                                            span: Span {
                                                                start: Position {
                                                                    line: 8,
                                                                    column: 16,
                                                                    absolute: 77
                                                                },
                                                                end: Position {
                                                                    line: 8,
                                                                    column: 20,
                                                                    absolute: 81
                                                                }
                                                            },
                                                            value: Symbol(
                                                                0
                                                            )
                                                        },
                                                        tys: Spanned {
                                                            span: Span {
                                                                start: Position {
                                                                    line: 8,
                                                                    column: 22,
                                                                    absolute: 83
                                                                },
                                                                end: Position {
                                                                    line: 8,
                                                                    column: 26,
                                                                    absolute: 87
                                                                }
                                                            },
                                                            value: [
                                                                Spanned {
                                                                    span: Span {
                                                                        start: Position {
                                                                            line: 8,
                                                                            column: 23,
                                                                            absolute: 84
                                                                        },
                                                                        end: Position {
                                                                            line: 8,
                                                                            column: 26,
                                                                            absolute: 87
                                                                        }
                                                                    },
                                                                    value: I32
                                                                }
                                                            ]
                                                        },
                                                        fields: [
                                                            Spanned {
                                                                span: Span {
                                                                    start: Position {
                                                                        line: 9,
                                                                        column: 9,
                                                                        absolute: 99
                                                                    },
                                                                    end: Position {
                                                                        line: 9,
                                                                        column: 13,
                                                                        absolute: 103
                                                                    }
                                                                },
                                                                value: StructLitField {
                                                                    ident: Spanned {
                                                                        span: Span {
                                                                            start: Position {
                                                                                line: 9,
                                                                                column: 9,
                                                                                absolute: 99
                                                                            },
                                                                            end: Position {
                                                                                line: 9,
                                                                                column: 13,
                                                                                absolute: 103
                                                                            }
                                                                        },
                                                                        value: Symbol(
                                                                            2
                                                                        )
                                                                    },
                                                                    expr: Spanned {
                                                                        span: Span {
                                                                            start: Position {
                                                                                line: 9,
                                                                                column: 14,
                                                                                absolute: 104
                                                                            },
                                                                            end: Position {
                                                                                line: 9,
                                                                                column: 16,
                                                                                absolute: 106
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
                                                            },
                                                            Spanned {
                                                                span: Span {
                                                                    start: Position {
                                                                        line: 10,
                                                                        column: 9,
                                                                        absolute: 116
                                                                    },
                                                                    end: Position {
                                                                        line: 10,
                                                                        column: 13,
                                                                        absolute: 120
                                                                    }
                                                                },
                                                                value: StructLitField {
                                                                    ident: Spanned {
                                                                        span: Span {
                                                                            start: Position {
                                                                                line: 10,
                                                                                column: 9,
                                                                                absolute: 116
                                                                            },
                                                                            end: Position {
                                                                                line: 10,
                                                                                column: 13,
                                                                                absolute: 120
                                                                            }
                                                                        },
                                                                        value: Symbol(
                                                                            3
                                                                        )
                                                                    },
                                                                    expr: Spanned {
                                                                        span: Span {
                                                                            start: Position {
                                                                                line: 10,
                                                                                column: 14,
                                                                                absolute: 121
                                                                            },
                                                                            end: Position {
                                                                                line: 10,
                                                                                column: 17,
                                                                                absolute: 124
                                                                            }
                                                                        },
                                                                        value: Literal(
                                                                            Nil
                                                                        )
                                                                    }
                                                                }
                                                            }
                                                        ]
                                                    }
                                                }
                                            )
                                        }
                                    )
                                }
                            },
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 13,
                                        column: 5,
                                        absolute: 137
                                    },
                                    end: Position {
                                        line: 13,
                                        column: 16,
                                        absolute: 148
                                    }
                                },
                                value: Return(
                                    Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 13,
                                                column: 12,
                                                absolute: 144
                                            },
                                            end: Position {
                                                line: 13,
                                                column: 16,
                                                absolute: 148
                                            }
                                        },
                                        value: Var(
                                            Spanned {
                                                span: Span {
                                                    start: Position {
                                                        line: 13,
                                                        column: 12,
                                                        absolute: 144
                                                    },
                                                    end: Position {
                                                        line: 13,
                                                        column: 16,
                                                        absolute: 148
                                                    }
                                                },
                                                value: Simple(
                                                    Spanned {
                                                        span: Span {
                                                            start: Position {
                                                                line: 13,
                                                                column: 12,
                                                                absolute: 144
                                                            },
                                                            end: Position {
                                                                line: 13,
                                                                column: 16,
                                                                absolute: 148
                                                            }
                                                        },
                                                        value: Symbol(
                                                            5
                                                        )
                                                    }
                                                )
                                            }
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