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
                    absolute: 40
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
                            absolute: 40
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
                                    column: 11,
                                    absolute: 39
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
                                            column: 11,
                                            absolute: 39
                                        }
                                    },
                                    value: Simple(
                                        Spanned {
                                            span: Span {
                                                start: Position {
                                                    line: 3,
                                                    column: 10,
                                                    absolute: 38
                                                },
                                                end: Position {
                                                    line: 3,
                                                    column: 11,
                                                    absolute: 39
                                                }
                                            },
                                            value: Symbol(
                                                1
                                            )
                                        }
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
                    absolute: 44
                },
                end: Position {
                    line: 11,
                    column: 1,
                    absolute: 99
                }
            },
            value: Function {
                span: Span {
                    start: Position {
                        line: 7,
                        column: 1,
                        absolute: 44
                    },
                    end: Position {
                        line: 11,
                        column: 1,
                        absolute: 99
                    }
                },
                name: Spanned {
                    span: Span {
                        start: Position {
                            line: 7,
                            column: 4,
                            absolute: 47
                        },
                        end: Position {
                            line: 7,
                            column: 8,
                            absolute: 51
                        }
                    },
                    value: ItemName {
                        name: Spanned {
                            span: Span {
                                start: Position {
                                    line: 7,
                                    column: 4,
                                    absolute: 47
                                },
                                end: Position {
                                    line: 7,
                                    column: 8,
                                    absolute: 51
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
                            absolute: 51
                        },
                        end: Position {
                            line: 7,
                            column: 9,
                            absolute: 52
                        }
                    },
                    value: []
                },
                returns: Some(
                    Spanned {
                        span: Span {
                            start: Position {
                                line: 7,
                                column: 14,
                                absolute: 57
                            },
                            end: Position {
                                line: 7,
                                column: 22,
                                absolute: 65
                            }
                        },
                        value: Poly(
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 7,
                                        column: 14,
                                        absolute: 57
                                    },
                                    end: Position {
                                        line: 7,
                                        column: 18,
                                        absolute: 61
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
                                            line: 7,
                                            column: 19,
                                            absolute: 62
                                        },
                                        end: Position {
                                            line: 7,
                                            column: 22,
                                            absolute: 65
                                        }
                                    },
                                    value: I32
                                }
                            ]
                        )
                    }
                ),
                body: Spanned {
                    span: Span {
                        start: Position {
                            line: 7,
                            column: 24,
                            absolute: 67
                        },
                        end: Position {
                            line: 11,
                            column: 1,
                            absolute: 99
                        }
                    },
                    value: Block(
                        [
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 8,
                                        column: 5,
                                        absolute: 73
                                    },
                                    end: Position {
                                        line: 8,
                                        column: 18,
                                        absolute: 86
                                    }
                                },
                                value: Let {
                                    escapes: false,
                                    ident: Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 8,
                                                column: 9,
                                                absolute: 77
                                            },
                                            end: Position {
                                                line: 8,
                                                column: 13,
                                                absolute: 81
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
                                                    absolute: 84
                                                },
                                                end: Position {
                                                    line: 8,
                                                    column: 18,
                                                    absolute: 86
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
                                        line: 10,
                                        column: 9,
                                        absolute: 97
                                    },
                                    end: Position {
                                        line: 10,
                                        column: 9,
                                        absolute: 97
                                    }
                                },
                                value: Expr(
                                    Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 10,
                                                column: 5,
                                                absolute: 93
                                            },
                                            end: Position {
                                                line: 10,
                                                column: 9,
                                                absolute: 97
                                            }
                                        },
                                        value: Var(
                                            Spanned {
                                                span: Span {
                                                    start: Position {
                                                        line: 10,
                                                        column: 5,
                                                        absolute: 93
                                                    },
                                                    end: Position {
                                                        line: 10,
                                                        column: 9,
                                                        absolute: 97
                                                    }
                                                },
                                                value: Simple(
                                                    Spanned {
                                                        span: Span {
                                                            start: Position {
                                                                line: 10,
                                                                column: 5,
                                                                absolute: 93
                                                            },
                                                            end: Position {
                                                                line: 10,
                                                                column: 9,
                                                                absolute: 97
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