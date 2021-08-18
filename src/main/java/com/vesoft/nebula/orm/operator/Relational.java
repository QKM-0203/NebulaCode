/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to represent attribute relationships
 */
public enum Relational implements Filter {
    EQ("=="),
    GE(">="),
    GT(">"),
    LE("<="),
    LT("<"),
    NE("!="),
    Regular("=～"),
    ENDSWITH("ENDS WITH"),
    STARTSWITH("STARTS WITH"),
    IN("IN"),
    CONTAINS("CONTAINS");

    String symbol;
    Object value;
    Relational(String symbol) {
        this.symbol = symbol;
    }

    public String getSymbol() {
        return symbol;
    }

    public Relational setValue(Object value) {
        this.value = value;
        return this;
    }

    public Object getValue() {
        return value;
    }
}
