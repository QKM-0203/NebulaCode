/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * used to represent logical relationships
 */
public enum Logical implements Filter {
    AND("AND"),
    XOR("XOR"),
    OR("OR");

    String symbol;
    Relational leftRelational;
    Relational rightRelational;

    Logical(String symbol) {
        this.symbol = symbol;
    }

    /**
     * pass in two relational,logical representation of two relations
     * @param leftRelational logical front
     * @param rightRelational logical behind
     * @return Logical
     */
    public Logical setRelational(Relational leftRelational, Relational rightRelational) {
        this.leftRelational = leftRelational;
        this.rightRelational = rightRelational;
        return this;
    }

    public Relational getLeftRelational() {
        return leftRelational;
    }

    public Relational getRightRelational() {
        return rightRelational;
    }
}
