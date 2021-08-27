/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * aggregate function
 */
public enum AggregateFunction {
    COLLECT,
    AVG,
    MIN,
    COUNT,
    MAX,
    STD,
    SUM,
    BIT_AND,
    BIT_OR,
    BIT_XOR,
    COLLECT_SET;

    private String value;

    public String getValue() {
        return value;
    }

    public AggregateFunction setValue(String value) {
        this.value = value;
        return this;
    }
}
