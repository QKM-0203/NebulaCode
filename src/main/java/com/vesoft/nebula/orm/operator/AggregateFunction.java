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
    COLLETC,
    AVG,
    MIN,
    COUNT,
    MAX,
    STD,
    SUM;

    private String value;

    public String getValue() {
        return value;
    }

    public AggregateFunction setValue(String value) {
        this.value = value;
        return this;
    }
}
