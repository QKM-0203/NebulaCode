/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package com.vesoft.nebula.orm.operator;

/**
 * unary operator
 */
public enum UnaryOperation implements Filter {
    IsNotNull("IS NOT NULL"),
    IsNull("IS NULL");

    private final String symbol;
    UnaryOperation(String symbol) {
        this.symbol = symbol;
    }

    public String getSymbol() {
        return symbol;
    }
}
